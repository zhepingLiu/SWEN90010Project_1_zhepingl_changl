with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Principal; use Principal;
with HRM;
with ImpulseGenerator;
with Network; use Network;
with Measures; use Measures;

-- ICD component that monitors the Heart condition through HRM and
-- apply impulse through Impluse Generator if necessary. ICD also receives
-- some specific message from the network and return the response
package body ICD is

    -- Initialise the ICD object
    procedure Init(IcdUnit : out ICDType; Monitor : in HRM.HRMType;
    Gen : in ImpulseGenerator.GeneratorType;
    KnownPrincipals : access Network.PrincipalArray) is
    begin
        -- initialze Mode, Monitor, Generator, Principals, History in ICD
        IcdUnit.IsOn := False;
        IcdUnit.Monitor := Monitor;
        IcdUnit.Gen := Gen;
        IcdUnit.KnownPrincipals := KnownPrincipals;
        IcdUnit.HistoryPos := IcdUnit.History'First;

        -- initialise the settings of ICD
        IcdUnit.CurrentSetting.TachyBound := INITIAL_TACHY_BOUND;
        IcdUnit.CurrentSetting.JoulesToDeliver := INITIAL_JOULES_TO_DELIVER;
    end Init;

    -- Receive 'ModeOn' message from the network and return response to the
    -- source of the message
    function On(IcdUnit : in out ICDType; Hrt : Heart.HeartType;
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : Network.NetworkMessage(ModeOn);
    begin
        -- turn on the ICD unit
        IcdUnit.IsOn := True;
        -- turn on the HRM (need to pass the Hrt in)
        HRM.On(IcdUnit.Monitor, Hrt);
        -- turn on the Impulse Generator
        ImpulseGenerator.On(IcdUnit.Gen);
        -- set the source of the response message
        Response.MOnSource := Prin;
        -- reset the history when restart the ICD
        IcdUnit.HistoryPos := IcdUnit.History'First;
        ICD.ResetHistory(IcdUnit);
        -- reset the tachy count and isTachycardia when restart the ICD
        IcdUnit.TachyCount := 0;
        IcdUnit.IsTachycardia := False;
        return Response;
    end On;

    -- Receive 'ModeOff' message from the network and return response to the
    -- source of the message
    function Off(IcdUnit : in out ICDType; Prin : in Principal.PrincipalPtr)
    return Network.NetworkMessage is
        Response : Network.NetworkMessage(ModeOff);
    begin
        -- Turn off ICD, impulse generator and HRM
        IcdUnit.IsOn := False;
        HRM.Off(IcdUnit.Monitor);
        ImpulseGenerator.Off(IcdUnit.Gen);
        Response.MOffSource := Prin;
        return Response;
    end Off;

    -- Receive 'ReadRateHistoryRequest' message from the network and 
    -- return response contains latest 5 history records to the source 
    -- of the message
    function ReadRateHistoryResponse(IcdUnit : in ICD.ICDType;
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : Network.NetworkMessage(ReadRateHistoryResponse);
    begin
        -- assign the source of the message
        Response.HDestination := Prin;
        -- read the history for 5 recent heart rate and time
        Response.History := IcdUnit.History;
        return Response;
    end ReadRateHistoryResponse;

    -- Receive 'ReadSettingsRequest' message from the network and 
    -- return response contains current setting of ICD to the source 
    -- of the message
    function ReadSettingsResponse(IcdUnit : in ICD.ICDType;
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : NetworkMessage(ReadSettingsResponse);
    begin
        -- assign setting value to response and return response
        Response.RDestination := Prin;
        Response.RTachyBound := IcdUnit.CurrentSetting.TachyBound;
        Response.RJoulesToDeliver :=
                            IcdUnit.CurrentSetting.JoulesToDeliver;
        return Response;
    end ReadSettingsResponse;

    -- Receive 'ChangeSettingsRequest' message from the network, 
    -- change the setting of ICD according to the message contents and
    -- return response to the source of the message
    function ChangeSettingsResponse(IcdUnit : in out ICD.ICDType;
    Prin : in Principal.PrincipalPtr;
    Request : in Network.NetworkMessage) return Network.NetworkMessage is
        Response : NetworkMessage(ChangeSettingsResponse);
    begin
        Response.CDestination := Prin;
        -- modify the settings here
        IcdUnit.CurrentSetting.TachyBound := Request.CTachyBound;
        IcdUnit.CurrentSetting.JoulesToDeliver := Request.CJoulesToDeliver;
        return Response;
    end ChangeSettingsResponse;

    -- Tick the ICD component (include HRM and Impulse Generator) to indicate
    -- one decisecond passes
    procedure Tick(IcdUnit : in out ICDType; Hrt : in out Heart.HeartType;
    CurrentTime : Measures.TickCount) is
        RecordRate : Network.RateRecord;
    begin
        -- Only Tick the ICD when ICD is Mode On
        if IcdUnit.IsOn then
            -- HRM Tick
            HRM.Tick(IcdUnit.Monitor, Hrt);

            -- record the current rate and current time
            HRM.GetRate(IcdUnit.Monitor, RecordRate.Rate);
            RecordRate.Time := CurrentTime;

            Put_Line("Current Rate is " & Integer'Image(RecordRate.Rate));

            -- set the impulse generator to deliver zero joules when
            -- no problem detected in heart
            ImpulseGenerator.SetImpulse(IcdUnit.Gen, ZERO_JOULES);

            -- append the record into History
            AppendHistory(IcdUnit, RecordRate);

            -- check if the patient has ventricle fibrillation at this moment
            if (IsVentricleFibrillation(IcdUnit)) then
                Put_Line("Ventricle Fibrillation Detected.");
                -- set impluse of the generator according the current setting
                ImpulseGenerator.SetImpulse(IcdUnit.Gen,
                        IcdUnit.CurrentSetting.JoulesToDeliver);

            -- check if there is a Tachycardia
            elsif (IcdUnit.IsTachycardia OR RecordRate.Rate >=
                        IcdUnit.CurrentSetting.TachyBound) then
                -- check if this is the patient's first impulse
                if (IcdUnit.TachyCount = 0) then
                    -- Set the impulse joules to signal joules
                    ImpulseGenerator.SetImpulse(IcdUnit.Gen, SIGNAL_JOULES);
                    -- Increment the treatment count
                    IcdUnit.TachyCount := IcdUnit.TachyCount + 1;

                    Put_Line("Tachycardia is detected at " & 
                            Integer'Image(RecordRate.Rate));

                    -- Treatment started
                    IcdUnit.IsTachycardia := True;
                    -- Compute the interval between each impulse
                    IcdUnit.ShotInterval := TOTAL_TICKS_MINUTE /
                                    (RecordRate.Rate + TACHYCARDIA_RATE);
                    -- Compute the next impulse time
                    IcdUnit.ShotTime := Integer(CurrentTime) + 
                                    Integer(IcdUnit.ShotInterval);
                    Put_Line("Shot Interval is " & 
                                    Integer'Image(IcdUnit.ShotInterval));

                -- keep treating patients who is already in treatment
                elsif (IcdUnit.TachyCount < SIGNAL_NUMBER AND
                        Integer(CurrentTime) = IcdUnit.ShotTime) then
                    -- Set the impulse joules to signal joules
                    ImpulseGenerator.SetImpulse(IcdUnit.Gen, SIGNAL_JOULES);
                    -- Increment the treatment count
                    IcdUnit.TachyCount := IcdUnit.TachyCount + 1;
                    -- Recompute the next impulse time
                    IcdUnit.ShotTime := IcdUnit.ShotTime +
                                        IcdUnit.ShotInterval;
                end if;
            end if;

            -- check if treatment has completed
            if (IcdUnit.TachyCount = SIGNAL_NUMBER) then
                Put_Line("Tachycardia Treatment stops at " & 
                                    Integer'Image(RecordRate.Rate));
                -- reset the count for treatment
                IcdUnit.TachyCount := 0;
                -- indicate current tachycardia treatment is completed
                IcdUnit.IsTachycardia := False;
            end if;
        end if;

        -- Tick the generator to impact the heart
        ImpulseGenerator.Tick(IcdUnit.Gen, Hrt);
    end Tick;

    -- Check the authorisation of the Principal
    function CheckAuthorisation(IcdUnit : in ICDType;
    Prin : Principal.PrincipalPtr; Role : Principal.Role) return Boolean is
        -- In default, the function will return false
        Authorised : Boolean := False;
    begin
        -- check if the principal has the specified role
        if (Principal.HasRole(Prin.all, Role)) then
            -- search the known principles to find 
            -- whether the principal is authorised
            for I in IcdUnit.KnownPrincipals'Range loop
                if (IcdUnit.KnownPrincipals(I) = Prin) then
                    Authorised := True;
                end if;
            end loop;
        end if;
        return Authorised;
    end CheckAuthorisation;

    -- Check if the patient has Ventricle Fibrillation at the moment
    function IsVentricleFibrillation(IcdUnit : in ICDType) return Boolean is
        TotalChange : Integer := 0;
        AverageChange : Integer;
        I : Integer := 1;
    begin
        -- there is no 7 history records
        if (IcdUnit.HistoryPos < TOTAL_NUMBER_HISTORY) then
            return False;

        -- calculate the average change rate
        else
            while I < TOTAL_NUMBER_HISTORY loop
                if (I = 1) then
                    TotalChange := abs (IcdUnit.PreHistory(2).Rate -
                                        IcdUnit.PreHistory(1).Rate);
                    I := I + 1;
                elsif (I = 2) then
                    TotalChange := abs (IcdUnit.History(I-1).Rate -
                                        IcdUnit.PreHistory(2).Rate);
                    I := I + 1;
                else
                    TotalChange := TotalChange + abs (IcdUnit.History(I-1).Rate
                                                 - IcdUnit.History(I-2).Rate);
                    I := I + 1;
                end if;
            end loop;

            -- Calculate the average change
            AverageChange := TotalChange / NUMBER_CHANGE;

            -- check if the average change rate is too high
            if (AverageChange >= VF_BOUNDARY) then
                return True;
            else
                return False;
            end if;
        end if;
    end IsVentricleFibrillation;

    -- Append the Rate Record to the History array
    procedure AppendHistory(IcdUnit : in out ICDType;
            RecordRate : in Network.RateRecord) is
    begin
        -- check if the History array is full
        if (IcdUnit.HistoryPos <= IcdUnit.History'Last) then
            IcdUnit.History(IcdUnit.HistoryPos) := RecordRate;
            IcdUnit.HistoryPos := IcdUnit.HistoryPos + 1;
        else
            -- record the first two record in the PreHistory Array
            -- and move other record one index forward, record the current
            -- rate at the end of History array
            IcdUnit.PreHistory(1) := IcdUnit.PreHistory(2);
            IcdUnit.PreHistory(2) := IcdUnit.History(1);
            IcdUnit.History(1) := IcdUnit.History(2);
            IcdUnit.History(2) := IcdUnit.History(3);
            IcdUnit.History(3) := IcdUnit.History(4);
            IcdUnit.History(4) := IcdUnit.History(5);
            IcdUnit.History(5) := RecordRate;
            IcdUnit.HistoryPos := IcdUnit.HistoryPos + 1;
        end if;
    end AppendHistory;

    -- Clear history
    procedure ResetHistory(IcdUnit : in out ICDType) is
        RateRecord : Network.RateRecord;
    begin
        RateRecord.Rate := 0;
        RateRecord.Time := 0;

        -- Clear prehistory
        for I in Integer range 1..NUMBER_PREHISTORY loop
            IcdUnit.PreHistory(I) := RateRecord;
        end loop;

        -- Clear history
        for I in Integer range 1..IcdUnit.History'Last loop
            IcdUnit.History(I) := RateRecord;
        end loop;
    end ResetHistory;
end ICD;
