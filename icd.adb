with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Principal; use Principal;
with HRM;
with ImpulseGenerator;
with Network; use Network;
with Measures; use Measures;

package body ICD is

    procedure Init(IcdUnit : out ICDType; Monitor : in HRM.HRMType; 
    Gen : in ImpulseGenerator.GeneratorType;
    KnownPrincipals : access Network.PrincipalArray) is
    begin
        IcdUnit.IsOn := False;
        IcdUnit.Monitor := Monitor;
        IcdUnit.Gen := Gen;
        IcdUnit.KnownPrincipals := KnownPrincipals;
        IcdUnit.HistoryPos := IcdUnit.History'First;

        -- initialise the settings of ICD
        IcdUnit.CurrentSetting.TachyBound := INITIAL_TACHY_BOUND;
        IcdUnit.CurrentSetting.JoulesToDeliver := INITIAL_JOULES_TO_DELIVER;
    end Init;

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
        -- reset the tachy count and isTachycardia when restart the ICD
        IcdUnit.TachyCount := 0;
        IcdUnit.IsTachycardia := False;
        return Response;
    end On;

    function Off(IcdUnit : in out ICDType; Prin : in Principal.PrincipalPtr)
    return Network.NetworkMessage is
        Response : Network.NetworkMessage(ModeOff);
    begin
        IcdUnit.IsOn := False;
        HRM.Off(IcdUnit.Monitor);
        ImpulseGenerator.Off(IcdUnit.Gen);
        Response.MOffSource := Prin;
        return Response;
    end Off;

    function ReadRateHistoryResponse(IcdUnit : in ICD.ICDType; 
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is 
        Response : Network.NetworkMessage(ReadRateHistoryResponse);
    begin
        -- read the source of the message
        Response.HDestination := Prin;
        -- read the history for 5 recent heart rate and time
        Response.History := IcdUnit.History;
        return Response;
    end ReadRateHistoryResponse;

    function ReadSettingsResponse(IcdUnit : in ICD.ICDType; 
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : NetworkMessage(ReadSettingsResponse);
    begin
        Response.RDestination := Prin;
        Response.RTachyBound := IcdUnit.CurrentSetting.TachyBound;
        Response.RJoulesToDeliver := 
                            IcdUnit.CurrentSetting.JoulesToDeliver;
        return Response;
    end ReadSettingsResponse;

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

    procedure Tick(IcdUnit : in out ICDType; Hrt : in out Heart.HeartType;
    CurrentTime : Measures.TickCount) is
        RecordRate : Network.RateRecord;
    begin
        if IcdUnit.IsOn then
            -- HeartMonitor Tick
            HRM.Tick(IcdUnit.Monitor, Hrt);

            -- record the current rate and current time
            HRM.GetRate(IcdUnit.Monitor, RecordRate.Rate);
            RecordRate.Time := CurrentTime;
            
            Put_Line("Current Rate is " & Integer'Image(RecordRate.Rate));

            -- append the record into History
            AppendHistory(IcdUnit, RecordRate);

            if (IcdUnit.IsTachycardia OR RecordRate.Rate >= 
                        IcdUnit.CurrentSetting.TachyBound) then
                if (IcdUnit.TachyCount = 0) then
                    -- Set the impulse joules to signal joules
                    ImpulseGenerator.SetImpulse(IcdUnit.Gen, SIGNAL_JOULES);
                    -- Tick the generator to impact the heart
                    ImpulseGenerator.Tick(IcdUnit.Gen, Hrt);
                    -- Set back the impulse to zero joules
                    ImpulseGenerator.SetImpulse(IcdUnit.Gen, ZERO_JOULES);
                    IcdUnit.TachyCount := IcdUnit.TachyCount + 1;

                    Put_Line("Tachycardia is detected at " & Integer'Image(RecordRate.Rate));
                    IcdUnit.IsTachycardia := True;
                    IcdUnit.ShotInterval := TOTAL_TICKS_MINUTE / 
                                    (RecordRate.Rate + TACHYCARDIA_RATE);
                    IcdUnit.ShotTime := Integer(CurrentTime) + Integer(IcdUnit.ShotInterval);
                    Put_Line("Shot Interval is " & Integer'Image(IcdUnit.ShotInterval));

                elsif (IcdUnit.TachyCount < SIGNAL_NUMBER AND 
                        Integer(CurrentTime) = IcdUnit.ShotTime) then
                    -- Set the impulse joules to signal joules
                    ImpulseGenerator.SetImpulse(IcdUnit.Gen, SIGNAL_JOULES);
                    -- Tick the generator to impact the heart
                    ImpulseGenerator.Tick(IcdUnit.Gen, Hrt);
                    -- Set back the impulse to zero joules
                    ImpulseGenerator.SetImpulse(IcdUnit.Gen, ZERO_JOULES);
                    
                    IcdUnit.TachyCount := IcdUnit.TachyCount + 1;
                    IcdUnit.ShotTime := IcdUnit.ShotTime + 
                                        IcdUnit.ShotInterval;
                end if;
            end if;
                
            if (IcdUnit.TachyCount = SIGNAL_NUMBER) then
                Put_Line("Tachycardia Treatment stops at " & Integer'Image(RecordRate.Rate));
                IcdUnit.TachyCount := 0;
                IcdUnit.IsTachycardia := False;
            end if;

            -- check if the patient has ventricle fibrillation at this moment
            if (IsVentricleFibrillation(IcdUnit)) then
                Put_Line("Ventricle Fibrillation Detected.");
                ImpulseGenerator.SetImpulse(IcdUnit.Gen, 
                        IcdUnit.CurrentSetting.JoulesToDeliver);
                ImpulseGenerator.Tick(IcdUnit.Gen, Hrt);
                -- Set back the impulse to zero joules
                ImpulseGenerator.SetImpulse(IcdUnit.Gen, ZERO_JOULES);
            end if;
        end if;
    end Tick;

    function CheckAuthorisation(IcdUnit : in ICDType; 
    Prin : Principal.PrincipalPtr; Role : Principal.Role) return Boolean is
        Authorised : Boolean := False;
    begin
        if (Principal.HasRole(Prin.all, Role)) then
            for I in IcdUnit.KnownPrincipals'Range loop
                if (IcdUnit.KnownPrincipals(I) = Prin) then
                    Authorised := True;
                end if;
            end loop;
        end if;
        return Authorised;
    end CheckAuthorisation;

    function IsVentricleFibrillation(IcdUnit : in ICDType) return Boolean is
        TotalChange : Integer := 0;
        AverageChange : Integer;
        I : Integer := 1;
        LoopRange : Integer;
    begin
        LoopRange := IcdUnit.History'Last + NUMBER_PREHISTORY;
        -- there is no 7 history records
        if (IcdUnit.HistoryPos < TOTAL_NUMBER_HISTORY) then
            return False;
        else
            while I < LoopRange loop
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
            
            AverageChange := TotalChange / 6;

            if (AverageChange >= 10) then
                return True;
            else
                return False;
            end if;
        end if;
    end IsVentricleFibrillation;

    procedure AppendHistory(IcdUnit : in out ICDType; 
    RecordRate : in Network.RateRecord) is
    begin
        if (IcdUnit.HistoryPos <= IcdUnit.History'Last) then
            IcdUnit.History(IcdUnit.HistoryPos) := RecordRate;
            IcdUnit.HistoryPos := IcdUnit.HistoryPos + 1;
        else
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
end ICD;