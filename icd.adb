with Ada.Text_IO; use Ada.Text_IO;
with Principal; use Principal;
with HRM;
with ImpulseGenerator;
with Network; use Network;

package body ICD is

    procedure Init(IcdUnit : out ICDType; Monitor : in HRM.HRMType; Hrt : in Heart.HeartType;
                    Gen : in ImpulseGenerator.GeneratorType; Net : in Network.Network;
                    KnownPrincipals : access Network.PrincipalArray) is
    begin
        IcdUnit.IsOn := False;
        IcdUnit.Monitor := Monitor;
        IcdUnit.Gen := Gen;
        IcdUnit.Net := Net;
        IcdUnit.KnownPrincipals := KnownPrincipals;
        IcdUnit.HistoryPos := IcdUnit.History'First;

        -- assign the heart object (need refactor)
        IcdUnit.Hrt := Hrt;

        -- initialise the settings of ICD
        IcdUnit.CurrentSetting.TachyBound := INITIAL_TACHY_BOUND;
        IcdUnit.CurrentSetting.JoulesToDeliver := INITIAL_JOULES_TO_DELIVER;

    end Init;

    function On(IcdUnit : in out ICDType; Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : Network.NetworkMessage(ModeOn);
    begin
        -- check the principal of the operator
        if CheckAuthorisation(IcdUnit, Prin, ClinicalAssistant)
        OR CheckAuthorisation(IcdUnit, Prin, Cardiologist) then
            -- turn on the ICD unit
            IcdUnit.IsOn := True;
            -- turn on the HRM (need to pass the Hrt in)
            HRM.On(IcdUnit.Monitor, IcdUnit.Hrt);
            -- turn on the Impulse Generator
            ImpulseGenerator.On(IcdUnit.Gen);
            -- set the source of the response message
            Response.MOnSource := Prin;
        end if;
        return Response;
    end On;

    function Off(IcdUnit : in out ICDType; Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : Network.NetworkMessage(ModeOff);
    begin
        -- check the principal of the operator
        if CheckAuthorisation(IcdUnit, Prin, ClinicalAssistant)
        OR CheckAuthorisation(IcdUnit, Prin, Cardiologist) then
            IcdUnit.IsOn := False;
            HRM.Off(IcdUnit.Monitor);
            ImpulseGenerator.Off(IcdUnit.Gen);
            Response.MOffSource := Prin;
        end if;
        return Response;
    end Off;

    function Request(IcdUnit : in out ICDType; Command : in Network.NetworkMessage; 
                    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : Network.NetworkMessage;
    begin
        case Command.MessageType is
            when ReadRateHistoryRequest =>
                return ReadRateHistoryResponse(IcdUnit, Prin);
            when ReadSettingsRequest => 
                return ReadSettingsResponse(IcdUnit, Prin);
            -- this is only allowed for Cardiologist
            when ChangeSettingsRequest => 
                return ICD.ChangeSettingsResponse(IcdUnit, Prin, Command);
            when ModeOn =>
                return On(IcdUnit, Prin);
            when ModeOff =>
                return Off(IcdUnit, Prin);
            when others =>
                -- return what message here?
                Put_Line("ERROR: Incorrect Message");
                return Response;
        end case;
    end Request;

    function ReadRateHistoryResponse(IcdUnit : in ICD.ICDType; 
                                     Prin : Principal.PrincipalPtr) return Network.NetworkMessage is 
        Response : NetworkMessage(ReadRateHistoryResponse);
    begin
        if CheckAuthorisation(IcdUnit, Prin, ClinicalAssistant)
        OR CheckAuthorisation(IcdUnit, Prin, Cardiologist) then
            -- read the source of the message
            Response.HDestination := Prin;
            -- read the history for 5 recent heart rate and time
            Response.History := IcdUnit.History;
        end if;
        return Response;
    end ReadRateHistoryResponse;

    function ReadSettingsResponse(IcdUnit : in ICD.ICDType; 
                                  Prin : Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : NetworkMessage(ReadSettingsResponse);
    begin
        if CheckAuthorisation(IcdUnit, Prin, ClinicalAssistant)
        OR CheckAuthorisation(IcdUnit, Prin, Cardiologist) then
            Response.RDestination := Prin;
            Response.RTachyBound := IcdUnit.CurrentSetting.TachyBound;
            Response.RJoulesToDeliver := IcdUnit.CurrentSetting.JoulesToDeliver;
        end if;
        return Response;
    end ReadSettingsResponse;

    function ChangeSettingsResponse(IcdUnit : in out ICD.ICDType; Prin : Principal.PrincipalPtr;
                                    Request : in Network.NetworkMessage) return Network.NetworkMessage is
        Response : NetworkMessage(ChangeSettingsResponse);
    begin
        if CheckAuthorisation(IcdUnit, Prin, Cardiologist) then
            Response.CDestination := Prin;
            -- modify the settings here
            IcdUnit.CurrentSetting.TachyBound := Request.CTachyBound;
            IcdUnit.CurrentSetting.JoulesToDeliver := Request.CJoulesToDeliver;
        end if;
        return Response;
    end ChangeSettingsResponse;

    procedure Tick(IcdUnit : in out ICDType; Monitor : in HRM.HRMType; Hrt : in Heart.HeartType;
                    Gen : in out ImpulseGenerator.GeneratorType; CurrentTime : Measures.TickCount) is
        RecordRate : Network.RateRecord;
    begin
        if IcdUnit.IsOn then
            -- record the current rate and current time
            HRM.GetRate(Monitor, RecordRate.Rate);
            RecordRate.Time := CurrentTime;
            -- append the record into History
            AppendHistory(IcdUnit, RecordRate);

            -- check if the patient has tachycardia at this moment
            if (RecordRate.Rate >= IcdUnit.CurrentSetting.TachyBound + TACHYCARDIA_RATE) then
               for I in Integer range 1..SIGNAL_NUMBER loop
                    ImpulseGenerator.SetImpulse(Gen, SIGNAL_JOULES);
               end loop;
            end if;

            -- check if the patient has ventricle fibrillation at this moment
            if (IsVentricleFibrillation(IcdUnit)) then
                ImpulseGenerator.SetImpulse(Gen, IcdUnit.CurrentSetting.JoulesToDeliver);
            end if;
        end if;
    end Tick;

    function CheckAuthorisation(IcdUnit : in ICDType; Prin : Principal.PrincipalPtr; Role : Principal.Role) return Boolean is
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
        TotalChange : Integer;
        AverageChange : Integer;
    begin
        -- there is no 6 history records
        if (IcdUnit.HistoryPos <= IcdUnit.History'Last) then
            return False;
        else
            for I in IcdUnit.History'Range loop
                if (I = 1) then
                    TotalChange := abs (IcdUnit.History(I).Rate - IcdUnit.ZeroHistory.Rate);
                else
                    TotalChange := TotalChange + abs (IcdUnit.History(I).Rate - IcdUnit.History(I-1).Rate);
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

    procedure AppendHistory(IcdUnit : in out ICDType; RecordRate : in Network.RateRecord) is
    begin
        if (IcdUnit.HistoryPos <= IcdUnit.History'Last) then
            IcdUnit.History(IcdUnit.HistoryPos) := RecordRate;
            IcdUnit.HistoryPos := IcdUnit.HistoryPos + 1;
        else
            IcdUnit.ZeroHistory := IcdUnit.History(1);
            IcdUnit.History(1) := IcdUnit.History(2);
            IcdUnit.History(2) := IcdUnit.History(3);
            IcdUnit.History(3) := IcdUnit.History(4);
            IcdUnit.History(4) := IcdUnit.History(5);
            IcdUnit.History(5) := RecordRate;
        end if;
    end AppendHistory;
end ICD;