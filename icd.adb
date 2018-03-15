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

        -- assign the heart object (need refactor)
        IcdUnit.Hrt := Hrt;

        -- initialise the settings of ICD
        IcdUnit.CurrentSetting.TachyBound := InitialTachyBound;
        IcdUnit.CurrentSetting.JoulesToDeliver := InitialJoulesToDeliver;

    end Init;

    function On(Icd : in out ICDType; Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : Network.NetworkMessage(ModeOn);
    begin
        -- check the principla of the operator
        if Principal.HasRole(Prin.all, Patient) OR Principal.HasRole(Prin.all, Cardiologist) then
            -- turn on the ICD unit
            Icd.IsOn := True;
            -- turn on the HRM (need to pass the Hrt in)
            HRM.On(Icd.Monitor, Icd.Hrt);
            -- turn on the Impulse Generator
            ImpulseGenerator.On(Icd.Gen);
        end if;
        return Response;
    end On;

    function Off(Icd : in out ICDType; Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : Network.NetworkMessage(ModeOff);
    begin
        -- check the principla of the operator
        if Principal.HasRole(Prin.all, Principal.Patient) 
        OR Principal.HasRole(Prin.all, Principal.Cardiologist) then
            Icd.IsOn := False;
            HRM.Off(Icd.Monitor);
            ImpulseGenerator.Off(Icd.Gen);
        end if;
        return Response;
    end Off;

    function Request(IcdUnit : in out ICDType; Command : in Network.NetworkMessage; 
                    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage is
    begin
        -- TODO make sure the Principlas are authorised only to this ICD unit
        if Principal.HasRole(Prin.all, Principal.Cardiologist)
        OR Principal.HasRole(Prin.all, Principal.Cardiologist)
        OR Principal.HasRole(Prin.all, Principal.ClinicalAssistant) then
            case Command.MessageType is
                -- TODO complete the responses of each request
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
            end case;
        end if;
    end Request;

    function ReadRateHistoryResponse(IcdUnit : in ICD.ICDType; Prin : Principal.PrincipalPtr) return Network.NetworkMessage is
        History : Network.RateHistory;
        Response : NetworkMessage(ReadRateHistoryResponse);
    begin
        -- read the source of the message
        Response.HDestination := Prin;
        -- read the history for 5 recent heart rate and time
        Response.History := History;
        return Response;
    end ReadRateHistoryResponse;

    function ReadSettingsResponse(IcdUnit : in ICD.ICDType; Prin : Principal.PrincipalPtr) return Network.NetworkMessage is
        Response : NetworkMessage(ReadSettingsResponse);
    begin
        Response.RDestination := Prin;
        Response.RTachyBound := IcdUnit.CurrentSetting.TachyBound;
        Response.RJoulesToDeliver := IcdUnit.CurrentSetting.JoulesToDeliver;
        return Response;
    end ReadSettingsResponse;

    function ChangeSettingsResponse(IcdUnit : out ICD.ICDType; Prin : Principal.PrincipalPtr;
                                    Request : in Network.NetworkMessage) return Network.NetworkMessage is
        Response : NetworkMessage(ChangeSettingsResponse);
    begin
        Response.CDestination := Prin;
        -- modify the settings here
        IcdUnit.CurrentSetting.TachyBound := Request.CTachyBound;
        IcdUnit.CurrentSetting.JoulesToDeliver := Request.CJoulesToDeliver;
        return Response;
    end ChangeSettingsResponse;

end ICD;