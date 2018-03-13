with Principal; use Principal;
with HRM;
with ImpulseGenerator;

package body ICD is

    procedure Init(IcdUnit : out ICDType) is
    begin
        IcdUnit.IsOn := False;
        Heart.Init(IcdUnit.Hrt);
        HRM.Init(IcdUnit.Monitor);
        ImpulseGenerator.Init(IcdUnit.Gen);

        IcdUnit.Patient := new Principal.Principal;
        IcdUnit.Cardiologist := new Principal.Principal;
        IcdUnit.ClinicalAssistant := new Principal.Principal;

        Principal.InitPrincipalForRole(IcdUnit.Patient.all, Principal.Patient);
        Principal.InitPrincipalForRole(IcdUnit.Cardiologist.all, Principal.Cardiologist);
        Principal.InitPrincipalForRole(IcdUnit.ClinicalAssistant.all, Principal.ClinicalAssistant);

        IcdUnit.KnownPrincipals := new Network.PrincipalArray(0..2);
        IcdUnit.KnownPrincipals(0) := IcdUnit.Patient;
        IcdUnit.KnownPrincipals(1) := IcdUnit.Cardiologist;
        IcdUnit.KnownPrincipals(2) := IcdUnit.ClinicalAssistant;
        Network.Init(IcdUnit.Net, IcdUnit.KnownPrincipals);
    end Init;

    procedure On(Icd : in out ICDType; Prin : in Principal.Principal) is
    begin
        -- check the principla of the operator
        if Principal.HasRole(Prin, Patient) OR Principal.HasRole(Prin, Cardiologist) then
            -- turn on the ICD unit
            Icd.IsOn := True;
            -- turn on the HRM
            HRM.On(Icd.Monitor, Icd.Hrt);
            -- turn on the Impulse Generator
            ImpulseGenerator.On(Icd.Gen);
        end if;
    end On;

    procedure Off(Icd : in out ICDType; Prin : in Principal.Principal) is
    begin
        -- check the principla of the operator
        if Principal.HasRole(Prin, Principal.Patient) 
        OR Principal.HasRole(Prin, Principal.Cardiologist) then
            Icd.IsOn := False;
            HRM.Off(Icd.Monitor);
            ImpulseGenerator.Off(Icd.Gen);
        end if;
    end Off;

    function Request(IcdUnit : in out ICDType; Command : in String; 
    Prin : in Principal.Principal) return String is
        type Commands is (ReadRateHistoryRequest, ReadSettingsRequest,
        ChangeSettingsResponse, ModeOn, ModeOff);
    begin
        if Principal.HasRole(Prin, Principal.Patient)
        OR Principal.HasRole(Prin, Principal.Cardiologist) then
            case Commands'Value(Command) is
                -- TODO complete the responses of each request
                when ReadRateHistoryRequest => 
                    return "RateHistoryResponse";
                when ReadSettingsRequest => 
                    return "Settings";
                when ChangeSettingsResponse => 
                    return "ChangeSettings";
                when ModeOn =>
                    On(IcdUnit, Prin);
                    return "ICD is On";
                when ModeOff =>
                    Off(IcdUnit, Prin);
                    return "ICD is Off";
            end case;
        else
            return "None";
        end if;
    end Request;

    function ReadRateHistoryResponse(Prin : in Principal.Principal) return RateHistory is
        History : RateHistory;
    begin
        -- read the source of the message
        -- read the recent 5 heart rate as indexes as value in the array
        -- read the recent 5 measurement's time as value in the array
        return History;
    end ReadRateHistoryResponse;

    function ReadSettingsResponse(Prin : in Principal.Principal) return Setting is
        SettingReponse : Setting;
    begin
        return SettingReponse;
    end ReadSettingsResponse;

    function ChangeSettingsResponse(Prin : in Principal.Principal) return String is
    begin
        return "Setting has been changed";
    end ChangeSettingsResponse;

end ICD;