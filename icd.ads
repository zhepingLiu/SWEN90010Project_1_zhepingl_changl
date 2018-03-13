with Principal;
with Heart;
with HRM;
with Network;
with ImpulseGenerator;

package ICD is

    type ICDType is 
    record
        -- the mode of the ICD
        IsOn : Boolean;
        Hrt : Heart.HeartType;
        Monitor : HRM.HRMType;
        Gen : ImpulseGenerator.GeneratorType;
        Net : Network.Network;

        Patient : Principal.PrincipalPtr;
        Cardiologist : Principal.PrincipalPtr;
        ClinicalAssistant : Principal.PrincipalPtr;
        KnownPrincipals : access Network.PrincipalArray;

    end record;

    procedure Init(IcdUnit : out ICDType);
    procedure On(Icd : in out ICDType; Prin : in Principal.Principal);
    procedure Off(Icd : in out ICDType; Prin : in Principal.Principal);
    function Request(IcdUnit : in out ICDType; Command : in String; Prin : in Principal.Principal) return String;

private

    type RateHistory is array (Integer) of Float;

    type Setting is
    record
        TachyBound : Integer;
        JoulesToDeliver: Integer;
    end record;

    function ReadRateHistoryResponse(Prin : in Principal.Principal) return RateHistory;
    function ReadSettingsResponse(Prin : in Principal.Principal) return Setting;
    function ChangeSettingsResponse(Prin : in Principal.Principal) return String;
end ICD;