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

    procedure Init(Icd : out ICDType);

    procedure On(Icd : in out ICDType; Prin : in Principal.Principal);

    procedure Off(Icd : in out ICDType; Prin : in Principal.Principal);

end ICD;