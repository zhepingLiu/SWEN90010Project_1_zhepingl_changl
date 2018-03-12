package ICD body is

    procedure Init(Icd : out ICDType) is
    begin
        Icd.IsOn := False;
        InitPrincipalForRole(Icd.Patient, Patient);
        InitPrincipalForRole(Icd.Cardiologist, Cardiologist);
        InitPrincipalForRole(Icd.ClinicalAssistant, ClinicalAssistant);
    end Init;

    procedure On(Icd : out ICDType; Prin : in Principal) is
    begin
        -- check the principla of the operator
        if (Principal.HasRole(Prin, Patient) OR Principal.HasRole(Prin, Cardiologist)) then
            Icd.IsOn := True;
        end if;
    end On;

    procedure Off(Icd : out ICDType; Prin : in Principal) is
    begin
        -- TODO check the principla of the operator
        if (Principal.HasRole(Prin, Patient) OR Principal.HasRole(Prin, Cardiologist)) then
            Icd.IsOn := False;
        end if;
    end Off;

end ICD;