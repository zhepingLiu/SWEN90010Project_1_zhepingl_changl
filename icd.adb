with Principal; use Principal;
with HRM;
with ImpulseGenerator;

package body ICD is

    procedure Init(Icd : out ICDType) is
    begin
        Icd.IsOn := False;
        Heart.Init(Icd.Hrt);
        HRM.Init(Icd.Monitor);
        ImpulseGenerator.Init(Icd.Gen);

        Icd.Patient := new Principal.Principal;
        Icd.Cardiologist := new Principal.Principal;
        Icd.ClinicalAssistant := new Principal.Principal;

        Principal.InitPrincipalForRole(Icd.Patient.all, Principal.Patient);
        Principal.InitPrincipalForRole(Icd.Cardiologist.all, Principal.Cardiologist);
        Principal.InitPrincipalForRole(Icd.ClinicalAssistant.all, Principal.ClinicalAssistant);

        Icd.KnownPrincipals := new Network.PrincipalArray(0..2);
        Icd.KnownPrincipals(0) := Icd.Patient;
        Icd.KnownPrincipals(1) := Icd.Cardiologist;
        Icd.KnownPrincipals(2) := Icd.ClinicalAssistant;
        Network.Init(Icd.Net, Icd.KnownPrincipals);
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

end ICD;