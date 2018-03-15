package body HRM is

    procedure Init() is
    begin
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
end HRM;