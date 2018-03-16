with Principal;
with Network;
with ICD;
with HRM;
with Measures; use Measures;
with Heart;
with ImpulseGenerator;

package body ClosedLoop is

    procedure Init is
        Patient : Principal.PrincipalPtr;
        Cardiologist : Principal.PrincipalPtr;
        ClinicalAssistant : Principal.PrincipalPtr;

        Net : Network.Network;
        KnownPrincipals : access Network.PrincipalArray;

        Hrt : Heart.HeartType;
        Monitor : HRM.HRMType;
        Gen : ImpulseGenerator.GeneratorType;
        IcdUnit : ICD.ICDType;

        CurrentTime : Measures.TickCount;
    begin
        Patient := new Principal.Principal;
        Cardiologist := new Principal.Principal;
        ClinicalAssistant := new Principal.Principal;

        Principal.InitPrincipalForRole(Patient.all, Principal.Patient);
        Principal.InitPrincipalForRole(Cardiologist.all, 
                                        Principal.Cardiologist);
        Principal.InitPrincipalForRole(ClinicalAssistant.all, 
                                        Principal.ClinicalAssistant);

        KnownPrincipals := new Network.PrincipalArray(0..2);
        KnownPrincipals(0) := Patient;
        KnownPrincipals(1) := Cardiologist;
        KnownPrincipals(2) := ClinicalAssistant;
        Network.Init(Net, KnownPrincipals);

        Heart.Init(Hrt);
        HRM.Init(Monitor);
        ImpulseGenerator.Init(Gen);
        ICD.Init(IcdUnit, Monitor, Hrt, Gen, Net, KnownPrincipals);

        CurrentTime := 0;
    end Init;

    procedure Tick is
    begin
        -- TODO Not implemented
        Init;
    end Tick;
end ClosedLoop;