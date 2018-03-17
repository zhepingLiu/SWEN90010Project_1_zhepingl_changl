with Principal;
with Network;
with ICD;
with HRM;
with Measures; use Measures;
with Heart;
with ImpulseGenerator;

package body ClosedLoop is

    Patient : Principal.PrincipalPtr;
    Cardiologist : Principal.PrincipalPtr;
    ClinicalAssistant : Principal.PrincipalPtr;
    KnownPrincipals : access Network.PrincipalArray;
    Net : Network.Network;
    Hrt : Heart.HeartType;
    Monitor : HRM.HRMType;
    Gen : ImpulseGenerator.GeneratorType;
    IcdUnit : ICD.ICDType;
    CurrentTime : Measures.TickCount;

    procedure Init is
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
        ICD.Init(IcdUnit, Monitor, Gen, Net, KnownPrincipals);
        CurrentTime := 0;
    end Init;

    procedure Tick is

        -- stores whether there was a message available on the network
        MsgAvailable : Boolean := False;
        -- stores the current message read from the network (if one was available)
        Msg : Network.NetworkMessage;
        -- stores the current message response from the ICD
        Response : Network.NetworkMessage;

    begin
        -- ICD Tick (included Generator Tick if needed)
        ICD.Tick(IcdUnit, Hrt, CurrentTime);
        -- HRM Tick
        HRM.Tick(Monitor, Hrt);
        -- Heart Tick
        Heart.Tick(Hrt);
        -- Network Tick
        Network.Tick(Net);

        -- Receive the messages from the network 
        -- and send them into the ICD unit
        Network.GetNewMessage(Net, MsgAvailable, Msg);
        if MsgAvailable then
            Network.SendMessage(Net, Msg);
            Response := ICD.Request(IcdUnit, Msg, Hrt);
            Network.DebugPrintMessage(Response);
        end if;

    end Tick;
end ClosedLoop;