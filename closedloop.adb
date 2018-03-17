with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Principal;
with Network; use Network;
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
        -- HeartMonitor Tick
        HRM.Tick(Monitor, Hrt);

        -- Heart Tick
        Heart.Tick(Hrt);

        -- NetWork Tick
        Network.Tick(Net);

        -- ICD Tick (included Generator Tick if needed)
        ICD.Tick(IcdUnit, Hrt, CurrentTime);

        -- Receive the messages from the network
        -- and send them into the ICD unit
        Network.GetNewMessage(Net, MsgAvailable, Msg);
        if MsgAvailable then
            Network.SendMessage(Net, Msg);
            case Msg.MessageType is
                when ReadRateHistoryRequest =>
                    if ICD.CheckAuthorisation(IcdUnit, Msg.HSource, Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.HSource, Principal.Cardiologist) then
                        Response := ICD.ReadRateHistoryResponse(IcdUnit, Msg.HSource);
                    end if;
                when ReadSettingsRequest => 
                    if ICD.CheckAuthorisation(IcdUnit, Msg.RSource, Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.RSource, Principal.Cardiologist) then
                        Response := ICD.ReadSettingsResponse(IcdUnit, Msg.RSource);
                    end if;
                when ChangeSettingsRequest => 
                    if ICD.CheckAuthorisation(IcdUnit, Msg.CSource, Principal.Cardiologist) then
                        Response := ICD.ChangeSettingsResponse(IcdUnit, 
                                        Msg.CSource, Msg);
                    end if;
                when ModeOn =>
                    if ICD.CheckAuthorisation(IcdUnit, Msg.MOnSource, Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.MOnSource, Principal.Cardiologist) then
                        Response := ICD.On(IcdUnit, Hrt, Msg.MOnSource);
                    end if;
                when ModeOff =>
                    if ICD.CheckAuthorisation(IcdUnit, Msg.MOffSource, Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.MOffSource, Principal.Cardiologist) then
                        Response := ICD.Off(IcdUnit, Msg.MOffSource);
                    end if;
                when others =>
                    Put_Line("ERROR: Incorrect Message Type");
            end case;
            Network.DebugPrintMessage(Response);
        end if;

        -- increment the current time
        CurrentTime := CurrentTime + 1;

    end Tick;

end ClosedLoop;
