with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Principal;
with Network; use Network;
with ICD;
with HRM;
with Measures; use Measures;
with Heart;
with ImpulseGenerator;

-- The ClosedLoop package includes all components for the ICD system,
-- including Network, ICD, HRM, Heart and Impulse Generator
package body ClosedLoop is

    -- Patient of the ICD
    Patient : Principal.PrincipalPtr;
    -- Cardiologist of the patient
    Cardiologist : Principal.PrincipalPtr;
    -- Clinical Assistant of the patient
    ClinicalAssistant : Principal.PrincipalPtr;
    -- Array contains Patient, Cardiologist and Clinical Assistant Roles 
    -- of this ICD
    KnownPrincipals : access Network.PrincipalArray;
    -- Network component
    Net : Network.Network;
    -- Heart component
    Hrt : Heart.HeartType;
    -- HRM component
    Monitor : HRM.HRMType;
    -- Impulse Generator component
    Gen : ImpulseGenerator.GeneratorType;
    -- ICD component
    IcdUnit : ICD.ICDType;
    -- Current time represented with TickCount
    CurrentTime : Measures.TickCount;

    -- initialize all components
    procedure Init is
    begin
        -- initialze all roles in principals
        Patient := new Principal.Principal;
        Cardiologist := new Principal.Principal;
        ClinicalAssistant := new Principal.Principal;

        Principal.InitPrincipalForRole(Patient.all, Principal.Patient);
        Principal.InitPrincipalForRole(Cardiologist.all,
                                        Principal.Cardiologist);
        Principal.InitPrincipalForRole(ClinicalAssistant.all,
                                        Principal.ClinicalAssistant);

        -- initialize all pricipals and network
        KnownPrincipals := new Network.PrincipalArray(0..2);
        KnownPrincipals(0) := Patient;
        KnownPrincipals(1) := Cardiologist;
        KnownPrincipals(2) := ClinicalAssistant;
        Network.Init(Net, KnownPrincipals);

        -- initialize Heart, Heart monitor and ImpluseGenerator
        Heart.Init(Hrt);
        HRM.Init(Monitor);
        ImpulseGenerator.Init(Gen);

        -- initialize ICD and currentTime
        ICD.Init(IcdUnit, Monitor, Gen, KnownPrincipals);
        CurrentTime := 0;
    end Init;

    -- Tick all components in the system to indicate one decisecond passes
    procedure Tick is
        -- stores whether there was a message available on the network
        MsgAvailable : Boolean := False;
        -- stores the current message read from the network
        -- (if one was available)
        Msg : Network.NetworkMessage;
        -- stores the current message response from the ICD
        Response : Network.NetworkMessage;

    begin
        -- Heart Tick
        Heart.Tick(Hrt);
        -- ICD Tick (included Generator Tick and HRM Tick)
        ICD.Tick(IcdUnit, Hrt, CurrentTime);
        -- NetWork Tick
        Network.Tick(Net);

        -- Receive the messages from the network
        -- and send them into the ICD unit
        Network.GetNewMessage(Net, MsgAvailable, Msg);
        if MsgAvailable then
            Network.SendMessage(Net, Msg);
            case Msg.MessageType is
                -- Check authorisation and read history only when ICD is ModeOn
                when ReadRateHistoryRequest =>
                    if IcdUnit.IsOn AND (ICD.CheckAuthorisation(IcdUnit,
                    Msg.HSource, Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.HSource,
                                                Principal.Cardiologist)) then
                        Response := ICD.ReadRateHistoryResponse(IcdUnit,
                                                Msg.HSource);
                    end if;

                -- Check authorisation and read current settings
                when ReadSettingsRequest =>
                    if ICD.CheckAuthorisation(IcdUnit, Msg.RSource,
                                                Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.RSource,
                                                Principal.Cardiologist) then
                        Response := ICD.ReadSettingsResponse(IcdUnit,
                                                Msg.RSource);
                    end if;

                -- Check authorisation and change current settings only
                -- when the ICD is off
                when ChangeSettingsRequest =>
                    if not ICDunit.IsOn AND ICD.CheckAuthorisation(IcdUnit, 
                    Msg.CSource, Principal.Cardiologist) then
                        Response := ICD.ChangeSettingsResponse(IcdUnit,
                                        Msg.CSource, Msg);
                    end if;

                -- Check authorisation and turn the ICD On only when ICD is off
                when ModeOn =>
                    if not IcdUnit.IsOn AND (ICD.CheckAuthorisation(IcdUnit, 
                                Msg.MOnSource, Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.MOnSource,
                                                Principal.Cardiologist)) then
                        Response := ICD.On(IcdUnit, Hrt, Msg.MOnSource);
                    end if;
                -- Check authorisation and turn the ICD off only when ICD is on
                when ModeOff =>
                    if IcdUnit.IsOn AND (ICD.CheckAuthorisation(IcdUnit,
                            Msg.MOffSource, Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.MOffSource,
                                            Principal.Cardiologist)) then
                        Response := ICD.Off(IcdUnit, Msg.MOffSource);
                    end if;
                when others =>
                    Put_Line("ERROR: Incorrect Message Type");
            end case;
            
            -- Print the Response
            Network.DebugPrintMessage(Response);
        end if;

        -- increment the current time
        CurrentTime := CurrentTime + 1;
        --delay 0.1;
    end Tick;

end ClosedLoop;
