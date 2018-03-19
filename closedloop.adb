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
    -- initialize all types
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
        -- ICD Tick (included Generator Tick if needed)
        ICD.Tick(IcdUnit, Hrt, CurrentTime);
        -- NetWork Tick
        Network.Tick(Net);

        -- Receive the messages from the network
        -- and send them into the ICD unit
        Network.GetNewMessage(Net, MsgAvailable, Msg);
        if MsgAvailable then
            Network.SendMessage(Net, Msg);
            case Msg.MessageType is
            -- Check authorisation and read history
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

          -- Check authorisation and change current settings
                when ChangeSettingsRequest =>
                    if ICD.CheckAuthorisation(IcdUnit, Msg.CSource,
                                                Principal.Cardiologist) then
                        Response := ICD.ChangeSettingsResponse(IcdUnit,
                                        Msg.CSource, Msg);
                    end if;

          -- Check authorisation and turn the mode to On
                when ModeOn =>
                    if not IcdUnit.IsOn AND (ICD.CheckAuthorisation(IcdUnit, Msg.MOnSource,
                                                Principal.ClinicalAssistant)
                    OR ICD.CheckAuthorisation(IcdUnit, Msg.MOnSource,
                                                Principal.Cardiologist)) then
                        Response := ICD.On(IcdUnit, Hrt, Msg.MOnSource);
                    end if;
                --when ModeOff =>
                    --if IcdUnit.IsOn AND (ICD.CheckAuthorisation(IcdUnit, Msg.MOffSource,
                      --                          Principal.ClinicalAssistant)
                    --OR ICD.CheckAuthorisation(IcdUnit, Msg.MOffSource,
                        --                        Principal.Cardiologist)) then
                        -- Response := ICD.Off(IcdUnit, Msg.MOffSource);

                    --end if;
                when others =>
                    Put_Line("ERROR: Incorrect Message Type");
            end case;
            Network.DebugPrintMessage(Response);
        end if;

        -- increment the current time
        CurrentTime := CurrentTime + 1;
        -- delay 0.1;
    end Tick;

end ClosedLoop;
