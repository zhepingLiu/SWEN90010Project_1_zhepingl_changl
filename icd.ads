with Principal;
with Heart;
with HRM;
with Network;
with ImpulseGenerator;
with Measures;

package ICD is

    InitialTachyBound : constant Integer := 100;
    InitialJoulesToDeliver : constant Integer := 30;

    type Setting is
    record
        TachyBound : Measures.BPM;
        JoulesToDeliver: Measures.Joules;
    end record;

    type ICDType is 
    record
        -- the mode of the ICD
        IsOn : Boolean;
        Monitor : HRM.HRMType;
        Gen : ImpulseGenerator.GeneratorType;
        Net : Network.Network;
        CurrentSetting : ICD.Setting;

        -- these variables may not be included in this package
        Hrt : Heart.HeartType;
        KnownPrincipals : access Network.PrincipalArray;
    end record;

    procedure Init(IcdUnit : out ICDType; Monitor : in HRM.HRMType; Hrt : in Heart.HeartType;
                    Gen : in ImpulseGenerator.GeneratorType; Net : in Network.Network;
                    KnownPrincipals : access Network.PrincipalArray);
    function On(Icd : in out ICDType; Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;
    function Off(Icd : in out ICDType; Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;
    function Request(IcdUnit : in out ICDType; Command : in Network.NetworkMessage; 
                    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;

private

    function ReadRateHistoryResponse(IcdUnit : in ICD.ICDType; Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;
    function ReadSettingsResponse(IcdUnit : in ICD.ICDType; Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;
    function ChangeSettingsResponse(IcdUnit : out ICD.ICDType; Prin : in Principal.PrincipalPtr; 
                                    Request : in Network.NetworkMessage) return Network.NetworkMessage;

end ICD;