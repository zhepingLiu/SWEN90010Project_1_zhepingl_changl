with Principal;
with Heart;
with HRM;
with Network;
with ImpulseGenerator;
with Measures;

package ICD is

    INITIAL_TACHY_BOUND : constant Integer := 100;
    INITIAL_JOULES_TO_DELIVER : constant Integer := 30;
    TACHYCARDIA_RATE : constant Integer := 15;
    SIGNAL_NUMBER : constant Integer := 10;
    SIGNAL_JOULES : constant Measures.Joules := 2;
    SIGNAL_INTERVAL : constant Measures.TickCount := 4;
    NUMBER_PREHISTORY : constant Integer := 2;

    type PreRateHistory is array (Integer range 1..NUMBER_PREHISTORY)
                                 of Network.RateRecord;

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
        KnownPrincipals : access Network.PrincipalArray;
        Net : Network.Network;
        CurrentSetting : ICD.Setting;
        History : Network.RateHistory;
        PreHistory : PreRateHistory;
        HistoryPos : Integer;

        TachyCount : Integer;
        IsTachycardia : Boolean;
        ShotTime : Measures.TickCount;
    end record;

    procedure Init(IcdUnit : out ICDType; Monitor : in HRM.HRMType; 
    Gen : in ImpulseGenerator.GeneratorType; 
    Net : in Network.Network; KnownPrincipals : access Network.PrincipalArray);

    function Request(IcdUnit : in out ICDType; 
    Command : in Network.NetworkMessage; Hrt : in Heart.HeartType;
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;

    procedure Tick(IcdUnit : in out ICDType; Hrt : in out Heart.HeartType;
    CurrentTime : Measures.TickCount);

private

    procedure AppendHistory(IcdUnit : in out ICDType; 
    RecordRate : in Network.RateRecord);

    function CheckAuthorisation(IcdUnit : in ICDType; 
    Prin : Principal.PrincipalPtr; Role : in Principal.Role) return Boolean;

    function IsVentricleFibrillation(IcdUnit : in ICDType) return Boolean;

    function On(IcdUnit : in out ICDType; Hrt : in Heart.HeartType; 
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;

    function Off(IcdUnit : in out ICDType; Prin : in Principal.PrincipalPtr) 
                return Network.NetworkMessage;

    function ReadRateHistoryResponse(IcdUnit : in ICD.ICDType; 
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;

    function ReadSettingsResponse(IcdUnit : in ICD.ICDType; 
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;

    function ChangeSettingsResponse(IcdUnit : in out ICD.ICDType; 
    Prin : in Principal.PrincipalPtr; 
    Request : in Network.NetworkMessage) return Network.NetworkMessage;

end ICD;