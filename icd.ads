with Principal;
with Heart;
with HRM;
with Network;
with ImpulseGenerator;
with Measures;

package ICD is

    INITIAL_TACHY_BOUND : constant Measures.BPM := 100;
    INITIAL_JOULES_TO_DELIVER : constant Measures.Joules := 30;
    ZERO_JOULES : constant Measures.Joules := 0;
    TACHYCARDIA_RATE : constant Measures.BPM := 15;
    SIGNAL_NUMBER : constant Integer := 10;
    SIGNAL_JOULES : constant Measures.Joules := 2;
    NUMBER_PREHISTORY : constant Integer := 2;
    TOTAL_NUMBER_HISTORY : constant Integer := 7;
    TOTAL_TICKS_MINUTE : constant Integer := 600;

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
        CurrentSetting : ICD.Setting;
        History : Network.RateHistory;
        PreHistory : PreRateHistory;
        HistoryPos : Integer;

        TachyCount : Integer;
        TachycardiaDetectedRate : Measures.BPM;
        IsTachycardia : Boolean;
        ShotTime : Integer;
        ShotInterval : Integer;
    end record;

    procedure Init(IcdUnit : out ICDType; Monitor : in HRM.HRMType; 
    Gen : in ImpulseGenerator.GeneratorType; 
    KnownPrincipals : access Network.PrincipalArray);

    procedure Tick(IcdUnit : in out ICDType; Hrt : in out Heart.HeartType;
    CurrentTime : Measures.TickCount);

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