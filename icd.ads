with Principal;
with Heart;
with HRM;
with Network;
with ImpulseGenerator;
with Measures;

-- ICD component that monitors the Heart condition through HRM and
-- apply impulse through Impluse Generator if necessary. ICD also receives
-- some specific message from the network and return the response
package ICD is

    INITIAL_TACHY_BOUND : constant Measures.BPM := 100;
    INITIAL_JOULES_TO_DELIVER : constant Measures.Joules := 30;
    ZERO_JOULES : constant Measures.Joules := 0;
    SIGNAL_NUMBER : constant Integer := 10;
    SIGNAL_JOULES : constant Measures.Joules := 2;

    -- The heart rate to add when tachycardia is detected
    TACHYCARDIA_RATE : constant Measures.BPM := 15;
    -- Number of entries in the pre-history record
    NUMBER_PREHISTORY : constant Integer := 2;
    -- Total number of entries history and pre-history records will hold
    TOTAL_NUMBER_HISTORY : constant Integer := 7;
    -- 60 seconds per minute * 10 ticks per second
    TOTAL_TICKS_MINUTE : constant Integer := 600;
    -- Number of changes to compute the Ventricle Fibrillation
    NUMBER_CHANGE : constant Integer := 6;
    -- Boundary value of dectecting the Ventricle Fibrillation
    VF_BOUNDARY : constant Measures.BPM := 10;

    -- Array can contain up to NUMBER_PREHISTORY entries of RateRecord
    type PreRateHistory is array (Integer range 1..NUMBER_PREHISTORY)
                                 of Network.RateRecord;

    -- The setting of the ICD
    type Setting is
    record
        TachyBound : Measures.BPM;
        JoulesToDeliver: Measures.Joules;
    end record;

    -- ICD object
    type ICDType is
    record
        -- the mode of the ICD
        IsOn : Boolean;
        -- HRM object
        Monitor : HRM.HRMType;
        -- Impulse generator object
        Gen : ImpulseGenerator.GeneratorType;
        -- All known (authorised) principal for this ICD
        KnownPrincipals : access Network.PrincipalArray;
        CurrentSetting : ICD.Setting;
        History : Network.RateHistory;
        PreHistory : PreRateHistory;
        -- An index to indicate how many entries is recorded in the history
        -- and the pre-history
        HistoryPos : Integer;
        -- Count the number of tachycardia treatment applied
        TachyCount : Integer;
        -- Boolean value to indicate if the patient is during tachycardia
        -- treatment
        IsTachycardia : Boolean;
        -- Next tachycardia treatment time
        ShotTime : Integer;
        -- The interval between each tachycardia treatment
        ShotInterval : Integer;
    end record;

    -- Initialise the ICD object
    procedure Init(IcdUnit : out ICDType; Monitor : in HRM.HRMType; 
    Gen : in ImpulseGenerator.GeneratorType; 
    KnownPrincipals : access Network.PrincipalArray);

    -- Tick the ICD component (include HRM and Impulse Generator) to indicate
    -- one decisecond passes
    procedure Tick(IcdUnit : in out ICDType; Hrt : in out Heart.HeartType;
    CurrentTime : Measures.TickCount);

    -- Append the Rate Record to the History array
    procedure AppendHistory(IcdUnit : in out ICDType; 
    RecordRate : in Network.RateRecord);

    -- Check the authorisation of the Principal
    function CheckAuthorisation(IcdUnit : in ICDType; 
    Prin : Principal.PrincipalPtr; Role : in Principal.Role) return Boolean;

    -- Check if the patient has Ventricle Fibrillation at the moment
    function IsVentricleFibrillation(IcdUnit : in ICDType) return Boolean;

    -- Receiving 'ModeOn' message from the network and return response to the
    -- source of the message
    function On(IcdUnit : in out ICDType; Hrt : in Heart.HeartType; 
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;

    -- Receive 'ModeOff' message from the network and return response to the
    -- source of the message
    function Off(IcdUnit : in out ICDType; Prin : in Principal.PrincipalPtr) 
                return Network.NetworkMessage;

    -- Receive 'ReadRateHistoryRequest' message from the network and 
    -- return response contains latest 5 history records to the source 
    -- of the message
    function ReadRateHistoryResponse(IcdUnit : in ICD.ICDType; 
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;

    -- Receive 'ReadSettingsRequest' message from the network and 
    -- return response contains current setting of ICD to the source 
    -- of the message
    function ReadSettingsResponse(IcdUnit : in ICD.ICDType; 
    Prin : in Principal.PrincipalPtr) return Network.NetworkMessage;

    -- Receive 'ChangeSettingsRequest' message from the network, 
    -- change the setting of ICD according to the message contents and
    -- return response to the source of the message
    function ChangeSettingsResponse(IcdUnit : in out ICD.ICDType; 
    Prin : in Principal.PrincipalPtr; 
    Request : in Network.NetworkMessage) return Network.NetworkMessage;

    -- Clear history
    procedure ResetHistory(IcdUnit : in out ICDType);
end ICD;