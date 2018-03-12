with Principal;
with HRM;
with Network;
with ImpulseGenerator;

package ICD is

    type ICDType is 
    record
        -- the mode of the ICD
        IsOn : Boolean;
        Patient : Principal;
        Cardiologist : Principal;
        ClinicalAssistant : Principal;
    end record;

    procedure Init(Icd : out ICDType);

    procedure On(Icd : out ICDType; Prin : in Principal);

    procedure Off(Icd : out ICDType; Prin : in Principal);

end ICD;