package Intcode is
    subtype Word is Natural;

    type Opcode is (Halt, Add, Multiply);
    type Memory_Type is array (Natural range 0 .. 128) of Word;
    subtype Pointer_Type is Natural range Memory_Type'Range;
    type Arguments_Type is array (Natural range 0 .. 4) of Word;

    Invalid_Opcode : exception;
    Halted : exception;

    procedure Load_Word (W : in Word);
    procedure Dump;
    procedure Run;
    procedure Reset;

    procedure Fetch (W : out Word);

    procedure Decode (W : in Word;
                      Op : out Opcode;
                      Num_Args : out Natural);

    procedure Execute (Op : in Opcode;
                       Args : in Arguments_Type);

    procedure Peek (Address : in Pointer_Type; Value : out Word);
    procedure Poke (Address : in Pointer_Type; Value : in Word);

    Memory : Memory_Type;
private
    Pointer : Pointer_Type := Memory'First;
end Intcode;
