package Intcode is
    subtype Word is Natural;

    type Opcode is (Halt, Add, Multiply);
    type Memory_Type is array (Natural range 0 .. 128) of Word;
    subtype Pointer_Type is Natural range Memory_Type'Range;

    Invalid_Opcode : exception;

    procedure Load_Word (W : in Word);
    procedure Dump;
    procedure Run;

    procedure Fetch (W : out Word);

    function Decode (W : in Word) return Opcode;

    procedure Execute (Op : in Opcode;
                       Arg1 : in Word;
                       Arg2 : in Word;
                       Arg3 : in Word);

    procedure Peek (Address : in Pointer_Type; Value : out Word);
    procedure Poke (Address : in Pointer_Type; Value : in Word);

private
    Memory : Memory_Type;
    Pointer : Pointer_Type := Memory'First;
    Halted : Boolean := False;
end Intcode;
