with Ada.Containers.Vectors;
with Stack;

package Intcode is
    subtype Word is Integer;

    type Opcode is (Halt, Add, Multiply, Input, Output);
    type Parameter_Mode is (Position_Mode, Immediate_Mode);
    type Memory_Type is array (Natural range 0 .. 8192) of Word;
    subtype Pointer_Type is Natural range Memory_Type'Range;

    type Argument is record
        Mode    : Parameter_Mode;
        Value   : Word;
    end record;

    package Arguments_Stack is new Stack
        (Element_Type => Argument);

    Invalid_Opcode          : exception;
    Invalid_Operand_Mode    : exception;
    Too_Many_Args           : exception;
    Halted                  : exception;

    procedure Load_Word (W : in Word);
    procedure Load_From_File (Filename : in String);
    procedure Dump;
    procedure Run;
    procedure Reset;

    procedure Fetch (W : out Word);

    procedure Decode (W : in Word;
                      Op : out Opcode;
                      Arguments : out Arguments_Stack.Stack);

    procedure Execute (Op : in Opcode;
                       Args : in out Arguments_Stack.Stack);

    procedure Peek (Address : in Pointer_Type; Value : out Word);
    procedure Poke (Address : in Pointer_Type; Value : in Word);

    Memory : Memory_Type;
private
    Pointer : Pointer_Type := Memory'First;
end Intcode;
