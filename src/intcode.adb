with Ada.Text_IO; use Ada.Text_IO;

package body Intcode is
    procedure Load_Word (W : in Word) is
    begin
        Memory (Pointer) := W;
        if Pointer = Memory'Last then
            Pointer := Memory'First;
        else
            Pointer := Pointer + 1;
        end if;
    end Load_Word;

    procedure Dump is
    begin
        for I in Memory'Range loop
            Put ( Memory (I)'Image );
        end loop;
        Put_Line ("");
    end Dump;

    procedure Run is
        Op : Opcode;
        Arg0, Arg1, Arg2, Arg3 : Word;
    begin
        Pointer := Memory'First;
        loop
            exit when Halted;
            Fetch (Arg0);
            Fetch (Arg1);
            Fetch (Arg2);
            Fetch (Arg3);
            Op := Decode (Arg0);
            --Put_Line(Op'Image & Arg1'Image & Arg2'Image & Arg3'Image);
            Execute (Op, Arg1, Arg2, Arg3);
        end loop;
    end Run;

    procedure Fetch (W : out Word) is
    begin
        W := Memory (Pointer);
        Pointer := Pointer + 1;
    end Fetch;

    function Decode (W : in Word) return Opcode is
    begin
        case W is
            when 99 => return Halt;
            when 1  => return Add;
            when 2  => return Multiply;
            when others => raise Invalid_Opcode with W'Image;
        end case;
    end Decode;

    procedure Execute (Op : in Opcode;
                       Arg1 : in Word;
                       Arg2 : in Word;
                       Arg3 : in Word) is
    begin
        case Op is
            when Add => 
                Memory (Arg3) := Memory (Arg1) + Memory (Arg2);
            when Multiply => 
                Memory (Arg3) := Memory (Arg1) * Memory (Arg2);
            when Halt => Halted := True;
        end case;
    end Execute;

    procedure Peek (Address : in Pointer_Type; Value : out Word) is
    begin
        Value := Memory (Address);
    end Peek;

    procedure Poke (Address : in Pointer_Type; Value : in Word) is
    begin
        Memory (Address) := Value;
    end Poke;

end Intcode;
