with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Intcode is
    procedure Get_Natural (File : in File_Type; Item : out Natural) is
        use Ada.Characters.Latin_1;
        subtype Digit is Character range '0' .. '9';
        C : Character;
    begin
        Item := 0;
        loop
            exit when End_of_File (File);
            Get (File, C);
            case C is
                when Digit =>
                    Item := (Item * 10) +
                            (Digit'Pos (C) -
                             Digit'Pos(Digit'First));
                when Space => null;
                when CR => null;
                when LF  => null;
                when HT => null;
                when others => return;
            end case;
        end loop;
    end Get_Natural;

    function Get_Digit(	N : Natural;
					   	Magnitude : Natural;
						Base : Positive := 10) return Natural is
    begin
        return (N / (Base ** Magnitude)) mod Base;
    end Get_Digit;

    procedure Read_Input (Value : out Natural) is
    begin
        Value := Natural'Value (Get_Line (Standard_Input));
    end Read_Input;

    procedure Load_Word (W : in Word) is
    begin
        Memory (Pointer) := W;
        if Pointer = Memory'Last then
            Pointer := Memory'First;
        else
            Pointer := Pointer + 1;
        end if;
    end Load_Word;

    procedure Load_From_File (Filename : in String) is
        File : File_Type;
        W : Word;
    begin
        Open (File, In_File, Filename);
        loop
            exit when End_of_File (File);
            Get_Natural (File, W);
            Intcode.Load_Word (W);
        end loop;
    end Load_From_File;

    procedure Dump is
    begin
        for I in Memory'Range loop
            Put ( Memory (I)'Image );
        end loop;
        Put_Line ("");
    end Dump;

    procedure Reset is
    begin
        Pointer := Memory'First;
    end Reset;

    procedure Fetch (W : out Word) is
    begin
        W := Memory (Pointer);
        Pointer := Pointer + 1;
    end Fetch;

    procedure Decode (  W : in Word;
                        Op : out Opcode;
                        Arguments : out Arguments_Vector.Vector) is
        Opcode_Num : Natural range 0 .. 99;
        Num_Args : Natural;
        Arg : Argument;
    begin
        Put_Line ("Decode " & W'Image);
        Opcode_Num := W mod 100;

        case Opcode_Num is
            when 99 => Op := Halt;
                       Num_Args := 0;
            when 1  => Op := Add;
                       Num_Args := 3;
            when 2  => Op := Multiply;
                       Num_Args := 3;
            when 3  => Op := Input;
                       Num_Args := 1;
            when 4  => Op := Output;
                       Num_Args := 1;
            when others => raise Invalid_Opcode with Opcode_Num'Image;
        end case;

        for I in 1 .. Num_Args loop
            case Get_Digit (W, (I + 2)) is
                when 0 => Arg.Mode := Position_Mode;
                when 1 => Arg.Mode := Immediate_Mode;
                when others => Arg.Mode := Position_Mode;
            end case;
            Fetch (Arg.Value);
            Arguments.Append (Arg);
        end loop;
    end Decode;

    procedure Execute (Op : in Opcode;
                       Args : in Arguments_Vector.Vector) is
    begin
        Put_Line ("Execute " & Op'Image & " ");
        for Arg of Args loop
            Put_Line ("    " & Arg.Mode'Image & " " & Arg.Value'Image);
        end loop;

        case Op is
            when Add =>
                Memory (Args.Element (3).Value) := Memory (Args.Element (1).Value) + Memory (Args.Element (2).Value);
            when Multiply =>
                Memory (Args.Element (3).Value) := Memory (Args.Element (1).Value) * Memory (Args.Element (2).Value);
            when Halt => raise Halted;
            when Input =>
                Read_Input (Memory (Args.Element (1).Value));
            when Output =>
                Put_Line (Memory (Args.Element (1).Value)'Image);
        end case;
    end Execute;

    procedure Run is
        W : Word;
        Op : Opcode;
        Args : Arguments_Vector.Vector;
    begin
        Reset;
        loop
            Fetch (W);
            Decode (W, Op, Args);
            if Op = Halt then
                return;
            end if;
            Execute (Op, Args);
        end loop;
    end Run;

    procedure Peek (Address : in Pointer_Type; Value : out Word) is
    begin
        Value := Memory (Address);
    end Peek;

    procedure Poke (Address : in Pointer_Type; Value : in Word) is
    begin
        Memory (Address) := Value;
    end Poke;

end Intcode;
