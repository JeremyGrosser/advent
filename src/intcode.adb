with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Latin_1;

package body Intcode is
    procedure Get_Integer (File : in File_Type; Item : out Integer) is
        use Ada.Characters.Latin_1;
        subtype Digit is Character range '0' .. '9';
        C : Character;
        Negative : Boolean := False;
    begin
        Item := 0;
        loop
            exit when End_of_File (File);
            Get (File, C);
            case C is
                when Digit =>
                    Item := (Item * 10) +
                            (Digit'Pos (C) -
                             Digit'Pos (Digit'First));
                when '-' =>
                    Negative := True;
                when Space => null;
                when CR => null;
                when LF  => null;
                when HT => null;
                when others =>
                    if Negative then
                        Item := Item * (-1);
                    end if;
                    return;
            end case;
        end loop;
    end Get_Integer;

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

    procedure Store (W : in Word; Pointer : Natural) is
    begin
        Put_Line ("Store [" & Pointer'Image & "] := " & W'Image);
        Memory (Pointer) := W;
    end Store;

    procedure Load_From_File (Filename : in String) is
        File : File_Type;
        W : Word;
    begin
        Open (File, In_File, Filename);
        loop
            exit when End_of_File (File);
            Get_Integer (File, W);
            Intcode.Load_Word (W);
        end loop;
    end Load_From_File;

    procedure Dump is
    begin
        for I in Memory'Range loop
            Put ( Memory (I)'Image & " " );
            if (I mod 32) = 0 then
                Put_Line ("");
                Put (I'Image & "  ");
            end if;
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
        Put_Line ("Fetch [" & Pointer'Image & "] = " & W'Image);
        Pointer := Pointer + 1;
    end Fetch;

    procedure Decode (  W : in Word;
                        Op : out Opcode;
                        Arguments : out Arguments_Stack.Stack) is
        Opcode_Num : Natural range 0 .. 99;
        Num_Args : Natural;
        Arg : Argument;
    begin
        Opcode_Num := W mod 100;

        case Opcode_Num is
            when 99 => Op := Halt;
                       Num_Args := 0;
            when 1  => Op := Add;
                       Num_Args := 2;
            when 2  => Op := Multiply;
                       Num_Args := 2;
            when 3  => Op := Input;
                       Num_Args := 0;
            when 4  => Op := Output;
                       Num_Args := 1;
            when others => raise Invalid_Opcode with Opcode_Num'Image;
        end case;

        for I in 0 .. (Num_Args - 1) loop
            case Get_Digit (W, (I + 2)) is
                when 0 => Arg.Mode := Position_Mode;
                when 1 => Arg.Mode := Immediate_Mode;
                when others => Arg.Mode := Position_Mode;
            end case;
            Fetch (Arg.Value);
            Arguments.Push (Arg);
        end loop;
    end Decode;

    procedure Execute (Op : in Opcode;
                       Args : in out Arguments_Stack.Stack) is
        Operand_1, Operand_2 : Argument;
        Destination, Result : Word;
    begin
        Put_Line ("Execute " & Op'Image);

        case Op is
            when Add =>
                Fetch (Destination);
                Args.Pop (Operand_1);
                Args.Pop (Operand_2);
                if not Args.Empty then
                    raise Too_Many_Args;
                end if;

                if Operand_1.Mode = Position_Mode then
                    Operand_1.Value := Memory (Operand_1.Value);
                end if;

                if Operand_2.Mode = Position_Mode then
                    Operand_2.Value := Memory (Operand_2.Value);
                end if;

                Result := Operand_1.Value + Operand_2.Value;
                Put_Line (Operand_1.Value'Image & " + " & Operand_2.Value'Image & " = " & Result'Image);
                Store (Result, Destination);
            when Multiply =>
                Fetch (Destination);
                Args.Pop (Operand_1);
                Args.Pop (Operand_2);

                if Operand_1.Mode = Position_Mode then
                    Operand_1.Value := Memory (Operand_1.Value);
                end if;

                if Operand_2.Mode = Position_Mode then
                    Operand_2.Value := Memory (Operand_2.Value);
                end if;

                Result := Operand_1.Value * Operand_2.Value;
                Put_Line (Operand_1.Value'Image & " * " & Operand_2.Value'Image & " = " & Result'Image);
                Store (Result, Destination);

            when Input =>
                Fetch (Destination);
                Read_Input (Result);
                Store (Result, Destination);
            when Output =>
                Args.Pop (Operand_1);
                if Operand_1.Mode = Position_Mode then
                    Operand_1.Value := Memory (Operand_1.Value);
                end if;
                Put_Line (Operand_1.Value'Image);
            when Halt => raise Halted;
        end case;
        if not Args.Empty then
            Put_Line (Args.Size'Image);
            raise Too_Many_Args;
        end if;
    end Execute;

    procedure Run is
        W : Word;
        Op : Opcode;
        Args : Arguments_Stack.Stack;
    begin
        Reset;
        loop
            Fetch (W);
            Decode (W, Op, Args);
            Put_Line ("Op=" & Op'Image & " Args=" & Args.Size'Image);
            if Op = Halt then
                return;
            end if;
            Execute (Op, Args);
            Put_Line ("");
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
