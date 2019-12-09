with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Latin_1;

package body Intcode is
    procedure Get_Word (File : in File_Type; Item : out Word) is
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
    end Get_Word;

    function Get_Digit( N : Natural;
                        Magnitude : Natural;
                        Base : Positive := 10) return Natural is
    begin
        return (N / (Base ** Magnitude)) mod Base;
    end Get_Digit;

    procedure Read_Input (Value : out Word) is
    begin
        Put("> ");
        Value := Word'Value (Get_Line (Standard_Input));
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

    procedure Store (W : in Word; Pointer : Pointer_Type) is
    begin
        --Put_Line ("Store [" & Pointer'Image & "] := " & W'Image);
        Memory (Pointer) := W;
        if Pointer > Max_Memory_Used then
            Max_Memory_Used := Pointer;
        end if;
    end Store;

    procedure Load_From_File (Filename : in String) is
        File : File_Type;
        W : Word;
    begin
        Open (File, In_File, Filename);
        loop
            exit when End_of_File (File);
            Get_Word (File, W);
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
        --Put_Line ("Fetch [" & Pointer'Image & "] = " & W'Image);
        if Pointer > Max_Memory_Used then
            Max_Memory_Used := Pointer;
        end if;
        Pointer := Pointer + 1;
    end Fetch;

    procedure Decode (  W : in Word;
                        Op : out Opcode;
                        Arguments : out Arguments_Stack.Stack) is
        Opcode_Num : Natural range 0 .. 99;
        Num_Args : Natural;
        Arg : Argument;
    begin
        Opcode_Num := Natural(W) mod 100;

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
            when 5  => Op := Jump_If_True;
                       Num_Args := 2;
            when 6  => Op := Jump_If_False;
                       Num_Args := 2;
            when 7  => Op := Less_Than;
                       Num_Args := 3;
            when 8  => Op := Equals;
                       Num_Args := 3;
            when 9  => Op := Set_Relative;
                       Num_Args := 1;
            when others => raise Invalid_Opcode with Opcode_Num'Image;
        end case;

        for I in 0 .. (Num_Args - 1) loop
            case Get_Digit (Natural (W), (I + 2)) is
                when 0 => Arg.Mode := Position_Mode;
                when 1 => Arg.Mode := Immediate_Mode;
                when 2 => Arg.Mode := Relative_Mode;
                when others => Arg.Mode := Position_Mode;
            end case;

            Fetch (Arg.Literal);

            case Arg.Mode is
                when Position_Mode =>
                    Arg.Value := Memory (Arg.Literal);
                when Immediate_Mode =>
                    Arg.Value := Arg.Literal;
                when Relative_Mode =>
                    Arg.Literal := Arg.Literal + Relative_Base;
                    Arg.Value := Memory (Arg.Literal);
            end case;

            Arguments.Push (Arg);
        end loop;
    end Decode;

    procedure Execute (Op : in Opcode;
                       Args : in out Arguments_Stack.Stack) is
        Operand_1, Operand_2, Operand_3 : Argument;
        Result : Word;
    begin
        --Put_Line ("Execute " & Op'Image);

        case Op is
            when Add =>
                Args.Pop (Operand_3);
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                Result := Operand_1.Value + Operand_2.Value;
                --Put_Line (Operand_1.Value'Image & " + " & Operand_2.Value'Image & " = " & Result'Image);
                Store (Result, Operand_3.Literal);
            when Multiply =>
                Args.Pop (Operand_3);
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                Result := Operand_1.Value * Operand_2.Value;
                --Put_Line (Operand_3.Value'Image & " := " & Operand_1.Value'Image & " * " & Operand_2.Value'Image & " = " & Result'Image);
                Store (Result, Operand_3.Literal);
            when Input =>
                Args.Pop (Operand_1);
                Read_Input (Result);
                Store (Result, Operand_1.Literal);
            when Output =>
                Args.Pop (Operand_1);
                Put_Line (Operand_1.Value'Image);
            when Jump_If_True =>
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                if Operand_1.Value /= 0 then
                    Pointer := Operand_2.Value;
                end if;
            when Jump_If_False =>
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                if Operand_1.Value = 0 then
                    Pointer := Operand_2.Value;
                end if;
            when Less_Than =>
                Args.Pop (Operand_3);
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                if Operand_1.Value < Operand_2.Value then
                    Store (1, Operand_3.Literal);
                else
                    Store (0, Operand_3.Literal);
                end if;
            when Equals =>
                Args.Pop (Operand_3);
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                if Operand_1.Value = Operand_2.Value then
                    Store (1, Operand_3.Literal);
                else
                    Store (0, Operand_3.Literal);
                end if;
            when Set_Relative =>
                Args.Pop (Operand_1);
                --Put_Line ("Relative_Base := " & Relative_Base'Image & " + " & Operand_1.Value'Image);
                Relative_Base := Relative_Base + Operand_1.Value;
                --Put_Line (Relative_Base'Image);
            when Halt => 
                Put_Line ("Memory Used: " & Max_Memory_Used'Image);
                raise Halted;
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
            Execute (Op, Args);
            --Put_Line ("");
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
