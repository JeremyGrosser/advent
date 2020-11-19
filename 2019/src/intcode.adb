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

    procedure Read_Input (M : in out Machine; Value : out Word) is
    begin
       if M.Input.Is_Empty then
          raise Buffer_Underrun;
       else
          Value := M.Input.First_Element;
          M.Input.Delete_First;
       end if;
    end Read_Input;

    procedure Load_Word (
        M : in out Machine;
        W : in Word) is
    begin
        M.Memory (M.Pointer) := W;
        M.Pointer := M.Pointer + 1;
    end Load_Word;

    procedure Store (
        M : in out Machine;
        W : in Word;
        Pointer : in Pointer_Type) is
    begin
        --Put_Line ("Store [" & Pointer'Image & "] := " & W'Image);
        M.Memory (Pointer) := W;
        if Pointer > M.Max_Memory_Used then
            M.Max_Memory_Used := Pointer;
        end if;
    end Store;

    procedure Load_From_File (
        M : in out Machine;
        Filename : in String) is
        File : File_Type;
        W : Word;
    begin
        Open (File, In_File, Filename);
        loop
            exit when End_of_File (File);
            Get_Word (File, W);
            M.Load_Word (W);
        end loop;
        Close (File);
    end Load_From_File;

    procedure Reset (M : in out Machine) is
    begin
        M.Pointer := M.Memory'First;
        M.Input.Clear;
        M.Output.Clear;
    end Reset;

    procedure Clear_Memory (M : in out Machine) is
    begin
       M.Memory := (others => 0);
    end Clear_Memory;

    procedure Fetch (
        M : in out Machine;
        W : out Word) is
    begin
        W := M.Memory (M.Pointer);
        --Put_Line ("Fetch [" & M.Pointer'Image & "] = " & W'Image);
        if M.Pointer > M.Max_Memory_Used then
            M.Max_Memory_Used := M.Pointer;
        end if;
        M.Pointer := M.Pointer + 1;
    end Fetch;

    procedure Decode (
        M : in out Machine;
        W : in Word;
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
        --Put_Line ("Op=" & Op'Image);

        for I in 0 .. (Num_Args - 1) loop
            case Get_Digit (Natural (W), (I + 2)) is
                when 0 => Arg.Mode := Position_Mode;
                when 1 => Arg.Mode := Immediate_Mode;
                when 2 => Arg.Mode := Relative_Mode;
                when others => Arg.Mode := Position_Mode;
            end case;

            M.Fetch (Arg.Literal);

            case Arg.Mode is
                when Position_Mode =>
                    Arg.Value := M.Memory (Arg.Literal);
                when Immediate_Mode =>
                    Arg.Value := Arg.Literal;
                when Relative_Mode =>
                    Arg.Literal := Arg.Literal + M.Relative_Base;
                    Arg.Value := M.Memory (Arg.Literal);
            end case;

            --Put_Line ("Arg Mode=" & Arg.Mode'Image & " Literal=" & Arg.Literal'Image & " Value=" & Arg.Value'Image);
            Arguments.Push (Arg);
        end loop;
    end Decode;

    procedure Execute (
        M : in out Machine;
        Op : in Opcode;
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
                M.Store (Result, Operand_3.Literal);
            when Multiply =>
                Args.Pop (Operand_3);
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                Result := Operand_1.Value * Operand_2.Value;
                --Put_Line (Operand_3.Value'Image & " := " & Operand_1.Value'Image & " * " & Operand_2.Value'Image & " = " & Result'Image);
                M.Store (Result, Operand_3.Literal);
            when Input =>
                Args.Pop (Operand_1);
                Read_Input (M, Result);
                M.Store (Result, Operand_1.Literal);
            when Output =>
                Args.Pop (Operand_1);
                M.Output.Append (Operand_1.Value);
            when Jump_If_True =>
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                if Operand_1.Value /= 0 then
                    M.Pointer := Operand_2.Value;
                end if;
            when Jump_If_False =>
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                if Operand_1.Value = 0 then
                    M.Pointer := Operand_2.Value;
                end if;
            when Less_Than =>
                Args.Pop (Operand_3);
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                if Operand_1.Value < Operand_2.Value then
                    M.Store (1, Operand_3.Literal);
                else
                    M.Store (0, Operand_3.Literal);
                end if;
            when Equals =>
                Args.Pop (Operand_3);
                Args.Pop (Operand_2);
                Args.Pop (Operand_1);
                if Operand_1.Value = Operand_2.Value then
                    M.Store (1, Operand_3.Literal);
                else
                    M.Store (0, Operand_3.Literal);
                end if;
            when Set_Relative =>
                Args.Pop (Operand_1);
                --Put_Line ("Relative_Base := " & Relative_Base'Image & " + " & Operand_1.Value'Image);
                M.Relative_Base := M.Relative_Base + Operand_1.Value;
                --Put_Line (Relative_Base'Image);
            when Halt => 
                raise Halted;
        end case;
        if not Args.Empty then
            Put_Line (Args.Size'Image);
            raise Too_Many_Args;
        end if;
    end Execute;

    procedure Step (M : in out Machine) is
        W : Word;
        Op : Opcode;
        Args : Arguments_Stack.Stack;
    begin
        M.Fetch (W);
        M.Decode (W, Op, Args);
        M.Execute (Op, Args);
        M.Cycle_Count := M.Cycle_Count + 1;
    end Step;

    procedure Run (M : in out Machine) is
    begin
        loop
            M.Step;
        end loop;
    exception
        when Halted =>
            Put_Line ("HALT");
            M.Print_Summary;
    end Run;

    procedure Print_Summary (M : Machine) is
    begin
        Put_Line ("--------------------------");
        Put_Line ("Memory Used: " & M.Max_Memory_Used'Image);
        Put_Line ("Cycle count: " & M.Cycle_Count'Image);
    end Print_Summary;

    procedure Run_Until_Output (M : in out Machine)
    is
    begin
       loop
          exit when not M.Output.Is_Empty;
          M.Step;
       end loop;
    end Run_Until_Output;

    procedure Peek (
        M       : in Machine;
        Address : in Pointer_Type;
        Value   : out Word) is
    begin
        Value := M.Memory (Address);
    end Peek;

    procedure Poke (
        M       : out Machine;
        Address : in Pointer_Type;
        Value : in Word) is
    begin
        M.Memory (Address) := Value;
    end Poke;

    procedure Write_Input (
       M : in out Machine;
       W : in Word) is
    begin
       M.Input.Append (W);
    end Write_Input;

    procedure Read_Output (
       M : in out Machine;
       W : out Word) is
    begin
       W := M.Output.First_Element;
       M.Output.Delete_First;
    end Read_Output;

    function Has_Output (M : Machine) return Boolean is
    begin
       return not M.Output.Is_Empty;
    end Has_Output;

end Intcode;
