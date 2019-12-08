with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

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

    procedure Run is
        Op : Opcode;
        Args : Arguments_Type;
        Num_Args : Natural;
    begin
        Reset;
        loop
            Fetch (Args (0));
            Decode (Args (0), Op, Num_Args);
            if Op = Halt then
                return;
            end if;
            for I in 1 .. Num_Args loop
                Fetch (Args (I));
            end loop;
            Execute (Op, Args);
        end loop;
    end Run;

    procedure Fetch (W : out Word) is
    begin
        W := Memory (Pointer);
        Pointer := Pointer + 1;
    end Fetch;

    procedure Decode (W : in Word; Op : out Opcode; Num_Args : out Natural) is
    begin
        case W is
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
            when others => raise Invalid_Opcode with W'Image;
        end case;
    end Decode;

    procedure Read_Input (Value : out Natural) is
    begin
        Value := Natural'Value (Get_Line (Standard_Input));
    end Read_Input;

    procedure Execute (Op : in Opcode;
                       Args : in Arguments_Type) is
    begin
        --Put (Op'Image & Args(1)'Image & Args(2)'Image & Args(3)'Image);
        case Op is
            when Add => 
                Memory (Args (3)) := Memory (Args (1)) + Memory (Args (2));
            when Multiply => 
                Memory (Args (3)) := Memory (Args (1)) * Memory (Args (2));
            when Halt => raise Halted;
            when Input =>
                Read_Input (Memory (Args (1)));
            when Output =>
                Put_Line (Memory (Args (1))'Image);
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
