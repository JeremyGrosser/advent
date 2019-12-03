with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure Advent2 is
    subtype Digit is Character range '0' .. '9';
    type Opcode is (Halt, Add, Multiply);
    type Memory_Type is array (Natural range 0 .. 128) of Natural;
    subtype Pointer_Type is Natural range Memory_Type'Range;

    procedure Get_Natural (File : in File_Type; Item : out Natural) is
        use Ada.Characters.Latin_1;
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

    procedure Print_State (Memory : in Memory_Type) is
    begin
        for Pointer in Memory'Range loop
            Put ( Memory (Pointer)'Image );
        end loop;
        Put_Line ("");
    end Print_State;

    Op : Opcode;
    Arg1, Arg2, Arg3 : Natural;

    Memory : Memory_Type;
    Pointer : Pointer_Type := Memory'First;
begin
    loop
        exit when End_of_File (Standard_Input) or Pointer = Memory'Last;

        -- Read an opcode
        Get_Natural (Standard_Input, Memory (Pointer));
        Pointer := Pointer + 1;
    end loop;

    -- Fix 1202 alarm
    Memory (1) := 12;
    Memory (2) := 2;

    Pointer := Memory'First;

    loop
        case Memory (Pointer) is
            when 99 => Op := Halt;
            when 1  => Op := Add;
            when 2  => Op := Multiply;
            when others =>
                Put_Line ("Invalid opcode, abort");
                return;
        end case;
        Pointer := Pointer + 1;

        Arg1 := Memory (Pointer);
        Pointer := Pointer + 1;
        Arg2 := Memory (Pointer);
        Pointer := Pointer + 1;
        Arg3 := Memory (Pointer);
        Pointer := Pointer + 1;

        case Op is
            when Add =>
                Put_Line ("[" & Arg3'Image & "] = [" & Arg1'Image & "] + [" & Arg2'Image & "]");
                Memory (Arg3) := Memory (Arg1) + Memory (Arg2);
            when Multiply =>
                Put_Line ("[" & Arg3'Image & "] = [" & Arg1'Image & "] * [" & Arg2'Image & "]");
                Memory (Arg3) := Memory (Arg1) * Memory (Arg2);
            when Halt =>
                Put_Line ("HALT");
                Print_State (Memory);
                return;
        end case;
    end loop;
end Advent2;
