with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Intcode;

procedure Advent5 is
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

    Word : Intcode.Word;
    Program_File : File_Type;
begin
    Open(Program_File, In_File, "input/advent5-echo");

    loop
        exit when End_of_File (Program_File);

        -- Read an opcode
        Get_Natural (Program_File, Word);
        Intcode.Load_Word (Word);
    end loop;

    Intcode.Run;
    --Intcode.Dump;
end Advent5;
