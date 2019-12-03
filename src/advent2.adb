with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Intcode;

procedure Advent2 is
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
begin
    loop
        exit when End_of_File (Standard_Input);

        -- Read an opcode
        Get_Natural (Standard_Input, Word);
        Intcode.Load_Word (Word);
    end loop;

    -- Fix 1202 alarm
    Intcode.Poke (1, 12);
    Intcode.Poke (2, 2);

    Intcode.Run;

    Intcode.Dump;
end Advent2;
