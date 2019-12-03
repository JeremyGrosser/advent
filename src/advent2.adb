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

    procedure Test (A : in Intcode.Word; B : in Intcode.Word) is
        Word : Intcode.Word;
    begin
        Intcode.Reset;
        loop
            exit when End_of_File (Standard_Input);

            -- Read an opcode
            Get_Natural (Standard_Input, Word);
            Intcode.Load_Word (Word);
        end loop;

        Intcode.Poke (1, A);
        Intcode.Poke (2, B);

        Intcode.Run;
        Intcode.Peek(0, Word);
        if Word = 19690720 then
            Put_Line("A = " & A'Image);
            Put_Line("B = " & B'Image);
            return;
        end if;
    exception
        when Intcode.Invalid_Opcode => return;
    end Test;

begin
    for A in 0 .. 65536 loop
        for B in 0 .. 65536 loop
            Test (A, B);
        end loop;
    end loop;
end Advent2;
