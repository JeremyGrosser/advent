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

    procedure Test (Noun : in Intcode.Word; Verb : in Intcode.Word) is
        Word : Intcode.Word;
    begin
        Intcode.Reset;

        Intcode.Poke (1, Noun);
        Intcode.Poke (2, Verb);

        begin
            Intcode.Run;
        exception
            when Intcode.Invalid_Opcode => null;
        end;

        Intcode.Peek(0, Word);
        --Put_Line(Noun'Image & Verb'Image & Word'Image);
        if Word = 19690720 then
            Put_Line("Noun = " & Noun'Image);
            Put_Line("Verb = " & Verb'Image);
            Word := 100 * Noun + Verb;
            Put_Line(Word'Image);
        end if;
    end Test;

    Word : Intcode.Word;
    Program : Intcode.Memory_Type;
begin
    loop
        exit when End_of_File (Standard_Input);

        -- Read an opcode
        Get_Natural (Standard_Input, Word);
        Intcode.Load_Word (Word);
    end loop;
    Program := Intcode.Memory;

    for Noun in 0 .. 99 loop
        for Verb in 0 .. 99 loop
            Intcode.Memory := Program;
            Test (Noun, Verb);
        end loop;
    end loop;
end Advent2;
