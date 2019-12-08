with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure Advent4 is
    subtype Password is Natural range 111111 .. 999999;

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

    function Get_Digit(N : Natural; Magnitude : Natural) return Natural is
    begin
        return (N / (10 ** Magnitude)) mod 10;
    end Get_Digit;

    function Has_Adjacent_Digits (N : Password) return Boolean is
        Adjacent_Digits : Natural := 0;
        Digit, Last_Digit : Natural := 0;
    begin
        Last_Digit := Get_Digit (N, 5);
        for Magnitude in reverse 0 .. 4 loop
            Digit := Get_Digit (N, Magnitude);
            if Digit = Last_Digit then
                Adjacent_Digits := Adjacent_Digits + 1;
            end if;
            Last_Digit := Digit;
        end loop;
        return Adjacent_Digits > 0;
    end Has_Adjacent_Digits;

    function Never_Decrease (N : Password) return Boolean is
        Digit, Last_Digit : Natural;
    begin
        --Put_Line (N'Image);
        Last_Digit := Get_Digit (N, 5);
        for Magnitude in reverse 0 .. 4 loop
            Digit := Get_Digit (N, Magnitude);
            --Put_Line (Magnitude'Image & " " & Digit'Image & " " & Last_Digit'Image);
            if Digit < Last_Digit then
                return False;
            end if;
            Last_Digit := Digit;
        end loop;
        return True;
    end Never_Decrease;

    Range_Start, Range_End : Password;
    Count : Natural := 0;
begin
    Get_Natural (Standard_Input, Range_Start);
    Get_Natural (Standard_Input, Range_End);

    for I in Range_Start .. Range_End loop
        Put_Line (I'Image & " " & Has_Adjacent_Digits (I)'Image & " " & Never_Decrease (I)'Image);
        if Has_Adjacent_Digits (I) and Never_Decrease (I) then
            Count := Count + 1;
        end if;
    end loop;

    Put_Line (Count'Image);
end Advent4;
