with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Advent1 is
    Line            : Unbounded_String;
    Mass            : Positive;
    Fuel            : Natural;
    Sum             : Natural := 0;

    function Fuel_Requirement (Mass : Positive) return Natural is
        Fuel : Natural;
    begin
        Fuel := Mass / 3;
        if Fuel >= 2 then
            return (Fuel - 2);
        else
            return 0;
        end if;
    end Fuel_Requirement;
begin
    loop
        exit when End_Of_File (Standard_Input);
        Line := Get_Line (Standard_Input);
        Mass := Positive'Value (To_String (Line));

        Fuel := Mass;
        loop
            exit when Fuel = 0;
            Fuel := Fuel_Requirement (Fuel);
            Sum := Sum + Fuel;
        end loop;
    end loop;

    Put_Line (Standard_Output, Sum'Image);
end Advent1;
