with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Advent1 is
    Line            : Unbounded_String;
    Mass            : Positive;
    Fuel_Required   : Positive;
    Sum             : Natural := 0;
begin
    loop
        exit when End_Of_File (Standard_Input);
        Line := Get_Line (Standard_Input);
        Mass := Positive'Value (To_String (Line));
        Fuel_Required := ((Mass / 3) - 2);
        Sum := Sum + Fuel_Required;
    end loop;

    Put_Line (Standard_Output, Sum'Image);
end Advent1;
