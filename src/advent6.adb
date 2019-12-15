with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;
with Orbit_Map;

procedure Advent6 is
    package SF renames Ada.Strings.Fixed;

    Line : String (1 .. 7);
    Last : Natural;
    A, B : Orbit_Map.Orbital;
begin
    loop
        exit when End_of_File (Standard_Input);
        Get_Line (Standard_Input, Line, Last);
        SF.Move (
            Source  => Line,
            Target  => A,
            Drop    => Right,
            Justify => Left);
        SF.Move (
            Source  => Line,
            Target  => B,
            Drop    => Left,
        Justify => Right);

        Orbit_Map.Add (A, B);
    end loop;
    
    Put_Line (Orbit_Map.Count ("COM")'Image);
end Advent6;
