with Ada.Containers.Vectors;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day8_1 is
   Width  : constant Natural := Input.Read_Until (ASCII.LF)'Length;
   Height : constant Natural := Input.Length / Width;

   type Frequency is new Character;

   type Coordinate is record
      Y, X : Integer;
   end record;

   function In_Bounds
      (C : Coordinate)
      return Boolean
   is (C.Y in 0 .. Height - 1 and then C.X in 0 .. Width - 1);

   function "-" (Left, Right : Coordinate) return Coordinate
   is ((Left.Y - Right.Y, Left.X - Right.X));

   function "*" (Left : Integer; Right : Coordinate) return Coordinate
   is ((Left * Right.Y, Left * Right.X));

   package Coordinate_Vectors is new Ada.Containers.Vectors (Positive, Coordinate);
   use Coordinate_Vectors;
   type Frequency_Map is array (Frequency) of Coordinate_Vectors.Vector;

   Antennas  : Frequency_Map;
   Antinodes : Coordinate_Vectors.Vector;

   procedure Add_Antinodes
      (A, B : Coordinate)
   is
      Pos : Coordinate;
   begin
      if A = B then
         return;
      end if;

      Pos := 2 * A - B;
      if In_Bounds (Pos) and then not Contains (Antinodes, Pos) then
         Append (Antinodes, Pos);
      end if;
   end Add_Antinodes;

   Pos : Coordinate := (0, 0);
   Ch  : Character;
begin
   Input.Seek (0, Input.Seek_Start);

   while not Input.End_Of_Input loop
      Input.Get (Ch);
      case Ch is
         when ASCII.LF =>
            Pos.X := 0;
            Pos.Y := Pos.Y + 1;
         when '.' =>
            Pos.X := Pos.X + 1;
         when others =>
            Append (Antennas (Frequency (Ch)), Pos);
            Pos.X := Pos.X + 1;
      end case;
   end loop;

   for Freq in Frequency'Range loop
      for A of Antennas (Freq) loop
         for B of Antennas (Freq) loop
            Add_Antinodes (A, B);
         end loop;
      end loop;
   end loop;

   Output.Put (Natural (Length (Antinodes)));
end Day8_1;
