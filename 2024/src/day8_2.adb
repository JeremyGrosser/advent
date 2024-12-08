with Ada.Containers.Hashed_Sets;
with Ada.Containers;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day8_2 is
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

   function "+" (Left, Right : Coordinate) return Coordinate
   is ((Left.Y + Right.Y, Left.X + Right.X));

   --  This is not a good hash function, but it is *a* hash function.
   function Hash (Pos : Coordinate) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Pos.Y * Width + Pos.X));

   package Coordinate_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type        => Coordinate,
       Hash                => Hash,
       Equivalent_Elements => "=");
   use Coordinate_Sets;
   type Frequency_Map is array (Frequency) of Coordinate_Sets.Set;

   Antennas  : Frequency_Map;
   Antinodes : Coordinate_Sets.Set;

   procedure Add_Antinodes
      (A, B : Coordinate)
   is
      Distance : constant Coordinate := A - B;
      Pos : Coordinate;
   begin
      if A = B then
         return;
      end if;

      Pos := A;
      loop
         Pos := Pos + Distance;
         exit when not In_Bounds (Pos);
         Include (Antinodes, Pos);
      end loop;

      Pos := A;
      loop
         Pos := Pos - Distance;
         exit when not In_Bounds (Pos);
         Include (Antinodes, Pos);
      end loop;
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
            Include (Antennas (Frequency (Ch)), Pos);
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
end Day8_2;
