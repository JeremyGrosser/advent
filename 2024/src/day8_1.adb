pragma Ada_2022;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day8_1 is

   Width  : constant := 12;
   Height : constant := 12;

   type Frequency is new Character;

   type Coordinate is record
      Y, X : Integer;
   end record;

   function "<" (Left, Right : Coordinate) return Boolean
   is (Left.Y < Right.Y or else Left.X < Right.X);

   function "-" (Left, Right : Coordinate) return Coordinate
   is ((Left.Y - Right.Y, Left.X - Right.X));

   function "+" (Left, Right : Coordinate) return Coordinate
   is ((Left.Y + Right.Y, Left.X + Right.X));

   --  {
   --     'f': {(1, 2), (3, 4), ...},
   --  }
   package Coordinate_Sets is new Ada.Containers.Ordered_Sets (Coordinate);

   type Frequency_Map is array (Frequency) of Coordinate_Sets.Set;

   Antennas : Frequency_Map;
   Antinodes : Coordinate_Sets.Set;

   procedure Add_Antenna
      (Pos  : Coordinate;
       Freq : Frequency)
   is
   begin
      Coordinate_Sets.Include (Antennas (Freq), Pos);
   end Add_Antenna;

   procedure Add_Antinodes
      (A, B : Coordinate)
   is
      use Coordinate_Sets;
      Pos : Coordinate;
   begin
      if A = B then
         return;
      end if;

      Output.Log (A'Image & B'Image);

      if A.X < B.X then
         Pos.X := A.X - abs (A.X - B.X);
      elsif A.X > B.X then
         Pos.X := A.X + abs (A.X - B.X);
      else
         Pos.X := A.X;
      end if;

      if A.Y < B.Y then
         Pos.Y := A.Y - abs (A.Y - B.Y);
      elsif A.Y > B.Y then
         Pos.Y := A.Y + abs (A.Y - B.Y);
      else
         Pos.Y := A.Y;
      end if;

      if Pos.X in 0 .. (Width - 1) and then Pos.Y in 0 .. (Height - 1) then
         Include (Antinodes, Pos);
      end if;
   end Add_Antinodes;

   procedure Plot
      (S : Coordinate_Sets.Set)
   is
   begin
      for Y in 0 .. 11 loop
         for X in 0 .. 11 loop
            if Coordinate_Sets.Contains (S, Coordinate'(Y, X)) then
               Ada.Text_IO.Put ('#');
            else
               Ada.Text_IO.Put ('.');
            end if;
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Plot;

   Sum : Natural := 0;
   Pos : Coordinate := (0, 0);
   Ch  : Character;
begin
   while not Input.End_Of_Input loop
      Input.Get (Ch);
      case Ch is
         when ASCII.LF =>
            Pos.X := 0;
            Pos.Y := Pos.Y + 1;
         when '.' =>
            Pos.X := Pos.X + 1;
         when others =>
            Add_Antenna (Pos, Frequency (Ch));
            Pos.X := Pos.X + 1;
      end case;
   end loop;

   Plot (Antennas ('a'));
   Ada.Text_IO.New_Line;

   for Freq in Frequency'Range loop
      for A of Antennas (Freq) loop
         for B of Antennas (Freq) loop
            Add_Antinodes (A, B);
         end loop;
      end loop;
   end loop;

   Plot (Antinodes);

   Output.Put (Sum);
end Day8_1;
