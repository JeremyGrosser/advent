pragma Warnings (Off, "array aggregate using () is an obsolescent syntax, use [] instead");
pragma Ada_2022;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Text_IO;
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

   function "<" (Left, Right : Coordinate) return Boolean
   is (Left.Y < Right.Y or else Left.X < Right.X);

   function "=" (Left, Right : Coordinate) return Boolean
   is (Left.Y = Right.Y and then Left.X = Right.X);

   function "-" (Left, Right : Coordinate) return Coordinate
   is ((Left.Y - Right.Y, Left.X - Right.X));

   function "*" (Left : Integer; Right : Coordinate) return Coordinate
   is ((Left * Right.Y, Left * Right.X));

   package Coordinate_Sets is new Ada.Containers.Vectors (Positive, Coordinate);
   use Coordinate_Sets;
   type Frequency_Map is array (Frequency) of Coordinate_Sets.Vector;

   Antennas  : Frequency_Map;
   Antinodes : Coordinate_Sets.Vector;

   procedure Add_Antinodes
      (A, B : Coordinate)
   is
      Pos : Coordinate;
   begin
      if A = B then
         return;
      end if;

      Pos := 2 * A - B;
      --  if In_Bounds (Pos) then
      if In_Bounds (Pos) and then not Contains (Antinodes, Pos) then
         Append (Antinodes, Pos);
      end if;
      --  end if;

      Pos := 2 * B - A;
      --  if In_Bounds (Pos) then
      if In_Bounds (Pos) and then not Contains (Antinodes, Pos) then
         Append (Antinodes, Pos);
      end if;
      --  end if;
   end Add_Antinodes;

   procedure Plot
      (S : Coordinate_Sets.Vector)
   is
      Map : array (0 .. Height - 1, 0 .. Width - 1) of Natural := (others => (others => 0));
   begin
      --  for Freq in Frequency'Range loop
      --     for Pos of Antennas (Freq) loop
      --        Map (Pos.Y, Pos.X) := @ + 1;
      --     end loop;
      --  end loop;

      for Pos of S loop
         if In_Bounds (Pos) then
            Map (Pos.Y, Pos.X) := @ + 1;
         end if;
      end loop;

      for Y in Map'Range (1) loop
         for X in Map'Range (2) loop
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Character'Val (Character'Pos ('0') + Map (Y, X)));
         end loop;
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end loop;
      Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end Plot;

   Pos : Coordinate := (0, 0);
   Ch  : Character;
   Sum : Natural := 0;
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
            --  Add_Antinodes (B, A);
         end loop;
      end loop;
   end loop;

   for Pos of Antinodes loop
      if In_Bounds (Pos) then
         Output.Log (Pos'Image);
         Sum := Sum + 1;
      else
         Output.Log ("OOB: " & Pos'Image);
      end if;
   end loop;

   Output.Log (Natural (Length (Antinodes)));

   Plot (Antinodes);

   Output.Put (Sum);
end Day8_1;
