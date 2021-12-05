with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Maps;
with Ada.Streams;

procedure D5_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

   type Coordinate is record
      X, Y : Integer;
   end record;

   type Segment is record
      From, To : Coordinate;
   end record;

   procedure Read_Coordinate
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Coordinate);

   procedure Write_Coordinate
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Coordinate);

   for Coordinate'Read use Read_Coordinate;
   for Coordinate'Write use Write_Coordinate;

   procedure Read_Segment
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Segment);

   procedure Write_Segment
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Segment);

   for Segment'Read use Read_Segment;
   for Segment'Write use Write_Segment;

   --------------------------------------------------------------------

   procedure Read_Coordinate
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Coordinate)
   is
      use Ada.Strings.Maps;
      Comma : constant Character_Set := To_Set (',');
   begin
      Item.X := Get (Stream, Delimiter => Comma);
      Item.Y := Get (Stream, Delimiter => Whitespace);
   end Read_Coordinate;

   procedure Write_Coordinate
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Coordinate)
   is
   begin
      Put (Stream, Item.X);
      Character'Write (Stream, ',');
      Put (Stream, Item.Y);
   end Write_Coordinate;

   procedure Read_Segment
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Segment)
   is
   begin
      Coordinate'Read (Stream, Item.From);
      declare
         Arrow : constant String := Read_Until (Stream, Whitespace)
            with Unreferenced;
      begin
         null;
      end;
      Coordinate'Read (Stream, Item.To);
   end Read_Segment;

   procedure Write_Segment
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Segment)
   is
   begin
      Coordinate'Write (Stream, Item.From);
      String'Write (Stream, " -> ");
      Coordinate'Write (Stream, Item.To);
   end Write_Segment;

   function "<"
      (Left, Right : Coordinate)
      return Boolean
   is (Left.X < Right.X or Left.Y < Right.Y);

   --------------------------------------------------------------------

   package Coordinate_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type        => Coordinate,
       Element_Type    => Natural);

   function Limit
      (Field : Coordinate_Maps.Map)
      return Coordinate
   is
      use Coordinate_Maps;
      L : Coordinate := (0, 0);
      C : Coordinate;
   begin
      for Cursor in Iterate (Field) loop
         C := Key (Cursor);
         if C.X > L.X then
            L.X := C.X;
         end if;
         if C.Y > L.Y then
            L.Y := C.Y;
         end if;
      end loop;
      return L;
   end Limit;

   procedure Write_Coordinate_Map
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Coordinate_Maps.Map)
   is
      use Coordinate_Maps;
      Size : constant Coordinate := Limit (Item);
      N    : Natural;
      Pos  : Coordinate;
   begin
      for Y in 0 .. Size.Y loop
         for X in 0 .. Size.X loop
            Pos := (X, Y);
            if Contains (Item, Pos) then
               N := Element (Item, Pos);
            else
               N := 0;
            end if;

            case N is
               when 0 =>
                  Character'Write (Stream, '.');
               when 1 .. 9 =>
                  Character'Write (Stream, Character'Val (16#30# + N));
               when others =>
                  Character'Write (Stream, '^');
            end case;
         end loop;
         New_Line (Stream);
      end loop;
      New_Line (Stream);
   end Write_Coordinate_Map;

   procedure Increment
      (Field : in out Coordinate_Maps.Map;
       Pos   : Coordinate)
   is
      use Coordinate_Maps;
      Height : Natural;
   begin
      if Contains (Field, Pos) then
         Height := Element (Field, Pos) + 1;
      else
         Height := 1;
      end if;
      Include (Field, Pos, Height);
   end Increment;

   procedure Swap
      (A, B : in out Integer)
   is
      T : Integer;
   begin
      T := A;
      A := B;
      B := T;
   end Swap;

   procedure Line
      (Field    : in out Coordinate_Maps.Map;
       From, To : Coordinate)
   is
      F : Coordinate := From;
      T : Coordinate := To;
   begin
      if F.X > T.X then
         Swap (F.X, T.X);
      end if;

      if F.Y > T.Y then
         Swap (F.Y, T.Y);
      end if;

      for X in F.X .. T.X loop
         for Y in F.Y .. T.Y loop
            Increment (Field, (X, Y));
         end loop;
      end loop;
   end Line;

   Seafloor : Coordinate_Maps.Map := Coordinate_Maps.Empty_Map;
   Vent     : Segment;
   From, To : Integer;
begin
   New_Line (Error);
   while not End_Of_Input loop
      Segment'Read (Input, Vent);

      --  Only consider vertical and horizontal lines (ignore diagonal)
      if Vent.From.X = Vent.To.X or Vent.From.Y = Vent.To.Y then
         Segment'Write (Error, Vent);
         New_Line (Error);
         Line (Seafloor, Vent.From, Vent.To);
      end if;
   end loop;

   Write_Coordinate_Map (Error, Seafloor);

   declare
      use Coordinate_Maps;
      Count : Natural := 0;
   begin
      for Cursor in Iterate (Seafloor) loop
         if Element (Cursor) > 2 then
            Count := Count + 1;
         end if;
      end loop;
      Put (Output, Count);
      New_Line (Output);
   end;
end D5_1;
