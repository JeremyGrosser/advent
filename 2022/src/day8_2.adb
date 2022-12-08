with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.IO_Exceptions;

procedure Day8_2 is
   function Line_Count
      return Natural
   is
      Count : Natural := 0;
   begin
      while not End_Of_Input loop
         declare
            Line : constant String := Read_Until (Input, ASCII.LF);
            pragma Unreferenced (Line);
         begin
            Count := Count + 1;
         end;
      end loop;

      Seek (Input, 0, Seek_Start);
      return Count;
   end Line_Count;

   function Line_Length
      return Natural
   is
      Line : constant String := Read_Until (Input, ASCII.LF);
   begin
      Seek (Input, 0, Seek_Start);
      return Line'Length;
   end Line_Length;

   generic
      Width  : Natural;
      Height : Natural;
   package Forest is
      subtype Row is Natural range 0 .. Height - 1;
      subtype Column is Natural range 0 .. Width - 1;
      subtype Tree_Height is Natural range 0 .. 9;
      type Heightmap is array (Row, Column) of Tree_Height;

      procedure Read_Heightmap
         (Item   : out Heightmap);

      function Scenic_Score
         (Map : Heightmap;
          Y   : Row;
          X   : Column)
          return Natural;
   end Forest;

   package body Forest is
      procedure Read_Heightmap
         (Item   : out Heightmap)
      is
         Y  : Row := Row'First;
         X  : Column := Column'First;
         Ch : Character;
      begin
         while not End_Of_Input loop
            Character'Read (Input, Ch);
            case Ch is
               when '0' .. '9' =>
                  Item (Y, X) := Character'Pos (Ch) - Character'Pos ('0');
                  if X = Column'Last then
                     X := Column'First;
                     if Y /= Row'Last then
                        Y := Y + 1;
                     end if;
                  else
                     X := X + 1;
                  end if;
               when ASCII.LF =>
                  null;
               when others =>
                  raise Program_Error with "Invalid input";
            end case;
         end loop;
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
      end Read_Heightmap;

      function Scenic_Score
         (Map : Heightmap;
          Y   : Row;
          X   : Column)
          return Natural
      is
         H : constant Tree_Height := Map (Y, X);
         Up, Down, Left, Right : Natural := 0;
      begin
         --  Up
         for RY in reverse Row'First .. Y - 1 loop
            Up := Up + 1;
            if Map (RY, X) >= H then
               exit;
            end if;
         end loop;

         --  Down
         for RY in Y + 1 .. Row'Last loop
            Down := Down + 1;
            if Map (RY, X) >= H then
               exit;
            end if;
         end loop;

         --  Left
         for RX in reverse Column'First .. X - 1 loop
            Left := Left + 1;
            if Map (Y, RX) >= H then
               exit;
            end if;
         end loop;

         --  Right
         for RX in X + 1 .. Column'Last loop
            Right := Right + 1;
            if Map (Y, RX) >= H then
               exit;
            end if;
         end loop;

         return Up * Down * Left * Right;
      end Scenic_Score;
   end Forest;

   package Sherwood is new Forest
      (Width  => Line_Length,
       Height => Line_Count);

   Trees : Sherwood.Heightmap;
   Best  : Natural := 0;
   Score : Natural;
begin
   Sherwood.Read_Heightmap (Trees);

   for Y in Trees'Range (1) loop
      for X in Trees'Range (2) loop
         Score := Sherwood.Scenic_Score (Trees, Y, X);
         if Score > Best then
            Best := Score;
         end if;
      end loop;
   end loop;

   Put (Output, Best);
   New_Line (Output);
end Day8_2;
