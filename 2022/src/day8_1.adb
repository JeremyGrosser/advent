with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.IO_Exceptions;

procedure Day8_1 is
   function Line_Count
      return Natural
   is
      Save : constant Seek_Offset := Tell (Input);
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

      Seek (Input, Save, Seek_Start);
      return Count;
   end Line_Count;

   function Line_Length
      return Natural
   is
      Save : constant Seek_Offset := Tell (Input);
      Line : constant String := Read_Until (Input, ASCII.LF);
   begin
      Seek (Input, Save, Seek_Start);
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

      function Is_Visible
         (Map : Heightmap;
          Y   : Row;
          X   : Column)
          return Boolean;
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

      function Is_Visible
         (Map : Heightmap;
          Y   : Row;
          X   : Column)
          return Boolean
      is
         Result : Boolean;
         H : constant Tree_Height := Map (Y, X);
      begin
         if X = Column'First or else
            X = Column'Last or else
            Y = Row'First or else
            Y = Row'Last
         then
            return True;
         end if;

         --  Up
         Result := True;
         for RY in Row'First .. Y - 1 loop
            if Map (RY, X) >= H then
               Result := False;
               exit;
            end if;
         end loop;
         if Result then
            return True;
         end if;

         --  Down
         Result := True;
         for RY in Y + 1 .. Row'Last loop
            if Map (RY, X) >= H then
               Result := False;
               exit;
            end if;
         end loop;
         if Result then
            return True;
         end if;

         --  Left
         Result := True;
         for RX in Column'First .. X - 1 loop
            if Map (Y, RX) >= H then
               Result := False;
               exit;
            end if;
         end loop;
         if Result then
            return True;
         end if;

         --  Right
         Result := True;
         for RX in X + 1 .. Column'Last loop
            if Map (Y, RX) >= H then
               Result := False;
               exit;
            end if;
         end loop;
         if Result then
            return True;
         end if;

         return False;
      end Is_Visible;
   end Forest;

   package Sherwood is new Forest
      (Width  => Line_Length,
       Height => Line_Count);

   Trees : Sherwood.Heightmap;
   Sum   : Natural := 0;
begin
   Sherwood.Read_Heightmap (Trees);

   for Y in Trees'Range (1) loop
      for X in Trees'Range (2) loop
         if Sherwood.Is_Visible (Trees, Y, X) then
            Sum := Sum + 1;
         end if;
      end loop;
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day8_1;
