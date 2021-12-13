with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Strings.Maps;
with Ada.Streams;

procedure D8_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

   subtype Segment is Character range 'a' .. 'g';
   type Segments is array (Segment) of Boolean;

   procedure Read
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Segments);

   procedure Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Segments);

   for Segments'Read use Read;
   for Segments'Write use Write;

   procedure Read
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Segments)
   is
      Ch    : Character;
      Count : Natural := 0;
   begin
      Item := ('a' .. 'g' => False);
      while not End_Of_Input loop
         Character'Read (Stream, Ch);
         case Ch is
            when 'a' .. 'g' =>
               Item (Ch) := True;
               Count := Count + 1;
            when '|' | ' ' | ASCII.LF =>
               if Count > 0 then
                  return;
               end if;
            when others =>
               New_Line (Error);
               String'Write (Error, "Unknown character: " & Ch);
               New_Line (Error);
         end case;

         if Count > 7 then
            New_Line (Error);
            String'Write (Error, "Got too many segments!");
            New_Line (Error);
         end if;
      end loop;
   end Read;

   procedure Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Segments)
   is
   begin
      for Seg in Item'Range loop
         if Item (Seg) = True then
            String'Write (Stream, Seg'Image);
         end if;
      end loop;
   end Write;

   function Popcount
      (S : Segments)
      return Natural
   is
      N : Natural := 0;
   begin
      for I in S'Range loop
         if S (I) = True then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Popcount;

   Signal_Pattern : array (1 .. 10) of Segments;
   Output_Value   : array (1 .. 4) of Segments;
   Result         : Natural := 0;
begin
   while not End_Of_Input loop
      for I in Signal_Pattern'Range loop
         Segments'Read (Input, Signal_Pattern (I));
      end loop;

      for I in Output_Value'Range loop
         Segments'Read (Input, Output_Value (I));
         case Popcount (Output_Value (I)) is
            when 2 | 3 | 4 | 7 =>
               Result := Result + 1;
            when others =>
               null;
         end case;
      end loop;
   end loop;
   Integer_IO.Put (Output, Result);
   New_Line (Output);
end D8_1;
