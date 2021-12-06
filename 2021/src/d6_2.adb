with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Vectors;
with Ada.Streams;

procedure D6_2 is

   type Count is new Long_Long_Integer;
   subtype Age is Natural range 0 .. 8;
   type Ages is array (Age) of Count;

   procedure Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Ages);
   for Ages'Write use Write;

   package Age_IO is new Advent_IO.Generic_Numbers
      (Number => Age);
   package Count_IO is new Advent_IO.Generic_Numbers
      (Number => Count);

   procedure Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : Ages)
   is
   begin
      for I in Item'Range loop
         Count_IO.Put (Stream, Item (I));
         Character'Write (Stream, ' ');
      end loop;
   end Write;

   Fishes : Ages := (others => 0);
   Total  : Count := 0;
   Tmp    : Count;
begin
   for A of Age_IO.Get_Numbers (Input, Delimiter => Comma) loop
      Fishes (A) := Fishes (A) + 1;
   end loop;

   --  New_Line (Error);
   --  Ages'Write (Error, Fishes);
   --  New_Line (Error);

   for Day in 1 .. 256 loop
      --  String'Write (Error, "Day " & Day'Image & ": ");
      --  Ages'Write (Error, Fishes);
      --  New_Line (Error);

      Tmp := Fishes (0);
      for I in 0 .. 7 loop
         Fishes (I) := Fishes (I + 1);
      end loop;
      Fishes (6) := Fishes (6) + Tmp;
      Fishes (8) := Tmp;
   end loop;

   for F of Fishes loop
      Total := Total + F;
   end loop;
   Count_IO.Put (Output, Total);
   New_Line (Output);

end D6_2;
