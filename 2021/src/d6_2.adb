with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;

procedure D6_2 is
   type Count is new Long_Long_Integer;
   subtype Age is Natural range 0 .. 8;
   type Ages is array (Age) of Count;

   package Age_IO is new Advent_IO.Generic_Numbers
      (Number => Age);
   package Count_IO is new Advent_IO.Generic_Numbers
      (Number => Count);

   Fishes : Ages := (others => 0);
   Total  : Count := 0;
   Tmp    : Count;
begin
   for A of Age_IO.Get_Numbers (Input, Delimiter => Comma) loop
      Fishes (A) := Fishes (A) + 1;
   end loop;

   for Day in 1 .. 256 loop
      Tmp := Fishes (0);
      for I in 0 .. 7 loop
         Fishes (I) := Fishes (I + 1);
      end loop;
      Fishes (6) := Fishes (6) + Tmp;
      Fishes (8) := Tmp;
   end loop;

   Total := Fishes'Reduce ("+", 0);
   Count_IO.Put (Output, Total);
   New_Line (Output);
end D6_2;
