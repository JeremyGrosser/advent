with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;

procedure D1_2 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;
   use Number_Vectors;

   Items    : constant Vector := Get_Vector (Input);
   Result   : Natural := 0;
begin
   for I in First_Index (Items) + 3 .. Last_Index (Items) loop
      --  The middle terms cancel out, so we don't need to compute the sum of the whole series.
      --  HT https://www.reddit.com/r/adventofcode/comments/r66vow/comment/hmscynu/
      if Element (Items, I - 3) < Element (Items, I) then
         Result := Result + 1;
      end if;
   end loop;
   Put (Output, Result);
   New_Line (Output);
end D1_2;
