with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;

procedure D1_2 is
   package Integer_IO is new Advent_IO.Generic_Numbers (Integer);
   use Integer_IO;

   Items    : constant Numbers := Get (Input);
   First    : Positive := Items'First + 2;
   Sum      : Integer;
   Previous : Integer := 0;
   Result   : Natural := 0;
begin
   while First <= Items'Last loop
      Sum := 0;
      for I in First - 2 .. First loop
         Sum := Sum + Items (I);
      end loop;
      if Sum > Previous then
         Result := Result + 1;
      end if;
      Previous := Sum;
      First := First + 1;
   end loop;
   Put (Output, Result - 1); --  subtract 1 because the first result doesn't count
   New_Line (Output);
end D1_2;
