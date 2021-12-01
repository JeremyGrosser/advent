with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;

procedure D1_2 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

   Items    : constant Numbers := Get (Input);
   I        : Positive := Items'First + 2;
   Sum      : Integer;
   Previous : Integer := 0;
   Result   : Natural := 0;
begin
   while I <= Items'Last loop
      Sum := 0;
      for J in I - 2 .. I loop
         Sum := Sum + Items (J);
      end loop;
      if Sum > Previous then
         Result := Result + 1;
      end if;
      Previous := Sum;
      I := I + 1;
   end loop;
   Put (Output, Result - 1); --  subtract 1 because the first result doesn't count
   New_Line (Output);
end D1_2;
