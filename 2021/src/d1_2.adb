with Advent_IO; use Advent_IO;
with Generic_Number_Stream_IO;

procedure D1_2 is
   package Integer_Stream_IO is new Generic_Number_Stream_IO (Integer);
   use Integer_Stream_IO;

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
