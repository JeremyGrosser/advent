with Advent_IO; use Advent_IO;

procedure D1_2 is
   Items    : constant Integers := Get_Integers;
   First    : Positive := Items'First + 2;
   Sum      : Integer;
   Previous : Integer := 0;
   Result   : Natural := 0;
begin
   while First <= Items'Last loop
      Sum := 0;
      for I in First - 2 .. First loop
         --  Put (Items (I));
         Sum := Sum + Items (I);
      end loop;
      --  Put (Sum);
      --  New_Line;
      if Sum > Previous then
         Result := Result + 1;
      end if;
      Previous := Sum;
      First := First + 1;
   end loop;
   Put (Result - 1); --  subtract 1 because the first result doesn't count
   New_Line;
end D1_2;
