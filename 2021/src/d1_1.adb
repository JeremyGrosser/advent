with Advent_IO; use Advent_IO;

procedure D1_1 is
   Current, Previous : Integer;
   Increases : Natural := 0;
begin
   Previous := Get_Integer;

   while not End_Of_File loop
      Current := Get_Integer;
      if Current > Previous then
         Increases := Increases + 1;
      end if;
      Previous := Current;
   end loop;

   Put (Increases);
   New_Line;
   Flush;
end D1_1;
