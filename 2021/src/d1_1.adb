with Advent_IO; use Advent_IO;
with Generic_Number_Stream_IO;

procedure D1_1 is
   package Integer_Stream_IO is new Generic_Number_Stream_IO (Integer);
   use Integer_Stream_IO;

   Current, Previous : Integer;
   Increases : Natural := 0;
begin
   Previous := Get (Input);

   while not End_Of_Input loop
      Current := Get (Input);
      if Current > Previous then
         Increases := Increases + 1;
      end if;
      Previous := Current;
   end loop;

   Put (Output, Increases);
   New_Line (Output);
end D1_1;
