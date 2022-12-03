with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;

procedure Day3_1 is
   function Find_Duplicate
      (A, B : String)
      return Character
   is
   begin
      for I in A'Range loop
         for J in B'Range loop
            if A (I) = B (J) then
               return A (I);
            end if;
         end loop;
      end loop;
      raise Program_Error with "No duplicate in input line";
   end Find_Duplicate;

   function Priority
      (Ch : Character)
      return Positive
   is
   begin
      if Ch in 'a' .. 'z' then
         return (Character'Pos (Ch) - Character'Pos ('a')) + 1;
      elsif Ch in 'A' .. 'Z' then
         return (Character'Pos (Ch) - Character'Pos ('A')) + 27;
      else
         raise Program_Error with "Unknown character: " & Ch;
      end if;
   end Priority;

   Sum : Natural := 0;
begin
   while not End_Of_Input loop
      declare
         Ruck  : constant String := Read_Until (Input, CRLF);
         Left  : constant String := Ruck (Ruck'First .. Ruck'Last - (Ruck'Length / 2));
         Right : constant String := Ruck (Left'Last + 1 .. Ruck'Last);
         Dupe  : Character;
      begin
         Dupe := Find_Duplicate (Left, Right);
         Sum := Sum + Priority (Dupe);
      end;
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day3_1;
