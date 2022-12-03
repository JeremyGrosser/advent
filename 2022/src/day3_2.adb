with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;

procedure Day3_2 is

   function Contains
      (S : String;
       C : Character)
      return Boolean
   is
   begin
      for Ch of S loop
         if Ch = C then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   function Find_Badge
      (A, B, C : String)
      return Character
   is
   begin
      for Candidate of A loop
         if Contains (B, Candidate) and then Contains (C, Candidate) then
            return Candidate;
         end if;
      end loop;
      raise Program_Error with "No common badge in group";
   end Find_Badge;

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
         A : constant String := Read_Until (Input, CRLF);
         B : constant String := Read_Until (Input, CRLF);
         C : constant String := Read_Until (Input, CRLF);
         Badge : Character;
      begin
         Badge := Find_Badge (A, B, C);
         Sum := Sum + Priority (Badge);
      end;
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day3_2;
