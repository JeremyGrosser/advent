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

   A, B, C : String (1 .. 51);
   A_Last, B_Last, C_Last : Natural;
   Sum : Natural := 0;
   Badge : Character;
begin
   while not End_Of_Input loop
      Read_Until (Input, ASCII.LF, A, A_Last);
      Read_Until (Input, ASCII.LF, B, B_Last);
      Read_Until (Input, ASCII.LF, C, C_Last);
      Badge := Find_Badge (A (1 .. A_Last), B (1 .. B_Last), C (1 .. C_Last));
      Sum := Sum + Priority (Badge);
   end loop;

   Put (Output, Sum);
   New_Line (Output);
end Day3_2;
