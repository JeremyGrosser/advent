with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure Day3_2 is

   function Find_Badge
      (A, B, C : String)
      return Character
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      Set : Character_Set;
   begin
      for Candidate of A loop
         Set := To_Set (Candidate);
         if Index (B, Set, B'First) /= 0 and then Index (C, Set, C'First) /= 0 then
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
