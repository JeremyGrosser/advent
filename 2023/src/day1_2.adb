with Advent_IO.Integers; use Advent_IO.Integers;
with Advent_IO; use Advent_IO;

with Chests.Ring_Buffers;

procedure Day1_2 is
   package Integer_Buffers is new Chests.Ring_Buffers (Integer, 16);
   use Integer_Buffers;

   Nums : Ring_Buffer;

   function Match
      (Sub : String)
      return Boolean
   is
   begin
      for I in 1 .. Sub'Length loop
         if Peek (Input, I) /= Sub (Sub'First + I - 1) then
            return False;
         end if;
      end loop;
      Seek (Input, Sub'Length);
      return True;
   end Match;

   procedure Number
      (N : Positive)
   is
   begin
      Append (Nums, N);
   end Number;

   Sum : Natural := 0;

   procedure Accumulate is
      N : Natural;
   begin
      N := First_Element (Nums) * 10;
      N := N + Last_Element (Nums);

      Sum := Sum + N;
      Clear (Nums);
   end Accumulate;

   Ch : Character;
begin
   Clear (Nums);
   while not End_Of_Input loop
      Ch := Peek (Input);
      if Ch = ASCII.CR or else Ch = ASCII.LF then
         Seek (Input, 1);
         Accumulate;
      elsif Ch in '1' .. '9' then
         Append (Nums, Character'Pos (Ch) - Character'Pos ('0'));
         Seek (Input, 1);
      elsif Match ("one") then
         Number (1);
      elsif Match ("two") then
         Number (2);
      elsif Match ("three") then
         Number (3);
      elsif Match ("four") then
         Number (4);
      elsif Match ("five") then
         Number (5);
      elsif Match ("six") then
         Number (6);
      elsif Match ("seven") then
         Number (7);
      elsif Match ("eight") then
         Number (8);
      elsif Match ("nine") then
         Number (9);
      else
         --  skip
         Seek (Input, 1);
      end if;
   end loop;

   Accumulate;

   Put (Output, Sum);
   New_Line (Output);
end Day1_2;
