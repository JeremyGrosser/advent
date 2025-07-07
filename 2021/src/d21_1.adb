pragma Ada_2022;
with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
--  with Ada.Text_IO; use Ada.Text_IO;

procedure D21_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers (Integer);
   use Integer_IO;

   type Player is range 1 .. 2;
   type Position is range 1 .. 10;

   function "+" (Left : Position; Right : Natural)
      return Position
   is
      N : Integer := Integer (Left) + Integer (Right);
   begin
      while N > 10 loop
         N := N - 10;
      end loop;
      return Position (N);
   end "+";

   Roll_Count : Natural := 0;
   Next_Roll : Natural := 1;

   function Deterministic_Roll
      return Natural
   is
      R : constant Natural := Next_Roll;
   begin
      Roll_Count := Roll_Count + 1;
      Next_Roll := Next_Roll + 1;
      if Next_Roll > 100 then
         Next_Roll := 1;
      end if;
      return R;
   end Deterministic_Roll;

   Board : array (Player) of Position;
   Score : array (Player) of Natural := (others => 0);

   procedure Turn
      (P : Player)
   is
      Steps : Natural := 0;
      R : Natural;
   begin
      for Roll in 1 .. 3 loop
         R := Deterministic_Roll;
         Steps := Steps + R;
      end loop;
      Board (P) := Board (P) + Steps;
      Score (P) := Score (P) + Natural (Board (P));
   end Turn;

   function Lowest_Score
      return Natural
   is
      Min : Integer := Integer'Last;
   begin
      for S of Score loop
         if S < Min then
            Min := S;
         end if;
      end loop;
      return Min;
   end Lowest_Score;

   Sum : Natural := 0;
   Ch : Character;
   I, Steps : Integer;
begin
   for P in Board'Range loop
      loop
         Character'Read (Input, Ch);
         exit when Ch = ':';
      end loop;
      Character'Read (Input, Ch);
      I := Integer_IO.Get (Input);
      Board (P) := Position (I);
   end loop;

   Turns : loop
      for P in Board'Range loop
         Turn (P);
         exit Turns when Score (P) >= 1000;
      end loop;
   end loop Turns;

   Sum := Roll_Count * Lowest_Score;
   Put (Output, Sum);
   New_Line (Output);
end D21_1;
