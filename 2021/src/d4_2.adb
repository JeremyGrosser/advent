with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure D4_2 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;
   use Integer_IO.Number_Vectors;

   type Square is record
      Number : Integer := -1;
      Marked : Boolean := False;
   end record;

   type Squares is array (1 .. 5, 1 .. 5) of Square;

   type Card is record
      Values : Squares := (others => (others => <>));
      Won    : Boolean := False;
   end record;

   package Card_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Card);

   function Read_Numbers
      return Vector
   is
      use Ada.Strings.Fixed;
      Line  : constant String := Read_Until (Input, Whitespace);
      V     : Vector := Empty_Vector;
      First : Positive := Line'First;
      Comma : Natural;
   begin
      loop
         Comma := Index (Line (First .. Line'Last), ",");
         if Comma = 0 then
            Append (V, Integer'Value (Line (First .. Line'Last)));
            return V;
         else
            Append (V, Integer'Value (Line (First .. Comma - 1)));
            First := Comma + 1;
         end if;
      end loop;
   end Read_Numbers;

   function Read_Card
      return Card
   is
      C : Card := (others => <>);
   begin
      for Y in C.Values'Range (1) loop
         for X in C.Values'Range (2) loop
            C.Values (Y, X).Number := Get (Input);
         end loop;
      end loop;
      return C;
   end Read_Card;

   function Is_Winning
      (C : Card)
      return Boolean
   is
      Count : Natural;
   begin
      for Y in C.Values'Range (1) loop
         Count := 0;
         for X in C.Values'Range (2) loop
            if C.Values (Y, X).Marked = True then
               Count := Count + 1;
            end if;
         end loop;
         if Count = 5 then
            return True;
         end if;
      end loop;

      for X in C.Values'Range (2) loop
         Count := 0;
         for Y in C.Values'Range (1) loop
            if C.Values (Y, X).Marked = True then
               Count := Count + 1;
            end if;
         end loop;
         if Count = 5 then
            return True;
         end if;
      end loop;

      return False;
   end Is_Winning;

   function Score
      (C : Card)
      return Natural
   is
      Total : Natural := 0;
   begin
      for Y in C.Values'Range (1) loop
         for X in C.Values'Range (2) loop
            if C.Values (Y, X).Marked = False then
               Total := Total + C.Values (Y, X).Number;
            end if;
         end loop;
      end loop;
      return Total;
   end Score;

   Draw  : constant Vector := Read_Numbers;
   Cards : Card_Vectors.Vector := Card_Vectors.Empty_Vector;
   Num_Winners : Natural := 0;
begin
   while not End_Of_Input loop
      Card_Vectors.Append (Cards, Read_Card);
   end loop;

   for Called_Number of Draw loop
      for C of Cards loop
         for Y in C.Values'Range (1) loop
            for X in C.Values'Range (2) loop
               if C.Values (Y, X).Number = Called_Number then
                  C.Values (Y, X).Marked := True;
               end if;
            end loop;
         end loop;

         if not C.Won and Is_Winning (C) then
            C.Won := True;
            Num_Winners := Num_Winners + 1;
            if Num_Winners = Natural (Length (Draw)) then
               Put (Output, Called_Number * Score (C));
               New_Line (Output);
               return;
            end if;
         end if;
      end loop;
   end loop;

   Put (Output, -1);
   New_Line (Output);
end D4_2;
