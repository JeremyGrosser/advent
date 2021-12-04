with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure D4_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;
   use Integer_IO.Number_Vectors;

   type Square is record
      Number : Integer := -1;
      Marked : Boolean := False;
   end record;

   type Card is array (1 .. 5, 1 .. 5) of Square;

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
         exit when Comma = 0;
         Append (V, Integer'Value (Line (First .. Comma - 1)));
         First := Comma + 1;
      end loop;
      return V;
   end Read_Numbers;

   function Read_Card
      return Card
   is
      C : Card := (others => (others => <>));
   begin
      for Y in C'Range (1) loop
         for X in C'Range (2) loop
            C (Y, X).Number := Get (Input);
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
      for Y in C'Range (1) loop
         Count := 0;
         for X in C'Range (2) loop
            if C (Y, X).Marked = True then
               Count := Count + 1;
            end if;
         end loop;
         if Count = 5 then
            return True;
         end if;
      end loop;

      for X in C'Range (2) loop
         Count := 0;
         for Y in C'Range (1) loop
            if C (Y, X).Marked = True then
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
      for Y in C'Range (1) loop
         for X in C'Range (2) loop
            if C (Y, X).Marked = False then
               Total := Total + C (Y, X).Number;
            end if;
         end loop;
      end loop;
      return Total;
   end Score;

   Draw  : constant Vector := Read_Numbers;
   Cards : Card_Vectors.Vector := Card_Vectors.Empty_Vector;
begin
   while not End_Of_Input loop
      Card_Vectors.Append (Cards, Read_Card);
   end loop;

   for Called_Number of Draw loop
      for C of Cards loop
         for Y in C'Range (1) loop
            for X in C'Range (2) loop
               if C (Y, X).Number = Called_Number then
                  C (Y, X).Marked := True;
               end if;
            end loop;
         end loop;

         if Is_Winning (C) then
            Put (Output, Called_Number * Score (C));
            return;
         end if;
      end loop;
   end loop;
end D4_1;
