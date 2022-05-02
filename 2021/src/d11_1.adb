with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Streams;

procedure D11_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers (Integer);
   use Integer_IO;

   type Octopus is record
      Energy  : Natural := 0;
      Flashed : Boolean := False;
   end record;

   subtype Index is Integer range 1 .. 10;
   type Cavern is array (Index, Index) of Octopus;

   procedure Read
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Cavern)
   is
      C : Character;
   begin
      for Row in Item'Range (1) loop
         for Column in Item'Range (2) loop
            loop
               Character'Read (Stream, C);
               exit when C in '0' .. '9';
            end loop;
            Item (Row, Column).Energy := Character'Pos (C) - Character'Pos ('0');
         end loop;
      end loop;
   end Read;

   function Increment
      (Cave : Cavern)
      return Cavern
   is
      Result : Cavern;
   begin
      for Row in Cave'Range (1) loop
         for Column in Cave'Range (2) loop
            Result (Row, Column).Energy := Cave (Row, Column).Energy + 1;
         end loop;
      end loop;
      return Result;
   end Increment;

   function Increment
      (Cave : Cavern;
       Y, X : Integer)
       return Cavern
   is
      Result : Cavern := Cave;
   begin
      if Y in Index'Range and X in Index'Range then
         Result (Y, X).Energy := Result (Y, X).Energy + 1;
      end if;
      return Result;
   end Increment;

   function Increment_Flashed_Neighbors
      (Cave    : Cavern;
       Flashed : out Boolean)
      return Cavern
   is
      Result : Cavern := Cave;
   begin
      for Y in Cave'Range (1) loop
         for X in Cave'Range (2) loop
            if not Cave (Y, X).Flashed and Cave (Y, X).Energy > 9 then
               Flashed := True;
               Result (Y, X).Flashed := True;
               Result := Increment (Result, Y - 1, X - 1);
               Result := Increment (Result, Y - 1, X);
               Result := Increment (Result, Y - 1, X + 1);
               Result := Increment (Result, Y, X - 1);
               Result := Increment (Result, Y, X + 1);
               Result := Increment (Result, Y + 1, X - 1);
               Result := Increment (Result, Y + 1, X);
               Result := Increment (Result, Y + 1, X + 1);
               return Result;
            end if;
         end loop;
      end loop;
      Flashed := False;
      return Result;
   end Increment_Flashed_Neighbors;

   function Reset_Flashed
      (Cave : Cavern)
      return Cavern
   is
      Result : Cavern := Cave;
   begin
      for Y in Result'Range (1) loop
         for X in Result'Range (2) loop
            if Result (Y, X).Flashed then
               Result (Y, X) := (Flashed => False, Energy => 0);
            end if;
         end loop;
      end loop;
      return Result;
   end Reset_Flashed;

   Current : Cavern;
   Flashes : Natural := 0;
   Flashed : Boolean;
begin
   Read (Input, Current);
   for I in 1 .. 100 loop
      --  New_Line (Error);
      --  String'Write (Error, "I=");
      --  Put (Error, I);
      --  String'Write (Error, " Flashes=");
      --  Put (Error, Flashes);

      Current := Increment (Current);
      loop
         Current := Increment_Flashed_Neighbors (Current, Flashed);
         exit when not Flashed;
         Flashes := Flashes + 1;
      end loop;
      Current := Reset_Flashed (Current);
   end loop;

   --  String'Write (Error, Current'Image);
   Put (Output, Flashes);
   New_Line (Output);
end D11_1;
