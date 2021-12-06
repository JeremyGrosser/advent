with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Vectors;

procedure D6_1 is
   package Integer_IO is new Advent_IO.Generic_Numbers
      (Number => Integer);
   use Integer_IO;

   type Fish is record
      Timer : Natural;
   end record;

   package Fish_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Fish);

   Fishes : Fish_Vectors.Vector := Fish_Vectors.Empty_Vector;
begin
   for F of Get_Vector (Input, Delimiter => Comma) loop
      Fish_Vectors.Append (Fishes, Fish'(Timer => F));
   end loop;

   for Day in 1 .. 80 loop
      for I in 1 .. Positive (Fish_Vectors.Length (Fishes)) loop
         if Fishes (I).Timer = 0 then
            Fishes (I).Timer := 6;
            Fish_Vectors.Append (Fishes, Fish'(Timer => 8));
         else
            Fishes (I).Timer := Fishes (I).Timer - 1;
         end if;
      end loop;
   end loop;

   Put (Output, Integer (Fish_Vectors.Length (Fishes)));
   New_Line (Output);
end D6_1;
