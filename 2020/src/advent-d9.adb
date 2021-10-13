with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D9 is
   function Valid
      (Numbers : Integer_Vectors.Vector;
       N       : Integer)
       return Boolean
   is
   begin
      for I of Numbers loop
         for J of Numbers loop
            if I + J = N then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Valid;

   function Part_1
      (Filename        : String;
       Preamble_Length : Positive)
      return Integer
   is
      Numbers : Integer_Vectors.Vector := Integer_Vectors.Empty_Vector;
      Input   : File_Type;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            use Integer_Vectors;
            N        : constant Integer := Integer'Value (Get_Line (Input));
            Preamble : constant Boolean := (Natural (Length (Numbers)) <= Preamble_Length);
         begin
            if not Preamble then
               if not Valid (Numbers, N) then
                  --  Put_Line (N'Image & " is not a sum of two of the previous " & Preamble_Length'Image & " numbers");
                  Close (Input);
                  return N;
               else
                  Delete_First (Numbers);
               end if;
            end if;
            Append (Numbers, N);
         end;
      end loop;
      Close (Input);
      return 0;
   end Part_1;

   procedure Run is
   begin
      pragma Assert (Part_1 ("input/d9-test", 5) = 127);
      Put_Line ("9.1 solution: " & Part_1 ("input/d9", 25)'Image);
   end Run;
end Advent.D9;
