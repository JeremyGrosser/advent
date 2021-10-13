with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D9 is
   function Valid_1
      (Numbers : Long_Integer_Vectors.Vector;
       N       : Long_Integer)
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
   end Valid_1;

   function Part_1
      (Filename        : String;
       Preamble_Length : Positive)
      return Long_Integer
   is
      Numbers : Long_Integer_Vectors.Vector := Long_Integer_Vectors.Empty_Vector;
      Input   : File_Type;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            use Long_Integer_Vectors;
            N        : constant Long_Integer := Long_Integer'Value (Get_Line (Input));
            Preamble : constant Boolean := (Natural (Length (Numbers)) <= Preamble_Length);
         begin
            if not Preamble then
               if not Valid_1 (Numbers, N) then
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

   function Read_Numbers
      (Filename : String)
      return Int_Array
   is
      use Long_Integer_Vectors;
      Numbers : Vector := Empty_Vector;
      Input   : File_Type;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            N : constant Long_Integer := Long_Integer'Value (Get_Line (Input));
         begin
            Append (Numbers, N);
         end;
      end loop;

      declare
         Result : Int_Array (1 .. Last_Index (Numbers));
      begin
         for I in Result'Range loop
            Result (I) := Element (Numbers, I);
         end loop;
         return Result;
      end;
   end Read_Numbers;

   function Valid_2
      (Numbers     : Int_Array;
       First, Last : Positive)
       return Result_Type
   is
      R : Result_Type;
      N : Long_Integer;
   begin
      for I in First .. Last loop
         N := Numbers (I);
         if N < R.Smallest then
            R.Smallest := N;
         elsif N > R.Largest then
            R.Largest := N;
         end if;
         R.Sum := R.Sum + N;
      end loop;
      return R;
   end Valid_2;

   function Part_2
      (Filename        : String;
       Preamble_Length : Positive)
      return Long_Integer
   is
      Target   : constant Long_Integer := Part_1 (Filename, Preamble_Length);
      Numbers  : constant Int_Array := Read_Numbers (Filename);
      R        : Result_Type;
   begin
      for Last in Numbers'First + 1 .. Numbers'Last loop
         for First in Numbers'First .. Last - 1 loop
            R := Valid_2 (Numbers, First, Last);
            if R.Sum = Target then
               return R.Smallest + R.Largest;
            end if;
         end loop;
      end loop;

      return 0;
   end Part_2;

   procedure Run is
   begin
      pragma Assert (Part_1 ("input/d9-test", 5) = 127);
      Put_Line ("9.1 solution: " & Part_1 ("input/d9", 25)'Image);

      pragma Assert (Part_2 ("input/d9-test", 5) = 62);
      Put_Line ("9.2 solution: " & Part_2 ("input/d9", 25)'Image);
   end Run;
end Advent.D9;
