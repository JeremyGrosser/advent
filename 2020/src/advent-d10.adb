with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D10 is
   use Integer_Vectors;
   use Integer_Sorting;

   function Part_1
      (Filename : String)
      return Integer
   is
      Adapters : Vector := Read_Integers (Filename);
      Previous : Integer;
      Gap      : Integer;
      I        : Cursor;
      Count_1  : Natural := 0;
      Count_3  : Natural := 0;
   begin
      Append (Adapters, 0);
      Sort (Adapters);
      Append (Adapters, Last_Element (Adapters) + 3);

      I := First (Adapters);
      loop
         Previous := Element (I);
         I := Next (I);
         Gap := Element (I) - Previous;
         case Gap is
            when 1 => Count_1 := Count_1 + 1;
            when 3 => Count_3 := Count_3 + 1;
            when others => raise No_Answer with "Unexpected gap" & Gap'Image;
         end case;
         exit when I = Last (Adapters);
      end loop;

      return Count_1 * Count_3;
   end Part_1;

   function Sequence_Length
      (Adapters : Vector;
       First    : Integer)
       return Natural
   is
      Match : constant Integer := Element (Adapters, First);
      N : Natural := 0;
   begin
      for J in First + 1 .. Last_Index (Adapters) loop
         if Element (Adapters, J) - Match in 1 .. 3 then
            N := N + 1;
         else
            return N;
         end if;
      end loop;
      return N;
   end Sequence_Length;

   function Part_2
      (Filename : String)
      return Count_Type
   is
      Adapters : Vector := Read_Integers (Filename);
      Permutations : Count_Type := 1;
      I : Integer := First_Index (Adapters);
      J : Natural;
   begin
      Append (Adapters, 0);
      Sort (Adapters);
      Append (Adapters, Last_Element (Adapters) + 3);

      while I < Last_Index (Adapters) loop
         J := Sequence_Length (Adapters, I);
         case J is
            when 0 .. 2 =>
               J := 1;
            when 3 =>
               Permutations := Permutations * 2;
            when 4 =>
               Permutations := Permutations * 3;
            when others =>
               raise No_Answer;
         end case;
         I := I + J;
      end loop;
      Put_Line (Permutations'Image);
      return Permutations;
   end Part_2;

   procedure Run is
   begin
      pragma Assert (Part_1 ("input/d10-test") = 35);
      Put_Line ("10.1 solution: " & Part_1 ("input/d10")'Image);

      Put_Line ("Part 2 test 1...");
      pragma Assert (Part_2 ("input/d10-test") = 8);
      Put_Line ("Part 2 test 2...");
      pragma Assert (Part_2 ("input/d10-test2") = 19208);
      Put_Line ("10.2 solution: " & Part_2 ("input/d10")'Image);
   end Run;
end Advent.D10;
