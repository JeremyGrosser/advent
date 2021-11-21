with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D10 is
   use Integer_Vectors;
   use Integer_Sorting;

   function Maximum
      (V : Vector)
      return Integer
   is
      M : Integer := First_Element (V);
   begin
      for N of V loop
         if N > M then
            M := N;
         end if;
      end loop;
      return M;
   end Maximum;

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
      Append (Adapters, Maximum (Adapters) + 3);
      Sort (Adapters);

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

   function Part_2
      (Filename : String)
      return Integer
   is
   begin
      return 0;
   end Part_2;

   procedure Run is
   begin
      pragma Assert (Part_1 ("input/d10-test") = 35);
      Put_Line ("10.1 solution: " & Part_1 ("input/d10")'Image);

      pragma Assert (Part_2 ("input/d10-test") = -1);
      Put_Line ("10.2 solution: " & Part_2 ("input/d10")'Image);
   end Run;
end Advent.D10;
