with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D1 is
   function Part_1 (Filename : in String)
      return Integer
   is
   begin
      return 1;
   end Part_1;

   function Part_2 (Filename : in String)
      return Integer
   is
   begin
      return 1;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "1.1", "input/d1.1-test", 0);
      Put_Line ("1.1 solution: " & Part_1 ("input/d1")'Image);

      Test (Part_2'Access, "1.2", "input/d1.2-test", 0);
      Put_Line ("1.2 solution: " & Part_2 ("input/d1")'Image);
   end Run;
end Advent.D1;
