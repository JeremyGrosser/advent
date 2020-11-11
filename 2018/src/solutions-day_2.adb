with Ada.Text_IO; use Ada.Text_IO;

package body Solutions.Day_2 is
   function Part_1 (Filename : String)
      return Integer
   is
   begin
      return 0;
   end Part_1;

   procedure Run is
      Result : Integer;
   begin
      Result := Part_1 ("input/day2");
      Put_Line ("Solution 2.1: " & Result'Image);
   end Run;
end Solutions.Day_2;
