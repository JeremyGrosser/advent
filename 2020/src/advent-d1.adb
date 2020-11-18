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
      Result : Integer;
   begin
      Result := Part_1 ("input/d1.1-test");
      if Result /= 0 then
         Put_Line ("D1.1 test failed: expected 0, got " & Result'Image);
         return;
      end if;
      Result := Part_1 ("input/d1.1");
      Put_Line ("D1.1 solution: " & Result'Image);

      Result := Part_2 ("input/d1.1-test");
      if Result /= 0 then
         Put_Line ("D1.2 test failed: expected 0, got " & Result'Image);
         return;
      end if;
      Result := Part_2 ("input/d1.2");
      Put_Line ("D1.2 solution: " & Result'Image);
   end Run;
end Advent.D1;
