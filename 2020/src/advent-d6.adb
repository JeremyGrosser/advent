with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D6 is
   function Popcount
      (FA    : Flag_Array;
       Match : Natural := 1)
      return Natural
   is
      Sum : Natural := 0;
   begin
      for I in FA'Range loop
         if FA (I) = Match then
            Sum := Sum + 1;
         end if;
      end loop;
      return Sum;
   end Popcount;

   function Part_1
      (Filename : String)
      return Integer
   is
      Input   : File_Type;
      Answers : Flag_Array := (others => 0);
      Sum     : Natural := 0;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            Line : constant String := Get_Line (Input);
         begin
            if Line'Length = 0 then
               Sum := Sum + Popcount (Answers);
               Answers := (others => 0);
            else
               for C of Line loop
                  Answers (C) := 1;
               end loop;
            end if;
         end;
      end loop;
      Close (Input);
      Sum := Sum + Popcount (Answers);
      return Sum;
   end Part_1;

   function Part_2
      (Filename : String)
      return Integer
   is
      Input      : File_Type;
      Answers    : Flag_Array := (others => 0);
      Group_Size : Natural := 0;
      Sum        : Natural := 0;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            Line : constant String := Get_Line (Input);
         begin
            if Line'Length = 0 then
               Sum := Sum + Popcount (Answers, Group_Size);
               Group_Size := 0;
               Answers := (others => 0);
            else
               Group_Size := Group_Size + 1;
               for C of Line loop
                  Answers (C) := Answers (C) + 1;
               end loop;
            end if;
         end;
      end loop;
      Close (Input);
      Sum := Sum + Popcount (Answers, Group_Size);
      return Sum;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "6.1", "input/d6-test", 11);
      Put_Line ("6.1 solution: " & Part_1 ("input/d6")'Image);

      Test (Part_2'Access, "6.2", "input/d6-test", 6);
      Put_Line ("6.2 solution: " & Part_2 ("input/d6")'Image);
   end Run;
end Advent.D6;
