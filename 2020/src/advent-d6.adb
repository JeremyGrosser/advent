with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D6 is
   subtype Index_Type is Character range 'a' .. 'z';
   type Flag_Array is array (Index_Type) of Boolean;

   function Popcount
      (FA : Flag_Array)
      return Natural
   is
      Sum : Natural := 0;
   begin
      for I in FA'Range loop
         if FA (I) then
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
      Answers : Flag_Array := (others => False);
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
               Answers := (others => False);
            else
               for C of Line loop
                  Answers (C) := True;
               end loop;
            end if;
         end;
      end loop;
      Close (Input);
      Sum := Sum + Popcount (Answers);
      return Sum;
   end Part_1;

   procedure Run is
   begin
      Test (Part_1'Access, "6.1", "input/d6-test", 11);
      Put_Line ("6.1 solution: " & Part_1 ("input/d6")'Image);
   end Run;
end Advent.D6;
