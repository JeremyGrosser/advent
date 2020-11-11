with Ada.Text_IO; use Ada.Text_IO;

package body Solutions.Day_1 is
   procedure Run (Filename : String) is
      Input_File : File_Type;
      Frequency  : Integer := 0;
      X          : Integer;
   begin
      Open (Input_File, In_File, Filename);

      loop
         exit when End_Of_File (Input_File);
         X := Integer'Value (Get_Line (Input_File));
         Frequency := Frequency + X;
      end loop;

      Put_Line (Filename & " solution: " & Frequency'Image);
   end Run;
end Solutions.Day_1;
