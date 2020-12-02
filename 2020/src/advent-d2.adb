with Ada.Text_IO; use Ada.Text_IO;
with Str; use Str;

package body Advent.D2 is
   function Part_1
      (Filename : in String)
      return Integer
   is
      Input       : File_Type;
      Valid_Count : Natural := 0;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            Line          : String  := Get_Line (Input);
            Num_Start_End : String  := Split (Line, ' ', 0);
            Num_Start     : Natural := Natural'Value (Split (Num_Start_End, '-', 0));
            Num_End       : Natural := Natural'Value (Split (Num_Start_End, '-', 1));
            Substr        : String  := Strip (Split (Line, ' ', 1), ':');
            Ch            : Character := Substr (Substr'First);
            Password      : String  := Split (Line, ' ', 2);
            Count         : Natural := 0;
         begin
            for I in Password'Range loop
               if Password (I) = Ch then
                  Count := Count + 1;
               end if;
            end loop;
            if Count >= Num_Start and then Count <= Num_End then
               Valid_Count := Valid_Count + 1;
            end if;
         end;
      end loop;
      Close (Input);

      return Valid_Count;
   end Part_1;

   function Part_2
      (Filename : in String)
      return Integer
   is
      Input       : File_Type;
      Valid_Count : Natural := 0;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            Line          : String    := Get_Line (Input);
            Num_Start_End : String    := Split (Line, ' ', 0);
            Num_Start     : Positive  := Natural'Value (Split (Num_Start_End, '-', 0));
            Num_End       : Positive  := Natural'Value (Split (Num_Start_End, '-', 1));
            Substr        : String    := Strip (Split (Line, ' ', 1), ':');
            Ch            : Character := Substr (Substr'First);
            Password      : String    := Split (Line, ' ', 2);
            Matches       : Natural   := 0;
         begin
            --Put_Line (Password & " = " & Password'Length'Image & " : " & Num_Start'Image & " - " & Num_End'Image);
            if Password (Password'First + Num_Start - 1) = Ch then
               Matches := Matches + 1;
            end if;
            if Password'Length >= (Num_End - 1) and then Password (Password'First + Num_End - 1) = Ch then
               Matches := Matches + 1;
            end if;
            if Matches = 1 then
               Valid_Count := Valid_Count + 1;
            end if;
         end;
      end loop;
      Close (Input);

      return Valid_Count;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "2.1", "input/d2.1-test", 2);
      Put_Line ("2.1 solution: " & Part_1 ("input/d2")'Image);

      Test (Part_2'Access, "2.2", "input/d2.1-test", 1);
      Put_Line ("2.2 solution: " & Part_2 ("input/d2")'Image);
   end Run;
end Advent.D2;
