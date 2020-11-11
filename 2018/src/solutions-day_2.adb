with Ada.Text_IO; use Ada.Text_IO;

package body Solutions.Day_2 is
   procedure Checksum
      (Box_Id       : String;
       Twos, Threes : out Natural)
   is
      type Count_Array is array (Character) of Natural;
      Counts : Count_Array := (others => 0);
   begin
      for I in Box_Id'Range loop
         Counts (Box_Id (I)) := Counts (Box_Id (I)) + 1;
      end loop;

      Twos := 0;
      Threes := 0;

      for I in Counts'Range loop
         if Counts (I) = 2 then
            Twos := 1;
         elsif Counts (I) = 3 then
            Threes := 1;
         end if;
      end loop;
   end Checksum;

   function Part_1 (Filename : String)
      return Integer
   is
      Input_File : File_Type;
      Twos, Threes : Natural := 0;
   begin
      Open (Input_File, In_File, Filename);

      loop
         exit when End_Of_File (Input_File);
         declare
            Line : constant String := Get_Line (Input_File);
            A, B : Natural;
         begin
            Checksum (Line, A, B);
            Twos := Twos + A;
            Threes := Threes + B;
         end;
      end loop;

      Close (Input_File);

      return Twos * Threes;
   end Part_1;

   procedure Run is
      Result : Integer;
   begin
      Result := Part_1 ("input/day2.1a");
      if Result /= 12 then
         Put_Line ("day2.1a failed: got " & Result'Image & ", expected 12");
         return;
      end if;

      Result := Part_1 ("input/day2");
      Put_Line ("Solution 2.1: " & Result'Image);
   end Run;
end Solutions.Day_2;
