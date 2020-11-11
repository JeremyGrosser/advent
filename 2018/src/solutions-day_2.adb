with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Solutions.Day_2 is
   package String_Set is new Ada.Containers.Indefinite_Ordered_Sets
      (String);
   
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

   function Distance (A, B : String)
      return Natural is
      D : Natural := 0;
   begin
      for I in A'Range loop
         if A (I) /= B (I) then
            D := D + 1;
         end if;
      end loop;
      return D;
   end Distance;

   procedure Correct_Boxes
      (Boxes : String_Set.Set;
       A, B  : out String)
   is
   begin
      for I of Boxes loop
         for J of Boxes loop
            if Distance (I, J) = 1 then
               A := I;
               B := J;
               return;
            end if;
         end loop;
      end loop;
   end Correct_Boxes;

   function Part_2 (Filename : String)
      return String
   is
      Input_File : File_Type;
      Boxes      : String_Set.Set := String_Set.Empty_Set;
   begin
      Open (Input_File, In_File, Filename);

      loop
         exit when End_Of_File (Input_File);
         declare
            Line : constant String := Get_Line (Input_File);
         begin
            Boxes.Insert (Line);
         end;
      end loop;

      Close (Input_File);

      declare
         A, B   : String (1 .. Boxes.First_Element'Length);
         Common : Unbounded_String;
      begin
         Correct_Boxes (Boxes, A, B);
         for I in A'Range loop
            if A (I) = B (I) then
               Append (Common, A (I));
            end if;
         end loop;
         return To_String (Common);
      end;
   end Part_2;

   procedure Run is
      Result_1 : Integer;
   begin
      Result_1 := Part_1 ("input/day2.1a");
      if Result_1 /= 12 then
         Put_Line ("day2.1a failed: got " & Result_1'Image & ", expected 12");
         return;
      end if;

      Result_1 := Part_1 ("input/day2");
      Put_Line ("Solution 2.1: " & Result_1'Image);

      declare
         Result_2 : String := Part_2 ("input/day2.2a");
      begin
         if Result_2 /= "fgij" then
            Put_Line ("day2.2a failed: got " & Result_2 & ", expected fgij");
         end if;
      end;

      declare
         Result_2 : String := Part_2 ("input/day2");
      begin
         Put_Line ("Solution 2.2: " & Result_2);
      end;
   end Run;
end Solutions.Day_2;
