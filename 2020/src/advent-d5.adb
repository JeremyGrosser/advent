with Ada.Assertions; use Ada.Assertions;

package body Advent.D5 is
   function Read_Boarding_Pass
      (S : String)
      return Seat_Id
   is
      subtype Row_Type    is Natural range 0 .. 127;
      subtype Column_Type is Natural range 0 .. 7;

      Row_High : Row_Type := Row_Type'Last;
      Row_Mid  : Row_Type;
      Row_Low  : Row_Type := Row_Type'First;
      Col_High : Column_Type := Column_Type'Last;
      Col_Mid  : Column_Type;
      Col_Low  : Column_Type := Column_Type'First;
   begin
      for C of S loop
         case C is
            when 'F' =>
               Row_Mid := Row_Low + (Row_High - Row_Low) / 2;
               Row_High := Row_Mid;
            when 'B' =>
               Row_Mid := Row_Low + (Row_High - Row_Low) / 2;
               Row_Low := Row_Mid + 1;
            when 'L' =>
               Col_Mid := Col_Low + (Col_High - Col_Low) / 2;
               Col_High := Col_Mid;
            when 'R' =>
               Col_Mid := Col_Low + (Col_High - Col_Low) / 2;
               Col_Low := Col_Mid + 1;
            when others =>
               raise Input_Format_Error with "Unknown character: " & C;
         end case;
      end loop;
      Row_Mid := Row_Low + (Row_High - Row_Low) / 2;
      Col_Mid := Col_Low + (Col_High - Col_Low) / 2;
      return Seat_Id ((Row_Mid * 8)) + Seat_Id (Col_Mid);
   end Read_Boarding_Pass;

   function Part_1
      (Filename : String)
      return Integer
   is
      Input   : File_Type;
      Highest : Seat_Id := Seat_Id'First;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         declare
            Line : constant String := Get_Line (Input);
            Seat : Seat_Id;
         begin
            Seat := Read_Boarding_Pass (Line);
            if Seat > Highest then
               Highest := Seat;
            end if;
         end;
      end loop;
      Close (Input);
      return Integer (Highest);
   end Part_1;

   procedure Run is
   begin
      Assert (Read_Boarding_Pass ("FBFBBFFRLR") = 357);

      Test (Part_1'Access, "5.1", "input/d5-test", 820);
      Put_Line ("5.1 solution: " & Part_1 ("input/d5")'Image);
   end Run;
end Advent.D5;