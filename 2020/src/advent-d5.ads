with Ada.Text_IO; use Ada.Text_IO;

package Advent.D5 is
   Input_Format_Error : exception;

   subtype Row_Type    is Natural range 0 .. 127;
   subtype Column_Type is Natural range 0 .. 7;
   subtype Seat_Id     is Natural range 0 .. (Row_Type'Last * (Column_Type'Last + 1) + Column_Type'Last);

   function Read_Boarding_Pass
      (S : String)
      return Seat_Id;

   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Integer;

   procedure Run;
end Advent.D5;
