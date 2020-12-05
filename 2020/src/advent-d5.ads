with Ada.Text_IO; use Ada.Text_IO;

package Advent.D5 is
   Input_Format_Error : exception;

   type Seat_Id is new Natural;

   function Read_Boarding_Pass
      (S : String)
      return Seat_Id;

   function Part_1
      (Filename : String)
      return Integer;

   procedure Run;
end Advent.D5;
