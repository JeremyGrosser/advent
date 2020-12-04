with Ada.Text_IO; use Ada.Text_IO;

package Advent.D3 is
   subtype Coordinate is Count_Type range 1 .. Count_Type'Last;
   type Map_Value is (Open, Tree);
   type Map_Type is array (Coordinate range <>, Coordinate range <>) of Map_Value;

   Invalid_Input : exception;

   function Num_Lines
      (File : File_Type)
      return Count_Type;

   function Read_Map
      (Filename : String)
      return Map_Type;

   function Check_Slope
      (Filename : String;
       Right    : Coordinate;
       Down     : Coordinate)
      return Count_Type;

   function Part_1
      (Filename : String)
      return Count_Type;

   function Part_2
      (Filename : String)
      return Count_Type;

   procedure Run;
end Advent.D3;
