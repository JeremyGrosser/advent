package Advent.D10 is

   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Count_Type;

   procedure Run;

private

   function Sequence_Length
      (Adapters : Integer_Vectors.Vector;
       First    : Integer)
       return Natural;

end Advent.D10;
