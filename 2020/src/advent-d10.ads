package Advent.D10 is

   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Integer;

   procedure Run;

private

   function Maximum
      (V : Integer_Vectors.Vector)
      return Integer;

end Advent.D10;
