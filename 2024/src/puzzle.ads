with Advent.Input;
with Advent.Output;

package Puzzle is
   type Any_Solution is access procedure
      (Input  : in out Advent.Input.Buffer;
       Output : Advent.Output.Buffer);

   type Day_Type is range 1 .. 25;
   type Part_Type is range 1 .. 2;

   function Get_Solution
      (Day  : Day_Type;
       Part : Part_Type)
       return Any_Solution;
end Puzzle;
