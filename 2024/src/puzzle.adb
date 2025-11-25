with Day1_1;
with Day1_2;
with Day2_1;
with Day2_2;
with Day3_1;
with Day3_2;
with Day4_1;
with Day4_2;
with Day5_1;
with Day5_2;
with Day6_1;
with Day6_2;
with Day7_1;
with Day7_2;
with Day8_1;
with Day8_2;
with Day9_1;
with Day9_2;
with Day10_1;
with Day10_2;
with Day11_1;
with Day11_2;
with Day14_1;
with Day14_2;
with Day15_1;
with Day15_2;
with Day17_1;
with Day25_1;

package body Puzzle is
   Solutions : constant array (Day_Type, Part_Type) of Any_Solution :=
      (1 => (1 => Day1_1'Access, 2 => Day1_2'Access),
       2 => (1 => Day2_1'Access, 2 => Day2_2'Access),
       3 => (1 => Day3_1'Access, 2 => Day3_2'Access),
       4 => (1 => Day4_1'Access, 2 => Day4_2'Access),
       5 => (1 => Day5_1'Access, 2 => Day5_2'Access),
       6 => (1 => Day6_1'Access, 2 => Day6_2'Access),
       7 => (1 => Day7_1'Access, 2 => Day7_2'Access),
       8 => (1 => Day8_1'Access, 2 => Day8_2'Access),
       9 => (1 => Day9_1'Access, 2 => Day9_2'Access),
       10 => (1 => Day10_1'Access, 2 => Day10_2'Access),
       11 => (1 => Day11_1'Access, 2 => Day11_2'Access),
       14 => (1 => Day14_1'Access, 2 => Day14_2'Access),
       15 => (1 => Day15_1'Access, 2 => Day15_2'Access),
       17 => (1 => Day17_1'Access, 2 => null),
       25 => (1 => Day25_1'Access, 2 => null),
       others => (others => null));

   function Get_Solution
      (Day  : Day_Type;
       Part : Part_Type)
       return Any_Solution
   is
   begin
      return Solutions (Day, Part);
   end Get_Solution;
end Puzzle;
