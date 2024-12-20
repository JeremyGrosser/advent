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

package body Puzzle is
   Solutions : constant array (Day_Type, Part_Type) of Any_Solution :=
      (1 => (1 => Day1_1'Access, 2 => Day1_2'Access),
       2 => (1 => Day2_1'Access, 2 => Day2_2'Access),
       3 => (1 => Day3_1'Access, 2 => Day3_2'Access),
       4 => (1 => Day4_1'Access, 2 => Day4_2'Access),
       5 => (1 => Day5_1'Access, 2 => Day5_2'Access),
       6 => (1 => Day6_1'Access, 2 => Day6_2'Access),
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
