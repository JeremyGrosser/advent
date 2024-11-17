with Day1_1;
with Day1_2;
with Day2_1;
with Day2_2;
with Day3_1;

package body Puzzle is
   Solutions : constant array (Day_Type, Part_Type) of Any_Solution :=
      (1 => (1 => Day1_1'Access, 2 => Day1_2'Access),
       2 => (1 => Day2_1'Access, 2 => Day2_2'Access),
       3 => (1 => Day3_1'Access, 2 => null),
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
