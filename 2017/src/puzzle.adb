with Day4_1;

package body Puzzle is
   Solutions : constant array (Day_Type, Part_Type) of Any_Solution :=
      (4 => (1 => Day4_1'Access, 2 => null),
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
