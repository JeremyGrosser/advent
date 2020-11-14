package Solutions.Day_3 is
   Syntax_Error : exception;

   function Part_1
      (Filename    : String;
       Fabric_Size : Positive)
       return Integer;

   function Part_2
      (Filename    : String;
       Fabric_Size : Positive)
       return Integer;

   procedure Run;
end Solutions.Day_3;
