package Advent is
   type Test_Function is access function (Filename : in String) return Integer;
   Test_Failure : exception;

   procedure Test
      (F        : Test_Function;
       Name     : String;
       Filename : String;
       Expected : Integer);
end Advent;
