package body Advent is
   procedure Test
      (F        : Test_Function;
       Name     : String;
       Filename : String;
       Expected : Integer)
   is
      Result : constant Integer := F (Filename);
   begin
      if Result /= Expected then
         raise Test_Failure with Name & " failed, expected " & Expected'Image & " got " & Result'Image;
      end if;
   end Test;
end Advent;
