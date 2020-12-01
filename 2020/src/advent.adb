with Ada.Text_IO; use Ada.Text_IO;

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

   function Read_Integers
      (Filename : String)
      return Integer_Vectors.Vector
   is
      Input : File_Type;
      Nums  : Integer_Vectors.Vector;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         Nums.Append (Integer'Value (Get_Line (Input)));
      end loop;
      Close (Input);
      return Nums;
   end Read_Integers;
end Advent;
