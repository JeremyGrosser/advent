with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Execution_Time;
with Str; use Str;

package body Advent.D4 is
   function Read_Until_Space
      (Input : File_Type)
      return String
   is
      Ch : Character;
   begin
      loop
         exit when End_Of_File (Input);
         Get_Immediate (Input, Ch);
         exit when Ch = ' ' or Ch = ASCII.LF;
         return Ch & Read_Until_Space (Input);
      end loop;
      return "";
   end Read_Until_Space;

   function Read_Passport
      (Input : File_Type)
      return Passport
   is
      P : Passport := Empty_Passport;
   begin
      loop
         declare
            Token : constant String := Read_Until_Space (Input);
            Key   : constant String := Split (Token, ':', 0);
            Value : constant String := Split (Token, ':', 1);
         begin
            if Token'Length = 0 then
               return P;
            else
               P.Insert (Key, Value);
            end if;
         end;
      end loop;
   end Read_Passport;

   function Has_Required_Fields
      (P : Passport)
      return Boolean
   is
   begin
      return P.Contains ("byr") and then
             P.Contains ("iyr") and then
             P.Contains ("eyr") and then
             P.Contains ("hgt") and then
             P.Contains ("hcl") and then
             P.Contains ("ecl") and then
             P.Contains ("pid");
   end Has_Required_Fields;

   function Check_Number_Range
      (S           : String;
       First, Last : Integer)
      return Boolean
   is
      N : constant Integer := Integer'Value (S);
   begin
      return N >= First and N <= Last;
   end Check_Number_Range;

   function Check_Height
      (S : String)
      return Boolean
   is
   begin
      if S'Length < 4 then
         return False;
      end if;

      declare
         Units : constant String := S (S'Last - 1 .. S'Last);
         N     : constant Integer := Integer'Value (S (S'First .. S'Last - 2));
      begin
         if Units = "in" then
            return N >= 59 and N <= 76;
         elsif Units = "cm" then
            return N >= 150 and N <= 193;
         else
            return False;
         end if;
      end;
   end Check_Height;

   function Check_Hair
      (S : String)
      return Boolean
   is
      Valid_Chars : constant String := "0123456789abcdef";
   begin
      if S'Length /= 7 then
         return False;
      end if;

      if S (S'First) /= '#' then
         return False;
      end if;

      for I in S'First + 1 .. S'Last loop
         if Find (Valid_Chars, S (I)) = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Check_Hair;

   function Check_Eyes
      (S : String)
      return Boolean
   is
      type Color_Array is array (Positive range <>) of String (1 .. 3);
      Valid_Colors : constant Color_Array :=
         ("amb", "blu", "brn", "gry", "grn", "hzl", "oth");
   begin
      if S'Length /= 3 then
         return False;
      end if;
      for Color of Valid_Colors loop
         if S = Color then
            return True;
         end if;
      end loop;
      return False;
   end Check_Eyes;

   function Check_Passport_Id
      (S : String)
      return Boolean
   is
   begin
      if S'Length /= 9 then
         return False;
      end if;

      for C of S loop
         if not Ada.Characters.Handling.Is_Digit (C) then
            return False;
         end if;
      end loop;
      return True;
   end Check_Passport_Id;

   function Validate
      (P : Passport)
      return Boolean
   is
   begin
      return Has_Required_Fields (P) and then
             Check_Number_Range (P.Element ("byr"), 1920, 2002) and then
             Check_Number_Range (P.Element ("iyr"), 2010, 2020) and then
             Check_Number_Range (P.Element ("eyr"), 2020, 2030) and then
             Check_Height (P.Element ("hgt")) and then
             Check_Hair (P.Element ("hcl")) and then
             Check_Eyes (P.Element ("ecl")) and then
             Check_Passport_Id (P.Element ("pid"));
   exception
      when Constraint_Error =>
         return False;
   end Validate;

   function Part_1
      (Filename : String)
      return Integer
   is
      Input : File_Type;
      Valid : Natural := 0;
      P     : Passport;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         P := Read_Passport (Input);
         if Has_Required_Fields (P) then
            Valid := Valid + 1;
         end if;
      end loop;
      Close (Input);
      return Valid;
   end Part_1;

   function Part_2
      (Filename : String)
      return Integer
   is
      use Ada.Execution_Time;
      T : CPU_Time;

      Input : File_Type;
      Valid : Natural := 0;
      P     : Passport;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         P := Read_Passport (Input);
         T := Ada.Execution_Time.Clock;
         if Validate (P) then
            Valid := Valid + 1;
         end if;
         Total_Time := Total_Time + (Clock - T);
      end loop;
      Close (Input);
      return Valid;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "4.1", "input/d4-test", 2);
      Put_Line ("4.1 solution: " & Part_1 ("input/d4")'Image);

      Assert (Check_Number_Range ("1921", 1920, 2002) = True);
      Assert (Check_Number_Range ("1900", 1920, 2002) = False);
      Assert (Check_Height ("21cm") = False);
      Assert (Check_Height ("180cm") = True);
      Assert (Check_Height ("21in") = False);
      Assert (Check_Height ("59in") = True);
      Assert (Check_Hair ("#0fffff") = True);
      Assert (Check_Hair ("fffffff") = False);
      Assert (Check_Hair ("#ABCDEF") = False);
      Assert (Check_Eyes ("amb") = True);
      Assert (Check_Eyes ("AMB") = False);
      Assert (Check_Passport_Id ("087499704") = True);
      Assert (Check_Passport_Id ("08749970?") = False);

      Test (Part_2'Access, "4.2 T1", "input/d4.2-test1", 1);
      Test (Part_2'Access, "4.2 T2", "input/d4.2-test2", 0);
      Test (Part_2'Access, "4.2 T3", "input/d4.2-test3", 4);

      Total_Time := Time_Span_Zero;
      Put_Line ("4.2 solution: " & Part_2 ("input/d4")'Image);
      Put_Line ("Execution time: " & To_Duration (Total_Time)'Image & 's');
   end Run;
end Advent.D4;
