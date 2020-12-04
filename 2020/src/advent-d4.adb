with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;
with Str; use Str;

package body Advent.D4 is
   function Read_Until_Space
      (F : File_Type)
      return String
   is
      Ch : Character;
   begin
      loop
         exit when End_Of_File (F);
         Get_Immediate (F, Ch);
         exit when Ch = ' ' or Ch = ASCII.LF;
         return Ch & Read_Until_Space (F);
      end loop;
      return "";
   end Read_Until_Space;

   function Read_Passport
      (Input : File_Type)
      return Boolean
   is
      package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
      Token : String (1 .. 32);
      Seen  : String_Sets.Set;
   begin
      loop
         declare
            Token : String := Read_Until_Space (Input);
            Key   : String := Split (Token, ':', 0);
            Value : String := Split (Token, ':', 0);
         begin
            if Token'Length = 0 then
               return Seen.Contains ("byr") and
                      Seen.Contains ("iyr") and
                      Seen.Contains ("eyr") and
                      Seen.Contains ("hgt") and
                      Seen.Contains ("hcl") and
                      Seen.Contains ("ecl") and
                      Seen.Contains ("pid");
            else
               Seen.Include (Key);
            end if;
         end;
      end loop;
   end Read_Passport;

   function Part_1 (Filename : in String)
      return Integer
   is
      Input : File_Type;
      Valid : Natural := 0;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         if Read_Passport (Input) then
            Valid := Valid + 1;
         end if;
      end loop;
      Close (Input);
      return Valid;
   end Part_1;

   function Part_2 (Filename : in String)
      return Integer
   is
   begin
       return 0;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "4.1", "input/d4-test", 2);
      Put_Line ("4.1 solution: " & Part_1 ("input/d4")'Image);

      Test (Part_2'Access, "4.2", "input/d4-test", 99);
      Put_Line ("4.2 solution: " & Part_2 ("input/d4")'Image);
   end Run;
end Advent.D4;
