with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

package Advent.D4 is
   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (String, String);

   subtype Passport is String_Maps.Map;
   Empty_Passport : constant Passport := String_Maps.Empty_Map;

   function Read_Until_Space
      (Input : File_Type)
      return String;

   function Read_Passport
      (Input : File_Type)
      return Passport;

   function Has_Required_Fields
      (P : Passport)
      return Boolean;

   function Check_Number_Range
      (S           : String;
       First, Last : Integer)
      return Boolean;

   function Check_Height
      (S : String)
      return Boolean;

   function Check_Hair
      (S : String)
      return Boolean;

   function Validate
      (P : Passport)
      return Boolean;

   procedure Run;
end Advent.D4;
