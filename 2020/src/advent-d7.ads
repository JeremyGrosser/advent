with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers;

package Advent.D7 is
   Syntax_Error : exception;

   type Parser is private;

   procedure Feed
      (This : in out Parser;
       Text : String);

   procedure Clear
      (This : in out Parser);

   function Part_1
      (Filename : String)
      return Integer;

   function Part_2
      (Filename : String)
      return Integer;

   procedure Run;

private

   package String_Holders is new Ada.Containers.Indefinite_Holders (String);
   use String_Holders;

   type Token_Name is
      (Color, Separator, Contain, Number, Stop);

   type Token is record
      Name  : Token_Name;
      Value : String_Holders.Holder;
   end record;

   subtype Bag is String_Holders.Holder;
   No_Bag : constant Bag := To_Holder ("");

   package Bag_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Bag);
   use Bag_Vectors;
   type Bag_List is access Bag_Vectors.Vector;

   function Hash (B : Bag)
      return Ada.Containers.Hash_Type;

   package Bag_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type        => Bag,
       Element_Type    => Bag_List,
       Hash            => Hash,
       Equivalent_Keys => String_Holders."=");

   type Parser is record
      Adjacency : Bag_Maps.Map;
      Previous  : Token;
      Parent    : Bag;
      Number    : Natural := 0;
      Expand    : Boolean := False;
   end record;

   procedure Emit
      (This : in out Parser;
       T    : Token);

   function Parse_Color
      (S : String) return Token;
   function Parse_Separator
      (S : String) return Token;
   function Parse_Contain
      (S : String) return Token;
   function Parse_Number
      (S : String) return Token;

   function Last (T : Token)
      return Positive;

   procedure Add_Edge
      (This : in out Bag_Maps.Map;
       From : Bag;
       To   : Bag);

   function Strip_Bags
      (B : Bag)
      return Bag;

   procedure Print
      (This : Parser);

   function Contains
      (This   : Parser;
       Root   : Bag;
       Target : Bag)
      return Boolean;

   function Num_Children
      (This : Parser;
       Root : Bag)
      return Natural;

end Advent.D7;
