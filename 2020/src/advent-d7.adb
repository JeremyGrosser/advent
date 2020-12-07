with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat; use GNAT.Regpat;

package body Advent.D7 is
   function Parse_Color
      (S : String)
      return Token
   is
      Pattern : constant Pattern_Matcher := Compile ("([a-z]+ [a-z]+|other) bags?");
      Matches : Match_Array (0 .. 0);
   begin
      Match (Pattern, S, Matches);
      if Matches (0) = No_Match then
         raise Syntax_Error with "Expected COLOR, got " & S & " at" & S'First'Image;
      else
         return (Color, To_Holder (S (S'First .. Matches (0).Last)));
      end if;
   end Parse_Color;

   function Parse_Separator
      (S : String)
      return Token
   is
      Pattern : constant Pattern_Matcher := Compile ("[ ,.]+");
      Matches : Match_Array (0 .. 0);
   begin
      Match (Pattern, S, Matches);
      if Matches (0) = No_Match then
         raise Syntax_Error with "Expected SPACE, got " & S & " at" & S'First'Image;
      else
         if S (S'First .. Matches (0).Last) = "." then
            return (Stop, To_Holder (S (S'First .. Matches (0).Last)));
         else
            return (Separator, To_Holder (S (S'First .. Matches (0).Last)));
         end if;
      end if;
   end Parse_Separator;

   function Parse_Contain
      (S : String)
      return Token
   is
      Literal : constant String := "contain";
   begin
      if S'Length < Literal'Length then
         raise Syntax_Error with "Expected CONTAIN, got " & S & " at" & S'First'Image;
      else
         return (Contain, To_Holder (S (S'First .. S'First + Literal'Length - 1)));
      end if;
   end Parse_Contain;

   function Parse_Number
      (S : String)
      return Token
   is
      Pattern : constant Pattern_Matcher := Compile ("^([0-9]+|no)");
      Matches : Match_Array (0 .. 0);
   begin
      Match (Pattern, S, Matches);
      if Matches (0) = No_Match then
         raise Syntax_Error with "Expected NUMBER, got " & S & " at" & S'First'Image;
      else
         return (Number, To_Holder (S (S'First .. Matches (0).Last)));
      end if;
   end Parse_Number;

   function Last (T : Token)
      return Positive
   is
   begin
      return Element (T.Value)'Last + 1;
   end Last;

   procedure Feed
      (This : in out Parser;
       Text : String)
   is
      T : Token;
   begin
      This.Parent := No_Bag;
      T := Parse_Color (Text);
      Emit (This, T);
      T := Parse_Separator (Text (Last (T) .. Text'Last));
      T := Parse_Contain (Text (Last (T) .. Text'Last));
      Emit (This, T);
      T := Parse_Separator (Text (Last (T) .. Text'Last));
      while Last (T) < Text'Last and then T.Name /= Stop loop
         T := Parse_Number (Text (Last (T) .. Text'Last));
         Emit (This, T);
         T := Parse_Separator (Text (Last (T) .. Text'Last));
         T := Parse_Color (Text (Last (T) .. Text'Last));
         Emit (This, T);
         T := Parse_Separator (Text (Last (T) .. Text'Last));
      end loop;
      if T.Name /= Stop then
         raise Syntax_Error with "Reached end of line before STOP token";
      end if;
      Emit (This, T);
   end Feed;

   procedure Add_Edge
      (This : in out Bag_Maps.Map;
       From : Bag;
       To   : Bag)
   is
      V : Bag_List;
   begin
      if not This.Contains (From) then
         V := new Bag_Vectors.Vector;
         This.Insert (From, V);
      else
         V := This.Element (From);
      end if;
      V.Append (To);
   end Add_Edge;

   function Strip_Bags
      (B : Bag)
      return Bag
   is
      use Ada.Strings.Fixed;
      V : constant String := Element (B);
   begin
      return To_Holder (V (V'First .. Index (V, "bag", V'First) - 2));
   end Strip_Bags;

   procedure Emit
      (This : in out Parser;
       T    : Token)
   is
      Bag_Color : Bag;
   begin
      case T.Name is
         when Color =>
            if This.Parent /= No_Bag and Element (T.Value) /= "other bags" then
               if This.Expand then
                  for I in 1 .. This.Number loop
                     Add_Edge (This.Adjacency, Strip_Bags (This.Parent), Strip_Bags (T.Value));
                  end loop;
               else
                  Add_Edge (This.Adjacency, Strip_Bags (This.Parent), Strip_Bags (T.Value));
               end if;
            end if;
         when Contain =>
            This.Parent := Bag (This.Previous.Value);
         when Number =>
            if Element (T.Value) = "no" then
               This.Number := 0;
            else
               This.Number := Natural'Value (Element (T.Value));
            end if;
         when Separator =>
            null;
         when Stop =>
            null;
      end case;
      This.Previous := T;
   end Emit;

   function Hash (B : Bag)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash (Element (B));
   end Hash;

   procedure Print (This : Parser)
   is
      use Bag_Maps;
      Parent, Child : Bag;
   begin
      Put_Line ("digraph G {");
      for Cursor in This.Adjacency.Iterate loop
         Parent := Key (Cursor);
         for VCursor in Element (Cursor).Iterate loop
            Child := Element (VCursor);
            Put_Line ("  " & '"' & Element (Parent) & '"' & " -> " & '"' & Element (Child) & '"' & ';');
         end loop;
      end loop;
      Put_Line ("}");
   end Print;

   procedure Clear
      (This : in out Parser)
   is
      procedure Free is new Ada.Unchecked_Deallocation (Bag_Vectors.Vector, Bag_List);
   begin
      for Cursor in This.Adjacency.Iterate loop
         declare
            V : Bag_List := Bag_Maps.Element (Cursor);
         begin
            Free (V);
         end;
      end loop;
   end Clear;

   function Contains
      (This   : Parser;
       Root   : Bag;
       Target : Bag)
      return Boolean
   is
   begin
      if not This.Adjacency.Contains (Root) then
         return False;
      end if;
      for Child of This.Adjacency.Element (Root).all loop
         if Child = Target or else Contains (This, Child, Target) then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   function Num_Children
      (This : Parser;
       Root : Bag)
      return Natural
   is
      Count : Natural := 0;
   begin
      if not This.Adjacency.Contains (Root) then
         return 0;
      end if;
      for Child of This.Adjacency.Element (Root).all loop
         Count := Count + Num_Children (This, Child) + 1;
      end loop;
      return Count;
   end Num_Children;

   function Part_1
      (Filename : String)
      return Integer
   is
      use Bag_Maps;
      Target      : constant Bag := To_Holder ("shiny gold");
      Input       : File_Type;
      Line_Number : Positive := 1;
      Count       : Natural := 0;
      P           : Parser;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         Feed (P, Get_Line (Input));
         Line_Number := Line_Number + 1;
      end loop;
      Close (Input);
      --  Print (P);
      for Cursor in P.Adjacency.Iterate loop
         if Contains (P, Key (Cursor), Target) then
            Count := Count + 1;
         end if;
      end loop;
      Clear (P);
      return Count;
   exception
      when E : Syntax_Error =>
         Put_Line ("");
         Put_Line ("Syntax error on line" & Line_Number'Image & ": " & Exception_Message (E));
         return 0;
   end Part_1;

   function Part_2
      (Filename : String)
      return Integer
   is
      use Bag_Maps;
      Target      : constant Bag := To_Holder ("shiny gold");
      Input       : File_Type;
      Line_Number : Positive := 1;
      Count       : Natural := 0;
      P           : Parser;
   begin
      P.Expand := True;
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         Feed (P, Get_Line (Input));
         Line_Number := Line_Number + 1;
      end loop;
      Close (Input);
      --  Print (P);
      Count := Num_Children (P, Target);
      Clear (P);
      return Count;
   exception
      when E : Syntax_Error =>
         Put_Line ("");
         Put_Line ("Syntax error on line" & Line_Number'Image & ": " & Exception_Message (E));
         return 0;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "7.1", "input/d7-test", 4);
      Put_Line ("7.1 solution: " & Part_1 ("input/d7")'Image);

      Test (Part_2'Access, "7.2", "input/d7-test2", 126);
      Put_Line ("7.2 solution: " & Part_2 ("input/d7")'Image);
   end Run;
end Advent.D7;
