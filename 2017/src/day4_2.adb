pragma Ada_2022;
pragma Style_Checks ("M120");
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Advent.Input;
with Advent.Output;
with Advent; use Advent;

procedure Day4_2 is
   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   use String_Vectors;
   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
      (Element_Type => String,
       Hash => Ada.Strings.Hash,
       Equivalent_Elements => "=");
   use String_Sets;

   package Character_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Character,
       Element_Type => Natural);
   use Character_Maps;

   function To_Map
      (Word : String)
      return Character_Maps.Map
   is
      M : Character_Maps.Map;
   begin
      for Ch of Word loop
         if Contains (M, Ch) then
            Replace (M, Ch, Element (M, Ch) + 1);
         else
            Include (M, Ch, 1);
         end if;
      end loop;
      return M;
   end To_Map;

   function Is_Valid
      (Words : String_Vectors.Vector)
      return Boolean
   is
      A, B : Character_Maps.Map;
   begin
      for I in First_Index (Words) .. Last_Index (Words) loop
         A := To_Map (Words (I));
         for J in First_Index (Words) .. Last_Index (Words) loop
            if I /= J and then To_Map (Words (J)) = A then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Valid;

   function Parse_Line
      return String_Vectors.Vector
   is
      Words : String_Vectors.Vector;
      Buffer : Unbounded_String;
      Ch : Character;
   begin
      while not Input.End_Of_Input loop
         Input.Get (Ch);
         case Ch is
            when LF =>
               exit;
            when ' ' =>
               Append (Words, To_String (Buffer));
               Buffer := Null_Unbounded_String;
            when others =>
               Append (Buffer, Ch);
         end case;
      end loop;
      Append (Words, To_String (Buffer));
      return Words;
   end Parse_Line;

   Count : Natural := 0;
begin
   while not Input.End_Of_Input loop
      declare
         Line : constant String_Vectors.Vector := Parse_Line;
      begin
         Output.Log (Line'Image);
         if Is_Valid (Line) then
            Output.Log ("PASS ");
            Count := Count + 1;
         else
            Output.Log ("FAIL ");
         end if;
      end;
   end loop;
   Output.Put (Count);
end Day4_2;
