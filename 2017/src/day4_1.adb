pragma Ada_2022;
pragma Style_Checks ("M120");
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Advent.Input;
with Advent.Output;
with Advent; use Advent;

procedure Day4_1 is
   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   use String_Vectors;
   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
      (Element_Type => String,
       Hash => Ada.Strings.Hash,
       Equivalent_Elements => "=");
   use String_Sets;

   function Is_Valid
      (Words : String_Vectors.Vector)
      return Boolean
   is
      Seen : String_Sets.Set;
   begin
      for Word of Words loop
         if Contains (Seen, Word) then
            return False;
         else
            Include (Seen, Word);
         end if;
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
               Append (Words, To_String (Buffer));
               return Words;
            when ' ' =>
               Append (Words, To_String (Buffer));
               Buffer := Null_Unbounded_String;
            when others =>
               Append (Buffer, Ch);
         end case;
      end loop;
      return Empty_Vector;
   end Parse_Line;

   Count : Natural := 0;
begin
   while not Input.End_Of_Input loop
      declare
         Line : constant String_Vectors.Vector := Parse_Line;
      begin
         if Is_Valid (Line) then
            Output.Log ("PASS ", False);
            Count := Count + 1;
         else
            Output.Log ("FAIL ", False);
         end if;
         Output.Log (Line'Image);
      end;
   end loop;
   Output.Put (Count);
end Day4_1;
