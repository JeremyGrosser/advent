with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;

with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day5_2 is
   function Is_Nice
      (S : String)
      return Boolean
   is
      package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
         (Element_Type => String,
          Hash => Ada.Strings.Hash,
          Equivalent_Elements => "=");
      use String_Sets;
      Pairs : Set;

      I : Positive := S'First;
      Pair, Repeat : Boolean := False;
   begin
      if S'Length < 4 then
         return False;
      end if;

      while I <= S'Last loop
         if I <= S'Last - 1 then
            if Contains (Pairs, S (I .. I + 1)) then
               I := I + 1;
               Pair := True;
            else
               Include (Pairs, S (I .. I + 1));
            end if;
         end if;

         if I <= S'Last - 2 and then S (I) = S (I + 2) and then S (I + 1) /= S (I) then
            Repeat := True;
         end if;

         I := I + 1;
      end loop;

      if Pair and then Repeat then
         Output.Log ("NICE    ", False);
      else
         Output.Log ("NAUGHTY ", False);
      end if;
      Output.Log (S);

      return Pair and then Repeat;
   end Is_Nice;

   Nice : Natural := 0;
begin
   while not Input.End_Of_Input loop
      if Is_Nice (Input.Read_Until (CRLF)) then
         Nice := Nice + 1;
      end if;
   end loop;
   Output.Put (Nice);
end Day5_2;
