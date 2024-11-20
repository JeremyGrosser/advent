with Ada.Strings.Fixed;

with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day5_2 is
   function Find_Pair
      (S : String)
      return Boolean
   is
      Candidate : String (1 .. 2);
   begin
      for I in S'First .. S'Last - 1 loop
         Candidate := S (I .. I + 1);
         if Ada.Strings.Fixed.Index (S (S'First .. I - 1), Candidate) /= 0 or else
            Ada.Strings.Fixed.Index (S (I + 2 .. S'Last), Candidate) /= 0
         then
            Output.Log ("Found non-overlapping pair of " & Candidate);
            return True;
         end if;
      end loop;
      return False;
   end Find_Pair;

   function Find_Repeats
      (S : String)
      return Boolean
   is
   begin
      for I in S'First .. S'Last - 2 loop
         if S (I) = S (I + 2) then
            Output.Log ("Found repeat: " & S (I .. I + 2));
            return True;
         end if;
      end loop;
      return False;
   end Find_Repeats;

   function Is_Nice
      (S : String)
      return Boolean
   is
   begin
      Output.Log ("Testing " & S);
      return Find_Pair (S) and then Find_Repeats (S);
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
