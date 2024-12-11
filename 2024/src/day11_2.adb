pragma Ada_2022;
with Ada.Containers.Indefinite_Vectors;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day11_2 is
   subtype Long is Long_Long_Integer;
   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   use String_Vectors;

   function Strip_Leading
      (Ch : Character;
       S  : String)
      return String
   is
   begin
      for I in S'Range loop
         if S (I) /= Ch then
            return S (I .. S'Last);
         end if;
      end loop;
      return "" & Ch;
   end Strip_Leading;

   procedure Blink
      (V : in out String_Vectors.Vector)
   is
      X : String_Vectors.Vector := Empty_Vector;
   begin
      for Stone of V loop
         if Stone = "0" then
            Append (X, "1");
         elsif Stone'Length >= 2 and then Stone'Length mod 2 = 0 then
            declare
               Split : constant Positive := Stone'Last - Stone'Length / 2;
               Left  : constant String := Strip_Leading ('0', Stone (Stone'First .. Split));
               Right : constant String := Strip_Leading ('0', Stone (Split + 1 .. Stone'Last));
            begin
               Append (X, Left);
               Append (X, Right);
            end;
         else
            declare
               New_Stone : constant String := Strip_Leading
                  (' ', Long'Image (Long'Value (Stone) * 2024));
            begin
               Append (X, New_Stone);
            end;
         end if;
      end loop;
      V := X;
   end Blink;

   Stones : String_Vectors.Vector;
begin
   while not Input.End_Of_Input loop
      Append (Stones, Input.Read_Until (" " & ASCII.LF));
   end loop;

   for I in 1 .. 75 loop
      Output.Log (I'Image);
      Blink (Stones);
   end loop;

   Output.Put (Natural (Length (Stones)));
end Day11_2;
