with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Containers.Vectors;

procedure Day6_2 is
   package Character_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Character);
   use Character_Vectors;

   function Has_Duplicates
      (V : Vector)
      return Boolean
   is
      A, B : Character;
   begin
      for I in First_Index (V) .. Last_Index (V) loop
         for J in I + 1 .. Last_Index (V) loop
            A := V (I);
            B := V (J);
            if A = B then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Has_Duplicates;

   Prefix_Length : Natural := 0;
   Window : Vector := Empty_Vector;
   Ch : Character;
begin
   while not End_Of_Input loop
      Character'Read (Input, Ch);
      Append (Window, Ch);

      Prefix_Length := Prefix_Length + 1;

      if Natural (Length (Window)) > 14 then
         Delete_First (Window);
      end if;

      if Natural (Length (Window)) = 14 and then not Has_Duplicates (Window) then
         Put (Output, Prefix_Length);
         New_Line (Output);
         return;
      end if;
   end loop;
end Day6_2;
