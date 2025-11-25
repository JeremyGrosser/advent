pragma Ada_2022;
with Advent.Input;
with Advent.Output;
with Advent; use Advent;
with Ada.Containers.Hashed_Sets;
with Ada.Containers;

procedure Day25_1 is
   type Row is range 0 .. 6;
   type Column is range 0 .. 4;
   type Grid is array (Row, Column) of Character;

   type Pin_Array is array (Column) of Natural;

   function Hash
      (Item : Pin_Array)
      return Ada.Containers.Hash_Type
   is
      use type Ada.Containers.Hash_Type;
      Sum : Ada.Containers.Hash_Type := 0;
   begin
      for Pin in Item'Range loop
         Sum := Sum + Ada.Containers.Hash_Type (Item (Pin));
      end loop;
      return Sum;
   end Hash;

   package Pin_Sets is new Ada.Containers.Hashed_Sets (Pin_Array, Hash, "=");

   function Read_Grid
      return Grid
   is
      G : Grid;
   begin
      for Y in Row'Range loop
         for X in Column'Range loop
            Input.Get (G (Y, X));
         end loop;
         Input.Expect (ASCII.LF);
      end loop;
      return G;
   end Read_Grid;

   function Parse_Pins
      (G    : Grid;
       Stop : Character)
      return Pin_Array
   is
      P : Pin_Array := (others => 0);
   begin
      for X in Column'Range loop
         for Y in Row'Range loop
            if G (Y, X) = Stop then
               P (X) := Natural (Row'Last - Y);
               exit;
            end if;
         end loop;
      end loop;
      Output.Log ("Pins:" & P'Image);
      return P;
   end Parse_Pins;

   function Fit
      (Key, Lock : Pin_Array)
      return Boolean
   is
   begin
      for Pin in Key'Range loop
         if Key (Pin) > Lock (Pin) then
            return False;
         end if;
      end loop;
      return True;
   end Fit;

   Keys, Locks : Pin_Sets.Set := Pin_Sets.Empty_Set;
   Count : Natural := 0;
begin
   while not Input.End_Of_Input loop
      case Input.Peek is
         when '.' =>
            Output.Log ("Key");
            Pin_Sets.Include (Keys, Parse_Pins (Read_Grid, '#'));
         when '#' =>
            Output.Log ("Lock");
            Pin_Sets.Include (Locks, Parse_Pins (Read_Grid, '.'));
         when ASCII.LF =>
            Input.Seek (1);
         when others =>
            raise Program_Error with "Unexpected character in input: " & Input.Peek;
      end case;
   end loop;

   for Key of Keys loop
      for Lock of Locks loop
         Output.Log ("Key=" & Key'Image);
         Output.Log ("Lock=" & Lock'Image);
         if Fit (Key, Lock) then
            Output.Log ("Fit");
            Count := Count + 1;
         else
            Output.Log ("No fit");
         end if;
      end loop;
   end loop;

   Output.Put (Count);
end Day25_1;
