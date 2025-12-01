pragma Extensions_Allowed (On);
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers;
with Advent.Input;
with Advent.Output;

procedure Day9_1
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
   type Position is new Natural;
   type Block_Id is new Natural;
   subtype Long is Long_Long_Integer;

   function Hash
      (Item : Position)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Item));

   package Block_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Position,
       Element_Type     => Block_Id,
       Hash             => Hash,
       Equivalent_Keys  => "=");
   use Block_Maps;

   package Position_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Position);
   use Position_Vectors;

   function Is_Contiguous
      (M    : Block_Maps.Map;
       Last : Position)
       return Boolean
   is
   begin
      for I in reverse 0 .. Last loop
         if not Contains (M, I) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Contiguous;

   function Num (Ch : Character) return Natural
   is (Character'Pos (Ch) - Character'Pos ('0'));

   Blocks   : Block_Maps.Map;
   Free     : Position_Vectors.Vector;
   Ch       : Character;
   Id       : Block_Id := 0;
   Pos      : Position := 0;

   Last_Pos, First_Free : Position;
   Sum : Long := 0;
begin
   while not Input.End_Of_Input loop
      Input.Get (Ch);
      exit when Ch not in '0' .. '9';

      for I in 1 .. Num (Ch) loop --  file blocks
         Insert (Blocks, Pos, Id);
         Pos := Pos + 1;
      end loop;
      Id := Id + 1;

      Input.Get (Ch);
      exit when Ch not in '0' .. '9';
      for I in 1 .. Num (Ch) loop
         Append (Free, Pos);
         Pos := Pos + 1;
      end loop;
   end loop;

   Last_Pos := Pos - 1;

   loop
      while not Contains (Blocks, Last_Pos) loop
         Last_Pos := Last_Pos - 1;
      end loop;

      --  Print (Blocks, Last_Pos);

      exit when Is_Contiguous (Blocks, Last_Pos);

      First_Free := First_Element (Free);
      Delete_First (Free);
      Append (Free, Last_Pos);

      Insert (Blocks, First_Free, Element (Blocks, Last_Pos));
      Delete (Blocks, Last_Pos);
   end loop;

   Id := Id - 1;
   Pos := Pos - 1;

   for Cursor in Iterate (Blocks) loop
      Pos := Key (Cursor);
      Id := Element (Cursor);
      Sum := Sum + Long (Pos) * Long (Id);
   end loop;

   Output.Put_Long (Sum);
end Day9_1;
