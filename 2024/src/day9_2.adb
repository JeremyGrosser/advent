pragma Ada_2022;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day9_2 is
   type Position is new Natural;
   type Block_Id is new Natural;
   subtype Long is Long_Long_Integer;

   type Span is record
      First, Last : Position;
   end record;

   function Hash
      (Item : Position)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Item));

   function Hash
      (Item : Block_Id)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Item));

   package Position_Block_Id_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Position,
       Element_Type     => Block_Id,
       Hash             => Hash,
       Equivalent_Keys  => "=");

   package Block_Span_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type => Block_Id,
       Element_Type => Span,
       Hash => Hash,
       Equivalent_Keys => "=");

   package Position_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Position);
   use Position_Vectors;

   procedure Print
      (M    : Position_Block_Id_Maps.Map;
       Last : Position)
   is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Position_Block_Id_Maps;
   begin
      New_Line (Standard_Error);
      for I in 0 .. Last loop
         if Contains (M, I) then
            Put (Standard_Error, Integer (Element (M, I)), Width => 0);
         else
            Put (Standard_Error, '.');
         end if;
      end loop;
      New_Line (Standard_Error);
   end Print;

   function Num (Ch : Character) return Natural
   is (Character'Pos (Ch) - Character'Pos ('0'));

   function Is_Free
      (M : Position_Block_Id_Maps.Map;
       First, Last : Position)
      return Boolean
   is
   begin
      for Pos in First .. Last loop
         if Position_Block_Id_Maps.Contains (M, Pos) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Free;

   procedure Add_Block
      (M  : in out Position_Block_Id_Maps.Map;
       Id : Block_Id;
       First, Last : Position)
   is
   begin
      for Pos in First .. Last loop
         Position_Block_Id_Maps.Insert (M, Pos, Id);
      end loop;
   end Add_Block;

   procedure Delete_Block
      (M    : in out Position_Block_Id_Maps.Map;
       Id   : Block_Id;
       Last : Position)
   is
      use Position_Block_Id_Maps;
      Block_Last : Position := Last;
   begin
      loop
         exit when Contains (M, Block_Last) and then Element (M, Block_Last) = Id;
         Block_Last := Block_Last - 1;
      end loop;

      for Pos in reverse 0 .. Block_Last loop
         exit when not Contains (M, Pos) or else Element (M, Pos) /= Id;
         Delete (M, Pos);
      end loop;
   end Delete_Block;

   Block_Span : Block_Span_Maps.Map;
   Blocks   : Position_Block_Id_Maps.Map;
   Ch       : Character;
   Id       : Block_Id := 0;
   Pos      : Position := 0;

   Sum : Long := 0;
begin
   while not Input.End_Of_Input loop
      Input.Get (Ch);
      exit when Ch not in '0' .. '9';

      Block_Span_Maps.Insert (Block_Span, Id,
         (First => Pos,
          Last  => Pos + Position (Num (Ch))));

      for I in 1 .. Num (Ch) loop --  file blocks
         Position_Block_Id_Maps.Insert (Blocks, Pos, Id);
         Pos := Pos + 1;
      end loop;
      Id := Id + 1;

      Input.Get (Ch);
      exit when Ch not in '0' .. '9';
      Pos := Pos + Position (Num (Ch));
   end loop;

   declare
      Last_Pos : constant Position := Pos - 1;
      Last_Id  : constant Block_Id := Id - 1;
      First_Free, Last_Free : Position;
      Block : Span;
   begin
      --  Print (Blocks, Last_Pos);

      for Id in reverse 0 .. Last_Id loop
         Block := Block_Span_Maps.Element (Block_Span, Id);
         Output.Log (Id'Image);
         Output.Log (Block'Image);
         for Pos in 0 .. Block.First loop
            First_Free := Pos;
            Last_Free := Pos + Block.Last - Block.First - 1;
            Output.Log ("Try " & First_Free'Image & " .. " & Last_Free'Image);
            if Last_Free <= Last_Pos and then
               Is_Free (Blocks, First_Free, Last_Free)
            then
               Delete_Block (Blocks, Id, Last_Pos);
               Add_Block (Blocks, Id, First_Free, Last_Free);
               Output.Log (First_Free'Image & " .. " & Last_Free'Image);
               --  Print (Blocks, Last_Pos);
               exit;
            end if;
         end loop;
      end loop;
   end;


   for Cursor in Position_Block_Id_Maps.Iterate (Blocks) loop
      Pos := Position_Block_Id_Maps.Key (Cursor);
      Id := Position_Block_Id_Maps.Element (Cursor);
      Sum := Sum + Long (Pos) * Long (Id);
   end loop;

   Output.Put_Long (Sum);
end Day9_2;
