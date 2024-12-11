pragma Ada_2022;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day11_2 is
   subtype Int is Long_Long_Integer;

   function Hash (I : Int) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (I mod 2 ** 32));

   package Int_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type => Int,
       Element_Type => Int,
       Hash => Hash,
       Equivalent_Keys => "=");
   use Int_Maps;

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Int);
   use Int_Vectors;

   function Strip_Leading
      (Ch : Character;
       Str : String)
       return String
   is
   begin
      for I in Str'Range loop
         if Str (I) /= Ch then
            return Str (I .. Str'Last);
         end if;
      end loop;
      return "" & Ch;
   end Strip_Leading;

   function Blink
      (Stone : Int)
      return Int_Vectors.Vector
   is
   begin
      if Stone = 0 then
         return [1];
      else
         declare
            Str   : constant String := Strip_Leading (' ', Stone'Image);
            Split : constant Natural := Str'Last - Str'Length / 2;
         begin
            if Str'Length >= 2 and then Str'Length mod 2 = 0 then
               return [Int'Value (Str (Str'First .. Split)),
                       Int'Value (Str (Split + 1 .. Str'Last))];
            else
               return [Stone * 2024];
            end if;
         end;
      end if;
   end Blink;

   type Cache_Key is record
      Stone : Int;
      Steps : Natural;
   end record;

   function Cache_Hash
      (Item : Cache_Key)
      return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
   begin
      return Hash (Item.Stone) * Hash_Type (Item.Steps);
   end Cache_Hash;

   package Int_Vector_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type => Cache_Key,
       Element_Type => Int,
       Hash => Cache_Hash,
       Equivalent_Keys => "=");
   use Int_Vector_Maps;

   Cache : Int_Vector_Maps.Map;

   function Fast_Forward_Cached
      (Stone : Int;
       Steps : Natural)
       return Int;

   function Fast_Forward
      (Stone : Int;
       Steps : Natural)
      return Int
   is
      Next_Step : constant Int_Vectors.Vector := Blink (Stone);
      Sum : Int := 0;
   begin
      if Steps = 1 then
         return Int (Length (Next_Step));
      else
         for S of Next_Step loop
            Sum := Sum + Fast_Forward_Cached (S, Steps - 1);
         end loop;
         return Sum;
      end if;
   end Fast_Forward;

   function Fast_Forward_Cached
      (Stone : Int;
       Steps : Natural)
       return Int
   is
      Key : constant Cache_Key := (Stone, Steps);
   begin
      if not Contains (Cache, Key) then
         Insert (Cache, Key, Fast_Forward (Stone, Steps));
      end if;
      return Element (Cache, Key);
   end Fast_Forward_Cached;

   Stones : Int_Maps.Map;
   Sum : Int := 0;
begin
   while not Input.End_Of_Input loop
      Insert (Stones, Int'Value (Input.Read_Until (" " & ASCII.LF)), 1);
   end loop;

   for Cursor in Iterate (Stones) loop
      Sum := Sum + Fast_Forward_Cached (Key (Cursor), 75);
   end loop;

   Output.Put_Long (Sum);
end Day11_2;
