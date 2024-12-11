pragma Ada_2022;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day11_2 is
   type Int is mod 2 ** 64;

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Int);
   use Int_Vectors;

   function To_String
      (N : Int)
      return String
   is
      Num : constant String := "0123456789";
      Max_Digits : constant := 16;
      S : String (1 .. Max_Digits);
      I : Natural := S'Last;
      X : Int := N;
   begin
      loop
         S (I) := Num (Num'First + Natural (X mod 10));
         X := X / 10;
         exit when X = 0;
         I := I - 1;
      end loop;
      return S (I .. S'Last);
   end To_String;

   function Blink
      (Stone : Int)
      return Int_Vectors.Vector
   is
   begin
      if Stone = 0 then
         return [1];
      else
         declare
            Str   : constant String := To_String (Stone);
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

      function Shift_Right
         (Item   : Hash_Type;
          Amount : Natural)
          return Hash_Type
      with Import, Convention => Intrinsic;

      A : Hash_Type := Hash_Type (Item.Stone mod 2 ** 32);
      B : constant Hash_Type := Hash_Type (Item.Steps);
   begin
      A := A xor B * 2654435769; --  golden ratio prime 2**32
      A := Shift_Right (A, 16) xor A;
      return A;
   end Cache_Hash;

   package Int_Vector_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Cache_Key,
       Element_Type     => Int,
       Hash             => Cache_Hash,
       Equivalent_Keys  => "=");
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

   Stones : Int_Vectors.Vector;
   Sum : Int := 0;
begin
   while not Input.End_Of_Input loop
      Append (Stones, Int'Value (Input.Read_Until (" " & ASCII.LF)));
   end loop;

   for Stone of Stones loop
      Sum := Sum + Fast_Forward_Cached (Stone, 75);
   end loop;

   Output.Put_Long (Long_Long_Integer (Sum));
end Day11_2;
