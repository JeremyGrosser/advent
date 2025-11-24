with Advent.Input;
with Advent.Output;
with Advent; use Advent;

procedure Day9_2 is
   type Big is range 0 .. 2 ** 64 - 1;

   function Parse_Natural
      (Str    : String;
       Length : out Natural)
      return Natural
   is
      N : Natural := 0;
   begin
      Length := 0;
      for Ch of Str loop
         exit when Ch not in '0' .. '9';
         Length := Length + 1;
         N := N * 10 + Character'Pos (Ch) - Character'Pos ('0');
      end loop;
      return N;
   end Parse_Natural;

   function Deflate
      (Item : String)
      return Big
   is
      Chunk_Length, Repeat : Natural;
      N_Length, R_Length : Natural;
      I : Natural := Item'First;
      Count : Big := 0;
   begin
      Output.Log (Item);
      while I <= Item'Last loop
         case Item (I) is
            when '(' =>
               I := I + 1;
               Chunk_Length := Parse_Natural (Item (I .. Item'Last), N_Length);
               I := I + N_Length + 1;
               Repeat := Parse_Natural (Item (I .. Item'Last), R_Length);
               I := I + R_Length + 1;
               Output.Log ("Chunk_Length=" & Chunk_Length'Image);
               Output.Log ("Repeat=" & Repeat'Image);
               declare
                  Chunk : constant String := Item (I .. I + Chunk_Length - 1);
               begin
                  Count := Count + Deflate (Chunk) * Big (Repeat);
               end;
               I := I + Chunk_Length;
            when others =>
               Count := Count + 1;
               I := I + 1;
         end case;
      end loop;
      Output.Log ("Return " & Count'Image);
      return Count;
   end Deflate;

   function To_String
      (Item : Big)
      return String
   is
      S : String (1 .. 32);
      N : Big := Item;
   begin
      for I in reverse S'Range loop
         if N = 0 then
            return S (I + 1 .. S'Last);
         end if;
         S (I) := Character'Val (Character'Pos ('0') + Natural (N mod 10));
         N := N / 10;
      end loop;
      return S;
   end To_String;

   Result : Big;
begin
   Result := Deflate (Input.Lookahead (Input.Length));
   Output.Put (To_String (Result));
end Day9_2;
