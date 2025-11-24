with Advent.Input;
with Advent.Output;
with Advent; use Advent;

procedure Day9_1 is
   Decompressed_Length : Natural := 0;

   function Read_Natural
      return Natural
   is
      N : Natural := 0;
      Ch : Character;
   begin
      while not Input.End_Of_Input loop
         Ch := Input.Peek;
         exit when Ch not in '0' .. '9';
         Input.Seek (1);
         N := N * 10 + Character'Pos (Ch) - Character'Pos ('0');
      end loop;
      return N;
   end Read_Natural;

   function Match
      (Ch : Character)
      return Boolean
   is
   begin
      if Input.Peek = Ch then
         Input.Seek (1);
         return True;
      else
         return False;
      end if;
   end Match;

   procedure Expect
      (Ch : Character)
   is
   begin
      if not Match (Ch) then
         Output.Log ("Expected '" & Ch & "'");
      end if;
   end Expect;

   procedure Emit
      (Ch : Character)
   is
      pragma Unreferenced (Ch);
   begin
      Decompressed_Length := Decompressed_Length + 1;
   end Emit;

   procedure Deflate is
      Chunk_Length, Repeat : Natural;
   begin
      Chunk_Length := Read_Natural;
      Expect ('x');
      Repeat := Read_Natural;
      Expect (')');

      for I in 1 .. Repeat loop
         for J in 1 .. Chunk_Length loop
            Emit (Input.Peek (J));
         end loop;
      end loop;
      Input.Seek (Chunk_Length);
   end Deflate;

   Ch : Character;
begin
   while not Input.End_Of_Input loop
      Input.Get (Ch);
      case Ch is
         when '(' =>
            Deflate;
         when others =>
            Emit (Ch);
      end case;
   end loop;
   Output.Put (Decompressed_Length);
end Day9_1;
