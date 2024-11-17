with Advent; use Advent;
with Advent.Input;
with Advent.Output;

with MD5;

procedure Day4_2 is
   Key : constant String := Input.Read_Until (CRLF);

   function Find_Hash
      return Natural
   is
      C : MD5.Context;
   begin
      for I in 1 .. 100_000_000 loop
         MD5.Init (C);
         MD5.Update (C, Key);
         declare
            use type MD5.Byte_Array;
            Num : constant String := Natural'Image (I);
            Digest : MD5.Fingerprint;
         begin
            MD5.Update (C, Num (2 .. Num'Last));
            MD5.Final (C, Digest);
            if Digest (1 .. 3) = (0, 0, 0) then
               return I;
            end if;
         end;
      end loop;

      return 0;
   end Find_Hash;
begin
   Output.Put (Find_Hash);
end Day4_2;
