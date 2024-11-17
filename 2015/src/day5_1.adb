with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day5_1 is
   function Is_Vowel
      (Ch : Character)
      return Boolean
   is
   begin
      case Ch is
         when 'a' | 'e' | 'i' | 'o' | 'u' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Vowel;

   function Is_Nice
      (S : String)
      return Boolean
   is
      Vowels : Natural := 0;
      Bad, Consecutive : Boolean := False;
      Prev : Character := ASCII.NUL;
   begin
      for Ch of S loop
         if Is_Vowel (Ch) then
            Vowels := Vowels + 1;
         end if;

         if Ch = Prev then
            Consecutive := True;
         end if;

         if (Prev = 'a' and then Ch = 'b') or else
            (Prev = 'c' and then Ch = 'd') or else
            (Prev = 'p' and then Ch = 'q') or else
            (Prev = 'x' and then Ch = 'y')
         then
            Bad := True;
         end if;

         exit when Bad;

         Prev := Ch;
      end loop;

      return Vowels >= 3 and then Consecutive and then not Bad;
   end Is_Nice;

   Nice : Natural := 0;
begin
   while not Input.End_Of_Input loop
      if Is_Nice (Input.Read_Until (CRLF)) then
         Nice := Nice + 1;
      end if;
   end loop;
   Output.Put (Nice);
end Day5_1;
