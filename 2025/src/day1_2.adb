pragma Style_Checks ("M120");
with Advent.Input;
with Advent.Output;
with Advent; use Advent;

procedure Day1_2 is
   Dial : Integer := 50;
   Distance : Integer;
   Negate : Boolean := False;
   Count : Natural := 0;
begin
   while not Input.End_Of_Input loop
      Input.Skip_Whitespace;
      case Input.Peek is
         when 'L' =>
            Negate := True;
            Input.Seek (1);
         when 'R' =>
            Negate := False;
            Input.Seek (1);
         when '0' .. '9' =>
            Distance := Input.Get_Integer;
            for I in 1 .. Distance loop
               if Negate then
                  Dial := Dial - 1;
               else
                  Dial := Dial + 1;
               end if;

               if Dial > 99 then
                  Dial := 0;
               elsif Dial < 0 then
                  Dial := 99;
               end if;

               if Dial = 0 then
                  Count := Count + 1;
               end if;
            end loop;
         when others =>
            raise Program_Error with "Invalid character in input: " & Input.Peek;
      end case;
   end loop;
   Output.Put (Count);
end Day1_2;
