pragma Style_Checks ("M120");
with Advent.Input;
with Advent.Output;
with Advent; use Advent;

procedure Day1_1 is
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
            if Negate then
               Distance := Distance * (-1);
            end if;
            Output.Log ("Rotate " & Distance'Image);
            Dial := Dial + Distance;
            while Dial < 0 loop
               Dial := Dial + 100;
            end loop;
            while Dial > 99 loop
               Dial := Dial - 100;
            end loop;
            Output.Log (Dial'Image);
            if Dial = 0 then
               Count := Count + 1;
            end if;
         when others =>
            raise Program_Error with "Invalid character in input: " & Input.Peek;
      end case;
   end loop;
   Output.Put (Count);
end Day1_1;
