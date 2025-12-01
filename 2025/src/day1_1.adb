pragma Style_Checks ("M120");
pragma Extensions_Allowed (On);
with Advent.Input;
with Advent.Output;

procedure Day1_1
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
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
            Input.Get_Integer (Distance);
            if Distance not in 0 .. 1000 then
               Output.Log ("Distance out of range: ", False);
               Output.Log (Distance);
               exit;
            end if;
            if Negate then
               Distance := Distance * (-1);
            end if;
            pragma Assert (Distance in -1000 .. 1000 and then Dial in -1_000_000 .. 1_000_000);
            Dial := Dial + Distance;
            while Dial < 0 loop
               Dial := Dial + 100;
            end loop;
            while Dial > 99 loop
               Dial := Dial - 100;
            end loop;
            if Dial = 0 then
               Count := Count + 1;
            end if;
         when others =>
            raise Program_Error with "Invalid character in input: " & Input.Peek;
      end case;
   end loop;
   Output.Put (Count);
end Day1_1;
