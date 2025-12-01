pragma Style_Checks ("M120");
pragma Extensions_Allowed (On);
pragma SPARK_Mode (On);
with Advent.Input;
with Advent.Output;

procedure Day1_2
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
   type Pos is range 0 .. 99;
   Dial : Pos := 50;
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
            if Distance not in -1000 .. 1000 then
               Output.Log ("Distance out of range: ", False);
               Output.Log (Distance);
               exit;
            end if;

            for I in 1 .. Distance loop
               if Negate then
                  if Dial = 0 then
                     Dial := 99;
                  else
                     Dial := Dial - 1;
                  end if;
               else
                  if Dial = 99 then
                     Dial := 0;
                  else
                     Dial := Dial + 1;
                  end if;
               end if;

               if Dial = 0 then
                  if Count = Natural'Last then
                     Output.Log ("Zero count exceeds Natural'Last");
                     exit;
                  else
                     Count := Count + 1;
                  end if;
               end if;
            end loop;
         when others =>
            Output.Log ("Invalid character in input: " & Input.Peek);
            exit;
      end case;
   end loop;
   Output.Put (Count);
end Day1_2;
