pragma Style_Checks ("M120");
pragma Extensions_Allowed (On);
with Advent.Input;
with Advent.Output;

procedure Day2_1
   (Input  : in out Advent.Input.Buffer;
    Output : Advent.Output.Buffer)
is
   subtype Int is Long_Long_Integer;

   function To_String
      (N : Int)
      return String
   is
      X : Int := N;
      S : String (1 .. 16);
   begin
      for I in reverse S'Range loop
         S (I) := Character'Val (Natural (X mod 10) + Character'Pos ('0'));
         X := X / 10;
         if X = 0 then
            return S (I .. S'Last);
         end if;
      end loop;
      return S;
   end To_String;

   function Is_Invalid
      (N : Int)
      return Boolean
   is
      S   : constant String := To_String (N);
      Cut : constant Natural := S'Length / 2;
   begin
      if S (S'First .. S'First + Cut - 1) = S (S'First + Cut .. S'Last) then
         return True;
      else
         return False;
      end if;
   end Is_Invalid;

   First, Last : Int;
   Sum : Int := 0;
begin
   while not Input.End_Of_Input loop
      Input.Get_Long (First);
      Input.Expect ('-');
      Input.Get_Long (Last);
      for I in First .. Last loop
         if Is_Invalid (I) then
            Sum := Sum + I;
         end if;
      end loop;
      if Input.Peek /= ',' then
         exit;
      else
         Input.Seek (1);
      end if;
   end loop;
   Output.Put_Long (Sum);
end Day2_1;
