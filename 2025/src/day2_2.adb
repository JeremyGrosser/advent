pragma Style_Checks ("M120");
pragma Extensions_Allowed (On);
with Advent.Input;
with Advent.Output;

procedure Day2_2
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

   function Starts_With
      (S      : String;
       Prefix : String)
       return Boolean
   is (S'Length >= Prefix'Length and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);

   function Is_Repeating
      (S       : String;
       Pattern : String)
       return Boolean
   is
      I : Natural := S'First;
   begin
      while I in S'Range loop
         if not Starts_With (S (I .. S'Last), Pattern) then
            return False;
         end if;
         I := I + Pattern'Length;
      end loop;
      return True;
   end Is_Repeating;

   function Is_Invalid
      (N : Int)
      return Boolean
   is
      S : constant String := To_String (N);
   begin
      for Len in 1 .. S'Length / 2 loop
         if Is_Repeating (S, S (S'First .. S'First + Len - 1)) then
            return True;
         end if;
      end loop;
      return False;
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
end Day2_2;
