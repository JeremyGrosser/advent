with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day2_1 is
   Length, Width, Height : Positive;
   Area : Positive;
   S1, S2, S3 : Positive;
   Smallest : Positive;
   Sum : Natural := 0;
begin
   while not Input.End_Of_Input loop
      Length := Positive'Value (Input.Read_Until ('x'));
      Width  := Positive'Value (Input.Read_Until ('x'));
      Height := Positive'Value (Input.Read_Until (CRLF));
      S1 := Length * Width;
      S2 := Width * Height;
      S3 := Height * Length;
      Smallest := S1;
      if S2 < Smallest then
         Smallest := S2;
      end if;

      if S3 < Smallest then
         Smallest := S3;
      end if;
      Area := (2 * S1) + (2 * S2) + (2 * S3);

      Sum := Sum + Smallest + Area;
   end loop;
   Output.Put (Sum);
end Day2_1;
