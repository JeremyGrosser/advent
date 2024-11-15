pragma Ada_2022;
with Ada.Integer_Text_IO;
with Advent_IO; use Advent_IO;

procedure Day3_1 is
   function To_Natural (Ch : Character) return Natural
   is (Character'Pos (Ch) - Character'Pos ('0'));

   function Is_Symbol
      (Ch : Character)
      return Boolean
   is (Ch not in '0' .. '9' and then Ch /= '.');

   Width  : Positive;
   Height : Positive;
   Sum    : Natural := 0;
begin
   Width := Read_Until (CRLF)'Length;
   Height := Input_Length / Width - 1;
   Seek (Offset => 0, From => Seek_Start);

   declare
      subtype Row is Natural range 1 .. Height;
      subtype Column is Natural range 1 .. Width;

      type Span_Type is record
         Y              : Row := Row'First;
         First, Last    : Column := Column'First;
         Num            : Natural := 0;
         Is_Part_Number : Boolean := False;
         Active         : Boolean := False;
      end record;

      Schematic : array (Row, Column) of Character;
      Span : Span_Type;
   begin
      for Y in Row'Range loop
         for X in Column'Range loop
            Get (Schematic (Y, X));
         end loop;
         Seek (1); --  skip LF
      end loop;

      for Y in Row'Range loop
         for X in Column'Range loop
            if Schematic (Y, X) in '0' .. '9' then
               if not Span.Active then
                  Span.Active := True;
                  Span.Y := Y;
                  Span.First := X;
                  Span.Last := X;
                  Span.Num := To_Natural (Schematic (Y, X));
               else
                  Span.Last := X;
                  Span.Num := (Span.Num * 10) + To_Natural (Schematic (Y, X));
               end if;
            end if;

            if Span.Active and then (X = Column'Last or else Schematic (Y, X) not in '0' .. '9') then
               for SY in Span.Y - 1 .. Span.Y + 1 loop
                  for SX in Span.First - 1 .. Span.Last + 1 loop
                     if SY in Row'Range and then
                        SX in Column'Range and then
                        Is_Symbol (Schematic (SY, SX))
                     then
                        Span.Is_Part_Number := True;
                     end if;
                  end loop;
               end loop;

               --  String'Write (Error, "Y=");
               --  Put (Error, Span.Y);
               --  String'Write (Error, " First=");
               --  Put (Error, Span.First);
               --  String'Write (Error, " Last=");
               --  Put (Error, Span.Last);
               --  String'Write (Error, " Is_Part_Number=" & Span.Is_Part_Number'Image);
               --  New_Line (Error);

               if Span.Is_Part_Number then
                  Sum := Sum + Span.Num;
               end if;
               Span := (others => <>);
            end if;
         end loop;
      end loop;
   end;

   Ada.Integer_Text_IO.Put (Sum, Width => 0);
end Day3_1;
