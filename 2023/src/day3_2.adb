pragma Ada_2022;
with Advent_IO; use Advent_IO;
with Ada.Containers.Ordered_Maps;

procedure Day3_2 is
   function To_Natural (Ch : Character) return Natural
   is (Character'Pos (Ch) - Character'Pos ('0'));

   function Is_Gear
      (Ch : Character)
      return Boolean
   is (Ch = '*');

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
      subtype Gear_Id is Natural;

      type Span_Type is record
         Y              : Row := Row'First;
         First, Last    : Column := Column'First;
         Num            : Natural := 0;
         Is_Geared      : Boolean := False;
         Gear           : Gear_Id := 0;
         Active         : Boolean := False;
      end record;

      type Gear_Data is record
         Value : Natural := 0;
         Adjacent : Natural := 0;
      end record;

      package Gear_Maps is new Ada.Containers.Ordered_Maps
         (Key_Type     => Gear_Id,
          Element_Type => Gear_Data);
      Ratios : Gear_Maps.Map := Gear_Maps.Empty_Map;
      use Gear_Maps;

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
                        Is_Gear (Schematic (SY, SX))
                     then
                        Span.Is_Geared := True;
                        Span.Gear := (SY * Width + SX);
                        if not Contains (Ratios, Span.Gear) then
                           Insert (Ratios, Span.Gear, (Value => Span.Num, Adjacent => 1));
                        else
                           declare
                              Cur : constant Cursor := Find (Ratios, Span.Gear);
                              G   : Gear_Data := Element (Ratios, Span.Gear);
                           begin
                              G.Adjacent := G.Adjacent + 1;
                              G.Value := G.Value * Span.Num;
                              Replace_Element (Ratios, Cur, G);
                           end;
                        end if;
                     end if;
                  end loop;
               end loop;

               --  String'Write (Error, "Y=");
               --  Put (Error, Span.Y);
               --  String'Write (Error, " First=");
               --  Put (Error, Span.First);
               --  String'Write (Error, " Last=");
               --  Put (Error, Span.Last);
               --  String'Write (Error, " Is_Geared=" & Span.Is_Geared'Image);
               --  String'Write (Error, " Gear=");
               --  Put (Error, Span.Gear);
               --  String'Write (Error, " Num=");
               --  Put (Error, Span.Num);
               --  New_Line (Error);

               Span := (others => <>);
            end if;
         end loop;
      end loop;

      for R of Ratios loop
         if R.Adjacent = 2 then
            Sum := Sum + R.Value;
         end if;
      end loop;
   end;

   Put (Sum);
end Day3_2;
