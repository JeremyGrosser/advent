with Advent_IO; use Advent_IO;
with Advent_IO.Generic_Numbers;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure D3_2 is
   package Unsigned_IO is new Advent_IO.Generic_Numbers
      (Number => Unsigned_32);
   use Unsigned_IO;

   package Unsigned_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Unsigned_32);
   use Unsigned_Vectors;

   function Get_Word
      (Size : out Natural)
       return Unsigned_32
   is
      Line : constant String := Read_Until (Input, Whitespace);
      X    : Unsigned_32;
   begin
      Size := Line'Length;
      X := 0;
      for I in Line'Range loop
         X := X or Shift_Left (Unsigned_32'Value (String'(1 => Line (I))), Line'Last - I);
      end loop;
      return X;
   end Get_Word;

   type Popcount is array (Unsigned_32 range 0 .. 1) of Natural;

   function Get_Popcount
      (V        : Vector;
       Position : Natural)
       return Popcount
   is
      Counts : Popcount := (others => 0);
      Bit    : Unsigned_32;
   begin
      for X of V loop
         Bit := Shift_Right (X, Position) and 1;
         Counts (Bit) := Counts (Bit) + 1;
      end loop;
      return Counts;
   end Get_Popcount;

   No_Answer : exception;

   function Equipment_Rating
      (V      : Vector;
       Size   : Positive;
       Invert : Boolean)
      return Unsigned_32
   is
      PC          : Popcount;
      Most_Common : Unsigned_32;
      A : Vector;
      B : Vector := Copy (V);
   begin
      for Position in reverse 0 .. Size - 1 loop
         A := Copy (B);
         B := Empty_Vector;
         PC := Get_Popcount (A, Position);
         if PC (0) > PC (1) then
            Most_Common := 0;
         else
            Most_Common := 1;
         end if;

         if Invert then
            Most_Common := (not Most_Common) and 1;
         end if;

         for Item of A loop
            if (Shift_Right (Item, Position) and 1) = Most_Common then
               Append (B, Item);
            end if;
         end loop;

         if Natural (Length (B)) = 1 then
            return Last_Element (B);
         end if;
      end loop;

      raise No_Answer;
      return 0;
   end Equipment_Rating;

   Inputs    : Vector := Empty_Vector;
   Word_Size : Positive;
begin
   while not End_Of_Input loop
      Inputs.Append (Get_Word (Word_Size));
   end loop;

   declare
      O2_Generator_Rating : constant Unsigned_32 := Equipment_Rating (Inputs, Word_Size, False);
      CO2_Scrubber_Rating : constant Unsigned_32 := Equipment_Rating (Inputs, Word_Size, True);
   begin
      Put (Output, O2_Generator_Rating * CO2_Scrubber_Rating);
      New_Line (Output);
   end;
end D3_2;
