with Advent_IO; use Advent_IO;
with Advent_IO.Integers; use Advent_IO.Integers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

procedure Day5_1 is
   subtype Crate is Character; --  range 'A' .. 'Z';

   package Crate_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Crate);
   use Crate_Vectors;

   package Stack_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Positive,
       Element_Type => Crate_Vectors.Vector);
   use Stack_Vectors;

   Yard   : Stack_Vectors.Vector := Stack_Vectors.Empty_Vector;
   Column : Positive := 1;
   End_Of_Crates : Boolean := False;

   procedure Add_Crate
      (C : Crate)
   is
   begin
      while Natural (Length (Yard)) < Column loop
         Append (Yard, Crate_Vectors.Empty_Vector);
      end loop;

      Append (Reference (Yard, Column), C);
   end Add_Crate;

   procedure Move_Crate
      (From, To : Positive)
   is
      C : constant Crate := First_Element (Reference (Yard, From));
   begin
      Delete_First (Reference (Yard, From));
      Prepend (Reference (Yard, To), C);
   end Move_Crate;

   procedure Read_Crate is
      Ch : Character;
   begin
      Character'Read (Input, Ch);
      case Ch is
         when ' ' =>
            Character'Read (Input, Ch);
            Character'Read (Input, Ch);
         when '[' =>
            Character'Read (Input, Ch);
            Add_Crate (Ch);
            Character'Read (Input, Ch); --  ignore ']'
         when ASCII.LF =>
            End_Of_Crates := True;
         when others =>
            raise Program_Error with "Unexpected input : " & Character'Pos (Ch)'Image;
      end case;
   end Read_Crate;

   Ch : Character;
begin
   while not End_Of_Crates loop
      Read_Crate;
      Column := Column + 1;
      Character'Read (Input, Ch);
      if Ch = ASCII.LF then
         Column := 1;
      end if;
   end loop;

   --  now read instructions
   declare
      Move, From, To : String (1 .. 5);
      Last        : Natural;
      Move_Count, From_Column, To_Column : Positive := 1;
   begin
      while not End_Of_Input loop
         --  Print_Yard;

         Read_Until (Input, ' ', Move, Last);
         Move_Count := Get (Input, Whitespace);
         Read_Until (Input, ' ', From, Last);
         From_Column := Get (Input, Whitespace);
         Read_Until (Input, ' ', To, Last);
         To_Column := Get (Input, Whitespace);
         Character'Read (Input, Ch); --  LF

         for I in 1 .. Move_Count loop
            Move_Crate (From_Column, To_Column);
         end loop;
      end loop;
   end;

   for Column of Yard loop
      Crate'Write (Output, First_Element (Column));
   end loop;
   New_Line (Output);
end Day5_1;
