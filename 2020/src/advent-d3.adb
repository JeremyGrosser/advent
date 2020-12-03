with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D3 is
   type Map_Value is (Open, Tree);
   type Map_Type is array (Positive range <>, Positive range <>) of Map_Value;

   Invalid_Input : exception;

   function Num_Lines (File : in File_Type)
      return Natural
   is
      Count : Natural := 0;
   begin
      loop
         exit when End_Of_File (File);
         Skip_Line (File);
         Count := Count + 1;
      end loop;
      return Count;
   end Num_Lines;

   function Read_Map (Filename : in String)
      return Map_Type
   is
      Input : File_Type;
      Width, Height : Natural;
   begin
      Open (Input, In_File, Filename);
      Width := Get_Line (Input)'Length;
      Reset (Input);
      Height := Num_Lines (Input);
      Reset (Input);

      declare
         Ch   : Character;
         Map  : Map_Type (1 .. Width, 1 .. Height);
         X, Y : Positive := 1;
      begin
         loop
            exit when End_Of_File (Input);
            Get_Immediate (Input, Ch);
            case Ch is
               when ASCII.LF =>
                  X := 1;
                  Y := Y + 1;
               when '.' =>
                  Map (X, Y) := Open;
                  X := X + 1;
               when '#' =>
                  Map (X, Y) := Tree;
                  X := X + 1;
               when others =>
                  raise Invalid_Input with "Unknown character in map: " & Ch;
            end case;
         end loop;
         Close (Input);
         return Map;
      end;
   end Read_Map;

   function Check_Slope
      (Filename : in String;
       Right    : in Positive;
       Down     : in Positive)
      return Natural
   is
      M      : constant Map_Type := Read_Map (Filename);
      Width  : constant Positive := M'Last (1);
      Height : constant Positive := M'Last (2);
      X, Y   : Positive := 1;
      Count  : Natural := 0;
   begin
      loop
         X := (X + Right);
         if X > Width then
            X := (X mod Width);
         end if;

         Y := Y + Down;
         exit when Y > Height;

         if M (X, Y) = Tree then
            Count := Count + 1;
         end if;
      end loop;

      return Count;
   end Check_Slope;

   function Part_1
      (Filename : in String)
      return Integer
   is
   begin
      return Check_Slope (Filename, 3, 1);
   end Part_1;

   function Part_2
      (Filename : in String)
      return Long_Integer
   is
      N : Long_Integer := 1;
   begin
      N := N * Long_Integer (Check_Slope (Filename, 1, 1));
      N := N * Long_Integer (Check_Slope (Filename, 3, 1));
      N := N * Long_Integer (Check_Slope (Filename, 5, 1));
      N := N * Long_Integer (Check_Slope (Filename, 7, 1));
      N := N * Long_Integer (Check_Slope (Filename, 1, 2));
      return N;
   end Part_2;

   procedure Run is
      Result : Long_Integer;
   begin
      Test (Part_1'Access, "3.1", "input/d3.1-test", 7);
      Put_Line ("3.1 solution: " & Part_1 ("input/d3")'Image);

      --Test (Part_2'Access, "3.2", "input/d3.1-test", 336);
      Result := Part_2 ("input/d3.1-test");
      if Result /= 336 then
         Put_Line ("test failed: " & Result'Image);
         return;
      end if;
      Put_Line ("3.2 solution: " & Part_2 ("input/d3")'Image);
   end Run;
end Advent.D3;
