with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;

package body Advent.D11 is

   function Read_Seats
      (Filename : String)
      return Seats
   is
      Rows, Columns : Positive;
      Input : File_Type;
   begin
      Open (Input, In_File, Filename);
      declare
         Line : constant String := Get_Line (Input);
      begin
         Columns := Line'Length;
         Rows := Positive (Ada.Directories.Size (Filename)) / (Columns + 1);
      end;

      Close (Input);
      Open (Input, In_File, Filename);

      declare
         Result : Seats (1 .. Rows, 1 .. Columns);
         I : Natural := 0;
         Y, X : Positive;
         Ch : Character;
      begin
         while not End_Of_File (Input) loop
            Get (Input, Ch);
            Y := (I / Columns) + 1;
            X := (I mod Columns) + 1;
            I := I + 1;
            case Ch is
               when '.' =>
                  Result (Y, X) := Floor;
               when 'L' =>
                  Result (Y, X) := Empty;
               when '#' =>
                  Result (Y, X) := Occupied;
               when others =>
                  I := I - 1;
            end case;
         end loop;
         Close (Input);
         return Result;
      end;
   end Read_Seats;

   function Adjacent_Seats
      (S    : Seats;
       Y, X : Positive)
       return Seats
   is
      Top    : constant Positive := (if Y > S'First (1) then Y - 1 else Y);
      Left   : constant Positive := (if X > S'First (2) then X - 1 else X);
      Bottom : constant Positive := (if Y < S'Last (1) then Y + 1 else Y);
      Right  : constant Positive := (if X < S'Last (2) then X + 1 else X);
      Result : Seats (Top .. Bottom, Left .. Right);
   begin
      for Row in Top .. Bottom loop
         for Column in Left .. Right loop
            Result (Row, Column) := S (Row, Column);
         end loop;
      end loop;
      return Result;
   end Adjacent_Seats;

   function Occupancy
      (S : Seats)
      return Natural
   is
      N : Natural := 0;
   begin
      for Y in S'Range (1) loop
         for X in S'Range (2) loop
            if S (Y, X) = Occupied then
               N := N + 1;
            end if;
         end loop;
      end loop;
      return N;
   end Occupancy;

   procedure Print
      (S : Seats)
   is
   begin
      for Y in S'Range (1) loop
         for X in S'Range (2) loop
            case S (Y, X) is
               when Occupied => Put ('#');
               when Empty    => Put ('L');
               when Floor    => Put ('.');
            end case;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print;

   function Next_State
      (S    : Seats;
       Y, X : Positive)
       return Seat
   is
      A : constant Seats := Adjacent_Seats (S, Y, X);
   begin
      if S (Y, X) = Empty and Occupancy (A) = 0 then
         return Occupied;
      elsif S (Y, X) = Occupied and Occupancy (A) >= 4 then
         return Empty;
      else
         return S (Y, X);
      end if;
   end Next_State;

   function Part_1
      (Filename : String)
      return Integer
   is
      A : Seats := Read_Seats (Filename);
      B : Seats (A'Range (1), A'Range (2)) := (others => (others => Floor));
   begin
      Put_Line ("Initial");
      Print (A);
      loop
         for Y in A'Range (1) loop
            for X in A'Range (2) loop
               B (Y, X) := Next_State (A, Y, X);
            end loop;
         end loop;
         Print (B);
         New_Line;
         exit when A = B;
         A := B;
      end loop;
      return Occupancy (A);
   end Part_1;

   function Part_2
      (Filename : String)
      return Integer
   is
   begin
      return 0;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "11.1", "input/d11-test", 37);
      Put_Line ("11.1 solution: " & Part_1 ("input/d11")'Image);

      Test (Part_2'Access, "11.2", "input/d11-test", 241861950);
      Put_Line ("11.2 solution: " & Part_2 ("input/d11")'Image);
   end Run;
end Advent.D11;
