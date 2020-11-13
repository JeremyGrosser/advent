with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package body Solutions.Day_3 is
   type Fabric_Array is array (Natural range <>, Natural range <>) of Natural;

   function Find
      (S : String;
       C : Character)
       return Natural
   is
   begin
      for I in S'Range loop
         if S (I) = C then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Split
      (S         : String;
       Delimiter : Character;
       Skip      : Natural)
       return String
   is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      if Skip > 0 then
         for I in 1 .. Skip loop
            First := Find (S (First .. Last), Delimiter) + 1;
         end loop;
      end if;
      Last := Find (S (First .. Last), Delimiter);
      if Last = 0 then
         Last := S'Last;
      else
         Last := Last - 1;
      end if;
      return S (First .. Last);
   end Split;

   function Strip
      (S : String;
       C : Character)
       return String
   is
   begin
      if S (S'First) = C then
         return Strip (S (S'First + 1 .. S'Last), C);
      elsif S (S'Last) = C then
         return Strip (S (S'First .. S'Last - 1), C);
      else
         return S;
      end if;
   end Strip;

   procedure Print (FA : Fabric_Array) is
   begin
      for Y in FA'Range (2) loop
         for X in FA'Range (1) loop
            if FA (X, Y) = 0 then
               Put (" .");
            else
               Put (FA (X, Y)'Image);
            end if;
         end loop;
         Put_Line ("");
      end loop;
      Put_Line ("");
   end Print;

   function Part_1
      (Filename    : String;
       Fabric_Size : Positive)
      return Integer
   is
      Fabric     : Fabric_Array (0 .. Fabric_Size, 0 .. Fabric_Size) := (others => (others => 0));
      Count      : Natural := 0;
      Input_File : File_Type;
   begin
      Open (Input_File, In_File, Filename);

      loop
         exit when End_Of_File (Input_File);
         declare
            Line : constant String := Get_Line (Input_File);
         begin
            if Line (Line'First) /= '#' then
               raise Syntax_Error with "Line does not begin with '#'";
            end if;

            declare
               Claim_Id : constant Positive := Positive'Value (Split (Line (2 .. Line'Last), ' ', 0));
               Coord    : constant String   := Strip (Split (Line, ' ', 2), ':');
               X1       : constant Natural  := Natural'Value (Split (Coord, ',', 0));
               Y1       : constant Natural  := Natural'Value (Split (Coord, ',', 1));
               Size     : constant String   := Split (Line, ' ', 3);
               X2       : constant Natural  := X1 + Natural'Value (Split (Size, 'x', 0)) - 1;
               Y2       : constant Natural  := Y1 + Natural'Value (Split (Size, 'x', 1)) - 1;
            begin
               for X in X1 .. X2 loop
                  for Y in Y1 .. Y2 loop
                     Fabric (X, Y) := Fabric (X, Y) + 1;
                     --Fabric (X, Y) := Claim_Id;
                  end loop;
               end loop;
            end;
         end;
      end loop;

      Close (Input_File);

      --Print (Fabric);

      for X in Fabric'Range (1) loop
         for Y in Fabric'Range (2) loop
            if Fabric (X, Y) >= 2 then
               Count := Count + 1;
            end if;
         end loop;
      end loop;


      return Count;
   end Part_1;

   procedure Run is
      Result_1 : Integer;
   begin
      Result_1 := Part_1 ("input/day3.1a", 8);
      if Result_1 /= 4 then
         Put_Line ("day3.1a failed: got " & Result_1'Image & ", expected 4");
         return;
      end if;

      Result_1 := Part_1 ("input/day3", 1024);
      Put_Line ("Solution 3.1: " & Result_1'Image);
   end Run;
end Solutions.Day_3;
