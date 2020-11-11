with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

package body Solutions.Day_1 is
   function Part_1 (Filename : String) return Integer is
      Input_File : File_Type;
      Frequency  : Integer := 0;
      X          : Integer;
   begin
      Open (Input_File, In_File, Filename);

      loop
         exit when End_Of_File (Input_File);
         X := Integer'Value (Get_Line (Input_File));
         Frequency := Frequency + X;
      end loop;

      Close (Input_File);

      return Frequency;
   end Part_1;

   function Part_2 (Filename : String) return Integer is
      package Int_Set is new Ada.Containers.Ordered_Sets
         (Element_Type => Integer);

      Input_File : File_Type;
      Frequency  : Integer := 0;
      X          : Integer;
      Seen       : Int_Set.Set := Int_Set.Empty_Set;
   begin
      Seen.Insert (0);

      loop
         Open (Input_File, In_File, Filename);
         loop
            exit when End_Of_File (Input_File);
            X := Integer'Value (Get_Line (Input_File));
            Frequency := Frequency + X;
            if Seen.Contains (Frequency) then
               Close (Input_File);
               return Frequency;
            end if;
            Seen.Insert (Frequency);
         end loop;
         Close (Input_File);
      end loop;

      return 0;
   end Part_2;

   procedure Run is
      Result : Integer;
   begin
      if Part_1 ("input/day1.1a") /= 3 then
         return;
      end if;
      Result := Part_1 ("input/day1");
      Put_Line ("Solution 1.1: " & Result'Image);

      if Part_2 ("input/day1.2a") /= 0 then
         return;
      end if;

      if Part_2 ("input/day1.2b") /= 10 then
         return;
      end if;

      if Part_2 ("input/day1.2c") /= 5 then
         return;
      end if;
      
      if Part_2 ("input/day1.2d") /= 14 then
         return;
      end if;
      Result := Solutions.Day_1.Part_2 ("input/day1");
      Put_Line ("Solution 1.2: " & Result'Image);
   end Run;
end Solutions.Day_1;
