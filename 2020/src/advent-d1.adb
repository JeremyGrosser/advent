with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers;

package body Advent.D1 is
   function Part_1 (Filename : in String)
      return Integer
   is
      package Integer_Vectors is new Ada.Containers.Vectors (Positive, Integer);
      use type Ada.Containers.Count_Type;

      Input : File_Type;
      Nums : Integer_Vectors.Vector;
      X, Y : Integer;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         Nums.Append (Integer'Value (Get_Line (Input)));
      end loop;

      Close (Input);

      for A in Nums.Iterate loop
         for B in Nums.Iterate loop
            X := Integer_Vectors.Element (A);
            Y := Integer_Vectors.Element (B);
            if X + Y = 2020 then
               return X * Y;
            end if;
         end loop;
      end loop;
      return 0;
   end Part_1;

   function Part_2 (Filename : in String)
      return Integer
   is
      package Integer_Vectors is new Ada.Containers.Vectors (Positive, Integer);
      use type Ada.Containers.Count_Type;

      Input : File_Type;
      Nums : Integer_Vectors.Vector;
      X, Y, Z : Integer;
   begin
      Open (Input, In_File, Filename);
      loop
         exit when End_Of_File (Input);
         Nums.Append (Integer'Value (Get_Line (Input)));
      end loop;

      Close (Input);

      for A in Nums.Iterate loop
         for B in Nums.Iterate loop
            for C in Nums.Iterate loop
               X := Integer_Vectors.Element (A);
               Y := Integer_Vectors.Element (B);
               Z := Integer_Vectors.Element (C);
               if X + Y + Z = 2020 then
                  return X * Y * Z;
               end if;
            end loop;
         end loop;
      end loop;
      return 0;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "1.1", "input/d1.1-test", 514579);
      Put_Line ("1.1 solution: " & Part_1 ("input/d1")'Image);

      Test (Part_2'Access, "1.2", "input/d1.1-test", 241861950);
      Put_Line ("1.2 solution: " & Part_2 ("input/d1")'Image);
   end Run;
end Advent.D1;
