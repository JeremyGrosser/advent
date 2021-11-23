with Ada.Text_IO; use Ada.Text_IO;
with Str;

package body Advent.D13 is

   function Parse_Buses
      (S : String)
      return Bus_Vectors.Vector
   is
      use Bus_Vectors;
      V : Vector := Empty_Vector;
      First : Integer := S'First;
   begin
      while First < S'Last loop
         declare
            Bus_String : constant String := Str.Split (S (First .. S'Last), ',', 0);
         begin
            if Bus_String = "" then
               V.Append (Bus_Id'Value (S (First .. S'Last)));
               First := S'Last;
            elsif Bus_String = "x" then
               V.Append (-1);
               First := Bus_String'Last + 2;
            else
               V.Append (Bus_Id'Value (Bus_String));
               First := Bus_String'Last + 2;
            end if;
         end;
      end loop;
      return V;
   end Parse_Buses;

   function Part_1
      (Filename : String)
      return Long_Long_Integer
   is
      Input : File_Type;
   begin
      Open (Input, In_File, Filename);
      declare
         Departure : constant Timestamp := Timestamp'Value (Get_Line (Input));
         Buses     : constant Bus_Vectors.Vector := Parse_Buses (Get_Line (Input));
         Now       : Timestamp := Departure;
         Next_Bus  : Timestamp;
      begin
         Close (Input);
         loop
            for Bus of Buses loop
               if Bus /= -1 and Now mod Bus = 0 then
                  Next_Bus := Bus * (Now / Bus);
                  return Bus * (Next_Bus - Departure);
               end if;
            end loop;
            Now := Now + 1;
         end loop;
      end;
   end Part_1;

   function Is_Valid
      (Buses : Bus_Vectors.Vector;
       I     : Timestamp)
       return Boolean
   is
      T : Timestamp := 1;
   begin
      for Bus of Buses loop
         if Bus /= -1 and (Bus * I) mod T /= 0 then
            return False;
         else
            T := T + 1;
         end if;
      end loop;
      return True;
   end Is_Valid;

   function Part_2
      (Filename : String)
      return Long_Long_Integer
   is
      Input : File_Type;
   begin
      Open (Input, In_File, Filename);
      Skip_Line (Input);
      declare
         Buses : constant Bus_Vectors.Vector := Parse_Buses (Get_Line (Input));
         T     : Timestamp := 1;
      begin
         Close (Input);
         while not Is_Valid (Buses, T) loop
            T := T + 1;
         end loop;
         Put_Line (T'Image);
         return T;
      end;
   end Part_2;

   procedure Run is
   begin
      pragma Assert (Part_1 ("input/d13-test") = 295);
      Put_Line ("13.1 solution: " & Part_1 ("input/d13")'Image);

      pragma Assert (Part_2 ("input/d13-test2") = 3417);
      Put_Line ("13.2 solution: " & Part_2 ("input/d13")'Image);
   end Run;
end Advent.D13;
