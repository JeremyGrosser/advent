with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D12 is
   type Actions is (N, S, E, W, L, R, F);
   type Degrees is mod 360;
   subtype Radians is Float;

   function To_Radians
      (D : Degrees)
      return Radians
   is (Float (D) * (Pi / 180.0));

   function "+"
      (Left  : Degrees;
       Right : Integer)
       return Degrees
   is
      A : constant Integer := Integer (Left) + Right;
   begin
      return Degrees (A mod 360);
   end "+";

   North : constant Degrees := 0;
   East  : constant Degrees := 90;
   South : constant Degrees := 180;
   West  : constant Degrees := 270;

   type State is record
      X, Y   : Integer := 0;
      Facing : Degrees := East;
   end record;

   function Part_1
      (Filename : String)
      return Integer
   is
      Input : File_Type;
      Ship  : State;
   begin
      Open (Input, In_File, Filename);
      while not End_Of_File (Input) loop
         declare
            Line   : constant String := Get_Line (Input);
            Action : constant Actions := Actions'Value (Line (1 .. 1));
            Value  : constant Integer := Integer'Value (Line (2 .. Line'Last));
         begin
            case Action is
               when N => Ship.Y := Ship.Y + Value;
               when S => Ship.Y := Ship.Y - Value;
               when E => Ship.X := Ship.X + Value;
               when W => Ship.X := Ship.X - Value;
               when L => Ship.Facing := Ship.Facing + (-Value);
               when R => Ship.Facing := Ship.Facing + Value;
               when F =>
                  Ship.X := Ship.X + Integer (Float (Value) * Sin (To_Radians (Ship.Facing)));
                  Ship.Y := Ship.Y + Integer (Float (Value) * Cos (To_Radians (Ship.Facing)));
            end case;
            Put (Line);
            Put (" Y=");
            Put (Ship.Y'Image);
            Put (" X=");
            Put (Ship.X'Image);
            Put (" Hdg=");
            Put (Ship.Facing'Image);
            New_Line;
         end;
      end loop;
      Close (Input);

      declare
         Distance : constant Integer := (abs Ship.Y) + (abs Ship.X);
      begin
         Put_Line ("Distance=" & Distance'Image);
         return Distance;
      end;
   end Part_1;

   function Part_2
      (Filename : String)
      return Integer
   is
   begin
      return -1;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "12.1", "input/d12-test", 25);
      Put_Line ("12.1 solution: " & Part_1 ("input/d12")'Image);

      Test (Part_2'Access, "12.2", "input/d1.1-test", 0);
      Put_Line ("12.2 solution: " & Part_2 ("input/d1")'Image);
   end Run;
end Advent.D12;
