with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

package body Advent.D12 is
   type Actions is (N, S, E, W, L, R, F);

   function To_Radians
      (D : Degrees)
      return Radians
   is (Float (D) * (Pi / 180.0));

   function To_Degrees
      (R : Radians)
      return Degrees
   is (Degrees (Integer (R * (180.0 / Pi)) mod 360));

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
         end;
      end loop;
      Close (Input);

      return (abs Ship.Y) + (abs Ship.X);
   end Part_1;

   procedure Move_Toward
      (Point  : in out Coordinate;
       To     : Coordinate;
       Count  : Positive)
   is
   begin
      Point :=
         (X => Point.X + To.X * Count,
          Y => Point.Y + To.Y * Count);
   end Move_Toward;

   function Distance_To
      (From, To : Coordinate)
      return Integer
   is (abs (From.X - To.X) + abs (From.Y - To.Y));

   function Part_2
      (Filename : String)
      return Long_Long_Integer
   is
      Input    : File_Type;
      Ship     : Coordinate := (0, 0);
      Waypoint : Coordinate := (10, 1);
      Tmp      : Integer;
   begin
      Open (Input, In_File, Filename);
      while not End_Of_File (Input) loop
         declare
            Line   : constant String := Get_Line (Input);
            Action : constant Actions := Actions'Value (Line (1 .. 1));
            Value  : constant Integer := Integer'Value (Line (2 .. Line'Last));
         begin
            case Action is
               when N => Waypoint.Y := Waypoint.Y + Value;
               when S => Waypoint.Y := Waypoint.Y - Value;
               when E => Waypoint.X := Waypoint.X + Value;
               when W => Waypoint.X := Waypoint.X - Value;
               when R =>
                  Tmp := Waypoint.X;
                  case Value is
                     when 90 =>
                        Waypoint.X := Waypoint.Y;
                        Waypoint.Y := -Tmp;
                     when 180 =>
                        Waypoint.X := -Waypoint.X;
                        Waypoint.Y := -Waypoint.Y;
                     when 270 =>
                        Waypoint.X := -Waypoint.Y;
                        Waypoint.Y := Tmp;
                     when others =>
                        raise No_Answer with "Can only rotate in 90, 180, or 270 degree increments";
                  end case;
               when L =>
                  Tmp := Waypoint.X;
                  case Value is
                     when 90 =>
                        Waypoint.X := -Waypoint.Y;
                        Waypoint.Y := Tmp;
                     when 180 =>
                        Waypoint.X := -Waypoint.X;
                        Waypoint.Y := -Waypoint.Y;
                     when 270 =>
                        Waypoint.X := Waypoint.Y;
                        Waypoint.Y := -Tmp;
                     when others =>
                        raise No_Answer with "Can only rotate in 90, 180, or 270 degree increments";
                  end case;
               when F => Move_Toward (Ship, Waypoint, Value);
            end case;
            Put (Line);
            Put (" Ship=");
            Put (Ship.X'Image);
            Put (Ship.Y'Image);
            Put (" Waypoint=");
            Put (Waypoint.X'Image);
            Put (Waypoint.Y'Image);
            New_Line;
         end;
      end loop;
      Close (Input);

      declare
         Distance : constant Long_Long_Integer := (abs Long_Long_Integer (Ship.Y)) + (abs Long_Long_Integer (Ship.X));
      begin
         Put_Line ("Distance=" & Distance'Image);
         return Distance;
      end;
   end Part_2;

   procedure Run is
   begin
      Test (Part_1'Access, "12.1", "input/d12-test", 25);
      Put_Line ("12.1 solution: " & Part_1 ("input/d12")'Image);

      declare
         Result : constant Long_Long_Integer := Part_2 ("input/d12-test");
      begin
         if Result /= 286 then
            Put_Line ("12.2 test got " & Result'Image & ", expected 286");
            return;
         end if;
      end;

      Put_Line ("12.2 solution: " & Part_2 ("input/d12")'Image);
   end Run;
end Advent.D12;
