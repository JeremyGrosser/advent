pragma Ada_2022;
with Ada.Containers.Vectors;
with Advent; use Advent;
with Advent.Input;
with Advent.Output;

procedure Day2_1 is

   type Report is array (Positive range <>) of Natural;

   function Read_Report
      return Report
   is
      package Natural_Vectors is new Ada.Containers.Vectors (Positive, Natural);
      use Natural_Vectors;
      Rep : Natural_Vectors.Vector;
   begin
      loop
         Append (Rep, Input.Get_Integer);
         exit when Input.End_Of_Input or else Input.Peek = ASCII.LF;
      end loop;

      declare
         R : Report (1 .. Last_Index (Rep));
      begin
         for I in R'Range loop
            R (I) := Rep (I);
         end loop;
         return R;
      end;
   end Read_Report;

   function Is_Safe
      (Rep : Report)
      return Boolean
   is
      Diff : Integer;
      Inc, Dec, Mag : Natural := 0;
   begin
      for I in Rep'First .. Rep'Last - 1 loop
         Diff := Rep (I) - Rep (I + 1);

         if Diff > 0 then
            Inc := Inc + 1;
         elsif Diff < 0 then
            Dec := Dec + 1;
         end if;

         if abs Diff in 1 .. 3 then
            Mag := Mag + 1;
         end if;
      end loop;

      return (Inc = Rep'Length - 1 or else Dec = Rep'Length - 1) and then Mag = Rep'Length - 1;
   end Is_Safe;

   Sum : Natural := 0;
begin
   while not Input.End_Of_Input loop
      declare
         R : constant Report := Read_Report;
      begin
         if Is_Safe (R) then
            Sum := Sum + 1;
         end if;
      end;
   end loop;
   Output.Put (Sum);
end Day2_1;
