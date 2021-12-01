with Advent_IO; use Advent_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

procedure D1 is

   function Get_Integer
      return Integer
   is
      Line : constant String := Read_Until (Whitespace);
   begin
      if Line'Length > 1 then
         return Integer'Value (Line);
      else
         raise End_Error;
      end if;
   end Get_Integer;

   procedure Part_1 is
      Current, Previous : Integer;
      Increases : Natural := 0;
   begin
      Previous := Get_Integer;
      loop
         Current := Get_Integer;
         if Current > Previous then
            Increases := Increases + 1;
         end if;
         Previous := Current;
      end loop;
   exception
      when End_Error =>
         String'Write (Standard_Output, Increases'Image);
         New_Line;
         Flush;
   end Part_1;
begin
   Part_1;
end D1;
