with Advent_IO; use Advent_IO;

procedure D1 is

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
      when End_Of_Input =>
         String'Write (Standard_Output, Increases'Image);
         New_Line;
         Flush;
   end Part_1;

begin
   Part_1;
end D1;
