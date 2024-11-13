with Ada.Text_IO;

package body Advent.Output is
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   procedure Put
      (Item : Integer)
   is
   begin
      Int_IO.Put (Item, Width => 0);
      Ada.Text_IO.New_Line;
   end Put;
end Advent.Output;
