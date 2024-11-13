with Ada.Text_IO;

package body Debug is
   Enabled : Boolean := False;

   procedure Enable is
   begin
      Enabled := True;
   end Enable;

   procedure Log
      (Item     : String;
       New_Line : Boolean := True)
   is
   begin
      if Enabled then
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Item);
         if New_Line then
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         end if;
      end if;
   end Log;

   procedure Log
      (Item     : Integer;
       New_Line : Boolean := True)
   is
      package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   begin
      if Enabled then
         Int_IO.Put (Ada.Text_IO.Standard_Error, Item, Width => 0);
         if New_Line then
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         end if;
      end if;
   end Log;
end Debug;
