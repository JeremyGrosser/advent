pragma Style_Checks ("M120");
with Ada.Text_IO;
with AnsiAda;

package body Advent.Output is
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   package LLI_IO is new Ada.Text_IO.Integer_IO (Long_Long_Integer);
   Log_Enabled : Boolean := False;

   procedure Put
      (Item     : Integer;
       New_Line : Boolean := True)
   is
   begin
      Int_IO.Put (Item, Width => 0);
      if New_Line then
         Ada.Text_IO.New_Line;
      end if;
   end Put;

   procedure Put_Long
      (Item     : Long_Long_Integer;
       New_Line : Boolean := True)
   is
   begin
      LLI_IO.Put (Item, Width => 0);
      if New_Line then
         Ada.Text_IO.New_Line;
      end if;
   end Put_Long;

   procedure Put
      (Item     : String;
       New_Line : Boolean := True)
   is
   begin
      Ada.Text_IO.Put (Item);
      if New_Line then
         Ada.Text_IO.New_Line;
      end if;
   end Put;

   procedure Start_Log is
   begin
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, AnsiAda.Foreground (AnsiAda.Light_Cyan));
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, AnsiAda.Style (AnsiAda.Bright));
   end Start_Log;

   procedure Stop_Log is
   begin
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, AnsiAda.Reset);
   end Stop_Log;

   procedure Log
      (Item     : String;
       New_Line : Boolean := True)
   is
   begin
      if Log_Enabled then
         Start_Log;
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Item);
         if New_Line then
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         end if;
         Stop_Log;
      end if;
   end Log;

   procedure Log
      (Item     : Integer;
       New_Line : Boolean := True)
   is
   begin
      if Log_Enabled then
         Start_Log;
         Int_IO.Put (Ada.Text_IO.Standard_Error, Item, Width => 0);
         if New_Line then
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         end if;
         Stop_Log;
      end if;
   end Log;

   procedure Enable_Log is
   begin
      Log_Enabled := True;
   end Enable_Log;
end Advent.Output;
