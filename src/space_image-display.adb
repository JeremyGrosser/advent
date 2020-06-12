with Terminal_Interface.Curses;
use Terminal_Interface.Curses;

package Space_Image.Display is
    procedure Initialize is
    begin
        Init_Screen;
        Set_Cbreak_Mode (True);
        Set_Echo_Mode (False);
        Set_NL_Mode (True);
    end Initialize;

    procedure Show (Image : in SIF_Image) is
    begin
        --Set_Flush_On_Interrupt_Mode (Win, False);
        --Set_KeyPad_Mode (Win, True);

        for X in Image'Range (1) loop
            for Y in Image'Range (2) loop
                Move_Cursor
                    (Line   => Line_Position (Y),
                     Column => Column_Position (X));
                Add (Ch => Character (Image (X, Y)));
            end loop;
        end loop;

        Refresh;
    end Show;

    procedure Wait_For_Command is
    begin
        loop
            exit when Key_Name (Get_Keystroke) = "q";
        end loop;

        End_Screen;
        Curses_Free_All;
    end Wait_For_Command;
end Space_Image.Display;
