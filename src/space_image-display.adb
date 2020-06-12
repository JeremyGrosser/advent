with Terminal_Interface.Curses;
use Terminal_Interface.Curses;

package body Space_Image.Display is
    Black_Color       : constant Color_Pair := 1;
    White_Color       : constant Color_Pair := 2;
    Transparent_Color : constant Color_Pair := 3;
    Error_Color       : constant Color_Pair := 4;
    Visibility        : Cursor_Visibility := Invisible;

    procedure Initialize is
    begin
        Init_Screen;
        Set_Cbreak_Mode (True);
        Set_Echo_Mode (False);
        Set_NL_Mode (True);
        Set_Cursor_Visibility (Visibility);

        Start_Color;
        Init_Pair (Pair => Black_Color, Fore => Black, Back => Black);
        Init_Pair (Pair => White_Color, Fore => White, Back => White);
        Init_Pair (Pair => Transparent_Color, Fore => Magenta, Back => Magenta);
        Init_Pair (Pair => Error_Color, Fore => Red, Back => Red);
        
        Erase;
    end Initialize;

    procedure Show (Image : in SIF_Image) is
        AC : Attributed_Character := (
            Ch    => 'X',
            Color => Color_Pair (2),
            Attr  => (others => False));
    begin
        --Set_Flush_On_Interrupt_Mode (Win, False);
        --Set_KeyPad_Mode (Win, True);

        for X in Image'Range (1) loop
            for Y in Image'Range (2) loop
                Move_Cursor
                    (Line   => Line_Position (Y),
                     Column => Column_Position (X));
                case Image (X, Y) is
                    when '0'     => AC.Color := Black_Color;
                    when '1'     => AC.Color := White_Color;
                    when '2'     => AC.Color := Transparent_Color;
                    when others  => AC.Color := Error_Color;
                end case;
                Add (Standard_Window, AC);
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
