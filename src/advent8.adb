with Terminal_Interface.Curses;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Streams;
with Ada.Text_IO;

procedure Advent8 is
    use Ada.Streams;
    subtype SIF_Color is Character range '0' .. '9';

    Width  : constant Positive := 25;
    Height : constant Positive := 6;
    type SIF_Image is array (1 .. Width, 1 .. Height) of SIF_Color
        with Default_Component_Value => '0';

    package Image_Vector is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => SIF_Image);
    use type Image_Vector.Vector;

    function "+" (Left : in SIF_Color; Right : in SIF_Color) return SIF_Color is
        V : Natural;
        Zero : Natural := SIF_Color'Pos('0');
    begin
        V := SIF_Color'Pos (Left) - Zero;
        V := V + SIF_Color'Pos (Right) - Zero;
        V := V mod 9;
        V := V + Zero;
        return SIF_Color'Val (V);
    end "+";

    procedure Initialize is
        use Terminal_Interface.Curses;
    begin
        Init_Screen;
        Set_Cbreak_Mode (True);
        Set_Echo_Mode (False);
        Set_NL_Mode (True);
    end Initialize;

    procedure Display (Image : in SIF_Image) is
        use Terminal_Interface.Curses;
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
    end Display;

    procedure Wait_For_Command is
        use Terminal_Interface.Curses;
    begin
        loop
            exit when Key_Name (Get_Keystroke) = "q";
        end loop;

        End_Screen;
        Curses_Free_All;
    end Wait_For_Command;

    function Num_Zeroes (Image : in SIF_Image) return Natural is
        Result : Natural := 0;
    begin
        for X in SIF_Image'Range (1) loop
            for Y in SIF_Image'Range (2) loop
                if Image (X, Y) = '0' then
                    Result := Result + 1;
                end if;
            end loop;
        end loop;
        return Result;
    end Num_Zeroes;

    function Elf_Digits (Image : in SIF_Image) return Natural is
        Ones, Twos : Natural := 0;
    begin
        for X in SIF_Image'Range (1) loop
            for Y in SIF_Image'Range (2) loop
                case Image (X, Y) is
                    when '1' => Ones := Ones + 1;
                    when '2' => Twos := Twos + 1;
                    when others => null;
                end case;
            end loop;
        end loop;

        return Ones * Twos;
    end Elf_Digits;

    Input_File   : Stream_IO.File_Type;
    Input_Stream : Stream_IO.Stream_Access;

    Layers : Image_Vector.Vector := Image_Vector.Empty_Vector;
    Image  : SIF_Image;
    Pixel  : SIF_Color;
    NZ     : Natural := 0;
    Best   : Natural := Natural'Last;
begin
    Stream_IO.Open (Input_File, Stream_IO.In_File, "input/advent8");
    Input_Stream := Stream_IO.Stream (Input_File);

    --Initialize;

    loop
        for Y in Image'Range (2) loop
            for X in Image'Range (1) loop
                SIF_Color'Read (Input_Stream, Pixel);
                Image (X, Y) := Pixel;
            end loop;
        end loop;

        Layers.Append (Image);

        exit when Stream_IO.End_of_File (Input_File);
    end loop;

    for Layer of Layers loop
        NZ := Num_Zeroes (Layer);
        if NZ < Best then
            Image := Layer;
            Best := NZ;
        end if;
    end loop;

    Ada.Text_IO.Put_Line (Elf_Digits (Image)'Image);
    --Display (Image);

    Stream_IO.Close (Input_File);

    --Wait_For_Command;
end Advent8;
