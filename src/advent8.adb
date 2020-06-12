with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Streams;
with Ada.Text_IO;
with Space_Image;

procedure Advent8 is
    use Ada.Streams;
    use Space_Image;

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

    Ada.Text_IO.Put_Line (Checksum (Image)'Image);

    Stream_IO.Close (Input_File);
end Advent8;
