with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Streams;
with Ada.Text_IO;
with Space_Image;
with Space_Image.Display;

procedure Advent8 is
    use Ada.Streams;
    use Space_Image;

    File    : Stream_IO.File_Type;
    Layers  : Image_Vector.Vector := Image_Vector.Empty_Vector;
    Image   : SIF_Image;
    NZ      : Natural := 0;
    Best    : Natural := Natural'Last;
begin
    Stream_IO.Open (File, Stream_IO.In_File, "input/advent8");
    Read_File (File, Layers);
    Stream_IO.Close (File);

    -- Part 1
    for Layer of Layers loop
        NZ := Num_Zeroes (Layer);
        if NZ < Best then
            Image := Layer;
            Best := NZ;
        end if;
    end loop;

    Ada.Text_IO.Put_Line (Checksum (Image)'Image);

    -- Part 2
    Image := Transparent_Image;
    for Layer of Layers loop
        for Y in Image'Range (2) loop
            for X in Image'Range (1) loop
                if Image (X, Y) = '2' then
                    Image (X, Y) := Layer (X, Y);
                end if;
            end loop;
        end loop;
    end loop;

    Space_Image.Display.Initialize;
    Space_Image.Display.Show (Image);
    Space_Image.Display.Wait_For_Command;

end Advent8;
