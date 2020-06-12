with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Streams;
with Ada.Text_IO;
with Space_Image;

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

    for Layer of Layers loop
        NZ := Num_Zeroes (Layer);
        if NZ < Best then
            Image := Layer;
            Best := NZ;
        end if;
    end loop;

    Ada.Text_IO.Put_Line (Checksum (Image)'Image);

    Stream_IO.Close (File);
end Advent8;
