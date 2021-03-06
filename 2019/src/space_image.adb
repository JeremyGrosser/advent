package body Space_Image is
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

    function Checksum (Image : in SIF_Image) return Natural is
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
    end Checksum;
    
    procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out SIF_Image) is
         Pixel : SIF_Color;
    begin
        for Y in Item'Range (2) loop
            for X in Item'Range (1) loop
                SIF_Color'Read (Stream, Pixel);
                Item (X, Y) := Pixel;
            end loop;
        end loop;
    end Read;

    procedure Read_File
        (File   : in Ada.Streams.Stream_IO.File_Type;
         Layers : out Image_Vector.Vector) is
        use Ada.Streams;
        Stream : Stream_IO.Stream_Access;
        Image  : SIF_Image;
    begin
        Stream := Stream_IO.Stream (File);
        loop
            SIF_Image'Read (Stream, Image);
            Layers.Append (Image);
            exit when Stream_IO.End_Of_File (File);
        end loop;
    end Read_File;
end Space_Image;
