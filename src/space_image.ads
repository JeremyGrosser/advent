with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Streams;

package Space_Image is
    subtype SIF_Color is Character range '0' .. '9';

    Width  : constant Positive := 25;
    Height : constant Positive := 6;
    type SIF_Image is array (1 .. Width, 1 .. Height) of SIF_Color
        with Default_Component_Value => '0';

    procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
         Item   : out SIF_Image);
    
	for SIF_Image'Read use Read;

    package Image_Vector is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => SIF_Image);
    use type Image_Vector.Vector;

    procedure Read_File
        (File   : in Ada.Streams.Stream_IO.File_Type;
         Layers : out Image_Vector.Vector);

    function "+" (Left : in SIF_Color; Right : in SIF_Color) return SIF_Color;
    function Num_Zeroes (Image : in SIF_Image) return Natural;
    function Checksum (Image : in SIF_Image) return Natural;
end Space_Image;
