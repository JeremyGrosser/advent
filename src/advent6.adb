with Ada.Streams.Stream_IO;
with Ada.Characters.Handling;
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Real_Arrays;

procedure Advent6 is
    Data_Format_Error   : exception;
    Input_Changed_Error : exception;

    subtype Orbital_Body is String (1 .. 1);
    subtype Orbital_Index is Positive;
    subtype Real is Float;

    package Real_IO is new Ada.Text_IO.Float_IO (Real);

    package Orbital_Vector is new Ada.Containers.Vectors
        (Element_Type => Orbital_Body,
         Index_Type   => Orbital_Index);

    package Orbital_Matrix is new Ada.Numerics.Generic_Real_Arrays (Real);
    use Orbital_Matrix;

    procedure Next_Edge
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         A      : out Orbital_Body;
         B      : out Orbital_Body) is
        use Ada.Characters.Handling;
        Delim : Character;
    begin
        Orbital_Body'Read (Stream, A);

        Character'Read (Stream, Delim);
        if Delim /= ')' then
            raise Data_Format_Error with "Unknown delimiter: " & Delim;
        end if;

        Orbital_Body'Read (Stream, B);

        Character'Read (Stream, Delim);
        if Is_Line_Terminator (Delim) /= True then
            raise Data_Format_Error with "Expected a line terminator, got " & Delim;
        end if;
    end Next_Edge;

    procedure Print (M : in Orbital_Matrix.Real_Matrix) is
        use Ada.Text_IO;
    begin
        for I in M'Range (1)  loop
            for J in M'Range (2) loop
                if M (I, J) = 1.0 then
                    Put ("1 ");
                else
                    Put ("0 ");
                end if;
            end loop;
            Put_Line ("");
        end loop;
    end Print;

    function Degree (M : in Orbital_Matrix.Real_Matrix;
                     I : in Orbital_Index) return Real is
        D : Real := 0.0;
    begin
        for J in M'Range (2) loop
            D := D + M (I, J);
        end loop;
        return D;
    end Degree;

    Input_File : Ada.Streams.Stream_IO.File_Type;
    Input_Stream : Ada.Streams.Stream_IO.Stream_Access;

    A, B : Orbital_Body;
    V : Orbital_Vector.Vector := Orbital_Vector.Empty_Vector;
begin
    Ada.Streams.Stream_IO.Open (Input_File, Ada.Streams.Stream_IO.In_File, "input/advent6-example");
    Input_Stream := Ada.Streams.Stream_IO.Stream (Input_File);

    loop
        exit when Ada.Streams.Stream_IO.End_of_File (Input_File);
        Next_Edge (Input_Stream, A, B);
        if Orbital_Vector.Contains (V, A) = False then
            Orbital_Vector.Append (V, A);
        end if;
        if Orbital_Vector.Contains (V, B) = False then
            Orbital_Vector.Append (V, B);
        end if;
    end loop;

    declare
        N : constant Orbital_Index := Orbital_Index (V.Length);
        subtype Adjacency_Matrix is Orbital_Matrix.Real_Matrix (1 .. N, 1 .. N);

        M : Adjacency_Matrix := (others => (others => 0.0));
        I, J : Orbital_Vector.Extended_Index;
    begin
        Ada.Streams.Stream_IO.Reset (Input_File);
        loop
            exit when Ada.Streams.Stream_IO.End_of_File (Input_File);
            Next_Edge (Input_Stream, A, B);
            I := Orbital_Vector.Find_Index (V, A, Orbital_Index'First);
            J := Orbital_Vector.Find_Index (V, B, Orbital_Index'First);
            if I = Orbital_Vector.No_Index or
               J = Orbital_Vector.No_Index then
                raise Input_Changed_Error;
            end if;
            M (Integer (I), Integer (J)) := 1.0;
        end loop;

        --Print (M);

        Real_IO.Default_Fore := 0;
        Real_IO.Default_Aft := 0;
        Real_IO.Default_Exp := 0;

        for I in M'Range (1) loop
            Ada.Text_IO.Put (V (I) & " ");
            Real_IO.Put (Degree (M, I));
            Ada.Text_IO.Put_Line ("");
        end loop;
    end;

    Ada.Streams.Stream_IO.Close (Input_File);
end Advent6;
