with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Hash;
with Ada.Characters.Latin_1;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Containers;

procedure Advent3 is
    type Position is record
        X : Integer;
        Y : Integer;
    end record;

    type Direction_Type is (Left, Right, Up, Down);

    type Move_Type is record
        Direction : Direction_Type;
        Distance : Natural;
    end record;

    function Equal (A : Position; B : Position) return Boolean is
    begin
        return (A.X = B.X) and (A.Y = B.Y);
    end Equal;

    function Hash (P : Position) return Ada.Containers.Hash_Type is
    begin
        return Ada.Strings.Hash (P.X'Image & P.Y'Image);
    end;

    package Position_Sets is new Ada.Containers.Hashed_Sets
        (Element_Type => Position,
         Hash => Hash,
         Equivalent_Elements => Equal);

    package Position_Vectors is new Ada.Containers.Vectors
        (Index_Type => Natural,
         Element_Type => Position);

    -- Manhattan distance
    function Distance (A : Position; B : Position; Wire : Position_Vectors.Vector) return Natural is
        D : Natural := 0;
    begin
        for I of Wire loop
            D := D + 1;
            if Equal (B, I) then
                return D;
            end if;
        end loop;
        return 0;
        --return abs (A.X - B.X) + abs (A.Y - B.Y);
    end Distance;

    procedure Get_Move (
        File : in File_Type;
        Item : out Move_Type;
        End_of_Line : out Boolean) is
        use Ada.Characters.Latin_1;
        subtype Digit is Character range '0' .. '9';
        C : Character;
    begin
        Item.Distance := 0;
        End_of_Line := False;
        loop
            exit when End_of_File (File);
            Get_Immediate (File, C);
            case C is
                when Digit =>
                    Item.Distance := 
                            (Item.Distance * 10) +
                            (Digit'Pos (C) -
                             Digit'Pos(Digit'First));
                when 'L' => Item.Direction := Left;
                when 'R' => Item.Direction := Right;
                when 'U' => Item.Direction := Up;
                when 'D' => Item.Direction := Down;
                when LF =>
                    End_of_Line := True;
                    return;
                when others => return;
            end case;
        end loop;
    end Get_Move;


    Origin : constant Position := (X => 0, Y => 0);
    Wires : array (1 .. 2) of Position_Sets.Set;
    WiresV : array (1 .. 2) of Position_Vectors.Vector;
    Wire_Index : Natural := Wires'First;
    Intersects : Position_Sets.Set;
    Current : Position := Origin;
    Nearest, D : Natural := Natural'Last;
    Move : Move_Type;
    EOL : Boolean := False;
begin
    loop
        exit when End_of_File (Standard_Input);
        Get_Move (Standard_Input, Move, EOL);
        for I in 1 .. Move.Distance loop
            case Move.Direction is
                when Left   => Current.X := Current.X - 1;
                when Right  => Current.X := Current.X + 1;
                when Up     => Current.Y := Current.Y + 1;
                when Down   => Current.Y := Current.Y - 1;
            end case;
            Wires (Wire_Index).Include (Current);
            WiresV (Wire_Index).Append (Current);
        end loop;
        if EOL then
            Wire_Index := Wire_Index + 1;
            Current := Origin;
            EOL := False;
        end if;
    end loop;

    Position_Sets.Intersection (Wires (1), Wires (2));
    for I of Wires (1) loop
        --Put_Line (I.X'Image & "," & I.Y'Image);
        D :=  Distance (Origin, I, WiresV (1))
            + Distance (Origin, I, WiresV (2));
        --Put_Line (D'Image);
        if D < Nearest then
            Nearest := D;
        end if;
    end loop;

    Put_Line ("Nearest intersection distance = " & Nearest'Image);
end Advent3;
