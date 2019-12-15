with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings;

package Orbit_Map is
    subtype Orbital is String (1 .. 3);

    procedure Add (A : in Orbital; B : in Orbital);
    function Count (A : Orbital) return Natural;

private
    package Orbital_Vector is new Ada.Containers.Vectors
        (Index_Type     => Positive,
         Element_Type   => Orbital);

    function "=" (Left, Right : Orbital_Vector.Vector) return Boolean;
    
    package Orbital_Hash is new Ada.Containers.Hashed_Maps
        (Key_Type       => Orbital,
         Element_Type   => Orbital_Vector.Vector,
         Hash           => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    Bodies : Orbital_Hash.Map;
end Orbit_Map;
