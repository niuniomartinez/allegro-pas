UNIT Math3D;
(*<Linear 3D algebra.

  This unit defines an alternative 3D vector.  It is borrowed from the engine I
  made for my 2nd PGD Challenge contest entry adapted to Allegro. *)
(*
  Copyright (c) 2012, 2015 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

INTERFACE

  TYPE
  (* Vector object. *)
    TVector3D = OBJECT
    PUBLIC
    (* X component of vector. *)
      X,
    (* Y component of vector. *)
      Y,
    (* Z component of vector. *)
      Z: SINGLE;

    (* Calculates length of vector. @seealso(Length2) @seealso(Distance) *)
      FUNCTION Length: SINGLE; INLINE;
    (* Calculates the square of length of vector.

      It's slightly faster than @code(Length), and for comparision purposes
      it doesn't matter, as lenghts are proportional to its squares.

      For example, this code:
    @longcode(#
       IF Distance < Vector.Length THEN ...
    #)
      Can be replaced by this faster one:
    @longcode(#
       IF (Distance * Distance) < Vector.Length2 THEN ...
    #)
      @seealso(Length)
    *)
      FUNCTION Length2: SINGLE; INLINE;

    (* Normalizes the vector. *)
      PROCEDURE Normalize; INLINE;
    (* Adds the given vector. *)
      PROCEDURE Add (CONST V: TVector3D); INLINE;
    (* Substracts the given vector. *)
      PROCEDURE Sub (CONST V: TVector3D); INLINE;
    (* Multiplies vector with scalar). *)
      PROCEDURE Multiply (CONST S: SINGLE); INLINE;
    (* Divides vector by scalar). *)
      PROCEDURE Divide (CONST S: SINGLE); INLINE;
    (* Scales vector.  This is, multiplies each component with given vector
      component. *)
      PROCEDURE Scale (CONST V: TVector3D); INLINE;
    END;

  (* List of vectors. *)
    TListVector3D = ARRAY OF TVector3D;

  CONST
  (* Value used to compare float values. *)
    FLOAT_THRESHOLD = 0.000001;



(* Returns a vector created from coordinates.  Useful if you need to use a
   pocedure or function that spects a vector but you have only the
   coordinates. *)
  FUNCTION DoVector (CONST aX, aY, aZ: SINGLE): TVector3D; INLINE;

(* Addition operator. *)
  OPERATOR + (CONST V1, V2: TVector3D) Vr: TVector3D; INLINE;

(* Substraction operator. *)
  OPERATOR - (CONST V1, V2: TVector3D) Vr: TVector3D; INLINE;



(* Calculates cross-product of given vectors. *)
  FUNCTION CrossProduct (CONST V1, V2: TVector3D): TVector3D; INLINE;

(* Calculates dot-product of given vectors. *)
  FUNCTION DotProduct (CONST V1, V2: TVector3D): SINGLE; INLINE;

(* Calculates the normal of the plane defined by the given points. *)
  FUNCTION PlaneNormal (CONST p1, p2, p3: TVector3D): TVector3D; INLINE;

(* Calculates distace between points.
  @seealso(Distance2) @seealso(TVector3D.Length) *)
  FUNCTION Distance (CONST P1, P2: TVector3D): SINGLE; INLINE;

(* Calculates square distance between points.

  It's slightly faster than @code(Distance), and for comparision purposes it
  doesn't matter, as distances are proportional to its squares.
  @seealso(Distance) @seealso(TVector3D.Length2) *)
  FUNCTION Distance2 (CONST P1, P2: TVector3D): SINGLE; INLINE;

IMPLEMENTATION

  USES
    sysutils;

(* Returns a vector created from coordinates. *)
  FUNCTION DoVector (CONST aX, aY, aZ: SINGLE): TVector3D;
  BEGIN
    RESULT.X := aX; RESULT.Y := aY; RESULT.Z := aZ;
  END;



(* Addition operator. *)
  OPERATOR + (CONST V1, V2: TVector3D) Vr: TVector3D;
  BEGIN
    Vr.X := V1.X + V2.X;
    Vr.Y := V1.Y + V2.Y;
    Vr.Z := V1.Z + V2.Z
  END;



(* Substraction operator. *)
  OPERATOR - (CONST V1, V2: TVector3D) Vr: TVector3D;
  BEGIN
    Vr.X := V1.X - V2.X;
    Vr.Y := V1.Y - V2.Y;
    Vr.Z := V1.Z - V2.Z
  END;



(* Calculates cross-product of given vectors. *)
  FUNCTION CrossProduct (CONST V1, V2: TVector3D): TVector3D;
  BEGIN
    RESULT.X := v1.Y * v2.Z - v1.Z * v2.Y;
    RESULT.Y := v1.Z * v2.X - v1.X * v2.Z;
    RESULT.Z := v1.X * v2.Y - v1.Y * v2.X
  END;



(* Calculates dot-product of given vectors. *)
  FUNCTION DotProduct (CONST V1, V2: TVector3D): SINGLE;
  BEGIN
    RESULT := (V1.X * V2.X) + (V1.Y * V2.Y) + (V1.Z * V2.Z);
  END;



(* Calculates the normal of the plane defined by the given vertices. *)
  FUNCTION PlaneNormal (CONST p1, p2, p3: TVector3D): TVector3D;
  BEGIN
    RESULT := CrossProduct (p2 - p1, p3 - p2); RESULT.Normalize
  END;



(* Calculates distace between points. @seealso(Distance2) *)
  FUNCTION Distance (CONST P1, P2: TVector3D): SINGLE;
  BEGIN
    RESULT := Sqrt (Sqr (P1.X - P2.X) + Sqr (P1.Y - P2.Y) + Sqr (P1.Z - P2.Z))
  END;



(* Calculates square distance between points. @seealso(Distance) *)
  FUNCTION Distance2 (CONST P1, P2: TVector3D): SINGLE;
  BEGIN
    RESULT := Sqr (P1.X - P2.X) + Sqr (P1.Y - P2.Y) + Sqr (P1.Z - P2.Z)
  END;



(*
 * TVector3D
 ****************************************************************************)

(* Calculates length of vector. *)
  FUNCTION TVector3D.Length: SINGLE;
  BEGIN
    RESULT := Sqrt (Sqr (SELF.X) + Sqr (SELF.Y) + Sqr (SELF.Z))
  END;



(* Calculates the square of length of vector. *)
  FUNCTION TVector3D.Length2: SINGLE;
  BEGIN
    RESULT := Sqr (SELF.X) + Sqr (SELF.Y) + Sqr (SELF.Z);
  END;



(* Normalizes the vector. *)
  PROCEDURE TVector3D.Normalize;
  VAR
    aLength: SINGLE;
  BEGIN
    aLength := SELF.Length;
    IF aLength <> 0 THEN
      SELF.Divide (aLength)
      {
    ELSE
      RAISE Exception.Create ('Zero length vector when normalizing vector!');
      }
  END;



(* Adds vector. *)
  PROCEDURE TVector3D.Add (CONST V: TVector3D);
  BEGIN
    SELF.X := SELF.X + V.X;
    SELF.Y := SELF.Y + V.Y;
    SELF.Z := SELF.Z + V.Z
  END;



(* Substracts vector. *)
  PROCEDURE TVector3D.Sub (CONST V: TVector3D);
  BEGIN
    SELF.X := SELF.X - V.X;
    SELF.Y := SELF.Y - V.Y;
    SELF.Z := SELF.Z - V.Z
  END;



(* Multiplies vector. *)
  PROCEDURE TVector3D.Multiply (CONST S: SINGLE);
  BEGIN
    SELF.X := SELF.X * S;
    SELF.Y := SELF.Y * S;
    SELF.Z := SELF.Z * S
  END;



(* Divides vector. *)
  PROCEDURE TVector3D.Divide (CONST S: SINGLE);
  BEGIN
    IF S = 0 THEN
      RAISE Exception.Create ('Trying to divide vector by zero!');
    SELF.X := SELF.X / S;
    SELF.Y := SELF.Y / S;
    SELF.Z := SELF.Z / S
  END;



(* Scales vector. *)
  PROCEDURE TVector3D.Scale (CONST V: TVector3D);
  BEGIN
    SELF.X := SELF.X * V.X;
    SELF.Y := SELF.Y * V.Y;
    SELF.Z := SELF.Z * V.Z
  END;

END.
