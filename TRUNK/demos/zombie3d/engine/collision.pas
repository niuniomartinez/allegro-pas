UNIT Collision;
(*<Defines classes to deal with collsions. *)
(*
  Copyright (c) 2015 Guillermo Martínez J.

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

  USES
    Math3D;

  TYPE
  (* Base class for collision objects. *)
    TCustomCollisionObject = CLASS (TObject)
    PUBLIC
    (* Collision with ray.
      @param(Orig Origin of ray.) @param(Dir Direction vector of ray.)
      @return(On collision, the distance from the ray origin to the collision
	point. A negative number otherwise.)
      *)
      FUNCTION WithRay (CONST Orig, Dir: TVector3D): SINGLE;
        VIRTUAL; ABSTRACT;
    (* Check collision with ray and calculates the collision point.
      @param(Orig Origin of ray.) @param(Dir Direction vector of ray.)
      @param(cPoint Collision point.)
      @return(@true on collision, @false otherwise.)
     *)
      FUNCTION GetCollisionPointWithRay
        (CONST Orig, Dir: TVector3D; OUT cPoint: TVector3D): BOOLEAN;
        VIRTUAL;

    (* Collision with sphere.
      @param(Center Center of sphere.) @param(Radius Radius of sphere.)
      @param(Point Collision point.)
      @return(@true on collision, @false otherwise.) *)
      FUNCTION WithSphere
        (CONST aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
        VIRTUAL; ABSTRACT;
    (* Checks collision with sphere and updates sphere position.
      @param(Center Center of sphere.  On collision, function will update this
	parameter to a new one.)
      @param(Radius Radius of sphere.)
      @return(@true on collision, @false otherwise.) *)
      FUNCTION GetCollisionSpherePosition
        (VAR aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
        VIRTUAL; ABSTRACT;
    END;



  (* Define a collision sphere. *)
    TCollisionSphere = CLASS (TCustomCollisionObject)
    PRIVATE
      fCenter: TVector3D;
      fRadius: SINGLE;

      PROCEDURE SetCenter (CONST P: TVector3D); INLINE;

    PUBLIC
    (* Sets sphere properties. *)
      PROCEDURE Define (CONST aX, aY, aZ, aR: SINGLE); INLINE;

    (* Collision with ray.
      @param(Orig Origin of ray.) @param(Dir Direction vector of ray.)
      @return(On collision, the distance from the ray origin to the collision
	point. A negative number otherwise.)
      *)
      FUNCTION WithRay (CONST Orig, Dir: TVector3D): SINGLE;
        OVERRIDE;

    (* Collision with sphere.
      @param(Center Center of sphere.) @param(Radius Radius of sphere.)
      @param(Point Collision point.)
      @return(@true on collision, @false otherwise.)
     *)
      FUNCTION WithSphere
        (CONST aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
        OVERRIDE;
    (* Checks collision with sphere and updates sphere position.
      @param(Center Center of sphere.  On collision, function will update this
	parameter to a new one.)
      @param(Radius Radius of sphere.)
      @return(@true on collision, @false otherwise.) *)
      FUNCTION GetCollisionSpherePosition
        (VAR aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
        OVERRIDE;

    (* Sphere cencer. *)
      PROPERTY Center: TVector3D READ fCenter WRITE SetCenter;
    (* Sphere radius. *)
      PROPERTY Radius: SINGLE READ fRadius WRITE fRadius;
    END;



  (* Define a collision plane. *)
    TCollisionPlane = CLASS (TCustomCollisionObject)
    PRIVATE
      fNormal, fP0: TVector3D;
      fDist: SINGLE;
    PUBLIC
    (* Defines the plane by 3 points. *)
      PROCEDURE Define (CONST p1, p2, p3: TVector3D);

    (* Collision with ray.
      @param(Orig Origin of ray.) @param(Dir Direction vector of ray.)
      @return(On collision, the distance from the ray origin to the collision
	point. A negative number otherwise.)
      *)
      FUNCTION WithRay (CONST Orig, Dir: TVector3D): SINGLE;
        OVERRIDE;

    (* Collision with sphere.
      @param(Center Center of sphere.) @param(Radius Radius of sphere.)
      @param(Point Collision point.)
      @return(@true on collision, @false otherwise.)
     *)
      FUNCTION WithSphere
        (CONST aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
        OVERRIDE;
    (* Checks collision with sphere and updates sphere position.
      @param(Center Center of sphere.  On collision, function will update this
	parameter to a new one.)
      @param(Radius Radius of sphere.)
      @return(@true on collision, @false otherwise.) *)
      FUNCTION GetCollisionSpherePosition
        (VAR aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
        OVERRIDE;

    (* Plane normal. *)
      PROPERTY Normal: TVector3D READ fNormal;
    (* Distance of plane. *)
      PROPERTY Distance: SINGLE READ fDist;
    END;




  (* Define a collision square. *)
    TCollisionSquare = CLASS (TCollisionPlane)
    PRIVATE
      fP1, fP2, fP3, fP4: TVector3D;
      fArea1, fArea2: SINGLE;

      FUNCTION AreaTriangle (CONST p1, p2, p3: TVector3D): SINGLE;
    PUBLIC
    (* Defines the square by four points. *)
      PROCEDURE DefineSquare (CONST p1, p2, p3, p4: TVector3D);

    (* Collision with ray.
      @param(Orig Origin of ray.) @param(Dir Direction vector of ray.)
      @return(On collision, the distance from the ray origin to the collision
	point. A negative number otherwise.)
      *)
      FUNCTION WithRay (CONST Orig, Dir: TVector3D): SINGLE;
        OVERRIDE;
    (* Check collision with ray and calculates the collision point.
      @param(Orig Origin of ray.) @param(Dir Direction vector of ray.)
      @param(cPoint Collision point.)
      @return(@true on collision, @false otherwise.) *)
      FUNCTION GetCollisionPointWithRay
        (CONST Orig, Dir: TVector3D; OUT cPoint: TVector3D): BOOLEAN;
        OVERRIDE;
    END;

IMPLEMENTATION

(*
 * TCustomCollisionObject
 ****************************************************************************)

(* Check collision with ray and calculates the collision point. *)
  FUNCTION TCustomCollisionObject.GetCollisionPointWithRay
    (CONST Orig, Dir: TVector3D; OUT cPoint: TVector3D): BOOLEAN;
  VAR
    Dist: SINGLE;
  BEGIN
    Dist := SELF.WithRay (Orig, Dir);
    IF Dist >= 0 THEN
    BEGIN
      cPoint := Dir; cPoint.Multiply (Dist); cPoint.Add (Orig);
      RESULT := TRUE
    END
    ELSE
      RESULT := FALSE
  END;



(*
 * TCollisionSphere
 ****************************************************************************)

  PROCEDURE TCollisionSphere.SetCenter (CONST P: TVector3D);
  BEGIN
    fCenter := P
  END;



(* Set properties. *)
  PROCEDURE TCollisionSphere.Define (CONST aX, aY, aZ, aR: SINGLE);
  BEGIN
    fCenter := DoVector (aX, aY, aZ);
    fRadius := aR
  END;



(* Collision with ray. *)
  FUNCTION TCollisionSphere.WithRay (CONST Orig, Dir: TVector3D): SINGLE;
  VAR
    vTmp: TVector3D;
  { Literals from "axx + bx + c = 0" }
    b, c: SINGLE;
  BEGIN
    vTmp := Orig - fCenter; { Pre-calculation tables are good. }

    b := 2 * DotProduct (Dir, vTmp);
    c := Sqr (vTmp.x) + Sqr (vTmp.y) + Sqr (vTmp.x) - Sqr (fRadius);
    RESULT := b * b - 4 * c;
    IF RESULT >= 0 THEN RESULT := (-b + Sqr (RESULT)) / 2
  END;



(* Collision with sphere. *)
  FUNCTION TCollisionSphere.WithSphere
    (CONST aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
  BEGIN
    RESULT := Distance2 (fCenter, aCenter) <= Sqr (fRadius + aRadius)
  END;



(* Collision with sphere, updating position. *)
  FUNCTION TCollisionSphere.GetCollisionSpherePosition
    (VAR aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
  VAR
    Dist, nRadius: SINGLE;
    vTmp: TVector3D;
  BEGIN
    vTmp := aCenter - fCenter;
    Dist := vTmp.Length2;
    nRadius := aRadius + fRadius;
    RESULT := Dist <= Sqr (nRadius);
    IF RESULT THEN
    BEGIN
      Dist := Sqrt (Dist);
      vTmp.Divide (Dist); { This normalizes. }
      vTmp.Multiply (Dist - nRadius);
      aCenter := aCenter + vTmp;
    END

  { I think this method is optimised to the maximun.
    Any way, here you have an alternate version:

    Dist := Distance2 (fCenter, aCenter);
    nRadius := fRadius + aRadius;
    RESULT := Dist <= Sqr (nRadius);
    IF RESULT THEN
    BEGIN
      vTmp := aCenter - fCenter;
      vTmp.Normalize;
      vTmp.Multiply (Sqrt (Dist) - nRadius);
      aCenter := aCenter + vTmp;
    END
  }
  END;



(*
 * TCollisionPlane
 ****************************************************************************)

(* Defines the plane by four points. *)
  PROCEDURE TCollisionPlane.Define (CONST p1, p2, p3: TVector3D);
  BEGIN
    fNormal := PlaneNormal (p1, p2, p3);
    fDist := DotProduct (fNormal, p1);
    fP0 := p1
  END;



(* Collision with ray. *)
  FUNCTION TCollisionPlane.WithRay (CONST Orig, Dir: TVector3D): SINGLE;
  VAR
    Divisor: SINGLE;
  BEGIN
    Divisor := DotProduct (Dir, fNormal);
    IF Divisor = 0 THEN EXIT (-1); { Paralel ray. }
    RESULT := (fP0.X * fNormal.X + fP0.Y * fNormal.Y + fP0.Z * fNormal.Z
	     - fNormal.X * Orig.X - fNormal.Y * Orig.Y - fNormal.Z * Orig.Z
	     ) / Divisor
  END;



(* Collision with sphere. *)
  FUNCTION TCollisionPlane.WithSphere
    (CONST aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
  VAR
    Dist1, Dist2: SINGLE;
    Tmp, Direction: TVector3D;
  BEGIN
    Dist2 := 0;
  { Opposite plane. }
    Direction := fNormal;
    fNormal.Multiply (-1);
    Dist1 := SELF.WithRay (aCenter, Direction);
  { Normal plane. }
    Tmp := Direction;
    Direction := fNormal; fNormal := Tmp;
    Dist2 := SELF.WithRay (aCenter, Direction);
  { Check. }
    IF (Dist1 > aRadius) OR (Dist2 > aRadius) THEN
      RESULT := FALSE
    ELSE
      RESULT := TRUE
  END;



(* Collision with sphere, updating position. *)
  FUNCTION TCollisionPlane.GetCollisionSpherePosition
    (VAR aCenter: TVector3D; CONST aRadius: SINGLE): BOOLEAN;
  VAR
    Dist1, Dist2: SINGLE;
    Tmp, Direction: TVector3D;
  BEGIN
    Dist2 := 0;
  { Opposite plane. }
    Direction := fNormal;
    fNormal.Multiply (-1);
    Dist1 := SELF.WithRay (aCenter, Direction);
  { Normal plane. }
    Tmp := Direction;
    Direction := fNormal; fNormal := Tmp;
    Dist2 := SELF.WithRay (aCenter, Direction);
  { Check. }
    IF (Dist1 > aRadius) OR (Dist2 > aRadius) THEN EXIT (FALSE);
    RESULT := TRUE;
  { New sphere position. }
    IF Dist1 > 0 THEN
    BEGIN
      aCenter.X := aCenter.X - fNormal.X * (aRadius - Dist1);
      aCenter.Y := aCenter.Y - fNormal.Y * (aRadius - Dist1);
      aCenter.Z := aCenter.Z - fNormal.Z * (aRadius - Dist1);
    END
    ELSE BEGIN
      aCenter.X := aCenter.X + fNormal.X * (aRadius - Dist2);
      aCenter.Y := aCenter.Y + fNormal.Y * (aRadius - Dist2);
      aCenter.Z := aCenter.Z + fNormal.Z * (aRadius - Dist2);
    END
  END;



(*
 * TCollisionSquare
 ****************************************************************************)

  FUNCTION TCollisionSquare.AreaTriangle (CONST p1, p2, p3: TVector3D): SINGLE;
  VAR
    a, b, c, s: SINGLE;
  BEGIN
  { Uses Heron. }
    a := (p2 - p1).Length;
    b := (p3 - p2).Length;
    c := (p3 - p1).Length;
    s := (a + b + c) / 2;
    RESULT := Sqrt (s * (s - a) * (s - b) * (s - c))
  END;



(* Defines the square by four points. *)
  PROCEDURE TCollisionSquare.DefineSquare (CONST p1, p2, p3, p4: TVector3D);
  BEGIN
    SELF.Define (p1, p2, p3);
    fP1 := p1; fP2 := p2; fP3 := p3; fP4 := p4;
    fArea1 := AreaTriangle (p1, p2, p3); fArea2 := AreaTriangle (p3, p4, p1);
  END;



(* Collision with ray. *)
  FUNCTION TCollisionSquare.WithRay (CONST Orig, Dir: TVector3D): SINGLE;
  VAR
    Point: TVector3D;
    nArea: SINGLE;
  BEGIN
    RESULT := INHERITED WithRay (Orig, Dir);
    IF RESULT >= 0 THEN
    BEGIN
    { Collision point. }
      Point.X := Orig.X + RESULT * Dir.X;
      Point.Y := Orig.Y + RESULT * Dir.Y;
      Point.Z := Orig.Z + RESULT * Dir.Z;
    { We use the collision point to create triangles.  Then compare the areas
      of those triangles with the triangles of the quad to know if its inside.
      Draw a quad, a point and then draw the triangles to see why it works. }
      nArea := AreaTriangle (fP1, fP2, Point) + AreaTriangle (fP2, fP3, Point)
	     + AreaTriangle (fP3, fP1, Point);
      IF ABS (nArea - fArea1) >= FLOAT_THRESHOLD THEN
      BEGIN
	nArea := AreaTriangle (fP1, fP3, Point) + AreaTriangle (fP3, fP4, Point)
		+ AreaTriangle (fP4, fP1, Point);
	IF ABS (nArea - fArea2) > FLOAT_THRESHOLD THEN
	{ The point is outside both triangles. }
	  RESULT := - 1
      END
    END
  END;



(* Check collision with ray and calculates the collision point. *)
  FUNCTION TCollisionSquare.GetCollisionPointWithRay
    (CONST Orig, Dir: TVector3D; OUT cPoint: TVector3D): BOOLEAN;
  VAR
    Dist, nArea: SINGLE;
  BEGIN
    Dist := INHERITED WithRay (Orig, Dir);
    IF Dist >= 0 THEN
    BEGIN
    { Collision point. }
      cPoint.X := Orig.X + Dist * Dir.X;
      cPoint.Y := Orig.Y + Dist * Dir.Y;
      cPoint.Z := Orig.Z + Dist * Dir.Z;
    { We use the collision point to create triangles.  Then compare the areas
      of those triangles with the triangles of the quad to know if its inside.
      Draw a quad, a point and then draw the triangles to see why it works. }
      nArea := AreaTriangle (fP1, fP2, cPoint) + AreaTriangle (fP2, fP3, cPoint)
	     + AreaTriangle (fP3, fP1, cPoint);
      IF ABS (nArea - fArea1) >= FLOAT_THRESHOLD THEN
      BEGIN
	nArea := AreaTriangle (fP1, fP3, cPoint) + AreaTriangle (fP3,fP4,cPoint)
		+ AreaTriangle (fP4, fP1, cPoint);
	IF ABS (nArea - fArea2) > FLOAT_THRESHOLD THEN
	{ The point is outside both triangles. }
	  EXIT (FALSE)
      END;
      RESULT := TRUE
    END
    ELSE
      RESULT := FALSE
  END;

END.
