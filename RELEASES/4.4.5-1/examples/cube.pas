UNIT cube;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

   Defines a simple 3D cube used by ex3d and exzbuf examples.

   It isn't a very efficient piece of code, but it does show the stuff in
   action.  It is left to the reader as an exercise to design a proper model
   structure and rendering pipeline:  after all, the best way to do that sort
   of stuff varies hugely from one game to another.

   By Guillermo "Ñuño" Martínez
   Some parts are inspired or ported from an example of the Allegro library
   by Shawn Hargreaves.

	See README file for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$MODE OBJFPC}
{$ENDIF}

INTERFACE

  USES
    allegro, alfixed, al3D;



  TYPE
  (* A simple vector. *)
    TVector = OBJECT
    PRIVATE
      fx, fy, fz: AL_FIXED;
    PUBLIC
      CONSTRUCTOR Init (ax, ay, az: AL_FIXED);
    (* Adds the given vector. *)
      PROCEDURE Add (Vector: TVector);

      PROPERTY x: AL_FIXED READ fx WRITE fx;
      PROPERTY y: AL_FIXED READ fy WRITE fY;
      PROPERTY z: AL_FIXED READ fz WRITE fz;
    END;



  (* A face. *)
    TFace = RECORD
      Point: ARRAY [1..4] OF AL_V3D;
    END;



  (* A simple cube. *)
    TCube = CLASS (TObject)
    PRIVATE
      fPosition, fAngle, fRotate: TVector;
      fSize: AL_FIXED;
      fVertexColor: ARRAY [1..4] OF INTEGER;
      fUseZbuff: BOOLEAN;
      fDrawmode: LONGINT;
      fTexture: AL_BITMAPptr;
      fTexWidth, fTexHeight: AL_FIXED;
    PROTECTED
      fFaces: ARRAY [1..6] OF TFace;
      fVisibleFaces: INTEGER; (* Number of faces to draw. *)

    (* Draws the faces. *)
      PROCEDURE DrawFaces (aBitmap: AL_BITMAPptr);
    PUBLIC
    (* Creates the cube. *)
      CONSTRUCTOR Create (px, py, pz, aSize: AL_FIXED; aTexture: AL_BITMAPptr);
	VIRTUAL;
    (* Sets a color for the cube. *)
      PROCEDURE SetColor (aR, aG, aB: INTEGER);
    (* Updates cube. *)
      PROCEDURE Update; VIRTUAL;
    (* Draws the cube. *)
      PROCEDURE Draw (aBitmap: AL_BITMAPptr; aMatrix: AL_MATRIX);

    (* Cube position. *)
      PROPERTY Pos: TVector READ fPosition;
    (* Cube angle. *)
      PROPERTY Ang: TVector READ fAngle;
    (* Cube size. *)
      PROPERTY Size: AL_FIXED READ fSize;
    (* Polygon draw mode. *)
      PROPERTY DrawMode: LONGINT READ fDrawmode WRITE fDrawmode;
    (* Cube texture.  Note it isn't destroyed by the cube. *)
      PROPERTY Texture: AL_BITMAPptr READ fTexture WRITE fTexture;
    (* To use Z-buffer if available.  Don't use it by default. *)
      PROPERTY UseZbuff: BOOLEAN READ fUseZbuff WRITE fUseZbuff;
    END;



  CONST
  (* A custom polytype.  Used to draw a wired cube. *)
    POLYTYPE_WIRED = -1;



IMPLEMENTATION

(* Returns max value. *)
  FUNCTION MyMax (x, y: LONGINT): LONGINT; INLINE;
  BEGIN
    IF x > y THEN RESULT := x ELSE RESULT := y;
  END;



(* Returns min value. *)
  FUNCTION MyMin (x, y: LONGINT): LONGINT; INLINE;
  BEGIN
    IF x < y THEN RESULT := x ELSE RESULT := y;
  END;



(* Returns mid value.  Use only when x < z. *)
  FUNCTION MyClamp (x, y, z: LONGINT): LONGINT; INLINE;
  BEGIN
    RESULT := MyMax (x, MyMin (y, z));
  END;



(*
 * TVector
 *****************************************************************************)

  CONSTRUCTOR TVector.Init (ax, ay, az: AL_FIXED);
  BEGIN
    fx := ax;
    fy := ay;
    fz := az;
  END;



(* Adds the given vector. *)
  PROCEDURE TVector.Add (Vector: TVector);
  BEGIN
    fx := (fx + Vector.fx) AND $00FFFFFF;
    fy := (fy + Vector.fy) AND $00FFFFFF;
    fz := (fz + Vector.fz) AND $00FFFFFF;
  END;



(*
 * TCube
 *****************************************************************************)

  VAR
  (* Coordinates for each vertex of the cube. *)
    PointCoordinates: ARRAY [0..7] OF TVector;
  (* Dictionary to know the vertex for each face. *)
    VertexIndex: ARRAY [0..23] OF INTEGER = (
    { Each line is a face. }
      2, 1, 0, 3,
      4, 5, 6, 7,
      0, 1, 5, 4,
      2, 3, 7, 6,
      4, 7, 3, 0,
      1, 2, 6, 5
    );



(* Draws faces. *)
  PROCEDURE TCube.DrawFaces (aBitmap: AL_BITMAPptr);
  VAR
    fZbuff, Cnt, Clr: INTEGER;
  BEGIN
    IF UseZbuff THEN
      fZbuff := AL_POLYTYPE_ZBUF
    ELSE
      fZbuff := 0;
    FOR Cnt := 1 TO fVisibleFaces DO
    WITH SELF.fFaces[Cnt] DO
    BEGIN
      IF fDrawmode <> POLYTYPE_WIRED THEN
      BEGIN
	al_quad3d (aBitmap, fDrawmode OR fZbuff, fTexture, @Point[1], @Point[2], @Point[3], @Point[4])
      END
      ELSE BEGIN
	Clr := MyClamp (128, 255 - al_fixtoi (Point[1].z + Point[3].z) DIV 16, 255);
	al_line (aBitmap, Point[1].x SHR 16, Point[1].y SHR 16,
			Point[2].x SHR 16, Point[2].y SHR 16,
			al_palette_color^[Clr]);
	al_line (aBitmap, Point[2].x SHR 16, Point[2].y SHR 16,
			Point[3].x SHR 16, Point[3].y SHR 16,
			al_palette_color^[Clr]);
	al_line (aBitmap, Point[3].x SHR 16, Point[3].y SHR 16,
			Point[4].x SHR 16, Point[4].y SHR 16,
			al_palette_color^[Clr]);
	al_line (aBitmap, Point[4].x SHR 16, Point[4].y SHR 16,
			Point[1].x SHR 16, Point[1].y SHR 16,
			al_palette_color^[Clr]);
      END;
    END;
  END;



(* Constructor. *)
  CONSTRUCTOR TCube.Create (px, py, pz, aSize: AL_FIXED; aTexture: AL_BITMAPptr);
  BEGIN
    fPosition.Init (px, py, pz);
    fAngle.Init (0, 0, 0);
    fRotate.Init (al_ftofix ((Random (32) - 16) / 8),
    	          al_ftofix ((Random (32) - 16) / 8),
		  al_ftofix ((Random (32) - 16) / 8));
    fSize := aSize;
    fTexture := aTexture;
    fUseZbuff := FALSE;
    IF fTexture <> NIL THEN
    BEGIN
      fDrawmode  := AL_POLYTYPE_PTEX;
      fTexWidth  := al_itofix (fTexture^.w);
      fTexHeight := al_itofix (fTexture^.h);
    END
    ELSE
      fDrawmode := AL_POLYTYPE_FLAT;
    fVertexColor[1] := $000000;
    fVertexColor[2] := $7F0000;
    fVertexColor[3] := $FF0000;
    fVertexColor[4] := $7F0000;
  END;



(* Sets a color for the cube. *)
  PROCEDURE TCube.SetColor (aR, aG, aB: INTEGER);
  VAR
    tR, tG, tB: INTEGER;
  BEGIN
    tR := aR SHR 1;
    tG := aG SHR 1;
    tB := aB SHR 1;
  { Vertex 1 is still black. }
    fVertexColor[2] := (tR SHL 16) OR (tG SHL 8) OR tB;
    fVertexColor[3] := (aR SHL 16) OR (aG SHL 8) OR aB;
    fVertexColor[4] := fVertexColor[2];
  END;



(* Updates the cube. *)
  PROCEDURE TCube.Update;
  BEGIN
    fAngle.Add (fRotate);
  END;



(* Draws the cube.

   This is very inefficient and doesn't draw the masked and transparent modes
   in the right order. *)
  PROCEDURE TCube.Draw (aBitmap: AL_BITMAPptr; aMatrix: AL_MATRIX);
  VAR
    Cnt, v1, v2, v3, v4, Face: LONGINT;
    z1, z2: AL_FIXED;
    Matrix: AL_MATRIX;
    Vertex: ARRAY [0..7] OF AL_V3D;

  (* Helper procedure to define a face. *)
    PROCEDURE SetFace (Ndx: INTEGER);
    VAR
      Clr: INTEGER;
    BEGIN
      WITH fFaces[Ndx] DO
      BEGIN
      { Vertices. }
	Point[1] := Vertex[v1];
	Point[2] := Vertex[v2];
	Point[3] := Vertex[v3];
	Point[4] := Vertex[v4];
      { Texturization. }
	IF (fDrawmode >= AL_POLYTYPE_ATEX) AND (fTexture <> NIL) THEN
	BEGIN
	  Point[1].u :=         0; Point[1].v := fTexHeight;
	  Point[2].u :=         0; Point[2].v :=          0;
	  Point[3].u := fTexWidth; Point[3].v :=          0;
	  Point[4].u := fTexWidth; Point[4].v := fTexHeight;
	END;
	CASE fDrawmode OF
	AL_POLYTYPE_FLAT:
	  BEGIN
	    Clr := MyClamp (128, 255 - al_fixtoi (Point[1].z + Point[3].z) DIV 16, 255);
	    Point[1].c := al_palette_color^[Clr];
	  END;
	AL_POLYTYPE_GCOL:
	  BEGIN
	    Point[1].c := al_palette_color^[$D0];
	    Point[2].c := al_palette_color^[$80];
	    Point[3].c := al_palette_color^[$B0];
	    Point[4].c := al_palette_color^[$FF];
	  END;
	AL_POLYTYPE_GRGB:
	  BEGIN
	    Point[1].c := fVertexColor[1];
	    Point[2].c := fVertexColor[2];
	    Point[3].c := fVertexColor[3];
	    Point[4].c := fVertexColor[4];
	  END;
	  ELSE BEGIN
	    Point[1].c := MyClamp (0, 255 - al_fixtoi (Point[1].z) DIV 4, 255);
	    Point[2].c := MyClamp (0, 255 - al_fixtoi (Point[2].z) DIV 4, 255);
	    Point[3].c := MyClamp (0, 255 - al_fixtoi (Point[3].z) DIV 4, 255);
	    Point[4].c := MyClamp (0, 255 - al_fixtoi (Point[4].z) DIV 4, 255);
	  END;
	END;
      END;
    END;

  BEGIN
  { Create the transformation matrix. }
    al_get_transformation_matrix (Matrix, fSize, fAngle.x, fAngle.y, fAngle.z,
				  fPosition.x, fPosition.y, fPosition.z);
    al_matrix_mul (Matrix, aMatrix, Matrix);
  { vertices of the cube }
    FOR Cnt := 0 TO 7 DO
    BEGIN
    { "Move" and project each vertex. }
      al_apply_matrix (Matrix,
	PointCoordinates[Cnt].x, PointCoordinates[Cnt].y, PointCoordinates[Cnt].z,
	Vertex[Cnt].x, Vertex[Cnt].y, Vertex[Cnt].z);
      al_persp_project (Vertex[Cnt].x, Vertex[Cnt].y, Vertex[Cnt].z,
			Vertex[Cnt].x, Vertex[Cnt].y);
    END;
  { faces of the cube. }
    fVisibleFaces := 0;
    FOR Cnt := 0 TO 5 DO
    BEGIN
      v1 := Cnt * 4;
      v2 := VertexIndex[v1 + 1];
      v3 := VertexIndex[v1 + 2];
      v4 := VertexIndex[v1 + 3];
      v1 := VertexIndex[v1];
      IF ((AL_POLYTYPE_FLAT <= fDrawmode) AND (fDrawmode <= AL_POLYTYPE_PTEX))
      OR (fDrawmode = AL_POLYTYPE_ATEX_LIT)
      OR (fDrawmode = AL_POLYTYPE_PTEX_LIT)
      THEN BEGIN
      { Only faces with positive normals are visible. }
	IF al_polygon_z_normal_sign (Vertex[v1], Vertex[v2], Vertex[v3]) < 0
        THEN
	  CONTINUE;
      END;
    { Insert the face in the face list. }
      INC (fVisibleFaces);
    { If list is empty. }
      IF fVisibleFaces = 1 THEN
	SetFace (1)
      ELSE IF fVisibleFaces = 2 THEN
      BEGIN
	z1 := Vertex[v1].z + Vertex[v2].z + Vertex[v3].z + Vertex[v4].z;
	WITH fFaces[1] DO
	  z2 := Point[1].z + Point[2].z + Point[3].z + Point[4].z;
	IF z1 > z2 THEN
	BEGIN
	  fFaces[2] := fFaces[1];
	  SetFace (1);
	END
	ELSE
	  SetFace (2);
      END
      ELSE BEGIN
	z1 := Vertex[v1].z + Vertex[v2].z + Vertex[v3].z + Vertex[v4].z;
	Face := fVisibleFaces - 1;
	REPEAT
	  WITH fFaces[Face] DO
	    z2 := Point[1].z + Point[2].z + Point[3].z + Point[4].z;
	  IF z1 > z2 THEN
	    fFaces[Face + 1] := fFaces[Face]
	  ELSE
	    BREAK;
	  DEC (FACE);
	UNTIL (Face = 0);
	SetFace (Face + 1);
      END;
    END;
  { Draw it. }
    SELF.DrawFaces (aBitmap)
  END;



CONST
  One = 1 SHL 16; { See al_itofix documentation. }
INITIALIZATION
{ Initialize cube coordinates. }
  PointCoordinates[0].Init (-One, -One, -One);
  PointCoordinates[1].Init (-One,  One, -One);
  PointCoordinates[2].Init ( One,  One, -One);
  PointCoordinates[3].Init ( One, -One, -One);
  PointCoordinates[4].Init (-One, -One,  One);
  PointCoordinates[5].Init (-One,  One,  One);
  PointCoordinates[6].Init ( One,  One,  One);
  PointCoordinates[7].Init ( One, -One,  One)
FINALIZATION
  ;
END.
