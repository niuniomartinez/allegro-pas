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

   Defines a simple 3D cube used by 3D examples.

   It isn't a very efficient piece of code, but it does show the stuff in
   action.  It is left to the reader as an exercise to design a proper model
   structure and rendering pipeline:  after all, the best way to do that sort
   of stuff varies hugely from one game to another.

   by Ñuño Martínez <niunio(at)users.sourceforge.net>
   Some parts are inspired or ported from an example of the Allegro library
   by Shawn Hargreaves. *)

{$IFDEF FPC}
{ Free Pascal. }
  {$MODE OBJFPC}
{$ENDIF}

INTERFACE

USES
  allegro, alfixed, al3d;



TYPE
(* A simple vector. *)
  TVector = CLASS
  PRIVATE
    fx, fy, fz: AL_FIXED;
  PUBLIC
    CONSTRUCTOR Create (ax, ay, az: AL_FIXED);
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
  TCube = CLASS
  PRIVATE
    fPosition, fAngle: TVector;
    fSize: AL_FIXED;
  PROTECTED
    fDrawmode: LONGINT;
    fTexture: AL_BITMAPptr;
    fTexWidth, fTexHeight: AL_FIXED;
    fFaces: ARRAY [1..6] OF TFace;
    fVisibleFaces: INTEGER; (* Number of faces to draw. *)
  PUBLIC
  (* Creates the cube. *)
    CONSTRUCTOR Create (px, py, pz, aSize: AL_FIXED; aTexture: AL_BITMAPptr);
	VIRTUAL;
  (* Destroys the cube. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Draws the cube. *)
    PROCEDURE Draw (aBitmap: AL_BITMAPptr; aMatrix: AL_MATRIXptr); VIRTUAL;

  (* Cube position. *)
    PROPERTY Pos: TVector READ fPosition WRITE fPosition;
  (* Cube angle. *)
    PROPERTY Ang: TVector READ fAngle WRITE fAngle;
  (* Cube size. *)
    PROPERTY Size: AL_FIXED READ fSize WRITE fSize;
  (* Polygon draw mode. *)
    PROPERTY DrawMode: LONGINT READ fDrawmode WRITE fDrawmode;
  (* Cube texture.  Note it isn't destroyed by the cube. *)
    PROPERTY Texture: AL_BITMAPptr READ fTexture WRITE fTexture;
  PRIVATE
  (* Draws the faces. *)
    PROCEDURE DrawFaces (aBitmap: AL_BITMAPptr);
  END;



CONST
(* A custom polytype.  Used to draw a wired cube. *)
  POLYTYPE_WIRED = -1;



IMPLEMENTATION

VAR
(* Due to a bug, the renderer fails when drawing some textured poligons when
   they are near to the "perpendicular".  This value allows a workaround.
   See the Draw method and the documentation of unit al3d. *)
  MAX_NORMAL: AL_FIXED;



(***********
 * TVector *
 ***********)
  CONSTRUCTOR TVector.Create (ax, ay, az: AL_FIXED);
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



(*********
 * TCube *
 *********)

VAR
(* Coordinates for each vertex of the cube. *)
  PointCoordinates: ARRAY [0..7] OF TVector;
(* Dictionary to know the vertex for each face. *)
  VertexIndex: ARRAY [0..23] OF INTEGER = (
  { Each line is a face. }
    1, 2, 3, 0,
    7, 6, 5, 4,
    4, 5, 1, 0,
    6, 7, 3, 2,
    3, 7, 4, 0,
    5, 6, 2, 1
  );



(* Constructor. *)
  CONSTRUCTOR TCube.Create (px, py, pz, aSize: AL_FIXED; aTexture: AL_BITMAPptr);

    FUNCTION RandomValue: BYTE;
    BEGIN
      RandomValue := (Random (16) * 16) - 1;
    END;

  BEGIN
    fPosition := TVector.Create (px, py, pz);
    fAngle := TVector.Create (0, 0, 0);
    fSize := aSize;
    fTexture := aTexture;
    IF fTexture <> NIL THEN
    BEGIN
      fDrawmode  := AL_POLYTYPE_PTEX;
      fTexWidth  := al_itofix (fTexture^.w);
      fTexHeight := al_itofix (fTexture^.h);
    END
    ELSE
      fDrawmode := AL_POLYTYPE_FLAT;
  { The "bad normal" value depends on the size of the polygon and this depends
    on the screen definition.  The values proposed here where found by
    try-and-error process. }
    IF AL_SCREEN_W < 800 THEN
      MAX_NORMAL := al_itofix (200)
    ELSE
      MAX_NORMAL := al_itofix (500)
  END;



(* Destructor. *)
  DESTRUCTOR TCube.Destroy;
  BEGIN
    fPosition.Free;
    fAngle.Free;
    INHERITED Destroy;
  END;



(* Draws the cube.

   This is quite inefficient and doesn't draw the masked and transparent modes
   in the right order. *)
  PROCEDURE TCube.Draw (aBitmap: AL_BITMAPptr; aMatrix: AL_MATRIXptr);
  VAR
    Cnt, v1, v2, v3, v4, Face: LONGINT;
    z1, z2: AL_FIXED;
    Matrix: AL_MATRIX;
    Vertex: ARRAY [0..7] OF AL_V3D;
    Normal: LONGINT;

  (* Helper procedure to define a face. *)
    PROCEDURE SetFace (Ndx: INTEGER); INLINE;
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
	  Point[1].c := al_palette_color^[Cnt+1];
	AL_POLYTYPE_GCOL:
	  BEGIN
	    Point[1].c := al_palette_color^[$D0];
	    Point[2].c := al_palette_color^[$80];
	    Point[3].c := al_palette_color^[$B0];
	    Point[4].c := al_palette_color^[$FF];
	  END;
	AL_POLYTYPE_GRGB:
	  BEGIN
	    Point[1].c := $000000;
	    Point[2].c := $7F0000;
	    Point[3].c := $FF0000;
	    Point[4].c := $7F0000;
	  END;
	  ELSE BEGIN
	    Point[1].c := al_palette_color^[$D0];
	    Point[2].c := al_palette_color^[$80];
	    Point[3].c := al_palette_color^[$B0];
	    Point[4].c := al_palette_color^[$FF];
	  END;
	END;
      END;
    END;

  BEGIN
  { Create the transformation matrix. }
    al_get_transformation_matrix (@Matrix, fSize, fAngle.x, fAngle.y, fAngle.z,
				  fPosition.x, fPosition.y, fPosition.z);
    al_matrix_mul (@Matrix, aMatrix, @Matrix);
  { vertices of the cube }
    FOR Cnt := 0 TO 7 DO
    BEGIN
    { "Move" and project each vertex. }
      al_apply_matrix (@Matrix,
	PointCoordinates[Cnt].x, PointCoordinates[Cnt].y, PointCoordinates[Cnt].z,
	@Vertex[Cnt].x, @Vertex[Cnt].y, @Vertex[Cnt].z);
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
    { Calculate the z normal to know if face should be drawn.
      Read documentation of al_polygon_z_normal for more info. }
      Normal := al_polygon_z_normal (@Vertex[v1], @Vertex[v2], @Vertex[v3]);
      IF ((AL_POLYTYPE_FLAT <= fDrawmode) AND (fDrawmode <= AL_POLYTYPE_PTEX))
      OR (fDrawmode = AL_POLYTYPE_ATEX_LIT)
      OR (fDrawmode = AL_POLYTYPE_PTEX_LIT)
      THEN BEGIN
      { Only faces with positive normals are visible. }
	IF (Normal <= MAX_NORMAL) THEN
	  CONTINUE;
      END
      ELSE
      { Transparent and masked faces should be drawn allways, because we can
	see through them, but due a weird bug some faces near to the
	"perpendicular" fails when draw.  See the MAS_NORMAL comment abobe. }
	IF (MAX_NORMAL >= Normal) AND (Normal >= (-MAX_NORMAL)) THEN
	  CONTINUE;
    { Insert the face in the face list ordered by Z. }
      INC (fVisibleFaces);
    { If list is empty. }
      IF fVisibleFaces = 1 THEN
	SetFace (1)
      ELSE IF fVisibleFaces = 2 THEN
      BEGIN
	z1 := Vertex[v1].z + Vertex[v2].z + Vertex[v3].z + Vertex[v4].z;
	WITH fFaces[1] DO
	  z2 := Point[1].z + Point[2].z + Point[3].z + Point[4].z;
	IF z1 < z2 THEN
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
	  IF z1 < z2 THEN
	    fFaces[Face + 1] := fFaces[Face]
	  ELSE
	    BREAK;
	  DEC (FACE);
	UNTIL (Face = 0);
	SetFace (Face + 1);
      END;
    END;
  { Draw it. }
    SELF.DrawFaces (aBitmap);
  END;



(* Draws a simple face.  It's virtual so it can be changed. *)
  PROCEDURE TCube.DrawFaces (aBitmap: AL_BITMAPptr);
  VAR
    Cnt: INTEGER;
  BEGIN
    FOR Cnt := 1 TO fVisibleFaces DO
    WITH SELF.fFaces[Cnt] DO
    BEGIN
      IF fDrawmode <> POLYTYPE_WIRED THEN
      BEGIN
	al_quad3d (aBitmap, fDrawmode, fTexture, @Point[1], @Point[2], @Point[3], @Point[4])
      END
      ELSE BEGIN
	al_line (aBitmap, Point[1].x SHR 16, Point[1].y SHR 16,
			Point[2].x SHR 16, Point[2].y SHR 16,
			al_palette_color^[255]);
	al_line (aBitmap, Point[2].x SHR 16, Point[2].y SHR 16,
			Point[3].x SHR 16, Point[3].y SHR 16,
			al_palette_color^[255]);
	al_line (aBitmap, Point[3].x SHR 16, Point[3].y SHR 16,
			Point[4].x SHR 16, Point[4].y SHR 16,
			al_palette_color^[255]);
	al_line (aBitmap, Point[4].x SHR 16, Point[4].y SHR 16,
			Point[1].x SHR 16, Point[1].y SHR 16,
			al_palette_color^[255]);
      END;
    END;
  END;



CONST
  One = 1 SHL 16; { See al_itofix documentation. }
VAR
  Cnt: INTEGER;
INITIALIZATION
{ Initialize cube coordinates. }
  PointCoordinates[0] := TVector.Create (-One, -One, -One);
  PointCoordinates[1] := TVector.Create (-One,  One, -One);
  PointCoordinates[2] := TVector.Create ( One,  One, -One);
  PointCoordinates[3] := TVector.Create ( One, -One, -One);
  PointCoordinates[4] := TVector.Create (-One, -One,  One);
  PointCoordinates[5] := TVector.Create (-One,  One,  One);
  PointCoordinates[6] := TVector.Create ( One,  One,  One);
  PointCoordinates[7] := TVector.Create ( One, -One,  One);
FINALIZATION
{ Destroy objects. }
  FOR Cnt := 0 TO 7 DO PointCoordinates[Cnt].Free;
END.
