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

    PROPERTY x: AL_FIXED READ fx WRITE fx;
    PROPERTY y: AL_FIXED READ fy WRITE fY;
    PROPERTY z: AL_FIXED READ fz WRITE fz;
  END;



(* A simple cube. *)
  TCube = CLASS
  PRIVATE
    fPosition, fAngle: TVector;
    fSize: AL_FIXED;
    fDrawmode: LONGINT;
    fTexture: AL_BITMAPptr;
    fTexWidth, fTexHeight: AL_FIXED;
  PUBLIC
  (* Creates the cube. *)
    CONSTRUCTOR Create (px, py, pz, aSize: AL_FIXED; aTexture: AL_BITMAPptr);
  (* Destroys the cube. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Draws the cube. *)
    PROCEDURE Draw (aBitmap: AL_BITMAPptr; aMatrix: AL_MATRIXptr);

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
  END;



IMPLEMENTATION

(***********
 * TVector *
 ***********)
  CONSTRUCTOR TVector.Create (ax, ay, az: AL_FIXED);
  BEGIN
    fx := ax;
    fy := ay;
    fz := az;
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
(* Precalculated color for vertex. *)
  VertexColor, GrayColor: ARRAY [0..7] OF LONGINT;



(* Constructor. *)
  CONSTRUCTOR TCube.Create (px, py, pz, aSize: AL_FIXED; aTexture: AL_BITMAPptr);

    FUNCTION RandomValue: BYTE;
    BEGIN
      RandomValue := (Random (16) * 16) - 1;
    END;

  VAR
    Cnt, Color: LONGINT;
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
  { Pre-calculate colors.  It does this here to be sure the correct color depth
    was selected. }
    FOR Cnt := 0 TO 7 DO
    BEGIN
      VertexColor[Cnt] := al_makecol (RandomValue, RandomValue, RandomValue);
      Color := 127 + (128 DIV (Cnt + 1));
      GrayColor[Cnt] := al_makecol (Color, Color, Color);
    END;
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
    Cnt, v1, v2, v3, v4: LONGINT;
    Matrix: AL_MATRIX;
    Vertex: ARRAY [0..7] OF AL_V3D;
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
    { Calculate the color. }
      IF (fDrawmode < AL_POLYTYPE_ATEX)
      AND (fDrawmode <> AL_POLYTYPE_GCOL)
      THEN
	Vertex[Cnt].c := VertexColor[Cnt]
      ELSE
	Vertex[Cnt].c := GrayColor[Cnt];
    END;
  { faces of the cube. }
    FOR Cnt := 0 TO 5 DO
    BEGIN
      v1 := Cnt * 4;
      v2 := VertexIndex[v1 + 1];
      v3 := VertexIndex[v1 + 2];
      v4 := VertexIndex[v1 + 3];
      v1 := VertexIndex[v1];
    { If faces aren't transparente, then avoid faces behind the cube. }
      IF (fDrawmode < AL_POLYTYPE_ATEX_TRANS)
      AND (al_polygon_z_normal (@Vertex[v1], @Vertex[v2], @Vertex[v3]) < 0)
      THEN
	CONTINUE;
    { Texturization. }
      IF (fDrawmode >= AL_POLYTYPE_ATEX) AND (fTexture <> NIL) THEN
      BEGIN
        Vertex[v1].u :=         0; Vertex[v3].v :=          0;
        Vertex[v2].u :=         0; Vertex[v4].v := fTexHeight;
        Vertex[v3].u := fTexWidth; Vertex[v1].v := fTexHeight;
        Vertex[v4].u := fTexWidth; Vertex[v2].v :=          0;
      END;
    { Draw it. }
      al_quad3d (aBitmap, fDrawmode, fTexture,
		 @Vertex[v1], @Vertex[v2], @Vertex[v3], @Vertex[v4]);
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
