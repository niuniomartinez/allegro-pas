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

   by Ñuño Martínez <niunio(at)users.sourceforge.net> *)

{$IFDEF FPC}
{ Free Pascal. }
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
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
  VertexIndex: ARRAY [1..24] OF INTEGER = (
  { Each line is a face. }
    0, 3, 2, 1,
    4, 5, 6, 7,
    0, 1, 5, 4,
    2, 3, 7, 6,
    0, 4, 7, 3,
    1, 2, 6, 5
  );



(* Constructor. *)
  CONSTRUCTOR TCube.Create (px, py, pz, aSize: AL_FIXED; aTexture: AL_BITMAPptr);
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
  END;



(* Destructor. *)
  DESTRUCTOR TCube.Destroy;
  BEGIN
    fPosition.Free;
    fAngle.Free;
    INHERITED Destroy;
  END;


(* Draws the cube. *)
  PROCEDURE TCube.Draw (aBitmap: AL_BITMAPptr; aMatrix: AL_MATRIXptr);

  (* Helper function to get the mid value. *)
    FUNCTION MID (x, y, z: INTEGER): INTEGER;
    VAR
      Tmp: INTEGER;
    BEGIN
      IF y <   z THEN Tmp := y ELSE Tmp := z;
      IF x > Tmp THEN MID := x ELSE MID := Tmp;
    END;

  VAR
    Cnt, Color, v1, v2, v3, v4: LONGINT;
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
      al_apply_matrix (@Matrix,
	PointCoordinates[Cnt].x, PointCoordinates[Cnt].y, PointCoordinates[Cnt].z,
	@Vertex[Cnt].x, @Vertex[Cnt].y, @Vertex[Cnt].z);
      al_persp_project (Vertex[Cnt].x, Vertex[Cnt].y, Vertex[Cnt].z,
			Vertex[Cnt].x, Vertex[Cnt].y);
    END;
  { faces of the cube. }
    FOR Cnt := 0 TO 5 DO
    BEGIN
      v1 := Cnt * 4;
      v2 := VertexIndex[v1 + 1];
      v3 := VertexIndex[v1 + 2];
      v4 := VertexIndex[v1 + 3];
      v1 := VertexIndex[v1];
WriteLn (v1, ', ', v2, ', ', v3, ', ', v4);
    { Calculate the color. }
      Color := MID (128, 255 - (((Vertex[v1].z + Vertex[v2].z) DIV 16) SHR 16), 255);
      Vertex[v1].c := al_makecol (Color, Color, Color);
      Vertex[v2].c := Vertex[v1].c;
      Vertex[v3].c := Vertex[v1].c;
      Vertex[v4].c := Vertex[v1].c;
    { Texturization. }
      IF (fDrawmode >= AL_POLYTYPE_ATEX) AND (fTexture <> NIL) THEN
      BEGIN
        Vertex[v1].u :=         0; Vertex[v1].c :=          0;
        Vertex[v2].u := fTexWidth; Vertex[v2].c :=          0;
        Vertex[v3].u := fTexWidth; Vertex[v3].c := fTexHeight;
        Vertex[v4].u :=         0; Vertex[v4].c := fTexHeight;
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
