UNIT Graphics;
(*< Grphics stuff. *)
(*
  Copyright (c) 2019 Guillermo Martínez J.

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

{$IFDEF FPC}
  {$modeSwitch advancedRecords}
{$ENDIF}

INTERFACE

  USES
    Allegro5, al5primitives;

  CONST
  (* Max points in polygon. *)
    MAX_POINTS = 10;

  TYPE
  (* A polygon.

     Note that this object assumes that polygon is centered at {0, 0}, and it
     is a closed polygon. *)
    TPolygon = RECORD
    PRIVATE
      fNPoints: INTEGER;
      fPoints: ARRAY [0..MAX_POINTS + 1] OF ALLEGRO_VERTEX;
      fFill: BOOLEAN;

      FUNCTION GetColor: ALLEGRO_COLOR; INLINE;
      PROCEDURE SetColor (CONST aColor: ALLEGRO_COLOR); INLINE;
    PUBLIC
    (* Resets the polygon (this is, zero points). *)
      PROCEDURE Reset;
    (* Adds a point to the polygon. *)
      PROCEDURE AddPoint (CONST aX, aY: SINGLE);
    (* Set a list of vertices. *)
      PROCEDURE SetVertices (CONST aVx, aVy: ARRAY OF INTEGER);
    (* Draw polygon. *)
      PROCEDURE Draw;

    (* Number of points of polygon. *)
      PROPERTY NumPoints: INTEGER READ fNPoints;
    (* Tells if polygon is filled. *)
      PROPERTY Fill: BOOLEAN READ fFill WRITE fFill;
    (* Polygon color. *)
      PROPERTY Color: ALLEGRO_COLOR READ GetColor WRITE SetColor;
    END;

  VAR
  (* Identity matrix. *)
    IdentityMatrix: ALLEGRO_TRANSFORM;
  (* Colors. *)
    Black, White,
    Yelow, Green,
    LightBlue, Cyan, Magenta: ALLEGRO_COLOR;

(* Initializes graphics system.  This doesn't creates the display. *)
  PROCEDURE Initialize;

IMPLEMENTATION

  USES
    sysutils;

(* Initializes graphics system.  This doesn't creates the display. *)
  PROCEDURE Initialize;
  BEGIN
    al_init_primitives_addon;

    Black := al_map_rgb (  0,   0,   0);
    White := al_map_rgb (255, 255, 255);

    Yelow     := al_map_rgb (255, 255,   0);
    Green     := al_map_rgb (  0, 255,   0);
    LightBlue := al_map_rgb (150, 150, 233);
    Cyan      := al_map_rgb (  0, 255, 255);
    Magenta   := al_map_rgb (255,   0, 255);

    al_identity_transform (IdentityMatrix);
  END;



(*
 * TPolygon
 ***************************************************************************)

   FUNCTION TPolygon.GetColor: ALLEGRO_COLOR;
   BEGIN
     RESULT := fPoints[0].color
   END;



   PROCEDURE TPolygon.SetColor (CONST aColor: ALLEGRO_COLOR);
   VAR
     Ndx: INTEGER;
   BEGIN
     FOR Ndx := LOW (fPoints) TO HIGH (fPoints) DO
       fPoints[Ndx].color := aColor
   END;



(* Reset. *)
  PROCEDURE TPolygon.Reset;
  VAR
    Ndx: INTEGER;
  BEGIN
    fNPoints := 0; SELF.AddPoint (0, 0);
    FOR Ndx := LOW (fPoints) TO HIGH (fPoints) DO
    BEGIN
      fPoints[Ndx].z := 0;
      fPoints[Ndx].u := 0;
      fPoints[Ndx].v := 0
    END
  END;



(* Adds a point to the polygon. *)
  PROCEDURE TPolygon.AddPoint (CONST aX, aY: SINGLE);
  BEGIN
    IF fNPoints > MAX_POINTS THEN
      RAISE Exception.CreateFmt ('Exceeded polygon size (%d)', [MAX_POINTS]);
    fPoints[fNPoints].x := aX; fPoints[fNPoints].y := aY;
    INC (fNPoints);
  { Close the loop to correct triangle fan. }
    IF fNPoints > 1 THEN fPoints[fNPoints] := fPoints[1]
  END;



(* Sets vertices. *)
  PROCEDURE TPolygon.SetVertices (CONST aVx, aVy: ARRAY OF INTEGER);
  VAR
    Ndx: INTEGER;
  BEGIN
    IF HIGH (aVx) > HIGH (fPoints) THEN
      RAISE Exception.CreateFmt
        ('Can''t create polygon of %d points...', [HIGH (aVx)]);
    SELF.Reset;
    FOR Ndx := LOW(aVx) TO HIGH(aVx) DO SELF.AddPoint (aVx[Ndx], aVy[Ndx])
  END;



(* Draw poly. *)
  PROCEDURE TPolygon.Draw;
  BEGIN
    IF SELF.fFill THEN
      al_draw_prim (SELF.fPoints, NIL, NIL, 0, fNPoints + 1, ALLEGRO_PRIM_TRIANGLE_FAN)
    ELSE
      al_draw_prim (SELF.fPoints, NIL, NIL, 1, fNPoints, ALLEGRO_PRIM_LINE_LOOP)
  END;

END.

