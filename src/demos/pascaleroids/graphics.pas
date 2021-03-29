unit Graphics;
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

interface

  uses
    Allegro5, al5primitives;

  const
  (* Max points in polygon. *)
    MAX_POINTS = 10;

  type
  (* A polygon.

     Note that this object assumes that polygon is centered at {0, 0}, and it
     is a closed polygon. *)
    TPolygon = record
    private
      fNPoints: Integer;
      fPoints: array [0..MAX_POINTS + 1] of ALLEGRO_VERTEX;
      fFill: Boolean;

      function GetColor: ALLEGRO_COLOR; inline;
      procedure SetColor (const aColor: ALLEGRO_COLOR); inline;
    public
    (* Resets the polygon (this is, zero points). *)
      procedure Reset;
    (* Adds a point to the polygon. *)
      procedure AddPoint (const aX, aY: Single);
    (* Set a list of vertices. *)
      procedure SetVertices (const aVx, aVy: array of Integer);
    (* Draw polygon. *)
      procedure Draw;

    (* Number of points of polygon. *)
      property NumPoints: Integer read fNPoints;
    (* Tells if polygon is filled. *)
      property Fill: Boolean read fFill write fFill;
    (* Polygon color. *)
      property Color: ALLEGRO_COLOR read GetColor write SetColor;
    end;

  var
  (* Identity matrix. *)
    IdentityMatrix: ALLEGRO_TRANSFORM;
  (* Colors. *)
    Black, White,
    Yelow, Green,
    LightBlue, Cyan, Magenta: ALLEGRO_COLOR;

(* Initializes graphics system.  This doesn't creates the display. *)
  procedure Initialize;

implementation

  uses
    sysutils;

(* Initializes graphics system.  This doesn't creates the display. *)
  procedure Initialize;
  begin
    al_init_primitives_addon;

    Black := al_map_rgb (  0,   0,   0);
    White := al_map_rgb (255, 255, 255);

    Yelow     := al_map_rgb (255, 255,   0);
    Green     := al_map_rgb (  0, 255,   0);
    LightBlue := al_map_rgb (150, 150, 233);
    Cyan      := al_map_rgb (  0, 255, 255);
    Magenta   := al_map_rgb (255,   0, 255);

    al_identity_transform (IdentityMatrix);
  end;



(*
 * TPolygon
 ***************************************************************************)

   function TPolygon.GetColor: ALLEGRO_COLOR;
   begin
     Result := fPoints[0].color
   end;



   procedure TPolygon.SetColor (const aColor: ALLEGRO_COLOR);
   var
     Ndx: Integer;
   begin
     for Ndx := Low (fPoints) to High (fPoints) do
       fPoints[Ndx].color := aColor
   end;



(* Reset. *)
  procedure TPolygon.Reset;
  var
    Ndx: Integer;
  begin
    fNPoints := 0; Self.AddPoint (0, 0);
    for Ndx := Low (fPoints) to High (fPoints) do
    begin
      fPoints[Ndx].z := 0;
      fPoints[Ndx].u := 0;
      fPoints[Ndx].v := 0
    end
  end;



(* Adds a point to the polygon. *)
  procedure TPolygon.AddPoint (const aX, aY: Single);
  begin
    if fNPoints > MAX_POINTS then
      RAISE Exception.CreateFmt ('Exceeded polygon size (%d)', [MAX_POINTS]);
    fPoints[fNPoints].x := aX; fPoints[fNPoints].y := aY;
    Inc (fNPoints);
  { Close the loop to correct triangle fan. }
    if fNPoints > 1 then fPoints[fNPoints] := fPoints[1]
  end;



(* Sets vertices. *)
  procedure TPolygon.SetVertices (const aVx, aVy: array of Integer);
  var
    Ndx: Integer;
  begin
    if High (aVx) > High (fPoints) then
      RAISE Exception.CreateFmt
        ('Can''t create polygon of %d points...', [High (aVx)]);
    Self.Reset;
    for Ndx := LOW(aVx) to HIGH(aVx) do Self.AddPoint (aVx[Ndx], aVy[Ndx])
  end;



(* Draw poly. *)
  procedure TPolygon.Draw;
  begin
    if Self.fFill then
      al_draw_prim (Self.fPoints, Nil, Nil, 0, fNPoints + 1, ALLEGRO_PRIM_TRIANGLE_FAN)
    else
      al_draw_prim (Self.fPoints, Nil, Nil, 1, fNPoints, ALLEGRO_PRIM_LINE_LOOP)
  end;

end.

