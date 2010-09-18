PROGRAM exspline;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *    This program demonstrates the use of spline curves to create smooth 
 *    paths connecting a number of node points. This can be useful for 
 *    constructing realistic motion and animations.
 *
 *    The technique is to connect the series of guide points p1..p(n) with
 *    spline curves from p1-p2, p2-p3, etc. Each spline must pass though
 *    both of its guide points, so they must be used as the first and fourth
 *    of the spline control points. The fun bit is coming up with sensible
 *    values for the second and third spline control points, such that the
 *    spline segments will have equal gradients where they meet. I came
 *    up with the following solution:
 *
 *    For each guide point p(n), calculate the desired tangent to the curve
 *    at that point. I took this to be the vector p(n-1) -> p(n+1), which 
 *    can easily be calculated with the inverse tangent function, and gives 
 *    decent looking results. One implication of this is that two dummy 
 *    guide points are needed at each end of the curve, which are used in 
 *    the tangent calculations but not connected to the set of splines.
 *
 *    Having got these tangents, it becomes fairly easy to calculate the
 *    spline control points. For a spline between guide points p(a) and
 *    p(b), the second control point should lie along the positive tangent
 *    from p(a), and the third control point should lie along the negative
 *    tangent from p(b). How far they are placed along these tangents 
 *    controls the shape of the curve: I found that applying a 'curviness'
 *    scaling factor to the distance between p(a) and p(b) works well.
 *
 *    One thing to note about splines is that the generated points are
 *    not all equidistant. Instead they tend to bunch up nearer to the
 *    ends of the spline, which means you will need to apply some fudges
 *    to get an object to move at a constant speed. On the other hand,
 *    in situations where the curve has a noticeable change of direction 
 *    at each guide point, the effect can be quite nice because it makes
 *    the object slow down for the curve.
 *
 *	by Ñuño Martínez <>
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  allegro, alfixed, algui,
  sysutils;

TYPE
(* A node. *)
  NODE = RECORD
    x, y: LONGINT;
    tangent: AL_FIXED;
  END;

CONST
  MAX_NODES = 1024;

VAR
  Nodes: ARRAY [0..(MAX_NODES-1)] OF NODE;
  NodeCount: INTEGER;
  Curviness: AL_FIXED;
  ShowTangents, ShowControlPoints: BOOLEAN;



(* Calculates the distance between two nodes. *)
  FUNCTION NodeDist (n1, n2: NODE): AL_FIXED;
  CONST
    SCALE = 64;
  VAR
    dx, dy: AL_FIXED;
  BEGIN
     dx := al_itofix (n1.x - n2.x) DIV SCALE;
     dy := al_itofix (n1.y - n2.y) DIV SCALE;
     NodeDist := al_fixsqrt (al_fixmul (dx, dx) + al_fixmul (dy, dy)) * SCALE;
  END;



(* Constructs nodes to go at the ends of the list, for tangent calculations. *)
  FUNCTION DummyNode (aNode, prev: NODE): NODE;
  BEGIN
    DummyNode.x := aNode.x - (prev.x - aNode.x) DIV 8;
    DummyNode.y := aNode.y - (prev.y - aNode.y) DIV 8;
    DummyNode.tangent := al_itofix (0);
  END;



(* Calculates a set of node tangents. *)
  PROCEDURE CalcTangents;
  VAR
    i: INTEGER;
  BEGIN
    Nodes[0] := DummyNode (Nodes[1], Nodes[2]);
    Nodes[NodeCount] := DummyNode (Nodes[NodeCount-1], Nodes[NodeCount-2]);
    FOR i := 1 TO (NodeCount) DO
      Nodes[i].tangent := al_fixatan2 (al_itofix(Nodes[i+1].y - Nodes[i-1].y),
				  al_itofix(Nodes[i+1].x - Nodes[i-1].x));
    INC (NodeCount);
  END;



(* Draws one of the path nodes. *)
  PROCEDURE DrawNode (n: INTEGER);
  BEGIN
    al_circlefill (al_screen, Nodes[n].x, Nodes[n].y, 2, al_palette_color^[1]);
    al_textout_ex (al_screen, al_font, IntToStr (n), Nodes[n].x-7, Nodes[n].y-7,
		   al_palette_color^[255], -1);
  END;



(* Calculates the control points for a spline segment. *)
  PROCEDURE GetControlPoints (n1, n2: NODE; VAR points: ARRAY OF LONGINT);
  VAR
    Dist: AL_FIXED;
  BEGIN
    Dist := al_fixmul (NodeDist (n1, n2), Curviness);

    points[0] := n1.x;
    points[1] := n1.y;

    points[2] := n1.x + al_fixtoi (al_fixmul (al_fixcos (n1.tangent), Dist));
    points[3] := n1.y + al_fixtoi (al_fixmul (al_fixsin (n1.tangent), Dist));

    points[4] := n2.x - al_fixtoi (al_fixmul (al_fixcos (n2.tangent), Dist));
    points[5] := n2.y - al_fixtoi (al_fixmul (al_fixsin (n2.tangent), Dist));

    points[6] := n2.x;
    points[7] := n2.y;
  END;



(* Draws a spline curve connecting two nodes. *)
  PROCEDURE DrawSpline (n1, n2: NODE);
  VAR
    Points: ARRAY [0..8] OF LONGINT;
    Index: INTEGER;
  BEGIN
    GetControlPoints (n1, n2, Points);
    al_spline (al_screen, Points, al_palette_color^[255]);
    IF ShowControlPoints THEN
      FOR Index := 1 TO 2 DO
	al_circlefill (al_screen, Points[Index*2], Points[Index*2+1], 2,
		       al_palette_color^[2]);
  END;



(* Draws the spline paths. *)
  PROCEDURE DrawSplines;
  VAR
    Index: INTEGER;
  BEGIN
    al_acquire_screen;

    al_clear_to_color (al_screen, al_makecol (255, 255, 255));

    al_textout_centre_ex (al_screen, al_font, 'Spline curve path', AL_SCREEN_W DIV 2, 8,
		al_palette_color^[255], al_palette_color^[0]);
    al_textout_centre_ex (al_screen, al_font, Format ('Curviness = %.2f', [al_fixtof (Curviness)]),
		AL_SCREEN_W DIV 2, 32, al_palette_color^[255], al_palette_color^[0]);
    al_textout_centre_ex (al_screen, al_font, 'Up/down keys to alter', AL_SCREEN_W DIV 2, 44,
		     al_palette_color^[255], al_palette_color^[0]);
    al_textout_centre_ex (al_screen, al_font, 'Space to walk', AL_SCREEN_W DIV 2, 68,
		     al_palette_color^[255], al_palette_color^[0]);
    al_textout_centre_ex (al_screen, al_font, 'C to display control points', AL_SCREEN_W DIV 2,
		     92, al_palette_color^[255], al_palette_color^[0]);
    al_textout_centre_ex (al_screen, al_font, 'T to display tangents', AL_SCREEN_W DIV 2, 104,
		     al_palette_color^[255], al_palette_color^[0]);

    FOR Index := 1 TO NodeCount - 3 DO
      DrawSpline (Nodes[Index], Nodes[Index+1]);

    FOR Index := 1 TO NodeCount - 2 DO
    BEGIN
      DrawNode (Index);

      IF ShowTangents THEN
      BEGIN
	al_line (al_screen, Nodes[Index].x - al_fixtoi (al_fixcos(Nodes[Index].tangent) * 24),
		 Nodes[Index].y - al_fixtoi (al_fixsin(Nodes[Index].tangent) * 24),
		 Nodes[Index].x + al_fixtoi (al_fixcos(Nodes[Index].tangent) * 24),
		 Nodes[Index].y + al_fixtoi (al_fixsin(Nodes[Index].tangent) * 24),
		 al_palette_color^[1]);
      END;
    END;

    al_release_screen;
  END;



(* Helper procedure. *)
  PROCEDURE ClearUserInput;
  BEGIN
    REPEAT
      al_poll_mouse;
    UNTIL al_mouse_b = 0;
    al_clear_keybuf();
  END;



(* Let the user input a list of path nodes. *)
  PROCEDURE InputNodes;
  VAR
    ExitLoop: BOOLEAN;
  BEGIN
    al_clear_to_color (al_screen, al_makecol (255, 255, 255));

    al_textout_centre_ex (al_screen, al_font, 'Click the left mouse button to add path nodes',
		     AL_SCREEN_W DIV 2, 8, al_palette_color^[255], al_palette_color^[0]);
    al_textout_centre_ex (al_screen, al_font, 'Right mouse button or any key to finish',
		     AL_SCREEN_W DIV 2, 24, al_palette_color^[255], al_palette_color^[0]);

    NodeCount := 1;

    al_show_mouse (al_screen);

    ClearUserInput;

    ExitLoop := FALSE;
    REPEAT
      al_poll_mouse();

      IF (al_mouse_b AND 1) <> 0 THEN
      BEGIN
	IF NodeCount < (MAX_NODES - 1) THEN
	BEGIN
	  Nodes[NodeCount].x := al_mouse_x;
	  Nodes[NodeCount].y := al_mouse_y;

	  al_show_mouse (NIL);
	  DrawNode (NodeCount);
	  al_show_mouse (al_screen);

	  INC (NodeCount);
	END;
	ClearUserInput;
      END
      ELSE IF ((al_mouse_b AND 2) <> 0) OR al_keypressed THEN
      BEGIN
	IF NodeCount < 3 THEN
	  al_alert ('You must enter at least two nodes',
		    '', '', 'Ok', '', 13, 0)
	ELSE
	  ExitLoop := TRUE;
      END;
    UNTIL ExitLoop;

    al_show_mouse (NIL);

    ClearUserInput;
  END;



(* Moves a sprite along the spline path. *)
  PROCEDURE Walk;
  CONST
    MAX_POINTS = 256;
  VAR
    Points: ARRAY [0..7] OF LONGINT;
    x, y: ARRAY [0..MAX_POINTS] OF LONGINT;
    n, i, nPoints, ox, oy: INTEGER;
  BEGIN
    al_acquire_screen;

    al_clear_to_color (al_screen, al_makecol (255, 255, 255));

    FOR i := 1 TO NodeCount - 2 DO
      DrawNode (i);

    al_release_screen();

    ClearUserInput;

    ox := -16;
    oy := -16;

    al_xor_mode (-1);

    FOR n := 1 TO NodeCount - 3 DO
    BEGIN
      nPoints := (al_fixtoi (NodeDist (Nodes[n], Nodes[n+1])) + 3) DIV 4;
      IF nPoints < 1 THEN
	nPoints := 1
      ELSE IF nPoints > MAX_POINTS THEN
	nPoints := MAX_POINTS;

      GetControlPoints (Nodes[n], Nodes[n+1], Points);
      al_calc_spline (Points, nPoints, x, y);

      FOR i := 1 TO (nPoints - 1) DO
      BEGIN
	al_vsync;
	al_acquire_screen();
	al_circlefill (al_screen, ox, oy, 6, al_palette_color^[2]);
	al_circlefill (al_screen, x[i], y[i], 6, al_palette_color^[2]);
	al_release_screen();
	ox := x[i];
	oy := y[i];

	al_poll_mouse();

	IF al_keypressed OR (al_mouse_b <> 0) THEN
	  BREAK;
      END;
    END;

    al_xor_mode (0);

    ClearUserInput;
  END;



VAR
  Key: LONGINT;
BEGIN { The program starts here. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_mouse;
  al_install_timer;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 360, 480, 0, 0) THEN
      BEGIN
	al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      { Shows an error message. }
	al_message (al_error);
      { Shutdowns Allegro. }
	al_exit;
	EXIT;
      END;

  al_set_palette (al_desktop_palette);

  InputNodes;
  CalcTangents;

  Curviness := al_ftofix (0.25);
  ShowTangents := FALSE;
  ShowControlPoints := FALSE;

  DrawSplines;

  REPEAT
    IF al_keypressed THEN
    BEGIN
      Key := al_readkey SHR 8;
      IF Key = AL_KEY_ESC THEN
	break
      ELSE IF Key = AL_KEY_UP THEN
      BEGIN
	INC (Curviness, al_ftofix (0.05));
	DrawSplines;
      END
      ELSE IF Key = AL_KEY_DOWN THEN
      BEGIN
	DEC (Curviness, al_ftofix (0.05));
	DrawSplines;
      END
      ELSE IF Key = AL_KEY_SPACE THEN
      BEGIN
	Walk;
	DrawSplines;
      END
      ELSE IF Key = AL_KEY_T THEN
      BEGIN
	ShowTangents := NOT ShowTangents;
	DrawSplines;
      END
      ELSE IF Key = AL_KEY_C THEN
      BEGIN
	ShowControlPoints := NOT ShowControlPoints;
	DrawSplines;
      END
    END;
  UNTIL FALSE;

  al_exit;

{ End of the program. }
END.
