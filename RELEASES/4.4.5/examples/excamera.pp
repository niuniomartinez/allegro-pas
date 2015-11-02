PROGRAM excamera;
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
 *    This program demonstrates how to use the ex_get_camera_matrix
 *    function to view a 3d world from any position and angle.
 *
 *    The example draws a checkered floor through a viewport region on the
 *    screen. You can use the keyboard to move around the camera or modify the
 *    size of the viewport. The keys that can be used with this example are
 *    displayed between brackets at the top of the screen.
 *
 *	It uses fixed point values, that are less exact than floating point,
 *	so there are few glitches depending on angle and distance.
 *
 *	By Guillermo "Ñuño" Martínez
 *	From Shawn Hargreaves' example.
 *
 *	See README file for license and copyright information.
 *)

{$IFDEF FPC}
{ This example uses Object Pascal language. }
  {$MODE OBJFPC}
{$ENDIF}

  USES
    allegro, al3d, alfixed,
    sysutils;

  TYPE
  (* A vector. *)
    TVector = OBJECT
    PUBLIC
      x, y, z: AL_FIXED;

      PROCEDURE SetAll (ax, ay, az: AL_FIXED);
    END;



  (* The camera. *)
    TCamera = CLASS (TObject)
    PRIVATE
    (* Camera position and rotation. *)
      fPosition, fAngle,
      fFront, fUp: TVector;
    (* Camera viewport. *)
      fViewportW, fViewportH, fFOV: INTEGER;
      fAspect: AL_FIXED;
    (* Camera matrix. *)
      fMatrix: AL_MATRIX;

    (* Calculates. *)
      FUNCTION GetMatrix: AL_MATRIX;
    PUBLIC
    (* Creates the camera. *)
      CONSTRUCTOR Create;

    (* Camera position. *)
      PROPERTY Position: TVector READ fPosition;
    (* Camera angle. *)
      PROPERTY Angle: TVector READ fAngle;
    (* Front vector. *)
      PROPERTY Front: TVector READ fFront;
    (* Up vector. *)
      PROPERTY Up: TVector READ fUp;
    (* Viewport width. *)
      PROPERTY ViewportW: INTEGER READ fViewportW WRITE fViewportW;
    (* Viewport height. *)
      PROPERTY ViewportH: INTEGER READ fViewportH WRITE fViewportH;
    (* Camera Field Of View. *)
      PROPERTY FOV: INTEGER READ fFOV WRITE fFOV;
    (* Aspect ratio. *)
      PROPERTY Aspect: AL_FIXED READ fAspect WRITE fAspect;
    (* Camera matrix. *)
      PROPERTY Matrix: AL_MATRIX READ GetMatrix;
    END;



  CONST
  (* Times we check frames. *)
    FPS_INT = 2;
  VAR
  (* Frame count. *)
    FPS, FrameCount: INTEGER;
  (* Camera. *)
    Camera: TCamera;
  (* Backbuffer. *)
    Backbuffer: AL_BITMAPptr;
  (* Use or not to use... *)
    UseVSync: BOOLEAN;
  (* Used colours. *)
    ClrGreen, ClrYellow, ClrRed, ClrWhite, ClrBlack: INTEGER;



(***********
 * TVector *
 ***********)

  PROCEDURE TVector.SetAll (ax, ay, az: AL_FIXED);
  BEGIN
    x := ax; y := ay; z := az
  END;



(***********
 * TCamera *
 ***********)

(* Returns matrix. *)
  FUNCTION TCamera.GetMatrix: AL_MATRIX;
  VAR
    Roller: AL_MATRIX;
    XFront, YFront, ZFront,
    XUp, YUp, ZUp: AL_FIXED;
  BEGIN
  { calculate the in-front vector }
    XFront := al_fixmul (al_fixsin (fAngle.y),  al_fixcos (fAngle.x));
    YFront := al_fixsin (fAngle.x);
    ZFront := al_fixmul (al_fixcos (fAngle.y), al_fixcos (fAngle.x));

  { rotate the up vector around the in-front vector by the roll angle }
    al_get_vector_rotation_matrix (Roller, XFront, YFront, ZFront, fAngle.z);
    al_apply_matrix (Roller, 0, al_itofix (-1), 0, XUp, YUp, ZUp);

  { build the camera matrix }
    al_get_camera_matrix (
	fMatrix,
	fPosition.x, fPosition.y, fPosition.z, { camera position }
	XFront, YFront, ZFront,                { in-front vector }
	XUp, YUp, ZUp,                         { up vector }
	al_itofix (fFOV),                      { field of view }
	fAspect                                { aspect ratio }
    );

    fFront.X := XFront; fFront.Y := YFront; fFront.Z := ZFront;
    fUp.X := XUp; fUp.Y := YUp; fUp.Z := ZUp;

{ Next is a hack that implements a working camera matrix using rotation instead
  of Front/Up matrices.  FOV an Aspect can be simulated by scaling.

  al_get_translation_matrix (fMatrix, -fPosition.x, -fPosition.y, -fPosition.z);

  al_get_y_rotate_matrix (Roller, -fAngle.y);
  al_matrix_mul (fMatrix, Roller, fMatrix);

  al_get_x_rotate_matrix (Roller, -fAngle.x);
  al_matrix_mul (fMatrix, Roller, fMatrix);

  al_get_z_rotate_matrix (Roller, -fAngle.z);
  al_matrix_mul (fMatrix, Roller, fMatrix);

  End of hack. }

    RESULT := fMatrix
  END;



(* Constructor. *)
  CONSTRUCTOR TCamera.Create;
  BEGIN
    INHERITED Create;
    fPosition.SetAll (0, al_itofix (-2), al_itofix (-4));
    fViewportW := 320; fViewportH := 240;
    fFOV := 48;
    fAspect := al_ftofix (1.33);
  END;



(****************************************************************************)

  CONST
    GRID_SIZE = 8;

(* Draws the grid. *)
  PROCEDURE DrawGrid (OutBmp: AL_BITMAPptr; CameraMatrix: AL_MATRIX);

  (* Displays a grid square. *)
    PROCEDURE DrawSquare (X, Z: INTEGER);
    VAR
      _V: ARRAY [1..4] OF AL_V3D;
      _VOut, _VTmp: ARRAY [1..8] OF AL_V3D;
      V: ARRAY [1..4] OF AL_V3Dptr;
      VOut, VTmp: ARRAY [1..8] OF AL_V3Dptr;
      Flags: ARRAY [1..4] OF INTEGER;
      Out_: ARRAY [1..8] OF INTEGER;
      Ndx, VCnt: INTEGER;
    BEGIN
      FOR Ndx := LOW (V) TO HIGH (V) DO
	V[Ndx] := @_V[Ndx];
      FOR Ndx := LOW (VOut) TO HIGH (VOut) DO
      BEGIN
	VOut[Ndx] := @_VOut[Ndx];
	VTmp[Ndx] := @_VTmp[Ndx];
      END;
    { Set up four vertices with the world-space position of the tile }
      v[1]^.x := al_ftofix (x - GRID_SIZE / 2);
      v[1]^.y := 0;
      v[1]^.z := al_ftofix (z - GRID_SIZE / 2);

      v[2]^.x := al_ftofix (x - GRID_SIZE / 2 + 1);
      v[2]^.y := 0;
      v[2]^.z := al_ftofix (z - GRID_SIZE / 2);

      v[3]^.x := al_ftofix (x - GRID_SIZE / 2 + 1);
      v[3]^.y := 0;
      v[3]^.z := al_ftofix (z - GRID_SIZE / 2 + 1);

      v[4]^.x := al_ftofix (x - GRID_SIZE / 2);
      v[4]^.y := 0;
      v[4]^.z := al_ftofix (z - GRID_SIZE / 2 + 1);

    { apply the camera matrix, translating world space -> view space }
      FOR Ndx := LOW (V) TO HIGH (V) DO
      BEGIN
	al_apply_matrix (
	  CameraMatrix,
	  V[Ndx]^.x, V[Ndx]^.y, V[Ndx]^.z,
	  V[Ndx]^.x, V[Ndx]^.y, V[Ndx]^.z
        );

      { set flags if this vertex is off the edge of the screen }
	Flags[Ndx] := 0;

	IF V[Ndx]^.x < -V[Ndx]^.z THEN
	  Flags[Ndx] := Flags[Ndx] OR 1
	ELSE IF V[Ndx]^.x > V[Ndx]^.z THEN
	  Flags[Ndx] := Flags[Ndx] OR 2;

	IF V[Ndx]^.y < -V[Ndx]^.z THEN
	  Flags[Ndx] := Flags[Ndx] OR 4
	ELSE IF V[Ndx]^.y > V[Ndx]^.z THEN
	  Flags[Ndx] := Flags[Ndx] OR 8;

	IF V[Ndx]^.z < al_ftofix (0.1) THEN
	  Flags[Ndx] := Flags[Ndx] OR 16;
      END;

    { quit if all vertices are off the same edge of the screen }
      IF (Flags[1] <> 0) AND (Flags[2] <> 0) AND (Flags[3] <> 0) AND (Flags[4] <> 0) THEN
        EXIT;

      IF (Flags[1] <> 0) OR (Flags[2] <> 0) OR (Flags[3] <> 0) OR (Flags[4] <> 0) THEN
      BEGIN
      { clip if any vertices are off the edge of the screen }
	VCnt := al_clip3d (
	  AL_POLYTYPE_FLAT, al_ftofix (0.1), al_ftofix (0.1), 4, v,
	  VOut, VTmp, Out_
	);

	IF VCnt <= 0 THEN EXIT
      END
      ELSE BEGIN
      { no need to bother clipping this one }
	VOut[1] := v[1];
	VOut[2] := v[2];
	VOut[3] := v[3];
	VOut[4] := v[4];

	VCnt := 4
      END;

    { project view space -> screen space }
      FOR Ndx := LOW (VOut) TO VCnt DO
	al_persp_project (
	  VOut[Ndx]^.x, VOut[Ndx]^.y, VOut[Ndx]^.z,
	  VOut[Ndx]^.x, VOut[Ndx]^.y
	);

    { set the color }
      IF ((X + Z) AND 1) <> 0 THEN
	VOut[1]^.c := ClrGreen
      ELSE
	VOut[1]^.c := ClrYellow;

    { render the polygon }
      al_polygon3d (OutBmp, AL_POLYTYPE_FLAT, NIL, VCnt, VOut);
    END;

  VAR
    X, Y: INTEGER;
  BEGIN
    FOR X := 1 TO GRID_SIZE DO
      FOR Y := 1 TO GRID_SIZE DO
        DrawSquare (X - 1, Y - 1);
  END;



(* Draws everithing. *)
  PROCEDURE Render (OutBmp: AL_BITMAPptr);
  VAR
    X, Y, W, H: INTEGER;
  BEGIN
  { Clear the background and the z-buffer. }
    al_clear_to_color (OutBmp, ClrWhite);

  { set up the viewport region }
    x := (AL_SCREEN_W - Camera.ViewportW) DIV 2;
    y := (AL_SCREEN_H - Camera.ViewportH) DIV 2;
    w := Camera.ViewportW;
    h := Camera.ViewportH;

    al_set_projection_viewport (x, y, w, h);
    al_rect (OutBmp, x - 1, y - 1, x + w, y + h, ClrRed);
    al_set_clip_rect  (OutBmp, x, y, x + w - 1, y + h - 1);

  { Draw grid. }
    DrawGrid (OutBmp, Camera.Matrix);

  { overlay some text }
    al_set_clip_rect (OutBmp, 0, 0, OutBmp^.w, OutBmp^.h);
    al_textprintf_ex (
      OutBmp, al_font,
      0,  0, ClrBlack, -1,
      'Viewport width: %d (w/W changes)', [Camera.ViewportW]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0,  8, ClrBlack, -1,
      'Viewport height: %d (h/H changes)', [Camera.ViewportH]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 16, ClrBlack, -1,
      'Field of view: %d (f/F changes)', [Camera.FOV]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 24, ClrBlack, -1,
      'Aspect ratio: %.2f (a/A changes)', [al_fixtof (Camera.Aspect)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 32, ClrBlack, -1,
      'X position: %.2f (x/X changes)', [al_fixtof (Camera.Position.X)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 40, ClrBlack, -1,
      'Y position: %.2f (y/Y changes)', [al_fixtof (Camera.Position.Y)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 48, ClrBlack, -1,
      'Z position: %.2f (z/Z changes)', [al_fixtof (Camera.Position.Z)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 56, ClrBlack, -1,
      'Heading: %.2f (left/right changes)', [al_fixtof (Camera.Angle.Y)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 64, ClrBlack, -1,
      'Pitch: %.2f (pgup/pgdn changes)', [al_fixtof (Camera.Angle.X)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 72, ClrBlack, -1,
      'Roll: %.2f (r/R changes)', [al_fixtof (Camera.Angle.Z)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 80, ClrBlack, -1,
      'Front vector: %.2f, %.2f, %.2f',
      [al_fixtof (Camera.Front.X), al_fixtof (Camera.Front.Y),
       al_fixtof (Camera.Front.Z)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 88, ClrBlack, -1,
      'Up vector: %.2f, %.2f, %.2f',
      [al_fixtof (Camera.Up.X), al_fixtof (Camera.Up.Y),
       al_fixtof (Camera.Up.Z)]
    );
    al_textprintf_ex (
      OutBmp, al_font,
      0, 96, ClrBlack, -1,
      'Frames per second: %d', [FPS]
    );
    IF UseVSync THEN
      al_textout_ex (
	OutBmp, al_font,
	'Using vsync (v changes)',
	0, 104, ClrBlack, -1
      )
    ELSE
      al_textout_ex (
	OutBmp, al_font,
	'Don''t using vsync (V changes)',
	0, 104, ClrBlack, -1
      );
  END;



(* deal with user input *)
  PROCEDURE ProcessInput;
  BEGIN
    al_poll_keyboard;

    IF al_key[AL_KEY_W] THEN
    BEGIN
      IF al_key_shifts AND AL_KB_SHIFT_FLAG <> 0 THEN
      BEGIN
	IF Camera.ViewportW < AL_SCREEN_W THEN
	  Camera.ViewportW := Camera.ViewportW + 8;
      END
      ELSE BEGIN
	IF Camera.ViewportW > 16 THEN
	  Camera.ViewportW := Camera.ViewportW - 8;
      END;
    END;

    IF al_key[AL_KEY_H] THEN
    BEGIN
      IF al_key_shifts AND AL_KB_SHIFT_FLAG <> 0 THEN
      BEGIN
	IF Camera.ViewportH < AL_SCREEN_H THEN
	  Camera.ViewportH := Camera.ViewportH + 8;
      END
      ELSE BEGIN
	IF Camera.ViewportH > 16 THEN
	  Camera.ViewportH := Camera.ViewportH - 8;
      END;
    END;

    IF al_key[AL_KEY_F] THEN
    BEGIN
      IF al_key_shifts AND AL_KB_SHIFT_FLAG <> 0 THEN
      BEGIN
	IF Camera.FOV < 96 THEN
	  Camera.FOV := Camera.FOV + 1
      END
      ELSE BEGIN
	IF Camera.FOV > 16 THEN
	  Camera.FOV := Camera.FOV - 1;
      END
    END;

    IF al_key[AL_KEY_A] THEN
    BEGIN
      IF al_key_shifts AND AL_KB_SHIFT_FLAG <> 0 THEN
      BEGIN
	IF Camera.Aspect < al_itofix (2) THEN
	  Camera.Aspect := al_fixadd (Camera.Aspect, al_ftofix (0.04));
      END
      ELSE BEGIN
	IF Camera.Aspect > al_ftofix (0.1) THEN
	  Camera.Aspect := al_fixsub (Camera.Aspect, al_ftofix (0.04));
      END;
    END;

    IF al_key[AL_KEY_X] THEN
    BEGIN
      IF al_key_shifts AND AL_KB_SHIFT_FLAG <> 0 THEN
	Camera.Position.X := al_fixadd (Camera.Position.X, al_ftofix (0.05))
      ELSE
	Camera.Position.X := al_fixsub (Camera.Position.X, al_ftofix (0.05));
    END;

    IF al_key[AL_KEY_Y] THEN
    BEGIN
      IF al_key_shifts AND AL_KB_SHIFT_FLAG <> 0 THEN
	Camera.Position.Y := al_fixadd (Camera.Position.Y, al_ftofix (0.05))
      ELSE
	Camera.Position.Y := al_fixsub (Camera.Position.Y, al_ftofix (0.05));
    END;

    IF al_key[AL_KEY_Z] THEN
    BEGIN
      IF al_key_shifts AND AL_KB_SHIFT_FLAG <> 0 THEN
	Camera.Position.Z := al_fixadd (Camera.Position.Z, al_ftofix (0.05))
      ELSE
	Camera.Position.Z := al_fixsub (Camera.Position.Z, al_ftofix (0.05));
    END;

    IF al_key[AL_KEY_LEFT] THEN
      Camera.Angle.Y := al_fixsub (Camera.Angle.Y, al_itofix (1));

    IF al_key[AL_KEY_RIGHT] THEN
      Camera.Angle.Y := al_fixadd (Camera.Angle.Y, al_itofix (1));

    IF al_key[AL_KEY_PGUP] THEN
      IF Camera.Angle.X > al_itofix (-32) THEN
        Camera.Angle.X := al_fixsub (Camera.Angle.X, al_itofix (1));

    IF al_key[AL_KEY_PGDN] THEN
      IF Camera.Angle.X < al_itofix (32) THEN
        Camera.Angle.X := al_fixadd (Camera.Angle.X, al_itofix (1));

    IF al_key[AL_KEY_R] THEN
    BEGIN
      IF al_key_shifts AND AL_KB_SHIFT_FLAG <> 0 THEN
      BEGIN
	IF Camera.Angle.Z < al_itofix (32) THEN
	  Camera.Angle.Z := al_fixadd (Camera.Angle.Z, al_itofix (1));
      END
      ELSE BEGIN
	IF Camera.Angle.Z > al_itofix (-32) THEN
	  Camera.Angle.Z := al_fixsub (Camera.Angle.Z, al_itofix (1));
      END;
    END;

    IF al_key[AL_KEY_UP] THEN
    BEGIN
      Camera.Position.X := al_fixadd (Camera.Position.X, al_fixsin (Camera.Angle.Y) DIV 2);
      Camera.Position.Z := al_fixadd (Camera.Position.Z, al_fixcos (Camera.Angle.Y) DIV 2);
    END;

    IF al_key[AL_KEY_DOWN] THEN
    BEGIN
      Camera.Position.X := al_fixsub (Camera.Position.X, al_fixsin (Camera.Angle.Y) DIV 2);
      Camera.Position.Z := al_fixsub (Camera.Position.Z, al_fixcos (Camera.Angle.Y) DIV 2);
    END;

    IF al_key[AL_KEY_V] THEN
      UseVSync := al_key_shifts AND AL_KB_SHIFT_FLAG <> 0;
  END;



  PROCEDURE FPSCheck; CDECL;
  BEGIN
    FPS := FrameCount * FPS_INT;
    FrameCount := 0;
  END;



BEGIN { The program starts here. }

  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_timer;
  al_install_keyboard;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;
  UseVSync := TRUE;

  al_set_palette (al_desktop_palette);
  ClrBlack := al_makecol (0, 0, 0);
  ClrGreen := al_makecol (0, 255, 0);
  ClrRed   := al_makecol (255, 0, 0);
  ClrYellow:= al_makecol (255, 255, 0);
  ClrWhite := al_makecol (255, 255, 255);

  Backbuffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);

  Camera := TCamera.Create;

  al_install_int_ex (@FPSCheck, AL_BPS_TO_TIMER (FPS_INT));

  TRY
    WHILE NOT al_key[AL_KEY_ESC] DO
    BEGIN
      Render (Backbuffer);

      IF UseVSync THEN al_vsync;

      al_blit (Backbuffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
      INC (FrameCount);

      ProcessInput;
    END
  FINALLY
    Camera.Free;
    al_destroy_bitmap (Backbuffer);
  END;
END.
