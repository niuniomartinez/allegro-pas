PROGRAM exquat;
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
 *	Euler angles are convenient for storing and creating 3D orientations.
 *	However, this program demonstrates that they are not good when
 *	interpolating between two different orientations. The problem is
 *	solved by using Allegro's quaternion operations.
 *
 *	In this program, two cubes are rotated between random orientations.
 *	Notice that although they have the same beginning and ending
 *	orientations, they do not follow the same path between orientations.
 *
 *	One cube is being rotated by directly incrementing or decrementing
 *	the Euler angles from the starting point to the ending point.
 *	This is an intuitive notion, but it is incorrect because it does not
 *	cause the object to turn around a single unchanging axis of rotation.
 *	The axis of rotation wobbles resulting in the object spinning in
 *	strange ways. The object will eventually end up in the orientation
 *	that the user intended, but it gets there in a way that is unattractive.
 *	Imagine if this method was used to update the position of a camera in a
 *	game! Sometimes it would swing wildly and disorient the player.
 *
 *	The other cube is animated using quaternions. This results in a much
 *	more pleasing animation because the cube turns around a single axis
 *	of rotation.
 *
 *	By Guillermo "Ñuño" Martínez
 *	From an example of Allegro Game Library by Jason Wilkins.
 *
 *	See README file for license and copyright information.
 *)

  USES
    cube,
    allegro, al3d;

  CONST
  (* the number of steps to get from the starting to the ending orientation *)
    NUM_STEPS = 200;

  TYPE
  (* this structure holds an orientation expressed as Euler angles. Each number
   * represents a rotation about the x, y, and z axis. In the case of Allegro
   * there are 256 degrees to a circle.  Yaw, pitch, and roll correspond to
   * x, y, and z *)
    EULER = RECORD
      x, y, z: SINGLE;
    END;

  (* Stores a point. *)
    POINT = ARRAY [0..2] OF SINGLE;

  (* Store an edge. *)
    EDGE = ARRAY [0..1] OF INTEGER;

  (* Stores a path. *)
    PATH = ARRAY [0..NUM_STEPS] OF ARRAY [0..2] OF SINGLE;

  VAR
  (* matrix to transform world coordinates into normalized eye coordinates *)
    Camera, Rotation: AL_MATRIX_f;

  (* these are backbuffers, drawing is done here before updating the screen *)
    EulerBuffer, QuatBuffer: AL_BITMAPptr;

  (* In these identifiers, 'From' refers to the starting orientation, 'To'
   * refers to the ending orientation and 'In' refers to the interpolated
   * orientation. 'q' refers to quaternion, 'e' refers to Euler *)
    qFromMatrix, qToMatrix, qInMatrix,
    eFromMatrix, eToMatrix, eInMatrix: AL_MATRIX_F;
    qTo, qIn, qFrom: AL_QUAT;
    eFrom, eTo, eIn: EULER;

  (* Here is defined a 2x2x2 cube centered about the origin, and
   * an arrow pointing straight up. They are wireframe objects
   * so only the points and edges are specified.
   *
   * It should be noted that the world coordinate system in this
   * program is oriented like it is in most math books. X and Y
   * are oriented like a floor and Z refers to the height above
   * that floor. (Mathematically, this is known as right-handed
   * coordinate system.)
   *
   * N - North
   * S - South
   * W - West
   * E - East
   * U - Up
   * D - Down
   *)
    BoxPoints: ARRAY [0..7] OF POINT = (
      { X,    Y,    Z   }
      ( -1.0, -1.0, -1.0 ),   { NWD }
      ( -1.0, -1.0,  1.0 ),   { NWU }
      ( -1.0,  1.0, -1.0 ),   { NED }
      ( -1.0,  1.0,  1.0 ),   { NEU }
      (  1.0, -1.0, -1.0 ),   { SWD }
      (  1.0, -1.0,  1.0 ),   { SWU }
      (  1.0,  1.0, -1.0 ),   { SED }
      (  1.0,  1.0,  1.0 )    { SEU }
    );

    BoxEdges: ARRAY [0..11] OF EDGE = (
    { from, to }
      ( 0, 2 ),               { bottom }
      ( 2, 6 ),
      ( 6, 4 ),
      ( 4, 0 ),
      ( 1, 3 ),               { top }
      ( 3, 7 ),
      ( 7, 5 ),
      ( 5, 1 ),
      ( 0, 1 ),               { sides }
      ( 2, 3 ),
      ( 4, 5 ),
      ( 6, 7 )
    );

    ArrowPoints: ARRAY [0..3] OF POINT = (
      { X,    Y,    Z  }
      ( 0.0,  0.0,  0.0 ),    { tail of the arrow, at the origin }
      ( 0.0,  0.0,  2.0 ),    { tip of the arrow head }
      ( 0.0,  0.25, 1.5 ),    { eastern part of the head }
      ( 0.0, -0.25, 1.5 )     { western part of the head }
    );

    ArrowEdges: ARRAY [0..2] OF EDGE = (
      { from, to }
      ( 0, 1 ),
      ( 1, 2 ),
      ( 1, 3 )
    );

  (* Each demo box has associated with it two paths (stored as wireframe
   * objects). These are used to store a history of the orientation of their
   * interpolated axis. These sets of points are used to draw ribbons that
   * show how an object rotated from one orientation to another.
   *)
    ePathPoints1, ePathPoints2, qPathPoints1, qPathPoints2: PATH;

  (* these arrays are shared by both ribbons *)
    PathEdges: ARRAY [0..(NUM_STEPS - 1)] OF EDGE;



(* draw an object defined as a set of points and edges *)
  PROCEDURE RenderWireframeObject (m: AL_MATRIX_Fptr; b: AL_BITMAPptr; Points: ARRAY OF POINT; Edges: ARRAY OF EDGE; np, ne, c: INTEGER);
  VAR
    Index, From, _To: INTEGER;
    TmpPoints: ARRAY OF POINT;
  BEGIN
  { transform the points and store them in a buffer }
    SetLength (TmpPoints, np);
    FOR Index := LOW (TmpPoints)TO HIGH (TmpPoints) DO
    BEGIN
      al_apply_matrix_f (m, Points[Index][0], Points[Index][1], Points[Index][2],
			TmpPoints[Index][0], TmpPoints[Index][1], TmpPoints[Index][2]);

      al_persp_project_f (TmpPoints[Index][0], TmpPoints[Index][1], TmpPoints[Index][2],
			TmpPoints[Index][0], TmpPoints[Index][1]);
    END;

  { draw the edges }
    FOR Index := (ne - 1) DOWNTO LOW (Edges) DO
    BEGIN
      From := Edges[index][0];
      _To := Edges[index][1];

      al_line (b, TRUNC (TmpPoints[From][0]), TRUNC (TmpPoints[From][1]), TRUNC (TmpPoints[_To][0]), TRUNC (TmpPoints[_To][1]), c);
    END;
  END;



(* draws a set of objects that demonstrate whats going on. It consists
 * of a cube, an arrow showing the 'to' orientation, an another arrow 
 * showing the 'from' orientation, and another arrow showing the
 * interpolated orientation.
 *)
  PROCEDURE RenderDemoBox (b: AL_BITMAPptr; From, _In, _to: AL_MATRIX_Fptr; Col1, Col2, Col3: INTEGER);
  BEGIN
    RenderWireframeObject (_in, b, BoxPoints, BoxEdges, 8, 12, Col1);
    RenderWireframeObject (from, b, ArrowPoints, ArrowEdges, 4, 3, Col3);
    RenderWireframeObject (_to, b, ArrowPoints, ArrowEdges, 4, 3, Col3);
    RenderWireframeObject (_in, b, ArrowPoints, ArrowEdges, 4, 3, Col2);
  END;



(* Just interpolate linearly yaw, pitch, and roll.  Doing this _correctly_
 * (I.E get the same results as al_quat_interpolate) would require one to use
 * linear integration, a subject that is in the last 100 pages of my 1500
 * page Calculus book.  This function is an example of what you should NOT
 * do, as in some cases it will cause the orientation to swing wildly about.
 * The path could be anything from nearly correct, a spiral, or a curly Q.
 * The simple solution is to use quaternion interpolation, which always
 * results in a simple circular path.
 *)
  PROCEDURE EulerInterpolate (From, _To: EULER; t: SINGLE; VAR _Out: EULER);
  VAR
    Delta: SINGLE;
  BEGIN
    Delta := (_To.x - From.x) * t;
    _Out.x := From.x + Delta;

    Delta := (_To.y - From.y) * t;
    _Out.y := From.y + Delta;

    Delta := (_To.z - From.z) * t;
    _Out.z := From.z + Delta;
  END;



VAR
  Index: INTEGER;
  t: SINGLE;
BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode(AL_GFX_SAFE, 640, 480, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

  al_set_palette (al_desktop_palette);
  al_clear_to_color (al_screen, al_palette_color^[0]);

{ Each back-buffer is one quarter the size of the screen }
  EulerBuffer := al_create_bitmap (320, 240);
  QuatBuffer := al_create_bitmap (320, 240);

  IF (EulerBuffer = NIL) OR (EulerBuffer = NIL) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error creating bitmaps'#10);
    EXIT;
  END;

  al_set_palette (al_desktop_palette);

{ setup the viewport for rendering into the back-buffers }
  al_set_projection_viewport (0, 0, 320, 240);

{ print out something helpful for the user }
  al_textout_ex (al_screen, al_font, 'SPACE - next interpolation', 184, 24, al_palette_color^[15], -1);
  al_textout_ex (al_screen, al_font, '    R - repeat last interpolation', 184, 40, al_palette_color^[15], -1);
  al_textout_ex (al_screen, al_font, '  ESC - quit', 184, 56, al_palette_color^[15], -1);

  al_textout_ex (al_screen, al_font, 'Interpolating Euler Angles', 56, 110, al_palette_color^[15], -1);
  al_textout_ex (al_screen, al_font, 'Interpolating Quaternions', 380, 110, al_palette_color^[15], -1);

  al_textout_ex (al_screen, al_font, 'Incorrect!', 120, 360, al_palette_color^[15], -1);
  al_textout_ex (al_screen, al_font, 'Correct!', 448, 360, al_palette_color^[15], -1);

{ initialize the path edges. This structure is used by both the Euler
  path and the quaternion path. It connects all the points end to end }
  FOR Index := 0 TO (NUM_STEPS - 2) DO
  BEGIN
    PathEdges[Index][0] := Index;
    PathEdges[Index][1] := Index + 1;
  END;

{ initialize the first destination orientation }
  RANDOMIZE;

  eFrom.x := 0;
  eFrom.y := 0;
  eFrom.z := 0;
  eTo.x := RANDOM (256);
  eTo.y := RANDOM (256);
  eTo.z := RANDOM (256);

{ the camera is backed away from the origin and turned to face it }
  al_get_camera_matrix_f (@Camera, 5, 0, 0, -1, 0, 0, 0, 0, 1, 46, 1.33);

{ this is a for-ever loop }
  WHILE TRUE DO
  BEGIN
    al_clear_keybuf;

    FOR Index := 0 TO NUM_STEPS DO
    BEGIN
      IF al_keypressed THEN
	BREAK;

      t := Index * (1 / NUM_STEPS);

    { the first part shows how to animate the cube incorrectly
      using Euler angles }

    { create the matrix for the starting orientation }
      al_get_rotation_matrix_f (@Rotation, eFrom.x, eFrom.y, eFrom.z);
      al_matrix_mul_f (@Rotation, @Camera, @eFromMatrix);

    { create the matrix for the ending orientation }
      al_get_rotation_matrix_f (@Rotation, eTo.x, eTo.y, eTo.z);
      al_matrix_mul_f (@rotation, @Camera, @eToMatrix);

    { use the incorrect method to interpolate between them }
      EulerInterpolate (eFrom, eTo, t, eIn);
      al_get_rotation_matrix_f (@Rotation, eIn.x, eIn.y, eIn.z);
      al_matrix_mul_f (@Rotation, @Camera, @eInMatrix);

    { update the lines that make up the Euler orientation path }
      al_apply_matrix_f (@Rotation, 0, 0, 1.5,
			ePathPoints1[Index][0],
			ePathPoints1[Index][1],
			ePathPoints1[Index][2]);

      al_apply_matrix_f (@rotation, 0, 0, 2.0,
			ePathPoints2[Index][0],
			ePathPoints2[Index][1],
			ePathPoints2[Index][2]);

    { render the results to the Euler sub-bitmap }
      al_clear_to_color (EulerBuffer, al_palette_color^[0]);
      RenderDemoBox (EulerBuffer, @eFromMatrix, @eInMatrix, @eToMatrix,
	al_palette_color^[15], al_palette_color^[1], al_palette_color^[4]);

      RenderWireframeObject (@Camera, EulerBuffer, ePathPoints1,
				PathEdges, Index + 1, Index,
				al_palette_color^[5]);

      RenderWireframeObject (@Camera, EulerBuffer, ePathPoints2,
				PathEdges, Index + 1, Index,
				al_palette_color^[5]);

    { here is how to animate the cube correctly using quaternions }

    { create a matrix for the starting orientation. This time
      we create it using quaternions.  This is to demonstrate
      that the quaternion gotten with get_rotation_quat will
      generate the same matrix as that gotten by get_rotation_matrix }
      al_get_rotation_quat (@qFrom, eFrom.x, eFrom.y, eFrom.z);
      al_quat_to_matrix (@qFrom, @Rotation);
      al_matrix_mul_f (@Rotation, @Camera, @qFromMatrix);

    { this is the same as above, but for the ending orientation }
      al_get_rotation_quat (@qTo, eTo.x, eTo.y, eTo.z);
      al_quat_to_matrix (@qTo, @Rotation);
      al_matrix_mul_f (@Rotation, @Camera, @qToMatrix);

    { quat_interpolate is the proper way to interpolate between two
      orientations. }
      al_quat_interpolate (@qFrom, @qTo, t, @qIn);
      al_quat_to_matrix (@qIn, @Rotation);
      al_matrix_mul_f (@Rotation, @Camera, @qInMatrix);

    { update the lines that make up the quaternion orientation path }
      al_apply_matrix_f (@Rotation, 0, 0, 1.5,
			qPathPoints1[Index][0],
			qPathPoints1[Index][1],
			qPathPoints1[Index][2]);

      al_apply_matrix_f (@Rotation, 0, 0, 2.0,
			qPathPoints2[Index][0],
			qPathPoints2[Index][1],
			qPathPoints2[Index][2]);

    { render the results to the quaternion sub-bitmap }
      al_clear_to_color (QuatBuffer, al_palette_color^[0]);

      RenderDemoBox (QuatBuffer, @qFromMatrix, @qInMatrix, @qToMatrix,
		al_palette_color^[15], al_palette_color^[1], al_palette_color^[4]);

      RenderWireframeObject (@Camera, QuatBuffer, qPathPoints1,
			PathEdges, Index + 1, Index,
			al_palette_color^[5]);

      RenderWireframeObject (@Camera, QuatBuffer, qPathPoints2,
			PathEdges, Index + 1, Index,
			al_palette_color^[5]);

      al_acquire_bitmap (al_screen);
      al_blit (EulerBuffer, al_screen, 0, 0, 0,   120, 320, 240);
      al_blit (QuatBuffer,  al_screen, 0, 0, 320, 120, 320, 240);
      al_release_bitmap (al_screen);

      al_rest (1);
    END;

  { handle user input }
    REPEAT
      CASE al_readkey() >> 8 OF
      AL_KEY_R:
	BEGIN
	{ skip updating the EULER angles so that the last interpolation
	  will repeat }
	  BREAK;
	END;
      AL_KEY_SPACE:
	BEGIN
	{ make the last ending orientation the starting orientation and
	  generate a random new ending orientation }
	  eFrom := eTo;

	  eTo.x := RANDOM (256);
	  eTo.y := RANDOM (256);
	  eTo.z := RANDOM (256);
	  BREAK;
	END;
      AL_KEY_ESC:
	BEGIN
	{ quit the program }
	  al_destroy_bitmap (EulerBuffer);
	  al_destroy_bitmap (QuatBuffer);
	  EXIT;
	END;
      END;
    UNTIl FALSE;
  END;
END.
