PROGRAM ex_camera;
(* An example demonstrating how to use ALLEGRO_TRANSFORM to represent a 3D
 * camera.
 *)
(*
  Copyright (c) 2012-2018 Guillermo Martínez J.

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
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  USES
    Common,
    Allegro5, al5primitives, al5color, al5font,
    math, sysutils;

  CONST
    pi = ALLEGRO_PI;

  TYPE
    RVector = RECORD
      x, y, z: SINGLE;
    END;

    RCamera = RECORD
      Position: RVector;
      xAxis: RVector; { This represent the direction looking to the right. }
      yAxis: RVector; { This is the up direction. }
      zAxis: RVector; { This is the direction towards the viewer ('backwards'). }
      VerticalFieldOfView: DOUBLE; { In radians. }
    END;

    TExample = RECORD
      Camera: RCamera;

    { Controls sensitivity }
      MouseLookSpeed,
      MovementSpeed: DOUBLE;

    { Keyboard and mouse state }
      Button: ARRAY [0..9] OF BOOLEAN;
      Key: ARRAY [0..ALLEGRO_KEY_MAX-1] OF BOOLEAN;
      KeyState: ARRAY [0..ALLEGRO_KEY_MAX-1] OF BOOLEAN;
      MouseDx, MouseDy: INTEGER;

    { Control scheme selection }
      controls: INTEGER;
      ControlsNames: ARRAY [0..2] OF STRING;

    { the vertex data }
      n: INTEGER;
      v: ARRAY OF ALLEGRO_VERTEX;

    { used to draw some info text }
      Font: ALLEGRO_FONTptr;
    END;

  VAR
    Example: TExample;



(* Calculates the dot product between two vectors.  This corresponds to the
 * angle between them times their lengths. *)
  FUNCTION VectorDotProduct (a, b: RVector): DOUBLE;
  BEGIN
    VectorDotProduct := a.x * b.x + a.y * b.y + a.z * b.z
  END;



(* Returns a vector multiplied by a scalar. *)
  FUNCTION VectorMul (a: RVector; s: SINGLE): RVector;
  BEGIN
    VectorMul.x := a.x * s;
    VectorMul.y := a.y * s;
    VectorMul.z := a.z * s
  END;



(* Returns the vector norm (length). *)
  FUNCTION VectorNorm (a: RVector): DOUBLE;
  BEGIN
    VectorNorm := SQRT (VectorDotProduct (a, a))
  END;



(* Returns a normalized version of the given vector. *)
  FUNCTION VectorNormalize (a: RVector): RVector;
  VAR
    s: DOUBLE;
  BEGIN
    s := VectorNorm (a);
    IF s = 0 THEN EXIT (a) ELSE EXIT (VectorMul (a, 1 / s))
  END;



(* In-place add another vector to a vector. *)
  PROCEDURE VectorIadd (VAR a: RVector; b: RVector);
  BEGIN
    a.x := a.x + b.x;
    a.y := a.y + b.y;
    a.z := a.z + b.z
  END;



(* Rotates the camera around the given axis. *)
  PROCEDURE RotateCameraAroundAxis
    (VAR c: RCamera; Axis: RVector; Radians: DOUBLE);
  VAR
    t: ALLEGRO_TRANSFORM;
  BEGIN
    al_identity_transform (t);
    al_rotate_transform_3d (t, Axis.x, Axis.y, Axis.z, Radians);
    al_transform_coordinates_3d (t, c.xAxis.x, c.xAxis.y, c.xAxis.z);
    al_transform_coordinates_3d (t, c.yAxis.x, c.yAxis.y, c.yAxis.z);
    al_transform_coordinates_3d (t, c.zAxis.x, c.zAxis.y, c.zAxis.z)
  END;



(* Move the camera along its x axis and z axis (which corresponds to
   right and backwards directions). *)
  PROCEDURE MoveCameraAlongDirection
    (VAR Camera: RCamera; aRight, aForward: DOUBLE);
  BEGIN
    VectorIadd (Camera.Position, VectorMul (Camera.xAxis, aRight));
    VectorIadd (Camera.Position, VectorMul (Camera.zAxis, -aForward))
  END;



(* Get a vector with y = 0 looking in the opposite direction as the camera z
   axis. If looking straight up or down returns a 0 vector instead. *)
  FUNCTION  GetGroundForwardVector (CONST Camera: RCamera): RVector;
  VAR
    Move: RVector;
  BEGIN
    Move := VectorMul (Camera.zAxis, -1);
    Move.y := 0;
    EXIT (VectorNormalize (Move))
  END;



(* Get a vector with y = 0 looking in the same direction as the camera x axis.
   If looking straight up or down returns a 0 vector instead. *)
  FUNCTION GetGroundRightVector (CONST Camera: RCamera): RVector;
  VAR
    Move: RVector;
  BEGIN
    Move := Camera.xAxis;
    Move.y := 0;
    EXIT (VectorNormalize (Move))
  END;



(* Like CameraMoveAlongDirection but moves the camera along the ground plane
   only. *)
  PROCEDURE MoveCameraAlongGround
    (VAR Camera: RCamera; aRight, aForward: DOUBLE);
  VAR
    f, r: RVector;
  BEGIN
    f := GetGroundForwardVector (Camera);
    r := GetGroundRightVector (Camera);
    Camera.Position.x := Camera.Position.x + (f.x * aForward + r.x * aRight);
    Camera.position.z := Camera.Position.z + (f.z * aForward + r.z * aRight)
  END;



(* Calculate the pitch of the camera. This is the angle between the z axis
   vector and our direction vector on the y = 0 plane. *)
  FUNCTION GetPitch (CONST Camera: RCamera): DOUBLE;
  VAR
    f: RVector;
  BEGIN
    f := GetGroundForwardVector (Camera);
    EXIT (arcsin (VectorDotProduct (f, Camera.yAxis)))
  END;



(* Calculate the yaw of the camera. This is basically the compass direction. *)
  FUNCTION GetYaw (CONST Camera: RCamera): DOUBLE;
  BEGIN
    EXIT (arctan2 (Camera.zAxis.x, Camera.zAxis.z))
  END;



(* Calculate the roll of the camera. This is the angle between the x axis
   vector and its project on the y = 0 plane. *)
  FUNCTION GetRoll (CONST Camera: RCamera): DOUBLE;
  VAR
    R: RVector;
  BEGIN
    r := GetGroundRightVector (Camera);
    EXIT (arcsin (VectorDotProduct (r, Camera.yAxis)))
  END;



(* Set up a perspective transform. We make the screen span
   2 vertical units (-1 to +1) with square pixel aspect and the camera's
   vertical field of view. Clip distance is always set to 1. *)
  PROCEDURE Setup3DProjection;
  VAR
    Projection: ALLEGRO_TRANSFORM;
    Display: ALLEGRO_DISPLAYptr;
    dw, dh, f: DOUBLE;
  BEGIN
    Display := al_get_current_display;
    dw := al_get_display_width (Display);
    dh := al_get_display_height (Display);
    al_identity_transform (Projection);
    al_translate_transform_3d (Projection, 0, 0, -1);
    f := tan (Example.Camera.VerticalFieldOfView / 2);
    al_perspective_transform (
      Projection, -1 * dw / dh * f, f,
      1,
      f * dw / dh, -f, 1000
    );
    al_use_projection_transform (Projection)
  END;



(* Adds a new vertex to our scene. *)
  PROCEDURE AddVertex (x, y, z: DOUBLE; Color: ALLEGRO_COLOR);
  BEGIN
    IF Example.n >= Length (Example.v) THEN
      Setlength (Example.v, (Length (Example.v) + 1) * 2);
    Example.v[Example.n].x := x;
    Example.v[Example.n].y := y;
    Example.v[Example.n].z := z;
    Example.v[Example.n].color := Color;
    INC (Example.n);
  END;



(* Adds two triangles (6 vertices) to the scene. *)
  PROCEDURE AddQuad
    (x, y, z, ux, uy, uz, vx, vy, vz: DOUBLE; c1, c2: ALLEGRO_COLOR);
  BEGIN
    AddVertex (x, y, z, c1);
    AddVertex (x + ux, y + uy, z + uz, c1);
    AddVertex (x + vx, y + vy, z + vz, c2);
    AddVertex (x + vx, y + vy, z + vz, c2);
    AddVertex (x + ux, y + uy, z + uz, c1);
    AddVertex (x + ux + vx, y + uy + vy, z + uz + vz, c2)
  END;



(* Create a checkerboard made from colored quads. *)
  PROCEDURE AddCheckerboard;
  VAR
    x, y: INTEGER;
    Color: ALLEGRO_COLOR;
    px, py, pz: DOUBLE;
  BEGIN
    FOR y := 0 TO 19 DO
      FOR x := 0 TO 19 DO
      BEGIN
        px := x - 20 * 0.5;
        py := 0.2;
        pz := y - 20 * 0.5;
        IF ((x + y) AND 1) = 0 THEN
          Color := al_color_name ('yellow')
        ELSE BEGIN
          py := py - 0.1;
          Color := al_color_name ('green')
        END;
        AddQuad (px, py, pz, 1, 0, 0, 0, 0, 1, Color, Color)
      END
  END;



(* Create a skybox. This is simply 5 quads with a fixed distance to the
   camera. *)
  PROCEDURE AddSkybox;
  VAR
    p: RVector;
    c1, c2, c3: ALLEGRO_COLOR;
  BEGIN
    p := Example.Camera.Position;
    c1 := al_color_name ('black');
    c2 := al_color_name ('blue');
    c3 := al_color_name ('white');

  { Back skybox wall. }
    AddQuad (p.x - 50, 0, p.z - 50, 100, 0, 0, 0, 50, 0, c1, c2);
  { Front skybox wall. }
    AddQuad (p.x - 50, 0, p.z + 50, 100, 0, 0, 0, 50, 0, c1, c2);
  { Left skybox wall. }
    AddQuad (p.x - 50, 0, p.z - 50, 0, 0, 100, 0, 50, 0, c1, c2);
  { Right skybox wall. }
    AddQuad (p.x + 50, 0, p.z - 50, 0, 0, 100, 0, 50, 0, c1, c2);

  { Top of skybox. }
    AddVertex (p.x - 50, 50, p.z - 50, c2);
    AddVertex (p.x + 50, 50, p.z - 50, c2);
    AddVertex (p.x, 50, p.z, c3);

    AddVertex (p.x + 50, 50, p.z - 50, c2);
    AddVertex (p.x + 50, 50, p.z + 50, c2);
    AddVertex (p.x, 50, p.z, c3);

    AddVertex (p.x + 50, 50, p.z + 50, c2);
    AddVertex (p.x - 50, 50, p.z + 50, c2);
    AddVertex (p.x, 50, p.z, c3);

    AddVertex (p.x - 50, 50, p.z + 50, c2);
    AddVertex (p.x - 50, 50, p.z - 50, c2);
    AddVertex (p.x, 50, p.z, c3)
  END;



  PROCEDURE DrawScene;
  VAR
    Projection, t: ALLEGRO_TRANSFORM;
    Back, Front: ALLEGRO_COLOR;
    th: INTEGER;
    Pitch, Yaw, Roll: DOUBLE;
  BEGIN
  { We save Allegro's projection so we can restore it for drawing text. }
    Projection := al_get_current_projection_transform^;
    Back := al_color_name ('black');
    Front := al_color_name ('white');

    Setup3DProjection;
    al_clear_to_color (Back);

  { We use a depth buffer. }
    al_set_render_state (ALLEGRO_DEPTH_TEST, 1);
    al_clear_depth_buffer (1);

  { Recreate the entire scene geometry - this is only a very small example
    so this is fine. }
    Example.n := 0;
    AddCheckerboard;
    AddSkybox;

  { Construct a transform corresponding to our camera. This is an inverse
    translation by the camera position, followed by an inverse rotation
    from the camera orientation. }
    al_build_camera_transform (t,
      Example.Camera.Position.x, Example.Camera.Position.y, Example.Camera.Position.z,
      Example.Camera.Position.x - Example.Camera.zAxis.x,
      Example.Camera.Position.y - Example.Camera.zAxis.y,
      Example.Camera.Position.z - Example.Camera.zAxis.z,
      Example.Camera.yAxis.x, Example.Camera.yAxis.y, Example.Camera.yAxis.z);
    al_use_transform (t);
    al_draw_prim (Example.v, NIL, NIL, 0, Example.n, ALLEGRO_PRIM_TRIANGLE_LIST);

  { Restore projection. }
    al_identity_transform (t);
    al_use_transform (t);
    al_use_projection_transform (Projection);

  { Draw some text. }
    th := al_get_font_line_height (Example.font);
    al_draw_text (
      Example.Font, Front, 0, th * 0, 0,
      Format (
        'look: %3.1f/%3.1f/%3.1f (change with left mouse button and drag)',
        [
          -Example.Camera.zAxis.x,
          -Example.Camera.zAxis.y,
          -Example.Camera.zAxis.z
        ]
      )
    );
    Pitch := GetPitch (Example.Camera) * 180 / pi;
    Yaw   := GetYaw (Example.Camera) * 180 / pi;
    Roll  := GetRoll (Example.Camera) * 180 / pi;
    al_draw_text (
      Example.Font, Front, 0, th * 1, 0,
      Format (
        'pitch: %4.0f yaw: %4.0f roll: %4.0f',
        [ Pitch, Yaw, Roll]
      )
    );
    al_draw_text (
      Example.Font, Front, 0, th * 2, 0,
      Format (
        'vertical field of view: %3.1f (change with Z/X)',
        [Example.Camera.VerticalFieldOfView * 180 / pi]
      )
    );
    al_draw_text
      (Example.Font, Front, 0, th * 3, 0, 'move with WASD or cursor');
    al_draw_text (
      Example.Font, Front, 0, th * 4, 0,
      Format (
        'control style: %s (space to change)',
        [Example.ControlsNames[Example.Controls]]
      )
    )
  END;



  PROCEDURE SetupScene;
  BEGIN
    Example.camera.xAxis.x := 1;
    Example.camera.yAxis.y := 1;
    Example.camera.zAxis.z := 1;
    Example.camera.Position.y := 2;
    Example.camera.VerticalFieldOfView := 60 * pi / 180;

    Example.MouseLookSpeed := 0.03;
    Example.MovementSpeed := 0.05;

    Example.ControlsNames[0] := 'FPS';
    Example.ControlsNames[1] := 'airplane';
    Example.ControlsNames[2] := 'spaceship';

    Example.Font := al_create_builtin_font
  END;



  PROCEDURE HandleInput;
  VAR
    x, y, xy, m, Roll: DOUBLE;
    up: RVector;
  BEGIN
    x := 0; y := 0;

    IF Example.Key[ALLEGRO_KEY_A] OR Example.Key[ALLEGRO_KEY_LEFT] THEN x := -1;
    IF Example.Key[ALLEGRO_KEY_S] OR Example.Key[ALLEGRO_KEY_DOWN] THEN y := -1;
    IF Example.Key[ALLEGRO_KEY_D] OR Example.Key[ALLEGRO_KEY_RIGHT] THEN x := 1;
    IF Example.Key[ALLEGRO_KEY_W] OR Example.Key[ALLEGRO_KEY_UP] THEN y := 1;

  { Change field of view with Z/X. }
    IF Example.Key[ALLEGRO_KEY_Z] THEN
    BEGIN
      m := 20 * pi / 180;
      Example.Camera.VerticalFieldOfView :=
        Example.Camera.VerticalFieldOfView - 0.01;
      IF Example.Camera.VerticalFieldOfView < m THEN
        Example.Camera.VerticalFieldOfView := m
    END;
    IF Example.Key[ALLEGRO_KEY_X] THEN
    BEGIN
      m := 120 * pi / 180;
      Example.Camera.VerticalFieldOfView :=
        Example.Camera.VerticalFieldOfView + 0.01;
      IF Example.Camera.VerticalFieldOfView > m THEN
        Example.Camera.VerticalFieldOfView := m
    END;

  { In FPS style, always move the camera to height 2. }
    IF Example.Controls = 0 THEN
    BEGIN
      IF Example.Camera.Position.y > 2 THEN
        Example.Camera.Position.y := Example.Camera.Position.y - 0.1;
      IF Example.Camera.Position.y < 2 THEN
        Example.Camera.Position.y := 2
    END;

  { Set the roll (leaning) angle to 0 if not in airplane style. }
    IF (Example.Controls = 0) OR (Example.Controls = 2) THEN
    BEGIN
      Roll := GetRoll (Example.camera);
      RotateCameraAroundAxis (Example.Camera, Example.Camera.zAxis, Roll / 60)
    END;

  { Move the camera, either freely or along the ground. }
    xy := SQRT (SQR (x) + SQR (y));
    IF xy > 0 THEN
    BEGIN
      x := x / xy;
      y := y / xy;
      IF Example.Controls = 0 THEN
        MoveCameraAlongGround
          (Example.Camera, Example.MovementSpeed * x, Example.MovementSpeed * y)
      ELSE IF (Example.Controls = 1) OR (Example.Controls = 2) THEN
        MoveCameraAlongDirection
          (Example.Camera, Example.MovementSpeed * x, Example.MovementSpeed * y)
    END;

  { Rotate the camera, either freely or around world up only. }
    IF Example.Button[1] THEN
    BEGIN
      IF (Example.controls = 0) OR (Example.Controls = 2) THEN
      BEGIN
        up.x := 0; up.y := 1; up.z := 0;
        RotateCameraAroundAxis (
          Example.Camera, Example.Camera.xAxis,
          -Example.MouseLookSpeed * Example.MouseDy
        );
        RotateCameraAroundAxis (
          Example.Camera, up,
          -Example.MouseLookSpeed * Example.MouseDx
        )
      END
      ELSE IF Example.Controls = 1 THEN
      BEGIN
        RotateCameraAroundAxis (
          Example.Camera, Example.Camera.xAxis,
          -Example.MouseLookSpeed * Example.MouseDy
        );
        RotateCameraAroundAxis (
          Example.Camera, Example.Camera.zAxis,
          -Example.MouseLookSpeed * Example.MouseDx
        )
      END
    END
  END;



VAR
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  i: INTEGER;
  Redraw, EndExample: BOOLEAN;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  al_init_font_addon;
  al_init_primitives_addon;
  InitPlatformSpecific;
  al_install_keyboard;
  al_install_mouse;

  al_set_new_display_option (ALLEGRO_SAMPLE_BUFFERS, 1, ALLEGRO_SUGGEST);
  al_set_new_display_option (ALLEGRO_SAMPLES, 8, ALLEGRO_SUGGEST);
  al_set_new_display_option (ALLEGRO_DEPTH_SIZE, 16, ALLEGRO_SUGGEST);
  al_set_new_display_flags (ALLEGRO_RESIZABLE);
  Display := al_create_display (640, 360);
  IF Display = NIL THEN AbortExample ('Error creating display');

  Timer := al_create_timer (1.0 / 60);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));

  SetupScene;
  Redraw := TRUE;

  al_start_timer (Timer);
  EndExample := FALSE;
  REPEAT
    al_wait_for_event (Queue, Event);
    CASE Event._type OF
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndExample := TRUE;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      al_acknowledge_resize (Display);
    ALLEGRO_EVENT_KEY_DOWN:
      BEGIN
        IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
          EndExample := TRUE
        ELSE IF Event.keyboard.keycode = ALLEGRO_KEY_SPACE THEN
          Example.Controls := (Example.Controls + 1) MOD 3;
        Example.Key[Event.keyboard.keycode] := TRUE;
        Example.Keystate[Event.keyboard.keycode] := TRUE
      END;
    ALLEGRO_EVENT_KEY_UP:
    { In case a key gets pressed and immediately released, we will still
      have set Example.key so it is not lost. }
      Example.Keystate[Event.keyboard.keycode] := FALSE;
    ALLEGRO_EVENT_TIMER:
      BEGIN
        HandleInput;
        Redraw := TRUE;

      { Reset keyboard state for keys not held down anymore. }
        FOR i := LOW (Example.Key) TO HIGH (Example.Key) DO
          IF NOT Example.Keystate[i] THEN
            Example.Key[i] := FALSE;
         Example.MouseDx := 0;
         Example.MouseDy := 0;
       END;
     ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
       Example.Button[1 {Event.mouse.button}] := TRUE;
     ALLEGRO_EVENT_MOUSE_BUTTON_UP:
       Example.Button[1 {Event.mouse.button}] := FALSE;
     ALLEGRO_EVENT_MOUSE_AXES:
       BEGIN
         INC (Example.MouseDx, Event.mouse.dx);
         INC (Example.MouseDy, Event.mouse.dy)
       END;
     END;

     IF Redraw AND al_is_event_queue_empty (Queue) THEN
     BEGIN
       DrawScene;

       al_flip_display;
       Redraw := FALSE
     END
  UNTIL EndExample
END.
