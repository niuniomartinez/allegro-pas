PROGRAM ex_camera;
(* An example demonstrating how to use ALLEGRO_TRANSFORM to represent a 3D
 * camera.
 *)
(*
  Copyright (c) 2012-2020 Guillermo Martínez J.

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

uses
  Common,
  Allegro5, al5primitives, al5color, al5font,
  math;

const
  pi = ALLEGRO_PI;

type
  RVector = record
    x, y, z: SINGLE;
  end;

  RCamera = record
    Position: RVector;
    xAxis: RVector; { This represent the direction looking to the right. }
    yAxis: RVector; { This is the up direction. }
    zAxis: RVector; { This is the direction towards the viewer ('backwards'). }
    VerticalFieldOfView: Double; { In radians. }
  end;

  TExample = record
    Camera: RCamera;

  { Controls sensitivity }
    MouseLookSpeed,
    MovementSpeed: Double;

  { Keyboard and mouse state }
    Button: array [0..9] of Boolean;
    Key: array [0..ALLEGRO_KEY_MAX-1] of Boolean;
    KeyState: array [0..ALLEGRO_KEY_MAX-1] of Boolean;
    MouseDx, MouseDy: Integer;

  { Control scheme selection }
    controls: Integer;
    ControlsNames: array [0..2] of String;

  { the vertex data }
    n: Integer;
    v: array of ALLEGRO_VERTEX;

  { used to draw some info text }
    Font: ALLEGRO_FONTptr;
  end;

var
  Example: TExample;



(* Calculates the dot product between two vectors.  This corresponds to the
 * angle between them times their lengths. *)
  function VectorDotProduct (a, b: RVector): Double;
  begin
    VectorDotProduct := a.x * b.x + a.y * b.y + a.z * b.z
  end;



(* Returns a vector multiplied by a scalar. *)
  function VectorMul (a: RVector; s: SINGLE): RVector;
  begin
    VectorMul.x := a.x * s;
    VectorMul.y := a.y * s;
    VectorMul.z := a.z * s
  end;



(* Returns the vector norm (length). *)
  function VectorNorm (a: RVector): Double;
  begin
    VectorNorm := SQRT (VectorDotProduct (a, a))
  end;



(* Returns a normalized version of the given vector. *)
  function VectorNormalize (a: RVector): RVector;
  var
    s: Double;
  begin
    s := VectorNorm (a);
    if s = 0 then Exit (a) else Exit (VectorMul (a, 1 / s))
  end;



(* In-place add another vector to a vector. *)
  procedure VectorIadd (var a: RVector; b: RVector);
  begin
    a.x := a.x + b.x;
    a.y := a.y + b.y;
    a.z := a.z + b.z
  end;



(* Rotates the camera around the given axis. *)
  procedure RotateCameraAroundAxis
    (var c: RCamera; Axis: RVector; Radians: Double);
  var
    t: ALLEGRO_TRANSFORM;
  begin
    al_identity_transform (t);
    al_rotate_transform_3d (t, Axis.x, Axis.y, Axis.z, Radians);
    al_transform_coordinates_3d (t, c.xAxis.x, c.xAxis.y, c.xAxis.z);
    al_transform_coordinates_3d (t, c.yAxis.x, c.yAxis.y, c.yAxis.z);
    al_transform_coordinates_3d (t, c.zAxis.x, c.zAxis.y, c.zAxis.z)
  end;



(* Move the camera along its x axis and z axis (which corresponds to
   right and backwards directions). *)
  procedure MoveCameraAlongDirection
    (var Camera: RCamera; aRight, aForward: Double);
  begin
    VectorIadd (Camera.Position, VectorMul (Camera.xAxis, aRight));
    VectorIadd (Camera.Position, VectorMul (Camera.zAxis, -aForward))
  end;



(* Get a vector with y = 0 looking in the opposite direction as the camera z
   axis. If looking straight up or down returns a 0 vector instead. *)
  function  GetGroundForwardVector (const Camera: RCamera): RVector;
  var
    Move: RVector;
  begin
    Move := VectorMul (Camera.zAxis, -1);
    Move.y := 0;
    Exit (VectorNormalize (Move))
  end;



(* Get a vector with y = 0 looking in the same direction as the camera x axis.
   If looking straight up or down returns a 0 vector instead. *)
  function GetGroundRightVector (const Camera: RCamera): RVector;
  var
    Move: RVector;
  begin
    Move := Camera.xAxis;
    Move.y := 0;
    Exit (VectorNormalize (Move))
  end;



(* Like CameraMoveAlongDirection but moves the camera along the ground plane
   only. *)
  procedure MoveCameraAlongGround
    (var Camera: RCamera; aRight, aForward: Double);
  var
    f, r: RVector;
  begin
    f := GetGroundForwardVector (Camera);
    r := GetGroundRightVector (Camera);
    Camera.Position.x := Camera.Position.x + (f.x * aForward + r.x * aRight);
    Camera.position.z := Camera.Position.z + (f.z * aForward + r.z * aRight)
  end;



(* Calculate the pitch of the camera. This is the angle between the z axis
   vector and our direction vector on the y = 0 plane. *)
  function GetPitch (const Camera: RCamera): Double;
  var
    f: RVector;
  begin
    f := GetGroundForwardVector (Camera);
    Exit (arcsin (VectorDotProduct (f, Camera.yAxis)))
  end;



(* Calculate the yaw of the camera. This is basically the compass direction. *)
  function GetYaw (const Camera: RCamera): Double;
  begin
    Exit (arctan2 (Camera.zAxis.x, Camera.zAxis.z))
  end;



(* Calculate the roll of the camera. This is the angle between the x axis
   vector and its project on the y = 0 plane. *)
  function GetRoll (const Camera: RCamera): Double;
  var
    R: RVector;
  begin
    r := GetGroundRightVector (Camera);
    Exit (arcsin (VectorDotProduct (r, Camera.yAxis)))
  end;



(* Set up a perspective transform. We make the screen span
   2 vertical units (-1 to +1) with square pixel aspect and the camera's
   vertical field of view. Clip distance is always set to 1. *)
  procedure Setup3DProjection;
  var
    Projection: ALLEGRO_TRANSFORM;
    Display: ALLEGRO_DISPLAYptr;
    dw, dh, f: Double;
  begin
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
  end;



(* Adds a new vertex to our scene. *)
  procedure AddVertex (x, y, z: Double; Color: ALLEGRO_COLOR);
  begin
    if Example.n >= Length (Example.v) then
      Setlength (Example.v, (Length (Example.v) + 1) * 2);
    Example.v[Example.n].x := x;
    Example.v[Example.n].y := y;
    Example.v[Example.n].z := z;
    Example.v[Example.n].color := Color;
    Inc (Example.n);
  end;



(* Adds two triangles (6 vertices) to the scene. *)
  procedure AddQuad
    (x, y, z, ux, uy, uz, vx, vy, vz: Double; c1, c2: ALLEGRO_COLOR);
  begin
    AddVertex (x, y, z, c1);
    AddVertex (x + ux, y + uy, z + uz, c1);
    AddVertex (x + vx, y + vy, z + vz, c2);
    AddVertex (x + vx, y + vy, z + vz, c2);
    AddVertex (x + ux, y + uy, z + uz, c1);
    AddVertex (x + ux + vx, y + uy + vy, z + uz + vz, c2)
  end;



(* Create a checkerboard made from colored quads. *)
  procedure AddCheckerboard;
  var
    x, y: Integer;
    Color: ALLEGRO_COLOR;
    px, py, pz: Double;
  begin
    for y := 0 to 19 do
      for x := 0 to 19 do
      begin
        px := x - 20 * 0.5;
        py := 0.2;
        pz := y - 20 * 0.5;
        if ((x + y) and 1) = 0 then
          Color := al_color_name ('yellow')
        else begin
          py := py - 0.1;
          Color := al_color_name ('green')
        end;
        AddQuad (px, py, pz, 1, 0, 0, 0, 0, 1, Color, Color)
      end
  end;



(* Create a skybox. This is simply 5 quads with a fixed distance to the
   camera. *)
  procedure AddSkybox;
  var
    p: RVector;
    c1, c2, c3: ALLEGRO_COLOR;
  begin
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
  end;



  procedure DrawScene;
  var
    Projection, t: ALLEGRO_TRANSFORM;
    Back, Front: ALLEGRO_COLOR;
    th: Integer;
    Pitch, Yaw, Roll: Double;
  begin
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
    al_draw_prim (Example.v, Nil, 0, Example.n, ALLEGRO_PRIM_TRIANGLE_LIST);

  { Restore projection. }
    al_identity_transform (t);
    al_use_transform (t);
    al_use_projection_transform (Projection);

  { Draw some text. }
    th := al_get_font_line_height (Example.font);
    al_draw_textf (
      Example.Font, Front, 0, th * 0, 0,
      'look: %3.1f/%3.1f/%3.1f (change with left mouse button and drag)',
      [
        -Example.Camera.zAxis.x,
        -Example.Camera.zAxis.y,
        -Example.Camera.zAxis.z
      ]
    );
    Pitch := GetPitch (Example.Camera) * 180 / pi;
    Yaw   := GetYaw (Example.Camera) * 180 / pi;
    Roll  := GetRoll (Example.Camera) * 180 / pi;
    al_draw_textf (
      Example.Font, Front, 0, th * 1, 0,
      'pitch: %4.0f yaw: %4.0f roll: %4.0f',
      [ Pitch, Yaw, Roll]
    );
    al_draw_textf (
      Example.Font, Front, 0, th * 2, 0,
      'vertical field of view: %3.1f (change with Z/X)',
      [Example.Camera.VerticalFieldOfView * 180 / pi]
    );
    al_draw_text
      (Example.Font, Front, 0, th * 3, 0, 'move with WASD or cursor');
    al_draw_textf (
      Example.Font, Front, 0, th * 4, 0,
      'control style: %s (space to change)',
      [Example.ControlsNames[Example.Controls]]
    )
  end;



  procedure SetupScene;
  begin
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
  end;



  procedure HandleInput;
  var
    x, y, xy, m, Roll: Double;
    up: RVector;
  begin
    x := 0; y := 0;

    if Example.Key[ALLEGRO_KEY_A] or Example.Key[ALLEGRO_KEY_LEFT] then x := -1;
    if Example.Key[ALLEGRO_KEY_S] or Example.Key[ALLEGRO_KEY_DOWN] then y := -1;
    if Example.Key[ALLEGRO_KEY_D] or Example.Key[ALLEGRO_KEY_RIGHT] then x := 1;
    if Example.Key[ALLEGRO_KEY_W] or Example.Key[ALLEGRO_KEY_UP] then y := 1;

  { Change field of view with Z/X. }
    if Example.Key[ALLEGRO_KEY_Z] then
    begin
      m := 20 * pi / 180;
      Example.Camera.VerticalFieldOfView :=
        Example.Camera.VerticalFieldOfView - 0.01;
      if Example.Camera.VerticalFieldOfView < m then
        Example.Camera.VerticalFieldOfView := m
    end;
    if Example.Key[ALLEGRO_KEY_X] then
    begin
      m := 120 * pi / 180;
      Example.Camera.VerticalFieldOfView :=
        Example.Camera.VerticalFieldOfView + 0.01;
      if Example.Camera.VerticalFieldOfView > m then
        Example.Camera.VerticalFieldOfView := m
    end;

  { In FPS style, always move the camera to height 2. }
    if Example.Controls = 0 then
    begin
      if Example.Camera.Position.y > 2 then
        Example.Camera.Position.y := Example.Camera.Position.y - 0.1;
      if Example.Camera.Position.y < 2 then
        Example.Camera.Position.y := 2
    end;

  { Set the roll (leaning) angle to 0 if not in airplane style. }
    if (Example.Controls = 0) or (Example.Controls = 2) then
    begin
      Roll := GetRoll (Example.camera);
      RotateCameraAroundAxis (Example.Camera, Example.Camera.zAxis, Roll / 60)
    end;

  { Move the camera, either freely or along the ground. }
    xy := SQRT (SQR (x) + SQR (y));
    if xy > 0 then
    begin
      x := x / xy;
      y := y / xy;
      if Example.Controls = 0 then
        MoveCameraAlongGround
          (Example.Camera, Example.MovementSpeed * x, Example.MovementSpeed * y)
      else if (Example.Controls = 1) or (Example.Controls = 2) then
        MoveCameraAlongDirection
          (Example.Camera, Example.MovementSpeed * x, Example.MovementSpeed * y)
    end;

  { Rotate the camera, either freely or around world up only. }
    if Example.Button[1] then
    begin
      if (Example.controls = 0) or (Example.Controls = 2) then
      begin
        up.x := 0; up.y := 1; up.z := 0;
        RotateCameraAroundAxis (
          Example.Camera, Example.Camera.xAxis,
          -Example.MouseLookSpeed * Example.MouseDy
        );
        RotateCameraAroundAxis (
          Example.Camera, up,
          -Example.MouseLookSpeed * Example.MouseDx
        )
      end
      else if Example.Controls = 1 then
      begin
        RotateCameraAroundAxis (
          Example.Camera, Example.Camera.xAxis,
          -Example.MouseLookSpeed * Example.MouseDy
        );
        RotateCameraAroundAxis (
          Example.Camera, Example.Camera.zAxis,
          -Example.MouseLookSpeed * Example.MouseDx
        )
      end
    end
  end;



var
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
  Queue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  i: Integer;
  Redraw, EndExample: Boolean;
begin
  if not al_init then AbortExample ('Could not init Allegro.');
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
  if Display = Nil then AbortExample ('Error creating display');

  Timer := al_create_timer (1.0 / 60);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_mouse_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));

  SetupScene;
  Redraw := True;

  al_start_timer (Timer);
  EndExample := False;
  repeat
    al_wait_for_event (Queue, @Event);
    case Event.ftype OF
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndExample := True;
    ALLEGRO_EVENT_DISPLAY_RESIZE:
      al_acknowledge_resize (Display);
    ALLEGRO_EVENT_KEY_DOWN:
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          EndExample := True
        else if Event.keyboard.keycode = ALLEGRO_KEY_SPACE then
{ Ignore message:
ex_camera.pas(592,32) Warning: Variable "Example" of a managed type does not seem to be initialized
  It is initialized at "Init". }
          Example.Controls := (Example.Controls + 1) mod 3;
        Example.Key[Event.keyboard.keycode] := True;
        Example.Keystate[Event.keyboard.keycode] := True
      end;
    ALLEGRO_EVENT_KEY_UP:
    { In case a key gets pressed and immediately released, we will still
      have set Example.key so it is not lost. }
      Example.Keystate[Event.keyboard.keycode] := False;
    ALLEGRO_EVENT_TIMER:
      begin
        HandleInput;
        Redraw := True;

      { Reset keyboard state for keys not held down anymore. }
        for i := Low (Example.Key) to High (Example.Key) do
          if not Example.Keystate[i] then
            Example.Key[i] := False;
         Example.MouseDx := 0;
         Example.MouseDy := 0;
       end;
     ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
       Example.Button[1 {Event.mouse.button}] := True;
     ALLEGRO_EVENT_MOUSE_BUTTON_UP:
       Example.Button[1 {Event.mouse.button}] := False;
     ALLEGRO_EVENT_MOUSE_AXES:
       begin
         Inc (Example.MouseDx, Event.mouse.dx);
         Inc (Example.MouseDy, Event.mouse.dy)
       end;
     end;

     if Redraw and al_is_event_queue_empty (Queue) then
     begin
       DrawScene;

       al_flip_display;
       Redraw := False
     end
  until EndExample
end.
