program ex_gldepth;
(* Shows how to set and use an OpenGL display.
 *
 * Use arrow keys to rotate, PgUp/PgDown to move closer/farther away.
 *)
(*
  Copyright (c) 2012-2024 Guillermo Martínez J.

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
    Allegro5, al5image, al5font, al5opengl,
  {$IFDEF FPC}
    GL,
  {$ELSE}
    OpenGL,
  {$ENDIF}
    sysutils;

  const
  (* Window size. *)
    wWidth = 640; wHeight = 480;
  (* Texture size. *)
    TextureWidth = 128;
    TextureHeight = 128;
  (* Speed animation. *)
    AngleSpeed = 5.0;
    DistSpeed = 1.0;

  type
  (* Stores camera information. *)
    TCamera = record
    (* Camera angle. *)
      xAngle, yAngle, zAngle: Double;
    (* Camera distance. *)
      Dist: Double
    end;

  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;
    Redraw, Terminated: Boolean;
  (* Stores if key is pressed. *)
    KeyStatus: array [0..ALLEGRO_KEY_MAX] of Boolean;
  (* Camera. *)
    Camera: TCamera = (
      xAngle:  0.0;
      yAngle:  0.0;
      zAngle:  0.0;
      Dist  : 20.0;
    );
  (* Texture. *)
    BitmapTexture: ALLEGRO_BITMAPptr = Nil;
    Texture: GLuint;



(* Program initialization. *)
  function Initialize: Boolean;

    function InitOpenGLDisplay: Boolean;
    begin
    { Create display. }
      al_set_new_display_flags (ALLEGRO_OPENGL);
      al_set_new_display_option (ALLEGRO_DEPTH_SIZE, 16, ALLEGRO_SUGGEST);
      Window := al_create_display (wWidth, wHeight);
      if not Assigned (Window) then
      begin
        WriteLn ('Can''t create OpenGL context.');
        Exit (False)
      end;
    { OpenGL initialization. }
      glEnable (GL_DEPTH_TEST);
      glDisable (GL_CULL_FACE);

      InitOpenGLDisplay := True
    end;

    function SetupTextures: Boolean;
    var
      lFont: ALLEGRO_FONTptr;
      lBitmap: ALLEGRO_BITMAPptr;
      lTextColor: ALLEGRO_COLOR;
      lDepth: Integer;
    begin
    { Load data. }
      lFont := al_load_font ('data/fixed_font.tga', 0, 0);
      if not Assigned (lFont) then
      begin
        ErrorMessage ('Can''t load "data/fixed_font.tga".');
        Exit (False)
      end;
      lBitmap := al_load_bitmap ('data/mysha.pcx');
      if not Assigned (lBitmap) then
      begin
        al_destroy_font (lFont);
        ErrorMessage ('Can''t load "data/mysha.pcx".');
        Exit (False)
      end;
    { Create texture. }
      BitmapTexture := al_create_bitmap (TextureWidth, TextureHeight);
      if not Assigned (BitmapTexture) then
      begin
        al_destroy_font (lFont);
        al_destroy_bitmap (lBitmap);
        ErrorMessage ('Can''t create OpenGL textures.');
        Exit (False)
      end;
      begin
        al_set_target_bitmap (BitmapTexture);
        al_draw_scaled_bitmap (
          lBitmap,
          0, 0, al_get_bitmap_width (BitmapTexture), al_get_bitmap_height (BitmapTexture),
          0, 0, TextureWidth, TextureHeight,
          0
        );
        lTextColor := al_map_rgb (255, 0, 0);
        lDepth := al_get_display_option (Window, ALLEGRO_DEPTH_SIZE);
        if lDepth = 0 then
          al_draw_text (lFont, lTextColor, 0, 5, 0, 'No Z-buffer!')
        else
          al_draw_textf (lFont, lTextColor, 0, 5, 0, 'Z-buffer: %d bits', [lDepth]);
        al_set_target_backbuffer (Window);
      end;
    { Upload texture. }
      glEnable (GL_TEXTURE_2D);
      glTexEnvi (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
      Texture := al_get_opengl_texture (BitmapTexture);
    { Release resources. }
      al_destroy_bitmap (lBitmap);
      al_destroy_font (lFont);

      SetupTextures := True
    end;

  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_image_addon
    or not al_init_font_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create the timer. }
    Timer := al_create_timer (1 / 60);
    if not Assigned (Timer) then
    begin
      WriteLn ('Can''t initialize timer.');
      Exit (False)
    end;
  { Create display. }
    if not InitOpenGLDisplay then Exit (False);
    if not SetupTextures then Exit (False);
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      ErrorMessage ('Can''t initialize event queue!');
      Exit (False)
    end;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));
    al_register_event_source (EventQueue, al_get_timer_event_source (Timer));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (BitmapTexture) then al_destroy_bitmap (BitmapTexture);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;

    procedure SetCameraPosition; inline;
    begin
      glMatrixMode (GL_PROJECTION);
        glLoadIdentity;
        glFrustum (-1.0, 1.0, -1.0, 1.0, 1.0, 40.0);
        glTranslatef (0, 0, -Camera.Dist);
        glRotatef (Camera.xAngle, 1, 0, 0);
        glRotatef (Camera.yAngle, 0, 1, 0);
        glRotatef (Camera.zAngle, 0, 0, 1);
      glMatrixMode (GL_MODELVIEW)
    end;

    procedure RenderPyramid; inline;
    begin
      glLoadIdentity;
      glTranslatef (-2.5, 0.0, 0.0);
      glRotatef (-30, 1.0, 0.0, 0.0);
      glRotatef (30, 0.0, 1.0, 0.0);
      glRotatef (30, 0.0, 0.0, 1.0);
    { Draw the sides of the three-sided pyramid. }
      glColor3f (1.0, 0.0, 1.0);
      glEnable (GL_TEXTURE_2D);
      glBindTexture (GL_TEXTURE_2D, Texture);
      glBegin (GL_TRIANGLE_FAN);
        glTexCoord2f (0, 0); glVertex3d ( 0,  4,  0);
        glTexCoord2f (1, 0); glVertex3d ( 0, -4, -4);
        glTexCoord2f (1, 1); glVertex3d (-4, -4,  4);
        glTexCoord2f (0, 1); glVertex3d ( 4, -4,  4);
        glTexCoord2f (1, 0); glVertex3d ( 0, -4, -4);
      glEnd;
    { Draw the base of the pyramid. }
      glColor3f (0.0, 1.0, 1.0);
      glBegin (GL_TRIANGLES);
        glTexCoord2f (1, 0); glVertex3d ( 0, -4, -4);
        glTexCoord2f (0, 1); glVertex3d ( 4, -4,  4);
        glTexCoord2f (1, 1); glVertex3d (-4, -4,  4);
      glEnd;
    end;

    procedure RenderCube; inline;
    begin
      glLoadIdentity;
      glTranslatef (2.5, 0.0, 0.0);
      glRotatef (45, 1.0, 0.0, 0.0);
      glRotatef (45, 0.0, 1.0, 0.0);
      glRotatef (45, 0.0, 0.0, 1.0);
    { Draw the sides of the cube. }
      glColor3f (0.0, 1.0, 0.0);
      glDisable (GL_TEXTURE_2D);
      glBegin (GL_QUAD_STRIP);
        glVertex3d ( 3,  3, -3);
        glVertex3d ( 3, -3, -3);
        glVertex3d (-3,  3, -3);
        glVertex3d (-3, -3, -3);
        glVertex3d (-3,  3,  3);
        glVertex3d (-3, -3,  3);
        glVertex3d ( 3,  3,  3);
        glVertex3d ( 3, -3,  3);
        glVertex3d ( 3,  3, -3);
        glVertex3d ( 3, -3, -3);
      glEnd;
    { Draw the top of the cube. }
      glColor3f (0.0, 0.0, 1.0);
      glBegin (GL_QUADS);
        glVertex3d (-3, -3, -3);
        glVertex3d ( 3, -3, -3);
        glVertex3d ( 3, -3,  3);
        glVertex3d (-3, -3,  3);
      glEnd;
    { Bottom is texture-mapped. }
      glEnable (GL_TEXTURE_2D);
      glBindTexture (GL_TEXTURE_2D, Texture);
      glBegin (GL_QUADS);
        glTexCoord2f (0, 0); glVertex3d (-3,  3, -3);
        glTexCoord2f (1, 0); glVertex3d (-3,  3,  3);
        glTexCoord2f (1, 1); glVertex3d ( 3,  3,  3);
        glTexCoord2f (0, 1); glVertex3d ( 3,  3, -3);
      glEnd
    end;

  begin
    glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    SetCameraPosition;
    RenderPyramid;
    RenderCube;
    al_flip_display
  end;



(* Update camera position and rotation. *)
  procedure UpdateCamera; inline;
  begin
    if KeyStatus[ALLEGRO_KEY_LEFT] then
      Camera.yAngle := Camera.yAngle + AngleSpeed;
    if KeyStatus[ALLEGRO_KEY_RIGHT] then
      Camera.yAngle := Camera.yAngle - AngleSpeed;

    if KeyStatus[ALLEGRO_KEY_UP] then
      Camera.xAngle := Camera.xAngle + AngleSpeed;
    if KeyStatus[ALLEGRO_KEY_DOWN] then
      Camera.xAngle := Camera.xAngle - AngleSpeed;

    if KeyStatus[ALLEGRO_KEY_PGUP] then
      Camera.Dist := Camera.Dist - DistSpeed;
    if KeyStatus[ALLEGRO_KEY_PGDN] then
      Camera.Dist := Camera.Dist + DistSpeed
  end;

begin
  if not Initialize then Exit;
{ "Game loop". }
  al_start_timer (Timer);
  Redraw := False;
  Terminated := False;
  repeat
  { Screen update. }
    if Redraw and al_is_event_queue_empty (EventQueue) then
    begin
      UpdateScreen;
      Redraw := False
    end;
  { Check events. }
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          Terminated := True;
        KeyStatus[Event.keyboard.keycode] := True
      end;
    ALLEGRO_EVENT_KEY_UP:
      KeyStatus[Event.keyboard.keycode] := False;
    ALLEGRO_EVENT_TIMER:
      begin
        UpdateCamera;
        Redraw := True
      end;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
