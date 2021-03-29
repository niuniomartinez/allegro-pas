PROGRAM ex_gldepth;
(* An example program showing how to set and use a depth buffer with an OpenGL
 * display.
 *
 * Use arrow keys to rotate, PgUp/PgDown to move closer/farther away.
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
    Allegro5, al5image, al5font, al5opengl,
  {$IFDEF FPC}
    GL,
  {$ELSE}
    OpenGL,
  {$ENDIF}
    sysutils;

  type
  (* Stores camera information. *)
    TCamera = record
    (* Camera angle. *)
      xAngle, yAngle, zAngle: Double;
    (* Camera distance. *)
      Dist: Double
    end;



  var
  (* Camera. *)
    Camera: TCamera = (
      xAngle:  0.0;
      yAngle:  0.0;
      zAngle:  0.0;
      Dist  : 20.0;
    );

  const
  (* Speed animation. *)
    AngleSpeed = 5.0;
    DistSpeed = 1.0;

  var
  (* Texture. *)
    Texture: GLuint;
    Bitmap: ALLEGRO_BITMAPptr = Nil;
  (* Stores if key is pressed. *)
    KeyStatus: array [0..ALLEGRO_KEY_MAX] of Boolean;

(* Sets camera position. *)
  procedure SetCameraPosition;
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



(* Process keyboard input. *)
  procedure Keyboard;
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



(* Draws the scene. *)
  procedure DrawScene;
  begin
  { Clear the RGB buffer and the depth buffer. }
    glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  { Set the modelview matrix to be the identity matrix. }
    glLoadIdentity;
  { Translate and rotate the object. }
    glTranslatef (-2.5, 0.0, 0.0);
    glRotatef (-30, 1.0, 0.0, 0.0);
    glRotatef (30, 0.0, 1.0, 0.0);
    glRotatef (30, 0.0, 0.0, 1.0);

    glColor3f (1.0, 0.0, 1.0);

  { Draw the sides of the three-sided pyramid. }
    glEnable (GL_TEXTURE_2D);
    glBindTexture (GL_TEXTURE_2D, Texture);
    glBegin (GL_TRIANGLE_FAN);
      glTexCoord2f (0, 0); glVertex3d ( 0,  4,  0);
      glTexCoord2f (1, 0); glVertex3d ( 0, -4, -4);
      glTexCoord2f (1, 1); glVertex3d (-4, -4,  4);
      glTexCoord2f (0, 1); glVertex3d ( 4, -4,  4);
      glTexCoord2f (1, 0); glVertex3d ( 0, -4, -4);
    glEnd;

    glColor3f (0.0, 1.0, 1.0);

  { Draw the base of the pyramid. }
    glBegin (GL_TRIANGLES);
      glTexCoord2f (1, 0); glVertex3d ( 0, -4, -4);
      glTexCoord2f (0, 1); glVertex3d ( 4, -4,  4);
      glTexCoord2f (1, 1); glVertex3d (-4, -4,  4);
    glEnd;


    glLoadIdentity;
    glTranslatef (2.5, 0.0, 0.0);
    glRotatef (45, 1.0, 0.0, 0.0);
    glRotatef (45, 0.0, 1.0, 0.0);
    glRotatef (45, 0.0, 0.0, 1.0);

    glColor3f (0.0, 1.0, 0.0);

    glDisable (GL_TEXTURE_2D);
  { Draw the sides of the cube. }
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

    glColor3f (0.0, 0.0, 1.0);

  { Draw the top of the cube. }
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



(* Loads and adds the textures. *)
  function SetupTextures (Display: ALLEGRO_DISPLAYptr): Boolean;
  var
    tmpBmp: ALLEGRO_BITMAPptr;
    aFont: ALLEGRO_FONTptr;
    w, h, Depth: Integer;
    TextColor: ALLEGRO_COLOR;
  begin
    aFont := al_load_font ('data/fixed_font.tga', 0, 0);
    if aFont = Nil then
      AbortExample ('Error loading `data/fixed_font.tga');

    tmpBmp := al_load_bitmap ('data/mysha.pcx');
    if tmpBmp = Nil then
      AbortExample ('Error loading ''data/mysha.pcx''');
    SetupTextures := True;
    w := 128;
    h := 128;
    Bitmap := al_create_bitmap (w, h);
    al_set_target_bitmap (Bitmap);
    al_draw_scaled_bitmap (tmpBmp,
      0, 0, al_get_bitmap_width (Bitmap), al_get_bitmap_height (Bitmap),
      0, 0, w, h, 0);

    TextColor := al_map_rgb (255, 0, 0);
    Depth := al_get_display_option (Display, ALLEGRO_DEPTH_SIZE);
    if Depth = 0 then
      al_draw_text (aFont, TextColor, 0, 5, 0, 'No Z-buffer!')
    else
      al_draw_textf (aFont, TextColor, 0, 5, 0, 'Z-buffer: %d bits', [Depth]);
    al_set_target_backbuffer (Display);
    al_destroy_bitmap (tmpBmp);
    al_destroy_font (aFont);

    glEnable (GL_TEXTURE_2D);
    glTexEnvi (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);

    Texture := al_get_opengl_texture (Bitmap)
  end;



  var
    Display: ALLEGRO_DISPLAYptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Timer: ALLEGRO_TIMERptr;
    Event: ALLEGRO_EVENT;
    DoLoop: Boolean;
begin
{ Inits Allegro. }
  if not al_init then
    AbortExample ('Error initialising Allegro.');

  al_init_image_addon;
  al_init_font_addon;
  al_install_keyboard;

  al_set_new_display_flags (ALLEGRO_OPENGL);
  al_set_new_display_option (ALLEGRO_DEPTH_SIZE, 16, ALLEGRO_SUGGEST);
  Display := al_create_display (640, 480);
  if Display = Nil then
    AbortExample ('Could not create display.');

  Timer := al_create_timer (1 / 60);

  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));

  glEnable (GL_DEPTH_TEST);
  glDisable (GL_CULL_FACE);

  if not SetupTextures (Display) then
    Exit;
  al_start_timer (Timer);

  DoLoop := True;
  while DoLoop do
  begin
    al_wait_for_event (Queue, @Event);
    case Event.ftype OF
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      DoLoop := False;
    ALLEGRO_EVENT_KEY_DOWN:
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          DoLoop := False;
        KeyStatus[Event.keyboard.keycode] := True
      end;
    ALLEGRO_EVENT_KEY_UP:
      KeyStatus[Event.keyboard.keycode] := False;
    ALLEGRO_EVENT_TIMER:
      begin
        Keyboard;
        if al_is_event_queue_empty (Queue) then
        begin
          SetCameraPosition;
          DrawScene;
          al_flip_display
        end;
      end;
    end
  end;
  al_destroy_bitmap (Bitmap)
end.
