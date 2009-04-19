PROGRAM exmasked;
(* Example of Allegro.pas+OpenGL masked texture routines *)

USES
  Gl, { OpenGL support. }
  sysutils,
  allegro, alfile,
  algl,     { Allegro.pas+OpenGL basic interface. }
  algraph,  { Graphic mode configuration. }
  almouse;  { Mouse input. }



{$include '../running.inc'}



VAR
  Chrono: LONGINT = 0;
  RenderTime: LONGINT;



(* Timer control *)
PROCEDURE TheTimer; CDECL;
BEGIN
  INC (Chrono);
END;



VAR
  Texture: AL_BITMAPptr;
  Dat: AL_DATAFILEptr;
  Tex: ARRAY [0..10] OF GLuint;
  i, j: INTEGER;
  FileName: STRING;

(* Program starts here. *)
BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  IF NOT al_gl_init THEN
  BEGIN
    al_message ('Can''t initialize Allegro''s OpenGL interface!');
    EXIT;
  END;
  al_install_keyboard;
  IF al_install_mouse < 0 THEN
  BEGIN
    al_message ('You need a mouse to test this example.');
    EXIT;
  END;

  al_gl_clear_settings;
  al_gl_set (AL_GL_COLOR_DEPTH, 32);
  al_gl_set (AL_GL_DOUBLEBUFFER, 1);
  al_gl_set (AL_GL_Z_DEPTH, 24);
  al_gl_set_boolean (AL_GL_WINDOWED, true);
  al_gl_set (AL_GL_RENDERMETHOD, 1);
  al_gl_set (AL_GL_SUGGEST, AL_GL_COLOR_DEPTH OR AL_GL_DOUBLEBUFFER OR
             AL_GL_RENDERMETHOD OR AL_GL_Z_DEPTH OR AL_GL_WINDOWED);

  IF NOT al_set_gfx_mode (AL_GFX_OPENGL, 640, 480, 0, 0) THEN
  BEGIN
    al_message ('Error setting OpenGL graphics mode.');
    EXIT
  END;

  al_install_timer;

  al_install_int (@TheTimer, 10);

  FileName := ExtractFilePath (ParamStr (0)) + '../running.dat';
  Dat := al_load_datafile (FileName);

  IF dat = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message('Unable to load running.dat!');
    EXIT
  END;

{ Convert running ball thingie }
  FOR j := 0 TO 9 DO
  BEGIN
    Texture := Dat^[FRAME_01 + j].dat;
    Tex[j] := al_gl_make_texture (AL_GL_TEXTURE_MASKED OR AL_GL_TEXTURE_MIPMAP,
				  Texture, -1);
  END;
{ Convert Mysha }
  FileName := ExtractFilePath (ParamStr (0)) + '../misha.pcx';
  Texture := al_load_bitmap (FileName, NIL);
  IF Texture = NIL THEN
  BEGIN
    al_unload_datafile (Dat);
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Unable to load mysha.pcx');
    EXIT
  END;
  Tex[10] := al_gl_make_texture (AL_GL_TEXTURE_MIPMAP, Texture, -1);

{ Now display everything }

{ Setup OpenGL like we want }
  glEnable (GL_TEXTURE_2D);

{ Skip pixels which alpha channel is lower than 0.5 }
  glAlphaFunc (GL_GREATER, 0.5);

  glShadeModel (GL_FLAT);
  glPolygonMode (GL_FRONT, GL_FILL);

  glViewport (0, 0, AL_SCREEN_W, AL_SCREEN_H);
  glMatrixMode (GL_PROJECTION);
  glLoadIdentity;
  glFrustum (-1.0, 1.0, -1.0, 1.0, 1, 60.0);

{ Set culling mode - not that we have anything to cull }
  glEnable (GL_CULL_FACE);
  glFrontFace (GL_CCW);

  glMatrixMode (GL_MODELVIEW);


  al_clear_keybuf;

  RenderTime := Chrono;
  REPEAT
    glClear (GL_COLOR_BUFFER_BIT OR GL_DEPTH_BUFFER_BIT);

    glLoadIdentity;
    glTranslatef (-10.0, -10.0, -10.0);

  { Draw BG }
    glBindTexture (GL_TEXTURE_2D, Tex[10]);
    glBegin (GL_QUADS);
      glTexCoord2f (0, 0);
      glVertex2f (0, 0);

      glTexCoord2f (1, 0);
      glVertex2f (20, 0);

      glTexCoord2f (1, 1);
      glVertex2f (20, 20);

      glTexCoord2f (0, 1);
      glVertex2f (0, 20);
    glEnd;

    i := (Chrono DIV 10) MOD 10;

  { Draw running ball thingie }

  { Enable removing of thingie's transparent pixels }
    glEnable (GL_ALPHA_TEST);
    glLoadIdentity;
    IF (AL_SCREEN_W <> 0) AND (AL_SCREEN_H <> 0) THEN
    BEGIN
      glTranslatef (al_mouse_x *  20.0 / AL_SCREEN_W - 10.0,
		    al_mouse_y * -20.0 / AL_SCREEN_H + 10.0, -9.99);
    END;
    glRotatef (Chrono, 0, 0, 1.0);
    glBindTexture (GL_TEXTURE_2D, tex[i]);

    glBegin (GL_QUADS);
      glTexCoord2f (0, 0);
      glVertex2f (-1.0, -1.0);

      glTexCoord2f (1, 0);
      glVertex2f (1.0, -1.0);

      glTexCoord2f (1, 1);
      glVertex2f (1.0, 1.0);

      glTexCoord2f (0, 1);
      glVertex2f (-1.0, 1.0);
    glEnd;

    glDisable (GL_ALPHA_TEST); { Disable removing of transparent pixels }

    al_gl_flip;

    INC (RenderTime);
    WHILE (NOT al_keypressed) AND (RenderTime > Chrono) DO
      al_rest (2);

  UNTIL al_keypressed;

  al_destroy_bitmap (Texture); { misha.pcx }
  al_unload_datafile (Dat);    { runner.dat }
  al_gl_exit;
  al_exit;
END.
