PROGRAM exbasic;
(* Basic example of Allegro.pas+OpenGL.

   Based in a NeHe's examble. *)

USES
  Gl, { OpenGL support. }
  sysutils,
  allegro,
  algl;     { Allegro.pas+OpenGL basic interface. }



VAR
  Chrono: LONGINT = 0;



(* Timer control *)
PROCEDURE TheTimer; CDECL;
BEGIN
  INC (Chrono);
END;



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
WriteLn ('Allegro inicializado');
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
  al_gl_set (AL_GL_RENDERMETHOD, 1);
  al_gl_set (AL_GL_SUGGEST, AL_GL_COLOR_DEPTH OR AL_GL_DOUBLEBUFFER OR
             AL_GL_RENDERMETHOD OR AL_GL_Z_DEPTH);

WriteLn ('OpenGL configurado');
  IF NOT al_set_gfx_mode (AL_GFX_OPENGL_WINDOWED, 640, 480, 0, 0) THEN
//    IF NOT al_set_gfx_mode (AL_GFX_OPENGL_FULLSCREEN, 640, 480, 0, 0) THEN
    BEGIN
      al_message ('Error setting OpenGL graphics mode.');
      EXIT
    END;
WriteLn ('Modo gráfico establecido');

  al_install_timer;

  al_install_int (@TheTimer, 10);

{ Now display everything }

{ Setup OpenGL like we want }

{ Set culling mode - not that we have anything to cull }
  glEnable (GL_CULL_FACE);
  glFrontFace (GL_CCW);

  glMatrixMode (GL_MODELVIEW);


  al_clear_keybuf;

WriteLn ('Bucle...');
  REPEAT
    WHILE (NOT al_keypressed) DO
      al_rest (2);
  UNTIL al_keypressed;

WriteLn ('Nos vamos');
  al_gl_exit;
  al_exit;
END.
