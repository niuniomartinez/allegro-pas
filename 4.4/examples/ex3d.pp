PROGRAM ex3d2;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

   Example for Allegro.pas that displays a 3D cube and rotates it using
   keyboard or mouse.

   by Ñuño Martínez <niunio(at)users.sourceforge.net> *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  cube,
  allegro, algui, al3d;



VAR
(* Graphics mode selection. *)
  c, w, h, bpp: LONGINT;
  Buffer: AL_BITMAPptr;
(* Input. *)
  Key: LONGINT;
BEGIN { The program starts here. }

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;

{ Set up the keyboard handler. }
  al_install_keyboard;
  al_install_mouse;
  al_install_timer;

{ Set a graphics mode. }
  IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
  { Show an error message. }
    al_message (al_error);
  { Shutdown Allegro.  You should do it because it isn't automatic. }
    al_exit;
    EXIT;
  END;
  al_set_palette (al_desktop_palette);

  c := AL_GFX_AUTODETECT;
  w := AL_SCREEN_W; h := AL_SCREEN_H; bpp := al_bitmap_color_depth (al_screen);
  IF NOT al_gfx_mode_select_ex (c, w, h, bpp) THEN
  BEGIN
    al_exit;
    EXIT;
  END;

  al_set_color_depth (bpp);
  IF NOT al_set_gfx_mode (c, w, h, 0, 0) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
  { Show an error message. }
    al_message (al_error);
  { Shutdown Allegro.  You should do it because it isn't automatic. }
    al_exit;
    EXIT;
  END;

{ Double buffer animation. }
  Buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);

{ Set up the viewport for the perspective projection }
  al_set_projection_viewport (0, 0, AL_SCREEN_W, AL_SCREEN_H);

{ Initialise the cube. }

  REPEAT
  { Draw. }
    al_clear_bitmap (Buffer);
    al_vsync;
    al_blit (Buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

  { User input. }
    IF al_keypressed THEN
    BEGIN
      Key := al_readkey SHR 8;
    END;

  { Wait until a key is pressed. }
  UNTIL Key = AL_KEY_ESC;

{ Shutdown Allegro.  You should do it because it isn't automatic. }
  al_destroy_bitmap (Buffer);
  al_exit;

{ End of the program. }
END.
