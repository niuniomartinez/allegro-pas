PROGRAM ex3d;
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
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  cube,
  allegro, algui, al3d, alfixed, alvga, alblend,
  sysutils;



(* Frame control. *)
  VAR
    Tick: LONGINT;
  PROCEDURE Timer; CDECL;
  BEGIN
    INC (Tick);
  END;



VAR
(* Graphics mode selection. *)
  c, w, h, bpp: LONGINT;
  Buffer: AL_BITMAPptr;
(* Input. *)
  Key: LONGINT;
(* The cube. *)
  TheCube: TCube;
  Palette: AL_PALETTE;
  Bitmap, Texture: AL_BITMAPptr;
  Filename: STRING;
  Rotate: TVector;
(* Color management. *)
  RGBTable: AL_RGB_MAP;
  LightTable, TransTable: AL_COLOR_MAP;
(* Name of modes. *)
  DrawModeName: ARRAY[0..15] OF STRING = (
   'Wireframe',
   'Flat shaded',
   'Single color Gouraud shaded',
   'Gouraud shaded',
   'Texture mapped',
   'Perspective correct texture mapped',
   'Masked texture mapped',
   'Masked persp. correct texture mapped',
   'Lit texture map',
   'Lit persp. correct texture map',
   'Masked lit texture map',
   'Masked lit persp. correct texture map',
   'Transparent texture mapped',
   'Transparent perspective correct texture mapped',
   'Transparent masked texture mapped',
   'Transparent masked persp. correct texture mapped'
  );
BEGIN { The program starts here. }

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_mouse;
  al_install_timer;
  al_install_int_ex (@Timer, AL_BPS_TO_TIMER (50));

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

{ Load the cube texture. }
  Filename := ExtractFilePath (ParamStr (0)) + 'allegro.pcx';

  Bitmap := al_load_bitmap (Filename, @Palette);
  IF Bitmap = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
  { Show an error message. }
    al_message ('Error reading mysha.pcx');
  { Shutdown Allegro. }
    al_exit;
    EXIT;
  END;
  al_set_palette (Palette);
{ Better if texture is power of two. }
  Texture := al_create_bitmap (128, 128);
  al_stretch_blit (Bitmap, Texture,
		   0, 0, Bitmap^.w, Bitmap^.h,
		   0, 0, Texture^.w, Texture^.h);

{ Make bitmap same size than screen.  This way draw the background is faster. }
  al_stretch_blit (Bitmap, al_screen, 0, 0, Bitmap^.w, Bitmap^.h,
				      0, 0, AL_SCREEN_W, AL_SCREEN_H);
  al_destroy_bitmap (Bitmap);
  Bitmap := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);
  al_blit (al_screen, Bitmap, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

{ Build a rgb_map table.  Not needed, but speeds things up. }
  al_create_rgb_table (@RGBTable, Palette, NIL);
  al_rgb_table := @RGBTable;

{ Build a lighting table. }
  al_create_light_table (@LightTable, Palette, 0, 0, 0, NIL);
  al_color_table := @LightTable;

{ Build a transparency table (50% transparent). }
  al_create_trans_table (@TransTable, Palette, 128, 128, 128, NIL);

{ Set up truecolor blending function (50% transparent). }
  al_set_trans_blender (0, 0, 0, 128);

{ Initialise the cube. }
  Randomize;
  TheCube := TCube.Create (0, 0, al_itofix (-5), al_itofix (1), Texture);
  TheCube.DrawMode := POLYTYPE_WIRED;
  Rotate := TVector.Create (al_ftofix ((Random (32) - 16) / 8),
			   al_ftofix ((Random (32) - 16) / 8),
			   al_ftofix ((Random (32) - 16) / 8));

  Tick := 1;
  REPEAT
  { Update. }
    WHILE Tick > 0 DO
    BEGIN
      TheCube.Ang.Add (Rotate);
    { User input. }
      IF al_keypressed THEN
      BEGIN
	Key := al_readkey SHR 8;
	IF Key <> AL_KEY_ESC THEN
	BEGIN
	  INC (TheCube.DrawMode);
	  IF TheCube.DrawMode >= AL_POLYTYPE_MAX THEN
	  BEGIN
	    TheCube.DrawMode := POLYTYPE_WIRED;
	    al_color_table := @LightTable;
	  END;
	  IF TheCube.DrawMode >= AL_POLYTYPE_ATEX_TRANS THEN
	    al_color_table := @TransTable;
	END;
      END;
    { Next tick. }
      DEC (Tick);
    END;
  { Draw. }
    al_blit (Bitmap, Buffer, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
    TheCube.Draw (Buffer, @al_identity_matrix);
    al_textout_ex (Buffer, al_font, 'Poly type: '+DrawModeName[TheCube.DrawMode + 1],
		   1, 1, -1, -1);
    al_vsync;
    al_blit (Buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

  { Wait until a key is pressed. }
  UNTIL Key = AL_KEY_ESC;

{ Release resources. }
  TheCube.Free;
  al_destroy_bitmap (Buffer);
  al_destroy_bitmap (Texture);

{ Shutdown Allegro.  You should do it because it isn't automatic. }
  al_exit;

{ End of the program. }
END.
