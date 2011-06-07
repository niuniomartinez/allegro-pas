PROGRAM exzbuf;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

   Example for Allegro.pas that displays a 3D cube and rotates it.

   by Ñuño Martínez <niunio(at)users.sourceforge.net>
   inspired in an example for the Allegro library by Bertrand Coconnier. *)

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



TYPE
(* Extends the basic cube. *)
  TZCube = CLASS (TCube)
  PUBLIC
    CONSTRUCTOR Create (px, py, pz: AL_FIXED); OVERLOAD;
  END;


(* Cube constructor. *)
  CONSTRUCTOR TZCube.Create (px, py, pz: AL_FIXED);
  BEGIN
    INHERITED Create (px, py, pz, al_itofix (30), NIL);
    DrawMode := AL_POLYTYPE_GRGB;
    UseZbuff := TRUE;
  END;



VAR
  Buffer: AL_BITMAPptr;
(* Input. *)
  Key: LONGINT;
  Paused: BOOLEAN;
(* The cubes. *)
  Cube1, Cube2: TCube;
  Palette: AL_PALETTE;
(* Color management. *)
  RGBTable: AL_RGB_MAP;
(* The Z-buffer. *)
  Zbuf: AL_ZBUFFERptr;



(* Creates the palette. *)
  PROCEDURE CreatePalette;
  VAR
    Cnt: INTEGER;
  BEGIN
  { Color 0 = black }
    Palette[0].r := 0; Palette[0].g := 0; Palette[0].b := 0;

  { Copy the desktop palette. }
    FOR Cnt := 1 TO 63 DO
      Palette[Cnt] := al_desktop_palette[Cnt];

  { Make a red gradient }
    FOR Cnt := 64 TO 95 DO
    BEGIN
      Palette[Cnt].r := (Cnt - 64) * 2;
      Palette[Cnt].g := 0;
      Palette[Cnt].b := 0;
    END;

  { Make a green gradient }
    FOR Cnt := 96 TO 127 DO
    BEGIN
      Palette[Cnt].r := 0;
      Palette[Cnt].g := (Cnt - 96) * 2;
      Palette[Cnt].b := 0;
    END;

  { Set up a greyscale in the top half of the palette }
    FOR Cnt :=128 TO 255 DO
    BEGIN
      Palette[Cnt].r := (Cnt - 128) DIV 2;
      Palette[Cnt].g := (Cnt - 128) DIV 2;
      Palette[Cnt].b := (Cnt - 128) DIV 2;
    END;

    al_set_palette (Palette);

  { Build a rgb_map table.  Not needed, but speeds things up. }
    al_create_rgb_table (@RGBTable, Palette, NIL);
    al_rgb_table := @RGBTable;
  END;



BEGIN (* The program starts here. *)

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
  al_set_color_depth (8);
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 240, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 240, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message (al_error);
      EXIT;
    END;

{ Double buffer the animation and create the Z-buffer. }
  Buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);
  Zbuf := al_create_zbuffer (Buffer);
  al_set_zbuffer (Zbuf);

{ Set up the viewport for the perspective projection }
  al_set_projection_viewport (0, 0, AL_SCREEN_W, AL_SCREEN_H);

  CreatePalette;

{ Initialise the cubes. }
  Randomize;
  Cube1 := TZCube.Create (al_itofix ( 16), 0, al_itofix (100));
  Cube1.SetColor ($FF, 0, 0);
  Cube2 := TZCube.Create (al_itofix (-16), 0, al_itofix (105));
  Cube2.SetColor (0, $FF, 0);

  Paused := FALSE;
  Tick := 1;
  REPEAT
  { Update. }
    WHILE Tick > 0 DO
    BEGIN
    { Animate cubes. }
      IF NOT Paused THEN
      BEGIN
	Cube1.Update;
	Cube2.Update;
      END;
    { User input. }
      IF al_keypressed THEN
      BEGIN
	Key := al_readkey SHR 8;
	IF Key <> AL_KEY_ESC THEN
	BEGIN
	  IF Key = AL_KEY_PGDN THEN
	  BEGIN
	    Cube1.Pos.z := Cube1.Pos.z + (1 SHL 14);
	    Cube2.Pos.z := Cube1.Pos.z + (1 SHL 14);
	  END
	  ELSE IF Key = AL_KEY_PGUP THEN
	  BEGIN
	    Cube1.Pos.z := Cube1.Pos.z - (1 SHL 14);
	    Cube2.Pos.z := Cube1.Pos.z - (1 SHL 14);
	  END
	  ELSE IF Key = AL_KEY_P THEN
	    Paused := NOT Paused;
	END;
      END;
    { Next tick. }
      DEC (Tick);
    END;
  { Draw. }
    al_clear_bitmap (Buffer);
    al_clear_zbuffer (Zbuf, 0);
    Cube1.Draw (Buffer, @al_identity_matrix);
    Cube2.Draw (Buffer, @al_identity_matrix);
    al_textout_ex (Buffer, al_font, 'Z-buffer example', 1, 9, -1, -1);
    al_textout_ex (Buffer, al_font, 'Press ''P'' to pause', 1, 19, -1, -1);
    al_vsync;
    al_blit (Buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

  { Wait until a key is pressed. }
  UNTIL Key = AL_KEY_ESC;

{ Release resources. }
  Cube1.Free;
  Cube2.Free;
  al_destroy_bitmap (Buffer);
  al_destroy_zbuffer (Zbuf);

{ End of the program. }
END.
