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
 *
 *	Example for Allegro.pas that displays a 3D cube and rotates it.
 *
 *	By Guillermo "Ñuño" Martínez
 *
 *	See README file for license and copyright information.
 *)

{$IFDEF FPC}
{ This example uses Object Pascal language. }
  {$MODE OBJFPC}
{$ENDIF}

  USES
    cube,
    allegro, algui, al3D, alfixed, alvga, alblend,
    sysutils;



(* Frame control. *)
  VAR
    Tick: INTEGER;
  PROCEDURE Timer; CDECL;
  BEGIN
    INC (Tick);
  END;



  TYPE
  (* Extends the basic cube to make it bounce. *)
    TBouncingCube = CLASS (TCube)
    PRIVATE
      fDeltaz: AL_FIXED;
    PUBLIC
      CONSTRUCTOR Create (aTexture: AL_BITMAPptr); OVERLOAD;
      PROCEDURE Update; OVERRIDE;
    END;



(* Creates the cube. *)
  CONSTRUCTOR TBouncingCube.Create (aTexture: AL_BITMAPptr);
  BEGIN
    INHERITED Create (
      al_itofix (Random (256) - 128),
      al_itofix (Random (256) - 128),
      al_itofix (768),
      al_itofix (32),
      aTexture
    );
    SELF.DrawMode := POLYTYPE_WIRED;
    REPEAT
      fDeltaz := al_itofix (Random (32) - 16)
    UNTIL fDeltaz <> 0;

  END;



(* Updates the cube. *)
  PROCEDURE TBouncingCube.Update;
  BEGIN
    INHERITED Update;
    SELF.Pos.Z := SELF.Pos.Z + fDeltaz;
    IF ((192 SHL 16) > SELF.Pos.Z) OR (SELF.Pos.Z > (1024 SHL 16)) THEN
      fDeltaz := fDeltaz * (-1);
  END;



  VAR
  (* Graphics mode selection. *)
    c, w, h, bpp: LONGINT;
    Buffer: AL_BITMAPptr;
  (* Input. *)
    Key: LONGINT;
  (* The cubes. *)
    TheCubes: ARRAY [1..8] OF TBouncingCube;
    Palette: AL_PALETTE;
    Texture: AL_BITMAPptr;
  (* Color management. *)
    RGBTable: AL_RGB_MAP;
    LightTable, TransTable: AL_COLOR_MAP;
  (* Name of modes. *)
    DrawModeName: ARRAY [0..15] OF STRING = (
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
    al_create_rgb_table (RGBTable, Palette, NIL);
    al_rgb_table := @RGBTable;
  END;



(* Creates a texture to be used by the cube. *)
  PROCEDURE CreateTexture;
  BEGIN
    Texture := al_create_bitmap (32, 32);
    al_clear_to_color (Texture, al_bitmap_mask_color (Texture));
    al_line (Texture, 0, 0, 31, 31, al_palette_color^[1]);
    al_line (Texture, 0, 32, 32, 0, al_palette_color^[1]);
    al_rect (Texture, 0, 0, 31, 31, al_palette_color^[1]);
    al_textout_ex (Texture, al_font, 'dead', 0,  0, al_palette_color^[2], -1);
    al_textout_ex (texture, al_font, 'pigs', 0,  8, al_palette_color^[2], -1);
    al_textout_ex (texture, al_font, 'cant', 0, 16, al_palette_color^[2], -1);
    al_textout_ex (texture, al_font, 'fly.', 0, 24, al_palette_color^[2], -1);
  END;



(* Orders cubes by Z axis. *)
  PROCEDURE OrderCubes;

    PROCEDURE SwapCubes (C1, C2: INTEGER); INLINE;
    VAR
      Tmp: TBouncingCube;
    BEGIN
      Tmp := TheCubes[C1];
      TheCubes[C1] := TheCubes[C2];
      TheCubes[C2] := Tmp;
    END;

  VAR
    Cnt: INTEGER;
  BEGIN
  { It uses simple bubble ordering.  This is because cubes are almost ordered
    (except at start) and it is fastest than QSort in that cases! }
    FOR Cnt := LOW (TheCubes) TO (HIGH (TheCubes) - 1) DO
      IF TheCubes[Cnt].Pos.z < TheCubes[Cnt + 1].Pos.z THEN
        SwapCubes (Cnt, Cnt + 1);
  END;



VAR
  Cnt, DrawMode: INTEGER;
BEGIN (* The program starts here. *)

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_timer;
  al_install_mouse;
  al_install_keyboard;
  al_install_int_ex (@Timer, AL_BPS_TO_TIMER (50));

{ Set a graphics mode. }
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;
  al_set_palette (al_desktop_palette);

  c := AL_GFX_AUTODETECT;
  w := AL_SCREEN_W; h := AL_SCREEN_H; bpp := al_bitmap_color_depth (al_screen);
  IF NOT al_gfx_mode_select_ex (c, w, h, bpp) THEN
  BEGIN
    EXIT;
  END;

  al_set_color_depth (bpp);
  IF NOT al_set_gfx_mode (c, w, h, 0, 0) THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
  { Show an error message. }
    al_message ('Unable to set any graphic mode'#10+al_error+''#10);
    EXIT;
  END;

{ Double buffer animation. }
  Buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);

{ Set up the viewport for the perspective projection }
  al_set_projection_viewport (0, 0, AL_SCREEN_W, AL_SCREEN_H);

  CreatePalette;

{ Build a lighting table. }
  al_create_light_table (@LightTable, Palette, 0, 0, 0, NIL);
  al_color_table := @LightTable;

{ Build a transparency table (25% transparent). }
  al_create_trans_table (@TransTable, Palette, 192, 192, 192, NIL);

{ Set up truecolor blending function (25% transparent). }
  al_set_trans_blender (0, 0, 0, 192);

{ Initialise the cubes. }
  DrawMode := POLYTYPE_WIRED;
  CreateTexture;
  Randomize;
  FOR Cnt := LOW (TheCubes) TO HIGH (TheCubes) DO
    TheCubes[Cnt] := TBouncingCube.Create (Texture);

  Tick := 1;
  REPEAT
  { Update. }
    WHILE Tick > 0 DO
    BEGIN
    { User input. }
      IF al_keypressed THEN
      BEGIN
	Key := al_readkey SHR 8;
	IF Key <> AL_KEY_ESC THEN
	BEGIN
	  INC (DrawMode);
	  IF DrawMode >= AL_POLYTYPE_MAX THEN
	  BEGIN
	    DrawMode := POLYTYPE_WIRED;
	    al_color_table := @LightTable;
	  END;
	  IF DrawMode >= AL_POLYTYPE_ATEX_TRANS THEN
	    al_color_table := @TransTable;
	END;
      END;
      FOR Cnt := LOW (TheCubes) TO HIGH (TheCubes) DO
      BEGIN
	TheCubes[Cnt].Update;
	TheCubes[Cnt].DrawMode := DrawMode;
      END;
      OrderCubes;
    { Next tick. }
      DEC (Tick);
    END;
  { Draw. }
    al_clear_bitmap (Buffer);
    FOR Cnt := LOW (TheCubes) TO HIGH (TheCubes) DO
      TheCubes[Cnt].Draw (Buffer, al_identity_matrix);
    al_textout_ex (Buffer, al_font, 'Poly type: '+DrawModeName[DrawMode + 1],
		   1, 1, -1, -1);
    al_textout_ex (Buffer, al_font, 'Color depth: '+IntToStr (bpp)+'bpp',
		   1, 9, -1, -1);
    al_vsync;
    al_blit (Buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

  { Wait until a key is pressed. }
  UNTIL Key = AL_KEY_ESC;

{ Release resources. }
  FOR Cnt := LOW (TheCubes) TO HIGH (TheCubes) DO
    TheCubes[Cnt].Free;
  al_destroy_bitmap (Buffer);
  al_destroy_bitmap (Texture);

{ End of the program. }
END.
