PROGRAM exscn3d;
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
 *	This program demonstrates how to use scanline sorting algorithm in
 *	Allegro (al_create_scene, al_clear_scene, ... functions).  It also
 *	provides an example of how to use the 3D clipping function.  The
 *	example consists of a flyby through a lot of rotating 3d cubes.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library, by Bertrand Coconnier.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro, algui, al3d,
    sysutils;

  CONST
    CUBE_CUBES = 4;

  TYPE
    QUAD = RECORD
      v: ARRAY [1..4] OF LONGINT;
    END;

  VAR
    Vertex: ARRAY [1..8] OF AL_V3D_F = (
      (x: -10; y: -10; z: -10; u: 0; v: 0; c: 72),
      (x: -10; y:  10; z: -10; u: 0; v: 0; c: 80),
      (x:  10; y:  10; z: -10; u: 0; v: 0; c: 95),
      (x:  10; y: -10; z: -10; u: 0; v: 0; c: 88),
      (x: -10; y: -10; z:  10; u: 0; v: 0; c: 72),
      (x: -10; y:  10; z:  10; u: 0; v: 0; c: 80),
      (x:  10; y:  10; z:  10; u: 0; v: 0; c: 95),
      (x:  10; y: -10; z:  10; u: 0; v: 0; c: 88)
    );
    Cube: ARRAY [1..6] OF QUAD = (
      (v: (3, 2, 1, 4)),
      (v: (5, 6, 7, 8)),
      (v: (1, 2, 6, 5)),
      (v: (3, 4, 8, 7)),
      (v: (5, 8, 4, 1)),
      (v: (2, 3, 7, 6))
    );
    V: ARRAY [1..4] OF AL_V3D_f;
    Vout, Vtmp: ARRAY [1..12] OF AL_V3D_f;
    pV: ARRAY [1..4] OF AL_V3D_fptr;
    pVout, pVtmp: ARRAY [1..12] OF AL_V3D_fptr;



(* Frame control. *)
  VAR
    Tick: LONGINT;
  PROCEDURE Timer; CDECL;
  BEGIN
    INC (Tick);
  END;



  VAR
    Texture: AL_BITMAPptr;
    Palette: AL_PALETTE;
    RGBTable: AL_RGB_MAP;



  FUNCTION LoadTexture: BOOLEAN;
  VAR
    Filename: STRING;
    Pic: AL_BITMAPptr;
  BEGIN
    Filename := ExtractFilePath (ParamStr (0)) + 'allegro.pcx';
    Pic := al_load_bitmap (Filename, @Palette);
    IF Pic = NIL THEN
    BEGIN
      al_message ('Can''t load allegro.pcx.');
      LoadTexture := FALSE;
      EXIT;
    END;
  { Texture size must be power of two. }
    Texture := al_create_bitmap (128, 128);
    al_stretch_blit (Pic, Texture, 0, 0, Pic^.w, Pic^.h, 0, 0, 128, 128);
    al_destroy_bitmap (Pic);
    LoadTexture := TRUE;
  END;



(* Set up the graphic mode and creates the color palette. *)
  FUNCTION InitGfx: BOOLEAN;
  VAR
    Cnt: INTEGER;
    c, w, h, bpp: LONGINT;
  BEGIN
  { Color 0 = black }
    Palette[0].r := 0; Palette[0].g := 0; Palette[0].b := 0;

  { Color 1 = red }
    Palette[1].r := 63; Palette[1].g := 0; Palette[1].b := 0;

  { Copy the desktop palette. }
    FOR Cnt := 2 TO 63 DO
      Palette[Cnt] := al_desktop_palette[Cnt];

  { Make a blue gradient }
    FOR Cnt := 64 TO 95 DO
    BEGIN
      Palette[Cnt].r := 0;
      Palette[Cnt].g := 0;
      Palette[Cnt].b := (Cnt - 64) * 2;
    END;

    FOR Cnt := 96 TO 255 DO
    BEGIN
      Palette[Cnt].r := 0;
      Palette[Cnt].g := 0;
      Palette[Cnt].b := 0;
    END;

  { Set the graphics mode. }
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      InitGfx := FALSE;
      EXIT;
    END;
    al_set_palette (al_desktop_palette);

    c := AL_GFX_AUTODETECT;
    w := AL_SCREEN_W; h := AL_SCREEN_H; bpp := al_bitmap_color_depth (al_screen);
    IF NOT al_gfx_mode_select_ex (c, w, h, bpp) THEN
    BEGIN
      InitGfx := FALSE;
      EXIT;
    END;

    al_set_color_depth (bpp);
    IF NOT al_set_gfx_mode (c, w, h, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message (al_error);
      InitGfx := FALSE;
      EXIT;
    END;
    IF NOT LoadTexture THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      InitGfx := FALSE;
      EXIT;
    END;
    al_set_palette (Palette);

  { Build a rgb_map table.  Not needed, but speeds things up. }
    al_create_rgb_table (RGBTable, Palette, NIL);
    al_rgb_table := @RGBTable;
    InitGfx := TRUE;
  END;



(*  Translates, rotates, clips, projects, culls backfaces and draws a cube. *)
  PROCEDURE DrawCube (Matrix: AL_MATRIX_fptr; NumPolys: INTEGER);
  VAR
    i, j, nv: LONGINT;
    Out: ARRAY [0..12] OF LONGINT;
  BEGIN
    FOR I := 1 TO NumPolys DO
    BEGIN
      FOR J := 1 TO 4 DO
      BEGIN
	v[j] := Vertex[Cube[i].v[j]];
	al_apply_matrix_f (Matrix, v[j].x, v[j].y, v[j].z,
				 v[j].x, v[j].y, v[j].z);
      END;
      v[1].u := 128; v[1].v := 128;
      v[2].u :=   0; v[2].v := 128;
      v[3].u :=   0; v[3].v :=   0;
      v[4].u := 128; v[4].v :=   0;
    (* nv: number of vertices after clipping is done. *)
      nv := al_clip3d_f (AL_POLYTYPE_PTEX, 0.24, 1000, 4, pV, pVout, pVtmp, Out);
      IF nv > 0 THEN
      BEGIN
	FOR J := 1 TO nv DO
	  al_persp_project_f (Vout[j].x, Vout[j].y, Vout[j].z,
				Vout[j].x, Vout[j].y);
	IF al_polygon_z_normal_f (@vout[1], @vout[2], @vout[3]) > 0 THEN
	  al_scene_polygon3d_f (AL_POLYTYPE_PTEX, Texture, nv, pVout);
      END;
    END;
  END;



VAR
  Buffer: AL_BITMAPptr;
  Matrix, Matrix1, Matrix2, Matrix3: AL_MATRIX_f;
  rx, ry, rot, incr: SINGLE;
  tz, i, j, k: LONGINT;
  Frame: LONGINT;
  FPS: SINGLE;
  FrameLabel: STRING;
BEGIN (* The program starts here. *)

  rx := 0; ry := 0; tz := 40;
  rot := 0; incr := 1;

  Frame := 0; FPS := 0;

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_mouse;
  al_install_timer;
  al_install_int (@Timer, 10);

{ Set up graphics mode. }
  IF NOT InitGfx THEN
  BEGIN
    EXIT;
  END;
  al_set_palette (Palette);

{ Initialize buffers and viewport. }
  Buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);
  al_create_scene (24 * CUBE_CUBES * CUBE_CUBES * CUBE_CUBES,
		6 * CUBE_CUBES * CUBE_CUBES * CUBE_CUBES);
  al_set_projection_viewport (0, 0, AL_SCREEN_W, AL_SCREEN_H);

{ Initialize pointers. }
  FOR J := 1 TO 4 DO
    pV[j] := @v[j];

  FOR J := 1 TO 12 DO
  BEGIN
    pVtmp[j] := @Vtmp[j];
    pVout[j] := @Vout[j];
  END;

  Tick := 0;
  FrameLabel := '(0.0 fps)';
  WHILE al_key[AL_KEY_ESC] = 0 DO
  BEGIN
    al_clear_bitmap (Buffer);
    al_clear_scene  (Buffer);

  { Matrix2: rotates cube }
    al_get_rotation_matrix_f (@matrix2, rx, ry, 0);
  { Matrix3: turns head right/left }
    al_get_rotation_matrix_f (@matrix3, 0, rot, 0);

    FOR K := (CUBE_CUBES - 1) DOWNTO 0 DO
      FOR J := 0 TO (CUBE_CUBES - 1) DO
        FOR I := 0 TO (CUBE_CUBES - 1) DO
	BEGIN
	{ Matrix1: locates cubes }
	  al_get_translation_matrix_f (@matrix1, (j * 40 - CUBE_CUBES * 20 + 20),
					(i * 40 - CUBE_CUBES * 20 + 20), (tz + k * 40));

	{ Matrix: rotates cube THEN locates cube THEN turns
	  head right/left }
	  al_matrix_mul_f (@matrix2, @matrix1, @matrix);
	  al_matrix_mul_f (@matrix,  @matrix3, @matrix);

	{ Cubes are just added to the scene.
	  No sorting is done at this stage. }
	  DrawCube (@matrix, 6);
	END;
  { Sorts and renders polys }
    al_render_scene;
    al_textout_ex (Buffer, al_font, FrameLabel, 1, 1, -1, -1);
    al_blit (Buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
    Frame := Frame + 1;

  { Manage cubes movement }
    DEC (tz, 2);
    IF tz = 0 THEN tz := 40;
    rx := rx + 4;
    ry := ry + 4;
    rot := rot + incr;
    IF (rot >= 25) OR (rot <= -25) THEN incr := -incr;

  { Computes fps }
    IF Tick > 100 THEN
    BEGIN
      FPS := (100.0 * Frame) / Tick;
      FrameLabel := '('+FloatToStr (FPS)+' fps)';
      Tick := 0;
      Frame := 0;
    END;
  END;
{ Release resources. }
  al_destroy_bitmap (Texture);
  al_destroy_bitmap (Buffer);
  al_destroy_scene;

{ End of the program. }
END.
