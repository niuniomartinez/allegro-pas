PROGRAM extrans2;
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
 *	This program demonstrates how to draw trans and lit sprites and flip them
 *	at the same time, using al_draw_sprite_ex() function.
 *	It displays several images moving around using different drawing modes
 *	while you can press space key to change the flipping orientation.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of the Allegro Game Library, by Jon Rafkind.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro, alblend, sysutils;

  CONST
  (* define the number of sprites that will be displayed *)
    SPRITE_COUNT = 20;

  TYPE
  (* define directions in which the sprites can move *)
    DIRECTION = ( DIR_UP, DIR_DOWN, DIR_LEFT, DIR_RIGHT );

  (* define our sprite *)
    SPRITEptr = ^SPRITE;
    SPRITE = RECORD
      x, y: INTEGER;

      Direction: DIRECTION;

    (* what order to draw this sprite at, lower before higher *)
      Level: INTEGER;

    (* AL_SPRITE_NORMAL, AL_SPRITE_TRANS, or AL_SPRITE_LIT *)
      DrawType: INTEGER;
    END;



(* puts the sprite somewhere on the screen and sets its initial state *)
  PROCEDURE SetupSprite (s: SPRITEptr; i: INTEGER);
  BEGIN
    CASE i MOD 4 OF
    0:
      BEGIN
	s^.x := -100 + i * 50;
	s^.y := 40 + (i MOD 3) * 100;
	s^.Direction := DIR_RIGHT;
	s^.DrawType := AL_DRAW_SPRITE_NORMAL_MODE;
	s^.Level := 0;
      END;
    1:
      BEGIN;
	s^.x := 640 + 100 - i * 70;
	s^.y := 50 + (i MOD 3) * 110;
	s^.Direction := DIR_LEFT;
	s^.DrawType := AL_DRAW_SPRITE_TRANS_MODE;
	s^.Level := 1;
      END;
    2:
      BEGIN;
	s^.x := 90 + (i MOD 3) * 200;
	s^.y := -100 + i * 70;
	s^.Direction := DIR_DOWN;
	s^.DrawType := AL_DRAW_SPRITE_LIT_MODE;
	s^.Level := 2;
      END;
    3:
      BEGIN;
	s^.x := 50 + (i MOD 3) * 200;
	s^.y := 480 + 100 - i * 70;
	s^.Direction := DIR_UP;
	s^.DrawType := AL_DRAW_SPRITE_TRANS_MODE;
	s^.Level := 3;
      END;
    END;
  END;


(* used by sort function to compare sprites *)
  FUNCTION SpriteCompare (CONST a, b: SPRITE): INTEGER; INLINE;
  BEGIN
    SpriteCompare := a.Level - b.Level;
  END;



(* used by sort function to swap positions. *)
  PROCEDURE SpriteSwap (VAR a, b: SPRITE); INLINE;
  VAR
    t: SPRITE;
  BEGIN
    t := a;
    a := b;
    b := t;
  END;



(* moves the sprite by one pixel acording to its direction *)
  PROCEDURE MoveSprite (s: SPRITEptr);
  BEGIN
    CASE s^.Direction OF
    DIR_UP:
      BEGIN
	DEC (s^.y);
	IF s^.y < -64 THEN
	  s^.y := 480;
      END;
    DIR_DOWN:
      BEGIN
	INC (s^.y);
	IF s^.y > 480 THEN
	  s^.y := -64;
      END;
    DIR_RIGHT:
      BEGIN
	INC (s^.x);
	IF s^.x > 640 THEN
	  s^.x := -64;
      END;
    DIR_LEFT:
      BEGIN
	DEC (s^.x);
	IF s^.x < -64 THEN
	  s^.x := 640;
      END;
    END;
  END;



  VAR
    Sprites: ARRAY [1..SPRITE_COUNT] OF SPRITE;
    Spr: SPRITEptr;
    Buffer, Pic, Tmp: AL_BITMAPptr;
    Mode, i, c: INTEGER;
    HoldSpace: BOOLEAN;
    Filename, Msg: STRING;
BEGIN { The program starts here. }
   Mode := AL_DRAW_SPRITE_NO_FLIP_MODE;
   HoldSpace := FALSE;
   RANDOMIZE;
{ standard init }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_timer;

{ setting graphics mode }
  al_set_color_depth (16);
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

{ set initial position, direction and drawing for all sprites }
  FOR i := LOW (Sprites) TO HIGH (Sprites) DO
    SetupSprite (@Sprites[i], i);

{ sort the sprites by drawing level }
  FOR c := HIGH (Sprites) DOWNTO LOW (Sprites) - 1 DO
    FOR i := c - 1 DOWNTO LOW (Sprites) DO
      IF SpriteCompare (Sprites[i + 1], Sprites[i]) < 0 THEN
        SpriteSwap (Sprites[i + 1], Sprites[i]);

{ locate the bitmap we will use }
  Filename := ExtractFilePath (ParamStr (0)) + 'allegro.pcx';

{ load the bitmap and stretch it }
  Tmp := al_load_bitmap (Filename, NIL);
  Pic := al_create_bitmap (64, 64);
  al_stretch_blit (Tmp, Pic, 0, 0, Tmp^.w, Tmp^.h, 0, 0, Pic^.w, Pic^.h);
  al_destroy_bitmap (Tmp);

{ we are using double buffer mode, so create the back buffer }
  Buffer := al_create_bitmap (al_screen^.w, al_screen^.h);

  al_set_trans_blender (128, 0, 64, 128);

{ exit on Esc key }
  WHILE NOT al_key[AL_KEY_ESC] DO
  BEGIN
  { move every sprite and draw it on the back buffer }
    FOR i := LOW (Sprites) TO HIGH (Sprites) DO
    BEGIN
      Spr := @Sprites[i];
      MoveSprite (Spr);
      al_draw_sprite_ex (Buffer, Pic, Spr^.x, Spr^.y, Spr^.DrawType, Mode);
    END;

  { handle the space key }
    IF al_key[AL_KEY_SPACE] AND NOT HoldSpace THEN
    BEGIN
      HoldSpace := TRUE;
    { switch to next flipping mode }
      CASE Mode OF
      AL_DRAW_SPRITE_H_FLIP_MODE:
	Mode := AL_DRAW_SPRITE_V_FLIP_MODE;
      AL_DRAW_SPRITE_V_FLIP_MODE:
	Mode := AL_DRAW_SPRITE_VH_FLIP_MODE;
      AL_DRAW_SPRITE_VH_FLIP_MODE:
	Mode := AL_DRAW_SPRITE_NO_FLIP_MODE;
      AL_DRAW_SPRITE_NO_FLIP_MODE:
	Mode := AL_DRAW_SPRITE_H_FLIP_MODE;
      END;
    END;
    IF NOT al_key[AL_KEY_SPACE] THEN HoldSpace := FALSE;

  { set the title according to the flipping mode used }
    CASE Mode OF
    AL_DRAW_SPRITE_VH_FLIP_MODE:
      Msg := 'horizontal and vertical flip';
    AL_DRAW_SPRITE_H_FLIP_MODE:
      Msg := 'horizontal flip';
    AL_DRAW_SPRITE_V_FLIP_MODE:
      Msg := 'vertical flip';
    ELSE
      Msg := 'no flipping';
    END;
    al_textout_ex (Buffer, al_font, Msg, 1, 1, al_makecol (255, 255, 255), -1);

  { finally blit the back buffer on the screen }
    al_blit (Buffer, al_screen, 0, 0, 0, 0, Buffer^.w, Buffer^.h);
    al_clear_bitmap (Buffer);

  { reduce CPU usage }
    al_rest (20);
  END;

{ clean up }
  al_destroy_bitmap (Pic);
  al_destroy_bitmap (Buffer);
END.
