UNIT title;
(* Program: Demo game for the Allegro.pas library.
 * Description: The title sequence.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

INTERFACE

(* Shows the title sequence.  Returns TRUE if player wants to play or FALSE
   if he/she wants to exit. *)
  FUNCTION RunTitle: BOOLEAN;



IMPLEMENTATION

USES
  allegro,
  config,   { Management of the game configuration. }
  framectr, { The frame speed controller. }
  gamedata; { Management of the game data: graphics, sound, etc. }

VAR
(* The text for the slide show. *)
  TextShow: ARRAY [0..11] OF STRING = (
	  'Help Alex the Allegator to get the coins and find the exit of the level.  Use left and right cursor keys or the joystick to move Alex.  Use the space bar or the joystick ''A'' button to jump.  Press ''jump'' to play or [Esc] to exit. ',
	  'This game is a demonstration of the Allegro.pas library.  Its goal is to create a good piece of code to show how to write your games using Allegro.pas.  See the sources and read the documentation. ',
	  'Now, the credits... ',
	  'Original game concept: Ken Silverman (http://www.advsys.net/ken/) ',
	  'Programming and graphics: '#176'u'#208'o Mart'#204'nez (http://www.burdjia.com/) ',
	  'Alex the Allegator character courtesy from:  Johan Peitz (http://www.freelunchdesign.com/) ',
	  'Title music (provisional):  Garret Thomsom (g@sirsonic.com) (ripped from the original Allegro demo game) ',
	  'Game music:  Partners In Rhyme (http://www.partnersinrhyme.com/) ',
	  'Sound FX ripped from games by Shawn Hargreaves and Johan Peitz ',
	  'Allegro originally created by Shawn Hargreaves (http://talula.demon.co.uk/) and currently mantained by the Allegro Development Team (http://alleg.sourceforge.net/) ',
	  'Allegro.pas:  '#176'u'#208'o Mart'#204'nez (again...) ',
	  'Thanks to the Spanish Delphi community (http://www.clubdelphi.net/) and the Allegro community (http://www.allegro.cc/) '
  );




(* Shows the title sequence.  Returns TRUE if player wants to play or FALSE
 * if he/she wants to exit. *)
  FUNCTION RunTitle: BOOLEAN;
  VAR
    Palette: AL_PALETTEptr;
    SlideBmp: AL_BITMAPptr; { Double buffer. }
    Cnt, Key: INTEGER;
    EndSlideshow: BOOLEAN;
    NumLine: INTEGER;
  BEGIN
  { Setup of the title sequence. }
    Palette := Data^[TITLE_PAL].dat;
    al_set_color (0, @(Palette[0]));
    al_clear_bitmap (al_screen);
    al_set_palette (Palette^);
    SlideBmp := al_create_bitmap (AL_SCREEN_W, 50);
    al_clear_bitmap (SlideBmp);
    al_clear_keybuf;
  { Sound. }
    al_play_midi (Data^[TITLE_MUSIC].dat, TRUE);
    al_play_sample (Data^[WELCOME_SPL].dat, 255, 127, 1000, 0);
  { If "-jumpstart" used then doesn't do the first part of title. }
    IF DoIntro THEN
    BEGIN
    { First part, zoom-in the title bitmap. }
      Cnt := 0; Tick := 1; { This is to draw one frame first. }
      REPEAT
      { Tick is incremented by the timer routine.  See the 'framectr' unit. }
	WHILE Tick > 0 DO
	BEGIN
	{ For each 'Tick' we increment the size of the title bitmap. }
	  DEC (Tick);
	  INC (Cnt);
	END;
      { Draws the title bitmap. }
	al_vsync;
	al_stretch_blit (Data^[TITLE_BMP].dat, al_screen,
			0, 0, 320, 128,
		     { Calculates the center of screen. }
			(AL_SCREEN_W DIV 2) -  Cnt,
			(AL_SCREEN_H DIV 2) - (Cnt * 64) DIV 160 - 32,
			Cnt * 2, (Cnt * 128) DIV 160);
      UNTIL (Cnt >= 160) OR al_keypressed;
    END;
  { Draws it again, at full size. }
    al_blit (Data^[TITLE_BMP].dat, al_screen, 0, 0,
	     (AL_SCREEN_W DIV 2) - 160, (AL_SCREEN_H DIV 2) - 96,
	     320, 128);
  { Second part, the slide show. }
    EndSlideshow := FALSE; Cnt := AL_SCREEN_W; Tick := 1;
    NumLine := 0;
    REPEAT
      WHILE Tick > 0 DO
      BEGIN
      { Checks keyboard. }
	IF al_keypressed THEN
	BEGIN
	  Key := al_readkey SHR 8;
	  IF (Key = AL_KEY_SPACE) OR (Key = AL_KEY_ESC) THEN
	    EndSlideShow := TRUE;
	END
      { Checks joystick. }
	ELSE IF al_num_joysticks > 0 THEN
	BEGIN
	  al_poll_joystick;
	  IF al_joy[0].button[0].b THEN
	  BEGIN
	    EndSlideShow := TRUE;
	    Key := AL_KEY_SPACE; { Simulate keyboard. }
	  END;
	END;
      { For each 'Tick' we decrement the x coordinate of the slide show. }
	DEC (Tick);
	DEC (Cnt);
      { Checks if we have shown all text and change the line if so. }
	IF Cnt <= Length (TextShow[NumLine]) * (-8) THEN
	BEGIN
	  INC (NumLine);
	  IF NumLine >= Length (TextShow) THEN
	    NumLine := 0;
	  Cnt := AL_SCREEN_W;
	END;
      END;
    { The slide show.  It's a double buffer. }
      al_textout_ex (SlideBmp, Data^[TITLE_FONT].dat, TextShow[NumLine],
		     Cnt, 0, -1, 0);
      al_vsync;
      al_blit (SlideBmp, al_screen, 0, 0, 0, AL_SCREEN_H - 50, AL_SCREEN_W, 50);
    UNTIL EndSlideshow;
  { Result. }
    RunTitle := Key = AL_KEY_SPACE;
  { Releases resources. }
    al_destroy_bitmap (SlideBmp);
    al_stop_midi;
  END;

END.
