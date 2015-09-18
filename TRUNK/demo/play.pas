UNIT play;
(* Program: Demo game for the Allegro.pas library.
 * Description: Play the game.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *	   Translated from a game by Ken Silverman <http://www.advsys.net/ken/>
 *)

INTERFACE

  CONST
  (* Size of the output without scaling. *)
    SCREENW = 256;
    SCREENH = 192;

(* Runs the play game loop. *)
  PROCEDURE PlayGame;

IMPLEMENTATION

  USES
    allegro,
    alex,     { Alex the Allegator. }
    config,   { Management of the game configuration. }
    bombs,    { Bombs. }
    error,    { To show nice error messages. }
    framectr, { The frame speed controller. }
    gamedata, { Management of the game data: graphics, sound, etc. }
    sprites,  { Sprites management. }
    sysutils, { For string manipulation. }
    tilemap;  { Tilemap management. }

  VAR
    MapNum: INTEGER;



(* Runs the play game loop. *)
  PROCEDURE PlayGame;
  VAR
    Palette: AL_PALETTEptr; TextFont: AL_FONTptr;
    EndLoop, EndPlayLoop: BOOLEAN;
    DBuffer, ScreenShoot: AL_BITMAPptr;
    ScreenShootCnt: INTEGER;
    SizeW, SizeH: INTEGER; { Actual size of output bitmap at screen. }
    PosX, PosY: INTEGER; { Actual position of output bitmap at screen. }
    PreNumCoins: INTEGER; { To know if the number of coins changed. }
    AlexLifes, PreAlexLifes: INTEGER; { Number of Alex' lifes. }

  (* Helper function to set up the board. *)
    FUNCTION NewMap (MapNum: INTEGER): BOOLEAN;
    BEGIN
      NewMap := FALSE;
    { Load the board. }
      IF LoadMap ('level'+IntToStr (MapNum)+'.map') THEN
      BEGIN
      { Check the start point and the end point. }
	IF StartX = -1 THEN
	BEGIN
	  ErrorMessage ('No start point in map "level'+IntToStr (MapNum)+'.map"!');
	  EXIT;
	END;
	IF EndX = -1 THEN
	BEGIN
	  ErrorMessage ('Not end point in map "level'+IntToStr (MapNum)+'.map"!');
	  EXIT;
	END;
      { Initialize game objects. }
	InitSprites (10);
	InitAlex;
	InitBombs;
      { Everything is 0k. }
	NewMap := TRUE;
      END;
    END;

  (* End secuence.  Shows the final score. *)
    PROCEDURE EndGame (PlayerWin: BOOLEAN);
    BEGIN
      IF PlayerWin THEN
	al_textout_centre_ex (DBuffer, TextFont, 'You win!',
			      SCREENW DIV 2, (SCREENH DIV 2) - 16, 255, -1)
      ELSE
	al_textout_centre_ex (DBuffer, TextFont, 'Game over',
			      SCREENW DIV 2, (SCREENH DIV 2) - 16, 255, -1);
      al_textout_centre_ex (DBuffer, TextFont,
			    IntToStr (NumCoins)+' coins collected',
			    SCREENW DIV 2, SCREENH DIV 2, 255, -1);
      al_stretch_blit (DBuffer, al_screen, 0, 0, SCREENW, SCREENH,
		       PosX, PosY, SizeW, SizeH);
    { Pause. }
      al_rest (1000);
      al_textout_centre_ex (DBuffer, TextFont,
			    'Press a key to continue',
			    SCREENW DIV 2, SCREENH DIV 2 + 16, 255, -1);
      al_stretch_blit (DBuffer, al_screen, 0, 0, SCREENW, SCREENH,
		       PosX, PosY, SizeW, SizeH);
      al_clear_keybuf;
      al_readkey;
      al_fade_out (8);
    { While this function is in pause the "Tick" variable is incremented by the
      interruption procedure (see framectr.pas) so we must set it to 1 to
      prevent pauses. }
      Tick := 1;
    END;

  BEGIN
    al_fade_out (8);
  { Set up the play. }
    al_play_sample (Data^[GAME_MUSIC].dat, 127, 127, 1000, TRUE);
    EndLoop := FALSE;
    MapNum := 1;
    Palette := Data^[GAME_PAL].dat;
    al_clear_to_color (al_screen, 1);
    al_set_palette (Palette^);
    TextFont := Data^[GAME_FONT].dat;
    PreNumCoins := -1;  NumCoins := 0;
    PreAlexLifes := -1; AlexLifes := 3;
  { Set up the double buffer. }
    DBuffer := al_create_bitmap (SCREENW, SCREENH);
    SizeW := Round (SCREENW * ScaleSc);
    SizeH := Round (SCREENH * ScaleSc);
    PosX := (AL_SCREEN_W DIV 2) - (SizeW DIV 2);
    PosY := (AL_SCREEN_H DIV 2) - (SizeH DIV 2);
    al_textout_ex (al_screen, TextFont, 'coins:', PosX,     PosY - 10, 245,-1);
    al_textout_ex (al_screen, TextFont, 'Lifes:', PosX+160, PosY - 10, 245,-1);
  { Set up the screen shoot system. }
    ScreenShoot := al_create_sub_bitmap (al_screen,0,0,AL_SCREEN_W,AL_SCREEN_H);
    ScreenShootCnt := 0;
  { The game loop. }
    REPEAT
    {  Load a map. }
      IF NOT NewMap (MapNum) THEN
      BEGIN
	EndGame (TRUE);
	EndLoop := TRUE;
      END
      ELSE BEGIN
      { Play the game. }
	EndPlayLoop := FALSE;
	Tick := 1; { Must update the first frame. }
	REPEAT
	{ Logical loop. }
	  WHILE Tick > 0 DO { While there are frames to uptade. }
	  BEGIN
	  { Keyboard. }
	    IF al_key[AL_KEY_ESC] THEN
	    BEGIN
	      EndPlayLoop := TRUE;
	      EndLoop := TRUE;
	    END;
	    IF al_key[AL_KEY_F12] THEN
	    BEGIN
	      al_save_bitmap ('shoot'+IntToStr (ScreenShootCnt)+'.pcx',
			      ScreenShoot,
			      Data^[GAME_PAL].dat);
	      INC (ScreenShootCnt);
	      al_key[AL_KEY_F12] := FALSE; { To avoid repetition. }
	    { Save a bitmap is a long task so restart the Tick variable. }
	      Tick := 1;
	    END;
	  { Update objects. }
	    UpdateAlex;
	    IF AlexDead THEN
	    BEGIN
	      DEC (AlexLifes);
	      IF AlexLifes < 1 THEN
	      BEGIN
	      { No more lifes, so end play. }
		EndGame (FALSE);
		EndPlayLoop := TRUE;
		EndLoop := TRUE;
	      END
	      ELSE BEGIN
	      { More lifes, so restart the level. }
		InitSprites (10);
		InitAlex;
		InitBombs;
		Tick := 1;
	      END;
	    END
	    ELSE IF AlexWin THEN
	      EndPlayLoop := TRUE;
	    IF NOT Cheat THEN
	      UpdateBombs;
	    DEC (Tick); { Frame updated. }
	  { If there are too much frames to update then it is too slow. }
	    IF Tick > 30 THEN
	    BEGIN
	      ErrorMessage ('Sorry but your computer is too slow.');
	      EndPlayLoop := TRUE;
	      EndLoop := TRUE;
	      BREAK;
	    END;
	  END;
	{ Graphics. }
	  FixScroll (DBuffer, ScrollX, ScrollY, ScrollX, ScrollY);
	  al_clear_to_color (DBuffer, al_makecol (0, 255, 255));
	  DrawMap (DBuffer, ScrollX, ScrollY);
	  DrawSprites (DBuffer, ScrollX, ScrollY);
	  al_vsync;
	  IF SCREENW <> SizeW THEN
	    al_stretch_blit (DBuffer, al_screen, 0, 0, SCREENW, SCREENH,
			     PosX, PosY, SizeW, SizeH)
	  ELSE
	    al_blit (DBuffer, al_screen, 0, 0, PosX, PosY, SizeW, SizeH);
	{Prints the number of coins only if it changes.  This way it's faster.}
	  IF PreNumCoins <> NumCoins THEN
	  BEGIN
	    al_textout_ex (al_screen, TextFont, IntToStr (NumCoins),
			   PosX + 48, PosY - 10, 252, 1);
	    PreNumCoins := NumCoins;
	  END;
	{Prints the number of lifes only if it changes.  This way it's faster.}
	    IF PreAlexLifes <> AlexLifes THEN
	  BEGIN
	    al_textout_ex (al_screen, TextFont, IntToStr (AlexLifes),
			   PosX + 208, PosY - 10, 252, 1);
	    PreAlexLifes := AlexLifes;
	  END;
	UNTIL EndPlayLoop;
      END;
      INC (MapNum);
    UNTIL EndLoop;
  { Release resources. }
    al_destroy_bitmap (DBuffer);
    al_destroy_bitmap (ScreenShoot);
    al_stop_sample (Data^[GAME_MUSIC].dat);
  END;

END.
