PROGRAM demo;
(* Program: Demo game for the Allegro.pas library.
 * Description: Demo game for Allegro.pas.
 * Author: Ñuño Martínez <niunio@users.sourceforge.net>
 *)

USES
  allegro,
  albitmap, { Bitmap manipulation. }
  aldigi,   { Digital sound. }
  alflic,   { FLIC animations. }
  algraph,  { Graphic mode configuration. }
  alpalete, { Color palette manipulation. }
  alsound,  { Sound configuration. }
  config,   { Management of the game configuration. }
  error,    { To show nice error messages. }
  framectr, { The frame speed controller. }
  game,     { Game main loop. }
  gamedata  { Management of the game data: graphics, sound, etc. }
;



  (* InitProgram:
   *   Initializes the program. *)
  FUNCTION InitProgram: BOOLEAN;
  VAR
    GDriver, Sw, Sh: INTEGER;
  BEGIN
    InitProgram := FALSE;
  { Install Allegro. }
    IF NOT al_init THEN
    BEGIN
      WriteLn ('Can''t initialize Allegro!');
      EXIT;
    END;
  { Get the configuration. }
    GetConfiguration;
  { Install Allegro modules.  Do it after set the configuracion file because
    Allegro will use it too. }
    al_install_keyboard;
    IF NOT al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_AUTODETECT) THEN
      al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_NONE);
    al_install_joystick (AL_JOY_TYPE_AUTODETECT);
    al_install_timer;
  { Set the graphic mode.  }
    al_set_color_depth (8);
    IF FullScreen THEN
    BEGIN
      GDriver := AL_GFX_AUTODETECT_FULLSCREEN;
      Sw := 320; Sh := 240;
    { Calculate the screen scale factor.  See the play loop. }
      ScaleSc := 1;
    END
    ELSE BEGIN
      GDriver := AL_GFX_AUTODETECT_WINDOWED;
      Sw := 640; sh := 480;
    { Calculate the screen scale factor.  See the play loop. }
      ScaleSc := 2;
    END;
    IF  NOT al_set_gfx_mode (GDriver, Sw, Sh, 0, 0) THEN
    BEGIN
      IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
	IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
	BEGIN
	  al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0); { Be sure it's closed. }
	{ Show an error message.
	  Can't use 'ErrorMessage' because the graphic mode isn't up. }
	  al_message (al_error);
	{ Shutdown Allegro. }
	  al_exit;
	  EXIT;
	END;
    { Calculate the screen scale factor.  See the play loop. }
      ScaleSc := 2;
    END;
    al_set_window_title ('Allegro.pas Demo Game');
  { Don't let Allegro twist colors. }
    al_set_color_conversion (AL_COLORCONV_NONE);
  { Set the frame controller. }
    IF NOT InstallFrameRate THEN
    BEGIN
      ErrorMessage ('Can''t install frame controller.  More timer interrupts needed.');
    { Shutdown Allegro. }
      al_exit;
      EXIT;
    END;
  { Load the game data. }
    IF NOT LoadData THEN
    BEGIN
      ErrorMessage ('Can''t load the game data.');
    { Shutdown Allegro. }
      al_exit;
      EXIT;
    END;
  { Initialize the random number generator. }
    RANDOMIZE;
    InitProgram := TRUE;
  { Wait few seconds.  This is because some modern screens needs time to get
    synchronised. }
    al_rest (2000);
  END;



  (* Intro:
   *   Shows an introduction animation with a fanfarre. *)
  PROCEDURE Intro;
  VAR
    Bmp: AL_BITMAPptr; { This bitmap helps to center the animation. }
  BEGIN
  { Play the fanfarre. }
    al_play_sample (Data^[INTRO_SPL].dat, 255, 128, 1000, 0);
  { Create a sub-bitmap in the center of the screen. }
    Bmp := al_create_sub_bitmap (al_screen,
				AL_SCREEN_W DIV 2 - 160,
				AL_SCREEN_H DIV 2 - 100,
				320, 200);
  { Show the animation. }
    al_play_memory_fli (Data^[INTRO_ANIM].dat, Bmp, FALSE, NIL);
  { Destroy the bitmap, because we don't need it anymore. }
    al_destroy_bitmap (Bmp);
  { Wait a moment and fade out. }
    al_rest (1000);
    al_fade_out (1);
  END;



  (* EndProgram:
   *   Releases all resources and closes down the program. *)
  PROCEDURE EndProgram;
  BEGIN
  { Release resources. }
    ReleaseData;
  { Shutdown Allegro. }
    al_exit;
  END;



(* The program starts here. *)
BEGIN
  IF NOT InitProgram THEN EXIT;
  IF DoIntro THEN Intro;
  RunGame;
  EndProgram;
END.

