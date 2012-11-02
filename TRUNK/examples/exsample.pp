PROGRAM exsample;
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
 *	This program demonstrates how to play samples. You have to
 *	use this example from the command line to specify as first
 *	parameter a WAV or VOC sound file to play. If the file is
 *	loaded successfully, the sound will be played in an infinite
 *	loop. While it is being played, you can use the left and right
 *	arrow keys to modify the panning of the sound. You can also
 *	use the up and down arrow keys to modify the pitch.
 *
 *	by Ñuño Martínez <>
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *)

  USES
    allegro;

  VAR
   TheSample: AL_SAMPLEptr;
   Pan: INTEGER = 128;
   Pitch: INTEGER = 1000;

BEGIN (* The program starts here. *)

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;

  IF ParamCount <> 1 THEN
  BEGIN
    al_message ('Usage: "exsample filename.[wav|voc]"'#10);
    EXIT;
  END;

  al_install_keyboard;
  al_install_timer;

{ install a digital sound driver }
  IF NOT al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_NONE) THEN
  BEGIN
    al_message ('Error initialising sound system'#10+al_error+''#10);
    EXIT;
  END;

{ read in the WAV file }
  TheSample := al_load_sample (ParamStr (1));
  IF TheSample = NIL THEN
  BEGIN
    al_message ('Error reading sample file '+ParamStr (1)+''#10);
    EXIT;
  END;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

{
   IF NOT al_set_display_switch_mode (AL_SWITCH_BACKGROUND) THEN
      al_set_display_switch_mode (AL_SWITCH_BACKAMNESIA);
}

  al_set_palette (al_desktop_palette);
  al_clear_to_color (al_screen, al_makecol (255,255,255));

  al_textout_centre_ex (al_screen, al_font, 'Playing '+ParamStr(1),
	AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 3, al_makecol (0, 0, 0), -1);
  al_textout_centre_ex (al_screen, al_font, 'Use the arrow keys to adjust it',
	AL_SCREEN_W DIV 2, (AL_SCREEN_H * 2) DIV 3, al_makecol (0, 0, 0), -1);

{ start up the sample }
  al_play_sample (TheSample, 255, Pan, Pitch, -1);

  REPEAT
    al_poll_keyboard;

  { alter the pan position? }
    IF (al_key[AL_KEY_LEFT] <> 0) AND (Pan > 0) THEN
      DEC (Pan)
    ELSE IF (al_key[AL_KEY_RIGHT] <> 0) AND (Pan < 255) THEN
      INC (Pan);

  { alter the pitch? }
    IF (al_key[AL_KEY_UP] <> 0) AND (Pitch < 16384) THEN
      Pitch := ((Pitch * 513) DIV 512) + 1
    ELSE IF (al_key[AL_KEY_UP] <> 0) AND (Pitch > 64) THEN
      Pitch := ((Pitch * 511) DIV 512) - 1;

  { adjust the sample }
    al_adjust_sample (TheSample, 255, Pan, Pitch, -1);

  { delay a bit }
    al_rest (2);

  UNTIL (al_key[AL_KEY_ESC] <> 0) OR (al_key[AL_KEY_SPACE] <> 0);

{ destroy the sample }
  al_destroy_sample (TheSample);
END.
