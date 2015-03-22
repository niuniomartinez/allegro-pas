PROGRAM exmidi;
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
 *	This program demonstrates how to play MIDI files.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro, sysutils;

  VAR
    TheMusic: AL_MIDIptr;
    TheLength, Position: INTEGER;
    Beats, Beat: INTEGER;
    x, y, tw, th, k: INTEGER;
    BackgroundColor, TextColor: INTEGER;
    Paused, Done: BOOLEAN;

BEGIN (* The program starts here. *)

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;

  IF ParamCount <> 1 THEN
  BEGIN
    al_message ('Usage: "exmidi filename.mid"'#10);
    EXIT;
  END;

  al_install_keyboard;
  al_install_timer;

{ install a MIDI sound driver }
  IF NOT al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_AUTODETECT) THEN
  BEGIN
    al_message ('Error initialising sound system'#10+al_error+''#10);
    EXIT;
  END;

{ read in the MIDI file }
  TheMusic := al_load_midi (ParamStr (1));
  IF TheMusic = NIL THEN
  BEGIN
    al_message ('Error reading MIDI file '+ParamStr (1)+''#10);
    EXIT;
  END;
  TheLength := al_get_midi_length (TheMusic);
  Beats := -al_midi_pos; { al_get_midi_length updates midi_pos to the negative
                           number of beats (quarter notes) in the midi. }

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

{ try to continue in the background }
{
   IF NOT al_set_display_switch_mode (AL_SWITCH_BACKGROUND) THEN
      al_set_display_switch_mode (AL_SWITCH_BACKAMNESIA);
}

  al_set_palette (al_desktop_palette);
  BackgroundColor := al_makecol (255, 255, 255);
  TextColor := al_makecol (0, 0, 0);
  al_clear_to_color (al_screen, BackgroundColor);
  th := al_text_height (al_font);
  x := AL_SCREEN_W DIV 2;

  al_textout_centre_ex (al_screen, al_font, 'Playing '+ParamStr(1),
	x, AL_SCREEN_H DIV 3, TextColor, -1);

{ start up the MIDI file }
  al_play_midi (TheMusic, TRUE);

  y := 2 * AL_SCREEN_H DIV 3;
  tw := al_text_length (al_font, '0000:00 / 0000:00');
  Paused := FALSE;
  Done := FALSE;
{ wait for a key press }
  WHILE NOT Done DO
  BEGIN
  { P key pauses/resumes, any other key exits. }
    WHILE al_keypressed DO
    BEGIN
      K := al_readkey AND 255;
      IF K = Ord ('p') THEN
      BEGIN
        Paused := NOT Paused;
	IF Paused THEN
	BEGIN
	  al_midi_pause;
	  al_textout_centre_ex (al_screen, al_font, 'P A U S E D',
		x, y + th * 3, TextColor, -1);
	END
	ELSE BEGIN
	  al_midi_resume;
	  al_rectfill (al_screen, x - tw DIV 2, y + th *3,
		x + tw DIV 2, y + th * 4, BackgroundColor);
	END;
      END ELSE
        Done := TRUE;
    END;
    Position := al_midi_time;
    Beat := al_midi_pos;
    al_rectfill (al_screen, x - tw DIV 2, y, x + tw DIV 2, y + th * 2, BackgroundColor);
    al_textout_centre_ex (al_screen, al_font,
	Format ('%d:%02d / %d:%02d', [Position DIV 60, Position MOD 60, TheLength DIV 60, TheLength MOD 60]),
	x, y, TextColor, -1);
    al_textout_centre_ex (al_screen, al_font,
	Format ('beat %d /%d', [Beat, Beats]),
	x, y + th, TextColor, -1);
  { We have nothing else to do. }
    al_rest (100);
  END;

{ destroy the MIDI file }
  al_destroy_midi (TheMusic);
END.
