PROGRAM ex_audio_simple;
(*    Example program for the Allegro library.
 *
 *    Demonstrate 'simple' audio interface.
 *)
(*
  Copyright (c) 2012-2020 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

USES
  Allegro5, al5acodec, al5audio, al5base, al5strings, Common;

CONST
  RESERVED_SAMPLES = 16;
  MAX_SAMPLE_DATA  = 10;

VAR
  SampleData: ARRAY [1..MAX_SAMPLE_DATA] OF ALLEGRO_SAMPLEptr;
  Display: ALLEGRO_DISPLAYptr;
  EventQueue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Ndx: INTEGER;
  FileName: AL_STR;
  EndLoop: BOOLEAN;

BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  IF Paramcount < 1 THEN
  BEGIN
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_audio_simple {audio_files}');
    CloseLog (TRUE);
    HALT (1)
  END;

  al_install_keyboard;

  Display := al_create_display (640, 480);
  IF Display = NIL THEN AbortExample ('Could not create display');

  EventQueue := al_create_event_queue;
  al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));

  al_init_acodec_addon;
  IF NOT al_install_audio THEN
    AbortExample ('Could not init sound!');
  IF NOT al_reserve_samples (RESERVED_SAMPLES) THEN
    AbortExample ('Could not set up voice and mixer.');

  FOR Ndx := LOW (SampleData) TO HIGH (SampleData) DO
  BEGIN
    IF Ndx <= Paramcount THEN
    BEGIN
      FileName := al_string_to_str (ParamStr (Ndx));
    { Load the entire sound file from disk. }
      SampleData[Ndx] := al_load_sample (FileName);
      IF SampleData[Ndx] = NIL THEN
        LogPrintLn ('Could not load sample from "%s"!', [FileName])
    END
    ELSE
      SampleData[Ndx] := NIL
  END;

  LogWriteLn ('Press digits to play sounds, space to stop sounds, Escape to quit.');
  EndLoop := FALSE;
  REPEAT
    al_wait_for_event (EventQueue, @Event);
    CASE Event.ftype OF
    ALLEGRO_EVENT_KEY_CHAR:
      BEGIN
        IF Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE THEN
          EndLoop := TRUE;
        IF Event.keyboard.unichar = ORD (' ') THEN
          al_stop_samples
        ELSE IF (ORD ('0') <= Event.keyboard.unichar)
        AND (Event.keyboard.unichar <= ORD ('9')) THEN
        BEGIN
          IF Event.keyboard.unichar = ORD ('0') THEN
            Ndx := 10
          ELSE
            Ndx := (Event.keyboard.unichar - ORD ('0') + 10) MOD 10;
          IF SampleData[Ndx] <> NIL THEN
          BEGIN
            LogPrintLn ('Playing %d', [Ndx]);
            IF NOT al_play_sample (SampleData[Ndx], 1, 0.5, 1, ALLEGRO_PLAYMODE_LOOP, NIL) THEN
              LogWriteLn ('al_play_sample_data failed, perhaps too many sounds');
          END
        END
      END;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndLoop := TRUE;
    END
  UNTIL EndLoop;
{ Sample data and other objects will be automatically freed. }
  al_uninstall_audio;
  CloseLog (FALSE)
END.
