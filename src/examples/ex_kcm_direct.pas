PROGRAM ex_kcm_direct;
(* Shows the ability to play a sample without a mixer. *)
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
  Common,
  allegro5, al5base, al5audio, al5acodec, al5strings;



  PROCEDURE Play (FileName: AL_STR);
  VAR
    SampleData: ALLEGRO_SAMPLEptr;
    Sample: ALLEGRO_SAMPLE_INSTANCEptr;
    Voice: ALLEGRO_VOICEptr;
    Chan, Depth: INTEGER;
    Freq: LONGWORD;
    SampleTime: REAL;
  BEGIN
  { Load the entire sound file from disk. }
    SampleData := al_load_sample (FileName);
    IF SampleData = NIL THEN
    BEGIN
      LogPrintLn ('Could not load sample from ''%s''!', [FileName]);
      EXIT
    END;

    Sample := al_create_sample_instance (NIL);
    IF Sample = NIL THEN
      AbortExample ('al_create_sample failed.');

    IF NOT al_set_sample (Sample, SampleData) THEN
    BEGIN
      LogWriteLn ('al_set_sample failed.');
      EXIT
    END;

    Depth := ORD (al_get_sample_instance_depth (Sample));
    IF Depth < 8 THEN Depth := 8 + Depth * 8 ELSE Depth := 0;
    Chan := ORD (al_get_sample_instance_channels (Sample));
    Freq := al_get_sample_instance_frequency (Sample);
    LogPrintLn (
      'Loaded sample: %d-bit depth, %d channels, %d Hz.',
      [Depth, (Chan SHR 4) + (Chan MOD $F), Freq]
    );
    LogWrite ('Trying to create a voice with the same specs... ');
    Voice := al_create_voice (
      Freq,
      al_get_sample_instance_depth (Sample),
      al_get_sample_instance_channels (Sample)
    );
    IF Voice = NIL THEN AbortExample ('Could not create ALLEGRO_VOICE.');
    LogWriteLn ('done.');

    IF NOT al_attach_sample_instance_to_voice (Sample, Voice) THEN
      AbortExample ('al_attach_sample_instance_to_voice failed.');

  { Play sample in looping mode. }
    al_set_sample_instance_playmode (Sample, ALLEGRO_PLAYMODE_LOOP);
    al_play_sample_instance (Sample);

    SampleTime := al_get_sample_instance_time (Sample);
    LogPrint (
      'Playing ''%s'' (%.3f seconds) 3 times',
      [FileName, SampleTime]
    );

    al_rest (SampleTime * 3);

    al_stop_sample_instance (Sample);
    LogWriteLn ('');

  { Free the memory allocated. }
    al_set_sample (Sample, NIL);
    al_destroy_sample (SampleData);
    al_destroy_sample_instance (Sample);
    al_destroy_voice (Voice)
  END;

VAR
  i: INTEGER;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  IF ParamCount < 1 THEN
  BEGIN
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_kcm_direct {audio_files}');
    CloseLog (TRUE);
    HALT
  END;

  al_init_acodec_addon;

  IF NOT al_install_audio THEN AbortExample ('Could not init sound!');

  FOR i := 1 TO ParamCount DO Play (al_string_to_str (ParamStr (i)));

  al_uninstall_audio;
  LogWriteLn ('Done');
  CloseLog (TRUE)
END.
