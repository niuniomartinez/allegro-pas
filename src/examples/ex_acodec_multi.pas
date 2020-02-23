PROGRAM ex_acodec_multi;
(*
 * Milan Mimica (Translated to Pascal by Guillermo Martínez J.)
 * Audio example that plays multiple files at the same time
 * Originlly derived from the ex_acodec example.
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
  Common,
  allegro5, al5base, al5audio, al5acodec, al5strings;

VAR
  i: INTEGER;
  SampleData: ARRAY OF ALLEGRO_SAMPLEptr;
  Samples: ARRAY OF ALLEGRO_SAMPLE_INSTANCEptr;
  Mixer: ALLEGRO_MIXERptr;
  Voice: ALLEGRO_VOICEptr;
  LongestSample, SampleTime: REAL;
  FileName: AL_STR;

BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  IF ParamCount < 1 THEN
  BEGIN
    LogWriteLn ('This example needs to be run from the command line.');
    LogWrite ('Usage: ex_acodec_multi {audio_files}.');
    CloseLog (TRUE);
    HALT
  END;

  al_init_acodec_addon;

  IF NOT al_install_audio THEN AbortExample ('Could not init sound!');

  SetLength (Samples, ParamCount);
  SetLength (SampleData, ParamCount);

{ a voice is used for playback }
  Voice := al_create_voice
    (44100, ALLEGRO_AUDIO_DEPTH_INT16, ALLEGRO_CHANNEL_CONF_2);
  IF Voice = NIL THEN
    AbortExample ('Could not create ALLEGRO_VOICE from sample.');

  Mixer := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  IF Mixer = NIL THEN AbortExample ('al_create_mixer failed.');

  IF NOT al_attach_mixer_to_voice (Mixer, Voice) THEN
    AbortExample ('al_attach_mixer_to_voice failed.');

  FOR I := 1 TO ParamCount DO
  BEGIN
    FileName := al_string_to_str (ParamStr (i));
    Samples[i - 1] := NIL;

  { loads the entire sound file from disk into sample data }
    SampleData[i - 1] := al_load_sample (FileName);
    IF SampleData[i - 1] = NIL THEN
      AbortExample ('Could not load sample from ''' + FileName + '''!');

    Samples[i - 1] := al_create_sample_instance (SampleData[i - 1]);
    IF Samples[i - 1] = NIL THEN
    BEGIN
      LogWriteLn ('Could not create sample!');
      al_destroy_sample (SampleData[i - 1]);
      SampleData[i - 1] := NIL
    END
    ELSE IF NOT al_attach_sample_instance_to_mixer (Samples[i - 1], Mixer) THEN
      LogWriteLn ('al_attach_sample_instance_to_mixer failed.')
  END;

  LongestSample := 0;

  FOR I := 1 TO ParamCount DO
    IF Samples[i - 1] <> NIL THEN
    BEGIN
      FileName := al_string_to_str (ParamStr (i));
    { play each sample once }
      al_play_sample_instance (Samples[i - 1]);

      SampleTime := al_get_sample_instance_time (Samples[i - 1]);
      LogPrintLn ('Playing ''%s'' (%.3f seconds)', [FileName, SampleTime]);

      IF SampleTime > LongestSample THEN LongestSample := SampleTime
    END;

  al_rest (LongestSample);

  LogWrite ('Done.');

  FOR i := LOW (Samples) TO HIGH (Samples) DO
  { free the memory allocated when creating the sample + voice }
    IF Samples[i] <> NIL THEN
    BEGIN
      al_stop_sample_instance (Samples[i]);
      al_destroy_sample_instance (Samples[i]);
      al_destroy_sample (SampleData[i])
    END;
  al_destroy_mixer (Mixer);
  al_destroy_voice (Voice);

  al_uninstall_audio;

  CloseLog (TRUE)
END.
