PROGRAM ex_mixer_chain;
(*
 *    Example program for the Allegro library.
 *
 *    Test chaining mixers to mixers.
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
  Voice: ALLEGRO_VOICEptr;
  Mixer: ALLEGRO_MIXERptr;
  SubMixer: ARRAY [0..1] OF ALLEGRO_MIXERptr;
  Samples: ARRAY [0..1] OF ALLEGRO_SAMPLE_INSTANCEptr;
  SampleData: ARRAY [0..1] OF ALLEGRO_SAMPLEptr;
  SampleTime, MaxSampleTime: REAL;
  i: INTEGER;
  FileName: AL_STR;

BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  IF ParamCount < 2 THEN
  BEGIN
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_mixer_chain file1 file2');
    CloseLog (TRUE);
    Halt
  END;

  al_init_acodec_addon;

  IF NOT al_install_audio THEN AbortExample ('Could not init sound!');

  Voice := al_create_voice
    (44100, ALLEGRO_AUDIO_DEPTH_INT16, ALLEGRO_CHANNEL_CONF_2);
  IF Voice = NIL THEN AbortExample ('Could not create ALLEGRO_VOICE.');

  Mixer := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  SubMixer[0] := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  SubMixer[1] := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  IF (Mixer = NIL) OR (SubMixer[0] = NIL) OR (SubMixer[1] = NIL) THEN
    AbortExample ('al_create_mixer failed.');

  IF NOT al_attach_mixer_to_voice (Mixer, Voice) THEN
      AbortExample ('al_attach_mixer_to_voice failed.');

  FOR i := 0 TO 1 DO
  BEGIN
    FileName := al_string_to_str (ParamStr (i + 1));
    SampleData[i] := al_load_sample (FileName);
    IF SampleData[i] = NIL THEN
      AbortExample (al_str_format ('Could not load sample from ''%s''!', [filename]));
    Samples[i] := al_create_sample_instance (NIL);
    IF Samples[i] = NIL THEN AbortExample ('al_create_sample failed.');
    IF NOT al_set_sample (Samples[i], SampleData[i]) THEN
      AbortExample ('al_set_sample failed.');
    IF NOT al_attach_sample_instance_to_mixer (Samples[i], SubMixer[i]) THEN
      AbortExample ('al_attach_sample_instance_to_mixer failed.');
    IF NOT al_attach_mixer_to_mixer (SubMixer[i], Mixer) THEN
      AbortExample ('al_attach_mixer_to_mixer failed.')
  END;

{ Play sample in looping mode. }
  FOR i := 0 TO 1 DO
  BEGIN
    al_set_sample_instance_playmode (Samples[i], ALLEGRO_PLAYMODE_LOOP);
    al_play_sample_instance (Samples[i])
  END;

  MaxSampleTime := al_get_sample_instance_time (Samples[0]);
  SampleTime := al_get_sample_instance_time (Samples[1]);
  IF SampleTime > MaxSampleTime THEN MaxSampleTime := SampleTime;

  LogWriteLn ('Playing...');

  al_rest (MaxSampleTime);

  al_set_sample_instance_gain (Samples[0], 0.5);
  al_rest (MaxSampleTime);

  al_set_sample_instance_gain (Samples[1], 0.25);
  al_rest (MaxSampleTime);

  al_stop_sample_instance (Samples[0]);
  al_stop_sample_instance(Samples[1]);
  LogWriteLn ('Done.');

{ Free the memory allocated. }
  FOR i := 0 TO 1 DO
  BEGIN
    al_set_sample (Samples[i], NIL);
    al_destroy_sample (SampleData[i]);
    al_destroy_sample_instance (Samples[i]);
    al_destroy_mixer (SubMixer[i])
  END;
  al_destroy_mixer (Mixer);
  al_destroy_voice (Voice);

  al_uninstall_audio;

  CloseLog (TRUE)
END.
