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

uses
  Common,
  allegro5, al5base, al5audio, al5acodec, al5strings;

var
  Voice: ALLEGRO_VOICEptr;
  Mixer: ALLEGRO_MIXERptr;
  SubMixer: array [0..1] of ALLEGRO_MIXERptr;
  Samples: array [0..1] of ALLEGRO_SAMPLE_INSTANCEptr;
  SampleData: array [0..1] of ALLEGRO_SAMPLEptr;
  SampleTime, MaxSampleTime: Real;
  i: Integer;
  FileName: AL_STR;

begin
  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

  if ParamCount < 2 then
  begin
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_mixer_chain file1 file2');
    CloseLog (True);
    Halt
  end;

  al_init_acodec_addon;

  if not al_install_audio then AbortExample ('Could not init sound!');

  Voice := al_create_voice
    (44100, ALLEGRO_AUDIO_DEPTH_INT16, ALLEGRO_CHANNEL_CONF_2);
  if Voice = Nil then AbortExample ('Could not create ALLEGRO_VOICE.');

  Mixer := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  SubMixer[0] := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  SubMixer[1] := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  if (Mixer = Nil) or (SubMixer[0] = Nil) or (SubMixer[1] = Nil) then
    AbortExample ('al_create_mixer failed.');

  if not al_attach_mixer_to_voice (Mixer, Voice) then
      AbortExample ('al_attach_mixer_to_voice failed.');

  for i := 0 to 1 do
  begin
    FileName := al_string_to_str (ParamStr (i + 1));
    SampleData[i] := al_load_sample (FileName);
    if SampleData[i] = Nil then
      AbortExample (al_str_format ('Could not load sample from ''%s''!', [filename]));
    Samples[i] := al_create_sample_instance (Nil);
    if Samples[i] = Nil then AbortExample ('al_create_sample failed.');
    if not al_set_sample (Samples[i], SampleData[i]) then
      AbortExample ('al_set_sample failed.');
    if not al_attach_sample_instance_to_mixer (Samples[i], SubMixer[i]) then
      AbortExample ('al_attach_sample_instance_to_mixer failed.');
    if not al_attach_mixer_to_mixer (SubMixer[i], Mixer) then
      AbortExample ('al_attach_mixer_to_mixer failed.')
  end;

{ Play sample in looping mode. }
  for i := 0 to 1 do
  begin
    al_set_sample_instance_playmode (Samples[i], ALLEGRO_PLAYMODE_LOOP);
    al_play_sample_instance (Samples[i])
  end;

  MaxSampleTime := al_get_sample_instance_time (Samples[0]);
  SampleTime := al_get_sample_instance_time (Samples[1]);
  if SampleTime > MaxSampleTime then MaxSampleTime := SampleTime;

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
  for i := 0 to 1 do
  begin
    al_set_sample (Samples[i], Nil);
    al_destroy_sample (SampleData[i]);
    al_destroy_sample_instance (Samples[i]);
    al_destroy_mixer (SubMixer[i])
  end;
  al_destroy_mixer (Mixer);
  al_destroy_voice (Voice);

  al_uninstall_audio;

  CloseLog (True)
end.
