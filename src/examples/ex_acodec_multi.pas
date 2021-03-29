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

uses
  Common,
  allegro5, al5base, al5audio, al5acodec, al5strings;

var
  i: Integer;
  SampleData: array of ALLEGRO_SAMPLEptr;
  Samples: array of ALLEGRO_SAMPLE_INSTANCEptr;
  Mixer: ALLEGRO_MIXERptr;
  Voice: ALLEGRO_VOICEptr;
  LongestSample, SampleTime: REAL;
  FileName: AL_STR;

begin
  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

  if ParamCount < 1 then
  begin
    LogWriteLn ('This example needs to be run from the command line.');
    LogWrite ('Usage: ex_acodec_multi {audio_files}.');
    CloseLog (True);
    HALT
  end;

  al_init_acodec_addon;

  if not al_install_audio then AbortExample ('Could not init sound!');

  SetLength (Samples, ParamCount);
  SetLength (SampleData, ParamCount);

{ a voice is used for playback }
  Voice := al_create_voice
    (44100, ALLEGRO_AUDIO_DEPTH_INT16, ALLEGRO_CHANNEL_CONF_2);
  if Voice = Nil then
    AbortExample ('Could not create ALLEGRO_VOICE from sample.');

  Mixer := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  if Mixer = Nil then AbortExample ('al_create_mixer failed.');

  if not al_attach_mixer_to_voice (Mixer, Voice) then
    AbortExample ('al_attach_mixer_to_voice failed.');

  for I := 1 to ParamCount do
  begin
    FileName := al_string_to_str (ParamStr (i));
    Samples[i - 1] := Nil;

  { loads the entire sound file from disk into sample data }
    SampleData[i - 1] := al_load_sample (FileName);
    if SampleData[i - 1] = Nil then
      AbortExample ('Could not load sample from ''' + FileName + '''!');

    Samples[i - 1] := al_create_sample_instance (SampleData[i - 1]);
    if Samples[i - 1] = Nil then
    begin
      LogWriteLn ('Could not create sample!');
      al_destroy_sample (SampleData[i - 1]);
      SampleData[i - 1] := Nil
    end
    else if not al_attach_sample_instance_to_mixer (Samples[i - 1], Mixer) then
      LogWriteLn ('al_attach_sample_instance_to_mixer failed.')
  end;

  LongestSample := 0;

  for I := 1 to ParamCount do
    if Samples[i - 1] <> Nil then
    begin
      FileName := al_string_to_str (ParamStr (i));
    { play each sample once }
      al_play_sample_instance (Samples[i - 1]);

      SampleTime := al_get_sample_instance_time (Samples[i - 1]);
      LogPrintLn ('Playing ''%s'' (%.3f seconds)', [FileName, SampleTime]);

      if SampleTime > LongestSample then LongestSample := SampleTime
    end;

  al_rest (LongestSample);

  LogWrite ('Done.');

  for i := Low (Samples) to High (Samples) do
  { free the memory allocated when creating the sample + voice }
    if Samples[i] <> Nil then
    begin
      al_stop_sample_instance (Samples[i]);
      al_destroy_sample_instance (Samples[i]);
      al_destroy_sample (SampleData[i])
    end;
  al_destroy_mixer (Mixer);
  al_destroy_voice (Voice);

  al_uninstall_audio;

  CloseLog (True)
end.
