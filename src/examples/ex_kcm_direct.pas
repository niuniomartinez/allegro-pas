program ex_kcm_direct;
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

uses
  Common,
  allegro5, al5base, al5audio, al5acodec, al5strings;



  procedure Play (FileName: AL_STR);
  var
    SampleData: ALLEGRO_SAMPLEptr;
    Sample: ALLEGRO_SAMPLE_INSTANCEptr;
    Voice: ALLEGRO_VOICEptr;
    Chan, Depth: Integer;
    Freq: LongWord;
    SampleTime: Real;
  begin
  { Load the entire sound file from disk. }
    SampleData := al_load_sample (FileName);
    if SampleData = Nil then
    begin
      LogPrintLn ('Could not load sample from ''%s''!', [FileName]);
      Exit
    end;

    Sample := al_create_sample_instance (Nil);
    if Sample = Nil then
      AbortExample ('al_create_sample failed.');

    if not al_set_sample (Sample, SampleData) then
    begin
      LogWriteLn ('al_set_sample failed.');
      Exit
    end;

    Depth := Ord (al_get_sample_instance_depth (Sample));
    if Depth < 8 then Depth := 8 + Depth * 8 else Depth := 0;
    Chan := Ord (al_get_sample_instance_channels (Sample));
    Freq := al_get_sample_instance_frequency (Sample);
    LogPrintLn (
      'Loaded sample: %d-bit depth, %d channels, %d Hz.',
      [Depth, (Chan shr 4) + (Chan mod $F), Freq]
    );
    LogWrite ('Trying to create a voice with the same specs... ');
    Voice := al_create_voice (
      Freq,
      al_get_sample_instance_depth (Sample),
      al_get_sample_instance_channels (Sample)
    );
    if Voice = Nil then AbortExample ('Could not create ALLEGRO_VOICE.');
    LogWriteLn ('done.');

    if not al_attach_sample_instance_to_voice (Sample, Voice) then
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
    al_set_sample (Sample, Nil);
    al_destroy_sample (SampleData);
    al_destroy_sample_instance (Sample);
    al_destroy_voice (Voice)
  end;

var
  i: Integer;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

  if ParamCount < 1 then
  begin
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_kcm_direct {audio_files}');
    CloseLog (True);
    HALT
  end;

  al_init_acodec_addon;

  if not al_install_audio then AbortExample ('Could not init sound!');

  for i := 1 to ParamCount do Play (al_string_to_str (ParamStr (i)));

  al_uninstall_audio;
  LogWriteLn ('Done');
  CloseLog (True)
end.
