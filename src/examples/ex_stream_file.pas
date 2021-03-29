PROGRAM ex_stream_file;
(*
 * An example program that plays a file from the disk using Allegro5
 * streaming API. The file is being read in small chunks and played on the
 * sound device instead of being loaded at once.
 *
 * usage: ./ex_stream_file file.[wav,ogg...] ...
 *
 * by Milan Mimica (Translated to Pascal by Gillermo Martínez J.)
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

(* Attaches the stream directly to a voice. Streamed file's and voice's sample
 * rate, channels and depth must match.
 *)
{ -- $define BYPASS_MIXER }

var
  i, ArgStart: Integer;
  Voice: ALLEGRO_VOICEptr;
  Mixer: ALLEGRO_MIXERptr;
  Loop, Playing: Boolean;
  Stream: ALLEGRO_AUDIO_STREAMptr;
  FileName: AL_STR;
  Event: ALLEGRO_EVENT;
  Queue: ALLEGRO_EVENT_QUEUEptr;

begin
  Loop := False;
  ArgStart := 1;

  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

  if ParamCount < 1 then
  begin
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_stream_file [--loop] {audio_files}');
    CloseLog (True)
  end;

  if ParamStr (1) = '--loop' then
  begin
    Loop := True;
    ArgStart := 2
  end;

  al_init_acodec_addon;

  if not al_install_audio then AbortExample ('Could not init sound!');

  Voice := al_create_voice
    (44100, ALLEGRO_AUDIO_DEPTH_INT16, ALLEGRO_CHANNEL_CONF_2);
  if Voice = Nil then AbortExample ('Could not create ALLEGRO_VOICE.');
  LogWriteLn ('Voice created.');

{$IF NOT DEFINED(BYPASS_MIXER) }
  Mixer := al_create_mixer
    (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2);
  if Mixer = Nil then AbortExample ('Could not create ALLEGRO_MIXER.');
  LogWriteLn ('Mixer created.');

  if not al_attach_mixer_to_voice (Mixer, Voice) then
    AbortExample ('al_attach_mixer_to_voice failed.');
{$ENDIF }

  for i := ArgStart to ParamCount do
  begin
    FileName := al_string_to_str (ParamStr (i));
    Playing := True;
    Queue := al_create_event_queue;

    Stream := al_load_audio_stream (FileName, 4, 2048);
    if Stream = Nil then
      AbortExample (al_str_format (
        'Could not create an ALLEGRO_AUDIO_STREAM from "%s"!', [FileName]
      ));
    LogPrintLn ('Stream created from "%s".', [FileName]);
    if Loop then
      al_set_audio_stream_playmode (Stream, ALLEGRO_PLAYMODE_LOOP)
    else
      al_set_audio_stream_playmode (Stream, ALLEGRO_PLAYMODE_ONCE);

    al_register_event_source (Queue, al_get_audio_stream_event_source (Stream));

{$IF NOT DEFINED(BYPASS_MIXER) }
    if not al_attach_audio_stream_to_mixer (Stream, Mixer) then
      LogWriteLn ('al_attach_audio_stream_to_mixer failed')
{$ELSE }
    if not al_attach_audio_stream_to_voice (Stream, Voice) then
      AbortExample ('al_attach_audio_stream_to_voice failed.')
{$ENDIF }
    else begin
      LogPrint ('Playing %s ... Waiting for stream to finish ', [FileName]);
      repeat
	al_wait_for_event (Queue, @Event);
	if Event.ftype = ALLEGRO_EVENT_AUDIO_STREAM_FINISHED then
	  Playing := False
      until not Playing;
      LogWriteLn (' ')
    end;
    al_destroy_event_queue (Queue);
    al_destroy_audio_stream (Stream)
  end;
  LogWriteLn ('Done.');

{$IF NOT DEFINED(BYPASS_MIXER) }
  al_destroy_mixer (Mixer);
{$ENDIF }
  al_destroy_voice (Voice);

  al_uninstall_audio;

  CloseLog (True)
end.
