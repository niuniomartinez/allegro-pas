program ex_saw;
(* Recreate exstream.c from A4. *)
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
  {$POINTERMATH ON} { When compiled in Delphi mode, this is disabled. }
  {$RANGECHECKS OFF} { If set, example doesn't run. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

{$IFDEF DCC}
{ Pointer arithmetic needed by this example.
  Not all Delphi versions support it though. }
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion >= 20.0}
      {$POINTERMATH ON}
    {$ELSE}
      {$MESSAGE error 'Need pointer arithmetics.'}
    {$ENDIF}
  {$ELSE}
    {$MESSAGE error 'Need pointer arithmetics.'}
  {$ENDIF}
{$ENDIF}

  uses
    Common,
    Allegro5, al5base, al5audio, al5nativedlg;

  const
    SAMPLES_PER_BUFFER = 1024;


  procedure Saw (Stream: ALLEGRO_AUDIO_STREAMptr);
  var
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Buf: ^AL_INT8; { int8_t * }
    Pitch, Val, i, n: LongInt;
  begin
    Pitch := $10000;
    n := 200;
    Val := 0;

    Queue := al_create_event_queue;
    al_register_event_source (Queue, al_get_audio_stream_event_source (Stream));

    if TextLog <> Nil then
      al_register_event_source
        (Queue, al_get_native_text_log_event_source (TextLog));

    LogWriteLn ('Generating saw wave...');

    while n > 0 do
    begin
      al_wait_for_event (Queue, @Event);

      if Event.ftype = ALLEGRO_EVENT_AUDIO_STREAM_FRAGMENT then
      begin
	Buf := al_get_audio_stream_fragment (Stream);
	if Buf <> Nil then
	begin
	  for i := 0 to SAMPLES_PER_BUFFER - 1 do
	  begin
	  { Crude saw wave at maximum amplitude. Please keep this compatible
	    to the A4 example so we know when something has broken for now.

	    It would be nice to have a better example with user interface
	    and some simple synth effects. }
	    (Buf + i)^ := (Val shr 16) and $FF;
	    Inc (Val, Pitch);
	    Inc (Pitch)
	  end;

	  if not al_set_audio_stream_fragment (Stream, Buf) then
	    LogWriteLn ('Error setting stream fragment.');

	  Dec (n);
	  if n mod 10 = 0 then LogWrite ('.')
	end
      end;
      if Event.ftype = ALLEGRO_EVENT_NATIVE_DIALOG_CLOSE then
	n := -1 { See the loop condition. }
    end;

    al_drain_audio_stream (Stream);

    LogWriteLn ('');

    al_destroy_event_queue (Queue)
  end;



  var
    Stream: ALLEGRO_AUDIO_STREAMptr;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

  if not al_install_audio then AbortExample ('Could not init sound.');
  al_reserve_samples (0);

  Stream := al_create_audio_stream (
    8, SAMPLES_PER_BUFFER, 22050,
    ALLEGRO_AUDIO_DEPTH_UINT8, ALLEGRO_CHANNEL_CONF_1);
  if Stream = Nil then AbortExample ('Could not create stream.');

  if not al_attach_audio_stream_to_mixer (Stream, al_get_default_mixer) then
    AbortExample ('Could not attach stream to mixer.');

  OpenLog;

  Saw (Stream);

  CloseLog (False);

  al_drain_audio_stream (Stream);
  al_destroy_audio_stream (Stream);
  al_uninstall_audio;
end.
