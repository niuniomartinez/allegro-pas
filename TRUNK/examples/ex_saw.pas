PROGRAM ex_saw;
(* Recreate exstream.c from A4. *)

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5base, al5audio, al5nativedlg;

  CONST
    SAMPLES_PER_BUFFER = 1024;


  PROCEDURE Saw (Stream: ALLEGRO_AUDIO_STREAMptr);
  VAR
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Buf: ^AL_INT8; { int8_t * }
    Pitch, Val, i, n: LONGINT;
  BEGIN
    Pitch := $10000;
    n := 200;
    Val := 0;

    Queue := al_create_event_queue;
    al_register_event_source (Queue, al_get_audio_stream_event_source (Stream));

    IF TextLog <> NIL THEN
      al_register_event_source
        (Queue, al_get_native_text_log_event_source (TextLog));

    LogWriteLn ('Generating saw wave...');

    WHILE n > 0 DO
    BEGIN
      al_wait_for_event (Queue, Event);

      IF Event._type = ALLEGRO_EVENT_AUDIO_STREAM_FRAGMENT THEN
      BEGIN
	Buf := al_get_audio_stream_fragment (Stream);
	IF Buf <> NIL THEN
	BEGIN
	  FOR i := 0 TO SAMPLES_PER_BUFFER - 1 DO
	  BEGIN
	  { Crude saw wave at maximum amplitude. Please keep this compatible
	    to the A4 example so we know when something has broken for now.

	    It would be nice to have a better example with user interface
	    and some simple synth effects. }
	    Buf[i] := (Val SHR 16) AND $FF;
	    INC (Val, Pitch);
	    INC (Pitch)
	  END;

	  IF NOT al_set_audio_stream_fragment (Stream, Buf) THEN
	    LogWriteLn ('Error setting stream fragment.');

	  DEC (n);
	  IF n MOD 10 = 0 THEN LogWrite ('.')
	END
      END;
      IF Event._type = ALLEGRO_EVENT_NATIVE_DIALOG_CLOSE THEN
	n := -1 { See the loop condition. }
    END;

    al_drain_audio_stream (Stream);

    LogWriteLn ('');

    al_destroy_event_queue (Queue)
  END;



  VAR
    Stream: ALLEGRO_AUDIO_STREAMptr;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  IF NOT al_install_audio THEN AbortExample ('Could not init sound.');
  al_reserve_samples (0);

  Stream := al_create_audio_stream (
    8, SAMPLES_PER_BUFFER, 22050,
    ALLEGRO_AUDIO_DEPTH_UINT8, ALLEGRO_CHANNEL_CONF_1);
  IF Stream = NIL THEN AbortExample ('Could not create stream.');

  IF NOT al_attach_audio_stream_to_mixer (Stream, al_get_default_mixer) THEN
    AbortExample ('Could not attach stream to mixer.');

  OpenLog;

  Saw (Stream);

  CloseLog (FALSE);

  al_drain_audio_stream (Stream);
  al_destroy_audio_stream (Stream);
  al_uninstall_audio;
END.
