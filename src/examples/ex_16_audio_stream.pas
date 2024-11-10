program ex_16_audio_stream;
(* An example program that plays a file from disk using Allegro5 streaming
 * API. The file is being read in small chunks and played on the sound device
 * instead of being loaded at once.
 *
 * Original by by Milan Mimica (Translated to Pascal by Gillermo Martínez J.)
 *)
(*
  Copyright (c) 2012-2024 Guillermo Martínez J.

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
    alcon, Common,
    allegro5, al5audio, al5acodec, al5strings,
    sysutils;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
(* Attaches the stream directly to a voice. Streamed file's and voice's sample
 * rate, channels and depth must match.
 *)
{ -- $define BYPASS_MIXER }

  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To know when to stop. *)
    Terminated: Boolean;
  (* First argument with files to play. *)
    ArgStart: Integer;
  (* Sound stuff. *)
    Voice: ALLEGRO_VOICEptr;
    Mixer: ALLEGRO_MIXERptr;
    PlayLoop: Boolean;

(* Update window content. *)
  procedure UpdateScreen;
  begin
    DrawConsole;
    al_flip_display
  end;



(* Program initialization. *)
  function Initialize: Boolean;

  (* Initialize Allegro basics. *)
    function InitializeAllegro: Boolean;
    begin
    { Initialize Allegro. }
      if not al_init or not al_install_keyboard then
      begin
        WriteLn ('Can''t initialize Allegro!');
        Exit (False)
      end;
    { Create window. }
      al_set_new_display_flags (ALLEGRO_WINDOWED);
      Window := al_create_display (wWidth, wHeight);
      if not Assigned (Window) then
      begin
        WriteLn ('Can''t create window.');
        Exit (False)
      end;
      if not alcon.Initialize then Exit (False);
    { Create the event queue. }
      EventQueue := al_create_event_queue;
      if not Assigned (EventQueue) then
      begin
        ErrorMessage ('Can''t initialize event queue!');
        Exit (False)
      end;
      al_register_event_source (EventQueue, al_get_keyboard_event_source);
      al_register_event_source (EventQueue, al_get_display_event_source (Window));

      Result := True
    end;

  (* Get information from params list. *)
    function GetParams: Boolean;
    begin
      ArgStart := 1;
      if ParamCount < 1 then
      begin
        PrintLn ('This example needs to be run from the command line.');
        PrintLn ('Usage: ex_stream_file [--loop] {audio_files}');
        PrintLn ('');
        PrintLn ('Press [C] to close');
        UpdateScreen;
        WaitCPress;
        Exit (False)
      end;
      if ParamStr (1) = '--loop' then
      begin
        PlayLoop := True;
        ArgStart := 2
      end;
      Result := True
    end;

  { initialize sound subsystem. }
    function InitSound: Boolean;
    begin
    { Init Allegro's sound subsystem. }
      if not al_install_audio or not al_init_acodec_addon then
      begin
        ErrorMessage ('Could not initialize sound subsystem.');
        Exit (False)
      end;
    { Create voice. }
      Voice := al_create_voice (
        44100, ALLEGRO_AUDIO_DEPTH_INT16, ALLEGRO_CHANNEL_CONF_2
      );
      if not Assigned (Voice) then
      begin
        ErrorMessage ('Could not create ALLEGRO_VOICE.');
        Exit (False)
      end;
      PrintLn ('Voice created.');
    { Create mixer. }
      Mixer := al_create_mixer (
        44100, ALLEGRO_AUDIO_DEPTH_FLOAT32, ALLEGRO_CHANNEL_CONF_2
      );
      if not Assigned (Mixer) then
      begin
        ErrorMessage ('Could not create ALLEGRO_MIXER.');
        Exit (False)
      end;
      PrintLn ('Mixer created.');
      if not al_attach_mixer_to_voice (Mixer, Voice) then
      begin
        ErrorMessage ('al_attach_mixer_to_voice failed.');
        Exit (False)
      end;

      Result := True
    end;

  begin
    if not InitializeAllegro then Exit (False);
    if not GetParams then Exit (False);
    if not InitSound then Exit (False);
    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    alcon.Finalize;
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Voice) then al_destroy_voice (Voice);
    if Assigned (Mixer) then al_destroy_mixer (Mixer);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Plays a stream file. *)
  procedure PlayStreamFile (const aFile: String);

    function CreateStream (const aFile: String): ALLEGRO_AUDIO_STREAMptr;
    begin
      Result := al_load_audio_stream (al_string_to_str (aFile), 4, 2048);
      if not Assigned (Result) then
      begin
        PrintLn ('Could not create an audio stream from "%s"!', [aFile]);
        Exit
      end;
      PrintLn ('Stream created from "%s".', [aFile]);
    { Looping? }
      if PlayLoop then
        al_set_audio_stream_playmode (Result, ALLEGRO_PLAYMODE_LOOP)
      else
        al_set_audio_stream_playmode (Result, ALLEGRO_PLAYMODE_ONCE);
    { Add to the event queue so we can control it. }
      al_register_event_source (
        EventQueue,
        al_get_audio_stream_event_source (Result)
      );
    { Attach to mixer or voice. }
{$IF NOT DEFINED(BYPASS_MIXER) }
      if not al_attach_audio_stream_to_mixer (Result, Mixer) then
      begin
        PrintLn ('al_attach_audio_stream_to_mixer failed');
{$ELSE }
      if not al_attach_audio_stream_to_voice (Result, Voice) then
        PrintLn ('al_attach_audio_stream_to_voice failed.');
{$ENDIF }
        al_destroy_audio_stream (Result);
        Exit (Nil)
      end;
    end;

  var
    lEvent: ALLEGRO_EVENT;
    lStream: ALLEGRO_AUDIO_STREAMptr;
    lPlaying: Boolean;
  begin
  { Create audio stream. }
    lStream := CreateStream  (aFile);
    if not Assigned (lStream) then Exit;
    PrintLn ('Playing %s ... Waiting for stream to finish ', [ExtractFileName (aFile)]);
    lPlaying := True;
    repeat
      if al_is_event_queue_empty (EventQueue) then UpdateScreen;
      al_wait_for_event (EventQueue, @lEvent);
      case lEvent.ftype of
      { ALLEGTRO_EVENT_AUDIO_STREAM_FINISHED - the stream finished. }
      ALLEGRO_EVENT_AUDIO_STREAM_FINISHED:
        begin
          al_destroy_audio_stream (lStream);
          lPlaying := False
        end;
      { ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed. }
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        Terminated := True;
      { ALLEGRO_EVENT_KEY_CHAR - a keyboard key was pressed. }
      ALLEGRO_EVENT_KEY_CHAR:
        if lEvent.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Terminated := True;
      end
    until Terminated or not lPlaying;
    al_destroy_audio_stream (lStream)
  end;

var
  Ndx: Integer;
begin
{ Program initialization. }
  if not Initialize then Exit;
{ Play streams. }
  Ndx := ArgStart;
  repeat
    PlayStreamFile (ParamStr (Ndx));
    Inc (Ndx)
  until Terminated or (Ndx > ParamCount);
  Finalize
end.
