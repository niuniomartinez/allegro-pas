program ex_15_audio;
(* Show basic usage of the audio subsystem. *)
(*
  Copyright (c) 2025 Guillermo Martínez J.

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
    allegro5  in '../lib/allegro5.pas',
    al5audio  in '../lib/al5audio.pas',
    al5acodec in '../lib/al5acodec.pas',
    al5font   in '../lib/al5font.pas',
    al5strings in '../lib/al5strings.pas',
    sysutils;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;
  (* Sound. *)
    ReservedSamples = 16;
    MaxSamples = 9;
    DefaultSampleNames: array [0 .. 8] of String = (
      'data/welcome.wav',
      'data/haiku/air_0.ogg', 'data/haiku/air_1.ogg',
      'data/haiku/earth_0.ogg', 'data/haiku/earth_1.ogg',
      'data/haiku/fire_0.ogg', 'data/haiku/fire_1.ogg',
      'data/haiku/water_0.ogg', 'data/haiku/water_1.ogg'
    );
  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* An event. *)
    Event: ALLEGRO_EVENT;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To store a text font. *)
    TextFont: ALLEGRO_FONTptr;
  (* To know when to stop. *)
    Terminated: Boolean;
  (* Sound samples. *)
    SampleNames: array of String;
    SampleList: array of ALLEGRO_SAMPLEptr;
  (* To print errors or actions. *)
    Message: String;

(* Program initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_font_addon then
    begin
      WriteLn ('Can''t use text fonts!');
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
    TextFont := al_create_builtin_font;
    if not alcon.Initialize then Exit (False);
  { Initialize the audio subsystem.  al5acodec doesn't need to be initialized. }
    if not al_install_audio then
    begin
      ErrorMessage ('Can''t initialize audio subsystem.');
      Exit (False)
    end;
    if not al_init_acodec_addon then
    begin
      ErrorMessage ('Can''t register audio codecs.');
      Exit (False)
    end;
    if not al_reserve_samples (ReservedSamples) then
    begin
      ErrorMessage ('Can''t reserve enough samples.');
      Exit (False)
    end;
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



(* Program finalization. *)
  procedure Finalize;
  var
    lNdx: Integer;
  begin
    alcon.Finalize;
    if Length (SampleList) > 0 then
      for lNdx := Low (SampleList) to High (SampleList) do
        if Assigned (SampleList[lNdx]) then
          al_destroy_sample (SampleList[lNdx]);
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (TextFont) then al_destroy_font (TextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;
  var
    lNdx: Integer;
  begin
    ClrScr;
    PrintLn ('');
    PrintLn (' CONTROLS');
    PrintLn ('  1-9       - play the sounds.');
    PrintLn ('  Alt 1-9   - play sounds with regular looping.');
    PrintLn ('  Ctrl 1-9  - play sounds with bidirectional looping.');
    PrintLn ('  Shift 1-9 - play sounds reversed.');
    PrintLn ('  SPACE     - stop all sounds.');
    PrintLn ('');
    PrintLn (' SOUNDS.');
    for lNdx := Low (SampleNames) to High (SampleNames) do
      PrintLn ('  %d - %s', [lNdx + 1, SampleNames[lNdx]]);
    PrintLn ('');
    PrintLn (Message);
  { Make it visible. }
    DrawConsole;
    al_flip_display
  end;



(* Process keyboard event. *)
  procedure ProcessKeyboardEvent (aKeyboard: ALLEGRO_KEYBOARD_EVENT);

    function PlaySound (
      const aSample: Integer;
      const aMode: ALLEGRO_PLAYMODE;
      const aReversed: Boolean
    ): Boolean;
    var
      lNewSampleId: ALLEGRO_SAMPLE_ID;
      lSampleInstance: ALLEGRO_SAMPLE_INSTANCEptr;
    begin
      Message := '';
    { Check sound exists. }
      if aSample > High (SampleList) then Exit (False);
      if not Assigned (SampleList[aSample]) then Exit (False);
    { Play sound. }
      Result := al_play_sample (
        SampleList[aSample],
        1, 0, 1, aMode,
        @lNewSampleId
      );
      if not Result then Exit;
    { Reverse effect. }
      if aReversed then
      begin
        lSampleInstance := al_lock_sample_id (lNewSampleId);
        al_set_sample_instance_position (
          lSampleInstance,
          al_get_sample_instance_length (lSampleInstance) - 1
        );
        al_set_sample_instance_speed (lSampleInstance, -1);
        al_unlock_sample_id (lNewSampleId)
      end
    end;

  var
    lPlaymode: ALLEGRO_PLAYMODE;
    lPlaymodeStr: String;
  begin
    case aKeyboard.keycode of
    ALLEGRO_KEY_ESCAPE:
      Terminated := True;
    ALLEGRO_KEY_1 .. ALLEGRO_KEY_9:
      begin
        if (aKeyboard.modifiers and ALLEGRO_KEYMOD_ALT) <> 0 then
        begin
          lPlaymode := ALLEGRO_PLAYMODE_LOOP;
          lPlaymodeStr := 'on a loop'
        end
        else if (aKeyboard.modifiers and ALLEGRO_KEYMOD_CTRL) <> 0 then
        begin
          lPlaymode := ALLEGRO_PLAYMODE_BIDIR;
          lPlaymodeStr := 'on a bidirectional loop'
        end
        else
        begin
          lPlaymode := ALLEGRO_PLAYMODE_ONCE;
          lPlaymodeStr := 'once'
        end;
        if PlaySound (
          aKeyboard.keycode - ALLEGRO_KEY_0 - 1,
          lPlaymode,
          (aKeyboard.modifiers and ALLEGRO_KEYMOD_SHIFT) <> 0
        ) then
          Message := Concat ('Playing ', lPlaymodeStr)
        else
          Message := Concat ('Playing failed.  Perhaps too many sounds.')
      end;
    ALLEGRO_KEY_SPACE:
      begin
        Message := 'Stopping ald sounds.';
        al_stop_samples
      end;
    end
  end;



(* Get sample names *)
  function GetSamples: Boolean;

    procedure GetNames;
    var
      lNumSamples, lNdx: Integer;
    begin
    { Get names. }
      if ParamCount > 0 then
      begin
        lNumSamples := Clamp (1, ParamCount, MaxSamples);
        SetLength (SampleNames, lNumSamples);
        for lNdx := 1 to lNumSamples do
          SampleNames[lNdx - 1] := ParamStr (lNdx)
      end
      else
      begin
        SetLength (SampleNames, Length (DefaultSampleNames));
        for lNdx := Low (DefaultSampleNames) to High (DefaultSampleNames) do
          SampleNames[lNdx] := DefaultSampleNames[lNdx]
      end
    end;

    function LoadSamples: Boolean;
    var
      lNdx: Integer;
    begin
      SetLength (SampleList, Length (SampleNames));
      for lNdx := Low (SampleNames) to High (SampleNames) do
      begin
        SampleList[lNdx] := al_load_sample (al_string_to_str (SampleNames[lNdx]));
        if not Assigned (SampleList[lNdx]) then
        begin
          ErrorMessage (al_str_format (
            'Could not load sample from "%s"!',
            [SampleNames[lNdx]]
          ));
          Exit (False)
        end
      end;
      Result := True
    end;

  begin
    GetNames;
    Result := LoadSamples
  end;

begin
{ Program initialization. }
  if not Initialize then Exit;
{ Get sample list. }
  if not GetSamples then
  begin
    Finalize;
    Exit
  end;
{ The main loop of the program. }
  Terminated := False;
  repeat
  { Window update. }
    if al_is_event_queue_empty (EventQueue) then UpdateScreen;
  { Check events. }
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    { ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed. }
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    { ALLEGRO_EVENT_KEY_CHAR - a keyboard key was pressed. }
    ALLEGRO_EVENT_KEY_CHAR:
      ProcessKeyboardEvent (Event.keyboard);
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
