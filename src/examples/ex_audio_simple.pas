PROGRAM ex_audio_simple;
(*    Example program for the Allegro library.
 *
 *    Demonstrate 'simple' audio interface.
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
  Allegro5, al5acodec, al5audio, al5base, al5strings, Common;

const
  RESERVED_SAMPLES = 16;
  MAX_SAMPLE_DATA  = 10;

var
  SampleData: array [1..MAX_SAMPLE_DATA] of ALLEGRO_SAMPLEptr;
  Display: ALLEGRO_DISPLAYptr;
  EventQueue: ALLEGRO_EVENT_QUEUEptr;
  Event: ALLEGRO_EVENT;
  Ndx: Integer;
  FileName: AL_STR;
  EndLoop: Boolean;

begin
  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

  if ParamCount < 1 then
  begin
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_audio_simple {audio_files}');
    CloseLog (True);
    HALT (1)
  end;

  al_install_keyboard;

  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Could not create display');

  EventQueue := al_create_event_queue;
  al_register_event_source (EventQueue, al_get_keyboard_event_source);
  al_register_event_source (EventQueue, al_get_display_event_source (Display));

  al_init_acodec_addon;
  if not al_install_audio then
    AbortExample ('Could not init sound!');
  if not al_reserve_samples (RESERVED_SAMPLES) then
    AbortExample ('Could not set up voice and mixer.');

  for Ndx := Low (SampleData) to High (SampleData) do
  begin
    if Ndx <= ParamCount then
    begin
      FileName := al_string_to_str (ParamStr (Ndx));
    { Load the entire sound file from disk. }
      SampleData[Ndx] := al_load_sample (FileName);
      if SampleData[Ndx] = Nil then
        LogPrintLn ('Could not load sample from "%s"!', [FileName])
    end
    else
      SampleData[Ndx] := Nil
  end;

  LogWriteLn ('Press digits to play sounds, space to stop sounds, Escape to quit.');
  EndLoop := False;
  repeat
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype OF
    ALLEGRO_EVENT_KEY_CHAR:
      begin
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
          EndLoop := True;
        if Event.keyboard.unichar = Ord (' ') then
          al_stop_samples
        else if (Ord ('0') <= Event.keyboard.unichar)
        and (Event.keyboard.unichar <= Ord ('9')) then
        begin
          if Event.keyboard.unichar = Ord ('0') then
            Ndx := 10
          else
            Ndx := (Event.keyboard.unichar - Ord ('0') + 10) mod 10;
          if SampleData[Ndx] <> Nil then
          begin
            LogPrintLn ('Playing %d', [Ndx]);
            if not al_play_sample (SampleData[Ndx], 1, 0.5, 1, ALLEGRO_PLAYMODE_LOOP, Nil) then
              LogWriteLn ('al_play_sample_data failed, perhaps too many sounds');
          end
        end
      end;
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndLoop := True;
    end
  until EndLoop;
{ Sample data and other objects will be automatically freed. }
  al_uninstall_audio;
  CloseLog (False)
end.
