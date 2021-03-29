unit Common;
(* Common stuff for examples. *)
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
  {$IFNDEF FPC_DELPHI}{$MODE DELPHI}{$ENDIF}
{$ENDIF}

interface

  uses
    allegro5, al5base, al5nativedlg;

  var
    TextLog: ALLEGRO_TEXTLOGptr;

(* Initializes platform specific stuff. *)
  procedure InitPlatformSpecific;
(* Exits program with error. *)
  procedure AbortExample (const Message: AL_STR);
(* Opens a log window. *)
  procedure OpenLog;
  procedure OpenLogMonospace;
(* Closes the log window. *)
  procedure CloseLog (WaitForUser: Boolean);
(* Prints a message on the log window. *)
  procedure LogWrite (Str: AL_STR);
  procedure LogWriteLn (Str: AL_STR);
(* Prints a formatted message on the log window. *)
  procedure LogPrint (Fmt: AL_STR; Values: array of const);
  procedure LogPrintLn (Fmt: AL_STR; Values: array of const);

{$IFDEF DCC }
(* Delphi doesn't has GetTempFilename (or I can't find the declaration, and
   Internet failed to tell me) so let's define it. *)
  function GetTempFilename: AL_STR;
{$ENDIF }

implementation

  uses
    al5strings,
    sysutils;

(* Platform specific stuff. *)
  procedure InitPlatformSpecific;
  begin
  { TODO: Android stuff, if android. }
  end;



(* Exits program with error. *)
  procedure AbortExample (const Message: AL_STR);
  var
    Display: ALLEGRO_DISPLAYptr;
  begin
    if al_init_native_dialog_addon then
    begin
      if al_is_system_installed then
        Display := al_get_current_display
      else
        Display := Nil;
      al_show_native_message_box
        (Display, 'Error', 'Cannot run example', Message, '', 0)
    end
    else
      WriteLn (ErrOutput, Message);
    HALT (1)
  end;



(* Opens a log window. *)
  procedure OpenLog;
  begin
    if al_init_native_dialog_addon then
      TextLog := al_open_native_text_log ('Log', 0)
  end;



  procedure OpenLogMonospace;
  begin
    if al_init_native_dialog_addon then
      TextLog := al_open_native_text_log ('Log', ALLEGRO_TEXTLOG_MONOSPACE)
   end;



(* Closes the log window. *)
  procedure CloseLog (WaitForUser: Boolean);
  var
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
  begin
    if (TextLog <> Nil) and WaitForUser then
    begin
      Queue := al_create_event_queue;
      al_register_event_source (
        Queue,
        al_get_native_text_log_event_source (TextLog)
      );
      al_wait_for_event (Queue, @Event);
      al_destroy_event_queue (Queue)
    end;
    al_close_native_text_log (TextLog);
    TextLog := Nil
  end;


(* Prints a message on the log window. *)
  procedure LogWrite (Str: AL_STR);
  begin
    al_append_native_text_log (TextLog, Str)
  end;

  procedure LogWriteLn (Str: AL_STR);
  begin
    LogWrite (Str + #10)
  end;



(* Prints a formatted message on the log window. *)
  procedure LogPrint (Fmt: AL_STR; Values: array of const);
  begin
    al_append_native_text_log (TextLog, al_str_format (Fmt, Values))
  end;

  procedure LogPrintLn (Fmt: AL_STR; Values: array of const);
  begin
    LogPrint (Fmt + #10, Values)
  end;


{$IFDEF DCC }
  function GetTempFilename: AL_STR;
  var
    Count: Integer;
  begin
    Count := 1;
    repeat
      Result := UTF8Encode (Format ('tmpfile_%0.2d', [Count]));
      Inc (Count)
    until not FileExists (al_str_to_string (Result))
  end;
{$ENDIF }

end.
