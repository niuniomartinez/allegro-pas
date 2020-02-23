UNIT Common;
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

INTERFACE

  USES
    allegro5, al5base, al5nativedlg;

  VAR
    TextLog: ALLEGRO_TEXTLOGptr;

(* Initializes platform specific stuff. *)
  PROCEDURE InitPlatformSpecific;
(* Exits program with error. *)
  PROCEDURE AbortExample (CONST Message: AL_STR);
(* Opens a log window. *)
  PROCEDURE OpenLog;
  PROCEDURE OpenLogMonospace;
(* Closes the log window. *)
  PROCEDURE CloseLog (WaitForUser: BOOLEAN);
(* Prints a message on the log window. *)
  PROCEDURE LogWrite (Str: AL_STR);
  PROCEDURE LogWriteLn (Str: AL_STR);
(* Prints a formatted message on the log window. *)
  PROCEDURE LogPrint (Fmt: AL_STR; Values: ARRAY OF CONST);
  PROCEDURE LogPrintLn (Fmt: AL_STR; Values: ARRAY OF CONST);

{$IFDEF DCC }
(* Delphi doesn't has GetTempFilename (or I can't find the declaration, and
   Internet failed to tell me) so let's define it. *)
  FUNCTION GetTempFilename: AL_STR;
{$ENDIF }

IMPLEMENTATION

  USES
    al5strings,
    sysutils;

(* Platform specific stuff. *)
  PROCEDURE InitPlatformSpecific;
  BEGIN
  { TODO: Android stuff, if android. }
  END;



(* Exits program with error. *)
  PROCEDURE AbortExample (CONST Message: AL_STR);
  VAR
    Display: ALLEGRO_DISPLAYptr;
  BEGIN
    IF al_init_native_dialog_addon THEN
    BEGIN
      IF al_is_system_installed THEN
        Display := al_get_current_display
      ELSE
        Display := NIL;
      al_show_native_message_box
        (Display, 'Error', 'Cannot run example', Message, '', 0)
    END
    ELSE
      WriteLn (ErrOutput, Message);
    HALT (1)
  END;



(* Opens a log window. *)
  PROCEDURE OpenLog;
  BEGIN
    IF al_init_native_dialog_addon THEN
      TextLog := al_open_native_text_log ('Log', 0)
  END;



  PROCEDURE OpenLogMonospace;
  BEGIN
    IF al_init_native_dialog_addon THEN
      TextLog := al_open_native_text_log ('Log', ALLEGRO_TEXTLOG_MONOSPACE)
   END;



(* Closes the log window. *)
  PROCEDURE CloseLog (WaitForUser: BOOLEAN);
  VAR
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
  BEGIN
    IF (TextLog <> NIL) AND WaitForUser THEN
    BEGIN
      Queue := al_create_event_queue;
      al_register_event_source (
        Queue,
        al_get_native_text_log_event_source (TextLog)
      );
      al_wait_for_event (Queue, @Event);
      al_destroy_event_queue (Queue)
    END;
    al_close_native_text_log (TextLog);
    TextLog := NIL
  END;


(* Prints a message on the log window. *)
  PROCEDURE LogWrite (Str: AL_STR);
  BEGIN
    al_append_native_text_log (TextLog, Str)
  END;

  PROCEDURE LogWriteLn (Str: AL_STR);
  BEGIN
    LogWrite (Str + #10)
  END;



(* Prints a formatted message on the log window. *)
  PROCEDURE LogPrint (Fmt: AL_STR; Values: ARRAY OF CONST);
  BEGIN
    al_append_native_text_log (TextLog, al_str_format (Fmt, Values))
  END;

  PROCEDURE LogPrintLn (Fmt: AL_STR; Values: ARRAY OF CONST);
  BEGIN
    LogPrint (Fmt + #10, Values)
  END;


{$IFDEF DCC }
  FUNCTION GetTempFilename: AL_STR;
  VAR
    Count: INTEGER;
  BEGIN
    Count := 1;
    REPEAT
      RESULT := UTF8Encode (Format ('tmpfile_%0.2d', [Count]));
      INC (Count)
    UNTIL NOT FileExists (al_str_to_string (RESULT))
  END;
{$ENDIF }

END.
