UNIT al5nativedlg;
(*         ______   ___    ___
 *        /\  _  \ /\_ \  /\_ \
 *        \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___
 *         \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\
 *          \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \
 *           \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/
 *            \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/
 *                                           /\____/
 *                                           \_/__/
 *
 *      See readme.txt for copyright information.
 *)

INTERFACE

  USES
    Allegro5;

{$include allegro.cfg}

  TYPE
    ALLEGRO_FILECHOOSERptr = POINTER;
     ALLEGRO_TEXTLOGptr = POINTER;

  CONST
    ALLEGRO_FILECHOOSER_FILE_MUST_EXIST = 1;
    ALLEGRO_FILECHOOSER_SAVE            = 2;
    ALLEGRO_FILECHOOSER_FOLDER          = 4;
    ALLEGRO_FILECHOOSER_PICTURES        = 8;
    ALLEGRO_FILECHOOSER_SHOW_HIDDEN     = 16;
    ALLEGRO_FILECHOOSER_MULTIPLE        = 32;

    ALLEGRO_MESSAGEBOX_WARN             = 1 SHL 0;
    ALLEGRO_MESSAGEBOX_ERROR            = 1 SHL 1;
    ALLEGRO_MESSAGEBOX_OK_CANCEL        = 1 SHL 2;
    ALLEGRO_MESSAGEBOX_YES_NO           = 1 SHL 3;
    ALLEGRO_MESSAGEBOX_QUESTION         = 1 SHL 4;

    ALLEGRO_TEXTLOG_NO_CLOSE            = 1 SHL 0;
    ALLEGRO_TEXTLOG_MONOSPACE           = 1 SHL 1;

    ALLEGRO_EVENT_NATIVE_DIALOG_CLOSE   = 600;

  FUNCTION al_create_native_file_dialog (CONST initial_path, title, patterns: STRING; Mode: LONGINT): ALLEGRO_FILECHOOSERptr; CDECL;
  FUNCTION al_show_native_file_dialog (display: ALLEGRO_DISPLAYptr; dialog: ALLEGRO_FILECHOOSERptr): BOOLEAN; CDECL;
  FUNCTION al_get_native_file_dialog_count (CONST dialog: ALLEGRO_FILECHOOSERptr): LONGINT; CDECL;
  FUNCTION al_get_native_file_dialog_path (CONST dialog: ALLEGRO_FILECHOOSERptr; index: LONGINT): PCHAR; CDECL;
  PROCEDURE al_destroy_native_file_dialog (dialog: ALLEGRO_FILECHOOSERptr); CDECL;

  FUNCTION al_show_native_message_box (display: ALLEGRO_DISPLAYptr; title, heading, str, buttons: STRING; flags: LONGINT): LONGINT;

  FUNCTION al_open_native_text_log (CONST title: STRING; flags: LONGINT): ALLEGRO_TEXTLOGptr; CDECL;
  PROCEDURE al_close_native_text_log (textlog: ALLEGRO_TEXTLOGptr); CDECL;
  PROCEDURE al_append_native_text_log (textlog: ALLEGRO_TEXTLOGptr; CONST str: STRING); CDECL;
  FUNCTION al_get_native_text_log_event_source (textlog: ALLEGRO_TEXTLOGptr): ALLEGRO_EVENT_SOURCEptr; CDECL;

  FUNCTION al_get_allegro_native_dialog_version: LONGWORD; CDECL;

IMPLEMENTATION

  FUNCTION al_create_native_file_dialog (CONST initial_path, title, patterns: STRING; Mode: LONGINT): ALLEGRO_FILECHOOSERptr; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_show_native_file_dialog (display: ALLEGRO_DISPLAYptr; dialog: ALLEGRO_FILECHOOSERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_get_native_file_dialog_count (CONST dialog: ALLEGRO_FILECHOOSERptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_get_native_file_dialog_path (CONST dialog: ALLEGRO_FILECHOOSERptr; index: LONGINT): PCHAR; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  PROCEDURE al_destroy_native_file_dialog (dialog: ALLEGRO_FILECHOOSERptr); CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

(* Parameter "buttons" must be a pointer because the NIL value is used to
  set "no buttons". *)
  FUNCTION _al_show_native_message_box (display: ALLEGRO_DISPLAYptr; title, heading, str, buttons: PCHAR; flags: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME NAME 'al_show_native_message_box';

  FUNCTION al_show_native_message_box (display: ALLEGRO_DISPLAYptr; title, heading, str, buttons: STRING; flags: LONGINT): LONGINT;
  VAR
    TitlePtr, HeadingPtr, StrPtr, ButtonsPtr: PCHAR;
  BEGIN
     TitlePtr := PCHAR (title);
     HeadingPtr := PCHAR (heading);
     StrPtr := PCHAR (str);
    IF buttons <> '' THEN
      ButtonsPtr := PCHAR (buttons)
    ELSE
      ButtonsPtr := NIL;
    al_show_native_message_box := _al_show_native_message_box (
       display, TitlePtr, HeadingPtr, StrPtr, ButtonsPtr, flags
    );
  END;

  FUNCTION al_open_native_text_log (CONST title: STRING; flags: LONGINT): ALLEGRO_TEXTLOGptr; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  PROCEDURE al_close_native_text_log (textlog: ALLEGRO_TEXTLOGptr); CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  PROCEDURE al_append_native_text_log (textlog: ALLEGRO_TEXTLOGptr; CONST str: STRING); CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_get_native_text_log_event_source (textlog: ALLEGRO_TEXTLOGptr): ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_get_allegro_native_dialog_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

END.
