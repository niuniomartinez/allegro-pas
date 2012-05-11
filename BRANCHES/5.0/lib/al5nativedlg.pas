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

{$include allegro.cfg}

INTERFACE

  USES
    Allegro5;

{$include allegro.inc}

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

  FUNCTION al_create_native_file_dialog (CONST initial_path, title, patterns: STRING; Mode: LONGINT): ALLEGRO_FILECHOOSERptr; INLINE;
  FUNCTION al_show_native_file_dialog (display: ALLEGRO_DISPLAYptr; dialog: ALLEGRO_FILECHOOSERptr): BOOLEAN; CDECL;
  FUNCTION al_get_native_file_dialog_count (CONST dialog: ALLEGRO_FILECHOOSERptr): LONGINT; CDECL;
  FUNCTION al_get_native_file_dialog_path (CONST dialog: ALLEGRO_FILECHOOSERptr; index: LONGINT): STRING; INLINE;
  PROCEDURE al_destroy_native_file_dialog (dialog: ALLEGRO_FILECHOOSERptr); CDECL;

  FUNCTION al_show_native_message_box (display: ALLEGRO_DISPLAYptr; title, heading, str, buttons: STRING; flags: LONGINT): LONGINT; INLINE;

  FUNCTION al_open_native_text_log (CONST title: STRING; flags: LONGINT): ALLEGRO_TEXTLOGptr; INLINE;
  PROCEDURE al_close_native_text_log (textlog: ALLEGRO_TEXTLOGptr); CDECL;
  PROCEDURE al_append_native_text_log (textlog: ALLEGRO_TEXTLOGptr; CONST str: STRING); INLINE;
  FUNCTION al_get_native_text_log_event_source (textlog: ALLEGRO_TEXTLOGptr): ALLEGRO_EVENT_SOURCEptr; CDECL;

  FUNCTION al_get_allegro_native_dialog_version: LONGWORD; CDECL;

IMPLEMENTATION

  USES
    al5data;

  FUNCTION _al_create_native_file_dialog_ (CONST initial_path, title, patterns: PCHAR; Mode: LONGINT): ALLEGRO_FILECHOOSERptr; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME NAME 'al_create_native_file_dialog';

  FUNCTION al_create_native_file_dialog (CONST initial_path, title, patterns: STRING; Mode: LONGINT): ALLEGRO_FILECHOOSERptr;
  VAR
    InitialPathZ, TitleZ, PatternsZ: PCHAR;
  BEGIN
    InitialPathZ := _al_create_null_str_ (initial_path);
    TitleZ := _al_create_null_str_ (title);
    PatternsZ := _al_create_null_str_ (patterns);

    al_create_native_file_dialog := _al_create_native_file_dialog_ (InitialPathZ, TitleZ, PatternsZ, Mode);

    _al_dispose_str_ (InitialPathZ);
    _al_dispose_str_ (TitleZ);
    _al_dispose_str_ (PatternsZ);
  END;




  FUNCTION al_show_native_file_dialog (display: ALLEGRO_DISPLAYptr; dialog: ALLEGRO_FILECHOOSERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_get_native_file_dialog_count (CONST dialog: ALLEGRO_FILECHOOSERptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION _al_get_native_file_dialog_path_ (CONST dialog: ALLEGRO_FILECHOOSERptr; index: LONGINT): PCHAR; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME NAME 'al_get_native_file_dialog_path';

  FUNCTION al_get_native_file_dialog_path (CONST dialog: ALLEGRO_FILECHOOSERptr; index: LONGINT): STRING;
  BEGIN
    al_get_native_file_dialog_path := _al_pchar_to_str_ (
      _al_get_native_file_dialog_path_ (dialog, index)
    );
  END;



  PROCEDURE al_destroy_native_file_dialog (dialog: ALLEGRO_FILECHOOSERptr); CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;



  FUNCTION _al_show_native_message_box (display: ALLEGRO_DISPLAYptr; title, heading, str, buttons: PCHAR; flags: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME NAME 'al_show_native_message_box';

  FUNCTION al_show_native_message_box (display: ALLEGRO_DISPLAYptr; title, heading, str, buttons: STRING; flags: LONGINT): LONGINT;
  VAR
    TitleZ, HeadingZ, StrZ, ButtonsZ: PCHAR;
  BEGIN
    TitleZ := _al_create_null_str_ (title);
    HeadingZ := _al_create_null_str_ (heading);
    StrZ := _al_create_null_str_ (str);
    IF buttons <> '' THEN
      ButtonsZ := _al_create_null_str_ (buttons)
    ELSE
      ButtonsZ := NIL;
    al_show_native_message_box := _al_show_native_message_box (
       display, TitleZ, HeadingZ, StrZ, ButtonsZ, flags
    );
    _al_dispose_str_ (TitleZ);
    _al_dispose_str_ (HeadingZ);
    _al_dispose_str_ (StrZ);
    IF ButtonsZ <> NIL THEN
      _al_dispose_str_ (ButtonsZ);
  END;



  FUNCTION _al_open_native_text_log_ (CONST title: PCHAR; flags: LONGINT): ALLEGRO_TEXTLOGptr; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME NAME 'al_open_native_text_log';

  FUNCTION al_open_native_text_log (CONST title: STRING; flags: LONGINT): ALLEGRO_TEXTLOGptr;
  VAR
    TitleZ: PCHAR;
  BEGIN
    TitleZ := _al_create_null_str_ (title);
    al_open_native_text_log := _al_open_native_text_log_ (TitleZ, flags);
    _al_dispose_str_ (TitleZ);
  END;



  PROCEDURE al_close_native_text_log (textlog: ALLEGRO_TEXTLOGptr); CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  PROCEDURE _al_append_native_text_log_ (textlog: ALLEGRO_TEXTLOGptr; CONST str: PCHAR); CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME NAME 'al_append_native_text_log';

  PROCEDURE al_append_native_text_log (textlog: ALLEGRO_TEXTLOGptr; CONST str: STRING);
  VAR
    StrZ: PCHAR;
  BEGIN
    StrZ := _al_create_null_str_ (str);
    _al_append_native_text_log_ (textlog, StrZ);
    _al_dispose_str_ (StrZ);
  END;



  FUNCTION al_get_native_text_log_event_source (textlog: ALLEGRO_TEXTLOGptr): ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_get_allegro_native_dialog_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

END.
