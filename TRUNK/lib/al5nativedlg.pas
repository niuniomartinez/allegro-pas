UNIT al5nativedlg;
(*<Defines an API that allows to use native dialogs and menus in a
   cross-platform way.  This includes message dialogs, file choosers, main
   menu and more.

   You should not use these dialogs if your app is running in full-screen
   modes.

   Note that this isn't integrated with VCL, CLX, LCL nor fpGUI packages.
   Integration isn't even planned.
 *)
(* Copyright (c) 2012-2016 Guillermo Martínez J.

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

{$include allegro5.cfg}

INTERFACE

  USES
    allegro5, al5base;

  CONST
  (* Builds library name. *)
    { @exclude }
    ALLEGRO_NATIVE_DLG_LIB_NAME = _A5_LIB_PREFIX_+'allegro_dialog'+_DBG_+_A5_LIB_EXT_;

  TYPE
    ALLEGRO_FILECHOOSERptr = AL_POINTER;
    ALLEGRO_TEXTLOGptr = AL_POINTER;

  FUNCTION al_init_native_dialog_addon: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_shutdown_native_dialog_addon;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;



(******************************************************************************
  These const lines were in the end of the  C header.
 **********)

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
    ALLEGRO_EVENT_MENU_CLICK            = 601;

    ALLEGRO_MENU_ITEM_ENABLED           = 0;
    ALLEGRO_MENU_ITEM_CHECKBOX          = 1;
    ALLEGRO_MENU_ITEM_CHECKED           = 2;
    ALLEGRO_MENU_ITEM_DISABLED          = 4;



  FUNCTION al_create_native_file_dialog (CONST initial_path, title, patterns: AL_STR; Mode: AL_INT): ALLEGRO_FILECHOOSERptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_show_native_file_dialog (display: ALLEGRO_DISPLAYptr; dialog: ALLEGRO_FILECHOOSERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_get_native_file_dialog_count (CONST dialog: ALLEGRO_FILECHOOSERptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_get_native_file_dialog_path (CONST dialog: ALLEGRO_FILECHOOSERptr; index: AL_SIZE_T): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_destroy_native_file_dialog (dialog: ALLEGRO_FILECHOOSERptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_show_native_message_box (display: ALLEGRO_DISPLAYptr; CONST title, heading, str, buttons: STRING; flags: AL_INT): AL_INT;
    INLINE;

  FUNCTION al_open_native_text_log (CONST title: AL_STR; flags: AL_INT): ALLEGRO_TEXTLOGptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_close_native_text_log (textlog: ALLEGRO_TEXTLOGptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_append_native_text_log (textlog: ALLEGRO_TEXTLOGptr; CONST str: AL_STR);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_get_native_text_log_event_source (textlog: ALLEGRO_TEXTLOGptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

{ TODO: Creating/modifying menu. }

  FUNCTION al_get_allegro_native_dialog_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

IMPLEMENTATION

  FUNCTION _al_show_native_message_box (display: ALLEGRO_DISPLAYptr; CONST title, heading, str, buttons: AL_STRptr; flags: AL_INT): AL_INT; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME NAME 'al_show_native_message_box';

  FUNCTION al_show_native_message_box (display: ALLEGRO_DISPLAYptr; CONST title, heading, str, buttons: STRING; flags: AL_INT): AL_INT;
  VAR
    ButtonsPtr: AL_STRptr;
  BEGIN
    IF buttons <> '' THEN
      ButtonsPtr := AL_STRptr (buttons)
    ELSE
      ButtonsPtr := NIL;
    al_show_native_message_box := _al_show_native_message_box (
       display, AL_STRptr (Title), AL_STRptr (Heading), AL_STRptr (Str), ButtonsPtr, flags
    );
  END;

END.
