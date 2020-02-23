UNIT al5nativedlg;
(***<Defines an API that allows to use native dialogs and menus in a
  cross-platform way.  This includes message dialogs, file choosers, main
  menu and more.

  @include(../docs/al5nativedlg.pds) *)
(* Copyright (c) 2012-2019 Guillermo Martínez J.

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

{$IF DEFINED (LINUX) OR DEFINED (UNIX)}
{$HINT Assumming GTK+.}
  CONST
    ALLEGRO_GTK_TOPLEVEL = ALLEGRO_GTK_TOPLEVEL_INTERNAL; {**<@exclude }
{$ENDIF}

  TYPE
    ALLEGRO_FILECHOOSERptr = AL_POINTER;
    ALLEGRO_TEXTLOGptr = AL_POINTER;
    ALLEGRO_MENUptr = AL_POINTER;
    ALLEGRO_MENUptrptr = ^ALLEGRO_MENUptr;
    ALLEGRO_MENU_INFO = RECORD
    (*** Label of the option. *)
      caption: AL_STRptr;
    (*** Identifier. *)
      id: AL_UINT16;
    (*** Option flags. *)
      flags: AL_INT;
    (*** Option icon/glyph. *)
      icon: ALLEGRO_BITMAPptr;
    END;

{ IMPLEMENTATION NOTE:
    ALLEGRO_START_OF_MENU is not defined due to differences between Pascal and
    C strings.  Original code is ("[]" are actually brackets):

#define ALLEGRO_START_OF_MENU(caption, id) [ caption "->", id, 0, NULL ]

    This assumes that 'caption' is a constant string, preprocessor translates
    'caption "->"' as a constant string, wich is not possible in Pascal (AFAIK
    there are preprocessors for Pascal too but they aren't very common or
    normalized; also it is possible to use C's preprocessors in Pascal but it
    will force to install it, make makefiles more complex and force Delphi and
    Lazarus users to add a step... Too much complex).

    If somebody finds or knows a solution that I don't know, please suggest it.
}
(*** Helper to build native menus. @seealso(ALLEGRO_MENU_INFO) *)
  FUNCTION ALLEGRO_ITEM_OF_MENU (
    CONST caption: AL_STRptr; id, flags: AL_INT; icon: ALLEGRO_BITMAPptr
  ): ALLEGRO_MENU_INFO; INLINE;
(*** Helper to build native menus. @seealso(ALLEGRO_MENU_INFO) *)
  FUNCTION ALLEGRO_MENU_SEPARATOR: ALLEGRO_MENU_INFO; INLINE;
(*** Helper to build native menus. @seealso(ALLEGRO_MENU_INFO) *)
  FUNCTION ALLEGRO_END_OF_MENU: ALLEGRO_MENU_INFO;

  FUNCTION al_init_native_dialog_addon: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_shutdown_native_dialog_addon;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_create_native_file_dialog
    (CONST initial_path, title, patterns: AL_STR; Mode: AL_INT)
  : ALLEGRO_FILECHOOSERptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_show_native_file_dialog
    (display: ALLEGRO_DISPLAYptr; dialog: ALLEGRO_FILECHOOSERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_get_native_file_dialog_count
    (CONST dialog: ALLEGRO_FILECHOOSERptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_get_native_file_dialog_path
    (CONST dialog: ALLEGRO_FILECHOOSERptr; index: AL_SIZE_T): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(*** Frees up all resources used by the file dialog. *)
  PROCEDURE al_destroy_native_file_dialog (dialog: ALLEGRO_FILECHOOSERptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_show_native_message_box (
    display: ALLEGRO_DISPLAYptr;
    CONST title, heading, str, buttons: AL_STR;
    flags: AL_INT
  ): AL_INT; INLINE;

  FUNCTION al_open_native_text_log
    (CONST title: AL_STR; flags: AL_INT): ALLEGRO_TEXTLOGptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_close_native_text_log (textlog: ALLEGRO_TEXTLOGptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_append_native_text_log
    (textlog: ALLEGRO_TEXTLOGptr; CONST str: AL_STR);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_get_native_text_log_event_source (textlog: ALLEGRO_TEXTLOGptr)
    : ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

{ creating/modifying menus }
  FUNCTION al_create_menu: ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_create_popup_menu: ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_build_menu (VAR info: ARRAY OF ALLEGRO_MENU_INFO): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_append_menu_item (
    parent: ALLEGRO_MENUptr; CONST title: AL_STRptr; id: AL_UINT16;
    flags: AL_INT; icon: ALLEGRO_BITMAPptr; submenu: ALLEGRO_MENUptr
  ): AL_INT;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
   FUNCTION al_insert_menu_item (
     parent: ALLEGRO_MENUptr; pos: AL_INT; CONST title: AL_STRptr;
     id: AL_UINT16; flags: AL_INT; icon: ALLEGRO_BITMAPptr;
     submenu: ALLEGRO_MENUptr
   ): AL_BOOL;
   CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_remove_menu_item (menu: ALLEGRO_MENUptr; pos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_clone_menu (menu: ALLEGRO_MENUptr): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_clone_menu_for_popup (menu: ALLEGRO_MENUptr): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_destroy_menu (menu: ALLEGRO_MENUptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

{ properties }
  FUNCTION al_get_menu_item_caption (menu: ALLEGRO_MENUptr; pos: AL_INT)
    : AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_set_menu_item_caption
    (menu: ALLEGRO_MENUptr; pos: AL_INT; CONST caption: AL_STR);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_get_menu_item_flags (menu: ALLEGRO_MENUptr; pos: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_set_menu_item_flags (menu: ALLEGRO_MENUptr; pos, flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_get_menu_item_icon (menu: ALLEGRO_MENUptr; pos: AL_INT)
    : ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_set_menu_item_icon
    (menu: ALLEGRO_MENUptr; pos: AL_INT; icon: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

{
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_NATIVE_DIALOG_SRC)
}
  FUNCTION al_toggle_menu_item_flags
    (menu: ALLEGRO_MENUptr; pos, flags: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

{ querying menus }
  FUNCTION al_find_menu (haystack: ALLEGRO_MENUptr; id: AL_UINT16)
    : ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_find_menu_item (
    haystack: ALLEGRO_MENUptr; id: AL_INT16;
    menu: ALLEGRO_MENUptrptr; index: AL_INTptr
  ): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

{ menu events }
  FUNCTION al_get_default_menu_event_source: ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_enable_menu_event_source (menu: ALLEGRO_MENUptr)
    : ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  PROCEDURE al_disable_menu_event_source (menu: ALLEGRO_MENUptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

{ displaying menus }
  FUNCTION al_get_display_menu (display: ALLEGRO_DISPLAYptr): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_set_display_menu (display: ALLEGRO_DISPLAYptr; menu: ALLEGRO_MENUptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_popup_menu (popup: ALLEGRO_MENUptr; display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
  FUNCTION al_remove_display_menu (display: ALLEGRO_DISPLAYptr): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  FUNCTION al_get_allegro_native_dialog_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

  CONST
    ALLEGRO_FILECHOOSER_FILE_MUST_EXIST = 1; {**<@exclude }
    ALLEGRO_FILECHOOSER_SAVE            = 2; {**<@exclude }
    ALLEGRO_FILECHOOSER_FOLDER          = 4; {**<@exclude }
    ALLEGRO_FILECHOOSER_PICTURES        = 8; {**<@exclude }
    ALLEGRO_FILECHOOSER_SHOW_HIDDEN     = 16; {**<@exclude }
    ALLEGRO_FILECHOOSER_MULTIPLE        = 32; {**<@exclude }

    ALLEGRO_MESSAGEBOX_WARN             = 1 SHL 0; {**<@exclude }
    ALLEGRO_MESSAGEBOX_ERROR            = 1 SHL 1; {**<@exclude }
    ALLEGRO_MESSAGEBOX_OK_CANCEL        = 1 SHL 2; {**<@exclude }
    ALLEGRO_MESSAGEBOX_YES_NO           = 1 SHL 3; {**<@exclude }
    ALLEGRO_MESSAGEBOX_QUESTION         = 1 SHL 4; {**<@exclude }

    ALLEGRO_TEXTLOG_NO_CLOSE            = 1 SHL 0; {**<@exclude }
    ALLEGRO_TEXTLOG_MONOSPACE           = 1 SHL 1; {**<@exclude }

    ALLEGRO_EVENT_NATIVE_DIALOG_CLOSE   = 600; {**<@exclude }
    ALLEGRO_EVENT_MENU_CLICK            = 601; {**<@exclude }

    ALLEGRO_MENU_ITEM_ENABLED           = 0; {**<@exclude }
    ALLEGRO_MENU_ITEM_CHECKBOX          = 1; {**<@exclude }
    ALLEGRO_MENU_ITEM_CHECKED           = 2; {**<@exclude }
    ALLEGRO_MENU_ITEM_DISABLED          = 4; {**<@exclude }



{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.  In
  delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here. }
{**@exclude }
  FUNCTION _al_show_native_message_box (
    display: ALLEGRO_DISPLAYptr; CONST title, heading, str, buttons: AL_STRptr;
    flags: AL_INT
  ): AL_INT; CDECL;
  EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME NAME 'al_show_native_message_box';

IMPLEMENTATION


  FUNCTION al_show_native_message_box (
    display: ALLEGRO_DISPLAYptr; CONST title, heading, str, buttons: AL_STR;
    flags: AL_INT
  ): AL_INT;
  VAR
    ButtonsPtr: AL_STRptr;
  BEGIN
    IF buttons <> '' THEN
      ButtonsPtr := AL_STRptr (buttons)
    ELSE
      ButtonsPtr := NIL;
    al_show_native_message_box := _al_show_native_message_box (
       display, AL_STRptr (Title), AL_STRptr (Heading), AL_STRptr (Str), ButtonsPtr, flags
    )
  END;



(* Menu options. *)
  FUNCTION ALLEGRO_ITEM_OF_MENU (
    CONST caption: AL_STRptr; id, flags: AL_INT; icon: ALLEGRO_BITMAPptr
  ): ALLEGRO_MENU_INFO;
  BEGIN
    ALLEGRO_ITEM_OF_MENU.caption := caption;
    ALLEGRO_ITEM_OF_MENU.id := id;
    ALLEGRO_ITEM_OF_MENU.flags := flags;
    ALLEGRO_ITEM_OF_MENU.icon := icon
  END;



  FUNCTION ALLEGRO_MENU_SEPARATOR: ALLEGRO_MENU_INFO;
  BEGIN
  { There's an issue here.  Allegro sets it to (-1) but id is unsigned
    (uint16_t) wich makes FPC to raise a warning.  That's why I set it to
    maximum value for 16bit integers. }
    ALLEGRO_MENU_SEPARATOR := ALLEGRO_ITEM_OF_MENU (NIL, $FFFF, 0, NIL)
  END;



  FUNCTION ALLEGRO_END_OF_MENU: ALLEGRO_MENU_INFO;
  BEGIN
    ALLEGRO_END_OF_MENU := ALLEGRO_ITEM_OF_MENU (NIL, 0, 0, NIL)
  END;

END.
