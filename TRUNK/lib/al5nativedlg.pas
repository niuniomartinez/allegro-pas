UNIT al5nativedlg;
(*<Defines an API that allows to use native dialogs and menus in a
   cross-platform way.  This includes message dialogs, file choosers, main
   menu and more.

   You should not use these dialogs if your app is running in full-screen
   modes.

   Note that this isn't integrated with VCL, CLX, LCL nor fpGUI packages.
   Integration isn't even planned.
 *)
(* Copyright (c) 2012-2018 Guillermo Martínez J.

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
    { @exclude }
    ALLEGRO_FILECHOOSER_FILE_MUST_EXIST = 1;
    { @exclude }
    ALLEGRO_FILECHOOSER_SAVE            = 2;
    { @exclude }
    ALLEGRO_FILECHOOSER_FOLDER          = 4;
    { @exclude }
    ALLEGRO_FILECHOOSER_PICTURES        = 8;
    { @exclude }
    ALLEGRO_FILECHOOSER_SHOW_HIDDEN     = 16;
    { @exclude }
    ALLEGRO_FILECHOOSER_MULTIPLE        = 32;

    { @exclude }
    ALLEGRO_MESSAGEBOX_WARN             = 1 SHL 0;
    { @exclude }
    ALLEGRO_MESSAGEBOX_ERROR            = 1 SHL 1;
    { @exclude }
    ALLEGRO_MESSAGEBOX_OK_CANCEL        = 1 SHL 2;
    { @exclude }
    ALLEGRO_MESSAGEBOX_YES_NO           = 1 SHL 3;
    { @exclude }
    ALLEGRO_MESSAGEBOX_QUESTION         = 1 SHL 4;

    { @exclude }
    ALLEGRO_TEXTLOG_NO_CLOSE            = 1 SHL 0;
    { @exclude }
    ALLEGRO_TEXTLOG_MONOSPACE           = 1 SHL 1;

    { @exclude }
    ALLEGRO_EVENT_NATIVE_DIALOG_CLOSE   = 600;
    { @exclude }
    ALLEGRO_EVENT_MENU_CLICK            = 601;

    { @exclude }
    ALLEGRO_MENU_ITEM_ENABLED           = 0;
    { @exclude }
    ALLEGRO_MENU_ITEM_CHECKBOX          = 1;
    { @exclude }
    ALLEGRO_MENU_ITEM_CHECKED           = 2;
    { @exclude }
    ALLEGRO_MENU_ITEM_DISABLED          = 4;

{$IF DEFINED (LINUX) OR DEFINED (UNIX)}
{$HINT Assume GTK+.}
    { @exclude }
    ALLEGRO_GTK_TOPLEVEL = ALLEGRO_GTK_TOPLEVEL_INTERNAL;
{$ENDIF}


  TYPE
  (* Opaque handle to a native file dialog.
     @seealso(al_create_native_file_dialog) *)
    ALLEGRO_FILECHOOSERptr = AL_POINTER;
  (* Opaque handle to a text log window.
     @seealso(al_open_native_text_log) *)
    ALLEGRO_TEXTLOGptr = AL_POINTER;
  (* An opaque data type that represents a menu that contains menu items. Each
     of the menu items may optionally include a sub-menu. *)
    ALLEGRO_MENUptr = AL_POINTER;
  (* Pointer to @link(ALLEGRO_MENUptr). *)
    ALLEGRO_MENUptr_RESULT = ^ALLEGRO_MENUptr;
  (* A structure that defines how to create a complete menu system.

     An example:
@longcode(#
VAR
  MenuInfo: ARRAY [0..12] OF ALLEGRO_MENU_INFO;
  Menu: ALLEGRO_MENUptr;
BEGIN
  MenuInfo[ 0] := ALLEGRO_ITEM_OF_MENU ('&File->', 1);
  MenuInfo[ 1] := ALLEGRO_ITEM_OF_MENU  ('&Open', 2, 0, NIL);
  MenuInfo[ 2] := ALLEGRO_ITEM_OF_MENU ('Open &Recent...->', 3);
  MenuInfo[ 3] := ALLEGRO_ITEM_OF_MENU  ('Recent 1', 4, 0, NIL);
  MenuInfo[ 4] := ALLEGRO_ITEM_OF_MENU  ('Recent 2', 5, 0, NIL);
  MenuInfo[ 5] := ALLEGRO_END_OF_MENU;
  MenuInfo[ 6] := ALLEGRO_MENU_SEPARATOR;
  MenuInfo[ 7] := ALLEGRO_ITEM_OF_MENU ('E&xit', 6, 0, NIL);
  MenuInfo[ 8] := ALLEGRO_END_OF_MENU;
  MenuInfo[ 9] := ALLEGRO_ITEM_OF_MENU ('&Help->', 7);
  MenuInfo[10] := ALLEGRO_ITEM_OF_MENU ('&About', 8, 0, NIL);
  MenuInfo[11] := ALLEGRO_END_OF_MENU;
  MenuInfo[12] := ALLEGRO_END_OF_MENU;

  Menu := al_build_menu (MenuInfo)
END;
#)
  If you prefer, you can build the menu without the structure by using
  @link(al_create_menu) and @link(al_insert_menu_item).
  @seealso(ALLEGRO_ITEM_OF_MENU)
  @seealso(ALLEGRO_MENU_SEPARATOR) @seealso(ALLEGRO_END_OF_MENU)
  @seealso(al_build_menu) *)
    ALLEGRO_MENU_INFO = RECORD
    (* Label of the option. *)
      caption: AL_STRptr;
    (* Identifier. *)
      id: AL_UINT16;
    (* Option flags. *)
      flags: AL_INT;
    (* Option icon/glyph. *)
      icon: ALLEGRO_BITMAPptr;
    END;

(* Initialise the native dialog addon.
   @return(@true on success, @false on error.)
   @seealso(al_shutdown_native_dialog_addon) *)
  FUNCTION al_init_native_dialog_addon: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Shut down the native dialog addon.
   @seealso(al_init_native_dialog_addon) *)
  PROCEDURE al_shutdown_native_dialog_addon;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

(* Creates a new native file dialog. You should only have one such dialog
   opened at a time.
   @param(initial_path The initial search path and filename.  To start with a
	  blank file name the string should end with a directory separator
	  @(this should be the common case@).)
   @param(title Title of the dialog.)
   @param(patterns A list of semi-colon separated patterns to match. This
	  should not contain any whitespace characters. If a pattern contains 
	  the '/' character, then it is treated as a MIME type @(e.g.
          'image/png'@). Not all platforms support file patterns. If the native
	  dialog does not provide support, this parameter is ignored.)
   @param(mode 0, or a combination @(@code(OR)@) of the following flags:
   @unorderedlist(
     @item(@code(ALLEGRO_FILECHOOSER_FILE_MUST_EXIST) If supported by the
       native dialog, it will not allow entering new names, but just allow
       existing files to be selected. Else it is ignored.)
     @item(@code(ALLEGRO_FILECHOOSER_SAVE) If the native dialog system has a
       different dialog for saving @(for example one which allows creating new
       directories@), it is used. Else it is ignored.)
     @item(@code(ALLEGRO_FILECHOOSER_FOLDER) If there is support for a separate
       dialog to select a folder instead of a file, it will be used.)
     @item(@code(ALLEGRO_FILECHOOSER_PICTURES) If a different dialog is
       available for selecting pictures, it is used. Else it is ignored.)
     @item(@code(ALLEGRO_FILECHOOSER_SHOW_HIDDEN) If the platform supports it,
       also hidden files will be shown.)
     @item(@code(ALLEGRO_FILECHOOSER_MULTIPLE) If supported, allow selecting
      multiple files.)
    ))
    @Returns(A handle to the dialog which you can pass to 
	    @link(al_show_native_file_dialog) to display it, and from which you
	    then can query the results using
	    @link(al_get_native_file_dialog_count) and
	    @link(al_get_native_file_dialog_path). When you are done, call 
	    @link(al_destroy_native_file_dialog) on it.

            If a dialog window could not be created then this function returns
	    @nil.) *)
  FUNCTION al_create_native_file_dialog (
    CONST initial_path, title, patterns: AL_STR; Mode: AL_INT
  ): ALLEGRO_FILECHOOSERptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Shows the dialog window. The display may be @nil, otherwise the given
   display is treated as the parent if possible.

   This function blocks the calling thread until it returns, so you may want to
   spawn a thread and call it from inside that thread.
   @return(@true on success, @false on failure.)
   @seealso(al_create_native_file_dialog) *)
  FUNCTION al_show_native_file_dialog (display: ALLEGRO_DISPLAYptr; dialog: ALLEGRO_FILECHOOSERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Returns the number of files selected, or @code(0) if the dialog was
   cancelled.
   @seealso(al_create_native_file_dialog) @seealso(al_show_native_file_dialog)
 *)
  FUNCTION al_get_native_file_dialog_count (CONST dialog: ALLEGRO_FILECHOOSERptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Returns one of the selected paths with index @code(i). The index should
   range from @code(0) to the return value of
   @code(@link(al_get_native_file_dialog_count) - 1). *)
  FUNCTION al_get_native_file_dialog_path
    (CONST dialog: ALLEGRO_FILECHOOSERptr; index: AL_SIZE_T): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Frees up all resources used by the file dialog. *)
  PROCEDURE al_destroy_native_file_dialog (dialog: ALLEGRO_FILECHOOSERptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

 (* Shows a native GUI message box.  This can be used for example to display an
    error message if creation of an initial display fails. The display may be
    @nil, otherwise the given display is treated as the parent if possible.

    The message box will have a single "OK" button and use the style
    informative dialog boxes usually have on the native system. If the buttons
    parameter is not an empty string, you can instead specify the button text
    in a string, with buttons separated by a vertical bar (|).

    The flags available are:
    @unorderedlist(
      @item(@code(ALLEGRO_MESSAGEBOX_WARN) The message is a warning.  This may
        cause a different icon @(or other effects@).)
      @item(@code(ALLEGRO_MESSAGEBOX_ERROR) The message is an error.)
      @item(@code(ALLEGRO_MESSAGEBOX_QUESTION) The message is a question.)
      @item(@code(ALLEGRO_MESSAGEBOX_OK_CANCEL) Display a cancel button
        alongside the "OK" button. Ignored if buttons is not an empty string.)
      @item(@code(ALLEGRO_MESSAGEBOX_YES_NO) Display Yes/No buttons instead of
        the "OK" button. Ignored if buttons is not an empty string.)
    )
    @code(al_show_native_message_box) may be called without Allegro being
    installed. This is useful to report an error during initialisation of
    Allegro itself.

    @bold(Example:)
@longcode(#
button := al_show_native_message_box (
  display,
  'Warning',
  'Are you sure?',
  'If you click yes then you are confirming that "Yes" '+
  'is your response to the query which you have '+
  'generated by the action you took to open this '+
  'message box.',
  '',
  ALLEGRO_MESSAGEBOX_YES_NO
);
#)
    @return(0 if the dialog window was closed without activating a button.

    1 if the OK or Yes button was pressed.

    2 if the Cancel or No button was pressed

    If buttons is not an empty string, the number of the pressed button is
    returned, starting with 1.

    If a message box could not be created then this returns 0, as if the window
    was dismissed without activating a button.) *)
  FUNCTION al_show_native_message_box (
    display: ALLEGRO_DISPLAYptr;
    CONST title, heading, str, buttons: AL_STR;
    flags: AL_INT
  ): AL_INT; INLINE;

(* Opens a window to which you can append log messages with
   @link(al_append_native_text_log).  This can be useful for debugging if you
   don't want to depend on a console being available.

   Use @link(al_close_native_text_log) to close the window again.
   @param(title Title of the dialog.)
   @param(flags One of:@unorderedlist(
    @item(@bold(ALLEGRO_TEXTLOG_NO_CLOSE) Prevent the window from having a
      close button. Otherwise, if the close button is pressed, an event is
      generated; see @link(al_get_native_text_log_event_source).)
    @item(@bold(ALLEGRO_TEXTLOG_MONOSPACE) Use a monospace font to display the
      text.)
   ))
   @returns(@nil if there was an error opening the window, or if text log 
	    windows are not implemented on the platform.)
   @seealso(al_append_native_text_log) @seealso(al_close_native_text_log) *)
  FUNCTION al_open_native_text_log
    (CONST title: AL_STR; flags: AL_INT): ALLEGRO_TEXTLOGptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Closes a message log window opened with @code(al_open_native_text_log)
   earlier.

   Does nothing if passed @nil.
   @seealso(al_open_native_text_log) *)
  PROCEDURE al_close_native_text_log (textlog: ALLEGRO_TEXTLOGptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Appends a line of text to the message log window and scrolls to the bottom
   (if the line would not be visible otherwise). A line is continued until you
   add a newline character.

   If the window is @nil then this function will fall back to calling
   @code(Write). This makes it convenient to support logging to a window or a
   terminal. *)
  PROCEDURE al_append_native_text_log
    (textlog: ALLEGRO_TEXTLOGptr; CONST str: AL_STR);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Get an event source for a text log window. The possible events are:
   @unorderedlist(
     @item(@bold(ALLEGRO_EVENT_NATIVE_DIALOG_CLOSE) The window was requested to
       be closed, either by pressing the close button or pressing Escape on the
       keyboard. The @code(user.data1) field will hold a pointer to the
       @code(ALLEGRO_TEXTLOG) which generated the event. The @code(user.data2)
       field will be @code(1) if the event was generated as a result of a key
       press; otherwise it will be zero.)
   ) *)
  FUNCTION al_get_native_text_log_event_source (textlog: ALLEGRO_TEXTLOGptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

(* Returns the (compiled) version of the addon, in the same format as
   @link(al_get_allegro_version). *)
  FUNCTION al_get_allegro_native_dialog_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;



(* Helper to build native menus. *)
  FUNCTION ALLEGRO_ITEM_OF_MENU (
    CONST caption: AL_STRptr; id, flags: AL_INT; icon: ALLEGRO_BITMAPptr
  ): ALLEGRO_MENU_INFO; INLINE;
(* Helper to build native menus. *)
  FUNCTION ALLEGRO_MENU_SEPARATOR: ALLEGRO_MENU_INFO; INLINE;
(* Helper to build native menus. *)
  FUNCTION ALLEGRO_END_OF_MENU: ALLEGRO_MENU_INFO;

(* Creates a menu container that can hold menu items.
   @return(Pointer to the menu struct or @nil on failure.)
   @seealso(al_create_popup_menu) @seealso(al_build_menu) *)
  FUNCTION al_create_menu: ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Creates a menu container for popup menus. Only the root (outermost) menu
   should be created with this function. Sub menus of popups should be created
   with @link(al_create_menu).
   @return(Pointer to the menu struct or @nil on failure.)
   @seealso(al_create_menu) @seealso(al_build_menu) *)
  FUNCTION al_create_popup_menu: ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Builds a menu based on the specifications of a sequence of
   @code(ALLEGRO_MENU_INFO) elements.
   @returns(A pointer to the root @code(ALLEGRO_MENU), or @nil on failure. To
    gain access to the other menus and items, you will need to search for them
    using @link(al_find_menu_item).)
    @seealso(ALLEGRO_MENU_INFO) @seealso(al_create_menu)
    @seealso(al_create_popup_menu) *)
  FUNCTION al_build_menu (VAR info: ARRAY OF ALLEGRO_MENU_INFO): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Appends a menu item to the end of the menu. See @link(al_insert_menu_item)
   for more information.
   @seealso(al_insert_menu_item) @seealso(al_remove_menu_item) *)
  FUNCTION al_append_menu_item (
    parent: ALLEGRO_MENUptr; CONST title: AL_STRptr; id: AL_UINT16;
    flags: AL_INT; icon: ALLEGRO_BITMAPptr; submenu: ALLEGRO_MENUptr
  ): AL_INT;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Inserts a menu item at the spot specified.  See the introductory text for a
   detailed explanation of how the @code(pos) parameter is interpreted.

   The parent menu can be a popup menu or a regular menu.  To underline one
   character in the title, prefix it with an ampersand.

   The flags can be any combination of: @unorderedlist(
    @item(@code(@bold(ALLEGRO_MENU_ITEM_DISABLED)) The item is @italic("grayed
     out") and cannot be selected.)
    @item(@code(@bold(ALLEGRO_MENU_ITEM_CHECKBOX)) The item is a check box.
     This flag can only be set at the time the menu is created.  If a check box
     is clicked, it will automatically be toggled.)
    @item(@code(@bold(ALLEGRO_MENU_ITEM_CHECKED)) The item is checked.  If set,
     @code(ALLEGRO_MENU_ITEM_CHECKBOX) will automatically be set as well.)
    )
    The @code(icon) is not yet supported.

    The submenu parameter indicates that this item contains a child menu. The
    child menu must have previously been created with @code(al_create_menu),
    and not be associated with any other menu.
    @return(@true on success.)
    @seealso(al_append_menu_item) @seealso(al_remove_menu_item) *)
   FUNCTION al_insert_menu_item (
     parent: ALLEGRO_MENUptr; pos: AL_INT; CONST title: AL_STRptr;
     id: AL_UINT16; flags: AL_INT; icon: ALLEGRO_BITMAPptr;
     submenu: ALLEGRO_MENUptr
   ): AL_BOOL;
   CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Removes the specified item from the menu and destroys it.  If the item
   contains a sub-menu, it too is destroyed.  Any references to it are
   invalidated. If you want to preserve that sub-menu, you should first make a
   copy with @link(al_clone_menu).

   This is safe to call on a menu that is currently being displayed.
   @returns(@true if an item was removed.)
   @seealso(al_append_menu_item) @seealso(al_insert_menu_item)
   @seealso(al_destroy_menu) *)
  FUNCTION al_remove_menu_item (menu: ALLEGRO_MENUptr; pos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Makes a copy of a menu so that it can be reused on another display.  The
   menu being cloned can be anything: a regular menu, a popup menu, or a
   sub-menu. @return(The cloned menu.)
   @seealso(al_clone_menu_for_popup) *)
  FUNCTION al_clone_menu (menu: ALLEGRO_MENUptr): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Exactly like @link(al_clone_menu), except that the copy is for a popup menu. *)
  FUNCTION al_clone_menu_for_popup (menu: ALLEGRO_MENUptr): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Destroys an entire menu, including its sub-menus. Any references to it or a
   sub-menu are no longer valid. It is safe to call this on a menu that is
   currently being displayed. @seealso(al_remove_menu_item) *)
  PROCEDURE al_destroy_menu (menu: ALLEGRO_MENUptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;

(* Returns the caption associated with the menu item.  It is valid as long as
   the caption is not modified.

   Returns @nil if the item was not found.
   @seealso(al_set_menu_item_caption) *)
  FUNCTION al_get_menu_item_caption (menu: ALLEGRO_MENUptr; pos: AL_INT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Updates the menu item caption with the new caption.  This will invalidate any
   previous calls to @seealso(al_get_menu_item_caption).
   @seealso(al_get_menu_item_caption) *)
  PROCEDURE al_set_menu_item_caption (menu: ALLEGRO_MENUptr; pos: AL_INT; CONST caption: AL_STR);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Returns the currently set flags.  See @link(al_insert_menu_item) for a
   description of the available flags.
   @returns(@code(-1) if the item was not found.)
   @seealso(al_set_menu_item_flags) *)
  FUNCTION al_get_menu_item_flags (menu: ALLEGRO_MENUptr; pos: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Updates the menu item's flags.  See @link(al_insert_menu_item) for a
   description of the available flags.
   @seealso(al_get_menu_item_flags) *)
  PROCEDURE al_set_menu_item_flags (menu: ALLEGRO_MENUptr; pos, flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Returns the icon associated with the menu.  It is safe to draw to the
   returned bitmap, but you must call @link(al_set_menu_item_icon) in order for
   the changes to be applied.
   @returns(@nil if the item was not found or if it has no icon.)
   @seealso(al_set_menu_item_icon) *)
  FUNCTION al_get_menu_item_icon (menu: ALLEGRO_MENUptr; pos: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Sets the icon for the specified menu item.  The menu assumes ownership of
   the @code(ALLEGRO_BITMAP) and may invalidate the pointer, so you must clone
   it if you wish to continue using it.

   If a video bitmap is passed, it will automatically be converted to a memory
   bitmap, so it is preferrable to pass a memory bitmap.)
   @seealso(al_get_menu_item_icon) @seealso(al_clone_bitmap) *)
  PROCEDURE al_set_menu_item_icon (
    menu: ALLEGRO_MENUptr; pos: AL_INT; icon: ALLEGRO_BITMAPptr
  );
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
{ Not sure if this should be declared.
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_NATIVE_DIALOG_SRC)
ALLEGRO_DIALOG_FUNC(int, al_toggle_menu_item_flags, (ALLEGRO_MENU *menu, int pos, int flags));
#endif
}

(* Searches in the haystack menu for any submenu with the given id. (Note that
   this only represents a literal ID, and cannot be used as an index.)
   @returns(The menu, if found. Otherwise returns @nil.)
   @seealso(al_find_menu_item) *)
  FUNCTION al_find_menu (haystack: ALLEGRO_MENUptr; id: AL_UINT16): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Searches in the haystack menu for an item with the given id. (Note that this
   only represents a literal ID, and cannot be used as an index.)

   If menu and index are not @nil, they will be set as the parent menu
   containing the item and the zero-based (positive) index of the item. (If the
   menu item was not found, then their values are undefined.)
   @returns(@true if the menu item was found.)
   @seealso(al_find_menu) *)
  FUNCTION al_find_menu_item (
    haystack: ALLEGRO_MENUptr; id: AL_INT16;
    menu: ALLEGRO_MENUptr_RESULT; index: AL_INTptr
  ): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Returns the default event source used for menu clicks.  If a menu was not
   given its own event source via @link(al_enable_menu_event_source), then it
   will use this default source.
   @seealso(al_register_event_source) @seealso(al_enable_menu_event_source)
   @seealso(al_disable_menu_event_source) *)
  FUNCTION al_get_default_menu_event_source: ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Enables a unique event source for this menu. It and all of its sub-menus
   will use this event source. (It is safe to call this multiple times on the
   same menu.)
   @returns(The event source.)
   @seealso(al_register_event_source)
   @seealso(al_get_default_menu_event_source)
   @seealso(al_disable_menu_event_source) *)
  FUNCTION al_enable_menu_event_source (menu: ALLEGRO_MENUptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Disables a unique event source for the menu, causing it to use the default
   event source.
   @seealso(al_get_default_menu_event_source)
   @seealso(al_enable_menu_event_source) *)
  PROCEDURE al_disable_menu_event_source (menu: ALLEGRO_MENUptr);
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Returns the menu associated with the display, or @nil if it does not have a
   menu. @seealso(al_set_display_menu) *)
  FUNCTION al_get_display_menu (display: ALLEGRO_DISPLAYptr): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Associates the menu with the display and shows it.  If there was a previous
   menu associated with the display, it will be destroyed.  If you don't want
   that to happen, you should first remove the menu with
   @link(al_remove_display_menu).

   If the menu is already attached to a display, it will not be attached to the
   new display.  If menu is @nil, the current menu will still be destroyed.

   @bold(Note:) Attaching a menu may cause the window as available to your
   application to be resized!  You should listen for a resize event, check how
   much space was lost, and resize the window accordingly if you want to
   maintain your window's prior size.
   @return(@true if successful.)
   @seealso(al_create_menu) @seealso(al_remove_display_menu) *)
  FUNCTION al_set_display_menu (display: ALLEGRO_DISPLAYptr; menu: ALLEGRO_MENUptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Displays a context menu next to the mouse cursor.  The menu must have been
   created with @link(al_create_popup_menu). It generates events just like a
   regular display menu does.  It is possible that the menu will be canceled
   without any selection being made.

   The display parameter indicates which window the menu is associated with
   (when you process the menu click event), but does not actually affect where
   the menu is located on the screen.
   @return(@true if the context menu was displayed.)
   @seealso(al_create_popup_menu) *)
  FUNCTION al_popup_menu (popup: ALLEGRO_MENUptr; display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Detaches the menu associated with the display and returns it.  The menu can
   then be used on a different display.

   If you simply want to destroy the active menu, you can call
   @link(al_set_display_menu) with a @nil menu.
   @seealso(al_set_display_menu) *)
  FUNCTION al_remove_display_menu (display: ALLEGRO_DISPLAYptr): ALLEGRO_MENUptr;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;



{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.  In
  delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here. }
{@exclude}
  FUNCTION _al_show_native_message_box (
    display: ALLEGRO_DISPLAYptr; CONST title, heading, str, buttons: AL_STR;
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
    );
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
