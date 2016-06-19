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
  (* Opaque handle to a native file dialog.
     @seealso(al_create_native_file_dialog) *)
    ALLEGRO_FILECHOOSERptr = AL_POINTER;
  (* Opaque handle to a text log window.
     @seealso(al_open_native_text_log) *)
    ALLEGRO_TEXTLOGptr = AL_POINTER;

(* Initialise the native dialog addon.
   @return(@true on success, @false on error.)
   @seealso(al_shutdown_native_dialog_addon) *)
  FUNCTION al_init_native_dialog_addon: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;
(* Shut down the native dialog addon.
   @seealso(al_init_native_dialog_addon) *)
  PROCEDURE al_shutdown_native_dialog_addon;
    CDECL; EXTERNAL ALLEGRO_NATIVE_DLG_LIB_NAME;



(******************************************************************************
  These const lines were in the end of the  C header.
 **********)

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
  FUNCTION al_create_native_file_dialog (CONST initial_path, title, patterns: AL_STR; Mode: AL_INT): ALLEGRO_FILECHOOSERptr;
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
  FUNCTION al_get_native_file_dialog_path (CONST dialog: ALLEGRO_FILECHOOSERptr; index: AL_SIZE_T): AL_STRptr;
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
  FUNCTION al_show_native_message_box (display: ALLEGRO_DISPLAYptr; CONST title, heading, str, buttons: STRING; flags: AL_INT): AL_INT;
    INLINE;

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
  FUNCTION al_open_native_text_log (CONST title: AL_STR; flags: AL_INT): ALLEGRO_TEXTLOGptr;
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
  PROCEDURE al_append_native_text_log (textlog: ALLEGRO_TEXTLOGptr; CONST str: AL_STR);
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

{ TODO: Creating/modifying menu. }

(* Returns the (compiled) version of the addon, in the same format as
   @link(al_get_allegro_version). *)
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
