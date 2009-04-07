UNIT alsystem;
(*< System level: initialization, cleanup, etc. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$MODE FPC}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}
{$H+}


INTERFACE

USES
  albase;



CONST
(* Tells to @link(al_install) that must autodetect the system driver. *)
  AL_SYSTEM_AUTODETECT	= 0;
(* Tells to @link(al_install) that must install a stripped down version of
   Allegro that won't even try to touch the hardware or do anything platform
   specific:  this can be useful for situations where you only want to
   manipulate memory bitmaps, such as the text mode datafile tools or the
   Windows GDI interfacing functions.

   Is equivalent to @code(AL_ID @('NONE'@);). *)
  AL_SYSTEM_NONE	= $4E4F4E45;
(* Defined to the major version of Allegro.  From a version number like 4.1.16,
   this would be defined to the integer 4. *)
  AL_VERSION = 4;
(* Defined to the middle version of Allegro.  From a version number like
   4.1.16, this would be defined to the integer 1. *)
  AL_SUB_VERSION = 3;
(* Defined to the minor version of Allegro.pas.  From a version number like
   4.1.16, this would be defined to the integer 16. *)
  AL_PAS_VERSION = 0;
(* Defined to TRUE if current version is a BETA version.  A BETA version is a
   test version and may be uncomplete or untested. *)
  AL_PAS_IS_BETA = TRUE;
(* Defined to a text string containing all version numbers and maybe some
   additional text. *)
  AL_PAS_VERSION_STR = '4.3.0 SVN';



VAR
(* Stores the last error number. *)
  al_errno: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_errno';
(* Text string used by @link(al_set_gfx_mode), @link(al_install_sound) and
   other functions to report error messages.  If they fail and you want to tell
   the user why, this is the place to look for a description of the problem. *)
  al_error: STRING; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_error';
(* Text string containing a date and version number for the library, in case
   you want to display these somewhere. *)
  al_id_string: STRING; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_id';



  (* This function can be used to create a packed 32 bit integer from 8 bit
     characters, on both 32 and 64 bit machines.  These can be used for various
     things, like custom datafile objects or system IDs. Example:

@longcode(#
VAR
  OSTYPE_LINUX: LONGINT;
BEGIN
  OSTYPE_LINUX := AL_ID('TUX '));
END;
  #) *)
  FUNCTION AL_ID (str: SHORTSTRING): LONGINT;



(* Initialises the Allegro library.  You must call either this or al_init
   before doing anything other.  The functions that can be called before this
   one will be marked explicitly in the documentation, like
   @link(al_set_config_file).

   The available system ID codes will vary from one platform to another, but
   you will almost always want to pass @link(AL_SYSTEM_AUTODETECT).
   Alternatively, @link(AL_SYSTEM_NONE) installs a stripped down version of
   Allegro that won't even try to touch your hardware or do anything platform
   specific:  this can be useful for situations where you only want to
   manipulate memory bitmaps, such as the text mode datafile tools or the
   Windows GDI interfacing functions.
   @param(system_id Identification of the system.)
   @returns(@true on success or @false on failure @(e.g. no system driver
     could be used@).) *)
  FUNCTION al_install (system_id: LONGINT): BOOLEAN;

(* Function which initialises the Allegro library.  This is the same thing as
   calling @code(al_install @(AL_SYSTEM_AUTODETECT@)).

   @returns(@true on success or @false on failure @(e.g. no system driver
     could be used@).) *)
  FUNCTION al_init: BOOLEAN;

(* Closes down the Allegro system.  This includes returning the system to text
   mode and removing whatever mouse, keyboard, and timer routines have been
   installed.  This procedure must be called before exit the program.

   Note that after you call this function, other functions like
   al_destroy_bitmap will most likely crash. *)
  PROCEDURE al_exit; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_exit';



(* Outputs a message.  Usually you want to use this to report messages to the
   user in an OS independant way when some Allegro subsystem cannot be
   initialised.  But you must not use this function if you are in a graphic
   mode, only before calling @link(al_set_gfx_mode), or after a
   @code(al_set_gfx_mode @(AL_GFX_TEXT@)).  Also, this function depends on a
   system driver being installed, which means that it won't display the message
   at all on some platforms if Allegro has not been initialised correctly.

   On platforms featuring a windowing system, it will bring up a blocking GUI
   message box.  If there is no windowing system, it will try to print the
   string to a text console, attempting to work around codepage differences by
   reducing any accented characters to 7-bit ASCII approximations. Example:

@longcode(#
  IF NOT al_init THEN
    EXIT (1);
  IF NOT init_my_data THEN
  BEGIN
    al_message ('Sorry, missing game data!\n');
    EXIT (2);
  END;
  #) *)
  PROCEDURE al_message (CONST msg: STRING);



(* On platforms that have a close button, this routine installs a callback
   function to handle the close event.  In other words, when the user clicks
   the close button on your program's window or any equivalent device, the
   function you specify here will be called.

   This function should not generally attempt to exit the program or save any
   data itself.  The function could be called at any time, and there is usually
   a risk of conflict with the main thread of the program.  Instead, you should
   set a flag during this function, and test it on a regular basis in the main
   loop of the program.

   Pass @nil as the `proc' argument to this function to disable the close
   button functionality, which is the default state.

   Example:

@longcode(#
VAR
  CloseButtonPressed: BOOLEAN = FALSE;

PROCEDURE CloseButtonHandler; CDECL;
BEGIN
  ClosePuttonPressed := TRUE;
END;

    ...
  al_init;
  al_set_close_button_callback (@CloseButtonHandler);
    ...
  WHILE NOT ClosePuttonPressed DO
    DoStuff;
  #)
   @returns(@true on success or @false on failure @(e.g. the feature is not
     supported by the platform@).) *)
  FUNCTION al_set_close_button_callback (proc: AL_SIMPLE_PROC): BOOLEAN;



(* Finds out the currently selected desktop color depth.  You can use this
   information to make your program use the same color depth as the desktop,
   which will likely make it run faster because the graphic driver won't be
   doing unnecessary color conversions behind your back.

   Under some OSes, switching to a full screen graphics mode may automatically
   change the desktop color depth.  You have, therefore, to call this function
   before setting any graphics mode in order to retrieve the real desktop color
   depth.
   @returns(the color depth or zero on platforms where this information is not
   available or does not apply.) *)
  FUNCTION al_desktop_color_depth: LONGINT;



(* Finds out the currently selected desktop resolution.  You can use this
   information to avoid creating windows bigger than the current resolution.
   This is especially important for some windowed drivers which are unable to
   create windows bigger than the desktop.  Each parameter is a pointer to an
   integer where one dimension of the screen will be stored.

   Under some OSes, switching to a full screen graphics mode may automatically
   change the desktop resolution.  You have, therefore, to call this function
   before setting any graphics mode in order to retrieve the real desktop
   resolution.
   @param(w Width desktop resolution.) @param(h Height desktop resolution.)
   @returns(@true on success, or @false if this information is not available or
   does not apply, in which case the values stored in the variables you
   provided for `width' and `height' are undefined.) *)
  FUNCTION al_get_desktop_resolution (VAR w, h: LONGINT): BOOLEAN;


(* On platforms that are capable of it, this routine alters the window title
   for your Allegro program.
   @param(title Title string.) *)
  PROCEDURE al_set_window_title (CONST title: STRING);



IMPLEMENTATION

TYPE
{$include alsysdrv.inc}



VAR
(* To be used as "errnum". *)
  NumError: LONGINT;
(* To access to stytem drivers. *)
  system_driver: __SYSTEM_DRIVER__PTR; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

(* Function for internal use. *)
  FUNCTION _install_allegro_version_check (
	system_id: LONGINT; errno_ptr: PLONGINT; atexit_ptr: POINTER;
	version: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

(* Function for messages. *)
  PROCEDURE _allegro_message (CONST msg: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_message';

(* Function for close button handler. *)
  FUNCTION _set_close_button_callback (proc: AL_SIMPLE_PROC): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_close_button_callback';



(* Converts four 8 bit values to a packed 32 bit integer ID. *)
FUNCTION AL_ID (str: SHORTSTRING): LONGINT;
BEGIN
  AL_ID := (ORD (str[1]) SHL 24) OR (ORD (str[2]) SHL 16)
        OR (ORD (str[3]) SHL  8) OR  ORD (str[4]);
END;



(* Initialises the Allegro library. *)
FUNCTION al_install (system_id: LONGINT): BOOLEAN;
BEGIN
  al_install := _install_allegro_version_check (system_id, @NumError, NIL,
	(4 SHL 16) OR (3 SHL 8) OR 10) = 0;
END;



(* Initialises the Allegro library. *)
FUNCTION al_init: BOOLEAN;
BEGIN
  al_init := _install_allegro_version_check (AL_SYSTEM_AUTODETECT, @NumError,
	NIL, (4 SHL 16) OR (3 SHL 8) OR 10) = 0;
END;



(* Outputs a message. *)
PROCEDURE al_message (CONST msg: STRING);
BEGIN
  _allegro_message (PCHAR (msg));
END;



(* On platforms that have a close button, this routine installs a callback
   function to handle the close event. *)
FUNCTION al_set_close_button_callback (proc: AL_SIMPLE_PROC): BOOLEAN;
BEGIN
  al_set_close_button_callback := (_set_close_button_callback (proc) = 0);
END;




(* Finds out the currently selected desktop color depth. *)
FUNCTION al_desktop_color_depth: LONGINT;
BEGIN
  IF system_driver^.desktop_color_depth <> NIL THEN
    al_desktop_color_depth := (system_driver^.desktop_color_depth ())
  ELSE
    al_desktop_color_depth := 0;
END;



(* Finds out the currently selected desktop resolution. *)
FUNCTION al_get_desktop_resolution (VAR w, h: LONGINT): BOOLEAN;
BEGIN
  IF system_driver^.get_desktop_resolution <> NIL THEN
    al_get_desktop_resolution := (system_driver^.get_desktop_resolution (@w, @h) = 0)
  ELSE
    al_get_desktop_resolution := FALSE;
END;



(* This routine alters the window title. *)
PROCEDURE al_set_window_title (CONST title: STRING);
BEGIN
  IF system_driver^.set_window_title <> NIL THEN
    system_driver^.set_window_title (PCHAR (title));
END;

END.
