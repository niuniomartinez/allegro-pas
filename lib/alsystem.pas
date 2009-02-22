(* System level: initialization, cleanup, etc. *)
UNIT alsystem;

{ Defines the frame. }
{$MODE Delphi}
{$PACKRECORDS C}
{$H+}



INTERFACE

USES
  albase;



CONST
(* Tells to al_install that must autodetect the system driver. *)
  AL_SYSTEM_AUTODETECT	= 0;
(* Tells to al_install that must install a stripped down version of Allegro
   that won't even try to touch the hardware or do anything platform specific:
   this can be useful for situations where you only want to manipulate memory
   bitmaps, such as the text mode datafile tools or the Windows GDI interfacing
   functions.. *)
  AL_SYSTEM_NONE	= $4E4F4E45; { AL_ID ('N', 'O', 'N', 'E'); }
(* Defined to the major version of Allegro.  From a version number like 4.1.16,
   this would be defined to the integer 4. *)
  AL_VERSION = 4;
(* Defined to the middle version of Allegro.  From a version number like
   4.1.16, this would be defined to the integer 1. *)
  AL_SUB_VERSION = 2;
(* Defined to the minor version of Allegro.pas.  From a version number like
   4.1.16, this would be defined to the integer 16. *)
  AL_PAS_VERSION = 3;
(* Defined to TRUE if current version is a BETA version.  A BETA version is a
   test version and may be uncomplete or untested. *)
  AL_PAS_IS_BETA = TRUE;
(* Defined to a text string containing all version numbers and maybe some
   additional text.  This could be `4.1.16 BETA' for an Allegro.pas BETA
   version. *)
  AL_PAS_VERSION_STR = '4.2.3 WIP';



VAR
(* Stores the last error number. *)
  al_errno: PLONGINT;
(* Text string used by al_set_gfx_mode, al_install_sound and other functions to
   report error messages.  If they fail and you want to tell the user why, this
   is the place to look for a description of the problem. *)
  al_error,
(* Text string containing a date and version number for the library, in case
   you want to display these somewhere. *)
  al_id_string: PCHAR;



(* Converts four 8 bit values to a packed 32 bit integer ID.

   This function can be used to create a packed 32 bit integer from 8 bit
   characters, on both 32 and 64 bit machines.  These can be used for various
   things, like custom datafile objects or system IDs. *)
  FUNCTION AL_ID (a, b, c, d: LONGINT): LONGINT;

(* Initialises the Allegro.pas library.  You must call either this or al_init
   before doing anything other.  The functions that can be called before this
   one will be marked explicitly in the documentation, like al_set_config_file.

   The available system ID codes will vary from one platform to another, but
   you will almost always want to pass AL_SYSTEM_AUTODETECT.  Alternatively,
   AL_SYSTEM_NONE installs a stripped down version of Allegro that won't even
   try to touch your hardware or do anything platform specific:  this can be
   useful for situations where you only want to manipulate memory bitmaps,
   such as the text mode datafile tools or the Windows GDI interfacing
   functions.

   The `errno_ptr' should point to the errno variable from your libc:  this is
   required because when Allegro is linked as a DLL, it doesn't have direct
   access to your local libc data.  The `atexit_ptr' parameter is for
   compatibility purpose only and may be NIL. *)
  FUNCTION al_install (system_id: LONGINT; errno_ptr: PLONGINT; atexit_ptr: POINTER): LONGINT;

(* Function which initialises the Allegro library.  This is the same thing as
   calling al_install (AL_SYSTEM_AUTODETECT, @errno, NIL), where "errno" is a
   variable. *)
  FUNCTION al_init: LONGINT;

VAR
(* Closes down the Allegro system.  This includes returning the system to text
   mode and removing whatever mouse, keyboard, and timer routines have been
   installed.  This procedure must be called before exit the program.

   Note that after you call this function, other functions like
   al_destroy_bitmap will most likely crash. *)
  al_exit: PROCEDURE ; CDECL;

(* Outputs a message.  Usually you want to use this to report messages to the
   user in an OS independant way when some Allegro subsystem cannot be
   initialised.  But you must not use this function if you are in a graphic
   mode, only before calling al_set_gfx_mode, or after a
   al_set_gfx_mode (AL_GFX_TEXT).  Also, this function depends on a system
   driver being installed, which means that it won't display the message at all
   on some platforms if Allegro has not been initialised correctly.

   On platforms featuring a windowing system, it will bring up a blocking GUI
   message box.  If there is no windowing system, it will try to print the
   string to a text console, attempting to work around codepage differences by
   reducing any accented characters to 7-bit ASCII approximations. *)
  PROCEDURE al_message (CONST msg: STRING);

VAR
(* On platforms that have a close button, this routine installs a callback
   function to handle the close event.  In other words, when the user clicks
   the close button on your program's window or any equivalent device, the
   function you specify here will be called.

   This function should not generally attempt to exit the program or save any
   data itself.  The function could be called at any time, and there is usually
   a risk of conflict with the main thread of the program.  Instead, you should
   set a flag during this function, and test it on a regular basis in the main
   loop of the program.

   Pass NIL as the `proc' argument to this function to disable the close button
   functionality, which is the default state.

   Returns zero on success and non-zero on failure (e.g. the feature is not
   supported by the platform). *)
  al_set_close_button_callback: FUNCTION (proc: AL_SIMPLE_PROC): LONGINT; CDECL;

(* Finds out the currently selected desktop color depth.  You can use this
   information to make your program use the same color depth as the desktop,
   which will likely make it run faster because the graphic driver won't be
   doing unnecessary color conversions behind your back.

   Under some OSes, switching to a full screen graphics mode may automatically
   change the desktop color depth.  You have, therefore, to call this function
   before setting any graphics mode in order to retrieve the real desktop color
   depth.

   Returns the color depth or zero on platforms where this information is not
   available or does not apply. *)
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

   Returns zero on success, or a negative number if this information is not
   available or does not apply, in which case the values stored in the
   variables you provided for `width' and `height' are undefined. *)
  FUNCTION al_get_desktop_resolution (w, h: PLONGINT): LONGINT;

(* On platforms that are capable of it, this routine alters the window title
   for your Allegro program.  Note that Allegro cannot set the window title
   when running in a DOS box under Windows.

   Note: On Delphi compilers this procedure hasn't any effect, so the window
   will keep its default caption. *)
  PROCEDURE al_set_window_title (CONST title: STRING);



IMPLEMENTATION

USES
  dynlibs;



VAR
(* Function for internal use. *)
  _install_allegro_version_check: FUNCTION (
	system_id: LONGINT; errno_ptr: PLONGINT; atexit_ptr: POINTER;
	version: LONGINT): LONGINT; CDECL;
(* Function for messages. *)
  _allegro_message: PROCEDURE (CONST msg: PCHAR); CDECL;
(* To be used as "errnum". *)
  NumError: LONGINT;



(* Converts four 8 bit values to a packed 32 bit integer ID. *)
FUNCTION AL_ID (a, b, c, d: LONGINT): LONGINT;
BEGIN
  AL_ID := (a SHL 24) OR (b SHL 16) OR (c SHL 8) OR d;
END;



(* Initialises the Allegro.pas library. *)
FUNCTION al_install (system_id: LONGINT; errno_ptr: PLONGINT;
		     atexit_ptr: POINTER): LONGINT;
BEGIN
  al_install := _install_allegro_version_check (system_id, errno_ptr, atexit_ptr,
	(4 SHL 16) OR (2 SHL 8) OR 2);
END;



(* Initialises the Allegro.pas library. *)
FUNCTION al_init: LONGINT;
BEGIN
  al_init := al_install (AL_SYSTEM_AUTODETECT, @NumError, NIL);
END;




(* Outputs a message. *)
PROCEDURE al_message (CONST msg: AL_STRING);
BEGIN
  _allegro_message (PCHAR (msg));
END;




(* Finds out the currently selected desktop color depth. *)
FUNCTION al_desktop_color_depth: LONGINT;
BEGIN
{ It's implemented using inline and drivers so I'll keep it this way at moment. }
  al_desktop_color_depth := 0;
END;



(* Finds out the currently selected desktop resolution. *)
FUNCTION al_get_desktop_resolution (w, h: PLONGINT): LONGINT;
BEGIN
{ It's implemented using inline and drivers so I'll keep it this way at moment. }
  al_get_desktop_resolution := -1;
END;



(* This routine alters the window title. *)
PROCEDURE al_set_window_title (CONST title: AL_STRING);
BEGIN
{ It's implemented using inline and drivers so I'll keep it this way at moment. }
END;



INITIALIZATION
{ Gets the function and procedure address. }
  @_install_allegro_version_check := GetProcAddress (__al_library_id__, '_install_allegro_version_check');
  @al_exit := GetProcAddress (__al_library_id__, 'allegro_exit');
  @al_set_close_button_callback := GetProcAddress (__al_library_id__, 'set_close_button_callback');
  @_allegro_message := GetProcAddress (__al_library_id__, 'allegro_message');
{ Idem with variables. }
  al_errno := GetProcAddress (__al_library_id__, 'allegro_errno');
  al_error := GetProcAddress (__al_library_id__, 'allegro_error');
  al_id_string := GetProcAddress (__al_library_id__, 'allegro_id');
END.
