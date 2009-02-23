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
   additional text. *)
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



(* Converts four 8 bit values to a packed 32 bit integer ID. *)
  FUNCTION AL_ID (a, b, c, d: LONGINT): LONGINT;

(* Initialises the Allegro.pas library. *)
  FUNCTION al_install (system_id: LONGINT; errno_ptr: PLONGINT; atexit_ptr: POINTER): LONGINT;

(* Function which initialises the Allegro library. *)
  FUNCTION al_init: LONGINT;

VAR
(* Closes down the Allegro system. *)
  al_exit: PROCEDURE ; CDECL;

(* Outputs a message. *)
  PROCEDURE al_message (CONST msg: STRING);

VAR
(* On platforms that have a close button, this routine installs a callback
   function to handle the close event. *)
  al_set_close_button_callback: FUNCTION (proc: AL_SIMPLE_PROC): LONGINT; CDECL;

(* Finds out the currently selected desktop color depth. *)
  FUNCTION al_desktop_color_depth: LONGINT;

(* Finds out the currently selected desktop resolution. *)
  FUNCTION al_get_desktop_resolution (w, h: PLONGINT): LONGINT;

(* On platforms that are capable of it, this routine alters the window title
   for your Allegro program. *)
  PROCEDURE al_set_window_title (CONST title: STRING);



IMPLEMENTATION

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
  @_install_allegro_version_check := al_get_object_address ('_install_allegro_version_check');
  @al_exit := al_get_object_address ('allegro_exit');
  @al_set_close_button_callback := al_get_object_address ('set_close_button_callback');
  @_allegro_message := al_get_object_address ('allegro_message');
{ Idem with variables. }
  al_errno := al_get_object_address ('allegro_errno');
  al_error := al_get_object_address ('allegro_error');
  al_id_string := al_get_object_address ('allegro_id');
END.
