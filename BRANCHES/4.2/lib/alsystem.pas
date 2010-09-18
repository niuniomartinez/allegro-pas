UNIT alsystem;
(*
  ______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\ 
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	System level: initialization, cleanup, etc.
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$LONGSTRINGS ON}
{$ELSE}
{ Assumes Borland Delphi/Turbo. }
 {$A-}
 {$H+}
{$ENDIF}



INTERFACE

USES
  albase;



CONST
(* System identification. *)
  AL_SYSTEM_AUTODETECT	= 0;
  AL_SYSTEM_NONE	= $4E4F4E45; { AL_ID ('N', 'O', 'N', 'E'); }

  AL_VERSION = 4;
  AL_SUB_VERSION = 2;
  AL_PAS_VERSION = 3;
  AL_PAS_IS_BETA = TRUE;

  AL_PAS_VERSION_STR = '4.2.3 Beta';



VAR
(* Global variables. *)
  al_errno: AL_INTPTR;
  al_id_string, al_error: PCHAR;



  FUNCTION AL_ID (a, b, c, d: AL_INT): AL_INT; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME NAME 'get_AL_ID';

(* Initialization. *)
  FUNCTION al_install (system_id: AL_INT; errno_ptr: AL_INTPTR; atexit_ptr: AL_PTR): AL_INT;
  FUNCTION al_init: AL_INT;

(* Finalization. *)
  PROCEDURE al_exit; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_exit';

(* Messages. *)
  PROCEDURE al_message (CONST msg: AL_STRING);

(* System configuration. *)
  FUNCTION al_set_close_button_callback (proc: AL_SIMPLE_PROC): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_close_button_callback';

  FUNCTION al_desktop_color_depth: AL_INT; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

  FUNCTION al_get_desktop_resolution (w, h: AL_INTptr): AL_INT; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

  PROCEDURE al_set_window_title (CONST title: AL_STRING);

  PROCEDURE __al_init_system__; { For internal use. }



IMPLEMENTATION

USES
  alfixed, alpalete, altext;



{ Delphi can't access to the public variables from Allegro, so we need some
  magic to access them. }
  FUNCTION _al_install_ (system_id: AL_INT; errno_ptr: AL_INTPTR;
		       atexit_ptr: AL_PTR): AL_INT; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME NAME 'al_install';
  FUNCTION _al_init_: AL_INT; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME NAME 'al_init';
  FUNCTION _get_al_id_string_: PCHAR; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_al_errno_: AL_INTPTR; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_al_error_: PCHAR; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_black_palette_: AL_PALETTEptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_default_palette_: AL_PALETTEptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_desktop_palette_: AL_PALETTEptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_font_: AL_PTR; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_allegro_404_char_: AL_INTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;



FUNCTION al_install (system_id: AL_INT; errno_ptr: AL_INTPTR; atexit_ptr: AL_PTR): AL_INT;
VAR
  R: AL_INT;
BEGIN
  R := _al_install_ (system_id, errno_ptr, atexit_ptr);
  IF R = 0 THEN __al_init_system__;
  al_install := R;
END;



FUNCTION al_init: AL_INT;
VAR
  R: AL_INT;
BEGIN
  R := _al_init_;
  IF R = 0 THEN __al_init_system__;
  BEGIN
  END;
  al_init := R;
END;



(* __al_init_system__:
 *   Initialises variables and system.  It's also useful if using a 3rd party
 *   library that initialises Allegro. *)
PROCEDURE __al_init_system__;
BEGIN
{ Get pointers of public variables. }
  al_id_string := _get_al_id_string_;
  al_errno := _get_al_errno_;
  al_error := _get_al_error_;
  al_black_palette := _get_black_palette_;
  al_default_palette := _get_default_palette_;
  al_desktop_palette := _get_desktop_palette_;
  al_font := _get_font_;
  al_404_char := _get_allegro_404_char_;

  __al_inittrig__;
END;



PROCEDURE message (CONST msg: PCHAR); CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'allegro_message';

PROCEDURE al_message (CONST msg: AL_STRING);
BEGIN
  message (PCHAR (msg));
END;



{$IFDEF FPC}
PROCEDURE set_window_title (CONST title: PCHAR);
  EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME NAME 'al_set_window_title';
{$ENDIF}

PROCEDURE al_set_window_title (CONST title: AL_STRING);
BEGIN
{ Delphi compiles it but it throws a segment fault! }
{$IFDEF FPC}
  set_window_title (PCHAR (title));
{$ENDIF}
END;

END.

