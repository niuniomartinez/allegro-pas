UNIT aljstick;
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
 *	Joystick interface.
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
  AL_JOY_TYPE_AUTODETECT = -(1);
  AL_JOY_TYPE_NONE = 0;

  AL_MAX_JOYSTICKS = 8;
  AL_MAX_JOYSTICK_AXIS = 3;
  AL_MAX_JOYSTICK_STICKS = 5;
  AL_MAX_JOYSTICK_BUTTONS = 32;

(* joystick status flags *)
  AL_JOYFLAG_DIGITAL = 1;
  AL_JOYFLAG_ANALOGUE = 2;
  AL_JOYFLAG_CALIB_DIGITAL = 4;
  AL_JOYFLAG_CALIB_ANALOGUE = 8;
  AL_JOYFLAG_CALIBRATE = 16;
  AL_JOYFLAG_SIGNED = 32;
  AL_JOYFLAG_UNSIGNED = 64;
{ alternative spellings  }
  AL_JOYFLAG_ANALOG = AL_JOYFLAG_ANALOGUE;
  AL_JOYFLAG_CALIB_ANALOG = AL_JOYFLAG_CALIB_ANALOGUE;



TYPE
(* information about a single joystick axis  *)
  AL_JOYSTICK_AXIS_INFO = RECORD
    pos : AL_INT;
    d1 : AL_INT;
    d2 : AL_INT;
    name : PCHAR;
  END;

(* information about one or more axis (a slider or directional control) *)
  AL_JOYSTICK_STICK_INFO = RECORD
    flags : AL_INT;
    num_axis : AL_INT;
    axis : ARRAY [0..(AL_MAX_JOYSTICK_AXIS)-1] OF AL_JOYSTICK_AXIS_INFO;
    name : PCHAR;
  END;

(* information about a joystick button *)
  AL_JOYSTICK_BUTTON_INFO = RECORD
    b : AL_INT;
    name : PCHAR;
  END;

(* information about an entire joystick *)
  AL_JOYSTICK_INFOptr = ^AL_JOYSTICK_INFO;
  AL_JOYSTICK_INFO = RECORD
    flags : AL_INT;
    num_sticks : AL_INT;
    num_buttons : AL_INT;
    stick : ARRAY [0..(AL_MAX_JOYSTICK_STICKS)-1] OF AL_JOYSTICK_STICK_INFO;
    button : ARRAY [0..(AL_MAX_JOYSTICK_BUTTONS)-1] OF AL_JOYSTICK_BUTTON_INFO;
  END;

  AL_JOYSTICK_INFO_LISTptr = ^AL_JOYSTICK_INFO_LIST;
  AL_JOYSTICK_INFO_LIST = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_JOYSTICK_INFO;



VAR
(* global joystick information. *)
  al_joy: AL_JOYSTICK_INFO_LISTptr;
  al_num_joysticks: AL_INTptr;



  FUNCTION al_install_joystick (atype: AL_INT): AL_INT;
  PROCEDURE al_remove_joystick; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_joystick';

  FUNCTION al_calibrate_joystick_name (n: AL_INT): AL_STRING;
  FUNCTION al_calibrate_joystick (n: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'calibrate_joystick';
    
  FUNCTION al_save_joystick_data (filename: AL_STRING): AL_INT;
  FUNCTION al_load_joystick_data (filename: AL_STRING): AL_INT;

  FUNCTION al_poll_joystick: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'poll_joystick';
    

    
IMPLEMENTATION

(* Delphi can't access to the public variables from Allegro, so we need some
 * magic to access them. *)
  FUNCTION install_joystick (atype: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION _get_joy_: AL_JOYSTICK_INFO_LISTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;
  FUNCTION _get_num_joysticks_: AL_INTptr;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

FUNCTION al_install_joystick (atype: AL_INT): AL_INT;
VAR
  R: AL_INT;
BEGIN
  R := install_joystick (atype);
  IF R = 0 THEN
  BEGIN
    al_joy := _get_joy_;
    al_num_joysticks := _get_num_joysticks_;
  END;
  al_install_joystick := R;
END;



  FUNCTION calibrate_joystick_name (n: AL_INT): PCHAR; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'calibrate_joystick_name';

  FUNCTION al_calibrate_joystick_name (n: AL_INT): AL_STRING;
  VAR
    Tmp: PCHAR;
  BEGIN
    Tmp := calibrate_joystick_name (n);
    al_calibrate_joystick_name := (Tmp);
  END;



  FUNCTION save_joystick_data (filename: PCHAR): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_joystick_data';

  FUNCTION al_save_joystick_data (filename: AL_STRING): AL_INT;
  BEGIN
    al_save_joystick_data := save_joystick_data (PCHAR (filename));
  END;



  FUNCTION load_joystick_data (filename: PCHAR): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_joystick_data';

  FUNCTION al_load_joystick_data (filename: AL_STRING): AL_INT;
  BEGIN
    al_load_joystick_data := load_joystick_data (PCHAR (filename));
  END;

END.
