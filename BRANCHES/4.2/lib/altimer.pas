UNIT altimer;
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
 *	Timer routines.
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



(* Utils for time calculations. *)
  FUNCTION AL_SECS_TO_TIMER (x: AL_LONG): AL_LONG;
  FUNCTION AL_MSEC_TO_TIMER (x: AL_LONG): AL_LONG;
  FUNCTION AL_BPS_TO_TIMER  (x: AL_LONG): AL_LONG;
  FUNCTION AL_BPM_TO_TIMER  (x: AL_LONG): AL_LONG;



(* Timer *)
  FUNCTION al_install_timer: AL_INT;
  PROCEDURE al_remove_timer; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_timer';

  FUNCTION al_install_int_ex (proc: AL_SIMPLE_PROC; speed: AL_LONG): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'install_int_ex';
  FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: AL_LONG): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'install_int';
  PROCEDURE al_remove_int (proc: AL_SIMPLE_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_int';

  FUNCTION al_install_param_int_ex (proc: AL_PARAM_PROC; speed: AL_LONG): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'install_param_int_ex';
  FUNCTION al_install_param_int (proc: AL_PARAM_PROC; speed: AL_LONG): AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'install_param_int';
  PROCEDURE al_remove_param_int (proc: AL_PARAM_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_param_int';

  PROCEDURE al_rest (time: AL_INT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest';
  PROCEDURE al_rest_callback (time: AL_INT; callback: AL_PARAM_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest_callback';



VAR
(* Retrace counter or simulated. *)
  al_retrace_count: AL_INTptr;



IMPLEMENTATION

(* Timer *)

{ Delphi can't access to the public variables from Allegro, so we need some
  magic to access them. }
  FUNCTION install_timer: AL_INT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION _get_retrace_count_: AL_INTptr; CDECL;
    EXTERNAL ALL_PAS_SHARED_LIBRARY_NAME;

FUNCTION al_install_timer: AL_INT;
VAR
  R: AL_INT;
BEGIN
  R := install_timer;
  IF R = 0 THEN
    al_retrace_count := _get_retrace_count_;
  al_install_timer := R;
END;



(* Utils for time calculations. *)
CONST
  AL_TIMERS_PER_SECOND = 1193181;



FUNCTION AL_SECS_TO_TIMER (x: AL_LONG): AL_LONG;
BEGIN
  AL_SECS_TO_TIMER := x * AL_TIMERS_PER_SECOND;
END;



FUNCTION AL_MSEC_TO_TIMER (x: AL_LONG): AL_LONG;
BEGIN
  AL_MSEC_TO_TIMER := x * (AL_TIMERS_PER_SECOND DIV 1000);
END;



FUNCTION AL_BPS_TO_TIMER  (x: AL_LONG): AL_LONG;
BEGIN
  AL_BPS_TO_TIMER := AL_TIMERS_PER_SECOND DIV x;
END;



FUNCTION AL_BPM_TO_TIMER  (x: AL_LONG): AL_LONG;
BEGIN
  AL_BPM_TO_TIMER := (60 * AL_TIMERS_PER_SECOND) DIV x;
END;



END.

