UNIT altimer;
(*< Allegro can set up several virtual timer functions, all going at different
    speeds.

    They are usually implemented using threads, which run parallel to the main
    thread.  Therefore timer callbacks on such platforms will not block the
    main thread when called, so you may need to use appropriate synchronisation
    devices (eg. mutexes, semaphores, etc.) when accessing data that is shared
    by a callback and the main thread.  (Currently Allegro does not provide
    such devices.) *)

{$IFDEF FPC}
{ Free Pascal. }
 {$MODE FPC}
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}
{$H+}



INTERFACE

USES
  albase;



(* Utils for time calculations. *)
  FUNCTION AL_SECS_TO_TIMER (x: LONGINT): LONGINT;
  FUNCTION AL_MSEC_TO_TIMER (x: LONGINT): LONGINT;
  FUNCTION AL_BPS_TO_TIMER  (x: LONGINT): LONGINT;
  FUNCTION AL_BPM_TO_TIMER  (x: LONGINT): LONGINT;



(* Timer *)
  FUNCTION al_install_timer: BOOLEAN;

  PROCEDURE al_remove_timer; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_timer';

  FUNCTION al_install_int_ex (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;

  FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;

  PROCEDURE al_remove_int (proc: AL_SIMPLE_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_int';

  FUNCTION al_install_param_int_ex (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;

  FUNCTION al_install_param_int (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;

  PROCEDURE al_remove_param_int (proc: AL_PARAM_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_param_int';

  PROCEDURE al_rest (time: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest';
  PROCEDURE al_rest_callback (time: LONGINT; callback: AL_PARAM_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest_callback';



VAR
(* Retrace counter or simulated. *)
  al_retrace_count: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'retrace_count';



IMPLEMENTATION

(* Timer *)

{ Delphi can't access to the public variables from Allegro, so we need some
  magic to access them. }
  FUNCTION install_timer: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

FUNCTION al_install_timer: BOOLEAN;
BEGIN
  al_install_timer := install_timer = 0;
END;



FUNCTION install_int_ex (proc: AL_SIMPLE_PROC; speed: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

FUNCTION al_install_int_ex (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;
BEGIN
  al_install_int_ex := install_int_ex (proc, speed) = 0;
END;



FUNCTION install_int (proc: AL_SIMPLE_PROC; speed: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;
BEGIN
  al_install_int := install_int (proc, speed) = 0;
END;



FUNCTION install_param_int_ex (proc: AL_PARAM_PROC; speed: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

FUNCTION al_install_param_int_ex (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;
BEGIN
  al_install_param_int_ex := install_param_int_ex (proc, speed) = 0;
END;



FUNCTION install_param_int (proc: AL_PARAM_PROC; speed: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

FUNCTION al_install_param_int (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;
BEGIN
  al_install_param_int := install_param_int (proc, speed) = 0;
END;



(* Utils for time calculations. *)
CONST
  AL_TIMERS_PER_SECOND = 1193181;



FUNCTION AL_SECS_TO_TIMER (x: LONGINT): LONGINT;
BEGIN
  AL_SECS_TO_TIMER := x * AL_TIMERS_PER_SECOND;
END;



FUNCTION AL_MSEC_TO_TIMER (x: LONGINT): LONGINT;
BEGIN
  AL_MSEC_TO_TIMER := x * (AL_TIMERS_PER_SECOND DIV 1000);
END;



FUNCTION AL_BPS_TO_TIMER  (x: LONGINT): LONGINT;
BEGIN
  AL_BPS_TO_TIMER := AL_TIMERS_PER_SECOND DIV x;
END;



FUNCTION AL_BPM_TO_TIMER  (x: LONGINT): LONGINT;
BEGIN
  AL_BPM_TO_TIMER := (60 * AL_TIMERS_PER_SECOND) DIV x;
END;



END.

