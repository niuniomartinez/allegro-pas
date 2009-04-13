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
{$ENDIF}

INTERFACE

USES
  albase;



(* Give the number of seconds between each tick to @link(al_install_int_ex). *)
  FUNCTION AL_SECS_TO_TIMER (x: LONGINT): LONGINT;
(* Give the number of milliseconds between each tick to
   @link(al_install_int_ex). *)
  FUNCTION AL_MSEC_TO_TIMER (x: LONGINT): LONGINT;
(* Give the number of ticks each second to @link(al_install_int_ex). *)
  FUNCTION AL_BPS_TO_TIMER  (x: LONGINT): LONGINT;
(* Give the number of ticks each minute to @link(al_install_int_ex). *)
  FUNCTION AL_BPM_TO_TIMER  (x: LONGINT): LONGINT;



(* Installs the Allegro timer interrupt handler.  You must do this before
   installing any user timer routines, and also before displaying a mouse
   pointer and playing FLI animations or MIDI music.

   @returns(@true on success, or @false on failure @(but you may decide not to
     check the return value as this function is very unlikely to fail@).)  *)
  FUNCTION al_install_timer: BOOLEAN;

(* Removes the Allegro timer handler.  You don't normally need to bother
   calling this, because @link(al_exit) will do it for you. *)
  PROCEDURE al_remove_timer; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_timer';

(* Adds a function to the list of user timer handlers or, if it is already
   installed, retroactively adjusts its speed (i.e makes as though the speed
   change occurred precisely at the last tick).  The speed is given in hardware
   clock ticks, of which there are 1193181 a second. You can convert from other
   time formats to hardware clock ticks with the functions
   @link(AL_SECS_TO_TIMER), @link(AL_MSEC_TO_TIMER), @link(AL_BPS_TO_TIMER) and
   @link(AL_BPM_TO_TIMER).

   There can only be sixteen timers in use at a time, and some other parts of
   Allegro (the mouse pointer display routines, @link(al_rest), the FLI
   player, and the MIDI player) need to install handlers of their own, so you
   should avoid using too many at the same time.  If you call this routine
   without having first installed the timer module, @link(al_install_timer)
   will be called automatically.

   Your function will be called by the Allegro interrupt handler and not
   directly by the processor, so it can be a normal @code(CDECL) function.
   You should be aware, however, that it will be called in an interrupt
   context, which imposes a lot of restrictions on what you can do in it.  It
   should not use large amounts of stack, it must not make any calls to the
   operating system, use the run-time library functions, or contain any
   floating point code, and it must execute very quickly.  Don't try to do lots
   of complicated code in a timer handler:  as a general rule you should just
   set some flags and respond to these later in your main control loop.

   @returns(@true on success, or @false if there is no room to add a new user
     timer.) *)
  FUNCTION al_install_int_ex (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;

(* Installs a user timer handler, with the speed given as the number of
   milliseconds between ticks.  This is the same thing as
   @code(al_install_int_ex @(@@proc, AL_MSEC_TO_TIMER @(speed@)@)).  If you call
   this routine without having first installed the timer module,
   @link(al_install_timer) will be called automatically.  Calling again this
   routine with the same timer handler as parameter allows you to adjust its speed.

   @returns(@true on success, or @false if there is no room to add a new user
     timer.) *)
  FUNCTION al_install_int (proc: AL_SIMPLE_PROC; speed: LONGINT): BOOLEAN;

(* Removes a function from the list of user interrupt routines.
   @link(al_exit) does this automatically. *)
  PROCEDURE al_remove_int (proc: AL_SIMPLE_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_int';

(* Like @link(al_install_int_ex), but the callback routine will be passed a
   copy of the specified void pointer parameter.  To disable the handler, use
   @link(al_remove_param_int) instead of @link(al_remove_int). *)
  FUNCTION al_install_param_int_ex (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;

(* Like @link(al_install_int), but the callback routine will be passed a copy
   of the specified void pointer parameter.  To disable the handler, use
   @link(al_remove_param_int) instead of @link(al_remove_int). *)
  FUNCTION al_install_param_int (proc: AL_PARAM_PROC; speed: LONGINT): BOOLEAN;

(* Like @link(al_remove_int), but for use with timer callbacks that have
   parameter values.  If there is more than one copy of the same callback
   active at a time, it identifies which one to remove by checking the
   parameter value (so you can't have more than one copy of a handler using an
   identical parameter).  *)
  PROCEDURE al_remove_param_int (proc: AL_PARAM_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'remove_param_int';

(* This function waits for the specified number of milliseconds.

    Passing 0 as parameter will not wait, but just yield.  This can be useful
    in order to @italic("play nice") with other processes.  Other values will
    cause CPU time to be dropped on most platforms.  This will look better to
    users, and also does things like saving battery power and making fans less
    noisy.

    Note that calling this inside your active game loop is a bad idea, as you
    never know when the OS will give you the CPU back, so you could end up
    missing the vertical retrace and skipping frames.  On the other hand, on
    multitasking operating systems it is good form to give up the CPU for a
    while if you will not be using it. *)
  PROCEDURE al_rest (time: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest';

(* Like @link(al_rest), but for non-zero values continually calls the
   specified function while it is waiting for the required time to elapse.  If
   the provided `callback' parameter is @nil, this function does exactly the
   same thing as calling @code(al_rest). *)
  PROCEDURE al_rest_callback (time: LONGINT; callback: AL_PARAM_PROC); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'rest_callback';



VAR
(* If the retrace simulator is installed, this count is incremented on each
   vertical retrace; otherwise, if the refresh rate is known, the count is
   incremented at the same rate (ignoring retraces); otherwise, it is
   incremented 70 times a second.  This provides a way of controlling the speed
   of your program without installing user timer functions. *)
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

