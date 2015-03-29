UNIT alWin;
(*<Windows specific stuff.

  Note that this unit compiles @bold(only) on Windows. *)

{$INCLUDE allegro.cfg }

INTERFACE

  USES
    windows;

(*****************************************************************************
 * winalleg.h
 *     Windows header file for the Allegro library.
 *
 *     It must be included by Allegro programs that need to use
 *     direct Win32 API calls and by Win32 programs that need to
 *     interface with Allegro.
 *)
  TYPE
(**)
    __AL_WIN_CREATE_PROC__ = FUNCTION (p: WNDPROC): HWND; CDECL;
    __AL_WIN_MESSAGE_PROC__ = FUNCTION (w: HWND; m: UINT; wp: WPARAM; lp: LPARAM; p: AL_INTptr): AL_INT; CDECL;

(* Returns the window handler. *)
  FUNCTION al_win_get_window: HWND;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_get_window';

(* Sets the window handler. *)
  PROCEDURE al_win_set_window (wnd: HWND);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_window';

(* Set the window creation procedure. *)
  PROCEDURE al_win_set_wnd_create_proc (proc: __AL_WIN_CREATE_PROC__);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_wnd_create_proc';

  PROCEDURE al_win_set_msg_pre_proc (proc: __AL_WIN_MESSAGE_PROC__);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_msg_pre_proc';

IMPLEMENTATION

END.
