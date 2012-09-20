UNIT aldrv;
(*<Defines structs and variables used internally by Allegro and the add-ons. 

  Note that some parts of this unit are platform-dependent so they aren't
  available in all platforms. *)

{$INCLUDE allegro.cfg }

INTERFACE

USES
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  albase;

TYPE
  __AL_SYSTEM_DRIVER__ptr = ^__AL_SYSTEM_DRIVER__;
(* Defines methods and stuff for system initialization and management.
   This is used internally by Allegro and Allegro add-ons.

   REMEMBER that these methods should NOT be called directly. *)
  __AL_SYSTEM_DRIVER__ = RECORD
    id: AL_INT;
    name, desc, ascii_name: AL_STRptr;
    init: AL_SIMPLE_FUNC;
    exit: AL_SIMPLE_PROC;
  (* get_executable_name must NOT be used. *)
    get_executable_name, find_resource: AL_POINTER;

    set_window_title: PROCEDURE (CONST name: AL_STRptr); CDECL;

    set_close_button_callback: FUNCTION (proc: AL_SIMPLE_PROC): AL_INT; CDECL;
    _message: PROCEDURE (CONST msg: AL_STRptr); CDECL;
    assert, save_console_state,
    restore_console_state, create_bitmap, created_bitmap, create_sub_bitmap,
    created_sub_bitmap, destroy_bitmap, read_hardware_palette,
    set_palette_range, get_vtable, set_display_switch_mode,
    display_switch_lock: AL_POINTER;

    desktop_color_depth: AL_SIMPLE_FUNC;

    get_desktop_resolution: FUNCTION (width, height: AL_INTptr): AL_INT; CDECL;

    get_gfx_safe_mode, yield_timeslice, create_mutex, destroy_mutex,
    lock_mutex, unlock_mutex: AL_POINTER;

    gfx_drivers, digi_drivers, midi_drivers, keyboard_drivers, mouse_drivers,
    joystick_drivers, timer_drivers: AL_POINTER;
  END;

  VAR
  (* Pointer to the system driver. *)
    al_system_driver: __AL_SYSTEM_DRIVER__ptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'system_driver';



{$IFDEF MSWINDOWS}
TYPE
  __AL_WIN_CREATE_PROC__ = FUNCTION (p: WNDPROC): HWND; CDECL;
  __AL_WIN_MESSAGE_PROC__ = FUNCTION (w: HWND; m: UINT; wp: WPARAM; lp: LPARAM; p: PLONGINT): LONGINT; CDECL;

(* Returns the window handler. *)
  FUNCTION al_win_get_window: HWND; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_get_window';

(* Sets the window handler. *)
  PROCEDURE al_win_set_window (wnd: HWND); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_window';

(* Set the window creation procedure. *)
  PROCEDURE al_win_set_wnd_create_proc (proc: __AL_WIN_CREATE_PROC__); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_wnd_create_proc';

  PROCEDURE al_win_set_msg_pre_proc (proc: __AL_WIN_MESSAGE_PROC__); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_msg_pre_proc';
{$ENDIF}

IMPLEMENTATION

END.
