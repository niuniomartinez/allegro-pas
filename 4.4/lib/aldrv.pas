UNIT aldrv;
(*<Defines structs and variables used internally by Allegro and the add-ons. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$MODE FPC}
{$ENDIF}

INTERFACE

USES
  albase, allegro;

TYPE
  __AL_DRIVER_INFO__ptr = ^__AL_DRIVER_INFO__;
(* info about a hardware driver *)
  __AL_DRIVER_INFO__ = RECORD
    id: LONGINT;              (*<integer ID *)
    driver: POINTER;          (*<the driver structure *)
    autodetect: LONGINT;      (*<set to non zero to allow autodetection *)
  END;



  __AL_SYSTEM_DRIVER__PTR = ^__AL_SYSTEM_DRIVER__;
(* Defines methods and stuff for system initialization and management.
   This is used internally by Allegro and Allegro add-ons.

   Note that these methods aren't called directly. *)
  __AL_SYSTEM_DRIVER__ = RECORD
    id: LONGINT;
    name, desc, ascii_name: PCHAR;
    init: FUNCTION: LONGINT; CDECL;
    exit: PROCEDURE; CDECL;
    get_executable_name, find_resource: PROCEDURE;

    set_window_title: PROCEDURE (CONST name: PCHAR); CDECL;

    set_close_button_callback: FUNCTION (proc: AL_SIMPLE_PROC): LONGINT; CDECL;
    _message: PROCEDURE (msg: PCHAR); CDECL;
    assert, save_console_state,
    restore_console_state, create_bitmap, created_bitmap, create_sub_bitmap,
    created_sub_bitmap, destroy_bitmap, read_hardware_palette,
    set_palette_range, get_vtable, set_display_switch_mode,
    display_switch_lock: PROCEDURE;

    desktop_color_depth: FUNCTION: LONGINT; CDECL;

    get_desktop_resolution: FUNCTION (width, height: PLONGINT): LONGINT; CDECL;

    get_gfx_safe_mode, yield_timeslice, create_mutex, destroy_mutex,
    lock_mutex, unlock_mutex: PROCEDURE;

    gfx_drivers, digi_drivers, midi_drivers, keyboard_drivers, mouse_drivers,
    joystick_drivers, timer_drivers: POINTER;
  END;



  __AL_GFX_MODE__ptr = ^__AL_GFX_MODE__;
  __AL_GFX_MODE__ = RECORD
    width, height, bpp: LONGINT;
  END;



   __AL_GFX_MODE_LIST__ptr = ^__AL_GFX_MODE_LIST__;
   __AL_GFX_MODE_LIST__ = RECORD
     num_modes: LONGINT; (*<number of gfx modes *)
     mode: __AL_GFX_MODE__ptr; (*<pointer to the actual mode list array *)
   END;



  __AL_GFX_DRIVER__PTR = ^__AL_GFX_DRIVER__;
(* Defines methods and stuff for graphics initialization and management.
   This is used internally by Allegro and Allegro add-ons.

   Note that these methods aren't called directly. *)
  __AL_GFX_DRIVER__ = RECORD
    id: LONGINT;
    name, desc, ascii_name: PCHAR;
    init: FUNCTION (w, h, v_w, v_h, color_depth: LONGINT): AL_BITMAPptr; CDECL;
    exit: PROCEDURE (b: AL_BITMAPptr); CDECL;
    scroll: FUNCTION (x, y: LONGINT): LONGINT; CDECL;
    vsync: PROCEDURE; CDECL;
    set_palette: PROCEDURE (CONST p: AL_RGBptr; from, _to, retracesync: LONGINT); CDECL;
    request_scroll: FUNCTION (x, y: LONGINT): LONGINT; CDECL;
    poll_scroll: FUNCTION: LONGINT; CDECL;
    enable_triple_buffer: PROCEDURE; CDECL;
    create_video_bitmap: FUNCTION (width, height: LONGINT): AL_BITMAPptr; CDECL;
    destroy_video_bitmap: PROCEDURE (b: AL_BITMAPptr); CDECL;
    show_video_bitmap: FUNCTION (bitmap: AL_BITMAPptr): LONGINT; CDECL;
    request_video_bitmap: FUNCTION (bitmap: AL_BITMAPptr): LONGINT; CDECL;
    create_system_bitmap: FUNCTION (width, height: LONGINT): AL_BITMAPptr; CDECL;
    destroy_system_bitmap: PROCEDURE (b: AL_BITMAPptr); CDECL;
    set_mouse_sprite: FUNCTION (sprite: AL_BITMAPptr; xfocus, yfocus: LONGINT): LONGINT; CDECL;
    show_mouse: FUNCTION (bmp: AL_BITMAPptr; x, y: LONGINT): LONGINT; CDECL;
    hide_mouse: PROCEDURE; CDECL;
    move_mouse: PROCEDURE (x, y: LONGINT); CDECL;
    drawing_mode: PROCEDURE; CDECL;
    save_video_state: PROCEDURE; CDECL;
    restore_video_state: PROCEDURE; CDECL;
    set_blender_mode: PROCEDURE (mode, r, g, b, a: LONGINT); CDECL;
    fetch_mode_list: FUNCTION: __AL_GFX_MODE_LIST__ptr; CDECL;
    w, h: LONGINT;                     (*<physical (not virtual!) screen size *)
    linear: LONGINT;                   (*<@code(NOT 0) if video memory is linear *)
    bank_size: LONGINT;                (*<bank size, in bytes *)
    bank_gran: LONGINT;                (* bank granularity, in bytes *)
    vid_mem: LONGINT;                  (* video memory size, in bytes *)
    vid_phys_base: LONGINT;            (* physical address of video memory *)
    windowed: LONGINT;                 (*<@code(NOT 0) if driver runs windowed *)
  END;



VAR
(* Pointer to the system driver. *)
  al_system_driver: __AL_SYSTEM_DRIVER__PTR;
	EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'system_driver';
(* Pointer to the active graphics driver. *)
  al_gfx_driver: __AL_GFX_DRIVER__PTR;
	EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gfx_driver';
(* Pointer to the active keyboard driver. *)
  al_keyboard_driver: POINTER;
	EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'keyboard_driver';
(* Pointer to the active mouse driver. *)
  al_mouse_driver: POINTER;
	EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'mouse_driver';



IMPLEMENTATION

END.
