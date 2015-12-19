UNIT alDrv;
(*<Defines structs and variables used internally by Allegro and the add-ons.

  Note that this unit is @bold(FOR INTERNAL USE ONLY).  You should @bold(NOT)
  use it in any of your projects.
*)
{$INCLUDE allegro.cfg }

INTERFACE

  USES
    albase;

(*****************************************************************************
 * system.h
 *     System level: initialization, cleanup, etc.
 *)

  TYPE
  (* Pointer to @link(__AL_SYSTEM_DRIVER__). *)
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

      set_window_title: PROCEDURE (CONST name: AL_STR); CDECL;

      set_close_button_callback: FUNCTION (proc: AL_SIMPLE_PROC): AL_INT; CDECL;
      _message: PROCEDURE (CONST msg: AL_STR); CDECL;
      assert, save_console_state,
      restore_console_state, create_bitmap, created_bitmap, create_sub_bitmap,
      created_sub_bitmap, destroy_bitmap, read_hardware_palette,
      set_palette_range, get_vtable, set_display_switch_mode,
      display_switch_lock: AL_POINTER;

      desktop_color_depth: AL_SIMPLE_FUNC;

      get_desktop_resolution: FUNCTION (OUT width, height: AL_INT): AL_BOOL; CDECL;

      get_gfx_safe_mode, yield_timeslice, create_mutex, destroy_mutex,
      lock_mutex, unlock_mutex: AL_POINTER;

      gfx_drivers, digi_drivers, midi_drivers, keyboard_drivers, mouse_drivers,
      joystick_drivers, timer_drivers: AL_POINTER;
    END;

  VAR
  (* Pointer to the system driver. *)
    al_system_driver: __AL_SYSTEM_DRIVER__ptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'system_driver';



(*****************************************************************************
 * gfx.h
 *     Graphics driver.
 *)

  TYPE
  (* Pointer to __AL_GFX_DRIVER__. *)
    __AL_GFX_DRIVER__ptr = ^__AL_GFX_DRIVER__;
  (* Creates and manages the screen bitmap. *)
    __AL_GFX_DRIVER__ = RECORD
      id: AL_INT;
      name, desc, ascii_name: AL_STRptr;
      init, exit, scroll, vsync, set_palette, request_scroll, poll_scroll,
      enable_triple_buffer, create_video_bitmap, destroy_video_bitmap,
      show_video_bitmap, request_video_bitmap, create_system_bitmap,
      destroy_system_bitmap, set_mouse_sprite, show_mouse, hide_mouse,
      move_mouse, drawing_mode, save_video_state, restore_video_state,
      set_blender_mode, fetch_mode_list: AL_POINTER;
      w, h: AL_INT;                     (*<physical (not virtual!) screen size *)
      linear: AL_BOOL;                  (*<true if video memory is linear *)
      bank_size: AL_LONG;               (* bank size, in bytes *)
      bank_gran: AL_LONG;               (* bank granularity, in bytes *)
      vid_mem: AL_LONG;                 (* video memory size, in bytes *)
      vid_phys_base: AL_LONG;           (* physical address of video memory *)
      windowed: AL_BOOL;                (* true if driver runs windowed *)
    END;

  VAR
  (* Pointer to the current graphics driver. *)
    al_gfx_driver: __AL_GFX_DRIVER__ptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gfx_driver';

IMPLEMENTATION

END.
