(* @exclude
 * Defines methods and stuff for system initialization and management.
 * This is used internally by Allegro.  Only the parts used by Allegro.pas are
 * fully defined.
 *
 * Note that these methods aren't called directly. *)
  __SYSTEM_DRIVER__PTR = ^__SYSTEM_DRIVER__;
  __SYSTEM_DRIVER__ = RECORD
    id: LONGINT;
    name, desc, ascii_name: PCHAR;
    init, exit, get_executable_name, find_resource: PROCEDURE;

    set_window_title: PROCEDURE (CONST name: PCHAR); CDECL;

    set_close_button_callback, message, assert, save_console_state,
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
