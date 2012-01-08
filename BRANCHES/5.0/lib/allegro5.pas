UNIT Allegro5;
(*         ______   ___    ___
 *        /\  _  \ /\_ \  /\_ \
 *        \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___
 *         \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\
 *          \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \
 *           \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/
 *            \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/
 *                                           /\____/
 *                                           \_/__/
 *
 * Wrapper of the Allegro 5 core library.
 *
 *      See readme.txt for copyright information.
 *)

INTERFACE

{$include allegro.cfg}

(* The code is distributed in sections.  Each section wraps with a header file (approx.). *)

(******************************************************************************
 * base.h
 *      Defines basic stuff needed by pretty much everything else.
 *
 *      By Shawn Hargreaves.
 **********)

  CONST
  (* Major version of Allegro. *)
    ALLEGRO_VERSION      =   5;
  (* Minor version of Allegro. *)
    ALLEGRO_SUB_VERSION  =   0;
  (* Revision number of Allegro. *)
    ALLEGRO_WIP_VERSION  =   5;
  (* Not sure we need it, but since ALLEGRO_VERSION_STR contains it:
     0 = SVN
     1 = first release
     2... = hotfixes?

     Note x.y.z (= x.y.z.0) has release number 1, and x.y.z.1 has release
     number 2, just to confuse you.
  *)
    ALLEGRO_RELEASE_NUMBER = 0;
  (* Packs version number in a simple LONGWORD number.
   *)
    ALLEGRO_VERSION_INT  = (
	   (ALLEGRO_VERSION SHL 24)
	OR (ALLEGRO_SUB_VERSION SHL 16)
	OR (ALLEGRO_WIP_VERSION SHL  8)
	OR  ALLEGRO_RELEASE_NUMBER
    );

    ALLEGRO_PI = 3.14159265358979323846;


  TYPE
  (* Description of user main function for al_run_main. *)
    ALLEGRO_USER_MAIN = FUNCTION (c: LONGINT; v: POINTER): LONGINT; CDECL;

(* Returns the compiled version of the Allegro library. *)
  FUNCTION al_get_allegro_version: LONGWORD; CDECL;

  FUNCTION al_run_main (argc: LONGINT; argv: POINTER; user_main: ALLEGRO_USER_MAIN): LONGINT; CDECL;

(* This function can be used to create a packed 32 bit integer from 8 bit
   characters, on both 32 and 64 bit machines.  These can be used for various
   things, like custom datafile objects or system IDs. Example:

@longcode(#
VAR
  OSTYPE_LINUX: LONGINT;
BEGIN
  OSTYPE_LINUX := AL_ID('TUX ');
END;
  #) *)
  FUNCTION AL_ID (str: SHORTSTRING): LONGINT;



(******************************************************************************
 * config.h
 ************)

{ TODO:
  At the moment I'll not include this header.  Object Pascal defines the
  TStrings class that implements a similar functionality. }



(******************************************************************************
 * system.h *
 ************)

  TYPE
    ALLEGRO_SYSTEMptr = POINTER;

  (* "Forward" types needed below. *)
    ALLEGRO_EVENT_SOURCEptr = ^ALLEGRO_EVENT_SOURCE;
    ALLEGRO_EVENT_SOURCE = RECORD
      __pad : ARRAY [0..31] OF LONGINT;
    END;



  FUNCTION al_init: BOOLEAN;

  FUNCTION al_install_system (version: LONGWORD; atexit_ptr: POINTER): BOOLEAN; CDECL;
  PROCEDURE al_uninstall_system; CDECL;
  FUNCTION al_is_system_installed: BOOLEAN; CDECL;
  FUNCTION al_get_system_driver: ALLEGRO_SYSTEMptr; CDECL;

{ Modern Pascal compilers (i.e. Free Pascal) has functions and methods to get
  the path of system directories and the application name, so Allegro's
  functions for this aren't included. }

  FUNCTION al_inhibit_screensaver (inhibit: BOOLEAN): BOOLEAN; CDECL;



(******************************************************************************
 * color.h *
 ***********)

(* Describes a color in a device independant way. Use @link(al_map_rgb) et al. and @link(al_unmap_rgb) et al. to translate from and to various color representations. *)
  TYPE
    ALLEGRO_COLOR = RECORD
      r, g, b, a: SINGLE;
    END;



(******************************************************************************
 * bitmap.h *
 ************)

  TYPE
  (* Abstract type representing a bitmap (2D image). *)
    ALLEGRO_BITMAPptr = POINTER;

  (* ALLEGRO_PIXEL_FORMAT *)
    ALLEGRO_PIXEL_FORMAT = (
      ALLEGRO_PIXEL_FORMAT_ANY = 0,
      ALLEGRO_PIXEL_FORMAT_ANY_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_WITH_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_15_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_16_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_16_WITH_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_24_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_32_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ARGB_8888,
      ALLEGRO_PIXEL_FORMAT_RGBA_8888,
      ALLEGRO_PIXEL_FORMAT_ARGB_4444,
      ALLEGRO_PIXEL_FORMAT_RGB_888,	{ 24 bit format }
      ALLEGRO_PIXEL_FORMAT_RGB_565,
      ALLEGRO_PIXEL_FORMAT_RGB_555,
      ALLEGRO_PIXEL_FORMAT_RGBA_5551,
      ALLEGRO_PIXEL_FORMAT_ARGB_1555,
      ALLEGRO_PIXEL_FORMAT_ABGR_8888,
      ALLEGRO_PIXEL_FORMAT_XBGR_8888,
      ALLEGRO_PIXEL_FORMAT_BGR_888,	{ 24 bit format }
      ALLEGRO_PIXEL_FORMAT_BGR_565,
      ALLEGRO_PIXEL_FORMAT_BGR_555,
      ALLEGRO_PIXEL_FORMAT_RGBX_8888,
      ALLEGRO_PIXEL_FORMAT_XRGB_8888,
      ALLEGRO_PIXEL_FORMAT_ABGR_F32,
      ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE,
      ALLEGRO_PIXEL_FORMAT_RGBA_4444,
      ALLEGRO_NUM_PIXEL_FORMATS
    );

  CONST
  (* Bitmap flags *)
    ALLEGRO_MEMORY_BITMAP            = $0001;
    ALLEGRO_KEEP_BITMAP_FORMAT       = $0002;
    ALLEGRO_FORCE_LOCKING            = $0004;
    ALLEGRO_NO_PRESERVE_TEXTURE      = $0008;
    ALLEGRO_ALPHA_TEST               = $0010;
    _ALLEGRO_INTERNAL_OPENGL         = $0020;
    ALLEGRO_MIN_LINEAR               = $0040;
    ALLEGRO_MAG_LINEAR               = $0080;
    ALLEGRO_MIPMAP                   = $0100;
    ALLEGRO_NO_PREMULTIPLIED_ALPHA   = $0200;
    ALLEGRO_VIDEO_BITMAP             = $0400;



  (* Flags for the blitting functions *)
    ALLEGRO_FLIP_HORIZONTAL = $00001;
    ALLEGRO_FLIP_VERTICAL   = $00002;



  (* Locking flags *)
    ALLEGRO_LOCK_READWRITE  = 0;
    ALLEGRO_LOCK_READONLY   = 1;
    ALLEGRO_LOCK_WRITEONLY  = 2;

  TYPE
  (* Blending modes *)
    ALLEGRO_BLEND_MODE = (
      ALLEGRO_ZERO = 0,
      ALLEGRO_ONE = 1,
      ALLEGRO_ALPHA = 2,
      ALLEGRO_INVERSE_ALPHA = 3
    );



    ALLEGRO_BLEND_OPERATIONS = (
      ALLEGRO_ADD = 0,
      ALLEGRO_SRC_MINUS_DEST = 1,
      ALLEGRO_DEST_MINUS_SRC = 2,
      ALLEGRO_NUM_BLEND_OPERATIONS
    );



    ALLEGRO_LOCKED_REGIONptr = ^ALLEGRO_LOCKED_REGION;
    ALLEGRO_LOCKED_REGION = RECORD
      data: POINTER;
      format, pitch, pixel_size: LONGINT;
    END;



  PROCEDURE al_set_new_bitmap_format (format: ALLEGRO_PIXEL_FORMAT); CDECL;
  PROCEDURE al_set_new_bitmap_flags (flags: LONGINT); CDECL;
  FUNCTION al_get_new_bitmap_format: ALLEGRO_PIXEL_FORMAT; CDECL;
  FUNCTION al_get_new_bitmap_flags: LONGINT; CDECL;
  PROCEDURE al_add_new_bitmap_flag (flag: LONGINT); CDECL;

  FUNCTION al_get_bitmap_width (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;
  FUNCTION al_get_bitmap_height (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;
  FUNCTION al_get_bitmap_format (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;
  FUNCTION al_get_bitmap_flags (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;

  FUNCTION al_create_bitmap (w, h: LONGINT): ALLEGRO_BITMAPptr; CDECL;
  PROCEDURE al_destroy_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;

(* Blitting *)
  PROCEDURE al_draw_bitmap (bitmap: ALLEGRO_BITMAPptr; dx, dy: SINGLE; flags: LONGINT); CDECL;
  PROCEDURE al_draw_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy: SINGLE; flags: LONGINT); CDECL;
  PROCEDURE al_draw_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy, dw, dh: SINGLE; flags: LONGINT); CDECL;
  PROCEDURE al_draw_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, angle: SINGLE; flags: LONGINT); CDECL;
  PROCEDURE al_draw_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, xscale, yscale, angle: SINGLE; flags: LONGINT); CDECL;

(* Tinted blitting *)
  PROCEDURE al_draw_tinted_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; dx, dy: SINGLE; flags: LONGINT); CDECL;
  PROCEDURE al_draw_tinted_bitmap_region (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy: SINGLE; flags: LONGINT); CDECL;
  PROCEDURE al_draw_tinted_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy, dw, dh: SINGLE; flags: LONGINT); CDECL;
  PROCEDURE al_draw_tinted_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, angle: SINGLE; flags: LONGINT); CDECL;
  PROCEDURE al_draw_tinted_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, xscale, yscale, angle: SINGLE; flags: LONGINT); CDECL;

(* Locking *)
  FUNCTION al_lock_bitmap (bitmap: ALLEGRO_BITMAPptr; format: ALLEGRO_PIXEL_FORMAT; flags: LONGINT): ALLEGRO_LOCKED_REGIONptr; CDECL;
  FUNCTION al_lock_bitmap (bitmap: ALLEGRO_BITMAPptr; x, y, width, height: LONGINT; format: ALLEGRO_PIXEL_FORMAT; flags: LONGINT): ALLEGRO_LOCKED_REGIONptr; CDECL;
  PROCEDURE al_unlock_bitmap (bitmap: ALLEGRO_BITMAPptr); CDECL;

  PROCEDURE al_put_pixel (x, y: LONGINT; color: ALLEGRO_COLOR); CDECL;
  PROCEDURE al_put_blended_pixel (x, y: LONGINT; color: ALLEGRO_COLOR); CDECL;
  FUNCTION al_get_pixel (bitmap: ALLEGRO_BITMAPptr; x, y: LONGINT): ALLEGRO_BITMAPptr; CDECL;
  FUNCTION al_get_pixel_size (format: ALLEGRO_PIXEL_FORMAT): LONGINT;

(* Pixel mapping *)
  FUNCTION al_map_rgb (r, g, b: BYTE): ALLEGRO_COLOR; CDECL;
  FUNCTION al_map_rgba (r, g, b, a: BYTE): ALLEGRO_COLOR; CDECL;
  FUNCTION al_map_rgb_f (r, g, b: SINGLE): ALLEGRO_COLOR; CDECL;
  FUNCTION al_map_rgba_f (r, g, b, a: SINGLE): ALLEGRO_COLOR; CDECL;

(* Pixel unmapping *)
  PROCEDURE al_unmap_rgb (color: ALLEGRO_COLOR; VAR r, g, b: BYTE); CDECL;
  PROCEDURE al_unmap_rgba (color: ALLEGRO_COLOR; VAR r, g, b, a: BYTE); CDECL;
  PROCEDURE al_unmap_rgb_f (color: ALLEGRO_COLOR; VAR r, g, b: SINGLE); CDECL;
  PROCEDURE al_unmap_rgba_f (color: ALLEGRO_COLOR; VAR r, g, b, a: SINGLE); CDECL;
  FUNCTION al_get_pixel_format_bits (format: LONGINT): LONGINT; CDECL;

(* Masking *)
  PROCEDURE al_convert_mask_to_alpha (bitmap: ALLEGRO_BITMAPptr; mask_color: ALLEGRO_COLOR); CDECL;

(* Clipping *)
  PROCEDURE al_set_clipping_rectangle (x, y, width, height: LONGINT); CDECL;
  PROCEDURE al_get_clipping_rectangle (VAR x, y, w, h: LONGINT); CDECL;

(* Sub bitmaps *)
  FUNCTION al_create_sub_bitmap (parent: ALLEGRO_BITMAPptr; x, y, w, h: LONGINT): ALLEGRO_BITMAPptr; CDECL;
  FUNCTION al_is_sub_bitmap (bitmap: ALLEGRO_BITMAPptr): BOOLEAN; CDECL;

(* Miscellaneous *)
  FUNCTION al_clone_bitmap (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_BITMAPptr; CDECL;
  FUNCTION al_is_bitmap_locked (bitmap: ALLEGRO_BITMAPptr): BOOLEAN; CDECL;

(* Blending *)
  PROCEDURE al_set_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE); CDECL;
  PROCEDURE al_get_blender (VAR op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE); CDECL;
  PROCEDURE al_set_separate_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE;
				      alpha_op: ALLEGRO_BLEND_OPERATIONS; alpha_source, alpha_dest: ALLEGRO_BLEND_MODE); CDECL;
  PROCEDURE al_get_separate_blender (VAR op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE;
				      alpha_op: ALLEGRO_BLEND_OPERATIONS; alpha_source, alpha_dest: ALLEGRO_BLEND_MODE); CDECL;

  PROCEDURE _al_put_pixel (bitmap: ALLEGRO_BITMAPptr; x,y: LONGINT; color: ALLEGRO_COLOR); CDECL;



(******************************************************************************
 * file.h
 ***************)

{ TODO:
  Actually, this header is needed by Allegro to define new loaders and savers,
  but at the moment I'll not add it. }



(******************************************************************************
 * bitmap_io.h *
 ***************)

{ TODO: Some functions need the file.h definitions. }

  FUNCTION al_load_bitmap (CONST filename: PCHAR): ALLEGRO_BITMAPptr; CDECL;
  FUNCTION al_save_bitmap (CONST filename: PCHAR): ALLEGRO_BITMAPptr; CDECL;



(******************************************************************************
 * display.h *
 *************)


  CONST
    ALLEGRO_DEFAULT_DISPLAY_ADAPTER = -1;
  (* Possible bit combinations for the flags parameter of al_set_new_display_flags. *)
    ALLEGRO_DEFAULT                     = 0 SHL 0;
    ALLEGRO_WINDOWED                    = 1 SHL 0;
    ALLEGRO_FULLSCREEN                  = 1 SHL 1;
    ALLEGRO_OPENGL                      = 1 SHL 2;
    ALLEGRO_DIRECT3D_INTERNAL           = 1 SHL 3;
    ALLEGRO_RESIZABLE                   = 1 SHL 4;
    ALLEGRO_NOFRAME                     = 1 SHL 5;
    ALLEGRO_GENERATE_EXPOSE_EVENTS      = 1 SHL 6;
    ALLEGRO_OPENGL_3_0                  = 1 SHL 7;
    ALLEGRO_OPENGL_FORWARD_COMPATIBLE   = 1 SHL 8;
    ALLEGRO_FULLSCREEN_WINDOW           = 1 SHL 9;
    ALLEGRO_MINIMIZED                   = 1 SHL 10;

  TYPE
  (* Possible parameters for al_set_display_option.
   * Make sure to update ALLEGRO_EXTRA_DISPLAY_SETTINGS if you modify
   * anything here.
   *)
    ALLEGRO_DISPLAY_OPTIONS = (
      ALLEGRO_RED_SIZE,
      ALLEGRO_GREEN_SIZE,
      ALLEGRO_BLUE_SIZE,
      ALLEGRO_ALPHA_SIZE,
      ALLEGRO_RED_SHIFT,
      ALLEGRO_GREEN_SHIFT,
      ALLEGRO_BLUE_SHIFT,
      ALLEGRO_ALPHA_SHIFT,
      ALLEGRO_ACC_RED_SIZE,
      ALLEGRO_ACC_GREEN_SIZE,
      ALLEGRO_ACC_BLUE_SIZE,
      ALLEGRO_ACC_ALPHA_SIZE,
      ALLEGRO_STEREO,
      ALLEGRO_AUX_BUFFERS,
      ALLEGRO_COLOR_SIZE,
      ALLEGRO_DEPTH_SIZE,
      ALLEGRO_STENCIL_SIZE,
      ALLEGRO_SAMPLE_BUFFERS,
      ALLEGRO_SAMPLES,
      ALLEGRO_RENDER_METHOD,
      ALLEGRO_FLOAT_COLOR,
      ALLEGRO_FLOAT_DEPTH,
      ALLEGRO_SINGLE_BUFFER,
      ALLEGRO_SWAP_METHOD,
      ALLEGRO_COMPATIBLE_DISPLAY,
      ALLEGRO_UPDATE_DISPLAY_REGION,
      ALLEGRO_VSYNC,
      ALLEGRO_MAX_BITMAP_SIZE,
      ALLEGRO_SUPPORT_NPOT_BITMAP,
      ALLEGRO_CAN_DRAW_INTO_BITMAP,
      ALLEGRO_SUPPORT_SEPARATE_ALPHA,
      ALLEGRO_DISPLAY_OPTIONS_COUNT
    );

  CONST
    ALLEGRO_DONTCARE = 0;
    ALLEGRO_REQUIRE = 1;
    ALLEGRO_SUGGEST = 2;

  TYPE
    ALLEGRO_DISPLAY_ORIENTATION = (
      ALLEGRO_DISPLAY_ORIENTATION_0_DEGREES,
      ALLEGRO_DISPLAY_ORIENTATION_90_DEGREES,
      ALLEGRO_DISPLAY_ORIENTATION_180_DEGREES,
      ALLEGRO_DISPLAY_ORIENTATION_270_DEGREES,
      ALLEGRO_DISPLAY_ORIENTATION_FACE_UP,
      ALLEGRO_DISPLAY_ORIENTATION_FACE_DOWN
    );



    ALLEGRO_DISPLAYptr = POINTER;



    ALLEGRO_DISPLAY_MODEptr = ^ALLEGRO_DISPLAY_MODE;
    ALLEGRO_DISPLAY_MODE = RECORD
      width, height, format, refresh_rate: LONGINT;
    END;



    ALLEGRO_MONITOR_INFO = RECORD
      x1, y1, x2, y2: LONGINT;
    END;

  PROCEDURE al_set_new_display_refresh_rate (refresh_rate: LONGINT); CDECL;
  PROCEDURE al_set_new_display_flags (flags: LONGINT); CDECL;
  FUNCTION al_get_new_display_refresh_rate: LONGINT; CDECL;
  FUNCTION al_get_new_display_flags: LONGINT; CDECL;

  FUNCTION al_get_display_width (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  FUNCTION al_get_display_height (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  FUNCTION al_get_display_format (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  FUNCTION al_get_display_refresh_rate (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  FUNCTION al_get_display_flags (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  FUNCTION al_toggle_display_flag (display: ALLEGRO_DISPLAYptr; flag: LONGINT; onoff: BOOLEAN): BOOLEAN; CDECL;

  FUNCTION al_create_display (w, h: LONGINT): ALLEGRO_DISPLAYptr; CDECL;
  PROCEDURE al_destroy_display (display: ALLEGRO_DISPLAYptr); CDECL;
  FUNCTION al_get_current_display: ALLEGRO_DISPLAYptr; CDECL;
  PROCEDURE al_set_target_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;
  PROCEDURE al_set_target_backbuffer (display: ALLEGRO_DISPLAYptr); CDECL;
  FUNCTION al_get_backbuffer (display: ALLEGRO_DISPLAYptr): ALLEGRO_BITMAPptr; CDECL;
  FUNCTION al_get_target_bitmap: ALLEGRO_BITMAPptr; CDECL;

  FUNCTION al_acknowledge_resize (display: ALLEGRO_DISPLAYptr): BOOLEAN; CDECL;
  FUNCTION al_resize_display (display: ALLEGRO_DISPLAYptr; width, height: LONGINT): BOOLEAN; CDECL;
  PROCEDURE al_flip_display; CDECL;
  PROCEDURE al_update_display_region (x, y, Width, height: LONGINT); CDECL;
  FUNCTION al_is_compatible_bitmap (bitmap: ALLEGRO_BITMAPptr): BOOLEAN; CDECL;

  FUNCTION al_get_num_display_modes: LONGINT; CDECL;
  FUNCTION al_get_display_mode (index: LONGINT; mode: ALLEGRO_DISPLAY_MODEptr): ALLEGRO_DISPLAY_MODEptr;

  FUNCTION al_wait_for_vsync: BOOLEAN; CDECL;

  FUNCTION al_get_display_event_source (display: ALLEGRO_DISPLAYptr): ALLEGRO_EVENT_SOURCEptr; CDECL;

(* Primitives *)
  PROCEDURE al_clear_to_color (color: ALLEGRO_COLOR); CDECL;
  PROCEDURE al_draw_pixel (x, y: SINGLE; color: ALLEGRO_COLOR); CDECL;

  PROCEDURE al_set_display_icon (display: ALLEGRO_DISPLAYptr; icon: ALLEGRO_BITMAPptr); CDECL;

(* Stuff for multihead/window management *)
  FUNCTION al_get_num_video_adapters: LONGINT; CDECL;
  FUNCTION al_get_monitor_info (adapter: LONGINT; VAR info: ALLEGRO_MONITOR_INFO): BOOLEAN; CDECL;
  FUNCTION al_get_new_display_adapter: LONGINT; CDECL;
  PROCEDURE al_set_new_display_adapter (adapter: LONGINT); CDECL;
  PROCEDURE al_set_new_window_position (x, y: LONGINT); CDECL;
  PROCEDURE al_get_new_window_position (VAR x, y: LONGINT); CDECL;
  PROCEDURE al_set_window_position (display: ALLEGRO_DISPLAYptr; x, y: LONGINT); CDECL;
  PROCEDURE al_get_window_position (display: ALLEGRO_DISPLAYptr; VAR x, y: LONGINT); CDECL;

  PROCEDURE al_set_window_title (display: ALLEGRO_DISPLAYptr; CONST title: PCHAR);CDECL;

  PROCEDURE al_set_new_display_option (option: ALLEGRO_DISPLAY_OPTIONS; value, importance: LONGINT); CDECL;
  FUNCTION al_get_new_display_option (display: ALLEGRO_DISPLAYptr; option: ALLEGRO_DISPLAY_OPTIONS): LONGINT; CDECL;
  PROCEDURE al_reset_new_display_options; CDECL;
  FUNCTION al_get_display_option (display: ALLEGRO_DISPLAYptr; option: ALLEGRO_DISPLAY_OPTIONS): LONGINT; CDECL;

(*Deferred drawing*)
  PROCEDURE al_hold_bitmap_drawing (hold: BOOLEAN); CDECL;
  FUNCTION al_is_bitmap_drawing_held: BOOLEAN; CDECL;



(******************************************************************************
 * keycodes.h *
 **************)

{$include keycodes.inc}



(******************************************************************************
 * keyboard.h *
 **************)

  TYPE
    ALLEGRO_KEYBOARDptr = POINTER;


    ALLEGRO_KEYBOARD_STATEptr = ^ALLEGRO_KEYBOARD_STATE;
    ALLEGRO_KEYBOARD_STATE = RECORD
      display: ALLEGRO_DISPLAYptr;
      __key_down__internal__: ARRAY [0..((ALLEGRO_KEY_MAX * 31) DIV 32) - 1] OF LONGWORD;
    END;

  FUNCTION al_is_keyboard_installed: BOOLEAN; CDECL;
  FUNCTION al_install_keyboard: BOOLEAN; CDECL;
  PROCEDURE al_uninstall_keyboard; CDECL;

  FUNCTION al_set_keyboard_leds (leds: LONGINT): BOOLEAN; CDECL;

  FUNCTION al_keycode_to_name (keycode: LONGINT): PCHAR; CDECL;

  PROCEDURE al_get_keyboard_state (ret_state: ALLEGRO_KEYBOARD_STATEptr); CDECL;
  FUNCTION al_key_down (CONST state: ALLEGRO_KEYBOARD_STATEptr; keycode: LONGINT): BOOLEAN; CDECL;

(* Retrieves the keyboard event source.
   @returns(@nil if the keyboard subsystem was not installed.) *)
  FUNCTION al_get_keyboard_event_source: ALLEGRO_EVENT_SOURCEptr; CDECL;



(******************************************************************************
 * joystick.h *
 ************)

  CONST
(* internal values *)
    _AL_MAX_JOYSTICK_AXES    = 3;
    _AL_MAX_JOYSTICK_STICKS  = 8;
    _AL_MAX_JOYSTICK_BUTTONS = 32;

  TYPE
    ALLEGRO_JOYSTICKptr = POINTER;



    ALLEGRO_JOYSTICK_STATE = RECORD
      stick: ARRAY [0.._AL_MAX_JOYSTICK_STICKS - 1] OF RECORD
	axis: ARRAY [0.._AL_MAX_JOYSTICK_AXES - 1] OF SINGLE; { -1.0 to 1.0 }
      END;
      button: ARRAY [0.._AL_MAX_JOYSTICK_BUTTONS - 1] OF LONGINT; { 0 to 32767 }
    END;



    ALLEGRO_JOYFLAGS = (
      ALLEGRO_JOYFLAG_DIGITAL  = $01,
      ALLEGRO_JOYFLAG_ANALOGUE = $02
    );

  FUNCTION al_install_joystick     : BOOLEAN; CDECL;
  PROCEDURE al_uninstall_joystick           ; CDECL;
  FUNCTION al_is_joystick_installed: BOOLEAN; CDECL;
  FUNCTION al_reconfigure_joysticks: BOOLEAN; CDECL;

  FUNCTION al_get_num_joysticks                  : LONGINT            ; CDECL;
  FUNCTION al_get_joystick        (joyn: LONGINT): ALLEGRO_JOYSTICKptr; CDECL;
  PROCEDURE al_release_joystick   (j: ALLEGRO_JOYSTICKptr)            ; CDECL;
  FUNCTION al_get_joystick_active (j: ALLEGRO_JOYSTICKptr): BOOLEAN   ; CDECL;
  FUNCTION al_get_joystick_name   (j: ALLEGRO_JOYSTICKptr): PCHAR     ; CDECL;

  FUNCTION al_get_joystick_num_sticks  (j: ALLEGRO_JOYSTICKptr)                : LONGINT; CDECL;
  FUNCTION al_get_joystick_stick_flags (j: ALLEGRO_JOYSTICKptr; stick: LONGINT): LONGINT; CDECL;
  FUNCTION al_get_joystick_stick_name  (j: ALLEGRO_JOYSTICKptr; stick: LONGINT): PCHAR; CDECL;

  FUNCTION al_get_joystick_num_axes  (j: ALLEGRO_JOYSTICKptr; stick: LONGINT)      : LONGINT; CDECL;
  FUNCTION al_get_joystick_axis_name (j: ALLEGRO_JOYSTICKptr; stick, axis: LONGINT): PCHAR; CDECL;

  FUNCTION al_get_joystick_num_buttons (j: ALLEGRO_JOYSTICKptr)                  : LONGINT; CDECL;
  FUNCTION al_get_joystick_button_name (j: ALLEGRO_JOYSTICKptr; buttonn: LONGINT): PCHAR; CDECL;

  PROCEDURE al_get_joystick_state (j: ALLEGRO_JOYSTICKptr; VAR ret_state: ALLEGRO_JOYSTICK_STATE); CDECL;

  FUNCTION al_get_joystick_event_source: ALLEGRO_EVENT_SOURCEptr; CDECL;



(******************************************************************************
 * mouse.h *
 ***********)

  CONST
  (* Allow up to four extra axes for future expansion. *)
    ALLEGRO_MOUSE_MAX_EXTRA_AXES = 4;

  TYPE
    ALLEGRO_MOUSEptr = POINTER;

  (* Type: ALLEGRO_MOUSE_STATE *)
    ALLEGRO_MOUSE_STATEptr = ^ALLEGRO_MOUSE_STATE;
    ALLEGRO_MOUSE_STATE = RECORD
    (* (x, y) Primary mouse position
     * (z) Mouse wheel position (1D 'wheel'), or,
     * (w, z) Mouse wheel position (2D 'ball')
     * display - the display the mouse is on (coordinates are relative to this)
     * pressure - the pressure appleid to the mouse (for stylus/tablet)
     *)
      x, y, z, w: LONGINT;
      more_axes: ARRAY [0..(ALLEGRO_MOUSE_MAX_EXTRA_AXES - 1)] OF LONGINT;
      buttons: LONGINT;
      pressure: SINGLE;
      display: ALLEGRO_DISPLAYptr;
    END;


  (* Mouse cursors *)
    ALLEGRO_MOUSE_CURSORptr = POINTER;

    ALLEGRO_SYSTEM_MOUSE_CURSOR = (
      ALLEGRO_SYSTEM_MOUSE_CURSOR_NONE        =  0,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_DEFAULT     =  1,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_ARROW       =  2,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_BUSY        =  3,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_QUESTION    =  4,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_EDIT        =  5,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_MOVE        =  6,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_N    =  7,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_W    =  8,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_S    =  9,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_E    = 10,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_NW   = 11,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_SW   = 12,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_SE   = 13,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_NE   = 14,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_PROGRESS    = 15,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_PRECISION   = 16,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_LINK        = 17,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_ALT_SELECT  = 18,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_UNAVAILABLE = 19,
      ALLEGRO_NUM_SYSTEM_MOUSE_CURSORS
    );

  FUNCTION al_is_mouse_installed: BOOLEAN; CDECL;
  FUNCTION al_install_mouse: BOOLEAN; CDECL;
  PROCEDURE al_uninstall_mouse; CDECL;
  FUNCTION al_get_mouse_num_buttons: LONGWORD; CDECL;
  FUNCTION al_get_mouse_num_axes: LONGWORD; CDECL;
  FUNCTION al_set_mouse_xy (display: ALLEGRO_DISPLAYptr; x, y: LONGINT): BOOLEAN; CDECL;
  FUNCTION al_set_mouse_z (z: LONGINT): BOOLEAN; CDECL;
  FUNCTION al_set_mouse_w (w: LONGINT): BOOLEAN; CDECL;
  FUNCTION al_set_mouse_axis (axis, value: LONGINT): BOOLEAN; CDECL;
  PROCEDURE al_get_mouse_state (ret_state: ALLEGRO_MOUSE_STATEptr); CDECL;
  FUNCTION al_mouse_button_down (CONST state: ALLEGRO_MOUSE_STATEptr; button: LONGINT): BOOLEAN; CDECL;
  FUNCTION al_get_mouse_state_axis (CONST state: ALLEGRO_MOUSE_STATEptr; axis: LONGINT): LONGINT; CDECL;

  FUNCTION al_get_mouse_event_source: ALLEGRO_EVENT_SOURCEptr; CDECL;

(*
 * Cursors:
 *
 * This will probably become part of the display API.  It provides for
 * hardware cursors only; software cursors may or may not be provided
 * for later (it would need significant cooperation from the display
 * API).
 *)
  FUNCTION al_create_mouse_cursor (sprite: ALLEGRO_BITMAPptr; xfocus, yfocus: LONGINT): ALLEGRO_MOUSE_CURSORptr; CDECL;
  PROCEDURE al_destroy_mouse_cursor (cursor: ALLEGRO_MOUSE_CURSORptr);
  FUNCTION al_set_mouse_cursor (display: ALLEGRO_DISPLAYptr; cursor: ALLEGRO_MOUSE_CURSORptr): BOOLEAN; CDECL;
  FUNCTION al_set_system_mouse_cursor (display: ALLEGRO_DISPLAYptr; cursor_id: ALLEGRO_SYSTEM_MOUSE_CURSOR): BOOLEAN; CDECL;
  FUNCTION al_show_mouse_cursor (display: ALLEGRO_DISPLAYptr): BOOLEAN; CDECL;
  FUNCTION al_hide_mouse_cursor (display: ALLEGRO_DISPLAYptr): BOOLEAN; CDECL;
  FUNCTION al_get_mouse_cursor_position (ret_x, ret_y: PLONGINT): BOOLEAN; CDECL;
  FUNCTION al_grab_mouse (display: ALLEGRO_DISPLAYptr): BOOLEAN; CDECL;
  FUNCTION al_ungrab_mouse: BOOLEAN; CDECL;



(******************************************************************************
 * timer.h *
 ***********)

(* Converts microseconds to seconds. *)
  FUNCTION ALLEGRO_USECS_TO_SECS (x: DOUBLE): DOUBLE; INLINE;

(* Converts milliseconds to seconds. *)
  FUNCTION ALLEGRO_MSECS_TO_SECS (x: DOUBLE): DOUBLE; INLINE;

(* Converts beats per second to seconds. *)
  FUNCTION ALLEGRO_BPS_TO_SECS (x: DOUBLE): DOUBLE; INLINE;

(* Converts beats per minute to seconds. *)
  FUNCTION ALLEGRO_BPM_TO_SECS (x: DOUBLE): DOUBLE; INLINE;

  TYPE
    ALLEGRO_TIMERptr = POINTER;

  FUNCTION al_create_timer (speed_secs: DOUBLE): ALLEGRO_TIMERptr; CDECL;
  PROCEDURE al_destroy_timer (timer: ALLEGRO_TIMERptr); CDECL;
  PROCEDURE al_start_timer (timer: ALLEGRO_TIMERptr); CDECL;
  PROCEDURE al_stop_timer (timer: ALLEGRO_TIMERptr); CDECL;
  FUNCTION al_get_timer_started (CONST timer: ALLEGRO_TIMERptr): BOOLEAN; CDECL;
  FUNCTION al_get_timer_speed (CONST timer: ALLEGRO_TIMERptr): DOUBLE; CDECL;
  PROCEDURE al_set_timer_speed (timer: ALLEGRO_TIMERptr; speed_secs: DOUBLE); CDECL;
  FUNCTION al_get_timer_count (CONST timer: ALLEGRO_TIMERptr): INT64; CDECL;
  PROCEDURE al_set_timer_count (timer: ALLEGRO_TIMERptr; count: INT64); CDECL;
  PROCEDURE al_add_timer_count (timer: ALLEGRO_TIMERptr; diff: INT64); CDECL;
  FUNCTION al_get_timer_event_source (timer: ALLEGRO_TIMERptr): ALLEGRO_EVENT_SOURCEptr; CDECL;



(******************************************************************************
 * altime.h *
 ************)

  TYPE
    ALLEGRO_TIMEOUTptr = ^ALLEGRO_TIMEOUT;
  (* This is an abstract data type representing a timer object. *)
    ALLEGRO_TIMEOUT = RECORD
      __pad1__, __pad2__: INT64;
    END;

    FUNCTION al_get_time: DOUBLE; CDECL;
    PROCEDURE al_rest (seconds: DOUBLE); CDECL;

  (* TODO: Change to VAR? *)
    PROCEDURE al_init_timeout (timeout: ALLEGRO_TIMEOUTptr; seconds: DOUBLE); CDECL;



(******************************************************************************
 * events.h *
 ************)

  TYPE
    ALLEGRO_EVENT_TYPE = LONGWORD;

  CONST
    ALLEGRO_EVENT_JOYSTICK_AXIS          = 1;
    ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN   = 2;
    ALLEGRO_EVENT_JOYSTICK_BUTTON_UP     = 3;
    ALLEGRO_EVENT_JOYSTICK_CONFIGURATION = 4;

    ALLEGRO_EVENT_KEY_DOWN               = 10;
    ALLEGRO_EVENT_KEY_CHAR               = 11;
    ALLEGRO_EVENT_KEY_UP                 = 12;

    ALLEGRO_EVENT_MOUSE_AXES             = 20;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN      = 21;
    ALLEGRO_EVENT_MOUSE_BUTTON_UP        = 22;
    ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY    = 23;
    ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY    = 24;
    ALLEGRO_EVENT_MOUSE_WARPED           = 25;

    ALLEGRO_EVENT_TIMER                  = 30;

    ALLEGRO_EVENT_DISPLAY_EXPOSE         = 40;
    ALLEGRO_EVENT_DISPLAY_RESIZE         = 41;
    ALLEGRO_EVENT_DISPLAY_CLOSE          = 42;
    ALLEGRO_EVENT_DISPLAY_LOST           = 43;
    ALLEGRO_EVENT_DISPLAY_FOUND          = 44;
    ALLEGRO_EVENT_DISPLAY_SWITCH_IN      = 45;
    ALLEGRO_EVENT_DISPLAY_SWITCH_OUT     = 46;
    ALLEGRO_EVENT_DISPLAY_ORIENTATION    = 47;

(*    1 <= n < 512  - builtin events
 *  512 <= n < 1024 - reserved user events (for addons)
 * 1024 <= n        - unreserved user events
 *)
  FUNCTION ALLEGRO_EVENT_TYPE_IS_USER (t: LONGINT): BOOLEAN; INLINE;

(*
 * Event structures
 *
 * All event types have the following fields in common.
 *
 *  _type     -- the type of event this is
 *  timestamp -- when this event was generated
 *  source    -- which event source generated this event
 *
 * For people writing event sources: The common fields must be at the
 * very start of each event structure.
 *)
  TYPE
    ALLEGRO_ANY_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_EVENT_SOURCEptr;
      timestamp : DOUBLE;
    END;

    ALLEGRO_DISPLAY_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_DISPLAYptr;
      timestamp : DOUBLE;
      x : LONGINT;
      y : LONGINT;
      width : LONGINT;
      height : LONGINT;
      orientation : LONGINT;
    END;

    ALLEGRO_JOYSTICK_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_JOYSTICKptr;
      timestamp : DOUBLE;
      id : ALLEGRO_JOYSTICKptr;
      stick : LONGINT;
      axis : LONGINT;
      pos : SINGLE;
      button : LONGINT;
    END;

    ALLEGRO_KEYBOARD_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_KEYBOARDptr;
      timestamp : DOUBLE;
      display : ALLEGRO_DISPLAYptr;
      keycode : LONGINT;
      unichar : LONGINT;
      modifiers : LONGWORD;
      _repeat : BOOLEAN;
    END;

    ALLEGRO_MOUSE_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_MOUSEptr;
      timestamp : DOUBLE;
      display : ALLEGRO_DISPLAYptr;
      x, y, z, w : LONGINT;
      dx, dy, dz, dw : LONGINT;
      button : LONGWORD;
      pressure : SINGLE;
    END;

    ALLEGRO_TIMER_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_TIMERptr;
      timestamp : DOUBLE;
      count : INT64;
      error : DOUBLE;
    END;

    ALLEGRO_USER_EVENT_DESCRIPTORptr = POINTER;

    ALLEGRO_USER_EVENTptr = ^ALLEGRO_USER_EVENT;
    ALLEGRO_USER_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_EVENT_SOURCEptr;
      timestamp : DOUBLE;
      __internal__descr : ALLEGRO_USER_EVENT_DESCRIPTORptr;
      data1 : PLONGINT;
      data2 : PLONGINT;
      data3 : PLONGINT;
      data4 : PLONGINT;
    END;

    ALLEGRO_EVENTptr = ^ALLEGRO_EVENT;

    ALLEGRO_EVENT = RECORD
      case LONGINT OF
   (* This must be the same as the first field of _AL_EVENT_HEADER.  *)
	0 : ( _type : ALLEGRO_EVENT_TYPE );
   (* `any' is to allow the user to access the other fields which are
    * common to all event types, without using some specific type
    * structure.
    *)
	1 : ( any : ALLEGRO_ANY_EVENT );
	2 : ( display : ALLEGRO_DISPLAY_EVENT );
	3 : ( joystick : ALLEGRO_JOYSTICK_EVENT );
	4 : ( keyboard : ALLEGRO_KEYBOARD_EVENT );
	5 : ( mouse : ALLEGRO_MOUSE_EVENT );
	6 : ( timer : ALLEGRO_TIMER_EVENT );
	7 : ( user : ALLEGRO_USER_EVENT );
      END;

      ALLEGRO_EVENT_DTOR_PROC = PROCEDURE (evt: ALLEGRO_USER_EVENTptr); CDECL;

(* Event sources *)
  PROCEDURE al_init_user_event_source (source: ALLEGRO_EVENT_SOURCEptr); CDECL; { TODO: Use VAR parameter? }
  PROCEDURE al_destroy_user_event_source (source: ALLEGRO_EVENT_SOURCEptr); CDECL;
(* The second argument is ALLEGRO_EVENT instead of ALLEGRO_USER_EVENT
 * to prevent users passing a pointer to a too-short structure.
 *)
  FUNCTION al_emit_user_event (source: ALLEGRO_EVENT_SOURCEptr; Event: ALLEGRO_EVENTptr; dtor: ALLEGRO_EVENT_DTOR_PROC): BOOLEAN; CDECL;
  PROCEDURE al_unref_user_event (event: ALLEGRO_USER_EVENTptr); CDECL;
  PROCEDURE al_set_event_source_data (source: ALLEGRO_EVENT_SOURCEptr; data: PLONGINT); CDECL;
  FUNCTION al_get_event_source_data (CONST source: ALLEGRO_EVENT_SOURCEptr): PLONGINT; CDECL;

  TYPE
    ALLEGRO_EVENT_QUEUEptr = POINTER;

  FUNCTION al_create_event_queue: ALLEGRO_EVENT_QUEUEptr; CDECL;
  PROCEDURE al_destroy_event_queue (queue: ALLEGRO_EVENT_QUEUEptr); CDECL;
  PROCEDURE al_register_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr); CDECL;
  PROCEDURE al_unregister_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr); CDECL;
  FUNCTION al_is_event_queue_empty (queue: ALLEGRO_EVENT_QUEUEptr): BOOLEAN; CDECL;
  FUNCTION al_get_next_event (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr): BOOLEAN; CDECL;
  FUNCTION al_peek_next_event (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr): BOOLEAN; CDECL;
  FUNCTION al_drop_next_event (queue: ALLEGRO_EVENT_QUEUEptr): BOOLEAN; CDECL;
  PROCEDURE al_flush_event_queue (queue: ALLEGRO_EVENT_QUEUEptr); CDECL;
  PROCEDURE al_wait_for_event (queue: ALLEGRO_EVENT_QUEUEptr; VAR event: ALLEGRO_EVENT); CDECL;
  FUNCTION al_wait_for_event_timed (queue: ALLEGRO_EVENT_QUEUEptr; VAR event: ALLEGRO_EVENT; secs: SINGLE): BOOLEAN; CDECL;
  FUNCTION al_wait_for_event_until (queue: ALLEGRO_EVENT_QUEUEptr; VAR event: ALLEGRO_EVENT; timeout: ALLEGRO_TIMEOUTptr): BOOLEAN; CDECL;

IMPLEMENTATION

(******************************************************************************
 * base.h *
 **********)

  FUNCTION al_get_allegro_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_run_main (argc: LONGINT; argv: POINTER; user_main: ALLEGRO_USER_MAIN): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION AL_ID (str: SHORTSTRING): LONGINT;
  BEGIN
    AL_ID := (ORD (str[1]) SHL 24) OR (ORD (str[2]) SHL 16)
	     OR (ORD (str[3]) SHL  8) OR  ORD (str[4]);
  END;



(******************************************************************************
 * system.h *
 ************)

  FUNCTION al_init: BOOLEAN;
  BEGIN
    al_init := al_install_system (ALLEGRO_VERSION_INT, NIL);
  END;

  FUNCTION al_install_system (version: LONGWORD; atexit_ptr: POINTER): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_uninstall_system; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_is_system_installed: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_system_driver: ALLEGRO_SYSTEMptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_inhibit_screensaver (inhibit: BOOLEAN): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * bitmap.h *
 ************)

  PROCEDURE al_set_new_bitmap_format (format: ALLEGRO_PIXEL_FORMAT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_bitmap_flags (flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_new_bitmap_format: ALLEGRO_PIXEL_FORMAT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_new_bitmap_flags: LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_add_new_bitmap_flag (flag: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_bitmap_width (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_bitmap_height (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_bitmap_format (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_bitmap_flags (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_bitmap (w, h: LONGINT): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_destroy_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Blitting *)
  PROCEDURE al_draw_bitmap (bitmap: ALLEGRO_BITMAPptr; dx, dy: SINGLE; flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_draw_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy: SINGLE; flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_draw_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy, dw, dh: SINGLE; flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_draw_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, angle: SINGLE; flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_draw_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, xscale, yscale, angle: SINGLE; flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Tinted blitting *)
   PROCEDURE al_draw_tinted_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; dx, dy: SINGLE; flags: LONGINT); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   PROCEDURE al_draw_tinted_bitmap_region (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy: SINGLE; flags: LONGINT); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   PROCEDURE al_draw_tinted_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy, dw, dh: SINGLE; flags: LONGINT); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   PROCEDURE al_draw_tinted_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, angle: SINGLE; flags: LONGINT); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   PROCEDURE al_draw_tinted_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, xscale, yscale, angle: SINGLE; flags: LONGINT); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

(* Locking *)
  FUNCTION al_lock_bitmap (bitmap: ALLEGRO_BITMAPptr; format: ALLEGRO_PIXEL_FORMAT; flags: LONGINT): ALLEGRO_LOCKED_REGIONptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_lock_bitmap (bitmap: ALLEGRO_BITMAPptr; x, y, width, height: LONGINT; format: ALLEGRO_PIXEL_FORMAT; flags: LONGINT): ALLEGRO_LOCKED_REGIONptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_unlock_bitmap (bitmap: ALLEGRO_BITMAPptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_put_pixel (x, y: LONGINT; color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_put_blended_pixel (x, y: LONGINT; color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_pixel (bitmap: ALLEGRO_BITMAPptr; x, y: LONGINT): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_pixel_size (format: ALLEGRO_PIXEL_FORMAT): LONGINT;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Pixel mapping *)
  FUNCTION al_map_rgb (r, g, b: BYTE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_map_rgba (r, g, b, a: BYTE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_map_rgb_f (r, g, b: SINGLE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_map_rgba_f (r, g, b, a: SINGLE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Pixel unmapping *)
  PROCEDURE al_unmap_rgb (color: ALLEGRO_COLOR; VAR r, g, b: BYTE); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_unmap_rgba (color: ALLEGRO_COLOR; VAR r, g, b, a: BYTE); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_unmap_rgb_f (color: ALLEGRO_COLOR; VAR r, g, b: SINGLE); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_unmap_rgba_f (color: ALLEGRO_COLOR; VAR r, g, b, a: SINGLE); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_pixel_format_bits (format: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Masking *)
  PROCEDURE al_convert_mask_to_alpha (bitmap: ALLEGRO_BITMAPptr; mask_color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Clipping *)
  PROCEDURE al_set_clipping_rectangle (x, y, width, height: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_clipping_rectangle (VAR x, y, w, h: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Sub bitmaps *)
  FUNCTION al_create_sub_bitmap (parent: ALLEGRO_BITMAPptr; x, y, w, h: LONGINT): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_is_sub_bitmap (bitmap: ALLEGRO_BITMAPptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Miscellaneous *)
   FUNCTION al_clone_bitmap (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_BITMAPptr; CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   FUNCTION al_is_bitmap_locked (bitmap: ALLEGRO_BITMAPptr): BOOLEAN; CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

(* Blending *)
   PROCEDURE al_set_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   PROCEDURE al_get_blender (VAR op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   PROCEDURE al_set_separate_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE;
				      alpha_op: ALLEGRO_BLEND_OPERATIONS; alpha_source, alpha_dest: ALLEGRO_BLEND_MODE); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   PROCEDURE al_get_separate_blender (VAR op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE;
				      alpha_op: ALLEGRO_BLEND_OPERATIONS; alpha_source, alpha_dest: ALLEGRO_BLEND_MODE); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;

   PROCEDURE _al_put_pixel (bitmap: ALLEGRO_BITMAPptr; x,y: LONGINT; color: ALLEGRO_COLOR); CDECL;
   EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * bitmap_io.h *
 ***************)

  FUNCTION al_load_bitmap (CONST filename: PCHAR): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_save_bitmap (CONST filename: PCHAR): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * display.h *
 *************)

  PROCEDURE al_set_new_display_refresh_rate (refresh_rate: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_display_flags (flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_new_display_refresh_rate: LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_new_display_flags: LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_width (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_height (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_format (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_refresh_rate (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_flags (display: ALLEGRO_DISPLAYptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_toggle_display_flag (display: ALLEGRO_DISPLAYptr; flag: LONGINT; onoff: BOOLEAN): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_display (w, h: LONGINT): ALLEGRO_DISPLAYptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_destroy_display (display: ALLEGRO_DISPLAYptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_current_display: ALLEGRO_DISPLAYptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_target_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_target_backbuffer (display: ALLEGRO_DISPLAYptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_backbuffer (display: ALLEGRO_DISPLAYptr): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_target_bitmap: ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_acknowledge_resize (display: ALLEGRO_DISPLAYptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_resize_display (display: ALLEGRO_DISPLAYptr; width, height: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_flip_display; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_update_display_region (x, y, Width, height: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_is_compatible_bitmap (bitmap: ALLEGRO_BITMAPptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_num_display_modes: LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_mode (index: LONGINT; mode: ALLEGRO_DISPLAY_MODEptr): ALLEGRO_DISPLAY_MODEptr;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_wait_for_vsync: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_event_source (display: ALLEGRO_DISPLAYptr): ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Primitives *)
  PROCEDURE al_clear_to_color (color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_draw_pixel (x, y: SINGLE; color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_display_icon (display: ALLEGRO_DISPLAYptr; icon: ALLEGRO_BITMAPptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(* Stuff for multihead/window management *)
  FUNCTION al_get_num_video_adapters: LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_monitor_info (adapter: LONGINT; VAR info: ALLEGRO_MONITOR_INFO): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_new_display_adapter: LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_display_adapter (adapter: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_window_position (x, y: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_new_window_position (VAR x, y: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_window_position (display: ALLEGRO_DISPLAYptr; x, y: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_window_position (display: ALLEGRO_DISPLAYptr; VAR x, y: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_window_title (display: ALLEGRO_DISPLAYptr; CONST title: PCHAR);CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_display_option (option: ALLEGRO_DISPLAY_OPTIONS; value, importance: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_new_display_option (display: ALLEGRO_DISPLAYptr; option: ALLEGRO_DISPLAY_OPTIONS): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_reset_new_display_options; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_option (display: ALLEGRO_DISPLAYptr; option: ALLEGRO_DISPLAY_OPTIONS): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

(*Deferred drawing*)
  PROCEDURE al_hold_bitmap_drawing (hold: BOOLEAN); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_is_bitmap_drawing_held: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * keyboard.h *
 **************)

  FUNCTION al_is_keyboard_installed: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_install_keyboard: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_uninstall_keyboard; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_keyboard_leds (leds: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_keycode_to_name (keycode: LONGINT): PCHAR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_keyboard_state (ret_state: ALLEGRO_KEYBOARD_STATEptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_key_down (CONST state: ALLEGRO_KEYBOARD_STATEptr; keycode: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_keyboard_event_source: ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * joystick.h *
 ************)

  FUNCTION al_install_joystick: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_uninstall_joystick  ; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_is_joystick_installed: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_reconfigure_joysticks: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_num_joysticks: LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick  (joyn: LONGINT): ALLEGRO_JOYSTICKptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_release_joystick (j: ALLEGRO_JOYSTICKptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_active (j: ALLEGRO_JOYSTICKptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_name (j: ALLEGRO_JOYSTICKptr): PCHAR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_num_sticks (j: ALLEGRO_JOYSTICKptr) : LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_stick_flags (j: ALLEGRO_JOYSTICKptr; stick: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_stick_name (j: ALLEGRO_JOYSTICKptr; stick: LONGINT): PCHAR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_num_axes  (j: ALLEGRO_JOYSTICKptr; stick: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_axis_name (j: ALLEGRO_JOYSTICKptr; stick, axis: LONGINT): PCHAR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_num_buttons (j: ALLEGRO_JOYSTICKptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_button_name (j: ALLEGRO_JOYSTICKptr; buttonn: LONGINT): PCHAR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_joystick_state (j: ALLEGRO_JOYSTICKptr; VAR ret_state: ALLEGRO_JOYSTICK_STATE); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_event_source: ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * mouse.h *
 ***********)

  FUNCTION al_is_mouse_installed: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_install_mouse: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_uninstall_mouse; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_mouse_num_buttons: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_mouse_num_axes: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_mouse_xy (display: ALLEGRO_DISPLAYptr; x, y: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_mouse_z (z: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_mouse_w (w: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_mouse_axis (axis, value: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_mouse_state (ret_state: ALLEGRO_MOUSE_STATEptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_mouse_button_down (CONST state: ALLEGRO_MOUSE_STATEptr; button: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_mouse_state_axis (CONST state: ALLEGRO_MOUSE_STATEptr; axis: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_mouse_event_source: ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_mouse_cursor (sprite: ALLEGRO_BITMAPptr; xfocus, yfocus: LONGINT): ALLEGRO_MOUSE_CURSORptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_destroy_mouse_cursor (cursor: ALLEGRO_MOUSE_CURSORptr);
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_mouse_cursor (display: ALLEGRO_DISPLAYptr; cursor: ALLEGRO_MOUSE_CURSORptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_system_mouse_cursor (display: ALLEGRO_DISPLAYptr; cursor_id: ALLEGRO_SYSTEM_MOUSE_CURSOR): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_show_mouse_cursor (display: ALLEGRO_DISPLAYptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_hide_mouse_cursor (display: ALLEGRO_DISPLAYptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_mouse_cursor_position (ret_x, ret_y: PLONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_grab_mouse (display: ALLEGRO_DISPLAYptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_ungrab_mouse: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * timer.h *
 ***********)

  FUNCTION ALLEGRO_USECS_TO_SECS (x: DOUBLE): DOUBLE;
  BEGIN
    ALLEGRO_USECS_TO_SECS := x / 1000000
  END;

  FUNCTION ALLEGRO_MSECS_TO_SECS (x: DOUBLE): DOUBLE;
  BEGIN
    ALLEGRO_MSECS_TO_SECS := x / 1000
  END;

  FUNCTION ALLEGRO_BPS_TO_SECS (x: DOUBLE): DOUBLE;
  BEGIN
    ALLEGRO_BPS_TO_SECS := 1 / x
  END;

  FUNCTION ALLEGRO_BPM_TO_SECS (x: DOUBLE): DOUBLE;
  BEGIN
    ALLEGRO_BPM_TO_SECS := 60 / x
  END;

  FUNCTION al_create_timer (speed_secs: DOUBLE): ALLEGRO_TIMERptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_destroy_timer (timer: ALLEGRO_TIMERptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_start_timer (timer: ALLEGRO_TIMERptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_stop_timer (timer: ALLEGRO_TIMERptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_timer_started (CONST timer: ALLEGRO_TIMERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_timer_speed (CONST timer: ALLEGRO_TIMERptr): DOUBLE; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_timer_speed (timer: ALLEGRO_TIMERptr; speed_secs: DOUBLE); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_timer_count (CONST timer: ALLEGRO_TIMERptr): INT64; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_timer_count (timer: ALLEGRO_TIMERptr; count: INT64); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_add_timer_count (timer: ALLEGRO_TIMERptr; diff: INT64); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_timer_event_source (timer: ALLEGRO_TIMERptr): ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * altime.h *
 ************)

  FUNCTION ALLEGRO_EVENT_TYPE_IS_USER (t: LONGINT): BOOLEAN;
  BEGIN
    ALLEGRO_EVENT_TYPE_IS_USER := t >= 512;
  END;

  FUNCTION al_get_time: DOUBLE; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_rest (seconds: DOUBLE); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_init_timeout (timeout: ALLEGRO_TIMEOUTptr; seconds: DOUBLE); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;



(******************************************************************************
 * events.h *
 ************)

  PROCEDURE al_init_user_event_source (source: ALLEGRO_EVENT_SOURCEptr); CDECL; { TODO: Use VAR parameter? }
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_destroy_user_event_source (source: ALLEGRO_EVENT_SOURCEptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_emit_user_event (source: ALLEGRO_EVENT_SOURCEptr; Event: ALLEGRO_EVENTptr; dtor: ALLEGRO_EVENT_DTOR_PROC): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_unref_user_event (event: ALLEGRO_USER_EVENTptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_event_source_data (source: ALLEGRO_EVENT_SOURCEptr; data: PLONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_event_source_data (CONST source: ALLEGRO_EVENT_SOURCEptr): PLONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_event_queue: ALLEGRO_EVENT_QUEUEptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_destroy_event_queue (queue: ALLEGRO_EVENT_QUEUEptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_register_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_unregister_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_is_event_queue_empty (queue: ALLEGRO_EVENT_QUEUEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_next_event (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_peek_next_event (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_drop_next_event (queue: ALLEGRO_EVENT_QUEUEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_flush_event_queue (queue: ALLEGRO_EVENT_QUEUEptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_wait_for_event (queue: ALLEGRO_EVENT_QUEUEptr; VAR event: ALLEGRO_EVENT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_wait_for_event_timed (queue: ALLEGRO_EVENT_QUEUEptr; VAR event: ALLEGRO_EVENT; secs: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_wait_for_event_until (queue: ALLEGRO_EVENT_QUEUEptr; VAR event: ALLEGRO_EVENT; timeout: ALLEGRO_TIMEOUTptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

INITIALIZATION
{ Delphi forces an INITIALIZATION section if FINALIZATION is used. }
  ;

FINALIZATION
{ Ensures that we call it, as Pascal hasn't an "atexit" function. }
  al_uninstall_system;
END.
