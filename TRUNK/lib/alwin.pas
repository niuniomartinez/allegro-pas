UNIT alWin;
(*<Windows specific stuff.  Note that this unit compiles @bold(only) on Windows.

  In most cases, a Windows program that uses the Allegro library doesn't need
  this unit; however, if it also needs to directly call non portable Win32 API
  functions, it must include this Windows-specific unit.

  When creating the main window, Allegro searches the executable for an
  @code(ICON) resource named @italic("allegro_icon"). If it is present, Allegro
  automatically loads it and uses it as its application icon; otherwise,
  Allegro uses the default @code(IDI_APPLICATION) icon. See the manual of your
  compiler for a method to create an @code(ICON) resource, or use the
  @code(wfixicon) utility from the tools/win directory.

  DirectX requires that system and video bitmaps (including the screen) be
  locked before you can draw onto them. This will be done automatically, but
  you can usually get much better performance by doing it yourself: see the
  @link(al_acquire_bitmap) function for details.

  Due to a major oversight in the design of DirectX, there is no way to
  preserve the contents of video memory when the user switches away from your
  program. You need to be prepared for the fact that your screen contents, and
  the contents of any video memory bitmaps, may be destroyed at any point. You
  can use the @link(al_set_display_switch_callback) function to find out when
  this happens.

  On the Windows platform, the only return values for the
  @link(al_desktop_color_depth) function are @code(8), @code(16), @code(24) and
  @code(32). This means that 15-bit and 16-bit desktops cannot be
  differentiated and are both reported as 16-bit desktops. See below for the
  consequences for windowed and overlay DirectX drivers.
*)

{$INCLUDE allegro.cfg }

INTERFACE

  USES
    windows;


(*****************************************************************************
 * platform/alwin.h
 *      Windows-specific header defines.
 *)
  CONST
  (* DirectX system driver. @seealso(al_install) *)
    AL_SYSTEM_DIRECTX = $44582020;

  (* Alias for @link(AL_GFX_DIRECTX_ACCEL). *)
    AL_GFX_DIRECTX = $44584143;
  (* The regular fullscreen DirectX driver, running with hardware acceleration
    enabled. @seealso(al_set_gfx_mode) *)
    AL_GFX_DIRECTX_ACCEL = $44584143;
  (* Simplified fullscreen DirectX driver that doesn't support any hardware
    acceleration, video or system bitmaps, etc. @seealso(al_set_gfx_mode) *)
    AL_GFX_DIRECTX_SAFE = $44585341;
  (* DirectX fullscreen driver that only uses software drawing, rather than any
    hardware accelerated features. @seealso(al_set_gfx_mode) *)
    AL_GFX_DIRECTX_SOFT = $4458534F;
  (* The regular windowed DirectX driver, running in color conversion mode when
    the color depth doesn't match that of the Windows desktop. Color conversion
    is much slower than direct drawing and is not supported between 15-bit and
    16-bit color depths. This limitation is needed to work around that of
    @link(al_desktop_color_depth) and allows to select the direct drawing mode
    in a reliable way on desktops reported as 16-bit.

    Note that, mainly for performance reasons, this driver requires the width
    of the screen to be a multiple of 4. This driver is capable of displaying a
    hardware cursor, but there are size restrictions. Typically, the cursor
    image cannot be more than 32x32 pixels.
    @seealso(al_set_gfx_mode) *)
    AL_GFX_DIRECTX_WIN = $4458574E;
  (* he DirectX overlay driver. It uses special hardware features to run your
    program in a windowed mode: it doesn't work on all hardware, but
    performance is excellent on cards that are capable of it. It requires the
    color depth to be the same as that of the Windows desktop. In light of the
    limitation of @link(al_desktop_color_depth), the reliable way of setting
    the overlay driver on desktops reported as 16-bit is:
@longcode(#
  IF al_desktop_color_depth = 16 THEN
  BEGIN
    al_set_color_depth (16);
    IF NOT al_set_gfx_mode (AL_GFX_DIRECTX_OVL, 640, 480, 0, 0) THEN
    BEGIN
      al_set_color_depth (15);
      IF NOT al_set_gfx_mode (AL_GFX_DIRECTX_OVL, 640, 480, 0, 0) THEN
      // DirectX overlay driver not supported, so raise an error.
	RAISE Exception.Create ('640x480 overlay driver not supported.');
    END;
  // Ok, the 640x480 overlay driver is running.
  END;
#)
    @seealso(al_set_gfx_mode) *)
    AL_GFX_DIRECTX_OVL = $44584F56;
  (* The windowed GDI driver. It is extremely slow, but is guaranteed to work
    on all hardware, so it can be useful for situations where you want to run
    in a window and don't care about performance. Note that this driver
    features a hardware mouse cursor emulation in order to speed up basic mouse
    operations (like GUI operations).
    @seealso(al_set_gfx_mode) *)
    AL_GFX_GDI = $47444942;

(* Use DirectSound device @code(n) (zero-based) with direct mixing.
   @seealso(al_detect_digi_driver) @seealso(al_install_sound)
   @seealso(al_install_sound_input) *)
  FUNCTION AL_DIGI_DIRECTX (CONST n: INTEGER): AL_LONGINT; INLINE
(* Use DirectSound device @code(n) (zero-based) with Allegro mixing.
   @seealso(al_detect_digi_driver) @seealso(al_install_sound)
   @seealso(al_install_sound_input) *)
  FUNCTION AL_DIGI_DIRECTAMX (CONST n: INTEGER): AL_LONGINT; INLINE
(* High or low quality WaveOut device.
   @param(HighQuality Tells if use high @(@true@) or low @(@false@) quality.)
   @seealso(al_detect_digi_driver) @seealso(al_install_sound)
   @seealso(al_install_sound_input) *)
  FUNCTION AL_DIGI_WAVOUTID (CONST HighQuality: BOOLEAN): AL_LONGINT; INLINE

  CONST
  (* Use win32 MIDI mapper.
    @seealso(al_detect_midi_driver) @seealso(al_install_sound)
    @seealso(al_install_sound_input) *)
    AL_MIDI_WIN32MAPPER = $5733324D;
(* Use win32 device @code(n) (zero-based)
    @seealso(al_detect_midi_driver) @seealso(al_install_sound)
    @seealso(al_install_sound_input) *)
  FUNCTION AL_MIDI_WIN32 (CONST n: INTEGER): AL_LONGINT; INLINE
(* @exclude Undocumented(?) *)
  FUNCTION AL_MIDI_WIN32_IN (CONST n: INTEGER): AL_LONGINT; INLINE

  CONST
  (* Use DirectInput to access the joystick. @seealso(al_install_joystick) *)
    AL_JOY_TYPE_DIRECTX = $44582020;
  (* Use the regular Win32 interface rather than DirectInput to access the
    joystick. @seealso(al_install_joystick) *)
    AL_JOY_TYPE_WIN32 = $57333220;



(*****************************************************************************
 * winalleg.h
 *     Windows header file for the Allegro library.
 *
 *     It must be included by Allegro programs that need to use
 *     direct Win32 API calls and by Win32 programs that need to
 *     interface with Allegro.
 *)
  TYPE
  (* See @link(al_win_set_wnd_create_proc). *)
    __AL_WIN_CREATE_PROC__ = FUNCTION (p: WNDPROC): HWND; CDECL;
  (* See @link(al_win_set_msg_pre_proc). *)
    __AL_WIN_MESSAGE_PROC__ = FUNCTION (w: HWND; m: UINT; wp: WPARAM; lp: LPARAM; p: AL_INTptr): AL_INT; CDECL;

(* Retrieves a handle to the window used by Allegro. Note that Allegro uses an
   underlying window even though you don't set any graphics mode, unless you
   have installed the neutral system driver (@link(AL_SYSTEM_NONE)). *)
  FUNCTION al_win_get_window: HWND;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_get_window';

(* Registers an user-created window to be used by Allegro. This function is
  meant to be called before initialising the library with @link(al_init) or
  installing the autodetected system driver (@link(AL_SYSTEM_AUTODETECT)). It
  lets you attach Allegro to any already existing window and prevents the
  library from creating its own, thus leaving you total control over the
  window; in particular, you are responsible for processing the events as usual
  (Allegro will automatically monitor a few of them, but will not filter out
  any of them). You can then use every component of the library (graphics,
  mouse, keyboard, sound, timers and so on), bearing in mind that some Allegro
  functions are blocking (e.g. @link(al_readkey)) if the key buffer is empty)
  and thus must be carefully manipulated by the window thread.

  However you can also call it after the library has been initialised, provided
  that no graphics mode is set. In this case the keyboard, mouse, joystick,
  sound and sound recording modules will be restarted.

  Passing @nil instructs Allegro to switch back to its built-in window if an
  user-created window was registered, or to request a new handle from Windows
  for its built-in window if this was already in use. *)
  PROCEDURE al_win_set_window (wnd: HWND);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_window';

(* Registers an user-defined procedure to be used by Allegro for creating its
  window. This function must be called @bold(before) initializing the library
  with @link(al_init) or installing the autodetected system driver
  (@link(AL_SYSTEM_AUTODETECT)). It lets you customize Allegro's window but
  only by its creation: unlike with @link(al_win_set_window), you have no
  control over the window once it has been created (in particular, you are not
  responsible for processing the events). The registered function will be
  passed a window procedure (@code(WNDPROC) object) that it must make the
  procedure of the new window of and it must return a handle to the new window.
  You can then use the full-featured library in the regular way. *)
  PROCEDURE al_win_set_wnd_create_proc (proc: __AL_WIN_CREATE_PROC__);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_wnd_create_proc';

(* Set a user callback to be called on window events. This callback function
  takes the same parameters as a normal window callback procedure plus an
  additional pointer to an integer. This pointer should be filled with a return
  value that must be set if the callback has completely processed the window
  event. If the callback has completely processed the window event, it should
  return @code(0) and fill @code(retval) with a proper value (the default is
  zero), otherwise it should return a non-zero value, and event processing will
  continue in Allegro's default window callback procedure. *)
  PROCEDURE al_win_set_msg_pre_proc (proc: __AL_WIN_MESSAGE_PROC__);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'win_set_msg_pre_proc';

IMPLEMENTATION

(* DirectSound device. *)
  FUNCTION AL_DIGI_DIRECTX (CONST n: INTEGER): AL_LONGINT;
  BEGIN
    RESULT := ( ORD ('D')      SHL 24) +
	      ( ORD ('X')      SHL 16) +
	      ((ORD ('A') + n) SHL  8) +
	        ORD (' ')
  END;



(* DirectSound device with Allegro mixing. *)
  FUNCTION AL_DIGI_DIRECTAMX (CONST n: INTEGER): AL_LONGINT;
  BEGIN
    RESULT := ( ORD ('A')      SHL 24) +
	      ( ORD ('X')      SHL 16) +
	      ((ORD ('A') + n) SHL  8) +
	        ORD (' ')
  END;



(* WaveOut device. *)
  FUNCTION AL_DIGI_WAVOUTID (CONST HighQuality: BOOLEAN): AL_LONGINT;
  VAR
    n = INTEGER;
  BEGIN
    IF HighQuality THEN n := 0 ELSE n := 1;
    RESULT := ( ORD ('W')      SHL 24) +
	      ( ORD ('O')      SHL 16) +
	      ((ORD ('A') + n) SHL  8) +
	        ORD (' ')
  END;



(* Win32 device. *)
  FUNCTION AL_MIDI_WIN32 (CONST n: INTEGER): AL_LONGINT;
  BEGIN
    RESULT := AL_ID ('W32A') + n
  END;



(* Win32 device. *)
  FUNCTION AL_MIDI_WIN32_IN (CONST n: INTEGER): AL_LONGINT;
  BEGIN
    RESULT := AL_ID ('W32A') + n
  END;

END.
