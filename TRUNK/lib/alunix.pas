UNIT alUNIX;
(*<UNIX and Linux specific stuff.

  In most cases, an UNIX or Linux program that uses the Allegro library
  doesn't need this unit.
*)

{$INCLUDE allegro.cfg }

INTERFACE

  VAR
  (* This is a pointer to the Allegro X11 icon, which is in the format of
    standard .xpm bitmap data. You do not normally have to bother with this at
    all: you can use the xfixicon.sh utility from the @code(tools/x11)
    directory to convert a true colour bitmap to a C file that you only need
    to link with your own code to set the icon. *)
    allegro_icon: AL_VOIDptr;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

(*****************************************************************************
 * platform/alunix.h
 *      UNIX and Linux based specific header defines.
 *)

(* Note:  I've changed the order for the sake of the documentation. *)

  CONST
  (* X Windows system driver. @seealso(al_install) *)
    AL_SYSTEM_XWINDOWS = $5857494E;
  (* Linux system driver. @seealso(al_install) *)
    AL_SYSTEM_LINUX = $4C4E5843;

  (* The standard X graphics driver. This should work on any Unix system, and
    can operate remotely. It does not require root permissions. If the ARGB
    cursor extension is available, this driver is capable of displaying a
    hardware cursor. This needs to be enabled by calling
    @link(al_enable_hardware_cursor) because it cannot be used reliably
    alongside @link(al_get_mouse_mickeys).
    @seealso(al_set_gfx_mode) @seealso(AL_GFX_XWINDOWS_FULLSCREEN) *)
    AL_GFX_XWINDOWS = $47444942;
  (* The same as @code(AL_GFX_XWINDOWS), but while @code(AL_GFX_XWINDOWS) runs
    windowed, this one uses the @italic(XF86VidMode) extension to make it run
    in fullscreen mode even without root permissions. You're still using the
    standard X protocol though, so expect the same low performances as with the
    windowed driver version. If the ARGB cursor extension is available, this
    driver is capable of displaying a hardware cursor. This needs to be enabled
    by calling @link(al_enable_hardware_cursor) because it cannot be used
    reliably alongside @link(al_get_mouse_mickeys).
    @seealso(al_set_gfx_mode) @seealso(AL_GFX_XWINDOWS) *)
    AL_GFX_XWINDOWS_FULLSCREEN = $58574653;
  (* @exclude Undocumented. *)
    AL_GFX_XDGA = $58444741;
  (* @exclude Undocumented. *)
    AL_GFX_XDGA_FULLSCREEN = $58444653;
  (* Uses new DGA 2.0 extension provided by XFree86 4.0.x. This will work in
    fullscreen mode, and it will support hardware acceleration if available.
    This driver requires root permissions.
    @seealso(al_set_gfx_mode) @seealso(AL_GFX_XDGA2_SOFT) *)
    AL_GFX_XDGA2 = $44474132;
  (* The same as @code(AL_GFX_XDGA2), but turns off hardware acceleration support. This
    driver requires root permissions.
    @seealso(al_set_gfx_mode) @seealso(AL_GFX_XDGA2) *)
    AL_GFX_XDGA2_SOFT = $44474153;



  (* Uses the framebuffer device (eg. @code(/dev/fb0)). This requires you to
    have framebuffer support compiled into your kernel, and correctly
    configured for your hardware. It is currently the only console mode driver
    that will work without root permissions, unless you are using a development
    version of SVGAlib.
    @seealso(al_set_gfx_mode) *)
    AL_GFX_FBCON = $46422020;
  (* Uses a VBE/AF driver (@code(vbeaf.drv)), assuming that you have installed
    one which works under Linux (currently only two of the FreeBE/AF project
    drivers are capable of this: I don't know about the SciTech ones). VBE/AF
    requires root permissions, but is currently the only Linux driver which
    supports hardware accelerated graphics.

    @bold(Note:) Previous paragraph may be outdated.
    @seealso(al_set_gfx_mode) *)
    AL_GFX_VBEAF = $56424146;
  (* Uses the SVGAlib library for graphics output. This requires root
    permissions if your version of SVGAlib requires them.
    @seealso(al_set_gfx_mode) *)
    AL_GFX_SVGALIB = $53564741;
  (* Uses direct hardware access to set standard VGA resolutions, supporting
    the same modes as in the DOS version of this driver. Requires root
    permissions. @seealso(al_set_gfx_mode) *)
    AL_GFX_VGA = $56474120;
  (* Uses direct hardware access to set @italic(standard but not documented)
    Mode-X resolutions, supporting the same modes as in the DOS version of this
    driver. Requires root permissions. @seealso(al_set_gfx_mode) *)
    AL_GFX_MODEX = $4D4F4458;



  (* Open Sound System. @seealso(al_install_sound) *)
    AL_DIGI_OSS = $4F535344;
  (* Enlightened Sound Daemon. @seealso(al_install_sound) *)
    AL_DIGI_ESD = $45534444;
  (* aRts (Analog Real-Time Synthesizer). @seealso(al_install_sound) *)
    AL_DIGI_ARTS = $41525453;
  (* ALSA sound driver. @seealso(al_install_sound) *)
    AL_DIGI_ALSA = $414C5341;
  (* JACK sound driver. @seealso(al_install_sound) *)
    AL_DIGI_JACK = $4A41434B;

  (* Open Sound System. @seealso(al_install_sound) *)
    AL_MIDI_OSS = $4F53534D;
  (* ALSA RawMIDI driver. @seealso(al_install_sound) *)
    AL_MIDI_ALSA = $414D4944;

  (* Regular joystick interface. Joystick support needs to be enabled in your
    kernel. @seealso(al_install_joystick) *)
    AL_JOY_TYPE_LINUX_ANALOGUE = $4C4E5841;



(*****************************************************************************
 * xalleg.h
 *      X header file for the Allegro library.
 *
 *      This prototypes some things which might be useful to
 *      the calling application, but you don't need it.
 *)

(* This function is only available under X. It lets you to specify the window
  name and group (or class). They are important because they allow the window
  manager to remember the window attributes (position, layer, etc). Note that
  the name and the title of the window are two different things: the title is
  what appears in the title bar of the window, but usually has no other effects
  on the behaviour of the application.
  @seealso(al_set_window_title) *)
  PROCEDURE al_xwin_set_window_name (CONST name, group: AL_STR);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'xwin_set_window_name';

IMPLEMENTATION

END.
