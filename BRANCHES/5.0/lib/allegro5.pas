UNIT Allegro5;
(*<Wrapper of the Allegro 5 core library. *)
(*TODO: License. *)

{$include allegro.cfg}

INTERFACE

  CONST
  (* Name of the dynamicly linked unit.

    @bold(TODO:) This should be defined at the @code(allegro.cfg) file as it's different in each platform.
   *)
    ALLEGRO_LIB_NAME = 'liballegro.so.5.0';

  (* Major version of Allegro. *)
    ALLEGRO_VERSION      =   5;
  (* Minor version of Allegro. *)
    ALLEGRO_SUB_VERSION  =   0;
  (* Revision number of Allegro. *)
    ALLEGRO_WIP_VERSION  =   5;
  (* Not sure we need it, but since @link(ALLEGRO_VERSION_STR) contains it:
     0 = SVN
     1 = first release
     2... = hotfixes?

     Note x.y.z (= x.y.z.0) has release number 1, and x.y.z.1 has release
     number 2, just to confuse you.
  *)
    ALLEGRO_RELEASE_NUMBER = 0;
  (* Packs version number in a simple @code(DWORD) number.
     @seealso(al_install_system)
   *)
    ALLEGRO_VERSION_INT  = (
		          (ALLEGRO_VERSION SHL 24)
		       OR (ALLEGRO_SUB_VERSION SHL 16)
		       OR (ALLEGRO_WIP_VERSION SHL  8)
		       OR  ALLEGRO_RELEASE_NUMBER
    );



(* Initialize the Allegro system.  No other Allegro functions can be called before this (with one or two exceptions).
   @param(version Should always be set to @link(ALLEGRO_VERSION_INT).)
   @param(atexit_ptr If it is non-@nil, and if hasn't been done already, @link(al_uninstall_system) will be registered as an @code(atexit) function.

   @bold(TODO:) Since Pascal doesn't has an @code(atexit) function @(it's a C function with similar functionality than @code(FINALIZATION) section@) may be it should be set always to @nil.  Ask to Allegro's developers about it.)
   @returns(@true if Allegro was successfully initialized by this function call @(or already was initialized previously@), @false if Allegro cannot be used.)
   @seealso(al_init)
 *)
  FUNCTION al_install_system (
    version: DWORD;
    atexit_ptr: POINTER
  ): BOOLEAN; CDECL;

(* Closes down the Allegro system.

   @bold(Note:) @code(al_uninstall_system) can be called without a corresponding @link(al_install_system) call.
 *)
  PROCEDURE al_uninstall_system; CDECL;

(* Like @link(al_install_system), but automatically passes @link(ALLEGRO_VERSION_INT) in the version and passes @nil as the @code(atexit_ptr) parameter.
   @seealso(al_install_system)
 *)
  FUNCTION al_init: BOOLEAN;

(* Returns the (compiled) version of the Allegro library.

  The version number is packed into a single integer as groups of 8 bits in the form @code(@(major SHL 24@) OR @(minor SHL 16@) OR @(revision SHL 8@) OR release).

  You can use code like this to extract them:
@longcode(#
VAR
  Version: DWORD;
  Major, Minor, Revision, Release: INTEGER;
BEGIN
  version := al_get_allegro_version;
  major := version SHR 24;
  minor := (version SHR 16) AND $FF;
  revision := (version SHR 8) AND $FF;
  release := version AND $FF;
END;
#)

  The release number is 0 for an unofficial version and 1 or greater for an official release. For example "5.0.2[1]" would be the (first) official 5.0.2 release while "5.0.2[0]" would be a compile of a version from the "5.0.2" branch before the official release.
*)
  FUNCTION al_get_allegro_version: DWORD; CDECL;



  TYPE
  (* Abstract type representing a bitmap (2D image). *)
    ALLEGRO_BITMAPptr = POINTER;



  (* Creates a new bitmap using the bitmap format and flags for the current thread. Blitting between bitmaps of differing formats, or blitting between memory bitmaps and display bitmaps may be slow.

  Unless you set the @link(ALLEGRO_MEMORY_BITMAP) flag, the bitmap is created for the current display. Blitting to another display may be slow.

  If a display bitmap is created, there may be limitations on the allowed dimensions. For example a DirectX or OpenGL backend usually has a maximum allowed texture size - so if bitmap creation fails for very large dimensions, you may want to re-try with a smaller bitmap. Some platforms also dictate a minimum texture size, which is relevant if you plan to use this bitmap with the primitives addon. If you try to create a bitmap smaller than this, this call will not fail but the returned bitmap will be a section of a larger bitmap with the minimum size. This minimum size is 16 by 16.

  Some platforms do not directly support display bitmaps whose dimensions are not powers of two. Allegro handles this by creating a larger bitmap that has dimensions that are powers of two and then returning a section of that bitmap with the dimensions you requested. This can be relevant if you plan to use this bitmap with the primitives addon but shouldn't be an issue otherwise.

  @seealso(al_set_new_bitmap_format) @seealso(al_set_new_bitmap_flags) @seealso(al_clone_bitmap) @seealso(al_create_sub_bitmap)
 *)
  FUNCTION al_create_bitmap (w, h: INTEGER): ALLEGRO_BITMAPptr; CDECL;

(* Destroys the given bitmap, freeing all resources used by it. Does nothing if given the @nil pointer. *)
  PROCEDURE al_destroy_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;

(* Loads an image file into an @link(ALLEGRO_BITMAPptr). The file type is determined by the extension.

   @bold(Note:) the core Allegro library does not support any image file formats by default. You must use the @bold(allegro_image) addon, or register your own format handler.
  @returns(@nil on error.)
  @seealso(al_load_bitmap_f) @seealso(al_register_bitmap_loader) @seealso(al_set_new_bitmap_format) @seealso(al_set_new_bitmap_flags) @seealso(al_init_image_addon)
 *)
  FUNCTION al_load_bitmap (CONST filename: PCHAR): ALLEGRO_BITMAPptr; CDECL;



{$include alkeyid.inc}



  TYPE
  (* An opaque type representing an open display or window. *)
    ALLEGRO_DISPLAYptr = POINTER;

  CONST
(* Possible bit combinations for the flags parameter of al_set_new_display_flags. *)
(* Use default values.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_DEFAULT                     = 0 SHL 0;
(* Prefer a windowed mode.

   Under multi-head X (not XRandR/TwinView), the use of more than one adapter is impossible due to bugs in X and GLX. @link(al_set_new_display_flags) will fail if more than one adapter is attempted to be used. *)
    ALLEGRO_WINDOWED                    = 1 SHL 0;
(* Prefer a fullscreen mode.

   Under X the use of more than one FULLSCREEN display when using multi-head X, or true Xinerama is not possible due to bugs in X and GLX, display creation will fail if more than one adapter is attempted to be used.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_FULLSCREEN                  = 1 SHL 1;
(* Require the driver to provide an initialized OpenGL context after returning successfully.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_OPENGL                      = 1 SHL 2;
(* Require the driver to do rendering with Direct3D and provide a Direct3D device.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_DIRECT3D_INTERNAL           = 1 SHL 3;
(* The display is resizable (only applicable if combined with @link(ALLEGRO_WINDOWED)).
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_RESIZABLE                   = 1 SHL 4;
(* Try to create a window without a frame (i.e. no border or titlebar). This usually does nothing for fullscreen modes, and even in windowed modes it depends on the underlying platform whether it is supported or not.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_NOFRAME                     = 1 SHL 5;
(* Let the display generate expose events.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_GENERATE_EXPOSE_EVENTS      = 1 SHL 6;
(* Require the driver to provide an initialized OpenGL context compatible with OpenGL version 3.0.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_OPENGL_3_0                  = 1 SHL 7;
(* If this flag is set, the OpenGL context created with @link(ALLEGRO_OPENGL_3_0) will be forward compatible @italic(only), meaning that all of the OpenGL API declared deprecated in OpenGL 3.0 will not be supported. Currently, a display created with this flag will not be compatible with Allegro drawing routines; the display option @link(ALLEGRO_COMPATIBLE_DISPLAY) will be set to false.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_OPENGL_FORWARD_COMPATIBLE   = 1 SHL 8;
(* Make the window span the entire screen. Unlike @link(ALLEGRO_FULLSCREEN) this will never attempt to modify the screen resolution. Instead the pixel dimensions of the created display will be the same as the desktop.

   The passed width and height are only used if the window is switched out of fullscreen mode later but will be ignored initially.

   Under Windows and X11 a fullscreen display created with this flag will behave differently from one created with the @code(ALLEGRO_FULLSCREEN) flag - even if the @code(ALLEGRO_FULLSCREEN) display is passed the desktop dimensions. The exact difference is platform dependent, but some things which may be different is how alt-tab works, how fast you can toggle between fullscreen/windowed mode or how additional monitors behave while your display is in fullscreen mode.

   Additionally under X, the use of more than one adapter in multi-head mode or with true Xinerama enabled is impossible due to bugs in X/GLX, creation will fail if more than one adapter is attempted to be used.
   @seealso(al_set_new_display_flags) *)
    ALLEGRO_FULLSCREEN_WINDOW           = 1 SHL 9;
(* ? *)
    ALLEGRO_MINIMIZED                   = 1 SHL 10;

  TYPE
(* Possible parameters for al_set_display_option.
 * Make sure to update ALLEGRO_EXTRA_DISPLAY_SETTINGS if you modify
 * anything here.
 *)
    ALLEGRO_DISPLAY_OPTIONS =  INTEGER;

  CONST
        ALLEGRO_RED_SIZE = 0;
        ALLEGRO_GREEN_SIZE = 1;
        ALLEGRO_BLUE_SIZE = 2;
        ALLEGRO_ALPHA_SIZE = 3;
        ALLEGRO_RED_SHIFT = 4;
        ALLEGRO_GREEN_SHIFT = 5;
        ALLEGRO_BLUE_SHIFT = 6;
        ALLEGRO_ALPHA_SHIFT = 7;
        ALLEGRO_ACC_RED_SIZE = 8;
        ALLEGRO_ACC_GREEN_SIZE = 9;
        ALLEGRO_ACC_BLUE_SIZE = 10;
        ALLEGRO_ACC_ALPHA_SIZE = 11;
        ALLEGRO_STEREO = 12;
        ALLEGRO_AUX_BUFFERS = 13;
        ALLEGRO_COLOR_SIZE = 14;
        ALLEGRO_DEPTH_SIZE = 15;
        ALLEGRO_STENCIL_SIZE = 16;
        ALLEGRO_SAMPLE_BUFFERS = 17;
        ALLEGRO_SAMPLES = 18;
        ALLEGRO_RENDER_METHOD = 19;
        ALLEGRO_FLOAT_COLOR = 20;
        ALLEGRO_FLOAT_DEPTH = 21;
        ALLEGRO_SINGLE_BUFFER = 22;
        ALLEGRO_SWAP_METHOD = 23;
        ALLEGRO_COMPATIBLE_DISPLAY = 24;
        ALLEGRO_UPDATE_DISPLAY_REGION = 25;
        ALLEGRO_VSYNC = 26;
        ALLEGRO_MAX_BITMAP_SIZE = 27;
        ALLEGRO_SUPPORT_NPOT_BITMAP = 28;
        ALLEGRO_CAN_DRAW_INTO_BITMAP = 29;
        ALLEGRO_SUPPORT_SEPARATE_ALPHA = 30;
        ALLEGRO_DISPLAY_OPTIONS_COUNT = 31;

    ALLEGRO_DONTCARE = 0;
    ALLEGRO_REQUIRE = 1;
    ALLEGRO_SUGGEST = 2;

  TYPE
    ALLEGRO_DISPLAY_ORIENTATION =  INTEGER;

   Const
        ALLEGRO_DISPLAY_ORIENTATION_0_DEGREES = 0;
        ALLEGRO_DISPLAY_ORIENTATION_90_DEGREES = 1;
        ALLEGRO_DISPLAY_ORIENTATION_180_DEGREES = 2;
        ALLEGRO_DISPLAY_ORIENTATION_270_DEGREES = 3;
        ALLEGRO_DISPLAY_ORIENTATION_FACE_UP = 4;
        ALLEGRO_DISPLAY_ORIENTATION_FACE_DOWN = 5;

  TYPE
  (* Used for display mode queries. Contains information about a supported fullscreen display mode.
     @seealso(al_get_display_mode) *)
    ALLEGRO_DISPLAY_MODE = RECORD
    (* Screen width. *)
      width : INTEGER;
    (* Screen height. *)
      height : INTEGER;
    (* The pixel format of the mode. *)
      format : INTEGER;
    (* The refresh rate of the mode.  It may be zero if unknown. *)
      refresh_rate : INTEGER;
    END;
  (* Describes a monitors size and position relative to other monitors. @code(x1), @code(y1) will be @code(0), @code(0) on the primary display. Other monitors can have negative values if they are to the left or above the primary display. *)
    ALLEGRO_MONITOR_INFO = RECORD
      x1 : INTEGER;
      y1 : INTEGER;
      x2 : INTEGER;
      y2 : INTEGER;
    END;

(* Sets various flags to be used when creating new displays on the calling thread.

   @param(flags is a bitfield containing any reasonable combination of the constants listed at the @bold(See also) section.) @seealso(ALLEGRO_DEFAULT) @seealso(ALLEGRO_WINDOWED) @seealso(ALLEGRO_FULLSCREEN) @seealso(ALLEGRO_OPENGL) @seealso(ALLEGRO_DIRECT3D_INTERNAL) @seealso(ALLEGRO_RESIZABLE) @seealso(ALLEGRO_NOFRAME) @seealso(ALLEGRO_GENERATE_EXPOSE_EVENTS) @seealso(ALLEGRO_OPENGL_3_0) @seealso(ALLEGRO_OPENGL_FORWARD_COMPATIBLE) @seealso(ALLEGRO_FULLSCREEN_WINDOW) @seealso(ALLEGRO_MINIMIZED)
   @seealso(al_set_new_display_option) @seealso(al_get_display_option)
 *)
  PROCEDURE al_set_new_display_flags (flags: INTEGER); CDECL;

  FUNCTION al_create_display (w, h: INTEGER): ALLEGRO_DISPLAYptr; CDECL;

  PROCEDURE al_destroy_display (display: ALLEGRO_DISPLAYptr); CDECL;

(* This function selects the bitmap to which all subsequent drawing operations in the calling thread will draw to. To return to drawing to a display, set the backbuffer of the display as the target bitmap, using @link(al_get_backbuffer). As a convenience, you may also use @link(al_set_target_backbuffer).

  Each video bitmap is tied to a display. When a video bitmap is set to as the target bitmap, the display that the bitmap belongs to is automatically made "current" for the calling thread (if it is not current already). Then drawing other bitmaps which are tied to the same display can be hardware accelerated.

  A single display cannot be current for multiple threads simultaneously. If you need to release a display, so it is not current for the calling thread, call @code(al_set_target_bitmap @(@nil@);)

  Setting a memory bitmap as the target bitmap will not change which display is current for the calling thread.

  @bold(OpenGL note:)

  Framebuffer objects (FBOs) allow OpenGL to directly draw to a bitmap, which is very fast. When using an OpenGL display, if all of the following conditions are met an FBO will be created for use with the bitmap:

  @unorderedList(
    @item(The @code(GL_EXT_framebuffer_object) OpenGL extension is available.)
    @item(The bitmap is not a memory bitmap.)
    @item(The bitmap is not currently locked.)
  )

  In Allegro 5.0.0, you had to be careful as an FBO would be kept around until the bitmap is destroyed or you explicitly called @link(al_remove_opengl_fbo) on the bitmap, wasting resources. In newer versions, FBOs will be freed automatically when the bitmap is no longer the target bitmap, unless you have called @link(al_get_opengl_fbo) to retrieve the FBO id.

  In the following example, no FBO will be created:

@longcode(#
lock := al_lock_bitmap (bitmap);
al_set_target_bitmap (bitmap);
al_put_pixel (x, y, color);
al_unlock_bitmap (bitmap);
#)

The above allows using @link(al_put_pixel) on a locked bitmap without creating an FBO.

In this example an FBO is created however:

@longcode(#
al_set_target_bitmap (bitmap);
al_draw_line (x1, y1, x2, y2, color, 0);
#)

  An OpenGL command will be used to directly draw the line into the bitmap's associated texture.

  @seealso(al_get_target_bitmap) @seealso(al_set_target_backbuffer)
 *)
  PROCEDURE al_set_target_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;

(* Same as @code(al_set_target_bitmap @(al_get_backbuffer @(display@)@);)
   @seealso(al_set_target_bitmap) @seealso(al_get_backbuffer)
 *)
  PROCEDURE al_set_target_backbuffer (display: ALLEGRO_DISPLAYptr); CDECL;

(* Return a special bitmap representing the back-buffer of the display.

   Care should be taken when using the backbuffer bitmap (and its sub-bitmaps) as the source bitmap (e.g as the bitmap argument to @link(al_draw_bitmap)). Only untransformed operations are hardware accelerated. This consists of @link(al_draw_bitmap) and @link(al_draw_bitmap_region) when the current transformation is the identity. If the tranformation is not the identity, or some other drawing operation is used, the call will be routed through the memory bitmap routines, which are slow. If you need those operations to be accelerated, then first copy a region of the backbuffer into a temporary bitmap (via the @link(al_draw_bitmap) and @link(al_draw_bitmap_region)), and then use that temporary bitmap as the source bitmap.
 *)
  FUNCTION al_get_backbuffer (display: ALLEGRO_DISPLAYptr): ALLEGRO_BITMAPptr; CDECL;

(* Return the target bitmap of the calling thread.
  @link(al_set_target_bitmap)
 *)
  FUNCTION al_get_target_bitmap: ALLEGRO_BITMAPptr; CDECL;

  PROCEDURE al_flip_display; CDECL;

  PROCEDURE al_set_new_display_option (option, value, importance: INTEGER); CDECL;

(* Return an extra display setting of the display.
   @seealso(al_set_new_display_option)
 *)
  FUNCTION al_get_display_option (display: ALLEGRO_DISPLAYptr; option: INTEGER): INTEGER; CDECL;

IMPLEMENTATION

  FUNCTION al_install_system (version: DWORD; atexit_ptr: POINTER): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_uninstall_system; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_init: BOOLEAN;
  BEGIN
    al_init := al_install_system (ALLEGRO_VERSION_INT, NIL);
  END;

  FUNCTION al_get_allegro_version: DWORD; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_bitmap (w, h: INTEGER): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_destroy_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_load_bitmap (CONST filename: PCHAR): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_bitmap_flags (flags: INTEGER); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_display_flags (flags: INTEGER); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_display (w, h: INTEGER): ALLEGRO_DISPLAYptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_destroy_display (display: ALLEGRO_DISPLAYptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_target_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_target_backbuffer (display: ALLEGRO_DISPLAYptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_backbuffer (display: ALLEGRO_DISPLAYptr): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_target_bitmap: ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_flip_display; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_display_option (option, value, importance: INTEGER); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_option (display: ALLEGRO_DISPLAYptr; option: INTEGER): INTEGER; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

FINALIZATION
  al_uninstall_system;
END.
