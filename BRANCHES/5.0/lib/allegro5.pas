UNIT Allegro5;
(*<Wrapper of the Allegro 5 core library. *)
(*TODO: License. *)

{$include allegro.cfg}

INTERFACE

  USES
    ctypes;

(* The code is distributed in sections.  Each section wraps with a header file (approx.). *)

(******************************************************************************
 * system.h *
 ************)

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
  FUNCTION al_install_system (version: DWORD; atexit_ptr: POINTER): BOOLEAN; CDECL;

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
  Major, Minor, Revision, Release: LONGINT;
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



(* Types "forwarded". *)
  TYPE
    ALLEGRO_EVENT_SOURCEptr = ^ALLEGRO_EVENT_SOURCE;
    ALLEGRO_EVENT_SOURCE = RECORD
      __pad : ARRAY [0..31] OF LONGINT;
    END;



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


(* Returns the width of a bitmap in pixels. *)
  FUNCTION al_get_bitmap_width (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;

(* Returns the height of a bitmap in pixels. *)
  FUNCTION al_get_bitmap_height (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;

(* Returns the pixel format of a bitmap.
   @seealso(ALLEGRO_PIXEL_FORMAT) @seealso(al_set_new_bitmap_flags) *)
  FUNCTION al_get_bitmap_format (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;

(* Return the flags user to create the bitmap.
  @seealso(al_set_new_bitmap_flags) *)
  FUNCTION al_get_bitmap_flags (bitmap: ALLEGRO_BITMAPptr): LONGINT; CDECL;

  (* Creates a new bitmap using the bitmap format and flags for the current thread. Blitting between bitmaps of differing formats, or blitting between memory bitmaps and display bitmaps may be slow.

  Unless you set the @link(ALLEGRO_MEMORY_BITMAP) flag, the bitmap is created for the current display. Blitting to another display may be slow.

  If a display bitmap is created, there may be limitations on the allowed dimensions. For example a DirectX or OpenGL backend usually has a maximum allowed texture size - so if bitmap creation fails for very large dimensions, you may want to re-try with a smaller bitmap. Some platforms also dictate a minimum texture size, which is relevant if you plan to use this bitmap with the primitives addon. If you try to create a bitmap smaller than this, this call will not fail but the returned bitmap will be a section of a larger bitmap with the minimum size. This minimum size is 16 by 16.

  Some platforms do not directly support display bitmaps whose dimensions are not powers of two. Allegro handles this by creating a larger bitmap that has dimensions that are powers of two and then returning a section of that bitmap with the dimensions you requested. This can be relevant if you plan to use this bitmap with the primitives addon but shouldn't be an issue otherwise.

  @seealso(al_set_new_bitmap_format) @seealso(al_set_new_bitmap_flags) @seealso(al_clone_bitmap) @seealso(al_create_sub_bitmap)
 *)
  FUNCTION al_create_bitmap (w, h: LONGINT): ALLEGRO_BITMAPptr; CDECL;

(* Destroys the given bitmap, freeing all resources used by it. Does nothing if given the @nil pointer. *)
  PROCEDURE al_destroy_bitmap (Bitmap: ALLEGRO_BITMAPptr); CDECL;

(* Draws an unscaled, unrotated bitmap at the given position to the current target bitmap (see @link(al_set_target_bitmap)).

   @bold(Note:) The current target bitmap must be a different bitmap. Drawing a bitmap to itself (or to a sub-bitmap of itself) or drawing a sub-bitmap to its parent (or another sub-bitmap of its parent) are not currently supported. To copy part of a bitmap into the same bitmap simply use a temporary bitmap instead.

   @bold(Note:) The backbuffer (or a sub-bitmap thereof) can not be transformed, blended or tinted. If you need to draw the backbuffer draw it to a temporary bitmap first with no active transformation (except translation). Blending and tinting settings/parameters will be ignored. This does not apply when drawing into a memory bitmap.
   @param(flags Can be a combination of:@definitionList(
     @itemLabel(@code(ALLEGRO_FLIP_HORIZONTAL))@item(flip the bitmap about the y-axis)
     @itemLabel(@code(ALLEGRO_FLIP_VERTICAL))@item(flip the bitmap about the x-axis)
   ))
   @param(dx destination x) @param(dy destination y)
   @seealso(al_draw_bitmap_region)@seealso(al_draw_scaled_bitmap)@seealso(al_draw_rotated_bitmap)@seealso(al_draw_scaled_rotated_bitmap) *)
  PROCEDURE al_draw_bitmap (bitmap: ALLEGRO_BITMAPptr; dx, dy: SINGLE; flags: LONGINT); CDECL;

(* Draws a region of the given bitmap to the target bitmap.
   @param(sx source x) @param(sy source y) @param(sw source width (width of region to blit)) @param(sh source height (height of region to blit)) @param(dx destination x) @param(dy destination y) @param(flags same as for @link(al_draw_bitmap))
   @seealso(al_draw_bitmap) @seealso(al_draw_scaled_bitmap) @seealso(al_draw_rotated_bitmap) @seealso(al_draw_scaled_rotated_bitmap) *)
  PROCEDURE al_draw_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy: SINGLE; flags: LONGINT); CDECL;

(* Draws a scaled version of the given bitmap to the target bitmap.
   @param(sx source x) @param(sy source y) @param(sw source width (width of region to blit)) @param(sh source height (height of region to blit)) @param(dx destination x) @param(dy destination y) @param(dw destination width) @param(dh destination height) @param(flags same as for @link(al_draw_bitmap))
   @seealso(al_draw_bitmap) @seealso(al_draw_bitmap_region) @seealso(al_draw_rotated_bitmap) @seealso(al_draw_scaled_rotated_bitmap) *)
  PROCEDURE al_draw_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy, dw, dh: SINGLE; flags: LONGINT); CDECL;

(* Draws a rotated version of the given bitmap to the target bitmap. The bitmap is rotated by @code(angle) radians clockwise.

   The point at @code(cx)/@code(cy) relative to the upper left corner of the bitmap will be drawn at @code(dx)/@code(dy) and the bitmap is rotated around this point. If @code(cx, cy) is @code(0, 0) the bitmap will rotate around its upper left corner.

   Example
@longcode(#
VAR
  w, h: SINGLE;
BEGIN
  w := al_get_bitmap_width (bitmap);
  h := al_get_bitmap_height (bitmap);
  al_draw_rotated_bitmap (bitmap, w / 2, h / 2, x, y, ALLEGRO_PI / 2, 0);
END;
#)

   The above code draws the bitmap centered on @code(cx)/@code(cy) and rotates it 90° clockwise.
   @param(cx center x (relative to the bitmap)) @param(cy center y (relative to the bitmap)) @param(dx destination x) @param(dy destination y) @param(angle angle by which to rotate) @param(flags same as for @link(al_draw_bitmap))
   @seealso(al_draw_bitmap) @seealso(al_draw_bitmap_region) @seealso(al_draw_scaled_bitmap) @seealso(al_draw_scaled_rotated_bitmap) *)
  PROCEDURE al_draw_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, angle: SINGLE; flags: LONGINT); CDECL;

(* Like @link(al_draw_rotated_bitmap), but can also scale the bitmap.

   The point at @code(cx)/@code(cy) in the bitmap will be drawn at @code(dx)/@code(dy) and the bitmap is rotated and scaled around this point.
   @param(cx center x (relative to the bitmap)) @param(cy center y (relative to the bitmap)) @param(dx destination x) @param(dy destination y) @param(xscale how much to scale on the x-axis (e.g. 2 for twice the size)) @param(yscale how much to scale on the y-axis) @param(angle angle by which to rotate) @param(flags same as for @link(al_draw_bitmap))
   @seealso(al_draw_bitmap) @seealso(al_draw_bitmap_region) @seealso(al_draw_scaled_bitmap) @seealso(al_draw_rotated_bitmap) *)
  PROCEDURE al_draw_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, xscale, yscale, angle: SINGLE; flags: LONGINT); CDECL;

(* Convert r, g, b (ranging from 0-255) into an @link(ALLEGRO_COLOR), using 255 for alpha.
   @seealso(al_map_rgba) @seealso(al_map_rgba_f) @seealso(al_map_rgb_f) *)
  FUNCTION al_map_rgb (r, g, b: BYTE): ALLEGRO_COLOR; CDECL;

(* Convert r, g, b (ranging from 0.0-1.0) into an @link(ALLEGRO_COLOR), using 1.0 for alpha.
   @seealso(al_map_rgb) @seealso(al_map_rgba_f) @seealso(al_map_rgb_f) *)
  FUNCTION al_map_rgba (r, g, b, a: BYTE): ALLEGRO_COLOR; CDECL;

(* Convert r, g, b, a (ranging from 0-255) into an @link(ALLEGRO_COLOR).
   @seealso(al_map_rgb) @seealso(al_map_rgba) @seealso(al_map_rgba_f) *)
  FUNCTION al_map_rgb_f (r, g, b: SINGLE): ALLEGRO_COLOR; CDECL;

(* Convert r, g, b, a (ranging from 0.0-1.0) into an @link(ALLEGRO_COLOR).
   @seealso(al_map_rgb) @seealso(al_map_rgba) @seealso(al_map_rgb_f) *)
  FUNCTION al_map_rgba_f (r, g, b, a: SINGLE): ALLEGRO_COLOR; CDECL;



(******************************************************************************
 * bitmap_io.h *
 ***************)

(* Loads an image file into an @link(ALLEGRO_BITMAPptr). The file type is determined by the extension.

   @bold(Note:) the core Allegro library does not support any image file formats by default. You must use the @bold(allegro_image) addon, or register your own format handler.
  @returns(@nil on error.)
  @seealso(al_load_bitmap_f) @seealso(al_register_bitmap_loader) @seealso(al_set_new_bitmap_format) @seealso(al_set_new_bitmap_flags) @seealso(al_init_image_addon)
 *)
  FUNCTION al_load_bitmap (CONST filename: PCHAR): ALLEGRO_BITMAPptr; CDECL;



(******************************************************************************
 * keyboard.h *
 **************)

  TYPE
    ALLEGRO_KEYBOARDptr = POINTER;

  FUNCTION al_install_keyboard: BOOLEAN; CDECL;

  FUNCTION al_get_keyboard_event_source: ALLEGRO_EVENT_SOURCEptr; CDECL;



(******************************************************************************
 * keycodes.h *
 **************)

{$include keycodes.inc}



(******************************************************************************
 * display.h *
 *************)

  TYPE
  (* An opaque type representing an open display or window.
     @seealso(al_set_new_display_flags)*)
    ALLEGRO_DISPLAYptr = POINTER;

  CONST
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_DEFAULT                     = 0 SHL 0;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_WINDOWED                    = 1 SHL 0;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_FULLSCREEN                  = 1 SHL 1;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_OPENGL                      = 1 SHL 2;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_DIRECT3D_INTERNAL           = 1 SHL 3;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_RESIZABLE                   = 1 SHL 4;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_NOFRAME                     = 1 SHL 5;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_GENERATE_EXPOSE_EVENTS      = 1 SHL 6;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_OPENGL_3_0                  = 1 SHL 7;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_OPENGL_FORWARD_COMPATIBLE   = 1 SHL 8;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_FULLSCREEN_WINDOW           = 1 SHL 9;
(* Possible bit combination for the @code(flags) parameter of @link(al_set_new_display_flags). *)
    ALLEGRO_MINIMIZED                   = 1 SHL 10;

  TYPE
(* Possible parameters for al_set_display_option.
 * Make sure to update ALLEGRO_EXTRA_DISPLAY_SETTINGS if you modify
 * anything here.
 *)
    ALLEGRO_DISPLAY_OPTIONS =  LONGINT;

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
    ALLEGRO_DISPLAY_ORIENTATION =  LONGINT;

  CONST
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
      width : LONGINT;
    (* Screen height. *)
      height : LONGINT;
    (* The pixel format of the mode. *)
      format : LONGINT;
    (* The refresh rate of the mode.  It may be zero if unknown. *)
      refresh_rate : LONGINT;
    END;
  (* Describes a monitors size and position relative to other monitors. @code(x1), @code(y1) will be @code(0), @code(0) on the primary display. Other monitors can have negative values if they are to the left or above the primary display. *)
    ALLEGRO_MONITOR_INFO = RECORD
      x1 : LONGINT;
      y1 : LONGINT;
      x2 : LONGINT;
      y2 : LONGINT;
    END;

(* Sets various flags to be used when creating new displays on the calling thread.

   @param(flags is a bitfield containing any reasonable combination of the next constants:@definitionList(
     @itemLabel(@code(ALLEGRO_DEFAULT))@item(Use default values.)
     @itemLabel(@code(ALLEGRO_WINDOWED))@item(Prefer a windowed mode.

   Under multi-head X (not XRandR/TwinView), the use of more than one adapter is impossible due to bugs in X and GLX. @code(al_set_new_display_flags) will fail if more than one adapter is attempted to be used. )
     @itemLabel(@code(ALLEGRO_FULLSCREEN))@item(Prefer a fullscreen mode.

   Under X the use of more than one FULLSCREEN display when using multi-head X, or true Xinerama is not possible due to bugs in X and GLX, display creation will fail if more than one adapter is attempted to be used.)
     @itemLabel(@code(ALLEGRO_OPENGL))@item(Require the driver to provide an initialized OpenGL context after returning successfully.)
     @itemLabel(@code(ALLEGRO_DIRECT3D_INTERNAL))@item(Require the driver to do rendering with Direct3D and provide a Direct3D device.)
     @itemLabel(@code(ALLEGRO_RESIZABLE))@item(The display is resizable (only applicable if combined with @code(ALLEGRO_WINDOWED)).)
     @itemLabel(@code(ALLEGRO_NOFRAME))@item(Try to create a window without a frame (i.e. no border or titlebar). This usually does nothing for fullscreen modes, and even in windowed modes it depends on the underlying platform whether it is supported or not.)
     @itemLabel(@code(ALLEGRO_GENERATE_EXPOSE_EVENTS))@item(Let the display generate expose events.)
     @itemLabel(@code(ALLEGRO_OPENGL_3_0))@item(Require the driver to provide an initialized OpenGL context compatible with OpenGL version 3.0.)
     @itemLabel(@code(ALLEGRO_OPENGL_FORWARD_COMPATIBLE))@item(If this flag is set, the OpenGL context created with @code(ALLEGRO_OPENGL_3_0) will be forward compatible @italic(only), meaning that all of the OpenGL API declared deprecated in OpenGL 3.0 will not be supported. Currently, a display created with this flag will not be compatible with Allegro drawing routines; the display option @code(ALLEGRO_COMPATIBLE_DISPLAY) will be set to false.)
     @itemLabel(@code(ALLEGRO_FULLSCREEN_WINDOW))@item(Make the window span the entire screen. Unlike @code(ALLEGRO_FULLSCREEN) this will never attempt to modify the screen resolution. Instead the pixel dimensions of the created display will be the same as the desktop.

   The passed width and height are only used if the window is switched out of fullscreen mode later but will be ignored initially.

   Under Windows and X11 a fullscreen display created with this flag will behave differently from one created with the @code(ALLEGRO_FULLSCREEN) flag - even if the @code(ALLEGRO_FULLSCREEN) display is passed the desktop dimensions. The exact difference is platform dependent, but some things which may be different is how alt-tab works, how fast you can toggle between fullscreen/windowed mode or how additional monitors behave while your display is in fullscreen mode.

   Additionally under X, the use of more than one adapter in multi-head mode or with true Xinerama enabled is impossible due to bugs in X/GLX, creation will fail if more than one adapter is attempted to be used.)
     @itemLabel(@code(ALLEGRO_MINIMIZED))@item(?)
   ))
   @seealso(al_set_new_display_option) @seealso(al_get_display_option)
 *)
  PROCEDURE al_set_new_display_flags (flags: LONGINT); CDECL;

(* Create a display, or window, with the specified dimensions. The parameters of the display are determined by the last calls to @code(al_set_new_display_* ). Default parameters are used if none are set explicitly. Creating a new display will automatically make it the active one, with the backbuffer selected for drawing.

   Each display has a distinct OpenGL rendering context associated with it. See @link(al_set_target_bitmap) for the discussion about rendering contexts.
   @returns(@nil on error.)
   @seealso(al_set_new_display_flags) @seealso(al_set_new_display_option) @seealso(al_set_new_display_refresh_rate) @seealso(al_set_new_display_adapter) @seealso(al_set_window_position) *)
  FUNCTION al_create_display (w, h: LONGINT): ALLEGRO_DISPLAYptr; CDECL;

(* Destroy a display.

   If the target bitmap of the calling thread is tied to the display, then it implies a call to @code(al_set_target_bitmap @(@nil@);) before the display is destroyed.

   That special case notwithstanding, you should make sure no threads are currently targeting a bitmap which is tied to the display before you destroy it.
   @seealso(al_set_target_bitmap) *)
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

(* Copies or updates the front and back buffers so that what has been drawn previously on the currently selected display becomes visible on screen. Pointers to the special back buffer bitmap remain valid and retain their semantics as the back buffer, although the contents may have changed.

  Several display options change how this function behaves:@unorderedList(
    @item(With @link(ALLEGRO_SINGLE_BUFFER), no flipping is done. You still have to call this function to display graphics, depending on how the used graphics system works.)
    @item(The @link(ALLEGRO_SWAP_METHOD) option may have additional information about what kind of operation is used internally to flip the front and back buffers.)
    @item(If @link(ALLEGRO_VSYNC) is 1, this function will force waiting for vsync. If @code(ALLEGRO_VSYNC) is 2, this function will not wait for vsync. With many drivers the vsync behavior is controlled by the user and not the application, and @code(ALLEGRO_VSYNC) will not be set; in this case @code(al_flip_display) will wait for vsync depending on the settings set in the system's graphics preferences.)
  )
  @seealso(al_set_new_display_flags) @seealso(al_set_new_display_option) *)
  PROCEDURE al_flip_display; CDECL;

  FUNCTION al_get_display_event_source (display: ALLEGRO_DISPLAYptr): ALLEGRO_EVENT_SOURCEptr; CDECL;

(* Set an extra display option, to be used when creating new displays on the calling thread. Display options differ from display flags, and specify some details of the context to be created within the window itself. These mainly have no effect on Allegro itself, but you may want to specify them, for example if you want to use multisampling.

  The 'importance' parameter can be either:@definitionList(
    @itemLabel(@code(ALLEGRO_REQUIRE))@item(The display will not be created if the setting can not be met.)
    @itemLabel(@code(ALLEGRO_SUGGEST))@item(If the setting is not available, the display will be created anyway. FIXME: We need a way to query the settings back from a created display.)
    @itemLabel(@code(ALLEGRO_DONTCARE))@item(If you added a display option with one of the above two settings before, it will be removed again. Else this does nothing.)
  )
  The supported options are:@definitionList(
    @itemLabel(@code(ALLEGRO_COLOR_SIZE))@item(This can be used to ask for a specific bit depth. For example to force a 16-bit framebuffer set this to 16.)
    @itemLabel(@code(ALLEGRO_RED_SIZE, ALLEGRO_GREEN_SIZE, ALLEGRO_BLUE_SIZE, ALLEGRO_ALPHA_SIZE))@item(Individual color component size in bits.)
    @itemLabel(@code(ALLEGRO_RED_SHIFT, ALLEGRO_GREEN_SHIFT, ALLEGRO_BLUE_SHIFT, ALLEGRO_ALPHA_SHIFT))@item(Together with the previous settings these can be used to specify the exact pixel layout the display should use. Normally there is no reason to use these.)
    @itemLabel(@code(ALLEGRO_ACC_RED_SIZE, ALLEGRO_ACC_GREEN_SIZE, ALLEGRO_ACC_BLUE_SIZE, ALLEGRO_ACC_ALPHA_SIZE))@item(This can be used to define the required accumulation buffer size.)
    @itemLabel(@code(ALLEGRO_STEREO))@item(Whether the display is a stereo display.)
    @itemLabel(@code(ALLEGRO_AUX_BUFFERS))@item(Number of auxiliary buffers the display should have.)
    @itemLabel(@code(ALLEGRO_DEPTH_SIZE))@item(How many depth buffer @(z-buffer@) bits to use.)
    @itemLabel(@code(ALLEGRO_STENCIL_SIZE))@item(How many bits to use for the stencil buffer.)
    @itemLabel(@code(ALLEGRO_SAMPLE_BUFFERS))@item(Whether to use multisampling @(1@) or not @(0@).)
    @itemLabel(@code(ALLEGRO_SAMPLES))@item(If the above is 1, the number of samples to use per pixel. Else 0.)
    @itemLabel(@code(ALLEGRO_RENDER_METHOD:))@item(0 if hardware acceleration is not used with this display.)
    @itemLabel(@code(ALLEGRO_FLOAT_COLOR))@item(Whether to use floating point color components.)
    @itemLabel(@code(ALLEGRO_FLOAT_DEPTH))@item(Whether to use a floating point depth buffer.)
    @itemLabel(@code(ALLEGRO_SINGLE_BUFFER))@item(Whether the display uses a single buffer @(1@) or another update method @(0@).)
    @itemLabel(@code(ALLEGRO_SWAP_METHOD))@item(If the above is 0, this is set to 1 to indicate the display is using a copying method to make the next buffer in the flip chain available, or to 2 to indicate a flipping or other method.)
    @itemLabel(@code(ALLEGRO_COMPATIBLE_DISPLAY))@item(Indicates if Allegro's graphics functions can use this display. If you request a display not useable by Allegro, you can still use for example OpenGL to draw graphics.)
    @itemLabel(@code(ALLEGRO_UPDATE_DISPLAY_REGION))@item(Set to 1 if the display is capable of updating just a region, and 0 if calling @link(al_update_display_region) is equivalent to @link(al_flip_display).)
    @itemLabel(@code(ALLEGRO_VSYNC))@item(Set to 1 to tell the driver to wait for vsync in al_flip_display, or to 2 to force vsync off. The default of 0 means that Allegro does not try to modify the vsync behavior so it may be on or off. Note that even in the case of 1 or 2 it is possible to override the vsync behavior in the graphics driver so you should not rely on it.)
    @itemLabel(@code(ALLEGRO_MAX_BITMAP_SIZE))@item(When queried this returns the maximum size @(width as well as height@) a bitmap can have for this display. Calls to al_create_bitmap or al_load_bitmap for bitmaps larger than this size will fail. It does not apply to memory bitmaps which always can have arbitrary size @(but are slow for drawing@).)
    @itemLabel(@code(ALLEGRO_SUPPORT_NPOT_BITMAP))@item(Set to 1 if textures used for bitmaps on this display can have a size which is not a power of two. This is mostly useful if you use Allegro to load textures as otherwise only power-of-two textures will be used internally as bitmap storage.)
    @itemLabel(@code(ALLEGRO_CAN_DRAW_INTO_BITMAP))@item(Set to 1 if you can use al_set_target_bitmap on bitmaps of this display to draw into them. If this is not the case software emulation will be used when drawing into display bitmaps @(which can be very slow@).)
    @itemLabel(@code(ALLEGRO_SUPPORT_SEPARATE_ALPHA))@item(This is set to 1 if the @link(al_set_separate_blender) function is supported. Otherwise the alpha parameters will be ignored.)
   )
   @seealso(al_set_new_display_flags) *)
  PROCEDURE al_set_new_display_option (option, value, importance: LONGINT); CDECL;

(* Return an extra display setting of the display.
   @seealso(al_set_new_display_option)
 *)
  FUNCTION al_get_display_option (display: ALLEGRO_DISPLAYptr; option: LONGINT): LONGINT; CDECL;



(******************************************************************************
 * joystick.h *
 ************)

  TYPE
    ALLEGRO_JOYSTICKptr = POINTER;



(******************************************************************************
 * mouse.h *
 ***********)

  TYPE
    ALLEGRO_MOUSEptr = POINTER;



(******************************************************************************
 * timer.h *
 ***********)

  TYPE
    ALLEGRO_TIMERptr = POINTER;
    ALLEGRO_TIMEOUTptr = POINTER;

  FUNCTION al_create_timer (speed_secs: SINGLE): ALLEGRO_TIMERptr; CDECL;

  PROCEDURE al_start_timer (timer: ALLEGRO_TIMERptr); CDECL;

  PROCEDURE al_stop_timer (timer: ALLEGRO_TIMERptr); CDECL;

  FUNCTION al_get_timer_event_source (timer: ALLEGRO_TIMERptr): ALLEGRO_EVENT_SOURCEptr; CDECL;



(******************************************************************************
 * events.h *
 ************)

  TYPE
    ALLEGRO_EVENT_TYPE = DWORD;

  CONST
    ALLEGRO_EVENT_JOYSTICK_AXIS = 1;
    ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN = 2;
    ALLEGRO_EVENT_JOYSTICK_BUTTON_UP = 3;
    ALLEGRO_EVENT_JOYSTICK_CONFIGURATION = 4;

    ALLEGRO_EVENT_KEY_DOWN = 10;
    ALLEGRO_EVENT_KEY_CHAR = 11;
    ALLEGRO_EVENT_KEY_UP = 12;

    ALLEGRO_EVENT_MOUSE_AXES = 20;
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN = 21;
    ALLEGRO_EVENT_MOUSE_BUTTON_UP = 22;
    ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY = 23;
    ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY = 24;
    ALLEGRO_EVENT_MOUSE_WARPED = 25;

    ALLEGRO_EVENT_TIMER = 30;

    ALLEGRO_EVENT_DISPLAY_EXPOSE = 40;
    ALLEGRO_EVENT_DISPLAY_RESIZE = 41;
    ALLEGRO_EVENT_DISPLAY_CLOSE = 42;
    ALLEGRO_EVENT_DISPLAY_LOST = 43;
    ALLEGRO_EVENT_DISPLAY_FOUND = 44;
    ALLEGRO_EVENT_DISPLAY_SWITCH_IN = 45;
    ALLEGRO_EVENT_DISPLAY_SWITCH_OUT = 46;
    ALLEGRO_EVENT_DISPLAY_ORIENTATION = 47;

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
      modifiers : DWORD;
      _repeat : BOOLEAN;
    END;

    ALLEGRO_MOUSE_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_MOUSEptr;
      timestamp : DOUBLE;
      display : ALLEGRO_DISPLAYptr;
      x : LONGINT;
      y : LONGINT;
      z : LONGINT;
      w : LONGINT;
      dx : LONGINT;
      dy : LONGINT;
      dz : LONGINT;
      dw : LONGINT;
      button : DWORD;
      pressure : SINGLE;
    END;

    ALLEGRO_TIMER_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_TIMERptr;
      timestamp : DOUBLE;
      count : cint64;
      error : DOUBLE;
    END;

    ALLEGRO_USER_EVENT_DESCRIPTORptr = POINTER;
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
	0 : ( _type : ALLEGRO_EVENT_TYPE );
	1 : ( any : ALLEGRO_ANY_EVENT );
	2 : ( display : ALLEGRO_DISPLAY_EVENT );
	3 : ( joystick : ALLEGRO_JOYSTICK_EVENT );
	4 : ( keyboard : ALLEGRO_KEYBOARD_EVENT );
	5 : ( mouse : ALLEGRO_MOUSE_EVENT );
	6 : ( timer : ALLEGRO_TIMER_EVENT );
	7 : ( user : ALLEGRO_USER_EVENT );
      END;

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

  PROCEDURE al_wait_for_event (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr); CDECL;

  FUNCTION al_wait_for_event_timed (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr; secs: SINGLE): BOOLEAN; CDECL;

  FUNCTION al_wait_for_event_until (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr; timeout: ALLEGRO_TIMEOUTptr): BOOLEAN; CDECL;

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

  FUNCTION al_map_rgb (r, g, b: BYTE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_map_rgba (r, g, b, a: BYTE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_map_rgb_f (r, g, b: SINGLE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_map_rgba_f (r, g, b, a: SINGLE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_install_keyboard: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_keyboard_event_source: ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_load_bitmap (CONST filename: PCHAR): ALLEGRO_BITMAPptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_bitmap_flags (flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_display_flags (flags: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_display (w, h: LONGINT): ALLEGRO_DISPLAYptr; CDECL;
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

  FUNCTION al_get_display_event_source (display: ALLEGRO_DISPLAYptr): ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_display_option (option, value, importance: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_option (display: ALLEGRO_DISPLAYptr; option: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_timer (speed_secs: SINGLE): ALLEGRO_TIMERptr; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_start_timer (timer: ALLEGRO_TIMERptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_stop_timer (timer: ALLEGRO_TIMERptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_timer_event_source (timer: ALLEGRO_TIMERptr): ALLEGRO_EVENT_SOURCEptr; CDECL;
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

  PROCEDURE al_wait_for_event (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_wait_for_event_timed (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr; secs: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_wait_for_event_until (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr; timeout: ALLEGRO_TIMEOUTptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

FINALIZATION
WriteLn ('We''re in the Allegro5 utin FINALIZATION section, going to call "al_uninstall_system".');
  al_uninstall_system;
WriteLn ('Closed.');
END.
