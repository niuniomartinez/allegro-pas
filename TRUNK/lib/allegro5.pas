UNIT Allegro5;
(*<Wrapper of the Allegro 5 core library.

  This unit defines core functions, procedures and data types, that aren't in
  add-ons. *)
(* Copyright (c) 2012-2016 Guillermo Martínez J. <niunio@users.sourceforge.net>

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{ There are a lot of unfinished stuff.  Some times I've wrote that "I'll do"
   something.  In most cases "I" means "me or somebody else".  Since this is
   free open source you (or anybody) can add or improve this unit in any way.
   If you do it, please let me know and I would add it to the next release! }

{$include allegro5.cfg}

INTERFACE

  USES
    al5base;

  CONST
  (* @exclude Builds library name. *)
    ALLEGRO_LIB_NAME = _A5_LIB_PREFIX_+'allegro'+_DBG_+_A5_LIB_EXT_;

{ The code is distributed in sections, each one wraps a header file.

  Order of sections is the same as C loads them by including the "allegro.h"
  header file. }

(*
 * base.h
 *      Defines basic stuff needed by pretty much everything else.
 *
 *      By Shawn Hargreaves.
 *****************************************************************************)

  CONST
  (* Major version of Allegro. *)
    ALLEGRO_VERSION      =   5;
  (* Minor version of Allegro. *)
    ALLEGRO_SUB_VERSION  =   2;
  (* Revision number of Allegro. *)
    ALLEGRO_WIP_VERSION  =   0;
  (* Not sure we need it, but ALLEGRO_VERSION_STR contains it:
     0 = SVN
     1 = first release
     2... = hotfixes?

     Note x.y.z (= x.y.z.0) has release number 1, and x.y.z.1 has release
     number 2, just to confuse you. *)
    ALLEGRO_RELEASE_NUMBER = 1;
  (* Packs version number in a simple AL_INT number. *)
    ALLEGRO_VERSION_INT  = (
	   (ALLEGRO_VERSION SHL 24)
	OR (ALLEGRO_SUB_VERSION SHL 16)
	OR (ALLEGRO_WIP_VERSION SHL  8)
	OR  ALLEGRO_RELEASE_NUMBER
    );
  (* Just to be sure that PI number is available. *)
    ALLEGRO_PI = 3.14159265358979323846;

  TYPE
  (* Description of user main function for @link(al_run_main). *)
    ALLEGRO_USER_MAIN = FUNCTION (argc: AL_INT; argv: AL_POINTER): AL_INT; CDECL;

  (* Returns the (compiled) version of the Allegro library, packed into a
     single integer as groups of 8 bits.

     You can use code like this to extract the version number:
@longcode(#
  VAR
    Version: AL_INT;
    Major, Minor, Revision, Release: INTEGER;
    VersionStr: STRING;
  BEGIN
    Version := al_get_allegro_version;
    Major    :=  Version SHR 24;
    Minor    := (Version SHR 16) AND 255;
    Revision := (Version SHR  8) AND 255;
    Release  :=  Version         AND 255;
    VersionStr := Format ('%d.%d.%d(%d)', [Major, Minor, Revision, Release])
  END;
#)
    The release number is 0 for an unofficial version and 1 or greater for an
    official release. For example "5.0.2[1]” would be the (first) official
    5.0.2 release while “5.0.2[0]” would be a compile of a version from the
    “5.0.2” branch before the official release.
 *)
  FUNCTION al_get_allegro_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* This function is useful in cases where you don’t have a @code(main) function
   but want to run Allegro (mostly useful in a wrapper library).  Under Windows
   and Linux this is no problem because you simply can call
   @link(al_install_system).  But some other system (like OSX) don’t allow
   calling @code(al_install_system) in the main thread.  @code(al_run_main)
   will know what to do in that case.

   The passed @code(argc) and @code(argv) will simply be passed on to
   @code(user_main) and the return value of @code(user_main) will be
   returned.

   @bold(Note:)  This is used because the way the C language works.  I didn't
   test if Pascal do need this kind of stuff.  Future versions of Allegro.pas
   would not include this function, so don't use it unless your really need to
   (and tell me if you really need it to remove this warning from
     documentation). *)
  FUNCTION al_run_main (argc: AL_INT; argv: AL_POINTER; user_main: ALLEGRO_USER_MAIN): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



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
  FUNCTION AL_ID (CONST str: SHORTSTRING): AL_INT;



(*
 * altime.h
 *****************************************************************************)

  TYPE
  (* Represent a timeout value.

     The size of the structure is known so it can be statically allocated.  The
     contents are private.
     @seealso(al_init_timeout) *)
    ALLEGRO_TIMEOUT = RECORD
      __pad1__, __pad2__: AL_UINT64;
    END;


  (* Return the number of seconds since the Allegro library was initialised.
     The return value is undefined if Allegro is uninitialised. The resolution
     depends on the used driver, but typically can be in the order of
     microseconds. *)
    FUNCTION al_get_time: AL_DOUBLE;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  (* Waits for the specified number of seconds.  This tells the system to pause
     the current thread for the given amount of time.  With some operating
     systems, the accuracy can be in the order of 10ms.  That is, even
@longcode(#
    al_rest(0.000001)
#)
     might pause for something like 10ms.  Also see the section on timer events
     (i.e. @link(al_create_timer)) for easier ways to time your program
     without using up all CPU. *)
    PROCEDURE al_rest (seconds: AL_DOUBLE);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  (* Set timeout value of some number of seconds after the function call.

     For compatibility with all platforms, seconds must be 2,147,483.647
     seconds or less.
     @seealso(ALLEGRO_TIMEOUT) @seealso(al_wait_for_event_until) *)
    PROCEDURE al_init_timeout (OUT timeout: ALLEGRO_TIMEOUT; seconds: AL_DOUBLE);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * color.h
 *****************************************************************************)

  TYPE
  (* An @code(ALLEGRO_COLOR) structure describes a color in a device independant
     way.  Use @link(al_map_rgb) et al. and @link(al_unmap_rgb) et al. to
     translate from and to various color representations. *)
    ALLEGRO_COLOR = RECORD
    (* Color component. *)
      r, g, b, a: AL_FLOAT;
    END;

  (* Pixel formats.  Each pixel format specifies the exact size and bit layout
     of a pixel in memory.  Components are specified from high bits to low
     bits, so for example a fully opaque red pixel in ARGB_8888 format is
     0xFFFF0000.

    @bold(Note:)
    The pixel format is independent of endianness.  That is, in the above
    example you can always get the red component with

    @code(@(pixel AND $00ff0000@) SHR 16)

    But you can not rely on this code:

    @code(@(PBYTE @(pixel + 2@)@)^)

    It will return the red component on little endian systems, but the green
    component on big endian systems.

    Also note that Allegro’s naming is different from OpenGL naming here, where
    a format of @code(GL_RGBA8) merely defines the component order and the
    exact layout including endianness treatment is specified separately.
    Usually @code(GL_RGBA8) will correspond to @code(ALLEGRO_PIXEL_ABGR_8888)
    though on little endian systems, so care must be taken (note the reversal
    of RGBA <-> ABGR).

    The only exception to this @code(ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE) which
    will always have the components as 4 bytes corresponding to red, green,
    blue and alpha, in this order, independent of the endianness.
    @seealso(al_set_new_bitmap_format) @seealso(al_get_bitmap_format) *)
    ALLEGRO_PIXEL_FORMAT = (
    (* Let the driver choose a format. This is the default format at program
       start. *)
      ALLEGRO_PIXEL_FORMAT_ANY = 0,
    (* Let the driver choose a format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_NO_ALPHA = 1,
    (* Let the driver choose a format with alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_WITH_ALPHA = 2,
    (* Let the driver choose a 15 bit format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_15_NO_ALPHA = 3,
    (* Let the driver choose a 16 bit format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_16_NO_ALPHA = 4,
    (* Let the driver choose a 16 bit format with alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_16_WITH_ALPHA = 5,
    (* Let the driver choose a 24 bit format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_24_NO_ALPHA = 6,
    (* Let the driver choose a 32 bit format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_32_NO_ALPHA = 7,
    (* Let the driver choose a 32 bit format with alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA = 8,
    (* 32 bit *)
      ALLEGRO_PIXEL_FORMAT_ARGB_8888 = 9,
    (* 32 bit *)
      ALLEGRO_PIXEL_FORMAT_RGBA_8888 = 10,
    (* 24 bit *)
      ALLEGRO_PIXEL_FORMAT_ARGB_4444 = 11,
    (* 24 bit *)
      ALLEGRO_PIXEL_FORMAT_RGB_888 = 12,
    (* 16 bit *)
      ALLEGRO_PIXEL_FORMAT_RGB_565 = 13,
    (* 15 bit *)
      ALLEGRO_PIXEL_FORMAT_RGB_555 = 14,
    (* 16 bit *)
      ALLEGRO_PIXEL_FORMAT_RGBA_5551 = 15,
    (* 16 bit *)
      ALLEGRO_PIXEL_FORMAT_ARGB_1555 = 16,
    (* 32 bit *)
      ALLEGRO_PIXEL_FORMAT_ABGR_8888 = 17,
    (* 32 bit *)
      ALLEGRO_PIXEL_FORMAT_XBGR_8888 = 18,
    (* 24 bit *)
      ALLEGRO_PIXEL_FORMAT_BGR_888 = 19,
    (* 16 bit *)
      ALLEGRO_PIXEL_FORMAT_BGR_565 = 20,
    (* 15 bit *)
      ALLEGRO_PIXEL_FORMAT_BGR_555 = 21,
    (* 32 bit *)
      ALLEGRO_PIXEL_FORMAT_RGBX_8888 = 22,
    (* 32 bit *)
      ALLEGRO_PIXEL_FORMAT_XRGB_8888 = 23,
    (* 128 bit *)
      ALLEGRO_PIXEL_FORMAT_ABGR_F32 = 24,
    (* Like the version without _LE, but the component order is guaranteed to
       be red, green, blue, alpha. This only makes a difference on big endian
       systems, on little endian it is just an alias. *)
      ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE = 25,
    (* 16bit *)
      ALLEGRO_PIXEL_FORMAT_RGBA_4444  = 26,
      ALLEGRO_PIXEL_FORMAT_SINGLE_CHANNEL_8 = 27,
      ALLEGRO_PIXEL_FORMAT_COMPRESSED_RGBA_DXT1 = 28,
      ALLEGRO_PIXEL_FORMAT_COMPRESSED_RGBA_DXT3 = 29,
      ALLEGRO_PIXEL_FORMAT_COMPRESSED_RGBA_DXT5 = 30,
      ALLEGRO_NUM_PIXEL_FORMATS
    );

(* Convert r, g, b (ranging from 0-255) into an @link(ALLEGRO_COLOR), using 255
   for alpha.
   @seealso(al_map_rgba) @seealso(al_map_rgba_f) @seealso(al_map_rgb_f) *)
  FUNCTION al_map_rgb (r, g, b: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Convert r, g, b, a (ranging from 0-255) into an @link(ALLEGRO_COLOR).
   @seealso(al_map_rgb) @seealso(al_map_rgba_f) @seealso(al_map_rgb_f) *)
  FUNCTION al_map_rgba (r, g, b, a: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Convert r, g, b (ranging from 0.0f-1.0f) into an @link(ALLEGRO_COLOR), using
   1.0f for alpha.
   @seealso(al_map_rgba) @seealso(al_map_rgba_f) @seealso(al_map_rgb) *)
  FUNCTION al_map_rgb_f (r, g, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Convert r, g, b, a (ranging from 0.0f-1.0f) into an @link(ALLEGRO_COLOR).
   @seealso(al_map_rgba) @seealso(al_map_rgba_f) @seealso(al_map_rgb) *)
  FUNCTION al_map_rgba_f (r, g, b, a: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* This is a shortcut for
   @code(al_map_rgba @(r * a / 255, g * a / 255, b * a / 255, a@)).

   By default Allegro uses pre-multiplied alpha for transparent blending of
   bitmaps and primitives (see @link(al_load_bitmap_flags) for a discussion of
   that feature). This means that if you want to tint a bitmap or primitive to
   be transparent you need to multiply the color components by the alpha
   components when you pass them to this function. For example, to draw the
   bitmap tinted red and half-transparent.

@longcode(#
VAR
  c: ALLEGRO_COLOR;
  bmp: ALLEGRO_BITMAPptr;
BEGIN
  c := al_premul_rgba (255, 0, 0, 127);
  al_draw_tinted_bitmap (bmp, c, 0, 0, 0);
END;
#)
  @seealso(al_map_rgba) @seealso(al_premul_rgba_f) *)
  FUNCTION al_premul_rgba (r, g, b: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* This is a shortcut for @code(al_map_rgba @(r * a, g * a, b * a, a@)).

   By default Allegro uses pre-multiplied alpha for transparent blending of
   bitmaps and primitives (see @link(al_load_bitmap_flags) for a discussion of
   that feature). This means that if you want to tint a bitmap or primitive to
   be transparent you need to multiply the color components by the alpha
   components when you pass them to this function. For example, to draw the
   bitmap tinted red and half-transparent.

@longcode(#
VAR
  c: ALLEGRO_COLOR;
  bmp: ALLEGRO_BITMAPptr;
BEGIN
  c := al_premul_rgba_f (1, 0, 0, 0.5);
  al_draw_tinted_bitmap (bmp, c, 0, 0, 0);
END;
#)
  @seealso(al_map_rgba_f) @seealso(al_premul_rgba) *)
  FUNCTION al_premul_rgba_f (r, g, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Retrieves components of an @link(ALLEGRO_COLOR), ignoring alpha.  Components
   will range from 0-255.
   @seealso(al_unmap_rgba) @seealso(al_unmap_rgba_f) @seealso(al_unmap_rgb_f) *)
  PROCEDURE al_unmap_rgb (color: ALLEGRO_COLOR; OUT r, g, b: AL_UCHAR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Retrieves components of an @link(ALLEGRO_COLOR).  Components will range from
   0-255.
   @seealso(al_unmap_rgba) @seealso(al_unmap_rgba_f) @seealso(al_unmap_rgb_f) *)
  PROCEDURE al_unmap_rgba (color: ALLEGRO_COLOR; OUT r, g, b, a: AL_UCHAR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Retrieves components of an @link(ALLEGRO_COLOR), ignoring alpha.  Components
   will range from 0.0f-1.0f.
   @seealso(al_unmap_rgba) @seealso(al_unmap_rgba_f) @seealso(al_unmap_rgb_f) *)
  PROCEDURE al_unmap_rgb_f (color: ALLEGRO_COLOR; OUT r, g, b: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Retrieves components of an @link(ALLEGRO_COLOR).  Components will range from
   0.0f-1.0f.
   @seealso(al_unmap_rgba) @seealso(al_unmap_rgba_f) @seealso(al_unmap_rgb_f) *)
  PROCEDURE al_unmap_rgba_f (color: ALLEGRO_COLOR; OUT r, g, b, a: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Return the number of bytes that a pixel of the given format occupies.  For
   blocked pixel formats (e.g. compressed formats), this returns 0.
   @seealso(ALLEGRO_PIXEL_FORMAT) @seealso(al_get_pixel_format_bits) *)
  FUNCTION al_get_pixel_size (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Return the number of bits that a pixel of the given format occupies.  For
   blocked pixel formats (e.g. compressed formats), this returns 0.
   @seealso(ALLEGRO_PIXEL_FORMAT) @seealso(al_get_pixel_size) *)
  FUNCTION al_get_pixel_format_bits (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Return the number of bytes that a block of pixels with this format occupies.
   @seealso(ALLEGRO_PIXEL_FORMAT)
   @seealso(al_get_pixel_block_width) @seealso(al_get_pixel_block_height) *)
  FUNCTION al_get_pixel_block_size (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Return the width of the pixel block of this format.
   @seealso(ALLEGRO_PIXEL_FORMAT)
   @seealso(al_get_pixel_block_size) @seealso(al_get_pixel_block_height) *)
  FUNCTION al_get_pixel_block_width (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Return the height of the pixel block of this format.
   @seealso(ALLEGRO_PIXEL_FORMAT)
   @seealso(al_get_pixel_block_width) @seealso(al_get_pixel_block_size) *)
  FUNCTION al_get_pixel_block_height (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * bitmap.h
 *****************************************************************************)

  TYPE
  (* Abstract type representing a bitmap (2D image). *)
    ALLEGRO_BITMAPptr = AL_POINTER;

  CONST
  (* Bitmap flags.  Documented at al_set_new_bitmap_flags. *)
  { @exclude }
    ALLEGRO_MEMORY_BITMAP            = $0001;
  { @exclude }
    _ALLEGRO_KEEP_BITMAP_FORMAT      = $0002;
  { @exclude }
    ALLEGRO_FORCE_LOCKING            = $0004;
  { @exclude }
    ALLEGRO_NO_PRESERVE_TEXTURE      = $0008;
  { @exclude }
    _ALLEGRO_ALPHA_TEST              = $0010;
  { @exclude }
    _ALLEGRO_INTERNAL_OPENGL         = $0020;
  { @exclude }
    ALLEGRO_MIN_LINEAR               = $0040;
  { @exclude }
    ALLEGRO_MAG_LINEAR               = $0080;
  { @exclude }
    ALLEGRO_MIPMAP                   = $0100;
  { @exclude }
    _ALLEGRO_NO_PREMULTIPLIED_ALPHA  = $0200;
  { @exclude }
    ALLEGRO_VIDEO_BITMAP             = $0400;

(* Sets the pixel format for newly created bitmaps.  The default format is
   @code(ALLEGRO_PIXEL_FORMAT_ANY) and means the display driver will choose the
   best format.
   @seealso(ALLEGRO_PIXEL_FORMAT) @seealso(al_get_new_bitmap_format)
   @seealso(al_get_bitmap_format) *)
  PROCEDURE al_set_new_bitmap_format (format: ALLEGRO_PIXEL_FORMAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Sets the flags to use for newly created bitmaps. Valid flags are:
   @unorderedlist(
     @item(@bold(ALLEGRO_VIDEO_BITMAP) Creates a bitmap that resides in the video card memory. These types of bitmaps receive the greatest benefit from hardware acceleration. @link(al_set_new_bitmap_flags) will implicitly set this flag unless @code(ALLEGRO_MEMORY_BITMAP) is present.)
     @item(@bold(ALLEGRO_MEMORY_BITMAP) Create a bitmap residing in system memory. Operations on, and with, memory bitmaps will not be hardware accelerated. However, direct pixel access can be relatively quick compared to video bitmaps, which depend on the display driver in use. @italic(Note: Allegro’s software rendering routines are currently very unoptimised.))
     @item(@bold(ALLEGRO_KEEP_BITMAP_FORMAT) Only used when loading bitmaps from disk files, forces the resulting ALLEGRO_BITMAP to use the same format as the file. @italic(This is not yet honoured.))
     @item(@bold(ALLEGRO_FORCE_LOCKING) When drawing to a bitmap with this flag set, always use pixel locking and draw to it using Allegro’s software drawing primitives. This should never be used if you plan to draw to the bitmap using Allegro’s graphics primitives as it would cause severe performance penalties. However if you know that the bitmap will only ever be accessed by locking it, no unneeded FBOs will be created for it in the OpenGL drivers.)
     @item(@bold(ALLEGRO_NO_PRESERVE_TEXTURE) Normally, every effort is taken to preserve the contents of bitmaps, since Direct3D may forget them. This can take extra processing time. If you know it doesn’t matter if a bitmap keeps its pixel data, for example its a temporary buffer, use this flag to tell Allegro not to attempt to preserve its contents. This can increase performance of your game or application, but there is a catch. See ALLEGRO_EVENT_DISPLAY_LOST for further information.)
     @item(@bold(ALLEGRO_ALPHA_TEST) This is a driver hint only. It tells the graphics driver to do alpha testing instead of alpha blending on bitmaps created with this flag. Alpha testing is usually faster and preferred if your bitmaps have only one level of alpha @(0@). This flag is currently not widely implemented @(i.e., only for memory bitmaps@).)
     @item(@bold(ALLEGRO_MIN_LINEAR) When drawing a scaled down version of the bitmap, use linear filtering. This usually looks better. You can also combine it with the MIPMAP flag for even better quality.)
     @item(@bold(ALLEGRO_MAG_LINEAR) When drawing a magnified version of a bitmap, use linear filtering. This will cause the picture to get blurry instead of creating a big rectangle for each pixel. It depends on how you want things to look like whether you want to use this or not.)
     @item(@bold(ALLEGRO_MIPMAP) This can only be used for bitmaps whose width and height is a power of two. In that case, it will generate mipmaps and use them when drawing scaled down versions. For example if the bitmap is 64x64, then extra bitmaps of sizes 32x32, 16x16, 8x8, 4x4, 2x2 and 1x1 will be created always containing a scaled down version of the original.)
     @item(@bold(ALLEGRO_NO_PREMULTIPLIED_ALPHA) By default, Allegro pre-multiplies the alpha channel of an image with the images color data when it loads it. Typically that would look something like this:
@longcode(#
  r := get_float_byte ();
  g := get_float_byte ();
  b := get_float_byte ();
  a := get_float_byte ();

  r := r * a;
  g := g * a;
  b := b * a;

  set_image_pixel (x, y, r, g, b, a);
#)
  The reason for this can be seen in the Allegro example ex_premulalpha, ie,
  using pre-multiplied alpha gives more accurate color results in some cases.
  To use alpha blending with images loaded with pre-multiplied alpha, you would
  use the default blending mode, which is set with @code(al_set_blender
  @(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA@)) to set the correct
  blender.  This has some caveats.  First, as mentioned above, drawing such an
  image can result in less accurate color blending @(when drawing an image with
  linear filtering on, the edges will be darker than they should be@).  Second,
  the behaviour is somewhat confusing, which is explained in the example below.
@longcode(#
// Load and create bitmaps with an alpha channel
  al_set_new_bitmap_format (ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA);
// Load some bitmap with alpha in it
  bmp = al_load_bitmap ('some_alpha_bitmap.png');
// We will draw to this buffer and then draw this buffer to the screen
  tmp_buffer = al_create_bitmap (SCREEN_W, SCREEN_H);
// Set the buffer as the target and clear it
  al_set_target_bitmap (tmp_buffer);
  al_clear_to_color (al_map_rgba_f (0, 0, 0, 1));
// Draw the bitmap to the temporary buffer
  al_draw_bitmap (bmp, 0, 0, 0);
// Finally, draw the buffer to the screen
// The output will look incorrect (may take close inspection
// depending on the bitmap -- it may also be very obvious)
  al_set_target_bitmap (al_get_backbuffer (display));
  al_draw_bitmap (tmp_buffer, 0, 0, 0);
#)
     )
   )
   To explain further, if you have a pixel with 0.5 alpha, and you’re using
   (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA) for blending, the
   formula is:
@longcode(#
  a := da * dst + sa * src
#)
Expands to:
@longcode(#
  result_a := dst_a * (1-0.5) + 0.5 * 0.5;
#)
   So if you draw the image to the temporary buffer, it is blended once
   resulting in 0.75 alpha, then drawn again to the screen, blended in the
   same way, resulting in a pixel has 0.1875 as an alpha value.
   @seealso(al_get_new_bitmap_flags) @seealso(al_get_bitmap_flags) *)
  PROCEDURE al_set_new_bitmap_flags (flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the format used for newly created bitmaps.
   @seealso(ALLEGRO_PIXEL_FORMAT) @seealso(al_set_new_bitmap_format) *)
  FUNCTION al_get_new_bitmap_format: ALLEGRO_PIXEL_FORMAT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the flags used for newly created bitmaps.
   @seealso(al_set_new_bitmap_flags) *)
  FUNCTION al_get_new_bitmap_flags: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* A convenience function which does the same as @longcode(#
  al_set_new_bitmap_flags (al_get_new_bitmap_flags OR flag);
#)
   @seealso(al_set_new_bitmap_flags) @seealso(al_get_new_bitmap_flags)
   @seealso(al_get_bitmap_flags) *)
  PROCEDURE al_add_new_bitmap_flag (flag: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Returns the width of a bitmap in pixels. *)
  FUNCTION al_get_bitmap_width (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the height of a bitmap in pixels. *)
  FUNCTION al_get_bitmap_height (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the pixel format of a bitmap.
   @seealso(ALLEGRO_PIXEL_FORMAT) @seealso(al_set_new_bitmap_flags) *)
  FUNCTION al_get_bitmap_format (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_PIXEL_FORMAT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the flags user to create the bitmap.
   @seealso(al_set_new_bitmap_flags) *)
  FUNCTION al_get_bitmap_flags (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Creates a new bitmap using the bitmap format and flags for the current
   thread. Blitting between bitmaps of differing formats, or blitting between
   memory bitmaps and display bitmaps may be slow.

   Unless you set the ALLEGRO_MEMORY_BITMAP flag, the bitmap is created for the
   current display.  Blitting to another display may be slow.

   If a display bitmap is created, there may be limitations on the allowed
   dimensions. For example a DirectX or OpenGL backend usually has a maximum
   allowed texture size - so if bitmap creation fails for very large
   dimensions, you may want to re-try with a smaller bitmap. Some platforms
   also dictate a minimum texture size, which is relevant if you plan to use
   this bitmap with the primitives addon. If you try to create a bitmap smaller
   than this, this call will not fail but the returned bitmap will be a section
   of a larger bitmap with the minimum size. This minimum size is 16 by 16.

   Some platforms do not directly support display bitmaps whose dimensions are
   not powers of two. Allegro handles this by creating a larger bitmap that has
   dimensions that are powers of two and then returning a section of that
   bitmap with the dimensions you requested. This can be relevant if you plan
   to use this bitmap with the primitives addon but shouldn’t be an issue
   otherwise.
   @seealso(al_set_new_bitmap_format) @seealso(al_set_new_bitmap_flags)
   @seealso(al_clone_bitmap) @seealso(al_create_sub_bitmap) *)
  FUNCTION al_create_bitmap (w, h: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Destroys the given bitmap, freeing all resources used by it. Does nothing if
   given the null pointer. *)
  PROCEDURE al_destroy_bitmap (Bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Draw a single pixel on the target bitmap.  This operation is slow on
   non-memory bitmaps. Consider locking the bitmap if you are going to use this
   function multiple times on the same bitmap.  This function is not affected
   by the transformations or the color blenders.
   @seealso(al_get_pixel) @seealso(al_put_blended_pixel)
   @seealso(al_lock_bitmap) *)
  PROCEDURE al_put_pixel (x, y: AL_INT; color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_put_pixel), but the pixel color is blended using the current
   blenders before being drawn. *)
  PROCEDURE al_put_blended_pixel (x, y: AL_INT; color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Get a pixel's color value from the specified bitmap.  This operation is slow
   on non-memory bitmaps.  Consider locking the bitmap if you are going to use
   this function multiple times on the same bitmap.
   @seealso(al_put_pixel) @seealso(al_lock_bitmap) *)
  FUNCTION al_get_pixel (bitmap: ALLEGRO_BITMAPptr; x, y: AL_INT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Convert the given mask color to an alpha channel in the bitmap.  Can be used
   to convert older 4.2-style bitmaps with magic pink to alpha-ready bitmaps. *)
  PROCEDURE al_convert_mask_to_alpha (bitmap: ALLEGRO_BITMAPptr; mask_color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Set the region of the target bitmap or display that pixels get clipped to.
   The default is to clip pixels to the entire bitmap.
  @seealso(al_get_clipping_rectangle) @seealso(al_reset_clipping_rectangle) *)
  PROCEDURE al_set_clipping_rectangle (x, y, width, height: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Equivalent to calling @code(al_set_clipping_rectangle @(0, 0, w, h@))' where
   w and h are the width and height of the target bitmap respectively.

   Does nothing if there is no target bitmap.
   @seealso(al_set_clipping_rectangle) *)
  PROCEDURE al_reset_clipping_rectangle;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Gets the clipping rectangle of the target bitmap.
  @seealso(al_set_clipping_rectangle) *)
  PROCEDURE al_get_clipping_rectangle (OUT x, y, w, h: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Creates a sub-bitmap of the parent, at the specified coordinates and of the
  specified size. A sub-bitmap is a bitmap that shares drawing memory with a
  pre-existing (parent) bitmap, but possibly with a different size and clipping
  settings.

  The sub-bitmap may originate off or extend past the parent bitmap.

  See the discussion in @link(al_get_backbuffer) about using sub-bitmaps of the
  backbuffer.

  The parent bitmap's clipping rectangles are ignored.

  If a sub-bitmap was not or cannot be created then NULL is returned.

  When you are done with using the sub-bitmap you must call
  @link(al_destroy_bitmap) on it to free any resources allocated for it.

  Note that destroying parents of sub-bitmaps will not destroy the sub-bitmaps;
  instead the sub-bitmaps become invalid and should no longer be used for
  drawing - they still must be destroyed with al_destroy_bitmap however.  It
  does not matter whether you destroy a sub-bitmap before or after its parent
  otherwise.
  @seealso(al_create_bitmap) *)
  FUNCTION al_create_sub_bitmap (parent: ALLEGRO_BITMAPptr; x, y, w, h: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns true if the specified bitmap is a sub-bitmap, false otherwise.
  @seealso(al_create_sub_bitmap) @seealso(al_get_parent_bitmap) *)
  FUNCTION al_is_sub_bitmap (bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the bitmap this bitmap is a sub-bitmap of. Returns NULL if this
   bitmap is not a sub-bitmap. This function always returns the real bitmap,
   and never a sub-bitmap. This might NOT match what was passed to
   @code(al_create_sub_bitmap). Consider this code, for instance:
@longcode(#
VAR
  a, b, c: ALLEGRO_BITMAPptr;
BEGIN
  a := al_create_bitmap (512, 512);
  b := al_create_sub_bitmap (a, 128, 128, 256, 256);
  c := al_create_sub_bitmap (b, 64, 64, 128, 128);
  IF (al_get_parent_bitmap (b)== a) AND (al_get_parent_bitmap(c) = a) THEN
    WriteLn ('b & c are sub-bitmaps of a')
END;
#)
  The message will be printed because only a is a real bitmap, and both b and c
  are its sub-bitmaps.
  @seealso(al_create_sub_bitmap) @seealso(al_is_sub_bitmap) *)
  FUNCTION al_get_parent_bitmap (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* For a sub-bitmap, return it's x position within the parent.
  @seealso(al_create_sub_bitmap) @seealso(al_get_parent_bitmap)
  @seealso(al_get_bitmap_y) *)
  FUNCTION al_get_bitmap_x (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* For a sub-bitmap, return it's y position within the parent.
  @seealso(al_create_sub_bitmap) @seealso(al_get_parent_bitmap)
  @seealso(al_get_bitmap_x) *)
  FUNCTION al_get_bitmap_y (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* For a sub-bitmap, changes the parent, position and size.  This is the same
   as destroying the bitmap and re-creating it with @code(al_create_sub_bitmap)
   - except the bitmap pointer stays the same.  This has many uses, for example
   an animation player could return a single bitmap which can just be
   re-parented to different animation frames without having to re-draw the
   contents.  Or a sprite atlas could re-arrange its sprites without having to
   invalidate all existing bitmaps.
   @seealso(al_create_sub_bitmap) @seealso(al_get_parent_bitmap) *)
  PROCEDURE al_reparent_bitmap (bitmap, parent: ALLEGRO_BITMAPptr; x, y, w, h: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Create a new bitmap with al_create_bitmap, and copy the pixel data from the
   old bitmap across. If the new bitmap is a memory bitmap, its projection
   bitmap is reset to be orthographic.
   @seealso(al_create_bitmap) @seealso(al_set_new_bitmap_format)
   @seealso(al_set_new_bitmap_flags) @seealso(al_convert_bitmap) *)
  FUNCTION al_clone_bitmap (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Converts the bitmap to the current bitmap flags and format.  The bitmap will
   be as if it was created anew with @code(al_create_bitmap) but retain its
   contents.  All of this bitmap's sub-bitmaps are also converted.  If the new
   bitmap type is memory, then the bitmap's projection bitmap is reset to be
   orthographic.

   If this bitmap is a sub-bitmap, then it, its parent and all the sibling
   sub-bitmaps are also converted.
   @seealso(al_create_bitmap) @seealso(al_set_new_bitmap_format)
   @seealso(al_set_new_bitmap_flags) @seealso(al_clone_bitmap) *)
  PROCEDURE al_convert_bitmap (bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* If you create a bitmap when there is no current display (for example because
   you have not called @code(al_create_display) in the current thread) and are
   using the @code(ALLEGRO_CONVERT_BITMAP) bitmap flag (which is set by default)
   then the bitmap will be created successfully, but as a memory bitmap.  This
   function converts all such bitmaps to proper video bitmaps belonging to the
   current display.

   Note that video bitmaps get automatically converted back to memory bitmaps
   when the last display is destroyed.

   This operation will preserve all bitmap flags except
   @code(ALLEGRO_VIDEO_BITMAP) and @code(ALLEGRO_MEMORY_BITMAP).
   @seealso(al_convert_bitmap) @seealso(al_create_bitmap) *)
  PROCEDURE al_convert_memory_bitmap;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * bitmap_draw.h
 *****************************************************************************)

  CONST
  (* Flags for the blitting functions.  Documented at al_draw_bitmap. *)
  { @exclude }
    ALLEGRO_FLIP_HORIZONTAL = $00001;
  { @exclude }
    ALLEGRO_FLIP_VERTICAL   = $00002;



(* Draws an unscaled, unrotated bitmap at the given position to the current
   target bitmap.  @code(flags) can be a combination of:
@unorderedlist(
  @item(ALLEGRO_FLIP_HORIZONTAL - flip the bitmap about the y-axis)
  @item(ALLEGRO_FLIP_VERTICAL - flip the bitmap about the x-axis)
)
   @bold(Note:) The current target bitmap must be a different bitmap. Drawing a
   bitmap to itself (or to a sub-bitmap of itself) or drawing a sub-bitmap to
   its parent (or another sub-bitmap of its parent) are not currently
   supported. To copy part of a bitmap into the same bitmap simply use a
   temporary bitmap instead.

   @bold(Note:) The backbuffer (or a sub-bitmap thereof) can not be
   transformed, blended or tinted. If you need to draw the backbuffer draw it
   to a temporary bitmap first with no active transformation (except
   translation). Blending and tinting settings/parameters will be ignored. This
   does not apply when drawing into a memory bitmap.
   @param(bitmap Origin bitmap.)
   @param(dx Destination x.)
   @param(dy Destination y.)
   @param(flags)
   @seealso(al_set_target_bitmap) @seealso(al_draw_bitmap_region)
   @seealso(al_draw_scaled_bitmap) @seealso(al_draw_rotated_bitmap)
   @seealso(al_draw_scaled_rotated_bitmap) *)
  PROCEDURE al_draw_bitmap (bitmap: ALLEGRO_BITMAPptr; dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Draws a region of the given bitmap to the target bitmap.
   @param(bitmap Origin bitmap.)
   @param(sx source x)
   @param(sy source y)
   @param(sw source width @(width of region to blit@))
   @param(sh source height @(height of region to blit@))
   @param(dx destination x)
   @param(dy destination y)
   @param(flags same as for @code(al_draw_bitmap))
   @seealso(al_draw_bitmap) @seealso(al_draw_scaled_bitmap)
   @seealso(al_draw_rotated_bitmap) @seealso(al_draw_scaled_rotated_bitmap) *)
  PROCEDURE al_draw_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Draws a scaled version of the given bitmap to the target bitmap.
   @param(bitmap Origin bitmap.)
   @param(sx source x)
   @param(sy source y)
   @param(sw source width)
   @param(sh source height)
   @param(dx destination x)
   @param(dy destination y)
   @param(dw destination width)
   @param(dh destination height)
   @param(flags same as for al_draw_bitmap)
   @seealso(al_draw_bitmap) @seealso(al_draw_bitmap_region)
   @seealso(al_draw_rotated_bitmap) @seealso(al_draw_scaled_rotated_bitmap) *)
  PROCEDURE al_draw_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy, dw, dh: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Draws a rotated version of the given bitmap to the target bitmap.
   Example:
@longcode(#
VAR
  w, h: SINGLE;
BEGIN
  w := al_get_bitmap_width (bitmap);
  h := al_get_bitmap_height (bitmap);
  al_draw_rotated_bitmap (bitmap, w / 2, h / 2, x, y, ALLEGRO_PI / 2, 0);
#)
   The above code draws the bitmap centered on x/y and rotates it 90° clockwise.
   @param(bitmap Origin bitmap.)
   @param(cx center x @(relative to the left of bitmap@))
   @param(cy center y @(relative to the top or bitmap@))
   @param(dx destination x)
   @param(dy destination y)
   @param(angle angle in radians by which to rotate clockwise)
   @param(flags same as for al_draw_bitmap)
   @seealso(al_draw_bitmap) @seealso(al_draw_bitmap_region)
   @seealso(al_draw_scaled_bitmap) @seealso(al_draw_scaled_rotated_bitmap) *)
  PROCEDURE al_draw_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_draw_rotated_bitmap), but can also scale the bitmap.

   The point at cx/cy in the bitmap will be drawn at dx/dy and the bitmap is
   rotated and scaled around this point.
   @param(bitmap Origin bitmap.)
   @param(cx center x @(relative to the left of bitmap@))
   @param(cy center y @(relative to the top or bitmap@))
   @param(dx destination x)
   @param(dy destination y)
   @param(xscale how much to scale on the x-axis @(e.g. 2 for twice the size@))
   @param(yscale how much to scale on the y-axis)
   @param(angle angle in radians by which to rotate clockwise)
   @param(flags same as for al_draw_bitmap)
   @seealso(al_draw_bitmap) @seealso(al_draw_bitmap_region)
   @seealso(al_draw_scaled_bitmap) *)
  PROCEDURE al_draw_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Like @code(al_draw_bitmap) but multiplies all colors in the bitmap with the
   given color. For example:
@longcode(#
al_draw_tinted_bitmap (bitmap, al_map_rgba_f (0.5, 0.5, 0.5, 0.5), x, y, 0);
#)
   The above will draw the bitmap 50% transparently (r/g/b values need to be
   pre-multiplied with the alpha component with the default blend mode).
@longcode(#
al_draw_tinted_bitmap(bitmap, al_map_rgba_f(1, 0, 0, 1), x, y, 0);
#)
The above will only draw the red component of the bitmap.

   See @link(al_draw_bitmap) for a note on restrictions on which bitmaps can be
   drawn where. *)
  PROCEDURE al_draw_tinted_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_draw_bitmap_region) but multiplies all colors in the bitmap
   with the given color. @seealso(al_draw_tinted_bitmap) *)
  PROCEDURE al_draw_tinted_bitmap_region (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_draw_scaled_bitmap) but multiplies all colors in the bitmap
   with the given color. @seealso(al_draw_tinted_bitmap) *)
  PROCEDURE al_draw_tinted_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy, dw, dh: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_draw_rotated_bitmap) but multiplies all colors in the bitmap
   with the given color. @seealso(al_draw_tinted_bitmap) *)
  PROCEDURE al_draw_tinted_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_draw_scaled_rotated_bitmap) but multiplies all colors in the bitmap
   with the given color. @seealso(al_draw_tinted_bitmap) *)
  PROCEDURE al_draw_tinted_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_draw_tinted_scaled_rotated_bitmap) but you specify an area
   within the bitmap to be drawn.

   You can get the same effect with a sub bitmap:
@longcode(#
  al_draw_tinted_scaled_rotated_bitmap (
    bitmap, sx, sy, sw, sh, tint, cx, cy, dx, dy, xscale, yscale, angle, flags
  );

// This draws the same:
  sub_bitmap := al_create_sub_bitmap (bitmap, sx, sy, sw, sh);
  al_draw_tinted_scaled_rotated_bitmap (
    sub_bitmap, tint, cx, cy, dx, dy, xscale, yscale, angle, flags
  );
#)
  See @link(al_draw_bitmap) for a note on restrictions on which bitmaps can be
  drawn where.
  @seealso(al_draw_tinted_bitmap) *)
  PROCEDURE al_draw_tinted_scaled_rotated_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh: AL_FLOAT; tint: ALLEGRO_COLOR; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * path.h
 *****************************************************************************)

{ TODO:
  Actually, this header is needed by Allegro to build file paths,
  but at the moment I'll not add it. }



(*
 * utf8.h
 *****************************************************************************)

  {TODO: Documentation says it's not needed as it's used internally.
	Only basic functionality is implemented for convenience.

	Use of WIDESTRING and UTFSTRING is recommendable. }
  {TODO: There are a lot of stuff to add here. }

  TYPE
    _al_tagbstring = RECORD
      mlen, slen: AL_INT;
      data: AL_VOIDptr;
    END;
    ALLEGRO_USTRptr = ^ALLEGRO_USTR;
    ALLEGRO_USTR = _al_tagbstring;
    ALLEGRO_USTR_INFOptr = ^ALLEGRO_USTR_INFO;
    ALLEGRO_USTR_INFO = _al_tagbstring;

(* Creating strings *)
  FUNCTION al_ustr_new (CONST s: AL_STR): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_new_from_buffer (CONST s: AL_STRptr; size: AL_SIZE_T): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_ustr_free (us: ALLEGRO_USTRptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_cstr (CONST us: ALLEGRO_USTRptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_ustr_to_buffer (CONST us: ALLEGRO_USTRptr; buffer: AL_STRptr; size: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_cstr_dup (CONST us: ALLEGRO_USTRptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_dup (CONST us: ALLEGRO_USTRptr): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_dup_substr (CONST us: ALLEGRO_USTRptr; start_pos, end_pos: AL_INT): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Predefined string *)
  FUNCTION al_ustr_empty_string: ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Create a string that references the storage of a C-style string. The
  information about the string (e.g. its size) is stored in the @code(info)
  parameter. The string will not have any other storage allocated of its own,
  so if you allocate the info structure on the stack then no explicit "free"
  operation is required.

  The string is valid until the underlying C string disappears.

  Example:
@longcode(#
VAR
  Info: ALLEGRO_USTR_INFO;
  us: ALLEGRO_USTRptr;
BEGIN
  us := al_ref_cstr (Info, 'my string')
END;
#)
   @seealso(al_ref_buffer) @seealso(al_ref_ustr) *)
  FUNCTION al_ref_cstr (OUT info: ALLEGRO_USTR_INFO; CONST s: AL_STR): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ref_buffer (OUT info: ALLEGRO_USTR_INFO; CONST s: AL_STRptr;
      size: AL_SIZE_T): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ref_ustr (OUT info: ALLEGRO_USTR_INFO;
      CONST us: ALLEGRO_USTRptr; star_pos, end_pos: AL_INT): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Sizes and offsets *)
  FUNCTION al_ustr_size (CONST us: ALLEGRO_USTRptr): AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_length (CONST us: ALLEGRO_USTRptr): AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_offset (CONST us: ALLEGRO_USTRptr;index: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Find the byte offset of the next code point in string, beginning at
   @code(aPos). @code(aPos) does not have to be at the beginning of a code point.

   This function just looks for an appropriate byte; it doesn't check if found
   offset is the beginning of a valid code point. If you are working with
   possibly invalid UTF-8 strings then it could skip over some invalid bytes.
   @return(@true on success, and @code(aPos) will be updated to the found
   offset. Otherwise returns @false if @code(aPos) was already at the end of
   the string, and @code(aPos) is unmodified.)
   @seealso(al_ustr_prev)  *)
  FUNCTION al_ustr_next (CONST us: ALLEGRO_USTRptr; VAR aPos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Find the byte offset of the previous code point in string, before
   @code(aPos). @code(aPos) does not have to be at the beginning of a code point.

   This function just looks for an appropriate byte; it doesn't check if found
   offset is the beginning of a valid code point. If you are working with
   possibly invalid UTF-8 strings then it could skip over some invalid bytes.
   @return(@true on success, and @code(aPos) will be updated to the found
   offset. Otherwise returns @false if @code(aPos) was already at the end of
   the string, and @code(aPos) is unmodified.)
   @seealso(al_ustr_next) *)
  FUNCTION al_ustr_prev (CONST us: ALLEGRO_USTRptr; VAR aPos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;


(* Assign *)
  FUNCTION al_ustr_assign (us1: ALLEGRO_USTRptr; CONST us2: ALLEGRO_USTRptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_assign_cstr (us1: ALLEGRO_USTRptr; CONST s: AL_STR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Compare *)
  FUNCTION al_ustr_equal (CONST us1, us2: ALLEGRO_USTRptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_compare (CONST u, v: ALLEGRO_USTRptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_ncompare (CONST u, v: ALLEGRO_USTRptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * file.h
 *****************************************************************************)

{ TODO:
  Actually, this header is needed by Allegro to define new loaders and savers,
  but at the moment I'll not add it. }



(*
 * bitmap_io.h
 *****************************************************************************)

{ TODO: Several functions aren't added yet. }

{ Bitmap loader flag }
  CONST
  { @exclude }
    ALLEGRO_KEEP_BITMAP_FORMAT     = $0002;
  { @exclude }
    ALLEGRO_NO_PREMULTIPLIED_ALPHA = $0200;
  { @exclude }
    ALLEGRO_KEEP_INDEX             = $0800;


(* Loads an image file into a new @code(ALLEGRO_BITMAPptr). The file type is
   determined by the extension, except if the file has no extension in which
   case @code(al_identify_bitmap) is used instead.

   This is the same as calling @code(al_load_bitmap_flags) with a flags
   parameter of 0.

   @bold(Note:) the core Allegro library does not support any image file
   formats by default. You must use the @link(al5Image) addon, or register your
   own format handler.
   @return(@nil on error.)
   @seealso(al_load_bitmap_flags) @seealso(al_set_new_bitmap_format)
   @seealso(al_set_new_bitmap_flags) @seealso(al_init_image_addon) *)
  FUNCTION al_load_bitmap (CONST filename: AL_STR): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Loads an image file into a new @code(ALLEGRO_BITMAPptr). The file type is
   determined by the extension, except if the file has no extension in which
   case @link(al_identify_bitmap) is used instead.

   @code(Note:) the core Allegro library does not support any image file
   formats by default. You must use the allegro_image addon, or register your
   own format handler.
   @param(flags It may be a combination of the following constants:
@unorderedlist(
   @item(@bold(@code(ALLEGRO_NO_PREMULTIPLIED_ALPHA))
    By default, Allegro pre-multiplies the alpha channel of an image with the
    images color data when it loads it. Typically that would look something like
    this:
@longcode(#
    r := get_float_byte;
    g := get_float_byte;
    b := get_float_byte;
    a := get_float_byte;

    r := r * a;
    g := g * a;
    b := b * a;

    set_image_pixel (x, y, r, g, b, a);
#)
    The reason for this can be seen in the Allegro example @code(ex_premulalpha),
    ie, using pre-multiplied alpha gives more accurate color results in some
    cases. To use alpha blending with images loaded with pre-multiplied alpha,
    you would use the default blending mode, which is set with
    @code(al_set_blender @(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA@)).

    The @code(ALLEGRO_NO_PREMULTIPLIED_ALPHA) flag being set will ensure that
    images are not loaded with alpha pre-multiplied, but are loaded with color
    values direct from the image. That looks like this:
@longcode(#
    r := get_float_byte;
    g := get_float_byte;
    b := get_float_byte;
    a := get_float_byte;

    set_image_pixel (x, y, r, g, b, a);
#)
    To draw such an image using regular alpha blending, you would use
    @code(al_set_blender @(ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA@))
    to set the correct blender. This has some caveats. First, as mentioned
    above, drawing such an image can result in less accurate color blending
    (when drawing an image with linear filtering on, the edges will be darker
    than they should be). Second, the behaviour is somewhat confusing, which is
    explained in the example below.
@longcode(#
  // Load and create bitmaps with an alpha channel
    al_set_new_bitmap_format (ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA);
  // Load some bitmap with alpha in it
    bmp := al_load_bitmap ('some_alpha_bitmap.png');
  // We will draw to this buffer and then draw this buffer to the screen
    tmp_buffer := al_create_bitmap (SCREEN_W, SCREEN_H);
  // Set the buffer as the target and clear it
    al_set_target_bitmap (tmp_buffer);
    al_clear_to_color (al_map_rgba_f (0, 0, 0, 1));
  // Draw the bitmap to the temporary buffer
    al_draw_bitmap (bmp, 0, 0, 0);
  // Finally, draw the buffer to the screen
  // The output will look incorrect (may take close inspection
  // depending on the bitmap -- it may also be very obvious)
    al_set_target_bitmap (al_get_backbuffer (display));
    al_draw_bitmap (tmp_buffer, 0, 0, 0);
#)
    To explain further, if you have a pixel with 0.5 alpha, and you're using
    @code(@(ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA@)) for blending,
    the formula is:
@longcode(#
    a := da * dst + sa * src
#)
    Expands to:
@longcode(#
    result_a := dst_a * (1-0.5) + 0.5 * 0.5
#)
    So if you draw the image to the temporary buffer, it is blended once
    resulting in 0.75 alpha, then drawn again to the screen, blended in the
    same way, resulting in a pixel has 0.1875 as an alpha value.)
  @item(@bold(@code(ALLEGRO_KEEP_INDEX))

    Load the palette indices of 8-bit .bmp and .pcx files instead of the rgb
    colors.)
  @item(@bold(@code(ALLEGRO_KEEP_BITMAP_FORMAT))

    Force the resulting ALLEGRO_BITMAP to use the same format as the file.

    This is not yet honoured.)
  ))
   @returns(@nil on error.)
   @seealso(al_load_bitmap) *)
  FUNCTION al_load_bitmap_flags (CONST filename: AL_STR; flags: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Saves an ALLEGRO_BITMAP to an image file. The file type is determined by the
   extension.

   @bold(Note:) the core Allegro library does not support any image file
   formats by default. You must use the @link(al5Image) addon, or register your
   own format handler.
   @return(@true on success, @false on error.)
   @seealso(al_init_image_addon) *)
  FUNCTION al_save_bitmap (CONST filename: AL_STR; bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Tries to guess the bitmap file type of the given file by reading the first
   few bytes. The extension, if any, of the passed filename is not taken into
   account - only the file contents. By default Allegro cannot recognize any
   file types, but calling @link(al_init_image_addon) will add detection of
   (some of) the types it can read.
   @returns(a pointer to a static string with a file extension for the type,
   including the leading dot. For example ".png" or ".jpg". Returns @nil if the
   bitmap type cannot be determined.)
   @seealso(al_init_image_addon) @seealso(al_identify_bitmap)
   @seealso(al_register_bitmap_identifier) *)
  FUNCTION al_identify_bitmap (CONST filename: AL_STR): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * bitmap_lock.h
 *****************************************************************************)

  CONST
  (* Locking flags. Documented at al_lock_bitmap. *)
  { @exclude }
    ALLEGRO_LOCK_READWRITE  = 0;
  { @exclude }
    ALLEGRO_LOCK_READONLY   = 1;
  { @exclude }
    ALLEGRO_LOCK_WRITEONLY  = 2;

  TYPE
  (* Pointer to @link(ALLEGRO_LOCKED_REGION). *)
    ALLEGRO_LOCKED_REGIONptr = ^ALLEGRO_LOCKED_REGION;
  (* Users who wish to manually edit or read from a bitmap are required to lock
     it first.  The @code(ALLEGRO_LOCKED_REGION) structure represents the
     locked region of the bitmap.  This call will work with any bitmap,
     including memory bitmaps.
     @seealso(al_lock_bitmap) @seealso(al_lock_bitmap_region)
     @seealso(al_unlock_bitmap) @seealso(ALLEGRO_PIXEL_FORMAT) *)
    ALLEGRO_LOCKED_REGION = RECORD
    (* Points to the leftmost pixel of the first row (row 0) of the locked
       region. *)
      data: AL_VOIDptr;
    (* Indicates the pixel format of the data. *)
      format,
    (* Gives the size in bytes of a single row (also known as the stride).  The
       pitch may be greater than @code(width * pixel_size) due to padding; this
       is not uncommon.  It is also not uncommon for the pitch to be negative
       (the bitmap may be upside down). *)
      pitch,
    (* Number of bytes used to represent a single pixel. *)
      pixel_size: AL_INT;
    END;



 (* Lock an entire bitmap for reading or writing. If the bitmap is a display
    bitmap it will be updated from system memory after the bitmap is unlocked
    (unless locked read only).

    On some platforms, Allegro automatically backs up the contents of video
    bitmaps because they may be occasionally lost (see discussion in
    @link(al_create_bitmap)'s documentation). If you're completely recreating
    the bitmap contents often (e.g. every frame) then you will get much better
    performance by creating the target bitmap with
    @code(ALLEGRO_NO_PRESERVE_TEXTURE) flag.

    @bold(Note:) While a bitmap is locked, you can not use any drawing
    operations on it (with the sole exception of @link(al_put_pixel) and
    @link(al_put_blended_pixel)).
    @returns(@nil if the bitmap cannot be locked, e.g. the bitmap was locked
    previously and not unlocked. This function also returns @nil if the format
    is a compressed format.)
    @param(flags @unorderedlist(
      @item(@code(ALLEGRO_LOCK_READONLY) The locked region will not be written
        to. This can be faster if the bitmap is a video texture, as it can be
        discarded after the lock instead of uploaded back to the card.)
      @item(@code(ALLEGRO_LOCK_WRITEONLY) The locked region will not be read
        from. This can be faster if the bitmap is a video texture, as no data
        need to be read from the video card. You are required to fill in all
        pixels before unlocking the bitmap again, so be careful when using this
        flag.)
      @item(@code(ALLEGRO_LOCK_READWRITE) The locked region can be written to
        and read from. Use this flag if a partial number of pixels need to be
        written to, even if reading is not needed.)
    ))
    @param(format Indicates the pixel format that the returned buffer will be
      in. To lock in the same format as the bitmap stores its data internally,
      call with @code(al_get_bitmap_format @(bitmap@)) as the format or use
      @code(ALLEGRO_PIXEL_FORMAT_ANY). Locking in the native format will
      usually be faster. If the bitmap format is compressed, using
      @code(ALLEGRO_PIXEL_FORMAT_ANY) will choose an implementation defined
      non-compressed format.)
    @seealso(ALLEGRO_LOCKED_REGION) @seealso(ALLEGRO_PIXEL_FORMAT)
    @seealso(al_unlock_bitmap) @seealso(al_lock_bitmap_region)
    @seealso(al_lock_bitmap_blocked) @seealso(al_lock_bitmap_region_blocked) *)
  FUNCTION al_lock_bitmap (bitmap: ALLEGRO_BITMAPptr; format: ALLEGRO_PIXEL_FORMAT; flags: AL_INT): ALLEGRO_LOCKED_REGIONptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_lock_bitmap), but only locks a specific area of the bitmap. If
   the bitmap is a video bitmap, only that area of the texture will be updated
   when it is unlocked. Locking only the region you indend to modify will be
   faster than locking the whole bitmap.

   @bold(Note:) Using the @code(ALLEGRO_LOCK_WRITEONLY) with a blocked pixel
   format (i.e. formats for which @link(al_get_pixel_block_width) or
   @link(al_get_pixel_block_height) do not return 1) requires you to have the
   region be aligned to the block width for optimal performance. If it is not,
   then the function will have to lock the region with the
   @code(ALLEGRO_LOCK_READWRITE) instead in order to pad this region with
   valid data.
   @seealso(ALLEGRO_LOCKED_REGION) @seealso(ALLEGRO_PIXEL_FORMAT)
   @seealso(al_unlock_bitmap) *)
  FUNCTION al_lock_bitmap_region (bitmap: ALLEGRO_BITMAPptr; x, y, width, height: AL_INT; format: ALLEGRO_PIXEL_FORMAT; flags: AL_INT): ALLEGRO_LOCKED_REGIONptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_lock_bitmap), but allows locking bitmaps with a blocked pixel
   format (i.e. a format for which @link(al_get_pixel_block_width) or
   @link(al_get_pixel_block_height) do not return 1) in that format. To that
   end, this function also does not allow format conversion. For bitmap formats
   with a block size of 1, this function is identical to calling
   @code(al_lock_bitmap @(bmp, al_get_bitmap_format @(bmp@), flags@)).

   @bold(Note:) Currently there are no drawing functions that work when the
   bitmap is locked with a compressed format. @link(al_get_pixel) will also not
   work.
   @seealso(al_lock_bitmap) @seealso(al_lock_bitmap_region_blocked) *)
  FUNCTION al_lock_bitmap_blocked (bitmap: ALLEGRO_BITMAPptr; flags: AL_INT): ALLEGRO_LOCKED_REGIONptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @link(al_lock_bitmap_blocked), but allows locking a sub-region, for
   performance. Unlike @link(al_lock_bitmap_region) the region specified in
   terms of blocks and not pixels.
   @seealso(al_lock_bitmap_region) @seealso(al_lock_bitmap_blocked) *)
  FUNCTION al_lock_bitmap_region_blocked (bitmap: ALLEGRO_BITMAPptr; x_block, y_block, width_block, height_block, flags: AL_INT): ALLEGRO_LOCKED_REGIONptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*Unlock a previously locked bitmap or bitmap region. If the bitmap is a video
  bitmap, the texture will be updated to match the system memory copy (unless
  it was locked read only).
  @seealso(al_lock_bitmap) @seealso(al_lock_bitmap_region)
  @seealso(al_lock_bitmap_blocked) @seealso(al_lock_bitmap_region_blocked) *)
  PROCEDURE al_unlock_bitmap (bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns whether or not a bitmap is already locked.
   @seealso(al_lock_bitmap) @seealso(al_lock_bitmap_region)
   @seealso(al_unlock_bitmap) *)
  FUNCTION al_is_bitmap_locked (bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * blender.h
 *****************************************************************************)

  TYPE
  (* Blending modes. @seealso(al_set_blender) *)
    ALLEGRO_BLEND_MODE = (
      ALLEGRO_ZERO                = 0,
      ALLEGRO_ONE                 = 1,
      ALLEGRO_ALPHA               = 2,
      ALLEGRO_INVERSE_ALPHA       = 3,
      ALLEGRO_SRC_COLOR           = 4,
      ALLEGRO_DEST_COLOR          = 5,
      ALLEGRO_INVERSE_SRC_COLOR   = 6,
      ALLEGRO_INVERSE_DEST_COLOR  = 7,
      ALLEGRO_CONST_COLOR         = 8,
      ALLEGRO_INVERSE_CONST_COLOR = 9,
      ALLEGRO_NUM_BLEND_MODES
    );



  (* Blending modes. @seealso(al_set_blender) *)
    ALLEGRO_BLEND_OPERATIONS = (
      ALLEGRO_ADD                  = 0,
      ALLEGRO_SRC_MINUS_DEST       = 1,
      ALLEGRO_DEST_MINUS_SRC       = 2,
      ALLEGRO_NUM_BLEND_OPERATIONS
    );


(* Sets the function to use for blending for the current thread.

   Blending means, the source and destination colors are combined in drawing
   operations.

   Assume the source color (e.g. color of a rectangle to draw, or pixel of a
   bitmap to draw) is given as its red/green/blue/alpha components (if the
   bitmap has no alpha it always is assumed to be fully opaque, so 255 for
   8-bit or 1.0 for floating point): @italic(s = s.r, s.g, s.b, s.a). And this
   color is drawn to a destination, which already has a color:
   @italic(d = d.r, d.g, d.b, d.a).

   The conceptional formula used by Allegro to draw any pixel then depends on
   the op parameter:
   @unorderedlist(
   @item(@code(ALLEGRO_ADD)
@longcode(#
     r = d.r * df.r + s.r * sf.r
     g = d.g * df.g + s.g * sf.g
     b = d.b * df.b + s.b * sf.b
     a = d.a * df.a + s.a * sf.a
#))
    @item(@code(ALLEGRO_DEST_MINUS_SRC)
@longcode(#
     r = d.r * df.r - s.r * sf.r
     g = d.g * df.g - s.g * sf.g
     b = d.b * df.b - s.b * sf.b
     a = d.a * df.a - s.a * sf.a
#))
    @item(@code(ALLEGRO_SRC_MINUS_DEST)
@longcode(#
     r = s.r * sf.r - d.r * df.r
     g = s.g * sf.g - d.g * df.g
     b = s.b * sf.b - d.b * df.b
     a = s.a * sf.a - d.a * df.a
#))
    )
    Valid values for the factors @italic(sf) and @italic(df) passed to this
    function are as follows, where @italic(s) is the source color, @italic(d)
    the destination color and @italic(cc) the color set with
    @link(al_set_blend_color) (white by default)
    @unorderedlist(
    @item(@bold(ALLEGRO_ZERO) @code(f = 0, 0, 0, 0))
    @item(@bold(ALLEGRO_ONE) @code(f = 1, 1, 1, 1))
    @item(@bold(ALLEGRO_ALPHA) @code(f = s.a, s.a, s.a, s.a))
    @item(@bold(ALLEGRO_INVERSE_ALPHA) @code(f = 1 - s.a, 1 - s.a, 1 - s.a, 1 - s.a))
    @item(@bold(ALLEGRO_SRC_COLOR) @code(f = s.r, s.g, s.b, s.a))
    @item(@bold(ALLEGRO_DEST_COLOR) @code(f = d.r, d.g, d.b, d.a))
    @item(@bold(ALLEGRO_INVERSE_SRC_COLOR) @code(f = 1 - s.r, 1 - s.g, 1 - s.b, 1 - s.a))
    @item(@bold(ALLEGRO_INVERSE_DEST_COLOR) @code(f = 1 - d.r, 1 - d.g, 1 - d.b, 1 - d.a))
    @item(@bold(ALLEGRO_CONST_COLOR) @code(f = cc.r, cc.g, cc.b, cc.a))
    @item(@bold(ALLEGRO_INVERSE_CONST_COLOR) @code(f = 1 - cc.r, 1 - cc.g, 1 - cc.b, 1 - cc.a))
    )
    So for example, to restore the default of using premultiplied alpha
    blending, you would use:
@longcode(#
al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
#)
    As formula:
@longcode(#
r = d.r * (1 - s.a) + s.r * 1
g = d.g * (1 - s.a) + s.g * 1
b = d.b * (1 - s.a) + s.b * 1
a = d.a * (1 - s.a) + s.a * 1
#)
    If you are using non-pre-multiplied alpha, you could use
@longcode(#
al_set_blender(ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
#)
    Additive blending would be achieved with
@longcode(#
al_set_blender(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE);
#)
    Copying the source to the destination (including alpha) unmodified
@longcode(#
al_set_blender(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
#)
    Multiplying source and destination components
@longcode(#
al_set_blender(ALLEGRO_ADD, ALLEGRO_DEST_COLOR, ALLEGRO_ZERO)
#)
    Tinting the source (like @link(al_draw_tinted_bitmap))
@longcode(#
al_set_blender(ALLEGRO_ADD, ALLEGRO_CONST_COLOR, ALLEGRO_ONE);
al_set_blend_color(al_map_rgb(0, 96, 255)); /* nice Chrysler blue */
#)
    Averaging source and destination pixels
@longcode(#
al_set_blender(ALLEGRO_ADD, ALLEGRO_CONST_COLOR, ALLEGRO_CONST_COLOR);
al_set_blend_color(al_map_rgba_f(0.5, 0.5, 0.5, 0.5));
#)
    As formula:
@longcode(#
r = d.r * 0 + s.r * d.r
g = d.g * 0 + s.g * d.g
b = d.b * 0 + s.b * d.b
a = d.a * 0 + s.a * d.a
#)
   @seealso(al_set_separate_blender) @seealso(al_set_blend_color)
   @seealso(al_get_blender) *)
  PROCEDURE al_set_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Sets the color to use for blending when using the @code(ALLEGRO_CONST_COLOR)
   or @code(ALLEGRO_INVERSE_CONST_COLOR) blend functions. See
   @link(al_set_blender) for more information.
   @seealso(al_set_blender) @seealso(al_get_blend_color) *)
  PROCEDURE al_set_blend_color (color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the active blender for the current thread.
   @seealso(al_set_blender) @seealso(al_get_separate_blender) *)
  PROCEDURE al_get_blender (OUT op: ALLEGRO_BLEND_OPERATIONS; OUT source, dest: ALLEGRO_BLEND_MODE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the color currently used for constant color blending (white by
   default).
   @seealso(al_set_blend_color) @seealso(al_set_blender) *)
  FUNCTION al_get_blend_color: ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Like @code(al_set_blender), but allows specifying a separate blending
   operation for the alpha channel. This is useful if your target bitmap also
   has an alpha channel and the two alpha channels need to be combined in a
   different way than the color components.
   @seealso(al_set_blender) @seealso(al_get_blender)
   @seealso(al_get_separate_blender) *)
  PROCEDURE al_set_separate_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE;
				     alpha_op: ALLEGRO_BLEND_OPERATIONS; alpha_source, alpha_dest: ALLEGRO_BLEND_MODE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_separate_blender (OUT op: ALLEGRO_BLEND_OPERATIONS; OUT source, dest: ALLEGRO_BLEND_MODE;
				     OUT alpha_op: ALLEGRO_BLEND_OPERATIONS; OUT alpha_source, alpha_dest: ALLEGRO_BLEND_MODE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * events.h
 *)

  TYPE
  (* An integer used to distinguish between different types of events.
     @seealso(ALLEGRO_EVENT) @seealso(ALLEGRO_GET_EVENT_TYPE)
     @seealso(ALLEGRO_EVENT_TYPE_IS_USER) *)
    ALLEGRO_EVENT_TYPE = AL_UINT;

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
    ALLEGRO_EVENT_DISPLAY_HALT_DRAWING   = 48;
    ALLEGRO_EVENT_DISPLAY_RESUME_DRAWING = 49;

    ALLEGRO_EVENT_TOUCH_BEGIN            = 50;
    ALLEGRO_EVENT_TOUCH_END              = 51;
    ALLEGRO_EVENT_TOUCH_MOVE             = 52;
    ALLEGRO_EVENT_TOUCH_CANCEL           = 53;

    ALLEGRO_EVENT_DISPLAY_CONNECTED      = 60;
    ALLEGRO_EVENT_DISPLAY_DISCONNECTED   = 61;

(* Returns @true if the event type is not a builtin event type, i.e. one of
   those described in @link(ALLEGRO_EVENT_TYPE).*)
  FUNCTION ALLEGRO_EVENT_TYPE_IS_USER (t: ALLEGRO_EVENT_TYPE): AL_BOOL; INLINE;

(* Makes an event type identifier, which is a 32-bit integer. Usually, but not
   necessarily, this will be made from four 8-bit character codes, for example:
@longcode(#
  MY_EVENT_TYPE := ALLEGRO_GET_EVENT_TYPE ('MINE');
#)
   IDs less than 1024 are reserved for Allegro or its addons. Don't use
   anything lower than @code(ALLEGRO_GET_EVENT_TYPE @(#0#0#4#0@)).

   You should try to make your IDs unique so they don't clash with any 3rd
   party code you may be using. Be creative. Numbering from 1024 is not
   creative.

   If you need multiple identifiers, you could define them like this:
@longcode(#
  BASE_EVENT := ALLEGRO_GET_EVENT_TYPE ('MINE');
  BARK_EVENT := BASE_EVENT + 1;
  MEOW_EVENT := BASE_EVENT + 2;
  SQUAWK_EVENT := BASE_EVENT + 3;
#)
  @seealso(ALLEGRO_EVENT) @seealso(ALLEGRO_USER_EVENT)
  @seealso(ALLEGRO_EVENT_TYPE_IS_USER) *)
  FUNCTION ALLEGRO_GET_EVENT_TYPE (CONST str: SHORTSTRING): AL_INT; INLINE;


  TYPE
  (* Pointer to a display. *)
    ALLEGRO_DISPLAYptr = AL_POINTER;
  (* Pointer to joystick. *)
    ALLEGRO_JOYSTICKptr = AL_POINTER;
  (* Pointer to keyboard. *)
    ALLEGRO_KEYBOARDptr = AL_POINTER;
  (* Pointer to mouse. *)
    ALLEGRO_MOUSEptr = AL_POINTER;
  (* Pointer to timer. *)
    ALLEGRO_TIMERptr = AL_POINTER;


  (* Pointer to @link(ALLEGRO_EVENT_SOURCE). *)
    ALLEGRO_EVENT_SOURCEptr = ^ALLEGRO_EVENT_SOURCE;
  (* An event source is any object which can generate events.  For example, an
     @link(ALLEGRO_DISPLAYptr) can generate events, and you can get the
     @code(ALLEGRO_EVENT_SOURCE) pointer from an @code(ALLEGRO_DISPLAYptr) with
     @link(al_get_display_event_source).

     You may create your own “user” event sources that emit custom events.
     @seealso(ALLEGRO_EVENT) @seealso(al_init_user_event_source)
     @seealso(al_emit_user_event)
   *)
    ALLEGRO_EVENT_SOURCE = RECORD
      __pad : ARRAY [0..31] OF AL_INT;
    END;


  { @exclude }
    ALLEGRO_ANY_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : ALLEGRO_EVENT_SOURCEptr;
      timestamp : AL_DOUBLE;
    END;

    ALLEGRO_DISPLAY_EVENT = RECORD
      _type: ALLEGRO_EVENT_TYPE;
      source: ALLEGRO_DISPLAYptr;
      timestamp: AL_DOUBLE;
      x, y: AL_INT;
      width, height: AL_INT;
      orientation: AL_INT;
    END;

    ALLEGRO_JOYSTICK_EVENT = RECORD
      _type: ALLEGRO_EVENT_TYPE;
      source: ALLEGRO_JOYSTICKptr;
      timestamp: AL_DOUBLE;
      id: ALLEGRO_JOYSTICKptr;
      stick: AL_INT;
      axis: AL_INT;
      pos: AL_FLOAT;
      button: AL_INT;
    END;

    ALLEGRO_KEYBOARD_EVENT = RECORD
      _type: ALLEGRO_EVENT_TYPE;
      source: ALLEGRO_KEYBOARDptr;
      timestamp: AL_DOUBLE;
      display: ALLEGRO_DISPLAYptr;
      keycode: AL_INT;
      unichar: AL_INT;
      modifiers: AL_UINT;
      _repeat: AL_BOOL;
    END;

    ALLEGRO_MOUSE_EVENT = RECORD
      _type: ALLEGRO_EVENT_TYPE;
      source: ALLEGRO_MOUSEptr;
      timestamp: AL_DOUBLE;
      display: ALLEGRO_DISPLAYptr;
      x, y, z, w: AL_INT;
      dx, dy, dz, dw: AL_INT;
      button: AL_UINT;
      pressure: AL_FLOAT;
    END;

    ALLEGRO_TIMER_EVENT = RECORD
      _type: ALLEGRO_EVENT_TYPE;
      source: ALLEGRO_TIMERptr;
      timestamp: AL_DOUBLE;
      count: AL_INT64;
      error: AL_DOUBLE;
    END;

    ALLEGRO_TOUCH_EVENT = RECORD
      _type: ALLEGRO_EVENT_TYPE;
      source: AL_POINTER;
      timestamp: AL_DOUBLE;
      display: AL_POINTER;
      id: AL_INT;
      x, y: AL_DOUBLE;
      dx, dy: AL_DOUBLE;
      primary: AL_BOOL;
    END;

    ALLEGRO_USER_EVENT_DESCRIPTORptr = POINTER;

    ALLEGRO_USER_EVENTptr = ^ALLEGRO_USER_EVENT;
    ALLEGRO_USER_EVENT = RECORD
      _type : ALLEGRO_EVENT_TYPE;
      source : AL_POINTER;
      timestamp : AL_DOUBLE;
      __internal__descr : ALLEGRO_USER_EVENT_DESCRIPTORptr;
      data1 : AL_POINTER;
      data2 : AL_POINTER;
      data3 : AL_POINTER;
      data4 : AL_POINTER;
    END;

    ALLEGRO_EVENTptr = ^ALLEGRO_EVENT;
    ALLEGRO_EVENT = RECORD
      case LONGINT OF
   (* This must be the same as the first field of _AL_EVENT_HEADER.  *)
	0: ( _type: ALLEGRO_EVENT_TYPE );
   (* `any' is to allow the user to access the other fields which are
    * common to all event types, without using some specific type
    * structure.
    *)
	1: ( any: ALLEGRO_ANY_EVENT );
	2: ( display: ALLEGRO_DISPLAY_EVENT );
	3: ( joystick: ALLEGRO_JOYSTICK_EVENT );
	4: ( keyboard: ALLEGRO_KEYBOARD_EVENT );
	5: ( mouse: ALLEGRO_MOUSE_EVENT );
	6: ( timer: ALLEGRO_TIMER_EVENT );
        7: ( touch: ALLEGRO_TOUCH_EVENT );
	8: ( user: ALLEGRO_USER_EVENT );
      END;

    (* User event destructor. @seealso(al_emit_user_event) *)
      ALLEGRO_EVENT_DTOR_PROC = PROCEDURE (evt: ALLEGRO_USER_EVENTptr); CDECL;

(* Initialices an event source for emitting user events. The space for the event
   source must already have been allocated.

   One possible way of creating custom event sources is to derive other
   structures with @code(ALLEGRO_EVENT_SOURCE) at the head, e.g.
@longcode(#
TYPE
  THINGptr = ^THING;
  THING = RECORD
    event_source: ALLEGRO_EVENT_SOURCE;
    field1, field2: INTEGER;
    // etc.
  END;

FUNCTION CreateThing: THINGptr;
BEGIN
  RESULT := getmem (sizeof (THING));
  IF RESULT <> NIL THEN
  BEGIN
    al_init_user_event_source (@@(RESULT^.event_source));
    RESULT^.field1 := 0;
    RESULT^.field2 := 0;
  END
END;
#)

  The advantage here is that the @code(THING) pointer will be the same as the
  @code(ALLEGRO_EVENT_SOURCE) pointer. Events emitted by the event source will
  have the event source pointer as the source field, from which you can get a
  pointer to a @code(THING) by a simple cast (after ensuring checking the event
  is of the correct type).

  However, it is only one technique and you are not obliged to use it.

  The user event source will never be destroyed automatically. You must destroy
  it manually with @code(al_destroy_user_event_source).
  @seealso(ALLEGRO_EVENT_SOURCE) @seealso(al_destroy_user_event_source)
  @seealso(al_emit_user_event) @seealso(ALLEGRO_USER_EVENT) *)
  PROCEDURE al_init_user_event_source (source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Destroys an event source initialised with @link(al_init_user_event_source).

   This does not free the memory, as that was user allocated to begin with.
   @seealso(ALLEGRO_EVENT_SOURCE) *)
  PROCEDURE al_destroy_user_event_source (source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* The second argument is ALLEGRO_EVENT instead of ALLEGRO_USER_EVENT
 * to prevent users passing a pointer to a too-short structure.
 *)
  FUNCTION al_emit_user_event (source: ALLEGRO_EVENT_SOURCEptr; Event: ALLEGRO_EVENTptr; dtor: ALLEGRO_EVENT_DTOR_PROC): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_unref_user_event (event: ALLEGRO_USER_EVENTptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_event_source_data (source: ALLEGRO_EVENT_SOURCEptr; data: AL_POINTER);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_event_source_data (CONST source: ALLEGRO_EVENT_SOURCEptr): AL_POINTER;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



  TYPE
    ALLEGRO_EVENT_QUEUEptr = AL_POINTER;

(* Creates a new, empty event queue.
   @return(A pointer to the newly created object if successful, @nil on error.)
   @seealso(al_register_event_source) @seealso(al_destroy_event_queue)
   @seealso(ALLEGRO_EVENT_QUEUEptr) *)
  FUNCTION al_create_event_queue: ALLEGRO_EVENT_QUEUEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Destroys the event queue specified. All event sources currently registered
   with the queue will be automatically unregistered before the queue is
   destroyed.
   @seealso(al_create_event_queue) @seealso(ALLEGRO_EVENT_QUEUEptr) *)
  PROCEDURE al_destroy_event_queue (queue: ALLEGRO_EVENT_QUEUEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_event_source_registered (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Register the event source with the event queue specified. An event source
   may be registered with any number of event queues simultaneously, or none.
   Trying to register an event source with the same event queue more than once
   does nothing.
   @seealso(al_unregister_event_source) @seealso(ALLEGRO_EVENT_SOURCE) *)
  PROCEDURE al_register_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_unregister_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_pause_event_queue (queue: ALLEGRO_EVENT_QUEUEptr; pause: AL_BOOL);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_event_queue_paused (CONST queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_event_queue_empty (queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL;EXTERNAL ALLEGRO_LIB_NAME;
(* Takes the next event out of the event queue specified, and copy the contents
   into @code(ret_event), returning @true.  The original event will be removed
   from the queue.  If the event queue is empty, returns @false and the
   contents of @code(ret_event) are unspecified.
   @seealso(ALLEGRO_EVENT) @seealso(al_peek_next_event)
   @seealso(al_wait_for_event) *)
  FUNCTION al_get_next_event (queue: ALLEGRO_EVENT_QUEUEptr; OUT ret_event: ALLEGRO_EVENT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_peek_next_event (queue: ALLEGRO_EVENT_QUEUEptr; OUT ret_event: ALLEGRO_EVENT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_drop_next_event (queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_flush_event_queue (queue: ALLEGRO_EVENT_QUEUEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Wait until the event queue specified is non-empty. The first event in the
   queue will be copied into @code(ret_event) and removed from the queue.
   @seealso(ALLEGRO_EVENT) @seealso(al_wait_for_event_timed)
   @seealso(al_wait_for_event_until) @seealso(al_get_next_event) *)
  PROCEDURE al_wait_for_event (queue: ALLEGRO_EVENT_QUEUEptr; OUT ret_event: ALLEGRO_EVENT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_wait_for_event_timed (queue: ALLEGRO_EVENT_QUEUEptr; OUT event: ALLEGRO_EVENT; secs: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_wait_for_event_until (queue: ALLEGRO_EVENT_QUEUEptr; OUT event: ALLEGRO_EVENT; VAR timeout: ALLEGRO_TIMEOUT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * display.h
 *****************************************************************************)


  CONST
  (* Possible bit combinations for the flags parameter of al_set_new_display_flags. *)
    ALLEGRO_DEFAULT                     = 0 SHL 0;
    ALLEGRO_WINDOWED                    = 1 SHL 0;
    ALLEGRO_FULLSCREEN                  = 1 SHL 1;
    ALLEGRO_OPENGL                      = 1 SHL 2;
    ALLEGRO_DIRECT3D_INTERNAL           = 1 SHL 3;
    ALLEGRO_RESIZABLE                   = 1 SHL 4;
    ALLEGRO_FRAMELESS                   = 1 SHL 5;
    ALLEGRO_NOFRAME                     = ALLEGRO_FRAMELESS;
    ALLEGRO_GENERATE_EXPOSE_EVENTS      = 1 SHL 6;
    ALLEGRO_OPENGL_3_0                  = 1 SHL 7;
    ALLEGRO_OPENGL_FORWARD_COMPATIBLE   = 1 SHL 8;
    ALLEGRO_FULLSCREEN_WINDOW           = 1 SHL 9;
    ALLEGRO_MINIMIZED                   = 1 SHL 10;
    ALLEGRO_PROGRAMMABLE_PIPELINE       = 1 SHL 11;
    ALLEGRO_GTK_TOPLEVEL_INTERNAL       = 1 SHL 12;
    ALLEGRO_MAXIMIZED                   = 1 SHL 13;
    ALLEGRO_OPENGL_ES_PROFILE           = 1 SHL 14;

  TYPE
  (* Possible parameters for al_set_display_option.

     Make sure to update ALLEGRO_EXTRA_DISPLAY_SETTINGS if you modify
     anything here.  *)
    ALLEGRO_DISPLAY_OPTIONS = (
      ALLEGRO_RED_SIZE = 0,
      ALLEGRO_GREEN_SIZE = 1,
      ALLEGRO_BLUE_SIZE = 2,
      ALLEGRO_ALPHA_SIZE = 3,
      ALLEGRO_RED_SHIFT = 4,
      ALLEGRO_GREEN_SHIFT = 5,
      ALLEGRO_BLUE_SHIFT = 6,
      ALLEGRO_ALPHA_SHIFT = 7,
      ALLEGRO_ACC_RED_SIZE = 8,
      ALLEGRO_ACC_GREEN_SIZE = 9,
      ALLEGRO_ACC_BLUE_SIZE = 10,
      ALLEGRO_ACC_ALPHA_SIZE = 11,
      ALLEGRO_STEREO = 12,
      ALLEGRO_AUX_BUFFERS = 13,
      ALLEGRO_COLOR_SIZE = 14,
      ALLEGRO_DEPTH_SIZE = 15,
      ALLEGRO_STENCIL_SIZE = 16,
      ALLEGRO_SAMPLE_BUFFERS = 17,
      ALLEGRO_SAMPLES = 18,
      ALLEGRO_RENDER_METHOD = 19,
      ALLEGRO_FLOAT_COLOR = 20,
      ALLEGRO_FLOAT_DEPTH = 21,
      ALLEGRO_SINGLE_BUFFER = 22,
      ALLEGRO_SWAP_METHOD = 23,
      ALLEGRO_COMPATIBLE_DISPLAY = 24,
      ALLEGRO_UPDATE_DISPLAY_REGION = 25,
      ALLEGRO_VSYNC = 26,
      ALLEGRO_MAX_BITMAP_SIZE = 27,
      ALLEGRO_SUPPORT_NPOT_BITMAP = 28,
      ALLEGRO_CAN_DRAW_INTO_BITMAP = 29,
      ALLEGRO_SUPPORT_SEPARATE_ALPHA = 30,
      ALLEGRO_AUTO_CONVERT_BITMAPS = 31,
      ALLEGRO_SUPPORTED_ORIENTATIONS = 32,
      ALLEGRO_OPENGL_MAJOR_VERSION = 33,
      ALLEGRO_OPENGL_MINOR_VERSION = 34,
      ALLEGRO_DISPLAY_OPTIONS_COUNT
    );

  CONST
    ALLEGRO_DISPLAY_ORIENTATION_UNKNOWN = 0;
    ALLEGRO_DISPLAY_ORIENTATION_0_DEGREES = 1;
    ALLEGRO_DISPLAY_ORIENTATION_90_DEGREES = 2;
    ALLEGRO_DISPLAY_ORIENTATION_180_DEGREES = 4;
    ALLEGRO_DISPLAY_ORIENTATION_PORTRAIT = 5;
    ALLEGRO_DISPLAY_ORIENTATION_270_DEGREES = 8;
    ALLEGRO_DISPLAY_ORIENTATION_LANDSCAPE = 10;
    ALLEGRO_DISPLAY_ORIENTATION_ALL = 15;
    ALLEGRO_DISPLAY_ORIENTATION_FACE_UP = 16;
    ALLEGRO_DISPLAY_ORIENTATION_FACE_DOWN = 32;

    ALLEGRO_DONTCARE = 0;
    ALLEGRO_REQUIRE = 1;
    ALLEGRO_SUGGEST = 2;

{ enum ALLEGRO_DISPLAY_ORIENTATION declared at section "events.h". }

  { Formelly part of the primitives addon. }
    _ALLEGRO_PRIM_MAX_USER_ATTR = 10;

{ pointer ALLEGRO_DISPLAYptr declared at section "events.h". }

    ALLEGRO_NEW_WINDOW_TITLE_MAX_SIZE = 255;



  PROCEDURE al_set_new_display_refresh_rate (refresh_rate: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_new_display_flags (flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_new_display_refresh_rate: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_new_display_flags: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_new_window_title (CONST title: AL_STR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_new_window_title: AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_display_width (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_height (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Gets the pixel format of the display. *)
  FUNCTION al_get_display_format (display: ALLEGRO_DISPLAYptr): ALLEGRO_PIXEL_FORMAT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_refresh_rate (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_flags (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_orientation (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_toggle_display_flag (display: ALLEGRO_DISPLAYptr; flag: AL_INT; onoff: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Creates a display, or window, with the specified dimensions. The parameters
   of the display are determined by the last calls to
   @code(al_set_new_display_* ). Default parameters are used if none are set
   explicitly. Creating a new display will automatically make it the active
   one, with the backbuffer selected for drawing.

   Each display that uses OpenGL as a backend has a distinct OpenGL rendering
   context associated with it. See @link(al_set_target_bitmap) for the
   discussion about rendering contexts.
   @returns(@nil on error.)
   @seealso(al_set_new_display_flags) @seealso(al_set_new_display_option)
   @seealso(al_set_new_display_refresh_rate)
   @seealso(al_set_new_display_adapter) @seealso(al_set_new_window_title)
   @seealso(al_destroy_display) *)
  FUNCTION al_create_display (w, h: AL_INT): ALLEGRO_DISPLAYptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Destroys a display.

   If the target bitmap of the calling thread is tied to the display, then it
   implies a call to @code(al_set_target_bitmap @(@nil@);) before the display
   is destroyed.

   That special case notwithstanding, you should make sure no threads are
   currently targeting a bitmap which is tied to the display before you destroy
   it.
   @seealso(al_set_target_bitmap) *)
  PROCEDURE al_destroy_display (display: ALLEGRO_DISPLAYptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_current_display: ALLEGRO_DISPLAYptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* This function selects the bitmap to which all subsequent drawing operations
   in the calling thread will draw to. To return to drawing to a display, set
   the backbuffer of the display as the target bitmap, using
   @link(al_get_backbuffer). As a convenience, you may also use
   @link(al_set_target_backbuffer).

   Each video bitmap is tied to a display. When a video bitmap is set to as the
   target bitmap, the display that the bitmap belongs to is automatically made
   "current" for the calling thread (if it is not current already). Then
   drawing other bitmaps which are tied to the same display can be hardware
   accelerated.

   A single display cannot be current for multiple threads simultaneously. If
   you need to release a display, so it is not current for the calling thread,
   call @code(al_set_target_bitmap @(@nil@);)

   Setting a memory bitmap as the target bitmap will not change which display
   is current for the calling thread.

   On some platforms, Allegro automatically backs up the contents of video
   bitmaps because they may be occasionally lost (see discussion in
   @link(al_create_bitmap)'s documentation). If you're completely recreating
   the bitmap contents often (e.g. every frame) then you will get much better
   performance by creating the target bitmap with
   @code(ALLEGRO_NO_PRESERVE_TEXTURE) flag.

   @bold(OpenGL note:)

   Framebuffer objects (FBOs) allow OpenGL to directly draw to a bitmap, which
   is very fast. When using an OpenGL display, if all of the following
   conditions are met an FBO will be created for use with the bitmap:
   @unorderedlist(
    @item(The @code(GL_EXT_framebuffer_object) OpenGL extension is available.)
    @item(The bitmap is not a memory bitmap.)
    @item(The bitmap is not currently locked.)
   )
   In Allegro 5.0.0, you had to be careful as an FBO would be kept around until
   the bitmap is destroyed or you explicitly called @link(al_remove_opengl_fbo)
   on the bitmap, wasting resources. In newer versions, FBOs will be freed
   automatically when the bitmap is no longer the target bitmap, unless you
   have called @link(al_get_opengl_fbo) to retrieve the FBO id.

   In the following example, no FBO will be created:
@longcode(#
lock := al_lock_bitmap (bitmap);
al_set_target_bitmap (bitmap);
al_put_pixel (x, y, color);
al_unlock_bitmap (bitmap);
#)
   The above allows using @code(al_put_pixel) on a locked bitmap without
   creating an FBO.

   In this example an FBO is created however:
@longcode(#
al_set_target_bitmap(bitmap);
al_draw_line(x1, y1, x2, y2, color, 0);
#)
   An OpenGL command will be used to directly draw the line into the bitmap's
   associated texture.
   @seealso(al_get_target_bitmap) @seealso(al_set_target_backbuffer) *)
  PROCEDURE al_set_target_bitmap (Bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Same as @code(al_set_target_bitmap@(al_get_backbuffer@(display@)@);).
   @seealso(al_set_target_bitmap) @seealso(al_get_backbuffer) *)
  PROCEDURE al_set_target_backbuffer (display: ALLEGRO_DISPLAYptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Return a special bitmap representing the back-buffer of the display.

   Care should be taken when using the backbuffer bitmap (and its sub-bitmaps)
   as the source bitmap (e.g as the bitmap argument to @link(al_draw_bitmap)).
   Only untransformed operations are hardware accelerated. These consist of
   @link(al_draw_bitmap) and @link(al_draw_bitmap_region) when the current
   transformation is the identity. If the tranformation is not the identity, or
   some other drawing operation is used, the call will be routed through the
   memory bitmap routines, which are slow. If you need those operations to be
   accelerated, then first copy a region of the backbuffer into a temporary
   bitmap (via the @link(al_draw_bitmap) and @link(al_draw_bitmap_region)), and
   then use that temporary bitmap as the source bitmap. *)
  FUNCTION al_get_backbuffer (display: ALLEGRO_DISPLAYptr): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Return the target bitmap of the calling thread.
   @seealso(al_set_target_bitmap) *)
  FUNCTION al_get_target_bitmap: ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



  FUNCTION al_acknowledge_resize (display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_resize_display (display: ALLEGRO_DISPLAYptr; width, height: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_flip_display;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_update_display_region (x, y, Width, height: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_compatible_bitmap (bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Wait for the beginning of a vertical retrace. Some driver/card/monitor
   combinations may not be capable of this.

   Note how @code(al_flip_display) usually already waits for the vertical
   retrace, so unless you are doing something special, there is no reason to
   call this function.
   @returns(@false if not possible, @true if successful.)
   @seealso(al_flip_display) *)
  FUNCTION al_wait_for_vsync: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Retrieve the associated event source. See the documentation on events for a
   list of the events displays will generate. *)
  FUNCTION al_get_display_event_source (display: ALLEGRO_DISPLAYptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



  PROCEDURE al_set_display_icon (display: ALLEGRO_DISPLAYptr; icon: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_display_icons (display: ALLEGRO_DISPLAYptr; num_icons: AL_INT; VAR icons: ARRAY OF ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Stuff for multihead/window management *)
  FUNCTION al_get_new_display_adapter: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_new_display_adapter (adapter: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_new_window_position (x, y: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_new_window_position (OUT x, y: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_window_position (display: ALLEGRO_DISPLAYptr; x, y: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_window_position (display: ALLEGRO_DISPLAYptr; OUT x, y: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_window_constraints (display: ALLEGRO_DISPLAYptr; min_w, min_h, max_w, max_h: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_window_constraints (display: ALLEGRO_DISPLAYptr; OUT min_w, min_h, max_w, max_h: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



  PROCEDURE al_set_window_title (display: ALLEGRO_DISPLAYptr; const title: AL_STR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



  PROCEDURE al_set_new_display_option (option: ALLEGRO_DISPLAY_OPTIONS; value, importance: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_new_display_option (option: ALLEGRO_DISPLAY_OPTIONS; VAR importance: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_reset_new_display_options;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_display_option (display: ALLEGRO_DISPLAYptr; option: ALLEGRO_DISPLAY_OPTIONS; value: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_option (display: ALLEGRO_DISPLAYptr; option: ALLEGRO_DISPLAY_OPTIONS): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Deferred drawing *)
  PROCEDURE al_hold_bitmap_drawing (hold: AL_BOOL);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_bitmap_drawing_held: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



  PROCEDURE al_acknowledge_drawing_halt (display: ALLEGRO_DISPLAYptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_acknowledge_drawing_resume (display: ALLEGRO_DISPLAYptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * clipboard.h
 *
 *   Clipboard handling.
 *****************************************************************************)

  FUNCTION al_get_clipboard_text (display: ALLEGRO_DISPLAYptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_clipboard_text
    (display: ALLEGRO_DISPLAYptr; CONST text: AL_STR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_clipboard_has_text (display: ALLEGRO_DISPLAYptr): AL_BOOL
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * config.h
 *****************************************************************************)

{ TODO:
  At the moment I'll not include this header.  Object Pascal defines the
  TStrings class that implements a similar functionality.  Also both FCL and
  VCL define classes that allow to manage INI files too.

  Actually I'll add this unit only if it's necessary because Allegro does need
  any special configuration file.
 }



(*
 * cpu.h
 *
 *   CPU and system information handling.
 *****************************************************************************)

(* Returns the number of CPU cores that the system Allegro is running on has
   and which could be detected, or a negative number if detection failed.  Even
   if a positive number is returned, it might be that it is not correct.  For
   example, Allegro running on a virtual machine will return the amount of
   CPU's of the VM, and not that of the underlying system.

   Furthermore even if the number is correct, this only gives you information
   about the total CPU cores of the system Allegro runs on.  The amount of
   cores available to your program may be less due to circumstances such as
   programs that are currently running.

   Therefore, it's best to use this for advisory purposes only.  It is
   certainly a bad idea to make your program exclusive to systems for which
   this function returns a certain "desirable" number.

   This function may be called prior to @link(al_install_system) or
   @link(al_init). *)
  FUNCTION al_get_cpu_count: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the size in MB of the random access memory that the system Allegro
   is running on has and which could be detected, or a negative number if
   detection failed.  Even if a positive number is returned, it might be that
   it is not correct.  For example, Allegro running on a virtual machine will
   return the amount of RAM of the VM, and not that of the underlying system.

   Furthermore even if the number is correct, this only gives you information
   about the total physical memory of the system Allegro runs on.  The memory
   available to your program may be less due to circumstances such as virtual
   memory, and other programs that are currently running.

   Therefore, it's best to use this for advisory purposes only.  It is
   certainly a bad idea to make your program exclusive to systems for which
   this function returns a certain "desirable" number.

   This function may be called prior to @link(al_install_system) or
   @link(al_init). *)
  FUNCTION al_get_ram_size: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * debug.h
 *
 *   Debug facilities.
 *
 *   By Shawn Hargreaves.
 *****************************************************************************)

{ TODO:
  At the moment I'll not include this header.

  Actually I'll add this unit only if it's necessary or helpful. }



(*
 * drawing.h
 *****************************************************************************)

(* Clears the complete target bitmap, but confined by the clipping rectangle.
   @seealso(al_set_clipping_rectangle) @seealso(al_clear_depth_buffer) *)
  PROCEDURE al_clear_to_color (color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_clear_depth_buffer (x: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_pixel (x, y: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * error.h
 *     Error handling.
 *****************************************************************************)

(* Some Allegro functions will set an error number as well as returning an
   error code.  Call this function to retrieve the last error number set for
   the calling thread. @seealso(al_set_errno) *)
  FUNCTION al_get_errno: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Sets the error number for the calling thread. @seealso(al_get_errno) *)
  PROCEDURE al_set_errno (errnum: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * fixed.h
 *
 *   Fixed point type.
 *
 *   By Shawn Hargreaves.
 *****************************************************************************)

 { TODO: Coming soon ;). }



(*
 * fmath.h
 *
 *   Fixed point math routines.
 *
 *   By Shawn Hargreaves.
 *****************************************************************************)

 { TODO: Coming soon ;). }



(*
 * fshook.h
 *
 * File system hooks.
 *****************************************************************************)

 { TODO: Don't do nothing until file.h is added. }



(*
 * fullscreen_mode.h
 *****************************************************************************)

  TYPE
    ALLEGRO_DISPLAY_MODEptr = ^ALLEGRO_DISPLAY_MODE;
    ALLEGRO_DISPLAY_MODE = RECORD
      width, height, format, refresh_rate: AL_INT;
    END;

  FUNCTION al_get_num_display_modes: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Retrieves a fullscreen mode.  Display parameters should not be changed
   between a call of @code(al_get_num_display_modes) and
   @code(al_get_display_mode).
   @param(index Must be between 0 and the number returned from
     @code(al_get_num_display_modes - 1).)
   @param(mode Must be an allocated @code(ALLEGRO_DISPLAY_MODE) structure.)
   @return(@nil on failure, and the mode parameter that was passed in on
     success.)
   @seealso(ALLEGRO_DISPLAY_MODE) @seealso(al_get_num_display_modes) *)
  FUNCTION al_get_display_mode (index: AL_INT; OUT mode: ALLEGRO_DISPLAY_MODE): ALLEGRO_DISPLAY_MODEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * joystick.h
 *
 *   Joystick routines.
 *****************************************************************************)

  CONST
  (* internal values *)
    _AL_MAX_JOYSTICK_AXES    =  3;
    _AL_MAX_JOYSTICK_STICKS  = 16;
    _AL_MAX_JOYSTICK_BUTTONS = 32;

  TYPE
    ALLEGRO_JOYSTICK_STATE = RECORD
      stick: ARRAY [0.._AL_MAX_JOYSTICK_STICKS - 1] OF RECORD
	axis: ARRAY [0.._AL_MAX_JOYSTICK_AXES - 1] OF AL_FLOAT; { -1.0 to 1.0 }
      END;
      button: ARRAY [0.._AL_MAX_JOYSTICK_BUTTONS - 1] OF AL_INT; { 0 to 32767 }
    END;



    ALLEGRO_JOYFLAGS = (
      ALLEGRO_JOYFLAG_DIGITAL  = $01,
      ALLEGRO_JOYFLAG_ANALOGUE = $02
    );

(* Install a joystick driver, returning @true if successful.  If a joystick
   driver was already installed, returns @true immediately.
   @seealso(al_uninstall_joystick) *)
  FUNCTION al_install_joystick: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_uninstall_joystick;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_joystick_installed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_reconfigure_joysticks: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_num_joysticks: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick (joyn: AL_INT): ALLEGRO_JOYSTICKptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_release_joystick (j: ALLEGRO_JOYSTICKptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_active (j: ALLEGRO_JOYSTICKptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_name (j: ALLEGRO_JOYSTICKptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_num_sticks (j: ALLEGRO_JOYSTICKptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_stick_flags (j: ALLEGRO_JOYSTICKptr; stick: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_stick_name (j: ALLEGRO_JOYSTICKptr; stick: AL_INT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_num_axes (j: ALLEGRO_JOYSTICKptr; stick: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_axis_name (j: ALLEGRO_JOYSTICKptr; stick, axis: AL_INT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_num_buttons (j: ALLEGRO_JOYSTICKptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_button_name (j: ALLEGRO_JOYSTICKptr; buttonn: AL_INT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_joystick_state (j: ALLEGRO_JOYSTICKptr; VAR ret_state: ALLEGRO_JOYSTICK_STATE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_event_source: ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * keycodes.h
 *****************************************************************************)

{$include keycodes.inc}



(*
 * keyboard.h
 *
 *   Keyboard routines.
 *****************************************************************************)

  TYPE
    ALLEGRO_KEYBOARD_STATE = RECORD
      display: ALLEGRO_DISPLAYptr;
    { @exclude internal }
      __key_down__internal__: ARRAY [0..((ALLEGRO_KEY_MAX + 31) DIV 32) - 1] OF AL_UINT;
    END;

  FUNCTION al_is_keyboard_installed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Install a keyboard driver.  Returns @true if successful.  If a driver was
   already installed, nothing happens and @true is returned.
   @seealso(al_uninstall_keyboard) @seealso(al_is_keyboard_installed) *)
  FUNCTION al_install_keyboard: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_uninstall_keyboard;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_keyboard_leds (leds: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_keycode_to_name (keycode: AL_INT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_keyboard_state (OUT ret_state: ALLEGRO_KEYBOARD_STATE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_key_down (VAR state: ALLEGRO_KEYBOARD_STATE; keycode: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_keyboard_event_source: ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * mouse.h
 *
 *  Mouse routines.
 *****************************************************************************)

  CONST
  (* Allow up to four extra axes for future expansion. *)
    ALLEGRO_MOUSE_MAX_EXTRA_AXES = 4;

  TYPE
  (* Type: ALLEGRO_MOUSE_STATE *)
    ALLEGRO_MOUSE_STATE = RECORD
    (* (x, y) Primary mouse position
     * (z) Mouse wheel position (1D 'wheel'), or,
     * (w, z) Mouse wheel position (2D 'ball')
     * display - the display the mouse is on (coordinates are relative to this)
     * pressure - the pressure appleid to the mouse (for stylus/tablet)
     *)
      x, y, z, w: AL_INT;
      more_axes: ARRAY [0..(ALLEGRO_MOUSE_MAX_EXTRA_AXES - 1)] OF AL_INT;
      buttons: AL_INT;
      pressure: AL_FLOAT;
      display: ALLEGRO_DISPLAYptr;
    END;



  FUNCTION al_is_mouse_installed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Install a mouse driver.

   Returns @true if successful.  If a driver was already installed, nothing
   happens and @true is returned. *)
  FUNCTION al_install_mouse: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_uninstall_mouse;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_mouse_num_buttons: AL_UINT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_mouse_num_axes: AL_UINT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_mouse_xy (display: ALLEGRO_DISPLAYptr; x, y: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_mouse_z (z: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_mouse_w (w: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_mouse_axis (axis, value: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_mouse_state (OUT ret_state: ALLEGRO_MOUSE_STATE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_mouse_button_down (VAR state: ALLEGRO_MOUSE_STATE; button: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_mouse_state_axis (VAR state: ALLEGRO_MOUSE_STATE; axis: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_mouse_cursor_position (VAR ret_x, ret_y: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_grab_mouse (display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ungrab_mouse: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_mouse_wheel_precision (precision: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_mouse_wheel_precision: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



  FUNCTION al_get_mouse_event_source: ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * touch_input.h
 *
 *   Touch input routines.
 *****************************************************************************)

{ TODO: Not yet. }



(*
 * haptic.h
 *
 *   Haptic (that is, force feedback) routines for Allegro.
 *
 *   By Beoran.
 *****************************************************************************)

{ TODO: Not yet.  Needs touch_input.h. }



(*
 * memory.h
 *
 *   Memory management routines.
 *****************************************************************************)

 TYPE
 (* Used to define the memory management functions.
    @seealso(al_set_memory_interface) *)
   ALLEGRO_MEMORY_INTERFACE = RECORD
     mi_malloc: FUNCTION (n: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER; CDECL;
     mi_free: PROCEDURE (ptr: AL_POINTER; line: AL_INT; CONST afile, func: AL_STR); CDECL;
     mi_realloc: FUNCTION (ptr: AL_POINTER; n: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER; CDECL;
     mi_calloc: FUNCTION (n, count: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER; CDECL;
   END;

(* Overrides the memory management functions with implementations of
   @link(al_malloc_with_context), @link(al_free_with_context),
   @link(al_realloc_with_context) and @link(al_calloc_with_context). The
   context arguments may be used for debugging. The new functions should be
   thread safe.
   @seealso(ALLEGRO_MEMORY_INTERFACE) @seealso(al_restore_memory_interface) *)
  PROCEDURE al_set_memory_interface (VAR iface: ALLEGRO_MEMORY_INTERFACE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Restores the default behavior of the memory management functions.
   @seealso(al_set_memory_interface) *)
  PROCEDURE al_restore_memory_interface; INLINE;


(* Like @code(malloc) in the C standard library, but the implementation may be
   overridden.
   @seealso(al_free) @seealso(al_realloc) @seealso(al_calloc)
   @seealso(al_malloc_with_context) @seealso(al_set_memory_interface) *)
  FUNCTION al_malloc (CONST n: AL_SIZE_T): AL_POINTER; INLINE;
(* Like @code(free) in the C standard library, but the implementation may be
   overridden.

   Additionally, on Windows, a memory block allocated by one DLL must be freed
   from the same DLL. In the few places where an Allegro function returns a
   pointer that must be freed, you must use al_free for portability to Windows.
   @seealso(al_malloc) @seealso(al_free_with_context) *)
  PROCEDURE al_free (p: AL_POINTER); INLINE;
(* Like @code(realloc) in the C standard library, but the implementation may be
   overridden.
   @seealso(al_malloc) @seealso(al_realloc_with_context) *)
  FUNCTION al_realloc (p: AL_POINTER; CONST n: AL_SIZE_T): AL_POINTER; INLINE;
(* Like @code(calloc) in the C standard library, but the implementation may be
   overridden.
   @seealso(al_malloc) @seealso(al_calloc_with_context) *)
  FUNCTION al_calloc (CONST c, n: AL_SIZE_T): AL_POINTER; INLINE;



(* This calls @code(malloc) from the Allegro library (this matters on Windows),
   unless overridden with @link(al_set_memory_interface).

   Generally you should use the @link(al_malloc) function. *)
  FUNCTION al_malloc_with_context
    (n: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* This calls @code(free) from the Allegro library (this matters on Windows),
   unless overridden with @link(al_set_memory_interface).

   Generally you should use the @link(al_free) function. *)
  PROCEDURE al_free_with_context
    (ptr: AL_POINTER; line: AL_INT; CONST afile, func: AL_STR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* This calls @code(realloc) from the Allegro library (this matters on Windows),
   unless overridden with @link(al_set_memory_interface).

   Generally you should use the @link(al_realloc) function. *)
  FUNCTION al_realloc_with_context
    (ptr: AL_POINTER; n: AL_SIZE_T; line: AL_INT;
     CONST afile, func: AL_STR): AL_POINTER;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* This calls @code(calloc) from the Allegro library (this matters on Windows),
   unless overridden with @link(al_set_memory_interface).

   Generally you should use the @link(al_calloc) function. *)
  FUNCTION al_calloc_with_context
    (n, count: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * monitor.h
 *****************************************************************************)

  CONST
    ALLEGRO_DEFAULT_DISPLAY_ADAPTER = -1;

  TYPE
    ALLEGRO_MONITOR_INFOptr = ^ALLEGRO_MONITOR_INFO;
    ALLEGRO_MONITOR_INFO = RECORD
      x1, y1, x2, y2: AL_INT;
    END;

  FUNCTION al_get_num_video_adapters: AL_INT; CDECL;
    EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_monitor_info (adapter: AL_INT; OUT info: ALLEGRO_MONITOR_INFO): AL_BOOL; CDECL;
    EXTERNAL ALLEGRO_LIB_NAME;



(*
 * mouse_cursor.h
 *****************************************************************************)

  TYPE
  (* Mouse cursors *)
    ALLEGRO_MOUSE_CURSORptr = AL_POINTER;

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

  FUNCTION al_create_mouse_cursor (sprite: ALLEGRO_BITMAPptr; xfocus, yfocus: AL_INT): ALLEGRO_MOUSE_CURSORptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_destroy_mouse_cursor (cursor: ALLEGRO_MOUSE_CURSORptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_mouse_cursor (display: ALLEGRO_DISPLAYptr; cursor: ALLEGRO_MOUSE_CURSORptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_system_mouse_cursor (display: ALLEGRO_DISPLAYptr; cursor_id: ALLEGRO_SYSTEM_MOUSE_CURSOR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_show_mouse_cursor (display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_hide_mouse_cursor (display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * render_state.h
 *****************************************************************************)

  TYPE
  { Used by @link(al_set_render_state). }
    ALLEGRO_RENDER_STATE = (
    (* ALLEGRO_ALPHA_TEST was the name of a rare bitmap flag only used on the
       Wiz port.  Reuse the name but retain the same value. *)
    { If this is set to 1, the values of @code(ALLEGRO_ALPHA_FUNCTION) and
      @code(ALLEGRO_ALPHA_TEST_VALUE) define a comparison function which is
      performed for each pixel. Only if it evaluates to true the pixel is
      written. Otherwise no subsequent processing @(like depth test or
	blending@) is performed. }
      ALLEGRO_ALPHA_TEST = $0010,
    { This determines how the framebuffer and depthbuffer are updated whenever
      a pixel is written @(in case alpha and/or depth testing is enabled: after
      all such enabled tests succeed@). Depth values are only written if
      @code(ALLEGRO_DEPTH_TEST) is 1, in addition to the write mask flag being
      set. }
      ALLEGRO_WRITE_MASK,
    { If this is set to 1, compare the depth value of any newly written pixels
      with the depth value already in the buffer, according to
      @code(ALLEGRO_DEPTH_FUNCTION). Allegro primitives with no explicit z
      coordinate will write a value of 0 into the depth buffer. }
      ALLEGRO_DEPTH_TEST,
    { One of @code(ALLEGRO_RENDER_NEVER) @code(ALLEGRO_RENDER_ALWAYS)
      @code(ALLEGRO_RENDER_LESS) @code(ALLEGRO_RENDER_EQUAL)
      @code(ALLEGRO_RENDER_LESS_EQUAL) @code(ALLEGRO_RENDER_GREATER)
      @code(ALLEGRO_RENDER_NOT_EQUAL) @code(ALLEGRO_RENDER_GREATER_EQUAL), only
      used when @code(ALLEGRO_DEPTH_TEST) is 1. }
      ALLEGRO_DEPTH_FUNCTION,
    { One of @code(ALLEGRO_RENDER_NEVER) @code(ALLEGRO_RENDER_ALWAYS)
      @code(ALLEGRO_RENDER_LESS) @code(ALLEGRO_RENDER_EQUAL)
      @code(ALLEGRO_RENDER_LESS_EQUAL) @code(ALLEGRO_RENDER_GREATER)
      @code(ALLEGRO_RENDER_NOT_EQUAL) @code(ALLEGRO_RENDER_GREATER_EQUAL), only
      used when @code(ALLEGRO_ALPHA_TEST) is 1.}
      ALLEGRO_ALPHA_FUNCTION,
    { Only used when @code(ALLEGRO_ALPHA_TEST) is 1. }
      ALLEGRO_ALPHA_TEST_VALUE
    );

  CONST
    { @exclude }
    ALLEGRO_RENDER_NEVER = 0;
    { @exclude }
    ALLEGRO_RENDER_ALWAYS = 1;
    { @exclude }
    ALLEGRO_RENDER_LESS = 2;
    { @exclude }
    ALLEGRO_RENDER_EQUAL = 3;
    { @exclude }
    ALLEGRO_RENDER_LESS_EQUAL = 4;
    { @exclude }
    ALLEGRO_RENDER_GREATER = 5;
    { @exclude }
    ALLEGRO_RENDER_NOT_EQUAL = 6;
    { @exclude }
    ALLEGRO_RENDER_GREATER_EQUA = 7;

    { @exclude }
    ALLEGRO_MASK_RED = 1 SHL 0;
    { @exclude }
    ALLEGRO_MASK_GREEN = 1 SHL 1;
    { @exclude }
    ALLEGRO_MASK_BLUE = 1 SHL 2;
    { @exclude }
    ALLEGRO_MASK_ALPHA = 1 SHL 3;
    { @exclude }
    ALLEGRO_MASK_DEPTH = 1 SHL 4;
    { @exclude }
    ALLEGRO_MASK_RGB = (ALLEGRO_MASK_RED OR ALLEGRO_MASK_GREEN OR ALLEGRO_MASK_BLUE);
    { @exclude }
    ALLEGRO_MASK_RGBA = (ALLEGRO_MASK_RGB OR ALLEGRO_MASK_ALPHA);

(* Set one of several render attributes.

   This function does nothing if the target bitmap is a memory bitmap.
   @param(state Possible render states which can be one of
     @link(ALLEGRO_RENDER_STATE).) *)
  PROCEDURE al_set_render_state (state: ALLEGRO_RENDER_STATE; value: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * transformations.h
 *****************************************************************************)

  TYPE
    ALLEGRO_TRANSFORMptr = ^ALLEGRO_TRANSFORM;
    ALLEGRO_TRANSFORM = RECORD
      m: ARRAY [0..3] OF ARRAY [0..3] OF AL_FLOAT;
    END;

(* Transformations*)
  PROCEDURE al_use_transform (VAR trans: ALLEGRO_TRANSFORM);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_use_projection_transform (VAR trans: ALLEGRO_TRANSFORM);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_copy_transform (OUT dest: ALLEGRO_TRANSFORM; VAR src: ALLEGRO_TRANSFORM);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_identity_transform (OUT trans: ALLEGRO_TRANSFORM);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_build_transform (OUT trans: ALLEGRO_TRANSFORM; x, y, sx, sy, theta: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_build_camera_transform (OUT trans: ALLEGRO_TRANSFORM;
    position_x, position_y, position_z,
    look_x, look_y, look_z,
    up_x, up_y, up_z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_translate_transform (VAR trans: ALLEGRO_TRANSFORM; x, y: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_translate_transform_3d (VAR trans: ALLEGRO_TRANSFORM; x, y, z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_rotate_transform (VAR trans: ALLEGRO_TRANSFORM; theta: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_rotate_transform_3d (VAR trans: ALLEGRO_TRANSFORM; x, y, z, theta: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_scale_transform (VAR trans: ALLEGRO_TRANSFORM; sx, sy: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_scale_transform_3D (VAR trans: ALLEGRO_TRANSFORM; sx, sy, sz: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_transform_coordinates (VAR trans: ALLEGRO_TRANSFORM; OUT x, y: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_transform_coordinates_3d (VAR trans: ALLEGRO_TRANSFORM; OUT x, y, z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_compose_transform (VAR trans, other: ALLEGRO_TRANSFORM);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_current_transform: ALLEGRO_TRANSFORMptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_current_inverse_transform: ALLEGRO_TRANSFORMptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_current_projection_transform: ALLEGRO_TRANSFORMptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_invert_transform (VAR trans: ALLEGRO_TRANSFORM);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_check_inverse (VAR trans: ALLEGRO_TRANSFORM; tol: AL_FLOAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_orthographic_transform (VAR trans: ALLEGRO_TRANSFORM; left, top, n, right, bottom, f: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_perspective_transform (VAR trans: ALLEGRO_TRANSFORM; left, top, n, right, bottom, f: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_horizontal_shear_transform (VAR trans: ALLEGRO_TRANSFORM; theta: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_vertical_shear_transform (VAR trans: ALLEGRO_TRANSFORM; theta: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * shader.h
 *****************************************************************************)

 { TODO: Coming soon. ;) }



(*
 * system.h
 *****************************************************************************)

  TYPE
  (* Pointer to the Allegro system description record.
     For internal use only. *)
    ALLEGRO_SYSTEMptr = AL_POINTER;

(* Like @link(al_install_system), but automatically passes in the version and
   uses the @code(atexit) function visible in the current compilation unit. *)
  FUNCTION al_init: AL_BOOL;



(* Initializes the Allegro system.  No other Allegro functions can be called
   before this (with one or two exceptions).
   @param(version Should always be set to @link(ALLEGRO_VERSION_INT).)
   @param(atexit_ptr If non-@nil, and if hasn’t been done already,
    @code(al_uninstall_system) will be registered as an atexit function.)
   @returns(@true if Allegro was successfully initialized by this function
    call @(or already was initialized previously@), @false if Allegro cannot
    be used.)
   @seealso(al_init)
 *)
  FUNCTION al_install_system (version: AL_INT; atexit_ptr: AL_POINTER): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Closes down the Allegro system.

   In most cases you don't need to call this, because it's called by the
   @code(FINALIZATION) section.
   @seealso(al_init) @seealso(al_install_system)
 *)
  PROCEDURE al_uninstall_system;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Returns @true if Allegro is initialized, otherwise returns @false. *)
  FUNCTION al_is_system_installed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



{ TODO: Some stuff needs the "path.h" section. }



(* This function allows the user to stop the system screensaver from starting
   up if @true is passed, or resets the system back to the default state (the
   state at program start) if @false is passed.
   @returns(@true if the state was set successfully, otherwise @false.)
 *)
  FUNCTION al_inhibit_screensaver (inhibit: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * timer.h
 *
 *  Timer routines.
 *****************************************************************************)

(* Converts microseconds to seconds. *)
  FUNCTION ALLEGRO_USECS_TO_SECS (x: AL_INT): AL_DOUBLE; INLINE;
(* Converts milliseconds to seconds. *)
  FUNCTION ALLEGRO_MSECS_TO_SECS (x: AL_INT): AL_DOUBLE; INLINE;
(* Converts beats per second to seconds. *)
  FUNCTION ALLEGRO_BPS_TO_SECS (x: AL_INT): AL_DOUBLE; INLINE;
(* Converts beats per minute to seconds. *)
  FUNCTION ALLEGRO_BPM_TO_SECS (x: AL_INT): AL_DOUBLE; INLINE;



(* Allocates and initializes a timer. The new timer is initially stopped.

   Usage note:  typical granularity is on the order of microseconds, but with
   some drivers might only be milliseconds.
   @param(speed_secs Seconds per "tick". Must be positive.)
   @returns(If successful, a pointer to a new timer object is returned,
     otherwise @nil is returned.)
  @seealso(al_start_timer) @seealso(al_destroy_timer) *)
  FUNCTION al_create_timer (speed_secs: AL_DOUBLE): ALLEGRO_TIMERptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_destroy_timer (timer: ALLEGRO_TIMERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Starts the timer specified.  From then, the timer's counter will increment
   at a constant rate, and it will begin generating events.  Starting a timer
   that is already started does nothing.  Starting a timer that was stopped
   will reset the timer's counter, effectively restarting the timer from the
   beginning.
   @seealso(al_stop_timer) @seealso(al_get_timer_started)
   @seealso(al_resume_timer) *)
  PROCEDURE al_start_timer (timer: ALLEGRO_TIMERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_stop_timer (timer: ALLEGRO_TIMERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_resume_timer (timer: ALLEGRO_TIMERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_timer_started (CONST timer: ALLEGRO_TIMERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_timer_speed (CONST timer: ALLEGRO_TIMERptr): AL_DOUBLE;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_timer_speed (timer: ALLEGRO_TIMERptr; speed_secs: AL_DOUBLE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_timer_count (CONST timer: ALLEGRO_TIMERptr): AL_INT64;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_timer_count (timer: ALLEGRO_TIMERptr; count: AL_INT64);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_add_timer_count (timer: ALLEGRO_TIMERptr; diff: AL_INT64);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_timer_event_source (timer: ALLEGRO_TIMERptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * tls.h
 *      Thread local storage routines.
 *****************************************************************************)

  CONST
  { @exclude }
    ALLEGRO_STATE_NEW_DISPLAY_PARAMETERS = $0001;
  { @exclude }
    ALLEGRO_STATE_NEW_BITMAP_PARAMETERS  = $0002;
  { @exclude }
    ALLEGRO_STATE_DISPLAY                = $0004;
  { @exclude }
    ALLEGRO_STATE_TARGET_BITMAP          = $0008;
  { @exclude }
    ALLEGRO_STATE_BITMAP                 = $000A; { ALLEGRO_STATE_TARGET_BITMAP + ALLEGRO_STATE_NEW_BITMAP_PARAMETERS, }
  { @exclude }
  { @exclude }
    ALLEGRO_STATE_BLENDER                = $0010;
  { @exclude }
    ALLEGRO_STATE_NEW_FILE_INTERFACE     = $0020;
  { @exclude }
    ALLEGRO_STATE_TRANSFORM              = $0040;
  { @exclude }
    ALLEGRO_STATE_PROJECTION_TRANSFORM   = $0100;
  { @exclude }
    ALLEGRO_STATE_ALL                    = $FFFF;



  TYPE
  (* Opaque type which is passed to
     @link(al_store_state)/@link(al_restore_state).

     The various state kept internally by Allegro can be displayed like this:
@longcode(#
  global
      active system driver
          current config
  per thread
      new bitmap params
      new display params
      active file interface
      errno
      current blending mode
      current display
          deferred drawing
      current target bitmap
          current transformation
          current projection transformation
          current clipping rectangle
          bitmap locking
          current shader
#)
     In general, the only real global state is the active system driver. All
     other global state is per-thread, so if your application has multiple
     separate threads they never will interfere with each other. (Except if
     there are objects accessed by multiple threads of course. Usually you want
     to minimize that though and for the remaining cases use synchronization
     primitives described in the threads section or events described in the
     events section to control inter-thread communication.)
  *)
    ALLEGRO_STATE = RECORD
    { Internally, a thread_local_state structure is placed here. }
      _tls: ARRAY [0..1023] OF AL_CHAR;
    END;

(* Stores part of the state of the current thread in the given
   @code(ALLEGRO_STATE) object. The flags parameter can take any
   bit-combination of these flags:
   @unorderedlist(
    @item(@code(ALLEGRO_STATE_NEW_DISPLAY_PARAMETERS) - new_display_format,
      new_display_refresh_rate, new_display_flags)
    @item(@code(ALLEGRO_STATE_NEW_BITMAP_PARAMETERS) - new_bitmap_format,
      new_bitmap_flags)
    @item(@code(ALLEGRO_STATE_DISPLAY) - current_display)
    @item(@code(ALLEGRO_STATE_TARGET_BITMAP) - target_bitmap)
    @item(@code(ALLEGRO_STATE_BLENDER) - blender)
    @item(@code(ALLEGRO_STATE_TRANSFORM) - current_transformation)
    @item(@code(ALLEGRO_STATE_PROJECTION_TRANSFORM) -
      current_projection_transformation)
    @item(@code(ALLEGRO_STATE_NEW_FILE_INTERFACE) - new_file_interface)
    @item(@code(ALLEGRO_STATE_BITMAP) - same as
      @code(ALLEGRO_STATE_NEW_BITMAP_PARAMETERS) and
      @code(ALLEGRO_STATE_TARGET_BITMAP))
    @item(@code(ALLEGRO_STATE_ALL) - all of the above)
   )
   @seealso(al_restore_state) *)
  PROCEDURE al_store_state (OUT state: ALLEGRO_STATE; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Restores part of the state of the current thread from the given
   @code(ALLEGRO_STATE) object.
   @seealso(al_store_state) *)
  PROCEDURE al_restore_state (VAR state: ALLEGRO_STATE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

IMPLEMENTATION

(*
 * base.h
 *****************************************************************************)

  FUNCTION AL_ID (CONST str: SHORTSTRING): AL_INT;
  BEGIN
    AL_ID := (ORD (str[1]) SHL 24) OR (ORD (str[2]) SHL 16)
	     OR (ORD (str[3]) SHL  8) OR  ORD (str[4])
  END;



(*
 * events.h
 *****************************************************************************)

  FUNCTION ALLEGRO_EVENT_TYPE_IS_USER (t: ALLEGRO_EVENT_TYPE): AL_BOOL;
  BEGIN
    ALLEGRO_EVENT_TYPE_IS_USER := t >= 512
  END;



  FUNCTION ALLEGRO_GET_EVENT_TYPE (CONST str: SHORTSTRING): AL_INT;
  BEGIN
    ALLEGRO_GET_EVENT_TYPE := AL_ID (str)
  END;



(*
 * memory.h
 *****************************************************************************)

  PROCEDURE _al_set_memory_interface_ (iface: AL_POINTER);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME NAME 'al_set_memory_interface';

  PROCEDURE al_restore_memory_interface;
  BEGIN
    _al_set_memory_interface_ (NIL)
  END;



  FUNCTION al_malloc (CONST n: AL_SIZE_T): AL_POINTER;
  BEGIN
    al_malloc := al_malloc_with_context (n, 0, '', '')
  END;



  PROCEDURE al_free (p: AL_POINTER);
  BEGIN
    al_free_with_context (p, 0, '', '')
  END;



  FUNCTION al_realloc (p: AL_POINTER; CONST n: AL_SIZE_T): AL_POINTER;
  BEGIN
    al_realloc := al_realloc_with_context (p, n, 0, '', '')
  END;



  FUNCTION al_calloc (CONST c, n: AL_SIZE_T): AL_POINTER;
  BEGIN
    al_calloc := al_calloc_with_context (c, n, 0, '', '')
  END;



(*
 * system.h
 *****************************************************************************)

  FUNCTION al_init: AL_BOOL;
  BEGIN
    al_init := al_install_system (ALLEGRO_VERSION_INT, NIL)
  END;



(*
 * timer.h
 *****************************************************************************)

  FUNCTION ALLEGRO_USECS_TO_SECS (x: AL_INT): AL_DOUBLE;
  BEGIN
    ALLEGRO_USECS_TO_SECS := x / 1000000
  END;

  FUNCTION ALLEGRO_MSECS_TO_SECS (x: AL_INT): AL_DOUBLE;
  BEGIN
    ALLEGRO_MSECS_TO_SECS := x / 1000
  END;

  FUNCTION ALLEGRO_BPS_TO_SECS (x: AL_INT): AL_DOUBLE;
  BEGIN
    ALLEGRO_BPS_TO_SECS := 1 / x
  END;

  FUNCTION ALLEGRO_BPM_TO_SECS (x: AL_INT): AL_DOUBLE;
  BEGIN
    ALLEGRO_BPM_TO_SECS := 60 / x
  END;

INITIALIZATION
{ Delphi forces an INITIALIZATION section if FINALIZATION is used. }
  ;
{ Suggested by FPC mailing list user. }

{ $if defined(cpui386) or defined(cpux86_64)}
{ SetExceptionMask(GetExceptionMask + [exZeroDivide, exInvalidOp]); }
{ $ENDIF}

FINALIZATION
{ Ensures that we call it, as Pascal hasn't an "atexit" function. }
  al_uninstall_system
END.
