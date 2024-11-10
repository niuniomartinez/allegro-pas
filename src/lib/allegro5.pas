unit allegro5;
(***<Wrapper of the Allegro 5 core library.

  This unit defines core functions, procedures and data types, that aren't in
  add-ons.

  @bold(See also:) @link(getst Getting started) *)
(* Copyright (c) 2012-2024 Guillermo Martínez J. <niunio@users.sourceforge.net>

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

{$INCLUDE allegro5.cfg}

interface

  uses
    al5base, al5strings;

{ The code is distributed in sections, each one wraps a header file.

  Order of sections is the same as C loads them by including the "allegro5.h"
  header file.

  Only comments that starts with ** are included in the documentation.  Most
  documentation is at file srcdoc/allegro5.pds. }

(*
 * base.h
 *
 *   Defines basic stuff needed by pretty much everything else.
 *
 *   By Shawn Hargreaves.
 *************************************************************************)

  const
  (* These "_VERSION" constants exist just to build the ALLEGRO_VERSION_INT one;
     they are defined just to check if DLL/so/dylib files are compatible and
     don't represent the actual Allegro.pas version.  They're defined to use
     version 5.2.6 or later.

     If you want to check or display Allegro.pas' version then Use
     ALLEGRO_PAS_VERSION_STR instead.
   *)
    ALLEGRO_VERSION      =   5;
    ALLEGRO_SUB_VERSION  =   2;
    ALLEGRO_WIP_VERSION  =   6;
    ALLEGRO_RELEASE_NUMBER = 1;

    ALLEGRO_PAS_VERSION_STR = '5.2.0';
  (* Dates aren't for Allegro but for Allegro.pas. *)
    ALLEGRO_DATE_STR = '2024';
    ALLEGRO_DATE = 20241110; { yyyymmdd }
    ALLEGRO_VERSION_INT  = (
           (ALLEGRO_VERSION shl 24)
        or (ALLEGRO_SUB_VERSION shl 16)
        or (ALLEGRO_WIP_VERSION shl  8)
        or  ALLEGRO_RELEASE_NUMBER
    );

  type
    ALLEGRO_USER_MAIN = function (argc: AL_INT; argv: AL_POINTER): AL_INT; CDECL;

  function al_get_allegro_version: AL_UINT32;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_run_main (argc: AL_INT; argv: AL_POINTER; user_main: ALLEGRO_USER_MAIN): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;

  const
    ALLEGRO_PI = 3.14159265358979323846;
    ALLEGRO_TAU = ALLEGRO_PI * 2;

  function AL_ID (const str: AL_STR): AL_INT;



(*
 * altime.h
 *************************************************************************)

  type
    ALLEGRO_TIMEOUTptr = ^ALLEGRO_TIMEOUT;
    ALLEGRO_TIMEOUT = record
      __pad1__, __pad2__: AL_UINT64; {**<@exclude }
    end;



  function al_get_time: AL_DOUBLE;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_rest (seconds: AL_DOUBLE);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_init_timeout (out timeout: ALLEGRO_TIMEOUT; seconds: AL_DOUBLE);
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * color.h
 *************************************************************************)

  type
    ALLEGRO_COLOR = record
    (*** Color component. *)
      r, g, b, a: AL_FLOAT;
    end;

    ALLEGRO_PIXEL_FORMAT = (
      ALLEGRO_PIXEL_FORMAT_ANY = 0,
    (*** Let the driver choose a format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_NO_ALPHA = 1,
    (*** Let the driver choose a format with alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_WITH_ALPHA = 2,
    (*** Let the driver choose a 15 bit format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_15_NO_ALPHA = 3,
    (*** Let the driver choose a 16 bit format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_16_NO_ALPHA = 4,
    (*** Let the driver choose a 16 bit format with alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_16_WITH_ALPHA = 5,
    (*** Let the driver choose a 24 bit format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_24_NO_ALPHA = 6,
    (*** Let the driver choose a 32 bit format without alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_32_NO_ALPHA = 7,
    (*** Let the driver choose a 32 bit format with alpha. *)
      ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA = 8,
    (*** 32 bit *)
      ALLEGRO_PIXEL_FORMAT_ARGB_8888 = 9,
    (*** 32 bit *)
      ALLEGRO_PIXEL_FORMAT_RGBA_8888 = 10,
    (*** 24 bit *)
      ALLEGRO_PIXEL_FORMAT_ARGB_4444 = 11,
    (*** 24 bit *)
      ALLEGRO_PIXEL_FORMAT_RGB_888 = 12,
    (*** 16 bit *)
      ALLEGRO_PIXEL_FORMAT_RGB_565 = 13,
    (*** 15 bit *)
      ALLEGRO_PIXEL_FORMAT_RGB_555 = 14,
    (*** 16 bit *)
      ALLEGRO_PIXEL_FORMAT_RGBA_5551 = 15,
    (*** 16 bit *)
      ALLEGRO_PIXEL_FORMAT_ARGB_1555 = 16,
    (*** 32 bit *)
      ALLEGRO_PIXEL_FORMAT_ABGR_8888 = 17,
    (*** 32 bit *)
      ALLEGRO_PIXEL_FORMAT_XBGR_8888 = 18,
    (*** 24 bit *)
      ALLEGRO_PIXEL_FORMAT_BGR_888 = 19,
    (*** 16 bit *)
      ALLEGRO_PIXEL_FORMAT_BGR_565 = 20,
    (*** 15 bit *)
      ALLEGRO_PIXEL_FORMAT_BGR_555 = 21,
    (*** 32 bit *)
      ALLEGRO_PIXEL_FORMAT_RGBX_8888 = 22,
    (*** 32 bit *)
      ALLEGRO_PIXEL_FORMAT_XRGB_8888 = 23,
    (*** 128 bit *)
      ALLEGRO_PIXEL_FORMAT_ABGR_F32 = 24,
      ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE = 25,
    (*** 16bit *)
      ALLEGRO_PIXEL_FORMAT_RGBA_4444  = 26,
      ALLEGRO_PIXEL_FORMAT_SINGLE_CHANNEL_8 = 27,
      ALLEGRO_PIXEL_FORMAT_COMPRESSED_RGBA_DXT1 = 28,
      ALLEGRO_PIXEL_FORMAT_COMPRESSED_RGBA_DXT3 = 29,
      ALLEGRO_PIXEL_FORMAT_COMPRESSED_RGBA_DXT5 = 30,
      ALLEGRO_NUM_PIXEL_FORMATS
    );

(* Pixel mapping *)
  function al_map_rgb (r, g, b: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_map_rgba (r, g, b, a: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_map_rgb_f (r, g, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_map_rgba_f (r, g, b, a: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_premul_rgba (r, g, b: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_premul_rgba_f (r, g, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_LIB_NAME;

(* Pixel unmapping *)
  procedure al_unmap_rgb (color: ALLEGRO_COLOR; out r, g, b: AL_UCHAR);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_unmap_rgba (color: ALLEGRO_COLOR; out r, g, b, a: AL_UCHAR);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_unmap_rgb_f (color: ALLEGRO_COLOR; out r, g, b: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_unmap_rgba_f (color: ALLEGRO_COLOR; out r, g, b, a: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;

(* Pixel formats *)
  function al_get_pixel_size (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_pixel_format_bits (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_pixel_block_size (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_pixel_block_width (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_pixel_block_height (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * bitmap.h
 *************************************************************************)

  type
  (*** Abstract type representing a bitmap (2D image). *)
    ALLEGRO_BITMAPptr = type AL_POINTER;

  const
    ALLEGRO_MEMORY_BITMAP            = $0001; {**<@exclude }
    _ALLEGRO_KEEP_BITMAP_FORMAT      = $0002; {**<@exclude now a bitmap loader flag }
    ALLEGRO_FORCE_LOCKING            = $0004; {**<@exclude no longer honoured }
    ALLEGRO_NO_PRESERVE_TEXTURE      = $0008; {**<@exclude }
    _ALLEGRO_ALPHA_TEST              = $0010; {**<@exclude now a render state flag }
    _ALLEGRO_INTERNAL_OPENGL         = $0020; {**<@exclude }
    ALLEGRO_MIN_LINEAR               = $0040; {**<@exclude }
    ALLEGRO_MAG_LINEAR               = $0080; {**<@exclude }
    ALLEGRO_MIPMAP                   = $0100; {**<@exclude }
    _ALLEGRO_NO_PREMULTIPLIED_ALPHA  = $0200; {**<@exclude now a bitmap loader flag }
    ALLEGRO_VIDEO_BITMAP             = $0400; {**<@exclude }
    ALLEGRO_CONVERT_BITMAP           = $1000; {**<@exclude }

  procedure al_set_new_bitmap_format (format: ALLEGRO_PIXEL_FORMAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_new_bitmap_flags (flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_new_bitmap_format: ALLEGRO_PIXEL_FORMAT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_new_bitmap_flags: AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_add_new_bitmap_flag (flag: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;

(*** Returns the width of a bitmap in pixels. *)
  function al_get_bitmap_width (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
(*** Returns the height of a bitmap in pixels. *)
  function al_get_bitmap_height (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_bitmap_format (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_PIXEL_FORMAT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_bitmap_flags (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_create_bitmap (w, h: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_destroy_bitmap (Bitmap: ALLEGRO_BITMAPptr);
    CDECL; external ALLEGRO_LIB_NAME;

  procedure al_put_pixel (x, y: AL_INT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_put_blended_pixel (x, y: AL_INT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_pixel (bitmap: ALLEGRO_BITMAPptr; x, y: AL_INT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_LIB_NAME;

(* Masking. *)
  procedure al_convert_mask_to_alpha (bitmap: ALLEGRO_BITMAPptr; mask_color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_LIB_NAME;

(* Clipping.*)
  procedure al_set_clipping_rectangle (x, y, width, height: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_reset_clipping_rectangle;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_get_clipping_rectangle (out x, y, w, h: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;

(* Sub bitmaps. *)
  function al_create_sub_bitmap (parent: ALLEGRO_BITMAPptr; x, y, w, h: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_is_sub_bitmap (bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_parent_bitmap (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_bitmap_x (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_bitmap_y (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_reparent_bitmap (bitmap, parent: ALLEGRO_BITMAPptr; x, y, w, h: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;

(* Miscelaneous. *)
  function al_clone_bitmap (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_convert_bitmap (bitmap: ALLEGRO_BITMAPptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_convert_memory_bitmaps;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * bitmap_draw.h
 *************************************************************************)

  const
  (* Flags for the blitting functions.  Documented at al_draw_bitmap. *)
    ALLEGRO_FLIP_HORIZONTAL = $00001; {**<@exclude }
    ALLEGRO_FLIP_VERTICAL   = $00002; {**<@exclude }

(* Blitting *)
  procedure al_draw_bitmap (bitmap: ALLEGRO_BITMAPptr; dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy, dw, dh: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, angle: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;

(* Tinted blitting *)
  procedure al_draw_tinted_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_tinted_bitmap_region (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_tinted_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy, dw, dh: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_tinted_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, angle: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_tinted_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_tinted_scaled_rotated_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh: AL_FLOAT; tint: ALLEGRO_COLOR; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * path.h
 *************************************************************************)

{ This header is used by Allegro to build file paths.  Both Delphi and FPC
  runtime libraries have equivalent functionality so I'll not add it.
}



(*
 * file.h
 *************************************************************************)

  type
    ALLEGRO_FILEptr = type AL_POINTER;

  (*** Pointer to @link(ALLEGRO_FILE_INTERFACE). *)
    ALLEGRO_FILE_INTERFACEptr = ^ALLEGRO_FILE_INTERFACE;
    ALLEGRO_FILE_INTERFACE = record
      fi_open: function (const path, mode: AL_STR): AL_POINTER; CDECL;
      fi_fclose: function (handle: ALLEGRO_FILEptr): AL_BOOL; CDECL;
      fi_fread: function (f: ALLEGRO_FILEptr; ptr: AL_POINTER; size: AL_SIZE_T): AL_SIZE_T; CDECL;
      fi_fwrite: function (f: ALLEGRO_FILEptr; const ptr: AL_POINTER; size: AL_SIZE_T): AL_SIZE_T; CDECL;
      fi_fflush: function (f: ALLEGRO_FILEptr): AL_BOOL; CDECL;
      fi_ftell: function (f: ALLEGRO_FILEptr): AL_INT64; CDECL;
      fi_fseek: function (f: ALLEGRO_FILEptr; offset: AL_INT64; whence: AL_INT): AL_BOOL; CDECL;
      fi_feof: function (f: ALLEGRO_FILEptr): AL_BOOL; CDECL;
      fi_ferror: function (f: ALLEGRO_FILEptr): AL_INT; CDECL;
      fi_ferrmsg: function (f: ALLEGRO_FILEptr): AL_STRptr; CDECL;
      fi_fclearerr: procedure (f: ALLEGRO_FILEptr); CDECL;
      fi_fungetc: function (f: ALLEGRO_FILEptr; c: AL_INT): AL_INT; CDECL;
      fi_fsize: function (f: ALLEGRO_FILEptr): AL_OFF_T; CDECL;
    end;

  const
  { Not defined by Allegro, but used. }
    AL_EOF = -1; (***<End of file.  Returned by some file functions. *)
  { May be these should be an enum as the original implementation. }
    ALLEGRO_SEEK_SET = 0; (***<Seek relative to beginning of file. *)
    ALLEGRO_SEEK_CUR = 1; (***<Seek relative to current file position. *)
    ALLEGRO_SEEK_END = 2; (***<Seek relative to end of file. *)

  (* The basic operations. *)
    function al_fopen (const path, mode: AL_STR): ALLEGRO_FILEptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fopen_interface (const vt: ALLEGRO_FILE_INTERFACEptr; const path, mode: AL_STR): ALLEGRO_FILEptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_create_file_handle (const vt: ALLEGRO_FILE_INTERFACEptr; userdata: AL_POINTER): ALLEGRO_FILEptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fclose (f: ALLEGRO_FILEptr): AL_BOOL;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fread (f: ALLEGRO_FILEptr; ptr: AL_POINTER; size: AL_SIZE_T): AL_SIZE_T;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fwrite (f: ALLEGRO_FILEptr; const ptr: AL_POINTER; size: AL_SIZE_T): AL_SIZE_T;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fflush (f: ALLEGRO_FILEptr): AL_BOOL;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_ftell (f: ALLEGRO_FILEptr): AL_INT64;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fseek (f: ALLEGRO_FILEptr; offset: AL_INT64; whence: AL_INT): AL_BOOL;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_feof (f: ALLEGRO_FILEptr): AL_BOOL;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_ferror (f: ALLEGRO_FILEptr): AL_INT;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_ferrmsg (f: ALLEGRO_FILEptr): AL_STRptr;
      CDECL; external ALLEGRO_LIB_NAME;
    procedure al_fclearerr (f: ALLEGRO_FILEptr);
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fungetc (f: ALLEGRO_FILEptr; c: AL_INT): AL_INT;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fsize (f: ALLEGRO_FILEptr): AL_INT64;
      CDECL; external ALLEGRO_LIB_NAME;

  (* Convenience functions. *)
    function al_fgetc (f: ALLEGRO_FILEptr): AL_INT;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fputc (f: ALLEGRO_FILEptr; c: AL_INT): AL_INT;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fread16le (f: ALLEGRO_FILEptr): AL_INT16;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fread16be (f: ALLEGRO_FILEptr): AL_INT16;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fwrite16le (f: ALLEGRO_FILEptr; w: AL_INT16): AL_SIZE_T;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fwrite16be (f: ALLEGRO_FILEptr; w: AL_INT16): AL_SIZE_T;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fread32le (f: ALLEGRO_FILEptr): AL_INT32;
      CDECL; external ALLEGRO_LIB_NAME;
   function al_fread32be (f: ALLEGRO_FILEptr): AL_INT32;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fwrite32le (f: ALLEGRO_FILEptr; l: AL_INT32): AL_SIZE_T;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fwrite32be (f: ALLEGRO_FILEptr; l: AL_INT32): AL_SIZE_T;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fgets (f: ALLEGRO_FILEptr; const p: AL_STRptr; max: AL_SIZE_T): AL_STRptr
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fget_ustr (f: ALLEGRO_FILEptr): ALLEGRO_USTRptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_fputs (f: ALLEGRO_FILEptr; const p: AL_STR): AL_INT;
      CDECL; external ALLEGRO_LIB_NAME;

{
/* Specific to stdio backend. */
AL_FUNC(ALLEGRO_FILE*, al_fopen_fd, (int fd, const char *mode));
AL_FUNC(ALLEGRO_FILE*, al_make_temp_file, (const char *tmpl,
      ALLEGRO_PATH **ret_path));
}

  (* Specific to slices. *)
    function al_fopen_slice (fp: ALLEGRO_FILEptr; initial_size: AL_SIZE_T; const mode: AL_STR): ALLEGRO_FILEptr;
      CDECL; external ALLEGRO_LIB_NAME;

  (* Thread local state. *)
    function al_get_new_file_interface: ALLEGRO_FILE_INTERFACEptr;
      CDECL; external ALLEGRO_LIB_NAME;
    procedure al_set_new_file_interface (const file_interface: ALLEGRO_FILE_INTERFACEptr);
      CDECL; external ALLEGRO_LIB_NAME;
    procedure al_set_standard_file_interface;
      CDECL; external ALLEGRO_LIB_NAME;

  (* ALLEGRO_FILE field accessors *)
    function al_get_file_userdata (f: ALLEGRO_FILEptr): AL_POINTER;
      CDECL; external ALLEGRO_LIB_NAME;



(*
 * bitmap_io.h
 *************************************************************************)

(* Bitmap loader flag. *)
  const
  { May be these should be an enum as the original implementation. }
    ALLEGRO_KEEP_BITMAP_FORMAT     = $0002; {**<@exclude }
    ALLEGRO_NO_PREMULTIPLIED_ALPHA = $0200; {**<@exclude }
    ALLEGRO_KEEP_INDEX             = $0800; {**<@exclude }


  type
  (*** Used by @link(al_register_bitmap_loader). *)
    ALLEGRO_IIO_LOADER_FUNCTION = function (const filename: AL_STR; flags: AL_INT): ALLEGRO_BITMAPptr; CDECL;
  (*** Used by @link(al_register_bitmap_loader_f). *)
    ALLEGRO_IIO_FS_LOADER_FUNCTION = function (fp: ALLEGRO_FILEptr; flags: AL_INT): ALLEGRO_BITMAPptr; CDECL;
  (*** Used by @link(al_register_bitmap_saver). *)
    ALLEGRO_IIO_SAVER_FUNCTION = function (const filename: AL_STR; bitmap: ALLEGRO_BITMAPptr): AL_BOOL; CDECL;
  (*** Used by @link(al_register_bitmap_saver_f). *)
    ALLEGRO_IIO_FS_SAVER_FUNCTION = function (fp: ALLEGRO_FILEptr; bitmap: ALLEGRO_BITMAPptr): AL_BOOL; CDECL;
  (*** Used by @link(al_register_bitmap_identifier). *)
    ALLEGRO_IIO_IDENTifIER_FUNCTION = function (fp: ALLEGRO_FILEptr): AL_BOOL; CDECL;

  function al_register_bitmap_loader (const ext: AL_STR; loader: ALLEGRO_IIO_LOADER_FUNCTION): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_register_bitmap_saver (const ext: AL_STR; saver: ALLEGRO_IIO_SAVER_FUNCTION): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_register_bitmap_loader_f (const ext: AL_STR; fs_loader: ALLEGRO_IIO_FS_LOADER_FUNCTION): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_register_bitmap_saver_f (const ext: AL_STR; fs_saver: ALLEGRO_IIO_FS_SAVER_FUNCTION): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_register_bitmap_identifier (const ext: AL_STR; identifier: ALLEGRO_IIO_IDENTifIER_FUNCTION): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_load_bitmap (const filename: AL_STR): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_load_bitmap_flags (const filename: AL_STR; flags: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_load_bitmap_f (fp: ALLEGRO_FILEptr; const ident: AL_STR): ALLEGRO_BITMAPptr;
  function al_load_bitmap_flags_f (fp: ALLEGRO_FILEptr; const ident: AL_STR; flags: AL_INT): ALLEGRO_BITMAPptr;
  function al_save_bitmap (const filename: AL_STR; bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_save_bitmap_f (fp: ALLEGRO_FILEptr; const ident: AL_STR; bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_identify_bitmap_f (fp: ALLEGRO_FILEptr): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_identify_bitmap (const filename: AL_STR): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * bitmap_lock.h
 *************************************************************************)

  type
  (*** Locking flags. *)
    ALLEGRO_LOCK = (
      ALLEGRO_LOCK_READWRITE  = 0,
      ALLEGRO_LOCK_READONLY   = 1,
      ALLEGRO_LOCK_WRITEONLY  = 2
    );

  (*** Pointer to @link(ALLEGRO_LOCKED_REGION). *)
    ALLEGRO_LOCKED_REGIONptr = ^ALLEGRO_LOCKED_REGION;
    ALLEGRO_LOCKED_REGION = record
      data: AL_VOIDptr;
      format,
      pitch,
      pixel_size: AL_INT;
    end;



  function al_lock_bitmap (bitmap: ALLEGRO_BITMAPptr; format: ALLEGRO_PIXEL_FORMAT; flags: ALLEGRO_LOCK): ALLEGRO_LOCKED_REGIONptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_lock_bitmap_region (bitmap: ALLEGRO_BITMAPptr; x, y, width, height: AL_INT; format: ALLEGRO_PIXEL_FORMAT; flags: ALLEGRO_LOCK): ALLEGRO_LOCKED_REGIONptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_lock_bitmap_blocked (bitmap: ALLEGRO_BITMAPptr; flags: ALLEGRO_LOCK): ALLEGRO_LOCKED_REGIONptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_lock_bitmap_region_blocked (bitmap: ALLEGRO_BITMAPptr; x_block, y_block, width_block, height_block, flags: ALLEGRO_LOCK): ALLEGRO_LOCKED_REGIONptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_unlock_bitmap (bitmap: ALLEGRO_BITMAPptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_is_bitmap_locked (bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * blender.h
 *************************************************************************)

  type
  (*** @exclude Blending modes. *)
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



  {** @exclude }
    ALLEGRO_BLEND_OPERATIONS = (
      ALLEGRO_ADD                  = 0,
      ALLEGRO_SRC_MINUS_DEST       = 1,
      ALLEGRO_DEST_MINUS_SRC       = 2,
      ALLEGRO_NUM_BLEND_OPERATIONS
    );

  procedure al_set_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_blend_color (color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_get_blender (out op: ALLEGRO_BLEND_OPERATIONS; out source, dest: ALLEGRO_BLEND_MODE);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_blend_color: ALLEGRO_COLOR;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_separate_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE;
				     alpha_op: ALLEGRO_BLEND_OPERATIONS; alpha_source, alpha_dest: ALLEGRO_BLEND_MODE);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_get_separate_blender (out op: ALLEGRO_BLEND_OPERATIONS; out source, dest: ALLEGRO_BLEND_MODE;
				     out alpha_op: ALLEGRO_BLEND_OPERATIONS; out alpha_source, alpha_dest: ALLEGRO_BLEND_MODE);
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * events.h
 *************************************************************************)

  type
    ALLEGRO_EVENT_TYPE = AL_UINT;

  const
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

(* Function: ALLEGRO_EVENT_TYPE_IS_USER
 *
 *    1 <= n < 512  - builtin events
 *  512 <= n < 1024 - reserved user events (for addons)
 * 1024 <= n        - unreserved user events
 *)
  function ALLEGRO_EVENT_TYPE_IS_USER (t: ALLEGRO_EVENT_TYPE): AL_BOOL; inline;

  function ALLEGRO_GET_EVENT_TYPE (const str: AL_STR): AL_INT; inline;


  type
  (* These pointers are declared here as they're needed by the event system. *)
    ALLEGRO_DISPLAYptr = type AL_POINTER;
    ALLEGRO_JOYSTICKptr = type AL_POINTER;
  (*** Pointer to keyboard. *)
    ALLEGRO_KEYBOARDptr = type AL_POINTER;
  (*** Pointer to mouse. *)
    ALLEGRO_MOUSEptr = type AL_POINTER;
    ALLEGRO_TIMERptr = type AL_POINTER;


  (*** Pointer to @link(ALLEGRO_EVENT_SOURCE). *)
    ALLEGRO_EVENT_SOURCEptr = ^ALLEGRO_EVENT_SOURCE;
    ALLEGRO_EVENT_SOURCE = record
      __pad : array [0..31] of AL_INT; {**<@exclude }
    end;


  (*
   * Event structures
   *
   * All event types have the following fields in common.
   *
   *  ftype     -- the type of event this is
   *  timestamp -- when this event was generated
   *  source    -- which event source generated this event
   *
   * For people writing event sources: The common fields must be at the
   * very start of each event structure.
   *)

    ALLEGRO_ANY_EVENT = record
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The event source which generated the event. *)
      source: ALLEGRO_EVENT_SOURCEptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
    end;

    ALLEGRO_DISPLAY_EVENT = record
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The display that generated the event. *)
      source: ALLEGRO_DISPLAYptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
      x, y: AL_INT;
      width, height: AL_INT;
      orientation: AL_INT;
    end;

    ALLEGRO_JOYSTICK_EVENT = record
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The joystick which generated the event. *)
      source: ALLEGRO_JOYSTICKptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
      id: ALLEGRO_JOYSTICKptr;
      stick: AL_INT;
    (*** The axis number on the stick, counting from zero. *)
      axis: AL_INT;
    (*** The axis position, from -1.0 to +1.0. *)
      pos: AL_FLOAT;
    (*** The button which was pressed, counting from zero. *)
      button: AL_INT;
    end;

    ALLEGRO_KEYBOARD_EVENT = record
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The keyboard which generated the event. *)
      source: ALLEGRO_KEYBOARDptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
    (*** The display which had keyboard focus when the event occurred. *)
      display: ALLEGRO_DISPLAYptr;
      keycode: AL_INT;
      unichar: AL_INT;
      modifiers: AL_UINT;
    (*** Indicates if this is a repeated character. *)
      frepeat: AL_BOOL;
    end;

    ALLEGRO_MOUSE_EVENT = record
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The mouse which generated the event. *)
      source: ALLEGRO_MOUSEptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
    (*** The display which had mouse focus. *)
      display: ALLEGRO_DISPLAYptr;
      x, y, z, w: AL_INT;
      dx, dy, dz, dw: AL_INT;
    (*** The mouse button which was pressed or released, numbering from 1. *)
      button: AL_UINT;
    (*** Pressure, ranging from 0.0 to 1.0. *)
      pressure: AL_FLOAT;
    end;

  (*** Contains the timer events information. @seealso(ALLEGRO_EVENT) *)
    ALLEGRO_TIMER_EVENT = record
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The timer which generated the event. *)
      source: ALLEGRO_TIMERptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
    (*** The timer count value. *)
      count: AL_INT64;
    (*** @exclude undocumented (?) *)
      error: AL_DOUBLE;
    end;

    ALLEGRO_TOUCH_EVENT = record
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The event source which generated the event. *)
      source: AL_POINTER;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
    (*** The display which was touched.  *)
      display: AL_POINTER;
      id: AL_INT;
    (*** The coordinate of the touch in pixels. *)
      x, y: AL_DOUBLE;
    (*** Movement speed in pixels. *)
      dx, dy: AL_DOUBLE;
    (*** Whether this is the only/first touch or an additional touch. *)
      primary: AL_BOOL;
    end;

  (*** Pointer to the user event descriptor. @seealso(ALLEGRO_USER_EVENT) *)
    ALLEGRO_USER_EVENT_DESCRIPTORptr = type AL_POINTER;

  (*** Pointer to @link(ALLEGRO_USER_EVENT). *)
    ALLEGRO_USER_EVENTptr = ^ALLEGRO_USER_EVENT;
    ALLEGRO_USER_EVENT = record
    (*** Event identifier. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** Pointer to the event source. *)
      source: AL_POINTER;
    (*** When the event was emited. *)
      timestamp: AL_DOUBLE;
    (*** @exclude *)
      __internal__descr: ALLEGRO_USER_EVENT_DESCRIPTORptr;
    (*** Extra data. *)
      data1,
    (*** Extra data. *)
      data2,
    (*** Extra data. *)
      data3,
    (*** Extra data. *)
      data4: AL_DATA_PTR_T;
    end;

  (*** Pointer to @link(ALLEGRO_EVENT) *)
    ALLEGRO_EVENTptr = ^ALLEGRO_EVENT;
    ALLEGRO_EVENT = record
      case LongInt OF
      (*** The event type. *)
	0: ( ftype: ALLEGRO_EVENT_TYPE );
	1: ( any: ALLEGRO_ANY_EVENT );
      (*** Information of display events. *)
	2: ( display: ALLEGRO_DISPLAY_EVENT );
      (*** Information of joysitck events. *)
	3: ( joystick: ALLEGRO_JOYSTICK_EVENT );
      (*** Information of keyboard events. *)
	4: ( keyboard: ALLEGRO_KEYBOARD_EVENT );
      (*** Information of mouse events. *)
	5: ( mouse: ALLEGRO_MOUSE_EVENT );
      (*** Information of timer events. *)
	6: ( timer: ALLEGRO_TIMER_EVENT );
      (*** Information of touch events. *)
        7: ( touch: ALLEGRO_TOUCH_EVENT );
      (*** Information of user events. *)
	8: ( user: ALLEGRO_USER_EVENT );
    end;

  (*** User event destructor. @seealso(al_emit_user_event) *)
    ALLEGRO_EVENT_DTOR_PROC = procedure (evt: ALLEGRO_USER_EVENTptr); CDECL;

(* Event sources. *)
  procedure al_init_user_event_source (source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_destroy_user_event_source (source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; external ALLEGRO_LIB_NAME;
(* The second argument is ALLEGRO_EVENTptr instead of ALLEGRO_USER_EVENTptr
 * to prevent users passing a pointer to a too-short structure.
 *)
  function al_emit_user_event (source: ALLEGRO_EVENT_SOURCEptr; Event: ALLEGRO_EVENTptr; dtor: ALLEGRO_EVENT_DTOR_PROC): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_unref_user_event (event: ALLEGRO_USER_EVENTptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_event_source_data (source: ALLEGRO_EVENT_SOURCEptr; data: AL_POINTER);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_event_source_data (const source: ALLEGRO_EVENT_SOURCEptr): AL_POINTER;
    CDECL; external ALLEGRO_LIB_NAME;



(* Event queues. *)
  type
    ALLEGRO_EVENT_QUEUEptr = type AL_POINTER;

  function al_create_event_queue: ALLEGRO_EVENT_QUEUEptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_destroy_event_queue (queue: ALLEGRO_EVENT_QUEUEptr);
    CDECL; external ALLEGRO_LIB_NAME;
(*** Returns @true if the event source is registered. @seealso(al_register_event_source) *)
  function al_is_event_source_registered (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_register_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_unregister_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_pause_event_queue (queue: ALLEGRO_EVENT_QUEUEptr; pause: AL_BOOL);
    CDECL; external ALLEGRO_LIB_NAME;
(*** Returns @true if the event queue is paused. @seealso(al_pause_event_queue) *)
  function al_is_event_queue_paused (const queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_is_event_queue_empty (queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL;external ALLEGRO_LIB_NAME;
  function al_get_next_event (queue: ALLEGRO_EVENT_QUEUEptr; out ret_event: ALLEGRO_EVENT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_peek_next_event (queue: ALLEGRO_EVENT_QUEUEptr; out ret_event: ALLEGRO_EVENT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_drop_next_event (queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
(*** Drops all events, if any, from the queue. @seealso(al_drop_next_event) @seealso(al_is_event_queue_empty) *)
  procedure al_flush_event_queue (queue: ALLEGRO_EVENT_QUEUEptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_wait_for_event (queue: ALLEGRO_EVENT_QUEUEptr; ret_event: ALLEGRO_EVENTptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_wait_for_event_timed (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr; secs: AL_FLOAT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_wait_for_event_until (queue: ALLEGRO_EVENT_QUEUEptr; event: ALLEGRO_EVENTptr; var timeout: ALLEGRO_TIMEOUT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * display.h
 *************************************************************************)

  const
  (* Possible bit combinations for the flags parameter of al_set_new_display_flags. *)
    ALLEGRO_DEFAULT                     = 0 shl 0; {**<@exclude }
    ALLEGRO_WINDOWED                    = 1 shl 0; {**<@exclude }
    ALLEGRO_FULLSCREEN                  = 1 shl 1; {**<@exclude }
    ALLEGRO_OPENGL                      = 1 shl 2; {**<@exclude }
    ALLEGRO_DIRECT3D_INTERNAL           = 1 shl 3; {**<@exclude }
    ALLEGRO_RESIZABLE                   = 1 shl 4; {**<@exclude }
    ALLEGRO_FRAMELESS                   = 1 shl 5; {**<@exclude }
    ALLEGRO_NOFRAME                     = ALLEGRO_FRAMELESS; {**<@exclude }
    ALLEGRO_GENERATE_EXPOSE_EVENTS      = 1 shl 6; {**<@exclude }
    ALLEGRO_OPENGL_3_0                  = 1 shl 7; {**<@exclude }
    ALLEGRO_OPENGL_FORWARD_COMPATIBLE   = 1 shl 8; {**<@exclude }
    ALLEGRO_FULLSCREEN_WINDOW           = 1 shl 9; {**<@exclude }
    ALLEGRO_MINIMIZED                   = 1 shl 10; {**<@exclude }
    ALLEGRO_PROGRAMMABLE_PIPELINE       = 1 shl 11; {**<@exclude }
    ALLEGRO_GTK_TOPLEVEL_INTERNAL       = 1 shl 12; {**<@exclude Needed by (unimplemented) "native dialogs" unit(?). }
    ALLEGRO_MAXIMIZED                   = 1 shl 13; {**<@exclude }
    ALLEGRO_OPENGL_ES_PROFILE           = 1 shl 14; {**<@exclude }

  type
  (*** @exclude Possible parameters for al_set_display_option.

     Make sure to update ALLEGRO_EXTRA_DISPLAY_SETTINGS if you modify
     anything here.
   *)
    ALLEGRO_DISPLAY_OPTIONS = (
      ALLEGRO_RED_SIZE = 0, {**<@exclude }
      ALLEGRO_GREEN_SIZE = 1, {**<@exclude }
      ALLEGRO_BLUE_SIZE = 2, {**<@exclude }
      ALLEGRO_ALPHA_SIZE = 3, {**<@exclude }
      ALLEGRO_RED_SHIFT = 4, {**<@exclude }
      ALLEGRO_GREEN_SHIFT = 5, {**<@exclude }
      ALLEGRO_BLUE_SHIFT = 6, {**<@exclude }
      ALLEGRO_ALPHA_SHIFT = 7, {**<@exclude }
      ALLEGRO_ACC_RED_SIZE = 8, {**<@exclude }
      ALLEGRO_ACC_GREEN_SIZE = 9, {**<@exclude }
      ALLEGRO_ACC_BLUE_SIZE = 10, {**<@exclude }
      ALLEGRO_ACC_ALPHA_SIZE = 11, {**<@exclude }
      ALLEGRO_STEREO = 12, {**<@exclude }
      ALLEGRO_AUX_BUFFERS = 13, {**<@exclude }
      ALLEGRO_COLOR_SIZE = 14, {**<@exclude }
      ALLEGRO_DEPTH_SIZE = 15, {**<@exclude }
      ALLEGRO_STENCIL_SIZE = 16, {**<@exclude }
      ALLEGRO_SAMPLE_BUFFERS = 17, {**<@exclude }
      ALLEGRO_SAMPLES = 18, {**<@exclude }
      ALLEGRO_RENDER_METHOD = 19, {**<@exclude }
      ALLEGRO_FLOAT_COLOR = 20, {**<@exclude }
      ALLEGRO_FLOAT_DEPTH = 21, {**<@exclude }
      ALLEGRO_SINGLE_BUFFER = 22, {**<@exclude }
      ALLEGRO_SWAP_METHOD = 23, {**<@exclude }
      ALLEGRO_COMPATIBLE_DISPLAY = 24, {**<@exclude }
      ALLEGRO_UPDATE_DISPLAY_REGION = 25, {**<@exclude }
      ALLEGRO_VSYNC = 26, {**<@exclude }
      ALLEGRO_MAX_BITMAP_SIZE = 27, {**<@exclude }
      ALLEGRO_SUPPORT_NPOT_BITMAP = 28, {**<@exclude }
      ALLEGRO_CAN_DRAW_INTO_BITMAP = 29, {**<@exclude }
      ALLEGRO_SUPPORT_SEPARATE_ALPHA = 30, {**<@exclude }
      ALLEGRO_AUTO_CONVERT_BITMAPS = 31, {**<@exclude }
      ALLEGRO_SUPPORTED_ORIENTATIONS = 32, {**<@exclude }
      ALLEGRO_OPENGL_MAJOR_VERSION = 33, {**<@exclude }
      ALLEGRO_OPENGL_MINOR_VERSION = 34, {**<@exclude }
      ALLEGRO_DEFAULT_SHADER_PLATFORM = 35, {**<@exclude }
      ALLEGRO_DISPLAY_OPTIONS_COUNT {**<@exclude }
    );

  const
    ALLEGRO_DONTCARE = 0; {**<@exclude }
    ALLEGRO_REQUIRE = 1; {**<@exclude }
    ALLEGRO_SUGGEST = 2; {**<@exclude }



(* Bitflags so they can be used for the ALLEGRO_SUPPORTED_ORIENTATIONS option. *)
    ALLEGRO_DISPLAY_ORIENTATION_UNKNOWN = 0; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_0_DEGREES = 1; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_90_DEGREES = 2; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_180_DEGREES = 4; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_270_DEGREES = 8; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_PORTRAIT = 5; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_LANDSCAPE = 10; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_ALL = 15; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_FACE_UP = 16; {**<@exclude }
    ALLEGRO_DISPLAY_ORIENTATION_FACE_DOWN = 32; {**<@exclude }

  { Formelly part of the primitives addon. }
    _ALLEGRO_PRIM_MAX_USER_ATTR = 10; {**<@exclude }

{ pointer ALLEGRO_DISPLAYptr declared at section "events.h". }

    ALLEGRO_NEW_WINDOW_TITLE_MAX_SIZE = 255;



  procedure al_set_new_display_refresh_rate (refresh_rate: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_new_display_flags (flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_new_display_refresh_rate: AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_new_display_flags: AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;

  procedure al_set_new_window_title (const title: AL_STR);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_new_window_title: AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_display_width (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_display_height (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_display_format (display: ALLEGRO_DISPLAYptr): ALLEGRO_PIXEL_FORMAT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_display_refresh_rate (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_display_flags (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_display_orientation (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_display_flag
    (display: ALLEGRO_DISPLAYptr; flag: AL_INT; onoff: AL_BOOL): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_create_display (w, h: AL_INT): ALLEGRO_DISPLAYptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_destroy_display (display: ALLEGRO_DISPLAYptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_current_display: ALLEGRO_DISPLAYptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_target_bitmap (Bitmap: ALLEGRO_BITMAPptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_target_backbuffer (display: ALLEGRO_DISPLAYptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_backbuffer (display: ALLEGRO_DISPLAYptr): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_target_bitmap: ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_acknowledge_resize (display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_resize_display (display: ALLEGRO_DISPLAYptr; width, height: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_flip_display;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_update_display_region (x, y, Width, height: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_is_compatible_bitmap (bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_wait_for_vsync: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_display_event_source (display: ALLEGRO_DISPLAYptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; external ALLEGRO_LIB_NAME;

  procedure al_set_display_icon (display: ALLEGRO_DISPLAYptr; icon: ALLEGRO_BITMAPptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_display_icons (display: ALLEGRO_DISPLAYptr; num_icons: AL_INT; var icons: array of ALLEGRO_BITMAPptr);
    CDECL; external ALLEGRO_LIB_NAME;

(* Stuff for multihead/window management *)
  function al_get_new_display_adapter: AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_new_display_adapter (adapter: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_new_window_position (x, y: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_get_new_window_position (out x, y: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_window_position (display: ALLEGRO_DISPLAYptr; x, y: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_get_window_position (display: ALLEGRO_DISPLAYptr; out x, y: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_window_constraints (display: ALLEGRO_DISPLAYptr; min_w, min_h, max_w, max_h: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_window_constraints (display: ALLEGRO_DISPLAYptr; out min_w, min_h, max_w, max_h: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_apply_window_constraints (display: ALLEGRO_DISPLAYptr; onoff: AL_BOOL);
    CDECL; external ALLEGRO_LIB_NAME;

  procedure al_set_window_title (display: ALLEGRO_DISPLAYptr; const title: AL_STR);
    CDECL; external ALLEGRO_LIB_NAME;

(* Defined in display settings.c *)
  procedure al_set_new_display_option (option: ALLEGRO_DISPLAY_OPTIONS; value, importance: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_new_display_option (option: ALLEGRO_DISPLAY_OPTIONS; var importance: AL_INT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_reset_new_display_options;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_display_option (display: ALLEGRO_DISPLAYptr; option: ALLEGRO_DISPLAY_OPTIONS; value: AL_INT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_display_option (display: ALLEGRO_DISPLAYptr; option: ALLEGRO_DISPLAY_OPTIONS): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;

(* Deferred drawing *)
  procedure al_hold_bitmap_drawing (hold: AL_BOOL);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_is_bitmap_drawing_held: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

(* Miscellaneous. *)
  procedure al_acknowledge_drawing_halt (display: ALLEGRO_DISPLAYptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_acknowledge_drawing_resume (display: ALLEGRO_DISPLAYptr);
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * clipboard.h
 *
 *   Clipboard handling.
 *************************************************************************)

  function al_get_clipboard_text (display: ALLEGRO_DISPLAYptr): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_clipboard_text
    (display: ALLEGRO_DISPLAYptr; const text: AL_STR): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_clipboard_has_text (display: ALLEGRO_DISPLAYptr): AL_BOOL
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * config.h
 *************************************************************************)

  type
  (*** An abstract configuration structure. @seealso(al_create_config) *)
    ALLEGRO_CONFIGptr = type AL_POINTER;
  { Iterating sections and entries means to define the internals of these
    structures.  Instead of that, use TIniFile defined by both Delphi and Free
    Pascal.

    ALLEGRO_CONFIG_SECTIONptr = AL_POINTER;
    ALLEGRO_CONFIG_ENTRYptr = AL_POINTER;
  }

    function al_create_config: ALLEGRO_CONFIGptr;
      CDECL; external ALLEGRO_LIB_NAME;
    procedure al_add_config_section (config: ALLEGRO_CONFIGptr; const name: AL_STR);
      CDECL; external ALLEGRO_LIB_NAME;
    procedure al_set_config_value (config: ALLEGRO_CONFIGptr; const section, key, value: AL_STR);
      CDECL; external ALLEGRO_LIB_NAME;
    procedure al_add_config_comment (config: ALLEGRO_CONFIGptr; const section, comment: AL_STR);
      CDECL; external ALLEGRO_LIB_NAME;
    function al_get_config_value (const config: ALLEGRO_CONFIGptr; const section, key: AL_STR): AL_STRptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_load_config_file (const filename: AL_STR): ALLEGRO_CONFIGptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_load_config_file_f (fp: ALLEGRO_FILEptr): ALLEGRO_CONFIGptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_save_config_file (const filename: AL_STR; const config: ALLEGRO_CONFIGptr): AL_BOOL;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_save_config_file_f (fp: ALLEGRO_FILEptr; const config: ALLEGRO_CONFIGptr): AL_BOOL;
      CDECL; external ALLEGRO_LIB_NAME;
    procedure al_merge_config_into (master: ALLEGRO_CONFIGptr; const add: ALLEGRO_CONFIGptr);
      CDECL; external ALLEGRO_LIB_NAME;
    function al_merge_config (const cfg1, cfg2: ALLEGRO_CONFIGptr): ALLEGRO_CONFIGptr;
      CDECL; external ALLEGRO_LIB_NAME;
    procedure al_destroy_config (config: ALLEGRO_CONFIGptr);
      CDECL; external ALLEGRO_LIB_NAME;
    function al_remove_config_section (config: ALLEGRO_CONFIGptr; const section: AL_STR): AL_BOOL;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_remove_config_key (config: ALLEGRO_CONFIGptr; const section, key: AL_STR): AL_BOOL;
      CDECL; external ALLEGRO_LIB_NAME;

  { Iterating sections and entries means to define the internals of some
    structures.  Instead of that, use TIniFile provided by both Delphi and Free
    Pascal.

    function al_get_first_config_section
      (const config: ALLEGRO_CONFIGptr; out iterator: ALLEGRO_CONFIG_SECTIONptr): AL_STRptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_get_next_config_section (var iterator: ALLEGRO_CONFIG_SECTIONptr): AL_STRptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_get_first_config_entry (
      const config: ALLEGRO_CONFIGptr; const section: AL_STR;
      out iterator: ALLEGRO_CONFIG_ENTRYptr
    ): AL_STRptr;
      CDECL; external ALLEGRO_LIB_NAME;
    function al_get_next_config_entry (var iterator: ALLEGRO_CONFIG_ENTRYptr): AL_STRptr;
      CDECL; external ALLEGRO_LIB_NAME;
  }



(*
 * cpu.h
 *
 *   CPU and system information handling.
 *************************************************************************)

  function al_get_cpu_count: AL_INT; CDECL; external ALLEGRO_LIB_NAME;
  function al_get_ram_size: AL_INT;  CDECL; external ALLEGRO_LIB_NAME;



(*
 * debug.h
 *
 *   Debug facilities.
 *
 *   By Shawn Hargreaves.
 *************************************************************************)

{ At the moment I'll not include this header. }



(*
 * drawing.h
 *************************************************************************)

  procedure al_clear_to_color (color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_clear_depth_buffer (x: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_draw_pixel (x, y: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * error.h
 *
 *   Error handling.
 *************************************************************************)

  const
  (* Constants for error values. *)
    AL_EDOM   = 1;
    AL_ERANGE = 2;

  function al_get_errno: AL_INT; CDECL; external ALLEGRO_LIB_NAME;
(*** Sets the error number for the calling thread. @seealso(al_get_errno) *)
  procedure al_set_errno (errnum: AL_INT); CDECL; external ALLEGRO_LIB_NAME;



(*
 * fixed.h
 *
 *   Fixed point type.
 *
 *   By Shawn Hargreaves.
 *************************************************************************)

  type
  (*** A fixed point number. *)
    AL_FIXED = type AL_INT32;
  (*** @exclude. *)
    __FIXED_ARRAY__ = array [0..1023] of AL_FIXED;

{$INCLUDE trigtabl.inc}

  const
  (*** This constant gives a ratio which can be used to convert a fixed point
     number in binary angle format to a fixed point number in radians. *)
    AL_FIXTORAD_R: AL_FIXED = 1608;    { al_ftofix (tau/256) }
  (*** This constant gives a ratio which can be used to convert a fixed point
     number in radians to a fixed point number in binary angle format. *)
    AL_RADTOFIX_R: AL_FIXED = 2670177; { al_ftofix (256/2pi) }



(*
 * fmaths.h
 *
 *   Fixed point math routines.
 *
 *   By Shawn Hargreaves.
 *************************************************************************)

(*** This finds out the non negative square root of `x'. *)
  function al_fixsqrt (x: AL_FIXED): AL_FIXED; cdecl;
    external ALLEGRO_LIB_NAME;
(*** Fixed point hypotenuse (returns the square root of `x*x + y*y'). *)
  function al_fixhypot (x, y: AL_FIXED): AL_FIXED; cdecl;
    external ALLEGRO_LIB_NAME;
(*** This function finds the inverse tangent of a value using a lookup table. *)
  function al_fixatan (x: AL_FIXED): AL_FIXED; cdecl;
    external ALLEGRO_LIB_NAME;
  function al_fixatan2 (y, x: AL_FIXED): AL_FIXED; cdecl;
    external ALLEGRO_LIB_NAME;



(*
 * fmaths.inl
 *
 *   Fixed point math inline functions (generic C).
 *
 *   By Shawn Hargreaves.
 *************************************************************************)

(*** Converts a floating point value to fixed point. *)
  function al_ftofix (x: AL_DOUBLE): AL_FIXED;
    inline;
(*** Converts fixed point to floating point. *)
  function al_fixtof (x: AL_FIXED): AL_DOUBLE;
    inline;

(*** Safe function to add fixed point numbers clamping overflow. *)
  function al_fixadd (x, y: AL_FIXED): AL_FIXED;
    inline;
(*** Safe function to subtract fixed point numbers clamping underflow. *)
  function al_fixsub (x, y: AL_FIXED): AL_FIXED;
    inline;
  function al_fixmul (x, y: AL_FIXED): AL_FIXED;
    inline;
  function al_fixdiv (x, y: AL_FIXED): AL_FIXED;
    inline;

(*** Returns the greatest integer not greater than x. *)
  function al_fixfloor (x: AL_FIXED): AL_FIXED; inline;
(*** Returns the smallest integer not less than x. *)
  function al_fixceil (x: AL_FIXED): AL_FIXED; inline;
(*** Converts an integer to fixed point. *)
  function al_itofix (x: AL_INT): AL_FIXED; inline;
(*** Returns the integer part of x, which is alwais smaller than (or equal to)
  X in absolute value. *)
  function al_fixtrunc (x: AL_FIXED): AL_INT; inline;
(*** Converts fixed point to integer, rounding as required to the nearest
  integer. *)
  function al_fixtoi (x: AL_FIXED): AL_INT; inline;

(*** This function finds the cosine of a value using a lookup table. *)
  function al_fixcos (x: AL_FIXED): AL_FIXED; inline;
(*** This function finds the sine of a value using a lookup table. *)
  function al_fixsin (x: AL_FIXED): AL_FIXED; inline;
(*** This function finds the tangent of a value using a lookup table. *)
  function al_fixtan (x: AL_FIXED): AL_FIXED; inline;
(*** This function finds the inverse cosine of a value using a lookup table. *)
  function al_fixacos (x: AL_FIXED): AL_FIXED; inline;
(*** This function finds the inverse sine of a value using a lookup table. *)
  function al_fixasin (x: AL_FIXED): AL_FIXED; inline;



(*
 * fshook.h
 *
 *   File system hooks.
 *************************************************************************)

{ Both Delphi and Free Pascal RTLs (RunTime Libraries) provide a complete set
  of functions and procedures to work with the file system, I'll not implement
  this.

  If you think it is interesting to have this stuff, may be I add it as an
  add-on.
}



(*
 * fullscreen_mode.h
 *************************************************************************)

  type
  (*** Pointer to @link(ALLEGRO_DISPLAY_MODE). *)
    ALLEGRO_DISPLAY_MODEptr = ^ALLEGRO_DISPLAY_MODE;
    ALLEGRO_DISPLAY_MODE = record
      width, height, format, refresh_rate: AL_INT;
    end;

  function al_get_num_display_modes: AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_display_mode (index: AL_INT; out mode: ALLEGRO_DISPLAY_MODE): ALLEGRO_DISPLAY_MODEptr;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * joystick.h
 *
 *   Joystick routines.
 *************************************************************************)

  const
  (* internal values *)
    _AL_MAX_JOYSTICK_AXES    =  3; {**<@exclude }
    _AL_MAX_JOYSTICK_STICKS  = 16; {**<@exclude }
    _AL_MAX_JOYSTICK_BUTTONS = 32; {**<@exclude }

  type
    ALLEGRO_JOYSTICK_STATE = record
      stick: array [0.._AL_MAX_JOYSTICK_STICKS - 1] of record
        axis: array [0.._AL_MAX_JOYSTICK_AXES - 1] of AL_FLOAT; { -1.0 to 1.0 }
      end;
      button: array [0.._AL_MAX_JOYSTICK_BUTTONS - 1] of AL_INT; { 0 to 32767 }
    end;



    ALLEGRO_JOYFLAGS = (
      ALLEGRO_JOYFLAG_DIGITAL  = $01,
      ALLEGRO_JOYFLAG_ANALOGUE = $02
    );

  function al_install_joystick: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_uninstall_joystick;
    CDECL; external ALLEGRO_LIB_NAME;
(*** Returns @true if @link(al_install_joystick) was called successfully. *)
  function al_is_joystick_installed: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_reconfigure_joysticks: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_num_joysticks: AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_joystick (joyn: AL_INT): ALLEGRO_JOYSTICKptr;
    CDECL; external ALLEGRO_LIB_NAME;
(*** This procedure currently does nothing. @seealso(al_get_joystick) *)
  procedure al_release_joystick (j: ALLEGRO_JOYSTICKptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_joystick_active (j: ALLEGRO_JOYSTICKptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_joystick_name (j: ALLEGRO_JOYSTICKptr): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_joystick_num_sticks (j: ALLEGRO_JOYSTICKptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_joystick_stick_flags (j: ALLEGRO_JOYSTICKptr; stick: AL_INT): AL_INT; (* junk? *)
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_joystick_stick_name (j: ALLEGRO_JOYSTICKptr; stick: AL_INT): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_joystick_num_axes (j: ALLEGRO_JOYSTICKptr; stick: AL_INT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_joystick_axis_name (j: ALLEGRO_JOYSTICKptr; stick, axis: AL_INT): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_joystick_num_buttons (j: ALLEGRO_JOYSTICKptr): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_joystick_button_name (j: ALLEGRO_JOYSTICKptr; buttonn: AL_INT): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;

  procedure al_get_joystick_state (j: ALLEGRO_JOYSTICKptr; out ret_state: ALLEGRO_JOYSTICK_STATE);
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_joystick_event_source: ALLEGRO_EVENT_SOURCEptr;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * keycodes.h
 *************************************************************************)

{$INCLUDE keycodes.inc}



(*
 * keyboard.h
 *
 *   Keyboard routines.
 *************************************************************************)

  type
    ALLEGRO_KEYBOARD_STATE = record
      display: ALLEGRO_DISPLAYptr; (* public *)
    (*** @exclude internal *)
      __key_down__internal__: array [0..((ALLEGRO_KEY_MAX + 31) div 32) - 1] of AL_UINT;
    end;


(*** Returns @true if @link(al_install_keyboard) was called successfully. *)
  function al_is_keyboard_installed: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_install_keyboard: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_uninstall_keyboard;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_set_keyboard_leds (leds: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

(*** Converts the given keycode to a description of the key. *)
  function al_keycode_to_name (keycode: AL_INT): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;

  procedure al_get_keyboard_state (out ret_state: ALLEGRO_KEYBOARD_STATE);
    CDECL; external ALLEGRO_LIB_NAME;
    {
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_SRC)
    }
  procedure al_clear_keyboard_state (display: ALLEGRO_DISPLAYptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_key_down (var state: ALLEGRO_KEYBOARD_STATE; keycode: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_keyboard_event_source: ALLEGRO_EVENT_SOURCEptr;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * mouse.h
 *
 *  Mouse routines.
 *************************************************************************)

  const
  (*** @exclude Allow up to four extra axes for future expansion. *)
    ALLEGRO_MOUSE_MAX_EXTRA_AXES = 4;

  type
    ALLEGRO_MOUSE_STATE = record
    (* (x, y) Primary mouse position
     * (z) Mouse wheel position (1D 'wheel'), or,
     * (w, z) Mouse wheel position (2D 'ball')
     * display - the display the mouse is on (coordinates are relative to this)
     * pressure - the pressure applied to the mouse (for stylus/tablet)
     *)
      x,
      y,
      z, w: AL_INT;
    (*** @exclude *)
      more_axes: array [0..(ALLEGRO_MOUSE_MAX_EXTRA_AXES - 1)] of AL_INT;
      buttons: AL_INT;
      pressure: AL_FLOAT;
      display: ALLEGRO_DISPLAYptr;
    end;



(*** Returns @true if @link(al_install_mouse) was called successfully. *)
  function al_is_mouse_installed: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_install_mouse: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_uninstall_mouse;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_mouse_num_buttons: AL_UINT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_mouse_num_axes: AL_UINT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_mouse_xy (display: ALLEGRO_DISPLAYptr; x, y: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_mouse_z (z: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_mouse_w (w: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_mouse_axis (axis, value: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_get_mouse_state (out ret_state: ALLEGRO_MOUSE_STATE);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_mouse_button_down (var state: ALLEGRO_MOUSE_STATE; button: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_mouse_state_axis (var state: ALLEGRO_MOUSE_STATE; axis: AL_INT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_mouse_cursor_position (out ret_x, ret_y: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_grab_mouse (display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_ungrab_mouse: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_mouse_wheel_precision (precision: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_mouse_wheel_precision: AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_mouse_event_source: ALLEGRO_EVENT_SOURCEptr;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * touch_input.h
 *
 *   Touch input routines.
 *************************************************************************)

{ TODO: Not yet. }



(*
 * haptic.h
 *
 *   Haptic (that is, force feedback) routines for Allegro.
 *
 *   By Beoran.
 *************************************************************************)

{ TODO: Not yet.  Needs touch_input.h. }



(*
 * memory.h
 *
 *   Memory management routines.
 *************************************************************************)

  type
    ALLEGRO_MEMORY_INTERFACE = record
      mi_malloc: function (n: AL_SIZE_T; line: AL_INT; const afile, func: AL_STR): AL_POINTER; CDECL;
      mi_free: procedure (ptr: AL_POINTER; line: AL_INT; const afile, func: AL_STR); CDECL;
      mi_realloc: function (ptr: AL_POINTER; n: AL_SIZE_T; line: AL_INT; const afile, func: AL_STR): AL_POINTER; CDECL;
      mi_calloc: function (n, count: AL_SIZE_T; line: AL_INT; const afile, func: AL_STR): AL_POINTER; CDECL;
    end;

  procedure al_set_memory_interface (var iface: ALLEGRO_MEMORY_INTERFACE);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_restore_memory_interface;


  function al_malloc (const n: AL_SIZE_T): AL_POINTER; inline;
  procedure al_free (p: AL_POINTER); inline;
  function al_realloc (p: AL_POINTER; const n: AL_SIZE_T): AL_POINTER; inline;
  function al_calloc (const c, n: AL_SIZE_T): AL_POINTER; inline;


  function al_malloc_with_context
    (n: AL_SIZE_T; line: AL_INT; const afile, func: AL_STR): AL_POINTER;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_free_with_context
    (ptr: AL_POINTER; line: AL_INT; const afile, func: AL_STR);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_realloc_with_context
    (ptr: AL_POINTER; n: AL_SIZE_T; line: AL_INT;
     const afile, func: AL_STR): AL_POINTER;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_calloc_with_context
    (n, count: AL_SIZE_T; line: AL_INT; const afile, func: AL_STR): AL_POINTER;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * monitor.h
 *************************************************************************)

  type
    ALLEGRO_MONITOR_INFO = record
      x1, y1, x2, y2: AL_INT;
    end;

  const
    ALLEGRO_DEFAULT_DISPLAY_ADAPTER = -1; {**<@exclude }

  function al_get_num_video_adapters: AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_monitor_info (adapter: AL_INT; out info: ALLEGRO_MONITOR_INFO): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_monitor_dpi (adapter: AL_INT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME;
{ #if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_SRC)
AL_FUNC(int, al_get_monitor_refresh_rate, (int adapter));
#endif}



(*
 * mouse_cursor.h
 *************************************************************************)

  type
  (*** Pointer to a custom mouse cursor *)
    ALLEGRO_MOUSE_CURSORptr = type AL_POINTER;

    ALLEGRO_SYSTEM_MOUSE_CURSOR = (
      ALLEGRO_SYSTEM_MOUSE_CURSOR_CUSTOM      = -1, { New! }
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



  function al_create_mouse_cursor (sprite: ALLEGRO_BITMAPptr; xfocus, yfocus: AL_INT): ALLEGRO_MOUSE_CURSORptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_destroy_mouse_cursor (cursor: ALLEGRO_MOUSE_CURSORptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_mouse_cursor (display: ALLEGRO_DISPLAYptr; cursor: ALLEGRO_MOUSE_CURSORptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_system_mouse_cursor (display: ALLEGRO_DISPLAYptr; cursor_id: ALLEGRO_SYSTEM_MOUSE_CURSOR): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_show_mouse_cursor (display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_hide_mouse_cursor (display: ALLEGRO_DISPLAYptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * render_state.h
 *************************************************************************)

  type
    ALLEGRO_RENDER_STATE = (
    (* ALLEGRO_ALPHA_TEST was the name of a rare bitmap flag only used on the
       Wiz port.  Reuse the name but retain the same value. *)
      ALLEGRO_ALPHA_TEST = $0010,
      ALLEGRO_WRITE_MASK,
      ALLEGRO_DEPTH_TEST,
      ALLEGRO_DEPTH_FUNCTION,
      ALLEGRO_ALPHA_FUNCTION,
      ALLEGRO_ALPHA_TEST_VALUE
    );

  const
    ALLEGRO_RENDER_NEVER = 0; {**<@exclude }
    ALLEGRO_RENDER_ALWAYS = 1; {**<@exclude }
    ALLEGRO_RENDER_LESS = 2; {**<@exclude }
    ALLEGRO_RENDER_EQUAL = 3; {**<@exclude }
    ALLEGRO_RENDER_LESS_EQUAL = 4; {**<@exclude }
    ALLEGRO_RENDER_GREATER = 5; {**<@exclude }
    ALLEGRO_RENDER_NOT_EQUAL = 6; {**<@exclude }
    ALLEGRO_RENDER_GREATER_EQUAL = 7; {**<@exclude }

    ALLEGRO_MASK_RED = 1 shl 0; {**<@exclude }
    ALLEGRO_MASK_GREEN = 1 shl 1; {**<@exclude }
    ALLEGRO_MASK_BLUE = 1 shl 2; {**<@exclude }
    ALLEGRO_MASK_ALPHA = 1 shl 3; {**<@exclude }
    ALLEGRO_MASK_DEPTH = 1 shl 4; {**<@exclude }
    ALLEGRO_MASK_RGB = (ALLEGRO_MASK_RED or ALLEGRO_MASK_GREEN or ALLEGRO_MASK_BLUE); {**<@exclude }
    ALLEGRO_MASK_RGBA = (ALLEGRO_MASK_RGB or ALLEGRO_MASK_ALPHA); {**<@exclude }

  procedure al_set_render_state (state: ALLEGRO_RENDER_STATE; value: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * transformations.h
 *************************************************************************)

  type
  (*** Pointer to a @link(ALLEGRO_TRANSFORM). *)
    ALLEGRO_TRANSFORMptr = ^ALLEGRO_TRANSFORM;
    ALLEGRO_TRANSFORM = record
      m: array [0..3] of array [0..3] of AL_FLOAT;
    end;

(* Transformations *)
  procedure al_use_transform (var trans: ALLEGRO_TRANSFORM);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_use_projection_transform (var trans: ALLEGRO_TRANSFORM);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_copy_transform (out dest: ALLEGRO_TRANSFORM; var src: ALLEGRO_TRANSFORM);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_identity_transform (out trans: ALLEGRO_TRANSFORM);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_build_transform (out trans: ALLEGRO_TRANSFORM; x, y, sx, sy, theta: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_build_camera_transform (out trans: ALLEGRO_TRANSFORM; position_x, position_y, position_z, look_x, look_y, look_z, up_x, up_y, up_z: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_translate_transform (var trans: ALLEGRO_TRANSFORM; x, y: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_translate_transform_3d (var trans: ALLEGRO_TRANSFORM; x, y, z: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_rotate_transform (var trans: ALLEGRO_TRANSFORM; theta: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_rotate_transform_3d (var trans: ALLEGRO_TRANSFORM; x, y, z, theta: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_scale_transform (var trans: ALLEGRO_TRANSFORM; sx, sy: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_scale_transform_3D (var trans: ALLEGRO_TRANSFORM; sx, sy, sz: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_transform_coordinates (var trans: ALLEGRO_TRANSFORM; var x, y: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_transform_coordinates_3d (var trans: ALLEGRO_TRANSFORM; var x, y, z: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_transform_coordinates_4d (var trans: ALLEGRO_TRANSFORM; var x, y, z, w: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_transform_coordinates_3d_projective (var trans: ALLEGRO_TRANSFORM; var x, y, z: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_compose_transform (var trans, other: ALLEGRO_TRANSFORM);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_current_transform: ALLEGRO_TRANSFORMptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_current_inverse_transform: ALLEGRO_TRANSFORMptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_current_projection_transform: ALLEGRO_TRANSFORMptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_invert_transform (var trans: ALLEGRO_TRANSFORM);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_transpose_transform (var trans: ALLEGRO_TRANSFORM);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_check_inverse (var trans: ALLEGRO_TRANSFORM; tol: AL_FLOAT): AL_BOOL; inline;
  procedure al_orthographic_transform (var trans: ALLEGRO_TRANSFORM; left, top, n, right, bottom, f: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_perspective_transform (var trans: ALLEGRO_TRANSFORM; left, top, n, right, bottom, f: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_horizontal_shear_transform (var trans: ALLEGRO_TRANSFORM; theta: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_vertical_shear_transform (var trans: ALLEGRO_TRANSFORM; theta: AL_FLOAT);
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * shader.h
 *************************************************************************)

  type
    ALLEGRO_SHADERptr = type AL_POINTER;

    ALLEGRO_SHADER_TYPE = (
      ALLEGRO_VERTEX_SHADER = 1,
      ALLEGRO_PIXEL_SHADER = 2
    );



    ALLEGRO_SHADER_PLATFORM = (
      ALLEGRO_SHADER_AUTO = 0,
    (*** OpenGL Shading Language. *)
      ALLEGRO_SHADER_GLSL = 1,
    (*** High Level Shader Language (for Direct3D). *)
      ALLEGRO_SHADER_HLSL = 2,
    (*** Like @code(ALLEGRO_SHADER_AUTO), but pick a more minimalimplementation
         that supports only basic alpha blending..
     *)
      ALLEGRO_SHADER_AUTO_MINIMAL = 3,
    (*** Minimal GLSL shader. *)
      ALLEGRO_SHADER_GLSL_MINIMAL = 4,
    (*** Minimal HLSL shader. *)
      ALLEGRO_SHADER_HLSL_MINIMAL = 5,
    (*** HLSL shader using shader model 3.0. *)
      ALLEGRO_SHADER_HLSL_SM_3_0 = 6
    );



  const
  (* Shader variable names. *)
    ALLEGRO_SHADER_VAR_COLOR           = 'al_color';           {**<@exclude }
    ALLEGRO_SHADER_VAR_POS             = 'al_pos';             {**<@exclude }
    ALLEGRO_SHADER_VAR_PROJVIEW_MATRIX = 'al_projview_matrix'; {**<@exclude }
    ALLEGRO_SHADER_VAR_TEX             = 'al_tex';             {**<@exclude }
    ALLEGRO_SHADER_VAR_TEXCOORD        = 'al_texcoord';        {**<@exclude }
    ALLEGRO_SHADER_VAR_TEX_MATRIX      = 'al_tex_matrix';      {**<@exclude }
    ALLEGRO_SHADER_VAR_USER_ATTR       = 'al_user_attr_';      {**<@exclude }
    ALLEGRO_SHADER_VAR_USE_TEX         = 'al_use_tex';         {**<@exclude }
    ALLEGRO_SHADER_VAR_USE_TEX_MATRIX  = 'al_use_tex_matrix';  {**<@exclude }
    ALLEGRO_SHADER_VAR_ALPHA_TEST      = 'al_alpha_test';      {**<@exclude }
    ALLEGRO_SHADER_VAR_ALPHA_FUNCTION  = 'al_alpha_func';      {**<@exclude }
    ALLEGRO_SHADER_VAR_ALPHA_TEST_VALUE= 'al_alpha_test_val';  {**<@exclude }

  function al_create_shader (platform: ALLEGRO_SHADER_PLATFORM): ALLEGRO_SHADERptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_attach_shader_source (shader: ALLEGRO_SHADERptr; aType: ALLEGRO_SHADER_TYPE; const Source: AL_STR): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_attach_shader_source_file (shader: ALLEGRO_SHADERptr; aType: ALLEGRO_SHADER_TYPE; const filename: AL_STR): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_build_shader (shader: ALLEGRO_SHADERptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_shader_log (shader: ALLEGRO_SHADERptr): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_shader_platform (shader: ALLEGRO_SHADERptr): ALLEGRO_SHADER_PLATFORM;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_use_shader (shader: ALLEGRO_SHADERptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_destroy_shader (shader: ALLEGRO_SHADERptr);
    CDECL; external ALLEGRO_LIB_NAME;

  function al_set_shader_sampler (const name: AL_STR; bitmap: ALLEGRO_BITMAPptr; aUnit: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_shader_matrix (const name: AL_STR; var matrix: ALLEGRO_TRANSFORM): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_shader_int (const name: AL_STR; i: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_shader_float (const name: AL_STR; f: AL_FLOAT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_shader_int_vector (const name: AL_STR; num_components: AL_INT; i: AL_INTptr; num_elems: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_shader_float_vector (const name: AL_STR; num_components: AL_INT; f: AL_FLOATptr; num_elems: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_set_shader_bool (const name: AL_STR; b: AL_BOOL): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_get_default_shader_source (platform: ALLEGRO_SHADER_PLATFORM; aType: ALLEGRO_SHADER_TYPE): AL_STRptr;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * system.h
 ************************************************************************)

{ Some stuff here need the "path.h" section.

  By the way, both Free pascal and Delphi provide functions to get the same
  information, so I'll not add it at the moment.
}

  type
    ALLEGRO_SYSTEM_ID = (
      ALLEGRO_SYSTEM_ID_UNKNOWN    = 0,
      ALLEGRO_SYSTEM_ID_ANDROID    = $414E4452, { AL_ID ('ANDR') }
      ALLEGRO_SYSTEM_ID_IPHONE     = $4950484F, { AL_ID ('IPHO') }
      ALLEGRO_SYSTEM_ID_MACOSX     = $4F535820, { AL_ID ('OSX ') }
      ALLEGRO_SYSTEM_ID_RASPERRYPI = $52415350, { AL_ID ('RASP') }
      ALLEGRO_SYSTEM_ID_SDL        = $53444C32, { AL_ID ('SDL2') }
      ALLEGRO_SYSTEM_ID_WINDOWS    = $57494E44, { AL_ID ('WIND') }
      ALLEGRO_SYSTEM_ID_GP32XWIZ   = $57495A20, { AL_ID ('WIZ ') }
      ALLEGRO_SYSTEM_ID_XGLX       = $58474C58  { AL_ID ('XGLX') }
    );


  function al_init: AL_BOOL; inline;

  function al_install_system (version: AL_INT; atexit_ptr: AL_POINTER): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_uninstall_system;
    CDECL; external ALLEGRO_LIB_NAME;
(*** Returns @true if Allegro is initialized, otherwise returns @false. *)
  function al_is_system_installed: AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_system_config: ALLEGRO_CONFIGptr;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_system_id: ALLEGRO_SYSTEM_ID;
    CDECL; external ALLEGRO_LIB_NAME;

  function al_inhibit_screensaver (inhibit: AL_BOOL): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * threads.h
 *
 *   Thread routines.
 *************************************************************************)

{ Allegro's thread code doesn't work in Pascal (neither Free Pascal or Delphi
  compilers) so it was removed.
}



(*
 * timer.h
 *
 *   Timer routines.
 *************************************************************************)

(*** Converts microseconds to seconds. *)
  function ALLEGRO_USECS_TO_SECS (x: AL_INT): AL_DOUBLE; inline;
(*** Converts milliseconds to seconds. *)
  function ALLEGRO_MSECS_TO_SECS (x: AL_INT): AL_DOUBLE; inline;
(*** Converts beats per second to seconds. *)
  function ALLEGRO_BPS_TO_SECS (x: AL_INT): AL_DOUBLE; inline;
(*** Converts beats per minute to seconds. *)
  function ALLEGRO_BPM_TO_SECS (x: AL_INT): AL_DOUBLE; inline;



  function al_create_timer (speed_secs: AL_DOUBLE): ALLEGRO_TIMERptr;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_destroy_timer (timer: ALLEGRO_TIMERptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_start_timer (timer: ALLEGRO_TIMERptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_stop_timer (timer: ALLEGRO_TIMERptr);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_resume_timer (timer: ALLEGRO_TIMERptr);
    CDECL; external ALLEGRO_LIB_NAME;
(*** Returns @true if the timer specified is currently started. *)
  function al_get_timer_started (const timer: ALLEGRO_TIMERptr): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_timer_speed (const timer: ALLEGRO_TIMERptr): AL_DOUBLE;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_timer_speed (timer: ALLEGRO_TIMERptr; speed_secs: AL_DOUBLE);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_timer_count (const timer: ALLEGRO_TIMERptr): AL_INT64;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_timer_count (timer: ALLEGRO_TIMERptr; count: AL_INT64);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_add_timer_count (timer: ALLEGRO_TIMERptr; diff: AL_INT64);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_timer_event_source (timer: ALLEGRO_TIMERptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; external ALLEGRO_LIB_NAME;



(*
 * tls.h
 *
 *   Thread local storage routines.
 *************************************************************************)

  const
    ALLEGRO_STATE_NEW_DISPLAY_PARAMETERS = $0001; {**<@exclude }
    ALLEGRO_STATE_NEW_BITMAP_PARAMETERS  = $0002; {**<@exclude }
    ALLEGRO_STATE_DISPLAY                = $0004; {**<@exclude }
    ALLEGRO_STATE_TARGET_BITMAP          = $0008; {**<@exclude }
    ALLEGRO_STATE_BITMAP                 = $000A; {**<@exclude
                                                   ALLEGRO_STATE_TARGET_BITMAP
                                       + ALLEGRO_STATE_NEW_BITMAP_PARAMETERS, }
    ALLEGRO_STATE_BLENDER                = $0010; {**<@exclude }
    ALLEGRO_STATE_NEW_FILE_INTERFACE     = $0020; {**<@exclude }
    ALLEGRO_STATE_TRANSFORM              = $0040; {**<@exclude }
    ALLEGRO_STATE_PROJECTION_TRANSFORM   = $0100; {**<@exclude }
    ALLEGRO_STATE_ALL                    = $FFFF; {**<@exclude }



  type
    ALLEGRO_STATE = record
    {** @exclude Internally, a thread_local_state structure is placed here. }
      ftls: array [0..1023] of AL_CHAR;
    end;

  procedure al_store_state (out state: ALLEGRO_STATE; flags: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_restore_state (var state: ALLEGRO_STATE);
    CDECL; external ALLEGRO_LIB_NAME;


{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.

  On Delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here.
}
{@exclude}
  function _al_load_bitmap_f (fp: ALLEGRO_FILEptr; const ident: AL_STRptr): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME name 'al_load_bitmap_f';
{@exclude}
  function _al_load_bitmap_flags_f (fp: ALLEGRO_FILEptr; const ident: AL_STRptr; flags: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_LIB_NAME name 'al_load_bitmap_flags_f';
{@exclude}
  procedure _al_set_memory_interface_ (iface: AL_POINTER);
    CDECL; external ALLEGRO_LIB_NAME name 'al_set_memory_interface';
{@exclude}
  function _al_check_inverse (var trans: ALLEGRO_TRANSFORM; tol: AL_FLOAT): AL_INT;
    CDECL; external ALLEGRO_LIB_NAME name 'al_check_inverse';
{$IF FALSE} { see thread.h explanation.}
{@exclude}
  procedure _al_join_thread (outer: ALLEGRO_THREADptr; ret_value: AL_POINTER);
    CDECL; external ALLEGRO_LIB_NAME name 'al_join_thread';
{$ENDIF}

implementation

(*
 * base.h
 *************************************************************************)

  function AL_ID (const str: AL_STR): AL_INT;
  begin
    AL_ID := (Ord (str[1]) shl 24) or (Ord (str[2]) shl 16)
          or (Ord (str[3]) shl  8) or  Ord (str[4])
  end;



(*
 * bitmap_io.h
 *************************************************************************)

  function al_load_bitmap_f (fp: ALLEGRO_FILEptr; const ident: AL_STR)
    : ALLEGRO_BITMAPptr;
  begin
    if ident = '' then
      Result := _al_load_bitmap_f (fp, Nil)
    else
      Result := _al_load_bitmap_f (fp, AL_STRptr (ident))
  end;



  function al_load_bitmap_flags_f (
    fp: ALLEGRO_FILEptr;
    const ident: AL_STR;
    flags: AL_INT
  ): ALLEGRO_BITMAPptr;
  begin
    if ident = '' then
      Result := _al_load_bitmap_flags_f (fp, Nil, flags)
    else
      Result := _al_load_bitmap_flags_f (fp, AL_STRptr (ident), flags)
  end;



(*
 * events.h
 *************************************************************************)

  function ALLEGRO_EVENT_TYPE_IS_USER (t: ALLEGRO_EVENT_TYPE): AL_BOOL;
  begin
    ALLEGRO_EVENT_TYPE_IS_USER := t >= 512
  end;



  function ALLEGRO_GET_EVENT_TYPE (const str: AL_STR): AL_INT;
  begin
    ALLEGRO_GET_EVENT_TYPE := AL_ID (str)
  end;



(*
 * fmaths.inl
 *************************************************************************)

  function al_ftofix (x: AL_DOUBLE): AL_FIXED;
  begin
    if x > 32767.0 then
    begin
      al_set_errno (AL_ERANGE);
      Result := $7FFFFFFF
    end
    else if x < -32767.0 then
    begin
      al_set_errno (AL_ERANGE);
      Result := -$7FFFFFFF
    end
    else if x < 0 then
      Result := Trunc (x * 65536 - 0.5)
    else
      Result := Trunc (x * 65536 + 0.5)
  end;



  function al_fixtof (x: AL_FIXED): al_double;
  begin
    Result := x / 65536.0
  end;



  function al_fixadd (x, y: AL_FIXED): AL_FIXED;
  begin
    Result := x + y;
    if Result >= 0 then
    begin
      if (x < 0) and (y < 0) then
      begin
        al_set_errno (AL_ERANGE);
        Result := -$7FFFFFFF
      end
    end
    else begin
      if (x > 0) and (y > 0) then
      begin
        al_set_errno (AL_ERANGE);
        Result := $7FFFFFFF
      end
    end
  end;



  function al_fixsub (x, y: AL_FIXED): AL_FIXED;
  begin
    Result := x - y;
    if Result >= 0 then
    begin
      if (x < 0) and (y > 0) then
      begin
        al_set_errno (AL_ERANGE);
        Result := -$7FFFFFFF
      end
    end
    else begin
      if (x > 0) and (y < 0) then
      begin
        al_set_errno (AL_ERANGE);
        Result := $7FFFFFFF
      end
    end
  end;



  function al_fixmul (x, y: AL_FIXED): AL_FIXED;
  begin
  { Martin Kalbfuss suggested this. }
    Result := AL_FIXED (( AL_INT64 ( x ) * y ) shr 16)
  end;



  function al_fixdiv (x, y: AL_FIXED): AL_FIXED;
  begin
    if y = 0 then
    begin
      al_set_errno (AL_ERANGE);
      if x < 0 then
        Result := -$7FFFFFFF
      else
        Result := $7FFFFFFF
    end
    else
      Result := al_ftofix (al_fixtof (x) / al_fixtof (y))
  end;



  function al_fixfloor (x: AL_FIXED): AL_FIXED;
  begin
    if x < 0 then
      Result := not ((not x) shr 16)
    else
      Result := x shr 16
  end;



  function al_fixceil (x: AL_FIXED): AL_FIXED;
  begin
    if x > $7FFF0000 then
    begin
      al_set_errno (AL_ERANGE);
      Exit ($7FFF)
    end;
    Result := al_fixfloor (x + $FFFF)
  end;



  function al_itofix (x: AL_INT): AL_FIXED;
  begin
    Result := x shl 16
  end;



  function al_fixtrunc (x: AL_FIXED): AL_INT;
  begin
    if x < 0 then
    { SHR doesn't keep the sign bit. }
      Result := AL_INT ((x shr 16) or $ffff0000)
    else
      Result := x shr 16
  end;



  function al_fixtoi (x: AL_FIXED): AL_INT;
  begin
    Result := al_fixfloor (x) + ((x and $8000) shr 15)
  end;



  function al_fixcos (x: AL_FIXED): AL_FIXED;
  begin
    Result := _al_fix_cos_tbl[((x + $4000) shr 15) and $1ff]
  end;



  function al_fixsin (x: AL_FIXED): AL_FIXED;
  begin
    Result := _al_fix_cos_tbl[((x - $400000 + $4000) shr 15) and $1ff]
  end;



  function al_fixtan (x: AL_FIXED): AL_FIXED;
  begin
    Result := _al_fix_tan_tbl[((x + $4000) shr 15) and $ff];
  end;



  function al_fixacos (x: AL_FIXED): AL_FIXED;
  begin
    if (-65536 > x) or (x > 65536) then
    begin
      al_set_errno (AL_EDOM);
      Result := 0
    end
    else
      Result := _al_fix_acos_tbl [(x + 65536 + 127) shr 8]
  end;



  function al_fixasin (x: AL_FIXED): AL_FIXED;
  begin
    if (-65536 > x) or (x > 65536) then
    begin
      al_set_errno (AL_EDOM);
      Result := 0
    end
    else
      Result := $00400000 - _al_fix_acos_tbl [(x + 65536 + 127) shr 8];
  end;



(*
 * memory.h
 *************************************************************************)

  procedure al_restore_memory_interface;
  begin
    _al_set_memory_interface_ (Nil)
  end;



  function al_malloc (const n: AL_SIZE_T): AL_POINTER;
  begin
    al_malloc := al_malloc_with_context (n, 0, '', '')
  end;



  procedure al_free (p: AL_POINTER);
  begin
    al_free_with_context (p, 0, '', '')
  end;



  function al_realloc (p: AL_POINTER; const n: AL_SIZE_T): AL_POINTER;
  begin
    al_realloc := al_realloc_with_context (p, n, 0, '', '')
  end;



  function al_calloc (const c, n: AL_SIZE_T): AL_POINTER;
  begin
    al_calloc := al_calloc_with_context (c, n, 0, '', '')
  end;



(*
 * transformation.h
 *************************************************************************)

  function al_check_inverse (var trans: ALLEGRO_TRANSFORM; tol: AL_FLOAT): AL_BOOL;
  begin
    Result := _al_check_inverse (trans, tol) = 1
  end;



(*
 * system.h
 *************************************************************************)

  function al_init: AL_BOOL;
  begin
    al_init := al_install_system (ALLEGRO_VERSION_INT, Nil)
  end;



(*
 * threads.h
 *************************************************************************)

 {$IF FALSE See explanation in the INTERFACE section. }

  procedure al_join_thread (outer: ALLEGRO_THREADptr);
  begin
    _al_join_thread (outer, Nil)
  end;

  procedure al_join_thread_ex
    (outer: ALLEGRO_THREADptr; out ret_value: AL_POINTER);
  begin
    _al_join_thread (outer, @ret_value)
  end;

{$ENDIF}



(*
 * timer.h
 *************************************************************************)

  function ALLEGRO_USECS_TO_SECS (x: AL_INT): AL_DOUBLE;
  begin
    ALLEGRO_USECS_TO_SECS := x / 1000000
  end;

  function ALLEGRO_MSECS_TO_SECS (x: AL_INT): AL_DOUBLE;
  begin
    ALLEGRO_MSECS_TO_SECS := x / 1000
  end;

  function ALLEGRO_BPS_TO_SECS (x: AL_INT): AL_DOUBLE;
  begin
    ALLEGRO_BPS_TO_SECS := 1 / x
  end;

  function ALLEGRO_BPM_TO_SECS (x: AL_INT): AL_DOUBLE;
  begin
    ALLEGRO_BPM_TO_SECS := 60 / x
  end;

initialization
{ Next code suggested by FPC mailing list user.  This should fix some issues
  with memory.  It was removed as it seems to be fixed by Allegro itself.
  Anyway I'll keep it here in case the bug re-appears again.
}

{ $IF DEFINED(cpui386) OR DEFINED(cpux86_64)}
{ SetExceptionMask(GetExceptionMask + [exZeroDivide, exInvalidOp]); }
{ $ENDIF}

{ Delphi forces an initialization section if finalization is used. }
  ;

finalization
{ Ensures that we call it, as Pascal hasn't an "atexit" function. }
  al_uninstall_system
end.
