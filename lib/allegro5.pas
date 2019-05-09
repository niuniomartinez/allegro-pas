UNIT Allegro5;
(***<Wrapper of the Allegro 5 core library.

  This unit defines core functions, procedures and data types, that aren't in
  add-ons.

  @bold(See also:) @link(getst Getting started) *)
(* Copyright (c) 2012-2019 Guillermo Martínez J. <niunio@users.sourceforge.net>

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
 *****************************************************************************)

  CONST
    ALLEGRO_VERSION      =   5;
    ALLEGRO_SUB_VERSION  =   2;
    ALLEGRO_WIP_VERSION  =   0;
  (* Not sure we need it, but ALLEGRO_VERSION_STR contains it:
     0 = SVN
     1 = first release
     2... = hotfixes?

     Note x.y.z (= x.y.z.0) has release number 1, and x.y.z.1 has release
     number 2, just to confuse you. *)
    ALLEGRO_RELEASE_NUMBER = 1;
    ALLEGRO_PAS_VERSION_STR = 'Allegro.pas 5.2.b.SVN';
    ALLEGRO_DATE_STR = '2018';
    ALLEGRO_DATE = 20180606; { yyyymmdd }
    ALLEGRO_VERSION_INT  = (
	   (ALLEGRO_VERSION SHL 24)
	OR (ALLEGRO_SUB_VERSION SHL 16)
	OR (ALLEGRO_WIP_VERSION SHL  8)
	OR  ALLEGRO_RELEASE_NUMBER
    );

  TYPE
    ALLEGRO_USER_MAIN = FUNCTION (argc: AL_INT; argv: AL_POINTER): AL_INT; CDECL;

  FUNCTION al_get_allegro_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_run_main (
    argc: AL_INT; argv: AL_POINTER; user_main: ALLEGRO_USER_MAIN
  ): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  CONST
    ALLEGRO_PI = 3.14159265358979323846;
    ALLEGRO_TAU = ALLEGRO_PI * 2;

  FUNCTION AL_ID (CONST str: SHORTSTRING): AL_INT;



(*
 * altime.h
 *****************************************************************************)

  TYPE
    ALLEGRO_TIMEOUT = RECORD
    (*** @exclude *)
      __pad1__, __pad2__: AL_UINT64;
    END;



  FUNCTION al_get_time: AL_DOUBLE;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_rest (seconds: AL_DOUBLE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_init_timeout (OUT timeout: ALLEGRO_TIMEOUT; seconds: AL_DOUBLE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * color.h
 *****************************************************************************)

  TYPE
    ALLEGRO_COLOR = RECORD
    (*** Color component. *)
      r, g, b, a: AL_FLOAT;
    END;

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
  FUNCTION al_map_rgb (r, g, b: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_map_rgba (r, g, b, a: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_map_rgb_f (r, g, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_map_rgba_f (r, g, b, a: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_premul_rgba (r, g, b: AL_UCHAR): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_premul_rgba_f (r, g, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Pixel unmapping *)
  PROCEDURE al_unmap_rgb (color: ALLEGRO_COLOR; OUT r, g, b: AL_UCHAR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_unmap_rgba (color: ALLEGRO_COLOR; OUT r, g, b, a: AL_UCHAR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_unmap_rgb_f (color: ALLEGRO_COLOR; OUT r, g, b: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_unmap_rgba_f (color: ALLEGRO_COLOR; OUT r, g, b, a: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Pixel formats *)
  FUNCTION al_get_pixel_size (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_pixel_format_bits (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_pixel_block_size (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_pixel_block_width (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_pixel_block_height (format: ALLEGRO_PIXEL_FORMAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * bitmap.h
 *****************************************************************************)

  TYPE
  (*** Abstract type representing a bitmap (2D image). *)
    ALLEGRO_BITMAPptr = AL_POINTER;

  CONST
    ALLEGRO_MEMORY_BITMAP            = $0001; (***<@exclude *)
    _ALLEGRO_KEEP_BITMAP_FORMAT      = $0002; (***<@exclude now a bitmap loader flag *)
    ALLEGRO_FORCE_LOCKING            = $0004; (***<@exclude no longer honoured *)
    ALLEGRO_NO_PRESERVE_TEXTURE      = $0008; (***<@exclude *)
    _ALLEGRO_ALPHA_TEST              = $0010; (***<@exclude now a render state flag *)
    _ALLEGRO_INTERNAL_OPENGL         = $0020; (***<@exclude *)
    ALLEGRO_MIN_LINEAR               = $0040; (***<@exclude *)
    ALLEGRO_MAG_LINEAR               = $0080; (***<@exclude *)
    ALLEGRO_MIPMAP                   = $0100; (***<@exclude *)
    _ALLEGRO_NO_PREMULTIPLIED_ALPHA  = $0200; (***<@exclude now a bitmap loader flag *)
    ALLEGRO_VIDEO_BITMAP             = $0400; (***<@exclude *)
    ALLEGRO_CONVERT_BITMAP           = $1000; (***<@exclude *)

  PROCEDURE al_set_new_bitmap_format (format: ALLEGRO_PIXEL_FORMAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_new_bitmap_flags (flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_new_bitmap_format: ALLEGRO_PIXEL_FORMAT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_new_bitmap_flags: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_add_new_bitmap_flag (flag: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(*** Returns the width of a bitmap in pixels. *)
  FUNCTION al_get_bitmap_width (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*** Returns the height of a bitmap in pixels. *)
  FUNCTION al_get_bitmap_height (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_bitmap_format (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_PIXEL_FORMAT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_bitmap_flags (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_bitmap (w, h: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_destroy_bitmap (Bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_put_pixel (x, y: AL_INT; color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_put_blended_pixel (x, y: AL_INT; color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_pixel (bitmap: ALLEGRO_BITMAPptr; x, y: AL_INT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Masking. *)
  PROCEDURE al_convert_mask_to_alpha (bitmap: ALLEGRO_BITMAPptr; mask_color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Clipping.*)
  PROCEDURE al_set_clipping_rectangle (x, y, width, height: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_reset_clipping_rectangle;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_clipping_rectangle (OUT x, y, w, h: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Sub bitmaps. *)
  FUNCTION al_create_sub_bitmap (parent: ALLEGRO_BITMAPptr; x, y, w, h: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_sub_bitmap (bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_parent_bitmap (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_bitmap_x (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_bitmap_y (bitmap: ALLEGRO_BITMAPptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_reparent_bitmap (bitmap, parent: ALLEGRO_BITMAPptr; x, y, w, h: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Miscelaneous. *)
  FUNCTION al_clone_bitmap (bitmap: ALLEGRO_BITMAPptr): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_convert_bitmap (bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_convert_memory_bitmaps;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * bitmap_draw.h
 *****************************************************************************)

  CONST
  (* Flags for the blitting functions.  Documented at al_draw_bitmap. *)
    ALLEGRO_FLIP_HORIZONTAL = $00001; (***<@exclude *)
    ALLEGRO_FLIP_VERTICAL   = $00002; (***<@exclude *)

(* Blitting *)
  PROCEDURE al_draw_bitmap (bitmap: ALLEGRO_BITMAPptr; dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh, dx, dy, dw, dh: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Tinted blitting *)
  PROCEDURE al_draw_tinted_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_tinted_bitmap_region (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_tinted_scaled_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; sx, sy, sw, sh, dx, dy, dw, dh: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_tinted_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_tinted_scaled_rotated_bitmap (bitmap: ALLEGRO_BITMAPptr; tint: ALLEGRO_COLOR; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_tinted_scaled_rotated_bitmap_region (bitmap: ALLEGRO_BITMAPptr; sx, sy, sw, sh: AL_FLOAT; tint: ALLEGRO_COLOR; cx, cy, dx, dy, xscale, yscale, angle: AL_FLOAT; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * path.h
 *****************************************************************************)

{ TODO:
  This header is used by Allegro to build file paths.  AFAIK Delphi and FPC
  runtime libraries had same functionality so I'll not add it unless it is
  really needed (i.e. I'm wrong and RTL doesn't include such funtionality). }



(*
 * utf8.h
 *****************************************************************************)

  {TODO: Documentation says it's not needed as it's used internally.
	Only basic functionality is implemented for convenience.

	Use Delphi/FPC native UNICODE support if needed. }
  {TODO: There are a lot of stuff to add here, including WIDESTRING and/or
	 UNICODESTRING support. }

  TYPE
  (*** @exclude *)
    _al_tagbstring = RECORD
      mlen, slen: AL_INT;
      data: AL_VOIDptr;
    END;
  (*** Pointer to @link(ALLEGRO_USTR). *)
    ALLEGRO_USTRptr = ^ALLEGRO_USTR;
    ALLEGRO_USTR = _al_tagbstring;
  (*** Pointer to @link(ALLEGRO_USTR_INFO). *)
    ALLEGRO_USTR_INFOptr = ^ALLEGRO_USTR_INFO;
    ALLEGRO_USTR_INFO = _al_tagbstring;

(* Creating strings. *)
  FUNCTION al_ustr_new (CONST s: AL_STR): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_new_from_buffer (CONST s: AL_STRptr; size: AL_SIZE_T): ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* AL_PRINTFUNC(ALLEGRO_USTR *, al_ustr_newf, (const char *fmt, ...), 1, 2); *)
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

(* Predefined string. *)
  FUNCTION al_ustr_empty_string: ALLEGRO_USTRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Reference strings. *)
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
  FUNCTION al_ustr_next (CONST us: ALLEGRO_USTRptr; VAR aPos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_ustr_prev (CONST us: ALLEGRO_USTRptr; VAR aPos: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Insert *)
  FUNCTION al_ustr_insert_chr (us1: ALLEGRO_USTRptr; aPos: AL_INT; c: AL_INT32)
  : AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Remove *)
  FUNCTION al_ustr_remove_chr (us: ALLEGRO_USTRptr; apos: AL_INT): AL_BOOL;
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

(* Low level UTF-8 functions *)
  FUNCTION al_utf8_width (c: AL_INT32): AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * file.h
 *****************************************************************************)

  TYPE
    ALLEGRO_FILEptr = AL_POINTER;

  (*** Pointer to @link(ALLEGRO_FILE_INTERFACE). *)
    ALLEGRO_FILE_INTERFACEptr = ^ALLEGRO_FILE_INTERFACE;
    ALLEGRO_FILE_INTERFACE = RECORD
      fi_open: FUNCTION (CONST path, mode: AL_STR): AL_POINTER; CDECL;
      fi_fclose: FUNCTION (handle: ALLEGRO_FILEptr): AL_BOOL; CDECL;
      fi_fread: FUNCTION (f: ALLEGRO_FILEptr; ptr: AL_POINTER; size: AL_SIZE_T): AL_SIZE_T; CDECL;
      fi_fwrite: FUNCTION (f: ALLEGRO_FILEptr; CONST ptr: AL_POINTER; size: AL_SIZE_T): AL_SIZE_T; CDECL;
      fi_fflush: FUNCTION (f: ALLEGRO_FILEptr): AL_BOOL; CDECL;
      fi_ftell: FUNCTION (f: ALLEGRO_FILEptr): AL_INT64; CDECL;
      fi_fseek: FUNCTION (f: ALLEGRO_FILEptr; offset: AL_INT64; whence: AL_INT): AL_BOOL; CDECL;
      fi_feof: FUNCTION (f: ALLEGRO_FILEptr): AL_BOOL; CDECL;
      fi_ferror: FUNCTION (f: ALLEGRO_FILEptr): AL_INT; CDECL;
      fi_ferrmsg: FUNCTION (f: ALLEGRO_FILEptr): AL_STRptr; CDECL;
      fi_fclearerr: PROCEDURE (f: ALLEGRO_FILEptr); CDECL;
      fi_fungetc: FUNCTION (f: ALLEGRO_FILEptr; c: AL_INT): AL_INT; CDECL;
      fi_fsize: FUNCTION (f: ALLEGRO_FILEptr): AL_OFF_T; CDECL;
    END;

  CONST
  { May be these should be an enum as the original implementation. }
    ALLEGRO_SEEK_SET = 0; (***<Seek relative to beginning of file. *)
    ALLEGRO_SEEK_CUR = 1; (***<Seek relative to current file position. *)
    ALLEGRO_SEEK_END = 2; (***<Seek relative to end of file. *)

  (* The basic operations. *)
    FUNCTION al_fopen (CONST path, mode: AL_STR): ALLEGRO_FILEptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fopen_interface (
      CONST vt: ALLEGRO_FILE_INTERFACEptr; CONST path, mode: AL_STR
    ): ALLEGRO_FILEptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_create_file_handle (
      CONST vt: ALLEGRO_FILE_INTERFACEptr; userdata: AL_POINTER
    ): ALLEGRO_FILEptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fclose (f: ALLEGRO_FILEptr): AL_BOOL;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fread
      (f: ALLEGRO_FILEptr; ptr: AL_POINTER; size: AL_SIZE_T): AL_SIZE_T;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fwrite
      (f: ALLEGRO_FILEptr; CONST ptr: AL_POINTER; size: AL_SIZE_T): AL_SIZE_T;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fflush (f: ALLEGRO_FILEptr): AL_BOOL;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_ftell (f: ALLEGRO_FILEptr): AL_INT64;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fseek
      (f: ALLEGRO_FILEptr; offset: AL_INT64; whence: AL_INT): AL_BOOL;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_feof (f: ALLEGRO_FILEptr): AL_BOOL;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_ferror (f: ALLEGRO_FILEptr): AL_INT;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_ferrmsg (f: ALLEGRO_FILEptr): AL_STRptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    PROCEDURE al_fclearerr (f: ALLEGRO_FILEptr);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fungetc (f: ALLEGRO_FILEptr; c: AL_INT): AL_INT;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fsize (f: ALLEGRO_FILEptr): AL_INT64;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  (* Convenience functions. *)
    FUNCTION al_fgetc (f: ALLEGRO_FILEptr): AL_INT;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fputc (f: ALLEGRO_FILEptr; c: AL_INT): AL_INT;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fread16le (f: ALLEGRO_FILEptr): AL_INT16;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fread16be (f: ALLEGRO_FILEptr): AL_INT16;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fwrite16le (f: ALLEGRO_FILEptr; w: AL_INT16): AL_SIZE_T;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fwrite16be (f: ALLEGRO_FILEptr; w: AL_INT16): AL_SIZE_T;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fread32le (f: ALLEGRO_FILEptr): AL_INT32;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
   FUNCTION al_fread32be (f: ALLEGRO_FILEptr): AL_INT32;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fwrite32le (f: ALLEGRO_FILEptr; l: AL_INT32): AL_SIZE_T;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fwrite32be (f: ALLEGRO_FILEptr; l: AL_INT32): AL_SIZE_T;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fgets
      (f: ALLEGRO_FILEptr; CONST p: AL_STRptr; max: AL_SIZE_T): AL_STRptr
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fget_ustr (f: ALLEGRO_FILEptr): ALLEGRO_USTRptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_fputs (f: ALLEGRO_FILEptr; CONST p: AL_STR): AL_INT;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;

{
/* Specific to stdio backend. */
AL_FUNC(ALLEGRO_FILE*, al_fopen_fd, (int fd, const char *mode));
AL_FUNC(ALLEGRO_FILE*, al_make_temp_file, (const char *tmpl,
      ALLEGRO_PATH **ret_path));
}

  (* Specific to slices. *)
    FUNCTION al_fopen_slice (
      fp: ALLEGRO_FILEptr; initial_size: AL_SIZE_T; CONST mode: AL_STR
    ): ALLEGRO_FILEptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  (* Thread local state. *)
    FUNCTION al_get_new_file_interface: ALLEGRO_FILE_INTERFACEptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    PROCEDURE al_set_new_file_interface
      (CONST file_interface: ALLEGRO_FILE_INTERFACEptr);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    PROCEDURE al_set_standard_file_interface;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  (* ALLEGRO_FILE field accessors *)
    FUNCTION al_get_file_userdata (f: ALLEGRO_FILEptr): AL_POINTER;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * bitmap_io.h
 *****************************************************************************)

(* Bitmap loader flag. *)
  CONST
  { May be these should be an enum as the original implementation. }
    ALLEGRO_KEEP_BITMAP_FORMAT     = $0002; (***<@exclude *)
    ALLEGRO_NO_PREMULTIPLIED_ALPHA = $0200; (***<@exclude *)
    ALLEGRO_KEEP_INDEX             = $0800; (***<@exclude *)


  TYPE
  (*** Used by @link(al_register_bitmap_loader). *)
    ALLEGRO_IIO_LOADER_FUNCTION = FUNCTION (CONST filename: AL_STR; flags: AL_INT): ALLEGRO_BITMAPptr; CDECL;
  (*** Used by @link(al_register_bitmap_loader_f). *)
    ALLEGRO_IIO_FS_LOADER_FUNCTION = FUNCTION (fp: ALLEGRO_FILEptr; flags: AL_INT): ALLEGRO_BITMAPptr; CDECL;
  (*** Used by @link(al_register_bitmap_saver). *)
    ALLEGRO_IIO_SAVER_FUNCTION = FUNCTION (CONST filename: AL_STR; bitmap: ALLEGRO_BITMAPptr): AL_BOOL; CDECL;
  (*** Used by @link(al_register_bitmap_saver_f). *)
    ALLEGRO_IIO_FS_SAVER_FUNCTION = FUNCTION (fp: ALLEGRO_FILEptr; bitmap: ALLEGRO_BITMAPptr): AL_BOOL; CDECL;
  (*** Used by @link(al_register_bitmap_identifier). *)
    ALLEGRO_IIO_IDENTIFIER_FUNCTION = FUNCTION (fp: ALLEGRO_FILEptr): AL_BOOL; CDECL;

  FUNCTION al_register_bitmap_loader
    (CONST ext: AL_STR; loader: ALLEGRO_IIO_LOADER_FUNCTION): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_register_bitmap_saver
    (CONST ext: AL_STR; saver: ALLEGRO_IIO_SAVER_FUNCTION): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_register_bitmap_loader_f
    (CONST ext: AL_STR; fs_loader: ALLEGRO_IIO_FS_LOADER_FUNCTION): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_register_bitmap_saver_f
    (CONST ext: AL_STR; fs_saver: ALLEGRO_IIO_FS_SAVER_FUNCTION): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_register_bitmap_identifier
    (CONST ext: AL_STR; identifier: ALLEGRO_IIO_IDENTIFIER_FUNCTION): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_load_bitmap (CONST filename: AL_STR): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_load_bitmap_flags (CONST filename: AL_STR; flags: AL_INT): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_load_bitmap_f
    (fp: ALLEGRO_FILEptr; CONST ident: AL_STR): ALLEGRO_BITMAPptr;
  FUNCTION al_load_bitmap_flags_f (
    fp: ALLEGRO_FILEptr; CONST ident: AL_STR; flags: AL_INT
  ): ALLEGRO_BITMAPptr;
  FUNCTION al_save_bitmap (CONST filename: AL_STR; bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_save_bitmap_f (
    fp: ALLEGRO_FILEptr; CONST ident: AL_STR; bitmap: ALLEGRO_BITMAPptr
  ): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_identify_bitmap_f (fp: ALLEGRO_FILEptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_identify_bitmap (CONST filename: AL_STR): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * bitmap_lock.h
 *****************************************************************************)

  CONST
  (* Locking flags. *)
  { May be these should be an enum as the original implementation. }
    ALLEGRO_LOCK_READWRITE  = 0; (***<@exclude *)
    ALLEGRO_LOCK_READONLY   = 1; (***<@exclude *)
    ALLEGRO_LOCK_WRITEONLY  = 2; (***<@exclude *)

  TYPE
  (*** Pointer to @link(ALLEGRO_LOCKED_REGION). *)
    ALLEGRO_LOCKED_REGIONptr = ^ALLEGRO_LOCKED_REGION;
    ALLEGRO_LOCKED_REGION = RECORD
      data: AL_VOIDptr;
      format,
      pitch,
      pixel_size: AL_INT;
    END;



  FUNCTION al_lock_bitmap (bitmap: ALLEGRO_BITMAPptr; format: ALLEGRO_PIXEL_FORMAT; flags: AL_INT): ALLEGRO_LOCKED_REGIONptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_lock_bitmap_region (bitmap: ALLEGRO_BITMAPptr; x, y, width, height: AL_INT; format: ALLEGRO_PIXEL_FORMAT; flags: AL_INT): ALLEGRO_LOCKED_REGIONptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_lock_bitmap_blocked (bitmap: ALLEGRO_BITMAPptr; flags: AL_INT): ALLEGRO_LOCKED_REGIONptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_lock_bitmap_region_blocked (bitmap: ALLEGRO_BITMAPptr; x_block, y_block, width_block, height_block, flags: AL_INT): ALLEGRO_LOCKED_REGIONptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_unlock_bitmap (bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_bitmap_locked (bitmap: ALLEGRO_BITMAPptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * blender.h
 *****************************************************************************)

  TYPE
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

  PROCEDURE al_set_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_blend_color (color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_blender (OUT op: ALLEGRO_BLEND_OPERATIONS; OUT source, dest: ALLEGRO_BLEND_MODE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_blend_color: ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_separate_blender (op: ALLEGRO_BLEND_OPERATIONS; source, dest: ALLEGRO_BLEND_MODE;
				     alpha_op: ALLEGRO_BLEND_OPERATIONS; alpha_source, alpha_dest: ALLEGRO_BLEND_MODE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_separate_blender (OUT op: ALLEGRO_BLEND_OPERATIONS; OUT source, dest: ALLEGRO_BLEND_MODE;
				     OUT alpha_op: ALLEGRO_BLEND_OPERATIONS; OUT alpha_source, alpha_dest: ALLEGRO_BLEND_MODE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * events.h
 *****************************************************************************)

  TYPE
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

(* Function: ALLEGRO_EVENT_TYPE_IS_USER
 *
 *    1 <= n < 512  - builtin events
 *  512 <= n < 1024 - reserved user events (for addons)
 * 1024 <= n        - unreserved user events
 *)
  FUNCTION ALLEGRO_EVENT_TYPE_IS_USER (t: ALLEGRO_EVENT_TYPE): AL_BOOL; INLINE;

  FUNCTION ALLEGRO_GET_EVENT_TYPE (CONST str: SHORTSTRING): AL_INT; INLINE;


  TYPE
  (* These pointers are declared here as they're needed by the event system. *)
    ALLEGRO_DISPLAYptr = AL_POINTER;
    ALLEGRO_JOYSTICKptr = AL_POINTER;
  (*** Pointer to keyboard. *)
    ALLEGRO_KEYBOARDptr = AL_POINTER;
  (*** Pointer to mouse. *)
    ALLEGRO_MOUSEptr = AL_POINTER;
    ALLEGRO_TIMERptr = AL_POINTER;


  (*** Pointer to @link(ALLEGRO_EVENT_SOURCE). *)
    ALLEGRO_EVENT_SOURCEptr = ^ALLEGRO_EVENT_SOURCE;
    ALLEGRO_EVENT_SOURCE = RECORD
    (*** @exclude *)
      __pad : ARRAY [0..31] OF AL_INT;
    END;


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

    ALLEGRO_ANY_EVENT = RECORD
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The event source which generated the event. *)
      source: ALLEGRO_EVENT_SOURCEptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
    END;

    ALLEGRO_DISPLAY_EVENT = RECORD
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** The display that generated the event. *)
      source: ALLEGRO_DISPLAYptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
      x, y: AL_INT;
      width, height: AL_INT;
      orientation: AL_INT;
    END;

    ALLEGRO_JOYSTICK_EVENT = RECORD
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
    END;

    ALLEGRO_KEYBOARD_EVENT = RECORD
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
    END;

    ALLEGRO_MOUSE_EVENT = RECORD
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
    END;

  (*** Contains the timer events information. @seealso(ALLEGRO_EVENT) *)
    ALLEGRO_TIMER_EVENT = RECORD
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
    END;

    ALLEGRO_TOUCH_EVENT = RECORD
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
    END;

  (*** Pointer to the user event descriptor. @seealso(ALLEGRO_USER_EVENT) *)
    ALLEGRO_USER_EVENT_DESCRIPTORptr = POINTER;

  (*** Pointer to @link(ALLEGRO_USER_EVENT). *)
    ALLEGRO_USER_EVENTptr = ^ALLEGRO_USER_EVENT;

    ALLEGRO_USER_EVENT = RECORD
    (*** Event identifier. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** Pointer to the event source. *)
      source: AL_POINTER;
    (*** When the event was emited. *)
      timestamp: AL_DOUBLE;
    (*** @exclude *)
      __internal__descr: ALLEGRO_USER_EVENT_DESCRIPTORptr;
    (*** Extra data. *)
      data1: AL_INTPTR_T;
    (*** Extra data. *)
      data2: AL_INTPTR_T;
    (*** Extra data. *)
      data3: AL_INTPTR_T;
    (*** Extra data. *)
      data4: AL_INTPTR_T;
    END;

  (*** Pointer to @link(ALLEGRO_EVENT) *)
    ALLEGRO_EVENTptr = ^ALLEGRO_EVENT;
    ALLEGRO_EVENT = RECORD
      CASE LONGINT OF
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
    END;

    (*** User event destructor. @seealso(al_emit_user_event) *)
      ALLEGRO_EVENT_DTOR_PROC = PROCEDURE (evt: ALLEGRO_USER_EVENTptr); CDECL;

(* Event sources. *)
  PROCEDURE al_init_user_event_source (source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_destroy_user_event_source (source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* The second argument is ALLEGRO_EVENT instead of ALLEGRO_USER_EVENT
 * to prevent users passing a pointer to a too-short structure.
 *)
  FUNCTION al_emit_user_event (
    source: ALLEGRO_EVENT_SOURCEptr; Event: ALLEGRO_EVENTptr;
    dtor: ALLEGRO_EVENT_DTOR_PROC
  ): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_unref_user_event (event: ALLEGRO_USER_EVENTptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_event_source_data (source: ALLEGRO_EVENT_SOURCEptr; data: AL_POINTER);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_event_source_data (CONST source: ALLEGRO_EVENT_SOURCEptr): AL_POINTER;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(* Event queues. *)
  TYPE
    ALLEGRO_EVENT_QUEUEptr = AL_POINTER;

  FUNCTION al_create_event_queue: ALLEGRO_EVENT_QUEUEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_destroy_event_queue (queue: ALLEGRO_EVENT_QUEUEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*** Returns @true if the event source is registered.@seealso(al_register_event_source) *)
  FUNCTION al_is_event_source_registered (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_register_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_unregister_event_source (queue: ALLEGRO_EVENT_QUEUEptr; source: ALLEGRO_EVENT_SOURCEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_pause_event_queue (queue: ALLEGRO_EVENT_QUEUEptr; pause: AL_BOOL);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*** Returns @true if the event queue is paused. @seealso(al_pause_event_queue) *)
  FUNCTION al_is_event_queue_paused (CONST queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_is_event_queue_empty (queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL;EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_next_event (queue: ALLEGRO_EVENT_QUEUEptr; OUT ret_event: ALLEGRO_EVENT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_peek_next_event (queue: ALLEGRO_EVENT_QUEUEptr; OUT ret_event: ALLEGRO_EVENT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_drop_next_event (queue: ALLEGRO_EVENT_QUEUEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*** Drops all events, if any, from the queue. @seealso(al_drop_next_event) @seealso(al_is_event_queue_empty) *)
  PROCEDURE al_flush_event_queue (queue: ALLEGRO_EVENT_QUEUEptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
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
    ALLEGRO_DEFAULT                     = 0 SHL 0; (***<@exclude *)
    ALLEGRO_WINDOWED                    = 1 SHL 0; (***<@exclude *)
    ALLEGRO_FULLSCREEN                  = 1 SHL 1; (***<@exclude *)
    ALLEGRO_OPENGL                      = 1 SHL 2; (***<@exclude *)
    ALLEGRO_DIRECT3D_INTERNAL           = 1 SHL 3; (***<@exclude *)
    ALLEGRO_RESIZABLE                   = 1 SHL 4; (***<@exclude *)
    ALLEGRO_FRAMELESS                   = 1 SHL 5; (***<@exclude *)
    ALLEGRO_NOFRAME                     = ALLEGRO_FRAMELESS; (***<@exclude *)
    ALLEGRO_GENERATE_EXPOSE_EVENTS      = 1 SHL 6; (***<@exclude *)
    ALLEGRO_OPENGL_3_0                  = 1 SHL 7; (***<@exclude *)
    ALLEGRO_OPENGL_FORWARD_COMPATIBLE   = 1 SHL 8; (***<@exclude *)
    ALLEGRO_FULLSCREEN_WINDOW           = 1 SHL 9; (***<@exclude *)
    ALLEGRO_MINIMIZED                   = 1 SHL 10; (***<@exclude *)
    ALLEGRO_PROGRAMMABLE_PIPELINE       = 1 SHL 11; (***<@exclude *)
    ALLEGRO_GTK_TOPLEVEL_INTERNAL       = 1 SHL 12; (***<@exclude *)
    ALLEGRO_MAXIMIZED                   = 1 SHL 13; (***<@exclude *)
    ALLEGRO_OPENGL_ES_PROFILE           = 1 SHL 14; (***<@exclude *)

  TYPE
  (* Possible parameters for al_set_display_option.

     Make sure to update ALLEGRO_EXTRA_DISPLAY_SETTINGS if you modify
     anything here.  *)
    ALLEGRO_DISPLAY_OPTIONS = ( (***<@exclude *)
      ALLEGRO_RED_SIZE = 0, (***<@exclude *)
      ALLEGRO_GREEN_SIZE = 1, (***<@exclude *)
      ALLEGRO_BLUE_SIZE = 2, (***<@exclude *)
      ALLEGRO_ALPHA_SIZE = 3, (***<@exclude *)
      ALLEGRO_RED_SHIFT = 4, (***<@exclude *)
      ALLEGRO_GREEN_SHIFT = 5, (***<@exclude *)
      ALLEGRO_BLUE_SHIFT = 6, (***<@exclude *)
      ALLEGRO_ALPHA_SHIFT = 7, (***<@exclude *)
      ALLEGRO_ACC_RED_SIZE = 8, (***<@exclude *)
      ALLEGRO_ACC_GREEN_SIZE = 9, (***<@exclude *)
      ALLEGRO_ACC_BLUE_SIZE = 10, (***<@exclude *)
      ALLEGRO_ACC_ALPHA_SIZE = 11, (***<@exclude *)
      ALLEGRO_STEREO = 12, (***<@exclude *)
      ALLEGRO_AUX_BUFFERS = 13, (***<@exclude *)
      ALLEGRO_COLOR_SIZE = 14, (***<@exclude *)
      ALLEGRO_DEPTH_SIZE = 15, (***<@exclude *)
      ALLEGRO_STENCIL_SIZE = 16, (***<@exclude *)
      ALLEGRO_SAMPLE_BUFFERS = 17, (***<@exclude *)
      ALLEGRO_SAMPLES = 18, (***<@exclude *)
      ALLEGRO_RENDER_METHOD = 19, (***<@exclude *)
      ALLEGRO_FLOAT_COLOR = 20, (***<@exclude *)
      ALLEGRO_FLOAT_DEPTH = 21, (***<@exclude *)
      ALLEGRO_SINGLE_BUFFER = 22, (***<@exclude *)
      ALLEGRO_SWAP_METHOD = 23, (***<@exclude *)
      ALLEGRO_COMPATIBLE_DISPLAY = 24, (***<@exclude *)
      ALLEGRO_UPDATE_DISPLAY_REGION = 25, (***<@exclude *)
      ALLEGRO_VSYNC = 26, (***<@exclude *)
      ALLEGRO_MAX_BITMAP_SIZE = 27, (***<@exclude *)
      ALLEGRO_SUPPORT_NPOT_BITMAP = 28, (***<@exclude *)
      ALLEGRO_CAN_DRAW_INTO_BITMAP = 29, (***<@exclude *)
      ALLEGRO_SUPPORT_SEPARATE_ALPHA = 30, (***<@exclude *)
      ALLEGRO_AUTO_CONVERT_BITMAPS = 31, (***<@exclude *)
      ALLEGRO_SUPPORTED_ORIENTATIONS = 32, (***<@exclude *)
      ALLEGRO_OPENGL_MAJOR_VERSION = 33, (***<@exclude *)
      ALLEGRO_OPENGL_MINOR_VERSION = 34, (***<@exclude *)
      ALLEGRO_DISPLAY_OPTIONS_COUNT (***<@exclude *)
    );

  CONST
    ALLEGRO_DONTCARE = 0; (***<@exclude *)
    ALLEGRO_REQUIRE = 1; (***<@exclude *)
    ALLEGRO_SUGGEST = 2; (***<@exclude *)



(* Bitflags so they can be used for the ALLEGRO_SUPPORTED_ORIENTATIONS option. *)
    ALLEGRO_DISPLAY_ORIENTATION_UNKNOWN = 0; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_0_DEGREES = 1; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_90_DEGREES = 2; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_180_DEGREES = 4; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_270_DEGREES = 8; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_PORTRAIT = 5; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_LANDSCAPE = 10; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_ALL = 15; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_FACE_UP = 16; (***<@exclude *)
    ALLEGRO_DISPLAY_ORIENTATION_FACE_DOWN = 32; (***<@exclude *)

  { Formelly part of the primitives addon. }
    _ALLEGRO_PRIM_MAX_USER_ATTR = 10; (***<@exclude *)

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
  FUNCTION al_get_display_format (display: ALLEGRO_DISPLAYptr): ALLEGRO_PIXEL_FORMAT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_refresh_rate (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_flags (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_orientation (display: ALLEGRO_DISPLAYptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_display_flag
    (display: ALLEGRO_DISPLAYptr; flag: AL_INT; onoff: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_create_display (w, h: AL_INT): ALLEGRO_DISPLAYptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_destroy_display (display: ALLEGRO_DISPLAYptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_current_display: ALLEGRO_DISPLAYptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_target_bitmap (Bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_set_target_backbuffer (display: ALLEGRO_DISPLAYptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_backbuffer (display: ALLEGRO_DISPLAYptr): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
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

  FUNCTION al_wait_for_vsync: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

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
  PROCEDURE al_apply_window_constraints (display: ALLEGRO_DISPLAYptr; onoff: AL_BOOL);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_window_title (display: ALLEGRO_DISPLAYptr; const title: AL_STR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(* Defined in display settings.c *)
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

(* Miscellaneous. *)
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

  TYPE
  (*** An abstract configuration structure. @seealso(al_create_config) *)
    ALLEGRO_CONFIGptr = AL_POINTER;
  { Iterating sections and entries means to define the internals of these
    structures.  Instead of that, use TIniFile defined by both Delphi and Free
    Pascal.

    ALLEGRO_CONFIG_SECTIONptr = AL_POINTER;
    ALLEGRO_CONFIG_ENTRYptr = AL_POINTER;
  }

    FUNCTION al_create_config: ALLEGRO_CONFIGptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    PROCEDURE al_add_config_section (config: ALLEGRO_CONFIGptr; CONST name: AL_STR);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    PROCEDURE al_set_config_value (config: ALLEGRO_CONFIGptr; CONST section, key, value: AL_STR);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    PROCEDURE al_add_config_comment (config: ALLEGRO_CONFIGptr; CONST section, comment: AL_STR);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_get_config_value (CONST config: ALLEGRO_CONFIGptr; CONST section, key: AL_STR): AL_STRptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_load_config_file (CONST filename: AL_STR): ALLEGRO_CONFIGptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_load_config_file_f (fp: ALLEGRO_FILEptr): ALLEGRO_CONFIGptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_save_config_file 
      (CONST filename: AL_STR; CONST config: ALLEGRO_CONFIGptr): AL_BOOL;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_save_config_file_f
      (fp: ALLEGRO_FILEptr; CONST config: ALLEGRO_CONFIGptr): AL_BOOL;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    PROCEDURE al_merge_config_into (master: ALLEGRO_CONFIGptr; CONST add: ALLEGRO_CONFIGptr);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_merge_config (CONST cfg1, cfg2: ALLEGRO_CONFIGptr): ALLEGRO_CONFIGptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    PROCEDURE al_destroy_config (config: ALLEGRO_CONFIGptr);
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_remove_config_section
      (config: ALLEGRO_CONFIGptr; CONST section: AL_STR): AL_BOOL;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_remove_config_key
      (config: ALLEGRO_CONFIGptr; CONST section, key: AL_STR): AL_BOOL;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  { Iterating sections and entries means to define the internals of some
    structures.  Instead of that, use TIniFile defined by both Delphi and Free
    Pascal.

    FUNCTION al_get_first_config_section
      (CONST config: ALLEGRO_CONFIGptr; OUT iterator: ALLEGRO_CONFIG_SECTIONptr): AL_STRptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_get_next_config_section (VAR iterator: ALLEGRO_CONFIG_SECTIONptr): AL_STRptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_get_first_config_entry (
      CONST config: ALLEGRO_CONFIGptr; CONST section: AL_STR;
      OUT iterator: ALLEGRO_CONFIG_ENTRYptr
    ): AL_STRptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
    FUNCTION al_get_next_config_entry (VAR iterator: ALLEGRO_CONFIG_ENTRYptr): AL_STRptr;
      CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  }



(*
 * cpu.h
 *
 *   CPU and system information handling.
 *****************************************************************************)

  FUNCTION al_get_cpu_count: AL_INT; CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_ram_size: AL_INT;  CDECL; EXTERNAL ALLEGRO_LIB_NAME;



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

  PROCEDURE al_clear_to_color (color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_clear_depth_buffer (x: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_draw_pixel (x, y: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * error.h
 *
 *   Error handling.
 *****************************************************************************)

  FUNCTION al_get_errno: AL_INT; CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(*** Sets the error number for the calling thread. @seealso(al_get_errno) *)
  PROCEDURE al_set_errno (errnum: AL_INT); CDECL; EXTERNAL ALLEGRO_LIB_NAME;



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
 *   File system hooks.
 *****************************************************************************)

 { TODO:
   Both Delphi and Free Pascal RTL (RunTime Libraries) have a complete set of
   functions and procedures to work with the file system, I'll not implement
   this.

   If you think it is interesting to have this stuff, may be I add it as an
   add-on. }



(*
 * fullscreen_mode.h
 *****************************************************************************)

  TYPE
  (*** Pointer to @link(ALLEGRO_DISPLAY_MODE). *)
    ALLEGRO_DISPLAY_MODEptr = ^ALLEGRO_DISPLAY_MODE;
    ALLEGRO_DISPLAY_MODE = RECORD
      width, height, format, refresh_rate: AL_INT;
    END;

  FUNCTION al_get_num_display_modes: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_display_mode (index: AL_INT; OUT mode: ALLEGRO_DISPLAY_MODE): ALLEGRO_DISPLAY_MODEptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * joystick.h
 *
 *   Joystick routines.
 *****************************************************************************)

  CONST
  (* internal values *)
    _AL_MAX_JOYSTICK_AXES    =  3; (***<@exclude *)
    _AL_MAX_JOYSTICK_STICKS  = 16; (***<@exclude *)
    _AL_MAX_JOYSTICK_BUTTONS = 32; (***<@exclude *)

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

  FUNCTION al_install_joystick: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_uninstall_joystick;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*** Returns @true if @link(al_install_joystick) was called successfully. *)
  FUNCTION al_is_joystick_installed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_reconfigure_joysticks: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_num_joysticks: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick (joyn: AL_INT): ALLEGRO_JOYSTICKptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*** This procedure currently does nothing. @seealso(al_get_joystick) *)
  PROCEDURE al_release_joystick (j: ALLEGRO_JOYSTICKptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_active (j: ALLEGRO_JOYSTICKptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_name (j: ALLEGRO_JOYSTICKptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_joystick_num_sticks (j: ALLEGRO_JOYSTICKptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_joystick_stick_flags (j: ALLEGRO_JOYSTICKptr; stick: AL_INT): AL_INT; (* junk? *)
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

  PROCEDURE al_get_joystick_state (j: ALLEGRO_JOYSTICKptr; OUT ret_state: ALLEGRO_JOYSTICK_STATE);
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
      display: ALLEGRO_DISPLAYptr; (* public *)
    (*** @exclude internal *)
      __key_down__internal__: ARRAY [0..((ALLEGRO_KEY_MAX + 31) DIV 32) - 1] OF AL_UINT;
    END;


(*** Returns @true if @link(al_install_keyboard) was called successfully. *)
  FUNCTION al_is_keyboard_installed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_install_keyboard: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_uninstall_keyboard;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_keyboard_leds (leds: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

(*** Converts the given keycode to a description of the key. *)
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
  (*** @exclude Allow up to four extra axes for future expansion. *)
    ALLEGRO_MOUSE_MAX_EXTRA_AXES = 4;

  TYPE
    ALLEGRO_MOUSE_STATE = RECORD
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
      more_axes: ARRAY [0..(ALLEGRO_MOUSE_MAX_EXTRA_AXES - 1)] OF AL_INT;
      buttons: AL_INT;
      pressure: AL_FLOAT;
      display: ALLEGRO_DISPLAYptr;
    END;



(*** Returns @true if @link(al_install_mouse) was called successfully. *)
  FUNCTION al_is_mouse_installed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
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
  FUNCTION al_get_mouse_cursor_position (OUT ret_x, ret_y: AL_INT): AL_BOOL;
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
    ALLEGRO_MEMORY_INTERFACE = RECORD
      mi_malloc: FUNCTION (n: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER; CDECL;
      mi_free: PROCEDURE (ptr: AL_POINTER; line: AL_INT; CONST afile, func: AL_STR); CDECL;
      mi_realloc: FUNCTION (ptr: AL_POINTER; n: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER; CDECL;
      mi_calloc: FUNCTION (n, count: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER; CDECL;
    END;

  PROCEDURE al_set_memory_interface (VAR iface: ALLEGRO_MEMORY_INTERFACE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_restore_memory_interface;


  FUNCTION al_malloc (CONST n: AL_SIZE_T): AL_POINTER; INLINE;
  PROCEDURE al_free (p: AL_POINTER); INLINE;
  FUNCTION al_realloc (p: AL_POINTER; CONST n: AL_SIZE_T): AL_POINTER; INLINE;
  FUNCTION al_calloc (CONST c, n: AL_SIZE_T): AL_POINTER; INLINE;


  FUNCTION al_malloc_with_context
    (n: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_free_with_context
    (ptr: AL_POINTER; line: AL_INT; CONST afile, func: AL_STR);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_realloc_with_context
    (ptr: AL_POINTER; n: AL_SIZE_T; line: AL_INT;
     CONST afile, func: AL_STR): AL_POINTER;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_calloc_with_context
    (n, count: AL_SIZE_T; line: AL_INT; CONST afile, func: AL_STR): AL_POINTER;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * monitor.h
 *****************************************************************************)

  TYPE
    ALLEGRO_MONITOR_INFO = RECORD
      x1, y1, x2, y2: AL_INT;
    END;

  CONST
    ALLEGRO_DEFAULT_DISPLAY_ADAPTER = -1; (***<@exclude *)

  FUNCTION al_get_num_video_adapters: AL_INT; CDECL;
    EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_monitor_info (adapter: AL_INT; OUT info: ALLEGRO_MONITOR_INFO): AL_BOOL; CDECL;
    EXTERNAL ALLEGRO_LIB_NAME;



(*
 * mouse_cursor.h
 *****************************************************************************)

  TYPE
  (*** Pointer to a custom mouse cursor *)
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

  CONST
    ALLEGRO_RENDER_NEVER = 0; (***< @exclude *)
    ALLEGRO_RENDER_ALWAYS = 1; (***< @exclude *)
    ALLEGRO_RENDER_LESS = 2; (***< @exclude *)
    ALLEGRO_RENDER_EQUAL = 3; (***< @exclude *)
    ALLEGRO_RENDER_LESS_EQUAL = 4; (***< @exclude *)
    ALLEGRO_RENDER_GREATER = 5; (***< @exclude *)
    ALLEGRO_RENDER_NOT_EQUAL = 6; (***< @exclude *)
    ALLEGRO_RENDER_GREATER_EQUAL = 7; (***< @exclude *)

    ALLEGRO_MASK_RED = 1 SHL 0; (***< @exclude *)
    ALLEGRO_MASK_GREEN = 1 SHL 1; (***< @exclude *)
    ALLEGRO_MASK_BLUE = 1 SHL 2; (***< @exclude *)
    ALLEGRO_MASK_ALPHA = 1 SHL 3; (***< @exclude *)
    ALLEGRO_MASK_DEPTH = 1 SHL 4; (***< @exclude *)
    ALLEGRO_MASK_RGB = (ALLEGRO_MASK_RED OR ALLEGRO_MASK_GREEN OR ALLEGRO_MASK_BLUE); (***< @exclude *)
    ALLEGRO_MASK_RGBA = (ALLEGRO_MASK_RGB OR ALLEGRO_MASK_ALPHA); (***< @exclude *)

  PROCEDURE al_set_render_state (state: ALLEGRO_RENDER_STATE; value: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * transformations.h
 *****************************************************************************)

  TYPE
  (*** Pointer to a @link(ALLEGRO_TRANSFORM). *)
    ALLEGRO_TRANSFORMptr = ^ALLEGRO_TRANSFORM;
    ALLEGRO_TRANSFORM = RECORD
      m: ARRAY [0..3] OF ARRAY [0..3] OF AL_FLOAT;
    END;

(* Transformations *)
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
  PROCEDURE al_transform_coordinates (VAR trans: ALLEGRO_TRANSFORM; VAR x, y: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_transform_coordinates_3d (VAR trans: ALLEGRO_TRANSFORM; VAR x, y, z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_transform_coordinates_4d (VAR trans: ALLEGRO_TRANSFORM; VAR x, y, z, w: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_transform_coordinates_3d_projective (VAR trans: ALLEGRO_TRANSFORM; VAR x, y, z: AL_FLOAT);
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
  FUNCTION al_check_inverse (VAR trans: ALLEGRO_TRANSFORM; tol: AL_FLOAT): AL_BOOL; INLINE;
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

  TYPE
    ALLEGRO_SHADERptr = AL_POINTER;

    ALLEGRO_SHADER_TYPE = (
      ALLEGRO_VERTEX_SHADER = 1,
      ALLEGRO_PIXEL_SHADER = 2
    );



    ALLEGRO_SHADER_PLATFORM = (
      ALLEGRO_SHADER_AUTO = 0,
    (*** OpenGL Shading Language. *)
      ALLEGRO_SHADER_GLSL = 1,
    (*** High Level Shader Language (for Direct3D). *)
      ALLEGRO_SHADER_HLSL = 2
    );


  CONST
    ALLEGRO_SHADER_VAR_COLOR           = 'al_color';           {**<@exclude }
    ALLEGRO_SHADER_VAR_POS             = 'al_pos';             {**<@exclude }
    ALLEGRO_SHADER_VAR_PROJVIEW_MATRIX = 'al_projview_matrix'; {**<@exclude }
    ALLEGRO_SHADER_VAR_TEX             = 'al_tex';             {**<@exclude }
    ALLEGRO_SHADER_VAR_TEXCOORD        = 'al_texcoord';        {**<@exclude }
    ALLEGRO_SHADER_VAR_TEX_MATRIX      = 'al_tex_matrix';      {**<@exclude }
    ALLEGRO_SHADER_VAR_USER_ATTR       = 'al_user_attr_';      {**<@exclude }
    ALLEGRO_SHADER_VAR_USE_TEX         = 'al_use_tex';         {**<@exclude }
    ALLEGRO_SHADER_VAR_USE_TEX_MATRIX  = 'al_use_tex_matrix';  {**<@exclude }

  FUNCTION al_create_shader (platform: ALLEGRO_SHADER_PLATFORM): ALLEGRO_SHADERptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_attach_shader_source (shader: ALLEGRO_SHADERptr; aType: ALLEGRO_SHADER_TYPE; CONST Source: AL_STR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_attach_shader_source_file (shader: ALLEGRO_SHADERptr; aType: ALLEGRO_SHADER_TYPE; CONST filename: AL_STR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_build_shader (shader: ALLEGRO_SHADERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_shader_log (shader: ALLEGRO_SHADERptr): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_shader_platform (shader: ALLEGRO_SHADERptr): ALLEGRO_SHADER_PLATFORM;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_use_shader (shader: ALLEGRO_SHADERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_destroy_shader (shader: ALLEGRO_SHADERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_set_shader_sampler (CONST name: AL_STR; bitmap: ALLEGRO_BITMAPptr; aUnit: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_shader_matrix (CONST name: AL_STR; VAR matrix: ALLEGRO_TRANSFORM): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_shader_int (CONST name: AL_STR; i: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_shader_float (CONST name: AL_STR; f: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_shader_int_vector (CONST name: AL_STR; num_components: AL_INT; i: AL_INTptr; num_elems: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_shader_float_vector (CONST name: AL_STR; num_components: AL_INT; f: AL_FLOATptr; num_elems: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_set_shader_bool (CONST name: AL_STR; b: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_default_shader_source (platform: ALLEGRO_SHADER_PLATFORM; aType: ALLEGRO_SHADER_TYPE): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * system.h
 *****************************************************************************)

{ TODO: Some stuff needs the "path.h" section. }
{ TODO: Also, Free pascal and Delphi include functions to get information about
        the application and the executable. }

  FUNCTION al_init: AL_BOOL; INLINE;

  FUNCTION al_install_system (version: AL_INT; atexit_ptr: AL_POINTER): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_uninstall_system;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*** Returns @true if Allegro is initialized, otherwise returns @false. *)
  FUNCTION al_is_system_installed: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_system_config: ALLEGRO_CONFIGptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_inhibit_screensaver (inhibit: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;



(*
 * threads.h
 *
 *   Thread routines.
 *****************************************************************************)

{ TODO:
   I'm not sure if this unit should be included.
   Is RTL's TThread compatible with Allegro?  There seems to be some problems
   as LCL fails when used with Allegro (I didn't test VCL yet).
}



(*
 * timer.h
 *
 *   Timer routines.
 *****************************************************************************)

(*** Converts microseconds to seconds. *)
  FUNCTION ALLEGRO_USECS_TO_SECS (x: AL_INT): AL_DOUBLE; INLINE;
(*** Converts milliseconds to seconds. *)
  FUNCTION ALLEGRO_MSECS_TO_SECS (x: AL_INT): AL_DOUBLE; INLINE;
(*** Converts beats per second to seconds. *)
  FUNCTION ALLEGRO_BPS_TO_SECS (x: AL_INT): AL_DOUBLE; INLINE;
(*** Converts beats per minute to seconds. *)
  FUNCTION ALLEGRO_BPM_TO_SECS (x: AL_INT): AL_DOUBLE; INLINE;



  FUNCTION al_create_timer (speed_secs: AL_DOUBLE): ALLEGRO_TIMERptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_destroy_timer (timer: ALLEGRO_TIMERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_start_timer (timer: ALLEGRO_TIMERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_stop_timer (timer: ALLEGRO_TIMERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_resume_timer (timer: ALLEGRO_TIMERptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(*** Returns @true if the timer specified is currently started. *)
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
 *
 *   Thread local storage routines.
 *****************************************************************************)

  CONST
    ALLEGRO_STATE_NEW_DISPLAY_PARAMETERS = $0001; {**< @exclude }
    ALLEGRO_STATE_NEW_BITMAP_PARAMETERS  = $0002; {**< @exclude }
    ALLEGRO_STATE_DISPLAY                = $0004; {**< @exclude }
    ALLEGRO_STATE_TARGET_BITMAP          = $0008; {**< @exclude }
    ALLEGRO_STATE_BITMAP                 = $000A; {**< @exclude
                                                   ALLEGRO_STATE_TARGET_BITMAP
                                       + ALLEGRO_STATE_NEW_BITMAP_PARAMETERS, }
    ALLEGRO_STATE_BLENDER                = $0010; {**< @exclude }
    ALLEGRO_STATE_NEW_FILE_INTERFACE     = $0020; {**< @exclude }
    ALLEGRO_STATE_TRANSFORM              = $0040; {**< @exclude }
    ALLEGRO_STATE_PROJECTION_TRANSFORM   = $0100; {**< @exclude }
    ALLEGRO_STATE_ALL                    = $FFFF; {**< @exclude }



  TYPE
    ALLEGRO_STATE = RECORD
    {** @exclude Internally, a thread_local_state structure is placed here. }
      ftls: ARRAY [0..1023] OF AL_CHAR;
    END;

  PROCEDURE al_store_state (OUT state: ALLEGRO_STATE; flags: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_restore_state (VAR state: ALLEGRO_STATE);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;


{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.  In
  delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here. }
{@exclude}
  FUNCTION _al_load_bitmap_f
    (fp: ALLEGRO_FILEptr; CONST ident: AL_STRptr): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME NAME 'al_load_bitmap_f';
{@exclude}
  FUNCTION _al_load_bitmap_flags_f (
    fp: ALLEGRO_FILEptr; CONST ident: AL_STRptr; flags: AL_INT
  ): ALLEGRO_BITMAPptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME NAME 'al_load_bitmap_flags_f';
{@exclude}
  PROCEDURE _al_set_memory_interface_ (iface: AL_POINTER);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME NAME 'al_set_memory_interface';
{@exclude}
  FUNCTION _al_check_inverse (VAR trans: ALLEGRO_TRANSFORM; tol: AL_FLOAT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME NAME 'al_check_inverse';

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
 * bitmap_io.h
 *****************************************************************************)

  FUNCTION al_load_bitmap_f
    (fp: ALLEGRO_FILEptr; CONST ident: AL_STR): ALLEGRO_BITMAPptr;
  BEGIN
    IF ident = '' THEN
      RESULT := _al_load_bitmap_f (fp, NIL)
    ELSE
      RESULT := _al_load_bitmap_f (fp, AL_STRptr (ident))
  END;



  FUNCTION al_load_bitmap_flags_f (
    fp: ALLEGRO_FILEptr; CONST ident: AL_STR; flags: AL_INT
  ): ALLEGRO_BITMAPptr;
  BEGIN
    IF ident = '' THEN
      RESULT := _al_load_bitmap_flags_f (fp, NIL, flags)
    ELSE
      RESULT := _al_load_bitmap_flags_f (fp, AL_STRptr (ident), flags)
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
 * transformation.h
 *****************************************************************************)

  FUNCTION al_check_inverse (VAR trans: ALLEGRO_TRANSFORM; tol: AL_FLOAT): AL_BOOL;
  BEGIN
    RESULT := _al_check_inverse (trans, tol) = 1
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
{ Next code suggested by FPC mailing list user.  This should fix some issues
  with memory.  It was removed as it seems to be fixed by Allegro itself.
  Anyway I'll keep it here in case the bug appears again. }

{ $if defined(cpui386) or defined(cpux86_64)}
{ SetExceptionMask(GetExceptionMask + [exZeroDivide, exInvalidOp]); }
{ $ENDIF}

FINALIZATION
{ Ensures that we call it, as Pascal hasn't an "atexit" function. }
  al_uninstall_system
END.
