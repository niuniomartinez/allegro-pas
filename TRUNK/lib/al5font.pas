UNIT al5font;
(*<Text font management.

  @bold(See also)

  @link(al5ttf) *)
(* Copyright (c) 2012-2017 Guillermo Martínez J.

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


{$include allegro5.cfg}

INTERFACE

  USES
    Allegro5, al5base;

  CONST
  (* Builds library name. *)
    { @exclude }
    ALLEGRO_FONT_LIB_NAME = _A5_LIB_PREFIX_+'allegro_font'+_DBG_+_A5_LIB_EXT_;

  TYPE
    { @exclude }
    ALLEGRO_FONT_VTABLEptr = AL_POINTER;



  (* Pointer to @link(ALLEGRO_FONT). *)
    ALLEGRO_FONTptr = ^ALLEGRO_FONT;
  (* A handle identifying any kind of font. Usually you will create it with
     @link(al_load_font) which supports loading all kinds of TrueType fonts
     supported by the FreeType library. If you instead pass the filename of
     a bitmap file, it will be loaded with @link(al_load_bitmap) and a font in
     Allegro's bitmap font format will be created from it with
     @link(al_grab_font_from_bitmap). *)
    ALLEGRO_FONT = RECORD
      data : AL_VOIDptr;
      height : AL_INT;
      fallback: ALLEGRO_FONTptr;
      vtable : ALLEGRO_FONT_VTABLEptr;
    END;



  { TODO: Needs Allegro's file functions.

    FONT_LOADER_FUNCTION = FUNCTION (CONST filename: AL_STRptr; size, flags: AL_INT): ALLEGRO_FONTptr; CDECL;
  }

  CONST
    ALLEGRO_NO_KERNING    = -1;
    ALLEGRO_ALIGN_LEFT    =  0;
    ALLEGRO_ALIGN_CENTRE  =  1;
    ALLEGRO_ALIGN_CENTER  =  1;
    ALLEGRO_ALIGN_RIGHT   =  2;
    ALLEGRO_ALIGN_INTEGER =  4;

{ TODO: Needs Allegro's file functions.
  FUNCTION al_register_font_loader (CONST ext: AL_STR; load: FONT_LOADER_FUNCTION): AL_BOOL; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
}
(* Loads a bitmap font from a file. This is done by first calling
   @link(al_load_bitmap_flags) and then @link(al_grab_font_from_bitmap).

   If you wanted to load an old A4 font, for example, it would be better to
   load the bitmap yourself in order to call @link(al_convert_mask_to_alpha) on
   it before passing it to al_grab_font_from_bitmap.
   @seealso(al_load_bitmap_font_flags) @seealso(al_load_font)
   @seealso(al_load_bitmap_flags) *)
  FUNCTION al_load_bitmap_font (CONST filename: AL_STR): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_load_bitmap_font_flags (CONST filename: AL_STR; flags: AL_INT): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
(* Loads a font from disk. This will use @link(al_load_bitmap_font_flags) if
   you pass the name of a known bitmap format, or else @link(al_load_ttf_font).

   The @code(flags) parameter is passed through to either of those functions.
   Bitmap and TTF fonts are also affected by the current bitmap flags at the
   time the font is loaded.
   @seealso(al_destroy_font) @seealso(al_init_font_addon)
   @seealso(al_register_font_loader) @seealso(al_load_bitmap_font_flags)
   @seealso(al_load_ttf_font) *)
  FUNCTION al_load_font (CONST filename: AL_STR; size, flags: AL_INT): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

(* Creates a new font from an Allegro bitmap. You can delete the bitmap after
   the function returns as the font will contain a copy for itself.

    The bitmap format is as in the following example, which contains three
    glyphs for 1, 2 and 3.
@longcode(#
.............
. 1 .222.333.
. 1 .  2.  3.
. 1 .222.333.
. 1 .2  .  3.
. 1 .222.333.
.............
#)
    In the above illustration, the dot is for pixels having the background
    color. It is determined by the color of the top left pixel in the bitmap.
    There should be a border of at least 1 pixel with this color to the bitmap
    edge and between all glyphs.

    Each glyph is inside a rectangle of pixels not containing the background
    color. The height of all glyph rectangles should be the same, but the width
    can vary.

    The placement of the rectangles does not matter, except that glyphs are
    scanned from left to right and top to bottom to match them to the specified
    unicode codepoints.

    The glyphs will simply be drawn using @link(al_draw_bitmap), so usually you
    will want the rectangles filled with full transparency and the glyphs drawn
    in opaque white.

    Examples:
@longcode(#
VAR
  Ranges: ARRAY (0..1) OF AL_INT = (32, 126);
BEGIN
  al_grab_font_from_bitmap (Bitmap, 1, Ranges)
END;

VAR
  Ranges = ARRAY (0..7) OF AL_INT = (
    0x0020, 0x007F,  // ASCII
    0x00A1, 0x00FF,  // Latin 1
    0x0100, 0x017F,  // Extended-A
    0x20AC, 0x20AC   // Euro
  );
BEGIN
  al_grab_font_from_bitmap (Bitmap, 4, Ranges)
END;
#)

    The first example will grab glyphs for the 95 standard printable ASCII
    characters, beginning with the space character (32) and ending with the
    tilde character (126).  The second example will map the first 96 glyphs
    found in the bitmap to ASCII range, the next 95 glyphs to Latin 1, the next
    128 glyphs to Extended-A, and the last glyph to the Euro character. (This
    is just the characters found in the Allegro 4 font.)
    @param(bmp The bitmap with the glyphs drawn onto it.)
    @param(n Number of unicode ranges in the bitmap.)
    @param(ranges @italic(n) pairs of first and last unicode point to map glyphs to for each range.)
    @seealso(al_load_bitmap) *)
  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: AL_INT; VAR ranges: ARRAY OF AL_INT): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_create_builtin_font: ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST ustr: ALLEGRO_USTRptr);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
(* Writes the string text onto the target bitmap at position @code(x), @code(y),
   using the specified @code(font).

   The flags parameter can be 0 or one of the following flags:@unorderedlist(
    @item(@bold(@code(ALLEGRO_ALIGN_LEFT)) - Draw the text left-aligned
     @(same as 0@).)
    @item(@bold(@code(ALLEGRO_ALIGN_CENTRE)) - Draw the text centered around
     the given position.)
    @item(@bold(@code(ALLEGRO_ALIGN_RIGHT)) - Draw the text right-aligned to
     the given position.)
    )
    It can also be combined with this flag:@unorderedlist(
     @item(@bold(@code(ALLEGRO_ALIGN_INTEGER)) - Always draw text aligned to an
      integer pixel position. This was formerly the default behaviour.)
    )
    This function does not support newline characters (#10), but you can use
    @link(al_draw_multiline_text) for multi line text output.
    @seealso(al_draw_ustr) @seealso(al_draw_justified_text)
    @seealso(al_draw_multiline_text) *)
  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    INLINE;
  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    INLINE;
  PROCEDURE al_draw_justified_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: ALLEGRO_USTRptr);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: AL_STR): AL_INT;
    INLINE;
  FUNCTION al_get_ustr_width (CONST font: ALLEGRO_FONTptr; CONST ustr: ALLEGRO_USTRptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_font_line_height (CONST font: ALLEGRO_FONTptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_font_ascent (CONST font: ALLEGRO_FONTptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_font_descent (CONST font: ALLEGRO_FONTptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_destroy_font (font: ALLEGRO_FONTptr);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_get_ustr_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: ALLEGRO_USTRptr; OUT bbx, bby, bbw, bbh: AL_INT);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: AL_STR; VAR bbx, bby, bbw, bbh: AL_INT);
    INLINE;
(* Initialise the font addon.

   Note that if you intend to load bitmap fonts, you will need to initialise
   @link(al5image) separately (unless you are using another library to load
   images).

   Similarly, if you wish to load truetype-fonts, do not forget to also call
   @link(al_init_ttf_addon).
   @return(@true on success, @false on failure.)
   @seealso(al_init_image_addon) @seealso(al_init_ttf_addon)
   @seealso(al_shutdown_font_addon) *)
  FUNCTION al_init_font_addon: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_shutdown_font_addon;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
(* Returns the (compiled) version of the addon, in the same format as
   @link(al_get_allegro_version). *)
  FUNCTION al_get_allegro_font_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
(* Gets information about all glyphs contained in a font, as a list of ranges.
   Ranges have the same format as with @code(al_grab_font_from_bitmap).
   @param(ranges_count Is the maximum number of ranges that will be returned.)
   @param(ranges Should be an array with room for @code(ranges_count * 2)
     elements.  The even integers are the first unicode point in a range, the
     odd integers the last unicode point in a range.)
   @return(The number of ranges contained in the font @(even if it is bigger than
     @code(ranges_count)@).)
   @seealso(al_grab_font_from_bitmap) *)
  FUNCTION al_get_font_ranges (font: ALLEGRO_FONTptr; ranges_count: AL_INT; VAR ranges: ARRAY OF AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_glyph (CONST f: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; codepoint: AL_INT); CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_glyph_advance (CONST f: ALLEGRO_FONTptr; codepoint1, codepoint2: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;



{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.  In
  delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here. }
{@exclude}
  PROCEDURE _al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_draw_text';
{@exclude}
  PROCEDURE _al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_draw_justified_text';
{@exclude}
  FUNCTION _al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: AL_STR): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_width';
{@exclude}
  PROCEDURE _al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: AL_STR; VAR bbx, bby, bbw, bbh: AL_INT);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_dimensions';

IMPLEMENTATION

  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
  BEGIN
    IF str <> '' THEN _al_draw_text (font, color, x, y, flags, str)
  END;

  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
  BEGIN
    IF str <> '' THEN _al_draw_justified_text (font, color, x1, x2, y, diff, flags, str)
  END;

  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: AL_STR): AL_INT;
  BEGIN
    IF str <> '' THEN
      RESULT := _al_get_text_width (font, str)
    ELSE
      RESULT := 0
  END;

  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: AL_STR; VAR bbx, bby, bbw, bbh: AL_INT);
  BEGIN
    IF str <> '' THEN
      _al_get_text_dimensions (f, str, bbx, bby, bbw, bbh)
    ELSE BEGIN
      bbx := 0; bby := 0; bbw := 0; bbh := 0
    END
  END;

END.
