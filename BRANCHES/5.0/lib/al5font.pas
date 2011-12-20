UNIT al5font;
(*<The Allegro 5 Font addon. *)
(*TODO: License. *)


INTERFACE

  USES
    Allegro5;

{$include allegro.cfg}

  TYPE
    ALLEGRO_FONT_VTABLEptr = ^ALLEGRO_FONT_VTABLE;
    ALLEGRO_FONT_VTABLE = RECORD
{
   ALLEGRO_FONT_METHOD(int, font_height, (const ALLEGRO_FONT *f));
   ALLEGRO_FONT_METHOD(int, font_ascent, (const ALLEGRO_FONT *f));
   ALLEGRO_FONT_METHOD(int, font_descent, (const ALLEGRO_FONT *f));
   ALLEGRO_FONT_METHOD(int, char_length, (const ALLEGRO_FONT *f, int ch));
   ALLEGRO_FONT_METHOD(int, text_length, (const ALLEGRO_FONT *f, const ALLEGRO_USTR *text));
   ALLEGRO_FONT_METHOD(int, render_char, (const ALLEGRO_FONT *f, ALLEGRO_COLOR color, int ch, float x, float y));
   ALLEGRO_FONT_METHOD(int, render, (const ALLEGRO_FONT *f, ALLEGRO_COLOR color, const ALLEGRO_USTR *text, float x, float y));
   ALLEGRO_FONT_METHOD(void, destroy, (ALLEGRO_FONT *f));
   ALLEGRO_FONT_METHOD(void, get_text_dimensions, (const ALLEGRO_FONT *f,
      const ALLEGRO_USTR *text, int *bbx, int *bby, int *bbw, int *bbh));
}
    END;

  (* A handle identifying any kind of font.

    Usually you will create it with al_load_font which supports loading all kinds of TrueType fonts supported by the FreeType library. If you instead pass the filename of a bitmap file, it will be loaded with @link(al_load_bitmap) and a font in Allegro's bitmap font format will be created from it with @link(al_grab_font_from_bitmap). *)
    ALLEGRO_FONTptr = ^ALLEGRO_FONT;
    ALLEGRO_FONT = RECORD
      data : POINTER;
      height : LONGINT;
      vtable : ALLEGRO_FONT_VTABLEptr;
    END;

    FONT_LOADER_FUNCTION = FUNCTION (CONST filename: PCHAR; size, flags: LONGINT): ALLEGRO_FONTptr; CDECL;

  CONST
    ALLEGRO_ALIGN_LEFT   = 0;
    ALLEGRO_ALIGN_CENTRE = 1;
    ALLEGRO_ALIGN_RIGHT  = 2;



(* Initialise the font addon.

   Note that if you intend to load bitmap fonts, you will need to initialise @link(al5image) separately (unless you are using another library to load images).
   @seealso(al_init_image_addon) @seealso(al_init_ttf_addon) @seealso(al_shutdown_font_addon) *)
  PROCEDURE al_init_font_addon; CDECL;

(* Shut down the font addon. This is done automatically at program exit, but can be called any time the user wishes as well.
   @seealso(al_init_font_addon) *)
  PROCEDURE al_shutdown_font_addon; CDECL;

(* Returns the (compiled) version of the addon, in the same format as @link(al_get_allegro_version.) *)
  FUNCTION al_get_allegro_font_version: LONGWORD; CDECL;

(* Informs Allegro of a new font file type, telling it how to load files of this format.

   The extension should include the leading dot ('.') character. It will be matched case-insensitively.

   @param(load_font May be @nil to unregister an entry.)
   @returns(@true on success, @false on error. Returns @false if unregistering an entry that doesn't exist.)
   @seealso(al_init_font_addon) *)
  FUNCTION al_register_font_loader (CONST ext: PCHAR; load: FONT_LOADER_FUNCTION): BOOLEAN; CDECL;

(* Load a bitmap font from. It does this by first calling @link(al_load_bitmap) and then @link(al_grab_font_from_bitmap). If you want to for example load an old A4 font, you could load the bitmap yourself, then call @link(al_convert_mask_to_alpha) on it and only then pass it to @link(al_grab_font_from_bitmap). *)
  FUNCTION al_load_bitmap_font (CONST filename: PCHAR): ALLEGRO_FONTptr; CDECL;

(* Loads a font from disk.

  This will use @link(al_load_bitmap_font) if you pass the name of a known bitmap format, or else @link(al_load_ttf_font).
  Bitmap and TTF fonts are affected by the current bitmap flags at the time the font is loaded.

  @seealso(al_destroy_font) @seealso(al_init_font_addon) @seealso(al_register_font_loader)
 *)
  FUNCTION al_load_font (CONST filename: PCHAR; size, flags: INTEGER): ALLEGRO_FONTptr; CDECL;

(* Creates a new font from an Allegro bitmap. You can delete the bitmap after the function returns as the font will contain a copy for itself.

   The bitmap format is as in the following example, which contains three glyphs for 1, 2 and 3.
@longcode(#
.............
. 1 .222.333.
. 1 .  2.  3.
. 1 .222.333.
. 1 .2  .  3.
. 1 .222.333.
.............
#)

   In the above illustration, the dot is for pixels having the background color. It is determined by the color of the top left pixel in the bitmap. There should be a border of at least 1 pixel with this color to the bitmap edge and between all glyphs.

   Each glyph is inside a rectangle of pixels not containing the background color. The height of all glyph rectangles should be the same, but the width can vary.

   The placement of the rectangles does not matter, except that glyphs are scanned from left to right and top to bottom to match them to the specified unicode codepoints.

   The glyphs will simply be drawn using al_draw_bitmap, so usually you will want the rectangles filled with full transparency and the glyphs drawn in opaque white.

   Next example will grab glyphs for the 95 standard printable ASCII characters, beginning with the space character (32) and ending with the tilde character (126):
@longcode(#
VAR
  Ranges = ARRAY (0..1) OF INTEGER = (32, 126);
BEGIN
  al_grab_font_from_bitmap (bitmap, 1, ranges)
END.
#)
   Next example will map the first 96 glyphs found in the bitmap to ASCII range, the next 95 glyphs to Latin 1, the next 128 glyphs to Extended-A, and the last glyph to the Euro character. (This is just the characters found in the Allegro 4 font.):
@longcode(#
VAR
  Ranges = ARRAY (0..7) OF INTEGER = (
    0x0020, 0x007F,  // ASCII
    0x00A1, 0x00FF,  // Latin 1
    0x0100, 0x017F,  // Extended-A
    0x20AC, 0x20AC); // Euro
BEGIN
  al_grab_font_from_bitmap (bitmap, 4, ranges)
END.
#)
   @param(bmp The bitmap with the glyphs drawn onto it.)
   @param(n Number of unicode ranges in the bitmap.)
   @param(ranges 'n' pairs of first and last unicode point to map glyphs to for each range.)
   @seealso(al_load_bitmap) @seealso(al_grab_font_from_bitmap) *)
  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: LONGINT; ranges: ARRAY OF LONGINT): ALLEGRO_FONTptr; CDECL;

{
  PROCEDURE al_draw_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: flags: LONGINT; CONST ustr: ALLEGRO_USTRptr); CDECL;
}

(* Writes the string text onto bmp at position x, y, using the specified font.
   @param(flags can be 0 or one of the following flags:@definitionList(
     @itemLabel(@code(ALLEGRO_ALIGN_LEFT))@item(Draw the text left-aligned (same as 0).)
     @itemLabel(@code(ALLEGRO_ALIGN_CENTRE)) @item(raw the text centered around the given position.)
     @itemLabel(@code(ALLEGRO_ALIGN_RIGHT)) @item(raw the text right-aligned to the given position.)
   ))
   @seealso(al_draw_ustr) @seealso(al_draw_textf) @seealso(al_draw_justified_text) *)
  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST str: PCHAR); CDECL;

(* Like @link(al_draw_text), but justifies the string to the specified area.
   @seealso(al_draw_justified_textf) @seealso(al_draw_justified_ustr) *)
  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: PCHAR); CDECL;

{
ALLEGRO_FONT_FUNC(void, al_draw_justified_ustr, (const ALLEGRO_FONT *font, ALLEGRO_COLOR color, float x1, float x2, float y, float diff, int flags, ALLEGRO_USTR const *text));
}

{
ALLEGRO_FONT_PRINTFUNC(void, al_draw_textf, (const ALLEGRO_FONT *font, ALLEGRO_COLOR color, float x, float y, int flags, char const *format, ...), 6, 7);
ALLEGRO_FONT_PRINTFUNC(void, al_draw_justified_textf, (const ALLEGRO_FONT *font, ALLEGRO_COLOR color, float x1, float x2, float y, float diff, int flags, char const *format, ...), 8, 9);
}

(* Calculates the length of a string in a particular font, in pixels.
   @seealso(al_get_ustr_width) @seealso(al_get_font_line_height) @seealso(al_get_text_dimensions) *)
  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: PCHAR): LONGINT; CDECL;

{
ALLEGRO_FONT_FUNC(int, al_get_ustr_width, (const ALLEGRO_FONT *f, const ALLEGRO_USTR *ustr));
}

(* Returns the usual height of a line of text in the specified font. For bitmap fonts this is simply the height of all glyph bitmaps. For truetype fonts it is whatever the font file specifies. In particular, some special glyphs may be higher than the height returned here.

   If the X is the position you specify to draw text, the meaning of ascent and descent and the line height is like in the figure below.
@longcode(#
X------------------------
    /\         |        |
   /  \        |        |
  /____\       ascent   |
 /      \      |        |
/        \     |        height
----------------        |
               |        |
               descent  |
               |        |
-------------------------
#)
   @seealso(al_get_text_width) @seealso(al_get_text_dimensions) *)
  FUNCTION al_get_font_line_height (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;

(* Returns the ascent of the specified font.
   @seealso(al_get_font_descent) @seealso(al_get_font_line_height) *)
  FUNCTION al_get_font_ascent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;

(* Returns the descent of the specified font.
   @seealso(al_get_font_ascent) @seealso(al_get_font_line_height) *)
  FUNCTION al_get_font_descent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;

(* Frees the memory being used by a font structure. Does nothing if passed @nil.
   @seealso(al_load_font) *)
  PROCEDURE al_destroy_font (font: ALLEGRO_FONTptr); CDECL;

{
ALLEGRO_FONT_FUNC(void, al_get_ustr_dimensions, (const ALLEGRO_FONT *f,
   ALLEGRO_USTR const *text,
   int *bbx, int *bby, int *bbw, int *bbh));
}

(* Sometimes, the @link(al_get_text_width) and @link(al_get_font_line_height) functions are not enough for exact text placement, so this function returns some additional information.

   Note that glyphs may go to the left and upwards of the X, in which case @code(bbx) and @code(bby) will have negative values.
   @param(bbx Offset to left of bounding box)
   @param(bby Offset to upper of bounding box.)
   @param(bbw Width of bounding box.)
   @param(bbh Height of bounding box.)
   @seealso(al_get_text_width) @seealso(al_get_font_line_height) @seealso(al_get_ustr_dimensions) *)
  PROCEDURE al_get_text_dimensions (CONST font: ALLEGRO_FONTptr; CONST str: PCHAR; bbx, bby, bbw, bbh: PLONGINT); CDECL;

IMPLEMENTATION

  PROCEDURE al_init_font_addon; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_shutdown_font_addon; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_allegro_font_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_register_font_loader (CONST ext: PCHAR; load: FONT_LOADER_FUNCTION): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_load_bitmap_font (CONST filename: PCHAR): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_load_font (CONST filename: PCHAR; size, flags: INTEGER): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: LONGINT; ranges: ARRAY OF LONGINT): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST str: PCHAR); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: PCHAR); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: PCHAR): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_font_line_height (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_font_ascent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_font_descent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_destroy_font (font: ALLEGRO_FONTptr); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_get_text_dimensions (CONST font: ALLEGRO_FONTptr; CONST str: PCHAR; bbx, bby, bbw, bbh: PLONGINT); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

END.
