UNIT al5font;
(***<Text font management.

  @bold(See also)

  @link(al5ttf) *)
(* Copyright (c) 2012-2020 Guillermo Martínez J.

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
    Allegro5, al5base, al5strings;

  CONST
    ALLEGRO_NO_KERNING    = -1; {**<@exclude }
    ALLEGRO_ALIGN_LEFT    =  0; {**<@exclude }
    ALLEGRO_ALIGN_CENTRE  =  1; {**<@exclude }
    ALLEGRO_ALIGN_CENTER  =  1; {**<@exclude }
    ALLEGRO_ALIGN_RIGHT   =  2; {**<@exclude }
    ALLEGRO_ALIGN_INTEGER =  4; {**<@exclude }

  TYPE
    ALLEGRO_FONTptr = AL_POINTER;
  (*** Callback declaration for @link(al_do_multiline_text). *)
    ALFONT_CALLBACK_MULTILINE_TEXT =
      FUNCTION (line_num: AL_INT; CONST line: AL_STRptr; size: AL_INT; extra: AL_POINTER): AL_BOOL; CDECL;
  (*** Callback declaration for @link(al_do_multiline_ustr). *)
    ALFONT_CALLBACK_MULTILINE_USTR =
      FUNCTION (line_num: AL_INT; CONST line: ALLEGRO_USTRptr; size: AL_INT; extra: AL_POINTER): AL_BOOL; CDECL;


(* Initialization. *)
  FUNCTION al_init_font_addon: AL_BOOL; CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_shutdown_font_addon; CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_allegro_font_version: AL_UINT32; CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_load_bitmap_font (CONST filename: AL_STR): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_load_bitmap_font_flags (CONST filename: AL_STR; flags: AL_INT): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_load_font (CONST filename: AL_STR; size, flags: AL_INT): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: AL_INT; VAR ranges: ARRAY OF AL_INT): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_create_builtin_font: ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST ustr: ALLEGRO_USTRptr);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    INLINE;
  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    INLINE;
  PROCEDURE al_draw_justified_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: ALLEGRO_USTRptr);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_draw_textf (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST fmt: AL_STR; CONST values: ARRAY OF CONST);
  PROCEDURE al_draw_justified_textf (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST fmt: AL_STR; CONST values: ARRAY OF CONST);
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
  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: AL_STR; OUT bbx, bby, bbw, bbh: AL_INT);
    INLINE;
{ These functions where moved to the beginning of the file.
  FUNCTION al_init_font_addon: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_shutdown_font_addon;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_allegro_font_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
}
  FUNCTION al_get_font_ranges (font: ALLEGRO_FONTptr; ranges_count: AL_INT; VAR ranges: ARRAY OF AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_glyph (CONST f: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; codepoint: AL_INT);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_glyph_width (CONST f: ALLEGRO_FONTptr; codepoint: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_glyph_dimensions (CONST f: ALLEGRO_FONTptr; codepoint: AL_INT; OUT bbx, bby, bbw, bbh: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_glyph_advance (CONST f: ALLEGRO_FONTptr; codepoint1, codepoint2: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
{ TODO:
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_FONT_SRC)
ALLEGRO_FONT_FUNC(bool, al_get_glyph, (const ALLEGRO_FONT *f, int prev_codepoint, int codepoint, ALLEGRO_GLYPH *glyph));
}

  PROCEDURE al_draw_multiline_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y, max_width, line_height: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_draw_multiline_textf (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y, max_width, line_height: AL_FLOAT; flags: AL_INT; CONST fmt: AL_STR; CONST values: ARRAY OF CONST);
  PROCEDURE al_draw_multiline_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y, max_width, line_height: AL_FLOAT; flags: AL_INT; CONST str: ALLEGRO_USTRptr);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_do_multiline_text (CONST font: ALLEGRO_FONTptr; max_width: AL_FLOAT; str: AL_STR; cb: ALFONT_CALLBACK_MULTILINE_TEXT; extra: AL_POINTER);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_do_multiline_ustr (CONST font: ALLEGRO_FONTptr; max_width: AL_FLOAT; str: ALLEGRO_USTRptr; cb: ALFONT_CALLBACK_MULTILINE_USTR; extra: AL_POINTER);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_set_fallback_font (font, fallback: ALLEGRO_FONTptr);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_fallback_font (font: ALLEGRO_FONTptr): ALLEGRO_FONTptr;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME;



{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.  In
  delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here. }
{**@exclude}
  PROCEDURE _al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_draw_text';
{**@exclude}
  PROCEDURE _al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: AL_STR);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_draw_justified_text';
{**@exclude}
  FUNCTION _al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: AL_STR): AL_INT;
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_width';
{**@exclude}
  PROCEDURE _al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: AL_STR; OUT bbx, bby, bbw, bbh: AL_INT);
    CDECL; EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_dimensions';

IMPLEMENTATION

  USES
  {$IFDEF ISDELPHI2009ANDUP}
  { This unit implements SysUtils and Strings using ANSISTRING instead of
    UNICODESTRING, which is the default in modern Delphi compilers. }
    AnsiStrings;
  {$ELSE}
    sysutils;
  {$ENDIF}

  PROCEDURE al_draw_text (
    CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x, y: AL_FLOAT; flags: AL_INT;
    CONST str: AL_STR
  );
  BEGIN
    IF str <> '' THEN _al_draw_text (font, color, x, y, flags, str)
  END;



  PROCEDURE al_draw_justified_text (
    CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x1, x2, y, diff: AL_FLOAT; flags: AL_INT;
    CONST str: AL_STR
  );
  BEGIN
    IF str <> '' THEN
      _al_draw_justified_text (font, color, x1, x2, y, diff, flags, str)
  END;



  PROCEDURE al_draw_textf (
    CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x, y: AL_FLOAT; flags: AL_INT;
    CONST fmt: AL_STR; CONST values: ARRAY OF CONST
  );
  BEGIN
    al_draw_text (font, color, x, y, flags, Format (fmt, values))
  END;



  PROCEDURE al_draw_justified_textf (
    CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x1, x2, y, diff: AL_FLOAT; flags: AL_INT;
    CONST fmt: AL_STR; CONST values: ARRAY OF CONST
  );
  BEGIN
    al_draw_justified_text (
      font, color,
      x1, x2, y, diff, flags,
      Format (fmt, values)
    )
  END;



  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: AL_STR)
    : AL_INT;
  BEGIN
    IF str <> '' THEN
      RESULT := _al_get_text_width (font, str)
    ELSE
      RESULT := 0
  END;



  PROCEDURE al_get_text_dimensions (
    CONST f: ALLEGRO_FONTptr; CONST str: AL_STR;
    OUT bbx, bby, bbw, bbh: AL_INT
  );
  BEGIN
    IF str <> '' THEN
      _al_get_text_dimensions (f, str, bbx, bby, bbw, bbh)
    ELSE BEGIN
      bbx := 0; bby := 0; bbw := 0; bbh := 0
    END
  END;



  PROCEDURE al_draw_multiline_textf (
    CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x, y, max_width, line_height: AL_FLOAT; flags: AL_INT;
    CONST fmt: AL_STR; CONST values: ARRAY OF CONST
  );
  BEGIN
    al_draw_multiline_text (
      font, color,
      x, y, max_width, line_height, flags,
      Format (fmt, values)
    );
  END;

END.
