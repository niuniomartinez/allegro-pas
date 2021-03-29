unit al5font;
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


{$INCLUDE allegro5.cfg}

interface

  uses
    Allegro5, al5base, al5strings;

  const
    ALLEGRO_NO_KERNING    = -1; {**<@exclude }
    ALLEGRO_ALIGN_LEFT    =  0; {**<@exclude }
    ALLEGRO_ALIGN_CENTRE  =  1; {**<@exclude }
    ALLEGRO_ALIGN_CENTER  =  1; {**<@exclude }
    ALLEGRO_ALIGN_RIGHT   =  2; {**<@exclude }
    ALLEGRO_ALIGN_INTEGER =  4; {**<@exclude }

  type
    ALLEGRO_FONTptr = AL_POINTER;
  (*** Callback declaration for @link(al_do_multiline_text). *)
    ALFONT_CALLBACK_MULTILINE_TEXT =
      function (line_num: AL_INT; const line: AL_STRptr; size: AL_INT; extra: AL_POINTER): AL_BOOL; CDECL;
  (*** Callback declaration for @link(al_do_multiline_ustr). *)
    ALFONT_CALLBACK_MULTILINE_USTR =
      function (line_num: AL_INT; const line: ALLEGRO_USTRptr; size: AL_INT; extra: AL_POINTER): AL_BOOL; CDECL;


(* Initialization. *)
  function al_init_font_addon: AL_BOOL; CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_shutdown_font_addon; CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_allegro_font_version: AL_UINT32; CDECL; external ALLEGRO_FONT_LIB_NAME;

  function al_load_bitmap_font (const filename: AL_STR): ALLEGRO_FONTptr;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_load_bitmap_font_flags (const filename: AL_STR; flags: AL_INT): ALLEGRO_FONTptr;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_load_font (const filename: AL_STR; size, flags: AL_INT): ALLEGRO_FONTptr;
    CDECL; external ALLEGRO_FONT_LIB_NAME;

  function al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: AL_INT; var ranges: array OF AL_INT): ALLEGRO_FONTptr;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_create_builtin_font: ALLEGRO_FONTptr;
    CDECL; external ALLEGRO_FONT_LIB_NAME;

  procedure al_draw_ustr (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; const ustr: ALLEGRO_USTRptr);
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_draw_text (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; const str: AL_STR);
    inline;
  procedure al_draw_justified_text (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; const str: AL_STR);
    inline;
  procedure al_draw_justified_ustr (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; const str: ALLEGRO_USTRptr);
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_draw_textf (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; const fmt: AL_STR; const values: array OF const);
  procedure al_draw_justified_textf (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; const fmt: AL_STR; const values: array OF const);
  function al_get_text_width (const font: ALLEGRO_FONTptr; const str: AL_STR): AL_INT;
    inline;
  function al_get_ustr_width (const font: ALLEGRO_FONTptr; const ustr: ALLEGRO_USTRptr): AL_INT;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_font_line_height (const font: ALLEGRO_FONTptr): AL_INT;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_font_ascent (const font: ALLEGRO_FONTptr): AL_INT;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_font_descent (const font: ALLEGRO_FONTptr): AL_INT;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_destroy_font (font: ALLEGRO_FONTptr);
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_get_ustr_dimensions (const f: ALLEGRO_FONTptr; const str: ALLEGRO_USTRptr; out bbx, bby, bbw, bbh: AL_INT);
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_get_text_dimensions (const f: ALLEGRO_FONTptr; const str: AL_STR; out bbx, bby, bbw, bbh: AL_INT);
    inline;
{ These functions where moved to the beginning of the file.
  function al_init_font_addon: AL_BOOL;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_shutdown_font_addon;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_allegro_font_version: AL_UINT32;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
}
  function al_get_font_ranges (font: ALLEGRO_FONTptr; ranges_count: AL_INT; var ranges: array OF AL_INT): AL_INT;
    CDECL; external ALLEGRO_FONT_LIB_NAME;

  procedure al_draw_glyph (const f: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; codepoint: AL_INT);
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_glyph_width (const f: ALLEGRO_FONTptr; codepoint: AL_INT): AL_INT;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_glyph_dimensions (const f: ALLEGRO_FONTptr; codepoint: AL_INT; out bbx, bby, bbw, bbh: AL_INT): AL_BOOL;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_glyph_advance (const f: ALLEGRO_FONTptr; codepoint1, codepoint2: AL_INT): AL_INT;
    CDECL; external ALLEGRO_FONT_LIB_NAME;
{ TODO:
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_FONT_SRC)
ALLEGRO_FONT_FUNC(bool, al_get_glyph, (const ALLEGRO_FONT *f, int prev_codepoint, int codepoint, ALLEGRO_GLYPH *glyph));
}

  procedure al_draw_multiline_text (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y, max_width, line_height: AL_FLOAT; flags: AL_INT; const str: AL_STR);
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_draw_multiline_textf (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y, max_width, line_height: AL_FLOAT; flags: AL_INT; const fmt: AL_STR; const values: array OF const);
  procedure al_draw_multiline_ustr (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y, max_width, line_height: AL_FLOAT; flags: AL_INT; const str: ALLEGRO_USTRptr);
    CDECL; external ALLEGRO_FONT_LIB_NAME;

  procedure al_do_multiline_text (const font: ALLEGRO_FONTptr; max_width: AL_FLOAT; str: AL_STR; cb: ALFONT_CALLBACK_MULTILINE_TEXT; extra: AL_POINTER);
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  procedure al_do_multiline_ustr (const font: ALLEGRO_FONTptr; max_width: AL_FLOAT; str: ALLEGRO_USTRptr; cb: ALFONT_CALLBACK_MULTILINE_USTR; extra: AL_POINTER);
    CDECL; external ALLEGRO_FONT_LIB_NAME;

  procedure al_set_fallback_font (font, fallback: ALLEGRO_FONTptr);
    CDECL; external ALLEGRO_FONT_LIB_NAME;
  function al_get_fallback_font (font: ALLEGRO_FONTptr): ALLEGRO_FONTptr;
    CDECL; external ALLEGRO_FONT_LIB_NAME;



{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.  In
  delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here. }
{**@exclude}
  procedure _al_draw_text (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; const str: AL_STR);
    CDECL; external ALLEGRO_FONT_LIB_NAME NAME 'al_draw_text';
{**@exclude}
  procedure _al_draw_justified_text (const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; const str: AL_STR);
    CDECL; external ALLEGRO_FONT_LIB_NAME NAME 'al_draw_justified_text';
{**@exclude}
  function _al_get_text_width (const font: ALLEGRO_FONTptr; const str: AL_STR): AL_INT;
    CDECL; external ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_width';
{**@exclude}
  procedure _al_get_text_dimensions (const f: ALLEGRO_FONTptr; const str: AL_STR; out bbx, bby, bbw, bbh: AL_INT);
    CDECL; external ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_dimensions';

implementation

  uses
  {$IFDEF ISDELPHI2009ANDUP}
  { This unit implements SysUtils and Strings using ANSISTRING instead of
    UNICODESTRING, which is the default in modern Delphi compilers. }
    AnsiStrings;
  {$ELSE}
    sysutils;
  {$ENDIF}

  procedure al_draw_text (
    const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x, y: AL_FLOAT; flags: AL_INT;
    const str: AL_STR
  );
  begin
    if str <> '' then _al_draw_text (font, color, x, y, flags, str)
  end;



  procedure al_draw_justified_text (
    const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x1, x2, y, diff: AL_FLOAT; flags: AL_INT;
    const str: AL_STR
  );
  begin
    if str <> '' then
      _al_draw_justified_text (font, color, x1, x2, y, diff, flags, str)
  end;



  procedure al_draw_textf (
    const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x, y: AL_FLOAT; flags: AL_INT;
    const fmt: AL_STR; const values: array OF const
  );
  begin
    al_draw_text (font, color, x, y, flags, Format (fmt, values))
  end;



  procedure al_draw_justified_textf (
    const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x1, x2, y, diff: AL_FLOAT; flags: AL_INT;
    const fmt: AL_STR; const values: array OF const
  );
  begin
    al_draw_justified_text (
      font, color,
      x1, x2, y, diff, flags,
      Format (fmt, values)
    )
  end;



  function al_get_text_width (const font: ALLEGRO_FONTptr; const str: AL_STR)
    : AL_INT;
  begin
    if str <> '' then
      Result := _al_get_text_width (font, str)
    else
      Result := 0
  end;



  procedure al_get_text_dimensions (
    const f: ALLEGRO_FONTptr; const str: AL_STR;
    out bbx, bby, bbw, bbh: AL_INT
  );
  begin
    if str <> '' then
      _al_get_text_dimensions (f, str, bbx, bby, bbw, bbh)
    else begin
      bbx := 0; bby := 0; bbw := 0; bbh := 0
    end
  end;



  procedure al_draw_multiline_textf (
    const font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR;
    x, y, max_width, line_height: AL_FLOAT; flags: AL_INT;
    const fmt: AL_STR; const values: array OF const
  );
  begin
    al_draw_multiline_text (
      font, color,
      x, y, max_width, line_height, flags,
      Format (fmt, values)
    );
  end;

end.
