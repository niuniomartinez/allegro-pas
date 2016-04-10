UNIT al5font;
(*<Text font management. *)
(* Copyright (c) 2012-2016 Guillermo Martínez J.

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
    ALLEGRO_FONT_VTABLEptr = AL_POINTER;



    ALLEGRO_FONTptr = ^ALLEGRO_FONT;
    ALLEGRO_FONT = RECORD
      data : AL_VOIDptr;
      height : AL_INT;
      fallback: ALLEGRO_FONTptr;
      vtable : ALLEGRO_FONT_VTABLEptr;
    END;



    FONT_LOADER_FUNCTION = FUNCTION (CONST filename: AL_STRptr; size, flags: AL_INT): ALLEGRO_FONTptr; CDECL;

  CONST
    ALLEGRO_NO_KERNING    = -1;
    ALLEGRO_ALIGN_LEFT    =  0;
    ALLEGRO_ALIGN_CENTRE  =  1;
    ALLEGRO_ALIGN_CENTER  =  1;
    ALLEGRO_ALIGN_RIGHT   =  2;
    ALLEGRO_ALIGN_INTEGER =  4;

  FUNCTION al_register_font_loader (CONST ext: AL_STR; load: FONT_LOADER_FUNCTION): AL_BOOL; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_load_bitmap_font (CONST filename: AL_STR): ALLEGRO_FONTptr; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_load_font (CONST filename: AL_STR; size, flags: AL_INT): ALLEGRO_FONTptr; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: AL_INT; VAR ranges: ARRAY OF AL_INT): ALLEGRO_FONTptr; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_create_builtin_font: ALLEGRO_FONTptr; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST ustr: ALLEGRO_USTRptr); CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST str: AL_STR); CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: AL_STR); CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_draw_justified_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST str: ALLEGRO_USTRptr); CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_draw_textf (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST aFormat: AL_STR; vars: ARRAY OF CONST);
  PROCEDURE al_draw_justified_textf (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST aFormat: AL_STR; vars: ARRAY OF CONST);

{ No "format" procedures.  Use Pascal's Format function defined at sysutils unit instead. }
  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: STRING): AL_INT; INLINE;
  FUNCTION al_get_ustr_width (CONST font: ALLEGRO_FONTptr; CONST ustr: ALLEGRO_USTRptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_font_line_height (CONST font: ALLEGRO_FONTptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_font_ascent (CONST font: ALLEGRO_FONTptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_font_descent (CONST font: ALLEGRO_FONTptr): AL_INT; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_destroy_font (font: ALLEGRO_FONTptr); CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_get_ustr_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: ALLEGRO_USTRptr; VAR bbx, bby, bbw, bbh: AL_INT); CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: STRING; VAR bbx, bby, bbw, bbh: AL_INT); INLINE;
  PROCEDURE al_init_font_addon; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  PROCEDURE al_shutdown_font_addon; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_glyph (CONST f: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; codepoint: AL_INT); CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;
  FUNCTION al_get_glyph_advance (CONST f: ALLEGRO_FONTptr; codepoint1, codepoint2: AL_INT): AL_INT; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;

(* Returns the (compiled) version of the addon, in the same format as @link(al_get_allegro_version). *)
  FUNCTION al_get_allegro_font_version: AL_UINT32; CDECL;
    EXTERNAL ALLEGRO_FONT_LIB_NAME;

IMPLEMENTATION

  USES
    sysutils;

  PROCEDURE al_draw_textf (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: AL_FLOAT; flags: AL_INT; CONST aFormat: AL_STR; vars: ARRAY OF CONST);
  BEGIN
    al_draw_text (font, color, x, y, flags, Format (aFormat, vars))
  END;



  PROCEDURE al_draw_justified_textf (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: AL_FLOAT; flags: AL_INT; CONST aFormat: AL_STR; vars: ARRAY OF CONST);
  BEGIN
    al_draw_justified_text (font, color, x1, x2, y, diff, flags, Format (aFormat, vars))
  END;



  FUNCTION _al_get_text_width_ (CONST font: ALLEGRO_FONTptr; CONST str: AL_STRptr): AL_INT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_width';

  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: STRING): AL_INT;
  BEGIN
    al_get_text_width := _al_get_text_width_ (font, AL_STRptr (str));
  END;



  PROCEDURE _al_get_text_dimensions_ (CONST f: ALLEGRO_FONTptr; CONST str: AL_STRptr; VAR bbx, bby, bbw, bbh: AL_INT); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_dimensions';

  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: STRING; VAR bbx, bby, bbw, bbh: AL_INT);
  BEGIN
    _al_get_text_dimensions_ (f, AL_STRptr (str), bbx, bby, bbw, bbh);
  END;

END.
