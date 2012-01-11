UNIT al5font;
(*         ______   ___    ___
 *        /\  _  \ /\_ \  /\_ \
 *        \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___
 *         \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\
 *          \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \
 *           \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/
 *            \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/
 *                                           /\____/
 *                                           \_/__/
 *
 *      See readme.txt for copyright information.
 *)

INTERFACE

  USES
    Allegro5;

{$include allegro.cfg}

  TYPE
    ALLEGRO_FONT_VTABLEptr = POINTER;



    ALLEGRO_FONTptr = ^ALLEGRO_FONT;
    ALLEGRO_FONT = RECORD
      data : POINTER;
      height : LONGINT;
      vtable : ALLEGRO_FONT_VTABLEptr;
    END;



    FONT_LOADER_FUNCTION = FUNCTION (CONST filename: STRING; size, flags: LONGINT): ALLEGRO_FONTptr; CDECL;

  CONST
    ALLEGRO_ALIGN_LEFT   = 0;
    ALLEGRO_ALIGN_CENTRE = 1;
    ALLEGRO_ALIGN_RIGHT  = 2;

  FUNCTION al_register_font_loader (CONST ext: STRING; load: FONT_LOADER_FUNCTION): BOOLEAN; CDECL;
  FUNCTION al_load_bitmap_font (CONST filename: STRING): ALLEGRO_FONTptr; CDECL;
  FUNCTION al_load_font (CONST filename: STRING; size, flags: INTEGER): ALLEGRO_FONTptr; CDECL;

  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: LONGINT; ranges: ARRAY OF LONGINT): ALLEGRO_FONTptr; CDECL;

  PROCEDURE al_draw_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST ustr: ALLEGRO_USTRptr); CDECL;
  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST str: STRING); CDECL;
  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: STRING); CDECL;
  PROCEDURE al_draw_justified_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: ALLEGRO_USTRptr); CDECL;
{ No "format" procedures.  Use Pascal's Format function defined at sysutils unit instead. }
  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: STRING): LONGINT; CDECL;
  FUNCTION al_get_ustr_width (CONST font: ALLEGRO_FONTptr; CONST ustr: ALLEGRO_USTRptr): LONGINT; CDECL;
  FUNCTION al_get_font_line_height (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  FUNCTION al_get_font_ascent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  FUNCTION al_get_font_descent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  PROCEDURE al_destroy_font (font: ALLEGRO_FONTptr); CDECL;
  PROCEDURE al_get_ustr_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: ALLEGRO_USTRptr; VAR bbx, bby, bbw, bbh: LONGINT); CDECL;
  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: STRING; VAR bbx, bby, bbw, bbh: LONGINT); CDECL;
  PROCEDURE al_init_font_addon; CDECL;
  PROCEDURE al_shutdown_font_addon; CDECL;
  FUNCTION al_get_allegro_font_version: LONGWORD; CDECL;

IMPLEMENTATION

  FUNCTION al_register_font_loader (CONST ext: STRING; load: FONT_LOADER_FUNCTION): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_load_bitmap_font (CONST filename: STRING): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_load_font (CONST filename: STRING; size, flags: INTEGER): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: LONGINT; ranges: ARRAY OF LONGINT): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST ustr: ALLEGRO_USTRptr); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST str: STRING); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: STRING); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_draw_justified_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: ALLEGRO_USTRptr); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: STRING): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_ustr_width (CONST font: ALLEGRO_FONTptr; CONST ustr: ALLEGRO_USTRptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_font_line_height (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_font_ascent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_font_descent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_destroy_font (font: ALLEGRO_FONTptr); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_get_ustr_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: ALLEGRO_USTRptr; VAR bbx, bby, bbw, bbh: LONGINT); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: STRING; VAR bbx, bby, bbw, bbh: LONGINT); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_init_font_addon; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_shutdown_font_addon; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_allegro_font_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

END.
