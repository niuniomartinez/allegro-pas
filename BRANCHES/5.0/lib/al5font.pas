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

{$include allegro.cfg}

INTERFACE

  USES
    Allegro5;

{$include allegro.inc}

  TYPE
    ALLEGRO_FONT_VTABLEptr = POINTER;



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

  FUNCTION al_register_font_loader (CONST ext: STRING; load: FONT_LOADER_FUNCTION): BYTEBOOL; INLINE;
  FUNCTION al_load_bitmap_font (CONST filename: STRING): ALLEGRO_FONTptr; INLINE;
  FUNCTION al_load_font (CONST filename: STRING; size, flags: INTEGER): ALLEGRO_FONTptr; INLINE;

  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: LONGINT; ranges: ARRAY OF LONGINT): ALLEGRO_FONTptr; INLINE;

  PROCEDURE al_draw_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST ustr: ALLEGRO_USTRptr); CDECL;
  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST str: STRING); INLINE;
  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: STRING); INLINE;
  PROCEDURE al_draw_justified_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: ALLEGRO_USTRptr); CDECL;
{ No "format" procedures.  Use Pascal's Format function defined at sysutils unit instead. }
  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: STRING): LONGINT; INLINE;
  FUNCTION al_get_ustr_width (CONST font: ALLEGRO_FONTptr; CONST ustr: ALLEGRO_USTRptr): LONGINT; CDECL;
  FUNCTION al_get_font_line_height (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  FUNCTION al_get_font_ascent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  FUNCTION al_get_font_descent (CONST font: ALLEGRO_FONTptr): LONGINT; CDECL;
  PROCEDURE al_destroy_font (font: ALLEGRO_FONTptr); CDECL;
  PROCEDURE al_get_ustr_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: ALLEGRO_USTRptr; VAR bbx, bby, bbw, bbh: LONGINT); CDECL;
  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: STRING; VAR bbx, bby, bbw, bbh: LONGINT); INLINE;
  PROCEDURE al_init_font_addon; CDECL;
  PROCEDURE al_shutdown_font_addon; CDECL;
  FUNCTION al_get_allegro_font_version: LONGWORD; CDECL;

IMPLEMENTATION

  USES
    al5data;

  FUNCTION _al_register_font_loader_ (CONST ext: PCHAR; load: FONT_LOADER_FUNCTION): BYTEBOOL; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_register_font_loader';

  FUNCTION al_register_font_loader (CONST ext: STRING; load: FONT_LOADER_FUNCTION): BYTEBOOL;
  VAR
    ExtZ: PCHAR;
  BEGIN
    ExtZ := _al_create_null_str_ (ext);
    al_register_font_loader := _al_register_font_loader_ (ExtZ, load);
    _al_dispose_str_ (ExtZ);
  END;



  FUNCTION _al_load_bitmap_font_ (CONST filename: PCHAR): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_load_bitmap_font';

  FUNCTION al_load_bitmap_font (CONST filename: STRING): ALLEGRO_FONTptr;
  VAR
    FilenameZ: PCHAR;
  BEGIN
    FilenameZ := _al_create_null_str_ (filename);
    al_load_bitmap_font := _al_load_bitmap_font_ (FilenameZ);
    _al_dispose_str_ (FilenameZ);
  END;



  FUNCTION _al_load_font_ (CONST filename: PCHAR; size, flags: INTEGER): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_load_font';

  FUNCTION al_load_font (CONST filename: STRING; size, flags: INTEGER): ALLEGRO_FONTptr;
  VAR
    FilenameZ: PCHAR;
  BEGIN
    FilenameZ := _al_create_null_str_ (filename);
    al_load_font := _al_load_font_ (FilenameZ, size, flags);
    _al_dispose_str_ (FilenameZ);
  END;



  FUNCTION _al_grab_font_from_bitmap_ (bmp: ALLEGRO_BITMAPptr; n: LONGINT; ranges: PLONGINT): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_grab_font_from_bitmap';

  FUNCTION al_grab_font_from_bitmap (bmp: ALLEGRO_BITMAPptr; n: LONGINT; ranges: ARRAY OF LONGINT): ALLEGRO_FONTptr;
  BEGIN
    al_grab_font_from_bitmap := _al_grab_font_from_bitmap_ (bmp, n, @ranges[0]);
  END;



  PROCEDURE al_draw_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST ustr: ALLEGRO_USTRptr); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE _al_draw_text_ (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST str: PCHAR); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_draw_text';

  PROCEDURE al_draw_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x, y: SINGLE; flags: LONGINT; CONST str: STRING);
  VAR
    TheString: PCHAR;
  BEGIN
    TheString := _al_create_null_str_ (str);
    _al_draw_text_ (font, color, x, y, flags, TheString);
    _al_dispose_str_ (TheString);
  END;



  PROCEDURE _al_draw_justified_text_ (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: PCHAR); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_draw_justified_text';

  PROCEDURE al_draw_justified_text (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: STRING);
  VAR
    TheString: PCHAR;
  BEGIN
    TheString := _al_create_null_str_ (str);
    _al_draw_justified_text_ (font, color, x1, x2, y, diff, flags, TheString);
    _al_dispose_str_ (TheString);
  END;



  PROCEDURE al_draw_justified_ustr (CONST font: ALLEGRO_FONTptr; color: ALLEGRO_COLOR; x1, x2, y, diff: SINGLE; flags: LONGINT; CONST str: ALLEGRO_USTRptr); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION _al_get_text_width_ (CONST font: ALLEGRO_FONTptr; CONST str: PCHAR): LONGINT; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_width';

  FUNCTION al_get_text_width (CONST font: ALLEGRO_FONTptr; CONST str: STRING): LONGINT;
  VAR
    TheString: PCHAR;
  BEGIN
    TheString := _al_create_null_str_ (str);
    al_get_text_width := _al_get_text_width_ (font, TheString);
    _al_dispose_str_ (TheString);
  END;



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

  PROCEDURE _al_get_text_dimensions_ (CONST f: ALLEGRO_FONTptr; CONST str: PCHAR; VAR bbx, bby, bbw, bbh: LONGINT); CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME NAME 'al_get_text_dimensions';

  PROCEDURE al_get_text_dimensions (CONST f: ALLEGRO_FONTptr; CONST str: STRING; VAR bbx, bby, bbw, bbh: LONGINT);
  VAR
    TheString: PCHAR;
  BEGIN
    TheString := _al_create_null_str_ (str);
    _al_get_text_dimensions_ (f, TheString, bbx, bby, bbw, bbh);
    _al_dispose_str_ (TheString);
  END;



  PROCEDURE al_init_font_addon; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  PROCEDURE al_shutdown_font_addon; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

  FUNCTION al_get_allegro_font_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

END.
