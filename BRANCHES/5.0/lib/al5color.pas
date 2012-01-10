UNIT al5color;
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

  FUNCTION al_get_allegro_color_version: LONGWORD; CDECL;

  PROCEDURE al_color_hsv_to_rgb (h, s, v: SINGLE; VAR r, g, b: SINGLE); CDECL;
  PROCEDURE al_color_rgb_to_hsl (r, g, b: SINGLE; VAR h, s, l: SINGLE); CDECL;
  PROCEDURE al_color_rgb_to_hsv (r, g, b: SINGLE; VAR h, s, v: SINGLE); CDECL;
  PROCEDURE al_color_hsl_to_rgb (h, s, l: SINGLE; VAR r, g, b: SINGLE); CDECL;
  FUNCTION al_color_name_to_rgb (CONST name: PCHAR; VAR r, g, b: SINGLE): BOOLEAN; CDECL;
  FUNCTION al_color_rgb_to_name (r, g, b: SINGLE): PCHAR; CDECL;
  PROCEDURE al_color_cmyk_to_rgb, (c, m, y, k: SINGLE; VAR r, g, b: SINGLE); CDECL;
  PROCEDURE al_color_rgb_to_cmyk (r, g, b: SINGLE; VAR c, m, y, k: SINGLE); CDECL;
  PROCEDURE al_color_yuv_to_rgb, (y, u, v: SINGLE;  VAR r, g, b: SINGLE); CDECL;
  PROCEDURE al_color_rgb_to_yuv (r, g, b: SINGLE; VAR y, u, v: SINGLE); CDECL;
  PROCEDURE al_color_rgb_to_html (r, g, b: SINGLE; str: PCHAR); CDECL;
  PROCEDURE al_color_html_to_rgb, (CONST str: PCHAR; VAR r, g, b: SINGLE); CDECL;
  FUNCTION al_color_yuv (y, u, v: SINGLE): ALLEGRO_COLOR; CDECL;
  FUNCTION al_color_cmyk (c, m, y, k: SINGLE): ALLEGRO_COLOR; CDECL;
  FUNCTION al_color_hsl (h, s, l: SINGLE): ALLEGRO_COLOR; CDECL;
  FUNCTION al_color_hsv (h, s, v: SINGLE): ALLEGRO_COLOR; CDECL;
  FUNCTION al_color_name (CONST name: PCHAR): ALLEGRO_COLOR; CDECL;
  FUNCTION al_color_html (CONST str: PCHAR): ALLEGRO_COLOR; CDECL;

IMPLEMENTATION

  FUNCTION al_get_allegro_color_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_hsv_to_rgb (h, s, v: SINGLE; VAR r, g, b: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_rgb_to_hsl (r, g, b: SINGLE; VAR h, s, l: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_rgb_to_hsv (r, g, b: SINGLE; VAR h, s, v: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_hsl_to_rgb (h, s, l: SINGLE; VAR r, g, b: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  FUNCTION al_color_name_to_rgb (CONST name: PCHAR; VAR r, g, b: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  FUNCTION al_color_rgb_to_name (r, g, b: SINGLE): PCHAR; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_cmyk_to_rgb, (c, m, y, k: SINGLE; VAR r, g, b: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_rgb_to_cmyk (r, g, b: SINGLE; VAR c, m, y, k: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_yuv_to_rgb, (y, u, v: SINGLE;  VAR r, g, b: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_rgb_to_yuv (r, g, b: SINGLE; VAR y, u, v: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_rgb_to_html (r, g, b: SINGLE; str: PCHAR); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_html_to_rgb, (CONST str: PCHAR; VAR r, g, b: SINGLE); CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  FUNCTION al_color_yuv (y, u, v: SINGLE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  FUNCTION al_color_cmyk (c, m, y, k: SINGLE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  FUNCTION al_color_hsl (h, s, l: SINGLE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  FUNCTION al_color_hsv (h, s, v: SINGLE): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  FUNCTION al_color_name (CONST name: PCHAR): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  FUNCTION al_color_html (CONST str: PCHAR): ALLEGRO_COLOR; CDECL;
  EXTERNAL ALLEGRO_COLOR_LIB_NAME;

END.
