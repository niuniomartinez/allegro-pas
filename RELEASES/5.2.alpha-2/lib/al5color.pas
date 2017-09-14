UNIT al5color;
(*<Color management. *)
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
  (* @exclude Builds library name. *)
    ALLEGRO_COLOR_LIB_NAME = _A5_LIB_PREFIX_+'allegro_color'+_DBG_+_A5_LIB_EXT_;

  FUNCTION al_get_allegro_color_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;

  PROCEDURE al_color_hsv_to_rgb (h, s, v: AL_FLOAT; OUT r, g, b: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  PROCEDURE al_color_rgb_to_hsl (r, g, b: AL_FLOAT; OUT h, s, l: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  PROCEDURE al_color_rgb_to_hsv (r, g, b: AL_FLOAT; OUT h, s, v: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  PROCEDURE al_color_hsl_to_rgb (h, s, l: AL_FLOAT; OUT r, g, b: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_name_to_rgb (CONST name: AL_STR; OUT r, g, b: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_rgb_to_name (r, g, b: AL_FLOAT): AL_STRptr;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  PROCEDURE al_color_cmyk_to_rgb (c, m, y, k: AL_FLOAT; OUT r, g, b: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  PROCEDURE al_color_rgb_to_cmyk (r, g, b: AL_FLOAT; OUT c, m, y, k: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  PROCEDURE al_color_yuv_to_rgb (y, u, v: AL_FLOAT;  OUT r, g, b: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  PROCEDURE al_color_rgb_to_yuv (r, g, b: AL_FLOAT; OUT y, u, v: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_rgb_to_html (r, g, b: AL_FLOAT): AL_STRptr; INLINE;
  PROCEDURE al_color_html_to_rgb (CONST str: AL_STR; OUT r, g, b: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_yuv (y, u, v: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_cmyk (c, m, y, k: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_hsl (h, s, l: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_hsv (h, s, v: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_name (CONST name: AL_STR): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;
  FUNCTION al_color_html (CONST str: al_STR): ALLEGRO_COLOR;
    CDECL; EXTERNAL ALLEGRO_COLOR_LIB_NAME;

IMPLEMENTATION

  PROCEDURE _al_color_rgb_to_html_ (r, g, b: AL_FLOAT; str: AL_STRptr); CDECL;
    EXTERNAL ALLEGRO_COLOR_LIB_NAME NAME 'al_color_rgb_to_html';

  FUNCTION al_color_rgb_to_html (r, g, b: AL_FLOAT): AL_STRptr;
  BEGIN
    RESULT := '      ';
    _al_color_rgb_to_html_ (r, g, b, AL_STRptr (RESULT));
  END;

END.
