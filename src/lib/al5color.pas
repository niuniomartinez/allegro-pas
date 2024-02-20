unit al5color;
(***<Color management.

  @include(../docs/al5color.pds) *)
(* Copyright (c) 2012-2022 Guillermo Martínez J.

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

interface

  uses
    Allegro5, al5base;

  function al_get_allegro_color_version: AL_UINT32;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;

  procedure al_color_hsv_to_rgb (h, s, v: AL_FLOAT; out r, g, b: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_hsl (r, g, b: AL_FLOAT; out h, s, l: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_hsv (r, g, b: AL_FLOAT; out h, s, v: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_hsl_to_rgb (h, s, l: AL_FLOAT; out r, g, b: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_name_to_rgb (const name: AL_STR; out r, g, b: AL_FLOAT): AL_BOOL;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_rgb_to_name (r, g, b: AL_FLOAT): AL_STRptr;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_cmyk_to_rgb (c, m, y, k: AL_FLOAT; out r, g, b: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_cmyk (r, g, b: AL_FLOAT; out c, m, y, k: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_yuv_to_rgb (y, u, v: AL_FLOAT;  out r, g, b: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_yuv (r, g, b: AL_FLOAT; out y, u, v: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_rgb_to_html (r, g, b: AL_FLOAT): AL_STRptr; inline;
  procedure al_color_html_to_rgb (const str: AL_STR; out r, g, b: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_yuv (y, u, v: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_cmyk (c, m, y, k: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_hsl (h, s, l: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_hsv (h, s, v: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_name (const name: AL_STR): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_html (const str: al_STR): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_xyz_to_rgb (x, y, z: AL_FLOAT; out red, green, blue: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_xyz (red, green, blue: AL_FLOAT; out x, y, z: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_xyz (x, y, z:AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_lab_to_rgb (l, a, b: AL_FLOAT; out red, green, blue: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_lab (red, green, blue: AL_FLOAT; out l, a, b: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_lab (l, a, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_xyy_to_rgb (x, y, y2: AL_FLOAT; out red, green, blue: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_xyy (red, green, blue: AL_FLOAT; out x, y, y2: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_xyy (x, y, y2: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_distance_ciede2000 (c1, c2: ALLEGRO_COLOR): AL_DOUBLE;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_lch_to_rgb (l, c, h: AL_FLOAT; out red, green, blue);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_lch (red, green, blue: AL_FLOAT; out l, c, h: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_lch (l, c, h: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_is_color_valid (color: ALLEGRO_COLOR): AL_BOOL;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_oklab_to_rgb (l, a, b: AL_FLOAT; out red, green, blue: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_oklab (red, green, blue: AL_FLOAT; out l, a, b: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_oklab (l, a, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_linear_to_rgb (x, y, z: AL_FLOAT; out red, green, blue: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  procedure al_color_rgb_to_linear (red, green, blue: AL_FLOAT; out x, y, z: AL_FLOAT);
    CDECL; external ALLEGRO_COLOR_LIB_NAME;
  function al_color_linear (r, g, b: AL_FLOAT): ALLEGRO_COLOR;
    CDECL; external ALLEGRO_COLOR_LIB_NAME;


{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.  In
  delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here. }
{**@exclude}
  procedure _al_color_rgb_to_html_ (r, g, b: AL_FLOAT; str: AL_STRptr); CDECL;
    external ALLEGRO_COLOR_LIB_NAME NAME 'al_color_rgb_to_html';

implementation

  function al_color_rgb_to_html (r, g, b: AL_FLOAT): AL_STRptr;
  begin
    Result := '      ';
    _al_color_rgb_to_html_ (r, g, b, AL_STRptr (Result))
  end;

end.
