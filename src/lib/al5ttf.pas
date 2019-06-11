UNIT al5ttf;
(***<Allows to use TrueType Font format. *)
(* Copyright (c) 2012-2019 Guillermo Martínez J.

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
    Allegro5, al5base, al5font;

  CONST
    ALLEGRO_TTF_NO_KERNING = 1;  {**<@exclude }
    ALLEGRO_TTF_MONOCHROME  = 2; {**<@exclude }
    ALLEGRO_TTF_NO_AUTOHINT = 4; {**<@exclude }

{ Declaration order is different than in the original header to build the
  documentation in correct order. }
  FUNCTION al_init_ttf_addon: AL_BOOL;
    CDECL;EXTERNAL ALLEGRO_TTF_LIB_NAME;
  PROCEDURE al_shutdown_ttf_addon;
    CDECL; EXTERNAL ALLEGRO_TTF_LIB_NAME;
  FUNCTION al_get_allegro_ttf_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_TTF_LIB_NAME;

  FUNCTION al_load_ttf_font (CONST filename: AL_STR; size, flags: AL_INT): ALLEGRO_FONTptr;
    CDECL;EXTERNAL ALLEGRO_TTF_LIB_NAME;
  FUNCTION al_load_ttf_font_f (afile: ALLEGRO_FILEptr; CONST filename: AL_STR; size, flags: AL_INT): ALLEGRO_FONTptr;
    CDECL;EXTERNAL ALLEGRO_TTF_LIB_NAME;
  FUNCTION al_load_ttf_font_stretch (CONST filename: AL_STR; w, h, flags: AL_INT): ALLEGRO_FONTptr;
    CDECL;EXTERNAL ALLEGRO_TTF_LIB_NAME;
  FUNCTION al_load_ttf_font_stretch_f (afile: ALLEGRO_FILEptr; CONST filename: AL_STR; w, h, flags: AL_INT): ALLEGRO_FONTptr;
    CDECL;EXTERNAL ALLEGRO_TTF_LIB_NAME;

IMPLEMENTATION

END.
