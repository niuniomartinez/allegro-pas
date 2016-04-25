UNIT al5ttf;
(*<This unit registers TrueType Font format handlers for @link(al_load_font). *)
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
    al5base;

  CONST
  (* Builds library name. *)
    { @exclude }
    ALLEGRO_TTF_LIB_NAME = _A5_LIB_PREFIX_+'allegro_ttf'+_DBG_+_A5_LIB_VER_+_A5_LIB_EXT_;

  (* Do not use any kerning even if the font file supports it.
    @seealso(al_load_font) @seealso(al_load_ttf_font) *)
    ALLEGRO_TTF_NO_KERNING = 1;
  (* Loads as monochrome, which means no anti-aliasing of the font is done.
    @seealso(al_load_font) @seealso(al_load_ttf_font) *)
    ALLEGRO_TTF_MONOCHROME  = 2;
  (* Do not use hinging (?).
    @seealso(al_load_font) @seealso(al_load_ttf_font) *)
    ALLEGRO_TTF_NO_AUTOHINT = 4;

  FUNCTION al_load_ttf_font (filename: AL_STR; size, flags: AL_INT);
    CDECL;EXTERNAL ALLEGRO_TTF_LIB_NAME;
  FUNCTION al_load_ttf_font_stretch (filename: AL_STR; w, h, flags: AL_INT);
    CDECL;EXTERNAL ALLEGRO_TTF_LIB_NAME;
(* Initializes the TTF addon. *)
  FUNCTION al_init_ttf_addon: AL_BOOL;
    CDECL;EXTERNAL ALLEGRO_TTF_LIB_NAME;
(* Shuts down the TTF addon. This is done automatically at program exit, but
   can be called any time the user wishes as well. *)
  PROCEDURE al_shutdown_ttf_addon;
    CDECL; EXTERNAL ALLEGRO_TTF_LIB_NAME;
(* Returns the (compiled) version of the addon, in the same format as
   @link(al_get_allegro_version). *)
  FUNCTION al_get_allegro_ttf_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_TTF_LIB_NAME;

IMPLEMENTATION

END.
