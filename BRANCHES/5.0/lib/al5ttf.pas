UNIT al5ttf;
(*<This unit registers TrueType Font format handlers for @link(al_load_font). *)
(*TODO: License. *)

{$include allegro.cfg}

INTERFACE

{$include allegro.inc}

  CONST
  (* Do not use any kerning even if the font file supports it.
    @seealso(al_load_font) @seealso(al_load_ttf_font) *)
    ALLEGRO_TTF_NO_KERNING = 1;
  (* Loads as monochrome, which means no anti-aliasing of the font is done.
    @seealso(al_load_font) @seealso(al_load_ttf_font) *)
    ALLEGRO_TTF_MONOCHROME  = 2;
  (* Do not use hinging (?).
    @seealso(al_load_font) @seealso(al_load_ttf_font) *)
    ALLEGRO_TTF_NO_AUTOHINT = 4;

(* Initializes the TTF addon. *)
  FUNCTION al_init_ttf_addon: BYTEBOOL; CDECL;

(* Shut down the TTF addon. This is done automatically at program exit, but can be called any time the user wishes as well. *)
  PROCEDURE al_shutdown_ttf_addon; CDECL;

(* Returns the (compiled) version of the addon, in the same format as @link(al_get_allegro_version). *)
  FUNCTION al_get_allegro_ttf_version: LONGWORD; CDECL;

IMPLEMENTATION

  FUNCTION al_init_ttf_addon: BYTEBOOL; CDECL;
  EXTERNAL ALLEGRO_TTF_LIB_NAME;

  PROCEDURE al_shutdown_ttf_addon; CDECL;
  EXTERNAL ALLEGRO_TTF_LIB_NAME;

  FUNCTION al_get_allegro_ttf_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_TTF_LIB_NAME;

END.
