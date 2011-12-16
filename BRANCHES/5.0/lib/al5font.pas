UNIT al5font;
(*<The Allegro 5 Font addon. *)
(*TODO: License. *)

{$include allegro.cfg}

INTERFACE

CONST
(* Name of the dynamicly linked unit.

  @bold(TODO:) This should be defined at the @code(allegro.cfg) file as it's different in each platform.
 *)
  ALLEGRO_FONT_LIB_NAME = 'liballegro_font.so.5.0';

TYPE
(* A handle identifying any kind of font.

  Usually you will create it with al_load_font which supports loading all kinds of TrueType fonts supported by the FreeType library. If you instead pass the filename of a bitmap file, it will be loaded with @link(al_load_bitmap) and a font in Allegro's bitmap font format will be created from it with @link(al_grab_font_from_bitmap). *)
  ALLEGRO_FONTptr = POINTER;



(* Loads a font from disk.

  This will use @link(al_load_bitmap_font) if you pass the name of a known bitmap format, or else @link(al_load_ttf_font).
  Bitmap and TTF fonts are affected by the current bitmap flags at the time the font is loaded.

  @seealso(al_destroy_font) @seealso(al_init_font_addon) @seealso(al_register_font_loader)
 *)
  FUNCTION al_load_font (CONST filename: PCHAR; size, flags: INTEGER): ALLEGRO_FONTptr; CDECL;

IMPLEMENTATION

  FUNCTION al_load_font (CONST filename: PCHAR; size, flags: INTEGER): ALLEGRO_FONTptr; CDECL;
  EXTERNAL ALLEGRO_FONT_LIB_NAME;

END.
