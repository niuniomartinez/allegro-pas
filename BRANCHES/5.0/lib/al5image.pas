UNIT al5image;
(*<This unit registers bitmap format handlers for @link(al_load_bitmap), @link(al_load_bitmap_f), @link(al_save_bitmap), @link(al_save_bitmap_f).

   The following types are built into the Allegro image addon and guaranteed to be available: BMP, PCX, TGA. Every platform also supports JPEG and PNG via external dependencies.

   Other formats may be available depending on the operating system and installed libraries, but are not guaranteed and should not be assumed to be universally available. *)
(*TODO: License. *)

INTERFACE

{$include allegro.cfg}

(* Initializes the image addon. *)
  FUNCTION al_init_image_addon: BOOLEAN; CDECL;

(* Shut down the image addon. This is done automatically at program exit, but can be called any time the user wishes as well. *)
  PROCEDURE al_shutdown_image_addon; CDECL;

(* Returns the (compiled) version of the addon, in the same format as @link(al_get_allegro_version). *)
  FUNCTION al_get_allegro_image_version: LONGWORD; CDECL;

IMPLEMENTATION

  FUNCTION al_init_image_addon: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_IMAGE_LIB_NAME;

  PROCEDURE al_shutdown_image_addon; CDECL;
  EXTERNAL ALLEGRO_IMAGE_LIB_NAME;

  FUNCTION al_get_allegro_image_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_IMAGE_LIB_NAME;

END.
