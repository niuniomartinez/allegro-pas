UNIT al5codec;

INTERFACE

{$include allegro.cfg}

  FUNCTION al_init_acodec_addon: BOOLEAN; CDECL;
  FUNCTION al_get_allegro_acodec_version: LONGWORD; CDECL;

IMPLEMENTATION

  FUNCTION al_init_acodec_addon: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_CODEC_LIB_NAME;

  FUNCTION al_get_allegro_acodec_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_CODEC_LIB_NAME;

END.
