UNIT al5codec;
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

{$include allegro.cfg}

  FUNCTION al_init_acodec_addon: BOOLEAN; CDECL;
  FUNCTION al_get_allegro_acodec_version: LONGWORD; CDECL;

IMPLEMENTATION

  FUNCTION al_init_acodec_addon: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_CODEC_LIB_NAME;

  FUNCTION al_get_allegro_acodec_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_CODEC_LIB_NAME;

END.
