UNIT al5acodec;
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

{$include allegro.cfg}

INTERFACE

{$include allegro.inc}

  FUNCTION al_init_acodec_addon: BYTEBOOL; CDECL;
  FUNCTION al_get_allegro_acodec_version: LONGWORD; CDECL;

IMPLEMENTATION

  FUNCTION al_init_acodec_addon: BYTEBOOL; CDECL;
  EXTERNAL ALLEGRO_ACODEC_LIB_NAME;

  FUNCTION al_get_allegro_acodec_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_ACODEC_LIB_NAME;

END.
