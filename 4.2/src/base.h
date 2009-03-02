/*
  ______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\ 
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	Define some basic configuration and conventions.
 *
 *	by Ñuño Martínez.
 *
 *	See readme.txt for license and copyright information.
 */

#ifndef _Allegro_pas_BASE_H_
#define _Allegro_pas_BASE_H_



/* All need Allegro. */
#include <allegro.h>



/***********************************
 *  Platform dependent definitions *
 ***********************************/

/* Win32 definitions. */
#ifdef ALLEGRO_WINDOWS
  #ifdef ALLEGRO_MINGW32
    #define AL_EXPORT __declspec(dllexport)
  #endif /* ALLEGRO_MINGW32 */
#endif /* ALLEGRO_WINDOWS */



/* Check calling conventions. */
#ifndef AL_EXPORT
#define AL_EXPORT
#endif



#endif /* _Allegro_pas_BASE_H_ */

