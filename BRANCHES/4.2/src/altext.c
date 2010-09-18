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
 *	
 *	Text drawing.
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"



/* get_font:
 *   Returns the "font" pointer. */
AL_EXPORT FONT** _get_font_ (void)
{
  return &font;
}



/* _get_allegro_404_char_:
 *   Returns the "allegro_404_char" pointer. */
AL_EXPORT int* _get_allegro_404_char_ (void)
{
  return &allegro_404_char;
}

