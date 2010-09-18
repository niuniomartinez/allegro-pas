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
 *	Graphic level: initialization, cleanup, etc.
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"

/* get_gfx_capabilities_:
 *   Returns the "gfx_capabilities" pointer. */
AL_EXPORT int* _get_gfx_capabilities_ (void)
{
  return &gfx_capabilities;
}



/* get_screen_:
 *   Returns the "screen" bitmap pointer. */
AL_EXPORT BITMAP* _get_screen_ (void)
{
  return screen;
}

