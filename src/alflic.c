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
 *	FLIC routines.
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"



/* get_fli_bitmap_
 *   Returns the "fli_bitmap" pointer. */
AL_EXPORT BITMAP* _get_fli_bitmap_ (void)
{
  return fli_bitmap;
}

/* get_fli_palette_
 *   Returns the "fli_palette" pointer. */
AL_EXPORT RGB* _get_fli_palette_ (void)
{
  return &(fli_palette[0]);
}

/* get_fli_bmp_dirty_from_
 *   Returns the "fli_bmp_dirty_from_" pointer. */
AL_EXPORT int* _get_fli_bmp_dirty_from_ (void)
{
  return &fli_bmp_dirty_from;
}

/* get_fli_bmp_dirty_to_
 *   Returns the "fli_bmp_dirty_to_" pointer. */
AL_EXPORT int* _get_fli_bmp_dirty_to_ (void)
{
  return &fli_bmp_dirty_to;
}

/* get_fli_pal_dirty_from_
 *   Returns the "fli_pal_dirty_from_" pointer. */
AL_EXPORT int* _get_fli_pal_dirty_from_ (void)
{
  return &fli_pal_dirty_from;
}

/* get_fli_pal_dirty_to_
 *   Returns the "fli_pal_dirty_to_" pointer. */
AL_EXPORT int* _get_fli_pal_dirty_to_ (void)
{
  return &fli_pal_dirty_to;
}

/* get_fli_timer_
 *   Returns the "fli_timer" pointer. */
AL_EXPORT volatile int* _get_fli_timer_ (void)
{
  return &fli_timer;
}

/* get_fli_frame_
 *   Returns the "fli_frame" pointer. */
AL_EXPORT volatile int* _get_fli_frame_ (void)
{
  return &fli_frame;
}

