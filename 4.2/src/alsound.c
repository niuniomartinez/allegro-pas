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
 *	Sound routines.
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"



/* _get_midi_pos_
 *   Returns the "midi_pos" pointer. */
AL_EXPORT volatile long* _get_midi_pos_ (void)
{
  return &midi_pos;
}

/* _get_midi_time_
 *   Returns the "midi_time_" pointer. */
AL_EXPORT volatile long* _get_midi_time_ (void)
{
  return &midi_time;
}

