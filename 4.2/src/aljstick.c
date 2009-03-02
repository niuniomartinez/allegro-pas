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
 *	Joystick routines.
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"



/* _get_joy_
 *   Returns the "joy" pointer. */
AL_EXPORT JOYSTICK_INFO* _get_joy_ (void)
{
  return &(joy[0]);
}

/* _get_num_joysticks_
 *   Returns the "num_joysticks_" pointer. */
AL_EXPORT int* _get_num_joysticks_ (void)
{
  return &num_joysticks;
}

