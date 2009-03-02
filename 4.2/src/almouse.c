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
 *	Mouse support.
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"

/* _get_mouse_x_
 *   Returns the pointer to "mouse_x". */
AL_EXPORT volatile int* _get_mouse_x_ (void)
{
  return &mouse_x;
}

/* _get_mouse_y_
 *   Returns the pointer to "mouse_y". */
AL_EXPORT volatile int* _get_mouse_y_ (void)
{
  return &mouse_y;
}

/* _get_mouse_z_
 *   Returns the pointer to "mouse_z". */
AL_EXPORT volatile int* _get_mouse_z_ (void)
{
  return &mouse_z;
}

/* _get_mouse_b_
 *   Returns the pointer to "mouse_b". */
AL_EXPORT volatile int* _get_mouse_b_ (void)
{
  return &mouse_b;
}

/* _get_mouse_pos_
 *   Returns the pointer to "mouse_pos". */
AL_EXPORT volatile int* _get_mouse_pos_ (void)
{
  return &mouse_pos;
}

/* _get_mouse_sprite_
 *   Returns the pointer to "mouse_sprite". */
AL_EXPORT BITMAP* _get_mouse_sprite_ (void)
{
  return mouse_sprite;
}

/* _get_mouse_x_focus_
 *   Returns the pointer to "mouse_x_focus". */
AL_EXPORT volatile int* _get_mouse_x_focus_ (void)
{
  return &mouse_x_focus;
}

/* _get_mouse_y_focus_
 *   Returns the pointer to "mouse_y_focus". */
AL_EXPORT volatile int* _get_mouse_y_focus_ (void)
{
  return &mouse_y_focus;
}

