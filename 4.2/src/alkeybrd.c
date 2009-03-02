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
 *	Keyboard support.
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"

/* _get_key_:
 *   Returns the pointer to "key". */
AL_EXPORT volatile char* _get_key_ (void)
{
  return &(key[0]);
}



/* _get_key_shifts_:
 *   Returns the pointer to "key_shifts_". */
AL_EXPORT volatile int* _get_key_shifts_ (void)
{
  return &key_shifts;
}



/* _get_three_finger_flag_:
 *   Returns the pointer to "three_finger_flag". */
AL_EXPORT int* _get_three_finger_flag_ (void)
{
  return &three_finger_flag;
}



/* _get_key_led_flag_:
 *   Returns the pointer to "key_led_flag". */
AL_EXPORT int* _get_key_led_flag_ (void)
{
  return &key_led_flag;
}

