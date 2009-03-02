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
 *	System level: initialization, cleanup, etc.
 *	by Ñuño Martínez <>
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"



/* get_AL_ID:
 *   The "AL_IDE" is a macro/inline function, so a wrapper is needed. */
AL_EXPORT int get_AL_ID (int a, int b, int c, int d)
{
  return AL_ID (a, b, c, d);
}



/* al_install:
 *   The "install_allegro" is a macro/inline function, so a wrapper is
 *   needed. */
AL_EXPORT int al_install (int system_id, int *errno_ptr,
		AL_METHOD(int, atexit_ptr, (AL_METHOD(void, func, (void)))))
{
  set_uformat (U_ASCII);
  return install_allegro (system_id, errno_ptr, atexit_ptr);
}



/* al_init:
 *   The "allegro_init" is a macro/inline function, so a wrapper is needed. */
AL_EXPORT int al_init (void)
{
  set_uformat (U_ASCII);
  return allegro_init ();
}



/* _get_al_errno_:
 *   Returns the "allegro_errno" pointer. */
AL_EXPORT int* _get_al_errno_ (void)
{
  return allegro_errno;
}



/* _get_al_error_:
 *   Returns the "allegro_error" pointer. */
AL_EXPORT char* _get_al_error_ (void)
{
  return allegro_error;
}



/* _get_al_id_string_:
 *   Returns the "allegro_id" pointer. */
AL_EXPORT char* _get_al_id_string_ (void)
{
  return allegro_id;
}



/* _get_*_palette_:
 *   Returns the palette pointer. */
AL_EXPORT RGB* _get_black_palette_   (void) { return &(black_palette[0]); }
AL_EXPORT RGB* _get_default_palette_ (void) { return &(default_palette[0]); }
AL_EXPORT RGB* _get_desktop_palette_ (void) { return &(desktop_palette[0]); }



/* al_desktop_color_depth:
 *   Finds out the currently selected desktop color depth. */
AL_EXPORT int al_desktop_color_depth (void)
{
  return desktop_color_depth ();
}



/* al_get_desktop_resolution:
 *   Finds out the currently selected desktop resolution. */
AL_EXPORT void al_get_desktop_resolution (int *w, int *h)
{
  get_desktop_resolution (w, h);
}



/* al_set_window_title:
 *   Alters the window title for your Allegro program. */
AL_EXPORT void al_set_window_title (const char *name)
{
  set_window_title (name);
}

