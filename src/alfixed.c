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
 *	Fixed type routines.
 *
 *	See readme.txt for license and copyright information.
 */



#include "base.h"



/* _get_cos_tbl_
 *   Returns the "_cos_tbl" pointer. */
AL_EXPORT void* _get_cos_tbl_ (void)
{
  return _cos_tbl;
}

/* _get_tan_tbl_
 *   Returns the "_tan_tbl" pointer. */
AL_EXPORT void* _get_tan_tbl_ (void)
{
  return _tan_tbl;
}

/* _get_acos_tbl_
 *   Returns the "_acos_tbl" pointer. */
AL_EXPORT void* _get_acos_tbl_ (void)
{
  return _acos_tbl;
}

