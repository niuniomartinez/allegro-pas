unit al5image;
(***<This unit registers bitmap format handlers for @link(al_load_bitmap),
   @link(al_save_bitmap), etc.

   Some of the format handlers define configuration options for specifying
   things like compression level or gamma handling. Refer to
   @link(al_get_system_config) for their documentation.
 *)
(* Copyright (c) 2012-2022 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{$include allegro5.cfg}

interface

  uses
    al5base;

  function al_init_image_addon: AL_BOOL;
    CDECL; external ALLEGRO_IMAGE_LIB_NAME;
  function al_is_image_addon_initialized: AL_BOOL;
    CDECL; external ALLEGRO_IMAGE_LIB_NAME;
  procedure al_shutdown_image_addon;
    CDECL; external ALLEGRO_IMAGE_LIB_NAME;

  function al_get_allegro_image_version: AL_UINT32;
    CDECL; external ALLEGRO_IMAGE_LIB_NAME;

implementation

end.
