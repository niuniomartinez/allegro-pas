unit al5acodec;
(***<This unit registers audio codec handlers for @link(al_load_sample),
     @link(al_save_sample), @link(al_load_audio_stream), etc.
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

  function al_init_acodec_addon: AL_BOOL;
    CDECL; external ALLEGRO_ACODEC_LIB_NAME;
  function al_get_allegro_acodec_version: AL_UINT32;
    CDECL; external ALLEGRO_ACODEC_LIB_NAME;

implementation

end.
