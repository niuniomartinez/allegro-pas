unit al5memfile;
(***<Defines the @italic(memfile interface).

     The memfile interface allows you to treat a fixed block of contiguous
     memory as a file that can be used with Allegro's I/O functions. *)
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
    allegro5, al5base;


  function al_open_memfile (mem: AL_POINTER; size: AL_INT64; const mode: AL_STR): ALLEGRO_FILEptr;
    CDECL; external ALLEGRO_MEMFILE_LIB_NAME;
  function al_get_allegro_memfile_version: AL_UINT64;
    CDECL; external ALLEGRO_MEMFILE_LIB_NAME;

implementation

end.
