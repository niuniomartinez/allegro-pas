UNIT al5gl;
(*<public OpenGL-related API. *)
(* Copyright (c) 2012-2016 Guillermo Martínez J.

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

INTERFACE

  USES
    Allegro5, al5base,
{$IFDEF FPC}
    GL;
{$ELSE}
    OpenGL;
{$ENDIF}

(******************************************************************************
 * opengl/gl_ext.h
 *)

{ TODO: The extension system uses the C preprocessor to create the structs and
        constants used.  It's quite complex so I decided to delay it. }



(******************************************************************************
 * allegro_opengl.h
 *      Main header file for all OpenGL drivers.
 *
 *      By Milan Mimica.
 *)

  TYPE
    ALLEGRO_OPENGL_VARIANT = (
      ALLEGRO_DESKTOP_OPENGL = 0,
      ALLEGRO_OPENGL_ES
    );

  FUNCTION al_get_opengl_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_have_opengl_extension (CONST extension: AL_STR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_opengl_proc_address (CONST name: AL_STR): AL_VOIDptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_opengl_texture (bitmap: ALLEGRO_BITMAPptr): GLuint;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_remove_opengl_fbo (bitmap: ALLEGRO_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_opengl_fbo (bitmap: ALLEGRO_BITMAPptr): GLuint;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_opengl_texture_size (bitmap: ALLEGRO_BITMAPptr; OUT w, h: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  PROCEDURE al_get_opengl_texture_position (bitmap: ALLEGRO_BITMAPptr; OUT u, v: AL_INT);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
{ TODO: al_get_opengl_program_object, when shaders are implemented. }
  PROCEDURE al_set_current_opengl_context (display: ALLEGRO_DISPLAYptr);
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
  FUNCTION al_get_opengl_variant: AL_INT;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;

IMPLEMENTATION

END.
