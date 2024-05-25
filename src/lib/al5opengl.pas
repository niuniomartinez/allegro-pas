unit al5opengl;
(***<OpenGL integration.

  You can disable the detection of any OpenGL extension by Allegro with a
  section like this in allegro5.cfg:
@preformatted(
[opengl_disabled_extensions]
  GL_ARB_texture_non_power_of_two=0
  GL_EXT_framebuffer_object=0
)
  Any extension which appears in the section is treated as not available (it
  does not matter if you set it to 0 or any other value).
 *)
(* Copyright (c) 2012-2024 Guillermo Martínez J.

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

{$INCLUDE allegro5.cfg}

interface

  uses
    Allegro5, al5base,
{$IFDEF FPC}
    GL;
{$ELSE}
    OpenGL;
{$ENDIF}

(******************************************************************************
 * opengl/gl_ext.h
 *)

{ The extension system uses the C preprocessor to create the structs and
  constants used.  It's quite complex so I decided to not include it.
}



(******************************************************************************
 * allegro_opengl.h
 *      Main header file for all OpenGL drivers.
 *
 *      By Milan Mimica.
 *)

  type
  {** @exclude }
    ALLEGRO_OPENGL_VARIANT = (
      ALLEGRO_DESKTOP_OPENGL = 0,
      ALLEGRO_OPENGL_ES
    );

  function al_get_opengl_version: AL_UINT32;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_have_opengl_extension (const extension: AL_STR): AL_BOOL;
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_opengl_proc_address (const name: AL_STR): AL_VOIDptr;
    CDECL; external ALLEGRO_LIB_NAME;
{ ALLEGRO_OGL_EXT_LIST is defined using a pre-processor.  The result is a HUGE
  (and I mean HUGE as in HUGE) record with thousands (ok, may be just hundreds)
  of fields.  That's why this function isn't defined.
AL_FUNC(ALLEGRO_OGL_EXT_LIST*, al_get_opengl_extension_list,     (void));
}
  function al_get_opengl_texture (bitmap: ALLEGRO_BITMAPptr): GLuint;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_remove_opengl_fbo (bitmap: ALLEGRO_BITMAPptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_opengl_fbo (bitmap: ALLEGRO_BITMAPptr): GLuint;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_get_opengl_texture_size (bitmap: ALLEGRO_BITMAPptr; out w, h: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_get_opengl_texture_position (bitmap: ALLEGRO_BITMAPptr; out u, v: AL_INT);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_opengl_program_object (shader: ALLEGRO_SHADERptr): GLuint;
    CDECL; external ALLEGRO_LIB_NAME;
  procedure al_set_current_opengl_context (display: ALLEGRO_DISPLAYptr);
    CDECL; external ALLEGRO_LIB_NAME;
  function al_get_opengl_variant: ALLEGRO_OPENGL_VARIANT;
    CDECL; external ALLEGRO_LIB_NAME;

implementation

end.
