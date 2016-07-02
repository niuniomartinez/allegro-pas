UNIT al5opengl;
(*<OpenGL integration.

  You can disable the detection of any OpenGL extension by Allegro with a
  section like this in allegro5.cfg:
@longcode(#
[opengl_disabled_extensions]
GL_ARB_texture_non_power_of_two=0
GL_EXT_framebuffer_object=0
#)
  Any extension which appears in the section is treated as not available (it
  does not matter if you set it to 0 or any other value). *)
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

(* Returns the OpenGL or OpenGL ES version number of the client (the computer
   the program is running on), for the current display. "1.0" is returned as
   @code($01000000), "1.2.1" is returned as @code($01020100), and "1.2.2" as
   @code($01020200) ,etc.

   A valid OpenGL context must exist for this function to work, which means you
   may not call it before @link(al_create_display).
   @seealso(al_get_opengl_variant) *)
  FUNCTION al_get_opengl_version: AL_UINT32;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* This function is a helper to determine whether an OpenGL extension is
   available on the given display or not.

   @bold(Example)
@longcode(#
packedpixels := al_have_opengl_extension ('GL_EXT_packed_pixels');
#)
   If @italic(packedpixels) is @true then you can safely use the constants
   related to the packed pixels extension.
  @return(@true if the extension is available; @false otherwise.)
  @seealso(al_get_opengl_proc_address) *)
  FUNCTION al_have_opengl_extension (CONST extension: AL_STR): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Helper to get the address of an OpenGL symbol.

  @bold(Example)

  How to get the function @code(glMultiTexCoord3fARB) that comes with ARB's
  Multitexture extension:
@longcode(#
  TYPE
  { define the type of the function. }
    PROCEDURE MULTI_TEX_FUNC (a: GLenum; b, c, d: GLfloat); CDECL;
  VAR
  { declare the function pointer }
    glMultiTexCoord3fARB: MULTI_TEX_FUNC;
  BEGIN
  { get the address of the function }
    glMultiTexCoord3fARB := MULTI_TEX_FUNC (al_get_opengl_proc_address ('glMultiTexCoord3fARB'));
  END;
#)
  If @code(glMultiTexCoord3fARB) is not @nil then it can be used as if it has
  been defined in the OpenGL core library.
  @param(name The name of the symbol you want to link to.)
  @return(A pointer to the symbol if available or @nil otherwise.)
  @seealso(al_have_opengl_extension) *)
  FUNCTION al_get_opengl_proc_address (CONST name: AL_STR): AL_VOIDptr;
    CDECL; EXTERNAL ALLEGRO_LIB_NAME;
(* Returns the OpenGL texture id internally used by the given bitmap if it uses
   one, else @code(0).

  @bold(Example)
@longcode(#
  Bitmap := al_load_bitmap ('my_texture.png');
  Texture := al_get_opengl_texture (Bitmap);
  IF texture <> NIL THEN glBindTexture (GL_TEXTURE_2D, Texture);
#) *)
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
