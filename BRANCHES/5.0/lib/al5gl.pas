UNIT al5gl;
(*         ______   ___    ___
 *        /\  _  \ /\_ \  /\_ \
 *        \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___
 *         \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\
 *          \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \
 *           \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/
 *            \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/
 *                                           /\____/
 *                                           \_/__/
 *
 * Interfaces between Allegro and OpenGL.
 *
 *      See readme.txt for copyright information.
 *)

INTERFACE

  USES
    Allegro5,
    GL;

{$include allegro.cfg}

(******************************************************************************
 * opengl/gl_ext.h *
 ********************)

{ TODO: The original extension system uses the C preprocessor to create the
    structs and constants used.  It's quite complex so I decided to delay it. }



(******************************************************************************
 * allegro_opengl.h *
 ********************)

  TYPE
    ALLEGRO_OPENGL_VARIANT = (
      ALLEGRO_DESKTOP_OPENGL = 0,
      ALLEGRO_OPENGL_ES
    );

  FUNCTION al_get_opengl_version: LONGWORD; CDECL;
  FUNCTION al_have_opengl_extension (CONST extension: STRING): BOOLEAN; CDECL;
  FUNCTION al_get_opengl_proc_address (CONST name: STRING): POINTER;
  FUNCTION al_get_opengl_texture (bitmap: ALLEGRO_BITMAPptr): GLuint; CDECL;
  PROCEDURE al_remove_opengl_fbo (bitmap: ALLEGRO_BITMAPptr); CDECL;
  FUNCTION al_get_opengl_fbo (bitmap: ALLEGRO_BITMAPptr): GLuint; CDECL;
  PROCEDURE al_get_opengl_texture_size (bitmap: ALLEGRO_BITMAPptr; VAR w, h: LONGINT); CDECL;
  PROCEDURE al_get_opengl_texture_position (bitmap: ALLEGRO_BITMAPptr; VAR u, v: LONGINT); CDECL;
  PROCEDURE al_set_current_opengl_context (display: ALLEGRO_DISPLAYptr); CDECL;
  FUNCTION al_get_opengl_variant: LONGINT; CDECL;

IMPLEMENTATION

  FUNCTION al_get_opengl_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_have_opengl_extension (CONST extension: STRING): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_opengl_proc_address (CONST name: STRING): POINTER;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_opengl_texture (bitmap: ALLEGRO_BITMAPptr): GLuint; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_remove_opengl_fbo (bitmap: ALLEGRO_BITMAPptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_opengl_fbo (bitmap: ALLEGRO_BITMAPptr): GLuint; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_opengl_texture_size (bitmap: ALLEGRO_BITMAPptr; VAR w, h: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_get_opengl_texture_position (bitmap: ALLEGRO_BITMAPptr; VAR u, v: LONGINT); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  PROCEDURE al_set_current_opengl_context (display: ALLEGRO_DISPLAYptr); CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

  FUNCTION al_get_opengl_variant: LONGINT; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

END.
