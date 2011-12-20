UNIT al5gl;
(*<Interfaces between Allegro and OpenGL. *)

INTERFACE

  USES
    Allegro5,
    GL;

{$include allegro.cfg}

(* Returns the OpenGL texture id internally used by the given bitmap if it uses one, else 0.

   Example:
@longcode(#
Bitmap := al_load_bitmap ('my_texture.png');
Texture := al_get_opengl_texture (Bitmap);
IF Texture <> 0 THEN
  glBindTexture (GL_TEXTURE_2D, Texture);
#)
 *)
  FUNCTION al_get_opengl_texture (bitmap: ALLEGRO_BITMAPptr): GLuint; CDECL;

IMPLEMENTATION

  FUNCTION al_get_opengl_texture (bitmap: ALLEGRO_BITMAPptr): GLuint; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME;

END.
