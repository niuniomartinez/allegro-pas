UNIT al3d;
(*<Sofware-based 3D engine.

  @bold(3D maths)

  Allegro contains some 3d helper functions for manipulating vectors,
  constructing and using transformation matrices, and doing perspective
  projections from 3d space onto the screen.  It is not, and never will be, a
  fully fledged 3d library (the goal is to supply generic support routines, not
  shrink-wrapped graphics code :-) but these functions may be useful for
  developing your own 3d code.

  Allegro uses a right-handed coordinate system, i.e. if you point the thumb of
  your right hand along the x axis, and the index finger along the y axis, your
  middle finger points in the direction of the z axis.

  Allegro's world coordinate system typically has the positive x axis right,
  the positive y axis up, and the positive z axis out of the screen.  What all
  this means is this:  Assume, the viewer is located at the origin (0/0/0) in
  world space, looks along the negative z axis (0/0/-1), and is oriented so up
  is along the positive y axis (0/1/0).  Then something located at
  (100/200/-300) will be 100 to the right, 200 above, and 300 in front of the
  viewer.  Just like in OpenGL.  (Of course, both OpenGL and Allegro allow to
  use a different system.)

  @bold(See also) @link(al_set_projection_viewport) @link(al_get_camera_matrix)
  @link(al_persp_project)

  All the 3d math functions are available in two versions:  one which uses
  fixed point arithmetic, and another which uses floating point.  The syntax
  for these is identical, but the floating point functions and structures are
  postfixed with @code('_f'), eg. the fixed point function
  @link(al_cross_product) has a floating point equivalent
  @link(al_cross_product_f).

  3d transformations are accomplished by the use of a modelling matrix.  This
  is a 4x4 array of numbers that can be multiplied with a 3d point to produce a
  different 3d point.  By putting the right values into the matrix, it can be
  made to do various operations like translation, rotation, and scaling.  The
  clever bit is that you can multiply two matrices together to produce a third
  matrix, and this will have the same effect on points as applying the original
  two matrices one after the other.  For example, if you have one matrix that
  rotates a point and another that shifts it sideways, you can combine them to
  produce a matrix that will do the rotation and the shift in a single step.
  You can build up extremely complex transformations in this way, while only
  ever having to multiply each point by a single matrix.

  Allegro actually cheats in the way it implements the matrix structure.
  Rotation and scaling of a 3d point can be done with a simple 3x3 matrix, but
  in order to translate it and project it onto the screen, the matrix must be
  extended to 4x4, and the point extended into 4d space by the addition of an
  extra coordinate, w=1.  This is a bad thing in terms of efficiency, but
  fortunately an optimisation is possible. Given the 4x4 matrix:
@longcode(#
   ( a, b, c, d )
   ( e, f, g, h )
   ( i, j, k, l )
   ( m, n, o, p )
#)
  a pattern can be observed in which parts of it do what.  The top left 3x3
  grid implements rotation and scaling.  The three values in the top right
  column (d, h, and l) implement translation, and as long as the matrix is only
  used for affine transformations, m, n and o will always be zero and p will
  always be 1.  If you don't know what affine means, read Foley & Van Damme:
  basically it covers scaling, translation, and rotation, but not projection.
  Since Allegro uses a separate function for projection, the matrix functions
  only need to support affine transformations, which means that there is no
  need to store the bottom row of the matrix.  Allegro implicitly assumes that
  it contains (0,0,0,1), and optimises the matrix manipulation functions
  accordingly.

@bold(Polygon rendering)

  All the 3d functions that accept a `type' parameter are asking for a polygon
  rendering mode, which can be any of the @code(POLYTYPE_* ) values.  If the
  CPU_MMX flag of the cpu_capabilities global variable is set, the GRGB and
  truecolor *LIT routines will be optimised using MMX instructions.  If the
  CPU_3DNOW flag is set, the truecolor PTEX*LIT routines will take advantage of
  the 3DNow! CPU extensions.

  Using MMX for *LIT routines has a side effect:  normally (without MMX), these
  routines use the blender functions used also for other lighting functions,
  set with @link(al_set_trans_blender) or @link(al_set_blender_mode).  The MMX
  versions only use the RGB value passed to @link(al_set_trans_blender) and do
  the linear interpolation themselves.  Therefore a new set of blender
  functions passed to @code(al_set_blender_mode) is ignored.

  @bold(See also) @link(al_polygon3d)

@bold(Zbuffered rendering)

  A Z-buffer stores the depth of each pixel that is drawn on a viewport.  When
  a 3D object is rendered, the depth of each of its pixels is compared against
  the value stored into the Z-buffer:  if the pixel is closer it is drawn,
  otherwise it is skipped.

  No polygon sorting is needed.  However, backface culling should be done
  because it prevents many invisible polygons being compared against the
  Z-buffer.  Z-buffered rendering is the only algorithm supported by Allegro
  that directly solves penetrating shapes (see example exzbuf, for instance).
  The price to pay is more complex (and slower) routines.

  Z-buffered polygons are designed as an extension of the normal
  @code(AL_POLYTYPE_* ) rendering styles.  Just OR the POLYTYPE with the value
  @link(AL_POLYTYPE_ZBUF), and the normal @link(al_polygon3d),
  @link(al_polygon3d_f), @link(al_quad3d), etc. functions will render
  z-buffered polygons.

  Example:
  @longcode(#al_polygon3d (bmp, AL_POLYTYPE_ATEX OR AL_POLYTYPE_ZBUF, tex, vc, vtx);#)
  Of course, the z coordinates have to be valid regardless of rendering style.

  A Z-buffered rendering procedure looks like a double-buffered rendering
  procedure.  You should follow four steps: create a Z-buffer at the beginning
  of the program and make the library use it by calling @link(al_set_zbuffer).
  Then, for each frame, clear the Z-buffer and draw polygons with
  @code(AL_POLYTYPE_* OR AL_POLYTYPE_ZBUF) and finally destroy the Z-buffer
  when leaving the program.

  Notes on Z-buffered renderers:
  @unorderedList(
    @item(Unlike the normal @link(AL_POLYTYPE_FLAT) renderers, the Z-buffered
      ones don't use the @link(al_hline) routine.  Therefore
      @link(AL_DRAW_MODE) has no effect.)
    @item(The @code( *LIT* ) routines work the traditional way - through the set
      of blender routines)
    @item(All the Z-buffered routines are much slower than their normal
      counterparts (they all use the FPU to interpolate and test 1/z values).)
  )

  @bold(See also) @link(al_create_zbuffer)

@bold(Scene rendering)

  Allegro provides another simple approache to remove hidden surfaces:
  Scan-line algorithms.

  Along each scanline on your screen, you keep track of what polygons you are
  "in" and which is the nearest.  This status changes only where the scanline
  crosses some polygon edge.  So you have to juggle an edge list and a polygon
  list.  And you have to sort the edges for each scanline (this can be
  countered by keeping the order of the previous scanline - it won't change
  much).  The @bold(big) advantage is that you write each pixel only once.  If
  you have a lot of overlapping polygons you can get incredible speeds compared
  to any of the previous algorithms.  This algorithm is covered by the
  @code( *_scene) routines. 

  The scene rendering has approximately the following steps:
  @orderedList(
    @item(Initialize the scene (set the clip area, clear the bitmap, blit a
      background, etc.))
    @item(Call @link(al_clear_scene).)
    @item(Transform all your points to camera space.)
    @item(Clip polygons.)
    @item(Project with @link(al_persp_project) or @link(al_persp_project_f).)
    @item("Draw" polygons with @link(al_scene_polygon3d) and/or
      @link(al_scene_polygon3d_f).  This doesn't do any actual drawing, only
      initializes tables.)
    @item(Render all the polygons defined previously to the bitmap with
      @link(al_render_scene).)
    @item(Overlay some non-3D graphics.)
    @item(Show the bitmap (blit it to screen, flip the page, etc).)
  )

  For each horizontal line in the viewport an x-sorted edge list is used to
  keep track of what polygons are "in" and which is the nearest.  Vertical
  coherency is used - the edge list for a scanline is sorted starting from the
  previous one - it won't change much.  The scene rendering routines use the
  same low-level asm routines as normal @link(al_polygon3d).

  Notes on scene rendering:
  @unorderedList(
    @item(Unlike @code(al_polygon3d), @code(al_scene_polygon3d) requires valid
      z coordinates for all vertices, regardless of rendering style @(unlike
      @code(al_polygon3d), which only uses z coordinate for @code ( *PTEX* ).@))
    @item(All polygons passed to @code(al_scene_polygon3d) have to be
      @code(al_persp_project)'ed.)
    @item(After @link(al_render_scene) the mode is reset to @link(AL_SOLID.))
  )

  Using a lot of @code( *MASK* ) polygons drastically reduces performance,
  because when a MASKed polygon is the first in line of sight, the polygons
  underneath have to be drawn too.  The same applies to @code(AL_FLAT) polygons
  drawn with @link(AL_DRAW_MODE_TRANS).

  Z-buffered rendering works also within the scene renderer.  It may be helpful
  when you have a few intersecting polygons, but most of the polygons may be
  safely rendered by the normal scanline sorting algo.  Same as before: just
  @code(OR) the @code(POLYTYPE) with @code(AL_POLYTYPE_ZBUF).  Also, you have
  to clear the z-buffer at the start of the frame.

  @bold(See also) @link(al_create_scene)
 *)

{$IFDEF FPC}
{ Free Pascal. }
 {$MODE FPC}
 {$PACKRECORDS C}
{$ENDIF}

INTERFACE

USES
  albase, allegro, alfixed;



TYPE
(* Pointer to @link(AL_MATRIX). *)
  AL_MATRIXptr = ^AL_MATRIX;
(* Fixed point matrix structure.
  @seealso(al3d) @seealso(AL_MATRIX_F) *)
  AL_MATRIX = RECORD
    v: ARRAY [0..2, 0..2] OF AL_FIXED; (*<Scaling and rotation. *)
    t: ARRAY [0..2] OF AL_FIXED; (*<Translation. *)
  END;

(* Pointer to @link(AL_MATRIX_F).
   @seealso(al3d) @seealso(AL_MATRIX) *)
  AL_MATRIX_Fptr = ^AL_MATRIX_F;
(* Floating point matrix structure. *)
  AL_MATRIX_F = RECORD
    v: ARRAY [0..2, 0..2] OF DOUBLE; (*<Scaling and rotation. *)
    t: ARRAY [0..2] OF DOUBLE; (*<Translation. *)
  END;

VAR
  al_identity_matrix: AL_MATRIX;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'identity_matrix';
  al_identity_matrix_f: AL_MATRIX;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'identity_matrix_f';

  PROCEDURE al_get_translation_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_translation_matrix';
  PROCEDURE al_get_translation_matrix_f (m: AL_MATRIX_Fptr; x, y, z: DOUBLE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_translation_matrix_f';

  PROCEDURE al_get_scaling_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_scaling_matrix';
  PROCEDURE al_get_scaling_matrix_f (m: AL_MATRIX_Fptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_scaling_matrix_f';

  PROCEDURE al_get_x_rotate_matrix (m: AL_MATRIXptr; r: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_x_rotate_matrix';
  PROCEDURE al_get_x_rotate_matrix_f (m: AL_MATRIX_Fptr; r: DOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_x_rotate_matrix_f';

  PROCEDURE al_get_y_rotate_matrix (m: AL_MATRIXptr; r: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_y_rotate_matrix';
  PROCEDURE al_get_y_rotate_matrix_f (m: AL_MATRIX_Fptr; r: DOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_y_rotate_matrix_f';

  PROCEDURE al_get_z_rotate_matrix (m: AL_MATRIXptr; r: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_z_rotate_matrix';
  PROCEDURE al_get_z_rotate_matrix_f (m: AL_MATRIX_Fptr; r: DOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_z_rotate_matrix_f';

  PROCEDURE al_get_rotation_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rotation_matrix';
  PROCEDURE al_get_rotation_matrix_f (m: AL_MATRIX_Fptr; x, y, z: DOUBLE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rotation_matrix_f';

  PROCEDURE al_get_align_matrix (m: AL_MATRIXptr; xfront, yfront, zfront,
				 xup, yup, zup: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_align_matrix';
  PROCEDURE al_get_align_matrix_f (m: AL_MATRIX_Fptr; xfront, yfront, zfront,
				 xup, yup, zup: DOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_align_matrix_f';

  PROCEDURE al_get_vector_rotation_matrix (m: AL_MATRIXptr; x,y,z,a: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME
    'get_vector_rotation_matrix';
  PROCEDURE al_get_vector_rotation_matrix_f (m: AL_MATRIX_Fptr; x,y,z,a: DOUBLE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME
    'get_vector_rotation_matrix_f';

  PROCEDURE al_get_transformation_matrix (m:AL_MATRIXptr;
			scale, xr, yr, zr, x, y, z: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_transformation_matrix';
  PROCEDURE al_get_transformation_matrix_f (m:AL_MATRIX_Fptr;
			scale, xr, yr, zr, x, y, z: DOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_transformation_matrix_f';

  PROCEDURE al_get_camera_matrix (m: AL_MATRIXptr; x, y, z,
		xfront, yfront, zfront, xup, yup, zup, fov, aspect: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_camera_matrix';
  PROCEDURE al_get_camera_matrix_f (m: AL_MATRIX_Fptr; x, y, z,
		xfront, yfront, zfront, xup, yup, zup, fov, aspect: DOUBLE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_camera_matrix_f';

  PROCEDURE al_qtranslate_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_qtranslate_matrix';
  PROCEDURE al_qtranslate_matrix_f (m: AL_MATRIX_Fptr; x, y, z: DOUBLE);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_qtranslate_matrix_f';

  PROCEDURE al_qscale_matrix (m: AL_MATRIXptr; scale: AL_FIXED); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'qscale_matrix';
  PROCEDURE al_qscale_matrix_f (m: AL_MATRIX_Fptr; scale: DOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'qscale_matrix_f';

  PROCEDURE al_matrix_mul (m1, m2, out: AL_MATRIXptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'matrix_mul';
  PROCEDURE al_matrix_mul_f (m1, m2, out: AL_MATRIX_Fptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'matrix_mul_f';

  PROCEDURE al_apply_matrix (m: AL_MATRIXptr;
		x, y, z: AL_FIXED; xout, yout, zout: AL_FIXEDptr);
  PROCEDURE al_apply_matrix_f (m: AL_MATRIX_Fptr;
		x, y, z: DOUBLE; xout, yout, zout: PDOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'apply_matrix_f';


  FUNCTION al_vector_length (x, y, z: AL_FIXED): AL_FIXED; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'vector_length';
  FUNCTION al_vector_length_f (x, y, z: DOUBLE): AL_FIXED; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'vector_length_f';

  PROCEDURE al_normalize_vector (x, y, z: AL_FIXEDptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'normalize_vector';
  PROCEDURE al_normalize_vector_f (x, y, z: DOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'normalize_vector_f';

  PROCEDURE al_cross_product (x1, y1, z1, x2, y2, z2: AL_FIXED;
			      xout, yout, zout: AL_FIXEDptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'cross_product';
  PROCEDURE al_cross_product_f (x1, y1, z1, x2, y2, z2: DOUBLE;
			      xout, yout, zout: PDOUBLE); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'cross_product_f';

  FUNCTION al_dot_product (x1, y1, z1, x2, y2, z2: AL_FIXED): AL_FIXED;
  FUNCTION al_dot_product_f (x1, y1, z1, x2, y2, z2: DOUBLE): DOUBLE;

  PROCEDURE al_set_projection_viewport (x, y, w, h: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_projection_viewport';

  PROCEDURE al_persp_project (x, y, z: AL_FIXED; VAR xout, yout: AL_FIXED);
  PROCEDURE al_persp_project_f (x, y, z: DOUBLE; VAR xout, yout: DOUBLE);



TYPE
(* Pointer to @link(AL_V3D). *)
  AL_V3Dptr = ^AL_V3D;
(* A vertex structure used by @link(al_polygon3d) and other polygon rendering
   functions. @seealso(AL_V3D_F) @seealso(alfixed) *)
  AL_V3D = RECORD
  (* Position. *)
    x, y, z: AL_FIXED;
  (* Texture map coordinates. *)
    u, v: AL_FIXED;
  (* Color. *)
    c: LONGINT;
  END;
(* @ignore *)
  AL_V3D_LIST = ARRAY OF AL_V3Dptr;

(* Pointer to @link(AL_V3D_F). *)
  AL_V3D_Fptr = ^AL_V3D_F;
(*Like @link(AL_V3D) but using float values instead of fixed ones.
  @seealso(al_polygon3d_f) *)
  AL_V3D_F = RECORD
  (* Position. *)
    x, y, z: DOUBLE;
  (* Texture map coordinates. *)
    u, v: DOUBLE;
  (* Color. *)
    c: LONGINT;
  END;
(* @ignore *)
  AL_V3D_LIST_F = ARRAY OF AL_V3D_Fptr;



  FUNCTION al_clip3d (_type: LONGINT; min_z, max_z: AL_FIXED; vc: LONGINT;
    vtx, vout, vtmp: AL_V3D_LIST; out: ARRAY OF LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clip3d';
  FUNCTION al_clip3d_f (_type: LONGINT; min_z, max_z: DOUBLE; vc: LONGINT;
    vtx, vout, vtmp: AL_V3D_LIST_F; out: ARRAY OF LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clip3d_f';

  FUNCTION al_polygon_z_normal (v1, v2, v3: AL_V3Dptr): AL_FIXED; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'polygon_z_normal';
  FUNCTION al_polygon_z_normal_f (v1, v2, v3: AL_V3D_Fptr): DOUBLE; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'polygon_z_normal_f';



CONST
(* A simple flat shaded polygon, taking the color from the `c' value of the
   first vertex.  This polygon type is affected by the @link(al_drawing_mode)
   function, so it can be used to render @code(XOR) or translucent polygons.
   @seealso(al_polygon3d) *)
  AL_POLYTYPE_FLAT            =  0;
(* A single-color gouraud shaded polygon.  The colors for each vertex are taken
   from the `c' value, and interpolated across the polygon.  This is very fast,
   but will only work in 256-color modes if your palette contains a smooth
   gradient between the colors.  In truecolor modes it interprets the color as
   a packed, display-format value as produced by the @link(al_makecol)
  function.
   @seealso(al_polygon3d) *)
  AL_POLYTYPE_GCOL            =  1;
(* A gouraud shaded polygon which interpolates RGB triplets rather than a
   single color.  In 256-color modes this uses the global @link(al_rgb_map)
   table to convert the result to an 8-bit paletted color, so it must only be
   used after you have set up the RGB mapping table!  The colors for each
   vertex are taken from the `c' value, which is interpreted as a 24-bit RGB
   triplet ($FF0000 is red, $00FF00 is green, and $0000FF is blue).
   @seealso(al_polygon3d) *)
  AL_POLYTYPE_GRGB            =  2;
  AL_POLYTYPE_ATEX            =  3;
  AL_POLYTYPE_PTEX            =  4;
  AL_POLYTYPE_ATEX_MASK       =  5;
  AL_POLYTYPE_PTEX_MASK       =  6;
  AL_POLYTYPE_ATEX_LIT        =  7;
  AL_POLYTYPE_PTEX_LIT        =  8;
  AL_POLYTYPE_ATEX_MASK_LIT   =  9;
  AL_POLYTYPE_PTEX_MASK_LIT   = 10;
  AL_POLYTYPE_ATEX_TRANS      = 11;
  AL_POLYTYPE_PTEX_TRANS      = 12;
  AL_POLYTYPE_ATEX_MASK_TRANS = 13;
  AL_POLYTYPE_PTEX_MASK_TRANS = 14;
  AL_POLYTYPE_MAX             = 15;
  AL_POLYTYPE_ZBUF            = 16;


  PROCEDURE al_polygon3d (bmp: AL_BITMAPptr; _type: LONGINT;
			  texture: AL_BITMAPptr; vc: LONGINT;
			  vtx: ARRAY OF AL_V3Dptr);

  PROCEDURE al_polygon3d_f (bmp: AL_BITMAPptr; _type: LONGINT;
			    texture: AL_BITMAPptr; vc: LONGINT;
			    vtx: ARRAY OF AL_V3D_Fptr);

  PROCEDURE al_triangle3d (bmp: AL_BITMAPptr; _type: LONGINT;
			   texture: AL_BITMAPptr; v1, v2, v3: AL_V3Dptr);
  PROCEDURE al_triangle3d_f (bmp: AL_BITMAPptr; _type: LONGINT;
			   texture: AL_BITMAPptr; v1, v2, v3: AL_V3D_Fptr);

  PROCEDURE al_quad3d (bmp: AL_BITMAPptr; _type: LONGINT;
			texture: AL_BITMAPptr; v1, v2, v3, v4: AL_V3Dptr);
  PROCEDURE al_quad3d_f (bmp: AL_BITMAPptr; _type: LONGINT;
			texture: AL_BITMAPptr; v1, v2, v3, v4: AL_V3D_Fptr);

VAR
(* This number (default value = 100.0) controls the behaviour of the z-sorting
   algorithm.  When an edge is very close to another's polygon plane, there is
   an interval of uncertainty in which you cannot tell which object is visible
   (which z is smaller).  This is due to cumulative numerical errors for edges
   that have undergone a lot of transformations and interpolations.

   The default value means that if the 1/z values (in projected space) differ
   by only 1/100 (one percent), they are considered to be equal and the
   x-slopes of the planes are used to find out which plane is getting closer
   when we move to the right.

   Larger values means narrower margins, and increasing the chance of missing
   true adjacent edges/planes.  Smaller values means larger margins, and
   increasing the chance of mistaking close polygons for adjacent ones.  The
   value of 100 is close to the optimum.  However, the optimum shifts slightly
   with resolution, and may be application-dependent.  It is here for you to
   fine-tune.

   @seealso(al_create_scene) @seealso(al_clear_scene)
   @seealso(al_scene_polygon3d) *)
  al_scene_gap: DOUBLE;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scene_gap';



TYPE
(* Pointer to a z-buffer. *)
  AL_ZBUFFERptr = ^AL_ZBUFFER;
(* Structure used by Allegro's 3d zbuffered rendering functions.

   You are not supposed to mix @code(AL_ZBUFFER) with @link(AL_BITMAP) even
   though it is currently possible to do so.  This is just an internal
   representation, and it may change in the future.
   @seealso(al_create_zbuffer) *)
  AL_ZBUFFER = AL_BITMAP;


(* Creates a Z-buffer using the size of the @link(AL_BITMAP) you are planning
   to draw on.  Several Z-buffers can be defined but only one can be used at
   the same time, so you must call @link(al_set_zbuffer) to make this Z-buffer
   active.
   @returns(the pointer to the @link(AL_ZBUFFER) or @nil if there was an error.
     Remember to destroy the @code(AL_ZBUFFER) once you are done with it, to
     avoid having memory leaks.)
   @seealso(al_create_sub_zbuffer) @seealso(al_clear_zbuffer)
   @seealso(al_destroy_zbuffer) *)
  FUNCTION al_create_zbuffer (bmp: AL_BITMAPptr): AL_ZBUFFERptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_zbuffer';

  FUNCTION al_create_sub_zbuffer (parent: AL_ZBUFFERptr;
	x, y, width, height: LONGINT): AL_ZBUFFERptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sub_zbuffer';

  PROCEDURE al_set_zbuffer (zbuf: AL_ZBUFFERptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_zbuffer';

  PROCEDURE al_clear_zbuffer (zbuf: AL_ZBUFFERptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_zbuffer';

  PROCEDURE al_destroy_zbuffer (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sub_zbuffer';


(* Allocates memory for a scene, @code(nedge) and @code(npoly) are your
   estimates of how many edges and how many polygons you will render (you
   cannot get over the limit specified here).  If you use same values in
   succesive calls, the space will be reused (no new @code(GetMem)).

  The memory allocated is a little less than 150 * (nedge + npoly) bytes.
  @returns(@true on success, or @false if allocations fail.)
  @seealso(al_scene_polygon3d) @seealso(al_render_scene)
  @seealso(al_clear_scene) @seealso(al_destroy_scene) @seealso(al_scene_gap)
  @seealso(al_create_zbuffer) *)
  FUNCTION al_create_scene (nedge, npoly: LONGINT): BOOLEAN;

  PROCEDURE al_clear_scene (bmp: AL_BITMAPptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_scene';

  PROCEDURE al_destroy_scene; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_scene';

(* Puts a polygon in the rendering list.  Nothing is really rendered at this
   moment.  Should be called between @link(al_clear_scene) and
   @link(al_render_scene).

   Arguments are the same as for @link(al_polygon3d), except the bitmap is
   missing.  The one passed to @code(al_clear_scene) will be used.

   Unlike @link(al_polygon3d), the polygon may be concave or self-intersecting.
   Shapes that penetrate one another may look OK, but they are not really
   handled by this code.

   Note that the texture is stored as a pointer only, and you should keep the
   actual bitmap around until @code(al_render_scene), where it is used.

   Since the @link(AL_FLAT) style is implemented with the low-level
   @link(al_hline) funtion, the @code(AL_FLAT) style is subject to
   DRAW_MODEs.  All these modes are valid.  Along with the polygon, this mode
   will be stored for the rendering moment, and also all the other related
   variables (color_map pointer, pattern pointer, anchor, blender values).

   The settings of the CPU_MMX and CPU_3DNOW flags of the cpu_capabilities
   global variable on entry in this routine affect the choice of low-level asm
   routine that will be used by @code(al_render_scene) for this polygon.
   @returns(zero on success, or a negative number if it won't be rendered for
    lack of a rendering routine.)
   @seealso(al_create_scene) @seealso(al_clear_scene) @seealso(al_render_scene)
   @seealso(al_polygon3d) *)
  FUNCTION al_scene_polygon3d (_type: LONGINT; texture: AL_BITMAPptr;
			       vtx: ARRAY OF AL_V3Dptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scene_polygon3d';

(* Floating point version of @link(al_scene_polygon3d). *)
  FUNCTION al_scene_polygon3d_f (_type: LONGINT; texture: AL_BITMAPptr;
			         vtx: ARRAY OF AL_V3D_Fptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scene_polygon3d';

(* Renders all the specified @link(al_scene_polygon3d)'s on the bitmap passed
   to @link(al_clear_scene).  Rendering is done one scanline at a time, with no
   pixel being processed more than once.

   Note that between @code(al_clear_scene) and @code(al_render_scene) you
   shouldn't change the clip rectangle of the destination bitmap.  For speed
   reasons, you should set the clip rectangle to the minimum.

   Note also that all the textures passed to @code(al_scene_polygon3d) are
   stored as pointers only and actually used in @code(al_render_scene). *)
  PROCEDURE al_render_scene; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'render_scene';



IMPLEMENTATION




  PROCEDURE al_apply_matrix (m: AL_MATRIXptr;
		x, y, z: AL_FIXED;  xout, yout, zout: AL_FIXEDptr);
  BEGIN
    xout^ := al_fixmul (x, m^.v[0, 0]) +
	     al_fixmul (y, m^.v[0, 1]) +
	     al_fixmul (z, m^.v[0, 2]) +
	     m^.t[0];
    yout^ := al_fixmul (x, m^.v[1, 0]) +
	     al_fixmul (y, m^.v[1, 1]) +
	     al_fixmul (z, m^.v[1, 2]) +
	     m^.t[1];
    zout^ := al_fixmul (x, m^.v[2, 0]) +
	     al_fixmul (y, m^.v[2, 1]) +
	     al_fixmul (z, m^.v[2, 2]) +
	     m^.t[2];
  END;



VAR
  _persp_xscale: AL_FIXED; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  _persp_yscale: AL_FIXED; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  _persp_xoffset: AL_FIXED; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  _persp_yoffset: AL_FIXED; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_persp_project (x, y, z: AL_FIXED; VAR xout, yout: AL_FIXED);
  BEGIN
    xout := al_fixmul (al_fixdiv (x, z), _persp_xscale) + _persp_xoffset;
    yout := al_fixmul (al_fixdiv (y, z), _persp_yscale) + _persp_yoffset;
  END;

VAR
  _persp_xscale_f: DOUBLE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  _persp_yscale_f: DOUBLE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  _persp_xoffset_f: DOUBLE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  _persp_yoffset_f: DOUBLE; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_persp_project_f (x, y, z: DOUBLE; VAR xout, yout: DOUBLE);
  VAR
    z1: DOUBLE;
  BEGIN
    z1 := 1.0 / z;
    xout := ((x * z1) * _persp_xscale_f) + _persp_xoffset_f;
    yout := ((y * z1) * _persp_yscale_f) + _persp_yoffset_f;
  END;



  FUNCTION al_dot_product (x1, y1, z1, x2, y2, z2: AL_FIXED): AL_FIXED;
  BEGIN
    al_dot_product := al_fixmul(x1,x2) + al_fixmul(y1,y2) + al_fixmul(z1,z2);
  END;

  FUNCTION al_dot_product_f (x1, y1, z1, x2, y2, z2: DOUBLE): DOUBLE;
  BEGIN
    al_dot_product_f := (x1 * x2) + (y1 * y2) + (z1 * z2);
  END;



  PROCEDURE al_polygon3d (bmp: AL_BITMAPptr; _type: LONGINT;
			  texture: AL_BITMAPptr; vc: LONGINT;
			  vtx: ARRAY OF AL_V3Dptr);
  BEGIN
    bmp^.vtable^.polygon3d (bmp, _type, texture, vc, @(vtx[0]));
  END;



  PROCEDURE al_polygon3d_f (bmp: AL_BITMAPptr; _type: LONGINT;
			  texture: AL_BITMAPptr; vc: LONGINT;
			  vtx: ARRAY OF AL_V3D_Fptr);
  BEGIN
    bmp^.vtable^.polygon3d (bmp, _type, texture, vc, @(vtx[0]));
  END;


  PROCEDURE al_triangle3d (bmp: AL_BITMAPptr; _type: LONGINT;
			   texture: AL_BITMAPptr; v1, v2, v3: AL_V3Dptr);
  BEGIN
    bmp^.vtable^.triangle3d (bmp, _type, texture, v1, v2, v3);
  END;

  PROCEDURE al_triangle3d_f (bmp: AL_BITMAPptr; _type: LONGINT;
			   texture: AL_BITMAPptr; v1, v2, v3: AL_V3D_Fptr);
  BEGIN
    bmp^.vtable^.triangle3d_f (bmp, _type, texture, v1, v2, v3);
  END;

  PROCEDURE al_quad3d (bmp: AL_BITMAPptr; _type: LONGINT;
			texture: AL_BITMAPptr; v1, v2, v3, v4: AL_V3Dptr);
  BEGIN
    bmp^.vtable^.quad3d (bmp, _type, texture, v1, v2, v3, v4);
  END;

  PROCEDURE al_quad3d_f (bmp: AL_BITMAPptr; _type: LONGINT;
			texture: AL_BITMAPptr; v1, v2, v3, v4: AL_V3D_Fptr);
  BEGIN
    bmp^.vtable^.quad3d_f (bmp, _type, texture, v1, v2, v3, v4);
  END;



  FUNCTION create_scene (nedge, npoly: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_create_scene (nedge, npoly: LONGINT): BOOLEAN;
  BEGIN
    al_create_scene := create_scene (nedge, npoly) = 0;
  END;

END.
