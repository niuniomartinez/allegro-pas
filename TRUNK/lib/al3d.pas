UNIT al3d;
(*<Sofware-based 3D routines.

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
  the positive y axis up, and the positive z axis in to the screen.  What all
  this means is this:  Assume, the viewer is located at the origin (0/0/0) in
  world space, looks along the negative z axis (0/0/1), and is oriented so up
  is along the positive y axis (0/1/0).  Then something located at
  (100/200/300) will be 100 to the right, 200 above, and 300 in front of the
  viewer.  Just like in OpenGL.  (Of course, both OpenGL and Allegro allow to
  use a different system.)

  @bold(See also) @link(al_set_projection_viewport) @link(al_get_camera_matrix)
  @link(al_persp_project)

  All the 3d math functions are available in two versions:  one which uses
  fixed point arithmetic, and another which uses floating point.  The syntax
  for these is identical, but the floating point functions and structures are
  postfixed with @code('_f'), eg. the fixed point function
  @code(al_cross_product) has a floating point equivalent
  @code(al_cross_product_f).

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

  All the 3d functions that accept a @code(type) parameter are asking for a
  polygon rendering mode, which can be any of the @code(POLYTYPE_* ) values.
  If the AL_CPU_MMX flag of the @link(al_cpu_capabilities) global variable is
  set, the GRGB and truecolor *LIT routines will be optimised using MMX
  instructions.  If the AL_CPU_3DNOW flag is set, the truecolor PTEX*LIT
  routines will take advantage of the 3DNow! CPU extensions.

  Using MMX for *LIT routines has a side effect:  normally (without MMX), these
  routines use the blender functions used also for other lighting functions,
  set with @link(al_set_trans_blender) or @link(al_set_blender_mode).  The MMX
  versions only use the RGB value passed to @link(al_set_trans_blender) and do
  the linear interpolation themselves.  Therefore a new set of blender
  functions passed to @link(al_set_blender_mode) is ignored.

  @bold(Note:) *LIT polygon types don't work correctly in high color and real
  color graphic modes (i.e. 15, 16, 24 and 32 bits per pixel).  This is a
  problem with Allegro not Allegro.pas.  8bpp paletted mode works correctly.

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
      @link(al_drawing_mode) has no effect.)
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
  same low-level asm routines as normal @code(al_polygon3d).

  Notes on scene rendering:
  @unorderedList(
    @item(Unlike @code(al_polygon3d), @code(al_scene_polygon3d) requires valid
      z coordinates for all vertices, regardless of rendering style @(unlike
      @code(al_polygon3d), which only uses z coordinate for @code ( *PTEX* ).@))
    @item(All polygons passed to @code(al_scene_polygon3d) have to be
      @code(al_persp_project)'ed.)
    @item(After @code(al_render_scene) the mode is reset to
      @code(AL_DRAW_MODE_SOLID).)
  )

  Using a lot of @code( *MASK* ) polygons drastically reduces performance,
  because when a MASKed polygon is the first in line of sight, the polygons
  underneath have to be drawn too.  The same applies to @code(AL_POLYTYPE_FLAT)
  polygons drawn with @link(AL_DRAW_MODE_TRANS).

  Z-buffered rendering works also within the scene renderer.  It may be helpful
  when you have a few intersecting polygons, but most of the polygons may be
  safely rendered by the normal scanline sorting algo.  Same as before: just
  @code(OR) the @code(POLYTYPE) with @code(AL_POLYTYPE_ZBUF).  Also, you have
  to clear the z-buffer at the start of the frame.

  @bold(See also) @link(al_create_scene)

@bold(Quaternion math)

  Quaternions are an alternate way to represent the rotation part of a
  transformation, and can be easier to manipulate than matrices. As with a
  matrix, you can encode a geometric transformations in one, concatenate
  several of them to merge multiple transformations, and apply them to a
  vector, but they can only store pure rotations. The big advantage is that you
  can accurately interpolate between two quaternions to get a part-way
  rotation, avoiding the gimbal problems of the more conventional Euler angle
  interpolation.

  Quaternions only have floating point versions, without any _f suffix. Other
  than that, most of the quaternion functions correspond with a matrix function
  that performs a similar operation.

  Quaternion means 'of four parts', and that's exactly what it is. Here is the
  structure:
@longcode(#
  AL_QUAT = RECORD
    w, x, y, z: AL_FLOAT;
  END;
#)
  You will have lots of fun figuring out what these numbers actually mean, but
  that is beyond the scope of this documentation. Quaternions do work -- trust
  me.
  @bold(See also) @link(al_get_rotation_quat) @link(al_apply_quat)
 *)

{$INCLUDE allegro.cfg}

INTERFACE

USES
  albase, allegro, alfixed;


(*****************************************************************************
 * matrix.h
 *     Matrix math routines.
 *)

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
      v: ARRAY [0..2, 0..2] OF AL_FLOAT; (*<Scaling and rotation. *)
      t: ARRAY [0..2] OF AL_FLOAT; (*<Translation. *)
    END;

  VAR
  (* Global variable containing the @italic(do nothing) identity matrix.
     Multiplying by the identity matrix has no effect. *)
    al_identity_matrix: AL_MATRIX;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'identity_matrix';
  (* Global variable containing the @italic(do nothing) identity matrix.
     Multiplying by the identity matrix has no effect. *)
    al_identity_matrix_f: AL_MATRIX;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'identity_matrix_f';



(* Constructs a translation matrix, storing it in @code(m).  When applied to
   the point @code(@(px, py, pz@)), this matrix will produce the point
   @code(@(px+x, py+y, pz+z@)).  In other words, it moves things sideways.
   @seealso(al_apply_matrix) @seealso(al_get_transformation_matrix)
   @seealso(al_qtranslate_matrix) *)
  PROCEDURE al_get_translation_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_translation_matrix';
(* Same as @link(al_get_translation_matrix) but in floating point. *)
  PROCEDURE al_get_translation_matrix_f (m: AL_MATRIX_Fptr; x, y, z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_translation_matrix_f';

(* Constructs a scaling matrix, storing it in m.  When applied to the point
   (px, py, pz), this matrix will produce the point (px*x, py*y, pz*z).  In
   other words, it stretches or shrinks things.
   @seealso(al_apply_matrix) @seealso(al_get_transformation_matrix)
   @seealso(al_qtranslate_matrix) *)
  PROCEDURE al_get_scaling_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_scaling_matrix';
(* Same as @link(al_get_scaling_matrix) but in floating point. *)
  PROCEDURE al_get_scaling_matrix_f (m: AL_MATRIX_Fptr; x, y, z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_scaling_matrix_f';

(* Construct X axis rotation matrices, storing them in m.  When applied to a
   point, these matrices will rotate it about the X axis by the specified
   angle (given in binary, 256 degrees to a circle format).
   @seealso(al_apply_matrix) @seealso(al_get_rotation_matrix) *)
  PROCEDURE al_get_x_rotate_matrix (m: AL_MATRIXptr; r: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_x_rotate_matrix';
(* Same as @link(al_get_x_rotate_matrix) but in floating point. *)
  PROCEDURE al_get_x_rotate_matrix_f (m: AL_MATRIX_Fptr; r: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_x_rotate_matrix_f';

(* Construct Y axis rotation matrices, storing them in m.  When applied to a
   point, these matrices will rotate it about the Y axis by the specified
   angle (given in binary, 256 degrees to a circle format).
   @seealso(al_apply_matrix) @seealso(al_get_rotation_matrix) *)
  PROCEDURE al_get_y_rotate_matrix (m: AL_MATRIXptr; r: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_y_rotate_matrix';
(* Same as @link(al_get_y_rotate_matrix) but in floating point. *)
  PROCEDURE al_get_y_rotate_matrix_f (m: AL_MATRIX_Fptr; r: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_y_rotate_matrix_f';

(* Construct Z axis rotation matrices, storing them in m.  When applied to a
   point, these matrices will rotate it about the Z axis by the specified
   angle (given in binary, 256 degrees to a circle format).
   @seealso(al_apply_matrix) @seealso(al_get_rotation_matrix) *)
  PROCEDURE al_get_z_rotate_matrix (m: AL_MATRIXptr; r: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_z_rotate_matrix';
(* Same as @link(al_get_z_rotate_matrix) but in floating point. *)
  PROCEDURE al_get_z_rotate_matrix_f (m: AL_MATRIX_Fptr; r: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_z_rotate_matrix_f';

(* Constructs a transformation matrix which will rotate points around all three
   axes by the specified amounts (given in binary, 256 degrees to a circle
   format).  The direction of rotation can simply be found out with the
   right-hand rule:  Point the dumb of your right hand towards the origin along
   the axis of rotation, and the fingers will curl in the positive direction of
   rotation.  E.g. if you rotate around the y axis, and look at the scene from
   above, a positive angle will rotate in clockwise direction.
   @seealso(al_get_rotation_matrix_f) @seealso(al_apply_matrix)
   @seealso(al_get_transformation_matrix) @seealso(al_get_x_rotate_matrix)
   @seealso(al_get_y_rotate_matrix) @seealso(al_get_z_rotate_matrix)
   @seealso(al_get_align_matrix) *)
  PROCEDURE al_get_rotation_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rotation_matrix';
(* Same as @link(al_get_rotation_matrix) but in floating point. *)
  PROCEDURE al_get_rotation_matrix_f (m: AL_MATRIX_Fptr; x, y, z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rotation_matrix_f';

(* Rotates a matrix so that it is aligned along the specified coordinate
   vectors (they need not be normalized or perpendicular, but the up and front
   must not be equal).  A front vector of 0,0,-1 and up vector of 0,1,0 will
   return the identity matrix.
   @seealso(al_get_align_matrix_f) @seealso(al_apply_matrix)
   @seealso(al_get_camera_matrix) *)
  PROCEDURE al_get_align_matrix (m: AL_MATRIXptr; xfront, yfront, zfront, xup, yup, zup: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_align_matrix';
(* Same as @link(al_get_align_matrix) but in floating point. *)
  PROCEDURE al_get_align_matrix_f (m: AL_MATRIX_Fptr; xfront, yfront, zfront, xup, yup, zup: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_align_matrix_f';

(* Constructs a transformation matrix which will rotate points around the
   specified x,y,z vector by the specified angle (given in binary, 256 degrees
   to a circle format).
   @seealso(al_get_vector_rotation_matrix_f) @seealso(al_apply_matrix)
   @seealso(al_get_rotation_matrix) @seealso(al_get_align_matrix) *)
  PROCEDURE al_get_vector_rotation_matrix (m: AL_MATRIXptr; x,y,z,a: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_vector_rotation_matrix';
(* Same as @link(al_get_vector_rotation_matrix) but in floating point. *)
  PROCEDURE al_get_vector_rotation_matrix_f (m: AL_MATRIX_Fptr; x,y,z,a: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_vector_rotation_matrix_f';

(* Constructs a transformation matrix which will rotate points around all three
   axes by the specified amounts (given in binary, 256 degrees to a circle
   format), scale the result by the specified amount (pass 1 for no change of
   scale), and then translate to the requested x, y, z position.
   @seealso(al_get_transformation_matrix_f) @seealso(al_get_rotation_matrix)
   @seealso(al_get_scaling_matrix) @seealso(al_get_translation_matrix) *)
  PROCEDURE al_get_transformation_matrix (m:AL_MATRIXptr; scale, xr, yr, zr, x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_transformation_matrix';
(* Same as @link(al_get_transformation_matrix) but in floating point. *)
  PROCEDURE al_get_transformation_matrix_f (m:AL_MATRIX_Fptr; scale, xr, yr, zr, x, y, z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_transformation_matrix_f';

(* Constructs a camera matrix for translating world-space objects into a
   normalised view space, ready for the perspective projection.
   @param(x,y,z specify the camera position)
   @param(xfront,yfront,zfront are the 'in front' vector specifying which way
     the camera is facing @(this can be any length: normalisation is not
     required@).)
   @param(xup,yup,zup are the 'up' direction vector.)
   @param(fov specifies the field of view @(ie. width of the camera focus@) in
     binary, 256 degrees to the circle format.  For typical projections, a
     field of view in the region 32-48 will work well.  64 @(90°@) applies no
     extra scaling - so something which is one unit away from the viewer will
     be directly scaled to the viewport.  A bigger FOV moves you closer to the
     viewing plane, so more objects will appear.  A smaller FOV moves you away
     from the viewing plane, which means you see a smaller part of the world.)
   @param(aspect is used to scale the Y dimensions of the image relative to the
     X axis, so you can use it to adjust the proportions of the output image
     @(set it to 1 for no scaling - but keep in mind that the projection also
     performs scaling according to the viewport size@).  Typically, you will
     pass @code(w/h), where w and h are the parameters you passed to
     @code(al_set_projection_viewport).)
   @seealso(al_apply_matrix) @seealso(al_set_projection_viewport)
   @seealso(al_persp_project) *)
  PROCEDURE al_get_camera_matrix (m: AL_MATRIXptr; x, y, z, xfront, yfront, zfront, xup, yup, zup, fov, aspect: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_camera_matrix';
(* Same as @link(al_get_camera_matrix) but in floating point. *)
  PROCEDURE al_get_camera_matrix_f (m: AL_MATRIX_Fptr; x, y, z, xfront, yfront, zfront, xup, yup, zup, fov, aspect: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_camera_matrix_f';

(* Optimised routine for translating an already generated matrix:  this simply
   adds in the translation offset, so there is no need to build two temporary
   matrices and then multiply them together.
   @seealso(al_qtranslate_matrix_f) @seealso(al_get_translation_matrix) *)
  PROCEDURE al_qtranslate_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_qtranslate_matrix';
(* Same as @link(al_qtranslate_matrix) but in floating point. *)
  PROCEDURE al_qtranslate_matrix_f (m: AL_MATRIX_Fptr; x, y, z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_qtranslate_matrix_f';

(* Optimised routine for scaling an already generated matrix: this simply adds
   in the scale factor, so there is no need to build two temporary matrices and
   then multiply them together.
   @seealso(al_qscale_matrix_f) @seealso(al_get_scaling_matrix) *)
  PROCEDURE al_qscale_matrix (m: AL_MATRIXptr; scale: AL_FIXED);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'qscale_matrix';
(* Same as @link(al_qscale_matrix) but in floating point. *)
  PROCEDURE al_qscale_matrix_f (m: AL_MATRIX_Fptr; scale: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'qscale_matrix_f';

(* Multiplies two matrices, storing the result in @code(out) (this may be a
   duplicate of one of the input matrices, but it is faster when the inputs and
   output are all different).  The resulting matrix will have the same effect
   as the combination of @code(m1) and @code(m2), ie. when applied to a point
   p, @code(@(p * out@) = @(@(p * m1@) * m2@)).  Any number of transformations
   can be concatenated in this way.  Note that matrix multiplication is not
   commutative, ie. @code(al_matrix_mul @(m1, m2@) <> al_matrix_mul @(m2,
   m1@)).
   @seealso(al_apply_matrix) @seealso(al_matrix_mul_f) *)
  PROCEDURE al_matrix_mul (m1, m2, out: AL_MATRIXptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'matrix_mul';
(* Same as @link(al_matrix_mul) but using floats instead than fixed. *)
  PROCEDURE al_matrix_mul_f (m1, m2, out: AL_MATRIX_Fptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'matrix_mul_f';

(* Same as @link(al_apply_matrix) but using floats instead than fixed. *)
  PROCEDURE al_apply_matrix_f (m: AL_MATRIX_Fptr; x, y, z: AL_FLOAT; VAR xout, yout, zout: AL_FLOAT);
    INLINE;



(*****************************************************************************
 * matrix.inl
 *)

(* Multiplies the point (x, y, z) by the transformation matrix m, storing the
   result in (xout, yout, zout).
   @seealso(al_matrix_mul) @seealso(al_apply_matrix_f) *)
  PROCEDURE al_apply_matrix (m: AL_MATRIXptr; x, y, z: AL_FIXED; VAR xout, yout, zout: AL_FIXED);
    INLINE;



(*****************************************************************************
 * quat.h
 *     Quaternion routines.
 *)

  CONST
    AL_QUAT_SHORT = 0; {<@exclude }
    AL_QUAT_LONG  = 1; {<@exclude }
    AL_QUAT_CW    = 2; {<@exclude }
    AL_QUAT_CCW   = 3; {<@exclude }
    AL_QUAT_USER  = 4; {<@exclude }

  TYPE
  (* Pointer to @link(AL_QUAT). *)
    AL_QUATptr = ^AL_QUAT;
  (* Quaternion. *)
    AL_QUAT = RECORD
      w, x, y, z: AL_FLOAT;
    END;

  VAR
  (* Global variable containing the 'do nothing' identity quaternion.
     Multiplying by the identity quaternion has no effect. *)
    al_identity_quat: AL_QUAT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'identity_quat';


(* Multiplies two quaternions, storing the result in out.  The resulting
   quaternion will have the same effect as the combination of p and q, ie. when
   applied to a point, @code(@(point * out@) = @(@(point * p@) * q@)).  Any
   number of rotations can be concatenated in this way.  Note that quaternion
   multiplication is not commutative, ie.
   @code(al_quat_mul @(p, q@) != al_quat_mul @(q, p@)). *)
    PROCEDURE al_quat_mul (CONST p, q: AL_QUATptr; out: AL_QUATptr);
      CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'quat_mul';

(* Construct axis rotation quaternion, storing it in q. When applied to a
   point, this quaternion will rotate it about the relevant axis by the
   specified angle (given in binary, 256 degrees to a circle format). *)
    PROCEDURE al_get_x_rotate_quat (p: AL_QUATptr; r: AL_FLOAT);
      CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_x_rotate_quat';

(* Construct axis rotation quaternion, storing it in q. When applied to a
   point, this quaternion will rotate it about the relevant axis by the
   specified angle (given in binary, 256 degrees to a circle format). *)
    PROCEDURE al_get_y_rotate_quat (p: AL_QUATptr; r: AL_FLOAT);
      CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_y_rotate_quat';

(* Construct axis rotation quaternion, storing it in q. When applied to a
   point, this quaternion will rotate it about the relevant axis by the
   specified angle (given in binary, 256 degrees to a circle format). *)
    PROCEDURE al_get_z_rotate_quat (p: AL_QUATptr; r: AL_FLOAT);
      CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_z_rotate_quat';

(* Constructs a quaternion that will rotate points around all three axes by the
   specified amounts (given in binary, 256 degrees to a circle format). *)
    PROCEDURE al_get_rotation_quat (p: AL_QUATptr; x, y, z: AL_FLOAT);
      CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_rotation_quat';

(* Constructs a quaternion that will rotate points around the especified x,y,z
   vector by the specified amounts (given in binary, 256 degrees to a circle
   format). *)
    PROCEDURE al_get_vector_rotation_quat (p: AL_QUATptr; x, y, z, a: AL_FLOAT);
      CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_vector_rotation_quat';



(* Multiplies the point @code(x, y, z) by the quaternion q, storing the result
   in @code(xout, yout, zout).  This is quite a bit slower than
   @link(al_apply_matrix_f), so only use it to translate a few points.  If you
   have many points, it is much more efficient to call @link(al_quat_to_matrix)
   and then use @code(al_apply_matrix_f). *)
    PROCEDURE al_apply_quat (CONST q: AL_QUATptr; x, y, z: AL_FLOAT; VAR xout, yout, zout: AL_FLOAT);
      INLINE;

(* The same as @link(al_quat_interpolate), but allows more control over how the
   rotation is done. The @code(how) parameter can be any one of the values:
   @unorderedList(
    @item(AL_QUAT_SHORT  - like @link(al_quat_interpolate), use shortest path)
    @item(AL_QUAT_LONG   - rotation will be greater than 180 degrees)
    @item(AL_QUAT_CW     - rotate clockwise when viewed from above)
    @item(AL_QUAT_CCW    - rotate counterclockwise when viewed from above)
    @item(AL_QUAT_USER   - the quaternions are interpolated exactly as given)
   ) *)
    PROCEDURE al_quat_slerp (CONST _from, _to: AL_QUATptr; t: AL_FLOAT; _out: AL_QUATptr; how: AL_INT);
      CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'quat_slerp';

(* Constructs a quaternion that represents a rotation between _from and _to.
   The argument t can be anything between 0 and 1 and represents where between
   from and to the result will be.  0 returns from, 1 returns to, and 0.5 will
   return a rotation exactly in between.  The result is copied to _out.  This
   function will create the short rotation (less than 180 degrees) between
   _from and _to. *)
    PROCEDURE al_quat_interpolate (CONST _from, _to: AL_QUATptr; t: AL_FLOAT; _out: AL_QUATptr);
      INLINE;



(*****************************************************************************
 * 3dmaths.h
 *     3D oriented math routines.
 *)

(* Calculates the length of the vector (x, y, z), using that good 'ole
   Pythagoras theorem.
   @seealso(al_vector_length_f) @seealso(al_normalize_vector) *)
  FUNCTION al_vector_length (x, y, z: AL_FIXED): AL_FIXED;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'vector_length';

(* Same as @code(al_vector_length) but using floats instead than fixed. *)
  FUNCTION al_vector_length_f (x, y, z: AL_FLOAT): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'vector_length_f';



(* Converts the vector (x, y, z) to a unit vector.  This points in the same
   direction as the original vector, but has a length of one.
   @seealso(al_normalize_vector_f) @seealso(al_vector_length) *)
  PROCEDURE al_normalize_vector (VAR x, y, z: AL_FIXED);
    INLINE;

(* Same as @code(al_normalize_vector) but using floats instead than fixed. *)
  PROCEDURE al_normalize_vector_f (VAR x, y, z: AL_FLOAT);
    INLINE;



(* Calculates the cross product (x1, y1, z1) x (x2, y2, z2), storing the result
   in (xout, yout, zout).  The cross product is perpendicular to both of the
   input vectors, so it can be used to generate polygon normals.
   @seealso(al_dot_product) @seealso(al_polygon_z_normal)
   @seealso(al_normalize_vector) *)
  PROCEDURE al_cross_product (x1, y1, z1, x2, y2, z2: AL_FIXED; VAR xout, yout, zout: AL_FIXED);
    INLINE;

(* Same as @link(al_cross_product) but using floats instead than fixed. *)
  PROCEDURE al_cross_product_f (x1, y1, z1, x2, y2, z2: AL_FLOAT; VAR xout, yout, zout: AL_FLOAT);
    INLINE;



(* Sets the viewport used to scale the output of the @link(al_persp_project)
   function.  Pass the dimensions of the screen area you want to draw onto,
   which will typically be 0, 0, @link(AL_SCREEN_W), and @link(AL_SCREEN_H).
   Also don't forget to pass an appropriate aspect ratio to
   @link(al_get_camera_matrix) later.  The width and height you specify here
   will determine how big your viewport is in 3d space.  So if an object in
   your 3D space is w units wide, it will fill the complete screen when you run
   into it (i.e., if it has a distance of 1.0 after the camera matrix was
   applied.  The fov and aspect-ratio parameters to @code(al_get_camera_matrix)
   also apply some scaling though, so this isn't always completely true).  If
   you pass -1/-1/2/2 as parameters, no extra scaling will be performed by the
   projection. *)
  PROCEDURE al_set_projection_viewport (x, y, w, h: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_projection_viewport';



(* Constructs a rotation matrix from a quaternion.
   @seealso(al_matrix_to_quad) *)
  PROCEDURE al_quad_to_matrix (CONST q: AL_QUATptr; m: AL_MATRIX_Fptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'quad_to_matrix';

(* Constructs a quaternion from a rotation matrix.  Translation is discarded
   during the conversion.  Use @link(al_get_align_matrix_f) if the matrix is
   not orthonormalized, because strange things may happen otherwise. *)
  PROCEDURE al_matrix_to_quat (CONST m: AL_MATRIX_Fptr; q: AL_QUATptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'matrix_to_quat';



(*****************************************************************************
 * 3dmaths.inl
 *)

(* Calculates the dot product (x1, y1, z1) . (x2, y2, z2), returning the
   result.
   @seealso(al_dot_product_f) @seealso(al_cross_product)
   @seealso(al_normalize_vector) *)
  FUNCTION al_dot_product (x1, y1, z1, x2, y2, z2: AL_FIXED): AL_FIXED;
    INLINE;

(* Same as @link(al_dot_product) but using floats instead than fixed. *)
  FUNCTION al_dot_product_f (x1, y1, z1, x2, y2, z2: AL_FLOAT): AL_FLOAT;
    INLINE;

(* Projects the 3d point (x, y, z) into 2d screen space, storing the result in
   (xout, yout) and using the scaling parameters previously set by calling
   @code(al_set_projection_viewport).  This function projects from the
   normalized viewing pyramid, which has a camera at the origin and facing
   along the positive z axis.  The x axis runs left/right, y runs up/down, and
   z increases with depth into the screen.  The camera has a 90 degree field of
   view, ie. points on the planes x=z and -x=z will map onto the left and right
   edges of the screen, and the planes y=z and -y=z map to the top and bottom
   of the screen.  If you want a different field of view or camera location,
   you should transform all your objects with an appropriate viewing matrix,
   eg. to get the effect of panning the camera 10 degrees to the left, rotate
   all your objects 10 degrees to the right.
   @seealso(al_persp_project_f) @seealso(al_set_projection_viewport)
   @seealso(al_get_camera_matrix) *)
  PROCEDURE al_persp_project (x, y, z: AL_FIXED; VAR xout, yout: AL_FIXED);
    INLINE;

(* Same as @code(al_persp_project_f) but using floats instead than fixed. *)
  PROCEDURE al_persp_project_f (x, y, z: AL_FLOAT; VAR xout, yout: AL_FLOAT);
    INLINE;



(*****************************************************************************
 * 3d.h
 *     3D polygon drawing routines.
 *)

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
      c: AL_INT;
    END;
  (* List of pointers to @link(AL_V3D). *)
    AL_V3D_LIST = ^AL_V3Dptr;

  (* Pointer to @link(AL_V3D_F). *)
    AL_V3D_Fptr = ^AL_V3D_F;
  (*Like @link(AL_V3D) but using float values instead of fixed ones.
    @seealso(al_polygon3d_f) *)
    AL_V3D_F = RECORD
    (* Position. *)
      x, y, z: AL_FLOAT;
    (* Texture map coordinates. *)
      u, v: AL_FLOAT;
    (* Color. *)
      c: AL_INT;
    END;
  (* List of pointers to @link(AL_V3D_F). *)
    AL_V3D_LIST_F = ^AL_V3D_Fptr;

  CONST
  (* A simple flat shaded polygon, taking the color from the `c' value of the
     first vertex.  This polygon type is affected by the @link(al_drawing_mode)
     function, so it can be used to render @code(XOR) or translucent polygons.
     @seealso(al_polygon3d) *)
    AL_POLYTYPE_FLAT = 0;
  (* A single-color gouraud shaded polygon.  The colors for each vertex are taken
     from the `c' value, and interpolated across the polygon.  This is very fast,
     but will only work in 256-color modes if your palette contains a smooth
     gradient between the colors.  In truecolor modes it interprets the color as
     a packed, display-format value as produced by the @link(al_makecol)
     function.
     @seealso(al_polygon3d) @seealso(al_drawing_mode) *)
    AL_POLYTYPE_GCOL = 1;
  (* A gouraud shaded polygon which interpolates RGB triplets rather than a
     single color.  In 256-color modes this uses the global @link(al_rgb_table)
     table to convert the result to an 8-bit paletted color, so it must only be
     used after you have set up the RGB mapping table!  The colors for each
     vertex are taken from the `c' value, which is interpreted as a 24-bit RGB
     triplet ($FF0000 is red, $00FF00 is green, and $0000FF is blue).
     @seealso(al_polygon3d) @seealso(al_drawing_mode) *)
    AL_POLYTYPE_GRGB = 2;
  (* An affine texture mapped polygon.  This stretches the texture across the
    polygon with a simple 2D linear interpolation, which is fast but not
    mathematically correct.  It can look OK if the polygon is fairly small or
    flat-on to the camera, but because it doesn't deal with perspective
    foreshortening, it can produce strange warping artifacts.  To see what this
    means, run Allegro's ex3d example program and see what happens to the
    @link(al_polygon3d) procedure when you zoom in very close to the cube. *)
    AL_POLYTYPE_ATEX = 3;
  (* A perspective-correct texture mapped polygon.  This uses the `z' value from
    the vertex structure as well as the u/v coordinates, so textures are
    displayed correctly regardless of the angle they are viewed from.  Because it
    involves division calculations in the inner texture mapping loop, this mode
    is a lot slower than @link(AL_POLYTYPE_ATEX), and it uses floating point so
    it will be very slow on anything less than a Pentium (even with an FPU, a 486
    can't overlap floating point division with other integer operations like the
    Pentium can). @seealso(al_polygon3d) *)
    AL_POLYTYPE_PTEX = 4;
  (* Like @link(AL_POLYTYPE_ATEX), but @link(al_bitmap_mask_color) texture map
    pixels are skipped, allowing parts of the texture map to be transparent.
    @seealso(al_polygon3d) *)
    AL_POLYTYPE_ATEX_MASK = 5;
  (* Like @link(AL_POLYTYPE_PTEX), but @link(al_bitmap_mask_color) texture map
    pixels are skipped, allowing parts of the texture map to be transparent.
    @seealso(al_polygon3d) *)
    AL_POLYTYPE_PTEX_MASK = 6;
  (* Like @link(AL_POLYTYPE_ATEX), but the global @link(al_color_table) (for
    256-color modes) or @link(alblend blender) function (for non-MMX truecolor
    modes) is used to blend the texture with a light level taken from the `c'
    value in the vertex structure.  This must only be used after you have set up
    the color mapping table or blender functions! @seealso(al_polygon3d) *)
    AL_POLYTYPE_ATEX_LIT = 7;
  (* Like @link(AL_POLYTYPE_PTEX), but the global @link(al_color_table) (for
    256-color modes) or @link(alblend blender) function (for non-MMX truecolor
    modes) is used to blend the texture with a light level taken from the `c'
    value in the vertex structure.  This must only be used after you have set up
    the color mapping table or blender functions!

    @bold(Note:) this polygon type doesn't work correctly in high color and real
    color graphic modes (i.e. 15, 16, 24 and 32 bits per pixel).  This is a
    problem with Allegro not Allegro.pas.  8bpp paletted mode works correctly.
    @seealso(al_polygon3d) *)
    AL_POLYTYPE_PTEX_LIT = 8;
  (* Like @link(AL_POLYTYPE_ATEX_LIT), but @link(al_bitmap_mask_color) texture
    map pixels are skipped, allowing parts of the texture map to be transparent.
  
    @bold(Note:) this polygon type doesn't work correctly in high color and real
    color graphic modes (i.e. 15, 16, 24 and 32 bits per pixel).  This is a
    problem with Allegro not Allegro.pas.  8bpp paletted mode works correctly.
    @seealso(al_polygon3d) *)
    AL_POLYTYPE_ATEX_MASK_LIT = 9;
  (* Like @link(AL_POLYTYPE_PTEX_LIT), but @link(al_bitmap_mask_color) texture
    map pixels are skipped, allowing parts of the texture map to be transparent.

    @bold(Note:) this polygon type doesn't work correctly in high color and real
    color graphic modes (i.e. 15, 16, 24 and 32 bits per pixel).  This is a
    problem with Allegro not Allegro.pas.  8bpp paletted mode works correctly.
    @seealso(al_polygon3d) *)
    AL_POLYTYPE_PTEX_MASK_LIT = 10;
  (* Render translucent textures.  All the general rules for drawing translucent
    things apply.  However, these modes have a major limitation:  they only work
    with memory bitmaps or linear frame buffers (not with banked frame buffers).
    Don't even try, they do not check and your program will die horribly (or at
    least draw wrong things).
    @seealso(al_polygon3d) @seealso(AL_POLYTYPE_ATEX)
    @seealso(al_create_trans_table) @seealso(al_set_trans_blender) *)
    AL_POLYTYPE_ATEX_TRANS = 11;
  (* Render translucent textures.  All the general rules for drawing translucent
    things apply.  However, these modes have a major limitation:  they only work
    with memory bitmaps or linear frame buffers (not with banked frame buffers).
    Don't even try, they do not check and your program will die horribly (or at
    least draw wrong things).
    @seealso(al_polygon3d) @seealso(AL_POLYTYPE_PTEX)
    @seealso(al_create_trans_table) @seealso(al_set_trans_blender) *)
    AL_POLYTYPE_PTEX_TRANS = 12;
  (* Like @link(AL_POLYTYPE_ATEX_TRANS), but @link(al_bitmap_mask_color) texture
    map pixels are skipped, allowing parts of the texture map to be transparent.
    @seealso(al_polygon3d) *)
    AL_POLYTYPE_ATEX_MASK_TRANS = 13;
  (* Like @link(AL_POLYTYPE_PTEX_TRANS), but @link(al_bitmap_mask_color) texture
    map pixels are skipped, allowing parts of the texture map to be transparent.
    @seealso(al_polygon3d) *)
    AL_POLYTYPE_PTEX_MASK_TRANS = 14;
  (* *)
    AL_POLYTYPE_MAX = 15;
  (* Used for z-buffered mode. @seealso(al_create_zbuffer) *)
    AL_POLYTYPE_ZBUF = 16;

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
    al_scene_gap: AL_FLOAT;
      EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'scene_gap';

(* Fixed point version of @link(al_clip3d_f).  This function should be used
   with caution, due to the limited precision of fixed point arithmetic and
   high chance of rounding errors:  the floating point code is better for most
   situations. @returns(the number of vertices after clipping is done.)
   @seealso(al_clip3d_f) @seealso(al_polygon3d) *)
  FUNCTION al_clip3d (_type: AL_INT; min_z, max_z: AL_FIXED; vc: AL_INT; vtx, vout, vtmp: AL_V3D_LIST; out: AL_INTptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clip3d';

(* Clips the polygon given in @code(vtx).  The number of vertices is @code(vc),
   the result goes in @code(vout), and @code(vtmp) and @code(out) are needed
   for internal purposes.  The pointers in @code(vtx), @code(vout) and
   @code(vtmp) must point to valid @link(AL_V3D_f) structures.

   As additional vertices may appear in the process of clipping, so the size
   of @code(vout), @code(vtmp) and @code(out) should be at least @code(vc *
   POW @(1.5, n@)), where @code(n) is the number of clipping planes (5 or 6).

   The frustum (viewing volume) is defined by @code(-z<x<z, -z<y<z,
   0<min_z<z<max_z). If @code(max_z<=min_z), the @code(z<max_z) clipping is not
   done.  As you can see, clipping is done in the camera space, with
   perspective in mind, so this routine should be called after you apply the
   camera matrix, but before the perspective projection.  The routine will
   correctly interpolate @code(u), @code(v), and @code(c) in the vertex
   structure.  However, no provision is made for high/truecolor
   @link(AL_POLYTYPE_GCOL).
   @returns(the number of vertices after clipping is done.)
   @seealso(al_clip3d) @seealso(al_polygon3d_f) *)
  FUNCTION al_clip3d_f (_type: AL_INT; min_z, max_z: AL_FLOAT; vc: AL_INT; vtx, vout, vtmp: AL_V3D_LIST_F; out: AL_INTptr): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clip3d_f';



(* Finds the Z component of the normal vector to the specified three vertices
   (which must be part of a convex polygon).  This is used mainly in back-face
   culling.  The back-faces of closed polyhedra are never visible to the
   viewer, therefore they never need to be drawn.  This can cull on average
   half the polygons from a scene.  If the normal is negative the polygon can
   safely be culled.  If it is zero, the polygon is perpendicular to the screen.

   However, this method of culling back-faces must only be used once the X and
   Y coordinates have been projected into screen space using
   @link(al_persp_project) (or if an orthographic (isometric) projection is
   being used).  Note that this function will fail if the three vertices are
   co-linear (they lie on the same line) in 3D space.  Also note that the fixed
   point version of the function is less accurate than the floating point one
   so it might return wrong values in some cases. @seealso(al_cross_product) *)
  FUNCTION al_polygon_z_normal (CONST v1, v2, v3: AL_V3Dptr): AL_FIXED;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'polygon_z_normal';

(* Same as @link(al_polygon_z_normal) but using floats instead than fixed. *)
  FUNCTION al_polygon_z_normal_f (CONST v1, v2, v3: AL_V3D_Fptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'polygon_z_normal_f';



  TYPE
  (* Pointer to a @link(AL_ZBUFFER). *)
    AL_ZBUFFERptr = ^AL_ZBUFFER;
  (* Structure used by Allegro's 3d zbuffered rendering functions.

     You are not supposed to mix @code(AL_ZBUFFER) with @link(AL_BITMAP) even
     though it is currently possible to do so.  This is just an internal
     representation, and it may change in the future.
     @seealso(al_create_zbuffer) *)
    AL_ZBUFFER = AL_BITMAP;

(* Creates a Z-buffer using the size of the @link(AL_BITMAP) you are planning
   to draw on.  Several Z-buffers can be defined but only one can be used at
   the same time, so you must call @code(al_set_zbuffer) to make this Z-buffer
   active.
   @returns(the pointer to the @link(AL_ZBUFFER) or @nil if there was an error.
     Remember to destroy the @code(AL_ZBUFFER) once you are done with it, to
     avoid having memory leaks.)
   @seealso(al_create_sub_zbuffer) @seealso(al_clear_zbuffer)
   @seealso(al_destroy_zbuffer) *)
  FUNCTION al_create_zbuffer (bmp: AL_BITMAPptr): AL_ZBUFFERptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_zbuffer';

(* Creates a sub-z-buffer, ie. a z-buffer sharing drawing memory with a
  pre-existing z-buffer, but possibly with a different size.  The same rules as
  for sub-bitmaps apply:  the sub-z-buffer width and height can extend beyond
  the right and bottom edges of the parent (they will be clipped), but the
  origin point must lie within the parent region.

  When drawing z-buffered to a bitmap, the top left corner of the bitmap is
  always mapped to the top left corner of the current z-buffer.  So this
  function is primarily useful if you want to draw to a sub-bitmap and use the
  corresponding sub-area of the z-buffer.  In other cases, eg. if you just want
  to draw to a sub-bitmap of screen (and not to other parts of screen), then
  you would usually want to create a normal z-buffer (not sub-z-buffer) the
  size of the visible screen.  You don't need to first create a z-buffer the
  size of the virtual screen and then a sub-z-buffer of that.
  @returns(The pointer to the sub @code(AL_ZBUFFER) or @nil if there was an
  error.  Remember to destroy the z-buffer once you are done with it, to avoid
  having memory leaks.)
  @seealso(al_create_zbuffer) *)
  FUNCTION al_create_sub_zbuffer (parent: AL_ZBUFFERptr; x, y, width, height: AL_INT): AL_ZBUFFERptr;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'create_sub_zbuffer';

(* Makes the given Z-buffer be the active one.  This should have been
  previously created with @link(al_create_zbuffer). *)
  PROCEDURE al_set_zbuffer (zbuf: AL_ZBUFFERptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_zbuffer';

(* Writes z into the given Z-buffer (0 means far away).  This function should
  be used to initialize the Z-buffer before each frame.  Actually, low-level
  routines compare depth of the current pixel with 1/z:  for example, if you
  want to clip polygons farther than 10, you must call
  @code(al_clear_zbuffer @(zbuf, 0.1@)).
  @seealso(al_create_zbuffer) *)
  PROCEDURE al_clear_zbuffer (zbuf: AL_ZBUFFERptr; z: AL_FLOAT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_zbuffer';

(* Destroys the Z-buffer when you are finished with it.  Use this to avoid
  memory leaks in your program. @seealso(al_create_zbuffer) *)
  PROCEDURE al_destroy_zbuffer (bmp: AL_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_zbuffer';



(* Allocates memory for a scene, @code(nedge) and @code(npoly) are your
   estimates of how many edges and how many polygons you will render (you
   cannot get over the limit specified here).  If you use same values in
   succesive calls, the space will be reused (no new @code(GetMem)).

  The memory allocated is a little less than 150 * (nedge + npoly) bytes.
  @returns(@true on success, or @false if allocations fail.)
  @seealso(al_scene_polygon3d) @seealso(al_render_scene)
  @seealso(al_clear_scene) @seealso(al_destroy_scene) @seealso(al_scene_gap)
  @seealso(al_create_zbuffer) *)
  FUNCTION al_create_scene (nedge, npoly: AL_INT): BOOLEAN;
    INLINE;

(* Initializes a scene. The bitmap is the bitmap you will eventually render
  on. @seealso(al_create_scene) *)
  PROCEDURE al_clear_scene (bmp: AL_BITMAPptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'clear_scene';

(* Deallocate memory previously allocated by create_scene. Use this to avoid
  memory leaks in your program. @seealso(al_create_scene) *)
  PROCEDURE al_destroy_scene;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'destroy_scene';

(* Puts a polygon in the rendering list.  Nothing is really rendered at this
   moment.  Should be called between @link(al_clear_scene) and
   @link(al_render_scene).

   Arguments are the same as for @link(al_polygon3d), except the bitmap is
   missing.  The one passed to @code(al_clear_scene) will be used.

   Unlike @code(al_polygon3d), the polygon may be concave or self-intersecting.
   Shapes that penetrate one another may look OK, but they are not really
   handled by this code.

   Note that the texture is stored as a pointer only, and you should keep the
   actual bitmap around until @code(al_render_scene), where it is used.

   Since the @link(AL_POLYTYPE_FLAT) style is implemented with the low-level
   @link(al_hline) funtion, the @code(AL_POLYTYPE_FLAT) style is subject to
   @link(al_drawing_mode).  All these modes are valid.  Along with the polygon,
   this mode will be stored for the rendering moment, and also all the other
   related variables (color-map, pattern pointer, anchor, blender values).

   The settings of the @link(AL_CPU_MMX) and @link(AL_CPU_3DNOW) flags of the
   @link(al_cpu_capabilities) global variable on entry in this routine affect
   the choice of low-level asm routine that will be used by
   @code(al_render_scene) for this polygon.
   @returns(zero on success, or a negative number if it won't be rendered for
    lack of a rendering routine.)
   @seealso(al_create_scene) @seealso(al_clear_scene) @seealso(al_render_scene)
   @seealso(al_polygon3d) *)
  FUNCTION al_scene_polygon3d (_type: AL_INT; texture: AL_BITMAPptr; vx: AL_INT; vtx: AL_V3D_LIST): BOOLEAN;
    INLINE;

(* Floating point version of @link(al_scene_polygon3d). *)
  FUNCTION al_scene_polygon3d_f (_type: AL_INT; texture: AL_BITMAPptr; vx: AL_INT; vtx: AL_V3D_LIST_F): BOOLEAN;
    INLINE;

(* Renders all the specified @link(al_scene_polygon3d)'s on the bitmap passed
   to @link(al_clear_scene).  Rendering is done one scanline at a time, with no
   pixel being processed more than once.

   Note that between @code(al_clear_scene) and @code(al_render_scene) you
   shouldn't change the clip rectangle of the destination bitmap.  For speed
   reasons, you should set the clip rectangle to the minimum.

   Note also that all the textures passed to @code(al_scene_polygon3d) are
   stored as pointers only and actually used in @code(al_render_scene). *)
  PROCEDURE al_render_scene;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'render_scene';



(*****************************************************************************
 * draw.inl
 *)

(* Draw 3d polygons onto the specified bitmap, using the specified rendering
  mode.  This routine don't support concave or self-intersecting shapes, and
  it can't draw onto mode-X screen bitmaps (if you want to write 3d code in
  mode-X, draw onto a memory bitmap and then @link(al_blit) to the
  @link(al_screen)).  The width and height of the texture bitmap must be powers
  of two, but can be different, eg. a 64x16 texture is fine, but a 17x3 one is
  not.  The vertex count parameter (@code(vc)) should be followed by an array
  containing the appropriate number of pointers to @link(AL_V3D) structures.

  How the vertex data is used depends on the rendering mode:

  The @code(x) and @code(y) values specify the position of the vertex in 2D
  screen coordinates.

  The @code(z) value is only required when doing perspective correct texture
  mapping, and specifies the depth of the point in 3D world coordinates.

  The @code(u) and @code(v) coordinates are only required when doing texture
  mapping, and specify a point on the texture plane to be mapped on to this
  vertex.  The texture plane is an infinite plane with the texture bitmap
  tiled across it.  Each vertex in the polygon has a corresponding vertex on
  the texture plane, and the image of the resulting polygon in the texture
  plane will be mapped on to the polygon on the screen.

  We refer to pixels in the texture plane as @italic(texels).  Each texel is a
  block, not just a point, and whole numbers for @code(u) and @code(v) refer to
  the top-left corner of a texel.  This has a few implications.  If you want to
  draw a rectangular polygon and map a texture sized 32x32 on to it, you would
  use the texture coordinates (0,0), (0,32), (32,32) and (32,0), assuming the
  vertices are specified in anticlockwise order.  The texture will then be
  mapped perfectly on to the polygon.  However, note that when we set u=32, the
  last column of texels seen on the screen is the one at u=31, and the same
  goes for v.  This is because the coordinates refer to the top-left corner of
  the texels.  In effect, texture coordinates at the right and bottom on the
  texture plane are exclusive.

  There is another interesting point here.  If you have two polygons side by
  side sharing two vertices (like the two parts of folded piece of cardboard),
  and you want to map a texture across them seamlessly, the values of u and v
  on the vertices at the join will be the same for both polygons.  For example,
  if they are both rectangular, one polygon may use (0,0), (0,32), (32,32) and
  (32,0), and the other may use (32,0), (32,32), (64,32), (64,0).  This would
  create a seamless join.

  Of course you can specify fractional numbers for u and v to indicate a point
  part-way across a texel.  In addition, since the texture plane is infinite,
  you can specify larger values than the size of the texture.  This can be used
  to tile the texture several times across the polygon.

  The @code(c) value specifies the vertex color, and is interpreted differently
  by various rendering modes. Read the description of the @code(AL_POLYTYPE_* )
  constants for details.
  @seealso(al_polygon3d_f) @seealso(al_triangle3d) @seealso(al_quad3d)
  @seealso(al_gfx_capabilities). *)
  PROCEDURE al_polygon3d (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; vc: AL_INT; vtx: AL_V3D_LIST);
    INLINE;

(* Same as @link(al_polygon3d) but using floats instead than fixed. *)
  PROCEDURE al_polygon3d_f (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; vc: AL_INT; vtx: AL_V3D_LIST_F);
    INLINE;

(* Draw 3D triangles, using fixed point vertex structures.  Unlike
  @link(al_quad3d), this procedure is not a wrapper of @link(al_polygon3d).
  The @code(al_triangle3d) procedure uses their own routines taking into
  account the constantness of the gradients.  Therefore @code(al_triangle3d
  @(bmp, type, tex, v1, v2, v3@)) is faster than @code (al_polygon3d @(bmp,
  type, tex, 3, v@)).
  @seealso(al_triangle3d_f) *)
  PROCEDURE al_triangle3d (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; v1, v2, v3: AL_V3Dptr);
    INLINE;

(* Same as @code(al_triangle3d) but using floats instead than fixed. *)
  PROCEDURE al_triangle3d_f (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; v1, v2, v3: AL_V3D_Fptr);
    INLINE;

(* Draw 3D quads, using fixed point vertex structures.  This is equivalent to
   calling @code(al_polygon3d)@code(@(bmp, type, tex, 4, v@)).
   @seealso(al_triangle3d) @seealso(al_quad3d_f) *)
  PROCEDURE al_quad3d (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; v1, v2, v3, v4: AL_V3Dptr);
    INLINE;

(* Same as @code(al_quad3d) but using floats instead than fixed. *)
  PROCEDURE al_quad3d_f (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; v1, v2, v3, v4: AL_V3D_Fptr);
    INLINE;

IMPLEMENTATION

(*****************************************************************************
 * matrix.h
 *)

  PROCEDURE apply_matrix_f (m: AL_MATRIX_fptr; x, y, z: AL_FLOAT; xout, yout, zout: AL_FLOATptr);
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_apply_matrix_f (m: AL_MATRIX_fptr; x, y, z: AL_FLOAT; VAR xout, yout, zout: AL_FLOAT);
  BEGIN
    apply_matrix_f (m, x, y, z, @xout, @yout, @zout);
{
    xout := (x * m^.v[0, 0]) +
	    (y * m^.v[0, 1]) +
	    (z * m^.v[0, 2]) +
	    m^.t[0];
    yout := (x * m^.v[1, 0]) +
	    (y * m^.v[1, 1]) +
	    (z * m^.v[1, 2]) +
	     m^.t[1];
    zout := (x * m^.v[2, 0]) +
	    (y * m^.v[2, 1]) +
	    (z * m^.v[2, 2]) +
	    m^.t[2];
}
  END;

(*****************************************************************************
 * matrix.inl
 *)

  PROCEDURE al_apply_matrix (m: AL_MATRIXptr;
		x, y, z: AL_FIXED; VAR xout, yout, zout: AL_FIXED);
  BEGIN
    xout := al_fixmul (x, m^.v[0, 0]) +
	    al_fixmul (y, m^.v[0, 1]) +
	    al_fixmul (z, m^.v[0, 2]) +
	    m^.t[0];
    yout := al_fixmul (x, m^.v[1, 0]) +
	    al_fixmul (y, m^.v[1, 1]) +
	    al_fixmul (z, m^.v[1, 2]) +
	    m^.t[1];
    zout := al_fixmul (x, m^.v[2, 0]) +
	    al_fixmul (y, m^.v[2, 1]) +
	    al_fixmul (z, m^.v[2, 2]) +
	    m^.t[2];
  END;



(*****************************************************************************
 * quat.h
 *)

  PROCEDURE apply_quat (CONST q: AL_QUATptr; x, y, z: AL_FLOAT; xout, yout, zout: AL_FLOATptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_apply_quat (CONST q: AL_QUATptr; x, y, z: AL_FLOAT; VAR xout, yout, zout: AL_FLOAT);
  BEGIN
    apply_quat (q, x, y, z, @xout, @yout, @zout);
  END;



  PROCEDURE al_quat_interpolate (CONST _from, _to: AL_QUATptr; t: AL_FLOAT; _out: AL_QUATptr);
  BEGIN
    al_quat_slerp (_from, _to, t, _out, AL_QUAT_SHORT);
  END;



(*****************************************************************************
 * 3dmaths.h
*)

  PROCEDURE normalize_vector (x, y, z: AL_FIXEDptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_normalize_vector (VAR x, y, z: AL_FIXED);
  BEGIN
    normalize_vector (@x, @y, @z);
  END;



  PROCEDURE normalize_vector_f (x, y, z: AL_FLOATptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_normalize_vector_f (VAR x, y, z: AL_FLOAT);
  BEGIN
    normalize_vector_f (@x, @y, @z);
  END;



  PROCEDURE cross_product (x1, y1, z1, x2, y2, z2: AL_FIXED; xout, yout, zout: AL_FIXEDptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_cross_product (x1, y1, z1, x2, y2, z2: AL_FIXED; VAR xout, yout, zout: AL_FIXED);
  BEGIN
    cross_product (x1, y1, z1, x2, y2, z2, @xout, @yout, @zout);
  END;



  PROCEDURE cross_product_f (x1, y1, z1, x2, y2, z2: AL_FLOAT; xout, yout, zout: AL_FLOATptr);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  PROCEDURE al_cross_product_f (x1, y1, z1, x2, y2, z2: AL_FLOAT; VAR xout, yout, zout: AL_FLOAT);
  BEGIN
    cross_product_f (x1, y1, z1, x2, y2, z2, @xout, @yout, @zout);
  END;



  VAR
    _persp_xscale: AL_FIXED; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
    _persp_yscale: AL_FIXED; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
    _persp_xoffset: AL_FIXED; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
    _persp_yoffset: AL_FIXED; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

    _persp_xscale_f: AL_FLOAT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
    _persp_yscale_f: AL_FLOAT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
    _persp_xoffset_f: AL_FLOAT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
    _persp_yoffset_f: AL_FLOAT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;



(*****************************************************************************
 * 3dmaths.inl
 *)

  FUNCTION al_dot_product (x1, y1, z1, x2, y2, z2: AL_FIXED): AL_FIXED;
  BEGIN
    al_dot_product := al_fixmul (x1, x2) + al_fixmul (y1, y2) + al_fixmul (z1, z2);
  END;



  FUNCTION al_dot_product_f (x1, y1, z1, x2, y2, z2: AL_FLOAT): AL_FLOAT;
  BEGIN
    al_dot_product_f := (x1 * x2) + (y1 * y2) + (z1 * z2);
  END;



  PROCEDURE al_persp_project (x, y, z: AL_FIXED; VAR xout, yout: AL_FIXED);
  BEGIN
    xout := al_fixmul (al_fixdiv (x, z), _persp_xscale) + _persp_xoffset;
    yout := al_fixmul (al_fixdiv (y, z), _persp_yscale) + _persp_yoffset;
  END;



  PROCEDURE al_persp_project_f (x, y, z: AL_FLOAT; VAR xout, yout: AL_FLOAT);
  VAR
    z1: AL_FLOAT;
  BEGIN
    z1 := 1.0 / z;
    xout := ((x * z1) * _persp_xscale_f) + _persp_xoffset_f;
    yout := ((y * z1) * _persp_yscale_f) + _persp_yoffset_f;
  END;



(*****************************************************************************
 * 3d.h
 *)

{ Direct implementation.  Was used in previous versions.
  FUNCTION al_polygon_z_normal (v1, v2, v3: AL_V3Dptr): AL_FIXED;
  BEGIN
    al_polygon_z_normal :=
      (al_fixmul(v2^.x-v1^.x, v3^.y-v2^.y) - al_fixmul(v3^.x-v2^.x, v2^.y-v1^.y));
  END;



  FUNCTION al_polygon_z_normal_f (v1, v2, v3: AL_V3D_Fptr): SINGLE;
  BEGIN
    al_polygon_z_normal_f := ((v2^.x-v1^.x) * (v3^.y-v2^.y)) - ((v3^.x-v2^.x) * (v2^.y-v1^.y));
  END;
}


  FUNCTION create_scene (nedge, npoly: AL_INT): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_create_scene (nedge, npoly: AL_INT): BOOLEAN;
  BEGIN
    al_create_scene := create_scene (nedge, npoly) = 0;
  END;



  FUNCTION scene_polygon3d (_type: AL_INT; texture: AL_BITMAPptr; vx: AL_INT; vtx: AL_V3D_LIST): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_scene_polygon3d (_type: AL_INT; texture: AL_BITMAPptr; vx: AL_INT; vtx: AL_V3D_LIST): BOOLEAN;
  BEGIN
    al_scene_polygon3d := scene_polygon3d (_type, texture, vx, vtx) = 0;
  END;



  FUNCTION scene_polygon3d_f (_type: AL_INT; texture: AL_BITMAPptr; vx: AL_INT; vtx: AL_V3D_LIST_F): AL_INT;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_scene_polygon3d_f (_type: AL_INT; texture: AL_BITMAPptr; vx: AL_INT; vtx: AL_V3D_LIST_F): BOOLEAN;
  BEGIN
    al_scene_polygon3d_f := scene_polygon3d_f (_type, texture, vx, vtx) = 0;
  END;



(*****************************************************************************
 * draw.inl
 *)

  PROCEDURE al_polygon3d (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; vc: AL_INT; vtx: AL_V3D_LIST);
  BEGIN
    bmp^.vtable^.polygon3d (bmp, _type, texture, vc, vtx);
  END;



  PROCEDURE al_polygon3d_f (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; vc: AL_INT; vtx: AL_V3D_LIST_F);
  BEGIN
    bmp^.vtable^.polygon3d_f (bmp, _type, texture, vc, vtx);
  END;



  PROCEDURE al_triangle3d (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; v1, v2, v3: AL_V3Dptr);
  BEGIN
    bmp^.vtable^.triangle3d (bmp, _type, texture, v1, v2, v3);
  END;



  PROCEDURE al_triangle3d_f (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; v1, v2, v3: AL_V3D_Fptr);
  BEGIN
    bmp^.vtable^.triangle3d_f (bmp, _type, texture, v1, v2, v3);
  END;



  PROCEDURE al_quad3d (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; v1, v2, v3, v4: AL_V3Dptr);
  BEGIN
    bmp^.vtable^.quad3d (bmp, _type, texture, v1, v2, v3, v4);
  END;



  PROCEDURE al_quad3d_f (bmp: AL_BITMAPptr; _type: AL_INT; texture: AL_BITMAPptr; v1, v2, v3, v4: AL_V3D_Fptr);
  BEGIN
    bmp^.vtable^.quad3d_f (bmp, _type, texture, v1, v2, v3, v4);
  END;

END.
