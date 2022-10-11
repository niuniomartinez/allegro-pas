unit al5primitives;
(***<Primitive drawing.

  @include(../docs/al5primitives.pds) *)
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

{$INCLUDE allegro5.cfg}

interface

  uses
    Allegro5, al5base;

  const
    ALLEGRO_PRIM_MAX_USER_ATTR = 10; {**<@exclude }

  type
    ALLEGRO_PRIM_TYPE = (
      ALLEGRO_PRIM_LINE_LIST,
      ALLEGRO_PRIM_LINE_STRIP,
      ALLEGRO_PRIM_LINE_LOOP,
      ALLEGRO_PRIM_TRIANGLE_LIST,
      ALLEGRO_PRIM_TRIANGLE_STRIP,
      ALLEGRO_PRIM_TRIANGLE_FAN,
      ALLEGRO_PRIM_POINT_LIST,
      ALLEGRO_PRIM_NUM_TYPES
    );



    ALLEGRO_PRIM_ATTR = (
      ALLEGRO_PRIM_ATTR_NONE = 0,
      ALLEGRO_PRIM_POSITION = 1,
      ALLEGRO_PRIM_COLOR_ATTR,
      ALLEGRO_PRIM_TEX_COORD,
      ALLEGRO_PRIM_TEX_COORD_PIXEL,
      ALLEGRO_PRIM_USER_ATTR,
      ALLEGRO_PRIM_USER_ATTR2,
      ALLEGRO_PRIM_USER_ATTR3,
      ALLEGRO_PRIM_USER_ATTR4,
      ALLEGRO_PRIM_USER_ATTR5,
      ALLEGRO_PRIM_USER_ATTR6,
      ALLEGRO_PRIM_USER_ATTR7,
      ALLEGRO_PRIM_USER_ATTR8,
      ALLEGRO_PRIM_USER_ATTR9,
      ALLEGRO_PRIM_USER_ATTR10,
      ALLEGRO_PRIM_ATTR_NUM
    );



    ALLEGRO_PRIM_STORAGE = (
      ALLEGRO_PRIM_FLOAT_2,
      ALLEGRO_PRIM_FLOAT_3,
      ALLEGRO_PRIM_SHORT_2,
      ALLEGRO_PRIM_FLOAT_1,
      ALLEGRO_PRIM_FLOAT_4,
      ALLEGRO_PRIM_UBYTE_4,
      ALLEGRO_PRIM_SHORT_4,
      ALLEGRO_PRIM_NORMALIZED_UBYTE_4,
      ALLEGRO_PRIM_NORMALIZED_SHORT_2,
      ALLEGRO_PRIM_NORMALIZED_SHORT_4,
      ALLEGRO_PRIM_NORMALIZED_USHORT_2,
      ALLEGRO_PRIM_NORMALIZED_USHORT_4,
      ALLEGRO_PRIM_HALF_FLOAT_2,
      ALLEGRO_PRIM_HALF_FLOAT_4
    );

  const
  (*** Default storage value.  @seealso(ALLEGRO_PRIM_STORAGE) *)
    ALLEGRO_PRIM_STORAGE_NONE = ALLEGRO_PRIM_FLOAT_2;

  type
    ALLEGRO_LINE_JOIN = (
      ALLEGRO_LINE_JOIN_NONE,
      ALLEGRO_LINE_JOIN_BEVEL,
      ALLEGRO_LINE_JOIN_ROUND,
      ALLEGRO_LINE_JOIN_MITER
    );

   const
   (*** Alternative name for @code(ALLEGRO_LINE_JOIN_MITER). *)
     ALLEGRO_LINE_JOIN_MITRE = ALLEGRO_LINE_JOIN_MITER;

   type
     ALLEGRO_LINE_CAP = (
       ALLEGRO_LINE_CAP_NONE,
       ALLEGRO_LINE_CAP_SQUARE,
       ALLEGRO_LINE_CAP_ROUND,
       ALLEGRO_LINE_CAP_TRIANGLE,
       ALLEGRO_LINE_CAP_CLOSED
     );

     ALLEGRO_PRIM_BUFFER_FLAGS = (
       ALLEGRO_PRIM_BUFFER_NONE         = 0,
       ALLEGRO_PRIM_BUFFER_STREAM       = $01,
       ALLEGRO_PRIM_BUFFER_STATIC       = $02,
       ALLEGRO_PRIM_BUFFER_DYNAMIC      = $04,
       ALLEGRO_PRIM_BUFFER_READWRITE    = $08
     );

  const
    ALLEGRO_VERTEX_CACHE_SIZE = 256;
  { Ignore this value as it doesn't affect Allegro.
    ALLEGRO_PRIM_QUALITY = 10; }

  type
  (*** Pointer to @link(ALLEGRO_VERTEX_ELEMENT). *)
    ALLEGRO_VERTEX_ELEMENTptr = ^ALLEGRO_VERTEX_ELEMENT;
    ALLEGRO_VERTEX_ELEMENT = record
      attribute: ALLEGRO_PRIM_ATTR;
      storage: ALLEGRO_PRIM_STORAGE;
      offset: AL_INT;
    end;


    ALLEGRO_VERTEX_DECLptr = AL_POINTER;

  (*** Pointer to @link(ALLEGRO_VERTEX). *)
    ALLEGRO_VERTEXptr = ^ALLEGRO_VERTEX;
    ALLEGRO_VERTEX = record
    (*** Position of the vertex. *)
      x, y, z: AL_FLOAT;
    (*** Texture coordinates measured in pixels. *)
      u, v: AL_FLOAT;
    (*** Color of the vertex. *)
      color: ALLEGRO_COLOR;
    end;

    ALLEGRO_VERTEX_BUFFERptr = AL_POINTER;

    ALLEGRO_INDEX_BUFFERptr = AL_POINTER;

  { Some additional types to help in declarations. }
    ALLEGRO_EMIT_TRIANGLE_PROC = procedure (a, b, c: AL_INT; p: AL_VOIDptr); CDECL;

    ALLEGRO_INIT_TRIANGLE_PROC = procedure (state: AL_UINTPTR_T; v1, v2, v3: ALLEGRO_VERTEXptr); CDECL;
    ALLEGRO_FIRST_TRIANGLE_PROC = procedure (state: AL_UINTPTR_T; x, y, l1, l2: AL_INT); CDECL;
    ALLEGRO_DRAW_TRIANGLE_PROC = procedure (state: AL_UINTPTR_T; x1, y, x2: AL_INT); CDECL;

    ALLEGRO_FIRST_LINE_PROC = procedure (state: AL_UINTPTR_T; px, py: AL_INT; v1, v2: ALLEGRO_VERTEXptr); CDECL;
    ALLEGRO_DRAW_LINE_PROC = procedure (state: AL_UINTPTR_T; x, y: AL_INT); CDECL;

    ALLEGRO_STEP_PROC = procedure (state: AL_UINTPTR_T; atype: AL_INT); CDECL;

    ALLEGRO_SPLINE_CONTROL_POINTS = array [0..7] of AL_FLOAT;

  function al_get_allegro_primitives_version: AL_UINT32;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;

(*
 * Primary Functions
 *)
  function al_init_primitives_addon: AL_BOOL;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  function al_is_primitives_addon_initialized: AL_BOOL;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_shutdown_primitives_addon;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  function al_draw_prim (var vtxs: array of ALLEGRO_VERTEX; texture: ALLEGRO_BITMAPptr; start, finish: AL_INT; atype: ALLEGRO_PRIM_TYPE): AL_INT;
  function al_draw_prim_ex (const vtxs: AL_VOIDptr; const decl: ALLEGRO_VERTEX_DECLptr; texture: ALLEGRO_BITMAPptr; start, finish: AL_INT; atype: ALLEGRO_PRIM_TYPE): AL_INT;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_draw_prim';
  function al_draw_indexed_prim (var vtxs: array of ALLEGRO_VERTEX; texture: ALLEGRO_BITMAPptr; var indices: array of AL_INT; num_vtx: AL_INT; atype: ALLEGRO_PRIM_TYPE): AL_INT;
  function al_draw_indexed_prim_ex (const vtxs: AL_VOIDptr; const decl: ALLEGRO_VERTEX_DECLptr; texture: ALLEGRO_BITMAPptr; var indices: array of AL_INT; num_vtx: AL_INT; atype: ALLEGRO_PRIM_TYPE): AL_INT;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_draw_indexed_prim';
  function al_draw_vertex_buffer (vertex_buffer: ALLEGRO_VERTEX_BUFFERptr; texture: ALLEGRO_BITMAPptr; start, finish: AL_INT; atype: ALLEGRO_PRIM_TYPE): AL_INT;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  function al_draw_indexed_buffer (vertex_buffer: ALLEGRO_VERTEX_BUFFERptr; texture: ALLEGRO_BITMAPptr; index_buffer: ALLEGRO_INDEX_BUFFERptr; start, finish: AL_INT; atype: ALLEGRO_PRIM_TYPE): AL_INT;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;

  function al_create_vertex_decl (var elements: array of ALLEGRO_VERTEX_ELEMENT; stride: AL_INT): ALLEGRO_VERTEX_DECLptr;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_destroy_vertex_decl (decl: ALLEGRO_VERTEX_DECLptr);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;

(*
 * Vertex buffers
 *)
  function al_create_vertex_buffer (var initial_data: array of ALLEGRO_VERTEX; flags: ALLEGRO_PRIM_BUFFER_FLAGS): ALLEGRO_VERTEX_BUFFERptr;
  function al_create_vertex_buffer_ex (decl: ALLEGRO_VERTEX_DECLptr; const initial_data: AL_VOIDptr; num_vertices: AL_INT; flags: ALLEGRO_PRIM_BUFFER_FLAGS): ALLEGRO_VERTEX_BUFFERptr;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_create_vertex_buffer';
  procedure al_destroy_vertex_buffer (buffer: ALLEGRO_VERTEX_BUFFERptr);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  function al_lock_vertex_buffer (buffer: ALLEGRO_VERTEX_BUFFERptr; offset, length: AL_INT; flags: ALLEGRO_LOCK): AL_VOIDptr;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_unlock_vertex_buffer (buffer: ALLEGRO_VERTEX_BUFFERptr);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  function al_get_vertex_buffer_size (buffer: ALLEGRO_VERTEX_BUFFERptr): AL_INT;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;

(*
 * Index buffers
 *)
  function al_create_index_buffer (index_size: AL_INT; const initial_data: AL_VOIDptr; num_indices: AL_INT; flags: ALLEGRO_PRIM_BUFFER_FLAGS): ALLEGRO_INDEX_BUFFERptr;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_destroy_index_buffer (buffer: ALLEGRO_INDEX_BUFFERptr);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  function al_lock_index_buffer (buffer: ALLEGRO_INDEX_BUFFERptr; offset, length: AL_INT; flags: ALLEGRO_LOCK): AL_VOIDptr;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_unlock_index_buffer (buffer: ALLEGRO_INDEX_BUFFERptr);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  function al_get_index_buffer_size (buffer: ALLEGRO_INDEX_BUFFERptr): AL_INT;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;


(*
 * Utilities for high level primitives.
 *)
  function al_triangulate_polygon (var vertices: array of AL_FLOAT; svertex_stride: AL_SIZE_T; var vertex_counts: array of AL_INT; emit_triangle: ALLEGRO_EMIT_TRIANGLE_PROC; userdata: AL_VOIDptr): AL_BOOL;
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;



(*
 * Custom primitives
 *)
  procedure al_draw_soft_triangle (v1, v2, v3: ALLEGRO_VERTEXptr; state: AL_UINTPTR_T; init: ALLEGRO_INIT_TRIANGLE_PROC; first: ALLEGRO_FIRST_TRIANGLE_PROC; step: ALLEGRO_STEP_PROC; draw: ALLEGRO_DRAW_TRIANGLE_PROC);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_soft_line (v1, v2: ALLEGRO_VERTEXptr; state: AL_UINTPTR_T; first: ALLEGRO_FIRST_LINE_PROC; step: ALLEGRO_STEP_PROC; draw: ALLEGRO_DRAW_LINE_PROC);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;

(*
 * High level primitives
 *)
  procedure al_draw_line (x1, y1, x2, y2: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_triangle (x1, y1, x2, y2, x3, y3: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_rectangle (x1, y1, x2, y2: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_rounded_rectangle (x1, y1, x2, y2, rx, ry: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;

  procedure al_calculate_arc (dest: array of AL_FLOAT; cx, cy, rx, ry, start_theta, delta_theta, thickness: AL_FLOAT);
  procedure al_calculate_arc_ex (dest: AL_VOIDptr; stride: AL_INT; cx, cy, rx, ry, start_theta, delta_theta, thickness: AL_FLOAT; num_segments: AL_INT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_calculate_arc';
  procedure al_draw_circle (cx, cy, r: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_ellipse (cx, cy, rx, ry: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_arc (cx, cy, r, start_theta, delta_theta: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_elliptical_arc (cx, cy, rx, ry, start_theta, delta_theta: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_pieslice (cx, cy, r, start_theta, fdelta_theta: AL_FLOAT; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;

  procedure al_calculate_spline (dest: AL_FLOATptr; stride: AL_INT; var points: ALLEGRO_SPLINE_CONTROL_POINTS; thickness: AL_FLOAT; num_segments: AL_INT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_spline (var points: ALLEGRO_SPLINE_CONTROL_POINTS; color: ALLEGRO_COLOR; thickness: AL_FLOAT);
    CDECL;external ALLEGRO_PRIMITIVES_LIB_NAME;

  procedure al_calculate_ribbon (dest: AL_FLOATptr; dest_stride: AL_INT; var points: array of AL_FLOAT; points_stride: AL_INT; thickness: AL_FLOAT; num_segments: AL_INT);
    CDECL;external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_ribbon (const points: AL_FLOATptr; points_stride: AL_INT; color: ALLEGRO_COLOR; thickness: AL_FLOAT; num_segments: AL_INT);
    CDECL;external ALLEGRO_PRIMITIVES_LIB_NAME;

  procedure al_draw_filled_triangle (x1, y1, x2, y2, x3, y3: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_filled_rectangle (x1, y1, x2, y2: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_filled_ellipse (cx, cy, rx, ry: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_filled_circle (cx, cy, r: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_filled_pieslice (cx, cy, r, start_theta, fdelta_theta: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;
  procedure al_draw_filled_rounded_rectangle (x1, y1, x2, y2, rx, ry: AL_FLOAT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;

  procedure al_draw_polyline (var vertices: array of AL_FLOAT; join_style: ALLEGRO_LINE_JOIN; cap_style: ALLEGRO_LINE_CAP; color: ALLEGRO_COLOR; thickness, miter_limit: AL_FLOAT);
  procedure al_draw_polyline_ex (const vertices: AL_VOIDptr; vertex_stride, vertex_count: AL_INT; join_style: ALLEGRO_LINE_JOIN; cap_style: ALLEGRO_LINE_CAP; color: ALLEGRO_COLOR; thickness, miter_limit: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_draw_polyline';

  procedure al_draw_polygon (var vertices: array of AL_FLOAT; join_style: ALLEGRO_LINE_JOIN; color: ALLEGRO_COLOR; thickness, miter_limit: AL_FLOAT);
  procedure al_draw_filled_polygon (var vertices: array of AL_FLOAT; color: ALLEGRO_COLOR);
  procedure al_draw_filled_polygon_with_holes (var vertices: array of AL_FLOAT; var vertex_counts: array of AL_INT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME;



{ DO NOT USE ANY SYMBOL BELOW THIS COMMENT.  They're for internal use only.  In
  delphi, inline function declared in interface section must not use local
  symbols, that's why I've defined it here. }
{**@exclude}
  procedure _al_draw_polygon (const vertices: AL_FLOATptr; vertex_count: AL_INT; join_style: ALLEGRO_LINE_JOIN; color: ALLEGRO_COLOR; thickness, miter_limit: AL_FLOAT);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_draw_polygon';
{**@exclude}
  procedure _al_draw_filled_polygon (const vertices: AL_FLOATptr; vertex_count: AL_INT; color: ALLEGRO_COLOR);
    CDECL; external ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_draw_filled_polygon';
implementation

  function al_draw_prim (
    var vtxs: array of ALLEGRO_VERTEX;
    texture: ALLEGRO_BITMAPptr;
    start, finish: AL_INT;
    atype: ALLEGRO_PRIM_TYPE
  ): AL_INT;
  begin
    al_draw_prim := al_draw_prim_ex (
      @vtxs[0], Nil, texture, start, finish, atype
    )
  end;



  function al_draw_indexed_prim (
    var vtxs: array of ALLEGRO_VERTEX;
    texture: ALLEGRO_BITMAPptr;
    var indices: array of AL_INT;
    num_vtx: AL_INT;
    atype: ALLEGRO_PRIM_TYPE
  ): AL_INT;
  begin
    al_draw_indexed_prim := al_draw_indexed_prim_ex (
      @vtxs[0], Nil, texture, indices, num_vtx, atype
    )
  end;



  function al_create_vertex_buffer (
    var initial_data: array of ALLEGRO_VERTEX;
    flags: ALLEGRO_PRIM_BUFFER_FLAGS
  ): ALLEGRO_VERTEX_BUFFERptr;
  begin
    al_create_vertex_buffer := al_create_vertex_buffer_ex (
      Nil, @initial_data[0], Length (initial_data), flags
    )
  end;



  procedure al_calculate_arc (dest: array of AL_FLOAT; cx, cy, rx, ry, start_theta, delta_theta, thickness: AL_FLOAT);
  begin
    al_calculate_arc_ex (
      @dest[Low (dest)],
      2 * SizeOf (AL_FLOAT),
      cx, cy, rx, ry,
      start_theta, delta_theta,
      thickness,
      Length (dest) div 2
    )
  end;



  procedure al_draw_polyline (var vertices: array of AL_FLOAT; join_style: ALLEGRO_LINE_JOIN; cap_style: ALLEGRO_LINE_CAP; color: ALLEGRO_COLOR; thickness, miter_limit: AL_FLOAT);
  begin
    al_draw_polyline_ex (
      @vertices[Low (vertices)], SizeOf(AL_FLOAT) * 2, Length (vertices) div 2,
      join_style, cap_style, color, thickness, miter_limit
    )
  end;



  procedure al_draw_polygon (var vertices: array of AL_FLOAT; join_style: ALLEGRO_LINE_JOIN; color: ALLEGRO_COLOR; thickness, miter_limit: AL_FLOAT);
  begin
    _al_draw_polygon (@vertices[Low (vertices)], Length (vertices), join_style, color, thickness, miter_limit)
  end;



  procedure al_draw_filled_polygon (var vertices: array of AL_FLOAT; color: ALLEGRO_COLOR);
  begin
    _al_draw_filled_polygon (@vertices[Low (vertices)], Length (vertices), color)
  end;

end.
