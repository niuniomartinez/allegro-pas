UNIT al5primitives;
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
 *      See readme.txt for copyright information.
 *)

{$include allegro.cfg}

INTERFACE

  USES
    Allegro5;

{$include allegro.inc}

  TYPE
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
      ALLEGRO_PRIM_ATTR_NONE := 0,
      ALLEGRO_PRIM_POSITION := 1,
      ALLEGRO_PRIM_COLOR_ATTR,
      ALLEGRO_PRIM_TEX_COORD,
      ALLEGRO_PRIM_TEX_COORD_PIXEL,
      ALLEGRO_PRIM_ATTR_NUM
    );



    ALLEGRO_PRIM_STORAGE = (
      ALLEGRO_PRIM_FLOAT_2 := 0,
      ALLEGRO_PRIM_FLOAT_3,
      ALLEGRO_PRIM_SHORT_2
    );

  CONST
    ALLEGRO_PRIM_STORAGE_NONE = ALLEGRO_PRIM_FLOAT_2;
    ALLEGRO_VERTEX_CACHE_SIZE = 256;



    ALLEGRO_PRIM_QUALITY = 10;

  TYPE
    ALLEGRO_VERTEX_ELEMENTptr = ^ALLEGRO_VERTEX_ELEMENT;
    ALLEGRO_VERTEX_ELEMENT = RECORD
      attribute: ALLEGRO_PRIM_ATTR;
      storage: ALLEGRO_PRIM_STORAGE;
      offset: LONGINT;
    END;


    ALLEGRO_VERTEX_DECLptr = ^POINTER;


    ALLEGRO_VERTEXptr = ^ALLEGRO_VERTEX;
    ALLEGRO_VERTEX = RECORD
      x, y, z: SINGLE;
      u, v: SINGLE;
      color: ALLEGRO_COLOR;
    END;



    ALLEGRO_INIT_TRIANGLE_PROC = PROCEDURE (state: PLONGWORD; v1, v2, v3: ALLEGRO_VERTEXptr); CDECL;
    ALLEGRO_FIRST_TRIANGLE_PROC = PROCEDURE (state: PLONGWORD; x, y, l1, l2: LONGINT); CDECL;
    ALLEGRO_DRAW_TRIANGLE_PROC = PROCEDURE (state: PLONGWORD; x1, y, x2: LONGINT); CDECL;

    ALLEGRO_FIRST_LINE_PROC = PROCEDURE (state: PLONGWORD; px, py: LONGINT; v1, v2: ALLEGRO_VERTEXptr); CDECL;
    ALLEGRO_DRAW_LINE_PROC = PROCEDURE (state: PLONGWORD; x, y: LONGINT); CDECL;

    ALLEGRO_STEP_PROC = PROCEDURE (state: PLONGWORD; _type: LONGINT); CDECL;

    ALLEGRO_SPLINE_CONTROL_POINTS = ARRAY [0..7] OF SINGLE;

  FUNCTION al_get_allegro_primitives_version: LONGWORD; CDECL;

(* Primary Functions *)
  FUNCTION al_init_primitives_addon: BOOLEAN; CDECL;
  PROCEDURE al_shutdown_primitives_addon; CDECL;
  FUNCTION al_draw_prim (CONST vtxs: POINTER; CONST decl: ALLEGRO_VERTEX_DECLptr; texture: ALLEGRO_BITMAPptr; start, finish: LONGINT; _type: ALLEGRO_PRIM_TYPE): LONGINT; CDECL;
  FUNCTION al_draw_indexed_prim (CONST vtxs: POINTER; CONST decl: ALLEGRO_VERTEX_DECLptr; texture: ALLEGRO_BITMAPptr; CONST indices: ARRAY OF LONGINT; num_vtx: LONGINT; _type: ALLEGRO_PRIM_TYPE): LONGINT; INLINE; {TODO: Need to test if the "indices" parameter does work correctly }

  FUNCTION al_create_vertex_decl (CONST elements: ALLEGRO_VERTEX_ELEMENTptr; stride: LONGINT): ALLEGRO_VERTEX_DECLptr; CDECL;
  PROCEDURE al_destroy_vertex_decl (decl: ALLEGRO_VERTEX_DECLptr); CDECL;

(* Custom primitives *)
  PROCEDURE al_draw_soft_triangle (v1, v2, v3: ALLEGRO_VERTEXptr; state: PLONGWORD; init: ALLEGRO_INIT_TRIANGLE_PROC; first: ALLEGRO_FIRST_TRIANGLE_PROC; step: ALLEGRO_STEP_PROC; draw: ALLEGRO_DRAW_TRIANGLE_PROC); CDECL;
  PROCEDURE al_draw_soft_line (v1, v2: ALLEGRO_VERTEXptr; state: PLONGWORD; first: ALLEGRO_FIRST_LINE_PROC; step: ALLEGRO_STEP_PROC; draw: ALLEGRO_DRAW_LINE_PROC); CDECL;

(* High level primitives *)
  PROCEDURE al_draw_line (x1, y1, x2, y2: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  PROCEDURE al_draw_triangle (x1, y1, x2, y2, x3, y3: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  PROCEDURE al_draw_rectangle (x1, y1, x2, y2: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  PROCEDURE al_draw_rounded_rectangle (x1, y1, x2, y2, rx, ry: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;

  PROCEDURE al_calculate_arc (dest: PSINGLE; stride: LONGINT; cx, cy, rx, ry, start_theta, delta_theta, thickness: SINGLE; num_segments: LONGINT); CDECL;
  PROCEDURE al_draw_circle (cx, cy, r: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  PROCEDURE al_draw_ellipse (cx, cy, rx, ry: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  PROCEDURE al_draw_arc (cx, cy, r, start_theta, delta_theta: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;

  PROCEDURE al_calculate_spline (dest: PSINGLE; stride: LONGINT; points: ALLEGRO_SPLINE_CONTROL_POINTS; thickness: SINGLE; num_segments: LONGINT); CDECL;
  PROCEDURE al_draw_spline (points: ALLEGRO_SPLINE_CONTROL_POINTS; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;

  PROCEDURE al_calculate_ribbon (dest: PSINGLE; dest_stride: LONGINT; CONST points: ARRAY OF SINGLE; points_stride: LONGINT; thickness: SINGLE; num_segments: LONGINT); INLINE;
  PROCEDURE al_draw_ribbon (CONST points: ARRAY OF SINGLE; points_stride: LONGINT; color: ALLEGRO_COLOR; thickness: SINGLE; num_segments: LONGINT); INLINE;

  PROCEDURE al_draw_filled_triangle (x1, y1, x2, y2, x3, y3: SINGLE; color: ALLEGRO_COLOR); CDECL;
  PROCEDURE al_draw_filled_rectangle (x1, y1, x2, y2: SINGLE; color: ALLEGRO_COLOR); CDECL;
  PROCEDURE al_draw_filled_ellipse (cx, cy, rx, ry: SINGLE; color: ALLEGRO_COLOR); CDECL;
  PROCEDURE al_draw_filled_circle (cx, cy, r: SINGLE; color: ALLEGRO_COLOR); CDECL;
  PROCEDURE al_draw_filled_rounded_rectangle (x1, y1, x2, y2, rx, ry: SINGLE; color: ALLEGRO_COLOR); CDECL;

IMPLEMENTATION

  FUNCTION al_get_allegro_primitives_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

(* Primary Functions *)
  FUNCTION al_init_primitives_addon: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_shutdown_primitives_addon; CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  FUNCTION al_draw_prim (CONST vtxs: POINTER; CONST decl: ALLEGRO_VERTEX_DECLptr; texture: ALLEGRO_BITMAPptr; start, finish: LONGINT; _type: ALLEGRO_PRIM_TYPE): LONGINT; CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  FUNCTION _al_draw_indexed_prim_ (CONST vtxs: POINTER; CONST decl: ALLEGRO_VERTEX_DECLptr; texture: ALLEGRO_BITMAPptr; CONST indices: PLONGINT; num_vtx: LONGINT; _type: ALLEGRO_PRIM_TYPE): LONGINT; CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_draw_indexed_prim';

  FUNCTION al_draw_indexed_prim (CONST vtxs: POINTER; CONST decl: ALLEGRO_VERTEX_DECLptr; texture: ALLEGRO_BITMAPptr; CONST indices: ARRAY OF LONGINT; num_vtx: LONGINT; _type: ALLEGRO_PRIM_TYPE): LONGINT;
  BEGIN
    al_draw_indexed_prim := _al_draw_indexed_prim_ (vtxs, decl, texture, @indices[0], num_vtx, _type);
  END;

  FUNCTION al_create_vertex_decl (CONST elements: ALLEGRO_VERTEX_ELEMENTptr; stride: LONGINT): ALLEGRO_VERTEX_DECLptr; CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_destroy_vertex_decl (decl: ALLEGRO_VERTEX_DECLptr); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

(* Custom primitives *)
  PROCEDURE al_draw_soft_triangle (v1, v2, v3: ALLEGRO_VERTEXptr; state: PLONGWORD; init: ALLEGRO_INIT_TRIANGLE_PROC; first: ALLEGRO_FIRST_TRIANGLE_PROC; step: ALLEGRO_STEP_PROC; draw: ALLEGRO_DRAW_TRIANGLE_PROC); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_soft_line (v1, v2: ALLEGRO_VERTEXptr; state: PLONGWORD; first: ALLEGRO_FIRST_LINE_PROC; step: ALLEGRO_STEP_PROC; draw: ALLEGRO_DRAW_LINE_PROC); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

(* High level primitives *)
  PROCEDURE al_draw_line (x1, y1, x2, y2: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_triangle (x1, y1, x2, y2, x3, y3: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_rectangle (x1, y1, x2, y2: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_rounded_rectangle (x1, y1, x2, y2, rx, ry: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_calculate_arc (dest: PSINGLE; stride: LONGINT; cx, cy, rx, ry, start_theta, delta_theta, thickness: SINGLE; num_segments: LONGINT); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_circle (cx, cy, r: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_ellipse (cx, cy, rx, ry: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_arc (cx, cy, r, start_theta, delta_theta: SINGLE; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_calculate_spline (dest: PSINGLE; stride: LONGINT; points: ALLEGRO_SPLINE_CONTROL_POINTS; thickness: SINGLE; num_segments: LONGINT); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_spline (points: ALLEGRO_SPLINE_CONTROL_POINTS; color: ALLEGRO_COLOR; thickness: SINGLE); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE _al_calculate_ribbon_ (dest: PSINGLE; dest_stride: LONGINT; CONST points: PSINGLE; points_stride: LONGINT; thickness: SINGLE; num_segments: LONGINT); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_calculate_ribbon';

  PROCEDURE al_calculate_ribbon (dest: PSINGLE; dest_stride: LONGINT; CONST points: ARRAY OF SINGLE; points_stride: LONGINT; thickness: SINGLE; num_segments: LONGINT);
  BEGIN
    _al_calculate_ribbon_ (dest, dest_stride, @points[0], points_stride, thickness, num_segments);
  END;

  PROCEDURE _al_draw_ribbon_ (CONST points: PSINGLE; points_stride: LONGINT; color: ALLEGRO_COLOR; thickness: SINGLE; num_segments: LONGINT); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME NAME 'al_draw_ribbon';

  PROCEDURE al_draw_ribbon (CONST points: ARRAY OF SINGLE; points_stride: LONGINT; color: ALLEGRO_COLOR; thickness: SINGLE; num_segments: LONGINT);
  BEGIN
    _al_draw_ribbon_ (@points[0], points_stride, color, thickness, num_segments);
  END;

  PROCEDURE al_draw_filled_triangle (x1, y1, x2, y2, x3, y3: SINGLE; color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_filled_rectangle (x1, y1, x2, y2: SINGLE; color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_filled_ellipse (cx, cy, rx, ry: SINGLE; color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_filled_circle (cx, cy, r: SINGLE; color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

  PROCEDURE al_draw_filled_rounded_rectangle (x1, y1, x2, y2, rx, ry: SINGLE; color: ALLEGRO_COLOR); CDECL;
  EXTERNAL ALLEGRO_PRIMITIVES_LIB_NAME;

END.
