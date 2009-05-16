PROGRAM ex3d;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *    This program demonstrates how to use the 3d matrix functions.
 *    It isn't a very elegant or efficient piece of code, but it
 *    does show the stuff in action. It is left to the reader as
 *    an exercise to design a proper model structure and rendering
 *    pipeline: after all, the best way to do that sort of stuff
 *    varies hugely from one game to another.
 *
 *    The example first shows a screen resolution selection dialog.
 *    Then, a number of bouncing 3d cubes are animated. Pressing
 *    a key modifies the rendering of the cubes, which can be
 *    wireframe, the more complex transparent perspective correct
 *    texture mapped version, and many other.
 *	by Ñuño Martínez <>
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See readme.txt for license and copyright information.
 *)

USES
  allegro, al3d, alfixed;



CONST
  NUM_SHAPES   = 8; (* number of bouncing cubes *)
  NUM_VERTICES = 8; (* a cube has eight corners *)
  NUM_FACES    = 6; (* a cube has six faces *)



TYPE
  VTXptr = ^VTX;
  VTX = RECORD
    x, y, z: AL_FIXED;
  END;

  VTX_LISTptr = ^VTX_LIST;
  VTX_LIST = ARRAY OF VTX;



  QUAD = RECORD     (* four vertices makes a quad *)
    vtxlist: VTX_LISTptr;
    v1, v2, v3, v4: INTEGER;
  END;


  SHAPE = RECORD    (* store position of a shape *)
    x, y, z: AL_FIXED;      (* x, y, z position *)
    rx, ry, rz: AL_FIXED;   (* rotations *)
    dz: AL_FIXED;           (* speed of movement *)
    drx, dry, drz: AL_FIXED;(* speed of rotation *)
  END;



VAR
  Points: ARRAY [1..8] OF VTX; (* a cube, centered on the origin *)

  Faces: ARRAY [1..6] OF QUAD; (* group the vertices into polygons *)

  Shapes: ARRAY [1..NUM_SHAPES] OF SHAPE; (* a list of shapes *)


(* somewhere to put translated vertices *)
  output_points: ARRAY [1..NUM_VERTICES * NUM_SHAPES] OF VTX;
  output_faces: ARRAY [1..NUM_FACES * NUM_SHAPES] OF QUAD;

TYPE
  TRenderMode = (
    wireframe,
    flat,
    gcol,
    grgb,
    atex,
    ptex,
    atex_mask,
    ptex_mask,
    atex_lit,
    ptex_lit,
    atex_mask_lit,
    ptex_mask_lit,
    atex_trans,
    ptex_trans,
    atex_mask_trans,
    ptex_mask_trans,
    last_mode
  );
VAR
  render_mode: TRenderMode = wireframe;

  render_type: ARRAY [1..16] OF LONGINT = (
    0,
    AL_POLYTYPE_FLAT,
    AL_POLYTYPE_GCOL,
    AL_POLYTYPE_GRGB,
    AL_POLYTYPE_ATEX,
    AL_POLYTYPE_PTEX,
    AL_POLYTYPE_ATEX_MASK,
    AL_POLYTYPE_PTEX_MASK,
    AL_POLYTYPE_ATEX_LIT,
    AL_POLYTYPE_PTEX_LIT,
    AL_POLYTYPE_ATEX_MASK_LIT,
    AL_POLYTYPE_PTEX_MASK_LIT,
    AL_POLYTYPE_ATEX_TRANS,
    AL_POLYTYPE_PTEX_TRANS,
    AL_POLYTYPE_ATEX_MASK_TRANS,
    AL_POLYTYPE_PTEX_MASK_TRANS
  );


  mode_desc: ARRAY [1..16] OF STRING = (
    'Wireframe',
    'Flat shaded',
    'Single color Gouraud shaded',
    'Gouraud shaded',
    'Texture mapped',
    'Perspective correct texture mapped',
    'Masked texture mapped',
    'Masked persp. correct texture mapped',
    'Lit texture map',
    'Lit persp. correct texture map',
    'Masked lit texture map',
    'Masked lit persp. correct texture map',
    'Transparent texture mapped',
    'Transparent perspective correct texture mapped',
    'Transparent masked texture mapped',
    'Transparent masked persp. correct texture mapped'
  );


  Texture: AL_BITMAPptr;



(* initialise shape positions *)
  PROCEDURE InitShapes;

    PROCEDURE AssignPoint (Index, x, y, z: INTEGER);
    BEGIN
      Points[Index].x := al_itofix (x);
      Points[Index].y := al_itofix (y);
      Points[Index].z := al_itofix (z);
    END;

    PROCEDURE AssignFace (Index, v1, v2, v3, v4: INTEGER);
    BEGIN
      Faces[Index].vtxlist := @Points;
      Faces[Index].v1 := v1;
      Faces[Index].v2 := v2;
      Faces[Index].v3 := v3;
      Faces[Index].v4 := v4;
    END;

  VAR
    Cnt: INTEGER;
  BEGIN
  { vertices of the cube }
    AssignPoint (1, -32, -32, -32);
    AssignPoint (2, -32,  32, -32);
    AssignPoint (3,  32,  32, -32);
    AssignPoint (4,  32, -32, -32);
    AssignPoint (5, -32, -32,  32);
    AssignPoint (6, -32,  32,  32);
    AssignPoint (7,  32,  32,  32);
    AssignPoint (8,  32, -32,  32);
  { faces of the cube. }
    AssignFace (1, 0, 3, 2, 1);
    AssignFace (2, 4, 5, 6, 7);
    AssignFace (3, 0, 1, 5, 4);
    AssignFace (4, 2, 3, 7, 6);
    AssignFace (5, 0, 4, 7, 3);
    AssignFace (6, 1, 2, 6, 5);
  { Initial positions. }
    FOR Cnt := 0 TO NUM_SHAPES DO
    BEGIN
      shapes[Cnt].x := al_itofix (Random (255) - 128);
      shapes[Cnt].y := al_itofix (Random (255) - 128);
      shapes[Cnt].z := al_itofix (768);
      shapes[Cnt].rx := 0;
      shapes[Cnt].ry := 0;
      shapes[Cnt].rz := 0;
      shapes[Cnt].dz :=  (Random (255) - 8) SHL 12;
      shapes[Cnt].drx := (Random (31) - 16) SHL 12;
      shapes[Cnt].dry := (Random (31) - 16) SHL 12;
      shapes[Cnt].drz := (Random (31) - 16) SHL 12;
    END;
  END;



(* update shape positions *)
  PROCEDURE AnimateShapes;
  VAR
    Cnt: INTEGER;
  BEGIN
    FOR Cnt := 0 TO NUM_SHAPES DO
    BEGIN
      INC (shapes[Cnt].z, shapes[Cnt].dz);
      IF (shapes[Cnt].z > al_itofix (1024))
      OR (shapes[Cnt].z < al_itofix (192)) THEN
	shapes[Cnt].dz := -shapes[Cnt].dz;
      INC (shapes[Cnt].rx, shapes[Cnt].drx);
      INC (shapes[Cnt].ry, shapes[Cnt].dry);
      INC (shapes[Cnt].rz, shapes[Cnt].drz);
    END;
  END;



(* translate shapes from 3d world space to 2d screen space *)
  PROCEDURE TranslateShapes;
  VAR
    c, d: LONGINT;
    Matrix: AL_MATRIX;
    outpoint, outface: INTEGER;
  BEGIN
    outpoint := 1; outface := 1;
    FOR c := 1 TO NUM_SHAPES DO
    BEGIN
    { build a transformation matrix }
      al_get_transformation_matrix(@Matrix, al_itofix (1),
				shapes[c].rx, shapes[c].ry, shapes[c].rz,
				shapes[c].x, shapes[c].y, shapes[c].z);
    { output the vertices }
      FOR d := 0 TO (NUM_VERTICES - 1) DO
      BEGIN
	al_apply_matrix (@Matrix, points[d].x, points[d].y, points[d].z,
		@output_points[outpoint + d].x, @output_points[outpoint + d].y, @output_points[outpoint + d].z);
	al_persp_project (output_points[outpoint + d].x, output_points[outpoint + d].y, output_points[outpoint + d].z,
		output_points[outpoint + d].x, output_points[outpoint + d].y);
      END;
    { output the faces }
      FOR d := 1 TO NUM_FACES DO
      BEGIN
	output_faces[outface + d] := faces[d];
	output_faces[outface + d].vtxlist := @output_points[outpoint];
      END;
      INC (outpoint, NUM_VERTICES);
      INC (outface, NUM_FACES);
    END;
  END;



(* Returns the median of v1, v2 and v3. *)
  FUNCTION CLAMP (v1, v2, v3: LONGINT): LONGINT; INLINE;
  VAR
    Tmp: LONGINT;
  BEGIN
    IF v2 < v3 THEN
      Tmp := v2
    ELSE
      Tmp := v3;
    IF v1 > Tmp THEN
      CLAMP := v1
    ELSE
      CLAMP := Tmp;
  END;



(* draw a line (for wireframe display) *)
  PROCEDURE DrawWire (b: AL_BITMAPptr; v1, v2: VTXptr);
  VAR
    Col: LONGINT;
  BEGIN
    Col := CLAMP (128, 255 - al_fixtoi (v1^.z + v2^.z) DIV 16, 255);
    al_line (b, al_fixtoi (v1^.x), al_fixtoi (v1^.y), al_fixtoi (v2^.x), al_fixtoi (v2^.y),
	al_palette_color[Col]);
  END;



(* draw a quad *)
  PROCEDURE DrawQuad (b: AL_BITMAPptr; v1, v2, v3, v4: VTXptr; mode: LONGINT);

    PROCEDURE AssignVertex (VAR Vtx: AL_V3D; x, y, z, u, v, c: LONGINT); INLINE;
    BEGIN
      Vtx.x := al_itofix (x);
      Vtx.y := al_itofix (y);
      Vtx.x := al_itofix (z);
      Vtx.u := al_itofix (u);
      Vtx.v := al_itofix (v);
      Vtx.c := c;
    END;

  VAR
    Col: LONGINT;
  { four vertices }
    vtx1, vtx2, vtx3, vtx4: AL_V3D;
  BEGIN
   AssignVertex (vtx1, v1^.x, v1^.y, v1^.z,  0,  0, 0);
   AssignVertex (vtx2, v2^.x, v2^.y, v2^.z, 32,  0, 0);
   AssignVertex (vtx3, v3^.x, v3^.y, v3^.z, 32, 32, 0);
   AssignVertex (vtx4, v4^.x, v4^.y, v4^.z,  0, 32, 0);
 { cull backfaces }
   IF  (mode <> AL_POLYTYPE_ATEX_MASK) AND (mode <> AL_POLYTYPE_PTEX_MASK)
   AND (mode <> AL_POLYTYPE_ATEX_MASK_LIT) AND (mode <> AL_POLYTYPE_PTEX_MASK_LIT)
   AND (al_polygon_z_normal (@vtx1, @vtx2, @vtx3) < 0)
   THEN
     EXIT;
  { set up the vertex color, differently for each rendering mode }
    CASE (mode) OF
      AL_POLYTYPE_FLAT: BEGIN
	col := CLAMP (128, 255 - al_fixtoi (v1^.z + v2^.z) DIV 16, 255);
	vtx1.c := al_palette_color[col];
	vtx2.c := al_palette_color[col];
	vtx3.c := al_palette_color[col];
	vtx4.c := al_palette_color[col];
      END;
      AL_POLYTYPE_GCOL: BEGIN
	vtx1.c := al_palette_color[$D0];
	vtx2.c := al_palette_color[$80];
	vtx3.c := al_palette_color[$B0];
	vtx4.c := al_palette_color[$FF];
      END;
      AL_POLYTYPE_GRGB: BEGIN
	vtx1.c := $000000;
	vtx2.c := $7F0000;
	vtx3.c := $FF0000;
	vtx4.c := $7F0000;
      END
      ELSE BEGIN
	vtx1.c := CLAMP (0, 255 - al_fixtoi (v1^.z) DIV 4, 255);
	vtx2.c := CLAMP (0, 255 - al_fixtoi (v2^.z) DIV 4, 255);
	vtx3.c := CLAMP (0, 255 - al_fixtoi (v3^.z) DIV 4, 255);
	vtx4.c := CLAMP (0, 255 - al_fixtoi (v4^.z) DIV 4, 255);
      END;
    END;
  { draw the quad }
    al_quad3d (b, mode, texture, @vtx1, @vtx2, @vtx3, @vtx4);
  END;



/* draw the shapes calculated by translate_shapes() */
void draw_shapes(BITMAP *b)
{
   int c;
   QUAD *face = output_faces;
   VTX *v1, *v2, *v3, *v4;

   /* depth sort */
   qsort(output_faces, NUM_FACES * NUM_SHAPES, sizeof(QUAD), quad_cmp);

   for (c=0; c < NUM_FACES * NUM_SHAPES; c++) {
      /* find the vertices used by the face */
      v1 = face->vtxlist + face->v1;
      v2 = face->vtxlist + face->v2;
      v3 = face->vtxlist + face->v3;
      v4 = face->vtxlist + face->v4;

      /* draw the face */
      if (render_mode == wireframe) {
	 wire(b, v1, v2);
	 wire(b, v2, v3);
	 wire(b, v3, v4);
	 wire(b, v4, v1);
      }
      else {
	 draw_quad(b, v1, v2, v3, v4, render_type[render_mode]);
      }

      face++;
   }
}



/* RGB -> color mapping table. Not needed, but speeds things up */
RGB_MAP rgb_table;


/* lighting color mapping table */
COLOR_MAP light_table;

/* transparency color mapping table */
COLOR_MAP trans_table;



int main(void)
{
   BITMAP *buffer;
   PALETTE pal;
   int c, w, h, bpp;
   int last_retrace_count;

   if (allegro_init() != 0)
      return 1;
   install_keyboard();
   install_mouse();
   install_timer();

   /* color 0 = black */
   pal[0].r = pal[0].g = pal[0].b = 0;

   /* copy the desktop palette */
   for (c=1; c<64; c++)
      pal[c] = desktop_palette[c];

   /* make a red gradient */
   for (c=64; c<96; c++) {
      pal[c].r = (c-64)*2;
      pal[c].g = pal[c].b = 0;
   }

   /* make a green gradient */
   for (c=96; c<128; c++) {
      pal[c].g = (c-96)*2;
      pal[c].r = pal[c].b = 0;
   }

   /* set up a greyscale in the top half of the palette */
   for (c=128; c<256; c++)
      pal[c].r = pal[c].g = pal[c].b = (c-128)/2;

   /* build rgb_map table */
   create_rgb_table(&rgb_table, pal, NULL);
   rgb_map = &rgb_table;

   /* build a lighting table */
   create_light_table(&light_table, pal, 0, 0, 0, NULL);
   color_map = &light_table;

   /* build a transparency table */
   /* textures are 25% transparent (75% opaque) */
   create_trans_table(&trans_table, pal, 192, 192, 192, NULL);

   /* set up the truecolor blending functions */
   /* textures are 25% transparent (75% opaque) */
   set_trans_blender(0, 0, 0, 192);

   /* set the graphics mode */
   if (set_gfx_mode(GFX_SAFE, 320, 200, 0, 0) != 0) {
      set_gfx_mode(GFX_TEXT, 0, 0, 0, 0);
      allegro_message("Unable to set any graphic mode\n%s\n", allegro_error);
      return 1;
   }
   set_palette(desktop_palette);

   c = GFX_AUTODETECT;
   w = SCREEN_W;
   h = SCREEN_H;
   bpp = bitmap_color_depth(screen);
   if (!gfx_mode_select_ex(&c, &w, &h, &bpp)) {
      allegro_exit();
      return 1;
   }

   set_color_depth(bpp);

   if (set_gfx_mode(c, w, h, 0, 0) != 0) {
      set_gfx_mode(GFX_TEXT, 0, 0, 0, 0);
      allegro_message("Error setting graphics mode\n%s\n", allegro_error);
      return 1;
   }

   set_palette(pal);

   /* make a bitmap for use as a texture map */
   texture = create_bitmap(32, 32);
   clear_to_color(texture, bitmap_mask_color(texture));
   line(texture, 0, 0, 31, 31, palette_color[1]);
   line(texture, 0, 31, 31, 0, palette_color[1]);
   rect(texture, 0, 0, 31, 31, palette_color[1]);
   textout_ex(texture, font, "dead", 0, 0, palette_color[2], -1);
   textout_ex(texture, font, "pigs", 0, 8, palette_color[2], -1);
   textout_ex(texture, font, "cant", 0, 16, palette_color[2], -1);
   textout_ex(texture, font, "fly.", 0, 24, palette_color[2], -1);

   /* double buffer the animation */
   buffer = create_bitmap(SCREEN_W, SCREEN_H);

   /* set up the viewport for the perspective projection */
   set_projection_viewport(0, 0, SCREEN_W, SCREEN_H);

   /* initialise the bouncing shapes */
   init_shapes();

   last_retrace_count = retrace_count;

   for (;;) {
      clear_bitmap(buffer);

      while (last_retrace_count < retrace_count) {
	 animate_shapes();
	 last_retrace_count++;
      }

      translate_shapes();
      draw_shapes(buffer);

      textprintf_ex(buffer, font, 0, 0, palette_color[192], -1, "%s, %d bpp",
		    mode_desc[render_mode], bitmap_color_depth(screen));
      textout_ex(buffer, font, "Press a key to change", 0, 12,
		 palette_color[192], -1);

      vsync();
      blit(buffer, screen, 0, 0, 0, 0, SCREEN_W, SCREEN_H); 

      if (keypressed()) {
	 if ((readkey() & 0xFF) == 27)
	    break;
	 else {
	    render_mode++;
	    if (render_mode >= last_mode) {
	       render_mode = wireframe;
	       color_map = &light_table;
	    }
	    if (render_type[render_mode] >= POLYTYPE_ATEX_TRANS)
	       color_map = &trans_table;
	 }
      }
   }

   destroy_bitmap(buffer);
   destroy_bitmap(texture);

   return 0;
}

END_OF_MAIN()
