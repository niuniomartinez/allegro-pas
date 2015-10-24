PROGRAM exstars;
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
 *
 *	This program draws a 3D star field (depth-cued) and a polygon
 *	starship (controllable with the keyboard cursor keys), using
 *	the Allegro math functions.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro library, by Dave Thomson.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro, al3d, alfixed,
    sysutils;



(* Star field system. *)
  TYPE
    VECTOR = RECORD
      x, y, z: AL_FIXED;
    END;

  CONST
    NUM_STARS   = 512;
    Z_NEAR      = 24;
    Z_FAR       = 1024;
    XY_CUBE     = 2048;
    SPEED_LIMIT = 20;

  VAR
    Stars: ARRAY [1..NUM_STARS] OF VECTOR;
    StarX, StarY: ARRAY [1..NUM_STARS] OF AL_FIXED;
    Delta: VECTOR;



(* Polygonal models. *)
  CONST
    NUM_VERTS = 4;
    NUM_FACES = 4;

    ENGINE = 4;  { Which face is the engine. }
    ENGINE_ON  = 64; { Color index. }
    ENGINE_OFF = 32;

  TYPE
  (* For triangular models. *)
    FACE = RECORD
      v1, v2, v3: INTEGER;
      colour, range: INTEGER;
      normal, rnormal: VECTOR;
    END;

    MODEL = RECORD
      points: ARRAY [1..NUM_VERTS] OF VECTOR;
      faces: ARRAY  [1..NUM_FACES] OF FACE;
      x, y, z: AL_FIXED;
      rx, ry, rz: AL_FIXED;
      minx, miny, maxx, maxy: INTEGER;
      aim: VECTOR;
      velocity: INTEGER;
    END;



  VAR
    Ship: MODEL;
    Direction: VECTOR;

    Buffer: AL_BITMAPptr;



(* Initialises the star field system. *)
  PROCEDURE InitStars;
  VAR
    Ndx: INTEGER;
  BEGIN
    FOR Ndx := Low (Stars) TO High (Stars) DO
    BEGIN
      Stars[Ndx].x := al_itofix (random (XY_CUBE) - (XY_CUBE SHR 1));
      Stars[Ndx].y := al_itofix (random (XY_CUBE) - (XY_CUBE SHR 1));
      Stars[Ndx].z := al_itofix (random (Z_FAR - Z_NEAR) + Z_NEAR);
    END;
    Delta.x := 0;
    Delta.y := 0;
    Delta.z := 0;
  END;



(* Draws the star field. *)
  PROCEDURE DrawStars;
  VAR
    Ndx, Clr: INTEGER;
    Matrix: AL_MATRIX;
    Outs: ARRAY [1..NUM_STARS] OF VECTOR;
  BEGIN
    FOR Ndx := Low (Stars) TO High (Stars) DO
    BEGIN
      al_get_translation_matrix (Matrix, Delta.x, Delta.y, Delta.z);
      al_apply_matrix (Matrix, Stars[Ndx].x, Stars[Ndx].y, Stars[Ndx].z,
		       Outs[Ndx].x, Outs[Ndx].y, Outs[Ndx].z);
      al_persp_project (Outs[Ndx].x, Outs[Ndx].y, Outs[Ndx].z,
			StarX[Ndx], StarY[Ndx]);
      Clr := (al_fixtoi (Outs[Ndx].z) SHR 8) + 16;
      al_putpixel (Buffer, al_fixtoi (StarX[Ndx]), al_fixtoi (StarY[Ndx]),
		   al_palette_color^[Clr]);
    END;
  END;



(* Deletes the stars from the screen. *)
  PROCEDURE EraseStars;
  VAR
    Ndx: INTEGER;
  BEGIN
    FOR Ndx := Low (StarX) TO High (StarX) DO
      al_putpixel (Buffer, al_fixtoi (StarX[Ndx]), al_fixtoi (StarY[Ndx]),
		   al_palette_color^[0]);
  END;



(* Moves the stars. *)
  PROCEDURE MoveStars;
  VAR
    Ndx: INTEGER;
  BEGIN
    FOR Ndx := Low (Stars) TO High (Stars) DO
    BEGIN
      INC (Stars[Ndx].x, Delta.x);
      INC (Stars[Ndx].y, Delta.y);
      INC (Stars[Ndx].z, Delta.z);

      IF Stars[Ndx].x > al_itofix (XY_CUBE SHR 1) THEN
	Stars[Ndx].x := al_itofix (-(XY_CUBE SHR 1))
      ELSE IF Stars[Ndx].x < al_itofix (-(XY_CUBE SHR 1)) THEN
	Stars[Ndx].x := al_itofix (XY_CUBE SHR 1);

      IF Stars[Ndx].y > al_itofix (XY_CUBE SHR 1) THEN
	Stars[Ndx].y := al_itofix (-(XY_CUBE SHR 1))
      ELSE IF Stars[Ndx].y < al_itofix (-(XY_CUBE SHR 1)) THEN
	Stars[Ndx].y := al_itofix (XY_CUBE SHR 1);

      IF Stars[Ndx].z > al_itofix (Z_FAR) THEN
	Stars[Ndx].z := al_itofix (Z_NEAR)
      ELSE IF Stars[Ndx].z < al_itofix (Z_NEAR) THEN
	Stars[Ndx].z := al_itofix (Z_FAR);
    END;
  END;



(* Initialises the ship model. *)
  PROCEDURE InitShip;
  VAR
    v1, v2: VECTOR;
    pts: ^VECTOR;
    aFace: ^FACE;
    Ndx: INTEGER;
  BEGIN
    Ship.points[1].x := al_itofix(0);
    Ship.points[1].y := al_itofix(0);
    Ship.points[1].z := al_itofix(32);

    Ship.points[2].x := al_itofix(16);
    Ship.points[2].y := al_itofix(-16);
    Ship.points[2].z := al_itofix(-32);

    Ship.points[3].x := al_itofix(-16);
    Ship.points[3].y := al_itofix(-16);
    Ship.points[3].z := al_itofix(-32);

    Ship.points[4].x := al_itofix(0);
    Ship.points[4].y := al_itofix(16);
    Ship.points[4].z := al_itofix(-32);

    Ship.faces[1].v1 := 4;
    Ship.faces[1].v2 := 1;
    Ship.faces[1].v3 := 2;
    pts := @Ship.points[1];
    aFace := @Ship.faces[1];
  { NOTE: Next lines should build the facet normal, but they don't! }
    v1.x := (pts[aFace^.v2].x - pts[aFace^.v1].x);
    v1.y := (pts[aFace^.v2].y - pts[aFace^.v1].y);
    v1.z := (pts[aFace^.v2].z - pts[aFace^.v1].z);
    v2.x := (pts[aFace^.v3].x - pts[aFace^.v2].x);
    v2.y := (pts[aFace^.v3].y - pts[aFace^.v2].y);
    v2.z := (pts[aFace^.v3].z - pts[aFace^.v2].z);
    al_cross_product (v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
		aFace^.normal.x, aFace^.normal.y, aFace^.normal.z);

    Ship.faces[2].v1 := 3;
    Ship.faces[2].v2 := 1;
    Ship.faces[2].v3 := 4;
    aFace := @Ship.faces[2];
  { NOTE: Next lines should build the facet normal, but they don't! }
    v1.x := (pts[aFace^.v2].x - pts[aFace^.v1].x);
    v1.y := (pts[aFace^.v2].y - pts[aFace^.v1].y);
    v1.z := (pts[aFace^.v2].z - pts[aFace^.v1].z);
    v2.x := (pts[aFace^.v3].x - pts[aFace^.v2].x);
    v2.y := (pts[aFace^.v3].y - pts[aFace^.v2].y);
    v2.z := (pts[aFace^.v3].z - pts[aFace^.v2].z);
    al_cross_product (v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
		aFace^.normal.x, aFace^.normal.y, aFace^.normal.z);

    Ship.faces[3].v1 := 2;
    Ship.faces[3].v2 := 1;
    Ship.faces[3].v3 := 3;
    aFace := @Ship.faces[3];
  { NOTE: Next lines should build the facet normal, but they don't! }
    v1.x := (pts[aFace^.v2].x - pts[aFace^.v1].x);
    v1.y := (pts[aFace^.v2].y - pts[aFace^.v1].y);
    v1.z := (pts[aFace^.v2].z - pts[aFace^.v1].z);
    v2.x := (pts[aFace^.v3].x - pts[aFace^.v2].x);
    v2.y := (pts[aFace^.v3].y - pts[aFace^.v2].y);
    v2.z := (pts[aFace^.v3].z - pts[aFace^.v2].z);
    al_cross_product (v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
		aFace^.normal.x, aFace^.normal.y, aFace^.normal.z);

    Ship.faces[4].v1 := 3;
    Ship.faces[4].v2 := 4;
    Ship.faces[4].v3 := 2;
    aFace := @Ship.faces[4];
  { NOTE: Next lines should build the facet normal, but they don't! }
    v1.x := (pts[aFace^.v2].x - pts[aFace^.v1].x);
    v1.y := (pts[aFace^.v2].y - pts[aFace^.v1].y);
    v1.z := (pts[aFace^.v2].z - pts[aFace^.v1].z);
    v2.x := (pts[aFace^.v3].x - pts[aFace^.v2].x);
    v2.y := (pts[aFace^.v3].y - pts[aFace^.v2].y);
    v2.z := (pts[aFace^.v3].z - pts[aFace^.v2].z);
    al_cross_product (v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
		aFace^.normal.x, aFace^.normal.y, aFace^.normal.z);

    FOR Ndx := Low (Ship.faces) TO High (Ship.faces) DO
    BEGIN;
      Ship.faces[Ndx].colour := 32;
      Ship.faces[Ndx].range := 15;
      al_normalize_vector(Ship.faces[Ndx].normal.x, Ship.faces[Ndx].normal.y,
			  Ship.faces[Ndx].normal.z);
      Ship.faces[Ndx].rnormal.x := Ship.faces[Ndx].normal.x;
      Ship.faces[Ndx].rnormal.y := Ship.faces[Ndx].normal.y;
      Ship.faces[Ndx].rnormal.z := Ship.faces[Ndx].normal.z;
    END;

    Ship.x := 0;
    Ship.y := 0;
    Ship.z := al_itofix (192);
    Ship.rx := 0;
    Ship.ry := 0;
    Ship.rz := 0;

    Ship.aim.x := 0;
    Ship.aim.y := 0;
    Ship.aim.z := al_itofix(-1);
    Direction.x := 0;
    Direction.y := 0;
    Direction.z := al_itofix(-1);
    Ship.velocity := 0;
  END;



(* Draws the ship model. *)
  PROCEDURE DrawShip;
  VAR
    Outs: ARRAY [1..NUM_VERTS] OF AL_V3D;
    Matrix: AL_MATRIX;
    Ndx, Col: INTEGER;
  BEGIN
    Ship.minx := AL_SCREEN_W;
    Ship.miny := AL_SCREEN_H;
    Ship.maxx := 0;
    Ship.maxy := 0;

    al_get_rotation_matrix (Matrix, Ship.rx, Ship.ry, Ship.rz);
    al_apply_matrix (Matrix, Ship.aim.x, Ship.aim.y, Ship.aim.z,
		     Outs[1].x, Outs[1].y, Outs[1].z);
    Direction.x := Outs[1].x;
    Direction.y := Outs[1].y;
    Direction.z := Outs[1].z;

    FOR Ndx := Low (Ship.faces) TO High (Ship.faces) DO
      al_apply_matrix (Matrix,
			Ship.faces[Ndx].normal.x, Ship.faces[Ndx].normal.y,
			Ship.faces[Ndx].normal.z,
			Ship.faces[Ndx].rnormal.x, Ship.faces[Ndx].rnormal.y,
			Ship.faces[Ndx].rnormal.z);

    al_get_transformation_matrix (Matrix, al_itofix (1),
				  Ship.rx, Ship.ry, Ship.rz,
				  Ship.x, Ship.y, Ship.z);

    FOR Ndx := Low (Ship.points) TO High (Ship.points) DO
    BEGIN
      al_apply_matrix (Matrix,
		Ship.points[Ndx].x, Ship.points[Ndx].y, Ship.points[Ndx].z,
		Outs[Ndx].x, Outs[Ndx].y, Outs[Ndx].z);
      al_persp_project (
	Outs[Ndx].x, Outs[Ndx].y, Outs[Ndx].z, Outs[Ndx].x, Outs[Ndx].y);
      IF al_fixtoi(outs[Ndx].x) < ship.minx THEN
	 Ship.minx := al_fixtoi (Outs[Ndx].x);
      IF al_fixtoi(outs[Ndx].x) > ship.maxx THEN
	 Ship.maxx := al_fixtoi (Outs[Ndx].x);
      IF al_fixtoi(outs[Ndx].y) < ship.miny THEN
	 Ship.miny := al_fixtoi (Outs[Ndx].y);
      IF al_fixtoi(outs[Ndx].y) > ship.maxy THEN
	 Ship.maxy := al_fixtoi (Outs[Ndx].y);
    END;

    FOR Ndx := Low (Ship.faces) TO High (Ship.faces) DO
    BEGIN
    { Since it isn't able to build the normal of faces, this is useless.
      IF al_fixtof (Ship.faces[Ndx].rnormal.z) < 0.0 THEN
      Better to use next:
    }
      IF al_polygon_z_normal (
	Outs[Ship.faces[Ndx].v1],
	Outs[Ship.faces[Ndx].v2],
	Outs[Ship.faces[Ndx].v3]
      ) < 0 THEN
      BEGIN
	Col := al_fixtoi (al_fixmul (al_dot_product (
					Ship.faces[Ndx].rnormal.x,
					Ship.faces[Ndx].rnormal.y,
					Ship.faces[Ndx].rnormal.z, 0, 0,
					al_itofix (1)),
			al_itofix (Ship.faces[Ndx].range)));
	IF Col < 0 THEN
	  Col := -Col + Ship.faces[Ndx].colour
	ELSE
	  Col := Col + Ship.faces[Ndx].colour;

	al_triangle (buffer,
		al_fixtoi (Outs[Ship.faces[Ndx].v1].x),
		al_fixtoi (Outs[Ship.faces[Ndx].v1].y),
		al_fixtoi (Outs[Ship.faces[Ndx].v2].x),
		al_fixtoi (Outs[Ship.faces[Ndx].v2].y),
		al_fixtoi (Outs[Ship.faces[Ndx].v3].x),
		al_fixtoi (Outs[Ship.faces[Ndx].v3].y),
		al_palette_color^[Col]);
      END;
    END;
  END;



(* Removes the ship model from the screen. *)
  PROCEDURE EraseShip;
  BEGIN
    al_rectfill (buffer, Ship.minx, Ship.miny, Ship.maxx, Ship.maxy,
		 al_palette_color^[0]);
  END;



VAR
  Pal: AL_PALETTE;
  Ndx: INTEGER;
(* The program starts here. *)
BEGIN
{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_timer;

{ Set a graphics mode. }
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 200, 0, 0) THEN
  BEGIN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;
  END;

  FOR Ndx := 0 TO 15 DO
  BEGIN
    Pal[Ndx].r := 0;
    Pal[Ndx].g := 0;
    Pal[Ndx].b := 0;
  END;

{ Greyscale. }
   Pal[16].r := 63; Pal[16].g := 63; Pal[16].b := 63;
   Pal[17].r := 48; Pal[17].g := 48; Pal[17].b := 48;
   Pal[18].r := 32; Pal[18].g := 32; Pal[18].b := 32;
   Pal[19].r := 16; Pal[19].g := 16; Pal[19].b := 16;
   Pal[20].r := 8; Pal[20].g := 8; Pal[20].b := 8;

{ Red range. }
  FOR Ndx := 0 TO 15 DO
  BEGIN
    Pal[Ndx + 32].r := 31 + Ndx * 2;
    Pal[Ndx + 32].g := 15;
    Pal[Ndx + 32].b := 7;
  END;

{ A nice fire orange. }
  FOR Ndx := 64 TO 67 DO
  BEGIN
    Pal[Ndx].r := 63;
    Pal[Ndx].g := 17 + (Ndx - 64) * 3;
    Pal[Ndx].b := 0;
  END;

  al_set_palette (Pal);

  buffer := al_create_bitmap (AL_SCREEN_W, AL_SCREEN_H);
  al_clear_bitmap (buffer);

  al_set_projection_viewport (0, 0, AL_SCREEN_W, AL_SCREEN_H);

  InitStars;
  DrawStars;
  InitShip;
  DrawShip;

  REPEAT
    EraseStars;
    EraseShip;

    MoveStars;
    DrawStars;

    al_textprintf_centre_ex (buffer, al_font,
	AL_SCREEN_W DIV 2, AL_SCREEN_H - 10,
	al_palette_color^[17], 0,
        '     direction: <%F, %F, %F>   ', [
          al_fixtof (Direction.x),
          al_fixtof (Direction.y),
          al_fixtof (Direction.z)
        ]
    );
    al_textprintf_centre_ex (buffer, al_font,
	AL_SCREEN_W DIV 2, AL_SCREEN_H - 20,
	al_palette_color^[17], 0,
        '     delta: <%F, %F, %F>   ', [
          al_fixtof (Delta.x),
          al_fixtof (Delta.y),
          al_fixtof (Delta.z)
        ]
    );
    al_textprintf_centre_ex (buffer, al_font,
	AL_SCREEN_W DIV 2, AL_SCREEN_H - 30,
	al_palette_color^[17], 0,
      	'   velocity: %d   ', [Ship.velocity]
    );

    al_textout_centre_ex (buffer, al_font,
	'Press ESC to exit', AL_SCREEN_W DIV 2, 16,
	al_palette_color^[18], 0);
    al_textout_centre_ex (buffer, al_font,
	'Press CTRL to fire engine', AL_SCREEN_W DIV 2, 32,
	al_palette_color^[18], 0);

    DrawShip;

    al_vsync;
    al_blit (buffer, al_screen, 0, 0, 0, 0, AL_SCREEN_W, AL_SCREEN_H);

    al_poll_keyboard;

  { Rotates the ship. }
    IF al_key[AL_KEY_UP] THEN
      DEC (Ship.rx, al_itofix (5))
    ELSE IF al_key[AL_KEY_DOWN] THEN
      INC (Ship.rx, al_itofix (5));

    IF al_key[AL_KEY_LEFT] THEN
      DEC (Ship.ry, al_itofix (5))
    ELSE IF al_key[AL_KEY_RIGHT] THEN
      INC (Ship.ry, al_itofix (5));

    IF al_key[AL_KEY_PGUP] THEN
      DEC (Ship.rz, al_itofix (5))
    ELSE IF al_key[AL_KEY_PGDN] THEN
      INC (Ship.rz, al_itofix (5));

  { Thrust. }
    IF al_key[AL_KEY_LCONTROL] OR al_key[AL_KEY_RCONTROL] THEN
    BEGIN
      Ship.faces[ENGINE].colour := ENGINE_ON;
      Ship.faces[ENGINE].range := 3;
      IF Ship.velocity < SPEED_LIMIT THEN
	INC (Ship.velocity, 2);
    END
    ELSE BEGIN
      Ship.faces[ENGINE].colour := ENGINE_OFF;
      Ship.faces[ENGINE].range := 15;
      IF Ship.velocity > 0 THEN
	DEC (ship.velocity, 2);
    END;

    Ship.rx := Ship.rx AND al_itofix (255);
    Ship.ry := Ship.ry AND al_itofix (255);
    Ship.rz := Ship.rz AND al_itofix (255);

    Delta.x := al_fixmul (Direction.x, al_itofix (Ship.velocity));
    Delta.y := al_fixmul (Direction.y, al_itofix (Ship.velocity));
    Delta.z := al_fixmul (Direction.z, al_itofix (Ship.velocity));
  UNTIL al_key[AL_KEY_ESC];

  al_destroy_bitmap (buffer);
END.
