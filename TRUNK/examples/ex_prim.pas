PROGRAM ex_prim;
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
 *      A sampler of the primitive addon.
 *      All primitives are rendered using the additive blender, so overdraw will manifest itself as overly bright pixels.
 *
 *
 *      By Pavel Sountsov.
 *      Translated to Pascal by Ñuño Martínez.
 *
 *      See readme.txt for copyright information.
 *)

{$LONGSTRINGS ON}
{$IFDEF FPC}
{ Needed to support classes and C-style pointers. }
  {$IF NOT DEFINED(FPC_OBJFPC)}
    {$MODE OBJFPC}
  {$ENDIF}
{$ENDIF}

  USES
    common,
    Allegro5, al5Base, al5font, al5image, al5primitives,
    sysutils;

  CONST
    ScreenW = 800; ScreenH = 600;
    RefreshRate = 60;
    FixedTimestep = 1 / RefreshRate;
    NUM_SCREENS = 12;
    ROTATE_SPEED = 0.0001;

  VAR
    UseShader, Soft, Blend, Background: BOOLEAN;
    MainTrans, Identity: ALLEGRO_TRANSFORM;
    Speed, Thickness, Theta: SINGLE;

  TYPE
  (* Base class for example screens. *)
    TCustomScreen = CLASS (TObject)
    PRIVATE
      fName: STRING;
    PROTECTED
    (* Renders the screen. *)
      PROCEDURE DoDraw; VIRTUAL; ABSTRACT;
    (* Helper that renders several high-level primitives. *)
      PROCEDURE DrawHighPrimitives;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create (CONST aName: STRING);
    (* Updates the example.
   
      By default increases Theta and builds MainTrans. *)
      PROCEDURE Update; VIRTUAL;
    (* Draws the screen. *)
      PROCEDURE Draw;

    (* Screen name. *)
      PROPERTY Name: STRING READ fName;
    END;
    
(* Draws high level primitives. *)
  PROCEDURE TCustomScreen.DrawHighPrimitives;
  VAR
    Points: ALLEGRO_SPLINE_CONTROL_POINTS = (
         -300, -200,
         700, 200,
         -700, 200,
         300, -200
    );
  BEGIN
    al_draw_line (-300, -200, 300, 200, al_map_rgba_f (0, 0.5, 0.5, 1), Thickness);
    al_draw_triangle (-150, -250, 0, 250, 150, -250, al_map_rgba_f (0.5, 0, 0.5, 1), Thickness);
    al_draw_rectangle (-300, -200, 300, 200, al_map_rgba_f (0.5, 0, 0, 1), Thickness);
    al_draw_rounded_rectangle (-200, -125, 200, 125, 50, 100, al_map_rgba_f (0.2, 0.2, 0, 1), Thickness);

    al_draw_ellipse (0, 0, 300, 150, al_map_rgba_f (0, 0.5, 0.5, 1), Thickness);
    al_draw_elliptical_arc (-20, 0, 300, 200, -ALLEGRO_PI / 2, -ALLEGRO_PI, al_map_rgba_f (0.25, 0.25, 0.5, 1), Thickness);
    al_draw_arc (0, 0, 200, -ALLEGRO_PI / 2, ALLEGRO_PI, al_map_rgba_f (0.5, 0.25, 0, 1), Thickness);
    al_draw_spline (Points, al_map_rgba_f (0.1, 0.2, 0.5, 1), Thickness);
    al_draw_pieslice (0, 25, 150, ALLEGRO_PI * 3 / 4, -ALLEGRO_PI / 2, al_map_rgba_f (0.4, 0.3, 0.1, 1), Thickness);
  END;



(* Constructor. *)
  CONSTRUCTOR TCustomScreen.Create (CONST aName: STRING);
  BEGIN
    INHERITED Create;
    fName := aName
  END;

(* Draws the screen. *)
  PROCEDURE TCustomScreen.Draw;
  BEGIN
    IF Blend THEN
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE)
    ELSE
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    al_use_transform (MainTrans);
    SELF.DoDraw;
    al_use_transform (Identity)
  END;



(* Executes screen. *)
  PROCEDURE TCustomScreen.Update;
  BEGIN
    Theta := Theta + Speed;
    al_build_transform (MainTrans, ScreenW / 2, ScreenH / 2, 1, 1, Theta)
  END;



  VAR
    Font: ALLEGRO_FONTptr;

    Display: ALLEGRO_DISPLAYptr;
    Bkg, Texture: ALLEGRO_BITMAPptr;
    SolidWhite, Black: ALLEGRO_COLOR;
    Queue: ALLEGRO_EVENT_QUEUEptr;

    Screens: ARRAY [0..NUM_SCREENS - 1] OF TCustomScreen;

(*
 * TLowPrimitives
 ***************************************************************************)

  TYPE
    TLowPrimitives = CLASS (TCustomScreen)
    PRIVATE
      Vtx, Vtx2: ARRAY [0..12] OF ALLEGRO_VERTEX;
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    END;

(* Draws screen. *)
  PROCEDURE TLowPrimitives.DoDraw;
  BEGIN
    al_draw_prim (Vtx, NIL, NIL, 0, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_prim (Vtx, NIL, NIL, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_prim (Vtx, NIL, NIL, 9, 13, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_prim (Vtx2, NIL, NIL, 0, 13, ALLEGRO_PRIM_POINT_LIST);
  END;



(* Constructor. *)
  CONSTRUCTOR TLowPrimitives.Create;
  VAR
    Ndx, x, y: INTEGER;
    Color: ALLEGRO_COLOR;
  BEGIN
    INHERITED Create ('Low Level Primitives');

    FOR Ndx := LOW (Vtx) TO HIGH (Vtx) DO
    BEGIN
      x := TRUNC (200 * Cos (Ndx / 13 * 2 * ALLEGRO_PI));
      y := TRUNC (200 * Sin (Ndx / 13 * 2 * ALLEGRO_PI));
         
      Color := al_map_rgb (
	(Ndx + 1) DIV 3 * 64,
	(Ndx + 2) DIV 3 * 64,
	(Ndx    ) DIV 3 * 64);
         
      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      Vtx[Ndx].color := color;
      Vtx2[Ndx].color := color
    END
  END;



(*
 * TIndexedPrimitives
 ***************************************************************************)

  TYPE
    TIndexedPrimitives = CLASS (TCustomScreen)
    PRIVATE
      Indices1, Indices2, Indices3: ARRAY [0..3] OF INTEGER;
      Vtx, Vtx2: ARRAY [0..12] OF ALLEGRO_VERTEX;
    PROTECTED
    (* Draws the Screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Executes the example. *)
      PROCEDURE Update; OVERRIDE;
    END;

(* Draws screen. *)
  PROCEDURE TIndexedPrimitives.DoDraw;
  BEGIN
    al_draw_indexed_prim (vtx, NIL, NIL, indices1, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_indexed_prim (vtx, NIL, NIL, indices2, 4, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_indexed_prim (vtx, NIL, NIL, indices3, 4, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_indexed_prim (vtx2, NIL, NIL, indices3, 4, ALLEGRO_PRIM_POINT_LIST);
  END;



(* Constructor. *)
  CONSTRUCTOR TIndexedPrimitives.Create;
  VAR
    Ndx, x, y: INTEGER;
    Color: ALLEGRO_COLOR;
  BEGIN
    INHERITED Create ('Indexed Primitives');

    FOR Ndx := 0 TO 3 DO
    BEGIN
      Indices1[Ndx] := Ndx;
      Indices2[Ndx] := Ndx + 5;
      Indices3[Ndx] := Ndx + 9;
    END;
    Indices1[2] := 3; Indices1[3] := 4;

    FOR Ndx := LOW (vtx) TO HIGH (vtx) DO
    BEGIN
      x := TRUNC (200 * cos (Ndx / 13 * 2 * ALLEGRO_PI));
      y := TRUNC (200 * sin (Ndx / 13 * 2 * ALLEGRO_PI));
         
      Color := al_map_rgb (
	(Ndx + 1) DIV 3 * 64,
	(Ndx + 2) DIV 3 * 64,
	(Ndx    ) DIV 3 * 64);
         
      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      Vtx[Ndx].color := color;
      Vtx2[Ndx].color := color
    END
  END;

  

(* Executes screen. *)
  PROCEDURE TIndexedPrimitives.Update;
  VAR
    Ndx: INTEGER;
  BEGIN
    INHERITED Update; { Updates Theta and builds MainTrans. }
    FOR Ndx := LOW (Indices1) TO HIGH (Indices1) DO
    BEGIN
      Indices1[Ndx] := TRUNC (al_get_time() + Ndx) MOD 13;
      Indices2[Ndx] := TRUNC (al_get_time() + Ndx + 4) MOD 13;
      Indices3[Ndx] := TRUNC (al_get_time() + Ndx + 8) MOD 13;
    END;
  END;



(*
 * THighPrimitives
 ***************************************************************************)

  TYPE
    THighPrimitives = CLASS (TCustomScreen)
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    END;

(* Draws screen. *)
  PROCEDURE THighPrimitives.DoDraw;
  BEGIN
    SELF.DrawHighPrimitives
  END;



(* Constructor. *)
  CONSTRUCTOR THighPrimitives.Create;
  BEGIN
    INHERITED Create ('High Level Primitives')
  END;

  

(*
 * TTransformationsPrimitives
 ***************************************************************************)

  TYPE
    TTransformationsPrimitives = CLASS (TCustomScreen)
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Updates the example. *)
      PROCEDURE Update; OVERRIDE;
    END;

(* Draws screen. *)
  PROCEDURE TTransformationsPrimitives.DoDraw;
  BEGIN
    SELF.DrawHighPrimitives
  END;



(* Constructor. *)
  CONSTRUCTOR TTransformationsPrimitives.Create;
  BEGIN
    INHERITED Create ('Transformations')
  END;



(* Updates screen. *)
  PROCEDURE TTransformationsPrimitives.Update;
  VAR
    t: DOUBLE;
  BEGIN
    t := al_get_time / 5;
    Theta := Theta + Speed;
    al_build_transform (
      MainTrans,
      ScreenW / 2, ScreenH / 2, sin (t), cos (t),
      Theta
    )
  END;



(*
 * TFilledPrimitives
 ***************************************************************************)

  TYPE
    TFilledPrimitives = CLASS (TCustomScreen)
    PRIVATE
      vtx: ARRAY [0..20] OF ALLEGRO_VERTEX;
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    END;

(* Draws screen. *)
  PROCEDURE TFilledPrimitives.DoDraw;
  BEGIN
    al_draw_prim (vtx, NIL, NIL, 0, 6, ALLEGRO_PRIM_TRIANGLE_FAN);
    al_draw_prim (vtx, NIL, NIL, 7, 13, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_prim (vtx, NIL, NIL, 14, 20, ALLEGRO_PRIM_TRIANGLE_STRIP)
  END;



(* Constructor. *)
  CONSTRUCTOR TFilledPrimitives.Create;
  VAR
    Ndx: INTEGER;
    x, y: SINGLE;
    Color: ALLEGRO_COLOR;
  BEGIN
    INHERITED Create ('Low Level Filled Primitives');
    FOR Ndx := LOW (vtx) TO HIGH (vtx) DO
    BEGIN
      IF (Ndx MOD 2) = 0 THEN
      BEGIN
        x := 150 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
	y := 150 * sin (Ndx / 20 * 2 * ALLEGRO_PI)
      END
      ELSE BEGIN
	x := 200 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
	y := 200 * sin (Ndx / 20 * 2 * ALLEGRO_PI);
      END;
         
      IF Ndx = 0 THEN
      BEGIN
        x := 0; Y := 0
      END;
         
      Color := al_map_rgb (
        (7 * Ndx + 1) MOD 3 * 64,
       	(2 * Ndx + 2) MOD 3 * 64,
       	(Ndx        ) MOD 3 * 64);
         
      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].color := color
    END
  END;



(*
 * TIndexedFilledPrimitives
 ***************************************************************************)

  TYPE
    TIndexedFilledPrimitives = CLASS (TCustomScreen)
    PRIVATE
      vtx: ARRAY [0..20] OF ALLEGRO_VERTEX;
      Indices1, Indices2, Indices3: ARRAY [0..5] OF INTEGER;
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Updates the example. *)
      PROCEDURE Update; OVERRIDE;
    END;

(* Draws screen. *)
  PROCEDURE TIndexedFilledPrimitives.DoDraw;
  BEGIN
    al_draw_indexed_prim (vtx, NIL, NIL, Indices1, 6, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_indexed_prim (vtx, NIL, NIL, Indices2, 6, ALLEGRO_PRIM_TRIANGLE_STRIP);
    al_draw_indexed_prim (vtx, NIL, NIL, Indices3, 6, ALLEGRO_PRIM_TRIANGLE_FAN)
  END;



(* Constructor. *)
  CONSTRUCTOR TIndexedFilledPrimitives.Create;
  VAR
    Ndx: INTEGER;
    x, y: SINGLE;
    Color: ALLEGRO_COLOR;
  BEGIN
    INHERITED Create ('Indexed Filled Primitives');
    FOR Ndx := LOW (Indices1) TO HIGH (Indices1) DO
    BEGIN
      Indices1[Ndx] := Ndx + 12;
      IF Ndx > 2 THEN INC(Indices1[Ndx]);
      Indices2[Ndx] := Ndx + 6;
      Indices1[Ndx] := Ndx;
    END;
    FOR Ndx := LOW (vtx) TO HIGH (vtx) DO
    BEGIN
      IF (Ndx MOD 2) = 0 THEN
      BEGIN
        x := 150 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
	y := 150 * sin (Ndx / 20 * 2 * ALLEGRO_PI)
      END
      ELSE BEGIN
	x := 200 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
	y := 200 * sin (Ndx / 20 * 2 * ALLEGRO_PI);
      END;
         
      IF Ndx = 0 THEN
      BEGIN
        x := 0; Y := 0
      END;
         
      Color := al_map_rgb (
        (7 * Ndx + 1) MOD 3 * 64,
       	(2 * Ndx + 2) MOD 3 * 64,
       	(Ndx        ) MOD 3 * 64);
         
      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].color := color
    END
  END;



(* Updates screen. *)
  PROCEDURE TIndexedFilledPrimitives.Update;
  VAR
    Ndx: INTEGER;
  BEGIN
    Theta := Theta + Speed;
    FOR Ndx := LOW (Indices1) TO HIGH (Indices1) DO
    BEGIN
      Indices1[Ndx] := (TRUNC (al_get_time) + Ndx) MOD 20 + 1;
      Indices2[Ndx] := (TRUNC (al_get_time) + Ndx + 6) MOD 20 + 1;
      IF Ndx > 0 THEN
        Indices3[Ndx] := (TRUNC (al_get_time) + Ndx + 12) MOD 20 + 1
    END;
    al_build_transform (MainTrans, ScreenW / 2, ScreenH / 2, 1, 1, Theta)
  END;



(*
 * THighFilledPrimitives
 ***************************************************************************)

  TYPE
    THighFilledPrimitives = CLASS (TCustomScreen)
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    END;

(* Draws screen. *)
  PROCEDURE THighFilledPrimitives.DoDraw;
  BEGIN
    al_draw_filled_triangle (-100, -100, -150, 200, 100, 200, al_map_rgb_f (0.5, 0.7, 0.3));
    al_draw_filled_rectangle (20, -50, 200, 50, al_map_rgb_f (0.3, 0.2, 0.6));
    al_draw_filled_ellipse (-250, 0, 100, 150, al_map_rgb_f (0.3, 0.3, 0.3));
    al_draw_filled_rounded_rectangle (50, -250, 350, -75, 50, 70, al_map_rgb_f (0.4, 0.2, 0));
    al_draw_filled_pieslice (200, 125, 50, ALLEGRO_PI / 4, 3 * ALLEGRO_PI / 2, al_map_rgb_f (0.3, 0.3, 0.1))
  END;



(* Constructor. *)
  CONSTRUCTOR THighFilledPrimitives.Create;
  BEGIN
    INHERITED Create ('High Level Filled Primitives');
  END;



(*
 * TTexturePrimitives
 ***************************************************************************)

  TYPE
    TTexturePrimitives = CLASS (TCustomScreen)
    PRIVATE
      Vtx, Vtx2: ARRAY [0..12] OF ALLEGRO_VERTEX;
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    END;

(* Draws screen. *)
  PROCEDURE TTexturePrimitives.DoDraw;
  BEGIN
    al_draw_prim (Vtx, NIL, Texture, 0, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_prim (Vtx, NIL, Texture, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_prim (Vtx, NIL, Texture, 9, 13, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_prim (Vtx2, NIL, Texture, 0, 13, ALLEGRO_PRIM_POINT_LIST);
  END;



(* Constructor. *)
  CONSTRUCTOR TTexturePrimitives.Create;
  VAR
    Ndx, x, y: INTEGER;
    Color: ALLEGRO_COLOR;
  BEGIN
    INHERITED Create ('Textured Primitives');

    FOR Ndx := LOW (Vtx) TO HIGH (Vtx) DO
    BEGIN
      x := TRUNC (200 * Cos (Ndx / 13 * 2 * ALLEGRO_PI));
      y := TRUNC (200 * Sin (Ndx / 13 * 2 * ALLEGRO_PI));
         
      Color := al_map_rgb (
	(Ndx + 1) DIV 3 * 64,
	(Ndx + 2) DIV 3 * 64,
	(Ndx    ) DIV 3 * 64);
         
      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      vtx[Ndx].u := 64 * x / 100; vtx[Ndx].v := 64 * y / 100;
      vtx2[Ndx].u := 64 * x / 100; vtx2[Ndx].v := 64 * y / 100;
      IF Ndx < 10 THEN
         vtx[Ndx].color := al_map_rgba_f (1, 1, 1, 1)
      ELSE
         vtx[Ndx].color := Color;
      Vtx2[Ndx].color := Color
    END
  END;



(*
 * TFilledTexturePrimitives
 ***************************************************************************)

  TYPE
    TFilledTexturePrimitives = CLASS (TCustomScreen)
    PRIVATE
      vtx: ARRAY [0..20] OF ALLEGRO_VERTEX;
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    END;

(* Draws screen. *)
  PROCEDURE TFilledTexturePrimitives.DoDraw;
  BEGIN
    al_draw_prim (vtx, NIL, Texture, 0, 6, ALLEGRO_PRIM_TRIANGLE_FAN);
    al_draw_prim (vtx, NIL, Texture, 7, 13, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_prim (vtx, NIL, Texture, 14, 20, ALLEGRO_PRIM_TRIANGLE_STRIP)
  END;



(* Constructor. *)
  CONSTRUCTOR TFilledTexturePrimitives.Create;
  VAR
    Ndx: INTEGER;
    x, y: SINGLE;
    Color: ALLEGRO_COLOR;
  BEGIN
    INHERITED Create ('Filled Textured Primitives');
    FOR Ndx := LOW (vtx) TO HIGH (vtx) DO
    BEGIN
      IF (Ndx MOD 2) = 0 THEN
      BEGIN
        x := 150 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
	y := 150 * sin (Ndx / 20 * 2 * ALLEGRO_PI)
      END
      ELSE BEGIN
	x := 200 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
	y := 200 * sin (Ndx / 20 * 2 * ALLEGRO_PI);
      END;
         
      IF Ndx = 0 THEN
      BEGIN
        x := 0; Y := 0
      END;
         
      Color := al_map_rgb (
        (7 * Ndx + 1) MOD 3 * 64,
       	(2 * Ndx + 2) MOD 3 * 64,
       	(Ndx        ) MOD 3 * 64);
         
      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].u := 64 * x / 100; vtx[Ndx].v := 64 * y / 100;
      IF Ndx < 10 THEN
        vtx[Ndx].color := al_map_rgba_f(1, 1, 1, 1)
      ELSE
        vtx[Ndx].color := Color
    END
  END;



(*
 * TCustomVertexFormatPrimitives
 ***************************************************************************)

  TYPE
    CUSTOM_VERTEX = RECORD
    { Use types define at al5Base, so we know their size. }
      u, v: AL_INT16;
      x, y: AL_INT16;
      Color: ALLEGRO_COLOR;
      junk: ARRAY [0..5] OF AL_INT;
    END;

    TCustomVertexFormatPrimitives = CLASS (TCustomScreen)
    PRIVATE
      vtx: ARRAY [0..3] OF CUSTOM_VERTEX;
      Decl: ALLEGRO_VERTEX_DECLptr;
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    END;

(* Draws screen. *)
  PROCEDURE TCustomVertexFormatPrimitives.DoDraw;
  BEGIN
    al_draw_prim_ex (@vtx, Decl, Texture, 0, 4, ALLEGRO_PRIM_TRIANGLE_FAN);
  END;



(* Constructor. *)
  CONSTRUCTOR TCustomVertexFormatPrimitives.Create;
  VAR
   Elems: ARRAY [0..3] OF ALLEGRO_VERTEX_ELEMENT = (
     (attribute: ALLEGRO_PRIM_POSITION; storage: ALLEGRO_PRIM_SHORT_2; offset: 4),
     (attribute: ALLEGRO_PRIM_TEX_COORD_PIXEL; storage: ALLEGRO_PRIM_SHORT_2; offset: 0),
     (attribute: ALLEGRO_PRIM_COLOR_ATTR; storage: ALLEGRO_PRIM_STORAGE_NONE; offset: 8),
     (attribute: ALLEGRO_PRIM_ATTR_NONE; storage: ALLEGRO_PRIM_STORAGE_NONE; offset: 0)
    );
    Ndx, x, y: INTEGER;
  BEGIN
    INHERITED Create ('Custom Vertex Format');

    Decl := al_create_vertex_decl (Elems, sizeof (CUSTOM_VERTEX));

    FOR Ndx := LOW (vtx) TO HIGH (vtx) DO
    BEGIN
      x := TRUNC (200 * cos (Ndx / 4 * 2 * ALLEGRO_PI));
      y := TRUNC (200 * sin (Ndx / 4 * 2 * ALLEGRO_PI));
         
      vtx[Ndx].x := x; vtx[Ndx].y := y;
      vtx[Ndx].u := TRUNC (64 * x / 100); vtx[Ndx].v := TRUNC (64 * y / 100);
      vtx[Ndx].color := al_map_rgba_f (1, 1, 1, 1)
    END
  END;



(*
 * TVertexBuffers
 ***************************************************************************)

  TYPE
    TVertexBuffers = CLASS (TCustomScreen)
    PRIVATE
      vtx, vtx2: ARRAY [0..12] OF ALLEGRO_VERTEX;
      vbuff, vbuff2: ALLEGRO_VERTEX_BUFFERptr;
      NoSoft, NoSoft2: BOOLEAN;
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    END;

(* Draws screen. *)
  PROCEDURE TVertexBuffers.DoDraw;
  BEGIN
    IF (vbuff <> NIL) AND NOT (Soft AND NoSoft) THEN
    BEGIN
      al_draw_vertex_buffer (vbuff, NIL, 0, 4, ALLEGRO_PRIM_LINE_LIST);
      al_draw_vertex_buffer (vbuff, NIL, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
      al_draw_vertex_buffer (vbuff, NIL, 9, 13, ALLEGRO_PRIM_LINE_LOOP)
    END
    ELSE
      al_draw_text (
        Font, al_map_rgb_f (1, 1, 1), 0, -40, 0, 'Vertex buffers not supported'
      );

    IF (vbuff2 <> NIL) AND NOT (Soft AND NoSoft2) THEN
      al_draw_vertex_buffer (vbuff2, NIL, 0, 13, ALLEGRO_PRIM_POINT_LIST)
    ELSE
      al_draw_text (
        Font, al_map_rgb_f (1, 1, 1), 0, -40, 0, 'Vertex buffers not supported'
      )
  END;



(* Constructor. *)
  CONSTRUCTOR TVertexBuffers.Create;
  VAR
    Ndx, x, y: INTEGER;
    Color: ALLEGRO_COLOR;
  BEGIN
    INHERITED Create ('Vertex Buffers');

    FOR Ndx := LOW (Vtx) TO HIGH (Vtx) DO
    BEGIN
      x := TRUNC (200 * Cos (Ndx / 13 * 2 * ALLEGRO_PI));
      y := TRUNC (200 * Sin (Ndx / 13 * 2 * ALLEGRO_PI));
         
      Color := al_map_rgb (
	(Ndx + 1) DIV 3 * 64,
	(Ndx + 2) DIV 3 * 64,
	(Ndx    ) DIV 3 * 64);
         
      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      Vtx[Ndx].color := color;
      Vtx2[Ndx].color := color
    END;

    vbuff := al_create_vertex_buffer (NIL, vtx, 13, ALLEGRO_PRIM_BUFFER_READWRITE);
    IF vbuff = NIL THEN
    BEGIN
      vbuff := al_create_vertex_buffer (NIL, vtx, 13, ALLEGRO_PRIM_BUFFER_NONE);
      NoSoft := TRUE
    END
    ELSE
      NoSoft := FALSE;

    vbuff2 := al_create_vertex_buffer (NIL, vtx2, 13, ALLEGRO_PRIM_BUFFER_READWRITE);
    IF vbuff2 = NIL THEN
    BEGIN
      vbuff2 := al_create_vertex_buffer (NIL, vtx2, 13, ALLEGRO_PRIM_BUFFER_NONE);
      NoSoft2 := TRUE
    END
    ELSE
      NoSoft2 := FALSE
  END;



(* Destructor. *)
  DESTRUCTOR TVertexBuffers.Destroy;
  BEGIN
    al_destroy_vertex_buffer (vbuff);
    al_destroy_vertex_buffer (vbuff2);
    INHERITED Destroy
  END;



(*
 * TIndexedBuffers
 ***************************************************************************)

  TYPE
    TIndexedBuffers = CLASS (TCustomScreen)
    PRIVATE
      vbuff: ALLEGRO_VERTEX_BUFFERptr;
      ibuff: ALLEGRO_INDEX_BUFFERptr;
      Soft: BOOLEAN;
    PROTECTED
    (* Draws the screen. *)
      PROCEDURE DoDraw; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Updates screen. *)
      PROCEDURE Update; OVERRIDE;
    END;

(* Draws screen. *)
  PROCEDURE TIndexedBuffers.DoDraw;
  BEGIN
    IF (NOT Soft AND NOT SELF.Soft) AND (vbuff <> NIL) AND (ibuff <> NIL) THEN
    BEGIN
      al_draw_indexed_buffer (vbuff, NIL, ibuff, 0, 4, ALLEGRO_PRIM_LINE_LIST);
      al_draw_indexed_buffer (vbuff, NIL, ibuff, 4, 8, ALLEGRO_PRIM_LINE_STRIP)
    END
    ELSE
      al_draw_text (
        Font, al_map_rgb_f (1, 1, 1), 0, 0, 0, 'Indexed buffers not supported'
      )
  END;



(* Constructor. *)
  CONSTRUCTOR TIndexedBuffers.Create;
  VAR
    Ndx: INTEGER;
    Color: ALLEGRO_COLOR;
    vtx: ALLEGRO_VERTEXptr;
    Flags: ALLEGRO_PRIM_BUFFER_FLAGS;
    x, y: SINGLE;
  BEGIN
    INHERITED Create ('Indexed Buffers');
    vbuff := al_create_vertex_buffer_ex
      (NIL, NIL, 13, ALLEGRO_PRIM_BUFFER_READWRITE);
    IF vbuff = NIL THEN
    BEGIN
      vbuff := al_create_vertex_buffer_ex
        (NIL, NIL, 13, ALLEGRO_PRIM_BUFFER_NONE);
      SELF.Soft := FALSE;
      Flags := ALLEGRO_PRIM_BUFFER_NONE
    END
    ELSE BEGIN
      SELF.Soft := TRUE;
      Flags := ALLEGRO_PRIM_BUFFER_READWRITE
    END;

    ibuff := al_create_index_buffer (sizeof (AL_SHORT), NIL, 8, Flags);

    IF vbuff <> NIL THEN
    BEGIN
      vtx := al_lock_vertex_buffer (vbuff, 0, 13, ALLEGRO_LOCK_WRITEONLY);
      TRY
        FOR Ndx := 0 TO 13 DO
        BEGIN
          x := 200 * cos (Ndx / 13 * 2 * ALLEGRO_PI);
	  y := 200 * sin (Ndx / 13 * 2 * ALLEGRO_PI);

	  Color := al_map_rgb (
	    (Ndx + 1) MOD 3 * 64,
	    (Ndx + 2) MOD 3 * 64,
	    (Ndx    ) MOD 3 * 64
	  );

          vtx[Ndx].x := x;
          vtx[Ndx].y := y;
          vtx[Ndx].z := 0;
          vtx[Ndx].color := Color
        END
      FINALLY
         al_unlock_vertex_buffer(vbuff) 
      END
    END
  END;



(* Destructor. *)
  DESTRUCTOR TIndexedBuffers.Destroy;
  BEGIN
    al_destroy_vertex_buffer (vbuff);
    al_destroy_index_buffer (ibuff);
    INHERITED Destroy
  END;

  

(* Updates screen. *)
  PROCEDURE TIndexedBuffers.Update;
  VAR
    Ndx, t: INTEGER;
    Indices: ^AL_SHORT;
  BEGIN
    IF ibuff <> NIL THEN
    BEGIN
      t := TRUNC (al_get_time);
      Indices := al_lock_index_buffer (ibuff, 0, 8, ALLEGRO_LOCK_WRITEONLY);

      FOR Ndx := 0 TO 7 DO
        Indices[Ndx] := (t + Ndx) MOD 13;
      al_unlock_index_buffer (ibuff);
    END;
    INHERITED Update
  END;



(* Runs example. *)
  PROCEDURE RunExample;
  VAR
    Timer: ALLEGRO_TIMERptr;
    TimerQueue: ALLEGRO_EVENT_QUEUEptr;
    Buffer: ALLEGRO_BITMAPptr;
    Old, CurScreen, FramesDone: INTEGER;
    Done, Clip: BOOLEAN;
    RealTime, GameTime, StartTime, TimeDiff, FrameDuration: DOUBLE;
    KeyEvent: ALLEGRO_EVENT;
  BEGIN
    Clip := FALSE;
    RealTime := al_get_time;
    FramesDone := 0;
    TimeDiff := al_get_time;
    GameTime := al_get_time;

    Timer := al_create_timer (ALLEGRO_BPS_TO_SECS (RefreshRate));
    al_start_timer (Timer);
    TimerQueue := al_create_event_queue;
    al_register_event_source (TimerQueue, al_get_timer_event_source (Timer));

    Old := al_get_new_bitmap_flags;
    al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
    Buffer := al_create_bitmap (ScreenW, ScreenH);
    al_set_new_bitmap_flags (Old);

    FOR CurScreen := LOW (Screens) TO HIGH (Screens) DO
      IF Screens[CurScreen] <> NIL THEN Screens[CurScreen].Update;

    Done := FALSE;
    CurScreen := 0;
    REPEAT
      FrameDuration := al_get_time - RealTime;
      al_rest (FixedTimestep - FrameDuration); { rest at least fixed_dt }
      FrameDuration := al_get_time - RealTime;
      RealTime := al_get_time;
      GameTime := RealTime;

      IF RealTime - GameTime > FrameDuration THEN { eliminate excess overflow }
        GameTime := GameTime + FixedTimestep * TRUNC ((RealTime - GameTime) / FixedTimestep);

      WHILE RealTime - GameTime >= 0 DO
      BEGIN
        StartTime := al_get_time;
        GameTime := GameTime + FixedTimestep;
	
        WHILE al_get_next_event (Queue, KeyEvent) DO
       	BEGIN
	  CASE KeyEvent._type OF
          ALLEGRO_EVENT_MOUSE_BUTTON_DOWN: 
	    BEGIN
	      INC (CurScreen);
	      IF CurScreen > HIGH (Screens) THEN CurScreen := 0
	    END;
          ALLEGRO_EVENT_DISPLAY_CLOSE:
	    Done := TRUE;
          ALLEGRO_EVENT_KEY_CHAR:
	    BEGIN
	      CASE KeyEvent.keyboard.keycode OF
              ALLEGRO_KEY_ESCAPE:
	        Done := TRUE;
              ALLEGRO_KEY_S:
	        BEGIN
                  Soft := NOT Soft;
		  TimeDiff := al_get_time;
		  FramesDone := 0
		END;
              ALLEGRO_KEY_C:
		BEGIN
                  Clip := NOT Clip;
		  TimeDiff := al_get_time;
		  FramesDone := 0
		END;
              ALLEGRO_KEY_L: 
	        BEGIN
                  Blend := NOT Blend;
		  TimeDiff := al_get_time;
		  FramesDone := 0
	        END;
              ALLEGRO_KEY_B: 
	        BEGIN
                  Background := NOT Background;
		  TimeDiff := al_get_time;
		  FramesDone := 0
	        END;
              ALLEGRO_KEY_LEFT:
		Speed := Speed - ROTATE_SPEED;
              ALLEGRO_KEY_RIGHT:
		Speed := Speed + ROTATE_SPEED;
              ALLEGRO_KEY_PGUP:
		BEGIN
                  Thickness := Thickness + 0.5;
                  IF Thickness < 1.0 THEN Thickness := 1.0;
		END;
              ALLEGRO_KEY_PGDN: 
		BEGIN
                  Thickness := Thickness - 0.5;
                  IF Thickness < 1.0 THEN Thickness := 1.0;
		END;
              ALLEGRO_KEY_UP:
	        BEGIN
	          INC (CurScreen);
	          IF CurScreen > HIGH (Screens) THEN CurScreen := 0
	        END;
              ALLEGRO_KEY_SPACE:
		Speed := 0;
              ALLEGRO_KEY_DOWN:
	        BEGIN
	          DEC (CurScreen);
	          IF CurScreen < LOW (Screens) THEN CurScreen := HIGH (Screens)
	        END;
	      END
	    END;
	  END
        END;
        IF Screens[CurScreen] <> NIL THEN Screens[CurScreen].Update;

        IF al_get_time - StartTime >= FixedTimestep THEN { break if we start taking too long }
          BREAK
      END;
      
      al_clear_to_color (Black);            

      IF Soft THEN
      BEGIN
        al_set_target_bitmap (Buffer);
        al_clear_to_color (Black)
      END;
      
      IF Background AND (bkg <> NIL) THEN
      BEGIN
        al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
        al_draw_scaled_bitmap (
	  bkg,
	  0, 0, al_get_bitmap_width (bkg), al_get_bitmap_height (bkg),
	  0, 0, ScreenW, ScreenH,
	  0
	)
      END;
      
      IF Clip THEN
        al_set_clipping_rectangle 
	  (ScreenW DIV 2, ScreenH DIV 2, ScreenW DIV 2, ScreenH DIV 2);
      
      IF Screens[CurScreen] <> NIL THEN Screens[CurScreen].Draw;

      al_set_clipping_rectangle (0, 0, ScreenW, ScreenH);

      IF Soft THEN
      BEGIN
        al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
        al_set_target_backbuffer (Display);
        al_draw_bitmap (Buffer, 0, 0, 0)
      END;

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_text (
        Font, SolidWhite, ScreenW DIV 2, ScreenH - 20, ALLEGRO_ALIGN_CENTRE,
       	Screens[CurScreen].Name);
      al_draw_text (
        Font, SolidWhite, 0, 0, 0,
       	Format ('FPS: %f', [FramesDone / (al_get_time - TimeDiff)])
      );
      al_draw_text (
        Font, SolidWhite, 0, 20, 0,
        'Change Screen (Up/Down). Esc to Quit.');
      al_draw_text (
        Font, SolidWhite, 0, 40, 0,
        Format ('Rotation (Left/Right/Space): %f', [Speed])
      );
      al_draw_text (
        Font, SolidWhite, 0, 60, 0,
        Format ('Thickness (PgUp/PgDown): %f', [Thickness])
      );
      IF Soft THEN
        al_draw_text (Font, SolidWhite, 0, 80, 0, 'Software (S): TRUE')
      ELSE
        al_draw_text (Font, SolidWhite, 0, 80, 0, 'Software (S): FALSE');
      IF Blend THEN
        al_draw_text (Font, SolidWhite, 0, 100, 0, 'Blending (L): TRUE')
      ELSE
        al_draw_text (Font, SolidWhite, 0, 100, 0, 'Blending (L): FALSE');
      IF Background THEN
        al_draw_text (Font, SolidWhite, 0, 120, 0, 'Background (B): TRUE')
      ELSE
        al_draw_text (Font, SolidWhite, 0, 120, 0, 'Background (B): FALSE');
      IF Clip THEN
        al_draw_text (Font, SolidWhite, 0, 140, 0, 'Clip (C): TRUE')
      ELSE
        al_draw_text (Font, SolidWhite, 0, 140, 0, 'Clip (C): FALSE');

      al_flip_display;
      INC (FramesDone)
    UNTIL Done;

    FOR CurScreen := LOW (Screens) TO HIGH (Screens) DO
      IF Screens[CurScreen] <> NIL THEN Screens[CurScreen].Free;

    al_destroy_bitmap (Buffer);
    al_destroy_event_queue (TimerQueue);
    al_destroy_Timer (Timer)
  END;

BEGIN
  UseShader := FALSE;
  Speed := ROTATE_SPEED;
  Thickness := 1;
  Blend := TRUE;
  Soft := TRUE;
  Background := TRUE;

  IF ParamCount > 0 THEN
  BEGIN
    IF ParamStr (1) = '--shader' THEN
      UseShader := TRUE
    ELSE
      AbortExample (Format (
        'Invalid command line option: %s.', [ParamStr(1)]
      ))
  END;

{ Initialize Allegro 5 and addons. }
  IF NOT al_init THEN
    AbortExample ('Could not init Allegro.');
  al_init_image_addon;
  al_init_font_addon;
  al_init_primitives_addon;
  InitPlatformSpecific;

  IF UseShader THEN
    al_set_new_display_flags (ALLEGRO_PROGRAMMABLE_PIPELINE);

{ Create a window to display things on: 640x480 pixels. }
  Display := al_create_display (ScreenW, ScreenH);
  IF Display = NIL THEN AbortExample ('Error creating display.');

{ Install the keyboard and mouse handlers. }
  IF NOT al_install_keyboard THEN AbortExample ('Error installing keyboard.');
  IF NOT al_install_mouse THEN AbortExample ('Error installing mouse.');

{ Load a font. }
  Font := al_load_font ('data/fixed_font.tga', 0, 0);
  IF Font = NIL THEN AbortExample ('Error loading "data/fixed_font.tga".');

  SolidWhite := al_map_rgba_f (1, 1, 1, 1);

  Bkg := al_load_bitmap ('data/bkg.png');

  Texture := al_load_bitmap ('data/texture.tga');

{ Make and set some color to draw with. }
  Black := al_map_rgba_f (0, 0, 0, 1);

{ Start the event queue to handle keyboard input and our timer. }
  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_mouse_event_source);

  al_set_window_title (Display, 'Primitives Example');

  al_identity_transform (Identity);

  Screens[0] := TLowPrimitives.Create;
  Screens[1] := TIndexedPrimitives.Create;
  Screens[2] := THighPrimitives.Create;
  Screens[3] := TTransformationsPrimitives.Create;
  Screens[4] := TFilledPrimitives.Create;
  Screens[5] := TIndexedFilledPrimitives.Create;
  Screens[6] := THighFilledPrimitives.Create;
  Screens[7] := TTexturePrimitives.Create;
  Screens[8] := TFilledTexturePrimitives.Create;
  Screens[9] := TCustomVertexFormatPrimitives.Create;
  Screens[10] := TVertexBuffers.Create;
  Screens[11] := TIndexedBuffers.Create;

  RunExample;

  al_destroy_bitmap (Texture);
  al_destroy_event_queue (Queue);
  al_destroy_display (Display);
END.
