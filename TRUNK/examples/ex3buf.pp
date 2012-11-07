PROGRAM ex3buf;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

 *    This program demonstrates the use of triple buffering. Several
 *    triangles are displayed rotating and bouncing on the screen
 *    until you press a key. Note that on some platforms you
 *    can't get real hardware triple buffering.  The Allegro code
 *    remains the same, but most likely the graphic driver will
 *    emulate it. Unfortunately, in these cases you can't expect
 *    the animation to be completely smooth and flicker free.
 *
 *    by Ñuño Martínez'<niunio(at)users.sourceforge.net> from an example
 *    by Shawn Hargreaves.
 *)

  USES
    allegro, alfixed;

  CONST
    NUM_SHAPES = 16;

  TYPE
    SHAPE = RECORD
      Clr: INTEGER;                  { color of the shape }
      X, Y: AL_FIXED;                { centre of the shape }
      Dir1, Dir2, Dir3: AL_FIXED;    { directions to the three corners }
      Dist1, Dist2, Dist3: AL_FIXED; { distances to the three corners }
      Xc, Yc, Ac: AL_FIXED;          { position and angle change values }
    END;

  VAR
    Shapes: ARRAY [1..NUM_SHAPES] OF SHAPE;
    TriplebufferNotAvailable: BOOLEAN;



(* randomly initialises a shape structure *)
  PROCEDURE InitShape (ShapeNdx: INTEGER);
  BEGIN
    Shapes[ShapeNdx].Clr := 1 + (Random (15));

  { randomly position the corners }
    Shapes[ShapeNdx].Dir1 := al_itofix (Random (256));
    Shapes[ShapeNdx].Dir2 := al_itofix (Random (256));
    Shapes[ShapeNdx].Dir3 := al_itofix (Random (256));

    Shapes[ShapeNdx].Dist1 := al_itofix (Random (64));
    Shapes[ShapeNdx].Dist2 := al_itofix (Random (64));
    Shapes[ShapeNdx].Dist3 := al_itofix (Random (64));

  { rand centre position and movement speed/direction }
    Shapes[ShapeNdx].X := al_itofix (Random (AL_SCREEN_W));
    Shapes[ShapeNdx].Y := al_itofix (Random (AL_SCREEN_H));
    Shapes[ShapeNdx].Ac := al_itofix (Random (9) - 4);
    Shapes[ShapeNdx].Xc := al_itofix (Random (7) - 2);
    Shapes[ShapeNdx].Yc := al_itofix (Random (7) - 2);
  END;



(* updates the position of a shape structure *)
  PROCEDURE MoveShape (ShapeNdx: INTEGER);
  BEGIN
    INC (Shapes[ShapeNdx].X, Shapes[ShapeNdx].Xc);
    INC (Shapes[ShapeNdx].Y, Shapes[ShapeNdx].Yc);

    INC (Shapes[ShapeNdx].Dir1, Shapes[ShapeNdx].Ac);
    INC (Shapes[ShapeNdx].Dir2, Shapes[ShapeNdx].Ac);
    INC (Shapes[ShapeNdx].Dir3, Shapes[ShapeNdx].Ac);

    IF ((Shapes[ShapeNdx].X <= 0) AND (Shapes[ShapeNdx].Xc < 0))
    OR ((Shapes[ShapeNdx].X >= al_itofix (AL_SCREEN_W)) AND (Shapes[ShapeNdx].Xc > 0))
    THEN BEGIN
      Shapes[ShapeNdx].Xc := -Shapes[ShapeNdx].Xc;
      Shapes[ShapeNdx].Ac := al_itofix (Random (9) - 4);
    END;

    IF ((Shapes[ShapeNdx].Y <= 0) AND (Shapes[ShapeNdx].Yc < 0))
    OR ((Shapes[ShapeNdx].Y >= al_itofix (AL_SCREEN_H)) AND (Shapes[ShapeNdx].Yc > 0))
    THEN BEGIN
      Shapes[ShapeNdx].Yc := -Shapes[ShapeNdx].Yc;
      Shapes[ShapeNdx].ac := al_itofix (Random (9) - 4);
    END;
  END;



(* draws a frame of the animation *)
  PROCEDURE Draw (b: AL_BITMAPptr);
  VAR
    c: INTEGER;
  BEGIN
    al_acquire_bitmap (b);

    al_clear_bitmap (b);

    FOR c := Low (Shapes) TO High (Shapes) DO
    BEGIN
      al_triangle (b,
	al_fixtoi (Shapes[c].x + al_fixmul(Shapes[c].Dist1, al_fixcos (Shapes[c].Dir1))),
	al_fixtoi (Shapes[c].y + al_fixmul(Shapes[c].Dist1, al_fixsin (Shapes[c].Dir1))),
	al_fixtoi (Shapes[c].x + al_fixmul(Shapes[c].Dist2, al_fixcos (Shapes[c].Dir2))),
	al_fixtoi (Shapes[c].y + al_fixmul(Shapes[c].Dist2, al_fixsin (Shapes[c].Dir2))),
	al_fixtoi (Shapes[c].x + al_fixmul(Shapes[c].Dist3, al_fixcos (Shapes[c].Dir3))),
	al_fixtoi (Shapes[c].y + al_fixmul(Shapes[c].Dist3, al_fixsin (Shapes[c].Dir3))),
	Shapes[c].Clr
      );

      MoveShape (c);
    END;

    IF TriplebufferNotAvailable THEN
      al_textout_ex (b, al_font, 'Simulated triple buffering', 0, 0, 255, -1)
    ELSE
      al_textout_ex (b, al_font, 'Real triple buffering', 0, 0, 255, -1);

    al_release_bitmap (b);
  END;



(* main animation control loop *)
  PROCEDURE TripleBuffer (Page1, Page2, Page3: AL_BITMAPptr);
  VAR
    ActivePage: AL_BITMAPptr;
    Page: INTEGER;
  BEGIN
    ActivePage := Page1;
    Page := 0;

    REPEAT
    { draw a frame }
      Draw (ActivePage);

    { make sure the last flip request has actually happened }
      WHILE al_poll_scroll DO
        ;

    { post a request to display the page we just drew }
      al_request_video_bitmap (ActivePage);

    { update counters to point to the next page }
      CASE Page OF
	0: BEGIN Page := 1;  ActivePage := Page2; END;
	1: BEGIN Page := 2;  ActivePage := Page3; END;
	2: BEGIN Page := 0;  ActivePage := Page1; END;
      END;
    UNTIL al_keypressed;

    al_clear_keybuf;
  END;



(* Helper function to create video pages. *)
  FUNCTION CreateVideoPage: AL_BITMAPptr; INLINE;
  BEGIN
    CreateVideoPage := al_create_video_bitmap (AL_SCREEN_W, AL_SCREEN_H);
    IF CreateVideoPage = NIL THEN
      al_message ('Unable to create three video memory pages'#10);
  END;

VAR
  Page1, Page2, Page3: AL_BITMAPptr;
  c, w, h: INTEGER;
BEGIN (* The program starts here. *)
  w := 640;
  h := 480;

{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_timer;
  al_install_mouse;
  al_install_keyboard;

{ see comments in exflip.c }
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, w, h, 0, h * 3) THEN
    IF NOT al_set_gfx_mode(AL_GFX_SAFE, w, h, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Shows an error message. }
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

  al_set_palette (al_desktop_palette);

{ if triple buffering isn't available, try to enable it }
  IF (al_gfx_capabilities AND AL_GFX_CAN_TRIPLE_BUFFER) <> AL_GFX_CAN_TRIPLE_BUFFER THEN
    al_enable_triple_buffer;

{ if that didn't work, give up }
  IF (al_gfx_capabilities AND AL_GFX_CAN_TRIPLE_BUFFER) <> AL_GFX_CAN_TRIPLE_BUFFER THEN
    TriplebufferNotAvailable := TRUE
  ELSE
    TriplebufferNotAvailable := FALSE;

{ allocate three sub bitmaps to access pages of the screen }
  Page1 := CreateVideoPage;
  IF Page1 = NIL THEN
    EXIT;
  Page2 := CreateVideoPage;
  IF Page2 = NIL THEN
  BEGIN
    al_destroy_bitmap (Page1);
    EXIT;
  END;
  Page3 := CreateVideoPage;
  IF Page3 = NIL THEN
  BEGIN
    al_destroy_bitmap (Page2);
    al_destroy_bitmap (Page1);
    EXIT;
  END;

{ initialise the shapes }
  FOR C := Low (Shapes) TO High (Shapes) DO
    InitShape (C);

  TripleBuffer (Page1, Page2, Page3);

  al_destroy_bitmap (Page1);
  al_destroy_bitmap (Page2);
  al_destroy_bitmap (Page3);
END.
