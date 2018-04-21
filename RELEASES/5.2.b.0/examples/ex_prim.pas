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
 *      All primitives are rendered using the additive blender, so overdraw
 *      will manifest itself as overly bright pixels.
 *
 *      By Pavel Sountsov.
 *      Translated to Pascal by Ñuño Martínez.
 *
 *      See README for copyright information.
 *)
(*
  Copyright (c) 2012-2018 Guillermo Martínez J.

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

{$IFDEF FPC}
{ Needed to support classes and C-style pointers. }
  {$IF NOT DEFINED(FPC_OBJFPC)} {$MODE OBJFPC} {$ENDIF}
{ Needed to make the executable suitable for  modern Windows OS. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

{$IFDEF DCC}
{ Pointer arithmetic needed by this example.
  Not all Delphi versions support it though. }
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion >= 20.0}
      {$POINTERMATH ON}
    {$ELSE}
      {$MESSAGE error 'Need pointer arithmetics.'}
    {$ENDIF}
  {$ELSE}
    {$MESSAGE error 'Need pointer arithmetics.'}
  {$ENDIF}
{$ENDIF}

  USES
    Common,
    Allegro5, al5Base, al5font, al5image, al5nativedlg, al5primitives,
    sysutils;

  CONST
    ScreenW = 800; ScreenH = 600;
    RefreshRate = 60;
    FixedTimestep = 1 / RefreshRate;
    NUM_SCREENS = 12;
    ROTATE_SPEED = 0.0001;

  TYPE
  (* Base class for example screens. *)
    TCustomScreen = CLASS (TObject)
    PRIVATE
      fName: ANSISTRING;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create (CONST aName: ANSISTRING);
    (* Updates the example.

      By default builds MainTrans rotating by Theta angle. *)
      PROCEDURE Update; VIRTUAL;
    (* Draws the screen. *)
      PROCEDURE Draw; VIRTUAL; ABSTRACT;

    (* Screen name. *)
      PROPERTY Name: ANSISTRING READ fName;
    END;



  (* The example object.

     This is something similar to VCL/LCL TApplication class. *)
    TPrimitivesExample = CLASS (TObject)
    PRIVATE
      fTerminated,
      fUseShader, fSoft, fBlend, fDrawBkg, fClip: BOOLEAN;
      fScreens: ARRAY [1..NUM_SCREENS] OF TCustomScreen;
      fCurScreen, fFramesDone: INTEGER;
      fDisplay: ALLEGRO_DISPLAYptr;
      fFont: ALLEGRO_FONTptr;
      fBkg, fTexture, fBuffer: ALLEGRO_BITMAPptr;
      fQueue: ALLEGRO_EVENT_QUEUEptr;
      fIdentity, fTransformationMatrix: ALLEGRO_TRANSFORM;
      fSpeed, fThickness, fTheta: SINGLE;
      fRealTime, fGameTime, fStartTime, fTimeDiff, fFrameDuration: DOUBLE;

    (* Updates the example.  Increases Theta angle by Speed. *)
      PROCEDURE Update;
    (* Renders the example.

      It applyes blender, transformation matrix, calls screen's draw method and
      appyes identity matrix. *)
      PROCEDURE Draw;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the application. *)
      PROCEDURE Initialize;
    (* Runs the example. *)
      PROCEDURE Run;
    (* Terminates the example. *)
      PROCEDURE Terminate;
    (* Shows an error message box. *)
      PROCEDURE ShowErrorMessage (CONST aMessage: ANSISTRING);
    (* Builds the transformation matrix. *)
      PROCEDURE BuildTransform (ax, ay, sx, sy, aTheta: AL_FLOAT);
    (* Draws text telling the example isn't supported. *)
      PROCEDURE DrawUnsupported;

    (* Texture used by primitives. *)
      PROPERTY Texture: ALLEGRO_BITMAPptr READ fTexture;
    (* Theta angle. *)
      PROPERTY Theta: SINGLE READ fTheta;
    (* Trace thickness. *)
      PROPERTY Thickness: SINGLE READ fThickness;
    (* Tells if it's using software rendering. *)
      PROPERTY Soft: BOOLEAN READ fSoft;
    END;



  (* Low-level primitives. *)
    TLowPrimitives = CLASS (TCustomScreen)
    PRIVATE
      Vtx, Vtx2: ARRAY [0..12] OF ALLEGRO_VERTEX;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Indexed primitives. *)
    TIndexedPrimitives = CLASS (TCustomScreen)
    PRIVATE
      Indices1, Indices2, Indices3: ARRAY [0..3] OF INTEGER;
      Vtx, Vtx2: ARRAY [0..12] OF ALLEGRO_VERTEX;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Executes the example. *)
      PROCEDURE Update; OVERRIDE;
    (* Draws the Screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* High-level primitives. *)
    THighPrimitives = CLASS (TCustomScreen)
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Transformations. *)
    TTransformationsPrimitives = CLASS (TCustomScreen)
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Updates the example. *)
      PROCEDURE Update; OVERRIDE;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Filled primitives. *)
    TFilledPrimitives = CLASS (TCustomScreen)
    PRIVATE
      vtx: ARRAY [0..20] OF ALLEGRO_VERTEX;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Indexed filled primitives. *)
    TIndexedFilledPrimitives = CLASS (TCustomScreen)
    PRIVATE
      vtx: ARRAY [0..20] OF ALLEGRO_VERTEX;
      Indices1, Indices2, Indices3: ARRAY [0..5] OF INTEGER;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Updates the example. *)
      PROCEDURE Update; OVERRIDE;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* High-level filled primitives. *)
    THighFilledPrimitives = CLASS (TCustomScreen)
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Textured primitives. *)
    TTexturePrimitives = CLASS (TCustomScreen)
    PRIVATE
      Vtx, Vtx2: ARRAY [0..12] OF ALLEGRO_VERTEX;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Filled & testured primitives. *)
    TFilledTexturePrimitives = CLASS (TCustomScreen)
    PRIVATE
      vtx: ARRAY [0..20] OF ALLEGRO_VERTEX;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Stores a custom vertex. *)
    CUSTOM_VERTEX = RECORD
    { Use types defined at al5Base, so we know their size. }
      u, v: AL_INT16;
      x, y: AL_INT16;
      Color: ALLEGRO_COLOR;
      junk: ARRAY [0..5] OF AL_INT;
    END;



  (* Primitives using the premious defined custom vertex struct. *)
    TCustomVertexFormatPrimitives = CLASS (TCustomScreen)
    PRIVATE
      vtx: ARRAY [0..3] OF CUSTOM_VERTEX;
      Decl: ALLEGRO_VERTEX_DECLptr;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Vertex buffers. *)
    TVertexBuffers = CLASS (TCustomScreen)
    PRIVATE
      vtx, vtx2: ARRAY [0..12] OF ALLEGRO_VERTEX;
      vbuff, vbuff2: ALLEGRO_VERTEX_BUFFERptr;
      NoSoft, NoSoft2: BOOLEAN;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;



  (* Indexed vertex buffers.

    Note this example is buggy (or the primitives addon is) so it's not used by
    the application. *)
    TIndexedBuffers = CLASS (TCustomScreen)
    PRIVATE
      vbuff: ALLEGRO_VERTEX_BUFFERptr;
      ibuff: ALLEGRO_INDEX_BUFFERptr;
      Soft: BOOLEAN;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Updates screen. *)
      PROCEDURE Update; OVERRIDE;
    (* Draws the screen. *)
      PROCEDURE Draw; OVERRIDE;
    END;

  VAR
  (* The example object.  This is something like the VCL/LCL Application
     object. *)
    Example: TPrimitivesExample;
  (* Some colors. *)
    White, SolidWhite, SolidBlack: ALLEGRO_COLOR;
  (* Points used in the spline example at THighPrimitives.Draw and
     TTransformationsPrimitives.Draw. *)
    SplinePoints: ALLEGRO_SPLINE_CONTROL_POINTS = (
      -300, -200,
      700, 200,
      -700, 200,
      300, -200
    );



(*
 * TCustomScreen
 ***************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TCustomScreen.Create (CONST aName: ANSISTRING);
  BEGIN
    INHERITED Create;
    fName := aName
  END;



(* Executes screen. *)
  PROCEDURE TCustomScreen.Update;
  BEGIN
    Example.BuildTransform (ScreenW / 2, ScreenH / 2, 1, 1, Example.Theta)
  END;



(*
 * TPrimitivesExample
 ***************************************************************************)

(* Updates the example. *)
  PROCEDURE TPrimitivesExample.Update;
  VAR
    Event: ALLEGRO_EVENT;
  BEGIN
  { Events. }
    WHILE NOT fTerminated AND al_get_next_event (fQueue, Event) DO
    BEGIN
      CASE Event._type OF
      ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
	BEGIN
	  INC (fCurScreen);
	  IF fCurScreen > HIGH (fScreens) THEN fCurScreen := LOW (fScreens)
	END;
      ALLEGRO_EVENT_DISPLAY_CLOSE:
	  SELF.Terminate;
      ALLEGRO_EVENT_KEY_CHAR:
	CASE Event.keyboard.keycode OF
	ALLEGRO_KEY_ESCAPE:
	  SELF.Terminate;
	ALLEGRO_KEY_S:
	  BEGIN
	    fSoft := NOT fSoft;
	    fTimeDiff := al_get_time;
	    fFramesDone := 0
	  END;
	ALLEGRO_KEY_C:
	  BEGIN
	    fClip := NOT fClip;
	    fTimeDiff := al_get_time;
	    fFramesDone := 0
	  END;
	ALLEGRO_KEY_L:
	  BEGIN
	    fBlend := NOT fBlend;
	    fTimeDiff := al_get_time;
	    fFramesDone := 0
	  END;
	ALLEGRO_KEY_B:
	  BEGIN
	    fDrawBkg := NOT fDrawBkg;
	    fTimeDiff := al_get_time;
	    fFramesDone := 0
	  END;
	ALLEGRO_KEY_LEFT:
	  fSpeed := fSpeed - ROTATE_SPEED;
	ALLEGRO_KEY_RIGHT:
	  fSpeed := fSpeed + ROTATE_SPEED;
	ALLEGRO_KEY_SPACE:
	  fSpeed := 0;
	ALLEGRO_KEY_PGUP:
	  BEGIN
	    fThickness := fThickness + 0.5;
	    IF fThickness < 1.0 THEN fThickness := 1.0
	  END;
	ALLEGRO_KEY_PGDN:
	  BEGIN
	    fThickness := fThickness - 0.5;
	    IF fThickness < 1.0 THEN fThickness := 1.0
	  END;
	ALLEGRO_KEY_UP:
	  BEGIN
	    INC (fCurScreen);
	    IF fCurScreen > HIGH (fScreens) THEN fCurScreen := LOW (fScreens)
	  END;
	ALLEGRO_KEY_DOWN:
	  BEGIN
	    DEC (fCurScreen);
	    IF fCurScreen < LOW (fScreens) THEN fCurScreen := HIGH (fScreens)
	  END;
	END
      END
    END;
  { Updates screens. }
    fTheta := fTheta + fSpeed;
    fScreens[fCurScreen].Update
  END;



(* Renders the example. *)
  PROCEDURE TPrimitivesExample.Draw;

  { Helper to print boolean statuses. }
    PROCEDURE ShowBoolean
      (CONST aY: INTEGER; CONST aLabel: ANSISTRING; CONST aState: BOOLEAN);
    BEGIN
      IF aState THEN
        al_draw_text (fFont, SolidWhite, 0, aY, 0, aLabel + ': TRUE')
      ELSE
        al_draw_text (fFont, SolidWhite, 0, aY, 0, aLabel + ': FALSE')
    END;

  BEGIN
  { Prepare rendering. }
    al_clear_to_color (SolidBlack);
    IF fSoft THEN
    BEGIN
      al_set_target_bitmap (fBuffer);
      al_clear_to_color (SolidBlack)
    END;
  { Background. }
    IF fDrawBkg AND (fBkg <> NIL) THEN
    BEGIN
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      al_draw_scaled_bitmap (
	fBkg,
	0, 0, al_get_bitmap_width (fbkg), al_get_bitmap_height (fbkg),
	0, 0, ScreenW, ScreenH,
	0
      )
    END;
    IF fClip THEN
      al_set_clipping_rectangle
	(ScreenW DIV 2, ScreenH DIV 2, ScreenW DIV 2, ScreenH DIV 2);
  { Set blender. }
    IF fBlend THEN
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE)
    ELSE
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
  { Draw example. }
    al_use_transform (fTransformationMatrix);
    fScreens[fCurScreen].Draw;
  { Restore. }
    al_use_transform (fIdentity);
    al_set_clipping_rectangle (0, 0, ScreenW, ScreenH);
    IF fSoft THEN
    BEGIN
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      al_set_target_backbuffer (fDisplay);
      al_draw_bitmap (fBuffer, 0, 0, 0)
    END;
  { Show states. }
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    al_draw_text (
      fFont, SolidWhite, ScreenW DIV 2, ScreenH - 20, ALLEGRO_ALIGN_CENTRE,
      fScreens[fCurScreen].Name);
    al_draw_text (
      fFont, SolidWhite, 0, 0, 0,
      Format ('FPS: %.2f', [fFramesDone / (al_get_time - fTimeDiff)])
    );
    al_draw_text (
      fFont, SolidWhite, 0, 20, 0, 'Change Screen (Up/Down). Esc to Quit.');
    al_draw_text (
      fFont, SolidWhite, 0, 40, 0,
      Format ('Rotation (Left/Right/Space): %.4f', [fSpeed])
    );
    al_draw_text (
      fFont, SolidWhite, 0, 60, 0,
      Format ('Thickness (PgUp/PgDown): %.2f', [fThickness])
    );
    ShowBoolean (80, 'Software (S)', fSoft);
    ShowBoolean (100, 'Blending (L)', fBlend);
    ShowBoolean (120, 'Background (B)', fDrawBkg);
    ShowBoolean (140, 'Clip (C)', fClip);

    al_flip_display;
  END;



(* Constructor. *)
  CONSTRUCTOR TPrimitivesExample.Create;
  BEGIN
    INHERITED Create;
    fDisplay := NIL;
    fBkg := NIL;
    fTexture := NIL;
    fBuffer := NIL;

    fUseShader := FALSE;
    fBlend := TRUE;
    fTheta := 0;
    fSpeed := ROTATE_SPEED;
    fThickness := 1;
    fSoft := TRUE;
    fDrawBkg := TRUE;
    fClip := FALSE
  END;



(* Destructor. *)
  DESTRUCTOR TPrimitivesExample.Destroy;
  VAR
    Cnt: INTEGER;
  BEGIN
    IF fBuffer <> NIL THEN al_destroy_bitmap (fBuffer);
    FOR Cnt := LOW (fScreens) TO HIGH (fScreens) DO fScreens[Cnt].Free;
    IF fQueue <> NIL THEN al_destroy_event_queue (fQueue);
    IF fTexture <> NIL THEN al_destroy_bitmap (fTexture);
    IF fBkg <> NIL THEN al_destroy_bitmap (fBkg);
    IF fFont <> NIL THEN al_destroy_font (fFont);
    IF fDisplay <> NIL THEN al_destroy_display (fDisplay);
    INHERITED Destroy
  END;



(* Initializes the application. *)
  PROCEDURE TPrimitivesExample.Initialize;
  VAR
    Old: INTEGER;
  BEGIN
  { Check parameters. }
    IF ParamCount > 0 THEN
    BEGIN
      IF ParamStr (1) = '--shader' THEN
	fUseShader := TRUE
      ELSE BEGIN
	ShowErrorMessage (
	  Format ('Invalid command line option: %s.', [ParamStr (1)])
	);
	EXIT
      END
    END;
  { Initializes Allegro 5 and addons. }
    IF NOT al_init THEN
    BEGIN
      ShowErrorMessage ('Could not init Allegro.');
      EXIT
    END;
    al_init_image_addon;
    al_init_font_addon;
    al_init_primitives_addon;
    InitPlatformSpecific;
    IF NOT al_install_keyboard THEN
    BEGIN
      ShowErrorMessage ('Error installing keyboard.');
      EXIT
    END;
    IF NOT al_install_mouse THEN
    BEGIN
      ShowErrorMessage ('Error installing mouse.');
      EXIT
    END;
  { Display. }
    IF fUseShader THEN
      al_set_new_display_flags (ALLEGRO_PROGRAMMABLE_PIPELINE);
    fDisplay := al_create_display (ScreenW, ScreenH);
    IF fDisplay = NIL THEN
    BEGIN
      ShowErrorMessage ('Error creating display.');
      EXIT
    END;
    al_set_window_title (fDisplay, 'Primitives Example');
  { Make and set some color to draw with. }
    SolidBlack := al_map_rgba_f (0, 0, 0, 1);
    SolidWhite := al_map_rgba_f (1, 1, 1, 1);
    White := al_map_rgb_f (1, 1, 1);
  { Load a font. }
    fFont := al_load_font ('data/fixed_font.tga', 0, 0);
    IF fFont = NIL THEN
    BEGIN
      ShowErrorMessage ('Error loading "data/fixed_font.tga".');
      EXIT
    END;
  { Textures. }
    fBkg := al_load_bitmap ('data/bkg.png');
    IF fBkg = NIL THEN
    BEGIN
      ShowErrorMessage ('Error loading "data/bkg.png".');
      EXIT
    END;
    fTexture := al_load_bitmap ('data/texture.tga');
    IF fTexture = NIL THEN
    BEGIN
      ShowErrorMessage ('Error loading "data/texture.tga".');
      EXIT
    END;
  { Start the event queue. }
    fQueue := al_create_event_queue;
    al_register_event_source (fQueue, al_get_keyboard_event_source);
    al_register_event_source (fQueue, al_get_display_event_source (fDisplay));
    al_register_event_source (fQueue, al_get_mouse_event_source);

    al_identity_transform (fIdentity);
  { Create the list of screens. }
    fScreens[1] := TLowPrimitives.Create;
    fScreens[2] := TIndexedPrimitives.Create;
    fScreens[3] := THighPrimitives.Create;
    fScreens[4] := TTransformationsPrimitives.Create;
    fScreens[5] := TFilledPrimitives.Create;
    fScreens[6] := TIndexedFilledPrimitives.Create;
    fScreens[7] := THighFilledPrimitives.Create;
    fScreens[8] := TTexturePrimitives.Create;
    fScreens[9] := TFilledTexturePrimitives.Create;
    fScreens[10] := TCustomVertexFormatPrimitives.Create;
    fScreens[11] := TVertexBuffers.Create;
    fScreens[12] := TIndexedBuffers.Create;
  { Backbuffer. }
    Old := al_get_new_bitmap_flags;
    al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
    fBuffer := al_create_bitmap (ScreenW, ScreenH);
    IF fBuffer = NIL THEN
    BEGIN
      ShowErrorMessage ('Could not create buffer.');
      EXIT
    END;
    al_set_new_bitmap_flags (Old);

    fTerminated := FALSE
  END;



(* Runs the example. *)
  PROCEDURE TPrimitivesExample.Run;
  BEGIN
    IF fTerminated THEN EXIT; { Skip execution. }

    fRealTime := al_get_time;
    fFramesDone := 0;
    fTimeDiff := al_get_time;
    fGameTime := al_get_time;
  { Main loop. }
    fCurScreen := LOW (fScreens);
    REPEAT
      fFrameDuration := al_get_time - fRealTime;
      al_rest (FixedTimestep - fFrameDuration); { rest at least fixed_dt }
      fFrameDuration := al_get_time - fRealTime;
      fRealTime := al_get_time;
      fGameTime := fRealTime;
      IF fRealTime - fGameTime > fFrameDuration THEN
      { eliminate excess overflow }
	fGameTime := fGameTime + FixedTimestep * TRUNC ((fRealTime - fGameTime) / FixedTimestep);
      WHILE NOT fTerminated AND (fRealTime - fGameTime >= 0) DO
      BEGIN
	fStartTime := al_get_time;
	fGameTime := fGameTime + FixedTimestep;
	SELF.Update;
	IF al_get_time - fStartTime >= FixedTimestep THEN
	{ break if we start taking too long }
	  BREAK
      END;
      SELF.Draw;
      INC (fFramesDone)
    UNTIL fTerminated
  END;



(* Terminates the example. *)
  PROCEDURE TPrimitivesExample.Terminate;
  BEGIN
    fTerminated := TRUE
  END;



(* Shows an error message box. *)
  PROCEDURE TPrimitivesExample.ShowErrorMessage (CONST aMessage: ANSISTRING);
  BEGIN
    al_show_native_message_box
      (fDisplay, 'Error', 'Cannot run example', aMessage, '', 0);
    SELF.Terminate
  END;



(* Builds the transformation matrix. *)
  PROCEDURE TPrimitivesExample.BuildTransform (ax, ay, sx, sy, aTheta: AL_FLOAT);
  BEGIN
    al_build_transform (fTransformationMatrix, ax, ay, sx, sy, aTheta)
  END;



(* Draws text telling the example isn't supported. *)
  PROCEDURE TPrimitivesExample.DrawUnsupported;
  BEGIN
    al_draw_text (
      fFont, White, 0, 0, 0,
      Format ('%s not supported', [fScreens[fCurScreen].Name])
    )
  END;



(*
 * TLowPrimitives
 ***************************************************************************)


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
	(Ndx + 1) MOD 3 * 64,
	(Ndx + 2) MOD 3 * 64,
	(Ndx    ) MOD 3 * 64);
         
      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      Vtx[Ndx].color := color;
      Vtx2[Ndx].color := color
    END
  END;



(* Draws screen. *)
  PROCEDURE TLowPrimitives.Draw;
  BEGIN
    al_draw_prim (Vtx, NIL, NIL, 0, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_prim (Vtx, NIL, NIL, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_prim (Vtx, NIL, NIL, 9, 13, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_prim (Vtx2, NIL, NIL, 0, 13, ALLEGRO_PRIM_POINT_LIST);
  END;



(*
 * TIndexedPrimitives
 ***************************************************************************)

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
	(Ndx + 1) MOD 3 * 64,
	(Ndx + 2) MOD 3 * 64,
	(Ndx    ) MOD 3 * 64
      );

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
      Indices3[Ndx] := TRUNC (al_get_time() + Ndx + 8) MOD 13
    END
  END;



(* Draws screen. *)
  PROCEDURE TIndexedPrimitives.Draw;
  BEGIN
    al_draw_indexed_prim (vtx, NIL, NIL, indices1, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_indexed_prim (vtx, NIL, NIL, indices2, 4, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_indexed_prim (vtx, NIL, NIL, indices3, 4, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_indexed_prim (vtx2, NIL, NIL, indices3, 4, ALLEGRO_PRIM_POINT_LIST);
  END;



(*
 * THighPrimitives
 ***************************************************************************)

(* Constructor. *)
  CONSTRUCTOR THighPrimitives.Create;
  BEGIN
    INHERITED Create ('High Level Primitives')
  END;



(* Draws screen. *)
  PROCEDURE THighPrimitives.Draw;
  BEGIN
    al_draw_line (-300, -200, 300, 200, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_triangle (-150, -250, 0, 250, 150, -250, al_map_rgba_f (0.5, 0, 0.5, 1), Example.Thickness);
    al_draw_rectangle (-300, -200, 300, 200, al_map_rgba_f (0.5, 0, 0, 1), Example.Thickness);
    al_draw_rounded_rectangle (-200, -125, 200, 125, 50, 100, al_map_rgba_f (0.2, 0.2, 0, 1), Example.Thickness);

    al_draw_ellipse (0, 0, 300, 150, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_elliptical_arc (-20, 0, 300, 200, -ALLEGRO_PI / 2, -ALLEGRO_PI, al_map_rgba_f (0.25, 0.25, 0.5, 1), Example.Thickness);
    al_draw_arc (0, 0, 200, -ALLEGRO_PI / 2, ALLEGRO_PI, al_map_rgba_f (0.5, 0.25, 0, 1), Example.Thickness);
    al_draw_spline (SplinePoints, al_map_rgba_f (0.1, 0.2, 0.5, 1), Example.Thickness);
    al_draw_pieslice (0, 25, 150, ALLEGRO_PI * 3 / 4, -ALLEGRO_PI / 2, al_map_rgba_f (0.4, 0.3, 0.1, 1), Example.Thickness)
  END;



(*
 * TTransformationsPrimitives
 ***************************************************************************)

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
    Example.BuildTransform (
      ScreenW / 2, ScreenH / 2, sin (t), cos (t), Example.Theta
    )
  END;



(* Draws screen. *)
  PROCEDURE TTransformationsPrimitives.Draw;
  BEGIN
    al_draw_line (-300, -200, 300, 200, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_triangle (-150, -250, 0, 250, 150, -250, al_map_rgba_f (0.5, 0, 0.5, 1), Example.Thickness);
    al_draw_rectangle (-300, -200, 300, 200, al_map_rgba_f (0.5, 0, 0, 1), Example.Thickness);
    al_draw_rounded_rectangle (-200, -125, 200, 125, 50, 100, al_map_rgba_f (0.2, 0.2, 0, 1), Example.Thickness);

    al_draw_ellipse (0, 0, 300, 150, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_elliptical_arc (-20, 0, 300, 200, -ALLEGRO_PI / 2, -ALLEGRO_PI, al_map_rgba_f (0.25, 0.25, 0.5, 1), Example.Thickness);
    al_draw_arc (0, 0, 200, -ALLEGRO_PI / 2, ALLEGRO_PI, al_map_rgba_f (0.5, 0.25, 0, 1), Example.Thickness);
    al_draw_spline (SplinePoints, al_map_rgba_f (0.1, 0.2, 0.5, 1), Example.Thickness);
    al_draw_pieslice (0, 25, 150, ALLEGRO_PI * 3 / 4, -ALLEGRO_PI / 2, al_map_rgba_f (0.4, 0.3, 0.1, 1), Example.Thickness)
  END;



(*
 * TFilledPrimitives
 ***************************************************************************)

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
	(    Ndx    ) MOD 3 * 64);

      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].color := color
    END
  END;



(* Draws screen. *)
  PROCEDURE TFilledPrimitives.Draw;
  BEGIN
    al_draw_prim (vtx, NIL, NIL, 0, 6, ALLEGRO_PRIM_TRIANGLE_FAN);
    al_draw_prim (vtx, NIL, NIL, 7, 13, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_prim (vtx, NIL, NIL, 14, 20, ALLEGRO_PRIM_TRIANGLE_STRIP)
  END;



(*
 * TIndexedFilledPrimitives
 ***************************************************************************)

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
	(    Ndx    ) MOD 3 * 64);

      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].color := color
    END
  END;



(* Updates screen. *)
  PROCEDURE TIndexedFilledPrimitives.Update;
  VAR
    Ndx: INTEGER;
  BEGIN
    FOR Ndx := LOW (Indices1) TO HIGH (Indices1) DO
    BEGIN
      Indices1[Ndx] := (TRUNC (al_get_time) + Ndx) MOD 20 + 1;
      Indices2[Ndx] := (TRUNC (al_get_time) + Ndx + 6) MOD 20 + 1;
      IF Ndx > 0 THEN
        Indices3[Ndx] := (TRUNC (al_get_time) + Ndx + 12) MOD 20 + 1
    END;
    Example.BuildTransform (ScreenW / 2, ScreenH / 2, 1, 1, Example.Theta)
  END;



(* Draws screen. *)
  PROCEDURE TIndexedFilledPrimitives.Draw;
  BEGIN
    al_draw_indexed_prim (vtx, NIL, NIL, Indices1, 6, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_indexed_prim (vtx, NIL, NIL, Indices2, 6, ALLEGRO_PRIM_TRIANGLE_STRIP);
    al_draw_indexed_prim (vtx, NIL, NIL, Indices3, 6, ALLEGRO_PRIM_TRIANGLE_FAN)
  END;



(*
 * THighFilledPrimitives
 ***************************************************************************)

(* Constructor. *)
  CONSTRUCTOR THighFilledPrimitives.Create;
  BEGIN
    INHERITED Create ('High Level Filled Primitives');
  END;



(* Draws screen. *)
  PROCEDURE THighFilledPrimitives.Draw;
  BEGIN
    al_draw_filled_triangle (-100, -100, -150, 200, 100, 200, al_map_rgb_f (0.5, 0.7, 0.3));
    al_draw_filled_rectangle (20, -50, 200, 50, al_map_rgb_f (0.3, 0.2, 0.6));
    al_draw_filled_ellipse (-250, 0, 100, 150, al_map_rgb_f (0.3, 0.3, 0.3));
    al_draw_filled_rounded_rectangle (50, -250, 350, -75, 50, 70, al_map_rgb_f (0.4, 0.2, 0));
    al_draw_filled_pieslice (200, 125, 50, ALLEGRO_PI / 4, 3 * ALLEGRO_PI / 2, al_map_rgb_f (0.3, 0.3, 0.1))
  END;



(*
 * TTexturePrimitives
 ***************************************************************************)

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
	(Ndx + 1) MOD 3 * 64,
	(Ndx + 2) MOD 3 * 64,
	(Ndx    ) MOD 3 * 64);

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



(* Draws screen. *)
  PROCEDURE TTexturePrimitives.Draw;
  BEGIN
    al_draw_prim (Vtx, NIL, Example.Texture, 0, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_prim (Vtx, NIL, Example.Texture, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_prim (Vtx, NIL, Example.Texture, 9, 13, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_prim (Vtx2, NIL, Example.Texture, 0, 13, ALLEGRO_PRIM_POINT_LIST)
  END;



(*
 * TFilledTexturePrimitives
 ***************************************************************************)

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
	(    Ndx    ) MOD 3 * 64);

      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].u := 64 * x / 100; vtx[Ndx].v := 64 * y / 100;
      IF Ndx < 10 THEN
        vtx[Ndx].color := al_map_rgba_f(1, 1, 1, 1)
      ELSE
        vtx[Ndx].color := Color
    END
  END;



(* Draws screen. *)
  PROCEDURE TFilledTexturePrimitives.Draw;
  BEGIN
    al_draw_prim (vtx, NIL, Example.Texture, 0, 6, ALLEGRO_PRIM_TRIANGLE_FAN);
    al_draw_prim (vtx, NIL, Example.Texture, 7, 13, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_prim (vtx, NIL, Example.Texture, 14, 20, ALLEGRO_PRIM_TRIANGLE_STRIP)
  END;



(*
 * TCustomVertexFormatPrimitives
 ***************************************************************************)

VAR
{ Used to create the vertices by the constructor bellow. }
  Elems: ARRAY [0..3] OF ALLEGRO_VERTEX_ELEMENT = (
    (attribute: ALLEGRO_PRIM_POSITION; storage: ALLEGRO_PRIM_SHORT_2; offset: 4),
    (attribute: ALLEGRO_PRIM_TEX_COORD_PIXEL; storage: ALLEGRO_PRIM_SHORT_2; offset: 0),
    (attribute: ALLEGRO_PRIM_COLOR_ATTR; storage: ALLEGRO_PRIM_STORAGE_NONE; offset: 8),
    (attribute: ALLEGRO_PRIM_ATTR_NONE; storage: ALLEGRO_PRIM_STORAGE_NONE; offset: 0)
  );



(* Constructor. *)
  CONSTRUCTOR TCustomVertexFormatPrimitives.Create;
  VAR
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



(* Draws screen. *)
  PROCEDURE TCustomVertexFormatPrimitives.Draw;
  BEGIN
    al_draw_prim_ex (@vtx, Decl, Example.Texture, 0, 4, ALLEGRO_PRIM_TRIANGLE_FAN);
  END;



(*
 * TVertexBuffers
 ***************************************************************************)

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
	(Ndx + 1) MOD 3 * 64,
	(Ndx + 2) MOD 3 * 64,
	(Ndx    ) MOD 3 * 64);

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



(* Draws screen. *)
  PROCEDURE TVertexBuffers.Draw;
  BEGIN
    IF (vbuff <> NIL) AND NOT (Example.Soft AND NoSoft) THEN
    BEGIN
      al_draw_vertex_buffer (vbuff, NIL, 0, 4, ALLEGRO_PRIM_LINE_LIST);
      al_draw_vertex_buffer (vbuff, NIL, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
      al_draw_vertex_buffer (vbuff, NIL, 9, 13, ALLEGRO_PRIM_LINE_LOOP)
    END
    ELSE
      Example.DrawUnsupported;

    IF (vbuff2 <> NIL) AND NOT (Example.Soft AND NoSoft2) THEN
      al_draw_vertex_buffer (vbuff2, NIL, 0, 13, ALLEGRO_PRIM_POINT_LIST)
    ELSE
      Example.DrawUnsupported
  END;



(*
 * TIndexedBuffers
 ***************************************************************************)

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
{$IFDEF DCC}
	{ Pointer access. }
	  (vtx + Ndx)^.x := x;
	  (vtx + Ndx)^.y := y;
	  (vtx + Ndx)^.z := 0;
	  (vtx + Ndx)^.color := Color
{$ELSE}
	{ Free Pascal can manage it as an ARRAY. }
	  vtx[Ndx].x := x;
	  vtx[Ndx].y := y;
	  vtx[Ndx].z := 0;
	  vtx[Ndx].color := Color
{$ENDIF}
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
{$IFDEF DCC}
	{ Pointer access. }
	(Indices + Ndx)^ := (t + Ndx) MOD 13;
{$ELSE}
	{ Free Pascal can manage it as an ARRAY. }
	Indices[Ndx] := (t + Ndx) MOD 13;
{$ENDIF}
      al_unlock_index_buffer (ibuff);
    END;
    INHERITED Update
  END;



(* Draws screen. *)
  PROCEDURE TIndexedBuffers.Draw;
  BEGIN
    IF (NOT Soft AND NOT SELF.Soft) AND (vbuff <> NIL) AND (ibuff <> NIL) THEN
    BEGIN
      al_draw_indexed_buffer (vbuff, NIL, ibuff, 0, 4, ALLEGRO_PRIM_LINE_LIST);
      al_draw_indexed_buffer (vbuff, NIL, ibuff, 4, 8, ALLEGRO_PRIM_LINE_STRIP)
    END
    ELSE
      Example.DrawUnsupported
  END;

BEGIN
  Example := TPrimitivesExample.Create;
  TRY
    Example.Initialize;
    Example.Run
  FINALLY
    FreeAndNil (Example)
  END
END.
