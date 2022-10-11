program ex_prim;
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
  Copyright (c) 2012-2020 Guillermo Martínez J.

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
{ Needed to make the executable suitable for modern Windows OS. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

{$IFDEF DCC}
{ Pointer arithmetic needed by this example.
  Not all Delphi versions support it though. }
  {$INCLUDE allegro5.cfg }
  {$IFDEF ISDELPHI2009ANDUP}
    {$POINTERMATH ON}
  {$ELSE}
    {$MESSAGE error 'Need pointer arithmetics.'}
  {$ENDIF}
{$ENDIF}

  uses
    Common,
    Allegro5,
    al5base, al5font, al5image, al5nativedlg, al5primitives, al5strings,
    sysutils;

  const
    ScreenW = 800; ScreenH = 600;
    RefreshRate = 60;
    FixedTimestep = 1 / RefreshRate;
    NUM_SCREENS = 12;
    ROTATE_SPEED = 0.0001;

  type
  (* Base class for example screens. *)
    TCustomScreen = class (TObject)
    private
      fName: AL_STR;
    public
    (* Constructor. *)
      constructor Create (const aName: AL_STR);
    (* Updates the example.

      By default builds MainTrans rotating by Theta angle. *)
      procedure Update; virtual;
    (* Draws the screen. *)
      procedure Draw; virtual; abstract;

    (* Screen name. *)
      property Name: AL_STR read fName;
    end;



  (* The example object.

     This is something similar to VCL/LCL TApplication class. *)
    TPrimitivesExample = class (TObject)
    private
      fTerminated,
      fUseShader, fSoft, fBlend, fDrawBkg, fClip: Boolean;
      fScreens: array [1..NUM_SCREENS] of TCustomScreen;
      fCurScreen, fFramesDone: Integer;
      fDisplay: ALLEGRO_DISPLAYptr;
      fFont: ALLEGRO_FONTptr;
      fBkg, fTexture, fBuffer: ALLEGRO_BITMAPptr;
      fQueue: ALLEGRO_EVENT_QUEUEptr;
      fIdentity, fTransformationMatrix: ALLEGRO_TRANSFORM;
      fSpeed, fThickness, fTheta: Single;
      fRealTime, fGameTime, fStartTime, fTimeDiff, fFrameDuration: Double;

    (* Updates the example.  Increases Theta angle by Speed. *)
      procedure Update;
    (* Renders the example.

      It applyes blender, transformation matrix, calls screen's draw method and
      appyes identity matrix. *)
      procedure Draw;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initializes the application. *)
      procedure Initialize;
    (* Runs the example. *)
      procedure Run;
    (* Terminates the example. *)
      procedure Terminate;
    (* Shows an error message box. *)
      procedure ShowErrorMessage (const aMessage: AL_STR);
    (* Builds the transformation matrix. *)
      procedure BuildTransform (ax, ay, sx, sy, aTheta: AL_FLOAT);
    (* Draws text telling the example isn't supported. *)
      procedure DrawUnsupported;

    (* Texture used by primitives. *)
      property Texture: ALLEGRO_BITMAPptr read fTexture;
    (* Theta angle. *)
      property Theta: Single read fTheta;
    (* Trace thickness. *)
      property Thickness: Single read fThickness;
    (* Tells if it's using software rendering. *)
      property Soft: Boolean read fSoft;
    end;



  (* Low-level primitives. *)
    TLowPrimitives = class (TCustomScreen)
    private
      Vtx, Vtx2: array [0..12] of ALLEGRO_VERTEX;
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Indexed primitives. *)
    TIndexedPrimitives = class (TCustomScreen)
    private
      Indices1, Indices2, Indices3: array [0..3] of Integer;
      Vtx, Vtx2: array [0..12] of ALLEGRO_VERTEX;
    public
    (* Constructor. *)
      constructor Create;
    (* Executes the example. *)
      procedure Update; override;
    (* Draws the Screen. *)
      procedure Draw; override;
    end;



  (* High-level primitives. *)
    THighPrimitives = class (TCustomScreen)
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Transformations. *)
    TTransformationsPrimitives = class (TCustomScreen)
    public
    (* Constructor. *)
      constructor Create;
    (* Updates the example. *)
      procedure Update; override;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Filled primitives. *)
    TFilledPrimitives = class (TCustomScreen)
    private
      vtx: array [0..20] of ALLEGRO_VERTEX;
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Indexed filled primitives. *)
    TIndexedFilledPrimitives = class (TCustomScreen)
    private
      vtx: array [0..20] of ALLEGRO_VERTEX;
      Indices1, Indices2, Indices3: array [0..5] of Integer;
    public
    (* Constructor. *)
      constructor Create;
    (* Updates the example. *)
      procedure Update; override;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* High-level filled primitives. *)
    THighFilledPrimitives = class (TCustomScreen)
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Textured primitives. *)
    TTexturePrimitives = class (TCustomScreen)
    private
      Vtx, Vtx2: array [0..12] of ALLEGRO_VERTEX;
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Filled & testured primitives. *)
    TFilledTexturePrimitives = class (TCustomScreen)
    private
      vtx: array [0..20] of ALLEGRO_VERTEX;
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Stores a custom vertex. *)
    CUSTOM_VERTEX = record
    { Use types defined at al5base, so we know their size. }
      u, v: AL_INT16;
      x, y: AL_INT16;
      Color: ALLEGRO_COLOR;
      junk: array [0..5] of AL_INT;
    end;



  (* Primitives using the premious defined custom vertex struct. *)
    TCustomVertexFormatPrimitives = class (TCustomScreen)
    private
      vtx: array [0..3] of CUSTOM_VERTEX;
      Decl: ALLEGRO_VERTEX_DECLptr;
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Vertex buffers. *)
    TVertexBuffers = class (TCustomScreen)
    private
      vtx, vtx2: array [0..12] of ALLEGRO_VERTEX;
      vbuff, vbuff2: ALLEGRO_VERTEX_BUFFERptr;
      NoSoft, NoSoft2: Boolean;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Indexed vertex buffers. *)
    TIndexedBuffers = class (TCustomScreen)
    private
      vbuff: ALLEGRO_VERTEX_BUFFERptr;
      ibuff: ALLEGRO_INDEX_BUFFERptr;
      Soft: Boolean;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Updates screen. *)
      procedure Update; override;
    (* Draws the screen. *)
      procedure Draw; override;
    end;

  var
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
  constructor TCustomScreen.Create (const aName: AL_STR);
  begin
    inherited Create;
    fName := aName
  end;



(* Executes screen. *)
  procedure TCustomScreen.Update;
  begin
    Example.BuildTransform (ScreenW / 2, ScreenH / 2, 1, 1, Example.Theta)
  end;



(*
 * TPrimitivesExample
 ***************************************************************************)

(* Updates the example. *)
  procedure TPrimitivesExample.Update;
  var
    Event: ALLEGRO_EVENT;
  begin
  { Events. }
    while not fTerminated and al_get_next_event (fQueue, Event) do
    begin
      case Event.ftype of
      ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
        begin
          Inc (fCurScreen);
          if fCurScreen > High (fScreens) then fCurScreen := Low (fScreens)
        end;
      ALLEGRO_EVENT_DISPLAY_CLOSE:
          Self.Terminate;
      ALLEGRO_EVENT_KEY_CHAR:
        case Event.keyboard.keycode of
        ALLEGRO_KEY_ESCAPE:
          Self.Terminate;
        ALLEGRO_KEY_S:
          begin
            fSoft := not fSoft;
            fTimeDiff := al_get_time;
            fFramesDone := 0
          end;
        ALLEGRO_KEY_C:
          begin
            fClip := not fClip;
            fTimeDiff := al_get_time;
            fFramesDone := 0
          end;
        ALLEGRO_KEY_L:
          begin
            fBlend := not fBlend;
            fTimeDiff := al_get_time;
            fFramesDone := 0
          end;
        ALLEGRO_KEY_B:
          begin
            fDrawBkg := not fDrawBkg;
            fTimeDiff := al_get_time;
            fFramesDone := 0
          end;
        ALLEGRO_KEY_LEFT:
          fSpeed := fSpeed - ROTATE_SPEED;
        ALLEGRO_KEY_RIGHT:
          fSpeed := fSpeed + ROTATE_SPEED;
        ALLEGRO_KEY_SPACE:
          fSpeed := 0;
        ALLEGRO_KEY_PGUP:
          begin
            fThickness := fThickness + 0.5;
            if fThickness < 1.0 then fThickness := 1.0
          end;
        ALLEGRO_KEY_PGDN:
          begin
            fThickness := fThickness - 0.5;
            if fThickness < 1.0 then fThickness := 1.0
          end;
        ALLEGRO_KEY_UP:
          begin
            Inc (fCurScreen);
            if fCurScreen > High (fScreens) then fCurScreen := Low (fScreens)
          end;
        ALLEGRO_KEY_DOWN:
          begin
            Dec (fCurScreen);
            if fCurScreen < Low (fScreens) then fCurScreen := High (fScreens)
          end;
        end
      end
    end;
  { Updates screens. }
    fTheta := fTheta + fSpeed;
    if fScreens[fCurScreen] <> Nil then
      fScreens[fCurScreen].Update
    else
      al_draw_textf (
        fFont, White, 0, 0, 0,
        'Example %d not supported', [fCurScreen]
      )
  end;



(* Renders the example. *)
  procedure TPrimitivesExample.Draw;

  { Helper to print boolean statuses. }
    procedure ShowBoolean
      (const aY: Integer; const aLabel: AL_STR; const aState: Boolean);
    begin
      if aState then
        al_draw_text (fFont, SolidWhite, 0, aY, 0, aLabel + ': True')
      else
        al_draw_text (fFont, SolidWhite, 0, aY, 0, aLabel + ': False')
    end;

  begin
  { Prepare rendering. }
    al_clear_to_color (SolidBlack);
    if fSoft then
    begin
      al_set_target_bitmap (fBuffer);
      al_clear_to_color (SolidBlack)
    end;
  { Background. }
    if fDrawBkg and (fBkg <> Nil) then
    begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      al_draw_scaled_bitmap (
        fBkg,
        0, 0, al_get_bitmap_width (fbkg), al_get_bitmap_height (fbkg),
        0, 0, ScreenW, ScreenH,
        0
      )
    end;
    if fClip then
      al_set_clipping_rectangle
        (ScreenW div 2, ScreenH div 2, ScreenW div 2, ScreenH div 2);
  { Set blender. }
    if fBlend then
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE)
    else
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
  { Draw example. }
    al_use_transform (fTransformationMatrix);
    if fScreens[fCurScreen] <> Nil then fScreens[fCurScreen].Draw;
  { Restore. }
    al_use_transform (fIdentity);
    al_set_clipping_rectangle (0, 0, ScreenW, ScreenH);
    if fSoft then
    begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      al_set_target_backbuffer (fDisplay);
      al_draw_bitmap (fBuffer, 0, 0, 0)
    end;
  { Show states. }
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    if fScreens[fCurScreen] <> Nil then
      al_draw_textf (
        fFont, SolidWhite, ScreenW div 2, ScreenH - 20, ALLEGRO_ALIGN_CENTRE,
        '%d: %s', [fCurScreen, fScreens[fCurScreen].Name]
      )
    else
      al_draw_textf (
        fFont, SolidWhite, ScreenW div 2, ScreenH - 20, ALLEGRO_ALIGN_CENTRE,
        'Example %d doesn''t work.', [fCurScreen]
      );
    al_draw_textf (
      fFont, SolidWhite, 0, 0, 0,
      'FPS: %.2f', [fFramesDone / (al_get_time - fTimeDiff)]
    );
    al_draw_text (
      fFont, SolidWhite, 0, 20, 0, 'Change Screen (Up/Down). Esc to Quit.');
    al_draw_textf (
      fFont, SolidWhite, 0, 40, 0,
      'Rotation (Left/Right/Space): %.4f', [fSpeed]
    );
    al_draw_textf (
      fFont, SolidWhite, 0, 60, 0,
      'Thickness (PgUp/PgDown): %.2f', [fThickness]
    );
    ShowBoolean (80, 'Software (S)', fSoft);
    ShowBoolean (100, 'Blending (L)', fBlend);
    ShowBoolean (120, 'Background (B)', fDrawBkg);
    ShowBoolean (140, 'Clip (C)', fClip);

    al_flip_display;
  end;



(* Constructor. *)
  constructor TPrimitivesExample.Create;
  begin
    inherited Create;
    fDisplay := Nil;
    fBkg := Nil;
    fTexture := Nil;
    fBuffer := Nil;

    fUseShader := False;
    fBlend := True;
    fTheta := 0;
    fSpeed := ROTATE_SPEED;
    fThickness := 1;
    fSoft := True;
    fDrawBkg := True;
    fClip := False
  end;



(* Destructor. *)
  destructor TPrimitivesExample.Destroy;
  var
    Cnt: Integer;
  begin
    if fBuffer <> Nil then al_destroy_bitmap (fBuffer);
    for Cnt := Low (fScreens) to High (fScreens) do fScreens[Cnt].Free;
    if fQueue <> Nil then al_destroy_event_queue (fQueue);
    if fTexture <> Nil then al_destroy_bitmap (fTexture);
    if fBkg <> Nil then al_destroy_bitmap (fBkg);
    if fFont <> Nil then al_destroy_font (fFont);
    if fDisplay <> Nil then al_destroy_display (fDisplay);
    inherited Destroy
  end;



(* Initializes the application. *)
  procedure TPrimitivesExample.Initialize;
  var
    Old: Integer;
  begin
  { Check parameters. }
    if ParamCount > 0 then
    begin
      if ParamStr (1) = '--shader' then
        fUseShader := True
      else begin
        ShowErrorMessage (
          al_str_format ('Invalid command line option: %s.', [ParamStr (1)])
        );
        Exit
      end
    end;
  { Initializes Allegro 5 and addons. }
    if not al_init then
    begin
      ShowErrorMessage ('Could not init Allegro.');
      Exit
    end;
    al_init_image_addon;
    al_init_font_addon;
    al_init_primitives_addon;
    InitPlatformSpecific;
    if not al_install_keyboard then
    begin
      ShowErrorMessage ('Error installing keyboard.');
      Exit
    end;
    if not al_install_mouse then
    begin
      ShowErrorMessage ('Error installing mouse.');
      Exit
    end;
  { Display. }
    if fUseShader then
      al_set_new_display_flags (ALLEGRO_PROGRAMMABLE_PIPELINE);
    fDisplay := al_create_display (ScreenW, ScreenH);
    if fDisplay = Nil then
    begin
      ShowErrorMessage ('Error creating display.');
      Exit
    end;
    al_set_window_title (fDisplay, 'Primitives Example');
  { Make and set some color to draw with. }
    SolidBlack := al_map_rgba_f (0, 0, 0, 1);
    SolidWhite := al_map_rgba_f (1, 1, 1, 1);
    White := al_map_rgb_f (1, 1, 1);
  { Load a font. }
    fFont := al_load_font ('data/fixed_font.tga', 0, 0);
    if fFont = Nil then
    begin
      ShowErrorMessage ('Error loading "data/fixed_font.tga".');
      Exit
    end;
  { Textures. }
    fBkg := al_load_bitmap ('data/bkg.png');
    if fBkg = Nil then
    begin
      ShowErrorMessage ('Error loading "data/bkg.png".');
      Exit
    end;
    fTexture := al_load_bitmap ('data/texture.tga');
    if fTexture = Nil then
    begin
      ShowErrorMessage ('Error loading "data/texture.tga".');
      Exit
    end;
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
{$IF DEFINED(WINDOWS)}
    fScreens[12] := TIndexedBuffers.Create;
{$ELSE}
  { For some reason this seems not to work on Linux (or doesn't in my system).

    My system is Xubuntu 18.04.3 + nVidia GeForce GT610 + nouveau 2.4.97.

    If your system is different you may try.  If it works (or find why mine
    doesn't), let me know. }
    fScreens[12] := Nil;
{$ENDIF}
  { Backbuffer. }
    Old := al_get_new_bitmap_flags;
    al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
    fBuffer := al_create_bitmap (ScreenW, ScreenH);
    if fBuffer = Nil then
    begin
      ShowErrorMessage ('Could not create buffer.');
      Exit
    end;
    al_set_new_bitmap_flags (Old);

    fTerminated := False
  end;



(* Runs the example. *)
  procedure TPrimitivesExample.Run;
  begin
    if fTerminated then Exit; { Skip execution. }

    fRealTime := al_get_time;
    fFramesDone := 0;
    fTimeDiff := al_get_time;
    fGameTime := al_get_time;
  { Main loop. }
    fCurScreen := Low (fScreens);
    repeat
      fFrameDuration := al_get_time - fRealTime;
      al_rest (FixedTimestep - fFrameDuration); { rest at least fixed_dt }
      fFrameDuration := al_get_time - fRealTime;
      fRealTime := al_get_time;
      fGameTime := fRealTime;
      if fRealTime - fGameTime > fFrameDuration then
      { eliminate excess overflow }
        fGameTime := fGameTime + FixedTimestep * Trunc ((fRealTime - fGameTime) / FixedTimestep);
      while not fTerminated and (fRealTime - fGameTime >= 0) do
      begin
        fStartTime := al_get_time;
        fGameTime := fGameTime + FixedTimestep;
        Self.Update;
        if al_get_time - fStartTime >= FixedTimestep then
        { break if we start taking too long }
          Break
      end;
      Self.Draw;
      Inc (fFramesDone)
    until fTerminated
  end;



(* Terminates the example. *)
  procedure TPrimitivesExample.Terminate;
  begin
    fTerminated := True
  end;



(* Shows an error message box. *)
  procedure TPrimitivesExample.ShowErrorMessage (const aMessage: AL_STR);
  begin
    al_show_native_message_box
      (fDisplay, 'Error', 'Cannot run example', aMessage, '', 0);
    Self.Terminate
  end;



(* Builds the transformation matrix. *)
  procedure TPrimitivesExample.BuildTransform (ax, ay, sx, sy, aTheta: AL_FLOAT);
  begin
    al_build_transform (fTransformationMatrix, ax, ay, sx, sy, aTheta)
  end;



(* Draws text telling the example isn't supported. *)
  procedure TPrimitivesExample.DrawUnsupported;
  begin
    al_draw_textf (
      fFont, White, 0, 0, 0,
      '%s not supported', [fScreens[fCurScreen].Name]
    )
  end;



(*
 * TLowPrimitives
 ***************************************************************************)


(* Constructor. *)
  constructor TLowPrimitives.Create;
  var
    Ndx, x, y: Integer;
    Color: ALLEGRO_COLOR;
  begin
    inherited Create ('Low Level Primitives');

    for Ndx := Low (Vtx) to High (Vtx) do
    begin
      x := Trunc (200 * Cos (Ndx / 13 * 2 * ALLEGRO_PI));
      y := Trunc (200 * Sin (Ndx / 13 * 2 * ALLEGRO_PI));

      Color := al_map_rgb (
        (Ndx + 1) mod 3 * 64,
        (Ndx + 2) mod 3 * 64,
        (Ndx    ) mod 3 * 64);

      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      Vtx[Ndx].color := color;
      Vtx2[Ndx].color := color
    end
  end;



(* Draws screen. *)
  procedure TLowPrimitives.Draw;
  begin
    al_draw_prim (Vtx, Nil, 0, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_prim (Vtx, Nil, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_prim (Vtx, Nil, 9, 13, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_prim (Vtx2, Nil, 0, 13, ALLEGRO_PRIM_POINT_LIST);
  end;



(*
 * TIndexedPrimitives
 ***************************************************************************)

(* Constructor. *)
  constructor TIndexedPrimitives.Create;
  var
    Ndx, x, y: Integer;
    Color: ALLEGRO_COLOR;
  begin
    inherited Create ('Indexed Primitives');

    for Ndx := 0 to 3 do
    begin
      Indices1[Ndx] := Ndx;
      Indices2[Ndx] := Ndx + 5;
      Indices3[Ndx] := Ndx + 9;
    end;
    Indices1[2] := 3; Indices1[3] := 4;

    for Ndx := Low (vtx) to High (vtx) do
    begin
      x := Trunc (200 * cos (Ndx / 13 * 2 * ALLEGRO_PI));
      y := Trunc (200 * sin (Ndx / 13 * 2 * ALLEGRO_PI));

      Color := al_map_rgb (
        (Ndx + 1) mod 3 * 64,
        (Ndx + 2) mod 3 * 64,
        (Ndx    ) mod 3 * 64
      );

      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      Vtx[Ndx].color := color;
      Vtx2[Ndx].color := color
    end
  end;



(* Executes screen. *)
  procedure TIndexedPrimitives.Update;
  var
    Ndx: Integer;
  begin
    inherited Update; { Updates Theta and builds MainTrans. }
    for Ndx := Low (Indices1) to High (Indices1) do
    begin
      Indices1[Ndx] := Trunc (al_get_time() + Ndx) mod 13;
      Indices2[Ndx] := Trunc (al_get_time() + Ndx + 4) mod 13;
      Indices3[Ndx] := Trunc (al_get_time() + Ndx + 8) mod 13
    end
  end;



(* Draws screen. *)
  procedure TIndexedPrimitives.Draw;
  begin
    al_draw_indexed_prim (vtx, Nil, indices1, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_indexed_prim (vtx, Nil, indices2, 4, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_indexed_prim (vtx, Nil, indices3, 4, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_indexed_prim (vtx2, Nil, indices3, 4, ALLEGRO_PRIM_POINT_LIST);
  end;



(*
 * THighPrimitives
 ***************************************************************************)

(* Constructor. *)
  constructor THighPrimitives.Create;
  begin
    inherited Create ('High Level Primitives')
  end;



(* Draws screen. *)
  procedure THighPrimitives.Draw;
  begin
    al_draw_line (-300, -200, 300, 200, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_triangle (-150, -250, 0, 250, 150, -250, al_map_rgba_f (0.5, 0, 0.5, 1), Example.Thickness);
    al_draw_rectangle (-300, -200, 300, 200, al_map_rgba_f (0.5, 0, 0, 1), Example.Thickness);
    al_draw_rounded_rectangle (-200, -125, 200, 125, 50, 100, al_map_rgba_f (0.2, 0.2, 0, 1), Example.Thickness);

    al_draw_ellipse (0, 0, 300, 150, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_elliptical_arc (-20, 0, 300, 200, -ALLEGRO_PI / 2, -ALLEGRO_PI, al_map_rgba_f (0.25, 0.25, 0.5, 1), Example.Thickness);
    al_draw_arc (0, 0, 200, -ALLEGRO_PI / 2, ALLEGRO_PI, al_map_rgba_f (0.5, 0.25, 0, 1), Example.Thickness);
    al_draw_spline (SplinePoints, al_map_rgba_f (0.1, 0.2, 0.5, 1), Example.Thickness);
    al_draw_pieslice (0, 25, 150, ALLEGRO_PI * 3 / 4, -ALLEGRO_PI / 2, al_map_rgba_f (0.4, 0.3, 0.1, 1), Example.Thickness)
  end;



(*
 * TTransformationsPrimitives
 ***************************************************************************)

(* Constructor. *)
  constructor TTransformationsPrimitives.Create;
  begin
    inherited Create ('Transformations')
  end;



(* Updates screen. *)
  procedure TTransformationsPrimitives.Update;
  var
    t: Double;
  begin
    t := al_get_time / 5;
    Example.BuildTransform (
      ScreenW / 2, ScreenH / 2, sin (t), cos (t), Example.Theta
    )
  end;



(* Draws screen. *)
  procedure TTransformationsPrimitives.Draw;
  begin
    al_draw_line (-300, -200, 300, 200, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_triangle (-150, -250, 0, 250, 150, -250, al_map_rgba_f (0.5, 0, 0.5, 1), Example.Thickness);
    al_draw_rectangle (-300, -200, 300, 200, al_map_rgba_f (0.5, 0, 0, 1), Example.Thickness);
    al_draw_rounded_rectangle (-200, -125, 200, 125, 50, 100, al_map_rgba_f (0.2, 0.2, 0, 1), Example.Thickness);

    al_draw_ellipse (0, 0, 300, 150, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_elliptical_arc (-20, 0, 300, 200, -ALLEGRO_PI / 2, -ALLEGRO_PI, al_map_rgba_f (0.25, 0.25, 0.5, 1), Example.Thickness);
    al_draw_arc (0, 0, 200, -ALLEGRO_PI / 2, ALLEGRO_PI, al_map_rgba_f (0.5, 0.25, 0, 1), Example.Thickness);
    al_draw_spline (SplinePoints, al_map_rgba_f (0.1, 0.2, 0.5, 1), Example.Thickness);
    al_draw_pieslice (0, 25, 150, ALLEGRO_PI * 3 / 4, -ALLEGRO_PI / 2, al_map_rgba_f (0.4, 0.3, 0.1, 1), Example.Thickness)
  end;



(*
 * TFilledPrimitives
 ***************************************************************************)

(* Constructor. *)
  constructor TFilledPrimitives.Create;
  var
    Ndx: Integer;
    x, y: Single;
    Color: ALLEGRO_COLOR;
  begin
    inherited Create ('Low Level Filled Primitives');
    for Ndx := Low (vtx) to High (vtx) do
    begin
      if (Ndx mod 2) = 0 then
      begin
        x := 150 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
        y := 150 * sin (Ndx / 20 * 2 * ALLEGRO_PI)
      end
      else begin
        x := 200 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
        y := 200 * sin (Ndx / 20 * 2 * ALLEGRO_PI);
      end;

      if Ndx = 0 then
      begin
        x := 0; Y := 0
      end;

      Color := al_map_rgb (
        (7 * Ndx + 1) mod 3 * 64,
        (2 * Ndx + 2) mod 3 * 64,
        (    Ndx    ) mod 3 * 64);

      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].color := color
    end
  end;



(* Draws screen. *)
  procedure TFilledPrimitives.Draw;
  begin
    al_draw_prim (vtx, Nil, 0, 6, ALLEGRO_PRIM_TRIANGLE_FAN);
    al_draw_prim (vtx, Nil, 7, 13, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_prim (vtx, Nil, 14, 20, ALLEGRO_PRIM_TRIANGLE_STRIP)
  end;



(*
 * TIndexedFilledPrimitives
 ***************************************************************************)

(* Constructor. *)
  constructor TIndexedFilledPrimitives.Create;
  var
    Ndx: Integer;
    x, y: Single;
    Color: ALLEGRO_COLOR;
  begin
    inherited Create ('Indexed Filled Primitives');
    for Ndx := Low (Indices1) to High (Indices1) do
    begin
      Indices1[Ndx] := Ndx + 12;
      if Ndx > 2 then INC(Indices1[Ndx]);
      Indices2[Ndx] := Ndx + 6;
      Indices1[Ndx] := Ndx;
    end;
    for Ndx := Low (vtx) to High (vtx) do
    begin
      if (Ndx mod 2) = 0 then
      begin
        x := 150 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
        y := 150 * sin (Ndx / 20 * 2 * ALLEGRO_PI)
      end
      else begin
        x := 200 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
        y := 200 * sin (Ndx / 20 * 2 * ALLEGRO_PI);
      end;

      if Ndx = 0 then
      begin
        x := 0; Y := 0
      end;

      Color := al_map_rgb (
        (7 * Ndx + 1) mod 3 * 64,
        (2 * Ndx + 2) mod 3 * 64,
        (    Ndx    ) mod 3 * 64);

      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].color := color
    end
  end;



(* Updates screen. *)
  procedure TIndexedFilledPrimitives.Update;
  var
    Ndx: Integer;
  begin
    for Ndx := Low (Indices1) to High (Indices1) do
    begin
      Indices1[Ndx] := (Trunc (al_get_time) + Ndx) mod 20 + 1;
      Indices2[Ndx] := (Trunc (al_get_time) + Ndx + 6) mod 20 + 1;
      if Ndx > 0 then
        Indices3[Ndx] := (Trunc (al_get_time) + Ndx + 12) mod 20 + 1
    end;
    Example.BuildTransform (ScreenW / 2, ScreenH / 2, 1, 1, Example.Theta)
  end;



(* Draws screen. *)
  procedure TIndexedFilledPrimitives.Draw;
  begin
    al_draw_indexed_prim (vtx, Nil, Indices1, 6, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_indexed_prim (vtx, Nil, Indices2, 6, ALLEGRO_PRIM_TRIANGLE_STRIP);
    al_draw_indexed_prim (vtx, Nil, Indices3, 6, ALLEGRO_PRIM_TRIANGLE_FAN)
  end;



(*
 * THighFilledPrimitives
 ***************************************************************************)

(* Constructor. *)
  constructor THighFilledPrimitives.Create;
  begin
    inherited Create ('High Level Filled Primitives');
  end;



(* Draws screen. *)
  procedure THighFilledPrimitives.Draw;
  begin
    al_draw_filled_triangle (-100, -100, -150, 200, 100, 200, al_map_rgb_f (0.5, 0.7, 0.3));
    al_draw_filled_rectangle (20, -50, 200, 50, al_map_rgb_f (0.3, 0.2, 0.6));
    al_draw_filled_ellipse (-250, 0, 100, 150, al_map_rgb_f (0.3, 0.3, 0.3));
    al_draw_filled_rounded_rectangle (50, -250, 350, -75, 50, 70, al_map_rgb_f (0.4, 0.2, 0));
    al_draw_filled_pieslice (200, 125, 50, ALLEGRO_PI / 4, 3 * ALLEGRO_PI / 2, al_map_rgb_f (0.3, 0.3, 0.1))
  end;



(*
 * TTexturePrimitives
 ***************************************************************************)

(* Constructor. *)
  constructor TTexturePrimitives.Create;
  var
    Ndx, x, y: Integer;
    Color: ALLEGRO_COLOR;
  begin
    inherited Create ('Textured Primitives');

    for Ndx := Low (Vtx) to High (Vtx) do
    begin
      x := Trunc (200 * Cos (Ndx / 13 * 2 * ALLEGRO_PI));
      y := Trunc (200 * Sin (Ndx / 13 * 2 * ALLEGRO_PI));
         
      Color := al_map_rgb (
        (Ndx + 1) mod 3 * 64,
        (Ndx + 2) mod 3 * 64,
        (Ndx    ) mod 3 * 64);

      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      vtx[Ndx].u := 64 * x / 100; vtx[Ndx].v := 64 * y / 100;
      vtx2[Ndx].u := 64 * x / 100; vtx2[Ndx].v := 64 * y / 100;
      if Ndx < 10 then
         vtx[Ndx].color := al_map_rgba_f (1, 1, 1, 1)
      else
         vtx[Ndx].color := Color;
      Vtx2[Ndx].color := Color
    end
  end;



(* Draws screen. *)
  procedure TTexturePrimitives.Draw;
  begin
    al_draw_prim (Vtx, Example.Texture, 0, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_prim (Vtx, Example.Texture, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_prim (Vtx, Example.Texture, 9, 13, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_prim (Vtx2, Example.Texture, 0, 13, ALLEGRO_PRIM_POINT_LIST)
  end;



(*
 * TFilledTexturePrimitives
 ***************************************************************************)

(* Constructor. *)
  constructor TFilledTexturePrimitives.Create;
  var
    Ndx: Integer;
    x, y: Single;
    Color: ALLEGRO_COLOR;
  begin
    inherited Create ('Filled Textured Primitives');
    for Ndx := Low (vtx) to High (vtx) do
    begin
      if (Ndx mod 2) = 0 then
      begin
        x := 150 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
        y := 150 * sin (Ndx / 20 * 2 * ALLEGRO_PI)
      end
      else begin
        x := 200 * cos (Ndx / 20 * 2 * ALLEGRO_PI);
        y := 200 * sin (Ndx / 20 * 2 * ALLEGRO_PI);
      end;

      if Ndx = 0 then
      begin
        x := 0; Y := 0
      end;

      Color := al_map_rgb (
        (7 * Ndx + 1) mod 3 * 64,
        (2 * Ndx + 2) mod 3 * 64,
        (    Ndx    ) mod 3 * 64);

      vtx[Ndx].x := x; vtx[Ndx].y := y; vtx[Ndx].z := 0;
      vtx[Ndx].u := 64 * x / 100; vtx[Ndx].v := 64 * y / 100;
      if Ndx < 10 then
        vtx[Ndx].color := al_map_rgba_f(1, 1, 1, 1)
      else
        vtx[Ndx].color := Color
    end
  end;



(* Draws screen. *)
  procedure TFilledTexturePrimitives.Draw;
  begin
    al_draw_prim (vtx, Example.Texture, 0, 6, ALLEGRO_PRIM_TRIANGLE_FAN);
    al_draw_prim (vtx, Example.Texture, 7, 13, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_prim (vtx, Example.Texture, 14, 20, ALLEGRO_PRIM_TRIANGLE_STRIP)
  end;



(*
 * TCustomVertexFormatPrimitives
 ***************************************************************************)

var
{ Used to create the vertices by the constructor bellow. }
  Elems: array [0..3] of ALLEGRO_VERTEX_ELEMENT = (
    (attribute: ALLEGRO_PRIM_POSITION; storage: ALLEGRO_PRIM_SHORT_2; offset: 4),
    (attribute: ALLEGRO_PRIM_TEX_COORD_PIXEL; storage: ALLEGRO_PRIM_SHORT_2; offset: 0),
    (attribute: ALLEGRO_PRIM_COLOR_ATTR; storage: ALLEGRO_PRIM_STORAGE_NONE; offset: 8),
    (attribute: ALLEGRO_PRIM_ATTR_NONE; storage: ALLEGRO_PRIM_STORAGE_NONE; offset: 0)
  );



(* Constructor. *)
  constructor TCustomVertexFormatPrimitives.Create;
  var
    Ndx, x, y: Integer;
  begin
    inherited Create ('Custom Vertex Format');

    Decl := al_create_vertex_decl (Elems, sizeof (CUSTOM_VERTEX));

    for Ndx := Low (vtx) to High (vtx) do
    begin
      x := Trunc (200 * cos (Ndx / 4 * 2 * ALLEGRO_PI));
      y := Trunc (200 * sin (Ndx / 4 * 2 * ALLEGRO_PI));

      vtx[Ndx].x := x; vtx[Ndx].y := y;
      vtx[Ndx].u := Trunc (64 * x / 100); vtx[Ndx].v := Trunc (64 * y / 100);
      vtx[Ndx].color := al_map_rgba_f (1, 1, 1, 1)
    end
  end;



(* Draws screen. *)
  procedure TCustomVertexFormatPrimitives.Draw;
  begin
    al_draw_prim_ex (@vtx, Decl, Example.Texture, 0, 4, ALLEGRO_PRIM_TRIANGLE_FAN);
  end;



(*
 * TVertexBuffers
 ***************************************************************************)

(* Constructor. *)
  constructor TVertexBuffers.Create;
  var
    Ndx, x, y: Integer;
    Color: ALLEGRO_COLOR;
  begin
    inherited Create ('Vertex Buffers');

    for Ndx := Low (Vtx) to High (Vtx) do
    begin
      x := Trunc (200 * Cos (Ndx / 13 * 2 * ALLEGRO_PI));
      y := Trunc (200 * Sin (Ndx / 13 * 2 * ALLEGRO_PI));

      Color := al_map_rgb (
        (Ndx + 1) mod 3 * 64,
        (Ndx + 2) mod 3 * 64,
        (Ndx    ) mod 3 * 64);

      Vtx[Ndx].x := x; Vtx[Ndx].y := y; Vtx[Ndx].z := 0;
      Vtx2[Ndx].x := 0.1 * x; Vtx2[Ndx].y := 0.1 * y;
      Vtx[Ndx].color := color;
      Vtx2[Ndx].color := color
    end;

    vbuff := al_create_vertex_buffer (vtx, ALLEGRO_PRIM_BUFFER_readwrite);
    if vbuff = Nil then
    begin
      vbuff := al_create_vertex_buffer (vtx, ALLEGRO_PRIM_BUFFER_NONE);
      NoSoft := True
    end
    else
      NoSoft := False;

    vbuff2 := al_create_vertex_buffer (vtx2, ALLEGRO_PRIM_BUFFER_readwrite);
    if vbuff2 = Nil then
    begin
      vbuff2 := al_create_vertex_buffer (vtx2, ALLEGRO_PRIM_BUFFER_NONE);
      NoSoft2 := True
    end
    else
      NoSoft2 := False
  end;



(* Destructor. *)
  destructor TVertexBuffers.Destroy;
  begin
    al_destroy_vertex_buffer (vbuff);
    al_destroy_vertex_buffer (vbuff2);
    inherited Destroy
  end;



(* Draws screen. *)
  procedure TVertexBuffers.Draw;
  begin
    if (vbuff <> Nil) and not (Example.Soft and NoSoft) then
    begin
      al_draw_vertex_buffer (vbuff, Nil, 0, 4, ALLEGRO_PRIM_LINE_LIST);
      al_draw_vertex_buffer (vbuff, Nil, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
      al_draw_vertex_buffer (vbuff, Nil, 9, 13, ALLEGRO_PRIM_LINE_LOOP)
    end
    else
      Example.DrawUnsupported;

    if (vbuff2 <> Nil) and not (Example.Soft and NoSoft2) then
      al_draw_vertex_buffer (vbuff2, Nil, 0, 13, ALLEGRO_PRIM_POINT_LIST)
    else
      Example.DrawUnsupported
  end;



(*
 * TIndexedBuffers
 ***************************************************************************)

(* Constructor. *)
  constructor TIndexedBuffers.Create;
  var
    Ndx: Integer;
    Color: ALLEGRO_COLOR;
    vtx: ALLEGRO_VERTEXptr;
    Flags: ALLEGRO_PRIM_BUFFER_FLAGS;
    x, y: Single;
  begin
    inherited Create ('Indexed Buffers');
    vbuff := al_create_vertex_buffer_ex
      (Nil, Nil, 13, ALLEGRO_PRIM_BUFFER_readwrite);
    if vbuff = Nil then
    begin
      vbuff := al_create_vertex_buffer_ex
        (Nil, Nil, 13, ALLEGRO_PRIM_BUFFER_NONE);
      Self.Soft := False;
      Flags := ALLEGRO_PRIM_BUFFER_NONE
    end
    else begin
      Self.Soft := True;
      Flags := ALLEGRO_PRIM_BUFFER_readwrite
    end;

    ibuff := al_create_index_buffer (sizeof (AL_SHORT), Nil, 8, Flags);

    if vbuff <> Nil then
    begin
      vtx := al_lock_vertex_buffer (vbuff, 0, 13, ALLEGRO_LOCK_WRITEONLY);
      try
        for Ndx := 0 to 13 do
        begin
          x := 200 * cos (Ndx / 13 * 2 * ALLEGRO_PI);
          y := 200 * sin (Ndx / 13 * 2 * ALLEGRO_PI);

          Color := al_map_rgb (
            (Ndx + 1) mod 3 * 64,
            (Ndx + 2) mod 3 * 64,
            (Ndx    ) mod 3 * 64
          );
{$IFDEF DCC}
        { Pointer access. }
          (vtx + Ndx)^.x := x;
          (vtx + Ndx)^.y := y;
          (vtx + Ndx)^.z := 0;
          (vtx + Ndx)^.color := Color
{$ELSE}
        { Free Pascal can manage it as an array. }
          vtx[Ndx].x := x;
          vtx[Ndx].y := y;
          vtx[Ndx].z := 0;
          vtx[Ndx].color := Color
{$ENDIF}
        end
      finally
        al_unlock_vertex_buffer (vbuff)
      end
    end
  end;



(* Destructor. *)
  destructor TIndexedBuffers.Destroy;
  begin
    al_destroy_vertex_buffer (vbuff);
    al_destroy_index_buffer (ibuff);
    inherited Destroy
  end;



(* Updates screen. *)
  procedure TIndexedBuffers.Update;
  var
    Ndx, t: Integer;
    Indices: ^AL_SHORT;
  begin
    if ibuff <> Nil then
    begin
      t := Trunc (al_get_time);
      Indices := al_lock_index_buffer (ibuff, 0, 8, ALLEGRO_LOCK_WRITEONLY);

      for Ndx := 0 to 7 do
{$IFDEF DCC}
        { Pointer access. }
        (Indices + Ndx)^ := (t + Ndx) mod 13;
{$ELSE}
        { Free Pascal can manage it as an array. }
        Indices[Ndx] := (t + Ndx) mod 13;
{$ENDIF}
      al_unlock_index_buffer (ibuff)
    end;
    inherited Update
  end;



(* Draws screen. *)
  procedure TIndexedBuffers.Draw;
  begin
    if (not Soft and not Self.Soft) and (vbuff <> Nil) and (ibuff <> Nil) then
    begin
      al_draw_indexed_buffer (vbuff, Nil, ibuff, 0, 4, ALLEGRO_PRIM_LINE_LIST);
      al_draw_indexed_buffer (vbuff, Nil, ibuff, 4, 8, ALLEGRO_PRIM_LINE_STRIP)
    end
    else
      Example.DrawUnsupported
  end;

begin
  Example := TPrimitivesExample.Create;
  try
    Example.Initialize;
    Example.Run
  finally
    FreeAndNil (Example)
  end
end.
