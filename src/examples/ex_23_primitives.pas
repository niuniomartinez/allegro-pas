program ex_23_primitives;
(* A sampler of the primitive addon.
 *
 * All primitives are rendered using the additive blender, so overdraw will
 * manifest itself as overly bright pixels.
 *
 * From an example by Pavel Sountsov.
 * Ported to Object Pascal by Guillermo Martínez J.
 *)
(*
  Copyright (c) 2012-2024 Guillermo Martínez J.

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
{ Windows manifest. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

{$IFDEF DCC}
{ Pointer arithmetic needed by this example.
  Not all Delphi versions support it though.
}
  {$INCLUDE allegro5.cfg }
  {$IFDEF ISDELPHI2009ANDUP}
    {$POINTERMATH ON}
  {$ELSE}
    {$MESSAGE error 'Need pointer arithmetics.'}
  {$ENDIF}
{$ENDIF}

  uses
    Common,
    Allegro5, al5base, al5font, al5image, al5primitives, al5strings,
    sysutils;

  const
    ScreenW = 800; ScreenH = 600;
    RefreshRate = 60;
    FixedTimestep = 1 / RefreshRate;
    NumScreens = 12;
    RotationSpeed = 0.0005;

  type
  (* Array of vertices.  Needed because Delphi. *)
    TVertexList_12 = array [0..12] of ALLEGRO_VERTEX;
    TVertexList_20 = array [0..20] of ALLEGRO_VERTEX;
  (* Base class for example screens. *)
    TCustomScreen = class (TObject)
    private
      fName: AnsiString;
    public
    (* Constructor. *)
      constructor Create (const aName: AnsiString);
    (* Update the example.

      By default it builds MainTrans rotating by Theta angle.
     *)
      procedure Update; virtual;
    (* Draw the screen. *)
      procedure Draw; virtual; abstract;

    (* Screen name. *)
      property Name: AnsiString read fName;
    end;



  (* The example object.

     This is something similar to VCL/LCL TApplication class.
   *)
    TPrimitivesExample = class (TObject)
    private
      fTerminated,
      fSoft, fBlend, fDrawBkg, fClip: Boolean;
      fScreens: array [1..NumScreens] of TCustomScreen;
      fCurScreen, fFramesDone: Integer;
      fDisplay: ALLEGRO_DISPLAYptr;
      fFont: ALLEGRO_FONTptr;
      fBkg, fTexture, fBuffer: ALLEGRO_BITMAPptr;
      fQueue: ALLEGRO_EVENT_QUEUEptr;
      fIdentity, fTransformationMatrix: ALLEGRO_TRANSFORM;
      fSpeed, fThickness, fTheta: Single;
      fRealTime, fGameTime, fStartTime, fTimeDiff, fFrameDuration: Double;

    (* Helper to draw texts. *)
      procedure DrawText (
        const aText: String; aParams: array of const;
        const aX, aY: Integer;
        const aCentered: Boolean = False
      );
    (* Update the example.  Increases Theta angle by Speed. *)
      procedure Update;
    (* Render the example.

       It applies blender, transformation matrix, calls screen's draw method and
       applies identity matrix.
     *)
      procedure Draw;
    public
    (* Constructor. *)
      constructor Create;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initialize the application. *)
      procedure Initialize;
    (* Run the example. *)
      procedure Run;
    (* Terminate the example. *)
      procedure Terminate;
    (* Build the transformation matrix. *)
      procedure BuildTransform (ax, ay, sx, sy, aTheta: AL_FLOAT);
    (* Draw text telling the example isn't supported. *)
      procedure DrawUnsupported;

    (* Texture used by primitives. *)
      property Texture: ALLEGRO_BITMAPptr read fTexture;
    (* Theta angle. *)
      property Theta: Single read fTheta;
    (* Trace thickness. *)
      property Thickness: Single read fThickness;
    (* Tells if it's using software rendering. *)
      property Soft: Boolean read fSoft;
    public
    (* Data shared by some examples. *)
      Vtx, Vtx2: TVertexList_12;
      Vtx3: TVertexList_20;
    end;



  (* Low-level primitives. *)
    TLowPrimitives = class (TCustomScreen)
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
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Indexed filled primitives. *)
    TIndexedFilledPrimitives = class (TCustomScreen)
    private
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
      vtx: TVertexList_12;
    public
    (* Constructor. *)
      constructor Create;
    (* Draws the screen. *)
      procedure Draw; override;
    end;



  (* Filled & testured primitives. *)
    TFilledTexturePrimitives = class (TCustomScreen)
    private
      vtx: TVertexList_20;
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
      junk: array [0..5] of AL_INT
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
    clrWhite, SolidBlack: ALLEGRO_COLOR;
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

  procedure TPrimitivesExample.DrawText (
    const aText: String; aParams: array of const;
    const aX, aY: Integer;
    const aCentered: Boolean
  );
  var
    lFlags: LongInt;
  begin
    if aCentered then lFlags := ALLEGRO_ALIGN_CENTRE else lFlags := 0;
    al_draw_textf (
      fFont, clrWhite, aX, aY, lFlags,
      al_string_to_str (aText), aParams
    )
  end;



(* Updates the example. *)
  procedure TPrimitivesExample.Update;

    procedure ProcessKeyboard (const aKeyEvent: ALLEGRO_KEYBOARD_EVENT);
      {$IfDef FPC}inline;{$EndIf}
    begin
      case aKeyEvent.keycode of
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
        fSpeed := fSpeed - RotationSpeed;
      ALLEGRO_KEY_RIGHT:
        fSpeed := fSpeed + RotationSpeed;
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
    end;

  var
    lEvent: ALLEGRO_EVENT;
  begin
  { Events. }
    while not fTerminated and al_get_next_event (fQueue, lEvent) do
    begin
      case lEvent.ftype of
      ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
        begin
          Inc (fCurScreen);
          if fCurScreen > High (fScreens) then fCurScreen := Low (fScreens)
        end;
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        Self.Terminate;
      ALLEGRO_EVENT_KEY_CHAR:
        ProcessKeyboard (lEvent.keyboard)
      end
    end;
  { Update screen. }
    fTheta := fTheta + fSpeed;
    if fScreens[fCurScreen] <> Nil then
      fScreens[fCurScreen].Update
    else
      Self.DrawText ('Example %d not supported', [fCurScreen], 0, 0)
  end;



(* Renders the example. *)
  procedure TPrimitivesExample.Draw;

    procedure DrawStates; {$IfDef FPC}inline;{$EndIf}

    { Helper to print boolean statuses. }
      procedure ShowBoolean (
        const aY: Integer;
        const aLabel: String;
        const aState: Boolean
      );
      var
        lStr: String;
      begin
        if aState then lStr := 'True' else lStr := 'False';
        Self.DrawText ('%s: %s', [aLabel, lStr], 0, aY)
      end;

    begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      if Assigned (fScreens[fCurScreen]) then
        Self.DrawText (
          '%d: %s', [fCurScreen, fScreens[fCurScreen].Name],
          ScreenW div 2, ScreenH - 20, 
          true
        )
      else
        Self.DrawText (
          'Example %d doesn''t work.', [fCurScreen],
          ScreenW div 2, ScreenH - 20, 
          true
        );
      Self.DrawText (
        'FPS: %.2f', [fFramesDone / (al_get_time - fTimeDiff)],
        0, 0
        );
      Self.DrawText ('Change Screen (Up/Down). Esc to Quit.', [], 0, 20);
      Self.DrawText ('Rotation (Left/Right/Space): %.4f', [fSpeed], 0, 40);
      Self.DrawText ('Thickness (PgUp/PgDown): %.2f', [fThickness], 0, 60);
      ShowBoolean (80, 'Software (S)', fSoft);
      ShowBoolean (100, 'Blending (L)', fBlend);
      ShowBoolean (120, 'Background (B)', fDrawBkg);
      ShowBoolean (140, 'Clip (C)', fClip)
    end;

    procedure DrawBackground; {$IfDef FPC}inline;{$EndIF}
    begin
      if fDrawBkg and Assigned (fBkg) then
      begin
        al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
        al_draw_scaled_bitmap (
          fBkg,
          0, 0, al_get_bitmap_width (fbkg), al_get_bitmap_height (fbkg),
          0, 0, ScreenW, ScreenH,
          0
        )
      end
    end;

    procedure DrawExample;{$IfDef FPC}inline;{$EndIf}
    begin
      if fClip then
        al_set_clipping_rectangle (
          ScreenW div 2, ScreenH div 2,
          ScreenW div 2, ScreenH div 2
        );
      if fBlend then
        al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ONE)
      else
        al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      if Assigned (fScreens[fCurScreen]) then
      begin
        al_use_transform (fTransformationMatrix);
        fScreens[fCurScreen].Draw
      end;
      al_use_transform (fIdentity);
      al_set_clipping_rectangle (0, 0, ScreenW, ScreenH)
    end;

  begin
    al_clear_to_color (SolidBlack);
    if fSoft then
    begin
      al_set_target_bitmap (fBuffer);
      al_clear_to_color (SolidBlack)
    end;
    DrawBackground;
    Drawexample;
    if fSoft then
    begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      al_set_target_backbuffer (fDisplay);
      al_draw_bitmap (fBuffer, 0, 0, 0)
    end;
    DrawStates;
    al_flip_display
  end;



(* Constructor. *)
  constructor TPrimitivesExample.Create;
  begin
    inherited Create;
    fBlend := True;
    fTheta := 0;
    fSpeed := RotationSpeed;
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
    if Assigned (fBuffer) then al_destroy_bitmap (fBuffer);
    for Cnt := Low (fScreens) to High (fScreens) do fScreens[Cnt].Free;
    if Assigned (fQueue) then al_destroy_event_queue (fQueue);
    if Assigned (fTexture) then al_destroy_bitmap (fTexture);
    if Assigned (fBkg) then al_destroy_bitmap (fBkg);
    if Assigned (fFont) then al_destroy_font (fFont);
    if Assigned (fDisplay) then al_destroy_display (fDisplay);
    inherited Destroy
  end;



(* Initializes the application. *)
  procedure TPrimitivesExample.Initialize;

    procedure InitializeAllegro;
    var
      lOldBitmapFlags: LongInt;
    begin
      if not al_init or not al_install_keyboard or not al_install_mouse
      or not al_init_image_addon or not al_init_font_addon
      or not al_init_primitives_addon then
        raise Exception.CreateFmt (
          'Could not init Allegro.  Should be compatible with version %d.%d.%d.',
          [ALLEGRO_VERSION, ALLEGRO_SUB_VERSION, ALLEGRO_WIP_VERSION]
        );
    { Display. }
      fDisplay := al_create_display (ScreenW, ScreenH);
      if not Assigned (fDisplay) then
        raise Exception.Create ('Error creating display.');
    { Make and set some color to draw with. }
      SolidBlack := al_map_rgba_f (0, 0, 0, 1);
      clrWhite := al_map_rgba_f (1, 1, 1, 1);
    { Backbuffer. }
      lOldBitmapFlags := al_get_new_bitmap_flags;
      al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
      fBuffer := al_create_bitmap (ScreenW, ScreenH);
      if not Assigned (fBuffer) then
      begin
        ErrorMessage ('Could not create buffer.');
        Self.Terminate;
        Exit
      end;
      al_set_new_bitmap_flags (lOldBitmapFlags);
    { Start the event queue. }
      fQueue := al_create_event_queue;
      al_register_event_source (fQueue, al_get_keyboard_event_source);
      al_register_event_source (fQueue, al_get_display_event_source (fDisplay));
      al_register_event_source (fQueue, al_get_mouse_event_source);
    { Identity matrix. }
      al_identity_transform (fIdentity)
    end;

    procedure LoadData;
    begin
    { Load a font. }
      fFont := al_load_font ('data/fixed_font.tga', 0, 0);
      if not Assigned (fFont) then
      begin
        ErrorMessage ('Error loading "data/fixed_font.tga".');
        Self.Terminate;
        Exit
      end;
    { Textures. }
      fBkg := al_load_bitmap ('data/bkg.png');
      if not Assigned (fBkg) then
      begin
        ErrorMessage ('Error loading "data/bkg.png".');
        Self.Terminate;
        Exit
      end;
      fTexture := al_load_bitmap ('data/texture.tga');
      if not Assigned (fTexture) then
      begin
        ErrorMessage ('Error loading "data/texture.tga".');
        Self.Terminate;
        Exit
      end
    end;

    procedure InitializeVerticesData;
    var
      Ndx: Integer;
      x, y: Single;
      Color: ALLEGRO_COLOR;
    begin
      for Ndx := Low (Self.Vtx) to High (Self.Vtx) do
      begin
        x := Trunc (200 * Cos (Ndx / 13 * ALLEGRO_TAU));
        y := Trunc (200 * Sin (Ndx / 13 * ALLEGRO_TAU));

        Color := al_map_rgb (
          (Ndx + 1) mod 3 * 64,
          (Ndx + 2) mod 3 * 64,
          (Ndx    ) mod 3 * 64
        );

        Self.Vtx[Ndx].x := x; Self.Vtx[Ndx].y := y; Self.Vtx[Ndx].z := 0;
        Self.Vtx[Ndx].u := 64 * x / 100; Self.Vtx[Ndx].v := 64 * y / 100;
        Self.Vtx[Ndx].color := color;
        Self.Vtx2[Ndx].x := 0.1 * x; Self.Vtx2[Ndx].y := 0.1 * y;
        Self.Vtx2[Ndx].u := 64 * x / 100; Self.Vtx2[Ndx].v := 64 * y / 100;
        Self.Vtx2[Ndx].color := color
      end;

      for Ndx := Low (Self.Vtx3) to High (Self.Vtx3) do
      begin
        if (Ndx mod 2) = 0 then
        begin
          x := 150 * Cos (Ndx / 20 * ALLEGRO_TAU);
          y := 150 * Sin (Ndx / 20 * ALLEGRO_TAU)
        end
        else begin
          x := 200 * Cos (Ndx / 20 * ALLEGRO_TAU);
          y := 200 * Sin (Ndx / 20 * ALLEGRO_TAU)
        end;

        if Ndx = 0 then
        begin
          x := 0; Y := 0
        end;

        Self.Vtx3[Ndx].x := x; Self.Vtx3[Ndx].y := y; Self.Vtx3[Ndx].z := 0;
        Self.Vtx3[Ndx].u := 64 * x / 100; Self.Vtx3[Ndx].v := 64 * y / 100;
        Self.Vtx3[Ndx].color := al_map_rgb (
          (7 * Ndx + 1) mod 3 * 64,
          (2 * Ndx + 2) mod 3 * 64,
          (    Ndx    ) mod 3 * 64
        );
      end
    end;

    procedure CreateScreens;
    begin
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
      fScreens[12] := TIndexedBuffers.Create
{$ELSE}
    { For some reason this seems not to work on Linux (or doesn't in my system).

      My system were Xubuntu 18.04.3 + nVidia GeForce GT610 + nouveau 2.4.97.
      Now I've updated to Debian 12 + nVidia GeForce GT610 + nouveau 2.4.114.

      If your system is different you may try.  If it works (or find why mine
      doesn't), let me know.
    }
      fScreens[12] := Nil
{$ENDIF}
    end;

  begin
    fTerminated := False;
    InitializeAllegro;
    LoadData;
    InitializeVerticesData;
    CreateScreens
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
      al_rest (FixedTimestep - fFrameDuration); { rest at least FixedTimestep }
      fFrameDuration := al_get_time - fRealTime;
      fRealTime := al_get_time;
      fGameTime := fRealTime;
      if fRealTime - fGameTime > fFrameDuration then
      { eliminate excess overflow }
        fGameTime := fGameTime + FixedTimestep
                   * Trunc ((fRealTime - fGameTime) / FixedTimestep)
        ;
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



(* Builds the transformation matrix. *)
  procedure TPrimitivesExample.BuildTransform (ax, ay, sx, sy, aTheta: AL_FLOAT);
  begin
    al_build_transform (fTransformationMatrix, ax, ay, sx, sy, aTheta)
  end;



(* Draws text telling the example isn't supported. *)
  procedure TPrimitivesExample.DrawUnsupported;
  begin
    Self.DrawText ('%s not supported', [fScreens[fCurScreen].Name], 0, 0)
  end;



(*
 * TLowPrimitives
 ***************************************************************************)


  constructor TLowPrimitives.Create;
  begin
    inherited Create ('Low Level Primitives')
  end;



  procedure TLowPrimitives.Draw;
  begin
    al_draw_prim (Example.Vtx, Nil, 0, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_prim (Example.Vtx, Nil, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_prim (Example.Vtx, Nil, 9, 13, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_prim (Example.Vtx2, Nil, 0, 13, ALLEGRO_PRIM_POINT_LIST)
  end;



(*
 * TIndexedPrimitives
 ***************************************************************************)

  constructor TIndexedPrimitives.Create;
  var
    Ndx: Integer;
  begin
    inherited Create ('Indexed Primitives');

    for Ndx := 0 to 3 do
    begin
      Indices1[Ndx] := Ndx;
      Indices2[Ndx] := Ndx + 5;
      Indices3[Ndx] := Ndx + 9;
    end;
    Indices1[2] := 3; Indices1[3] := 4
  end;



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



  procedure TIndexedPrimitives.Draw;
  begin
    al_draw_indexed_prim (Example.vtx, Nil, indices1, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_indexed_prim (Example.vtx, Nil, indices2, 4, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_indexed_prim (Example.vtx, Nil, indices3, 4, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_indexed_prim (Example.vtx2, Nil, indices3, 4, ALLEGRO_PRIM_POINT_LIST)
  end;



(*
 * THighPrimitives
 ***************************************************************************)

  constructor THighPrimitives.Create;
  begin
    inherited Create ('High Level Primitives')
  end;



  procedure THighPrimitives.Draw;
  begin
    al_draw_line (-300, -200, 300, 200, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_triangle (-150, -250, 0, 250, 150, -250, al_map_rgba_f (0.5, 0, 0.5, 1), Example.Thickness);
    al_draw_rectangle (-300, -200, 300, 200, al_map_rgba_f (0.5, 0, 0, 1), Example.Thickness);
    al_draw_rounded_rectangle (-200, -125, 200, 125, 50, 100, al_map_rgba_f (0.2, 0.2, 0, 1), Example.Thickness);

    al_draw_ellipse (0, 0, 300, 150, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_elliptical_arc (-20, 0, 300, 200, -ALLEGRO_TAU / 4, -ALLEGRO_TAU / 2, al_map_rgba_f (0.25, 0.25, 0.5, 1), Example.Thickness);
    al_draw_arc (0, 0, 200, -ALLEGRO_TAU / 4, ALLEGRO_TAU / 2, al_map_rgba_f (0.5, 0.25, 0, 1), Example.Thickness);
    al_draw_spline (SplinePoints, al_map_rgba_f (0.1, 0.2, 0.5, 1), Example.Thickness);
    al_draw_pieslice (0, 25, 150, ALLEGRO_TAU * 3 / 8, -ALLEGRO_TAU / 4, al_map_rgba_f (0.4, 0.3, 0.1, 1), Example.Thickness)
  end;



(*
 * TTransformationsPrimitives
 ***************************************************************************)

  constructor TTransformationsPrimitives.Create;
  begin
    inherited Create ('Transformations')
  end;



  procedure TTransformationsPrimitives.Update;
  var
    t: Double;
  begin
    t := al_get_time / 5;
    Example.BuildTransform (
      ScreenW / 2, ScreenH / 2, Sin (t), Cos (t), Example.Theta
    )
  end;



  procedure TTransformationsPrimitives.Draw;
  begin
    al_draw_line (-300, -200, 300, 200, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_triangle (-150, -250, 0, 250, 150, -250, al_map_rgba_f (0.5, 0, 0.5, 1), Example.Thickness);
    al_draw_rectangle (-300, -200, 300, 200, al_map_rgba_f (0.5, 0, 0, 1), Example.Thickness);
    al_draw_rounded_rectangle (-200, -125, 200, 125, 50, 100, al_map_rgba_f (0.2, 0.2, 0, 1), Example.Thickness);

    al_draw_ellipse (0, 0, 300, 150, al_map_rgba_f (0, 0.5, 0.5, 1), Example.Thickness);
    al_draw_elliptical_arc (-20, 0, 300, 200, -ALLEGRO_TAU / 4, -ALLEGRO_TAU / 2, al_map_rgba_f (0.25, 0.25, 0.5, 1), Example.Thickness);
    al_draw_arc (0, 0, 200, -ALLEGRO_TAU / 4, ALLEGRO_TAU / 2, al_map_rgba_f (0.5, 0.25, 0, 1), Example.Thickness);
    al_draw_spline (SplinePoints, al_map_rgba_f (0.1, 0.2, 0.5, 1), Example.Thickness);
    al_draw_pieslice (0, 25, 150, ALLEGRO_TAU * 3 / 8, -ALLEGRO_TAU / 4, al_map_rgba_f (0.4, 0.3, 0.1, 1), Example.Thickness)
  end;



(*
 * TFilledPrimitives
 ***************************************************************************)

  constructor TFilledPrimitives.Create;
  begin
    inherited Create ('Low Level Filled Primitives')
  end;



  procedure TFilledPrimitives.Draw;
  begin
    al_draw_prim (Example.Vtx3, Nil, 0, 6, ALLEGRO_PRIM_TRIANGLE_FAN);
    al_draw_prim (Example.Vtx3, Nil, 7, 13, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_prim (Example.Vtx3, Nil, 14, 20, ALLEGRO_PRIM_TRIANGLE_STRIP)
  end;



(*
 * TIndexedFilledPrimitives
 ***************************************************************************)

  constructor TIndexedFilledPrimitives.Create;
  var
    Ndx: Integer;
  begin
    inherited Create ('Indexed Filled Primitives');
    for Ndx := Low (Indices1) to High (Indices1) do
    begin
      Indices1[Ndx] := Ndx + 12;
      if Ndx > 2 then Inc (Indices1[Ndx]);
      Indices2[Ndx] := Ndx + 6;
      Indices1[Ndx] := Ndx
    end
  end;



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



  procedure TIndexedFilledPrimitives.Draw;
  begin
    al_draw_indexed_prim (Example.Vtx3, Nil, Indices1, 6, ALLEGRO_PRIM_TRIANGLE_LIST);
    al_draw_indexed_prim (Example.Vtx3, Nil, Indices2, 6, ALLEGRO_PRIM_TRIANGLE_STRIP);
    al_draw_indexed_prim (Example.Vtx3, Nil, Indices3, 6, ALLEGRO_PRIM_TRIANGLE_FAN)
  end;



(*
 * THighFilledPrimitives
 ***************************************************************************)

  constructor THighFilledPrimitives.Create;
  begin
    inherited Create ('High Level Filled Primitives')
  end;



  procedure THighFilledPrimitives.Draw;
  begin
    al_draw_filled_triangle (-100, -100, -150, 200, 100, 200, al_map_rgb_f (0.5, 0.7, 0.3));
    al_draw_filled_rectangle (20, -50, 200, 50, al_map_rgb_f (0.3, 0.2, 0.6));
    al_draw_filled_ellipse (-250, 0, 100, 150, al_map_rgb_f (0.3, 0.3, 0.3));
    al_draw_filled_rounded_rectangle (50, -250, 350, -75, 50, 70, al_map_rgb_f (0.4, 0.2, 0));
    al_draw_filled_pieslice (200, 125, 50, ALLEGRO_TAU / 8, 3 * ALLEGRO_TAU / 4, al_map_rgb_f (0.3, 0.3, 0.1))
  end;



(*
 * TTexturePrimitives
 ***************************************************************************)

  constructor TTexturePrimitives.Create;
  var
    Ndx: Integer;
  begin
    inherited Create ('Textured Primitives');

    Self.Vtx := Example.Vtx;
    for Ndx := Low (Vtx) to High (Vtx) do
      if Ndx < 10 then
         vtx[Ndx].color := al_map_rgba_f (1, 1, 1, 1)
  end;



  procedure TTexturePrimitives.Draw;
  begin
    al_draw_prim (Self.Vtx, Example.Texture, 0, 4, ALLEGRO_PRIM_LINE_LIST);
    al_draw_prim (Self.Vtx, Example.Texture, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
    al_draw_prim (Self.Vtx, Example.Texture, 9, 13, ALLEGRO_PRIM_LINE_LOOP);
    al_draw_prim (Example.Vtx2, Example.Texture, 0, 13, ALLEGRO_PRIM_POINT_LIST)
  end;



(*
 * TFilledTexturePrimitives
 ***************************************************************************)

  constructor TFilledTexturePrimitives.Create;
  var
    Ndx: Integer;
  begin
    inherited Create ('Filled Textured Primitives');

    Self.Vtx := Example.Vtx3;
    for Ndx := Low (vtx) to High (vtx) do
      if Ndx < 10 then
        vtx[Ndx].color := al_map_rgba_f (1, 1, 1, 1)
  end;



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



  constructor TCustomVertexFormatPrimitives.Create;
  var
    Ndx, x, y: Integer;
  begin
    inherited Create ('Custom Vertex Format');

    Self.Decl := al_create_vertex_decl (Elems, sizeof (CUSTOM_VERTEX));

    for Ndx := Low (vtx) to High (vtx) do
    begin
      x := Trunc (200 * cos (Ndx / 4 * ALLEGRO_TAU));
      y := Trunc (200 * sin (Ndx / 4 * ALLEGRO_TAU));

      Self.Vtx[Ndx].x := x;
      Self.Vtx[Ndx].y := y;
      Self.Vtx[Ndx].u := Trunc (64 * x / 100);
      Self.Vtx[Ndx].v := Trunc (64 * y / 100);
      Self.Vtx[Ndx].color := al_map_rgba_f (1, 1, 1, 1)
    end
  end;



  procedure TCustomVertexFormatPrimitives.Draw;
  begin
    al_draw_prim_ex (
      @Self.Vtx,
      Self.Decl,
      Example.Texture,
      0,
      4,
      ALLEGRO_PRIM_TRIANGLE_FAN
    )
  end;



(*
 * TVertexBuffers
 ***************************************************************************)

  constructor TVertexBuffers.Create;
  begin
    inherited Create ('Vertex Buffers');

    vbuff := al_create_vertex_buffer (Example.Vtx, ALLEGRO_PRIM_BUFFER_readwrite);
    if not Assigned (vbuff) then
    begin
      vbuff := al_create_vertex_buffer (Example.Vtx, ALLEGRO_PRIM_BUFFER_NONE);
      NoSoft := True
    end
    else
      NoSoft := False;

    vbuff2 := al_create_vertex_buffer (Example.Vtx2, ALLEGRO_PRIM_BUFFER_readwrite);
    if not Assigned (vbuff2) then
    begin
      vbuff2 := al_create_vertex_buffer (Example.Vtx2, ALLEGRO_PRIM_BUFFER_NONE);
      NoSoft2 := True
    end
    else
      NoSoft2 := False
  end;



  destructor TVertexBuffers.Destroy;
  begin
    al_destroy_vertex_buffer (vbuff);
    al_destroy_vertex_buffer (vbuff2);
    inherited Destroy
  end;



  procedure TVertexBuffers.Draw;
  begin
    if Assigned (vbuff) and not (Example.Soft and NoSoft) then
    begin
      al_draw_vertex_buffer (vbuff, Nil, 0, 4, ALLEGRO_PRIM_LINE_LIST);
      al_draw_vertex_buffer (vbuff, Nil, 4, 9, ALLEGRO_PRIM_LINE_STRIP);
      al_draw_vertex_buffer (vbuff, Nil, 9, 13, ALLEGRO_PRIM_LINE_LOOP)
    end
    else
      Example.DrawUnsupported;

    if Assigned (vbuff2) and not (Example.Soft and NoSoft2) then
      al_draw_vertex_buffer (vbuff2, Nil, 0, 13, ALLEGRO_PRIM_POINT_LIST)
    else
      Example.DrawUnsupported
  end;



(*
 * TIndexedBuffers
 ***************************************************************************)

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
    if not Assigned (vbuff) then
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

    if Assigned (vbuff) then
    begin
      vtx := al_lock_vertex_buffer (vbuff, 0, 13, ALLEGRO_LOCK_WRITEONLY);
      try
        for Ndx := 0 to 13 do
        begin
          x := 200 * cos (Ndx / 13 * ALLEGRO_TAU);
          y := 200 * sin (Ndx / 13 * ALLEGRO_TAU);

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



  destructor TIndexedBuffers.Destroy;
  begin
    al_destroy_vertex_buffer (vbuff);
    al_destroy_index_buffer (ibuff);
    inherited Destroy
  end;



  procedure TIndexedBuffers.Update;
  var
    Ndx, t: Integer;
    Indices: ^AL_SHORT;
  begin
    if Assigned (ibuff) then
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
    Example.Free
  end
end.
