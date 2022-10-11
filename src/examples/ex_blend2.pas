program ex_blend2;
(* Example program for the Allegro library.
 *
 * Compare software blending routines with hardware blending.
 *
 * Original by Peter Wang.
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
{ Needed to support classes. }
  {$IFNDEF FPC_DELPHI}
    {$MODE DELPHI}
  {$ENDIF}
{ Windows manifest. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

uses
  alGUI, Common,
  allegro5, al5base, al5font, al5image, al5primitives,
  Classes;

const
  SOURCE_IMAGE_TAG = 0;
  DESTINATION_IMAGE_TAG = 1;

  ORIENTATION_ORIGINAL = 0;
  ORIENTATION_SCALED = 1;
  ORIENTATION_ROTATED = 2;

  ImageNameList: array [0..4] of AL_STR = (
    'Mysha', 'Allegro', 'Mysha (tinted)', 'Allegro (tinted)', 'Color'
  );
  ModeNameList: array [0..9] of AL_STR = (
    'ONE', 'ZERO', 'ALPHA', 'INVERSE',
    'SRC_COLOR', 'DEST_COLOR', 'INV_SRC_COLOR',
    'INV_DEST_COLOR', 'CONST_COLOR',
    'INV_CONST_COLOR'
  );
  ModeValueList: array [0..9] of ALLEGRO_BLEND_MODE = (
    ALLEGRO_ONE, ALLEGRO_ZERO, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA,
    ALLEGRO_SRC_COLOR, ALLEGRO_DEST_COLOR, ALLEGRO_INVERSE_SRC_COLOR,
    ALLEGRO_INVERSE_DEST_COLOR, ALLEGRO_CONST_COLOR,
    ALLEGRO_INVERSE_CONST_COLOR
  );
  OperationTypeNameList: array [0..1] of AL_STR = ('Color', 'Alpha');
  OperationNameList: array [0..2] of AL_STR = (
    'ADD', 'SRC_MINUS_DEST', 'DEST_MINUS_SRC'
  );
  OperationValueList: array [0..2] of ALLEGRO_BLEND_OPERATIONS = (
  ALLEGRO_ADD, ALLEGRO_SRC_MINUS_DEST, ALLEGRO_DEST_MINUS_SRC
  );

type
(* Sample widget. *)
  TSample = class (TWidget)
  private
    fMemory: Boolean;
    fTarget: ALLEGRO_BITMAPptr;
    fOp, fAlphaOp: ALLEGRO_BLEND_OPERATIONS;
    fSrcMode, fDstMode, fSrcAlphaMode, fDstAlphaMode: ALLEGRO_BLEND_MODE;
    fSrcColor, fDstColor, fBlendColor: ALLEGRO_COLOR;
    fSrcImage, fDstImage: ALLEGRO_BITMAPptr; { If Nil then "Color". }
    fSrcTinted, fDstTinted: Boolean;
    fOrientation: Integer;
  public
  (* Constructor. *)
    constructor CreateSample (const aMemory: Boolean);
  (* Destructor. *)
    destructor Destroy; override;
  (* Initializes widget. *)
    procedure Initialize; override;
  (* Render the widget. *)
    procedure Draw; override;

  (* Blender definition. *)
    property Op: ALLEGRO_BLEND_OPERATIONS read fOp write fOp;
    property AOp: ALLEGRO_BLEND_OPERATIONS read fAlphaOp write fAlphaOp;
    property Src: ALLEGRO_BLEND_MODE read fSrcMode write fSrcMode;
    property ASrc: ALLEGRO_BLEND_MODE read fSrcAlphaMode write fSrcAlphaMode;
    property Dst: ALLEGRO_BLEND_MODE read fDstMode write fDstMode;
    property ADst: ALLEGRO_BLEND_MODE read fDstAlphaMode write fDstAlphaMode;
    property SrcColor: ALLEGRO_COLOR read fSrcColor write fSrcColor;
    property DstColor: ALLEGRO_COLOR read fDstColor write fDstColor;
    property Color: ALLEGRO_COLOR read fBlendColor write fBlendColor;
  { Images to use. }
    property How: Integer read fOrientation write fOrientation;
    property SrcImage: ALLEGRO_BITMAPptr read fSrcImage write fSrcImage;
    property SrcTinted: Boolean read fSrcTinted write fSrcTinted;
    property DstImage: ALLEGRO_BITMAPptr read fDstImage write fDstImage;
    property DstTinted: Boolean read fDstTinted write fDstTinted;
  end;



(* Encapsulates the example. *)
  TBlend2Example = class (TDialog)
  private
    Display: ALLEGRO_DISPLAYptr;
    Allegro, Mysha,
    AllegroBmp, MyshaBmp: ALLEGRO_BITMAPptr;
    Sample, SampleBmp: TSample;
    r, g, b, a: array [0..2] of TSlider;

  (* Triggers for opton changing. *)
    procedure onBlendOperationChange (Sender: TObject);
    procedure onBlendModeChange (Sender: TObject);
    procedure onBlendColorChange (Sender: TObject);

    procedure onImageChange (Sender: TObject);
    procedure onOrientationChange (Sender: TObject);
  public
  (* Destructor. *)
    destructor Destroy; override;
  (* Initializes the example. *)
    procedure Initialize; override;
  end;



(* Helper function to build colors. *)
  function Makecol (R, G, B, A: Integer): ALLEGRO_COLOR;
  var
    af: Real;
  begin
  { Premultiply alpha. }
    af := a / 255;
    Result :=
      al_map_rgba_f ((r / 255) * af, (g / 255) * af, (b / 255) * af, af)
  end;



(*
 * TSample
 ***************************************************************************)

(* Constructor. *)
  constructor TSample.CreateSample (const aMemory: Boolean);
  begin
    inherited Create;
    fMemory := aMemory
  end;



(* Destructor. *)
  destructor TSample.Destroy;
  begin
    if fTarget <> Nil then al_destroy_bitmap (fTarget);
    inherited Destroy
  end;



(* Initializes. *)
  procedure TSample.Initialize;
  begin
    inherited Initialize;
  { Combining ALLEGRO_MEMORY_BITMAP with ALLEGRO_VIDEO_BITMAP is invalid,
    so be sure this will not happen. }
      al_set_new_bitmap_flags (
        al_get_new_bitmap_flags
        and (not (ALLEGRO_MEMORY_BITMAP or ALLEGRO_VIDEO_BITMAP))
      );
    if fMemory then
      al_add_new_bitmap_flag (ALLEGRO_MEMORY_BITMAP)
    else
      al_add_new_bitmap_flag (ALLEGRO_VIDEO_BITMAP);
    fTarget := al_create_bitmap (320, 200);
    if fTarget = Nil then
      AbortExample ('Failed creating target bitmap.')
  end;



(* Renders the widget. *)
  procedure TSample.Draw;

    procedure DrawBackground;
    var
      Clr: array [0..1] of ALLEGRO_COLOR;
      i, j, c: Integer;
    begin
    { Draw a background, in case our target bitmap will end up with alpha in it. }
      Clr[0] := al_map_rgba ($66, $66, $66, $FF);
      Clr[1] := al_map_rgba ($99, $99, $99, $FF);
      c := 0;
      for i := 0 to (320 div 16) do for j := 0 to (200 div 16) do
      begin
        al_draw_filled_rectangle (
          X + i * 16,      Y + j * 16,
          X + i * 16 + 16, Y + j * 16 + 16,
          Clr[c]
        );
        c := 1 - c
      end;
    end;

    procedure DrawBitmap (
      aMode: Integer;
      aBitmap: ALLEGRO_BITMAPptr;
      aTinted: Boolean;
      aColor: ALLEGRO_COLOR
    );
    var
      w, h: Integer;
      s: Real;
    begin
      case aMode of
      ORIENTATION_ORIGINAL:
        begin
          if aBitmap = Nil then
            al_draw_filled_rectangle (0, 0, 320, 200, aColor)
          else if aTinted then
            al_draw_tinted_bitmap (aBitmap, aColor, 0, 0, 0)
          else
            al_draw_bitmap (aBitmap, 0, 0, 0)
        end;
      ORIENTATION_SCALED:
        begin
          if aBitmap <> Nil then
          begin
            w := al_get_bitmap_width (aBitmap);
            h := al_get_bitmap_height (aBitmap);
            s := 200 / h * 0.9;
            if aTinted then
              al_draw_tinted_scaled_bitmap (
                aBitmap, aColor,
                0, 0, w, h,
                160 - w * s / 2, 100 - h * s / 2, w * s, h * s,
                0
              )
            else
              al_draw_scaled_bitmap (
                aBitmap,
                0, 0, w, h,
                160 - w * s / 2, 100 - h * s / 2, w * s, h * s,
                0
              )
          end
          else
            al_draw_filled_rectangle (10, 10, 300, 180, aColor)
        end;
      ORIENTATION_ROTATED:
        begin
          if aBitmap = Nil then
            al_draw_filled_circle (160, 100, 100, aColor)
          else if aTinted then
            al_draw_tinted_rotated_bitmap (
              aBitmap, aColor,
              160, 100,
              160, 100,
              ALLEGRO_TAU / 16,
              0
            )
          else
            al_draw_rotated_bitmap (
              aBitmap,
              160, 100,
              160, 100,
              ALLEGRO_TAU / 16,
              0
            )
        end;
      end
    end;

  var
    State: ALLEGRO_STATE;
  begin
    al_store_state (State, ALLEGRO_STATE_TARGET_BITMAP or ALLEGRO_STATE_BLENDER);
    try
      DrawBackground;
      al_set_target_bitmap (fTarget);
    { Initialize with destination. }
      al_clear_to_color (al_map_rgba_f (0, 0, 0, 0)); { Just in case. }
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      DrawBitmap (ORIENTATION_ORIGINAL, DstImage, DstTinted, DstColor);
    { Now draw the blended source over it. }
      al_set_separate_blender (Op, Src, Dst, AOp, ASrc, ADst);
      al_set_blend_color (Color);
      DrawBitmap (How, SrcImage, SrcTinted, SrcColor);
    { Display results. }
      al_restore_state (State);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_bitmap (fTarget, Self.X, Self.Y, 0)
    finally
      al_restore_state (State)
    end
  end;



(*
 * TBlend2Example
 ***************************************************************************)

(* Triggers for opton changing. *)
  procedure TBlend2Example.onBlendOperationChange (Sender: TObject);
  begin
    case TOptionList (Sender).Tag of
    4:
      begin
        Sample.Op := OperationValueList[TOptionList (Sender).Selected];
        SampleBmp.Op := OperationValueList[TOptionList (Sender).Selected]
      end;
    5:
      begin
        Sample.aOp := OperationValueList[TOptionList (Sender).Selected];
        SampleBmp.aOp := OperationValueList[TOptionList (Sender).Selected]
      end;
    else
      AbortExample ('Bad trigger (onOperationChange)');
    end
  end;

  procedure TBlend2Example.onBlendModeChange (Sender: TObject);
  begin
    case TOptionList (Sender).Tag of
    0:
      begin
        Sample.Src := ModeValueList[TOptionList (Sender).Selected];
        SampleBmp.Src := ModeValueList[TOptionList (Sender).Selected]
      end;
    1:
      begin
        Sample.ASrc := ModeValueList[TOptionList (Sender).Selected];
        SampleBmp.ASrc := ModeValueList[TOptionList (Sender).Selected]
      end;
    2:
      begin
        Sample.Dst := ModeValueList[TOptionList (Sender).Selected];
        SampleBmp.Dst := ModeValueList[TOptionList (Sender).Selected]
      end;
    3:
      begin
        Sample.ADst := ModeValueList[TOptionList (Sender).Selected];
        SampleBmp.ADst := ModeValueList[TOptionList (Sender).Selected]
      end;
    else
      AbortExample ('Bad trigger (onOperationChange)');
    end
  end;



  procedure TBlend2Example.onBlendColorChange (Sender: TObject);
  var
    Clr: ALLEGRO_COLOR;
    Tag: Integer;
  begin
    Tag := TWidget (Sender).Tag;
    Clr := Makecol (r[Tag].Value, g[Tag].Value, b[Tag].Value, a[Tag].Value);
    case Tag of
    0:
      begin
        Sample.SrcColor := Clr;
        SampleBmp.SrcColor := Clr
      end;
    1:
      begin
        Sample.DstColor := Clr;
        SampleBmp.DstColor := Clr
      end;
    2:
      begin
        Sample.Color := Clr;
        SampleBmp.Color := Clr
      end;
    end
  end;



  procedure TBlend2Example.onImageChange (Sender: TObject);
  var
    lBmp, lBmpMem: ALLEGRO_BITMAPptr;
  begin
  { Get image reference. }
    case TOptionList (Sender).Selected of
    0, 2:
      begin
        lBmp := Mysha; lBmpMem := MyshaBmp
      end;
    1, 3:
      begin
        lBmp := Allegro; lBmpMem := AllegroBmp
      end;
    else
      begin
        lBmp := Nil; lBmpMem := Nil
      end;
    end;
  { Assign to samples. }
    case TOptionList (Sender).Tag of
    SOURCE_IMAGE_TAG:
      begin
        Sample.SrcImage := lBmp;
        SampleBmp.SrcImage := lBmpMem;
        Sample.SrcTinted := TOptionList (Sender).Selected IN [2, 3];
        SampleBmp.SrcTinted := TOptionList (Sender).Selected IN [2, 3]
      end;
    DESTINATION_IMAGE_TAG:
      begin
        Sample.DstImage := lBmp;
        SampleBmp.DstImage := lBmpMem;
        Sample.DstTinted := TOptionList (Sender).Selected IN [2, 3];
        SampleBmp.DstTinted := TOptionList (Sender).Selected IN [2, 3]
      end;
    end
  end;



  procedure TBlend2Example.onOrientationChange (Sender: TObject);
  begin
    Sample.How := TOptionList (Sender).Selected;
    SampleBmp.How := TOptionList (Sender).Selected
  end;



(* Destructor. *)
  destructor TBlend2Example.Destroy;
  begin
    if AllegroBmp <> Nil then al_destroy_bitmap (AllegroBmp);
    if MyshaBmp <> Nil then al_destroy_bitmap (MyshaBmp);
    if Allegro <> Nil then al_destroy_bitmap (Allegro);
    if Mysha <> Nil then al_destroy_bitmap (Mysha);

    inherited Destroy
  end;



(* Initializes the example. *)
  procedure TBlend2Example.Initialize;

    function AddRGBSlider (aNdx, aY: Integer): TSlider;
    var
      aX: Integer;
    begin
      aX := 1 + aNdx * 6;

      Result := TSlider.CreateSlider (255, oHorizontal);
      Result.Tag := aNdx;
      Result.Value := 255;
      Result.OnChange := Self.onBlendColorChange;
      Self.Add (Result, aX, aY, 5, 1)
    end;

    function CreateOptionList (
      aOptions: array of AL_STR;
      const aTag: Integer;
      aCallback: TNotifyEvent
    ): TOptionList;
    begin
      Result := TOptionList.CreateOptionList (aOptions);
      Result.Tag := aTag;
      Result.OnSelectChange := aCallback;
    { This is to set the initial values of the related objects. }
      Result.Selected := 1; Result.Selected := 0
    end;

  var
    lTextFont: ALLEGRO_FONTptr;
    Ndx: Integer;
  begin
    al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
    Display := al_create_display (800, 600);
    if Display = Nil then  AbortExample ('Error creating display.');

    lTextFont := al_load_font ('data/fixed_font.tga', 0, 0);
    if lTextFont = Nil then
      AbortExample ('Failed to load data/fixed_font.tga.');
    Self.TextFont := lTextFont;
  { Load and prepare bitmaps. }
    Allegro := al_load_bitmap ('data/allegro.pcx');
    if Allegro = Nil then
      AbortExample ('Failed to load data/allegro.pcx.');
    Mysha := al_load_bitmap ('data/mysha256x256.png');
    if Mysha = Nil then
      AbortExample ('Failed to load data/mysha256x256.png.');

    al_add_new_bitmap_flag (ALLEGRO_MEMORY_BITMAP);
    AllegroBmp := al_clone_bitmap (Allegro);
    MyshaBmp := al_clone_bitmap (Mysha);
  { Create dialog. }
    Self.Add (
      TLabel.CreateLabel ('Texture'),
      0, 0, 10, 2
    );
    Self.Sample := TSample.CreateSample (False);
    Self.Add (Self.Sample, 1, 1, 8, 14);
    Self.Add (
      TLabel.CreateLabel ('Memory'),
      9, 0, 10, 2
    );
    Self.SampleBmp := TSample.CreateSample (True);
    Self.Add (Self.SampleBmp, 10, 1, 8, 14);
  { source_image }
    Self.Add (
      TLabel.CreateLabel ('Source', False),
      1, 15, 5, 2
    );
    Self.Add (
      CreateOptionList (ImageNameList, SOURCE_IMAGE_TAG, Self.onImageChange),
      1, 16, 4, 6
    );
  { destination_image }
    Self.Add (
      TLabel.CreateLabel ('Destination', False),
      7, 15, 6, 2
    );
    Self.Add (
      CreateOptionList (ImageNameList, DESTINATION_IMAGE_TAG, Self.onImageChange),
      7, 16, 4, 6
    );
  { draw_mode }
    Self.Add (
      CreateOptionList (['original','scaled','rotated'], 0, Self.onOrientationChange),
      13, 16, 4, 6
    );
  { operations[i] }
    for Ndx := 0 to 3 do
    begin
      Self.Add ( { operation_label[i] }
        TLabel.CreateLabel (OperationTypeNameList[Ndx mod 2], False),
        1 + Ndx * 3, 23, 3, 2
      );
      Self.Add (
        CreateOptionList (ModeNameList, Ndx, Self.onBlendModeChange),
        1 + Ndx * 3, 24, 3, 10
      )
    end;
    Self.Add ( { operation_label[4] }
      TLabel.CreateLabel ('Blend op', False),
        13, 23, 3, 2
      );
    Self.Add ( { operation_label[5] }
      TLabel.CreateLabel ('Alpha op', False),
        16, 23, 3, 2
      );

    for Ndx := 4 to 5 do
      Self.Add ( { operations[i] }
        CreateOptionList (OperationNameList, Ndx, Self.onBlendOperationChange),
        1 + Ndx * 3, 24, 3, 6
      );

    Self.Add ( { rgba_label[0] }
      TLabel.CreateLabel ('Source tint/color RGBA'),
      1, 34, 5, 1
    );
    Self.Add ( { rgba_label[1] }
      TLabel.CreateLabel ('Dest tint/color RGBA'),
      7, 34, 5, 1
    );
    Self.Add ( { rgba_label[2] }
      TLabel.CreateLabel ('Const color RGBA'),
      13, 34, 5, 1
    );
    for Ndx := 0 to 2 do
    begin
      r[Ndx] := AddRGBSlider (Ndx, 35);
      g[Ndx] := AddRGBSlider (Ndx, 36);
      b[Ndx] := AddRGBSlider (Ndx, 37);
      a[Ndx] := AddRGBSlider (Ndx, 38);
    { This is to set the initial values of the related objects. }
      a[Ndx].Value := 0; a[Ndx].Value := 255
    end;

    inherited Initialize
  end;

var
  Blend2Example: TBlend2Example;

begin
  if not al_init then AbortExample ('Could not init Allegro.');
  al_install_keyboard;
  al_install_mouse;

  al_init_font_addon;
  al_init_image_addon;
  al_init_primitives_addon;
  InitPlatformSpecific;

  Blend2Example := TBlend2Example.Create (20, 40);
  Blend2Example.Initialize;
  Blend2Example.Run;
  Blend2Example.Free
end.
