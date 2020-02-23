PROGRAM ex_blend2;
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

USES
  alGUI, Common,
  allegro5, al5base, al5font, al5image, al5primitives,
  Classes;

CONST
  SOURCE_IMAGE_TAG = 0;
  DESTINATION_IMAGE_TAG = 1;

  ORIENTATION_ORIGINAL = 0;
  ORIENTATION_SCALED = 1;
  ORIENTATION_ROTATED = 2;

  ImageNameList: ARRAY [0..4] OF AL_STR = (
    'Mysha', 'Allegro', 'Mysha (tinted)', 'Allegro (tinted)', 'Color'
  );
  ModeNameList: ARRAY [0..9] OF AL_STR = (
    'ONE', 'ZERO', 'ALPHA', 'INVERSE',
    'SRC_COLOR', 'DEST_COLOR', 'INV_SRC_COLOR',
    'INV_DEST_COLOR', 'CONST_COLOR',
    'INV_CONST_COLOR'
  );
  ModeValueList: ARRAY [0..9] OF ALLEGRO_BLEND_MODE = (
    ALLEGRO_ONE, ALLEGRO_ZERO, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA,
    ALLEGRO_SRC_COLOR, ALLEGRO_DEST_COLOR, ALLEGRO_INVERSE_SRC_COLOR,
    ALLEGRO_INVERSE_DEST_COLOR, ALLEGRO_CONST_COLOR,
    ALLEGRO_INVERSE_CONST_COLOR
  );
  OperationTypeNameList: ARRAY [0..1] OF AL_STR = ('Color', 'Alpha');
  OperationNameList: ARRAY [0..2] OF AL_STR = (
    'ADD', 'SRC_MINUS_DEST', 'DEST_MINUS_SRC'
  );
  OperationValueList: ARRAY [0..2] OF ALLEGRO_BLEND_OPERATIONS = (
  ALLEGRO_ADD, ALLEGRO_SRC_MINUS_DEST, ALLEGRO_DEST_MINUS_SRC
  );

TYPE
(* Sample widget. *)
  TSample = CLASS (TWidget)
  PRIVATE
    fMemory: BOOLEAN;
    fTarget: ALLEGRO_BITMAPptr;
    fOp, fAlphaOp: ALLEGRO_BLEND_OPERATIONS;
    fSrcMode, fDstMode, fSrcAlphaMode, fDstAlphaMode: ALLEGRO_BLEND_MODE;
    fSrcColor, fDstColor, fBlendColor: ALLEGRO_COLOR;
    fSrcImage, fDstImage: ALLEGRO_BITMAPptr; { If NIL then "Color". }
    fSrcTinted, fDstTinted: BOOLEAN;
    fOrientation: INTEGER;
  PUBLIC
  (* Constructor. *)
    CONSTRUCTOR CreateSample (CONST aMemory: BOOLEAN);
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Initializes widget. *)
    PROCEDURE Initialize; OVERRIDE;
  (* Render the widget. *)
    PROCEDURE Draw; OVERRIDE;

  (* Blender definition. *)
    PROPERTY Op: ALLEGRO_BLEND_OPERATIONS READ fOp WRITE fOp;
    PROPERTY AOp: ALLEGRO_BLEND_OPERATIONS READ fAlphaOp WRITE fAlphaOp;
    PROPERTY Src: ALLEGRO_BLEND_MODE READ fSrcMode WRITE fSrcMode;
    PROPERTY ASrc: ALLEGRO_BLEND_MODE READ fSrcAlphaMode WRITE fSrcAlphaMode;
    PROPERTY Dst: ALLEGRO_BLEND_MODE READ fDstMode WRITE fDstMode;
    PROPERTY ADst: ALLEGRO_BLEND_MODE READ fDstAlphaMode WRITE fDstAlphaMode;
    PROPERTY SrcColor: ALLEGRO_COLOR READ fSrcColor WRITE fSrcColor;
    PROPERTY DstColor: ALLEGRO_COLOR READ fDstColor WRITE fDstColor;
    PROPERTY Color: ALLEGRO_COLOR READ fBlendColor WRITE fBlendColor;
  { Images to use. }
    PROPERTY How: INTEGER READ fOrientation WRITE fOrientation;
    PROPERTY SrcImage: ALLEGRO_BITMAPptr READ fSrcImage WRITE fSrcImage;
    PROPERTY SrcTinted: BOOLEAN READ fSrcTinted WRITE fSrcTinted;
    PROPERTY DstImage: ALLEGRO_BITMAPptr READ fDstImage WRITE fDstImage;
    PROPERTY DstTinted: BOOLEAN READ fDstTinted WRITE fDstTinted;
  END;



(* Encapsulates the example. *)
  TBlend2Example = CLASS (TDialog)
  PRIVATE
    Display: ALLEGRO_DISPLAYptr;
    Allegro, Mysha,
    AllegroBmp, MyshaBmp: ALLEGRO_BITMAPptr;
    Sample, SampleBmp: TSample;
    r, g, b, a: ARRAY [0..2] OF TSlider;

  (* Triggers for opton changing. *)
    PROCEDURE onBlendOperationChange (Sender: TObject);
    PROCEDURE onBlendModeChange (Sender: TObject);
    PROCEDURE onBlendColorChange (Sender: TObject);

    PROCEDURE onImageChange (Sender: TObject);
    PROCEDURE onOrientationChange (Sender: TObject);
  PUBLIC
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Initializes the example. *)
    PROCEDURE Initialize; OVERRIDE;
  END;



(* Helper function to build colors. *)
  FUNCTION Makecol (R, G, B, A: INTEGER): ALLEGRO_COLOR;
  VAR
    af: REAL;
  BEGIN
  { Premultiply alpha. }
    af := a / 255;
    RESULT :=
      al_map_rgba_f ((r / 255) * af, (g / 255) * af, (b / 255) * af, af)
  END;



(*
 * TSample
 ***************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TSample.CreateSample (CONST aMemory: BOOLEAN);
  BEGIN
    INHERITED Create;
    fMemory := aMemory
  END;



(* Destructor. *)
  DESTRUCTOR TSample.Destroy;
  BEGIN
    IF fTarget <> NIL THEN al_destroy_bitmap (fTarget);
    INHERITED Destroy
  END;



(* Initializes. *)
  PROCEDURE TSample.Initialize;
  BEGIN
    INHERITED Initialize;
  { Combining ALLEGRO_MEMORY_BITMAP with ALLEGRO_VIDEO_BITMAP is invalid,
    so be sure this will not happen. }
      al_set_new_bitmap_flags (
        al_get_new_bitmap_flags
        AND (NOT (ALLEGRO_MEMORY_BITMAP OR ALLEGRO_VIDEO_BITMAP))
      );
    IF fMemory THEN
      al_add_new_bitmap_flag (ALLEGRO_MEMORY_BITMAP)
    ELSE
      al_add_new_bitmap_flag (ALLEGRO_VIDEO_BITMAP);
    fTarget := al_create_bitmap (320, 200);
    IF fTarget = NIL THEN
      AbortExample ('Failed creating target bitmap.')
  END;



(* Renders the widget. *)
  PROCEDURE TSample.Draw;

    PROCEDURE DrawBackground;
    VAR
      Clr: ARRAY [0..1] OF ALLEGRO_COLOR;
      i, j, c: INTEGER;
    BEGIN
    { Draw a background, in case our target bitmap will end up with alpha in it. }
      Clr[0] := al_map_rgba ($66, $66, $66, $FF);
      Clr[1] := al_map_rgba ($99, $99, $99, $FF);
      c := 0;
      FOR i := 0 TO (320 DIV 16) DO FOR j := 0 TO (200 DIV 16) DO
      BEGIN
        al_draw_filled_rectangle (
          X + i * 16,      Y + j * 16,
          X + i * 16 + 16, Y + j * 16 + 16,
          Clr[c]
        );
        c := 1 - c
      END;
    END;

    PROCEDURE DrawBitmap (
      aMode: INTEGER;
      aBitmap: ALLEGRO_BITMAPptr;
      aTinted: BOOLEAN;
      aColor: ALLEGRO_COLOR
    );
    VAR
      w, h: INTEGER;
      s: REAL;
    BEGIN
      CASE aMode OF
      ORIENTATION_ORIGINAL:
        BEGIN
          IF aBitmap = NIL THEN
            al_draw_filled_rectangle (0, 0, 320, 200, aColor)
          ELSE IF aTinted THEN
            al_draw_tinted_bitmap (aBitmap, aColor, 0, 0, 0)
          ELSE
            al_draw_bitmap (aBitmap, 0, 0, 0)
        END;
      ORIENTATION_SCALED:
        BEGIN
          IF aBitmap <> NIL THEN
          BEGIN
            w := al_get_bitmap_width (aBitmap);
            h := al_get_bitmap_height (aBitmap);
            s := 200 / h * 0.9;
            IF aTinted THEN
              al_draw_tinted_scaled_bitmap (
                aBitmap, aColor,
                0, 0, w, h,
                160 - w * s / 2, 100 - h * s / 2, w * s, h * s,
                0
              )
            ELSE
              al_draw_scaled_bitmap (
                aBitmap,
                0, 0, w, h,
                160 - w * s / 2, 100 - h * s / 2, w * s, h * s,
                0
              )
          END
          ELSE
            al_draw_filled_rectangle (10, 10, 300, 180, aColor)
        END;
      ORIENTATION_ROTATED:
        BEGIN
          IF aBitmap = NIL THEN
            al_draw_filled_circle (160, 100, 100, aColor)
          ELSE IF aTinted THEN
            al_draw_tinted_rotated_bitmap (
              aBitmap, aColor,
              160, 100,
              160, 100,
              ALLEGRO_TAU / 16,
              0
            )
          ELSE
            al_draw_rotated_bitmap (
              aBitmap,
              160, 100,
              160, 100,
              ALLEGRO_TAU / 16,
              0
            )
        END;
      END
    END;

  VAR
    State: ALLEGRO_STATE;
  BEGIN
    al_store_state (State, ALLEGRO_STATE_TARGET_BITMAP OR ALLEGRO_STATE_BLENDER);
    TRY
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
      al_draw_bitmap (fTarget, SELF.X, SELF.Y, 0)
    FINALLY
      al_restore_state (State)
    END
  END;



(*
 * TBlend2Example
 ***************************************************************************)

(* Triggers for opton changing. *)
  PROCEDURE TBlend2Example.onBlendOperationChange (Sender: TObject);
  BEGIN
    CASE TOptionList (Sender).Tag OF
    4:
      BEGIN
        Sample.Op := OperationValueList[TOptionList (Sender).Selected];
        SampleBmp.Op := OperationValueList[TOptionList (Sender).Selected]
      END;
    5:
      BEGIN
        Sample.aOp := OperationValueList[TOptionList (Sender).Selected];
        SampleBmp.aOp := OperationValueList[TOptionList (Sender).Selected]
      END;
    ELSE
      AbortExample ('Bad trigger (onOperationChange)');
    END
  END;

  PROCEDURE TBlend2Example.onBlendModeChange (Sender: TObject);
  BEGIN
    CASE TOptionList (Sender).Tag OF
    0:
      BEGIN
        Sample.Src := ModeValueList[TOptionList (Sender).Selected];
        SampleBmp.Src := ModeValueList[TOptionList (Sender).Selected]
      END;
    1:
      BEGIN
        Sample.ASrc := ModeValueList[TOptionList (Sender).Selected];
        SampleBmp.ASrc := ModeValueList[TOptionList (Sender).Selected]
      END;
    2:
      BEGIN
        Sample.Dst := ModeValueList[TOptionList (Sender).Selected];
        SampleBmp.Dst := ModeValueList[TOptionList (Sender).Selected]
      END;
    3:
      BEGIN
        Sample.ADst := ModeValueList[TOptionList (Sender).Selected];
        SampleBmp.ADst := ModeValueList[TOptionList (Sender).Selected]
      END;
    ELSE
      AbortExample ('Bad trigger (onOperationChange)');
    END
  END;



  PROCEDURE TBlend2Example.onBlendColorChange (Sender: TObject);
  VAR
    Clr: ALLEGRO_COLOR;
    Tag: INTEGER;
  BEGIN
    Tag := TWidget (Sender).Tag;
    Clr := Makecol (r[Tag].Value, g[Tag].Value, b[Tag].Value, a[Tag].Value);
    CASE Tag OF
    0:
      BEGIN
        Sample.SrcColor := Clr;
        SampleBmp.SrcColor := Clr
      END;
    1:
      BEGIN
        Sample.DstColor := Clr;
        SampleBmp.DstColor := Clr
      END;
    2:
      BEGIN
        Sample.Color := Clr;
        SampleBmp.Color := Clr
      END;
    END
  END;



  PROCEDURE TBlend2Example.onImageChange (Sender: TObject);
  VAR
    lBmp, lBmpMem: ALLEGRO_BITMAPptr;
  BEGIN
  { Get image reference. }
    CASE TOptionList (Sender).Selected OF
    0, 2:
      BEGIN
        lBmp := Mysha; lBmpMem := MyshaBmp
      END;
    1, 3:
      BEGIN
        lBmp := Allegro; lBmpMem := AllegroBmp
      END;
    ELSE
      BEGIN
        lBmp := NIL; lBmpMem := NIL
      END;
    END;
  { Assign to samples. }
    CASE TOptionList (Sender).Tag OF
    SOURCE_IMAGE_TAG:
      BEGIN
        Sample.SrcImage := lBmp;
        SampleBmp.SrcImage := lBmpMem;
        Sample.SrcTinted := TOptionList (Sender).Selected IN [2, 3];
        SampleBmp.SrcTinted := TOptionList (Sender).Selected IN [2, 3]
      END;
    DESTINATION_IMAGE_TAG:
      BEGIN
        Sample.DstImage := lBmp;
        SampleBmp.DstImage := lBmpMem;
        Sample.DstTinted := TOptionList (Sender).Selected IN [2, 3];
        SampleBmp.DstTinted := TOptionList (Sender).Selected IN [2, 3]
      END;
    END
  END;



  PROCEDURE TBlend2Example.onOrientationChange (Sender: TObject);
  BEGIN
    Sample.How := TOptionList (Sender).Selected;
    SampleBmp.How := TOptionList (Sender).Selected
  END;



(* Destructor. *)
  DESTRUCTOR TBlend2Example.Destroy;
  BEGIN
    IF AllegroBmp <> NIL THEN al_destroy_bitmap (AllegroBmp);
    IF MyshaBmp <> NIL THEN al_destroy_bitmap (MyshaBmp);
    IF Allegro <> NIL THEN al_destroy_bitmap (Allegro);
    IF Mysha <> NIL THEN al_destroy_bitmap (Mysha);

    INHERITED Destroy
  END;



(* Initializes the example. *)
  PROCEDURE TBlend2Example.Initialize;

    FUNCTION AddRGBSlider (aNdx, aY: INTEGER): TSlider;
    VAR
      aX: INTEGER;
    BEGIN
      aX := 1 + aNdx * 6;

      RESULT := TSlider.CreateSlider (255, oHorizontal);
      RESULT.Tag := aNdx;
      RESULT.Value := 255;
      RESULT.OnChange := SELF.onBlendColorChange;
      SELF.Add (RESULT, aX, aY, 5, 1)
    END;

    FUNCTION CreateOptionList (
      aOptions: ARRAY OF AL_STR;
      CONST aTag: INTEGER;
      aCallback: TNotifyEvent
    ): TOptionList;
    BEGIN
      RESULT := TOptionList.CreateOptionList (aOptions);
      RESULT.Tag := aTag;
      RESULT.OnSelectChange := aCallback;
    { This is to set the initial values of the related objects. }
      RESULT.Selected := 1; RESULT.Selected := 0
    END;

  VAR
    lTextFont: ALLEGRO_FONTptr;
    Ndx: INTEGER;
  BEGIN
    al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
    Display := al_create_display (800, 600);
    IF Display = NIL THEN  AbortExample ('Error creating display.');

    lTextFont := al_load_font ('data/fixed_font.tga', 0, 0);
    IF lTextFont = NIL THEN
      AbortExample ('Failed to load data/fixed_font.tga.');
    SELF.TextFont := lTextFont;
  { Load and prepare bitmaps. }
    Allegro := al_load_bitmap ('data/allegro.pcx');
    IF Allegro = NIL THEN
      AbortExample ('Failed to load data/allegro.pcx.');
    Mysha := al_load_bitmap ('data/mysha256x256.png');
    IF Mysha = NIL THEN
      AbortExample ('Failed to load data/mysha256x256.png.');

    al_add_new_bitmap_flag (ALLEGRO_MEMORY_BITMAP);
    AllegroBmp := al_clone_bitmap (Allegro);
    MyshaBmp := al_clone_bitmap (Mysha);
  { Create dialog. }
    SELF.Add (
      TLabel.CreateLabel ('Texture'),
      0, 0, 10, 2
    );
    SELF.Sample := TSample.CreateSample (FALSE);
    SELF.Add (SELF.Sample, 1, 1, 8, 14);
    SELF.Add (
      TLabel.CreateLabel ('Memory'),
      9, 0, 10, 2
    );
    SELF.SampleBmp := TSample.CreateSample (TRUE);
    SELF.Add (SELF.SampleBmp, 10, 1, 8, 14);
  { source_image }
    SELF.Add (
      TLabel.CreateLabel ('Source', FALSE),
      1, 15, 5, 2
    );
    SELF.Add (
      CreateOptionList (ImageNameList, SOURCE_IMAGE_TAG, SELF.onImageChange),
      1, 16, 4, 6
    );
  { destination_image }
    SELF.Add (
      TLabel.CreateLabel ('Destination', FALSE),
      7, 15, 6, 2
    );
    SELF.Add (
      CreateOptionList (ImageNameList, DESTINATION_IMAGE_TAG, SELF.onImageChange),
      7, 16, 4, 6
    );
  { draw_mode }
    SELF.Add (
      CreateOptionList (['original','scaled','rotated'], 0, SELF.onOrientationChange),
      13, 16, 4, 6
    );
  { operations[i] }
    FOR Ndx := 0 TO 3 DO
    BEGIN
      SELF.Add ( { operation_label[i] }
        TLabel.CreateLabel (OperationTypeNameList[Ndx MOD 2], FALSE),
        1 + Ndx * 3, 23, 3, 2
      );
      SELF.Add (
        CreateOptionList (ModeNameList, Ndx, SELF.onBlendModeChange),
        1 + Ndx * 3, 24, 3, 10
      )
    END;
    SELF.Add ( { operation_label[4] }
      TLabel.CreateLabel ('Blend op', FALSE),
        13, 23, 3, 2
      );
    SELF.Add ( { operation_label[5] }
      TLabel.CreateLabel ('Alpha op', FALSE),
        16, 23, 3, 2
      );

    FOR Ndx := 4 TO 5 DO
      SELF.Add ( { operations[i] }
        CreateOptionList (OperationNameList, Ndx, SELF.onBlendOperationChange),
        1 + Ndx * 3, 24, 3, 6
      );

    SELF.Add ( { rgba_label[0] }
      TLabel.CreateLabel ('Source tint/color RGBA'),
      1, 34, 5, 1
    );
    SELF.Add ( { rgba_label[1] }
      TLabel.CreateLabel ('Dest tint/color RGBA'),
      7, 34, 5, 1
    );
    SELF.Add ( { rgba_label[2] }
      TLabel.CreateLabel ('Const color RGBA'),
      13, 34, 5, 1
    );
    FOR Ndx := 0 TO 2 DO
    BEGIN
      r[Ndx] := AddRGBSlider (Ndx, 35);
      g[Ndx] := AddRGBSlider (Ndx, 36);
      b[Ndx] := AddRGBSlider (Ndx, 37);
      a[Ndx] := AddRGBSlider (Ndx, 38);
    { This is to set the initial values of the related objects. }
      a[Ndx].Value := 0; a[Ndx].Value := 255
    END;

    INHERITED Initialize
  END;

VAR
  Blend2Example: TBlend2Example;

BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
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
END.
