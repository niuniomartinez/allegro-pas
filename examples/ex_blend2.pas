PROGRAM ex_blend2;
(* Example program for the Allegro library.
 *
 * Compare software blending routines with hardware blending.
 *
 * Original by Peter Wang.
 *)
(*
  Copyright (c) 2012-2019 Guillermo Martínez J.

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
  {$IF NOT DEFINED(FPC_DELPHI)}
    {$MODE DELPHI}
  {$ENDIF}
{ Windows manifest. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  USES
    Common, NihGUI,
    Allegro5, al5font, al5image, al5primitives;

  VAR
    Allegro, Mysha, AllegroBmp, MyshaBmp, Target, TargetBmp: ALLEGRO_BITMAPptr;

  TYPE
    TProg = CLASS (TObject)
    PRIVATE
      fDialog: TDialog;
      fMemoryLabel, fTextureLabel, fSourceLabel, fDestinationLabel: TLabel;
      fSourceImage, fDestinationImage, fDrawMode: TList;
      fOperationLabel: ARRAY [0..5] OF TLabel;
      fOperations: ARRAY [0..5] OF TList;
      fRgbaLabel: ARRAY [0..3] OF TLabel;
      fR, fG, fB, fA: ARRAY [0..3] OF THSlider;
    PRIVATE
      FUNCTION StrToBlendMode (aStr: STRING): ALLEGRO_BLEND_MODE;
      FUNCTION StrToBlendOperation (aStr: STRING): ALLEGRO_BLEND_OPERATIONS;
      FUNCTION makecol (r, g, b, a: INTEGER): ALLEGRO_COLOR;
      PROCEDURE DrawBackground (x, y: INTEGER);

      PROCEDURE BlendingTest (aMemory: BOOLEAN);
      PROCEDURE DrawSamples;
      PROCEDURE DrawBitmap
	(aStr, aHow: STRING; aMemory, aDestination: BOOLEAN);
    PUBLIC
      CONSTRUCTOR Create (aTheme: TTheme; aDisplay: ALLEGRO_DISPLAYptr);
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Run;
    END;

  FUNCTION TProg.StrToBlendMode (aStr: STRING): ALLEGRO_BLEND_MODE;
  BEGIN
    IF aStr = 'ZERO' THEN EXIT (ALLEGRO_ZERO);
    IF aStr = 'ONE' THEN EXIT (ALLEGRO_ONE);
    IF aStr = 'SRC_COLOR' THEN EXIT (ALLEGRO_SRC_COLOR);
    IF aStr = 'DEST_COLOR' THEN EXIT (ALLEGRO_DEST_COLOR);
    IF aStr = 'INV_SRC_COLOR' THEN EXIT (ALLEGRO_INVERSE_SRC_COLOR);
    IF aStr = 'INV_DEST_COLOR' THEN EXIT (ALLEGRO_INVERSE_DEST_COLOR);
    IF aStr = 'ALPHA' THEN EXIT (ALLEGRO_ALPHA);
    IF aStr = 'INVERSE' THEN EXIT (ALLEGRO_INVERSE_ALPHA);
    IF aStr = 'CONST_COLOR' THEN EXIT (ALLEGRO_CONST_COLOR);
    IF aStr = 'INV_CONST_COLOR' THEN EXIT (ALLEGRO_INVERSE_CONST_COLOR);
    RESULT := ALLEGRO_ONE
  END;



  FUNCTION TProg.StrToBlendOperation (aStr: STRING): ALLEGRO_BLEND_OPERATIONS;
  BEGIN
    IF aStr = 'ADD' THEN EXIT (ALLEGRO_ADD);
    IF aStr = 'SRC_MINUS_DEST' THEN EXIT (ALLEGRO_SRC_MINUS_DEST);
    IF aStr = 'DEST_MINUS_SRC' THEN EXIT (ALLEGRO_DEST_MINUS_SRC);
    RESULT := ALLEGRO_ADD
  END;



  FUNCTION TProg.makecol (r, g, b, a: INTEGER): ALLEGRO_COLOR;
  VAR
    af: REAL;
  BEGIN
  { Premultiply alpha. }
    af := a / 255;
    RESULT :=
      al_map_rgba_f ((r / 255) * af, (g / 255) * af, (b / 255) * af, af)
  END;



  PROCEDURE TProg.DrawBackground (x, y: INTEGER);
  VAR
    c: ARRAY [0..1] OF ALLEGRO_COLOR;
    i, j: INTEGER;
  BEGIN
    c[0] := al_map_rgba ($66, $66, $66, $FF);
    c[1] := al_map_rgba ($99, $99, $99, $FF);

    FOR i := 0 TO (320 DIV 16) - 1 DO
      FOR j := 0 TO (200 DIV 16) - 1 DO
	al_draw_filled_rectangle (
	  x + i * 16 + 16, y + j * 16,
	  x + i * 16 + 16, y + j * 16 + 16,
	  c[(i + j) AND 1]
	)
  END;



  PROCEDURE TProg.BlendingTest (aMemory: BOOLEAN);
  VAR
    Transparency, Color: ALLEGRO_COLOR;
    src, asrc, dst, adst: ALLEGRO_BLEND_MODE;
    op, aop: ALLEGRO_BLEND_OPERATIONS;
    rv, gv, bv, av: INTEGER;
  BEGIN
    Transparency := al_map_rgba_f (0, 0, 0, 0);
    op   := StrToBlendOperation (fOperations[4].SelectedItemText);
    aop  := StrToBlendOperation (fOperations[5].SelectedItemText);
    src  := StrToBlendMode (fOperations[0].SelectedItemText);
    asrc := StrToBlendMode (fOperations[1].SelectedItemText);
    dst  := StrToBlendMode (fOperations[2].SelectedItemText);
    adst := StrToBlendMode (fOperations[3].SelectedItemText);
    rv := fR[2].CurValue;
    gv := fG[2].CurValue;
    bv := fB[2].CurValue;
    av := fA[2].CurValue;
    Color := SELF.makecol (rv, gv, bv, av);

  { Initialize with destination. }
    al_clear_to_color (Transparency); { Just in case. }
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    SELF.DrawBitmap
      (fDestinationImage.SelectedItemText, 'original', aMemory, TRUE);
  { Now draw the blended source over it. }
    al_set_separate_blender (op, src, dst, aop, asrc, adst);
    al_set_blend_color (Color);
    SELF.DrawBitmap (
      fSourceImage.SelectedItemText, fDrawMode.SelectedItemText, aMemory, FALSE
    )
  END;



  PROCEDURE TProg.DrawSamples;
  VAR
    lState: ALLEGRO_STATE;
  BEGIN
    al_store_state
      (lState, ALLEGRO_STATE_TARGET_BITMAP OR ALLEGRO_STATE_BLENDER);
    TRY
    { Draw a background, in case our target bitmap will end up with alpha in
      it. }
      SELF.DrawBackground (40, 20);
      SELF.DrawBackground (400, 20);

    { Test standard blending. }
      al_set_target_bitmap (Target);
      SELF.BlendingTest (FALSE);

    { Test memory blending. }
      al_set_target_bitmap (TargetBmp);
      SELF.BlendingTest (TRUE);

    { Display results. }
      al_restore_state (lState);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_bitmap (Target, 40, 20, 0);
      al_draw_bitmap (TargetBmp, 400, 20, 0)
    FINALLY
      al_restore_state (lState)
    END
  END;



  PROCEDURE TProg.DrawBitmap
    (aStr, aHow: STRING; aMemory, aDestination: BOOLEAN);

    FUNCTION Contains (Haystack, Needle: STRING): BOOLEAN;
    BEGIN
      RESULT := Pos (UpCase (Needle), UpCase (Haystack)) > 0
    END;

  VAR
    i, rv, gv, bv, av, w, h: INTEGER;
    s: REAL;
    lColor: ALLEGRO_COLOR;
    lBmp: ALLEGRO_BITMAPptr;
  BEGIN
    IF aDestination THEN i := 1 ELSE i := 0;
    rv := fR[i].CurValue;
    gv := fG[i].CurValue;
    bv := fB[i].CurValue;
    av := fA[i].CurValue;
    lColor := makecol (rv, gv, bv, av);

    IF Contains (aStr, 'Mysha') THEN
    BEGIN
      IF aMemory THEN lBmp := MyshaBmp ELSE lBmp := Mysha
    END
    ELSE BEGIN
      IF aMemory THEN lBmp := AllegroBmp ELSE lBmp := Allegro
    END;

    IF aHow = 'original' THEN
    BEGIN
      IF aStr = 'Color' THEN
	al_draw_filled_rectangle (0, 0, 320, 200, lColor)
      ELSE IF Contains (aStr, 'tint') THEN
	al_draw_tinted_bitmap (lBmp, lColor, 0, 0, 0)
      ELSE
	al_draw_bitmap (lBmp, 0, 0, 0)
    END
    ELSE IF aHow = 'scaled' THEN
    BEGIN
      w := al_get_bitmap_width (lBmp);
      h := al_get_bitmap_height (lBmp);
      s := 200  / h * 0.9;

      IF aStr = 'Color' THEN
	al_draw_filled_rectangle (10, 10, 300, 180, lColor)
      ELSE IF Contains (aStr, 'tint') THEN
	al_draw_tinted_scaled_bitmap (
	  lBmp, lColor,
	  0, 0, w, h,
	  160 - w * s / 2, 100 - h * s / 2, w * s, h * s,
	  0
	)
      ELSE
	al_draw_scaled_bitmap (
	  lBmp,
	  0, 0, w, h,
	  160 - w * s / 2, 100 - h * s / 2, w * s, h * s,
	  0)
    END
    ELSE IF aHow = 'rotated' THEN
    BEGIN
      IF aStr = 'Color' THEN
	al_draw_filled_circle (160, 100, 100, lColor)
      ELSE IF Contains (aStr, 'tint') THEN
	al_draw_tinted_rotated_bitmap (
	  lBmp, lColor,
	  160, 100,
	  160, 100, ALLEGRO_TAU / 16,
	  0
	)
      ELSE
	al_draw_rotated_bitmap (
	  lBmp,
	  160, 100,
	  160, 100, ALLEGRO_TAU / 16,
	  0
	)
    END
  END;



  CONSTRUCTOR TProg.Create (aTheme: TTheme; aDisplay: ALLEGRO_DISPLAYptr);
  CONST
    ClrLbl: ARRAY [0..1] OF STRING = ('Color', 'Alpha');
  VAR
    Images: ARRAY [0..2] OF TList;
    Ndx: INTEGER;
  BEGIN
    INHERITED Create;
    fDialog := TDialog.CreateDialog (aTheme, aDisplay, 20, 40);
    fMemoryLabel := TLabel.CreateLabel ('Memory');
    fTextureLabel := TLabel.CreateLabel ('Texture');
    fSourceLabel := TLabel.CreateLabel ('Source', FALSE);
    fDestinationLabel := TLabel.CreateLabel ('Destination', FALSE);
    fSourceImage := TList.CreateList (0);
    fDestinationImage := TList.CreateList (1);
    fDrawMode := TList. CreateList (0);

    fDialog.Add (fMemoryLabel, 9, 0, 10, 2);
    fDialog.Add (fTextureLabel, 0, 0, 10, 2);
    fDialog.Add (fSourceLabel, 1, 15, 6, 2);
    fDialog.Add (fDestinationLabel, 7, 15, 6, 2);

    Images[0] := fSourceImage;
    Images[1] := fDestinationImage;
    Images[2] := fDrawMode;
    FOR Ndx := HIGH (Images) TO LOW (Images) DO
    BEGIN
      IF Ndx < 2 THEN
      BEGIN
	Images[Ndx].AppendItem ('Mysha');
	Images[Ndx].AppendItem ('Allegro');
	Images[Ndx].AppendItem ('Mysha (tinted)');
	Images[Ndx].AppendItem ('Allegro (tinted)');
	Images[Ndx].AppendItem ('Color')
      END
      ELSE BEGIN
	Images[Ndx].AppendItem ('original');
	Images[Ndx].AppendItem ('scaled');
	Images[Ndx].AppendItem ('rotated');
      END;
      fDialog.Add (Images[Ndx], 1 + Ndx * 6, 16, 4, 6)
    END;

    FOR Ndx := 0 TO 3 DO
    BEGIN
      fOperationLabel[Ndx] := TLabel.CreateLabel (ClrLbl[Ndx MOD 2], FALSE);
      fDialog.Add (fOperationLabel[Ndx], 1 + Ndx * 3, 23, 3, 2);
      fOperations[Ndx] := TList.CreateList;
      fOperations[Ndx].AppendItem ('ONE');
      fOperations[Ndx].AppendItem ('ZERO');
      fOperations[Ndx].AppendItem ('ALPHA');
      fOperations[Ndx].AppendItem ('INVERSE');
      fOperations[Ndx].AppendItem ('SRC_COLOR');
      fOperations[Ndx].AppendItem ('DEST_COLOR');
      fOperations[Ndx].AppendItem ('INV_SRC_COLOR');
      fOperations[Ndx].AppendItem ('INV_DEST_COLOR');
      fOperations[Ndx].AppendItem ('CONST_COLOR');
      fOperations[Ndx].AppendItem ('INV_CONST_COLOR');
      fDialog.Add (fOperations[Ndx], 1 + Ndx * 3, 24, 3, 10)
    END;

    FOR Ndx := 4 TO 5 DO
    BEGIN
      IF Ndx = 4 THEN
	fOperationLabel[Ndx] := TLabel.CreateLabel ('Blend op', FALSE)
      ELSE
	fOperationLabel[Ndx] := TLabel.CreateLabel ('Alpha op', FALSE);
      fDialog.Add (fOperationLabel[Ndx], 1 + Ndx * 3, 23, 3, 2);
      fOperations[Ndx] := TList.CreateList;
      fOperations[Ndx].AppendItem ('ADD');
      fOperations[Ndx].AppendItem ('SRC_MINUS_DEST');
      fOperations[Ndx].AppendItem ('DEST_MINUS_SRC');
      fDialog.Add (fOperations[Ndx], 1 + Ndx * 3, 24, 3, 10)
    END;

    fRgbaLabel[0] := TLabel.CreateLabel ('Source tint/color RGBA');
    fRgbaLabel[1] := TLabel.CreateLabel ('Dest tint/color RGBA');
    fRgbaLabel[2] := TLabel.CreateLabel ('Const color RGBA');
    fDialog.Add (fRgbaLabel[0], 1, 34, 5, 1);
    fDialog.Add (fRgbaLabel[1], 7, 34, 5, 1);
    fDialog.Add (fRgbaLabel[2], 13, 34, 5, 1);
    FOR Ndx := LOW (fR) TO HIGH (fR) DO
    BEGIN
      fR[Ndx] := THSlider.CreateSlider (255, 255);
      fG[Ndx] := THSlider.CreateSlider (255, 255);
      fB[Ndx] := THSlider.CreateSlider (255, 255);
      fA[Ndx] := THSlider.CreateSlider (255, 255);
      fDialog.Add (fR[Ndx], 1 + Ndx * 6, 35, 5, 1);
      fDialog.Add (fG[Ndx], 1 + Ndx * 6, 36, 5, 1);
      fDialog.Add (fB[Ndx], 1 + Ndx * 6, 37, 5, 1);
      fDialog.Add (fA[Ndx], 1 + Ndx * 6, 38, 5, 1)
    END
  END;



  DESTRUCTOR TProg.Destroy;
  BEGIN
    fDialog.Free;
    INHERITED Destroy
  END;



  PROCEDURE TProg.Run;
  BEGIN
    fDialog.Prepare;
    REPEAT
      IF fDialog.DrawRequested THEN
      BEGIN
	al_clear_to_color (al_map_rgb (128, 128, 128));
	SELF.DrawSamples;
	fDialog.Draw;
	al_flip_display
      END;
      fDialog.RunStep (TRUE)
    UNTIL fDialog.QuitRequested
  END;

VAR
  Display: ALLEGRO_DISPLAYptr;
  Font: ALLEGRO_FONTptr;
  Theme: TTheme;
  Prog: TProg;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  al_init_primitives_addon;
  al_install_keyboard;
  al_install_mouse;

  al_init_font_addon;
  al_init_image_addon;
  InitPlatformSpecific;

  al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
  Display := al_create_display (800, 600);
  IF Display = NIL THEN AbortExample ('Unable to create display');

  Font := al_load_font ('data/fixed_font.tga', 0, 0);
  IF Font = NIL THEN AbortExample ('Failed to load data/fixed_font.tga');
  Allegro := al_load_bitmap ('data/allegro.pcx');
  IF Allegro = NIL THEN AbortExample ('Failed to load data/allegro.pcx');
  Mysha := al_load_bitmap ('data/mysha256x256.png');
  IF Mysha = NIL THEN AbortExample ('Failed to load data/mysha256x256.png');

  Target := al_create_bitmap (320, 200);

  al_add_new_bitmap_flag (ALLEGRO_MEMORY_BITMAP);
  AllegroBmp := al_clone_bitmap (Allegro);
  MyshaBmp := al_clone_bitmap (Mysha);
  TargetBmp := al_clone_bitmap (Target);

  TRY
    Theme := TTheme.Create (Font);
    Prog := TProg.Create (Theme, Display);
    Prog.Run
  FINALLY
    Prog.Free;
    Theme.Free
  END;

  al_destroy_bitmap (Allegro); al_destroy_bitmap (AllegroBmp);
  al_destroy_bitmap (Mysha); al_destroy_bitmap (MyshaBmp);
  al_destroy_bitmap (Target); al_destroy_bitmap (TargetBmp);

  al_destroy_font (Font)
END.
