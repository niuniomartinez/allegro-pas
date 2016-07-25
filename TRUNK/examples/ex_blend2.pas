PROGRAM ex_blend2;
(* Example program for the Allegro library.
 *
 * Compare software blending routines with hardware blending.
 *
 * Original by Peter Wang.
 *)

{$MODE DELPHI}

  USES
    Common, nihGUI,
    Allegro5, al5primitives, al5font, al5image,
    sysutils;

  TYPE
  (* Encapsulates the example program and data. *)
    TProg = CLASS (TObject)
    PRIVATE
      fDisplay: ALLEGRO_DISPLAYptr;
      fAllegro, fMysha, fTarget,
      fAllegroBmp, fMyshaBmp, fTargetBmp: ALLEGRO_BITMAPptr;
      fDialog: TDialog;

      fSourceImage, fDestinationImage, fDrawMode: TList;
      fOperations: ARRAY [0..5] OF TList;
      fR, fG, fB, fA: ARRAY [0..2] OF TSlider;

      FUNCTION MakeColor (CONST aR, aG, aB, aA: INTEGER): ALLEGRO_COLOR;
      FUNCTION StrToBlendMode (CONST aStr: STRING): ALLEGRO_BLEND_MODE;
      FUNCTION StrToBlendOperation (CONST aStr: STRING): ALLEGRO_BLEND_OPERATIONS;
      PROCEDURE BlendingTest (CONST Memory: BOOLEAN);
      PROCEDURE DrawBitmap (CONST str: STRING; CONST How: STRING;
                           CONST Memory, Destination: BOOLEAN);
      PROCEDURE DrawBackground (CONST X, Y: INTEGER);
      PROCEDURE DrawSamples;
    PUBLIC
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes. *)
      PROCEDURE Initialize;
    (* Execution. *)
      PROCEDURE Run;
    END;



  FUNCTION TProg.MakeColor (CONST aR, aG, aB, aA: INTEGER): ALLEGRO_COLOR;
  VAR
    rf, gf, bf, af: REAL;
  BEGIN
  { Premultiply alpha. }
    rf := aR / 255.0;
    gf := aG / 255.0;
    bf := aB / 255.0;
    af := aA / 255.0;
    RESULT := al_map_rgba_f (rf * af, gf * af, bf * af, af)
  END;



  FUNCTION TProg.StrToBlendMode (CONST aStr: STRING): ALLEGRO_BLEND_MODE;
  BEGIN
    IF aStr = 'ZERO' THEN
      EXIT (ALLEGRO_ZERO);
    IF aStr = 'ONE' THEN
      EXIT (ALLEGRO_ONE);
    IF aStr = 'SRC_COLOR' THEN
      EXIT (ALLEGRO_SRC_COLOR);
    IF aStr = 'DEST_COLOR' THEN
      EXIT (ALLEGRO_DEST_COLOR);
    IF aStr = 'INV_SRC_COLOR' THEN
      EXIT (ALLEGRO_INVERSE_SRC_COLOR);
    IF aStr = 'INV_DEST_COLOR' THEN
      EXIT (ALLEGRO_INVERSE_DEST_COLOR);
    IF aStr = 'ALPHA' THEN
      EXIT (ALLEGRO_ALPHA);
    IF aStr = 'INVERSE' THEN
      EXIT (ALLEGRO_INVERSE_ALPHA);
    IF aStr = 'CONST_COLOR' THEN
      EXIT (ALLEGRO_CONST_COLOR);
    IF aStr = 'INV_CONST_COLOR' THEN
      EXIT (ALLEGRO_INVERSE_CONST_COLOR);
    RESULT := ALLEGRO_ONE
  END;



  FUNCTION TProg.StrToBlendOperation (CONST aStr: STRING): ALLEGRO_BLEND_OPERATIONS;
  BEGIN
    IF aStr = 'ADD' THEN
      EXIT (ALLEGRO_ADD);
    IF aStr = 'SRC_MINUS_DEST' THEN
      EXIT (ALLEGRO_SRC_MINUS_DEST);
    IF aStr = 'DEST_MINUS_SRC' THEN
      EXIT (ALLEGRO_DEST_MINUS_SRC);
  END;



  PROCEDURE TProg.BlendingTest (CONST Memory: BOOLEAN);
  VAR
    Transparency, Color: ALLEGRO_COLOR;
    src, asrc, dst, adst: ALLEGRO_BLEND_MODE;
    op, aop: ALLEGRO_BLEND_OPERATIONS;
    rv, gv, bv, av: INTEGER;
  BEGIN
    Transparency := al_map_rgba_f (0, 0, 0, 0);
    op   := StrToBlendOperation (fOperations[4].SelectedText);
    aop  := StrToBlendOperation (fOperations[5].SelectedText);
    src  := StrToBlendMode (fOperations[0].SelectedText);
    asrc := StrToBlendMode (fOperations[1].SelectedText);
    dst  := StrToBlendMode (fOperations[2].SelectedText);
    adst := StrToBlendMode (fOperations[3].SelectedText);
    rv := fR[2].Value;
    gv := fG[2].Value;
    bv := fB[2].Value;
    av := fA[2].Value;
    Color := SELF.MakeColor (rv, gv, bv, av);

  { Initialize with destination. }
    al_clear_to_color (Transparency); { Just in case. }
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    SELF.DrawBitmap (
      fDestinationImage.SelectedText, 'original', Memory, TRUE
    );

  { Now draw the blended source over it. }
    al_set_separate_blender (op, src, dst, aop, asrc, adst);
    al_set_blend_color (Color);
    SELF.DrawBitmap (
      fSourceImage.SelectedText, fDrawMode.SelectedText, Memory, FALSE
    )
  END;



  PROCEDURE TProg.DrawBitmap (CONST str: STRING; CONST How: STRING;
                             CONST Memory, Destination: BOOLEAN);
  VAR
    i, rv, gv, bv, av, w, h: INTEGER;
    s: SINGLE;
    Color: ALLEGRO_COLOR;
    Bmp: ALLEGRO_BITMAPptr;
  BEGIN
    IF Destination THEN i := 1 ELSE i := 0;
    rv := fR[i].Value;
    gv := fG[i].Value;
    bv := fB[i].Value;
    av := fA[i].Value;
    Color := MakeColor (rv, gv, bv, av);

    IF Pos ('Mysha', str) > 0 THEN
    BEGIN
      IF Memory THEN
        Bmp := fMyshaBmp
      ELSE
        Bmp := fMysha;
    END
    ELSE BEGIN
      IF Memory THEN
        Bmp := fAllegroBmp
      ELSE
        Bmp := fAllegro;
    END;

    IF How = 'original' THEN
    BEGIN
      IF str = 'Color' THEN
        al_draw_filled_rectangle (0, 0, 320, 200, Color)
      ELSE IF Pos ('tint', str) > 0 THEN
         al_draw_tinted_bitmap (Bmp, Color, 0, 0, 0)
      ELSE
         al_draw_bitmap (Bmp, 0, 0, 0);
    END
    ELSE IF How = 'scaled' THEN
    BEGIN
      w := al_get_bitmap_width (Bmp);
      h := al_get_bitmap_height (Bmp);
      s := 200.0 / h * 0.9;

      IF str = 'Color' THEN
        al_draw_filled_rectangle (10, 10, 300, 180, Color)
      ELSE IF Pos ('tint', str) > 0 THEN
        al_draw_tinted_scaled_bitmap (
          Bmp, Color, 0, 0, w, h,
          160 - w * s / 2, 100 - h * s / 2, w * s, h * s, 0
        )
      ELSE
        al_draw_scaled_bitmap (
          Bmp, 0, 0, w, h,
          160 - w * s / 2, 100 - h * s / 2, w * s, h * s, 0
        )
    END
    ELSE IF how = 'rotated' THEN
    BEGIN
      IF str = 'Color' THEN
        al_draw_filled_circle (160, 100, 100, Color)
      ELSE IF Pos ('tint', str) > 0 THEN
        al_draw_tinted_rotated_bitmap
          (Bmp, Color, 160, 100, 160, 100, ALLEGRO_PI / 8, 0)
      ELSE
        al_draw_rotated_bitmap (Bmp, 160, 100, 160, 100, ALLEGRO_PI / 8, 0)
    END
  END;



  PROCEDURE TProg.DrawBackground (CONST X, Y: INTEGER);
  CONST
    MaxI = (320 DIV 16) - 1;
    MaxJ = (200 DIV 16) - 1;
  VAR
    c: ARRAY [0..1] OF ALLEGRO_COLOR;
    i, j: INTEGER;
  BEGIN
    c[0] := al_map_rgba ($66, $66, $66, $FF);
    c[1] := al_map_rgba ($99, $99, $99, $FF);

    FOR i := 0 TO MaxI DO
      FOR j := 0 TO MaxJ DO
        al_draw_filled_rectangle (
          x + i * 16, y + j * 16,
          x + i * 16 + 16, y + j * 16 + 16,
          c[(i + j) AND 1]
        )
  END;



  PROCEDURE TProg.DrawSamples;
  VAR
    State: ALLEGRO_STATE;
  BEGIN
    al_store_state(State, ALLEGRO_STATE_TARGET_BITMAP OR ALLEGRO_STATE_BLENDER);

  { Draw a background, in case our target bitmap will end up with
    alpha in it. }
    SELF.DrawBackground (40, 20);
    SELF.DrawBackground (400, 20);

  { Test standard blending. }
    al_set_target_bitmap (fTarget);
    BlendingTest (FALSE);

  { Test memory blending. }
    al_set_target_bitmap (fTargetBmp);
    BlendingTest (TRUE);

  { Display results. }
    al_restore_state (State);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    al_draw_bitmap (fTarget, 40, 20, 0);
    al_draw_bitmap (fTargetBmp, 400, 20, 0);

    al_restore_state (State)
  END;



(* Destructor. *)
  DESTRUCTOR TProg.Destroy;

    PROCEDURE DestroyBitmap (aBmp: ALLEGRO_BITMAPptr); INLINE;
    BEGIN
      IF aBmp <> NIL THEN al_destroy_bitmap (aBmp)
    END;

  BEGIN
    FreeAndNil (fDialog);
    DestroyBitmap (fAllegro); DestroyBitmap (fAllegroBmp);
    DestroyBitmap (fMysha);   DestroyBitmap (fMyshaBmp);
    DestroyBitmap (fTarget);  DestroyBitmap (fTargetBmp);
    IF fDisplay <> NIL THEN al_destroy_display (fDisplay);
    INHERITED Destroy
  END;



(* Initializes example. *)
  PROCEDURE TProg.Initialize;

    PROCEDURE FillItemsImageList (aItemList: TList);
    CONST
      ImageNames: ARRAY [0..4] OF STRING = (
        'Mysha', 'Allegro', 'Mysha (tinted)', 'Allegro (tinted)', 'Color'
      );
    VAR
      Ndx: INTEGER;
    BEGIN
      FOR Ndx := LOW (ImageNames) TO HIGH (ImageNames) DO
        aItemList.Items.Add (ImageNames[Ndx])
    END;

  VAR
    Ndx: INTEGER;
  BEGIN
    IF NOT al_init THEN AbortExample ('Could not init Allegro.');
    al_init_primitives_addon;
    al_install_keyboard;
    al_install_mouse;

    al_init_font_addon;
    al_init_image_addon;
    InitPlatformSpecific;

    al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
    fDisplay := al_create_display (800, 600);
    IF fDisplay = NIL THEN AbortExample ('Unable to create display.');

    fAllegro := al_load_bitmap ('data/allegro.pcx');
    IF fAllegro = NIL THEN AbortExample ('Failed to load data/allegro.pcx');
    fMysha := al_load_bitmap ('data/mysha256x256.png');
    IF fMysha = NIL THEN AbortExample ('Failed to load data/mysha256x256.pcx');

    fTarget := al_create_bitmap (320, 200);

    al_add_new_bitmap_flag (ALLEGRO_MEMORY_BITMAP);
    fAllegroBmp := al_clone_bitmap (fAllegro);
    fMyshaBmp := al_clone_bitmap (fMysha);
    fTargetBmp := al_clone_bitmap (fTarget);
  { Create the dialog. }
    fDialog := TDialog.Create (fDisplay, 20, 40);
    IF fDialog.Font = NIL THEN
      AbortExample ('Failed to load data/fixed_font.tga');
    fDialog.Add (TLabel.CreateLabel ('Memory', TRUE), 9, 0, 10, 2);
    fDialog.Add (TLabel.CreateLabel ('Texture', TRUE), 0, 0, 10, 2);
    fDialog.Add (TLabel.CreateLabel ('Source', FALSE), 1, 15, 6, 2);
    fDialog.Add (TLabel.CreateLabel ('Destination', FALSE), 7, 15, 6, 2);

    fSourceImage := TList.Create;
    FillItemsImageList (fSourceImage);
    fDialog.Add (fSourceImage, 1, 16, 4, 6);
    fDestinationImage := TList.Create;
    FillItemsImageList (fDestinationImage);
    fDialog.Add (fDestinationImage, 7, 16, 4, 6);
    fDestinationImage.SelectedItem := 1;
    fDrawMode := TList.Create;
    fDrawMode.Items.Add ('original');
    fDrawMode.Items.Add ('scaled');
    fDrawMode.Items.Add ('rotated');
    fDialog.Add (fDrawMode,  13, 16, 4, 6);

    FOR Ndx := 0 TO 3 DO
    BEGIN
      IF Ndx MOD 2 = 0 THEN
        fDialog.Add (TLabel.CreateLabel ('Color'), 1 + Ndx * 3, 23, 3, 2)
      ELSE
        fDialog.Add (TLabel.CreateLabel ('Alpha'), 1 + Ndx * 3, 23, 3, 2);

      fOperations[Ndx] := TList.Create;
      WITH fOperations[Ndx] DO
      BEGIN
        Items.Add ('ONE');
        Items.Add ('ZERO');
        Items.Add ('ALPHA');
        Items.Add ('INVERSE');
        Items.Add ('SRC_COLOR');
        Items.Add ('DEST_COLOR');
        Items.Add ('INV_SRC_COLOR');
        Items.Add ('INV_DEST_COLOR');
        Items.Add ('CONST_COLOR');
        Items.Add ('INV_CONST_COLOR')
      END;
      fDialog.Add (fOperations[Ndx], 1 + Ndx * 3, 24, 3, 10);
    END;
  { Select blender mode to paste beautiful composition. }
    fOperations[0].SelectedItem := 0; { ONE }
    fOperations[1].SelectedItem := 4; { SRC_COLOR }
    fOperations[2].SelectedItem := 3; { INVERSE }
    fOperations[3].SelectedItem := 5; { DEST_COLOR }

    FOR Ndx := 4 TO 5 DO
    BEGIN
      IF Ndx = 4 THEN
        fDialog.Add
          (TLabel.CreateLabel ('Blend op'), 1 + Ndx * 3, 23, 3, 2)
      ELSE
        fDialog.Add
          (TLabel.CreateLabel ('Alpha op'), 1 + Ndx * 3, 23, 3, 2);

      fOperations[Ndx] := TList.Create;
      WITH fOperations[Ndx] DO
      BEGIN
        Items.Add ('ADD');
        Items.Add ('SRC_MINUS_DEST');
        Items.Add ('DEST_MINUS_SRC')
      END;
      fDialog.Add (fOperations[Ndx], 1 + Ndx * 3, 24, 3, 6);
    END;

    fDialog.Add (TLabel.CreateLabel ('Source tint/color RGBA'), 1, 34, 5, 1);
    fDialog.Add (TLabel.CreateLabel ('Dest tint/color RGBA'), 7, 34, 5, 1);
    fDialog.Add (TLabel.CreateLabel ('Cons tint/color RGBA'), 13, 34, 5, 1);

    FOR Ndx := LOW (fR) TO HIGH (fR) DO
    BEGIN
      fR[Ndx] := TSlider.CreateSlider (255, wdHorizontal); fR[Ndx].Value := 255;
      fG[Ndx] := TSlider.CreateSlider (255, wdHorizontal); fG[Ndx].Value := 255;
      fB[Ndx] := TSlider.CreateSlider (255, wdHorizontal); fB[Ndx].Value := 255;
      fA[Ndx] := TSlider.CreateSlider (255, wdHorizontal); fA[Ndx].Value := 255;

      fDialog.Add (fR[Ndx], 1 + Ndx * 6, 35, 5, 1);
      fDialog.Add (fG[Ndx], 1 + Ndx * 6, 36, 5, 1);
      fDialog.Add (fB[Ndx], 1 + Ndx * 6, 37, 5, 1);
      fDialog.Add (fA[Ndx], 1 + Ndx * 6, 38, 5, 1)
    END
  END;



(* Executes the example. *)
  PROCEDURE TProg.Run;
  BEGIN
    fDialog.Initialize;
    REPEAT
      IF fDialog.DrawRequested THEN
      BEGIN
        al_clear_to_color (al_map_rgb (128, 128, 128));
        SELF.DrawSamples;
        fDialog.Draw;
        al_flip_display
      END;
      fDialog.RunStep (TRUE)
    UNTIL fDialog.Terminated
  END;

VAR
  TheExample: TProg;
BEGIN
  TheExample := TProg.Create;
  TRY
    TheExample.Initialize;
    TheExample.Run
  FINALLY
    TheExample.Free
  END
END.
