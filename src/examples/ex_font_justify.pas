PROGRAM ex_font_justify;
(*
 * Example program for the Allegro library.
 *
 * Test text justification routines.
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
  {$IF NOT DEFINED(FPC_DELPHI)}
    {$MODE DELPHI}
  {$ENDIF}
{ Windows manifest. }
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

USES
  alGUI, Common,
  allegro5, al5base, al5font, al5image, al5primitives, al5ttf;

CONST
  DEFAULT_TEXT = 'Lorem ipsum dolor sit amet';

TYPE
(* A widget that aligns text. *)
  TTextBox = CLASS (TWidget)
  PRIVATE
    fcx, fth, fx1, fx2, fdiff: INTEGER;
    fClrText, fClr1, fClr2: ALLEGRO_COLOR;
    fText: AL_STR;
  PUBLIC
  (* Initializes the widget. *)
    PROCEDURE Initialize; OVERRIDE;
  (* Draws the widget. *)
    PROCEDURE Draw; OVERRIDE;
  (* Handles sliders events. *)
    PROCEDURE OnWidthSliderChanges (Sender: TObject);
    PROCEDURE OnDiffSlideChanges (Sender: TObject);
  (* Handles text entry event. *)
    PROCEDURE OnTextChanges (Sender: TObject);
  END;



(* Encapsulates the example. *)
  TFontJustifyExample = CLASS (TDialog)
  PRIVATE
    fFont: ALLEGRO_FONTptr;
  PUBLIC
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Initializes the example. *)
    PROCEDURE Initialize; OVERRIDE;
  END;

(*
 * TTextBox
 ***************************************************************************)

(* Initializes the widget. *)
  PROCEDURE TTextBox.Initialize;
  BEGIN
    fText := DEFAULT_TEXT;
    fcx := al_get_bitmap_width (al_get_target_bitmap) DIV 2;
    fth := al_get_font_line_height (Dialog.TextFont);
    fClrText := al_map_rgb_f (0.1, 0.1, 0.1);
    fClr1 := al_map_rgb (0, 0, 255);
    fClr2 := al_map_rgb (0, 255, 0);

    fx1 := fcx - 200;
    fx2 := fcx + 200;
    fdiff := 100
  END;



(* Draws the widget. *)
  PROCEDURE TTextBox.Draw;
  BEGIN
   al_draw_justified_text (
     Dialog.TextFont, fClrText,
     fx1, fx2, 50, fdiff,
     ALLEGRO_ALIGN_INTEGER,
     fText
   );
   al_draw_rectangle (fx1, 50, fx2, 50 + fth, fClr1, 0);
   al_draw_line (
     fcx - fdiff / 2, 53 + fth,
     fcx + fdiff / 2, 53 + fth,
     fClr2, 0
   )
  END;



(* Handles sliders events. *)
  PROCEDURE TTextBox.OnWidthSliderChanges (Sender: TObject);
  BEGIN
    fx1 := fcx - TSlider (Sender).Value DIV 2;
    fx2 := fcx + TSlider (Sender).Value DIV 2
  END;

  PROCEDURE TTextBox.OnDiffSlideChanges (Sender: TObject);
  BEGIN
    fdiff := TSlider (Sender).Value
  END;



(* Handles text entry event. *)
  PROCEDURE TTextBox.OnTextChanges (Sender: TObject);
  BEGIN
    fText := TTextEntry (Sender).Text
  END;



(*
 * TFontJustifyExample
 ***************************************************************************)

(* Destructor. *)
  DESTRUCTOR TFontJustifyExample.Destroy;
  BEGIN
    IF fFont <> NIL THEN al_destroy_font (fFont);
  { fFontGUI is destroyed by TDialog. }
    INHERITED Destroy
  END;



(* Initializes the example. *)
  PROCEDURE TFontJustifyExample.Initialize;
  VAR
    Display: ALLEGRO_DISPLAYptr;
    lFontGUI: ALLEGRO_FONTptr;
    lTextBox: TTextBox;
    lSlider: TSlider;
    lEntry: TTextEntry;
  BEGIN
    al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
    Display := al_create_display (640, 480);
    IF Display = NIL THEN  AbortExample ('Error creating display.');

    fFont := al_load_font ('data/DejaVuSans.ttf', 24, 0);
    IF fFont = NIL THEN AbortExample ('Failed to load data/DejaVuSans.ttf');
{ TODO: Add a button to swap between both textfonts?
    fFont := al_load_font ('data/font.tga', 0, 0);
    IF fFont = NIL THEN AbortExample ('Failed to load data/font.tga');
}
    lFontGUI := al_load_font ('data/DejaVuSans.ttf', 14, 0);
    IF lFontGUI = NIL THEN AbortExample ('Failed to load data/DejaVuSans.ttf');
    SELF.TextFont := lFontGUI;
  { Creates the dialog. }
    lTextBox := TTextBox.Create;
    SELF.Add (lTextBox, 0, 0, 10, 10);

    SELF.Add (TLabel.CreateLabel ('Text'), 0, 10, 1, 1);
    lEntry := TTextEntry.CreateTextEntry (DEFAULT_TEXT);
    lEntry.OnTextChanges := lTextBox.OnTextChanges;
    SELF.Add (lEntry, 1, 10, 8, 1);

    SELF.Add (TLabel.CreateLabel ('Width'), 0, 12, 1, 1);
    lSlider := TSlider.CreateSlider (al_get_display_width (Display), oHorizontal);
    lSlider.OnChange := lTextBox.OnWidthSliderChanges;
    lSlider.Value := 400;
    SELF.Add (lSlider, 1, 12, 8, 1);

    SELF.Add (TLabel.CreateLabel ('Diff'), 0, 14, 1, 1);
    lSlider := TSlider.CreateSlider (al_get_display_width (Display), oHorizontal);
    lSlider.OnChange := lTextBox.OnDiffSlideChanges;
    lSlider.Value := 100;
    SELF.Add (lSlider, 1, 14, 8, 1);
    INHERITED Initialize
  END;

VAR
  Example: TFontJustifyExample;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  al_init_primitives_addon;
  al_install_keyboard;
  al_install_mouse;

  al_init_image_addon;
  al_init_font_addon;
  al_init_ttf_addon;
  InitPlatformSpecific;

  Example := TFontJustifyExample.Create (10, 20);
  Example.Initialize;
  Example.Run;
  Example.Free
END.
