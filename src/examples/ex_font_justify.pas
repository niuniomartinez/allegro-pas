program ex_font_justify;
(*
 * Example program for the Allegro library.
 *
 * Test text justification routines.
 *
 * Original by Peter Wang.
 *)
(*
  Copyright (c) 2012-2023 Guillermo Martínez J.

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

uses
  alGUI, Common,
  allegro5, al5font, al5image, al5primitives, al5strings, al5ttf;

const
  DEFAULT_TEXT = 'Lorem ipsum dolor sit amet';

type
(* A widget that aligns text. *)
  TTextBox = class (TWidget)
  private
    fcx, fth, fx1, fx2, fdiff: Integer;
    fClrText, fClr1, fClr2: ALLEGRO_COLOR;
    fText: String;
  public
  (* Initializes the widget. *)
    procedure Initialize; override;
  (* Draws the widget. *)
    procedure Draw; override;
  (* Handles sliders events. *)
    procedure OnWidthSliderChanges (Sender: TObject);
    procedure OnDiffSlideChanges (Sender: TObject);
  (* Handles text entry event. *)
    procedure OnTextChanges (Sender: TObject);
  end;



(* Encapsulates the example. *)
  TFontJustifyExample = class (TDialog)
  private
    fFont: ALLEGRO_FONTptr;
  public
  (* Destructor. *)
    destructor Destroy; override;
  (* Initializes the example. *)
    procedure Initialize; override;
  end;

(*
 * TTextBox
 ***************************************************************************)

(* Initializes the widget. *)
  procedure TTextBox.Initialize;
  begin
    fText := DEFAULT_TEXT;
    fcx := al_get_bitmap_width (al_get_target_bitmap) div 2;
    fth := al_get_font_line_height (Dialog.Theme.TextFont);
    fClrText := al_map_rgb_f (0.1, 0.1, 0.1);
    fClr1 := al_map_rgb (0, 0, 255);
    fClr2 := al_map_rgb (0, 255, 0);

    fx1 := fcx - 200;
    fx2 := fcx + 200;
    fdiff := 100
  end;



(* Draws the widget. *)
  procedure TTextBox.Draw;
  begin
   al_draw_justified_text (
     Dialog.Theme.TextFont, fClrText,
     fx1, fx2, 50, fdiff,
     ALLEGRO_ALIGN_INTEGER,
     al_string_to_str (fText)
   );
   al_draw_rectangle (fx1, 50, fx2, 50 + fth, fClr1, 0);
   al_draw_line (
     fcx - fdiff / 2, 53 + fth,
     fcx + fdiff / 2, 53 + fth,
     fClr2, 0
   )
  end;



(* Handles sliders events. *)
  procedure TTextBox.OnWidthSliderChanges (Sender: TObject);
  begin
    fx1 := fcx - TSlider (Sender).Value div 2;
    fx2 := fcx + TSlider (Sender).Value div 2
  end;

  procedure TTextBox.OnDiffSlideChanges (Sender: TObject);
  begin
    fdiff := TSlider (Sender).Value
  end;



(* Handles text entry event. *)
  procedure TTextBox.OnTextChanges (Sender: TObject);
  begin
    fText := TTextEntry (Sender).Text
  end;



(*
 * TFontJustifyExample
 ***************************************************************************)

(* Destructor. *)
  destructor TFontJustifyExample.Destroy;
  begin
    if fFont <> Nil then al_destroy_font (fFont);
  { fFontGUI is destroyed by TDialog. }
    inherited Destroy
  end;



(* Initializes the example. *)
  procedure TFontJustifyExample.Initialize;
  var
    Display: ALLEGRO_DISPLAYptr;
    lTheme: TTheme;
    lTextBox: TTextBox;
    lSlider: TSlider;
    lEntry: TTextEntry;
  begin
    al_set_new_display_flags (ALLEGRO_GENERATE_EXPOSE_EVENTS);
    Display := al_create_display (640, 480);
    if Display = Nil then  AbortExample ('Error creating display.');

    fFont := al_load_font ('data/DejaVuSans.ttf', 24, 0);
    if fFont = Nil then AbortExample ('Failed to load data/DejaVuSans.ttf');
{ TODO: Add a button to swap between both textfonts?
    fFont := al_load_font ('data/font.tga', 0, 0);
    if fFont = Nil then AbortExample ('Failed to load data/font.tga');
}
    lTheme := Self.Theme;
    lTheme.TextFont := al_load_font ('data/DejaVuSans.ttf', 14, 0);
    if lTheme.TextFont = Nil then AbortExample ('Failed to load data/DejaVuSans.ttf');
    Self.Theme := lTheme;
  { Creates the dialog. }
    lTextBox := TTextBox.Create;
    Self.Widgets.Add (lTextBox, 0, 0, 10, 10);

    Self.Widgets.Add (TLabel.CreateLabel ('Text'), 0, 10, 1, 1);
    lEntry := TTextEntry.CreateTextEntry (DEFAULT_TEXT);
    lEntry.OnTextChanges := lTextBox.OnTextChanges;
    Self.Widgets.Add (lEntry, 1, 10, 8, 1);

    Self.Widgets.Add (TLabel.CreateLabel ('Width'), 0, 12, 1, 1);
    lSlider := TSlider.CreateSlider (al_get_display_width (Display), oHorizontal);
    lSlider.OnChange := lTextBox.OnWidthSliderChanges;
    lSlider.Value := 400;
    Self.Widgets.Add (lSlider, 1, 12, 8, 1);

    Self.Widgets.Add (TLabel.CreateLabel ('Diff'), 0, 14, 1, 1);
    lSlider := TSlider.CreateSlider (al_get_display_width (Display), oHorizontal);
    lSlider.OnChange := lTextBox.OnDiffSlideChanges;
    lSlider.Value := 100;
    Self.Widgets.Add (lSlider, 1, 14, 8, 1);
    inherited Initialize
  end;

var
  Example: TFontJustifyExample;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

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
end.
