program ex_13_color;
(*
 * Demonstrates some of the conversion functions in the color addon.
 *
 * This example uses "algui", a unit that defines a very simple GUI.  Delphi and
 * Lazarus users would feel familiar with some concepts.
 *
 * From a demo for the Allegro library, by Elias Pschernig.
 *)
(*
  Copyright (c) 2019-2024 Guillermo Martínez J.

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
   algui, Common,
   allegro5, al5base, al5color, al5font, al5primitives, al5strings, al5ttf;

  const
    SlidersCount = 19;
    SliderMax = 1000;
    SliderStep = 100;
  (* To identify color space. *)
    csRGB = 0;
    csHSV = 1;
    csHSL = 2;
    csYUV = 3;
    csCMYK= 4;
    csLCH = 5;
  (* Initial color. *)
    icR = SliderMax;
    icG = Trunc (SliderMax * 0.6);
    icB = SliderMax;

  type
  (* A new widget to display a color box. *)
    TColorBoxWidget = class (TWidget)
    private
      fColor: ALLEGRO_COLOR;
    public
    (* Render the widget. *)
      procedure Draw; override;

    (* Color to display. *)
      property Color: ALLEGRO_COLOR read fColor write fColor;
    end;



  (* Extends dialog to build example. *)
    TColorExample = class (TDialog)
    private
      fSliders: array [0..SlidersCount-1] of TSlider;
      fChannelLbl, fValueLbl: array [0..SlidersCount-1] of TLabel;
      fHexLbl, fNameLbl: TLabel;
      fColorBox: TColorBoxWidget;
    { Needs this to help deal with some rounding errors.  See UpdateSliders. }
      fColumnChanged: Integer;

      procedure OnSliderChange (aSender: TObject);
    public
    (* Initialize the example. *)
      procedure Initialize; override;
    (* Update sliders with the ColorBox colour. *)
      procedure UpdateSliders;
    end;



(*
 * TColorBoxWidget
 ************************************************************************)

  procedure TColorBoxWidget.Draw;
  begin
    al_draw_filled_rectangle (
      Self.X + 0.5, Self.Y + 0.5,
      Self.X + Self.Width + 0.5, Self.Y + Self.Height + 0.5,
      fColor
    );
    al_draw_rectangle (
      Self.X + 0.5, Self.Y + 0.5,
      Self.X + Self.Width + 0.5, Self.Y + Self.Height + 0.5,
      al_map_rgb (0, 0, 0), 2
    )
  end;



(*
 * TColorExample
 ************************************************************************)

  procedure TColorExample.OnSliderChange (aSender: TObject);
  var
    lWidget: TWidget absolute aSender;
  begin
    fColumnChanged := lWidget.Tag;
    case fColumnChanged of
    csRGB:
      fColorBox.Color := al_map_rgb_f (
        fSliders[0].Value / SliderMax,
        fSliders[1].Value / SliderMax,
        fSliders[2].Value / SliderMax
      );
    csHSV:
      fColorBox.Color := al_color_hsv (
        (fSliders[3].value / SliderMax) * 360,
        fSliders[4].Value / SliderMax,
        fSliders[5].Value / SliderMax
      );
    csHSL:
      fColorBox.Color := al_color_hsl (
        (fSliders[6].value / SliderMax) * 360,
        fSliders[7].Value / SliderMax,
        fSliders[8].Value / SliderMax
      );
    csYUV:
      fColorBox.Color := al_color_yuv (
        fSliders[9].value / SliderMax,
        fSliders[10].Value / SliderMax,
        fSliders[11].Value / SliderMax
      );
    csCMYK:
      fColorBox.Color := al_color_cmyk (
        fSliders[12].value / SliderMax,
        fSliders[13].Value / SliderMax,
        fSliders[14].Value / SliderMax,
        fSliders[15].Value / SliderMax
      );
    csLCH:
      fColorBox.Color := al_color_lch (
        fSliders[16].value / SliderMax,
        fSliders[17].Value / SliderMax,
        fSliders[18].Value / SliderMax
      );
    end;
    Self.UpdateSliders
  end;



  procedure TColorExample.Initialize;
  const
    SliderX = 48; SliderY = 8; SliderW = 16; SliderH = 424;
    LabelOffset = 7;
    ChannelY = SliderH + 8;
    ValueY = ChannelY + 16;
    HexX = 8; HexY = wHeight - 132;
    NameX = 8; NameY = wHeight - 116;
    ColorX = 0; ColorY = wHeight - 100; ColorW = wWidth; ColorH = 100;
    Names: array [0..SlidersCount-1] of String = (
    'R', 'G', 'B',
    'H', 'S', 'V',
    'H', 'S', 'L',
    'Y', 'U', 'V',
    'C', 'M', 'Y', 'K',
    'L', 'C', 'H'
  );

    function GetXPos (aNdx: Integer): Integer; inline;
    var
      lColumn: Integer;
    begin
      if aNdx < 12 then
        lColumn := aNdx div 3
      else if aNdx < 16 then
        lColumn := 4
      else
        lColumn := 5;
      Result := SliderX + aNdx * 32 + lColumn * 16
    end;

    procedure InitDialog;
    begin
      if Self.Terminated then Exit;
      Self.Title := 'Allegro''s color example';
      if not al_init_ttf_addon then
      begin
        Self.ShowErrorMessage ('Could not initialize TTF fonts.');
        Self.Terminate;
        Exit
      end;
      Self.TextFont := al_load_font ('data/DejaVuSans.ttf', 12, 0);
      if not Assigned (Self.TextFont) then
      begin
        Self.ShowErrorMessage ('Failed to load data/DejaVuSans.ttf');
        Self.Terminate;
        Exit
      end
    end;

    procedure CreateWidgets;

      function GetColumn (aNdx: Integer): Integer; inline;
      begin
        if aNdx < 15 then
          Result := aNdx div 3
        else if aNdx < 16 then
          Result := csCMYK
        else
          Result := csLCH
      end;

    var
      lNdx, lPosX: Integer;
    begin
      for lNdx := Low (fSliders) to High (fSliders) do
      begin
        lPosX := GetXPos (lNdx);
      { Add slider. }
        fSliders[lNdx] := TSlider.CreateSlider (
          oVertical,
          lPosX, SliderY, SliderW, SliderH,
          0, SliderMax, SliderStep
        );
        fSliders[lNdx].Inverted := True;
        fSliders[lNdx].Tag := GetColumn (lNdx);
        fSliders[lNdx].OnChange := Self.OnSliderChange;
        Self.AppendWidget (fSliders[lNdx]);
      { Add labels. }
        Inc (lPosX, LabelOffset);
        fChannelLbl[lNdx] := TLabel.CreateLabel (
          lPosX, ChannelY,
          Names[lNdx], ALLEGRO_ALIGN_CENTRE
        );
        Self.AppendWidget (fChannelLbl[lNdx]);
        fValueLbl[lNdx] := TLabel.CreateLabel (
          lPosX, ValueY,
          '0', ALLEGRO_ALIGN_CENTRE
        );
        Self.AppendWidget (fValueLbl[lNdx]);
      end;
    { Additional labels. }
      fHexLbl := TLabel.CreateLabel (HexX, HexY, '');
      Self.AppendWidget (fHexLbl);
      fNameLbl := TLabel.CreateLabel (NameX, NameY, '');
      Self.AppendWidget (fNameLbl);
    { Color box. }
      fColorBox := TColorBoxWidget.Create (ColorX, ColorY, ColorW, ColorH);
      Self.AppendWidget (fColorBox);
    { Set initial color. }
      fSliders[0].Value := icR;
      fSliders[1].Value := icG;
      fSliders[2].Value := icB
    end;

  begin
    inherited Initialize;
    InitDialog;
    CreateWidgets
  end;



  procedure TColorExample.UpdateSliders;

    procedure SetSliders (
      const aNdx: Integer;
      aV1, aV2, aV3: AL_FLOAT;
      aChanged: Boolean
    );
    begin
      if not aChanged then
      begin
        fSliders[aNdx + 0].Value := Trunc (aV1 * SliderMax);
        fSliders[aNdx + 1].Value := Trunc (aV2 * SliderMax);
        fSliders[aNdx + 2].Value := Trunc (aV3 * SliderMax)
      end;
      fValueLbl[aNdx + 0].Caption := al_str_format ('%f', [aV1]);
      fValueLbl[aNdx + 1].Caption := al_str_format ('%f', [aV2]);
      fValueLbl[aNdx + 2].Caption := al_str_format ('%f', [aV3])
    end;

    procedure SetSliders360 (
      const aNdx: Integer;
      aV1, aV2, aV3: AL_FLOAT;
      aChanged: Boolean
    );
    begin
      if not aChanged then
      begin
        fSliders[aNdx + 0].Value := Trunc ((aV1 / 360) * SliderMax);
        fSliders[aNdx + 1].Value := Trunc (aV2 * SliderMax);
        fSliders[aNdx + 2].Value := Trunc (aV3 * SliderMax)
      end;
      fValueLbl[aNdx + 0].Caption := al_str_format ('%d', [Trunc (aV1)]);
      fValueLbl[aNdx + 1].Caption := al_str_format ('%f', [aV2]);
      fValueLbl[aNdx + 2].Caption := al_str_format ('%f', [aV3])
    end;

  var
    lNdx: Integer;
    lR, lG, lB,
    v1, v2, v3, v4: AL_FLOAT;
  begin
  { Get color components. }
    al_unmap_rgb_f (fColorBox.Color, lR, lG, lB);
  { For some reason, LCH space makes things go out RGB space. }
    lR := Clamp (0, lR, 1);
    lG := Clamp (0, lG, 1);
    lB := Clamp (0, lB, 1);
  { Deactivate slider events. }
    for lNdx := Low (fSliders) to High (fSliders) do
      fSliders[lNdx].OnChange := Nil;
  { Update sliders. }
    SetSliders (0, lR, lG, lB, fColumnChanged = csRGB);
    al_color_rgb_to_hsv (lR, lG, lB, v1, v2, v3);
    SetSliders360 (3, v1, v2, v3, fColumnChanged = csHSV);
    al_color_rgb_to_hsl (lR, lG, lB, v1, v2, v3);
    SetSliders360 (6, v1, v2, v3, fColumnChanged = csHSL);
    al_color_rgb_to_yuv (lR, lG, lB, v1, v2, v3);
    SetSliders (9, v1, v2, v3, fColumnChanged = csYUV);
    al_color_rgb_to_cmyk (lR, lG, lB, v1, v2, v3, v4);
    SetSliders (12, v1, v2, v3, fColumnChanged = csCMYK);
    if fColumnChanged <> csCMYK then
      fSliders[15].Value := Trunc (V4 * SliderMax); { K }
    fValueLbl[15].Caption := al_str_format ('%d', [fSliders[15].Value]);
    al_color_rgb_to_lch (lR, lG, lB, v1, v2, v3);
    SetSliders (16, v1, v2, v3, fColumnChanged = csLCH);
  { Reactivate slider events. }
    for lNdx := Low (fSliders) to High (fSliders) do
      fSliders[lNdx].OnChange := Self.OnSliderChange;
  { Update HEX and name labels. }
    fHexLbl.Caption := al_str_format (
      '#%.2x%.2x%.2x',
      [Trunc (lR * 255), Trunc (lG * 255), Trunc (lB * 255)]
    );
    fNameLbl.Caption := al_color_rgb_to_name (lR, lG, lB)
  end;

var
  ColorExample: TColorExample;
begin
  ColorExample := TColorExample.Create;
  try
    ColorExample.Initialize;
    ColorExample.Run
  finally
    ColorExample.Free
  end
end.
