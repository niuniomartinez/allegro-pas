PROGRAM haiku;
(*
 *    Haiku - A Musical Instrument, by Mark Oates.
 *
 *    Allegro example version by Peter Wang.
 *
 *    Pascal translation by Guillermo "Ñuño" Martínez.
 *
 *    It demonstrates use of the audio functions, and other things besides.
 *)

(* This version leaves out some things from Mark's original version:
 * the nice title sequence, text labels and mouse cursors.
 *)

(* This is a translation to Pascal from the original C code.  Unfortunatelly
   I (Ñuño) didn't do it correctly and an animation (earth) doesn't work as
   spected.
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
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
  {$MODE DELPHI}
  {$MODESWITCH ADVANCEDrecordS+}
{$ENDIF}


uses
   Common,
   Allegro5,
   al5base, al5audio, al5acodec, al5image, al5strings;

const
   TYPE_EARTH = 0;
   TYPE_WIND  = 1;
   TYPE_WATER = 2;
   TYPE_FIRE  = 3;
   NUM_TYPES  = 4;
   TYPE_NONE  = NUM_TYPES;

   IMG_EARTH        = TYPE_EARTH;
   IMG_WIND         = TYPE_WIND;
   IMG_WATER        = TYPE_WATER;
   IMG_FIRE         = TYPE_FIRE;
   IMG_BLACK        = 4;
   IMG_DROPSHADOW   = 5;
   IMG_GLOW         = 6;
   IMG_GLOW_OVERLAY = 7;
   IMG_AIR_EFFECT   = 8;
   IMG_WATER_DROPS  = 9;
   IMG_FLAME        = 10;
   IMG_MAIN_FLAME   = 11;
   IMG_MAX          = 12;

   MAX_ANIMS = 10;

type
   TInterp = (
      INTERP_LINEAR,
      INTERP_FAST,
      INTERP_DOUBLE_FAST,
      INTERP_SLOW,
      INTERP_DOUBLE_SLOW,
      INTERP_SLOW_IN_OUT,
      INTERP_BOUNCE
   );

   PAnim = ^TAnim;
   TAnim = record
      lVal: PSingle;   { Nil if unused. }
      StartVal,
      EndVal:     Single;
      Func: TInterp;
      StartTime, EndTime: Single;
   end;

   PSprite = ^TSprite;
   TSprite = record
      Image: LongWord; { IMG_ }
      X, ScaleX, AlignX: Single;
      Y, ScaleY, AlignY: Single;
      Angle: Single;
      R, G, B: Single;
      Opacity: Single;
      Anims: array [0..MAX_ANIMS - 1] of TAnim; { Keep it simple. }
   end;

   PToken = ^TToken;
   TToken = record
      TheType: LongWord; { type_ }
      X, Y: Single;
      Pitch: LongInt; { [0, NUM_PITCH] }
      Bot, Top: TSprite;
   end;

   PFlair = ^TFlair;
   TFlair = record
     Next: PFlair;
     EndTime: Single;
     Sprite: TSprite;
   end;



(****************************************************************************
 * Globals                                                                  *
 ****************************************************************************)

const
   NUM_PITCH   = 8;
   TOKENS_X    = 16;
   TOKENS_Y    = NUM_PITCH;
   NUM_TOKENS  = TOKENS_X * TOKENS_Y;

var
   Display: ALLEGRO_DISPLAYptr;
   RefreshTimer, PlaybackTimer: ALLEGRO_TIMERptr;

   Images: array [0..IMG_MAX - 1] of ALLEGRO_BITMAPptr;
   ElementSamples: array [0..NUM_TYPES - 1] of array [0..NUM_PITCH - 1] of ALLEGRO_SAMPLEptr;
   SelectSample: ALLEGRO_SAMPLEptr;

   Tokens: array [0..NUM_TOKENS - 1] of TToken;
   Buttons: array [0..NUM_TYPES - 1] of TToken;
   Glow, GlowOverlay: TSprite;
   GlowColor: array [0..NUM_TYPES - 1] of ALLEGRO_COLOR;
   Flairs: PFlair = Nil;
   HoverToken: PToken = Nil;
   SelectedButton: PToken = Nil;
   PlaybackColumn: LongInt = 0;

const
   ScreenW = 1024;
   ScreenH = 600;
   GameBoardX = 100.0;
   TokenSize = 64;
   TokenScale = 0.8;
   ButtonSize = 64;
   ButtonUnselScale = 0.8;
   ButtonSelScale = 1.1;
   DropshadowUnselScale = 0.6;
   DropshadowSelScale = 0.9;
   RefreshRate = 60.0;
   PlaybackPeriod = 2.7333;

   HAIKU_DATA = 'data/haiku/';



(****************************************************************************
 * Init                                                                     *
 ****************************************************************************)

   procedure LoadImages;
   var
      Ndx: Integer;
   begin
      Images[IMG_EARTH]       := al_load_bitmap (HAIKU_DATA+'earth4.png');
      Images[IMG_WIND]        := al_load_bitmap (HAIKU_DATA+'wind3.png');
      Images[IMG_WATER]       := al_load_bitmap (HAIKU_DATA+'water.png');
      Images[IMG_FIRE]        := al_load_bitmap (HAIKU_DATA+'fire.png');
      Images[IMG_BLACK]       := al_load_bitmap (HAIKU_DATA+'black_bead_opaque_A.png');
      Images[IMG_DROPSHADOW]  := al_load_bitmap (HAIKU_DATA+'dropshadow.png');
      Images[IMG_AIR_EFFECT]  := al_load_bitmap (HAIKU_DATA+'air_effect.png');
      Images[IMG_WATER_DROPS] := al_load_bitmap (HAIKU_DATA+'water_droplets.png');
      Images[IMG_FLAME]       := al_load_bitmap (HAIKU_DATA+'flame2.png');
      Images[IMG_MAIN_FLAME]  := al_load_bitmap (HAIKU_DATA+'main_flame2.png');
      Images[IMG_GLOW]        := al_load_bitmap (HAIKU_DATA+'healthy_glow.png');
      Images[IMG_GLOW_OVERLAY]:= al_load_bitmap (HAIKU_DATA+'overlay_pretty.png');

      for Ndx := Low (Images) to High (Images) do
         if Images[Ndx] = Nil then
            AbortExample ('Error loading image.');
   end;



   procedure LoadSamples;
   const
      Base: array [0..NUM_TYPES - 1] of AL_STR = (
         'earth', 'air', 'water', 'fire'
      );
   var
      Name: AL_STR;
      t, p: LongInt;
   begin
      for t := 0 to NUM_TYPES - 1 do
      begin
         for p := 0 to NUM_PITCH - 1 do
         begin
            Name := HAIKU_DATA + al_str_format ('%s_%d.ogg', [Base[t], p]);
            ElementSamples[t][p] := al_load_sample (Name);
            if ElementSamples[t][p] = Nil then
               AbortExample ('Error loading "' + Name + '".');
         end;
      end;
      SelectSample := al_load_sample (HAIKU_DATA+'select.ogg');
      if SelectSample = Nil then
         AbortExample ('Error loading select.ogg.');
   end;



   procedure InitSprite (var Spr: TSprite; Image: Integer; x, y, Scale, Opacity: Single);
   var
      Ndx: Integer;
   begin
      Spr.Image := Image;
      Spr.X := x;
      Spr.Y := y;
      Spr.ScaleX := Scale;
      Spr.ScaleY := Scale;
      Spr.AlignX := 0.5;
      Spr.AlignY := 0.5;
      Spr.Angle := 0.0;
      Spr.R := 1.0;
      Spr.G := 1.0;
      Spr.B := 1.0;
      Spr.Opacity := Opacity;
      for Ndx := Low (Spr.Anims) to High (Spr.Anims) do
         Spr.Anims[Ndx].lval := Nil;
   end;



   procedure InitTokens;
   const
      TokenW = TokenSize * TokenScale;
      TokenX = GameBoardX + TokenW / 2;
      TokenY = 80;
   var
      Ndx, tx, ty: Integer;
      px, py: Single;
   begin
      for Ndx := Low (Tokens) to High (Tokens) do
      begin
         tx := Ndx mod TOKENS_X;
         ty := Ndx div TOKENS_X;
         px := TokenX + tx * TokenW;
         py := TokenY + ty * TokenW;

         Tokens[Ndx].TheType := TYPE_NONE;
         Tokens[Ndx].X := px;
         Tokens[Ndx].Y := py;
         Tokens[Ndx].Pitch := NUM_PITCH - 1 - ty;
         InitSprite (Tokens[Ndx].Bot, IMG_BLACK, px, py, TokenScale, 0.4);
         InitSprite (Tokens[Ndx].Top, IMG_BLACK, px, py, TokenScale, 0.0);
      end;
   end;



   procedure InitButtons;
   const
      Dist: array [0..NUM_TYPES - 1] of Single = (-1.5, -0.5, 0.5, 1.5);
   var
      Ndx: Integer;
      X, Y: Single;
   begin
      Y := ScreenH - 80;
      for Ndx := Low (Buttons) to High (Buttons) do
      begin
         X := ScreenW div 2 + 150 * Dist[Ndx];
         Buttons[Ndx].TheType := Ndx;
         Buttons[Ndx].X := X;
         Buttons[Ndx].Y := Y;
         InitSprite (Buttons[Ndx].Bot, IMG_DROPSHADOW, X, Y, DropshadowUnselScale, 0.4);
         Buttons[Ndx].Bot.AlignY := 0.0;
         InitSprite (Buttons[Ndx].Top, Ndx, X, Y, ButtonUnselScale, 1.0);
      end;
   end;



   procedure InitGlow;
   begin
     InitSprite (Glow, IMG_GLOW, ScreenW div 2, ScreenH, 1.0, 1.0);
     Glow.AlignY := 1.0;
     Glow.R := 0.0;
     Glow.g := 0.0;
     Glow.b := 0.0;

     InitSprite (GlowOverlay, IMG_GLOW_OVERLAY, 0.0, 0.0, 1.0, 1.0);
     GlowOverlay.AlignX := 0.0;
     GlowOverlay.AlignY := 0.0;
     GlowOverlay.R := 0.0;
     GlowOverlay.g := 0.0;
     GlowOverlay.b := 0.0;

     GlowColor[TYPE_EARTH] := al_map_rgb ($6B, $8E, $23); { olivedrab }
     GlowColor[TYPE_WIND]  := al_map_rgb ($AD, $D8, $E6); { lightblue }
     GlowColor[TYPE_WATER] := al_map_rgb ($41, $69, $E1); { royalblue }
     GlowColor[TYPE_FIRE]  := al_map_rgb ($FF, $00, $00); { red }
   end;



(****************************************************************************
 * Flairs                                                                   *
 ****************************************************************************)

   function MakeFlair (Image: Integer; X, Y, EndTime: Single): PSprite;
   var
      FL: PFlair;
   begin
      GetMem (FL, SizeOf (TFlair));
      InitSprite (FL^.Sprite, Image, X, Y, 1.0, 1.0);
      FL^.EndTime := EndTime;
      FL^.Next := Flairs;
      Flairs := FL;
      Exit (@(FL^.Sprite))
   end;



   procedure FreeOldFlairs (Now: Single);
   var
      Prev, Fl, Next: PFlair;
   begin
      Prev := Nil;
      Fl := Flairs;
      while Fl <> Nil do
      begin
         Next := Fl^.Next;
         if Fl^.EndTime > Now then
            Prev := Fl
         else begin
            if Prev <> Nil then
               Prev^.Next := Next
            else
               Flairs := Next;
            FreeMem (Fl, SizeOf (TFlair));
         end;
         Fl := Next;
      end;
   end;



   procedure FreeAllFlairs;
   var
      Next: PFlair;
   begin
     while Flairs <> Nil do
     begin
        Next := Flairs^.Next;
        FreeMem (Flairs, SizeOf (TFlair));
        Flairs := Next;
     end;
   end;



(****************************************************************************
 * Animations                                                               *
 ****************************************************************************)

   var
     DummyAnim: TAnim;

   function GetNextAnim (const Spr: TSprite): PAnim;
   var
     Ndx: LongWord;
   begin
     for Ndx := Low (Spr.Anims) to High (Spr.Anims) do
       if Spr.Anims[Ndx].lval = Nil then
         Exit (@(Spr.Anims[Ndx]));
     Exit (@DummyAnim)
   end;



   procedure FixConflictingAnims (var Grp: TSprite; lVal: PSingle; StartTime, StartVal: Single);
   var
      Ndx: LongWord;
      Anim: PAnim;
   begin
      for Ndx := Low (Grp.Anims) to High (Grp.Anims) do
      begin
         Anim := @(Grp.Anims[Ndx]);
         if Anim^.lVal = lVal then
         begin
         { If an old animation would overlap with the new one, truncate it
           and make it converge to the new animation's starting value. }
            if Anim^.EndTime > StartTime then
            begin
               Anim^.EndTime := StartTime;
               Anim^.EndVal := StartVal;
            end;
         { Cancel any old animations which are scheduled to start after the
           new one, or which have been reduced to nothing. }
            if (Anim^.StartTime >= StartTime)
            or (Anim^.StartTime >= Anim^.EndTime) then
               Grp.Anims[Ndx].lVal := Nil;
         end;
      end;
   end;



   procedure AnimFull (var Spr: TSprite; lVal: PSingle; StartVal, EndVal: Single; Func: TInterp; Delay, Duration: Single);
   var
      StartTime: Single;
      Anim: PAnim;
   begin
     StartTime := al_get_time + Delay;
     FixConflictingAnims (Spr, lVal, StartTime, StartVal);

     Anim := GetNextAnim (Spr);
     Anim^.lVal := lVal;
     Anim^.StartVal := StartVal;
     Anim^.EndVal := EndVal;
     Anim^.Func := Func;
     Anim^.StartTime := StartTime;
     Anim^.EndTime := StartTime + Duration;
   end;



   procedure Anim (var Spr: TSprite; lVal: PSingle; StartVal, EndVal: Single; Func: TInterp; Duration: Single);
   begin
      AnimFull (Spr, lVal, StartVal, EndVal, Func, 0, Duration);
   end;



   procedure AnimTo (var Spr: TSprite; lVal: PSingle; EndVal: Single; Func: TInterp; Duration: Single);
   begin
     AnimFull (Spr, lVal, lVal^, EndVal, Func, 0, Duration);
   end;



   procedure AnimDelta (var Spr: TSprite; lVal: PSingle; Delta: Single; Func: TInterp; Duration: Single);
   begin
     AnimFull (Spr, lVal, lVal^, lVal^ + Delta, Func, 0, Duration);
   end;



   procedure AnimTint (var Spr: TSprite; const Color: ALLEGRO_COLOR; Func: TInterp; Duration: Single);
   var
      R, G, B: Single;
   begin
     al_unmap_rgb_f (Color, R, G, B);
     AnimTo (Spr, @(Spr.r), R, Func, Duration);
     AnimTo (Spr, @(Spr.g), G, Func, Duration);
     AnimTo (Spr, @(Spr.b), B, Func, Duration);
   end;



   function Interpolate (Func: TInterp; t: Single): Single;
   var
      b, c, d: Single;
   begin
      case Func of
         INTERP_LINEAR:
            Exit (t);
         INTERP_FAST:
            Exit (-t * (t - 2));
         INTERP_DOUBLE_FAST:
         begin
            t := t - 1;
            Exit (t * t * t + 1);
         end;
         INTERP_SLOW:
            Exit (t * t);
         INTERP_DOUBLE_SLOW:
            Exit (t * t * t);
         INTERP_SLOW_IN_OUT:
         begin
         { Quadratic easing in/out - acceleration until halfway, then deceleration. }
            b := 0; { TODO: Why are these values in variables? }
            c := 1;
            d := 1;
            t := t / (d / 2);
            if t < 1 then
               Exit (c / 2 * t * t + b)
            else begin
               t := t - 1;
               Exit ((-c) / 2 * (t * (t - 2) - 1) + b)
            end
         end;
         INTERP_BOUNCE:
         begin { TODO: Next comment may explay the previous TODO (WTF?) }
         { BOUNCE EASING: exponentially decaying parabolic bounce
           t: current time, b: beginning value, c: change in position, d: duration
           bounce easing out }
            if t < (1 / 2.75) then
               Exit (7.5625 * t * t)
            else if t < (2 / 2.75) then
            begin
               t := t - (1.5 / 2.75);
               Exit (7.5625 * t * t + 0.75)
            end
            else if t < (2.5 / 2.75) then
            begin
               t := t - (2.5 / 2.75);
               Exit (7.5625 * t * t + 0.9375)
            end
            else begin
               t := t - (2.625 / 2.75);
               Exit (7.5625 * t * t + 0.984375)
            end;
         end;
         else
            Exit (0.0)
      end
   end;



   procedure UpdateAnim (var Anim: TAnim; Now: Single);
   var
      dt, t, Range: Single;
   begin
      if Anim.lVal = Nil then
         Exit;
      if Now < Anim.StartTime then
         Exit;
      dt := Now - Anim.StartTime;
      t := dt / (Anim. EndTime - Anim.StartTime);
      if t >= 1.0 then
      begin
      { Animation has run to completion }
         Anim.lVal^ := Anim.EndVal;
         Anim.lVal := Nil;
         Exit;
      end;
      Range := Anim.EndVal - Anim.StartVal;
      Anim.lVal^ := Anim.StartVal + Interpolate (Anim.Func, t) * Range;
   end;



   procedure UpdateSpriteAnims (var Spr: TSprite; Now: Single);
   var
      Ndx: Integer;
   begin
      for Ndx := Low (Spr.Anims) to High (Spr.Anims) do
         UpdateAnim (Spr.Anims[Ndx], Now);
   end;


   procedure UpdateTokenAnims (var Token: TToken; Now: Single);
   begin
      UpdateSpriteAnims (Token.Bot, Now);
      UpdateSpriteAnims (Token.Top, Now);
   end;



   procedure UpdateAnims (Now: Single);
   var
      fl: PFlair;
      Ndx: Integer;
   begin
      for Ndx := Low (Tokens) to High (Tokens) do
         UpdateTokenAnims (Tokens[Ndx], Now);
      for Ndx := Low (Buttons) to High (Buttons) do
         UpdateTokenAnims (Buttons[Ndx], Now);
      UpdateSpriteAnims (Glow, Now);
      UpdateSpriteAnims (GlowOverlay, Now);
      fl := Flairs;
      while fl <> Nil do
      begin
         UpdateSpriteAnims (fl^.Sprite, Now);
         fl := fl^.Next;
      end;
   end;



(****************************************************************************
 * Drawing                                                                  *
 ****************************************************************************)

   procedure DrawSprite (const Spr: TSprite);
   var
      Bmp: ALLEGRO_BITMAPptr;
      Tint: ALLEGRO_COLOR;
      cx, cy: Single;
   begin
      Bmp := Images[Spr.Image];
      cx := Spr.AlignX * al_get_bitmap_width (Bmp);
      cy := Spr.AlignY * al_get_bitmap_height (Bmp);
      Tint := al_map_rgba_f (Spr.r, Spr.g, Spr.b, Spr.Opacity);
      al_draw_tinted_scaled_rotated_bitmap (
         Bmp, Tint, cx, cy,
         Spr.x, Spr.y, Spr.ScaleX, Spr.ScaleY, Spr.Angle, 0
      );
   end;



   procedure DrawToken (const Token: TToken);
   begin
      DrawSprite (Token.Bot);
      DrawSprite (Token.Top);
   end;



   procedure DrawScreen;
   var
      fl: PFlair;
      Ndx: Integer;
   begin
      al_clear_to_color (al_map_rgb (0, 0, 0));
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_ONE);
      DrawSprite (Glow);
      DrawSprite (GlowOverlay);
      for Ndx := Low (Tokens) to High (Tokens) do
         DrawToken (Tokens[Ndx]);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      for Ndx := Low (Buttons) to High (Buttons) do
         DrawToken (Buttons[Ndx]);
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_ONE);
      fl := Flairs;
      while fl <> Nil do
      begin
         DrawSprite (fl^.Sprite);
         fl := fl^.Next;
      end;
      al_flip_display;
   end;



(****************************************************************************
 * Playback                                                                 *
 ****************************************************************************)

   procedure SpawnWindEffects (x, y: Single);
   var
      Now: Single;
      Spr: PSprite;
   begin
      Now := al_get_time;
      Spr := MakeFlair (IMG_AIR_EFFECT, x, y, Now + 1);
      Anim (Spr^, @(Spr^.ScaleX), 0.9, 1.3, INTERP_FAST, 1);
      Anim (Spr^, @(Spr^.ScaleY), 0.9, 1.3, INTERP_FAST, 1);
      Anim (Spr^, @(Spr^.Opacity), 1, 0, INTERP_FAST, 1);

      Spr := MakeFlair (IMG_AIR_EFFECT, x, y, Now + 1.2);
      Anim (Spr^, @(Spr^.Opacity), 1, 0, INTERP_LINEAR, 1.2);
      Anim (Spr^, @(Spr^.ScaleX), 1.1, 1.5, INTERP_FAST, 1.2);
      Anim (Spr^, @(Spr^.ScaleY), 1.1, 0.5, INTERP_FAST, 1.2);
      AnimDelta (Spr^, @(spr^.x), 10, INTERP_FAST, 1.2);
   end;



   procedure SpawnFireEffects (x, y: Single);
   var
      Now: Single;
      Spr: PSprite;
      i: Integer;
   begin
      Now := al_get_time;
      Spr := MakeFlair (IMG_MAIN_FLAME, x, y, Now + 0.8);
      Spr^.AlignY := 0.75;
      AnimFull (Spr^, @(Spr^.ScaleX),  0.2, 1.3, INTERP_BOUNCE, 0.0, 0.4);
      AnimFull (Spr^, @(Spr^.ScaleY),  0.2, 1.3, INTERP_BOUNCE, 0.0, 0.4);
      AnimFull (Spr^, @(Spr^.ScaleX),  1.3, 1.4, INTERP_BOUNCE, 0.4, 0.5);
      AnimFull (Spr^, @(Spr^.ScaleY),  1.3, 1.4, INTERP_BOUNCE, 0.4, 0.5);
      AnimFull (Spr^, @(Spr^.Opacity), 1.0, 0.0, INTERP_FAST, 0.3, 0.5);
      for i := 0 to 2 do
      begin
         Spr := MakeFlair (IMG_FLAME, x, y, Now + 0.7);
         Spr^.AlignX := 1.3;
         Spr^.Angle := ALLEGRO_TAU / 3 * i;
         AnimDelta (Spr^, @(Spr^.Angle), -PI, INTERP_DOUBLE_FAST, 0.7);
         Anim (Spr^, @(Spr^.Opacity), 1.0, 0.0, INTERP_SLOW, 0.7);
         Anim (Spr^, @(Spr^.ScaleX),  0.2, 1.0, INTERP_FAST, 0.7);
         Anim (Spr^, @(Spr^.ScaleY),  0.2, 1.0, INTERP_FAST, 0.7);
      end;
   end;



   procedure SpawnWaterEffects (x, y: Single);

      function RandomSign: Single;
      begin
         if Random (2) > 0 then
            Exit (-1)
         else
            Exit (1)
      end;

      function RandomFloat (Min, Max: Single): Single;
      begin
         Exit (Random * (Max - Min) + Min)
      end;

   const
      MaxDuration = 1;

      function MRand (Min, Max: Single): Single;
      begin
         Exit (RandomFloat (Min, Max) * MaxDuration)
      end;

   var
      Now: Single;
      Spr: PSprite;
      i: Integer;
   begin
      Now := al_get_time;
      Spr := MakeFlair (IMG_WATER, x, y, Now + MaxDuration);
      Anim (Spr^, @(Spr^.ScaleX), 1.0, 2.0, INTERP_FAST, 0.5);
      Anim (Spr^, @(Spr^.ScaleY), 1.0, 2.0, INTERP_FAST, 0.5);
      Anim (Spr^, @(Spr^.Opacity), 0.5, 0.0, INTERP_FAST, 0.5);
      for i := 0 to 8 do
      begin
         Spr := MakeFlair (IMG_WATER_DROPS, x, y, Now + MaxDuration);
         Spr^.ScaleX := RandomFloat (0.3, 1.2) * RandomSign;
         Spr^.ScaleY := RandomFloat (0.3, 1.2) * RandomSign;
         Spr^.Angle := RandomFloat (0, ALLEGRO_TAU);
         Spr^.r := RandomFloat (0, 0.6);
         Spr^.g := RandomFloat (0.4, 0.6);
         Spr^.b := 1;
         if i = 0 then
            AnimTo (Spr^, @(Spr^.Opacity), 0, INTERP_LINEAR, MaxDuration)
         else
            AnimTo (Spr^, @(Spr^.Opacity), 0, INTERP_DOUBLE_SLOW, MRand (0.7, 1));
         AnimTo (Spr^, @(Spr^.ScaleX), RandomFloat (0.8, 3), INTERP_FAST, MRand (0.7, 1));
         AnimTo (Spr^, @(Spr^.ScaleY), RandomFloat (0.8, 3), INTERP_FAST, MRand (0.7, 1));
         AnimDelta (Spr^, @(Spr^.X), MRand (0, 20) * RandomSign, INTERP_FAST, MRand (0.7, 1));
         AnimDelta (Spr^, @(Spr^.Y), MRand (0, 20) * RandomSign, INTERP_FAST, MRand (0.7, 1))
      end
   end;



   procedure PlayElement (TheType, Pitch: Integer; Volume, Pan: Single);
   begin
      al_play_sample (ElementSamples[TheType][Pitch], Volume, Pan, 1.0, ALLEGRO_PLAYMODE_ONCE, Nil);
   end;



   procedure ActivateToken (Token: TToken);
   const
      sc = TokenScale;
   var
      Spr: PSprite;
   begin
      Spr := @(Token.Top);
      case Token.TheType of
      TYPE_EARTH:
         begin
            PlayElement (TYPE_EARTH, Token.Pitch, 0.8, 0.0);
            Anim (Spr^, @(Spr^.ScaleX), Spr^.ScaleX + 0.4, Spr^.ScaleX, INTERP_FAST, 0.3);
            Anim (Spr^, @(Spr^.ScaleY), Spr^.ScaleY + 0.4, Spr^.ScaleY, INTERP_FAST, 0.3);
         end;
      TYPE_WIND:
         begin
            PlayElement (TYPE_WIND, Token.Pitch, 0.8, 0.0);
            AnimFull (Spr^, @(Spr^.ScaleX), sc * 1.0, sc * 0.8, INTERP_SLOW_IN_OUT, 0.0, 0.5);
            AnimFull (Spr^, @(Spr^.ScaleX), sc * 0.8, sc * 1.0, INTERP_SLOW_IN_OUT, 0.5, 0.8);
            AnimFull (Spr^, @(Spr^.ScaleY), sc * 1.0, sc * 0.8, INTERP_SLOW_IN_OUT, 0.0, 0.5);
            AnimFull (Spr^, @(Spr^.ScaleY), sc * 0.8, sc * 1.0, INTERP_SLOW_IN_OUT, 0.5, 0.8);
            SpawnWindEffects (Spr^.x, Spr^.y);
         end;
      TYPE_WATER:
         begin
            PlayElement (TYPE_WATER, Token.Pitch, 0.7, 0.5);
            AnimFull (Spr^, @(Spr^.ScaleX), sc * 1.3, sc * 0.8, INTERP_BOUNCE, 0.0, 0.5);
            AnimFull (Spr^, @(Spr^.ScaleX), sc * 0.8, sc * 1.0, INTERP_BOUNCE, 0.5, 0.5);
            AnimFull (Spr^, @(Spr^.ScaleY), sc * 0.8, sc * 1.3, INTERP_BOUNCE, 0.0, 0.5);
            AnimFull (Spr^, @(Spr^.ScaleY), sc * 1.3, sc * 1.0, INTERP_BOUNCE, 0.5, 0.5);
            SpawnWaterEffects (Spr^.x, Spr^.y);
         end;
      TYPE_FIRE:
         begin
            PlayElement (TYPE_FIRE, Token.Pitch, 0.8, 0.0);
            Anim (Spr^, @(Spr^.ScaleX), sc * 1.3, sc, INTERP_SLOW_IN_OUT, 1.0);
            Anim (Spr^, @(Spr^.ScaleY), sc * 1.3, sc, INTERP_SLOW_IN_OUT, 1.0);
            SpawnFireEffects (Spr^.x, Spr^.y);
         end;
      end;
   end;



   procedure UpdatePlayback;
   var
      y: Integer;
   begin
      for Y := 0 to (TOKENS_Y - 1) do
         ActivateToken (Tokens[Y * TOKENS_X + PlaybackColumn]);
      Inc (PlaybackColumn);
      if PlaybackColumn >= TOKENS_X then
         PlaybackColumn := 0;
   end;

(****************************************************************************
 * Control                                                                  *
 ****************************************************************************)

   function IsTouched (Token: TToken; Size, x, y: Single): Boolean;
   var
      Half: Single;
   begin
     Half := Size / 2;
     Exit ((Token.x - Half <= x) and (x < Token.x + Half)
           and (Token.y - Half <= y) and (y < Token.y + Half))
   end;



   function GetTouchedToken (x, y: Single): PToken;
   var
     Ndx: Integer;
   begin
     for Ndx := Low (Tokens) to High (Tokens) do
       if IsTouched (Tokens[Ndx], TokenSize, x, y) then
         Exit (@Tokens[Ndx]);
     Exit (Nil)
   end;



   function GetTouchedButton (x, y: Single): PToken;
   var
      Ndx: Integer;
   begin
     for Ndx := Low (Buttons) to High (Buttons) do
       if IsTouched (Buttons[Ndx], ButtonSize, x, y) then
         Exit (@Buttons[Ndx]);
     Exit (Nil)
   end;



  procedure UnselectToken (var token: TToken);
  var
    Spr: PSprite;
  begin
    if token.TheType <> TYPE_NONE then
    begin
      spr := @(token.top);
      AnimFull (Spr^, @(Spr^.Opacity), Spr^.Opacity, 0, INTERP_SLOW, 0.15, 0.15);
      token.TheType := TYPE_NONE
    end
  end;



  procedure UnselectAllTokens;
  var
    Ndx: Integer;
  begin
    for Ndx := Low (Tokens) to High (Tokens) do
      UnselectToken (Tokens[Ndx])
  end;



   procedure SelectToken (var Token: TToken);
   var
     PrevType: LongWord;
     Spr: PSprite;
   begin
     if SelectedButton <> Nil then
     begin
       PrevType := Token.TheType;
       UnselectToken (Token);

     { Unselect only if same type, for touch input. }
       if PrevType <> SelectedButton^.TheType then
       begin
         Spr := @(Token.Top);
         Spr^.Image := SelectedButton^.TheType;
         AnimTo (Spr^, @(Spr^.Opacity), 1, INTERP_FAST, 0.15);
         Token.TheType := SelectedButton^.TheType
       end
     end
   end;



   procedure ChangeHealthyGlow (TheType: Integer; x: Single);
   begin
      AnimTint (Glow, GlowColor[TheType], INTERP_SLOW_IN_OUT, 3.0);
      AnimTo (Glow, @Glow.x, x, INTERP_SLOW_IN_OUT, 3.0);

      AnimTint (GlowOverlay, GlowColor[TheType], INTERP_SLOW_IN_OUT, 4.0);
      AnimTo (GlowOverlay, @GlowOverlay.Opacity, 1.0, INTERP_SLOW_IN_OUT, 4.0);
   end;



   procedure SelectButton (Button: PToken);
   var
      Spr: PSprite;
   begin
      if Button <> SelectedButton then
      begin
        if SelectedButton <> Nil then
        begin
           Spr := @(SelectedButton^.Top);
           AnimTo (Spr^, @(Spr^.ScaleX), ButtonUnselScale, INTERP_SLOW, 0.3);
           AnimTo (Spr^, @(Spr^.ScaleY), ButtonUnselScale, INTERP_SLOW, 0.3);
           AnimTo (Spr^, @(Spr^.Opacity), 0.5, INTERP_DOUBLE_SLOW, 0.2);

           Spr := @(SelectedButton^.Bot);
           AnimTo (Spr^, @(Spr^.ScaleX), DropshadowUnselScale, INTERP_SLOW, 0.3);
           AnimTo (Spr^, @(Spr^.ScaleY), DropshadowUnselScale, INTERP_SLOW, 0.3);
        end;
        SelectedButton := Button;

        Spr := @(Button^.Top);
        AnimTo (Spr^, @(Spr^.ScaleX), ButtonSelScale, INTERP_FAST, 0.3);
        AnimTo (Spr^, @(Spr^.ScaleY), ButtonSelScale, INTERP_FAST, 0.3);
        AnimTo (Spr^, @(Spr^.Opacity), 1.0, INTERP_FAST, 0.3);

        Spr := @(Button^.Bot);
        AnimTo (Spr^, @(Spr^.ScaleX), DropshadowSelScale, INTERP_FAST, 0.3);
        AnimTo (Spr^, @(Spr^.ScaleY), DropshadowSelScale, INTERP_FAST, 0.3);

        ChangeHealthyGlow (Button^.TheType, Button^.x);

        al_play_sample (SelectSample, 1.0, 0.0, 1.0, ALLEGRO_PLAYMODE_ONCE, Nil);
      end
   end;



   procedure onMouseDown (x, y: Single; mButton: Integer);
   var
      Token, Button: PToken;
   begin
      if mButton = 1 then
      begin
         Token := GetTouchedToken (x, y);
         if Token <> Nil then
            SelectToken (Token^)
         else begin
            Button := GetTouchedButton (x, y);
            if Button <> Nil then
               SelectButton (Button);
         end;
      end
      else if mButton = 2 then
      begin
         Token := GetTouchedToken (x, y);
         if Token <> Nil then
            UnselectToken (Token^);
      end;
   end;



   procedure onMouseAxes (x, y: Single);
   var
      Token: PToken;
      Spr: PSprite;
   begin
      Token := GetTouchedToken (x, y);
      if Token = HoverToken then
         Exit;
      if HoverToken <> Nil then
      begin
         Spr := @(HoverToken^.Bot);
         AnimTo (Spr^, @(Spr^.Opacity), 0.4, INTERP_DOUBLE_SLOW, 0.2);
      end;
      HoverToken := Token;
      if HoverToken <> Nil then
      begin
         Spr := @(HoverToken^.Bot);
         AnimTo (Spr^, @(Spr^.Opacity), 0.7, INTERP_FAST, 0.2);
      end;
   end;



   procedure MainLoop (Queue: ALLEGRO_EVENT_QUEUEptr);
   var
      Event: ALLEGRO_EVENT;
      EndLoop, Redraw: Boolean;
      Now: Single;
   begin
      EndLoop := False;
      Redraw := True;
      repeat
         if Redraw and al_is_event_queue_empty (Queue) then
         begin
            Now := al_get_time;
            FreeOldFlairs (Now);
            UpdateAnims (Now);
            DrawScreen;
            Redraw := False;
         end;

         al_wait_for_event (Queue, @Event);

         if Event.timer.source = RefreshTimer then
            Redraw := true
         else if Event.timer.source = PlaybackTimer then
            UpdatePlayback
         else if Event.ftype = ALLEGRO_EVENT_MOUSE_AXES then
            onMouseAxes (Event.mouse.x, Event.mouse.y)
         else if Event.ftype = ALLEGRO_EVENT_MOUSE_BUTTON_DOWN then
            onMouseDown (event.mouse.x, event.mouse.y, event.mouse.button)
         else if Event.ftype = ALLEGRO_EVENT_DISPLAY_CLOSE then
            EndLoop := True
         else if Event.ftype = ALLEGRO_EVENT_KEY_DOWN then
         begin
            if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
               EndLoop := True;
            if Event.keyboard.keycode = ALLEGRO_KEY_C then
               UnselectAllTokens;
         end;
      until EndLoop;
   end;

var
   Queue: ALLEGRO_EVENT_QUEUEptr;
begin
   if not al_init then
      AbortExample ('Error initialising Allegro.');
   if not al_install_audio or not al_reserve_samples (128) then
      AbortExample ('Error initialising audio.');
   al_init_acodec_addon;
   al_init_image_addon;

   al_set_new_bitmap_flags (ALLEGRO_MIN_LINEAR or ALLEGRO_MAG_LINEAR);

   Display := al_create_display (ScreenW, ScreenH);
   if Display = Nil then
      AbortExample ('Error creating display.');
   al_set_window_title (Display, 'Haiku - A Musical Instrument');

   LoadImages;
   LoadSamples;

   InitTokens;
   InitButtons;
   InitGlow;
   SelectButton (@(Buttons[TYPE_EARTH]));

   al_install_keyboard;
   al_install_mouse;

   RefreshTimer := al_create_timer (1.0 / RefreshRate);
   PlaybackTimer := al_create_timer (PlaybackPeriod / TOKENS_X);

   Queue := al_create_event_queue;
   al_register_event_source (Queue, al_get_display_event_source (Display));
   al_register_event_source (Queue, al_get_keyboard_event_source);
   al_register_event_source (Queue, al_get_mouse_event_source);
   al_register_event_source (Queue, al_get_timer_event_source (RefreshTimer));
   al_register_event_source (Queue, al_get_timer_event_source (PlaybackTimer));

   al_start_timer (RefreshTimer);
   al_start_timer (PlaybackTimer);

   MainLoop (Queue);

   FreeAllFlairs;
end.
