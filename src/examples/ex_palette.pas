program ex_palette;
(* Show how to use shaders to simulate VGA indexed mode. *)
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
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  uses
    Common,
    Allegro5, al5base, al5color, al5image, al5strings,
    math;

(*
 * Palette stuff.
 ************************************************************************)

  const
  (* Vertex shader program:  It gets the texel coordinate to use it in the
     fragment shader.
   *)
    VertexShaderSrc = 'attribute vec4 al_pos;'+
                      'attribute vec4 al_color;'+
                      'attribute vec2 al_texcoord;'+
                      'uniform mat4 al_projview_matrix;'+
                      'varying vec2 varying_texcoord;'+
                      'void main ()'+
                      '{'+
                      '  varying_texcoord = al_texcoord;'+
                      '  gl_Position = al_projview_matrix * al_pos;'+
                      '}';
  (* Fragment shader:  It uses the texel coordinate to get the color from the
     texture (al_tex) and then use it to get the actual color from the color
     palette and plot it in the destination.  Note it uses the red component as
     index and the alpha component for transparency; the other components are
     ignored.
   *)
    PixelShaderSrc = 'uniform sampler2D al_tex;'+
                     'uniform vec3 pal[256];'+
                     'varying vec2 varying_texcoord;'+
                     'void main ()'+
                     '{'+
                     '  vec4 c = texture2D (al_tex, varying_texcoord);'+
                     '  int index = int (c.r * 255.0);'+
                     '  if (index != 0) {;'+
                     '    gl_FragColor = vec4 (pal[index], 1);'+
                     '  }'+
                     '  else {;'+
                     '    gl_FragColor = vec4 (0, 0, 0, 0);'+
                     '  };'+
                     '}';

  type
  (* Store palette information. *)
    TPalette = array [0..255] of record r, g, b: AL_FLOAT end;

  var
  (* Shader used to simulate indexed graphics. *)
    Shader: ALLEGRO_SHADERptr;
  (* Color data. *)
    PalHex: array [0..255] of AL_INT = (
      $FF00FF, $000100, $060000, $040006, $000200,
      $000306, $010400, $030602, $02090C, $070A06,
      $020C14, $030F1A, $0F0E03, $0D0F0C, $071221,
      $0D1308, $0D1214, $121411, $12170E, $151707,
      $0A182B, $171816, $131B0C, $1A191C, $171D08,
      $081D35, $1A200E, $1D1F1C, $1D2013, $0E2139,
      $06233F, $17230E, $1C270E, $21260F, $0D2845,
      $0A294C, $1F2A12, $252724, $232B19, $222D15,
      $0C2F51, $0D2F57, $263012, $2B2D2B, $233314,
      $273617, $0D3764, $17355E, $2C3618, $2E3623,
      $333432, $2C3A15, $093D70, $333B17, $163C6A,
      $2F3D18, $323D24, $383A38, $30401B, $2F431C,
      $1E4170, $12447D, $154478, $3F403E, $34471A,
      $3D482C, $134B8B, $3A4D20, $184D86, $474846,
      $3A511D, $13549A, $3D5420, $195595, $0F57A3,
      $4E504D, $415925, $435B27, $485837, $125DA9,
      $485E24, $175FB2, $235DA3, $555754, $0565BD,
      $1C61B5, $2163B7, $2164B1, $49662A, $1268C1,
      $2365B9, $1769C3, $5E605D, $196BBE, $55673D,
      $1B6BC5, $2968BC, $246BB8, $526D2A, $0E73CC,
      $0E74C6, $246FC9, $2470C4, $56712E, $666865,
      $007DCE, $537530, $2A72CC, $55762B, $1B77D0,
      $1F77D8, $1E79CC, $2E74CF, $58782D, $2E75CA,
      $59792E, $2279D3, $5A7A2F, $3276D2, $6D6F6C,
      $1081D3, $137FDF, $237DC9, $5B7C30, $637848,
      $2A7DD7, $5E7F33, $2C7DDE, $2A80CD, $1D82E2,
      $1A85D1, $2B80D5, $747673, $2D82CF, $2F84D1,
      $3381E3, $2289D5, $3285D2, $2986EE, $2189ED,
      $4782C5, $3884DF, $4083D2, $3487D4, $278BD7,
      $298ADD, $67883B, $7B7D7A, $2A8CD9, $6C8653,
      $3289E2, $3889D7, $2C8DDA, $2E8FDB, $3D8CDA,
      $2F90DC, $338EE8, $3191DD, $3E8EDE, $3392DE,
      $838582, $709145, $3593E0, $4191D9, $3794E1,
      $698AB1, $4590E5, $3B93E6, $789158, $4594DC,
      $3C97E4, $4896DE, $4397EA, $3D9AE1, $8B8E8B,
      $409CE3, $4B99E1, $439CEA, $539AD6, $5898E2,
      $439EE5, $4E9BE4, $439FEC, $809C5F, $7C9E57,
      $45A0E7, $509FE1, $47A1E8, $599EDB, $48A2E9,
      $80A153, $4AA4EB, $959794, $5CA1DE, $51A3EF,
      $59A3E3, $4DA6ED, $4FA7EF, $51A8F0, $87A763,
      $5AA8EA, $53AAF2, $9C9E9B, $49AFF5, $56ACF5,
      $55AFF0, $8CAD67, $64ACE8, $60ADF0, $59AFF7,
      $6EACE2, $79A9E1, $63AFF2, $59B2F3, $90B162,
      $A6A8A5, $60B5F4, $94B56D, $99BC72, $AEB0AD,
      $74BBF2, $8DB8ED, $94B7E3, $8ABEEA, $A0C379,
      $82C0F2, $B6B8B5, $A3C77C, $A5C97E, $A9CA79,
      $8FC7F3, $BEC0BD, $A1C6E9, $97C9F0, $ADD07E,
      $C8CAC7, $ACD1F0, $B6CFF0, $B9D5ED, $D1D3D0,
      $BEDAF4, $D9DBD8, $C7E2FB, $CDE3F6, $E1E3E0,
      $E4E9EC, $DBEBF9, $EAECE9, $E7EFF8, $F1F3F0,
      $ECF4FD, $F2F7FA, $F6F8F5, $F7FCFF, $FAFCF8,
      $FDFFFC
    );

(* Create a shader that simulates indexed graphics. *)
  function CreateShader: ALLEGRO_SHADERptr;
  begin
    Result := al_create_shader (ALLEGRO_SHADER_GLSL);
    if not Assigned (Result) then
    begin
      ErrorMessage ('Cannot use GLSL (OpenGL) shader.');
      Exit (Nil)
    end;
    if not al_attach_shader_source (Result, ALLEGRO_VERTEX_SHADER, VertexShaderSrc)
    or not al_attach_shader_source (Result, ALLEGRO_PIXEL_SHADER, PixelShaderSrc)
    or not al_build_shader (Result) then
    begin
      ErrorMessage (al_str_to_string (al_get_shader_log (Result)));
      al_destroy_shader (Result);
      Exit (Nil)
    end
  end;



(* Interpolate two palettes.

   If t=0 then it uses Pal1.
   If t=1 then it uses Pal2.
   If t=0.5 then it uses (Pal1+Pal2)/2.
   etc.
 *)
  function InterpolatePalette (Pal1, Pal2: TPalette; t: AL_FLOAT): TPalette;
  var
    lNdx: Integer;
  begin
    Result := Default (TPalette);
    for lNdx := 0 to 255 do
    begin
      Result[lNdx].r := Pal1[lNdx].r * (1 - t) + Pal2[lNdx].r * t;
      Result[lNdx].g := Pal1[lNdx].g * (1 - t) + Pal2[lNdx].g * t;
      Result[lNdx].b := Pal1[lNdx].b * (1 - t) + Pal2[lNdx].b * t
    end
  end;



(* Apply the given palette. *)
  procedure SetPalette (var aPalette: TPalette); inline;
  begin
    al_set_shader_float_vector ('pal', 3, @aPalette, 256)
  end;



(*
 * The example.
 ************************************************************************)

  const
  (* Window size. *)
    wWidth = 640; wHeight = 480;

  type
    TSprite = record
      x, y, Angle, t: Single;
      Flags, i, j: Integer;
    end;

  var
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Window: ALLEGRO_DISPLAYptr;
    Timer: ALLEGRO_TIMERptr;
    Redraw, Terminated: Boolean;
  { Images. }
    Bitmap, Background: ALLEGRO_BITMAPptr;
  { Palettes. }
    Pals: array [0..6] of TPalette;
  { Sprites stuff. }
    Sprites: array [0..7] of TSprite;

(* Program initialization. *)
  function Initialize: Boolean;

    function CreateDisplay: Boolean;
    begin
    { Create display. }
      al_set_new_display_flags (
        ALLEGRO_PROGRAMMABLE_PIPELINE { To allow use of shaders. }
        or ALLEGRO_OPENGL             { To use OpenGL shaders. }
      );
      Window := al_create_display (wWidth, wHeight);
      if not Assigned (Window) then
      begin
        ErrorMessage ('Could not create display.');
        Exit (False)
      end;
    { Create shader. }
      Shader := CreateShader;
      if not Assigned (Shader) then Exit (False);
    { Tell Allegro to use 8bpp pixel formats. }
      al_set_new_bitmap_format (ALLEGRO_PIXEL_FORMAT_SINGLE_CHANNEL_8);

      Result := True
    end;

    function CreateSprites: Boolean;
    var
      lNdx: Integer;
      lSin, lCos: Single;
    begin
      Bitmap := al_load_bitmap_flags ('data/alexlogo.png', ALLEGRO_KEEP_INDEX);
      if not Assigned (Bitmap) then
      begin
        ErrorMessage ('"data/alexlogo.png" not found or failed to load.');
        Exit (False)
      end;

      for lNdx := Low (Sprites) to High (Sprites) do
      begin
        Sprites[lNdx].Angle := ALLEGRO_TAU * lNdx / 8;
        SinCos (Sprites[lNdx].Angle, lSin, lCos);
        Sprites[lNdx].x := 320 + lSin * (64 + lNdx * 16);
        Sprites[lNdx].y := 240 - lCos * (64 + lNdx * 16);
        if lNdx mod 2 <> 0 then
          Sprites[lNdx].Flags := ALLEGRO_FLIP_HORIZONTAL
        else
          Sprites[lNdx].Flags := 0;
        Sprites[lNdx].t := lNdx / 8;
        Sprites[lNdx].i := lNdx mod 6;
        Sprites[lNdx].j := (Sprites[lNdx].i + 1) mod 6
      end;
      Result := True
    end;

    procedure BuildColorPalettes;
    var
      j, i: Integer;
      r, g, b, h, s, l: AL_FLOAT;
    begin
      for j := Low (Pals) to High (Pals) do
      begin
        for i := Low (PalHex) to High (PalHex) do
        begin
          r := ((PalHex[i] shr 16) and $0000FFFF) / 255;
          g := ((PalHex[i] shr  8) and $000000FF) / 255;
          b := ( PalHex[i]         and $000000FF) / 255;

          al_color_rgb_to_hsl (r, g, b, h, s, l);
          if j = 6 then
          begin
            if (l < 0.3) or (0.7 < l) then
            begin
              h := 0;
              s := 1;
              l := 0.5
            end
          end
          else begin
            h := h + (j * 60);
            if (l < 0.3) or (0.7 < l) then
            begin
              if (j and 1) <> 0 then l := 1 - l
            end
          end;
          al_color_hsl_to_rgb (h, s, l, r, g, b);
 
          Pals[j][i].r := r;
          Pals[j][i].g := g;
          Pals[j][i].b := b
        end
      end
    end;

  begin
    if not al_init or not al_install_keyboard or not al_init_image_addon then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;

    Timer := al_create_timer (1 / 60);
    if not Assigned (Timer) then
    begin
      WriteLn ('Can''t initialize timer.');
      Exit (False)
    end;

    if not CreateDisplay then Exit (False);
    if not CreateSprites then Exit (False);
    Background := al_load_bitmap ('data/bkg.png');
    if not Assigned (Background) then
      ErrorMessage ('"data/bkg.png" not found or failed to load.');
  { Continue even if fail to load. }
    BuildColorPalettes;

    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      ErrorMessage ('Can''t initialize event queue!');
      Exit (False)
    end;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));
    al_register_event_source (EventQueue, al_get_timer_event_source (Timer));

    Result := True
  end;



(* Program finalization. *)
  procedure Finalize;
  begin
    al_use_shader (Nil);
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    if Assigned (Bitmap) then al_destroy_bitmap (Bitmap);
    if Assigned (Background) then al_destroy_bitmap (Background);
    if Assigned (Timer) then al_destroy_timer (Timer);
    if Assigned (Shader) then al_destroy_shader (Shader);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Update sprites position and rotation. *)
  procedure UpdateSprites;
  var
    lSpr, lDirection: Integer;
    lSin, lCos: Single;
  begin
    Redraw := True;
    for lSpr := Low (Sprites) to High (Sprites) do
    begin
      if Sprites[lSpr].Flags <> 0 then lDirection := 1 else lDirection := -1;
      sincos (Sprites[lSpr].Angle, lSin, lCos);
      Sprites[lSpr].x := Sprites[lSpr].x + lCos * 2 * lDirection;
      Sprites[lSpr].y := Sprites[lSpr].y + lSin * 2 * lDirection;
      Sprites[lSpr].Angle := Sprites[lSpr].Angle + ALLEGRO_TAU / 360 * lDirection
    end
  end;



(* Draw window content. *)
  procedure UpdateScreen;

    procedure DrawScaledBitmap (const aX, aY: Integer; const aSx, aSy: Single);
      inline;
    begin
      al_draw_scaled_rotated_bitmap (Bitmap, 0, 0, aX, aY, aSx, aSy, 0, 0)
    end;

  const
    lScale = 0.5;
  var
    lPosition: Single;
    lTicks, lP1, lP2, lNdx, lSpr: Integer;
    lPal: TPalette;
  begin
    lTicks := al_get_timer_count (Timer);
    lPosition := lTicks mod 60 / 60;
    lP1 := (lTicks div 60) mod 3;
    lP2 := (lP1 + 1) mod 3;

    al_clear_to_color (al_map_rgb_f (0, 0, 0));
    lPal := InterpolatePalette (Pals[lP1], Pals[lP2], lPosition);
    SetPalette (lPal);
    if Assigned (Background) then al_draw_bitmap (Background, 0, 0, 0);

    for lNdx := Low (Sprites) to High (Sprites) do
    begin
      lSpr := 7 - lNdx;
      lPosition := (
        1 + sin ((lTicks / 60 + Sprites[lSpr].t) * ALLEGRO_TAU)
      ) / 2;
      lPal := InterpolatePalette (
        Pals[Sprites[lSpr].i], Pals[Sprites[lSpr].j], lPosition
      );
      SetPalette (lPal);
      al_draw_rotated_bitmap (
        Bitmap,
        64, 64,
        Sprites[lSpr].x, Sprites[lSpr].y, Sprites[lSpr].Angle,
        Sprites[lSpr].Flags
      );
    end;

    if lTicks mod 20 > 15 then lSpr := 6 else lSpr := 0;
    SetPalette (Pals[lSpr]);
    DrawScaledBitmap (  0,   0,  lScale,  lScale);
    DrawScaledBitmap (640,   0, -lScale,  lScale);
    DrawScaledBitmap (  0, 480,  lScale, -lScale);
    DrawScaledBitmap (640, 480, -lScale, -lScale);

    al_flip_display
  end;


begin
  if not Initialize then Exit;
{ "Game loop". }
  al_start_timer (Timer); al_set_timer_count (Timer, 0);
  Redraw := False;
  Terminated := False;
  al_use_shader (Shader);
  repeat
  { Screen update. }
    if Redraw and al_is_event_queue_empty (EventQueue) then
    begin
      UpdateScreen;
      Redraw := False
    end;
  { Check events. }
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_CHAR:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Terminated := True;
    ALLEGRO_EVENT_TIMER:
      UpdateSprites;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
