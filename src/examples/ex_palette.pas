PROGRAM ex_palette;
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
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  uses
    Allegro5, al5base, al5color, al5image,
    Common,
    math;

  type
    TPalette = array [0..255] of record r, g, b: AL_FLOAT; end;

    RSprite = record
      x, y, Angle, t: Real;
      Flags, i, j: Integer;
    end;

  var
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



  function InterpolatePalette (Pal1, Pal2: TPalette; t: AL_FLOAT): TPalette;
  var
    Pal: TPalette;
    i: Integer;
  begin
    for i := 0 to 255 do
    begin
      Pal[i].r := Pal1[i].r * (1 - t) + Pal2[i].r * t;
      Pal[i].g := Pal1[i].g * (1 - t) + Pal2[i].g * t;
      Pal[i].b := Pal1[i].b * (1 - t) + Pal2[i].b * t
    end;
    Exit (Pal)
  end;



  var
    Display: ALLEGRO_DISPLAYptr;
    Bitmap, Background: ALLEGRO_BITMAPptr;
    Timer: ALLEGRO_TIMERptr;
    Queue: ALLEGRO_EVENT_QUEUEptr;
    Event: ALLEGRO_EVENT;
    Redraw, EndExample: Boolean;
    Shader: ALLEGRO_SHADERptr;
    Pal: TPalette;
    Pals: array [0..6] of TPalette;
    i, j, Dir, p1, p2: Integer;
    r, g, b, h, s, l, t, Position, sc: AL_FLOAT;
    Sprites: array [0..7] of RSprite;

begin
  Redraw := True;
  EndExample := False;
  t := 0;

  if not al_init then AbortExample ('Could not init Allegro.');

  al_install_mouse;
  al_install_keyboard;
  al_init_image_addon;
  InitPlatformSpecific;

  al_set_new_display_flags (ALLEGRO_PROGRAMMABLE_PIPELINE or ALLEGRO_OPENGL);
  Display := al_create_display (640, 480);
  if Display = Nil then AbortExample ('Could not create display.');

  al_set_new_bitmap_format (ALLEGRO_PIXEL_FORMAT_SINGLE_CHANNEL_8);
  Bitmap := al_load_bitmap_flags ('data/alexlogo.bmp', ALLEGRO_KEEP_INDEX);
  if Bitmap = Nil then
    AbortExample ('"data/alexlogo.bmp" not found or failed to load.');

{ Create 8 sprites. }
  for i := Low (Sprites) to High (Sprites) do
  begin
    Sprites[i].Angle := ALLEGRO_PI * 2 * i / 8;
    sincos (Sprites[i].Angle, s, h);
    Sprites[i].x := 320 + s * (64 + i * 16);
    Sprites[i].y := 240 - h * (64 + i * 16);
    if i mod 2 <> 0 then
      Sprites[i].Flags := ALLEGRO_FLIP_HORIZONTAL
    else
      Sprites[i].Flags := 0;
    Sprites[i].t := i / 8;
    Sprites[i].i := i mod 6;
    Sprites[i].j := (Sprites[i].i + 1) mod 6;
  end;

  Background := al_load_bitmap ('data/bkg.png');
  if Background = Nil then
    AbortExample ('"data/bkg.png" not found or failed to load.');
{ Continue even if fail to load. }

{ Create 7 palettes with changed hue. }
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
  end;

  Shader := al_create_shader(ALLEGRO_SHADER_GLSL);
  if Shader = Nil then
    AbortExample ('Cannot use GLSL (OpenGL) shader.');

  al_attach_shader_source (
    Shader,
    ALLEGRO_VERTEX_SHADER,
    'attribute vec4 al_pos;'#10+
    'attribute vec4 al_color;'#10+
    'attribute vec2 al_texcoord;'#10+
    'uniform mat4 al_projview_matrix;'#10+
    'varying vec4 varying_color;'#10+
    'varying vec2 varying_texcoord;'#10+
    'void main()'#10+
    '{'#10+
    '  varying_color = al_color;'#10+
    '  varying_texcoord = al_texcoord;'#10+
    '  gl_Position = al_projview_matrix * al_pos;'#10+
    '}'#10
  );
  al_attach_shader_source(
    Shader,
    ALLEGRO_PIXEL_SHADER,
    'uniform sampler2D al_tex;'#10+
    'uniform vec3 pal[256];'#10+
    'varying vec4 varying_color;'#10+
    'varying vec2 varying_texcoord;'#10+
    'void main()'#10+
    '{'#10+
    '  vec4 c = texture2D(al_tex, varying_texcoord);'#10+
    '  int index = int(c.r * 255.0);'#10+
    '  if (index != 0) {;'#10+
    '    gl_FragColor = vec4(pal[index], 1);'#10+
    '  }'#10+
    '  else {;'#10+
    '    gl_FragColor = vec4(0, 0, 0, 0);'#10+
    '  };'#10+
    '}'#10
  );

  al_build_shader (Shader);
  al_use_shader (Shader);

  Timer := al_create_timer (1 / 60);
  Queue := al_create_event_queue;
  al_register_event_source (Queue, al_get_keyboard_event_source);
  al_register_event_source (Queue, al_get_display_event_source (Display));
  al_register_event_source (Queue, al_get_timer_event_source (Timer));
  al_start_timer (Timer);

  repeat
    al_wait_for_event (Queue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      EndExample := True;
    ALLEGRO_EVENT_KEY_CHAR:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then EndExample := True;
    ALLEGRO_EVENT_TIMER:
      begin
	Redraw := True;
	t := t + 1;
	for i := Low (Sprites) to High (Sprites) do
	begin
	  if Sprites[i].Flags <> 0 then Dir := 1 else Dir := -1;
	  sincos (Sprites[i].Angle, s, h);
	  Sprites[i].x := Sprites[i].x + h * 2 * Dir;
	  Sprites[i].y := Sprites[i].y + s * 2 * Dir;
	  Sprites[i].Angle := Sprites[i].Angle + ALLEGRO_PI / 180 * Dir
	end
      end;
    end;

    if Redraw and al_is_event_queue_empty (Queue) then
    begin
      Position := Trunc (t) mod 60 / 60;
      p1 := Trunc (t / 60) mod 3;
      p2 := (p1 + 1) mod 3;

      Redraw := False;
      al_clear_to_color (al_map_rgb_f (0, 0, 0));

      Pal := InterpolatePalette (Pals[p1 * 2], Pals[p2 * 2], Position);

      al_set_shader_float_vector ('pal', 3, @Pal, 256);
      if Background <> Nil then al_draw_bitmap (Background, 0, 0, 0);

      for i := Low (Sprites) to High (Sprites) do
      begin
	j := 7 - i;
	Position := (1 + sin ((t / 60 + Sprites[j].t) * 2 * ALLEGRO_PI)) / 2;
	Pal := InterpolatePalette (
		 Pals[Sprites[j].i], Pals[Sprites[j].j], Position
	       );
	al_set_shader_float_vector ('pal', 3, @Pal, 256);
	al_draw_rotated_bitmap (
	  Bitmap,
	  64, 64,
	  Sprites[j].x, Sprites[j].y, Sprites[j].Angle,
	  Sprites[j].Flags
	);
      end;

      sc := 0.5;
      if Trunc (t) mod 20 > 15 then i := 6 else i := 0;
      al_set_shader_float_vector ('pal', 3, @Pals[i], 256);
      al_draw_scaled_rotated_bitmap (Bitmap, 0, 0,   0,   0,  sc,  sc, 0, 0);
      al_draw_scaled_rotated_bitmap (Bitmap, 0, 0, 640,   0, -sc,  sc, 0, 0);
      al_draw_scaled_rotated_bitmap (Bitmap, 0, 0,   0, 480,  sc, -sc, 0, 0);
      al_draw_scaled_rotated_bitmap (Bitmap, 0, 0, 640, 480, -sc, -sc, 0, 0);

      al_flip_display
    end
  until EndExample;

  al_use_shader (Nil);

  al_destroy_bitmap (Bitmap);
  al_destroy_shader (Shader)
end.
