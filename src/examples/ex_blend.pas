PROGRAM ex_blend;
(* An example demonstrating different blending modes. *)
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
{$ENDIF}

uses
  Common,
  Allegro5, al5base, al5font, al5image, al5primitives, al5strings;


var
(* A structure holding all variables of our example program. *)
  Ex: record
   Example: ALLEGRO_BITMAPptr; (* Our example bitmap. *)
   OffScreen: ALLEGRO_BITMAPptr; (* An offscreen buffer, for testing. *)
   Memory: ALLEGRO_BITMAPptr; (* A memory buffer, for testing. *)
   MyFont: ALLEGRO_FONTptr; (* Our font. *)
   Queue: ALLEGRO_EVENT_QUEUEptr; (* Our events queue. *)
   Image: Integer; (* Which test image to use. *)
   Mode: Integer; (* How to draw it. *)
   ButtonsX: Integer; (* Where to draw buttons. *)

   FPS: Integer;
   LastSecond: Double;
   FramesAccum: Integer;
   fFPS: Double;
  end;



(* Print some text with a shadow. *)
  procedure Print
    (const aX, aY: Integer; const aVertical: Boolean; const Text: AL_STR);
  var
    Color: ALLEGRO_COLOR;
    h, i, j: Integer;
    ui, Letter: ALLEGRO_USTR_INFO;
    us: ALLEGRO_USTRptr;
  begin
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    h := al_get_font_line_height (Ex.MyFont);

    for j := 0 to 1 do
    begin
      if j = 0 then
        Color := al_map_rgb (0, 0, 0)
      else
        Color := al_map_rgb (255, 255, 255);

      if aVertical then
      begin
        us := al_ref_cstr (ui, Text);
        for i := 0 to al_ustr_length (us) - 1 do
        begin
          al_draw_ustr (
            Ex.MyFont, Color, aX + 1 - j, aY + 1 - j + h * i, 0,
            al_ref_ustr (
              Letter, us, al_ustr_offset (us, i),
              al_ustr_offset (us, i + 1)
            )
          )
        end
      end
      else
        al_draw_text (Ex.MyFont, Color, aX + 1 - j, aY + 1 - j, 0, Text)
    end
  end;



(* Create an example bitmap. *)
  function CreateExampleBitmap: ALLEGRO_BITMAPptr;
  var
    lBmp: ALLEGRO_BITMAPptr;
    i, j, x, y, r: Integer;
    rc: Double;
    Locked: ALLEGRO_LOCKED_REGIONptr;
    Data: PBYTE;
  begin
    lBmp := al_create_bitmap (100, 100);
    Locked := al_lock_bitmap (
      lBmp, ALLEGRO_PIXEL_FORMAT_ABGR_8888, ALLEGRO_LOCK_WRITEONLY
    );
    Data := Locked^.data;

    for j := 0 to 99 do
    begin
      for i := 0 to 99 do
      begin
        x := i - 50; y := j - 50;
        r := Trunc (sqrt (x * x + y * y));
        rc := 1 - r / 50.0;
        if rc < 0 then rc := 0;
        Data[i * 4 + 0] := Trunc (i * 255 / 100);
        Data[i * 4 + 1] := Trunc (j * 255 / 100);
        Data[i * 4 + 2] := Trunc (rc * 255);
        Data[i * 4 + 3] := Trunc (rc * 255);
      end;
      Data := Data + Locked^.pitch;
    end;
    al_unlock_bitmap (lBmp);
    Exit (lBmp)
  end;



(* Draw our example scene. *)
  procedure Draw;

    function mIs (a, b: Integer): AL_STR; inline;
    begin
      if a = b then mIs := '*' else mIs := ' '
    end;

  const
    BlendNames: array [0..3] of AL_STR = ('ZERO', 'ONE', 'ALPHA', 'INVERSE');
    BlendVNnames: array [0..3] of AL_STR = ('ZERO', 'ONE', 'ALPHA', 'INVER');
    BlendModes: array [0..3] of ALLEGRO_BLEND_MODE = (
      ALLEGRO_ZERO, ALLEGRO_ONE, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA
    );
   var
    Test: array [0..4] of ALLEGRO_COLOR;
    Target: ALLEGRO_BITMAPptr;
    x, y: Single;
    i, j: Integer;
   begin
    Target := al_get_target_bitmap;
    x := 40; y := 40;

    al_clear_to_color (al_map_rgb_f (0.5, 0.5, 0.5));

    Test[0] := al_map_rgba_f (1, 1, 1, 1);
    Test[1] := al_map_rgba_f (1, 1, 1, 0.5);
    Test[2] := al_map_rgba_f (1, 1, 1, 0.25);
    Test[3] := al_map_rgba_f (1, 0, 0, 0.75);
    Test[4] := al_map_rgba_f (0, 0, 0, 0);

    Print (Trunc (x), 0, False, al_str_format (
      'D  E  S  T  I  N  A  T  I  O  N  (%0.2f fps)', [Ex.fFPS])
    );
    Print (0, Trunc (y), True, 'S O U R C E');
    for i := Low (BlendNames) to High (BlendNames) do
    begin
      Print (Trunc (x + i * 110), 20, False, BlendNames[i]);
      Print (20, Trunc (y + i * 110), True, BlendVNnames[i])
    end;

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
    if (Ex.Mode >= 1) and (Ex.Mode <= 5) then
    begin
      al_set_target_bitmap (Ex.OffScreen);
      al_clear_to_color(test[Ex.Mode - 1])
    end;
    if (Ex.Mode >= 6) and (Ex.Mode <= 10) then
    begin
      al_set_target_bitmap (Ex.Memory);
      al_clear_to_color (Test[Ex.Mode - 6])
    end;

    for j := Low (BlendModes) to High (BlendModes) do
    begin
      for i := Low (BlendModes) to High (BlendModes) do
      begin
        al_set_blender (ALLEGRO_ADD, BlendModes[j], BlendModes[i]);
        if Ex.Image = 0 then
          al_draw_bitmap (Ex.Example, x + i * 110, y + j * 110, 0)
        else if (Ex.Image >= 1) and (Ex.Image <= 6) then
          al_draw_filled_rectangle (
            x + i * 110,       y + j * 110,
            x + i * 110 + 100, y + j * 110 + 100,
            Test[Ex.Image - 1]
          )
      end
    end;

    if (Ex.Mode >= 1) and (Ex.Mode <= 5) then
    begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_set_target_bitmap (Target);
      al_draw_bitmap_region (Ex.OffScreen, x, y, 430, 430, x, y, 0)
    end;
    if (Ex.Mode >= 6) and (Ex.Mode <= 10) then
    begin
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_set_target_bitmap (Target);
      al_draw_bitmap_region (Ex.Memory, x, y, 430, 430, x, y, 0)
    end;

    Print (Ex.ButtonsX, 20 * 1, False, 'What to draw');
    Print (Ex.ButtonsX, 20 * 2, False, mIs (Ex.Image, 0)+' Picture');
    Print (Ex.ButtonsX, 20 * 3, False, mIs (Ex.Image, 1)+' Rec1 (1/1/1/1)');
    Print (Ex.ButtonsX, 20 * 4, False, mIs (Ex.Image, 2)+' Rec2 (1/1/1/.5)');
    Print (Ex.ButtonsX, 20 * 5, False, mIs (Ex.Image, 3)+' Rec3 (1/1/1/.25)');
    Print (Ex.ButtonsX, 20 * 6, False, mIs (Ex.Image, 4)+' Rec4 (1/0/0/.75)');
    Print (Ex.ButtonsX, 20 * 7, False, mIs (Ex.Image, 5)+' Rec5 (0/0/0/0)');

    Print (Ex.ButtonsX, 20 * 9, False, 'Where to draw');
    Print (Ex.ButtonsX, 20 * 10, False, mIs (Ex.Mode, 0)+' screen');

    Print (Ex.ButtonsX, 20 * 11, False, mIs (Ex.Mode, 1)+' offscreen1');
    Print (Ex.ButtonsX, 20 * 12, False, mIs (Ex.Mode, 2)+' offscreen2');
    Print (Ex.ButtonsX, 20 * 13, False, mIs (Ex.Mode, 3)+' offscreen3');
    Print (Ex.ButtonsX, 20 * 14, False, mIs (Ex.Mode, 4)+' offscreen4');
    Print (Ex.ButtonsX, 20 * 15, False, mIs (Ex.Mode, 5)+' offscreen5');

    Print (Ex.ButtonsX, 20 * 16, False, mIs (Ex.Mode, 6)+' memory1');
    Print (Ex.ButtonsX, 20 * 17, False, mIs (Ex.Mode, 7)+' memory2');
    Print (Ex.ButtonsX, 20 * 18, False, mIs (Ex.Mode, 8)+' memory3');
    Print (Ex.ButtonsX, 20 * 19, False, mIs (Ex.Mode, 9)+' memory4');
    Print (Ex.ButtonsX, 20 * 20, False, mIs (Ex.Mode, 10)+' memory5')
  end;



(* Called a fixed amount of times per second. *)
  procedure Tick;
  var
    t: Double;
  begin
  { Count frames during the last second or so. }
    t := al_get_time ();
    if t >= Ex.LastSecond + 1 then
    begin
      Ex.fFPS := Ex.FramesAccum / (t - Ex.LastSecond);
      Ex.FramesAccum := 0;
      Ex.LastSecond := t
    end;

    draw;
    al_flip_display;
    Ex.FramesAccum := Ex.FramesAccum + 1
  end;



(* Run our test. *)
  procedure run;
  var
    Event: ALLEGRO_EVENT;
    x, y: Single;
    NeedDraw: Boolean;
  begin
    NeedDraw := True;

    repeat
    { Perform frame skipping so we don't fall behind the timer events. }
      if NeedDraw and al_is_event_queue_empty (Ex.Queue) then
      begin
        Tick;
        NeedDraw := False
      end;

      al_wait_for_event (ex.Queue, @Event);

      case Event.ftype OF
      { Was the X button on the window pressed? }
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        Exit;
      { Was a key pressed? }
      ALLEGRO_EVENT_KEY_DOWN:
        if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Exit;
      { Is it time for the next timer tick? }
      ALLEGRO_EVENT_TIMER:
        NeedDraw := true;
      { Mouse click? }
      ALLEGRO_EVENT_MOUSE_BUTTON_UP:
        begin
          x := Event.mouse.x;
          y := Event.mouse.y;
          if x >= Ex.ButtonsX then
          begin
            case Trunc (y / 20) OF
              2: Ex.image := 0;
              3: Ex.image := 1;
              4: Ex.image := 2;
              5: Ex.image := 3;
              6: Ex.image := 4;
              7: Ex.image := 5;

              10: Ex.mode := 0;

              11: Ex.mode := 1;
              12: Ex.mode := 2;
              13: Ex.mode := 3;
              14: Ex.mode := 4;
              15: Ex.mode := 5;

              16: Ex.mode := 6;
              17: Ex.mode := 7;
              18: Ex.mode := 8;
              19: Ex.mode := 9;
              20: Ex.mode := 10;
            end
          end
        end;
      end
    until False
  end;



(* Initialize the example. *)
  procedure Init;
  begin
   Ex.ButtonsX := 40 + 110 * 4;
   Ex.FPS := 60;

   Ex.MyFont := al_load_font ('data/font.tga', 0, 0);
   if Ex.MyFont = Nil then
     AbortExample ('data/font.tga not found');
   Ex.Example := CreateExampleBitmap;

   Ex.OffScreen := al_create_bitmap (640, 480);
   al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
   Ex.Memory := al_create_bitmap (640, 480)
  end;



var
  Display: ALLEGRO_DISPLAYptr;
  Timer: ALLEGRO_TIMERptr;
begin
  if not al_init then AbortExample ('Could not init Allegro.');

   al_init_primitives_addon;
   al_install_keyboard;
   al_install_mouse;
 {  al_install_touch_input; }
   al_init_image_addon;
   al_init_font_addon;
   InitPlatformSpecific;

   Display := al_create_display (640, 480);
   if display = Nil then  AbortExample ('Error creating display.');

   Init;

{ Ignore message:
ex_blend.pas(364,36) Warning: Variable "Ex" does not seem to be initialized
  It is initialized at "Init". }
   Timer := al_create_timer (1.0 / Ex.FPS);

   Ex.Queue := al_create_event_queue;
   al_register_event_source (Ex.Queue, al_get_keyboard_event_source);
   al_register_event_source (Ex.Queue, al_get_mouse_event_source);
   al_register_event_source (Ex.Queue, al_get_display_event_source (Display));
   al_register_event_source (Ex.Queue, al_get_timer_event_source (Timer));
{ TODO:
   if al_is_touch_input_installed then
     al_register_event_source (
       Ex.Queue, al_get_touch_input_mouse_emulation_event_source
     );
}

   al_start_timer (Timer);
   Run;

   al_destroy_event_queue (Ex.Queue)
end.
