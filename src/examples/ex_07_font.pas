program ex_07font;
(* Show how to use text fonts. *)
(*
  Copyright (c) 2024 Guillermo Martínez J.

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
    allegro5, al5font, al5image, al5strings, al5ttf;

  const
  (* Window size. *)
    wWidth = 640; wHeight = 480;
  (* Text size. *)
    ttfTextSize = 22;

  var
  (* The event queue. *)
    EventQueue: ALLEGRO_EVENT_QUEUEptr;
  (* An event. *)
    Event: ALLEGRO_EVENT;
  (* Pointer to the window information. *)
    Window: ALLEGRO_DISPLAYptr;
  (* To store text fonts. *)
    SystemTextFont, BitmapTextFont, TTFTextFont: ALLEGRO_FONTptr;
  (* Colors used. *)
    clrBackground, clrText: ALLEGRO_COLOR;
  (* To know when to update the screen, when to stop. *)
    NeedRedraw, Terminated: Boolean;

(* Program Initialization. *)
  function Initialize: Boolean;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_init_image_addon
    or not al_init_font_addon or not al_init_ttf_addon
    then
    begin
      WriteLn ('Can''t initialize Allegro!');
      Exit (False)
    end;
  { Create window. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    Window := al_create_display (wWidth, wHeight);
    if not Assigned (Window) then
    begin
      WriteLn ('Can''t create window.');
      Exit (False)
    end;
    clrBackground := al_map_rgb (255, 255, 204);
    clrText := al_map_rgb (2, 2, 2);
  { Create text fonts. }
    SystemTextFont := al_create_builtin_font;
    BitmapTextFont := al_load_font ('data/font.tga', 0, 0);
    if not Assigned (BitmapTextFont) then
    begin
      ErrorMessage ('Error loading font.tga');
      Exit (False)
    end;
    TTFTextFont := al_load_font ('data/DejaVuSans.ttf', ttfTextSize, 0);
    if not Assigned (TTFTextFont) then
    begin
      ErrorMessage ('Error loading DejaVuSans.ttf');
      Exit (False)
    end;
  { Create the event queue. }
    EventQueue := al_create_event_queue;
    if not Assigned (EventQueue) then
    begin
      ErrorMessage ('Can''t initialize event queue!');
      Exit (False)
    end;
    al_register_event_source (EventQueue, al_get_keyboard_event_source);
    al_register_event_source (EventQueue, al_get_display_event_source (Window));

    Result := True
  end;



  procedure Finalize;

    procedure DestroyFont (aFont: ALLEGRO_FONTptr); inline;
    begin
      if Assigned (aFont) then al_destroy_font (aFont)
    end;

  begin
    if Assigned (EventQueue) then al_destroy_event_queue (EventQueue);
    DestroyFont (SystemTextFont);
    DestroyFont (BitmapTextFont);
    DestroyFont (TTFTextFont);
    if Assigned (Window) then al_destroy_display (Window)
  end;



(* Draw window content. *)
  procedure UpdateScreen;

    procedure PrintTextSample (
      const aName: String;
      aFont: ALLEGRO_FONTptr;
      aY: Integer
    );
    var
      lTextHeight: Integer;

      procedure PrintText (const aText: AnsiString); {$IfDef FPC}inline;{$EndIf}
      begin
        al_draw_text (
	  aFont, clrText,
	  4, aY, 0,
	  al_string_to_str (aText)
	);
	Inc (aY, lTextHeight)
      end;

    const
    { Note that this text is in UTF-8.  That's why it uses AnsiString
      instead of string.  Otherwise Delphi will try to use UnicodeString
      wich is UTF-16 and will mess all the text.
    }
      TextSamples: array [1..5] of AnsiString = (
        'abcdefghijklmnopqrstuvwxyz',
        'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
        '1234567890?!/()&%@{}áéíóúÀÈÌÒÜçÇ',
      { 'αβγδεζηθικλμνξοπρστυφχψω', }
        'Εύρηκα, το βρήκα!',
        '¿Las cigüeñas, en Andalucía, hacen nidos en los ríos?'
      );
    var
      lCnt: Integer;
    begin
      lTextHeight := al_get_font_line_height (aFont);
      PrintText (Concat ('Text sample for ', aName, ' text font.'));
      for lCnt := 1 to High (TextSamples) do
        PrintText (TextSamples[lCnt])
    end;

  begin
    al_clear_to_color (clrBackground);

    PrintTextSample ('Built-in', SystemTextFont, 4);
    PrintTextSample ('Bitmap', BitmapTextFont, 84);
    PrintTextSample ('True Type', TTFTextFont, 254);

  { Done. }
    al_flip_display;
    NeedRedraw := False
  end;

begin
  if not Initialize then Exit;
{ "Game loop". }
  Terminated := False;
  NeedRedraw := True;
  repeat
    if NeedRedraw and al_is_event_queue_empty (EventQueue) then UpdateScreen;
    al_wait_for_event (EventQueue, @Event);
    case Event.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Terminated := True;
    ALLEGRO_EVENT_KEY_DOWN:
      if Event.keyboard.keycode = ALLEGRO_KEY_ESCAPE then Terminated := True;
    end
  until Terminated;
{ Program finalization. }
  Finalize
end.
