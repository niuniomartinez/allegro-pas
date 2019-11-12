UNIT NihGUI;
(* This is a GUI for example programs that require GUI-style interaction. *)
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

{$IFDEF FPC}{$MODE OBJFPC}{$ENDIF}

INTERFACE

  USES
{$IFDEF FPC}
    fgl,
{$ELSE}
    Generics.Collections,
{$ENDIF}
    Allegro5, al5font, Classes;

  TYPE
  (* Forward declaration. *)
    TDialog = CLASS;



  (* Theme. *)
    TTheme = CLASS (TObject)
    PRIVATE
      fBg, fFg, fHighlight: ALLEGRO_COLOR;
      fFont: ALLEGRO_FONTptr;
    PUBLIC
    (* Nil font is fine if you don't use widget that requires text. *)
      CONSTRUCTOR Create (aFont: ALLEGRO_FONTptr = NIL);

      PROPERTY bg: ALLEGRO_COLOR READ fBg WRITE fBg;
      PROPERTY fg: ALLEGRO_COLOR READ fFg WRITE fFg;
      PROPERTY Highlight: ALLEGRO_COLOR READ fHighlight WRITE fHighlight;
      PROPERTY Font: ALLEGRO_FONTptr READ fFont;
    END;



  (* Base for widgets. *)
    TWidget = CLASS (TObject)
    PRIVATE
      fGridX, fGridY, fGridW, fGridH: INTEGER;
      fDisabled: BOOLEAN;

      FUNCTION GetWidth: LONGWORD;
      FUNCTION GetHeight: LONGWORD;
    PROTECTED
      fDialog: TDialog;
      fX1, fY1, fX2, fY2: INTEGER;

      FUNCTION GetDisabled: BOOLEAN; VIRTUAL;
      PROCEDURE SetDisabled (Value: BOOLEAN); VIRTUAL;
    PUBLIC
      CONSTRUCTOR CreateWidget;

      PROCEDURE Configure (axSize, aySize, axPadding, ayPadding: INTEGER);
      FUNCTION Contains (aX, aY: INTEGER): BOOLEAN; VIRTUAL;

      FUNCTION WantMouseFocus: BOOLEAN; VIRTUAL;
      PROCEDURE GotMouseFocus; VIRTUAL;
      PROCEDURE LostMouseFocus; VIRTUAL;
      PROCEDURE onMouseButtonDown (mx, my: INTEGER); VIRTUAL;
      PROCEDURE onMouseButtonHold (mx, my: INTEGER); VIRTUAL;
      PROCEDURE onMouseButtonUp (mx, my: INTEGER); VIRTUAL;
      PROCEDURE onClick (mx, my: INTEGER); VIRTUAL;

      FUNCTION WantKeyFocus: BOOLEAN; VIRTUAL;
      PROCEDURE GotKeyFocus; VIRTUAL;
      PROCEDURE LostKeyFocus; VIRTUAL;
      PROCEDURE onKeyDown (CONST Event: ALLEGRO_KEYBOARD_EVENT); VIRTUAL;

      PROCEDURE Draw; VIRTUAL; ABSTRACT;

      PROPERTY Width: LONGWORD READ GetWidth;
      PROPERTY Height: LONGWORD READ GetHeight;
      PROPERTY Disabled: BOOLEAN READ GetDisabled WRITE SetDisabled;
    END;



    TEventHandler = CLASS (TObject)
    PUBLIC
      PROCEDURE HandleEvent (CONST Event: ALLEGRO_EVENT); VIRTUAL; ABSTRACT;
    END;



  (* The dialog. *)
    TDialog = CLASS (TObject)
    PRIVATE
      fTheme: TTheme;
      fDisplay: ALLEGRO_DISPLAYptr;
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fGridM, fGridN, fxPadding, fyPadding: INTEGER;

      fDrawRequested, fQuitRequested: BOOLEAN;

    {$IFDEF FPC}
      TYPE TWidgetList = SPECIALIZE TFPGObjectList<TWidget>;
     PRIVATE
      fAllWidgets: TWidgetList;
    {$ELSE}
      fAllWidgets: TObjectList<TWidget>;
    {$ENDIF}
      fMouseOverWidget, fMouseDownWidget, fKeyWidget: TWidget;

      fEventHandler: TEventHandler;

      PROCEDURE ConfigureAll;
      PROCEDURE onKeyDown (CONST aEvent: ALLEGRO_KEYBOARD_EVENT);
      PROCEDURE onMouseAxes (CONST aEvent: ALLEGRO_MOUSE_EVENT);
      PROCEDURE CheckMouseOver (mx, my: INTEGER);
      PROCEDURE onMouseButtonDown (CONST aEvent: ALLEGRO_MOUSE_EVENT);
      PROCEDURE onMouseButtonUp (CONST aEvent: ALLEGRO_MOUSE_EVENT);
    PUBLIC
      CONSTRUCTOR CreateDialog (
	aTheme: TTheme;
	aDisplay: ALLEGRO_DISPLAYptr;
	aGridM, aGridN: INTEGER
      );
      DESTRUCTOR Destroy; OVERRIDE;

      PROCEDURE SetPadding (axPadding, ayPadding: INTEGER);
      PROCEDURE Add
	(aWidget: TWidget; aGridX, aGridY, aGridW, aGridH: INTEGER);
      PROCEDURE Prepare;
      PROCEDURE RunStep (aBlock: BOOLEAN);
      PROCEDURE RequestQuit;
      PROCEDURE RequestDraw;
      PROCEDURE Draw;

      PROCEDURE RegisterEventSource (aSource: ALLEGRO_EVENT_SOURCEptr);
      PROCEDURE SetEventHandler (aEventHandler: TEventHandler);

      PROPERTY Theme: TTheme READ fTheme;
      PROPERTY QuitRequested: BOOLEAN READ fQuitRequested;
      PROPERTY DrawRequested: BOOLEAN READ fDrawRequested;
    END;



    TLabel = CLASS (TWidget)
    PRIVATE
      fText: STRING;
      fCentred: BOOLEAN;
    PUBLIC
      CONSTRUCTOR CreateLabel (aText: STRING = ''; aCentred: BOOLEAN = TRUE);
      PROCEDURE Draw; OVERRIDE;
      FUNCTION WantMouseFocus: BOOLEAN; OVERRIDE;
    END;



    TButton = CLASS (TWidget)
    PROTECTED
      fText: STRING;
      fPushed: BOOLEAN;
    PUBLIC
      CONSTRUCTOR CreateButton (aText: STRING);
      PROCEDURE onMouseButtonDown (mx, my: INTEGER); OVERRIDE;
      PROCEDURE onMouseButtonUp   (mx, my: INTEGER); OVERRIDE;
      PROCEDURE Draw; OVERRIDE;

      PROPERTY Pushed: BOOLEAN READ fPushed;
    END;



    TToggleButton = CLASS (TButton)
    PUBLIC
      CONSTRUCTOR CreateToggleButton (aText: STRING);
      PROCEDURE onMouseButtonDown (mx, my: INTEGER); OVERRIDE;
      PROCEDURE onMouseButtonUp   (mx, my: INTEGER); OVERRIDE;

      PROCEDURE SetPushed (aPushed: BOOLEAN);
    END;



    TList = CLASS (TWidget)
    PRIVATE
      fItems: TStringList;
      fSelectedItem: LONGINT;

      FUNCTION GetSelectedItemText: STRING;
    PUBLIC
      CONSTRUCTOR CreateList (InitialSelection: INTEGER = 0);
      DESTRUCTOR Destroy; OVERRIDE;
      FUNCTION WantKeyFocus: BOOLEAN; OVERRIDE;
      PROCEDURE onKeyDown (CONST Event: ALLEGRO_KEYBOARD_EVENT); OVERRIDE;
      PROCEDURE onClick (mx, my: INTEGER); OVERRIDE;
      PROCEDURE Draw; OVERRIDE;

      PROCEDURE ClearItems;
      PROCEDURE AppendItem (aText: STRING);

      PROPERTY SelectedItemText: STRING READ GetSelectedItemText;
      PROPERTY SelectedItem: LONGINT READ fSelectedItem;
    END;



    TCustomSlider = CLASS (TWidget)
    PROTECTED
      fCurValue, fMaxValue: INTEGER;
    PUBLIC
      CONSTRUCTOR CreateSlider (aCurValue: INTEGER=0; aMaxValue: INTEGER=0);
      PROCEDURE onMouseButtonDown (mx, my: INTEGER); OVERRIDE;

      PROPERTY MaxValue: INTEGER READ fMaxValue;
      PROPERTY CurValue: INTEGER READ fCurValue WRITE fCurValue;
    END;



    TVSlider = CLASS (TCustomSlider)
    PUBLIC
      PROCEDURE onMouseButtonHold (mx, my: INTEGER); OVERRIDE;
      PROCEDURE Draw; OVERRIDE;
    END;



    THSlider = CLASS (TCustomSlider)
    PUBLIC
      PROCEDURE onMouseButtonHold (mx, my: INTEGER); OVERRIDE;
      PROCEDURE Draw; OVERRIDE;
    END;



    TTextEntry = CLASS (TWidget)
    PRIVATE
      fText: ALLEGRO_USTRptr;
      fFocused: BOOLEAN;
      fCursorPos, fLeftPos: INTEGER;

      PROCEDURE MaybeScroll;
      FUNCTION GetText: STRING;
    PUBLIC
      CONSTRUCTOR CreateTextEntry (aInitialText: STRING = '');
      DESTRUCTOR Destroy; OVERRIDE;

      FUNCTION WantKeyFocus: BOOLEAN; OVERRIDE;
      PROCEDURE GotKeyFocus; OVERRIDE;
      PROCEDURE LostKeyFocus; OVERRIDE;
      PROCEDURE onKeyDown (CONST Event: ALLEGRO_KEYBOARD_EVENT); OVERRIDE;
      PROCEDURE Draw; OVERRIDE;
    END;

IMPLEMENTATION

  USES
    al5primitives, sysutils;

  TYPE
    TSaveState = CLASS (TObject)
    PRIVATE
      State: ALLEGRO_STATE;
    PUBLIC
      CONSTRUCTOR Create (CONST Save: INTEGER = ALLEGRO_STATE_ALL);
      DESTRUCTOR Destroy; OVERRIDE;
    END;



    TUString = CLASS (TObject)
    PRIVATE
      Info: ALLEGRO_USTR_INFO;
      fUStr: ALLEGRO_USTRptr;
    PUBLIC
      CONSTRUCTOR Create
        (s: ALLEGRO_USTRptr; aFirst: INTEGER; aEnd: INTEGER = -1);

      PROPERTY UStr: ALLEGRO_USTRptr READ fUStr;
    END;



  FUNCTION CLAMP (v1, v2, v3: DOUBLE): DOUBLE;
  BEGIN
    IF v1 > v2 THEN EXIT (v1);
    IF v2 > v3 THEN EXIT (v3);
    EXIT (v2)
  END;



(*
 * TTheme
 ***************************************************************************)

(* Costructor. *)
  CONSTRUCTOR TTheme.Create (aFont: ALLEGRO_FONTptr);
  BEGIN
    INHERITED Create;
    fBg := al_map_rgb (255, 255, 255);
    fFg := al_map_rgb (0, 0, 0);
    fHighlight := al_map_rgb (128, 128, 128);
    fFont := aFont
  END;



(*
 * TWidget
 ***************************************************************************)

  FUNCTION TWidget.GetWidth: LONGWORD;
  BEGIN
    RESULT := fX2 - fX1
  END;

  FUNCTION TWidget.GetHeight: LONGWORD;
  BEGIN
    RESULT := fY2 - fY1
  END;



  FUNCTION TWidget.GetDisabled: BOOLEAN; BEGIN RESULT := fDisabled END;



  PROCEDURE TWidget.SetDisabled (Value: BOOLEAN);
  BEGIN
    fDisabled := Value
  END;



(* Constructor. *)
  CONSTRUCTOR TWidget.CreateWidget;
  BEGIN
    INHERITED Create;
    fGridX := 0; fGridY := 0; fGridW := 0; fGridH := 0;
    fDialog := NIL;
    fX1 := 0; fY1 := 0; fX2 := 0; fY2 := 0;
    fDisabled := FALSE
  END;



  PROCEDURE TWidget.Configure (axSize, aySize, axPadding, ayPadding: INTEGER);
  BEGIN
    fX1 := axSize * fGridX + axPadding;
    fY1 := aySize * fGridY + ayPadding;
    fX2 := axSize * (fGridX + fGridW) - axPadding - 1;
    fY2 := aySize * (fGridY + fGridH) - ayPadding - 1
  END;



  FUNCTION TWidget.Contains (aX, aY: INTEGER): BOOLEAN;
  BEGIN
    RESULT := (aX >= fX1) AND (aY >= fY1) AND (aX <= fX2) AND (aY <= fY2)
  END;



  FUNCTION TWidget.WantMouseFocus: BOOLEAN; BEGIN RESULT := TRUE END;

  PROCEDURE TWidget.GotMouseFocus; BEGIN END;

  PROCEDURE TWidget.LostMouseFocus; BEGIN END;

  PROCEDURE TWidget.onMouseButtonDown (mx, my: INTEGER); BEGIN mx := my END;

  PROCEDURE TWidget.onMouseButtonHold (mx, my: INTEGER); BEGIN mx := my END;

  PROCEDURE TWidget.onMouseButtonUp (mx, my: INTEGER); BEGIN mx := my END;

  PROCEDURE TWidget.onClick (mx, my: INTEGER); BEGIN mx := my END;



  FUNCTION TWidget.WantKeyFocus: BOOLEAN; BEGIN RESULT := TRUE END;

  PROCEDURE TWidget.GotKeyFocus; BEGIN END;

  PROCEDURE TWidget.LostKeyFocus; BEGIN END;

  PROCEDURE TWidget.onKeyDown (CONST Event: ALLEGRO_KEYBOARD_EVENT); BEGIN END;



(*
 * TDialog
 ***************************************************************************)

  PROCEDURE TDialog.ConfigureAll;
  VAR
    lxSize, lySize: INTEGER;
    lWidget: TWidget;
  BEGIN
    lxSize := TRUNC (al_get_display_width (fDisplay) / fGridM);
    lySize := TRUNC (al_get_display_height (fDisplay) / fGridN);

    FOR lWidget IN SELF.fAllWidgets DO
      lWidget.Configure (lxSize, lySize, fxPadding, fyPadding)
  END;



  PROCEDURE TDialog.onKeyDown (CONST aEvent: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    IF aEvent.display <> SELF.fDisplay THEN EXIT;
  { XXX think of something better when we need it }
    IF aEvent.keycode = ALLEGRO_KEY_ESCAPE THEN SELF.RequestQuit;
    IF fKeyWidget <> NIL THEN fKeyWidget.onKeyDown (aEvent)
  END;



  PROCEDURE TDialog.onMouseAxes (CONST aEvent: ALLEGRO_MOUSE_EVENT);
  BEGIN
    IF aEvent.display <> SELF.fDisplay THEN EXIT;
    IF fMouseDownWidget <> NIL THEN
      fMouseDownWidget.onMouseButtonHold (aEvent.x, aEvent.y)
    ELSE
      SELF.CheckMouseOver (aEvent.x, aEvent.y)
  END;



  PROCEDURE TDialog.CheckMouseOver (mx, my: INTEGER);
  VAR
    lWidget: TWidget;
  BEGIN
    IF (fMouseDownWidget <> NIL) AND fMouseDownWidget.Contains (mx, my) THEN
    {* no change *}
      EXIT;

    FOR lWidget IN SELF.fAllWidgets DO
      IF lWidget.Contains (mx, my) AND lWidget.WantMouseFocus THEN
      BEGIN
	fMouseDownWidget := lWidget;
	fMouseDownWidget.GotMouseFocus;
	EXIT
      END;
    IF fMouseOverWidget <> NIL THEN
    BEGIN
      fMouseOverWidget.LostMouseFocus;
      fMouseOverWidget := NIL
    END
  END;



  PROCEDURE TDialog.onMouseButtonDown (CONST aEvent: ALLEGRO_MOUSE_EVENT);
  BEGIN
    IF aEvent.button <> 1 THEN EXIT;
  { With touch input we may not receive mouse axes event before the touch
    so we must check which widget the touch is over.
  }
    SELF.CheckMouseOver (aEvent.x, aEvent.y);
    IF fMouseOverWidget = NIL THEN EXIT;

    fMouseDownWidget := fMouseOverWidget;
    fMouseDownWidget.onMouseButtonDown (aEvent.x, aEvent.y);

  { transfer key focus }
    IF fMouseDownWidget <> fKeyWidget THEN
    BEGIN
      IF fKeyWidget <> NIL THEN
      BEGIN
	fKeyWidget.LostKeyFocus;
	fKeyWidget := NIL
      END;
      IF fMouseDownWidget.WantKeyFocus THEN
      BEGIN
	fKeyWidget := fMouseDownWidget;
	fKeyWidget.GotKeyFocus
      END
    END
  END;



  PROCEDURE TDialog.onMouseButtonUp (CONST aEvent: ALLEGRO_MOUSE_EVENT);
  BEGIN
    IF aEvent.button <> 1 THEN EXIT;
    IF fMouseDownWidget = NIL THEN EXIT;

    fMouseDownWidget.onMouseButtonUp (aEvent.x, aEvent.y);
    IF fMouseDownWidget.Contains (aEvent.x, aEvent.y) THEN
      fMouseDownWidget.onClick (aEvent.x, aEvent.y);
    fMouseDownWidget := NIL
  END;



  CONSTRUCTOR TDialog.CreateDialog (
	aTheme: TTheme;
	aDisplay: ALLEGRO_DISPLAYptr;
	aGridM, aGridN: INTEGER
  );
  BEGIN
    INHERITED Create;
    fTheme := aTheme;
    fDisplay := aDisplay;
    fGridM := aGridM; fGridN := aGridN;
    fxPadding := 1; fyPadding := 1;

    fDrawRequested := TRUE;
    fQuitRequested := FALSE;
    fMouseOverWidget := NIL; fMouseDownWidget := NIL;
    fKeyWidget := NIL;

    fEventHandler := NIL;

    fEventQueue := al_create_event_queue;
    al_register_event_source (fEventQueue, al_get_keyboard_event_source);
    al_register_event_source (fEventQueue, al_get_mouse_event_source);
    al_register_event_source (
      fEventQueue,
      al_get_display_event_source (fDisplay)
    );
  { Disabled because it isn't implemented (yet).
    IF al_is_touch_input_installed THEN
      al_register_event_source (
	fEventQueue,
	al_get_touch_input_mouse_emulation_event_source
      );
  }
  {$IFDEF FPC}
    fAllWidgets := TWidgetList.Create;
  {$ELSE}
    fAllWidgets := TObjectList<TWidget>.Create;
  {$ENDIF}
  END;



  DESTRUCTOR TDialog.Destroy;
  BEGIN
    fAllWidgets.Free;
    fDisplay := NIL;
    al_destroy_event_queue (fEventQueue);
    INHERITED Destroy
  END;



  PROCEDURE TDialog.SetPadding (axPadding, ayPadding: INTEGER);
  BEGIN
    fxPadding := axPadding; fyPadding := ayPadding
  END;



  PROCEDURE TDialog.Add
	(aWidget: TWidget; aGridX, aGridY, aGridW, aGridH: INTEGER);
  BEGIN
    aWidget.fGridX := aGridX; aWidget.fGridY := aGridY;
    aWidget.fGridW := aGridW; aWidget.fGridH := aGridH;
    SELF.fAllWidgets.Add (aWidget);
    aWidget.fDialog := SELF
  END;



  PROCEDURE TDialog.Prepare;
  VAR
    mst: ALLEGRO_MOUSE_STATE;
  BEGIN
    SELF.ConfigureAll;
  { XXX this isn't working right in X.  The mouse position is reported as
    (0,0) initially, until the mouse pointer is moved.
  }
    al_get_mouse_state (mst);
    SELF.CheckMouseOver (mst.x, mst.y)
  END;



  PROCEDURE TDialog.RunStep (aBlock: BOOLEAN);
  VAR
    lEvent: ALLEGRO_EVENT;
  BEGIN
    IF aBlock THEN  al_wait_for_event (fEventQueue, NIL);

    WHILE al_get_next_event (fEventQueue, lEvent) DO
    BEGIN
      CASE lEvent.ftype OF
      ALLEGRO_EVENT_DISPLAY_CLOSE:
	SELF.RequestQuit;
      ALLEGRO_EVENT_KEY_CHAR:
	SELF.onKeyDown (lEvent.keyboard);
      ALLEGRO_EVENT_MOUSE_AXES:
	SELF.onMouseAxes (lEvent.mouse);
      ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
	SELF.onMouseButtonDown (lEvent.mouse);
      ALLEGRO_EVENT_MOUSE_BUTTON_UP:
	SELF.onMouseButtonUp (lEvent.mouse);
      ALLEGRO_EVENT_DISPLAY_EXPOSE:
	SELF.RequestDraw;
      ELSE
	IF Assigned (fEventHandler) THEN fEventHandler.HandleEvent (levent);
      END
    END
  END;



  PROCEDURE TDialog.RequestQuit; BEGIN fQuitRequested := TRUE END;



  PROCEDURE TDialog.RequestDraw; BEGIN fDrawRequested := TRUE END;



  PROCEDURE TDialog.Draw;
  VAR
    cx, cy, cw, ch: INTEGER;
    lWidget: TWidget;
  BEGIN
    al_get_clipping_rectangle (cx, cy, cw, ch);

    FOR lWidget IN SELF.fAllWidgets DO
    BEGIN
      al_set_clipping_rectangle (
	lWidget.fX1, lWidget.fY1, lWidget.Width, lWidget.Height
      );
      lWidget.Draw
    END;
    al_set_clipping_rectangle (cx, cy, cw, ch);
    SELF.fDrawRequested := FALSE
  END;



  PROCEDURE TDialog.RegisterEventSource (aSource: ALLEGRO_EVENT_SOURCEptr);
  BEGIN
    al_register_event_source (fEventQueue, aSource)
  END;



  PROCEDURE TDialog.SetEventHandler (aEventHandler: TEventHandler);
  BEGIN
    fEventHandler := aEventHandler
  END;



(*
 * TLabel
 ***************************************************************************)

  CONSTRUCTOR TLabel.CreateLabel (aText: STRING; aCentred: BOOLEAN);
  BEGIN
    INHERITED CreateWidget;
    fText := aText;
    fCentred := aCentred
  END;



  PROCEDURE TLabel.Draw;
  VAR
    lState: TSaveState;
    fg: ALLEGRO_COLOR;
  BEGIN
    lState := TSaveState.Create;
    TRY
      IF SELF.Disabled THEN
	fg := al_map_rgb (64, 64, 64)
      ELSE
	fg := SELF.fDialog.Theme.fg;
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      IF fCentred THEN
	al_draw_text (
	  fDialog.Theme.Font, fg,
	  (SELF.fX1 + SELF.fX2 + 1) / 2, SELF.fY1,
	  ALLEGRO_ALIGN_CENTRE,
	  fText
	)
      ELSE
	al_draw_text (
	  fDialog.Theme.Font, fg,
	  SELF.fX1, SELF.fY1,
	  0,
	  fText
	)
    FINALLY
      lState.Free
    END
  END;



  FUNCTION TLabel.WantMouseFocus: BOOLEAN; BEGIN RESULT := FALSE END;



(*
 * TButton
 ***************************************************************************)

  CONSTRUCTOR TButton.CreateButton (aText: STRING);
  BEGIN
    INHERITED CreateWidget;
    fText := aText;
    fPushed := FALSE
  END;



  PROCEDURE TButton.onMouseButtonDown (mx, my: INTEGER);
  BEGIN
    IF SELF.Disabled THEN EXIT;
    fPushed := TRUE;
    fDialog.RequestDraw
  END;



  PROCEDURE TButton.onMouseButtonUp (mx, my: INTEGER);
  BEGIN
    IF SELF.Disabled THEN EXIT;
    fPushed := FALSE;
    fDialog.RequestDraw
  END;



  PROCEDURE TButton.Draw;
  VAR
    lState: TSaveState;
    lfg, lbg: ALLEGRO_COLOR;
    y: DOUBLE;
  BEGIN
    lState := TSaveState.Create;
    TRY
      IF fPushed THEN
      BEGIN
	lfg := fDialog.Theme.bg;
	lbg := fDialog.Theme.fg;
      END
      ELSE BEGIN
	lfg := fDialog.Theme.fg;
	lbg := fDialog.Theme.bg;
      END;
      IF SELF.Disabled THEN lbg := al_map_rgb (64, 64, 64);

      al_draw_filled_rectangle (SELF.fX1, SELF.fY1, SELF.fX2, SELF.fY2, lbg);
      al_draw_rectangle (
	SELF.fX1 + 0.5, SELF.fY1 + 0.5,
	SELF.fX2 - 0.5, SELF.fY2 - 0.5,
	lfg, 0
      );
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

    { Center the text vertically in the button, taking the font size
      into consideration. }
      y := al_get_font_line_height (fDialog.Theme.Font);
      y := (SELF.fY1 + SELF.fY2 - Y - 1) / 2;
      al_draw_text (
	fDialog.Theme.font, lfg,
	(SELF.fX1 + SELF.fX2 + 1) / 2, y,
	ALLEGRO_ALIGN_CENTRE,
	fText
      )
    FINALLY
      lState.Free
    END
  END;



(*
 * TToggleButton
 ***************************************************************************)

  CONSTRUCTOR TToggleButton. CreateToggleButton (aText: STRING);
  BEGIN
    INHERITED CreateButton (aText)
  END;



  PROCEDURE TToggleButton.onMouseButtonDown (mx, my: INTEGER);
  BEGIN
    IF SELF.Disabled THEN EXIT;
    SELF.SetPushed (NOT SELF.Pushed)
  END;



  PROCEDURE TToggleButton.onMouseButtonUp (mx, my: INTEGER);
  BEGIN
    { Disable default behavior. }
  END;



  PROCEDURE TToggleButton.SetPushed (aPushed: BOOLEAN);
  BEGIN
    IF fPushed <> aPushed THEN
    BEGIN
      fPushed := aPushed;
      fDialog.RequestDraw
    END
  END;



(*
 * TList
 ***************************************************************************)

  FUNCTION TList.GetSelectedItemText: STRING;
  BEGIN
    IF fSelectedItem < fItems.Count THEN
      RESULT := fItems[fSelectedItem]
    ELSE
      RESULT := ''
  END;



  CONSTRUCTOR TList.CreateList (InitialSelection: INTEGER);
  BEGIN
    INHERITED CreateWidget;
    fItems := TStringList.Create;
    fSelectedItem := InitialSelection
  END;



  DESTRUCTOR TList.Destroy;
  BEGIN
    fItems.Free;
    INHERITED Destroy
  END;



  FUNCTION TList.WantKeyFocus: BOOLEAN; BEGIN RESULT := NOT SELF.Disabled END;



  PROCEDURE TList.onKeyDown (CONST Event: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    IF SELF.Disabled THEN EXIT;
    CASE Event.keycode OF
    ALLEGRO_KEY_DOWN:
      IF fSelectedItem < fItems.Count - 1 THEN
      BEGIN
	INC (fSelectedItem);
	fDialog.RequestDraw
      END;
    ALLEGRO_KEY_UP:
      IF fSelectedItem > 0 THEN
      BEGIN
	DEC (fSelectedItem);
	fDialog.RequestDraw
      END;
    END
  END;



  PROCEDURE TList.onClick (mx, my: INTEGER);
  VAR
    lItem: INTEGER;
  BEGIN
    IF SELF.Disabled THEN EXIT;
    lItem := (my - SELF.fY1) DIV al_get_font_line_height (fDialog.Theme.font);
    IF lItem < fItems.Count THEN
    BEGIN
      fSelectedItem := lItem;
      fDialog.RequestDraw
    END
  END;



  PROCEDURE TList.Draw;
  VAR
    lState: TSaveState;
    lbg: ALLEGRO_COLOR;
    FontHeight, lItem, yi: INTEGER;
  BEGIN
    IF SELF.Disabled THEN
      lbg := al_map_rgb (64, 64, 64)
    ELSE
      lbg := fDialog.Theme.bg;

    al_draw_filled_rectangle (fX1 + 1, fY1 + 1, fX2 - 1, fY2 - 1, lbg);

    lState := TSaveState.Create;
    TRY
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      FontHeight := al_get_font_line_height (fDialog.Theme.Font);
      FOR lItem := 0 TO fItems.Count - 1 DO
      BEGIN
	yi := fY1 + lItem * FontHeight;
	IF lItem = fSelectedItem THEN
	  al_draw_filled_rectangle (
	    fX1 + 1, yi, fX2 - 1, yi + FontHeight - 1,
	    fDialog.Theme.Highlight
	  );
	al_draw_text (
	  fDialog.Theme.Font, fDialog.Theme.fg,
	  fX1, yi,
	  0,
	  fItems[lItem]
	)
      END
    FINALLY
      lState.Free
    END;
  END;



  PROCEDURE TList.ClearItems;
  BEGIN
    fItems.Clear;
    fSelectedItem := 0
  END;



  PROCEDURE TList.AppendItem (aText: STRING);
  BEGIN
    fItems.Add (aText)
  END;



(*
 * TCustomSlider
 ***************************************************************************)

  CONSTRUCTOR TCustomSlider.CreateSlider (aCurValue, aMaxValue: INTEGER);
  BEGIN
    INHERITED CreateWidget;
    fCurValue := aCurValue; fMaxValue := aMaxValue
  END;



  PROCEDURE TCustomSlider.onMouseButtonDown (mx, my: INTEGER);
  BEGIN
    IF SELF.Disabled THEN EXIT;
    SELF.onMouseButtonHold (mx, my);
  END;



(*
 * TVSlider
 ***************************************************************************)

  PROCEDURE TVSlider.onMouseButtonHold (mx, my: INTEGER);
  VAR
    r: DOUBLE;
  BEGIN
    IF SELF.Disabled THEN EXIT;

    r := (SELF.fY2 - 1 - my) / (SELF.Height - 2);
    r := CLAMP (0, r, 1);
    fCurValue := TRUNC (r * fMaxValue);
    fDialog.RequestDraw
  END;



  PROCEDURE TVSlider.Draw;
  VAR
    lbg: ALLEGRO_COLOR;
    Left, Top, Right, Bottom: REAL;
    Ratio: DOUBLE;
    yPos: INTEGER;
  BEGIN
    Left := SELF.fX1; Top := SELF.fY1 + 0.5;
    Right := SELF.fX2 + 0.5; Bottom := SELF.fY2 + 0.5;
    IF SELF.Disabled THEN
      lbg := al_map_rgb (64, 64, 64)
    ELSE
      lbg := fDialog.Theme.bg;
    al_draw_rectangle (Left, Top, Right, Bottom, lBg, 1);

    Ratio := SELF.CurValue / SELF.MaxValue;
    yPos := TRUNC (Bottom - 0.5 - (Ratio * (SELF.Height - 7)));
    al_draw_filled_rectangle (
      Left + 0.5, yPos - 5,
      Right - 0.5, yPos,
      fDialog.Theme.fFg
    )
  END;



(*
 * THSlider
 ***************************************************************************)

  PROCEDURE THSlider.onMouseButtonHold (mx, my: INTEGER);
  VAR
    r: DOUBLE;
  BEGIN
    IF SELF.Disabled THEN EXIT;

    r := (mx - 1 - SELF.fX1) / (SELF.Width - 2);
    r := CLAMP (0, r, 1);
    fCurValue := TRUNC (r * fMaxValue);
    fDialog.RequestDraw
  END;



  PROCEDURE THSlider.Draw;
  VAR
    lbg: ALLEGRO_COLOR;
    cY: INTEGER;
    Ratio: DOUBLE;
    xPos: INTEGER;
  BEGIN
    cY := (SELF.fY1 + SELF.fY2) DIV 2;
    IF SELF.Disabled THEN
      lbg := al_map_rgb (64, 64, 64)
    ELSE
      lbg := fDialog.Theme.bg;
    al_draw_filled_rectangle (SELF.fX1, SELF.fY1, SELF.fX2, SELF.fY2, lbg);
    al_draw_line (SELF.fX1, cY, SELF.fX2, cY, fDialog.Theme.fg, 0);

    Ratio := SELF.CurValue / SELF.MaxValue;
    xPos := SELF.fX1 + TRUNC (Ratio * (SELF.Width - 2));
    al_draw_filled_rectangle (
      xPos - 2, SELF.fY1,
      xPos + 2, SELF.fY2,
      fDialog.Theme.fFg
    )
  END;



(*
 * TTextEntry
 ***************************************************************************)

  CONST
    CURSOR_WIDTH = 8;

  PROCEDURE TTextEntry.MaybeScroll;
  VAR
    tw: INTEGER;
    tmp: TUString;
  BEGIN
    IF fCursorPos < fLeftPos + 3 THEN
    BEGIN
      IF fCursorPos < 3 THEN fLeftPos := 0 ELSE fLeftPos := fCursorPos - 3
    END
    ELSE WHILE TRUE DO
    TRY
      tmp := TUString.Create (fText, fLeftPos, fCursorPos);
      tw := al_get_ustr_width (fDialog.Theme.Font, tmp.UStr);
      IF fX1 + tw + CURSOR_WIDTH < fX2 THEN EXIT;
      al_ustr_next (fText, fLeftPos)
    FINALLY
      tmp.Free
    END
  END;



  FUNCTION TTextEntry.GetText: STRING;
  BEGIN
    RESULT := al_cstr (fText)
  END;



  CONSTRUCTOR TTextEntry.CreateTextEntry (aInitialText: STRING);
  BEGIN
    INHERITED CreateWidget;
    fFocused := FALSE;
    fCursorPos := 0;
    fLeftPos := 0;
    fText := al_ustr_new (aInitialText)
  END;



  DESTRUCTOR TTextEntry.Destroy;
  BEGIN
    al_ustr_free (fText);
    INHERITED Destroy
  END;



  FUNCTION TTextEntry.WantKeyFocus: BOOLEAN;
  BEGIN
    RESULT := NOT SELF.Disabled
  END;



  PROCEDURE TTextEntry.GotKeyFocus;
  BEGIN
    SELF.fFocused := TRUE;
    fDialog.RequestDraw
  END;



  PROCEDURE TTextEntry.LostKeyFocus;
  BEGIN
    SELF.fFocused := FALSE;
    fDialog.RequestDraw
  END;



  PROCEDURE TTextEntry.onKeyDown (CONST Event: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    IF SELF.Disabled THEN EXIT;
    CASE Event.keycode OF
    ALLEGRO_KEY_LEFT:
      al_ustr_prev (fText, fCursorPos);
    ALLEGRO_KEY_RIGHT:
      al_ustr_next (fText, fCursorPos);
    ALLEGRO_KEY_HOME:
      fCursorPos := 0;
    ALLEGRO_KEY_END:
      fCursorPos := al_ustr_size (fText);
    ALLEGRO_KEY_DELETE:
      al_ustr_remove_chr (fText, fCursorPos);
    ALLEGRO_KEY_BACKSPACE:
      IF al_ustr_prev (fText, fCursorPos) THEN
	al_ustr_remove_chr (fText, fCursorPos);
    ELSE
      IF Event.unichar >= ORD (' ') THEN
      BEGIN
	al_ustr_insert_chr (fText, fCursorPos, Event.unichar);
	INC (fCursorPos, al_utf8_width (Event.unichar))
      END;
    END;
    SELF.MaybeScroll;
    fDialog.RequestDraw
  END;



  PROCEDURE TTextEntry.Draw;
  VAR
    lbg: ALLEGRO_COLOR;
    lState: TSaveState;
    lText, Sub: TUString;
    lx, PostCursor, Subw: INTEGER;
  BEGIN
    IF SELF.Disabled THEN
      lbg := al_map_rgb (64, 64, 64)
    ELSE
      lbg := fDialog.Theme.bg;
    al_draw_filled_rectangle (fX1, fY1, fX2, fY2, lbg);

    TRY
      lState := TSaveState.Create;

      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);

      IF NOT fFocused THEN
      BEGIN
	lText := TUString.Create (fText, fLeftPos);
	al_draw_ustr (
	  fDialog.Theme.Font, fDialog.Theme.fg,
	  fX1, fY1,
	  0,
	  lText.UStr
	)
      END
      ELSE BEGIN
	lx := fX1;
	IF fCursorPos > 0 THEN
	TRY
	  Sub := TUString.Create (fText, fLeftPos, fCursorPos);
	  al_draw_ustr (
	    fDialog.Theme.Font, fDialog.Theme.fg,
	    fX1, fY1,
	    0,
	    Sub.UStr
	  );
	  INC (lx, al_get_ustr_width (fDialog.Theme.Font, Sub.UStr))
	FINALLY
	  Sub.Free
	END;

	IF fCursorPos = al_ustr_size (fText) THEN
	  al_draw_filled_rectangle (
	    lx, fY1,
	    lx + CURSOR_WIDTH,
	    fY1 + al_get_font_line_height (fDialog.Theme.Font),
	    fDialog.Theme.fg
	  )
	ELSE TRY
	  PostCursor := fCursorPos;
	  al_ustr_next (fText, PostCursor);

	  Sub := TUString.Create (fText, fCursorPos, PostCursor);
	  Subw := al_get_ustr_width (fDialog.Theme.Font, Sub.UStr);
	  al_draw_filled_rectangle (
	    lx, fY1,
	    lx + Subw, fY1 + al_get_font_line_height (fDialog.Theme.Font),
	    fDialog.Theme.fg
	  );
	  al_draw_ustr (
	    fDialog.Theme.Font, fDialog.Theme.fg,
	    lx, fY1,
	    0, Sub.UStr
	  );
	  INC (lx, Subw);

	  Sub.Free; Sub := TUString.Create (fText, PostCursor);
	  al_draw_ustr (
	    fDialog.Theme.Font, fDialog.Theme.fg,
	    lx, fY1,
	    0, Sub.UStr
	  );
	FINALLY
	  Sub.Free
	END
      END
    FINALLY
      lText.Free;
      lState.Free
    END
  END;



(*
 * TSaveState
 ***************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TSaveState.Create (CONST Save: INTEGER);
  BEGIN
    INHERITED Create;
    al_store_state (SELF.State, Save)
  END;



(* Destructor. *)
  DESTRUCTOR TSaveState.Destroy;
  BEGIN
    al_restore_state (State);
    INHERITED Destroy
  END;



(*
 * TUString
 ***************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TUString.Create
    (s: ALLEGRO_USTRptr; aFirst: INTEGER; aEnd: INTEGER = -1);
  BEGIN
    INHERITED Create;
    IF aEnd <= -1 THEN aEnd := al_ustr_size (s);
    fUStr := al_ref_ustr (SELF.Info, s, aFirst, aEnd)
  END;

END.
