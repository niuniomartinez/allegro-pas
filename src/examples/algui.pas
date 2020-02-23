UNIT alGUI;
(* A very simple GUI to be used by the example programs.
 *
 * It's intended to be as simple and transparent as possible (simplistic,
 * even).
 *)
(*
  Copyright (c) 2019-2020 Guillermo Martínez J.

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
    allegro5, al5base, al5font, al5strings,
    Classes;

  CONST
  (* Initial number of widgets. *)
    MIN_WIDGETS = 16;

  TYPE
    TWidget = CLASS; { Forward declaration. }



  (* Dialog container.  This is much like VCL/LCL TForm class so most Delphi
     and Lazarus users should be used to it.

     Note this dialog manages its own event queue that dispatches events from
     current display, keyboard and mouse. *)
    TDialog = CLASS (TObject)
    PRIVATE
    { Defines a grid that helps defining widget position and sizes. }
      fGridM, fGridN,
    { Defines a margin in the upper-left side.  In pixels. }
      fPadX, fPadY: INTEGER;
    { Dialog graphic style. }
      fBgColor, fFgColor, fFgDisabledColor,
      fBgTextColor, fSelectColor: ALLEGRO_COLOR;
      fTextFont: ALLEGRO_FONTptr;
    { Event management. }
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fEvent: ALLEGRO_EVENT;

      fAllWidgets: ARRAY OF TWidget;
      fNextWidget, fKeyFocus, fMouseDownWidget: INTEGER;

      fTerminated: BOOLEAN;

      PROCEDURE SetTextFont (aFont: ALLEGRO_FONTptr);
      FUNCTION GetWidget (CONST Ndx: INTEGER): TWidget;
    (* Updates the dialog. *)
      PROCEDURE Update;
    (* Renders the dialog. *)
      PROCEDURE Draw;
    (* Looks for a widget that wants key focus. *)
      FUNCTION SearchKeyFocus(CONST aNdx:INTEGER; CONST aInc:INTEGER=1):INTEGER;
    (* Changes key focus, if widget wants it. *)
      PROCEDURE SetKeyFocus (CONST aNdx: INTEGER);
    (* Looks for a widget in the given pixel. *)
      FUNCTION SearchWidgetIn (CONST aX, aY: INTEGER): INTEGER;
    PUBLIC
    (* Constructor.

       Do not create the dialog before to initialize Allegro and create a
       display. *)
      CONSTRUCTOR Create (CONST aGridM, aGridN: INTEGER);
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Adds a new widget. Returns the widget index. *)
      FUNCTION Add (aWidget: TWidget; agx, agy, agw, agh: INTEGER): INTEGER;
    (* Initializes the dialog.  Should call this after adding all widgets. *)
      PROCEDURE Initialize; VIRTUAL;
    (* Executes the dialog until Terminate is called. *)
      PROCEDURE Run; VIRTUAL;
    (* Sets "Terminated" to TRUE. *)
      PROCEDURE Terminate;

    (* Background color.  Default is grey. *)
      PROPERTY BgColor: ALLEGRO_COLOR READ fBgColor WRITE fBgColor;
    (* Foreground/text color.  Default is black. *)
      PROPERTY FgColor: ALLEGRO_COLOR READ fFgColor WRITE fFgColor;
    (* Disabled foreground color. *)
      PROPERTY FgDisabledColor: ALLEGRO_COLOR
        READ fFgDisabledColor WRITE fFgDisabledColor;
    (* Background color for text widgets. *)
      PROPERTY BgTextColor: ALLEGRO_COLOR READ fBgTextColor WRITE fBgTextColor;
    (* Background color for selected text. *)
      PROPERTY SelectedColor: ALLEGRO_COLOR
        READ fSelectColor WRITE fSelectColor;
    (* Text font.  Default is 8x8 system font. *)
      PROPERTY TextFont: ALLEGRO_FONTptr READ fTextFont WRITE SetTextFont;
    (* Number of widgets. *)
      PROPERTY WidgetCount: INTEGER READ fNextWidget;
    (* Indexed access to widgets. *)
      PROPERTY Widgets[CONST Ndx: INTEGER]: TWidget READ GetWidget;
    (* Tells if method "Terminate" was called.  Default is FALSE. *)
      PROPERTY Terminated: BOOLEAN READ fTerminated;
    END;



  (* Base class for dialog widgets. *)
    TWidget = CLASS (TObject)
    PRIVATE
      fDialog: TDialog;
    { Position and size in grid units. }
      fGridX, fGridY, fGridW, fGridH,
    { Position and size in pixels. }
      fX, fY, fWidth, fHeight: INTEGER;
    { State. }
      fEnabled, fHasKeyFocus: BOOLEAN;
      fTag: INTEGER;

      PROCEDURE AdjustSize (aSizeX, aSizeY, aPadX, aPadY: INTEGER);
    PROTECTED
    (* Tells if widget wants key focus.  Default returns FALSE. *)
      FUNCTION WantsKeyFocus: BOOLEAN; VIRTUAL;

    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      PROCEDURE onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); VIRTUAL;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
      PROCEDURE onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT); VIRTUAL;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_UP. *)
      PROCEDURE onMouseUp; VIRTUAL;
    (* Handler for the ALLEGRO_EVENT_MOUSE_AXES. *)
      PROCEDURE onMouseMove (Mouse: ALLEGRO_MOUSE_EVENT); VIRTUAL;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Initializes the widget.  By default it does nothing. *)
      PROCEDURE Initialize; VIRTUAL;
    (* Tells if pixel is inside the widget. *)
      FUNCTION IsInside (CONST aX, aY: INTEGER): BOOLEAN; INLINE;
    (* Renders the widget. *)
      PROCEDURE Draw; VIRTUAL; ABSTRACT;

    (* Dialog that contains the widget. *)
      PROPERTY Dialog: TDialog READ fDialog;
    (* X coordinate. *)
      PROPERTY X: INTEGER READ fX;
    (* Y coordinate. *)
      PROPERTY Y: INTEGER READ fY;
    (* Widget width. *)
      PROPERTY Width: INTEGER READ fWidth;
    (* Widget height. *)
      PROPERTY Height: INTEGER READ fHeight;
    (* Tells if widget is enabled. *)
      PROPERTY Enabled: BOOLEAN READ fEnabled WRITE fEnabled;
    (* Widget has key focus. *)
      PROPERTY KeyFocus: BOOLEAN READ fHasKeyFocus;
    (* A tag value. *)
      PROPERTY Tag: INTEGER READ fTag WRITE fTag;
    END;



  (* A text label. *)
    TLabel = CLASS (TWidget)
    PRIVATE
      fCentered: BOOLEAN;
      fCaption: AL_STR;
    PUBLIC
    (* Creates the label. *)
      CONSTRUCTOR CreateLabel
        (CONST aCaption: AL_STR; CONST aCentered: BOOLEAN=TRUE);
    (* Renders label. *)
      PROCEDURE Draw; OVERRIDE;

    (* Centers caption. Default is TRUE. *)
      PROPERTY Centered: BOOLEAN READ fCentered WRITE fCentered;
    (* Label caption. *)
      PROPERTY Caption: AL_STR READ fCaption WRITE fCaption;
    END;



  (* Draws a bitmap. *)
    TBitmap = CLASS (TWidget)
    PRIVATE
      fBitmap: ALLEGRO_BITMAPptr;
      fOwnsBitmap: BOOLEAN;

      PROCEDURE SetBitmap (aBmp: ALLEGRO_BITMAPptr); INLINE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
      CONSTRUCTOR CreateBitmap (aBmp: ALLEGRO_BITMAPptr; aOwns: BOOLEAN=TRUE);
        OVERLOAD;
      CONSTRUCTOR CreateBitmap (aWidth, aHeight: INTEGER);
        OVERLOAD;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Draws bitmap. *)
      PROCEDURE Draw; OVERRIDE;

    (* Tells if widget owns the bitmap.  Default is TRUE. *)
      PROPERTY Owns: BOOLEAN READ fOwnsBitmap WRITE fOwnsBitmap;
    (* Reference to bitmap. *)
      PROPERTY Bmp: ALLEGRO_BITMAPptr READ fBitmap WRITE SetBitmap;
    END;



  (* Presents a list of options to select one. *)
    TOptionList = CLASS (TWidget)
    PRIVATE
      fItemList: TStringList;
      fSelectedItem: INTEGER;
      fOnChange: TNotifyEvent;

      PROCEDURE SetSelected (CONST aNdx: INTEGER); INLINE;
    PROTECTED
    (* Tells if widget wants key focus. *)
      FUNCTION WantsKeyFocus: BOOLEAN; OVERRIDE;

    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      PROCEDURE onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); OVERRIDE;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
      PROCEDURE onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT); OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates an option list. *)
      CONSTRUCTOR CreateOptionList (aOptions: ARRAY OF AL_STR);
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the widget. *)
      PROCEDURE Initialize; OVERRIDE;
    (* Draws option list. *)
      PROCEDURE Draw; OVERRIDE;

    (* List of options. *)
      PROPERTY Options: TStringList READ fItemList;
    (* Index of the selected item. *)
      PROPERTY Selected: INTEGER READ fSelectedItem WRITE SetSelected;

    (* Event triggered when selection changes. *)
      PROPERTY OnSelectChange: TNotifyEvent READ fOnChange WRITE fOnChange;
    END;



  (* Orientation. *)
    TOrientation = (oHorizontal, oVertical);

  (* A slider. *)
    TSlider = CLASS (TWidget)
    PRIVATE
      fOrientation: TOrientation;
      fMax, fCurrent: INTEGER;

      fOnChange: TNotifyEvent;

      PROCEDURE SetCurrent (aCurrent: INTEGER); INLINE;
      FUNCTION CalculateCurrent (aPosition: ALLEGRO_MOUSE_EVENT): INTEGER; INLINE;
    PROTECTED
    (* Tells widget wants key focus. *)
      FUNCTION WantsKeyFocus: BOOLEAN; OVERRIDE;

    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      PROCEDURE onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); OVERRIDE;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
      PROCEDURE onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT); OVERRIDE;
    (* Handler for the ALLEGRO_EVENT_MOUSE_AXES. *)
      PROCEDURE onMouseMove (Mouse: ALLEGRO_MOUSE_EVENT); OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates a slider. *)
      CONSTRUCTOR CreateSlider
        (CONST aMax: INTEGER; CONST aOrientation: TOrientation);
    (* Draws option list. *)
      PROCEDURE Draw; OVERRIDE;

    (* Current value. *)
      PROPERTY Value: INTEGER READ fCurrent WRITE SetCurrent;

    (* Event triggered when value changes. *)
      PROPERTY OnChange: TNotifyEvent READ fOnChange WRITE fOnChange;
    END;



  (* Text input field. *)
    TTextEntry = CLASS (TWidget)
    PRIVATE
      fText: ALLEGRO_USTRptr;
      fLeftPos, fCursorPos: INTEGER;

      fOnTextChanges: TNotifyEvent;

      PROCEDURE MaybeScroll;

      FUNCTION GetText: AL_STR;
      PROCEDURE SetText (CONST aText: AL_STR);
    PROTECTED
    (* Tells widget wants focus. *)
      FUNCTION WantsKeyFocus: BOOLEAN; OVERRIDE;
    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      PROCEDURE onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
      CONSTRUCTOR CreateTextEntry (CONST aText: AL_STR);
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Renders the widget. *)
      PROCEDURE Draw; OVERRIDE;

    (* Access to the input string. *)
      PROPERTY Text: AL_STR READ GetText WRITE SetText;

    (* Event triggered when text changes. *)
      PROPERTY OnTextChanges: TNotifyEvent
        READ fOnTextChanges WRITE fOnTextChanges;
    END;

IMPLEMENTATION

  USES
    Common,
    al5primitives,
    sysutils;

  CONST
    CURSOR_WIDTH = 8;

  TYPE
  (* Saves the Allegro state when created and restores it when destroyed. *)
    TSaveState = CLASS (TObject)
    PRIVATE
      State: ALLEGRO_STATE;
    PUBLIC
    (* Creates (and saves) the Allegro state. *)
      CONSTRUCTOR Create (CONST Save: INTEGER = ALLEGRO_STATE_ALL);
    (* Restores (and destroys) the Allegro state. *)
      DESTRUCTOR Destroy; OVERRIDE;
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



(* Destructor .*)
  DESTRUCTOR TSaveState.Destroy;
  BEGIN
    al_restore_state (State);
    INHERITED Destroy
  END;



(*
 * TDialog
 ***************************************************************************)

  PROCEDURE TDialog.SetTextFont (aFont: ALLEGRO_FONTptr);
  BEGIN
    IF fTextFont <> NIL THEN al_destroy_font (fTextFont);
    IF aFont <> NIL THEN
      fTextFont := aFont
    ELSE
      fTextFont := al_create_builtin_font
  END;



  FUNCTION TDialog.GetWidget (CONST Ndx: INTEGER): TWidget;
  BEGIN
    RESULT := fAllWidgets[Ndx]
  END;



(* Updates dialog. *)
  PROCEDURE TDialog.Update;

    PROCEDURE RelativeCoordinates
      (CONST aNdx: INTEGER; VAR Mouse: ALLEGRO_MOUSE_EVENT);
    BEGIN
        Mouse.x := Mouse.x - fAllWidgets[aNdx].fX;
        Mouse.y := Mouse.y - fAllWidgets[aNdx].fY;
    END;

    FUNCTION GetWidgetMouse (VAR Mouse: ALLEGRO_MOUSE_EVENT): INTEGER;
    BEGIN
      RESULT := SELF.SearchWidgetIn (Mouse.x, Mouse.y);
      IF RESULT >= 0 THEN RelativeCoordinates (RESULT, Mouse)
    END;

  VAR
    Ndx: INTEGER;
  BEGIN
    WHILE al_get_next_event (fEventQueue, fEvent) DO
      CASE fEvent.ftype OF
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        SELF.Terminate;

      ALLEGRO_EVENT_KEY_CHAR:
        CASE fEvent.keyboard.keycode OF
        ALLEGRO_KEY_ESCAPE:
          SELF.Terminate;
        ALLEGRO_KEY_TAB:
          IF (fEvent.keyboard.modifiers AND ALLEGRO_KEYMOD_SHIFT) <> 0 THEN
            SELF.SetKeyFocus (SELF.SearchKeyFocus (fKeyFocus - 1, -1))
          ELSE
            SELF.SetKeyFocus (SELF.SearchKeyFocus (fKeyFocus + 1));
        ELSE
          IF (fKeyFocus >= 0) AND fAllWidgets[fKeyFocus].Enabled THEN
            fAllWidgets[fKeyFocus].onKeyChar (fEvent.keyboard);
        END;

      ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
        BEGIN
          Ndx := GetWidgetMouse (fEvent.mouse);
          IF Ndx <> fMouseDownWidget THEN
          BEGIN
            IF fMouseDownWidget >= 0 THEN
              fAllWidgets[fMouseDownWidget].onMouseUp;
            fMouseDownWidget := Ndx
          END;
          SELF.SetKeyFocus (Ndx);
          IF (Ndx >= 0) AND fAllWidgets[Ndx].Enabled THEN
            fAllWidgets[Ndx].onMouseDown (fEvent.mouse)
        END;
      ALLEGRO_EVENT_MOUSE_BUTTON_UP:
        IF fMouseDownWidget >= 0 THEN
        BEGIN
          fAllWidgets[fMouseDownWidget].onMouseUp;
          fMouseDownWidget := -1
        END;
      ALLEGRO_EVENT_MOUSE_AXES:
        IF (fMouseDownWidget >= 0)
        AND fAllWidgets[fMouseDownWidget].Enabled
        THEN BEGIN
          RelativeCoordinates (fMouseDownWidget, fEvent.mouse);
          fAllWidgets[fMouseDownWidget].onMouseMove (fEvent.mouse)
        END;
      END
  END;



(* Renders dialog. *)
  PROCEDURE TDialog.Draw;
  VAR
    cx, cy, cw, ch, Ndx: INTEGER;
  BEGIN
    al_get_clipping_rectangle (cx, cy, cw, ch);
    al_clear_to_color (fBgColor);
    TRY
      FOR Ndx := 0 TO fNextWidget - 1 DO
      BEGIN
        al_set_clipping_rectangle (
          fAllWidgets[Ndx].X, fAllWidgets[Ndx].Y,
          fAllWidgets[Ndx].Width, fAllWidgets[Ndx].Height
        );
        fAllWidgets[Ndx].Draw
      END
    FINALLY
      al_set_clipping_rectangle (cx, cy, cw, ch);
      al_flip_display
    END
  END;



(* Looks for key focus. *)
  FUNCTION TDialog.SearchKeyFocus (CONST aNdx, aInc: INTEGER): INTEGER;
  BEGIN
    RESULT := aNdx; IF RESULT >= fNextWidget THEN RESULT := 0;
    REPEAT
      IF fAllWidgets[RESULT].Enabled AND fAllWidgets[RESULT].WantsKeyFocus THEN
        EXIT;
    { Next widget. }
      INC (RESULT, aInc);
      IF RESULT >= fNextWidget THEN RESULT := 0;
      IF RESULT < 0 THEN RESULT := fNextWidget - 1
    UNTIL RESULT = aNdx;
  { If here, no widget wants key focus. }
    RESULT := -1
  END;



(* Sets key focus. *)
  PROCEDURE TDialog.SetKeyFocus (CONST aNdx: INTEGER);
  BEGIN
  { Only if focus changes. }
    IF aNdx <> fKeyFocus THEN
    BEGIN
    { If new focus doesn't want focus, then don't change. }
      IF (0 <= aNdx) AND (aNdx < fNextWidget) THEN
        IF NOT fAllWidgets[aNdx].Enabled
        OR NOT fAllWidgets[aNdx].WantsKeyFocus
        THEN
          EXIT;
    { Current widget with focus lost it. }
      IF fKeyFocus >= 0 THEN fAllWidgets[fKeyFocus].fHasKeyFocus := FALSE;
    { New focus. }
      fKeyFocus := aNdx;
      IF (0 <= fKeyFocus) AND (fKeyFocus < fNextWidget) THEN
        fAllWidgets[fKeyFocus].fHasKeyFocus := TRUE
    END
  END;



(* Looks for a widget in the given pixel. *)
  FUNCTION TDialog.SearchWidgetIn (CONST aX, aY: INTEGER): INTEGER;
  BEGIN
    FOR RESULT := fNextWidget - 1 DOWNTO 0 DO
      IF fAllWidgets[RESULT].IsInside (ax, ay) THEN
        EXIT;
  { No widget found. }
    RESULT := -1
  END;



(* Constructor. *)
  CONSTRUCTOR TDialog.Create (CONST aGridM, aGridN: INTEGER);
  BEGIN
    INHERITED Create;
    SetLength (fAllWidgets, MIN_WIDGETS);
    fGridM := aGridM; fGridN := aGridN;
    fPadX := 1; fPadY := 1;
    fKeyFocus := -1; fMouseDownWidget := -1;

    fBgColor :=         al_map_rgb (204, 204, 204);
    fFgColor :=         al_map_rgb (  0,   0,   0);
    fFgDisabledColor := al_map_rgb (  51, 51,  51);
    fBgTextColor :=     al_map_rgb (255, 255, 255);
    fSelectColor :=     al_map_rgb (  0,   0, 153);
    SELF.SetTextFont (NIL)
  END;



(* Destructor. *)
  DESTRUCTOR TDialog.Destroy;
  VAR
    Ndx: INTEGER;
  BEGIN
    IF fEventQueue <> NIL THEN al_destroy_event_queue (fEventQueue);
    IF fNextWidget > 0 THEN
      FOR Ndx := 0 TO fNextWidget - 1 DO
        fAllWidgets[Ndx].Free;
    IF fTextFont <> NIL THEN al_destroy_font (fTextFont);
    INHERITED Destroy
  END;



(* Adds a new widget. Returns the widget index. *)
  FUNCTION TDialog.Add (aWidget: TWidget; agx, agy, agw, agh: INTEGER): INTEGER;
  BEGIN
    aWidget.fDialog := SELF;
    aWidget.fGridX := agx; aWidget.fGridY := agy;
    aWidget.fGridW := agw; aWidget.fGridH := agh;

    RESULT := fNextWidget;
    IF fNextWidget >= Length (fAllWidgets) THEN
      SetLength (fAllWidgets, Length (fAllWidgets) * 2);
    fAllWidgets[fNextWidget] := aWidget;
    INC (fNextWidget)
  END;



(* Initializes the dialog.  Should call this after adding all widgets. *)
  PROCEDURE TDialog.Initialize;
  VAR
    lDisplay: ALLEGRO_DISPLAYptr;
    Ndx,
    SizeX, SizeY: INTEGER;
  BEGIN
    lDisplay := al_get_current_display;
  { Calculate grid cell sizes. }
    SizeX := al_get_display_width (lDisplay) DIV fGridM;
    SizeY := al_get_display_height (lDisplay) DIV fGridN;
  { Adjust widget position and size, and initializes it. }
    FOR Ndx := 0 TO fNextWidget - 1 DO
    BEGIN
      fAllWidgets[Ndx].AdjustSize (SizeX, SizeY, fPadX, fPadY);
      fAllWidgets[Ndx].initialize
    END;
  { Key focus. }
    SELF.SetKeyFocus (SELF.SearchKeyFocus (0));
  { Event management. }
    fEventQueue := al_create_event_queue;
    al_register_event_source (fEventQueue, al_get_display_event_source (lDisplay));
    al_register_event_source (fEventQueue, al_get_keyboard_event_source);
    al_register_event_source (fEventQueue, al_get_mouse_event_source);
  { Start. }
    fTerminated := FALSE
  END;



(* Executes the dialog. *)
  PROCEDURE TDialog.Run;
  BEGIN
    TRY
      REPEAT
        SELF.Update;
        SELF.Draw
      UNTIL fTerminated
    EXCEPT
      ON Error: Exception DO
        AbortExample (al_string_to_str (Error.Message))
    END
  END;



(* Terminate dialog. *)
  PROCEDURE TDialog.Terminate;
  BEGIN
    fTerminated := TRUE
  END;



(*
 * TWidget
 ***************************************************************************)

  PROCEDURE TWidget.AdjustSize (aSizeX, aSizeY, aPadX, aPadY: INTEGER);
  BEGIN
    fX := aSizeX * fGridX + aPadX;
    fY := aSizeY * fGridY + aPadY;
    fWidth := aSizeX * fGridW - aPadX - 1;
    fHeight := aSizeY * fGridH - aPadY - 1
  END;



(* Check if wants focus. *)
  FUNCTION TWidget.WantsKeyFocus: BOOLEAN;
  BEGIN
    RESULT := FALSE
  END;



(* Event handlers. *)
{$IFDEF FPC} { Ignore warnings. }
  {$PUSH}
  {$WARN 5024 OFF : Parameter "$1" not used}
{$ENDIF}
  PROCEDURE TWidget.onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); BEGIN END;

  PROCEDURE TWidget.onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT); BEGIN END;

  PROCEDURE TWidget.onMouseUp; BEGIN END;

  PROCEDURE TWidget.onMouseMove (Mouse: ALLEGRO_MOUSE_EVENT); BEGIN END;
{$IFDEF FPC} {$POP} {$ENDIF}


(* Constructor. *)
  CONSTRUCTOR TWidget.Create;
  BEGIN
    INHERITED Create;
    fEnabled := TRUE;
    fHasKeyFocus := FALSE;
    fTag := 0
  END;



(* initializes. *)
  PROCEDURE TWidget.initialize; BEGIN END;



(* Tells if pixel is inside. *)
  FUNCTION TWidget.IsInside (CONST aX, aY: INTEGER): BOOLEAN;
  BEGIN
    RESULT := (fX <= aX) AND (aX <= fX + fWidth) AND
              (fY <= aY) AND (aY <= fY + fHeight)
  END;



(*
 * TLabel
 ***************************************************************************)

(* Creates the label. *)
  CONSTRUCTOR TLabel.CreateLabel
    (CONST aCaption: AL_STR; CONST aCentered: BOOLEAN);
  BEGIN
    INHERITED Create;
    fCentered := aCentered;
    fCaption := aCaption
  END;



(* Renders label. *)
  PROCEDURE TLabel.Draw;
  VAR
    State: TSaveState;
    Clr: ALLEGRO_COLOR;
  BEGIN
    IF SELF.Enabled THEN
      Clr := Dialog.FgColor
    ELSE
      Clr := Dialog.FgDisabledColor;
    State := TSaveState.Create;
    TRY
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      IF fCentered THEN
        al_draw_text (
          Dialog.TextFont, Clr,
          fX + (fWidth DIV 2), fY,
          ALLEGRO_ALIGN_CENTRE,
          fCaption
        )
      ELSE
        al_draw_text (
          Dialog.TextFont, Clr,
          fX, fY,
          0,
          fCaption
        )
    FINALLY
    { Restore state, including blender. }
      State.Free
    END
  END;



(*
 * TBitmap
 ***************************************************************************)

  PROCEDURE TBitmap.SetBitmap (aBmp: ALLEGRO_BITMAPptr);
  BEGIN
    IF fOwnsBitmap AND Assigned (fBitmap) THEN
      al_destroy_bitmap (fBitmap);
    fBitmap := aBmp
  END;



(* Constructor. *)
  CONSTRUCTOR TBitmap.Create;
  BEGIN
    INHERITED Create;
    fBitmap := NIL;
    fOwnsBitmap := TRUE
  END;



  CONSTRUCTOR TBitmap.CreateBitmap (aBmp: ALLEGRO_BITMAPptr; aOwns: BOOLEAN);
  BEGIN
    INHERITED Create;
    fBitmap := aBmp;
    fOwnsBitmap := aOwns
  END;



  CONSTRUCTOR TBitmap.CreateBitmap (aWidth, aHeight: INTEGER);
  BEGIN
    INHERITED Create;
    fBitmap := al_create_bitmap (aWidth, aHeight);
    fOwnsBitmap := fBitmap <> NIL
  END;



(* Destructor. *)
  DESTRUCTOR TBitmap.Destroy;
  BEGIN
    IF fOwnsBitmap AND Assigned (fBitmap) THEN al_destroy_bitmap (fBitmap);
    INHERITED Destroy
  END;



(* Draws bitmap. *)
  PROCEDURE TBitmap.Draw;
  BEGIN
    al_draw_scaled_bitmap (
      fBitmap,
      0, 0, al_get_bitmap_width (fBitmap), al_get_bitmap_height (fBitmap),
      fX, fY, fWidth, fHeight,
      0
    )
  END;



(*
 * TOptionList
 ***************************************************************************)

  PROCEDURE TOptionList.SetSelected (CONST aNdx: INTEGER);
  BEGIN
    IF fSelectedItem <> aNdx THEN
    BEGIN
      fSelectedItem := aNdx;
      IF (0 <= fSelectedItem) AND (fSelectedItem < fItemList.Count)
      AND Assigned (fOnChange) THEN
        fOnChange (SELF)
    END
  END;



(* Check if wants focus. *)
  FUNCTION TOptionList.WantsKeyFocus: BOOLEAN;
  BEGIN
    RESULT := TRUE
  END;



(* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
  PROCEDURE TOptionList.onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    CASE Event.keycode OF
    ALLEGRO_KEY_UP:
      IF fSelectedItem <= 0 THEN
        SELF.SetSelected (fItemList.Count - 1)
      ELSE
        SELF.SetSelected (fSelectedItem - 1);
    ALLEGRO_KEY_DOWN:
      IF fSelectedItem >= fItemList.Count - 1 THEN
        SELF.SetSelected (0)
      ELSE
        SELF.SetSelected (fSelectedItem + 1);
    END
  END;



(* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
  PROCEDURE TOptionList.onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT);
  VAR
    Ndx: INTEGER;
  BEGIN
    Ndx := Mouse.y DIV al_get_font_line_height (Dialog.TextFont);
    IF (0 <= Ndx) AND (Ndx < fItemList.Count) THEN SELF.SetSelected (Ndx)
  END;



(* Constructor. *)
  CONSTRUCTOR TOptionList.Create;
  BEGIN
    INHERITED Create;
    fItemList := TStringList.Create
  END;



  CONSTRUCTOR TOptionList.CreateOptionList (aOptions: ARRAY OF AL_STR);
  VAR
    Ndx: INTEGER;
  BEGIN
    INHERITED Create;
    fItemList := TStringList.Create;
    FOR Ndx := LOW (aOptions) TO HIGH (aOptions) DO
      fItemList.Add (al_str_to_string (aOptions[Ndx]))
  END;



(* Destructor. *)
  DESTRUCTOR TOptionList.Destroy;
  BEGIN
    fItemList.Free;
    INHERITED Destroy
  END;



(* Initializes the widget. *)
  PROCEDURE TOptionList.Initialize;
  BEGIN
    INHERITED Initialize;
    fSelectedItem := 0
  END;



(* Draws list. *)
  PROCEDURE TOptionList.Draw;
  VAR
    State: TSaveState;
    ClrText, ClrSelected: ALLEGRO_COLOR;
    Ndx, pX, pY, iY: INTEGER;
  BEGIN
  { Colors depend if enabled and has focus. }
    IF SELF.Enabled THEN
    BEGIN
      ClrText := Dialog.fgColor;
      IF SELF.KeyFocus THEN
        ClrSelected := Dialog.SelectedColor
      ELSE
        ClrSelected := Dialog.fgColor
    END
    ELSE BEGIN
      ClrText := Dialog.FgDisabledColor;
      ClrSelected := Dialog.FgDisabledColor
    END;
  { Draw control. }
    State := TSaveState.Create;
    TRY
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_filled_rectangle (
        fX, fY, fX + fWidth, fY + fHeight,
        Dialog.BgTextColor
      );
      al_draw_rectangle (
        fX, fY, fX + fWidth, fY + fHeight,
        Dialog.FgColor,
        1
      );
      pX := fx + 1 ; pY := fY + 1;
      iY := al_get_font_line_height (Dialog.TextFont);
      FOR Ndx := 0 TO fItemList.Count - 1 DO
      BEGIN
        IF Ndx = fSelectedItem THEN
        BEGIN
          al_draw_filled_rectangle (
            pX, pY, pX + fWidth - 3, pY + iY,
            ClrSelected
          );
          al_draw_text (
            Dialog.TextFont, Dialog.BgTextColor,
            pX, pY,
            0,
            al_string_to_str (fItemList[Ndx])
          )
        END
        ELSE
          al_draw_text (
            Dialog.TextFont, ClrText,
            pX, pY,
            0,
            al_string_to_str (fItemList[Ndx])
          );
        INC (pY, iY)
      END
    FINALLY
      State.Free
    END
  END;



(*
 * TSlider
 ***************************************************************************)

  PROCEDURE TSlider.SetCurrent (aCurrent: INTEGER);
  BEGIN
    IF aCurrent < 0 THEN aCurrent := 0;
    IF aCurrent > fMax THEN aCurrent := fMax;
    IF aCurrent <> fCurrent THEN
    BEGIN
      fCurrent := aCurrent;
      IF Assigned (fOnChange) THEN fOnChange (SELF)
    END
  END;



  FUNCTION TSlider.CalculateCurrent (aPosition: ALLEGRO_MOUSE_EVENT): INTEGER;
  BEGIN
    IF fOrientation = oHorizontal THEN
      EXIT (TRUNC ((fMax / fWidth) * aPosition.x))
    ELSE { oVertical }
      EXIT (TRUNC ((fMax / fHeight) * aPosition.y))
  END;



(* Check if wants focus. *)
  FUNCTION TSlider.WantsKeyFocus: BOOLEAN;
  BEGIN
    RESULT := TRUE
  END;



(* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
  PROCEDURE TSlider.onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    CASE Event.keycode OF
    ALLEGRO_KEY_UP, ALLEGRO_KEY_LEFT:
      SELF.SetCurrent (fCurrent - (fMax DIV 100));
    ALLEGRO_KEY_DOWN, ALLEGRO_KEY_RIGHT:
      SELF.SetCurrent (fCurrent + (fMax DIV 100));
    END
  END;



(* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
  PROCEDURE TSlider.onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT);
  BEGIN
    SELF.SetCurrent (SELF.CalculateCurrent (Mouse))
  END;



(* Handler for the ALLEGRO_EVENT_MOUSE_AXES. *)
  PROCEDURE TSlider.onMouseMove (Mouse: ALLEGRO_MOUSE_EVENT);
  BEGIN
    SELF.SetCurrent (SELF.CalculateCurrent (Mouse))
  END;



(* Constructor. *)
  CONSTRUCTOR TSlider.Create;
  BEGIN
    INHERITED Create;
    fOrientation := oHorizontal;
    fMax := 100; fCurrent := 0
  END;



(* Creates a slider. *)
  CONSTRUCTOR TSlider.CreateSlider
        (CONST aMax: INTEGER; CONST aOrientation: TOrientation);
  BEGIN
    INHERITED Create;
    fOrientation := aOrientation;
    fMax := aMax; fCurrent := 0
  END;



(* Draws option list. *)
  PROCEDURE TSlider.Draw;
  VAR
    State: TSaveState;
    Clr: ALLEGRO_COLOR;
    cX, cY: INTEGER;

    PROCEDURE DrawHorizontal;
    BEGIN
      cY := fY + (fHeight DIV 2);
      cX := fX + TRUNC (fWidth * (fCurrent / fMax)) - 2;

      al_draw_line (fX, cY, fX + fWidth, cY, Clr, 0);
      al_draw_filled_rectangle (cX - 2, fY, cX + 2, fY + fHeight, Clr)
    END;

    PROCEDURE DrawVertical;
    BEGIN
      cX := fX + (fWidth DIV 2);
      cY := fY + TRUNC (fHeight * (fCurrent / fMax)) - 2;

      al_draw_line (cX, fY, cX, fY + fHeight, Clr, 0);
      al_draw_filled_rectangle (fX, cY - 2, fX + fWidth, cY + 2, Clr)
    END;

  BEGIN
  { Widget color. }
    IF SELF.Enabled THEN
    BEGIN
      IF SELF.KeyFocus THEN
        Clr := Dialog.SelectedColor
      ELSE
        Clr := Dialog.FgColor
    END
    ELSE
      Clr := Dialog.FgDisabledColor;
  { Draw color. }
    State := TSaveState.Create;
    TRY
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_filled_rectangle (
        fX, fY, fX + fWidth, fY + fHeight,
        Dialog.BgColor
      );
      CASE fOrientation OF
      oHorizontal:
        DrawHorizontal;
      oVertical:
        DrawVertical;
      END;
    FINALLY
      State.Free
    END
  END;



(*
 * TTextEntry
 ***************************************************************************)

  PROCEDURE TTextEntry.MaybeScroll;
  VAR
    tw: LONGINT;
    lUstrInfo: ALLEGRO_USTR_INFO;
  BEGIN
    IF fCursorPos < fLeftPos + 3 THEN
    BEGIN
      IF fCursorPos < 3 THEN fLeftPos := 0 ELSE fLeftPos := fCursorPos -3
    END
    ELSE WHILE TRUE DO BEGIN
      tw := al_get_ustr_width (
        Dialog.TextFont,
        al_ref_ustr (lUstrInfo, fText, fLeftPos, fCursorPos)
      );
      IF SELF.X + tw + CURSOR_WIDTH < SELF.X + SELF.Width THEN EXIT;
      al_ustr_next (fText, fLeftPos)
    END
  END;



  FUNCTION TTextEntry.GetText: AL_STR;
  BEGIN
    RESULT := al_cstr (fText)
  END;

  PROCEDURE TTextEntry.SetText (CONST aText: AL_STR);
  BEGIN
    al_ustr_assign_cstr (fText, aText);
    IF Assigned (fOnTextChanges) THEN fOnTextChanges (SELF)
  END;



(* Wants key focus. *)
  FUNCTION TTextEntry.WantsKeyFocus: BOOLEAN;
  BEGIN
    RESULT := TRUE
  END;



(* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
  PROCEDURE TTextEntry.onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    CASE Event.keycode OF
    { Cursor position. }
    ALLEGRO_KEY_LEFT:
      al_ustr_prev (fText, fCursorPos);
    ALLEGRO_KEY_RIGHT:
      al_ustr_next (fText, fCursorPos);
    ALLEGRO_KEY_HOME:
      fCursorPos := 0;
    ALLEGRO_KEY_END:
      fCursorPos := al_ustr_size (fText);
    { Text edition. }
    ALLEGRO_KEY_BACKSPACE:
      IF al_ustr_prev (fText, fCursorPos) THEN
      BEGIN
        al_ustr_remove_chr (fText, fCursorPos);
        IF Assigned (fOnTextChanges) THEN fOnTextChanges (SELF)
      END;
    ELSE
      IF Event.unichar >= ORD (' ') THEN
      BEGIN
        al_ustr_insert_chr (fText, fCursorPos, Event.unichar);
        INC (fCursorPos, al_utf8_width (Event.unichar));
        IF Assigned (fOnTextChanges) THEN fOnTextChanges (SELF)
      END;
    END;
    SELF.MaybeScroll
  END;



(* Constructor. *)
  CONSTRUCTOR TTextEntry.Create;
  BEGIN
    INHERITED Create;
    fCursorPos := 0; fLeftPos := 0;
    fText := al_ustr_new ('')
  END;

  CONSTRUCTOR TTextEntry.CreateTextEntry (CONST aText: AL_STR);
  BEGIN
    INHERITED Create;
    fCursorPos := 0; fLeftPos := 0;
    fText := al_ustr_new (aText)
  END;



(* Destructor. *)
  DESTRUCTOR TTextEntry.Destroy;
  BEGIN
    al_ustr_free (fText);
    INHERITED Destroy
  END;



(* Renders the widget. *)
  PROCEDURE TTextEntry.Draw;
  VAR
    lUstrInfo: ALLEGRO_USTR_INFO;
    lSubStr: ALLEGRO_USTRptr;
    lState: TSaveState;
    lBgColor: ALLEGRO_COLOR;
    lX, lPostCursor, lSubW: INTEGER;
  BEGIN
    lState := TSaveState.Create;
    TRY
    { Background. }
      IF SELF.Enabled THEN
        lBgColor := Dialog.BgTextColor
      ELSE
        lBgColor := Dialog.BgColor;
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_filled_rectangle (fX, fY, fX + fWidth, fY + fHeight, lBgColor);
    { Text (and cursor). }
      IF SELF.KeyFocus THEN
      BEGIN
        lX := SELF.X;
        IF fCursorPos > 0 THEN
        BEGIN
          lSubStr := al_ref_ustr (lUstrInfo, fText, fLeftPos, fCursorPos);
          al_draw_ustr (
            Dialog.TextFont, Dialog.FgColor,
            SELF.X, SELF.Y,
            0,
            lSubStr
          );
          INC (lX, al_get_ustr_width (Dialog.TextFont, lSubStr))
        END;

        IF LONGWORD (fCursorPos) = al_ustr_size (fText) THEN
          al_draw_filled_rectangle (
            lX, SELF.Y,
            lX+CURSOR_WIDTH, SELF.Y+al_get_font_line_height (Dialog.TextFont),
            Dialog.FgColor
          )
        ELSE BEGIN
          lPostCursor := fCursorPos;
          al_ustr_next (fText, lPostCursor);
          lSubStr := al_ref_ustr (lUstrInfo, fText, fCursorPos, lPostCursor);
          lSubW := al_get_ustr_width (Dialog.TextFont, lSubStr);
          al_draw_filled_rectangle (
            lX, SELF.Y,
            lX + lSubW, SELF.Y + al_get_font_line_height (Dialog.TextFont),
            Dialog.FgColor
          );
          al_draw_ustr (Dialog.TextFont, lBgColor, lX, SELF.Y, 0, lSubStr);
          INC (lX, lSubW);
          al_draw_ustr (
            Dialog.TextFont, Dialog.FgColor,
            lX, SELF.Y,
            0,
            al_ref_ustr (lUstrInfo, fText, lPostCursor, al_ustr_size (fText))
          )
        END
      END
      ELSE
        al_draw_ustr (
          Dialog.TextFont, Dialog.FgColor,
          SELF.X, SELF.Y,
          0,
          al_ref_ustr (lUstrInfo, fText, fLeftPos, al_ustr_size (fText))
        )
    FINALLY
      lState.Free
    END
  END;

END.
