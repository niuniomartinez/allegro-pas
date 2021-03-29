unit alGUI;
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

interface

  uses
    allegro5, al5base, al5font, al5strings,
    Classes;

  const
  (* Initial number of widgets. *)
    MIN_WIDGETS = 16;

  type
    TWidget = class; { Forward declaration. }



  (* Dialog container.  This is much like VCL/LCL TForm class so most Delphi
     and Lazarus users should be used to it.

     Note this dialog manages its own event queue that dispatches events from
     current display, keyboard and mouse. *)
    TDialog = class (TObject)
    private
    { Defines a grid that helps defining widget position and sizes. }
      fGridM, fGridN,
    { Defines a margin in the upper-left side.  In pixels. }
      fPadX, fPadY: Integer;
    { Dialog graphic style. }
      fBgColor, fFgColor, fFgDisabledColor,
      fBgTextColor, fSelectColor: ALLEGRO_COLOR;
      fTextFont: ALLEGRO_FONTptr;
    { Event management. }
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fEvent: ALLEGRO_EVENT;

      fAllWidgets: array of TWidget;
      fNextWidget, fKeyFocus, fMouseDownWidget: Integer;

      fTerminated: Boolean;

      procedure SetTextFont (aFont: ALLEGRO_FONTptr);
      function GetWidget (const Ndx: Integer): TWidget;
    (* Updates the dialog. *)
      procedure Update;
    (* Renders the dialog. *)
      procedure Draw;
    (* Looks for a widget that wants key focus. *)
      function SearchKeyFocus(const aNdx:Integer; const aInc:Integer=1):Integer;
    (* Changes key focus, if widget wants it. *)
      procedure SetKeyFocus (const aNdx: Integer);
    (* Looks for a widget in the given pixel. *)
      function SearchWidgetIn (const aX, aY: Integer): Integer;
    public
    (* Constructor.

       Do not create the dialog before to initialize Allegro and create a
       display. *)
      constructor Create (const aGridM, aGridN: Integer);
    (* Destructor. *)
      destructor Destroy; override;
    (* Adds a new widget. Returns the widget index. *)
      function Add (aWidget: TWidget; agx, agy, agw, agh: Integer): Integer;
    (* Initializes the dialog.  Should call this after adding all widgets. *)
      procedure Initialize; virtual;
    (* Executes the dialog until Terminate is called. *)
      procedure Run; virtual;
    (* Sets "Terminated" to True. *)
      procedure Terminate;

    (* Background color.  Default is grey. *)
      property BgColor: ALLEGRO_COLOR read fBgColor write fBgColor;
    (* Foreground/text color.  Default is black. *)
      property FgColor: ALLEGRO_COLOR read fFgColor write fFgColor;
    (* Disabled foreground color. *)
      property FgDisabledColor: ALLEGRO_COLOR
        read fFgDisabledColor write fFgDisabledColor;
    (* Background color for text widgets. *)
      property BgTextColor: ALLEGRO_COLOR read fBgTextColor write fBgTextColor;
    (* Background color for selected text. *)
      property SelectedColor: ALLEGRO_COLOR
        read fSelectColor write fSelectColor;
    (* Text font.  Default is 8x8 system font. *)
      property TextFont: ALLEGRO_FONTptr read fTextFont write SetTextFont;
    (* Number of widgets. *)
      property WidgetCount: Integer read fNextWidget;
    (* Indexed access to widgets. *)
      property Widgets[const Ndx: Integer]: TWidget read GetWidget;
    (* Tells if method "Terminate" was called.  Default is False. *)
      property Terminated: Boolean read fTerminated;
    end;



  (* Base class for dialog widgets. *)
    TWidget = class (TObject)
    private
      fDialog: TDialog;
    { Position and size in grid units. }
      fGridX, fGridY, fGridW, fGridH,
    { Position and size in pixels. }
      fX, fY, fWidth, fHeight: Integer;
    { State. }
      fEnabled, fHasKeyFocus: Boolean;
      fTag: Integer;

      procedure AdjustSize (aSizeX, aSizeY, aPadX, aPadY: Integer);
    protected
    (* Tells if widget wants key focus.  Default returns False. *)
      function WantsKeyFocus: Boolean; virtual;

    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      procedure onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); virtual;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
      procedure onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT); virtual;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_UP. *)
      procedure onMouseUp; virtual;
    (* Handler for the ALLEGRO_EVENT_MOUSE_AXES. *)
      procedure onMouseMove (Mouse: ALLEGRO_MOUSE_EVENT); virtual;
    public
    (* Constructor. *)
      constructor Create; virtual;
    (* Initializes the widget.  By default it does nothing. *)
      procedure Initialize; virtual;
    (* Tells if pixel is inside the widget. *)
      function IsInside (const aX, aY: Integer): Boolean; inline;
    (* Renders the widget. *)
      procedure Draw; virtual; abstract;

    (* Dialog that contains the widget. *)
      property Dialog: TDialog read fDialog;
    (* X coordinate. *)
      property X: Integer read fX;
    (* Y coordinate. *)
      property Y: Integer read fY;
    (* Widget width. *)
      property Width: Integer read fWidth;
    (* Widget height. *)
      property Height: Integer read fHeight;
    (* Tells if widget is enabled. *)
      property Enabled: Boolean read fEnabled write fEnabled;
    (* Widget has key focus. *)
      property KeyFocus: Boolean read fHasKeyFocus;
    (* A tag value. *)
      property Tag: Integer read fTag write fTag;
    end;



  (* A text label. *)
    TLabel = class (TWidget)
    private
      fCentered: Boolean;
      fCaption: AL_STR;
    public
    (* Creates the label. *)
      constructor CreateLabel
        (const aCaption: AL_STR; const aCentered: Boolean=True);
    (* Renders label. *)
      procedure Draw; override;

    (* Centers caption. Default is True. *)
      property Centered: Boolean read fCentered write fCentered;
    (* Label caption. *)
      property Caption: AL_STR read fCaption write fCaption;
    end;



  (* Draws a bitmap. *)
    TBitmap = class (TWidget)
    private
      fBitmap: ALLEGRO_BITMAPptr;
      fOwnsBitmap: Boolean;

      procedure SetBitmap (aBmp: ALLEGRO_BITMAPptr); inline;
    public
    (* Constructor. *)
      constructor Create; override;
      constructor CreateBitmap (aBmp: ALLEGRO_BITMAPptr; aOwns: Boolean=True);
        overload;
      constructor CreateBitmap (aWidth, aHeight: Integer);
        overload;
    (* Destructor. *)
      destructor Destroy; override;
    (* Draws bitmap. *)
      procedure Draw; override;

    (* Tells if widget owns the bitmap.  Default is True. *)
      property Owns: Boolean read fOwnsBitmap write fOwnsBitmap;
    (* Reference to bitmap. *)
      property Bmp: ALLEGRO_BITMAPptr read fBitmap write SetBitmap;
    end;



  (* Presents a list of options to select one. *)
    TOptionList = class (TWidget)
    private
      fItemList: TStringList;
      fSelectedItem: Integer;
      fOnChange: TNotifyEvent;

      procedure SetSelected (const aNdx: Integer); inline;
    protected
    (* Tells if widget wants key focus. *)
      function WantsKeyFocus: Boolean; override;

    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      procedure onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); override;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
      procedure onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT); override;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Creates an option list. *)
      constructor CreateOptionList (aOptions: array of AL_STR);
    (* Destructor. *)
      destructor Destroy; override;
    (* Initializes the widget. *)
      procedure Initialize; override;
    (* Draws option list. *)
      procedure Draw; override;

    (* List of options. *)
      property Options: TStringList read fItemList;
    (* Index of the selected item. *)
      property Selected: Integer read fSelectedItem write SetSelected;

    (* Event triggered when selection changes. *)
      property OnSelectChange: TNotifyEvent read fOnChange write fOnChange;
    end;



  (* Orientation. *)
    TOrientation = (oHorizontal, oVertical);

  (* A slider. *)
    TSlider = class (TWidget)
    private
      fOrientation: TOrientation;
      fMax, fCurrent: Integer;

      fOnChange: TNotifyEvent;

      procedure SetCurrent (aCurrent: Integer); inline;
      function CalculateCurrent (aPosition: ALLEGRO_MOUSE_EVENT): Integer; inline;
    protected
    (* Tells widget wants key focus. *)
      function WantsKeyFocus: Boolean; override;

    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      procedure onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); override;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
      procedure onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT); override;
    (* Handler for the ALLEGRO_EVENT_MOUSE_AXES. *)
      procedure onMouseMove (Mouse: ALLEGRO_MOUSE_EVENT); override;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Creates a slider. *)
      constructor CreateSlider
        (const aMax: Integer; const aOrientation: TOrientation);
    (* Draws option list. *)
      procedure Draw; override;

    (* Current value. *)
      property Value: Integer read fCurrent write SetCurrent;

    (* Event triggered when value changes. *)
      property OnChange: TNotifyEvent read fOnChange write fOnChange;
    end;



  (* Text input field. *)
    TTextEntry = class (TWidget)
    private
      fText: ALLEGRO_USTRptr;
      fLeftPos, fCursorPos: Integer;

      fOnTextChanges: TNotifyEvent;

      procedure MaybeScroll;

      function GetText: AL_STR;
      procedure SetText (const aText: AL_STR);
    protected
    (* Tells widget wants focus. *)
      function WantsKeyFocus: Boolean; override;
    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      procedure onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); override;
    public
    (* Constructor. *)
      constructor Create; override;
      constructor CreateTextEntry (const aText: AL_STR);
    (* Destructor. *)
      destructor Destroy; override;
    (* Renders the widget. *)
      procedure Draw; override;

    (* Access to the input string. *)
      property Text: AL_STR read GetText write SetText;

    (* Event triggered when text changes. *)
      property OnTextChanges: TNotifyEvent
        read fOnTextChanges write fOnTextChanges;
    end;

implementation

  uses
    Common,
    al5primitives,
    sysutils;

  const
    CURSOR_WIDTH = 8;

  type
  (* Saves the Allegro state when created and restores it when destroyed. *)
    TSaveState = class (TObject)
    private
      State: ALLEGRO_STATE;
    public
    (* Creates (and saves) the Allegro state. *)
      constructor Create (const Save: Integer = ALLEGRO_STATE_ALL);
    (* Restores (and destroys) the Allegro state. *)
      destructor Destroy; override;
    end;



(*
 * TSaveState
 ***************************************************************************)

(* Constructor. *)
  constructor TSaveState.Create (const Save: Integer);
  begin
    inherited Create;
    al_store_state (Self.State, Save)
  end;



(* Destructor .*)
  destructor TSaveState.Destroy;
  begin
    al_restore_state (State);
    inherited Destroy
  end;



(*
 * TDialog
 ***************************************************************************)

  procedure TDialog.SetTextFont (aFont: ALLEGRO_FONTptr);
  begin
    if fTextFont <> Nil then al_destroy_font (fTextFont);
    if aFont <> Nil then
      fTextFont := aFont
    else
      fTextFont := al_create_builtin_font
  end;



  function TDialog.GetWidget (const Ndx: Integer): TWidget;
  begin
    Result := fAllWidgets[Ndx]
  end;



(* Updates dialog. *)
  procedure TDialog.Update;

    procedure RelativeCoordinates
      (const aNdx: Integer; var Mouse: ALLEGRO_MOUSE_EVENT);
    begin
        Mouse.x := Mouse.x - fAllWidgets[aNdx].fX;
        Mouse.y := Mouse.y - fAllWidgets[aNdx].fY;
    end;

    function GetWidgetMouse (var Mouse: ALLEGRO_MOUSE_EVENT): Integer;
    begin
      Result := Self.SearchWidgetIn (Mouse.x, Mouse.y);
      if Result >= 0 then RelativeCoordinates (Result, Mouse)
    end;

  var
    Ndx: Integer;
  begin
    while al_get_next_event (fEventQueue, fEvent) do
      case fEvent.ftype of
      ALLEGRO_EVENT_DISPLAY_CLOSE:
        Self.Terminate;

      ALLEGRO_EVENT_KEY_CHAR:
        case fEvent.keyboard.keycode of
        ALLEGRO_KEY_ESCAPE:
          Self.Terminate;
        ALLEGRO_KEY_TAB:
          if (fEvent.keyboard.modifiers and ALLEGRO_KEYMOD_SHIFT) <> 0 then
            Self.SetKeyFocus (Self.SearchKeyFocus (fKeyFocus - 1, -1))
          else
            Self.SetKeyFocus (Self.SearchKeyFocus (fKeyFocus + 1));
        else
          if (fKeyFocus >= 0) and fAllWidgets[fKeyFocus].Enabled then
            fAllWidgets[fKeyFocus].onKeyChar (fEvent.keyboard);
        end;

      ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
        begin
          Ndx := GetWidgetMouse (fEvent.mouse);
          if Ndx <> fMouseDownWidget then
          begin
            if fMouseDownWidget >= 0 then
              fAllWidgets[fMouseDownWidget].onMouseUp;
            fMouseDownWidget := Ndx
          end;
          Self.SetKeyFocus (Ndx);
          if (Ndx >= 0) and fAllWidgets[Ndx].Enabled then
            fAllWidgets[Ndx].onMouseDown (fEvent.mouse)
        end;
      ALLEGRO_EVENT_MOUSE_BUTTON_UP:
        if fMouseDownWidget >= 0 then
        begin
          fAllWidgets[fMouseDownWidget].onMouseUp;
          fMouseDownWidget := -1
        end;
      ALLEGRO_EVENT_MOUSE_AXES:
        if (fMouseDownWidget >= 0)
        and fAllWidgets[fMouseDownWidget].Enabled
        then begin
          RelativeCoordinates (fMouseDownWidget, fEvent.mouse);
          fAllWidgets[fMouseDownWidget].onMouseMove (fEvent.mouse)
        end;
      end
  end;



(* Renders dialog. *)
  procedure TDialog.Draw;
  var
    cx, cy, cw, ch, Ndx: Integer;
  begin
    al_get_clipping_rectangle (cx, cy, cw, ch);
    al_clear_to_color (fBgColor);
    try
      for Ndx := 0 to fNextWidget - 1 do
      begin
        al_set_clipping_rectangle (
          fAllWidgets[Ndx].X, fAllWidgets[Ndx].Y,
          fAllWidgets[Ndx].Width, fAllWidgets[Ndx].Height
        );
        fAllWidgets[Ndx].Draw
      end
    finally
      al_set_clipping_rectangle (cx, cy, cw, ch);
      al_flip_display
    end
  end;



(* Looks for key focus. *)
  function TDialog.SearchKeyFocus (const aNdx, aInc: Integer): Integer;
  begin
    Result := aNdx; if Result >= fNextWidget then Result := 0;
    repeat
      if fAllWidgets[Result].Enabled and fAllWidgets[Result].WantsKeyFocus then
        Exit;
    { Next widget. }
      Inc (Result, aInc);
      if Result >= fNextWidget then Result := 0;
      if Result < 0 then Result := fNextWidget - 1
    until Result = aNdx;
  { If here, no widget wants key focus. }
    Result := -1
  end;



(* Sets key focus. *)
  procedure TDialog.SetKeyFocus (const aNdx: Integer);
  begin
  { Only if focus changes. }
    if aNdx <> fKeyFocus then
    begin
    { If new focus doesn't want focus, then don't change. }
      if (0 <= aNdx) and (aNdx < fNextWidget) then
        if not fAllWidgets[aNdx].Enabled
        or not fAllWidgets[aNdx].WantsKeyFocus
        then
          Exit;
    { Current widget with focus lost it. }
      if fKeyFocus >= 0 then fAllWidgets[fKeyFocus].fHasKeyFocus := False;
    { New focus. }
      fKeyFocus := aNdx;
      if (0 <= fKeyFocus) and (fKeyFocus < fNextWidget) then
        fAllWidgets[fKeyFocus].fHasKeyFocus := True
    end
  end;



(* Looks for a widget in the given pixel. *)
  function TDialog.SearchWidgetIn (const aX, aY: Integer): Integer;
  begin
    for Result := fNextWidget - 1 downto 0 do
      if fAllWidgets[Result].IsInside (ax, ay) then
        Exit;
  { No widget found. }
    Result := -1
  end;



(* Constructor. *)
  constructor TDialog.Create (const aGridM, aGridN: Integer);
  begin
    inherited Create;
    SetLength (fAllWidgets, MIN_WIDGETS);
    fGridM := aGridM; fGridN := aGridN;
    fPadX := 1; fPadY := 1;
    fKeyFocus := -1; fMouseDownWidget := -1;

    fBgColor :=         al_map_rgb (204, 204, 204);
    fFgColor :=         al_map_rgb (  0,   0,   0);
    fFgDisabledColor := al_map_rgb (  51, 51,  51);
    fBgTextColor :=     al_map_rgb (255, 255, 255);
    fSelectColor :=     al_map_rgb (  0,   0, 153);
    Self.SetTextFont (Nil)
  end;



(* Destructor. *)
  destructor TDialog.Destroy;
  var
    Ndx: Integer;
  begin
    if fEventQueue <> Nil then al_destroy_event_queue (fEventQueue);
    if fNextWidget > 0 then
      for Ndx := 0 to fNextWidget - 1 do
        fAllWidgets[Ndx].Free;
    if fTextFont <> Nil then al_destroy_font (fTextFont);
    inherited Destroy
  end;



(* Adds a new widget. Returns the widget index. *)
  function TDialog.Add (aWidget: TWidget; agx, agy, agw, agh: Integer): Integer;
  begin
    aWidget.fDialog := Self;
    aWidget.fGridX := agx; aWidget.fGridY := agy;
    aWidget.fGridW := agw; aWidget.fGridH := agh;

    Result := fNextWidget;
    if fNextWidget >= Length (fAllWidgets) then
      SetLength (fAllWidgets, Length (fAllWidgets) * 2);
    fAllWidgets[fNextWidget] := aWidget;
    Inc (fNextWidget)
  end;



(* Initializes the dialog.  Should call this after adding all widgets. *)
  procedure TDialog.Initialize;
  var
    lDisplay: ALLEGRO_DISPLAYptr;
    Ndx,
    SizeX, SizeY: Integer;
  begin
    lDisplay := al_get_current_display;
  { Calculate grid cell sizes. }
    SizeX := al_get_display_width (lDisplay) div fGridM;
    SizeY := al_get_display_height (lDisplay) div fGridN;
  { Adjust widget position and size, and initializes it. }
    for Ndx := 0 to fNextWidget - 1 do
    begin
      fAllWidgets[Ndx].AdjustSize (SizeX, SizeY, fPadX, fPadY);
      fAllWidgets[Ndx].initialize
    end;
  { Key focus. }
    Self.SetKeyFocus (Self.SearchKeyFocus (0));
  { Event management. }
    fEventQueue := al_create_event_queue;
    al_register_event_source (fEventQueue, al_get_display_event_source (lDisplay));
    al_register_event_source (fEventQueue, al_get_keyboard_event_source);
    al_register_event_source (fEventQueue, al_get_mouse_event_source);
  { Start. }
    fTerminated := False
  end;



(* Executes the dialog. *)
  procedure TDialog.Run;
  begin
    try
      repeat
        Self.Update;
        Self.Draw
      until fTerminated
    except
      on Error: Exception do
        AbortExample (al_string_to_str (Error.Message))
    end
  end;



(* Terminate dialog. *)
  procedure TDialog.Terminate;
  begin
    fTerminated := True
  end;



(*
 * TWidget
 ***************************************************************************)

  procedure TWidget.AdjustSize (aSizeX, aSizeY, aPadX, aPadY: Integer);
  begin
    fX := aSizeX * fGridX + aPadX;
    fY := aSizeY * fGridY + aPadY;
    fWidth := aSizeX * fGridW - aPadX - 1;
    fHeight := aSizeY * fGridH - aPadY - 1
  end;



(* Check if wants focus. *)
  function TWidget.WantsKeyFocus: Boolean;
  begin
    Result := False
  end;



(* Event handlers. *)
{$IFDEF FPC} { Ignore warnings. }
  {$PUSH}
  {$WARN 5024 OFF : Parameter "$1" not used}
{$ENDIF}
  procedure TWidget.onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT); begin end;

  procedure TWidget.onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT); begin end;

  procedure TWidget.onMouseUp; begin end;

  procedure TWidget.onMouseMove (Mouse: ALLEGRO_MOUSE_EVENT); begin end;
{$IFDEF FPC} {$POP} {$ENDIF}


(* Constructor. *)
  constructor TWidget.Create;
  begin
    inherited Create;
    fEnabled := True;
    fHasKeyFocus := False;
    fTag := 0
  end;



(* initializes. *)
  procedure TWidget.initialize; begin end;



(* Tells if pixel is inside. *)
  function TWidget.IsInside (const aX, aY: Integer): Boolean;
  begin
    Result := (fX <= aX) and (aX <= fX + fWidth) and
              (fY <= aY) and (aY <= fY + fHeight)
  end;



(*
 * TLabel
 ***************************************************************************)

(* Creates the label. *)
  constructor TLabel.CreateLabel
    (const aCaption: AL_STR; const aCentered: Boolean);
  begin
    inherited Create;
    fCentered := aCentered;
    fCaption := aCaption
  end;



(* Renders label. *)
  procedure TLabel.Draw;
  var
    State: TSaveState;
    Clr: ALLEGRO_COLOR;
  begin
    if Self.Enabled then
      Clr := Dialog.FgColor
    else
      Clr := Dialog.FgDisabledColor;
    State := TSaveState.Create;
    try
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      if fCentered then
        al_draw_text (
          Dialog.TextFont, Clr,
          fX + (fWidth div 2), fY,
          ALLEGRO_ALIGN_CENTRE,
          fCaption
        )
      else
        al_draw_text (
          Dialog.TextFont, Clr,
          fX, fY,
          0,
          fCaption
        )
    finally
    { Restore state, including blender. }
      State.Free
    end
  end;



(*
 * TBitmap
 ***************************************************************************)

  procedure TBitmap.SetBitmap (aBmp: ALLEGRO_BITMAPptr);
  begin
    if fOwnsBitmap and Assigned (fBitmap) then
      al_destroy_bitmap (fBitmap);
    fBitmap := aBmp
  end;



(* Constructor. *)
  constructor TBitmap.Create;
  begin
    inherited Create;
    fBitmap := Nil;
    fOwnsBitmap := True
  end;



  constructor TBitmap.CreateBitmap (aBmp: ALLEGRO_BITMAPptr; aOwns: Boolean);
  begin
    inherited Create;
    fBitmap := aBmp;
    fOwnsBitmap := aOwns
  end;



  constructor TBitmap.CreateBitmap (aWidth, aHeight: Integer);
  begin
    inherited Create;
    fBitmap := al_create_bitmap (aWidth, aHeight);
    fOwnsBitmap := fBitmap <> Nil
  end;



(* Destructor. *)
  destructor TBitmap.Destroy;
  begin
    if fOwnsBitmap and Assigned (fBitmap) then al_destroy_bitmap (fBitmap);
    inherited Destroy
  end;



(* Draws bitmap. *)
  procedure TBitmap.Draw;
  begin
    al_draw_scaled_bitmap (
      fBitmap,
      0, 0, al_get_bitmap_width (fBitmap), al_get_bitmap_height (fBitmap),
      fX, fY, fWidth, fHeight,
      0
    )
  end;



(*
 * TOptionList
 ***************************************************************************)

  procedure TOptionList.SetSelected (const aNdx: Integer);
  begin
    if fSelectedItem <> aNdx then
    begin
      fSelectedItem := aNdx;
      if (0 <= fSelectedItem) and (fSelectedItem < fItemList.Count)
      and Assigned (fOnChange) then
        fOnChange (Self)
    end
  end;



(* Check if wants focus. *)
  function TOptionList.WantsKeyFocus: Boolean;
  begin
    Result := True
  end;



(* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
  procedure TOptionList.onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT);
  begin
    case Event.keycode of
    ALLEGRO_KEY_UP:
      if fSelectedItem <= 0 then
        Self.SetSelected (fItemList.Count - 1)
      else
        Self.SetSelected (fSelectedItem - 1);
    ALLEGRO_KEY_DOWN:
      if fSelectedItem >= fItemList.Count - 1 then
        Self.SetSelected (0)
      else
        Self.SetSelected (fSelectedItem + 1);
    end
  end;



(* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
  procedure TOptionList.onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT);
  var
    Ndx: Integer;
  begin
    Ndx := Mouse.y div al_get_font_line_height (Dialog.TextFont);
    if (0 <= Ndx) and (Ndx < fItemList.Count) then Self.SetSelected (Ndx)
  end;



(* Constructor. *)
  constructor TOptionList.Create;
  begin
    inherited Create;
    fItemList := TStringList.Create
  end;



  constructor TOptionList.CreateOptionList (aOptions: array of AL_STR);
  var
    Ndx: Integer;
  begin
    inherited Create;
    fItemList := TStringList.Create;
    for Ndx := Low (aOptions) to High (aOptions) do
      fItemList.Add (al_str_to_string (aOptions[Ndx]))
  end;



(* Destructor. *)
  destructor TOptionList.Destroy;
  begin
    fItemList.Free;
    inherited Destroy
  end;



(* Initializes the widget. *)
  procedure TOptionList.Initialize;
  begin
    inherited Initialize;
    fSelectedItem := 0
  end;



(* Draws list. *)
  procedure TOptionList.Draw;
  var
    State: TSaveState;
    ClrText, ClrSelected: ALLEGRO_COLOR;
    Ndx, pX, pY, iY: Integer;
  begin
  { Colors depend if enabled and has focus. }
    if Self.Enabled then
    begin
      ClrText := Dialog.fgColor;
      if Self.KeyFocus then
        ClrSelected := Dialog.SelectedColor
      else
        ClrSelected := Dialog.fgColor
    end
    else begin
      ClrText := Dialog.FgDisabledColor;
      ClrSelected := Dialog.FgDisabledColor
    end;
  { Draw control. }
    State := TSaveState.Create;
    try
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
      for Ndx := 0 to fItemList.Count - 1 do
      begin
        if Ndx = fSelectedItem then
        begin
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
        end
        else
          al_draw_text (
            Dialog.TextFont, ClrText,
            pX, pY,
            0,
            al_string_to_str (fItemList[Ndx])
          );
        Inc (pY, iY)
      end
    finally
      State.Free
    end
  end;



(*
 * TSlider
 ***************************************************************************)

  procedure TSlider.SetCurrent (aCurrent: Integer);
  begin
    if aCurrent < 0 then aCurrent := 0;
    if aCurrent > fMax then aCurrent := fMax;
    if aCurrent <> fCurrent then
    begin
      fCurrent := aCurrent;
      if Assigned (fOnChange) then fOnChange (Self)
    end
  end;



  function TSlider.CalculateCurrent (aPosition: ALLEGRO_MOUSE_EVENT): Integer;
  begin
    if fOrientation = oHorizontal then
      Exit (Trunc ((fMax / fWidth) * aPosition.x))
    else { oVertical }
      Exit (Trunc ((fMax / fHeight) * aPosition.y))
  end;



(* Check if wants focus. *)
  function TSlider.WantsKeyFocus: Boolean;
  begin
    Result := True
  end;



(* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
  procedure TSlider.onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT);
  begin
    case Event.keycode of
    ALLEGRO_KEY_UP, ALLEGRO_KEY_LEFT:
      Self.SetCurrent (fCurrent - (fMax div 100));
    ALLEGRO_KEY_DOWN, ALLEGRO_KEY_RIGHT:
      Self.SetCurrent (fCurrent + (fMax div 100));
    end
  end;



(* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
  procedure TSlider.onMouseDown (Mouse: ALLEGRO_MOUSE_EVENT);
  begin
    Self.SetCurrent (Self.CalculateCurrent (Mouse))
  end;



(* Handler for the ALLEGRO_EVENT_MOUSE_AXES. *)
  procedure TSlider.onMouseMove (Mouse: ALLEGRO_MOUSE_EVENT);
  begin
    Self.SetCurrent (Self.CalculateCurrent (Mouse))
  end;



(* Constructor. *)
  constructor TSlider.Create;
  begin
    inherited Create;
    fOrientation := oHorizontal;
    fMax := 100; fCurrent := 0
  end;



(* Creates a slider. *)
  constructor TSlider.CreateSlider
        (const aMax: Integer; const aOrientation: TOrientation);
  begin
    inherited Create;
    fOrientation := aOrientation;
    fMax := aMax; fCurrent := 0
  end;



(* Draws option list. *)
  procedure TSlider.Draw;
  var
    State: TSaveState;
    Clr: ALLEGRO_COLOR;
    cX, cY: Integer;

    procedure DrawHorizontal;
    begin
      cY := fY + (fHeight div 2);
      cX := fX + Trunc (fWidth * (fCurrent / fMax)) - 2;

      al_draw_line (fX, cY, fX + fWidth, cY, Clr, 0);
      al_draw_filled_rectangle (cX - 2, fY, cX + 2, fY + fHeight, Clr)
    end;

    procedure DrawVertical;
    begin
      cX := fX + (fWidth div 2);
      cY := fY + Trunc (fHeight * (fCurrent / fMax)) - 2;

      al_draw_line (cX, fY, cX, fY + fHeight, Clr, 0);
      al_draw_filled_rectangle (fX, cY - 2, fX + fWidth, cY + 2, Clr)
    end;

  begin
  { Widget color. }
    if Self.Enabled then
    begin
      if Self.KeyFocus then
        Clr := Dialog.SelectedColor
      else
        Clr := Dialog.FgColor
    end
    else
      Clr := Dialog.FgDisabledColor;
  { Draw color. }
    State := TSaveState.Create;
    try
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_filled_rectangle (
        fX, fY, fX + fWidth, fY + fHeight,
        Dialog.BgColor
      );
      case fOrientation of
      oHorizontal:
        DrawHorizontal;
      oVertical:
        DrawVertical;
      end;
    finally
      State.Free
    end
  end;



(*
 * TTextEntry
 ***************************************************************************)

  procedure TTextEntry.MaybeScroll;
  var
    tw: LongInt;
    lUstrInfo: ALLEGRO_USTR_INFO;
  begin
    if fCursorPos < fLeftPos + 3 then
    begin
      if fCursorPos < 3 then fLeftPos := 0 else fLeftPos := fCursorPos -3
    end
    else while True do begin
      tw := al_get_ustr_width (
        Dialog.TextFont,
        al_ref_ustr (lUstrInfo, fText, fLeftPos, fCursorPos)
      );
      if Self.X + tw + CURSOR_WIDTH < Self.X + Self.Width then Exit;
      al_ustr_next (fText, fLeftPos)
    end
  end;



  function TTextEntry.GetText: AL_STR;
  begin
    Result := al_cstr (fText)
  end;

  procedure TTextEntry.SetText (const aText: AL_STR);
  begin
    al_ustr_assign_cstr (fText, aText);
    if Assigned (fOnTextChanges) then fOnTextChanges (Self)
  end;



(* Wants key focus. *)
  function TTextEntry.WantsKeyFocus: Boolean;
  begin
    Result := True
  end;



(* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
  procedure TTextEntry.onKeyChar (Event: ALLEGRO_KEYBOARD_EVENT);
  begin
    case Event.keycode of
    { Cursor position. }
    ALLEGRO_KEY_LEFT:
      al_ustr_prev (fText, fCursorPos);
    ALLEGRO_KEY_RIGHT:
      al_ustr_next (fText, fCursorPos);
    ALLEGRO_KEY_HOME:
      fCursorPos := 0;
    ALLEGRO_KEY_end:
      fCursorPos := al_ustr_size (fText);
    { Text edition. }
    ALLEGRO_KEY_BACKSPACE:
      if al_ustr_prev (fText, fCursorPos) then
      begin
        al_ustr_remove_chr (fText, fCursorPos);
        if Assigned (fOnTextChanges) then fOnTextChanges (Self)
      end;
    else
      if Event.unichar >= Ord (' ') then
      begin
        al_ustr_insert_chr (fText, fCursorPos, Event.unichar);
        Inc (fCursorPos, al_utf8_width (Event.unichar));
        if Assigned (fOnTextChanges) then fOnTextChanges (Self)
      end;
    end;
    Self.MaybeScroll
  end;



(* Constructor. *)
  constructor TTextEntry.Create;
  begin
    inherited Create;
    fCursorPos := 0; fLeftPos := 0;
    fText := al_ustr_new ('')
  end;

  constructor TTextEntry.CreateTextEntry (const aText: AL_STR);
  begin
    inherited Create;
    fCursorPos := 0; fLeftPos := 0;
    fText := al_ustr_new (aText)
  end;



(* Destructor. *)
  destructor TTextEntry.Destroy;
  begin
    al_ustr_free (fText);
    inherited Destroy
  end;



(* Renders the widget. *)
  procedure TTextEntry.Draw;
  var
    lUstrInfo: ALLEGRO_USTR_INFO;
    lSubStr: ALLEGRO_USTRptr;
    lState: TSaveState;
    lBgColor: ALLEGRO_COLOR;
    lX, lPostCursor, lSubW: Integer;
  begin
    lState := TSaveState.Create;
    try
    { Background. }
      if Self.Enabled then
        lBgColor := Dialog.BgTextColor
      else
        lBgColor := Dialog.BgColor;
      al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
      al_draw_filled_rectangle (fX, fY, fX + fWidth, fY + fHeight, lBgColor);
    { Text (and cursor). }
      if Self.KeyFocus then
      begin
        lX := Self.X;
        if fCursorPos > 0 then
        begin
          lSubStr := al_ref_ustr (lUstrInfo, fText, fLeftPos, fCursorPos);
          al_draw_ustr (
            Dialog.TextFont, Dialog.FgColor,
            Self.X, Self.Y,
            0,
            lSubStr
          );
          Inc (lX, al_get_ustr_width (Dialog.TextFont, lSubStr))
        end;

        if LongWord (fCursorPos) = al_ustr_size (fText) then
          al_draw_filled_rectangle (
            lX, Self.Y,
            lX+CURSOR_WIDTH, Self.Y+al_get_font_line_height (Dialog.TextFont),
            Dialog.FgColor
          )
        else begin
          lPostCursor := fCursorPos;
          al_ustr_next (fText, lPostCursor);
          lSubStr := al_ref_ustr (lUstrInfo, fText, fCursorPos, lPostCursor);
          lSubW := al_get_ustr_width (Dialog.TextFont, lSubStr);
          al_draw_filled_rectangle (
            lX, Self.Y,
            lX + lSubW, Self.Y + al_get_font_line_height (Dialog.TextFont),
            Dialog.FgColor
          );
          al_draw_ustr (Dialog.TextFont, lBgColor, lX, Self.Y, 0, lSubStr);
          Inc (lX, lSubW);
          al_draw_ustr (
            Dialog.TextFont, Dialog.FgColor,
            lX, Self.Y,
            0,
            al_ref_ustr (lUstrInfo, fText, lPostCursor, al_ustr_size (fText))
          )
        end
      end
      else
        al_draw_ustr (
          Dialog.TextFont, Dialog.FgColor,
          Self.X, Self.Y,
          0,
          al_ref_ustr (lUstrInfo, fText, fLeftPos, al_ustr_size (fText))
        )
    finally
      lState.Free
    end
  end;

end.
