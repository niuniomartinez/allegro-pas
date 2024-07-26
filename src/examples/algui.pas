unit alGUI;
(* A very simple GUI to be used by the example programs.
 *
 * It's intended to be as simple and transparent as possible.  Delphi and
 * Lazarus programmers will see familiar stuff here.
 *
 * Remember this is not a complete GUI for final games or applications but it
 * can be used as gide or inspiration to build your own.
 *
 * Dialog is divided in two classes (TApplication and TDialog) just to make it
 * more readeable.
 *)
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
{ Needed to support classes. }
  {$IFNDEF FPC_DELPHI}
    {$MODE DELPHI}
  {$ENDIF}
{$ENDIF}

interface

  uses
    allegro5, al5base, al5font,
    Classes, Contnrs;

  const
  (* Window size. *)
    wWidth = 800; wHeight = 600;

  type
    TWidget = class; { Forward declaration. }



  (* Base class for examples.
   *
   * Much like Delphi/Lazarus TApplication.
   *)
    TApplication = class (TObject)
    private
      fTerminated: Boolean;
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;
      fDisplay: ALLEGRO_DISPLAYptr;
      fTextFont: ALLEGRO_FONTptr;
      fTitle: AL_STR;

      function GetTitle: String;
      procedure SetTitle (const aTitle: String);
    protected
    (* Process events.
     *
     * Descendant should call this if it doesn't process the event. *)
      procedure ProcessEvent (const aEvent: ALLEGRO_EVENT); virtual;
    (* Draw contents.
     *
     * Called by Run when needed. *)
      procedure Draw; virtual; abstract;
    public
    (* Destructor. *)
      destructor Destroy; override;
    (* Initialize the application.
     *
     * This initialize Allegro, keyboard, mouse and create the window.
     *)
      procedure Initialize; virtual;
    (* Execute the program. *)
      procedure Run;
    (* Tell the application that should terminate. *)
      procedure Terminate; virtual;
    (* Show an error message on window and wait user keypress. *)
      procedure ShowErrorMessage (const aText: String);

    (* Window title. *)
      property Title: String read GetTitle write SetTitle;
    (* Tells if terminate was called. *)
      property Terminated: Boolean read fTerminated;
    (* Pointer to system text font. *)
      property SysFont: ALLEGRO_FONTptr read fTextFont;
    end;




  (* Base class for dialogs.
   *
   * Its similar to Delphi/Lazarus TForm but see it extends TApplication.
   *)
    TDialog = class (TApplication)
    private
      fWidgetList: TObjectList;
      fClrBackground, fClrForeground,
      fClrDisabled,
      fClrSelectedBackground, fClrSelectedText: ALLEGRO_COLOR;
      fTextFont: ALLEGRO_FONTptr;
      fLineHeight, fKeyFocus, fMouseDownWidget: Integer;

      procedure DestroyTextFont;

      function GetWidgetCount: Integer;
      function GetWidget (const aNdx: Integer): TWidget;
      procedure SetTextFont (aFont: ALLEGRO_FONTptr);
    (* Look for a widget that wants key focus. *)
      function SearchKeyFocus(const aNdx:Integer; const aInc:Integer=1):Integer;
    (* Change key focus if widget wants it. *)
      procedure SetKeyFocus (const aNdx: Integer);
    protected
    (* Process events. *)
      procedure ProcessEvent (const aEvent: ALLEGRO_EVENT); override;
    (* Draw dialog. *)
      procedure Draw; override;
    public
    (* Constructor. *)
      constructor Create; virtual;
    (* Destructor. *)
      destructor Destroy; override;
    (* Initialize dialog. *)
      procedure Initialize; override;
    (* Add a new widget to the dialog.  Returns index. *)
      function AddWidget (aWidget: TWidget): Integer;
    (* Add a new widget to the dialog. *)
      procedure AppendWidget (aWidget: TWidget);
    (* Remove and destroy a widget from the dialog. *)
      procedure RemoveWidget (aWidget: TWidget);
    (* Extract a widget from the dialog. It doesn't destroy it. *)
      procedure ExtractWidget (aWidget: TWidget);
    (* Set dialog in initial state.  Call this after adding widgets. *)
      procedure Reset;
    (* Look for a widget in the given pixel.
     * Returns widget index or -1 if don't find any.
     *)
      function SearchWidgetIn (const aX, aY: Integer): Integer;

    (* How many widgets it has. *)
      property WidgetCount: Integer read GetWidgetCount;
    (* Indexed access to widgets.  It is zero-based. *)
      property Widget[const aNdx: Integer]: TWidget read GetWidget;
    (* Background color. *)
      property clrBackground: ALLEGRO_COLOR
        read fClrBackground write fClrBackground;
    (* Foreground color. *)
      property clrForeground: ALLEGRO_COLOR
        read fClrForeground write fClrForeground;
    (* Color used in disabled widgets. *)
      property clrDisabled: ALLEGRO_COLOR
        read fClrDisabled write fClrDisabled;
    (* Background color for selected widgets. *)
      property clrSelectedBackground: ALLEGRO_COLOR
        read fClrSelectedBackground write fClrSelectedBackground;
    (* Text color for selected widgets. *)
      property clrSelectedText: ALLEGRO_COLOR
        read fClrSelectedText write fClrSelectedText;
    (* Text font for widgets. *)
      property TextFont: ALLEGRO_FONTptr read fTextFont write SetTextFont;
    (* Text font height in pixels. *)
      property LineHeight: Integer read fLineHeight;
    end;



  (* Base class for GUI widgets. *)
    TWidget = class (TObject)
    private
      fDialog: TDialog;
      fX, fY, fWidth, fHeight: Integer;
      fCaption: AL_STR;
      fEnabled, fHasKeyFocus: Boolean;
      fTag: Integer;

      function GetCaption: String;
      procedure SetCaption (const aCaption: String);
    protected
    (* Set x coordinate. *)
      procedure SetX (const aValue: Integer); virtual;
    (* Set y coordinate. *)
      procedure SetY (const aValue: Integer); virtual;
    (* Set width. *)
      procedure SetWidth (const aValue: Integer); virtual;
    (* Set height. *)
      procedure SetHeight (const aValue: Integer); virtual;
    (* Set enabled. *)
      procedure SetEnabled (const aValue: Boolean); virtual;

    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      procedure onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT); virtual;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
      procedure onMouseDown (aMouse: ALLEGRO_MOUSE_EVENT); virtual;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_UP. *)
      procedure onMouseUp (aMouse: ALLEGRO_MOUSE_EVENT); virtual;
    (* Handler for the ALLEGRO_EVENT_MOUSE_AXES. *)
      procedure onMouseMove (aMouse: ALLEGRO_MOUSE_EVENT); virtual;

    (* Reference to the dialog. *)
      property Dialog: TDialog read fDialog;
    (* Caption in Allegro way. *)
      property CaptionStr: AL_STR read fCaption;
    public
    (* Constructor. *)
      constructor Create; overload; virtual;
      constructor Create (const aX, aY, aWidth, aHeight: Integer);
        overload; virtual;
    (* Tell if widget wants key focus.  Default returns false. *)
      function WantFocus: Boolean; virtual;
    (* Render the widget. *)
      procedure Draw; virtual; abstract;
    (* Check if given point is inside the widget. *)
      function IsInside (const aX, aY: Integer): Boolean;
    (* Move the widget the given pixels. *)
      procedure MoveBy (const aX, aY: Integer); virtual;

    (* Widget position. *)
      property X: Integer read fX write SetX;
      property Y: Integer read fY write SetY;
    (* Widget size. *)
      property Width: Integer read fWidth write SetWidth;
      property Height: Integer read fHeight write SetHeight;
    (* Widget caption. *)
      property Caption: String read GetCaption write SetCaption;
    (* Widget is enabled. *)
      property Enabled: Boolean read fEnabled write SetEnabled;
    (* Widget has focus. *)
      property Focus: Boolean read fHasKeyFocus;
    (* Tag value. *)
      property Tag: Integer read fTag write fTag;
    end;



  (* A label. *)
    TLabel = class (TWidget)
    private
      fAlign: Integer;
    public
    (* Constructor. *)
      constructor CreateLabel (
        const aX, aY: Integer;
        const aCaption: String;
        const aAlign: Integer = ALLEGRO_ALIGN_LEFT
      );
    (* Render the label. *)
      procedure Draw; override;

    (* Label alignment. *)
      property Align: Integer read fAlign write fAlign;
    end;



  (* A label with its own colors. *)
    TColoredLabel = class (TLabel)
    private
      fBgColor, fFgColor: ALLEGRO_COLOR;
    public
    (* Constructor. *)
      constructor CreateColoredLabel (
        const aX, aY: Integer;
        const aCaption: String;
        aBg, aFg: ALLEGRO_COLOR;
        const aAlign: Integer = ALLEGRO_ALIGN_LEFT
      );
    (* Render the label. *)
      procedure Draw; override;

    (* Label colors. *)
      property Background: ALLEGRO_COLOR read fBgColor write fBgColor;
      property Color: ALLEGRO_COLOR read fFgColor write fFgColor;
    end;



  (* To identify orientations. *)
    TOrientation = ( oHorizontal, oVertical );



  (* A slider bar. *)
    TSlider = class (TWidget)
    private
      fOrientation: TOrientation;
      fMin, fMax, fStep, fValue: Integer;
      fInverted, fPressed: Boolean;
      fOnChange: TNotifyEvent;

      procedure SetMin (aValue: Integer);
      procedure SetMax (aValue: Integer);
      procedure SetValue (aValue: Integer);
    protected
    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      procedure onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT); override;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_DOWN. *)
      procedure onMouseDown (aMouse: ALLEGRO_MOUSE_EVENT); override;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_UP. *)
      procedure onMouseUp (aMouse: ALLEGRO_MOUSE_EVENT); override;
    (* Handler for the ALLEGRO_EVENT_MOUSE_AXES. *)
      procedure onMouseMove (aMouse: ALLEGRO_MOUSE_EVENT); override;
    public
    (* Constructor. *)
      constructor Create; override;
    (* Creates a slider. *)
      constructor CreateSlider (
        aOrientation: TOrientation;
        aX, aY, aW, aH, aMin, aMax, aStep: integer
      );
    (* Tell if widget wants key focus.  Default returns false. *)
      function WantFocus: Boolean; override;
    (* Render the widget. *)
      procedure Draw; override;

    (* Slider orientation. *)
      property Orientation: TOrientation
        read fOrientation write fOrientation;
    (* Minimun value.  Default is 0. *)
      property Min: Integer read fMin write SetMin;
    (* Maximun value. Default is 100. *)
      property Max: Integer read fMax write SetMax;
    (* Step.  Default is 10.
       @seealso(Min) @seealso(Max) @seealso(Value) *)
      property Step: Integer read fStep write fStep;
    (* Current value. @seealso(Min) @seealso(Max) @seealso(step) *)
      property Value: Integer read fValue write SetValue;
    (* If @true slider will invert the axis.  Default is @false. *)
      property Inverted: Boolean read fInverted write fInverted;

    (* Event triggered when @link(Value) changes. *)
      property OnChange: TNotifyEvent read fOnChange write fOnChange;
    end;



  (* Base class for widgets that can be "clicked" with the mouse. *)
    TClickableWidget = class (TWidget)
    private
      fOnClick: TNotifyEvent;
    protected
    (* Handler for the ALLEGRO_EVENT_KEY_CHAR. *)
      procedure onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT); override;
    (* Handler for the ALLEGRO_EVENT_MOUSE_BUTTON_UP. *)
      procedure onMouseUp (aMouse: ALLEGRO_MOUSE_EVENT); override;
    (* Manages a click. *)
      procedure DoOnClick; virtual;
    public
    (* Widget want focus. *)
      function WantFocus: Boolean; override;
    (* CLick event. *)
      property OnClick: TNotifyEvent read fOnClick write fOnClick;
    end;



  (* A check box. *)
    TCheckBox = class (TClickableWidget)
    private
      fChecked: Boolean;
    protected
    (* Manages a click. *)
      procedure DoOnClick; override;
    public
    (* Constructor. *)
      constructor CreateCheckbox (
        const aX, aY, aWidth, aHeight: Integer;
        const aCaption: String
      );
    (* Render the widget. *)
      procedure Draw; override;

    (* Tell if checkbox is checked. *)
      property Checked: Boolean read fChecked write fChecked;
    end;

implementation

  uses
    Common,
    al5primitives, al5strings,
    sysutils;

(*
 * TApplication
 *************************************************************************)

  function TApplication.GetTitle: String;
  begin
    Result := al_str_to_string (fTitle)
  end;



  procedure TApplication.SetTitle (const aTitle: String);
  begin
    fTitle := al_string_to_str (aTitle);
    if Assigned (fDisplay) then
      al_set_window_title (fDisplay, fTitle)
  end;



  procedure TApplication.ProcessEvent (const aEvent: ALLEGRO_EVENT);
  begin
    case aEvent.ftype of
    ALLEGRO_EVENT_DISPLAY_CLOSE:
      Self.Terminate;
    ALLEGRO_EVENT_KEY_CHAR:
      if aEvent.keyboard.keycode = ALLEGRO_KEY_ESCAPE then
        Self.Terminate;
    end
  end;



  destructor TApplication.Destroy;
  begin
    if Assigned (fEventQueue) then al_destroy_event_queue (fEventQueue);
    if Assigned (fTextFont) then al_destroy_font (fTextFont);
    if Assigned (fDisplay) then al_destroy_display (fDisplay);
    inherited Destroy
  end;



  procedure TApplication.Initialize;
  begin
  { Initialize Allegro. }
    if not al_init or not al_install_keyboard or not al_install_mouse
    or not al_init_font_addon
    then
      raise Exception.Create ('Can''t initialize Allegro!');
  { Create window. }
    al_set_new_display_flags (ALLEGRO_WINDOWED);
    fDisplay := al_create_display (wWidth, wHeight);
    if not Assigned (fDisplay) then
      raise Exception.Create ('Can''t create window.');
  { System text font. }
    fTextFont := al_create_builtin_font;
  { Create the event queue. }
    fEventQueue := al_create_event_queue;
    if not Assigned (fEventQueue) then
    begin
      Self.ShowErrorMessage ('Can''t initialize event queue!');
      Self.Terminate;
      Exit
    end;
    al_register_event_source (fEventQueue, al_get_keyboard_event_source);
    al_register_event_source (fEventQueue, al_get_mouse_event_source);
    al_register_event_source (
      fEventQueue,
      al_get_display_event_source (fDisplay)
    );
  { Everything is correct. }
    fTerminated := False
  end;



  procedure TApplication.Run;
  var
    lEvent: ALLEGRO_EVENT;
  begin
    try
      while not fTerminated do
      begin
        if al_is_event_queue_empty (fEventQueue) then Self.Draw;
        al_wait_for_event (fEventQueue, @lEvent);
        Self.ProcessEvent (lEvent)
      end
    except
      on Error: Exception do
        Self.ShowErrorMessage (Error.Message)
    end
  end;



  procedure TApplication.Terminate;
  begin
    fTerminated := True
  end;



  procedure TApplication.ShowErrorMessage (const aText: String);
  const
    TextFontSize = 8;
    Margin = TextFontSize;
  var
    lMessage: AL_STR;
    lColor: ALLEGRO_COLOR;
    lKeyboardState: ALLEGRO_KEYBOARD_STATE;
    lIgnore: Boolean;
  begin
    if not al_is_primitives_addon_initialized then
      lIgnore := al_init_primitives_addon;
    {$IfDef FPC}lIgnore := lIgnore;{$EndIf} { Avoid compilation warning. }
  { Prepare to draw. }
    lMessage := al_string_to_str (aText);
    al_set_target_backbuffer (fDisplay);
    al_set_blender (ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
    lColor := al_map_rgb (255, 255, 255);
  { Draw message. }
    al_draw_filled_rectangle (
      0, 0,
      wWidth, TextFontSize * 4,
      al_map_rgb (255, 51, 51)
    );
    al_draw_rectangle (
      1.5, 1.5,
      wWidth - 1.5, TextFontSize * 4 - 1.5,
      lColor, 1
    );
    al_draw_text (fTextFont, lColor, Margin, Margin, 0, lMessage);
    al_draw_text (fTextFont, lColor, Margin, Margin * 2, 0, 'Press [C] to close');
    al_flip_display;
  { Wait until keypress. }
    repeat
      al_rest (0.1);
      al_get_keyboard_state (lKeyboardState)
    until al_key_down (lKeyboardState, ALLEGRO_KEY_C)
  end;



(*
 * TDialog
 *************************************************************************)

  procedure TDialog.DestroyTextFont;
  begin
    if Assigned (fTextFont) and (fTextFont <> Self.SysFont) then
      al_destroy_font (fTextFont)
  end;



  function TDialog.GetWidgetCount: Integer;
  begin
    Result := fWidgetList.Count
  end;



  function TDialog.GetWidget (const aNdx:Integer): TWidget;
  begin
    Result := TWidget (fWidgetList.Items[aNdx])
  end;



  procedure TDialog.SetTextFont (aFont: ALLEGRO_FONTptr);
  begin
    Self.DestroyTextFont;
    fTextFont := aFont;
    if assigned (fTextFont) then
      fLineHeight := al_get_font_line_height (fTextFont)
  end;



  function TDialog.SearchKeyFocus (const aNdx, aInc: Integer): Integer;
  begin
    Result := aNdx; if Result >= fWidgetList.Count then Result := 0;
    repeat
      if Self.GetWidget (Result).Enabled
      and Self.GetWidget (Result).WantFocus
      then
        Exit;
    { Next widget. }
      Inc (Result, aInc);
      if Result >= fWidgetList.Count then Result := 0;
      if Result < 0 then Result := fWidgetList.Count - 1
    until Result = aNdx;
  { If here, no widget wants key focus. }
    Result := -1
  end;



  procedure TDialog.SetKeyFocus (const aNdx: Integer);
  begin
  { Only if focus changes. }
    if aNdx <> fKeyFocus then
    begin
      if (0 > aNdx) or (aNdx >= fWidgetList.Count) then Exit;
    { If new focus doesn't want focus, then don't change. }
      if not Self.GetWidget (aNdx).Enabled
      or not Self.GetWidget (aNdx).WantFocus
      then
        Exit;
    { Current widget with focus lost it. }
      if fKeyFocus >= 0 then Self.GetWidget(fKeyFocus).fHasKeyFocus := False;
    { New focus. }
      fKeyFocus := aNdx;
      if (0 <= fKeyFocus) and (fKeyFocus < fWidgetList.Count) then
        Self.GetWidget(fKeyFocus).fHasKeyFocus := True
    end
  end;



  procedure TDialog.ProcessEvent (const aEvent: ALLEGRO_EVENT);
  var
    lWidget: TWidget;
    lNdx: Integer;

    procedure KeyCharEvent (const aKeyEvent: ALLEGRO_KEYBOARD_EVENT);
    begin
      case aKeyEvent.keycode of
      ALLEGRO_KEY_ESCAPE:
        Self.Terminate;
      ALLEGRO_KEY_TAB:
        if (aKeyEvent.modifiers and ALLEGRO_KEYMOD_SHIFT) = 0 then
          Self.SetKeyFocus (Self.SearchKeyFocus (fKeyFocus + 1))
        else
          Self.SetKeyFocus (Self.SearchKeyFocus (fKeyFocus - 1, - 1));
      else { otherwise }
        begin
          if fKeyFocus >= 0 then
          begin
            lWidget := Self.GetWidget (fKeyFocus);
            if lWidget.Enabled then lWidget.onKeyChar (aKeyEvent);
          end;
          inherited ProcessEvent (aEvent)
        end;
      end
    end;

    procedure MouseAxesEvent (const aMouseEvent: ALLEGRO_MOUSE_EVENT);
    begin
      if (fMouseDownWidget >= 0)
      and Self.GetWidget (fMouseDownWidget).Enabled
      then
        Self.GetWidget (fMouseDownWidget).onMouseMove (aMouseEvent)
      else
      { Try with current keyboard focus.  Otherwise it will not manage the mouse
        wheel unless the widget is being clicked.
      }
        if (fKeyFocus >= 0) and Self.GetWidget (fKeyFocus).Enabled then
          Self.GetWidget (fKeyFocus).onMouseMove (aMouseEvent)
    end;

    procedure MouseButtonDownEvent (const aMouseEvent: ALLEGRO_MOUSE_EVENT);
    begin
    { Find widget if any. }
      lNdx := Self.SearchWidgetIn (aMouseEvent.x, aMouseEvent.y);
      if lNdx < 0 then Exit;
      if lNdx <> fMouseDownWidget then
      begin
      { If another widget has been clicked (maybe it's blocked). }
        if fMouseDownWidget >= 0 then
          Self.GetWidget (fMouseDownWidget).onMouseUp (aMouseEvent);
        fMouseDownWidget := lNdx
      end;
    { Change focus and process. }
      Self.SetKeyFocus (fMouseDownWidget);
      lWidget := Self.GetWidget (fMouseDownWidget);
      if lWidget.Enabled then lWidget.onMouseDown (aMouseEvent)
    end;

    procedure MouseButtonUpEvent (const aMouseEvent: ALLEGRO_MOUSE_EVENT);
    begin
      if fMouseDownWidget >= 0 then
      begin
        Self.GetWidget (fMouseDownWidget).onMouseUp (aMouseEvent);
        fMouseDownWidget := -1
      end
    end;

  begin
    case aEvent.ftype of
    ALLEGRO_EVENT_KEY_CHAR:
      KeyCharEvent (aEvent.keyboard);
    ALLEGRO_EVENT_MOUSE_AXES:
      MouseAxesEvent (aEvent.mouse);
    ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
      MouseButtonDownEvent (aEvent.mouse);
    ALLEGRO_EVENT_MOUSE_BUTTON_UP:
      MouseButtonUpEvent (aEvent.mouse);
    else { otherwise }
      inherited ProcessEvent (aEvent);
    end
  end;



  procedure TDialog.Draw;
  var
    lNdx: Integer;
  begin
    try
      al_clear_to_color (fClrBackground);
      for lNdx := 0 to Self.WidgetCount - 1 do
        Self.GetWidget (lNdx).Draw
    finally
      al_flip_display
    end
  end;



  constructor TDialog.Create;
  begin
    inherited Create;
    fWidgetList := TObjectList.Create (True);
    fKeyFocus := -1; fMouseDownWidget := -1
  end;



  destructor TDialog.Destroy;
  begin
    Self.DestroyTextFont;
    fWidgetList.Free;
    inherited Destroy
  end;



  procedure TDialog.Initialize;
  var
    lIgnore: Boolean;
  begin
    inherited Initialize;
    if Self.Terminated then Exit;
    lIgnore := al_init_primitives_addon;
    lIgnore := lIgnore; { Avoid compilation warning. }
  { Init dialog. }
    Self.SetTextFont (Self.SysFont);
    fClrBackground :=         al_map_rgb (204, 204, 204);
    fClrForeground :=         al_map_rgb (  0,   0,   0);
    fClrDisabled :=           al_map_rgb (  51, 51,  51);
    fClrSelectedBackground := al_map_rgb (  0,   0, 153);
    fClrSelectedText :=       al_map_rgb (  0, 153,   0)
  end;



  function TDialog.AddWidget (aWidget: TWidget): Integer;
  begin
    Result := fWidgetList.Add (aWidget);
    aWidget.fDialog := Self
  end;



  procedure TDialog.AppendWidget (aWidget: TWidget);
  var
    lIgnore: Integer;
  begin
    lIgnore := Self.AddWidget (aWidget);
    lIgnore := lIgnore { Avoid compilation warning. }
  end;



  procedure TDialog.RemoveWidget (aWidget: TWidget);
  var
    lNdx: Integer;
  begin
    lNdx := fWidgetList.Remove (aWidget);
  { Safe focus. }
    if lNdx = fKeyFocus then fKeyFocus := -1;
    if lNdx = fMouseDownWidget then fMouseDownWidget := -1
  end;



  procedure TDialog.ExtractWidget (aWidget: TWidget);
  var
    lNdx: Integer;
  begin
    lNdx := fWidgetList.IndexOf (aWidget);
    if lNdx < 0 then Exit;
  { Extract widget. }
    if aWidget <> fWidgetList.Extract (aWidget) then
    { This error is very unlikely but... }
      raise Exception.Create ('Can''t extract widget!');
  { Safe focus. }
    if lNdx = fKeyFocus then fKeyFocus := -1;
    if lNdx = fMouseDownWidget then fMouseDownWidget := -1
  end;



  procedure TDialog.Reset;
  begin
    fMouseDownWidget := -1;
    Self.SetKeyFocus (Self.SearchKeyFocus (0))
  end;



  function TDialog.SearchWidgetIn (const aX, aY: Integer): Integer;
  begin
    for Result := fWidgetList.Count - 1 downto 0 do
      if Self.GetWidget (Result).IsInside (ax, ay) then
        Exit;
  { No widget found. }
    Result := -1
  end;



(*
 * TWidget
 *************************************************************************)

  function TWidget.GetCaption: String;
  begin
    Result := al_str_to_string (fCaption)
  end;



  procedure TWidget.SetCaption (const aCaption: String);
  begin
    fCaption := al_string_to_str (aCaption)
  end;



  procedure TWidget.SetX (const aValue: Integer);
  begin
    fX := aValue
  end;



  procedure TWidget.SetY (const aValue: Integer);
  begin
    fY := aValue
  end;



  procedure TWidget.SetWidth (const aValue: Integer);
  begin
    fWidth := aValue
  end;



  procedure TWidget.SetHeight (const aValue: Integer);
  begin
    fHeight := aValue
  end;



  procedure TWidget.SetEnabled (const aValue: Boolean);
  begin
    fEnabled := aValue
  end;



(* Event handlers. *)
{$IFDEF FPC} { Ignore warnings. }
  {$PUSH}
  {$WARN 5024 OFF : Parameter "$1" not used}
{$ENDIF}
  procedure TWidget.onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT); begin end;

  procedure TWidget.onMouseDown (aMouse: ALLEGRO_MOUSE_EVENT); begin end;

  procedure TWidget.onMouseUp (aMouse: ALLEGRO_MOUSE_EVENT); begin end;

  procedure TWidget.onMouseMove (aMouse: ALLEGRO_MOUSE_EVENT); begin end;
{$IFDEF FPC} {$POP} {$ENDIF}



  constructor TWidget.Create;
  begin
    inherited Create;
    Self.SetEnabled (True)
  end;



  constructor TWidget.Create (const aX, aY, aWidth, aHeight: Integer);
  begin
    inherited Create;
    Self.SetX (aX); Self.SetY (aY);
    Self.SetWidth (aWidth); Self.SetHeight (aHeight);
    Self.SetEnabled (True)
  end;



  function TWidget.WantFocus: Boolean;
  begin
    Result := False
  end;



  function TWidget.IsInside (const aX, aY: Integer): Boolean;
  begin
    Result := (fX < aX) and (aX <= fX + fWidth) and
              (fY < aY) and (aY <= fY + fHeight)
  end;



  procedure TWidget.MoveBy (const aX, aY: Integer);
  begin
    Self.SetX (fX + aX);
    Self.SetY (fY + aY)
  end;



(*
 * TLabel
 *************************************************************************)

  constructor TLabel.CreateLabel (
    const aX, aY: Integer;
    const aCaption: String;
    const aAlign: Integer
  );
  begin
    inherited Create (aX, aY, 0, 0);
    Self.Caption := aCaption;
    fAlign := aAlign
  end;



  procedure TLabel.Draw;
  begin
    al_draw_text (
      Self.Dialog.TextFont, Self.Dialog.clrForeground,
      Self.X, Self.Y, fAlign,
      Self.CaptionStr
    )
  end;



(*
 * TColoredLabel
 *************************************************************************)

  constructor TColoredLabel.CreateColoredLabel (
    const aX, aY: Integer;
    const aCaption: String;
    aBg, aFg: ALLEGRO_COLOR;
    const aAlign: Integer
  );
  begin
    inherited CreateLabel (aX, aY, aCaption, aAlign);
    fBgColor := aBg;
    fFgColor := aFg
  end;



  procedure TColoredLabel.Draw;
  var
    lWidth: Integer;
  begin
    lWidth := al_get_text_width (Self.Dialog.TextFont, fCaption);
    al_draw_filled_rectangle (
      Self.X + 0.5, Self.Y + 0.5,
      Self.X + lWidth + 0.5, Self.Y + Self.Dialog.LineHeight + 0.5,
      fBgColor
    );
    al_draw_text (
      Self.Dialog.TextFont, fFgColor,
      Self.X, Self.Y, fAlign,
      Self.CaptionStr
    )
  end;



(*
 * TSlider
 *************************************************************************)

  procedure TSlider.SetMin (aValue: Integer);
  begin
    if aValue <> fMin then
    begin
      fMin := aValue;
      if fMin > fMax then fMax := fMin + 1;
      if fValue < fMin then Self.SetValue (fMin)
    end
  end;



  procedure TSlider.SetMax (aValue: Integer);
  begin
    if aValue <> fMax then
    begin
      fMax := aValue;
      if fMin > fMax then fMin := fMax - 1;
      if fValue > fMax then Self.SetValue (fMax)
    end
  end;



  procedure TSlider.SetValue (aValue: Integer);
  begin
    if aValue <> fValue then
    begin
      fValue := Clamp (fMin, aValue, fMax);
      if assigned (fOnChange) then fOnChange (Self)
    end
  end;


  procedure TSlider.onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT);
  begin
    case aEvent.keycode of
    ALLEGRO_KEY_LEFT:
      if fOrientation = oHorizontal then
      begin
        if fInverted then
          Self.SetValue (fValue + 1)
        else
          Self.SetValue (fValue - 1)
      end;
    ALLEGRO_KEY_RIGHT:
      if fOrientation = oHorizontal then
      begin
        if fInverted then
          Self.SetValue (fValue - 1)
        else
          Self.SetValue (fValue + 1)
      end;
    ALLEGRO_KEY_UP:
      if fOrientation = oVertical then
      begin
        if fInverted then
          Self.SetValue (fValue + 1)
        else
          Self.SetValue (fValue - 1)
      end;
    ALLEGRO_KEY_DOWN:
      if fOrientation = oVertical then
      begin
        if fInverted then
          Self.SetValue (fValue - 1)
        else
          Self.SetValue (fValue + 1)
      end;
    ALLEGRO_KEY_PGUP:
      if fInverted then
        Self.SetValue (fValue + fStep)
      else
        Self.SetValue (fValue - fStep);
    ALLEGRO_KEY_PGDN:
      if fInverted then
        Self.SetValue (fValue - fStep)
      else
        Self.SetValue (fValue + fStep);
    end
  end;



  procedure TSlider.onMouseDown (aMouse: ALLEGRO_MOUSE_EVENT);
  var
    lPosition, lRange, lValue: Integer;
  begin
    fPressed := True;
  { Calculate the mouse position to move the slider. }
    lRange := fMax - fMin;
    if fOrientation = oHorizontal then
    begin
      lPosition := aMouse.x - Self.x;
      lValue := ((lPosition * lRange) div Self.Width)
    end
    else
    begin
      lPosition := aMouse.y - Self.y;
      lValue := ((lPosition * lRange) div Self.Height)
    end;
  { Set new value. }
    lValue := Clamp (fMin, lValue + fMin, fMax);
    if fInverted then lValue := fMax - lValue;
    Self.SetValue (lValue);
  end;



  procedure TSlider.onMouseUp (aMouse: ALLEGRO_MOUSE_EVENT);
  begin
    fPressed := False;
  end;



  procedure TSlider.onMouseMove (aMouse: ALLEGRO_MOUSE_EVENT);
  begin
    if fPressed then Self.onMouseDown (aMouse)
  end;



  constructor TSlider.Create;
  begin
    inherited Create;
    fOrientation := oHorizontal;
    fMin := 0; fMax := 100; fValue := 0;
    fStep := 10;
    fInverted := False; fPressed := False
  end;



  constructor TSlider.CreateSlider (
    aOrientation: TOrientation;
    aX, aY, aW, aH, aMin, aMax, aStep: integer
  );
  begin
    inherited Create (aX, aY, aW, aH);
    fOrientation := aOrientation;
    fMin := aMin; Self.SetMax (aMax); fValue := fMin;
    fStep := aStep;
    fInverted := False; fPressed := False
  end;


  function TSlider.WantFocus: Boolean;
  begin Result := True end;



  procedure TSlider.Draw;
  var
    lClr: ALLEGRO_COLOR;
    cX, cY: Single;
    lRange, lValue: Integer;

    procedure DrawHorizontal;
    begin
      cY := Self.Y + (Self.Height div 2) + 0.5;
      cX := Self.X + Trunc (Self.Width * (lValue / lRange)) - 1.5;

      al_draw_line (
        Self.X + 0.5, cY,
        Self.X + fWidth + 0.5, cY,
        lClr, 1
      );
      al_draw_filled_rectangle (
        cX - 2, Self.Y + 0.5,
        cX + 2, fY + Self.Height + 0.5,
        lClr
      )
    end;

    procedure DrawVertical;
    begin
      cX := Self.X + (Self.Width div 2) + 0.5;
      cY := Self.Y + TRUNC (Self.Height * (lValue / lRange)) - 1.5;

      al_draw_line (
        cX, Self.Y + 0.5,
        cX, Self.Y + Self.Height + 0.5,
        lClr, 0
      );
      al_draw_filled_rectangle (
        Self.X + 0.5, cY - 2,
        Self.X + Self.Width + 0.5, cY + 2,
        lClr
      )
    end;

  begin
  { Get slider properties. }
    if fInverted then lValue := fMax - fValue else lValue := fValue;
    if fMin < 0 then Inc (lValue, Abs (fMin));
    lRange := fMax - fMin;
  { Widget color. }
    if Self.Enabled then
    begin
      if Self.Focus THEN
        lClr := Self.Dialog.clrSelectedText
      else
        lClr := Self.Dialog.clrForeground
    end
    else
      lClr := Self.Dialog.clrDisabled;
  { Draw widget. }
    al_draw_filled_rectangle (
      Self.X + 0.5, Self.Y + 0.5,
      Self.X + Self.Width + 0.5, Self.Y + Self.Height + 0.5,
      Self.Dialog.clrBackground
    );
    case fOrientation of
    oHorizontal:
      DrawHorizontal;
    oVertical:
      DrawVertical;
    end
  end;



(*
 * TClickableWidget
 *************************************************************************)


  procedure TClickableWidget.onKeyChar (aEvent: ALLEGRO_KEYBOARD_EVENT);
  begin
    if aEvent.keycode
    in [ALLEGRO_KEY_ENTER, ALLEGRO_KEY_SPACE, ALLEGRO_KEY_PAD_ENTER]
    then
      Self.DoOnClick
  end;



  procedure TClickableWidget.onMouseUp (aMouse: ALLEGRO_MOUSE_EVENT);
  begin
    if Self.IsInside (aMouse.x, aMouse.y) then
      if (aMouse.button and $01) = 1 then
        Self.DoOnClick
  end;



  procedure TClickableWidget.DoOnClick;
  begin
    if Assigned (fOnClick) then fOnClick (Self)
  end;



  function TClickableWidget.WantFocus: Boolean;
  begin
    Result := True
  end;



(*
 * TCheckBox
 *************************************************************************)

  procedure TCheckBox.DoOnClick;
  begin
    fChecked := not fChecked;
    inherited DoOnClick
  end;



  constructor TCheckBox.CreateCheckbox (
    const aX, aY, aWidth, aHeight: Integer;
    const aCaption: String
  );
  begin
    inherited Create (aX, aY, aWidth, aHeight);
    Self.Caption := aCaption;
    fChecked := False
  end;



  procedure TCheckBox.Draw;
  var
    lBackColor, lForeColor: ALLEGRO_COLOR;

    procedure SetColors;
    begin
      if fChecked then
      begin
        lBackColor := Self.Dialog.clrForeground;
        lForeColor := Self.Dialog.clrBackground
      end
      else
      begin
        lBackColor := Self.Dialog.clrBackground;
        lForeColor := Self.Dialog.clrForeground
      end;
      if not Self.Enabled then
        lForeColor := Self.Dialog.clrDisabled
    end;

  begin
    SetColors;
    al_draw_filled_rectangle (
      Self.X + 0.5, Self.Y + 0.5,
      Self.X + Self.Width + 0.5, Self.Y + Self.Height + 0.5,
      lBackColor
    );
    al_draw_rectangle (
      Self.X + 0.5, Self.Y + 0.5,
      Self.X + Self.Width + 0.5, Self.Y + Self.Height + 0.5,
      lForeColor, 1
    );
    al_draw_text (
      Self.Dialog.TextFont, lForeColor,
      Self.X + Self.Width / 2,
      Self.Y + Self.Height / 2 - Self.Dialog.LineHeight / 2,
       ALLEGRO_ALIGN_CENTRE,
      Self.CaptionStr
    )
  end;

end.
