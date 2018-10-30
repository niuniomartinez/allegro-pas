UNIT nihGUI;
(*< This is a GUI for example programs that require GUI-style interaction.
    It's intended to be as simple and transparent as possible (simplistic,
    even).

    Note that this isn't a direct translation from the original.  I (Ñuño) have
    try to make it even simpler an more Pascal-ish, hence the event handling.

    Also note that mouse handling isn't perfect.  I've get all calculation from
    the original C++ implementation.  See TODO.
 *)

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

INTERFACE

  USES
    Allegro5, al5Font,
    Classes, sysutils;

  TYPE
  (* Exception class used by dialogs and widgets. *)
    EnihGUI = CLASS (Exception);



  (* Forward declaration. *)
    TTheme  = CLASS;
    TDialog = CLASS;
    TWidget = CLASS;



  (* Encapsulates a theme. *)
    TTheme = CLASS (TObject)
    PRIVATE
      fbg, ffg, fHighLight: ALLEGRO_COLOR;
      fFont: ALLEGRO_FONTptr;
    PUBLIC
    (* Constructor.
       NIL font is fine if you don't use a widget that requires text. *)
      CONSTRUCTOR Create (CONST aFont: ALLEGRO_FONTptr = NIL);

      PROPERTY bg: ALLEGRO_COLOR READ fbg;
      PROPERTY fg: ALLEGRO_COLOR READ ffg;
      PROPERTY HighLight: ALLEGRO_COLOR READ fHighLight;
      PROPERTY Font: ALLEGRO_FONTptr READ fFont;
    END;



  (* Encapsulates a dialog. *)
    TDialog = CLASS (TObject)
    PRIVATE
      fDisplay: ALLEGRO_DISPLAYptr;
      fEventQueue: ALLEGRO_EVENT_QUEUEptr;

      fGridM, fGridN, fxPadding, fyPadding: INTEGER;
      fBg, fFg, fHighlight: ALLEGRO_COLOR;
      fFont: ALLEGRO_FONTptr;
    { I would use a container such as TObjectList, but we don't need a complex
      component management, just add stuff, so a Pascal ARRAY is enough. }
      fWidgets: ARRAY OF TWidget;
      fMouseOverWidget, fMouseDownWidget, fKeyWidget: TWidget;

      fDrawRequested, fTerminated: BOOLEAN;

      FUNCTION GetWidget (Ndx: INTEGER): TWidget; INLINE;

      PROCEDURE HandleKeyDown (CONST aEvent: ALLEGRO_KEYBOARD_EVENT);
      PROCEDURE HandleMouseAxes (CONST aEvent: ALLEGRO_MOUSE_EVENT);
      PROCEDURE HandleMouseButtonDown (CONST aEvent: ALLEGRO_MOUSE_EVENT);
      PROCEDURE HandleMouseButtonUp (CONST aEvent: ALLEGRO_MOUSE_EVENT);
      PROCEDURE CheckMouseOver (CONST mx, my: INTEGER);
    PROTECTED
    (* Handles the event.  It @bold(must) call the inherited method if event
       isn't handled to ensure correct handling. *)
      PROCEDURE HandleEvent (CONST aEvent: ALLEGRO_EVENT); VIRTUAL;
    PUBLIC
    (* Constructor.
      @param(aDisplay Display where dialog is.)
      @param(gm Grid columns.)
      @param(gn Grid rows.) *)
      CONSTRUCTOR Create (aDisplay: ALLEGRO_DISPLAYptr; CONST gm, gn: INTEGER);
    (* Destructor.  It destroys widgets. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Adds a widget to the dialog.
      @param(aWidget Widget to add.)
      @return(Index of component.) *)
      FUNCTION Add (aWidget: TWidget; CONST gx, gy, gw, gh: INTEGER): INTEGER;
    (* Prepare dialog for execution.  You should call this after adding the
       widgets. *)
      PROCEDURE Initialize; VIRTUAL;
    (* Request the dialog to be drawn. @seealso(DrawRequested) *)
      PROCEDURE RequestDraw;
    (* Tells the dialog to terminate. *)
      PROCEDURE Terminate; VIRTUAL;
    (* Runs a single step on dialog.
       @param(Block If set to @true it wait's the event.  Else, it returns
              inmediatelly fi there's no events waiting.)
       @seealso(Draw) *)
      PROCEDURE RunStep (CONST Block: BOOLEAN);
    (* Renders dialog. @seealso(RunStep) *)
      PROCEDURE Draw;

    (* Background color. *)
      PROPERTY bg: ALLEGRO_COLOR READ fBg WRITE fBg;
    (* Foreground color. *)
      PROPERTY fg:  ALLEGRO_COLOR READ fFg  WRITE fFg;
    (* Highlighted color. *)
      PROPERTY Highlight: ALLEGRO_COLOR READ fHighlight WRITE fHighlight;
    (* Text font. *)
      PROPERTY Font: ALLEGRO_FONTptr READ fFont;
    (* Direct access to widets. *)
      PROPERTY Widgets[Ndx: INTEGER]: TWidget READ GetWidget;
    (* Tells if somebody requested the dialog to be drawn. *)
      PROPERTY DrawRequested: BOOLEAN READ fDrawRequested;
    (* Tells if somebody requested the dialog to finalize. *)
      PROPERTY Terminated: BOOLEAN READ fTerminated;
    END;



  (* Base class for dialog components. *)
    TWidget = CLASS (TObject)
    PRIVATE
      fOwner: TDialog;
      fGridX, fGridY, fGridW, fGridH: INTEGER;
      fEnabled: BOOLEAN;

      FUNCTION getWidth: INTEGER; INLINE;
      FUNCTION getHeight: INTEGER; INLINE;
    (* Configures widget. *)
      PROCEDURE Configure (CONST axSize, aySize, axPadding, ayPadding: INTEGER);
    PROTECTED
    (* Widget limits. *)
      fX1, fY1, fX2, fY2: INTEGER;

    (* Handles key down event. *)
      PROCEDURE HandleKeyDown (aEvent: ALLEGRO_KEYBOARD_EVENT); VIRTUAL;
    (* Handles mouse button hold(?). *)
      PROCEDURE HandleMouseButtonHold (aX, aY: INTEGER); VIRTUAL;
    (* Handles mouse button down. *)
      PROCEDURE HandleMouseButtonDown (aX, aY: INTEGER); VIRTUAL;
    (* Handles mouse button up. *)
      PROCEDURE HandleMouseButtonUp (aX, aY: INTEGER); VIRTUAL;
    (* Handles mouse button click. *)
      PROCEDURE HandleMouseClick (aX, aY: INTEGER); VIRTUAL;

    (* Owner dialog. *)
      PROPERTY fDialog: TDialog READ fOwner;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Renders the widget. *)
      PROCEDURE Draw; VIRTUAL; ABSTRACT;
    (* Tells if widget wants mouse focus.  Default is @true if enabled or
       @false if disabled. @seealso(Enabled) *)
      FUNCTION WantMouseFocus: BOOLEAN; VIRTUAL;
    (* Widget got mouse focus. *)
      PROCEDURE GotMouseFocus; VIRTUAL;
    (* Widget lost mouse focus. *)
      PROCEDURE LostMouseFocus; VIRTUAL;
    (* Tells if widget wants keyboard focus.  Default is @false. *)
      FUNCTION WantKeyFocus: BOOLEAN; VIRTUAL;
    (* Widget got keyboard focus. *)
      PROCEDURE GotKeyFocus; VIRTUAL;
    (* Widget lost keyboard focus. *)
      PROCEDURE LostKeyFocus; VIRTUAL;
    (* Checks if point is inside widget. *)
      FUNCTION IsInside (CONST aX, aY: INTEGER): BOOLEAN; VIRTUAL;

    (* Widget position. *)
      PROPERTY X: INTEGER READ fX1;
    (* Widget position. *)
      PROPERTY Y: INTEGER READ fY1;
    (* Widget width. *)
      PROPERTY Width: INTEGER READ getWidth;
    (* Widget height. *)
      PROPERTY Height: INTEGER READ getHeight;
    (* Tells if widget is enabled.  Default is @true. *)
      PROPERTY Enabled: BOOLEAN READ fEnabled WRITE fEnabled;
    END;



  (* A text label. *)
    TLabel = CLASS (TWidget)
    PRIVATE
      fText: STRING;
      fCentered: BOOLEAN;
    PUBLIC
    (* Empty constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates a label. *)
      CONSTRUCTOR CreateLabel
        (CONST aText: STRING; CONST aCenter: BOOLEAN = FALSE);
    (* Renders the label. *)
      PROCEDURE Draw; OVERRIDE;
    (* Labels doesn't want mouse focus. *)
      FUNCTION WantMouseFocus: BOOLEAN; OVERRIDE;

    (* Label text. *)
      PROPERTY Text: STRING READ fText WRITE fText;
    (* If draw label centered. *)
      PROPERTY Centered: BOOLEAN READ fCentered WRITE fCentered;
    END;



  (* Allows a list of strings that can be selected. *)
    TList = CLASS (TWidget)
    PRIVATE
      fItemList: TStringList;
      fSelectedItem: INTEGER;

      FUNCTION GetSelectedText: STRING; INLINE;
      PROCEDURE SetSelectedText (CONST aText: STRING); INLINE;
    PROTECTED
    (* Handles key down event. *)
      PROCEDURE HandleKeyDown (aEvent: ALLEGRO_KEYBOARD_EVENT); OVERRIDE;
    (* Handles mouse button click. *)
      PROCEDURE HandleMouseClick (aX, aY: INTEGER); OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Renders the label. *)
      PROCEDURE Draw; OVERRIDE;
    (* Tells if widget wants keyboard focus.  Default is @false. *)
      FUNCTION WantKeyFocus: BOOLEAN; OVERRIDE;

    (* Items. *)
      PROPERTY Items: TStringList READ fItemList;
    (* Index of selected item. *)
      PROPERTY SelectedItem: INTEGER READ fSelectedItem WRITE fSelectedItem;
    (* Text of selected item. *)
      PROPERTY SelectedText: STRING READ GetSelectedText WRITE SetSelectedText;
    END;



  (* Direction. *)
    TDirection = (
      wdHorizontal, wdVertical
    );



  (* A slider. *)
    TSlider = CLASS (TWidget)
    PRIVATE
      fCurrentValue, fMaxValue: INTEGER;
      fDirection: TDirection;
    PROTECTED
    (* Handles mouse button hold(?). *)
      PROCEDURE HandleMouseButtonHold (aX, aY: INTEGER); OVERRIDE;
    (* Handles mouse button down. *)
      PROCEDURE HandleMouseButtonDown (aX, aY: INTEGER); OVERRIDE;
    PUBLIC
    (* Empty constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates slider. *)
      CONSTRUCTOR CreateSlider (CONST aMax: INTEGER; CONST aDir: TDirection);
    (* Renders the slider. *)
      PROCEDURE Draw; OVERRIDE;

    (* Max value. *)
      PROPERTY Max: INTEGER READ fMaxValue WRITE fMaxValue;
    (* Current value. *)
      PROPERTY Value: INTEGER READ fCurrentValue WRITE fCurrentValue;
    (* slider direction. *)
      PROPERTY Dir: TDirection READ fDirection WRITE fDirection;
    END;



IMPLEMENTATION

  USES
    al5primitives;

  TYPE
  (* Helper class to save and restore states. *)
    TSaveState = CLASS (TObject)
    PRIVATE
      fState: ALLEGRO_STATE;
    PUBLIC
    (* Constructor.  Saves the state. *)
      CONSTRUCTOR Create (Save: INTEGER = ALLEGRO_STATE_ALL);
    (* Destructor.  Restores the state. *)
      DESTRUCTOR Destroy; OVERRIDE;
    END;



  (* Helper class to manage UNICODE strings. *)
    TUString = CLASS (TObject)
    (* Implementation note:  Note that this exists because it is a translation
       from the original example.  May be it is not neccesary.  Once Allegro's
       UString stuff is integrated with FPC/Delphi WideString stuff this class
       might use different API and implementation. *)
    PRIVATE
      fInfo: ALLEGRO_USTR_INFO;
      fUStr: ALLEGRO_USTRptr;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create
        (S: ALLEGRO_USTRptr; StartPos: INTEGER; EndPos: INTEGER = -1);

    (* Returns UString. *)
      PROPERTY UStr: ALLEGRO_USTRptr READ fUStr;
    END;



(*
 * TDialog
 *****************************************************************************)

  FUNCTION TDialog.GetWidget (Ndx: INTEGER): TWidget;
  BEGIN
    RESULT := fWidgets[Ndx]
  END;



  PROCEDURE TDialog.HandleKeyDown (CONST aEvent: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    IF aEvent.display = SELF.fDisplay THEN
    BEGIN
    { XXX think of something better when we need it }
      IF aEvent.keycode = ALLEGRO_KEY_ESCAPE THEN
        SELF.Terminate
      ELSE IF fKeyWidget <> NIL THEN
        fKeyWidget.HandleKeyDown (aEvent)
    END
  END;



  PROCEDURE TDialog.HandleMouseAxes (CONST aEvent: ALLEGRO_MOUSE_EVENT);
  BEGIN
    IF aEvent.display = SELF.fDisplay THEN
    BEGIN
      IF fMouseDownWidget <> NIL THEN
        fMouseDownWidget.HandleMouseButtonHold (aEvent.x, aEvent.y)
      ELSE
        SELF.CheckMouseOver (aEvent.x, aEvent.y)
    END
  END;



  PROCEDURE TDialog.HandleMouseButtonDown (CONST aEvent: ALLEGRO_MOUSE_EVENT);
  BEGIN
    IF aEvent.button = 1 THEN
    BEGIN
    { With touch input we may not receive mouse axes event before the touch
      so we must check which widget the touch is over. }
      SELF.CheckMouseOver (aEvent.x, aEvent.y);
      IF fMouseOverWidget <> NIL THEN
      BEGIN
        fMouseDownWidget := fMouseOverWidget;
        fMouseDownWidget.HandleMouseButtonDown (aEvent.x, aEvent.y);
      { Transfer key focus. }
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
      END
    END
  END;



  PROCEDURE TDialog.HandleMouseButtonUp (CONST aEvent: ALLEGRO_MOUSE_EVENT);
  BEGIN
    IF aEvent.button = 1 THEN
    BEGIN
      IF fMouseDownWidget <> NIL THEN
      BEGIN
        fMouseDownWidget.HandleMouseButtonUp (aEvent.x, aEvent.y);
        IF fMouseDownWidget.IsInside (aEvent.x, aEvent.y) THEN
          fMouseDownWidget.HandleMouseClick (aEvent.x, aEvent.y);
        fMouseDownWidget := NIL
     END
   END
  END;



  PROCEDURE TDialog.CheckMouseOver (CONST mx, my: INTEGER);
  VAR
    Ndx: INTEGER;
  BEGIN
    IF (fMouseOverWidget <> NIL) AND fMouseOverWidget.IsInside (mx, my) THEN
      EXIT; { no change }

    FOR Ndx := LOW (fWidgets) TO HIGH (fWidgets) DO
      IF fWidgets[Ndx].WantMouseFocus AND fWidgets[Ndx].IsInside (mx, my) THEN
      BEGIN
        fMouseOverWidget := fWidgets[Ndx];
        fMouseOverWidget.GotMouseFocus;
        EXIT
      END;

    IF fMouseOverWidget <> NIL THEN
    BEGIN
      fMouseOverWidget.LostMouseFocus;
      fMouseOverWidget := NIL
    END
  END;



(* Handles event. *)
  PROCEDURE TDialog.HandleEvent (CONST aEvent: ALLEGRO_EVENT);
  BEGIN
    CASE aEvent._type OF
      ALLEGRO_EVENT_KEY_CHAR:
        SELF.HandleKeyDown (aEvent.keyboard);
      ALLEGRO_EVENT_MOUSE_AXES:
        SELF.HandleMouseAxes (aEvent.mouse);
      ALLEGRO_EVENT_MOUSE_BUTTON_DOWN:
        SELF.HandleMouseButtonDown (aEvent.mouse);
      ALLEGRO_EVENT_MOUSE_BUTTON_UP:
        SELF.HandleMouseButtonUp (aEvent.mouse);
    END
  END;


(* Constructor. *)
  CONSTRUCTOR TDialog.Create
    (aDisplay: ALLEGRO_DISPLAYptr; CONST gm, gn: INTEGER);
  BEGIN
    INHERITED Create;
    fDisplay := aDisplay;

    fGridM := gm; fGridN := gn;
    fxPadding := 1; fyPadding := 1;
    fBg := al_map_rgb (255, 255, 255);
    fFg := al_map_rgb (0, 0, 0);
    fHighlight := al_map_rgb(128, 128, 255);
    fFont := al_load_font ('data/fixed_font.tga', 0, 0);

    fEventQueue := al_create_event_queue;
    al_register_event_source (fEventQueue, al_get_keyboard_event_source);
    al_register_event_source (fEventQueue, al_get_mouse_event_source);
    al_register_event_source
      (fEventQueue, al_get_display_event_source (fDisplay));
  { TODO:
    IF al_is_touch_input_installed THEN
      al_register_event_source
        (fEventQueue, al_get_touch_input_mouse_emulation_event_source)
  }
  END;



(* Destructor. *)
  DESTRUCTOR TDialog.Destroy;
  VAR
    Ndx: INTEGER;
  BEGIN
    al_unregister_event_source
      (fEventQueue, al_get_display_event_source (fDisplay));
    al_destroy_event_queue (fEventQueue);
    FOR Ndx := LOW (fWidgets) TO HIGH (fWidgets) DO fWidgets[Ndx].Free;
    IF fFont <> NIL THEN al_destroy_font (fFont);
    INHERITED Destroy
  END;



(* Adds widgets. *)
  FUNCTION TDialog.Add
    (aWidget: TWidget; CONST gx, gy, gw, gh: INTEGER): INTEGER;
  BEGIN
    IF aWidget <> NIL THEN
    BEGIN
      RESULT := Length (fWidgets);
      SetLength (fWidgets, RESULT + 1);
      fWidgets[RESULT] := aWidget;

      aWidget.fOwner := SELF;
      aWidget.fGridX := gx;
      aWidget.fGridY := gy;
      aWidget.fGridW := gw;
      aWidget.fGridH := gh
    END
    ELSE
      RAISE EnihGUI.Create ('Invalid widget added to dialog.')
  END;



(* Prepare dialog. *)
  PROCEDURE TDialog.Initialize;
  VAR
    xSize, ySize, Ndx: INTEGER;
  BEGIN
    xSize := al_get_display_width (fDisplay) DIV SELF.fGridM;
    ySize := al_get_display_height (fDisplay) DIV fGridN;

    fMouseOverWidget := NIL;
    fMouseDownWidget := NIL;
    fKeyWidget := NIL;
    FOR Ndx := LOW (fWidgets) TO HIGH (fWidgets) DO
      fWidgets[Ndx].Configure (xSize, ySize, fxPadding, fyPadding);

    SELF.RequestDraw;
    fTerminated := FALSE
  END;



(* Request the dialog to be drawn. *)
  PROCEDURE TDialog.RequestDraw;
  BEGIN
    fDrawRequested := TRUE
  END;



(* Tells the dialog to terminate. *)
  PROCEDURE TDialog.Terminate;
  BEGIN
    fTerminated := TRUE
  END;



(* Runs a single step on dialog. *)
  PROCEDURE TDialog.RunStep (CONST Block: BOOLEAN);
  VAR
    Event: ALLEGRO_EVENT;
    HaveEvents: BOOLEAN;
  BEGIN
    IF Block THEN
    BEGIN
      HaveEvents := TRUE;
      al_wait_for_event (fEventQueue, Event)
    END
    ELSE
      HaveEvents := al_get_next_event (fEventQueue, Event);
    IF HaveEvents THEN
    REPEAT
    { I would just call method HandleEvent, but I think some events should be
      handled this way. }
      CASE Event._type OF
        ALLEGRO_EVENT_DISPLAY_CLOSE: { User closed window. }
        SELF.Terminate;
      ALLEGRO_EVENT_DISPLAY_EXPOSE:
        SELF.RequestDraw;
      ELSE
        SELF.HandleEvent (Event)
      END
    UNTIL NOT al_get_next_event (fEventQueue, Event)
  END;



(* Renders dialog. *)
  PROCEDURE TDialog.Draw;
  VAR
    State: ALLEGRO_STATE;
    cx, cy, cw, ch, Ndx: INTEGER;
  BEGIN
    TRY
      al_store_state (State, ALLEGRO_STATE_ALL);
      al_get_clipping_rectangle (cx, cy, cw, ch);

      FOR Ndx := LOW (fWidgets) TO HIGH (fWidgets) DO
      BEGIN
        al_set_clipping_rectangle (
          fWidgets[Ndx].X, fWidgets[Ndx].Y,
          fWidgets[Ndx].Width, fWidgets[Ndx].Height
        );
        fWidgets[Ndx].Draw
      END
    FINAlLY
      al_set_clipping_rectangle (cx, cy, cw, ch);
      al_restore_state (State)
    END
  END;



(*
 * TWidget
 *****************************************************************************)

  FUNCTION TWidget.getWidth: INTEGER;
  BEGIN
    RESULT := fX2 - fX1 + 1
  END;



  FUNCTION TWidget.getHeight: INTEGER;
  BEGIN
    RESULT := fY2 - fY1 + 1
  END;



(* Configures widget. *)
  PROCEDURE TWidget.Configure
    (CONST axSize, aySize, axPadding, ayPadding: INTEGER);
  BEGIN
    fX1 := axSize * fGridX + axPadding;
    fY1 := aySize * fGridY + ayPadding;
    fX2 := axSize * (fGridX + fGridW) - axPadding - 1;
    fY2 := aySize * (fGridY + fGridH) - ayPadding - 1
  END;


{$PUSH}
  {$WARN 5024 OFF : Parameter "$1" not used}
(* Handles key down event. *)
  PROCEDURE TWidget.HandleKeyDown (aEvent: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    ; { Ignore. }
  END;




(* Handles mouse button hold(?). *)
  PROCEDURE TWidget.HandleMouseButtonHold (aX, aY: INTEGER);
  BEGIN
    ; { Ignore. }
  END;



(* Handles mouse button down. *)
  PROCEDURE TWidget.HandleMouseButtonDown (aX, aY: INTEGER);
  BEGIN
    ; { Ignore. }
  END;



(* Handles mouse button up. *)
  PROCEDURE TWidget.HandleMouseButtonUp (aX, aY: INTEGER);
  BEGIN
    ; { Ignore. }
  END;



(* Handles mouse button click. *)
  PROCEDURE TWidget.HandleMouseClick (aX, aY: INTEGER);
  BEGIN
    ; { Ignore. }
  END;
{$POP}



(* Constructor. *)
  CONSTRUCTOR TWidget.Create;
  BEGIN
    INHERITED Create;
    fEnabled := TRUE
  END;



(* Tells if widget wants mouse focus. *)
  FUNCTION TWidget.WantMouseFocus: BOOLEAN;
  BEGIN
    RESULT := SELF.Enabled
  END;



(* Widget got mouse focus. *)
  PROCEDURE TWidget.GotMouseFocus;
  BEGIN
    ; { Ignore. }
  END;



(* Widget lost mouse focus. *)
  PROCEDURE TWidget.LostMouseFocus;
  BEGIN
    ; { Ignore. }
  END;



(* Tells if widget wants keyboard focus. *)
  FUNCTION TWidget.WantKeyFocus: BOOLEAN;
  BEGIN
    RESULT := FALSE
  END;



(* Widget got keyboard focus. *)
  PROCEDURE TWidget.GotKeyFocus;
  BEGIN
    ; { Ignore. }
  END;



(* Widget lost keyboard focus. *)
  PROCEDURE TWidget.LostKeyFocus;
  BEGIN
    ; { Ignore. }
  END;



(* Checks if point is inside widget. *)
  FUNCTION TWidget.IsInside (CONST aX, aY: INTEGER): BOOLEAN;
  BEGIN
    RESULT := (aX >= fX1) AND (aY >= fY1) AND (aX <= fX2) AND (aY <= fY2)
  END;



(*
 * TLabel
 *****************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TLabel.Create;
  BEGIN
    INHERITED Create;
    fText := '';
    fCentered := FALSE
  END;



(* Constructor. *)
  CONSTRUCTOR TLabel.CreateLabel (CONST aText: STRING; CONST aCenter: BOOLEAN);
  BEGIN
    INHERITED Create;
    fText := aText;
    fCentered := aCenter
  END;



(* Render label. *)
  PROCEDURE TLabel.Draw;
  VAR
    Color: ALLEGRO_COLOR;
  BEGIN
    IF SELF.Enabled THEN
      Color := fDialog.fg
    ELSE
      Color := al_map_rgb (64, 64, 64);

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    IF fCentered THEN
      al_draw_text (
        fDialog.Font, color,
        (SELF.fX1 + SELF.fX2 + 1) DIV 2, SELF.Y,
        ALLEGRO_ALIGN_CENTRE,
        fText
      )
    ELSE
      al_draw_text (fDialog.Font, Color, SELF.fX1, SELF.fY1, 0, fText)
  END;



(* Tells if widget wants mouse focus. *)
  FUNCTION TLabel.WantMouseFocus: BOOLEAN;
  BEGIN
    RESULT := FALSE
  END;



(*
 * TList
 *****************************************************************************)

  FUNCTION TList.GetSelectedText: STRING;
  BEGIN
    RESULT := fItemList[fSelectedItem]
  END;



  PROCEDURE TList.SetSelectedText (CONST aText: STRING);
  BEGIN
    fItemList[fSelectedItem] := aText
  END;



(* Handles key down event. *)
  PROCEDURE TList.HandleKeyDown (aEvent: ALLEGRO_KEYBOARD_EVENT);
  BEGIN
    IF SELF.Enabled THEN
      CASE aEvent.keycode OF
      ALLEGRO_KEY_DOWN:
        IF fSelectedItem < fItemList.Count - 1 THEN
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



(* Handles mouse button click. *)
{$PUSH}
  {$WARN 5024 OFF : Parameter "$1" not used}
  PROCEDURE TList.HandleMouseClick (aX, aY: INTEGER);
  VAR
    Ndx: INTEGER;
  BEGIN
    IF SELF.Enabled THEN
    BEGIN
      Ndx := (aY - SELF.Y) DIV al_get_font_line_height (fDialog.Font);
      IF Ndx < fItemList.Count THEN
      BEGIN
        fSelectedItem := Ndx;
        fDialog.RequestDraw
      END
    END
  END;
{$POP}



(* Constructor. *)
  CONSTRUCTOR TList.Create;
  BEGIN
    INHERITED Create;
    fItemList := TStringList.Create;
    fSelectedItem := 0
  END;



(* Destructor. *)
  DESTRUCTOR TList.Destroy;
  BEGIN
    fItemList.Free;
    INHERITED Destroy
  END;



(* Renders the label. *)
  PROCEDURE TList.Draw;
  VAR
    bg: ALLEGRO_COLOR;
    FontHeight, Ndx, pY: INTEGER;
  BEGIN
    IF SELF.Enabled THEN bg := fDialog.bg ELSE bg := al_map_rgb (64, 64, 64);

    al_draw_filled_rectangle
      (SELF.fX1 + 1, SELF.fY1 + 1, SELF.fX2 - 1, SELF.fY2 - 1, bg);

    al_set_blender (ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
    FontHeight := al_get_font_line_height (fDialog.font);
    FOR Ndx := (fItemList.Count - 1) DOWNTO 0 DO
    BEGIN
      pY := SELF.fY1 + Ndx * FontHeight;

      IF Ndx = fSelectedItem THEN
        al_draw_filled_rectangle
          (fX1 + 1, pY, fX2 - 1, pY + FontHeight - 1, fDialog.Highlight);

      al_draw_text (fDialog.Font, fDialog.fg, fX1, pY, 0, fItemList[Ndx])
    END
  END;



(* Tells if widget wants keyboard focus. *)
  FUNCTION TList.WantKeyFocus: BOOLEAN;
  BEGIN
    RESULT := SELF.Enabled
  END;



(*
 * TSlider
 *****************************************************************************)

(* Handles mouse button hold(?). *)
  PROCEDURE TSlider.HandleMouseButtonHold (aX, aY: INTEGER);

    FUNCTION CLAMP (a, v, b: SINGLE): SINGLE; INLINE;
    BEGIN
      IF a >= v THEN EXIT (a);
      IF v >  b THEN EXIT (b);
      EXIT (v)
    END;

  VAR
    R: SINGLE;
  BEGIN
    IF SELF.Enabled THEN
    BEGIN
      IF fDirection = wdVertical THEN
        R := (SELF.fY2 - 1 - aY) / (SELF.Height - 2)
      ELSE
        R := (aX - 1 - SELF.fX1) / (SELF.Width - 2);
      fCurrentValue := TRUNC (CLAMP (0.0, R, 1.0) * fMaxValue);
      fDialog.RequestDraw
    END
  END;



(* Handles mouse button down. *)
  PROCEDURE TSlider.HandleMouseButtonDown (aX, aY: INTEGER);
  BEGIN
    IF SELF.Enabled THEN SELF.HandleMouseButtonHold (aX, aY)
  END;



(* Empty constructor. *)
  CONSTRUCTOR TSlider.Create;
  BEGIN
    INHERITED Create;
    fMaxValue := 1; fCurrentValue := 0;
    fDirection := wdHorizontal
  END;



(* Creates slider. *)
  CONSTRUCTOR TSlider.CreateSlider
    (CONST aMax: INTEGER; CONST aDir: TDirection);
  BEGIN
    INHERITED Create;
    fMaxValue := aMax; fCurrentValue := 0;
    fDirection := aDir
  END;



(* Renders the slider. *)
  PROCEDURE TSlider.Draw;
  VAR
    bg: ALLEGRO_COLOR;
    Left, Top, Right, Bottom: SINGLE;
    Ratio: DOUBLE;
    Position: INTEGER;
  BEGIN
    Ratio := fCurrentValue / fMaxValue;

    IF fDirection = wdVertical THEN
    BEGIN
      Left := fX1 + 0.5; Top := fY1 + 0.5;
      Right := fX2 + 0.5; Bottom := fY2 + 0.5;
      IF Enabled THEN
        bg := fDialog.fg
      ELSE
        bg := al_map_rgb (64, 64, 64);

      al_draw_rectangle (left, top, right, bottom, bg, 1);
      Position := TRUNC (bottom - 0.5 - (Ratio * (SELF.Height - 7)));

      al_draw_filled_rectangle
        (Left + 0.5, Position - 5, right - 0.5, Position, fDialog.fg)
    END
    ELSE BEGIN
      IF Enabled THEN
        bg := fDialog.bg
      ELSE
        bg := al_map_rgb (64, 64, 64);
      Position := (fY1 + fY2) DIV 2;

      al_draw_filled_rectangle (fX1, fY1, fX2, fY2, bg);
      al_draw_line (fX1, Position, fX2, Position, fDialog.fg, 0);

      Position := fX1 + TRUNC (Ratio * (SELF.width - 2));

      al_draw_filled_rectangle
        (Position - 2, fY1, Position + 2, fY2, fDialog.fg)
    END
  END;



(*
 * TSaveState
 *****************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TSaveState.Create (Save: INTEGER);
  BEGIN
    INHERITED Create;
    al_store_state (fState, Save)
  END;



(* Destructor. *)
  DESTRUCTOR TSaveState.Destroy;
  BEGIN
    al_restore_state (fState);
    INHERITED Destroy
  END;



(*
 * TTheme
 *****************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TTheme.Create (CONST aFont: ALLEGRO_FONTptr);
  BEGIN
    INHERITED Create;
    SELF.fbg := al_map_rgb (255, 255, 255);
    SELF.ffg := al_map_rgb (0, 0, 0);
    SELF.fHighLight := al_map_rgb (128, 128, 255);
    SELF.fFont := aFont
  END;



(*
 * TUString
 *****************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TUString.Create (S: ALLEGRO_USTRptr; StartPos, EndPos: INTEGER);
  BEGIN
    INHERITED Create;
    IF EndPos < 0 THEN EndPos := al_ustr_size (S);
    fUStr := al_ref_ustr (fInfo, S, StartPos, EndPos)
  END;

END.
