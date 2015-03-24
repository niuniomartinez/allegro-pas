UNIT alGUICommonCtrls;
(*<Defines some common controls, such as buttons and text inputs. *)

INTERFACE

  USES
    Allegro, alGUI;

  TYPE
  (* A simple button object. *)
    TalGUI_Button = CLASS (TalGUI_CustomButton)
    PRIVATE
      fCaption: STRING;
      PROCEDURE SetCaption   (CONST aCaption: STRING); INLINE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates the button.  Width and height are calculated from caption
      size. *)
      CONSTRUCTOR CreateButton (CONST aCaption: STRING; CONST aX, aY: INTEGER;
	 CONST aW: INTEGER=-1; CONST aH: INTEGER=-1);
    (* Initializes the control. *)
      PROCEDURE Initialize; OVERRIDE;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;

    (* Button label. *)
      PROPERTY Caption: STRING READ fCaption WRITE SetCaption;
    END;



  (* Slider control. *)
    TalGUI_Slider = CLASS (TalGUI_CustomSlider)
    PRIVATE
      fSliderFactor: DOUBLE;

      PROCEDURE AdjustSlider;
    PROTECTED
    (* Sets control width. *)
      PROCEDURE SetWidth (CONST aWidth: INTEGER); OVERRIDE;
    (* Sets control height. *)
      PROCEDURE SetHeight (CONST aHeight: INTEGER); OVERRIDE;
    (* Sets minimun value. *)
      PROCEDURE SetMin (CONST aMin: INTEGER); OVERRIDE;
    (* Sets maximun value. *)
      PROCEDURE SetMax (CONST aMax: INTEGER); OVERRIDE;

    (* Mouse clicked on control. *)
      FUNCTION MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates a slider. *)
      CONSTRUCTOR CreateSlider
	(CONST aX, aY: INTEGER; aDir: TalGUI_Direction;
	 CONST aW: INTEGER=-1; CONST aH: INTEGER=-1);
    (* Initializes the control. *)
      PROCEDURE Initialize; OVERRIDE;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;
    END;



  (* Scroll-bar control.

    Currently, it's just an alias for @code(TalGUI_Slider). *)
    TalGUI_ScrollBar = CLASS (TalGUI_Slider)
    END;



  (* A box with lines you can select. *)
    TalGUI_ListBox = CLASS (TalGUI_CustomItemListControl)
    PRIVATE
      fSelBgColor, fSelTxtColor: LONGINT;
    PROTECTED
    (* Informs the object that a mouse button has been clicked while the mouse
      was on top of the object.  Typically an object will perform its own
      mouse tracking as long as the button is held down, and only return from
      this message handler when it is released.
      @return(@true if message was handled, or @false if control isn't
       interested on it.) *)
      FUNCTION MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN; OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates the list.  Size will be calculated by @code(Initialize). *)
      CONSTRUCTOR CreateListBox
	(CONST aX, aY: INTEGER; CONST aW: INTEGER = -1; CONST aH: INTEGER = -1);
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;

    (* Initializes the control.  It's called by the @link(TalGUI_Dialog) object
      just before it displays the dialog. *)
      PROCEDURE Initialize; OVERRIDE;
    (* Sets colors to default. *)
      PROCEDURE SetDefaultColors; OVERRIDE;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;

    (* Text color for selected items. *)
      PROPERTY TextSelectedColor: LONGINT
	READ fSelTxtColor WRITE fSelTxtColor;
    (* Background color for selected items. *)
      PROPERTY BackgrounSelectedColor: LONGINT
	READ fSelBgColor WRITE fSelBgColor;
    END;



  (* A checkbox. *)
    TalGUI_CheckBox = CLASS (TalGUI_CustomCheckBox)
    PUBLIC
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;
    END;

IMPLEMENTATION

(*
 * TalGUI_Button
 ****************************************************************************)

  PROCEDURE TalGUI_Button.SetCaption (CONST aCaption: STRING);
  BEGIN
    fCaption := aCaption; SELF.RedrawMe
  END;



(* Creates the button. *)
  CONSTRUCTOR TalGUI_Button.Create;
  BEGIN
    INHERITED Create;
    fCaption := ''
  END;



(* Creates the button.  Width and height are calculated from caption size. *)
  CONSTRUCTOR TalGUI_Button.CreateButton
    (CONST aCaption: STRING; CONST aX, aY, aW, aH: INTEGER);
  BEGIN
    INHERITED Create;
    fCaption := aCaption;
    X := aX; Y := aY; Width := aW; Height := aH
  END;



(* Initializes the control. *)
  PROCEDURE TalGUI_Button.Initialize;
  VAR
    Margin: INTEGER;
  BEGIN
    INHERITED Initialize;
    IF Width < 0 THEN
    BEGIN
      Margin := al_text_length (Dialog.Style.TextFont, 'A');
      Width := al_text_length (Dialog.Style.TextFont, fCaption) + (Margin * 2);
      Height := Margin * 3
    END
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_Button.Draw (Bmp: AL_BITMAPptr);
  VAR
    pX, pY: INTEGER;
  BEGIN
    IF fPressed THEN
    BEGIN
      pX := X -1; pY := Y - 1
    END
    ELSE BEGIN
      pX := X; pY := Y
    END;
    Dialog.Style.DrawBox (
      Bmp, X, Y, X + Width - 1, Y + Height - 1,
      SELF.BackgroundColor,
      NOT fPressed
    );
    IF SELF.HasFocus THEN
      Dialog.Style.DrawFocusRect (
	Bmp,
	pX + 3, pY + 3,
	X + Width - 4, Y + Height - 4
      );
    IF SELF.Enabled THEN
      Dialog.Style.DrawText (
	Bmp, fCaption,
	pX + (Width DIV 2),
	pY + (Height DIV 2) - (al_text_height (al_font) DIV 2),
	SELF.Color,
	TRUE
      )
    ELSE
      Dialog.Style.DrawDisabledText (
	Bmp, fCaption,
	pX + (Width DIV 2),
	pY + (Height DIV 2) - (al_text_height (al_font) DIV 2),
	TRUE
      )
  END;



(*
 * TalGUI_Slider
 ****************************************************************************)

  CONST
  (* Slider width. *)
    SLIDER_W = 5;



  PROCEDURE TalGUI_Slider.AdjustSlider;
  BEGIN
    IF (Width > 0) AND (Height > 0) THEN
    CASE Direction OF
      agdHorizontal: fSliderFactor := (Width - SLIDER_W + 1) / ABS (Max - Min);
      agdVertical:   fSliderFactor := (Height - SLIDER_W + 1) / ABS (Max - Min);
    END
  END;



(* Sets control width. *)
  PROCEDURE TalGUI_Slider.SetWidth (CONST aWidth: INTEGER);
  BEGIN
    INHERITED SetWidth (aWidth);
    SELF.AdjustSlider
  END;



(* Sets control height. *)
  PROCEDURE TalGUI_Slider.SetHeight (CONST aHeight: INTEGER);
  BEGIN
    INHERITED SetHeight (aHeight);
    SELF.AdjustSlider
  END;



(* Sets minimun. *)
  PROCEDURE TalGUI_Slider.SetMin (CONST aMin: INTEGER);
  BEGIN
    INHERITED SetMin (aMin);
    SELF.AdjustSlider
  END;



(* Sets maximun. *)
  PROCEDURE TalGUI_Slider.SetMax (CONST aMax: INTEGER);
  BEGIN
    INHERITED SetMax (aMax);
    SELF.AdjustSlider
  END;



(* Mouse was clicked. *)
  FUNCTION TalGUI_Slider.MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN;

    PROCEDURE MyDraw; INLINE;
    BEGIN
      SELF.RedrawMe;
      SELF.Dialog.Draw
    END;

  VAR
    sX1, sY1, sX2, sY2, NewPos: INTEGER;

    PROCEDURE DoHorizontal;
    BEGIN
    { Test if mouse clicked inside the slider. }
      sY1 := SELF.Y; sY2 := sY1 + SELF.Width - 1;
      sX1 := SELF.X + TRUNC (Position * fSliderFactor); sX2 := sX1 + SLIDER_W;
      IF (sX1 <= aX) AND (aX <= sX2) AND (sY1 <= aY) AND (aY <= sY2) THEN
      BEGIN
	REPEAT
	  IF al_mouse_needs_poll THEN al_poll_mouse;
	  NewPos := TRUNC ((al_mouse_x - SELF.X) / fSliderFactor);
	  IF NewPos <> Position THEN
	  BEGIN
	    Position := NewPos; MyDraw
	  END;
	UNTIL al_mouse_b <> Button;
	EXIT;
      END
      ELSE BEGIN
      { Check if pressed page up or page down. }
	IF aX < sX1 THEN
	  Position := Position - Page
	ELSE
	  Position := Position + Page;
	MyDraw;
      { Wait until mouse release. }
	REPEAT
	  IF al_mouse_needs_poll THEN al_poll_mouse
	UNTIL al_mouse_b <> Button
      END
    END;


    PROCEDURE DoVertical;
    BEGIN
    { Test if mouse clicked inside the slider. }
      sX1 := SELF.X; sX2 := sX1 + SELF.Height - 1;
      sY1 := SELF.Y + TRUNC (Position * fSliderFactor); sY2 := sY1 + SLIDER_W;
      IF (sX1 <= aX) AND (aX <= sX2) AND (sY1 <= aY) AND (aY <= sY2) THEN
      BEGIN
	REPEAT
	  IF al_mouse_needs_poll THEN al_poll_mouse;
	  NewPos := TRUNC ((al_mouse_y - SELF.Y) / fSliderFactor);
	  IF NewPos <> Position THEN
	  BEGIN
	    Position := NewPos; MyDraw
	  END
	UNTIL al_mouse_b <> Button;
	EXIT;
      END
      ELSE BEGIN
      { Check if pressed page up or page down. }
	IF aY < sY1 THEN
	  Position := Position - Page
	ELSE
	  Position := Position + Page;
	MyDraw;
      { Wait until mouse release. }
	REPEAT
	  IF al_mouse_needs_poll THEN al_poll_mouse
	UNTIL al_mouse_b <> Button
      END
    END;

  BEGIN
    CASE Direction OF
      agdHorizontal: DoHorizontal;
      agdVertical:   DoVertical;
    END;
    RESULT := TRUE
  END;



(* Creates the slider. *)
  CONSTRUCTOR TalGUI_Slider.Create;
  BEGIN
    SELF.CreateSlider (-1, -1, agdHorizontal, -1, -1)
  END;



(* Creates the slider. *)
  CONSTRUCTOR TalGUI_Slider.CreateSlider
    (CONST aX, aY: INTEGER; aDir: TalGUI_Direction;
     CONST aW, aH: INTEGER);
  BEGIN
    INHERITED Create;
    Direction := aDir;
    X := aX; Y := aY; Width := aW; Height := aH;
    Min := 0; Max := 100; Position := 0
  END;



(* Initializes the control. *)
  PROCEDURE TalGUI_Slider.Initialize;
  VAR
    Size: INTEGER;
  BEGIN
    INHERITED Initialize;
    IF Width < 0 THEN
    BEGIN
      Size := al_text_length (Dialog.Style.TextFont, 'A') * 3;
      CASE Direction OF
      agdHorizontal:
	BEGIN
	  Width := ABS (Max - Min) + SLIDER_W; Height := Size;
	END;
      agdVertical:
	BEGIN
	  Height := ABS (Max - Min) + SLIDER_W; Width := Size;
	END;
      END
    END;
    SELF.AdjustSlider
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_Slider.Draw (Bmp: AL_BITMAPptr);
  VAR
    Offset: INTEGER;

    PROCEDURE DrawHorizontal;
    BEGIN
    { Trench. }
      Offset :=  Y + (SELF.Height DIV 2) - 1;
      Dialog.Style.DrawTrench (Bmp, X, Offset, X + Width - 1, Offset + 2);
    { Slider. }
      Offset := X + TRUNC (Position * fSliderFactor);
      Dialog.Style.DrawBox (
	Bmp, Offset, Y, Offset + SLIDER_W, Y + Height - 1,
	SELF.BackgroundColor,
	TRUE
      );
    END;

    PROCEDURE DrawVertical;
    BEGIN
    { Trench. }
      Offset :=  X + (SELF.Width DIV 2) - 1;
      Dialog.Style.DrawTrench (Bmp, Offset, y, Offset + 2, Y + Height - 1);
    { Slider. }
      Offset := Y + TRUNC (Position * fSliderFactor);
      Dialog.Style.DrawBox (
	Bmp, X, Offset, X + Width - 1, Offset + SLIDER_W,
	SELF.BackgroundColor,
	TRUE
      );
    END;

  BEGIN
    al_rectfill (
      Bmp, X, Y, x + Width - 1, Y + Height - 1,
      SELF.BackgroundColor
    );
    CASE Direction OF
      agdHorizontal: DrawHorizontal;
      agdVertical:   DrawVertical;
    END;
    IF HasFocus THEN
      Dialog.Style.DrawFocusRect (
	Bmp,
	X, Y,
	X + Width - 1, Y + Height - 1
      );
  END;



(*
 * TalGUI_ListBox
 ****************************************************************************)

(* Informs the object that a mouse button has been clicked. *)
  FUNCTION TalGUI_ListBox.MsgClick (CONST aX, aY, Button: INTEGER): BOOLEAN;
  VAR
    TextHeight, NewSel: INTEGER;
  BEGIN
    TextHeight := al_text_height (Dialog.Style.TextFont);
    NewSel := (aY - Y - (TextHeight DIV 2)) DIV TextHeight;
    SELF.selected := NewSel;
    RESULT := TRUE
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_ListBox.Create;
  BEGIN
    INHERITED Create;
  END;



(* Creates the box. *)
  CONSTRUCTOR TalGUI_ListBox.CreateListBox
    (CONST aX, aY, aW, aH: INTEGER);
  BEGIN
    SELF.Create;
    X := aX; Y := aY; Width := aW; Height := aH
  END;



(* Destructor. *)
  DESTRUCTOR TalGUI_ListBox.Destroy;
  BEGIN
    INHERITED Destroy
  END;



(* Initializes the control. *)
  PROCEDURE TalGUI_ListBox.Initialize;
  VAR
    Ndx, Tmp, Max: INTEGER;
  BEGIN
    INHERITED Initialize;
    IF Width < 0 THEN
    BEGIN
      Max := 0;
      FOR Ndx := SELF.Items.Count - 1 DOWNTO 0 DO
      BEGIN
	Tmp := al_text_length (Dialog.Style.TextFont, SELF.Items[Ndx]);
	IF Tmp > Max THEN Max := Tmp;
      END;
      Tmp := al_text_height (Dialog.Style.TextFont);
      Width := Max + (Tmp * 3)
    END;
    IF Height < 0 THEN
      Height := (SELF.Items.Count + 1) * al_text_height (Dialog.Style.TextFont)
  END;



(* Sets default colors. *)
  PROCEDURE TalGUI_ListBox.SetDefaultColors;
  BEGIN
    Color           := Dialog.Style.TextColor;
    BorderColor     := Dialog.Style.BorderColor;
    BackgroundColor := Dialog.Style.BackgroundTextBoxColor;
    fSelTxtColor    := Dialog.Style.SelectedTextColor;
    fSelBgColor     := Dialog.Style.SelectedBackgroundColor
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_ListBox.Draw (Bmp: AL_BITMAPptr);
  VAR
    SubBmp: AL_BITMAPptr;
    Ndx, Line, Increment: INTEGER;
  BEGIN
  { Set clipping, so text doesn't overloads the control }
    SubBmp := al_create_sub_bitmap (Bmp, X, Y, X + Width - 1, Y + Height - 1);
    TRY
    { Background. }
      al_rectfill (SubBmp, 0, 0, Width - 1, Height - 1, BackgroundColor);
    { Items. }
      Increment := al_text_height (Dialog.Style.TextFont);
      Line := Increment DIV 2;
      FOR Ndx := 0 TO Items.Count - 1 DO
      BEGIN
	IF Ndx = Selected THEN
	BEGIN
	  al_rectfill (
	    SubBmp, 0, Line, Width - 1, Line + Increment - 1, fSelBgColor
	  );
	  Dialog.Style.DrawText (
	    SubBmp, Items[Ndx], Increment DIV 2, Line, fSelTxtColor, FALSE
	  )
	END
	ELSE
	  Dialog.Style.DrawText (
	    SubBmp, Items[Ndx], Increment DIV 2, Line, SELF.Color, FALSE
	  );
	INC (Line, Increment)
      END;
    { Border. }
      al_rect (SubBmp, 0, 0, Width - 1, Height - 1, BorderColor);
      IF HasFocus THEN
	Dialog.Style.DrawFocusRect (SubBmp, 1, 1, Width - 2, Height - 2)
    FINALLY
    { Restore clipping. }
      al_destroy_bitmap (SubBmp)
    END
  END;



(*
 * TalGUI_CheckBox
 ****************************************************************************)

(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_CheckBox.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    Dialog.Style.DrawCheckbox (
      Bmp, X, Y, X + Width - 1, Y + Height - 1, SELF.Checked
    );
    IF HasFocus THEN
      Dialog.Style.DrawFocusRect (
	Bmp,
	X + 1, Y + 1,
	X + Width - 2, Y + Height - 2
      )
  END;

END.

