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
      CONSTRUCTOR Create (CONST aCaption: STRING; CONST aX, aY: INTEGER); OVERLOAD;
    (* Creaates the button. *)
      CONSTRUCTOR Create (CONST aCaption: STRING; CONST aX, aY, aW, aH: INTEGER;
	CONST aAlign: TalGUI_Alignment = agaLeft); OVERLOAD;
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
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Creates a slider with default size, width and height. *)
      CONSTRUCTOR Create (CONST aSize, aX, aY: INTEGER; aDir: TalGUI_Direction); OVERLOAD;
    (* Creates a slider. *)
      CONSTRUCTOR Create (CONST aMin, aMax, aX, aY, aW, aH: INTEGER;
	aDir: TalGUI_Direction); OVERLOAD;
    (* Initializes the control. *)
      PROCEDURE Initialize; OVERRIDE;
    (* Draws the control in the given bitmap.

      Note that it uses @code(DrawBevel) and @code(DrawBox) from current
      @link(TalGUI_Style). *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;
    END;



  (* Scroll-bar control.  Note that currently there are not difference between
    slider and scroll-bar. *)
   TalGUI_ScrollBar = CLASS (TalGUI_Slider)
   END;

IMPLEMENTATION

(*
 * TalGUI_Button
 ****************************************************************************)

  PROCEDURE TalGUI_Button.SetCaption (CONST aCaption: STRING);
  BEGIN
    fCaption := aCaption; SELF.RedrawMe := TRUE
  END;



(* Creates the button. *)
  CONSTRUCTOR TalGUI_Button.Create;
  BEGIN
    INHERITED Create;
    fCaption := ''
  END;



(* Creates the button.  Width and height are calculated from caption size. *)
  CONSTRUCTOR TalGUI_Button.Create (CONST aCaption: STRING; CONST aX, aY: INTEGER);
  BEGIN
    INHERITED Create;
    fCaption := aCaption;
    X := aX; Y := aY; Width := -1;
  END;



(* Creates the button. *)
  CONSTRUCTOR TalGUI_Button.Create (CONST aCaption: STRING; CONST aX, aY, aW, aH: INTEGER; CONST aAlign: TalGUI_Alignment);
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
      SELF.BackgroundColor, 2,
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

  PROCEDURE TalGUI_Slider.AdjustSlider;
  BEGIN
    IF (Width > 0) AND (Height > 0) THEN
    CASE Direction OF
      agdHorizontal: fSliderFactor := ABS (Max - Min) / Width;
      agdVertical: fSliderFactor := ABS (Max - Min) / Height;
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



(* Creates the slider. *)
  CONSTRUCTOR TalGUI_Slider.Create;
  BEGIN
    INHERITED Create
  END;



(* Creates the slider. *)
  CONSTRUCTOR TalGUI_Slider.Create (CONST aSize, aX, aY: INTEGER; aDir: TalGUI_Direction);
  BEGIN
    INHERITED Create;
    Direction := aDir;
    Max := aSize;
    X := aX; Y := aY; Width := -1
  END;



(* Creates the slider. *)
  CONSTRUCTOR TalGUI_Slider.Create (CONST aMin, aMax, aX, aY, aW, aH: INTEGER;
	aDir: TalGUI_Direction);
  BEGIN
    INHERITED Create;
    Direction := aDir;
    Min := aMin; Max := aMax;
    X := aX; Y := aY; Width := aW; Height := aH
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
	  Width := ABS (Max - Min); Height := Size;
	END;
      agdVertical:
	BEGIN
	  Height := ABS (Max - Min); Width := Size;
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
      Dialog.Style.DrawBox (
	Bmp, X, Offset, X + Width - 1, Offset + 2,
	SELF.BackgroundColor, 1,
	FALSE
      );
    { Slider. }
      Offset := X + TRUNC (Position * fSliderFactor);
      Dialog.Style.DrawBox (
	Bmp, Offset - 1, Y, Offset + 4, Y + Height,
	SELF.BackgroundColor, 1,
	TRUE
      );
    END;

    PROCEDURE DrawVertical;
    BEGIN
    { Trench. }
      Offset :=  X + (SELF.Width DIV 2) - 1;
      Dialog.Style.DrawBox (
	Bmp, Offset, y, Offset + 2, Y + Height - 1,
	SELF.BackgroundColor, 1,
	FALSE
      );
    { Slider. }
      Offset := Y + TRUNC (Position * fSliderFactor);
      Dialog.Style.DrawBox (
	Bmp, X, Offset - 1, X + Width, Offset + 4,
	SELF.BackgroundColor, 1,
	TRUE
      );
    END;

  BEGIN
    Dialog.Style.DrawBox (
      Bmp, X, Y, x + Width - 1, Y + Height - 1,
      SELF.BackgroundColor, 0, FALSE
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

END.

