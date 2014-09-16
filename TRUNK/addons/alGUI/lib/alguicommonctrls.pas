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
    (* Creaates the button.  Width and height are calculated from caption
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


IMPLEMENTATION

(*
 * TalGUI_Button
 ****************************************************************************)

  PROCEDURE TalGUI_Button.SetCaption (CONST aCaption: STRING);
  BEGIN
    fCaption := aCaption; SELF.RedrawMe := TRUE
  END;



(* Creaates the button. *)
  CONSTRUCTOR TalGUI_Button.Create;
  BEGIN
    INHERITED Create;
    fCaption := ''
  END;



(* Creaates the button.  Width and height are calculated from caption size. *)
  CONSTRUCTOR TalGUI_Button.Create (CONST aCaption: STRING; CONST aX, aY: INTEGER);
  BEGIN
    INHERITED Create;
    fCaption := aCaption;
    X := aX; Y := aY; Width := -1;
  END;



(* Creaates the button. *)
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

END.

