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

