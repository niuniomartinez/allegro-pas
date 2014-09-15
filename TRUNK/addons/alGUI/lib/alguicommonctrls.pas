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
    PUBLIC
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;

    (* Button label. *)
      PROPERTY Caption: STRING READ fCaption WRITE fCaption;
    END;


IMPLEMENTATION

(*
 * TalGUI_Button
 ****************************************************************************)

(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_Button.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    Dialog.Style.DrawBox (
      Bmp, X, Y, X + Width, Y + Height,
      SELF.Dialog.Style.BackgroundColor, 2,
      NOT fPressed
    );
    IF SELF.HasFocus THEN
      Dialog.Style.DrawFocusRect (
	Bmp,
	X + 3, Y + 3,
	X + Width - 4, Y + Height - 4
      );
    IF SELF.Enabled THEN
      Dialog.Style.DrawText (
	Bmp, fCaption,
	X + (Width DIV 2),
	Y + (Height DIV 2) - (al_text_height (al_font) DIV 2),
	Dialog.Style.TextColor,
	TRUE
      )
    ELSE
      Dialog.Style.DrawDisabledText (
	Bmp, fCaption,
	X + (Width DIV 2),
	Y + (Height DIV 2) - (al_text_height (al_font) DIV 2),
	TRUE
      )
  END;

END.

