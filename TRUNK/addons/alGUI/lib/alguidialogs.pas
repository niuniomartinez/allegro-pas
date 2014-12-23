UNIT alGUIdialogs;
(*<Defines some dialogs. *)
(*
  Copyright (c) 2014 Guillermo Martínez J.

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

INTERFACE

  USES
    allegro, alGUI, Classes;

  TYPE
  (* Draws a dialog frame. *)
    TalGUI_WindowFrame = CLASS (TalGUI_Yield)
    PRIVATE
      fCaption: STRING;

      PROCEDURE SetCaption (CONST aCaption: STRING); INLINE;
    PUBLIC
    (* Creates the control. *)
      CONSTRUCTOR Create
	(CONST aCaption: STRING; CONST aX, aY, aW, aH: INTEGER); OVERLOAD;
    (* Adjust frame size and position, so all controls in the owner dialog will
	be drawed inside the frame. @seealso(TalGUI_Control.Dialog) *)
      PROCEDURE AdjustSize;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;

    (* Window caption text. *)
      PROPERTY Caption: STRING READ fCaption WRITE SetCaption;
    END;



  (* Base class for default dialogs.

     It introduces some stuff as a frame to show the caption and helpers to add
     and manage controls. *)
    TalGUI_CustomDialogWindow = CLASS (TalGUI_Dialog)
    PRIVATE
      fNextY: INTEGER;
      Initialized: BOOLEAN;

      FUNCTION GetCaption: STRING;
      PROCEDURE SetCaption (CONST aCaption: STRING);
    PROTECTED
    (* Initializes the dialog.

      This method adjust the window size and then calls the @code(INHERITED)
      method. *)
      PROCEDURE Initialize (FocusCtrl: INTEGER); OVERRIDE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Adds a @code(Label).
       @param(aText Text of the label.)
       @return(The index to the label control.) *)
      FUNCTION AddLabel (CONST aText: STRING): INTEGER;
    (* Dialog caption. *)
      PROPERTY Caption: STRING READ GetCaption WRITE SetCaption;
    END;



(* Shows a dialog message.
   @param(aCaption The window caption.)
   @param(aMsg The message to show.  You can use line return @(@code(#10)@)
     here.)
   @param(aButtons List of strings, each one defines a button that can close
     the dialog window.)
   @return(The index of the pressed button.) *)
  FUNCTION MessageDlg (
    CONST aCaption, aMsg: STRING; Buttons: ARRAY OF STRING
  ): INTEGER;

IMPLEMENTATION

  USES
    alGUICommonCtrls,
    sysutils;

  CONST
  (* Window inner margin. *)
    DLG_MARGIN = 8;
    DLG_CAPTION_H = 16;

(* Shows a dialog message and returns index of the pressed button. *)
  FUNCTION MessageDlg (
    CONST aCaption, aMsg: STRING; Buttons: ARRAY OF STRING
  ): INTEGER;

  VAR
    Dialog: TalGUI_CustomDialogWindow;
    Lines: TStringList;

    Ndx, Tmp, DlgX, DlgY, DlgWidth, DlgHeight, TxtH, BtnLeft: INTEGER;
  BEGIN
    Lines := TStringList.Create;
    Dialog := TalGUI_CustomDialogWindow.Create;
    TRY
      Dialog.Caption := aCaption;
    { Get dialog lines. }
      Lines.Text := aMsg;
      FOR Ndx := 0 TO Lines.Count - 1 DO
	Dialog.AddLabel (Lines[Ndx]);

    { Get dialog sizes. }
{
      DlgWidth := 0; DlgHeight := 0;
      TxtH := al_text_height (Dialog.Style.TextFont);
      FOR Ndx := 0 TO Lines.Count - 1 DO
      BEGIN
	INC (DlgHeight, TxtH);
	Tmp := al_text_length (Dialog.Style.TextFont, Lines[Ndx]);
	IF DlgWidth < Tmp THEN DlgWidth := Tmp;
      END;
      Tmp := 0;
      FOR Ndx := 0 TO Length (Buttons) DO
	INC (Tmp, GetButtonWidth (Dialog.Style.TextFont, Buttons[Ndx]));
      IF DlgWidth < Tmp THEN
      BEGIN
	DlgWidth := Tmp;
	BtnLeft := DLG_MARGIN DIV 2
      END
      ELSE
	BtnLeft := (DlgWidth - Tmp) DIV 2;
      INC (DlgWidth, DLG_MARGIN);
      DlgHeight := DlgHeight + DLG_CAPTION_H +
	GetButtonHeight (Dialog.Style.TextFont) + (DLG_MARGIN * 2);
      DlgX := (Dialog.Bmp^.w - DlgWidth) DIV 2;
      DlgY := (Dialog.Bmp^.h - DlgHeight) DIV 2;
}
    { Creates the dialog. }
{
      Dialog.Controls.Add (TalGUI_WindowFrame.Create (
	aCaption,
	DlgX, DlgY,
	DlgWidth, DlgHeight
      ));

      FOR Ndx := 0 TO Lines.Count - 1 DO
	Dialog.Controls.Add (TalGUI_Label.Create (
	  Lines[Ndx],
	  DlgX + (DLG_MARGIN DIV 2),
	  DlgY + (TxtH * Ndx) + (DLG_MARGIN DIV 2) + DLG_CAPTION_H
	));

	BtnLeft := DlgX + BtnLeft;
      TxtH := DlgY + (TxtH * Lines.Count) + DLG_MARGIN + DLG_CAPTION_H;
      FOR Ndx := 0 TO Length (Buttons) - 1 DO
      BEGIN
	Dialog.Controls.Add (TalGUI_Button.Create (
	  Buttons[Ndx], BtnLeft, TxtH
	));
	INC (BtnLeft, GetButtonWidth (Dialog.Style.TextFont, Buttons[Ndx]) + DLG_MARGIN)
      END;
}
      Dialog.SetDefaultColors;
      Dialog.Run (0);
      RESULT := -1
    FINALLY
      FreeAndNil (Lines);
      FreeAndNil (Dialog)
    END
  END;



(*
 * TalGUI_WindowFrame
 ****************************************************************************)

  PROCEDURE TalGUI_WindowFrame.SetCaption (CONST aCaption: STRING);
  BEGIN
    fCaption := aCaption;
    SELF.Dialog.RedrawAll
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_WindowFrame.Create
    (CONST aCaption: STRING; CONST aX, aY, aW, aH: INTEGER);
  BEGIN
    INHERITED Create;
    fCaption := aCaption;
    SELF.X := aX; SELF.Y := aY;
    SELF.Width := aW; SELF.Height := aH
  END;



(* Adjust frame size and position. *)
  PROCEDURE TalGUI_WindowFrame.AdjustSize;
  VAR
    Ndx, Left, Top, Right, Bottom: INTEGER;
  BEGIN
  { Get limits. }
    Left := Dialog.Bmp^.w; Top := Dialog.Bmp^.h;
    Right := 0; Bottom := 0;
    FOR Ndx := Dialog.Controls.Count - 1 DOWNTO 1 DO
    BEGIN
      IF Dialog.Controls[Ndx].X < Left THEN Left := Dialog.Controls[Ndx].X;
      IF Dialog.Controls[Ndx].Y < Top  THEN Top := Dialog.Controls[Ndx].Y;
      IF (Dialog.Controls[Ndx].X + Dialog.Controls[Ndx].Width - 1) > Right THEN
	Right := Dialog.Controls[Ndx].X + Dialog.Controls[Ndx].Width - 1;
      IF (Dialog.Controls[Ndx].Y + Dialog.Controls[Ndx].Height - 1) > Bottom THEN
	Bottom := Dialog.Controls[Ndx].Y + Dialog.Controls[Ndx].Height - 1
    END;
  { Set frame size and position. }
    SELF.X      := Left - DLG_MARGIN;
    SELF.Y      := Top - DLG_MARGIN - DLG_CAPTION_H;
    SELF.Width  := Right - Left + (DLG_MARGIN * 2);
    SELF.Height := Bottom - Top + (DLG_MARGIN * 2) + DLG_CAPTION_H;
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_WindowFrame.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    Dialog.Style. DrawDialogFrame (
      Bmp, X, Y, X + Width - 1, Y + Height - 1,
      BackgroundColor, fCaption, TRUE)
  END;



(*
 * TalGUI_CustomDialogWindow
 ****************************************************************************)

  FUNCTION TalGUI_CustomDialogWindow.GetCaption: STRING;
  BEGIN
    RESULT := TalGUI_WindowFrame (SELF.Controls[0]).Caption
  END;



  PROCEDURE TalGUI_CustomDialogWindow.SetCaption (CONST aCaption: STRING);
  BEGIN
    TalGUI_WindowFrame (SELF.Controls[0]).Caption := aCaption
  END;



(* Initializes the dialog. *)
  PROCEDURE TalGUI_CustomDialogWindow.Initialize (FocusCtrl: INTEGER);
  VAR
    Left, Top: INTEGER;
  BEGIN
    IF NOT Initialized THEN
    BEGIN
    { Adjust frame size and position. }
      TalGUI_WindowFrame (Controls[0]).AdjustSize;
    { Now, centers the dialog. }
      Left := (SELF.Bmp^.w - Controls[0].Width)  DIV 2;
      Top  := (SELF.Bmp^.h - Controls[0].Height) DIV 2;
      SELF.Controls.MoveControls (Left - Controls[0].X, Top - Controls[0].Y);
    { Done. }
      Initialized := TRUE
    END;
  { Continues initialization. }
    INHERITED Initialize (FocusCtrl)
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_CustomDialogWindow.Create;
  BEGIN
    INHERITED Create;
    fNextY := DLG_MARGIN + DLG_CAPTION_H;
    Initialized := FALSE;
  { Window frame. }
    Controls.Add (TalGUI_WindowFrame.Create)
  END;



(* Adds a label. *)
  FUNCTION TalGUI_CustomDialogWindow.AddLabel (CONST aText: STRING): INTEGER;
  VAR
    TheLabel: TalGUI_Label;
  BEGIN
    TheLabel := TalGUI_Label.Create (
      aText,
      DLG_MARGIN, fNextY,
      al_text_length (SELF.Style.TextFont, aText),
      al_text_height (SELF.Style.TextFont)
    );
    INC (fNextY, TheLabel.Height);
    RESULT := Controls.Add (TheLabel);
  END;



(*
 * TMessageDialog
 ****************************************************************************)

(* Helper function that returns width of a button.

  Note that it uses same formula than TalGUI_Button to calculate margins. *)
{
  FUNCTION TMessageDialog.GetButtonWidth (CONST aCaption: STRING): INTEGER;
  BEGIN
    RESULT := al_text_length (SELF.Style.TextFont, aCaption)
	    + al_text_length (SELF.Style.TextFont, 'A')
  END;
 }


(* Helper function that returns height of a button.

  Note that it uses same formula than TalGUI_Button to calculate margins. *)
{
  FUNCTION TMessageDialog.GetButtonHeight: INTEGER;
  BEGIN
    RESULT := al_text_length (SELF.Style.TextFont, 'A') * 3
  END;
}



END.
