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
    allegro, alGUI;

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
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;

    (* Window caption text. *)
      PROPERTY Caption: STRING READ fCaption WRITE SetCaption;
    END;

    

(* Shows a dialog message and resturns index of the pressed button. *)
  FUNCTION MessageDlg (
    CONST aCaption, aMsg: STRING; Buttons: ARRAY OF STRING
  ): INTEGER;

IMPLEMENTATION

  USES
    alGUICommonCtrls,
    Classes, sysutils;

(* Shows a dialog message and resturns index of the pressed button. *)
  FUNCTION MessageDlg (
    CONST aCaption, aMsg: STRING; Buttons: ARRAY OF STRING
  ): INTEGER;
  CONST
  (* Window inner margin. *)
    DLG_MARGIN = 8;
    DLG_CAPTION_H = 16;
  (* Buttons size. *)
    BTN_H = 16;
  VAR
    Lines: TStringList;
    Ndx, Tmp, DlgX, DlgY, DlgWidth, DlgHeight, TxtH: INTEGER;
    Dialog: TalGUI_Dialog;
  BEGIN
    Lines := TStringList.Create;
    Dialog := TalGUI_Dialog.Create;
    TRY
    { Get dialog lines. }
      Lines.Text := aMsg;
    { Get dialog sizes. }
      DlgWidth := 0; DlgHeight := 0;
      TxtH := al_text_height (Dialog.Style.TextFont);
      FOR Ndx := 0 TO Lines.Count - 1 DO
      BEGIN
	INC (DlgHeight, TxtH);
	Tmp := al_text_length (Dialog.Style.TextFont, Lines[Ndx]);
	IF DlgWidth < Tmp THEN DlgWidth := Tmp;
      END;
      DlgX := 100; DlgY := 100;
    { Creates the dialog. }
      Dialog.Controls.Add (TalGUI_WindowFrame.Create (
	aCaption,
	DlgX, DlgY,
	DlgWidth + DLG_MARGIN, DlgHeight + DLG_CAPTION_H + BTN_H + DLG_MARGIN
      ));

      FOR Ndx := 0 TO Lines.Count - 1 DO
	Dialog.Controls.Add (TalGUI_Label.Create (
	  Lines[Ndx],
	  DlgX + (DLG_MARGIN DIV 2),
	  DlgY + (TxtH * Ndx) + (DLG_MARGIN DIV 2) + DLG_CAPTION_H
	));

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



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_WindowFrame.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    Dialog.Style. DrawDialogFrame (
      Bmp, X, Y, X + Width - 1, Y + Height - 1,
      BackgroundColor, fCaption, TRUE)
  END;

END.
