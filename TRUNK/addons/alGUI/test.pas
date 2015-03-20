PROGRAM Test;
(* Tests the GUI. *)

  USES
    Allegro, alGUI, alGUICommonCtrls, alGUIdialogs,
    sysutils;

  TYPE
  (* Defines an application class. *)
    TDemoGUI = CLASS (TObject)
    PRIVATE
      Dialog: TalGUI_Dialog;
      FirstRadio, LastRadio: INTEGER;
      CheckingRadio: BOOLEAN;

    (* Event executed when clicking the "Click me" button. *)
      PROCEDURE onClickMeBtnClick (Sender: TalGUI_Control);
    (* Event executed when slider changed. *)
      PROCEDURE onSliderChange (Sender: TalGUI_Control);
    (* Event used by checkboxes that simulates radio-butons. *)
      PROCEDURE onChangeRadio (Sender: TalGUI_Control);
    (* Event executed when clicking the close button. *)
      PROCEDURE onCloseBtnClick (Sender: TalGUI_Control);
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Initializes the application. *)
      PROCEDURE Initialize;
    (* Executes de application. *)
      PROCEDURE Run; INLINE;
    END;



(* Event executed when clicking the "Click me" button. *)
  PROCEDURE TDemoGUI.onClickMeBtnClick (Sender: TalGUI_Control);
  BEGIN
    MessageDlg (
      'Message box',
      'Congratulations!'#10'You clicked a button.',
      ['Close dialog']
    );
    Dialog.RedrawAll
  END;



(* Event executed when slider changed. *)
  PROCEDURE TDemoGUI.onSliderChange (Sender: TalGUI_Control);
  VAR
    Slider: TalGUI_CustomSlider;
  BEGIN
    Slider := TalGUI_CustomSlider (Sender);

    TalGUI_Label (Dialog.Controls[Slider.Tag]).Caption :=
      Format ('Position: %d of %d    ', [
	TalGUI_CustomSlider (Sender).Position,
	TalGUI_CustomSlider (Sender).Max
      ])
  END;



(* Event used by checkboxes that simulates radio-butons. *)
  PROCEDURE TDemoGUI.onChangeRadio (Sender: TalGUI_Control);
  VAR
    Ndx: INTEGER;
  BEGIN
    IF NOT CheckingRadio THEN
    BEGIN
      CheckingRadio := TRUE;
      TRY
	FOR Ndx := FirstRadio TO LastRadio DO
	BEGIN
	  TalGUI_CheckBox (Dialog.Controls[Ndx]).Checked := FALSE;
	  Dialog.Controls[Ndx].Enabled := TRUE
	END;
	Sender.Enabled := FALSE
      FINALLY
	CheckingRadio := FALSE
      END
    END
  END;



(* Event executed when clicking the close button. *)
  PROCEDURE TDemoGUI.onCloseBtnClick (Sender: TalGUI_Control);
  BEGIN
    Dialog.Close
  END;



(* Constructor. *)
  CONSTRUCTOR TDemoGUI.Create;
  BEGIN
    INHERITED Create;
    Dialog := NIL
  END;



(* Destructor. *)
  DESTRUCTOR TDemoGUI.Destroy;
  BEGIN
    Dialog.Free;
    INHERITED Destroy
  END;



(* Initializes the application. *)
  PROCEDURE TDemoGUI.Initialize;
  VAR
    Ndx: INTEGER;
  BEGIN
  { Initialize Allegro. }
    IF NOT al_init THEN
    BEGIN
      WriteLn ('Can''t init Allegro.');
      EXIT;
    END;
    al_install_timer;
    al_install_keyboard;
    al_install_mouse;
    al_set_color_depth (al_desktop_color_depth);
    IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
      IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
	RAISE Exception.Create ('Unable to set any graphic mode'#10+al_error+''#10);
    al_show_mouse (al_screen);
  { Dialog must be created after setting the graphics mode. }
    Dialog := TalGUI_Dialog.Create;
  { Play fair with OS. }
    Dialog.Controls.Add (TalGUI_ClearScreen.Create);
  { Some texts and static components. }
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      ' This is a label aligned left ',
      0, 16, AL_SCREEN_W, 8, agaLeft
    ));
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      ' This is a label centered ',
      0, 24, AL_SCREEN_W, 8, agaCenter
    ));
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      ' This is a label aligned right ',
      0, 32, AL_SCREEN_W, 8, agaRight
    ));

    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      'These are TalGUI_Box -->',
      0, 64, AL_SCREEN_W DIV 2, 8, agaRight
    ));
    Dialog.Controls.Add (TalGUI_Box.CreateBox (
      AL_SCREEN_W DIV 2 + 8, 50, 100, 32
    ));
    Dialog.Controls.Add (TalGUI_Box.CreateBox (
      AL_SCREEN_W DIV 2 + 115, 50, 100, 32, FALSE
    ));

  { Example buttons. }
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      'These are buttons -->',
      0, 96, AL_SCREEN_W DIV 2, 8, agaRight
    ));
    Ndx := Dialog.Controls.Add (TalGUI_Button.CreateButtonBox (
      'Press me',
      AL_SCREEN_W DIV 2 + 8, 88, 144, 24
    ));
    TalGUI_Button (Dialog.Controls[Ndx]).onCLick := @SELF.onClickMeBtnClick;

    Ndx := Dialog.Controls.Add (TalGUI_Button.CreateButtonBox (
      'Disabled button',
      AL_SCREEN_W DIV 2 + 160, 88, 144, 24
    ));
    TalGUI_Button (Dialog.Controls[Ndx]).Enabled := FALSE;

  { Example slider/scroll-bar. }
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      'This is a slider -->',
      0, 128, AL_SCREEN_W DIV 2, 8, agaRight
    ));
    Ndx := Dialog.Controls.Add (TalGUI_Slider.CreateSlider (
      100, AL_SCREEN_W DIV 2 + 8, 120, agdHorizontal
    ));
  { Store index of label in slider's Tag property. }
    TalGUI_Slider (Dialog.Controls[Ndx]).Tag :=
      Dialog.Controls.Add (TalGUI_Label.CreateLabel (
	'Position: 0 of 100',
	AL_SCREEN_W DIV 2 + 130, 128, 200, 24, agaLeft
      ));
    TalGUI_Slider (Dialog.Controls[Ndx]).OnChange := @SELF.onSliderChange;

  { Example list box. }
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      'This is a list box -->',
      0, 156, AL_SCREEN_W DIV 2, 8, agaRight
    ));
    Ndx := Dialog.Controls.Add (TalGUI_ListBox.CreateList (
      AL_SCREEN_W DIV 2 + 8, 156
    ));
    WITH TalGUI_ListBox (Dialog.Controls[Ndx]) DO
    BEGIN
      Items.Add ('One');
      Items.Add ('Two');
      Items.Add ('Three');
      Items.Add ('Four');
      Items.Add ('Five');
    END;

  { Example checkbox. }
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      'This is a checkbox -->',
      0, 212, AL_SCREEN_W DIV 2, 8, agaRight
    ));
    Ndx := Dialog.Controls.Add (TalGUI_CheckBox.Create);
    WITH TalGUI_CheckBox (Dialog.Controls[Ndx]) DO
    BEGIN
      X := AL_SCREEN_W DIV 2 + 8;
      Y := 210
    END;
  { Checkboxes don't have labels (at the moment). }
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      ' Check me!',
      AL_SCREEN_W DIV 2 + 22, 212, AL_SCREEN_W DIV 2, 8, agaLeft
    ));

  { Example of how to simulate radio-buttons using checkbox "onChange" event.

    There are some things done to make this work.
    Firs: it uses "CheckingRadio" property to avoid an infinite loop.  See the
	  "onChangeRadio" method and remove the "IF NOT CheckingRadio THEN" line
	  to see what happens.
    Second: the selected radio button is disabled, so it doesn't allow to
	    unselect the selected checkbox.  Method "onChangeRadio" deals with
	    this too.

    This method isn't perfect but works fairly well, doesn't it? }
    CheckingRadio := FALSE;
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      'You can select only one of these-->',
      0, 228, AL_SCREEN_W DIV 2, 8, agaRight
    ));
    FirstRadio := Dialog.Controls.Add (TalGUI_CheckBox.Create);
    WITH TalGUI_CheckBox (Dialog.Controls[FirstRadio]) DO
    BEGIN
      X := AL_SCREEN_W DIV 2 + 8;
      Y := 226;
      Checked := TRUE; Enabled := FALSE;
      onChange := @SELF.onChangeRadio
    END;
    Ndx := Dialog.Controls.Add (TalGUI_CheckBox.Create);
    WITH TalGUI_CheckBox (Dialog.Controls[Ndx]) DO
    BEGIN
      X := AL_SCREEN_W DIV 2 + 21;
      Y := 226;
      onChange := @SELF.onChangeRadio
    END;
    LastRadio := Dialog.Controls.Add (TalGUI_CheckBox.Create);
    WITH TalGUI_CheckBox (Dialog.Controls[LastRadio]) DO
    BEGIN
      X := AL_SCREEN_W DIV 2 + 34;
      Y := 226;
      onChange := @SELF.onChangeRadio
    END;

  { Close button. }
    Ndx := Dialog.Controls.Add (TalGUI_Button.CreateButton (
      'Close dialog',
      AL_SCREEN_W - 150, AL_SCREEN_H - 50
    ));
    TalGUI_Button (Dialog.Controls[Ndx]).onCLick := @SELF.onCloseBtnClick;
  { Set colors. }
    Dialog.SetDefaultColors;
    Dialog.Controls[0].Color := Dialog.Style.BackgroundColor;
  END;



(* Executes de application. *)
  PROCEDURE TDemoGUI.Run;
  BEGIN
    TRY
      Dialog.Run (-1)
    EXCEPT
      ON Error: Exception DO
      BEGIN
	al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
	al_message (Error.Message)
      END
    END
  END;

VAR
  Demo: TDemoGUI;
BEGIN
  Demo := TDemoGUI.Create;
  Demo.Initialize;
  Demo.Run;
  FreeAndNil (Demo);
END.
