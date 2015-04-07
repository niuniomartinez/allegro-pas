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
      Format ('Position: %d of %d    ', [Slider.Position, Slider.Max])
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
  CONST
  { Some default measures.  It assumes standard text font of 8x8. }
    WIDTH = 150; HEIGHT = 20; SPACE = HEIGHT + (HEIGHT DIV 2);
    LEFT = 1; RIGHT = 200; RIGHT_2 = RIGHT + WIDTH + 8;
  VAR
    LastY: INTEGER;

  { Helper function to add controls in rows with a label. }
    FUNCTION AddControl (CONST aName: STRING; aControl: TalGUI_Control;
      aControl2: TalGUI_Control=NIL)
      : INTEGER;
    BEGIN
      Dialog.Controls.Add (TalGUI_Label.CreateLabel (aName, LEFT, LastY));
      RESULT := Dialog.Controls.Add (aControl);
      IF aControl.X < 1 THEN
      BEGIN
	aControl.X := RIGHT; aControl.Y := LastY;
	aControl.Width := WIDTH; aControl.Height := HEIGHT
      END;
      IF aControl2 <> NIL THEN
      BEGIN
        Dialog.Controls.Add (aControl2);
	aControl2.X := RIGHT_2; aControl2.Y := LastY;
	aControl2.Width := WIDTH; aControl2.Height := HEIGHT
      END;
      INC (LastY, SPACE)
    END;

  VAR
    Ndx,
  { Some control index. }
    SliderLabel: INTEGER;
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
      'TalGUI_Label agaLeft',
      LEFT, 16, AL_SCREEN_W, 8, agaLeft
    ));
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      'TalGUI_Label agaCenter',
      LEFT, 16, AL_SCREEN_W, 8, agaCenter
    ));
    Dialog.Controls.Add (TalGUI_Label.CreateLabel (
      'TalGUI_Label agaRight',
      LEFT, 16, AL_SCREEN_W, 8, agaRight
    ));
    LastY := 32;

    Ndx := AddControl ('TalGUI_Box->', TalGUI_Box.Create, TalGUI_Box.Create);
    TalGUI_Box (Dialog.Controls[Ndx]).Raised := FALSE;
  { Example buttons. }
    Ndx := AddControl (
      'TalGUI_Button->',
      TalGUI_Button.CreateButton ('Press me', -1, -1),
      TalGUI_Button.CreateButton ('Disabled button', -1, -1)
    );
    TalGUI_Button (Dialog.Controls[Ndx]).onCLick := @SELF.onClickMeBtnClick;
    Dialog.Controls[Ndx + 1].Enabled := FALSE;

  { Example slider/scroll-bar. }
    Ndx := AddControl ('TalGUI_Slider->', TalGUI_Slider.Create);
  { Store index of label on slider's Tag property. }
    SliderLabel := Dialog.Controls.Add (TalGUI_Label.CreateLabel (
	'Position: 0 of 100',
	RIGHT_2, LastY - SPACE, WIDTH * 2, HEIGHT
    ));
    Dialog.Controls[Ndx].Tag := SliderLabel;
    TalGUI_Slider (Dialog.Controls[Ndx]).OnChange := @SELF.onSliderChange;

  { Example list box. }
    Ndx := AddControl ('TalGUI_ListBox->', TalGUI_ListBox.Create);
    WITH TalGUI_ListBox (Dialog.Controls[Ndx]) DO
    BEGIN
      Items.Add ('One');
      Items.Add ('Two');
      Items.Add ('Three');
      Items.Add ('Four');
      Items.Add ('Five');
      Height := 48
    END;

    LastY :=
      Dialog.Controls[Ndx].Y + Dialog.Controls[Ndx].Height + (HEIGHT DIV 2);
  { Example checkbox. }
    Ndx := AddControl (
      'TalGUI_CheckBox->',
      TalGUI_CheckBox.CreateCheckbox ('Check me!'),
      TalGUI_CheckBox.CreateCheckbox ('Disabled checkbox')
    );
    Dialog.Controls[Ndx + 1].Enabled := FALSE;

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
      'Radio button simulation->', LEFT, LastY
    ));
    FirstRadio := Dialog.Controls.Add (TalGUI_CheckBox.Create);
    WITH TalGUI_CheckBox (Dialog.Controls[FirstRadio]) DO
    BEGIN
      X := RIGHT;
      Y := LastY;
      Checked := TRUE; Enabled := FALSE;
      onChange := @SELF.onChangeRadio
    END;
    Ndx := Dialog.Controls.Add (TalGUI_CheckBox.Create);
    WITH TalGUI_CheckBox (Dialog.Controls[Ndx]) DO
    BEGIN
      X := RIGHT + 21;
      Y := LastY;
      onChange := @SELF.onChangeRadio
    END;
    LastRadio := Dialog.Controls.Add (TalGUI_CheckBox.Create);
    WITH TalGUI_CheckBox (Dialog.Controls[LastRadio]) DO
    BEGIN
      X := RIGHT + 42;
      Y := LastY;
      onChange := @SELF.onChangeRadio
    END;

  { Close button. }
    Ndx := Dialog.Controls.Add (TalGUI_Button.CreateButton (
      'Close dialog', LEFT, AL_SCREEN_H - 50
    ));
    TalGUI_Button (Dialog.Controls[Ndx]).onCLick := @SELF.onCloseBtnClick;
  { Set colors. }
    Dialog.Controls.SetDefaultColors;
    Dialog.Controls[0].Color := Dialog.Style.BackgroundColor;
  { By default, labels doesn't have background. }
    Dialog.Controls[SliderLabel].BackgroundColor := Dialog.Style.BackgroundColor
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
