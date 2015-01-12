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
    (* Event executed when clicking the "Click me" button. *)
      PROCEDURE onClickMeBtnClick (Sender: TalGUI_Control);
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
    Dialog.Controls.Add (TalGUI_Label.Create (
      ' This is a label aligned left ',
      0, 16, AL_SCREEN_W, 8, agaLeft
    ));
    Dialog.Controls.Add (TalGUI_Label.Create (
      ' This is a label centered ',
      0, 24, AL_SCREEN_W, 8, agaCenter
    ));
    Dialog.Controls.Add (TalGUI_Label.Create (
      ' This is a label aligned right ',
      0, 32, AL_SCREEN_W, 8, agaRight
    ));

    Dialog.Controls.Add (TalGUI_Label.Create (
      'These are TalGUI_Box -->',
      0, 64, AL_SCREEN_W DIV 2, 8, agaRight
    ));
    Dialog.Controls.Add (TalGUI_Box.Create (
      AL_SCREEN_W DIV 2 + 8, 50, 100, 32
    ));
    Dialog.Controls.Add (TalGUI_Box.Create (
      AL_SCREEN_W DIV 2 + 115, 50, 100, 32, FALSE
    ));

  { Example buttons. }
    Dialog.Controls.Add (TalGUI_Label.Create (
      'These are buttons -->',
      0, 96, AL_SCREEN_W DIV 2, 8, agaRight
    ));
    Ndx := Dialog.Controls.Add (TalGUI_Button.Create (
      'Press me',
      AL_SCREEN_W DIV 2 + 8, 88, 144, 24
    ));
    TalGUI_Button (Dialog.Controls[Ndx]).onCLick := @SELF.onClickMeBtnClick;

    Ndx := Dialog.Controls.Add (TalGUI_Button.Create (
      'Disabled button',
      AL_SCREEN_W DIV 2 + 160, 88, 144, 24
    ));
    TalGUI_Button (Dialog.Controls[Ndx]).Enabled := FALSE;
  { Close button. }
    Ndx := Dialog.Controls.Add (TalGUI_Button.Create (
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
