PROGRAM Test;
(* Tests the GUI. *)

  USES
    allegro, alGUI, alGUICommonCtrls,
    sysutils;

  TYPE
  (* Defines an application class. *)
    TDemoGUI = CLASS (TObject)
    PRIVATE
      Dialog: TalGUI_Dialog;
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
    Ndx := Dialog.Controls.Add (TalGUI_ClearScreen.Create);
    TalGUI_ClearScreen (Dialog.Controls[Ndx]).Color :=
      Dialog.Style.BackgroundColor;

    Ndx := Dialog.Controls.Add (TalGUI_Button.Create);
    WITH TalGUI_Button (Dialog.Controls[Ndx]) DO
    BEGIN
      X := 25; Y := 25;
      Width := 100; Height := 16;
      Caption := 'A button';
    END;
    Ndx := Dialog.Controls.Add (TalGUI_Button.Create);
    WITH TalGUI_Button (Dialog.Controls[Ndx]) DO
    BEGIN
      X := 25; Y := 125;
      Width := 100; Height := 16;
      Caption := 'Another Btn';
    END;
 { Close button. }
    Ndx := Dialog.Controls.Add (TalGUI_Button.Create);
    WITH TalGUI_Button (Dialog.Controls[Ndx]) DO
    BEGIN
      X := 25; Y := 200;
      Width := 100; Height := 16;
      Caption := 'Close dialog';
    { Set event. }
      onCLick := @SELF.onCloseBtnClick
    END;
  END;



(* Executes de application. *)
  PROCEDURE TDemoGUI.Run;
  BEGIN
    TRY
      Dialog.Run (1)
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


    {
    Dialog.Style.DrawDialogFrame (
      Dialog.Bmp, 5, 5, 315, 195, Dialog.Style.BackgroundColor, 'GUI example', TRUE
    );
    Dialog.Style.DrawBevel (Dialog.Bmp, 10, 26, 305, 185, FALSE);

    Dialog.Style.DrawText (
      Dialog.Bmp, 'This is normal text.', 20, 32, Dialog.Style.TextColor, FALSE
    );
    Dialog.Style.DrawDisabledText (
      Dialog.Bmp, 'This is disabled text.', 20, 40, FALSE
    );

    Dialog.Style.DrawBox (
      Dialog.Bmp, 20, 50, 50, 100, -1, 2, TRUE
    );

    Dialog.Style.DrawBox (
      Dialog.Bmp, 60, 50, 90, 100, -1, 2, FALSE
    );

    Dialog.Style.DrawBevel (Dialog.Bmp, 100, 50, 130, 100, TRUE);
    }
END.
