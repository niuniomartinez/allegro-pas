PROGRAM Test;
(* Tests the GUI. *)

  USES
    allegro, alGUI,
    sysutils;

  VAR
    Styles: ARRAY [0..0] OF TalGUI_Style;
    Ndx, ActiveStyle: INTEGER;
    Dialog: TalGUI_Dialog;
BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t init Allegro.');
    EXIT;
  END;

  al_install_timer;
  al_install_keyboard;
  al_install_mouse;

  al_set_color_depth (32);
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT
    END;
  al_show_mouse (al_screen);

  TRY
    Dialog := TalGUI_Dialog.Create;

    Dialog.Controls.Add (TalGUI_ClearScreen.Create); { Play fair with OS. }

    Styles[0] := Dialog.Style;
    ActiveStyle := 0;

    {
    al_clear_to_color (Bmp, Dialog.Style.TextColor);

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

    Dialog.Run (-1);
  EXCEPT
    ON Error: Exception DO
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message (Error.Message);
    END
  END;

  Dialog.Style := NIL; { Avoids destruction by dialog. }
  FreeAndNil (Dialog);
  FOR Ndx := LOW (Styles) TO HIGH (Styles) DO
    IF Styles[Ndx] <> NIL THEN Styles[Ndx].Free
END.
