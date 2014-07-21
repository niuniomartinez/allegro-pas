PROGRAM Test;
(* Tests the GUI. *)

  USES
    allegro;

BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t init Allegro.');
    EXIT;
  END;

  al_install_keyboard;
  al_install_timer;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Unable to set any graphic mode'#10+al_error+''#10);
      EXIT;
    END;

  al_set_palette (al_desktop_palette);
END.
