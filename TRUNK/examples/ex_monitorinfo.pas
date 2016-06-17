PROGRAM ex_monitorinfo;

  USES
    Allegro5,
    Common,
    sysutils;

  VAR
   Info: ALLEGRO_MONITOR_INFO;
   NumAdapters, i, j: INTEGER;
   Mode: ALLEGRO_DISPLAY_MODE;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

   OpenLog;

   NumAdapters := al_get_num_video_adapters;

   LogWriteLn (Format ('%d adapters found...', [NumAdapters]));

   FOR i := 1 TO NumAdapters DO
   BEGIN
     al_get_monitor_info (i - 1, Info);
     LogWriteLn (Format (
       'Adapter %d: (%d, %d) - (%d, %d)',
       [i - 1, info.x1, info.y1, info.x2, info.y2]
     ));
     al_set_new_display_adapter (i - 1);
     LogWriteLn ('   Available fullscreen display modes:');
     FOR j := 1 TO al_get_num_display_modes DO
     BEGIN
       al_get_display_mode (j - 1, Mode);
       LogWriteLn (Format (
         '   Mode %3d: %4d x %4d, %d Hz',
         [j - 1, Mode.width, Mode.height, Mode.refresh_rate]
       ))
     END
   END;

   CloseLog (TRUE)
END.
