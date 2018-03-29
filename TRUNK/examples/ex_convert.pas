PROGRAM ex_convert;
(* Image conversion example *)

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Common,
    Allegro5, al5image,
    sysutils;

  VAR
    Bitmap: ALLEGRO_BITMAPptr;
    t0, t1: DOUBLE;

BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

  OpenLog;

  IF ParamCount < 3 THEN
  BEGIN
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_convert <infile> <outfile>');
    LogWriteLn ('    Possible file types: BMP PCX PNG TGA');
    CloseLog (TRUE);
    HALT
  END;

  al_init_image_addon;

  al_set_new_bitmap_format (ALLEGRO_PIXEL_FORMAT_ARGB_8888);
  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);

  Bitmap := al_load_bitmap_flags (ParamStr (1), ALLEGRO_NO_PREMULTIPLIED_ALPHA);
  IF Bitmap = NIL THEN
  BEGIN
    LogWriteLn ('Error loading input file');
    CloseLog (TRUE);
    HALT
  END;

  t0 := al_get_time;
  IF NOT al_save_bitmap (ParamStr (2), Bitmap) THEN
  BEGIN
    LogWriteLn ('Error saving bitmap');
    CloseLog (TRUE);
    HALT
  END;
  t1 := al_get_time;
  LogWriteLn (Format ('Saving took %.4f seconds', [t1 - t0]));

  al_destroy_bitmap (Bitmap);

  CloseLog (TRUE)
END.
