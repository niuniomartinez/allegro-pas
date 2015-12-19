PROGRAM Test;
(* Tests the IFF PICT unit. *)

  USES
    Allegro, alIffPict,
    sysutils;

(* Initializes the application. *)
  FUNCTION Initialize: BOOLEAN;
  BEGIN
  { Initialize Allegro. }
    IF NOT al_init THEN
    BEGIN
      WriteLn ('Can''t init Allegro.');
      EXIT (FALSE);
    END;
    al_install_timer;
    al_install_keyboard;

    RESULT := TRUE
  END;



  FUNCTION InitGfx: BOOLEAN;
  BEGIN
    al_set_color_depth (al_desktop_color_depth);
    IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
      IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
      BEGIN
	al_message ('Unable to set any graphic mode'#10+al_error+''#10);
	EXIT (FALSE);
      END;

    RESULT := TRUE
  END;



VAR
  Bmp: AL_BITMAPptr;
  Pal: AL_PALETTE;
BEGIN
  IF Initialize THEN
  BEGIN
    CASE ParamCount OF
    1:
      IF InitGfx THEN
      BEGIN
	Bmp := al_load_pict (ParamStr (1), @Pal);
	IF Bmp <> NIL THEN
	BEGIN
	  al_set_palette (Pal);
	  al_blit (Bmp, al_screen, 0, 0, 0, 0, Bmp^.w, Bmp^.h);
	  al_destroy_bitmap (Bmp);
	  al_readkey
	END
	ELSE BEGIN
	  al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
	  al_message ('Unable to load PICT file.')
	END
      END;
    3:
      BEGIN
	IF ParamStr (1) = '-c' THEN
	BEGIN
	  Bmp := al_load_bitmap (ParamStr (2), @Pal);
	  IF Bmp <> NIL THEN
	  BEGIN
	    IF NOT al_save_pict (ParamStr (3), Bmp, @Pal) THEN
	      al_message ('Unable to save the PICT file.');
	    al_destroy_bitmap (Bmp);
	    al_message ('PICT file created.')
	  END
	  ELSE
	    al_message ('Unable to load bitmap.');
	  EXIT
	END
	ELSE
	  al_message ('Unknown parameter.');
      END;
    ELSE
      al_message ('Use "test -c bitmap file" to convert from BMP/PCX/TGA to PICT.'#10'Use "test file" to load and see a PICT.');
    END
  END
END.
