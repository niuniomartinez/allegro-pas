PROGRAM excustom;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	A follow up of the exgui.pp example showing how to customise the
 *	default Allegro framework. In this case a dialog procedure
 *	animates a graphical clock without disrupting other GUI
 *	dialogs.  A more simple option shows how to dynamically change
 *	the font used by all GUI elements.
 *
 *	By Guillermo "Ñuño" Martínez
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See README file for license and copyright information.
 *)

  USES
    allegro, albase, alfile, alfixed, algui,
    sysutils;


  CONST
    STR_LENGTH = 32;

  VAR
  (* we need to load example.dat to access the big font *)
    DataFile: AL_DATAFILEptr;
  (* for the al_d_edit_proc() object. *)
    TheString: STRING[255] = 'Change Me!'+#0;
  (* since we change the font, we need to store a copy of the original one *)
    OriginalFont: AL_FONTptr;
  (* the current time, for the clock object *)
    TheHour, TheMinute, TheSecond: WORD;

{$include example.inc}


(* A custom dialog procedure for the 'change font' button. This uses a
 * simple form of inheritance: it calls al_d_button_proc() to do most of
 * the work, so it behaves exactly like any other button, but when the
 * button is clicked and al_d_button_proc() returns AL_D_CLOSE, it
 * intercepts the message and changes the font instead.
 *)
  FUNCTION ChangeFontProc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT; CDECL;
  BEGIN
  { call the parent object }
    ChangeFontProc := al_d_button_proc (msg, d, c);

  { trap the close return value and change the font }
    IF ChangeFontProc = AL_D_CLOSE THEN
    BEGIN
      IF al_font = OriginalFont THEN
	al_font := DataFile^[BIG_FONT].dat
      ELSE
	al_font := OriginalFont;
      ChangeFontProc := AL_D_REDRAW;
    END;
  END;



(* custom dialog procedure for the clock object *)
  FUNCTION ClockProc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT; CDECL;

  (* helper function to draw a hand on the clock *)
    PROCEDURE DrawHand (bmp: AL_BITMAPptr; Value, Range: INTEGER; Length: AL_FIXED; Clr: INTEGER);
    VAR
      Angle, x, y: AL_FIXED;
      w, h: INTEGER;
    BEGIN
      Angle := ((al_itofix (Value) * 256) DIV Range) - al_itofix (64);
      x := al_fixmul (al_fixcos (Angle), Length);
      y := al_fixmul (al_fixsin (Angle), Length);
      w := bmp^.w DIV 2;
      h := bmp^.h DIV 2;

      al_line (bmp, w, h, w + al_fixtoi(x*w), h + al_fixtoi(y*h), Clr);
    END;

  VAR
    CurrentTime: TDateTime;
    Hour, Minute, Second, Millisecond: WORD;
    Temp: AL_BITMAPptr;
    Angle, Cnt, x, y: AL_FIXED;
  BEGIN
  { process the message }
    CASE Msg OF
    { initialise when we get a start message }
    AL_MSG_START:
      BEGIN
      { store the current time }
	CurrentTime := Time;
	DecodeTime (CurrentTime, TheHour, TheMinute, TheSecond, Millisecond);

      { draw the clock background onto a memory bitmap }
	Temp := al_create_bitmap (d^.w, d^.h);
	al_clear_to_color (Temp, d^.bg);

      { draw borders and a nobble in the middle }
	al_circle (Temp, Temp^.w DIV 2, Temp^.h DIV 2, Temp^.w DIV 2-1, d^.fg);
	al_circlefill (Temp, Temp^.w DIV 2, Temp^.h DIV 2, 2, d^.d1);

      { draw ticks around the edge }
	FOR Cnt := 1 TO 12 DO
	BEGIN
	  Angle := al_ftofix (Cnt * (256 / 12));
	  x := al_fixcos (Angle);
	  y := al_fixsin (Angle);
	  al_line (Temp, Temp^.w DIV 2 + al_fixtoi (x * Temp^.w * 15 DIV 32), 
		         Temp^.h DIV 2 + al_fixtoi (y * Temp^.w * 15 DIV 32), 
		         Temp^.w DIV 2 + al_fixtoi (x * Temp^.w DIV 2), 
		         Temp^.h DIV 2 + al_fixtoi (y * Temp^.w DIV 2), d^.fg);
	END;

      { store the clock background bitmap in d^.dp }
	d^.dp := Temp;
      END;

    { shutdown when we get an end message }
    AL_MSG_END:
    { destroy the clock background bitmap }
      al_destroy_bitmap (d^.dp);

    { update the clock in response to idle messages }
    AL_MSG_IDLE:
      BEGIN
      { read the current time }
	CurrentTime := Time;
	DecodeTime (CurrentTime, Hour, Minute, Second, Millisecond);

      { check if it has changed }
	IF (TheHour <> Hour) OR (TheMinute <> Minute) OR (TheSecond <> Second) THEN
	BEGIN
	  TheHour := Hour;
	  TheMinute := Minute;
	  TheSecond := Second;

	{ Redraw ourselves if the time has changed.  Note that the dialog
	  manager automatically turns off the mouse pointer whenever a
	  MSG_DRAW message is sent to an individual object or an entire
	  dialog, so we don't have to do it explicitly. Also note the use
	  of the al_object_message function rather than a simple recursive
	  call to ClockProc.  This vectors the call through the function
	  pointer in the dialog object, which allows other object
	  procedures to hook it, for example a different type of clock
	  could process the draw messages itself but pass idle messages
	  on to this procedure.
	}
	  al_object_message (d, AL_MSG_DRAW, 0);
	END;
      END;

    { draw the clock in response to draw messages }
    AL_MSG_DRAW:
      BEGIN
      { draw onto a temporary memory bitmap to prevent flicker }
	Temp := al_create_bitmap (d^.w, d^.h);

      { copy the clock background onto the temporary bitmap }
	al_blit (d^.dp, Temp, 0, 0, 0, 0, d^.w, d^.h);

      { draw the hands }
	DrawHand (Temp, TheMinute, 60, al_itofix (5) DIV  6, d^.fg);
	DrawHand (Temp, TheHour,   12, al_itofix (1) DIV  2, d^.fg);
	DrawHand (Temp, TheSecond, 60, al_itofix (9) DIV 10, d^.d1);
	al_circlefill (Temp, Temp^.w DIV 2, Temp^.h DIV 2, 2, d^.d1);

      { copy the temporary bitmap onto the screen }
	al_blit (Temp, al_screen, 0, 0, d^.x, d^.y, d^.w, d^.h);
	al_destroy_bitmap (Temp);
      END;
    END;
  { always return OK status, since the clock doesn't ever need to close
    the dialog or get the input focus. }
    ClockProc := AL_D_O_K;
  END;



VAR
  TheDialog: ARRAY [0..6] OF AL_DIALOG;
  Item: INTEGER;
  buf: STRING;
BEGIN
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_mouse;
  al_install_timer;

  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message ('Unable to set any graphic mode'+chr(13)+al_error);
      EXIT;
    END;

  al_set_palette (al_desktop_palette);
  Item := al_makecol (255, 0, 0);

{                                   (dialog proc)     (x)  (y)  (w) (h) (fg) (bg) (key)   (flags)   (d1) (d2)  (dp)         (dp2) (dp3) }
  al_set_dialog_item (TheDialog, 0, @al_d_clear_proc,   0,   0,   0,  0, 255, 0,     0,      0,        0,   0,  NIL,          NIL,  NIL);
  al_set_dialog_item (TheDialog, 1, @al_d_edit_proc,   12,  82, 256, 48, 255, 0,     0,      0, STR_LENGTH, 0, @TheString[1], NIL,  NIL);
  al_set_dialog_item (TheDialog, 2, @al_d_check_proc,  12,  12, 161, 49, 255, 0, ORD ('t'),  0,        0,   0, AL_STRptr('&Toggle Me'), NIL, NIL);
  al_set_dialog_item (TheDialog, 3, @ClockProc,       242,  12,  64, 64, 255, 0,     0,      0,     Item,   0,  NIL,          NIL,  NIL);
  al_set_dialog_item (TheDialog, 4, @ChangeFontProc,   12, 142, 141, 49, 255, 0, ORD ('f'), AL_D_EXIT, 0,   0, AL_STRptr('Change &Font'), NIL, NIL);
  al_set_dialog_item (TheDialog, 5, @al_d_button_proc,162, 142, 141, 49, 255, 0, ORD ('x'), AL_D_EXIT, 0,   0, AL_STRptr('E&xit'), NIL, NIL);

{ We set up colors to match screen color depth (in case it changed) }
  FOR Item := Low (TheDialog) TO High (TheDialog) DO
  BEGIN
    TheDialog[Item].fg := al_makecol (0, 0, 0);
    TheDialog[Item].bg := al_makecol (255, 255, 255);
  END;

{ load the datafile }
{ TODO: Find a better way to get the path. }
  buf := ExtractFilePath (ParamStr (0)) + 'example.dat';
  DataFile := al_load_datafile (buf);
  IF DataFile = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error loading example.dat');
    EXIT;
  END;

{ store a copy of the default font }
  OriginalFont := al_font;

  al_do_dialog (TheDialog, -1);

  al_unload_datafile (DataFile);

END.
