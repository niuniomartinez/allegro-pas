PROGRAM exjoy;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/


   This program uses the Allegro library to detect and read the value of a
   joystick.  The output of the program is a small target sight on the screen
   which you can move.  At the same time the program will tell you what you are
   doing with the joystick (moving or firing).

   By Ñuño Martínez <niunio(at)users.sourceforge.net>, inspired by an
   example program for the Allegro library, by Grzegorz Adam Hankiewicz. *)


USES
  allegro, sysutils;



VAR
(* Joystick mode. *)
  AnalogMode: BOOLEAN;



(* To make text drawing easer. *)
  PROCEDURE DrawText (Bmp: AL_BITMAPptr; Message: STRING; x, y: INTEGER);
  BEGIN
    al_textout_ex (Bmp, al_font, Message, x, y, al_palette_color^[255], 0);
  END;

  PROCEDURE DrawTextCentre (Bmp: AL_BITMAPptr; Message: STRING; x, y: INTEGER);
  BEGIN
    al_textout_centre_ex (Bmp, al_font, Message, x, y, al_palette_color^[255], 0);
  END;



(* Detects and calibrates the joystick.
   Returns TRUE on success or FALSE on failure. *)
  FUNCTION CalibrateJoystick: BOOLEAN;
  VAR
    Message: STRING;
    Key: INTEGER;
  BEGIN
    CalibrateJoystick := FALSE;
    al_clear_bitmap (al_screen);
    DrawTextCentre (al_screen, 'Please center the joystick',
	AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2 - 36);
    DrawTextCentre (al_screen, 'and press a key.',
	AL_SCREEN_W DIV 2, AL_SCREEN_H DIV 2 - 20);
    IF (al_readkey AND $000000FF) = 27 THEN
      EXIT;
  (* The first thing is to initialise the joystick driver. *)
    IF NOT al_install_joystick (AL_JOY_TYPE_AUTODETECT) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Error initialising joystick' + #10 + al_error);
      EXIT;
    END;
  { Make sure that we really do have a joystick. }
    IF al_num_joysticks < 1 THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
      al_message ('Error: joystick not found.');
      EXIT;
    END;
  { Before using the joystick, we have to calibrate it.  This loop only
    calibrates joystick number 0, but you could do the same thing for other
    sticks if they are present (the al_num_joysticks variable will tell you
    how many there are). }
    WHILE (al_joy[0].flags AND AL_JOYFLAG_CALIBRATE) <> 0 DO
    BEGIN
      Message := al_calibrate_joystick_name (0);

      al_clear_bitmap (al_screen);
      DrawTextCentre (al_screen, Message, AL_SCREEN_W DIV 2, 64);
      DrawTextCentre (al_screen, 'and press a key.', AL_SCREEN_W DIV 2, 80);

      IF (al_readkey AND $000000FF) = 27 THEN
	EXIT;

      IF NOT al_calibrate_joystick (0) THEN
      BEGIN
	al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
	al_message('Error calibrating joystick!');
	EXIT;
      END;
    END;
  { If this joystick supports analogue input, ask the user whether to use
    digital or analogue mode.  If it is only a digital pad, we don't bother
    with this question. }
    IF (al_joy[0].stick[0].flags AND AL_JOYFLAG_ANALOGUE) <> 0 THEN
    BEGIN
      al_clear_bitmap (al_screen);
      DrawTextCentre (al_screen, 'Now press "D" to use a digital',
	AL_SCREEN_W DIV 2, 64);
      DrawTextCentre (al_screen, 'joystick or "A" for analogue mode.',
	AL_SCREEN_W DIV 2, 80);
      REPEAT
	Key := al_readkey AND $000000FF;

	IF Key IN [ORD ('d'), ORD ('D')] THEN
	  AnalogMode := FALSE
	ELSE IF Key IN [ORD ('a'), ORD ('A')] THEN
	  AnalogMode := TRUE;
      UNTIL Key IN [ORD ('d'), ORD ('D'), ORD ('a'), ORD ('A'), 27];
    END
    ELSE
      AnalogMode := FALSE;
  { Everything is Ok. }
    CalibrateJoystick := TRUE;
  END;



(* The example itself. *)
  PROCEDURE Example;
  VAR
    Buffer: AL_BITMAPptr;
  { The target sight. }
    X, Y, Ndx: INTEGER;
  BEGIN
  { Initialize example. }
    al_xor_mode (TRUE);
    al_clear_keybuf;
    Buffer := al_create_bitmap (320, 200);
    X := 160;
    Y := 100;
  { Main loop. }
    REPEAT
    { We HAVE to do this to read the joystick. }
      al_poll_joystick;
    { Information. }
      al_clear_bitmap (Buffer);
      IF AnalogMode THEN
	DrawTextCentre (Buffer, 'Analog mode selected', 160, 160)
      ELSE
	DrawTextCentre (Buffer, 'Digital mode selected', 160, 170);
      DrawTextCentre (Buffer, 'Move the joystick all around', 160, 180);
      DrawTextCentre (Buffer, 'Press any key to exit', 160, 190);
    { If we detect any buttons, we print a message on the screen. }
      FOR Ndx := 0 TO al_joy[0].num_buttons - 1 DO
	IF al_joy[0].button[Ndx].b <> 0 THEN
	  DrawTextCentre (Buffer, al_joy[0].button[Ndx].name + ' pressed',
			  160, Ndx*10);
      IF NOT AnalogMode THEN
      BEGIN
      { Now we have to check individually every possible movement and
        actualize the coordinates of the target sight. }
	IF al_joy[0].stick[0].axis[0].d1 <> 0 THEN
	BEGIN
	  IF X > 0 THEN
	    DEC (X);
	  DrawTextCentre (Buffer, 'Left', 120, 100);
	END;
	IF al_joy[0].stick[0].axis[0].d2 <> 0 THEN
	BEGIN
	  IF X < 319 THEN
	    INC (X);
	  DrawTextCentre (Buffer, 'Right', 200, 100);
	END;
	IF al_joy[0].stick[0].axis[1].d1 <> 0 THEN
	BEGIN
	  IF Y > 0 THEN
	    DEC (Y);
	  DrawTextCentre (Buffer, 'Up', 200, 100);
	END;
	IF al_joy[0].stick[0].axis[1].d2 <> 0 THEN
	BEGIN
	  IF Y < 199 THEN
	    INC (Y);
	  DrawTextCentre (Buffer, 'Down', 200, 100);
	END;
      END
      ELSE BEGIN
      { Yeah!  Remember the 'ifs' of the digital part?  This looks much
	better, only 2 lines. }
	INC (X, al_joy[0].stick[0].axis[0].pos DIV 40);
	INC (y, al_joy[0].stick[0].axis[1].pos DIV 40);
      { For informational purposes, show the input values on screen. }
	DrawText (Buffer, 'Axis 0: ' + IntToStr (al_joy[0].stick[0].axis[0].pos), 0,  0);
	DrawText (Buffer, 'Axis 1: ' + IntToStr (al_joy[0].stick[0].axis[1].pos), 0, 10);
      { By checking if the values were positive or negative, we can know in
	which the direction the user pulled the joy. }
	IF (al_joy[0].stick[0].axis[0].pos DIV 40) < 0 THEN
	  DrawTextCentre (Buffer, 'left', 120, 100);
	IF (al_joy[0].stick[0].axis[0].pos DIV 40) > 0 THEN
	  DrawTextCentre (Buffer, 'Right', 200, 100);
	IF (al_joy[0].stick[0].axis[1].pos DIV 40) < 0 THEN
	  DrawTextCentre (Buffer, 'Up', 200, 100);
	IF (al_joy[0].stick[0].axis[1].pos DIV 40) > 0 THEN
	  DrawTextCentre (Buffer, 'Down', 200, 100);
      { WARNING!  An analog joystick can move more than 1 pixel at a time
	and the checks we did with the digital part don't work any longer
	because the steps of the target sight could 'jump' over the limits.
	To avoid this, we just check if the target sight has gone out of the
	screen. If yes, we put it back at the border. }
	IF X > 319 THEN
	  X := 319;
	IF X < 0 THEN
	  X := 0;
	IF Y < 0 THEN
	  Y := 0;
	IF Y > 199 THEN
	  Y := 199;
      END;
    { This draws the target sight. }
      al_circle (Buffer, X, Y, 5, al_palette_color^[255]);
      al_putpixel (Buffer, X    , Y    , al_palette_color^[255]);
      al_putpixel (Buffer, X + 1, Y    , al_palette_color^[255]);
      al_putpixel (Buffer, X    , Y + 1, al_palette_color^[255]);
      al_putpixel (Buffer, X - 1, Y    , al_palette_color^[255]);
      al_putpixel (Buffer, X    , Y - 1, al_palette_color^[255]);
      al_putpixel (Buffer, X + 5, Y    , al_palette_color^[255]);
      al_putpixel (Buffer, X    , Y + 5, al_palette_color^[255]);
      al_putpixel (Buffer, X - 5, Y    , al_palette_color^[255]);
      al_putpixel (Buffer, X    , Y - 5, al_palette_color^[255]);
    { Double buffer. }
      al_vsync;
      al_blit (Buffer, al_screen, 0, 0,
	AL_SCREEN_W DIV 2 - 160, AL_SCREEN_H DIV 2 - 100, 320, 200);
    UNTIL al_keypressed;
    al_destroy_bitmap (Buffer);
  END;



BEGIN (* The program starts here. *)
{ You should always do this at the start of Allegro programs. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;
  al_install_timer;
{ Set a graphics mode. }
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message (al_error);
      EXIT;
    END;
  al_set_palette (al_default_palette);
{ Call calibration routine. }
  IF NOT CalibrateJoystick THEN
    EXIT;
{ Call example routine. }
  Example;
{ End of the program. }
END.
