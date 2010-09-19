PROGRAM mapedit;
(* Project: The Allegro.pas demo game.
 * Description: This is the map editor.  Use it to create a board or to modify
 *		an existing one.
 * Author: Ken Silverman <http://www.advsys.net/ken/> - concept.
 *	   Ñuño Martínez <niunio at users.sourceforge.net>
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  allegro,
  algui,    { To use the Allegro's GUI. }
  error,    { To show nice error messages. }
  gamedata, { Management of the game data: graphics, sound, etc. }
  sysutils, { For string manipulation. }
  tilemap;  { Tilemap management. }

CONST
{ Coordinates and size of the first button.  This way we can change it easy. }
  BTN_POS_X = 10;
  BTN_POS_Y = 100;
  BTN_SIZE  = 32;
{ Window captions. }
  CAPTION = 'Allegro.pas Demo Game Map Editor';
  CAPTION_MODIFIED = CAPTION + ' [Modified]';

VAR
  BoardNumber: INTEGER;
  BoardModified: BOOLEAN; { TRUE if modified, FALSE if not. }
{ Colors }
  CWhite, CBlack, CGray: DWORD;
{ Buttons. }
  BtnBmp: ARRAY [0..6] OF AL_BITMAPptr;
{ Selected tile button. }
  TileButton: BYTE;
{ Key pressed. }
  Key: LONGINT;



(* Helper function to make simple questions. *)
  FUNCTION AskYesNo (Question: STRING): BOOLEAN;
  BEGIN
    AskYesNo :=
      al_alert (Question, '', '', '&Yes', '&No', Ord ('y'), Ord ('n')) = 1;
  END;



(* Helper procedure to draw boxes. *)
  PROCEDURE DrawBox (x1, y1, x2, y2: INTEGER);
  BEGIN
    al_rectfill (al_screen, x1, y1, x2 - 1, y2 - 1, CWhite);
    al_rect (al_screen, x1, y1, x2 - 1, y2 - 1, CBlack);
    al_vline (al_screen, x2, y1 + 1, y2, CBlack);
    al_hline (al_screen, x1 + 1, y2, x2, CBlack);
  END;



(* Helper procedure to ask the file names. *)
  PROCEDURE AskBoardNumber;
  VAR
    W2, H2: INTEGER;
  BEGIN
    W2 := AL_SCREEN_W DIV 2;
    H2 := AL_SCREEN_H DIV 2;
    DrawBox (W2 - 132, H2 - 16, W2 + 132, H2 + 25);
    al_gui_textout_ex (al_screen, 'Board number (1, 2, 3, etc.)?',
		   W2, H2, CBlack, CWhite, TRUE);
  { Wait until user press a valid key. }
    REPEAT
      Key := al_readkey AND $000000FF; { Get the ASCII code. }
      BoardNumber := Key - ORD ('0');
    UNTIL (0 < BoardNumber) AND (BoardNumber < 10);
  END;



(* Initializes the editor. *)
  FUNCTION InitProgram: BOOLEAN;
  VAR
    Palette: AL_PALETTEptr;
    Bmp, RefBmp: AL_BITMAPptr;
    Cnt: INTEGER;
  { Dictionary to know which bitmap draw in each button. }
    BtnGlyph: ARRAY [0..6] OF INTEGER = (
      -1,		{ Delete. }
      BMP_START,	{ Player start position. }
      BMP_END,		{ End of the map. }
      BMP_COIN,		{ Coins. }
      BMP_BLK1,		{ Blocks. }
      BMP_BLK2,
      BMP_BLK3
    );
  BEGIN
    InitProgram := FALSE;
  { Initialize Allegro. }
    IF NOT al_init THEN
    BEGIN
      WriteLn ('Can''t initialize Allegro!');
      EXIT;
    END;
    al_install_keyboard;
    al_install_timer;
  { Set the graphic mode.  First, tryes a windowed mode.  If no one is
    avaiable, tryes again with an auto-detected mode then a safe mode.
    If no graphic mode is available, shows the error. }
    al_set_color_depth (8);
    IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
      IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
	IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
	BEGIN
	  al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0); { Be sure it's closed. }
	{ Show an error message.
	  Can't use 'ErrorMessage' because the graphic mode isn't up. }
	  al_message (al_error);
	{ Shutdown Allegro. }
	  al_exit;
	  EXIT;
	END;
    al_set_window_title (CAPTION);
  { Don't let Allegro twist colors. }
    al_set_color_conversion (AL_COLORCONV_NONE);
  { Load the game data. }
    IF NOT LoadData THEN
    BEGIN
      ErrorMessage ('Can''t load the game data.');
    { Shutdown Allegro. }
      al_exit;
      EXIT;
    END;
  { Select the palette which was loaded from the datafile. }
    Palette := Data^[GAME_PAL].dat;
    al_set_palette (Palette^);
  { Calculate common colors. }
    CWhite := al_makecol (255, 255, 255);
    CBlack := al_makecol (0, 0, 0);
    CGray  := al_makecol (127, 127, 127);
    al_clear_to_color (al_screen, CGray);
    al_gui_fg_color := CBlack;
    al_gui_bg_color := CWhite;
    al_gui_mg_color := CGray;
  { Mouse.  Done here because it needs the correct palette color. }
    IF al_install_mouse < 0 THEN
    BEGIN
      ErrorMessage ('No mouse detected, but you need one!');
    { Release data and shutdown Allegro. }
      ReleaseData;
      al_exit;
      EXIT;
    END;
  { Create buttons.  We do it this way to make them bigger. }
    Bmp := al_create_bitmap (16, 16);
    FOR Cnt := 0 TO 6 DO
    BEGIN
      BtnBmp[Cnt] := al_create_bitmap (BTN_SIZE, BTN_SIZE);
      IF Cnt = 0 THEN
      BEGIN
      { The 'delete' button. }
	al_rectfill (BtnBmp[Cnt], 0, 0, BTN_SIZE, BTN_SIZE, al_makecol (0, 255, 255));
	al_textout_centre_ex (BtnBmp[Cnt], al_font, 'DEL',
				BTN_SIZE DIV 2, BTN_SIZE DIV 2 - 4,
				CWhite, -1);
      END
      ELSE
      BEGIN
      { These are bitmaps, so use stretch blit. }
	RefBmp := Data^[BtnGlyph[Cnt]].dat;
	al_stretch_blit (RefBmp, BtnBmp[Cnt],
			0, 0, RefBmp^.w, RefBmp^.h,
			0, 0, BTN_SIZE, BTN_SIZE);
      END;
    END;
    al_destroy_bitmap (Bmp); { Don't forget to destroy temporary bitmaps. }
  { If we're here, we're done. }
    InitProgram := TRUE;
  END;



(* Updates the buttons on the screen. *)
  PROCEDURE DrawButtons;

  (* Helper function to calculate the X coordinate. *)
    FUNCTION XPos (Btn: INTEGER): INTEGER;
    BEGIN
      XPos := BTN_POS_X + (Btn * BTN_SIZE);
    END;

  VAR
    Cnt, X: INTEGER;
  BEGIN
    al_acquire_screen;
    FOR Cnt := 0 TO 6 DO
      al_blit (BtnBmp[Cnt], al_screen, 0, 0,
		XPos (Cnt), BTN_POS_Y,
		BTN_SIZE, BTN_SIZE);
  { Draw a rectangle to show the selected button. }
    X := XPos (TileButton);
    al_rect (al_screen,
	     X, BTN_POS_Y,
	     X + BTN_SIZE - 1,
	     BTN_POS_Y + BTN_SIZE - 1,
	     CWhite);
    FOR Cnt := 0 TO ((BTN_SIZE DIV 2) - 1) DO
    BEGIN
      al_putpixel (al_screen, X + (Cnt * 2), BTN_POS_Y, CBlack);
      al_putpixel (al_screen, X + (Cnt * 2), BTN_POS_Y + BTN_SIZE - 1, CBlack);
      al_putpixel (al_screen, X, BTN_POS_Y + (Cnt * 2), CBlack);
      al_putpixel (al_screen, X + BTN_SIZE - 1, BTN_POS_Y + (Cnt * 2), CBlack);
    END;
    al_release_screen;
  END;



(* Redraws the screen.  It clears the screen creating flicks.  Note that
   sometimes the editor calls DrawBoardMiniature or DrawButtons that don't
   clears the screen avoiding flickering. *)
  PROCEDURE RedrawScreen;
  VAR
    Cnt: INTEGER;
  BEGIN
    al_acquire_screen;
    al_clear_to_color (al_screen, CGray);
  { The map. }
    DrawBoardMiniature (al_screen);
  { The buttons. }
    DrawBox (BTN_POS_X - 4, BTN_POS_Y - 4,
      BTN_POS_X + 4 + (BTN_SIZE * 7), BTN_POS_Y + 12 + BTN_SIZE);
    DrawButtons;
  { A bit of help. }
    Cnt := BTN_POS_X + 8 + (BTN_SIZE * 7);
    DrawBox (Cnt, BTN_POS_Y, Cnt + 344, BTN_POS_Y + 32);
    al_gui_textout_ex (al_screen, 'Functions:', Cnt + 8, BTN_POS_Y + 8, CBlack, CWhite, FALSE);
    al_gui_textout_ex (al_screen, 'F1 New|F2 Save|F3 Load|F4 Preview|F5 Quit',
		   Cnt + 8, BTN_POS_Y + 20, CBlack, CWhite, FALSE);
    FOR Cnt := 0 TO 6 DO
      al_textout_centre_ex (al_screen, al_font, IntToStr (Cnt),
			    BTN_POS_X + (Cnt * BTN_SIZE) + (BTN_SIZE DIV 2),
			    BTN_POS_Y + BTN_SIZE + 1,
			    CBlack, CWhite);
    al_release_screen;
  END;



(* Saves the current board in a file.  The name of the map is
   "boardN.brd". *)
  PROCEDURE SaveBoard;

  (* Translates the tile value to the file value.  This way the file can be
     loaded by a text editor. *)
    FUNCTION TranslateTile (Tile: BYTE): CHAR;
    VAR
      Dictionary: ARRAY [0..6] OF CHAR =
	('A', 'D', 'E', 'B', 'L', 'M', 'N');
    BEGIN
      TranslateTile := Dictionary[Tile];
    END;

  VAR
    Path, FileName: STRING; F: TEXT; { File definition. }
    Column: STRING[15];	     { To save the file. }
    x, y: INTEGER;
  BEGIN
  { Build the file name.
    First, get the path where is the execubable. }
    Path :=  ExtractFilePath (PARAMSTR (0));
  { Create the file name. }
    Filename := 'board' + IntToStr (BoardNumber) + '.brd';
  { Build the final name with path. }
    Filename := Path + Filename;
  { Open the file. }
  {$I-} { To save file errors in IOResult. }
    Assign (F, FileName); Rewrite (F);
    IF IOResult <> 0 THEN
    BEGIN
      ErrorMessage ('Can''t open/create the file ''board' +
		    IntToStr (BoardNumber) + '.brd''');
      EXIT;
    END;
  { First line is the length of the board. }
    WriteLn (F, BoardLength);
  { Save the columns. }
    FOR x := 1 TO BoardLength DO
    BEGIN
    { Create the columns. }
      Column := StringOfChar (' ', 15);
      FOR y := 1 TO 15 DO
	Column [y] := TranslateTile (Board [x, 16 - y]);
    { Save the column. }
      WriteLn (F, Column);
    END;
  {$I+} { End storing file errors in IOResult. }
  { Close the file. }
    Close (F);
  { Check errors. }
    IF IOResult <> 0 THEN
    BEGIN
      ErrorMessage ('Can''t save the file ''board' +
		    IntToStr (BoardNumber) + '.brd''');
      EXIT;
    END;
  { Now, board isn't modified. }
    BoardModified := FALSE;
    al_set_window_title (CAPTION);
  END;



(* Helper procedure to create a new board. *)
  PROCEDURE CreateBoard;
  VAR
    x, y: INTEGER;
    Response: INTEGER;
  BEGIN
  { Ask the board size. }
    Response := al_alert3 ('Board size?', '', '',
	'&Small', '&Medium', '&Big',
	ORD('S'), ORD('M'), ORD('B'));
  { Set board size. }
    CASE Response OF
      1: BoardLength := 50;
      2: BoardLength := 100;
      3: BoardLength := 150;
    END;
  { Initialize board. }
    FOR x := 1 TO BoardLength DO
    BEGIN
      FOR y := 1 TO 13 DO
	Board [x, y] := T_VOID;
    { The floor. }
      Board [x, 14] := T_BLK2;
      Board [x, 15] := T_BLK1;
    END;
  { The starting and the ending tiles. }
    StartX := 8; StartY := 13;
    EndX := BoardLength - 8; EndY := 13;
    Board [StartX, StartY] := T_START;
    Board [  EndX,   EndY] := T_END;
  { A new board is a modified board. }
    BoardModified := TRUE;
    al_set_window_title (CAPTION_MODIFIED);
  END;



(* Asks for a new board and creates it. *)
  PROCEDURE NewBoard;
  BEGIN
  { If the board was modified, ask if should save it. }
    IF BoardModified THEN
    BEGIN
      IF AskYesNo ('The board was changed.  Save it?') THEN
	SaveBoard;
      RedrawScreen;
    END;
    AskBoardNumber;
  { Check if the board yet exists. }
    IF FileExists ('board'+ IntToStr (BoardNumber)+'.brd') THEN
    BEGIN
    { Ask if wants to create a new board. }
      IF NOT AskYesNo ('The board exists.  Overwrite it?') THEN
      { If answer is 'not' then exits without create it. }
        EXIT;
    END;
    CreateBoard;
  END;



(* Asks for a new board and loads it. *)
  PROCEDURE LoadBoard;
  BEGIN
  { If the board was modified, ask if should save it. }
    IF BoardModified THEN
    BEGIN
      IF AskYesNo ('The board was changed.  Save it?') THEN
	SaveBoard;
      RedrawScreen;
    END;
    AskBoardNumber;
  { Try to load the board.   Note: it uses the unit name to tell the compiler
    we want to use the function from the 'tilemap' unit. }
    IF NOT tilemap.LoadBoard (BoardNumber) THEN
    BEGIN
    { Ask if wants to create a new board. }
      IF AskYesNo ('The board doesn''t exist.  Create it?') THEN
        CreateBoard;
    END
    ELSE BEGIN
    { Restore the starting and ending tiles to draw them. }
      Board[StartX, StartY] := T_START;
      Board[EndX,   EndY]   := T_END;
    { Successfully loaded. }
      BoardModified := FALSE;
      al_set_window_title (CAPTION);
    END;
  END;



(* Procedure for the close button of the window.  It just modifies a
   variable which is checked somewhere in the main loop. *)
  PROCEDURE CloseButtonCallback; CDECL;
  BEGIN
  { See the main loop below to see the ending condition. }
    Key := AL_KEY_ESC;
  END;



(* End of the program.  Releases all resources used. *)
  PROCEDURE EndProgram;
  VAR
    Cnt: INTEGER;
  BEGIN
  { Deactivate the close button. }
    al_set_close_button_callback (NIL);
  { Hide the mouse. }
    al_show_mouse (NIL);
  { If the board was modified, ask if should save it. }
    IF BoardModified THEN
    BEGIN
      IF AskYesNo ('The board was changed.  Save it?') THEN
        SaveBoard;
    END;
  { Release resources. }
    FOR Cnt := 0 TO 6 DO
    BEGIN
      al_destroy_bitmap (BtnBmp[Cnt]);
    END;
    ReleaseData;
  { Shutdown Allegro. }
    al_exit;
  END;



(* Shows a preview of the tilemap. *)
  PROCEDURE Preview;
  VAR
    BmpOut: AL_BITMAPptr;
    ScrollX, ScrollY: INTEGER;
    Key: INTEGER;
  BEGIN
    ScrollX := 0; ScrollY := 0;
  { Since the game will run in 320x240 pixel screen, we need a buffer to make
    the 'zoom-in'. }
    BmpOut := al_create_bitmap (320, 240);
    REPEAT
      al_clear_to_color (BmpOut, al_makecol (0, 255, 255));
      FixScroll (BmpOut, ScrollX, ScrollY, ScrollX, ScrollY);
      DrawBoard (BmpOut, ScrollX, ScrollY);
    { Zoom the bitmap. }
      al_stretch_blit (BmpOut, al_screen,
			0, 0, BmpOut^.w, BmpOut^.h,
			0, 0, AL_SCREEN_W, AL_SCREEN_H);
    { Use the keyboard to move arround the map. }
      IF al_keyboard_needs_poll THEN al_poll_keyboard;
      Key := al_readkey SHR 8;
      IF Key = AL_KEY_RIGHT THEN
      BEGIN
	INC (ScrollX);
	IF (al_key_shifts AND AL_KB_SHIFT_FLAG) <> 0 THEN
	  INC (ScrollX, 3);
      END;
      IF Key = AL_KEY_LEFT THEN
      BEGIN
	DEC (ScrollX);
	IF (al_key_shifts AND AL_KB_SHIFT_FLAG) <> 0 THEN
	  DEC (ScrollX, 3);
      END;
    UNTIL Key = AL_KEY_ESC;
  { Destroy the buffer. }
    al_destroy_bitmap (BmpOut);
  END;



(* Checks the user input and proccess functions as load, save, etc. *)
  PROCEDURE ActionKeys (VAR Key: LONGINT);
  BEGIN
  { Deactivate the close button. }
    al_set_close_button_callback (NIL);
  { Q and [Esc] are the same. }
    IF Key = AL_KEY_F5 THEN Key := AL_KEY_ESC;
  { Hide the mouse. }
    al_show_mouse (NIL);
  { Check for keys. }
    CASE Key OF
    AL_KEY_F3: { Load board. }
      LoadBoard;
    AL_KEY_F1: { Create new board. }
      NewBoard;
    AL_KEY_F2: { Save board. }
      IF AskYesNo ('Save the board?') THEN
	SaveBoard;
    AL_KEY_F4: { Tilemap preview. }
      Preview;
    AL_KEY_ESC: { Exit. }
      IF NOT AskYesNo ('Quit the editor?') THEN
      { Set Key with an arbritrary value to prevent quit. }
	Key := AL_KEY_MAX;
    END;
  { Update the screen. }
    RedrawScreen;
    al_show_mouse (al_screen);
  { Restore the close button. }
    al_set_close_button_callback (@CloseButtonCallback);
  END;



(* Proccess the cursor keys. *)
  PROCEDURE CursorKeys (Key: LONGINT; VAR cx, cy: INTEGER);
  BEGIN
  { Change cursor only if the it's inside the map. }
    IF (1 <= cx) AND (cx <= BoardLength) AND (1 <= cy) AND (cy <= BoardHeight) THEN
    BEGIN
    { Move the cursor. }
      CASE Key OF
	AL_KEY_LEFT:
	  IF cx > 1 THEN DEC (cx);
	AL_KEY_RIGHT:
	  IF cx < BoardLength THEN INC (cx);
	AL_KEY_UP:
	  IF cy > 1 THEN DEC (cy);
	AL_KEY_DOWN:
	  IF cy < 15 THEN INC (cy);
      END;
    { Move the mouse cursor to the center of the tile. }
      al_position_mouse ((cx * SMALL_TSIZE) + (SMALL_TSIZE DIV 2),
			 (cy * SMALL_TSIZE) + (SMALL_TSIZE DIV 2));
    END;
  END;



(* Sets the tile type.  Note that it changes it only if necesary, this way
   it's faster and prevents flickering. *)
  PROCEDURE ChangeTile (tx, ty: INTEGER);
  BEGIN
  { Change tile only if it's inside the map... }
    IF (1 <= tx) AND (tx <= BoardLength) AND (1 <= ty) AND (ty <= BoardHeight) THEN
    { ... and it's different. }
      IF Board[tx, ty] <> TileButton THEN
      BEGIN
      { If it's the starting tile, delete the old one and store the new. }
        IF TileButton = T_START THEN
	BEGIN
	  Board[StartX, StartY] := T_VOID;
	  StartX := tx; StartY := ty;
	END;
      { If it's the ending tile, delete the old one and store the new. }
        IF TileButton = T_END THEN
	BEGIN
	  Board[EndX, EndY] := T_VOID;
	  EndX := tx; EndY := ty;
	END;
	Board[tx, ty] := TileButton;
	IF NOT BoardModified THEN
	BEGIN
	  BoardModified := TRUE;
	  al_set_window_title (CAPTION_MODIFIED);
	END;
      { Show the new map. }
	al_show_mouse (NIL); { Hide the mouse to draw. }
	al_acquire_screen;
	DrawBoardMiniature (al_screen);
	al_release_screen;
	al_show_mouse (al_screen); { Show the mouse again. }
      END;
  END;



(* The program starts here. *)
VAR
  cx, cy, mb: INTEGER; { Coordinates of the cursor and state of the button. }
  Tmp: INTEGER;
BEGIN
{ Initialize the program. }
  IF NOT InitProgram THEN EXIT;
  LoadBoard;
  RedrawScreen;
  al_set_close_button_callback (@CloseButtonCallback); { Set the close button. }
  al_show_mouse (al_screen); { Show the mouse. }
  mb := al_mouse_b;
(* Main loop. *)
  REPEAT
  { Fair play.  Releases some CPU pressure. }
    al_rest (1);
  { Mouse. }
    IF al_mouse_needs_poll THEN al_poll_mouse;
  { Translate mouse coordinates to map coordinates. }
    cx := al_mouse_x DIV SMALL_TSIZE;
    cy := al_mouse_y DIV SMALL_TSIZE;
  { If the mouse button state changes... }
    IF al_mouse_b <> mb THEN
    BEGIN
      mb := al_mouse_b; { ...store the new state. }
      IF mb = 0 THEN { If the button is released, then it means it was pressed. }
      BEGIN
      { If mouse is inside the buttons... }
	IF (BTN_POS_X <= al_mouse_x) AND (al_mouse_x <= BTN_POS_X + (BTN_SIZE * 7)) AND
	   (BTN_POS_Y <= al_mouse_y) AND (al_mouse_y <= BTN_POS_Y + BTN_SIZE) THEN
	BEGIN
	{ ...calculate and assign the tile. }
	  Tmp := (al_mouse_x - BTN_POS_X) DIV BTN_SIZE;
	  IF (0 <= Tmp) AND (Tmp <= 6) THEN
	  BEGIN
	    TileButton := Tmp;
	    al_show_mouse (NIL); { Hide the mouse to draw. }
	    DrawButtons; { Change the button cursor. }
	    al_show_mouse (al_screen); { Show the mouse again. }
	  END;
	END;
      END
      ELSE 
      { If it's pressed, may be it wants to put a tile. }
      REPEAT { To make fast editing using mouse. }
        ChangeTile (cx, cy);
	IF al_mouse_needs_poll THEN al_poll_mouse;
      { Translate mouse coordinates to map coordinates. }
	cx := al_mouse_x DIV SMALL_TSIZE;
	cy := al_mouse_y DIV SMALL_TSIZE;
      UNTIL al_mouse_b = 0; { Repeat util it's released. }
    END;
  { Keyboard. }
    IF al_keyboard_needs_poll THEN al_poll_keyboard;
    IF al_keypressed THEN
    BEGIN
      Key := al_readkey SHR 8; { Get the scan code. }
    { Check if an action key was pressed. }
      IF Key IN [AL_KEY_F1, AL_KEY_F2, AL_KEY_F3, AL_KEY_F4, AL_KEY_F5, AL_KEY_ESC] THEN
	ActionKeys (Key);
    { Check if a number key was pressed. }
      IF ((AL_KEY_0 - 1) < Key) AND (Key < AL_KEY_7) THEN
      BEGIN
      { New tile selected. }
	TileButton := Key - AL_KEY_0;
	al_show_mouse (NIL); { Hide the mouse to draw. }
	DrawButtons; { Change the button cursor. }
	al_show_mouse (al_screen); { Show the mouse again. }
      END;
    { Check if a cursor key was pressed. }
      IF Key IN [AL_KEY_LEFT, AL_KEY_RIGHT, AL_KEY_UP, AL_KEY_DOWN] THEN
	CursorKeys (Key, cx, cy);
    { If space bar pressed, change the tile. }
      IF Key = AL_KEY_SPACE THEN
        ChangeTile (cx, cy);
    END;
  { Exit if [Esc] key was pressed. }
  UNTIL Key = AL_KEY_ESC;
{ End of the program. }
  EndProgram;
END.
