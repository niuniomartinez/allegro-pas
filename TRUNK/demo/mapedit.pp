PROGRAM mapedit;
(* This is the map editor for the Allegro.pas demo game.

  It's designed in a way that should be easy to expand and upgrade, so you can
  use it in your own projects.

  By Ñuño Martínez.
 *)

  USES
    allegro,
    albase,   { We need Allegro's types. }
    algui,    { To use the Allegro's GUI. }
    alfile,   { To access to Allegro's data files. }
    sysutils,
    tilemap;  { Tilemap management. }

  CONST
  (* Window caption. *)
    CAPTION = 'Allegro.pas Demo Game Map Editor - ';
  (* Size of tile buttons. *)
    BTN_SIZE = 32;
  (* Minimun size of map.  Maximun is defined at Tilemap. *)
    MIN_SIZE = 10;
  (* Key shortcuts to use in dialog definition. *)
    scINTRO = 13;

{ Indexes for data in the datafile. }
{$I mapedit.inc}

  VAR
  (* Some data used by the editor. *)
    Data: AL_DATAFILEptr;
  (* Some colors. *)
    CWhite, CBlack, CRed, CBlue, CGreen, CButton: LONGINT;
  (* The special edition buttons:  Delete, Start and End. *)
    EditButtons: ARRAY [0..2] OF AL_BITMAPptr;
  (* Main menu description. *)
    ProgMenu: ARRAY [0..4] OF AL_MENU;
    MapMenu: ARRAY [0..5] OF AL_MENU;
    ConfigMenu: ARRAY [0..2] OF AL_MENU;
    CfgBgMenu: ARRAY [0..3] OF AL_MENU;
    MainMenu: ARRAY [0..3] OF AL_MENU;
  (* Main dialog.  This is the editor itself. *)
    MainDialog: ARRAY [0..19] OF AL_DIALOG;
  (* Index of map scroll bar controls. *)
    NdxScrollBarW, NdxScrollBarH,
  (* Index of the map editor control. *)
    NdxMapedit: INTEGER;
  (* Name of the last map loaded/saved. *)
    MapName: STRING;
  (* Flag to know if map was modified. *)
    MapModified,
  (* This flag tells to dlgMapEditor if its safe to draw the map. *)
    CanDrawMap: BOOLEAN;
  (* Active brush.  Uses the CONST defined in mapedit.inc. *)
    ActiveBrush,
  (* The selected tile. *)
    ActiveTile: INTEGER;



(*****************************************************************************
 * Low level stuff like bitmap management, error handling, etc.
 *)

(* Shows a simple error message. *)
  PROCEDURE ErrorMessage (Line1, Line2: STRING);
  VAR
    OldFg, OldBg: LONGINT;
  BEGIN
  { Use different set of colors. }
    OldFg := al_gui_fg_color; OldBg := al_gui_bg_color;
    al_gui_fg_color := CWhite; al_gui_bg_color := CRed;

    al_alert ('ERROR', Line1, Line2, 'Oh dear', '', scINTRO, 0);
  { Restores colors. }
    al_gui_fg_color := OldFg; al_gui_bg_color := OldBg
  END;



(* Destroys bitmap if it exists. *)
  PROCEDURE DestroyBmp (Bitmap: AL_BITMAPptr); INLINE;
  BEGIN
    IF Bitmap <> NIL THEN
      al_destroy_bitmap (Bitmap);
  END;



(* Clone the given bitmap.  It changes size to fit to the given size and
   changes color depth if needed too.
 *)
  FUNCTION CloneBitmap (Original: AL_BITMAPptr; w, h: INTEGER): AL_BITMAPptr;
  VAR
    Tmp: AL_BITMAPptr = NIL;
  BEGIN
    CloneBitmap := al_create_bitmap (w, h);
  { Change color depth if needed. }
    IF al_bitmap_color_depth (Original) <> al_bitmap_color_depth (CloneBitmap)
    THEN BEGIN
      Tmp := al_create_bitmap (Original^.w, Original^.h);
      al_blit (Original, Tmp, 0, 0, 0, 0, Original^.w, Original^.h);
      Original := Tmp;
    END;
  { Clone. }
    al_stretch_blit (
      Original, CloneBitmap,
      0, 0, Original^.w, Original^.h,
      0, 0, CloneBitmap^.w, CloneBitmap^.h
    );
    DestroyBmp (Tmp);
  END;



(* Helper function to make simple questions. *)
  FUNCTION AskYesNo (Question1, Question2: STRING): BOOLEAN; INLINE;
  BEGIN
    AskYesNo :=
      al_alert (Question1, Question2, '', '&Yes', '&No', Ord ('y'), Ord ('n')) = 1;
  END;



(* Sets "map modified" flag and window caption. *)
  PROCEDURE SetMapModified;
  BEGIN
    IF NOT MapModified THEN
      MapModified := TRUE;
    al_set_window_title (CAPTION + ExtractFilename (MapName)+' [modified]');
  END;



(* Removes "map modified" flag and window caption. *)
  PROCEDURE ResetMapModified;
  BEGIN
    IF MapModified THEN
      MapModified := FALSE;
    al_set_window_title (CAPTION + ExtractFilename (MapName));
  END;



(*****************************************************************************
 * GUI stuff.  That are new GUI controls, etc.
 *)

(* Callback for map scrollbars. *)
  FUNCTION ScrollBarHandler (dp3: AL_VOIDptr; d2: AL_INT): AL_INT; CDECL;
  BEGIN
  { Just redraw map editor to do the scroll. }
    al_object_message (@MainDialog[NdxMapedit], AL_MSG_DRAW, 0);
    ScrollBarHandler := AL_D_O_K;
  END;



(* Draws a dotted rectangle.  Used to show wich object is selected. *)
  PROCEDURE DrawDottedRect (CONST X1, Y1, X2, Y2: INTEGER);
  VAR
    Cnt: INTEGER;
  BEGIN
    FOR Cnt := X1 TO X2 DO
      IF Cnt MOD 2 = 0 THEN
      BEGIN
	al_putpixel (al_gui_get_screen, Cnt, Y1, CWhite);
	al_putpixel (al_gui_get_screen, Cnt, Y2, CWhite);
      END
      ELSE BEGIN
	al_putpixel (al_gui_get_screen, Cnt, Y1, CBlack);
	al_putpixel (al_gui_get_screen, Cnt, Y2, CBlack);
      END;
    FOR Cnt := Y1 TO Y2 DO
      IF Cnt MOD 2 = 0 THEN
      BEGIN
	al_putpixel (al_gui_get_screen, X1, Cnt, CWhite);
	al_putpixel (al_gui_get_screen, X2, Cnt, CWhite);
      END
      ELSE BEGIN
	al_putpixel (al_gui_get_screen, X1, Cnt, CBlack);
	al_putpixel (al_gui_get_screen, X2, Cnt, CBlack);
      END;
  END;



(* Draws a square with an "x" inside to show that object isn't available. *)
  PROCEDURE DrawNilObject (CONST X1, Y1, X2, Y2: INTEGER; CONST Clr: LONGINT);
  BEGIN
    al_rectfill (al_gui_get_screen, X1, Y1, X2, Y2, Clr);
    al_rect (al_gui_get_screen, X1, Y1, X2, Y2, al_gui_mg_color);
    al_line (al_gui_get_screen, X1, Y1, X2, Y2, al_gui_mg_color);
    al_line (al_gui_get_screen, X2, Y1, X1, Y2, al_gui_mg_color);
  END;



(* Helper procedure to fix scroll bar ranges. *)
  PROCEDURE FixScrollBarsRange;
  VAR
    SX, SY: INTEGER;
  BEGIN
  { Get maximun scroll range. }
    SX := MapWidth * TSIZE; SY := MapHeight * TSIZE;
    FixScroll (MainDialog[NdxMapedit].dp, SX, SY, SX, SY);
  { If map is smaller than editing space, then SX and/or SY became negative,
    so it will draw it wrong.

    Hack note: You don't should do this in your games, as your output bitmap
		should be same size or bigger than your maps. }
    IF SX < 0 THEN SX := 1;
    IF SY < 0 THEN SY := 1;
  { Now, update scroll bar ranges. }
    MainDialog[NdxScrollBarW].d1 := SX;
    MainDialog[NdxScrollBarW].d2 := 0;
    MainDialog[NdxScrollBarH].d1 := SY;
    MainDialog[NdxScrollBarH].d2 := SY;
  END;



(* Extends Allegro radio button to build the "brush selector".

  Difference is that uses a bitmap (in dp) to draw itself and shows selection
  with inverted colors. *)
  FUNCTION dlgBrushSelProc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT; CDECL;
  VAR
    Bmp: AL_BITMAPptr;
  BEGIN
    IF msg = AL_MSG_DRAW THEN
    BEGIN
      Bmp := d^.dp;
      al_blit (Bmp, al_gui_get_screen, 0, 0, d^.x, d^.y, Bmp^.w, Bmp^.h);
      IF (d^.flags AND AL_D_SELECTED) = AL_D_SELECTED THEN
      BEGIN
	al_xor_mode (TRUE);
	al_rectfill (al_gui_get_screen, d^.x, d^.y, d^.x + Bmp^.w - 1, d^.y +Bmp^.h - 1, CWhite);
	al_xor_mode (FALSE);
      END;
      IF (d^.flags AND AL_D_GOTFOCUS) = AL_D_GOTFOCUS THEN
	DrawDottedRect (d^.x, d^.y, d^.x + Bmp^.w - 1, d^.y +Bmp^.h - 1);
      dlgBrushSelProc := AL_D_O_K;
    END
    ELSE BEGIN
    { Any else, radio button. }
      dlgBrushSelProc := al_d_radio_proc (msg, d, c);
    { Check if selected. }
      IF (d^.flags AND AL_D_SELECTED) = AL_D_SELECTED THEN
	ActiveBrush := d^.d2;
    END;
  END;



(* Creates a custom GUI object to show and edit the map. *)
  FUNCTION dlgMapEditorProc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT; CDECL;

  (* Helper procedure to draw the control. *)
    PROCEDURE DrawControl;
    VAR
      SX, SY, MW, MH, MSX, MSY: INTEGER;
    BEGIN
    { See if map is smaller than the control to draw a frame. }
      IF MapHeight * TSIZE < d^.h THEN
	MH := MapHeight * TSIZE - 1
      ELSE
	MH := d^.h;
      IF MapWidth * TSIZE < d^.w THEN
	MW := MapWidth * TSIZE - 1
      ELSE
	MW := d^.w;
      IF (MW < d^.w) OR (MH < d^.h) THEN
	al_rectfill (d^.dp, 0, 0, d^.w, d^.h, al_gui_mg_color);
    { Clipping rectangle. }
    { Draw a pattern to show transparent tiles. }
      al_drawing_mode (AL_DRAW_MODE_COPY_PATTERN, d^.dp2, 0, 0);
      al_rectfill (d^.dp, 0, 0, MW, MH, 0);
      al_solid_mode;
    { Use flag AL_D_SELECTED if background is a bitmap.  Then redraw the bitmap
      using blit because AL_DRAW_MODE_COPY_PATTERN cuts bitmap size if it's not
      power of two.  It still uses pattern to be sure it fills background. }
      IF d^.flags AND AL_D_SELECTED = AL_D_SELECTED THEN
	al_blit (
	  d^.dp2, d^.dp, 0, 0, 0, 0,
	  AL_BITMAPptr (d^.dp)^.w, AL_BITMAPptr (d^.dp)^.h
	);
    { Draw map.  Height scroll bar goes "backwards". }
      SX := 0;
      SY := MainDialog[NdxScrollBarH].d1 - MainDialog[NdxScrollBarH].d2;
      FixScroll (d^.dp, MainDialog[NdxScrollBarW].d2, sY, SX, SY);
    { If map is smaller than editing space, then SX and/or SY became negative,
      so it will draw it wrong.

      Hack note: You don't should do this in your games, as your output bitmap
		 should be same size or bigger than your maps. }
      IF SX < 0 THEN SX := 0;
      IF SY < 0 THEN SY := 0;
      DrawMap  (d^.dp, SX, SY);
    { Gets displacement to use it later. }
      MSX := SX MOD TSIZE; MSY := SY MOD TSIZE;
    { Put map on screen. }
      al_blit (d^.dp, al_gui_get_screen, 0, 0, d^.x + 1, d^.y + 1, d^.w, d^.h);
    { If mouse is in, draws a rectangle showing the brush. }
      SX := al_mouse_pos SHR 16;
      SY := al_mouse_pos AND $0000FFFF;
      IF (d^.x < SX) AND (SX < d^.x + MW)
      AND (d^.y < SY) AND (SY < d^.y + MH)
      THEN BEGIN
      { Set clipping rectangle to avoid drawing outside control. }
	al_set_clip_rect (al_gui_get_screen, d^.x + 1, d^.y + 1, d^.x + MW - 2, d^.y + MH + 1);
	CASE ActiveBrush OF
	  BMP_1x1:
	    BEGIN
	      MW := TSIZE; MH := TSIZE;
	    END;
	  BMP_1x2, BMP_O_1x2:
	    BEGIN
	      MW := TSIZE; MH := TSIZE * 2;
	    END;
	  BMP_2x1, BMP_O_2x1:
	    BEGIN
	      MW := TSIZE * 2; MH := TSIZE;
	    END;
	  BMP_2x2, BMP_O_2x2:
	    BEGIN
	      MW := TSIZE * 2; MH := TSIZE * 2;
	    END;
	END;
      { Calculates coordinates of tile square, fixing with displacement. }
	SX := (((SX + MSX) DIV TSIZE) * TSIZE) - MSX;
	SY := (((SY + MSY) DIV TSIZE) * TSIZE) - MSY;
	MW := SX + MW; MH := SY + MH;
	DrawDottedRect (SX, SY, MW, MH);
      { Restore clipping rectangle. }
	al_set_clip_rect (al_gui_get_screen, 0, 0, AL_SCREEN_W, AL_SCREEN_H);
      END;
    END;

  (* Helper procedure for mouse input. *)
    PROCEDURE MouseInput;

    (* Helper to set the tile avoiding NIL ones. *)
      PROCEDURE SetTile (X, Y, Tile: INTEGER);
      BEGIN
	IF (Tile < 256) AND ((Tileset[Tile] <> NIL) OR (Tile = 0)) THEN
	BEGIN
	{ Control start and end points. }
	  IF (X = StartX) AND (Y = StartY) THEN
	  BEGIN
	    StartX := -1; StartY := -1;
	  END;
	  IF (X = EndX) AND (Y = EndY) THEN
	  BEGIN
	    EndX := -1; EndY := -1;
	  END;
	  IF Tile = T_START THEN
	  BEGIN
	    IF StartX >= 0 THEN
	      Map[StartX, StartY] := T_VOID;
	    StartX := X; StartY := Y;
	  END;
	  IF Tile = T_END THEN
	  BEGIN
	    IF EndX >= 0 THEN
	      Map[EndX, EndY] := T_VOID;
	    EndX := X; EndY := Y;
	  END;
	{ Put the new tile. }
	  Map[X, Y] := Tile;
	  SetMapModified;
	END;
      END;

    VAR
      Px, Py: INTEGER;
    BEGIN
    { While button is pressed, put tiles. }
      REPEAT
      { NOTE: Here we should use al_gui_mouse_* stuff, but for some reason the
	FPC compiler insists that it's not possible. }
	IF al_mouse_needs_poll THEN al_poll_mouse;
      { Tile position. }
	Px := (al_mouse_x - d^.x + MainDialog[NdxScrollBarW].d2) DIV TSIZE;
	Py := (al_mouse_y - d^.y + MainDialog[NdxScrollBarH].d1 - MainDialog[NdxScrollBarH].d2) DIV TSIZE;
      { Draw tile(s). }
	SetTile (Px, Py, ActiveTile);
	CASE ActiveBrush OF
	  BMP_2x1:
	    SetTile (Px + 1, Py, ActiveTile);
	  BMP_1x2:
	    SetTile (Px, Py + 1, ActiveTile);
	  BMP_2x2:
	    BEGIN
	      SetTile (Px + 1, Py    , ActiveTile);
	      SetTile (Px    , Py + 1, ActiveTile);
	      SetTile (Px + 1, Py + 1, ActiveTile);
	    END;
	  BMP_O_2x1:
	    SetTile (Px + 1, Py, ActiveTile + 1);
	  BMP_O_1x2:
	    SetTile (Px, Py + 1, ActiveTile + 1);
	  BMP_O_2x2:
	    BEGIN
	      SetTile (Px + 1, Py    , ActiveTile + 1);
	      SetTile (Px    , Py + 1, ActiveTile + 2);
	      SetTile (Px + 1, Py + 1, ActiveTile + 3);
	    END;
	END;
      { Redraw to show result. }
	al_vsync;
	al_object_message (d, AL_MSG_DRAW, 0);
      UNTIL al_mouse_b = 0;
      dlgMapEditorProc := AL_D_REDRAWME;
    END;

  BEGIN
  { Default return value. }
    dlgMapEditorProc := AL_D_O_K;
  { Process message. }
    CASE msg OF
      AL_MSG_START: { Object initialization. }
	BEGIN
	{ Create a bitmpap to draw the map using "double-buffer" and prevent flickering. }
	  d^.dp := al_create_bitmap (d^.w - 2, d^.h - 2);
	{ Bitmap used as background to show transparency. }
	  d^.dp2 := al_create_bitmap (TSIZE * 4, TSIZE * 4);
	  al_clear_to_color (d^.dp2, d^.bg);
	  al_rectfill (d^.dp2, TSIZE * 2, 0, TSIZE * 4 - 1, TSIZE * 2 - 1, al_gui_mg_color);
	  al_rectfill (d^.dp2, 0, TSIZE * 2, TSIZE * 2 - 1, TSIZE * 4 - 1, al_gui_mg_color);
	{ Get mouse position to know when it's moved. }
	  d^.d1 := al_mouse_pos;
	END;
      AL_MSG_END: { Object destruction. }
	BEGIN
	{ Release resources. }
	  DestroyBmp (d^.dp);
	  DestroyBmp (d^.dp2);
	END;
      AL_MSG_DRAW: { To show the object. }
	IF CanDrawMap THEN
	  DrawControl
	ELSE
	  DrawNilObject (d^.x , d^.y, d^.x + d^.w - 1, d^.y + d^.h - 1, d^.bg);
      AL_MSG_IDLE:
      { If mouse moved and is inside the control, redraw it. }
	IF d^.d1 <> al_mouse_pos THEN
	BEGIN
	  d^.d1 := al_mouse_pos;
	  IF (d^.x < d^.d1 SHR 16) AND (d^.d1 SHR 16 < d^.x + d^.w)
	  AND (d^.y < d^.d1 AND $0000FFFF)
	  AND (d^.d1 AND $0000FFFF < d^.y + d^.h)
	  THEN
	    dlgMapEditorProc := AL_D_REDRAWME;
	END;
      AL_MSG_CLICK:
	IF CanDrawMap THEN
	  MouseInput;
    END;
  END;



(* Creates a custom GUI object that shows the available tiles and allows to
   select them.  Note that it uses the external Tileset list, so you can't use
   more than one tileset.

   d1 stores the index of the selected tile.
   d2 stores the first tile to draw (scroll).
 *)
  FUNCTION dlgTileSelectorProc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT; CDECL;

  (* Helper function to know how many tiles fits in the selector. *)
    FUNCTION NumOfTiles: INTEGER;
    BEGIN
    { "2" is the room for arrows. }
      NumOfTiles := (d^.w DIV BTN_SIZE) - Length (EditButtons) - 2;
    END;

  (* Helper procedure to draw the object. *)
    PROCEDURE DrawControl;
    CONST
    { X coordinate to first tile button.  With space for edition buttons and left
      arrow. }
      FB_X = (BTN_SIZE * 3) + (BTN_SIZE DIV 2);
    VAR
      Ndx, X: INTEGER;
    BEGIN
    { Clean background. }
      al_d_box_proc (msg, d, c);
    { Draw edition buttons. }
      FOR Ndx := LOW (EditButtons) TO HIGH (EditButtons) DO
      BEGIN
	X := d^.x + (BTN_SIZE * Ndx) + 1;
	al_stretch_blit (
	  EditButtons[Ndx], al_gui_get_screen,
	  0, 0, EditButtons[Ndx]^.w, EditButtons[Ndx]^.h,
	  X, d^.y + 1, BTN_SIZE, BTN_SIZE
	);
      { Selected tile. }
	IF -d^.d1 = 2 - Ndx THEN
	  DrawDottedRect (
	    X, d^.y + 1, X + BTN_SIZE - 1, d^.y + BTN_SIZE
	  );
      END;
    { Left and right arrows. }
      X := BTN_SIZE * 3; { Space for edition buttons. }
      al_triangle (
	al_gui_get_screen,
	d^.x + X +   BTN_SIZE DIV 8      , d^.y +   BTN_SIZE DIV 2,
	d^.x + X + ((BTN_SIZE DIV 8) * 3), d^.y +   BTN_SIZE DIV 4,
	d^.x + X + ((BTN_SIZE DIV 8) * 3), d^.y + ((BTN_SIZE DIV 4) * 3),
	d^.fg
      );
      al_triangle (
	al_gui_get_screen,
	d^.x + d^.w -   BTN_SIZE DIV 8      , d^.y +   BTN_SIZE DIV 2,
	d^.x + d^.w - ((BTN_SIZE DIV 8) * 3), d^.y +   BTN_SIZE DIV 4,
	d^.x + d^.w - ((BTN_SIZE DIV 8) * 3), d^.y + ((BTN_SIZE DIV 4) * 3),
	d^.fg
      );
    { Draw the tiles.  Draw them from right to left. }
      FOR Ndx := NumOfTiles DOWNTO 0 DO
      BEGIN
      { Calculates X coordinate. }
	X := FB_X + d^.x + (BTN_SIZE * Ndx);
      { Be sure we don't try to draw a non existent tile. }
	IF (d^.d2 + Ndx < Length (Tileset) - 2)
	AND (Tileset[d^.d2 + Ndx] <> NIL)
	THEN
	  al_stretch_blit (
	    Tileset[d^.d2 + Ndx], al_gui_get_screen,
	    0, 0, Tileset[d^.d2 + Ndx]^.w, Tileset[d^.d2 + Ndx]^.h,
	    X, d^.y + 1, BTN_SIZE, BTN_SIZE
	  )
	ELSE
	{ If tile doesn't exist, draw a "X". }
	  DrawNilObject (X , d^.y + 1, X + BTN_SIZE, d^.y + BTN_SIZE, d^.bg);
      { Selected tile. }
	IF d^.d1 = d^.d2 + Ndx THEN
	  DrawDottedRect (
	    X, d^.y + 1, X + BTN_SIZE - 1, d^.y + BTN_SIZE
	  );
      END;
      IF (d^.flags AND AL_D_GOTFOCUS) = AL_D_GOTFOCUS THEN
        DrawDottedRect (d^.x, d^.y, d^.x + d^.w - 1, d^.y + d^.h - 1);
    END;

  (* Helper procedure for key input. *)
    PROCEDURE KeyInput;
    BEGIN
    { Key input. }
      CASE c SHR 8 OF
	AL_KEY_LEFT:
	  BEGIN
	    DEC (d^.d1);
	    dlgTileSelectorProc := AL_D_USED_CHAR;
	  END;
	AL_KEY_RIGHT:
	  BEGIN
	    INC (d^.d1);
	    dlgTileSelectorProc := AL_D_USED_CHAR;
	  END;
	AL_KEY_PGUP:
	  IF d^.d1 > 0 THEN
	  BEGIN
	    DEC (d^.d1, NumOfTiles);
	    IF d^.d1 < 1 THEN
	      d^.d1 := 1;
	    DEC (d^.d2, NumOfTiles);
	    dlgTileSelectorProc := AL_D_USED_CHAR;
	  END;
	AL_KEY_PGDN:
	  IF d^.d1 > 0 THEN
	  BEGIN
	    INC (d^.d1, NumOfTiles);
	    INC (d^.d2, NumOfTiles);
	    dlgTileSelectorProc := AL_D_USED_CHAR;
	  END;
      END;
    { If key was used, do some extra work. }
      IF dlgTileSelectorProc = AL_D_USED_CHAR THEN
      BEGIN
      { Check limits. }
	IF d^.d1 < -2 THEN
	  d^.d1 := -2
	ELSE IF d^.d1 > MAX_TILES THEN
	  d^.d1 := MAX_TILES;

	IF d^.d2 > d^.d1 THEN
	  d^.d2 := d^.d1
	ELSE IF d^.d1 > d^.d2 + NumOfTiles THEN
	  d^.d2 := d^.d1 - NumOfTiles;

	IF d^.d2 < 1 THEN
	  d^.d2 := 1
	ELSE IF d^.d2 + NumOfTiles > MAX_TILES THEN
	  d^.d2 := MAX_TILES - NumOfTiles;
      { Selected tile. }
	IF d^.d1 > 0 THEN
	  ActiveTile := d^.d1
	ELSE CASE d^.d1 OF
	   0:
	    ActiveTile := T_END;
	  -1:
	    ActiveTile := T_START;
	  -2:
	    ActiveTile := T_VOID;
	END;
      { Redraw component. }
	al_object_message (d, AL_MSG_DRAW, 0);
      END;
    END;

  (* Helper procedure for mouse input. *)
    PROCEDURE MouseInput;
    VAR
      mX, mY: LONGINT;
    BEGIN
    { NOTE: Here we should use al_gui_mouse_* stuff, but for some reason the
      FPC compiler insists that it's not possible. }
      IF al_mouse_needs_poll THEN al_poll_mouse;
    { Wait until user releases the mouse button. }
      WHILE al_mouse_b <> 0 DO
      BEGIN
	mX := al_mouse_x; mY := al_mouse_y;
        IF al_mouse_needs_poll THEN al_poll_mouse;
      END;
    { Check where the mouse cursor is when button was released. }
      IF (d^.y < my) AND (my < d^.h + d^.w) THEN
      BEGIN
      { Edition buttons. }
	IF mX <= BTN_SIZE * 3 THEN
	BEGIN
	  CASE mX DIV BTN_SIZE OF
	    0:
	      BEGIN
		ActiveTile := T_VOID;
		d^.d1 := -2;
	      END;
	    1:
	      BEGIN
		ActiveTile := T_START;
		d^.d1 := -1;
	      END;
	    2:
	      BEGIN
		ActiveTile := T_END;
		d^.d1 := 0;
	      END;
	  END;
	END
      { Tiles. }
	ELSE IF ((BTN_SIZE * 7) DIV 2 <= mX) AND (mX < d^.w - (BTN_SIZE DIV 2)) THEN
	BEGIN
	  ActiveTile := ((mX - ((BTN_SIZE * 7) DIV 2)) DIV BTN_SIZE) + d^.d2;
	  d^.d1 := ActiveTile;
	END
      { Arrow buttons. }
	ELSE BEGIN
	  IF mX < d^.w DIV 2 THEN
	  BEGIN
	    IF d^.d1 > 0 THEN
	    BEGIN
	      DEC (d^.d1, NumOfTiles DIV 2);
	      IF d^.d1 < 1 THEN
		d^.d1 := 1;
	      DEC (d^.d2, NumOfTiles DIV 2);
	    END;
	  END
	  ELSE BEGIN
	    IF d^.d1 > 0 THEN
	    BEGIN
	      INC (d^.d1, NumOfTiles DIV 2);
	      INC (d^.d2, NumOfTiles DIV 2);
	    END;
	  END;
	{ Check limits. }
	  IF d^.d1 < -2 THEN
	    d^.d1 := -2
	  ELSE IF d^.d1 > MAX_TILES THEN
	    d^.d1 := MAX_TILES;

	  IF d^.d2 > d^.d1 THEN
	    d^.d2 := d^.d1
	  ELSE IF d^.d1 > d^.d2 + NumOfTiles THEN
	    d^.d2 := d^.d1 - NumOfTiles;

	  IF d^.d2 < 1 THEN
	    d^.d2 := 1
	  ELSE IF d^.d2 + NumOfTiles > MAX_TILES THEN
	    d^.d2 := MAX_TILES - NumOfTiles;
	{ Selected tile. }
	  ActiveTile := d^.d1;
	END;
      { In any case, redraw it. }
	dlgTileSelectorProc := AL_D_REDRAWME;
      END;
    END;

  BEGIN
  { Default return value. }
    dlgTileSelectorProc := AL_D_O_K;
  { Process message. }
    CASE msg OF
      AL_MSG_START: { Object initialization. }
	BEGIN
	{ Be sure that selected bitmap isn't out of bounds. }
	  IF (1 > d^.d1) OR (d^.d1 >= Length (Tileset)) THEN
	    d^.d1 := 1;
	  ActiveTile := d^.d1;
	{ Uses d2 to store the index of the first tile to draw. }
	  d^.d2 := 1;
	END;
      AL_MSG_WANTFOCUS:
	dlgTileSelectorProc := AL_D_WANTFOCUS;
      AL_MSG_CHAR:
	KeyInput;
      AL_MSG_CLICK:
	MouseInput;
      AL_MSG_DRAW: { Draws the object. }
	DrawControl;
    END;
  END;



(*****************************************************************************
 * Program stuff:  loading, saving, quitting, etc.
 *)

(* Shows help. *)
  FUNCTION Help: AL_INT; CDECL;
  VAR
    HelpFile: FILE OF CHAR;
    HelpText: ANSISTRING;
    C: CHAR;
    DlgHelp: ARRAY [0..5] OF AL_DIALOG;
  BEGIN
  { Load help text. }
    HelpText := '';
    Assign (HelpFile, './mapedit.txt');
    Reset (HelpFile);
    WHILE NOT EOF (HelpFile) DO
    BEGIN
      Read (HelpFile, C);
      HelpText := HelpText + C;
    END;
    Close (HelpFile);
  { Create help viewer. }
    al_set_dialog_item (DlgHelp, 0, @al_d_shadow_box_proc, 0, 0, 532, 252, CBlack, CWhite, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgHelp, 1, @al_d_ctext_proc, 0, 4, 532, 8, CBlack, -1, 0, 0, 0, 0, AL_STRptr ('Help text'), NIL, NIL);
    al_set_dialog_item (DlgHelp, 2, @al_d_textbox_proc, 16, 16, 500, 208, CBlack, CWhite, 0, 0, 0, 0, AL_STRptr (HelpText), NIL, NIL);
    al_set_dialog_item (DlgHelp, 3, @al_d_button_proc, 16, 228, 156, 16, CBlack, CWhite, scINTRO, AL_D_EXIT, 0, 0, AL_STRptr ('Read'), NIL, NIL);
    al_set_dialog_item (DlgHelp, 4, @al_d_yield_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
  { End of dialog. }
    al_set_dialog_item (DlgHelp, 5, NIL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
  { Center the dialog. }
    al_centre_dialog (DlgHelp);

    al_popup_dialog (DlgHelp, -1);

    Help := AL_D_O_K;
  END;



(* Our about box. *)
  FUNCTION About: AL_INT; CDECL;
  BEGIN
    al_alert ('* mapedit *', CAPTION, 'by Guillermo Martínez', 'Ok', '', 0, 0);
    About := AL_D_O_K;
  END;



(* Configuration menu: Fullscreen. *)
  FUNCTION FullscreenCheck: AL_INT; CDECL;
  BEGIN
    al_active_menu^.flags := al_active_menu^.flags XOR AL_D_SELECTED;
    IF (al_active_menu^.flags AND AL_D_SELECTED) <> 0 THEN
    BEGIN
      IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_FULLSCREEN, 640, 480, 0, 0) THEN
      BEGIN
	al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0); { Be sure it's closed. }
	{ Shows an error message.
	  Can't use 'ErrorMessage' because the graphic mode isn't up. }
	al_message (al_error);
	FullscreenCheck := AL_D_CLOSE;
      END
      ELSE BEGIN
	FullscreenCheck := AL_D_REDRAW;
	al_show_mouse (al_screen);
      END;
    END
    ELSE BEGIN
      IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
      BEGIN
	al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0); { Be sure it's closed. }
	{ Shows an error message.
	  Can't use 'ErrorMessage' because the graphic mode isn't up. }
	al_message (al_error);
	FullscreenCheck := AL_D_CLOSE;
      END
      ELSE BEGIN
	FullscreenCheck := AL_D_REDRAW;
	al_show_mouse (al_screen);
      END;
    END;
  END;



(* Restores default map background. *)
  FUNCTION mnuRestoreBackground: AL_INT; CDECL;
  BEGIN
  { Restores background. }
    DestroyBmp (MainDialog[NdxMapedit].dp2);
    MainDialog[NdxMapedit].dp2 := al_create_bitmap (TSIZE * 4, TSIZE * 4);
    MainDialog[NdxMapedit].flags := MainDialog[NdxMapedit].flags AND NOT AL_D_SELECTED;
    al_clear_to_color (MainDialog[NdxMapedit].dp2, MainDialog[NdxMapedit].bg);
    al_rectfill (MainDialog[NdxMapedit].dp2, TSIZE * 2, 0, TSIZE * 4 - 1, TSIZE * 2 - 1, al_gui_mg_color);
    al_rectfill (MainDialog[NdxMapedit].dp2, 0, TSIZE * 2, TSIZE * 2 - 1, TSIZE * 4 - 1, al_gui_mg_color);
  { Change selection. }
    CfgBgMenu[0].flags := CfgBgMenu[0].flags OR AL_D_SELECTED;
    CfgBgMenu[1].flags := CfgBgMenu[1].flags AND NOT AL_D_SELECTED;
    CfgBgMenu[2].flags := CfgBgMenu[2].flags AND NOT AL_D_SELECTED;

    mnuRestoreBackground := AL_D_REDRAW;
  END;



(* Selects a solid color as map background. *)
  VAR
    dlgColor: ARRAY [0..9] OF AL_DIALOG;

(* Slider handler. *)
  FUNCTION RgbScrollBarHandler (dp3: AL_VOIDptr; d2: AL_INT): AL_INT; CDECL;
  BEGIN
    dlgColor[5].bg := al_makecol (
      dlgColor[2].d2, dlgColor[3].d2, dlgColor[4].d2
    );
    al_object_message (@dlgColor[5], AL_MSG_DRAW, 0);
    RgbScrollBarHandler := AL_D_O_K;
  END;

  FUNCTION mnuSelectSolidBackground: AL_INT; CDECL;
  BEGIN
  { Creates dialog. }
    al_set_dialog_item (dlgColor, 0, @al_d_shadow_box_proc, 0, 0, 352, 112, CBlack, CWhite, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (dlgColor, 1, @al_d_ctext_proc, 0, 4, 352, 8, CBlack, -1, 0, 0, 0, 0, AL_STRptr ('Select background color'), NIL, NIL);
    al_set_dialog_item (dlgColor, 2, @al_d_slider_proc, 18, 24, 256, 16, CRed, CWhite, 0, 0, 255, 0, NIL, @RgbScrollBarHandler, NIL);
    al_set_dialog_item (dlgColor, 3, @al_d_slider_proc, 18, 42, 256, 16, CGreen, CWhite, 0, 0, 255, 0, NIL, @RgbScrollBarHandler, NIL);
    al_set_dialog_item (dlgColor, 4, @al_d_slider_proc, 18, 60, 256, 16, CBlue, CWhite, 0, 0, 255, 0, NIL, @RgbScrollBarHandler, NIL);
    al_set_dialog_item (dlgColor, 5, @al_d_box_proc,    280, 24, 54, 54, CBlack, CBlack, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (dlgColor, 6, @al_d_button_proc, 16, 88, 156, 16, CBlack, CWhite, scINTRO, AL_D_EXIT, 0, 0, AL_STRptr ('&Ok'), NIL, NIL);
    al_set_dialog_item (dlgColor, 7, @al_d_button_proc, 180, 88, 156, 16, CBlack, CWhite, 0, AL_D_EXIT, 0, 0, AL_STRptr ('&Cancel'), NIL, NIL);
    al_set_dialog_item (dlgColor, 8, @al_d_yield_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
  { End of dialog. }
    al_set_dialog_item (dlgColor, 9, NIL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
  { Center the dialog. }
    al_centre_dialog (dlgColor);

    IF al_popup_dialog (dlgColor, -1) = 6 THEN
    BEGIN
    { Changes background. }
      al_clear_to_color (MainDialog[NdxMapedit].dp2, al_makecol (
        dlgColor[2].d2, dlgColor[3].d2, dlgColor[4].d2
      ));
      MainDialog[NdxMapedit].flags := MainDialog[NdxMapedit].flags AND NOT AL_D_SELECTED;
    { Change selection. }
      CfgBgMenu[0].flags := CfgBgMenu[0].flags AND NOT AL_D_SELECTED;
      CfgBgMenu[1].flags := CfgBgMenu[1].flags OR AL_D_SELECTED;
      CfgBgMenu[2].flags := CfgBgMenu[2].flags AND NOT AL_D_SELECTED;
    END;
    mnuSelectSolidBackground := AL_D_REDRAW;
  END;



(* Selects a bitmap as map background. *)
  FUNCTION mnuSelectBitmapBackground: AL_INT; CDECL;
  VAR
    Filename: STRING;
    Bmp: AL_BITMAPptr;
    Palette: AL_PALETTE;
  BEGIN
    mnuSelectBitmapBackground := AL_D_O_K;
  { The file selector. }
    IF al_file_select_ex ('Select bitmap file', FileName, 'BMP;LBM;PCX;TGA;/-h', 512, 320, 240) THEN
    BEGIN
      Bmp := al_load_bitmap (FileName, @Palette);
      IF Bmp = NIL THEN
	ErrorMessage ('Can''t load bitmap from file', FileName)
      ELSE
      BEGIN
      { Clones the bitmap. }
	DestroyBmp (MainDialog[NdxMapedit].dp2);
	al_set_palette (Palette);
	MainDialog[NdxMapedit].dp2 := CloneBitmap (Bmp, Bmp^.w, Bmp^.h);
	DestroyBmp (Bmp);
	MainDialog[NdxMapedit].flags := MainDialog[NdxMapedit].flags OR AL_D_SELECTED;
      { Change selection. }
	CfgBgMenu[0].flags := CfgBgMenu[0].flags AND NOT AL_D_SELECTED;
	CfgBgMenu[1].flags := CfgBgMenu[1].flags AND NOT AL_D_SELECTED;
	CfgBgMenu[2].flags := CfgBgMenu[2].flags OR AL_D_SELECTED;

	mnuSelectBitmapBackground := AL_D_REDRAW;
      END;
    END;
  END;



(* Releases resources used by tileset. *)
  PROCEDURE UnsetTileset;
  VAR
    Ndx: INTEGER;
  BEGIN
    FOR Ndx := LOW (Tileset) TO HIGH (Tileset) DO
      DestroyBmp (Tileset[Ndx]);
  END;



(* Loads and assigns a tileset. *)
  PROCEDURE SetTileset (TilesetName: STRING);
  {$I demo.inc}
  VAR
    Palette: AL_PALETTEptr;
    DemoData: AL_DATAFILEptr;
    Tmp, TmpTile: AL_BITMAPptr;
    buf: STRING;
    Ndx, X, Y: INTEGER;
  BEGIN
  { Hack note: May be you want to call UnsetTileset to clean up the Tileset
		first.  I don't do it because Demo game uses the same
		Tileset for all maps. }
  { Load the demo datafile into memory. }
    buf :=  'demo.dat';
    DemoData := al_load_datafile (buf);
  { Check if it was loaded. }
    IF DemoData = NIL THEN
    BEGIN
      ErrorMessage ('Can''t load data from file', buf);
      EXIT;
    END;
  { Select the palette which was loaded from the datafile. }
    Palette := DemoData^[GAME_PAL].dat;
    al_set_palette (Palette^);
  { Map editor needs all 256 tiles because uses the last ones (254 and 255) as
    markers of "map startpoint" and "map endpoint". }
    SetLength (Tileset, 256);
    FOR Ndx := LOW (Tileset) TO HIGH (Tileset) DO
      Tileset[Ndx] := NIL; { To be safe. }
  { Copy the tiles to convert them to the current color depth.  It's necessary
    because dlgTileSelector uses al_stretch_blit to draw the tiles, and it
    doesn't convert color depth of bitmaps. }
    Tmp := DemoData^[BMP_TILES].dat;
    X := 0; Y := 0; Ndx := 1;
    REPEAT
    { Use a sub-bitmap to "extract" the tile from the tileset. }
      TmpTile := al_create_sub_bitmap (Tmp, X, Y, TSIZE, TSIZE);
      Tileset[Ndx] := CloneBitmap (TmpTile, TSIZE, TSIZE);
      al_destroy_bitmap (TmpTile);
    { Next tile. }
      INC (Ndx);
      INC (X, TSIZE);
      IF X >= Tmp^.w THEN
      BEGIN
	X := 0;
	INC (Y, TSIZE);
      END;
    UNTIL (Y >= Tmp^.h) OR (Ndx > 253);
  { Unload data, because we don't need it. }
    al_unload_datafile (DemoData);
  { Set start and end markers. }
    Tileset[254] := CloneBitmap (Data^[BMP_START].dat, TSIZE, TSIZE);
    Tileset[255] := CloneBitmap (Data^[BMP_END].dat, TSIZE, TSIZE);
  { Tell dlgMapEditor to not draw the map, because DrawMap will fail if tries
    to draw a NIL tile!  It will be set to TRUE again when loading a map file
    or creating a new map. }
    CanDrawMap := FALSE;
  END;



(* Shows a dialog to get new map size and creates it. *)
  FUNCTION NewMap: AL_INT; CDECL;
  VAR
    HeightInput, WidthInput: PCHAR;
    DlgMapSize: ARRAY [0..11] OF AL_DIALOG;
    NewWidth, NewHeight, Option: INTEGER;
  BEGIN
  { Warns if map was modified. }
    IF MapModified THEN
      IF NOT AskYesNo ('The map was changed.', 'Create without saving?') THEN
        EXIT;
  { Reserve space for input. }
    WidthInput := StrAlloc (10); HeightInput := StrAlloc (10);
  { Now, copy the last map size as default. }
    StrPCopy (HeightInput, IntToStr (MapHeight));
    StrPCopy (WidthInput, IntToStr (MapWidth));
  { Create a dialog to get new map size. }
    al_set_dialog_item (DlgMapSize, 0, @al_d_shadow_box_proc, 0, 0, 188, 120, CBlack, CWhite, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 1, @al_d_yield_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 2, @al_d_ctext_proc, 0, 4, 174, 8, CBlack, -1, 0, 0, 0, 0, AL_STRptr ('Create new map'), NIL, NIL);
    al_set_dialog_item (DlgMapSize, 3, @al_d_rtext_proc, 16, 28, 88, 16, CBlack, -1, 0, 0, 0, 0, AL_STRptr ('Map width:'), NIL, NIL);
    al_set_dialog_item (DlgMapSize, 4, @al_d_box_proc, 108, 24, 64, 16, CBlack, CWhite, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 5, @al_d_edit_proc, 116, 28, 48, 16, CBlack, CWhite, 0, 0, 5, 0, WidthInput, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 6, @al_d_rtext_proc, 16, 51, 88, 8, CBlack, -1, 0, 0, 0, 0, AL_STRptr ('Map height:'), NIL, NIL);
    al_set_dialog_item (DlgMapSize, 7, @al_d_box_proc, 108, 47, 64, 16, CBlack, CWhite, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 8, @al_d_edit_proc, 116, 51, 48, 16, CBlack, CWhite, 0, 0, 5, 0, HeightInput, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 9, @al_d_button_proc, 16, 72, 156, 16, CBlack, CWhite, scINTRO, AL_D_EXIT, 0, 0, AL_STRptr ('Create &new map'), NIL, NIL);
    al_set_dialog_item (DlgMapSize, 10, @al_d_button_proc, 16, 96, 156, 16, CBlack, CWhite, 0, AL_D_EXIT, 0, 0, AL_STRptr ('&Cancel'), NIL, NIL);
  { End of dialog. }
    al_set_dialog_item (DlgMapSize, 11, NIL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
  { Center the dialog. }
    al_centre_dialog (DlgMapSize);

    REPEAT
      Option := al_do_dialog (DlgMapSize, -1);
      IF Option = 10 THEN
	Option := -1
      ELSE IF Option = 9 THEN
      BEGIN
	NewHeight := StrToIntDef (HeightInput, -1);
	NewWidth := StrToIntDef (WidthInput, -1);
	IF (MIN_SIZE > NewWidth) OR (NewWidth > MAX_SIZE)
	OR (MIN_SIZE > NewHeight) OR (NewHeight > MAX_SIZE)
	THEN
	  ErrorMessage (
	    'Map size should be between',
	    IntToStr (MIN_SIZE)+' and '+IntToStr (MAX_SIZE)
	  )
	ELSE BEGIN
	  CreateMap (NewWidth, NewHeight);
	  FixScrollBarsRange;
	  MapName := '<noname>';
	  ResetMapModified;
	  CanDrawMap := TRUE;
	  Option := -1; { To exit loop. }
	END;
      END;
    UNTIL Option = -1;
    NewMap := AL_D_REDRAW;
  { Release strings. }
    StrDispose (WidthInput); StrDispose (HeightInput);
  END;



(* Shows a dialog to get map name and loads it. *)
  FUNCTION LoadMap: AL_INT; CDECL;
  VAR
    FileName: STRING;
  BEGIN
  { Warns if map was modified. }
    IF MapModified THEN
      IF NOT AskYesNo ('The map was changed.', 'Load without saving?') THEN
        EXIT;
  { The file selector. }
    IF MapName <> '<noname>' THEN
      FileName := MapName;
    IF al_file_select_ex ('Select map file', FileName, 'MAP;/-h', 512, 320, 240) THEN
    BEGIN
      IF NOT Tilemap.LoadMap (FileName) THEN
	ErrorMessage ('Can''t load map from file', FileName)
      ELSE
      BEGIN
	FixScrollBarsRange;
	MapName := FileName;
	ResetMapModified;
	CanDrawMap := TRUE;
      { To show start and end points. }
	Map[StartX, StartY] := T_START;
	Map[EndX, EndY] := T_END;
      END;
    END;
    LoadMap := AL_D_REDRAW;
  END;



(* Shows a dialog to get map name and loads it. *)
  FUNCTION SaveMap: AL_INT; CDECL;
  VAR
    FileName: STRING;
  BEGIN
    IF MapName <> '<noname>' THEN
      FileName := MapName;
    IF al_file_select_ex ('Save map in file', FileName, 'MAP;/-h', 512, 320, 240) THEN
    BEGIN
      IF NOT Tilemap.SaveMap (FileName) THEN
	ErrorMessage ('Can''t Save map in file', FileName)
      ELSE
      BEGIN
	al_alert ('', 'Map saved in file', FileName, 'Read', '', scINTRO, 0);
	MapName := FileName;
	ResetMapModified;
      END;
    END;
    SaveMap := AL_D_O_K;
  END;



(* Callback for quitting from dialog. *)
  FUNCTION Quitting: AL_INT; CDECL;
  BEGIN
    Quitting := AL_D_CLOSE;
  END;



(* Initializes the program.

   Returns TRUE on success, FALSE on failure.
 *)
  FUNCTION InitProgram: BOOLEAN;

  (* Helper function to load the editor data. *)
    FUNCTION LoadData: BOOLEAN; INLINE;
    VAR
      buf: STRING;
    BEGIN
    { Doesn't let Allegro twist colors. }
      al_set_color_conversion (AL_COLORCONV_NONE);
    { Load the datafile into memory. }
      buf :=  'mapedit.dat';
      Data := al_load_datafile (buf);
    { Check if it was loaded. }
      LoadData := Data <> NIL;
    END;

  (* Helper function to initialize the graphics mode. *)
    FUNCTION InitGraphics: BOOLEAN; INLINE;
    BEGIN
      InitGraphics := FALSE;
    { Sets the graphic mode.  First, tries a windowed mode.  If no one is
      avaiable, tries again with an auto-detected mode then a safe mode.
      If no graphic mode is available, shows the error. }
      al_set_color_depth (al_desktop_color_depth);
      IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 640, 480, 0, 0) THEN
	IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT, 640, 480, 0, 0) THEN
	  IF NOT al_set_gfx_mode (AL_GFX_SAFE, 640, 480, 0, 0) THEN
	  BEGIN
	    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0); { Be sure it's closed. }
	  { Shows an error message.
	    Can't use 'ErrorMessage' because the graphic mode isn't up. }
	    al_message (al_error);
	    EXIT;
	  END;
    { Calculate few common colors. }
      CWhite := al_makecol (255, 255, 255);
      CBlack := al_makecol (  0,   0,   0);
      CRed   := al_makecol (255,   0,   0);
      CGreen := al_makecol (  0, 255,   0);
      CBlue  := al_makecol (  0,   0, 255);
      CButton:= al_makecol (153, 153, 153);

      InitGraphics := TRUE;
    END;

  (* Helper procedure to create and set the edition buttons. *)
    PROCEDURE CreateEditionButtons; INLINE;
    BEGIN
    { The 'delete' button. }
      EditButtons[0] := CloneBitmap (Data^[BMP_DELETE].dat, TSIZE, TSIZE);
    { The 'start position' button. }
      EditButtons[1] := CloneBitmap (Data^[BMP_START].dat, TSIZE, TSIZE);
    { The 'end of map' button. }
      EditButtons[2] := CloneBitmap (Data^[BMP_END].dat, TSIZE, TSIZE);
    END;

  (* Helper procedure to define the menus. *)
    PROCEDURE SetMainMenu; INLINE;
    BEGIN
    { Sub-menus. }
      al_set_menu_item (ProgMenu, 0, '&Help    Ctrl+H',  @Help,     NIL,             0, NIL);
      al_set_menu_item (ProgMenu, 1, '&About',           @About,    NIL,             0, NIL);
      al_set_menu_item (ProgMenu, 2, '--------------',   NIL,       NIL, AL_D_DISABLED, NIL);
      al_set_menu_item (ProgMenu, 3, '&Quit       Esc',  @Quitting, NIL,             0, NIL);

      al_set_menu_item (MapMenu, 0, '&New    Ctrl+N', @NewMap, NIL,            0, NIL);
      al_set_menu_item (MapMenu, 1, '&Load   Ctrl+L', @LoadMap, NIL,           0, NIL);
      al_set_menu_item (MapMenu, 2, '-------------',  NIL,    NIL, AL_D_DISABLED, NIL);
      al_set_menu_item (MapMenu, 3, '&Save   Ctrl+S', @SaveMap, NIL,         0, NIL);

      al_set_menu_item (CfgBgMenu, 0, '&Default', @mnuRestoreBackground,    NIL,     AL_D_SELECTED, NIL);
      al_set_menu_item (CfgBgMenu, 1, '&Solid color', @mnuSelectSolidBackground,    NIL, 0, NIL);
      al_set_menu_item (CfgBgMenu, 2, '&Bitmap', @mnuSelectBitmapBackground,    NIL,      0, NIL);

      al_set_menu_item (ConfigMenu, 0, '&Fullscreen', @FullscreenCheck,    NIL,             0, NIL);
      al_set_menu_item (ConfigMenu, 1, '&Background',  NIL,    @CfgBgMenu,             0, NIL);
   { Put all together. }
      al_set_menu_item (MainMenu,  0, '&Program', NIL,       @ProgMenu,      0, NIL);
      al_set_menu_item (MainMenu,  1, '&Map',     NIL,       @MapMenu,       0, NIL);
      al_set_menu_item (MainMenu,  2, '&Config',  NIL,       @ConfigMenu,    0, NIL);
    END;

  (* Helper funciton to setup the GUI and define the main dialog. *)
    FUNCTION InitGUI: BOOLEAN;
    BEGIN
    { Initialize mouse. }
      IF al_install_mouse < 1 THEN
      BEGIN
	ErrorMessage ('Unable to set up a mouse.', 'You need a mouse or similar to use this editor!');
	InitGUI := FALSE;
	EXIT;
      END;
    { Try to use hardware cursor. }
      al_enable_hardware_cursor;
      al_select_mouse_cursor (AL_MOUSE_CURSOR_ARROW);
      al_show_mouse (al_screen);
      IF al_gfx_capabilities AND AL_GFX_HW_CURSOR = 0 THEN
      BEGIN
      { If no hardware cursor available, use Allegro's one. }
	al_disable_hardware_cursor;
	al_show_mouse (al_screen);
      END;
    { Creates the GUI. }
      SetMainMenu;

      al_set_dialog_item (MainDialog, 0, @al_d_yield_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
      al_set_dialog_item (MainDialog, 1, @al_d_box_proc, 0, 0, AL_SCREEN_W, AL_SCREEN_H, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
      al_set_dialog_item (MainDialog, 2, @al_d_menu_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, @MainMenu[0], NIL, NIL);
      al_set_dialog_item (MainDialog, 3, @dlgBrushSelProc, 1, 15, BTN_SIZE, BTN_SIZE, 0, 0, 0, AL_D_SELECTED, 0, BMP_1x1, Data^[BMP_1x1].dat, NIL, NIL);
      al_set_dialog_item (MainDialog, 4, @dlgBrushSelProc, 1+BTN_SIZE+2, 15, BTN_SIZE, BTN_SIZE, 0, 0, 0, 0, 0, BMP_1x2, Data^[BMP_1x2].dat, NIL, NIL);
      al_set_dialog_item (MainDialog, 5, @dlgBrushSelProc, 1+(BTN_SIZE+2) * 2, 15, BTN_SIZE, BTN_SIZE, 0, 0, 0, 0, 0, BMP_2x1, Data^[BMP_2x1].dat, NIL, NIL);
      al_set_dialog_item (MainDialog, 6, @dlgBrushSelProc, 1+(BTN_SIZE+2) * 3, 15, BTN_SIZE, BTN_SIZE, 0, 0, 0, 0, 0, BMP_2x2, Data^[BMP_2x2].dat, NIL, NIL);
      al_set_dialog_item (MainDialog, 7, @dlgBrushSelProc, 1+(BTN_SIZE+2) * 4, 15, BTN_SIZE, BTN_SIZE, 0, 0, 0, 0, 0, BMP_O_1x2, Data^[BMP_O_1X2].dat, NIL, NIL);
      al_set_dialog_item (MainDialog, 8, @dlgBrushSelProc, 1+(BTN_SIZE+2) * 5, 15, BTN_SIZE, BTN_SIZE, 0, 0, 0, 0, 0, BMP_O_2x1, Data^[BMP_O_2x1].dat, NIL, NIL);
      al_set_dialog_item (MainDialog, 9, @dlgBrushSelProc, 1+(BTN_SIZE+2) * 6, 15, BTN_SIZE, BTN_SIZE, 0, 0, 0, 0, 0, BMP_O_2x2, Data^[BMP_O_2x2].dat, NIL, NIL);
      NdxMapedit := 10;
      al_set_dialog_item (MainDialog, 10, @dlgMapEditorProc, 0, 47, AL_SCREEN_W - BTN_SIZE, AL_SCREEN_H - BTN_SIZE * 2 - 48, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
      NdxScrollBarH := 11;
      al_set_dialog_item (MainDialog, 11, @al_d_slider_proc, AL_SCREEN_W - BTN_SIZE, 47, BTN_SIZE - 1, AL_SCREEN_H - BTN_SIZE * 2 - 48, 0, 0, 0, 0, 1, 0, NIL, @ScrollBarHandler, NIL);
      NdxScrollBarW := 12;
      al_set_dialog_item (MainDialog, 12, @al_d_slider_proc, 1, AL_SCREEN_H - BTN_SIZE * 2 - 1, AL_SCREEN_W - BTN_SIZE - 2, BTN_SIZE - 1, 0, 0, 0, 0, 1, 0, NIL, @ScrollBarHandler, NIL);
      al_set_dialog_item (MainDialog, 13, @dlgTileSelectorProc, 0, AL_SCREEN_H - BTN_SIZE - 2, AL_SCREEN_W, BTN_SIZE + 2, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
    { Key shortcuts. }
      al_set_dialog_item (MainDialog, 14, @al_d_keyboard_proc, 0, 0, 0, 0, 0, 0, 0, 0, AL_KEY_F1, 0, @Help, NIL, NIL);
      al_set_dialog_item (MainDialog, 15, @al_d_keyboard_proc, 0, 0, 0, 0, 0, 0, 14, 0, 0, 0, @NewMap, NIL, NIL);
      al_set_dialog_item (MainDialog, 16, @al_d_keyboard_proc, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, @LoadMap, NIL, NIL);
      al_set_dialog_item (MainDialog, 17, @al_d_keyboard_proc, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, @SaveMap, NIL, NIL);
    { End of dialog. }
      al_set_dialog_item (MainDialog, 18, NIL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
    { Configures GUI. }
      al_gui_fg_color := CBlack;
      al_gui_mg_color := al_makecol (51, 51, 51);
      al_gui_bg_color := CWhite;
      al_set_dialog_color (MainDialog, CBlack, CButton);
      al_gui_mouse_focus := 0;

      InitGUI := TRUE;
    END;

  BEGIN
    InitProgram := FALSE;
  { Initializes Allegro. }
    IF NOT al_init THEN
    BEGIN
      WriteLn ('Can''t initialize Allegro!');
      EXIT;
    END;
    al_install_keyboard;
    al_install_timer;
  { Loads the editor data. }
    IF NOT LoadData THEN
    BEGIN
      al_message ('Can''t load the game data.');
      EXIT;
    END;
  { Init graphics. }
    IF NOT InitGraphics THEN
    { InitGraphics shows its own messages. }
      EXIT;
  { Set the default tileset. }
    SetTileset ('');
    MapName := '<noname>';
    ResetMapModified;
  { Set the edition buttons. }
    CreateEditionButtons;
  { Set default size, to show if select NewMap first. }
    MapWidth := 100; MapHeight := 100;
  { Set up the GUI system. }
    IF NOT InitGUI THEN
      EXIT;

  { If we're here, we're done. }
    InitProgram := TRUE;
  END;



(* End of the program.  Releases all resources used. *)
  PROCEDURE EndProgram;
  VAR
    Ndx: INTEGER;
  BEGIN
    IF Data <> NIL THEN
    BEGIN
      al_unload_datafile (Data);
      Data := NIL;
    END;
    UnsetTileset;
    FOR Ndx := LOW (EditButtons) TO HIGH (EditButtons) DO
      DestroyBmp (EditButtons[Ndx]);
  { The other edition button bitmaps are in the Data, so we don't need to
    destroy them here. }
  END;

BEGIN
{ Initializes the program. }
  IF NOT InitProgram THEN EXIT;

  REPEAT
    al_do_dialog (MainDialog, -1);
  UNTIL NOT MapModified
  OR AskYesNo ('The map was changed.', 'Exit anyway?');

{ End of the program. }
  EndProgram;
END.
