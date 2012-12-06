PROGRAM newd;
(* This is the map editor for the Allegro.pas demo game.

  It's designed in a way that should be ease to expand and upgrade, so you can
  use it in your projects.

  By Ñuño Martínez.
 *)

  USES
    allegro,
    albase,   { We need allegro's types. }
    algui,    { To use the Allegro's GUI. }
    alfile,   { To access to Allegro's data files. }
    sysutils, { For string manipulation. }
    tilemap;  { Tilemap management. }

  CONST
  (* Window captions. *)
    CAPTION = 'Allegro.pas Demo Game Map Editor';
    CAPTION_MODIFIED = CAPTION + ' [Modified]';
  (* Size of tile buttons. *)
    BTN_SIZE = 32;
  (* Minimun and maximun size of map. *)
    MIN_SIZE = 10; MAX_SIZE = 500;
  (* Key shortcuts to use in dialog definition. *)
    scINTRO = 13;

{ Indexes for data in the datafile. }
{$I mapedit.inc}

  VAR
  (* Some data used by the editor. *)
    Data: AL_DATAFILEptr;
  (* Some colors. *)
    CWhite, CBlack, CRed, CBlue, CButton: LONGINT;
  (* The special edition buttons:  Delete, Start and End. *)
    EditButtons: ARRAY [0..2] OF AL_BITMAPptr;
  (* Main menu description. *)
    ProgMenu: ARRAY [0..4] OF AL_MENU;
    MapMenu: ARRAY [0..5] OF AL_MENU;
    ConfigMenu: ARRAY [0..1] OF AL_MENU;
    MainMenu: ARRAY [0..3] OF AL_MENU;
  (* Main dialog.  This is the editor itself. *)
    MainDialog: ARRAY [0..10] OF AL_DIALOG;
  (* Index of map scroll bar controls. *)
    NdxScrollBarW, NdxScrollBarH: INTEGER;
  (* Flag to know if map was modified. *)
    MapModified,
  (* This flag tells to dlgMapEditor if it can draw the map. *)
    CanDrawMap: BOOLEAN;



(***************************
  Next data is needed to edit maps of the Allegro.pas' Demo Game.
  If you are modifying the editor to use it in other projects then
  you should remove or modify this block of code.
 *)

{ Indexes for tiles in the datafile. }
{$I demo.inc}

(*
  End of data needed to edit maps of Allegro.pas' Demo Game.
 ***************************)



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
    BEGIN
      MapModified := TRUE;
      al_set_window_title (CAPTION_MODIFIED);
    END;
  END;



(* Removes "map modified" flag and window caption. *)
  PROCEDURE ResetMapModified;
  BEGIN
    IF MapModified THEN
    BEGIN
      MapModified := FALSE;
      al_set_window_title (CAPTION);
    END;
  END;



(*****************************************************************************
 * GUI stuff.  That are new GUI controls, etc.
 *)

(* Callback for map scrollbars. *)
  FUNCTION ScrollBarHandler (dp3: AL_VOIDptr; d2: AL_INT): AL_INT; CDECL;
  BEGIN
  { Just redraw everything to do the scroll. }
    ScrollBarHandler := AL_D_REDRAW;
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



(* Creates a custom GUI object to show and edit the map. *)
  FUNCTION dlgMapEditorProc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT; CDECL;

  (* Helper procedure to draw the control. *)
    PROCEDURE DrawControl;
    VAR
      SX, SY: INTEGER;
    BEGIN
    { Draws a pattern to show transparent tiles. }
      al_drawing_mode (AL_DRAW_MODE_COPY_PATTERN, d^.dp2, 0, 0);
      al_rectfill (d^.dp, 0, 0, d^.w, d^.h, 0);
      al_solid_mode;
    { Now, draws the map using scrollbars. }
      FixScroll (d^.dp, MainDialog[NdxScrollBarH].d2, MainDialog[NdxScrollBarW].d2, SY, SX);
    { If map is smaller than editing space, then SX and/or SY became negative,
      so it will draw it wrong.

      Hack note: You don't should do this in your games, as your output bitmap
		 should be same size or bigger than your maps. }
      IF SX < 0 THEN SX := 0;
      IF SY < 0 THEN SY := 0;
      DrawMap  (d^.dp, SY, SX);
    END;

  BEGIN
  { Default return value. }
    dlgMapEditorProc := AL_D_O_K;
  { Process message. }
    CASE msg OF
      AL_MSG_START: { Object initialization. }
	BEGIN
	{ Creates a sub-bitmpap, storing it in AL_DIALOG.dp field.  It will be
	  used to draw the map. }
	  d^.dp := al_create_sub_bitmap (
	    al_gui_get_screen,
	  { Keep a border. }
	    d^.x + 1, d^.y + 1, d^.w - 2, d^.h - 2
	  );
	{ Bitmap used as background to show transparency. }
	  d^.dp2 := al_create_bitmap (TSIZE * 4, TSIZE * 4);
	  al_clear_to_color (d^.dp2, d^.bg);
	  al_rectfill (d^.dp2, TSIZE * 2, 0, TSIZE * 4 - 1, TSIZE * 2 - 1, al_gui_mg_color);
	  al_rectfill (d^.dp2, 0, TSIZE * 2, TSIZE * 2 - 1, TSIZE * 4 - 1, al_gui_mg_color);
	END;
      AL_MSG_END: { Object destruction. }
	BEGIN
	{ Releases resources. }
	  DestroyBmp (d^.dp);
	  DestroyBmp (d^.dp2);
	END;
      AL_MSG_DRAW: { To show the object. }
	IF CanDrawMap THEN
	  DrawControl
	ELSE
	  DrawNilObject (d^.x , d^.y, d^.x + d^.w - 1, d^.y + d^.h - 1, d^.bg);
    END;
  END;



(* Creates a custom GUI object that shows the available tiles and allows to
   select them.  Note that it uses the external Tileset list, so you can't use
   more than one tileset.

   d1 stores the index of the selected tile.
   d2 stores the first tile to draw (scroll).
 *)
  FUNCTION dlgTileSelectorProc (msg: AL_INT; d: AL_DIALOGptr; c: AL_INT): AL_INT; CDECL;

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
	IF -d^.d1 = Ndx THEN
	  DrawDottedRect (
	    X, d^.y + 1, X + BTN_SIZE - 1, d^.y + BTN_SIZE
	  );
      END;
    { Left and right arrows. }
      X := BTN_SIZE * 3; { Space for edition buttons. }
      al_vline (al_gui_get_screen, X, d^.y, d^.y + d^.w, CBlack);
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
    { Draw the tiles.  Draw them from right to left.
      "2" is the room for arrows. }
      FOR Ndx := (d^.w DIV BTN_SIZE) - Length (EditButtons) - 2 DOWNTO 0 DO
      BEGIN
      { Calculates X coordinate. }
	X := FB_X + d^.x + (BTN_SIZE * Ndx);
      { Be sure we don't try to draw a non existent tile. }
	IF (d^.d2 + Ndx < Length (Tileset) - 2)
	AND (Tileset[d^.d2 + Ndx] <> NIL)
	THEN BEGIN
	  al_stretch_blit (
	    Tileset[d^.d2 + Ndx], al_gui_get_screen,
	    0, 0, Tileset[d^.d2 + Ndx]^.w, Tileset[d^.d2 + Ndx]^.h,
	    X, d^.y + 1, BTN_SIZE, BTN_SIZE
	  );
	{ Selected tile. }
	  IF d^.d1 = d^.d2 + Ndx THEN
	    DrawDottedRect (
	      X, d^.y + 1, X + BTN_SIZE - 1, d^.y + BTN_SIZE
	    );
	END
	ELSE
	{ If tile doesn't exist, draw a "X". }
	  DrawNilObject (X , d^.y + 1, X + BTN_SIZE, d^.y + BTN_SIZE, d^.bg);
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
	{ Uses d2 to store the index of the first tile to draw. }
	  d^.d2 := 1;
	END;
      AL_MSG_DRAW: { Draws the object. }
	DrawControl;
    END;
  END;



(*****************************************************************************
 * Program stuff:  loading, saveing, quitting, etc.
 *)

(* Shows help. *)
  FUNCTION Help: AL_INT; CDECL;
  BEGIN
    al_alert ('* mapedit *', CAPTION, 'Aquí iría la ayuda o dos', 'Ok', '', 0, 0);
    Help := AL_D_O_K;
  END;



(* Our about box. *)
  FUNCTION About: AL_INT; CDECL;
  BEGIN
    al_alert ('* mapedit *', CAPTION, 'by Guillermo Martínez', 'Ok', '', 0, 0);
    About := AL_D_O_K;
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
  VAR
    Palette: AL_PALETTEptr;
    DemoData: AL_DATAFILEptr;
    buf: STRING;
    Ndx: INTEGER;
  BEGIN
  { Hack note: May be you want to call UnsetTileset to clean up the Tileset
		first.  I don't do it because Demo game uses the same
		Tileset for all maps. }
  { Load the demo datafile into memory. }
    buf :=  ExtractFilePath (PARAMSTR (0)) + 'demo.dat';
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
    FOR Ndx := BMP_COIN TO BMP_BLK3 DO
      Tileset[Ndx + 1] := CloneBitmap (DemoData^[Ndx].dat,TSIZE, TSIZE);
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
    DlgMapSize: ARRAY [0..12] OF AL_DIALOG;
    HeightInput, WidthInput: STRING[10];
    NewWidth, NewHeight, Option: INTEGER;
  BEGIN
  { Warns if map was modified. }
    IF MapModified THEN
      IF NOT AskYesNo ('The map was changed.', 'Create without saving?') THEN
        EXIT;
    HeightInput := IntToStr (MapHeight)+''#0;
    WidthInput  := IntToStr (MapWidth)+''#0;
  { Create a dialog to get new map size. }
    al_set_dialog_item (DlgMapSize, 0, @al_d_shadow_box_proc, 0, 0, 174, 120, CBlack, CButton, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 1, @al_d_yield_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 2, @al_d_box_proc, 0, 0, 173, 16, CBlack, CBlue, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 3, @al_d_ctext_proc, 0, 4, 174, 8, CWhite, -1, 0, 0, 0, 0, AL_STRptr ('Create new map'), NIL, NIL);
    al_set_dialog_item (DlgMapSize, 4, @al_d_rtext_proc, 16, 28, 88, 8, CBlack, -1, 0, 0, 0, 0, AL_STRptr ('Map height:'), NIL, NIL);
    al_set_dialog_item (DlgMapSize, 5, @al_d_box_proc, 108, 24, 50, 16, CBlack, CWhite, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 6, @al_d_edit_proc, 116, 28, 34, 16, CBlack, CWhite, 0, 0, 3, 0, @HeightInput[1], NIL, NIL);
    al_set_dialog_item (DlgMapSize, 7, @al_d_rtext_proc, 16, 51, 88, 16, CBlack, -1, 0, 0, 0, 0, AL_STRptr ('Map width:'), NIL, NIL);
    al_set_dialog_item (DlgMapSize, 8, @al_d_box_proc, 108, 47, 50, 16, CBlack, CWhite, 0, 0, 0, 0, NIL, NIL, NIL);
    al_set_dialog_item (DlgMapSize, 9, @al_d_edit_proc, 116, 51, 34, 16, CBlack, CWhite, 0, 0, 3, 0, @WidthInput[1], NIL, NIL);
    al_set_dialog_item (DlgMapSize, 10, @al_d_button_proc, 16, 72, 142, 16, CBlack, CButton, scINTRO, AL_D_EXIT, 0, 0, AL_STRptr ('Create &new map'), NIL, NIL);
    al_set_dialog_item (DlgMapSize, 11, @al_d_button_proc, 16, 96, 142, 16, CBlack, CButton, 0, AL_D_EXIT, 0, 0, AL_STRptr ('&Cancel'), NIL, NIL);
  { End of dialog. }
    al_set_dialog_item (DlgMapSize, 12, NIL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
  { Centers the dialog. }
    al_centre_dialog (@DlgMapSize);
    REPEAT
      Option := al_do_dialog (@DlgMapSize[0], -1);
      IF Option = 11 THEN
	Option := -1
      ELSE IF Option = 10 THEN
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
	  CreateMap (NewHeight, NewWidth);
	  CanDrawMap := TRUE;
	  Option := -1; { To exit loop. }
	  ResetMapModified;
	END;
      END;
    UNTIL Option = -1;
    NewMap := AL_D_REDRAW;
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
      buf :=  ExtractFilePath (PARAMSTR (0)) + 'mapedit.dat';
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
      al_set_window_title (CAPTION);
    { Calculate few common colors. }
      CWhite := al_makecol (255, 255, 255);
      CBlack := al_makecol (  0,   0,   0);
      CRed   := al_makecol (255,   0,   0);
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

      al_set_menu_item (MapMenu, 0, '&New    Ctrl+N', @NewMap,NIL,             0, NIL);
      al_set_menu_item (MapMenu, 1, '&Load        L', NIL,    NIL,             0, NIL);
      al_set_menu_item (MapMenu, 2, '-------------',  NIL,    NIL, AL_D_DISABLED, NIL);
      al_set_menu_item (MapMenu, 3, '&Save        S', NIL,    NIL,             0, NIL);
      al_set_menu_item (MapMenu, 4, 'Save &as...',    NIL,    NIL,        0, NIL);

      al_set_menu_item (ConfigMenu, 0, '&Fullscreen', NIL,    NIL,             0, NIL);
   { Put all together. }
      al_set_menu_item (MainMenu,  0, '&Program',      NIL,       @ProgMenu,      0, NIL);
      al_set_menu_item (MainMenu,  1, '&Map',          NIL,       @MapMenu,       0, NIL);
      al_set_menu_item (MainMenu,  2, '&Config',       NIL,       @ConfigMenu,    0, NIL);
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
      al_set_dialog_item (MainDialog, 1, @al_d_box_proc, 0, 0, AL_SCREEN_W, 50, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
      al_set_dialog_item (MainDialog, 2, @al_d_menu_proc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, @MainMenu[0], NIL, NIL);
      al_set_dialog_item (MainDialog, 3, @dlgMapEditorProc, 0, 46, AL_SCREEN_W - BTN_SIZE, AL_SCREEN_H - BTN_SIZE * 2 - 47, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
      NdxScrollBarH := 4;
      al_set_dialog_item (MainDialog, 4, @al_d_slider_proc, AL_SCREEN_W - BTN_SIZE, 46, BTN_SIZE - 1, AL_SCREEN_H - BTN_SIZE * 2 - 48, 0, 0, 0, 0, 1024, 1024, NIL, @ScrollBarHandler, NIL);
      NdxScrollBarW := 5;
      al_set_dialog_item (MainDialog, 5, @al_d_slider_proc, 1, AL_SCREEN_H - BTN_SIZE * 2 - 1, AL_SCREEN_W - BTN_SIZE - 2, BTN_SIZE - 1, 0, 0, 0, 0, 1024, 0, NIL, @ScrollBarHandler, NIL);
      al_set_dialog_item (MainDialog, 6, @dlgTileSelectorProc, 0, AL_SCREEN_H - BTN_SIZE - 2, AL_SCREEN_W, BTN_SIZE + 2, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
      al_set_dialog_item (MainDialog, 7, @al_d_box_proc, AL_SCREEN_W - BTN_SIZE - 1, AL_SCREEN_H - BTN_SIZE * 2 - 2, BTN_SIZE + 1, BTN_SIZE + 1, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
    { Key shortcuts. }
      al_set_dialog_item (MainDialog, 8, @al_d_keyboard_proc, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, @Help, NIL, NIL);
      al_set_dialog_item (MainDialog, 9, @al_d_keyboard_proc, 0, 0, 0, 0, 0, 0, 0, 0, 14, 0, @NewMap, NIL, NIL);
    { End of dialog. }
      al_set_dialog_item (MainDialog, 10, NIL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NIL, NIL, NIL);
    { Configures GUI. }
      al_gui_fg_color := CBlack;
      al_gui_mg_color := al_makecol (51, 51, 51);
      al_gui_bg_color := CWhite;
      al_set_dialog_color (@MainDialog[0], CBlack, CButton);

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
  { Sets the default tileset. }
    SetTileset ('');
  { Sets the edition buttons. }
    CreateEditionButtons;
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

  al_do_dialog (@MainDialog[0], -1);

{ End of the program. }
  EndProgram;
END.
