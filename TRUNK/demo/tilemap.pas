UNIT Tilemap;
(* Program: Demo game for the Allegro.pas library.
 * File: tilemap.pas
 * Description: Functions and procedures to load and render tilemaps.
 * Author: Ñuño Martínez <niunio at users.sourceforge.net>
 *	   Translated from a game by Ken Silverman <http://www.advsys.net/ken/>
 *)

INTERFACE

  USES
    allegro; { Bitmap manipulation. }


  VAR
  (* The tilemap.

     To access to the <X, Y> tile do "Map[Y][X]" or "Map[Y, X]".
     @seealso(LoadMap)
   *)
    Map: ARRAY OF ARRAY OF BYTE;
    MapHeight,		{ Height of the tilemap. }
    MapWidth: INTEGER;	{ Length of the tilemap. }
  (* Starting and ending coordinates. *)
    StartX, StartY, EndX, EndY: INTEGER;
  (* The tileset is the list of bitmaps used to draw the map.

    You must set them before to draw the tilemap.  DrawMap assumes that
    everything in this array is correct, so be sure to set all tiles or you may
    get SIGSEGV runtime errors.  Also, tile number 0 (zero) is never drawn so
    you don't need to set it.  Tiles 254 and 255 are used as markers for start
    and end points so you don't need to set them except you want to mark them
    in a special way (i. e. as map editor does.)
  *)
    Tileset: ARRAY OF AL_BITMAPptr;

  CONST
  (* The tile values. *)
    T_VOID  = 0;
    T_START = 254;
    T_END   = 255;
  (* Next one should be removed from this module. *)
    T_COIN  = 1;
    T_BLK1  = 2;
    T_BLK2  = 3;
    T_BLK3  = 4;

    TSIZE = 16; { Size of the tiles in pixels. }
    SMALL_TSIZE =  4;	{ Size of 'small board tiles'. }



(* Creates an empty map.  You don't need to do it except you're
   createing the map "on the fly".
   @seealso(LoadMap) *)
  PROCEDURE CreateMap (CONST Height, Width: INTEGER);

(* Gets the map information from a file.  The name of the map is
   "boardN.brd".  Returns TRUE on success or FALSE on failure. *)
  FUNCTION LoadMap (N: INTEGER): BOOLEAN;

(* Be sure that the scroll isn't out of the edges of the board.  Should be
   used before draw anything. *)
  PROCEDURE FixScroll (CONST Bmp: AL_BITMAPptr; CONST Iy, Ix: INTEGER;
			VAR Oy, Ox: INTEGER);

(* Draws the board in the given bitmap at the given scroll coordinates. *)
  PROCEDURE DrawMap (Bmp: AL_BITMAPptr; ScrollY, ScrollX: INTEGER);



IMPLEMENTATION

USES
  sysutils; { For string manipulation. }



(* Creates an empty map.  You don't need to do it except you're
   createing the map "on the fly".
   @seealso(LoadMap) *)
  PROCEDURE CreateMap (CONST Height, Width: INTEGER);
  VAR
    Y, X: INTEGER;
  BEGIN
    SetLength (Map, Height);
    FOR Y := LOW (Map) TO HIGH (Map) DO
    BEGIN
      SetLength (Map[Y], Width);
      FOR X := LOW (Map[Y]) TO HIGH (Map[Y]) DO
	Map[Y, X] := T_VOID;
    END;
  END;



(* Gets the map information from a file.  The name of the map is
   "boardN.brd".  Returns TRUE on success or FALSE on failure. *)
  FUNCTION LoadMap (N: INTEGER): BOOLEAN;

  (* Translates the value loaded from file to the actual tile value. *)
    FUNCTION TranslateTile (Tile: CHAR): BYTE;
    BEGIN
      TranslateTile := BYTE (ORD (Tile) - ORD ('A'));
      CASE TranslateTile OF
      3: { D }
	TranslateTile := T_START;
      4: { E }
	TranslateTile := T_END;
      1: { B }
	TranslateTile := T_COIN;
      11:{ L }
	TranslateTile := T_BLK1;
      12:{ M }
	TranslateTile := T_BLK2;
      13:{ N }
	TranslateTile := T_BLK3;
      END;
    END;

  VAR
    Path, FileName: STRING; F: TEXT; { File definition. }
    Column: STRING;	     { To read the file. }
    x, y, ry: INTEGER;
  BEGIN
    LoadMap := FALSE;
  { Builds the file name.
    First, gets the path where the execubable is. }
    Path :=  ExtractFilePath (PARAMSTR (0));
  { Creates the file name. }
    Filename :=  'board' + IntToStr (N) + '.brd';
  { Builds the final name with path. }
    FileName := Path + Filename;
  { Opens the file. }
    {$I-} { To save file errors in IOResult. }
    Assign (F, FileName); Reset (F);
    IF IOResult <> 0 THEN
      EXIT;
  { First line is the length of the board. }
    ReadLN (F, MapWidth);
    MapHeight := 15; { TODO: Current format don't allows different. }
    CreateMap (MapHeight, MapWidth);
  { Marks the starting and ending points: they aren't defined. }
    StartX := -1; StartY := -1;
    EndX := -1; EndY := -1;
  { Reads the columns. }
    FOR x := 1 TO MapWidth DO
    BEGIN
      ReadLN (F, Column);
    { Parses the columns. }
      FOR y := 1 TO MapHeight DO
      BEGIN
      { Needed because the y coordinate is inverted at the file. }
	ry := (MapHeight + 1) - y;
	Map [ry - 1, x - 1] := TranslateTile (Column [y]);
      { Look for the starting point. }
	IF Map[ry - 1, x - 1] = T_START THEN
	BEGIN
	  IF StartX = -1 THEN
	  BEGIN
	  { Stores the coordinates. }
	    StartX := x - 1;
	    StartY := ry - 1;
	  { Deletes the starting point tile. }
	    Map[StartY, StartX] := T_VOID;
	  END;
	END
      { Looks for the ending point. }
	ELSE IF Map[ry - 1, x - 1] = T_END THEN
	{ Stores the right-most exit point. }
	  IF EndX <= x THEN
	  BEGIN
	  { Stores the coordinates. }
	    EndX := x - 1;
	    EndY := ry - 1;
	  { Deletes the ending point tile. }
	    Map[EndY, EndX] := T_VOID;
	  END;
	END;
      END;
    {$I+} { End storing file errors in IOResult. }
  { Closes the file. }
    Close (F);
  { Checks errors. }
    IF IOResult <> 0 THEN
      EXIT
    ELSE
      LoadMap := TRUE;
  END;



(* Fixes the scroll values so it isn't out of the edges of the board.  Should be
   used before to draw anything. *)
  PROCEDURE FixScroll (CONST Bmp: AL_BITMAPptr; CONST Iy, Ix: INTEGER;
			VAR Oy, Ox: INTEGER);
  BEGIN
    IF Ix < 0 THEN
      Ox := 0
    ELSE IF Ix > (MapWidth * TSIZE) - Bmp^.w THEN
      Ox := (MapWidth * TSIZE) - Bmp^.w
    ELSE
      Ox := Ix;

    IF Iy < 0 THEN
      Oy := 0
    ELSE IF Iy > (MapHeight * TSIZE) - Bmp^.h THEN
      Oy := (MapHeight * TSIZE) - Bmp^.h
    ELSE
      Oy := Iy;
  END;



(* Draws the board in the given bitmap at the given scroll coordinates.
 * I'm sure it can be optimized a lot (and it should be) but I try to keep
 * it simple and understandable. *)
  PROCEDURE DrawMap (Bmp: AL_BITMAPptr; ScrollY, ScrollX: INTEGER);
  VAR
    NumTilesW, NumTilesH: INTEGER; { Number of tiles to draw. }
    FirstTileX, FirstTileY: INTEGER; { First tile to draw. }
    OffsetX, OffsetY: INTEGER; { Relative coordinates of first tile. }
    PosX, PosY: INTEGER; { Where to draw the tile. }
    X, Y: INTEGER; { Tile to draw. }
  BEGIN
  { Calculates how many tiles can be drawn in the bitmap. }
    NumTilesW  := (Bmp^.w DIV TSIZE) + 1; { Adds one for the edges. }
    NumTilesH  := (Bmp^.h DIV TSIZE) + 1;
    IF NumTilesH > MapHeight THEN NumTilesH := MapHeight;
  { Calculates the first tile to be drawn. }
    FirstTileX := (ScrollX DIV TSIZE);
    FirstTileY := (ScrollY DIV TSIZE);
  { Calculates the offset of that first tile.  They are the coordinates where the
    first tile will be drawn.  The tile offset is the pixel of the tile that is in
    the upper left pixel of the output bitmap. }
    OffsetX := -(ScrollX MOD TSIZE);
    OffsetY := -(ScrollY MOD TSIZE);
  { Draws. }
    PosY := OffsetY;
    FOR Y := FirstTileY TO FirstTileY + NumTilesH DO
    BEGIN
      PosX := OffsetX;
      FOR X := FirstTileX TO FirstTileX + NumTilesW DO
      BEGIN
	IF (X < MapWidth) AND (Y < MapHeight)
	AND (Map[Y, X] > T_VOID) THEN
	  al_blit (Tileset[Map[Y, X]], Bmp, 0, 0, PosX, PosY, TSIZE, TSIZE);
      { Next tile position. }
	INC (PosX, TSIZE);
      END;
    { Next tile position. }
      INC (PosY, TSIZE);
    END;
  END;

END.
