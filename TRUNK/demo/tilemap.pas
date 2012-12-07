UNIT Tilemap;
(* Program: Demo game for the Allegro.pas library.
 * File: tilemap.pas
 * Description: Functions and procedures to load and render tilemaps.
 * Author: �u�o Mart�nez <niunio at users.sourceforge.net>
 *	   Translated from a game by Ken Silverman <http://www.advsys.net/ken/>
 *)

INTERFACE

  USES
    allegro, { Bitmap manipulation. }
    alfile;  { File access. }


  VAR
  (* The tilemap.

     To access to the <X, Y> tile do "Map[X][Y]" or "Map[X, Y]".
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
  PROCEDURE CreateMap (CONST Width, Height: INTEGER);

(* Gets the map information from a file.
  @returns(@true on success or @false on failure.) *)
  FUNCTION LoadMap (FileName: STRING): BOOLEAN;

(* Like @link(LoadMap), but reads from a packfile. *)
  FUNCTION LoadMapPf (PackFile: AL_PACKFILEptr): BOOLEAN;

(* Be sure that the scroll isn't out of the edges of the board.  Should be
   used before draw anything. *)
  PROCEDURE FixScroll (CONST Bmp: AL_BITMAPptr; CONST Ix, Iy: INTEGER;
			VAR Ox, Oy: INTEGER);

(* Draws the board in the given bitmap at the given scroll coordinates. *)
  PROCEDURE DrawMap (Bmp: AL_BITMAPptr; ScrollX, ScrollY: INTEGER);

(* Saves the map information to a file.  It's used by the map editor.
  @seealso(LoadMap) @seealso(SaveMapPf)
 *)
  FUNCTION SaveMap (FileName: STRING): BOOLEAN;

(* Like @link(SaveMap), but reads from a packfile. *)
  FUNCTION SaveMapPf (PackFile: AL_PACKFILEptr): BOOLEAN;

IMPLEMENTATION

USES
  sysutils; { For string manipulation. }



(* Creates an empty map.  You don't need to do it except you're
   createing the map "on the fly".
   @seealso(LoadMap) *)
  PROCEDURE CreateMap (CONST Width, Height: INTEGER);
  VAR
    Y, X: INTEGER;
  BEGIN
    SetLength (Map, Width);
    FOR X := LOW (Map) TO HIGH (Map) DO
    BEGIN
      SetLength (Map[X], Height);
      FOR Y := LOW (Map[X]) TO HIGH (Map[X]) DO
	Map[X][Y] := T_VOID;
    END;
    MapWidth := Width;
    MapHeight := Height;
  END;



(* Gets the map information from a file.
  @returns(@true on success or @false on failure.) *)
  FUNCTION LoadMap (FileName: STRING): BOOLEAN;
  VAR
    PackFile: AL_PACKFILEptr;
  BEGIN
    LoadMap := FALSE;
    PackFile := al_pack_fopen (FileName, 'r');
    IF PackFile <> NIL THEN
    BEGIN
      LoadMap := LoadMapPf (PackFile);
      al_pack_fclose (PackFile);
    END;
  END;



(* Like @link(LoadMap), but reads from a packfile. *)
  FUNCTION LoadMapPf (PackFile: AL_PACKFILEptr): BOOLEAN;
  VAR
    X, Y: INTEGER;
  BEGIN
    LoadMapPf := FALSE;
  { First, loads map size. }
    MapWidth := al_pack_mgetw (PackFile); MapHeight := al_pack_mgetw (PackFile);
    IF al_pack_ferror (PackFile) <> 0 THEN
      EXIT;
    CreateMap (MapWidth, MapHeight);
  { Now, map start and map end. }
    StartX := al_pack_mgetw (PackFile); StartY := al_pack_mgetw (PackFile);
    EndX := al_pack_mgetw (PackFile); EndY := al_pack_mgetw (PackFile);
  { Now, the map. }
    FOR Y := 0 TO MapHeight - 1 DO
      FOR X := 0 TO MapWidth - 1 DO
	Map[X][Y] := al_pack_getc (PackFile);
  { Check for errors. }
    loadMapPf := al_pack_ferror (PackFile) = 0;
  END;



(* Fixes the scroll values so it isn't out of the edges of the board.  Should be
   used before to draw anything. *)
  PROCEDURE FixScroll (CONST Bmp: AL_BITMAPptr; CONST Ix, Iy: INTEGER;
			VAR Ox, Oy: INTEGER);
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
  PROCEDURE DrawMap (Bmp: AL_BITMAPptr; ScrollX, ScrollY: INTEGER);
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
    PosX := OffsetX;
    FOR X := FirstTileX TO FirstTileX + NumTilesW DO
    BEGIN
      PosY := OffsetY;
      FOR Y := FirstTileY TO FirstTileY + NumTilesH DO
      BEGIN
	IF (X < MapWidth) AND (Y < MapHeight)
	AND (Map[X][Y] > T_VOID)
	AND (Tileset[Map[x, y]] <> NIL) THEN
	  al_blit (Tileset[Map[X][Y]], Bmp, 0, 0, PosX, PosY, TSIZE, TSIZE);
      { Next tile position. }
	INC (PosY, TSIZE);
      END;
    { Next tile position. }
      INC (PosX, TSIZE);
    END;
  END;



(* Saves the map information to a file.  It's used by the map editor.
  @seealso(LoadMap) @seealso(SaveMapPf)
 *)
  FUNCTION SaveMap (FileName: STRING): BOOLEAN;
  VAR
    PackFile: AL_PACKFILEptr;
  BEGIN
    SaveMap := FALSE;
    PackFile := al_pack_fopen (FileName, 'w');
    IF PackFile <> NIL THEN
    BEGIN
      SaveMap := SaveMapPf (PackFile);
      al_pack_fclose (PackFile);
    END;
  END;



(* Like @link(SaveMap), but reads from a packfile. *)
  FUNCTION SaveMapPf (PackFile: AL_PACKFILEptr): BOOLEAN;
  VAR
    X, Y: INTEGER;
  BEGIN
  { First, saves map size. }
    al_pack_mputw (MapWidth, PackFile); al_pack_mputw (MapHeight, PackFile);
  { Now, map start and map end. }
    al_pack_mputw (StartX, PackFile); al_pack_mputw (StartY, PackFile);
    al_pack_mputw (EndX, PackFile); al_pack_mputw (EndY, PackFile);
  { Now, the map. }
    FOR Y := 0 TO MapHeight - 1 DO
      FOR X := 0 TO MapWidth - 1 DO
	al_pack_putc (Map[X][Y], PackFile);
  { Check for errors. }
    SaveMapPf := al_pack_ferror (PackFile) = 0;
  END;

END.
