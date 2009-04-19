UNIT tilemap;
(* Program: Demo game for the Allegro.pas library.
 * File: tilemap.pas
 * Description: Functions and procedures to load and render tilemaps.
 * Author: Ñuño Martínez <niunio at users.sourceforge.net>
 *	   Translated from a game by Ken Silverman <http://www.advsys.net/ken/>
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  allegro; { Bitmap manipulation. }



CONST
  BoardHeight = 15;	{ Height of the board. }



VAR
(* The tilemap. *)
  Board: ARRAY [1..150, 1..15] OF BYTE;
  BoardLength: INTEGER;	     { Length of the board. }
(* Starting and ending coordinates. *)
  StartX, StartY, EndX, EndY: INTEGER;



CONST
(* The tile values. *)
  T_VOID  = 0;
  T_START = 3;
  T_END   = 4;
  T_COIN  = 1;
  T_BLK1  = 11;
  T_BLK2  = 12;
  T_BLK3  = 13;

  TSIZE = 16; { Size of the tiles in pixels. }
  SMALL_TSIZE =  4;	{ Size of 'small board tiles'. }



  (* LoadBoard:
   *   Gets the map information from a file.  The name of the map is
   *   "boardN.brd".  Returns TRUE on success or FALSE on failure. *)
  FUNCTION LoadBoard (N: INTEGER): BOOLEAN;



  (* DrawBoardMiniature:
   *   Draws an small version of the map in the given bitmap. *)
  PROCEDURE DrawBoardMiniature (Bmp: AL_BITMAPptr);



  (* FixScroll:
   *   Be sure that the scroll isn't out of the edges of the board.  Should be
   *   used before draw anything. *)
  PROCEDURE FixScroll (CONST Bmp: AL_BITMAPptr; CONST Ix, Iy: INTEGER;
			VAR Ox, Oy: INTEGER);



  (* DrawBoard:
   *   Draws the board in the given bitmap at the given scroll coordinates. *)
  PROCEDURE DrawBoard (Bmp: AL_BITMAPptr; ScrollX, ScrollY: INTEGER);



IMPLEMENTATION

USES
  albltspr, { Draw bitmaps and sprites. }
  aldraw,   { Drawing primitives. }
  alsprrle, { Draw RLE sprites. }
  gamedata, { To acces to de game datafile. }
  sysutils; { For string manipulation. }



(* LoadBoard:
 *   Gets the map information from a file.  The name of the map is
 *   "boardN.brd".  Returns TRUE on success or FALSE on failure. *)
FUNCTION LoadBoard (N: INTEGER): BOOLEAN;
VAR
  Path, FileName: STRING; F: TEXT; { File definition. }
  Column: STRING;	     { To read the file. }
  x, y, ry: INTEGER;
BEGIN
  LoadBoard := FALSE;
{ Build the file name.
  First, get the path where the execubable is. }
  Path :=  ExtractFilePath (PARAMSTR (0));
{ Create the file name. }
  Filename :=  'board' + IntToStr (N) + '.brd';
{ Build the final name with path. }
  FileName := Path + Filename;
{ Open the file. }
  {$I-} { To save file errors in IOResult. }
  Assign (F, FileName); Reset (F);
  IF IOResult <> 0 THEN
    EXIT;
{ First line is the length of the board. }
  ReadLN (F, BoardLength);
{ Mark the starting and ending points: they aren't defined. }
  StartX := -1; StartY := -1;
  EndX := -1; EndY := -1;
{ Read the columns. }
  FOR x := 1 TO BoardLength DO
  BEGIN
    ReadLN (F, Column);
  { Parse the columns. }
    FOR y := 1 TO BoardHeight DO
    BEGIN
    { Needed because the y coordinate is inverted at the file. }
      ry := (BoardHeight + 1) - y;
      Board [x, ry] := BYTE (ORD (Column [y]) - ORD ('A'));
    { Look for the starting point. }
      IF Board[x, ry] = T_START THEN
      BEGIN
	IF StartX = -1 THEN
	BEGIN
	{ Store the coordinates. }
	  StartX := x;
	  StartY := ry;
	{ Delete the starting point tile. }
	  Board[x, ry] := T_VOID;
	END;
      END
    { Look for the ending point. }
      ELSE IF Board[x, ry] = T_END THEN
      { Store the right-most exit point. }
	IF EndX <= x THEN
	BEGIN
	{ Store the coordinates. }
	  EndX := x;
	  EndY := ry;
	{ Delete the ending point tile. }
	  Board[x, ry] := T_VOID;
	END;
      END;
    END;
  {$I+} { End storing file errors in IOResult. }
{ Close the file. }
  Close (F);
{ Check errors. }
  IF IOResult <> 0 THEN
    EXIT
  ELSE
    LoadBoard := TRUE;
END;



(* DrawBoardMiniature:
 *   Draws an small version of the map in the given bitmap. *)
PROCEDURE DrawBoardMiniature (Bmp: AL_BITMAPptr);
CONST
  W = SMALL_TSIZE - 1;   { Width of the tile. }
  C = W DIV 2; { Circle radius. }
VAR
  x, y: INTEGER;
BEGIN
  FOR x := 1 TO BoardLength DO
    FOR y := 1 TO 15 DO
      CASE Board[x, y] OF
      T_VOID:
	al_rectfill (Bmp, x * SMALL_TSIZE, y * SMALL_TSIZE,
		(x * SMALL_TSIZE) + W, (y * SMALL_TSIZE) + W,
		al_makecol (0, 255, 255));
      T_START:
	BEGIN
	  al_rectfill (Bmp, x * SMALL_TSIZE, y * SMALL_TSIZE,
			(x * SMALL_TSIZE) + W, (y * SMALL_TSIZE) + W,
			al_makecol (0, 0, 0));
	  al_circlefill (Bmp, (x * SMALL_TSIZE) + C, (y * SMALL_TSIZE) + C, C, al_makecol (0, 255, 0));
	END;
      T_END:
	BEGIN
	  al_rectfill (Bmp, x * SMALL_TSIZE, y * SMALL_TSIZE,
			(x * SMALL_TSIZE) + W, (y * SMALL_TSIZE) + W,
			al_makecol (0, 0, 0));
	  al_circlefill (Bmp, (x * SMALL_TSIZE) + C, (y * SMALL_TSIZE) + C, C, al_makecol (255, 0, 255));
	END;
      T_COIN:
	BEGIN
	  al_rectfill (Bmp, x * SMALL_TSIZE, y * SMALL_TSIZE,
			(x * SMALL_TSIZE) + W, (y * SMALL_TSIZE) + W,
			al_makecol (0, 0, 0));
          al_circlefill (Bmp, (x * SMALL_TSIZE) + C, (y * SMALL_TSIZE) + C, C, al_makecol (255, 255, 0));
	END;
      T_BLK1:
	al_rectfill (Bmp, x * SMALL_TSIZE, y * SMALL_TSIZE, (x * SMALL_TSIZE) + W, (y * SMALL_TSIZE) + W,
		     al_makecol (216, 216, 0));
      T_BLK2:
	BEGIN
	  al_rectfill (Bmp, x * SMALL_TSIZE, y * SMALL_TSIZE, (x * SMALL_TSIZE) + W, (y * SMALL_TSIZE) + 2,
		     al_makecol (0, 136, 0));
	  al_rectfill (Bmp,
			x * SMALL_TSIZE,       (y * SMALL_TSIZE) + 2,
			(x * SMALL_TSIZE) + W, (y * SMALL_TSIZE) + W,
			al_makecol (216, 216, 0));
	END;
      T_BLK3:
	al_rectfill (Bmp, x * SMALL_TSIZE, y * SMALL_TSIZE, (x * SMALL_TSIZE) + W, (y * SMALL_TSIZE) + W,
		     al_makecol (136, 136, 136));
      END;
END;



(* FixScroll:
 *   Be sure that the scroll isn't out of the edges of the board.  Should be
 *   used before to draw anything. *)
PROCEDURE FixScroll (CONST Bmp: AL_BITMAPptr; CONST Ix, Iy: INTEGER;
		     VAR Ox, Oy: INTEGER);
BEGIN
  IF Ix < 0 THEN
    Ox := 0
  ELSE IF Ix > (BoardLength * TSIZE) - Bmp^.w THEN
    Ox := (BoardLength * TSIZE) - Bmp^.w
  ELSE
    Ox := Ix;

  IF Iy < 0 THEN
    Oy := 0
  ELSE IF Iy > (BoardHeight * TSIZE) - Bmp^.h THEN
    Oy := (BoardHeight * TSIZE) - Bmp^.h
  ELSE
    Oy := Iy;
END;



(* DrawBoard:
 *   Draws the board in the given bitmap at the given scroll coordinates.
 *   I'm sure it can be optimized a lot (and it should be) but I try to keep
 *   it simple and understandable. *)
PROCEDURE DrawBoard (Bmp: AL_BITMAPptr; ScrollX, ScrollY: INTEGER);
VAR
  NumTilesW, NumTilesH: INTEGER; { Number of tiles to draw. }
  FirstTileX, FirstTileY: INTEGER; { First tile to draw. }
  OffsetX, OffsetY: INTEGER; { Relative coordinates of first tile. }
  PosX, PosY: INTEGER; { Where to draw the tile. }
  X, Y: INTEGER; { Tile to draw. }
BEGIN
{ Calculate how many tiles can be drawn in the bitmap. }
  NumTilesW  := (Bmp^.w DIV TSIZE) + 1; { Adds one for the edges. }
  NumTilesH  := (Bmp^.h DIV TSIZE) + 1;
  IF NumTilesH > BoardHeight THEN NumTilesH := BoardHeight;
{ Calculate the first tile to be drawn. }
  FirstTileX := (ScrollX DIV TSIZE) + 1;
  FirstTileY := (ScrollY DIV TSIZE) + 1;
{ Calculate the offset of that first tile.  They are the coordinates where the
  first tile will be drawn.  The tile offset is the pixel of the tile that is in
  the upper left pixel of the output bitmap. }
  OffsetX := -(ScrollX MOD TSIZE);
  OffsetY := -(ScrollY MOD TSIZE);
{ Draw. }
  PosY := OffsetY;
  FOR Y := FirstTileY TO FirstTileY + NumTilesH DO
  BEGIN
    PosX := OffsetX;
    FOR X := FirstTileX TO FirstTileX + NumTilesW DO
    BEGIN
      IF Board[X, Y] IN [T_BLK1, T_BLK2, T_BLK3] THEN
	al_blit (Data^[Board[X, Y] - T_BLK1].dat, Bmp, 0, 0, PosX, PosY, TSIZE, TSIZE)
      ELSE
      IF Board[X, Y] = T_COIN THEN
	al_draw_rle_sprite (Bmp, Data^[BMP_COIN].dat, PosX, PosY)
      ELSE
      IF Board[X, Y] = T_START THEN
	al_draw_rle_sprite (Bmp, Data^[BMP_MAIN_R0].dat, PosX, PosY)
      ELSE
      IF Board[X, Y] = T_END THEN
	al_draw_rle_sprite (Bmp, Data^[BMP_END].dat, PosX, PosY);
    { Increment the tile position. }
      INC (PosX, TSIZE);
    END;
  { Increment the tile position. }
    INC (PosY, TSIZE);
  END;
END;



END.
