UNIT gamedata;
(* Program: Demo game for the Allegro.pas library.
 * File: gamedata.pas
 * Description: Manages the data of the demo: graphics, animations, music and
 *		sound.
 * Author: Ñuño Martínez <niunio at users.sourceforge.net>
 *)

INTERFACE

  USES
    allegro, alfile;

  VAR
  { This allows to acess all game graphics and sound. }
    Data: AL_DATAFILEptr;

{ Indexes for each object stored in the datafile. }
{$I demo.inc}

  CONST
  (* Identifiers of blocks.  They're tiles, not bitmaps. *)
    T_COIN = 1;
    T_BLK1 = 2;
    T_BLK2 = 3;
    T_BLK3 = 4;
    T_ROWR = 5;
    T_ROWL = 6;
    T_ROWD = 7;



(* Loads the datafile and stores it in the "Data" pointer.  Return TRUE
   on success. *)
  FUNCTION LoadData: BOOLEAN;

(* Releases all resources used by the data.  Should be called before uninstall
   Allegro. *)
  PROCEDURE ReleaseData;

IMPLEMENTATION

  USES
    Tilemap, { To assign tileset bitmaps. }
    sysutils;

(* Loads the datafile and stores it in the "Data" pointer.  Returns TRUE
   on success. *)
  FUNCTION LoadData: BOOLEAN;
  VAR
    buf: STRING;
    Tiles: AL_BITMAPptr;
    Ndx, X, Y: INTEGER;
  BEGIN
  { Load the datafile into memory. }
    buf :=  'demo.dat';
    Data := al_load_datafile (buf);
  { Check if it was loaded. }
    IF Data <> NIL THEN
    BEGIN
    { Assigns tileset. }
      SetLength (TileSet, T_ROWD + 1);
      Tiles := Data^[BMP_TILES].dat;
      X := 0; Y := 0;
      FOR Ndx := T_COIN TO T_ROWD DO
      BEGIN
	TileSet[Ndx] := al_create_sub_bitmap (Tiles, X, Y, TSIZE, TSIZE);
      { Next tile. }
	INC (X, TSIZE);
	IF X >= Tiles^.w THEN
	BEGIN
	  X := 0;
	  INC (Y, TSIZE);
	END
      END;
      EXIT (TRUE)
    END;
    LoadData := FALSE { Didn't load data! }
  END;



(* Releases all resources used by the data.  Should be called before uninstall
   Allegro. *)
  PROCEDURE ReleaseData;
  VAR
    Ndx: INTEGER;
  BEGIN
    IF Data <> NIL THEN
    BEGIN
      FOR Ndx := T_COIN TO T_ROWD DO
	al_destroy_bitmap (TileSet[Ndx]);
      al_unload_datafile (Data);
      Data := NIL;
    END;
  END;

END.
