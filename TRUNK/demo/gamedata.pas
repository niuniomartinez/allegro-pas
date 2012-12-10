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
    Ndx: INTEGER;
  BEGIN
  { Load the datafile into memory. }
    buf :=  ExtractFilePath (PARAMSTR (0)) + 'demo.dat';
    Data := al_load_datafile (buf);
  { Check if it was loaded. }
    LoadData := (Data <> NIL);
  { Assigns tileset. }
    SetLength (TileSet, T_ROWD + 1);
    FOR Ndx := T_COIN TO T_ROWD DO
      TileSet[Ndx] := Data^[Ndx - 1].dat;
  END;



(* Releases all resources used by the data.  Should be called before uninstall
   Allegro. *)
  PROCEDURE ReleaseData;
  BEGIN
    IF Data <> NIL THEN
    BEGIN
      al_unload_datafile (Data);
      Data := NIL;
    END;
  END;

END.
