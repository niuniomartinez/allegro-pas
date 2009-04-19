UNIT gamedata;
(* Program: Demo game for the Allegro.pas library.
 * File: gamedata.pas
 * Description: Manages the data of the demo: graphics, animations, music and
 *		sound.
 * Author: Ñuño Martínez <niunio at users.sourceforge.net>
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  allegro, alfile;



VAR
{ This allows to acess all game graphics and sound. }
  Data: AL_DATAFILEptr;



{ Indexes for each object stored in the datafile. }
{$I demo.inc}



(* LoadData:
 *   Loads the datafile and stores it in the "Data" pointer.  Return TRUE
 *   on success. *)
FUNCTION LoadData: BOOLEAN;



(* ReleaseData:
 *   Releases all resources used by the data.  Should be called before
 *   uninstall Allegro. *)
PROCEDURE ReleaseData;



IMPLEMENTATION

USES
  sysutils;



(* LoadData:
 *   Loads the datafile and stores it in the "Data" pointer.  Returns TRUE
 *   on success. *)
FUNCTION LoadData: BOOLEAN;
VAR
  buf: STRING;
BEGIN
{ Load the datafile into memory. }
  buf :=  ExtractFilePath (PARAMSTR (0)) + 'demo.dat';
  Data := al_load_datafile (buf);
{ Check if it was loaded. }
  LoadData := (Data <> NIL);
END;



(* ReleaseData:
 *   Releases all resources used by the data.  Should be called before
 *   uninstall Allegro. *)
PROCEDURE ReleaseData;
BEGIN
  IF Data <> NIL THEN
  BEGIN
    al_unload_datafile (Data);
    Data := NIL;
  END;
END;



END.
