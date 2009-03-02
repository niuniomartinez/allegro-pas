UNIT gamedata;
(* Program: Demo game for the Allegro.pas library.
 * File: gamedata.pas
 * Description: Manages the data of the demo: graphics, animations, music and
 *		sound.
 * Author: Ñuño Martínez <niunio at users.sourceforge.net>
 *)

{$H+}
 
INTERFACE

USES
  aldtfile; { Access to grabber data files. }



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
  alfile;   { File functions. }



(* LoadData:
 *   Loads the datafile and stores it in the "Data" pointer.  Returns TRUE
 *   on success. *)
FUNCTION LoadData: BOOLEAN;
VAR
  buf: ANSISTRING;
BEGIN
{ Load the datafile into memory. }
  al_replace_filename (buf, PARAMSTR (0), 'demo.dat', 256);
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
