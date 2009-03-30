UNIT UnitDataFile;
(*< Defines a class that expands Allegro's AL_DATAFILE struct making it easer to
    edit. *)
{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils,
  albase, aldtfile;



TYPE
(* Class that encapsulates and expands Allegro's AL_DATAFILE. *)
  TDataFile = CLASS
  PRIVATE
  (* Stores the datafile.  Note it isn't a pointer to make edition more easy. *)
    fData: ARRAY [0..AL_UNKNOWN_SIZE] OF AL_DATAFILE_OBJECTptr;
  (* Stores the latest loaded datafile. *)
    fDataFile: AL_DATAFILEptr;
  (* To know if it was modified. *)
    fModified: BOOLEAN;
  PUBLIC
  (* Constructor. *)
    CONSTRUCTOR Create;
  (* Destructor. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Cleans the file. *)
    PROCEDURE Clean;
  (* Loads the datafile. *)
    PROCEDURE LoadFrom (FileName: STRING);
  (* Saves the datafile. *)
    PROCEDURE SaveTo (Filename: STRING);

    PROPERTY Modified: BOOLEAN READ fModified;
  END;



IMPLEMENTATION

(* Constructor. *)
CONSTRUCTOR TDataFile.Create;
BEGIN
  SELF.fModified := FALSE;
END;



(* Destructor. *)
DESTRUCTOR TDataFile.Destroy;
BEGIN
  IF SELF.fDataFile <> NIL THEN
    al_unload_datafile (SELF.fDataFile);
  INHERITED;
END;



(* Cleans the file. *)
PROCEDURE TDataFile.Clean;
BEGIN
  IF SELF.fDataFile <> NIL THEN
    al_unload_datafile (SELF.fDataFile);
  SELF.fDataFile := NIL;
  SELF.fModified := FALSE;
END;



(* Loads the datafile. *)
PROCEDURE TDataFile.LoadFrom (FileName: STRING);
VAR
  TmpDataFile: AL_DATAFILEptr;
BEGIN
  TmpDataFile := al_load_datafile (FileName);
  IF TmpDataFile = NIL THEN
    RAISE Exception.Create ('Can''t load '+FileName+'+');
{ TODO: Add data to the fData list. }
{ Temporal datafile doesn't needed. }
  SELF.Clean;
  SELF.fDataFile := TmpDataFile;
  SELF.fModified := FALSE;
END;



(* Saves the datafile. *)
PROCEDURE TDataFile.SaveTo (Filename: STRING);
BEGIN
  RAISE Exception.Create ('TDataFile::SaveTo unimplemented!');
END;

END.

