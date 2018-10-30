UNIT Data;
(* Manages and stores game data. *)
(*
  Copyright (c) 2018 Handoko, Guillermo Martínez.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

INTERFACE

  USES
    Allegro5, al5audio, al5font, al5ttf;

(* Loads the requested font file.  On error raises an exception. *)
  FUNCTION LoadFont (CONST FileName: STRING; Size: INTEGER): ALLEGRO_FONTptr;
(* Loads the requested bitmap file.  On error raises an exception. *)
  FUNCTION LoadBitmap (CONST FileName: STRING): ALLEGRO_BITMAPptr;
(* Loads the requested sound or music file.  On error raises an exception. *)
  FUNCTION LoadSample (CONST FileName: STRING): ALLEGRO_SAMPLEptr;

IMPLEMENTATION

  USES
    sysutils;

  VAR
  { Data directory path. }
    DataPath: STRING;

  (* Looks for the given data file and returns the full path or raises an
     exception if it can't find it. *)
  FUNCTION FindDataFile (CONST FileName: STRING): STRING;
  BEGIN
    { TODO:  Actual implementation should look data files in different paths
             depending on the operating system.  For example:  Windows should
             look in the working directory, then in the same directory than the
             executable,  Linux hsould work first in the working directory, then
             in the data directory (like "/usr/share/games/furriouspaladin" or
             similar) then in the same directory than the executable. }
    IF DataPath = '' THEN
    BEGIN
      DataPath := ExtractFilePath(ParamStr(0))+'data'+DirectorySeparator;
      IF NOT DirectoryExists (DataPath) THEN
        RAISE Exception.Create ('Can''t find data directory.  Reinstall.')
    END;
  { Right now it just check if file exists. }
    RESULT := DataPath + FileName;
    IF NOT FileExists (RESULT) THEN
      RAISE Exception.CreateFmt ('Can''t find data file "%s".', [FileName])
  END;



(* Loads font. *)
  FUNCTION LoadFont (CONST FileName: STRING; Size: INTEGER): ALLEGRO_FONTptr;
  BEGIN
    RESULT := al_load_ttf_font (FindDataFile (FileName), Size, 0);
    IF RESULT = NIL THEN
      RAISE Exception.CreateFmt ('Can''t load "%s" font.', [FileName]);
  END;



(* Loads bitmap. *)
  FUNCTION LoadBitmap (CONST FileName: STRING): ALLEGRO_BITMAPptr;
  BEGIN
    RESULT := al_load_bitmap (FindDataFile (FileName));
    IF RESULT = NIL THEN
      RAISE Exception.CreateFmt ('Can''t load "%s" bitmap.', [FileName]);
  END;



(* Loads sample. *)
  FUNCTION LoadSample (CONST FileName: STRING): ALLEGRO_SAMPLEptr;
  BEGIN
    RESULT := al_load_sample (FindDataFile (FileName));
    IF RESULT = NIL THEN
      RAISE Exception.CreateFmt ('Can''t load "%s".', [FileName]);
  END;

END.

