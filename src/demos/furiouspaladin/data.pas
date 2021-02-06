UNIT Data;
(*<Manages and stores game data. *)
(*
  Copyright (c) 2018 Handoko
            (c) 2019-2020 Guillermo Martínez.

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

INTERFACE

  USES
    Allegro5, al5audio, al5base, al5font, al5ttf;

(* Loads the requested font file.  On error returns nil. *)
  FUNCTION LoadFont (CONST FileName: AL_STR; Size: INTEGER): ALLEGRO_FONTptr;
(* Loads the requested bitmap file.  returns nilexception. *)
  FUNCTION LoadBitmap (CONST FileName: AL_STR): ALLEGRO_BITMAPptr;
(* Loads the requested sound or music file.  returns nilexception. *)
  FUNCTION LoadSample (CONST FileName: AL_STR): ALLEGRO_SAMPLEptr;

IMPLEMENTATION

  USES
    sysutils, al5strings;

{$IFDEF DCC}
  CONST
    DirectorySeparator = PathDelim;
{$ENDIF}

  VAR
  { Data directory path. }
    DataPath: AL_STR;

  (* Looks for the given data file and returns the full path or returns the
     FileName parameter if it can't find it. *)
  FUNCTION FindDataFile (CONST FileName: AL_STR): AL_STR;
  BEGIN
    { TODO:  Actual implementation should look data files in different paths
             depending on the operating system.  For example:  Windows should
             look in the working directory, then in the same directory than the
             executable,  Linux should look first in the working directory, then
             in the data directory (like "/usr/share/games/furiouspaladin" or
             similar) then in the same directory than the executable. }
    IF DataPath = '' THEN
    BEGIN
      DataPath := al_string_to_str (
        ExtractFilePath(ParamStr(0))+'data'+DirectorySeparator
      );
      IF NOT DirectoryExists (al_str_to_string (DataPath)) THEN
        EXIT (FileName)
    END;
  { Right now just check if file exists. }
    RESULT := DataPath + FileName;
    IF NOT FileExists (al_str_to_string (RESULT)) THEN RESULT := FileName
  END;



(* Loads font. *)
  FUNCTION LoadFont (CONST FileName: AL_STR; Size: INTEGER): ALLEGRO_FONTptr;
  BEGIN
    RESULT := al_load_ttf_font (FindDataFile (FileName), Size, 0)
  END;



(* Loads bitmap. *)
  FUNCTION LoadBitmap (CONST FileName: AL_STR): ALLEGRO_BITMAPptr;
  BEGIN
    RESULT := al_load_bitmap (FindDataFile (FileName))
  END;



(* Loads sample. *)
  FUNCTION LoadSample (CONST FileName: AL_STR): ALLEGRO_SAMPLEptr;
  BEGIN
    RESULT := al_load_sample (FindDataFile (FileName))
  END;

END.

