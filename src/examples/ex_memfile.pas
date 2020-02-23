PROGRAM ex_memfile;
(*
 *      Example program for the Allegro library.
 *
 *      Test memfile addon.
 *)
(*
  Copyright (c) 2012-2020 Guillermo Martínez J.

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

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

USES
  Common,
  allegro5, al5base, al5memfile,
  sysutils;

CONST
  DataSize = 1024;
VAR
  MemFile: ALLEGRO_FILEptr;
  Data: PBYTE;
  i: INTEGER;
  Ret: AL_INT32;
  Buffer: STRING[50]; { <-- This is a Pascal STRING not an ANSISTRING! }

  PROCEDURE EndProgram;
  BEGIN
    al_fclose (MemFile);
    Freemem (Data, DataSize);

    CloseLog (TRUE);

    HALT (1)
  END;

BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');
  OpenLog;
{ Reserve memory for the mem file. }
  Getmem (Data, DataSize);
  IF NOT Assigned (Data) THEN AbortExample ('Out of memory.');

  LogWriteLn ('Creating memfile.');
  MemFile := al_open_memfile (Data, DataSize, 'rw');
  IF NOT Assigned (MemFile) THEN
  BEGIN
    LogWriteLn ('Error opening memfile :(');
    EndProgram
  END;

  LogWriteLn ('Writing data to memfile.');
  FOR i := 0 TO (DataSize DIV 4) - 1 DO
    IF al_fwrite32le (MemFile, i) < 4 THEN
    BEGIN
      LogPrintLn ('Failed to write %i to memfile.', [i]);
      EndProgram
    END;

  al_fseek (MemFile, 0, ALLEGRO_SEEK_SET);

  LogWriteLn ('Reading and testing data from memfile.');
  FOR i := 0 TO (DataSize DIV 4) - 1 DO
  BEGIN
    Ret := al_fread32le (MemFile);
    IF (Ret <> i) OR al_feof (MemFile) THEN
    BEGIN
      LogPrintLn ('Item %d failed to verify, got %d.', [i, Ret]);
      EndProgram
    END
  END;

  IF al_feof (MemFile) THEN
  BEGIN
    LogWriteLn ('EOF indicator prematurely set!');
    EndProgram
  END;

  LogWriteLn ('Testing the ungetc buffer.');
  al_fseek (MemFile, 0, ALLEGRO_SEEK_SET);

  i := 0;
  WHILE al_fungetc (MemFile, i) <> AL_EOF DO INC (i);
  LogPrintLn ('Length of ungetc buffer: %d.', [i]);

  IF al_ftell (MemFile) <> -i THEN
  BEGIN
    LogPrintLn (
      'Current position is not correct. Expected -%d, but got %d.',
      [i, al_ftell (MemFile)]
    );
    EndProgram
  END;

  DEC (i);
  WHILE i >= 0 DO
  BEGIN
    IF i <> al_fgetc (MemFile) THEN
    BEGIN
      LogWriteLn ('Failed to verify ungetc data.');
      EndProgram
    END;
    DEC (i)
  END;

  IF al_ftell (MemFile) <> 0 THEN
  BEGIN
    LogWriteLn ('Current position is not correct after reading back the ungetc buffer.');
    LogPrintLn ('Expected 0, but got %d.', [al_ftell (MemFile)]);
    EndProgram
  END;

  LogWriteLn ('Working with strings.');
{ I (Guillermo) am not sure why al_fgets must read 15 chars, but string length
  is 14.  Try to change any of these values and see what happens.

  May be al_fputs writes the string length too but I doubt it.   May be it is
  the #0 character that closes strings on C.  I hope I can find why this should
  be that way. }
  al_fputs (MemFile, 'legro rocks!');
  al_fseek (MemFile, 0, ALLEGRO_SEEK_SET);
  al_fungetc (MemFile, ORD ('l'));
  al_fungetc (MemFile, ORD ('A'));
  al_fgets (MemFile, @Buffer[1], 15);
  Buffer[0] := #14; { String length. }
  IF Buffer <> 'Allegro rocks!' THEN
  BEGIN
    LogPrintLn ('Expected to see "Allegro rocks!" but got "%s" instead.', [Buffer]);
    LogWriteLn ('(Maybe the ungetc buffer isn''t big enough.)');
    EndProgram
  END;

  LogWriteLn ('Done.');

  al_fclose (MemFile);
  Freemem (Data, DataSize);

  CloseLog (TRUE)
END.
