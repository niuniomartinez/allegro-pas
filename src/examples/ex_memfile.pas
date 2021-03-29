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

uses
  Common,
  allegro5, al5base, al5memfile,
  sysutils;

const
  DataSize = 1024;
var
  MemFile: ALLEGRO_FILEptr;
  Data: PBYTE;
  i: Integer;
  Ret: AL_INT32;
  Buffer: String[50]; { <-- This is a Pascal String not an AnsiString! }

  procedure EndProgram;
  begin
    al_fclose (MemFile);
    Freemem (Data, DataSize);

    CloseLog (True);

    HALT (1)
  end;

begin
  if not al_init then AbortExample ('Could not init Allegro.');
  OpenLog;
{ Reserve memory for the mem file. }
  Getmem (Data, DataSize);
  if not Assigned (Data) then AbortExample ('Out of memory.');

  LogWriteLn ('Creating memfile.');
  MemFile := al_open_memfile (Data, DataSize, 'rw');
  if not Assigned (MemFile) then
  begin
    LogWriteLn ('Error opening memfile :(');
    EndProgram
  end;

  LogWriteLn ('Writing data to memfile.');
  for i := 0 to (DataSize div 4) - 1 do
    if al_fwrite32le (MemFile, i) < 4 then
    begin
      LogPrintLn ('Failed to write %i to memfile.', [i]);
      EndProgram
    end;

  al_fseek (MemFile, 0, ALLEGRO_SEEK_SET);

  LogWriteLn ('Reading and testing data from memfile.');
  for i := 0 to (DataSize div 4) - 1 do
  begin
    Ret := al_fread32le (MemFile);
    if (Ret <> i) or al_feof (MemFile) then
    begin
      LogPrintLn ('Item %d failed to verify, got %d.', [i, Ret]);
      EndProgram
    end
  end;

  if al_feof (MemFile) then
  begin
    LogWriteLn ('EOF indicator prematurely set!');
    EndProgram
  end;

  LogWriteLn ('Testing the ungetc buffer.');
  al_fseek (MemFile, 0, ALLEGRO_SEEK_SET);

  i := 0;
  while al_fungetc (MemFile, i) <> AL_EOF do Inc (i);
  LogPrintLn ('Length of ungetc buffer: %d.', [i]);

  if al_ftell (MemFile) <> -i then
  begin
    LogPrintLn (
      'Current position is not correct. Expected -%d, but got %d.',
      [i, al_ftell (MemFile)]
    );
    EndProgram
  end;

  Dec (i);
  while i >= 0 do
  begin
    if i <> al_fgetc (MemFile) then
    begin
      LogWriteLn ('Failed to verify ungetc data.');
      EndProgram
    end;
    Dec (i)
  end;

  if al_ftell (MemFile) <> 0 then
  begin
    LogWriteLn ('Current position is not correct after reading back the ungetc buffer.');
    LogPrintLn ('Expected 0, but got %d.', [al_ftell (MemFile)]);
    EndProgram
  end;

  LogWriteLn ('Working with strings.');
{ I (Guillermo) am not sure why al_fgets must read 15 chars, but string length
  is 14.  Try to change any of these values and see what happens.

  May be al_fputs writes the string length too but I doubt it.   May be it is
  the #0 character that closes strings on C.  I hope I can find why this should
  be that way. }
  al_fputs (MemFile, 'legro rocks!');
  al_fseek (MemFile, 0, ALLEGRO_SEEK_SET);
  al_fungetc (MemFile, Ord ('l'));
  al_fungetc (MemFile, Ord ('A'));
  al_fgets (MemFile, @Buffer[1], 15);
  Buffer[0] := #14; { String length. }
  if Buffer <> 'Allegro rocks!' then
  begin
    LogPrintLn ('Expected to see "Allegro rocks!" but got "%s" instead.', [Buffer]);
    LogWriteLn ('(Maybe the ungetc buffer isn''t big enough.)');
    EndProgram
  end;

  LogWriteLn ('Done.');

  al_fclose (MemFile);
  Freemem (Data, DataSize);

  CloseLog (True)
end.
