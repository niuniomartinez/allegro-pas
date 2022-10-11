program ex_file_slice;
(*
 *  ex_file_slice - Use slices to pack many objects into a single file.
 *
 *  This example packs two strings into a single file, and then uses a
 *  file slice to open them one at a time. While this usage is contrived,
 *  the same principle can be used to pack multiple images (for example)
 *  into a single file, and later read them back via Allegro's image loader. 
 *
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
    Allegro5, al5base, al5strings, Common,
    sysutils;

  const
    BUFFER_SIZE = 1024;

  procedure PackObject
    (aFile: ALLEGRO_FILEptr; const aObject: POINTER; aLen: AL_SIZE_T);
  begin
  { First write the length of the object, so we know how big to make the slice
    when it is opened later. }
    al_fwrite32le (aFile, aLen);
    al_fwrite (aFile, aObject, aLen)
  end;


  function GetNextChunk (aFile: ALLEGRO_FILEptr): ALLEGRO_FILEptr;
  var
    lLength: LongInt;
  begin
  { Reads the length of the next chunk, and if not at end of file, returns a
    slice that represents that portion of the file. }
    lLength := al_fread32le (aFile);
    if al_feof (aFile) then
      Exit (Nil)
    else
     Exit (al_fopen_slice (aFile, lLength, 'rw'))
  end;



  const
   FirstString = 'Hello, World!';
   SecondString = 'The quick brown fox jumps over the lazy dog.';

  var
    Master, Slice: ALLEGRO_FILEptr;
    Buffer: PCHAR;
    FileName: AL_STR;

begin
  if not al_init then
    AbortExample ('Could not init Allegro.');

  OpenLog;

  FileName := al_string_to_str (GetTempFilename);
  Master := al_fopen (FileName, 'w');
  if Master = Nil then
    AbortExample ('Unable to create temporary file.');

{ Pack both strings into the master file. }
  PackObject (Master, PCHAR (FirstString), Length (FirstString));
  PackObject (Master, PCHAR (SecondString), Length (SecondString));

{ Closes and opens again, so we can read the created data. }
  al_fclose (Master);
  Master := al_fopen (FileName, 'r');

  Buffer := StrAlloc (BUFFER_SIZE);

{ Loop through the main file, opening a slice for each object. }
  repeat
    Slice := GetNextChunk (Master);
    if Slice <> Nil then
    begin
    { Note: While the slice is open, we must avoid using the master file!
	    If you were dealing with packed images, this is where you would
	    pass 'slice' to al_load_bitmap_f(). }
      if al_fsize (Slice) < BUFFER_SIZE then
      begin
      { We could have used al_fgets(), but just to show that the file slice
	is constrained to the string object, we'll read the entire slice. }
	al_fread (Slice, Buffer, al_fsize (Slice));
	Buffer[al_fsize (Slice)] := CHAR (0);
	LogWriteLn (al_str_format (
	  'Chunk of size %d: ''%s''.', [al_fsize (Slice), Buffer]
	))
      end;
    { The slice must be closed before the next slice is opened. Closing
      the slice will advanced the master file to the end of the slice. }
      al_fclose (Slice)
    end
  until Slice = Nil;

  al_fclose (Master);

  StrDispose (Buffer);
  CloseLog (True)
end.
