PROGRAM ex_file_slice;
(*
 *  ex_file_slice - Use slices to pack many objects into a single file.
 *
 *  This example packs two strings into a single file, and then uses a
 *  file slice to open them one at a time. While this usage is contrived,
 *  the same principle can be used to pack multiple images (for example)
 *  into a single file, and later read them back via Allegro's image loader. 
 *
 *)

{$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}

  USES
    Allegro5, al5base, Common,
    sysutils;

  CONST
    BUFFER_SIZE = 1024;

  PROCEDURE PackObject
    (aFile: ALLEGRO_FILEptr; CONST aObject: POINTER; aLen: AL_SIZE_T);
  BEGIN
  { First write the length of the object, so we know how big to make the slice
    when it is opened later. }
    al_fwrite32le (aFile, aLen);
    al_fwrite (aFile, aObject, aLen)
  END;


  FUNCTION GetNextChunk (aFile: ALLEGRO_FILEptr): ALLEGRO_FILEptr;
  VAR
    lLength: LONGINT;
  BEGIN
  { Reads the length of the next chunk, and if not at end of file, returns a
    slice that represents that portion of the file. }
    lLength := al_fread32le (aFile);
    IF al_feof (aFile) THEN
      EXIT (NIL)
    ELSE
     EXIT (al_fopen_slice (aFile, lLength, 'rw'))
  END;



  CONST
   FirstString = 'Hello, World!';
   SecondString = 'The quick brown fox jumps over the lazy dog.';

  VAR
    Master, Slice: ALLEGRO_FILEptr;
    Buffer: PCHAR;
    FileName: STRING;

BEGIN
  IF NOT al_init THEN
    AbortExample ('Could not init Allegro.');

  OpenLog;

  FileName := GetTempFilename;
  Master := al_fopen (FileName, 'w');
  IF Master = NIL THEN
    AbortExample ('Unable to create temporary file.');

{ Pack both strings into the master file. }
  PackObject (Master, PCHAR (FirstString), Length (FirstString));
  PackObject (Master, PCHAR (SecondString), Length (SecondString));

{ Closes and opens again, so we can read the created data. }
  al_fclose (Master);
  Master := al_fopen (FileName, 'r');

  Buffer := StrAlloc (BUFFER_SIZE);

{ Loop through the main file, opening a slice for each object. }
  REPEAT
    Slice := GetNextChunk (Master);
    IF Slice <> NIL THEN
    BEGIN
    { Note: While the slice is open, we must avoid using the master file!
            If you were dealing with packed images, this is where you would
	    pass 'slice' to al_load_bitmap_f(). }
      IF al_fsize (Slice) < BUFFER_SIZE THEN
      BEGIN
      { We could have used al_fgets(), but just to show that the file slice
        is constrained to the string object, we'll read the entire slice. }
         al_fread (Slice, Buffer, al_fsize (Slice));
         Buffer[al_fsize (Slice)] := CHAR (0);
         LogWriteLn (Format (
	   'Chunk of size %d: ''%s''.', [al_fsize (Slice), Buffer]
	 ))
      END;
    { The slice must be closed before the next slice is opened. Closing
      the slice will advanced the master file to the end of the slice. }
      al_fclose (Slice)
    END
  UNTIL Slice = NIL;

  al_fclose (Master);

  StrDispose (Buffer);
  CloseLog (TRUE)
END.
