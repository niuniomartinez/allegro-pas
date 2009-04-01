UNIT alfile;
(*< The routines of this module implement a fast buffered file I/O system,
    which supports the reading and writing of compressed files using a ring
    buffer algorithm based on the LZSS compressor by Haruhiko Okumura.  This
    does not achieve quite such good compression as programs like zip and lha,
    but unpacking is very fast and it does not require much memory.  Packed
    files always begin with the 32-bit value @link (AL_F_PACK_MAGIC), and
    autodetect files with the value F_NOPACK_MAGIC. *)

{$H+}
{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}



INTERFACE

{$IFNDEF FPC}
 {$IFDEF MSWINDOWS}
{ Delphi needs the Windows unit. }
USES
  albase, { Needs some basic definitions. }
  Windows;
 {$ENDIF}
{$ELSE}
USES
  albase; { Needs some basic definitions. }
{$ENDIF}



(* File input and output. *)
CONST
(* To be used as mode paramter of @link (al_pack_fopen). *)
  AL_F_READ         = 'r';
  AL_F_WRITE        = 'w';
  AL_F_READ_PACKED  = 'rp';
  AL_F_WRITE_PACKED = 'wp';
  AL_F_WRITE_NOPACK = 'w!';

  AL_F_PACK_MAGIC   = $736C6821; (*< magic number for packed files *)
  AL_F_NOPACK_MAGIC = $736C682E; (*< magic number for autodetect *)
  AL_F_EXE_MAGIC    = $736C682B; (*< magic number for appended data *)



TYPE
(* To be used to identify an opened file. *)
  AL_PACKFILEptr = POINTER;



(* Opens a file according to mode, which may contain any of the flags:
   @unorderedList (
     @item (@code (r) - open file for reading.)
     @item (@code (w) - open file for writing, overwriting any existing data.)
     @item (@code (p) - open file in packed mode.  Data will be compressed as
       it is written to the file, and automatically uncompressed during read
       operations.  Files created in this mode will produce garbage if they are
       read without this flag being set.)
     @item (@code (!) - open file for writing in normal, unpacked mode, but add
       the value @link (AL_F_NOPACK_MAGIC) to the start of the file, so that it
       can later be opened in packed mode and Allegro will automatically detect
       that the data does not need to be decompressed.)
   )

  Instead of these flags, one of the constants @link (AL_F_READ), @link
  (AL_F_WRITE), @link (AL_F_READ_PACKED), @link (AL_F_WRITE_PACKED) or @link
  (AL_F_WRITE_NOPACK) may be used as the @code (mode) parameter.

  The packfile functions also understand several @italic ("magic") filenames
  that are used for special purposes.  These are @bold
  (filename.dat#object_name) - open a specific object from a datafile, and read
  from it as if it was a regular file.  You can treat nested datafiles exactly
  like a normal directory structure, for example you could open @code
  (filename.dat#graphics/level1/mapdata).)

  With these special filenames, the contents of a datafile object or appended
  file can be read in an identical way to a normal disk file, so any of the
  file access functions in Allegro (eg. @link (al_set_config_file)) can be used
  to read from them.  Note that you can't write to these special files, though:
  the fake file is read only.  Also, you must save your datafile uncompressed
  or with per-object compression if you are planning on loading individual
  objects from it (otherwise there will be an excessive amount of seeking when
  it is read).

  Finally, be aware that the special Allegro object types aren't the same
  format as the files you import the data from.  When you import data like
  bitmaps or samples into the grabber, they are converted into a special
  Allegro-specific format, but the @code (`#') marker file syntax reads the
  objects as raw binary chunks.  This means that if, for example, you want
  to use @link (al_load_bitmap) to read an image from a datafile, you should
  import it as a binary block rather than as an @italic (BITMAP) object.

  @returns (on success, a pointer of type @link (AL_PACKFILEptr), and on error
    a @nil and stores an error code in @link (al_errno).  An attempt to read a
    normal file in packed mode will cause @code (al_errno) to be set to EDOM. *)
  FUNCTION al_pack_fopen (filename, mode: STRING): AL_PACKFILEptr;

(* Closes the stream @code (f) previously opened with @link (al_pack_fopen).
   After you have closed the stream, performing operations on it will yield
   errors in your application (e.g. crash it) or even block your OS.

   @returns (zero on success or an error code which is also stored in @link
     (al_errno).  This function can fail only when writing to files:  if the
     file was opened in read mode, it will always succeed. *)
  FUNCTION al_pack_fclose (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fclose';

(* Moves the position indicator of the stream @code (f).  This only supports
   forward movements relative to the current position and in read-only streams,
   so don't use negative offsets.  Note that seeking is very slow when reading
   compressed files, and so should be avoided unless you are sure that the file
   is not compressed.
   @returns (@true on success, @false on failure, storing the error code in
     @link (al_errno).) *)
  FUNCTION al_pack_fseek (f: AL_PACKFILEptr; offset: LONGINT): BOOLEAN;

(* Finds out if you have reached the end of the file.  It does not wait for you
   to attempt to read beyond the end of the file, contrary to the ISO C @code
   (feof) function.  The only way to know whether you have read beyond the end
   of the file is to check the return value of the read operation you use (and
   be wary of @code (al_pack_*getl) as EOF is also a valid return value with
   these functions).
   @returns (@true if you are at the end of the file, @false otherwise.) *)
  FUNCTION al_pack_feof (f: AL_PACKFILEptr): BOOLEAN;

(* Since EOF is used to report errors by some functions, it's often better to
   use the @link (al_pack_feof) function to check explicitly for end of file
   and @code (al_pack_ferror) to check for errors.  Both functions check
   indicators that are part of the internal state of the stream to detect
   correctly the different situations.
   @returns (nonzero if the error indicator for the stream is set, meaning that
     an error has occurred during a previous operation on the stream. *)
  FUNCTION al_pack_ferror (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_ferror';


(* Returns the next character from the stream `f', or EOF if the end of the
   file has been reached. *)
  FUNCTION al_pack_getc (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_getc';

(* Puts a character in the stream f.
   @returns (the character written on success, or EOF on error.) *)
  FUNCTION al_pack_putc (c: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_putc';

(* Like @link (al_pack_getc), but reads a 16-bit word from a file, using Intel
   byte ordering (least significant byte first, a.k.a. little-endian). *)
  FUNCTION al_pack_igetw (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_igetw';

(* Like @link (al_pack_getc), but reads a 32-bit word from a file, using Intel
   byte ordering (least significant byte first, a.k.a. little-endian). *)
  FUNCTION al_pack_igetl (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_igetl';

(* Like @link (al_pack_putc), but writes a 16-bit word to a file, using Intel
   byte ordering (least significant byte first, a.k.a. little-endian). *)
  FUNCTION al_pack_iputw (w: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_iputw';

(* Like @link (al_pack_putc), but writes a 32-bit word to a file, using Intel
   byte ordering (least significant byte first, a.k.a. little-endian). *)
  FUNCTION al_pack_iputl (l: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_iputl';

(* Like @link (al_pack_getc), but reads a 16-bit word from a file, using
   Motorola byte ordering (most significant byte first, a.k.a. big-endian). *)
  FUNCTION al_pack_mgetw (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mgetw';

(* Like @link (al_pack_getc), but reads a 32-bit word from a file, using
   Motorola byte ordering (most significant byte first, a.k.a. big-endian). *)
  FUNCTION al_pack_mgetl (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mgetl';

(* Like @link (al_pack_putc), but writes a 16-bit word from a file, using
   Motorola byte ordering (most significant byte first, a.k.a. big-endian). *)
  FUNCTION al_pack_mputw (w: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mputw';

(* Like @link (al_pack_putc), but writes a 32-bit word from a file, using
   Motorola byte ordering (most significant byte first, a.k.a. big-endian). *)
  FUNCTION al_pack_mputl (l: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mputl';

(* Reads @code (n) bytes from the stream @code (f), storing them at the memory
   location pointed to by @code (p).
   @returns the number of bytes read, which will be less than @code (n) if EOF
     is reached or an error occurs.  Error codes are stored in @link
     (al_errno).) *)
  FUNCTION al_pack_fread (p: POINTER; n: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fread';

(* Writes @code (n) bytes from the stream @code (f), storing them at the memory
   location pointed to by @code (p).
   @returns the number of bytes written, which will be less than @code (n) if
     EOF is reached or an error occurs.  Error codes are stored in @link
     (al_errno).) *)
  FUNCTION al_pack_fwrite (p: POINTER; n: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fwrite';


(* Puts a character back to the file's input buffer.  Like with @code (ungetc)
   from libc, only a single push back is guaranteed.

   Note: @link (al_pack_fgets) internally uses @code (al_pack_ungetc), so never
   use @code (al_pack_ungetc) directly after using @code (al_pack_fgets) on a
   PACKFILE.
   @returns (@code (c) on success, or EOF on error. *)
  FUNCTION al_pack_ungetc (c: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_ungetc';

(* Reads a line from the stream @code (f) and returns it.  Stops when a
   linefeed is encountered, or @code (max) bytes have been read.  The end of
   line is handled by detecting the right combination of characters for the
   platform.  This supports CR-LF (DOS/Windows), LF (Unix), and CR (Mac)
   formats.  However, the trailing carriage return is not included in the
   returned string, in order to provide easy code portability across platforms.
   If you need the carriage return, use @link (al_pack_fread) and/or @link
   (pack_getc) instead.

   Note:  This function internally may make calls to @link (al_pack_ungetc), so
   you cannot use @code (al_pack_ungetc) directly afterwards. *)
  FUNCTION al_pack_fgets (max: LONGINT; f: AL_PACKFILEptr): STRING;

(* Writes a string to the stream @code (f).  The input string is converted from
   the current text encoding format to UTF-8 before writing.  Newline
   characters are written as `\r\n' on DOS and Windows platforms.  If you don't
   want this behaviour, use @link (al_pack_fwrite) and/or @link (pack_putc)
   instead.
   @returns (@true on success or @falseon error. *)
  FUNCTION al_pack_fputs (p: STRING; f: AL_PACKFILEptr): BOOLEAN;




IMPLEMENTATION

USES
  sysutils;



(* File input and output. *)
  FUNCTION pack_fopen (filename, mode: PCHAR): AL_PACKFILEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION pack_fseek (f: AL_PACKFILEptr; offset: LONGINT): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION pack_feof (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION pack_fgets (p: PCHAR; max: LONGINT; f: AL_PACKFILEptr): PCHAR; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
  FUNCTION pack_fputs (p: PCHAR; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

  FUNCTION al_pack_fopen (filename, mode: STRING): AL_PACKFILEptr;
  BEGIN
    al_pack_fopen := pack_fopen (PCHAR (filename), PCHAR (mode));
  END;

  FUNCTION al_pack_fseek (f: AL_PACKFILEptr; offset: LONGINT): BOOLEAN;
  BEGIN
    al_pack_fseek := (pack_fseek (f, offset) = 0);
  END;

  FUNCTION al_pack_feof (f: AL_PACKFILEptr): BOOLEAN;
  BEGIN
    al_pack_feof := (pack_feof (f) = 0)
  END;

  FUNCTION al_pack_fgets (max: LONGINT; f: AL_PACKFILEptr): STRING;
  VAR
    Temp: PChar;
  BEGIN
    Temp := STRALLOC (max + 1);
    al_pack_fgets := STRING (pack_fgets (Temp, max, f));
    StrDispose (Temp);
  END;

  FUNCTION al_pack_fputs (p: STRING; f: AL_PACKFILEptr): BOOLEAN;
  BEGIN
    al_pack_fputs := (pack_fputs (PCHAR (p), f) = 0);
  END;

END.

