UNIT alfile;
(*<Data files and low-level packed file.

 Datafiles are created by the @link(grabber) utility, and have a @code(.dat)
 extension.  They can contain bitmaps, palettes, fonts, samples, MIDI music,
 FLI/FLC animations, and any other binary data that you import.  You could
 distribute your bitmaps and samples in a myriad of separate files, but packing
 them in a few @code(.dat) binaries has a few advantages:
 @unorderedlist(
   @item(On some platforms loading a single big datafile at once is faster than
	loading individual resources one by one.)
   @item(Instead of several loops for your resources, you can write a single
	line of code with just a single point of failure to take care of.)
   @item(You can potentially reduce the size of your data by enabling
	compression on your datafiles.  Less download time for your end users,
	less wait during loading screens!)
   @item(If you don't need to load the whole datafile at once, you can still
	enable individual file compression.  It is slightly worse than global
	compression, but it is very fast with loading times because Allegro can
	easily seek inside the datafile to find a specific object.)
   @item(Even without encryption, most end users of your application won't be
	able to look at or modify the resources for your game.  A missing sound
	file or a modified bitmap could potentially crash the game if you
	haven't considered this in your loading code!)
   @item(It looks much more professional and convenient to distribute levels!
	For example, if you found a bug in a level of your game, just
	distribute your new @code(level4.dat) and tell users to overwrite their
	old version. )
  )
  Allegro allows you to load datafiles once and forget about them.  But if you
  have many levels it can be wise to load only the resources required for the
  current level.  You can accomplish the later by separating levels in
  different datafiles, or using functions like @code(al_load_datafile_object)
  to avoid loading everything at once.  You can even read directly from a
  specific datafile object with the @code(al_pack_fopen) function.

  Remember that with Allegro truecolor images can only be loaded after you have
  set a graphics mode.  This is true for datafiles too.  Load all your data
  after you have set the graphics mode, otherwise the pixel format (RGB or BGR)
  will not be known and the datafile may be converted wrongly.

  @bold(Note:)  even though Allegro datafiles provide encryption, you should
  consider it weak, so don't plan on hiding there the plans for a Death Star or
  something.  Determinate knowledgeable users will be able to rip your
  resources no matter how hard you try to hide them!  Use the encryption only
  as a slight deterrent towards unwanted tampering of your data.  How to crack
  an encrypted datafile is left as an exercise to the reader, though.

  @bold(See also) @link(al_load_datafile)

  Also this module implement a fast buffered file I/O system, which supports
  the reading and writing of compressed files using a ring buffer algorithm
  based on the LZSS compressor by Haruhiko Okumura.  This does not achieve
  quite such good compression as programs like zip and lha, but unpacking is
  very fast and it does not require much memory.  Packed files always begin
  with the 32-bit value @code(AL_F_PACK_MAGIC), and autodetect files with the
  value F_NOPACK_MAGIC.

  @bold(See also) @link(al_pack_fopen) *)

{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
 {$LONGSTRINGS ON}
 {$SMARTLINK ON}
{$ENDIF}

INTERFACE

USES
  albase, allegro; { Needs some basic definitions. }


(***************
 * Packed file *
 ***************)

CONST
(* To be used as mode parameter of @code(al_pack_fopen). *)
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
(* Pointer to @code(AL_PACKFILE_VTABLE). *)
  AL_PACKFILE_VTABLEptr = ^AL_PACKFILE_VTABLE;
(* Packfile vtable structure, for custom packfiles.

   This is the vtable which must be provided for custom packfiles, which then
   can read from and write to wherever you like (eg. files in memory).  You
   should provide all the entries of the vtable, even if they are empty stubs
   doing nothing, to avoid Allegro (or you) calling a NIL method at some point.
   @seealso(al_pack_fopen_vtable) *)
  AL_PACKFILE_VTABLE = RECORD
    pf_fclose: FUNCTION (userdata: POINTER): LONGINT; CDECL;
    pf_getc: FUNCTION (userdata: POINTER): LONGINT; CDECL;
    pf_ungetc: FUNCTION (c: LONGINT; userdata: POINTER): LONGINT; CDECL;
    pf_fread: FUNCTION (p: POINTER; n: LONGINT;  userdata: POINTER): LONGINT; CDECL;
    pf_putc: FUNCTION (c: LONGINT;  userdata: POINTER): LONGINT; CDECL;
    pf_fwrite: FUNCTION (p: POINTER; n: LONGINT;  userdata: POINTER): LONGINT; CDECL;
    pf_fseek: FUNCTION (userdata: POINTER; offset: LONGINT): LONGINT; CDECL;
    pf_feof: FUNCTION (userdata: POINTER): LONGINT; CDECL;
    pf_ferror: FUNCTION (userdata: POINTER): LONGINT; CDECL;
  END;


(* Sets the global I/O encryption password.

  Sets the encryption password to be used for all read/write operations on
  files opened in future using Allegro's packfile functions (whether they are
  compressed or not), including all the save, load and config routines.  Files
  written with an encryption password cannot be read unless the same password
  is selected, so be careful:  if you forget the key, nobody can make your data
  come back again!  Pass an empty string to return to the normal, non-encrypted
  mode.  If you are using this function to prevent people getting access to
  your datafiles, be careful not to store an obvious copy of the password in
  your executable:  if there are any strings like "I'm the password for the
  datafile", it would be fairly easy to get access to your data :-)

  @bold(Note #1:)  when writing a packfile, you can change the password to
  whatever you want after opening the file, without affecting the write
  operation.  On the contrary, when writing a sub-chunk of a packfile, you must
  make sure that the password that was active at the time the sub-chunk was
  opened is still active before closing the sub-chunk.  This is guaranteed to
  be true if you didn't call the @code(al_packfile_password) routine in the
  meantime.  Read operations, either on packfiles or sub-chunks, have no such
  restriction.

  @bold(Note #2:)  as explained above, the password is used for all read/write
  operations on files, including for several functions of the library that
  operate on files without explicitly using packfiles (e.g.
  @code(al_load_bitmap)).  The unencrypted mode is mandatory in order for those
  functions to work. Therefore remember to call @code(al_packfile_password
  @('')) before using them if you previously changed the password.  As a
  rule of thumb, always call @code(al_packfile_password @(''@)) when you are
  done with operations on packfiles.  The only exception to this is custom
  packfiles created with @code(al_pack_fopen_vtable).
  @seealso(al_pack_fopen) @seealso(al_load_datafile) *)
  PROCEDURE al_packfile_password (aPassword: STRING);



(* Opens a file according to mode, which may contain any of the flags:
   @unorderedList(
     @item(@code(r) - open file for reading.)
     @item(@code(w) - open file for writing, overwriting any existing data.)
     @item(@code(p) - open file in packed mode.  Data will be compressed as
       it is written to the file, and automatically uncompressed during read
       operations.  Files created in this mode will produce garbage if they are
       read without this flag being set.)
     @item(@code (!) - open file for writing in normal, unpacked mode, but add
       the value @code(AL_F_NOPACK_MAGIC) to the start of the file, so that it
       can later be opened in packed mode and Allegro will automatically detect
       that the data does not need to be decompressed.)
   )

  Instead of these flags, one of the constants @code(AL_F_READ),
  @code(AL_F_WRITE), @code(AL_F_READ_PACKED), @code(AL_F_WRITE_PACKED) or
  @code(AL_F_WRITE_NOPACK) may be used as the @code(mode) parameter.

  The packfile functions also understand several @italic("magic") filenames
  that are used for special purposes.  These are
  @bold(filename.dat#object_name) - open a specific object from a datafile, and
  read from it as if it was a regular file.  You can treat nested datafiles
  exactly like a normal directory structure, for example you could open
  @code(filename.dat#graphics/level1/mapdata).)

  With these special filenames, the contents of a datafile object or appended
  file can be read in an identical way to a normal disk file, so any of the
  file access functions in Allegro (eg. @code(al_set_config_file)) can be used
  to read from them.  Note that you can't write to these special files, though:
  the fake file is read only.  Also, you must save your datafile uncompressed
  or with per-object compression if you are planning on loading individual
  objects from it (otherwise there will be an excessive amount of seeking when
  it is read).

  Finally, be aware that the special Allegro object types aren't the same
  format as the files you import the data from.  When you import data like
  bitmaps or samples into the grabber, they are converted into a special
  Allegro-specific format, but the @code(`#') marker file syntax reads the
  objects as raw binary chunks.  This means that if, for example, you want
  to use @code(al_load_bitmap) to read an image from a datafile, you should
  import it as a binary block rather than as an @code(AL_BITMAP) object.

  @returns(on success, a pointer of type @code(AL_PACKFILEptr), and on error
    a @nil and stores an error code in @code(al_errno).  An attempt to read a
    normal file in packed mode will cause @code(al_errno) to be set to EDOM.)*)
  FUNCTION al_pack_fopen (filename, mode: STRING): AL_PACKFILEptr;



(* Creates a new packfile structure that uses the functions specified in the
   vtable instead of the standard functions.  The data pointer by `vtable' and
   `userdata' must remain available for the lifetime of the created packfile.

   While the created packfile structure can be used with other Allegro
   functions, there are two limitations.  First, opening chunks using
   @code(al_pack_fopen_chunk) on top of the returned packfile is not possible
   at this time.  And @code(al_packfile_password) does not have any effect on
   packfiles opened with @code(al_pack_fopen_vtable).
   @returns(on success,a pointer to a @code(AL_PACKFILEptr) structure, and on
     error it returns @nil and stores an error code in @code(al_errno).)
   @seealso(al_pack_fopen) *)
  FUNCTION al_pack_fopen_vtable (vtable: AL_PACKFILE_VTABLEptr; userdata: POINTER): AL_PACKFILEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fopen_vtable';



(* Closes the stream @code(f) previously opened with @code(al_pack_fopen).
   After you have closed the stream, performing operations on it will yield
   errors in your application (e.g. crash it) or even block your OS.

   @returns(zero on success or an error code which is also stored in
     @code(al_errno).  This function can fail only when writing to files:  if
     the file was opened in read mode, it will always succeed.) *)
  FUNCTION al_pack_fclose (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fclose';

(* Moves the position indicator of the stream @code(f).  This only supports
   forward movements relative to the current position and in read-only streams,
   so don't use negative offsets.  Note that seeking is very slow when reading
   compressed files, and so should be avoided unless you are sure that the file
   is not compressed.
   @returns (@true on success, @false on failure, storing the error code in
     @code(al_errno).) *)
  FUNCTION al_pack_fseek (f: AL_PACKFILEptr; offset: LONGINT): BOOLEAN;



(* Opens a sub-chunk of a file.  Chunks are primarily intended for use by the
  datafile code, but they may also be useful for your own file routines.  A
  chunk provides a logical view of part of a file, which can be compressed as
  an individual entity and will automatically insert and check length counts to
  prevent reading past the end of the chunk.  The @code(AL_PACKFILEptr)
  parameter is a previously opened file, and @code(Pack) is a boolean parameter
  which will turn compression on for the sub-chunk if it is non-zero. Example:
@longcode(#
VAR
  OutputFile: AL_PACKFILEptr;
BEGIN
  OutputFile := al_pack_fopen ('out.raw', 'w!');
      ...
  OutputFile := al_pack_fopen_chunk (OutputFile, TRUE);
  IF OutputFile = NIL THEN
    abort_on_error ('Error saving data!');
      ...
  OutputFile := al_pack_fclose_chunk (OutputFile);
#)
  The data written to the chunk will be prefixed with two length counts
  (32-bit, a.k.a. big-endian).  For uncompressed chunks these will both be set
  to the size of the data in the chunk.  For compressed chunks (created by
  setting the @code(Pack) flag), the first length will be the raw size of the
  chunk, and the second will be the negative size of the uncompressed data.

  To read the chunk, use the following code:
@longcode(#
VAR
  InputFile: AL_PACKFILEptr;
BEGIN
  InputFile := al_pack_fopen ('out.raw', 'rp');
      ...
  InputFile := al_pack_fopen_chunk (InputFile, TRUE);
      ...
  InputFile := al_pack_fclose_chunk (InputFile);
#)
  This sequence will read the length counts created when the chunk was written,
  and automatically decompress the contents of the chunk if it was compressed.
  The length will also be used to prevent reading past the end of the chunk
  (Allegro will return EOF if you attempt this), and to automatically skip past
  any unread chunk data when you call @code(al_pack_fclose_chunk).

  Chunks can be nested inside each other by making repeated calls to
  @code(al_pack_fopen_chunk).  When writing a file, the compression status is
  inherited from the parent file, so you only need to set the pack flag if the
  parent is not compressed but you want to pack the chunk data.  If the parent
  file is already open in packed mode, setting the pack flag will result in
  data being compressed twice:  once as it is written to the chunk, and again
  as the chunk passes it on to the parent file.
  @returns(a pointer to the sub-chunked @code(AL_PACKFILEptr), or @nil if there
   was some error @(eg. you are using a custom @code(AL_PACKFILEptr) vtable@).)
  @seealso(al_pack_fclose_chunk) @seealso(al_pack_fopen) *)
  FUNCTION al_pack_fopen_chunk (f: AL_PACKFILEptr; pack: BOOLEAN): AL_PACKFILEptr;
    INLINE;


(* Closes a sub-chunk of a file, previously obtained by calling
   @code(al_pack_fopen_chunk).  Returns a pointer to the parent of the
   sub-chunk you just closed.
   @returns(@nil if there was some error @(eg. you tried to close a
     @code(AL_PACKFILEptr) which wasn't sub-chunked@).)
   @seealso(al_pack_fopen_chunk) *)
  FUNCTION al_pack_fclose_chunk (f: AL_PACKFILEptr): AL_PACKFILEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fclose_chunk';



(* Finds out if you have reached the end of the file.  It does not wait for you
   to attempt to read beyond the end of the file, contrary to the ISO C
   @code(feof) function.  The only way to know whether you have read beyond the
   end of the file is to check the return value of the read operation you use
   (and be wary of @code(al_pack_*getl) as EOF is also a valid return value with
   these functions).
   @returns(@true if you are at the end of the file, @false otherwise.) *)
  FUNCTION al_pack_feof (f: AL_PACKFILEptr): BOOLEAN;

(* Since EOF is used to report errors by some functions, it's often better to
   use the @code(al_pack_feof) function to check explicitly for end of file
   and @code(al_pack_ferror) to check for errors.  Both functions check
   indicators that are part of the internal state of the stream to detect
   correctly the different situations.
   @returns(nonzero if the error indicator for the stream is set, meaning that
     an error has occurred during a previous operation on the stream.) *)
  FUNCTION al_pack_ferror (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_ferror';


(* Returns the next character from the stream @code(`f'), or EOF if the end of
   the file has been reached. *)
  FUNCTION al_pack_getc (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_getc';

(* Puts a character in the stream f.
   @returns(the character written on success, or EOF on error.) *)
  FUNCTION al_pack_putc (c: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_putc';

(* Like @code(al_pack_getc), but reads a 16-bit word from a file, using Intel
   byte ordering (least significant byte first, a.k.a. little-endian). *)
  FUNCTION al_pack_igetw (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_igetw';

(* Like @code(al_pack_getc), but reads a 32-bit word from a file, using Intel
   byte ordering (least significant byte first, a.k.a. little-endian). *)
  FUNCTION al_pack_igetl (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_igetl';

(* Like @code(al_pack_putc), but writes a 16-bit word to a file, using Intel
   byte ordering (least significant byte first, a.k.a. little-endian). *)
  FUNCTION al_pack_iputw (w: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_iputw';

(* Like @code(al_pack_putc), but writes a 32-bit word to a file, using Intel
   byte ordering (least significant byte first, a.k.a. little-endian). *)
  FUNCTION al_pack_iputl (l: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_iputl';

(* Like @code(al_pack_getc), but reads a 16-bit word from a file, using
   Motorola byte ordering (most significant byte first, a.k.a. big-endian). *)
  FUNCTION al_pack_mgetw (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mgetw';

(* Like @code(al_pack_getc), but reads a 32-bit word from a file, using
   Motorola byte ordering (most significant byte first, a.k.a. big-endian). *)
  FUNCTION al_pack_mgetl (f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mgetl';

(* Like @code(al_pack_putc), but writes a 16-bit word from a file, using
   Motorola byte ordering (most significant byte first, a.k.a. big-endian). *)
  FUNCTION al_pack_mputw (w: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mputw';

(* Like @code(al_pack_putc), but writes a 32-bit word from a file, using
   Motorola byte ordering (most significant byte first, a.k.a. big-endian). *)
  FUNCTION al_pack_mputl (l: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_mputl';

(* Reads @code(n) bytes from the stream @code(f), storing them at the memory
   location pointed to by @code(p).
   @returns(the number of bytes read, which will be less than @code(n) if EOF
     is reached or an error occurs.  Error codes are stored in
     @code(al_errno).) *)
  FUNCTION al_pack_fread (p: POINTER; n: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fread';

(* Writes @code(n) bytes from the stream @code(f), storing them at the memory
   location pointed to by @code(p).
   @returns(the number of bytes written, which will be less than @code(n) if
     EOF is reached or an error occurs.  Error codes are stored in
     @code(al_errno).) *)
  FUNCTION al_pack_fwrite (p: POINTER; n: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_fwrite';


(* Puts a character back to the file's input buffer.  Like with @code(ungetc)
   from libc, only a single push back is guaranteed.

   Note: @code(al_pack_fgets) internally uses @code(al_pack_ungetc), so never
   use @code(al_pack_ungetc) directly after using @code(al_pack_fgets) on a
   PACKFILE.
   @returns(@code(c) on success, or EOF on error.) *)
  FUNCTION al_pack_ungetc (c: LONGINT; f: AL_PACKFILEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'pack_ungetc';

(* Reads a line from the stream @code(f) and returns it.  Stops when a
   linefeed is encountered, or @code(max) bytes have been read.  The end of
   line is handled by detecting the right combination of characters for the
   platform.  This supports CR-LF (DOS/Windows), LF (Unix), and CR (Mac)
   formats.  However, the trailing carriage return is not included in the
   returned string, in order to provide easy code portability across platforms.
   If you need the carriage return, use @code(al_pack_fread) and/or
   @code(al_pack_getc) instead.

   Note:  This function internally may make calls to @code(al_pack_ungetc), so
   you cannot use @code(al_pack_ungetc) directly afterwards. *)
  FUNCTION al_pack_fgets (max: LONGINT; f: AL_PACKFILEptr): STRING;

(* Writes a string to the stream @code(f).  The input string is converted from
   the current text encoding format to UTF-8 before writing.  Newline
   characters are written as `\r\n' on DOS and Windows platforms.  If you don't
   want this behaviour, use @code(al_pack_fwrite) and/or @code(al_pack_putc)
   instead.
   @returns(@true on success or @false on error.) *)
  FUNCTION al_pack_fputs (p: STRING; f: AL_PACKFILEptr): BOOLEAN;



(****************************************
 * Semi-low-level object file functions *
 ****************************************)

(* A version of @code(al_load_bmp) which reads a BMP file from a packfile. *)
  FUNCTION al_load_bmp_pf (f: AL_PACKFILEptr; palette: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_bmp_pf';

(* A version of @code(al_load_lbm) which reads a BMP file from a packfile. *)
  FUNCTION al_load_lbm_pf (f: AL_PACKFILEptr; palette: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_lbm_pf';

(* A version of @code(al_load_pcx) which reads a BMP file from a packfile. *)
  FUNCTION al_load_pcx_pf (f: AL_PACKFILEptr; palette: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_pcx_pf';

(* A version of @code(al_load_pcx) which reads a BMP file from a packfile. *)
  FUNCTION al_load_tga_pf (f: AL_PACKFILEptr; palette: AL_PALETTEptr): AL_BITMAPptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_tga_pf';



(* A version of @code(al_save_bmp) which reads a BMP file from a packfile. *)
  FUNCTION al_save_bmp_pf (f: AL_PACKFILEptr; palette: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_bmp_pf';

(* A version of @code(al_save_pcx) which reads a BMP file from a packfile. *)
  FUNCTION al_save_pcx_pf (f: AL_PACKFILEptr; palette: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_pcx_pf';

(* A version of @code(al_save_pcx) which reads a BMP file from a packfile. *)
  FUNCTION al_save_tga_pf (f: AL_PACKFILEptr; palette: AL_PALETTEptr): LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'save_tga_pf';



(* A version of @code(al_load_wav) which reads from a packfile. *)
  FUNCTION al_load_wav_pf (f: AL_PACKFILEptr): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_wav_pf';

(* A version of @code(al_load_voc) which reads from a packfile. *)
  FUNCTION al_load_voc_pf (f: AL_PACKFILEptr): AL_SAMPLEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_voc_pf';



(**************
 * Data files *
 **************)

TYPE
{ @exclude }
  AL_DATAFILE_PROPERTYptr = ^AL_DATAFILE_PROPERTY;
  AL_DATAFILE_PROPERTY = RECORD
    dat : PCHAR;		{ pointer to the data  }
    ftype : LONGINT;		{ property type  }
  END;

  AL_DATAFILE_PROPERTY_LISTptr = ^AL_DATAFILE_PROPERTY_LIST;
  AL_DATAFILE_PROPERTY_LIST = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_DATAFILE_PROPERTY;

(* Pointer to @code(AL_DATAFILE_OBJECT). *)
  AL_DATAFILE_OBJECTptr = ^AL_DATAFILE_OBJECT;
(* Datafile object. *)
  AL_DATAFILE_OBJECT = RECORD
    dat : POINTER;		{< pointer to the data  }
    ftype : LONGINT;		{< object type  }
    size : LONGINT;		{< size of the object  }
    prop : AL_DATAFILE_PROPERTY_LISTptr; {< object properties  }
  END;

(* Pointer to @code(AL_DATAFILE). *)
  AL_DATAFILEptr = ^AL_DATAFILE;
(* Datafile content. *)
  AL_DATAFILE = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_DATAFILE_OBJECT;



VAR
(* Mnemonics for data object types.  You must assume this as constants.
   They're variables for technical reasons. *)
  AL_DAT_MAGIC, AL_DAT_FILE, AL_DAT_DATA, AL_DAT_FONT, AL_DAT_SAMPLE,
  AL_DAT_MIDI, AL_DAT_PATCH, AL_DAT_FLI, AL_DAT_BITMAP, AL_DAT_RLE_SPRITE,
  AL_DAT_C_SPRITE, AL_DAT_XC_SPRITE, AL_DAT_PALETTE, AL_DAT_PROPERTY,
  AL_DAT_NAME, AL_DAT_END: LONGINT;



(* Loads a datafile into memory in one go.  If the datafile contains truecolor
   graphics, you must set the video mode or call @code(al_set_color_conversion)
   before loading it.

   Remember to free this datafile to avoid memory leaks.
   @returns(a pointer to the @code(AL_DATAFILE), or @nil on error.)
   @seealso(grabber) *)
  FUNCTION al_load_datafile (filename: STRING): AL_DATAFILEptr;

(* Frees all the objects in a datafile.  Use this to avoid memory leaks in your
   program. *)
  PROCEDURE al_unload_datafile (dat: AL_DATAFILEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unload_datafile';



(* Loads a specific object from a datafile.  This won't work if you strip the
   object names from the file, and it will be very slow if you save the file
   with global compression. Example:
   @longcode(#
VAR
  MusicObject: AL_DATAFILE_OBJECTptr;
  ...
  MusicObject := al_load_datafile_object ('datafile.dat', 'MUSIC');
  al_play_midi (MusicObject^.dat);
  al_unload_datafile_object (MusicObject);
  #)
   Remember to free this DATAFILE later to avoid memory leaks, but use the
   correct unloading function!
   @returns(a pointer to a single @code(AL_DATAFILE_OBJECT) element whose
     @code(dat) member points to the object, or @nil if there was an error or
     there was no object with the requested name.)
   @seealso(grabber) *)
  FUNCTION al_load_datafile_object (filename, objectname: STRING): AL_DATAFILE_OBJECTptr;

(* Frees an object previously loaded by @code(al_load_datafile_object).  Use
   this to avoid memory leaks in your program. *)
  PROCEDURE al_unload_datafile_object (dat: AL_DATAFILE_OBJECTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unload_datafile_object';



IMPLEMENTATION

USES
  sysutils;



(***************
 * Packed file *
 ***************)

  PROCEDURE packfile_password (password: PCHAR); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;
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
  FUNCTION pack_fopen_chunk (f: AL_PACKFILEptr; pack: LONGINT): AL_PACKFILEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;



  PROCEDURE al_packfile_password (aPassword: STRING);
  BEGIN
    packfile_password (PCHAR (aPassword));
  END;

  FUNCTION al_pack_fopen (filename, mode: STRING): AL_PACKFILEptr;
  BEGIN
    al_pack_fopen := pack_fopen (PCHAR (filename), PCHAR (mode));
  END;

  FUNCTION al_pack_fseek (f: AL_PACKFILEptr; offset: LONGINT): BOOLEAN;
  BEGIN
    al_pack_fseek := (pack_fseek (f, offset) = 0);
  END;

  FUNCTION al_pack_fopen_chunk (f: AL_PACKFILEptr; pack: BOOLEAN): AL_PACKFILEptr;
  VAR
    P: INTEGER;
  BEGIN
    IF pack THEN P := -1 ELSE P := 0;
    al_pack_fopen_chunk := pack_fopen_chunk (f, P);
  END;

  FUNCTION al_pack_feof (f: AL_PACKFILEptr): BOOLEAN;
  BEGIN
    al_pack_feof := (pack_feof (f) <> 0)
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



(**************
 * Data files *
 **************)

  FUNCTION load_datafile (filename: PCHAR): AL_DATAFILEptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_datafile';

  FUNCTION al_load_datafile (filename: STRING): AL_DATAFILEptr;
  BEGIN
    al_load_datafile := load_datafile (PCHAR (filename));
  END;



  FUNCTION load_datafile_object (filename, objectname: PCHAR): AL_DATAFILE_OBJECTptr; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'load_datafile_object';

  FUNCTION al_load_datafile_object (filename, objectname: STRING): AL_DATAFILE_OBJECTptr;
  BEGIN
    al_load_datafile_object := load_datafile_object (PCHAR (filename), PCHAR (objectname));
  END;



INITIALIZATION
{ Generates identifiers for data objects. }
  AL_DAT_MAGIC		:= AL_ID ('ALL.');
  AL_DAT_FILE		:= AL_ID ('FILE');
  AL_DAT_DATA		:= AL_ID ('DATA');
  AL_DAT_FONT		:= AL_ID ('FONT');
  AL_DAT_SAMPLE		:= AL_ID ('SAMP');
  AL_DAT_MIDI		:= AL_ID ('MIDI');
  AL_DAT_PATCH		:= AL_ID ('PAT ');
  AL_DAT_FLI		:= AL_ID ('FLIC');
  AL_DAT_BITMAP		:= AL_ID ('BMP ');
  AL_DAT_RLE_SPRITE	:= AL_ID ('RLE ');
  AL_DAT_C_SPRITE	:= AL_ID ('CMP ');
  AL_DAT_XC_SPRITE	:= AL_ID ('XCMP');
  AL_DAT_PALETTE	:= AL_ID ('PAL ');
  AL_DAT_PROPERTY	:= AL_ID ('prop');
  AL_DAT_NAME		:= AL_ID ('NAME');
  AL_DAT_END		:= -1;
END.

