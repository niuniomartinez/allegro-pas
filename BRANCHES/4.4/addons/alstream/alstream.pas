UNIT alstream;
(*<Defines a TStream descendent that uses Allegro's AL_PACKFILE. *)

INTERFACE

  USES
    alfile, Classes;

  TYPE
  (* Defines a stream object that uses Allegro's AL_PACKFILE. *)
    TAllegroStream = CLASS (TStream)
    PRIVATE
    (* The PACKFILE. *)
      fPackfile: AL_PACKFILEptr;
    (* To know who created the packfile. *)
      fPackfileOwner: BOOLEAN;
    PROTECTED
    (* Returns EOF state. *)
      FUNCTION GetEOF: BOOLEAN;
    PUBLIC
    (* Opens a file stream. *)
      CONSTRUCTOR Create (CONST Filename, Mode: STRING); OVERLOAD;
    (* Creates a stream. *)
      CONSTRUCTOR Create (CONST aPackfile: AL_PACKFILEptr); OVERLOAD;
    (* Destructor.  If file was created by the stream, it's closed. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Sets the password.  Read description of al_packfile_password. *)
      PROCEDURE SetPassword (CONST aPassword: STRING);
    (* Implements the Seek method. *)
      FUNCTION Seek (Offset: LONGINT; Origin: WORD): LONGINT; OVERRIDE;
    (* Opens a sub-chunk of file.  Read al_pack_fopen_chunk description.
      @seealso(CloseChunk)*)
      PROCEDURE OpenChunk (Pack: BOOLEAN);
    (* Closes a sub-chunk of file. @seealso(OpenChunk) *)
      PROCEDURE CloseChunk;
    (* Reads data from the stream to a buffer and returns the number of bytes
      read.

      This method should be used when the number of bytes is not determined. If
      a specific number of bytes is expected, use @bold(ReadBuffer) instead.
      @param(Buffer Buffer to store the data read.)
      @param(Count Number of bytes to read.)
      @returns(the number of bytes actually read.)
      @seealso(Write) *)
      FUNCTION Read (VAR Buffer; Count: LONGINT): LONGINT; OVERRIDE;
    (* Writes data from a buffer to the stream and returns the actual number of bytes
      written.

      This method should be used when the number of bytes is not determined. If
      a specific number of bytes is expected, use @bold(WriteBuffer) instead.
      @param(Buffer Buffer that stores the data to write.)
      @param(Count Number of bytes to write.)
      @returns(the number of bytes actually written.)
      @seealso(Read) *)
      FUNCTION Write (CONST Buffer; Count: LONGINT): LONGINT; OVERRIDE;

    (* Returns the asociated @code(AL_PACKFILE) object. *)
      PROPERTY Packfile: AL_PACKFILEptr READ fPackfile;
    (* Returns EOF state. *)
      PROPERTY EOF: BOOLEAN READ GetEOF;
    (* If set to @true it closes the @code(AL_PACKFILE) when destroyed. *)
      PROPERTY PackfileOwner: BOOLEAN READ fPackfileOwner WRITE fPackfileOwner;
    END;

IMPLEMENTATION

  USES
    allegro; { Needed by al_error }

(*
 * TAllegroStream
 ****************************************************************************)

(* Returns EOF state. *)
  FUNCTION TAllegroStream.GetEOF: BOOLEAN;
  BEGIN
    RESULT := al_pack_feof (fPackfile)
  END;



(* Creates a stream. *)
  CONSTRUCTOR TAllegroStream.Create (CONST Filename, Mode: STRING);
  BEGIN
    INHERITED Create;
    fPackfile := al_pack_fopen (Filename, Mode);
    IF fPackfile = NIL THEN RAISE EStreamError.Create (al_error);
    fPackfileOwner := TRUE
  END;



(* Creates a stream. *)
  CONSTRUCTOR TAllegroStream.Create (CONST aPackfile: AL_PACKFILEptr);
  BEGIN
    INHERITED Create;
    fPackfile := aPackfile;
    fPackfileOwner := FALSE
  END;



(* Destructor.  If file was created by the stream, it's closed. *)
  DESTRUCTOR TAllegroStream.Destroy;
  BEGIN
    IF fPackfileOwner THEN al_pack_fclose (fPackfile);
    INHERITED Destroy
  END;



(* Sets the password.  Read description of al_packfile_password. *)
  PROCEDURE TAllegroStream.SetPassword (CONST aPassword: STRING);
  BEGIN
    al_packfile_password (aPassword)
  END;



(* Reimplements the Seek method. *)
  FUNCTION TAllegroStream.Seek (Offset: LONGINT; Origin: WORD): LONGINT;
  VAR
    Cnt: LONGINT;
    Value: BYTE;
  BEGIN
    IF Origin = soFromCurrent THEN
    BEGIN
      IF al_pack_fseek (fPackfile, Offset) THEN
	RESULT := Offset
      ELSE
	RESULT := -1
    END
    ELSE
      RAISE EStreamError.Create ('Allegro''s PACKFILE only supports soFromCurrent seeking.')
  END;



(* Opens a sub-chunk of file.  Read al_pack_fopen_chunk description. *)
  PROCEDURE TAllegroStream.OpenChunk (Pack: BOOLEAN);
  VAR
    Tmp: AL_PACKFILEptr;
  BEGIN
    Tmp := al_pack_fopen_chunk (fPackfile, Pack);
    IF Tmp = NIL THEN
      RAISE EStreamError.Create ('Error opening sub-chunk of a PACKFILE.');
    fPackfile := Tmp
  END;



(* Closes a sub-chunk of file. *)
  PROCEDURE TAllegroStream.CloseChunk;
  VAR
    Tmp: AL_PACKFILEptr;
  BEGIN
  { al_pack_fclose_chunk returns parent. }
    Tmp := al_pack_fclose_chunk (fPackfile);
    IF Tmp = NIL THEN
      RAISE EStreamError.Create ('Error closing sub-chunk of a PACKFILE.');
    fPackfile := Tmp
  END;



(* Reads data from the stream to a buffer and returns the number of bytes
  read. *)
  FUNCTION TAllegroStream.Read (VAR Buffer; Count: LONGINT): LONGINT;
  BEGIN
    RESULT := al_pack_fread (@Buffer, Count, fPackfile)
  END;



(* Writes data from a buffer to the stream and returns the actual number of bytes
  written. *)
  FUNCTION TAllegroStream.Write (CONST Buffer; Count: LONGINT): LONGINT;
  BEGIN
    RESULT := al_pack_fwrite (@Buffer, Count, fPackfile)
  END;

END.
