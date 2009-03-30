UNIT aldtfile;
(*> Datafiles are created by the grabber utility (see grabber.txt for more
    information), and have a @code (.dat) extension.  They can contain bitmaps,
    palettes, fonts, samples, MIDI music, FLI/FLC animations, and any other
    binary data that you import.  You could distribute your bitmaps and samples
    in a myriad of separate files, but packing them in a few @code (.dat)
    binaries has a few advantages:
    @unorderedlist (
      @item (On some platforms loading a single big datafile at once is faster
	than loading individual resources one by one.)
      @item (Instead of several loops for your resources, you can write a
	single line of code with just a single point of failure to take care
	of.)
      @item (You can potentially reduce the size of your data by enabling
	compression on your datafiles.  Less download time for your end users,
	less wait during loading screens!)
      @item (If you don't need to load the whole datafile at once, you can
	still enable individual file compression.  It is slightly worse than
	global compression, but it is very fast with loading times because
	Allegro can easily seek inside the datafile to find a specific object.)
      @item (Even without encryption, most end users of your application won't
	be able to look at or modify the resources for your game.  A missing
	sound file or a modified bitmap could potentially crash the game if you
	haven't considered this in your loading code!)
      @item (It looks much more professional and convenient to distribute
	levels!  For example, if you found a bug in a level of your game, just
	distribute your new @code (level4.dat) and tell users to overwrite
	their old version. )
    )
    Allegro allows you to load datafiles once and forget about them.  But if
    you have many levels it can be wise to load only the resources required for
    the current level.  You can accomplish the later by separating levels in
    different datafiles, or using functions like @link
    (al_load_datafile_object) to avoid loading everything at once.  You can
    even read directly from a specific datafile object with the @link
    (al_pack_fopen) function.

    Remember that with Allegro truecolor images can only be loaded after you
    have set a graphics mode.  This is true for datafiles too.  Load all your
    data after you have set the graphics mode, otherwise the pixel format (RGB
    or BGR) will not be known and the datafile may be converted wrongly.

    @bold (Note:)  even though Allegro datafiles provide encryption, you should
    consider it weak, so don't plan on hiding there the plans for a Death Star
    or something.  Determinate knowledgeable users will be able to rip your
    resources no matter how hard you try to hide them!  Use the encryption only
    as a slight deterrent towards unwanted tampering of your data.  How to
    crack an encrypted datafile is left as an exercise to the reader, though. *)


{$H+}
{$IFDEF FPC}
{ Free Pascal. }
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}




INTERFACE

USES
  albase; { Needs some basic definitions. }



TYPE
{ @ignore }
  AL_DATAFILE_PROPERTYptr = ^AL_DATAFILE_PROPERTY;
  AL_DATAFILE_PROPERTY = RECORD
    dat : PCHAR;		{ pointer to the data  }
    ftype : LONGINT;		{ property type  }
  END;

  AL_DATAFILE_PROPERTY_LISTptr = ^AL_DATAFILE_PROPERTY_LIST;
  AL_DATAFILE_PROPERTY_LIST = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_DATAFILE_PROPERTY;

(* Pointer to @link (AL_DATAFILE_OBJECT). *)
  AL_DATAFILE_OBJECTptr = ^AL_DATAFILE_OBJECT;
(* Datafile object. *)
  AL_DATAFILE_OBJECT = RECORD
    dat : POINTER;		{< pointer to the data  }
    ftype : LONGINT;		{< object type  }
    size : LONGINT;		{< size of the object  }
    prop : AL_DATAFILE_PROPERTY_LISTptr; {< object properties  }
  END;

(* Pointer to @link (AL_DATAFILE). *)
  AL_DATAFILEptr = ^AL_DATAFILE;
(* Datafile content. *)
  AL_DATAFILE = ARRAY [0..AL_UNKNOWN_SIZE] OF AL_DATAFILE_OBJECT;



CONST
(* Mnemonics for data object types. *)
  AL_DAT_END = -1; (*< End of data file. *)
VAR
(* Mnemonics for data object types.  You must assume this as constants.
   They're variables for technical reasons. *)
  AL_DAT_MAGIC, AL_DAT_FILE, AL_DAT_DATA, AL_DAT_FONT, AL_DAT_SAMPLE,
  AL_DAT_MIDI, AL_DAT_PATCH, AL_DAT_FLI, AL_DAT_BITMAP, AL_DAT_RLE_SPRITE,
  AL_DAT_C_SPRITE, AL_DAT_XC_SPRITE, AL_DAT_PALETTE, AL_DAT_PROPERTY,
  AL_DAT_NAME: LONGINT;



(* Loads a datafile into memory in one go.  If the datafile contains truecolor
   graphics, you must set the video mode or call @link
   (al_set_color_conversion) before loading it.

   Remember to free this datafile to avoid memory leaks.
   @returns (a pointer to the @link (AL_DATAFILE), or @nil on error. *)
  FUNCTION al_load_datafile (filename: STRING): AL_DATAFILEptr;

(* Frees all the objects in a datafile.  Use this to avoid memory leaks in your
   program. *)
  PROCEDURE al_unload_datafile (dat: AL_DATAFILEptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unload_datafile';



(* Loads a specific object from a datafile.  This won't work if you strip the
   object names from the file, and it will be very slow if you save the file
   with global compression. Example:
   @longcode (#
VAR
  MusicObject: AL_DATAFILE_OBJECTptr;
  ...
  MusicObject := al_load_datafile_object ('datafile.dat', 'MUSIC');
  al_play_midi (MusicObject^.dat);
  al_unload_datafile_object (MusicObject);
  #)
   Remember to free this DATAFILE later to avoid memory leaks, but use the
   correct unloading function!
   @returns (a pointer to a single @link (AL_DATAFILE_OBJECT) element whose
     @code (dat) member points to the object, or @nil if there was an error or
     there was no object with the requested name. *)
  FUNCTION al_load_datafile_object (filename, objectname: STRING): AL_DATAFILE_OBJECTptr;

(* Frees an object previously loaded by @link (al_load_datafile_object).  Use
   this to avoid memory leaks in your program. *)
  PROCEDURE al_unload_datafile_object (dat: AL_DATAFILE_OBJECTptr); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'unload_datafile_object';



IMPLEMENTATION

USES
  alsystem;



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
END.

