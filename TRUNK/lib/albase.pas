UNIT albase;
(*< Base definitions to interface with Allegro and the Allegro.pas dynamic
 * modules. *)

{$INCLUDE allegro.cfg}

INTERFACE

CONST
(* @exclude
   Defines the name of the shared library in each supported system. *)
 {$IFDEF MSWINDOWS}
   ALLEGRO_SHARED_LIBRARY_NAME = 'alleg44.dll';
 {$ELSE}
   {$IFDEF UNIX}
     {$IFDEF DARWIN}
       {$ERROR Can't compile on MacOS X. }
     {$ELSE}
{ @exclude }
       ALLEGRO_SHARED_LIBRARY_NAME = 'liballeg.so.4.4';
     {$ENDIF}
   {$ELSE}
     {$ERROR Can't compile this platform. }
  (* TODO: Add support for MacOS X dynamic libraries. *)
   {$ENDIF}
 {$ENDIF}



(* @exclude
   To be used as size of Zero-Sized arrays.  Only useful for pointers. *)
  AL_UNKNOWN_SIZE = 1024;



(*
   Next are definitions of numeric data types.  We may use FPC's ctype unit,
   but Delphi doesn't has it so I prefer to do it by hand.

   First: it defines some integers with specific lenght.
   Then: it defines the types used by C declarations.
 *)

TYPE
(* Generic pointer. *)
  AL_POINTER = POINTER;

(* Signed 8bit integer values. *)
  AL_INT8 = SHORTINT;

(* Unsigned 8bit integer values. *)
  AL_UINT8 = BYTE;

(* Signed 16bit integer values. *)
  AL_INT16 = SMALLINT;

(* Unsigned 16bit integer values. *)
  AL_UINT16 = WORD;

(* Signed 32bit integer values. *)
  AL_INT32 = LONGINT;

(* Unsigned 32bit integer values. *)
  AL_UINT32 = LONGWORD;

(* Signed 64bit integer values. *)
  AL_INT64 = INT64;

(* Unsigned 64bit integer values. *)
  AL_UINT64 = QWORD;



(* Signed 8bit integer.

  Note that it isn't Pascal's CHAR type! *)
  AL_CHAR = AL_INT8;

(* Unsigned 8bit integer values. *)
  AL_UCHAR = AL_UINT8;

(* Signed 16bit integer values. *)
  AL_SHORT = AL_INT16;

(* Unsigned 16bit integer values. *)
  AL_USHORT = AL_UINT16;

(* Signed 32bit integer values. *)
  AL_INT = AL_INT32;

(* Unsigned 32bit integer values. *)
  AL_UINT = AL_UINT32;

{$IFDEF CPU64}
  {$IFDEF WINDOWS}
(* Signed 32/64bit integer values. *)
  AL_LONG = AL_INT32;

(* Unsigned 32/64bit integer values. *)
  AL_ULONG = AL_UINT32;
  {$ELSE}
(* Signed 32/64bit integer values. *)
  AL_LONG = AL_INT64;

(* Unsigned 32/64bit integer values. *)
  AL_ULONG = AL_UINT64;
  {$ENDIF}

(* size_t equivalent. *)
  AL_SIZE_T = AL_UINT64;

(* Fake pointer type.  It's needed because the need of pointer arithmetics in
  some inlined methods. *)
  AL_UINTPTR_T = AL_UINT64;

{$ELSE}
(* Signed 32/64bit integer values. *)
  AL_LONG = AL_INT32;

(* Unsigned 32/64bit integer values. *)
  AL_ULONG = AL_UINT32;

(* size_t equivalent. *)
  AL_SIZE_T = AL_UINT32;

(* Fake pointer type.  It's needed because the need of pointer arithmetics in
  some inlined methods. *)
  AL_UINTPTR_T = AL_UINT32;

{$ENDIF}

(* Float value. *)
  AL_FLOAT = SINGLE;

(* Double value. *)
  AL_DOUBLE = DOUBLE;



(* Pointer. *)
  AL_VOIDptr = AL_POINTER;

(* Special 8bit integer pointer. *)
  AL_UINT8ptr = ^AL_UINT8;

(* Pointer to signed 8bit integer values. *)
  AL_CHARptr = ^AL_CHAR;

(* Pointer to unsigned 8bit integer values. *)
  AL_UCHARptr = ^AL_UCHAR;

(* Pointer to unsigned 16bit integer values. *)
  AL_USHORTptr = ^AL_USHORT;

(* Special 16bit integer pointer. *)
  AL_UINT16ptr = ^AL_UINT16;

(* Special 32bit integer pointer. *)
  AL_UINT32ptr = ^AL_UINT32;

(* Pointer to signed 32bit integer values. *)
  AL_INTptr = ^AL_INT;

(* Pointer to signed 32bit integer values. *)
  AL_UINTptr = ^AL_UINT;

(* Pointer to float values. *)
  AL_FLOATptr = ^AL_FLOAT;

(* Pointer to text strings. *)
  AL_STRptr = PCHAR;



(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code(CDECL) since Allegro is
   written in C. *)
  AL_SIMPLE_PROC = PROCEDURE; CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code(CDECL) since Allegro is
   written in C. *)
  AL_PARAM_PROC  = PROCEDURE (x: AL_VOIDptr); CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code(CDECL) since Allegro is
   written in C. *)
  AL_INT_PROC	 = PROCEDURE (x: AL_INT); CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code(CDECL) since Allegro is
   written in C. *)
  AL_SIMPLE_FUNC = FUNCTION: AL_INT; CDECL;



IMPLEMENTATION

END.
