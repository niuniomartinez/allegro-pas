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



TYPE
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code(CDECL) since Allegro is
   written in C. *)
  AL_SIMPLE_PROC = PROCEDURE; CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code(CDECL) since Allegro is
   written in C. *)
  AL_PARAM_PROC  = PROCEDURE (x: POINTER); CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code(CDECL) since Allegro is
   written in C. *)
  AL_INT_PROC	 = PROCEDURE (x: LONGINT); CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code(CDECL) since Allegro is
   written in C. *)
  AL_SIMPLE_FUNC = FUNCTION: LONGINT; CDECL;



IMPLEMENTATION

END.
