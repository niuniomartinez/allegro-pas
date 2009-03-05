UNIT albase;
(*< Base definitions to interface with Allegro and the Allegro.pas dynamic
 * modules. *)

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
  Windows;
 {$ENDIF}
{$ENDIF}



CONST
{ @exclude }
 {$IFDEF MSWINDOWS}
   ALLEGRO_SHARED_LIBRARY_NAME = 'alleg43.dll';
 {$ELSE}
   {$IFDEF UNIX}
     {$IFDEF DARWIN}
       {$ERROR Can't compile on MacOS X. }
     {$ELSE}
{ @exclude }
       ALLEGRO_SHARED_LIBRARY_NAME = 'liballeg-4.3.10.so';
     {$ENDIF}
   {$ELSE}
     {$ERROR Can't compile this platform. }
  (* TODO: Add support for MacOS X dynamic libraries. *)
   {$ENDIF}
 {$ENDIF}



{ To be used as size of Zero-Sized arrays. }
{ @exclude }
  AL_UNKNOWN_SIZE = 1024;



TYPE
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code (CDECL) since Allegro is
   written in C. *)
  AL_SIMPLE_PROC = PROCEDURE; CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code (CDECL) since Allegro is
   written in C. *)
  AL_PARAM_PROC  = PROCEDURE (x: POINTER); CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code (CDECL) since Allegro is
   written in C. *)
  AL_INT_PROC	 = PROCEDURE (x: LONGINT); CDECL;
(* This is used to define call-back parameter or pointers to call-back
   procedures.

   A call-back procedure must be declared as @code (CDECL) since Allegro is
   written in C. *)
  AL_SIMPLE_FUNC = FUNCTION: LONGINT; CDECL;



IMPLEMENTATION

END.

