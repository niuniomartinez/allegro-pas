{ Allegro 4.3.x }
PROGRAM test;

{$H+}

CONST
(* Can't use this version because Free Pascall looks for '-lalleg' and doesn't
   find it!
  libname = 'liballeg.so.4.3';

   This one works without problems but may be don't works on vewer Allegro
   versions. *)
  libname = 'liballeg-4.3.10.so';

FUNCTION  _install_allegro_version_check (
	system_id: LONGINT; errno_ptr: PLONGINT; atexit_ptr: POINTER;
	version: LONGINT): LONGINT; CDECL;
EXTERNAL libname;

PROCEDURE allegro_message (CONST text_format: PCHAR); CDECL;
EXTERNAL libname;

PROCEDURE allegro_exit; CDECL;
EXTERNAL libname;



VAR
  ErrorVar: LONGINT;
BEGIN
  _install_allegro_version_check (0, @ErrorVar, NIL,
				  (4 SHL 16) OR (3 SHL 8) OR 10);
  allegro_message ('Hello, World!');
  allegro_exit;
END.
