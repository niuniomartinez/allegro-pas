UNIT Allegro5;
(*
  First attempt.
 *)

INTERFACE

CONST

  ALLEGRO_LIB_NAME = 'liballegro.so.5.0'; { TODO: Move it to "al5base.pas" }

  ALLEGRO_VERSION      =   5;
  ALLEGRO_SUB_VERSION  =   0;
  ALLEGRO_WIP_VERSION  =   5;

  ALLEGRO_RELEASE_NUMBER = 1;

  ALLEGRO_VERSION_INT  = (
		          (ALLEGRO_VERSION SHL 24)
		       OR (ALLEGRO_SUB_VERSION SHL 16)
		       OR (ALLEGRO_WIP_VERSION SHL  8)
		       OR  ALLEGRO_RELEASE_NUMBER
  );


  FUNCTION al_install_system (
    version: DWORD;
    atexit_ptr: POINTER
  ): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME NAME 'al_install_system';

  PROCEDURE al_uninstall_system; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME NAME 'al_uninstall_system';

  FUNCTION al_init: BOOLEAN;

  FUNCTION al_get_allegro_version: DWORD; CDECL;
  EXTERNAL ALLEGRO_LIB_NAME NAME 'al_get_allegro_version';

IMPLEMENTATION

  FUNCTION al_init: BOOLEAN;
  BEGIN
    al_init := al_install_system (ALLEGRO_VERSION_INT, NIL);
  END;

FINALIZATION
  al_uninstall_system;
END.
