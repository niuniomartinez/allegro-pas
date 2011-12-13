PROGRAM test;
(*
  So far, this demonstrates tha it links and it seems to work.
 *)

USES
  Allegro5;



  FUNCTION mi_atexit (Ptr: POINTER): INTEGER; CDECL;
  BEGIN
    mi_atexit := 0;
  END;

VAR
  al_init_result: BOOLEAN;
BEGIN
  WriteLn ('Allegro version is ', al_get_allegro_version);
  WriteLn ('My Allegro.pas is ', ALLEGRO_VERSION_INT);
  al_init_result := al_init;
  WriteLn ('al_init returned => ', al_init_result);
  IF al_init_result THEN
    WriteLn ('Allegro 5 installed and initialised. :)')
  ELSE
    WriteLn ('Allegro 5 didn''t work! :(')
END.
