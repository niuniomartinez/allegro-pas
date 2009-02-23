(* Text fonts. *)
UNIT alfont;

{ Defines the frame. }
{$MODE Delphi}
{$PACKRECORDS C}
{$H+}



INTERFACE

USES
  albitmap, alpalete; { Needs some basic definitions. }



TYPE
  AL_FONTptr = POINTER; { Do you think we need it? }



  FUNCTION al_load_font (filename: STRING; palette: AL_PALETTEptr; p: POINTER)
	: AL_FONTptr;

VAR
  al_grab_font_from_bitmap: FUNCTION (bmp: AL_BITMAPptr): AL_FONTptr; CDECL;
  al_is_color_font: FUNCTION (f: AL_FONTptr): LONGINT; CDECL;
  al_is_mono_font: FUNCTION (f: AL_FONTptr): LONGINT; CDECL;
  al_is_compatible_font: FUNCTION (f1, f2: AL_FONTptr): LONGINT; CDECL;
  al_destroy_font: PROCEDURE (f: AL_FONTptr); CDECL;



IMPLEMENTATION

USES
  albase, alcolor;



VAR
  _load_font: FUNCTION (filename: PCHAR; palette: AL_PALETTEptr; p: POINTER)
	   : AL_FONTptr; CDECL;

  FUNCTION al_load_font (filename: STRING; palette: AL_PALETTEptr; p: POINTER)
	: AL_FONTptr;
  BEGIN
    al_load_font := _load_font (PCHAR (filename), palette, p);
  END;



INITIALIZATION
{ Gets function and procedure addess. }
  @al_grab_font_from_bitmap := al_get_object_address ('grab_font_from_bitmap');
  @al_is_color_font := al_get_object_address ('is_color_font');
  @al_is_mono_font := al_get_object_address ('is_mono_font');
  @al_is_compatible_font := al_get_object_address ('is_compatible_font');
  @al_destroy_font := al_get_object_address ('destroy_font');
  @_load_font := al_get_object_address ('load_font');
END.

