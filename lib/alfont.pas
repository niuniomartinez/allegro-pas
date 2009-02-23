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
  albase, alcolor, dynlibs;



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
  @al_grab_font_from_bitmap := GetProcAddress (__al_library_id__, 'grab_font_from_bitmap');
  @al_is_color_font := GetProcAddress (__al_library_id__, 'is_color_font');
  @al_is_mono_font := GetProcAddress (__al_library_id__, 'is_mono_font');
  @al_is_compatible_font := GetProcAddress (__al_library_id__, 'is_compatible_font');
  @al_destroy_font := GetProcAddress (__al_library_id__, 'destroy_font');
  @_load_font := GetProcAddress (__al_library_id__, 'load_font');
END.

