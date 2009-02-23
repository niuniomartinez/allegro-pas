(* Text drawing. *)
UNIT altext;

{ Defines the frame. }
{$MODE Delphi}
{$PACKRECORDS C}
{$H+}



INTERFACE

USES
  albitmap, alfont; { Needs some basic definitions. }



VAR
  al_font: ^AL_FONTptr; { A pointer to a pointer because we can change it. }
  al_404_char: PLONGINT;



  al_text_height: FUNCTION (f: AL_FONTptr): LONGINT; CDECL;
  PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
  PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x1, x2, y, diff, color, bg: LONGINT);

  FUNCTION al_text_length (f: AL_FONTptr; str: STRING): LONGINT;



IMPLEMENTATION

USES
  albase, dynlibs;



VAR
  textout_ex: PROCEDURE (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
  textout_centre_ex: PROCEDURE (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
  textout_right_ex: PROCEDURE (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x, y, color, bg: LONGINT); CDECL;
  textout_justify_ex: PROCEDURE (bmp: AL_BITMAPptr; f: AL_FONTptr; str: PCHAR; x1, x2, y, diff, color, bg: LONGINT); CDECL;
  text_length: FUNCTION (f: AL_FONTptr; str: PCHAR): LONGINT; CDECL;



PROCEDURE al_textout_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
BEGIN
  textout_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;



PROCEDURE al_textout_centre_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
BEGIN
  textout_centre_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;



PROCEDURE al_textout_right_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x, y, color, bg: LONGINT);
BEGIN
  textout_right_ex (bmp, f, PCHAR (str), x, y, color, bg);
END;



PROCEDURE al_textout_justify_ex (bmp: AL_BITMAPptr; f: AL_FONTptr; str: STRING; x1, x2, y, diff, color, bg: LONGINT);
BEGIN
  textout_justify_ex (bmp, f, PCHAR (str), x1, x2, y, diff, color, bg);
END;



FUNCTION al_text_length (f: AL_FONTptr; str: STRING): LONGINT;
BEGIN
  al_text_length := text_length (f, PCHAR (str));
END;



INITIALIZATION
{ Gets function and procedure address. }
  @al_text_height := GetProcAddress (__al_library_id__, 'text_height');
  @textout_ex := GetProcAddress (__al_library_id__, 'textout_ex');
  @textout_centre_ex := GetProcAddress (__al_library_id__, 'textout_centre_ex');
  @textout_right_ex := GetProcAddress (__al_library_id__, 'textout_right_ex');
  @textout_justify_ex := GetProcAddress (__al_library_id__, 'textout_justify_ex');
  @text_length := GetProcAddress (__al_library_id__, 'text_length');
{ Gets variable address. }
  al_font := GetProcAddress (__al_library_id__, 'font');
  al_404_char := GetProcAddress (__al_library_id__, 'allegro_404_char');
END.
