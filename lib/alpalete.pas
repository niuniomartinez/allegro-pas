(* Color palette manipulation. *)
UNIT alpalete;

{ Defines the frame. }
{$MODE Delphi}
{$PACKRECORDS C}
{$H+}



INTERFACE

USES
  albase, alcolor;



CONST
  AL_PAL_SIZE = 256;



TYPE
(* Pointer to color palette. *)
  AL_PALETTEptr = ^AL_PALETTE;
(* Color palette description for indexed modes (8bpp). *)
  AL_PALETTE = ARRAY [0..AL_PAL_SIZE-1] OF AL_RGB;
(* Pointer to AL_RGB_MAP. *)
  AL_RGB_MAPptr = ^AL_RGB_MAP;
(* Speed up reducing RGB values to 8-bit paletted colors. *)
  AL_RGB_MAP = RECORD
    data: ARRAY [0..31, 0..31, 0..31] OF BYTE;
  END;


VAR
(* A palette containing solid black colors, used by the fade routines. *)
  al_black_palette,
(* The palette used by the Atari ST low resolution desktop. *)
  al_desktop_palette,
(* The default IBM BIOS palette. *)
  al_default_palette: AL_PALETTEptr;



(* Sets the specified palette entry to the specified AL_RGB triplet. *)
  al_set_color: PROCEDURE (idx: LONGINT; p: AL_RGBptr); CDECL;

(* Sets the entire palette of 256 colors. *)
  al_set_palette: PROCEDURE (p: AL_PALETTE); CDECL;

(* Sets the palette entries between from and to *)
  al_set_palette_range: PROCEDURE (p: AL_PALETTE; from, ato, retrace: LONGINT); CDECL;

(* Retrieves the specified palette entry. *)
  al_get_color: PROCEDURE (idx: LONGINT; p: AL_RGBptr); CDECL;

(* Retrieves the entire palette of 256 colors.  You should provide a pointer to
   an AL_PALETTE to store it in. *)
  al_get_palette: PROCEDURE (p: AL_PALETTEptr); CDECL;

(* Retrieves the palette entries between from and to (inclusive:  pass 0 and
   255 to get the entire palette). *)
  al_get_palette_range: PROCEDURE (p: AL_PALETTEptr; from, ato, retrace: LONGINT); CDECL;

(* Calculates a temporary palette part way between source and dest, returning
   it in the output parameter. *)
  al_fade_interpolate: PROCEDURE (spource, dest: AL_PALETTE; aoutput: AL_PALETTEptr; apos, from, ato: LONGINT); CDECL;

(* Gradually fades a part of the palette from the source palette to the dest
   palette. *)
  al_fade_from_range: PROCEDURE (source, dest: AL_PALETTE; speed, from, ato: LONGINT); CDECL;

(* Gradually fades a part of the palette from a black screen to the specified
   palette. *)
  al_fade_in_range: PROCEDURE (p: AL_PALETTE; speed, from, ato: LONGINT); CDECL;

(* Gradually fades a part of the palette from the current palette to a black
   screen. *)
  al_fade_out_range: PROCEDURE (speed, from, ato: LONGINT); CDECL;

(* Fades gradually from the source palette to the dest palette. *)
  al_fade_from: PROCEDURE (source, dest: AL_PALETTE; speed: LONGINT); CDECL;

(* Fades gradually from a black screen to the specified palette. *)
  al_fade_in: PROCEDURE (p: AL_PALETTE; speed: LONGINT); CDECL;

(* Fades gradually from the current palette to a black screen. *)
  al_fade_out: PROCEDURE (speed: LONGINT); CDECL;

(* Ugly hack for use in various dodgy situations where you need to convert
   between paletted and truecolor image formats. *)
  al_select_palette: PROCEDURE (p: AL_PALETTE); CDECL;

(* Restores the palette tables that were in use before the last call to
   al_select_palette. *)
  al_unselect_palette: PROCEDURE; CDECL;

(* Constructs a fake truecolor palette, using three bits for red and green and
   two for the blue. *)
  al_generate_332_palette: PROCEDURE (p: AL_PALETTEptr); CDECL;

(* Searches the specified palette for the closest match to the requested color,
   which are specified in the VGA hardware 0-63 format. *)
  al_bestfit_color: FUNCTION (pal: AL_PALETTE; r, g, b: LONGINT): LONGINT; CDECL;

(* Fills the specified RGB mapping table with lookup data for the specified
   palette. *)
  al_create_rgb_table: PROCEDURE (table: AL_RGB_MAPptr; pal: AL_PALETTE;
				 callback: AL_INT_PROC); CDECL;

(* Sets a look up table to speed up reducing RGB values to palette colors. *)
  PROCEDURE al_set_rgb_map (map: AL_RGB_MAPptr);



IMPLEMENTATION

VAR
  _RGB_map: ^AL_RGB_MAPptr;




(* Sets a look up table to speed up reducing RGB values to palette colors. *)
PROCEDURE al_set_rgb_map (map: AL_RGB_MAPptr);
BEGIN
  _RGB_map^ := map;
END;



INITIALIZATION
{ Loads Procedure Address. }
  @al_set_color := al_get_object_address ('set_color');
  @al_set_palette := al_get_object_address ('set_palette');
  @al_set_palette_range := al_get_object_address ('set_palette_range');
  @al_get_color := al_get_object_address ('get_color');
  @al_get_palette := al_get_object_address ('get_palette');
  @al_get_palette_range := al_get_object_address ('get_palette_range');
  @al_fade_interpolate := al_get_object_address ('fade_interpolate');
  @al_fade_from_range := al_get_object_address ('fade_from_range');
  @al_fade_in_range := al_get_object_address ('fade_in_range');
  @al_fade_out_range := al_get_object_address ('fade_out_range');
  @al_fade_from := al_get_object_address ('fade_from');
  @al_fade_in := al_get_object_address ('fade_in');
  @al_fade_out := al_get_object_address ('fade_out');
  @al_select_palette := al_get_object_address ('select_palette');
  @al_unselect_palette := al_get_object_address ('unselect_palette');
  @al_generate_332_palette := al_get_object_address ('generate_332_palette');
  @al_bestfit_color := al_get_object_address ('bestfit_color');
  @al_create_rgb_table := al_get_object_address ('create_rgb_table');
{ Variables. }
  al_black_palette := al_get_object_address ('black_palette');
  al_desktop_palette := al_get_object_address ('desktop_palette');
  al_default_palette := al_get_object_address ('default_palette');
  _RGB_map := al_get_object_address ('rgb_map');
END.

