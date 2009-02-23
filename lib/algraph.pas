(* Graphic initialization. *)
UNIT algraph;

{ Defines the frame. }
{$MODE Delphi}
{$PACKRECORDS C}
{$H+}



INTERFACE

USES
  albitmap;



CONST
(* Graphic modes *)
  AL_GFX_TEXT			= -1;
  AL_GFX_AUTODETECT		=  0;
  AL_GFX_AUTODETECT_FULLSCREEN	=  1;
  AL_GFX_AUTODETECT_WINDOWED	=  2;
  AL_GFX_SAFE			= $53414645; { AL_ID('S','A','F','E') }

(* Graphic capabilities *)
  AL_GFX_CAN_SCROLL			= $00000001;
  AL_GFX_CAN_TRIPLE_BUFFER		= $00000002;
  AL_GFX_HW_CURSOR			= $00000004;
  AL_GFX_HW_HLINE			= $00000008;
  AL_GFX_HW_HLINE_XOR			= $00000010;
  AL_GFX_HW_HLINE_SOLID_PATTERN		= $00000020;
  AL_GFX_HW_HLINE_COPY_PATTERN		= $00000040;
  AL_GFX_HW_FILL			= $00000080;
  AL_GFX_HW_FILL_XOR			= $00000100;
  AL_GFX_HW_FILL_SOLID_PATTERN		= $00000200;
  AL_GFX_HW_FILL_COPY_PATTERN		= $00000400;
  AL_GFX_HW_LINE			= $00000800;
  AL_GFX_HW_LINE_XOR			= $00001000;
  AL_GFX_HW_TRIANGLE			= $00002000;
  AL_GFX_HW_TRIANGLE_XOR		= $00004000;
  AL_GFX_HW_GLYPH			= $00008000;
  AL_GFX_HW_VRAM_BLIT			= $00010000;
  AL_GFX_HW_VRAM_BLIT_MASKED		= $00020000;
  AL_GFX_HW_MEM_BLIT			= $00040000;
  AL_GFX_HW_MEM_BLIT_MASKED		= $00080000;
  AL_GFX_HW_SYS_TO_VRAM_BLIT		= $00100000;
  AL_GFX_HW_SYS_TO_VRAM_BLIT_MASKED	= $00200000;
  AL_GFX_SYSTEM_CURSOR			= $00400000;



VAR
  al_gfx_capabilities: PLONGINT;

(* Screen bitmap *)
  al_screen: AL_BITMAPptr;
  AL_SCREEN_W, AL_SCREEN_H, AL_VIRTUAL_W, AL_VIRTUAL_H: LONGINT;



CONST
(* Define color conversion modes. *)
  AL_COLORCONV_NONE	= 0;

  AL_COLORCONV_8_TO_15	= 1;
  AL_COLORCONV_8_TO_16	= 2;
  AL_COLORCONV_8_TO_24	= 4;
  AL_COLORCONV_8_TO_32	= 8;

  AL_COLORCONV_15_TO_8	= $10;
  AL_COLORCONV_15_TO_16	= $20;
  AL_COLORCONV_15_TO_24	= $40;
  AL_COLORCONV_15_TO_32	= $80;

  AL_COLORCONV_16_TO_8	= $100;
  AL_COLORCONV_16_TO_15	= $200;
  AL_COLORCONV_16_TO_24	= $400;
  AL_COLORCONV_16_TO_32	= $800;

  AL_COLORCONV_24_TO_8	= $1000;
  AL_COLORCONV_24_TO_15	= $2000;
  AL_COLORCONV_24_TO_16	= $4000;
  AL_COLORCONV_24_TO_32	= $8000;

  AL_COLORCONV_32_TO_8	= $10000;
  AL_COLORCONV_32_TO_15	= $20000;
  AL_COLORCONV_32_TO_16	= $40000;
  AL_COLORCONV_32_TO_24	= $80000;

  AL_COLORCONV_32A_TO_8		= $100000;
  AL_COLORCONV_32A_TO_15	= $200000;
  AL_COLORCONV_32A_TO_16	= $400000;
  AL_COLORCONV_32A_TO_24	= $800000;

  AL_COLORCONV_DITHER_PAL	= $1000000;
  AL_COLORCONV_DITHER_HI	= $2000000;
  AL_COLORCONV_KEEP_TRANS	= $4000000;

  AL_COLORCONV_DITHER	= AL_COLORCONV_DITHER_PAL OR AL_COLORCONV_DITHER_HI;

  AL_COLORCONV_EXPAND_256	= AL_COLORCONV_8_TO_15 OR AL_COLORCONV_8_TO_16 OR AL_COLORCONV_8_TO_24 OR AL_COLORCONV_8_TO_32;

  AL_COLORCONV_REDUCE_TO_256	= AL_COLORCONV_15_TO_8 OR AL_COLORCONV_16_TO_8 OR AL_COLORCONV_24_TO_8 OR AL_COLORCONV_32_TO_8 OR AL_COLORCONV_32A_TO_8;

  AL_COLORCONV_EXPAND_15_TO_16	= AL_COLORCONV_15_TO_16;

  AL_COLORCONV_REDUCE_16_TO_15	= AL_COLORCONV_16_TO_15;

  AL_COLORCONV_EXPAND_HI_TO_TRUE = AL_COLORCONV_15_TO_24 OR AL_COLORCONV_15_TO_32 OR AL_COLORCONV_16_TO_24 OR AL_COLORCONV_16_TO_32;

  AL_COLORCONV_REDUCE_TRUE_TO_HI = AL_COLORCONV_24_TO_15 OR AL_COLORCONV_24_TO_16 OR AL_COLORCONV_32_TO_15 OR AL_COLORCONV_32_TO_16;

  AL_COLORCONV_24_EQUALS_32	= AL_COLORCONV_24_TO_32 OR AL_COLORCONV_32_TO_24;

  AL_COLORCONV_TOTAL	= AL_COLORCONV_EXPAND_256 OR AL_COLORCONV_REDUCE_TO_256 OR AL_COLORCONV_EXPAND_15_TO_16 OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_EXPAND_HI_TO_TRUE OR AL_COLORCONV_REDUCE_TRUE_TO_HI OR AL_COLORCONV_24_EQUALS_32 OR AL_COLORCONV_32A_TO_15 OR AL_COLORCONV_32A_TO_16 OR AL_COLORCONV_32A_TO_24;

  AL_COLORCONV_PARTIAL	= AL_COLORCONV_EXPAND_15_TO_16 OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_24_EQUALS_32;

  AL_COLORCONV_MOST	= AL_COLORCONV_EXPAND_15_TO_16  OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_EXPAND_HI_TO_TRUE OR AL_COLORCONV_REDUCE_TRUE_TO_HI OR AL_COLORCONV_24_EQUALS_32;

  AL_COLORCONV_KEEP_ALPHA	= AL_COLORCONV_TOTAL AND NOT (AL_COLORCONV_32A_TO_8 OR AL_COLORCONV_32A_TO_15 OR AL_COLORCONV_32A_TO_16 OR AL_COLORCONV_32A_TO_24);



VAR
(* Configure graphic mode. *)
  al_set_color_depth: PROCEDURE (depth: LONGINT); CDECL;
  al_get_color_depth: FUNCTION : LONGINT; CDECL;
  al_set_color_conversion: PROCEDURE  (mode: LONGINT); CDECL;
  al_get_color_conversion: FUNCTION : LONGINT; CDECL;
(* Waits for a retrace. *)
  al_vsync: PROCEDURE ; CDECL;

(* Configure graphic mode. *)
  FUNCTION al_set_gfx_mode (card, w, h, v_w, v_h: LONGINT): LONGINT;
(* Screen bitmap. *)
  FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
  PROCEDURE al_acquire_screen;
  PROCEDURE al_release_screen;



IMPLEMENTATION

USES
  albase;



VAR
  _screen: ^AL_BITMAPptr;
  _gfx_capabilities: PLONGINT;
  _set_gfx_mode: FUNCTION (card, w, h, v_w, v_h: LONGINT): LONGINT; CDECL;



FUNCTION al_set_gfx_mode (card, w, h, v_w, v_h: LONGINT): LONGINT;
VAR
  R: LONGINT;
BEGIN
  R := _set_gfx_mode (card, w, h, v_w, v_h);
  IF R = 0 THEN
  BEGIN
  { Get pointers of public variables and public values. }
    al_gfx_capabilities := _gfx_capabilities;
    al_screen := _screen^;
    IF al_screen <> NIL THEN
    BEGIN
      AL_SCREEN_W := w;
      AL_SCREEN_H := h;
      AL_VIRTUAL_W := al_screen^.w;
      AL_VIRTUAL_H := al_screen^.h;
    END;
  END;
  al_set_gfx_mode := R;
END;



FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
BEGIN
  al_is_screen_bitmap := al_is_same_bitmap (al_screen, bmp);
END;



PROCEDURE al_acquire_screen;
BEGIN
  IF al_screen <> NIL THEN
    al_screen^.vtable^.acquire (al_screen);
END;



PROCEDURE al_release_screen;
BEGIN
  IF al_screen <> NIL THEN
    al_screen^.vtable^.release (al_screen);
END;


INITIALIZATION
{ Get procedure and variable addess. }
  @al_set_color_depth := al_get_object_address ('set_color_depth');
  @al_get_color_depth := al_get_object_address ('get_color_depth');
  @al_set_color_conversion := al_get_object_address ('set_color_conversion');
  @al_get_color_conversion := al_get_object_address ('get_color_conversion');
  @al_vsync := al_get_object_address ('vsync');
  @_set_gfx_mode := al_get_object_address ('set_gfx_mode');
  _screen := al_get_object_address ('screen');
  _gfx_capabilities := al_get_object_address ('gfx_capabilities');
END.
