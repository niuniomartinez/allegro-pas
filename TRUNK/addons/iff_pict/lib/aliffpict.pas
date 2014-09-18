UNIT alIffPict;
(*<Loads and saves IFF PICT files.

  Note that current implementation supports uncompressed indexed bitmaps only. *)
(*
  Copyright (c) 2014 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{ Using TStringList makes VARL parsing much easer! }
{$mode objfpc}{$H+}
INTERFACE

  USES
    Allegro, albase, alfile;

  TYPE
  (* Pixel format. *)
    TpictPixelFormat = (
    (* Indexed. *)
      pfIndexed,
    (* True color 8bpp. *)
      pfRGB8,
    (* True color 16bpp. *)
      pfRGB16,
    (* True color 24bpp. *)
      pfRGB24,
    (* RGBA. *)
      pfRGBA32
    );



  (* Compression algorithm. *)
    TpictCompression = (
    (* None. *)
      pcNone,
    (* Run-length. *)
      pcRLE,
    (* LZSS. *)
      pcLZSS,
    (* Deflate. *)
      pcDeflate
    );

  CONST
  (* IFF FORM identifier. *)
    ID_FORM = 1179603533;
  (* PICT FORM identifier. *)
    ID_PICT = 1346978644;
  (* VARL chunk identifier. *)
    ID_VARL = 1447121484;
  (* CMAP chunk identifier. *)
    ID_CMAP = 1129136464;
  (* BMP  chunk identifier. *)
    ID_BMP_ = 1112363040;

(* Loads a bitmap from a IFF PICT file. *)
  FUNCTION al_load_pict (CONST filename: STRING; pal: AL_PALETTEptr): AL_BITMAPptr;
(* Loads a bitmap from a IFF PICT file. *)
  FUNCTION al_load_pict_pf (f: AL_PACKFILEptr; pal: AL_PALETTEptr): AL_BITMAPptr;
(* Saves a bitmap to a IFF PICT file. *)
  FUNCTION al_save_pict (CONST filename: STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): BOOLEAN;
(* Saves a bitmap to a IFF PICT file. *)
  FUNCTION al_save_pict_pf (f: AL_PACKFILEptr; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): BOOLEAN;

(* Low-level function to load and parse VARL chunk.

  Note that file cursor should be at the chunk size position.  That is, call
  this function after loading and checking the chunk identifier.
  @return(Chunk size, excluding header and padding byte.) *)
  FUNCTION al_load_VARL_pf (f: AL_PACKFILEptr; OUT PixFormat: TpictPixelFormat;
    OUT Width, Height: INTEGER; OUT Compression: TpictCompression): AL_LONG;
(* Low-level function to load CMAP chunk.

  Note that file cursor should be at the chunk size position.  That is, call
  this function after loading and checking the chunk identifier.
  @return(Chunk size, excluding header and padding byte.) *)
  FUNCTION al_load_CMAP_pf (f: AL_PACKFILEptr; pal: AL_PALETTEptr): AL_LONG;
(* Low-level function to load BMP  chunk.

  Note that file cursor should be at the chunk size position.  That is, call
  this function after loading and checking the chunk identifier.
  @return(The bitmap loaded or @nil on error.) *)
  FUNCTION al_load_BMP__pf (f: AL_PACKFILEptr;
    CONST PixFormat: TpictPixelFormat; CONST Width, Height: INTEGER;
    CONST Compression: TpictCompression): AL_BITMAPptr;

(* Low-level function to save the CMAP chunk. *)
  FUNCTION al_save_CMAP_pf (f: AL_PACKFILEptr; Pal: AL_PALETTE): BOOLEAN;
(* Low-level function to save a BMP chunk. *)
  FUNCTION al_save_BMP_pf (f: AL_PACKFILEptr; Bmp: AL_BITMAPptr; CONST Compression: TpictCompression): BOOLEAN;

IMPLEMENTATION

  USES
    Classes, sysutils;

(* Helper function to write integer values. *)
  FUNCTION SaveInt (CONST Value: AL_LONG; pFile: AL_PACKFILEptr): BOOLEAN;
  BEGIN
    RESULT := al_pack_mputl (Value, pFile) = Value
  END;



(* Loads a bitmap from a IFF PICT file. *)
  FUNCTION al_load_pict (CONST filename: STRING; pal: AL_PALETTEptr): AL_BITMAPptr;
  VAR
    TheFile: AL_PALETTEptr;
  BEGIN
    TheFile := al_pack_fopen (filename, AL_F_READ);
    IF TheFile <> NIL THEN
    BEGIN
      RESULT := al_load_pict_pf (TheFile, pal);
      al_pack_fclose (TheFile)
    END
    ELSE
      RESULT := NIL
  END;



(* Loads a bitmap from a IFF PICT file. *)
  FUNCTION al_load_pict_pf (f: AL_PACKFILEptr; pal: AL_PALETTEptr): AL_BITMAPptr;
  VAR
    FormSize, ChunkSize: INTEGER;
    ChunkId: AL_LONG;
    PixFormat: TpictPixelFormat;
    Compression: TpictCompression;
    Width, Height: INTEGER;
  BEGIN
    FOR Width := 0 TO 255 DO
    BEGIN
      pal^[Width].R := al_default_palette[Width].R;
      pal^[Width].G := al_default_palette[Width].G;
      pal^[Width].B := al_default_palette[Width].B
    END;
  { Load FORM header }
    IF al_pack_mgetl (f) <> ID_FORM THEN EXIT (NIL);
    FormSize := al_pack_mgetl (f);
    IF FormSize < 12 THEN EXIT (NIL);
    IF al_pack_mgetl (f) <> ID_PICT THEN EXIT (NIL);
    { DEC (FormSize, 4); }
    ChunkId := al_pack_mgetl (f);
  { Check if VARL chunk exists. }
    IF ChunkId = ID_VARL THEN
    BEGIN
      ChunkSize := al_load_VARL_pf (f, PixFormat, Width, Height, Compression);
      IF ChunkSize < 1 THEN EXIT (NIL);
      {
      DEC (FormSize, 8 + ChunkSize);
      IF ChunkSize MOD 2 <> 0 THEN DEC (FormSize);
      IF FormSize < 8 THEN EXIT (NIL);
      }
    { Next chunk identifier. }
      ChunkId := al_pack_mgetl (f);
    END;
  { Check if CMAP chunk exists. }
    IF ChunkId = ID_CMAP THEN
    BEGIN
      ChunkSize := al_load_CMAP_pf (f, Pal);
      IF ChunkSize < 1 THEN EXIT (NIL);
    { Next chunk identifier. }
      ChunkId := al_pack_mgetl (f);
    END
    ELSE IF PixFormat = pfIndexed THEN
      EXIT (NIL);
  { Now, it must be a BMP  chunk. }
    IF ChunkId <> ID_BMP_ THEN EXIT (NIL);
    
    RESULT := al_load_BMP__pf (f, PixFormat, Width, Height, Compression)
  END;



(* Saves a bitmap to a IFF PICT file. *)
  FUNCTION al_save_pict (CONST filename: STRING; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): BOOLEAN;
  VAR
    TheFile: AL_PALETTEptr;
  BEGIN
    TheFile := al_pack_fopen (filename, AL_F_WRITE);
    IF TheFile <> NIL THEN
    BEGIN
      RESULT := al_save_pict_pf (TheFile, bmp, pal);
      al_pack_fclose (TheFile)
    END
    ELSE
      RESULT := FALSE
  END;



(* Saves a bitmap to a IFF PICT file. *)
  FUNCTION al_save_pict_pf (f: AL_PACKFILEptr; bmp: AL_BITMAPptr; pal: AL_PALETTEptr): BOOLEAN;
  VAR
    Varl: STRING;
    UsePalette: BOOLEAN;
    FormSize, VarlSize, CmapSize, BmpSize: INTEGER;
  BEGIN
  { Builds VARL chunk, and calculates BMP  chunk size. }
    CASE al_bitmap_color_depth (bmp) OF
    8:
      BEGIN
	Varl := 'pxlfmt=indexed';
	UsePalette := TRUE;
	BmpSize := bmp^.w * bmp^.h
      END;
    ELSE
    { At the moment, oly 8bpp supported. }
	EXIT (FALSE);
    END;
    Varl := Varl + #0'width=' + IntToStr (bmp^.w);
    Varl := Varl + #0'height=' + IntToStr (bmp^.h);
  { Calculate FORM chunk size, as packfiles doesn't allows seeking. }
    VarlSize := 8 + Length (Varl);
    IF VarlSize MOD 2 <> 0 THEN INC (VarlSize);

    IF UsePalette THEN CmapSize := 8 + (3 * 256) ELSE CmapSize := 0;

    INC (BmpSize, 8); { Header wasn't included. }
    IF BmpSize MOD 2 <> 0 THEN INC (BmpSize);

    FormSize := 4 + VarlSize + CmapSize + BmpSize;
  { Header. }
    IF NOT SaveInt (ID_FORM, f) THEN EXIT (FALSE);
    IF NOT SaveInt (FormSize, f) THEN EXIT (FALSE);
    IF NOT SaveInt (ID_PICT, f) THEN EXIT (FALSE);
  { VarlSize. }
    IF NOT SaveInt (ID_VARL, f) THEN EXIT (FALSE);
    IF NOT SaveInt (Length (Varl), f) THEN EXIT (FALSE);
    IF al_pack_fwrite (@Varl[1], Length (Varl), f) < Length (Varl) THEN EXIT (FALSE);
    IF Length (Varl) MOD 2 <> 0 THEN al_pack_putc (0, f);
  { CMAP }
    IF UsePalette THEN IF NOT al_save_CMAP_pf (f, Pal^) THEN EXIT (FALSE);
  { BMP }
    RESULT := al_save_BMP_pf (f, bmp, pcNone)
  END;



(* Low-level function to load and parse VARL chunk. *)
  FUNCTION al_load_VARL_pf (f: AL_PACKFILEptr; OUT PixFormat: TpictPixelFormat;
    OUT Width, Height: INTEGER; OUT Compression: TpictCompression): AL_LONG;
  VAR
    ChunkSize: LONGINT;
    VarlRawData: STRING;
    Parser: TStringList;
  BEGIN
    ChunkSize := al_pack_mgetl (f);
    VarlRawData := StringOfChar (' ', ChunkSize);
    IF al_pack_fread (PCHAR (VarlRawData), ChunkSize, f) <> ChunkSize THEN
	EXIT (-1);

    TRY
      Parser := TStringList.Create;
      Parser.Delimiter := #00;
      Parser.StrictDelimiter := TRUE;
      Parser.DelimitedText := Lowercase (VarlRawData);
      IF Parser.IndexOfName ('pixfmt') > -1 THEN
	IF Parser.Values ['pixfmt'] <> 'indexed' THEN EXIT (-1);
      PixFormat := pfIndexed;
      IF Parser.IndexOfName ('compression') > -1 THEN
	IF Parser.Values ['compression'] <> 'none' THEN EXIT (-1);
      Compression := pcNone;
      IF Parser.IndexOfName ('width') > -1 THEN
	Width := StrToInt (Parser.Values ['width'])
      ELSE
	Width := -1;
      IF Parser.IndexOfName ('height') > -1 THEN
	Height := StrToInt (Parser.Values ['height'])
      ELSE
	Height := -1;
    FINALLY
      FreeAndNil (Parser)
    END;

    RESULT := ChunkSize;
    IF ChunkSize MOD 2 <> 0 THEN ChunkSize := al_pack_getc (f);
  END;



(* Low-level function to load CMAP chunk. *)
  FUNCTION al_load_CMAP_pf (f: AL_PACKFILEptr; pal: AL_PALETTEptr): AL_LONG;
  VAR
    ChunkSize, Ndx: LONGINT;
  BEGIN
    ChunkSize := al_pack_mgetl (f);
    IF ChunkSize <> 256 * 3 THEN EXIT (-1);
    FOR Ndx := 0 TO 255 DO
    BEGIN
      Pal^[Ndx].R := al_pack_getc (f);
      Pal^[Ndx].B := al_pack_getc (f);
      Pal^[Ndx].G := al_pack_getc (f)
    END;
    RESULT := ChunkSize;
  END;



(* Low-level function to load BMP  chunk. *)
  FUNCTION al_load_BMP__pf (f: AL_PACKFILEptr;
    CONST PixFormat: TpictPixelFormat; CONST Width, Height: INTEGER;
    CONST Compression: TpictCompression): AL_BITMAPptr;
  VAR
    pX, pY, Pix: INTEGER;
  BEGIN
    IF PixFormat <> pfIndexed THEN EXIT (NIL);
    IF Compression <> pcNone THEN EXIT (NIL);

    RESULT := al_create_bitmap_ex (8, Width, Height);
    FOR pY := 0 TO Height - 1 DO
      FOR pX := 0 TO Width - 1 DO
      BEGIN
	Pix := al_pack_getc (f);
	IF (0 > Pix) OR (Pix > 255) THEN
	BEGIN
	  al_destroy_bitmap (RESULT);
	  EXIT (NIL)
	END
	ELSE
	  _al_putpixel (RESULT, pX, pY, Pix)
      END;
  { Byte padding. }
    IF (Width * Height) MOD 2 <> 0 THEN Pix := al_pack_getc (f)
  END;



(* Low-level function to save the CMAP chunk. *)
  FUNCTION al_save_CMAP_pf (f: AL_PACKFILEptr; Pal: AL_PALETTE): BOOLEAN;
  VAR
    Ndx: INTEGER;
  BEGIN
    IF NOT SaveInt (ID_CMAP, f) THEN EXIT (FALSE);
    IF NOT SaveInt (3 * 256, f) THEN EXIT (FALSE);
    FOR Ndx := 0 TO 255 DO
    BEGIN
      al_pack_putc (Pal[Ndx].R, f);
      al_pack_putc (Pal[Ndx].B, f);
      al_pack_putc (Pal[Ndx].G, f)
    END;
    RESULT := TRUE
  END;



(* Low-level function to save a BMP chunk. *)
  FUNCTION al_save_BMP_pf (f: AL_PACKFILEptr; Bmp: AL_BITMAPptr; CONST Compression: TpictCompression): BOOLEAN;
  VAR
    X, Y: INTEGER;
  BEGIN
    IF (Compression <> pcNone)
    OR (al_bitmap_color_depth (Bmp) <> 8)
    THEN EXIT (FALSE);

    IF NOT SaveInt (ID_BMP_, f) THEN EXIT (FALSE);
    IF NOT SaveInt (Bmp^.w * Bmp^.h, f) THEN  EXIT (FALSE);

    FOR Y := 0 TO Bmp^.h - 1 DO
      FOR X := 0 TO Bmp^.w - 1 DO
	al_pack_putc (_al_getpixel (Bmp, X, Y), f);
    IF (Bmp^.w * Bmp^.h) MOD 2 <> 0 THEN al_pack_putc (0, f);
    RESULT := TRUE
  END;

END.
