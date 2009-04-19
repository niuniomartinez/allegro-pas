PROGRAM exdata;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/
 *
 *	This program demonstrates how to access the contents of an Allegro
 *	datafile (created by the grabber utility).  The example loads the file
 *	`example.dat', then blits a bitmap and shows a font, both from this
 *	datafile.
 *	
 *	by Ñuño Martínez <>
 *	from an example of Allegro Game Library by Shawn Hargreaves.
 *
 *	See readme.txt for license and copyright information.
 *)

{$IFDEF FPC}
{ Free Pascal. }
  {$LONGSTRINGS ON}
{$ENDIF}

USES
  sysutils,
{ It needs some Allegro.pas units. }
  allegro,
  alfile,
  albltspr, { Image blitting and sprite drawing. }
  alcolor,  { Color manipulation. }
  altext,   { Text drawing. }
  algraph,  { Graphic mode configuration. }
  alpalete; { Color palette manipulation. }



{ The grabber produces a header, which contains defines for the names of all
  the objects in the datafile (BIG_FONT, SILLY_BITMAP, etc).  You can
  copnvert that header into Pascal code using the h2pas utility. }
{$I example.inc}



VAR
  datafile: AL_DATAFILEptr;
  palette: AL_PALETTEptr;
  buf: STRING;

BEGIN { The program starts here. }
  IF NOT al_init THEN
  BEGIN
    WriteLn ('Can''t initialize Allegro!');
    EXIT;
  END;
  al_install_keyboard;

{ Set a graphics mode sized 320x200. }
  IF NOT al_set_gfx_mode (AL_GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) THEN
    IF NOT al_set_gfx_mode (AL_GFX_SAFE, 320, 200, 0, 0) THEN
    BEGIN
      al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    { Show an error message. }
      al_message (al_error);
    { Shutdown Allegro. }
      al_exit;
      EXIT;
    END;

{ We still don't have a palette => Don't let Allegro twist colors. }
  al_set_color_conversion (AL_COLORCONV_NONE);

{ Load the datafile into memory. }
{ TODO: Find a better way to get the path. }
  buf := ExtractFilePath (ParamStr (0)) + 'example.dat';
  datafile := al_load_datafile (buf);
  IF datafile = NIL THEN
  BEGIN
    al_set_gfx_mode (AL_GFX_TEXT, 0, 0, 0, 0);
    al_message ('Error loading example.dat');
    al_exit;
    EXIT;
  END;

{ Select the palette which was loaded from the datafile. }
  palette := datafile^[THE_PALETTE].dat;
  al_set_palette (palette^);

{ Aha, set a palette and let Allegro convert colors when blitting. }
  al_set_color_conversion (AL_COLORCONV_TOTAL);
   
{ Display the bitmap from the datafile. }
  al_textout_ex (al_screen, al_font, 'This is the bitmap:', 32, 16,
		 al_makecol (255, 255, 255), -1); 
  al_blit (datafile^[SILLY_BITMAP].dat, al_screen, 0, 0, 64, 32, 64, 64);

{ And use the font from the datafile. }
  al_textout_ex (al_screen, datafile^[BIG_FONT].dat, 'And this is a big font!',
	      32, 128, al_makecol (0, 255, 0), -1);

  al_readkey;

{ Unload the datafile when we are finished with it. }
  al_unload_datafile (datafile);

{ Shutdown Allegro. }
  al_exit;

{ End of the program. }
END.

