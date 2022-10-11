program ex_convert;
(* Image conversion example *)
(*
  Copyright (c) 2012-2020 Guillermo Martínez J.

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

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

  uses
    Common,
    Allegro5, al5image, al5strings;

  var
    Bitmap: ALLEGRO_BITMAPptr;
    t0, t1: Double;

begin
  if not al_init then AbortExample ('Could not init Allegro.');

  OpenLog;

  if ParamCount < 2 then
  begin
    LogWriteLn ('This example needs to be run from the command line.');
    LogWriteLn ('Usage: ex_convert <infile> <outfile>');
    LogWriteLn ('    Possible file types: BMP PCX PNG TGA');
    CloseLog (True);
    HALT
  end;

  al_init_image_addon;

  al_set_new_bitmap_format (ALLEGRO_PIXEL_FORMAT_ARGB_8888);
  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);

  Bitmap := al_load_bitmap_flags (
    al_string_to_str (ParamStr (1)),
    ALLEGRO_NO_PREMULTIPLIED_ALPHA
  );
  if Bitmap = Nil then
  begin
    LogWriteLn ('Error loading input file');
    CloseLog (True);
    HALT
  end;

  t0 := al_get_time;
  if not al_save_bitmap (al_string_to_str (ParamStr (2)), Bitmap) then
  begin
    LogWriteLn ('Error saving bitmap');
    CloseLog (True);
    HALT
  end;
  t1 := al_get_time;
  LogPrintLn ('Saving took %.4f seconds', [t1 - t0]);

  al_destroy_bitmap (Bitmap);

  CloseLog (True)
end.
