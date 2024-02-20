program ex_convert;
(* Image conversion example.

   MUST be compiled as a console application. *)
(*
  Copyright (c) 2012-2023 Guillermo Martínez J.

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
    Allegro5, al5image, al5strings;

  var
    Bitmap: ALLEGRO_BITMAPptr;
    t0, t1: Double;

begin
  if ParamCount < 2 then
  begin
    WriteLn ('This example needs to be run from the command line.');
    WriteLn ('Usage: ex_convert <infile> <outfile>');
    WriteLn ('    Possible file types: BMP PCX PNG TGA.');
    WriteLn ('    It may support more file formats.');
    WriteLn;
    Exit
  end;
{ Initialize Allegro. }
  if not al_init then
  begin
    WriteLn (stderr, 'Could not init Allegro.');
    Halt (1)
  end;
  al_init_image_addon;
{ Set bitmap options to work without a display. }
  al_set_new_bitmap_format (ALLEGRO_PIXEL_FORMAT_ARGB_8888);
  al_set_new_bitmap_flags (ALLEGRO_MEMORY_BITMAP);
{ Load the bitmap. }
  Bitmap := al_load_bitmap_flags (
    al_string_to_str (ParamStr (1)),
    ALLEGRO_NO_PREMULTIPLIED_ALPHA
  );
  if not Assigned (Bitmap) then
  begin
    WriteLn (stderr, 'Error loading input file');
    Halt (-2)
  end;
{ Save the bitmap. }
  t0 := al_get_time;
  if not al_save_bitmap (al_string_to_str (ParamStr (2)), Bitmap) then
  begin
    WriteLn (stderr, 'Error saving bitmap.');
    Halt (-3)
  end;
{ Just for fun. }
  t1 := al_get_time;
  WriteLn (al_str_to_string (al_str_format ('Saving took %.4f seconds', [t1 - t0])));
{ Close. }
  al_destroy_bitmap (Bitmap)
end.
