PROGRAM ex_monitorinfo;
(*
  Copyright (c) 2012-2019 Guillermo Martínez J.

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

  USES
    Allegro5,
    Common;

  VAR
   Info: ALLEGRO_MONITOR_INFO;
   NumAdapters, i, j: INTEGER;
   Mode: ALLEGRO_DISPLAY_MODE;
BEGIN
  IF NOT al_init THEN AbortExample ('Could not init Allegro.');

   OpenLog;

   NumAdapters := al_get_num_video_adapters;

   LogPrintLn ('%d adapters found...', [NumAdapters]);

   FOR i := 1 TO NumAdapters DO
   BEGIN
     al_get_monitor_info (i - 1, Info);
     LogPrintLn (
       'Adapter %d: (%d, %d) - (%d, %d)',
       [i - 1, info.x1, info.y1, info.x2, info.y2]
     );
     al_set_new_display_adapter (i - 1);
     LogWriteLn ('   Available fullscreen display modes:');
     FOR j := 1 TO al_get_num_display_modes DO
     BEGIN
       al_get_display_mode (j - 1, Mode);
       LogPrintLn (
         '   Mode %3d: %4d x %4d, %d Hz',
         [j - 1, Mode.width, Mode.height, Mode.refresh_rate]
       )
     END
   END;

   CloseLog (TRUE)
END.
