PROGRAM example;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

   Shows how to use Allegro.pas in a Lazarus GUI application.

   by Ñuño Martínez <niunio(at)users.sourceforge.net>

   This project has 3 files:
   * example.lpr:  The main file.
   * UnitMainForm:  The main window description.  It also implements Allegro's
                    initialization and finalization.
   * UnitBitmapInterface:  A helpful unit, it helps to integrate Allegro's
                           AL_BITMAP structure and LCL's TPaintBox and so.

   The project is configured to add debug and profile information in the
   executable, so it is big.  Go to the porject menu and change the
   configuration to deactivate debug and profile and activate optimizations to
   create a definitive smaller executable.

   Allegro initialization and finalization is done here to be sure it's done in
   the right order (Allegro must be initialized before any Allegro call and
   once finalized you shouldn't call any Allegro function or procedure).
 *)

{$mode objfpc}{$H+}

USES
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UnitMainForm, LResources, UnitBitmapInterface
  { you can add units after this }
  , allegro, LCLType;

{$IFDEF WINDOWS}{$R example.rc}{$ENDIF}

BEGIN
  Application.Title := 'The Allegro''s Lazarus example';
  {$I example.lrs}
  Application.Initialize;
{ Initializes Allegro. }
  IF NOT al_init THEN
    Application.MessageBox ('Can''t initialize Allegro',
      'Error', MB_ICONERROR)
  ELSE BEGIN
  { Avoid automatic color conversion when loading bitmpas.  This way the
    example shows how to deal with different color formats. }
    al_set_color_conversion (AL_COLORCONV_NONE);
  { Use a TRY .. FINALLY block to be sure Allegro is finalized correctly. }
    TRY
    { Try to initialize sound system. }
      IF NOT al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_AUTODETECT) THEN
        IF NOT al_install_sound (AL_DIGI_AUTODETECT, AL_MIDI_NONE) THEN
          Application.MessageBox ('Can''t initialize sound.' +#10+
            'The example will continue anyway', 'Warning', MB_ICONEXCLAMATION)
        ELSE
          Application.MessageBox ('No complete support for sound.' +#10+
            'The example will continue anyway', 'Warning', MB_ICONEXCLAMATION);
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    FINALLY
      al_exit;
    END;
  END;
END.

