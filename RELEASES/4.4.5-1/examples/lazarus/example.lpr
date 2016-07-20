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
   * UnitMainForm:  The main window.
   * UnitBitmapInterface:  A helpful unit, it helps to integrate Allegro's
                           AL_BITMAP structure and LCL's TPaintBox and so.

   The project is configured to add debug and profile information in the
   executable, so it is big.  Go to the project menu and change the
   configuration to deactivate debug and profile and activate optimizations to
   create a definitive smaller executable.

   Allegro initialization and finalization is done here to be sure it's done in
   the right order (Allegro must be initialized before any Allegro call and
   once finalized you shouldn't call any Allegro function or procedure).
 *)

{$mode objfpc}{$H+}

USES
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  Cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UnitMainForm,
{ We add Allegro and LCLType. }
  Allegro, LCLType;

VAR
(* Result of Allegro initialisation. *)
  RS: BOOLEAN;

{$R *.res}

BEGIN
  Application.Title := 'The Allegro.pas Lazarus example';
  RequireDerivedFormResource := True;
{ Initialises Allegro, avoiding interferences.
  As we call al_install using "AL_SYSTEM_NONE" we can't use timers, keyboard,
  graphics modes, joysticks, sound, etc.  If you try to initialize them then
  it will throw a segmentation fault exception or just doesn't do nothing. }
  RS := al_install (AL_SYSTEM_NONE);
{ Initialises application. }
  Application.Initialize;
{ If Allegro did initialised, we follow normally. }
  IF RS THEN
  BEGIN
    Application.CreateForm(TForm1, Form1);
  { Avoids automatic color conversion when loading bitmaps.  This way the
    example shows how to deal with different color formats. }
    al_set_color_conversion (AL_COLORCONV_NONE);
  { Execute the application. }
    Application.Run
  END
  ELSE
  { Otherwise we show a message. }
    Application.MessageBox
      ('Can''t initialize Allegro', 'Error', MB_ICONERROR)
END.

