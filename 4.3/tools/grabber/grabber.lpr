program grabber;
(*< Helper application.  Creates and modifies DATA files you can load in your
    game using al_load_datafile. *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Dialogs, allegro
  { you can add units after this }, UnitMainWindow, LResources,
  UnitProgressWindow;

{$IFDEF WINDOWS}{$R grabber.rc}{$ENDIF}

begin
{ Installs Allegro library without window. }
  IF al_install (AL_SYSTEM_NONE) THEN
  TRY
  { Set color conversion mode to "Don't change anything". }
    al_set_color_conversion (AL_COLORCONV_NONE);
  { Runs the application. }
    Application.Title := 'Grabber [<unnamed>]';
    {$I grabber.lrs}
    Application.CreateForm(TMainWindow, MainWindow);
    Application.CreateForm(TProgressWindow, ProgressWindow);
    Application.Run;
  FINALLY
  { Allways exits nice. }
    al_exit;
  END
  ELSE
    MessageDlg ('Error', 'Can''t initialize Allegro!', mtError, [mbCancel], 0);
end.

