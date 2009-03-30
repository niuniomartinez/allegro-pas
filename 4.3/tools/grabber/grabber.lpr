program grabber;
(*< Helper application.  Creates and modifies DATA files you can load in your
    game using al_load_datafile. *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, UnitMainWindow, LResources;

{$IFDEF WINDOWS}{$R grabber.rc}{$ENDIF}

begin
  Application.Title := 'Grabber';
  {$I grabber.lrs}  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
end.

