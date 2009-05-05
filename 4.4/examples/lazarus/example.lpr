program example;
(*<Shows how to use Allegro.pas in Lazarus applications. *)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, UnitMainForm, LResources, allegro, Dialogs;

{$IFDEF WINDOWS}{$R example.rc}{$ENDIF}

begin
{ Initialize allgero. }
  IF al_install (AL_SYSTEM_NONE) THEN
  BEGIN
  {$I example.lrs}  Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  { Be sure to call this before exit! }
    al_exit;
  END
  ELSE
    MessageDlg ('Error', 'Error initializing Allegro.pas!', mtError, [mbAbort],0);
end.

