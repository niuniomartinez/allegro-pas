UNIT UnitProgressWindow;
(*< A progress window. *)

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls;

TYPE

  { TProgressWindow }

  TProgressWindow = CLASS(TForm)
    ProgressBar: TProgressBar;
    StatusLabel: TLabel;
  PRIVATE
    { PRIVATE declarations }
  PUBLIC
  (* Prepare the window. *)
    PROCEDURE Init (aCaption, aMessage: STRING; Max: INTEGER);
  (* Adds one step. *)
    PROCEDURE AddOne (aText: STRING);
  END;



VAR
  ProgressWindow: TProgressWindow;

IMPLEMENTATION

{ TProgressWindow }

(* Prepare the window. *)
PROCEDURE TProgressWindow.Init (aCaption, aMessage: STRING; Max: INTEGER);
BEGIN
  SELF.Caption := aCaption;
  StatusLabel.Caption := aMessage;
  ProgressBar.Max := Max;
  ProgressBar.Step := 1;
  ProgressBar.Position := 0;
END;



(* Adds one step. *)
PROCEDURE TProgressWindow.AddOne (aText: STRING);
BEGIN
  StatusLabel.Caption := aText;
  ProgressBar.StepIt;
{ This method would be called a lot of times, mostly inside loops.  To process
  messages will prevent freeze the application and will free some CPU pressure
  too if we forget (or can't) to process messages on the loop itself. }
  Application.ProcessMessages;
END;



INITIALIZATION
  {$I UnitProgressWindow.lrs}

END.

