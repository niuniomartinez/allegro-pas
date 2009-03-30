UNIT UnitMainWindow;
(*< Main window of the application.  Has the main menu and shows the content of
    the file. *)

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  UnitDataFile, Menus, ComCtrls;

TYPE

  { TMainWindow }

  TMainWindow = CLASS(TForm)
    DatafileImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItem_File__1: TMenuItem;
    MenuItem_File_New: TMenuItem;
    MenuItem_File_Save: TMenuItem;
    MenuItem_File_Save_as: TMenuItem;
    MenuItem_File_Load: TMenuItem;
    MenuItem_File: TMenuItem;
    OpenDialog: TOpenDialog;
    DatafileContent: TTreeView;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE MenuItem_File_LoadClick(Sender: TObject);
    PROCEDURE MenuItem_File_NewClick(Sender: TObject);
    PROCEDURE MenuItem_File_SaveClick(Sender: TObject);
    PROCEDURE MenuItem_File_Save_asClick(Sender: TObject);
  PRIVATE
  (* The datafile we'll edit. *)
    DataFileName: STRING;
    DataFile: TDataFile;
  END;

VAR
  MainWindow: TMainWindow;

IMPLEMENTATION

{ TMainWindow }

PROCEDURE TMainWindow.FormCreate(Sender: TObject);
BEGIN
  SELF.DataFile := TDataFile.Create;
END;

PROCEDURE TMainWindow.FormDestroy(Sender: TObject);
BEGIN
  IF SELF.DataFile <> NIL THEN
    SELF.DataFile.Free;
END;

PROCEDURE TMainWindow.MenuItem_File_LoadClick(Sender: TObject);
BEGIN
  TRY
    IF OpenDialog.Execute THEN
    BEGIN
      SELF.DataFile.LoadFrom (OpenDialog.FileName);
      SELF.DataFileName := OpenDialog.FileName;
      SELF.Caption := 'Grabber ['+ExtractFileName (SELF.DataFileName)+']';
    END;
  EXCEPT
    ON Error: Exception DO
      MessageDlg ('Error loading datafile', 'Can''t load '+OpenDialog.FileName,
                  mtError, [mbCancel], 0);
  END;
END;

PROCEDURE TMainWindow.MenuItem_File_NewClick(Sender: TObject);
VAR
  Response: TModalResult;
BEGIN
{ If datafile was modified, ask if save it. }
  IF DataFile.Modified THEN
  BEGIN
    Response := MessageDlg ('Modified datafile', 'Datafile was modified. Save it?',
              mtConfirmation, (mbYesNoCancel), 0);
    IF Response = mrCancel THEN
      EXIT;
    IF Response = mrYes THEN
      SELF.MenuItem_File_SaveClick (Sender);
  END;
  SELF.DataFile.Clean;
END;

PROCEDURE TMainWindow.MenuItem_File_SaveClick(Sender: TObject);
BEGIN
  TRY
  { If it wasn't saved yet, ask a name. }
    IF SELF.DataFileName = '' THEN
      SELF.MenuItem_File_Save_asClick (Sender)
    ELSE
      SELF.DataFile.SaveTo (SELF.DataFileName);
  EXCEPT
    ON Error: Exception DO
      MessageDlg ('Error saving datafile', 'Can''t write '+SELF.DataFileName,
                  mtError, [mbCancel], 0);
  END;
END;

PROCEDURE TMainWindow.MenuItem_File_Save_asClick(Sender: TObject);
BEGIN
  TRY
    IF SaveDialog.Execute THEN
    BEGIN
      SELF.DataFile.LoadFrom (SaveDialog.FileName);
      SELF.DataFileName := SaveDialog.FileName;
      SELF.Caption := 'Grabber ['+ExtractFileName (SELF.DataFileName)+']';
    END;
  EXCEPT
    ON Error: Exception DO
      MessageDlg ('Error saving datafile', 'Can''t write '+SaveDialog.FileName,
                  mtError, [mbCancel], 0);
  END;
END;

INITIALIZATION
  {$I UnitMainWindow.lrs}

END.

