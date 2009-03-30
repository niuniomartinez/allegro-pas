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
    procedure DatafileContentExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
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

  (* Updates the window's caption. *)
    PROCEDURE UpdateCaption;
  (* Updates the datafile content. *)
    PROCEDURE UpdateDatafileContent;
  END;

VAR
  MainWindow: TMainWindow;

IMPLEMENTATION

USES
aldtfile,
  UnitProgressWindow;



CONST
(* Mnemonics for images. *)
  NDX_BINARY     = 5; { Raw binary data. }
  NDX_BITMAP     = 6;
  NDX_DATAFILE   = 9;
  NDX_FLIC       = 1; { FLI, FLC and FLIC animations. }
  NDX_FONT       = 2;
  NDX_MIDI       = 4;
  NDX_PALETTE    = 7;
  NDX_RLE_SPRITE = 0;
  NDX_SAMPLE     = 3; { Digital sound samples. }



(* Helper procedures to show progress. *)
VAR
  LoadingDatafile: BOOLEAN;
  TitleDatafile: STRING;

PROCEDURE PrepareProgress (NumItems: INTEGER);
BEGIN
  IF LoadingDatafile THEN
    TitleDatafile := 'Loading '+ExtractFileName (MainWindow.DataFileName)+'...'
  ELSE
    TitleDatafile := 'Savng '+ExtractFileName (MainWindow.DataFileName)+'...';
  ProgressWindow.Init (TitleDatafile, TitleDatafile, NumItems);
  ProgressWindow.Show;
  Application.ProcessMessages;
END;

PROCEDURE AdvanceProgress;
BEGIN
  ProgressWindow.AddOne (TitleDatafile);
  Application.ProcessMessages;
END;



{ TMainWindow }

PROCEDURE TMainWindow.FormCreate(Sender: TObject);
BEGIN
  DataFile := TDataFile.Create;
  DataFile.PreLoadCallback := @PrepareProgress;
  DataFile.LoadCallback := @AdvanceProgress;
  DataFile.PreSaveCallback := @PrepareProgress;
  DataFile.SaveCallback := @AdvanceProgress;
END;

procedure TMainWindow.DatafileContentExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
VAR
  Cnt: INTEGER;
begin
  AllowExpansion := TRUE;
{ Note the last one is "END". }
  FOR Cnt := 1 TO (DataFile.Count - 1) DO
  BEGIN
    Node.TreeNodes.AddChild (TTreeNode.Create (Node.TreeNodes), 'Child');
  END;
end;

PROCEDURE TMainWindow.FormDestroy(Sender: TObject);
BEGIN
  IF SELF.DataFile <> NIL THEN
    SELF.DataFile.Free;
END;



PROCEDURE TMainWindow.MenuItem_File_LoadClick(Sender: TObject);
BEGIN
  TRY
    LoadingDatafile := TRUE;
    IF OpenDialog.Execute THEN
    BEGIN
      SELF.DataFile.LoadFrom (OpenDialog.FileName);
      SELF.DataFileName := OpenDialog.FileName;
    END;
  EXCEPT
    ON Error: Exception DO
      MessageDlg ('Error loading datafile', 'Can''t load '+OpenDialog.FileName
                 +':'+chr(13)+Error.Message, mtError, [mbCancel], 0);
  END;
  ProgressWindow.Hide;
  SELF.UpdateCaption;
  SELF.UpdateDatafileContent;
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
  SELF.DataFileName := '';
  SELF.UpdateCaption;
  SELF.UpdateDatafileContent;
END;

PROCEDURE TMainWindow.MenuItem_File_SaveClick(Sender: TObject);
BEGIN
  TRY
    LoadingDatafile := FALSE;
  { If it wasn't saved yet, ask a name. }
    IF SELF.DataFileName = '' THEN
      SELF.MenuItem_File_Save_asClick (Sender)
    ELSE
      SELF.DataFile.SaveTo (SELF.DataFileName);
  EXCEPT
    ON Error: Exception DO
      MessageDlg ('Error saving datafile', 'Can''t write '+SELF.DataFileName
                 +':'+chr(13)+Error.Message, mtError, [mbCancel], 0);
  END;
  ProgressWindow.Hide;
  SELF.UpdateCaption;
END;

PROCEDURE TMainWindow.MenuItem_File_Save_asClick(Sender: TObject);
BEGIN
  TRY
    LoadingDatafile := FALSE;
    IF SaveDialog.Execute THEN
    BEGIN
      SELF.DataFile.LoadFrom (SaveDialog.FileName);
      SELF.DataFileName := SaveDialog.FileName;
    END;
  EXCEPT
    ON Error: Exception DO
      MessageDlg ('Error saving datafile', 'Can''t write '+SaveDialog.FileName
                 +':'+chr(13)+Error.Message, mtError, [mbCancel], 0);
  END;
  ProgressWindow.Hide;
  SELF.UpdateCaption;
END;



(* Updates the window's caption. *)
PROCEDURE TMainWindow.UpdateCaption;
VAR
  Tmp: STRING;
BEGIN
  Tmp := 'Grabber [';
  IF SELF.DataFileName <> '' THEN
    Tmp := Tmp + SELF.DataFileName + ']'
  ELSE
    Tmp := Tmp + '<unnamed>]';
  IF SELF.DataFile.Modified THEN
    Tmp := Tmp + ' (modified)';
  SELF.Caption := Tmp;
END;




(* Updates the datafile content. *)
PROCEDURE TMainWindow.UpdateDatafileContent;

  FUNCTION GetImageIndex (ObjectType: LONGINT): LONGINT;
  BEGIN
    IF ObjectType = AL_DAT_DATA THEN
      RESULT := NDX_BINARY
    ELSE IF ObjectType = AL_DAT_FLI THEN
      RESULT := NDX_FLIC
    ELSE IF ObjectType = AL_DAT_BITMAP THEN
      RESULT := NDX_BITMAP
    ELSE IF ObjectType = AL_DAT_FONT THEN
      RESULT := NDX_FONT
    ELSE IF ObjectType = AL_DAT_MIDI THEN
      RESULT := NDX_MIDI
    ELSE IF ObjectType = AL_DAT_PALETTE THEN
      RESULT := NDX_PALETTE
    ELSE IF ObjectType = AL_DAT_SAMPLE THEN
      RESULT := NDX_SAMPLE
    ELSE IF (ObjectType = AL_DAT_RLE_SPRITE)
         OR (ObjectType = AL_DAT_C_SPRITE)
         OR (ObjectType = AL_DAT_XC_SPRITE) THEN
      RESULT := NDX_RLE_SPRITE;
  END;

  FUNCTION GetTypeName (ObjectType: LONGINT): STRING;
  BEGIN
    IF ObjectType = AL_DAT_DATA THEN
      RESULT := 'BIN'
    ELSE IF ObjectType = AL_DAT_FLI THEN
      RESULT := 'FLIC'
    ELSE IF ObjectType = AL_DAT_BITMAP THEN
      RESULT := 'BMP'
    ELSE IF ObjectType = AL_DAT_FONT THEN
      RESULT := 'FONT'
    ELSE IF ObjectType = AL_DAT_MIDI THEN
      RESULT := 'MIDI'
    ELSE IF ObjectType = AL_DAT_PALETTE THEN
      RESULT := 'PAL'
    ELSE IF ObjectType = AL_DAT_SAMPLE THEN
      RESULT := 'SAMP'
    ELSE IF ObjectType = AL_DAT_RLE_SPRITE THEN
      Result := 'RLE'
    ELSE IF ObjectType = AL_DAT_C_SPRITE THEN
      RESULT := 'CSPR'
    ELSE IF ObjectType = AL_DAT_XC_SPRITE THEN
      RESULT := 'XSPR';
  END;

VAR
  Node, SubNode: TTreeNode;
  Cnt: INTEGER;
BEGIN
  WITH DatafileContent DO
  BEGIN
    Items.Clear;
    Node := TTreeNode.Create (Items);
    Items.Add (Node, '<root>');
    Node.ImageIndex := NDX_DATAFILE;
    Node.SelectedIndex := NDX_DATAFILE;
    Node.StateIndex := NDX_DATAFILE;
    Node.HasChildren := TRUE;
  { Note the last one is "END". }
    FOR Cnt := 1 TO (DataFile.Count - 1) DO
    BEGIN
      Node.TreeNodes.Add (TTreeNode.Create (Items), GetTypeName (DataFile.Item[Cnt-1].ObjectType));
    END;
  END;
END;



INITIALIZATION
  {$I UnitMainWindow.lrs}

END.

