UNIT UnitMainForm; 
(*<Main window.

  IMPORTANT: See the LPR file to know how to initialize and finalize Allegro!

  It allows to load or create a bitmap and show it in the window. *)
{$mode objfpc}{$H+}

INTERFACE

USES
  allegro,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls;

TYPE

  { TMainForm }

  TMainForm = CLASS(TForm)
    MainMenu: TMainMenu;
    MenuItem_About: TMenuItem;
    MenuItem_Create: TMenuItem;
    MenuItem_Bitmap: TMenuItem;
    MenuItem_Load: TMenuItem;
    OpenDialog: TOpenDialog;
    PaintBox: TPaintBox;
  (* Load a bitmap. *)
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE MenuItem_LoadClick(Sender: TObject);
  PRIVATE
    fBitmap: AL_BITMAPptr;
  PUBLIC

  END; 

VAR
  MainForm: TMainForm;

IMPLEMENTATION

{ TMainForm }

PROCEDURE TMainForm.FormCreate(Sender: TObject);
BEGIN
  fBitmap := NIL;
end;

PROCEDURE TMainForm.FormDestroy(Sender: TObject);
BEGIN
{ Remember to release resources. }
  IF fBitmap <> NIL THEN
    al_destroy_bitmap (fBitmap);
end;



(* Load a bitmap. *)
PROCEDURE TMainForm.MenuItem_LoadClick(Sender: TObject);
BEGIN
  IF OpenDialog.Execute THEN
  BEGIN
    IF fBitmap <> NIL THEN
      al_destroy_bitmap (fBitmap);
    fBitmap := al_load_bitmap (OpenDialog.FileName, NIL);
  END;
END;

INITIALIZATION
  {$I UnitMainForm.lrs}

END.

