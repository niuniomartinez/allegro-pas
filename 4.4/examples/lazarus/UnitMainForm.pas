UNIT UnitMainForm;
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

   Read the comment in the "example.lpr" file for a global description of the
   example.

   Allegro initialization and finalization is done in the "example.lpr" to
   be sure it's done in the right order (Allegro must be initialized before any
   Allegro call and once finalized you shouldn't call any Allegro function or
   procedure).
 *)

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, Buttons,
  Allegro;

TYPE

  { TForm1 }

  TForm1 = CLASS(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    OpenBitmapDialog: TOpenDialog;
    OpenSoundDialog: TOpenDialog;
    PaintBox: TPaintBox;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE MenuItem3Click(Sender: TObject);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE MenuItem5Click(Sender: TObject);
    PROCEDURE PaintBoxPaint(Sender: TObject);
  PRIVATE
  (* The bitmap to be shown on the window. *)
    fBitmap: AL_BITMAPptr;
  (* The palette. *)
    fPalette: AL_PALETTE;
  (* The sound to play. *)
    fSample: AL_SAMPLEptr;
  PUBLIC
    { PUBLIC declarations }
  END; 

VAR
  Form1: TForm1; 

IMPLEMENTATION

USES
  LCLType, UnitBitmapInterface;



{ TForm1 }

(* Initializes the window.  See that Allegro must be initialized before to
   create the form. *)
PROCEDURE TForm1.FormCreate(Sender: TObject);
BEGIN
  fBitmap := NIL; { No bitmap! }
  fSample := NIL; { No sound! }
END;




(* Release resources. *)
PROCEDURE TForm1.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
BEGIN
{ Destroys the objects if it exists.  Don't destroy an Allegro object in the
  'OnDestroy' event because then it would be called after the calling of
  'al_exit'.  Be careful in the order of callings! }
  IF fBitmap <> NIL THEN
    al_destroy_bitmap (fBitmap);
  fBitmap := NIL;
  IF fSample <> NIL THEN
    al_destroy_sample (fSample);
  fSample := NIL;
END;



(* Allows to select a bitmap file, loads it and shows it on the window.
   See the file filter in the OpenBitmapDialog's Filter property.  It lists only the
   formats supported by Allegro. *)
PROCEDURE TForm1.MenuItem3Click(Sender: TObject);
BEGIN
  IF OpenBitmapDialog.Execute THEN
  BEGIN
  { If there is a bitmap, destroys it. }
    IF fBitmap <> NIL THEN
      al_destroy_bitmap (fBitmap);
  { Loads the bitmap. }
    fBitmap := al_load_bitmap (OpenBitmapDialog.FileName, @fPalette);
    IF fBitmap = NIL THEN
      Application.MessageBox (
        PChar ('Can''t load file '+ExtractFileName (OpenBitmapDialog.FileName)),
        'Error', MB_ICONERROR)
    ELSE
    { Forces redrawing of the window to show the new bitmap. }
      PaintBox.Invalidate;
  END;
END;



(* Allows to select a sound sample, loads it and playw it once.
   See the file filter in the OpenBitmapDialog's Filter property.
     It lists only the formats supported by Allegro. *)
PROCEDURE TForm1.MenuItem4Click(Sender: TObject);
BEGIN
  IF OpenSoundDialog.Execute THEN
  BEGIN
  { If there is a sample, destroys it. }
    IF fSample <> NIL THEN
      al_destroy_sample (fSample);
  { Loads the sample. }
    fSample := al_load_sample (OpenSoundDialog.FileName);
    IF fSample = NIL THEN
      Application.MessageBox (
        PChar ('Can''t load file '+ExtractFileName (OpenSoundDialog.FileName)),
        'Error', MB_ICONERROR)
    ELSE
    { Plays the sample once. }
      al_play_sample (fSample, 255, 127, 1000, 0);
  END;
END;



(* Shows a message box with a brief explaining of the project. *)
PROCEDURE TForm1.MenuItem5Click (Sender: TObject);
BEGIN
  Application.MessageBox (
        'This is a simple example of how to integrate Allegro.pas with Lazarus.'+
        #10+'Read the source comments for more  information.',
        'About...', MB_ICONINFORMATION);

END;



(* Paints the bitmap if exists. *)
PROCEDURE TForm1.PaintBoxPaint(Sender: TObject);
BEGIN
  IF fBitmap = NIL THEN
    EXIT;
  TRY
  { Creates a temporal bitmap. }
    BlitBitmap2Canvans (fBitmap, fPalette, PaintBox.Canvas);
  EXCEPT
    ON Error: Exception DO
      Application.MessageBox (PChar (Error.Message), 'Error', MB_ICONERROR);
  END;
END;



INITIALIZATION
  {$I UnitMainForm.lrs}

END.

