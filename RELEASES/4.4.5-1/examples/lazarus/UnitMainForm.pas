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
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls,
  Allegro;

TYPE

  { TForm1 }

  TForm1 = CLASS (TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OpenBitmapDialog: TOpenDialog;
    PaintBox: TPaintBox;
    PROCEDURE FormCreate (Sender: TObject);
    PROCEDURE FormClose (Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE MenuItem3Click (Sender: TObject);
    PROCEDURE MenuItem4Click (Sender: TObject);
    PROCEDURE PaintBoxPaint (Sender: TObject);
  PRIVATE
  (* The bitmap to be shown on the window. *)
    fBitmap: AL_BITMAPptr;
  (* The palette. *)
    fPalette: AL_PALETTE;
  PUBLIC
    { public declarations }
  END;

VAR
  Form1: TForm1;

IMPLEMENTATION

USES
  LCLType, UnitBitmapInterface;

{$R *.lfm}

{ TForm1 }

(* Initializes the window.  See that Allegro must be initialized before to
   create the form. *)
PROCEDURE TForm1.FormCreate (Sender: TObject);
BEGIN
  fBitmap := NIL; { No bitmap! }
  IF AL_PAS_IS_BETA THEN
    Application.MessageBox (
      'This is a beta version, wich means it is a test version.'+
      #10+'To get an stable version visit http://allegro-pas.sourceforge.net',
      'Information', MB_ICONINFORMATION
    )
END;



(* Releases resources. *)
PROCEDURE Tform1.FormClose (Sender: Tobject; VAR CloseAction: Tcloseaction);
BEGIN
{ Destroy the objects if they exist.  Don't destroy an Allegro object in the
  'OnDestroy' event because then it would be called after the calling of
  'al_exit'.  Be careful with the order of callings! }
  IF fBitmap <> NIL THEN
    al_destroy_bitmap (fBitmap);
  fBitmap := NIL
END;



(* Allows to select a bitmap file, loads it and shows it on the window.
   See the file filter in the OpenBitmapDialog's Filter property.  It lists only
   the formats supported by Allegro. *)
PROCEDURE TForm1.MenuItem3Click (Sender: TObject);
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
      PaintBox.Invalidate
  END;
END;



(* Shows a message box with a brief explanation of the project. *)
PROCEDURE TForm1.MenuItem4Click (Sender: TObject);
BEGIN
  Application.MessageBox (
    'This is a simple example of how to integrate Allegro.pas with Lazarus.'+
    #10+'Read the source comments for more information.'+
    #10+#10+'Linked with Allegro.pas '+AL_PAS_VERSION_STR,
    'About...', MB_ICONINFORMATION)
END;



(* Paints the bitmap if exists. *)
PROCEDURE TForm1.PaintBoxPaint (Sender: TObject);
VAR
  ZoomedImage: AL_BITMAPptr;
BEGIN
  IF fBitmap = NIL THEN
    EXIT;
  TRY
  { Creates a temporal bitmap to zoom the image to fit the client area. }
    ZoomedImage := al_create_bitmap_ex (al_bitmap_color_depth (fBitmap),
                PaintBox.Canvas.Width, PaintBox.Canvas.Height);
    al_stretch_blit (fBitmap, ZoomedImage,
      0, 0, fBitmap^.w, fBitmap^.h,
      0, 0, ZoomedImage^.w, ZoomedImage^.h);
    TRY
      BlitBitmap2Canvans (ZoomedImage, fPalette, PaintBox.Canvas);
    FINALLY
    { Destroys the temporal file. }
      al_destroy_bitmap (ZoomedImage);
    END;
  EXCEPT
    ON Error: Exception DO
      Application.MessageBox (PChar (Error.Message), 'Error', MB_ICONERROR);
  END;
END;

END.

