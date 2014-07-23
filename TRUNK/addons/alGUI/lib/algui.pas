UNIT alGUI;
(*<Defines a simple Object Oriented GUI system for Allegro.pas.

  It's much like the Allegro's GUI system but using Object Pascal classes.
 *)

INTERFACE

  USES
    allegro, contnrs;

  TYPE
  (* @exclude *)
    TalGUI_Dialog = CLASS;



  (* Defines a base class for GUI styles. *)
    TalGUI_Style = CLASS (TObject)
    PRIVATE
      fTxtColor, fDisabledColor, fSelectedTxtColor, fBgColor, fSelectedBgColor,
      fLightColor, fDarkColor, fBorderColor: LONGINT;
      fTxtFont: AL_FONTptr;
    PUBLIC
    (* Constructor.  Sets default colors and text font. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Sets default colors.  Call this after set graphics mode and define the
      color palette.

      Since it is an abstract method, @code(TalGUI_Style) itself does
      not implement @code(SetDefaultColors).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method. *)
      PROCEDURE SetDefaultColors; VIRTUAL; ABSTRACT;
    (* Draws a bevel, this is, a border, rectangle or frame.

      Since it is an abstract method, @code(TalGUI_Style) itself does
      not implement @code(DrawBevel).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(Raised @true, to draw as raised, if @false to draw as depressed.)
      @seealso(DrawBox)
    *)
      PROCEDURE DrawBevel (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER;
	Raised: BOOLEAN); VIRTUAL; ABSTRACT;
    (* Draws a box.  Useful for buttons and pannels.

      Since it is an abstract method, @code(TalGUI_Style) itself does
      not implement @code(DrawBevel).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(BorderWidth Width of border in pixels.)
      @param(BackColor Background color.  If negative, then it will not draw
        background.)
      @param(Raised @true, to draw the border as raised, if @false to draw it
        as depressed.)
      @seealso(DrawBevel)
     *)
      PROCEDURE DrawBox (
        Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor, BorderWidth: INTEGER;
        Raised: BOOLEAN); VIRTUAL; ABSTRACT;
    (* Draws a dialog frame, wich includes a title.

      Since it is an abstract method, @code(TalGUI_Style) itself does
      not implement @code(DrawDialogFrame).  Descendent classes such as
      @link(TalGUI_DefaultStyle) implement this method.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of window.)  @param(y1 Top limit of window.)
      @param(x2 Right limit of window.) @param(y2 Bottom limit of window.)
      @param(BackColor Background color.  If negative, then it will not draw
        background.)
      @param(Title Texto to draw on title.)
      @param(TitleCentered @true, to draw title centered.)
     *)
      PROCEDURE DrawDialogFrame (
        Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
        Title: STRING; TitleCentered: BOOLEAN
      ); VIRTUAL; ABSTRACT;
    (* Draws a dotted rectangle to show that the control has acquired focus. *)
      PROCEDURE DrawFocusRect (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER);
	VIRTUAL;
    (* Draws a text.
      @seealso(TextFont) @seealso(DrawDisabledText) *)
      PROCEDURE DrawText (Bmp: AL_BITMAPptr; CONST Msg: STRING;
	X, Y, Color: LONGINT; Centered: BOOLEAN); VIRTUAL;
    (* Draws a text as disabled.  By default it just calls @code(DrawText) with
       the @code(DisabledTextColor), but descendent classes may draw the text
       with special properties or different text fonts.
      @seealso(TextFont) @seealso(DrawText) *)
      PROCEDURE DrawDisabledText (Bmp: AL_BITMAPptr; CONST Msg:
	STRING; X, Y: LONGINT; Centered: BOOLEAN); VIRTUAL;


    (* Default text color. *)
      PROPERTY TextColor: LONGINT READ fTxtColor WRITE fTxtColor;
    (* Disabled text color. *)
      PROPERTY DisabledTextColor: LONGINT READ fDisabledColor WRITE fDisabledColor;
    (* Selected text color. *)
      PROPERTY SelectedTextColor: LONGINT READ fSelectedTxtColor WRITE fSelectedTxtColor;
    (* Default background color. *)
      PROPERTY BackgroundColor: LONGINT READ fBgColor WRITE fBgColor;
    (* Selected background color. *)
      PROPERTY SelectedBackgroundColor: LONGINT READ fSelectedBgColor WRITE fSelectedBgColor;
    (* Color used in the light side of 3D drawing.  For example, in 3D buttons. *)
      PROPERTY LightColor: LONGINT READ fLightColor WRITE fLightColor;
    (* Color used in the dark side of 3D drawing.  For example, in 3D buttons. *)
      PROPERTY DarkColor: LONGINT READ fDarkColor WRITE fDarkColor;
    (* Color used in borders. *)
      PROPERTY BorderColor: LONGINT READ fBorderColor WRITE fBorderColor;
    (* Default font. *)
      PROPERTY TextFont: AL_FONTptr READ fTxtFont WRITE fTxtFont;
    END;



  (* Base class for dialog controls. *)
    TalGUI_Control = CLASS (TObject)
    PRIVATE
      fOwner: TalGUI_Dialog;
    PUBLIC
    (* Reference to the owner dialog. *)
      PROPERTY Dialog: TalGUI_Dialog READ fOwner;
    END;



  (* Defines and manages the dialog. *)
    TalGUI_Dialog = CLASS (TObject)
    PRIVATE
      fBitmap: AL_BITMAPptr;
      fStyle: TalGUI_Style;
      fControlList : TFPObjectList;
    PUBLIC
    (* Constructor.

      It sets bitmap to screen and assigns default style. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;

    (* Bitmap where the dialog will be drawn.  By default it's the
      @code(al_screen). *)
      PROPERTY Bmp: AL_BITMAPptr READ fBitmap WRITE fBitmap;
    (* Dialog style.  Note that assign it will not destroy the previous style.
     *)
      PROPERTY Style: TalGUI_Style READ fStyle WRITE fStyle;
    END;




  (* An invisible helper object that yields time slices for the scheduler (if
     the system supports it) when the GUI has nothing to do but waiting for
     user actions.  You should put one instance of this object, or a descendent
     such as @code(TalGUI_ClearScreen), in each dialog array because it may be
     needed on systems with an unusual scheduling algorithm (for instance QNX)
     in order to make the GUI fully responsive.
     @seealso(TalGUI_ClearScreen) *)
    TalGUI_Yield = CLASS (TalGUI_Control)
    END;



  (* This just clears the screen when it is drawn. Useful as the first object
     in a dialog.

     Since it's a @code(TalGUI_Yield) descendent, such object isn't needed if
     @code(TalGUI_ClearScreen) is used. *)
    TalGUI_ClearScreen = CLASS (TalGUI_Yield)
    END;



  (* Draws a box or panel.  It may have border, bevel and/or background. *)
    TalGUI_Box = CLASS (TalGUI_Control)
    END;



  (* Draws text onto the dialog. *)
    TalGUI_Label = CLASS (TalGUI_Control)
    END;



  (* Introduces methods and stuff for many types of buttons.
    This object can be selected by clicking on it with the mouse or by pressing
    its keyboard shortcut. *)
    TalGUI_CustomButton = CLASS (TalGUI_Control)
    END;



  (* A simple button object. *)
    TalGUI_Button = CLASS (TalGUI_CustomButton)
    END;



  (* Extends @code(TalGUI_Style) to define a default style.

    It's inspired by the old Windows style. *)
    TalGUI_DefaultStyle = CLASS (TalGUI_Style)
    PUBLIC
    (* Draws a bevel, this is, a border, rectangle or frame.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(Raised @true, to draw as raised, if @false to draw as depressed.)
      @seealso(DrawBox)
    *)
   (* Sets default colors.  Call this after set graphics mode and define the
      color palette. *)
      PROCEDURE SetDefaultColors; OVERRIDE;

      PROCEDURE DrawBevel (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER;
	Raised: BOOLEAN); OVERRIDE;
    (* Draws a box.  Useful for buttons and pannels.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(BorderWidth Width of border in pixels.)
      @param(BackColor Background color.  If negative, then it will not draw
        background.)
      @param(Raised @true, to draw the border as raised, if @false to draw it
        as depressed.)
      @seealso(DrawBevel)
     *)
      PROCEDURE DrawBox (
        Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor, BorderWidth: INTEGER;
        Raised: BOOLEAN); OVERRIDE;
    (* Draws a dialog frame, wich includes a title.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of window.)  @param(y1 Top limit of window.)
      @param(x2 Right limit of window.) @param(y2 Bottom limit of window.)
      @param(BackColor Background color.  If negative, then it will not draw
        background.)
      @param(Title Texto to draw on title.)
      @param(TitleCentered @true, to draw title centered.)
     *)
      PROCEDURE DrawDialogFrame (
        Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
        Title: STRING; TitleCentered: BOOLEAN
      ); OVERRIDE;
    (* Draws a text as disabled. *)
      PROCEDURE DrawDisabledText (Bmp: AL_BITMAPptr; CONST Msg:
	STRING; X, Y: LONGINT; Centered: BOOLEAN); OVERRIDE;
    END;

IMPLEMENTATION

(*
 * TalGUI_Style
 *****************************************************************************)

(* Constructor.  Sets default colors. *)
  CONSTRUCTOR TalGUI_Style.Create;
  BEGIN
    INHERITED Create;
    SELF.SetDefaultColors;
    fTxtFont := al_font
  END;



(* Draws a dotted rectangle to show that the control has acquired focus. *)
  PROCEDURE TalGUI_Style.DrawFocusRect (Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER);
  VAR
    X, Y: INTEGER;
  BEGIN
    IF X1 > X2 THEN BEGIN X := X1; X1 := X2; X2 := X END;
    IF Y1 > Y2 THEN BEGIN Y := Y1; Y1 := Y2; Y2 := Y END;
  { Done this way to reduce video bank switching (or something). }
    FOR X := 0 TO ((X2 -X1) DIV 2) DO
    BEGIN
      al_putpixel (Bmp, X1 + (X * 2)    , Y1, BorderColor);
      al_putpixel (Bmp, X1 + (X * 2) + 1, Y1, LightColor);
    END;
    FOR X := 0 TO ((X2 -X1) DIV 2) DO
    BEGIN
      al_putpixel (Bmp, X1 + (X * 2)    , Y2, BorderColor);
      al_putpixel (Bmp, X1 + (X * 2) + 1, Y2, LightColor);
    END;
    FOR Y := 1 TO (Y2 - Y1) DO
      IF y MOD 2 = 0 THEN
      BEGIN
        al_putpixel (Bmp, X1, Y1 + Y, BorderColor);
        al_putpixel (Bmp, X2, Y1 + Y, BorderColor);
      END
      ELSE BEGIN
        al_putpixel (Bmp, X1, Y1 + Y, LightColor);
        al_putpixel (Bmp, X2, Y1 + Y, LightColor);
      END
  END;



(* Draws a text. *)
  PROCEDURE TalGUI_Style.DrawText (Bmp: AL_BITMAPptr; CONST Msg: STRING;
    X, Y, Color: LONGINT; Centered: BOOLEAN);
  BEGIN
    IF Centered THEN
      al_textout_centre_ex (Bmp, fTxtFont, Msg, X, Y, Color, -1)
    ELSE
      al_textout_ex (Bmp, fTxtFont, Msg, X, Y, Color, -1)
  END;



(* Draws a text as disabled. *)
  PROCEDURE TalGUI_Style.DrawDisabledText (Bmp: AL_BITMAPptr; CONST Msg:
    STRING; X, Y: LONGINT; Centered: BOOLEAN);
  BEGIN
    SELF.DrawText (Bmp, Msg, X, Y, fDisabledColor, Centered)
  END;



(*
 * TalGUI_Dialog
 *****************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TalGUI_Dialog.Create;
  BEGIN
    INHERITED Create;
    fControlList := TFPObjectList.Create (TRUE);
    fBitmap := al_screen;
    fStyle := TalGUI_DefaultStyle.Create
  END;



(* Destructor. *)
  DESTRUCTOR TalGUI_Dialog.Destroy;
  BEGIN
    fControlList.Free;
    IF fStyle <> NIL THEN fStyle.Free;
    INHERITED Destroy;
  END;



(*
 * TalGUI_DefaultStyle
 *****************************************************************************)

(* Sets default colors.  Call this after set graphics mode and define the
  color palette. *)
  PROCEDURE TalGUI_DefaultStyle.SetDefaultColors;
  BEGIN
    fTxtColor         := al_makecol (  0,   0,   0);
    fDisabledColor    := al_makecol (128, 128, 128);
    fSelectedTxtColor := al_makecol (255, 255, 255);
    fBgColor          := al_makecol (204, 204, 204);
    fSelectedBgColor  := al_makecol (  0,   0, 128);
    fLightColor       := al_makecol (255, 255, 255);
    fDarkColor        := al_makecol (102, 102, 102);
    fBorderColor      := al_makecol (  0,   0,   0);
  END;



(* Draws a bevel, this is, a border, rectangle or frame. *)
  PROCEDURE TalGUI_DefaultStyle.DrawBevel (
    Bmp: AL_BITMAPptr; x1, y1, x2, y2: INTEGER; Raised: BOOLEAN);
  BEGIN
    IF Raised THEN
    BEGIN
      al_rect (Bmp, x1, y1, x2 - 1, y2 - 1, fLightColor);
      al_rect (Bmp, x1 + 1, y1 + 1, x2, y2, fDarkColor)
    END
    ELSE BEGIN
      al_rect (Bmp, x1, y1, x2 - 1, y2 - 1, fDarkColor);
      al_rect (Bmp, x1 + 1, y1 + 1, x2, y2, fLightColor)
    END
  END;



(* Draws a box.  Useful for buttons and pannels. *)
  PROCEDURE TalGUI_DefaultStyle.DrawBox (
    Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor, BorderWidth: INTEGER;
    Raised: BOOLEAN);
  VAR
    Cnt, ClrLeft, ClrRight: INTEGER;
  BEGIN
    IF BackColor >= 0 THEN al_rectfill (Bmp, x1, y1, x2, y2, BackColor);
    IF BorderWidth > 0 THEN
    BEGIN
      IF Raised THEN
      BEGIN
        ClrLeft := fLightColor; ClrRight := fDarkColor
      END
      ELSE BEGIN
        ClrLeft := fDarkColor; ClrRight := fLightColor
      END;
      FOR Cnt := BorderWidth - 1 DOWNTO 0 DO
      BEGIN
        al_hline (Bmp, x1 + Cnt, y2 - Cnt, x2 - Cnt, ClrRight);
        al_vline (Bmp, X2 - Cnt, y1 + Cnt, y2 - Cnt, ClrRight);

        al_hline (Bmp, x1 + Cnt, y1 + Cnt, x2 - Cnt, ClrLeft);
        al_vline (Bmp, X1 + Cnt, y1 + Cnt, y2 - Cnt, ClrLeft);
      END
    END
    ELSE
      al_rect (Bmp, x1, y1, x2, y2, fBorderColor);
  END;



(* Draws a dialog frame, wich includes a title. *)
  PROCEDURE TalGUI_DefaultStyle.DrawDialogFrame (
    Bmp: AL_BitmapPtr; x1, y1, x2, y2, BackColor: INTEGER;
    Title: STRING; TitleCentered: BOOLEAN);
  BEGIN
    DrawBox (Bmp, X1, Y1, X2, Y2, BackColor, 1, TRUE);
    DrawBox (Bmp, X1, Y1, X2, Y1 + 16, fSelectedBgColor, 1, TRUE);
    IF TitleCentered THEN x1 := (x1 + X2) DIV 2;
    DrawText (Bmp, Title, X1, Y1 + 4, fSelectedTxtColor, TitleCentered)
  END;



(* Draws a text as disabled. *)
  PROCEDURE TalGUI_DefaultStyle.DrawDisabledText (Bmp: AL_BITMAPptr; CONST Msg:
    STRING; X, Y: LONGINT; Centered: BOOLEAN);
  BEGIN
    DrawText (Bmp, Msg, X + 1, Y + 1, fLightColor, Centered);
    DrawText (Bmp, Msg, X, Y,     fDarkColor,  Centered)
  END;

END.
