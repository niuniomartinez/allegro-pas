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
      fX, fY, fW, fH, fTag: INTEGER;
      fHasFocus, fDisabled: BOOLEAN;
    PROTECTED
    (* Sets the @code(Disabled) property.  Overriden implementation should call
      this to actually set the @code(Disabled) value. *)
      PROCEDURE SetDisabled (CONST SetDisabled: BOOLEAN); VIRTUAL;

    (* Informs the object that a mouse button has been clicked while the mouse
      was on top of the object.  Typically an object will perform its own
      mouse tracking as long as the button is held down, and only return from
      this message handler when it is released.

      If you process this message, use the functions @link(al_gui_mouse_* ) to
      read the state of the mouse.
      @return(@true if message was handled, or @false if control isn't
       interested on it.) *)
      FUNCTION MsgClick (CONST X, Y, Button: INTEGER): BOOLEAN; VIRTUAL;
    (* Sent whenever the dialog manager has nothing better to do. *)
      PROCEDURE MsgIddle; VIRTUAL;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); VIRTUAL;

    (* Reference to the owner dialog. *)
      PROPERTY Dialog: TalGUI_Dialog READ fOwner;
    (* Left position of the component. *)
      PROPERTY X: INTEGER READ fX WRITE fX;
    (* Top position of the component. *)
      PROPERTY Y: INTEGER READ fY WRITE fY;
    (* Width of the component in pixels. *)
      PROPERTY Width: INTEGER READ fW WRITE fW;
    (* Height of the component in pixels. *)
      PROPERTY Height: INTEGER READ fH WRITE fH;
    (* Extra value that can be used to identify the component or to store a
      value that may be useful somewhere. *)
      PROPERTY Tag: INTEGER READ fTag WRITE fTag;
    (* @true if control has input focus, @false otherwise.
      @seealso(MsgGotFocus) @seealso(MsgLostFocus) @seealso(WantFocus) *)
      PROPERTY HasFocus: BOOLEAN READ fHasFocus;
    (* @true if control is inactive, @false otherwise.

      Inactive controls can't get input focus and can't be selected.

      By default it's @false *)
      PROPERTY Disabled: BOOLEAN READ fDisabled WRITE SetDisabled;
    END;



  (* Defines and manages the dialog. *)
    TalGUI_Dialog = CLASS (TObject)
    PRIVATE
      fBitmap: AL_BITMAPptr;
      fStyle: TalGUI_Style;
      fControlList : TFPObjectList;

      FUNCTION GetCount: INTEGER;
      FUNCTION GetControl (CONST Index: INTEGER): TalGUI_Control;
    PUBLIC
    (* Constructor.

      It sets bitmap to screen and assigns default style. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Adds a new control to the dialog. @return(Index to the control)
      @seealso(Controls) *)
      FUNCTION Add (aControl: TalGUI_Control): INTEGER;

    (* Bitmap where the dialog will be drawn.  By default it's the
      @code(al_screen). *)
      PROPERTY Bmp: AL_BITMAPptr READ fBitmap WRITE fBitmap;
    (* Dialog style.  Note that assign it will not destroy the previous style.
     *)
      PROPERTY Style: TalGUI_Style READ fStyle WRITE fStyle;
    (* Number of controls in the dialog. @seealso(Controls) *)
      PROPERTY Count: INTEGER READ GetCount;
    (* Indexed access to the controls of the dialog.

      @code(Controls) is the default property of the class.
      The index @code(Index) is zero based, i.e., runs from 0 (zero) to
      @code(Count-1). @seealso(Count) @seealso(Add) *)
      PROPERTY Controls[Index: INTEGER]: TalGUI_Control READ GetControl;
    END;




  (* An invisible helper object that yields time slices for the scheduler (if
     the system supports it) when the GUI has nothing to do but waiting for
     user actions.  You should put one instance of this object, or a descendent
     such as @code(TalGUI_ClearScreen), in each dialog because it may be needed
     on systems with an unusual scheduling algorithm (for instance QNX) in
     order to make the GUI fully responsive.
     @seealso(TalGUI_ClearScreen) *)
    TalGUI_Yield = CLASS (TalGUI_Control)
    PROTECTED
    (* Sent whenever the dialog manager has nothing better to do. *)
      PROCEDURE MsgIddle; OVERRIDE;
    END;



  (* This just clears the screen when it is drawn. Useful as the first object
     in a dialog.

     Since it's a @code(TalGUI_Yield) descendent, such object isn't needed if
     @code(TalGUI_ClearScreen) is used. *)
    TalGUI_ClearScreen = CLASS (TalGUI_Yield)
    PRIVATE
      fColor: LONGINT;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create; OVERRIDE;
    (* Draws the control in the given bitmap. *)
      PROCEDURE Draw (Bmp: AL_BITMAPptr); OVERRIDE;

    (* Color to use to clear the screen. *)
      PROPERTY Color: LONGINT READ fColor WRITE fColor;
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



  (* Extends @code(TalGUI_Style) to define a default style.

    It's inspired by the old Windows style. *)
    TalGUI_DefaultStyle = CLASS (TalGUI_Style)
    PUBLIC
    (* Sets default colors.  Call this after set graphics mode and define the
      color palette. *)
      PROCEDURE SetDefaultColors; OVERRIDE;
    (* Draws a bevel, this is, a border, rectangle or frame.
      @param(Bmp Where to draw it.)
      @param(x1 Left limit of box.)  @param(y1 Top limit of box.)
      @param(x2 Right limit of box.) @param(y2 Bottom limit of box.)
      @param(Raised @true, to draw as raised, if @false to draw as depressed.)
      @seealso(DrawBox)
    *)
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
 * TalGUI_Control
 *****************************************************************************)

(* Sets the @code(Disabled) property.  Overriden implementation should call
  his to actually set the @code(Disabled) value. *)
  PROCEDURE TalGUI_Control.SetDisabled (CONST SetDisabled: BOOLEAN);
  BEGIN
    fDisabled := SetDisabled
  END;


(* Informs the object that a mouse button has been clicked. *)
  FUNCTION TalGUI_Control.MsgClick (CONST X, Y, Button: INTEGER): BOOLEAN;
  BEGIN
    RESULT := FALSE
  END;



(* Sent whenever the dialog manager has nothing better to do. *)
  PROCEDURE TalGUI_Control.MsgIddle;
  BEGIN
    { Does nothing by default. }
  END;



(* Constructor. *)
  CONSTRUCTOR TalGUI_Control.Create;
  BEGIN
    INHERITED Create;
    fOwner := NIL;
    fX := 0; fY := 0; fW := 0; fH := 0; fTag := 0;
    fDisabled := FALSE; fHasFocus := FALSE;
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_Control.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    { Does nothing by default. }
  END;



(*
 * TalGUI_Dialog
 *****************************************************************************)

  FUNCTION TalGUI_Dialog.GetCount: INTEGER;
  BEGIN
    RESULT := fControlList.Count
  END;



  FUNCTION TalGUI_Dialog.GetControl (CONST Index: INTEGER): TalGUI_Control;
  BEGIN
    RESULT := TalGUI_Control (fControlList.Items[Index])
  END;



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



(* Adds a new control to the dialog. @return(Index to the control) *)
  FUNCTION TalGUI_Dialog.Add (aControl: TalGUI_Control): INTEGER;
  BEGIN
    RESULT := fControlList.Add (aControl);
    aControl.fOwner := SELF
  END;



(*
 * TalGUI_Yield
 *****************************************************************************)

(* Sent whenever the dialog manager has nothing better to do. *)
  PROCEDURE TalGUI_Yield.MsgIddle;
  BEGIN
  { Play fair with Opeating System. }
    al_rest (1)
  END;



(*
 * TalGUI_ClearScreen
 *****************************************************************************)

(* Constructor. *)
  CONSTRUCTOR TalGUI_ClearScreen.Create;
  BEGIN
    INHERITED Create;
    fColor := al_makecol (0, 0, 0)
  END;



(* Draws the control in the given bitmap. *)
  PROCEDURE TalGUI_ClearScreen.Draw (Bmp: AL_BITMAPptr);
  BEGIN
    al_clear_to_color (Bmp, fColor)
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
