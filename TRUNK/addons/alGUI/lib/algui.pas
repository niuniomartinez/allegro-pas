UNIT alGUI;
(*<Defines a simple Object Oriented GUI system for Allegro.pas.

  It's much like the Allegro's GUI system but using Object Pascal classes.
 *)

INTERFACE

  USES
    allegro;

  TYPE
  (* Defines a base class for GUI styles. *)
    TalGUI_Style = CLASS (TObject)
    PRIVATE
      fTxtColor, fDisabledColor, fSelectedTxtColor, fBgColor, fSelectedBgColor,
      fLightColor, fDarkColor, fBorderColor: LONGINT;
      fTxtFont: AL_FONTptr;
    PUBLIC
    (* Draws a bevel, this is, a border, rectangle or frame.

      Since it is an abstract method, @code(TalGUI_Style) itself does
      not implement @code(DrawBevel).  Descendent classes such as
      @link(TalGUI_DefaultStyel) implement this method.
      @param(Raised @true, to draw as raised, if @false to draw as depressed.) *)
      PROCEDURE DrawBevel (Bmp: AL_BITMAPptr; x1, y1, x2, y2, Width: INTEGER;
	Raised: BOOLEAN); VIRTUAL; ABSTRACT;
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



  (* Extends @code(TalGUI_Style) to define a default style. *)
    TalGUI_DefaultStyle = CLASS (TalGUI_Style)
    END;

IMPLEMENTATION

(*
 * TalGUI_Style
 *****************************************************************************)

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

END.
