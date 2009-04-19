UNIT SimpleGUI;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

  Some classes that allows to create a simple graphical user interface.

  The classes use a double buffer system that must be configured at start. *)

INTERFACE

USES
  albitmap;



TYPE
(* The base for every GUI object. *)
  TRectangle = CLASS (TObject)
  PROTECTED
    fBgColor, fBrColor: LONGINT; (* Background and border color. *)
    fX, fY, fW, fH: INTEGER;     (* Position and size. *)
    fCanvas: AL_BITMAPptr;	 (* Where will draw. *)
  PRIVATE
    fFocus: BOOLEAN;		 (* If true, draws a "focus" rect. *)
  PUBLIC
  (* Creates a rectangle.  Receives the position, the size, its colours and
     the bitmap where will draw.  If no bitmap is given it will draw directly
     on screen. *)
    CONSTRUCTOR Create (aX, aY, aW, aH: INTEGER; aBgC, aBrC: LONGINT;
			aCanvas: AL_BITMAPptr); VIRTUAL;
  (* Draws the rectangle. *)
    PROCEDURE Draw; VIRTUAL;
  (* Process the rectangle. *)
    PROCEDURE Process; VIRTUAL;
  (* True if given coordinates are inside the rectangle. *)
    FUNCTION Inside (aX, aY: INTEGER): BOOLEAN;

  (* Has focus. *)
    PROPERTY Focus: BOOLEAN READ fFocus WRITE fFocus;
  END;



(* Base for buttons.  By default, changes its "Selected" state. *)
  TButton = CLASS (TRectangle)
  PRIVATE
    fLabel: STRING;
    fSelected: BOOLEAN;

    PROCEDURE SetSelected (aState: BOOLEAN);
  PROTECTED
  (* Response.  Called when pressed by mouse at "Process" or when "Selected" is
     set to TRUE. *)
    PROCEDURE Select; VIRTUAL;
  PUBLIC
  (* Creates a button.  Height is 16 pixel. *)
    CONSTRUCTOR Create (aLabel: STRING; aX, aY, aW: INTEGER; aBgC, aFgC: LONGINT;
			aCanvas: AL_BITMAPptr); OVERLOAD;
  (* Draws the button. *)
    PROCEDURE Draw; OVERRIDE;
  (* Default process.  Checks the mouse, changing color when pressed and modifying
     state. *)
    PROCEDURE Process; OVERRIDE;

  (* True if selected. *)
    PROPERTY Selected: BOOLEAN READ fSelected WRITE SetSelected;
  END;



(* Initialize double buffer. *)
  PROCEDURE InitDoubleBuffer (TheBackground: AL_BITMAPptr);

(* Finalize double buffer. *)
  PROCEDURE CloseDoubleBuffer;


IMPLEMENTATION

USES
  allegro,
  albltspr, aldraw, algraph, almouse, altext,
  sysutils;



(****************************
 * The double buffer system *
 ****************************)

VAR
(* The double buffer components. *)
  ActivePage, Background: AL_BITMAPptr;
(* The active page may have different size, so the mouse movement must be
   scaled.  This is the scale ratio. *)
  MouseScaleX, MouseScaleY: DOUBLE;
(* An this is the scaled position. *)
  PosMouseX, PosMouseY: INTEGER;



(* Clears the active page. *)
  PROCEDURE ClearActivePage;
  BEGIN
    al_blit (Background, ActivePage, 0, 0, 0, 0, Background^.w, Background^.h);
  END;



(* Updates double buffer. *)
  PROCEDURE UpdateDoubleBuffer;
  BEGIN
    al_show_mouse (NIL);
    al_stretch_blit (ActivePage, al_screen,
		     0, 0, ActivePage^.w, ActivePage^.h,
		     0, 0, AL_SCREEN_W, AL_SCREEN_H);
    al_show_mouse (al_screen);
  END;



(* Gets and scales the mouse position. *)
  PROCEDURE GetMouse;
  BEGIN
    IF al_mouse_needs_poll THEN al_poll_mouse;
    PosMouseX := TRUNC (al_mouse_x * MouseScaleX);
    PosMouseY := TRUNC (al_mouse_y * MouseScaleY)
  END;



(**************
 * TRectangle *
 **************)

(* Constructor. *)
  CONSTRUCTOR TRectangle.Create (aX, aY, aW, aH: INTEGER; aBgC, aBrC: LONGINT;
				aCanvas: AL_BITMAPptr);
  BEGIN
    fX := aX; fY := aY; fW := aW; fH := aH;
    fBgColor := aBgC; fBrColor := aBrC;
    IF aCanvas <> NIL THEN fCanvas := aCanvas ELSE fCanvas := ActivePage;
    fFocus := FALSE;
  END;



(* Draws the rectangle. *)
  PROCEDURE TRectangle.Draw;
  VAR
    c: INTEGER;
  BEGIN
    al_rectfill (fCanvas, fX, fY, fX + fW - 1, fy + fH - 1, fBgColor);
    al_rect (fCanvas, fX + 1, fY + 1, fX + fW - 2, fy + fH - 2, fBrColor);
    IF fFocus THEN
    BEGIN
    { Tree loops to avoid bank switches. }
      FOR c := 0 TO (fW -1) DO
	IF c AND 1 <> 0 THEN
	  al_putpixel (fCanvas, fX + c, fY, fBrColor)
	ELSE
	  al_putpixel (fCanvas, fX + c, fY, fBgColor);
      FOR c := 1 TO (fH - 1) DO
	IF c AND 1 <> 0 THEN
	BEGIN
	  al_putpixel (fCanvas, fX, fY + c, fBrColor);
	  al_putpixel (fCanvas, fX + fW - 1, fY + c, fBrColor);
	END
	ELSE BEGIN
	  al_putpixel (fCanvas, fX, fY + c, fBgColor);
	  al_putpixel (fCanvas, fX + fW - 1, fY + c, fBgColor);
	END;
      FOR c := 0 TO (fW -1) DO
	IF c AND 1 <> 0 THEN
	  al_putpixel (fCanvas, fX + c, fY + fH - 1, fBrColor)
	ELSE
	  al_putpixel (fCanvas, fX + c, fY + fH - 1, fBgColor);
    END;
  END;



(* Process the rectangle. *)
  PROCEDURE TRectangle.Process;
  BEGIN
    ; (* Does nothing. *)
  END;



(* True if given coordinates are inside the rectangle. *)
  FUNCTION TRectangle.Inside (aX, aY: INTEGER): BOOLEAN;
  BEGIN
    Inside := (fX <= aX) AND (aX < (fX + fW))
	      AND (fY <= aY) AND (aY < (fY + fH));
  END;



(***********
 * TButton *
 ***********)

(* Changes "Selected" state. *)
  PROCEDURE TButton.SetSelected (aState: BOOLEAN);
  BEGIN
    IF aState <> fSelected THEN
    BEGIN
    { Set state. }
      fSelected := aState;
    { Does it Select or Unselect? }
      IF fSelected THEN
	Select;
    END;
  END;



(* Response.  Called when pressed by mouse at "Process" or when "Selected" is
   set to TRUE. *)
  PROCEDURE TButton.Select;
  BEGIN
  { Does nothing.  This way changes state between "Selected" and "Unselected". }
    ;
  END;



(* Creates a button.  Height is 16 pixel. *)
  CONSTRUCTOR TButton.Create (aLabel: STRING; aX, aY, aW: INTEGER; aBgC, aFgC: LONGINT;
			aCanvas: AL_BITMAPptr);
  BEGIN
    INHERITED Create (aX, aY, aW, 16, aBgC, aFgC, aCanvas);
    fLabel := aLabel;
    fSelected := FALSE;
  END;



(* Draws the button. *)
  PROCEDURE TButton.Draw;
  BEGIN
    INHERITED Draw; { The rectangle. }
    al_textout_centre_ex (fCanvas, al_font, fLabel, (fX + fW) DIV 2, fY + 4, fBrColor, fBgColor);
    UpdateDoubleBuffer; { This is here until write "TDialog" class. }
  END;



(* Default process. *)
  PROCEDURE TButton.Process;
  VAR
    Tmp: LONGINT;
    Old: BOOLEAN;
  BEGIN
    GetMouse;
  { If pressed inside... }
    IF Inside (PosMouseX, PosMouseY) AND (al_mouse_b <> 0) THEN
    BEGIN
    { Change color and set focus. }
      Tmp := fBrColor; fBrColor := fBgColor; fBgColor := Tmp;
      Old := SELF.Focus;
      SELF.Focus := TRUE;
      SELF.Draw; UpdateDoubleBuffer;
    { While pressing inside...}
      WHILE Inside (PosMouseX, PosMouseY) AND (al_mouse_b <> 0)
      AND NOT al_keypressed DO
	GetMouse;
    { Did it release inside? }
      IF Inside (PosMouseX, PosMouseY) AND (al_mouse_b = 0) THEN
	SELF.SetSelected (NOT fSelected)
      ELSE BEGIN
      { If not then back to previous state. }
	Tmp := fBrColor; fBrColor := fBgColor; fBgColor := Tmp;
	SELF.Focus := Old;
	SELF.Draw; UpdateDoubleBuffer;
      END;
    END;
  END;



(* Initialize double buffer. *)
  PROCEDURE InitDoubleBuffer (TheBackground: AL_BITMAPptr);
  BEGIN
    Background := TheBackground;
    ActivePage := al_create_bitmap (Background^.w, Background^.h);
    IF ActivePage = NIL THEN
      RAISE Exception.Create ('Can''t create double buffer system.');
  (* Calculate mouse scale ratio. *)
    MouseScaleX := ActivePage^.w / AL_SCREEN_W;
    MouseScaleY := ActivePage^.h / AL_SCREEN_H;
    ClearActivePage;
  END;



(* Finalize double buffer. *)
  PROCEDURE CloseDoubleBuffer;
  BEGIN
    IF ActivePage <> NIL THEN
      al_destroy_bitmap (ActivePage);
  END;

END.
