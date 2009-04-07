UNIT DoSetup;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

  Does the setup.  You can add this unit to your project and just call its
  public procedure after setting a graphic mode and installing keyboard. *)

INTERFACE

  USES
    albitmap;



(* Just call this!
   Must give a background bitmap. *)
  PROCEDURE RunSetup (aBackGround: AL_BITMAPptr);



IMPLEMENTATION

  USES
    SimpleGUI,
    albltspr, alcolor, algraph, alkeybrd, almouse, alsystem;



(* Now it tests the SimpleGUI. *)
  PROCEDURE RunSetup (aBackGround: AL_BITMAPptr);
  VAR
    Rectangulo: TRectangle;
  BEGIN
    TRY
      SimpleGUI.InitDoubleBuffer (aBackGround);
      Rectangulo := TButton.Create ('Click me!', 10, 10, 100,
	al_makecol (0, 0, 0), al_makecol (255, 255, 255), NIL);
      Rectangulo.Draw;
      REPEAT
	Rectangulo.Process;
      UNTIL al_keypressed;
    FINALLY
      Rectangulo.Free;
      SimpleGUI.CloseDoubleBuffer;
    END;
  END;

END.
