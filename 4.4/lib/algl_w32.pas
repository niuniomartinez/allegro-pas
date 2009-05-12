UNIT algl_w32;
(*< @exclude

   Platform dependent implementation for Win32 systems of the OpenGL add-on.
   It's included in the implementation section of unit "algl". *)

INTERFACE

USES
  allegro;



(* Set up full screen GFX mode. *)
FUNCTION FullscreenGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;

(* Set up windowed GFX mode. *)
FUNCTION WindowedGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;

{ Closes the OpenGL context. }
PROCEDURE CloseGFX (bmp: AL_BITMAPptr); CDECL;



IMPLEMENTATION

USES
  aldrv;



FUNCTION CreateScreen (w, h, vw, vh, depth: LONGINT; FullScreen: BOOLEAN):
	AL_BITMAPptr;
VAR
  KeyboardWasInstalled, MouseWasInstalled: BOOLEAN;
BEGIN
{ Check which drivers where installed. }
  KeyboardWasInstalled := al_keyboard_driver <> NIL;
  MouseWasInstalled := al_mouse_driver <> NIL;
{ Uninstall drivers temporally. }
  IF KeyboardWasInstalled THEN al_remove_keyboard;
  IF MouseWasInstalled THEN al_remove_mouse;
  CreateScreen := NIL;
{ Restore drivers installed. }
  IF KeyboardWasInstalled THEN al_install_keyboard;
  IF MouseWasInstalled THEN al_install_mouse;
END;



(* Set up full screen GFX mode. *)
FUNCTION FullscreenGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;
BEGIN
WriteLn ('Iniciando pantalla completa...');
  FullscreenGFXInit := CreateScreen (w, h, vw, vh, bpp, FALSE);
WriteLn ('Pantalla completa inicializada.');
END;



(* Set up windowed GFX mode. *)
FUNCTION WindowedGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;
BEGIN
WriteLn ('Iniciando ventana...');
  WindowedGFXInit := CreateScreen (w, h, vw, vh, bpp, TRUE);
WriteLn ('Ventana iniciada.');
END;



{ Closes the OpenGL context. }
PROCEDURE CloseGFX (bmp: AL_BITMAPptr); CDECL;
BEGIN
{ Does nothing at the moment. }
END;

END.
