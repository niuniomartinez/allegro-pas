(* Platform dependent implementation for X Window systems of the OpenGL
   add-on.  It's included in the implementation section of unit "algl". *)

(* Set up full screen GFX mode. *)
FUNCTION FullscreenGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;
BEGIN
  FullscreenGFXInit := NIL;
END;



(* Set up windowed GFX mode. *)
FUNCTION WindowedGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;
BEGIN
  WindowedGFXInit := NIL;
END;
