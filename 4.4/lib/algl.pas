UNIT algl;
(*<Provides functions to allow you to use OpenGL alongside Allegro.

  You use OpenGL for your rendering to the screen, and Allegro for
  miscellaneous tasks like gathering input, doing timers, getting
  cross-platform portability, loading data, and drawing your textures.  So this
  library fills the same hole that things like glut do.

  Parts of this code are based on AllegroGL add-on. *)

{$IFDEF FPC}
{ Free Pascal. }
 {$MODE DELPHI} { This is different than FPC mode for link procedures. }
 {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  allegro;



(* Initializes the Allegro.pas' interface with OpenGL.  Should be called after
   @link(al_init) or @link(al_install).
   @returns(@true on success, @false on failure). *)
  FUNCTION al_gl_init: BOOLEAN;

(* Removes the AllegroGL addon.

   You should not call any more OpenGL or @code(al_gl_* ) functions after
   calling this function.  Note that it will be called automatically by
   @link(al_exit) so you don't need to explicitly do it. *)
  PROCEDURE al_gl_exit;



(* Clear the option settings.  All settings are set to their default values,
   and marked as neither suggested not required.

   The mode setting routines will now ignore all of the settings other than
   those which you explicitly mark with @link(AL_GL_SUGGEST) or
   @link(AL_GL_REQUIRE).

   @bold(Note:)  You should not rely on what the default values actually are
   - don't mark settings unless you've also assigned something to them.

   Some settings are turned on by default.  @link(AL_GL_DOUBLEBUFFER),
   @link(AL_GL_RENDERMETHOD) and @link(AL_GL_FULLSCREEN) are set to
   @code(AL_GL_SUGGEST).
   @seealso(al_gl_set) @seealso(al_gl_get) *)
  PROCEDURE al_gl_clear_settings;

CONST
  AL_GL_DONTCARE = 0; (*< Ignore these settings. @seealso(al_gl_set) *)
  AL_GL_SUGGEST  = -1; (*< Prefer the assigned values for these settings. @seealso(al_gl_set) *)
  AL_GL_REQUIRE  = -2; (*< Reject other values for these settings. @seealso(al_gl_set) *)
(*@exclude Use Allegro-compatible framebuffer.
  This define is ignored (deprecated). *)
  AL_GL_ALLEGRO_FORMAT = $00000001;
(* Select the red depth of the frame buffer.  This defines the number of bits
   for the red component you'd like. The driver may or may not find a suitable
   mode. *)
  AL_GL_RED_DEPTH      = $00000002;
(* Select the green depth of the frame buffer.  This defines the number of bits
   for the green component you'd like. The driver may or may not find a
   suitable mode. *)
  AL_GL_GREEN_DEPTH    = $00000004;
(* Select the blue depth of the frame buffer. This defines the number of bits
   for the blue component you'd like. The driver may or may not find a suitable
   mode. *)
  AL_GL_BLUE_DEPTH     = $00000008;
(* Select the alpha depth of the frame buffer. This defines the number of bits
   for the alpha component you'd like. Not many video cards support destination
   alpha, so be aware that the you may not get an alpha depth at all. *)
  AL_GL_ALPHA_DEPTH    = $00000010;
(* Specify the total color depth of the frame buffer. The driver is free to
   select any combination of red, green, blue and alpha bits. *)
  AL_GL_COLOR_DEPTH    = $00000020;
(* Select the red depth of the accumulator buffer. This defines the number of
   bits to use for the red component. The driver may or may not find a suitable
   mode.  Note that on many video cards, the accumulator buffer is not
   accelerated. *)
  AL_GL_ACC_RED_DEPTH  = $00000040;
(* Select the green depth of the accumulator buffer. This defines the number of
   bits to use for the green component. The driver may or may not find a
   suitable mode.  Note that on many video cards, the accumulator buffer is not
   accelerated. *)
  AL_GL_ACC_GREEN_DEPTH= $00000080;
(* Select the blue depth of the accumulator buffer. This defines the number of
   bits to use for the blue component. The driver may or may not find a
   suitable mode.  Note that on many video cards, the accumulator buffer is not
   accelerated. *)
  AL_GL_ACC_BLUE_DEPTH = $00000100;
(* Select the alpha depth of the accumulator buffer. This defines the number of
   bits to use for the alpha component. The driver may or may not find a
   suitable mode.  Note that on many video cards, the accumulator buffer is not
   accelerated.  Not many cards support destination alpha either. *)
  AL_GL_ACC_ALPHA_DEPTH= $00000200;
(* Creates a backbuffer if set. The buffering mode may be double buffering or
   page flipping, depending on the driver settings.  OpenGL programs cannot
   chose the buffering mode themselves. *)
  AL_GL_DOUBLEBUFFER   = $00000400;
(* Creates seperate left/right buffers for stereo display.  Stereo display is
   used with special hardware (tipically glasses) for giving the illusion of
   depth by drawing the left and right buffers with a slight horizontal
   displacement.  This makes the display appear to heavily flicker without the
   special hardware.  Set to @code(NOT 0) for enabling stereo viewing. *)
  AL_GL_STEREO         = $00000800;
(* Creates additional auxiliary buffers.  This allows you to have more than one
   rendering context. Few video cards support this feature. *)
  AL_GL_AUX_BUFFERS    = $00001000;
(* Select the depth of the z-buffer.  The z-buffer is used to properly display
   polygons in 3D without recurring to sorting.  The higher the depth of the
   z-buffer, the more precise it is. *)
  AL_GL_Z_DEPTH        = $00002000;
(* Select the depth of the stencil buffer.  The stencil buffer is used to to do
   per-pixel testing (like the z-buffer), but of an arbitrary pattern instead
   of depth.  Please see the OpenGL documentation for details.  Newer cards
   support stenciling in hardware, but older cards (TNT2s, Voodoos, ATI Rage)
   do not. *)
  AL_GL_STENCIL_DEPTH  = $00004000;
(* Requests a placement of the window to a specified pixel location.  The
   driver may or may not honnor the request. *)
  AL_GL_WINDOW_X       = $00008000;
(* Same as @link(AL_GL_WINDOW_X), but for the y-axis. *)
  AL_GL_WINDOW_Y       = $00010000;
(* Set it if you'd like AllegroGL to pay special attention on whether hardware
   acceleration is present or not.  Notice however this isn't a guarentee that
   the OpenGL operations will be accelerated, but rather a request that the
   operations go through the video card's drivers instead of Microsoft's
   software renderer.  The video card driver is free to drop back to software
   mode if it deems it necessary.  This setting has no effect in X. *)
  AL_GL_RENDERMETHOD   = $00020000;
(* Define AllegroGL's policy relative to video memory usage.  Sometimes
   AllegroGL needs to create an internal 256x256 texture in order to perform
   graphics operations like masked_blit, draw_sprite and so on.  This parameter
   defines the policy of AllegroGL relative to the management of this texture.
   Several options are available :
   @unorderedList(@item(@link(AL_GL_RELEASE): internal texture is released in
     order to free video memory.)
     @item(@link(AL_GL_KEEP): internal texture is kept in video memory.  This
     option generally accelerate graphics operations when
     @link(al_gl_set_allegro_mode) and @link(al_gl_unset_allegro_mode) are
     often called.))
   System with few video memory should use @code(AL_GL_RELEASE) while others
   should use @code(AL_GL_KEEP) since it allows the internal texture to be
   created once.  Default value is @code(AL_GL_KEEP). *)
  AL_GL_VIDEO_MEMORY_POLICY = $00100000;
(* Define multisample parameters.  Some OpenGL ICDs expose an extension called
   @code(GL_ARB_multisample) which provides a mechanism to anti-alias all GL
   primitives:  points, lines, polygons, bitmaps and pixel rectangles.

   In order to get an AllegroGL mode which supports multisample, you have to
   set both @code(AL_GL_SAMPLE_BUFFERS) to 1 and @link(AL_GL_SAMPLES) to the
   number of desired samples for multisampling.

   Notice however that since this feature relies on several extensions
   (@code(GL_ARB_multisample) and @code(GLX_ARB_multisample) or
   @code(WGL_ARB_multisample)), it isn't guaranteed that AllegroGL will find a
   graphics mode that supports multisample:  many not-so-old video cards, like
   the GeForce 2, do not support multisampling

   Hence, if you want your app to be able to run on most platforms, you should
   not require this parameter.

   Set this value to 1 to enable multisampling.
   @seealso(AL_GL_SAMPLES) *)
  AL_GL_SAMPLE_BUFFERS= $00200000;
(* Define multisample samples.  Set this value to the number of samples that
   can be accepted in the multisample buffers.
   @seealso(AL_GL_SAMPLE_BUFFERS) *)
  AL_GL_SAMPLES       = $00400000;
(* Floating-point Color buffer. *)
  AL_GL_FLOAT_COLOR   = $00800000;
(* Floating-point Depth buffer. *)
  AL_GL_FLOAT_Z       = $01000000;
(* @exclude DO NOT USE *)
  AL_GL_CONFIG_RESERVED= $A000000;



(* Sets a configuration option.

   Use this routine to configure the framebuffer, @bold(before) setting a
   graphics mode.  Options are integer constants, and all values are
   effectively integers.

   Three of the options are special.  @link(AL_GL_SUGGEST) and
   @link(AL_GL_REQUIRE) are used to mark which of the other options are merely
   suggestions and which are absolute requirements.  If the OpenGL
   implementation can't provide a feature which you mark with
   @code(AL_GL_REQUIRE), the call to @link(al_set_gfx_mode) will fail.  If you
   don't mark an option as either suggested or required, that option will be
   ignored (@link(AL_GL_DONTCARE)). You can @code(OR) together the other
   constants when using one of these three options to indicate your preferences
   for several settings at one time.  Selecting an option as one of the
   suggestion modes will remove it from the others.  For example, if you first
   set the color depth to be required, but then decide that you want it to be
   suggested instead, then the option will be removed from the required
   settings.  Setting any option to @code(AL_GL_DONTCARE) will remove any
   previous setting attributed to it, and default values will be used if
   necessary.

   The remaining options are: @link(AL_GL_RED_DEPTH), @link(AL_GL_GREEN_DEPTH),
   @link(AL_GL_BLUE_DEPTH), @link(AL_GL_ALPHA_DEPTH), @link(AL_GL_COLOR_DEPTH),
   @link(AL_GL_ACC_RED_DEPTH), @link(AL_GL_ACC_GREEN_DEPTH),
   @link(AL_GL_ACC_BLUE_DEPTH), @link(AL_GL_ACC_ALPHA_DEPTH),
   @link(AL_GL_DOUBLEBUFFER), @link(AL_GL_STEREO), @link(AL_GL_AUX_BUFFERS),
   @link(AL_GL_Z_DEPTH), @link(AL_GL_STENCIL_DEPTH), @link(AL_GL_WINDOW_X),
   @link(AL_GL_WINDOW_Y), @link(AL_GL_RENDERMETHOD), @link(AL_GL_FULLSCREEN),
   @link(AL_GL_WINDOWED), @link(AL_GL_VIDEO_MEMORY_POLICY),
   @link(AL_GL_SAMPLE_BUFFERS), @link(AL_GL_SAMPLES), @link(AL_GL_FLOAT_COLOR),
   @link(AL_GL_FLOAT_Z)
   @param(option Selects which option to change.)
   @param(value The new option value.)
   @seealso(al_gl_get) @seealso(al_gl_clear_settings) *)
  PROCEDURE al_gl_set (option, value: LONGINT);

(* Like @link(al_gl_set) but to set @true/@false values. *)
  PROCEDURE al_gl_set_boolean (option: LONGINT; value: BOOLEAN);

(* Reads the setting of a configuration option.

   This routine can be used to read back the configuration of the framebuffer.
   You can do this either before setting a graphics mode to check what
   configuration you are requesting, or afterwards to find out what settings
   were actually used.
   @param(option The option to have its value returned.)
   @returns(The value of the option selected by the parameter, or -1 if the
     option is invalid.)
   @seealso(al_gl_set), @seealso(al_gl_clear_settings) *)
  FUNCTION al_gl_get (option: LONGINT): LONGINT;



VAR
(* OpenGL graphics driver for Allegro.

   Use @link(al_set_gfx_mode) to select an OpenGL mode as normal, but using
   e.g.  @code(AL_GFX_OPENGL) as the driver.  The virtual width and height are
   ignored.  To set the colour depth, use @code(al_gl_set @(AL_GL_COLOR_DEPTH,
   nn@)).  However if the color depth is not set by @link(al_gl_set), AllegroGL
   will refer to the value set by the last call to @link(al_set_color_depth).

   Allegro modes are still available.  Use of @link(AL_GFX_AUTODETECT) or
   @link(AL_GFX_AUTODETECT_WINDOWED) will select Allegro modes, and not OpenGL
   modes. *)
   AL_GFX_OPENGL_WINDOWED, AL_GFX_OPENGL_FULLSCREEN, AL_GFX_OPENGL: LONGINT;



(* Flips the front and back framebuffers.

  If you chose, or were given, a double buffered OpenGL mode, you have access
  to a front buffer, which is visible on the screen, and also a back buffer,
  which is not visible.  This routine flips the buffers, so that the contents
  of the back buffer is now the contents of the (visible) front buffer.  The
  contents of the backbuffer is undefined after the operation.

  Normally in these modes you would do all your drawing to the back buffer,
  without the user seeing the image while it's partially drawn, and then call
  this function to flip the buffers, allowing the user to see what you've
  drawn, now that it's finished, while you proceed to draw the next frame.

  When drawing to the screen bitmap, you may not be drawing to what user
  currently sees on his monitor.  It is recommended that you rebuild the screen
  every frame, then flip, then draw again.
  @seealso(al_gl_set) @seealso(AL_GL_DOUBLEBUFFER) *)
  PROCEDURE al_gl_flip;



IMPLEMENTATION

USES
  aldrv;

FUNCTION DefaultGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;
FORWARD;
FUNCTION FullscreenGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;
FORWARD;
FUNCTION WindowedGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;
FORWARD;



(* The configuration system.  This is common to all platforms. *)
VAR
  SuggestedSettings, RequiredSettings: LONGINT;



  PROCEDURE al_gl_clear_settings;
  BEGIN
    SuggestedSettings := 0;
    RequiredSettings := 0;
  END;



  PROCEDURE al_gl_set (option, value: LONGINT);
  BEGIN
    CASE option OF
    { Set importance of options. }
      AL_GL_REQUIRE:
      BEGIN
	SuggestedSettings := SuggestedSettings AND (NOT value);
	RequiredSettings  := SuggestedSettings OR value;
      END;
      AL_GL_SUGGEST:
      BEGIN
	RequiredSettings  := SuggestedSettings AND (NOT value);
	SuggestedSettings := SuggestedSettings OR value;
      END;
      AL_GL_DONTCARE:
      BEGIN
	RequiredSettings  := SuggestedSettings AND (NOT value);
	SuggestedSettings := SuggestedSettings AND (NOT value);
      END;
    { Set configuration. }
    END;
  END;



  PROCEDURE al_gl_set_boolean (option: LONGINT; value: BOOLEAN);
  BEGIN
    IF value THEN
      al_gl_set (option, -1)
    ELSE
      al_gl_set (option, 0);
  END;



  FUNCTION al_gl_get (option: LONGINT): LONGINT;
  BEGIN
    al_gl_get := 0;
    CASE option OF
      AL_GL_REQUIRE:
	al_gl_get := RequiredSettings;
      AL_GL_SUGGEST:
	al_gl_get := SuggestedSettings;
    END;
  END;



(* System core. *)
VAR
(* Driver information. *)
  OpenGLDriverList: ARRAY [0..3] OF __AL_DRIVER_INFO__;
(* To save the default drivers and restore them. *)
  DefaultAllegroGFXDrivers: __AL_GFX_DRIVER__ptr;

  DefaultOpenGLGraphicDriver,
  OpenGLWindowedDriver,
  OpenGLFullScreenDriver: __AL_GFX_DRIVER__;



(* Default OpenGL graphics driver. *)
  FUNCTION al_gl_init: BOOLEAN;
  BEGIN
    IF al_system_driver = NIL THEN
    BEGIN
      al_gl_init := FALSE;
      EXIT;
    END;
  { Set drivers information. }
    WITH DefaultOpenGLGraphicDriver DO
    BEGIN
      id := AL_GFX_OPENGL;
      name := ''; desc := '';
      ascii_name := 'OpenGL Default Driver';
      init := @DefaultGFXInit;
      windowed := 0;
    { Any other values is set to NIL or 0 by default. }
    END;
    WITH OpenGLWindowedDriver DO
    BEGIN
      id := AL_GFX_OPENGL_WINDOWED;
      name := ''; desc := '';
      ascii_name := 'OpenGL Windowed Driver';
      init := @WindowedGFXInit;
      windowed := -1;
    { Any other values is set to NIL or 0 by default. }
    END;
    WITH OpenGLFullScreenDriver DO
    BEGIN
      id := AL_GFX_OPENGL_FULLSCREEN;
      name := ''; desc := '';
      ascii_name := 'OpenGL Full Screen Driver';
      init := @OpenGLFullScreenDriver;
      windowed := 0;
    { Any other values is set to NIL or 0 by default. }
    END;

    OpenGLDriverList[0].id := AL_GFX_OPENGL;
    OpenGLDriverList[0].driver := @DefaultOpenGLGraphicDriver;
    OpenGLDriverList[0].driver := 0;

    OpenGLDriverList[1].id := AL_GFX_OPENGL_WINDOWED;
    OpenGLDriverList[1].driver := @OpenGLWindowedDriver;
    OpenGLDriverList[1].driver := 0;

    OpenGLDriverList[2].id := AL_GFX_OPENGL_FULLSCREEN;
    OpenGLDriverList[2].driver := @OpenGLFullScreenDriver;
    OpenGLDriverList[2].driver := 0;

    OpenGLDriverList[3].id := 0;
    OpenGLDriverList[3].driver := NIL;
    OpenGLDriverList[3].driver := 0;

  { Save Allegro GFX drivers and set the new ones. }
    DefaultAllegroGFXDrivers := al_system_driver^.gfx_drivers;
    al_system_driver^.gfx_drivers := @OpenGLDriverList[0];
  { Initial configuration. }
    al_gl_clear_settings;
    al_gl_init := TRUE
  END;



  PROCEDURE al_gl_exit;
  BEGIN
  { Recuperar los controladores de Allegro. }
    IF al_system_driver = NIL THEN EXIT;
    IF DefaultAllegroGFXDrivers = NIL THEN EXIT;
    al_system_driver^.gfx_drivers := DefaultAllegroGFXDrivers;
    DefaultAllegroGFXDrivers := NIL;
  END;



(* Graphics interface. *)

{ Includes platform dependent code. }
{$IFDEF UNIX}
  {$IFDEF DARWIN}
    {$ERROR Sorry but your platform isn't support by the OpenGL add-on. }
  {$ELSE}
    {$INCLUDE algl_x.inc}
  {$ENDIF}
{$ELSE}
  {$ERROR Sorry but your platform isn't support by the OpenGL add-on. }
{$ENDIF}



(* Implements a graphics initialization for AL_GFX_OPENGL. *)
  FUNCTION DefaultGFXInit (w, h, vw, vh, bpp: LONGINT): AL_BITMAPptr; CDECL;
  VAR
    Bmp: AL_BITMAPptr;
  BEGIN
    Bmp := NIL;
  { First try full-screen. }
    al_gfx_driver := @OpenGLFullScreenDriver;
    Bmp := OpenGLFullScreenDriver.init (w, h, vw, vh, bpp);
    IF Bmp <> NIL THEN
    BEGIN
      DefaultGFXInit := Bmp;
      EXIT
    END;
  { Then try windowed mode. }
    al_gfx_driver := @OpenGLWindowedDriver;
    DefaultGFXInit := OpenGLWindowedDriver.init (w, h, vw, vh, bpp);
  END;



  PROCEDURE al_gl_flip;
  BEGIN
    ;
  END;



INITIALIZATION
{ Set identifiers. }
  AL_GFX_OPENGL := AL_ID ('OGLD');
  AL_GFX_OPENGL_WINDOWED := AL_ID ('OGLW');
  AL_GFX_OPENGL_FULLSCREEN := AL_ID ('OGLF');
END.
