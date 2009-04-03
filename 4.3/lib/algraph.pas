UNIT algraph;
(*< Graphics modes are the common denominator for most Allegro programs.  While
    it is possible to write platform specific programs using Allegro which
    don't set a graphic mode through the routines provided in this unit, these
    are not very common.

    The first thing to note is that due to the wide range of supported
    platforms, a graphic mode is the only way to safely communicate with the
    user.  When Allegro was a DOS only library (versions 3.x and previous), it
    was frequent for programmers to use functions from the runt-time library
    to communicate with the user, like calling @code(WriteLn) before setting a
    graphic mode or maybe @code(GetLn) to read the user's input.  However,
    what would happen for such a game running under Windows where there is no
    default console output or it may be hidden from the user?  Even if the game
    compiled successfully, it would be unplayable, especially if there was
    vital information for the user in those text only messages.

    Allegro provides the @link(al_message) function to deal with this problem,
    but this is not a very user friendly method of communicating with the user
    and its main purpose is displaying small error like messages when no
    graphic mode is available.  Therefore, the first thing your Allegro program
    should do is set a graphic mode, and from there on, use Allegro's text
    output routines to display messages to the user, just like
    `allegro.pas/examples/exhello.pp' does.

    Setting a graphic mode involves deciding how to allocate the memory of the
    video card for your program.  On some platforms this means creating a
    virtual screen bigger than the physical resolution to do hardware scrolling
    or page flipping.  Virtual screens can cause a lot of confusion, but they
    are really quite simple.  @bold(Warning:)  patronising explanation coming
    up, so you may wish to skip the rest of this paragraph.  Think of video
    memory as a rectangular piece of paper which is being viewed through a
    small hole (your monitor) in a bit of cardboard.  Since the paper is bigger
    than the hole you can only see part of it at any one time, but by sliding
    the cardboard around you can alter which portion of the image is visible.
    You could just leave the hole in one position and ignore the parts of video
    memory that aren't visible, but you can get all sorts of useful effects by
    sliding the screen window around, or by drawing images in a hidden part of
    video memory and then flipping across to display them.

    For example, you could select a 640x480 mode in which the monitor acts as a
    window onto a 1024x1024 virtual screen, and then move the visible screen
    around in this larger area (hardware scrolling).  Initially, with the
    visible screen positioned at the top left corner of video memory, this
    setup would look like:
    @longcode(#
          (0,0)------------(640,0)----(1024,0)
            |                  |           |
            |  visible screen  |           |
            |                  |           |
          (0,480)----------(640,480)       |
            |                              |
            |   the rest of video memory   |
            |                              |
          (0,1024)--------------------(1024,1024)
    #)
    With a virtual screen bigger than the visible screen you can perform smooth
    CPU inexpensive scrolling:  you draw your graphics once, and then only tell
    the video card to show a different portion of the screen.  However, virtual
    screens are not supported on all platforms, and on some they might be
    emulated through software, losing any performance.  On top of that, many
    video cards only allow horizontal scrolling in steps of 32 bytes.  This is
    not a problem if your game runs in 24 or 32 bit, but it tends to mean jerky
    scrolling for other color depths.

    The other reason you could use virtual screens for is page flipping.  This
    means showing one portion of the virtual screen while your program draws to
    the hidden one. When you finish, you show the part you have been drawing to
    and repeat the process with the area now hidden. The result is a perfectly
    smooth screen update without flickering or other graphical artifacts.

    Scrolling manually to one part of the video memory is one non portable way
    to accomplish this. The portable way is to use functions like
    @link(al_create_system_bitmap), @link(al_create_video_bitmap),
    @link(al_show_video_bitmap), etc.  These functions divide the memory of the
    video card in areas and switch between them, a feature supported on all
    platforms and video cards (given that they have enough memory for the
    screen resolutions you asked for).

    The last thing you need to know about setting a graphic mode are drivers.
    Each platform has a number of graphic drivers which support a different
    range of hardware or behave in different ways.  To avoid cluttering your
    own code with #ifdefs and dealing with drivers added after you release your
    program, Allegro provides several so called magic drivers.  These magic
    drivers don't really exists, they wrap around a specific kind of
    functionality.

    The magic drivers you can use are:  @link(AL_GFX_AUTODETECT),
    @link(AL_GFX_AUTODETECT_FULLSCREEN), @link(AL_GFX_AUTODETECT_WINDOWED),
    @link(AL_GFX_SAFE), @link(AL_GFX_TEXT).




 *)

{$H+}
{$IFDEF FPC}
{ Free Pascal. }
 {$MODE FPC}
 {$PACKRECORDS C}
{$ELSE}
{ Assumes Codegear Delphi/Turbo. }
 {$A-}
{$ENDIF}



INTERFACE

USES
  albase, albitmap;



CONST
(* Closes any previously opened graphics mode, making you unable to use the
   global variable @link(al_screen), and in those environments that have
   text modes, sets one previously used or the closest match to that (usually
   80x25).  With this driver the size parameters of @link(al_set_gfx_mode)
   don't mean anything, so you can leave them all to zero or any other number
   you prefer. *)
  AL_GFX_TEXT			= -1;
(* Allegro will try to set the specified resolution with the current color
   depth in fullscreen mode.  Failing that, it will try to repeat the same
   operation in windowed mode.  If the call to @link(al_set_gfx_mode)
   succeeds, you are guaranteed to have set the specified resolution in the
   current color depth, but you don't know if the program is running fullscreen
   or windowed. *)
  AL_GFX_AUTODETECT		=  0;
(* Allegro will try to set the specified resolution with the current color
   depth in fullscreen mode.  If that is not possible, @link(al_set_gfx_mode)
   will fail. *)
  AL_GFX_AUTODETECT_FULLSCREEN	=  1;
(* Allegro will try to set the specified resolution with the current color
   depth in a windowed mode.  If that is not possible, @link(al_set_gfx_mode)
   will fail.  When it comes to windowed modes, the `specified resolution'
   actually means the graphic area your program can draw on, without including
   window decorations (if any).  Note that in windowed modes running with a
   color depth other than the desktop may result in non optimal performance due
   to internal color conversions in the graphic driver. Use
   @link(al_desktop_color_depth) to your advantage in these situations. *)
  AL_GFX_AUTODETECT_WINDOWED	=  2;
(* Using this driver Allegro guarantees that a graphic mode will always be set
   correctly.  It will try to select the resolution that you request, and if
   that fails, it will fall back upon whatever mode is known to be reliable on
   the current platform (this is 640x480 resolution under Windows, the actual
   framebuffer's resolution under Linux if it's supported, etc).  If it
   absolutely cannot set any graphics mode at all, it will return negative as
   usual, meaning that there's no possible video output on the machine, and
   that you should abort your program immediately, possibly after notifying
   this to the user with @link(al_message).  This fake driver is useful for
   situations where you just want to get into some kind of workable display
   mode, and can't be bothered with trying multiple different resolutions and
   doing all the error checking yourself.  Note however, that after a
   successful call to @link(al_set_gfx_mode) with this driver, you cannot make
   any assumptions about the width, height or color depth of the screen:  your
   code will have to deal with this little detail. *)
  AL_GFX_SAFE			= $53414645; { AL_ID('S','A','F','E') }

(* @exclude Graphic capabilities *)
  AL_GFX_CAN_SCROLL			= $00000001;
{ @exclude }
  AL_GFX_CAN_TRIPLE_BUFFER		= $00000002;
{ @exclude }
  AL_GFX_HW_CURSOR			= $00000004;
(* Indicates that the normal opaque version of the @link(al_hline) function is
   implemented using a hardware accelerator.  This will improve the performance
   not only of @code(al_hline) itself, but also of many other functions that
   use it as a workhorse, for example @link(al_circlefill) and
   @link(al_floodfill). *)
  AL_GFX_HW_HLINE			= $00000008;
(* Indicates that the XOR version of the @link(al_hline) function, and any
   other functions that use it as a workhorse, are implemented using a
   hardware accelerator (see @link(AL_GFX_HW_HLINE)). *)
  AL_GFX_HW_HLINE_XOR			= $00000010;
(* Indicates that the solid and masked pattern modes of the @link(al_hline)
   function, and any other functions that use it as a workhorse, are
   implemented using a hardware accelerator (see @link(AL_GFX_HW_HLINE)). *)
  AL_GFX_HW_HLINE_SOLID_PATTERN		= $00000020;
(* Indicates that the copy pattern modes of the @link(al_hline) function, and
   any other functions that use it as a workhorse, are implemented using a
   hardware accelerator (see @link(AL_GFX_HW_HLINE)). *)
  AL_GFX_HW_HLINE_COPY_PATTERN		= $00000040;
(* Indicates that the opaque version of the @link(al_rectfill) function, the
   @link(al_clear_bitmap) routine, and @link(al_clear_to_color), are
   implemented using a hardware accelerator. *)
  AL_GFX_HW_FILL			= $00000080;
(* Indicates that the XOR version of the @link(al_rectfill) function is
   implemented using a hardware accelerator  (see @link(AL_GFX_HW_FILL)). *)
  AL_GFX_HW_FILL_XOR			= $00000100;
(* Indicates that the solid and masked pattern modes of the @link(al_rectfill)
   function is implemented using a hardware accelerator  (see
   @link(AL_GFX_HW_FILL)). *)
  AL_GFX_HW_FILL_SOLID_PATTERN		= $00000200;
(* Indicates that the copy pattern mode of the @link(al_rectfill) function
   is implemented using a hardware accelerator  (see @link(AL_GFX_HW_FILL)). *)
  AL_GFX_HW_FILL_COPY_PATTERN		= $00000400;
(* Indicates that the opaque mode @link(al_line) and @link(al_vline)
   functions are implemented using a hardware accelerator. *)
  AL_GFX_HW_LINE			= $00000800;
(* Indicates that the XOR version of the @link(al_line) and @link(al_vline)
   functions are implemented using a hardware accelerator. *)
  AL_GFX_HW_LINE_XOR			= $00001000;
{ @exclude }
  AL_GFX_HW_TRIANGLE			= $00002000;
{ @exclude }
  AL_GFX_HW_TRIANGLE_XOR		= $00004000;
(* Indicates that monochrome character expansion (for text drawing) is
   implemented using a hardware accelerator. *)
  AL_GFX_HW_GLYPH			= $00008000;
(* Indicates that blitting from one part of the screen to another is
   implemented using a hardware accelerator.  If this flag is set, blitting
   within the video memory will almost certainly be the fastest possible way to
   display an image, so it may be worth storing some of your more frequently
   used graphics in an offscreen portion of the video memory. *)
  AL_GFX_HW_VRAM_BLIT			= $00010000;
(* Indicates that the @link(al_masked_blit) routine is capable of a hardware
   accelerated copy from one part of video memory to another, and that
   @link(al_draw_sprite) will use a hardware copy when given a sub-bitmap of
   the screen or a video memory bitmap as the source image.  If this flag is
   set, copying within the video memory will almost certainly be the fastest
   possible way to display an image, so it may be worth storing some of your
   more frequently used sprites in an offscreen portion of the video memory.

   @bold(Warning:)  if this flag is not set, @code(al_masked_blit) and
   @code(al_draw_sprite) will not work correctly when used with a video memory
   source image!  You must only try to use these functions to copy within the
   video memory if they are supported in hardware. *)
  AL_GFX_HW_VRAM_BLIT_MASKED		= $00020000;
(* Indicates that blitting from a memory bitmap onto the screen is being
   accelerated in hardware. *)
  AL_GFX_HW_MEM_BLIT			= $00040000;
(* Indicates that the @link(al_masked_blit) and @link(al_draw_sprite)
   functions are being accelerated in hardware when the source image is a
   memory bitmap and the destination is the physical screen. *)
  AL_GFX_HW_MEM_BLIT_MASKED		= $00080000;
(* Indicates that blitting from a system bitmap onto the screen is being
   accelerated in hardware.  Note that some acceleration may be present even
   if this flag is not set, because system bitmaps can benefit from normal
   memory to screen blitting as well.  This flag will only be set if system
   bitmaps have further acceleration above and beyond what is provided by
   @link(AL_GFX_HW_MEM_BLIT). *)
  AL_GFX_HW_SYS_TO_VRAM_BLIT		= $00100000;
(* Indicates that the @link(al_masked_blit) and @link(al_draw_sprite)
   functions are being accelerated in hardware when the source image is a
   system bitmap and the destination is the physical screen.  Note that some
   acceleration may be present even if this flag is not set, because system
   bitmaps can benefit from normal memory to screen blitting as well.  This
   flag will only be set if system bitmaps have further acceleration above and
   beyond what is provided by @link(AL_GFX_HW_MEM_BLIT_MASKED). *)
  AL_GFX_HW_SYS_TO_VRAM_BLIT_MASKED	= $00200000;
{ @exclude }
  AL_GFX_SYSTEM_CURSOR			= $00400000;



VAR
(* Bitfield describing the capabilities of the current graphics driver and
   video hardware.  This may contain combination any of the @code(AL_GFX_* )
   flags. *)
  al_gfx_capabilities: LONGINT; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'gfx_capabilities';

(* Screen bitmap *)
  al_screen: AL_BITMAPptr; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'screen';
(* Screen size. *)
  AL_SCREEN_W, AL_SCREEN_H, AL_VIRTUAL_W, AL_VIRTUAL_H: LONGINT;



CONST
(* @exclude Define color conversion modes. *)
  AL_COLORCONV_NONE	= 0;

  AL_COLORCONV_8_TO_15	= 1; {< @exclude }
  AL_COLORCONV_8_TO_16	= 2; {< @exclude }
  AL_COLORCONV_8_TO_24	= 4; {< @exclude }
  AL_COLORCONV_8_TO_32	= 8; {< @exclude }

  AL_COLORCONV_15_TO_8	= $10; {< @exclude }
  AL_COLORCONV_15_TO_16	= $20; {< @exclude }
  AL_COLORCONV_15_TO_24	= $40; {< @exclude }
  AL_COLORCONV_15_TO_32	= $80; {< @exclude }

  AL_COLORCONV_16_TO_8	= $100; {< @exclude }
  AL_COLORCONV_16_TO_15	= $200; {< @exclude }
  AL_COLORCONV_16_TO_24	= $400; {< @exclude }
  AL_COLORCONV_16_TO_32	= $800; {< @exclude }

  AL_COLORCONV_24_TO_8	= $1000; {< @exclude }
  AL_COLORCONV_24_TO_15	= $2000; {< @exclude }
  AL_COLORCONV_24_TO_16	= $4000; {< @exclude }
  AL_COLORCONV_24_TO_32	= $8000; {< @exclude }

  AL_COLORCONV_32_TO_8	= $10000; {< @exclude }
  AL_COLORCONV_32_TO_15	= $20000; {< @exclude }
  AL_COLORCONV_32_TO_16	= $40000; {< @exclude }
  AL_COLORCONV_32_TO_24	= $80000; {< @exclude }

  AL_COLORCONV_32A_TO_8		= $100000; {< @exclude }
  AL_COLORCONV_32A_TO_15	= $200000; {< @exclude }
  AL_COLORCONV_32A_TO_16	= $400000; {< @exclude }
  AL_COLORCONV_32A_TO_24	= $800000; {< @exclude }

  AL_COLORCONV_DITHER_PAL	= $1000000; {< @exclude }
  AL_COLORCONV_DITHER_HI	= $2000000; {< @exclude }
  AL_COLORCONV_KEEP_TRANS	= $4000000; {< @exclude }

  AL_COLORCONV_DITHER	= AL_COLORCONV_DITHER_PAL OR AL_COLORCONV_DITHER_HI; {< @exclude }

  AL_COLORCONV_EXPAND_256	= AL_COLORCONV_8_TO_15 OR AL_COLORCONV_8_TO_16 OR AL_COLORCONV_8_TO_24 OR AL_COLORCONV_8_TO_32; {< @exclude }

  AL_COLORCONV_REDUCE_TO_256	= AL_COLORCONV_15_TO_8 OR AL_COLORCONV_16_TO_8 OR AL_COLORCONV_24_TO_8 OR AL_COLORCONV_32_TO_8 OR AL_COLORCONV_32A_TO_8; {< @exclude }

  AL_COLORCONV_EXPAND_15_TO_16	= AL_COLORCONV_15_TO_16; {< @exclude }

  AL_COLORCONV_REDUCE_16_TO_15	= AL_COLORCONV_16_TO_15; {< @exclude }

  AL_COLORCONV_EXPAND_HI_TO_TRUE = AL_COLORCONV_15_TO_24 OR AL_COLORCONV_15_TO_32 OR AL_COLORCONV_16_TO_24 OR AL_COLORCONV_16_TO_32; {< @exclude }

  AL_COLORCONV_REDUCE_TRUE_TO_HI = AL_COLORCONV_24_TO_15 OR AL_COLORCONV_24_TO_16 OR AL_COLORCONV_32_TO_15 OR AL_COLORCONV_32_TO_16; {< @exclude }

  AL_COLORCONV_24_EQUALS_32	= AL_COLORCONV_24_TO_32 OR AL_COLORCONV_32_TO_24; {< @exclude }

  AL_COLORCONV_TOTAL	= AL_COLORCONV_EXPAND_256 OR AL_COLORCONV_REDUCE_TO_256 OR AL_COLORCONV_EXPAND_15_TO_16 OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_EXPAND_HI_TO_TRUE OR AL_COLORCONV_REDUCE_TRUE_TO_HI OR AL_COLORCONV_24_EQUALS_32 OR AL_COLORCONV_32A_TO_15 OR AL_COLORCONV_32A_TO_16 OR AL_COLORCONV_32A_TO_24; {< @exclude }

  AL_COLORCONV_PARTIAL	= AL_COLORCONV_EXPAND_15_TO_16 OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_24_EQUALS_32; {< @exclude }

  AL_COLORCONV_MOST	= AL_COLORCONV_EXPAND_15_TO_16  OR AL_COLORCONV_REDUCE_16_TO_15 OR AL_COLORCONV_EXPAND_HI_TO_TRUE OR AL_COLORCONV_REDUCE_TRUE_TO_HI OR AL_COLORCONV_24_EQUALS_32; {< @exclude }

  AL_COLORCONV_KEEP_ALPHA	= AL_COLORCONV_TOTAL AND NOT (AL_COLORCONV_32A_TO_8 OR AL_COLORCONV_32A_TO_15 OR AL_COLORCONV_32A_TO_16 OR AL_COLORCONV_32A_TO_24); {< @exclude }



(* Sets the pixel format to be used by subsequent calls to
   @link(al_set_gfx_mode) and @link(al_create_bitmap).  Valid depths are 8 (the
   default), 15, 16, 24, and 32 bits.

   Note that the screen color depth won't change until the next successful
   call to @code(al_set_gfx_mode). *)
  PROCEDURE al_set_color_depth (depth: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_depth';

(* Returns the current pixel format.  This can be very useful to know in order
   to write generic functions which select a different code path internally
   depending on the color depth being used.

   Note that the function returns whatever value you may have set previously
   with @link(al_set_color_depth), which can be different from the current
   color depth of the @link(al_screen) global variable.  If you really need to
   know the color depth of the screen, use @link(al_bitmap_color_depth). *)
  FUNCTION al_get_color_depth: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color_depth';

(* Specifies how to convert images between the various color depths when reading
   graphics from external bitmap files or datafiles.  The mode is a bitmask
   specifying which types of conversion are allowed.  If the appropriate bit is
   set, data will be converted into the current pixel format (selected by
   calling the @link(al_set_color_depth) function), otherwise it will be left
   in the same format as the disk file, leaving you to convert it manually
   before the graphic can be displayed.  The default mode is total conversion,
   so that all images will be loaded in the appropriate format for the current
   video mode. Valid bit flags are:
   @longcode(#
          AL_COLORCONV_NONE                // disable all format
                                           // conversions
          AL_COLORCONV_8_TO_15             // expand 8-bit to 15-bit
          AL_COLORCONV_8_TO_16             // expand 8-bit to 16-bit
          AL_COLORCONV_8_TO_24             // expand 8-bit to 24-bit
          AL_COLORCONV_8_TO_32             // expand 8-bit to 32-bit
          AL_COLORCONV_15_TO_8             // reduce 15-bit to 8-bit
          AL_COLORCONV_15_TO_16            // expand 15-bit to 16-bit
          AL_COLORCONV_15_TO_24            // expand 15-bit to 24-bit
          AL_COLORCONV_15_TO_32            // expand 15-bit to 32-bit
          AL_COLORCONV_16_TO_8             // reduce 16-bit to 8-bit
          AL_COLORCONV_16_TO_15            // reduce 16-bit to 15-bit
          AL_COLORCONV_16_TO_24            // expand 16-bit to 24-bit
          AL_COLORCONV_16_TO_32            // expand 16-bit to 32-bit
          AL_COLORCONV_24_TO_8             // reduce 24-bit to 8-bit
          AL_COLORCONV_24_TO_15            // reduce 24-bit to 15-bit
          AL_COLORCONV_24_TO_16            // reduce 24-bit to 16-bit
          AL_COLORCONV_24_TO_32            // expand 24-bit to 32-bit
          AL_COLORCONV_32_TO_8             // reduce 32-bit RGB to 8-bit
          AL_COLORCONV_32_TO_15            // reduce 32-bit RGB to 15-bit
          AL_COLORCONV_32_TO_16            // reduce 32-bit RGB to 16-bit
          AL_COLORCONV_32_TO_24            // reduce 32-bit RGB to 24-bit
          AL_COLORCONV_32A_TO_8            // reduce 32-bit RGBA to 8-bit
          AL_COLORCONV_32A_TO_15           // reduce 32-bit RGBA to 15-bit
          AL_COLORCONV_32A_TO_16           // reduce 32-bit RGBA to 16-bit
          AL_COLORCONV_32A_TO_24           // reduce 32-bit RGBA to 24-bit
          AL_COLORCONV_DITHER_PAL          // dither when reducing to 8-bit
          AL_COLORCONV_DITHER_HI           // dither when reducing to
                                           // hicolor
          AL_COLORCONV_KEEP_TRANS          // keep original transparency
   #)
   For convenience, the following macros can be used to select common
   combinations of these flags:
   @longcode(#
          AL_COLORCONV_EXPAND_256          // expand 256-color to hi/truecolor
          AL_COLORCONV_REDUCE_TO_256       // reduce hi/truecolor to 256-color
          AL_COLORCONV_EXPAND_15_TO_16     // expand 15-bit hicolor to 16-bit
          AL_COLORCONV_REDUCE_16_TO_15     // reduce 16-bit hicolor to 15-bit
          AL_COLORCONV_EXPAND_HI_TO_TRUE   // expand 15/16-bit to 24/32-bit
          AL_COLORCONV_REDUCE_TRUE_TO_HI   // reduce 24/32-bit to 15/16-bit
          AL_COLORCONV_24_EQUALS_32        // convert between 24- and 32-bit
          AL_COLORCONV_TOTAL               // everything to current format
          AL_COLORCONV_PARTIAL             // convert 15 <-> 16-bit and
                                           // 24 <-> 32-bit
          AL_COLORCONV_MOST                // all but hi/truecolor <-> 256
          AL_COLORCONV_DITHER              // dither during all color reductions
          AL_COLORCONV_KEEP_ALPHA          // convert everything to current format
                                           // unless it would lose alpha information
   #)
   If you enable the @code(AL_COLORCONV_DITHER) flag, dithering will be
   performed whenever truecolor graphics are converted into a hicolor or
   paletted format, including by the @link(al_blit) function, and any
   automatic conversions that take place while reading graphics from disk.
   This can produce much better looking results, but is obviously slower than a
   direct conversion.

   If you intend using converted bitmaps with functions like
   @link(al_masked_blit) or @link(al_draw_sprite), you should specify the
   @code(AL_COLORCONV_KEEP_TRANS) flag.  It will ensure that the masked areas
   in the bitmap before and after the conversion stay exactly the same, by
   mapping transparent colors to each other and adjusting colors which would be
   converted to the transparent color otherwise.  It affects every
   @code(al_blit) operation between distinct pixel formats and every automatic
   conversion. *)
  PROCEDURE al_set_color_conversion (mode: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_conversion';

(* Returns the current color conversion mode. *)
  FUNCTION al_get_color_conversion: LONGINT; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'get_color_conversion';

(* Switches into graphics mode.  The card parameter should usually be one of
   the Allegro magic drivers (read introduction of this unit) or see the
   platform specific documentation for a list of the available drivers.  The
   @code(w) and @code(h) parameters specify what screen resolution you want.
   The color depth of the graphic mode has to be specified before calling this
   function with @link(al_set_color_depth).

   The @code(v_w) and @code(v_h) parameters specify the minimum virtual
   screen size, in case you need a large virtual screen for hardware scrolling
   or page flipping.  You should set them to zero if you don't care about the
   virtual screen size.

   When you call @code(al_set_gfx_mode), the @code(v_w) and @code(v_h)
   parameters represent the minimum size of virtual screen that is acceptable
   for your program.  The range of possible sizes is usually very restricted,
   and Allegro may end up creating a virtual screen much larger than the one
   you request.  Allowed sizes are driver dependent and some drivers do not
   allow virtual screens that are larger than the visible screen at all:  don't
   assume that whatever you pass will always work.

   Currently, using a big virtual screen for page flipping is considered bad
   practice.  There are platforms which don't support virtual screens bigger
   than the physical screen but can create different video pages to flip back
   and forth.  This means that, if you want page flipping and aren't going to
   use hardware scrolling, you should call @code(al_set_gfx_mode) with (0,0)
   as the virtual screen size and later create the different video pages with
   @link(al_create_video_bitmap).  Otherwise your program will be limited to
   the platforms supporting hardware scrolling.

   After you select a graphics mode, the physical and virtual screen sizes can
   be checked with the variables @link(AL_SCREEN_W), @link(AL_SCREEN_H),
   @link(AL_VIRTUAL_W), and @link(AL_VIRTUAL_H).

   @returns(@true on success.  On failure returns @false and stores a
     description of the problem in @link(al_error).) *)
  FUNCTION al_set_gfx_mode (card, w, h, v_w, v_h: LONGINT): BOOLEAN;

(* Screen bitmap. *)
  FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
  PROCEDURE al_acquire_screen;
  PROCEDURE al_release_screen;

(* Attempts to page flip the hardware screen to display the specified video
   bitmap object, which must be the same size as the physical screen, and
   should have been obtained by calling the @link(al_create_video_bitmap)
   function.

   Allegro will handle any necessary vertical retrace synchronisation when page
   flipping, so you don't need to call @link(al_vsync) before it.  This means
   that @code(al_show_video_bitmap) has the same time delay effects as
   @code(al_vsync) by default.  This can be adjusted with the "disable_vsync"
   config key in the @code([graphics]) section of allegro.cfg.

   @returns (zero on success and non-zero on failure.) *)
  FUNCTION al_show_video_bitmap (bmp: AL_BITMAPptr): LONGINT;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'show_video_bitmap';

(* Waits for a vertical retrace to begin.  The retrace happens when the
   electron beam in your monitor has reached the bottom of the screen and is
   moving back to the top ready for another scan.  During this short period the
   graphics card isn't sending any data to the monitor, so you can do things to
   it that aren't possible at other times, such as altering the palette without
   causing flickering (snow).  Allegro will automatically wait for a retrace
   before altering the palette or doing any hardware scrolling, though, so you
   don't normally need to bother with this function. *)
  PROCEDURE al_vsync; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'vsync';



IMPLEMENTATION

FUNCTION set_gfx_mode (card, w, h, v_w, v_h: LONGINT): LONGINT; CDECL;
  EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME;

FUNCTION al_set_gfx_mode (card, w, h, v_w, v_h: LONGINT): BOOLEAN;
VAR
  R: BOOLEAN;
BEGIN
  R := set_gfx_mode (card, w, h, v_w, v_h) = 0;
  IF R THEN
  BEGIN
    IF al_screen <> NIL THEN
    BEGIN
      AL_SCREEN_W := w;
      AL_SCREEN_H := h;
      AL_VIRTUAL_W := al_screen^.w;
      AL_VIRTUAL_H := al_screen^.h;
    END;
  END;
  al_set_gfx_mode := R;
END;



FUNCTION al_is_screen_bitmap (bmp: AL_BITMAPptr): BOOLEAN;
BEGIN
  al_is_screen_bitmap := al_is_same_bitmap (al_screen, bmp);
END;



PROCEDURE al_acquire_screen;
BEGIN
  IF al_screen <> NIL THEN
    al_screen^.vtable^.acquire (al_screen);
END;



PROCEDURE al_release_screen;
BEGIN
  IF al_screen <> NIL THEN
    al_screen^.vtable^.release (al_screen);
END;



END.

