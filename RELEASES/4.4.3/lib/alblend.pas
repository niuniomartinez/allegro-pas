UNIT alblend;
(*<Truecolor transparency.

  In truecolor video modes, translucency and lighting are implemented by a
  blender function of the form:
  @longcode(#
       FUNCTION AL_BLENDER_FUNC (x, y, n: DWORD): DWORD;
  #)
  For each pixel to be drawn, this routine is passed two color parameters
  @code(x) and @code(y), decomposes them into their red, green and blue
  components, combines them according to some mathematical transformation
  involving the interpolation factor @code(n), and then merges the result back
  into a single return color value, which will be used to draw the pixel onto
  the destination bitmap.

  The parameter @code(x) represents the blending modifier color and the
  parameter @code(y) represents the base color to be modified.  The
  interpolation factor @code(n) is in the range [0-255] and controls the
  solidity of the blending.

  When a translucent drawing function is used, @code(x) is the color of the
  source, @code(y) is the color of the bitmap being drawn onto and @code(n) is
  the alpha level that was passed to the function that sets the blending mode
  (the RGB triplet that was passed to this function is not taken into account).

  When a lit sprite drawing function is used, @code(x) is the color represented
  by the RGB triplet that was passed to the function that sets the blending
  mode (the alpha level that was passed to this function is not taken into
  account), @code(y) is the color of the sprite and @code(n) is the alpha level
  that was passed to the drawing function itself.

  Since these routines may be used from various different color depths, there
  are three such callbacks, one for use with 15-bit 5.5.5 pixels, one for 16
  bit 5.6.5 pixels, and one for 24-bit 8.8.8 pixels (this can be shared between
  the 24 and 32-bit code since the bit packing is the same). *)

{$INCLUDE allegro.cfg }

INTERFACE

USES
  albase;

TYPE
(* @code(alblend) @code(al_set_blender_mode) @code(al_set_blender_mode_ex) *)
  AL_BLENDER_FUNC = FUNCTION (x, y, n: DWORD): DWORD; CDECL;



(* Enables the special alpha-channel blending mode, which is used for drawing
   32-bit RGBA sprites.  After calling this function, you can use
   @code(al_draw_trans_sprite) or @code(al_draw_trans_rle_sprite) to draw a
   32-bit source image onto any hicolor or truecolor destination.  The alpha
   values will be taken directly from the source graphic, so you can vary the
   solidity of each part of the image.  You can't use any of the normal
   translucency functions while this mode is active, though, so you should
   reset to one of the normal blender modes (eg. @code(al_set_trans_blender))
   before drawing anything other than 32-bit RGBA sprites. *)
  PROCEDURE al_set_alpha_blender; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_alpha_blender';

(* Enables the special alpha-channel editing mode, which is used for drawing
   alpha channels over the top of an existing 32-bit RGB sprite, to turn it
   into an RGBA format image.  After calling this function, you can set the
   drawing mode to @code(AL_DRAW_MODE_TRANS) and then write draw color values
   (0-255) onto a 32-bit image.  This will leave the color values unchanged,
   but alter the alpha to whatever values you are writing.  After enabling this
   mode you can also use @code(al_draw_trans_sprite) to superimpose an 8-bit
   alpha mask over the top of an existing 32-bit sprite. *)
  PROCEDURE al_set_write_alpha_blender; CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_write_alpha_blender';

(* Enables a linear interpolator blender mode for combining translucent or lit
   truecolor pixels. *)
  PROCEDURE al_set_trans_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_trans_blender';

(* Enables an additive blender mode for combining translucent or lit truecolor
   pixels. *)
  PROCEDURE al_set_add_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_add_blender';

(* Enables a burn blender mode for combining translucent or lit truecolor
   pixels.  Here the lightness values of the colours of the source image reduce
   the lightness of the destination image, darkening the image. *)
  PROCEDURE al_set_burn_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_burn_blender';

(* Enables a color blender mode for combining translucent or lit truecolor
   pixels.  Applies only the hue and saturation of the source image to the
   destination image.  The luminance of the destination image is not
   affected. *)
  PROCEDURE al_set_color_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_blender';

(* Enables a difference blender mode for combining translucent or lit truecolor
   pixels.  This makes an image which has colours calculated by the difference
   between the source and destination colours. *)
  PROCEDURE al_set_difference_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_difference_blender';

(* Enables a dissolve blender mode for combining translucent or lit truecolor
   pixels.  Randomly replaces the colours of some pixels in the destination
   image with those of the source image.  The number of pixels replaced depends
   on the alpha value (higher value, more pixels replaced; you get the idea
   :). *)
  PROCEDURE al_set_dissolve_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_dissolve_blender';

(* Enables a dodge blender mode for combining translucent or lit truecolor
    pixels.  The lightness of colours in the source lighten the colours of the
    destination.  White has the most effect; black has none. *)
  PROCEDURE al_set_dodge_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_dodge_blender';

(* Enables a hue blender mode for combining translucent or lit truecolor
   pixels. This applies the hue of the source to the destination. *)
  PROCEDURE al_set_hue_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_hue_blender';

(* Enables an invert blender mode for combining translucent or lit truecolor
   pixels.  Blends the inverse (or negative) colour of the source with the
   destination. *)
  PROCEDURE al_set_invert_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_invert_blender';

(* Enables a luminance blender mode for combining translucent or lit truecolor
   pixels.  Applies the luminance of the source to the destination.  The colour
   of the destination is not affected. *)
  PROCEDURE al_set_luminance_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_luminance_blender';

(* Enables a multiply blender mode for combining translucent or lit truecolor
   pixels.  Combines the source and destination images, multiplying the colours
   to produce a darker colour.  If a colour is multiplied by white it remains
   unchanged; when multiplied by black it also becomes black. *)
  PROCEDURE al_set_multiply_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_multiply_blender';

(* Enables a saturation blender mode for combining translucent or lit truecolor
   pixels.  Applies the saturation of the source to the destination image. *)
  PROCEDURE al_set_saturation_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_saturation_blender';

(* Enables a screen blender mode for combining translucent or lit truecolor
   pixels.  This blender mode lightens the colour of the destination image by
   multiplying the inverse of the source and destination colours.  Sort of like
   the opposite of the multiply blender mode. *)
  PROCEDURE al_set_screen_blender (r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_screen_blender';

(* Specifies a custom set of truecolor blender routines, which can be used to
   implement whatever special interpolation modes you need.  This function
   shares a single blender between the 24 and 32-bit modes. *)
  PROCEDURE al_set_blender_mode (b15, b16, b24: AL_BLENDER_FUNC;
				 r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_blender_mode';

(* Like @code(al_set_blender_mode), but allows you to specify a more complete
   set of blender routines.  The @code(b15), @code(b16), @code(b24), and
   @code(b32) routines are used when drawing pixels onto destinations of the
   same format, while @code(b15x), @code(b16x), and @code(b24x) are used by
   @code(al_draw_trans_sprite) and @code(al_draw_trans_rle_sprite) when drawing
   RGBA images onto destination bitmaps of another format.  These blenders will
   be passed a 32-bit @code(x) parameter, along with a @code(y) value of a
   different color depth, and must try to do something sensible in response. *)
  PROCEDURE al_set_blender_mode_ex (b15, b16, b24, b32, b15x, b16x, b24x:
				  AL_BLENDER_FUNC; r, g, b, a: LONGINT); CDECL;
    EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_blender_mode_ex';



IMPLEMENTATION

END.
