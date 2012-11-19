UNIT alblend;
(*<Truecolor transparency.

  In truecolor video modes, translucency and lighting are implemented by a
  blender function of the form:
  @longcode(#
       FUNCTION AL_BLENDER_FUNC (x, y, n: AL_ULONG): AL_ULONG; CDECL;
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

(*****************************************************************************
 * color.h
 *     Color manipulation routines.
 *)

  TYPE
  (* @seealso(alblend) @seealso(al_set_blender_mode) @seealso(al_set_blender_mode_ex) *)
    AL_BLENDER_FUNC = FUNCTION (x, y, n: AL_ULONG): AL_ULONG; CDECL;



(* Specifies a custom set of truecolor blender routines, which can be used to
   implement whatever special interpolation modes you need.  This function
   shares a single blender between the 24 and 32-bit modes.
   @seealso(al_set_blender_mode_ex) @seealso(al_set_trans_blender)
   @seealso(al_color_map) @seealso(al_draw_trans_sprite)
   @seealso(al_draw_lit_sprite) @seealso(al_drawing_mode)
 *)
  PROCEDURE al_set_blender_mode (b15, b16, b24: AL_BLENDER_FUNC; r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_blender_mode';

(* Like @link(al_set_blender_mode), but allows you to specify a more complete
   set of blender routines.  The @code(b15), @code(b16), @code(b24), and
   @code(b32) routines are used when drawing pixels onto destinations of the
   same format, while @code(b15x), @code(b16x), and @code(b24x) are used by
   @link(al_draw_trans_sprite) and @link(al_draw_trans_rle_sprite) when drawing
   RGBA images onto destination bitmaps of another format.  These blenders will
   be passed a 32-bit @code(x) parameter, along with a @code(y) value of a
   different color depth, and must try to do something sensible in response.
   @seealso(al_set_alpha_blender) *)
  PROCEDURE al_set_blender_mode_ex (b15, b16, b24, b32, b15x, b16x, b24x: AL_BLENDER_FUNC; r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_blender_mode_ex';



(* Enables the special alpha-channel blending mode, which is used for drawing
   32-bit RGBA sprites.  After calling this function, you can use
   @link(al_draw_trans_sprite) or @link(al_draw_trans_rle_sprite) to draw a
   32-bit source image onto any hicolor or truecolor destination.  The alpha
   values will be taken directly from the source graphic, so you can vary the
   solidity of each part of the image.  You can't use any of the normal
   translucency functions while this mode is active, though, so you should
   reset to one of the normal blender modes (eg. @link(al_set_trans_blender))
   before drawing anything other than 32-bit RGBA sprites.
   @seealso(al_set_write_alpha_blender) *)
  PROCEDURE al_set_alpha_blender;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_alpha_blender';

(* Enables the special alpha-channel editing mode, which is used for drawing
   alpha channels over the top of an existing 32-bit RGB sprite, to turn it
   into an RGBA format image.  After calling this function, you can set the
   drawing mode to @link(AL_DRAW_MODE_TRANS) and then write draw color values
   (0-255) onto a 32-bit image.  This will leave the color values unchanged,
   but alter the alpha to whatever values you are writing.  After enabling this
   mode you can also use @link(al_draw_trans_sprite) to superimpose an 8-bit
   alpha mask over the top of an existing 32-bit sprite.
   @seealso(al_set_alpha_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_write_alpha_blender;
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_write_alpha_blender';

(* Enables a linear interpolator blender mode for combining translucent or lit
   truecolor pixels.
   @seealso(al_set_blender_mode) @seealso(al_set_alpha_blender)
   @seealso(al_set_write_alpha_blender) @seealso(al_color_map)
   @seealso(al_draw_trans_sprite) @seealso(al_draw_lit_sprite)
   @seealso(al_drawing_mode) @seealso(al_set_add_blender)
   @seealso(al_set_burn_blender)
   @seealso(al_set_color_blender) @seealso(al_set_difference_blender)
   @seealso(al_set_dissolve_blender) @seealso(al_set_dodge_blender)
   @seealso(al_set_hue_blender) @seealso(al_set_invert_blender)
   @seealso(al_set_luminance_blender) @seealso(al_set_multiply_blender)
   @seealso(al_set_saturation_blender) @seealso(al_set_screen_blender)
 *)
  PROCEDURE al_set_trans_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_trans_blender';

(* Enables an additive blender mode for combining translucent or lit truecolor
   pixels.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_add_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_add_blender';

(* Enables a burn blender mode for combining translucent or lit truecolor
   pixels.  Here the lightness values of the colours of the source image reduce
   the lightness of the destination image, darkening the image.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_burn_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_burn_blender';

(* Enables a color blender mode for combining translucent or lit truecolor
   pixels.  Applies only the hue and saturation of the source image to the
   destination image.  The luminance of the destination image is not
   affected.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_color_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_color_blender';

(* Enables a difference blender mode for combining translucent or lit truecolor
   pixels.  This makes an image which has colours calculated by the difference
   between the source and destination colours.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_difference_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_difference_blender';

(* Enables a dissolve blender mode for combining translucent or lit truecolor
   pixels.  Randomly replaces the colours of some pixels in the destination
   image with those of the source image.  The number of pixels replaced depends
   on the alpha value (higher value, more pixels replaced; you get the idea
   :).
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_dissolve_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_dissolve_blender';

(* Enables a dodge blender mode for combining translucent or lit truecolor
    pixels.  The lightness of colours in the source lighten the colours of the
    destination.  White has the most effect; black has none.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_dodge_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_dodge_blender';

(* Enables a hue blender mode for combining translucent or lit truecolor
   pixels. This applies the hue of the source to the destination.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_hue_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_hue_blender';

(* Enables an invert blender mode for combining translucent or lit truecolor
   pixels.  Blends the inverse (or negative) colour of the source with the
   destination.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_invert_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_invert_blender';

(* Enables a luminance blender mode for combining translucent or lit truecolor
   pixels.  Applies the luminance of the source to the destination.  The colour
   of the destination is not affected.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_luminance_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_luminance_blender';

(* Enables a multiply blender mode for combining translucent or lit truecolor
   pixels.  Combines the source and destination images, multiplying the colours
   to produce a darker colour.  If a colour is multiplied by white it remains
   unchanged; when multiplied by black it also becomes black.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_multiply_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_multiply_blender';

(* Enables a saturation blender mode for combining translucent or lit truecolor
   pixels.  Applies the saturation of the source to the destination image.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_saturation_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_saturation_blender';

(* Enables a screen blender mode for combining translucent or lit truecolor
   pixels.  This blender mode lightens the colour of the destination image by
   multiplying the inverse of the source and destination colours.  Sort of like
   the opposite of the multiply blender mode.
   @seealso(al_set_trans_blender) @seealso(al_drawing_mode) *)
  PROCEDURE al_set_screen_blender (r, g, b, a: AL_INT);
    CDECL; EXTERNAL ALLEGRO_SHARED_LIBRARY_NAME NAME 'set_screen_blender';

IMPLEMENTATION

END.
