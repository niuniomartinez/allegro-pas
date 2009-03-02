/* Small test file to see if Allegro was installed. */
#include <allegro.h>
#include <stdlib.h>

int main (void)
{
  if (allegro_init() != 0)
      return EXIT_FAILURE;
  install_keyboard(); 
  if (set_gfx_mode(GFX_AUTODETECT_WINDOWED, 320, 200, 0, 0) != 0) {
      if (set_gfx_mode(GFX_SAFE, 320, 200, 0, 0) != 0) {
	 set_gfx_mode(GFX_TEXT, 0, 0, 0, 0);
	 allegro_message("Unable to set any graphic mode\n%s\n", allegro_error);
	 return EXIT_FAILURE;
      }
   }
   clear_bitmap(screen);
   textout_centre_ex(screen, font, "It works!", SCREEN_W/2, SCREEN_H/2, 7, -1);
   readkey();
   return EXIT_SUCCESS;
}
END_OF_MAIN ()

