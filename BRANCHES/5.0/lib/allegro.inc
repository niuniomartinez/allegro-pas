(* Defines some constants with the names of the library files. *)
CONST
{$IF DEFINED(UNIX)}
  ALLEGRO_LIB_NAME            = 'liballegro.so.5.0';
  ALLEGRO_AUDIO_LIB_NAME      = 'liballegro_audio.so.5.0';
  ALLEGRO_ACODEC_LIB_NAME     = 'liballegro_acodec.so.5.0';
  ALLEGRO_COLOR_LIB_NAME      = 'liballegro_color.so.5.0';
  ALLEGRO_FONT_LIB_NAME       = 'liballegro_font.so.5.0';
  ALLEGRO_IMAGE_LIB_NAME      = 'liballegro_image.so.5.0';
  ALLEGRO_MAIN_LIB_NAME       = 'liballegro_main.so.5.0';
  ALLEGRO_NATIVE_DLG_LIB_NAME = 'liballegro_dialog.so.5.0';
  ALLEGRO_PRIMITIVES_LIB_NAME = 'liballegro_primitives.so.5.0';
  ALLEGRO_TTF_LIB_NAME        = 'liballegro_ttf.so.5.0';
{$ELSEIF DEFINED(WINDOWS)}
  {$IFDEF MONOLITH}
  {It uses monolith staticly linked version. }
    ALLEGRO_LIB_NAME            = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_AUDIO_LIB_NAME      = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_ACODEC_LIB_NAME     = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_COLOR_LIB_NAME      = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_FONT_LIB_NAME       = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_IMAGE_LIB_NAME      = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_MAIN_LIB_NAME       = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_NATIVE_DLG_LIB_NAME = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_PRIMITIVES_LIB_NAME = 'allegro-5.0.5-monolith-mt-debug.dll';
    ALLEGRO_TTF_LIB_NAME        = 'allegro-5.0.5-monolith-mt-debug.dll';
  {$ELSE}
  {It uses add-ons }
    ALLEGRO_LIB_NAME            = 'allegro-debug-5.0.dll';
    ALLEGRO_AUDIO_LIB_NAME      = 'allegro_audio-debug-5.0.dll';
    ALLEGRO_ACODEC_LIB_NAME     = 'allegro_acodec-debug-5.0.dll';
    ALLEGRO_COLOR_LIB_NAME      = 'allegro_color-debug-5.0.dll';
    ALLEGRO_FONT_LIB_NAME       = 'allegro_font-debug-5.0.dll';
    ALLEGRO_IMAGE_LIB_NAME      = 'allegro_image-debug-5.0.dll';
    ALLEGRO_MAIN_LIB_NAME       = 'allegro_main-debug-5.0.dll';
    ALLEGRO_NATIVE_DLG_LIB_NAME = 'allegro_dialog-debug-5.0.dll';
    ALLEGRO_PRIMITIVES_LIB_NAME = 'allegro_primitives-debug-5.0.dll';
    ALLEGRO_TTF_LIB_NAME        = 'allegro_primitives-debug-5.0.dll';
  {$ENDIF}
{$ENDIF}
