(* Defines some constants with the names of the library files. *)
CONST
{$IFDEF DEBUGMODE}
    _DBG_ = '-debug';
{$ELSE}
    _DBG_ = '';
{$ENDIF}

{$IF DEFINED(UNIX)}
  ALLEGRO_LIB_NAME            = 'liballegro'+_DBG_+'.so.5.0';
  ALLEGRO_ACODEC_LIB_NAME     = 'liballegro_acodec'+_DBG_+'.so.5.0';
  ALLEGRO_AUDIO_LIB_NAME      = 'liballegro_audio'+_DBG_+'.so.5.0';
  ALLEGRO_COLOR_LIB_NAME      = 'liballegro_color'+_DBG_+'.so.5.0';
  ALLEGRO_FONT_LIB_NAME       = 'liballegro_font'+_DBG_+'.so.5.0';
  ALLEGRO_IMAGE_LIB_NAME      = 'liballegro_image'+_DBG_+'.so.5.0';
  ALLEGRO_MAIN_LIB_NAME       = 'liballegro_main'+_DBG_+'.so.5.0';
  ALLEGRO_NATIVE_DLG_LIB_NAME = 'liballegro_dialog'+_DBG_+'.so.5.0';
  ALLEGRO_PRIMITIVES_LIB_NAME = 'liballegro_primitives'+_DBG_+'.so.5.0';
  ALLEGRO_TTF_LIB_NAME        = 'liballegro_ttf'+_DBG_+'.so.5.0';
{$ELSEIF DEFINED(WINDOWS)}
  {$IFDEF MONOLITH}
  {It uses monolith staticly linked version. }
    ALLEGRO_LIB_NAME            = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_ACODEC_LIB_NAME     = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_AUDIO_LIB_NAME      = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_COLOR_LIB_NAME      = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_FONT_LIB_NAME       = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_IMAGE_LIB_NAME      = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_MAIN_LIB_NAME       = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_NATIVE_DLG_LIB_NAME = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_PRIMITIVES_LIB_NAME = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
    ALLEGRO_TTF_LIB_NAME        = 'allegro-5.0.5-monolith-mt'+_DBG_+'.dll';
  {$ELSE}
  {It uses add-ons }
    ALLEGRO_LIB_NAME            = 'allegro'+_DBG_+'.5.0.dll';
    ALLEGRO_ACODEC_LIB_NAME     = 'allegro_acodec'+_DBG_+'.5.0.dll';
    ALLEGRO_AUDIO_LIB_NAME      = 'allegro_audio'+_DBG_+'.5.0.dll';
    ALLEGRO_COLOR_LIB_NAME      = 'allegro_color'+_DBG_+'.5.0.dll';
    ALLEGRO_FONT_LIB_NAME       = 'allegro_font'+_DBG_+'.5.0.dll';
    ALLEGRO_IMAGE_LIB_NAME      = 'allegro_image'+_DBG_+'.5.0.dll';
    ALLEGRO_MAIN_LIB_NAME       = 'allegro_main'+_DBG_+'.5.0.dll';
    ALLEGRO_NATIVE_DLG_LIB_NAME = 'allegro_dialog'+_DBG_+'.5.0.dll';
    ALLEGRO_PRIMITIVES_LIB_NAME = 'allegro_primitives'+_DBG_+'.5.0.dll';
    ALLEGRO_TTF_LIB_NAME        = 'allegro_primitives'+_DBG_+'.5.0.dll';
  {$ENDIF}
{$ENDIF}
