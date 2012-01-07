UNIT al5audio;

INTERFACE

{$include allegro.cfg}

  CONST
  (* Internal, used to communicate with acodec.
   * Must be in 512 <= n < 1024 *)
    _KCM_STREAM_FEEDER_QUIT_EVENT_TYPE = 512;

  (* User event type emitted when a stream fragment is ready to be
   * refilled with more audio data.
   * Must be in 512 <= n < 1024 *)
    ALLEGRO_EVENT_AUDIO_STREAM_FRAGMENT = 513;
    ALLEGRO_EVENT_AUDIO_STREAM_FINISHED = 514;

    ALLEGRO_MAX_CHANNELS = 8;

  TYPE
(* Enum: ALLEGRO_AUDIO_DEPTH
 *)
    ALLEGRO_AUDIO_DEPTH = (
   (* Sample depth and type, and signedness. Mixers only use 32-bit signed
    * float (-1..+1). The unsigned value is a bit-flag applied to the depth
    * value.
    *)
       ALLEGRO_AUDIO_DEPTH_INT8      = $00,
       ALLEGRO_AUDIO_DEPTH_INT16     = $01,
       ALLEGRO_AUDIO_DEPTH_INT24     = $02,
       ALLEGRO_AUDIO_DEPTH_FLOAT32   = $03,

       ALLEGRO_AUDIO_DEPTH_UNSIGNED  = $08,

   (* For convenience *)
       ALLEGRO_AUDIO_DEPTH_UINT8  = $08, { ALLEGRO_AUDIO_DEPTH_INT8  OR ALLEGRO_AUDIO_DEPTH_UNSIGNED }
       ALLEGRO_AUDIO_DEPTH_UINT16 = $09, { ALLEGRO_AUDIO_DEPTH_INT16 OR ALLEGRO_AUDIO_DEPTH_UNSIGNED }
       ALLEGRO_AUDIO_DEPTH_UINT24 = $0A  { ALLEGRO_AUDIO_DEPTH_INT24 OR ALLEGRO_AUDIO_DEPTH_UNSIGNED }
    );


  (* Enum: ALLEGRO_CHANNEL_CONF
   *)
    ALLEGRO_CHANNEL_CONF = (
    (* Speaker configuration (mono, stereo, 2.1, 3, etc). With regards to
     * behavior, most of this code makes no distinction between, say, 4.1 and
     * 5 speaker setups.. they both have 5 "channels". However, users would
     * like the distinction, and later when the higher-level stuff is added,
     * the differences will become more important. (v>>4)+(v&0xF) should yield
     * the total channel count.
     *)
      ALLEGRO_CHANNEL_CONF_1   = $10,
      ALLEGRO_CHANNEL_CONF_2   = $20,
      ALLEGRO_CHANNEL_CONF_3   = $30,
      ALLEGRO_CHANNEL_CONF_4   = $40,
      ALLEGRO_CHANNEL_CONF_5_1 = $51,
      ALLEGRO_CHANNEL_CONF_6_1 = $61,
      ALLEGRO_CHANNEL_CONF_7_1 = $71
    );


  (* Enum: ALLEGRO_PLAYMODE
   *)
    ALLEGRO_PLAYMODE = (
      ALLEGRO_PLAYMODE_ONCE   = $100,
      ALLEGRO_PLAYMODE_LOOP   = $101,
      ALLEGRO_PLAYMODE_BIDIR  = $102
    {
      _ALLEGRO_PLAYMODE_STREAM_ONCE   = 0x103,   /* internal */
      _ALLEGRO_PLAYMODE_STREAM_ONEDIR = 0x104    /* internal */
    }
    );


  (* Enum: ALLEGRO_MIXER_QUALITY
   *)
    ALLEGRO_MIXER_QUALITY = (
      ALLEGRO_MIXER_QUALITY_POINT   = $110,
      ALLEGRO_MIXER_QUALITY_LINEAR  = $111
    );

  CONST
  (* ALLEGRO_AUDIO_PAN_NONE *)
    ALLEGRO_AUDIO_PAN_NONE: SINGLE = -1000.0;

  TYPE
  (* Type: ALLEGRO_SAMPLE *)
    ALLEGRO_SAMPLEptr = POINTER;


  (* Type: ALLEGRO_SAMPLE_ID *)
    ALLEGRO_SAMPLE_IDptr = ^ALLEGRO_SAMPLE_ID;
    ALLEGRO_SAMPLE_ID = RECORD
      Index, _id: LONGINT;
    END;

  (* Type: ALLEGRO_SAMPLE_INSTANCE *)
    ALLEGRO_SAMPLE_INSTANCEptr = POINTER;

  (* Type: ALLEGRO_AUDIO_STREAM *)
    ALLEGRO_AUDIO_STREAMptr = POINTER;

  (* Type: ALLEGRO_MIXER *)
    ALLEGRO_MIXERptr = POINTER;

  (* Type: ALLEGRO_VOICE *)
    ALLEGRO_VOICEptr = POINTER;



(* Misc. audio functions *)
  FUNCTION al_install_audio: BOOLEAN; CDECL;
  PROCEDURE al_uninstall_audio; CDECL;
  FUNCTION al_is_audio_installed: BOOLEAN; CDECL;
  FUNCTION al_get_allegro_audio_version: LONGWORD; CDECL;

(* Sample functions *)
  FUNCTION al_create_sample (buf: POINTER; samples, freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF; free_buf: BOOLEAN):
    ALLEGRO_SAMPLEptr; CDECL;
  PROCEDURE al_destroy_sample (spl: ALLEGRO_SAMPLEptr); CDECL;
  FUNCTION al_load_sample (CONST filename: STRING): ALLEGRO_SAMPLEptr; CDECL;

(* Sample instance functions *)
  FUNCTION al_create_sample_instance (data: ALLEGRO_SAMPLEptr): ALLEGRO_SAMPLE_INSTANCEptr; CDECL;
  PROCEDURE al_destroy_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr); CDECL;

(*
ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_sample_frequency, (const ALLEGRO_SAMPLE *spl));
ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_sample_length, (const ALLEGRO_SAMPLE *spl));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_DEPTH, al_get_sample_depth, (const ALLEGRO_SAMPLE *spl));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_CHANNEL_CONF, al_get_sample_channels, (const ALLEGRO_SAMPLE *spl));
ALLEGRO_KCM_AUDIO_FUNC(void *, al_get_sample_data, (const ALLEGRO_SAMPLE *spl));

ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_sample_instance_frequency, (const ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_sample_instance_length, (const ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_sample_instance_position, (const ALLEGRO_SAMPLE_INSTANCE *spl));

ALLEGRO_KCM_AUDIO_FUNC(float, al_get_sample_instance_speed, (const ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(float, al_get_sample_instance_gain, (const ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(float, al_get_sample_instance_pan, (const ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(float, al_get_sample_instance_time, (const ALLEGRO_SAMPLE_INSTANCE *spl));

ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_DEPTH, al_get_sample_instance_depth, (const ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_CHANNEL_CONF, al_get_sample_instance_channels, (const ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_PLAYMODE, al_get_sample_instance_playmode, (const ALLEGRO_SAMPLE_INSTANCE *spl));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_get_sample_instance_playing, (const ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_get_sample_instance_attached, (const ALLEGRO_SAMPLE_INSTANCE *spl));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample_instance_position, (ALLEGRO_SAMPLE_INSTANCE *spl, unsigned int val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample_instance_length, (ALLEGRO_SAMPLE_INSTANCE *spl, unsigned int val));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample_instance_speed, (ALLEGRO_SAMPLE_INSTANCE *spl, float val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample_instance_gain, (ALLEGRO_SAMPLE_INSTANCE *spl, float val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample_instance_pan, (ALLEGRO_SAMPLE_INSTANCE *spl, float val));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample_instance_playmode, (ALLEGRO_SAMPLE_INSTANCE *spl, ALLEGRO_PLAYMODE val));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample_instance_playing, (ALLEGRO_SAMPLE_INSTANCE *spl, bool val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_detach_sample_instance, (ALLEGRO_SAMPLE_INSTANCE *spl));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample, (ALLEGRO_SAMPLE_INSTANCE *spl, ALLEGRO_SAMPLE *data));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_SAMPLE *, al_get_sample, (ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_play_sample_instance, (ALLEGRO_SAMPLE_INSTANCE *spl));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_stop_sample_instance, (ALLEGRO_SAMPLE_INSTANCE *spl));


/* Stream functions */
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_STREAM*, al_create_audio_stream, (size_t buffer_count,
      unsigned int samples, unsigned int freq,
      ALLEGRO_AUDIO_DEPTH depth, ALLEGRO_CHANNEL_CONF chan_conf));
ALLEGRO_KCM_AUDIO_FUNC(void, al_destroy_audio_stream, (ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(void, al_drain_audio_stream, (ALLEGRO_AUDIO_STREAM *stream));

ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_audio_stream_frequency, (const ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_audio_stream_length, (const ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_audio_stream_fragments, (const ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_available_audio_stream_fragments, (const ALLEGRO_AUDIO_STREAM *stream));

ALLEGRO_KCM_AUDIO_FUNC(float, al_get_audio_stream_speed, (const ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(float, al_get_audio_stream_gain, (const ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(float, al_get_audio_stream_pan, (const ALLEGRO_AUDIO_STREAM *stream));

ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_CHANNEL_CONF, al_get_audio_stream_channels, (const ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_DEPTH, al_get_audio_stream_depth, (const ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_PLAYMODE, al_get_audio_stream_playmode, (const ALLEGRO_AUDIO_STREAM *stream));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_get_audio_stream_playing, (const ALLEGRO_AUDIO_STREAM *spl));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_get_audio_stream_attached, (const ALLEGRO_AUDIO_STREAM *spl));

ALLEGRO_KCM_AUDIO_FUNC(void *, al_get_audio_stream_fragment, (const ALLEGRO_AUDIO_STREAM *stream));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_audio_stream_speed, (ALLEGRO_AUDIO_STREAM *stream, float val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_audio_stream_gain, (ALLEGRO_AUDIO_STREAM *stream, float val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_audio_stream_pan, (ALLEGRO_AUDIO_STREAM *stream, float val));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_audio_stream_playmode, (ALLEGRO_AUDIO_STREAM *stream, ALLEGRO_PLAYMODE val));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_audio_stream_playing, (ALLEGRO_AUDIO_STREAM *stream, bool val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_detach_audio_stream, (ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_audio_stream_fragment, (ALLEGRO_AUDIO_STREAM *stream, void *val));

ALLEGRO_KCM_AUDIO_FUNC(bool, al_rewind_audio_stream, (ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_seek_audio_stream_secs, (ALLEGRO_AUDIO_STREAM *stream, double time));
ALLEGRO_KCM_AUDIO_FUNC(double, al_get_audio_stream_position_secs, (ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(double, al_get_audio_stream_length_secs, (ALLEGRO_AUDIO_STREAM *stream));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_audio_stream_loop_secs, (ALLEGRO_AUDIO_STREAM *stream, double start, double end));

ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_EVENT_SOURCE *, al_get_audio_stream_event_source, (ALLEGRO_AUDIO_STREAM *stream));

/* Mixer functions */
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_MIXER*, al_create_mixer, (unsigned int freq,
      ALLEGRO_AUDIO_DEPTH depth, ALLEGRO_CHANNEL_CONF chan_conf));
ALLEGRO_KCM_AUDIO_FUNC(void, al_destroy_mixer, (ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_attach_sample_instance_to_mixer, (
   ALLEGRO_SAMPLE_INSTANCE *stream, ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_attach_audio_stream_to_mixer, (ALLEGRO_AUDIO_STREAM *stream,
   ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_attach_mixer_to_mixer, (ALLEGRO_MIXER *stream,
   ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_mixer_postprocess_callback, (
      ALLEGRO_MIXER *mixer,
      void ( *cb)(void *buf, unsigned int samples, void *data),
      void *data));

ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_mixer_frequency, (const ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_CHANNEL_CONF, al_get_mixer_channels, (const ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_DEPTH, al_get_mixer_depth, (const ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_MIXER_QUALITY, al_get_mixer_quality, (const ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_get_mixer_playing, (const ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_get_mixer_attached, (const ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_mixer_frequency, (ALLEGRO_MIXER *mixer, unsigned int val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_mixer_quality, (ALLEGRO_MIXER *mixer, ALLEGRO_MIXER_QUALITY val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_mixer_playing, (ALLEGRO_MIXER *mixer, bool val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_detach_mixer, (ALLEGRO_MIXER *mixer));

/* Voice functions */
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_VOICE*, al_create_voice, (unsigned int freq,
      ALLEGRO_AUDIO_DEPTH depth,
      ALLEGRO_CHANNEL_CONF chan_conf));
ALLEGRO_KCM_AUDIO_FUNC(void, al_destroy_voice, (ALLEGRO_VOICE *voice));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_attach_sample_instance_to_voice, (
   ALLEGRO_SAMPLE_INSTANCE *stream, ALLEGRO_VOICE *voice));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_attach_audio_stream_to_voice, (
   ALLEGRO_AUDIO_STREAM *stream, ALLEGRO_VOICE *voice ));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_attach_mixer_to_voice, (ALLEGRO_MIXER *mixer,
   ALLEGRO_VOICE *voice));
ALLEGRO_KCM_AUDIO_FUNC(void, al_detach_voice, (ALLEGRO_VOICE *voice));

ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_voice_frequency, (const ALLEGRO_VOICE *voice));
ALLEGRO_KCM_AUDIO_FUNC(unsigned int, al_get_voice_position, (const ALLEGRO_VOICE *voice));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_CHANNEL_CONF, al_get_voice_channels, (const ALLEGRO_VOICE *voice));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_DEPTH, al_get_voice_depth, (const ALLEGRO_VOICE *voice));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_get_voice_playing, (const ALLEGRO_VOICE *voice));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_voice_position, (ALLEGRO_VOICE *voice, unsigned int val));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_voice_playing, (ALLEGRO_VOICE *voice, bool val));


ALLEGRO_KCM_AUDIO_FUNC(size_t, al_get_channel_count, (ALLEGRO_CHANNEL_CONF conf));
ALLEGRO_KCM_AUDIO_FUNC(size_t, al_get_audio_depth_size, (ALLEGRO_AUDIO_DEPTH conf));
*)
   (* Simple audio layer *)
      FUNCTION al_reserve_samples (reserve_samples: LONGINT): BOOLEAN; CDECL;
(*
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_MIXER *, al_get_default_mixer, (void));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_default_mixer, (ALLEGRO_MIXER *mixer));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_restore_default_mixer, (void));
*)
   FUNCTION al_play_sample (data: ALLEGRO_SAMPLEptr; gain, pan, speed: SINGLE; loop: ALLEGRO_PLAYMODE; ret_id: ALLEGRO_SAMPLE_IDptr): BOOLEAN; CDECL;
(*
ALLEGRO_KCM_AUDIO_FUNC(void, al_stop_sample, (ALLEGRO_SAMPLE_ID *spl_id));
ALLEGRO_KCM_AUDIO_FUNC(void, al_stop_samples, (void));

/* File type handlers */
ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_sample_loader, (const char *ext,
	ALLEGRO_SAMPLE *( *loader)(const char *filename)));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_sample_saver, (const char *ext,
	bool ( *saver)(const char *filename, ALLEGRO_SAMPLE *spl)));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_audio_stream_loader, (const char *ext,
	ALLEGRO_AUDIO_STREAM *( *stream_loader)(const char *filename,
	    size_t buffer_count, unsigned int samples)));
       
ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_sample_loader_f, (const char *ext,
	ALLEGRO_SAMPLE *( *loader)(ALLEGRO_FILE *fp)));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_sample_saver_f, (const char *ext,
	bool ( *saver)(ALLEGRO_FILE *fp, ALLEGRO_SAMPLE *spl)));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_audio_stream_loader_f, (const char *ext,
	ALLEGRO_AUDIO_STREAM *( *stream_loader)(ALLEGRO_FILE *fp,
	    size_t buffer_count, unsigned int samples)));
*)


(*
ALLEGRO_KCM_AUDIO_FUNC(bool, al_save_sample, (const char *filename,
	ALLEGRO_SAMPLE *spl));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_STREAM *, al_load_audio_stream, (const char *filename,
	size_t buffer_count, unsigned int samples));
   
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_SAMPLE *, al_load_sample_f, (ALLEGRO_FILE* fp, const char *ident));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_save_sample_f, (ALLEGRO_FILE* fp, const char *ident,
	ALLEGRO_SAMPLE *spl));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_STREAM *, al_load_audio_stream_f, (ALLEGRO_FILE* fp, const char *ident,
	size_t buffer_count, unsigned int samples));

      *)
IMPLEMENTATION

(* Misc. audio functions *)
  FUNCTION al_install_audio: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_uninstall_audio; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_is_audio_installed: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_allegro_audio_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Sample functions *)
  FUNCTION al_create_sample (buf: POINTER; samples, freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF; free_buf: BOOLEAN):
    ALLEGRO_SAMPLEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_destroy_sample (spl: ALLEGRO_SAMPLEptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_load_sample (CONST filename: STRING): ALLEGRO_SAMPLEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Sample instance functions *)
  FUNCTION al_create_sample_instance (data: ALLEGRO_SAMPLEptr): ALLEGRO_SAMPLE_INSTANCEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_destroy_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Simple audio layer *)
   FUNCTION al_reserve_samples (reserve_samples: LONGINT): BOOLEAN; CDECL;
   EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

   FUNCTION al_play_sample (data: ALLEGRO_SAMPLEptr; gain, pan, speed: SINGLE; loop: ALLEGRO_PLAYMODE; ret_id: ALLEGRO_SAMPLE_IDptr): BOOLEAN; CDECL;
   EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

END.
