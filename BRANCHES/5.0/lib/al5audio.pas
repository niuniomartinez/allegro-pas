UNIT al5audio;
(*         ______   ___    ___
 *        /\  _  \ /\_ \  /\_ \
 *        \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___
 *         \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\
 *          \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \
 *           \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/
 *            \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/
 *                                           /\____/
 *                                           \_/__/
 *
 *      See readme.txt for copyright information.
 *)


{$include allegro.cfg}

INTERFACE

  USES
    Allegro5;

{$include allegro.inc}

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
    ALLEGRO_AUDIO_DEPTH = (
    (* Sample depth and type, and signedness. Mixers only use 32-bit signed
     * float (-1..+1). The unsigned value is a bit-flag applied to the depth
     * value.
     *)
      ALLEGRO_AUDIO_DEPTH_INT8      := $00,
      ALLEGRO_AUDIO_DEPTH_INT16     := $01,
      ALLEGRO_AUDIO_DEPTH_INT24     := $02,
      ALLEGRO_AUDIO_DEPTH_FLOAT32   := $03,

      ALLEGRO_AUDIO_DEPTH_UNSIGNED  := $08,

    (* For convenience *)
      ALLEGRO_AUDIO_DEPTH_UINT16 := $09, { ALLEGRO_AUDIO_DEPTH_INT16 OR ALLEGRO_AUDIO_DEPTH_UNSIGNED, }
      ALLEGRO_AUDIO_DEPTH_UINT24 := $0A  { ALLEGRO_AUDIO_DEPTH_INT24 OR ALLEGRO_AUDIO_DEPTH_UNSIGNED  }
    );

  CONST
  (* For convenience *)
    ALLEGRO_AUDIO_DEPTH_UINT8  = ALLEGRO_AUDIO_DEPTH_UNSIGNED; { ALLEGRO_AUDIO_DEPTH_INT8 OR ALLEGRO_AUDIO_DEPTH_UNSIGNED, }



  TYPE
    ALLEGRO_CHANNEL_CONF = (
    (* Speaker configuration (mono, stereo, 2.1, 3, etc). With regards to
     * behavior, most of this code makes no distinction between, say, 4.1 and
     * 5 speaker setups.. they both have 5 "channels". However, users would
     * like the distinction, and later when the higher-level stuff is added,
     * the differences will become more important. (v>>4)+(v&0xF) should yield
     * the total channel count.
     *)
      ALLEGRO_CHANNEL_CONF_1   := $10,
      ALLEGRO_CHANNEL_CONF_2   := $20,
      ALLEGRO_CHANNEL_CONF_3   := $30,
      ALLEGRO_CHANNEL_CONF_4   := $40,
      ALLEGRO_CHANNEL_CONF_5_1 := $51,
      ALLEGRO_CHANNEL_CONF_6_1 := $61,
      ALLEGRO_CHANNEL_CONF_7_1 := $71
    );



    ALLEGRO_PLAYMODE = (
      ALLEGRO_PLAYMODE_ONCE   := $100,
      ALLEGRO_PLAYMODE_LOOP   := $101,
      ALLEGRO_PLAYMODE_BIDIR  := $102
    {
      _ALLEGRO_PLAYMODE_STREAM_ONCE   = 0x103,   /* internal */
      _ALLEGRO_PLAYMODE_STREAM_ONEDIR = 0x104    /* internal */
    }
    );



    ALLEGRO_MIXER_QUALITY = (
      ALLEGRO_MIXER_QUALITY_POINT   := $110,
      ALLEGRO_MIXER_QUALITY_LINEAR  := $111
    );

  CONST
    ALLEGRO_AUDIO_PAN_NONE: SINGLE = -1000.0;

  TYPE
    ALLEGRO_SAMPLEptr = POINTER;



    ALLEGRO_SAMPLE_IDptr = ^ALLEGRO_SAMPLE_ID;
    ALLEGRO_SAMPLE_ID = RECORD
      _index, _id: LONGINT;
    END;

    ALLEGRO_SAMPLE_INSTANCEptr = POINTER;

    ALLEGRO_AUDIO_STREAMptr = POINTER;

    ALLEGRO_MIXERptr = POINTER;

    ALLEGRO_VOICEptr = POINTER;

    ALLEGRO_MIXER_CALLBACK = PROCEDURE (buf: POINTER; samples: LONGWORD; data: POINTER); CDECL;

    ALLEGRO_SAMPLE_LOADER = FUNCTION (CONST filename: PCHAR): ALLEGRO_SAMPLEptr; CDECL;

    ALLEGRO_SAMPLE_SAVER = FUNCTION (CONST filename: PCHAR; spl: ALLEGRO_SAMPLEptr): BOOLEAN; CDECL;

    ALLEGRO_AUDIO_STREAM_LOADER = FUNCTION (CONST filename: PCHAR; buffer_count, samples: LONGWORD): ALLEGRO_AUDIO_STREAMptr;



(* Sample functions *)
  FUNCTION al_create_sample (buf: POINTER; samples, freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF; free_buf: BOOLEAN): ALLEGRO_SAMPLEptr; CDECL;
  PROCEDURE al_destroy_sample (spl: ALLEGRO_SAMPLEptr); CDECL;

(* Sample instance functions *)
  FUNCTION al_create_sample_instance (data: ALLEGRO_SAMPLEptr): ALLEGRO_SAMPLE_INSTANCEptr; CDECL;
  PROCEDURE al_destroy_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr); CDECL;

  FUNCTION al_get_sample_frequency (CONST spl: ALLEGRO_SAMPLEptr): LONGWORD; CDECL;
  FUNCTION al_get_sample_length (CONST spl: ALLEGRO_SAMPLEptr): LONGWORD; CDECL;
  FUNCTION al_get_sample_depth (CONST spl: ALLEGRO_SAMPLEptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  FUNCTION al_get_sample_channels (CONST spl: ALLEGRO_SAMPLEptr): ALLEGRO_CHANNEL_CONF; CDECL;
  FUNCTION al_get_sample_data (CONST spl: ALLEGRO_SAMPLEptr): POINTER; CDECL;

  FUNCTION al_get_sample_instance_frequency (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): LONGWORD; CDECL;
  FUNCTION al_get_sample_instance_length (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): LONGWORD; CDECL;
  FUNCTION al_get_sample_instance_position (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): LONGWORD; CDECL;

  FUNCTION al_get_sample_instance_speed (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): SINGLE; CDECL;
  FUNCTION al_get_sample_instance_gain (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): SINGLE; CDECL;
  FUNCTION al_get_sample_instance_pan (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): SINGLE; CDECL;
  FUNCTION al_get_sample_instance_time (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): SINGLE; CDECL;

  FUNCTION al_get_sample_instance_depth (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  FUNCTION al_get_sample_instance_channels (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_CHANNEL_CONF; CDECL;
  FUNCTION al_get_sample_instance_playmode (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_PLAYMODE; CDECL;

  FUNCTION al_get_sample_instance_playing (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;
  FUNCTION al_get_sample_instance_attached (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;

  FUNCTION al_set_sample_instance_position (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: LONGWORD): BOOLEAN; CDECL;
  FUNCTION al_set_sample_instance_length (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: LONGWORD): BOOLEAN; CDECL;

  FUNCTION al_set_sample_instance_speed (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: SINGLE): BOOLEAN; CDECL;
  FUNCTION al_set_sample_instance_gain (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: SINGLE): BOOLEAN; CDECL;
  FUNCTION al_set_sample_instance_pan (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: SINGLE): BOOLEAN; CDECL;

  FUNCTION al_set_sample_instance_playmode (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: ALLEGRO_PLAYMODE): BOOLEAN; CDECL;

  FUNCTION al_set_sample_instance_playing (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: BOOLEAN): BOOLEAN; CDECL;
  FUNCTION al_detach_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;

  FUNCTION al_set_sample (spl: ALLEGRO_SAMPLE_INSTANCEptr; data: ALLEGRO_SAMPLEptr): BOOLEAN; CDECL;
  FUNCTION al_get_sample (spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_SAMPLEptr; CDECL;
  FUNCTION al_play_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;
  FUNCTION al_stop_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;

(* Stream functions *)
  FUNCTION al_create_audio_stream (buffer_count, samples, freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_AUDIO_STREAMptr; CDECL;
  PROCEDURE al_destroy_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr); CDECL;
  PROCEDURE al_drain_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr); CDECL;

  FUNCTION al_get_audio_stream_frequency (CONST stream: ALLEGRO_AUDIO_STREAMptr): LONGWORD; CDECL;
  FUNCTION al_get_audio_stream_length (CONST stream: ALLEGRO_AUDIO_STREAMptr): LONGWORD; CDECL;
  FUNCTION al_get_audio_stream_fragments (CONST stream: ALLEGRO_AUDIO_STREAMptr): LONGWORD; CDECL;
  FUNCTION al_get_available_audio_stream_fragments (CONST stream: ALLEGRO_AUDIO_STREAMptr): LONGWORD; CDECL;

  FUNCTION al_get_audio_stream_speed (CONST stream: ALLEGRO_AUDIO_STREAMptr): SINGLE; CDECL;
  FUNCTION al_get_audio_stream_gain (CONST stream: ALLEGRO_AUDIO_STREAMptr): SINGLE; CDECL;
  FUNCTION al_get_audio_stream_pan (CONST stream: ALLEGRO_AUDIO_STREAMptr): SINGLE; CDECL;

  FUNCTION al_get_audio_stream_channels (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_CHANNEL_CONF; CDECL;
  FUNCTION al_get_audio_stream_depth (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  FUNCTION al_get_audio_stream_playmode (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_PLAYMODE; CDECL;

  FUNCTION al_get_audio_stream_playing (CONST spl: ALLEGRO_AUDIO_STREAMptr): BOOLEAN; CDECL;
  FUNCTION al_get_audio_stream_attached (CONST spl: ALLEGRO_AUDIO_STREAMptr): BOOLEAN; CDECL;

  FUNCTION al_get_audio_stream_fragment (CONST stream: ALLEGRO_AUDIO_STREAMptr): POINTER; CDECL;

  FUNCTION al_set_audio_stream_speed (stream: ALLEGRO_AUDIO_STREAMptr; val: SINGLE): BOOLEAN; CDECL;
  FUNCTION al_set_audio_stream_gain (stream: ALLEGRO_AUDIO_STREAMptr; val: SINGLE): BOOLEAN; CDECL;
  FUNCTION al_set_audio_stream_pan (stream: ALLEGRO_AUDIO_STREAMptr; val: SINGLE): BOOLEAN; CDECL;

  FUNCTION al_set_audio_stream_playmode (stream: ALLEGRO_AUDIO_STREAMptr; val: ALLEGRO_PLAYMODE): BOOLEAN; CDECL;

  FUNCTION al_set_audio_stream_playing (stream: ALLEGRO_AUDIO_STREAMptr; val: BOOLEAN): BOOLEAN; CDECL;
  FUNCTION al_detach_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr): BOOLEAN; CDECL;
  FUNCTION al_set_audio_stream_fragment (stream: ALLEGRO_AUDIO_STREAMptr; val: POINTER): BOOLEAN; CDECL;

  FUNCTION al_rewind_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr): BOOLEAN; CDECL;
  FUNCTION al_seek_audio_stream_secs (stream: ALLEGRO_AUDIO_STREAMptr; time: DOUBLE): BOOLEAN; CDECL;
  FUNCTION al_get_audio_stream_position_secs (stream: ALLEGRO_AUDIO_STREAMptr): DOUBLE; CDECL;
  FUNCTION al_get_audio_stream_length_secs (stream: ALLEGRO_AUDIO_STREAMptr): DOUBLE; CDECL;
  FUNCTION al_set_audio_stream_loop_secs (stream: ALLEGRO_AUDIO_STREAMptr; start, finish: DOUBLE): BOOLEAN; CDECL;

  FUNCTION al_get_audio_stream_event_source (stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_EVENT_SOURCEptr; CDECL;

(* Mixer functions *)
  FUNCTION al_create_mixer (freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_MIXERptr; CDECL;
  PROCEDURE al_destroy_mixer (mixer: ALLEGRO_MIXERptr); CDECL;
  FUNCTION al_attach_sample_instance_to_mixer (sample: ALLEGRO_SAMPLE_INSTANCEptr; mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  FUNCTION al_attach_audio_stream_to_mixer (stream: ALLEGRO_AUDIO_STREAMptr; mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  FUNCTION al_attach_mixer_to_mixer (mixerA, mixerB: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  FUNCTION al_set_mixer_postprocess_callback (mixer: ALLEGRO_MIXERptr; cb: ALLEGRO_MIXER_CALLBACK; data: POINTER): BOOLEAN; CDECL;

  FUNCTION al_get_mixer_frequency (CONST mixer: ALLEGRO_MIXERptr): LONGWORD; CDECL;
  FUNCTION al_get_mixer_channels (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_CHANNEL_CONF; CDECL;
  FUNCTION al_get_mixer_depth (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  FUNCTION al_get_mixer_quality (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_MIXER_QUALITY; CDECL;
  FUNCTION al_get_mixer_playing (CONST mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  FUNCTION al_get_mixer_attached (CONST mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  FUNCTION al_set_mixer_frequency (mixer: ALLEGRO_MIXERptr; val: LONGWORD): BOOLEAN; CDECL;
  FUNCTION al_set_mixer_quality (mixer: ALLEGRO_MIXERptr; val: ALLEGRO_MIXER_QUALITY): BOOLEAN; CDECL;
  FUNCTION al_set_mixer_playing (mixer: ALLEGRO_MIXERptr; val: BOOLEAN): BOOLEAN; CDECL;
  FUNCTION al_detach_mixer (mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;

(* Voice functions *)
  FUNCTION al_create_voice (freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_VOICEptr; CDECL;
  PROCEDURE al_destroy_voice (voice: ALLEGRO_VOICEptr); CDECL;
  FUNCTION al_attach_sample_instance_to_voice (sample: ALLEGRO_SAMPLE_INSTANCEptr; voice: ALLEGRO_VOICEptr): BOOLEAN; CDECL;
  FUNCTION al_attach_audio_stream_to_voice (stream: ALLEGRO_AUDIO_STREAMptr; voice: ALLEGRO_VOICEptr): BOOLEAN; CDECL;
  FUNCTION al_attach_mixer_to_voice (mixer: ALLEGRO_MIXERptr; voice: ALLEGRO_VOICEptr): BOOLEAN; CDECL;
  PROCEDURE al_detach_voice (voice: ALLEGRO_VOICEptr); CDECL;

  FUNCTION al_get_voice_frequency (CONST voice: ALLEGRO_VOICEptr): LONGWORD; CDECL;
  FUNCTION al_get_voice_position (CONST voice: ALLEGRO_VOICEptr): LONGWORD; CDECL;
  FUNCTION al_get_voice_channels (CONST voice: ALLEGRO_VOICEptr): ALLEGRO_CHANNEL_CONF; CDECL;
  FUNCTION al_get_voice_depth (CONST voice: ALLEGRO_VOICEptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  FUNCTION al_get_voice_playing (CONST voice: ALLEGRO_VOICEptr): BOOLEAN; CDECL;
  FUNCTION al_set_voice_position (voice: ALLEGRO_VOICEptr; val: LONGWORD): BOOLEAN; CDECL;
  FUNCTION al_set_voice_playing (voice: ALLEGRO_VOICEptr; val: BOOLEAN): BOOLEAN; CDECL;

(* Misc. audio functions *)
  FUNCTION al_install_audio: BOOLEAN; CDECL;
  PROCEDURE al_uninstall_audio; CDECL;
  FUNCTION al_is_audio_installed: BOOLEAN; CDECL;
  FUNCTION al_get_allegro_audio_version: LONGWORD; CDECL;

  FUNCTION al_get_channel_count (conf: ALLEGRO_CHANNEL_CONF): LONGWORD; CDECL;
  FUNCTION al_get_audio_depth_size (conf: ALLEGRO_AUDIO_DEPTH): LONGWORD; CDECL;

(* Simple audio layer *)
  FUNCTION al_reserve_samples (reserve_samples: LONGINT): BOOLEAN; CDECL;
  FUNCTION al_get_default_mixer: ALLEGRO_MIXERptr; CDECL;
  FUNCTION al_set_default_mixer (mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  FUNCTION al_restore_default_mixer: BOOLEAN; CDECL;
  FUNCTION al_play_sample (data: ALLEGRO_SAMPLEptr; gain, pan, speed: SINGLE; loop: ALLEGRO_PLAYMODE; ret_id: ALLEGRO_SAMPLE_IDptr): BOOLEAN; CDECL;
  PROCEDURE al_stop_sample (spl_id: ALLEGRO_SAMPLE_IDptr); CDECL;
  PROCEDURE al_stop_samples; CDECL;

(* File type handlers *)
  FUNCTION al_register_sample_loader (CONST ext: STRING; loader: ALLEGRO_SAMPLE_LOADER): BOOLEAN; CDECL;
  FUNCTION al_register_sample_saver (CONST ext: STRING; saver: ALLEGRO_SAMPLE_SAVER): BOOLEAN; CDECL;
  FUNCTION al_register_audio_stream_loader (CONST ext: STRING; stream_loader: ALLEGRO_AUDIO_STREAM_LOADER): BOOLEAN; CDECL;

  {TODO: These needs Allegro's file access.

ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_sample_loader_f, (const char *ext,
	ALLEGRO_SAMPLE *( *loader)(ALLEGRO_FILE *fp)));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_sample_saver_f, (const char *ext,
	bool ( *saver)(ALLEGRO_FILE *fp, ALLEGRO_SAMPLE *spl)));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_register_audio_stream_loader_f, (const char *ext,
	ALLEGRO_AUDIO_STREAM *( *stream_loader)(ALLEGRO_FILE *fp,
	    size_t buffer_count, unsigned int samples)));
	  }

  FUNCTION al_load_sample (CONST filename: STRING): ALLEGRO_SAMPLEptr; CDECL;
  FUNCTION al_save_sample (CONST filename: STRING; spl: ALLEGRO_SAMPLEptr): BOOLEAN; CDECL;
  FUNCTION al_load_audio_stream (CONST filename: STRING; buffer_count, samples: LONGWORD): ALLEGRO_AUDIO_STREAMptr; CDECL;

  {TODO: These needs Allegro's file access.

ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_SAMPLE *, al_load_sample_f, (ALLEGRO_FILE* fp, const char *ident));
ALLEGRO_KCM_AUDIO_FUNC(bool, al_save_sample_f, (ALLEGRO_FILE* fp, const char *ident,
	ALLEGRO_SAMPLE *spl));
ALLEGRO_KCM_AUDIO_FUNC(ALLEGRO_AUDIO_STREAM *, al_load_audio_stream_f, (ALLEGRO_FILE* fp, const char *ident,
	size_t buffer_count, unsigned int samples));
      }

IMPLEMENTATION

(* Sample functions *)
  FUNCTION al_create_sample (buf: POINTER; samples, freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF; free_buf: BOOLEAN):
    ALLEGRO_SAMPLEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_destroy_sample (spl: ALLEGRO_SAMPLEptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Sample instance functions *)
  FUNCTION al_create_sample_instance (data: ALLEGRO_SAMPLEptr): ALLEGRO_SAMPLE_INSTANCEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_destroy_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_frequency (CONST spl: ALLEGRO_SAMPLEptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_length (CONST spl: ALLEGRO_SAMPLEptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_depth (CONST spl: ALLEGRO_SAMPLEptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_channels (CONST spl: ALLEGRO_SAMPLEptr): ALLEGRO_CHANNEL_CONF; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_data (CONST spl: ALLEGRO_SAMPLEptr): POINTER; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_frequency (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_length (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_position (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_speed (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): SINGLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_gain (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): SINGLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_pan (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): SINGLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_time (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): SINGLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_depth (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_channels (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_CHANNEL_CONF; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_playmode (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_PLAYMODE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_playing (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_attached (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_position (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: LONGWORD): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_length (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: LONGWORD): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_speed (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_gain (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_pan (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_playmode (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: ALLEGRO_PLAYMODE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_playing (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: BOOLEAN): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_detach_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample (spl: ALLEGRO_SAMPLE_INSTANCEptr; data: ALLEGRO_SAMPLEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample (spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_SAMPLEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_play_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_stop_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Stream functions *)
  FUNCTION al_create_audio_stream (buffer_count, samples, freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_AUDIO_STREAMptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_destroy_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_drain_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_frequency (CONST stream: ALLEGRO_AUDIO_STREAMptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_length (CONST stream: ALLEGRO_AUDIO_STREAMptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_fragments (CONST stream: ALLEGRO_AUDIO_STREAMptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_available_audio_stream_fragments (CONST stream: ALLEGRO_AUDIO_STREAMptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_speed (CONST stream: ALLEGRO_AUDIO_STREAMptr): SINGLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_gain (CONST stream: ALLEGRO_AUDIO_STREAMptr): SINGLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_pan (CONST stream: ALLEGRO_AUDIO_STREAMptr): SINGLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_channels (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_CHANNEL_CONF; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_depth (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_playmode (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_PLAYMODE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_playing (CONST spl: ALLEGRO_AUDIO_STREAMptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_attached (CONST spl: ALLEGRO_AUDIO_STREAMptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_fragment (CONST stream: ALLEGRO_AUDIO_STREAMptr): POINTER; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_speed (stream: ALLEGRO_AUDIO_STREAMptr; val: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_gain (stream: ALLEGRO_AUDIO_STREAMptr; val: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_pan (stream: ALLEGRO_AUDIO_STREAMptr; val: SINGLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_playmode (stream: ALLEGRO_AUDIO_STREAMptr; val: ALLEGRO_PLAYMODE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_playing (stream: ALLEGRO_AUDIO_STREAMptr; val: BOOLEAN): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_detach_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_fragment (stream: ALLEGRO_AUDIO_STREAMptr; val: POINTER): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_rewind_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_seek_audio_stream_secs (stream: ALLEGRO_AUDIO_STREAMptr; time: DOUBLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_position_secs (stream: ALLEGRO_AUDIO_STREAMptr): DOUBLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_length_secs (stream: ALLEGRO_AUDIO_STREAMptr): DOUBLE; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_loop_secs (stream: ALLEGRO_AUDIO_STREAMptr; start, finish: DOUBLE): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_event_source (stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_EVENT_SOURCEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Mixer functions *)
  FUNCTION al_create_mixer (freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_MIXERptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_destroy_mixer (mixer: ALLEGRO_MIXERptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_attach_sample_instance_to_mixer (sample: ALLEGRO_SAMPLE_INSTANCEptr; mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_attach_audio_stream_to_mixer (stream: ALLEGRO_AUDIO_STREAMptr; mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_attach_mixer_to_mixer (mixerA, mixerB: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_mixer_postprocess_callback (mixer: ALLEGRO_MIXERptr; cb: ALLEGRO_MIXER_CALLBACK; data: POINTER): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_mixer_frequency (CONST mixer: ALLEGRO_MIXERptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_mixer_channels (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_CHANNEL_CONF; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_mixer_depth (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_mixer_quality (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_MIXER_QUALITY; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_mixer_playing (CONST mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_mixer_attached (CONST mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_mixer_frequency (mixer: ALLEGRO_MIXERptr; val: LONGWORD): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_mixer_quality (mixer: ALLEGRO_MIXERptr; val: ALLEGRO_MIXER_QUALITY): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_mixer_playing (mixer: ALLEGRO_MIXERptr; val: BOOLEAN): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_detach_mixer (mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_create_voice (freq: LONGWORD; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_VOICEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_destroy_voice (voice: ALLEGRO_VOICEptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_attach_sample_instance_to_voice (sample: ALLEGRO_SAMPLE_INSTANCEptr; voice: ALLEGRO_VOICEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_attach_audio_stream_to_voice (stream: ALLEGRO_AUDIO_STREAMptr; voice: ALLEGRO_VOICEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_attach_mixer_to_voice (mixer: ALLEGRO_MIXERptr; voice: ALLEGRO_VOICEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_detach_voice (voice: ALLEGRO_VOICEptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_voice_frequency (CONST voice: ALLEGRO_VOICEptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_voice_position (CONST voice: ALLEGRO_VOICEptr): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_voice_channels (CONST voice: ALLEGRO_VOICEptr): ALLEGRO_CHANNEL_CONF; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_voice_depth (CONST voice: ALLEGRO_VOICEptr): ALLEGRO_AUDIO_DEPTH; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_voice_playing (CONST voice: ALLEGRO_VOICEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_voice_position (voice: ALLEGRO_VOICEptr; val: LONGWORD): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_voice_playing (voice: ALLEGRO_VOICEptr; val: BOOLEAN): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Misc. audio functions *)
  FUNCTION al_install_audio: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_uninstall_audio; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_is_audio_installed: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_allegro_audio_version: LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_channel_count (conf: ALLEGRO_CHANNEL_CONF): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_depth_size (conf: ALLEGRO_AUDIO_DEPTH): LONGWORD; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Simple audio layer *)
  FUNCTION al_reserve_samples (reserve_samples: LONGINT): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_default_mixer: ALLEGRO_MIXERptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_default_mixer (mixer: ALLEGRO_MIXERptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_restore_default_mixer: BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_play_sample (data: ALLEGRO_SAMPLEptr; gain, pan, speed: SINGLE; loop: ALLEGRO_PLAYMODE; ret_id: ALLEGRO_SAMPLE_IDptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_stop_sample (spl_id: ALLEGRO_SAMPLE_IDptr); CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_stop_samples; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* File type handlers *)
  FUNCTION al_register_sample_loader (CONST ext: STRING; loader: ALLEGRO_SAMPLE_LOADER): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_register_sample_saver (CONST ext: STRING; saver: ALLEGRO_SAMPLE_SAVER): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_register_audio_stream_loader (CONST ext: STRING; stream_loader: ALLEGRO_AUDIO_STREAM_LOADER): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_load_sample (CONST filename: STRING): ALLEGRO_SAMPLEptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_save_sample (CONST filename: STRING; spl: ALLEGRO_SAMPLEptr): BOOLEAN; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_load_audio_stream (CONST filename: STRING; buffer_count, samples: LONGWORD): ALLEGRO_AUDIO_STREAMptr; CDECL;
  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

END.
