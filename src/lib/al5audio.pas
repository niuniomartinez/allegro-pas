UNIT al5audio;
(***<Audio addon.

     @include(../docs/al5audio.pds) *)
(* Copyright (c) 2012-2019 Guillermo Martínez J.

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
 *)

{$include allegro5.cfg}

INTERFACE

  USES
    Allegro5, al5base;

  CONST
  (*** @exclude
   * Internal, used to communicate with acodec.
   * Must be in 512 <= n < 1024 *)
    _KCM_STREAM_FEEDER_QUIT_EVENT_TYPE = 512;
    ALLEGRO_EVENT_AUDIO_STREAM_FRAGMENT = 513;
  (*** Sent when a stream is finished. *)
    ALLEGRO_EVENT_AUDIO_STREAM_FINISHED = 514;
    {
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_KCM_AUDIO_SRC)
    }
    ALLEGRO_EVENT_AUDIO_RECORDER_FRAGMENT = 515;

  TYPE
    ALLEGRO_AUDIO_RECORDERptr = AL_POINTER;
    {
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_KCM_AUDIO_SRC)
    }
    ALLEGRO_AUDIO_RECORDER_EVENT = RECORD
    (*** Indicates the type of event. *)
      ftype: ALLEGRO_EVENT_TYPE;
    (*** Pointer to the audio recorder. *)
      source: ALLEGRO_AUDIO_RECORDERptr;
    (*** When the event was generated. *)
      timestamp: AL_DOUBLE;
    (*** @exclude *)
      __internal__descr: ALLEGRO_USER_EVENT_DESCRIPTORptr;
    (*** Pointer to buffer containing the audio samples. *)
      buffer: AL_POINTER;
    (*** Number of samples (not bytes) that are available. *)
      samples: AL_UINT;
    END;




    ALLEGRO_AUDIO_DEPTH = (
    (* Sample depth and type, and signedness. Mixers only use 32-bit signed
     * float (-1..+1). The unsigned value is a bit-flag applied to the depth
     * value. *)
      ALLEGRO_AUDIO_DEPTH_INT8     = $00,
      ALLEGRO_AUDIO_DEPTH_INT16    = $01,
      ALLEGRO_AUDIO_DEPTH_INT24    = $02,
      ALLEGRO_AUDIO_DEPTH_FLOAT32  = $03,

      ALLEGRO_AUDIO_DEPTH_UNSIGNED = $08,

    { For convenience. }
    {  ALLEGRO_AUDIO_DEPTH_UINT8  = $08, // INT8  + UNSIGNED }
      ALLEGRO_AUDIO_DEPTH_UINT16 = $09, {**<@code(INT16 + UNSIGNED) }
      ALLEGRO_AUDIO_DEPTH_UINT24 = $0A  {**<@code(INT24 + UNSIGNED) }
    );

  CONST
    ALLEGRO_AUDIO_DEPTH_UINT8  = ALLEGRO_AUDIO_DEPTH_UNSIGNED; {**<@exclude }



  TYPE
  (*** Speaker configuration (mono, stereo, 2.1, etc). *)
    ALLEGRO_CHANNEL_CONF = (
    (* Speaker configuration (mono, stereo, 2.1, 3, etc). With regards to
     * behavior, most of this code makes no distinction between, say, 4.1 and
     * 5 speaker setups.. they both have 5 "channels". However, users would
     * like the distinction, and later when the higher-level stuff is added,
     * the differences will become more important. (v SHR 4) + (v AND $F) should
     * yield the total channel count.
     *)
      ALLEGRO_CHANNEL_CONF_1   = $10,
      ALLEGRO_CHANNEL_CONF_2   = $20,
      ALLEGRO_CHANNEL_CONF_3   = $30,
      ALLEGRO_CHANNEL_CONF_4   = $40,
      ALLEGRO_CHANNEL_CONF_5_1 = $51,
      ALLEGRO_CHANNEL_CONF_6_1 = $61,
      ALLEGRO_CHANNEL_CONF_7_1 = $71
    );

  CONST
    ALLEGRO_MAX_CHANNELS = 8; {**<@exclude Undocumented }

  TYPE
  (*** Sample and stream playback mode. *)
    ALLEGRO_PLAYMODE = (
      ALLEGRO_PLAYMODE_ONCE   = $100,
      ALLEGRO_PLAYMODE_LOOP   = $101,
      ALLEGRO_PLAYMODE_BIDIR  = $102
    { @exclude
      _ALLEGRO_PLAYMODE_STREAM_ONCE   = 0x103,   /* internal */
      _ALLEGRO_PLAYMODE_STREAM_ONEDIR = 0x104    /* internal */
    }
    );



    ALLEGRO_MIXER_QUALITY = (
    (*** Point sampling. *)
      ALLEGRO_MIXER_QUALITY_POINT   = $110,
    (*** Linear interpolation. *)
      ALLEGRO_MIXER_QUALITY_LINEAR  = $111,
    (*** Cubic interpolation. *)
      ALLEGRO_MIXER_QUALITY_CUBIC   = $112
    );

  CONST
    ALLEGRO_AUDIO_PAN_NONE: AL_FLOAT = -1000.0;

  TYPE
    ALLEGRO_SAMPLEptr = AL_POINTER;
  (*** Pointer to @link(ALLEGRO_SAMPLE_ID). *)
    ALLEGRO_SAMPLE_IDptr = ^ALLEGRO_SAMPLE_ID;
    ALLEGRO_SAMPLE_ID = RECORD
      _index: AL_INT; (***<@exclude *)
      _id: AL_INT; (***<@exclude *)
    END;
    ALLEGRO_SAMPLE_INSTANCEptr = AL_POINTER;
    ALLEGRO_AUDIO_STREAMptr = AL_POINTER;
    ALLEGRO_MIXERptr = AL_POINTER;
    ALLEGRO_VOICEptr = AL_POINTER;

{
#ifndef __cplusplus
typedef enum ALLEGRO_AUDIO_DEPTH ALLEGRO_AUDIO_DEPTH;
typedef enum ALLEGRO_CHANNEL_CONF ALLEGRO_CHANNEL_CONF;
typedef enum ALLEGRO_PLAYMODE ALLEGRO_PLAYMODE;
typedef enum ALLEGRO_MIXER_QUALITY ALLEGRO_MIXER_QUALITY;
#endif
}

  (* Callback declarations. *)
    ALLEGRO_MIXER_CALLBACK = PROCEDURE (buf: AL_VOIDptr; samples: AL_UINT; data: AL_VOIDptr); CDECL;
    ALLEGRO_SAMPLE_LOADER = FUNCTION (CONST filename: AL_STRptr): ALLEGRO_SAMPLEptr; CDECL;
    ALLEGRO_SAMPLE_SAVER = FUNCTION (CONST filename: AL_STRptr; spl: ALLEGRO_SAMPLEptr): AL_BOOL; CDECL;
    ALLEGRO_AUDIO_STREAM_LOADER = FUNCTION (CONST filename: AL_STRptr; buffer_count: AL_SIZE_T; samples: AL_UINT): ALLEGRO_AUDIO_STREAMptr; CDECL;

    ALLEGRO_SAMPLE_LOADER_F = FUNCTION (fp: ALLEGRO_FILEptr): ALLEGRO_SAMPLEptr; CDECL;
    ALLEGRO_SAMPLE_SAVER_F = FUNCTION (fp: ALLEGRO_FILEptr; spl: ALLEGRO_SAMPLEptr): AL_BOOL; CDECL;
    ALLEGRO_AUDIO_STREAM_LOADER_F = FUNCTION (fp: ALLEGRO_FILEptr; buffer_count: AL_SIZE_T; samples: AL_UINT): ALLEGRO_AUDIO_STREAMptr; CDECL;

(* Initialization. *)
  FUNCTION al_install_audio: AL_BOOL; CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_uninstall_audio; CDECL;  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_is_audio_installed: AL_BOOL; CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_allegro_audio_version: AL_UINT32; CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Sample functions *)
  FUNCTION al_create_sample (buf: AL_VOIDptr; samples, freq: AL_UINT; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF; free_buf: AL_BOOL): ALLEGRO_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_destroy_sample (spl: ALLEGRO_SAMPLEptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Sample instance functions *)
  FUNCTION al_create_sample_instance (data: ALLEGRO_SAMPLEptr): ALLEGRO_SAMPLE_INSTANCEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_destroy_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_frequency (CONST spl: ALLEGRO_SAMPLEptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_length (CONST spl: ALLEGRO_SAMPLEptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_depth (CONST spl: ALLEGRO_SAMPLEptr): ALLEGRO_AUDIO_DEPTH;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_channels (CONST spl: ALLEGRO_SAMPLEptr): ALLEGRO_CHANNEL_CONF;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_data (CONST spl: ALLEGRO_SAMPLEptr): AL_VOIDptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_frequency (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_instance_length (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_instance_position (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_speed (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_instance_gain (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_instance_pan (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_instance_time (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_depth (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_AUDIO_DEPTH;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_instance_channels (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_CHANNEL_CONF;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_instance_playmode (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_PLAYMODE;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_sample_instance_playing (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample_instance_attached (CONST spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_position (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: AL_UINT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_sample_instance_length (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: AL_UINT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_speed (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_sample_instance_gain (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_sample_instance_pan (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_playmode (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: ALLEGRO_PLAYMODE): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample_instance_playing (spl: ALLEGRO_SAMPLE_INSTANCEptr; val: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_detach_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_sample (spl: ALLEGRO_SAMPLE_INSTANCEptr; data: ALLEGRO_SAMPLEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_sample (spl: ALLEGRO_SAMPLE_INSTANCEptr): ALLEGRO_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_play_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_stop_sample_instance (spl: ALLEGRO_SAMPLE_INSTANCEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
    {
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_KCM_AUDIO_SRC)
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_sample_instance_channel_matrix, (ALLEGRO_SAMPLE_INSTANCE *spl, const float *matrix));
#endif
    }


(* Stream functions *)
  FUNCTION al_create_audio_stream (buffer_count: AL_SIZE_T; samples, freq: AL_UINT; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_AUDIO_STREAMptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_destroy_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_drain_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_frequency (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_length (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_fragments (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_available_audio_stream_fragments (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_speed (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_gain (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_pan (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_channels (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_CHANNEL_CONF;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_depth (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_AUDIO_DEPTH;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_playmode (CONST stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_PLAYMODE;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_playing (CONST spl: ALLEGRO_AUDIO_STREAMptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_attached (CONST spl: ALLEGRO_AUDIO_STREAMptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_played_samples (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_UINT64;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_fragment (CONST stream: ALLEGRO_AUDIO_STREAMptr): AL_VOIDptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_speed (stream: ALLEGRO_AUDIO_STREAMptr; val: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_audio_stream_gain (stream: ALLEGRO_AUDIO_STREAMptr; val: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_audio_stream_pan (stream: ALLEGRO_AUDIO_STREAMptr; val: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_playmode (stream: ALLEGRO_AUDIO_STREAMptr; val: ALLEGRO_PLAYMODE): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_set_audio_stream_playing (stream: ALLEGRO_AUDIO_STREAMptr; val: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_detach_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_audio_stream_fragment (stream: ALLEGRO_AUDIO_STREAMptr; val: AL_VOIDptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_rewind_audio_stream (stream: ALLEGRO_AUDIO_STREAMptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_seek_audio_stream_secs (stream: ALLEGRO_AUDIO_STREAMptr; time: AL_DOUBLE): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_position_secs (stream: ALLEGRO_AUDIO_STREAMptr): AL_DOUBLE;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_stream_length_secs (stream: ALLEGRO_AUDIO_STREAMptr): AL_DOUBLE;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_audio_stream_loop_secs (stream: ALLEGRO_AUDIO_STREAMptr; start, finish: AL_DOUBLE): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_audio_stream_event_source (stream: ALLEGRO_AUDIO_STREAMptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

    {
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_KCM_AUDIO_SRC)
ALLEGRO_KCM_AUDIO_FUNC(bool, al_set_audio_stream_channel_matrix, (ALLEGRO_AUDIO_STREAM *stream, const float *matrix));
#endif
    }

(* Mixer functions *)
  FUNCTION al_create_mixer (freq: AL_UINT; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_MIXERptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_destroy_mixer (mixer: ALLEGRO_MIXERptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_attach_sample_instance_to_mixer (sample: ALLEGRO_SAMPLE_INSTANCEptr; mixer: ALLEGRO_MIXERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_attach_audio_stream_to_mixer (stream: ALLEGRO_AUDIO_STREAMptr; mixer: ALLEGRO_MIXERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_attach_mixer_to_mixer (mixerA, mixerB: ALLEGRO_MIXERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_mixer_postprocess_callback (mixer: ALLEGRO_MIXERptr; cb: ALLEGRO_MIXER_CALLBACK; data: AL_VOIDptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_mixer_frequency (CONST mixer: ALLEGRO_MIXERptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_mixer_channels (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_CHANNEL_CONF;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_mixer_depth (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_AUDIO_DEPTH;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_mixer_quality (CONST mixer: ALLEGRO_MIXERptr): ALLEGRO_MIXER_QUALITY;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_mixer_gain (CONST mixer: ALLEGRO_MIXERptr): AL_FLOAT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_mixer_playing (CONST mixer: ALLEGRO_MIXERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_mixer_attached (CONST mixer: ALLEGRO_MIXERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_mixer_frequency (mixer: ALLEGRO_MIXERptr; val: AL_UINT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_mixer_quality (mixer: ALLEGRO_MIXERptr; val: ALLEGRO_MIXER_QUALITY): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_mixer_gain (mixer: ALLEGRO_MIXERptr; gain: AL_FLOAT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_mixer_playing (mixer: ALLEGRO_MIXERptr; val: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_detach_mixer (mixer: ALLEGRO_MIXERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Voice functions. *)
  FUNCTION al_create_voice (freq: AL_UINT; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_VOICEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_destroy_voice (voice: ALLEGRO_VOICEptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_attach_sample_instance_to_voice (sample: ALLEGRO_SAMPLE_INSTANCEptr; voice: ALLEGRO_VOICEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_attach_audio_stream_to_voice (stream: ALLEGRO_AUDIO_STREAMptr; voice: ALLEGRO_VOICEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_attach_mixer_to_voice (mixer: ALLEGRO_MIXERptr; voice: ALLEGRO_VOICEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_detach_voice (voice: ALLEGRO_VOICEptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_get_voice_frequency (CONST voice: ALLEGRO_VOICEptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_voice_position (CONST voice: ALLEGRO_VOICEptr): AL_UINT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_voice_channels (CONST voice: ALLEGRO_VOICEptr): ALLEGRO_CHANNEL_CONF;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_voice_depth (CONST voice: ALLEGRO_VOICEptr): ALLEGRO_AUDIO_DEPTH;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_voice_playing (CONST voice: ALLEGRO_VOICEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_voice_position (voice: ALLEGRO_VOICEptr; val: AL_UINT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_voice_playing (voice: ALLEGRO_VOICEptr; val: AL_BOOL): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Misc. audio functions *)
{ Those where moved to the beginning of the file.
  FUNCTION al_install_audio: AL_BOOL; CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_uninstall_audio; CDECL;  EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_is_audio_installed: AL_BOOL; CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_allegro_audio_version: AL_UINT32; CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
}

  FUNCTION al_get_channel_count (conf: ALLEGRO_CHANNEL_CONF): AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_depth_size (conf: ALLEGRO_AUDIO_DEPTH): AL_SIZE_T;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  PROCEDURE al_fill_silence (bur: AL_VOIDptr; samples: AL_UINT; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* Simple audio layer. *)
  FUNCTION al_reserve_samples (reserve_samples: AL_INT): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_default_mixer: ALLEGRO_MIXERptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_set_default_mixer (mixer: ALLEGRO_MIXERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_restore_default_mixer: AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_play_sample (data: ALLEGRO_SAMPLEptr; gain, pan, speed: AL_FLOAT; loop: ALLEGRO_PLAYMODE; ret_id: ALLEGRO_SAMPLE_IDptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_stop_sample (VAR spl_id: ALLEGRO_SAMPLE_ID);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_stop_samples;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_default_voice: ALLEGRO_VOICEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_set_default_voice (voice: ALLEGRO_VOICEptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

    {
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_KCM_AUDIO_SRC)
    }
  FUNCTION al_lock_sample_id (VAR spl_id: ALLEGRO_SAMPLE_ID): ALLEGRO_SAMPLE_INSTANCEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_unlock_sample_id (VAR spl_id: ALLEGRO_SAMPLE_ID);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

(* File type handlers *)
  FUNCTION al_register_sample_loader (CONST ext: AL_STR; loader: ALLEGRO_SAMPLE_LOADER): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_register_sample_saver (CONST ext: AL_STR; saver: ALLEGRO_SAMPLE_SAVER): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_register_audio_stream_loader (CONST ext: AL_STR; stream_loader: ALLEGRO_AUDIO_STREAM_LOADER): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_register_sample_loader_f (CONST ext: AL_STR; loader: ALLEGRO_SAMPLE_LOADER_F): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_register_sample_saver_f (CONST ext: AL_STR; saver: ALLEGRO_SAMPLE_SAVER_F): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_register_audio_stream_loader_f (CONST ext: AL_STR; stream_loader: ALLEGRO_AUDIO_STREAM_LOADER_F): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_load_sample (CONST filename: AL_STR): ALLEGRO_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_save_sample (CONST filename: AL_STR; spl: ALLEGRO_SAMPLEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_load_audio_stream (CONST filename: AL_STR; buffer_count: AL_SIZE_T; samples: AL_UINT): ALLEGRO_AUDIO_STREAMptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  FUNCTION al_load_sample_f (fp: ALLEGRO_FILEptr; CONST ident: AL_STR): ALLEGRO_SAMPLEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_save_sample_f (fp: ALLEGRO_FILEptr; CONST ident: AL_STR; spl: ALLEGRO_FILEptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_load_audio_stream_f (fp: ALLEGRO_FILEptr; CONST ident: AL_STR; buffer_count: AL_STR; samples: AL_UINT): ALLEGRO_AUDIO_STREAMptr
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

  {
#if defined(ALLEGRO_UNSTABLE) || defined(ALLEGRO_INTERNAL_UNSTABLE) || defined(ALLEGRO_KCM_AUDIO_SRC)
  }

(* Recording functions *)
  FUNCTION al_create_audio_recorder (fragment_count: AL_SIZE_T; samples, freq: AL_UINT; depth: ALLEGRO_AUDIO_DEPTH; chan_conf: ALLEGRO_CHANNEL_CONF): ALLEGRO_AUDIO_RECORDERptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_start_audio_recorder (r: ALLEGRO_AUDIO_RECORDERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_stop_audio_recorder (r: ALLEGRO_AUDIO_RECORDERptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_is_audio_recorder_recording (r: ALLEGRO_AUDIO_RECORDERptr): AL_BOOL;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_recorder_event_source (r: ALLEGRO_AUDIO_RECORDERptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  FUNCTION al_get_audio_recorder_event (event: ALLEGRO_EVENT): ALLEGRO_AUDIO_RECORDER_EVENT;
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;
  PROCEDURE al_destroy_audio_recorder (r: ALLEGRO_AUDIO_RECORDERptr);
    CDECL; EXTERNAL ALLEGRO_AUDIO_LIB_NAME;

IMPLEMENTATION

END.
