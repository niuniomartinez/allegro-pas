unit al5video;
(***<Video playback.

  Currently we have an Ogg backend (Theora + Vorbis). See http://xiph.org/
  for installation instructions, licensing information and supported video
  formats..
 *)
(* Copyright (c) 2012-2024 Guillermo Martínez J.

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

{$INCLUDE allegro5.cfg}

interface

  uses
    allegro5, al5base, al5audio;

  const
    ALLEGRO_EVENT_VIDEO_FRAME_SHOW = 550;
    ALLEGRO_EVENT_VIDEO_FINISHED   = 551;
  { _ALLEGRO_EVENT_VIDEO_SEEK      = 552; } { internal }

  type
    ALLEGRO_VIDEO_POSITION_TYPE = (
      ALLEGRO_VIDEO_POSITION_ACTUAL        = 0,
      ALLEGRO_VIDEO_POSITION_VIDEO_DECODE  = 1,
      ALLEGRO_VIDEO_POSITION_AUDIO_DECODE  = 2
    );

  (*** Pointer to the video description. *)
    ALLEGRO_VIDEOptr = type AL_POINTER;

{ Declaration order is different than in the original header to build the
  documentation in correct order. }
  function al_init_video_addon: AL_BOOL;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  procedure al_shutdown_video_addon;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_get_allegro_video_version: AL_UINT32;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;

  function al_open_video (filename: AL_STR): ALLEGRO_VIDEOptr;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  procedure al_close_video (video: ALLEGRO_VIDEOptr);
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  procedure al_start_video (video: ALLEGRO_VIDEOptr; mixer: ALLEGRO_MIXERptr);
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  procedure al_start_video_with_voice (video: ALLEGRO_VIDEOptr; voice: ALLEGRO_VOICEptr);
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_get_video_event_source (video: ALLEGRO_VIDEOptr): ALLEGRO_EVENT_SOURCEptr;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  procedure al_set_video_playing (video: ALLEGRO_VIDEOptr; playing: AL_BOOL);
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_is_video_playing (video: ALLEGRO_VIDEOptr): AL_BOOL;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_get_video_audio_rate (video: ALLEGRO_VIDEOptr): AL_DOUBLE;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_get_video_fps (video: ALLEGRO_VIDEOptr): AL_DOUBLE;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_get_video_scaled_width (video: ALLEGRO_VIDEOptr): AL_FLOAT;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_get_video_scaled_height (video: ALLEGRO_VIDEOptr): AL_FLOAT;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_get_video_frame (video: ALLEGRO_VIDEOptr): ALLEGRO_BITMAPptr;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_get_video_position (video: ALLEGRO_VIDEOptr; which: ALLEGRO_VIDEO_POSITION_TYPE): AL_DOUBLE;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;
  function al_seek_video (video: ALLEGRO_VIDEOptr;  pos_in_seconds: AL_DOUBLE): AL_BOOL;
    CDECL; external ALLEGRO_VIDEO_LIB_NAME;

implementation

end.
