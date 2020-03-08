program FuriousPaladin;
(* A simple demo game for Allegro.pas 5 by Handoko.
   See README for more information. *)
(*
  Copyright (c) 2018 Handoko
            (c) 2019-2020 Guillermo Martínez.

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

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
  {$MODE DELPHI}
{$ENDIF}

uses
  Actor, Configuration, Data,
  Classes, SysUtils, al5Base, al5image, al5font, al5ttf,
  al5primitives, al5audio, al5acodec, Allegro5, al5nativedlg,
  al5strings;

type
  TAppState = (Intro1, Intro2, Playing, Paused, Lose, Win);

{$IFDEF DCC}
{ Delphi doesn't have TFPList, so define it here.

  We still use TFPList because his performance is higher than TList in
  Free Pascal. }
  TFPList = CLASS (TList)
  END;
{$ENDIF}

const
  MaxPlayerHealth = 500;
  PlayerHealthInc = 0.08;
  EnemyStrength   = 140;

var
  // Game data
  AppState:          TAppState;
  GameIsRunning:     Boolean;
  Player:            TActor;
  PlayerHealth:      Real;
  EnemyKilled:       Integer;
  Enemies:           TFPList;
  EnemySpawned:      Integer;
  SpawnMinDelay:     Integer;
  SpawnDelay:        Integer;
  // Allegro event
  EventQueue:        ALLEGRO_EVENTptr;
  Timer:             ALLEGRO_TIMERptr;
  // Display
  Display:           ALLEGRO_DISPLAYptr;
  // Background and info images
  ImgIntroPage1:     ALLEGRO_BITMAPptr;
  ImgIntroPage2:     ALLEGRO_BITMAPptr;
  ImgBackground1:    ALLEGRO_BITMAPptr;
  ImgBackground2:    ALLEGRO_BITMAPptr;
  ImgPaused:         ALLEGRO_BITMAPptr;
  ImgWin:            ALLEGRO_BITMAPptr;
  ImgFailed:         ALLEGRO_BITMAPptr;
  // Animation data
  ImgPlayerWalkL:    TAnimation;
  ImgPlayerWalkR:    TAnimation;
  ImgPlayerAttackL:  TAnimation;
  ImgPlayerAttackR:  TAnimation;
  ImgPlayerIdleL:    TAnimation;
  ImgPlayerIdleR:    TAnimation;
  ImgPlayerBeenHitL: TAnimation;
  ImgPlayerBeenHitR: TAnimation;
  ImgZombieWalkL:    TAnimation;
  ImgZombieWalkR:    TAnimation;
  ImgZombieAttackL:  TAnimation;
  ImgZombieAttackR:  TAnimation;
  ImgZombieKilledL:  TAnimation;
  ImgZombieKilledR:  TAnimation;
  // Audio data
  AudioInstance:     ALLEGRO_SAMPLE_INSTANCEptr;
  AudioBackground:   ALLEGRO_SAMPLEptr;
  AudioSword:        ALLEGRO_SAMPLEptr;
  AudioHurt:         ALLEGRO_SAMPLEptr;
  AudioFailed:       ALLEGRO_SAMPLEptr;
  // Font data
  SystemFont:        ALLEGRO_FONTptr;



  PROCEDURE ProcessShutdown; FORWARD;



(* Shows an error message and ends. *)
  PROCEDURE AbortProgram (CONST Message: STRING);
  VAR
    Display: ALLEGRO_DISPLAYptr;
  BEGIN
    IF al_init_native_dialog_addon THEN
    BEGIN
      IF al_is_system_installed THEN
        Display := al_get_current_display
      ELSE
        Display := NIL;
      al_show_native_message_box (
        Display,
        'Error', 'Cannot run Furious Paladin', al_string_to_str (Message),
        '', 0
      )
    END
    ELSE
      WriteLn (ErrOutput, Message);
    ProcessShutDown;
    HALT (1)
  END;



procedure ProcessInit;
var
  MonitorInfo: ALLEGRO_MONITOR_INFO;
  Transform: ALLEGRO_TRANSFORM;
begin
{ Loads configuration values. }
  IF NOT Configuration.Load THEN Halt;
{ Quit if Allegro fails to start. }
  IF NOT al_init THEN AbortProgram ('Cannot init Allegro!');

  // Prepare display
  al_get_monitor_info(0, MonitorInfo);
  if (MonitorInfo.x2 > 1060) and (MonitorInfo.y2 > 610) then begin
    Display := al_create_display(ScreenWidth*2, ScreenHeight*2);
    al_identity_transform(Transform);
    al_rotate_transform(Transform, 0);
    al_scale_transform(Transform, 2, 2);
    al_use_transform(Transform);
  end
  else
    al_create_display(ScreenWidth, ScreenHeight);
  al_set_window_title(Display, 'Furious Paladin');
  // Enable image loading
  al_init_image_addon;
  // Enable keyboard
  al_install_keyboard;
  // Enable font;
  al_init_font_addon;
  al_init_ttf_addon;
  SystemFont := LoadFont ('tuffy_bold.ttf', 12);
  // Enable drawing basic shapes
  al_init_primitives_addon;
  // Enable audio
  al_install_audio;
  al_init_acodec_addon;
  al_reserve_samples(8);

  // Enable timer for fixed fps
  Timer := al_create_timer(1 / 60); // we use 60 fps
  EventQueue := al_create_event_queue;
  al_register_event_source(EventQueue, al_get_timer_event_source(timer));
  al_start_timer(Timer);

  // Load background image
  ImgIntroPage1  := LoadBitmap ('imgintro1.jpg');
  ImgIntroPage2  := LoadBitmap ('imgintro2.jpg');
  ImgBackground1 := LoadBitmap ('imgdusk.jpg');
  ImgBackground2 := LoadBitmap ('imgdawn.jpg');
  ImgPaused      := LoadBitmap ('imgpaused.png');
  ImgWin         := LoadBitmap ('imgwin.png');
  ImgFailed      := LoadBitmap ('imgfailed.png');

  // Load animation data: Player Walk Left
  IF NOT LoadAnimation ('hero-walk-w', 4, imgPlayerWalkL) THEN
    AbortProgram ('Can''t load hero animation');
  // Load animation data: Player Walk Right
  IF NOT LoadAnimation ('hero-walk-e', 4, imgPlayerWalkR) THEN
    AbortProgram ('Can''t load hero animation');
  // Load animation data: Player Attack Left
  IF NOT LoadAnimation ('hero-attack-w', 6, ImgPlayerAttackL) THEN
    AbortProgram ('Can''t load hero animation');
  // Load animation data: Player Attack Right
  IF NOT LoadAnimation ('hero-attack-e', 6, ImgPlayerAttackR) THEN
    AbortProgram ('Can''t load hero animation');
  // Load animation data: Player Idle Left
  IF NOT LoadAnimation ('hero-idle-w', 12, imgPlayerIdleL) THEN
    AbortProgram ('Can''t load hero animation');
  // Load animation data: Player Idle Right
  IF NOT LoadAnimation ('hero-idle-e', 12, imgPlayerIdleR) THEN
    AbortProgram ('Can''t load hero animation');
  // Load animation data: Player Been Hit Left
  IF NOT LoadAnimation ('hero-been-hit-w', 6, ImgPlayerBeenHitL) THEN
    AbortProgram ('Can''t load hero animation');
  // Load animation data: Player Been Hit Right
  IF NOT LoadAnimation ('hero-been-hit-e', 6, ImgPlayerBeenHitR) THEN
    AbortProgram ('Can''t load hero animation');
  // Load animation data: Zombie Walk Left
  IF NOT LoadAnimation ('zombie-walk-w', 9, ImgZombieWalkL) THEN
    AbortProgram ('Can''t load zombie animation');
  // Load animation data: Zombie Walk Right
  IF NOT LoadAnimation ('zombie-walk-e', 9, ImgZombieWalkR) THEN
    AbortProgram ('Can''t load zombie animation');
  // Load animation data: Zombie Attack Left
  IF NOT LoadAnimation ('zombie-attack-w', 15, ImgZombieAttackL) THEN
    AbortProgram ('Can''t load zombie animation');
  // Load animation data: Zombie Attack Right
  IF NOT LoadAnimation ('zombie-attack-e', 15, ImgZombieAttackR) THEN
    AbortProgram ('Can''t load zombie animation');
  // Load animation data: Zombie Killed Left
  IF NOT LoadAnimation ('zombie-killed-w', 10, ImgZombieKilledL) THEN
    AbortProgram ('Can''t load zombie animation');
  // Load animation data: Zombie Killed Right
  IF NOT LoadAnimation ('zombie-killed-e', 10, ImgZombieKilledR) THEN
    AbortProgram ('Can''t load zombie animation');

  // Load audio data
  AudioBackground := LoadSample ('auduntitledremix.ogg');
  AudioSword      := LoadSample ('audsword.wav');
  AudioHurt       := LoadSample ('audhit1.ogg');
  AudioFailed     := LoadSample ('auddie1.ogg');
  AudioInstance   := al_create_sample_instance(AudioBackground);


  // Create list for enemies
  Enemies := TFPList.Create;

  // Start the game
  GameIsRunning := True;

end;



var
  SavedAudioPosition: AL_UINT = 0; // Used for pausing and resumming the game music

procedure Start(State: TAppState);
begin
  case State of

    Intro1: begin
      with Player do begin
        PosX            := 350;
        PosY            := 65;
        MinX            := 220;
        MaxX            := 400;
        Speed           := 2;
        CurrentActivity := IdleL;
        LastActivity    := IdleL;
        AnimImage       := nil;
        AnimProgress    := 0;
        AnimDelay       := 0;
      end;
      al_stop_samples;
      al_set_sample_instance_playing(AudioInstance, False);
      al_play_sample(AudioBackground, 1.5, 0, 1, ALLEGRO_PLAYMODE_LOOP, nil);
    end;

    Intro2: begin
      with Player do begin
        PosX            := 350;
        PosY            := 130;
        MinX            := 220;
        MaxX            := 400;
        Speed           := 2;
        CurrentActivity := IdleL;
        LastActivity    := IdleL;
        AnimImage       := nil;
        AnimProgress    := 0;
        AnimDelay       := 0;
      end;
    end;

    Playing: begin
      if (AppState = Intro2) then begin
        with Player do begin
          PosX            := 200;
          PosY            := 210;
          MinX            := 20;
          MaxX            := -20+ScreenWidth-96;
          CurrentActivity := IdleR;
          LastActivity    := IdleR;
          AnimImage       := nil;
          AnimProgress    := 0;
          AnimDelay       := 0;
        end;
        PlayerHealth      := MaxPlayerHealth;
        Enemies.Clear;
        EnemyKilled       := 0;
        EnemySpawned      := 0;
        SpawnMinDelay     := 60;
        SpawnDelay        := 0;
        al_stop_samples;
        al_attach_sample_instance_to_mixer(AudioInstance, al_get_default_mixer);
        al_set_sample_instance_playing(AudioInstance, True);
        al_set_sample_instance_playmode(AudioInstance, ALLEGRO_PLAYMODE_ONCE);
        al_set_sample_instance_gain(AudioInstance, 1.5)
      end;
      if (AppState = Paused) then begin
        al_set_sample_instance_position(AudioInstance, SavedAudioPosition);
        al_set_sample_instance_playing(AudioInstance, True);
        end;
    end;

    Paused: begin
      SavedAudioPosition := al_get_sample_instance_position(AudioInstance);
      al_set_sample_instance_playing(AudioInstance, False);
    end;

    Lose: begin
      al_play_sample(AudioFailed, 1, 0, 1, ALLEGRO_PLAYMODE_ONCE, nil);
    end;

  end;
  AppState := State;
end;



var
  EscJustPressed:   Boolean = False;
  SpaceJustPressed: Boolean = False;
procedure ProcessUserInput;
var
  KeyState:    ALLEGRO_KEYBOARD_STATE;
  isFaceRight: Boolean;
  isMoveLeft:  Boolean;
  isMoveRight: Boolean;
  isAttack:    Boolean;
  isBusy:      Boolean;
begin

  isBusy      := False;
  isFaceRight := (Player.CurrentActivity = WalkR) or
                 (Player.CurrentActivity = AttackR) or
                 (Player.CurrentActivity = IdleR) or
                 (Player.CurrentActivity = HitR);

  // Check keyboard input
  al_get_keyboard_state(KeyState);

  // Esc
  if al_key_down(KeyState, ALLEGRO_KEY_ESCAPE) then begin
    if not(EscJustPressed) then
      case AppState of
        Playing: Start(Paused);
        Lose, Win: Start(Intro1);
        Intro1, Intro2, Paused: GameIsRunning := False;
      end;
    EscJustPressed := True;
  end
  else
    EscJustPressed := False;

  // Space
  if al_key_down(KeyState, ALLEGRO_KEY_SPACE) then begin
    if not(SpaceJustPressed) then
      case AppState of
        Intro1: Start(Intro2);
        Intro2, Paused: Start(Playing);
        Lose, Win: Start(Intro1);
      end;
    SpaceJustPressed := True;
  end
  else
    SpaceJustPressed := False;

  // Cannot do anything is being hit
  if (Player.CurrentActivity = HitL) or (Player.CurrentActivity = HitR) then
    Exit;

  // Moving or attacking
  isMoveLeft  := al_key_down(KeyState, ALLEGRO_KEY_LCTRL);
  isMoveRight := al_key_down(KeyState, ALLEGRO_KEY_RCTRL);
  if (isMoveLeft and isMoveRight) then begin   // Do attack and disable moving if left & right shift pressed
    isAttack    := True;
    isMoveLeft  := False;
    isMoveRight := False;
  end
  else
    isAttack := False;

  // Move left
  if isMoveLeft then begin
    Player.CurrentActivity := WalkL;
    isBusy := True;
  end;
  // Move right
  if isMoveRight then begin
    Player.CurrentActivity := WalkR;
    isBusy := True;
  end;
  // Attack
  if isAttack then begin
    if isFaceRight then
      Player.CurrentActivity := AttackR
    else
      Player.CurrentActivity := AttackL;
    isBusy := True;
  end;
  // Idle
  if not(isBusy) then begin
    if isFaceRight then
      Player.CurrentActivity := IdleR
    else
      Player.CurrentActivity := IdleL;
  end;

end;



function BothIntersect(Xa1, Xa2, Xb1, Xb2: Real): Boolean;
begin
  Result := False;
  if (Xa2 < Xb1) or (Xb2 < Xa1) or (Xa1 > Xb2) or (Xb1 > Xa2) then Exit;
  Result := True;
end;



function iif(Condition: Boolean; ValTrue, ValFalse: Real): Real;
begin
  if Condition then
    Result := ValTrue
  else
    Result := ValFalse;
end;



procedure ProcessUpdate;
var
  Enemy: PActor;
  EnemiesExpired: array of Pointer;
  EnemiesExpiredCount: Integer;
  i: Integer;
begin

  if (AppState = Paused) or (AppState = Lose) or (AppState = Win) then Exit; // Do nothing

  { === Player === }

  // Paladin regains his health automatically
  if (PlayerHealth < MaxPlayerHealth) then
    PlayerHealth := PlayerHealth + iif(CheatFastRecover, 1, PlayerHealthInc);

  // Calculate player's animation data and position
  case Player.CurrentActivity of
    WalkL  : begin
             Player.AnimImage := @ImgPlayerWalkL;
             Player.PosX := Player.PosX - Player.Speed;
             end;
    WalkR  : begin
             Player.AnimImage := @ImgPlayerWalkR;
             Player.PosX := Player.PosX + Player.Speed;
             end;
    AttackL: Player.AnimImage := @ImgPlayerAttackL;
    AttackR: Player.AnimImage := @ImgPlayerAttackR;
    IdleL  : Player.AnimImage := @ImgPlayerIdleL;
    IdleR  : Player.AnimImage := @ImgPlayerIdleR;
    HitL:    Player.AnimImage := @ImgPlayerBeenHitL;
    HitR:    Player.AnimImage := @ImgPlayerBeenHitR;
  end;
  // Reset player's animation progress if activity changed
  if (Player.LastActivity <> Player.CurrentActivity) then Player.AnimProgress := 0;
  Player.LastActivity := Player.CurrentActivity;
  // Animate player
  Inc(Player.AnimDelay);
  if (Player.AnimDelay > Player.AnimImage^.Delay) then begin
    Player.AnimDelay := 0;
    Inc(Player.AnimProgress);
    if (Player.AnimProgress >= Player.AnimImage^.Count) then begin
      if (Player.CurrentActivity = HitL) then Player.CurrentActivity := IdleL;
      if (Player.CurrentActivity = HitR) then Player.CurrentActivity := IdleR;
      Player.AnimProgress := 0;
    end;
  end;
  // Limit player inside screen
  if (Player.PosX < Player.MinX) then Player.PosX := Player.MinX;
  if (Player.PosX > Player.MaxX) then Player.PosX := Player.MaxX;
  // Check player attacks
  if ((Player.CurrentActivity = AttackL) or (Player.CurrentActivity = AttackR)) and
    (Player.AnimProgress = 4) and (Player.AnimDelay = 1) then begin
      al_play_sample(AudioSword, 1, 0, 1, ALLEGRO_PLAYMODE_ONCE, nil);
      with Player do
        for i := 0 to (Enemies.Count-1) do begin
          Enemy := Enemies[i];
          if (Player.CurrentActivity = AttackL) and
            BothIntersect(PosX+15, PosX+40, Enemy^.PosX+30, Enemy^.PosX+65) then begin
              if (Enemy^.CurrentActivity <> KilledR) then Inc(EnemyKilled);
              Enemy^.CurrentActivity := KilledR;
          end;
          if (Player.CurrentActivity = AttackR) and
            BothIntersect(PosX+55, PosX+80, Enemy^.PosX+33, Enemy^.PosX+68) then begin
              if (Enemy^.CurrentActivity <> KilledL) then Inc(EnemyKilled);
              Enemy^.CurrentActivity := KilledL;
          end;
        end;
    end;

  { === Enemies === }

  if (AppState <> Playing) then Exit;

  // Already killed 100
  if (EnemyKilled >= 100) and ((Player.CurrentActivity = IdleL) or (Player.CurrentActivity = IdleR)) then begin
    al_rest(1);
    Start(Win);
  end;


  // Spawn new enemy
  if (SpawnDelay>SpawnMinDelay) and (Random(EnemyKilled+1000)>980) and (EnemySpawned<100) then begin
    New(Enemy);
    Enemies.Add(Enemy);
    with Enemy^ do begin
      if (Random(2) > 0) then begin
        PosX            := -96; // 96 is the width of the image
        CurrentActivity := WalkR;
        LastActivity    := WalkR;
      end
      else begin
        PosX            := ScreenWidth;
        CurrentActivity := WalkL;
        LastActivity    := WalkL;
      end;
      PosY              := 213;
      MinX              := -96;
      MaxX              := ScreenWidth;
      Speed             := 0.6;
      AnimImage         := nil; // AnimImage will be updated later
      AnimProgress      := 0;
      AnimDelay         := 0;
    end;
    Inc(EnemySpawned);
    SpawnDelay := 0;
  end;
  Inc(SpawnDelay);

  EnemiesExpiredCount := 0; SetLength (EnemiesExpired, 0);
  for i := 0 to (Enemies.Count-1) do
    with TActor(Enemies[i]^) do begin
      // Calculate animation data and position
      case CurrentActivity of
        WalkL:   begin
                 AnimImage := @ImgZombieWalkL;
                 PosX := PosX - Speed;
                 end;
        WalkR:   begin
                 AnimImage := @ImgZombieWalkR;
                 PosX := PosX + Speed;
                 end;
        AttackL: AnimImage := @ImgZombieAttackL;
        AttackR: AnimImage := @ImgZombieAttackR;
        KilledL: AnimImage := @ImgZombieKilledL;
        KilledR: AnimImage := @ImgZombieKilledR;
      end;
      // Reset animation progress if activity changed
      if (LastActivity <> CurrentActivity) then AnimProgress := 0;
      LastActivity := CurrentActivity;
      // Animate it
      Inc(AnimDelay);
      if (AnimDelay > AnimImage^.Delay) then begin
        AnimDelay := 0;
        Inc(AnimProgress);
        if (AnimProgress >= AnimImage^.Count) then
          AnimProgress := 0;
      end;
      // Set it expired if it is not inside the screen
      if ((PosX > MaxX) and (CurrentActivity = WalkR)) or
         ((PosX < MinX) and (CurrentActivity = WalkL)) then begin
           SetLength(EnemiesExpired, EnemiesExpiredCount+1);
           EnemiesExpired[EnemiesExpiredCount] := Enemies[i];
           Inc(EnemiesExpiredCount);
         end;
      // Start attack if close to player
      if (CurrentActivity = WalkL) and (PosX - Player.PosX < 35) then
        CurrentActivity := AttackL;
      if (CurrentActivity = WalkR) and (Player.PosX - PosX < 38) then
        CurrentActivity := AttackR;
      // If the attack hits player
      if (CurrentActivity = AttackL) and (AnimProgress = 4) and
        BothIntersect(PosX+22, PosX+47, Player.PosX+35, Player.PosX+60) then begin
          if (Player.CurrentActivity <> HitR) then begin
            al_play_sample(AudioHurt, 1, 0, 1, ALLEGRO_PLAYMODE_ONCE, nil);
            PlayerHealth := PlayerHealth - iif(CheatStoneSkin, 100, EnemyStrength);
            if (PlayerHealth < 0) then PlayerHealth := 0 ;
          end;
          Player.CurrentActivity := HitR;
        end;
      if (CurrentActivity = AttackR) and (AnimProgress = 4) and
        BothIntersect(PosX+50, PosX+75, Player.PosX+35, Player.PosX+60) then begin
          if (Player.CurrentActivity <> HitL) then begin
            al_play_sample(AudioHurt, 1, 0, 1, ALLEGRO_PLAYMODE_ONCE, nil);
            PlayerHealth := PlayerHealth - iif(CheatStoneSkin, 100, EnemyStrength);
            if (PlayerHealth < 0) then PlayerHealth := 0 ;
          end;
          Player.CurrentActivity := HitL;
        end;
      if (PlayerHealth <= 0) then Start(Lose);
      // Killed enemy become expired
      if (CurrentActivity = KilledL) and (AnimProgress >= 4) and
        (AnimDelay >= AnimImage^.Delay) then begin
          SetLength(EnemiesExpired, EnemiesExpiredCount+1);
          EnemiesExpired[EnemiesExpiredCount] := Enemies[i];
          Inc(EnemiesExpiredCount);
        end;
      if (CurrentActivity = KilledR) and (AnimProgress >= 4) and
        (AnimDelay >= AnimImage^.Delay) then begin
          SetLength(EnemiesExpired, EnemiesExpiredCount+1);
          EnemiesExpired[EnemiesExpiredCount] := Enemies[i];
          Inc(EnemiesExpiredCount);
        end;
    end;

  // Remove expired enemies
  if (EnemiesExpiredCount >= 1) then begin
    for i := 0 to (EnemiesExpiredCount-1) do Enemies.Remove(EnemiesExpired[i]);
  end;

end;



procedure ProcessDrawing;
var
  C: Real;
  i: Integer;
begin

  case AppState of

    Intro1:
      begin
        al_draw_bitmap(ImgIntroPage1, 0, 0, 0);
        with Player do
          al_draw_bitmap(AnimImage^.Images[AnimProgress], PosX, PosY, 0);
      end;

    Intro2:
      begin
        al_draw_bitmap(ImgIntroPage2, 0, 0, 0);
        with Player do
          al_draw_bitmap(AnimImage^.Images[AnimProgress], PosX, PosY, 0);
      end;

    Playing, Paused, Lose, Win:
      begin
        al_clear_to_color(al_map_rgb(0, 0, 10));
        // Draw health bar
        al_draw_filled_rectangle(0, 0, PlayerHealth, 20, al_map_rgb(0, 200, 0));
        C := (MaxPlayerHealth-PlayerHealth)/MaxPlayerHealth;
        al_draw_filled_rectangle(0, 0, PlayerHealth, 20, al_map_rgba_f(C, 0, 0, C));
        // Show killed information
        al_draw_textf(SystemFont, al_map_rgb(255, 255, 255), 20, 4,ALLEGRO_ALIGN_LEFT,
          'Killed: %d', [EnemyKilled]);
        // Brighten the sky
        al_draw_bitmap(ImgBackground1, 0, 20, 0);
        C := EnemyKilled/100;
        al_draw_tinted_bitmap(ImgBackground2, al_map_rgba_f(C, C, C, C), 0, 20, 0);
        // Draw player and enemies
        with Player do begin
          al_draw_bitmap(AnimImage^.Images[AnimProgress], PosX, PosY, 0);
          if DebugMode then begin
            al_draw_rectangle(PosX+35, 230, PosX+60, 280, al_map_rgb(0, 255, 0), 1);
            if (CurrentActivity = AttackL) then
              al_draw_rectangle(PosX+15, 230, PosX+40, 280, al_map_rgb(255, 0, 0), 1);
            if (CurrentActivity = AttackR) then
              al_draw_rectangle(PosX+55, 230, PosX+80, 280, al_map_rgb(255, 0, 0), 1);
          end;
        end;
        for i := 0 to (Enemies.Count-1) do
          with TActor(Enemies[i]^) do begin
            al_draw_bitmap(AnimImage^.Images[AnimProgress], PosX, PosY, 0);
            if DebugMode then begin
              if (CurrentActivity = WalkL) or (CurrentActivity = AttackL) then
                al_draw_rectangle(PosX+30, 230, PosX+65, 280, al_map_rgb(0, 0, 255), 1);
              if (CurrentActivity = WalkR) or (CurrentActivity = AttackR) then
                al_draw_rectangle(PosX+33, 230, PosX+68, 280, al_map_rgb(0, 0, 255), 1);
              if (CurrentActivity = AttackL) then
                al_draw_rectangle(PosX+22, 230, PosX+47, 280, al_map_rgb(255, 0, 0), 1);
              if (CurrentActivity = AttackR)  then
                al_draw_rectangle(PosX+50, 230, PosX+75, 280, al_map_rgb(255, 0, 0), 1);
            end;
          end;
        // Show cheat info
        if CheatFastRecover then
          al_draw_text(SystemFont, al_map_rgb(255, 0, 0), 20, 100,ALLEGRO_ALIGN_LEFT,
            'FastRecover');
        if CheatStoneSkin then
          al_draw_text(SystemFont, al_map_rgb(255, 0, 0), 20, 120,ALLEGRO_ALIGN_LEFT,
            'StoneSkin');
        // AppState related
        case AppState of
          Paused:
            al_draw_bitmap(ImgPaused, 140, 75, 0);
          Lose:
            al_draw_bitmap(ImgFailed, 140, 75, 0);
          Win:
            al_draw_bitmap(ImgWin, 140, 75, 0);
        end;
      end;

  end;

  al_flip_display;

end;



procedure ProcessShutDown;
begin
  UnloadAnimation (ImgPlayerWalkL);
  UnloadAnimation (ImgPlayerWalkR);
  UnloadAnimation (ImgPlayerAttackL);
  UnloadAnimation (ImgPlayerAttackR);
  UnloadAnimation (ImgPlayerIdleL);
  UnloadAnimation (ImgPlayerIdleR);
  UnloadAnimation (ImgPlayerBeenHitL);
  UnloadAnimation (ImgPlayerBeenHitR);
  UnloadAnimation (ImgZombieWalkL);
  UnloadAnimation (ImgZombieWalkR);
  UnloadAnimation (ImgZombieAttackL);
  UnloadAnimation (ImgZombieAttackR);
  UnloadAnimation (ImgZombieKilledL);
  UnloadAnimation (ImgZombieKilledR);

  al_destroy_bitmap(ImgIntroPage1);
  al_destroy_bitmap(ImgIntroPage2);
  al_destroy_bitmap(ImgBackground1);
  al_destroy_bitmap(ImgBackground2);
  al_destroy_bitmap(ImgPaused);
  al_destroy_bitmap(ImgWin);
  al_destroy_bitmap(ImgFailed);
  al_destroy_event_queue(EventQueue);
  al_destroy_sample(AudioBackground);
  al_destroy_sample(AudioSword);
  al_destroy_sample(AudioHurt);
  al_destroy_sample(AudioFailed);
  al_destroy_sample_instance(AudioInstance);
  al_uninstall_audio;
  al_destroy_font(SystemFont);
end;



var
  Event: ALLEGRO_EVENT;
begin
  ProcessInit;
  Start(Intro1);
  while (GameIsRunning) do begin
    al_wait_for_event (EventQueue, @Event);
    ProcessUserInput;
    ProcessUpdate;
    ProcessDrawing;
  end;
  ProcessShutdown;
end.
