program FuriousPaladin;

{$mode objfpc}{$H+}


uses
  Classes, SysUtils, al5Base, al5image, al5font, al5ttf,
  al5primitives, al5audio, al5acodec, Allegro5;

type

  TAppState = (Intro1, Intro2, Playing, Paused, Lose, Win);

  TActivity = (WalkL, WalkR, AttackL, AttackR, IdleL, IdleR, HitL, HitR, KilledL, KilledR);

  TImageList = record
    Images: array of ALLEGRO_BITMAPptr;
    Count:  Byte;
    Delay:  Byte;
  end;

  TActor = record
    PosX, PosY:      Real;
    MinX, MaxX:      Real;
    Speed:           Real;
    CurrentActivity: TActivity;
    LastActivity:    TActivity;
    AnimImage:       ^TImageList;
    AnimProgress:    Byte;
    AnimDelay:       Byte;
  end;

  PActor = ^TActor;

const
  ScreenWidth     = 500;
  ScreenHeight    = 300;
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
  // Debug and Cheat
  DebugMode:         Boolean = False;
  CheatFastRecover:  Boolean = False;
  CheatStoneSkin:    Boolean = False;
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
  ImgPlayerWalkL:    TImageList;
  ImgPlayerWalkR:    TImageList;
  ImgPlayerAttackL:  TImageList;
  ImgPlayerAttackR:  TImageList;
  ImgPlayerIdleL:    TImageList;
  ImgPlayerIdleR:    TImageList;
  ImgPlayerBeenHitL: TImageList;
  ImgPlayerBeenHitR: TImageList;
  ImgZombieWalkL:    TImageList;
  ImgZombieWalkR:    TImageList;
  ImgZombieAttackL:  TImageList;
  ImgZombieAttackR:  TImageList;
  ImgZombieKilledL:  TImageList;
  ImgZombieKilledR:  TImageList;
  // Audio data
  AudioInstance:     ALLEGRO_SAMPLE_INSTANCEptr;
  AudioBackground:   ALLEGRO_SAMPLEptr;
  AudioSword:        ALLEGRO_SAMPLEptr;
  AudioHurt:         ALLEGRO_SAMPLEptr;
  AudioFailed:       ALLEGRO_SAMPLEptr;
  // Font data
  SystemFont:        ALLEGRO_FONTptr;

procedure ProcessInit;
var
  MonitorInfo: ALLEGRO_MONITOR_INFO;
  Transform: ALLEGRO_TRANSFORM;
  DataPath: string;
  i: Integer;
begin

  // Debug and cheat
  for i := 1 to ParamCount do begin
    if (LowerCase(ParamStr(i)) = '/debug') then       DebugMode := True;
    if (LowerCase(ParamStr(i)) = '/fastrecover') then CheatFastRecover := True;
    if (LowerCase(ParamStr(i)) = '/stoneskin') then   CheatStoneSkin := True;
  end;

  // Quit if Allegro fails to start
  if not(al_init) then Halt;

  // Get default data path used for loading font, images and sounds
  DataPath := ExtractFilePath(ParamStr(0))+'Data'+DirectorySeparator;

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
  SystemFont := al_load_ttf_font(DataPath+'Tuffy_Bold.ttf', 12, 0);
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
  ImgIntroPage1  := al_load_bitmap(DataPath+'imgIntro1.jpg');
  ImgIntroPage2  := al_load_bitmap(DataPath+'imgIntro2.jpg');
  ImgBackground1 := al_load_bitmap(DataPath+'imgDusk.jpg');
  ImgBackground2 := al_load_bitmap(DataPath+'imgDawn.jpg');
  ImgPaused      := al_load_bitmap(DataPath+'imgPaused.png');
  ImgWin         := al_load_bitmap(DataPath+'imgWin.png');
  ImgFailed      := al_load_bitmap(DataPath+'imgFailed.png');

  // Load animation data: Player Walk Left
  with imgPlayerWalkL do begin
    Delay := 4;
    Count := 8;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'hero-walk-w00.png');
    Images[1] := al_load_bitmap(DataPath+'hero-walk-w01.png');
    Images[2] := al_load_bitmap(DataPath+'hero-walk-w02.png');
    Images[3] := al_load_bitmap(DataPath+'hero-walk-w03.png');
    Images[4] := al_load_bitmap(DataPath+'hero-walk-w04.png');
    Images[5] := al_load_bitmap(DataPath+'hero-walk-w05.png');
    Images[6] := al_load_bitmap(DataPath+'hero-walk-w06.png');
    Images[7] := al_load_bitmap(DataPath+'hero-walk-w07.png');
  end;
  // Load animation data: Player Walk Right
  with ImgPlayerWalkR do begin
    Delay := 4;
    Count := 8;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'hero-walk-e00.png');
    Images[1] := al_load_bitmap(DataPath+'hero-walk-e01.png');
    Images[2] := al_load_bitmap(DataPath+'hero-walk-e02.png');
    Images[3] := al_load_bitmap(DataPath+'hero-walk-e03.png');
    Images[4] := al_load_bitmap(DataPath+'hero-walk-e04.png');
    Images[5] := al_load_bitmap(DataPath+'hero-walk-e05.png');
    Images[6] := al_load_bitmap(DataPath+'hero-walk-e06.png');
    Images[7] := al_load_bitmap(DataPath+'hero-walk-e07.png');
  end;
  // Load animation data: Player Attack Left
  with ImgPlayerAttackL do begin
    Delay := 6;
    Count := 7;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'hero-attack-w00.png');
    Images[1] := al_load_bitmap(DataPath+'hero-attack-w03.png');
    Images[2] := al_load_bitmap(DataPath+'hero-attack-w04.png');
    Images[3] := al_load_bitmap(DataPath+'hero-attack-w07.png');
    Images[4] := al_load_bitmap(DataPath+'hero-attack-w09.png');
    Images[5] := al_load_bitmap(DataPath+'hero-attack-w11.png');
    Images[6] := al_load_bitmap(DataPath+'hero-attack-w12.png');
  end;
  // Load animation data: Player Attack Right
  with ImgPlayerAttackR do begin
    Delay := 6;
    Count := 7;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'hero-attack-e00.png');
    Images[1] := al_load_bitmap(DataPath+'hero-attack-e03.png');
    Images[2] := al_load_bitmap(DataPath+'hero-attack-e04.png');
    Images[3] := al_load_bitmap(DataPath+'hero-attack-e07.png');
    Images[4] := al_load_bitmap(DataPath+'hero-attack-e09.png');
    Images[5] := al_load_bitmap(DataPath+'hero-attack-e11.png');
    Images[6] := al_load_bitmap(DataPath+'hero-attack-e12.png');
  end;
  // Load animation data: Player Idle Left
  with ImgPlayerIdleL do begin
    Delay := 12;
    Count := 9;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'hero-idle-w00.png');
    Images[1] := al_load_bitmap(DataPath+'hero-idle-w01.png');
    Images[2] := al_load_bitmap(DataPath+'hero-idle-w02.png');
    Images[3] := al_load_bitmap(DataPath+'hero-idle-w03.png');
    Images[4] := al_load_bitmap(DataPath+'hero-idle-w04.png');
    Images[5] := al_load_bitmap(DataPath+'hero-idle-w05.png');
    Images[6] := al_load_bitmap(DataPath+'hero-idle-w06.png');
    Images[7] := al_load_bitmap(DataPath+'hero-idle-w07.png');
    Images[8] := al_load_bitmap(DataPath+'hero-idle-w07.png');
  end;
  // Load animation data: Player Idle Right
  with ImgPlayerIdleR do begin
    Delay := 12;
    Count := 9;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'hero-idle-e00.png');
    Images[1] := al_load_bitmap(DataPath+'hero-idle-e01.png');
    Images[2] := al_load_bitmap(DataPath+'hero-idle-e02.png');
    Images[3] := al_load_bitmap(DataPath+'hero-idle-e03.png');
    Images[4] := al_load_bitmap(DataPath+'hero-idle-e04.png');
    Images[5] := al_load_bitmap(DataPath+'hero-idle-e05.png');
    Images[6] := al_load_bitmap(DataPath+'hero-idle-e06.png');
    Images[7] := al_load_bitmap(DataPath+'hero-idle-e07.png');
    Images[8] := al_load_bitmap(DataPath+'hero-idle-e07.png');
  end;
  // Load animation data: Player Been Hit Left
  with ImgPlayerBeenHitL do begin
    Delay := 6;
    Count := 4;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'hero-been-hit-w00.png');
    Images[1] := al_load_bitmap(DataPath+'hero-been-hit-w04.png');
    Images[2] := al_load_bitmap(DataPath+'hero-been-hit-w07.png');
    Images[3] := al_load_bitmap(DataPath+'hero-been-hit-w08.png');
  end;
  // Load animation data: Player Been Hit Right
  with ImgPlayerBeenHitR do begin
    Delay := 6;
    Count := 4;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'hero-been-hit-e00.png');
    Images[1] := al_load_bitmap(DataPath+'hero-been-hit-e04.png');
    Images[2] := al_load_bitmap(DataPath+'hero-been-hit-e07.png');
    Images[3] := al_load_bitmap(DataPath+'hero-been-hit-e08.png');
  end;
  // Load animation data: Zombie Walk Left
  with ImgZombieWalkL do begin
    Delay := 9;
    Count := 8;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'zombie-walk-w00.png');
    Images[1] := al_load_bitmap(DataPath+'zombie-walk-w01.png');
    Images[2] := al_load_bitmap(DataPath+'zombie-walk-w02.png');
    Images[3] := al_load_bitmap(DataPath+'zombie-walk-w03.png');
    Images[4] := al_load_bitmap(DataPath+'zombie-walk-w04.png');
    Images[5] := al_load_bitmap(DataPath+'zombie-walk-w05.png');
    Images[6] := al_load_bitmap(DataPath+'zombie-walk-w06.png');
    Images[7] := al_load_bitmap(DataPath+'zombie-walk-w07.png');
  end;
  // Load animation data: Zombie Walk Right
  with ImgZombieWalkR do begin
    Delay := 9;
    Count := 8;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'zombie-walk-e00.png');
    Images[1] := al_load_bitmap(DataPath+'zombie-walk-e01.png');
    Images[2] := al_load_bitmap(DataPath+'zombie-walk-e02.png');
    Images[3] := al_load_bitmap(DataPath+'zombie-walk-e03.png');
    Images[4] := al_load_bitmap(DataPath+'zombie-walk-e04.png');
    Images[5] := al_load_bitmap(DataPath+'zombie-walk-e05.png');
    Images[6] := al_load_bitmap(DataPath+'zombie-walk-e06.png');
    Images[7] := al_load_bitmap(DataPath+'zombie-walk-e07.png');
  end;
  // Load animation data: Zombie Attack Left
  with ImgZombieAttackL do begin
    Delay := 15;
    Count := 7;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'zombie-attack-w00.png');
    Images[1] := al_load_bitmap(DataPath+'zombie-attack-w02.png');
    Images[2] := al_load_bitmap(DataPath+'zombie-attack-w04.png');
    Images[3] := al_load_bitmap(DataPath+'zombie-attack-w06.png');
    Images[4] := al_load_bitmap(DataPath+'zombie-attack-w08.png');
    Images[5] := al_load_bitmap(DataPath+'zombie-attack-w09.png');
    Images[6] := al_load_bitmap(DataPath+'zombie-attack-w10.png');
  end;
  // Load animation data: Zombie Attack Right
  with ImgZombieAttackR do begin
    Delay := 15;
    Count := 7;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'zombie-attack-e00.png');
    Images[1] := al_load_bitmap(DataPath+'zombie-attack-e02.png');
    Images[2] := al_load_bitmap(DataPath+'zombie-attack-e04.png');
    Images[3] := al_load_bitmap(DataPath+'zombie-attack-e06.png');
    Images[4] := al_load_bitmap(DataPath+'zombie-attack-e08.png');
    Images[5] := al_load_bitmap(DataPath+'zombie-attack-e09.png');
    Images[6] := al_load_bitmap(DataPath+'zombie-attack-e10.png');
  end;
  // Load animation data: Zombie Killed Left
  with ImgZombieKilledL do begin
    Delay := 10;
    Count := 5;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'zombie-killed-w00.png');
    Images[1] := al_load_bitmap(DataPath+'zombie-killed-w03.png');
    Images[2] := al_load_bitmap(DataPath+'zombie-killed-w04.png');
    Images[3] := al_load_bitmap(DataPath+'zombie-killed-w07.png');
    Images[4] := al_load_bitmap(DataPath+'zombie-killed-w08.png');
  end;
  // Load animation data: Zombie Killed Right
  with ImgZombieKilledR do begin
    Delay := 10;
    Count := 5;
    SetLength(Images, Count);
    Images[0] := al_load_bitmap(DataPath+'zombie-killed-e00.png');
    Images[1] := al_load_bitmap(DataPath+'zombie-killed-e03.png');
    Images[2] := al_load_bitmap(DataPath+'zombie-killed-e04.png');
    Images[3] := al_load_bitmap(DataPath+'zombie-killed-e07.png');
    Images[4] := al_load_bitmap(DataPath+'zombie-killed-e08.png');
  end;

  // Load audio data
  AudioBackground := al_load_sample(DataPath+'audUntitledremix.ogg');
  AudioSword      := al_load_sample(DataPath+'audSword sound.wav');
  AudioHurt       := al_load_sample(DataPath+'audHit1.ogg');
  AudioFailed     := al_load_sample(DataPath+'audDie1.ogg');
  AudioInstance   := al_create_sample_instance(AudioBackground);


  // Create list for enemies
  Enemies := TFPList.Create;

  // Start the game
  GameIsRunning := True;

end;

procedure Start(State: TAppState);
const
  SavedAudioPosition: AL_UINT = 0; // Used for pausing and resumming the game music
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

procedure ProcessUserInput;
const
  EscJustPressed:   Boolean = False;
  SpaceJustPressed: Boolean = False;
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

  EnemiesExpiredCount := 0;
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
        al_draw_text(SystemFont, al_map_rgb(255, 255, 255), 20, 4,ALLEGRO_ALIGN_LEFT,
          'Killed: '+IntToStr(EnemyKilled));
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
    al_wait_for_event(EventQueue, Event);
    ProcessUserInput;
    ProcessUpdate;
    ProcessDrawing;
  end;
  ProcessShutdown;
end.
