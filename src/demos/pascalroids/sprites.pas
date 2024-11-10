unit Sprites;
(* Implements the base classes for sprites. *)
(*
  Copyright (c) 2024 Guillermo Martínez J.

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
interface

  type
  (* Base class for sprites. *)
    TSprite = class (TObject)
    private
      fX, fY, fVx, fVy, fRadius: Double;
      fEnabled: Boolean;
    protected
    (* Update sprite position by its speed. *)
      procedure Move; inline;
    (* Check if sprite is outside the screen. *)
      function IsOutside: Boolean;
    (* Wrap the sprite around the screen.  Thats is, if sprite goes outside the
       screen it re-appears from the other side.

       aMargin is how much the sprite goes out of the screen before it wraps to
       the other side. *)
      procedure WrapAround (const aMargin: Integer);
    public
    (* Constructor. *)
      constructor Create; virtual;
    (* Initialize the sprite.

       It sets Velocity to 0 and Enable to True. *)
      procedure Initialize; virtual;
    (* Update sprite. *)
      procedure Update; virtual; abstract;
    (* Draw the sprite. *)
      procedure Draw; virtual; abstract;

    (* Check if sprite colided with given sprite.

       Note it doesn't check if the sprites are enabled. *)
      function CollideWith (aSpr: TSprite): Boolean; virtual;

    (* Sprite X coordinate. *)
      property X: Double read fX write fX;
    (* Sprite Y coordinate. *)
      property Y: Double read fY write fY;
    (* Sprite X velocity. *)
      property Vx: Double read fVx write fVx;
    (* Sprite Y velocity. *)
      property Vy: Double read fVy write fVy;
    (* Sprite radius. *)
      property Radius: Double read fRadius write fRadius;
    (* Tells if sprite is enabled.  This property is used by other methods and
       objects to know if they whould update or interact with the sprite.

       Default is @true. *)
      property Enabled: Boolean read fEnabled write fEnabled;
    end;



  (* Class of sprite ;) *)
    TSpriteClass = class of TSprite;
  (* Sprite collision notification. *)
    TCollisionNotification = procedure (S1, S2: TSprite) of object;



  (* Manages a group of sprites.

     It is an object pool. *)
    TSpriteManager = class (TObject)
    protected
    { I'm not using TObjectList here because it has a quite large footprint that
      does not give any advantage in this case.  A simple 'array' is fine.
    }
      fSpriteList: array of TSprite;
      fNextSprite: Integer;
    public
    (* Constructor.

       It creates all objects but it doesn't initialize them. *)
      constructor Create (
        const aNumObjects: Integer;
        aSpriteClass: TSpriteClass
      );
    (* Destructor. *)
      destructor Destroy; override;
    (* Disable all sprites. *)
      procedure DisableAll;
    (* Find the first disabled sprite in the list.

       Returns Nil if none found. *)
      function NewSprite: TSprite;
    (* Check collisions.

       It checks all collisions.  That is, if sprite isn't disabled by aHandler
       method then it will still check for new collisions.

       aSprite is passed as S1, the collided sprite is passed as S2.
     *)
      procedure CheckCollisions (
        aSprite: TSprite;
        aHandler: TCollisionNotification
      ); overload;
    (* Check collisions.

       It checks all collisions between given list sprites and Self sprites.

       Sprite in aSprList is passed as S1, the sprite in Self is passed as S2.
     *)
      procedure CheckCollisions (
        aSprList: TSpriteManager;
        aHandler: TCollisionNotification
      ); overload;
    (* Update enabled sprites. *)
      procedure Update;
    (* Draw enabled sprites. *)
      procedure Draw;
    end;

implementation

  uses
    Engine;

(*
 * TSprite
 ************************************************************************)

  procedure TSprite.Move;
  begin
    fX := fX + fVx; fY := fY + fVy
  end;



  function TSprite.IsOutside: Boolean;
  begin
    Result := (fX + fRadius < 0) or (fY + fRadius < 0)
           or (fX - fRadius > DisplayWidth) or (fY - fRadius > DisplayHeight)
  end;



  procedure TSprite.WrapAround (const aMargin: Integer);
  begin
    if fX + aMargin < 0 then fX := DisplayWidth + aMargin;
    if fY + aMargin < 0 then fY := DisplayHeight + aMargin;
    if fX - aMargin > DisplayWidth then fX := aMargin * (-1);
    if fY - aMargin > DisplayHeight then fY := aMargin * (-1)
  end;



  constructor TSprite.Create;
  begin
    inherited Create;
    fEnabled := False
  end;



  procedure TSprite.Initialize;
  begin
    fVx := 0; fVy := 0;
    fEnabled := True
  end;



  function TSprite.CollideWith (aSpr: TSprite): Boolean;
  begin
  { Use good old Pythagoras. }
    Result := Sqr (Self.X - aSpr.X) + Sqr (Self.Y - aSpr.Y)
            < Sqr (Self.Radius + aSpr.Radius)
  end;



(*
 * TSpriteManager
 ************************************************************************)

  constructor TSpriteManager.Create (
    const aNumObjects: Integer;
    aSpriteClass: TSpriteClass
  );
  var
    lNdx: Integer;
  begin
    inherited Create;
    SetLength (fSpriteList, aNumObjects);
    for lNdx := Low (fSpriteList) to High (fSpriteList) do
      fSpriteList[lNdx] := aSpriteClass.Create;
    fNextSprite := 0
  end;



  destructor TSpriteManager.Destroy;
  var
    lNdx: Integer;
  begin
    for lNdx := Low (fSpriteList) to High (fSpriteList) do
      fSpriteList[lNdx].Free;
    inherited Destroy
  end;



  procedure TSpriteManager.DisableAll;
  var
    lNdx: Integer;
  begin
    for lNdx := Low (fSpriteList) to High (fSpriteList) do
      fSpriteList[lNdx].Enabled := False
  end;


  function TSpriteManager.NewSprite: TSprite;
  var
    lEnd: Integer;
  begin
  { It's a ring buffer. }
    lEnd := fNextSprite;
    repeat
      if not fSpriteList[fNextSprite].Enabled then
        Exit (fSpriteList[fNextSprite]);
      Inc (fNextSprite);
      if fNextSprite > High (fSpriteList) then fNextSprite := 0
    until fNextSprite = lEnd;
  { Not found. }
    Result := Nil
  end;



  procedure TSpriteManager.CheckCollisions (
    aSprite: TSprite;
    aHandler: TCollisionNotification
  );
  var
    lNdx: Integer;
  begin
    if not aSprite.Enabled then Exit; { Shouldn't happen but just to be sure. }
    for lNdx := Low (fSpriteList) to High (fSpriteList) do
      if fSpriteList[lNdx].Enabled and aSprite.CollideWith (fSpriteList[lNdx]) then
      begin
        aHandler (aSprite, fSpriteList[lNdx]);
        if not aSprite.Enabled then Exit { Maybe aHandler disabled it. }
      end
  end;



  procedure TSpriteManager.CheckCollisions (
    aSprList: TSpriteManager;
    aHandler: TCollisionNotification
  );
  var
    lNdx: Integer;
  begin
    for lNdx := Low (aSprList.fSpriteList) to High (aSprList.fSpriteList)
     do
      if aSprList.fSpriteList[lNdx].Enabled then
      begin
        Self.CheckCollisions (aSprList.fSpriteList[lNdx], aHandler);
        if not aSprList.fSpriteList[lNdx].Enabled then continue { Maybe aHandler disabled it. }
      end
  end;



  procedure TSpriteManager.Update;
  var
    lNdx: Integer;
  begin
    for lNdx := Low (fSpriteList) to High (fSpriteList) do
      if fSpriteList[lNdx].Enabled then fSpriteList[lNdx].Update
  end;



  procedure TSpriteManager.Draw;
  var
    lNdx: Integer;
  begin
    for lNdx := Low (fSpriteList) to High (fSpriteList) do
      if fSpriteList[lNdx].Enabled then fSpriteList[lNdx].Draw
  end;

end.
