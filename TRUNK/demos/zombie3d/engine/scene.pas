UNIT Scene;
(*<Manages an scene (map). *)
(*
  Copyright (c) 2015 Guillermo Martínez J.

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

INTERFACE

  USES
    Collision, Math3D,
    Al3D;

  CONST
  (* Maximun number of objects that can be added to an scene. *)
    MAX_OBJECTS = 100;

  TYPE
  (* @exclude Forward declaration. *)
    TScene3D = CLASS;



  (* Camera information. *)
    TCamera3D = OBJECT
    PRIVATE
      fScene: TScene3D;
    PUBLIC
    { Camera position. }
      Pos,
    { Camera angle. }
      Ang: TVector3D;
    { Field of view angle. }
      FOV: SINGLE;
    { Camera matrix. }
      Matrix: AL_MATRIX_f;

    (* Calculates the camera matrix. *)
      PROCEDURE CalcMatrix;

    (* Reference to the scene that contains the object. *)
      PROPERTY Scene: TScene3D READ fScene;
    END;



  (* Base class for objects in scene. *)
    TCustomObject3D = CLASS (TObject)
    PRIVATE
      fCollisionDescription: TCustomCollisionObject;
      fNdx: INTEGER;
      fScene: TScene3D;
    PROTECTED
    (* Object position. *)
      fPos: TVector3D;

    (* Sets the collision object. *)
      PROCEDURE SetCollisionObject (aObj: TCustomCollisionObject); INLINE;
    PUBLIC

    (* Constructor. *)
      CONSTRUCTOR Create; VIRTUAL;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;

    (* Index of the object in the scene. *)
      PROPERTY Index: INTEGER READ fNdx;
    (* Reference to the scene that contains the object. *)
      PROPERTY Scene: TScene3D READ fScene;
    (* Collision description. *)
      PROPERTY Collision: TCustomCollisionObject READ fCollisionDescription;
    (* Object position. *)
      PROPERTY Pos: TVector3D READ fPos;
    END;



  (* Sprites. *)
    TSprite3D = CLASS (TObject)
    PRIVATE
      fAng, fRadius: SINGLE;
    PUBLIC
    (* Sprite angle. *)
      PROPERTY Ang: SINGLE READ fAng;
    (* Sprite radius. *)
      PROPERTY Radius: SINGLE READ fRadius;
    END;



  (* Encapsulates an scene. *)
    TScene3D = CLASS (TObject)
    PRIVATE
      fCamera: TCamera3D;
      fObjects: ARRAY [0..(MAX_OBJECTS - 1)] OF TCustomObject3D;

      FUNCTION GetObject (CONST Index: INTEGER): TCustomObject3D; INLINE;
      PROCEDURE SetObject (CONST Index: INTEGER; aObj: TCustomObject3D); INLINE;
    PUBLIC
    (* Constructor. *)
      CONSTRUCTOR Create;
    (* Destructor. *)
      DESTRUCTOR Destroy; OVERRIDE;
    (* Clear all elements of the scene, destroying all objects. *)
      PROCEDURE Clear;
    (* Adds an object to the scene.
      @return(The object index or @code(-1) if scene is full of objects.) *)
      FUNCTION AddObject (aObj: TCustomObject3D): INTEGER;
    (* Removes an object from the scene and destroys it.
      @param(Index Index of the object to remove.)
      @seealso(ExtractObject) *)
      PROCEDURE DeleteObject (CONST Index: INTEGER); INLINE;
    (* Extracts an object from the scene without destroying it.
      @param(Index Index of the object to remove.)
      @return(The reference to the extracted object. It may be @nil.)
      @seealso(DeleteObject) *)
      FUNCTION ExtractObject (CONST Index: INTEGER): TCustomObject3D; INLINE;

    (* Camera. *)
      PROPERTY Camera: TCamera3D READ fCamera;
    (* Indexed access to the objects in the scene. The @code(Index) is zero
      based, i.e., runs from @code(0) (zero) to @code(MAX_OBJECTS-1).*)
      PROPERTY Objects[Index: INTEGER]: TCustomObject3D
        READ GetObject WRITE SetObject;
    END;

IMPLEMENTATION

  USES
    sysutils;

(*
 * TCamera3D
 ****************************************************************************)

(* Calculates the camera matrix. *)
  PROCEDURE TCamera3D.CalcMatrix;
  CONST
  { Binary to radian conversion.  See alFixed.al_fixtorad. }
    BIN_TO_RAD = 1608.0 / 65536.0;
  VAR
    Roller: AL_MATRIX_f;
    XFront, YFront, ZFront,
    XUp, YUp, ZUp: SINGLE;
  BEGIN
  { Lock angles. }
    IF Ang.X < 0 THEN Ang.X := Ang.X + 256;
    IF Ang.X >= 256 THEN Ang.X := Ang.X - 256;
    IF Ang.Y < 0 THEN Ang.Y := Ang.Y + 256;
    IF Ang.Y >= 256 THEN Ang.Y := Ang.Y - 256;
    IF Ang.Z < 0 THEN Ang.Z := Ang.Z + 256;
    IF Ang.Z >= 256 THEN Ang.Z := Ang.Z - 256;
  { Calculate the in-front vector }
    XFront := Sin (Ang.y * BIN_TO_RAD) * Cos (Ang.x * BIN_TO_RAD);
    YFront := Sin (Ang.x * BIN_TO_RAD);
    ZFront := Cos (Ang.y * BIN_TO_RAD) * Cos (Ang.x * BIN_TO_RAD);
  { Rotate the up vector around the in-front vector by the roll angle }
    al_get_vector_rotation_matrix_f (Roller, XFront, YFront, ZFront, Ang.z);
    al_apply_matrix_f (Roller, 0, -1, 0, XUp, YUp, ZUp);
  { Build the camera matrix }
    al_get_camera_matrix_f (
	Matrix,
	Pos.x, Pos.y, Pos.z,
	XFront, YFront, ZFront,
	XUp, YUp, ZUp,
	FOV,
	1 { Aspect ratio is square. }
    );
  END;



(*
 * TCustomObject3D
 ****************************************************************************)

(* Sets the collision object. *)
  PROCEDURE TCustomObject3D.SetCollisionObject (aObj: TCustomCollisionObject);
  BEGIN
    FreeAndNil (fCollisionDescription);
    fCollisionDescription := aObj
  END;



(* Constructor. *)
  CONSTRUCTOR TCustomObject3D.Create;
  BEGIN
    INHERITED Create;
    fCollisionDescription := NIL;
    fNdx := -1;
    fScene := NIL
  END;



(* Destructor. *)
  DESTRUCTOR TCustomObject3D.Destroy;
  BEGIN
    FreeAndNil (fCollisionDescription);
    INHERITED Destroy
  END;



(*
 * TScene3D
 ****************************************************************************)

  FUNCTION TScene3D.GetObject (CONST Index: INTEGER): TCustomObject3D;
  BEGIN
    RESULT := fObjects[Index]
  END;

  PROCEDURE TScene3D.SetObject (CONST Index: INTEGER; aObj: TCustomObject3D);
  BEGIN
    IF fObjects[Index] <> NIL THEN fObjects[Index].Free;
    fObjects[Index] := aObj;
    IF aObj <> NIL THEN
    BEGIN
      aObj.fNdx := Index; aObj.fScene := SELF
    END
  END;



(* Constructor. *)
  CONSTRUCTOR TScene3D.Create;
  VAR
    Ndx: INTEGER;
  BEGIN
    INHERITED Create;
    fCamera.fScene := SELF;
    FOR Ndx := LOW (fObjects) TO HIGH (fObjects) DO fObjects[Ndx] := NIL
  END;



(* Destructor. *)
  DESTRUCTOR TScene3D.Destroy;
  BEGIN
    SELF.Clear;
    INHERITED Destroy
  END;



(* Cleans the scene. *)
  PROCEDURE TScene3D.Clear;
  VAR
    Ndx: INTEGER;
  BEGIN
    FOR Ndx := LOW (fObjects) TO HIGH (fObjects) DO FreeAndNil (fObjects[Ndx])
  END;



(* Adds an object to the scene. *)
  FUNCTION TScene3D.AddObject (aObj: TCustomObject3D): INTEGER;
  VAR
    Ndx: INTEGER;
  BEGIN
    IF aObj <> NIL THEN
      FOR Ndx := LOW (fObjects) TO HIGH (fObjects) DO
	IF fObjects[Ndx] = NIL THEN
	BEGIN
	  fObjects[Ndx] := aObj;
	  aObj.fNdx := Ndx; aObj.fScene := SELF;
	  EXIT (Ndx)
	END;
    RESULT := -1
  END;



(* Removes an object from the scene and destroys it. *)
  PROCEDURE TScene3D.DeleteObject (CONST Index: INTEGER);
  BEGIN
    FreeAndNil (fObjects[Index])
  END;



(* Extracts an object from the scene without destroying it. *)
  FUNCTION TScene3D.ExtractObject (CONST Index: INTEGER): TCustomObject3D;
  BEGIN
    RESULT := fObjects[Index]; fObjects[Index] := NIL;
    IF RESULT <> NIL THEN
    BEGIN
      RESULT.fNdx := -1;
      RESULT.fScene := NIL
    END
  END;

END.
