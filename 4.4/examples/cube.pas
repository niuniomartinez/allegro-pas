UNIT cube;
(*______   ___    ___
 /\  _  \ /\_ \  /\_ \
 \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___        __    ___      ____
  \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\    /'__`\ /\__`\  /'___/
   \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \__/\ \L\ \\/ __ \/\____`\
    \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/\_\ \  __//\____/\/\____/
     \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/\/_/\ \ \/ \/___/  \/___/
                                    /\____/               \ \_\
                                    \_/__/                 \/_/

   Defines a simple 3D cube used by 3D examples.

   by Ñuño Martínez <niunio(at)users.sourceforge.net> *)

{$IFDEF FPC}
{ Free Pascal. }
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

INTERFACE

USES
  allegro, alfixed;



TYPE
(* A simple vector. *)
  TVector = CLASS
  PRIVATE
    fx, fy, fz: AL_FIXED;
  PUBLIC
    CONSTRUCTOR Create (ax, ay, az: AL_FIXED);

    PROPERTY x: AL_FIXED READ fx WRITE fx;
    PROPERTY y: AL_FIXED READ fy WRITE fY;
    PROPERTY z: AL_FIXED READ fz WRITE fz;
  END;



(* A simple cube. *)
  TCube = CLASS
  PRIVATE
    fPosition, fAngle: TVector;
    fDrawmode: LONGINT;
    fTexture: AL_BITMAPptr;
  PUBLIC
  (* Creates the cube. *)
    CONSTRUCTOR Create (px, py, pz: AL_FIXED; aMode: LONGINT; aTexture: AL_BITMAPptr);
  (* Destroys the cube. *)
    DESTRUCTOR Destroy; OVERRIDE;
  (* Draws the cube. *)
    PROCEDURE Draw (aBitmap: AL_BITMAPptr);

  (* Cube position. *)
    PROPERTY Pos: TVector READ fPosition WRITE fPosition;
  (* Cube angle. *)
    PROPERTY Ang: TVector READ fAngle WRITE fAngle;
  (* Polygon draw mode. *)
    PROPERTY DrawMode: LONGINT READ fDrawmode WRITE fDrawmode;
  (* Cube texture.  Note it isn't destroyed by the cube. *)
    PROPERTY Texture: AL_BITMAPptr READ fTexture WRITE fTexture;
  END;



IMPLEMENTATION

USES
  al3d;



(***********
 * TVector *
 ***********)
  CONSTRUCTOR TVector.Create (ax, ay, az: AL_FIXED);
  BEGIN
    fx := ax;
    fy := ay;
    fz := az;
  END;



(*********
 * TCube *
 *********)

(* Constructor. *)
  CONSTRUCTOR TCube.Create (px, py, pz: AL_FIXED; aMode: LONGINT; aTexture: AL_BITMAPptr);
  BEGIN
    fPosition := TVector.Create (px, py, pz);
    fAngle := TVector.Create (0, 0, 0);
    fDrawmode := aMode;
    fTexture := aTexture;
  END;



(* Destructor. *)
  DESTRUCTOR TCube.Destroy;
  BEGIN
    fPosition.Free;
    fAngle.Free;
    INHERITED Destroy;
  END;


(* Draws the cube. *)
  PROCEDURE TCube.Draws (aBitmap: AL_BITMAPptr);
  BEGIN
  { vertices of the cube }
    AssignPoint (1, -32, -32, -32);
    AssignPoint (2, -32,  32, -32);
    AssignPoint (3,  32,  32, -32);
    AssignPoint (4,  32, -32, -32);
    AssignPoint (5, -32, -32,  32);
    AssignPoint (6, -32,  32,  32);
    AssignPoint (7,  32,  32,  32);
    AssignPoint (8,  32, -32,  32);
  { faces of the cube. }
    AssignFace (1, 0, 3, 2, 1);
    AssignFace (2, 4, 5, 6, 7);
    AssignFace (3, 0, 1, 5, 4);
    AssignFace (4, 2, 3, 7, 6);
    AssignFace (5, 0, 4, 7, 3);
    AssignFace (6, 1, 2, 6, 5);
  END;


END.
