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
    Math3D;

  TYPE
  (* Sprite information. *)
    TSprite3D = OBJECT
    PUBLIC
    { Sprite position. }
      Pos: TVector3D;
    { Sprite angle. }
      Ang,
    { Sprite radius. }
      Radius: SINGLE;
    END;



  (* Encapsulates an scene. *)
    TScene3D = CLASS (TObject)
    PRIVATE
      fSprites: ARRAY OF TSprite3D;

      FUNCTION GetNumSprites: INTEGER; INLINE;
      FUNCTION GetSprite (CONST Ndx: INTEGER): TSprite3D; INLINE;
    PUBLIC
    (* Number of sprites. *)
      PROPERTY NumSprites: INTEGER READ GetNumSprites;
    (* Indexed access to sprites. *)
      PROPERTY Sprites[Ndx: INTEGER]: TSprite3D READ GetSprite;
    END;

IMPLEMENTATION

(*
 * TScene3D
 ****************************************************************************)

  FUNCTION TScene3D.GetNumSprites: INTEGER;
  BEGIN
    RESULT := Length (fSprites)
  END;



  FUNCTION TScene3D.GetSprite (CONST Ndx: INTEGER): TSprite3D;
  BEGIN
    RESULT := fSprites[Ndx]
  END;

END.
