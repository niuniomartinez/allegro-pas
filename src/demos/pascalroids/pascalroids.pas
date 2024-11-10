program pascalroids;
(* Implementation of Asteroids. *)
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

{$IFDEF FPC}
  {$IFDEF WINDOWS}{$R 'manifest.rc'}{$ENDIF}
{$ENDIF}

uses
  Game;

var
  PascalroidsGame: TPascalroids;

begin
  PascalroidsGame := TPascalroids.Create;
  try
    if PascalroidsGame.Initialize then
      PascalroidsGame.Run
  finally
    PascalroidsGame.Free
  end
end.

See that the program can be rewritten as:

 program Pascalroids;
 uses Game;
 begin with TPascalroids.Create do try if Initialize then Run finally Free end end.

I was tempted to use that, as I find it elegant and also it needs less memory (8
octects in 64bit computers) but it is also a bit arrogant and it may send the
erroneous message that use `with` is good (and it isn't!).

BTW, didn't you notice that this unit has 4 semicolons, and that using `with` it
needs ONLY TWO?!?!  Semicolons are mandatory only to separate sentences in the
same ''level''.  That's why you can't put a semicolon before an `else`.

Also notice that this text isn't a { comment } but both Delphi and Free Pascal
won't return any compillation error.

Boy, I really love this language!
