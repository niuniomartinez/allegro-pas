#!/bin/bash
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_audio_simple.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/test.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_blit.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_font.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_gldepth.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_haiku.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_lines.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_prim.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_rotate.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_scale.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_transform.pas
fpc -O3 -Xs -XX -Fulib -FUobj -FEexamples examples/ex_warp_mouse.pas

