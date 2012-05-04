#!/bin/bash
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_audio_simple.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/test.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_blit.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_font.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_gldepth.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_haiku.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_lines.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_prim.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_rotate.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_scale.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_transform.pas
fpc -dDEBUGMODE -g -Fulib -FUobj -FEexamples examples/ex_warp_mouse.pas

