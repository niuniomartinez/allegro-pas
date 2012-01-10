#!/bin/bash
#fpc -g -Fulib -FUobj -FEexamples examples/test.pas
#fpc -g -Fulib -FUobj -FEexamples examples/ex_gldepth.pas
#fpc -g -Fulib -FUobj -FEexamples examples/ex_font.pas
fpc -O3 -Fulib -FUobj -FEexamples examples/ex_haiku.pas
