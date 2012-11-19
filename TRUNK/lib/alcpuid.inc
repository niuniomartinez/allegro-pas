  CONST
(* @exclude CPU Capabilities flags for x86 capable chips *)
    AL_CPU_ID       = $0001;
{ @exclude }
    AL_CPU_FPU      = $0002;
{ @exclude }
    AL_CPU_MMX      = $0004;
{ @exclude }
    AL_CPU_MMXPLUS  = $0008;
{ @exclude }
    AL_CPU_SSE      = $0010;
{ @exclude }
    AL_CPU_SSE2     = $0020;
{ @exclude }
    AL_CPU_3DNOW    = $0040;
{ @exclude }
    AL_CPU_ENH3DNOW = $0080;
{ @exclude }
    AL_CPU_CMOV     = $0100;
{ @exclude }
    AL_CPU_AMD64    = $0200;
{ @exclude }
    AL_CPU_IA64     = $0400;
{ @exclude }
    AL_CPU_SSE3     = $0800;
{ @exclude }
    AL_CPU_SSSE3    = $1000;
{ @exclude }
    AL_CPU_SSE41    = $2000;
{ @exclude }
    AL_CPU_SSE42    = $4000;

(* @exclude CPU families - PC *)
    AL_CPU_FAMILY_UNKNOWN  = 0;
{ @exclude }
    AL_CPU_FAMILY_I386     = 3;
{ @exclude }
    AL_CPU_FAMILY_I486     = 4;
{ @exclude }
    AL_CPU_FAMILY_I586     = 5;
{ @exclude }
    AL_CPU_FAMILY_I686     = 6;
{ @exclude }
    AL_CPU_FAMILY_ITANIUM  = 7;
{ @exclude }
    AL_CPU_FAMILY_EXTENDED = 15;
(* @exclude CPUID only returns 15 bits, we need extra information from the CPU
   model to identify Pentium IV, Xeon and Athlon 64 processors. *)

(* @exclude CPU families - Power PC *)
    AL_CPU_FAMILY_POWERPC  = 18;

(* @exclude CPU models - PC *)
(* @exclude 486 *)
    AL_CPU_MODEL_I486DX    = 0;
{ @exclude }
    AL_CPU_MODEL_I486DX50  = 1;
{ @exclude }
    AL_CPU_MODEL_I486SX    = 2;
{ @exclude }
    AL_CPU_MODEL_I487SX    = 3;
{ @exclude }
    AL_CPU_MODEL_I486SL    = 4;
{ @exclude }
    AL_CPU_MODEL_I486SX2   = 5;
{ @exclude }
    AL_CPU_MODEL_I486DX2   = 7;
{ @exclude }
    AL_CPU_MODEL_I486DX4   = 8;

(* @exclude Intel/586 *)
    AL_CPU_MODEL_PENTIUM              = 1;
{ @exclude }
    AL_CPU_MODEL_PENTIUMP54C          = 2;
{ @exclude }
    AL_CPU_MODEL_PENTIUMOVERDRIVE     = 3;
{ @exclude }
    AL_CPU_MODEL_PENTIUMOVERDRIVEDX4  = 4;
{ @exclude }
    AL_CPU_MODEL_CYRIX                = 14;
{ @exclude }
    AL_CPU_MODEL_UNKNOWN              = 15;

(* @exclude AMD/586 *)
    AL_CPU_MODEL_K5                   = 0;
    AL_CPU_MODEL_K6                   = 6;

(* @exclude Intel/686 *)
    AL_CPU_MODEL_PENTIUMPROA          = 0;
{ @exclude }
    AL_CPU_MODEL_PENTIUMPRO           = 1;
{ @exclude }
    AL_CPU_MODEL_PENTIUMIIKLAMATH     = 3;
{ @exclude }
    AL_CPU_MODEL_PENTIUMII            = 5;
{ @exclude }
    AL_CPU_MODEL_CELERON              = 6;
{ @exclude }
    AL_CPU_MODEL_PENTIUMIIIKATMAI     = 7;
{ @exclude }
    AL_CPU_MODEL_PENTIUMIIICOPPERMINE = 8;
{ @exclude }
    AL_CPU_MODEL_PENTIUMIIIMOBILE     = 9;

(* @exclude AMD/686 *)
    AL_CPU_MODEL_ATHLON               = 2;
{ @exclude }
    AL_CPU_MODEL_DURON                = 3;


(* @exclude Information when CPU_FAMILY is 15 *)
    AL_CPU_MODEL_PENTIUMIV            = 0;
{ @exclude }
    AL_CPU_MODEL_XEON                 = 2;

    AL_CPU_MODEL_ATHLON64             = 4;
{ @exclude }
    AL_CPU_MODEL_OPTERON              = 5;

(* @exclude Information for Power PC processors *)
(* @exclude these defines are taken from <mach-o/machine.h> *)
    AL_CPU_MODEL_POWERPC_601	       = 1;
{ @exclude }
    AL_CPU_MODEL_POWERPC_602	       = 2;
{ @exclude }
    AL_CPU_MODEL_POWERPC_603	       = 3;
{ @exclude }
    AL_CPU_MODEL_POWERPC_603e	       = 4;
{ @exclude }
    AL_CPU_MODEL_POWERPC_603ev	       = 5;
{ @exclude }
    AL_CPU_MODEL_POWERPC_604	       = 6;
{ @exclude }
    AL_CPU_MODEL_POWERPC_604e	       = 7;
{ @exclude }
    AL_CPU_MODEL_POWERPC_620	       = 8;
{ @exclude }
    AL_CPU_MODEL_POWERPC_750	       = 9;
{ @exclude }
    AL_CPU_MODEL_POWERPC_7400	       = 10;
{ @exclude }
    AL_CPU_MODEL_POWERPC_7450	       = 11;
