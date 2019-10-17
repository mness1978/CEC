;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;                                                                       ;
;   Amiga E Compiler/Assembler/Linker by $#%! in 1991/92/93/94/95       ;
;   CreativE 2.12 by Tomasz Wiszkowski (error/bb)                       ;
;                                                                       ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;



CREATIVE_VERSION: MACRO
    DC.B    "CreativE "
    DC.B    "2.12.3 (09.07.01)"
    DC.B    " by Tomasz Wiszkowski (Error/bla²)",10
    ENDM

VERSION_020 EQU 0       ; just a couple of changes; no speedup...

DEBUG_TYPE: macro
    IFNE DEBUG_MSGS
        MOVEM.L D0-A6,-(A7)
        MOVE.L #.dbg,D2
        MOVE.L #.edbg-.dbg,D3
        JSR WRITECON
        move.l #EXAMPLE,d2
        move.l #2,d3
        JSR WRITECON
        MOVEM.L (A7)+,D0-A6
        BRA .edbg
.dbg: DC.B \1
        even
.edbg:
    endc
    endm

    IFEQ VERSION_020
GOSR: MACRO
    JSR \1
    ENDM
    ELSE
GOSR: MACRO
    BSR.L \1
    ENDM
    ENDC


S:  MOVE.L  A7,INITSTACK
    GOSR INITALL
;; check stack
    MOVE.L  D0,D7
    MOVE.L  A0,A5
    MOVE.L  $4.W,A6
    MOVE.L  276(a6),a1
    MOVE.L  62(A1),D1
    SUB.L   58(A1),D1
    CMP.L   #20000,D1
;    BPL     .NUFF
;    MOVE.L  #80000,D0
;    BSR     NEW
;    BEQ     iERROR38
;    ADD.L   #80000,D0
;    MOVE.L  D0,A7
    blt     iERROR108
    MOVE.L  A5,A0
    MOVE.L  D7,D0

.NUFF:
;*-*
    GOSR INITBUFFERS
    GOSR CLEARHASHTAB
    GOSR SETVARS
    GOSR MAKEKEYHASH
    GOSR MAKEASMHASH
    GOSR SETUPLIBS
    GOSR INITFUNCS
    GOSR ADDGLOBVARS
    GOSR GETCACHE
    GOSR CHECKCOOKIES
    GOSR TOKENIZE
    GOSR CHECKCOOKIES
    GOSR SECONDARYLOAD
    GOSR CHECKIDENTS
    GOSR SWAPVALUES
    GOSR MAKEINITCODE
    GOSR CHECKCOOKIES
    GOSR DOARGMACROS
    GOSR PASS1
    GOSR CHECKCOOKIES
    GOSR LINKDELEGATECONSTRUCTORS
    GOSR MAKEEXITCODE
    GOSR MATHEXITETC
    GOSR LINKFUNCS
    GOSR LINKLIB
    GOSR APPEND_LISTS

    GOSR CALCADR

    GOSR WRITEEXE
    GOSR WRITELIBMOD
    GOSR CHECKCOOKIES
    GOSR NOERROR
    GOSR WRITEBUFFERS
    GOSR CLEANUPALL
    GOSR FIXRETVALUE
    MOVE.L  INITSTACK,A7
    RTS


K:  BSET    #6,CODEPREFS+3      ; -w
    BRA S

SC: MOVE.L  #CLINE,A0
    MOVEQ   #CLEND-CLINE,D0
    BRA S
CLINE:  DC.B    " ",10
CLEND:
    EVEN

EXAMPLE:
    DC.B    " ",10

EXAMPLEEND:
    DC.B    10,10

EVEN

;; Ident types
; No more demoversions!
UNDEF   = 0             ; ident types
LOCV    = 1
GLOBV   = 2
LAB = 3
;*-*
;; interim codes
EOI = 0             ; interimcodes
VALUE   = 1
LIBC    = 2
COM = 3
ASSGN   = 4
IDENT   = 5
STR = 6
;*-*
;; Object structure
ONEXT   = -4                ; M O  OBJECT STRUCTURE OFFSETS
OOFF    = 0                 ; M    M=MEMBER
ODEL    = 0                 ;   O  O=OBJECT
OFLAGS  = 2                 ; M O
OSIZE   = 4                 ; M O
OID = 6                     ; M O
OASCII  = 8                 ; M O
OPTRTYPE= 12                ; M
OSIZEOF = 16                ; M
OMEMB   = 12                ;   O
OMETHOD = 16                ;   O
OSUPER  = 20                ;   O
ODCODE  = 24                ;   O
OACC    = 28                ;   O
ODELOFF = 32                ;   O
ODESTR  = 34                ;   O
;*-*
;; Hunklist structure
H_NEXT  =   -4      ; ptr to hunk
H_ADDR  =   0       ; where it begins..
H_TYPE  =   4       ; it's type
H_END   =   8       ; where it ends..
H_PREV  =   12      ; previous
H_RELO  =   16      ; relocs (longs!)

R_NEXT  =   -4      ; reloc next
R_HUNK  =   0       ; destination hunk number
R_OFF   =   2       ; source hunk offset
;*-*
;; Lib structure
LIB_NEXT    =   -4      ; next
LIB_NAME    =   0       ; name
LIB_ARGS    =   4       ; args count
LIB_CLEN    =   6       ; code len
LIB_CODE    =   10      ; code itself
LIB_USED    =   14      ; set if used/position in code
LIB_TYPE    =   18      ; type (0=proc, 1=inline, 2=startup etc)
LIB_TARR    =   20      ; table args array - regs (inline only)
LIB_RELO    =   28      ; reloc
LIB_XTNS    =   32      ; extension - for XREFs and XDEFs.
LIB_CPU     =   36      ; required CPU
LIB_FPU     =   38      ; required FPU
LIB_MMU     =   40      ; required MMU
LIB_OSVERS  =   42      ; required os version
LIB_SIZE    =   44      ; SIZEOF lib
;*-*
;; Method structure
M_NEXT  = 0             ; METHOD
M_PROC  = 4
M_TYPE  = 8
M_FLAGS = 9
M_OFF   = 10
M_NAME  = 12
;*-*
;; method flags
MT_METHOD   = 0         ; METHODFLAGS
MT_FROZEN   = 1
MT_SHARED   = 2
MT_EMPTY    = 3
;*-*
;; module info
MI_NEXT     = 0         ; MODULEINFO
MI_FLAGS    = 4
MI_NAMELEN  = 6
MI_MOD      = 10
MI_LIST     = 14
MI_NAMEPTR  = 18
;MI_NAME    = 22            ; ARRAY FROM HERE...
;*-*
;; proc or class
PC_NEXT     = 0         ; PROC_OR_CLASS
PC_TYPE     = 4
PC_INFO     = 6
PC_ACC      = 10
;*-*
;; constants structure
CNEXT   = 0             ; CONST STRUCTURE OFFSETS
CASCII  = 4
CVAL    = 8
CFLAGS  = 12
;*-*

CURVERSION  = 11            ; IMPORTANT, MODULE VERSION!!

;; system consts
SKIPMARK    = $8000

MAXREGVARS  = 5         ; NOTE: DEPENDS ALSO ON EFUNCS!
MAXMULTRET  = 3         ; NOTE: DEPENDS ON REGVARS!
MIDHEAVY    = $100
VARHEAVY    = 12            ; OFFSET AT WHICH HEAVYINFOS IN ID

MINSTACKSIZE    = 10000         ; van gen. code
MINSTACKSIZELIB = 1500
MAXOBJSIZE  = 5000
;*-*
;; A4handle
CHOPMEM     = -100
CHOPLEFT    = -104
FMEMSIZE    = 5000
GLOBOFF     = -512
GLOBOFFNEWTAB   = GLOBOFF+16        ; 16..256+16
NEWTABMAX   = 256
;*-*
;; customs
IOFF        = 100
KB      = 1024
MAXIDENT    = 1000          ; size of *one* identifier
MAXSTACK    = 30000         ; each rec.step=6-10b+rec.databuffer
MAXLIBRAISE = 200           ; EACH = 10 BYTES
;*-*
MINIMUM_ACODE   = 100*KB        ; GENERATED CODE-BUF

;; MACRO SECTION
;; TSTMOD - check if we're in a module
TSTMOD: MACRO
    BTST    #4,CODEPREFS+2
    ENDM
;*-*
;; RANGE CHECK MACROS
;; CheckB
CHKB:   MACRO               ; \1=REG, \2=TRREG, \3=ERLAB
    MOVE.L  \1,\2
    BTST    #31,\2
    BEQ.S   *+4
    NEG.L   \2          ; BAD: RANGE=(-255,255)=>(-128,255)
    clr.b   \2
    TST.L   \2
    BNE \3
    ENDM
;*-*
;; CheckW
CHKW:   MACRO               ; \1=REG, \2=TRREG, \3=ERLAB
    MOVE.L  \1,\2
    BTST    #31,\2
    BEQ.S   *+4
    NEG.L   \2
    SWAP    \2
    TST.W   \2
    BNE \3
    ENDM
;*-*
;*-*
;; HASHING MACROS
;; HashC
HASHC:  MACRO               ; \1=HASHREG, \2=CHAR
    LSL.W   #4,\1
    ADD.B   \2,\1
    ENDM
;*-*
;; HashE
HASHE:  MACRO               ; \1=HASHREG
    DIVU    #211,\1
    SWAP    \1
    EXT.L   \1
    ENDM
;*-*
;; Hash
HASH:   MACRO               ; \1=ASCREG, \2=DATREG, \3=TRASHREG
    MOVEQ   #0,\2
.H1:MOVE.B  (\1)+,\3
    BEQ.S   .H2
    HASHC   \2,\3
    BRA.S   .H1
.H2:    HASHE   \2
    ENDM
;*-*
;*-*
;; APART VAR ACCESS: ASM_GETLABEL
;; GENGI - GENerateGlobalInfo: A4=MID INS.
GENGI:  MACRO               ; \1=IDENTREC, \2=TRASHDX
    MOVE.L  6(\1),\2        ; SEE IF WE HAVE TO ADD A GLOBINFO ENTRY
    BEQ.S   .GVNGI
    MOVEM.L \1/A3,-(A7)
    MOVE.L  \2,\1
    MOVE.L  4(\1),\2
    MOVE.L  \1,A3
    GETM    \1
    MOVE.L  \1,4(A3)
    MOVE.L  \2,(\1)+
    MOVE.L  A4,(\1)+
    MOVE.W  OPERSIZE,(\1)+
    DONEM   \1
    MOVEM.L (A7)+,\1/A3
.GVNGI:
    ENDM
;*-*
;; OpLong - Set operator size
OPLONG: MACRO
    MOVE.W  #4,OPERSIZE
    ENDM
;*-*
;; OpInt
OPINT:  MACRO
    MOVE.W  #2,OPERSIZE
    ENDM
;*-*
;; OpChar
OPCHAR: MACRO
    MOVE.W  #1,OPERSIZE
    ENDM
;*-*
;; GenVar
; OFF_1:IMM, IDENT_2:Ax, OFFSET_3:EA.W, THRASH_4:Dx
             ; NOT A3-A5!!

GENVAR: MACRO
    MOVE.B  4(\2),\4
    BNE .1
    JMP ERROR22         ; TEST FOR UNIDENT
.1:
    CMP.B   #GLOBV,\4
    BNE.S   .GV1
    MOVE.W  -2(A4),\4
    BCLR    #\1,\4          ; ADJUSTMODE
    MOVE.W  \4,-2(A4)

    GENGI   \2,\4

.GV1:   BTST    #3,5(\2)        ; SEE IF REGVAR
    BNE.S   .GV2
    MOVE.W  \3,(A4)+
    BRA.S   .GV3
.GV2:   MOVE.W  10(\2),\4
    IFEQ \1
     ANDI.W #%1111111111000000,-2(A4)
    ELSE
     AND.W  #%1111000000111111,-2(A4)
     LSL.W  #8,\4
     LSL.W  #1,\4
    ENDC
    OR.W    \4,-2(A4)
.GV3:                   ; END
    ENDM
;*-*

MOVEMS:     MOVEM.L D0/D1,-(A7)
MOVEMR:     MOVEM.L (A7)+,D0/D1
SMASK:      DC.W    0
;*-*
;; BitM
BITM:   MACRO               ; \1=BITNUM, \2=REG
    BTST    #\1,\2
    BEQ.S   .SR\1
    BSET    #15-\1,\2
.SR\1:
    ENDM
;*-*
;; LSaver
LSAVER: MACRO               ; \1=SAVEMASK.EA.W, \2=RETURN_SMASK.DX
    MOVE.W  \1,\2
    AND.W   #%11111000,\2       ; diffr. from SAVER
    MOVE.W  \2,SMASK
    BEQ.S   .NOSR
    MOVE.W  MOVEMS,(A4)+
    BITM    7,\2
    BITM    6,\2
    BITM    5,\2
    BITM    4,\2
    BITM    3,\2
    CLR.B   \2
    MOVE.W  \2,(A4)+
    MOVE.W  SMASK,\2
.NOSR:                  ; \2 IS HERE ATLEAST BOOL
    ENDM
;*-*
;; Saver
SAVER:  MACRO               ; \1=SAVEMASK.EA.W, \2=RETURN_SMASK.DX
    MOVE.W  \1,\2
    AND.W   PROCMASK,\2
    MOVE.W  \2,SMASK
    BEQ.S   .NOSR
    MOVE.W  MOVEMS,(A4)+
    BITM    7,\2
    BITM    6,\2
    BITM    5,\2
    BITM    4,\2
    BITM    3,\2
    CLR.B   \2
    MOVE.W  \2,(A4)+
    MOVE.W  SMASK,\2
.NOSR:                  ; \2 IS HERE ATLEAST BOOL
    ENDM
;*-*
;; Restr
RESTR:  MACRO               ; \1=SMASK.EA
    TST.W   \1
    BEQ.S   .NORR
    MOVE.W  MOVEMR,(A4)+
    MOVE.W  \1,(A4)+
.NORR:
    ENDM
;*-*
;; Lower
LOWER:  MACRO               ; \1=AX (STRING,TRASH), \2=(TRASH)
.LOWL:  MOVE.B  (\1),\2
    BEQ.S   .LOWO
    CMP.B   #"A",\2
    BMI.S   .LOWN
    CMP.B   #"Z"+1,\2
    BPL.S   .LOWN
    BSET    #5,\2;ADD.B   #32,\2
.LOWN:  MOVE.B  \2,(\1)+
    BRA.S   .LOWL
.LOWO:                  ; ax points at 0-byte at end string
    ENDM
;*-*
;; Lower2
LOWER2:  MACRO               ; \1=AX (STRING,TRASH), \2=(TRASH)
.LOWL2:  MOVE.B  (\1),\2
    BEQ.S   .LOWO2
    CMP.B   #"A",\2
    BMI.S   .LOWN2
    CMP.B   #"Z"+1,\2
    BPL.S   .LOWN2
    BSET    #5,\2;ADD.B   #32,\2
.LOWN2:  MOVE.B  \2,(\1)+
    BRA.S   .LOWL2
.LOWO2:                  ; ax points at 0-byte at end string
    ENDM
;*-*
;; FAST MEMORY ALLOCATION MACROS
;; GetM
GETM:   MACRO               ; \1=Ax (START USING MEM)
    MOVE.L  HEAP,\1
    ENDM
;*-*
;; DoneM
DONEM:  MACRO               ; \1=Ax (DONE WITH MEM (LIGHT VERSION))
    MOVE.L  \1,HEAP
    SUBQ.W  #1,CHECKHEAP
    BNE.S   *+16
    JSR     EXPAND5
    MOVE.W  #5,CHECKHEAP        ; MAX 200 BYTES EACH = IDENT OR STRUCT
    ENDM                ; doesn't trash \1 !!!!
;*-*
;; DoneH
DONEH:  MACRO               ; \1=Ax (HEAVY VERSION)
    MOVE.L  \1,HEAP
    BSR.L EXPAND5
    ENDM
;*-*
;*-*
;; CREATE GLOBINFO STRUCTURE FOR IDENT
;; GInfo
GINFO:  MACRO               ; \1=ASCIIPTR.EA, \2=TRASH.AX, \3=IDENTPTR.AX
    TSTMOD
    BEQ.S   .NGINF
    GETM    \2          ; BUILD "GLOBINFO"
    BTST    #1,5(\3)
    BNE.S   .NGINF
    MOVE.L  \2,6(\3)
    MOVE.L  \1,(\2)+
    CLR.L   (\2)+
    DONEM   \2
    BRA.S   .NGOUT
.NGINF: CLR.L   6(\3)
.NGOUT:
    ENDM
;*-*
;; ChEStB
CHESTB: MACRO               ; CHeckEStackBuf \1=CURBUF.EA, \2=TR.DX
    SUBQ.W  #1,.BC          ; \3=DIST, \4=ERROR
    BNE.S   .NC
    MOVE.W  #\3,.BC
    MOVE.L  #ESTACKBUF,\2
    ADD.L   #MAXSTACK,\2
    SUB.L   #1000,\2
    CMP.L   \1,\2
    BMI \4
    BRA.S   .NC
.BC:    DC.W    \3
.NC:
    ENDM
;*-*
;; INTERN
INTERN: MACRO               ; \1=ERRNUM
    MOVE.W  #\1,GURUNUM
    BRA.L DAMAGEDCOOKIES
    ENDM
;*-*
;*-*
;*-*


    INCDIR ""

    INCLUDE 'ppc.i'

    INCLUDE 'preprocessor.s'
    INCLUDE 'internal1.s'
    INCLUDE 'parse.s'
    INCLUDE 'ident.s'
    INCLUDE 'assembler.s'
    INCLUDE 'error1.s'
    INCLUDE 'chkident.s'
    INCLUDE 'io1.s'
    INCLUDE 'cache.s'
    INCLUDE 'internal2.s'
    INCLUDE 'io2.s'
    INCLUDE 'internal3.s'
    INCLUDE 'compiler.s'
    INCLUDE 'appendlists.s'
    INCLUDE 'address.s'
    INCLUDE 'expressions.s'
    INCLUDE 'error2.s'
    INCLUDE 'linker.s'
    INCLUDE 'functions.s'
    INCLUDE 'Datas.s'
;; BBS section

; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   UnInitialized DataSection for E Compiler                    ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;

        SECTION EDATA,BSS
FIB:        DS.B    300
        EVEN
NAMEBUF:    DS.B    300
        EVEN
PROCBUF:    DS.B    300
        EVEN
SRCDIRBUF:  DS.B    300
        EVEN
DIRNAME:    DS.B    300
        EVEN
PRINTBUF:   DS.B    300
        EVEN
STCNAME:    DS.B    256
        EVEN
WORKBUF:    DS.B    MAXIDENT+2
        EVEN
ESTACKBUF:  DS.B    MAXSTACK+2
        EVEN
EFUNCBYTE:  DS.B    NREFUNC+10
        EVEN
EFUNCRAISE: DS.B    (NREFUNC+10)*10
        EVEN
LIBRAISE:   DS.B    MAXLIBRAISE*10
        EVEN
BRACKETSBUF:    DS.B    MAXBRACK*4
        EVEN
HASHBUFS:               ; CONTIGUOUS BLOCKS
KEYHASH:    DS.B    1024
ASMHASH:    DS.B    1024
AKWHASH:    DS.B    1024
PPCHASH:    DS.B    1024
CONSTHASH:  DS.B    1024
IDENTHASH:  DS.B    1024
MACROHASH:  DS.B    1024
LIBHASH:    DS.B    1024
            DS.B    -GLOBOFF
PSEUDOA4:
ENDHASHBUFS:
;*-*
END



