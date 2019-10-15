;;CHIDENT

; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The CheckIdents and Procs part                              ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;


CHECKIDENTS:
    MOVE.W  #4,CURSPOT
    MOVE.L  #IDENTHASH+4,A5
.1A:    MOVE.L  A5,A4
    ADDQ.L  #4,A5
    CMP.L   #IDENTHASH+1028,A5
    BEQ.W   .2
.1: MOVE.L  -(A4),A4
    MOVE.L  A4,D0
    BEQ.S   .1A
    BTST    #0,5(A4)        ; SEE IF UNREFERENCED
    BEQ .UNREF
.UNRB:  MOVE.B  4(A4),D0
    TST.B   D0
    BEQ.S   .1          ; WAS .4
    CMP.B   #GLOBV,D0
    BEQ.W   .3
    CMP.B   #LOCV,D0
    BNE .5
    MOVEQ   #1,D0           ; -->LOC
    MOVE.L  D0,(A4)         ; PTR TO CHAR
    MOVE.L  6(A4),A3
    TST.W   10(A4)          ; AN ARG OR REGVAR
    BNE.S   .1

    ; no test for regalloc here.

;bra.s .pp
;.ppp: dc.l $1e0000
;.pp:
;move.l .ppp,a0
;move.l 12(a4),(a0)+
;move.l a0,.ppp

    MOVE.L  VARHEAVY(A4),D0     ; if 0 then cannot be regvar
    BEQ.W   .NR
    btst    #4,2(a3)
    bne     .NR

    MOVE.W  MAXREGALLOC(PC),D0
    CMP.B   3(A3),D0
    BLE.S   .RCALC

    BSET    #3,5(A4)        ; MAKE IT A REGVAR!
    ADDQ.B  #1,3(A3)
    MOVE.B  3(A3),D7
    EXT.W   D7
    NEG.W   D7
    ADDQ.W  #8,D7
    MOVE.W  D7,10(A4)       ; register nº
    MOVE.L  26(A3),D7
    BTST    #3,2(A3)
    BNE.S   .GOTT
    GETM    A0
    MOVE.L  A0,D7
    MOVE.L  A0,26(A3)
    BSET    #3,2(A3)        ; HASTABLE
    MOVEQ   #MAXREGVARS,D0      ; ONE TOO MUCH: NIL TERM
.ATL:   CLR.L   (A0)+
    DBRA    D0,.ATL
    DONEM   A0
.GOTT:  MOVE.L  D7,A0
.PL:    TST.L   (A0)+
    BNE.S   .PL
    MOVE.L  A4,-(A0)        ; ONE OF THE REGVARS
    BRA.W   .1

.RCALC: BTST    #3,2(A3)
    BEQ.S   .NR
    MOVE.L  26(A3),A0       ; A0=NOW FIND SMALLEST
    MOVE.L  #$7FFFFFFF,D1       ; D1=SMALLEST HEAVYNESS
    MOVEQ   #0,D2
.RCS:   MOVE.L  (A0)+,D0
    BEQ.S   .RCSO
    MOVE.L  D0,A1
    CMP.L   VARHEAVY(A1),D1
    BMI.S   .RCS
    MOVE.L  VARHEAVY(A1),D1
    MOVE.L  A1,D2           ; D2=BLACK SHEEP
    MOVE.L  A0,A2           ; A2=PLUG BACK
    BRA.S   .RCS
.RCSO:  TST.L   D2
    BNE.S   .RCR
    INTERN  104
.RCR:   MOVE.L  D2,A0           ; A0=OTHER IDENT
    MOVE.L  VARHEAVY(A0),D0
    CMP.L   VARHEAVY(A4),D0
    BPL.S   .NR
    MOVE.L  A4,-(A2)
    MOVE.W  10(A0),10(A4)
    BSET    #3,5(A4)
    BCLR    #3,5(A0)
    MOVE.W  4(A3),D0        ; MAKE IT A NEW LOCOFFSET
    SUBQ.W  #4,D0
    MOVE.W  D0,10(A0)
    MOVE.W  D0,4(A3)
    BRA .1

.NR:    MOVE.W  4(A3),D0        ; MAKE IT A NEW LOCOFFSET
    SUBQ.W  #4,D0
    MOVE.W  D0,10(A4)
    MOVE.W  D0,4(A3)
    BRA.W   .1

.2: TST.W   .UR         ; EXIT. BSR FROM OTHER POINTS
    BNE .WL
    RTS
.WL:    MOVEQ   #1,D3
    MOVE.L  #.WL2,D2
    BSR WRITECON
    RTS
.WL2:   DC.B    10,0

.3: GINFO   (A4),A3,A4
    MOVEQ   #1,D0           ; SET FLAGS=PTR TO CHAR
    MOVE.L  D0,(A4)         ; now - check for special structures;


    
    CMPA.L  PSEUDOA4-40,A4
    BNE     .NOTEXEC
    TST.L   OBJ_EXECBASE
    BEQ     .NOTEXEC
    MOVE.L  OBJ_EXECBASE(PC),(A4)
.NOTEXEC:
    CMPA.L  PSEUDOA4-48,A4
    BNE     .NOTINTUI
    TST.L   OBJ_INTUIBASE
    BEQ     .NOTINTUI
    MOVE.L  OBJ_INTUIBASE(PC),(A4)
.NOTINTUI:
    CMPA.L  PSEUDOA4-52,A4
    BNE     .NOTGFX
    TST.L   OBJ_GFXBASE
    BEQ     .NOTGFX
    MOVE.L  OBJ_GFXBASE(PC),(A4)
.NOTGFX:
    CMPA.L  PSEUDOA4-44,A4
    BNE     .NOTDOS
    TST.L   OBJ_DOSBASE
    BEQ     .NOTDOS
    MOVE.L  OBJ_DOSBASE(PC),(A4)
.NOTDOS:
    CMPA.L  PSEUDOA4-16,A4
    BNE     .NOTRST
    TST.L   OBJ_RASTPORT
    BEQ     .NOTRST
    MOVE.L  OBJ_RASTPORT(PC),(A4)
.NOTRST:
    CMPA.L  PSEUDOA4-36,A4
    BNE     .NOTWBM
    TST.L   OBJ_WBMESSAGE
    BEQ     .NOTWBM
    MOVE.L  OBJ_WBMESSAGE(PC),(A4)
.NOTWBM:
    TST.W   10(A4)
    BNE .1
    SUBQ.W  #4,NRGLOB       ; -->GLOB
    MOVE.W  NRGLOB(PC),10(A4)
    BRA .1

.5: CMP.W   #-4,10(A4)
    BEQ .1
    CMP.W   #-3,10(A4)
    BEQ .1
    CMP.W   #-2,10(A4)      ; -->LAB
    BEQ.S   .6
    CMP.W   #-1,10(A4)
    BNE.S   .ERL
    CLR.L   6(A4)
.6: MOVE.L  (A4),A1         ; CHECK FOR "MAIN"
    CMP.B   #"m",(A1)
    BNE.S   .NM
    CMP.B   #"a",1(A1)
    BNE.S   .NM
    CMP.B   #"i",2(A1)
    BNE.S   .NM
    CMP.B   #"n",3(A1)
    BNE.S   .NM
    CMP.B   #0,4(A1)
    BNE.S   .NM
    MOVE.W  #1,10(A4)       ; SET LAB MAIN
    MOVE.L  6(A4),A1
    TST.W   (A1)            ; MAIN --> NO ARGS
    BNE iERROR23
    BRA .1
.NM:    MOVE.W  CURLABNAME+2(PC),10(A4)
    ADDQ.L  #1,CURLABNAME
    BSR CHECKLABBUF
    BRA .1
.ERL:   BSR .2
    BRA iERROR14

.UNREF: BTST    #1,5(A4)
    BNE .UNRB
    TST.W   .UR
    BNE.S   .12
    MOVE.L  #.13,D2
    MOVE.L  #.14,D3
    SUB.L   D2,D3
    BSR WRITECON
    BRA.S   .16
.12:    MOVEQ   #1,D3
    MOVE.L  #.14,D2
    BSR WRITECON
.16:    MOVE.L  (A4),A0
.15:    TST.B   (A0)+
    BNE.S   .15
    SUB.L   (A4),A0
    SUBQ.L  #1,A0
    MOVE.L  A0,D3
    MOVE.L  (A4),D2
    BSR WRITECON
    MOVE.W  #-1,.UR
    BRA .UNRB
.13:    DC.B    "UNREFERENCED: "
.14:    DC.B    ","
    EVEN
.UR:    DC.W    0           ; MESSY DONE?

NRGLOB:
    DC.W    GLOBOFF         ; HASTABE initialized

CHECKLABBUF:
    SUBQ.W  #1,.1
    BEQ.S   .2
    RTS
.2: MOVE.L  D0,-(A7)
    MOVE.W  #25,.1
    MOVE.L  CURLABNAME(PC),D0
    LSL.L   #2,D0
    ADD.L   LABM+8(PC),D0
    MOVE.L  D0,LABM
    BSR REALLOC6
    MOVE.L  (A7)+,D0
    RTS
.1: DC.W    10
;*-*

