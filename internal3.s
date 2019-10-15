;; Internals
;; KillExe
KILLEXE:
    TST.W   KILLFLAG
    BEQ.S   .1
    MOVE.L  DOSBASE(PC),A6
    MOVE.L  #NAMEBUF,D1
    TSTMOD
    BEQ.S   .2
    MOVE.L  D1,A0
.3: TST.B   (A0)+
    BNE.S   .3
    MOVE.B  #".",-1(A0)
    MOVE.B  #"m",(A0)
    CLR.B   1(A0)
.2: JSR -72(A6)
.1: RTS
;*-*
;; CleanUp All
CLEANUPALL:
    move.l  DOSBASE,a6
    move.l  DOS_RDA,d1
    beq.S   .SKP
    JSR -$35A(A6)
.SKP:
    BSR CLOSECON
    BSR CLOSELOCK
    JSR FREEBUFFERS
    BSR SETOLDHANDLER
    BSR UNLOCKCACHE
    MOVE.L  DOSBASE,A1
    MOVE.L  4,A6
    JSR -414(A6)
    MOVE.L  UTILBASE,A1
    JSR -414(A6)
    TST.L   MATHBASE
    BEQ.S   .1
    MOVE.L  MATHBASE(PC),A1
    JSR -414(A6)
.1: RTS
;*-*
;; OpenCon
OPENCON:
    MOVE.L  #CONNAME,D1
    MOVE.L  #1006,D2
    MOVE.L  DOSBASE(PC),A6
    JSR -30(A6)
    MOVE.L  D0,STOUT
    BEQ LEAVEFAST       ; NO CON: --> DEEP TROUBLE
    RTS
;*-*
;; HoldCon
HOLDCON:
    MOVEM.L D0-D7/A0-A6,-(A7)
    BTST    #2,CODEPREFS+1
    BEQ.S   .2
    BSR READCON         ; MUST BE READABLE
.2: MOVEM.L (A7)+,D0-D7/A0-A6
    RTS
;*-*
;; CloseCon
CLOSECON:
    BSR HOLDCON
    TST.W   CONOUT
    BEQ.S   .1          ; NO CON: SO DON'T CLOSE IT
    MOVE.L  STOUT(PC),D1
    MOVE.L  DOSBASE(PC),A6
    JSR -36(A6)
.1: RTS
;*-*
;; ReadCon
READCON:
    MOVE.L  WORK(PC),D2
    MOVE.L  #10,D3
READSTRING:           ; D2=ADR, D3=LEN
    MOVE.L  STOUT(PC),D1
    MOVE.L  DOSBASE(PC),A6
    JSR -42(A6)
    RTS
;*-*

;; FixRetValue
FIXRETVALUE:
    MOVEQ   #0,D0
    MOVE.W  RETERROR(PC),D1
    BMI.S   .1
    MOVEQ   #5,D0
    BTST    #3,CODEPREFS+2
    BEQ.S   .2
    MOVE.L  D1,D0
    EXT.L   D0
    ADDQ.L  #1,D0
    BRA.S   .1
.2: BTST    #5,CODEPREFS+3
    BEQ.S   .1
    MOVE.L  BYTEOFF(PC),D0
.1: RTS
;*-*
;; LeaveError
LEAVEERROR:
    BSR ERROR
    BSR KILLEXE
LEAVEOTHERERROR:
    BSR CLEANUPALL
LEAVEFAST:
    MOVE.L  INITSTACK(PC),A7
    BSR.S   FIXRETVALUE
    RTS
;*-*
;; InstallHandler
INSTALLHANDLER:
    ;rts
    ;TST.W  CONOUT          ; DON'T INSTALL IF IN ASMONE
    ;BNE.S  .1
    MOVE.L  4.W,A6
    MOVE.L  276(A6),A0
    MOVE.L  50(A0),OLDTRAP
    MOVE.L  #TRAPHANDLER,50(A0)
    ;DIVU   #0,D0
.1: RTS
;*-*
;; SetOldHandler
SETOLDHANDLER:
    MOVE.L  4.W,A6
    MOVE.L  276(A6),A0
    MOVE.L  OLDTRAP,D0
    BEQ.S   .1
    MOVE.L  D0,50(A0)
.1: RTS
;*-*
;; TrapHandler
TRAPHANDLER:
    MOVE.L  (A7)+,D0
    MOVE.W  D0,GURUNUM
    MOVEQ   #3,D1
    CMP.L   D1,D0           ; BUS/ADR ER?
    BGT.S   .2
    MOVE.L  4.W,A6
    BTST    #0,297(A6)      ; 68010+ ?
    BNE.S   .2
    ADDQ.L  #8,A7
.2: MOVE.L  2(A7),CRASHPC
    MOVE.L  #.3,2(A7)
    RTE
.3: MOVE.W  GURUNUM(PC),D0
    MOVEQ   #31,D1
    BCLR    D1,D0
    CMP.L   #50,D0
    BMI.S   .1
    MOVEQ   #1,D0
.1: MOVE.W  D0,GURUNUM
    MOVE.L  CRASHPC(PC),D0
    SUB.L   #S,D0
    BMI.S   .4
    CMP.L   #100000,D0      ; MAX EXE SIZE EC
    BMI.S   .5
.4: MOVEQ   #0,D0
.5: MOVE.L  D0,CRASHPC
    JSR CHECKCOOKIES
DAMAGEDCOOKIES:           ; ENTRY FOR OWN-GEN ERRORS
    MOVE.W  LINENUM(PC),LINENUMC
    LEA INTERNALMESSY(PC),A0
    LEA CURSPOT(PC),A1
    TST.W   IEEEPROBLEM
    BNE.S   IEEEWARN
    BSR WRITEFORMAT
    BRA LEAVEOTHERERROR
IEEEWARN:
    MOVE.L  #IEEEMESSY,D2
    MOVEQ   #IEEEEND-IEEEMESSY,D3
    BSR WRITECON
    BRA LEAVEOTHERERROR
;*-*


CONNAME:
    DC.B    'CON:40/40/560/80/Amiga E Compiler Output',0
    EVEN

OLDTRAP:  DC.L    0

CURSPOT:  DC.W    0
GURUNUM:  DC.W    0
LINENUMC: DC.W    0
BUFSPOT:  DC.W    0
CRASHPC:  DC.L    0


INTERNALMESSY:    DC.B    "EC INTERNAL ERROR [%d,%d,%d,%d,$%lx] (please report!)",10,0
    EVEN
IEEEMESSY:    DC.B    "PROBLEM: your `mathieeesingbas.library' is"
            DC.B    " not properly patched (see docs)",10
IEEEEND:      EVEN

;*-*

