;;INIT

INITALL:
    MOVE.W  #6,CURSPOT
    MOVE.L  D0,ARGLEN       ; SAVE THESE 1ST
    MOVE.L  A0,ARGADR
    MOVE.L  4.W,A6
    MOVE.W  20(A6),KICKVERS

    LEA UTILNAME,A1
    MOVEQ   #0,D0
    JSR -552(A6)
    MOVe.L  D0,UTILBASE

    LEA INITDOS,A1
    MOVEQ   #0,D0
    JSR -552(A6)        ; OPEN DOS
    MOVE.L  D0,DOSBASE
    BEQ LEAVEFAST

    MOVE.L  D0,A6
    JSR -60(A6)         ; GET STDIO
    MOVE.L  D0,STOUT
    TST.L   D0
    BEQ NOSTDOUT

;   here start with cl-arg-parse
    BSR   INITOTHER

.NUFF:
    CMP.W   #37,KICKVERS
    BPL READARGS

    MOVE.L  ARGLEN(PC),D0
    MOVE.L  ARGADR(PC),A0

    CLR.W   MAXREGALLOC
    MOVE.L  CODEPREFS(PC),D5
    CMP.B   #'-',(A0)
    BNE .1
    SUBQ.L  #1,D0
    ADDQ.L  #1,A0
    MOVEQ   #0,D2
.2: MOVE.B  (A0)+,D2
    SUBQ.L  #1,D0
    CMP.W   #33,D2
    BMI .1
    CMP.W   #127,D2
    BPL .1
    LEA .OPTS(PC),A2
.XL:MOVE.B  (A2)+,D3
    BEQ iERROR10
    MOVE.B  (A2)+,D4
    CMP.B   D2,D3
    BNE.S   .XL
    BSET    D4,D5
    CMP.B   #"m",D3
    BEQ.S   .M
    CMP.B   #"r",D3
    BEQ.S   .R
    BRA.S   .2
.M: MOVE.B  (A0)+,D2
    SUBQ.L  #1,D0
    CMP.B   #"9"+1,D2
    BPL iERROR10
    CMP.B   #"1",D2
    BMI iERROR10
    SUB.B   #"0",D2
    EXT.W   D2
    EXT.L   D2
    MULU    #400,D2         ; SEE READARGS
    LSL.L   #8,D2
    ADD.L   D2,BUFADD
    BRA.S   .2
.R: MOVE.B  (A0),D2
    CMP.B   #"0"+1+MAXREGVARS,D2    ; PUT 1..5 VARS IN REGS
    BPL.S   .NR
    CMP.B   #"1",D2
    BMI.S   .NR
    SUBQ.L  #1,D0
    ADDQ.L  #1,A0
    SUB.B   #"0",D2
    EXT.W   D2
.SR:    MOVE.W  D2,MAXREGALLOC
    BRA.S   .2
.NR:    MOVEQ   #MAXREGVARS,D2
    BRA.S   .SR
.OPTS:  DC.B    'a',3,'n',4,'w',6,'b',9,'m',10,'l',2,'s',13
    DC.B    'e',11,'E',5,'r',7,'q',15,'c',17,'h',18,'L',21,0
    EVEN
.1: MOVE.L  D5,CODEPREFS
    MOVE.L  #NAMEBUF,A1     ; COPY ARGS
    CMP.L   #2,D0
    BMI iERROR18         ; NOSTDOUT
    SUBQ.L  #1,D0
    MOVE.L  D0,ARGLEN
    MOVE.L  A0,ARGADR
    SUBQ.L  #1,D0
    MOVEQ   #0,D2
COPYARGS:
    MOVE.B  (A0)+,D2
    CMP.W   #33,D2
    BPL.S   .1
    SUBQ.L  #1,ARGLEN
    BRA.S   .2
.1: MOVE.B  D2,(A1)+
.2: DBRA    D0,COPYARGS

INITOTHER:
    MOVEM.L D0-A6,-(A7)
    LEA MIDAT(PC),A1
    MOVEQ   #0,d1
.1: MOVE.B  (A1),D0
    BEQ .2
    ADDQ.L  #1,D1
    eori.b  #$45,d0
    move.b  d0,(a1)+
    bra .1
.2: move.l  d1,MIDAT-4
    lea MIDAT2(PC),A1
    moveq   #0,d0
    moveq   #0,d2
    move.l  ARGADR,A0
.3: move.b  (a0)+,d0
    tst.b   d0
    beq     .4
    cmp.b   #10,d0
    beq     .4
    addq.l  #1,d2
    eori.b  #$45,d0
    cmp.b   (a1)+,d0
    bne     .5
    bra .3
.4:
    cmp.l   #16,d2
    bne .5
    MOve.l  #MIDAT,d2
    exg.l   d1,d3
    JSR WRITECON
    JSR CLEANUPALL
    MOVE.L  INITSTACK,A7
    rts
.5:
    MOVEM.L (A7)+,D0-A6
    RTS


    DC.L    0
MIDAT:
    DC.B    'Oeeeeeeeeeeeee!f',$15,'e!f',$15,'eeeOe',$4,$28,$2c,'"$ee'
    DC.B    'eeee!f',$15,'e!f',$15,'eeeeOeeeeeeeeeee!f',$15,'e!f'
    DC.B    $15,"eeeeeOeeóf'eóf'e!f",$15,'e!f',$15,"eeeeeeOeeeóf'"
    DC.B    "eóf'e",$15,'e!f',$15,"eeeeeeeOeeeeóf'eóf'e!f",$15,'eeeeeeeeO'
    DC.B    "eeeeeóf'eófff",$15,'e',$3,'*7 3 7eOeeeeeeóf',"'",'eóf',$15
    DC.B    "eeeeeeeeeeOeeeeeeeeeeeeee 77j')$÷eOO",0
        EVEN
MIDAT2:
    DC.L    $0c622865,$3c2a3037,$65082436,$31203764,0

; HERE ARGADR IS OK, AND ARGLEN=PRECISELEN, A1=ENDOFSTR
BACKRA:

    CMP.B   #"e",-1(A1)
    BEQ.S   .4
    CMP.B   #"E",-1(A1)
    BNE.S   .3
.4: CMP.B   #".",-2(A1)
    BNE.S   .3
    MOVE.L  ARGLEN(PC),D2
    SUBQ.L  #2,D2
    BMI.S   .3
    MOVE.L  D2,ARGLEN
    SUBQ.L  #2,A1

.3: MOVE.B  #0,(A1)+
    MOVE.W  #0,CONOUT
    BSR.W   SHOWIDENT
    BRA.S   GOTOUT
NOSTDOUT:             ; WE'VE GOT NO STDIO
        MOVEQ   #0,D0
        MOVE.L  D0,ARGLEN       ; SIMULATE NO ARGS
        SWAP    D0
        MOVE.B  D0,NAMEBUF
    BSR OPENCON         ; WE NEED A CON: AS STDIO
    BSR.S   SHOWIDENT
    MOVE.W  #-1,CONOUT
    BSET    #2,CODEPREFS+1
GOTOUT:               ; WE GOT STDIO, SO START PRINTING
    BSR TOFRONT         ; PUT WBTOFRONT
    BSR INSTALLHANDLER
    MOVE.W  CONOUT(PC),D0
    CMP.W   #0,D0
    BNE.S   ERNOARGS
    BSR OPENFILE
    RTS
ERNOARGS:
    RTS

SHOWIDENT:
    TST.W   .2
    BNE.S   .1
    BTST    #7,CODEPREFS+2
    BNE.S   .1
    MOVE.L  #TITLE,D2
    MOVE.L  #TITLEEND-TITLE,D3
    BSR WRITECON        ; SHOW OUR REAL IDENTITY
    MOVE.W  #-1,.2
.1: RTS
.2: DC.W    0

ISOPENMATH:
    TST.L   MATHBASE
    BNE.S   .1
    ;MOVEM.L    D0-D1/A0-A1,-(A7)
    LEA INITMATH,A1
    MOVEQ   #0,D0
    MOVE.L  4.W,A6
    JSR -552(A6)        ; OPEN MATH
    MOVE.L  D0,MATHBASE
    ;MOVEM.L    (A7)+,D0-D1/A0-A1
    TST.L   MATHBASE
    BEQ iERROR58
.1: RTS

READARGS:
    MOVE.L  DOSBASE,A6
    MOVE.L  #TEMPLATE,D1
    MOVE.L  #.SRC,D2
    MOVEQ   #0,D3
    JSR -$31E(A6)       ; READARGS
    MOVE.L  D0,D7           ; D7=RASTRUCT
    move.l  D0,DOS_RDA
    BEQ iERROR18

    MOVE.L  #NAMEBUF,A1     ; COPY ARGS
    MOVE.L  .SRC(PC),A0
    MOVE.L  A1,ARGADR
    MOVE.L  A1,D0
.COPY:  MOVE.B  (A0)+,(A1)+
    BNE.S   .COPY
    SUBQ.L  #1,A1
    MOVE.L  A1,D5           ; D5=END
    SUB.L   A1,D0
    NEG.L   D0
    MOVE.L  D0,ARGLEN

    MOVE.L  CODEPREFS,D6        ; D6=CODEPREFS
    MOVE.L  .REG(PC),D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.L  (A0),D0
    TST.L   D0
    BMI.S   .D
    CMP.L   #MAXREGVARS+1,D0
    BMI.S   .O
.D: MOVEQ   #MAXREGVARS,D0
.O: MOVE.W  D0,MAXREGALLOC
.1: TST.L   .LARGE
    BEQ.S   .2
    BSET    #2,D6
.2: TST.L   .SYM
    BEQ.S   .3
    BSET    #13,D6
.3: TST.L   .NOWRN
    BEQ.S   .4
    BSET    #4,D6
.4: TST.L   .QUIET
    BEQ.S   .5
    BSET    #15,D6
.5: TST.L   .ASM
    BEQ.S   .6
    BSET    #3,D6
.6: TST.L   .ERRL
    BEQ.S   .7
    BSET    #11,D6
.7: TST.L   .ERRB
    BEQ.S   .8
    BSET    #5,D6
.8: TST.L   .SBUF
    BEQ.S   .9
    BSET    #9,D6
.9: TST.L   .ICA
    BEQ.S   .10
    BSET    #17,D6
.10:    TST.L   .HOLD
    BEQ.S   .11
    BSET    #18,D6
.11:    TST.L   .WB
    BEQ.S   .11B
    BSET    #6,D6
.11B:   TST.L   .LINED
    BEQ.S   .11BB
    BSET    #21,D6
.11BB:  TST.L   .NIL
    BEQ.S   .11C
    BSET    #1,D6
.11C:   TST.L   .DBG
    BEQ.S   .11D
    BSET    #22,D6
    BSET    #21,D6
.11D:
    TST.L   .OPTI
    BEQ.S   .12
    MOVE.W  #MAXREGVARS,MAXREGALLOC
    BSET    #5,ICODEPREFS+3
.12:    MOVE.L  .ABUF(PC),D0
    BEQ.S   .13
    MOVE.L  D0,A0
    MOVE.L  (A0),D0
    CMP.L   #1,D0
    BMI.S   .13
    CMP.L   #1001,D0
    BPL.S   .13
    MULU    #400,D0         ; SEE PARSE "-"
    LSL.L   #8,D0
    ADD.L   D0,BUFADD
.13:
    MOVE.L  D6,CODEPREFS

    MOVE.L  D7,D1
;    JSR -$35A(A6)
    MOVE.L  D5,A1
    BRA BACKRA

.SRC:   DC.L    0
.REG:   DC.L    0
.LARGE: DC.L    0
.SYM:   DC.L    0
.NOWRN: DC.L    0
.QUIET: DC.L    0
.ASM:   DC.L    0
.ERRL:  DC.L    0
.ERRB:  DC.L    0
.SBUF:  DC.L    0
.ABUF:  DC.L    0
.ICA:   DC.L    0
.HOLD:  DC.L    0
.WB:    DC.L    0
.LINED: DC.L    0
.OPTI:  DC.L    0
.DBG:   DC.L    0
.NIL:   DC.L    0
MCRS:   DC.L    0


DOS_RDA:DC.L    0
DOARGMACROS:
    movem.l d0-a6,-(a7)
    move.l  MCRS,D0
    beq     .11EX
    move.l  d0,a0
    PREPTA  A6
.11LP1:
    move.l  (a0)+,d0
    beq     .11EX

;    movem.l d0-a6,-(a7)
;    move.l  d0,a0
;.xxx:
;    tst.b   (a0)+
;    bne     .xxx
;    move.l  a0,d3
;    sub.l   d0,d3
;    move.l  d0,d2
;    jsr     WRITECON
;    move.l  #CLINE,d2
;    moveq   #2,d3
;    jsr     WRITECON
;    movem.l (a7)+,d0-a6

    moveq   #0,d1
    move.l  d0,a1
    GETM    a5
    move.l  a5,a4
    moveq   #0,d0
    move.b  (a1)+,d0
    PREPID  a6,d0
    bmi     iERROR64
.11LP2:
    HASHC   D1,D0
    MOVE.B  D0,(A5)+
    MOVE.B  (A1)+,D0
    PREPIDR A6,D0
    BPL.S   .11LP2

    CLR.B   (A5)+
    MOVE.L  A5,D0
    BTST    #0,D0
    BEQ.S   .11E2
    CLR.B   (A5)+
.11E2:
    HASHE   D1
    LSL.L   #2,D1
    ADD.L   #MACROHASH,D1
    MOVE.L  D1,A6
    MOVE.L  (A6),(A5)       ; MAC_NEXT
    MOVE.L  A5,(A6)
    MOVE.L  A5,A6           ; A6=MACOBJ
    ADDQ.L  #4,A5
    MOVE.L  A4,(A5)+        ; MAC_NAME
    CLR.L   (A5)+           ; MAC_BODY
    CLR.L   (A5)+           ; MAC_NARGS + MAC_FLAGS
    DONEH   A5
    bra     .11LP1
.11EX:
    movem.l (a7)+,d0-a6
    rts

TEMPLATE:   DC.B    'SOURCE/A,REG=NUMREGALLOC/N/K,LARGE/S,'
            DC.B    'SYM=SYMBOLHUNK/S,NOWARN/S,'
            DC.B    'QUIET/S,ASM/S,ERRLINE/S,ERRBYTE/S,'
            DC.B    'SHOWBUF/S,ADDBUF/N/K,IGNORECACHE/S,'
            DC.B    'HOLD/S,WB/S,LINEDEBUG/S,OPTI/S,'
            DC.B    'DEBUG/S,NILCHECK/S,DEFINE/M',0
            EVEN
KICKVERS:   DC.W    0
ARGLEN:     DC.L    0   ; =0 IF NO ARGS
ARGADR:     DC.L    0
HANDLE:     DC.L    0
LINEWRITE:  DC.W    0
DOSBASE:    DC.L    0
UTILBASE:   DC.L    0
MATHBASE:   DC.L    0
KILLFLAG:   DC.W    0   ; IF -1 THEN KILL EXE
STOUT:      DC.L    0
CONOUT:     DC.W    0   ; FLAG: true->STDOUT=CON:,asmone
CDLOCK:     DC.L    0
GOTLOCK:    DC.W    0
RETERROR:   DC.W    -1
ASSLINE:    DC.W    0   ; LINENUM OF LAST A=1
ERARGS:     DC.L    0,0,0,0,0
RETURNOUT:  DC.B    10
ERTEXT:     DC.B    'ERROR: '
ERTEXTEND:
ERLINE:     DC.B    'LINE %ld: %s',10,0
EROBJLINE:  DC.B    'WITH: %s',10,0
      DC.B    '$VER: '
TITLE:  CREATIVE_VERSION
TITLEEND:   DC.B    0
      EVEN
;; split name
SPLITNAME:
    MOVE.L  #NAMEBUF,A0
    MOVE.L  A0,A1       ; A1=BEGBUF
    LOWER   A0,D0
.XL:CMPA.L  A0,A1
    BEQ.S   .D
    MOVEQ   #0,D0
    MOVE.B  -(A0),D0
    CMP.W   #":",D0
    BEQ.S   .DM
    CMP.W   #"/",D0
    BEQ.S   .DM
    BRA.S   .XL
.DM:    ADDQ.L  #1,A0       ; A0=MIDBUF
.D: MOVE.L  #SRCDIRBUF,A2   ; A2=DIRBUF
    MOVE.L  A2,A4
    MOVE.L  A1,A3
.L1:    CMPA.L  A3,A0
    BEQ.S   .E1
    MOVE.B  (A3)+,(A4)+
    BRA.S   .L1
.E1:    CLR.B   (A4)+
    MOVE.L  A0,A3
    MOVE.L  A1,A4
.L2:    MOVE.B  (A3)+,(A4)+
    BNE.S   .L2
    MOVE.L  ARGLEN(PC),D0
    ADD.L   A1,D0
    SUB.L   A0,D0
    MOVE.L  D0,ARGLEN
    TST.B   (A2)
    BEQ.S   .1
    MOVE.L  DOSBASE(PC),A6
    MOVE.L  A2,D1
    MOVEQ   #-2,D2
    JSR -$54(A6)    ; LOCK
    BEQ.S   .1
    MOVE.L  #FIB,A2
    MOVE.L  A2,D2
    MOVE.L  D0,D1
    MOVE.L  D0,D3
    JSR -$66(A6)
    TST.L   4(A2)       ; IS IT A DIR?
    BMI.S   .2
    MOVE.L  D3,D1
    JSR -$7E(A6)    ; CURRENTDIR
    MOVE.L  D0,CDLOCK
    MOVE.W  #-1,GOTLOCK
.1: RTS
.2: MOVE.L  D3,D1
    JSR -$5A(A6)    ; UNLOCK
    RTS
;*-*
;; close lock
CLOSELOCK:
    TST.W   GOTLOCK
    BEQ.S   .1
    MOVE.L  DOSBASE(PC),A6
    MOVE.L  CDLOCK(PC),D1
    JSR -$7E(A6)    ; CURRENTDIR
    MOVE.L  D0,D1
    JSR -$5A(A6)    ; UNLOCK
.1: RTS
;*-*
;; open file
OPENFILE:
    BSR SPLITNAME
    MOVE.W  #7,CURSPOT
    MOVE.L  #NAMEBUF,A2     ; FIX '.E'
    ADD.L   ARGLEN(PC),A2       ; A2=MIDARG
    MOVE.B  #'.',(A2)
    MOVE.B  #'e',1(A2)
    MOVE.B  #0,2(A2)
    MOVE.L  #NAMEBUF,D0
    BSR.W   FILELENGTH      ; GET FILELENGTH
    CMP.L   #1,D0
    BMI iERROR16
    MOVE.L  D0,D7           ; D7=SIZE (-4)
    ADDQ.L  #4,D0           ; FIX MEM
    BSR NEW
    TST.L   D0
    BEQ iERROR38
    MOVE.L  D0,D6           ; D6=ADR
    MOVE.L  DOSBASE(PC),A6      ; OPEN FILE
    MOVE.L  #NAMEBUF,D1
    MOVE.L  #1005,D2
    JSR -30(A6)
    MOVE.B  #0,(A2)
    MOVE.L  D0,HANDLE
    BEQ iERROR16
    MOVE.L  D6,D4           ; READ DATA
    MOVE.L  D7,D3
    MOVE.L  D4,D2
    MOVE.L  HANDLE(PC),D1
    MOVE.L  DOSBASE(PC),A6
    ADDQ.L  #1,D4
    JSR -42(A6)
    MOVE.L  D0,D4
    MOVE.L  DOSBASE(PC),A6      ; CLOSE IT
    MOVE.L  HANDLE(PC),D1
    JSR -36(A6)
    CMP.L   D4,D7
    BNE iERROR16
    ADD.L   D6,D4
    MOVE.L  D4,ENDECODE     ; SET POINTERS
    MOVE.L  D4,A0
    MOVE.B  #10,(A0)+
    MOVE.B  #10,(A0)+
    MOVE.L  D6,D4
    MOVE.L  D4,CURECODE
    MOVE.L  D4,ECODE
    ADDQ.L  #4,D7
    MOVE.L  D6,EBUF
    MOVE.L  D7,EBUFSIZE
    RTS
;*-*
;; file length
FILELENGTH:           ; FILENAME-->D0-->LEN
    MOVEM.L D1-D2/D4/D6/D7/A6,-(A7)
    MOVE.L  D0,D1
    MOVEQ   #-2,D2
    MOVE.L  DOSBASE(PC),A6
    JSR -84(A6)
    MOVE.L  D0,D7           ; LOCK
    BEQ.S   .1
    MOVE.L  D0,D1
    MOVE.L  #-260,D4        ; FRAME WITH BCPL ADDRESS CONV.
    MOVE.L  A7,D0
    BTST    #1,D0
    BEQ.S   .4
    SUBQ.L  #2,D4
.4: LEA 0(A7,D4.L),A7
    MOVE.L  A7,D2
    JSR -102(A6)        ; EXAMINE
    MOVE.L  124(A7),D6      ; GET LENGTH FROM FIB
    NEG.L   D4
    LEA 0(A7,D4.L),A7
    TST.L   D0
    BEQ.S   .2
    BSR .3
    MOVE.L  D6,D0
    BRA.S   .EX
.3: MOVE.L  D7,D1           ; UNLOCK
    JSR -90(A6)
    RTS
.2: BSR.S   .3
.1: MOVEQ   #-1,D0
.EX:    MOVEM.L (A7)+,D1-D2/D4/D6/D7/A6
    RTS
;*-*
;; to front
TOFRONT:
    BTST    #6,CODEPREFS+3      ; -w
    BEQ.S   .2
    LEA INITINTUI,A1
    MOVEQ   #0,D0
    MOVE.L  4.W,A6
    JSR -552(A6)
    TST.L   D0
    BEQ.S   .2
    MOVE.L  D0,A6
    JSR -342(A6)
    MOVE.L  A6,A1
    MOVE.L  4.W,A6
    JSR -414(A6)
.2: RTS
;*-*
;; write buffers
WRITEBUFFERS:
    BTST    #1,CODEPREFS+2
    BNE .C
    RTS
.C: LEA HEAP(PC),A0
    MOVE.L  8(A0),D0
    ADD.L   12(A0),D0
    SUB.L   (A0),D0
    MOVE.L  GENERALSIZE,D1
    MOVE.L  D1,12(A0)
    SUB.L   D0,D1
    ADD.L   8(A0),D1
    MOVE.L  D1,(A0)
    LEA BTAB(PC),A5
    MOVEQ   #NUMBUFS-1,D5
.XL:LEA .MES(PC),A0
    LEA ERARGS(PC),A1
    MOVE.L  (A5)+,A4
    MOVE.L  (A5)+,(A1)      ; bufname
    MOVE.L  (A4),D0
    SUB.L   8(A4),D0
    MOVE.L  D0,4(A1)        ; used
    MOVE.L  12(A4),D1
    MOVE.L  D1,12(A1)       ; maxbuf
    LSR.L   #8,D0
    LSR.L   #8,D1
    MULU    #100,D0
    DIVU    D1,D0
    AND.L   #$FFFF,D0
    ADDQ.L  #1,D0
    MOVE.L  D0,8(A1)        ; percentage
    MOVE.L  (A5)+,D0
    LSL.L   #2,D0
    MOVE.L  .T(PC,D0.L),16(A1)  ; MAX/DYN/EXP
    BSR WRITEFORMAT
    DBRA    D5,.XL
    RTS
.MES:   DC.B    '%s buffer used %ld (%ld%%) of %ld (%s).',10,0
    EVEN
.T: DC.L    .1,.2,.3
.1: DC.B    'fixed',0
.2: DC.B    'reallocatable',0
.3: DC.B    'expandable',0
    EVEN
;*-*
;; clear hash table
CLEARHASHTAB:
    CLR.L   ConstsList
    CLR.L   ConstTemp
    LEA     LIBHASH,A1
    MOVE.L  #255,D0
.1:
    CLR.L   (A1)+
    DBRA    D0,.1
    RTS
;*-*
;*-*

