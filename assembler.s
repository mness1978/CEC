;; Assembler part

; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The ASSEMBLER part !!!!!!!              ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;
;; Parse
ASM_PARSE:
    MOVE.L  #ASM_KWORDS,D2
    MOVE.L  A2,D0
    MOVE.L  (A2),D1
    AND.L   #$FFFFFF00,D1
    CMP.L   #'PC'*$10000,D1
    BEQ ASM_PC

;; Check for keywords
    MOVE.L  HASHVAL(PC),D7
    LSL.L   #2,D7
    ADD.L   #AKWHASH,D7
    MOVE.L  D7,A6
.LOOPZ:
    MOVE.L  (A6),A6
    MOVE.L  A6,D6
    BEQ     .SKIP_GO1
    MOVE.L  (A6)+,A4
    MOVE.L  D0,A2
.3Z:CMPM.B  (A4)+,(A2)+
    BNE     .LOOPZ
    TST.B   -1(A2)
    BNE     .3Z
    SUB.L   D2,D6
    LSR.L   #3,D6
    MOVEQ   #47,D4
    MOVE.W  D4,(A1)+
    MOVE.W  D6,(A1)+
    MOVEQ   #-1,D0
    RTS
;*-*
;; Check for commands
.SKIP_GO1:
    TST.W   GENERATE_PPC
    BNE     .2
.2BACK:
    MOVE.L  #ASM_INSTAB,D2
    MOVE.L  HASHVAL(PC),D7
    LSL.L   #2,D7
    ADD.L   #ASMHASH,D7
    MOVE.L  D7,A6
.LOOP:
    MOVE.L  (A6),A6         ; A6 _NOW_ POINTS TO ENTRY IN INSTABLE
    MOVE.L  A6,D6
    BNE     .33
    TST.W   GENERATE_PPC
    BNE     .FAIL
    BRA     .2
.33:
    MOVE.L  (A6)+,A4
    MOVE.L  D0,A2
.3: CMPM.B  (A4)+,(A2)+
    BNE.S   .LOOP
    TST.B   -1(A2)
    BNE.S   .3
    SUB.L   D2,D6
    LSR.L   #3,D6
    MOVE.L  D6,D0           ; D0=1..112
    ADD.L   #$100,D0
;*-*
;; Check for sizes
    TST.W   GENERATE_PPC
    BNE     iERROR98
    MOVEQ   #0,D1
    CMP.B   #'.',(A0)       ; SEE IF THERE'S A:     .B .W .L .S
    BNE.S   .4
    ADDQ.L  #1,A0
    MOVE.B  (A0)+,D2
    LEA     .SIZES(PC),A6
    MOVEQ   #7,D4
.CHKSIZE:
    ADDQ.L  #1,D1
    CMP.B   (A6)+,D2
    BEQ     .4
    DBF     D4,.CHKSIZE
    BRA     iERROR31
.4: MOVE.W  D0,D4
    MOVE.W  D0,(A1)+
    MOVE.W  D1,(A1)+
    RTS
.SIZES:
    DC.B    "BWLSXPDQ",0,0
;*-*
;; Check for ppc commands
.2:
    MOVE.L  #PPC_INSTAB,D2
    MOVE.L  HASHVAL(PC),D7
    LSL.L   #2,D7
    ADD.L   #PPCHASH,D7
    MOVE.L  D7,A6
.PLOOP:
    MOVE.L  (A6),A6         ; A6 _NOW_ POINTS TO ENTRY IN INSTABLE
    MOVE.L  A6,D6
    BNE     .P2
    TST.W   GENERATE_PPC
    BEQ     .FAIL
    BRA     .2BACK
.P2:
    MOVE.L  (A6)+,A4
    MOVE.L  D0,A2
.P3:CMPM.B  (A4)+,(A2)+
    BNE.S   .PLOOP
    TST.B   -1(A2)
    BNE.S   .P3
    SUB.L   D2,D6
    LSR.L   #3,D6
    MOVE.L  D6,D0
    MOVEQ   #48,D4
    MOVE.W  D4,(A1)+
    MOVE.L  D0,(A1)+
    CMP.B   #".",(A0)
    SEQ     D0
    EXT.W   D0
    MOVE.W  D0,(A1)+
    neg.w   d0
    add.w   d0,a0
    TST.W   GENERATE_PPC
    BEQ iERROR99
    MOVEQ   #-1,D0
    RTS
;*-*
;; Fail
.FAIL:
    MOVE.L  WORK(PC),A2
    MOVEQ   #0,D0           ; 0=FAIL, ELSE=ISASM
    RTS
;*-*
;*-*
;; PC
ASM_PC:
    CMP.W   #17,D4
    BNE .2
    CMP.B   #')',(A0)
    BEQ.S   .1
.2:
    MOVE.W  #$E00,D4
    MOVE.W  D4,(A1)+
    MOVEQ   #-1,D0
    RTS
.1: CMP.W   #17,D4          ;-(A1)
    BNE iERROR19
    SUBQ.L  #2,A1
    ADDQ.L  #1,A0
    SUBQ.W  #1,PARSEBRACKET
    MOVE.W  #$D00,D4
    MOVE.W  D4,(A1)+
    MOVEQ   #-1,D0
    RTS
;*-*
;; IMM
ASM_PARSEIMM:
    BTST    #7,CODEPREFS+1
    BEQ.S   .1
    MOVE.L  STARTINTERIM(PC),A6
    SUBQ.L  #2,A6
    CMPA.L  A6,A1
    BEQ PREP
.1: MOVE.W  #$F00,D4
    MOVE.W  D4,(A1)+        ; 11+4
    BRA PARSELOOP
;*-*
;; ParseReg
ASM_PARSEREG:
    CMP.B   #0,2(A2)
    BNE REGBACK
    MOVE.B  (A2),D1         ; D1="A" OR "D"
    CMP.B   #'D',D1
    BEQ.S   .1
    CMP.B   #'A',D1
    BNE REGBACK
.1: MOVEQ   #0,D0
    MOVE.B  1(A2),D0        ; D0="0" .. "9"
    SUB.W   #48,D0
    MOVE.L  D0,D7           ; =>COPY
    CMP.W   #8,D0
    BPL iERROR19
    CMP.B   #'A',D1
    BEQ .2
    MOVEQ   #0,D3           ; D3=ADR.MOD NR.
    BRA.S   .EXIT           ; --> Dx
.2: CMP.W   #17,D4          ;-2(A1)
    BEQ.S   .3
.6: BSET    #3,D0           ; --> Ax
    MOVEQ   #1,D3
    BRA.S   .EXIT
.3: CMP.B   #')',(A0)
    BNE.S   .6
    SUBQ.L  #2,A1
    ADDQ.L  #1,A0
    SUBQ.W  #1,PARSEBRACKET
    CMP.W   #8,-2(A1)
    BEQ.S   .4
.7: CMP.B   #'+',(A0)
    BEQ.S   .5
    BSET    #4,D0           ; --> (Ax)
    MOVEQ   #2,D3
    BRA.S   .EXIT
.4: CMP.L   #$10000,-6(A1)
    BEQ.S   .7
    SUBQ.L  #2,A1           ; --> -(Ax)
    MOVEQ   #4,D3
    BSET    #5,D0
    BRA.S   .EXIT
.5: ADDQ.L  #1,A0           ; --> (Ax)+
    MOVEQ   #3,D3
    ADD.W   #%11000,D0
    BRA .EXIT

.EXIT:  ASL.W   #8,D3
    ADD.W   #$400,D3
    CMP.W   #4,D7
    BEQ.S   .8
    CMP.W   #5,D7
    BEQ.S   .8
.9: ADD.W   D3,D0
    MOVE.W  D0,D4
    MOVE.W  D0,(A1)+
    MOVE.L  WORK(PC),A2
    BRA PARSELOOP
.8: CMP.W   #$400,D3
    BEQ.S   .9
    BSET    #0,WARNINGS+3       ; ISSUE A4/A5 USED WARNING
    BRA.S   .9
;*-*

;; Hash
MAKEASMHASH:
    MOVE.L  #ASMHASH,A0     ; A0=TABEL
    LEA ASM_INSTAB,A1
    MOVE.L  A0,A2
    MOVE.L  #255,D0
.CL:
    CLR.L   (A2)+           ; CLEAR TABLE
    DBRA    D0,.CL
.LOOP:
    MOVE.L  (A1),D0
    BEQ.S   EX_KWORDS
    MOVE.L  D0,A2
    HASH    A2,D0,D1
    LSL.L   #2,D0
    LEA 0(A0,D0.L),A2
    MOVE.L  (A2),D0         ; READ PREVIOUS LIST (OR NIL)
    MOVE.L  A1,(A2)         ; PUT ENTRY IN LIST
    MOVE.L  D0,4(A1)        ; AND LINK TAIL TO IT
    ADDQ.L  #8,A1
    BRA.S   .LOOP
EX_KWORDS:
    MOVE.L  #AKWHASH,A0     ; A0=TABEL
    LEA     ASM_KWORDS,A1
    MOVE.L  A0,A2
    MOVE.L  #255,D0
.CLX:
    CLR.L   (A2)+           ; CLEAR TABLE
    DBRA    D0,.CLX
.LOOPX:
    MOVE.L  (A1),D0
    BEQ.S   PPCASMHASH
    MOVE.L  D0,A2
    HASH    A2,D0,D1
    LSL.L   #2,D0
    LEA     0(A0,D0.L),A2
    MOVE.L  (A2),D0         ; READ PREVIOUS LIST (OR NIL)
    MOVE.L  A1,(A2)         ; PUT ENTRY IN LIST
    MOVE.L  D0,4(A1)        ; AND LINK TAIL TO IT
    ADDQ.L  #8,A1
    BRA.S   .LOOPX
PPCASMHASH:
    MOVE.L  #PPCHASH,A0     ; A0=TABEL
    LEA     PPC_INSTAB,A1
    MOVE.L  A0,A2
    MOVE.L  #255,D0
.CLX:
    CLR.L   (A2)+
    DBRA    D0,.CLX
.LOOPX:
    MOVE.L  (A1),D0
    BEQ.S   .EXX
    MOVE.L  D0,A2
    HASH    A2,D0,D1
    LSL.L   #2,D0
    LEA     0(A0,D0.L),A2
    MOVE.L  (A2),D0
    MOVE.L  A1,(A2)
    MOVE.L  D0,4(A1)
    ADDQ.L  #8,A1
    BRA.S   .LOOPX
.EXX:
    RTS
;*-*
;*-*
;;ASM

ASM_COMPILE:          ; GETS INSCODE IN D7
    MOVEM.L A2/A5,-(A7)     ; JUST A3-A4 STAY PRESERVED
    MOVEQ   #0,D0
    MOVE.W  D7,D0           ; D0=INSNR/ADDRESSMODENR
    cmp.w   #$0f00,d0
    beq     iERROR12
    MOVEQ   #0,D1
    SUB.W   #$100,D0
    MOVE.W  (A3)+,D1
    LEA ASM_INSJOB,A0
    LSL.L   #2,D0
    ADD.L   D0,A0
    MOVE.L  (A0),A0         ; A0=JOBTAB ENTRY
    MOVE.L  A4,A1           ; A1=ptr START INS IN A4
    MOVE.W  (A0)+,(A4)+
ASM_LOOP:
    MOVEQ   #0,D7
    MOVE.W  (A0)+,D7
    BEQ.S   .1
    SUBQ.L  #1,D7
    LSL.L   #2,D7
    MOVE.L  ASM_JOBROUTINES(PC,D7.L),A2
    JSR (A2)
    BRA.S   ASM_LOOP
.1: MOVEM.L (A7)+,A2/A5
    RTS

ASM_STOP:             ; EN JOBCODE PROCESSING
    LEA .1(PC),A0
    RTS
.1: DC.W    0

SKIP244:
    CMP.W   #44,(A3)
    BNE.S   *+4
    ADDQ.L  #4,A3
    RTS


XR:
    BSR SKIP244

;; a:REG
ASM_REGID:
    CMP.W   #IDENT,(A3)
    BNE.S   .1
    MOVE.L  A0,-(A7)
    MOVE.L  2(A3),A0
    BTST    #3,5(A0)
    BEQ.S   .2
    MOVE.L  D0,-(A7)
    MOVE.W  #$400,D0
    OR.W    10(A0),D0
    MOVE.W  D0,(A3)
    MOVE.L  #$2C002C,2(A3)
    MOVE.L  (A7)+,D0
.2: MOVE.L  (A7)+,A0
.1: RTS
;*-*

ASM_JOBROUTINES:
    DC.L    ASM_NOTIMPL,ASM_PUTSIZE,ASM_PUTSIZEMOVE,ASM_DOEA
    DC.L    ASM_DOEAMOVEDEST,ASM_COMMA,ASM_ADDSUB,ASM_MOVEQ,ASM_ASQ
    DC.L    ASM_BCC,ASM_DBCC,ASM_GETADRREG,ASM_SHIFT,ASM_DATAREG
    DC.L    ASM_BIT,ASM_SIZEA,ASM_SIZEE,ASM_XREG,ASM_DOEXG
    DC.L    ASM_CMPM,ASM_MOVEM,ASM_TRAP,ASM_MOVEP,ASM_CMPI
    DC.L    ASM_CPUSUP,ASM_SIZEFIXED,ASM_SIZE0,ASM_SIZEO
    DC.L    ASM_MONADIC,ASM_FPUSUP,ASM_46EMUL,ASM_DYADIC
    DC.L    ASM_MMUSUP,ASM_COPY,ASM_FMOVE,ASM_FMOVECR
    DC.L    ASM_FMOVEM,ASM_FSINCOS,ASM_TRAPCC,ASM_FTST
    DC.L    ASM_PMMSTD,ASM_PMMLDX,ASM_PMMTST,ASM_PMMMOVE,ASM_PMMEA
    DC.L    ASM_IMM3B,ASM_MOVEC,ASM_MOVES,ASM_BITFIELD1,ASM_BITFIELD2
    DC.L    ASM_BITFIELD3,ASM_MULDIV,ASM_LINK,ASM_CPCACHE040,ASM_MOVE16
    DC.L    ASM_PULSE,ASM_MOVE,ASM_ANDI,ASM_EORI,ASM_ORI

;; Not implemented      1
ASM_NOTIMPL:          ; 1
    BRA iERROR87      ; TEMP
;*-*
;; Put size             2
ASM_PUTSIZE:          ; 2
    TST.L   D1
    BNE     .EXISTS
    MOVE.W  (A0),D1
.EXISTS:
    ADDQ.L  #2,A0
    MOVE.W  (A0)+,D2
    MOVE.L  D1,D0
    SUBQ.L  #1,D0
    BTST    D0,D2
    BEQ     iERROR79         ; 1=B,2=W,3=L...

    MOVE.W  (A0)+,D2        ; FIND OFFSET
    LSL.L   D2,D0
    OR.W    D0,(A1)         ; PUT IN INS.WORD
    RTS
;*-*
;; Put size (move)      3
ASM_PUTSIZEMOVE:          ; 3
    TST.L   D1
    BNE     .EXISTS
    MOVE.W  (A0),D1
.EXISTS:
    MOVE.L  D1,D0
    sUBQ.L  #1,D0
    ADDQ.L  #2,A0
    MOVE.W  (A0)+,D2
    btst    d0,d2
    beq     iERROR79

    MOVE.L  D1,D0
    CMP.W   #1,D0
    BEQ.S   .1
    BCHG    #0,D0
.1: MOVE.W  (A0)+,D2        ; FIND OFFSET
    LSL.L   D2,D0
    OR.W    D0,(A1)         ; PUT IN INS.WORD
    RTS
;*-*
;; EA                   4
ASM_DOEA:             ; 4
    BSR.W   ASM_GETEA
    MOVE.W  (A0)+,D2
    BTST    D0,D2
    BEQ iERROR19
    MOVE.W  (A0)+,D2
    TST.W   D2
    BMI.S   .1          ; OFFS=-1 --> NO OR.W EA
    LSL.W   D2,D3
    OR.W    D3,(A1)
.1: RTS
;*-*
;; EA (move dest)       5
ASM_DOEAMOVEDEST:         ; 5  MASK IN D3, # IN D0
    BSR.W   ASM_GETEA
    MOVE.W  (A0)+,D2
    BTST    D0,D2
    BEQ iERROR19
    MOVE.L  D3,D0
    LSR.W   #3,D3           ; UPPER BITS
    AND.W   #%111,D0        ; LOWER
    LSL.W   #3,D0
    ADD.W   D3,D0           ; SWAPPED REGISTER AND MODE FIELDS
    MOVE.W  (A0)+,D2
    LSL.W   D2,D0
    OR.W    D0,(A1)
    RTS
;*-*
;; Comma                6
ASM_COMMA:            ; 6
    CMP.W   #COM,(A3)+
    BNE iERROR5
    RTS
;*-*
;; Add/Sub              7
ASM_ADDSUB:           ; 7
    BSR ASM_REGID
    MOVE.B  (A3),D2
    CMP.B   #4,D2
    BEQ.S   .1
    CMP.B   #15,D2
    BEQ.S   .3
.2: BSR.W   ASM_GETEA       ; EA --> Dx
    BSR SKIP244
    OR.W    D3,(A1)
    BSR.S   ASM_COMMA
    CMP.B   #5,(A3)
    BEQ.S   .4
    MOVEQ   #9,D0
    BSR.W   ASM_GETDATAREG
    ADDQ.L  #2,A0
    RTS
.1: CMP.W   #44,2(A3)
    BEQ.W   .S
    ADDQ.L  #4,A3
    BSR ASM_REGID
    MOVE.B  (A3),D0
    SUBQ.L  #4,A3
    CMP.B   #4,D0
    BEQ.S   .2
    CMP.B   #5,D0
    BEQ.S   .2
.SB:    MOVEQ   #9,D0
    BSR.W   ASM_GETDATAREG      ; Dx --> EA
    BSR.S   ASM_COMMA
    MOVEQ   #0,D7
    BSR.W   ASM_ANYEA
    CMP.W   #8,D3           ; FROM DX TO DX, ALWAYS DEST.
    BMI.S   .DXDX
    BSET    #0,(A1)
.DXDX:  ADDQ.L  #2,A0
    RTS
.3: BSR ASM_GETEA       ; #x --> EA
    BSR ASM_COMMA
    CMP.B   #5,(A3)
    BEQ.S   .8
    BSR ASM_GETEA
    OR.W    D3,(A1)
    MOVE.B  (A0)+,(A1)
    ADDQ.L  #1,A0
    MOVE.W  #DEST,D2
    BTST    D0,D2
    BEQ iERROR19
    RTS
.4: MOVE.B  (A1),D6         ; EA --> Ax
    CMP.B   #13*16,D6
    BEQ.S   .5
    CMP.B   #9*16,D6
    BNE iERROR19
.5: MOVE.W  (A3)+,D6
    AND.W   #7,D6
    LSL.W   #1,D6
    OR.B    D6,(A1)
    MOVE.W  (A1),D6
    AND.W   #%11000000,D6
    TST.W   D6
    BEQ iERROR31
    BTST    #7,D6
    BEQ.S   .7
    BSET    #0,(A1)
.7: OR.B    #%11000000,1(A1)
    ADDQ.L  #2,A0
    RTS
.8: OR.W    #%111100,(A1)
    BRA.S   .4
.S: ADDQ.L  #8,A3
    BSR ASM_REGID
    MOVE.B  (A3),D0
    SUBQ.L  #8,A3
    CMP.B   #4,D0
    BEQ.W   .2
    CMP.B   #5,D0
    BEQ.W   .2
    BRA.W   .SB
;*-*
;; Moveq                8
ASM_MOVEQ:            ; 8
    CMP.W   #$0F00,(A3)+
    BNE iERROR30

    MOVEM.L A0/D1,-(A7)
    CMP.W   #LIBC,(A3)
    BNE .1_1
    ADDQ.L  #4,A3
    MOVE.W  (A3)+,D1
    EXT.L   D1
    ADDQ.L  #8,A3
    MOVEQ   #0,D0
    BRA .1_2
.1_1:
    BSR ASM_GRABVALUE
.1_2:
    TST.L   D0
    BMI iERROR30
    MOVE.L  D1,D0
    MOVEM.L (A7)+,A0/D1

    CMP.L   #128,D0
    BPL iERROR31
    CMP.L   #-127,D0
    BMI iERROR31
    MOVE.B  D0,1(A1)
    BSR.W   ASM_COMMA
    MOVEQ   #9,D0
    BSR     ASM_GETDATAREG
    RTS
;*-*
;; Addq/Subq            9
ASM_ASQ:              ; 9
    CMP.W   #$0F00,(A3)+
    BNE iERROR30

    MOVEM.L A0/D1,-(A7)
    BSR ASM_GRABVALUE
    TST.L   D0
    BEQ iERROR30
    MOVE.L  D1,D2
    MOVEM.L (A7)+,A0/D1

    TST.L   D2
    BEQ iERROR31
    CMP.L   #8,D2
    BNE.S   .1
    MOVEQ   #0,D2
.1: MOVE.L  D2,D3
    AND.L   #$FFFFFFF8,D3
    TST.L   D3
    BNE iERROR31
    AND.W   #%111,D2
    MOVEQ   #9,D3
    LSL.W   D3,D2
    ADD.W   D2,(A1)
    RTS
;*-*
;; Bcc                  10
ASM_BCC:              ; 10
    CMP.W   #IDENT,(A3)+
    ADDQ.L  #2,A0
    BNE iERROR4
    MOVE.L  (A3)+,A2
    CMP.B   #LAB,4(A2)
    BNE iERROR4
    BTST    #4,5(A2)        ; CHECK IF METHOD
    BNE iERROR4

    CMP.W   #3,D1
    BNE     .X0
    MOVE.W  -2(A0),(A1)     ; Bcc.L....
    MOVEM.L D1/A0,-(A7)
    MOVE.W  10(A2),D0
    BSR     ADDBRANCHPCREL32
    MOVEM.L (A7)+,D1/A0
    RTS

.X0:CMP.W   #4,D1
    BEQ.S   .X1
    CMP.W   #1,D1           ; A Bcc.S/Bcc.B ?
    BNE.S   .1
.X1:MOVE.B  #5,1(A1)
    BRA.S   .2
.1: MOVE.W  #0,(A4)+
.2: MOVE.W  10(A2),D0
    MOVEM.L D1/A0,-(A7)
    CLR.W   NEWOP
    BSR ADDBRANCH
    MOVEM.L (A7)+,D1/A0
    RTS

;*-*
;; DBcc                 11
ASM_DBCC:             ; 11
    BSR ASM_REGID
    CMP.B   #4,(A3)+
    BNE iERROR19
    MOVE.B  (A3)+,D0
    OR.B    D0,1(A1)
    BSR SKIP244
    BSR.W   ASM_COMMA
    CMP.W   #IDENT,(A3)+
    BNE iERROR4
    MOVE.L  (A3)+,A2
    CMP.B   #LAB,4(A2)
    BNE iERROR4
    BTST    #4,5(A2)        ; CHECK IF METHOD
    BNE iERROR4
    MOVE.W  #0,(A4)+
    MOVE.W  10(A2),D0
    MOVEM.L D1/A0,-(A7)
    CLR.W   NEWOP
    BSR ADDBRANCH
    MOVEM.L (A7)+,D1/A0
    RTS
;*-*
;; Address reg          12
ASM_GETADRREG:            ; 12
    MOVE.W  (A0)+,D0
    CMP.B   #5,(A3)+
    BNE iERROR19
    MOVE.B  (A3)+,D2
    AND.W   #%111,D2
    LSL.W   D0,D2
    ADD.W   D2,(A1)
    RTS
;*-*
;; Shift                13
ASM_SHIFT:            ; 13
    BSR ASM_REGID
    MOVE.B  (A3),D2
    CMP.B   #4,D2
    BEQ.S   .2
    CMP.B   #15,D2
    BNE.W   .1
    ADDQ.L  #2,A3           ; IMM,REG
    CMP.W   #VALUE,(A3)+
    BNE iERROR30
    MOVE.L  (A3)+,D4
    BEQ iERROR31
    CMP.L   #8,D4
    BNE.S   .4
    MOVEQ   #0,D4
.4: CMP.L   #8,D4
    BPL iERROR31
    TST.L   D4
    BMI iERROR31
    BSR ASM_COMMA
    BSR ASM_REGID
    CMP.B   #4,(A3)
    BNE.S   .5
    ADDQ.L  #1,A3
    MOVE.B  (A3)+,D5
    BSR SKIP244
    LSL.W   #1,D4
    OR.B    D4,(A1)
    OR.B    D5,1(A1)
    SUBQ.W  #1,D1
    LSL.W   #6,D1
    OR.W    D1,(A1)
    ADDQ.L  #2,A0
    RTS
.2: BSET    #5,1(A1)        ; REG,REG
    ADDQ.L  #1,A3
    MOVE.B  (A3)+,D3
    LSL.B   #1,D3
    OR.B    D3,(A1)
    BSR SKIP244
    BSR ASM_COMMA
    BSR ASM_REGID
    CMP.B   #4,(A3)+
    BNE iERROR19
    MOVE.B  (A3)+,D3
    BSR SKIP244
    OR.B    D3,1(A1)
    SUBQ.W  #1,D1
    LSL.W   #6,D1
    OR.W    D1,(A1)
    ADDQ.L  #2,A0
    RTS
.5: CMP.W   #1,D4
    BNE iERROR31
.1: CMP.W   #2,D1           ; SHIFT IN MEM NEEDS .W
    BNE iERROR31
    BSR.W   ASM_GETEA
    MOVE.W  #MDEST,D2       ; SHIFT IN MEMORY
    BTST    D0,D2
    BEQ iERROR19
    MOVE.W  (A0)+,(A1)      ; NEW OPCODE
    OR.W    D3,(A1)         ; PUT IN EA
    RTS
;*-*
;; Data reg             14
ASM_DATAREG:          ; 14
    MOVE.W  (A0)+,D0
    BSR ASM_REGID
    CMP.B   #4,(A3)+
    BNE iERROR19
    MOVEQ   #0,D2
    MOVE.B  (A3)+,D2
    LSL.W   D0,D2
    ADD.W   D2,(A1)
    BSR SKIP244
    RTS
;*-*
;; Bit                  15
ASM_BIT:              ; 15
    CMP.B   #$F,(A3)
    BNE.S   .1
    ADDQ.L  #2,A3           ; #IMM,EA
    CMP.W   #VALUE,(A3)+
    BNE iERROR30
    MOVE.L  (A3)+,D2
    CHKB    D2,D3,iERROR31
    MOVE.W  D2,(A4)+
    MOVE.W  (A0)+,(A1)      ; NEW OPCODE
    BSR ASM_COMMA
    BSR.W   ASM_GETEA
    MOVE.W  #BTDEST,D2
    BTST    D0,D2
    BEQ iERROR19
    OR.W    D3,(A1)
    RTS
.1: BSR ASM_REGID
    CMP.B   #4,(A3)+        ; Dx,EA
    BNE iERROR19
    MOVE.B  (A3)+,D2
    LSL.B   #1,D2
    OR.B    D2,(A1)
    BSR SKIP244
    BSR ASM_COMMA
    BSR.W   ASM_GETEA
    MOVE.W  #BTDEST,D2
    BTST    D0,D2
    BEQ iERROR19
    OR.W    D3,(A1)
    ADDQ.L  #2,A0
    RTS
;*-*
;; SizeA                16
ASM_SIZEA:            ; 16
    TST.W   D1
    bNE     .EXISTS
    MOVE.W  (A0),D1
.EXISTS:
    ADDQ.L  #2,A0
    MOVE.L  D1,D0
    SUBQ.L  #1,D0
    MOVE.W  (A0)+,D2
    BTST    D0,D2
    BEQ     iERROR79

    CMP.W   #1,D1
    BEQ iERROR79
    CMP.W   #3,D1
    BEQ.S   .1
    RTS
.1: BSET    #0,(A1)
    RTS
;*-*
;; SizeE                17
ASM_SIZEE:            ; 17
    TST.W   D1
    bNE     .EXISTS
    MOVE.W  (A0),D1
.EXISTS:
    ADDQ.L  #2,A0
    MOVE.L  D1,D0
    SUBQ.L  #1,D0
    MOVE.W  (A0)+,D2
    BTST    D0,D2
    BEQ     iERROR79

    CMP.W   #1,D1
    BEQ iERROR79
    CMP.W   #3,D1
    BEQ.S   .1
    RTS
.1: BSET    #6,1(A1)
    RTS
;*-*
;; Xreg                 18
ASM_XREG:             ; 18
    BSR ASM_REGID
    MOVE.B  (A3)+,D2
    MOVE.B  (A3)+,D3
    BSR SKIP244
    BSR ASM_COMMA
    BSR ASM_REGID
    MOVE.B  (A3)+,D4
    MOVE.B  (A3)+,D5
    BSR SKIP244
    CMP.B   D2,D4
    BNE iERROR19
    CMP.B   #4,D2
    BEQ.S   .1
    CMP.B   #8,D2
    BNE iERROR19
    BSET    #3,1(A1)
.1: AND.B   #%111,D3
    AND.B   #%111,D5
    OR.B    D3,1(A1)
    LSL.B   #1,D5
    OR.B    D5,(A1)
    RTS
;*-*
;; Exg                  19
ASM_DOEXG:            ; 19
    BSR ASM_REGID
    MOVE.B  (A3)+,D2
    MOVE.B  (A3)+,D3
    CMP.B   #4,D2
    BNE.S   .1
    BSR SKIP244

    LSL.B   #1,D3
    OR.B    D3,(A1)

    BSR ASM_COMMA
    BSR ASM_REGID
    MOVE.B  (A3)+,D2
    MOVE.B  (A3)+,D3
    AND.B   #%111,D3

    OR.B    D3,1(A1)

    CMP.B   #4,D2
    BNE.S   .2
    BSR SKIP244
    BSET    #6,1(A1)        ; d,d
    RTS
.2: BSET    #7,1(A1)        ; d,a
    BSET    #3,1(A1)
    RTS
.1: CMP.B   #5,D2
    BNE iERROR19
    AND.B   #%111,D3

    OR.B    D3,1(A1)

    BSR ASM_COMMA
    BSR ASM_REGID
    MOVE.B  (A3)+,D2
    MOVE.B  (A3)+,D3
    AND.B   #%111,D3

    LSL.B   #1,D3
    OR.B    D3,(A1)

    CMP.B   #4,D2
    BNE.S   .3
    BSR SKIP244
    BSET    #7,1(A1)        ; d,a
    BSET    #3,1(A1)
    RTS
.3: BSET    #6,1(A1)        ; a,a
    BSET    #3,1(A1)
    RTS
;*-*
;; Cmpm                 20
ASM_CMPM:             ; 20
    CMP.B   #7,(A3)+
    BNE iERROR19
    MOVE.B  (A3)+,D2
    BSR ASM_COMMA
    CMP.B   #7,(A3)+
    BNE iERROR19
    AND.W   #$7,D2
    OR.W    D2,(A1)
    MOVE.B  (A3)+,D2
    AND.W   #$7,D2
    LSL.B   #1,D2
    OR.B    D2,(A1)
    RTS
;*-*
;; Movem                21
ASM_MOVEM:            ; 21
    BSR ASM_REGID
    MOVE.B  (A3),D0
    BEQ.S   .1B
    CMPI.B  #6,D0
    BMI.S   .1
.1B:    ADDQ.L  #2,A4
    BSR.W   ASM_GETEA       ; (Ax)+,REGLIST
    MOVE.W  #MMSRC,D2
    BTST    D0,D2
    BEQ iERROR19
    OR.W    D3,(A1)
    BSET    #2,(A1)
    BSR ASM_COMMA
    BSR ASM_REGID
    BSR.S   .2
    MOVE.W  D2,2(A1)
    RTS
.1: BSR.S   .2
    MOVE.W  D2,(A4)+
    MOVE.W  D7,-(A7)
    BSR ASM_COMMA
    BSR.W   ASM_GETEA       ; REGLIST,-(Ax)
    MOVE.W  (A7)+,D7
    CMP.W   #4,D0
    BNE.S   .10
    MOVE.W  D7,2(A1)
.10:    MOVE.W  #MMDEST,D2
    BTST    D0,D2
    BEQ iERROR19
    OR.W    D3,(A1)
    RTS
.2: MOVEQ   #0,D2           ; MASK
    MOVEQ   #0,D7
    MOVE.B  (A3)+,D0        ; GET REGLIST
    BEQ iERROR0
.6: MOVEQ   #4,D6
    CMP.B   #4,D0
    BEQ.S   .4
    CMP.B   #5,D0
    BEQ.S   .5
    BRA iERROR19
.X: SUBQ.L  #2,A3
    RTS
.4: MOVEQ   #0,D4
    MOVE.B  (A3)+,D4        ; Dx
    BSR SKIP244
    BSET    D4,D2
    MOVEQ   #15,D3
    SUB.L   D4,D3
    BSET    D3,D7
.9: MOVE.W  (A3)+,D5
    CMP.W   #10,D5
    BEQ.S   .7
    CMP.W   #8,D5
    BNE.S   .X
    BSR ASM_REGID
    CMP.B   (A3)+,D6
    BNE iERROR19
    MOVEQ   #0,D5
    MOVE.B  (A3)+,D5
    BSR SKIP244
.8: ADDQ.L  #1,D4
    BSET    D4,D2
    MOVEQ   #15,D3
    SUB.L   D4,D3
    BSET    D3,D7
    CMP.L   D5,D4
    BMI.S   .8
    BEQ.S   .9
    BRA iERROR19
.5: MOVEQ   #5,D6           ; Ax
    BRA.S   .4
.7: BSR ASM_REGID
    MOVE.B  (A3)+,D0
    BRA.S   .6
;*-*
;; Trap                 22
ASM_TRAP:             ; 22
    CMP.L   #$0F000001,(A3)+
    BNE iERROR19
    MOVE.L  (A3)+,D2
    MOVE.L  D2,D3
    AND.L   #$FFFFFFF0,D3
    TST.L   D3
    BNE iERROR31
    OR.W    D2,(A1)
    RTS
;*-*
;; Movep                23
ASM_MOVEP:            ; 23
    CMP.B   #4,(A3)
    BNE.S   .1
    ADDQ.L  #1,A3
    BSET    #7,1(A1)        ; Dx,x(Ax)
    MOVE.B  (A3)+,D2
    LSL.B   #1,D2
    OR.B    D2,(A1)
    BSR ASM_COMMA
    BSR     ASM_GETEA
    CMP.W   #5,D0
    BNE iERROR19
    AND.W   #7,D3
    OR.W    D3,(A1)
    RTS
.1: BSR     ASM_GETEA       ; x(Ax),Dx
    CMP.W   #5,D0
    BNE iERROR19
    AND.W   #7,D3
    OR.W    D3,(A1)
    BSR ASM_COMMA
    CMP.B   #4,(A3)+
    BNE iERROR19
    MOVE.B  (A3)+,D2
    LSL.B   #1,D2
    OR.B    D2,(A1)
    RTS
;*-*
;; Cmpi                 24
ASM_CMPI:             ; 24
    CMP.W   #$0F00,(A3)
    BEQ .1
    RTS
.1: MOVE.B  #$C,(A1)        ; SET TO CMPI
    BSR     ASM_GETEA
    CMP.W   #11,D0
    BNE iERROR19
    BSR ASM_COMMA
    BSR ASM_GETEA
    MOVE.W  #DEST,D2
    BTST    D0,D2
    BEQ iERROR19
    OR.W    D3,(A1)
    BSR ASM_STOP
    RTS
;*-*
;; CpuSup               25
ASM_CPUSUP:           ; 25
    MOVE.W  ASMCPU(PC),D0
    AND.W   (A0)+,D0
    BEQ iERROR53
    RTS
;*-*
;; SizeFixed            26
ASM_SIZEFIXED:
    CMP.W   (A0)+,D1
    BNE iERROR31
    RTS
;*-*
;; Size0                27
ASM_SIZE0:
    TST.W   D1
    BNE     iERROR79
    RTS
;*-*
;; SizeO                28
ASM_SIZEO:
    TST.W   D1
    BNE     .EXISTS
    MOVE.W  (A0),D1
.EXISTS:
    ADDQ.L  #2,A0
    MOVe.W  (A0)+,D2
    MOVE.L  D1,D0
    SUBQ.L  #1,D0
    BTST    D0,D2
    BEQ     iERROR79
    RTS
;*-*
;; Monadic              29
ASM_MONADIC:
    MOVE.W  (A0)+,(A4)+
    CMP.W   #47,(A3)
    BNE     .EA
    CMP.W   #5,D1
    BNE     iERROR79
    BSR     ASM_GETFPREG
    MOVE.W  D2,D3
    CMP.W   #COM,(A3)
    BNE     .FP
    BSR     ASM_COMMA
    BSR     ASM_GETFPREG
.FP:LSL.W   #8,D3
    LSL.W   #2,D3
    LSL.W   #7,D2
    OR.W    D2,2(A1)
    OR.W    D3,2(A1)
    RTS
.EA:
    BSR     ASM_GETEA
    MOVE.L  #NASRC,D2
    BTST    D0,D2
    BEQ     iERROR19
    TST.W   D0
    BNE     .EA2
    CMp.W   #5,D1
    BPL     iERROR79
.EA2:
    TST.W   (A0)+
    BNE     .EA3
    CMP.W   #6,D1
    BEQ     iERROR79
.EA3:
    OR.W    D3,(A1)
    MOVE.L  D1,D2
    CMp.W   #8,D1
    BEQ     iERROR79
    LSL.W   #1,D2
    MOVE.W  .SIZES(PC,D2),D2
    OR.W    D2,2(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETFPREG
    LSL.W   #7,D2
    OR.W    D2,2(A1)
    RTS
.SIZES:
    DC.W    0,$5800,$5000,$4000,$4400,$4800,$4c00,$5400,$0000
;*-*
;; FpuSup               30
ASM_FPUSUP:
    MOVE.W  (A0)+,D2
    MOVE.W  EFPU,D0
    AND.W   D0,D2
    BEQ     iERROR53
    RTS
;*-*
;; 040/060 emulated     31
ASM_46EMUL:
    CMP.W   #2,EFPU
    BNE     .X
    BSET    #7,WARNINGS+3
.X: RTS
;*-*
;; Dyadic               32
ASM_DYADIC:
    MOVE.W  (A0)+,(A4)+
    CMP.W   #47,(A3)
    BNE     .EA
    CMP.W   #5,D1
    BNE     iERROR79
    BSR     ASM_GETFPREG
    MOVE.W  D2,D3
    BSR     ASM_COMMA
    BSR     ASM_GETFPREG
.FP:LSL.W   #8,D3
    LSL.W   #2,D3
    LSL.W   #7,D2
    OR.W    D2,2(A1)
    OR.W    D3,2(A1)
    RTS
.EA:
    BSR     ASM_GETEA
    MOVE.L  #NASRC,D2
    BTST    D0,D2
    BEQ     iERROR19
    TST.W   D0
    BNE     .EA2
    CMp.W   #5,D1
    BPL     iERROR79
.EA2:
    TST.W   (A0)+
    BNE     .EA3
    CMP.W   #6,D1
    BEQ     iERROR79
.EA3:
    OR.W    D3,(A1)
    MOVE.L  D1,D2
    CMp.W   #8,D1
    BEQ     iERROR79
    LSL.W   #1,D2
    MOVE.W  .SIZES(PC,D2),D2
    OR.W    D2,2(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETFPREG
    LSL.W   #7,D2
    OR.W    D2,2(A1)
    RTS
.SIZES:
    DC.W    0,$5800,$5000,$4000,$4400,$4800,$4c00,$5400,$0000
;*-*
;; MmuSup               33
ASM_MMUSUP:
    MOVE.W  (A0)+,D2
    MOVE.W  EMMU,D0
    AND.W   D0,D2
    BEQ     iERROR53
    RTS
;*-*
;; Copy                 34
ASM_COPY:
    MOVE.W  (A0)+,(A4)+
    RTS
;*-*
;; Fmove                35
ASM_FMOVE:
    CMP.W   #47,(A3)
    BNE     .EA
    CMP.W   #$24,2(A3)
    BPL     .FPCR1
    BSR     ASM_GETFPREG
    MOVE.W  D2,D7
    BSR     ASM_COMMA
    CMP.W   #47,(A3)
    BNE     .EAX
    CMP.W   #5,D1

    BNE     iERROR79         ; FP to FP
    BSR     ASM_GETFPREG
    LSL.W   #8,D7
    LSL.W   #2,D7
    LSL.W   #7,D2
    OR.W    D2,2(A1)
    OR.W    D7,2(A1)
    RTS
.EAX:
    BSR     ASM_GETEA
    MOVE.L  #DEST,D2
    BTST    D0,D2
    BEQ     iERROR19
    TST.W   D0
    BNE     .EAX1
    CMP.W   #5,D1
    BPL     iERROR79
.EAX1:
    OR.W    D3,(A1)
    LSL.W   #7,D7
    OR.W    D7,2(A1)
    BSR     .ADJUSTSIZE
    OR.W    #$2000,2(A1)
    CMP.W   #6,D1
    BNE     .X
    CMP.W   #23,(a3)
    BEQ     .EXT
.X: RTS
.EXT:
    ADDQ.L  #2,A3
    CMP.B   #4,(A3)
    BEQ     .EXTD
    CMp.L   #$0f000001,(a3)+
    bne     iERROR19
    move.l  (a3)+,d2
    bmi     iERROR31
    cmp.l   #64,d2
    bpl     iERROR31
    OR.W    D2,2(A1)
    CMP.W   #24,(A3)+
    BNE     iERROR29
    RTS
.EXTD:
    OR.W    #$1000,2(A1)
    MOve.W  (a3)+,d2
    and.l   #7,d2
    lsl.w   #4,d2
    or.w    d2,2(A1)
    cmp.w   #24,(a3)+
    bne     iERROR29
    RTS
.FPCR1:
    CMP.W   #3,D1
    BNE     iERROR79
    ADDq.L  #2,A3
    MOVE.W  (A3)+,D2
    cmp.w   #$27,d2
    bpl     iERROR80
    sub.w   #$24,d2
    beq     .F11
    cmp.w   #1,d2
    beq     .F12
    move.w  #$A400,D2
    BRA     .F13
.F11:
    move.w  #$a800,d2
    bra     .F13
.F12:
    MOVe.W  #$B000,d2
.F13:
    move.w  d2,2(a1)
    bsr     ASM_COMMA
    BSR     ASM_GETEA
    MOVe.W  #DEST,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    RTS

.EA:
    BSR     ASM_GETEA
    MOVE.L  #NASRC,D2
    BTST    D0,D2
    BEQ     iERROR19
    TST.W   D0
    BNE     .EA2
    CMp.W   #5,D1
    BPL     iERROR79
.EA2:
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    CMP.W   #$24,2(A3)
    BPL     .FPCR2
    BSR     ASM_GETFPREG
    LSL.W   #7,D2
    OR.W    D2,2(A1)
    BSR     .ADJUSTSIZE
    RTS

.FPCR2:
    CMP.W   #3,D1
    BNE     iERROR79
    CMP.W   #47,(a3)+
    BNE     iERROR80
    MOVE.W  (A3)+,D2
    cmp.w   #$27,d2
    bpl     iERROR80
    sub.w   #$24,d2
    beq     .F21
    cmp.w   #1,d2
    beq     .F22
    move.w  #$8400,D2
    BRA     .F23
.F21:
    move.w  #$8800,d2
    bra     .F23
.F22:
    MOVe.W  #$9000,d2
.F23:
    move.w  d2,2(a1)
    OR.W    D3,(A1)
    RTS
.ADJUSTSIZE:
    MOVE.L  D1,D2
    LSL.W   #1,D2
    MOVE.W  .SIZES(PC,D2),D2
    OR.W    D2,2(A1)
    RTS
.SIZES:
    DC.W    0,$5800,$5000,$4000,$4400,$4800,$4c00,$5400,$0000
;*-*
;; Fmovecr              36
ASM_FMOVECR:
    CMP.L   #$0f000001,(a3)+
    BNE     iERROR30
    MOVE.L  (a3)+,D2
    BMI     iERROR31
    CMP.L   #$7F,D2
    BGT     iERROR31
    OR.W    D2,2(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETFPREG
    LSL.W   #7,D2
    OR.W    D2,2(A1)
    RTS
;*-*
;; Fmovem               37
ASM_FMOVEM:
    CMP.W   #3,D1       ; ".L"?
    BEQ     .FPCR_LIST

    CMP.W   #47,(A3)
    BEQ     .01         ; FP reg list

    BSR     ASM_GETEA
    MOVE.L  #MMSRC+1,D2
    BTST    D0,D2
    BEQ     iERROR19
    TST.W   D0
    BEQ     .02         ; mask in Dx

    OR.W    D3,(A1)
    BSR     ASM_COMMA
    CMP.W   #47,(A3)
    BEQ     .03         ; restore list
;; FMOVEM <ea>,Dx
    Bsr     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    LSL.W   #3,D3
    or.w    d3,2(a1)
    OR.W    #$C800,2(A1)    ; FMOVEM.X  <ea>,Dx
    RTS
;*-*
;; FMOVEM <ea>,<list>
.03:
    BSR     .COMBINE_LIST
    MOVE.L  D0,D7
    BSR     .FLIP
    OR.W    D2,2(A1)
    OR.W    #$D000,2(A1)    ; FMOVEM.X  <ea>,<list>
    RTS
;*-*
;; FMOVEM Dx,<ea>
.02:
    LSL.W   #3,D3
    OR.W    D3,2(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    MOVE.L  #MMDEST,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    CMP.W   #4,d0
    bne     .X2
    OR.W    #$E800,2(A1)    ; FMOVEM.X  Dx,<ea>
    RTS
.X2:OR.W    #$F800,2(A1)
    RTS
;*-*
;; FMOVEM <list>,<ea>
.01:
    BSR     .COMBINE_LIST
    MOVE.L  D2,D7
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    MOVE.L  #MMDEST,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    CMP.W   #4,D0
    BNE     .X1
    OR.W    #$E000,2(A1)    ; FMOVEM.X <list>,<ea>
    OR.W    D7,2(a1)
    RTS
.X1:OR.W    #$F000,2(A1)
    BSR     .FLIP
    OR.W    D2,2(A1)
    RTS
;*-*
;; Flip bits
.FLIP:
    MOVEQ   #7,D0
    MOVEQ   #0,D2
.FLIPX:
    LSR.W   #1,D7
    BCC     .FLIPY
    BSET    D0,D2
.FLIPY:
    DBF     D0,.FLIPX
    RTS
;*-*
;; Combine mask list
.COMBINE_LIST:
    MOVEQ   #0,D0
    BSR     ASM_GETFPREG
    BSET    D2,D0           ; D0 - mask
    MOVE.L  D2,D7
.LOOP:
    CMP.W   #10,(A3)
    BEQ     .ONE
    CMP.W   #8,(A3)
    BNE     .XXX

    ADDQ.L  #2,A3
    ADDQ.L  #1,D7
    BSR     ASM_GETFPREG
    CMP.W   D2,D7
    BGT     iERROR19
.L1:
    BTST    D7,D0
    BNE     iERROR50
    BSET    D7,D0
    ADDQ.B  #1,D7
    CMP.W   D2,D7
    BLE     .L1
    CMP.W   #8,(A3)
    BEQ     iERROR19
    CMP.W   #10,(A3)
    BNE     .XXX

.ONE:
    ADDQ.L  #2,A3
    BSR     ASM_GETFPREG
    BTST    D2,D0
    BNE     iERROR50
    BSET    D2,D0
    MOVE.L  D2,D7
    BRA     .LOOP
.XXX:
    MOVE.L  D0,D2
    RTS
;*-*
;; FPcr
.FPCR_LIST:
    BSR     .COMBINE_FPCR
    OR.W    D2,2(A1)
    OR.W    #$A000,2(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    MOVE.W  #MMDEST,D2
    bTST    D0,D2
    bEQ     iERROR19
    OR.W    D3,(A1)
    RTS
.CREA:
    BSR     ASM_GETEA
    MOVE.W  #MMSRC,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    BSR     .COMBINE_FPCR
    OR.W    d2,2(A1)
    OR.W    #$C000,2(A1)
    RTS
;*-*
;; Combine FPcr
.COMBINE_FPCR:
    CMP.W   #47,(A3)+
    BNE     iERROR80
    MOVEQ   #0,D2
.CFLOOP:
    MOVE.W  (A3)+,D0
    SUB.L   #$24,D0
    BMI     iERROR80
    CMp.W   #1,D0
    BMI     .x1
    BEQ     .x2
    BTST    #10,d2
    bne     iERROR50
    bset    #10,d2
    bra     .x3
.x2:
    btst    #12,d2
    bne     iERROR50
    bset    #12,d2
    bra     .x3
.x1:
    btst    #11,d2
    bne     iERROR50
    bset    #11,d2
.x3:
    cmp.l   #$000a002f,(a3)+
    beq     .CFLOOP
    subq.l  #4,a3
    rts
;*-*
;*-*
;; Fsincos              38
ASM_FSINCOS:
    CMP.W   #47,(A3)
    BNE     .EA
    CMP.W   #5,D1
    BNE     iERROR79
    BSR     ASM_GETFPREG
    LSL.W   #8,D2
    LSL.W   #2,D2
    OR.W    D2,2(A1)
    BRA     .REST
.EA:
    BSR     .ADJUSTSIZE
    BSR     ASM_GETEA
    MOVE.L  #NASRC,D2
    BTST    D0,D2
    BEQ     iERROR19
    TST.W   D0
    BNE     .SKP
    CMP.W   #5,D1
    BPL     iERROR79
.SKP:
    OR.W    D3,(A1)
.REST:
    BSR     ASM_COMMA
    BSR     ASM_GETFPREG
    MOVE.L  D2,D7
    CMP.W   #19,(A3)+
    BNE     iERROR81
    BSR     ASM_GETFPREG
    LSL.W   #7,D2
    OR.W    D2,2(A1)
    OR.W    D7,2(A1)
    RTS

.ADJUSTSIZE:
    MOVE.L  D1,D2
    LSL.W   #1,D2
    MOVE.W  .SIZES(PC,D2),D2
    OR.W    D2,2(A1)
    RTS
.SIZES:
    DC.W    0,$5800,$5000,$4000,$4400,$4800,$4c00,$5400,$0000
;*-*
;; Trapcc               39
ASM_TRAPCC:
    CMP.L   #$0F000001,(A3)
    BEQ     .PARAM
    ADD.W   #2,(A1)
    RTS
.PARAM:
    ADDQ.L  #4,A3
    MOVE.L  (A3)+,D2
    CMP.W   #2,D1
    BEQ     .WRD
    ADD.W   #1,(A1)
    MOVE.L  D2,(A4)+
    RTS
.WRD:
    CHKW    D2,D7,iERROR31
    MOVE.W  D2,(A4)+
    RTS
;*-*
;; Ftst                 40
ASM_FTST:
    CMP.W   #47,(A3)
    BNE     .EA
    CMP.W   #5,D1
    BNE     iERROR79
    BSR     ASM_GETFPREG
    lsl.w   #8,d2
    lsl.w   #2,d2
    or.w    d2,2(a1)
    rts
.EA:
    BSR     ASM_GETEA
    MOVE.L  #NASRC,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    d3,(a1)
    TST.W   D0
    BNE     .SKP
    CMP.W   #5,D1
    BPl     iERROR79
.SKP:
    BSR     .ADJUSTSIZE
    RTS

.ADJUSTSIZE:
    MOVE.L  D1,D2
    LSL.W   #1,D2
    MOVE.W  .SIZES(PC,D2),D2
    OR.W    D2,2(A1)
    RTS
.SIZES:
    DC.W    0,$5800,$5000,$4000,$4400,$4800,$4c00,$5400,$0000
;*-*
;; Standard MMU         41
ASM_PMMSTD:
    CMP.W   #47,(A3)
    BNE     .EA
    ADDQ.L  #2,A3
    MOVE.W  (a3)+,D2
    CMP.W   #2,D2
    BPL     iERROR82
    OR.W    D2,2(A1)
    BRA     .C
.EA:
    BSR     ASM_GETEA
    TST.W   D0
    BNE     .040
    OR.W    #8,D3
    OR.W    D3,2(A1)
.C:
    BSR     ASM_COMMA
    CMP.L   #$0f000001,(a3)+
    BNE     iERROR30
    MOVE.L  (A3)+,D2
    BMI     iERROR31
    CMP.L   #8,D2
    BPL     iERROR31
    LSL.W   #5,D2
    OR.W    D2,2(A1)
    MOVE.W  #0,(A0)
    CMP.W   #COM,(A3)
    BEQ     .C2
    RTS
.C2:
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    MOVE.L  #%000111100100,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    d3,(a1)
    or.w    #$800,2(a1)
    rts
.040:
    TST.W   (A0)
    BEQ     iERROR19
    CMP.W   #2,ECPU
    BLT     iERROR53
    ADDQ.L  #2,A0
    CMP.W   #2,D0
    BNE     iERROR19
    MOVE.W  #$F508,(A1)
    AND.W   #7,D3
    OR.W    #8,D3
    OR.W    D3,(A1)
    LEA     2(A1),A4
    RTS

;*-*
;; MMU Load             42
ASM_PMMLDX:
    CMP.W   #47,(A3)
    BNE     .EA
    ADDQ.L  #2,A3
    MOVE.W  (a3)+,D2
    CMP.W   #2,D2
    BPL     iERROR82
    OR.W    D2,2(A1)
    BRA     .C
.EA:
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    OR.W    #8,D3
    OR.W    D3,2(A1)
.C:
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    MOVE.L  #%000111100100,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    d3,(a1)
    rts
;*-*
;; MMU Test             43
ASM_PMMTST:
    CMP.W   #47,(A3)
    BNE     .EA
    ADDQ.L  #2,A3
    MOVE.W  (a3)+,D2
    CMP.W   #2,D2
    BPL     iERROR82
    OR.W    D2,2(A1)
    BRA     .C
.EA:
    BSR     ASM_GETEA
    TST.W   D0
    BNE     .040
    OR.W    #8,D3
    OR.W    D3,2(A1)
.C:
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    MOVE.L  #%000111100100,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    d3,(a1)
    BSR     ASM_COMMA
    CMP.L   #$0f000001,(a3)+
    BNE     iERROR30
    MOVE.L  (A3)+,D2
    BMI     iERROR31
    CMP.L   #8,D2
    BPL     iERROR31
    LSL.W   #8,D2
    LSL.W   #2,D2
    OR.W    D2,2(A1)
    MOVE.W  #0,(A0)
    CMP.W   #COM,(A3)
    BEQ     .C2
    RTS
.C2:
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    CMP.W   #1,D0
    BNE     iERROR19
    OR.W    #$100,2(a1)
    AND.L   #$7,D3
    LSL.W   #5,D3
    OR.W    D3,2(a1)
    RTS
.040:
    CMP.W   #2,D0
    BNE     iERROR19
    CMP.W   #2,ECPU
    BMI     iERROR53
    ADDQ.L  #2,A0
    MOVE.W  (A0)+,(A1)
    AND.L   #7,D3
    OR.W    D3,(A1)
    LEA     2(A1),A4
    RTS
;*-*
;; MMU Move             44
ASM_PMMMOVE:
    CMP.W   #47,(A3)
    BEQ     .II
    BSR     .EA
    BSR     ASM_COMMA
    BSR     .SEL
    RTS
.II:BSR     .SEL
    BSR     ASM_COMMA
    BSR     .EA
    RTS

.EA:
    BSR     ASM_GETEA
    MOVE.L  #%000111111100,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    RTS

.SEL:
    CMP.W   #47,(a3)+
    BNE     iERROR82
    MOVE.W  (A3)+,D2
    CMP.W   #3,D1
    BEQ     .LO
    CMP.W   #8,D1
    BEQ     .QO
    CMP.W   #2,D1
    BEQ     .WO
    BRA     iERROR79
;; Long ones
.LO:CMP.W   #3,D2
    BNE     .L0
    MOVE.W  #$4200,D2
    BRA     .LZ
.L0:CMP.W   #$13,D2
    BNE     .L1
    MOVE.W  #$A00,D2
    BRA     .LZ
.L1:CMP.W   #$14,D2
    BNE     .L2
    MOVe.W  #$E00,D2
    BRA     .LZ
.L2:BRA     iERROR82

.LZ:OR.W    D2,2(A1)
    RTS
;*-*
;; Quad ones
.QO:CMP.W   #$10,D2
    BNE     .Q0
    MOVE.W  #$4A00,D2
    BRA     .QZ
.Q0:CMP.W   #$1b,D2
    BNE     .Q1
    MOVE.W  #$4600,D2
    BRA     .QZ
.Q1:CMP.W   #$12,D2
    BNE     .Q2
    MOVE.W  #$4e00,D2
    BRA     .QZ
.Q2:BRA     iERROR82

.QZ:OR.W    D2,2(A1)
    RTS
;*-*
;; Word ones
.WO:CMP.W   #$E,D2      ;MMUSR
    BNE     .W0
    MOVE.W  #$6200,D2
    BRA     .WZ
.W0:CMP.W   #$16,D2     ;BAC0
    BNE     .W1
    MOVE.W  #$7600,D2
    BRA     .WZ
.W1:CMP.W   #$30,D2     ;BAC1
    BNE     .W2
    MOVE.W  #$7604,D2
    BRA     .WZ
.W2:CMP.W   #$31,D2     ;BAC2
    BNE     .W3
    MOVE.W  #$7608,D2
    BRA     .WZ
.W3:CMP.W   #$32,D2     ;BAC3
    BNE     .W4
    MOVE.W  #$760c,D2
    BRA     .WZ
.W4:CMP.W   #$33,D2     ;BAC4
    BNE     .W5
    MOVE.W  #$7610,D2
    BRA     .WZ
.W5:CMP.W   #$34,D2     ;BAC5
    BNE     .W6
    MOVE.W  #$7614,D2
    BRA     .WZ
.W6:CMP.W   #$35,D2     ;BAC6
    BNE     .W7
    MOVE.W  #$7618,D2
    BRA     .WZ
.W7:CMP.W   #$36,D2     ;BAC7
    BNE     .W8
    MOVE.W  #$761C,D2
    BRA     .WZ
.W8:CMP.W   #$15,D2     ;BAD0
    BNE     .W9
    MOVE.W  #$7200,D2
    BRA     .WZ
.W9:CMP.W   #$29,D2     ;BAD1
    BNE     .WA
    MOVE.W  #$7204,D2
    BRA     .WZ
.WA:CMP.W   #$2a,D2     ;BAD2
    BNE     .WB
    MOVE.W  #$7208,D2
    BRA     .WZ
.WB:CMP.W   #$2b,D2     ;BAD3
    BNE     .WC
    MOVE.W  #$720c,D2
    BRA     .WZ
.WC:CMP.W   #$2c,D2     ;BAD4
    BNE     .WD
    MOVE.W  #$7210,D2
    BRA     .WZ
.WD:CMP.W   #$2d,D2     ;BAD5
    BNE     .WE
    MOVE.W  #$7214,D2
    BRA     .WZ
.WE:CMP.W   #$2e,D2     ;BAD6
    BNE     .WF
    MOVE.W  #$7218,D2
    BRA     .WZ
.WF:CMP.W   #$2f,D2     ;BAD7
    BNE     .WG
    MOVE.W  #$721C,D2
    BRA     .WZ
.WG:BRA     iERROR82
.WZ:OR.W    D2,2(A1)
    RTS
;*-*
;*-*
;; MMU EA               45
ASM_PMMEA:
    BSR     ASM_GETEA
    MOVE.W  (A0)+,D2
    BTST    D0,D2
    BEQ     iERROR19
    AND.L   #7,D3
    OR.W    D3,(A1)
    RTS
;*-*
;; Immediate 3 bit      46
ASM_IMM3B:
    CMp.L   #$0f000001,(a3)+
    BNE     iERROR30
    MOVE.L  (a3)+,D2
    BMI     iERROR31
    CMP.L   #8,D2
    BPL     iERROR31
    OR.W    D2,(A1)
    RTS
;*-*
;; Movec                47
ASM_MOVEC:
    CMP.W   #47,(A3)
    BNE     .EA_TO_CR
    BSR     .COMBINE_REG
    BSR     ASM_COMMA
    BSR     .COMBINE_EA
    RTS
.EA_TO_CR:
    BSR     .COMBINE_EA
    BSR     ASM_COMMA
    BSR     .COMBINE_REG
    ADD.W   #1,(A1)
    RTS
.COMBINE_REG:
    CMP.W   #47,(A3)+
    BNE     iERROR83
    MOVE.W  (A3)+,D2
    CMP.W   #$12,D2
    BPL     iERROR83
    LSL.W   #1,D2
    MOVE.W  .REGS(PC,D2),D2
    OR.W    D2,2(A1)
    RTS
.REGS:
    DC.W    $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,$0008
    DC.W    $0800,$0801,$0802,$0803,$0804,$0805,$0806,$0807,$0808
.COMBINE_EA:
    BSR     ASM_GETEA
    MOVE.L  #3,D2
    BTST    D0,D2
    BEQ     iERROR19
    LSL.W   #8,D3
    LSL.W   #4,D3
    OR.W    D3,2(A1)
    RTS
;*-*
;; Moves                48
ASM_MOVES:
    MOVEQ   #0,D2
    CMP.W   #1,D1
    BEQ     .SX
    ADD.W   #$40,D2
    CMP.W   #2,D1
    BEQ     .SX
    ADD.W   #$40,D2
.SX:
    ADD.W   D2,(A1)
    BSR     ASM_GETEA
    MOVE.L  #%000011111111,D2
    BTST    D0,D2
    BEQ     iERROR19
    CMP.W   #2,D0
    BMI     .SPACE
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    CMP.W   #2,D0
    BPL     iERROR19
    LSL.W   #8,D3
    LSL.W   #4,D3
    OR.W    D3,2(A1)
    RTS
.SPACE:
    LSL.W   #8,D3
    LSL.W   #4,D3
    OR.W    #$800,D3
    OR.W    D3,2(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    MOVE.L  #%000011111100,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    RTS
;*-*
;; BitField1            49
ASM_BITFIELD1:
    BSR     ASM_GETEA
    MOVE.L  #DEST,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    CMP.W   #23,(A3)+
    BNE     iERROR19
    BSR     .GET
    MOVE.L  D2,D7
    CMP.W   #19,(A3)+
    BNE     iERROR81
    BSR     .GET
    LSL.W   #6,D7
    OR.W    D7,2(A1)
    OR.W    D2,2(A1)
    CMP.W   #24,(A3)+
    BNE     iERROR29
    RTS
.GET:
    CMP.W   #$0001,(A3)
    BNE     .EA
    ADDQ.L  #2,A3
    MOVE.L  (A3)+,D2
    BMI     iERROR31
    CMP.L   #32,D2
    BPL     iERROR31
    RTS
.EA:
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    MOVE.L  D3,D2
    OR.W    #$20,D2
    RTS
;*-*
;; BitField2            50
ASM_BITFIELD2:
    BSR     ASM_GETEA
    MOVE.L  #DEST,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    CMP.W   #23,(A3)+
    BNE     iERROR19
    BSR     .GET
    MOVE.L  D2,D7
    CMP.W   #19,(A3)+
    BNE     iERROR81
    BSR     .GET
    LSL.W   #6,D7
    OR.W    D7,2(A1)
    OR.W    D2,2(A1)
    CMP.W   #24,(A3)+
    BNE     iERROR29
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    LSL.W   #8,D3
    LSL.W   #4,D3
    OR.W    D3,2(A1)
    RTS
.GET:
    CMP.W   #$0001,(A3)
    BNE     .EA
    ADDQ.L  #2,A3
    MOVE.L  (A3)+,D2
    BMI     iERROR31
    CMP.L   #32,D2
    BPL     iERROR31
    RTS
.EA:
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    MOVE.L  D3,D2
    OR.W    #$20,D2
    RTS
;*-*
;; BitField3            51
ASM_BITFIELD3:
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    LSL.W   #8,D3
    LSL.W   #4,D3
    OR.W    D3,2(A1)

    BSR     ASM_COMMA

    BSR     ASM_GETEA
    MOVE.L  #DEST,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    CMP.W   #23,(A3)+
    BNE     iERROR19
    BSR     .GET
    MOVE.L  D2,D7
    CMP.W   #19,(A3)+
    BNE     iERROR81
    BSR     .GET
    LSL.W   #6,D7
    OR.W    D7,2(A1)
    OR.W    D2,2(A1)
    CMP.W   #24,(A3)+
    BNE     iERROR29
    RTS
.GET:
    CMP.W   #$0001,(A3)
    BNE     .EA
    ADDQ.L  #2,A3
    MOVE.L  (A3)+,D2
    BMI     iERROR31
    CMP.L   #32,D2
    BPL     iERROR31
    RTS
.EA:
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    MOVE.L  D3,D2
    OR.W    #$20,D2
    RTS
;*-*
;; Mul/Div              52
ASM_MULDIV:
    CMP.W   #2,D1
    BEQ     .WRD
    MOVE.W  (A0)+,(A1)
    MOVE.W  (A0)+,(A4)+
    MOVE.W  ASMCPU(PC),D2
    AND.L   #$FE,D2
    BEQ     iERROR53
    BSR     ASM_GETEA
    MOVE.L  #NASRC,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    MOVE.W  D3,D7
    CMP.W   #19,(A3)
    BNE     .X1
    ADDQ.L  #2,A3
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
.X1:LSL.W   #8,D3
    LSL.W   #4,D3
    OR.W    D3,2(A1)
    OR.W    D7,2(A1)
    RTS
.WRD:
    ADDQ.L  #4,A0
    BSR     ASM_GETEA
    MOVE.L  #NASRC,D2
    BTST    D0,D2
    BEQ     iERROR19
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    TST.W   D0
    BNE     iERROR19
    LSL.W   #8,D3
    LSL.W   #1,D3
    OR.W    D3,(A1)
    RTS
;*-*
;; Link                 53
ASM_LINK:
    CMP.W   #3,D1
    BEQ     .LONG
    BSR     ASM_GETEA
    CMP.W   #1,D0
    BNE     iERROR19
    AND.L   #7,D3
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    CMP.L   #$0F000001,(A3)+
    BNE     iERROR30
    MOVE.L  (A3)+,D0
    CHKW    D0,D2,iERROR31
    MOVE.W  D0,(A4)+
    RTS
.LONG:
    MOVE.W  ASMCPU,D2
    AND.L   #%111110,D2
    BEQ     iERROR53
    MOVE.W  #$4800,(A1)
    BSR     ASM_GETEA
    CMP.W   #1,D0
    BNE     iERROR19
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    CMP.L   #$0F000001,(A3)+
    BNE     iERROR30
    MOVE.L  (A3)+,(A4)+
    RTS
;*-*
;; CpCache 040+         54
ASM_CPCACHE040:
    CMP.W   #47,(A3)+
    BNE     iERROR84
    MOVEQ   #0,D2
    MOVE.W  (A3)+,D2
    SUB.L   #$37,D2
    BMI     iERROR84
    CMP.W   #4,D2
    BPL     iERROR84
    LSL.W   #6,D2
    ADD.W    D2,(A1)
    TST.W   (A0)+
    BEQ     .XX
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    CMP.W   #2,D0
    BNE     iERROR19
    AND.L   #7,D3
    OR.W    D3,(A1)
.XX:RTS
;*-*
;; Move16               55
ASM_MOVE16:
    BSR     ASM_GETEA
    CMP.W   #8,D0
    BEQ     .ABS
    CMP.W   #3,D0
    BEQ     .INC
    CMP.W   #2,D0
    BNE     iERROR19
    AND.L   #7,d3
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    CMP.W   #8,D0
    BNE     iERROR19
    RTS
.ABS:
    SUB.W   #8,(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    CMP.W   #3,D0
    BEQ     .SKIP
    ADD.W   #16,(A1)
.SKIP:
    AND.L   #7,D3
    OR.W    D3,(A1)
    RTS
.INC:
    SUB.W   #$10,(A1)
    AND.L   #7,D3
    OR.W    D3,(A1)
    BSR     ASM_COMMA
    BSR     ASM_GETEA
    CMP.W   #8,D0
    BNE     .REL2
    RTS
.REL2:
    cmp.w   #3,d0
    bne     iERROR19
    add.w   #$20,(A1)
    lsl.w   #8,d3
    lsl.w   #4,d3
    move.w  d3,(a4)+
    rts
;*-*
;; Pulse                56
ASM_PULSE:
    CMP.L   #$0F000001,(A3)+
    BNE     iERROR30
    MOVE.L  (A3)+,D2
    CHKW    D2,D0,iERROR31
    MOVE.W  D2,(A4)+
    RTS
;*-*
;; Move                 57
ASM_MOVE:
    CMP.W   #47,(A3)
    BEQ     .KW1
    BSR     ASM_GETEA
    MOVE.L  #%111111111111,d2
    btst    d0,d2
    beq iERROR19     ;should never happen.
    cmp.w   #1,d0
    bne .1
    cmp.w   #1,d1
    beq iERROR31
.1:
    or.w    d3,(a1)

    bsr ASM_COMMA

    cmp.w   #47,(a3)
    beq     .KW2
    bsr ASM_GETEA
    move.l  #%000111111111,d2
    btst    d0,d2
    beq iERROR19
    cmp.w   #1,d0
    bne     .2

    cmp.w   #1,d1
    beq iERROR31
    and.l   #7,d3
    lsl.w   #8,d3
    lsl.w   #1,d3
    or.w    d3,(a1)
    or.w    #64,(a1)
    rts
.2:
    move.w  d3,d0
    lsr.w   #3,d3
    and.w   #%111,d0
    lsl.w   #3,d0
    add.w   d3,d0
    lsl.w   #6,d0
    or.w    d0,(a1)
    rts

.KW1:
    cmp.w   #2,d1
    bne iERROR31
    addq.l  #2,a3
    move.w  (a3)+,d0

    cmp.w   #$27,d0
    bne .KW1_2
    move.w  #$42c0,(A1)
    bra .KW1_EXIT
.KW1_2:
    cmp.w   #$28,d0
    bne iERROR19
    move.w  #$40c0,(a1)
.KW1_EXIT:
    bsr ASM_COMMA
    BSR ASM_GETEA
    move.l  #%000111111101,d2
    btst    d0,d2
    beq iERROR19
    or.w    d3,(A1)
    rts
.KW2:

    cmp.w   #1,d0
    beq     iERROR19     ; move A0,xxx
    addq.l  #2,a3
    move.w  (a3)+,d0
    cmp.w   #$27,d0
    bne     .KW2_2
    move.w  (a1),d0
    and.w   #%111111,d0
    move.w  #$44c0,(a1)
    or.w    d0,(a1)
    rts
.KW2_2:
    cmp.w   #$28,d0
    bne iERROR19
    move.w  (a1),d0
    and.w   #%111111,d0
    move.w  #$46c0,(a1)
    or.w    d0,(a1)
    rts
;*-*
;; Andi                 58
ASM_ANDI:
    BSR ASM_GETEA
    CMP.W   #11,d0
    bne iERROR30

    bsr ASM_COMMA

    cmp.w   #47,(a3)
    beq .KW
    bsr ASM_GETEA
    move.l  #%000111111101,d2
    btst    d0,d2
    beq iERROR19
    or.w    d3,(a1)
    rts
.KW:
    cmp.w   #1,d1
    bne iERROR31
    addq.l  #2,a3
    cmp.w   #$27,(a3)+
    bne iERROR19
    move.w  #$23c,(a1)
    rts
;*-*
;; Eori                 59
ASM_EORI:
    BSR ASM_GETEA
    CMP.W   #11,d0
    bne iERROR30

    bsr ASM_COMMA

    cmp.w   #47,(a3)
    beq .KW
    bsr ASM_GETEA
    move.l  #%000111111101,d2
    btst    d0,d2
    beq iERROR19
    or.w    d3,(a1)
    rts
.KW:
    cmp.w   #1,d1
    bne iERROR31
    addq.l  #2,a3
    cmp.w   #$27,(a3)+
    bne iERROR19
    move.w  #$A3c,(a1)
    rts
;*-*
;; Ori                  60
ASM_ORI:
    BSR ASM_GETEA
    CMP.W   #11,d0
    bne iERROR30

    bsr ASM_COMMA

    cmp.w   #47,(a3)
    beq .KW
    bsr ASM_GETEA
    move.l  #%000111111101,d2
    btst    d0,d2
    beq iERROR19
    or.w    d3,(a1)
    rts
.KW:
    cmp.w   #1,d1
    bne iERROR31
    addq.l  #2,a3
    cmp.w   #$27,(a3)+
    bne iERROR19
    move.w  #$3c,(a1)
    rts
;*-*

;; FPU reg
ASM_GETFPREG:
    CMP.W   #47,(A3)+
    BNE     iERROR80
    MOVE.W  (A3)+,D2
    SUB.W   #$1c,D2
    BMI     iERROR80
    CMP.W   #$8,D2
    BPL     iERROR80
    RTS
;*-*
;; Data reg
ASM_GETDATAREG:           ; D0=SHIFT
    BSR ASM_REGID
    CMP.B   #4,(A3)+
    BNE iERROR19
    MOVE.B  (A3)+,D2
    AND.W   #%111,D2
    LSL.W   D0,D2
    ADD.W   D2,(A1)
    BSR SKIP244
    RTS
;*-*
;; Any EA
ASM_ANYEA:            ; D7=SHIFT, res=d3.ea after shift
    BSR.W   ASM_GETEA
    LSL.W   D7,D3
    ADD.W   D3,(A1)
    RTS
;*-*
;; Get EA
ASM_GETEA:
    MOVE.W  (A3)+,D0
    CMP.W   #$400,D0        ; COMPLEX OR BASIC ADR.MODE
    BMI .2
;; Dx/Ax/(Ax)/(PC)/(A0)+/-(A0)
    SUB.W   #$400,D0        ; LOWBYTE IS ADR.MASK, HI IS #ADR.MODE
    MOVEQ   #0,D3
    MOVE.B  D0,D3           ; D3.L = ADR.MODE MASK
    LSR.W   #8,D0           ; D0.W = # ADR.MODE
    CMP.W   #11,D0
    BEQ.S   .1
    CMP.W   #5,D0
    BPL iERROR19
    CMP.W   #1,D0
    BEQ.S   .AX
.X: RTS                     ; DELIVERS MASK IN D3 AND # IN D0
.AX:    CMP.W   #1,D1
    BEQ iERROR19
    RTS
;*-*
;; Immediate
.1: MOVEM.L D1/A0,-(A7)
    CMP.W   #LIBC,(A3)
    BNE .1_1
    ADDQ.L  #4,A3
    MOVE.W  (A3)+,D1
    EXT.L   D1
    ADDQ.L  #8,A3
    MOVEQ   #0,D0
    BRa .1_2
.1_1:
    BSR ASM_GRABVALUE
.1_2:
    MOVE.L  D1,D4
    MOVEM.L (A7)+,D1/A0
    CMP.W   #-1,D0          ; IMMEDIATE VALUE?
    BEQ iERROR19
    MOVE.W  #%111100,D3
    MOVEQ   #11,D0
    CMP.W   #3,D1
    BNE.S   .4
    MOVE.L  D4,(A4)+        ; .L #
    RTS
.4: CMP.W   #2,D1
    BNE.S   .5
    CHKW    D4,D5,iERROR93
    MOVE.W  D4,(A4)+        ; .W #
    RTS
.5: CHKB    D4,D5,iERROR94
    MOVE.B  #0,(A4)+        ; .B #
    MOVE.B  D4,(A4)+
    RTS
;*-*
.2:
    CMP.W   #29,(A3)
    BEQ _020
    CMP.W   #17,D0
    BEQ _BRACKET
;; Other
    SUBQ.L  #2,A3           ; ANY OTHER MODE
    MOVEM.L D1/A0,-(A7)
    BSR ASM_GRABVALUE
    MOVE.L  D1,D4           ; D4=VALUE
    CMP.W   #-1,D0
    BEQ ASM_GETLABEL
    MOVEM.L (A7)+,D1/A0
GL_BACK:
    MOVE.B  (A3),D2         ; SEE WHAT'S NEXT
    CMP.B   #4,D2
    BMI     .6              ; ABS OR INDEXED MODE
    CMP.W   #$D,d2
    beq     .xxPC
    CMP.B   #6,D2           ;
    BNE iERROR19
    MOVE.W  (A3)+,D3
    AND.W   #%111,D3
    OR.W    #%101000,D3
    MOVEQ   #5,D0
    MOVE.W  D4,(A4)+
    CHKW    D4,D5,iERROR93
    RTS
;; $00(PC)
.xxPC:
    addq.l  #2,a3
    moveq   #0,d3
    move.w  #%111010,d3
    moveq   #9,d0
    move.w  d4,(a4)+
    CHKW    D4,D5,iERROR31
    RTS
;*-*
;; $00(PC,Rn)
.xxPCxx:
    addq.l  #2,a3
    BSR ASM_COMMA
    cmp.b   #4,(a3)
    blt     iERROR17
    cmp.b   #5,(a3)
    bgt     iERROR17
    seq d0
    move.w  (a3)+,d3
    and.w   #7,d3
    and.b   #8,d0
    or.b    d0,d3
    lsl.w   #4,d3
    move.b  d3,(a4)+
    move.b  d4,(a4)+
    MOVE.L  D4,D5           ; D5=EXTENSIONWORD
    CHKB    D5,D4,iERROR31

    moveq   #0,d3
    move.w  #%111011,d3
    moveq   #10,d0
    CHKW    D4,D5,iERROR31
    cmp.w   #18,(a3)+
    bne iERROR74
    RTS
;*-*
.6: MOVE.W  (A3),D2
    CMP.W   #17,D2
    BEQ.S   .9
    CMP.W   #26,D2
    BEQ.S   .7
    CMP.W   #27,D2
    BNE.S   .8
    ADDQ.L  #2,A3
.8: MOVE.W  #%111001,D3     ; ABS 4
    MOVEQ   #8,D0
    MOVE.L  D4,(A4)+
    RTS
.7: ADDQ.L  #2,A3           ; ABS 4.W
    MOVE.W  #%111000,D3
    MOVEQ   #7,D0
    MOVE.W  D4,(A4)+
    CHKW    D4,D5,iERROR93
    RTS

.9: ADDQ.L  #2,A3           ; off(Ax,Rx.s), D4=off
    CMP.B   #$E,(A3)
    BEQ .xxPCxx
    CMP.B   #5,(A3)+
    BNE iERROR19
    MOVE.B  (A3)+,D3        ; D3.B = Ax
    AND.L   #%111,D3        ; (Ax,...
    OR.W    #%110000,D3
    MOVEQ   #6,D0
    MOVE.L  D4,D5           ; D5=EXTENSIONWORD
    CHKB    D5,D4,iERROR94
    AND.W   #$FF,D5
    CMP.W   #COM,(A3)+
    BNE iERROR5
    BSR ASM_REGID
    MOVE.B  (A3)+,D2
    CMP.B   #4,D2           ; ...,Dx
    BEQ.S   .12
    CMP.B   #5,D2
    BNE iERROR19
    BSET    #15,D5          ; ...,Ax
.12:
    MOVE.B  (A3)+,D2
    BSR SKIP244
    AND.W   #%111,D2
    MOVEQ   #12,D4
    LSL.W   D4,D2
    OR.W    D2,D5

    MOVE.W  (A3)+,D2
    CMP.W   #26,D2
    BEQ .12_2
    CMP.W   #27,D2
    BNE .13
    BSET    #11,D5
.12_2:
    MOVE.W  (A3)+,D2
.13:
    CMp.W   #9,D2
    BNE .14
    CMP.W   #VALUE,(A3)+
    BNE iERROR73
    MOVE.L  (A3)+,D2
    MOVEQ   #0,D0

    CMP.W   #1,D2
    BEQ .16
    ADDQ.L  #1,D0
    CMP.W   #2,D2
    BEQ .16
    ADDQ.L  #1,D0
    CMP.W   #4,D2
    BEQ .16
    ADDQ.L  #1,D0
    CMP.W   #8,D2
    BNE iERROR91
.16:
    LSL.W   #8,D0
    LSL.W   #1,D0
    OR.W    D0,D5
    MOVE.W  (A3)+,D2
.14:
    CMP.W   #18,D2
    BNE iERROR74
    MOVE.W  D5,(A4)+
    RTS
;*-*
;; 020 full
_020:
    CMP.W   #1,ECPU
    BMI iERROR53
    MOVE.L  A0,-(A7)
    MOVE.L  A4,A0
    ADDQ.L  #2,A0

    MOVE.W  #$30,d3         ; adressing mode for suppressed reg
    MOVE.W  #6,d0
    MOVEQ   #0,D6
    MOVE.l  #%0000000111010000,D2   ; here we set a MOVE.L  (),...
    ADDQ.L  #6,A3           ; skip "[xx"

    CMP.W   #30,(A3)
    BNE     .020_0
    ADDQ.L  #2,A3
    BSR     ASM_COMMA
    BSET    #1,D6
    BRA     .020_2

.020_0:
    CMP.W   #IDENT,(A3)
    BEQ     .020_ID
    MOVEM.L D0/D2-D4/D6-A2/A4-a6,-(A7)
    BSR ASM_GRABVALUE
    MOVE.L  D0,D5
    MOVEM.L (A7)+,D0/D2-D4/D6-A2/A4-A6
    TST.L   D5
    BMI .020_1
    CMP.W   #26,(A3)
    BMI     .020_0_2
    CMP.W   #28,(A3)
    BPL     .020_0_2
    CMP.W   #27,(A3)+
    BEQ     .020_0_2
    MOVE.W  D1,(A0)+
    or.w    #32,d2
    BCLR    #4,D2
    BRA     .020_0_1
.020_0_2:
    MOVE.L  D1,(A0)+
    OR.W    #48,D2          ; MOVE.L    ([$xxxxxxxx.L
.020_0_1:
    CMP.W   #COM,(A3)+
    BEQ .020_1
    CMP.W   #30,-2(A3)
    bne iERROR0
    cmp.w   #18,(a3)
    beq     .020_EXIT
    BSR ASM_COMMA           ; MOVE.L    ([$xxxxxxxx.L],
    BSET    #0,D6
    BRA     .020_1
.020_ID:
    ADDQ.L  #2,A3
    MOVE.L  (A3)+,D1
    CMP.W   #$E00,2(a3)
    beq     .020_ID_PCREL
    cmp.w   #$E00,4(a3)
    beq     .020_ID_PCREL
    MOVEM.L D0-D7/A1/A2/A4-A6,-(A7)
    MOVE.L  D1,A1
    CMP.B   #LAB,4(A1)
    BNE     iERROR4
    MOVE.L  A0,A4
    MOVE.W  10(A1),D0
    BSR ADDBRANCHRELOC
    MOVE.L  A4,A0
    MOVEM.L (A7)+,D0-D7/A1/A2/A4-a6
    OR.W    #48,D2
    BRA .020_0_1
.020_ID_PCREL:
    CMP.W   #26,(A3)
    BMI .020_ID_NOADJ
    CMP.W   #28,(A3)
    BPL .020_ID_NOADJ
    CMP.W   #27,(A3)+
    BPL .020_ID_NOADJ
    BCLR    #4,D2
    OR.W    #32,D2
    CLR.W   NEWOP
    MOVEm.L  D0-D7/A1/a2/A4-a6,-(A7)
    move.l  a0,a4
    MOVE.L  D1,A1
    MOve.w  10(a1),d0
    BSR ADDBRANCHPCREL16E
    move.l  a4,a0
    movem.l  (a7)+,d0-d7/a1/a2/a4-a6
    BRA .020_0_1
.020_ID_NOADJ:
    MOVEm.L  D0-D7/A1/a2/A4-a6,-(A7)
    move.l  a0,a4
    MOVE.L  D1,A1
    MOve.w  10(a1),d0
    BSR ADDBRANCHPCREL32E
    move.l  a4,a0
    movem.l  (a7)+,d0-d7/a1/a2/a4-a6
    OR.W    #48,D2
    BRA .020_0_1

.020_1:
    CMP.w   #$E00,(A3)
    beq     .020_1_1
    CMP.B   #5,(A3)
    BNE     .020_2
    CMP.W   #9,2(A3)
    BEQ     .020_2
    CMP.W   #26,2(A3)
    BEQ     .020_2
    CMP.W   #27,2(a3)
    beq     .020_2
    BTST    #0,D6
    bne     .020_2          ; MOVE.L    ([$xxxxxxxx.L]
    MOVE.W  (A3)+,D1        ; MOVE.L    ([$xxxxxxxx.L,Ax
    BCLR    #7,D2
    and.l   #7,d1
    add.w   d1,d3
    bra     .020_1_2
.020_1_1:
    addq.l  #2,a3
    addq.l  #8,d3
    addq.l  #3,d3
    moveq   #10,d0
    bclr    #7,d2
.020_1_2:
    cmp.w   #COM,(A3)+
    beq     .020_2
    cmp.w   #30,-2(a3)
    bne     iERROR0
    cmp.w   #18,(a3)
    beq     .020_EXIT
    bset    #1,d6
    bsr ASM_COMMA
.020_2:
    CMP.B   #4,(A3)
    BMI .020_3
    CMP.B   #6,(A3)
    BPL .020_3

    BTST    #1,D6
    BEQ .020_2_1
    BSET    #2,D2
.020_2_1:
    bclr    #6,d2           ; MOVE.L    ([$xxxxxxxx.L, Ax, Rn
    MOVE.W  (A3)+,D1        ; or
    btst    #8,d1           ; MOVE.L    ([$xxxxxxxx.L, Ax], Rn
    beq .020_2_2
    bset    #15,d2
.020_2_2:
    and.l   #7,d1
    rol.w   #8,d1
    rol.w   #4,d1
    or.w    d1,d2
    cmp.w   #26,(A3)
    bmi     .020_2_3
    cmp.w   #28,(a3)
    bpl     .020_2_3
    cmp.w   #27,(a3)+
    bne     .020_2_3
    bset    #11,d2
.020_2_3:
    cmp.w   #9,(a3)
    bne     .020_2_5
    addq.l  #2,a3
    cmp.w   #VALUE,(a3)+
    bne     iERROR73
    move.l  (A3)+,d1
    moveq   #0,d0
    cmp.l   #1,d1
    beq     .020_2_4
    addq.l  #1,d0
    cmp.l   #2,d1
    beq     .020_2_4
    addq.l  #1,d0
    cmp.l   #4,d1
    beq     .020_2_4
    addq.l  #1,d0
    cmp.l   #8,d1
    bne     iERROR91
.020_2_4:
    rol.w   #8,d0           ; MOVE.L    ([$xxxxxxxx.L, Ax, Rn.s*y], $yyyyyyyy)
    rol.w   #1,d0
    or.w    d0,d2
.020_2_5:
    btst    #1,d6
    bne     .020_2_7
    cmp.w   #30,(a3)+
    bne     iERROR34
.020_2_7:
    cmp.w   #18,(a3)
    beq .020_EXIT
    BSR ASM_COMMA

.020_3:
    MOVEM.L D0/D2-D4/D6-A2/A4-a6,-(A7)
    BSR ASM_GRABVALUE
    MOVE.L  D0,D5
    MOVEM.L (A7)+,D0/D2-D4/D6-A2/A4-A6
    TST.L   D5
    BMI .020_EXIT

    CMP.W   #26,(A3)
    BMI     .020_3_1
    CMP.W   #28,(A3)
    BPL     .020_3_1
    cmp.w   #26,(A3)+
    BNE     .020_3_1
    move.w  d1,(a0)+
    or.w    #2,d2
    bra     .020_3_2
.020_3_1:
    move.l  d1,(a0)+
    or.w    #3,d2
.020_3_2:
.020_EXIT
    CMP.W   #18,(A3)+
    BNE iERROR0
    MOVE.W  D2,(A4)
    MOVE.L  A0,A4
    MOVE.L  (A7)+,A0
    RTS
;*-*
;; bracketed
;; value
_BRACKET:
    MOVEQ   #0,D4
    MOVE.W  (A3),D2
    CMP.W   #IDENT,D2
    BEQ .BR_IDENT
    CMP.W   #LIBC,D2
;    BEQ .BR_LIBC
    CMP.W   #VALUE,D2       ; ($xx,Ax/PC,Rn.S*F)
    BNE .BR_0
    ADDQ.L  #2,A3
    MOVE.L  (A3)+,D4
    swap    d4
    tst.w   d4
    bne iERROR93
    swap    d4

    BSR ASM_COMMA
.BR_0:
    CMP.B   #$E,(A3)
    BEQ .BR_1
    CMP.B   #$5,(A3)
    BNE iERROR92
    MOVE.W  (A3)+,D2
    AND.L   #7,D2
    moveq   #$28,d3
    or.w    d2,d3
    moveq   #5,d0
    BRA .BR_2
.BR_1:
    move.w  (a3)+,d2
    moveq   #$3a,d3
    moveq   #9,d0
.BR_2:
    CMP.W   #COM,(A3)
    BNE .BR_EXIT
    BSR ASM_COMMA
    CMP.B   #5,(a3)
    seq D5
    moveq   #8,d2
    and.l   d2,d5
    move.w  (a3)+,d2
    and.l   #7,d2
    or.w    d5,d2
    ror.w   #4,d2
    ror.l   #8,d4
    tst.b   D4
    bne iERROR94
    rol.l   #8,d4
    or.w    d2,d4
    cmp.w   #$3B,D3
    BEQ .BR_3
    ADDQ.L  #7,d3
.BR_3:
    ADDQ.L  #1,D3
    addq.l  #1,d0
    CMP.W   #18,D2
    BEQ .BR_EXIT
    move.w  (a3),d2
    cmp.w   #27,D2
    bgt .BR_4
    cmp.w   #26,d2
    blt .BR_4
    move.w  (a3)+,d2
    cmp.w   #26,d2
    beq .BR_4
    bset    #11,d4
.BR_4:
    cmp.w   #18,(A3)
    beq .BR_EXIT
    move.w  (a3)+,d2
    cmp.w   #9,d2
    bne iERROR0
    cmp.w   #VALUE,(a3)+
    bne iERROR73
    move.l  (a3)+,d2
    moveq   #0,d5

    CMP.W   #1,D2
    BEQ .BR_5
    ADDQ.L  #1,D5
    CMP.W   #2,D2
    BEQ .BR_5
    ADDQ.L  #1,D5
    CMP.W   #4,D2
    BEQ .BR_5
    ADDQ.L  #1,D5
    CMP.W   #8,D2
    BNE iERROR91
.BR_5:
    LSL.W   #8,D5
    LSL.W   #1,D5
    OR.W    D5,D4

.BR_EXIT:
    cmp.w   #18,(a3)+
    bne iERROR74
    move.w  d4,(a4)+
    rts
;*-*
;; ident
.BR_IDENT:
    addq.l  #2,a3
    MOVE.L  (A3)+,A5
    tst.b   4(a5)
    beq iERROR22
    cmp.b   #3,4(a5)
    beq .BRI_LABEL
    moveq   #$28+5,d3
    moveq   #5,d0
    moveq   #0,d4
    move.w  10(a5),d4
    cmp.b   #2,4(a5)
    bne .BRI_1
    subq.w  #1,d3
.BRI_1:
    cmp.w   #COM,(a3)
    bne .BR_EXIT
    ADDQ.L  #1,D0
    addq.l  #8,d3
    ror.w   #8,d4
    tst.b   d4
    beq .BRI_0
    cmp.b   #$ff,d4
    bne iERROR94
.BRI_0:
    clr.b   d4
    rol.w   #8,d4
    bsr ASM_COMMA
    cmp.b   #5,(a3)
    seq D2
    bgt iERROR95
    cmp.b   #4,(a3)
    blt iERROR95
    and.l   #8,d2
    ror.w   #4,d2
    or.w    d2,d4
    move.w  (a3)+,d2
    and.l   #7,d2
    ror.w   #4,d2
    or.w    d2,d4
    cmp.w   #18,(a3)
    beq .BR_EXIT
    cmp.w   #9,(a3)
    beq .BRI_2
    cmp.w   #26,(a3)
    blt iERROR0
    cmp.w   #27,(a3)
    bgt iERROR0
    seq D2
    rol.l   #8,d2
    and.l   #$800,d2
    or.w    d2,d4
    addq.l  #2,a3
.BRI_2:
    cmp.w   #18,(a3)
    beq .BR_EXIT
    cmp.w   #9,(a3)+
    bne iERROR9
    cmp.w   #VALUE,(a3)+
    bne iERROR73
    move.l  (a3)+,d2
    moveq   #0,d5
    cmp.w   #1,d2
    beq .BRI_3
    addq.l  #1,d5
    cmp.w   #2,d2
    beq .BRI_3
    addq.l  #1,d5
    cmp.w   #4,d2
    beq .BRI_3
    addq.l  #1,d5
    cmp.w   #8,d2
    bne iERROR91
.BRI_3:
    lsl.l   #8,d5
    lsl.l   #1,d5
    or.w    d5,d4
    bra .BR_EXIT
;*-*
;; label
.BRI_LABEL:
    BSR ASM_COMMA
    CMP.W   #$E00,(A3)+
    BNE iERROR92

    moveq   #$3a,d3
    moveq   #9,d0
    moveq   #0,d4

    cmp.w   #COM,(a3)
    bne .BRL_EXIT
    ADDQ.L  #1,D0
    addq.l  #1,d3
    bsr ASM_COMMA
    cmp.b   #5,(a3)
    seq D2
    bgt iERROR95
    cmp.b   #4,(a3)
    blt iERROR95
    and.l   #8,d2
    ror.w   #4,d2
    or.w    d2,d4
    move.w  (a3)+,d2
    and.l   #7,d2
    ror.w   #4,d2
    or.w    d2,d4
    cmp.w   #18,(a3)
    beq .BRL_EXIT
    cmp.w   #9,(a3)
    beq .BRL_2
    cmp.w   #26,(a3)
    blt iERROR0
    cmp.w   #27,(a3)
    bgt iERROR0
    seq D2
    rol.l   #8,d2
    and.l   #$800,d2
    or.w    d2,d4
    addq.l  #2,a3
.BRL_2:
    cmp.w   #18,(a3)
    beq .BRL_EXIT
    cmp.w   #9,(a3)+
    bne iERROR9
    cmp.w   #VALUE,(a3)+
    bne iERROR73
    move.l  (a3)+,d2
    moveq   #0,d5
    cmp.w   #1,d2
    beq .BRL_3
    addq.l  #1,d5
    cmp.w   #2,d2
    beq .BRL_3
    addq.l  #1,d5
    cmp.w   #4,d2
    beq .BRL_3
    addq.l  #1,d5
    cmp.w   #8,d2
    bne iERROR91
.BRL_3:
    lsl.l   #8,d5
    lsl.l   #1,d5
    or.w    d5,d4
.BRL_EXIT:
    cmp.w   #18,(a3)+
    bne iERROR74
    move.w  d4,(a4)+
    cmp.w   #10,d0
    beq .BRL_EXIT2
.BRL_FINE:
    movem.l  d0/A0,-(a7)
    move.w  10(a5),d0
    clr.w   NEWOP
    bsr ADDBRANCH
    movem.l  (a7)+,d0/a0
    rts
.BRL_EXIT2:
    move.b  #1,-1(a4)
    bra .BRL_FINE

;*-*
;*-*
;*-*
;; GetLabel
ASM_GETLABEL:         ; E INTERFACE TO ASM_GETEA ADR.MODE
    MOVEM.L (A7)+,D1/A0
    MOVE.W  (A3)+,D0
    CMP.W   #IDENT,D0
    BNE .4
    MOVE.L  (A3)+,A5        ; A5=VAR
    MOVE.B  4(A5),D2
    BEQ iERROR22
    CMP.B   #LAB,D2
    BEQ .3
    BTST    #3,5(A5)        ; SEE IF REGVAR
    BNE.W   .REG
    CMP.B   #GLOBV,D2
    BNE.S   .1
    MOVEQ   #%101100,D3
    BRA.S   .2
.1: MOVEQ   #%101101,D3
.2: MOVE.W  10(A5),D0
    MOVE.W  (A3),D2
    CMP.W   #29,D2
    BPL.S   .5
    CMP.W   #26,D2
    BMI.S   .5
    ADDQ.L  #2,A3
    CMP.W   #27,D2
    BEQ.S   .5
    CMP.W   #26,D2
    BNE.S   .6
    ADDQ.W  #2,D0
    OPINT
    BRA.S   .5
.6: ADDQ.W  #3,D0
    OPCHAR
.5: CMP.B   #GLOBV,4(A5)
    BNE.S   .8
    GENGI   A5,D2
.8: OPLONG
    MOVE.W  D0,(A4)+
    MOVEQ   #5,D0
    RTS

.REG:   MOVEQ   #0,D0
    MOVE.W  10(A5),D3
    RTS

.3: BTST    #4,5(A5)        ; CHECK IF METHOD
    BNE iERROR4
    CMP.W   #$D00,(A3)+
    BNE .9
    MOVE.W  10(A5),D0       ; LAB(PC)
    CLR.W   (A4)+
    MOVEM.L D1/A0,-(A7)
    CLR.W   NEWOP
    BSR ADDBRANCH
    MOVEM.L (A7)+,D1/A0
    MOVEQ   #9,D0
    MOVEQ   #%111010,D3
    RTS

.9: SUBQ.L  #2,A3           ; lab(PC,Rx.s)
    CMP.W   #17,(A3)+
    BNE iERROR19
    CMP.W   #$E00,(A3)+
    BNE iERROR19
    MOVEQ   #%111011,D3     ; (PC,...
    MOVEQ   #10,D0
    MOVEQ   #0,D4
    MOVE.L  D4,D5           ; D5=EXTENSIONWORD
    MOVEM.L D0-D1/A0,-(A7)
    MOVE.W  10(A5),D0
    MOVE.W  #1,(A4)+
    CLR.W   NEWOP
    BSR ADDBRANCH
    MOVEM.L (A7)+,D0-D1/A0
    MOVE.W  -(A4),D5
    CMP.W   #COM,(A3)+
    BNE iERROR5
    BSR ASM_REGID
    MOVE.B  (A3)+,D2
    CMP.B   #4,D2           ; ...,Dx
    BEQ.S   .12
    CMP.B   #5,D2
    BNE iERROR19
    BSET    #15,D5          ; ...,Ax
.12:    MOVE.B  (A3)+,D2
    BSR SKIP244
    AND.W   #%111,D2
    MOVEQ   #12,D4
    LSL.W   D4,D2
    ADD.W   D2,D5
.13:    MOVE.W  (A3)+,D2
    CMP.W   #26,D2
    BEQ.S   .13
    CMP.W   #18,D2
    BEQ .14
    CMP.W   #27,D2
    BNE iERROR0
    BSET    #11,D5
    BRA.S   .13
.14:    MOVE.W  D5,(A4)+
    RTS
.4: CMP.W   #LIBC,D0        ; LIBCALL(Ax)
    BNE iERROR19
    ADDQ.L  #2,A3
    MOVEQ   #0,D4
    MOVE.W  (A3)+,D4
    ADDQ.L  #8,A3
    EXT.L   D4
    BRA GL_BACK
    CMP.B   #6,(A3)+
    BNE iERROR19
    MOVE.B  (A3)+,D3
    AND.W   #%111,D3
    ADD.W   #%101000,D3
    MOVEQ   #5,D0
    RTS
;*-*
;; GrabValue

; EATS INTERIM CODE FOR TRUE VALUES + CALC
; -->D0=TRUE/FALSE WAS VALUE
; -->D1=RESULT
; TRASHES: D0-D4,A0

ASM_GRABVALUE:
    MOVEQ   #0,D1
    MOVEQ   #-1,D0          ; IF D0=-1 THEN NOVALUE ELSE CODENR
.XL:
    MOVEQ   #0,D2           ; D2<>0 --> NEGATE
    MOVE.W  (A3)+,D3
    CMP.W   #55,D3
    BEQ     .XL
    CMP.W   #8,D3           ; '-' SIGN ?
    BNE.S   .1
    MOVEQ   #-1,D2
.xl
    MOVE.W  (A3)+,D3
    CMP.W   #55,D3
    BEQ     .xl
    CMP.W   #VALUE,D3
    BNE.S   .X2
    BRA.S   .2
.1: CMP.W   #VALUE,D3       ; VALUE ?
    BNE.S   .X              ; sizeof or exit
.2: CMP.W   #-1,D0          ; SIZEOFBACK
    BNE.S   .3
    MOVEQ   #6,D0           ; ATLEAST ONE VALUE
.3: TST.L   D2
    BEQ.S   .4
    NEG.L   (A3)
.4: SUBQ.W  #6,D0
    LSL.W   #2,D0
    MOVE.L  .T(PC,D0.W),A0
    JMP (A0)

.T: DC.L    .MOVE,  .ADD,   .SUB,   .MUL,   .DIV
    DC.L    .AND,   .OR,    .LSL,   .LSR

.XB:
    MOVE.W  (A3)+,D0
    CMP.W   #11,D0
    BPL.S   .X3
    CMP.W   #7,D0
    BMI.S   .X3         ; SEE IF AND/OR, ELSE QUIT
.AOB:
    BRA.S   .XL
.X2:
    SUBQ.L  #4,A3
    CMP.W   #-1,D0
    BEQ.S   .5
    SUBQ.L  #2,A3
    RTS
.X: CMp.W   #LIBC,D3
    BEQ .LIB
    CMP.W   #IOFF+50,D3     ; check for sizeof
    BNE.S   .XNS
    CMP.W   #31,(A3)+       ; OBJECT
    BNE iERROR40
    MOVE.L  (A3)+,A0        ; WE USE A0=OBJ
    MOVE.W  4(A0),-(A3)
    BEQ iERROR40         ; NO SIZEOF IN CONST DECL. (SOB..)
    CLR.W   -(A3)
    BRA.S   .2
.LIB:
    ADDQ.L  #2,A3
    MOVE.L  D3,-(A7)
    MOVE.W  (A3)+,D3
    ADDQ.L  #4,A3
    EXT.L   d3
    MOVE.L  D3,(A3)
    MOVE.L  (A7)+,D3
    BRA     .2

.XNS:
    SUBQ.L  #2,A3           ; ASSUMES NEXT WORD ALREADY READ
    CMP.W   #-1,D0
    BEQ.S   .5
    SUBQ.L  #2,A3
.5: RTS


.X3:
    CMP.W   #IOFF+29,D0     ; CHECK FOR AND/OR
    BEQ.S   .AO
    CMP.W   #IOFF+30,D0
    BEQ.S   .AO
    CMP.W   #49,D0
    BEQ.S   .LSD            ; LOGICAL SHIFT [direction], not LSD ;)
    CMP.W   #50,D0
    BEQ.S   .LSD
    SUBQ.L  #2,A3
    RTS
.AO:
    SUB.W   #IOFF+18,D0
    BRA.S   .AOB
.LSD:
    SUB.W   #36,D0
    BRA.S   .AOB

.MOVE:
    MOVE.L  (A3)+,D1
    BRA .XB
.ADD:
    ADD.L   (A3)+,D1
    BRA .XB
.SUB:
    SUB.L   (A3)+,D1
    BRA .XB
.AND:
    AND.L   (A3)+,D1
    BRA .XB
.OR:
    OR.L    (A3)+,D1
    BRA .XB
.LSL:
    MOVE.L  (A3)+,D4
    LSL.L   D4,D1
    BRA .XB
.LSR:
    MOVe.L  (A3)+,D4
    LSR.L   D4,D1
    BRA .XB
.MUL:   MOVE.L  (A3)+,D4
    MOVE.L  D4,D2
    MOVE.L  D4,D3
    MULU    D1,D4
    SWAP    D3
    MULU    D1,D3
    SWAP    D3
    CLR.W   D3
    ADD.L   D3,D4
    SWAP    D1
    MULU    D1,D2
    SWAP    D2
    CLR.W   D2
    ADD.L   D2,D4
    MOVE.L  D4,D1
    BRA .XB

;.DIV:  MOVE.L  (A3)+,D4
;   MOVEQ   #32,D3
;   MOVEQ   #0,D2
;.D1:   SUB.L   D4,D2
;   BCC.B   .D2
;   ADD.L   D4,D2
;.D2:   ROXL.L  #1,D1
;   ROXL.L  #1,D2
;   DBF     D3,.D1
;   NOT.L   D1
;   BRA .XB

.DIV:   MOVE.L  D1,-(A7)
    MOVE.L  (A3)+,-(A7)
    TST.L   (A7)
    BEQ iERROR15
    JSR I_DIV
    ADDQ.L  #8,A7
    MOVE.L  D0,D1
    BRA .XB
;*-*
;*-*
;; PPC ASM
PASM_COMPILE:
    MOVEM.L A2/A5,-(A7)
    MOVE.W  #31,PASM_RC_BIT
    CLR.W   PASM_RC_FLG
    MOVE.L  (A3)+,D7
    MOVE.W  (A3)+,PASM_RC_FLG
    LSL.L   #2,D7
    LEA     PPC_INSJOB,A0
    MOVE.L  0(A0,D7),A5
.DO:MOVEQ   #0,D7
    MOVE.W  (A5)+,D7
    BEQ     .X
    SUBQ.L  #1,D7
    LSL.W   #2,D7
    LEA     .JOB(PC),A2
    MOVE.L  (A2,D7),A2
    JSR     (A2)
    BRA     .DO
.X: MOVEM.L (A7)+,A2/A5
    RTS
.JOB:
    DC.L    PASM_CODE,ASM_COMMA,PASM_ASKGPR,PASM_SETRC,PASM_DORC
    DC.L    PASM_GETSIMM,PASM_DISRC,PASM_GPRORZERO,PASM_REQUESTRC
    DC.L    PASM_NOTIMPL,PASM_GET5BIT,PASM_GETCRF,PASM_GETBIT
    DC.L    PASM_GETDBL5,PASM_GETFPR,PASM_GETCRFD,PASM_RELATIVE
    DC.L    PASM_GETSPR,PASM_GET4BIT,PASM_GETTB,PASM_GETXBIT
    DC.L    PASM_DBLGPR,PASM_SPLIT6BIT,PASM_LABEL,PASM_ABSOLUTE
PASM_RC_BIT:
    DC.W    31
PASM_RC_FLG:
    DC.W    0

;; Code
PASM_CODE:
    MOVEQ   #0,D7
    MOVE.W  (A5)+,D7
    SUBQ.L  #1,D7
    CLR.L   (A4)
.1: MOVEQ   #0,D0
    MOVE.W  (A5)+,D0
    MOVE.L  #31,D1
    SUB.W   (A5)+,D1
    LSL.L   D1,D0
    OR.L    D0,(A4)
    DBF     D7,.1
    ADDQ.L  #4,A4
    RTS
;*-*
;; Ask for GeneralPurposeRegister
PASM_ASKGPR:
    CMP.W   #47,(A3)+
    BNE     iERROR100
    MOVEQ   #0,D0
    MOVE.W  (A3)+,D0
    MOVEQ   #$3B,D1
    SUB.L   D1,D0
    BMI     iERROR100
    CMP.W   #$20,D0
    BPL     iERROR100
    moveq   #31,d1
    sub.w  (a5)+,d1
    lsl.l   d1,d0
    or.l    d0,-4(a4)
    rts
;*-*
;; Set RC
PASM_SETRC:
    MOVE.W  (A5)+,PASM_RC_BIT
    RTS
;*-*
;; Do RC
PASM_DORC:
    MOVE.W  PASM_RC_FLG(PC),D0
    AND.L   #1,D0
    MOVEQ   #31,D1
    SUB.W   PASM_RC_BIT(PC),D1
    LSL.L   D1,D0
    OR.L    D0,-4(A4)
    RTS
;*-*
;; Get Signed Immediate
PASM_GETSIMM:
    CMP.W   #$F00,(A3)+
    BNE     iERROR30
    BSR     ASM_GRABVALUE
    TST.L   D0
    BMI     iERROR30
    MOVE.L  D1,D0
    CHKW    D0,D1,iERROR93
    MOVEQ   #31,D7
    SUB.W   (A5)+,D7
    LSL.L   D7,D0
    OR.L    D0,-4(A4)
    RTS
;*-*
;; illegal RC
PASM_DISRC:
    TST.W   PASM_RC_FLG
    BNE iERROR101
    RTS
;*-*
;; Get GPR or zero
PASM_GPRORZERO:
    CMP.W   #VALUE,(A3)
    BEQ     .1
    CMP.W   #47,(A3)+
    BNE     iERROR100
    MOVEQ   #0,D0
    MOVE.W  (A3)+,D0
    SUB.L   #$3b,D0
    BMI     iERROR100
    CMP.L   #$20,D0
    BPL     iERROR100
.0:
    MOVEQ   #31,D1
    SUB.W   (A5)+,D1
    LSL.L   D1,D0
    OR.L    D0,-4(A4)
    RTS
.1: ADDQ.L  #2,A3
    MOVEQ   #0,D0
    TST.L   (A3)+
    BNE     iERROR100
    BRA     .0
;*-*
;; Request RC
PASM_REQUESTRC:
    TST.W   PASM_RC_FLG
    BEQ iERROR101
    RTS
;*-*
;; Not implemented
PASM_NOTIMPL:
    BRA iERROR87
;*-*
;; Get 5 bit immediate value
PASM_GET5BIT:
    CMP.W   #$F00,(A3)+
    BNE iERROR30
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR30
    CMP.L   #$20,D1
    BPL iERROR31
    MOVEQ   #31,D0
    SUB.W   (A5)+,D0
    LSL.L   D0,D1
    OR.L    D1,-4(A4)
    RTs
;*-*
;; Get CRx
PASM_GETCRF:
    CMP.W   #47,(A3)
    BEQ     .2
    CMP.W   #$F00,(A3)+
    BNE iERROR102
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR102
    CMP.L   #8,D1
    BPL iERROR102
    MOVEq   #31,d0
    sub.w   (a5)+,d0
    lsl.l   d0,d1
    or.l    d1,-4(a4)
    rts
.2: CMP.W   #91,2(a3)
    bpl .3
    addq.l  #4,a5
    rts
.3: addq.l  #2,a3
    moveq   #0,d1
    move.w  (a3)+,d1
    sub.l   #91,d1
    cmp.l   #8,d1
    bpl     iERROR102
    moveq   #31,d0
    sub.w   (a5)+,d0
    lsl.l   d0,d1
    or.l    d1,-4(a4)
    rts
;*-*
;; Get Bit
PASM_GETBIT:
    cmp.w   #$F00,(A3)+
    bne iERROR30
    bsr ASM_GRABVALUE
    tst.l   d0
    bmi iERROR30
    move.l  d1,d0
    and.l   #$FFFFFFFE,d0
    tst.l   d0
    bne iERROR31
    moveq   #31,d0
    sub.w   (a5)+,d0
    lsl.l   d0,d1
    or.l    d1,-4(a4)
    rts
;*-*
;; Get and double 5 bit immediate
PASM_GETDBL5:
    CMP.W   #$F00,(A3)+
    BNE iERROR30
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR30
    CMP.L   #$20,D1
    BPL iERROR31
    MOVE.L  D1,D2
    MOVEQ   #31,D0
    SUB.W   (A5)+,D0
    LSL.L   D0,D1
    OR.L    D1,-4(A4)
    MOVEQ   #31,D0
    SUB.W   (A5)+,D0
    LSL.L   D0,D2
    OR.L    D2,-4(A4)
    RTs
;*-*
;; Get Floating point register
PASM_GETFPR
    CMP.W   #47,(A3)+
    BNE     iERROR103
    MOVEQ   #0,D0
    MOVE.W  (A3)+,D0
    MOVEQ   #99,D1
    SUB.L   D1,D0
    BMI     iERROR103
    CMP.W   #$20,D0
    BPL     iERROR103
    moveq   #31,d1
    sub.w  (a5)+,d1
    lsl.l   d1,d0
    or.l    d0,-4(a4)
    rts
;*-*
;; Get CRFx
PASM_GETCRFD:
    CMP.W   #47,(A3)
    BEQ     .2
    CMP.W   #$F00,(A3)+
    BNE iERROR102
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR104
    CMP.L   #8,D1
    BPL iERROR104
    MOVEq   #31,d0
    sub.w   (a5)+,d0
    lsl.l   d0,d1
    or.l    d1,-4(a4)
    rts
.2: CMP.W   #131,2(a3)
    bpl .3
    addq.l  #4,a5
    rts
.3: addq.l  #2,a3
    moveq   #0,d1
    move.w  (a3)+,d1
    sub.l   #131,d1
    cmp.l   #8,d1
    bpl     iERROR104
    moveq   #31,d0
    sub.w   (a5)+,d0
    lsl.l   d0,d1
    or.l    d1,-4(a4)
    rts
;*-*
;; Get GPR or zero
PASM_RELATIVE:
    CMP.W   #VALUE,(A3)
    BNE     .brkt
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR73
    MOVE.L  D1,D0
    SWAP    D0
    TST.W   D0
    BNE iERROR93
    MOVEQ   #31,D0
    SUB.W   (A5),D0
    AND.W   2(A5),D1
    LSL.L   D0,D1
    OR.L    D1,-4(A4)
    CMP.W   #17,(A3)
    BNE     .XIT
.brkt:
    CMP.W   #17,(A3)+
    BNE iERROR19
    CMP.W   #47,(A3)+
    BNE     iERROR100
    MOVEQ   #0,D0
    MOVE.W  (A3)+,D0
    SUB.L   #$3b,D0
    BMI     iERROR100
    CMP.L   #$20,D0
    BPL     iERROR100
    MOVEQ   #31,D1
    SUB.W   4(A5),D1
    LSL.L   D1,D0
    OR.L    D0,-4(A4)
    CMP.W   #18,(A3)+
    BNE iERROR74
.XIT:
    ADDQ.L  #6,A5
    RTS
;*-*
;; Get SPR
PASM_GETSPR:
    CMP.W   #47,(A3)
    BEQ     .2
    CMP.W   #$F00,(A3)+
    BNE iERROR105
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR105
    CMP.L   #1024,D1
    BPL iERROR105
    MOVE.L  D1,D2
    AND.L   #%11111,D2
    LSR.L   #5,D1
    LSL.L   #5,D2
    OR.L    D2,D1
    MOVEq   #31,d0
    sub.w   (a5)+,d0
    lsl.l   d0,d1
    or.l    d1,-4(a4)
    rts
.2: CMP.W   #139,2(a3)
    bpl .3
    BRA iERROR105
.3: addq.l  #2,a3
    moveq   #0,d1
    move.w  (a3)+,d1
    sub.l   #139,d1
    cmp.l   #32,d1
    bpl     iERROR105
    LEA .TAB(PC),A0
    lsl.l   #1,d1
    move.w  (a0,d1),d1
    and.l   #$3FF,D1
    MOVE.L  D1,D2
    AND.L   #%11111,D2
    LSR.L   #5,D1
    LSL.L   #5,D2
    OR.L    D2,D1
    moveq   #31,d0
    sub.w   (a5)+,d0
    lsl.l   d0,d1
    or.l    d1,-4(a4)
    rts
.TAB:
    DC.W    001,008,009,018,019,022,025,026,027,272,273,274
    DC.W    275,280,282,287,528,529,530,531,532,533,534,535
    DC.W    536,537,538,539,540,541,542,543,1013
;*-*
;; Get 4 bit immediate value
PASM_GET4BIT:
    CMP.W   #$F00,(A3)+
    BNE iERROR30
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR30
    CMP.L   #$10,D1
    BPL iERROR31
    MOVEQ   #31,D0
    SUB.W   (A5)+,D0
    LSL.L   D0,D1
    OR.L    D1,-4(A4)
    RTs
;*-*
;; GetTimeBase
PASM_GETTB:
    CMP.W   #COM,(A3)
    BNE     .DEFTB
    ADDQ.L  #2,A3
    CMp.W   #47,(A3)+
    BNE     .VAL
    MOVEQ   #0,D1
    MOVE.W  (A3)+,D1
    SUB.L   #172,D1
    BMI     iERROR106
    CMP.W   #2,D1
    BPL     iERROR106
    ADD.L   #268,D1
.FINE:
    MOVE.L  D1,D0
    LSR.L   #5,D1
    AND.L   #31,d0
    LSL.L   #5,D0
    OR.L    D0,D1
    MOVEQ   #31,D0
    SUB.W   (A5)+,D0
    LSL.L   D0,D1
    OR.L    D1,-4(A4)
    RTS
.VAL:
    CMP.W   #VALUE,-2(A3)
    BNE     iERROR106
    MOVE.L  (A3)+,D1
    cmp.l   #268,d1
    bmi     iERROR106
    cmp.l   #270,d1
    bpl     iERROR106
    bra     .FINE
.DEFTB:
    MOVE.L  #268,D1
    bra     .FINE
;*-*
;; Get x bit immediate value
PASM_GETXBIT:
    CMP.W   #$F00,(A3)+
    BNE iERROR30
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR30
    MOVEQ   #0,D0
    MOVEQ   #0,D2
    MOVE.W  (A5)+,D2
    BSET    D2,D0
    CMP.L   D0,D1
    BPL iERROR31
    MOVEQ   #31,D0
    SUB.W   (A5)+,D0
    LSL.L   D0,D1
    OR.L    D1,-4(A4)
    RTs
;*-*
;; Get & double the GPR.
PASM_DBLGPR:
    CMP.W   #47,(A3)+
    BNE     iERROR100
    MOVEQ   #0,D0
    MOVE.W  (A3)+,D0
    MOVEQ   #$3B,D1
    SUB.L   D1,D0
    BMI     iERROR100
    CMP.W   #$20,D0
    BPL     iERROR100
    move.l  d0,d2
    moveq   #31,d1
    sub.w  (a5)+,d1
    lsl.l   d1,d0
    or.l    d0,-4(a4)
    moveq   #31,d1
    sub.w   (a5)+,d1
    lsl.l   d1,d2
    or.l    d2,-4(a4)
    rts
;*-*
;; Get & split 6 bit immediate value
PASM_SPLIT6BIT:
    CMP.W   #$F00,(A3)+
    BNE iERROR30
    BSR ASM_GRABVALUE
    TST.L   D0
    BMI iERROR30
    CMP.L   #$40,D1
    BPL iERROR31
    MOVE.L  D1,D2
    LSR.L   #5,D2
    MOVEQ   #31,D0
    SUB.W   (A5)+,D0
    LSL.L   D0,D2
    or.l    d2,-4(a4)
    AND.L   #31,D1
    MOVEQ   #31,d0
    sub.w   (a5)+,d0
    lsl.l   d0,d1
    OR.L    D1,-4(A4)
    RTs
;*-*
;; Get label
PASM_LABEL:
    CMP.W   #IDENT,(A3)+
    BNE     iERROR4
    MOVE.L  (A3)+,A1
    MOVe.W  10(A1),D0
    CLR.W   NEWOP
    BSR     ADDBRANCH
    RTS
;*-*
;; Get absolute
PASM_ABSOLUTE:
    CMP.W   #VALUE,(A3)+
    BNE     iERROR73
    MOVE.L  (A3)+,D0
    btst    #0,d0
    bne     iERROR0
    btst    #1,d0
    bne     iERROR0
    cmp.l   #$4000000,d0
    bpl     iERROR31
    or.l    d0,-4(a4)
    rts
;*-*
;; Get label 16bit
PASM_LAB16B:
    CMP.W   #IDENT,(A3)+
    BNE     iERROR4
    MOVE.L  (A3)+,A1
    MOVe.W  10(A1),D0
    CLR.W   NEWOP
    BSR     ADDBRANCH
    RTS
;*-*
;; Get absolute 16bit
PASM_ABS16B:
    CMP.W   #VALUE,(A3)+
    BNE     iERROR73
    MOVE.L  (A3)+,D0
    btst    #0,d0
    bne     iERROR0
    btst    #1,d0
    bne     iERROR0
    cmp.l   #$10000,d0
    bpl     iERROR31
    or.l    d0,-4(a4)
    rts
;*-*
;*-*

