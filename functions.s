;; Inlines
;; Table
INLINETAB:
    DC.L    .E,INL_MUL,INL_DIV,.E,.E,.E,.E,.E,.E,.E   ; 0-9
    DC.L    .E,.E,INL_LONG,INL_INT,INL_CHAR,INL_PLONG,INL_PINT,INL_PCHAR,.E,INL_CLEANUP   ; 10-9
    DC.L    .E,.E,INL_AND,INL_OR,INL_NOT,.E,.E,.E,.E,.E   ; 20-9
    DC.L    .E,.E,.E,.E,.E,.E,.E,.E,.E,.E   ; 30-9
    DC.L    .E,.E,.E,.E,.E,.E,.E,INL_MOUSEX,INL_MOUSEY,.E   ; 40-9
    DC.L    .E,.E,.E,.E,.E,.E,.E,INL_EVEN,INL_ODD,INL_EVAL   ; 50-9
    DC.L    .E,.E,.E,INL_ABS,INL_SHL,INL_SHR,.E,.E,.E,.E   ; 60-9
    DC.L    .E,.E,.E,.E,.E,INL_MSGCODE,INL_MSGQUAL,INL_MSGIADR,.E,INL_RNDQ   ; 70-9
    DC.L    INL_MOD,INL_EOR,.E,.E,.E,.E,.E,.E,.E,.E   ; 80-9
    DC.L    .E,.E,.E,.E,.E,.E,.E,.E,INL_FABS,INL_FFLOOR   ; 90-9
    DC.L    INL_FCEIL,INL_FSIN,INL_FCOS,INL_FTAN,INL_FEXP,INL_FLOG,INL_FPOW,INL_FSQRT,INL_FLOG10,.E   ; 100-9
    DC.L    .E,.E,.E,.E,.E,.E,.E,.E,INL_CAR,INL_CDR   ; 110-9
    DC.L    .E,.E,INL_FATAN,INL_FSINCOS,INL_FSINH,INL_FCOSH,INL_FTANH,INL_FTIEEE,INL_FFIEEE,INL_FASIN   ; 120-9
    DC.L    INL_FACOS,.E,.E,.E,.E,.E,.E,.E,.E,.E   ; 130-9
    DC.L    .E,.E,.E,.E,.E,.E,.E,.E,.E,.E   ; 140-9
    DC.L    .E,.E,.E,INL_GETA4,.E,INL_LSL,INL_LSR,.E,.E,.E   ; 150-9
    DC.L    .E,.E,.E,.E,.E,.E,.E,.E,.E,.E   ; 160-9
    DC.L    .E,.E,.E,.E,.E,.E,.E,.E,.E,.E   ; 170-9
;; ERROR
.E:
    JMP     ERROR0
;*-*
;*-*
;; GetA4
INL_GETA4:
    tst.l   LIBINFO
    BEQ .1
    JMP ERROR86
.1:
    MOVE.W  #$2879,(a4)+
    MOVE.L  A4STORAGEADR(PC),D0
    BEq ERROR89
    JSR     FORCEABSHERE
    RTS
;*-*
;; Long
INL_LONG:
    MOVE.L  A4,-(A7)
    MOVE.W  #8,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O:
    MOVe.L  (A7)+,a0
    move.l  a4,d0
    sub.l   a0,d0
    cmp.l   #2,d0
    beq     .O2
    cmp.l   #4,d0
    beq     .O4
    cmp.l   #6,d0
    beq     .O6
.NO:MOVE.W  #$2010,(a4)+
    RTS
.O2:cmp.w   #$91c8,(a0)
    bne     .NO
    move.l  #$20380000,(a0)+
    bra     INL_OPTI
.O4:cmp.w   #$307c,(a0)
    bne     .O41
    move.w  #$2038,(a0)+
    addq.l  #2,a0
    bra     INL_OPTI
.O41:
    move.w  (a0),d0
    and.l   #$FFF8,d0
    cmp.w   #$41e8,d0
    bne     .O42
    move.w  (a0),d0
    and.l   #$7,d0
    or.w    #$2028,d0
    move.w  d0,(a0)+
    addq.l  #2,a0
    bra     INL_OPTI
.O42:
    cmp.w   #$41fa,(a0)
    bne     .NO
    move.w  #$203a,(a0)+
    addq.l  #2,a0
    bra     INL_OPTI
.O6:cmp.w   #$207c,(a0)
    bne     .O61
    move.w  #$2039,(a0)+
    addq.l  #4,a0
    bra     INL_OPTI
.O61:
    cmp.w   #$41f9,(a0)
    bne     .NO
    move.w  #$2039,(a0)+
    addq.l  #4,a0
    bra     INL_OPTI
.C: move.l  d0,a0
;*-*
;; Int
INL_INT:
    MOVEQ   #$40,D0
    BSR     EA_GETEXP
    MOVE.W  #$3010,(a4)+
    move.w  #$48C0,(A4)+
    RTS
;*-*
;; Char
INL_CHAR:
    MOVEQ   #$40,D0
    BSR     EA_GETEXP
    MOVE.W  #$1010,(a4)+
    BSR     EXP_EXTD0
    RTS
;*-*
;; Mul
INL_MUL:
    MOVE.W  #16,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    JSR     EXP
.O2:mOVE.L  #$4C1F0c00,(a4)+    ; MULS.L    (A7)+,D1:D0
    RTS
.C: move.l  d0,-(a7)
.C2:move.l  d0,d1
;*-*
;; Div
INL_DIV:
    MOVE.W  #16,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    move.w  #2,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O2
    move.w  .C2,(a4)+
.O2:move.w  #$201f,(a4)+
    move.l  #$5bc149c1,(a4)+    ; SMI D1; EXTB.L D1
    MOVE.L  #$4C420C01,(a4)+    ; DIVS.L  D2,D1:D0
    RTS
.C: move.l  d0,-(a7)
.C2:move.l  d0,d2
;*-*
;; And
INL_AND:
    move.w  #16,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    jsr     EXP
    MOVE.W  #$C09f,(A4)+        ; AND.L     D1,D0
    RTS
.C: move.l  d0,-(a7)
;*-*
;; Or
INL_OR:
    move.w  #16,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    JSR     EXP
    MOVE.W  #$809f,(A4)+        ; OR.W      (a7)+,D0
    RTS
.C: move.l  d0,-(a7)
;*-*
;; Not
INL_NOT:
    JSR     EXP
    MOVE.W  #$4680,(A4)+        ; NOT.L     D0
    RTS
;*-*
;; PutLong
INL_PLONG:
    move.w  #16,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    jsr     EXP
    MOVE.W  #$205f,(A4)+        ; move.l    (a7)+,a0
    MOVE.W  #$2080,(A4)+        ; MOVE.L    D0,(A0)
    RTS
.C: move.l  d0,-(a7)
;*-*
;; PutInt
INL_PINT:
    move.w  #16,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    jsr     EXP
    MOVE.W  #$205f,(A4)+
    MOVE.W  #$3080,(A4)+        ; MOVE.W    D0,(A0)
    RTS
.C: move.l  d0,-(a7)
;*-*
;; PutChar
INL_PCHAR:
    move.w  #16,EAREQUEST
    JSR     EAEXP           ; MOVE.L    D0,-(A7)
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    jsr     EXP
    MOVe.W  #$205F,(a4)+        ; MOVE.L    (A7)+,A0
    MOVE.W  #$1080,(A4)+        ; MOVE.B    D0,(A0)
    RTS
.C: move.l  d0,-(a7)
;*-*
;; Eor
INL_EOR:
    move.w  #16,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    JSR     EXP
    MOVE.l  #$221fB380,(A4)+        ; EOR.L     D1,D0
    RTS
.C: move.l  d0,-(a7)

;*-*
;; Eval
INL_EVAL:
    move.w  #8,EAREQUEST
    JSR     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: MOVE.W  #$4E90,(A4)+        ; JSR       (A0)
    RTS
.C: move.l  d0,a0
;*-*
;; Even
INL_EVEN:
    JSR     EXP
    move.l  #$02000001,(a4)+
    move.w  #$57c0,(a4)+
    jsr     EXP_EXTD0
    RTS
;*-*
;; Odd
INL_ODD:
    JSR     EXP
    move.l  #$02000001,(a4)+
    move.w  #$56c0,(a4)+        ; SNE       D0
    bsr     EXP_EXTD0
    RTS
;*-*
;; Abs
INL_ABS:
    JSR     EXP
    MOVE.L  #$4A806a02,(a4)+        ; TST.L D0/BPL *+2
    MOVE.W  #$4480,(A4)+            ; NEG.L D0
    RTS
;*-*
;; Shl
INL_SHL:
    move.w  #16,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    move.w  #1,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O2
    move.w  .C2,(A4)+
.O2:MOVE.W  #$201F,(A4)+        ; MOVE.L    (A7)+,D0
    MOVE.W  #$E3A0,(A4)+        ; ASL.L     D1,D0
    RTS
.C: move.l  d0,-(a7)
.C2:move.l  d0,d1
;*-*
;; Shr
INL_SHR:
    move.w  #16,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    move.w  #1,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O2
    move.w  .C2,(A4)+
.O2:MOVE.W  #$201F,(A4)+        ; MOVE.L    (A7)+,D0
    MOVE.W  #$E2A0,(A4)+        ; ASR.L     D1,D0
    RTS
.C: move.l  d0,-(a7)
.C2:move.l  d0,d1
;*-*
;; Mod
INL_MOD:
    move.w  #16,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(A4)+
.O: CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    move.w  #2,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O2
    move.w  .C2,(A4)+
.O2:mOVE.W  #$221F,(A4)+        ; MOVE.L    (A7)+,D1
    move.l  #$5bc049c0,(a4)+    ; SMI D0; EXTB.L D0
    MOVE.L  #$4C421C00,(a4)+    ; DIVS.L    D2,D0:D1
    RTS                         ; D0=MODULO, D1=A/B
.C: move.l  d0,-(a7)
.C2:move.l  d0,d2
;*-*
;; CleanUp
INL_CLEANUP:
    jsr     EXP
    move.l  #$2940ffe4,(a4)+
    MOVE.L  #$206cFFE8,(A4)+
    move.w  #$4ed0,(a4)+
    RTS
;*-*
;; RndQ
INL_RNDQ:
    JSR     EXP
    MOVE.L  #$D0806206,(A4)+        ; ADD.L D0,D0 / BHI.S *+8
    MOVE.L  #$0A801d87,(A4)+        ; EORI.L    #$1d872b41,D0
    MOVE.W  #$2b41,(A4)+
    RTS
;*-*
;; MouseX
INL_MOUSEX:
    move.w  #8,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: move.w  #$7000,(a4)+
    MOVE.L  #$3028000E,(A4)+
;    move.w  #$48c0,(a4)+
    RTS
.C: move.l  d0,a0
;*-*
;; MouseY
INL_MOUSEY:
    move.w  #8,EAREQUEST
    jsr     EAEXP
    tst.l   d0
    bne     .O
    move.w  .C,(a4)+
.O: move.w  #$7000,(a4)+
    MOVE.L  #$3028000C,(A4)+
;    move.w  #$48c0,(a4)+
    RTS
.C: move.l  d0,a0
;*-*

;; Fabs
INL_FABS:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC8,(A4)+
    MOVE.L  #$4EAEFFCA,(A4)+
    RTS
;*-*
;; Floor
INL_FFLOOR:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC8,(A4)+
    MOVE.L  #$4EAEFFA6,(A4)+
    RTS
;*-*
;; Fceil
INL_FCEIL:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC8,(A4)+
    MOVE.L  #$4EAEFFA0,(A4)+
    RTS
;*-*
;; Fsin
INL_FSIN:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFDC,(A4)+
    RTS
;*-*
;; Fcos
INL_FCOS:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFD6,(A4)+
    RTS
;*-*
;; Ftan
INL_FTAN:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFD0,(A4)+
    RTS
;*-*
;; Fexp
INL_FEXP:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFB2,(A4)+
    RTS
;*-*
;; Flog
INL_FLOG:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFAC,(A4)+
    RTS
;*-*
;; Fpow
INL_FPOW:
    MOVE.L  #$F00,D0            ; move.l    d0,-(a7)
    BSR EA_GETEXP
    JSR ASM_COMMA
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.W  #$221F,(A4)+        ; move.l    (a7)+,d1
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFA6,(A4)+
    RTS
;*-*
;; Fsqrt
INL_FSQRT:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFA0,(A4)+
    RTS
;*-*
;; Flog10
INL_FLOG10:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFF82,(A4)+
    RTS
;*-*
;; Fatan
INL_FATAN:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFE2,(A4)+
    RTS
;*-*
;; FsinCos
INL_FSINCOS:
    MOVE.L  #$F00,D0            ; move.l    d0,-(a7)
    BSR EA_GETEXP
    JSR ASM_COMMA
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.W  #$205F,(A4)+        ; move.l    (a7)+,A0
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFCA,(A4)+
    RTS
;*-*
;; FsinH
INL_FSINH:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFC4,(A4)+
    RTS


    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR 60(A6)
    RTS
;*-*
;; FcosH
INL_FCOSH:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFbe,(A4)+
    RTS
;*-*
;; FtanH
INL_FTANH:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFFb8,(A4)+
    RTS
;*-*
;; Ftieee
INL_FTIEEE:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFF9A,(A4)+
    RTS
;*-*
;; Fieee
INL_FFIEEE:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFF94,(A4)+
    RTS
;*-*
;; Fasin
INL_FASIN:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFF8e,(A4)+
    RTS
;*-*
;; Facos
INL_FACOS:
    MOVEQ   #0,D0
    BSR EA_GETEXP
    MOVE.L  #$2C6CFFC4,(A4)+
    MOVE.L  #$4EAEFF88,(A4)+
    RTS
;*-*

;; MsgCode
INL_MSGCODE:
    MOVE.w  #$7000,(a4)+
    move.l  #$302cffbc,(a4)+
    RTS
;*-*
;; MsgQual
INL_MSGQUAL:
    move.w  #$7000,(a4)+
    move.l  #$302cffbe,(a4)+
    RTS
;*-*
;; MsgIaddr
INL_MSGIADR:
    MOVE.L  #$202cffb8,(a4)+
    RTS
;*-*

;; Car
INL_CAR:
    MOVEQ   #$40,D0
    BSR EA_GETEXP
    MOVE.W  #$2010,(a4)+
    RTS
;*-*
;; Cdr
INL_CDR:
    MOVEQ   #$40,D0
    BSR EA_GETEXP
    MOVE.L  #$20280004,(a4)+
    RTS
;*-*

;; Lsl
INL_LSL:
    MOVE.L  #$F00,D0
    BSR     EA_GETEXP
    CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    MOVE.L  #$200,D0
    BSR     EA_GETEXP
    MOVE.W  #$201F,(A4)+        ; MOVE.L    (A7)+,D0
    MOVE.W  #$E3A8,(A4)+        ; LSL.L     D1,D0
    RTS
;*-*
;; Lsr
INL_LSR:
    MOVE.L  #$F00,D0
    BSR     EA_GETEXP
    CMP.W   #COM,(A3)+
    BEQ     .1
    JMP     ERROR5
.1:
    MOVE.L  #$200,D0
    BSR     EA_GETEXP
    MOVE.W  #$201F,(A4)+        ; MOVE.L    (A7)+,D0
    MOVE.W  #$E2A8,(A4)+        ; LSR.L     D1,D0
    RTS
;*-*
;; Inline - optimized version end
INL_OPTI:
    move.l  a0,a4
    rts
;*-*
;*-*

;; Patcher main
PATCHER:
    MOVEM.L D0-A6,-(A7)
    LEA     EFUNCTAB,A6
    LEA     PATCHES,A5
    LEA     EFUNCFLAGSTAB,A3
.LOOP:
    CLR.W   .INL
    MOVE.L  (A5)+,D0
    BMI     .EXIT
    SUBQ.L  #1,D0
    MULU.W  #EFUNCENTRYSIZE,D0
    LEA     0(A6,D0),A4             ; function to be patched
    MOVE.L  (A5)+,D0
    BTST    #31,D0
    BEQ     .SK1
    MOVE.W  #-1,.INL
    BCLR    #31,D0
.SK1:
    LSL.W   #2,D0
    MOVE.L  .OFFS(PC,D0.W),A2
    JMP     (A2)
.OFFS:
    DC.L    .1,.2,.3,.4,.5,.6
.1: MOVE.L  (A5)+,D0
    CMP.W   OSVERSION,D0
    BGT     .BAD
    BRA     .CONT
.2: MOVEQ   #0,D0
    MOVE.W  ASMCPU,D0
    AND.L   (A5)+,D0
    BEQ     .BAD
    BRA     .CONT
.3: MOVE.L  (A5)+,D0
    AND.W   EFPU,D0
    BEQ     .BAD
    BRA     .CONT
.4: ADDQ.L  #4,A5
    BTST    #7,CODEPREFS
    BEQ     .BAD
    BRA     .CONT
.5: ADDQ.L  #4,A5
    BTST    #6,CODEPREFS
    BEQ     .BAD
    BRA     .CONT
.6: ADDQ.L  #4,A5
    BRA     .CONT
.BAD:
    ADDQ.L  #8,A5
    BRA     .LOOP
.CONT:
    TST.W   .INL
    BEQ     .CONT2
    BTST    #7,ICODEPREFS+3
    BEQ     .BAD
;    MOVE.L  #I_CODEEND,4(A4)
;    MOVE.L  #I_CODEEND,8(A4)
    MOVE.L  -12(A5),D0
    BCLR    #31,D0
    SUBQ.L  #1,D0
    LSL.W   #2,D0
    OR.L    #$100,0(A3,D0)
    ADDQ.L  #8,A5
    BRA     .LOOP
.CONT2:
    MOVE.L  (A5)+,4(A4)
    MOVE.L  (A5)+,8(A4)
    BRA     .LOOP
.EXIT:
    MOVEM.L (A7)+,d0-A6
    RTS
.INL:
    DC.W    0
;*-*

;;FUNCS

; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   ALL BUILDIN E FUNCTIONS                 ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;

EFUNCFLAGSTAB:
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ;  1-10
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 11-20
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 21-30
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 31-40
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 41-50
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 51-60
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 61-70
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 71-80
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 81-90
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0001,$0001,$0001,$0001 ; 91-100
    DC.l    $0001,$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0000 ; 101-110
    DC.l    $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000 ; 111-120
    DC.l    $0000,$0000,$0002,$0002,$0002,$0002,$0002,$0002,$0002,$0002 ; 121-130
    DC.l    $0002,$0004,$0004,$0004,$0004,$0004,$0004,$0004,$0004,$0004 ; 131-140
    DC.l    $0004,$0000,$0000,$0000,$0000,$0000,$0400,$0400,$0808,$0808 ; 141-150
    DC.l    $0000,$0000,$0000,$0100,$0000,$0000,$0000                   ; 151-160

; BITS: 0 = math                ; uses mathbas
;       1 = math                ; uses mathtrans
;       2 = OS 37+              ; OS
;       3 = OS 39+              ; OS
;       4 = 020+                ; CPU
;       5 = 040+                ; CPU
;       6 = 881+                ; FPU
;       7 = 040+                ; FPU
;       8 = INLINE              ; Inline instruction
;       9 = UTILLIB             ; uses utillib
;      10 = FILES               ; autoclose files
;      11 = POOLS               ; uses pools
;      12 = DOS                 ; dos.library
;      13 = INTUITION           ; intuition.library
;      14 = GRAPHICS            ; graphics.library
;      15 = stdout              ; stdout
;      16 = stdrast             ; stdrast
;      17 = conout              ; conout
;      18 = stdin               ; stdin

EFUNCENTRYSIZE=4*5
FOFF        = $100
NREFUNC     = 157

EFUNCTAB:
    DC.L    .1,     I_WRITEF,       I_MUL,          -1,0    ; = WRITEF SPECIAL ARGS
    DC.L    .2,     I_MUL,          I_DIV,          2,0     ; -> Mul 000+
    DC.L    .3,     I_DIV,          I_OPENW,        2,0     ; -> Div 000+
    DC.L    .4,     I_OPENW,        I_OPENS,        11,-1
    DC.L    .5,     I_OPENS,        I_MOUSE,        6,-1
    DC.L    .6,     I_MOUSE,        I_PLOT,         0,0
    DC.L    .7,     I_PLOT,         I_LINE,         3,1
    DC.L    .8,     I_LINE,         I_TEXTF,        5,1
    DC.L    .9,     I_TEXTF,        I_COLOR,        -3,0     ; NARGS*-1
    DC.L    .10,    I_COLOR,        I_SETRAST,      2,-1
    DC.L    .11,    I_SETRAST,      I_SETOUT,       1,0
    DC.L    .12,    I_SETOUT,       I_LONG,         1,0
    DC.L    .13,    I_LONG,         I_INT,          1,0
    DC.L    .14,    I_INT,          I_CHAR,         1,0
    DC.L    .15,    I_CHAR,         I_PUTLONG,      1,0
    DC.L    .16,    I_PUTLONG,      I_PUTINT,       2,0
    DC.L    .17,    I_PUTINT,       I_PUTCHAR,      2,0
    DC.L    .18,    I_PUTCHAR,      I_NEW,          2,0
    DC.L    .19,    I_NEW,          I_EXIT,         1,0
    DC.L    .20,    I_EXIT,         I_CLOSEW,       1,-1
    DC.L    .21,    I_CLOSEW,       I_CLOSES,       1,0
    DC.L    .22,    I_CLOSES,       I_AND,          1,0
    DC.L    .23,    I_AND,          I_OR,           2,0
    DC.L    .24,    I_OR,           I_NOT,          2,0
    DC.L    .25,    I_NOT,          I_BOOLGAD,      1,0
    DC.L    .26,    I_BOOLGAD,      I_SETTOPAZ,     8,0
    DC.L    .27,    I_SETTOPAZ,     I_STRCMP,       1,8
    DC.L    .28,    I_STRCMP,       I_STRCOPY,      3,$FF
    DC.L    .29,    I_STRCOPY,      I_STRADD,       3,$FF
    DC.L    .30,    I_STRADD,       I_STRLEN,       3,$FF
    DC.L    .31,    I_STRLEN,       I_ESTRLEN,      1,0
    DC.L    .32,    I_ESTRLEN,      I_ESTRMAX,      1,0
    DC.L    .33,    I_ESTRMAX,      I_STRING,       1,0
    DC.L    .34,    I_STRING,       I_RIGHTSTR,     1,0
    DC.L    .35,    I_RIGHTSTR,     I_MIDSTR,       3,0
    DC.L    .36,    I_MIDSTR,       I_STRINGF,      4,$FF
    DC.L    .37,    I_STRINGF,      I_VAL,          -2,0
    DC.L    .38,    I_VAL,          I_INSTR,        2,-1
    DC.L    .39,    I_INSTR,        I_TRIMSTR,      3,-1
    DC.L    .40,    I_TRIMSTR,      I_UPPERSTR,     1,0
    DC.L    .41,    I_UPPERSTR,     I_LOWERSTR,     1,0
    DC.L    .42,    I_LOWERSTR,     I_READSTR,      1,0
    DC.L    .43,    I_READSTR,      I_OUT,          2,0
    DC.L    .44,    I_OUT,          I_INP,          2,0
    DC.L    .45,    I_INP,          I_VERSION,      1,0
    DC.L    .46,    I_VERSION,      I_FILELENGTH,   1,0
    DC.L    .47,    I_FILELENGTH,   I_MOUSEX,       1,0
    DC.L    .48,    I_MOUSEX,       I_MOUSEY,       1,0
    DC.L    .49,    I_MOUSEY,       I_FREESTACK,    1,0
    DC.L    .50,    I_FREESTACK,    I_CTRLC,        0,0
    DC.L    .51,    I_CTRLC,        I_LIST,         0,0
    DC.L    .52,    I_LIST,         I_LISTCOPY,     1,0
    DC.L    .53,    I_LISTCOPY,     I_LISTADD,      3,$FF
    DC.L    .54,    I_LISTADD,      I_LISTCMP,      3,$FF
    DC.L    .55,    I_LISTCMP,      I_LISTLEN,      3,$FF
    DC.L    .56,    I_LISTLEN,      I_LISTMAX,      1,0
    DC.L    .57,    I_LISTMAX,      I_EVEN,         1,0
    DC.L    .58,    I_EVEN,         I_ODD,          1,0
    DC.L    .59,    I_ODD,          I_EVAL,         1,0
    DC.L    .60,    I_EVAL,         I_FORALL,       1,0
    DC.L    .61,    I_FORALL,       I_EXISTS,       3,0
    DC.L    .62,    I_EXISTS,       I_MAPLIST,      3,0
    DC.L    .63,    I_MAPLIST,      I_ABS,          4,0
    DC.L    .64,    I_ABS,          I_SHL,          1,0
    DC.L    .65,    I_SHL,          I_SHR,          2,0
    DC.L    .66,    I_SHR,          I_BOX,          2,0
    DC.L    .67,    I_BOX,          I_DISP,         5,1
    DC.L    .68,    I_DISP,         I_DISPL,        1,0
    DC.L    .69,    I_DISPL,        I_LINK,         1,0
    DC.L    .70,    I_LINK,         I_NEXT,         2,0
    DC.L    .71,    I_NEXT,         I_FORWARD,      1,0
    DC.L    .72,    I_FORWARD,      I_SETSTR,       2,0
    DC.L    .73,    I_SETSTR,       I_SETLIST,      2,0
    DC.L    .74,    I_SETLIST,      I_WAITMSG,      2,0
    DC.L    .75,    I_WAITMSG,      I_MSGCODE,      1,0
    DC.L    .76,    I_MSGCODE,      I_MSGQUAL,      0,0
    DC.L    .77,    I_MSGQUAL,      I_MSGIADR,      0,0
    DC.L    .78,    I_MSGIADR,      I_RND,          0,0
    DC.L    .79,    I_RND,          I_RNDQ,         1,0
    DC.L    .80,    I_RNDQ,         I_MOD,          1,0
    DC.L    .81,    I_MOD,          I_EOR,          2,0
    DC.L    .82,    I_EOR,          I_CAUSE,        2,0
    DC.L    .83,    I_CAUSE,        I_LISTITEM,     1,-1
    DC.L    .84,    I_LISTITEM,     I_NEWR,         2,0
    DC.L    .85,    I_NEWR,         I_SIGN,         1,0
    DC.L    .86,    I_SIGN,         I_PRINTF,       1,0
    DC.L    .87,    I_PRINTF,       I_WAITLEFTMOUSE,-1,0
    DC.L    .88,    I_WAITLEFTMOUSE,I_LEFTMOUSE,    1,0
    DC.L    .89,    I_LEFTMOUSE,    I_SETIN,        1,0
    DC.L    .90,    I_SETIN,        I_THROW,        1,0
    DC.L    .91,    I_THROW,        I_RETHROW,      2,0
    DC.L    .92,    I_RETHROW,      I_SELECTLIST,   0,0
    DC.L    .93,    I_SELECTLIST,   I_SETCOLOUR,    4,0
    DC.L    .94,    I_SETCOLOUR,    I_NEWM,         5,0
    DC.L    .95,    I_NEWM,         I_BOUNDS,       2,0
    DC.L    .96,    I_BOUNDS,       I_REALF,        3,0
    DC.L    .97,    I_REALF,        I_REALVAL,      3,0
    DC.L    .98,    I_REALVAL,      I_FABS,         1,0
    DC.L    .99,    I_FABS,         I_FFLOOR,       1,0
    DC.L    .100,   I_FFLOOR,       I_FCEIL,        1,0
    DC.L    .101,   I_FCEIL,        I_FSIN,         1,0
    DC.L    .102,   I_FSIN,         I_FCOS,         1,0
    DC.L    .103,   I_FCOS,         I_FTAN,         1,0
    DC.L    .104,   I_FTAN,         I_FEXP,         1,0
    DC.L    .105,   I_FEXP,         I_FLOG,         1,0
    DC.L    .106,   I_FLOG,         I_FPOW,         1,0
    DC.L    .107,   I_FPOW,         I_FSQRT,        2,0
    DC.L    .108,   I_FSQRT,        I_FLOG10,       1,0
    DC.L    .109,   I_FLOG10,       I_FASTDISPOSE,  1,0
    DC.L    .110,   I_FASTDISPOSE,  I_FASTNEW,      2,0
    DC.L    .111,   I_FASTNEW,      I_MIN,          1,0
    DC.L    .112,   I_MIN,          I_MAX,          2,0
    DC.L    .113,   I_MAX,          I_OSTRCMP,      2,0
    DC.L    .114,   I_OSTRCMP,      I_ASTRCOPY,     3,$FF
    DC.L    .115,   I_ASTRCOPY,     I_CELL,         3,$FF
    DC.L    .116,   I_CELL,         I_FREECELLS,    1,0
    DC.L    .117,   I_FREECELLS,    I_SETCHUNKSIZE, 0,0
    DC.L    .118,   I_SETCHUNKSIZE, I_CAR,          1,0
    DC.L    .119,   I_CAR,          I_CDR,          1,0
    DC.L    .120,   I_CDR,          I_CONS,         1,0
    DC.L    .121,   I_CONS,         I_FASTDISPOSELIST,0,0
    DC.L    .122,   I_FASTDISPOSELIST,I_FATAN,      1,0
    DC.L    .123,   I_FATAN,        I_FSINCOS,      1,0
    DC.L    .124,   I_FSINCOS,      I_FSINH,        2,0
    DC.L    .125,   I_FSINH,        I_FCOSH,        1,0
    DC.L    .126,   I_FCOSH,        I_FTANH,        1,0
    DC.L    .127,   I_FTANH,        I_FTIEEE,       1,0
    DC.L    .128,   I_FTIEEE,       I_FFIEEE,       1,0
    DC.L    .129,   I_FFIEEE,       I_FASIN,        1,0
    DC.L    .130,   I_FASIN,        I_FACOS,        1,0
    DC.L    .131,   I_FACOS,        I_DOMETHOD,     1,0
    DC.L    .132,   I_DOMETHOD,     I_DOMETHODA,    -1,0
    DC.L    .133,   I_DOMETHODA,    I_CRCMETHOD,    2,0
    DC.L    .134,   I_CRCMETHOD,    I_CRCMETHODA,   -2,0
    DC.L    .135,   I_CRCMETHODA,   I_DOSMETHOD,    3,0
    DC.L    .136,   I_DOSMETHOD,    I_DOSMETHODA,   -2,0
    DC.L    .137,   I_DOSMETHODA,   I_SET,          3,0
    DC.L    .138,   I_SET,          I_SETS,         -1,0
    DC.L    .139,   I_SETS,         I_GET,          3,0
    DC.L    .140,   I_GET,          I_GETS,         3,0
    DC.L    .141,   I_GETS,         I_CTRLD,        2,0
    DC.L    .142,   I_CTRLD,        I_CTRLE,        0,0
    Dc.L    .143,   I_CTRLE,        I_CTRLF,        0,0
    DC.L    .144,   I_CTRLF,        I_CHK,          0,0
    DC.L    .145,   I_CHK,          I_EOF,          1,0
    DC.L    .146,   I_EOF,          I_FOPEN,        1,0
    DC.L    .147,   I_FOPEN,        I_FCLOSE,       2,0
    DC.L    .148,   I_FCLOSE,       I_ALLOC,        1,0
    DC.L    .149,   I_ALLOC,        I_FREE,         1,0
    DC.L    .150,   I_FREE,         I_PUTF,         1,0
    DC.L    .151,   I_PUTF,         I_READB,        -2,0
    DC.L    .152,   I_READB,        I_WRITEB,       4,0
    DC.L    .153,   I_WRITEB,       I_SIZE,         4,0
    DC.L    .154,   I_CODEEND,      I_CODEEND+2,    0,0
    DC.L    .155,   I_SIZE,         I_LSL,          1,0
    DC.L    .156,   I_LSL,          I_LSR,          2,0
    DC.L    .157,   I_LSR,          I_CODEEND,      2,0
    DC.L    0,I_CODEEND,0,0

.1: DC.B    'WriteF',0
.2: DC.B    'Mul',0
.3: DC.B    'Div',0
.4: DC.B    'OpenW',0
.5: DC.B    'OpenS',0
.6: DC.B    'Mouse',0
.7: DC.B    'Plot',0
.8: DC.B    'Line',0
.9: DC.B    'TextF',0
.10:    DC.B    'Colour',0
.11:    DC.B    'SetStdRast',0
.12:    DC.B    'SetStdOut',0
.13:    DC.B    'Long',0
.14:    DC.B    'Int',0
.15:    DC.B    'Char',0
.16:    DC.B    'PutLong',0
.17:    DC.B    'PutInt',0
.18:    DC.B    'PutChar',0
.19:    DC.B    'New',0
.20:    DC.B    'CleanUp',0
.21:    DC.B    'CloseW',0
.22:    DC.B    'CloseS',0
.23:    DC.B    'And',0
.24:    DC.B    'Or',0
.25:    DC.B    'Not',0
.26:    DC.B    'Gadget',0
.27:    DC.B    'SetTopaz',0
.28:    DC.B    'StrCmp',0
.29:    DC.B    'StrCopy',0
.30:    DC.B    'StrAdd',0
.31:    DC.B    'StrLen',0
.32:    DC.B    'EstrLen',0
.33:    DC.B    'StrMax',0
.34:    DC.B    'String',0
.35:    DC.B    'RightStr',0
.36:    DC.B    'MidStr',0
.37:    DC.B    'StringF',0
.38:    DC.B    'Val',0
.39:    DC.B    'InStr',0
.40:    DC.B    'TrimStr',0
.41:    DC.B    'UpperStr',0
.42:    DC.B    'LowerStr',0
.43:    DC.B    'ReadStr',0
.44:    DC.B    'Out',0
.45:    DC.B    'Inp',0
.46:    DC.B    'KickVersion',0
.47:    DC.B    'FileLength',0
.48:    DC.B    'MouseX',0
.49:    DC.B    'MouseY',0
.50:    DC.B    'FreeStack',0
.51:    DC.B    'CtrlC',0
.52:    DC.B    'List',0
.53:    DC.B    'ListCopy',0
.54:    DC.B    'ListAdd',0
.55:    DC.B    'ListCmp',0
.56:    DC.B    'ListLen',0
.57:    DC.B    'ListMax',0
.58:    DC.B    'Even',0
.59:    DC.B    'Odd',0
.60:    DC.B    'Eval',0
.61:    DC.B    'ForAll',0
.62:    DC.B    'Exists',0
.63:    DC.B    'MapList',0
.64:    DC.B    'Abs',0
.65:    DC.B    'Shl',0
.66:    DC.B    'Shr',0
.67:    DC.B    'Box',0
.68:    DC.B    'Dispose',0
.69:    DC.B    'DisposeLink',0
.70:    DC.B    'Link',0
.71:    DC.B    'Next',0
.72:    DC.B    'Forward',0
.73:    DC.B    'SetStr',0
.74:    DC.B    'SetList',0
.75:    DC.B    'WaitIMessage',0
.76:    DC.B    'MsgCode',0
.77:    DC.B    'MsgQualifier',0
.78:    DC.B    'MsgIaddr',0
.79:    DC.B    'Rnd',0
.80:    DC.B    'RndQ',0
.81:    DC.B    'Mod',0
.82:    DC.B    'Eor',0
.83:    DC.B    'Raise',0
.84:    DC.B    'ListItem',0
.85:    DC.B    'NewR',0
.86:    DC.B    'Sign',0
.87:    DC.B    'PrintF',0
.88:    DC.B    'WaitLeftMouse',0
.89:    DC.B    'LeftMouse',0
.90:    DC.B    'SetStdIn',0
.91:    DC.B    'Throw',0
.92:    DC.B    'ReThrow',0
.93:    DC.B    'SelectList',0
.94:    DC.B    'SetColour',0
.95:    DC.B    'NewM',0
.96:    DC.B    'Bounds',0
.97:    DC.B    'RealF',0
.98:    DC.B    'RealVal',0
.99:    DC.B    'Fabs',0
.100:   DC.B    'Ffloor',0
.101:   DC.B    'Fceil',0
.102:   DC.B    'Fsin',0
.103:   DC.B    'Fcos',0
.104:   DC.B    'Ftan',0
.105:   DC.B    'Fexp',0
.106:   DC.B    'Flog',0
.107:   DC.B    'Fpow',0
.108:   DC.B    'Fsqrt',0
.109:   DC.B    'Flog10',0
.110:   DC.B    'FastDispose',0
.111:   DC.B    'FastNew',0
.112:   DC.B    'Min',0
.113:   DC.B    'Max',0
.114:   DC.B    'OstrCmp',0
.115:   DC.B    'AstrCopy',0
.116:   DC.B    'Cell',0
.117:   DC.B    'FreeCells',0
.118:   DC.B    'SetChunkSize',0
.119:   DC.B    'Car',0
.120:   DC.B    'Cdr',0
.121:   DC.B    '_Cons',0
.122:   DC.B    'FastDisposeList',0
.123:   DC.B    'Fatan',0
.124:   DC.B    'Fsincos',0
.125:   DC.B    'Fsinh',0
.126:   DC.B    'Fcosh',0
.127:   DC.B    'Ftanh',0
.128:   DC.B    'Ftieee',0
.129:   DC.B    'Ffieee',0
.130:   DC.B    'Fasin',0
.131:   DC.B    'Facos',0
.132:   DC.B    'DoMethod',0
.133:   DC.B    'DoMethodA',0
.134:   DC.B    'CoerceMethod',0
.135:   DC.B    'CoerceMethodA',0
.136:   DC.B    'DoSuperMethod',0
.137:   DC.B    'DoSuperMethodA',0
.138:   dc.b    'Set',0
.139:   dc.b    'Sets',0
.140:   dc.b    'Get',0
.141:   dc.b    'Gets',0
.142:   DC.B    'CtrlD',0
.143:   DC.B    'CtrlE',0
.144:   DC.B    'CtrlF',0
.145:   DC.B    'Chk',0
.146:   DC.B    'Eof',0
.147:   DC.B    'Fopen',0
.148:   DC.B    'Fclose',0
.149:   DC.B    'Alloc',0
.150:   DC.B    'Free',0
.151:   DC.B    'PutF',0
.152:   DC.B    'ReadB',0
.153:   DC.B    'WriteB',0
.154:   DC.B    'GetA4',0
.155:   DC.B    'Size',0
.156:   DC.B    'Lsl',0
.157:   DC.B    'Lsr',0
    EVEN
;*-*
;; FunctionsCode
; NOTE: ALL THIS CODE SAVES D3-D7/A4/A5/A7 BEFORE THRASHING


; P - w. Patch, U - Unused, F - Fixed/Optimized, I - Inline, O - obsolete
; e.g. PI = has an inline patch

;; WriteF                   P
I_WRITEF:
    TST.L   -8(A4)          ;STDIO
    BEQ .2
.3: LEA 8(A7),A1
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0.L),A0
    LEA     .1(PC),A2
    MOVE.L  4.W,A6
    MOVE.L  -64(A4),A3
    MOVE.L  A3,D2
    JSR     -522(A6)
    MOVE.L  -8(A4),D1
    MOVE.L  D2,A0
.5: TST.B   (A0)+
    BNE .5
    SUB.L   D2,A0
    MOVE.L  D3,A3           ; BACKUP
    MOVE.L  A0,D3
    SUBQ.L  #1,D3
    MOVE.L  -44(A4),A6
    JSR     -48(A6)
    MOVE.L  D3,D0
    MOVE.L  A3,D3
    RTS
.1: MOVE.B  D0,(A3)+        ; RDF DUMP
    RTS
.2: LEA .4(PC),A0
    MOVE.L  A0,D1           ;OPEN CON
    MOVE.L  #1006,D2
    MOVE.L  -44(A4),A6
    JSR -30(A6)
    MOVE.L  D0,-12(A4)
    MOVE.L  D0,-8(A4)
    TST.L   D0
    BNE.S   .3
    MOVEQ   #20,D0
    MOVE.L  D0,-28(A4)
    MOVE.L  -24(A4),A0
    JMP (A0)
.4: DC.B    "CON:0/11/640/80/Output",0
    EVEN
;*-*
;; Mul                      PI
I_MUL:
    MOVE.L  D3,A3
    MOVE.L  4(A7),D1
    MOVE.L  8(A7),D0
    MOVE.L  D0,D2
    MOVE.L  D0,D3
    MULU    D1,D0
    SWAP    D3
    MULU    D1,D3
    SWAP    D3
    CLR.W   D3
    ADD.L   D3,D0
    SWAP    D1
    MULU    D1,D2
    SWAP    D2
    CLR.W   D2
    ADD.L   D2,D0
    MOVE.L  A3,D3
    RTS
;*-*
;; Div                      PI
I_DIV:
;   MOVE.L  D3,A3
;   MOVE.L  8(A7),D1
;   MOVE.L  4(A7),D0
;   MOVEQ   #32,D3
;   MOVEQ   #0,D2
;.1:    SUB.L   D0,D2
;   BCC.B   .2
;   ADD.L   D0,D2
;.2:    ROXL.L  #1,D1
;   ROXL.L  #1,D2
;   DBF     D3,.1
;   NOT.L   D1
;   MOVE.L  D1,D0
;   MOVE.L  A3,D3
;   RTS


; maxon:

    MOVE.L  8(A7),D0
    MOVE.L  4(A7),D1

L6:      tst.l    d0
         bmi.s    L8
         tst.l    d1
         bpl.s    L9
         neg.l    d1
L7:      bsr.s    L9
         neg.l    d0
         rts

L8:      neg.l    d0
         tst.l    d1
         bpl.s    L7
         neg.l    d1
L9:      swap     d1
         tst.w    d1
         bne.s    L10
         move.l   d3,-(a7)
         swap     d1
         move.w   d1,d3
         move.w   d0,d2
         clr.w    d0
         swap     d0
         divu     d3,d0
         move.l   d0,d1
         swap     d0
         move.w   d2,d1
         divu     d3,d1
         move.w   d1,d0
         clr.w    d1
         swap     d1
         move.l   (a7)+,d3
         rts

L10:     swap     d1
E11:     movem.l  d3-d4,-(a7)
         moveq    #$1f,d2
         moveq    #$00,d3
         moveq    #$00,d4
L12:     asl.l    #$1,d3
         asl.l    #$1,d0
         roxl.l   #$1,d4
         cmp.l    d1,d4
         bcs.s    L13
         sub.l    d1,d4
         addq.l   #$1,d3
L13:     dbf      d2,L12
         move.l   d4,d1
         move.l   d3,d0
         movem.l  (a7)+,d3-d4
         rts
;*-*
;; OpenW                    F
I_OPENW:
    LEA .1(PC),A0
    LEA 4(A7),A1
    MOVE.L  (A1)+,48(A0)        ;4(A7)
    MOVE.L  (A1)+,18(A0)        ;8(A7)
    ADDQ.L  #2,A1
    MOVE.W  (A1)+,46(A0)        ;14(A7)
    MOVE.L  (a1)+,30(A0)        ;16(A7)
    MOVE.L  (A1)+,26(A0)        ;20(A7)
    MOVE.L  (A1)+,D0            ;24(A7)

    MOVEQ   #18,D1
    BSET    D1,D0
    MOVE.L  D0,14(A0)
    MOVE.L  (A1)+,10(A0)        ;28(A7)
    ADDQ.L  #2,A1
    MOVE.W  (A1)+,6(A0)         ;34(A7)
    ADDQ.L  #2,A1
    MOVE.W  (A1)+,4(A0)         ;38(A7)
    ADDQ.L  #2,A1
    MOVE.W  (A1)+,2(A0)         ;42(A7)
    ADDQ.L  #2,A1
    MOVE.W  (A1)+,(A0)          ;46(A7)
    MOVE.L  -48(A4),A6
    JSR     -$CC(A6)
    MOVE.L  D0,A0
    MOVE.L  A0,D0
    BEQ.S   .2
    MOVE.L  50(A0),D1
    MOVE.L  D1,-16(A4)
.2: RTS
.1: DC.W    0,0,0,0,-1
    DC.L    0,0,0,0,0,0,0
    DC.W    80,25,-1,-1,0,0,0
;*-*
;; OpenS
I_OPENS:
    LEA .1(PC),A0
    MOVE.L  4(A7),32(A0)
    MOVE.L  8(A7),20(A0)
    MOVE.W  14(A7),12(A0)
    MOVE.W  18(A7),8(A0)
    MOVE.W  22(A7),6(A0)
    MOVE.W  26(A7),4(A0)
    MOVE.L  -48(A4),A6
    JSR -198(A6)
    MOVE.L  D0,D1
    BEQ.S   .2
    ADD.L   #84,D1
    MOVE.L  D1,-16(A4)
.2: RTS
.1: DC.W    0,0,640,256,1,$0001,$8000,$100F
    DC.L    0,0,0,0,0
;*-*
;; Mouse                    F
I_MOUSE:
    MOVEQ   #0,D0
    BTST    #6,$BFE001
    BNE.S   .1
    MOVEQ   #1,D0
.1: LEA     $DFF016,A0
    MOVE.W  (A0),D1
    BTST    #10,D1
    BNE.S   .2
    BSET    #1,D0
.2: BTST    #8,D1
    BNE.S   .3
    BSET    #2,D0
.3: RTS
;*-*
;; Plot
I_PLOT:
    MOVE.L  -16(A4),A1
    MOVE.L  A1,D0
    BEQ.S   .1
    MOVE.L  -52(A4),A6
    MOVE.L  4(A7),D0
    JSR -342(A6)
    MOVE.L  -16(A4),A1
    MOVE.L  12(A7),D0
    MOVE.L  8(A7),D1
    JSR -324(A6)
.1: RTS
;*-*
;; Line                     F
I_LINE:
    MOVE.L  -16(A4),A1
    MOVE.L  A1,D0
    BEQ.S   .1
    LEA     4(A7),A3
    MOVE.L  -52(A4),A6
    MOVE.L  (A3)+,D0
    JSR -342(A6)
    MOVE.L  -16(A4),A1
    MOVE.L  (A3)+,D1
    MOVE.L  (A3)+,D0
    JSR -240(A6)
    MOVE.L  -16(A4),A1
    MOVE.L  (A3)+,D1
    MOVE.L  (A3)+,D0
    JSR -246(A6)
.1: RTS
;*-*
;; TextF                    F
I_TEXTF:
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0.L),A0
    LEA     8(A7),A1
    LEA     .3(PC),A2
    MOVE.L  4.W,A6
    MOVE.L  -64(A4),A3      ; BETTER!
    MOVE.L  A3,D2
    JSR     -522(A6)        ; RAWDOFMT

    MOVE.L  -16(A4),A1
    MOVE.L  A1,D0
    BEQ.S   .1
    MOVE.L  -52(A4),A6
    MOVE.L  4(A7),D0
    LEA     12(A7,D0),A2
    MOVE.L  (A2)+,D1
    MOVE.L  (A2)+,D0
    JSR -240(A6)            ; MOVE

    MOVE.L  -16(A4),A1
    MOVE.L  D2,A0
    MOVE.L  A0,A2
.2: TST.B   (A2)+
    BNE.S   .2
    MOVE.L  A2,D0
    SUB.L   A0,D0
    SUBQ.L  #1,D0
    MOVE.L  D0,D2
    JSR -60(A6)             ; TEXT

    MOVE.L  D2,D0
.1: RTS
.3: MOVE.B  D0,(A3)+        ;RDF DUMP
    RTS
;*-*
;; Color                    F
I_COLOR:
    MOVE.L  -16(A4),A1
    MOVE.L  A1,D2
    BEQ.S   .1
    MOVE.L  -52(A4),A6
    MOVE.L  8(A7),D0
    JSR     -342(A6)    ; SetAPen
    MOVE.L  D2,A1
    MOVE.L  4(A7),D0
    JSR     -348(A6)    ; SetBPen
.1: RTS
;*-*
;; SetRast
I_SETRAST:
    MOVE.L  -16(A4),D0
    MOVE.L  4(A7),D1
    BEQ.S   .1
    MOVE.L  D1,-16(A4)
.1: RTS
;*-*
;; SetOut
I_SETOUT:
    MOVE.L  -8(A4),D0
    MOVE.L  4(A7),D1
    BEQ.S   .1
    MOVE.L  D1,-8(A4)
.1: RTS
;*-*
;; Long                     I
I_LONG:
    MOVE.L  4(A7),A0
    MOVE.L  (A0),D0
    RTS
;*-*
;; Int                      I
I_INT:
    MOVE.L  4(A7),A0
    MOVEQ   #0,D0
    MOVE.W  (A0),D0
    RTS
;*-*
;; Char                     I
I_CHAR:
    MOVE.L  4(A7),A0
    MOVEQ   #0,D0
    MOVE.B  (A0),D0
    RTS
;*-*
;; PutLong                  I
I_PUTLONG:
    MOVE.L  8(A7),A0
    MOVE.L  4(A7),(A0)
    RTS
;*-*
;; PutInt                   I
I_PUTINT:
    MOVE.L  8(A7),A0
    MOVE.W  6(A7),(A0)
    RTS
;*-*
;; PutChar                  I
I_PUTCHAR:
    MOVE.L  8(A7),A0
    MOVE.B  7(A7),(A0)
    RTS
;*-*
;; New
I_NEW:
    MOVE.L  4(A7),D0
    MOVE.L  #$10000,D1
    ADDQ.L  #8,D0
    MOVE.L  D0,D2
    MOVE.L  4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.L  -20(A4),(A0)
    MOVE.L  D2,4(A0)
    MOVE.L  D0,-20(A4)
    ADDQ.L  #8,D0
.1: RTS
;*-*
;; CleanUp                  I
I_EXIT:
    MOVE.L  4(A7),-28(A4)
    MOVE.L  -24(A4),A0
    JMP (A0)
;*-*
;; CloseW
I_CLOSEW:
    MOVE.L  4(A7),A0
    MOVE.L  A0,D0
    BEQ.S   .1
    CLR.L   -16(A4)
    MOVE.L  -48(A4),A6
    JSR -72(A6)
.1: RTS
;*-*
;; CloseS
I_CLOSES:
    MOVE.L  4(A7),A0
    MOVE.L  A0,D0
    BEQ.S   .1
    CLR.L   -16(A4)
    MOVE.L  -48(A4),A6
    JSR -66(A6)
.1: RTS
;*-*
;; And                      I
I_AND:
    MOVE.L  8(A7),D0
    AND.L   4(A7),D0
    RTS
;*-*
;; Or                       I
I_OR:
    MOVE.L  8(A7),D0
    OR.L    4(A7),D0
    RTS
;*-*
;; Not                      I
I_NOT:
    MOVE.L  4(A7),D0
    NOT.L   D0
    RTS
;*-*
;; BoolGad
I_BOOLGAD:
    MOVE.L  32(A7),D2
    MOVE.L  D2,A1           ; A1,D2=GADGET
    MOVE.L  A1,A2
    MOVEQ   #28,D0
.XL:CLR.L   (A2)+
    DBRA    D0,.XL
    LEA 44(A1),A0       ; A0   =BORDER1
    LEA 16(A0),A2       ; A2   =INTUITEXT
    LEA 20(A2),A3       ; A3   =20 empty bytes
    MOVE.L  24(A7),40(A1)       ; --> INIT GADGET
    MOVE.W  18(A7),4(A1)
    MOVE.W  14(A7),6(A1)
    MOVE.W  10(A7),D0
    MOVE.W  D0,8(A1)
    MOVE.W  #12,10(A1)      ; D0,D1 = W,H
    SUBQ.W  #1,D0
    MOVEQ   #11,D1
    MOVE.W  #$0,12(A1)      ; FLAGS
    MOVE.W  #$1,14(A1)      ; ACTIVATEFLAGS
    MOVE.W  #$1,16(A1)      ; TYPEFLAGS
    BTST    #0,23(A7)
    BEQ.S   .5
    BSET    #0,14(A1)
.5: BTST    #1,23(A7)
    BEQ.S   .6
    BSET    #7,13(A1)
.6: MOVE.L  A2,26(A1)
    MOVE.L  A0,18(A1)
    MOVE.W  D0,8(A3)        ; --> INIT VECTORZ
    MOVE.W  D0,12(A3)
    MOVE.W  D1,6(A3)
    MOVE.W  D1,10(A3)
    MOVE.L  4(A7),D0
    MOVE.L  D0,12(A2)       ; --> INIT INTUITEXT
    MOVE.L  #$01000100,(A2)
    MOVE.W  #$2,6(A2)
    MOVE.L  D0,A6
.7: TST.B   (A6)+
    BNE.S   .7
    SUB.L   D0,A6
    SUBQ.L  #1,A6
    MOVE.L  8(A7),D1
    MOVE.L  A6,D0
    LSL.W   #3,D0
    SUB.L   D0,D1
    LSR.W   #1,D1
    MOVE.W  D1,4(A2)
    MOVEQ   #0,D0
    MOVE.L  D0,(A0)         ; --> INIT BORDER
    MOVE.L  #$01000005,4(A0)
    MOVE.L  A3,8(A0)
    LEA .3(PC),A3       ; --> TOPAZ
    LEA 8(A3),A0
    MOVE.L  A0,(A3)
    MOVE.L  A3,8(A2)
    MOVE.L  28(A7),A0       ; --> LINK GLIST
    MOVE.L  A0,D0
    BEQ.S   .2
    MOVE.L  (A0),D0         ; NEXTNODE OF PRED
    MOVE.L  A1,(A0)         ; HANG ONTO PRED
    MOVE.L  D0,(A1)         ; CONNECT NEXTNODE
.2: MOVE.L  D2,D0
    ADD.L   #120,D0
    RTS
.3: DC.L    0,$80060
.4: DC.B    "topaz.font",0,0
;*-*
;; SetTopaz
I_SETTOPAZ:
    MOVE.L  -52(A4),A6
    LEA .2(PC),A0
    LEA .3(PC),A1
    MOVE.L  A1,(A0)
    MOVE.W  6(A7),4(A0)
    JSR -72(A6)         ; OPENFONT
    TST.L   D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.L  -16(A4),A1
    MOVE.L  A1,D0
    BEQ.S   .1
    JSR -66(A6)         ; SETFONT
.1: RTS
.2: DC.L    0,$80060
.3: DC.B    "topaz.font",0,0
;*-*
;; StrCmp
I_STRCMP:
    MOVEQ   #0,D0
    MOVE.L  12(A7),A0
    MOVE.L  8(A7),A1
    MOVE.W  6(A7),D0
    ADDQ.L  #1,D0
.1: SUBQ.L  #1,D0
    BEQ.S   .3
    CMPM.B  (A0)+,(A1)+
    BNE.S   .2
    CMP.B   #0,-1(A0)
    BNE.S   .1
.3: MOVEQ   #-1,D0
    RTS
.2: MOVEQ   #0,D0
    RTS
;*-*
;; StrCopy
I_STRCOPY:
    MOVEQ   #0,D0
    MOVE.W  6(A7),D0        ; LEN
    MOVE.L  8(A7),A1        ; STR
    MOVE.L  12(A7),A0       ; ESTR
    MOVE.L  A0,A2
    MOVEQ   #0,D1
    MOVE.W  -4(A0),D1
    CMP.L   D0,D1
    BPL.S   .1
    MOVE.L  D1,D0
.1: MOVE.L  D0,D2
    BEQ.S   .4
    SUBQ.L  #1,D0           ; D0=COPYLEN
.2: MOVE.B  (A1)+,(A0)+
    BEQ.S   .3
    DBRA    D0,.2
    MOVE.B  #0,(A0)+
.3: ADDQ.W  #1,D0
    SUB.W   D0,D2
    MOVE.W  D2,-2(A2)
    MOVE.L  A2,D0
.4: RTS
;*-*
;; StrAdd
I_STRADD:
    MOVE.L  D3,A3
    MOVEQ   #0,D0
    MOVE.W  6(A7),D0        ; LEN
    MOVE.L  8(A7),A1        ; STR
    MOVE.L  12(A7),A0       ; ESTR
    MOVEQ   #0,D3
    MOVE.W  -2(A0),D3
    MOVE.L  A0,A2
    MOVEQ   #0,D1
    MOVE.W  -4(A0),D1
    SUB.L   D3,D1
    ADD.L   D3,A0
    CMP.L   D0,D1
    BPL.S   .1
    MOVE.L  D1,D0
.1: MOVE.W  D0,D2
    BEQ.S   .4
    SUBQ.L  #1,D0           ; D0=COPYLEN
.2: MOVE.B  (A1)+,(A0)+
    BEQ.S   .3
    DBRA    D0,.2
    MOVE.B  #0,(A0)+
.3: ADDQ.W  #1,D0
    SUB.W   D0,D2
    ADD.W   D3,D2
    MOVE.W  D2,-2(A2)
.4: MOVE.L  A3,D3
    MOVE.L  A2,D0
    RTS
;*-*
;; StrLen
I_STRLEN:
    MOVE.L  4(A7),A0
    MOVE.L  A0,D1
.1: TST.B   (A0)+
    BNE.S   .1
    SUBQ.L  #1,A0
    MOVE.L  A0,D0
    SUB.L   D1,D0
    RTS
;*-*
;; EstrLen
I_ESTRLEN:
    MOVE.L  4(A7),A0
    MOVEQ   #0,D0
    MOVE.W  -2(A0),D0
    RTS
;*-*
;; EstrMax
I_ESTRMAX:
    MOVE.L  4(A7),A0
    MOVEQ   #0,D0
    MOVE.W  -4(A0),D0
    RTS
;*-*
;; String
I_STRING:
    MOVE.L  D3,A3
    MOVE.L  4(A7),D0
    CMP.L   #$7FF0,D0
    BPL.S   .1
    MOVE.L  D0,D3
    ADD.L   #17,D0          ; 8 (LIST) + 4 (LINK) + 4 (LENGTHS) + 1 (ZERO)
    MOVE.L  #$10000,D1
    MOVE.L  D0,D2
    MOVE.L  4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.W  D3,12(A0)
    MOVE.L  -20(A4),(A0)
    MOVE.L  D2,4(A0)
    MOVE.L  D0,-20(A4)
    ADDQ.L  #8,D0
    ADDQ.L  #8,D0
    MOVE.L  A3,D3
    RTS
.1: MOVEQ   #0,D0
    MOVE.L  A3,D3
    RTS
;*-*
;; RightStr
I_RIGHTSTR:
    MOVEQ   #0,D0
    MOVE.L  12(A7),A0
    MOVE.L  8(A7),A1
    MOVE.W  -2(A1),D0
    SUB.L   4(A7),D0
    TST.L   D0
    BPL.S   .4
    MOVEQ   #0,D0
.4: ADD.L   D0,A1
    MOVE.L  #$FFFF,D0
    MOVE.L  A0,A2
    MOVEQ   #0,D1
    MOVE.W  -4(A0),D1
    CMP.L   D0,D1
    BPL.S   .1
    MOVE.L  D1,D0
.1: MOVE.L  D0,D2
    SUBQ.L  #1,D0           ; D0=COPYLEN
.2: MOVE.B  (A1)+,(A0)+
    BEQ.S   .3
    DBRA    D0,.2
    MOVE.B  #0,(A0)+
.3: ADDQ.W  #1,D0
    SUB.W   D0,D2
    MOVE.W  D2,-2(A2)
    MOVE.L  A2,D0
    RTS
;*-*
;; MidStr
I_MIDSTR:
    MOVEQ   #0,D0
    MOVE.W  6(A7),D0        ; LEN
    MOVE.L  12(A7),A1       ; STR
    ADD.L   8(A7),A1        ; +OFFSET
    MOVE.L  16(A7),A0       ; ESTR
    MOVE.L  A0,A2
    MOVEQ   #0,D1
    MOVE.W  -4(A0),D1
    CMP.L   D0,D1
    BPL.S   .1
    MOVE.L  D1,D0
.1: MOVE.L  D0,D2
    SUBQ.L  #1,D0           ; D0=COPYLEN
.2: MOVE.B  (A1)+,(A0)+
    BEQ.S   .3
    DBRA    D0,.2
    MOVE.B  #0,(A0)+
.3: ADDQ.W  #1,D0
    SUB.W   D0,D2
    MOVE.W  D2,-2(A2)
    MOVE.L  A2,D0
    RTS
;*-*
;; StringF
I_STRINGF:
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0.L),A0       ; formatstr
    LEA 8(A7),A1        ; argarray
    LEA     .1(PC),A2       ; putfun
    MOVE.L  4.W,A6
    ;MOVE.L  276(A6),A3
    ;MOVE.L  58(A3),A3
    MOVE.L  -64(A4),A3      ; BETTER!
    MOVE.L  A3,D2           ; both bottom OWN stack
    JSR     -522(A6)
    MOVE.L  D2,A1
    MOVE.L  4(A7),D0
    MOVE.L  12(A7,D0.L),A0      ; ESTR
    MOVE.L  A0,A2
    MOVEQ   #0,D0
    MOVE.W  -4(A0),D0
    MOVE.L  D0,D2
    SUBQ.L  #1,D0           ; D0=COPYLEN
.2: MOVE.B  (A1)+,(A0)+
    BEQ.S   .3
    DBRA    D0,.2
    MOVE.B  #0,(A0)+
.3: ADDQ.W  #1,D0
    SUB.W   D0,D2
    MOVE.W  D2,-2(A2)
    MOVE.L  D2,D1
    EXT.L   D1
    MOVE.L  A2,D0
    RTS
.1: MOVE.B  D0,(A3)+        ;RDF DUMP
    RTS
;*-*
;; Val
I_VAL:
    MOVEM.L D3-D6,-(A7)     ; 16 OFF STACK
    MOVE.L  24(A7),A2
    MOVE.L  20(A7),D5       ; D5=READLENADR
    MOVEQ   #0,D6           ; D6=MINUSFLAG
.5: MOVE.B  (A2)+,D0        ; reg d0,a2
    BEQ VALER
    CMP.B   #33,D0
    BMI.S   .5
    SUBQ.L  #1,A2
    CMP.B   #"-",(A2)
    BNE.S   .NM
    ADDQ.L  #1,A2
    MOVEQ   #-1,D6
.NM:    CMP.B   #'$',(A2)
    BEQ HEX
    CMP.B   #'%',(A2)
    BEQ BIN
    LEA 11(A2),A1       ; reg d0-d3,a0-a3
    MOVE.L  A2,A0
.4: MOVE.B  (A2)+,D0
    CMP.B   #58,D0
    BPL.S   .6
    CMP.B   #48,D0
    BPL.S   .4
.6: SUBQ.L  #1,A2
    MOVE.L  A2,D4           ; D4=TOREADLEN
    CMPA.L  A2,A0
    BEQ.S   VALER
    MOVEQ   #0,D0           ; RESULT
    CMPA.L  A1,A2
    BPL.S   VALER
    LEA .2(PC),A3
.1: MOVEQ   #0,D3
    MOVE.B  -(A2),D3
    MOVE.L  (A3)+,D2
    SUB.B   #48,D3
    MOVE.L  D3,D1
    MULU    D2,D3
    SWAP    D2
    MULU    D2,D1
    SWAP    D1
    ADD.L   D1,D3
    ADD.L   D3,D0
    BCS.S   VALER
    CMPA.L  A0,A2
    BNE.S   .1
    BRA.S   ISIND0
.2: DC.L    1,10,100,1000,10000,100000,1000000
    DC.L    10000000,100000000,1000000000
ISIND0: SUB.L   24(A7),D4
    TST.L   D5
    BEQ.S   .NP
    MOVE.L  D5,A0
    MOVE.L  D4,(A0)
.NP:    TST.L   D6
    BEQ.S   .NN
    NEG.L   D0
.NN:    MOVE.L  D4,D1
    MOVEM.L (A7)+,D3-D6
    RTS
VALER:  MOVEQ   #0,D0
    MOVEQ   #0,D1
    TST.L   D5
    BEQ.S   .NP
    MOVE.L  D5,A0
    CLR.L   (A0)
.NP:    MOVEM.L (A7)+,D3-D6
    RTS
HEX:    ADDQ.L  #1,A2
    MOVE.L  A2,A0           ; reg d0-d3,a0,a2
    MOVE.L  A0,D3
    MOVEQ   #0,D1           ; RESULT
    MOVEQ   #0,D2           ; #OF SHIFTS
.4: MOVE.B  (A0)+,D0
    CMP.B   #"G",D0
    BPL.S   .2B
    CMP.B   #"0",D0
    BMI.S   .1
    CMP.B   #"9"+1,D0
    BPL.S   .2
    BRA.S   .4
.1: SUBQ.L  #1,A0
    MOVE.L  A0,D4           ; SET TOREADLEN
    CMP.L   A0,D3
    BEQ.S   VALER
.3: CMP.L   A0,D3
    BEQ.S   .5
    MOVEQ   #0,D0
    MOVE.B  -(A0),D0
    CMP.B   #"A",D0
    BPL.S   .10
    SUB.B   #"0",D0
.11:    CMP.W   #32,D2
    BEQ VALER
    LSL.L   D2,D0
    ADD.L   D0,D1
    ADDQ.L  #4,D2
    BRA.S   .3
.5: MOVE.L  D1,D0
    BRA.S   ISIND0
.2: CMP.B   #"A",D0
    BPL.S   .4
    BRA.S   .1
.2B:    CMP.B   #"a",D0
    BMI.S   .1
    CMP.B   #"g",D0
    BPL.S   .1
    BRA.S   .4
.10:    CMP.B   #"a",D0
    BPL.S   .10B
    SUB.B   #55,D0
    BRA.S   .11
.10B:   SUB.B   #55+32,D0
    BRA.S   .11
BIN:    ADDQ.L  #1,A2
    MOVE.L  A2,A0           ; reg d0-d3,a0,a2
    MOVE.L  A0,D3
    MOVEQ   #0,D1           ; RESULT
    MOVEQ   #0,D2           ; BITNUM
.4: MOVE.B  (A0)+,D0
    CMP.B   #"1",D0
    BEQ.S   .4
    CMP.B   #"0",D0
    BEQ.S   .4
.1: SUBQ.L  #1,A0
    MOVE.L  A0,D4           ; SET TOREADLEN
    CMP.L   A0,D3
    BEQ VALER
.3: CMP.L   A0,D3
    BEQ.S   .5
    MOVEQ   #0,D0
    MOVE.B  -(A0),D0
    CMP.B   #"0",D0
    BEQ.S   .10
    BSET    D2,D1
.10:    CMP.W   #32,D2
    BEQ VALER
    ADDQ.L  #1,D2
    BRA.S   .3
.5: MOVE.L  D1,D0
    BRA ISIND0
;*-*
;; InStr
I_INSTR:
    MOVE.L  8(A7),A1        ; STRING TO FIND
    MOVE.L  12(A7),A0       ; THE STRING
    MOVE.L  A0,D2
    ADD.L   4(A7),A0
    MOVE.B  (A1)+,D0        ; FIRST CHAR
.1: MOVE.B  (A0)+,D1
    BEQ.S   .2
    CMP.B   D0,D1
    BNE.S   .1
    MOVE.L  A0,A2
    MOVE.L  A1,A3
.3: MOVE.B  (A3)+,D1
    BEQ.S   .4
    CMP.B   (A2)+,D1
    BNE.S   .1
    BEQ.S   .3
.4: SUBQ.L  #1,A0
    MOVE.L  A0,D0
    SUB.L   D2,D0
    RTS
.2: MOVEQ   #-1,D0
    RTS
;*-*
;; TrimStr
I_TRIMSTR:
    MOVE.L  4(A7),A2
    MOVEQ   #0,D0
.1: MOVE.B  (A2)+,D0
    BEQ.S   .2
    CMP.W   #33,D0
    BMI.S   .1
.2: SUBQ.L  #1,A2
    MOVE.L  A2,D0
    RTS
;*-*
;; UpperStr
I_UPPERSTR:
    MOVE.L  4(A7),A0
    MOVE.L  A0,A1
.1: MOVE.B  (A0)+,D0
    BEQ.S   .2
    CMP.B   #97,D0
    BMI.S   .1
    CMP.B   #123,D0
    BPL.S   .1
    SUB.B   #32,D0
    MOVE.B  D0,-1(A0)
    BRA.S   .1
.2: MOVE.L  A1,D0
    RTS
;*-*
;; LowerStr
I_LOWERSTR:
    MOVE.L  4(A7),A0
    MOVE.L  A0,A1
.1: MOVE.B  (A0)+,D0
    BEQ.S   .2
    CMP.B   #91,D0
    BPL.S   .1
    CMP.B   #65,D0
    BMI.S   .1
    ADD.B   #32,D0
    MOVE.B  D0,-1(A0)
    BRA.S   .1
.2: MOVE.L  A1,D0
    RTS
;*-*
;; ReadStr
I_READSTR:
    MOVEM.L D3/D4/D6/D7,-(A7)
    MOVE.L  20(A7),A2       ; ESTR
    MOVE.L  A2,D4
    MOVE.L  24(A7),D7       ; FH
    MOVEQ   #0,D6
    MOVE.W  -4(A2),D6       ; MAXLEN
    ADD.L   A2,D6
    MOVE.L  -44(A4),A6
    MOVEQ   #1,D3
.1: CMP.L   D6,A2
    BEQ.S   .4
    MOVE.L  D7,D1
    MOVE.L  A2,D2
    ADDQ.L  #1,A2
    JSR -42(A6)
    CMP.W   #1,D0
    BMI.S   .5
    CMP.B   #10,-1(A2)
    BNE.S   .1
    MOVEQ   #0,D0
    SUBQ.L  #1,A2
.4: MOVE.L  A2,D1
    MOVE.B  #0,(A2)+
    SUB.L   D4,D1
    MOVE.L  D4,A0
    MOVE.W  D1,-2(A0)
    MOVEM.L (A7)+,D3/D4/D6/D7
    RTS
.5: SUBQ.L  #1,A2
    MOVEQ   #-1,D0
    BRA.S   .4
;*-*

;; Out
I_OUT:
    MOVE.L  D3,A3
    MOVE.L  8(A7),D1
    LEA 7(A7),A0
    MOVE.L  A0,D2
    MOVEQ   #1,D3
    MOVE.L  -44(A4),A6
    JSR -48(A6)
    MOVE.L  A3,D3
    RTS
;*-*
;; Inp
I_INP:
    MOVE.L  D3,A3
    MOVE.L  4(A7),D1
    MOVEQ   #1,D3
    LEA 4(A7),A0
    MOVE.L  A0,D2
    MOVE.L  -44(A4),A6
    JSR -42(A6)
    MOVE.L  A3,D3
    TST.L   D0
    BEQ.S   .1
    MOVEQ   #0,D0
    MOVE.B  4(A7),D0
    RTS
.1: MOVEQ   #-1,D0
    RTS
;*-*
;; Version
I_VERSION:
    MOVE.L  4(A7),D0        ; V. REQUESTED
    MOVE.L  4.W,A6
    MOVE.W  20(A6),D1       ; V. CURRENTLY RUNNING
    CMP.W   D0,D1
    BMI.S   .1
    MOVEQ   #-1,D0          ; TRUE=RIGHT KICK
    RTS
.1: MOVEQ   #0,D0           ; FALSE
    RTS
;*-*
;; FileLength
I_FILELENGTH:
    MOVEM.L D4/D6/D7,-(A7)
    MOVE.L  16(A7),D1
    MOVEQ   #-2,D2
    MOVE.L  -44(A4),A6
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
    MOVEM.L (A7)+,D4/D6/D7
    RTS
.3: MOVE.L  D7,D1           ; UNLOCK
    JSR -90(A6)
    RTS
.2: BSR.S   .3
.1: MOVEQ   #-1,D0
    MOVEM.L (A7)+,D4/D6/D7
    RTS
;*-*
;; MouseX                   I
I_MOUSEX:
    MOVEQ   #0,D0
    MOVE.L  4(A7),A0
    MOVE.W  14(A0),D0
    RTS
;*-*
;; MouseY                   I
I_MOUSEY:
    MOVEQ   #0,D0
    MOVE.L  4(A7),A0
    MOVE.W  12(A0),D0
    RTS
;*-*
;; FreeStack
I_FREESTACK:
    MOVE.L  A7,D0
    SUB.L   -64(A4),D0
    SUB.L   #1000,D0
    RTS
;*-*
;; CtrlC
I_CTRLC:
    MOVEQ   #0,D0
    MOVEQ   #0,D1
    MOVE.L  4.W,A6
    JSR -306(A6)
    BTST    #12,D0          ; NOTE: 13=CTRLD ETC.
    BEQ.S   .1
    MOVEQ   #0,D0
    MOVE.L  #4096,D1
    JSR -306(A6)
    MOVEQ   #-1,D0
    RTS
.1: MOVEQ   #0,D0           ; D0<--TRUE/FALSE
    RTS
;*-*

;; List
I_LIST:
    MOVE.L  D3,A3
    MOVE.L  4(A7),D0
    CMP.L   #$7FF0,D0
    BPL.S   .1
    MOVE.L  D0,D3           ; LEN
    LSL.L   #2,D0
    ADD.L   #16,D0          ; 8 (LIST) + 4 (LINK) + 4 (LENGTHS)
    MOVE.L  #$10000,D1
    MOVE.L  D0,D2           ; SLEN
    MOVE.L  4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.W  D3,12(A0)
    MOVE.L  -20(A4),(A0)
    MOVE.L  D2,4(A0)
    MOVE.L  D0,-20(A4)
    ADDQ.L  #8,D0
    ADDQ.L  #8,D0
    MOVE.L  A3,D3
    RTS
.1: MOVEQ   #0,D0
    MOVE.L  A3,D3
    RTS
;*-*

;; ListCopy
I_LISTCOPY:
    LEA 6(A7),A1
    MOVE.w  (A1)+,D0 ; len
    MOVe.L  (A1)+,A0 ; src
    move.l  (a1)+,a1 ; dst
    MOVE.L  A1,A2
    CMP.W   #-1,D0
    BNE.S   .1
    MOVE.W  -2(A0),D0
.1: CMP.W   -4(A1),D0
    BMI.S   .2
    MOVE.W  -4(A1),D0
.2: CMP.W   #1,D0
    BMI.S   .3
    MOVE.W  D0,-2(A1)
    SUBQ.W  #1,D0
.XL:MOVE.L  (A0)+,(A1)+
    DBRA    D0,.XL
.3: MOVE.L  A2,D0
    RTS
;*-*
;; ListAdd
I_LISTADD:
    MOVE.W  6(A7),D0        ; LEN
    MOVE.L  8(A7),A0        ; SRC
    MOVE.L  12(A7),A1       ; DEST
    MOVE.L  A1,A2
    CMP.W   #-1,D0
    BNE.S   .1
    MOVE.W  -2(A0),D0
.1: MOVE.W  -4(A1),D1
    SUB.W   -2(A1),D1
    CMP.W   D1,D0
    BMI.S   .2
    MOVE.W  D1,D0
.2: CMP.W   #1,D0
    BMI.S   .3
    MOVE.W  -2(A1),D1
    ADD.W   D0,-2(A1)
    EXT.L   D1
    LSL.L   #2,D1
    ADD.L   D1,A1
    SUBQ.W  #1,D0
.XL:MOVE.L  (A0)+,(A1)+
    DBRA    D0,.XL
.3: MOVE.L  A2,D0
    RTS
;*-*
;; ListCmp
I_LISTCMP:
    MOVE.W  6(A7),D0        ; LEN
    MOVE.L  8(A7),A0        ; SRC
    MOVE.L  12(A7),A1       ; DEST
    CMP.W   #-1,D0
    BNE.S   .1
    MOVE.W  -2(A0),D0
.1: CMP.W   -2(A1),D0
    BNE.S   .4          ; LENDEST<>LENSRC
    CMP.W   #1,D0
    BMI.S   .3
    SUBQ.W  #1,D0
.XL:CMPM.L  (A0)+,(A1)+
    BNE.S   .4
    DBRA    D0,.XL
.3: MOVEQ   #-1,D0
    RTS
.4: MOVEQ   #0,D0
    RTS
;*-*
;; ListLen
I_LISTLEN:
    MOVE.L  4(A7),A0
    MOVEQ   #0,D0
    MOVE.W  -2(A0),D0
    RTS
;*-*
;; ListMax
I_LISTMAX:
    MOVE.L  4(A7),A0
    MOVEQ   #0,D0
    MOVE.W  -4(A0),D0
    RTS
;*-*
;; Even                     I
I_EVEN:
    BTST    #0,7(A7)
    BEQ.S   .1
    MOVEQ   #0,D0
    RTS
.1: MOVEQ   #-1,D0
    RTS
;*-*
;; Odd                      I
I_ODD:
    BTST    #0,7(A7)
    BNE.S   .1
    MOVEQ   #0,D0
    RTS
.1: MOVEQ   #-1,D0
    RTS
;*-*
;; Eval                     I
I_EVAL:
    MOVE.L  4(A7),A0
    JSR (A0)            ; ACTIONNES DANGEREUSES !!!
    RTS
;*-*
;; ForAll
I_FORALL:
    MOVE.L  4(A7),A0        ; code
    MOVE.L  8(A7),A1        ; list
    MOVE.L  12(A7),A2       ; VAR
    MOVEQ   #-1,D0          ; TRUTH VALUE ->D1
    MOVE.W  -2(A1),D2       ; COUNT
    BEQ.S   .1
    SUBQ.L  #1,D2
    MOVE.L  D0,D1
.XL:MOVE.L  (A1)+,(A2)
    MOVEM.L D1-D2/A0-A2,-(A7)
    JSR (A0)
    MOVEM.L (A7)+,D1-D2/A0-A2
    TST.L   D0
    BEQ.S   .2
    DBRA    D2,.XL
    BRA.S   .3
.2: MOVEQ   #0,D1
    DBRA    D2,.XL
.3: MOVE.L  D1,D0
.1: RTS
;*-*
;; Exists
I_EXISTS:
    MOVE.L  4(A7),A0        ; code
    MOVE.L  8(A7),A1        ; list
    MOVE.L  12(A7),A2       ; VAR
    MOVEQ   #0,D0           ; TRUTH VALUE ->D1
    MOVE.W  -2(A1),D2       ; COUNT
    BEQ.S   .1
    SUBQ.L  #1,D2
    MOVE.L  D0,D1
.XL:MOVE.L  (A1)+,(A2)
    MOVEM.L D1-D2/A0-A2,-(A7)
    JSR (A0)
    MOVEM.L (A7)+,D1-D2/A0-A2
    TST.L   D0
    BNE.S   .2
    DBRA    D2,.XL
    BRA.S   .3
.2: MOVEQ   #-1,D1
    DBRA    D2,.XL
.3: MOVE.L  D1,D0
.1: RTS
;*-*
;; MapList
I_MAPLIST:
    MOVE.L  4(A7),A0        ; code
    MOVE.L  8(A7),A3        ; DESTLIST
    MOVE.L  12(A7),A1       ; SRClist
    MOVE.L  16(A7),A2       ; VAR
    MOVE.L  A3,-(A7)
    MOVE.W  -4(A3),D0
    CMP.W   -2(A1),D0
    BMI.S   .1
    MOVE.W  -2(A1),-2(A3)
    MOVE.W  -2(A1),D2       ; COUNT
    BEQ.S   .1
    SUBQ.L  #1,D2
;   MOVE.L  D0,D1
.XL: MOVE.L  (A1)+,(A2)
    MOVEM.L D2/A0-A3,-(A7)
    JSR (A0)
    MOVEM.L (A7)+,D2/A0-A3
    MOVE.L  D0,(A3)+
    DBRA    D2,.XL
.1: MOVE.L  (A7)+,D0
    RTS
;*-*
;; Abs                      I
I_ABS:
    MOVE.L  4(A7),D0
    BPL.S   .1
    NEG.L   D0
.1: RTS
;*-*
;; Shl                      I
I_SHL:
    MOVE.L  8(A7),D0
    MOVE.L  4(A7),D1
    ASL.L   D1,D0
    RTS
;*-*
;; Shr                      I
I_SHR:
    MOVE.L  8(A7),D0
    MOVE.L  4(A7),D1
    ASR.L   D1,D0
    RTS
;*-*
;; Box
I_BOX:
    MOVE.L  D3,A3
    MOVE.L  -16(A4),A1      ; rast
    MOVE.L  A1,D0
    BEQ.S   .1
    MOVE.L  -52(A4),A6
    MOVE.L  4(A7),D0
    JSR -342(A6)        ; setapen
    MOVE.L  -16(A4),A1
    MOVE.L  20(A7),D0
    MOVE.L  16(A7),D1
    MOVE.L  12(A7),D2
    MOVE.L  8(A7),D3
    JSR -306(A6)        ; rectfill
.1: MOVE.L  A3,D3
    RTS
;*-*

;; Dispose
I_DISP:
    MOVE.L  4(A7),D0
    BEQ.S   .1
    SUBQ.L  #8,D0
    LEA -20(A4),A1
.XL:MOVE.L  (A1),D1
    BEQ.S   .1
    MOVE.L  A1,A2           ; ADR TO LINK BACK TO
    MOVE.L  D1,A1
    CMP.L   D1,D0
    BNE.S   .XL
    MOVE.L  4(A1),D0        ; MEMSIZE
    MOVE.L  (A1),(A2)       ; LINK BACK
    MOVE.L  4.W,A6
    JSR -210(A6)
.1: RTS
;*-*
;; DisposeList
I_DISPL:
    MOVE.L  4(A7),A3        ; A3=STRING
.ML:    MOVE.L  A3,D0
    BEQ.S   .1
    MOVE.L  -8(A3),A3
    SUBQ.L  #8,D0
    SUBQ.L  #8,D0           ; D0=MEM
    LEA -20(A4),A1
.XL:MOVE.L  (A1),D1
    BEQ.S   .ML
    MOVE.L  A1,A2           ; A2=ADR TO LINK BACK TO
    MOVE.L  D1,A1
    CMP.L   D1,D0
    BNE.S   .XL
    MOVE.L  4(A1),D0        ; MEMSIZE
    MOVE.L  (A1),(A2)       ; LINK BACK
    MOVE.L  4.W,A6
    JSR -210(A6)
    BRA.S   .ML
.1: RTS
;*-*

;; Link
I_LINK:
    MOVE.L  8(A7),A0
    MOVE.L  A0,D0
    BEQ.S   .1
    MOVE.L  4(A7),-8(A0)
.1: RTS
;*-*
;; Next
I_NEXT:
    MOVE.L  4(A7),D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.L  -8(A0),D0
.1: RTS
;*-*
;; Forward
I_FORWARD:
    LEA 8(A7),A0        ; LIST
    MOVE.L  4(A7),D1        ; NUM
    ADDQ.L  #1,D1
.XL:MOVE.L  (A0),D0
    BEQ.S   .1
    MOVE.L  D0,A0
    SUBQ.L  #8,A0
    SUBQ.L  #1,D1
    BNE.S   .XL
.1: RTS
;*-*
;; SetStr
I_SETSTR:
    MOVE.W  6(A7),D0
    MOVE.L  8(A7),A0
    CMP.W   -4(A0),D0
    BHI.S   .1
    MOVE.W  D0,-2(A0)
    MOVE.B  #0,0(A0,D0.W)
.1: RTS
;*-*
;; SetList
I_SETLIST:
    MOVE.W  6(A7),D0
    MOVE.L  8(A7),A0
    CMP.W   -4(A0),D0
    BHI.S   .1
    MOVE.W  D0,-2(A0)
.1: RTS
;*-*
;; WaitMsg
I_WAITMSG:
    MOVE.L  4(A7),A0
    MOVE.L  $56(A0),A2      ; A2=PORT
    MOVE.L  4.W,A6
    MOVE.L  A2,A0
    JSR -372(A6)        ; GETMSG
    MOVE.L  D0,A3           ; A3=MES
    TST.L   D0
    BNE.S   .1
.2: MOVE.L  A2,A0
    JSR -384(A6)        ; WAITPORT
    MOVE.L  A2,A0
    JSR -372(A6)        ; GETMSG
    MOVE.L  D0,A3
    TST.L   D0
    BEQ.S   .2
.1: MOVE.L  28(A3),-72(A4)
    MOVE.L  24(A3),-68(A4)
    MOVE.L  20(A3),D2
    MOVE.L  A3,A1
    JSR -378(A6)        ; REPLY
    MOVE.L  D2,D0           ; CLASS
    RTS
;*-*
;; MsgCode                  I
I_MSGCODE:
    MOVEQ   #0,D0
    MOVE.W  -68(A4),D0
    RTS
;*-*
;; MsgQual                  I
I_MSGQUAL:
    MOVEQ   #0,D0
    MOVE.W  -66(A4),D0
    RTS
;*-*
;; MsgIaddr                 I
I_MSGIADR:
    MOVE.L  -72(A4),D0
    RTS
;*-*
;; Rnd
I_RND:                ; rand16:=RangeRand(max16)
    MOVE.L  4(A7),D2        ; 6(a7) was E(a7) ????
    BMI.S   .SET
    MOVE.W  D2,D1           ; RangeRand(1000) --> 0..999
    SUBQ.W  #$1,D1
    MOVE.L  .S(PC),D0       ; randnr:=Rnd(max)
.2: ADD.L   D0,D0
    BHI.S   .3
    EORI.L  #$1D872B41,D0
.3: LSR.W   #$1,D1
    BNE.S   .2
    LEA .S(PC),A0
    MOVE.L  D0,(A0)
    TST.W   D2
    BNE.S   .4
    SWAP    D0
    BRA.S   .5
.4: MULU    D2,D0
.5: CLR.W   D0
    SWAP    D0
    RTS
.SET:   NEG.L   D2
    LEA .S(PC),A0
    MOVE.L  D2,(A0)
    RTS
.S: DC.L    0
;*-*
;; RndQ
I_RNDQ:               ;   seed32:=FastRand(seed32)
    MOVE.L  $0004(A7),D0
    ADD.L   D0,D0           ;   seed:=RndQ(seed)
    BHI.S   .1
    EORI.L  #$1D872B41,D0
.1: RTS
;*-*
;; Mod                      PI
I_MOD:
    MOVE.L  8(A7),D0
    DIVS    6(A7),D0
    MOVE.L  D0,D1
    SWAP    D0
    EXT.L   D0
    EXT.L   D1
    RTS
;*-*
;; Eor                      I
I_EOR:
    MOVE.L  4(A7),D0
    MOVE.L  8(A7),D1
    EOR.L   D1,D0
    RTS
;*-*
;; Cause
; see also NEWR(), Throw() and ReThrow()

I_CAUSE:
    MOVE.L  4(A7),-84(A4)       ; FILL "EXCEPTION" VAR
    MOVE.L  -76(A4),D0
    BEQ.S   .1
    MOVE.L  D0,A0           ; A0=CODE TO JUMP TO
    MOVE.L  -80(A4),A7      ; STACK BACK
    MOVE.L  -88(A4),A5
    MOVE.L  (A7)+,-88(A4)       ; (A5)
    MOVE.L  (A7)+,-76(A4)       ; PUT PREVIOUS HANDLER BACK (CODE)
    MOVE.L  (A7)+,-80(A4)       ; (STACK)
    JMP (A0)
.1: MOVE.L  -24(A4),A0      ; PERFORM CLEANUP(0)
    JMP (A0)
;*-*
;; ListItem
I_LISTITEM:           ; SIMPLE, BUT EFFECTIVE !
    MOVE.L  4(A7),D0
    LSL.L   #2,D0
    MOVE.L  8(A7),A0
    MOVE.L  0(A0,D0.L),D0
    RTS
;*-*

;; NewR
I_NEWR:
    MOVE.L  4(A7),D0        ; COPY OF New()
    MOVE.L  #$10000,D1
    ADDQ.L  #8,D0
    MOVE.L  D0,D2
    MOVE.L  4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.L  -20(A4),(A0)
    MOVE.L  D2,4(A0)
    MOVE.L  D0,-20(A4)
    ADDQ.L  #8,D0
    RTS
.1: MOVE.L  #"MEM",-84(A4)      ; COPY OF Raise()
    MOVE.L  -76(A4),D0
    BEQ.S   .2
    MOVE.L  D0,A0
    MOVE.L  -80(A4),A7
    MOVE.L  -88(A4),A5
    MOVE.L  (A7)+,-88(A4)
    MOVE.L  (A7)+,-76(A4)
    MOVE.L  (A7)+,-80(A4)
    JMP (A0)
.2: MOVE.L  -24(A4),A0
    JMP (A0)
    RTS
;*-*

;; Sign
I_SIGN:
    MOVE.L  4(A7),D0
    BMI.S   .1
    BEQ.S   .2
    MOVEQ   #1,D0
    RTS
.1: MOVEQ   #-1,D0
    RTS
.2: MOVEQ   #0,D0
    RTS
;*-*
;; PrintF
I_PRINTF:
    TST.L   -8(A4)          ;STDIO
    BEQ .2
.3: MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0.L),A0
    LEA 8(A7),A1
    LEA .1(PC),A2
    MOVE.L  4.W,A6
    MOVE.L  -64(A4),A3
    MOVE.L  A3,D2
    JSR -522(A6)
    MOVE.L  -8(A4),D1
    MOVE.L  -44(A4),A6
    JSR -342(A6)
    MOVE.L  D2,A0           ; JUST FOR LEN!
.C: TST.B   (A0)+
    BNE.S   .C
    SUB.L   D2,A0
    SUBQ.L  #1,A0
    MOVE.L  A0,D0
    RTS
.1: MOVE.B  D0,(A3)+        ; RDF DUMP
    RTS
.2: LEA .4(PC),A0
    MOVE.L  A0,D1           ; OPEN CON
    MOVE.L  #1006,D2
    MOVE.L  -44(A4),A6
    JSR -30(A6)
    MOVE.L  D0,-12(A4)
    MOVE.L  D0,-8(A4)
    TST.L   D0
    BNE.S   .3
    MOVEQ   #20,D0
    MOVE.L  D0,-28(A4)
    MOVE.L  -24(A4),A0
    JMP (A0)
.4: DC.B    "CON:0/11/640/80/Output",0
    EVEN
;*-*
;; WaitLeftMouse
I_WAITLEFTMOUSE:
    MOVE.L  4(A7),A3        ; A3=WIN
    MOVE.L  82(A3),D0
    BTST    #3,D0
    BNE.S   .3          ; CHECK IDCMP CONTAINS MOUSE
    BSET    #3,D0
    MOVE.L  -48(A4),A6
    MOVE.L  A3,A0
    JSR -150(A6)        ; MODIFYIDCMP
.3: MOVE.L  $56(A3),A2      ; A2=PORT
    MOVE.L  4.W,A6
    MOVE.L  A2,A0
    JSR -372(A6)        ; GETMSG
    MOVE.L  D0,A3           ; A3=MES
    TST.L   D0
    BNE.S   .1
.2: MOVE.L  A2,A0
    JSR -384(A6)        ; WAITPORT
    MOVE.L  A2,A0
    JSR -372(A6)        ; GETMSG
    MOVE.L  D0,A3
    TST.L   D0
    BEQ.S   .2
.1: MOVE.L  A3,A1           ; MES
    MOVE.L  20(A1),D2
    JSR -378(A6)        ; REPLY
    MOVE.L  D2,D0           ; CLASS
    BTST    #3,D0
    BEQ.S   .2
    RTS
;*-*
;; LeftMouse
I_LEFTMOUSE:
    MOVE.L  4(A7),A3        ; A3=WIN
    MOVE.L  82(A3),D0
    BTST    #3,D0
    BNE.S   .3          ; CHECK IDCMP CONTAINS MOUSE
    BSET    #3,D0
    MOVE.L  -48(A4),A6
    MOVE.L  A3,A0
    JSR -150(A6)        ; MODIFYIDCMP
.3: MOVE.L  $56(A3),A2      ; A2=PORT
    MOVE.L  4.W,A6
    MOVE.L  A2,A0
    JSR -372(A6)        ; GETMSG
    TST.L   D0
    BNE.S   .1
    RTS
.1: MOVE.L  D0,A1           ; MES
    MOVE.L  20(A1),D2
    JSR -378(A6)        ; REPLY
    MOVE.L  D2,D0           ; CLASS
    BTST    #3,D0
    BEQ.S   .2
    MOVEQ   #-1,D0
    RTS
.2: MOVEQ   #0,D0
    RTS
;*-*
;; SetIn
I_SETIN:
    MOVE.L  -8(A4),D0
    MOVE.L  4(A7),D1
    BEQ.S   .1
    MOVE.L  D1,-92(A4)
.1: RTS
;*-*
;; Throw
I_THROW:
    MOVE.L  8(A7),-84(A4)       ; FILL "EXCEPTION" VAR
    MOVE.L  4(A7),-96(A4)       ; FILL "EXCEPTIONINFO" VAR
    MOVE.L  -76(A4),D0
    BEQ.S   .1
    MOVE.L  D0,A0               ; A0=CODE TO JUMP TO
    MOVE.L  -80(A4),A7          ; STACK BACK
    MOVE.L  -88(A4),A5
    MOVE.L  (A7)+,-88(A4)       ; (A5)
    MOVE.L  (A7)+,-76(A4)       ; PUT PREVIOUS HANDLER BACK (CODE)
    MOVE.L  (A7)+,-80(A4)       ; (STACK)
    JMP (A0)
.1: MOVE.L  -24(A4),A0          ; PERFORM CLEANUP(0)
    JMP (A0)
    RTS
;*-*
;; ReThrow
I_RETHROW:
    TST.L   -84(A4)         ; NO RETHROW() IF EXC=0
    BEQ.S   .2
    MOVE.L  -76(A4),D0
    BEQ.S   .1
    MOVE.L  D0,A0           ; A0=CODE TO JUMP TO
    MOVE.L  -80(A4),A7      ; STACK BACK
    MOVE.L  -88(A4),A5
    MOVE.L  (A7)+,-88(A4)       ; (A5)
    MOVE.L  (A7)+,-76(A4)       ; PUT PREVIOUS HANDLER BACK (CODE)
    MOVE.L  (A7)+,-80(A4)       ; (STACK)
    JMP (A0)
.1: MOVE.L  -24(A4),A0      ; PERFORM CLEANUP(0)
    JMP (A0)
.2: RTS
;*-*
;; SelectList
I_SELECTLIST:
    MOVE.L  4(A7),A0        ; code
    MOVE.L  8(A7),A3        ; DESTLIST
    MOVE.L  A3,A6
    MOVE.L  12(A7),A1       ; SRClist
    MOVE.L  16(A7),A2       ; VAR
    MOVE.W  -2(A1),D2       ; COUNT
    BEQ.S   .1
    SUBQ.L  #1,D2
.XL:MOVE.L  (A1)+,(A2)
    MOVEM.L D2/A0-A3/A6,-(A7)
    JSR (A0)
    MOVEM.L (A7)+,D2/A0-A3/A6
    TST.L   D0
    BEQ.S   .2
    MOVE.L  (A2),(A3)+
    ADDQ.W  #1,-2(A6)
    MOVE.W  -4(A6),D0
    CMP.W   -2(A6),D0
    BEQ.S   .1
.2: DBRA    D2,.XL
.1: MOVE.W  -2(A6),D0
    EXT.L   D0
    RTS
;*-*
;; SetColour
I_SETCOLOUR:
    MOVE.L  D3,A3
    MOVE.L  D4,A2
    MOVE.L  4(A7),D3
    MOVE.L  8(A7),D2
    MOVE.L  12(A7),D1
    MOVE.L  16(A7),D0
    MOVE.L  20(A7),A0
    ADD.W   #44,A0
    MOVE.L  4.W,A6
    MOVE.W  20(A6),D4
    MOVE.L  -52(A4),A6
    CMP.W   #39,D4
    BPL.S   .39
    LSR.L   #4,D1
    LSR.L   #4,D2
    LSR.L   #4,D3
    JSR -$120(A6)
    MOVE.L  A3,D3
    MOVE.L  A2,D4
    RTS
.39:    MOVEQ   #24,D4
    LSL.L   D4,D1
    LSL.L   D4,D2
    LSL.L   D4,D3
    JSR -852(A6)
    MOVE.L  A3,D3
    MOVE.L  A2,D4
    RTS
;*-*

;; NewM
I_NEWM:
    MOVE.L  8(A7),D0        ; COPY OF New()
    MOVE.L  4(A7),D1
    ADDQ.L  #8,D0
    MOVE.L  D0,D2
    MOVE.L  4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.L  -20(A4),(A0)
    MOVE.L  D2,4(A0)
    MOVE.L  D0,-20(A4)
    ADDQ.L  #8,D0
    RTS
.1: MOVE.L  #"MEM",-84(A4)      ; COPY OF Raise()
    MOVE.L  -76(A4),D0
    BEQ.S   .2
    MOVE.L  D0,A0
    MOVE.L  -80(A4),A7
    MOVE.L  -88(A4),A5
    MOVE.L  (A7)+,-88(A4)
    MOVE.L  (A7)+,-76(A4)
    MOVE.L  (A7)+,-80(A4)
    JMP (A0)
.2: MOVE.L  -24(A4),A0
    JMP (A0)
    RTS
;*-*

;; Bounds
I_BOUNDS:
    MOVE.L  12(A7),D0       ; D0=VALUE
    MOVE.L  4(A7),D1        ; D1=HIGHERBOUND FIRST
    CMP.L   D1,D0
    BMI.S   .1
    MOVE.L  D1,D0
    BRA.S   .X
.1: MOVE.L  8(A7),D1        ; NOW TRY LOWERBOUND
    CMP.L   D1,D0
    BPL.S   .X
    MOVE.L  D1,D0
.X: RTS
;*-*
;; RealF
I_REALF:
    MOVE.L  12(A7),A2
    CLR.W   -2(A2)
    MOVE.L  8(A7),D2        ; D2=FLOAT
    MOVE.L  -56(A4),A6
    MOVE.L  D2,D0
    JSR -48(A6)
    BPL.S   .3
    MOVE.B  #"-",D0
    BSR.S   .ADDS
    MOVE.L  D2,D0
    JSR -54(A6)         ; ABS
    MOVE.L  D0,D2
.3: MOVE.L  4(A7),D1
    LSL.L   #2,D1
    MOVE.L  .RTAB(PC,D1.L),D1
    MOVE.L  D2,D0
    JSR -66(A6)
    MOVE.L  D0,D2
    MOVEQ   #-1,D1
    BSR.W   .ADD
    CMP.L   #1,4(A7)
    BMI.S   .DONE
    MOVE.L  -56(A4),A6
    MOVE.B  #".",D0
    BSR.S   .ADDS
    MOVE.L  D2,D0
    JSR -90(A6)         ; floor
    MOVE.L  D0,D1
    MOVE.L  D2,D0
    JSR -72(A6)         ; sub
    MOVE.L  4(A7),D1
    SUBQ.L  #1,D1
    LSL.L   #2,D1
    MOVE.L  .TAB(PC,D1.L),D1
    JSR -78(A6)         ; mul
    BSR.S   .ADD
.DONE:  MOVE.L  12(A7),D0
    RTS
.PROC:  MOVE.B  D0,(A3)+
    RTS
.ADDS:  MOVE.W  -2(A2),D1
    CMP.W   -4(A2),D1
    BPL.S   .1
    MOVE.B  D0,0(A2,D1.W)
    CLR.B   1(A2,D1.W)
    ADDQ.W  #1,-2(A2)
.1: RTS
.RTAB:  DC.L    $3f000000,$3d4ccccd,$3ba3d70a   ; okay for 8 digits
    DC.L    $3a03126f,$3851b717,$36a7c5ac
    DC.L    $350637bd,$3356bf95,$31abcc77
.TAB:   DC.L    $41200000,$42c80000,$447a0000   ; same here
    DC.L    $461c4000,$47c35000,$49742400
    DC.L    $4b189680,$4cbebc20 ; ,$4cbebc20
.ADD:   MOVE.L  D1,-(A7)
    JSR -90(A6)         ; FLOOR
    JSR -30(A6)         ; FIX, D0=INT
    MOVE.L  (A7)+,D1
    LEA -32(A7),A7
    MOVE.L  A7,A3
    LEA .PROC(PC),A2
    LEA 16(A3),A0
    MOVE.B  #"%",(A0)+
    TST.L   D1
    BMI.S   .2
    MOVE.L  4+4+32(A7),D1
    ADD.W   #$30,D1
    MOVE.B  #"0",(A0)+
    MOVE.B  D1,(A0)+
    MOVE.B  #".",(A0)+
    MOVE.B  D1,(A0)+
.2: MOVE.B  #"l",(A0)+
    MOVE.B  #"d",(A0)+
    CLR.B   (A0)+
    LEA 16(A3),A0
    LEA 28(A3),A1
    MOVE.L  D0,(A1)
    MOVE.L  4.W,A6
    JSR -522(A6)
    MOVE.L  4+12+32(A7),A2      ; A2=STRING
    MOVE.L  A7,A3
.XL:MOVE.B  (A3)+,D0
    BEQ.S   .O
    BSR.W   .ADDS
    BRA.S   .XL
.O: LEA 32(A7),A7
    RTS
;*-*
;; RealVal
I_REALVAL:
    MOVE.L  D3,A3
    MOVE.L  4(A7),A0        ; A0=STR
.XL:MOVE.B  (A0)+,D0
    CMP.B   #33,D0
    BMI.S   .XL
    MOVEQ   #0,D1
    CMP.B   #"-",D0
    BNE.S   .NNEG
    MOVEQ   #-1,D1
    MOVE.B  (A0)+,D0
.NNEG:  MOVE.W  D1,A6           ; A6=SIGN
    MOVEQ   #0,D2           ; D2=DOTFLAG
    MOVEQ   #1,D1           ; D1=DIVDOT
    MOVEQ   #0,D3           ; D3=RESULT SOFAR
    SUB.L   A1,A1           ; A1=COUNT
.L2:    CMP.B   #"9"+1,D0
    BPL.S   .D
    CMP.B   #".",D0
    BEQ.S   .DOT
    CMP.B   #"0",D0
    BMI.S   .D
    EXT.W   D0
    EXT.L   D0
    SUB.W   #"0",D0
    MOVE.L  D3,A2
    LSL.L   #2,D3
    ADD.L   A2,D3
    LSL.L   #1,D3           ; D3*10
    ADD.L   D0,D3
    TST.L   D2
    BEQ.S   .1
    MOVE.L  D1,A2
    LSL.L   #2,D1
    ADD.L   A2,D1
    LSL.L   #1,D1           ; D1*10
.1: ADDQ.L  #1,A1
.N: MOVE.B  (A0)+,D0
    BRA.S   .L2
.D: MOVE.L  A1,D0
    BEQ.S   .FAIL
    MOVE.L  A0,-(A7)
    MOVE.L  A6,A2
    MOVE.L  D1,D0
    MOVE.L  -56(A4),A6
    JSR -36(A6)
    MOVE.L  D0,D2
    MOVE.L  D3,D0
    JSR -36(A6)
    MOVE.L  D2,D1
    JSR -84(A6)
    MOVE.L  A2,D1
    BEQ.S   .2
    JSR -60(A6)
.2: MOVE.L  A3,D3
    MOVE.L  (A7)+,D1
    SUBQ.L  #1,D1
    SUB.L   4(A7),D1
    RTS
.DOT:   TST.L   D2
    BNE.S   .D
    MOVEQ   #1,D2
    BRA.S   .N
.FAIL:  MOVEQ   #0,D1
    MOVEQ   #0,D0
    RTS
;*-*
;; Fabs
I_FABS:
    MOVE.L  4(A7),D0
    MOVE.L  -56(A4),A6
    JSR -54(A6)
    RTS
;*-*
;; Floor
I_FFLOOR:
    MOVE.L  4(A7),D0
    MOVE.L  -56(A4),A6
    JSR -90(A6)
    RTS
;*-*
;; Fceil
I_FCEIL:
    MOVE.L  4(A7),D0
    MOVE.L  -56(A4),A6
    JSR -96(A6)
    RTS
;*-*
;; Fsin
I_FSIN:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -36(A6)
    RTS
;*-*
;; Fcos
I_FCOS:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -42(A6)
    RTS
;*-*
;; Ftan
I_FTAN:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -48(A6)
    RTS
;*-*
;; Fexp
I_FEXP:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -78(A6)
    RTS
;*-*
;; Flog
I_FLOG:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -84(A6)
    RTS
;*-*
;; Fpow
I_FPOW:
    MOVE.L  4(A7),D0
    MOVE.L  8(A7),D1
    MOVE.L  -60(A4),A6
    JSR -90(A6)
    RTS
;*-*
;; Fsqrt
I_FSQRT:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -96(A6)
    RTS
;*-*
;; Flog10
I_FLOG10:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -126(A6)
    RTS
;*-*

;; FastDispose
I_FASTDISPOSE:            ; SEE ALSO: FASTDISPOSELIST!!!!!
    MOVE.L  8(A7),D0        ; parms(ptr,size)
    BEQ.S   .oute
    MOVE.L  D0,A0
    MOVE.L  4(A7),D0
    CMP.L   #257,D0
    BPL.S   .free
    ADDQ.L  #3,D0
    AND.W   #%1111111100,D0
    LEA GLOBOFFNEWTAB(A4),A1
    ADD.L   D0,A1
    MOVE.L  (A1),(A0)
    MOVE.L  A0,(A1)
.oute:  RTS
.free:  SUBQ.L  #8,A0
    LEA -20(A4),A1
.loop:  MOVE.L  (A1),D1
    BEQ.S   .out
    MOVE.L  A1,A2           ; ADR TO LINK BACK TO
    MOVE.L  D1,A1
    CMPA.L  D1,A0
    BNE.S   .loop
    MOVE.L  4(A1),D0        ; MEMSIZE
    MOVE.L  (A1),(A2)       ; LINK BACK
    MOVE.L  $4.W,A6
    JSR -210(A6)        ; FREEMEM
.out:   RTS
;*-*
;; FastNew
I_FASTNEW:
    MOVE.L  4(A7),D0        ; 1st arg = size 0..
    CMP.L   #257,D0
    BPL.S   .mem
    ADDQ.L  #3,D0
    AND.W   #%1111111100,D0
    BEQ.S   .outn
    MOVE.L  D0,A3
    LEA GLOBOFFNEWTAB(A4),A0
    ADD.L   A3,A0
    MOVE.L  (A0),D0
    BEQ.S   .chop
    MOVE.L  D0,A1
    MOVE.L  (A1),(A0)
    MOVE.L  A3,D1
    LSR.W   #2,D1
    SUBQ.L  #1,D1
    MOVEQ   #0,D2
.clrl:  MOVE.L  D2,(A1)+
    DBRA    D1,.clrl
.outn:  RTS
.chop:  MOVE.L  CHOPMEM(A4),D0
    BEQ.S   .alloc
    MOVE.L  A3,D1
    SUB.L   D1,CHOPLEFT(A4)
    BMI.S   .alloc
    ADD.L   D1,CHOPMEM(A4)
    RTS
.alloc: MOVE.L  #FMEMSIZE+8,D0
    BSR.S   .al
    MOVE.L  D0,CHOPMEM(A4)
    MOVE.L  #FMEMSIZE,CHOPLEFT(A4)
    BRA.S   .chop
.mem:   ADDQ.L  #8,D0
.al:    MOVE.L  D0,D2           ; COPY OF New()
    MOVE.L  #$10000,D1
    MOVE.L  $4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .raise
    MOVE.L  D0,A0
    MOVE.L  -20(A4),(A0)
    MOVE.L  D2,4(A0)
    MOVE.L  D0,-20(A4)
    ADDQ.L  #8,D0
    RTS
.raise: MOVE.L  #"MEM",-84(A4)      ; COPY OF Raise()
    MOVE.L  -76(A4),D0
    BEQ.S   .clean
    MOVE.L  D0,A0
    MOVE.L  -80(A4),A7
    MOVE.L  -88(A4),A5
    MOVE.L  (A7)+,-88(A4)
    MOVE.L  (A7)+,-76(A4)
    MOVE.L  (A7)+,-80(A4)
    JMP (A0)
.clean: MOVE.L  -24(A4),A0
    JMP (A0)
;*-*

;; Min
I_MIN:
    MOVE.L  4(A7),D0
    MOVE.L  8(A7),D1
    CMP.L   D0,D1
    BMI.S   .1
    RTS
.1: MOVE.L  D1,D0
    RTS
;*-*
;; Max
I_MAX:
    MOVE.L  4(A7),D0
    MOVE.L  8(A7),D1
    CMP.L   D0,D1
    BPL.S   .1
    RTS
.1: MOVE.L  D1,D0
    RTS
;*-*
;; OstrCmp
I_OSTRCMP:
    MOVEQ   #0,D0
    MOVE.W  6(A7),D0
    MOVE.L  8(A7),A1
    MOVE.L  12(A7),A0
    ADDQ.L  #1,D0
.1: SUBQ.L  #1,D0
    BEQ.S   .3
    CMPM.B  (A0)+,(A1)+
    BGT.S   .2
    BMI.S   .4
    CMP.B   #0,-1(A0)
    BNE.S   .1
.3: MOVEQ   #0,D0
    RTS
.2: MOVEQ   #1,D0
    RTS
.4: MOVEQ   #-1,D0
    RTS
;*-*
;; AstrCopy
I_ASTRCOPY:
    MOVEQ   #0,D0
    MOVE.W  6(A7),D0
    BEQ.S   .x2
    MOVE.L  8(A7),A1
    MOVE.L  12(A7),A0
    ADDQ.L  #1,D0
.al:    SUBQ.L  #1,D0
    BEQ.S   .x
    MOVE.B  (A1)+,(A0)+
    BNE.S   .al
    BRA.S   .x2
.x: CLR.B   -(A0)
.x2:    RTS

;*-*

;; Some shitty comments
; Yet Another MileStone:
;
; Conservative Mark-Sweep Garbage-Collected Lisp-Cells in E
;
; speed of nrev500_10 test on 128k space (includes 1 collection):
;
; 4.5 x BinProLog, 15.5 x SBP, 26 x Gofer
;
; TODO:
; + stack checking?
; + chunk changable -> own chunksize var. only change if <>0
; + DBRA limit? -> now DBL. should be able to do >2meg
; + collect delegates? -> no.
; + request non-empty mem.
; + previous freelist is added ok now in new.
; + Cell() function
; - multiple args
; - special syntax + pattern matching
; - Root(x) etc.
; - 25% not ideal with small spaces and growing cell usage.


CELLSMEM    = -108
CELLSFREE   = -112
CHUNKSIZE   = -116
CHUNK       = 128*1024      ; must be multiple of 256, >1024

; mem layout:
;
; OBJECT cellmem
;   next:PTR TO cellmem         ; 0
;   end:PTR TO cellmem+SIZEOF cellmem   ; 4
;   cells[n]:ARRAY OF cell      ; 8
; ENDOBJECT
;   bits[n]:ARRAY OF BIT
;*-*

;; Cell
I_CELL:
    MOVE.L  4(A7),D0
    BEQ.S   .false          ; NIL
    MOVE.L  D0,D1
    AND.W   #%111,D1
    BNE.S   .false          ; not cell-aligned
    MOVE.L  CELLSMEM(A4),A0
.cloop: CMP.L   A0,D0
    BMI.S   .cnext          ; lower than bottom
    CMP.L   4(A0),D0
    BPL.S   .cnext          ; higher than top
    MOVEQ   #-1,D0
    RTS
.cnext: MOVE.L  (A0),A0
    MOVE.L  A0,D1
    BNE.S   .cloop
.false: MOVEQ   #0,D0
    RTS

I_FREECELLS:
    LEA CELLSFREE(A4),A0
    MOVEQ   #0,D0
.ccl:   MOVE.L  (A0),D1
    BEQ.S   .cco
    ADDQ.L  #1,D0
    MOVE.L  D1,A0
    BRA.S   .ccl
.cco:   RTS
;*-*
;; SetChunkSize
I_SETCHUNKSIZE:
    TST.L   CHUNKSIZE(A4)
    BNE.S   .chd
    MOVE.L  4(A7),D0
    MOVEQ   #10,D1
    LSL.L   D1,D0
    MOVE.L  D0,CHUNKSIZE(A4)
.chd:   RTS
;*-*
;; Car                      I
I_CAR:
    MOVE.L  4(A7),A0
    MOVE.L  (A0),D0
    RTS
;*-*
;; Cdr                      I
I_CDR:
    MOVE.L  4(A7),A0
    MOVE.L  4(A0),D0
    RTS
;*-*
;; FreeChunks
I_CONS:
    MOVE.L  CELLSFREE(A4),D0    ; Yep, this is IT!
    BEQ.S   .gc
    MOVE.L  D0,A0
    MOVE.L  (A0),A2
    MOVE.L  12(A7),(A0)
    MOVE.L  8(A7),4(A0)
    MOVE.W  6(A7),D1
    BEQ.S   .1
    LSR.W   #2,D1
    SUBQ.W  #1,D1
    LEA 16(A7),A1
.2: MOVE.L  A2,D2
    BEQ.S   .gc
    EXG D0,D2
    MOVE.L  (A2),A3
    MOVE.L  D2,4(A2)
    MOVE.L  (A1)+,(A2)
    MOVE.L  A3,A2
    DBRA    D1,.2
.1: MOVE.L  A2,CELLSFREE(A4)
    RTS

.gc:    CLR.L   CELLSFREE(A4)
    MOVE.L  CELLSMEM(A4),D0
    BEQ .new
    MOVEM.L D3-D7,-(A7)     ; roots too
    MOVE.L  D0,D7           ; D7=space
    MOVE.L  A7,A0           ; A0=roots
.grab:  CMPA.L  A0,A4           ; grab roots until A4
    BLE .sweep
    MOVE.L  (A0)+,D0
    PEA .grab(PC)
.trace: BEQ.S   .ex         ; NIL
    MOVE.L  D0,D1
    AND.W   #%111,D1
    BNE.S   .ex         ; not cell-aligned
    MOVE.L  D7,A2
.tloop: CMP.L   A2,D0
    BMI.S   .tnext          ; lower than bottom
    CMP.L   4(A2),D0
    BPL.S   .tnext          ; higher than top
    MOVE.L  D0,A1
    SUB.L   A2,D0
    LSR.L   #3,D0
    MOVE.L  D0,D1
    LSR.L   #3,D0
    MOVE.L  4(A2),A3        ; mark the sucker
    BSET    D1,0(A3,D0.L)       ; already marked? great!
    BNE.S   .ex
    MOVE.L  4(A1),-(A7)     ; save cdr for later
    MOVE.L  A7,D0
    SUBQ.L  #8,D0
    CMP.L   -64(A4),D0
    BMI .raises
    MOVE.L  (A1),D0         ; go do car now
    BSR.S   .trace          ; OOPS! stack!!!
    MOVE.L  (A7)+,D0
    BRA.S   .trace
.tnext: MOVE.L  (A2),A2
    MOVE.L  A2,D1
    BNE.S   .tloop
.ex:    RTS

.sweep: MOVEQ   #0,D0           ; D0=num cells collected
    MOVE.L  CELLSFREE(A4),A6    ; A6=freelist
.sl:    MOVE.L  D7,A0           ; A0=space
    MOVE.L  4(A0),A1        ; A1=endspace
    MOVE.L  (A0),D5         ; D5=next
    MOVEQ   #0,D1           ; D1=current bit
.sloop: ADDQ.L  #8,A0
    ADDQ.L  #1,D1
    CMPA.L  A0,A1
    BEQ.S   .snext
    MOVE.L  D1,D2
    LSR.L   #3,D2
    BCLR    D1,0(A1,D2.L)
    BNE.S   .sloop
    ADDQ.L  #1,D0
    MOVE.L  A6,(A0)         ; sweep the sucker!
    MOVE.L  A0,A6
    BRA.S   .sloop
.snext: MOVE.L  D5,D7
    BNE.S   .sl
    MOVE.L  A6,CELLSFREE(A4)
    MOVEM.L (A7)+,D3-D7
    MOVE.L  CHUNKSIZE(A4),D1
    LSR.L   #5,D1           ; /4 = 25% must be empty
    CMP.L   D1,D0
    BMI.S   .new
    BRA I_CONS

.new:   MOVE.L  CHUNKSIZE(A4),D0
    BNE.S   .cs
    MOVE.L  #CHUNK,D0
    MOVE.L  D0,CHUNKSIZE(A4)
.cs:    MOVE.L  D0,D1
    LSR.L   #6,D1           ; markspace
    ADD.L   D1,D0
    BSR .alloc
    MOVE.L  D0,A0
    MOVE.L  CELLSMEM(A4),(A0)   ; previous cellspace
    ADD.L   CHUNKSIZE(A4),D0
    MOVE.L  D0,4(A0)        ; end_of_cells
    MOVE.L  D0,A2           ; markspace
    MOVE.L  A0,CELLSMEM(A4)
    MOVE.L  CHUNKSIZE(A4),D0    ; now chop free cells
    LSR.L   #5,D0
    SUBQ.L  #2,D0           ; numcells
    MOVE.L  CELLSFREE(A4),A1
    ADDQ.L  #8,A0
    MOVE.L  A1,(A0)
.chop:  LEA 8(A0),A1        ; freelist in new cell
    MOVE.L  A0,(A1)         ; loop unrolled!!!
    LEA 8(A1),A0        ; 20 cycles per cell!!!
    MOVE.L  A1,(A0)         ; 4 cells in one loop
    LEA 8(A0),A1
    MOVE.L  A0,(A1)
    LEA 8(A1),A0
    MOVE.L  A1,(A0)
    DBRA    D0,.chop
    SUB.L   #$10000,D0
    BCC.S   .chop
    MOVE.L  A0,CELLSFREE(A4)
    MOVE.L  CHUNKSIZE(A4),D0    ; now clear mark space
    LSR.L   #8,D0
    SUBQ.L  #1,D0
    MOVEQ   #0,D1
.clmark:MOVE.L  D1,(A2)+
    DBRA    D0,.clmark
    SUB.L   #$10000,D0
    BCC.S   .clmark
    BRA I_CONS          ; try again!

.alloc: ADDQ.L  #8,D0
    MOVE.L  D0,D2           ; COPY OF New()
    MOVEQ   #0,D1           ; MOVE.L    #$10000,D1
    MOVE.L  $4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .raise
    MOVE.L  D0,A0
    MOVE.L  -20(A4),(A0)
    MOVE.L  D2,4(A0)
    MOVE.L  D0,-20(A4)
    ADDQ.L  #8,D0
    RTS

.raises:MOVE.L  #"STCK",-84(A4)
    BRA.S   .rraise
.raise: MOVE.L  #"MEM",-84(A4)      ; COPY OF Raise()
.rraise:MOVE.L  -76(A4),D0
    BEQ.S   .clean
    MOVE.L  D0,A0
    MOVE.L  -80(A4),A7
    MOVE.L  -88(A4),A5
    MOVE.L  (A7)+,-88(A4)
    MOVE.L  (A7)+,-76(A4)
    MOVE.L  (A7)+,-80(A4)
    JMP (A0)
.clean: MOVE.L  -24(A4),A0
    JMP (A0)
;*-*
;; FastDisposeList
I_FASTDISPOSELIST:        ; ALMOST SAME AS FASTDIPOSE!!!!!
    MOVE.L  4(A7),D0        ; parms(ptr)
    BEQ.S   .oute
    MOVE.L  D0,A0
    MOVE.L  -(A0),D0
    EXT.L   D0
    LSL.L   #2,D0
    ADDQ.L  #4,D0
    CMP.L   #257,D0
    BPL.S   .free
    ADDQ.L  #3,D0
    AND.W   #%1111111100,D0
    LEA GLOBOFFNEWTAB(A4),A1
    ADD.L   D0,A1
    MOVE.L  (A1),(A0)
    MOVE.L  A0,(A1)
.oute:  RTS
.free:  SUBQ.L  #8,A0
    LEA -20(A4),A1
.loop:  MOVE.L  (A1),D1
    BEQ.S   .out
    MOVE.L  A1,A2           ; ADR TO LINK BACK TO
    MOVE.L  D1,A1
    CMPA.L  D1,A0
    BNE.S   .loop
    MOVE.L  4(A1),D0        ; MEMSIZE
    MOVE.L  (A1),(A2)       ; LINK BACK
    MOVE.L  $4.W,A6
    JSR -210(A6)        ; FREEMEM
.out:   RTS
;*-*

;; Fatan
I_FATAN:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -30(A6)
    RTS
;*-*
;; FsinCos
I_FSINCOS:
    MOVE.L  8(A7),A0
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -54(A6)
    RTS
;*-*
;; FsinH
I_FSINH:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR 60(A6)
    RTS
;*-*
;; FcosH
I_FCOSH:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -66(A6)
    RTS
;*-*
;; FtanH
I_FTANH:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -72(A6)
    RTS
;*-*
;; Ftieee
I_FTIEEE:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -102(A6)
    RTS
;*-*
;; Fieee
I_FFIEEE:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -108(A6)
    RTS
;*-*
;; Fasin
I_FASIN:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -114(A6)
    RTS
;*-*
;; Facos
I_FACOS:
    MOVE.L  4(A7),D0
    MOVE.L  -60(A4),A6
    JSR -120(A6)
    RTS
;*-*

;; DoMethod
I_DOMETHOD:
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0.L),A2   ; object
    LEA     8(A7),A1
    MOVE.L  -4(A2),A0
    move.l  8(a0),a3
    JMP     (A3)
;*-*
;; DoMethodA
I_DOMETHODA:
    MOVE.L  8(A7),A2        ; object
    MOVE.L  -4(A2),A0       ; class
    MOVE.L  4(A7),A1        ; message
    move.l  8(a0),a3
    MOVEM.L D3-D7/A4-a6,-(A7)
    JSR     (A3)
    MOVEM.L (A7)+,D3-D7/A4-a6
    RTS
;*-*
;; CoerceMethod
I_CRCMETHOD:
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0),A2     ; object
    MOVE.L  12(A7,D0),A0    ; class
    LEA     8(A7),A1        ; message
    move.l  8(a0),a3
    MOVEM.L D3-D7/A4-a6,-(A7)
    JSR     (A3)
    MOVEM.L (A7)+,D3-D7/A4-a6
    RTS
;*-*
;; CoerceMethodA
I_CRCMETHODA:
    MOVE.L  8(A7),A2    ; object
    MOVE.L  12(A7),A0   ; class
    MOVE.L  4(A7),A1    ; message
    move.l  8(a0),a3
    MOVEM.L D3-D7/A4-a6,-(A7)
    JSR     (A3)
    MOVEM.L (A7)+,D3-D7/A4-a6
    RTS
;*-*
;; DoSuperMethod
I_DOSMETHOD:
    MOVE.L  4(A7),D0
    LEA     8(A7),A1
    MOVE.L  8(A7,D0),A2
    MOVE.L  12(A7,D0),A0
    MOVE.L  24(A0),A0
    MOVE.L  8(A0),A3
    JMP     (A3)
;*-*
;; DoSuperMethodA
I_DOSMETHODA:
    MOVE.L  4(A7),A1    ; message
    MOVE.L  8(A7),A2    ; object
    MOVE.L  12(A7),A0   ; class
    move.l  24(a0),a0
    MOVE.L  8(a0),a3
    JMP     (A3)
;*-*
;; Set
I_SET:
    MOVE.L  -48(A4),A6
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0),A0
    LEA     8(A7),A1
    JMP     -648(A6)
;*-*
;; Sets
I_SETS:
    MOVE.L  -48(A4),A6
    LEA     .T(PC),A1
    MOVE.L  4(A7),4(A1)
    MOVE.L  8(A7),(A1)
    MOVE.L  12(A7),A0
    JMP     -648(A6)
.T: DC.L    0,0,0
;*-*
;; Get
I_GET:
    MOVE.L  -48(A4),A6
    MOVE.L  4(A7),A1
    MOVE.L  8(A7),D0
    move.l  12(A7),A0
    JMP     -654(A6)
;*-*
;; Gets
I_GETS:
    MOVE.L  -48(A4),A6
    LEA     .T(PC),A1
    MOVE.L  4(A7),D0
    MOVE.L  8(A7),A0
    JSR     -654(A6)
    MOVE.L  .T(PC),D0
    RTS
.T: DC.L    0
;*-*
;; CtrlD
I_CTRLD:
    MOVEQ   #0,D0
    MOVEQ   #0,D1
    MOVE.L  4.W,A6
    JSR -306(A6)
    BTST    #13,D0
    BEQ.S   .1
    MOVEQ   #0,D0
    MOVE.L  #8192,D1
    JSR -306(A6)
    MOVEQ   #-1,D0
    RTS
.1: MOVEQ   #0,D0
    RTS
;*-*
;; CtrlE
I_CTRLE:
    MOVEQ   #0,D0
    MOVEQ   #0,D1
    MOVE.L  4.W,A6
    JSR -306(A6)
    BTST    #14,D0
    BEQ.S   .1
    MOVEQ   #0,D0
    MOVE.L  #16384,D1
    JSR -306(A6)
    MOVEQ   #-1,D0
    RTS
.1: MOVEQ   #0,D0
    RTS
;*-*
;; CtrlF
I_CTRLF:
    MOVEQ   #0,D0
    MOVEQ   #0,D1
    MOVE.L  4.W,A6
    JSR -306(A6)
    BTST    #15,D0
    BEQ.S   .1
    MOVEQ   #0,D0
    MOVE.L  #32768,D1
    JSR -306(A6)
    MOVEQ   #-1,D0
    RTS
.1: MOVEQ   #0,D0
    RTS
;*-*
;; Chk
I_CHK:
    MOVEQ   #0,D0
    TST.L   4(A7)
    BEQ.S   .1
    MOVEQ   #-1,D0
.1: RTS
;*-*
;; Eof
I_EOF:
    move.l  4(a7),d1
    movem.l d3-d7,-(a7)
    move.l  -44(a4),a6
    move.l  d1,d7
    moveq   #0,d2
    moveq   #1,d3
    jsr     -66(a6)         ; seek to end
    move.l  d0,d6
    move.l  d0,d2
    moveq   #-1,d3
    move.l  d7,d1
    jsr     -66(a6)         ; and back again
    cmp.l   d0,d6
    seq     d0
    ext.w   d0
    ext.l   d0
    movem.l (a7)+,d3-d7
    rts
;*-*
;; Fopen
I_FOPEN:
    MOVE.L  4(A7),D2            ; mode
    MOVE.L  8(A7),D1            ; name
    MOVE.L  -44(A4),A6          ; dosbase
    JSR     -30(A6)             ; open
    tst.l   d0
    bne     .ok
    rts
.ok:
    MOVE.L  d0,-(a7)            ; fh

    move.l  -40(a4),a6
    moveq   #8,d0
    moveq   #1,d1
    jsr     -198(a6)
    tst.l   d0
    beq     .close
    move.l  d0,a0
    move.l  -128(a4),(a0)
    move.l  (a7),4(a0)
    move.l  a0,-128(a4)
    move.l  (a7)+,d0
    rts

.close:
    move.l  (a7)+,d1
    move.l  -44(a4),a6
    jsr     -36(a6)
    moveq   #0,d0
    rts

;*-*
;; Fclose
I_FCLOSE:
    move.l  4(a7),d1
    move.l  -128(a4),a0
    lea     -128(a4),a1

.c: move.l  a0,d0
    beq     .k
    cmp.l   4(a0),d1
    beq     .cl
    move.l  a0,a1
    move.l  (a0),a0
    bra     .c
.k: moveq   #0,d0
    rts
.cl:move.l  (a0),(a1)   ; unchain
    move.l  d1,-(a7)
    move.l  -40(a4),a6
    move.l  a0,a1
    moveq   #8,d0
    jsr     -210(a6)
    move.l  (a7)+,d1
    move.l  -44(a4),a6
    jsr     -36(a6)
    moveq   #0,d0
    rts          
;*-*
;; Alloc
I_ALLOC:
    MOVE.L  -120(A4),A0
    MOVe.L  4(A7),D0
    ADDQ.L  #4,D0
    MOVE.L  D0,D2
    MOVE.L  $4.W,A6
    JSR     -708(A6)
    MOVE.L  D0,A0
    MOVE.L  D2,(A0)+
    MOVE.L  A0,D0
    RTS
;*-*
;; Free
I_FREE:
    MOVE.L  4(A7),A1
    MOVE.L  -(A1),D0
    MOVE.L  -120(A4),A0
    MOVE.L  $4.W,A6
    JSR     -714(A6)
    RTS
;*-*
;; PutF
I_PUTF:
.3: LEA 8(A7),A1
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0.L),A0
    LEA     .1(PC),A2
    MOVE.L  4.W,A6
    MOVE.L  -64(A4),A3
    MOVE.L  A3,D2
    JSR     -522(A6)
    MOVE.L  D2,A0
.5: TST.B   (A0)+
    BNE .5
    SUB.L   D2,A0
    MOVE.L  D3,A3           ; BACKUP
    MOVE.L  A0,D3
    SUBQ.L  #1,D3
    MOVE.L  -44(A4),A6
    MOVE.L  4(A7),D0
    MOVE.L  12(A7,D0),D1
    JSR     -48(A6)
    MOVE.L  D3,D0
    MOVE.L  A3,D3
    RTS
.1: MOVE.B  D0,(A3)+        ; RDF DUMP
    RTS
;*-*
;; ReadB
I_READB:
    MOVE.L  D3,A3
    MOVE.L  16(A7),D1
    MOVE.L  12(A7),D0
    MOVEQ   #0,D2
    MOVE.L  8(A7),D3
    SUBQ.L  #1,D3

.LP:ADD.L   D0,D2
    DBF     D3,.LP

    MOVE.L  -44(A4),A6
    MOVE.L  4(A7),D3
    EXG     D2,D3
    JSR     -42(A6)

    MOVEQ   #-1,D1
    MOVE.L  12(A7),D2
.LD:ADDQ.L  #1,D1
    SUB.L   D2,D0
    BPL     .LD
    MOVE.L  D1,D0
    MOVE.L  A3,D3
    RTS
;*-*
;; WriteB
I_WRITEB:
    MOVE.L  D3,A3
    MOVE.L  16(A7),D1
    MOVE.L  12(A7),D0
    MOVEQ   #0,D2
    MOVE.L  08(A7),D3
    SUBQ.L  #1,D3

.LP:ADD.L   D0,D2
    DBF     D3,.LP

    MOVE.L  -44(A4),A6
    MOVE.L  04(A7),D3
    EXG     D2,D3
    JSR     -48(A6)

    MOVEQ   #-1,D1
    MOVE.L  12(A7),D2
.LD:ADDQ.L  #1,D1
    SUB.L   D2,D0
    BPL     .LD
    MOVE.L  D1,D0
    MOVE.L  A3,D3
    RTS
;*-*
;; Size
I_SIZE:
    MOVE.L  4(A7),D1
    MOVE.L  D3,A3
    MOVEQ   #0,D2
    MOVEQ   #1,D3
    MOVE.L  -44(a4),A6
    JSR     -66(a6)

    move.l  4(a7),d1
    move.l  d0,d2
    moveQ   #-1,d3
    jsr     -66(a6)
    move.l  a3,d3
    rts
;*-*
;; Lsl
I_LSL:
    MOVE.L  4(A7),D1
    MOVE.L  8(A7),D0
    LSL.L   D1,D0
    RTS
;*-*
;; Lsr
I_LSR:
    MOVE.L  4(A7),D1
    MOVE.L  8(A7),D0
    LSR.L   D1,D0
    RTS
;*-*
I_CODEEND:
    NOP

;xxx:
;   lea .1(pc),a0
;   HASH    a0,d0,d1
;   rts
;.1:    dc.b    "left",0,0
;*-*
;; Patch codes
;; PrintF (37+)
I_PRINTF37:
    MOVE.L  D3,A3
    MOVE.L  -8(A4),D1
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0),D2
    MOVE.L  A7,D3
    ADDQ.L  #8,D3
    MOVE.L  -44(a4),A6
    JSR     -354(A6)
    MOVE.L  -8(A4),D1
    JSR     -360(A6)
    MOVE.L  A3,D3
    RTS
I_PRINTF37_E:
;*-*
;; PutF (37+)
I_PUTF37:
    MOVE.L  D3,A3
    MOVE.L  4(A7),D0
    MOVE.L  8(A7,D0),D2
    MOVE.L  12(A7,D0),D1
    MOVE.L  A7,D3
    ADDQ.L  #8,D3
    MOVE.L  -44(a4),A6
    JSR     -354(A6)
    MOVE.L  -8(A4),D1
    JSR     -360(A6)
    MOVE.L  A3,D3
    RTS
I_PUTF37_E:
;*-*
;; Long (020+)
I_LONG020:
    MOVE.L  ([4.W,A7]),D0
    RTS
;*-*
;; Int (020+)
I_INT020:
    MOVEQ   #0,D0
    MOVE.W  ([4.W,A7]),D0
    RTS
;*-*
;; Char (020+)
I_CHAR020:
    MOVEQ   #0,D0
    MOVE.B  ([4.W,A7]),D0
    RTS
;*-*
I_PATCHEND:
;*-*



