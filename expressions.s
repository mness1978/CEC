
;; Expression support
; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The Expression Calculation Part             ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;
;; EAEXP!!!


; ea specific version of EXP: optimizes some superfluous code away
;
; HANDLED ARE:
;
; MOVE.L #VAL,D0        6   1
; LEA X(PC),A0; MOVE.L A0,D0    6   'bla'
; MOVE.L X(A5),D0       4   a
; MOVEQ #VAL,D0         2   1
; MOVE.L Dx,D0          2   a
;
; RESULT IN D0:     0   NO OPTI DONE
;           1   OPTI DONE, BUT DON'T MOVE (MAY USE A0 TOO)
;           <ADR>   OPTI DONE, <ADR>..A4 IS CODE
EAEXP:
    MOVEM.L A1/D3/D4,-(A7)
    MOVE.L  A4,-(a7)
    MOVE.W  EAREQUEST,-(A7)
    BCLR    #2,ICODEPREFS+3
    BSR EXP
    MOVE.w  (A7)+,EAREQUEST
    move.l  (a7)+,a1
    MOVE.L  A4,D3
    SUB.L   A1,D3
    CMP.L   #7,D3
    BPL     .LASTADR
    CMP.L   #2,D3
    BNE     .1
;*-*

;; MOVEQ

    CMP.B   #%01110000,(A1)
    BNE .M
    MOVE.W  EAREQUEST(PC),D3
    CMP.W   #17,D3
    BPL .X
    CMP.W   #16,D3
    BEQ     .3
    CMP.W   #8,D3
    BPL     .5
    LSL.W   #1,D3
    OR.B    D3,(A1)         ; ** MOVEQ #1,Dx
    BRA     .OK
.3: MOVE.B  1(A1),D4
    beq     .3C
    EXT.W   D4
    MOVE.W  D4,(A4)+        ; ** PEA 1.W
    MOVE.W  .4(PC),(A1)
    BRA     .OK
.3C:move.w  .4C,(a1)
    bra     .OK
.4: PEA     1.W
.4C:CLR.L   -(a7)

.5: MOVE.B  1(A1),D4
    EXT.W   D4
    TST.W   D4
    BEQ     .5xx
    MOVE.W  D4,(A4)+        ; ** MOVE.W 1,Ax
    MOVE.W  .6(PC),D4
    LSL.W   #8,D3
    LSL.W   #1,D3
    OR.W    D3,D4
    MOVE.W  D4,(A1)
    BRA     .OK
.5xx:
    MOVE.W  .62(PC),D4
    OR.W    D3,D4
    LSL.W   #8,D3
    LSL.W   #1,D3
    OR.W    D3,D4
    MOVE.W  D4,(A1)+
    MOVE.L  A1,A4
    BRA     .OK

.6: MOVE.W  #1,A0
.62:SUB.L   A0,A0

.1: CMP.L   #4,D3
    BNE     .2

;*-*
;; VAR
    MOVE.W  (A1),D3
    and.w   #$fffe,d3
    CMP.W   #%0010000000101100,D3
    BNE     .X

    BSR.W   DOOPT           ; ** MOVE.L 2(A5),Rx/-(A7)/2(A5)
    BRA     .OK
;*-*
;; MOVE.L Dx,D0

.M: MOVE.W  (A1),D3
    AND.W   #%1111111111111000,D3
    CMP.W   #%0010000000000000,D3
    BNE     .X
    BSR     DOOPT           ; ** MOVE.L Dx,Rx/-(A7)/2(A5)
    BRA     .OK


.2: CMP.W   #%0100000111111010,(A1)
    BEQ     .7              ; ** LEA xx(PC),A0
    move.w  (a1),d3
    bclr    #0,d3
    CMP.W   #$41EC,d3
    beq     .7
;*-*
;; MOVE.L

    CMP.W   #%0010000000111100,(A1)
    BNE     .X
    MOVE.W  EAREQUEST(PC),D3
    CMP.W   #16,D3
    BNE     .9
    TST.W   2(A1)
    BEQ     .8
    CMP.W   #-1,2(A1)
    BEQ     .10
.9: BSR     DOOPT           ; ** MOVE.L #1,Rx/-(A7)/2(A5)
    BRA     .OK         ; ** PEA 1.W
.8: TST.W   4(A1)
    BMI     .9
.11:    MOVE.W  .4(PC),(A1)
    MOVE.W  4(A1),2(A1)
    SUBQ.L  #2,A4
    BRA     .OK
.10:    TST.W   4(A1)
    BMI     .11
    BRA     .9
;*-*
;; LEA+MOVE
.7:
    CMP.W   #%0010000000001000,4(A1)
    BNE     .X          ; ** MOVE.L A0,D0 [after LEA]
    MOVE.W  EAREQUEST(PC),D3
    CMP.W   #16,D3
    BPL     .OT
    CMP.W   #8,D3
    BPL     .A
    OR.W    #%110000000000,(A1)
    ADDQ.L  #4,A1
    OR.W    #%110,(A1)
    BSR     DOOPT           ; ** MOVE.L A0,Dx
    SUBQ.L  #4,A1
    BRA     .OKNM
.A: SUBQ.L  #2,A4
    BSR     DOOPT           ; ** MOVE.L LAB(PC),Ax
    BRA     .OKNM
.OT:
    cmp.w   #16,EAREQUEST
    bne     .OTX
    cmp.w   #%0100000111111010,(a1)
    bne     .OT2            ; ** LEA xx(PC),A0
    move.w  #%0100100001111010,(a1)
    subq.l  #2,a4
    bra     .OKNM
.OT2:
    move.w  (a1),d3
    bclr    #0,d3
    cmp.w   #$41ec,d3
    bne     .OTX            ; ** LEA xx(A5),A0
    move.w  (a1),d3
    and.l   #1,d3
    or.w    #$486c,d3       ; ** PEA 4(A5)
    move.w  d3,(A1)
    subq.l  #2,a4
    bra     .OKNM

.OTX:
    ADDQ.L  #4,A1
    BSR     DOOPT           ; ** MOVE.L A0,-(A7)/2(A5)
    SUBQ.L  #4,A1
    BRA     .OKNM

.LASTADR:
    btst    #2,ICODEPREFS+3
    bne     .X
    move.l  LAST_CMD_ADR,d0
    beq     .X
    move.l  d0,a1
    move.l  a4,d0
    sub.l   a1,d0
    cmp.l   #6,d0
    bgt     .X
    cmp.w   #16,EAREQUEST
    bne     .X
    cmp.w   #2,d0
    bgt     .X
    cmp.w   #$2008,(a1)     ; ** MOVE.L A0,D0 [arrays]
    bne     .X
    move.w  #$2f08,(a1)     ; ** -> MOVE.L A0,-(A7)
    bra     .OKNM


.OK:
    MOVE.L  A1,D0
    BRA     .OUT
.OKNM:
    MOVEQ   #1,D0
    BRA     .OUT
.X: MOVEQ   #0,D0
.OUT:
    MOVEM.L (A7)+,A1/D3/D4
    RTS
;*-*

DOOPT:  MOVE.W  EAREQUEST(PC),D3
    CMP.W   #17,D3          ; A1=OPCODE
    BPL.S   .VAR
    CMP.W   #16,D3
    BEQ.S   .ST
    LSL.W   #3,D3
    BTST    #6,D3
    BEQ.S   .XX
    BCLR    #6,D3
    BSET    #0,D3
    BRA.S   .XX
.ST:MOVE.W  #%111100,D3
.XX:LSL.W   #6,D3
    OR.W    D3,(A1)
    RTS


.VAR:   ORI.W   #%0000101101000000,(A1) ; SET TO MOVE.L EA,2(A5)
    MOVE.L  A2,D4
    MOVE.L  A6,D0
    LEA -2(A4),A2
    CMPA.L  A2,A1
    BEQ GRUNT
    MOVE.W  (A2),-(A7)
    MOVE.W  (A1),(A2)
    MOVE.L  EAIDENT(PC),A6
    JSR GVA6D3_9
    MOVE.W  (A2),(A1)
    MOVE.W  (A7)+,(A2)
    MOVE.L  D0,A6
    MOVE.L  D4,A2
    RTS
GRUNT:  MOVE.L  EAIDENT(PC),A6
    JSR GVA6D3_9
    MOVE.L  D0,A6
    MOVE.L  D4,A2
    RTS



EAREQUEST:    DC.W    0   ; 0-15=REG, 16=STACK, 17,25=VAR
EAIDENT:      DC.L    0

TINYEXP:              ; GENERATES CODE FOR D0, JUST VALUES
    MOVE.W  (A3)+,D0        ; TRASHES JUST D0
    CMP.W   #VALUE,D0
    BEQ.S   .1
    CMP.W   #43,D0
    BEQ.S   .1
    CMP.W   #8,D0
    BNE ERROR30
    CMP.W   #43,(A3)
    BEQ.S   .NF
    CMP.W   #VALUE,(A3)+
    BNE ERROR30
    MOVE.L  (A3)+,D0
    NEG.L   D0
    BRA.S   .2
.1: MOVE.L  (A3)+,D0
.2: CMP.L   #128,D0
    BPL.S   .3
    CMP.L   #-128,D0
    BMI.S   .3
    MOVE.B  .4(PC),(A4)+
    MOVE.B  D0,(A4)+
    RTS
.3: MOVE.W  .5(PC),(A4)+
    MOVE.L  D0,(A4)+
    RTS
.4: MOVEQ   #1,D0
.5: MOVE.L  #1,D0
.NF:    ADDQ.L  #2,A3
    MOVEM.L D0-D1/A0-A1/A6,-(A7)
    BSR ISOPENMATH
    MOVE.L  MATHBASE(PC),A6
    MOVE.L  (A3),D0
    JSR -60(A6)         ; SPNEG
    MOVE.L  D0,(A3)
    MOVEM.L (A7)+,D0-D1/A0-A1/A6
    BRA.S   .1


FINDMEMBER:   ; *** FIND O.MEMBER, D1=TRASH,a1=first->REAL,a6=obj
    MOVE.W  OID(A6),D1
    CMP.W   OID(A1),D1
    BEQ.S   .X
    MOVEM.L D0/D3/A3-A5,-(A7)
    MOVE.L  8(A1),D3
    LEA OMEMB+4(A6),A3
.XL:MOVE.L  ONEXT(A3),A3
    MOVE.L  A3,D0
    BEQ.S   .ERA3
    MOVE.L  D3,A4
    MOVE.L  OASCII(A3),A5
    MOVE.L  A5,D0
    BEQ.S   .XL
.L2:    CMPM.B  (A5)+,(A4)+
    BNE.S   .XL
    TST.B   -1(A5)
    BNE.S   .L2
    CMP.W   OID(A3),D1
    BNE.S   .XL          ; BNE ERA3
    MOVE.L  A3,A1
    MOVEM.L (A7)+,D0/D3/A3-A5
.X: RTS
.ERA3:  MOVEM.L (A7)+,D0/D3/A3-A5
    BRA ERROR42



; the main part for the objects  -  members  -  indexes

TYPERF_XXX:
        DC.B    0
        DC.B    0

EADDRESSMODI:         ; D5=-1/OTHER,A3=VOOR HELE EXP
    MOVEM.L D3-D5/A1,-(A7)  ; D0=[0=-->D0,1=-->D1,2=D0-->]
    CMP.W   #-1,D5          ;     SET(D5) PAR     PAR
    BNE.S   .1
    CMP.W   #1,D0
    BNE.S   .1
    MOVEQ   #0,D0           ; D0=FLAG
.1: MOVE.W  (A3)+,D1
    CMP.W   #32,D1          ; SEE IF ^IDENT
    BPL PL
    CMP.W   #IDENT,D1
    BNE ERROR6
    MOVE.L  (A3)+,A0        ; A0=IDENT
    TST.B   4(A0)
    BEQ ERROR22
    CMP.B   #LAB,4(A0)
    BEQ ERROR6
    MOVE.B  16(A0),TYPERF_XXX  ; number of PTR TO's
    MOVE.W  (A3)+,D1
    CMP.W   #33,D1          ; SEE IF IDENT++
    BPL PLX
PLB:
    CMP.W   #41,D1
    BNE     .0
    CMP.W   #31,(A3)+
    BNE ERROR42
    MOVE.L  (A3)+,.SPEC
    MOVE.W  (A3)+,D1
    CLR.L   .TDRF
    MOVE.B  #-1,TYPERF_XXX
    BRA .0_2        ; .DREFL
.0:
    MOVE.L  (A0),.SPEC
    CLR.B   .TDRF
.DREFL:
    MOVE.L  COMPILEPROC(PC),A1
    CMP.L   18(A1),A0
    BEQ .0_2                    ; self can't be a ptr to ptr ... - problems
    TST.B   TYPERF_XXX
    BGT     .0_0
    BMI     .0_2
    MOVE.L  (A0),.SPEC
    BRA     .0_1
.0_0:
    MOVE.L  #4,.SPEC
.0_1:
    SUBQ.B  #1,TYPERF_XXX
    TST.B   TYPERF_XXX
    BPL .0_2
    CMP.W   #29,d1
    beq     .0_2
    move.l  (a0),.SPEC
    subq.b  #1,TYPERF_XXX
.0_2:

    AND.L   #$FFFF,D0
    MOVE.W  D5,.KEEPD5

    MOVEQ   #0,D3
    CMP.W   #29,D1          ; "["
    BNE.W   .8
    ADDQ.L  #4,A3           ; SKIP ]OFFSET
    CMP.W   #30,(A3)        ; "[]"?
    BEQ.W   .10
    CMP.W   #VALUE,(A3)     ; "[VAL"
    BNE.S   .9
    CMP.W   #30,6(A3)       ; "[VAL]"
    BNE.S   .9
    MOVE.L  2(A3),D3        ; D3=OFFSET
    ADDQ.L  #6,A3
    BRA.S   .10
.9: CMP.W   #IDENT,(A3)     ; "[VAR"?
    BNE.S   .9B
    CMP.W   #30,6(A3)       ; "[VAR]"?
    BNE.S   .9B

    MOVE.L  2(A3),A6
    MOVE.W  .OPTID2(PC),(A4)+
    JSR GVA6D3_0
    ADDQ.L  #6,A3
    MOVEQ   #-1,D3
    BRA.S   .10

;; "Calculate expression in '[]'"

.9B:MOVEM.L D0/D3/A0,-(A7)
    TST.B   .TDRF
    BEQ.S   .NDR1
    MOVE.W  .SAVA0(PC),(A4)+
.NDR1:  MOVE.L  .SPEC(PC),-(A7)
    MOVE.L  .TDRF(PC),-(A7)
    CMP.W   #-1,D5
    BEQ.S   .31
    MOVE.W  .26(PC),(A4)+
    BSR EXP
    MOVE.L  .27(PC),(A4)+
    BRA.S   .32
.31:BSR EXP
    MOVE.W  .30(PC),(A4)+
.32:MOVE.L  (A7)+,.TDRF
    MOVE.L  (A7)+,.SPEC
    TST.B   .TDRF
    BEQ.S   .NDR2
    MOVE.W  .RETA0(PC),(A4)+
.NDR2:  MOVEM.L (A7)+,D0/D3/A0
;*-*

    MOVEQ   #-1,D3
.10:CMP.W   #30,(A3)+       ; "]"
    BNE ERROR34             ;
    MOVE.W  (A3)+,D1        ;

.8: SUBA.L  A1,A1           ; A1=OBJECT (member/NIL)
    SUBA.L  A6,A6           ; A6=OBJECT (HEAD/NIL)
    CMP.W   #41,D1          ; CHECK FOR POINTERTYPE
    BNE.S   .NCAST          ;
    CMP.W   #31,(A3)+       ; ::another objheader
    BNE ERROR40             ;
    MOVE.L  (A3)+,A6        ;
    MOVE.W  (A3)+,D1        ;
.NCAST:
    CMP.W   #35,D1          ; "."
    BNE .11
    CMP.W   #42,(A3)        ; another objmemberascii
    BEQ .EADDRMETHOD
    CMP.W   #39,(A3)+       ; another objmember
    BNE ERROR40
    CMP.W   #17,4(A3)       ; "(" - method
    BEQ .EADDRMETHOD2
    MOVE.L  .SPEC,D1
    CMP.L   #MAXOBJSIZE,D1
    BMI ERROR40
    MOVE.L  (A3)+,A1        ; A1=FIRST MATCH MEMBER
    MOVE.L  A6,D2
    BNE.S   .CASTO
    MOVE.L  D1,A6           ; A6=OBJECTHEAD
.CASTO: BSR FINDMEMBER
    BRA.S   .SKM
.FAKEO: DC.L    0,0,0,0
.SKMS:  SUBQ.L  #2,A3           ; JUMPSPOT FOR OBJ[I]
    LEA .FAKEO(PC),A1
.SKM:   MOVE.L  .SPEC,D1
    MOVE.L  D1,A2
    MOVE.W  4(A2),D5
    BRA.S   .12
.11:    MOVE.L  .SPEC,D5        ; D5=OBJSIZE
    CMP.L   #MAXOBJSIZE,D5
    BPL .SKMS           ; !!!ERROR40
.15:    SUBQ.L  #2,A3
.12:    BSR PP          ; D2=INC/DEC
    MOVEQ   #2,D1
    BSR GENID

    TST.L   D3          ; DECIDE ON CODE-GEN
    BEQ.S   .OO
    BMI.S   .EO
    BPL     .IO

;; Object offset

.OO:    MOVEQ   #0,D1
    CMP.L   A1,D1
    BEQ.S   .13
    MOVE.W  (A1),D1
    EXT.L   D1
.13:    BSR GEND2
    BRA     .CONT
;*-*
;; Object index

.EO:
    CMP.W   #1,D5
    BEQ     .SO
    CMP.L   #-1,D3
    BNE ERROR31
    CLR.W   .THE_FACTOR
    CMP.W   #1,ECPU
    BPL     .020_SCALE_FACTOR
    CMP.W   #2,D5
    BEQ.S   .S1
    CMP.W   #4,D5
    BEQ.S   .S2
    CMP.W   #8,D5
    BEQ.S   .S3
.020_SCALE_FACTOR:
    CMP.W   #2,D5
    BEQ     .SF1
    CMP.W   #4,D5
    BEQ     .SF2
    CMP.W   #8,D5
    BEQ     .SF3
    CMP.W   #16,D5
    BEQ.S   .S4
    MOVE.W  .28(PC),(A4)+
    MOVE.W  D5,(A4)+
    BRA.S   .SO
.S4:    MOVE.W  .29+6(PC),(A4)+     ; OBJ OF 16
    BRA.S   .SO
.S3:MOVE.W  .29+4(PC),(A4)+     ; OBJ OF 8
    BRA.S   .SO
.S2:MOVE.W  .29+2(PC),(A4)+     ; LONG
    BRA.S   .SO
.SF1:MOVE.W #1,.THE_FACTOR
    BRA     .SO
.SF2:MOVE.W #2,.THE_FACTOR
    BRA     .SO
.SF3:MOVE.W #3,.THE_FACTOR
    BRA     .SO
.S1:MOVE.W  .29(PC),(A4)+       ; INT
.SO:MOVEQ   #0,D1
    CMP.L   D1,A1
    BEQ.S   .CONT
    MOVE.W  (A1),D1
    BSR GENADD
    BRA.S   .CONT
;*-*
;; Object offset+index

.OPC:   MOVE.L  2(A5),A0

.IO:    MOVEQ   #0,D1
    CMP.L   A1,D1
    BEQ.S   .14
    MOVE.W  (A1),D1
    EXT.L   D1
.14:    MOVE.L  D3,D4
    MULU    D5,D4
    ADD.L   D4,D1
    BSR GEND2
;*-*

; FROM HERE D3=-1(D2 ALREADY CALC.), >0(RELA0)

.CONT:  ;MOVE.L #1,.SPEC        ; TO BE SURE
    TST.B   .TDRF
    BNE.S   .NXA5
    MOVE.W  .OPC(PC),(A4)+      ; MOVE.L X(A5),A0
    JSR GVA0D4_0
.NXA5:  BTST    #1,CODEPREFS+3
    BEQ.S   .NNCH
    BSR NILCHECK
.NNCH:  MOVEQ   #0,D4
    CMP.L   D4,A1
    BEQ.S   .16
    MOVE.W  4(A1),D4        ; D4=READSIZE
    BTST    #1,OFLAGS(A1)
    BEQ.S   .17 ;16B            ; was 17! (caused uninit .spec)
    MOVE.L  OPTRTYPE(A1),.SPEC
    BRA.S   .17
.16:    MOVE.L  D5,D4
.16B:   MOVE.L  #1,.SPEC
.17:

    CLR.B   .TDRF
    CMP.W   #35,(A3)        ; "." (SLOW)
    BEQ.S   .DD
    CMP.W   #41,(A3)        ; "::"
    BEQ.S   .DD
    CMP.W   #29,(A3)        ; "["
    BNE.S   .NDD
.DD:    MOVE.B  #1,.TDRF        ; THROUGH-DEREF x.y.z
    MOVE.B  D0,.ORIGD0
    MOVE.W  #3,D0           ; TDR MODE
    TST.L   D4
    BEQ.S   .DD1
    CMP.W   #4,D4
    BNE ERROR40
    BRA.S   .DD2
.DD1:   ADDQ.W  #4,D0
.DD2:   TST.W   D2
    BNE ERROR40
    BRA.S   .18

.NDD:
    TST.W   D4
    BNE.S   .NP
    CMP.W   #2,D0
    BEQ ERROR40
    ADD.W   #4,D0           ; SET TO GET POINTER TO SUBSTRUCT
    BRA.S   .18
.NP:BSR .MOVEQ
.18:CMP.L   #$10000,D0
    BPL     .23
    TST.L   D3
    BPL.W   .40

;; *** indexed version

    CMP.W   #ASSGN,(A3)
    BEQ     .ENHANCE0
    CMP.W   #11,2(A3)
    BEQ     .EXPEXT0
.E0_B0:
    LSL.W   #2,D0
    MOVE.L  .20(PC,D0.W),(A4)+
    LSR.W   #2,D0
.E0_B:
    TST.W   .THE_FACTOR
    BEQ     .E0_S0
    MOVE.W  .THE_FACTOR,D1
    LSL.W   #8,D1
    LSL.W   #1,D1
    OR.W    D1,-2(A4)
    CLR.W   .THE_FACTOR
.E0_S0:
    BSR .EXTL
    CMP.W   #2,D4
    BEQ .21
    BPL.S   .22
    TST.W   D4
    BEQ .DO         ; SKIP MOVE.X SET
    BCLR    #5,-4(A4)
    BRA .21
.22:BCLR    #4,-4(A4)
    BRA .21
.THE_FACTOR:
    DC.W    0
;; Enhance
.ENHANCE0:
    CMP.W   #2,D0
    BPL     .E0_B0
    ADDQ.L  #2,A3
    MOVE.L  .E0_S(PC),(A4)+
    BSR     EXP
    MOVE.L  .E0_R(PC),(A4)+
    MOVE.L  .20+8(PC),(A4)+
    BRA     .E0_B

.E0_S:  MOVEM.L  D2/A0,-(A7)
.E0_R:  MOVEM.L  (A7)+,D2/A0
;*-*
;; Codes
.20:MOVE.W  0(A0,D2.L),D0       ; LONG
    MOVE.W  0(A0,D2.L),D1
    MOVE.W  D0,0(A0,D2.L)
    MOVE.W  0(A0,D2.L),A0
    LEA 0(A0,D2.L),A0
    LEA 0(A0,D2.L),A0
    LEA 0(A0,D2.L),A0
    LEA 0(A0,D2.L),A0
;*-*
;; Extend
.EXPEXT0:
    CMP.W   #0,D0
    BNE     .E0_B0
    MOVE.W  (A3),D1
    CMP.W   #7,D1
    BMI     .E0_B0
    CMP.W   #10,D1
    BLE     .EXP_C0
    CMP.W   #49,D1
    BMI     .E0_B0
    CMP.W   #50,D1
    BLE     .EXP_C0
    CMP.W   #IOFF+29,D1
    BMI     .E0_B0
    CMP.W   #IOFF+30,D1
    BGT     .E0_B0
.EXP_C0:
    MOVE.W  (A3),2(A3)
    MOVE.W  #51,(A3)

    LSL.W   #2,D0
    MOVE.L  .20(PC,D0.W),(A4)+      ;
    LSR.W   #2,D0

    TST.W   .THE_FACTOR
    BEQ .EXP_C0_0
    MOVE.W  .THE_FACTOR,D1
    LSL.W   #8,D1
    LSL.W   #1,D1
    OR.W    D1,-2(A4)
.EXP_C0_0:
    BSR     .EXTL
    CMP.W   #2,D4
    BEQ.S   .EXP_C0_2
    BPL.S   .EXP_C0_1
    TST.W   D4
    BEQ     ERROR0                  ; compiler shouldn't use this code, anyway..
    BCLR    #5,-4(A4)               ; .B/.L size
    BRA.S   .EXP_C0_2
.EXP_C0_1:
    BCLR    #4,-4(A4)               ; same here
.EXP_C0_2:
    MOVE.L  D4,-(A7)
    MOVE.L  .EXP_ST2,(A4)+
    BSR     EXP
    MOVE.L  .EXP_RS2,(A4)+
    MOVE.L  (A7)+,D4
    MOVEQ   #2,D0
    MOVEQ   #0,D2
    BRA     .E0_B0
.EXP_ST2:
    MOVEM.L D2/A0,-(A7)
.EXP_RS2:
    MOVEM.L (A7)+,D2/A0
;*-*
;*-*
;; *** simple version

.23:CMP.W   #4,(A3)
    BEQ     .ENHANCE1
    CMP.W   #11,2(A3)
    BEQ     .EXPEXT1
.E1_B0:
    cmp.w   #4,d0
    bmi     .E1_B00
    tst.w   D4
    beq     .DO
    bra     .21
.E1_B00:
    LSL.W   #1,D0
    MOVE.W  .24(PC,D0.W),(A4)+
    LSR.W   #1,D0
.E1_B:
    BSR .EXTL
    CMP.W   #2,D4
    BEQ.W   .21
    BPL.S   .25
    TST.W   D4
    BEQ     .DO         ; SKIP MOVE.X SET
    BCLR    #5,-2(A4)
    BRA     .21
.25:BCLR    #4,-2(A4)
    BRA     .21
;; Enhance1 (:=)
.ENHANCE1:
    CMP.W   #2,D0
    BPL     .E1_B0
    ADDQ.L  #2,A3
    MOVE.W  .E1_S(PC),(A4)+
;    BSR     SD0
    BSR     EXP
;    BSR     RD0
    MOVE.W  .E1_R(PC),(A4)+
    MOVE.W  .24+4(PC),(A4)+
    BRA     .E1_B

.E1_S:  MOVE.L  A0,-(A7)
.E1_R:  MOVE.L  (A7)+,A0
;*-*
;; Codes
.24:MOVE.W  (A0),D0         ; a:=b[]
    MOVE.W  (A0),D1         ; a:=b+c[]
    MOVE.W  D0,(A0)         ; a[]:=b...
    MOVE.W  (A0),A0         ; a[][]?
    LEA (A0),A0
    LEA (A0),A0
    LEA (A0),A0
    LEA (A0),A0
;*-*
;; Extended (+=, -=, etc.)
.EXPEXT1:
    CMP.W   #0,D0
    BNE     .E1_B0
    MOVE.W  (A3),D1
    CMP.W   #7,D1
    BMI     .E1_B0
    CMP.W   #10,D1
    BLE     .EXP_C1
    CMP.W   #49,D1
    BMI     .E1_B0
    CMP.W   #50,D1
    BLE     .EXP_C1
    CMP.W   #IOFF+29,D1
    BMI     .E1_B0
    CMP.W   #IOFF+30,D1
    BGT     .E1_B0
.EXP_C1:
    MOVE.W  (A3),2(A3)
    MOVE.W  #51,(A3)

    LSL.W   #1,D0
    MOVE.W  .24(PC,D0.W),(A4)+      ;
    LSR.W   #1,D0

    BSR     .EXTL
    CMP.W   #2,D4
    BEQ.S   .EXP_C1_2
    BPL.S   .EXP_C1_1
    TST.W   D4
    BEQ     ERROR0                  ; compiler shouldn't use this code, anyway..
    BCLR    #5,-2(A4)               ; .B/.L size
    BRA.S   .EXP_C1_2
.EXP_C1_1:
    BCLR    #4,-2(A4)               ; same here
.EXP_C1_2:
    MOVE.L  D4,-(A7)
    MOVE.W  .EXP_ST,(A4)+
    BSR     EXP
    MOVE.W  .EXP_RS,(A4)+
    MOVE.L  (A7)+,D4
    MOVEQ   #2,D0
    MOVEQ   #0,d2
    BRA     .E1_B0
;*-*
;*-*
;; *** relative(a0) version

.40:CMP.W   #ASSGN,(A3)
    BEQ     .ENHANCE2

    CMP.W   #11,2(A3)
    BEQ     .EXPEXT2
.E2_B:
    CMP.W   #4,D0
    BMI     .E2_B0
    CMP.W   #8,D3
    BGT     .E2_B0
    CMP.W   #-8,D3
    BMI     .E2_B0
    move.l  d0,-(a7)
    tst.w   d3
    smi     d0
    eor.l   d0,d3
    and.l   #$100,d0
    or.l    #$5088,d0
    move.w  d0,(a4)
    and.l   #7,d3
    lsl.w   #8,d3
    lsl.w   #1,d3
    or.w    d3,(a4)+
    move.l  (a7)+,d0
    tst.w   d4
    beq     .DO
    bra     .21

.E2_B0
    LSL.W   #2,D0
    MOVE.W  .42(PC,D0.W),(A4)+      ;
    LSR.W   #2,D0
.E2_B1:

    MOVE.W  D3,(A4)+
    BSR .EXTL
    CMP.W   #2,D4
    BEQ     .21
    BPL.S   .41
    TST.W   D4
    BEQ     .DO                     ; SKIP MOVE.X SET
    BCLR    #5,-4(A4)               ; .B/.L size
    BRA     .21
.41:BCLR    #4,-4(A4)               ; same here
    BRA     .21

;; Enhanced (assigns in lists)
.ENHANCE2:
    CMP.W   #2,D0
    BPL     .E2_B
    ADDQ.L  #2,A3
    MOVE.W  .E2_S(PC),(A4)+
    MOVE.L  D3,-(A7)
    BSR     EXP
    MOVE.L  (A7)+,D3
    MOVE.W  .E2_R(PC),(A4)+
    MOVE.W  .42+8(PC),(A4)+
    BRA     .E2_B1

.E2_S:  MOVE.L  A0,-(A7)
.E2_R:  MOVE.L  (A7)+,A0
;*-*
;; Codes
.42:MOVE.W  2(A0),D0
    MOVE.W  2(A0),D1
    MOVE.W  D0,2(A0)
    MOVE.W  2(A0),A0
    LEA 2(A0),A0
    LEA 2(A0),A0
    LEA 2(A0),A0
    LEA 2(A0),A0
;*-*
;; Extended (+=, -=, etc.)
.EXPEXT2:
    CMP.W   #0,D0
    BNE     .E2_B
    MOVE.W  (A3),D1
    CMP.W   #7,D1
    BMI     .E2_B
    CMP.W   #10,D1
    BLE     .EXP_C2
    CMP.W   #49,D1
    BMI     .E2_B
    CMP.W   #50,D1
    BLE     .EXP_C2
    CMP.W   #IOFF+29,D1
    BMI     .E2_B
    CMP.W   #IOFF+30,D1
    BGT     .E2_B
.EXP_C2:
    MOVE.W  (A3),2(A3)
    MOVE.W  #51,(A3)

    LSL.W   #2,D0
    MOVE.W  .42(PC,D0.W),(A4)+      ;
    LSR.W   #2,D0

    MOVE.W  D3,(A4)+
    BSR     .EXTL
    CMP.W   #2,D4
    BEQ.S   .EXP_C2_2
    BPL.S   .EXP_C2_1
    TST.W   D4
    BEQ     ERROR0                  ; compiler shouldn't use this code, anyway..
    BCLR    #5,-4(A4)               ; .B/.L size
    BRA.S   .EXP_C2_2
.EXP_C2_1:
    BCLR    #4,-4(A4)               ; same here
.EXP_C2_2:
    MOVem.L  D3/D4,-(A7)
    MOVE.W  .EXP_ST,(A4)+
    BSR     EXP
    MOVE.W  .EXP_RS,(A4)+
    MOVEQ   #2,D0
    MOVEM.L (A7)+,D3/D4
    moveq   #0,d2
    BRA     .E2_B

.EXP_ST:
    MOVE.L  A0,-(A7)
.EXP_RS:
    MOVE.L  (A7)+,A0
;*-*
;*-*

;; LAST PARTS:

.DO:SUBQ.W  #4,D0           ; DO ADDITIONAL MOVE
    TST.B   .TDRF
    BNE     .DLOOP          ; try to optimize
    cmp.w   #1,d0
    ble     .doopti
.do21p:
    LSL.W   #1,D0
    move.l  a0,-(a7)
    lea     .MA0(PC),a0
    MOVE.W  0(a0,D0.W),(A4)+
    move.l  (a7)+,a0
    LSR.W   #1,D0

.21:    TST.B   .TDRF
    BNE     .DLOOP
    MOVEQ   #1,D1           ; OUT!
    BSR GENID
    BRA XIT

.doopti:
    move.w  -2(A4),d1
    and.w   #$FFF8,d1
    cmp.w   #$2040,d1
    beq     .dodo2
    cmp.w   #$206d,-4(a4)
    beq     .dodo1
    cmp.w   #$206c,-4(a4)
    beq     .dodo1
    bra     .do21p
.dodo1:
    subq.l  #4,a4
    move.l  a4,LAST_CMD_ADR
    addq.l  #4,a4
    move.w  -4(a4),d1
    and.w   #$F,D1
    or.w    #$2020,d1
    move.w  d1,-4(a4)
    lsl.w   #8,d0
    lsl.w   #1,d0
    or.w    d0,-4(a4)
    lsr.w   #8,d0
    lsr.w   #1,d0
    bra     .21
.dodo2:
    subq.l  #2,a4
    move.l  a4,LAST_CMD_ADR
    addq.l  #2,a4
    move.w  -2(a4),d1
    and.w   #$FFBF,D1
    lsl.w   #8,d0
    lsl.w   #1,d0
    move.w  d1,-2(a4)
    or.w    d0,-2(a4)
    lsr.w   #8,d0
    lsr.w   #1,d0
    bra     .21


.DLOOP: MOVE.W  .KEEPD5,D5
    EXT.L   D5
    MOVE.W  (A3)+,D1
    MOVE.B  .ORIGD0,D0
    BRA.W   .DREFL

.SPEC:  DC.L    0           ; 2 LONGWORDS <-> SAVE
.TDRF:  DC.B    0
.ORIGD0:DC.B    0
.KEEPD5:DC.W    0
.SAVA0: MOVE.L  A0,-(A7)
.RETA0: MOVE.L  (A7)+,A0
.OPTID2:MOVE.L  2(A5),D2

;*-*
;; CODES:

.19:MOVEQ   #0,D0
    MOVEQ   #0,D1
.19A:
    EXT.W   D0
    EXT.W   D1
.19B:
    EXT.L   D0
    EXT.L   D1
.19C:
    EXTB.L  D0
    EXTB.L  D1

.MA0:
    MOVE.L  A0,D0
    MOVE.L  A0,D1
.26:MOVE.L  D0,-(A7)
.27:MOVE.L  (A7)+,D2
    EXG.L   D0,D2
.28:MULU    #1,D2
.29:LSL.L   #1,D2
    LSL.L   #2,D2
    LSL.L   #3,D2
    LSL.L   #4,D2
.30:    MOVE.L  D0,D2


.MOVEQ: CMP.W   #4,D4           ; see if we need a "moveq #0,D0/D1"
    BEQ.S   .MOVEX
    CMP.W   #2,D0
    BPL.S   .MOVEX
    CMP.W   #2,D4
    BEQ.S   .MOVEX
    LSL.L   #1,D0
    MOVE.W  .19(PC,D0.W),(A4)+
    LSR.L   #1,D0
.MOVEX: RTS

.EXTL:  CMP.W   #4,D4           ; see if we need a "EXT.L D0/D1"
    BEQ.S   .EXTX
    CMP.W   #2,D0
    BPL.S   .EXTX
    CMP.W   #1,D4
    BEQ.S   .EXTX
    LSL.L   #1,D0
    MOVE.W  .19B(PC,D0.W),(A4)+
    LSR.L   #1,D0
.EXTX:  RTS

.EXTBL:
    CMP.W   #1,ECPU
    BPL     .EXTBL_2
    LSL.L   #1,D0
    MOVE.W  .19A(PC,D0),(A4)+
    MOVE.W  .19B(PC,D0),(A4)+
    LSR.L   #1,D0
    RTS
.EXTBL_2:
    LSL.L   #1,D0
    MOVE.W  .19C(PC,D0),(A4)+
    LSR.L   #1,D0
    RTS

.EADDRMETHOD2:
    MOVE.L  (A3),A6
    MOVE.L  OASCII(A6),(A3)
    MOVE.W  #42,-(A3)
.EADDRMETHOD:
    MOVE.L  A0,METHODIDENT
    CLR.L   METHODSPEC
    TST.B   .TDRF
    BEQ.S   .NMS
    MOVE.L  .SPEC,METHODSPEC
.NMS:   MOVEM.L (A7)+,D3-D5/A1
    BSR SD0
    BSR EXPCALLMETHOD
    BSR RD0
    MOVEQ   #1,D5
    ADDQ.L  #4,A7           ; FLUSH RTS
    BRA EXPLOOP


PLX:    CMP.W   #35,D1          ; p++  p--
    BPL PLB
    SUBQ.L  #2,A3
    MOVE.L  (A0),D5         ; D5=OBJSIZE
    CMP.L   #MAXOBJSIZE,D5
    BMI.S   .NOBJ
    MOVE.L  D5,A6
    MOVE.W  4(A6),D5
    EXT.L   D5
.NOBJ:  MOVEQ   #4,D6           ; D6=NOPTR
    BRA.S   PLS

PL: MOVEQ   #0,D6           ; D6=PTR
    MOVEQ   #4,D5           ; D5=4(SIZE)
    CMP.W   #IDENT,(A3)+        ; ^p  ^p++  ^p--
    BNE ERROR6
    MOVE.L  (A3)+,A0        ; A0=IDENT
    TST.B   4(A0)
    BEQ ERROR22
    CMP.B   #LAB,4(A0)
    BEQ ERROR6
PLS:    BSR.W   PP

    MOVEQ   #2,D1
    BSR GENID

.6: TST.W   D6
    BEQ.S   .6B
    CMP.W   #1,EXPRECC
    BNE.S   .6B
    TST.W   EXPSTAT
    BNE .SPTR
.6B:
    TST.L   D6
    BNE.S   .6BB
    MOVE.W  .2(PC),(A4)+
    BRA.S   .6BBB
.6BB:   MOVE.W  D0,D3
    LSL.W   #2,D3
    MOVE.W  .2B(PC,D3.W),(A4)+
.6BBB:  BSR GVA0D3_0

    MOVE.W  D0,D3
    LSL.W   #1,D3
    TST.L   D6
    BNE.S   .SPTR
    MOVE.W  .3(PC,D3.W),(A4)+
.SPTR:
    MOVEQ   #1,D1
    BSR GENID

.7: BRA XIT
.2: MOVE.L  2(A5),A0
.2B:    MOVE.L  2(A5),D0
    MOVE.L  2(A5),D1
.3: MOVE.L  (A0),D0
    MOVE.L  (A0),D1
    MOVE.L  D0,(A0)

PP: MOVEQ   #0,D2           ; D2 (1=++, 2=--, 0=NIX)
    MOVE.W  (A3),D1
    CMP.W   #33,D1
    BNE.S   .P1
    MOVEQ   #1,D2
    ADDQ.L  #2,A3
.P2:    RTS
.P1:    CMP.W   #34,D1
    BNE.S   .P2
    MOVEQ   #2,D2
    ADDQ.L  #2,A3
    RTS
GEND2:  MOVEQ   #-1,D3
    CMP.L   #30000,D1       ; VALUE IN D1, GENCODE FOR D2 (ONLY POSITIVE)
    BPL.S   .G1
    TST.L   D1
    BEQ.S   .G9
    MOVE.L  D1,D3
    RTS
.G9:    ADD.L   #$10000,D0
    RTS
.G1:    MOVE.W  .G3(PC),(A4)+
    MOVE.L  D1,(A4)+
    RTS
.G3:    MOVE.L  #0,D2
GENID:  CMP.L   D1,D2
    BEQ.S   .G4
    RTS
.G4:    SUBQ.L  #1,D1
    MOVE.W  D5,D4           ; d5=objsize, d4=trash
    EXT.L   D4
    CMP.L   #9,D4
    BPL .G5
    CMP.W   #8,D4
    BNE.S   .G8
    MOVEQ   #0,D4
.G8:    LSL.L   #2,D1
    MOVE.W  .G6(PC,D1.L),(A4)+
    LSL.W   #1,D4
    OR.B    D4,-2(A4)
    BRA GVA0D1_0
.G6:    ADDQ.L  #8,2(A5)
    SUBQ.L  #8,2(A5)
.G7:    ADD.L   #1,2(A5)
    SUB.L   #1,2(A5)
.G5:    LSL.L   #3,D1
    MOVE.W  .G7(PC,D1.W),(A4)+
    BSR GVA0D1_0
    BTST    #3,5(A0)
    BEQ.S   .GXX
    MOVE.L  D4,(A4)+
    RTS
.GXX:   MOVE.W  -(A4),D1
    MOVE.L  D4,(A4)+
    MOVE.W  D1,(A4)+
    RTS
GENADD: TST.L   D1          ; value in d1
    BNE.S   .GA1
    RTS
.GA1:   CMP.L   #9,D1
    BPL.S   .GA5
    CMP.W   #8,D1
    BNE.S   .GA8
    MOVEQ   #0,D1
.GA8:   MOVE.W  .GA6(PC),(A4)+
    LSL.W   #1,D1
    OR.B    D1,-2(A4)
    RTS
.GA5:   MOVE.W  .GA7(PC),(A4)+
    MOVE.L  D1,(A4)+
    RTS
.GA6:   ADDQ.L  #8,D2
.GA7:   ADD.L   #1,D2


XIT:    MOVEM.L (A7)+,D3-D5/A1
    RTS

;*-*
;; NilCheck
; WERE NOT TO DISTURB ANY REGS, NOT EVEN IN GENERATED CODE (D1?)
; (IN CODE): PTR TO CHECK IS IN A0

NILCHECK:
    MOVEM.L D0/D1/A0/A6,-(A7)
    MOVE.L  .CH(PC),(A4)+
    MOVE.L  A4,A6           ; A6=BRANCH BACKPATCH
    MOVE.L  .ARGS(PC),(A4)+
    MOVE.L  .ARGS+4(PC),(A4)+
    MOVE.W  LINENUM(PC),D0
    ADDQ.W  #1,D0
    MOVE.W  D0,(A4)+
    MOVEQ   #90+10,D0       ; D0=FASTNEW
    MOVE.B  #-1,EFUNCBYTE+90    ; fastnew=91
    MOVE.L  .4(PC),(A4)+        ; GEN CALL
    TSTMOD
    BNE.S   .MOD
    MOVE.W  .5(PC),NEWOP
    BSR ADDBRANCH
    BRA.S   .CC
.MOD:   SUBQ.L  #4,A4
    MOVE.W  .5(PC),(A4)+
    BSR ADDBRANCHRELOC
    MOVE.W  D0,-2(A4)
.CC:    MOVE.W  .SB(PC),(A4)+
    MOVE.L  A4,D0
    SUB.L   A6,D0
    MOVE.B  D0,-(A6)
    MOVEM.L (A7)+,D0/D1/A0/A6
    RTS
.CH:    MOVE.L  A0,D1
    BNE.S   .CH
.ARGS:  MOVE.L  #"NIL",-(A7)
    PEA $10.W
.SB:    ADDQ.L  #8,A7
.5: JSR .CH
.4: BSR.W   .CH
;*-*
OBJ_EXECBASE:
    DC.L    0
VAR_EXECBASE:
    DC.L    0
OBJ_DOSBASE:
    DC.L    0
VAR_DOSBASE:
    DC.L    0
OBJ_INTUIBASE:
    DC.L    0
VAR_INTUIBASE:
    DC.L    0
OBJ_GFXBASE:
    DC.L    0
VAR_GFXBASE:
    DC.L    0
OBJ_RASTPORT:
    DC.L    0
VAR_RASTPORT:
    DC.L    0
OBJ_WBMESSAGE:
    DC.L    0
VAR_WBMESSAGE:
    DC.L    0
LAST_CMD_ADR:
    DC.L    0
;*-*
;; Expression main
EXPRECC:  DC.W    0       ; RECURSION DEPTH OF EXP()
;; Main expression loop
EXP:              ; GENERATES EXP FOR D0
    BCLR    #2,ICODEPREFS+3
    MOVE.L  #0,LASTVAR
    MOVEM.L D3-D5/A1/A2,-(A7)   ; WE DO HEAVY RECURSION
    MOVE.W  FLTFLAG(PC),-(A7)
    CLR.W   FLTFLAG
    ADDQ.W  #1,EXPRECC
    MOVEQ   #6,D4           ;
    MOVEQ   #-1,D5          ; MUST BE = EXPBUT
    LEA EXPMOVE(PC),A1      ;

    CMP.W   #51,(A3)
    BNE     EXPLOOP
    MOVEQ   #1,D5           ; variable
    ADDQ.L  #2,A3

EXPLOOP:
    MOVE.W  (A3)+,D0
    BEQ EXPOUT
    CMP.W   #COM,D0
    BEQ EXPOUT
    CMP.W   #7,D0
    BPL EXPOPERATOR
EXPBACK:
    CMP.W   #IDENT,D0
    BEQ EXPVAR
    CMP.W   #STR,D0
    BEQ EXPSTRING
    CMP.W   #VALUE,D0
    BEQ EXPVALUE
    CMP.W   #11,D0
    BPL EXPCOMPARATOR
EXPBACK2:
    CMP.W   #17,D0
    BEQ EXPREC
    CMP.W   #55,D0
    BEQ EXPLOOP
    CMP.W   #LIBC,D0
    BEQ EXPLIBCALL
    CMP.W   #22,D0
    BEQ EXPEFUNC
    CMP.W   #23,D0
    BEQ CURLY
    CMP.W   #29,D0
    BEQ EXPIMMLIST
    CMP.W   #18,D0
    BEQ EXPOUT
    CMP.W   #30,D0
    BEQ EXPOUT
    CMP.W   #53,D0
    BEQ     EXPSWAP
    CMP.W   #32,D0
    BEQ.W   EXPPTR
    CMP.W   #IOFF+29,D0
    BPL EXPLOG
ANDORNOTBACK:
    CMP.W   #19,D0
    BEQ EXPOUT
    CMP.W   #IOFF+2,D0
    BEQ EXPIF
    CMP.W   #IOFF+50,D0
    BEQ EXPSIZEOF
    CMP.W   #36,D0
    BEQ EXPQUOTE
    CMP.W   #37,D0
    BEQ EXPFLT
    CMP.W   #IOFF+63,D0
    BEQ EXPNEW
    CMP.W   #IOFF+55,D0
    BEQ EXPBUT
    CMP.W   #43,D0
    BEQ EXPFLOATVAL
    CMP.W   #40,D0
    BEQ EXPUNIFY
    CMP.W   #IOFF+66,D0
    BEQ EXPSUPER
    CMP.W   #46,D0
    BEQ EXPOUT
    CMP.W   #49,D0
    BEQ EXPSHIFT
    CMP.W   #50,D0
    BEQ EXPSHIFT
    CMP.W   #54,D0
    BEQ EXPLIBFUNC
    CMP.W   #52,D0
    BEQ EXPQMARK            ; ?
    
    CMP.W   #IOFF,D0
    BMI ERROR0
EXPOUT:
    TST.W   FLTFLAG
    BEQ .0
    BTST    #1,CODEPREFS
    BEQ .0
    CMP.L   #EXPCMP,A1
    BEQ .0
    move.l  a4,LAST_CMD_ADR
    MOVE.L  .C0,(A4)+
.0:
    SUBQ.L  #2,A3
    CMP.W   #1,D5
    BNE ERROR0
    SUBQ.W  #1,EXPRECC
    MOVE.W  (A7)+,FLTFLAG
    MOVEM.L (A7)+,D3-D5/A1/A2
    RTS
.C0:FMOVE.S FP0,D0
;*-*
;; "^"
EXPPTR:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    MOVEQ   #1,D0
    SUBQ.L  #2,A3
    BSR EADDRESSMODI
    CMP.W   #-1,D5
    BEQ.S   .1
    BSR CLOSEEXP
.1: MOVEQ   #1,D5
    BRA EXPLOOP
;*-*
;; BUT
EXPBUT:
    CMP.W   #1,D5
    BNE ERROR0
;   MOVE.W  (A3),D0
;   CMP.W   #IOFF+29,D0
;   BEQ.S   .A
;   CMP.W   #IOFF+30,D0
;   BEQ.S   .O
    MOVEQ   #6,D4           ;
    MOVEQ   #-1,D5          ; = START OF EXP
    LEA EXPMOVE(PC),A1      ;
    BRA EXPLOOP
;.A:    MOVE.W  .EQ(PC),D0
;   BRA.S   .S
;.O:    MOVE.W  .NE(PC),D0
;.S:    ADDQ.L  #2,A3
;   MOVE.W  .TST(PC),(A4)+
;   MOVE.W  D0,(A4)+
;   MOVE.L  A4,A0
;   CLR.W   (A4)+
;   MOVE.L  A0,-(A7)
;   BSR.W   EXP
;   MOVE.L  (A7)+,A0
;   MOVE.L  A4,D0
;   SUB.L   A0,D0
;   MOVE.W  D0,(A0)
;   MOVEQ   #1,D5
;   BRA EXPLOOP
;.EQ:   BEQ.W   .A
;.NE:   BNE.W   .A
;.TST:  TST.L   D0

;*-*
;; Macro section
; RETURNS: 0=LIST, 1,2,4=SIMPLE, PTR=OBJECT

GTYPE:  MACRO               ; \1=TRASH.AX, \2=RESULT.DX
    MOVEQ   #0,\2
    MOVE.L  (A3)+,\1
    ADD.L   A3,\1
    SUBQ.L  #4,\1
    CMP.W   #19,(\1)+
    BNE.S   .GTOUT
    MOVE.W  (\1)+,\2
    CMP.W   #IOFF+20,\2
    BMI.S   .GT1
    CMP.W   #IOFF+23,\2
    BPL.S   .GT1
    SUB.W   #IOFF+20,\2
    MOVE.B  .GTTAB(PC,\2.W),\2
    EXT.W   \2
    EXT.L   \2
    BRA.S   .GTOUT
.GT1:   CMP.W   #31,\2
    BNE.S   .GTER
    MOVE.L  (\1),\2
    BRA.S   .GTOUT
.GTTAB: DC.B    4,2,1,0
.GTER:  MOVE.L  \1,A3
    BRA ERROR0
.GTOUT:
    ENDM

GEAT:   MACRO               ; \1=THRASH.DX
    CMP.W   #19,(A3)
    BNE.S   .GTDONE
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,\1
    CMP.W   #IOFF+20,\1
    BMI.S   .GT9
    CMP.W   #IOFF+23,\1
    BMI.S   .GTDONE
.GT9:   CMP.W   #31,\1
    BNE ERROR0
    ADDQ.L  #4,A3
.GTDONE:
    ENDM

MOVED0FP0: MACRO
    tst.w   FLTFLAG
    beq     .NOMOVE
    btst    #1,CODEPREFS
    beq     .NOMOVE
    move.l  A4,LAST_CMD_ADR
    MOVE.L  .D0FP0(pc),(a4)+
    bra     .NOMOVE
.D0FP0:
    fmove.s d0,fp0
.NOMOVE:
    ENDM
;*-*
;; SUPER
EXPSUPER:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    BSR SD0
    BSR SUPEREXP
    BSR RD0
    MOVEQ   #1,D5
    BRA EXPLOOP
SUPEREXP:
    CMP.W   #IDENT,(A3)+
    BNE ERROR0
    MOVE.L  (A3)+,A0        ; A0=IDENT
    MOVE.L  A0,METHODIDENT
    TST.B   4(A0)
    BEQ ERROR22
    CMP.B   #LAB,4(A0)
    BEQ ERROR6
    MOVE.L  (A0),A6         ; A6=OBJ
    CMPA.L  #MAXOBJSIZE,A6
    BMI.W   ERROR33
    MOVE.L  OSUPER(A6),D0
    BEQ ERROR50
    MOVE.L  D0,A6           ; A6=SUPER
    TST.W   ODEL(A6)
    BEQ ERROR40
    CMP.W   #35,(A3)+
    BNE ERROR0
    MOVE.L  A6,METHODFREEZ
    CLR.L   METHODSPEC
    BSR EXPCALLMETHOD       ; call SUPER-METHOD
    MOVE.L  METHODFREEZ(PC),A6
    CLR.L   METHODFREEZ
    RTS
;*-*
;; NEW
EXPNEW:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    BSR.W   SD0
    MOVEQ   #0,D0
    BSR.S   NEWEXP
    BSR.W   RD0
    MOVEQ   #1,D5
    BRA EXPLOOP
NEWEXP:
    MOVE.W  D0,-(A7)
.NLOOP: CMP.W   #IDENT,(A3)+
    BNE .NL
    MOVE.L  (A3)+,A0        ; A0=IDENT
    TST.B   4(A0)
    BEQ ERROR22
    CMP.B   #LAB,4(A0)
    BEQ ERROR6

    MOVE.W  #-1,.ISM

    MOVE.L  (A0),D0         ; D0=OBJECTSIZE
    SUB.L   A6,A6           ; A6=OBJECT | NIL
    CMP.L   #5,D0
    BMI.S   .0
    MOVE.L  D0,A6
    MOVE.W  OSIZE(A6),D0
    EXT.L   D0

    CMP.W   #35,(A3)
    BNE.S   .0
    CMP.W   #39,2(A3)
    BNE .0
    ADDQ.L  #4,A3
    MOVE.L  A1,-(A7)
    MOVE.L  (A3)+,A1        ; A1=MEMBER
    BSR FINDMEMBER      ; D1=TRASH,a1=first->REAL,a6=obj
    MOVE.W  OOFF(A1),.ISM
    BTST    #1,OFLAGS(A1)
    BEQ ERROR40
    CMP.W   #4,OSIZE(A1)
    BNE ERROR40
    SUB.L   A6,A6
    MOVE.L  OPTRTYPE(A1),D0
    BEQ.W   ERROR40
    BMI.S   .OO
    CMP.L   #5,D0
    BMI.S   .CON
.OO:    MOVE.L  D0,A6
    MOVE.W  OSIZE(A6),D0
    EXT.L   D0
.CON:   MOVE.L  (A7)+,A1
.0:
    CMP.W   #29,(A3)
    BNE.S   .1
    ADDQ.L  #6,A3           ; ALSO SKIP ]OFFSET
    MOVEM.L A0/D0/A6,-(A7)      ; NEW p[exp]
    BSR EXP
    MOVEM.L (A7)+,A0/D0/A6
    CLR.W   .SINGL
    CMP.W   #30,(A3)+
    BNE ERROR34

    CMP.W   #1,D0
    BEQ.S   .NM
    CMP.W   #2,D0
    BNE.S   .N1
    MOVE.W  .LSL1(PC),(A4)+
    BRA.S   .NM
.N1:    CMP.W   #4,D0
    BNE.S   .N2
    MOVE.W  .LSL2(PC),(A4)+
    BRA.S   .NM
.N2:    MOVE.W  .M(PC),(A4)+
    MOVE.W  D0,(A4)+

.NM:    MOVE.W  .S1(PC),(A4)+
    BRA.S   .2

.1: MOVE.W  .S2(PC),(A4)+       ; just NEW p
    MOVE.W  D0,(A4)+
    MOVE.W  #1,.SINGL

.2: MOVE.L  D0,D1           ; D1=OBJECTSIZE NOW
    BSR .GNEW
    MOVE.W  .A(PC),(A4)+

    MOVE.W  .ISM(PC),D1
    BPL.W   .A0O

    MOVE.W  .ST(PC),(A4)+
    BSR GVA0D0_9
    BRA.S   .NA0O

.A0O:   MOVE.W  .ST2(PC),(A4)+
    BSR GVA0D0_0
    MOVE.W  .ST3(PC),(A4)+
    MOVE.W  D1,(A4)+
.NA0O:
    TST.W   .SINGL
    BEQ.W   .3

    MOVE.L  A6,D0
    BEQ.W   .4B
    TST.W   ODEL(A6)
    BEQ.W   .4B
    MOVE.L  A0,METHODIDENT
    GETM    A0
    MOVE.L  OACC(A6),(A0)
    MOVE.L  A0,OACC(A6)
    ADDQ.L  #4,A0
    MOVE.W  .GOBJ(PC),(A4)+
    TST.W   ODELOFF(A6)
    BNE.S   .G1
    MOVE.W  .GOBJ2(PC),(A4)+
    MOVE.L  A4,(A0)+
    CLR.W   (A4)+
    BRA.S   .G2
.G1:    MOVE.W  .GOBJ3(PC),(A4)+
    MOVE.L  A4,(A0)+
    CLR.W   (A4)+
    MOVE.W  ODELOFF(A6),(A4)+
.G2:    CLR.W   (A0)+
    DONEM   A0
    CMP.W   #35,(A3)
    BNE.S   .4B
    ADDQ.L  #2,A3

    CLR.L   METHODSPEC
    TST.W   .ISM
    BMI.S   .NMS
    MOVE.L  A6,METHODSPEC
.NMS:
    MOVE.L  METHODIDENT(PC),-(A7)
    BSR EXPCALLMETHOD       ; call constructor on memory
    MOVE.L  (A7)+,METHODIDENT
    TST.W   (A7)
    BNE.S   .4B
    MOVE.W  .MV(PC),(A4)+
    MOVE.L  METHODIDENT(PC),A0
    BSR GVA0D0_0
    BRA.S   .4B
.3:
    MOVE.L  A6,D0
    BEQ.S   .4B
    TST.W   ODEL(A6)
    BNE ERROR40
.4B:
.XXXX:  TST.W   (A7)
    BEQ.S   .X1
    CMP.W   #COM,(A3)+
    BEQ .NLOOP
    SUBQ.L  #2,A3
.X1:    ADDQ.L  #2,A7
    RTS

.SINGL: DC.W    0
.ISM:   DC.W    0
.MV:    MOVE.L  2(A5),D0
.M: MULU    #6,D0
.LSL1:  LSL.L   #1,D0
.LSL2:  LSL.L   #2,D0
.S1:    MOVE.L  D0,-(A7)
.S2:    PEA 1.W
.5: JSR .M
.4: BSR.W   .M
.A: ADDQ.L  #4,A7
.ST:    MOVE.L  D0,2(A5)
.ST2:   MOVE.L  2(A5),A0
.ST3:   MOVE.L  D0,4(A0)
;.ST4:  MOVE.L  D0,A0
.GOBJ:  MOVE.L  D0,A0
.GOBJ2: MOVE.L  4(A4),(A0)
.GOBJ3: MOVE.L  4(A4),4(A0)

; GNEW TRASHES NOTHING

.GNEW:  MOVEM.L D0/D1/A0,-(A7)
    MOVEQ   #110+10,D0      ; D0=FASTNEW
    MOVE.B  #-1,EFUNCBYTE+110   ; fastnew=111
    MOVE.L  .4(PC),(A4)+        ; GEN CALL
    TSTMOD
    BNE.S   .MOD
    MOVE.W  .5(PC),NEWOP
    BSR ADDBRANCH
    BRA.S   .CC
.MOD:   SUBQ.L  #4,A4
    MOVE.W  .5(PC),(A4)+
    BSR ADDBRANCHRELOC
    MOVE.W  D0,-2(A4)
.CC:    MOVEM.L (A7)+,D0/D1/A0
    RTS

.AD:    MOVE.L  D0,(A7)
.ALLD:  MOVE.L  (A7)+,D0
.LOAD:  MOVE.L  (A7),A6

.NL:    CMP.W   #29,-2(A3)
    BNE ERROR6
    MOVE.W  .S2(PC),(A4)+
    MOVE.L  A4,A6           ; A6=BACKPATCH
    CLR.W   (A4)+
    BSR .GNEW
    MOVE.W  .AD(PC),(A4)+
    MOVEQ   #0,D6           ; D6=TOTAL MEM USED
    GTYPE   A0,D7           ; D7=OBJECT OR NIL
    MOVE.L  D7,D2           ; D2=ADDSIZE IF NIL, ELSE CURMEMBER
    AND.L   #7,D2
    CMP.L   D2,D7
    BNE.S   .TIO
    MOVEQ   #0,D7
    MOVE.L  D2,D1           ; D1=STORESIZE
    TST.L   D2
    BNE.S   .TTD
    MOVEQ   #4,D2           ; true list
    MOVEQ   #4,D6           ; 4 EXTRA, ONLY 2 USED!
    BRA.S   .TTD
.TIO:   MOVE.L  D7,A0
    TST.W   (A0)
    BNE ERROR60
    MOVE.L  OMEMB(A0),D2
    MOVE.L  D2,A0
    MOVE.W  (A0),D6
    EXT.L   D6
.TTD:   CMP.W   #30,(A3)
    BEQ.S   .NLO
.NLL:   TST.L   D7
    BEQ.S   .NOBJ
    TST.L   D2
    BEQ ERROR46
    MOVE.L  D2,A0
    ;MOVE.L OASCII(A0),D1       ; PRIVATE MEMBER IN OBJECT
    ;BEQ    ERROR60
    ;BTST   #0,OFLAGS(A0)
    ;BNE    ERROR60
    MOVE.W  OSIZE(A0),D1        ; ARRAY IN OBJECT
    BEQ ERROR40
.NOBJ:  MOVEM.L D1/D2/D6/D7/A6,-(A7)
    BSR EXP
    MOVEM.L (A7)+,D1/D2/D6/D7/A6
    MOVE.W  .LOAD(PC),(A4)+
    MOVE.B  .TAB(PC,D1.W),D0
    EXT.W   D0
    MOVE.W  .STORE(PC,D0.W),(A4)+
    MOVE.W  D6,(A4)+
    TST.L   D7
    BNE.S   .TOBJ
    ADD.L   D2,D6
    BRA.S   .NLO
.STORE: MOVE.B  D0,2(A6)
    MOVE.W  D0,2(A6)
    MOVE.L  D0,2(A6)
.TAB:   DC.B    8,0,4,0,8,0
.TOBJ:  MOVE.L  D2,A0
    MOVE.L  -(A0),A0
    MOVE.L  A0,D2           ; stop if last in memberchain
    BEQ.S   .TEND
    MOVE.W  (A0),D6
    EXT.L   D6
    BRA.S   .NLO
.TEND:  CMP.W   #30,(A3)+
    BNE ERROR0
    BRA.S   .TC
.NLO:   CMP.W   #COM,(A3)+      ; end of loop
    BEQ.S   .NLL
    CMP.W   #30,-2(A3)      ; expect an "]"
    BNE ERROR0
.TC:    TST.L   D7
    BNE.S   .SK1A           ; only if its a list, NO OBJ, SIZE=4
    CMP.W   #4,D2
    BNE.S   .SK1
    CMP.W   #19,(A3)        ; IF ":" THEN IT WASN'T A LIST
    BEQ.S   .SK1
    MOVE.L  .SL(PC),(A4)+
    MOVE.L  D6,D0
    LSR.L   #2,D0
    SUBQ.L  #1,D0
    MOVE.W  D0,(A4)+
    MOVE.W  D0,(A4)+
    MOVE.W  .SL2(PC),(A4)+
    BRA.S   .SK2
.SK1A:  MOVE.L  D7,A0
    MOVE.W  OSIZE(A0),D6
    BEQ ERROR40
    EXT.L   D6
.SK1:   MOVE.W  .ALLD(PC),(A4)+
.SK2:   MOVE.W  D6,(A6)
    GEAT    D0
    AND.L   #$FFFF8000,D6
    BNE ERROR46
    BRA.W   .XXXX
.SL:    MOVE.L  (A7)+,A6
    MOVE.L  #2,(A6)+
.SL2:   MOVE.L  A6,D0
;*-*
;; "?"
EXPQMARK:
    CMP.W   #1,D5
    BNE     ERROR0
    bset    #2,ICODEPREFS+3
    MOVE.W  #$4A80,(A4)+        ; TST.L D0
    MOVE.W  #$6700,(A4)+        ; BNE   xxx
    MOVE.L  A4,-(A7)
    CLR.W   (A4)+               ; BNE   (*+2) - temp

    BSR     EXP                 ; <EXP>

    CMP.W   #19,(A3)            ; "... ? <a> : <b>" ?
    BNE     .1
    MOVE.W  #$6000,(A4)+        ; BRA   xxx
    MOVE.L  A4,A1
    CLR.W   (A4)+               ; BRA   (*+2) - temp
.1:
    MOVE.L  A4,D0
    MOVE.L  (A7),A0
    SUB.L   (A7)+,D0
    MOVE.W  D0,(A0)             ; make _real_ BNE

    CMP.W   #19,(A3)            ; <a> ? <b> [: <c>]
    BNE     .X
    ADDQ.L  #2,A3
    MOVE.L  A1,-(A7)
    BSR     EXP
    MOVE.L  A4,D0
    MOVE.L  (A7),A0
    SUB.L   (A7)+,D0
    MOVE.W  D0,(A0)
.X: BRA     EXPLOOP
;*-*
;; "{"
CURLY:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    CMP.W   #IDENT,(A3)+
    BNE ERROR4
    MOVE.L  (A3)+,A0
    BTST    #3,5(A0)
    BNE ERROR50         ; YOU CAN'T GET THE ADDRESS OF A REG :-)
    CMP.B   #LAB,4(A0)
    BNE.W   .3
    BTST    #4,5(A0)        ; THE LAB VERSION
    BNE ERROR4          ; (NO ADR OF METHOD)

    MOVE.W  10(A0),D0
    BMI.S   .MINM
    MOVE.L  .1(PC),(A4)+
    MOVE.W  .1L(PC),NEWOP
    BSR ADDBRANCH
    BRA.S   .D
.MINM:  MOVE.W  .1L(PC),(A4)+       ; ADR OF LABEL OF OTHER MODULE
    GETM    A6
    MOVE.L  A1,-(A7)
    MOVE.L  VARHEAVY(A0),A1     ; A1=PTR TO PROCCLASS
    MOVE.L  PC_ACC(A1),D0
    MOVE.L  A6,PC_ACC(A1)
    MOVE.L  D0,(A6)+
    MOVE.L  A4,(A6)+
    MOVE.L  (A7)+,A1
    DONEM   A6
    CLR.L   (A4)+
.D:
    CMP.W   #-1,D5
    BNE.S   .8
    MOVE.W  .4(PC),(A4)+
    BRA.S   .9
.8: MOVE.W  .2(PC),(A4)+
    BSR CLOSEEXP
.9: CMP.W   #24,(A3)+
    BNE ERROR29
    MOVEQ   #1,D5
    BRA EXPLOOP
.1: LEA .1(PC),A0
.1L:    LEA .1,A0
.2: MOVE.L  A0,D1
.4: MOVE.L  A0,D0
.5: LEA 2(A5),A0
.3: MOVE.W  .5(PC),(A4)+        ; THE VAR VERSION
    BSR GVA0D2_0
    CMP.W   #-1,D5
    BNE.S   .6
    MOVE.W  .4(PC),(A4)+
    BRA.S   .7
.6: MOVE.W  .2(PC),(A4)+
    BSR CLOSEEXP
.7: MOVEQ   #1,D5
    CMP.W   #24,(A3)+
    BNE ERROR29
    BRA EXPLOOP
;*-*
;; "'"
STRINGLINK:   DC.L    0

EXPSTRING:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    MOVEQ   #0,D0
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,D0        ; D0=STRINGLENGTH IN W
    move.l  a4,-(a7)
    LEA EXPSTRINGCODE(PC),A0
    CMP.W   #-1,D5
    BNE.S   .SK
    LEA EXPSTRINGCODE2(PC),A0
.SK:    MOVE.L  (A0)+,(A4)+
    MOVEM.L A0/D0,-(A7)
    BSR.W   NEWLABEL        ; ->D0
    MOVE.W  LONGLEA(PC),NEWOP
    BSR ADDBRANCH
    GETM    A6          ; A6=HEAP
    MOVE.L  STRINGLINK(PC),(A6)
    MOVE.L  A6,STRINGLINK
    ADDQ.L  #4,A6
    MOVE.W  D0,(A6)+
    MOVEM.L (A7)+,D0/A0
    MOVE.L  A3,(A6)+
    MOVE.L  A6,D7           ; D7=LASTPTR
    CLR.L   (A6)+
    LSL.W   #1,D0
    MOVE.W  D0,D2           ; D2=TOTALLEN STRINGS
    EXT.L   D2
    ADD.W   D0,A3
    DONEM   A6
    move.l  a4,d0
    sub.l   (A7)+,d0
    cmp.l   #6,d0
    beq     .1
    MOVE.W  (A0)+,(A4)+
    bra     PLUSTRINGS
.1: cmp.l   #EXPSTRINGCODE2,a0
    bmi     PLUSTRINGS
    or.w    #1,-4(a4)
PLUSTRINGS:
    CMP.L   #$70006,(A3)
    BNE.S   .X
    ADDQ.L  #6,A3
    MOVE.W  (A3)+,D0
    GETM    A6
    MOVE.L  D7,A0
    MOVE.L  A6,(A0)
    MOVE.L  A3,(A6)+
    MOVE.L  A6,D7
    CLR.L   (A6)+
    DONEM   A6
    LSL.W   #1,D0
    EXT.L   D0
    ADD.L   D0,D2
    ADD.W   D0,A3
    BRA.S   PLUSTRINGS
.X: CMP.W   #-1,D5
    BEQ.S   .1
    BSR CLOSEEXP
.1: MOVEQ   #1,D5
    CMP.L   #800,D2
    BPL ERROR46
    BRA EXPLOOP


EXPSTRINGCODE:
    LEA EXPSTRINGCODE2(PC),A0   ; 6-2 BYTES
    MOVE.L  A0,D1
EXPSTRINGCODE2:
    LEA EXPSTRINGCODE(PC),A0    ; 6-2 BYTES
    MOVE.L  A0,D0
LONGLEA:
    MOVE.L #EXPSTRINGCODE,D0

MAKESTRLEN:
    MOVE.L  #0,-36(A4)
MAKESTRLEN2:
    MOVEQ   #0,D0
    MOVE.L  D0,-36(A4)

D0EXT:    EXT.L   D0
;*-*
;; "("
EXPREC:
    CMP.W   #1,D5
    BEQ ERROR0
    BSR.W   SD0
    BSR.W   EXP
    BSR.W   RD0
    ADDQ.L  #2,A3
    MOVEQ   #1,D5
    BRA EXPLOOP
;*-*
;; "<>"
EXPCONSNIL:
    MOVEQ   #1,D5
    MOVEQ   #0,D1
    BRA VALUENTRY
;*-*
;; <x>
EXPCONS:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    BSR.W   SD0
    CLR.W   -(A7)
.XL:MOVE.W  #16,EAREQUEST
    BSR EAEXP
    TST.L   D0
    BNE.S   .OPT
    MOVE.W  .4(PC),(A4)+
.OPT:   ADDQ.W  #4,(A7)
    CMP.W   #COM,(A3)+
    BEQ.S   .XL
    CMP.W   #46,-(A3)
    BNE.S   .NC
    ADDQ.L  #2,A3
    MOVE.W  #16,EAREQUEST
    BSR EAEXP
    TST.L   D0
    BNE.S   .OPT2
    MOVE.W  .4(PC),(A4)+
.OPT2:  BRA.S   .D
.NC:    MOVE.W  .5(PC),(A4)+
.D: CMP.W   #12,(A3)+
    BNE ERROR0
    MOVE.W  .6(PC),(A4)+
    MOVE.W  (A7),D0
    SUBQ.W  #4,D0
    MOVE.W  D0,(A4)+
    MOVE.L  #120+10,D0      ; D0=CONS
    MOVE.B  #-1,EFUNCBYTE+120   ; _Cons=121
    MOVE.L  .4B(PC),(A4)+       ; GEN CALL
    TSTMOD
    BNE.S   .MOD
    MOVE.W  .5B(PC),NEWOP
    BSR ADDBRANCH
    BRA.S   .CC
.MOD:   SUBQ.L  #4,A4
    MOVE.W  .5B(PC),(A4)+
    BSR ADDBRANCHRELOC
    MOVE.W  D0,-2(A4)
.CC:    MOVE.W  .7(PC),(A4)+
    MOVE.W  (A7)+,D0
    ADDQ.W  #8,D0
    MOVE.W  D0,(A4)+
    BSR.W   RD0
    MOVEQ   #1,D5
    BRA EXPLOOP
.4: MOVE.L  D0,-(A7)
.5: CLR.L   -(A7)
.6: PEA 0.W
.5B:    JSR .4
.4B:    BSR.W   .4
.7: LEA 4(A7),A7
;*-*
;; "!"
EXPFLT:
    BTST    #1,CODEPREFS
    BNE     .FP
    BSET    #3,CODEPREFS+1      ; USE MATHIEEESINGBAS
    CMP.W   #0,D5
    BEQ ERROR0          ; ALWAYS AFTER (1) OR (-1)
    LEA .2(PC),A0
    TST.W   FLTFLAG
    BEQ.S   .1
    CLR.W   FLTFLAG         ; CLEAR FLOATMODUS
    CMP.W   #-1,D5
    BEQ ERROR0
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    MOVE.W  #-30,(A4)+
    BRA EXPLOOP
.1: MOVE.W  #-1,FLTFLAG     ; SET FLOATMODUS
    CMP.W   #-1,D5
    BEQ.S   .3
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    MOVE.W  #-36,(A4)+
.3: BRA EXPLOOP
.2: MOVE.L  -56(A4),A6
    JSR -30(A6)

.FP:
    CMP.W   #0,D5
    BEQ ERROR0          ; ALWAYS AFTER (1) OR (-1)
    TST.W   FLTFLAG
    BEQ.S   .F1
    CLR.W   FLTFLAG         ; CLEAR FLOATMODUS
    CMP.W   #-1,D5
    BEQ ERROR0
    MOVE.L  .F2(PC),(A4)+
;    MOVE.L  .FF(PC),(A4)+  ; uncomment for rounding
;    MOVE.L  .FF+4(PC),(A4)+
;    MOVE.L  .FF+8(PC),(A4)+
;    CMP.W   #1,ECPU
;    BGT     .FXZ
;    MOVE.L  .FF+12(PC),(A4)+
;    MOVE.W  .FF+18(PC),(A4)+
    BRA EXPLOOP
.FXZ:
    MOVe.L  .FF+16(PC),(A4)+
    BRA EXPLOOP
.F1:MOVE.W  #-1,FLTFLAG     ; SET FLOATMODUS
    CMP.W   #-1,D5

    BEQ.S   .F3
    MOVE.L  .F2+4(PC),(a4)+
.F3:BRA EXPLOOP
.F2:FMOVE.L FP0,D0
    FMOVE.L D0,FP0
.FF:FMOVE.L D0,FP1  ; 4
    FCMP    FP1,FP0 ; 4
    FSLT    D2      ; 4
    EXT.W   D2      ; 2
    EXT.L   D2      ; 2
    EXTB.L  D2      ; 2
    ADD.L   D2,D0   ; 2
;*-*
;; "`"
EXPQUOTE:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    BSR.W   SD0
    LEA .1(PC),A0
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    MOVE.L  A4,-(A7)
    BSR.W   EXP
    MOVE.W  .2(PC),(A4)+
    MOVE.L  (A7)+,A0
    MOVE.L  A4,D0
    SUB.L   A0,D0
    ADDQ.L  #2,D0
    MOVE.W  D0,-2(A0)
    BSR.W   RD0
    MOVEQ   #1,D5
    BRA EXPLOOP
.1: LEA .2(PC),A0
    MOVE.L  A0,D0
    BRA .1
.2: RTS
;*-*
;; SIZEOF
EXPSIZEOF:
    CLR.L   LAST_CMD_ADR
    MOVE.W  (A3),D0
    CMP.W   #IOFF+20,D0
    BPL.S   .1
    CMP.W   #IDENT,D0
    BEQ     .4
    CMP.W   #31,D0
    BNE ERROR40
    MOVE.W  #55,-2(A3)
    MOVE.W  #VALUE,(A3)+
    MOVE.L  (A3),A0
    MOVE.W  4(A0),2(A3)
    BEQ ERROR40
    CLR.W   (A3)
    BRA EXPVALUE
.1: CMP.W   #IOFF+23,D0
    BPL ERROR0
    ADDQ.L  #2,A3
    SUB.W   #IOFF+20,D0
    CMP.W   #1,D5
    BEQ ERROR0
    BSR.W   SD0
    MOVE.B  .3(PC),(A4)+
    MOVE.B  .2(PC,D0.W),(A4)+
    BSR.W   RD0
    MOVEQ   #1,D5
    BRA EXPLOOP
.4: ADDQ.L  #2,A3
    MOVE.L  (A3)+,A0
    MOVE.L  (A0),D0
    CMP.L   #4,D0
    BLE     .5
    MOVE.L  D0,A0
    MOVE.W  OSIZE(A0),D0
.5: CMP.W   #126,D0
    BGT     .6
    BSR SD0
    MOVE.B  .3(PC),(A4)+
    MOVE.B  D0,(A4)+
    BSR RD0
    MOVEQ   #1,D5
    BRA EXPLOOP
.6: BSR SD0
    MOVE.W  .7(PC),(A4)+
    EXT.L   D0
    MOVE.L  D0,(A4)+
    BSR RD0
    MOVEQ   #1,D5
    BRA EXPLOOP

.2: DC.B    4,2,1,0
.3: MOVEQ   #0,D0
.7: MOVE.L  #0,D0
;*-*
;; IF
EXPIF:
    MOVE.L  D0,D7           ; FOR DOJOB
    CMP.W   #1,D5
    BEQ     ERROR0
    BSR.W   SD0
    CLR.W   ELSECHECK
    MOVE.L  A5,-(A7)
    MOVEM.L D3-D5/A1/A2,-(A7)
    BSR.W   DOKEYWORD
    MOVEM.L (A7)+,D3-D5/A1/A2
    CMP.L   (A7)+,A5
    BNE     ERROR41
    TST.W   ELSECHECK
    BEQ     ERROR41
    BSR     RD0
    MOVEQ   #1,D5
    bset    #2,ICODEPREFS+3
    BRA     EXPLOOP

ELSECHECK:    DC.W    0
;*-*
;; ==
EXP_CHECK:
    CMp.W   #1,D5
    BNE ERROR0
    addq.l  #2,a3
    TST.W   FLTFLAG
    BNE     .FLOATS
    CMP.W   #29,(A3)+
    BNE ERROR0
    MOVe.L  A1,-(A7)
    ADDQ.L  #4,A3
    BSR NEWLABEL
    MOVE.L  D0,-(A7)
.LOOP:
    BSR SD0
    BSR EXP
    LEA EXPCMP(PC),A1
    BSR RD0
    CMp.W   #IOFF+39,(A3)
    BNe .OTHER
    TST.W   FLTFLAG
    MOVE.L  .CODE0(PC),(A4)+
    BSR NEWLABEL
    MOVE.L  D0,-(A7)
    CLR.W   NEWOP
    BSR ADDBRANCH
    ADDQ.L #2,A3
    BSR SD0
    BSR EXP
    LEA EXPCMP(PC),A1
    BSR RD0
    MOVE.L  .CODE1(PC),(A4)+
    MOVE.L  4(A7),D0
    BSR ADDBRANCH
    MOVE.L  (A7)+,D0
    BSR ADDLABEL
    BRA .NEXT
.OTHER:
    MOVE.L  .CODE2(PC),(A4)+
    MOVE.L  (A7),D0
    BSR ADDBRANCH
.NEXT:
    CMP.W   #COM,(A3)+
    BEQ .LOOP

    CMP.W   #30,-2(A3)
    BNE ERROR34
    MOVE.L  .CODE3(PC),(A4)+
    CLR.W   (A4)+
    BSR NEWLABEL
    MOVe.L  D0,-(A7)
    BSR ADDBRANCH
    MOVE.L  4(A7),D0
    BSR ADDLABEL
    MOVE.W  .CODE4(PC),(A4)+
    MOVE.L  (A7)+,D0
    BSR ADDLABEL
    ADDQ.L  #4,A7
    MOVEQ   #1,D5
    MOVE.L  (A7)+,A1
    BRA EXPLOOP

.FLOATS:
    BTST    #1,CODEPREFS
    BEQ     .IEEE

    CMP.W   #29,(A3)+
    BNE ERROR0
    MOVe.L  A1,-(A7)
    ADDQ.L  #4,A3
    BSR NEWLABEL
    MOVE.L  D0,-(A7)
.LOOP2:
    BSR SD0
    BSR EXP
    LEA EXPCMP(PC),A1

    CMP.W   #IOFF+39,(A3)
    BNE     .00
    MOVEQ   #2,D3
    BRA     .01
.00:MOVEQ   #0,D3
.01:BSR RD0
    CMp.W   #IOFF+39,(A3)
    BNe .OTHER2
    TST.W   FLTFLAG
    MOVE.L  .CODE5(PC),(A4)+
    BSR NEWLABEL
    MOVE.L  D0,-(A7)
    CLR.W   NEWOP
    BSR ADDBRANCH
    ADDQ.L #2,A3
    BSR SD0
    BSR EXP
    LEA EXPCMP(PC),A1
    MOVEQ   #4,D3
    BSR RD0
    MOVE.L  .CODE5(PC),(A4)+
    MOVE.L  4(A7),D0
    BSR ADDBRANCH
    MOVE.L  (A7)+,D0
    BSR ADDLABEL
    BRA .NEXT2
.OTHER2:
    MOVE.L  .CODE5(PC),(A4)+
    MOVE.L  (A7),D0
    BSR ADDBRANCH
.NEXT2:
    CMP.W   #COM,(A3)+
    BEQ .LOOP2

    CMP.W   #30,-2(A3)
    BNE ERROR34
    MOVE.L  .CODE3(PC),(A4)+
    CLR.W   (A4)+
    BSR NEWLABEL
    MOVe.L  D0,-(A7)
    BSR ADDBRANCH
    MOVE.L  4(A7),D0
    BSR ADDLABEL
    MOVE.W  .CODE4(PC),(A4)+
    MOVE.L  (A7)+,D0
    BSR ADDLABEL
    ADDQ.L  #4,A7
    MOVEQ   #1,D5
    MOVE.L  (A7)+,A1
    MOVE.L  .CODE6(PC),(A4)+
    BRA EXPLOOP

.IEEE:
    CMP.W   #29,(A3)+
    BNE ERROR0
    MOVe.L  A1,-(A7)
    ADDQ.L  #4,A3
    BSR NEWLABEL
    MOVE.L  D0,-(A7)
    MOVE.W  .CODE7(PC),(A4)+
.LOOP3:
    BSR EXP
    MOVE.L  .CODE8(PC),(A4)+
    CMP.W   #IOFF+39,(A3)
    BNE     .20
    MOVE.W  .CODEB(PC),D3
    BRA     .21
.20:MOVE.W  .CODEA(PC),D3
.21:LEA EXPCMP(PC),A1
    BSR CLOSEEXP
    CMp.W   #IOFF+39,(A3)
    BNe .OTHER3
    TST.W   FLTFLAG
    MOVE.L  .CODE5(PC),(A4)+
    BSR NEWLABEL
    MOVE.L  D0,-(A7)
    CLR.W   NEWOP
    BSR ADDBRANCH
    ADDQ.L #2,A3
    BSR EXP
    MOVE.L  .CODE8(PC),(A4)+
    LEA EXPCMP(PC),A1
    MOVE.W  .CODEC(PC),D3
    BSR CLOSEEXP
    MOVE.L  .CODE5(PC),(A4)+
    MOVE.L  4(A7),D0
    BSR ADDBRANCH
    MOVE.L  (A7)+,D0
    BSR ADDLABEL
    BRA .NEXT3
.OTHER3:
    MOVE.L  .CODE5(PC),(A4)+
    MOVE.L  (A7),D0
    BSR ADDBRANCH
.NEXT3:
    CMP.W   #COM,(A3)+
    BEQ .LOOP3

    CMP.W   #30,-2(A3)
    BNE ERROR34
    MOVE.L  .CODE3(PC),(A4)+
    CLR.W   (A4)+
    BSR NEWLABEL
    MOVe.L  D0,-(A7)
    BSR ADDBRANCH
    MOVE.L  4(A7),D0
    BSR ADDLABEL
    MOVE.W  .CODE4(PC),(A4)+
    MOVE.L  (A7)+,D0
    BSR ADDLABEL
    ADDQ.L  #4,A7
    MOVEQ   #1,D5
    MOVE.L  (A7)+,A1
    MOVE.W  .CODE9(PC),(A4)+
    BRA EXPLOOP



.CODE0:
    BMI     .CODE0  ;4
.CODE1:
    BLE     .CODE1  ;4
.CODE2:
    BEQ     .CODE2  ;4
.CODE3:
    MOVEQ   #0,D0   ;2
    BRA     .CODE3  ;4
.CODE4:
    MOVEQ   #-1,D0  ;2
.CODE5:
    BNE     .CODE5
.CODE6:
    FMOVE.L D0,FP0
.CODE7:
    MOVE.L  D0,-(A7)
.CODE8:
    MOVE.L  D0,D1
    MOVe.L  (A7),D0
.CODE9:
    ADDQ.L  #4,A7
.CODEA:
    SEQ D0
.CODEB:
    SLT D0
.CODEC:
    SLE D0
;*-*

;; Inter-expression codes

EXPRCODE1:
    MOVE.L  D0,-(A7)
FEXPRCODE1:
    FMOVE.S FP0,-(A7)   ; 4
    FMOVE   FP0,FP0     ; 4
EXPRCODE2:
    MOVE.L  D0,D1
    MOVE.L  (A7)+,D0
FEXPRCODE2:
    FMOVE.S (A7)+,FP0   ; 4
    FSMOVE.S (A7)+,FP0  ; 4
FEXPRCODE_USED_REGS:
    DC.W    0           ; shouldn't fail ;)
EXTL:
    EXT.W   D0
    EXT.L   D0
EXTBL:
    EXTB.L  D0
MOVEQ:
    MOVEQ   #0,D0
FEXPSTART:
    FMOVE.L #0,FP0
FEXPS:
    FMOVEM.X FP2-FP7,-(A7)
FEXPR:
    FMOVEM.X (A7)+,FP2-FP7
;*-*
;; Extend D0
EXP_EXTD0:
    CMP.W   #1,ECPU     ;020+ mode?
    BPL     .020
    MOVE.L  EXTL(PC),(A4)+
    RTS
.020:
    MOVE.W  EXTBL(PC),(A4)+
    RTS
;*-*

;; ExpSwap
EXPSWAP:
    MOVE.L  LASTVAR,-(A7)
    TST.L   LASTVAR
    BEQ     ERROR0
    BSR SD0
    BSR EXP
    MOVEQ   #1,D4
    BSR RD0
    TST.L   LASTVAR
    BEQ     ERROR0
    MOVe.L  LASTVAR,A0
    MOVE.W  .1(PC),(A4)+
    BSR GVA0D0_0
    MOVe.L  (A7)+,a0
    MOVE.W  .2(PC),(A4)+
    BSR GVA0D0_0
    BRA EXPLOOP

.1: MOVE.L  D0,2(A5)
.2: MOVE.L  D1,2(A5)
;*-*

;; CloseExp
CLOSEEXP:
    TST.W   FLTFLAG
    BNE     CLOSEFLT
CLFLTB:
    CMP.B   #1,D4
    BEQ     .2
    CMP.W   #2,D4
    BEQ.S   .1
    CMP.W   #3,D4
    BEQ     .S
    CMP.W   #4,D4
    BNE     .3
    CMP.W   #1,ECPU
    BPL     .020
.3:
    tst.l   LAST_CMD_ADR
    bne     .TRY_TO_OPTI
.damn:
    cmp.l   #$2200201f,-4(a4)
    bne     .D2
    cmp.l   #EXPPLUS,A1
    beq     .OADD
    cmp.l   #EXPMUL,A1
    beq     .OMUL
.D2:
    MOVE.W  4(A1,D4.W),(A4)+    ; OP
    CMP.L   #EXPDIV,A1
    BNE.S   .2
    MOVE.W  D0EXT(PC),(A4)+
.2: RTS
.1: cmp.l   #$2200201f,-4(a4)
    beq     .1O
    MOVE.W  10(A1),(A4)+        ; CMP
    MOVE.W  D3,(A4)+
    BSR EXP_EXTD0
    RTS
.1O:subq.l  #4,a4
    move.l  #$221fb280,(a4)+
    move.w  d3,(a4)+
    bsr EXP_EXTD0
    rts
.S: MOVE.W  (A1),(A4)+
    RTS
.OADD:
    subq.l  #4,a4
    move.w  #$d09f,(a4)+
    rts
.OMUL:
    subq.l  #4,a4
    cmp.w   #1,ECPU
    bpl     .OMUL2
    move.l  #$548fc1df,(a4)+
    rts
.OMUL2:
    move.l  #$4c1f0800,(a4)+
    rts
.020:
    tst.l   LAST_CMD_ADR
    bne     .TRY_TO_OPTI
    CMP.L   #EXPDIV,A1
    BNE     .020M
    MOVE.L  #$4c410800,(a4)+
    rts
.020M:
    MOVE.L  #$4c010800,(a4)+
    rts

.TRY_TO_OPTI:
    move.l  LAST_CMD_ADR,a0
    clr.l   LAST_CMD_ADR
    move.w  (a0),d0
    and.w   #$FFFE,D0
    cmp.w   #$222c,d0
    beq     .OPTI_01
    and.w   #$FFF8,d0
    cmp.w   #$2200,d0
    beq     .OPTI_02
    move.l  a0,LAST_CMD_ADR
    bra     .damn
;; optimize #1
.OPTI_01:
    move.l  a4,d0
    sub.l   a0,d0
    cmp.l   #4,d0
    bgt     .damn
    move.l  a1,d0
    sub.l   #EXPAND,d0
    divs.w  #3,d0
    move.w  .Cx01(PC,d0),d0
    swap    d0
    move.w  (a0),d0
    and.w   #$7,d0
    move.w  d0,-(a7)
    swap    d0
    or.w    (a7)+,d0
    move.w  d0,(a0)
    cmp.l   #EXPMUL,a1
    bpl     .KEEP_01
.X_01:
    rts
.KEEP_01:
    cmp.w   #1,ECPU
    bpl     .KEEP_11
    add.w   #2,2(a0)
    cmp.l   #EXPMUL,a1
    beq     .X_01
    move.w  D0EXT(PC),(A4)+
    bra     .X_01
.KEEP_11:
    cmp.l   #EXPMUL,a1
    sgt     d0
    and.l   #6,d0
    move.l  .Cx11(PC,D0),D0
    move.l  d0,-(a7)
    move.w  (a0),d0
    and.l   #$7,d0
    swap    d0
    or.l    (a7)+,d0
    move.w  2(a0),-(a7)
    move.l  d0,(a0)
    move.w  (a7)+,(a4)+
    rts
.Cx01:
    and.l   1(a0),d0
    or.l    1(a0),d0
    cmp.l   1(a0),d0
    add.l   1(a0),d0
    sub.l   1(a0),d0
    muls.w  1(a0),d0
    divs.w  1(a0),d0
    move.l  1(a0),d0
.Cx11:
    muls.l  1(a0),d0
    divs.l  1(a0),d0
;*-*
;; optimize #2
.OPTI_02:
    move.l  a4,d0
    sub.l   a0,d0
    cmp.l   #2,d0
    bgt     .damn
    move.l  a1,d0
    sub.l   #EXPAND,d0
    divu.w  #6,d0
    move.w  .Cx02(pc,d0),d0
    swap    d0
    move.w  (a0),d0
    and.w   #7,d0
    move.w  d0,-(a7)
    swap    d0
    or.w    (a7)+,d0
    move.w  d0,(a0)
    cmp.l   #EXPMUL,A1
    bpl     .KEEP_02
    rts
.KEEP_02:
    cmp.w   #1,ECPU
    bpl     .KEEP_12
    cmp.l   #EXPMUL,A1
    beq     .X_01
    move.w  D0EXT(PC),(A4)+
    rts
.KEEP_12:
    swap    d0
    move.w  d0,-(a7)
    cmp.l   #EXPMUL,A1
    sgt     d0
    and.l   #4,d0
    move.l  .Cx12(PC,d0),d0
    swap    d0
    or.w    (a7)+,d0
    swap    d0
    move.l  d0,(a0)
    addq.l  #2,a4
    rts


.Cx02:
    and.l   d0,d0
    or.l    d0,d0
    cmp.l   d0,d0
    add.l   d0,d0
    sub.l   d0,d0
    muls.w  d0,d0
    divs.w  d0,d0
    move.l  d0,d0
.Cx12:
    muls.l  d0,d0
    divs.l  d0,d0
;*-*
;*-*
;; CloseFlt
CLOSEFLT:
    CMP.W   #-1,D5          ; NO MOVE
    BEQ     CLFLTB
    CMP.L   #EXPCMP,A1      ; NO AND/OR
    BMI     CLFLTB

    btst    #1,CODEPREFS
    bne     .DONEFP
    MOVE.L  D0,-(A7)
    BSR MAKEFLTOPER
    TST.W   D0
    BNE.S   .2          ; SEE IF SPCMP
    btst    #1,CODEPREFS
    bne .4
    MOVE.W  D3,(A4)+
    BSR     EXP_EXTD0
.2: MOVE.L  (A7)+,D0
    RTS
.3: move.w  #1,FINISHFPEXP
    bsr MAKEFLTOPER
    TST.W   D0
    BNE.S   .2          ; SEE IF SPCMP
.4:
    bsr EXPFPCMP
    bra .2

.DONEFP:
    moveq   #0,d1
    cmp.l   #EXPPLUS,a1
    beq     .FADD
    cmp.l   #EXPMINUS,a1
    beq     .FSUB
    cmp.l   #EXPMUL,a1
    beq     .FMUL
    cmp.l   #EXPDIV,a1
    beq     .FDIV
    cmp.l   #EXPCMP,a1
    beq     .FCMP
    bra     ERROR0
.FADD:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #4,d0
    lea     .FC,a0
    add.l   d0,a0
    bra     .FFIX
.FSUB:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #4,d0
    lea     .FC+8,a0
    add.l   d0,a0
    bra     .FFIX
.FMUL:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #4,d0
    lea     .FC+16,a0
    add.l   d0,a0
    bra     .FFIX
.FDIV:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #4,d0
    lea     .FC+24,a0
    add.l   d0,a0
    bra     .FFIX
.FCMP:
    lea     .FC+32,a0
    moveq   #-1,d1
.FFIX:
    move.l  a4,d0
    sub.l   LAST_CMD_ADR,d0
    cmp.l   #8,d0
    beq     .OPTI
.OPTIBACK
    move.l  (a0),(a4)+
    tst.l   d1
    beq     .FXIT
    bsr     EXPFPCMP
.FXIT:
    rts
.OPTI:
    move.l  .FO,d0
    cmp.l   -8(a4),d0
    bne     .OPTIBACK
    cmp.w   #2,ECPU
    spl     d0
    and.l   #4,d0
    cmp.l   #EXPMUL,a1
    beq     .OPTIMUL
    cmp.l   #EXPPLUS,a1
    bne     .OPTIBACK

    subq.l  #8,a4
    lea     .FR,a0
    add.l   d0,a0
    move.l  (a0),(a4)+
    rts
.OPTIMUL:
    subq.l  #8,a4
    lea     .FR+8,a0
    add.l   d0,a0
    move.l  (a0),(a4)+
    rts

.FO:FMOVE.S FP0,D0
.FR:FADD.S  (A7)+,FP0
    FSADD.S (A7)+,FP0
    FMUL.S  (A7)+,FP0
    FSGLMUL.S (A7)+,FP0

.FC:FADD.S  D0,FP0
    FSADD.S D0,FP0
    FSUB.S  D0,FP0
    FSSUB.S D0,FP0
    FMUL.S  D0,FP0
    FSGLMUL.S D0,FP0
    FDIV.S  D0,FP0
    FSGLDIV.S D0,FP0
    FCMP.S  D0,FP0

;*-*
;; Restore
RD0:
    CMP.W   #-1,D5
    BEQ.S   .1
    BTST    #1,CODEPREFS
    BEQ     .0
    TST.W   FLTFLAG
    BNE     .2
.0: MOVE.L  EXPRCODE2(PC),(A4)+
    BSR CLOSEEXP
.1: RTS
.2: cmp.w   #2,ECPU
    bpl     .2O
    MOVE.L  FEXPRCODE2(PC),(A4)+
    bra     .2X
.2O:MOVe.L  FEXPRCODE2+4(PC),(A4)+
.2X:BSR CLOSEEXP
    RTS
;*-*
;; Store
SD0:
    CMP.W   #-1,D5
    BEQ.S   .1
    tst.l   LAST_CMD_ADR
    bne     .OPTI
.OB:BCLR    #2,ICODEPREFS+3
    BTST    #1,CODEPREFS
    BEQ .0
    TST.W   FLTFLAG
    BNE .2
.0: MOVE.W  EXPRCODE1(PC),(A4)+
.1: RTS
.2: move.l  FEXPRCODE1(PC),(A4)+
    rts
.OPTI:
    btst    #2,ICODEPREFS+3
    bne     .OB
    move.l  a4,d0
    sub.l   LAST_CMD_ADR,d0
    cmp.w   #2,d0
    beq     .O2
    cmp.w   #4,d0
    beq     .O4
    cmp.w   #6,d0
    beq     .O6
    bra     .OB
.O4:move.l  LAST_CMD_ADR,a0
    move.w  (a0),d0
    and.l   #$FFFE,d0
    cmp.w   #$202c,d0
    bne     .OB
    move.w  (a0),d0
    and.l   #1,d0
    or.w    #$2f2c,d0
    move.w  d0,(a0)
    rts
.O6:move.l  LAST_CMD_ADR,a0
    cmp.w   #$203c,(a0)
    bne     .OB
    move.l  2(a0),d0
    cmp.l   #32767,d0
    bgt     .O60
    cmp.l   #-32768,d0
    bpl     .O61
.O60:
    move.w  #$4879,(a0)
    rts
.O61:
    move.w  #$4878,(a0)
    move.w  d0,2(a0)
    subq.l  #2,a4
    rts
.O2:move.l  LAST_CMD_ADR,a0
    move.w  (a0),d0
    and.l   #$FF00,d0
    cmp.w   #$7000,d0
    bne     .OB
    move.w  (a0),d0
    and.l   #$FF,d0
    move.w  #$4878,(a0)
    ext.w   d0
    move.w  d0,(a4)+
    rts
;*-*
poprawka=1
  IFeq poprawka
;; new imm list
EXPIMMLIST:
    CLR.L   LAST_CMD_ADR
    cmp.w   #-1,d5
    bne     ERROR0
    addq.l  #4,a3       ; skip ] off
    move.l  a5,-(a7)
    moveq   #0,d5
.L1:CHESTB  A5,D1,20,ERROR46
    addq.l  #1,d5
    move.l  a3,a0
    cmp.w   #LIBC,(A3)
    beq     .L2
    cmp.w   #STR,(a3)
    beq     .L4
    movem.l d5/a0,-(a7)
    jsr     ASM_GRABVALUE
    movem.l (A7)+,d5/a0
    tst.l   d0
    bmi     .L2
    move.w  #1,(a5)+    ; value
    move.l  d1,(a5)+
    bra     .L3
.L4:move.w  #3,(a5)+
    addq.l  #4,a3
    moveq   #0,d0
    move.w  (a3)+,d0
    lsl.w   #1,d0
    move.l  d0,-(a7)
    addq.l  #2,d0
    jsr     NEW
    tst.l   d0
    beq     ERROR38
    move.l  d0,a0
    move.l  a0,(a5)+
    move.l  (a7)+,d0
    move.w  d0,(a0)+
.L4c:
    tst.w   d0
    beq     .L3
    move.b  (a3)+,(a0)+
    subq.l  #1,d0
    bra     .L4c
.L2:move.l  a0,a3
    move.w  #16,EAREQUEST
    move.l  d5,-(a7)
    bsr     EAEXP
    tst.l   d0
    bne     .OPTI
    move.w  .K4,(a4)+
.OPTI:
    move.l  (a7)+,d5
    move.w  #2,(a5)+    ; expression
.L3:cmp.w   #COM,(a3)+
    beq     .L1
    move.w  #-1,(a5)+   ; done ;)
    cmp.w   #30,-2(a3)  ; d5 now: numentries
    bne     ERROR34     ; set backwards!
    moveq   #4,D2       ; entry size
    sub.l   a2,a2       ; object
    cmp.w   #19,(a3)    ; typed?
    bne     .CNT
    addq.l  #2,a3
    move.w  (a3)+,d0
    cmp.w   #31,d0
    bne     .C1

    move.l  (a3),a2    ; count members and calculate size
    lea     OMEMB+4(a2),a2
    moveq   #0,d0
.C2:addq.l  #1,d0
    tst.l   ONEXT(a2)
    beq     .C3
    move.l  ONEXT(a2),a2
    bra     .C2
.C3:move.l  (a3)+,a2
    move.l  d5,d1
    lsl.w   #2,d1
    bra     .CNT
.C1:cmp.w   #IOFF+20,d0
    bmi     ERROR40
    cmp.w   #IOFF+22,d0
    bgt     ERROR40
    cmp.w   #IOFF+21,d0
    bmi     .CNT
    bgt     .CHR
    moveq   #2,d2
    bra     .CNT
.CHR:
    moveq   #1,d2
.CNT:
    cmpa.l  #0,a2
    bne     .CNT2
    move.l  d5,d1
    mulu    d2,d1
.CNT2:
    move.l  d1,-(a7)
    GETM    A0          ; 0=next
    MOVE.L  IMMLH,(A0)  ; 4=id
    MOVE.L  A0,IMMLH    ; 6=list
    addq.l  #4,a0       ; 10=size
    move.l  a0,-(a7)
    jsr     NEWLABEL
    move.l  (a7)+,a0
    move.w  d0,(a0)+
    move.l  (a7)+,d1
    addq.l  #2,d1
    move.l  d1,d0
    JSR     NEW
    move.l  d0,(a0)+
    move.l  d1,(a0)+    ; :)
    clr.l   (a0)+
    DONEM   A0

    move.l  IMMLH,A0
    MOVE.W  4(a0),d0
    move.w  .K1,NEWOP
    MOVE.l  .K1S,(A4)+
    move.l  a0,-(a7)
    jsr     ADDBRANCH
    move.l  (a7)+,a0
    move.l  6(a0),a0
    move.w  d5,(a0)

    move.l  (a7),a1
    move.w  #-1,-(a7)
    moveq   #0,d5
    cmp.l   #0,a2
    beq     .F1
    move.l  OMEMB(a2),a6
.FL:add.w   OOFF(a6),d5
    move.w  OSIZE(a6),d2
    beq     ERROR40
    cmp.w   #4,d2
    bgt     ERROR40
.F1:moveq   #0,d0           ;
    move.w  (a1)+,d0
    cmp.w   #1,d0
    bne     .F2
    move.l  (a1)+,d0
    cmp.w   #2,d2
    blt     .Fb
    beq     .Fi
    move.l  d0,2(a0,d5)
    bra     .F
.Fb:move.b  d0,2(a0,d5)
    bra     .F
.Fi:move.w  d0,2(a0,d5)
    bra     .F
.F2:cmp.w   #2,d0
    bne     .F3

    move.w  d5,-(a7)
    move.w  d2,-(a7)
    move.l  #-1,.T0
    bra     .F

.F3:cmp.w   #3,d0
    bne     .F4
    move.l  a0,-(a7)
    GETM    A0
    move.l  (a1)+,4(a0)
    MOVE.L  a0,-(a7)
    bsr     NEWLABEL
    move.l  (a7)+,a0
    move.w  d0,8(a0)
    move.l  a1,-(a7)
    move.l  IMMLH,a1
    move.l  14(a1),(a0)
    move.l  a0,14(a1)
    move.l  (a7)+,a1
    move.w  d5,10(a0)
    add.l   #12,a0
    DONEM   A0
    move.l  (a7)+,a0
    bra     .F

.F: cmp.l   #0,a2
    beq     .F1L
    sub.w   OOFF(a6),d5
    move.l  ONEXT(a6),d0
    bne     .FN
    move.l  OMEMB(a2),d0
.FN:move.l  d0,a6
    bra     .FL
.F1L:
    add.l   d2,d5
    bra     .F1
.F4:cmp.w   #-1,d0
    bne     ERROR0

.VL:
    move.w  (a7)+,d0
    cmp.w   #-1,d0
    beq     .VX
    cmp.w   #2,d0
    bmi     .VC
    beq     .VI
    move.w  .K0,(a4)+
    move.w  (a7)+,(a4)+
    bra     .VL
.VI:move.l  .K2,(a4)+
    add.w   #$2000,-2(a4)
    move.w  (a7)+,(a4)+
    bra     .VL
.VC:move.l  .K2,(A4)+
    move.w  (A7)+,(A4)+
    bra     .VL
.VX:
    move.l  (a7)+,a5
    moveq   #1,d5
    tst.l   .T0
    bne     .X2
    cmp.w   #$41FA,-4(a4)
    beq     .X2
    move.w  #$203c,-6(a4)
    bra     EXPLOOP
.X2:mOVE.L  A4,LAST_CMD_ADR
    move.w  .K3,(a4)+
    bra     EXPLOOP

.K0:MOVE.L  (A7)+,4(A0)
.K1:LEA     $0,A0
.K1S:
    LEA     .K1S(PC),A0
.K2:MOVE.L  (A7)+,D0
    MOVE.B  D0,4(A0)
.K3:MOVE.L  A0,D0
.K4:MOVE.L  D0,-(A7)
.T0:
    DC.L    0
IMMLH:
    DC.L    0
; list entry struct:
; (  0) next
; (  4) listid [used with addreloc etc]
; (  6) list array
; ( 10) list size
; ( 14) list references
STRLH:
    DC.L    0
; string entry struct:
; (  0) next
; (  4) interim ptr
; (  8) strid [used with addreloc etc]
;*-*
  endc
  ifne poprawka
;; new imm list
EXPIMMLIST:
    CLR.L   LAST_CMD_ADR
    cmp.w   #-1,d5
    bne     ERROR0
    addq.l  #4,a3       ; skip ] off
    move.l  a5,-(a7)
    moveq   #0,d5
.L1:CHESTB  A5,D1,20,ERROR46
    addq.l  #1,d5
    move.l  a3,a0
    cmp.w   #LIBC,(A3)
    beq     .L2
    cmp.w   #STR,(a3)
    beq     .L4
    movem.l d5/a0,-(a7)
    jsr     ASM_GRABVALUE
    movem.l (A7)+,d5/a0
    tst.l   d0
    bmi     .L2
    move.w  #1,(a5)+    ; value
    move.l  d1,(a5)+
    bra     .L3
.L4:move.w  #3,(a5)+
    addq.l  #4,a3
    moveq   #0,d0
    move.w  (a3)+,d0
    lsl.w   #1,d0
    move.l  d0,-(a7)
    addq.l  #2,d0
    jsr     NEW
    tst.l   d0
    beq     ERROR38
    move.l  d0,a0
    move.l  a0,(a5)+
    move.l  (a7)+,d0
    move.w  d0,(a0)+
.L4c:
    tst.w   d0
    beq     .L3
    move.b  (a3)+,(a0)+
    subq.l  #1,d0
    bra     .L4c
.L2:move.l  a0,a3
    move.w  #16,EAREQUEST
    move.l  d5,-(a7)
    bsr     EAEXP
    tst.l   d0
    bne     .OPTI
    move.w  .K4,(a4)+
.OPTI:
    move.l  (a7)+,d5
    move.w  #2,(a5)+    ; expression
.L3:cmp.w   #COM,(a3)+
    beq     .L1
    move.w  #-1,(a5)+   ; done ;)
    cmp.w   #30,-2(a3)  ; d5 now: numentries
    bne     ERROR34     ; set backwards!
    moveq   #4,D2       ; entry size
    sub.l   a2,a2       ; object
    cmp.w   #19,(a3)    ; typed?
    bne     .CNT
    addq.l  #2,a3
    move.w  (a3)+,d0
    cmp.w   #31,d0
    bne     .C1

    move.l  (a3),a2    ; count members and calculate size
    lea     OMEMB+4(a2),a2
    moveq   #0,d0
.C2:addq.l  #1,d0
    tst.l   ONEXT(a2)
    beq     .C3
    move.l  ONEXT(a2),a2
    bra     .C2
.C3:move.l  (a3)+,a2
    move.l  d5,d1
    lsl.w   #2,d1
    bra     .CNT
.C1:cmp.w   #IOFF+20,d0
    bmi     ERROR40
    cmp.w   #IOFF+22,d0
    bgt     ERROR40
    cmp.w   #IOFF+21,d0
    bmi     .CNT
    bgt     .CHR
    moveq   #2,d2
    bra     .CNT
.CHR:
    moveq   #1,d2
.CNT:
    cmpa.l  #0,a2
    bne     .CNT2
    move.l  d5,d1
    mulu    d2,d1
.CNT2:
    move.l  d1,-(a7)
    GETM    A0          ; 0=next
    MOVE.L  IMMLH,(A0)  ; 4=id
    MOVE.L  A0,IMMLH    ; 6=list
    addq.l  #4,a0       ; 10=size
    move.l  a0,-(a7)
    jsr     NEWLABEL
    move.l  (a7)+,a0
    move.w  d0,(a0)+
    move.l  (a7)+,d1
    addq.l  #2,d1
    move.l  d1,d0
    JSR     NEW
    move.l  d0,(a0)+
    move.l  d1,(a0)+    ; :)
    clr.l   (a0)+
    DONEM   A0

    move.l  IMMLH,A0
    MOVE.W  4(a0),d0
    move.w  .K1,NEWOP
    MOVE.l  .K1S,(A4)+
    move.l  a0,-(a7)
    jsr     ADDBRANCH
    move.l  (a7)+,a0
    move.l  6(a0),a0
    move.w  d5,(a0)

    move.l  (a7),a1
    move.w  #-1,-(a7)
    lea     OMEMB+4(a2),a6
    moveq   #0,d5
    moveq   #0,d1       ;x;
.F: cmpa.l  #0,a2
    beq     .F1
    move.l  ONEXT(a6),d0
    bne     .F0
    move.l  OMEMB(A2),d0
    moveq   #0,d1       ;x;
.F0:move.l  d0,a6
    moveq   #0,d2
    sub.w   OOFF(a6),d1 ;x;
    neg.w   d1          ;x;
    add.w   d1,d5       ;x;
    move.w  OOFF(a6),d1 ;x;
    move.w  OSIZE(a6),d2
    add.w   d2,d1       ;x;
    beq     ERROR40
    cmp.l   #4,d2
    bgt     ERROR40
.F1:moveq   #0,d0           ;
    move.w  (a1)+,d0
    cmp.w   #1,d0
    bne     .F2
    move.l  (a1)+,d0
    cmp.w   #2,d2
    blt     .Fb
    beq     .Fi
    move.l  d0,2(a0,d5)
    bra     .Fc
.Fb:move.b  d0,2(a0,d5)
    bra     .Fc
.Fi:move.w  d0,2(a0,d5)
.Fc:add.l   d2,d5
    bra     .F
.F2:cmp.w   #2,d0
    bne     .F3

    move.w  d5,-(a7)
    move.w  d2,-(a7)
    add.l   d2,d5
    move.l  #-1,.T0
    bra     .F

.F3:cmp.w   #3,d0
    bne     .F4
    move.l  a0,-(a7)
    GETM    A0
    move.l  (a1)+,4(a0)
    MOVE.L  a0,-(a7)
    bsr     NEWLABEL
    move.l  (a7)+,a0
    move.w  d0,8(a0)
    move.l  a1,-(a7)
    move.l  IMMLH,a1
    move.l  14(a1),(a0)
    move.l  a0,14(a1)
    move.l  (a7)+,a1
    move.w  d5,10(a0)
    add.l   #12,a0
    DONEM   A0
    move.l  (a7)+,a0
    add.l   d2,d5
    bra     .F

.F4:cmp.w   #-1,d0
    bne     ERROR0

.VL:
    move.w  (a7)+,d0
    cmp.w   #-1,d0
    beq     .VX
    cmp.w   #2,d0
    bmi     .VC
    beq     .VI
    move.w  .K0,(a4)+
    move.w  (a7)+,(a4)+
    bra     .VL
.VI:move.l  .K2,(a4)+
    add.w   #$2000,-2(a4)
    move.w  (a7)+,(a4)+
    bra     .VL
.VC:move.l  .K2,(A4)+
    move.w  (A7)+,(A4)+
    bra     .VL
.VX:
    move.l  (a7)+,a5
    moveq   #1,d5
    tst.l   .T0
    bne     .X2
    cmp.w   #$41FA,-4(a4)
    beq     .X2
    move.w  #$203c,-6(a4)
    bra     EXPLOOP
.X2:mOVE.L  A4,LAST_CMD_ADR
    move.w  .K3,(a4)+
    bra     EXPLOOP

.K0:MOVE.L  (A7)+,4(A0)
.K1:LEA     $0,A0
.K1S:
    LEA     .K1S(PC),A0
.K2:MOVE.L  (A7)+,D0
    MOVE.B  D0,4(A0)
.K3:MOVE.L  A0,D0
.K4:MOVE.L  D0,-(A7)
.T0:
    DC.L    0
IMMLH:
    DC.L    0
; list entry struct:
; (  0) next
; (  4) listid [used with addreloc etc]
; (  6) list array
; ( 10) list size
; ( 14) list references
STRLH:
    DC.L    0
; string entry struct:
; (  0) next
; (  4) interim ptr
; (  8) strid [used with addreloc etc]
;*-*
  endc

;; << >>
EXPSHIFT:
    CMP.W   #1,D5
    BNE     ERROR0
    MOVEQ   #3,D4
    LEA     .S(PC),A1
    CMP.W   #49,D0
    BEQ.S   .LEFT
    ADDQ.L  #4,A1
.LEFT
    MOVEQ   #0,D5
    BRA     EXPLOOP
.S: ASL.L   D1,D0
    ASL.L   #8,D0
    ASR.L   D1,D0
    ASR.L   #8,D0
;*-*
;; +-*/
EXPOPERATOR:
    CMP.W   #11,D0
    BPL EXPBACK
    CMP.W   #1,D5           ; 0 OPER,COMP / 1 VAL,VAR
    BNE.S   NEGVALUE
NEGBACK:
    MOVEQ   #6,D4
    CMP.W   #9,D0
    BMI.S   EXPNODP
    MOVEQ   #4,D4
EXPNODP:
    CMP.W   #-1,D5
    BNE.S   EXPNOTFIRST
    CMP.W   #8,D0
    BNE ERROR0
    TST.W   FLTFLAG
    BEQ     .0
    BTST    #1,CODEPREFS
    BEQ     .0
    MOVE.L  FEXPSTART,(A4)+
    MOVe.L  FEXPSTART+4,(A4)+
    BRA EXPNOTFIRST
.0:
    MOVE.W  EXPSTART,(A4)+
EXPNOTFIRST:
    MOVEQ   #0,D5
    SUB.W   #7,D0
    MULU    #12,D0
    EXT.L   D0
    ADD.L   #EXPPLUS,D0
    MOVE.L  D0,A1
    BRA EXPLOOP
NEGBACK1:
    SUBQ.L  #2,A3
    CMP.W   #-1,D5
    BEQ.S   NEGBACK
    BRA ERROR0

NEGVALUE:
    CMP.W   #8,D0
    BNE ERROR0
    CMP.W   #43,(A3)
    BEQ EXPNEGFLOAT
    CMP.W   #VALUE,(A3)+
    BNE NEGBACK1
    NEG.L   (A3)
;*-*

;; Value
EXPVALUE:             ; ENTRY
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    TST.W   FLTFLAG
    BNE.W   VALUEFLOAT
VALFLTBACK:

    TST.W   D5          ; SEE IF WE CAN OPTI CONSTS!
    BEQ.S   .1
    SUBQ.L  #2,A3
    MOVEM.L D3/D4,-(A7)
    JSR ASM_GRABVALUE
    MOVEM.L (A7)+,D3/D4
    MOVEQ   #1,D5
    BRA.S   .2

.1: MOVEQ   #1,D5
    MOVE.L  (A3)+,D1
.2:
VALUENTRY:            ; VAL IN D1, #1 IN D5
    CMP.L   #4,D4
    BEQ.S   SHORTBACK
    CMP.L   #3,D4
    BEQ     PASS_SHIFT1
    move.l  a4,LAST_CMD_ADR
    CMP.L   #128,D1
    BMI.W   EXPSHORT
SHORTBACK:
    MOVE.W  (A1),(A4)+
    CMP.L   #4,D4
    BEQ.S   EXPDIVPROD
    MOVE.L  D1,(A4)+
    CMP.W   #2,D4
    BNE EXPLOOP
    tst.l   d1
    bne     .1
    subq.l  #6,a4
    move.w  #$4a80,(a4)+
.1:
    MOVE.W  D3,(A4)+
    BSR EXP_EXTD0
    BRA EXPLOOP
PASS_SHIFT1:
    TST.W   D1
    BEQ     .SOPTI
    CMP.W   #8,D1
    BPL     .LSHFT
    MOVE.W  2(A1),(A4)
    LSL.W   #8,D1
    LSL.W   #1,D1
    OR.W    D1,(A4)+
    BRA     .SOPTI
.LSHFT:
    cmp.l   #-128,d1
    bmi     .KP
    cmp.l   #127,d1
    bgt     .KP
    move.w  .MOVQ(PC),(A4)
    and.w   #$FF,d1
    or.w    d1,(a4)+
    move.w  (a1),(a4)+
    bra     .SOPTI
.KP:
    MOVE.W  .MOV(PC),(A4)+
    MOVE.L  D1,(A4)+
    MOVE.W  (A1),(A4)+
.SOPTI:
    BRA EXPLOOP
.MOV:
    MOVE.L  #$0,D1
.MOVQ:
   moveq    #0,d1

EXPDIVPROD:
    cmpa.l  #EXPMUL,a1
    bne     .SK1
    btst    #1,ICODEPREFS+3
    beq     .SK1
    cmp.l   #30,d1
    bgt     .SK1
    cmp.l   #2,d1
    bpl     .FAST_OPTI
.SK1:
    CMP.L   #2,D1
    BEQ.S   .OPTI
    CMP.L   #4,D1
    BEQ.S   .OPTI

    CMP.W   #1,ECPU
    BPL     .VALDIVMUL

    MOVE.W  D1,(A4)+
    TST.W   D1
    BEQ ERROR15
    SWAP    D1
    TST.W   D1
    BNE.S   .2
.3: CMPA.L  #EXPMUL,A1
    BEQ.S   .1
    MOVE.W  D0EXT(PC),(A4)+
.1: BRA EXPLOOP
.2: CMP.W   #-1,D1
    BNE ERROR11
    BRA.S   .3
.OPTI:  SUBQ.L  #2,A4
    CMPA.L  #EXPMUL,A1
    BNE.S   .OPTID
    CMP.W   #2,D1
    BNE.S   .OM4
    MOVE.W  .ADD(PC),(A4)+
    BRA EXPLOOP
.OM4:   MOVE.W  .ASL(PC),(A4)+
    BRA EXPLOOP
.OPTID: CMP.W   #2,D1
    BEQ .OD2
    MOVE.L  .AS2(PC),(A4)+
    MOVE.L  .AS2+4(PC),(A4)+
    BRA EXPLOOP
.OD2:   MOVE.L  .ASR(PC),(A4)+
    MOVE.L  .ASR+4(PC),(A4)+
    BRA EXPLOOP
.ADD:   ADD.L   D0,D0
.ASL:   ASL.L   #2,D0
.ASR:   TST.L   D0
    BGE.S   .AS
    ADDQ.L  #1,D0
.AS:    ASR.L   #1,D0
.AS2:   TST.L   D0
    BGE.S   .ASS
    ADDQ.L  #3,D0
.ASS:   ASR.L   #2,D0

.VALDIVMUL:
    cmp.l   #EXPDIV,A1
    bne     .VALMUL
    SUBQ.L  #2,A4
    MOVE.L  #$4c7c0800,(a4)+
    move.l  d1,(a4)+
    bra EXPLOOP
.VALMUL:
    SUBQ.L  #2,A4
    MOVe.L  #$4c3c0800,(A4)+
    MOVE.L  D1,(A4)+
    BRA EXPLOOP


;; Macro Section 1
; some useful macros
MoveD0D2: macro
    move.w  .CODE,(a4)+
    endm
Mul2D0: macro
    move.w  .CODE+2,(A4)+
    endm
AddD2D0: macro
    move.w  .CODE+4,(A4)+
    endm
SubD2D0: macro
    move.w  .CODE+6,(a4)+
    endm
Mul2D2: macro
    move.w  .CODE+8,(a4)+
    endm
Asl3D0: macro
    move.w  .CODE+10,(a4)+
    endm
Asl4D0: macro
    move.w  .CODE+12,(a4)+
    endm
LocateCode: macro
.CODE:
    move.l  d0,d2
    add.l   d0,d0
    add.l   d2,d0
    sub.l   d2,d0
    add.l   d2,d2
    asl.l   #3,d0
    asl.l   #4,d0
    endm
;*-*
.FAST_OPTI:
    subq.l  #2,a4
;; *2
    cmp.l   #2,d1
    bne     .FO3
    Mul2D0
    bra     EXPLOOP
;*-*
;; *3
.FO3:
    cmp.l   #3,D1
    bne     .FO4
    MoveD0D2
    Mul2D0
    AddD2D0
    bra     EXPLOOP
;*-*
;; *4
.FO4:
    cmp.l   #4,D1
    bne     .FO5
    Mul2D0
    Mul2D0
    bra     EXPLOOP
;*-*
;; *5
.FO5:
    cmp.l   #5,D1
    bne     .FO6
    MoveD0D2
    Mul2D0
    Mul2D0
    AddD2D0
    bra     EXPLOOP
;*-*
;; *6
.FO6:
    cmp.l   #6,D1
    bne     .FO7
    Mul2D0
    MoveD0D2
    Mul2D0
    AddD2D0
    bra     EXPLOOP
;*-*
;; *7
.FO7:
    cmp.l   #7,D1
    bne     .FO8
    MoveD0D2
    Asl3D0
    SubD2D0
    bra     EXPLOOP
;*-*
;; *8
.FO8:
    cmp.l   #8,D1
    bne     .FO9
    Asl3D0
    bra     EXPLOOP
;*-*
;; *9
.FO9:
    cmp.l   #9,D1
    bne     .FOA
    MoveD0D2
    Asl3D0
    AddD2D0
    bra     EXPLOOP
;*-*
;; *10
.FOA:
    cmp.l   #10,D1
    bne     .FOB
    MoveD0D2
    Asl3D0
    Mul2D2
    AddD2D0
    bra     EXPLOOP
;*-*
;; *11
.FOB:
    cmp.l   #11,D1
    bne     .FOC
    MoveD0D2
    Asl3D0
    AddD2D0
    Mul2D2
    AddD2D0
    bra     EXPLOOP
;*-*
;; *12
.FOC:
    cmp.l   #12,D1
    bne     .FOD
    Mul2D0
    Mul2D0
    MoveD0D2
    Mul2D0
    AddD2D0
    bra     EXPLOOP
;*-*
;; *13
.FOD:
    cmp.l   #13,D1
    bne     .FOE
    MoveD0D2
    Asl3D0  ; *8
    AddD2D0 ; *9
    Mul2D2  ; *9
    Mul2D2  ; *9
    AddD2D0 ; *13
    bra     EXPLOOP
;*-*
;; *14
.FOE:
    cmp.l   #14,D1
    bne     .FOF
    MoveD0D2 ;
    Asl4D0   ; *16
    Mul2D2   ;
    SubD2D0  ; *14
    bra     EXPLOOP
;*-*
;; *15
.FOF:
    cmp.l   #15,D1
    bne     .FOG
    MoveD0D2
    Asl4D0
    SubD2D0
    bra     EXPLOOP
;*-*
;; *16
.FOG:
    cmp.l   #16,D1
    bne     .FOH
    Asl4D0
    bra     EXPLOOP
;*-*
;; *17
.FOH:
    cmp.l   #17,D1
    bne     .FOI
    MoveD0D2
    Asl4D0
    AddD2D0
    bra     EXPLOOP
;*-*
;; *18
.FOI:
    cmp.l   #18,D1
    bne     .FOJ
    Mul2D0      ; *2
    MoveD0D2
    Asl3D0      ; *2*8
    AddD2D0     ; *(2*8+2)
    bra     EXPLOOP
;*-*
;; *19
.FOJ:
    cmp.l   #19,D1
    bne     .FOK
    MoveD0D2 ; *:
    Asl4D0   ; 16
    AddD2D0  ; 17
    Mul2D2   ;
    AddD2D0  ; 19
    bra     EXPLOOP
;*-*
;; *20
.FOK:
    cmp.l   #20,D1
    bne     .FOL
    Mul2D0      ; 2
    MoveD0D2    ; *
    Asl3D0      ; 16
    AddD2D0     ; 18
    AddD2D0     ; 20
    bra     EXPLOOP
;*-*
;; *21
.FOL:
    cmp.l   #21,D1
    bne     .FOM
    MoveD0D2 ; *:
    Asl4D0   ; 16
    AddD2D0  ; 17
    Mul2D2   ;
    AddD2D0  ; 19
    AddD2D0  ; 21
    bra     EXPLOOP
;*-*
;; *22
.FOM:
    cmp.l   #22,D1
    bne     .FON
    Mul2D0      ; 2
    MoveD0D2    ; *
    Asl3D0      ; 16
    AddD2D0     ; 18
    AddD2D0     ; 20
    AddD2D0     ; 22
    bra     EXPLOOP
;*-*
;; *23
.FON:
    cmp.l   #23,D1
    bne     .FOO
    MoveD0D2 ; *:
    Asl4D0   ; 16
    AddD2D0  ; 17
    Mul2D2   ;
    AddD2D0  ; 19
    AddD2D0  ; 21
    AddD2D0  ; 23
    bra     EXPLOOP
;*-*
;; *24
.FOO:
    cmp.l   #24,D1
    bne     .FOP
    Mul2D0      ; 2
    Mul2D0      ; 4
    MoveD0D2    ; *
    Mul2D0      ; 8
    Mul2D0      ; 16
    AddD2D0     ; 20
    AddD2D0     ; 24
    bra     EXPLOOP
;*-*
;; *25
.FOP:
    cmp.l   #25,D1
    bne     .FOQ
    MoveD0D2 ; *:
    Asl4D0   ; 16
    AddD2D0  ; 17
    Mul2D2   ;
    Mul2D2   ;
    AddD2D0  ; 21
    AddD2D0  ; 25
    bra     EXPLOOP
;*-*
;; *26
.FOQ:
    cmp.l   #26,D1
    bne     .FOR
    Mul2D0      ; 2
    MoveD0D2    ; *
    Asl3D0      ; 16
    AddD2D0     ; 18
    Mul2D2
    AddD2D0     ; 22
    AddD2D0     ; 26
    bra     EXPLOOP
;*-*
;; *27
.FOR:
    cmp.l   #27,D1
    bne     .FOS
    MoveD0D2 ; *:
    Asl4D0   ; 16
    AddD2D0  ; 17
    Mul2D2   ;
    AddD2D0  ; 19
    Mul2D2   ;
    AddD2D0  ; 23
    AddD2D0  ; 27
    bra     EXPLOOP
;*-*
;; *28
.FOS:
    cmp.l   #28,D1
    bne     .FOT
    Mul2D0      ; 2
    MoveD0D2    ; *
    Asl3D0      ; 16
    Mul2D2      ;
    AddD2D0     ; 20
    AddD2D0     ; 24
    AddD2D0     ; 28
    bra     EXPLOOP
;*-*
;; *29
.FOT:
    cmp.l   #29,D1
    bne     .FOU
    MoveD0D2 ; *:
    Asl4D0   ; 16
    AddD2D0  ; 17
    Mul2D2   ;
    Mul2D2   ;
    AddD2D0  ; 21
    AddD2D0  ; 25
    AddD2D0  ; 29
    bra     EXPLOOP
;*-*
;; *30
.FOU:
    cmp.l   #30,D1
    bne     ERROR0
    Mul2D0      ; 2
    MoveD0D2    ; *
    Asl3D0      ; 16
    AddD2D0     ; 18
    Mul2D2      ;
    AddD2D0     ; 22
    AddD2D0     ; 26
    AddD2D0     ; 30
    bra     EXPLOOP
;*-*
    LocateCode
;*-*
;; Float value
VALUEFLOAT:
    CLR.L   LAST_CMD_ADR
    TST.W   FLTFLAG
    BEQ .0
    BTST    #1,CODEPREFS
    BNE EXPFPVAL
.0:
    CMP.W   #-1,D5
    BEQ.W   VALFLTBACK
    CMP.L   #EXPCMP,A1      ; NO AND/OR
    BMI.W   VALFLTBACK
    BRA EXPFLOATVAL

EXPFPVAL:
    CMP.W   #1,D5
    BEQ     ERROR0
    CMP.L   #EXPCMP,A1      ; NO AND/OR
    BMI.W   VALFLTBACK
    CMP.W   #-1,D5
    BEQ     .FPFIRST
    moveq   #0,d1
    cmp.l   #EXPPLUS,a1
    beq     .FADD
    cmp.l   #EXPMINUS,a1
    beq     .FSUB
    cmp.l   #EXPMUL,a1
    beq     .FMUL
    cmp.l   #EXPDIV,a1
    beq     .FDIV
    cmp.l   #EXPCMP,a1
    beq     .FCMP
    bra     ERROR0
.FADD:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #8,d0
    lea     .FC,a1
    add.l   d0,a1
    bra     .FFIX
.FSUB:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #8,d0
    lea     .FC+16,a1
    add.l   d0,a1
    bra     .FFIX
.FMUL:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #8,d0
    lea     .FC+32,a1
    add.l   d0,a1
    bra     .FFIX
.FDIV:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #8,d0
    lea     .FC+48,a1
    add.l   d0,a1
    bra     .FFIX
.FCMP:
    lea     .FC+64,a1
    moveq   #-1,d1
.FFIX:
    move.l  (a1),(a4)+
    move.l  (a3)+,(a4)+
    moveq   #1,d5
    tst.l   d1
    beq     EXPLOOP
    bsr     EXPFPCMP
    bra     EXPLOOP

.FC:
    FADD.S  #0,fp0    ; each 8
    fsadd.s #0,fp0
    fsub.s  #0,fp0
    fssub.s #0,fp0
    fmul.s  #0,fp0
    fsglmul.s #0,fp0
    fdiv.s  #0,fp0
    fsgldiv.s #0,fp0
    fcmp.s  #0,fp0
.FPFIRST:
    move.w  #-1,FLTFLAG
    move.l  .FC2,(a4)+
    move.l  (a3)+,(a4)+
    moveq   #1,d5
    bra EXPLOOP
.FC2:   
    FSMOVE.S #0,FP0

EXPFLOATVAL:
    BTST    #1,CODEPREFS
    BNE     EXPFPVAL
    CMP.L   #EXPCMP,A1
    BMI ERROR0
    TST.W   D5
    BMI.S   .3
    BHI.W   ERROR0
    MOVE.W  .1(PC),(A4)+
    MOVE.L  (A3)+,(A4)+
    BSR MAKEFLTOPER
    TST.W   D0
    BNE.S   .2          ; SEE IF SPCMP
    MOVE.W  D3,(A4)+
    BSR EXP_EXTD0
.2: MOVEQ   #1,D5
    BRA EXPLOOP
.1: MOVE.L  #1,D1
.3: MOVE.W  .4(PC),(A4)+
    MOVE.L  (A3)+,(A4)+
    BRA.S   .2
.4: MOVE.L  #1,D0



EXPNEGFLOAT:
    ADDQ.L  #2,A3
    MOVEM.L D0-D1/A0-A1/A6,-(A7)
    BSR ISOPENMATH
    MOVE.L  MATHBASE(PC),A6
    MOVE.L  (A3),D0
    JSR -60(A6)         ; SPNEG
    MOVE.L  D0,(A3)
    MOVEM.L (A7)+,D0-D1/A0-A1/A6
    BRA.S   EXPFLOATVAL
;*-*
;; Short
EXPSHORT:
    CMP.L   #-128,D1
    BMI.W   SHORTBACK
    MOVE.W  (A1),D0
    CMP.W   EXPMOVE,D0
    BEQ.S   .1
    CMP.W   EXPPLUS,D0
    BEQ.S   SHORTADD
    CMP.W   EXPMINUS,D0
    BEQ.S   SHORTADD
    BRA SHORTBACK
.1: MOVE.B  EXPSTART,(A4)+
    MOVE.B  D1,(A4)+
    BRA EXPLOOP
SHORTADD:
    CMP.W   #9,D1
    BPL SHORTBACK
    CMP.W   #1,D1
    BMI SHORTBACK
    CMP.W   #8,D1
    BNE.S   SHORTNO8
    MOVEQ   #0,D1
SHORTNO8:
    CMP.W   EXPPLUS,D0
    BNE.S   SHORTSUB
    MOVE.W  EXPADDQ,D2
    ASL.W   #7,D1
    ASL.W   #2,D1
    OR.W    D1,D2
    MOVE.W  D2,(A4)+
    BRA EXPLOOP
SHORTSUB:
    CMP.W   EXPMINUS,D0
    BNE SHORTBACK
    MOVE.W  EXPSUBQ,D2
    ASL.W   #7,D1
    ASL.W   #2,D1
    OR.W    D1,D2
    MOVE.W  D2,(A4)+
    BRA EXPLOOP
;*-*
;; Variables
EXPVAR:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    MOVE.L  (A3)+,A0
    MOVE.L  A0,LASTVAR
    MOVE.W  (A3),D0         ; CHECK COMPLEX
    CMP.W   #11,2(A3)
    BEQ     EXPEXT
EXPEXTBACK:
    CMP.W   #33,D0          ; ++/--
    BPL EXPVARC
    CMP.W   #29,D0          ; []
    BEQ EXPVARC
    CMP.W   #41,D0          ; ::
    BEQ EXPVARC
    CMP.W   #17,D0          ; ()
    BEQ EXPOWNFUNC
EXPVARCBACK:
    CMP.W   #ASSGN,(A3)     ; :=
    BEQ EXPVARASSIGN
EXPVARASSBACK:
    MOVE.L  A4,LAST_CMD_ADR
    MOVE.W  10(A0),D0       ; D0=VAROFFSET
    MOVE.B  4(A0),D2        ; D2=TYPE
    CMP.B   #3,D2
    BEQ ERROR23
    CMP.W   #2,D4
    BEQ EXPVARCMP
    CMP.W   #3,D4
    BEQ .SHIFT
    TST.W   FLTFLAG
    BNE .2
.4: CMP.W   #4,D4           ; TAKE LOWWORD IF MULU/DIVU
    BNE.S   .1
    CMP.W   #1,ECPU
    BPL     .VARDIVMUL
    ADDQ.W  #2,D0
    OPINT
.1: MOVE.W  0(A1,D4.W),(A4)+
    BSR GVA0D0D5_0
    OPLONG
    MOVEQ   #1,D5
    CMPA.L  #EXPDIV,A1
    BNE EXPLOOP
    MOVE.W  D0EXT(PC),(A4)+
    BRA EXPLOOP

.VARDIVMUL:
    clr.w   (a4)+
    CMPA.L  #EXPDIV,A1
    BNE     .VARMUL
    MOVE.W  #$4c6d,(A4)+
    MOVE.L  A4,-(A7)
    BSR GVA0D0_0
    MOVEq   #0,d0
    move.l  (a7)+,d2
    CMP.L   A4,D2
    beq     .VDM1
    MOVE.W  -(A4),D0
.VDM1:
    move.w  -(a4),-(a4)
    addq.l  #2,a4
    move.w  #$0800,(a4)+
    TST.W   D0
    BEQ     .VDM2
    MOVE.W  D0,(A4)+
.VDM2:
    MOVEQ   #1,D5
    BRA EXPLOOP
.VARMUL:
    MOVE.W  #$4c2d,(A4)+
    MOVE.L  A4,-(a7)
    BSR GVA0D0_0
    MOVEq   #0,d0
    move.l  (a7)+,d2
    CMP.L   A4,D2
    beq     .VDM1
    MOVE.W  -(A4),D0
    BRA     .VDM1


.SHIFT:
    MOVE.W  .3(PC),(A4)+
    BSR GVA0D0D5_0
    MOVEQ   #1,D5
    MOVE.W  (A1),(A4)+
    BRA EXPLOOP
.3: MOVE.L  2(A5),D1
.2:
    CMP.L   #EXPCMP,A1      ; NO AND/OR
    BMI .4
    BTST    #1,CODEPREFS
    BNE EXPFPVAR
    CMP.W   #-1,D5          ; SPXXX
    BEQ .4
BLERK:
    MOVE.W  .3(PC),(A4)+
    BSR GVA0D0D5_0
    MOVEQ   #1,D5
    BSR MAKEFLTOPER
    BRA EXPLOOP
.3: MOVE.L  2(A5),D1

EXPFPVAR:
    cmp.w   #-1,D5
    bne     .FNOT1ST
    moveq   #0,d1
    cmp.w   #2,ECPU
    spl     d0
    and.l   #6,d0
    lea     .FC0,a1
    add.l   d0,a1
    bra     .FFIX
.FC0:
    FMOVE.S  2(A5),FP0
    FSMOVE.S 2(a5),FP0

.FNOT1ST:
    moveq   #0,d1
    CMP.L   #EXPPLUS,A1
    BEQ     .FADD
    CMP.L   #EXPMINUS,A1
    BEQ     .FSUB
    CMP.L   #EXPDIV,A1
    BEQ     .FDIV
    CMP.L   #EXPMUL,A1
    BEQ     .FMUL
    CMP.L   #EXPCMP,A1
    BEQ     .FCMP
    BRA     ERROR0
.FADD:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #6,d0
    lea     .FC1,a1
    add.l   d0,a1
    bra     .FFIX
.FSUB:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #6,d0
    lea     .FC1+12,a1
    add.l   d0,a1
    bra     .FFIX
.FMUL:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #6,d0
    lea     .FC1+24,a1
    add.l   d0,a1
    bra     .FFIX
.FDIV:
    cmp.w   #2,ECPU
    spl     d0
    and.l   #6,d0
    lea     .FC1+36,a1
    add.l   d0,a1
    bra     .FFIX
.FCMP:
    lea     .FC1+48,a1
    moveq   #-1,d1
.FFIX:
    move.w  (a1),(a4)+
    move.l  a4,a2
    jsr     GVA0D0_0
    cmp.l   a4,a2
    bne     .FFIX2
    move.w  2(a1),(a4)+
    moveq   #1,d5
    bra     .FXIT
.FFIX2:
    move.w  -(a4),d0
    move.w  2(a1),(a4)+
    move.w  d0,(a4)+
    moveq   #1,d5
.FXIT:
    tst.l   d1
    bpl     EXPLOOP
    bsr     EXPFPCMP
    bra     EXPLOOP

.FC1:
    fadd.s  2(a5),fp0
    fsadd.s 2(a5),fp0
    fsub.s  2(a5),fp0
    fssub.s 2(a5),fp0
    fmul.s  2(a5),fp0
    fsglmul.s 2(a5),fp0
    fdiv.s  2(a5),fp0
    fsgldiv.s 2(a5),fp0
    fcmp.s  2(a5),fp0

EXPEXT:
    CMP.W   #-1,D5
    BNE     EXPEXTBACK
    CMP.W   #7,D0
    BEQ     .PARSE
    CMP.W   #8,D0
    BEQ     .PARSE
    CMP.W   #9,D0
    BEQ     .PARSE
    CMP.W   #10,D0
    BEQ     .PARSE
    CMP.W   #49,D0
    BEQ     .PARSE
    CMP.W   #50,D0
    BEQ     .PARSE
    CMP.W   #IOFF+29,D0
    BEQ     .PARSE
    CMP.W   #IOFF+30,D0
    BEQ     .PARSE
    BRA     EXPEXTBACK
.PARSE:
    ADDQ.L  #4,A3
    MOVEM.L D0-A2/A5/A6,-(A7)
    BSR     EXP
    MOVEM.L (A7)+,D0-A2/A5/A6
    BTST.B  #3,5(A0)
    BNE     .REGVAR

    CMP.W   #9,D0
    BMI     .GOOD
    CMP.W   #IOFF+29,D0
    BPL     .GOOD
    MOVE.W  .C1(PC),(A4)+
    BSR     GVA0D1_0
    SUB.W   #9,D0
    CMP.W   #2,D0
    BMI.S   .S1
    SUB.W   #38,D0
.S1:LSL.L   #1,D0

    MOVE.W  .C2(PC,D0),(A4)+
    MOVE.W  .C1+4(PC),(A4)+
    BSR     GVA0D2_9
    MOVE.W  .C1+8(PC),(A4)+
    MOVEQ   #1,D5
    BRA EXPLOOP
.C1:MOVE.L  -4(A5),D1
    MOVE.L  D1,-4(A5)
    MOVE.L  D1,D0
.C2:MULS.W  D0,D1
    DIVS.W  D0,D1
    ASL.L   D0,D1
    ASR.L   D0,D1
.C3:ADD.L   D0,-4(A5)
    SUB.L   D0,-4(A5)
    AND.L   D0,-4(A5)
    OR.L    D0,-4(A5)
.GOOD:
    BTST.B  #3,5(A0)
    BNE     .REGVAR
    SUb.W   #7,D0
    cmp.w   #2,d0
    bmi     .S2
    sub.w   #IOFF+20,D0
.S2:LSL.L   #2,D0
    MOVE.W  .C3(PC,D0),(A4)+
    BSR GVA0D1_0
    MOVEQ   #1,D5
    BRA EXPLOOP

    MOVE.W  (A3),2(A3)
    MOVE.L  -4(A3),-2(A3)
    MOVE.W  -6(A3),-4(A3)
    SUBQ.L  #6,A3
    MOVE.W  #ASSGN,(A3)
    BRA     EXPVARASSIGN
.T1:DC.W    7,8,9,10,49,50,IOFF+29,IOFF+30,-1

.REGVAR:
    LEA     .T1(PC),A2
    moveq   #0,d2
.R01:
    MOVE.W  (a2)+,d1
    cmp.w   d1,d0
    beq     .R02
    addq.l  #1,d2
    bra     .R01
.C4:
    ADD.L   D0,D0
    SUB.L   D0,D0
    MULS    D0,D0
    DIVS    D0,D0
    ASL.L   d0,D0
    ASR.L   D0,D0
    ANd.L   D0,D0
    OR.L    D0,D0
.C5:
    MULS.L  D0,D0
    DIVS.L  D0,D0
    MOVE.L  D0,D0
.R02:
    lsl.w   #1,d2
    MOVE.W  .C4(PC,d2),(A4)+
    MOVE.W  10(A0),D0

    cmp.w   #1,ECPU
    blt     .R001
    cmp.w   #2*2,d2
    bmi     .R001
    cmp.w   #3*2,d2
    bgt     .R001
    subq.l  #2*2,d2
    lsl.w   #1,d2
    subq.l  #2,a4
    move.L  .C5(PC,D2),(A4)+
    or.w    D0,-2(A4)
    lsl.w   #8,d0
    lsl.w   #4,d0
    bra     .R002
.R001:
    cmp.w   #4*2,d2
    beq     .R002
    cmp.w   #5*2,d2
    beq     .R002
    LSL.L   #8,d0
    lsl.l   #1,d0
.R002:
    or.w    d0,-2(A4)
    MOVEQ   #1,d5
    move.w  .C5+8(PC),(A4)
    MOVE.W  10(A0),D0
    OR.W    D0,(A4)+
    BRA EXPLOOP


LASTVAR:
    DC.L    0
;*-*
;; < <= = => > <>
EXPVARCMP:
    CLR.L   LAST_CMD_ADR
    TST.W   FLTFLAG
    BNE .1
    MOVE.W  6(A1),(A4)+
    BSR GVA0D0D5_0
    MOVEQ   #1,D5
    MOVE.W  D3,(A4)+
    BSR EXP_EXTD0
    BRA EXPLOOP

.1:
BLAARGH:
    BTST    #1,CODEPREFS
    BNE     .FLOAT

    MOVE.W  .2(PC),(A4)+        ; SPCMP
    BSR GVA0D0D5_0
    BSET    #3,CODEPREFS+1      ; USE MATHIEEESINGBAS
    MOVEQ   #1,D5
    MOVE.L  .3(PC),(A4)+
    MOVE.L  .3+4(PC),(A4)+
    MOVE.W  D3,(A4)+
    BSR EXP_EXTD0
    BRA EXPLOOP
.2: MOVE.L  2(A5),D1
.3: MOVE.L  -56(A4),A6
    JSR -42(A6)
.FLOAT:
    MOVEQ   #1,D5
    move.w  .FC,(a4)+
    move.l  a4,a2
    bsr GVA0D0D5_0
    cmp.l   a4,a2
    bne .F2
    move.w  .FC+2,(a4)+
    bra .F3
.F2:move.w  -(a4),d0
    move.w  .FC+2,(a4)+
    move.w  d0,(a4)+
.F3:bsr     EXPFPCMP
    bra     EXPLOOP
.FC:
    fcmp.s  2(a5),fp0
;*-*
;; FPCmp
EXPFPCMP:
    lea     EXPCMP,a1
    CMP.W   #3,EFPU
    BEQ .FLOAT_2
    lsl.l   #2,d3
    LEA     .FPCMP(PC),A0
    MOVE.L  (A0,D3),(A4)+
    BSR EXP_EXTD0
    RTS
.FLOAT_2:
    lsl.l   #2,d3
    MOVE.W  #$70FF,(A4)+
    LEA     .FPCMP2(PC),A0
    MOVE.L  (A0,D3),(A4)+
    move.w  #$7000,(a4)+
    RTS

.FPCMP:
    FSEQ    D0
    FSGT    D0
    FSLT    D0
    FSGE    D0
    FSLE    D0
    FSNE    D0
.FPCMP2:
    FBEQ    *+6
    FBGT    *+6
    FBLT    *+6
    FBGE    *+6
    FBLE    *+6
    FBNE    *+6
;*-*
;; :: .
EXPVARC:
    CLR.L   LAST_CMD_ADR
    CMP.W   #41,D0
    BEQ.S   .3
    CMP.W   #36,D0
    BPL EXPVARCBACK
.3: TST.B   4(A0)
    BEQ ERROR22
    CMP.B   #LAB,4(A0)
    BEQ ERROR6
    CMP.W   #42,2(A3)
    BEQ     .2
    MOVEQ   #1,D0
    SUBQ.L  #6,A3
    TST.W   D5
    BMI     .i0
    TST.W   FLTFLAG
    BEQ     .i0
    BTST    #1,CODEPREFS
    BEQ     .i0
    MOVE.L  .c0,(A4)+
.i0:
    BSR EADDRESSMODI
    TST.W   D5
    BMI     .i1
    TST.W   FLTFLAG
    BEQ     .i1
    BTST    #1,CODEPREFS
    BEQ     .i1
    MOVE.L  .c1,(A4)+
.i1:
    TST.W   FLTFLAG
    BEQ     .0
    BTST    #1,CODEPREFS
    BEQ     .0
    addq.l  #1,d5
    lsl.l   #2,d5
    move.l  .C0(PC,D5.w),(a4)+
    lsr.l   #2,d5
    subq.l  #1,d5
    cmp.w   #-1,d5
    beq .C1
    bsr MAKEFLTOPER
    tst.w   d0
    bne     .C1
    bsr EXPFPCMP
.C1:
    moveq   #1,d5
    bra EXPLOOP
    bra     .0
.C0:
    FMOVE.S D0,FP0
    FMOVE.S D1,FP1

.0: CMP.W   #-1,D5
    BEQ     .1
    BSR CLOSEEXP
.1: MOVEQ   #1,D5
    BRA EXPLOOP
.2: CMP.W   #35,D0
    BNE     .3
    TST.B   4(A0)
    BEQ ERROR22
    ADDQ.L  #2,A3
    MOVE.L  A0,METHODIDENT      ; DO METHOD CALL HERE!
    BSR SD0
    CLR.L   METHODSPEC
    BSR.S   EXPCALLMETHOD
    BSR RD0
    MOVEQ   #1,D5
    BRA EXPLOOP

.c0:FMOVE.S FP0,D0
.c1:FMOVE.S D0,FP0

METHODIDENT:    DC.L    0
METHODSPEC: DC.L    0
METHODFREEZ:    DC.L    0   ; FROZEN CALL WITH THIS OBJECT
;*-*
;; FindMethod
; WANTS   OBJ=A6,NAME=D0
; TRASHES A0
; RETURNS D1=METHOD | NIL

FINDMETHOD:
    LEA OMETHOD(A6),A6
    MOVE.L  A6,D1
.XL:MOVE.L  D1,A6
    MOVE.L  (A6),D1
    BEQ .ERR
    MOVE.L  D1,A6
    MOVE.L  M_NAME(A6),A0
    MOVE.L  D0,A6
.CL:    CMPM.B  (A0)+,(A6)+
    BNE.S   .XL
    TST.B   -1(A0)
    BNE.S   .CL
.ERR:   RTS
;*-*
;; CallMethod
EXPCALLMETHOD:            ; IDENTENTRY IN METHODIDENT,
    CMP.W   #42,(A3)+       ; A3 JUST PAST "."
    BNE ERROR40
    MOVE.L  METHODSPEC(PC),D0
    BNE.S   .NS1
    MOVE.L  METHODIDENT(PC),A6
    MOVE.L  (A6),D0
.NS1:   CMP.L   #5,D0
    BMI ERROR40
    MOVE.L  D0,A6           ; A6=OBJECT
    MOVE.L  (A3)+,D0        ; D0=NAME
    BSR FINDMETHOD
    TST.L   D1
    BEQ .ERR
    MOVE.L  METHODSPEC(PC),-(A7)
    BEQ.S   .MS4
    MOVE.W  .SA0(PC),(A4)+
.MS4:   MOVE.L  METHODFREEZ(PC),-(A7)
    MOVE.L  D1,-(A7)        ; STACK: S.L, F.L, M.L, I.L, COUNT.W
    CMP.W   #17,(A3)+
    BNE ERROR23
    MOVE.L  METHODIDENT,-(A7)
    CLR.W   -(A7)
    CMP.W   #18,(A3)
    BEQ.S   .2
.3: MOVE.W  #16,EAREQUEST
    BSR EAEXP
    TST.L   D0
    BNE.S   .OPT
    MOVE.W  .4(PC),(A4)+
.OPT:   ADDQ.W  #4,(A7)
    CMP.W   #COM,(A3)+
    BEQ.S   .3
    SUBQ.L  #2,A3
.2: CMP.W   #18,(A3)+       ; OUT
    BNE ERROR23

    MOVEQ   #0,D0
    MOVE.W  (A7)+,D0        ; D0=ARGCOUNT
    MOVE.L  4(A7),A6
    MOVE.L  M_PROC(A6),A6
    BSR DODEFARGS

    MOVE.L  (A7)+,A0        ; A0.L=IDENT
    MOVE.L  (A7)+,A6        ; A6.L=METHOD
    MOVE.L  (A7)+,METHODFREEZ
    MOVE.L  (A7)+,METHODSPEC
    BNE.W   .MS2
    MOVE.W  .7(PC),(A4)+
    BSR GVA0D1_0
    MOVE.L  (A0),A0         ; A0=OBJ
    BRA.S   .MS3
.MS2:   MOVE.L  METHODSPEC(PC),A0
    MOVE.W  .GA0(PC),(A4)+
    MOVE.W  D0,(A4)+
    ADDQ.W  #4,D0
.MS3:
    TST.L   METHODFREEZ
    BNE.S   .FR

    TST.W   ODELOFF(A0)
    BMI ERROR71
    BHI .U
    MOVE.W  .6(PC),(A4)+
.UB:    BRA.S   .NFR

.FR:    MOVEM.L A0/A6,-(A7)     ; FREEZ!
    MOVE.L  METHODFREEZ(PC),A6
    GETM    A0          ; ADD TO SUPER-ACCESS
    MOVE.L  OACC(A6),(A0)
    MOVE.L  A0,OACC(A6)
    ADDQ.L  #4,A0
    MOVE.W  .GOBJ(PC),(A4)+     ; GEN: PUT SUPER-DEL IN a1
    MOVE.L  A4,(A0)+
    CLR.W   (A4)+
    CLR.W   (A0)+
    DONEM   A0
    MOVEM.L (A7)+,A0/A6

.NFR:   MOVE.W  .6A(PC),(A4)+
    MOVE.W  M_OFF(A6),(A4)+
    MOVE.W  .6B(PC),(A4)+
    TST.W   D0
    BEQ.S   .NOA
    MOVE.W  .5(PC),(A4)+
    MOVE.W  D0,(A4)+
.NOA:   RTS
.4: MOVE.L  D0,-(A7)
.5: LEA -8(A7),A7
.7: MOVEA.L 2(A5),A0
.6: MOVE.L  (A0),A1
.6X:    MOVE.L  2(A0),A1
.6A:    MOVE.L  4(A1),A1
.6B:    JSR (A1)
.U: MOVE.W  .6X(PC),(A4)+
    MOVE.W  ODELOFF(A0),(A4)+
    BRA.W   .UB
.SA0:   MOVE.L  A0,-(A7)
.GA0:   MOVE.L  2(A7),A0
.ERR:   CMP.W   #17,(A3)
    BEQ ERROR54
    BRA ERROR42
.GOBJ:  MOVE.L  4(A4),A1
;*-*
;; :=
EXPVARASSIGN:         ; VAR STILL IN A0
    CMP.W   #-1,D5
    BNE EXPVARASSBACK
    ADDQ.L  #2,A3
    TST.W   EXPLCHECK
    BNE.W   .CH
.CHB:
    MOVE.L  A0,-(A7)
    BSR EXP
    MOVE.L  (A7)+,A0
    MOVE.B  4(A0),D2
    BEQ ERROR22
    CMP.B   #LAB,D2
    BEQ ERROR6
    MOVE.W  .1(PC),(A4)+
    BSR GVA0D5_9
    MOVEQ   #1,D5
    BRA EXPLOOP
.1: MOVE.L  D0,2(A5)
.CH:BTST    #3,5(A0)
    BEQ.W   .CHB
    MOVE.W  10(A0),D0
    MOVE.W  EXPLMASK,D1
    BTST    D0,D1
    BEQ.W   .CHB
    BRA ERROR25

;*-*
;; MakeFloatOper

MAKEFLTOPER:          ; TRASHES D0 !!!!
    BTST    #1,CODEPREFS
    BNE .MAKEFPOPER
    BSET    #3,CODEPREFS+1      ; USE MATHIEEESINGBAS
    MOVE.L  A1,D0
    SUB.L   #EXPCMP,D0
    DIVU    #3,D0
    MOVE.L  .2(PC),(A4)+
    MOVE.L  .1(PC,D0.W),(A4)+
    RTS
.1: JSR -42(A6)         ; = <> etc.
    JSR -66(A6)         ; +
    JSR -72(A6)         ; -
    JSR -78(A6)         ; *
    JSR -84(A6)         ; /
.MAKEFPOPER:
    CMP.W   #1,FINISHFPEXP
    BEQ     .MAKEFP1
    MOVE.L  A1,D0
    SUB.L   #EXPCMP,D0
    DIVU    #3,D0
    MOVe.L  .3(PC,D0),(A4)+
    RTS
.CHK:
    FMOVE.L D0,FP0
.3: FCMP.X  FP1,FP0
    FADD.X  FP1,FP0
    FSUB.X  FP1,FP0
    FMUL.X  FP1,FP0
    FDIV.X  FP1,FP0
.MAKEFP1:
    addq.l  #2,d1
    MOVE.W  #0,FINISHFPEXP      ; D1 = other reg
    MOVE.L  A1,D0
    SUB.L   #EXPCMP,D0
    DIVU    #3,D0
    MOVe.L  .4(PC,D0),(A4)+
    lsl.w   #7,d1
    or.w    d1,-2(a4)
    tst.l   d0
    beq .C
    move.l  .5(PC),(a4)+
    lsl.w   #3,d1
    or.w    d1,-2(a4)
.C:
    TST.W   D0
    SNE D0
    EXT.W   D0
    MOVE.W  D0,FLTFLAG
    RTS

.4: FCMP.X  FP0,FP0
    FADD.X  FP0,FP0
    FSUB.X  FP0,FP0
    FMUL.X  FP0,FP0
    FDIV.X  FP0,FP0
.5: FMOVE.X FP0,FP0
.2: MOVE.L  -56(A4),A6

;*-*
;; parse < <= = => > <>
EXPCOMPARATOR:

    CMP.W   #17,D0
    BPL EXPBACK2
    CMP.W   #11,D0
    BNE.S   .00
    CMP.W   #11,(A3)
    BEQ     EXP_CHECK
.00:
    CMP.W   #1,D5
    BNE .1
    CMP.W   #12,D0
    BEQ.S   .2
.3: TST.W   FLTFLAG
    BEQ .3_0
    BTST    #1,CODEPREFS
    BNE .FLT
.3_0:
    MOVEQ   #0,D5
    LEA EXPEQ(PC),A0
    MOVE.L  D0,D1
    SUB.W   #11,D1
    ASL.W   #1,D1
    MOVE.W  0(A0,D1.W),D3
    LEA EXPCMP(PC),A1
    MOVEQ   #2,D4
    BRA EXPLOOP
.1: CMP.W   #13,D0
    BEQ EXPCONS
    CMP.W   #16,D0
    BEQ EXPCONSNIL
    BRA ERROR0
.FLT:
    MOVE.L  D0,D3
    SUB.W   #11,D3
    EXT.L   D3
    MOVEQ   #2,D4
    LEA EXPCMP(PC),A1
    MOVEQ   #0,D5
    BRA EXPLOOP
.2: CMP.W   #2,EXPRECC      ; ONLY IF EXP IN EXP
    BMI.S   .3
    MOVE.W  (A3),D1
    CMP.W   #1,D1
    BMI.S   .4
    CMP.W   #COM,D1
    BEQ.S   .4
    CMP.W   #12,D1
    BEQ.S   .4
    CMP.W   #18,D1
    BEQ.S   .4
    CMP.W   #30,D1
    BEQ.S   .4
    CMP.W   #40,D1
    BEQ.S   .4
    CMP.W   #46,D1
    BEQ.S   .4
    CMP.W   #IOFF+18,D1
    BEQ.S   .4
    CMP.W   #IOFF+55,D1
    BEQ.S   .4
    BRA     .3
.4: BRA EXPOUT
;*-*
;; & || AND OR
EXPLOG:
    CMP.W   #IOFF+31,D0
    BPL ANDORNOTBACK
    CMP.W   #1,D5
    BNE ERROR0
;   TST.W   FLTFLAG
;   BNE ERROR0  ; NO AND/OR FLOAT
    MOVEQ   #0,D5
    MOVEQ   #6,D4
    SUB.W   #IOFF+29,D0
    MULU    #12,D0
    EXT.L   D0
    ADD.L   #EXPAND,D0
    MOVE.L  D0,A1
    BRA EXPLOOP
;*-*

;; Commands
EXPEQ:
    SEQ D0          ; = 11
    SGT D0          ; > 12
    SLT D0          ; < 13
    SGE D0          ; >=    14
    SLE D0          ; <=    15
    SNE D0          ; <>    16

EXPSTART:   MOVEQ   #0,D0

EXPAND:
    AND.L   #0,D0       ; 0
    AND.L   2(A5),D0    ; 6
    AND.L   D1,D0       ; 10
EXPOR:
    OR.L    #0,D0
    OR.L    2(A5),D0
    OR.L    D1,D0
EXPCMP:
    CMP.L   #0,D0
    CMP.L   2(A5),D0
    CMP.L   D1,D0
EXPPLUS:              ; NO CHANGES TO THESE 8 !
    ADD.L   #0,D0           ; EACH 12
    ADD.L   2(A5),D0
    ADD.L   D1,D0
EXPMINUS:
    SUB.L   #0,D0
    SUB.L   2(A5),D0
    SUB.L   D1,D0
EXPMUL:
    MULS    #0,D0
    MULS    2(A5),D0
    MULS    D1,D0
    DC.W    0
EXPDIV:
    DIVS    #0,D0
    DIVS    2(A5),D0
    DIVS    D1,D0
    dc.w    0
EXPMOVE:
    MOVE.L  #0,D0
    MOVE.L  2(A5),D0
    MOVE.L  D1,D0

EXPADDQ:    ADDQ.L  #8,D0
EXPSUBQ:    SUBQ.L  #8,D0
;*-*

;; Procedure
EXPOWNFUNC:           ; A0=IDENTENTRY
    CLR.L   LAST_CMD_ADR
    BTST    #4,5(A0)
    BNE ERROR4
    CMP.B   #LAB,4(A0)
    BNE EXPVARPROCCALL
    MOVE.L  6(A0),A6        ; A6=PROC!!
    MOVE.L  A6,D6

    MOVEQ   #0,D0
    BTST    #5,2(A6)
    BEQ     .skippars
    MOVE.L  6(A6),A6
    MOVE.W  (A6)+,D1
    subq.l  #1,d1

.check:
    CMP.B   #4,(A6)
    BNE     .chk1
    CMP.B   #3,1(A6)
    BMI     .chk1
    MOVEQ   #15,d2
    sub.b   1(a6),d2
    bset    d2,d0
    moveq   #16,d2
    add.b   1(a6),d2
    bset    d2,d0
.chk1:
    addq.l  #2,a6
    dbf     d1,.check

    tst.w   d0
    beq     .skippars
    move.w  #$48e7,(a4)+
    move.w  d0,(a4)+

.skippars:
    swap    d0
    move.w  d0,-(a7)
    MOVE.L  D6,A6
    TST.L   D6
    BEQ ERROR25
    CMP.W   #17,(A3)+
    BNE ERROR23
    BSR SD0
    MOVEQ   #0,D0           ; #OF ARGS
    CMP.W   #18,(A3)
    BEQ.S   .2
.3: MOVEM.L D0/A0/A6,-(A7)
    MOVE.W  #16,EAREQUEST
    BSR EAEXP
    TST.L   D0
    BNE.S   .OPT
    MOVE.W  .4(PC),(A4)+
.OPT:   MOVEM.L (A7)+,D0/A0/A6
    ADDQ.L  #4,D0
    CMP.W   #COM,(A3)+
    BEQ.S   .3
    SUBQ.L  #2,A3
.2: CMP.W   #18,(A3)+       ; OUT
    BNE ERROR23
    BSR DODEFARGS
    MOVEM.L D0/A0,-(A7)

    MOVE.W  10(A0),D0
    BMI.S   .MINM
    MOVE.L  .1(PC),(A4)+
    MOVE.W  .1L(PC),NEWOP
    MOVE.W  .1L2(PC),NEWOP020
    BSR ADDBRANCH
    BRA.S   .D
.MINM:  MOVE.W  .1L(PC),(A4)+       ; ADR OF LABEL OF OTHER MODULE
    MOVEM.L A1/A6,-(A7)
    GETM    A6
    MOVE.L  VARHEAVY(A0),A1     ; A1=PTR TO PROCCLASS
    MOVE.L  PC_ACC(A1),D0
    MOVE.L  A6,PC_ACC(A1)
    MOVE.L  D0,(A6)+
    MOVE.L  A4,(A6)+
    DONEM   A6
    MOVEM.L (A7)+,A1/A6
    CLR.L   (A4)+
.D:
    MOVEM.L (A7)+,D0/A0
    MOVE.L  6(A0),A6
    TST.W   (A6)
    BEQ.S   .NOA
    BTST    #0,2(A6)
    BNE.S   .NOA
    BTST    #5,2(A6)
    BNE.S   .NOA
    cmp.w   #8,d0
    ble     .DOSTK
    MOVE.W  .5(PC),(A4)+
    MOVE.W  D0,(A4)+
    bra     .NOA
.DOSTK:
    and.l   #7,d0
    lsl.w   #8,d0
    lsl.w   #1,d0
    or.w    .5o,d0
    move.w  d0,(a4)+
    bra     .NOA
.NOA:
    MOVED0FP0
    BSR RD0
    move.w  (a7)+,d0
    tst.w   d0
    beq     .exit
    move.w  #$4cdf,(a4)+
    move.w  d0,(a4)+
.exit:
    MOVEQ   #1,D5
    BRA EXPLOOP
.1: BSR .1
.1L:    JSR .1
.1L2:   BSR.L .1
.4: MOVE.L  D0,-(A7)
.5: ADDa.w  #8,a7
.5o:addq.l  #8,a7
DOREGARGS:
    MOVE.L  D0,D6
    LSR.L   #2,D6
    CMP.W   (A6),D6
    BNE     ERROR23
    move.l  6(a6),a6
    subq.l  #1,d6

    move.w  (a6)+,d0
    lsl.w   #1,d0
    add.w   d0,a6
    lsr.w   #1,d0
.L1:
    subq.l  #2,a6
    cmp.b   #5,(a6)
    seq     d0
    and.l   #$40,d0
    ror.l   #8,d0
    move.b  1(a6),d0
    and.w   #7,d0
    lsl.w   #1,d0
    rol.l   #8,d0
    move.w  .C1(PC),(A4)
    or.w    d0,(a4)+

    dbf     d6,.L1

    rts
.C1:
    MOVe.L  (A7)+,D0



DODEFARGS:
    BTST    #5,2(A6)
    BNE     DOREGARGS
    MOVE.L  D0,D6           ; DEAL WITH DEFARGS
    LSR.L   #2,D6
    CMP.W   (A6),D6
    BEQ.S   .R
    BPL ERROR23
    MOVE.W  (A6),D0
    EXT.L   D0
    LSL.L   #2,D0
    SUB.W   (A6),D6
    NEG.W   D6          ; D6=NARG WE STILL NEED
    MOVE.L  6(A6),A6
    MOVE.L  A6,D7
    BEQ ERROR23
    MOVE.W  (A6)+,D7
    BEQ ERROR23
    SUB.W   D6,D7
    BMI ERROR23
    LSL.W   #2,D7
    EXT.L   D7
    ADD.L   D7,A6
    SUBQ.L  #1,D6
.DAL:   MOVE.L  (A6)+,D7
    SWAP    D7
    TST.W   D7
    BEQ.S   .DP0
    CMP.W   #-1,D7
    BEQ.S   .DP1
.LP:    SWAP    D7
    MOVE.W  .P2(PC),(A4)+
    MOVE.L  D7,(A4)+
    BRA.S   .N
.DP0:   TST.L   D7
    BMI.S   .LP
    BRA.S   .DP
.DP1:   TST.L   D7
    BPL.S   .LP
.DP:    SWAP    D7
    MOVE.W  .P1(PC),(A4)+
    MOVE.W  D7,(A4)+
.N: DBRA    D6,.DAL
.R: RTS
.P1:    PEA $0.W
.P2:    PEA $0
;*-*
;; Var proc
EXPVARPROCCALL:           ; A0=IDENTENTRY
    CLR.L   LAST_CMD_ADR
    CLR.W   .F
    TST.B   4(A0)
    BEQ ERROR22
    CMP.W   #17,(A3)+
    BNE ERROR23
    BSR SD0
    MOVE.L  A0,-(A7)
    CLR.W   -(A7)
    CMP.W   #18,(A3)
    BEQ.S   .2
    MOVEQ   #0,D0
    MOVEQ   #0,D1
    cmp.b   #4,(a3)
    seq d0
    cmp.b   #5,(a3)
    seq d1
    or.w    d0,d1
    move.w  d1,.F
    moveq   #0,d1
.3: addq.l  #1,d1
    CMP.B   #4,(A3)
    BLT     .3_1
    CMP.B   #5,(A3)
    BGT     .3_1
    MOVE.W  (A3)+,-(A7)
    cmp.w   #11,(a3)+
    bne ERROR0
    tst.w   .F
    beq ERROR50
    BRA .3_2
.3_1:
    TST.W   .F
    BNE ERROR50
.3_2:
    MOVE.W  #16,EAREQUEST
    move.l  d1,-(a7)
    BSR EAEXP
    move.l  (a7)+,d1
    TST.L   D0
    BNE.S   .OPT
    MOVE.W  .4(PC),(A4)+
.OPT:
    ADDQ.W  #4,(A7)
    CMP.W   #COM,(A3)+
    BEQ.S   .3
    SUBQ.L  #2,A3
.2: TST.W   .F
    BNE     .FIXREGS
    CMP.W   #18,(A3)+       ; OUT
    BNE ERROR23
    MOVE.W  (A7)+,D0
    MOVE.L  (A7)+,A0
    MOVE.W  .7(PC),(A4)+
    BSR GVA0D1_0
    MOVE.W  .6(PC),(A4)+
    TST.W   D0
    BEQ.S   .NOA
    MOVE.W  .5(PC),(A4)+
    MOVE.W  D0,(A4)+
.NOA:
    MOVED0FP0
    BSR RD0
    MOVEQ   #1,D5
    BSET    #5,WARNINGS+3
    BRA EXPLOOP
.FIXREGS:
    SUBQ.L  #1,d1
.L1: move.w  (a7)+,d2
    ror.l   #8,d2
    cmp.b   #5,d2
    seq d0
    rol.l   #8,d2
    and.l   #$40,d0
    and.l   #7,d2
    bchg    #2,d2
    cmp.w   #4,d2
    blt     .L2
    cmp.w   #6,d2
    beq .L2
    cmp.w   #$40,d0
    beq     ERROR50
.L2:lsl.w   #8,d2
    lsl.w   #1,d2
    move.w  .0(PC),(a4)
    or.w    d2,(a4)
    or.w    d0,(a4)+
    dbf d1,.L1

    CMP.W   #18,(A3)+       ; OUT
    BNE ERROR23
    MOVE.W  (A7)+,D0
    MOVE.L  (A7)+,A0
    MOVE.W  .A(PC),(A4)+
    MOVE.W  .8(PC),(A4)+
    BSR GVA0D1_0
    MOVE.W  .9(PC),(A4)+
    MOVE.W  .C(PC),(A4)+

    BSR VP

    BSR RD0
    MOVEQ   #1,D5
    BSET    #5,WARNINGS+3
    BRA EXPLOOP


.4: MOVE.L  D0,-(A7)
.5: adda.w  #8,a7
.7: MOVEA.L 2(A5),A0
.6: JSR (A0)
.8: MOVE.L 2(A5),A5
.9: JSR (A5)
.0: MOVE.L  (A7)+,D0
.A: MOVE.L  A5,-(A7)
.C: MOVE.L  (A7)+,A5
.F: DC.W    0
VP:
    MOVED0FP0
    RTS


;*-*
;; Library
NUMRECSAVE = 8

EXPLIBCALL:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    LEA EXPLCHECK(PC),A0    ; HERE WE CHECK LOOPED CALLS!
    TST.W   (A0)
    BEQ.S   .1          ; NO NEED TO
    MOVEQ   #NUMRECSAVE-1,D0
.2: MOVE.L  (A0)+,-(A7)     ; SHOVE NUMRECSAVE LONGS UP STACK
    DBRA    D0,.2
.1: SUB.L   A6,A6           ; NEED OF REGS
    MOVE.W  (A3)+,EXPLBASE
    MOVE.W  (A3)+,EXPLOFF
    MOVE.L  (A3)+,A2
    MOVE.W  (A3)+,D2
    MOVE.W  (A3)+,EXPLEXCEPT
    MOVE.W  D2,EXPLMASK
    MOVEQ   #0,D0           ; D0=# OF REGS
    LEA EXPLREGS(PC),A0
    MOVE.W  D5,-(A7)        ; SAVE D5
EXPLLOOP:
    MOVEQ   #0,D1
    MOVE.B  (A2)+,D1        ; D1=REG#
    CMP.W   #16,D1
    BPL.S   EXPLNEXT
    ADDQ.L  #1,D0
    MOVEQ   #13,D6
    MOVEQ   #0,D7           ; D7=POS
    CMP.W   #12,D1
    BMI .1          ; A4/A5 CALL
    ADDQ.L  #1,A6
.1: MOVEQ   #13,D5
    SUB.W   D6,D5
    CMP.W   D1,D5
    BEQ.S   .X
    BTST    D5,D2
    BEQ.S   .2
    ADDQ.L  #1,D7
.2: DBRA    D6,.1
.X: MOVE.B  D7,(A0)+
    BRA.S   EXPLLOOP
EXPLNEXT:
    MOVE.W  (A7)+,D5        ; RESTORE D5
    MOVE.B  #-1,(A0)+
    MOVE.W  D0,EXPLNUM
    CMP.W   #17,(A3)+       ; CHECK INCOMING (
    BNE ERROR0
.CONTINUE:
    BSR SD0
    TST.W   EXPLNUM
        BEQ.S   EXPLNOARGS
        BSR.S   EXPLIBARG
        BRA.S   EXPLFIN
EXPLNOARGS:
    BSR.W   EXPLIBJSR
EXPLFIN:

    MOVE.W  EXPLEXCEPT(PC),D7   ; USE D7/A6
    BMI.S   .NR
    MULU    #10,D7
    MOVE.L  #LIBRAISE,A6
    LEA 0(A6,D7.W),A6
    BSR EXPRAISE
.NR:
    MOVED0FP0
    BSR RD0
    ADDQ.L  #2,A3           ; EAT )
    MOVEQ   #1,D5
    LEA EXPLCHECK(PC),A0    ; PUT LOOPED CALLS BACK AGAIN
    TST.W   (A0)
    BEQ.S   .3          ; NO NEED TO
    LEA NUMRECSAVE*4(A0),A0
    MOVEQ   #NUMRECSAVE-1,D0
.2: MOVE.L  (A7)+,-(A0)
    DBRA    D0,.2
.3: BRA EXPLOOP

EXPLIBARG:            ; IN CASE OF Func(arg,...)
    LSAVER  EXPLMASK,D1
    MOVE.W  D1,EXPLRSM
    CMP.W   #0,A6
    BEQ.S   .1
    MOVE.L  SAVE45(PC),(A4)+    ; NO OPTI IF A4/A5 USED
    BRA.S   .2
.1:
    TST.W   D1          ; NO OPTI IF D3-D7 USED
    BNE.S   .2

    MOVE.L  A3,D1           ; CHECK IF OPTI POSSIBLE
.XL:MOVE.W  (A3)+,D0
    CMP.W   #8,D0
    BNE.S   .T1
    CMP.W   #VALUE,(A3)+
    BNE.S   .XB
    ADDQ.L  #4,A3
    BRA.S   .C
.T1:    CMP.W   #VALUE,D0
    BNE.S   .T2
    ADDQ.L  #4,A3
    BRA.S   .C
.T2:    CMP.W   #IDENT,D0
    BNE.S   .T3
    ADDQ.L  #4,A3
    BRA.S   .C
.T3:    CMP.W   #STR,D0
    BNE.S   .XB
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,D7
    LSL.W   #1,D7
    ADD.W   D7,A3
    BTST    #2,CODEPREFS+3
    BNE.S   .XB
.C: CMP.W   #COM,(A3)+
    BEQ.S   .XL
    CMP.W   #18,-2(A3)
    BNE.S   .XB
    MOVEQ   #-1,D0
    BRA.S   .O
.XB: MOVEQ   #0,D0
.O: MOVE.L  D1,A3

; OPTFLAG NOW IN D0, CHOOSE BETWEEN TWO:

    TST.L   D0
    BNE OPTIARGS

.2: MOVE.W  EXPLNUM(PC),D0
    MOVE.W  D0,D1
    MULU    #-4,D1
    btst    #1,ICODEPREFS+3
    bne     .O1
    MOVE.W  EXPGETSTACK(PC),(A4)+
    BRA     .O2
.O1:move.w  EXPGETSTACKF(PC),(a4)+
.O2:MOVE.W  D1,(A4)+
    SUBQ.W  #1,D0
    LEA EXPLREGS(PC),A0
EXPLARGLOOP:
    MOVEM.L D0/A0/A6,-(A7)
    MOVE.W  EXPLCHECK(PC),-(A7)
    MOVE.W  #-1,EXPLCHECK
    bclr    #2,ICODEPREFS+3
    BSR EXP
    MOVE.W  (A7)+,EXPLCHECK     ; RESTORE PREVIOUS VALUE
    MOVEM.L (A7)+,D0/A0/A6
    MOVE.B  (A0)+,D1
    EXT.W   D1
    LSL.W   #2,D1
    btst    #2,ICODEPREFS+3
    bne     .NO_OPT
    TST.L   LAST_CMD_ADR
    bne     .TRY_OPTI
.NO_OPT:
    tst.l   d1
    beq     .1
    MOVE.W  EXPTOSTACK(PC),(A4)+
    MOVE.W  D1,(A4)+
    bra     .DONE_OPT
.1: move.w  #$2e80,(a4)+        ; move.l    d0,(a7)
.DONE_OPT:
    TST.W   D0
    BEQ     EXPLARGOUT
    CMP.W   #COM,(A3)+
    BNE ERROR5
    DBRA    D0,EXPLARGLOOP
    BRA     EXPLARGOUT
.TRY_OPTI:
    move.l  a2,-(a7)
    move.l  a4,d2
    sub.l   LAST_CMD_ADR,d2
    cmp.l   #2,d2
    beq     .TRY2
    cmp.l   #4,d2
    beq     .TRY4
    cmp.l   #6,d2
    bne     .NO_OPTI
    move.l  LAST_CMD_ADR,a2
    cmp.w   #$203c,(a2)         ;move.l #xxxx,yy(a7)
    bne     .NO_OPTI
    move.l  a2,a4
    tst.w   d1
    beq     .2
    move.w  #$2f7c,(a4)+
    addq.l  #4,a4
    move.w  d1,(a4)+
    bra     .DONE_OPTI
.2: move.w  #$2ebc,(a4)+
    addq.l  #4,a4
    bra     .DONE_OPTI
.TRY2:
    move.l  LAST_CMD_ADR,a2
    cmp.w   #$7000,(a2)
    bne     .NO_OPTI
    move.l  a2,a4
    tst.w   d1
    beq     .3
    move.w  #$42af,(a4)+
    move.w  d1,(a4)+
    bra     .DONE_OPTI
.3: move.w  #$4297,(a4)+
    bra     .DONE_OPTI
.TRY4:
    move.l  LAST_CMD_ADR,a2
    move.w  (a2),d2
    and.l   #$FFFE,d2
    cmp.w   #$202c,d2
    bne     .NO_OPTI
    move.l  a2,a4
    move.w  (a2),d2
    and.l   #1,d2
    tst.l   d1
    beq     .4
    or.w    #$2f6c,d2
    move.w  d2,(a4)+
    addq.l  #2,a4
    move.w  d1,(a4)+
    bra     .DONE_OPTI
.4: or.w    #$2eac,d2
    move.w  d2,(a4)+
    addq.l  #2,a4
    bra     .DONE_OPTI
.NO_OPTI:
    move.l  (a7)+,a2
    bra     .NO_OPT
.DONE_OPTI:
    move.l  (a7)+,a2
    bra     .DONE_OPT

EXPLARGOUT:
    MOVE.W  EXPGETBASE(PC),(A4)+
    MOVE.W  EXPLBASE(PC),D0
    BSR DOOFF
    MOVE.W  D0,(A4)+
    MOVE.W  EXPMOVEM(PC),(A4)+
    MOVE.W  EXPLMASK(PC),(A4)+
    MOVE.W  EXPJSR(PC),(A4)+
    MOVE.W  EXPLOFF(PC),(A4)+
    CMP.W   #0,A6
    BEQ.S   .1
    MOVE.L  RETRIEVE45(PC),(A4)+
.1: MOVE.W  EXPLRSM(PC),D0
    RESTR   D0
    RTS

OPTIARGS:
    MOVE.W  EXPLNUM(PC),D0
    SUBQ.W  #1,D0
    MOVE.L  -10(A3),A0
.1: MOVE.B  (A0)+,D1
    MOVEM.L D0/A0/A6,-(A7)
    MOVE.W  EXPLCHECK(PC),-(A7)
    MOVE.W  #-1,EXPLCHECK
    EXT.W   D1
    MOVE.W  D1,EAREQUEST
    BSR     EAEXP
    TST.L   D0
    BEQ.S   .3
    MOVE.W  (A7)+,EXPLCHECK
    MOVEM.L (A7)+,D0/A0/A6
    TST.W   D0
    BEQ.S   .2
    CMP.W   #COM,(A3)+
    BNE ERROR5
    DBRA    D0,.1
.2: MOVE.W  EXPGETBASE(PC),(A4)+
    MOVE.W  EXPLBASE(PC),D0
    BSR DOOFF
    MOVE.W  D0,(A4)+
    MOVE.W  EXPJSR(PC),(A4)+
    MOVE.W  EXPLOFF(PC),(A4)+
    RTS
.3: INTERN  100

EXPLIBJSR:            ; IN CASE OF Func()
    MOVE.W  EXPGETBASE(PC),(A4)+
    MOVE.W  EXPLBASE(PC),D0
    BSR DOOFF
    MOVE.W  D0,(A4)+
    MOVE.W  EXPJSR(PC),(A4)+
    MOVE.W  EXPLOFF(PC),(A4)+
    RTS

EXPJSR:       JSR -30(A6)
EXPGETBASE:   MOVE.L  -40(A4),A6
EXPGETSTACK:  adda.w #4,A7
EXPGETSTACKF: add.w #4,A7
EXPMOVEM:     MOVEM.L (A7)+,D0/A0
EXPTOSTACK:   MOVE.L  D0,4(A7)
SAVE45:       MOVEM.L A4/A5,-(A7)
RETRIEVE45:   MOVEM.L (A7)+,A4/A5

EXPLCHECK:    DC.W    0   ; NOTE: THESE DATA=NUMRECSAVE LONGS!
EXPLBASE:     DC.W    0
EXPLOFF:      DC.W    0
EXPLMASK:     DC.W    0
EXPLNUM:      DC.W    0
EXPLEXCEPT:   DC.W    0
EXPLOPTI:     DC.W    0
EXPLRSM:      DC.W    0
EXPLREGS:     DC.L    0,0,0,0
;*-*
;; EFunction
EXPEFUNC:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ ERROR0
    MOVE.L  (A3)+,A0        ; A0=FUNCTABENTRY

    MOVE.L  A0,D0
    SUB.L   #EFUNCTAB,D0
    DIVU    #EFUNCENTRYSIZE,D0
    LSL.W   #2,D0
    LEA     EFUNCFLAGSTAB,A6
    MOVE.L  0(A6,D0),D2
    BTST    #8,D2
    BNE     EXPINLINE

    MOVEQ   #0,D0           ; D0=COUNT ARGS
    MOVEQ   #-100,D2
    TST.L   8(A0)           ; WRITEF/PRINTF?
    BPL.S   .3
    MOVE.L  8(A0),D2
    SUBQ.L  #1,D2
    ASL.L   #2,D2
.3: CMP.W   #17,(A3)+
    BNE ERROR23
    BSR SD0
    SUBA.L  A6,A6           ; NO BACKPATCH SOFAR

.1: CMP.W   #18,(A3)
    BEQ.S   .2
    ADDQ.L  #4,D2
    BMI.S   .4
    TST.L   D2
    BNE.S   .1C
    MOVE.L  A4,A6           ; A6=BACKPATCH
    MOVE.L  .LEA(PC),(A4)+
.1C:    MOVEM.L A0/D0/D2/A6,-(A7)
    BSR.W   EXP
    MOVEM.L (A7)+,A0/D0/D2/A6
    TST.L   D2
    BNE.S   .1B
    MOVE.W  WRITEFARG+4(PC),(A4)+
    BRA.S   .6
.1B:    MOVE.W  WRITEFARG(PC),(A4)+
    MOVE.W  D2,(A4)+
    BRA.S   .6
.4: MOVEM.L A0/D0/D2/A6,-(A7)
    MOVE.W  #16,EAREQUEST
    BSR.W   EAEXP
    TST.L   D0
    BNE.S   .NUS
    MOVE.W  UPSTACK(PC),(A4)+
.NUS:   MOVEM.L (A7)+,A0/D0/D2/A6
.6: ADDQ.L  #1,D0
    CMP.W   #COM,(A3)+
    BEQ.S   .1
    SUBQ.L  #2,A3

.2: TST.L   8(A0)
    BMI.S   .5
    CMP.L   8(A0),D0
    BEQ .DAB
    BSR EFUNDA
.DAB:   BRA.S   .5B
.5: CMP.L   #-4,D2          ; ATLEAST 1-3 ARGS
    BMI ERROR23
    MOVE.L  A6,D1
    BEQ.S   .5C
    ADDQ.L  #4,D2
    NEG.L   D2
    MOVE.W  D2,2(A6)
.5C:    MOVE.L  D0,D2
    ADD.L   8(A0),D2
    ADDQ.L  #1,D0
    LSL.L   #2,D2
    MOVE.W  .P(PC),(A4)+
    MOVE.W  D2,(A4)+
.5B:    MOVE.L  EFUNCJUMP(PC),(A4)+
    MOVE.L  D0,D2
    MOVE.L  A0,D0
    SUB.L   #EFUNCTAB,D0
    DIVU.W  #EFUNCENTRYSIZE,D0
    LSL.W   #2,D0

    MOVE.L  A0,-(A7)
    LEA     EFUNCFLAGSTAB,A0
    MOVE.L  0(A0,D0),D7
    BTST    #0,D7
    BEQ.S   .T0
    BSET    #3,CODEPREFS+1
.T0:BTST    #1,D7
    BEQ.S   .T1
    BSET    #4,CODEPREFS+1
.T1:BTST    #2,D7
    BEQ     .T2
    CMP.W   #37,OSVERSION
    BLT     ERROR78
.T2:BTST    #3,D7
    BEQ     .T3
    CMP.W   #39,OSVERSION
    BLT     ERROR78
.T3:BTST    #4,D7
    BEQ     .T4
    CMP.W   #1,ECPU
    BLT     ERROR53
.T4:BTST    #5,D7
    BEQ     .T5
    CMP.W   #2,ECPU
    BLT     ERROR53
.T5:BTST    #6,D7
    BEQ     .T6
    CMP.W   #1,EFPU
    BLT     ERROR53
.T6:BTST    #7,D7
    BEQ     .T7
    CMP.W   #2,EFPU
    BLT     ERROR53
.T7:BTST    #8,D7
    BEQ     .T8
    BTST    #7,CODEPREFS
    BEQ     ERROR0
.T8:BTST    #10,D7
    BEQ     .T9
    BSET    #3,CODEPREFS
.T9:BTST    #11,D7
    BEQ     .TA
    BTST    #6,CODEPREFS
    BEQ     ERROR85
.TA:
    MOVE.L  (A7)+,A0

    LSR.W   #2,D0
    MOVE.L  D0,D7           ; INDEX
    ADD.W   #10,D0

    TSTMOD
    BNE.S   .MOD
    MOVE.W  EFUNCJUMPL,NEWOP
    MOVE.W  EFUNCJUMPL020,NEWOP020
    BSR ADDBRANCH
    BRA.S   .CC
.MOD:
    SUBQ.L  #4,A4
    MOVE.W  EFUNCJUMPL(PC),(A4)+
    BSR ADDBRANCHRELOC
    MOVE.W  D0,-2(A4)
.CC:
    MULU    #10,D7
    MOVE.L  #EFUNCRAISE,A6
    LEA 0(A6,D7.W),A6
    TST.W   (A6)
    BEQ.S   .NR
    BSR EXPRAISE
.NR:
    TST.W   D2
    BEQ.S   .E
    LSL.L   #2,D2
    cmp.w   #8,d2
    ble     .E2
    btst    #1,ICODEPREFS+3
    bne     .E3
    MOVE.W  EXPGETSTACK(PC),(A4)+
    MOVE.W  D2,(A4)+
    bra     .E
.E2:and.l   #7,d2
    lsl.w   #8,d2
    lsl.w   #1,d2
    or.w    .FS,d2
    move.w  d2,(a4)+
    bra     .E
.E3:move.w  .FF,(a4)+
    move.w  d2,(a4)+
.E:
    MOVED0FP0
    BSR RD0
    CMP.W   #18,(A3)+       ; EAT )
    BNE ERROR23
    MOVEQ   #1,D5
    BRA EXPLOOP
.LEA:   adda.w #4,A7
.P: PEA 0.W
.FS:addq.l  #8,a7
.FF:adda.w  #4,a7
EFUNDA: ADDQ.L  #1,D0
    CMP.L   8(A0),D0
    BNE ERROR23
    MOVE.L  12(A0),D6
    BEQ ERROR23
    CMP.L   #-1,D6
    BNE.S   .SK
    MOVEQ   #0,D6
.SK:    MOVE.L  D6,D1           ;
    AND.L   #$FFFFFF00,D1       ; not really needed?
    BNE ERROR23         ;
    EXT.W   D6
    MOVE.W  .P(PC),(A4)+
    MOVE.W  D6,(A4)+
    RTS
.P: PEA 0.W

UPSTACK:
    MOVE.L  D0,-(A7)
EFUNCJUMP:
    BSR EFUNCJUMP
EFUNCJUMPL:
    JSR EFUNCJUMP
EFUNCJUMPL020:
    BSR.L EFUNCJUMP
WRITEFARG:
    MOVE.L  D0,4(A7)
    MOVE.L  D0,(A7)


EXPRAISE:             ; GETS PTR TO RAISESTRUCT IN A6,
    MOVEM.L D0/D1/A0,-(A7)      ; TRHASHES ONLY A6
    TST.L   6(A6)
    BEQ.S   .Z
    MOVE.W  .1(PC),(A4)+
    MOVE.L  6(A6),(A4)+
    BRA.S   .Z2
.Z: MOVE.W  .1B(PC),(A4)+
.Z2:    MOVE.W  (A6),D0
    SUB.W   #11,D0
    LSL.W   #1,D0
    MOVE.W  .2(PC,D0.W),(A4)+
    MOVE.L  A4,A0
    MOVE.W  .3(PC),(A4)+
    MOVE.L  2(A6),(A4)+
    MOVE.L  .4(PC),(A4)+
    MOVEQ   #92,D0          ; CAUSE
    MOVE.B  #-1,EFUNCBYTE+82
    MOVEM.L D0/A0,-(A7)


    TSTMOD
    BNE.S   .MOD
    MOVE.W  .5(PC),NEWOP
    BSR ADDBRANCH
    BRA.S   .CC
.MOD:   SUBQ.L  #4,A4
    MOVE.W  .5(PC),(A4)+
    BSR ADDBRANCHRELOC
    MOVE.W  D0,-2(A4)
.CC:

    MOVEM.L (A7)+,D0/A0
    MOVE.L  A4,D0
    SUB.L   A0,D0
    MOVE.B  D0,-1(A0)
    MOVEM.L (A7)+,D0/D1/A0
    RTS
.1: CMP.L   #1,D0
.1B:    TST.L   D0
.2: BNE.S   .1
    BLE.S   .1
    BGE.S   .1
    BLT.S   .1
    BGT.S   .1
    BEQ.S   .1
.3: MOVE.L  #1,-(A7)
.4: BSR.W   .3
.5: JSR .3

;*-*
;; EXPINLINE
EXPINLINE:
    CLR.L   LAST_CMD_ADR
    tst.l   d5
    bgt     ERROR0
    bmi     .n_d
    BSR     SD0
.n_d:
    cmp.w   #17,(a3)+
    bne     ERROR0
    MOVE.L  D0,-(A7)
    LEA     INLINETAB,A0
    MOVE.L  0(A0,D0),A0
    JSR     (A0)

    MOVED0FP0
    tst.l   d5
    bmi     .exit
    bsr     RD0
.exit:
    MOVe.L  (A7)+,D0

    MOVE.L  A0,-(A7)
    LEA     EFUNCFLAGSTAB,A0
    MOVE.L  0(A0,D0),D7
    BTST    #0,D7
    BEQ.S   .T0
    BSET    #3,CODEPREFS+1
.T0:BTST    #1,D7
    BEQ.S   .T1
    BSET    #4,CODEPREFS+1
.T1:BTST    #2,D7
    BEQ     .T2
    CMP.W   #37,OSVERSION
    BLT     ERROR78
.T2:BTST    #3,D7
    BEQ     .T3
    CMP.W   #39,OSVERSION
    BLT     ERROR78
.T3:BTST    #4,D7
    BEQ     .T4
    CMP.W   #1,ECPU
    BLT     ERROR53
.T4:BTST    #5,D7
    BEQ     .T5
    CMP.W   #2,ECPU
    BLT     ERROR53
.T5:BTST    #6,D7
    BEQ     .T6
    CMP.W   #1,EFPU
    BLT     ERROR53
.T6:BTST    #7,D7
    BEQ     .T7
    CMP.W   #2,EFPU
    BLT     ERROR53
.T7:BTST    #10,D7
    BEQ     .T8
    BSET    #3,CODEPREFS
.T8:BTST    #11,D7
    BEQ     .T9
    BTST    #6,CODEPREFS
    BEQ     ERROR85
.T9:
    MOVE.L  (A7)+,A0

    moveq   #1,d5
    cmp.w   #18,(a3)+
    bne     ERROR74
    bra     EXPLOOP

;*-*
;; LibFunc
EXPLIBFUNC:
    CLR.L   LAST_CMD_ADR
    CMP.W   #1,D5
    BEQ     ERROR0

    MOVE.L  (A3)+,A6
    MOVE.L  A6,-(A7)


    MOVEQ   #0,D6
    MOVE.W  LIB_ARGS(A6),D5
    subq.l  #1,d5
.checkit1:
    lsl.l   #1,d5
    cmp.b   #4,20(a6,d5)
    bne     .checkit1x
    moveq   #7,d7
    sub.b   21(a6,d5),d7
    cmp.b   #5,d7
    bpl     .checkit1x
    bset    d7,d6
.checkit1x:
    lsr.l   #1,d5
    dbf     d5,.checkit1

;    move.w  d6,-(a7)
    tst.w   d6
    beq     .skp1
    lsl.l   #8,d6
    move.w  #$48e7,(a4)+
    move.w  d6,(a4)+
.skp1:


    CMP.W   #17,(A3)+
    BNE     ERROR0
    MOVEQ   #0,D6
    MOVE.W  D6,.TARGS
    tst.l   d5
    bmi     .X1
    BSR SD0
.X1:
    MOVE.W  #0,-(A7)
    CMP.W   #18,(A3)
    BEQ .NARGS
.ARGLOOP:
    ADDQ.W  #1,(A7)
    MOVE.W  #16,EAREQUEST
    BSR     EAEXP
    TST.L   D0
    BNE.S   .OPT
    MOVE.W  .4(PC),(A4)+
.OPT:
    CMP.W   #COM,(A3)+
    BEQ .ARGLOOP
    SUBQ.L  #2,A3
.NARGS:
    MOVE.W  (A7)+,.TARGS
    MOVE.L  (A7)+,A6
    MOVEQ   #0,D6
    MOVE.W  .TARGS,D6
    CMP.W   #18,(A3)+
    BNE     ERROR74
    TST.W   LIB_ARGS(A6)
    BPL     .ARGSPLUS

    MOVEQ   #0,D2
    MOVE.L  D6,D0
    ADDQ.L  #1,D6
    MOVE.W  LIB_ARGS(A6),D2
    EXT.L   D2
    NEG.L   D2
    SUBQ.L  #1,D2

    SUB.L   D2,D0
    BMI ERROR23
    LSL.L   #2,D0

    MOVE.W  .45(PC),(A4)+
    MOVE.W  .46(PC),(A4)+
    MOVE.W  D0,(A4)+
    MOVE.L  D0,D2
    LSR.L   #3,D2
    SUBQ.L  #1,D2
    BMI .CD
.XYZ:
    MOVE.L  .47(PC),(A4)+
    MOVE.W  .47+4(PC),(A4)+
    DBF     D2,.XYZ
.CD:
    SWAP    D0
    TST.W   D0
    BNE     .LOADS
    SWAP D0
    MOVE.W  .43(PC),(A4)+
    MOVE.W  D0,(A4)+
    BRA .ARGSPLUS2
.LOADS:
    SWAP    D0
    MOVE.W  .44(PC),(A4)+
    MOVE.L  D0,(A4)+
    BRA .ARGSPLUS2
.ARGSPLUS:
    CMP.W   LIB_ARGS(A6),D6
    BNE     ERROR23
.ARGSPLUS2:
    CMP.W   #1,LIB_TYPE(a6)
    beq .INLINE

    BSR     PUTLIBBRANCH

    LSL.W   #2,D6
    TST.L   D6
    BEQ .EXIT
    MOVE.W  .LEA(PC),(A4)+
    MOVE.W  D6,(A4)+
.EXIT:
    MOVED0FP0
    tst.l   d5
    bmi     .X2
    BSR RD0
.X2:
    MOVEQ   #1,D5
    BRA EXPLOOP

.TARGS:
    DC.W    0
.LEA:
    LEA     4(A7),A7
.4:
    MOVE.L  D0,-(A7)
.42:
    MOVE.L  (A7)+,D0
.43:
    PEA $0.W
.44:
    PEA $0
.45:
    MOVE.L  A7,A0
.46:
    LEA 4(A7),A1
.47:
    MOVE.L  (A0),D0
    MOVE.L  -(A1),(A0)+
    MOVE.L  D0,(A1)

.INLINE:
    MOVE.W  LIB_ARGS(A6),D5
    BEQ     .INLCNT
    SUBQ.L  #1,D5
.INLMOVE:
    MOVE.W  .42(PC),(A4)
    MOVEQ   #0,D0
    MOVE.B  20(a6,d5),d0
    cmp.b   #4,d0
    SPL     D1
    AND.W   #$40,D1
    OR.W    D1,(A4)
    AND.B   #3,D0
    LSL.L   #8,D0
    LSL.L   #1,D0
    OR.W    D0,(A4)+
    DBF D5,.INLMOVE
.INLCNT:
    MOVE.L  A6,-(A7)
    MOVE.L  LIB_CODE(A6),A6
    MOVE.L  (A6)+,D0
    LSR.L   #1,D0
    SUBQ.L  #1,D0
    MOVE.L  A4,-(A7)
.INLCPY:
    MOVE.W  (A6)+,(A4)+
    DBF D0,.INLCPY
    MOVE.L  (A7)+,A6
    MOVe.L  (A7)+,d0
    move.l  a0,-(a7)
    move.l  d0,a0
    subq.l  #4,a6
    JSR FIXRELOC
    move.l  (a7)+,a0
    BRA .EXIT

;*-*

;; GetExp
; params: D0 -> final storage
EA_GETEXP:
    move.l  d0,-(a7)

    BSR.W   EXP

    move.l  (a7)+,d0
    TST.W   D0
    BEQ     .x
    move.w  .res(PC),(A4)
    OR.W    D0,(A4)+
.x:
    RTS
.res:
    MOVE.L  D0,D0
;*-*

;; <=>
; WARNING: AMAZING FEATURE AHEAD!

EXPUNIFY:
    CMP.W   #1,D5
    BNE ERROR0
    MOVEQ   #-1,D7          ; D7 = LEVEL/#OF_AX_USED
    BSR NEWLABEL
    MOVE.L  D0,D6           ; D6 = FALSELABEL
    CLR.W   NEWOP
    BSR.S   .R

    CMP.W   #1,EXPRECC
    BNE.S   .DI
    TST.W   EXPSTAT
    BNE .SR

.DI:    CMP.W   #-2,D7
    BEQ.S   .SH
    MOVE.L  .TRUE(PC),(A4)+
    MOVE.L  D6,D0
    BSR ADDLABEL
    MOVE.W  .FALSE(PC),(A4)+
    BRA EXPLOOP
.SH:    MOVE.W  .TRUE(PC),(A4)+
    BRA EXPLOOP

.SR:    MOVE.L  D6,D0
    BSR ADDLABEL
    BRA EXPLOOP

.R: MOVE.W  (A3)+,D0        ; RECURSIVE PART
    CMP.W   #VALUE,D0
    BNE.S   .1
.0: MOVE.L  (A3)+,D0
.0B:    TST.L   D7          ; VALUE
    BMI.S   .1B
    MOVE.W  .CMP2(PC),(A4)
    OR.W    D7,(A4)+
    BRA.S   .1C
.1B:    MOVE.W  .CMP(PC),(A4)+
.1C:    MOVE.L  D0,(A4)+
    MOVE.L  .BNE(PC),(A4)+
    MOVE.L  D6,D0
    BSR ADDBRANCH
    BRA .X

.1: CMP.W   #IDENT,D0
    BNE .2
    TST.L   D7          ; IDENT
    BMI.S   .2B
    MOVE.W  .M2(PC),(A4)
    OR.W    D7,(A4)+
    BRA.S   .2C
.2B:    MOVE.W  .M(PC),(A4)+
    SUBQ.L  #1,D7           ; AT TOP LEVEL: ALWAYS TRUE
.2C:    MOVE.L  (A3)+,A0
    BSR GVA0D0_9
    BRA.W   .X

.2: CMP.W   #29,D0
    BNE.S   .3
    ADDQ.L  #4,A3           ; LIST
    CLR.W   -(A7)           ; TOP STACK = NUM ELEMENTS
    TST.L   D7
    BMI.S   .3B
    MOVE.W  .GET2(PC),(A4)
    OR.W    D7,(A4)
    BRA.S   .3C
.3B:    MOVE.W  .GET(PC),(A4)
.3C:    ADDQ.L  #1,D7
    CMP.W   #4,D7
    BEQ ERROR46
    MOVE.L  D7,D0
    MOVEQ   #9,D1
    LSL.W   D1,D0
    OR.W    D0,(A4)+
    MOVE.W  .LEN(PC),(A4)
    OR.W    D7,(A4)+
    MOVE.L  A4,A6           ; A6=PATCHBACK
    MOVE.W  D0,(A4)+        ; DUM D0
    MOVE.W  #-2,(A4)+
    MOVE.L  .BNE(PC),(A4)+
    MOVE.L  D6,D0
    BSR ADDBRANCH
.XL:MOVE.L  A6,-(A7)
    BSR.W   .R
    MOVE.L  (A7)+,A6
    ADDQ.W  #1,(A7)
    CMP.W   #COM,(A3)+
    BEQ.S   .XL
    CMP.W   #30,-2(A3)
    BNE ERROR34
    MOVE.W  (A7)+,(A6)
    SUBQ.L  #1,D7
    BRA.W   .X

.3: CMP.W   #8,D0
    BNE.S   .4
    CMP.W   #VALUE,(A3)+        ; -VALUE
    BNE ERROR30
    NEG.L   (A3)
    BRA .0

.4: CMP.W   #13,D0          ; <CONS|CELL>
    BNE.S   .5
    TST.L   D7
    BMI.S   .4B
    MOVE.W  .GET2(PC),(A4)      ; GET CELL IN A0
    OR.W    D7,(A4)
    BRA.S   .4C
.4B:    MOVE.W  .GET(PC),(A4)
.4C:    ADDQ.L  #1,D7
    CMP.W   #4,D7
    BEQ ERROR46
    MOVE.L  D7,D0
    MOVEQ   #9,D1
    LSL.W   D1,D0
    OR.W    D0,(A4)+
.4L:    BSR.W   .R
    CMP.W   #COM,(A3)+
    BNE.S   .4D
    MOVE.W  .CDR(PC),(A4)

    MOVE.L  D7,D0
    MOVEQ   #9,D1
    LSL.W   D1,D0
    OR.W    D7,D0
    OR.W    D0,(A4)+

    BRA.S   .4L
.4D:    CMP.W   #46,-(A3)
    BNE.S   .4E
    ADDQ.L  #2,A3
    BSR.W   .R
    BRA.W   .4F
.4E:    ; TEST CDR=0 HERE

    MOVE.W  .NIL(PC),(A4)
    MOVE.L  D7,D0
    OR.W    D7,(A4)+
    MOVE.L  .BNE(PC),(A4)+
    MOVE.L  D6,D0
    BSR ADDBRANCH

.4F:    CMP.W   #12,(A3)+
    BNE ERROR0
    SUBQ.L  #1,D7
    BRA.S   .X

.5: CMP.W   #16,D0
    BNE.S   .6
    MOVEQ   #0,D0
    BRA.W   .0B

.6: ; OTHER TYPES
    BRA ERROR0

.X: RTS

.CMP:   CMPI.L  #1,D0
.CMP2:  CMPI.L  #1,(A0)+
.M: MOVE.L  D0,2(A5)
.M2:    MOVE.L  (A0)+,2(A5)
.BNE:   BNE .CMP
.TRUE:  MOVEQ   #-1,D0
    BRA.S   .FALSE+2
.FALSE: MOVEQ   #0,D0
.GET:   MOVE.L  D0,A0
.GET2:  MOVE.L  (A0)+,A0
.LEN:   CMPI.W  #1,-2(A0)
.CDR:   MOVE.L  (A0),A0
.NIL:   TST.L   (A0)

;*-*
FINISHFPEXP:
    DC.W    0
;*-*

