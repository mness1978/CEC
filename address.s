;;ADDRESS

; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The Calculate-Addresses-For-BRAnches Part                   ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;


CALCADR:
    MOVE.W  #10,CURSPOT
    MOVE.L  CURACODE(PC),A4     ; A4=CODE
    MOVE.L  A5,-(A7)
    MOVE.L  HunkList(PC),A5
    move.l  A4,H_END(A5)
    MOVe.L  (a7)+,A5
    MOVE.L  ACODE(PC),D2
    MOVE.L  A4,D4
    BTST    #1,D4
    BEQ.S   .1
    CLR.W   (A4)+
    MOVE.L  A4,D4
.1: SUB.L   D2,D4
    MOVE.L  D4,CODEAMOUNT       ; CLEANOFF AND SET CODESIZE
    MOVE.L  D2,D7           ; D7=START CODE

    MOVEM.L D0-A6,-(A7)
    BSR CALLLIBFUNC
    MOVem.l (A7)+,D0-A6

    TSTMOD
    BNE.S   .NM
    BTST    #5,CODEPREFS
    BNE .NM
    TST.W   MAINF
    BEQ ERROR13
.NM:LEA BRANCHLIST(PC),A5   ; A5=BRANCHES
    MOVE.L  LABM+8(PC),A6       ; A6=LABELS
    MOVE.L  CURLABNAME(PC),D0
    LSL.L   #2,D0
    ADD.L   A6,D0
    MOVE.L  D0,LABM         ; SET USED
    BSR PUTRELOC
BRLOOP:
    MOVE.L  (A5),A5
    MOVE.L  A5,D0
    BEQ.S   BROUT
    MOVE.L  4(A5),A0        ; BRAnch spot
    MOVEQ   #0,D0
    MOVE.W  8(A5),D0
    BEQ.S   BROUT
    LSL.L   #2,D0
    MOVE.L  0(A6,D0.L),D2       ; BRAnch to
    BTST    #0,1(A0)        ; IF BIT=1 --> .B OR .L RELOC
    BNE.S   OTHERRELOC

    BSR THRUHUNKS
    TST.L   D0
    BNE     ERROR44

    SUB.L   A0,D2

    CMP.W   #1,10(A5)
    BNE .1
    ADDQ.L  #2,D2
;    ADDQ.L  #2,A0
.1:
    CMP.L   #$7FF0,D2
    BPL ERROR44
    CMP.L   #-$7FF0,D2
    BMI ERROR44
    MOVE.W  D2,(A0)
    BRA.S   BRLOOP
BROUT:
    MOVE.L  NUMRELOC,D0
    BEQ.S   .1
    CLR.L   (A4)+
    MOVE.L  NUMRELADR,A0
    MOVE.L  D0,(A0)
.1: BSR BUILDSYMHUNK
    MOVE.L  A4,CURACODE     ; SET BACK.
    RTS
OTHERRELOC:
    BTST    #3,1(A0)
    BNE     XLONG
    BNE ERROR45
    BTST    #1,1(A0)
    BNE     LONG
    BSR THRUHUNKS
    TST.L   D0
    BTST    #2,1(A0)        ; 8 bit pcrel
    BEQ.S   .1
    SUBQ.L  #2,D2
.1: SUB.L   A0,D2
    BEQ ERROR45
    CMP.L   #120,D2
    BPL ERROR45
    CMP.L   #-120,D2
    BMI ERROR45
    MOVE.B  D2,1(A0)        ; PTR A0 = RELOCEND-2
    BRA BRLOOP

LONG:
    TSTMOD                  ; ALL MODULES linked to hunk #0!
    BNE     .3
    BSR     FINDNFIX            ; NOT allowed in modules!
    BRA BRLOOP
.3:
    BSR INCRELOC
    TST.W   2(A0)           ; SEE IF THERE'S EFUNCNUM
    BNE.S   .2
    SUB.L   D7,D2           ; D7=start asm code
    MOVE.L  D2,(A0)         ; d2=destination
    SUB.L   D7,A0           ; a0=source
    MOVE.L  A0,(A4)+
    BRA BRLOOP
.2:
    SUB.L   D7,A0           ; EFUNC CALL, BIT #32 SET.
    MOVE.L  A0,D0
    MOVEQ   #31,D2
    BSET    D2,D0
    MOVE.L  D0,(A4)+
    BRA BRLOOP

INCRELOC:
    TST.L   NUMRELOC        ; 32BIT RELOC
    BNE.S   .1
    MOVE.L  #$3EC,(A4)+
    MOVE.L  A4,NUMRELADR
    ADDQ.L  #4,A4
    MOVE.L  A4,RELOCTABSTART
    CLR.L   (A4)+
.1: ADDQ.L  #1,NUMRELOC
    RTS

XLONG:
    BTST    #1,1(A0)
    BNE     .XRELOC
    SUB.L   A0,D2
    CMP.W   #1,10(A5)
    BNE .1
    ADDQ.L  #2,D2
.1:
    MOVE.L  D2,(A0)
    BRA     BRLOOP
.XRELOC:
    MOVE.L  10(A5),D2
    ADD.L   D7,D2
    BSR     FINDNFIX
    BRA     BRLOOP

;    MOVE.L  10(a5),(a0)
;    BSR INCRELOC
;    SUB.L   D7,A0
;    MOVE.L  A0,(A4)+
;    BRA     BRLOOP
;; ThruHunks - check if reference is between two hunks
THRUHUNKS:          ; D2 = dest, A0 = src
    MOVEM.L D1-A6,-(A7)
    MOVE.L  HunkList(PC),D0
.1: MOVE.L  D0,A1
    CMP.L   H_ADDR(A1),D2
    BGE     .2
    MOVE.L  H_NEXT(A1),D0
    BRA     .1
.2: MOVE.L  A0,D1
    CMP.L   H_ADDR(A1),D1
    BMI     .3
    CMP.L   H_END(A1),D1
    BGE     .3
    MOVEQ   #0,D0
    MOVEm.L (A7)+,D1-A6
    RTS
.3: MOVEQ   #-1,D0
    MOVEm.L (A7)+,D1-A6
    rts
;*-*
;; FindNFix - sth like INCRELOC, but for multiple hunks
FINDNFIX:           ; D7 = acode, D2 = dest, A0 = src
    MOVEM.L D0-A6,-(A7)
    TSTMOD
    BEQ     .NORMAL
    BSR INCRELOC
    SUB.L   A0,D2
    MOVE.L  D2,(A0)
    MOVE.L  A0,(A4)+
    BRA     .XIT
.NORMAL:
    MOVE.L  NumHunks(PC),D1
    SUBQ.L  #1,D1
    MOVE.L  HunkList(PC),D0     ; adresses from the last to the first
.1: MOVE.L  D0,A1
    CMP.L   H_ADDR(A1),D2
    BGE     .FND
    SUBQ.L  #1,D1               ; next hunk
    MOVE.L  H_NEXT(A1),D0
    BEQ     ERROR57
    bra     .1
.FND:                           ; here we have the hunk number :)
    MOVE.L  A1,A3               ; and the offset of REAL start
    MOVE.L  HunkList(PC),D0     ; now the hunk where to put the reloc
.2: MOVE.L  D0,A1
    CMPA.L  H_ADDR(A1),A0
    BGE     .FND2
    MOVE.L  H_NEXT(A1),D0
    BEQ ERROR57
    bra     .2
.FND2:
    MOVE.L  H_ADDR(A3),D0
    SUB.L   D0,D2
    MOVE.L  D2,(A0)
    GETM    A2
    MOVE.L  H_RELO(A1),(A2)+
    MOVE.L  A2,H_RELO(A1)
    MOVE.W  D1,(A2)+
    SUB.L   H_ADDR(A1),A0
    MOVE.L  A0,(A2)+            ; ta daaa!
    DONEM   A2
.XIT
    MOVEM.L (A7)+,D0-A6         ; filled! :)
    RTS
;*-*
;; CallLibFunc
CALLLIBFUNC:
    LEA LIBPTRS+4,A0
.LOOP:
    MOVE.L  -4(A0),D0
    BEQ     .EXIT
    MOVE.L  D0,A0
    MOVE.L  4(A0),A1
    MOVE.L  14(A1),D0
    MOVE.L  (A0),A1

    CMP.W   #1,8(A0)
    BEQ     .LONG

    SUB.L   A1,D0
    SUBQ.L  #2,D0


    SWAP    D0
    TST.W   D0
    BNE     ERROR44
    SWAP    D0
    MOVE.L  (A0),A1
    MOVE.W  D0,2(A1)
    BRA     .LOOP
.EXIT:
    RTS
.LONG:
    MOVE.L  A0,-(A7)
    MOVE.L  D2,-(A7)
    MOVE.L  A1,A0
    ADDQ.L  #2,A0
    MOVE.L  D0,D2
    BSR FINDNFIX
    MOVe.L  (A7)+,D2
    MOVE.L  (A7)+,A0
    BRA .LOOP

;    SUB.L   D7,D0
;    MOVE.L  D0,2(A1)
;    SUb.L   D7,A1
;    addq.l  #2,a1
;    BSR INCRELOC
;    MOVE.L  A1,(A4)+
;    BRA .LOOP

;*-*

NUMRELOC:   DC.L    0
NUMRELADR:  DC.L    0
LIBPTRS:    DC.L    0

; LAST RELOC BYTE:
; <0xx0> BIT #0 CLR -->                PCREL RELOC16
; <0001>        SET --> BIT #1 CLR --> PCREL RELOC8
; <0101>        SET --> BIT #1 CLR --> PCREL RELOC8 + ADJUST
; <0x11>                       SET --> ABS   RELOC32 (ENTRY IN RELOCTAB)
; <1001> BIT #0 SET --> BIT #1 CLR --> PCREL RELOC32


DIVRELOC: DC.L    0

ADDDIVRELOC:          ; ADR=D0
    MOVE.L  A0,-(A7)
    GETM    A0
    MOVE.L  DIVRELOC(PC),(A0)
    MOVE.L  A0,DIVRELOC
    ADDQ.L  #4,A0
    MOVE.L  D0,(A0)+
    DONEM   A0
    MOVE.L  (A7)+,A0
    RTS

PUTRELOC:
    MOVEM.L A0-A3/A6/D0-D7,-(A7)    ; EXPECTS CODE IN A4
    LEA CODELIST(PC),A0     ; A0=CODEREM
    MOVE.L  LABM+8,A2       ; A2=LABELS
.XL:MOVE.L  (A0),A0
    MOVE.L  A0,D0
    BEQ.S   .E
    CMP.W   #3,4(A0)
    BNE.S   .LL
    MOVE.L  6(A0),A3        ; A3=RELOCTAB
    MOVE.L  10(A0),D3       ; D3=RELOCLEN
    SUBQ.L  #1,D3
.RL:MOVE.L  (A3)+,D2
    BMI.S   .P
;    BSR INCRELOC
    ADD.L   D1,D2           ; ADD OFFSET IN D1

    MOVEM.L D2/d7/A0,-(A7)
    MOVE.L  ACODE(PC),D7
    ADD.L   D7,D2
    MOVE.L  D2,A0
    MOVE.L  (A0),D2
    ADD.L   D7,D2
    BSR     FINDNFIX
    MOVEM.L (A7)+,D2/d7/A0

;    MOVE.L  D2,(A4)+
.P: DBRA    D3,.RL
    BRA     .XL
.E: LEA DIVRELOC(PC),A2     ; ADD SOME VARIOUS RELOCS
    MOVE.L  ACODE,D7
.DL:MOVE.L  (A2),D0
    BEQ.S   .DX
    MOVE.L  D0,A2
    MOVE.L  D0,A0
    MOVE.L  4(A0),A0
    MOVE.L  (A0),D2
    ADd.L   D7,D2
    BSR FINDNFIX
    BRA.S   .DL
.DX:    MOVEM.L (A7)+,A0-A3/A6/D0-D7
    RTS
.LL:CMP.W   #2,4(A0)
    BNE.S   .PROC
    MOVE.L  10(A0),D1       ; D1=ADD OFFSET
    SUB.L   ACODE,D1
    BRA.S   .XL

.PROC:
    CMP.W   #5,4(A0)
    BNE   .XL
    MOVE.L  6(A0),A1

    MOVE.L  A1,D0
    HASH    A1,D2,D7
    LSL.L   #2,D2
    ADD.L   #IDENTHASH+4,D2
    MOVE.L  D2,A3
.SL:    MOVE.L  -(A3),A3
    MOVE.L  A3,D2
    BEQ.S   .NOTF
    CMP.B   #LAB,4(A3)
    BNE.S   .SL
    MOVE.L  (A3),A1
    MOVE.L  D0,A6
.CL:    CMPM.B  (A1)+,(A6)+
    BNE.S   .SL
    TST.B   -1(A1)
    BNE.S   .CL
    MOVE.L  10(A0),A1
    MOVE.L  6(A3),D7
    BEQ.S   .SK
    MOVE.L  D7,A6
    MOVE.W  (A1),D7
    CMP.W   (A6),D7
    BNE.S   .SL
.SK:    ADDQ.L  #2,A1
    MOVE.W  (A1)+,D3
    BEQ.W   .XL
    SUBQ.W  #1,D3
    MOVEQ   #0,D2
    MOVE.W  10(A3),D2
    LSL.L   #2,D2
    MOVE.L  0(A2,D2.L),D2
    MOVE.L  ACODE,D7
    SUB.L   D7,D2

.PPL:   MOVE.L  (A1)+,A6
    ADD.L   D1,A6

    MOVEM.L D2/A0,-(A7)
    MOVE.L  A6,A0
    ADD.L   D7,A0
    ADD.L   D7,D2
    BSR     FINDNFIX
    MOVEM.L (A7)+,D2/A0
    DBRA    D3,.PPL
    BRA.W   .XL

.NOTF:  MOVE.L  D0,ERROROBJ
    BRA ERROR57

BUILDSYMHUNK:         ; GETS CODE IN A4
    MOVEM.L D0-D7/A0-A3/A5/A6,-(A7)
    BTST    #5,CODEPREFS+2
    BEQ NOSYM
    TSTMOD
    BNE NOSYM
    MOVE.L  A4,SYMADDR
    MOVE.L  LABM+8,A0       ; A0=LABELBUF
    MOVE.L  ACODE,D3        ; D3=ACODE
    MOVEQ   #0,D0           ; D0=NUMSYMS
    MOVE.L  #IDENTHASH+4,D6
    MOVE.L  #IDENTHASH+1028,D7  ; D6,D7=IDENTBUF,END
.OLOOP: MOVE.L  D6,A5           ; A5=IDENTPTR
    ADDQ.L  #4,D6
    CMP.L   D6,D7
    BEQ ESYMS
.LOOP:  MOVE.L  -(A5),A5
    MOVE.L  A5,D4           ; D4=DUMMY
    BEQ.S   .OLOOP
    CMP.B   #LAB,4(A5)
    BNE.S   .LOOP
    BTST    #4,5(A5)
    BNE.S   .LOOP           ; SKIP METHODS
    ;TST.L  6(A5)
    ;BEQ.S  .LOOP           ; skip labels yesorno?
    TST.L   D0          ; FIRST SYM, START HUNK
    BNE.S   .NF
    MOVE.L  #$3F0,(A4)+
.NF:CLR.L   (A4)+
    MOVE.L  A4,A6           ; A6=BACKPATCH
    MOVE.L  (A5),A2         ; A2=ASCII
.CL:MOVE.B  (A2)+,(A4)+
    BNE.S   .CL
    MOVE.L  A4,D4
    BTST    #0,D4
    BEQ.S   .1
    ADDQ.L  #1,D4
.1: BTST    #1,D4
    BEQ.S   .2
    ADDQ.L  #2,D4
.2: MOVE.L  D4,A4
    SUB.L   A6,D4
    LSR.L   #2,D4
    MOVE.W  D4,-2(A6)
    MOVEQ   #0,D1
    MOVE.W  10(A5),D1       ; LABEL-ID
    LSL.L   #2,D1
    MOVE.L  0(A0,D1.L),D1
    SUB.L   D3,D1
    MOVE.L  D1,(A4)+
    ADDQ.L  #1,D0
    BRA .LOOP
ESYMS:
    MOVE.L  #EFUNCBYTE,A3
    MOVE.W  #NREFUNC,D2
    SUBQ.W  #1,D2
    MOVE.L  #EFUNCTAB,D7
    MOVEQ   #0,D6
.3: TST.B   (A3)+
    BEQ.S   .4

    MOVE.L  D6,D5
    LSL.W   #1,D5
    LEA     EFUNCFLAGSTAB,A1
    MOVE.W  0(A1,D5),D5
    BTST    #8,D5
    BNE     .4

    MOVE.L  D7,A1
    MOVE.L  d6,D5
    MULU    #EFUNCENTRYSIZE,D5
    ADD.L   D5,A1           ; (a1)=ptr ascii

    TST.L   D0          ; FIRST SYM, START HUNK
    BNE.S   .NF
    MOVE.L  #$3F0,(A4)+
.NF:CLR.L   (A4)+
    MOVE.L  A4,A6           ; A6=BACKPATCH
    MOVE.L  (A1),A2         ; A2=ASCII
.CL:MOVE.B  (A2)+,(A4)+
    BNE.S   .CL
    MOVE.L  A4,D4
    BTST    #0,D4
    BEQ.S   .1
    ADDQ.L  #1,D4
.1: BTST    #1,D4
    BEQ.S   .2
    ADDQ.L  #2,D4
.2: MOVE.L  D4,A4
    SUB.L   A6,D4
    LSR.L   #2,D4
    MOVE.W  D4,-2(A6)
    MOVE.L  D6,D1
    ADD.L   #10,D1
    LSL.L   #2,D1
    MOVE.L  0(A0,D1.L),D1
    SUB.L   D3,D1
    MOVE.L  D1,(A4)+

.4: ADDQ.L  #1,D6
    DBRA    D2,.3

    TST.L   D0
    SNE     D0
    EXT.W   D0
    EXT.L   D0
    AND.L   D0,SYMADDR
    TST.L   D0
    BEQ.S   NOSYM
    CLR.L   (A4)+
    MOVE.L  A4,SYMEND
NOSYM:  MOVEM.L (A7)+,D0-D7/A0-A3/A5/A6
    RTS



;*-*

