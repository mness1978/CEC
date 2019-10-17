;; COMPILER
; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The COMPILER Part !!!!!!                ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;


; compile main vars

COMPILEPROC:    DC.L    0
PROCMASK:   DC.W    0   ; ONE .L
PROCMASKREV:    DC.W    0   ;
SCOPE:      DC.W    0   ; <>0 --> LOCAL
CURLABNAME: DC.L    FOFF
LABPRES:    DC.L    0
LABPRES2:   DC.L    0
VAROFFSET:  DC.W    0
VARNO:      DC.W    0
PROCF:      DC.W    0
FLAG:       DC.W    0
CURIDENTREC:    DC.L    0   ; IDENTPTR STRUCT
NRLOC:      DC.W    0   ; LINK
NRARG:      DC.W    0   ; RTD
TYPELAB:    DC.W    0   ; 0-3
COMPMESSY:  DC.B    'parsing and compiling ...',10
ENDCOMPMESSY:   EVEN

PASS1:

    MOVE.W  #-1,EXPORTFLAG
    BTST    #6,CODEPREFS+2
    BNE.S   .EA
    CLR.W   EXPORTFLAG
.EA:    MOVE.W  #9,CURSPOT
    BTST    #7,CODEPREFS+2
    BNE.S   .1
    MOVE.L  #COMPMESSY,D2
    MOVE.L  #ENDCOMPMESSY-COMPMESSY,D3
    BSR WRITECON
.1: CLR.W   LINEWRITE
    CLR.W   LINENUM
    LEA CURINTERIM(PC),A0
    MOVE.L  8(A0),(A0)
    MOVE.L  CURINTERIM(PC),A0
COMPILELINE:
    MOVE.L  CURACODE(PC),A4
    MOVE.L  ESTACK(PC),A5
    MOVE.L  CURINTERIM(PC),A3
    MOVEQ   #5,D0
    BSR ADDLABEL
    MOVE.W  #2,ERRWHERE
COMPILELOOP:
    BSR WRITELINENUM        ; TRASHES D7

    MOVE.W  (A3)+,D7
    CMP.W   #-1,D7
    BEQ.S   COMPILEOUTS
    CMP.W   #21,D7
    BNE ERROR12
    MOVE.W  (A3)+,D7
    MOVE.L  A3,INTERMED
    TST.L   LINEBUF
    BEQ.S   .1
    CMP.W   LINENUM(PC),D7
    BEQ.S   .1
    MOVE.W  D7,LINENUM
    JSR ADDLINEDBG
.1: MOVE.W  D7,LINENUM
    MOVE.W  (A3)+,D7
    BSR DOINSMAIN
    MOVE.L  A4,CURACODE
    JSR CHECK3
    BRA.S   COMPILELOOP
COMPILEOUTS:
    TST.W   PROCF
    BNE ERROR9
    MOVE.L  A5,ESTACK
    MOVE.L  A3,CURINTERIM
    MOVE.W  #-1,LINENUM
    BSR LINKDATA
    MOVE.L  A4,CURACODE
    RTS

LINKDATA:
    LEA STRINGLINK,A6
.XL:MOVE.L  (A6),A6
    MOVE.L  A6,D0
    BEQ.S   .X
    MOVEQ   #0,D0
    MOVE.W  4(A6),D0
    BSR ADDLABEL

    LEA 6(A6),A5
.L2:    MOVE.L  (A5)+,A3
    MOVE.W  -2(A3),D0
    MOVE.L  A4,A0
    LSL.W   #1,D0
    ADD.W   D0,A0
    MOVE.L  A0,CURACODE
    JSR CHECK3
    MOVE.W  -4(A3),D0
    BEQ.S   .NC
    SUBQ.W  #1,D0
.CL:    MOVE.B  (A3)+,(A4)+
    DBRA    D0,.CL          ; WAS A BNE LOOP!
.NC:    MOVE.L  (A5),D7
    MOVE.L  D7,A5
    BNE.S   .L2
    CLR.B   (A4)+

    BRA.S   .XL
.X: MOVE.L  A4,D0
    BTST    #0,D0
    BEQ.S   .1
    CLR.B   (A4)+
.1: RTS


DOINSMAIN:            ; FIRST INTERIM IN D7
    CMP.W   #IOFF+59,D7
    BEQ.S   .1
.2: BSR DOINS
    TST.W   (A3)+
    BNE ERROR12
.3: BTST    #6,CODEPREFS+2
    BEQ.S   .EA
    RTS
.EA:    CLR.W   EXPORTFLAG
    RTS
.1: MOVE.W  #-1,EXPORTFLAG
    MOVE.W  (A3)+,D7
    BEQ.S   .3
    BRA.S   .2

EXPORTFLAG:   DC.W    0
FLTFLAG:      DC.W    0
DEFFLAG:      DC.W    0
HANDLEFLAG:   DC.W    0

DOINS:
    TST.W   SCOPE
    BEQ.S   .2
    TST.W   DEFFLAG
    BNE.S   .DH
.DHB:
    CMP.W   #$100,D7        ; in procs part
    BPL DOASM
    CMP.W   #48,D7
    BEQ PPCDOASM
    CMP.W   #IDENT,D7       ; BECOMES+LABEL:
    BEQ DOLAB
    CMP.W   #32,D7
    BEQ PTRCOMPLEX
    BTST    #3,CODEPREFS+3
    BEQ.S   .1
    CMP.W   #IOFF+20,D7
    BMI ERROR32
    CMP.W   #IOFF+23,D7
    BPL ERROR32
.1: CMP.W   #IOFF,D7
    BPL DOJOB
    BRA DOEXP
.2: CMP.W   #IOFF+16,D7     ; in global part
    BEQ DOJOB
    CMP.W   #IOFF,D7
    BEQ DOJOB
    CMP.W   #IOFF+48,D7
    BEQ DOJOB
    BRA ERROR35
.DH:    CMP.W   #IOFF+17,D7
    BEQ.S   .DHB
    CLR.W   DEFFLAG         ; FIRST STATEMENT AFTER DEF'S
    TST.W   HANDLEFLAG
    BEQ .DHB
    BSR HANDLEPROCREALLY
    BRA .DHB
DOINSOUT:
    RTS

DOEXP:
    TST.W   PROCF
    BNE.S   .1
    BSET    #6,WARNINGS+3       ; CODE OUTSIDE PROCS
.1: MOVE.W  #1,EXPSTAT
    MOVE.W  D7,-(A3)
    BSR EXP
    CLR.W   EXPSTAT
    BRA.S   DOINSOUT

EXPSTAT:  DC.W    0

DOASM:
    BSR ASM_COMPILE     ; WITH D7
    BRA.S   DOINSOUT

PPCDOASM:
    BSR PASM_COMPILE
    BRA     DOINSOUT

DOJOB:
    BSR.S   DOKEYWORD
    BRA DOINSOUT

STOPJOB:              ; END JOBCODE PROCESSING
    LEA .1(PC),A2
    RTS
.1: DC.W    0

DOKEYWORD:
    SUB.W   #IOFF,D7
    ASL.W   #2,D7
    LEA INSJOBTAB,A2
    MOVE.L  0(A2,D7.W),A2
JOBLOOP:
    MOVE.W  (A2)+,D7
    TST.W   D7
    BEQ.S   JOBOUT
    ASL.W   #2,D7
    LEA JOBROUTTAB(PC),A0
    MOVE.L  (A0,D7.W),A0
    JMP (A0)
JOBOUT:
    RTS


DOBECOMESMETHOD:
    CMP.W   #42,6(A3)
    BEQ DOEXP
DOBECOMES:
    MOVE.L  (A3)+,A1
    MOVE.L  A1,EAIDENT
    CMP.W   #ASSGN,(A3)
    BNE COMPLEXBECOMES
    MOVE.B  4(A1),D2
    BEQ ERROR22
    CMP.B   #LAB,D2
    BEQ ERROR6
    ADDQ.L  #2,A3

    move.l  a4,-(a7)
    MOVE.W  #17,EAREQUEST
    BSR EAEXP
    move.l  (a7)+,a2            ; a2 now - output code before expression
    TST.L   D0
    BNE.W   .OPT
    BTST    #2,ICODEPREFS+3
    BNE     .SKdupa

    TST.L   LAST_CMD_ADR        ; see if we can optimize
    beq     .SKdupa
    move.l  LAST_CMD_ADR,a0
    move.l  a4,d0
    sub.l   a0,d0
    cmp.l   #4,d0
    bne     .SK1
    btst    #1,CODEPREFS
    beq     .SK1
    cmp.l   #$F2006400,(a0)
    bne     .SK1
    subq.l  #4,a4
    move.l  a4,d0
    sub.l   a2,d0
    cmp.l   #8,d0
    beq     .OPTI8
.O8B:
    move.w  .F1,(a4)+
    move.l  a4,d1
    jsr     GVA1D0_0
    cmp.l   a4,d1
    beq     .SK0
    move.w  -(a4),d0
    move.w  .F1+2(pc),(a4)+
    move.w  d0,(a4)+
    bra     .OPT
.SK0:
    move.w  .F1+2(PC),(A4)+
    bra     .OPT
.SK1:
    cmp.w   #6,d0
    bne     .SK2
    cmp.w   #$203c,(a0)
    bne     .SK2
    move.l  a0,a4
    move.l  2(a4),-(a7)
    move.w  #$2b7c,(a4)+
    move.l  a4,a2
    JSR     GVA1D2_9
    moveq   #0,d2
    cmp.l   a4,a2
    beq     .SK1S
    move.w  -(a4),d2
    move.l  (a7)+,(a4)+
    move.w  d2,(a4)+
    bra     .OPT
.SK1S:
    move.l  (a7)+,(a4)+
    bra     .OPT


.SK2:
.SKdupa:
    MOVE.W  .1(PC),(A4)+
    JSR GVA1D2_9
.OPT:   BRA DOINSOUT
.1: MOVE.L  D0,2(A5)
.F1:fmove.s fp0,4(a5)
.F2:fsmove.s #0,fp0
.F3:fmove.s #0,fp0
.2: move.l  #0,2(a5)
.OPTI8:
    MOVE.L  .F2,d0
    cmp.l   (a2),d0
    bne     .O82
    bra     .O8
.O82:
    MOVE.L  .F3,d0
    cmp.l   (a2),d0
    bne     .O8B
.O8:
    subq.l  #8,a4
    move.l  4(a4),d0
    move.w  .2,(a4)+
    move.l  a4,a2
    jsr     GVA1D2_9
    cmp.l   a4,a2
    bne     .O8X
    move.l  d0,(a4)+
    bra     .OPT
.O8X:
    move.w  -(A4),d2
    move.l  d0,(a4)+
    move.w  d2,(a4)+
    bra     .OPT
;COMPLEXEXP:
;    move.l  (a3)+,a1
;    move.l  a1,EAIDENT
;    move.w  (a3)+,-(a7)
;    addq.l  #2,a3
;    BSR EXP
;    move.w  (a7)+,d0
;    subq.w  #7,d0
;    lsl.w   #2,d0
;    move.l  EAIDENT(PC),A1
;    lea     .C0(PC),A0
;    move.w  0(a0,d0),(a4)+
;    jsr GVA1D2_0
;    bra DOINSOUT
;C0:add.l   d0,4(a5)
;    sub.l   d0,4(a5)
;


PTRCOMPLEX:           ; uses a0,d0
    SUBQ.L  #2,A3
    MOVE.L  A3,A0
    BRA.S   COMPLEXST
COMPLEXBECOMES:
    LEA -6(A3),A0
COMPLEXST:
    MOVE.L  A0,-(A7)        ; complexstart -->STACK (D7)
    BSR EATLEXP
    BSR EXP
    MOVE.L  (A7)+,D7
    MOVE.L  A3,-(A7)        ; complexend -->STACK
    MOVE.L  D7,A3
    MOVEQ   #2,D0
    BSR EADDRESSMODI
    CMP.W   #ASSGN,(A3)+
    BNE ERROR2
    MOVE.L  (A7)+,A3
    BRA DOINSOUT


NOASSIGN:
    MOVE.L  A0,A3           ; FROM COMPLEXST
    BSR EXP
    BRA DOINSOUT

EATLEXP:              ; uses d0
    MOVE.W  (A3)+,D0
    BEQ .3
    CMP.W   #IDENT,D0
    BEQ.S   .1
    CMP.W   #ASSGN,D0
    BEQ.S   .2
    CMP.W   #39,D0
    BEQ.S   .1
    CMP.W   #31,D0
    BEQ.S   .1
    CMP.W   #11,D0
    BEQ.S   .4
    CMP.W   #32,D0
    BMI.S   .XB
    CMP.W   #36,D0
    BMI.S   EATLEXP
.XB:CMP.W   #41,D0
    BEQ.S   EATLEXP
    CMP.W   #42,D0
    BEQ.S   .MET
    CMP.W   #29,D0
    BNE.S   .3          ; was ERROR0!!!
    ADD.L   (A3),A3
    BRA.S   EATLEXP
.2: RTS
.1: ADDQ.L  #4,A3
    BRA.S   EATLEXP
.4: BSET    #3,WARNINGS+3       ; a.complex=1 a statement
    MOVE.W  LINENUM,ASSLINE
.3: ADDQ.L  #8,A7
    BRA.S   NOASSIGN
.MET:   ADDQ.L  #4,A7
    MOVE.L  (A7)+,A3
    ADDQ.L  #2,A3
    BRA DOEXP

DOMULTIPLE:
    SUBQ.L  #2,A3
    MOVE.L  A3,-(A7)
.XL:CMP.W   #IDENT,(A3)+        ; SKIP VARS
    BNE ERROR6
    ADDQ.L  #4,A3
    CMP.W   #COM,(A3)+
    BEQ.S   .XL
    CMP.W   #ASSGN,-2(A3)
    BNE ERROR2
    BSR EXP
    MOVE.L  (A7)+,D0
    MOVE.L  A3,-(A7)
    MOVE.L  D0,A3
    MOVEQ   #0,D0           ; REGISTER COUNT
.L2:    CMP.W   #8,D0           ; NO MORE THAN 8 VARS
    BEQ ERROR0
    ADDQ.L  #2,A3           ; FILL VARS WITH D0..DX
    MOVE.L  (A3)+,A0
    LEA .1(PC),A1
    MOVE.B  4(A0),D2
    BEQ ERROR22
    CMP.B   #LAB,D2
    BEQ ERROR6
    MOVE.W  (A1),D1
    OR.W    D0,D1
    MOVE.W  D1,(A4)+
    JSR GVA0D7_9
    ADDQ.L  #1,D0
    CMP.W   #COM,(A3)+
    BEQ .L2
    MOVE.L  (A7)+,A3
    BRA DOINSOUT
.1: MOVE.L  D0,2(A5)


DOLAB:
    BTST    #3,CODEPREFS+3
    BNE.S   .1
.2: ;CMP.l   #$7000b,4(a3)
;    beq COMPLEXEXP
;    cmp.l   #$8000b,4(a3)
;    beq COMPLEXEXP
    MOVE.W  4(A3),D0
    CMP.W   #ASSGN,D0
    BEQ DOBECOMES
    CMP.W   #29,D0
    BEQ DOBECOMES
    CMP.W   #35,D0
    BEQ DOBECOMESMETHOD
    CMP.W   #41,D0
    BEQ DOBECOMES
    CMP.W   #COM,D0
    BEQ DOMULTIPLE
    CMP.W   #11,D0
    BNE.S   .3
    BSET    #3,WARNINGS+3       ; a=1 a statement
    MOVE.W  LINENUM,ASSLINE
.3: CMP.W   #19,D0
    BNE DOEXP
    SUBQ.L  #2,A3
    BSR CHLAB
    TST.W   (A3)+
    TST.W   (A3)            ; INTR. FOLLOWS OR WHAT?
    BEQ DOINSOUT
    BSR DOINSREC
    BRA DOINSOUT
.1: CMP.W   #19,4(A3)
    BNE ERROR32
    BRA     .2
;*-*
;;COMPILE


;   NOTEZBIEN:  DEZE MOGEN NIET MEER REGISTERS VERSTOREN!
;; ADD LABEL
ADDLABEL:             ; D0 LABEL (A4 IS ADR. LABEL)
    MOVE.L  LABM+8(PC),A0       ; THRASHES A0/D0!
    LSL.L   #2,D0
    AND.L   #$3FFFF,D0
    MOVE.L  A4,0(A0,D0.L)
    RTS
;*-*
;; ADD BRANCH
ADDBRANCH:            ; D0 LABEL (A4 IS ADR. NA OP.)
    MOVE.L  A4,D1
    SUBQ.L  #2,D1           ; +TRASH: D1/A0, -TRASH: D0 !

    TST.W   NEWOP
    BEQ.S   .NL
    BTST    #1,ICODEPREFS+3
    BNE     .1
    BTST    #2,CODEPREFS+3
    BEQ.S   .NL
.1:
    TST.W   NEWOP020
    BEQ     .LONG
    CMP.W   #1,ECPU
    BMI     .LONG
    subq.l  #4,a4
    move.w  NEWOP020,(a4)+
    CLR.W   NEWOP020
    CLR.W   NEWOP
    BRA     ADDBRANCHPCREL32
.LONG:
    MOVE.L  #$30000,-2(A4)
    ADDQ.L  #2,A4
    MOVE.W  D7,-(A7)
    MOVE.W  NEWOP,D7
    BEQ.S   .NNO
    MOVE.W  D7,-6(A4)
.NNO:   MOVE.W  (A7)+,D7
.NL:
    GETM    A0
    MOVE.L  BRANCHLIST,(A0)
    MOVE.L  A0,BRANCHLIST
    ADDQ.L  #4,A0
    MOVE.L  D1,(A0)+
    MOVE.W  D0,(A0)+
    CLR.W   (A0)+
    DONEM   A0
    CLR.W   NEWOP
    RTS
;*-*
;; ADD BRANCH RELOC
ADDBRANCHRELOC:           ; D0 LABEL (A4 IS ADR. VOOR REL.LONG.)
    MOVE.L  A4,D1           ; +TRASH: D1/A0, -TRASH: D0 !
    MOVE.L  #$30000,(A4)+
    GETM    A0
    MOVE.L  BRANCHLIST(PC),(A0)
    MOVE.L  A0,BRANCHLIST
    ADDQ.L  #4,A0
    MOVE.L  D1,(A0)+
    MOVE.W  D0,(A0)+
    CLR.W   (A0)+
    DONEM   A0
    RTS
;*-*
;; FORCE ABSOLUTE
FORCEABSHERE:
    MOVE.L  A4,D1           ; +TRASH: D1/A0, -TRASH: D0 !
    MOVE.L  #$B0000,(A4)+
    GETM    A0
    MOVE.L  BRANCHLIST(PC),(A0)
    MOVE.L  A0,BRANCHLIST
    ADDQ.L  #4,A0
    MOVE.L  D1,(A0)+
    move.w  #1,(a0)+
    MOVE.L  D0,(A0)+
    DONEM   A0
    RTS
;*-*
;; ADD BRANCH PCREL 32
ADDBRANCHPCREL32:
    MOVE.L  A4,D1
    MOVE.L  #$90000,(A4)+
    GETM    A0
    MOVE.L  BRANCHLIST,(A0)
    MOVE.L  A0,BRANCHLIST
    ADDQ.L  #4,A0
    MOVE.L  D1,(A0)+
    MOVE.W  D0,(A0)+
    CLR.W   (A0)+
    DONEM   A0
    RTS
;*-*
;; ADD BRANCH PCREL 16 EXTENDED
ADDBRANCHPCREL16E:
    MOVE.L  A4,D1
    CLR.W   (A4)+
    GETM    A0
    MOVE.L  BRANCHLIST(PC),(A0)
    MOVE.L  A0,BRANCHLIST
    ADDQ.L  #4,A0
    MOVE.L  D1,(A0)+
    MOVE.W  D0,(A0)+
    MOVE.W  #1,(A0)+
    DONEM   A0
    RTS
;*-*
;; ADD BRANCH PCREL 32 EXTENDED
ADDBRANCHPCREL32E:
    MOVE.L  A4,D1
    MOVE.L  #$90000,(A4)+
    GETM    A0
    MOVE.L  BRANCHLIST(PC),(A0)
    MOVE.L  A0,BRANCHLIST
    ADDQ.L  #4,A0
    MOVE.L  D1,(A0)+
    MOVE.W  D0,(A0)+
    MOVE.W  #1,(A0)+
    DONEM   A0
    RTS
;*-*
;; ADD LIB BRANCH
PUTLIBBRANCH:
    MOVE.L  A0,-(A7)
    GETM    A0
    MOVE.L  LIBPTRS(PC),(A0)+
    MOVE.L  A0,LIBPTRS
    MOVE.L  A4,(A0)+
    MOVE.L  A6,(A0)+
    BTST    #2,CODEPREFS+3
    BNE     .LARGE
    MOVE.W  #0,(A0)+
    MOVE.W  #$6100,(A4)+
    MOVE.W  #$0,(A4)+
    BRA .EXIT
.LARGE:
    MOVE.W  #1,(A0)+
    MOVE.W  #$4eb9,(a4)+
    move.l  #$0,(a4)+
.EXIT:
    DONEM   A0
    MOVE.L  (A7)+,A0
    RTS
;*-*
;; NEW LABEL
NEWLABEL:             ; => D0 LAB
    MOVE.L  CURLABNAME(PC),D0
    ADDQ.L  #1,CURLABNAME
    BSR CHECKLABBUF
    RTS
;*-*

;; central important vars:
;------------------------

CURINTERIM: DC.L 0          ; [1] ADR. OF CURRENT INTERIM
TOKENEND:   DC.L 0,0,0,10000,100*KB
CURLIBASC:
CURACODE:   DC.L 0,0        ; [3] ADR. OF CURRENT OBJPART
ACODE:      DC.L 0          ;     ADR. OF OBJECTCODE
        DC.L 0,MAXSTACK+1000,MINIMUM_ACODE
HEAP:       DC.L 0          ; [5] ADR. OF CURRENT HEAPSPACE
        DC.L 0,0,0,2000,10*KB
LABM:       DC.L 0,0,0,0,5000,15*KB ; [6]



ECODE:      DC.L EXAMPLE        ; ADR. OF E-CODE
CURECODE:   DC.L EXAMPLE        ; ADR. OF CURRENT ELINE
ENDECODE:   DC.L EXAMPLEEND
EBUF:       DC.L 0          ; <>0 --> DEALLOC
EBUFSIZE:   DC.L 0
GENERALSIZE:    DC.L 0

INITSTACK:  DC.L 0
ESTACK:     DC.L ESTACKBUF      ; ADR. OF COMPILESTACK
WORK:       DC.L WORKBUF        ; ADR. OF WORKBUFFER
DIRNAMEX:   DC.L DIRNAME
MODLEFT:    DC.L 0

LINENUM:    DC.W -1
LINENUM2:   DC.W 0
LINENUMZ:   DC.L 0
ERROROBJ:   DC.L 0          ; IDENTNAME THAT CAUSED ERROR
REALBRANCH: DC.L 0          ; OFFSET ADR
STARTINTERIM:   DC.L 0

LOADEDVERSION:  DC.W 0

CHECKHEAP:  DC.W 10
OPERSIZE:   DC.W 4          ; MUST BE KEPT AT 4 (FOR GLOBALS)

LIBINFO:    DC.L    0

;----------------------------
;*-*
;; VARS/CODE

; following code has HEAP in use!!!!!

DBLIST: DC.L    0       ; linked
DBTAIL: DC.L    0
DBBEG:  DC.L    0
DBCUR:  DC.L    0       ; =end after one, ISDEBUG FLAG
NEWOP:    DC.W    0     ; 0=NO32, X=NEWOP
NEWOP020: DC.W  0       ; 0=NoBcc.L, X=NEWOP
BRANCHLIST:   DC.L    0
INITCODE:   BSR GLOBSTACK
PROCRTS:    RTS
GLOBSTACK:  DC.L    0   ; TOTAL GLOBAL
XTRASTACK:  DC.L    0
MINSTACK:   DC.L    0   ; ALL LOCAL ADDED
CLEARXTRA:  ADD.L   #40,A7
PROCEND:    DC.L    0
PROCSTARTADR:   DC.L    0
;*-*
;; MACROS
MAXINT=$7FF0

LDEF:   MACRO
    TSTMOD
    BEQ.S   *+12
    CMP.W   #3,-2(A2)
    BEQ ERROR48
    ENDM
;*-*

;; OPTCACHELINE
OPTCACHELINE:               ; D0 LABEL (A4 IS ADR. LABEL)
    cmp.w   #1,ECPU
    bmi     .DO
    btst    #1,ICODEPREFS+3
    BEQ     .DO
    move.l  d1,-(a7)
    move.l  a4,d1
    sub.l   ACODE,d1
    and.l   #7,d1           ; boundaries!!
    cmp.w   #2,d1
    bne     .1
    move.w  #$51fb,(a4)+
    clr.l   (a4)+
    bra     .DO1
.1: cmp.w   #4,d1
    bne     .2
    move.l  #$51fa0000,(a4)+
    bra     .DO1
.2: cmp.w   #6,d1
    bne     .DO1
    move.w  #$51fc,(a4)+
.DO1:
    move.l  (a7)+,d1
.DO:
    RTS
;*-*

;; "1"
GETPARAMLABEL:            ; 1
    CMP.W   #IDENT,(A3)+
    BNE ERROR4
    MOVE.L  (A3)+,A0
    MOVE.W  10(A0),LABPRES+2
    CMP.B   #LAB,4(A0)
    BNE ERROR4
    BTST    #4,5(A0)
    BNE ERROR4
    BRA JOBLOOP
;*-*
;; "2"
JOBEXP:               ; 2
    BSR EXP
    BRA JOBLOOP
;*-*
;; "3"
GETPARAMVAR:          ; 3
    MOVE.W  (A3)+,D6
    CMP.W   #IDENT,D6
    BNE ERROR6
    MOVE.L  (A3)+,A0
    MOVE.L  A0,CURIDENTREC
    MOVE.W  10(A0),VAROFFSET
    MOVE.B  4(A0),D0
    BEQ ERROR22
    MOVE.W  D0,TYPELAB
    CMP.B   #LAB,D0
    BEQ ERROR6
    BRA JOBLOOP
;*-*
;; "4"
GETCOMMA:             ; 4
    MOVE.W  (A3)+,D6
    CMP.B   #COM,D6
    BNE ERROR5
    BRA JOBLOOP
;*-*
;; "5"
SAVESTIDENT:          ; 5
    MOVE.W  (A2)+,(A5)+
    BRA JOBLOOP
;*-*
;; "6"
FORWARDREFERENCE:         ; 6
    BSR     NEWLABEL
    MOVE.L  D0,(A5)+
    MOVE.L  D0,LABPRES
    BRA JOBLOOP
;*-*
;; "7"
CHECKSTIDENT:         ; 7
    MOVE.W  -(A5),D0
    CMP.W   (A2)+,D0
    BNE ERROR9
    BRA JOBLOOP
;*-*
;; "8"
MAKEBRANCH:           ; 8
    MOVE.L  LABPRES,D0
    CLR.W   NEWOP
    BSR ADDBRANCH
    BRA JOBLOOP
;*-*
;; "9"
COPY2:                ; 9
    MOVE.W  (A2)+,(A4)+
    BRA JOBLOOP
;*-*
;; 10
COPY4:                ; 10
    MOVE.L  (A2)+,(A4)+
    BRA JOBLOOP
;*-*
;; 11
COPYN:                ; 11
    MOVE.W  (A2)+,D0
COPYNLOOP:
    MOVE.W  (A2)+,(A4)+
    DBRA    D0,COPYNLOOP
    BRA JOBLOOP

;*-*
;; 12
COPY6:                ; 12
    MOVE.L  (A2)+,(A4)+
    MOVE.W  (A2)+,(A4)+
    BRA JOBLOOP
;*-*
;; 13
COPY8:                ; 13
    MOVE.L  (A2)+,(A4)+
    MOVE.L  (A2)+,(A4)+
    BRA JOBLOOP
;*-*
;; 14
COPY10:               ; 14
    MOVE.L  (A2)+,(A4)+
    MOVE.L  (A2)+,(A4)+
    MOVE.W  (A2)+,(A4)+
    BRA JOBLOOP
;*-*
;; 15
REMEMBERLAB:          ; 15
    MOVE.L  -(A5),D0
    BSR ADDLABEL
    BRA JOBLOOP
;*-*
;; 16               EXP/NIL
EXPORNILL:            ; 16
    TST.W   (A3)
    BEQ.S   MAKEMOVEQ
    CMP.W   #$400,(A3)
    BEQ MAKED0
    CMP.W   #IOFF+18,(A3)
    BEQ.S   MAKEMOVEQ
    BSR EXP
    CMP.W   #COM,(A3)
    BNE JOBLOOP
    CLR.W   .C
.XL:MOVE.W  .P(PC),(A4)+
    ADDQ.L  #2,A3
    BSR EXP
    ADDQ.W  #1,.C
    CMP.W   #COM,(A3)
    BEQ.S   .XL
    MOVE.W  .C(PC),D0
    CMP.W   #MAXMULTRET,D0
    BPL ERROR0
    MOVE.W  .M(PC),D1
    MOVE.W  D0,D2
    MOVEQ   #9,D3
    LSL.W   D3,D2
    OR.W    D2,D1
    MOVE.W  D1,(A4)+
.L2:    SUBQ.W  #1,D0
    MOVE.W  .R(PC),D1
    MOVE.W  D0,D2
    LSL.W   D3,D2
    OR.W    D2,D1
    MOVE.W  D1,(A4)+
    TST.W   D0
    BNE.S   .L2
    BRA JOBLOOP
.P: MOVE.L  D0,-(A7)
.C: DC.W    0
.M: MOVE.L  D0,D0
.R: MOVE.L  (A7)+,D0
MAKEMOVEQ:
    MOVE.W  MOVEQ(PC),(A4)+
    BRA JOBLOOP
MAKED0:
    ADDQ.L  #2,A3
    BRA JOBLOOP
;*-*
;; 17               Procedure
PROCFLAGON:           ; 17
;    BTST    #0,CODEPREFS
;    BEQ     .XXX
;    MOVE.L  #$4e880020,(a4)+
;.XXX:
    CLR.L   FIXUPSTACKREGS
    TST.W   PROCF
    BNE ERROR9
    BCLR    #3,ICODEPREFS+3
    BSR.W   NEWLABEL
    MOVE.L  D0,PROCEND
    MOVE.L  A4,PROCSTARTADR
    MOVEQ   #-1,D1
    MOVE.W  D1,PROCF
    MOVE.W  D1,DEFFLAG
    CLR.W   HANDLEFLAG
    MOVE.L  CURIDENTREC(PC),A0  ; NOW GET NR OF LOC VARS
    MOVE.L  6(A0),A0
    MOVE.L  A0,COMPILEPROC
    CLR.L   PROCMASK        ; + REV
    MOVE.W  4(A0),NRLOC
    BTST    #0,2(A0)
    BEQ.S   .1
    MOVE.W  (A0),NRARG
    BRA.S   .2
.1: CLR.W   NRARG
.2: CLR.L   XTRASTACK
.3: BRA JOBLOOP
;*-*
;; 18               ENDPROC
PROCFLAGOFF:          ; 18
    TST.W   PROCF
    BEQ ERROR9
    MOVE.L  PROCEND(PC),D0
    BSR ADDLABEL
    MOVE.L  A4,D0
    SUB.L   PROCSTARTADR(PC),D0
    CMP.L   #32000,D0
    BPL ERROR46
    CLR.W   PROCF
    MOVE.L  XTRASTACK(PC),D0
    ADD.L   D0,MINSTACK

    TST.L   D0
    BEQ.S   .2
    MOVE.W  CLEARXTRA(PC),(A4)+
    MOVE.L  D0,(A4)+
.2:
    TST.W   PROCMASKREV
    BEQ.S   .1
    RESTR   PROCMASKREV
.1:
    TST.W   HANDLEFLAG
    BEQ.S   .1B
    MOVE.L  .3(PC),(A4)+
.1B:
    BRA JOBLOOP
.3: MOVEM.L (A7)+,D3-D7
;*-*
;; 19
CHECK2LAB:            ; 19
    BSR.S   CHLAB
    BRA JOBLOOP

CHLAB:
    MOVE.W  (A3)+,D6
    CMP.W   #IDENT,D6
    BNE ERROR4
    MOVE.L  (A3)+,D0
    MOVE.L  D0,CURIDENTREC
    MOVE.L  D0,A0
    CMP.B   #LAB,4(A0)
    BNE ERROR4

    TST.W   EXPORTFLAG
    BEQ.S   .1
    BSET    #2,5(A0)        ; EXPORT
.1:
    MOVE.L  6(A0),A1        ; FOR MAIN
    MOVE.L  (A0),A0
    CMP.B   #'m',(A0)
    BEQ.S   MAINFOUND
MAINBACK:
    CMP.L   #FOFF,D0
    BMI.S   .1
    MOVE.L  D0,A1
    MOVEQ   #0,D0
    MOVE.W  10(A1),D0
.1: BSR ADDLABEL
    RTS

MAINF:    DC.W    0

MAINFOUND:
    CMP.B   #'a',1(A0)
    BNE.S   MAINBACK
    CMP.B   #'i',2(A0)
    BNE.S   MAINBACK
    CMP.B   #'n',3(A0)
    BNE.S   MAINBACK
    CMP.B   #0,4(A0)
    BNE.S   MAINBACK
    TST.W   (A1)
    BNE ERROR23
    MOVEQ   #1,D0           ; MAINLABCODE
    TST.W   MAINF
    BNE ERROR14
    MOVE.W  D0,MAINF
    TSTMOD
    BEQ.S   MAINBACK
    BRA ERROR48
;*-*
;; 20
REMINDCURPOS:         ; 20
    BSR     NEWLABEL
    MOVE.L  D0,(A5)+
    BSR ADDLABEL
    BRA JOBLOOP
;*-*
;; 21               POPLAB
POPLAB:               ; 21
    MOVE.L  -(A5),LABPRES
    BRA JOBLOOP
;*-*
;; 22
INSERTVAROFF:         ; 22
    SUBQ.L  #2,A4
    MOVE.L  CURIDENTREC(PC),A0
    JSR GVA0D2_0
    BRA JOBLOOP
;*-*
;; 23               PUSHVAR
PUSHVAR:              ; 23
    MOVE.L  CURIDENTREC(PC),(A5)+
    BRA JOBLOOP
;*-*
;; 24               POPVAR
POPVAR:               ; 24
    MOVEA.L -(A5),A0
    MOVE.L  A0,CURIDENTREC
    MOVE.B  4(A0),TYPELAB
    MOVE.W  10(A0),VAROFFSET
    BRA JOBLOOP
;*-*
;; 25               PUSHLAB
PUSHLAB:              ; 25
    MOVE.L  LABPRES(PC),(A5)+
    BRA JOBLOOP
;*-*
;; 26               PUSHLAB 2
PUSHLAB2:             ; 26
    MOVE.L  LABPRES2(PC),(A5)+
    BRA JOBLOOP
;*-*
;; 27               POPLAB 2
POPLAB2:              ; 27
    MOVE.L  -(A5),LABPRES2
    BRA JOBLOOP
;*-*
;; 28               NEWLAB 2
NEWLAB2:              ; 28
    BSR NEWLABEL
    MOVE.L  D0,(A5)+
    MOVE.L  D0,LABPRES2
    BRA JOBLOOP
;*-*
;; 29               BRANCH
MAKEBRANCH2:          ; 29
    MOVE.L  LABPRES2(PC),D0
    CLR.W   NEWOP
    BSR ADDBRANCH
    BRA JOBLOOP
;*-*
;; 30
SETFLAG:              ; 30
    MOVE.W  #-1,FLAG
    BRA JOBLOOP
;*-*
;; 31
SKIPFLAG:             ; 31
    MOVE.W  (A2)+,D0
    TST.W   FLAG
    BNE.S   SKIPOUT
SKIPLOOP:
    TST.W   (A2)+           ; NOP
    DBRA    D0,SKIPLOOP
SKIPOUT:
    BRA JOBLOOP
;*-*
;; 32
CLEARFLAG:            ; 32
    MOVE.W  #0,FLAG
    BRA JOBLOOP         ; was: "OUT"
;*-*
;; 33               VARS
SKIPDEFLOCAL:         ; 33
    MOVEQ   #0,D6           ; D6=DEFARGS COUNT
    MOVE.W  (A2)+,D5        ; 1=PROC,2=LOCAL,3=DEF

    CLR.L   .DAAR
    CLR.L   .DAAR+4
    CLR.L   .DAAR+8
    CLR.L   .DAAR+12


    CLR.L   DBCUR
    BTST    #6,CODEPREFS+1
    BEQ.W   .NDB            ; NOW __DEBUG__ SPECIFIC
    CMP.W   #1,D5
    BNE.W   .NN

    GETM    A0
    MOVE.L  DBTAIL(PC),D0
    BNE.S   .LA
    MOVE.L  A0,DBLIST
    BRA.S   .LN
.LA:    MOVE.L  D0,A1
    MOVE.L  A0,(A1)
.LN:    MOVE.L  A0,DBTAIL
    MOVE.L  A0,D2
    CLR.L   (A0)+           ; NEXT
    MOVE.W  #4,(A0)+        ; COPY NAME FUN OR METHOD
    MOVE.L  COMPILEPROC(PC),A1
    MOVE.L  A1,D1
    CLR.W   (A0)+
    MOVE.L  A0,A6
    MOVE.L  10(A1),D0
    BEQ.S   .NM
    MOVE.L  D0,A1
    MOVE.L  OASCII(A1),A1
.COL:   MOVE.B  (A1)+,(A0)+
    BNE.S   .COL
    MOVE.B  #":",-1(A0)
.NM:    MOVE.L  D1,A1
    MOVE.L  14(A1),A1
    MOVE.L  (A1),A1
.CIL:   MOVE.B  (A1)+,(A0)+
    BNE.S   .CIL
    MOVE.L  A0,D0
    BTST    #0,D0
    BEQ.S   .NA
    CLR.B   (A0)+
.NA:    MOVE.L  A0,D0
    SUB.L   A6,D0
    MOVE.W  D0,-2(A6)

    MOVE.L  D1,A1           ; THIS BIT ADDS A "5" SELF EXTENSION
    MOVE.L  18(A1),D0
    BEQ.S   .NSELF
    MOVE.L  #$50004,(A0)+
    MOVE.W  LINENUM(PC),(A0)+
    MOVE.L  D0,A1
    MOVE.W  10(A1),(A0)+
.NSELF:
    CLR.W   (A0)+           ; TO BE ABLE TO DETECT A 5
    DONEM   A0

.NN:    GETM    A0          ; START REGULAR VAR DEBUG, 10 BYTES
    MOVE.L  A0,DBBEG
    CLR.L   (A0)+
    MOVE.W  D5,(A0)+
    MOVE.W  LINENUM(PC),(A0)+
    CLR.W   (A0)+
    MOVE.L  A0,DBCUR

.NDB:   TST.W   SCOPE
    BNE.S   .S
    CMP.W   #3,D5           ; must be DEF
    BNE ERROR35
    BRA.S   .XB
.S: CMP.W   #3,D5           ; musn't be DEF (LOCAL)
    BEQ ERROR35
    TST.W   PROCF
    BEQ ERROR35
    tst.w   DEFFLAG
    bne .XB
    cmp.w   #1,-2(a5)
    bne ERROR35
;    TST.W   DEFFLAG
;    BEQ ERROR35         ; NO LOCAL DEFS AFTER 1ST STAT.
.XB:MOVEQ   #0,D0           ; D0=XTRA ARRAY STACKSPACE
    CMP.W   #18,(A3)
    BEQ .BRCL
.XL:CMP.W   #IDENT,(A3)+
    BNE ERROR6
    MOVE.L  (A3)+,A0        ; A0=VAR
    MOVE.W  (A3),D2
    CMP.W   #11,D2          ; =
    BEQ .15
    TST.L   D6
    BNE ERROR30         ; WE WANT DEFARGS TILL THE END
.DAB:   CMP.W   #19,D2          ; :
    BEQ .2
    CMP.W   #29,D2          ; []
    BNE.S   .1
    ADDQ.L  #6,A3           ; ALSO SKIP ]OFFSET
    LDEF
    CMP.W   #VALUE,(A3)+
    BNE ERROR30
    MOVE.L  (A3)+,D2        ; D2=SIZE
    CMP.W   #30,(A3)+
    BNE ERROR34
    CMP.W   #19,(A3)+
    BNE ERROR0
    MOVE.W  (A3)+,D1
    CMP.W   #IOFF+60,D1
    BNE.S   .NAR
    BSR .SETREG
    MOVE.W  (A3)+,D1
.NAR:   CMP.W   #IOFF+41,D1
    BEQ .3
    CMP.W   #IOFF+42,D1
    BEQ .4
    CMP.W   #IOFF+47,D1
    BEQ .LIST
    CMP.W   #IOFF+20,D1
    BEQ .3_1
    CMP.W   #IOFF+21,D1
    BEQ .3_1
    CMP.W   #IOFF+22,D1
    BEQ .3_1
    CMP.W   #31,D1
    BEQ .3_1
    BRA .SK2
    BRA ERROR33
.1:
    MOVE.L  DBCUR(PC),D2        ; ADD DEBUG ADDRESSING MODE
    BEQ.S   .NDBA
    MOVE.L  D2,A6
    MOVE.W  10(A0),D2
    BTST    #3,5(A0)
    BEQ.S   .NREG
    ADD.W   #30000,D2
.NREG:  MOVE.W  D2,(A6)+
    MOVE.L  A6,DBCUR
.NDBA:
    CMP.W   #COM,(A3)+
    BEQ .XL
    CMP.W   #18,-(A3)
    BEQ     .BRCL

.E:
    MOVE.L  DBCUR(PC),D0        ; FINISH DEBUG INFOS
    BEQ.S   .NDBF
    MOVE.L  DBBEG(PC),A0
    MOVE.L  A0,D1
    ADD.L   #10,D1
    MOVE.L  D0,A1
    SUB.L   D1,D0
    BNE.S   .NZ
    DONEM   A0
    BRA.S   .NDBF
.NZ:    LSR.W   #1,D0
    MOVE.W  D0,8(A0)
    DONEH   A1
    MOVE.L  DBTAIL(PC),D0
    BNE.S   .LA2
    MOVE.L  A0,DBLIST
    BRA.S   .LN2
.LA2:   MOVE.L  D0,A1
    MOVE.L  A0,(A1)
.LN2:   MOVE.L  A0,DBTAIL
.NDBF:
    BRA JOBLOOP

.BRCL:  TST.L   D0          ; NO ARRAYS AS PROC ARGS
    BNE ERROR0
    tst.l   FIXUPSTACKREGS
    BEq .E
    move.w  .FIXATE,(A4)+
    BRA    .E
.FIXATE:
    subq.l   #4,a7
.DA:    ADDQ.L  #1,D6           ; HANDLE DEFARGS
    ADDQ.L  #2,A3
    CMP.W   #8,(A3)
    BNE.S   .DA1
    ADDQ.L  #2,A3
.DA1:   CMP.W   #1,(A3)+
    BNE .DA2
    ADDQ.L  #4,A3           ; SKIP VALUE
    MOVE.W  (A3),D2
    BRA .DAB
.DA2:
    tst.l   FIXUPSTACKREGS
    bne     .DA2_2
    move.l  .DAC2(PC),(A4)+
.DA2_2:
    subq.l  #2,a3
    MOVE.B  (A3),D2
    AND.B   #$FE,D2
    CMP.B   #4,D2
    BNE     ERROR30
    cmp.b   #5,(a3)+
    seq     d2
    and.l   #8,d2
    or.b    (a3)+,d2
    cmp.w   #12,d2
    bpl ERROR0
    move.l  a0,-(a7)
    lea .DAAR(PC,D2),A0
    tst.b   (a0)
    bne ERROR50
    st  (a0)
    move.l  (a7)+,a0
    move.w  .DAC(PC),(A4)
    or.w    d2,(A4)+
    addq.l  #1,FIXUPSTACKREGS
    move.w  (a3),d2
    BRA .DAB
.DAC:
    MOVE.L  D0,-(A7)
.DAC2:
    MOVEM.L D2-D7/A2-A6,-(A7)
.DAAR:
    DC.L    0,0,0,0


.15:    CMP.W   #1,D5
    BEQ .DA
    ADDQ.L  #2,A3
    LDEF

    
    MOVEM.L D0/D5/A0,-(A7)
    MOVE.L  A4,A0
    BSR TINYEXP         ; no EXP allowed!!!
    BTST    #3,ICODEPREFS+3
    BNE.S   .15a
    BSET    #3,ICODEPREFS+3
    BRA.S   .15b
.15a:
    cmp.l   .LASTVAL,D0
    bne     .15b
    move.l  a0,a4
.15b:
    move.l  d0,.LASTVAL
    MOVEM.L (A7)+,D0/D5/A0      ; NOTE:    DEF a=x   -->problems

    
    MOVEQ   #0,D2
    CMP.L   #$0013003C+IOFF,(A3)
    BNE.S   .NRLA
    ADDQ.L  #4,A3
    BSR .SETREG
    MOVEQ   #1,D2
.NRLA:
    MOVE.W  .16(PC),(A4)+
    JSR GVA0D4_9
    MOVE.B  4(A0),D4
    BEQ ERROR22
    TST D2
    BNE.S   .RDONE
    CMP.W   #19,(A3)
    BNE .1
    MOVEQ   #-1,D2          ; TEST: D2 WAS INTERIM

.2: ADDQ.L  #2,A3
.AGAIN:
    CMP.W   #IOFF+44,(A3)+
    BEQ.S   .PTR
    CMP.W   #IOFF+20,-2(A3)     ; a:LONG
    BEQ .1
    CMP.W   #IOFF+82,-2(A3)     ; a:FLOAT
    BNE .AG_00
    BSET    #5,5(A0)
    BRA ERROR87
.AG_00:
    CMP.W   #IOFF+61,-2(A3)     ; a:REAL
    BNE .REG
    BSET    #4,5(A0)
    BRA .1
.REG:   CMP.W   #IOFF+60,-2(A3)     ; a:REG
    BNE .OBJ

    BSR .SETREG

.RDONE: CMP.W   #COM,(A3)
    BEQ .1
    TST.W   (A3)
    BEQ .1
    BRA.S   .AGAIN
.PTR:
    CMP.W   #IOFF+39,(A3)+
    BNE ERROR33
    CMP.W   #IOFF+44,(A3)
    BEQ .MULTIPTR
.PTR_BACK:
    BSR.S   .SETT
    BRA .1

.MULTIPTR:
    MOVE.B  #0,16(A0)           ; references
.MULTI_00:
    CMP.W   #IOFF+44,(A3)
    BNE .PTR_BACK
    ADDQ.L  #2,A3
    CMP.W   #IOFF+39,(A3)+
    BNE ERROR33
    ADDQ.B  #1,16(A0)
    BRA .MULTI_00

.OBJ:   CMP.L   #-1,D2          ; NO INIT OBJ'S
    BEQ ERROR0
    CMP.W   #31,-2(A3)
    BNE ERROR33
    LDEF
    MOVE.L  (A3)+,A6
    MOVE.W  OSIZE(A6),D2
    EXT.L   D2
    MOVE.L  D2,D7
    MOVE.L  A6,(A0)         ; TYPE=OBJECT
    BSR.W   .5
    BRA .1

.SETT:  MOVE.W  (A3)+,D1
    CMP.W   #31,D1
    BEQ.S   .W3
    CMP.W   #IOFF+20,D1     ; "OF LONG"
    BNE.S   .W1
    LSL.L   #2,D2
    MOVE.W  #4,2(A0)
    BRA.S   .SK
.W1:    CMP.W   #IOFF+21,D1     ; "OF INT"
    BNE.S   .W2
    LSL.L   #1,D2
    MOVE.W  #2,2(A0)
    BRA.S   .SK
.W2:    CMP.W   #IOFF+22,D1     ; "OF CHAR"
    BNE ERROR33
.SK:    RTS
.W3:    MOVE.L  A4,-(A7)        ; "OF <object>"
    MOVE.L  (A3)+,A4
    MOVE.L  A4,(A0)
    MULU    OSIZE(A4),D2
    MOVE.L  (A7)+,A4
    BRA.S   .SK

.3_1:
    subq.l  #2,a3
    move.l  d2,d7
    bsr.s   .SETT
    bra .SK2

.3: MOVE.L  D2,D7
    CMP.W   #IOFF+45,(A3)       ; "OF"
    BNE.S   .SK2
    ADDQ.L  #2,A3
    CMP.W   #IOFF+44,(A3)
    BEQ     .ARR_00
    BSR.S   .SETT
.SK2:   CMP.L   #MAXINT,D2
    BPL ERROR31
    BSR     .5          ; a[100]:ARRAY OF <TYPE>
    BRA .1
.ARR_00:
    MOVE.B  #0,16(A0)
    LSL.L   #2,D2
.ARR_01:
    CMP.W   #IOFF+44,(A3)
    BNE     .ARR_02
    ADDQ.L  #2,A3
    CMP.W   #IOFF+39,(A3)+
    BNE ERROR33
    ADDQ.B  #1,16(A0)
    BRA .ARR_01
.ARR_02:
    MOVE.L  D2,-(A7)
    BSR     .SETT
    MOVE.L  (A7)+,D2
.ARR_03:
    CMP.L   #MAXINT,D2
    BPL ERROR31
    BSR.S   .5          ; a[100]:ARRAY OF <TYPE>
    BRA .1

.4: MOVE.L  D2,D7
    ADDQ.L  #1,D2
    CMP.L   #MAXINT,D2
    BPL ERROR31
    BSR.S   .5          ; a[100]:STRING
    MOVE.W  .11(PC),(A4)+
    SWAP    D7
    MOVE.W  .10(PC),(A4)+
    MOVE.L  D7,(A4)+
    MOVE.W  .LC(PC),(A4)+
    CMP.W   #3,D5
    BEQ.S   .14
    ADDQ.L  #8,XTRASTACK
    BRA .1
.14:    ADDQ.L  #8,GLOBSTACK
    BRA .1

.LIST:  MOVE.W  #4,2(A0)        ; SET TO "PTR TO LONG"
    MOVE.L  D2,D7
    LSL.L   #2,D2
    CMP.L   #MAXINT,D2
    BPL ERROR31
    BSR.S   .5          ; a[25]:LIST
    MOVE.W  .11(PC),(A4)+
    SWAP    D7
    MOVE.W  .10(PC),(A4)+
    MOVE.L  D7,(A4)+
    MOVE.W  .LC(PC),(A4)+
    CMP.W   #3,D5
    BEQ.S   .L1
    ADDQ.L  #8,XTRASTACK
    BRA .1
.L1:    ADDQ.L  #8,GLOBSTACK
    BRA .1

.5: CMP.L   #1,D2           ; INIT ARRAY
    BMI ERROR31
    MOVE.W  .6(PC),(A4)+
    BTST    #0,D2
    BEQ.S   .7
    ADDQ.L  #1,D2
.7: BTST    #1,D2
    BEQ.S   .7B
    ADDQ.L  #2,D2
.7B:    MOVE.L  D2,D3
    NEG.L   D3
    MOVE.W  D3,(A4)+
    ADD.L   D2,D0
    CMP.W   #3,D5
    BEQ.S   .12
    MOVE.L  XTRASTACK(PC),D3    ; LOC
    BSR .C
    MOVE.L  D3,XTRASTACK
    BRA.S   .13
.12:    MOVE.L  GLOBSTACK(PC),D3    ; GLOB
    BSR .C
    MOVE.L  D3,GLOBSTACK
.13:    MOVE.W  .8(PC),(A4)+

    MOVE.B  4(A0),D4
    BEQ ERROR22
;   CMP.B   #GLOBV,D4       ;
;   BNE.S   .20         ; SET REG
;   BCLR    #1,-2(A4)       ;
;.20:   MOVE.W  10(A0),(A4)+
;   CMP.B   #GLOBV,D4
;   BEQ .9
;   BSET    #1,-4(A4)
    JMP GVA0D4_9

.6: LEA 4(A7),A7
.8: MOVE.L  A7,2(A5)
.10:    MOVE.L  #1,-(A7)
.LC:    CLR.L   -(A7)
.11:    CLR.W   (A7)
.16:    MOVE.L  D0,2(A5)
.C: ADD.L   D2,D3
    MOVE.L  D3,D2
    RTS

.SETREG:BTST    #3,5(A0)
    BEQ ERROR50
    CMP.W   #2,D5
    BNE ERROR33         ; regvars only local
    RTS
.LASTVAL:
    DC.L    0
FIXUPSTACKREGS:
    DC.L    0

;*-*
;; 34
GETNRLOC:             ; 34 NRLOC IN -2(A4)
    MOVE.W  NRLOC(PC),-2(A4)
    BRA JOBLOOP
;*-*
;; 35
INSERTVAROFFDEST:         ; 35
    SUBQ.L  #2,A4
    MOVE.L  CURIDENTREC(PC),A0
    JSR GVA0D2_9
    BRA JOBLOOP
;*-*
;; 36
GETBRACKLEFT:         ; 36
    MOVE.W  (A3)+,D6
    CMP.W   #17,D6
    BNE ERROR23
    BRA JOBLOOP
;*-*
;; 37
GETBRACKRIGHT:            ; 37
    MOVE.W  (A3)+,D6
    CMP.W   #18,D6
    BNE ERROR23
    BRA JOBLOOP
;*-*
;; 38
DOINCBIN:             ; 38
    LEA CURACODE(PC),A0
    MOVE.L  8(A0),D3
    ADD.L   12(A0),D3
    SUB.L   A4,D3
    SUBQ.L  #8,D3           ; MAXREADSIZE
    CMP.L   #3000,D3
    BMI ERROR37

    CMP.W   #STR,(A3)+      ; D0-D6/ A3/A6
    BNE ERROR0
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,D6        ; D6=WORDLENSTR
    MOVE.L  DOSBASE(PC),A6
    MOVE.L  A3,D1
    MOVE.L  #1005,D2
    JSR -30(A6)
    TST.L   D0
    BEQ ERROR28
    MOVE.L  D0,D5           ; D5=HANDLE

    MOVE.L  D5,D1
    MOVE.L  A4,D2
    JSR -42(A6)         ; READ
    MOVE.L  D0,D4           ; D4=READLEN
    MOVE.L  D5,D1
    JSR -36(A6)         ; CLOSE
    CMP.L   #2,D4
    BMI ERROR28
    SUB.L   D4,D3
    CMP.L   #3000,D3
    BMI ERROR37
    ADD.L   D4,A4
    BTST    #0,D4
    BEQ.S   .1
    MOVE.B  #0,(A4)+
.1:
    LSL.W   #1,D6
    EXT.L   D6
    ADD.L   D6,A3
    BRA JOBLOOP
;*-*
;; 39               LONG
DOLONG:               ; 39
.2: CMP.W   #IDENT,-2(A3)
    BEQ     .4
.3: bsr     ASM_GRABVALUE
    tst.l   d0
    bmi     ERROR30
    move.l  d1,(a4)+
    CMP.W   #COM,(A3)
    BNE.S   .1
    ADDQ.L  #2,A3
    BRA.S   .2
.1: BRA JOBLOOP
.4: MOVem.L D0-D7/A0-A2/A5/A6,-(a7)
    MOVE.L  (A3)+,a0
    cmp.b   #LAB,4(a0)
    bne ERROR4
    move.w  10(a0),d0
    bsr ADDBRANCHRELOC
    movem.l (a7)+,d0-a2/a5/a6
    cmp.w   #COM,(A3)
    BNE .1
    ADDQ.L  #2,a3
    bra .2
;*-*
;; 40               INT
DOINT:                ; 40
.2:
    bsr     ASM_GRABVALUE
    tst.l   d0
    bmi     ERROR30
    move.w  d1,(a4)+
    CMP.W   #COM,(A3)
    BNE.S   .1
    ADDQ.L  #2,A3
    BRA.S   .2
.1: BRA JOBLOOP
;*-*
;; 41               CHAR
DOCHAR:               ; 41
.2: bsr     ASM_GRABVALUE
    tst.l   d0
    bmi     .3
    move.b  d1,(a4)+
.5: CMP.W   #COM,(A3)
    BNE.S   .1
    ADDQ.L  #2,A3
    BRA.S   .2
.1: MOVE.L  A4,D0
    BTST    #0,D0
    BEQ.S   .6
    MOVE.B  #0,(A4)+
.6: BRA JOBLOOP
.3: CMP.W   #STR,(A3)+
    BNE ERROR30
    MOVE.W  (A3)+,D1        ; =REAL LEN
    MOVE.W  (A3)+,D0        ; =WORD LEN
    SUBQ.L  #1,D1
.4: MOVE.B  (A3)+,(A4)+
    DBRA    D1,.4
    ADDQ.L  #1,A3
    MOVE.L  A3,D0
    BTST    #0,D0
    BEQ.S   .5
    ADDQ.L  #1,A3
    BRA.S   .5
;*-*
;; 42
JOBINS:               ; 42
    MOVE.L  A5,-(A7)
    BSR.S   DOINSREC
    CMPA.L  (A7)+,A5
    BNE ERROR9
    BRA JOBLOOP

DOINSREC:
    MOVE.W  (A3)+,D7
    BEQ ERROR0
    CMP.W   #-1,D7
    BEQ ERROR0
    MOVE.L  A2,-(A7)
    BSR DOINS
    MOVE.L  (A7)+,A2
    RTS
;*-*
;; 43
EXPECT:               ; 43 IF FOLLOWING CODE IS PRESENT, CONTINUE
    MOVE.W  (A3),D0         ;    WITH JOB, ELSE STOP.
    CMP.W   (A2)+,D0
    BEQ.S   .1          ;    CONTINUE, EAT 1
.2: TST.W   (A2)+
    BNE.S   .2          ;    STOP, EAT ALL
    SUBQ.L  #2,A2
    BRA JOBLOOP
.1: ADDQ.L  #2,A3
    BRA JOBLOOP
;*-*
;; 44
SERROR:
    BRA ERROR0          ; 44
;*-*
;; 45
EXPECT2:              ; 45 IF FOLLOWING CODE IS PRESENT, CONTINUE
    MOVE.W  (A3),D0         ;    WITH JOB, ELSE FIND PAST ZERO.
    CMP.W   (A2)+,D0
    BEQ.S   .1          ;    CONTINUE, EAT 1
.2: TST.W   (A2)+
    BNE.S   .2          ;    STOP, EAT ALL TO NEXT ZERO
    BRA JOBLOOP
.1: ADDQ.L  #2,A3
    BRA JOBLOOP
;*-*
;; 46
JOBSKIP:              ; 46
    ADDQ.L  #2,A2
    BRA JOBLOOP
;*-*
;; 47
EXPECTASSIGN:         ; 47
    CMP.W   #ASSGN,(A3)+
    BNE ERROR2
    BRA JOBLOOP
;*-*
;; 48
EXPECTTO:             ; 48 CHOMEUR
    BRA JOBLOOP
;*-*
;; 49               FOR - middle part
DOMIDFOR:                 ; 49
    CMP.W   #IOFF+39,(A3)+      ; MUST BE "TO"
    BNE ERROR0                  ;

    bsr     EXP

    move.w  .P1(pc),(a4)+
    bsr     NEWLABEL
    move.l  d0,.ST
    bsr     ADDLABEL
    clr.l   (a4)+
    bsr     NEWLABEL
    move.l  d0,.SS
    bsr     ADDLABEL
    move.l  #1,(a4)+

.P1:bra.b   .cont
.ST:dc.l    0  ; to -
.SS:dc.l    0  ; step

.cont:
    move.w  .P2(pc),(a4)+
    move.l  .ST,d0
    bsr     ADDBRANCHRELOC    ; store destination


    cmp.w   #IOFF+40,(a3)
    bne     .cont2
    addq.l  #2,a3
    bsr     EXP
    move.w  .P2(PC),(a4)+
    move.l  .SS,d0            ; calculate step
    bsr     ADDBRANCHRELOC
.cont2:

    move.w  .MO(pc),(a4)+     ; get current value
    move.l  CURIDENTREC,a1
    jsr     GVA1D2_0          ; get var;

    bsr     NEWLABEL
    move.l  d0,.LB            ; entry point.
    bsr     ADDLABEL          ; here the code begins

    move.w  .TS(PC),(a4)+
    move.l  .SS(PC),D0        ; check step sign
    bsr     ADDBRANCHRELOC    ;

    move.l  .CD(PC),(A4)+
    move.w  .CD+4(PC),(A4)+
    move.l  .ST(pc),d0        ; compare actual against final
    bsr     ADDBRANCH         ; cmp [1]
    move.l  .CD+6(pc),(a4)+

    bsr     NEWLABEL
    move.l  d0,.FINE          ;
    bsr     ADDBRANCH

    move.l  .CD+10(pc),(a4)+  ;
    move.w  .CD+14(pc),(a4)+  ;
    move.l  .ST(pc),d0        ; as above...
    bsr     ADDBRANCH         ;
    move.l  .neg+4(pc),(a4)+
    move.l  .FINE,d0
    bsr     ADDBRANCH

    move.l  .FINE,(a5)+          ; label to be set after the loop code
    move.l  .LB,(a5)+            ; label to jump to perform next step
    move.l  .SS(PC),(a5)+            ; step position
    move.l  CURIDENTREC,(a5)+    ; current ident.
    bra     JOBLOOP

.P2:dc.w    $23c0,0,0      ; move.l d0,$xxx
.LB:dc.l    0
.FINE:dc.l  0
.TS:dc.w    $4ab9,0,0      ; tst.l $xxx
.CD:bmi.b   .neg           ; 2
    cmp.l   .CD(pc),d0      ; 2+[2]
    bgt     .CD            ; 4
    bra.B   .pos           ; 2
.neg:
    cmp.l   .CD(pc),d0      ; 2+[2]
    bmi     .CD            ; 4
.pos:
.MO:move.l  12(a5),d0


;*-*
;; 50               Initialization
ENDGLOBVAR:           ; 50
    TST.W   SCOPE
    BNE JOBLOOP
    MOVE.W  #-1,SCOPE
    TSTMOD
    BNE JOBLOOP
    BTST    #5,CODEPREFS
    BNE JOBLOOP
    MOVE.L  INITCODE(PC),(A4)+  ; DELEGATES
    MOVEQ   #6,D0
    MOVE.W  .1(PC),NEWOP
    BSR ADDBRANCH
    BTST    #0,CODEPREFS
    BNE     .SETUP_POWERPC      ; PPC!!!
    MOVE.L  INITCODE(PC),(A4)+  ; MAIN
    MOVEQ   #1,D0
    MOVE.W  .1(PC),NEWOP
    BSR ADDBRANCH
.BACK:
    MOVE.L  LIBINFO(PC),D0
    BNE.S   .5
    MOVE.L  GLOBSTACK(PC),D0
    BEQ.S   .3
    MOVE.W  CLEARXTRA(PC),(A4)+
    MOVE.L  D0,(A4)+
.3: MOVE.L  .4(PC),(A4)+
.8: MOVE.W  PROCRTS(PC),(A4)+
    BRA JOBLOOP
.4: MOVE.L  D0,-28(A4)
.1: JSR .1
.6: MOVE.L  -4(A4),A7
.7: MOVEQ   #-1,D0          ; SUCCES
.5: MOVE.L  .6(PC),(A4)+
    MOVE.W  .7(PC),(A4)+
    BRA.S   .8
.SETUP_POWERPC:
    MOVE.l  #$4e53ff70,(a4)+
    MOVE.l  #$244f24bc,(A4)+
    MOVEQ   #1,D0
    BSR ADDBRANCHRELOC
    MOVE.L  #8,D0
    LEA     .PPCCODE(PC),A0
.COPY:
    MOVE.L  (A0)+,(A4)+
    DBRA    D0,.COPY
    BRA     .BACK

.PPCCODE:
    CLR.L   4(A2)           ;4
    CLR.L   8(A2)           ;4
    MOVE.L  A7,12(A2)       ;4
    MOVE.L  A7,D2           ;2
    SUB.L   -64(A4),D2      ;4
    MOVE.L  D2,16(A2)       ;4
    MOVE.L  -132(A4),A6     ;4
    JSR     -30(A6)         ;4
    MOVE.L  20(A2),D0       ;4
    UNLK    A3              ;2

;*-*
;; 51
ELSEMAKELAB:          ; 51
    BSR.W   NEWLABEL
    MOVE.L  D0,(A5)+
    BRA JOBLOOP
;*-*
;; 52
ELSEUSELAB:           ; 52
    MOVE.L  -(A5),D0
    MOVE.L  D0,D4
    CLR.W   NEWOP
    BSR ADDBRANCH
    MOVE.L  D4,(A5)+
    BRA JOBLOOP
;*-*
;; 53
ELSEDEFLAB:           ; 53
    MOVE.L  -(A5),D0
    BSR ADDLABEL
    BRA JOBLOOP
;*-*
;; 54
ELSEWAISTELAB:            ; 54
    SUBQ.L  #4,A5
    BRA JOBLOOP
;*-*
;; Object blablabla
INHERITOBJECT:            ; A1=OBJECTHEAD, A2=MEMBERTAIL
    MOVEM.L D0-D2/A0/A4-A6,-(A7)    ; A3=SUPERCLASS INTERIM
    CMP.W   #31,(A3)+
    BNE ERROR39
    MOVE.L  (A3)+,A0        ; A0=SUPER
    MOVE.L  A0,OSUPER(A1)
    CMPA.L  A0,A1
    BEQ ERROR50
    MOVE.W  ODESTR(A0),ODESTR(A1)
.NID:   TST.W   OID(A0)
    BEQ ERROR39
    MOVE.W  OSIZE(A0),D1        ; SET D1=OFFSET
    EXT.L   D1
    LEA OMEMB+4(A0),A0      ; A0=SUPERMEMBERLIST
.XL:MOVE.L  -(A0),A0
    MOVE.L  A0,D0
    BEQ.S   DONE1
    GETM    A4
    CLR.L   (A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    MOVE.W  OBJCOUNT(PC),(A4)+
    ADDQ.L  #2,A0
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    LEA -16(A0),A0
    LEA -16(A4),A5
    MOVE.L  A5,(A2)
    LEA ONEXT(A5),A2
    CLR.L   (A2)
    DONEM   A4
    BRA.S   .XL
DONE1:  MOVE.L  D1,SUPERSIZE        ; STACK NEW OSIZE FROM SUPER
    MOVE.L  OSUPER(A1),A0
    LEA OMETHOD(A0),A0      ; A0=SUPER.METHODS
    MOVE.L  OMETHOD(A1),D1      ; D1=HEAD OWN.METHODS
    CLR.L   OMETHOD(A1)     ; REMAKE FROM SCRATCH
.L2:    MOVE.L  (A0),D0         ; M_NEXT
    BEQ .DONE2
    MOVE.L  D0,A0
    MOVE.L  M_NAME(A0),D0       ; D0=NAME
    TST.L   D1
    BEQ.S   .INH
    MOVE.L  D1,A4           ; A4=SEARCH IN OWN METHODS
    MOVEQ   #0,D1
.L3:    MOVE.L  D0,A5
    MOVE.L  M_NAME(A4),A6
.L4:    CMPM.B  (A5)+,(A6)+     ; STRCMP
    BNE.S   .C
    TST.B   -1(A6)
    BNE.S   .L4
    MOVE.L  (A4),A5         ; LINK IN OVERRIDING METHOD
    MOVE.L  OMETHOD(A1),(A4)
    MOVE.L  A4,OMETHOD(A1)
    MOVE.W  M_OFF(A0),M_OFF(A4) ; SAME OFFSET
    MOVE.L  M_PROC(A0),A6
    MOVE.W  (A6),D2
    MOVE.L  M_PROC(A4),A6
    CMP.W   (A6),D2
    BEQ.S   .ASAME
    BSET    #1,M_FLAGS(A4)
.ASAME: MOVE.L  A5,D2
    BEQ.S   .L2
    MOVE.L  A5,A6           ; IF THERE'S PART OF OWNMETHODS
.L5:    TST.L   (A5)            ; LEFT, RELINK IT.
    BEQ.S   .DD
    MOVE.L  (A5),A5
    BRA.S   .L5
.DD:    MOVE.L  D1,(A5)
    MOVE.L  A6,D1
    BRA.S   .L2
.C: MOVE.L  (A4),-(A7)      ; PICK NEXT OWN METHOD
    MOVE.L  D1,(A4)         ; RELINK D1
    MOVE.L  A4,D1
    MOVE.L  (A7),A4
    TST.L   (A7)+
    BNE.S   .L3
.INH:   GETM    A4
    MOVE.L  OMETHOD(A1),(A4)
    MOVE.L  A4,OMETHOD(A1)
    ADDQ.L  #4,A4
    MOVE.L  M_PROC(A0),(A4)+
    MOVE.W  M_TYPE(A0),(A4)+
    BSET    #0,-1(A4)       ; INHERITED
    MOVE.W  M_OFF(A0),(A4)+
    MOVE.L  M_NAME(A0),(A4)+
    DONEM   A4
    BRA .L2
.DONE2: MOVE.L  OSUPER(A1),A0       ; ADD NEW METHODS (D1)
    MOVE.W  ODEL(A0),D0     ; D0 = DELEGATESIZE
    MOVE.W  ODELOFF(A0),ODELOFF(A1)
    BSR.S   NEWMETHODS
    MOVEM.L (A7)+,D0-D2/A0/A4-A6
    MOVE.L  SUPERSIZE,D1
    RTS

NOINHERIT:
    MOVEM.L D0-D2/A0/A4/A6,-(A7)
    CLR.W   ODEL(A1)
    MOVE.L  OMETHOD(A1),D1
    BEQ.S   .1
    CLR.L   OMETHOD(A1)
    MOVEQ   #4,D0
    BSR NEWMETHODS
.1: MOVEM.L (A7)+,D0-D2/A0/A4/A6
    RTS

NEWMETHODS:
    TST.L   D1
    BEQ.S   .DONE3

    TST.W   D0
    BNE.S   .SAFE
    MOVEQ   #4,D0           ; INHERITANCE FROM METHOD-LESS OBJECT
.SAFE:
    MOVE.L  D1,A4
.L6:    MOVE.L  (A4),D2         ; NEW METHODS WITH OTHER M_OFF
    MOVE.L  OMETHOD(A1),(A4)
    MOVE.L  A4,OMETHOD(A1)
    MOVE.W  D0,M_OFF(A4)

    MOVE.L  M_NAME(A4),A6
    CMP.B   #'e',(A6)+
    BNE.S   .NEND
    CMP.B   #'n',(A6)+
    BNE.S   .NEND
    CMP.B   #'d',(A6)+
    BNE.S   .NEND
    TST.B   (A6)
    BNE.S   .NEND
    MOVE.L  M_PROC(A4),A0
    TST.W   (A0)            ; .end() NO ARGS
    BNE ERROR59
    BSET    #2,M_FLAGS(A4)
    MOVE.W  D0,ODESTR(A1)
.NEND:
    ADDQ.W  #4,D0
    MOVE.L  D2,A4
    TST.L   D2
    BNE.S   .L6
.DONE3: MOVE.W  D0,ODEL(A1)
    RTS

CREATEDELEGATEMEM:        ; A1=OBJECTHEAD, A2=MEMBERTAIL
    MOVEM.L A4-A5,-(A7)
    GETM    A4
    CLR.L   (A4)+           ; .NEXT
    MOVE.W  D1,(A4)+        ; .OFFSET
    MOVE.W  D1,ODELOFF(A1)
    ADDQ.W  #4,D1           ; SET D1=OFFSET
    MOVE.W  #$0100,(A4)+        ; .FLAGS
    MOVE.W  #4,(A4)+        ; .SIZE
    MOVE.W  OBJCOUNT(PC),(A4)+  ; .ID
    CLR.L   (A4)+           ; .ASCII (PRIVATE)
    LEA -12(A4),A5
    MOVE.L  A5,(A2)
    LEA ONEXT(A5),A2
    CLR.L   (A2)
    DONEM   A4
    MOVEM.L (A7)+,A4-A5
    RTS

SUPERSIZE:    DC.L    0
OBJCOUNT:     DC.W    0
OBJPRIV:      DC.W    0   ; 0=PUB / 1=PRIV

CHECKPR:
    MOVE.W  (A3),D0
    CMP.W   #IOFF+64,D0
    BNE.S   .y
    CLR.W   OBJPRIV
    ADDQ.L  #2,A3
    BRA.S   .x
.y: CMP.W   #IOFF+65,D0
    BNE.S   .x
    MOVE.W  #1,OBJPRIV
    ADDQ.L  #2,A3
.x:
    RTS


FINDDOUBLEMEMBER:         ; A0=MEMBER, A1=OBJH, USE D0, SAVE ALL
    MOVEM.L A3-A5,-(A7)
    LEA OMEMB+4(A1),A3
.XL:MOVE.L  ONEXT(A3),A3
    MOVE.L  A3,D0
    BEQ.S   .X
    CMP.L   A3,A0
    BEQ.S   .XL
    MOVE.L  OASCII(A0),A4
    MOVE.L  OASCII(A3),A5       ; ASCII
    MOVE.L  A5,D0
    BEQ.S   .XL
.L2:    CMPM.B  (A5)+,(A4)+
    BNE.S   .XL
    TST.B   -1(A5)
    BNE.S   .L2
    BRA ERROR43
.X: MOVEM.L (A7)+,A3-A5
    RTS

;*-*
;; 55
DOOBJECT:             ; 55
    MOVE.L  A2,-(A7)
    CLR.W   OBJPRIV
    ADDQ.W  #1,OBJCOUNT
    MOVEQ   #0,D1           ; D1=OFFSET !
    CMP.W   #31,(A3)+       ; objheader
    BNE ERROR39
    MOVE.L  (A3)+,A1        ; A1=OBJECT HEAD
    TST.W   OID(A1)
    BNE ERROR43             ; DOUBLE DECL OBJ
    MOVE.W  OBJCOUNT(PC),OID(A1)
    LEA OMEMB(A1),A2        ; A2=TAIL MEMLIST
    CLR.B   OFLAGS(A1)
    TST.W   EXPORTFLAG
    BEQ.S   .NE
    BSET    #0,OFLAGS(A1)       ; EXPORT OBJECT
.NE:
    TST.W   (A3)
    BEQ.S   .NOINH
    BSR CHECKPR
.z1:
    TST.W   (A3)
    BEQ.S   .NOINH
    CMP.W   #IOFF+45,(A3)+  ; OF <SUPERCLASS>
    BNE ERROR39
    BSR INHERITOBJECT       ; WANTS A1,A2, SAVES ALL
    BSR CHECKPR
.z2:
    BRA.S   .NOI
.NOINH: BSR NOINHERIT
.NOI:   TST.W   ODEL(A1)
    BEQ.S   .NVIRT
    CMP.W   #-1,ODELOFF(A1)
    BNE.S   .NVIRT
    BSR CREATEDELEGATEMEM   ; WANTS A1,A2, SAVES ALL
.NVIRT: TST.W   (A3)+
    BNE ERROR39
.XL:CMPI.W  #21,(A3)+
    BNE ERROR39
    MOVE.W  (A3)+,LINENUM   ; ERline
    CMP.W   #IOFF+49,(A3)   ; ENDOBJECT
    BEQ .X
    BSR CHECKPR
.z3:
    TST.W   (A3)
    BNE.S   .L2
    ADDQ.L  #2,A3
    BRA.S   .XL

.L2:
    CMP.W   #IOFF+75,(A3)
    BNE     .CONTINUE
    ADDQ.L  #2,A3
    CMP.W   #29,(A3)+
    BNE     ERROR0
    ADDQ.L  #4,A3
    MOVE.L  D1,.LAST_SIZE
    MOVE.L  D1,.TOTAL_SIZE
.UNION_LOOP:
    MOVE.L  .LAST_SIZE,D1
    BSR     OBJECT_UNION
    CMP.L   .TOTAL_SIZE,D1
    BLT     .UN_SKIP
    MOVE.L  D1,.TOTAL_SIZE
.UN_SKIP:
    CMP.W   #COM,(A3)+
    BEQ     .UNION_LOOP
    MOVE.L  .TOTAL_SIZE,D1
    CMp.W   #30,-2(A3)
    BNE     ERROR34
    BRA     .1
.LAST_SIZE:
    DC.L    0
.TOTAL_SIZE
    DC.L    0



.CONTINUE:
    CMP.W   #39,(A3)+       ; ONE OR MORE DEFS
    BNE ERROR31
    MOVE.L  (A3)+,A0        ; A0=IDENTPTR MEMBER
    BSR FINDDOUBLEMEMBER
    TST.W   OBJPRIV
    BEQ.S   .PUB
    BSET    #0,OFLAGS(A0)   ; MEMBER IS PRIVATE
.PUB:   MOVE.L  A0,(A2)
    LEA ONEXT(A0),A2
    CLR.L   (A2)
    TST.W   OID(A0)
    BNE ERROR43             ; DOUBLE DECL OBJ
    MOVE.W  D1,(A0)
    MOVE.W  #4,4(A0)
    MOVEQ   #4,D2           ; D2=ADDSIZE
    MOVE.W  (A3),D0
    CMP.W   #19,D0          ; :<TYPE>
    BNE .ARRAY
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,D0
    CMP.W   #IOFF+20,D0
    BEQ.S   .O
    CMP.W   #IOFF+21,D0
    BNE.S   .2
    MOVEQ   #2,D2           ; INT
    BRA.S   .O
.2: CMP.W   #IOFF+22,D0
    BNE.S   .SOBJ
    MOVEQ   #1,D2           ; CHAR
.O: MOVE.L  D2,D3
.O2:    MOVE.W  OBJCOUNT(PC),6(A0)
    MOVE.W  D3,4(A0)
    ADD.L   D2,D1
    BTST    #0,1(A0)        ; CHECK ON WORD BOUNDARIES
    BEQ.S   .1
    BTST    #0,D2
    BNE.S   .1
    ADDQ.W  #1,(A0)
    ADDQ.L  #1,D1
.1: MOVE.W  (A3)+,D0
    BEQ .XL
    CMP.W   #COM,D0
    BEQ .L2
    BRA ERROR39
.X: ADDQ.L  #2,A3
    MOVE.W  D1,OSIZE(A1)
    TST.W   D1
    BEQ ERROR39         ; EMPTY OBJECT
    MOVE.L  (A7)+,A2
    BRA JOBLOOP         ; ONLY EXIT!
.SOBJ:  CMP.W   #IOFF+44,D0
    BEQ.S   .PTR
    CMP.W   #31,D0
    BNE ERROR33
    MOVE.L  (A3)+,A6
    MOVE.L  A6,OPTRTYPE(A0)
    BSET    #1,OFLAGS(A0)
    TST.W   OID(A6)
    BEQ ERROR42
    CMP.L   A1,A6
    BEQ ERROR39         ; RECURSIVE DEFINITION
    MOVE.W  4(A6),D2
    EXT.L   D2
    MOVEQ   #0,D3
    BRA.S   .O2
.PTR:   CMP.W   #IOFF+39,(A3)+
    BNE ERROR33
    BSR .GETT
    MOVEQ   #4,D2
    MOVEQ   #4,D3
    BRA.W   .O2
.ARRAY: CMP.W   #29,D0          ; []:ARRAY
    BNE.W   .O
    ADDQ.L  #6,A3           ; ALSO SKIP ]OFFSET
    CMP.W   #VALUE,(A3)+
    BNE ERROR30
    MOVE.L  (A3)+,D2
    CMP.W   #30,(A3)+
    BNE ERROR34
    CMP.W   #19,(A3)+
    BNE ERROR0
    CMP.W   #IOFF+41,(A3)
    BNE     .zz1
;    BNE ERROR0
    ADDQ.L  #2,A3
    CMP.W   #IOFF+45,(A3)       ; OF?
    BNE.S   .A2
    ADDQ.L  #2,A3
.zz1:
    BSR .GETT
    MOVE.L  D0,D3
    AND.L   #$FFFFFFF0,D3
    BEQ.S   .A3
    MOVE.L  D0,A6
    MOVE.W  OSIZE(A6),D0
.A3:    MULU    D0,D2
    BRA.S   .A2A
.A2:    MOVE.L  #1,OPTRTYPE(A0)
    BSET    #1,OFLAGS(A0)
.A2A:   BTST    #0,D2
    BEQ .A1
    ADDQ.L  #1,D2
.A1:    CMP.L   #MAXOBJSIZE,D2
    BPL ERROR31
    CMP.L   #1,D2
    BMI ERROR31
    MOVEQ   #0,D3
    BRA .O2
.GETT:  MOVE.W  (A3)+,D0        ; TRASHES ONLY D0 (TYPE)
    CMP.W   #31,D0              ; PTR/ARRAY OF/TO <OBJECT>
    BNE.S   .GT1
    MOVE.L  (A3)+,D0
    BRA.S   .GTE
.GT1:   CMP.W   #IOFF+20,D0     ; LONG
    BNE.S   .GT2
    MOVEQ   #4,D0
    BRA.S   .GTE
.GT2:   CMP.W   #IOFF+21,D0     ; INT
    BNE.S   .GT3
    MOVEQ   #2,D0
    BRA.S   .GTE
.GT3:   CMP.W   #IOFF+22,D0     ; CHAR
    BNE.S   .MUT
    MOVEQ   #1,D0
.GTE:   MOVE.L  D0,OPTRTYPE(A0)
    BSET    #1,OFLAGS(A0)
    RTS
.MUT:   CMP.W   #45,D0          ; MUTUALLY RECURSIVE PTR TO <OBJ>
    BNE ERROR33
    MOVE.L  D1,-(A7)
    MOVE.L  (A3)+,D1
    BSR FINDOBJ
    TST.L   D0
    BEQ ERROR42
    MOVE.L  (A7)+,D1
    BRA.S   .GTE

;; UNION

OBJECT_UNION:
    CMP.W   #29,(A3)
    BNE     ERROR0
    ADDQ.L  #6,A3
.CONTINUE
    CMP.W   #39,(A3)+       ; ONE OR MORE DEFS
    BNE ERROR33             ; Not a member
    MOVE.L  (A3)+,A0        ; A0=IDENTPTR MEMBER
    BSR FINDDOUBLEMEMBER    ; check if already defined
    TST.W   OBJPRIV         ; check if private
    BEQ.S   .PUB            ;
    BSET    #0,OFLAGS(A0)   ; MEMBER IS PRIVATE
.PUB:
    MOVE.L  A0,(A2)         ; Link a member (ok)
    LEA ONEXT(A0),A2        ; A0 = member (from 0)
    CLR.L   (A2)            ; A2 = member link pos
    TST.W   OID(A0)         ; Check if defined already
    BNE ERROR43             ; DOUBLE DECL OBJ
    MOVE.W  D1,(A0)         ; D1 = object offset;
    MOVE.W  #4,4(A0)        ; standard size = LONG
    MOVEQ   #4,D2           ; D2=ADDSIZE
    MOVE.W  (A3),D0         ; check if there's somethig after member
    CMP.W   #19,D0          ; :<TYPE>
    BNE .ARRAY              ;
    ADDQ.L  #2,A3           ;
    MOVE.W  (A3)+,D0        ;
    CMP.W   #IOFF+20,D0     ; LONG
    BEQ.S   .O              ;
    CMP.W   #IOFF+21,D0     ; INT
    BNE.S   .2
    MOVEQ   #2,D2           ;
    BRA.S   .O
.2: CMP.W   #IOFF+22,D0     ; CHAR
    BNE     .SOBJ
    MOVEQ   #1,D2
.O: MOVE.L  D2,D3
.O2:MOVE.W  OBJCOUNT(PC),6(A0)
    MOVE.W  D3,4(A0)        ; SIZEOF member
    ADD.L   D2,D1           ;
    BTST    #0,1(A0)        ; CHECK ON WORD BOUNDARIES
    BEQ.S   .1              ;
    BTST    #0,D2           ;
    BNE.S   .1              ;
    ADDQ.W  #1,(A0)         ;
    ADDQ.L  #1,D1           ;
.1: MOVE.W  (A3)+,D0        ;
;    BEQ .XL                ; anything else?
    CMP.W   #COM,D0
    BEQ .CONTINUE
    CMP.W   #30,D0          ; "]" - end of part 1
    BNE ERROR39

;    GETM A0                 ; fill an with empty field
;    MOVE.L  A0,(A2)
;    LEA ONEXT(A0),A2
;    MOVE.W  OBJCOUNT(PC),6(A0)
;    MOVE.W  #0,4(A0)        ;
;    MOVE.W  D1,(A0)         ;
;    BSET    #0,2(A0)        ; private
;    BSET    #1,2(A0)        ; typed?
;    MOVE.L  #0,8(A0)        ; noname
;    MOVE.L  #1,12(A0)       ;
;    MOVE.L  #0,16(A0)       ;
;    DONEM A0

    RTS

.X: ADDQ.L  #2,A3           ; skip sth
    MOVE.W  D1,OSIZE(A1)    ;
    TST.W   D1              ;
    BEQ ERROR39             ; EMPTY OBJECT
    MOVE.L  (A7)+,A2        ;
    BRA JOBLOOP             ; ONLY EXIT!

.SOBJ:
    CMP.W   #IOFF+44,D0     ; PTR
    BEQ.S   .PTR            ;
    CMP.W   #31,D0          ; :object
    BNE ERROR33             ;
    MOVE.L  (A3)+,A6        ;
    MOVE.L  A6,OPTRTYPE(A0) ;
    BSET    #1,OFLAGS(A0)   ;
    TST.W   OID(A6)         ;
    BEQ ERROR42             ;
    CMP.L   A1,A6           ;
    BEQ ERROR39             ; RECURSIVE DEFINITION
    MOVE.W  4(A6),D2        ;
    EXT.L   D2              ;
    MOVEQ   #0,D3           ;
    BRA     .O2             ;
.PTR:                       ;
    CMP.W   #IOFF+39,(A3)+  ; "PTR TO"
    BNE ERROR33             ;
    BSR .GETT               ;
    MOVEQ   #4,D2           ;
    MOVEQ   #4,D3           ;
    BRA.W   .O2             ;
.ARRAY:
    CMP.W   #29,D0          ; []:ARRAY
    BNE.W   .O              ;
    ADDQ.L  #6,A3           ; ALSO SKIP ]OFFSET
    CMP.W   #VALUE,(A3)+    ;
    BNE ERROR30             ;
    MOVE.L  (A3)+,D2        ;
    CMP.W   #30,(A3)+       ;
    BNE ERROR34             ;
    CMP.W   #19,(A3)+       ;
    BNE ERROR0              ;
    CMP.W   #IOFF+41,(A3)+  ;
    BNE ERROR0              ;
    CMP.W   #IOFF+45,(A3)   ; OF?
    BNE.S   .A2             ;
    ADDQ.L  #2,A3           ;
    BSR .GETT               ;
    MOVE.L  D0,D3           ;
    AND.L   #$FFFFFFF0,D3   ;
    BEQ.S   .A3             ;
    MOVE.L  D0,A6           ;
    MOVE.W  OSIZE(A6),D0    ;
.A3:
    MULU    D0,D2           ;
    BRA.S   .A2A            ;
.A2:
    MOVE.L  #1,OPTRTYPE(A0) ;
    BSET    #1,OFLAGS(A0)
.A2A:
    BTST    #0,D2           ;
    BEQ .A1                 ;
    ADDQ.L  #1,D2           ;
.A1:
    MOVE.W  D2,OSIZEOF(A6)  ;
    CMP.L   #MAXOBJSIZE,D2  ;
    BPL ERROR31             ;
    CMP.L   #1,D2           ;
    BMI ERROR31             ;
    MOVEQ   #0,D3           ;
    BRA .O2                 ;
.GETT:  MOVE.W  (A3)+,D0        ; TRASHES ONLY D0 (TYPE)
    CMP.W   #31,D0              ; PTR/ARRAY OF/TO <OBJECT>
    BNE.S   .GT1
    MOVE.L  (A3)+,D0
    BRA.S   .GTE
.GT1:   CMP.W   #IOFF+20,D0     ; LONG
    BNE.S   .GT2
    MOVEQ   #4,D0
    BRA.S   .GTE
.GT2:   CMP.W   #IOFF+21,D0     ; INT
    BNE.S   .GT3
    MOVEQ   #2,D0
    BRA.S   .GTE
.GT3:   CMP.W   #IOFF+22,D0     ; CHAR
    BNE.S   .MUT
    MOVEQ   #1,D0
.GTE:   MOVE.L  D0,OPTRTYPE(A0)
    BSET    #1,OFLAGS(A0)
    RTS
.MUT:   CMP.W   #45,D0          ; MUTUALLY RECURSIVE PTR TO <OBJ>
    BNE ERROR33
    MOVE.L  D1,-(A7)
    MOVE.L  (A3)+,D1
    BSR FINDOBJ
    TST.L   D0
    BEQ ERROR42
    MOVE.L  (A7)+,D1
    BRA.S   .GTE
;*-*
;*-*

;; 56               OptiIF
OPTIIF:               ; 56
    cmp.l   #$488048c0,-4(a4)    ; ext.w ext.l
    bne     .2
    moveq   #4,d0
    subq.l  #4,a4
    bra     .dosth
.2: cmp.w   #$49c0,-2(a4)        ; extb.l
    bne     .4 ;.3
    moveq   #2,d0
    subq.l  #2,a4
    bra     .dosth

.4:
    MOVE.L  (A2)+,(A4)+
    move.w  (a2)+,(A4)+
    bra JOBLOOP

.dosth:
    move.w  -2(a4),d1      ; check for
    and.l   #$f0ff,d1      ; Scc D0
    cmp.w   #$50c0,d1      ; found one?
    beq     .dosth2
    add.l   d0,a4
    bra     .4
.dosth2:
;    move.w  -(a4),d1       ; skip back..
;    cmp.w   #$56c0,d1      ; do we have a SNE D0?
;    bne     .skp           ;
;    cmp.w   #$4a80,-2(a4)  ; check for TST.L D0...
;    bne     .skp           ;
;    subq.l  #2,a4          ; skip..?
.skp:
   move.w   -(a4),d1
    and.l   #$0f00,d1
    or.l    #$6000,d1
    eori.w  #$100,d1
    move.w  d1,(a4)+
    clr.w   (a4)+
    addq.l  #6,a2
    bra     JOBLOOP

.TC1:
    move.l  -4(a4),d0
    tst.l   -4(a4)
    beq     .TC1
;*-*
;; 57
DOELSECH:             ; 57
    MOVE.W  #1,ELSECHECK
    BRA JOBLOOP
;*-*
;; 58               RETURN
DORETURN:             ; 58
    TST.L   HANDLERPEA
    BEQ.S   .2
    TST.L   ENDPEA
    BNE.S   .2
    LEA .3(PC),A0
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
.2: TST.W   PROCF
    BEQ ERROR9
    MOVE.L  .1(PC),(A4)+
    MOVE.L  PROCEND(PC),D0
    CLR.W   NEWOP
    BSR ADDBRANCH
    BRA JOBLOOP
.1: BRA DORETURN
.3: MOVE.L  (A7)+,-88(A4)       ; (A5) ; ook: EXCEPT/Raise()
    MOVE.L  (A7)+,-76(A4)       ; PUT PREVIOUS HANDLER BACK (CODE)
    MOVE.L  (A7)+,-80(A4)       ; (STACK)
;*-*
;; 59
MAKEBRANCHL:          ; 59, like 8
    MOVE.L  LABPRES,D0
    MOVE.W  .1(PC),NEWOP
    BSR ADDBRANCH
    BRA JOBLOOP
.1: JMP .1
;*-*
;; 60
JOBINSIF:             ; 60
    TST.W   EXPRECC
    BNE.S   .1
    BSR.W   DOINSREC
    BRA JOBLOOP
.1: BSR EXP
    BRA JOBLOOP
;*-*
;; 61               IF
JOBEXPIF:             ; 61
    MOVE.L  LABPRES(PC),-(A7)
    BSR EXP
    MOVE.L  (A7)+,LABPRES
    BRA JOBLOOP
;*-*
;; 62               STEP
JOBFORSTEP:           ; 62
    move.l  -(a5),a1
    move.l  a1,.ID
    move.w  .CD(pc),(a4)+
    jsr     GVA1D2_0
    move.l  -(a5),d0
    move.l  .CD2,(a4)+
    bsr     ADDBRANCH
    move.w  .CD3(pc),(a4)+
    move.l  .ID,a1
    jsr     GVA1D2_9
    move.l  .CD4(PC),(a4)+
    move.l  -(a5),d0
    jsr     ADDBRANCH
    bra     JOBLOOP

.ID:dc.l    0
.CD:move.l  12(a5),d0
.CD2:add.l  .CD2(pc),d0
.CD3:move.l d0,12(a5)
.CD4:bra    .CD4
;*-*
;; 63
HANDLEPROC:           ; 63
    CLR.L   ENDPEA
    CMP.W   #IOFF+56,(A3)
    BNE.S   .2
    ADDQ.L  #2,A3
    MOVE.W  #-1,HANDLEFLAG
    MOVE.L  .3(PC),(A4)+
    BRA JOBLOOP
.2: CLR.L   HANDLERPEA
    BRA JOBLOOP
.3: MOVEM.L D3-D7,-(A7)

HANDLEPROCREALLY:
    LEA .1(PC),A0
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  A4,HANDLERPEA
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    RTS
.1: MOVE.L  -80(A4),-(A7)       ; STACK
    MOVE.L  -76(A4),-(A7)       ; CODE
    MOVE.L  -88(A4),-(A7)       ;
    MOVE.L  A7,-80(A4)
    LEA .1(PC),A0       ; TOTAL SIZE=5*.L, OFF=14
    MOVE.L  A0,-76(A4)
    MOVE.L  A5,-88(A4)

HANDLERPEA: DC.L    0
ENDPEA:     DC.L    0
;*-*
;; 64               EXCEPT
DOEXCEPT:             ; 64
    MOVEQ   #0,D1           ; D1=DOFLAG
    CMP.W   #IOFF+28,(A3)
    BNE.S   .1
    MOVEQ   #1,D1
    ADDQ.L  #2,A3
.1: LEA EREST(PC),A0
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    TST.L   D1
    BEQ.S   .2
    ADDQ.L  #4,A0           ; TAKE THE CLR INSTEAD OF BRA
.2: MOVE.L  (A0)+,(A4)+
    MOVE.L  A4,ENDPEA
    MOVE.L  HANDLERPEA,D0
    BEQ ERROR47
    MOVE.L  D0,A0
    SUB.L   A4,D0
    NEG.L   D0
    ADDQ.L  #2,D0
    MOVE.W  D0,-2(A0)
    BRA JOBLOOP
EREST:  MOVE.L  (A7)+,-88(A4)       ; (A5) ; ook: RETURN/Raise()
    MOVE.L  (A7)+,-76(A4)       ; PUT PREVIOUS HANDLER BACK (CODE)
    MOVE.L  (A7)+,-80(A4)       ; (STACK)
    BRA EREST           ; TO ENDPROC
    CLR.L   -84(A4)
;*-*
;; 65               ENDPROC [HANDLE]
CLOSEHANDLER:         ; 65
    TST.L   HANDLERPEA
    BEQ.S   .1
    MOVE.L  ENDPEA,D0
    BEQ ERROR47
    MOVE.L  D0,A0
    CMP.W   #-84,-2(A0)     ; CLR INSTEAD OF BRA
    BEQ.S   .1
    SUB.L   A4,D0
    NEG.L   D0
    ADDQ.L  #2,D0
    MOVE.W  D0,-2(A0)
.1: BRA JOBLOOP
;*-*
;; 66               PUSHTYPE
PUSHTYPE:             ; 66
    MOVE.W  TYPELAB(PC),(A5)+
    BRA JOBLOOP
;*-*
;; 67               POPTYPE
POPTYPE:              ; 67
    MOVE.W  -(A5),TYPELAB
    BRA JOBLOOP
;*-*
;; 68               RTS/RTD
RTSRTD:               ; 68
    CMP.W   #IOFF+78,(A3)
    BEQ .WITH
    MOVE.W  NRARG(PC),D0
    BEQ.S   .RTS
    MOVE.W  .RTD(PC),(A4)+
    LSL.W   #2,D0
    MOVE.W  D0,(A4)+
    BRA JOBLOOP
.RTS:   MOVE.W  PROCRTS(PC),(A4)+
    BRA JOBLOOP
.RTD:   RTD #4
.WITH:
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,D0
    CMP.W   #$100+47,D0
    BEQ .RTR
    CMP.W   #$100+48,D0
    BNE ERROR90
    MOVE.W  .1(PC),(A4)+
    TST.W   (A3)+
    BNE ERROR0
    BRA JOBLOOP
.RTR:
    MOVE.W  .2(PC),(A4)+
    TST.W   (A3)+
    BNE ERROR0
    BRA JOBLOOP
.1: RTE
.2: RTR
.3:
;*-*
;; 69
DONEW:                ; 69
    ;SUBQ.L #2,A3
    MOVEQ   #1,D0
    BSR NEWEXP
    BRA JOBLOOP
;*-*
;; 70               RETURN / PROC xx IS
EXPECTRETURNIS:           ; 70
    MOVE.W  (A3),D0
    CMP.W   #IOFF+51,D0
    BEQ.S   .1
    CMP.W   #IOFF+62,D0
    BEQ.S   .1
.2: TST.W   (A2)+
    BNE.S   .2
    SUBQ.L  #2,A2
    BRA JOBLOOP
.1: ADDQ.L  #2,A3
    BRA JOBLOOP


BEGL2:  MACRO
    CMP.W   #21,(A3)+
    BNE ERROR12

    ;MOVE.W LINENUM(PC),-(A7)

    MOVE.W  (A3)+,LINENUM
    MOVE.L  A3,INTERMED

    ;TST.L  LINEBUF
    ;BEQ.S  .ALB
    ;CMP.W  LINENUM(PC),(A7)
    ;BEQ.S  .ALB
    ;BSR    ADDLINEDBG
;.ALB:  ADDQ.L  #2,A7

    ENDM

BEGL1:  MACRO
    TST.W   (A3)+
    BNE ERROR12
    ENDM

BEGL:   MACRO
    BEGL1
    BEGL2
    ENDM

BLOCK:  MACRO               ; \1=ID, \2=IOFF1, \3=IOFF2, \4=IOFF3
    MOVEM.L D5-D7/A6,-(A7)
    BEGL1
.CCCL:  BSR WRITELINENUM        ; TRASHES D7
    BEGL2
    MOVE.W  (A3)+,D7
    CMP.W   \2,D7
    BEQ .CCCC
    CMP.W   \3,D7
    BEQ .CCCC
    CMP.W   \4,D7
    BEQ .CCCC
    BRA.S   .CCCN
.CCCC:  CMP.W   \1,-2(A5)
    BNE.S   .CCCN
    SUBQ.L  #2,A3
    BRA.S   .CCCO
.CCCN:  BSR DOINSMAIN
    MOVE.L  A4,CURACODE
    JSR CHECK3
    BRA.S   .CCCL
.CCCO:  MOVEM.L (A7)+,D5-D7/A6
    ENDM

BLOCK2: MACRO               ; note: same as BLOCK, without 3x check
    MOVEM.L D5-D7/A6,-(A7)
    BEGL1
.CCCL:  BSR WRITELINENUM        ; TRASHES D7
    BEGL2
    MOVE.W  (A3)+,D7
    CMP.W   \2,D7
    BEQ .CCCC
    BRA.S   .CCCN
.CCCC:  CMP.W   \1,-2(A5)
    BNE.S   .CCCN
    SUBQ.L  #2,A3
    BRA.S   .CCCO
.CCCN:  BSR DOINSMAIN
    MOVE.L  A4,CURACODE
    JSR CHECK3
    BRA.S   .CCCL
.CCCO:  MOVEM.L (A7)+,D5-D7/A6
    ENDM

RANGC:  MACRO               ; \1=DX
    TST.L   \1
    BMI ERROR31
    CMP.L   D7,\1
    BPL ERROR31
    ENDM

PUTRA:  MACRO               ; \1=DX,\2=TRASH.DX
    MOVE.L  \1,\2
    LSL.L   #1,\2
    TST.W   0(A6,\2.L)
    BNE ERROR50
    MOVE.W  D0,0(A6,\2.L)
    ENDM
;*-*
;; 71               SELECT OF
SELECTOF:             ; 71
    BSR     ASM_GRABVALUE
    TST.L   D0
    BMI JOBLOOP
    MOVE.L  D1,D7
;    CMP.W   #VALUE,(A3)
;    BNE JOBLOOP
;    ADDQ.L  #2,A3
;    MOVE.L  (A3)+,D7        ; D7=RANGE
    CMP.L   #1,D7
    BMI ERROR31
    CMP.L   #1024,D7
    BPL ERROR31
    CMP.W   #IOFF+45,(A3)+
    BNE ERROR0
    MOVE.L  D7,-(A7)
    BSR EXP
    MOVE.L  (A7)+,D7
    BSR NEWLABEL
    MOVE.L  D0,D5           ; D5=ENDLABEL
    BSR NEWLABEL
    MOVE.L  D0,D6           ; D6=DEFAULTLABEL
    MOVE.L  .C1(PC),(A4)+
    CLR.W   (A4)+
    CLR.W   NEWOP
    BSR ADDBRANCH
    MOVE.W  .C2(PC),(A4)+
    MOVE.L  D7,(A4)+
    MOVE.L  .C3(PC),(A4)+
    MOVE.L  D6,D0
    BSR ADDBRANCH
    MOVE.W  .C4(PC),(A4)+
    MOVE.L  .C5A(PC),(A4)+
    MOVE.L  .C5B(PC),(A4)+
    MOVE.L  A4,A6           ; A6=TABLESTART
    MOVE.L  D7,D0
    SUBQ.L  #1,D0
    MOVEQ   #0,D1
.FILL:  MOVE.W  D1,(A4)+
    DBRA    D0,.FILL
    BEGL
    MOVE.W  #8,(A5)+        ; SELECT_OF IDENT
.XL:MOVE.W  (A3)+,D0        ; PARSE ANY #OF CASE'S UNTIL DEF/ENDSEL
    CMP.W   #IOFF+11,D0
    BEQ .DEFA
    CMP.W   #IOFF+12,D0
    BEQ .ENDS
    CMP.W   #IOFF+10,D0
    BNE ERROR0
    MOVE.L  A4,D0
    SUB.L   A6,D0           ; D0=CURRENT OFFSET
    BRA.S   .RST
.RANGL: MOVE.L  (A3)+,D1        ; D1=1STVALUE
    CMP.W   #IOFF+39,(A3)
    BEQ.S   .RR
    RANGC   D1
    PUTRA   D1,D2
    BRA.S   .RNEXT
.RR:    ADDQ.L  #2,A3
    CMP.W   #VALUE,(A3)+
    BNE ERROR30
    MOVE.L  (A3)+,D2        ; D2=ENDVALUE
    RANGC   D1
    RANGC   D2
    CMP.W   D1,D2
    BMI ERROR50
.TOL:   PUTRA   D1,D3
    ADDQ.L  #1,D1
    CMP.L   D2,D1
    BLE.S   .TOL
.RNEXT: CMP.W   #COM,(A3)
    BNE.S   .RDONE
    ADDQ.L  #2,A3
.RST:   CMP.W   #VALUE,(A3)+
    BNE ERROR30
    BRA.S   .RANGL
.RDONE: BLOCK   #8,#IOFF+10,#IOFF+11,#IOFF+12
    MOVE.L  .C6(PC),(A4)+
    CLR.W   NEWOP
    MOVE.L  D5,D0
    BSR ADDBRANCH
    BRA .XL
.DEFA:  BSR .FDEF
    BSR AAAAAA
    ADDQ.L  #2,A3
    BRA.S   .ENDS2
.ENDS:  BSR .FDEF
.ENDS2: BSR STOPJOB
    MOVE.L  D5,D0
    BSR ADDLABEL
    CMP.W   #8,-(A5)
    BNE ERROR9
    BRA JOBLOOP
.FDEF:  MOVE.L  D6,D0
    BSR ADDLABEL
    MOVE.L  A4,D0
    SUB.L   A6,D0
    MOVE.L  A6,A0
    MOVE.L  D7,D1
    SUBQ.L  #1,D1
.FDL:   TST.W   (A0)+
    BNE.S   .NEXT
    MOVE.W  D0,-2(A0)
.NEXT:  DBRA    D1,.FDL
    RTS
.C1:    TST.L   D0
    BMI .C1
.C2:    CMP.L   #1,D0
.C3:    BPL .C1
.C4:    LSL.L   #1,D0
.C5A:   MOVE.W  .C6(PC,D0.L),D0
.C5B:   JMP .C6(PC,D0.W)
.C6:    ; TABLE START
    BRA .C2
AAAAAA: BLOCK2  #8,#IOFF+12
    RTS
;*-*
;; 72               OF object
EXPECTOFOBJECT:           ; 72
    cmp.w   #IOFF+93,(a3)
    bne     .1
    addq.l  #2,a3
.1: CMP.W   #IOFF+45,(A3)
    BNE JOBLOOP
    ADDQ.L  #2,A3           ; of_object
    CMP.W   #31,(A3)+
    BNE ERROR40
    MOVE.L  (A3)+,A0        ; A0=OBJ
    MOVE.L  COMPILEPROC(PC),A6
    MOVE.L  22(A6),A1
    BTST    #1,9(A1)
    BNE ERROR55
    MOVE.L  18(A6),A6       ; A6=SELF
    MOVE.L  A0,(A6)         ; SET SELF AS PTR TO OBJ
    MOVE.W  .CODE(PC),(A4)+
    JSR GVA6D0_9
    BRA JOBLOOP
.CODE:  MOVE.L  A0,2(A5)
;*-*
;; 73
SAVEREGSPROC:         ; 73
    MOVE.L  COMPILEPROC(PC),A0  ; COMPUTE MOVEMMASK
    MOVE.B  3(A0),D0
    BEQ.S   .NR
    EXT.W   D0
    SUBQ.W  #1,D0
    MOVEQ   #7,D1
    MOVEQ   #0,D2
.XL:BSET    D1,D2
    SUBQ.W  #1,D1
    DBRA    D0,.XL
    MOVE.W  D2,PROCMASK
    SAVER   #-1,D0
    MOVE.W  D0,PROCMASKREV
.NR:    BRA JOBLOOP
;*-*
;; 74               END
DOEND:                ; 74
    MOVE.W  #-1,.ISM
    CMP.W   #IDENT,(A3)+
    BNE ERROR6
    MOVE.L  (A3)+,A0        ; A0=IDENT
    TST.B   4(A0)
    BEQ ERROR22
    CMP.B   #LAB,4(A0)
    BEQ ERROR6

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
.CON:
    MOVE.W  .ST2(PC),(A4)+
    JSR GVA0D1_0
    MOVE.W  .ST3(PC),(A4)+
    MOVE.W  .ISM(PC),(A4)+
    BRA.S   .XB
.0:
    MOVE.W  .ST(PC),(A4)+
    JSR GVA0D1_0
.XB:MOVE.L  A4,.BEQS
    MOVE.L  .BEQ(PC),(A4)+

    MOVE.L  A6,D1
    BEQ.S   .ND
    BSR DESTR
.ND:
    CMP.W   #29,(A3)
    BNE.S   .1
    TST.L   D0
    BMI ERROR50
    ADDQ.L  #6,A3           ; ALSO SKIP ]OFFSET
    MOVEM.L A0/D0/A6,-(A7)      ; END p[exp]
    BSR EXP
    MOVEM.L (A7)+,A0/D0/A6
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
.1: TST.L   D0
    BMI.S   .1B
    MOVE.W  .S2(PC),(A4)+       ; just END p
    MOVE.W  D0,(A4)+
    BRA.W   .2
.1B:    ;MOVE.W .S1(PC),(A4)+
.2: MOVE.L  A0,-(A7)
    MOVEQ   #109+10,D0      ; D0=FASTDISPOSE
    MOVE.B  #-1,EFUNCBYTE+109   ; fastdispose=110
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
.CC:    MOVE.L  (A7)+,A0
    MOVE.W  .A(PC),(A4)+
    BSR .NUKEV
    MOVE.L  .BEQS(PC),A1
    MOVE.L  A4,D0
    SUB.L   A1,D0
    SUBQ.L  #2,D0
    MOVE.B  D0,1(A1)
    CMP.W   #COM,(A3)+
    BEQ DOEND
    SUBQ.L  #2,A3
    BRA JOBLOOP
.ISM:   DC.W    0
.M: MULU    #6,D0
.LSL1:  LSL.L   #1,D0
.LSL2:  LSL.L   #2,D0
.S1:    MOVE.L  D0,-(A7)
.S2:    PEA 1.W
.A: ADDQ.L  #8,A7
.5: JSR .M
.4: BSR.W   .M
.ST:    MOVE.L  2(A5),D0
.ST2:   MOVE.L  2(A5),A0
.ST3:   MOVE.L  4(A0),D0
.BEQ:   BEQ.S   .A
    MOVE.L  D0,-(A7)
.BEQS:  DC.L    0

.NUKEV: MOVE.W  .ISM(PC),D0
    BPL.S   .INUKE
    MOVE.W  .NUKE(PC),(A4)+
    JMP GVA0D1_0
.INUKE: MOVE.W  .NUKE2(PC),(A4)+
    JSR GVA0D1_0
    MOVE.W  .NUKE3(PC),(A4)+
    MOVE.W  D0,(A4)+
    RTS
.NUKE:  CLR.L   2(A5)
.NUKE2: MOVE.L  2(A5),A0
.NUKE3: CLR.L   4(A0)

DESTR:  MOVE.W  ODESTR(A6),D2       ; GETS OBJ=A6, OSIZE=D0
    BMI.W   .XX         ; D2=ENDOFF
    MOVE.W  .7(PC),(A4)+
    BSR.S   .GEND
    MOVE.W  .6A(PC),(A4)+
    MOVE.W  D2,(A4)+
    MOVE.W  .6B(PC),(A4)+
.XX:    TST.W   ODEL(A6)
    BEQ.S   .XXX
    MOVE.W  .GS(PC),(A4)+
    BSR.S   .GEND
    MOVE.W  .GS2(PC),(A4)+
    MOVEQ   #-1,D0          ; SIGNAL SIZE ALREADY on (A7)
.XXX:   RTS
.GS:    MOVE.L  (A7),A0
.GS2:   MOVE.L  (A1),-(A7)
.7: MOVEA.L D0,A0
.6: MOVE.L  (A0),A1
.6X:    MOVE.L  2(A0),A1
.6A:    MOVE.L  4(A1),A1
.6B:    JSR (A1)
.GEND:  TST.W   ODELOFF(A6)
    BMI ERROR71
    BHI .U
    MOVE.W  .6(PC),(A4)+
    RTS
.U: MOVE.W  .6X(PC),(A4)+
    MOVE.W  ODELOFF(A6),(A4)+
    RTS
;*-*
;; 75               do EXIT
; GETS BRANCH TO FILL AT -2(A4)

FILLEXIT:             ; 75
    MOVE.W  -2(A5),D0
    CMP.W   #3,D0
    BNE.S   .1
    MOVE.L  -10(A5),D0
    BRA.S   .D
.1:
    CMP.W   #4,D0
    BNE ERROR0
    MOVE.L  -4-4-4-4-2(a5),d0
.D: BCHG    #0,-4(A4)       ; negate CC
    CLR.W   NEWOP
    BSR ADDBRANCH
    BRA JOBLOOP
;*-*
;; 76               do SUPER
DOSUPER:              ; 76
    BSR SUPEREXP
    BRA JOBLOOP
;*-*
;; 77               Fix stack
FIXUPSTACK:
    MOVE.L  FIXUPSTACKREGS,D0
    BEQ .X
    ADDQ.L  #1,D0
    LSL.L   #2,D0
    MOVE.W  .c,(A4)+
    MOVE.W  D0,(A4)+
    MOVE.L  .r(PC),(A4)+
.X:
    BRA JOBLOOP
.c: LEA 0(A7),A7
.r: MOVEM.L (A7)+,D2-D7/A2-A6
;*-*
;; 78               Fix FOR stack
FIXFORSTACK:
   RTS
FIXFORSTACKSIZE:
    DC.W    0
;*-*
;; 79               Section
JOB_SECTION:
    BTST        #2,CODEPREFS+3
    BEQ         ERROR44
    MOVE.L      HunkList,A0
    MOVE.L      A4,H_END(A0)
    move.l      a0,d0

    CMP.W   #IOFF+84,(A3)+
    BNE     .1
    MOVE.L  #$3E9,D1
    BRA     .X
.1: CMP.W   #IOFF+85,-2(A3)
    BNE     ERROR97
    MOVE.L  #$3EA,D1
.X: CMP.W   #COM,(A3)
    BNE     .XX
    ADDQ.L  #2,A3
    CMP.W   #IOFF+86,(A3)+
    BNE     .X2
    bset    #30,d1
    BRA     .XX
.X2:CMP.W   #IOFF+87,-2(A3)
    BNE     ERROR97
    BSET    #31,D1
.XX:
    GETM        A0
    MOVE.L      HunkList(PC),(A0)+
    MOVE.L      A0,HunkList
    exg.L       A0,D0
    move.l      d0,H_PREV(A0)
    exg.l       a0,d0
    MOVE.L      A4,(A0)+
    move.l      D1,(a0)+
    clr.l       (a0)+
    clr.l       (a0)+
    clr.l       (a0)+
    DONEM       A0
    ADDQ.L      #1,NumHunks
    bra         JOBLOOP
;*-*
;; 80               Automatic procedure
JOB_AUTOPROC:
    CMP.W   #IOFF+90,(A3)
    BMI     JOBLOOP
    CMP.W   #IOFF+91,(A3)
    BGT     JOBLOOP
    TSTMOD
    BEQ     ERROR49
    MOVE.L  COMPILEPROC(PC),A0
    TST.W   (A0)
    BNE     ERROR23
    BTST    #1,2(A0)
    BNE     ERROR39

    MOVEQ   #0,D0
    MOVE.W  (A3)+,D0
    SUB.W   #IOFF+90-6,D0
    BSET    D0,2(A0)
    MOVE.L  14(A0),A0
    BSET    #2,5(A0)
    BRA JOBLOOP
;*-*
;; 81               optimize cache line
JOB_OPTCACHE:
    bsr     OPTCACHELINE
    bra     JOBLOOP
;*-*
;*-*

JOBROUTTAB:
    DC.L    JOBOUT,GETPARAMLABEL,JOBEXP,GETPARAMVAR,GETCOMMA; 0
    DC.L    SAVESTIDENT,FORWARDREFERENCE,CHECKSTIDENT       ; 5
    DC.L    MAKEBRANCH,COPY2,COPY4,COPYN,COPY6,COPY8        ; 8
    DC.L    COPY10,REMEMBERLAB,EXPORNILL,PROCFLAGON         ; 14
    DC.L    PROCFLAGOFF,CHECK2LAB,REMINDCURPOS,POPLAB       ; 18
    DC.L    INSERTVAROFF,PUSHVAR,POPVAR,PUSHLAB             ; 22
    DC.L    PUSHLAB2,POPLAB2,NEWLAB2,MAKEBRANCH2            ; 26
    DC.L    SETFLAG,SKIPFLAG,CLEARFLAG,SKIPDEFLOCAL,GETNRLOC; 30
    DC.L    INSERTVAROFFDEST,GETBRACKLEFT,GETBRACKRIGHT     ; 35
    DC.L    DOINCBIN,DOLONG,DOINT,DOCHAR,JOBINS,EXPECT      ; 38
    DC.L    SERROR,EXPECT2,JOBSKIP,EXPECTASSIGN,EXPECTTO    ; 44
    DC.L    DOMIDFOR,ENDGLOBVAR,ELSEMAKELAB,ELSEUSELAB      ; 49
    DC.L    ELSEDEFLAB,ELSEWAISTELAB,DOOBJECT,OPTIIF        ; 53
    DC.L    DOELSECH,DORETURN,MAKEBRANCHL,JOBINSIF,JOBEXPIF ; 57
    DC.L    JOBFORSTEP,HANDLEPROC,DOEXCEPT,CLOSEHANDLER     ; 62
    DC.L    PUSHTYPE,POPTYPE,RTSRTD,DONEW,EXPECTRETURNIS    ; 66
    DC.L    SELECTOF,EXPECTOFOBJECT,SAVEREGSPROC,DOEND      ; 71
    DC.L    FILLEXIT,DOSUPER,FIXUPSTACK,FIXFORSTACK         ; 75
    DC.L    JOB_SECTION,JOB_AUTOPROC,JOB_OPTCACHE,JOBOUT,JOBOUT          ; 79

