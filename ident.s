;;IDENT

PARSEGETIDENTNR:          ; GETS IDENTNR OUTOF WORKBUF
    TST.L   OBJECTD
    BNE OBJECTIDENT

    MOVE.L  CPROC(PC),D3
    MOVEQ   #0,D5
    MOVEQ   #UNDEF,D7
    MOVE.W  CURINS(PC),D6       ; INS-ioff

    CMP.W   #35,D4          ; .
    BEQ .1
    CMP.W   #IOFF+39,D4     ; TO
    BEQ.S   .5
    CMP.W   #19,D4          ; :
    BEQ.S   .4
    CMP.W   #IOFF+45,D4     ; OF
    BEQ.S   .2
    CMP.W   #IOFF+50,D4     ; SIZEOF
    BEQ.S   .1
    CMP.W   #41,D4          ; ::
    BEQ.S   .1
    BRA.S   XNEXT
.5: CMP.W   #IOFF+44,-4(A1)     ; PTR TO
    BNE.S   XNEXT
    BRA OBJUSE
.2: CMP.W   #IOFF+41,-4(A1)     ; ARRAY OF
    BEQ.S   .1
    CMP.W   #18,-4(A1)      ; ) OF
    BNE.S   XNEXT
    CMP.W   #1,-8(A1)       ; SELECT 1 OF (ERG DIRTY!)
    BEQ.S   .XTEST
.XBACK:
.1: BRA OBJUSE

.XTEST: CMP.W   #IOFF+8,-10(A1)
    BEQ.S   XNEXT
    BRA.S   .XBACK

.4: CMP.W   #-1,D6
    BNE.S   .1
    CMP.W   #30,-4(A1)
    BEQ.S   .1
XNEXT:

    TST.W   D6          ; "PROC"=0
    BNE.S   .1
        CMP.W   #IOFF,D4        ;-2(A1) ; PROCNAME OR ARGVAR ?
        BEQ.S   .2
    MOVEQ   #LOCV,D7
    BRA.S   .5
.2: MOVEQ   #LAB,D7
    BSR NEWPROC
    BRA.S   .5
.1: CMP.W   #16,D6
    BNE.S   .3
    MOVEQ   #GLOBV,D7
    BRA.S   .5
.3: CMP.W   #17,D6
    BNE.S   .4
    MOVEQ   #LOCV,D7
    BRA.S   .5
.4: CMP.B   #':',(A0)       ; DIRTY
    BNE.S   .5
    CMP.B   #'=',1(A0)
    BEQ.S   .5
    CMP.B   #':',1(A0)
    BEQ.S   .5
    MOVEQ   #LAB,D7
.5: LEA IDENTHASH,A3
    MOVE.L  HASHVAL,D1
    LSL.L   #2,D1
    ADD.L   D1,A3
    MOVE.L  A3,IDHASHADR
    ADDQ.L  #4,A3
    MOVE.L  WORK(PC),D2     ; D2,D1=BACKUP
IDCHECKLOOP:
    MOVE.L  -(A3),A3
    MOVE.L  A3,D1
    BEQ UNIDENT
    MOVE.L  A3,A5
    MOVE.L  (A3),A4
    MOVE.L  D2,A6
.1: CMPM.B  (A4)+,(A6)+
    BNE.S   IDCHECKLOOP
    TST.B   -1(A4)
    BNE.S   .1
    CMPI.B  #LOCV,4(A5)
    BNE.S   .2
    MOVE.L  6(A5),D0        ; check if proc is ok
    CMP.L   CPROC(PC),D0
    BNE.S   IDCHECKLOOP
    BRA.S   .3
.2: CMPI.B  #LAB,4(A5)      ; check if redef loc/glob > lab
    BNE.S   .3
    CMP.W   #LOCV,D7
    BEQ.S   IDCHECKLOOP
    CMP.W   #GLOBV,D7
    BEQ.S   IDCHECKLOOP
.3: MOVE.L  A5,D5
    CMPI.B  #GLOBV,4(A5)        ; check if locv > globv
    BEQ.S   IDCHECKLOOP

; HIERNA D4 TIJDLK. GETRASHED

IDALREADYPRESENT:
    TST.W   D7          ; USE IN EXP
    BEQ.W   .X2                     ; !!!!!!!!!!!!! WAS .X
    MOVE.B  4(A5),D4        ; USE IN 'LOCAL'
    CMP.B   #LOCV,D7
    BNE.S   .10
    TST.B   D4          ; var has type?
    BNE.S   .2
    MOVE.L  6(A5),D0
    CMP.L   D3,D0
    BNE.S   .X
    MOVE.B  #LOCV,4(A5)     ; no: local
    BRA.S   .X
.2: CMP.B   #GLOBV,D4
    BEQ UNIDENT2
    CMP.B   #LOCV,D4
    BNE iERROR21         ; LAB AS LOC OR LOC*2
    CMP.L   6(A5),D3
    BEQ iERROR21         ; same local name in other proc
    BRA UNIDENT2
.10:CMP.B   #LAB,D7         ; USE IN 'DEF'
    BEQ.S   .20
    CMP.B   #LOCV,D4
    BEQ UNIDENT2
    BTST    #2,5(A5)
    BNE.S   .12
    TST.B   D4
    BNE iERROR21
    BRA.S   .12B
.12:    TST.W   EXPORTFLAG      ; allow bases to be redefined nonexport
    BNE.S   .12B
    BCLR    #2,5(A5)
.12B:   MOVE.B  #GLOBV,4(A5)
    BRA.S   .X
.20:CMP.B   #":",(A0)       ; ONLY TEST LABELS, NOT PROCS/METHODS
    BNE.S   .MH
    CMP.B   #LAB,D4             ; OK THIS TEST?
    BEQ iERROR14
.MH:TST.B   D4                  ; USE IN 'PROC' OR ':'
    BNE UNIDENT3
    MOVE.B  #LAB,4(A5)
    MOVE.W  #-1,10(A5)
.X: MOVE.L  D3,6(A5)
.X2:    MOVE.L  D4,D2
    MOVEQ   #IDENT,D4
    MOVE.W  D4,(A1)+
    MOVE.L  D5,(A1)+
    TST.W   MAXREGALLOC
    BEQ.S   .NR
    CMP.B   #LOCV,4(A5)
    BNE.S   .NR
    CMP.W   #23,D2          ; used in {}
    BEQ.S   .NR2
    TST.L   VARHEAVY(A5)
    BEQ.S   .NR
    MOVE.L  HEAVYNESS(PC),D0
    ADD.L   D0,VARHEAVY(A5)
.NR:    TST.B   D7
    BEQ.S   .S
    TST.W   EXPORTFLAG
    BEQ.S   .NOEXP
    BSET    #0,5(A5)        ; USED IF DEF+EXPORT
.NOEXP: RTS
.S: BSET    #0,5(A5)        ; SET IF USED
    RTS
.NR2:   CLR.L   VARHEAVY(A5)
    BRA.S   .NR

UNIDENT3:
    TST.L   10(A5)          ; see if objhead in procstruct
    BEQ.W   iERROR21         ; no method!
    MOVE.L  CPROC(PC),A6
    BSET    #1,2(A6)        ; SET METHOD
    BRA.S   UNIDENT2
UNIDENT:
    TST.L   D5
    BNE DOUBLENAME
UNIDENT2:
    GETM    A3          ; A3=FRESH MEM
    MOVE.L  WORK(PC),A4
    MOVE.L  A3,A5           ; A5=ASCIIPTR IN HEAP
.1: MOVE.B  (A4)+,(A3)+
    BNE.S   .1
    MOVE.L  A3,D0
    BTST    #0,D0
    BEQ.S   .OK
    CLR.B   (A3)+
.OK:
    MOVE.L  IDHASHADR,A4
    MOVE.L  (A4),(A3)+
    MOVE.L  A3,(A4)
    MOVEQ   #IDENT,D4
    MOVE.W  D4,(A1)+
    MOVE.L  A3,(A1)+
    MOVE.L  A3,LASTID
    MOVE.L  A3,A6           ; A6=BASE ID
    MOVE.L  A5,(A3)+
    MOVEQ   #0,D0
    MOVE.B  D7,(A3)+        ; TST
    BEQ .S
    TST.W   EXPORTFLAG
    BNE.S   .SE
    CLR.B   (A3)+
    BRA.S   .S2
.SE:MOVE.B  #4,(A3)+        ; SET EXPORT
    CMP.W   #2,D7
    BEQ.S   .S2
    BSET    #0,-1(A3)       ; +USED
    BRA.S   .S2
.S: MOVE.B  #1,(A3)+        ; SET FLAGS=1 IF USED (D7=0)
                            ; ALS IDENT<>LABEL DAN ERROR22
.S2:MOVE.L  D3,(A3)+
    CMP.B   #LAB,D7
    BEQ.S   .2
    MOVE.W  D0,(A3)+        ; VARS:   OFFSET=0
    CMP.B   #LOCV,D7
    BNE.S   .3          ; SEE IF LOCAL --> HEAVYNESS
    TST.W   MAXREGALLOC
    BEQ.S   .3
    TST.W   REGSTOP
    BNE.S   .2A
    MOVE.L  HEAVYNESS(PC),(A3)+ ; must be offs VARHEAVY
    BRA.S   .3
.2A:CLR.L   (A3)+           ; no regalloc in HANDLEr
    BRA.S   .3
.2: MOVE.W  #-1,(A3)+       ; LABELS: OFFSET=-1
.3:
    CMP.L   #"main",(A5)
    BNE.S   .NM
    TST.B   4(A5)
    BNE.S   .NM
    BSET    #0,5(A6)

.NM:    CMP.L   #"self",(A5)
    BNE.S   .NS
    TST.B   4(A5)
    BNE.S   .NS
    MOVE.L  A6,CURSELF
.NS:MOVE.L  #0,(A3)+        ; 0 = DEF long
    MOVE.L  #0,(A3)+
    DONEM   A3
    RTS
DOUBLENAME:
    MOVE.L  D5,A5
    BRA IDALREADYPRESENT

LASTID:       DC.L    0
IDHASHADR:    DC.L    0
CURSELF:      DC.L    0
SELFHASHADR:  DC.L    0
SELFNAME:     DC.B    "self",0,0

NEWPROC:
    GETM    A3
    MOVE.L  PROCLIST(PC),(A3)+
    MOVE.L  A3,PROCLIST
    MOVE.L  A3,CPROC
    MOVE.L  A3,D3
    CLR.L   (A3)+
    BTST    #0,CODEPREFS+1
    BEQ.S   .1
    TSTMOD
    BNE.S   .1
    BSET    #0,-2(A3)       ; USE RTD
.1: CLR.W   (A3)+           ; FIRST 10 BYTES IN USE
    CLR.L   (A3)+           ; DEFARGS
    CLR.L   (A3)+           ; of_object
    CLR.L   (A3)+           ; IDENT
    CLR.L   (A3)+           ; SELF
    CLR.L   (A3)+           ; METHOD
    CLR.L   (A3)+           ; if regalloc then table
    DONEM   A3
    RTS

PROCLIST:   DC.L    0
CPROC:      DC.L    0
OBJNAME:    DC.L    0

FINDOBJ:              ; D1=ASCII, RET D0=OBJH | NIL
    MOVEM.L A3/A4/A5,-(A7)
    LEA OLIST+4(PC),A3
.XL:MOVE.L  ONEXT(A3),A3
    MOVE.L  A3,D0
    BEQ.S   .X
    MOVE.L  D1,A4
    MOVE.L  OASCII(A3),A5       ; ASCII
.L2:    CMPM.B  (A5)+,(A4)+
    BNE.S   .XL
    TST.B   -1(A5)
    BNE.S   .L2
    MOVE.L  A3,D0
.X: MOVEM.L (A7)+,A3/A4/A5
    RTS

OBJUSE:
    CMP.W   #35,D4          ; SEARCHING OBJECTNAME OR FIELD
    BEQ.S   .FIELD
    MOVE.L  WORK(PC),D1     ; D1=ASCIIPTR
    BSR FINDOBJ
    CMP.W   #IOFF+50,D4
    BEQ .000
.001:
    MOVEQ   #31,D4          ; FOUND
    MOVE.W  D4,(A1)+
    MOVE.L  D0,(A1)+
    BEQ iERROR42
    RTS
.000:
    TST.W   D0
    BNE .001
    BRA XNEXT

.FIELD: MOVE.L  WORK(PC),D1     ; IS THIS PART NECESSARY ?
    LEA MEMBERLIST+4(PC),A3
.FL:    MOVE.L  ONEXT(A3),A3
    MOVE.L  A3,D0
    BEQ .MODF
    MOVE.L  D1,A4
    MOVE.L  OASCII(A3),A5
    MOVE.L  A5,D0           ; SKIP IF PRIVATE
    BEQ.S   .FL
.FL2:   CMPM.B  (A5)+,(A4)+
    BNE.S   .FL
    TST.B   -1(A5)
    BNE.S   .FL2
    MOVEQ   #39,D4
    MOVE.W  D4,(A1)+
    MOVE.L  A3,(A1)+
    RTS
.MODF:  MOVE.L  WORK(PC),D1     ; SEARCH IN "MODULE" LIST
    LEA OLIST+4(PC),A3      ; (AFTER OWN OBJECTS!)
.MFL:   MOVE.L  ONEXT(A3),A3
    MOVE.L  A3,D0
    BEQ .UNF
    LEA OMEMB+4(A3),A6
.ML:    MOVE.L  ONEXT(A6),A6
    MOVE.L  A6,D0
    BEQ.S   .MFL
    MOVE.L  D1,A4
    MOVE.L  OASCII(A6),A5
    MOVE.L  A5,D0           ; SKIP IF PRIVATE
    BEQ.S   .ML
.ML2:   CMPM.B  (A5)+,(A4)+
    BNE.S   .ML
    TST.B   -1(A5)
    BNE.S   .ML2
    MOVEQ   #39,D4
    MOVE.W  D4,(A1)+
    MOVE.L  A6,(A1)+
    RTS
.UNF:   MOVEQ   #42,D4          ; NOT FOUND SOFAR
    MOVE.W  D4,(A1)+
    GETM    A3
    MOVE.L  A3,(A1)+
    MOVE.L  WORK(PC),A4
.UFL:   MOVE.B  (A4)+,(A3)+     ; KAN DUS NIET!!!
    BNE.S   .UFL            ; DOUBLE COPY
    MOVE.L  A3,D1
    BTST    #0,D1
    BEQ.S   .UF2
    ADDQ.L  #1,A3
.UF2:   DONEM   A3
    RTS


OLIST:        DC.L    0
MEMBERLIST:   DC.L    0   ; TIJDENS LEX

OBJECTIDENT:          ; definition
    CMP.W   #19,D4          ; 2. bla:INT IPV x:bla
    BEQ.S   .S
    CMP.W   #IOFF+39,D4     ; TO
    BEQ.S   .S
    CMP.W   #IOFF+45,D4     ; OF
    BNE     .NEW
.S: MOVE.L  WORK(PC),A5
    MOVE.L  A5,D5
    LEA OLIST+4(PC),A6      ; A6=LINKED LIST
.XL:MOVE.L  ONEXT(A6),A6
    MOVE.L  A6,D0
    BEQ.S   .MUT
    MOVE.L  OASCII(A6),A4
    MOVE.L  D5,A5
.COMP:  CMPM.B  (A5)+,(A4)+
    BNE.S   .XL
    TST.B   -1(A5)
    BNE.S   .COMP
    CMP.W   #IOFF+48,D4     ; 1. OBJECT <DOUBLEDECL>
    BEQ iERROR43
    MOVEQ   #31,D4          ; FOUND
    MOVE.W  D4,(A1)+
    MOVE.L  A6,(A1)+
    RTS
.MUT:   CMP.W   #IOFF+39,D4     ; ONLY "TO". ILLEGAL = "OF", ":"
    BNE iERROR42
    MOVEQ   #45,D4
    MOVE.W  D4,(A1)+
    GETM    A3
    MOVE.L  A3,(A1)+
    MOVE.L  D5,A5
.MUTL:  MOVE.B  (A5)+,(A3)+
    BNE.S   .MUTL
    MOVE.L  A3,D0
    ADDQ.L  #1,D0
    BCLR    #0,D0
    MOVE.L  D0,A3
    DONEM   A3
    RTS
.NEW:   MOVEQ   #0,D0           ; IF D0=1 THEN OHEAD, 0=MEMBER
    CMP.W   #IOFF+48,D4
    BNE.S   .1
    MOVEQ   #1,D0
.1: GETM    A3
    TST.W   D0
    BNE .5
    TST.L   OBJNAME
    BEQ iERROR39
.3: MOVE.L  WORK(PC),A2
    MOVE.L  A3,D1           ; D1=ASCPTR
.2: MOVE.B  (A2)+,(A3)+
    BNE.S   .2
    MOVE.L  A3,D4
    BTST    #0,D4
    BEQ.S   .OK
    CLR.B   (A3)+
.OK:    MOVE.L  A3,A4           ; STILL ON HEAP
    MOVEQ   #31,D4
    MOVE.W  D4,(A1)+
    ADDQ.L  #4,A4
    MOVE.L  A4,(A1)+
    TST.L   D0
    BEQ.S   .MEM
    MOVE.L  OLIST(PC),ONEXT(A4) ; OBJECTHEAD STRUCT
    MOVE.L  A4,OLIST
    CLR.L   (A4)+           ; DELEGATESIZE+FLAGS
    CLR.L   (A4)+           ; SIZEOF+ID
    MOVE.L  D1,(A4)+        ; ASCII
    CLR.L   (A4)+           ; MEMBERLIST=0
    CLR.L   (A4)+           ; METHODLIST=0
    CLR.L   (A4)+           ; SUPERCLASS=0
    MOVE.L  #1,(A4)+        ; DELEGATECODE=1
    CLR.L   (A4)+           ; ACCESLIST
    MOVE.L  #-1,(A4)+       ; DELOFFSET+DESTRUCTOFF
    BRA.S   .C
.MEM:   MOVEQ   #39,D4
    MOVE.W  D4,-6(A1)
    MOVE.L  MEMBERLIST(PC),ONEXT(A4); (NEXT) MEMBER STRUCT
    MOVE.L  A4,MEMBERLIST
    CLR.L   (A4)+
    CLR.L   (A4)+
    MOVE.L  D1,(A4)+
    CLR.L   (A4)+           ; OPTRTYPE
    CLR.L   (A4)+           ; info (used?)
    CLR.L   (A4)+           ; next (INT!!!)
.C: DONEM   A4
    RTS
.5: MOVE.L  A3,OBJNAME
    MOVE.L  WORK(PC),D1
    BSR FINDOBJ
    TST.L   D0
    BNE iERROR43
    MOVEQ   #1,D0
    BRA .3

PARSEGETVALUE:            ; A2 STRING, =>D0 LONGINT
    TST.B   (A2)+
    BNE.S   PARSEGETVALUE
    SUBQ.L  #1,A2
    MOVEQ   #0,D0           ; RESULT
    CMPA.L  #WORKBUF+11,A2
    BPL iERROR7
    LEA VALUETAB(PC),A3
    MOVEQ   #0,D7
.1: MOVEQ   #0,D3
    MOVE.L  (A3)+,D2
    MOVE.B  -(A2),D3
    SUB.W   #48,D3
    BMI.S   .2          ; DAMN! WERE DOING A FLOAT!
    MOVE.L  D3,D1
    MULU    D2,D3
    SWAP    D2
    MULU    D2,D1
    SWAP    D1
    ADD.L   D1,D3
    ADD.L   D3,D0
    BCS iERROR7
.3: CMPA.L  WORK(PC),A2
    BNE.S   .1
    TST.L   D7          ; NEED TO CONVERT FLOAT?
    BNE.S   .4
    RTS             ; NO
.4: MOVE.L  D0,D6
    MOVEM.L A0/A1,-(A7)
    BSR ISOPENMATH
    MOVE.L  MATHBASE(PC),A6     ; YES
    MOVE.L  D6,D0
    JSR -36(A6)         ; SPFLT
    CMP.L   #1,D7
    BEQ.S   .5
    MOVE.L  D0,D6
    MOVE.L  D7,D0
    JSR -36(A6)
    MOVE.L  D6,D1
    EXG.L   D0,D1
    MOVE.W  #-1,IEEEPROBLEM
    JSR -84(A6)         ; SPDIV
    CLR.W   IEEEPROBLEM
.5: MOVEM.L (A7)+,A0/A1
    RTS
.2: TST.L   D7
    BNE iERROR0
    MOVE.L  D2,D7           ; = FLT FACTOR
    SUBQ.L  #4,A3
    BRA.S   .3

IEEEPROBLEM:  DC.W    0

VALUETAB:
    DC.L    1,10,100,1000,10000,100000,1000000
    DC.L    10000000,100000000,1000000000






PARSESTRING:
    MOVEQ   #STR,D4
    MOVE.W  D4,(A1)+
    ADDQ.L  #4,A1
    MOVE.L  A1,A4
.LOOP:  MOVE.B  (A0)+,D0
    CMP.B   #10,D0
    BEQ.S   .ER
    CMP.B   #'\',D0
    BEQ.S   .SPEC
;    CMP.B   #'%',D0
;    BEQ.S   .PERC
    MOVE.B  D0,(A1)+
    CMP.B   #"'",D0
    BNE.S   .LOOP
    CMP.B   #"'",(A0)
    BEQ.S   .DQUO
    SUBQ.L  #1,A1
    MOVE.L  A1,D1
    MOVE.B  #0,(A1)+
    MOVE.L  A1,D0
    BTST    #0,D0
    BEQ.S   .EQSTR
    MOVE.B  #0,(A1)+
.EQSTR: SUB.L   A4,D1
    MOVE.L  A1,D0
    SUB.L   A4,D0
    ASR.L   #1,D0
    MOVE.W  D0,-2(A4)
    MOVE.W  D1,-4(A4)       ; REAL LEN
    MOVE.L  STRLENADR,A3
    MOVE.L  D1,(A3)
    BRA PARSELOOP
.PERC:  TST.W   XF
    BEQ.S   .SK
    MOVE.B  #"%",(A1)+
.SK:    MOVE.B  #"%",(A1)+
    BRA .LOOP
.DQUO:  ADDQ.L  #1,A0
    BRA .LOOP
.ER:    SUBQ.L  #1,A0
    BRA iERROR8

.SPEC:  MOVE.B  (A0)+,D1
    CMP.B   #'n',D1
    BEQ     .1
    CMP.B   #'a',D1
    BEQ     .2
    CMP.B   #'e',D1
    BEQ     .3
    CMP.B   #'t',D1
    BEQ     .4
    CMP.B   #'\',D1
    BEQ     .5
    CMP.B   #'0',D1
    BEQ     .6
    CMP.B   #'b',D1
    BEQ     .14
    CMP.B   #'d',D1
    BEQ     .7
    CMP.B   #'h',D1
    BEQ     .8
    CMP.B   #'s',D1
    BEQ     .9
    CMP.B   #'z',D1
    BEQ     .10
    CMP.B   #'l',D1
    BEQ     .11
    CMP.B   #'r',D1
    BEQ     .12
    CMP.B   #'c',D1
    BEQ     .13
    CMP.B   #'q',D1
    BEQ     .15
    CMP.B   #'!',D1
    BEQ     .16
    CMP.B   #'v',D1
    BEQ     .17
    CMP.B   #'x',D1
    BEQ     .18
    CMp.B   #"u",D1
    BEQ     .19
    BRA iERROR26
.1: MOVE.B  #10,(A1)+
    BRA .LOOP
.2: MOVE.B  #"'",(A1)+
    BRA .LOOP
.3: MOVE.B  #27,(A1)+
    BRA .LOOP
.4: MOVE.B  #9,(A1)+
    BRA .LOOP
.5: MOVE.B  #"\",(A1)+
    BRA .LOOP
.6: MOVE.B  #0,(A1)+
    BRA .LOOP
.7: MOVE.B  #'%',(A1)+
    BSR     DOFIELD
    MOVE.B  #'l',(A1)+
    MOVE.B  #'d',(A1)+
    BRA .LOOP
.8: MOVE.B  #'%',(A1)+
    BSR     DOFIELD
    MOVE.B  #'l',(A1)+
    MOVE.B  #'x',(A1)+
    BRA .LOOP
.9: MOVE.B  #'%',(A1)+
    BSR     DOFIELD
    MOVE.B  #'s',(A1)+
    BRA .LOOP
.10:    MOVE.W  #-1,FZERO
    BRA .LOOP
.11:    MOVE.W  #-1,FRL
    BRA .LOOP
.12:    MOVE.W  #0,FRL
    BRA .LOOP
.13:    MOVE.B  #'%',(A1)+
    MOVE.B  #'l',(A1)+
    MOVE.B  #'c',(A1)+
    BRA .LOOP
.14:    MOVE.B  #13,(A1)+
    BRA .LOOP
.15:    MOVE.B  #34,(A1)+
    BRA .LOOP
.16:    MOVE.B  #7,(A1)+
    BRA .LOOP
.17:    MOVe.B  #11,(A1)+
    BRA .LOOP
.18:
    MOVEM.L D0/D2,-(A7)
    MOVEQ   #1,D1
    MOVEQ   #0,D2
.18_1:
    LSL.B   #4,D2
    MOVE.B  (A0)+,D0
    CMP.B   #"0",D0
    BLT     iERROR72
    CMP.B   #"9",D0
    BLE     .DEC
    CMP.W   #"A",D0
    BLT     iERROR72
    CMP.W   #"F",D0
    BLE     .HEX
    CMP.W   #"a",D0
    BLT     iERROR72
    CMP.W   #"f",D0
    BGT     iERROR72
.HEX:
    BCLR    #5,D0
    AND.L   #$FF,D0
    SUB.B   #"A",D0
    ADD.B   #10,D0
    BRA     .CNT
.DEC:
    AND.L   #$FF,D0
    SUB.B   #"0",D0
.CNT:
    ADD.W   D0,D2
    DBF     D1,.18_1
    MOVE.B  D2,(A1)+
    MOVEM.L (A7)+,D0/D2
    BRA .LOOP
.19:
    MOVE.B  #"%",(A1)+
    BSR     DOFIELD
    MOVE.W  #"lu",(A1)+
    BRA .LOOP


FZERO:  DC.W    0           ; 0=PADSPACE, -1=PADZERO
FRL:    DC.W    0           ; 0=RIGHT, -1=LEFT
SSTR:   DC.L    0

DOFIELD:
    TST.W   FRL
    BEQ.S   .1
    MOVE.B  #'-',(A1)+
.1: TST.W   FZERO
    BEQ.S   .2
    MOVE.B  #'0',(A1)+
.2: CMP.B   #'(',(A0)
    BNE.S   .5
    MOVE.L  A0,SSTR
    ADDQ.L  #1,A0
.4: MOVE.B  (A0)+,D1
    CMP.B   #')',D1
    BEQ.S   .5
    CMP.B   #',',D1
    BNE.S   .3
    MOVE.B  #'.',(A1)+
    BRA.S   .4
.3: CMP.B   #58,D1
    BPL.S   .F
    CMP.B   #48,D1
    BMI.S   .F
    MOVE.B  D1,(A1)+
    BRA.S   .4
.F: MOVE.L  SSTR(PC),A0
.5: CMP.B   #'[',(A0)
    BEQ.S   .7
.6: MOVE.W  #0,FZERO
    RTS
.7: MOVE.L  WORK(PC),A5
    MOVE.L  A5,D5
    MOVE.L  A0,SSTR
    ADDQ.L  #1,A0
.8: MOVE.B  (A0)+,D1
    CMP.B   #58,D1
    BPL.S   .X
    CMP.B   #48,D1
    BMI.S   .X
    MOVE.B  D1,(A5)+
    BRA.S   .8
.X: CMP.B   #']',D1
    BNE.S   .F2
    CMP.L   D5,A5
    BEQ.S   .6
    MOVE.B  #0,(A5)+
    MOVE.L  D5,A5
.XL:MOVE.B  (A5)+,(A1)+
    BNE.S   .XL
    MOVE.B  #'.',-1(A1)
    MOVE.L  D5,A5
.L2:    MOVE.B  (A5)+,(A1)+
    BNE.S   .L2
    SUBQ.L  #1,A1
    BRA.S   .6
.F2:    MOVE.L  SSTR(PC),A0
    BRA.S   .6
;*-*

