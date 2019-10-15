;; Preprocessor

; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The (Macro) PreProcessor                ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;

; - recursive macros
; - check end_of_source at each "\n" skipped.

;; Macro structure offsets
MAC_NEXT    = 0
MAC_NAME    = 4
MAC_BODY    = 8
MAC_NARGS   = 12
MAC_FLAGS   = 14    ; .B
;*-*
;; Macro characters
PREPCHARS:
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0    ; 0
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0    ; 16
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0    ; 32
    DC.B    4,4,4,4,4,4,4,4, 4,4,0,0,0,0,0,0    ; 48

    DC.B    0,5,5,5,5,5,5,5, 5,5,5,5,5,5,5,5    ; 64
    DC.B    5,5,5,5,5,5,5,5, 5,5,5,0,0,0,0,5    ; 80
    DC.B    0,5,5,5,5,5,5,5, 5,5,5,5,5,5,5,5    ; 96
    DC.B    5,5,5,5,5,5,5,5, 5,5,5,0,0,0,0,0

    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0    ; 128
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0

    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
;*-*

; IN TOKENIZE CONTEXT: A0=SOURCE, A1=INTERIM, A2=IDENT WORK, D4=LAST TOKEN
;; Preprocessor macros
PREPTA: MACRO               ; \1=AX
    LEA PREPCHARS,\1
    ENDM

PREPID: MACRO               ; \1=TAB.AX, \2=CHAR.L.DX
    CMP.B   #5,0(\1,\2.L)
    ENDM

PREPIDR:MACRO               ; \1=TAB.AX, \2=CHAR.L.DX
    CMP.B   #4,0(\1,\2.L)
    ENDM
;*-*

PREP:             ; JUST READ A "#"
    ADDQ.L  #1,A0
    CMP.B   #"d",D6
    BNE.S   .1
    CMP.B   #"e",(A0)
    BEQ.S   PREPDEF
    CMP.B   #"a",(A0)
    BEQ     PREPDATE
    BRA     iERROR63
.1: CMP.B   #"i",D6
    BEQ.W   PREPIF
    CMP.B   #"e",D6
    BEQ.W   PREPENDIF
    BRA iERROR63

; format:
; 32-255: normal chars
; "\t" and "\\\n" translated to " "
; "\n" is end of macro (0)
; chars 1-31 are args.
; ";" not allowed
;; #define

PREPDEF:              ; #define
    LEA .1(PC),A6
    BSR PREPKCMP
    BSR PREPWHITE
    GETM    A5          ; A5=MEM
    MOVE.L  A5,A4           ; A4=IDENT
    PREPTA  A6          ; A6=TABLE
    MOVEQ   #0,D0
    MOVE.B  (A0)+,D0        ; D0=CHAR
    MOVEQ   #0,D1           ; D1=HASH
    PREPID  A6,D0
    BMI iERROR64
.XL:HASHC   D1,D0
    MOVE.B  D0,(A5)+
    MOVE.B  (A0)+,D0
    PREPIDR A6,D0
    BPL.S   .XL
    SUBQ.L  #1,A0
    CLR.B   (A5)+
    MOVE.L  A5,D0
    BTST    #0,D0
    BEQ.S   .2
    CLR.B   (A5)+
.2: HASHE   D1
    LSL.L   #2,D1
    ADD.L   #MACROHASH,D1
    MOVE.L  D1,A6
    BSR PREPFINDMACRO
    TST.L   D0
    BNE iERROR66
    MOVE.L  D1,A6
    MOVE.L  (A6),(A5)       ; MAC_NEXT
    MOVE.L  A5,(A6)
    MOVE.L  A5,A6           ; A6=MACOBJ
    ADDQ.L  #4,A5
    MOVE.L  A4,(A5)+        ; MAC_NAME
    CLR.L   (A5)+           ; MAC_BODY
    CLR.L   (A5)+           ; MAC_NARGS + MAC_FLAGS
    DONEH   A5          ; -->FOR MACBODY NEEDS HEAVY
    ;BSR    PREPWHITE
    MOVE.L  WORK,A2
    MOVE.L  A2,D7           ; A2,D7=ARGSASC
    CMP.B   #"(",(A0)
    BNE.S   .NARGS
    ADDQ.L  #1,A0
    PREPTA  A5          ; A5=TAB
    MOVEQ   #0,D0
    MOVEQ   #0,D1           ; D1=NARG
.AL:    BSR PREPWHITE
    ADDQ.L  #1,D1
    MOVE.B  (A0)+,D0
    PREPID  A5,D0
    BMI iERROR64
.ALC:   MOVE.B  D0,(A2)+
    MOVE.B  (A0)+,D0
    PREPIDR A5,D0
    BPL.S   .ALC
    SUBQ.L  #1,A0
    CLR.B   (A2)+
    ;               ; -->CHECK FOR DOUBLARGS?
    BSR PREPWHITE
    CMP.B   #",",(A0)+
    BEQ.S   .AL
    CMP.B   #")",-1(A0)
    BNE iERROR64
    MOVE.W  D1,MAC_NARGS(A6)
.NARGS: BSR PREPWHITE
    CLR.B   (A2)+
    GETM    A3          ; A3,D6=PRE_ANA_MACBODY
    MOVE.L  A3,MAC_BODY(A6)
    MOVE.L  A3,D6
    PREPTA  A6          ; A6=TAB
    MOVEQ   #0,D0
.MBL:   MOVE.B  (A0)+,D0
    CMP.B   #10,D0
    BEQ.W   .MBX
    CMP.B   #"\",D0
    BEQ.W   .BSL
    CMP.B   #" ",D0
    BEQ.W   .SP
    CMP.B   #9,D0
    BEQ.W   .TAB
    CMP.B   #";",D0
    BEQ iERROR64
    CMP.W   #32,D0
    BMI iERROR64
    PREPID  A6,D0
    BMI.S   .CH
    MOVE.L  D7,A2           ; A2=ASCIIARGS
    SUBQ.L  #1,A0
    MOVE.L  A0,D5           ; D5=START ID IN TEXT
    MOVEQ   #0,D2           ; D2=NTH ARG
.FAL:   TST.B   (A2)
    BEQ.S   .CRI
    ADDQ.L  #1,D2
    MOVE.L  D5,A0
.FALI:  CMPM.B  (A2)+,(A0)+
    BEQ.S   .FALI
    TST.B   -(A2)
    BEQ.S   .MA
.FAN:   TST.B   (A2)+
    BNE.S   .FAN
    BRA.S   .FAL
.MA:    ADDQ.L  #1,A2
    MOVE.B  -(A0),D0
    PREPIDR A6,D0
    BPL.S   .FAL
    MOVE.B  D2,D0
    BRA.S   .CH         ; INSERT ARGNUM AS CHAR
.CRI:   MOVE.L  D5,A0
.CRIL:  MOVE.B  (A0)+,D0
    MOVE.B  D0,(A3)+
    PREPIDR A6,D0
    BPL.S   .CRIL
    SUBQ.L  #2,A0
    SUBQ.L  #2,A3
    MOVE.B  (A0)+,D0        ; LAST AS CH FOR CHECK
.CH:    MOVE.B  D0,(A3)+
    MOVE.L  A3,D1
    SUB.L   D6,D1
    CMP.L   #1000,D1
    BPL iERROR64
    BRA.W   .MBL
.MBX:   SUBQ.L  #1,A0
    CLR.B   (A3)+
    MOVE.L  A3,D0
    BTST    #0,D0
    BEQ.S   .3
    CLR.B   (A3)+
.3: DONEH   A3
    BRA.W   PREPDONE
.1: DC.B    "efine",0
.BSL:   CMP.B   #10,(A0)
    BNE.S   .CH
    ADDQ.L  #1,A0
    ADDQ.W  #1,LINENUM
    CMPA.L  ENDECODE,A0
    BPL iERROR64
    MOVEQ   #" ",D0
    BRA.S   .CH
.SP:    CMP.B   #" ",(A0)+
    BEQ.S   .SP
    SUBQ.L  #1,A0
    BRA.S   .CH
.TAB:   CMP.B   #9,(A0)+
    BEQ.S   .TAB
    SUBQ.L  #1,A0
    MOVEQ   #" ",D0
    BRA.S   .CH
;*-*
;; #date

PREPDATE:             ; #date
    LEA .1(PC),A6           ; name
    BSR PREPKCMP            ; check if true
    TST.L   UTILBASE        ;
    BEQ     iERROR76         ;
    BSR PREPWHITE           ; and if yes - skip whitespaces.
    GETM    A5              ; A5=buffer mem
    MOVE.L  A5,A4           ; A4=IDENT
    PREPTA  A6              ; A6=TABLE
    MOVEQ   #0,D0           ;
    MOVE.B  (A0)+,D0        ; D0=CHAR
    MOVEQ   #0,D1           ; D1=HASH
    PREPID  A6,D0           ;
    BMI iERROR64             ;
.XL:HASHC   D1,D0           ; calculate macro hashvalue
    MOVE.B  D0,(A5)+        ; A5 - macro name
    MOVE.B  (A0)+,D0        ;
    PREPIDR A6,D0           ;
    BPL.S   .XL             ; until end of macro name
    SUBQ.L  #1,A0           ;
    CLR.B   (A5)+           ; macro name = "name\0"
    MOVE.L  A5,D0           ;
    BTST    #0,D0           ;
    BEQ.S   .2              ;
    CLR.B   (A5)+           ; must be word aligned
.2: HASHE   D1              ;
    LSL.L   #2,D1           ;
    ADD.L   #MACROHASH,D1   ; add macro to hash table
    MOVE.L  D1,A6           ;
    BSR PREPFINDMACRO       ; and check if it wasn't already defined
    TST.L   D0              ;
    BNE iERROR66             ;
    MOVE.L  D1,A6           ; restore location
    MOVE.L  (A6),(A5)       ; and add to list
    MOVE.L  A5,(A6)         ;
    MOVE.L  A5,A6           ; A6=MACOBJ
    ADDQ.L  #4,A5           ; leave space for links
    MOVE.L  A4,(A5)+        ; MAC_NAME
    CLR.L   (A5)+           ; MAC_BODY
    CLR.L   (A5)+           ; MAC_NARGS + MAC_FLAGS
    DONEM   A5              ; -->FOR MACBODY NEEDS HEAVY

    BSR     PREPWHITE
    BSR     .PREPARE

    GETM    A3              ; A3,D6=PRE_ANA_MACBODY
    MOVE.L  A3,A2
    MOVE.L  A3,MAC_BODY(A6) ;
    MOVE.B  (A0)+,D0
    CMP.B   #"'",D0
    BNE     iERROR64
    MOVE.B  D0,(A3)+
.LOOP:
    MOVE.B  (A0)+,D0
    CMP.B   #"%",D0
    BEQ     .EXTEND
    MOVE.B  d0,(A3)+
    CMP.B   #"'",D0
    BNE     .LOOP
    CMP.B   #"'",(a0)+
    BEQ     .LOOP
    SUBQ.L  #1,A0
    CLR.B   (A3)+
    MOVE.L  A3,D0
    BTST    #0,D0
    BEQ     .DONTSET
    CLR.B   (A3)+
.DONTSET:
    DONEH   A3
    BRA.W   PREPDONE

.PREPARE:
    MOVEM.L D0-D7/A0-A6,-(A7)
    TST.L   .BCRES
    BNE     .SKIP_GO
    LEA .BCNAME(PC),A1
    MOVE.L  $4.W,A6
    JSR     -498(A6)
    MOVE.L  D0,.BCRES
    MOVE.L  .BCRES,A6
    JSR     -12(A6)
    LEa     .DATESTORE(PC),A0
    MOVE.L  UTILBASE,A6
    JSR     -120(A6)

.SKIP_GO:
    MOVEM.L (A7)+,D0-D7/A0-A6
    RTS

.EXTEND:
    MOVEQ   #0,D1
    MOVe.B  (A0)+,D0
    CMp.B   #"%",D0
    BEQ     .S01
    CMP.B   #"d",D0
    BEQ     .S02
    CMp.B   #"m",D0
    BEQ     .S03
    CMp.B   #"y",D0
    BEQ     .S04
    CMp.B   #"D",D0
    BEQ     .S05
    CMp.B   #"M",D0
    BEQ     .S06
    CMP.B   #"Y",D0
    BEQ     .S09
    CMP.B   #"a",D0
    BNE     .XX
    MOVE.B  (A0)+,D0
    CMP.B   #"D",D0
    BEQ     .S07
    CMP.B   #"M",D0
    BEQ     .S08
.XX:BRA     iERROR64
.S01:
    MOVE.B  #"%",(A3)+
    BRA     .LOOP
.S02:
    MOVE.W  .DATESTORE+6(PC),D1
    MOVEM.L D0-D7/A1-A2/A4-A6,-(A7)
    MOVEQ   #1,D3
    BSR     .PUTNUMBER
    MOVEM.L (A7)+,D0-D7/A1-A2/A4-A6
    BRA     .LOOP
.S03:
    MOVE.W  .DATESTORE+8(PC),D1
    MOVEM.L D0-D7/A1-A2/A4-A6,-(A7)
    MOVEQ   #1,D3
    BSR     .PUTNUMBER
    MOVEM.L (A7)+,D0-D7/A1-A2/A4-A6
    BRA     .LOOP
.S04:
    MOVE.W  .DATESTORE+10(PC),D1
    MOVEM.L D0-D7/A1-A2/A4-A6,-(A7)
    MOVEQ   #3,D3
    BSR     .PUTNUMBER
    MOVEM.L (A7)+,D0-D7/A1-A2/A4-A6
    BRA     .LOOP
.S05:
    MOVE.W  .DATESTORE+12(PC),D1
    MOVEM.L D0-D7/A1-A2/A4-A6,-(A7)
    LEA     LongDays,A1
    BSR     .PUTSTRING
    MOVEM.L (A7)+,D0-D7/A1-A2/A4-A6
    BRA     .LOOP
.S06:
    MOVE.W  .DATESTORE+8(PC),D1
    SUBQ.L  #1,D1
    MOVEM.L D0-D7/A1-A2/A4-A6,-(A7)
    LEA     LongMonths,A1
    BSR     .PUTSTRING
    MOVEM.L (A7)+,D0-D7/A1-A2/A4-A6
    BRA     .LOOP
.S07:
    MOVE.W  .DATESTORE+12(PC),D1
    MOVEM.L D0-D7/A1-A2/A4-A6,-(A7)
    LEA     ShortDays,A1
    BSR     .PUTSTRING
    MOVEM.L (A7)+,D0-D7/A1-A2/A4-A6
    BRA     .LOOP
.S08:
    MOVE.W  .DATESTORE+8(PC),D1
    SUBQ.L  #1,D1
    MOVEM.L D0-D7/A1-A2/A4-A6,-(A7)
    LEA     ShortMonths,A1
    BSR     .PUTSTRING
    MOVEM.L (A7)+,D0-D7/A1-A2/A4-A6
    BRA     .LOOP
.S09:
    MOVEQ   #0,D1
    MOVE.W  .DATESTORE+10(PC),D1
.S09_1:
    SUB.L   #100,D1
    BPL     .S09_1
    ADD.L   #100,D1
    MOVEM.L D0-D7/A1-A2/A4-A6,-(A7)
    MOVEQ   #1,D3
    BSR     .PUTNUMBER
    MOVEM.L (A7)+,D0-D7/A1-A2/A4-A6
    BRA     .LOOP


.PUTSTRING:
    LSL.W   #2,D1
    MOVE.L  0(A1,D1),A1
.P:
    MOVE.B  (A1)+,(A3)+
    TST.B   (A1)
    BNE     .P
    RTS

.PUTNUMBER:
    LSL.W   #1,D3
    LEA     .NUMBERS(PC),A1
    ADD.L   D3,A1
    LSR.W   #1,D3

.D: MOVEQ   #-1,D2
.S: ADDQ.L  #1,D2
    SUB.W   (A1),D1
    BPL     .S
    ADD.W   (A1),D1
    ADD.B   #"0",D2
    MOVE.B  D2,(A3)+
    SUBQ.L  #2,A1
    DBF     D3,.D
    RTS
.NUMBERS:
    DC.W    1,10,100,1000,10000

.DATESTORE:
    DC.L    0,0,0,0
.BCRES:
    DC.L    0
.BCNAME:
    DC.B    "battclock.resource",0
.1: DC.B    "ate",0
    EVEN
;*-*
;; #ifdef
PREPIF:               ; #ifdef, #ifndef
    CMP.B   #"f",(A0)+
    BNE iERROR63
    MOVEQ   #-1,D7          ; D7=TRUTHFLAG
    CMP.B   #"n",(A0)
    BNE.S   .2
    MOVEQ   #0,D7
    ADDQ.L  #1,A0
.2: LEA .1(PC),A6
    BSR PREPKCMP
    BSR PREPWHITE
    MOVE.L  WORK,A2
    MOVE.L  A2,D6           ; A2,D6=IDENT
    PREPTA  A6          ; A6=TAB
    MOVEQ   #0,D0
    MOVEQ   #0,D1           ; D1=HASH
    MOVE.B  (A0)+,D0
    PREPID  A6,D0
    BMI iERROR67
.CL:    MOVE.B  D0,(A2)+
    HASHC   D1,D0
    MOVE.B  (A0)+,D0
    PREPIDR A6,D0
    BPL.S   .CL
    CLR.B   (A2)+
    SUBQ.L  #1,A0
    BSR PREPWHITE
    CMP.B   #10,(A0)
    BNE iERROR67
    MOVE.L  D6,A2
    HASHE   D1
    LSL.L   #2,D1
    ADD.L   #MACROHASH,D1
    MOVE.L  D1,A6
    MOVE.L  D6,A4
    BSR PREPFINDMACRO
    TST.L   D0
    BEQ.S   .3
    MOVEQ   #-1,D0
.3: EOR.L   D0,D7           ; NEW TRUTH
    NOT.L   D7
    MOVE.W  PREPSTACKSIZE(PC),D0
    LEA PREPIFSTACK(PC),A4
    MOVE.B  D7,0(A4,D0.W)
    ADDQ.W  #1,D0
    MOVE.W  D0,PREPSTACKSIZE
    CMP.W   #PREPMAXSTACK,D0
    BPL iERROR65
    TST.L   D7
    BNE.S   .4
    MOVEQ   #0,D6           ; NESTED LEVEL
    ADDQ.L  #1,A0
.EL:    ADDQ.W  #1,LINENUM      ; NOW EAT LF
    CMPA.L  ENDECODE,A0
    BPL iERROR65
    BSR PREPWHITE
    CMP.B   #"#",(A0)
    BNE.S   .ER
    ADDQ.L  #1,A0
    MOVE.B  (A0),D0
    CMP.W   #"i",D0
    BNE.S   .E1
    ADDQ.L  #1,D6
    BRA.S   .ER
.E1:    CMP.W   #"e",D0
    BNE.S   .ER
    TST.L   D6
    BEQ.S   .EX
    SUBQ.L  #1,D6
.ER:    MOVEQ   #10,D0
.ERL:   CMP.B   (A0)+,D0
    BNE.S   .ERL
    BRA.S   .EL
.EX:    ADDQ.L  #1,A0
    BRA.S   PREPENDIF
.4: BRA.S   PREPDONE
.1: DC.B    "def",0
;*-*
;; #endif
PREPENDIF:            ; #endif
    LEA .1(PC),A6
    BSR PREPKCMP
    MOVE.W  PREPSTACKSIZE(PC),D0
    SUBQ.W  #1,D0
    BMI iERROR65
    MOVE.W  D0,PREPSTACKSIZE
    ;LEA    PREPIFSTACK(PC),A4
    ;TST.B  0(A4,D0.W)
    ;BEQ    iERROR65
    BRA.S   PREPDONE
.1: DC.B    "ndif",0,0
;*-*

;; PrepDone
PREPDONE:
    MOVE.L  STARTINTERIM,A6
    SUBQ.L  #2,A6
    CMPA.L  A6,A1
    BNE.S   .1
    MOVE.L  WORK,A2
    BRA PARSELOOP
.1: INTERN  105
;*-*
;; PrepKCmp
PREPKCMP:
    CMPM.B  (A6)+,(A0)+
    BEQ.S   PREPKCMP
    TST.B   -(A6)
    BNE iERROR63
    MOVE.B  -(A0),D6
    CMP.B   #" ",D6
    BEQ.S   .1
    CMP.B   #10,D6
    BEQ.S   .1
    CMP.B   #9,D6
    BNE iERROR63
.1: RTS
;*-*
;; PrepWhite
PREPWHITE:                ; TR ONLY D0.B
    MOVE.B  (A0),D0
    CMP.B   #" ",D0
    BEQ.S   .1
    CMP.B   #9,D0
    BNE.S   .2
.1: ADDQ.L  #1,A0
    BRA.S   PREPWHITE
.2: RTS
;*-*
;; PrepFindMacro
PREPFINDMACRO:                ; A6=LISTADR,A4=STR, OUT=D0/A6
    MOVEM.L A0/A1,-(A7)
.XL:MOVE.L  (A6),D0
    BEQ.S   .1
    MOVE.L  D0,A6
    MOVE.L  MAC_NAME(A6),A0
    MOVE.L  A4,A1
.CL:    CMPM.B  (A0)+,(A1)+
    BNE.S   .XL
    TST.B   -1(A0)
    BNE.S   .CL
.1: MOVEM.L (A7)+,A0/A1
    RTS
;*-*
;; PrepExpand
PREPEXPAND:
    MOVE.L  WORK,A4
    MOVE.L  HASHVAL(PC),D0
    LSL.L   #2,D0
    ADD.L   #MACROHASH,D0
    MOVE.L  D0,A6
    BSR PREPFINDMACRO           ; A6=MACOBJ
    TST.L   D0
    BNE.S   .C
    RTS

.C: MOVEM.L D1-D7/A1-A6,-(A7)       ; KEEP ONLY A0,A6 FROM ENV

    TST.L   PREPESTACK
    BNE.S   .1
    MOVE.L  #PREPETOTAL,D0
    BSR NEW
    TST.L   D0
    BEQ iERROR38
    MOVE.L  D0,PREPESTACK
    ADD.L   #PREPESTACKSIZE,D0
    MOVE.L  D0,PREPEBUFUSE
    ADD.L   #PREPEBUFSIZE,D0
    MOVE.L  D0,PREPEBUFEND
.1:
    MOVE.L  PREPEBUFUSE(PC),PREPETEMP
    MOVE.W  MAC_NARGS(A6),D7        ; D7.W=NARGS
    BEQ.S   .NARG
    BSR PREPWHITE
    BSR PREPMULTIEXPAND
    BRA.S   .INAC

.NARG:  MOVE.L  MAC_BODY(A6),D6         ; D6=NEW ASCCODE
.INAC:  MOVE.W  PREPESTACKUSE(PC),D0
    CMP.W   #PREPESTACKSIZE,D0
    BPL iERROR68
    MOVE.L  PREPESTACK(PC),A5       ; A5=MACEXPANSIONSTACK
    MOVE.L  A0,0(A5,D0.W)
    MOVE.L  PREPETEMP(PC),4(A5,D0.W)
    ADDQ.W  #8,PREPESTACKUSE
    MOVE.L  D6,A0
    MOVEM.L (A7)+,D1-D7/A1-A6
    ADDQ.L  #4,A7               ; BSR OFF STACK
    BSR MOVEBUF             ; FOR LONG MACROS
    BRA PARSELOOP

;*-*

;; PreProcessor Consts And Vars
PREPMAXSTACK    = 10
PREPESTACKSIZE  = 20*8
PREPEBUFSIZE    = 10*KB
PREPETOTAL  = PREPESTACKSIZE+PREPEBUFSIZE

; EACH STACKFRAME: LASTA0,LASTEBUF


PREPIFSTACK:  DS.B    PREPMAXSTACK
PREPSTACKSIZE:    DC.W    0

PREPESTACK:   DC.L    0
PREPESTACKUSE:    DC.W    0

PREPETEMP:    DC.L    0
PREPEBUFEND:  DC.L    0
PREPEBUFUSE:  DC.L    0

PREPMACROMSG: DC.B    'MACRO: ',0     ; 7

PREPMACROBODYERR: DC.L    0
;*-*

;; PrepMacroDone
PREPMACRODONE:
    MOVE.W  PREPESTACKUSE(PC),D0
    BEQ E
    BMI E
    SUBQ.W  #8,D0
    MOVE.L  PREPESTACK(PC),A6
    MOVE.L  0(A6,D0.W),A0
    MOVE.L  4(A6,D0.W),PREPEBUFUSE
    MOVE.W  D0,PREPESTACKUSE
    BRA PARSELOOP
;*-*
;; PrepWriteMacroLine
PREPWRITEMACROLINE:
    MOVE.L  PREPMACROBODYERR(PC),D7
    BEQ.S   .1
    MOVE.L  #PREPMACROMSG,D2
    MOVEQ   #7,D3
    BSR WRITECON
    MOVE.L  D7,A0
    MOVE.L  D7,A1
.XL:CMP.B   #10,-1(A0)
    BEQ.S   .L2
    CMP.B   #";",-1(A0)
    BEQ.S   .L2
    TST.B   -1(A0)
    BPL.S   .KLUD
.XB:SUBQ.L  #1,A0
    BRA.S   .XL
.L2:    CMP.B   #10,(A1)
    BEQ.S   .D
    CMP.B   #";",(A1)
    BEQ.S   .D
    TST.B   (A1)
    BEQ.S   .D
    ADDQ.L  #1,A1
    BRA.S   .L2
.D: CLR.B   (A1)
    MOVE.L  A1,D3
    MOVE.L  A0,D2
    SUB.L   D2,D3
    BSR WRITECON
    BSR WRITELN
.1: RTS
.KLUD:  CMP.B   #8,-1(A0)       ; MACBODY FROM MOD HAS LEN:INT,CHARS..
    BPL.S   .XB
    ;ADDQ.L #1,A0           ; why?
    BRA.S   .L2
;*-*
;; PrepMacroError
PREPMACROERROR:
    TST.W   PREPESTACKUSE
    BEQ.S   .1
    MOVE.L  A0,PREPMACROBODYERR
    MOVE.L  PREPESTACK(PC),A0       ; OLDEST A0 BACK
    MOVE.L  (A0),A0
.1: RTS
;*-*
;; PrepMultiExpand

; IN:   A0=ASCII, A6=MACOBJ, D7.W=NARGS
; FILLS PREPBUFUSE++ TO MAX PREFBUFEND
; RET NEW ASCII IN D6


; PROBLEMS: <CONS>

PREPMULTIEXPAND:
    MOVE.L  PREPEBUFUSE(PC),A5  ; A5=CURBUF
    MOVE.W  D7,D0
    LSL.W   #3,D0
    MOVE.L  A5,A4           ; A4,D4=ARGSBUF
    MOVE.L  A4,D4
    ADD.W   D0,A5
    CMPA.L  PREPEBUFEND(PC),A5
    BPL iERROR37
    MOVE.L  A5,D6           ; D6=BEGIN
    CMP.B   #"(",(A0)+
    BNE iERROR0
    MOVEQ   #0,D5           ; D5=BRACKET RECURSION
.BL:MOVE.L  A0,(A4)+        ; SHOVE ADR FOR ARG
.XL:MOVE.B  (A0)+,D0        ; HERE!!!
    CMP.B   #"(",D0
    BEQ.S   .UPL
    CMP.B   #"[",D0
    BEQ.S   .UPL
    CMP.B   #"]",D0
    BEQ.S   .DOL
    CMP.B   #")",D0
    BEQ.S   .DOL
    CMP.B   #",",D0
    BEQ.S   .COM
    CMP.B   #"'",D0
    BEQ.S   .STR
    CMP.B   #'"',D0
    BEQ.S   .STR
    ;               ; MORE CASES HERE?
    CMP.B   #10,D0
    BEQ iERROR3          ; ERROR = \N \0 /* ->
    CMP.B   #";",D0
    BEQ iERROR3
    TST.B   D0
    BEQ iERROR3
    CMP.B   #"/",D0
    BEQ.S   .CM1
    CMP.B   #"-",D0
    BEQ.S   .CM2
    BRA.S   .XL
.COM:   TST.L   D5
    BNE.S   .XL
    SUBQ.W  #1,D7
    BEQ iERROR0
.XBL:   MOVE.L  A0,D0
    SUB.L   -4(A4),D0
    SUBQ.L  #1,D0
    MOVE.L  D0,(A4)+        ; SHOVE LEN FOR ARG
    TST.W   D7
    BNE.S   .BL
    BRA.S   .FILL
.UPL:   ADDQ.L  #1,D5
    BMI iERROR0
    BRA.S   .XL
.DOL:   SUBQ.L  #1,D5
    BPL.S   .XL
    SUBQ.W  #1,D7
    BNE iERROR20
    BRA.S   .XBL
.CM1:   CMP.B   #"*",(A0)
    BEQ iERROR0
    BRA.W   .XL
.CM2:   CMP.B   #">",(A0)
    BEQ iERROR0
    BRA.W   .XL
.STR:   MOVE.B  (A0)+,D1
    CMP.B   #10,D1
    BEQ iERROR3
    CMP.B   D0,D1
    BNE.S   .STR
    CMP.B   (A0),D0
    BNE.W   .XL
    ADDQ.L  #1,A0
    BRA.S   .STR
.FILL:  MOVE.L  MAC_BODY(A6),A1     ; A1=BODY (NOW FILL ARGS IN MACRO)
    MOVE.L  PREPEBUFEND(PC),D3  ; D3=END
    SUBQ.L  #8,D3
    MOVEQ   #0,D0
    MOVE.W  MAC_NARGS(A6),D7
    ADDQ.L  #1,D7
.FL:    CMPA.L  D3,A5
    BPL iERROR37
    MOVE.B  (A1)+,D0
    CMP.W   D7,D0
    BMI.S   .ARG
    MOVE.B  D0,(A5)+
    BRA.S   .FL
.ARG:   TST.W   D0
    BEQ.S   .XF
    MOVE.L  D0,D1
    SUBQ.L  #1,D1
    LSL.L   #3,D1
    ADD.L   D4,D1
    MOVE.L  D1,A2
    MOVE.L  4(A2),D1        ; D1=ARGLEN
    MOVE.L  (A2),A2         ; A2=ARG
    MOVE.L  A5,D2
    ADD.L   D1,D2
    CMP.L   D3,D2
    BPL iERROR37
    SUBQ.W  #1,D1
    BMI.S   .AX
.AL:    MOVE.B  (A2)+,(A5)+
    DBRA    D1,.AL
.AX:    BRA.S   .FL
.XF:    CLR.B   (A5)+
    MOVE.L  A5,D0
    BTST    #0,D0
    BEQ.S   .NA
    CLR.B   (A5)+
.NA:    MOVE.L  A5,PREPEBUFUSE
    RTS
;*-*
;; PrepSaveMacros
PREPSAVEMACROS:
    BTST    #6,CODEPREFS+2
    BEQ.S   .XX
    MOVE.L  #ESTACKBUF,D5
    MOVE.L  D5,A5           ; D5,A5=BUF
    MOVE.W  #11,(A5)+
    MOVE.L  #MACROHASH,A4       ; A4=TABLE
    MOVE.L  #MACROHASH+1024,D4  ; D4=END
    MOVEQ   #0,D7           ; D7=NUMBER OF MACROS
.XL:CMP.L   A4,D4
    BEQ.S   .X
    MOVE.L  (A4)+,D0
    BEQ.S   .XL
    MOVE.L  D0,A3           ; A3=MACLIST
.L2:    BTST    #0,MAC_FLAGS(A3)
    BNE.S   .NEXT
    ADDQ.L  #1,D7
    ADDQ.W  #2,A5           ; COPY LEN+NAME
    MOVE.L  A5,A1
    MOVE.L  MAC_NAME(A3),A2
.L3:    MOVE.B  (A2)+,(A5)+
    BNE.S   .L3
    CLR.B   (A5)+
    MOVE.L  A5,D0
    BCLR.L  #0,D0
    MOVE.L  D0,A5
    SUB.L   A1,D0
    MOVE.W  D0,-(A1)
    MOVE.L  MAC_NARGS(A3),(A5)+ ; NARGS+FLAGS+
    ADDQ.W  #2,A5           ; COPY LEN+BODY
    MOVE.L  A5,A1
    MOVE.L  MAC_BODY(A3),A2
.L4:    MOVE.B  (A2)+,(A5)+
    BNE.S   .L4
    CLR.B   (A5)+
    MOVE.L  A5,D0
    BCLR.L  #0,D0
    MOVE.L  D0,A5
    SUB.L   A1,D0
    MOVE.W  D0,-(A1)
    BSR.S   .WR
.NEXT:  MOVE.L  (A3)+,A3
    MOVE.L  A3,D0
    BNE.S   .L2
    BRA.S   .XL
.X: TST.L   D7
    BEQ.S   .XX
    CLR.W   (A5)+
    BSR .WR
.XX:    RTS
.WR:    MOVE.L  D5,D2
    MOVE.L  A5,D3
    SUB.L   D2,D3
    JSR WRITEFILE
    MOVE.L  D5,A5
    RTS

;*-*
;; PrepReadMacros
; A0=MOD,D1=MMODE(SKIP IF 2)
PREPREADMACROS:
.XL:MOVE.W  (A0)+,D0
    BEQ.S   .X
    MOVE.L  A0,D7           ; MAC_NAME
    ADD.W   D0,A0
    MOVE.L  (A0)+,D6        ; MAC_NARGS+MAC_FLAGS+..
    MOVE.W  (A0)+,D0
    MOVE.L  A0,D5           ; MAC_BODY
    ADD.W   D0,A0
    CMP.W   #2,D1
    BEQ.S   .XL
    MOVE.L  D7,A1
    HASH    A1,D2,D3
    LSL.L   #2,D2
    ADD.L   #MACROHASH,D2
    MOVE.L  D2,A1           ; A1=HASHSPOT
    GETM    A2
    MOVE.L  (A1),(A2)       ; MAC_NEXT
    MOVE.L  A2,(A1)
    ADDQ.L  #4,A2
    MOVE.L  D7,(A2)+
    MOVE.L  D5,(A2)+
    MOVE.L  D6,(A2)+
    BSET    #0,-2(A2)       ; MAC_FLAGS = ITS AN IMPORTED MACRO!
    DONEM   A2
    BRA.S   .XL
.X: RTS

;*-*
;*-*

