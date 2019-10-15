;;PARSE

; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The Parsing Routines                    ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;

LEXMESSY:   DC.B    'lexical analysing ...',10
ENDLEXMESSY:    EVEN

TOKENIZE:

    MOVE.W  #-1,EXPORTFLAG
    BTST    #6,CODEPREFS+2      ; SEE IF EXPORT ALL
    BNE.S   .EA
    CLR.W   EXPORTFLAG
.EA:    MOVE.W  #2,CURSPOT
    BTST    #7,CODEPREFS+2
    BNE.S   .11
    MOVE.L  #LEXMESSY,D2
    MOVEQ   #ENDLEXMESSY-LEXMESSY,D3
    BSR WRITECON
.11:    CLR.W   LINENUM
    CLR.L   CONSTD
    MOVE.W  #1,ERRWHERE
.2: MOVE.L  CURECODE,A0
    CMP.L   ENDECODE,A0
    BPL.S   .1
    BSR PARSELINE
    BRA.S   .2
.1: TST.W   PREPSTACKSIZE
    BNE iERROR65
    MOVE.W  LINENUM,MAXLINENUM
    MOVE.L  CURINTERIM,A1
    MOVE.L  A1,TOKENEND
    MOVE.L  #-1,(A1)+
    MOVE.W  #-1,LINENUM
    MOVE.W  #0,ERRWHERE
    CLR.W   LINENUM2
    RTS

PARSELINE:
    BSR REALLOC1
    BSR WRITELINENUM
    
    MOVE.W  #-1,CURINS
    MOVE.L  CURECODE,A0
    MOVE.L  CURINTERIM,A1
    MOVE.W  #21,(A1)+
    MOVE.W  LINENUM2,(A1)+
    LEA 2(A1),A2
    MOVE.L  A2,STARTINTERIM
    MOVE.L  WORK,A2
    MOVEQ   #0,D0
    MOVE.W  D0,PARSEBRACKET
    MOVE.W  D0,XF
    MOVEQ   #0,D4
    MOVE.W  #-1,CURINS
PARSELOOP:
    MOVEQ   #0,D7
    MOVE.B  (A0)+,D7        ; CURRENT CHAR
    MOVE.B  (A0),D6         ; NEXT CHAR; WE CAN LOOK AHEAD ! YEAH !

    MOVE.W  D7,D5
    LSL.W   #2,D5
    MOVE.L  JUMPTAB(PC,D5.W),A4
    JMP (A4)

JUMPTAB:
    DC.L    PREPMACRODONE,E,E,E,E,E,E,E         ; 0
    DC.L    E,PARSELOOP,PARSEENDLINE,E,PARSELOOP,E,E,E  ; 8
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 16
    DC.L    PARSELOOP,PARSEFLT,PARSELONGSTR,ASM_PARSEIMM    ; 32
    DC.L    PARSEHEXNUM,PARSEBINNUM,PARSEAMP,PARSESTRING       ; 36
    DC.L    PARSEOPEN,PARSECLOSE,PARSEPROD,PARSEPLUS    ; 40
    DC.L    PARSECOMMA,PARSEMINUS,PARSEPERIOD,PARSEDIV  ; 44
    DC.L    VAL,VAL,VAL,VAL,VAL,VAL,VAL,VAL         ; 48
    DC.L    VAL,VAL,PARSEBECOMES,PARSEENDLINE       ; 56
    DC.L    PARSESMALLER,PARSEEQ,PARSEBIGGER,QMARK  ; 60
    DC.L    E,ID,ID,ID,ID,ID,ID,ID              ; 64
    DC.L    ID,ID,ID,ID,ID,ID,ID,ID             ; 72
    DC.L    ID,ID,ID,ID,ID,ID,ID,ID             ; 80
    DC.L    ID,ID,ID,PARSESQ1,E,PARSESQ2,PARSEPTR,ID    ; 88
    DC.L    PARSEQUOTE,ID,ID,ID,ID,ID,ID,ID         ; 96
    DC.L    ID,ID,ID,ID,ID,ID,ID,ID             ; 104
    DC.L    ID,ID,ID,ID,ID,ID,ID,ID             ; 112
    DC.L    ID,ID,ID,PARSECURL1,PARSECONS,PARSECURL2,E,E    ; 120
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 128
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 144
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 160
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 176
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 192
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 208
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 224
    DC.L    E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E         ; 240

E: BRA iERROR3

;; ","
PARSECOMMA:           ; ,
    MOVEQ   #COM,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; + ++
PARSEPLUS:            ; + ++
    CMP.B   #'+',D6
    BEQ.S   .1
    MOVEQ   #7,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.1: ADDQ.L  #1,A0
    MOVEQ   #33,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; - --
PARSEMINUS:           ; - --
    CMP.B   #'-',D6
    BEQ.S   .1
    CMP.B   #'>',D6
    BEQ.S   .2
    MOVEQ   #8,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.1: ADDQ.L  #1,A0
    MOVEQ   #34,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.2: MOVEQ   #10,D0
.XL:CMP.B   (A0)+,D0
    BNE.S   .XL
    SUBQ.L  #1,A0
    BRA PARSELOOP
;*-*
;; "*"
PARSEPROD:            ; *
    CMP.B   #'/',D6
    BEQ iERROR27
    MOVEQ   #9,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; / //
PARSEDIV:             ; *
    CMP.B   #'*',D6
    BEQ.S   COMMENT
    CMP.B   #"/",D6
    BEQ     .CMNT
    MOVEQ   #10,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.CMNT:
    CMP.B   #10,(A0)+
    BNE     .CMNT
    SUBQ.L  #1,A0
    BRA PARSELOOP
;*-*
;; /* */
COMMENT:              ; /* */
    MOVE.L  A0,A3           ; BACKUP
    MOVE.L  LINENUM,A4      ; BACKUPTOO
    MOVEQ   #1,D0           ; COUNTING COMMENTS
    MOVEQ   #'/',D1
    MOVEQ   #'*',D2
    MOVE.L  ENDECODE,D3
    CMP.L   D3,A0
    BMI.S   .NM
    MOVEQ   #0,D3           ; IN A MACRO
.NM:    MOVEQ   #10,D5
    ADDQ.L  #1,A0
.1: TST.L   D0
    BEQ.S   .3
    TST.L   D3
    BEQ.S   .MA
    CMP.L   D3,A0
    BPL.S   .ER
.XB:MOVE.B  (A0)+,D7
    MOVE.B  (A0),D6
    CMP.B   D7,D5
    BEQ.S   .4
.5: CMP.B   D1,D7           ; IS THIS LIKE "/*" ?
    BNE.S   .2
    CMP.B   D2,D6
    BNE.S   .2
    ADDQ.L  #1,D0
    ADDQ.L  #1,A0
    BRA.S   .1
.2: CMP.B   D2,D7           ; OR LIKE "*/" ?
    BNE.S   .1
    CMP.B   D1,D6
    BNE.S   .1
    SUBQ.L  #1,D0
    ADDQ.L  #1,A0
    BRA.S   .1
.3: BRA PARSELOOP
.4: ADDQ.W  #1,LINENUM
    BRA.S   .5
.ER:    MOVE.L  A3,A0           ; FOR ERROR REPORT
    SUBQ.L  #1,A0
    MOVE.L  A4,LINENUM
    BRA iERROR27
.MA:    TST.B   (A0)
    BEQ.S   .ER
    BRA.S   .XB
;*-*
;; "`"
PARSEQUOTE:           ; `
    MOVEQ   #36,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; "="
PARSEEQ:              ; =
    CMP.B   #">",D6
    BEQ     .1
    CMP.B   #"<",D6
    BEQ     .2
    MOVEQ   #11,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.1: ADDQ.L  #1,a0
    MOVEQ   #14,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.2: ADDQ.L  #1,A0
    MOVEQ   #15,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; "^"
PARSEPTR:             ; ^
    MOVEQ   #32,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; "&"
PARSEAMP:
    MOVE.W  #IOFF+29,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; "!"
PARSEFLT:             ; !
    BTST    #1,CODEPREFS
    BNE     .1
    BSET    #3,CODEPREFS+1
.1: MOVEQ   #37,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; "| ||"
PARSECONS:            ; |
    CMP.B   #"|",D6
    BEQ     .OR
    MOVEQ   #46,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.OR:ADDQ.L  #1,A0
    MOVE.W  #IOFF+30,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; "{"
PARSECURL1:           ; {
    MOVEQ   #23,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
;*-*
;; "}"
PARSECURL2:           ; }
    MOVEQ   #0,D4
    MOVE.W  #24,(A1)+
    BRA PARSELOOP
;*-*
MAXBRACK    = 50
CURBRACK:   DC.L    0
;; "["
PARSESQ1:             ; [
    MOVEQ   #29,D4
    MOVE.W  D4,(A1)+
    MOVE.L  CURBRACK(PC),A3     ; 29:INT, otherbrackoff:LONG
    CMPA.L  #BRACKETSBUF,A3
    BNE.S   .1
    MOVE.L  A0,SQBRAKPOS
    MOVE.L  LINENUM,SQBRAKLINE
.1: MOVE.L  A1,(A3)+
    MOVE.L  A3,CURBRACK
    CLR.L   (A1)+

    BRA PARSELOOP
;*-*
;; "]"
PARSESQ2:             ; ]
    MOVEQ   #30,D4
    MOVE.W  D4,(A1)+
    MOVE.L  CURBRACK(PC),A3
    CMPA.L  #BRACKETSBUF,A3
    BEQ iERROR52
    MOVE.L  -(A3),A4
    MOVE.L  A3,CURBRACK
    MOVE.L  A1,D0
    SUB.L   A4,D0
    MOVE.L  D0,(A4)         ; OFFSET = {otherbrackoff} .. ob+2
    BRA PARSELOOP

;*-*
;; "."
PARSEPERIOD:          ; . .l .b .w
    MOVEQ   #26,D4
    CMP.B   #'W',D6
    BNE.S   .1
    MOVE.W  D4,(A1)+
    BRA .3
.1: CMP.B   #'B',D6
    BNE.S   .2
    MOVE.W  #28,(A1)+
    BRA .3
.2: CMP.B   #'L',D6
    BNE.S   .4
    MOVE.W  #27,(A1)+
.3: ADDQ.L  #1,A0
    BRA PARSELOOP
.4: CMP.B   #58,D6
    BMI.S   .5
.6: MOVEQ   #35,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.5: CMP.B   #48,D6
    BMI.S   .6
    BRA PARSEVALUE
;*-*
;; > >= >>
PARSEBIGGER:          ; > >=
    CMP.B   #'=',D6
    BEQ.S   .1
    CMP.B   #">",D6
    BEQ     .2
    MOVEQ   #12,D4
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.1: MOVEQ   #14,D4
    MOVE.W  D4,(A1)+
    ADDQ.L  #1,A0
    BRA PARSELOOP
.2: ADDQ.L  #1,A0
    MOVEQ   #50,D4
    MOVE.W  D4,(A1)+
    BRA     PARSELOOP
;*-*
;; < <= <> <=> <<
PARSESMALLER:
    CMP.B   #'=',D6
    BEQ.S   .1
    CMP.B   #'>',D6
    BEQ.S   .2
    CMP.B   #"<",D6
    BEQ     .4
    MOVEQ   #13,D4          ; <
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.1: ADDQ.L  #1,A0
    CMP.B   #'>',(A0)
    BEQ.S   .3
    MOVEQ   #15,D4          ; <=
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.2: MOVEQ   #16,D4          ; <>
    MOVE.W  D4,(A1)+
    ADDQ.L  #1,A0
    BRA PARSELOOP
.3: MOVEQ   #40,D4          ; <=>
    MOVE.W  D4,(A1)+
    ADDQ.L  #1,A0
    BRA PARSELOOP
.4: MOVEQ   #49,D4
    ADDQ.L  #1,A0
    MOVE.W  D4,(a1)+
    BRA     PARSELOOP
;*-*
;; := : :: :=:
PARSEBECOMES:
    CMP.B   #'=',D6
    BNE.S   .1
    CMP.B   #':',1(A0)      ; :=:
    BEQ     .3
    MOVEQ   #ASSGN,D4       ; :=
    MOVE.W  D4,(A1)+
    MOVE.B  (A0)+,D7
    BRA PARSELOOP
.1: CMP.B   #':',D6
    BNE.S   .2
    MOVEQ   #41,D4          ; ::
    MOVE.W  D4,(A1)+
    MOVE.B  (A0)+,D7
    BRA PARSELOOP
.2: MOVEQ   #19,D4          ; :
    MOVE.W  D4,(A1)+
    BRA PARSELOOP
.3: MOVEQ   #53,D4          ; :=:
    MOVE.W  D4,(A1)+
    ADDq.L  #2,A0
    BRA     PARSELOOP
;*-*
;; "("
PARSEOPEN:            ; (
    MOVEQ   #17,D4
    MOVE.W  D4,(A1)+
    TST.W   PARSEBRACKET
    BNE.S   .1
    MOVE.L  A0,BRAKPOS
    MOVE.L  LINENUM,BRAKLINE
.1: ADDQ.W  #1,PARSEBRACKET
    BRA PARSELOOP
;*-*
;; ")"
PARSECLOSE:           ; )
    MOVEQ   #18,D4
    MOVE.W  D4,(A1)+
    TST.W   PARSEBRACKET
    BEQ iERROR20
    SUBQ.W  #1,PARSEBRACKET
    BRA PARSELOOP
;*-*
;; "?"
QMARK:
    MOVEQ   #52,D4
    MOVE.W  D4,(A1)+
    BRA     PARSELOOP
;*-*
PARSEBRACKET: DC.W    0
BRAKPOS:      DC.L    0
BRAKLINE:     DC.L    0
SQBRAKPOS:    DC.L    0
SQBRAKLINE:   DC.L    0

ID:
PARSEIDENT:
    MOVEQ   #0,D5           ; D5=HASHVALUE!!!
.3: HASHC   D5,D7
    MOVE.B  D7,(A2)+        ; IDENT
    CMP.B   #123,D6
    BPL.S   .4
    CMP.B   #97,D6
    BPL .2
    CMP.B   #91,D6
    BPL.S   .5
    CMP.B   #65,D6
    BPL .2
    CMP.B   #58,D6
    BPL.S   .4
    CMP.B   #48,D6
    BPL .2
.4: CLR.B   (A2)+           ; IDENT FINISHED
    MOVE.B  #-1,(A2)+
    HASHE   D5
    MOVE.L  D5,HASHVAL
    MOVE.L  WORK,A2
    BTST    #7,CODEPREFS+1
    BEQ.S   .NMAC
    BSR PREPEXPAND
.NMAC:  MOVE.B  (A2),D0
    CMP.B   #'_',D0
    BMI.S   PARSEUPPER
    BRA.S   PARSELOWER
.2: MOVE.B  (A0)+,D7
    MOVE.B  (A0),D6
    BRA.S   .3
.5: CMP.B   #'_',D6
    BEQ.S   .2
    BRA.S   .4

HASHVAL:  DC.L    0       ; IDENT HASHVALUE!!!
INPROC:   DC.W    0
REGSTOP:  DC.W    0

PARSELOWER:
    BSR PARSEGETIDENTNR
    MOVE.L  WORK,A2
    BRA PARSELOOP

PARSEUPPER:
    MOVE.B  1(A2),D0        ; INS or Ins or Rx
    CMP.B   #58,D0
    BMI ASM_PARSEREG
REGBACK:
    CMP.B   #'a',D0
    BPL PARSEUPLOW
    CMP.B   #'.',(A0)
    BEQ PARSECONSTASM
    BSR.W   PARSEGETINSNR       ; INS
    TST.W   D0
    BEQ PARSECONSTASM

    LEA .TAB-(4*IOFF)(PC),A3
    MOVE.L  D0,D1
    LSL.W   #2,D1
    MOVE.L  0(A3,D1.W),A3
    JMP (A3)

.TAB:
    DC.L    .P,   .EP,  .DONE,.DONE,.DONE,.DONE ; PROC .. WHILE
    DC.L    .DONE,.DONE,.DONE,.DONE,.DONE,.DONE ; ENDW .. DEFAULT
    DC.L    .DONE,.DONE,.DONE,.DONE,.D,   .DONE ; ENDS .. LOCAL
    DC.L    .DONE,.DONE,.DONE,.DONE,.DONE,.DONE ; ELSE .. INC
    DC.L    .DONE,.DONE,.DONE,.DONE,.DONE,.DONE ; DEC  .. AND
    DC.L    .DONE,.RESO,.RESO,.RESO,.DONE,.DONE ; OR   .. EXIT
    DC.L    .DONE,.DONE,.DONE,.DONE,.DONE,.DONE ; LARG .. ARRAY
    DC.L    .DONE,.DONE,.DONE,.DONE,.DONE,.DONE ; STRI .. LIST
    DC.L    .OBJ, .EOBJ,.DONE,.EP2, .DONE,.RESO ; OBJE .. ENUM
    DC.L    .RESO,.DONE,.HAND,.DONE,.RSE, .EXP  ; SET  .. EXPORT
    DC.L    .REG, .DONE,.EP,  .DONE,.DONE,.DONE ; REG  .. PRIVATE
    DC.L    .DONE,.DONE,.RESO,.DONE,.DONE,.DONE ; SUPE .. NOSTARTUP
    DC.L    .DONE,.DONE,.DONE,.DONE,.RESO,.DONE ; LINK .. STARTUP
    DC.L    .DONE,.DONE,.DONE,.DONE,.DONE,.DONE ; WITH .. SECTION
    DC.L    .DONE,.DONE,.DONE,.DONE,.DONE,.DONE ; CODE ..
    DC.L    .DONE,.DONE,.DONE,.DONE,.DONE,.DONE ;

;; .RESO
.RESO:
    TST.L   CONSTD
    BNE .SER
    MOVE.L  A1,CONSTD
    MOVE.L  LINENUM,LINENUMZ
    MOVE.W  D0,(A1)+
    MOVE.W  D0,D4
    MOVE.L  WORK,A2
    BRA PARSELOOP
;*-*
;; .BRAK
.BRAK:
    TST.W   PARSEBRACKET
    BNE iERROR20
    RTS
;*-*
;; .DONE
.DONE:
    MOVE.W  D0,(A1)+
    MOVE.W  D0,D4
    TST.W   MAXREGALLOC
    BNE .HEAVY          ; DO REGALLOC STATS
.HB:MOVE.L  WORK,A2
    BRA PARSELOOP
;*-*
;; .P
.P: MOVE.W  #-1,.DEFF
    MOVE.W  #-1,INPROC
    MOVE.W  #-1,SCOPE
    CLR.W   REGSTOP
    BSR.S   .BRAK
    BRA.S   .RESO
.EP2:
    CMP.W   #18,D4          ; RETURN PREC ")" OR <OBJ>
    BEQ.S   .EP
    CMP.W   #31,D4
    BNE.S   .DONE
;*-*
;; .EP
.EP:CLR.W   INPROC
    CLR.W   SCOPE
    BSR.S   .BRAK
    BRA .DONE
;*-*
;; .OBJ
.OBJ:
    MOVE.L  #-1,OBJECTD
    CLR.L   OBJNAME
    BRA.S   .DONE
;*-*
;; .EOBJ
.EOBJ:
    CLR.L   OBJECTD
    BRA.S   .DONE
;*-*
;; .D
.D: TST.L   CONSTD
    BNE iERROR12
    TST.W   .DEFF
    BEQ     .DONE
    MOVE.L  #IOFF+17,D0     ; SET DEF TO LOCAL
    MOVE.W  #17,CURINS      ; FIX
    BRA.W   .DONE
.DEFF:  DC.W    0
;*-*
;; .RSE
.RSE:
    MOVE.W  #-1,BUSYRAISE
    BRA .RESO
;*-*
;; .SER
.SER:
    CMP.W   #IOFF+33,D0
    BNE iERROR12
    BRA .DONE
;*-*
;; .EXP
.EXP:
    MOVE.W  #-1,EXPORTFLAG
    TSTMOD
    BEQ iERROR49
    BRA .DONE
;*-*
;; .HAND
.HAND:
    MOVE.L  CPROC(PC),A2
    BSET    #2,2(A2)        ; SET NO_REGALLOC
    MOVE.W  #1,REGSTOP
    BRA .DONE
;*-*
;; .REG
.REG:
    CMP.W   #19,D4
    BNE .DONE
    MOVE.L  LASTID(PC),D1
    BEQ .DONE
    MOVE.L  D1,A3

    BTST    #3,5(A3)        ; SET REG!
    BNE iERROR50
    BSET    #3,5(A3)
    MOVE.L  6(A3),A6        ; A6=PROC
    TST.W   REGSTOP
    BNE iERROR56         ; NO :REG IN HANDLE
    ADDQ.B  #1,3(A6)
    MOVE.B  3(A6),D7
    EXT.W   D7
    CMP.W   #MAXREGVARS+1,D7
    BPL iERROR56         ; TOOMUCH regvars per proc
    NEG.W   D7
    ADDQ.W  #8,D7
    MOVE.W  D7,10(A3)       ; set regnum instead of offset

    BRA .DONE
;*-*
; UPDATE HEAVYNESS FACTOR ACCORDING TO KEYWORDS (KEY=D4)
;; .HEAVY
.HEAVY:
    TST.W   REGSTOP
    BNE .HB
    MOVE.B  HEAVYTABLE-IOFF(PC,D4),D0
    MOVE.L  HEAVYNESS,D1        ; D1 BECOMES NEW HEAVYNESS
    EXT.W   D0
    CMP.B   #"s",D0
    BEQ.S   .SET1
    CMP.B   #"m",D0
    BEQ.S   .LOOP
    CMP.B   #"d",D0
    BEQ.S   .ELOOP
    CMP.B   #"i",D0
    BEQ.S   .IF
    CMP.B   #"e",D0
    BEQ.S   .EIF
    CMP.B   #"D",D0
    BEQ.S   .DO
    CMP.B   #"S",D0
    BEQ.S   .SEL
    CMP.B   #"U",D0
    BEQ.S   .UNTIL
    CMP.B   #"E",D0
    BEQ.S   .ESEL
.HEND:
    CMP.L   #1,D1
    BPL.S   .HEND2
    MOVEQ   #1,D1
.HEND2:
    MOVE.L  D1,HEAVYNESS
    BRA .HB
;*-*
;; .SET1
.SET1:
    MOVE.L  #MIDHEAVY,D1
    BRA.S   .HEND
;*-*
;; .LOOP
.LOOP:
    LSL.L   #3,D1
    ADDQ.W  #3,POSTDFACTOR
    BRA.S   .HEND
;*-*
;; .ELOOP
.ELOOP:
    LSR.L   #3,D1
    BRA.S   .HEND
;*-*
;; .IF
.IF:
    ADDQ.W  #1,POSTDFACTOR
    MOVE.W  #1,POSTACTION
    BRA.S   .HEND
;*-*
;; .EIF
.EIF:
    LSL.L   #1,D1
    BRA.S   .HEND
;*-*
;; .SEL
.SEL:
    LSL.L   #4,D1
    MOVE.W  #2,POSTACTION
    BRA.S   .HEND
;*-*
;; .ESEL
.ESEL:
    LSL.L   #2,D1
    BRA.S   .HEND
;*-*
;; .DO
.DO:MOVE.W  #1,POSTACTION
    CMP.W   #IOFF+25,D4     ; then?
    BNE.S   .HEND
    LSR.L   #1,D1
    ADDQ.W  #1,POSTMFACTOR
    SUBQ.W  #1,POSTDFACTOR
    BRA.S   .HEND
;*-*
;; .UNTIL
.UNTIL:
    ADDQ.W  #3,POSTDFACTOR
    MOVE.W  #1,POSTACTION
    BRA.S   .HEND
;*-*
;; HEAVY - Data
HEAVYNESS:  DC.L    MIDHEAVY
POSTMFACTOR:    DC.W    0
POSTDFACTOR:    DC.W    0
POSTACTION: DC.W    0       ; WHAT TO DO AT EOLN
                    ; 0=NOP, 1=FAC, 2=SEL
HEAVYTABLE:
        DC.B "ssie mdmdS  EmU"  ; PROC-UNTIL
        DC.B "          DmdD "  ; JUMP-AND
        DC.B "               "  ; OR-PTR
        DC.B "            s  "  ; OF-EXPORT
        DC.B "               "  ; REG...
        DC.B "               "
        EVEN

; s h:=8        start of PROC etc.
; m h:=h*8      start of loop
; d h:=h/8      end of loop
; i lf;h:=h/2   if
; e h:=h*2      endif
; E h:=h*4      endselect
; S h:=h*16;lf;/4   select
; D lf      do/then
; U lf;h:=h/8   until
;*-*
POSTOPTI:

;   MOVE.W  LINENUM,D0      ; WRITE TABLE AT $1E0000
;   MOVE.L  #$1E0000,A2
;   LSL.W   #2,D0
;   ADD.W   D0,A2
;   MOVE.L  HEAVYNESS,(A2)

    TST.W   MAXREGALLOC
    BEQ.S   .E
    TST.W   REGSTOP
    BNE.S   .E
    TST.W   POSTACTION
    BEQ.S   .EC
    MOVEM.L D0/D1,-(A7)
    MOVE.L  HEAVYNESS(PC),D1
    MOVE.W  POSTACTION(PC),D0
    CMP.W   #1,D0
    BNE.S   .S
    MOVE.W  POSTMFACTOR(PC),D0
    LSL.L   D0,D1
    MOVE.W  POSTDFACTOR(PC),D0
    LSR.L   D0,D1
    BRA.S   .R
.S: LSR.L   #6,D1           ; /16/4
.R: CMP.L   #1,D1
    BPL.S   .O
    MOVEQ   #1,D1
.O: MOVE.L  D1,HEAVYNESS
    MOVEM.L (A7)+,D0/D1
    CLR.W   POSTACTION
.EC:CLR.L   POSTMFACTOR
.E: RTS

CONSTD:       DC.L    0
OBJECTD:      DC.L    0
BUSYRAISE:    DC.W    0

;; CONST/Assembler
PARSECONSTASM:
    MOVE.L  WORK,A2
    BSR     ASM_PARSE
    MOVE.L  WORK,A2
    TST.L   D0
    BNE PARSELOOP

    MOVE.L  #CONSTHASH,A3
    MOVE.L  HASHVAL,D0
    LSL.L   #2,D0
    ADD.L   D0,A3           ; A3=START CONST TABEL

    MOVE.L  A3,A6           ; A6=BACKUP
.1: MOVE.L  (A3),A3
    MOVE.L  A3,D0
    BEQ .3
    MOVE.L  CASCII(A3),A4       ; A4=ASCIIPTR
    MOVE.L  WORK,A5     ; A5=THIS CONST
.2: CMPM.B  (A4)+,(A5)+
    BNE.S   .1
    CMP.B   #0,-1(A4)
    BNE.S   .2
    btst    #1,CFLAGS(a3)
    bne     .STRING
    MOVEQ   #VALUE,D4
    MOVE.W  D4,(A1)+
    MOVE.L  CVAL(A3),(A1)+
    MOVE.L  WORK,A2
    BRA PARSELOOP

.STRING:
    moveq   #STR,D4
    move.w  d4,(a1)+
    move.l  CVAL(a3),a2
    move.w  (a2),d0
    move.w  d0,(a1)+
    addq.l  #1,d0
    btst    #0,d0
    beq     .T0
    addq.l  #1,d0
.T0:lsr.w   #1,d0
    move.w  d0,(a1)+
    move.w  (a2)+,d0
    subq.l  #1,d0
.TST:
    move.b  (a2)+,(a1)+
    dbf     d0,.TST
    clr.b   (a1)+
    move.l  a1,d0
    btst    #0,d0
    beq     .T1
    clr.b   (a1)+
.T1:MOVE.L  WORK,A2
    BRA PARSELOOP


.3: TST.L   CONSTD          ; MAKE NEW CONSTANT
    BEQ.S   .ER
    GETM    A3
    MOVE.L  A3,D0
    MOVE.L  WORK,A4
.4: MOVE.B  (A4)+,(A3)+
    BNE .4
    MOVE.L  A3,D4
    BTST    #0,D4
    BEQ.S   .5
    CLR.B   (A3)+
.5: DONEM   A3
    MOVEQ   #25,D4
    MOVE.W  D4,(A1)+
    MOVE.L  D0,(A1)+
    MOVE.L  A6,(A1)+        ; HASHENTRY
    MOVE.L  WORK,A2
    BRA PARSELOOP
.ER:MOVE.L  WORK,ERROROBJ
    BRA iERROR1
;*-*
;; EFunction

XF:   DC.W    0       ; 0=NO F-FUNC, -1=F

PARSEUPLOW:           ; Ins
    BSR PARSELIBFUNCTION
    TST.L   D4
    BNE PLIBOUT
    LEA EFUNCTAB,A3     ; IT MIGHT BE AN E FUNCTION!
    MOVEQ   #0,D3
    MOVEQ   #0,D1
.1: MOVE.L  (A3)+,A4        ; ASC
    CMP.L   A4,D1
    BEQ     MIGHTBELIB
    ADD.L   #EFUNCENTRYSIZE-4,A3
    MOVE.L  A2,A5
    ADDQ.L  #1,D3
.2: CMPM.B  (A4)+,(A5)+
    BNE.S   .1
    CMP.B   #0,(A4)
    BNE.S   .2
    CMP.B   #0,(A5)
    BNE.S   .2
    CMP.B   #'F',-1(A4)
    BNE.S   .3
    MOVE.W  #-1,XF
    BRA.S   .4
.3: CLR.W   XF
.4: SUB.L   #16,A3
    MOVEQ   #22,D4
    MOVE.W  D4,(A1)+
    MOVE.L  A3,(A1)+

    MOVEM.L D0/A1,-(A7)
    LEA EFUNCFLAGSTAB,A1
    lsl.l   #2,d3
    MOVE.L  -4(A1,D3.W),D0
    lsr.l   #2,d3
    BTST    #0,D0
    BEQ     .B0
    BSET    #3,CODEPREFS+1
.B0:BTST    #1,D0
    BEQ     .B1
    BSET    #4,CODEPREFS+1
.B1:BTST    #9,D0
    BEQ     .B2
    BSET    #7,CODEPREFS
.B2:BTST    #10,D0
    BEQ     .B3
    BSET    #3,CODEPREFS
.B3:BTST    #11,D0
    BEQ     .B4
    BSET    #6,CODEPREFS
.B4:BTST    #8,D0
    BNE     .INLINE
    MOVE.L  #EFUNCBYTE,A3
    MOVE.B  #-1,-1(A3,D3)
.INLINE:
    MOVEM.L (A7)+,D0/A1

    BRA.S   PLIBOUT
;*-*
;; Library call
MIGHTBELIB:
    MOVE.L  WORK,A2
    MOVE.L  ACODE,A3        ; IT MIGHT BE A LIBRARY CALL!
    MOVEQ   #0,D1           ; (WE USE ASMCODE BUF !!!)
.1: MOVE.L  (A3)+,A4        ; ASC
    CMP.L   A4,D1
    BEQ.S   PNOLIB
    MOVE.L  (A3)+,D0        ; D0=OFFSETS
    MOVE.W  (A3)+,D2        ; D2=EXCEPTION
    MOVE.L  A2,A5
.2: CMPM.B  (A4)+,(A5)+
    BNE.S   .1
    CMP.B   #32,(A4)
    BPL.S   .2
    CMP.B   #32,(A5)
    BPL.S   .2
    MOVEQ   #LIBC,D4
    TST.W   BUSYRAISE
    BNE.S   .RAISE
    MOVE.W  D4,(A1)+
    MOVE.L  D0,(A1)+
    MOVEQ   #0,D0
    MOVEQ   #0,D1           ; MOVEM.L MASK
    MOVE.L  A4,(A1)+
.4: MOVE.B  (A4)+,D0
    CMP.W   #16,D0
    BPL.S   .3
    BSET    D0,D1
    BRA.S   .4
.3: MOVE.W  D1,(A1)+
    MOVE.W  D2,(A1)+        ; PUT EXCEPTION #
    BRA.S   PLIBOUT
.RAISE: LEA -10(A3),A3
    MOVE.W  #38,(A1)+
    MOVE.L  A3,(A1)+
    BRA.S   PLIBOUT
PNOLIB:               ; WHAT ELSE?
    MOVE.L  WORK,ERROROBJ
    BRA iERROR24
PLIBOUT:
    MOVE.L  WORK,A2
    BRA PARSELOOP
;*-*
;; LibFunction
PARSELIBFUNCTION:
; quite intelligent :)
; searches the whole list for best match
; (note it checks for best matching CPU, FPU, MMU and OSVersion)
; the highest priority has the cpu required.
; so __OSVERSION=37 is same as __CPU=020 or (_FPU=68040 and _MMU=68040)
    MOVE.W  #-32768,.RES1
    CLR.L   .RES2
    clr.w   .RES3
    MOVE.L  D0,-(A7)
    MOVE.L  HASHVAL,D0
    LSL.W   #2,D0
    MOVE.L  #LIBHASH,A3
    ADD.L   D0,A3
    ADDQ.L  #4,A3
.FIND:
    MOVE.L  LIB_NEXT(A3),D0
    BEQ     .NFOUND
    MOVE.L  D0,A3
    MOVE.L  LIB_NAME(A3),A4
    MOVE.L  A2,A5
.CHK:
    CMPM.B  (A4)+,(A5)+         ; search for function
    BNE .FIND
    TST.B   -1(A4)
    BNE .CHK
    TST.B   -1(A5)
    BNE .CHK
    move.w  #-1,.RES3
    move.w  TCPU,d4
    cmp.w   LIB_CPU(A3),d4
    blt     .CHK
    MOVE.W  LIB_CPU(A3),D0
    DIVU.W  #5,D0
    move.w  EFPU,D4
    cmp.w   LIB_FPU(A3),d4
    blt     .CHK
    ADD.W   LIB_FPU(A3),D0

    move.w  EMMU,D4
    cmp.w   LIB_MMU(A3),D4
    blt     .CHK
    ADD.W   LIB_MMU(A3),D0

    move.w  OSVERSION,D4
    cmp.w   LIB_OSVERS(A3),D4
    blt     .CHK

    ADD.W   LIB_OSVERS(A3),D0
    SUB.W   #33,D0
    CMP.W   .RES1,D0
    BLT     .CHK
    MOVE.W  D0,.RES1
    MOVE.L  A3,.RES2
    BRA     .CHK
.FOUND:
    MOVEQ   #54,D4
    move.w  d4,(a1)+
    MOVE.L  .RES2,A3
    move.l  A3,(a1)+
    move.l  #-1,LIB_USED(a3)      ; set "used"
    MOVE.L  (A7)+,D0
    rts
.NFOUND:
    TST.L   .RES2
    BNE .FOUND
    tst.w   .RES3
    BNE iERROR107
    moveq   #0,d4
    MOVE.L  (A7)+,D0
    rts
.RES1:
    DC.W    0
.RES2:
    DC.L    0
.RES3:
    DC.W    0
;*-*
;; VALUES
VAL:
;; DEC
PARSEVALUE:
.2: MOVE.B  D7,(A2)+
    MOVE.B  (A0)+,D7
    CMP.B   #58,D7
    BPL.S   .1
    CMP.B   #48,D7
    BPL .2
    CMP.B   #".",D7
    BEQ.S   .2
.1: SUBQ.L  #1,A0
    MOVE.B  #0,(A2)+        ; VALUE FINISHED
    MOVE.L  WORK,A2
    BSR PARSEGETVALUE
    MOVE.L  WORK,A2

    MOVEQ   #VALUE,D4
    TST.L   D7
    BEQ.S   .3
    ADD.W   #42,D4          ; FLOAT
.3:
    MOVE.W  D4,(A1)+
    MOVE.L  D0,(A1)+
    BRA PARSELOOP
;*-*
;; HEX
PARSEHEXNUM:
    MOVE.L  WORK,A2
    MOVE.L  A2,D3
    MOVEQ   #0,D1           ; RESULT
    MOVEQ   #0,D2           ; #OF SHIFTS
.4: MOVE.B  (A0)+,D0
    CMP.B   #"G",D0
    BPL.S   .2B
    CMP.B   #"0",D0
    BMI.S   .1
    CMP.B   #"9"+1,D0
    BPL.S   .2
.3: MOVE.B  D0,(A2)+
    BRA.S   .4
.1: CMP.L   A2,D3
    BEQ.S   .5
    MOVEQ   #0,D0
    MOVE.B  -(A2),D0
    SUB.B   #48,D0
    CMP.W   #32,D2
    BEQ iERROR7
    LSL.L   D2,D0
    ADD.L   D0,D1
    ADDQ.L  #4,D2
    BRA.S   .1
.5: MOVEQ   #VALUE,D4
    MOVE.W  D4,(A1)+
    MOVE.L  D1,(A1)+
    SUBQ.L  #1,A0
    MOVE.L  WORK,A2
    BRA PARSELOOP
.2: CMP.B   #"A",D0
    BMI.S   .1
    SUB.B   #7,D0
    BRA.S   .3
.2B:    CMP.B   #"a",D0
    BMI.S   .1
    CMP.B   #"g",D0
    BPL.S   .1
    SUB.B   #7+32,D0
    BRA.S   .3
;*-*
;; BIN
PARSEBINNUM:
    MOVE.L  WORK,A2
    MOVE.L  A2,D3
    MOVEQ   #0,D1           ; RESULT
    MOVEQ   #0,D2           ; BITNUM
.4: MOVE.B  (A0)+,D0
    CMP.B   #"1",D0
    BEQ.S   .R
    CMP.B   #"0",D0
    BNE.S   .1
.R: MOVE.B  D0,(A2)+
    BRA.S   .4
.1: CMP.L   A2,D3
    BEQ.S   .5
    MOVEQ   #0,D0
    MOVE.B  -(A2),D0
    SUB.B   #"0",D0
    BEQ.S   .NS
    BSET    D2,D1
.NS:    CMP.W   #32,D2
    BEQ iERROR7
    ADDQ.L  #1,D2
    BRA.S   .1
.5: MOVEQ   #VALUE,D4
    MOVE.W  D4,(A1)+
    MOVE.L  D1,(A1)+
    SUBQ.L  #1,A0
    MOVE.L  WORK,A2
    BRA PARSELOOP
;*-*
;; STR
PARSELONGSTR:
    MOVEQ   #0,D0           ; RESULT
    MOVEQ   #24,D1          ; SHIFTCOUNT
.1: MOVEQ   #0,D2
    MOVE.B  (A0)+,D2
    CMP.B   #34,D2
    BEQ.S   .2
    CMP.B   #10,D2
    BEQ iERROR8
    CMP.L   #-8,D1
    BEQ iERROR8
    CMP.B   #"\",D2
    BEQ.S   .SP
.SPB:
    LSL.L   D1,D2
    ADD.L   D2,D0
    SUBQ.L  #8,D1
    BRA.S   .1
.2: ADDQ.L  #8,D1           ; 0,32=NOSHIFT
    CMP.W   #32,D1
    BPL.S   .3
    TST.W   D1
    BEQ.S   .3
    LSR.L   D1,D0
.3: MOVEQ   #VALUE,D4
    MOVE.W  D4,(A1)+
    MOVE.L  D0,(A1)+
    MOVE.L  WORK,A2
    BRA PARSELOOP
.SP:
    MOVE.B  (A0)+,D2
    CMP.B   #"x",D2
    bEQ     .EXTHEX
    LEA .SPT(PC),A3
.SPL:
    ADDQ.L  #1,A3
    CMP.B   (A3),D2
    BEQ.S   .SPD
    TST.B   (A3)+
    BNE.S   .SPL
    BRA iERROR26
.SPD:
    MOVE.B  -(A3),D2
    BRA.S   .SPB

.EXTHEX:
    MOVEM.L D0-D1/D3-D7/A1-A6,-(A7)
    MOVEQ   #1,D0
    MOVEQ   #0,D1
.LP:
    MOVE.B  (A0)+,D2
    CMP.B   #"0",D2
    BLT     iERROR72
    CMP.B   #"9",D2
    BLE     .DEC
    CMP.B   #"A",D2
    BLT     iERROR72
    CMP.B   #"F",D2
    BLE     .HEX
    CMP.B   #"a",D2
    BLT     iERROR72
    CMP.B   #"f",D2
    BGT     iERROR72
    BCLR    #5,D2
.HEX:
    SUB.B   #"A"+10,D2
    AND.B   #$F,D2
    OR.B    D2,D1
    BRA     .X
.DEC:
    SUB.B   #"0",D2
    AND.B   #$F,D2
    OR.B    D2,D1
.X: LSL.W   #4,D1
    DBF     D0,.LP
    MOVE.L  D1,D2
    LSR.W   #4,D2
    MOVEM.L (A7)+,D0-D1/D3-D7/A1-A6
    BRA     .SPB



.SPT:   DC.B    10,"n",39,"a",34,"q",27,"e",9,"t","\","\",0,"0",13,"b",11,"v"
;*-*
;*-*

CONTINUETAB:
        DC.B    0,0,1,1,1,0,0           ; EOS .. STR
        DC.B    1,1,1,1,1,0,1,1,1,0,1,0 ; "+" .. ")"
        DC.B    0,0,0,1,1,0,0,0,0,0     ; ":" .. ".B"
        DC.B    1,0,0,1,0,0,1,1,0       ; "[" .. "|"
        DC.B    1,0,1,1,0               ; LIB .. OBJA
        DC.B    0,0,0,0,0,0,0,0,0,0,0,0 ; ..
        DC.B    0,0,0,0,0,0,0,0,0,0,0,0 ; ..
        DC.B    0,0,0,0,0,0,0,0,0,0,0,0 ; ..
        EVEN

PARSEENDLINE:
    CMP.L   ENDECODE(PC),A0
    BPL.S   .SKIP
    CMP.W   #IOFF+65,D4
    BPL.S   .SKIP
    TST.W   D4
    BMI.S   .SKIP
    CMP.L   #BRACKETSBUF,CURBRACK       ; OPEN [ ( COUNT
    BNE PCONTINUE
    TST.W   PARSEBRACKET
    BNE PCONTINUE
    CMP.W   #IOFF,D4
    BPL.S   .2TAB
    MOVE.B  CONTINUETAB(PC,D4.W),D0
    BNE PCONTINUE
    BRA.S   .1
.2TAB:
    MOVE.B  CONTINUEKEY-IOFF(PC,D4.W),D0
    BNE PCONTINUE
    BRA.S   .1
.SKIP:
    CMP.L   #BRACKETSBUF,CURBRACK
    BEQ.S   .1C
    MOVE.L  SQBRAKPOS(PC),D0
    BEQ.S   .1D
    MOVE.L  D0,A0
    MOVE.L  SQBRAKLINE(PC),LINENUM
.1D:BRA iERROR34
.1C:TST.W   PARSEBRACKET
    BEQ.S   .1
    MOVE.L  BRAKPOS(PC),D0
    BEQ.S   .1B
    MOVE.L  D0,A0
    MOVE.L  BRAKLINE(PC),LINENUM
.1B:BRA iERROR20
.1: MOVE.L  CONSTD(PC),D0
    BNE RESOLVECONST
RESBACK:
    BTST    #6,CODEPREFS+2      ; SEE IF EXPORT ALL
    BNE.S   .EA
    CLR.W   EXPORTFLAG
.EA:MOVE.L  A0,CURECODE
    MOVEQ   #0,D4
    MOVE.W  D4,(A1)+
    CMP.B   #10,-1(A0)
    BNE.S   .2
    ADDQ.W  #1,LINENUM      ; COUNT LINES
    MOVE.W  LINENUM(PC),LINENUM2
.2: MOVE.L  STARTINTERIM(PC),A0 ; SEE IF WE PARSED AN EMPTY LINE
    CMP.L   A1,A0
    BEQ.S   .1
    MOVE.L  A1,CURINTERIM
    ADDQ.W  #1,STATS
    BRA.S   .3
.1: SUBQ.L  #6,A0
    MOVE.L  A0,CURINTERIM
.3: BSR POSTOPTI
    RTS             ; THE ONLY WAY OUT

CONTINUEKEY:
        DC.B    1,0,1,0,1,1,0,1,0,1,1,0 ; PROC .. DEFA
        DC.B    0,0,1,1,1,1,0,1,0,0,0,1 ; ENDS .. INC
        DC.B    1,1,0,0,0,1,1,1,1,0,0,0 ; DEC  .. FULL
        DC.B    0,0,0,1,1,0,0,1,1,1,1,0 ; LARG .. LIST
        DC.B    1,0,1,0,1,1,1,1,0,0,1,0 ; OBJE .. EXPO
        DC.B    0,1,1,1,0,0,0,0,1,0,0,0 ; REG  .. LINK
        DC.B    1,0,1,0,0,0,0,0,0,0,0,0 ; EXTR .. CODE
        DC.B    0,0,0,0,0,0,0,0,0,0,0,0 ; DATA ..
        EVEN


; ALSO CALLED FROM PREP!!!

MOVEBUF:
    MOVE.L  A1,CURINTERIM       ; FOR CHECK.
    MOVE.L  A1,D7           ;
    BSR REALLOC1        ; CHECK DOUBLECHECK
    MOVE.L  CURINTERIM(PC),A1   ;
    SUB.L   A1,D7           ;
    BEQ.S   .1          ;
    NEG.L   D7          ;
    ADD.L   D7,STARTINTERIM     ;
    TST.L   CONSTD          ;
    BEQ.S   .1          ;
    ADD.L   D7,CONSTD       ;
.1: RTS

PCONTINUE:
    BSR.S   MOVEBUF
    CMP.B   #10,-1(A0)
    BNE PARSELOOP
    ADDQ.W  #1,LINENUM      ; COUNT LINES
    BRA PARSELOOP

REBACK:
    MOVE.W  #1,ERRWHERE
    MOVE.L  LINENUMZ,LINENUM
    BRA RESBACK

STATS:    DC.W    0

;; RESOLVECONST
RESOLVECONST:
    ADDQ.W  #1,STATS
    MOVE.W  #2,ERRWHERE     ; A1 NOW
    MOVE.L  LINENUM(PC),D6
    MOVE.L  LINENUMZ(PC),LINENUM
    MOVE.L  D6,LINENUMZ
    MOVE.L  A1,D6           ; D6=ENDPARSE
    MOVEQ   #0,D4
    MOVE.W  D4,(A1)+
    MOVE.L  D0,A1
    MOVE.L  D0,A3
    MOVE.L  A3,INTERMED     ; !!!!
    MOVE.W  (A3)+,D7        ; INS. CODE
    CLR.L   CONSTD
    CMP.W   #IOFF,D7
    BEQ PROCPARSE
    CMP.W   #IOFF+32,D7
    BEQ .OPT
    CMP.W   #IOFF+33,D7
    BEQ MODULELOAD
    CMP.W   #IOFF+76,D7
    BEQ LIBLOAD
    CMP.W   #IOFF+53,D7
    BEQ DOENUM
    CMP.W   #IOFF+54,D7
    BEQ DOSET
    CMP.W   #IOFF+58,D7
    BEQ DORAISE
    CMP.W   #IOFF+68,D7
    BEQ DOLIBRARY
.1: TST.W   INPROC
    BNE iERROR35
;; CONST
    GETM    A4          ; CONST
    
    TST.L   ConstsList
    BNE.S   .a
    MOVE.L  A4,ConstsList
.a: TST.L   ConstTemp
    BEQ.S   .z
    MOVe.L  ConstTemp,A6
    MOVE.L  A4,(A6)
.z: MOVe.L  A4,ConstTemp
    CLR.L   (A4)
    ADDQ.L  #8,A4
    DONEM   A4
    MOVE.L  A4,A6
    GETM    A4
    MOVE.L  A4,-(A6)

    CMP.W   #25,(A3)+
    BNE     iERROR0
    MOVE.L  (A3)+,4(A4)
    MOVE.L  (A3)+,A6
    MOVE.L  (A6),(A4)
    MOVE.L  A4,(A6)
    ADDQ.L  #8,A4
    CMP.W   #11,(A3)+
    BNE iERROR0
    MOVE.L  A0,-(A7)
    BSR ASM_GRABVALUE
    MOVE.L  (A7)+,A0
    CMP.W   #-1,D0
    beq     .STRING
    MOVE.L  D1,(A4)+
    CLR.W   (A4)+
    TST.W   EXPORTFLAG
    BEQ.S   .NE
    BSET    #0,-2(A4)
.NE:DONEM   A4
.MAINLOOP:
    CMP.W   #COM,(A3)+
    BEQ     .1
    TST.W   -2(A3)
    BNE iERROR0
    BRA REBACK

.STRING:    ; string constants here 8D
    cmp.w   #STR,(a3)
    bne     iERROR30
    move.l  a4,d0
    addq.l  #6,d0
    move.l  d0,(a4)+
    tst.w   EXPORTFLAG
    sne     D0
    and.b   #$1,d0
    or.b    #2,d0
    move.b  d0,(a4)+
    clr.b   (a4)+
    moveq   #0,d7
    move.l  a4,a6
    clr.w   (a4)+
.LP:cmp.w   #STR,(a3)+
    bne     iERROR30
    moveq   #0,d0
    moveq   #0,d1
    move.w  (a3)+,d0
    add.w   d0,d7
    move.w  (a3)+,d1
    lsl.w   #1,d1
    sub.l   d0,d1
    subq.l  #1,d0
.C: move.b  (a3)+,(a4)+
    dbf     d0,.C
    add.l   d1,a3
    cmp.w   #7,(a3)+
    beq     .LP
    subq.l  #2,a3
    clr.b   (a4)+
    move.l  a4,d0
    btst    #0,d0
    beq     .CE
    clr.b   (a4)+
.CE:DONEH   A4
    move.w  d7,(a6)
    bra     .MAINLOOP


;*-*
;; OPTS
.OPT:   CMP.W   #1,STATS        ; OPT
    BNE iERROR35
    CLR.W   STATS
.2: MOVE.W  (A3)+,D7
    CMP.W   #VALUE,D7
    BEQ .CPU
;; STACK
    CMP.W   #IOFF+34,D7     ; --> STACK=X
    BNE.S   .4B
    CMP.L   #$B0001,(A3)+
    BNE iERROR0
    MOVE.L  (A3)+,D7
    CMP.L   #MINSTACKSIZE,D7
    BMI iERROR31
    BSET    #0,CODEPREFS+3
    MOVE.L  D7,CODESTACK
    BRA .5
;*-*
;; PREPROCESS
.4B:
    CMP.W   #IOFF+67,D7     ; --> PREPROCESS
    BNE.S   .4BB
    BSET    #7,CODEPREFS+1
    BRA .5
;*-*
;; EXPORT
.4BB:
    CMP.W   #IOFF+59,D7     ; --> EXPORT
    BNE.S   .4C
    BSET    #6,CODEPREFS+2
    BRA .5
;*-*
;; RTD
.4C:
    CMP.W   #256+117,D7        ; --> RTD
    BNE.S   .4D
    CMP.W   #0,(A3)+
    BNE iERROR0
    ;BSET   #0,CODEPREFS+1      ; probs with hooks/callbacks etc.
    BRA .5
;*-*
;; REG
.4D:
    CMP.W   #IOFF+60,D7     ; --> REG=X
    BNE.S   .6
    CMP.L   #$B0001,(A3)+
    BNE iERROR0
    MOVE.L  (A3)+,D7
    TST.L   D7
    BMI iERROR31
    CMP.L   #MAXREGVARS+1,D7
    BPL iERROR31
    MOVE.W  D7,MAXREGALLOC
    BRA .5
;*-*
;; ASM
.6: CMP.W   #IOFF+37,D7     ; --> ASM
    BNE.S   .7
    BSET    #3,CODEPREFS+3
    MOVE.W  #-1,SCOPE       ;     NO NEED OF SCOPES ANYMORE
    MOVE.W  #-1,MAINF       ;     ... AND NO MAIN EITHER
    BRA .5
;*-*
;; NOWARN
.7: CMP.W   #IOFF+38,D7     ; --> NOWARN
    BNE.S   .8
    BSET    #4,CODEPREFS+3
    BRA .5
;*-*
;; DIR
.8: CMP.W   #IOFF+43,D7     ; --> DIR
    BNE.S   .9
    CMP.L   #$B0006,(A3)+
    BNE iERROR0
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,D0
    MOVE.L  DIRNAMEX(PC),A4
    SUBQ.W  #1,D0
.XL:MOVE.W  (A3)+,(A4)+
    DBRA    D0,.XL
    BRA     .5
;*-*
;; OSVERSION
.9: CMP.W   #IOFF+52,D7     ; --> OSVERSION
    BNE.S   .10
    BSET    #0,CODEPREFS+2
    CMP.L   #$B0001,(A3)+
    BNE iERROR0
    MOVE.L  (A3)+,D7
    CMP.L   #33,D7
    BMI iERROR31
    CMP.L   #100,D7
    BPL iERROR31
    MOVE.W  D7,OSVERSION
    BRA     .5
;*-*
;; LARGE
.10:
    CMP.W   #IOFF+36,D7     ; --> LARGE
    BNE.S   .11
    BSET    #2,CODEPREFS+3
    BRA     .5
;*-*
;; MODULE
.11:
    CMP.W   #IOFF+33,D7     ; --> MODULE (OPT)
    BNE .UTIL
    BSET    #4,CODEPREFS+2
    BRA     .5
;*-*
;; UTILLIB
.UTIL:
    CMP.W   #IOFF+69,D7
    BNE     .POOL
    CMP.W   #37,OSVERSION
    BLT     iERROR78
    BSET    #7,CODEPREFS
    JSR     SETUPUTIL
    BRA     .5
;*-*
;; POOL
.POOL:
    CMP.W   #IOFF+70,D7
    BNE     .LINK
    CMP.W   #39,OSVERSION
    BLT     iERROR78
    BSET    #6,CODEPREFS
    CMP.W   #17,(A3)
    BNE     .5
    ADDQ.L  #2,A3
    CMP.W   #VALUE,(A3)+
    BNE     iERROR73
    MOVE.L  (A3)+,OPENPOOL+2
    CMP.W   #COM,(A3)
    BNE     .PX
    ADDQ.L  #2,A3
    CMp.W   #VALUE,(A3)+
    BNE     iERROR73
    MOVE.L  (A3)+,OPENPOOL+8
    CMP.W   #COM,(A3)
    BNE     .PX
    ADDQ.L  #2,A3
    CMP.W   #VALUE,(A3)+
    BNE     iERROR73
    MOVE.L  (A3)+,OPENPOOL+14
.PX:CMP.W   #18,(a3)+
    BNE     iERROR74
    BRA     .5
;*-*
;; LINKABLE
.LINK:
    CMP.W   #IOFF+72,D7
    BNE     .NOST
    TST.L   LIBINFO
    BNE     iERROR50
    BSET    #5,CODEPREFS
    BRA     .5
;*-*
;; NOSTARTUP
.NOST:
    CMP.W   #IOFF+71,D7
    BNE     .INLINE
    BSET    #4,CODEPREFS
    BRA     .5
;*-*
;; INLINE
.INLINE:
    CMP.W   #IOFF+74,D7
    BNE     .STARTUP
    BSET    #7,ICODEPREFS+3
    BRA     .5
;*-*
;; STARTUP
.STARTUP:
    CMP.W   #IOFF+77,D7
    BNE     .FPEXP

    BTST    #2,CODEPREFS
    BNE iERROR50
    BSET    #2,CODEPREFS
    MOVEQ   #0,d2
    CMP.W   #11,(A3)+
    BNE     iERROR0
    CMP.W   #STR,(A3)+
    BNE     iERROR0
    ADDQ.L  #2,A3
    move.w  (A3)+,d2
    SUBQ.W  #1,D2
    LEA     STCNAME,A4
.LOOP:
    MOVE.W  (A3)+,(A4)+
    DBRA D2,.LOOP

    BRA .5
;*-*
;; FPEXP
.FPEXP
    cmp.w   #IOFF+81,D7
    BNE .RUNBG
    tst.w   EFPU
    beq iERROR53
    bset    #1,CODEPREFS
    bra .5
;*-*
;; RUNBG
.RUNBG:
    cmp.w   #IOFF+89,D7
    bne .CODE

    MOVE.L  #BGE-BGS,D6

    CLR.L   bgName
    CLR.L   bgPri
    bset #4,ICODEPREFS+3
    CMP.W #37,OSVERSION
    BLT iERROR78
    CMP.W   #17,(A3)
    BNE .RUNBG4
    ADDQ.L  #2,A3

    ADDQ.L  #8,D6

    CMP.W   #STR,(A3)+
    BNE iERROR0
    ADDQ.W  #2,A3
    MOVEQ   #0,D2
    MOVE.W  (A3)+,D2
    LEA PROCBUF,A4
    MOVE.L  A4,bgName
    SUBQ.L  #1,D2
.RUNBG1:
    MOVE.W  (A3)+,(A4)+
    DBRA    D2,.RUNBG1
    CMp.W   #COM,(A3)
    BNE     .RUNBG2

    ADDQ.L  #2,A3
    CMP.W   #8,(A3)
    BNE     .RUNBG3
    ADDQ.L  #2,A3
    CMP.W   #VALUE,(A3)
    BNE iERROR30
    NEG.L   2(A3)
.RUNBG3:
    CMP.W   #VALUE,(A3)+
    BNE iERROR30
    MOVE.L  (A3)+,D0
    CMP.L   #128,D0
    BPl iERROR31
    CMP.L   #-128,D0
    BMI iERROR31
    MOVE.L D0,bgPri
.RUNBG2:
    CMP.W   #18,(A3)+
    BNE iERROR74
.RUNBG4:
    TST.L   bgPri
    BEQ .RUNBG5
    ADDQ.L  #8,D6
.RUNBG5:
    ADD.L   D6,A4STORAGEADR
    bra .5
;*-*
;; CODE
.CODE:
    CMP.W   #IOFF+84,d7
    bne     iERROR0
    cmp.w   #IOFF+87,(a3)
    bne     .CODE2
    addq.l  #2,a3
    bset    #1,ICODEPREFS+3
    bra     .5
.CODE2:
    cmp.w   #IOFF+92,(a3)
    bne     iERROR0
    ADDQ.L  #2,A3
    bset    #0,ICODEPREFS+3
    bra     .5
;*-*
.5: CMP.W   #COM,(A3)+
    BEQ .2
    TST.W   -2(A3)
    BNE iERROR0
    JSR     PATCHER
    BRA REBACK
;; CPU
.CPU:
    MOVE.L  (A3)+,D7        ; --> OPT 040
    LEA CPUTAB(PC),A6
    CMP.L   #603,D7
    BPL     .PPC
.CPUL:
    CMP.W   #-1,(A6)
    BEQ iERROR31
    ADD.L   #10,A6
    CMP.W   (A6),D7
    BNE.S   .CPUL
    MOVE.W  (A6)+,D0
    cmp.w   #060,d0
    bgt .CPU2
    move.w  D0,TCPU
.CPU2:
    MOVE.W  (A6)+,D0
    BEQ.S   .C1
    TST.W   ECPU
    BNE iERROR50
    MOVE.W  D0,ECPU
.C1:MOVE.W  (A6)+,D0
    BEQ.S   .C2
    TST.W   EFPU
    BNE iERROR50
    MOVE.W  D0,EFPU
.C2:MOVE.W  (A6)+,D0
    MOVE.W  ASMCPU(PC),D1
    BSET    D0,D1
    MOVE.W  D1,ASMCPU
    MOVE.W  (A6)+,D0
    MOVE.W  D0,EMMU
    BRA     .5
.PPC:
    CMP.L   #605,D7
    BPL     .CPUL
    MOVE.W  #-1,GENERATE_PPC
    BSET    #0,CODEPREFS
    MOVE.W  D7,PCPU
    BRA     .5
;*-*
;*-*
;; MODULE
MODULELOAD:
    TST.W   SCOPE           ; global scope only!
    BNE iERROR35             ;
    MOVEM.L A0/A1,-(A7)     ; MODULE    ; D4 GETRASHED!
MODULELOOP:
    CLR.L   THISMOD         ;
    MOVE.L  DOSBASE,A6      ;
    CMP.W   #STR,(A3)+      ;
    BNE iERROR0              ;
    ADDQ.L  #2,A3           ;
    MOVE.W  (A3)+,D0        ; string length (in words)
    MOVE.L  A3,A0           ; A0=MOD name
    MOVE.L  A0,A1           ;
    LOWER   A1,D1           ;
BLAAAAAARGH:
    EXT.L   D0              ;
    LSL.L   #1,D0           ; string length
    ADD.L   D0,A3           ;
    CMP.B   #"*",(A0)       ; check if in cdir
    BNE.S   .STD            ;
    ADDQ.L  #1,A0           ;
    MOVE.L  #PRINTBUF,D1    ;
    CLR.B   PRINTBUF        ;
    BRA.S   .CUR            ;
.STD:
    MOVE.L  DIRNAMEX(PC),D1 ; directory name
.CUR:
    MOVE.L  D1,D6           ; BOTH MODPATH
    MOVE.L  D1,A1           ;
    LOWER   A1,D7           ;
    MOVE.L  A1,D7           ; D7=NAMEOFFSET
    MOVE.B  -1(A1),D0       ;
    CMP.L   A1,D1           ;
    BEQ.S   .2              ;
    CMP.B   #':',D0         ;
    BEQ.S   .2              ;
    CMP.B   #'/',D0         ;
    BEQ.S   .2              ;
    MOVE.B  #'/',(A1)+      ; the path :)
.2: MOVE.B  (A0)+,(A1)+     ; copy
    BNE.S   .2              ;
    MOVE.B  #'.',-1(A1)
    MOVE.B  #'m',(A1)+
    MOVE.B  #0,(A1)+
    MOVE.L  D1,CACHENAMESTART
    MOVE.L  A1,CACHENAMEEND
    BSR SEARCHINMODLIST     ; CHECK CIRC. INCLUSIONS ETC.
    TST.L   D0
    BEQ.S   .1
    MOVE.L  D0,A0
    BTST    #0,MI_FLAGS(A0)
    BEQ .NEXT
    BCLR    #0,MI_FLAGS(A0)
    MOVE.L  A0,THISMOD
.1: BSR SEARCHINCACHE       ; SEE IF MODULE IS ALREADY THERE
    MOVE.L  D0,A0
    TST.L   D0
    BNE.S   .GOTM

    MOVE.L  D6,D0
    BSR FILELENGTH
    MOVE.L  D0,D3
    BEQ iERROR36
    BMI iERROR36
    MOVE.L  #1005,D2
    JSR -30(A6)         ; OPEN
    MOVE.L  D0,D5           ; D5=HANDLE
    BEQ iERROR36
    MOVE.L  D3,D0
    ADDQ.L  #8,D0
    BSR NEWCACHE
    MOVE.L  D0,D2
    BEQ iERROR38
    MOVE.L  D5,D1
    JSR -42(A6)         ; READ
    MOVE.L  D0,D4
    MOVE.L  D5,D1
    JSR -36(A6)         ; CLOSE
    CMP.L   D3,D4
    BNE iERROR36
    MOVE.L  D2,A0
    ADD.L   D3,D2
    MOVE.L  D2,A1
    CLR.B   (A1)+
    CLR.B   (A1)+
    CLR.B   (A1)+
    CLR.B   (A1)+
    CLR.B   (A1)+
    CLR.B   (A1)+           ; max 8!
.GOTM:  MOVEM.L D0-D7/A0-A6,-(A7)
    TST.L   THISMOD
    BEQ.S   .3
    MOVE.L  THISMOD,A1
    MOVE.L  A0,MI_MOD(A1)
    BRA.S   .GM
.3: GETM    A1              ; CREATE A MODINFO
    MOVE.L  A1,THISMOD
    MOVE.L  MODINFOLIST(PC),(A1)        ; MI_NEXT
    MOVE.L  A1,MODINFOLIST
    ADDQ.L  #4,A1
    CLR.W   (A1)+               ; MI_FLAGS
    MOVE.L  CACHENAMEEND(PC),A2
    SUB.L   CACHENAMESTART(PC),A2
    MOVE.L  A2,(A1)+            ; MI_NAMELEN
    MOVE.L  A0,(A1)+            ; MI_MOD
    CLR.L   (A1)+               ; MI_LIST
    LEA 4(A1),A2
    MOVE.L  A2,(A1)+            ; MI_NAMEPTR
    MOVE.L  CACHENAMESTART(PC),A2       ; MI_NAME
.XL:MOVE.B  (A2)+,(A1)+
    BNE.S   .XL
    MOVE.L  A1,D0
    BTST    #0,D0
    BEQ.S   .E
    CLR.B   (A1)+
.E: DONEH   A1
.GM:    CMP.L   #"EMOD",(A0)+
    BNE iERROR36
    JSR MODULEJOBENTRY
    MOVEM.L (A7)+,D0-D7/A0-A6
.NEXT:  MOVE.L  D7,A0
    MOVE.B  #0,(A0)+
    CMP.W   #COM,(A3)+
    BEQ MODULELOOP
    TST.W   -2(A3)
    BNE iERROR0
    MOVEM.L (A7)+,A0/A1
    BRA REBACK

MODINFOLIST:  DC.L    0

; GETS NAME FROM CACHENAMESTART, RETURNS MODINFO OR NIL IN D0

SEARCHINMODLIST:
    MOVEM.L D1-D2/A0-A2,-(A7)
    LEA MODINFOLIST(PC),A0  ; A0=MODLIST
    MOVE.L  CACHENAMESTART(PC),D1   ; D1=NAME
    MOVE.L  CACHENAMEEND(PC),D2
    SUB.L   D1,D2           ; D2=LEN
.XL:MOVE.L  (A0),D0
    BEQ.S   .X
    MOVE.L  D0,A0
    CMP.L   MI_NAMELEN(A0),D2
    BNE.S   .XL
    MOVE.L  MI_NAMEPTR(A0),A1
    MOVE.L  D1,A2
.CMP:   CMPM.B  (A1)+,(A2)+
    BNE.S   .XL
    TST.B   -1(A1)
    BNE.S   .CMP
    MOVE.L  A0,D0
.X: MOVEM.L (A7)+,D1-D2/A0-A2
    RTS

AGAIN:    DC.W    0

SECONDARYLOAD:
.ML:    CLR.W   AGAIN
    MOVE.L  DOSBASE,A6      ; A6=BASE
    LEA MODINFOLIST(PC),A5  ; A5=LIST
.XL: MOVE.L  (A5),D0
    BEQ.W   .X
    MOVE.L  D0,A5
    BTST    #0,MI_FLAGS(A5)
    BEQ.S   .XL
    BCLR    #0,MI_FLAGS(A5)

    MOVE.L  MI_NAMEPTR(A5),D0
    MOVE.L  D0,ERROROBJ
    MOVE.L  D0,CACHENAMESTART
    ADD.L   MI_NAMELEN(A5),D0
    MOVE.L  D0,CACHENAMEEND
    BSR SEARCHINCACHE
    MOVE.L  D0,A0
    TST.L   D0
    BNE.S   .GOTM

    MOVE.L  MI_NAMEPTR(A5),D0
    BSR FILELENGTH
    MOVE.L  D0,D3
    BEQ iERROR36
    BMI iERROR36

    MOVE.L  MI_NAMEPTR(A5),D1
    MOVE.L  #1005,D2
    JSR -30(A6)         ; OPEN
    MOVE.L  D0,D5           ; D5=HANDLE
    BEQ iERROR36

    MOVE.L  D3,D0
    ADDQ.L  #4,D0
    BSR NEWCACHE
    MOVE.L  D0,D2
    BEQ iERROR38
    MOVE.L  D5,D1
    JSR -42(A6)         ; READ
    MOVE.L  D0,D4
    MOVE.L  D5,D1
    JSR -36(A6)         ; CLOSE
    CMP.L   D3,D4
    BNE iERROR36
    MOVE.L  D2,A0
    ADD.L   D3,D2
    MOVE.L  D2,A1
    CLR.B   (A1)+
    CLR.B   (A1)+
    CLR.B   (A1)+
    CLR.B   (A1)+

.GOTM:  MOVE.L  A5,THISMOD
    MOVE.L  A0,MI_MOD(A5)
    CMP.L   #"EMOD",(A0)+
    BNE iERROR36
    MOVEM.L D0-D7/A0-A6,-(A7)
    JSR MINIMUMMODULE
    MOVEM.L (A7)+,D0-D7/A0-A6

    BRA.W   .XL
.X: TST.W   AGAIN
    BNE.W   .ML
    CLR.L   ERROROBJ
    RTS
;*-*
;; LIB/OBJECTS
LIBLOAD:
    TST.W   SCOPE
    BNE     iERROR30
    MOVEM.L A0/A1,-(A7)
    MOVE.L  DOSBASE,A6
.LIBLOOP1:
    CMP.W   #STR,(A3)+
    BNE     iERROR0
    ADDQ.L  #2,A3
    MOVEQ   #0,D0
    MOVE.W  (A3)+,D0

    move.l  a3,a0
    lsl.l   #1,d0
    add.l   d0,a3
    MOVE.L  #PRINTBUF,A1
    CLR.B   PRINTBUF

    MOVE.L  .ELIBPATH,(A1)+
    MOVE.B  #":",(A1)+
.LP:MOVE.B  (A0)+,(A1)+
    BNE     .LP

    MOVE.B  #".",-1(A1)
    MOVE.B  #"m",(A1)
    MOVE.B  #0,1(A1)

    MOVE.L  A1,.T
    MOVE.L  A3,-(A7)

    MOVE.L  #PRINTBUF,D1
    MOVE.L  D1,D0
    BSR FILELENGTH
    TST.L   D0
    BEQ     iERROR28
    BMI     iERROR28
    move.l  d0,-(a7)
    BSR NEW                 ; get mem
    move.l  d0,-(a7)
    move.l  #PRINTBUF,D1
    MOVE.L  #1005,D2
    JSR     -30(A6)
    MOVe.L  D0,D4
    move.l  (a7),D2
    move.l  4(a7),d3
    move.l  d4,d1
    jsr     -42(a6)
    move.l  d4,d1
    jsr     -36(a6)

    MOVE.L  .T,A1           ; now open lib
    MOVE.B  #"l",(A1)
    MOVE.B  #"i",1(A1)
    MOVE.B  #"b",2(A1)
    MOVE.B  #0,3(A1)

    MOVE.L  #PRINTBUF,D1
    MOVE.L  D1,D0
    BSR FILELENGTH
    TST.L   D0
    BEQ     iERROR28
    BMI     iERROR28
    move.l  d0,-(a7)
    BSR NEW                 ; get mem
    move.l  d0,-(a7)
    move.l  #PRINTBUF,D1
    MOVE.L  #1005,D2
    JSR     -30(A6)
    MOVe.L  D0,D4
    move.l  (a7),D2
    move.l  4(a7),d3
    move.l  d4,d1
    jsr     -42(a6)
    move.l  d4,d1
    jsr     -36(a6)


    move.l  (a7)+,d2        ; lib mem
    move.l  (a7)+,d3        ; lib size
    move.l  (a7)+,d0        ; mod mem
    move.l  (a7)+,d1        ; mod size
    JSR PARSEOBJECTFILE
    MOVE.L  (A7)+,A3
    cmp.w   #COM,(A3)+
    BEQ .LIBLOOP1
    TST.W   -(A3)
    BNE     iERROR0
    MOVEM.L (A7)+,A0/A1
    BRA REBACK

.ELIBPATH:
    DC.B    'ELIB'
.T: DC.L    0
;*-*
;; PROC
PROCPARSE:            ; PARSE 'PROC bla(a,b,c=1) OF o IS x\0'
    CLR.W   .REGCNT
    GETM    A6          ; DEFARGSLIST
    CLR.W   (A6)+
    MOVE.L  A6,.A6ST
    CLR.W   .CNT
    CMP.W   #IDENT,(A3)+
    BNE iERROR4
    MOVE.L  (A3)+,A2        ; A2=PROCNAMEVAR
    CMP.B   #LAB,4(A2)
    BNE iERROR4
    MOVE.W  #-2,10(A2)
    MOVE.L  6(A2),A4
    MOVE.L  A2,14(A4)       ; SET PROC.IDENT
    MOVE.L  A4,A2           ; A2=PTR PROC STRUCT
    ;BSET   #3,2(A2)
    ;BNE    iERROR14
    CMP.W   #17,(A3)+
    BNE iERROR23
    MOVEQ   #0,D1           ; D1=NARG
    MOVE.L  A3,A4           ; BACKUP
.1: MOVE.W  (A3)+,D0
    CMP.W   #IDENT,D0
    BNE .2
    ADDQ.L  #1,D1
    ADDQ.L  #4,A3

    CMP.W   #11,(A3)        ; CHECK FOR a=1 DEFARGS
    BNE.S   .NIS
    ADDQ.L  #2,A3
    cmp.b   #4,(a3)
    blt     .IS0
    cmp.b   #5,(a3)
    bgt     .IS0
    move.w  (a3)+,(a6)+
    bset    #5,2(A2)
    addq.w  #1,.REGCNT
    addq.w  #1,.CNT
    bra     .NIS
.REGCNT:    dc.w    0
.IS0:
    CMP.W   #8,(A3)
    BNE.S   .IS1
    ADDQ.L  #2,A3
    NEG.L   2(A3)
.IS1:
    CMP.W   #1,(A3)+
    BNE iERROR30
    ADDQ.W  #1,.CNT
    MOVE.L  (A3)+,(A6)+

.NIS:   BSR .SKIP
    MOVE.W  (A3)+,D0
    CMP.W   #COM,D0
    BEQ.S   .1
.2: CMP.W   #18,D0
    BNE iERROR6
    tst.w   .REGCNT
    beq     .2_2
    cmp.w   .REGCNT,D1
    bne     iERROR50
.2_2:
    CLR.W   (A6)+
    MOVE.L  A6,.A6MID
    MOVE.L  D6,A1
    MOVE.W  D1,(A2)
    BEQ.S   .4
    MOVE.L  A4,A3
    MOVEQ   #0,D2
    LSL.L   #2,D1
    ADDQ.L  #8,D1
.3: MOVE.W  (A3)+,D0
    CMP.W   #IDENT,D0
    BNE .4
    MOVE.L  (A3)+,A4        ; A4=VAR
    ADDQ.L  #4,D2
    MOVE.L  D1,D3
    SUB.L   D2,D3
    MOVE.W  D3,10(A4)

    TSTMOD
    BEQ.S   .NAN
    MOVE.L  (A4),(A6)+
.NAN:
    CMP.W   #11,(A3)
    BNE.S   .NIS2
    ADDQ.L  #2,A3
    CMP.B   #4,(A3)
    BLT     .IS00
    CMP.B   #5,(A3)
    BGT     .IS00
    ADDQ.L  #2,A3
    BRA     .NIS2
.IS00:
    CMP.W   #8,(A3)
    BNE.S   .IS11
    ADDQ.L  #2,A3
.IS11:  ADDQ.L  #6,A3

.NIS2:  BSR.W   .SKIP
    MOVE.W  (A3)+,D0
    CMP.W   #COM,D0
    BEQ.S   .3
.4: ADDQ.L  #2,.A6ST        ; EXIT, A3 JUST AFTER ")"
    CMPA.L  .A6ST(PC),A6
    BEQ.S   .EX         ; CHECK IF DEFARGS
    MOVE.L  A3,-(A7)
    MOVE.L  .A6ST(PC),A3
    MOVE.W  .CNT(PC),-4(A3)
    MOVE.L  .A6MID(PC),A3
    MOVE.L  A6,D0
    SUB.L   A3,D0
    LSR.L   #2,D0
    MOVE.W  D0,-2(A3)
    MOVE.L  (A7)+,A3
.22:    MOVE.L  .A6ST,D0
    SUBQ.L  #4,D0
    MOVE.L  D0,6(A2)        ; DEFARGSLIST IN PROCSTRUCT
    DONEH   A6

.EX:cmp.w   #IOFF+93,(a3)
    bne     .EX2
    addq.l  #2,a3
    bset    #4,2(a2)
.EX2: CMP.W   #IOFF+45,(A3)+      ; NOW CHECK FOR of_object
    BNE .NOM
    TST.W   .REGCNT
    BNE     iERROR50
    CMP.W   #31,(A3)+
    BNE .REX
    MOVE.L  (A3)+,A3        ; A3=OBJECTHEAD, A2=PROC
    BTST    #1,2(A3)
    BNE iERROR69
    MOVE.L  A3,10(A2)       ; SET PROC TO METHOD
    MOVE.L  -4(A2),PROCLIST     ; DISCONNECT FROM PROCLIST
    CLR.L   -4(A2)

    MOVE.L  14(A2),A6
    MOVE.L  (A6),D0
    MOVE.L  A3,A6
    MOVE.L  A0,-(A7)
    JSR FINDMETHOD      ; WANTS OBJ=A6,NAME=D0, TRASHES A0,
    MOVE.L  (A7)+,A0
    TST.L   D1          ;   RETURNS D1=METHOD | NIL
    BNE iERROR61

    GETM    A6
    MOVE.L  OMETHOD(A3),D2      ; BUILD NEW METHOD:
    MOVE.L  A6,OMETHOD(A3)
    MOVE.L  A6,22(A2)
    MOVE.L  D2,(A6)+        ; SET M_NEXT
    MOVE.L  A2,(A6)+        ; SET M_PROC
    MOVE.B  #MT_METHOD,(A6)+    ; SET M_TYPE
    CLR.B   (A6)+
    MOVEQ   #4,D0
    TST.L   D2
    BEQ.S   .F
    MOVE.L  D2,A4
    MOVE.W  M_OFF(A4),D0
    ADDQ.W  #4,D0
.F: MOVE.W  D0,(A6)+        ; SET M_OFF TO LAST M_OFF+4
    ADDQ.W  #4,D0
    MOVE.W  D0,ODEL(A3)
    MOVE.L  14(A2),A4       ; A4=IDENT
    MOVE.L  (A4),(A6)+      ; SET M_NAME
    BSET    #4,5(A4)        ; SET FLAGS OR METHOD
    BSET    #0,5(A4)
    DONEM   A6
    BSET    #1,2(A2)
    MOVE.L  CURSELF(PC),D0      ; NOW MAKE A "self" FOR THIS METHOD
    BNE.S   .GOTS
    GETM    A6
    MOVE.L  SELFHASHADR(PC),A4
    MOVE.L  (A4),(A6)+
    MOVE.L  A6,(A4)
    MOVE.L  A6,D0
    MOVE.L  #SELFNAME,(A6)+     ; IDENT.ASCII
    MOVE.W  #$0101,(A6)+        ; IDENT.TYPE/FLAGS
    MOVE.L  A2,(A6)+        ; IDENT.PROC
    CLR.W   (A6)+           ; IDENT.INFO

    TST.W   REGSTOP
    BNE.S   .CLH
    MOVE.L  #MIDHEAVY*3,(A6)+   ; VARHEAVY: USED 3x (FAKE)
.CLHB:
    DONEM   A6
    MOVE.L  D0,A6
    BRA.S   .GOTS2

.CLH:   CLR.L   (A6)+           ; SELF NOT IN REG
    BRA.S   .CLHB


.GOTS:  MOVE.L  D0,A6
    TST.B   4(A6)
    BNE iERROR50
    MOVE.B  #1,4(A6)
    BSET    #0,5(A6)
.GOTS2: MOVE.L  A6,18(A2)       ; HERE WE HAVE A VALID SELF IN A6
    BRA.S   .REX
.NOM:   BTST    #1,2(A2)
    BNE iERROR21
.REX:   CLR.L   CURSELF
    BRA REBACK
.SKIP:  MOVE.W  (A3)+,D0
    CMP.W   #19,D0
    BEQ.S   .SKIP
    CMP.W   #IOFF+20,D0
    BEQ.S   .S3
    CMP.W   #IOFF+44,D0
    BEQ.S   .S1
    CMP.W   #IOFF+61,D0
    BEQ.S   .S3
    SUBQ.L  #2,A3
.S3:    RTS
.S1:    CMP.W   #IOFF+39,(A3)+      ; PTR TO
    BNE iERROR33
    CMP.W   #IOFF+20,(A3)
    BPL.S   .S2
    CMP.W   #31,(A3)+
    BNE iERROR33
    ADDQ.L  #4,A3
    RTS
.S2:    CMP.W   #IOFF+23,(A3)+
    BPL iERROR33
    RTS
.CNT:   DC.W    0
.A6ST:  DC.L    0
.A6MID: DC.L    0
;*-*
;; ENUM
DOENUM:
    TST.W   INPROC
    BNE iERROR35
    MOVEQ   #0,D7
.1: GETM    A4          ; ENUM



    TST.L   ConstsList
    BNE.S   .a
    MOVE.L  A4,ConstsList
.a: MOVe.L  ConstTemp,A6
    CMPA.L  #0,A6
    BEQ.S   .z
    MOVE.L  A4,(A6)
.z: MOVe.L  A4,ConstTemp
    CLR.L   (A4)
    ADDQ.L  #8,A4
    DONEM   A4
    MOVE.L  A4,A6
    GETM    A4
    MOVE.L  A4,-(A6)



    CMP.W   #25,(A3)+
    BNE iERROR0
    MOVE.L  (A3)+,4(A4)
    MOVE.L  (A3)+,A6
    MOVE.L  (A6),(A4)
    MOVE.L  A4,(A6)
    ADDQ.L  #8,A4
    CMP.W   #11,(A3)        ; LOOK FOR BASE REDEF.
    BNE.S   .2
    ADDQ.L  #2,A3
    CMP.W   #8,(A3)
    BNE.S   .NM
    ADDQ.L  #2,A3
    CMP.W   #VALUE,(A3)
    BNE.S   .NM
    NEG.L   2(A3)
.NM:    CMP.W   #VALUE,(A3)+
    BNE iERROR0
    MOVE.L  (A3)+,D7
.2: MOVE.L  D7,(A4)+
    ADDQ.L  #1,D7
    CLR.W   (A4)+
    DONEM   A4
    TST.W   EXPORTFLAG
    BEQ.S   .NE
    BSET    #0,-2(A4)
.NE:    CMP.W   #COM,(A3)+
    BEQ     .1
    TST.W   -2(A3)
    BNE iERROR0
    BRA REBACK
;*-*
;; SET
DOSET:
    TST.W   INPROC
    BNE iERROR35
    MOVEQ   #1,D7
.1: GETM    A4



    TST.L   ConstsList
    BNE.S   .a
    MOVE.L  A4,ConstsList
.a: MOVe.L  ConstTemp,A6
    CMPA.L  #0,A6
    BEQ.S   .z
    MOVE.L  A4,(A6)
.z: MOVe.L  A4,ConstTemp
    CLR.L   (A4)
    ADDQ.L  #8,A4
    DONEM   A4
    MOVE.L  A4,A6
    GETM    A4
    MOVE.L  A4,-(A6)



    TST.L   D7          ; SEE IF SET>32
    BEQ iERROR7
    CMP.W   #25,(A3)+
    BNE iERROR0
    MOVE.L  (A3)+,4(A4)
    MOVE.L  (A3)+,A6
    MOVE.L  (A6),(A4)
    MOVE.L  A4,(A6)
    ADDQ.L  #8,A4
    MOVE.L  D7,(A4)+
    LSL.L   #1,D7
    CLR.W   (A4)+
    DONEM   A4
    TST.W   EXPORTFLAG
    BEQ.S   .NE
    BSET    #0,-2(A4)
.NE:    CMP.W   #COM,(A3)+
    BEQ     .1
    TST.W   -2(A3)
    BNE iERROR0
    BRA REBACK
;*-*
;; LIBRARY
DOLIBRARY:
    BTST    #5,CODEPREFS
    BNE iERROR50
    TST.W   SCOPE
    BNE iERROR35
    TSTMOD
    BNE iERROR50
    MOVE.L  LIBINFO(PC),D7
    BNE iERROR50
    CMP.W   #STR,(A3)+
    BNE iERROR0
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,D7
    LSL.W   #1,D7
    GETM    A4
    DONEH   A4
    GETM    A4
    MOVE.L  A4,A6           ; A6=LIBINFO
    MOVE.L  A4,LIBINFO
    MOVE.L  A3,(A4)+        ; LIBRARY NAME
    ADD.W   D7,A3
    CMP.W   #COM,(A3)+
    BNE iERROR5
    CMP.W   #VALUE,(A3)+
    BNE iERROR30
    MOVE.L  (A3)+,(A4)+     ; VERSION
    CMP.W   #COM,(A3)+
    BNE iERROR5
    CMP.W   #VALUE,(A3)+
    BNE iERROR30
    MOVE.L  (A3)+,(A4)+     ; REVISION
    CMP.W   #COM,(A3)+
    BNE iERROR5
    CMP.W   #STR,(A3)+
    BNE iERROR0
    ADDQ.L  #2,A3
    MOVE.W  (A3)+,D7
    LSL.W   #1,D7
    MOVE.L  A3,(A4)+
    ADD.W   D7,A3
    CLR.L   (A4)+
    CMP.W   #IOFF+73,(A3)
    BNE     .E
    ADDQ.L  #2,A3
    CMP.W   #VALUE,(A3)+    ; EXTRA!!!
    BNE     iERROR75
    MOVE.L  (A3)+,D7
    BMI     iERROR31
    CMP.L   #32000,D7
    BGT     iERROR31
    BTST    #0,D7
    BNE     iERROR75

    ADD.L   D7,InitTable
    ADD.W   D7,PatchLib1+2
    ADD.W   D7,PatchLib2+2
    ADD.W   D7,PatchLib3+2
    ADD.W   D7,GETTC+2
    ADD.W   D7,NEWTC+2
    ADD.W   D7,GETA4+6

.E: CMP.W   #IOFF+62,(A3)+
    BNE iERROR0
.X: CMP.W   #IDENT,(A3)+
    BNE iERROR4
    MOVE.L  (A3)+,(A4)+
    BSR PARSE_REGLIST
    CMP.W   #COM,(A3)+
    BEQ .X
    TST.W   -2(A3)
    BNE iERROR5
    MOVE.L  A4,D7
    SUB.L   A6,D7
    SUB.L   #20,D7
    DIVS.W  #20,D7          ; SIZE OF PROCS!
    MOVE.W  D7,18(A6)
    CLR.L   (A4)+
    MOVE.L  (A6),A5
    MOVE.L  A4,(A6)
.1: MOVE.B  (A5)+,(A4)+
    BNE.S   .1
    MOVE.L  12(A6),A5
    MOVE.L  A4,12(A6)
.2: MOVE.B  (A5)+,(A4)+
    BNE.S   .2
    MOVE.L  A4,D0
    BTST    #0,D0
    BEQ.S   .3
    CLR.B   (A4)+
.3: DONEH   A4
    BRA REBACK
;*-*
;; Library reglist parser
PARSE_REGLIST:            ; A3=INTERIM, A4=BUF
    CMP.W   #17,(A3)
    BNE.S   .X
    MOVEM.L D0/D1/D2,-(A7)
    MOVEQ   #0,D0           ; MASK
    MOVEQ   #0,D1           ; NARGS
    ADDQ.L  #2,A3
    CMP.W   #18,(A3)
    BEQ.S   .BR
.XL:MOVE.B  (A3)+,D2
    CMP.B   #4,D2
    BEQ.S   .1
    CMP.B   #5,D2
    BNE iERROR19
.1: MOVE.B  (A3),(A4)+
    MOVEQ   #15,D2          ; REVERSE MASK
    SUB.B   (A3)+,D2
    BSET    D2,D0
    BNE iERROR50
    ADDQ.L  #1,D1
    CMP.W   #18,(A3)
    BEQ.S   .BR
    CMP.W   #COM,(A3)+
    BNE iERROR5
    BRA.S   .XL
.BR:    ADDQ.L  #2,A3
    MOVEQ   #12,D2
    SUB.W   D1,D2
    BMI iERROR70
    ADD.W   D2,A4
    MOVE.W  D0,(A4)+
    MOVE.W  D1,(A4)+
    MOVEM.L (A7)+,D0/D1/D2
    RTS
.X: CMP.B   #6,(A3)
    BEQ.S   .AX
    ADD.W   #12,A4
    MOVE.L  #-1,(A4)+
    RTS
.AX:    MOVEM.L D0/D1,-(A7)
    MOVE.W  (A3)+,D0
    AND.W   #%111,D0
    ADDQ.W  #8,D0
    MOVE.B  D0,(A4)+
    ADD.W   #11,A4
    MOVEQ   #15,D1
    SUB.W   D0,D1
    MOVEQ   #0,D0
    BSET    D1,D0
    MOVE.W  D0,(A4)+
    MOVE.W  #1,(A4)+
    MOVEM.L (A7)+,D0/D1
    RTS

DORAISE:
    TST.W   SCOPE
    BNE iERROR35
.1: CMP.W   #VALUE,(A3)+
    BNE iERROR30
    MOVE.L  (A3)+,D7        ; RAISE ID
    CMP.W   #IOFF+2,(A3)+
    BNE iERROR0          ; WE NEED AN "IF"
    CMP.W   #22,(A3)+
    BNE .LIB            ; EFUNC

    MOVE.L  (A3)+,D0
    SUB.L   #EFUNCTAB,D0
    LSR.L   #4,D0
    MOVE.L  #EFUNCRAISE,A4
    MULU.W  #10,D0
    EXT.L   D0
    ADD.L   D0,A4
    BRA.S   .NEXT

.LIB:   CMP.W   #38,-2(A3)
    BNE iERROR0
    MOVE.L  (A3)+,A4        ; LIBRECORD
    MOVE.W  RAISENUM(PC),D0
    MOVE.W  D0,8(A4)
    CMP.W   #MAXLIBRAISE,D0
    BEQ iERROR37
    MULU    #10,D0
    EXT.L   D0
    MOVE.L  #LIBRAISE,A4
    ADD.L   D0,A4
    ADDQ.W  #1,RAISENUM

.NEXT:  MOVE.L  D7,2(A4)        ; SET EXCEPT ID
    CMP.L   #$110012,(A3)+      ; ()        A4 --> RECORD
    BNE iERROR23
    MOVE.W  (A3)+,D1
    CMP.W   #11,D1
    BMI iERROR0
    CMP.W   #17,D1
    BPL iERROR0
    MOVE.W  D1,(A4)
    CMP.W   #8,(A3)
    BNE.S   .2
    ADDQ.L  #2,A3
    CMP.W   #VALUE,(A3)
    BNE.S   .2
    NEG.L   2(A3)
.2: CMP.W   #VALUE,(A3)+
    BNE iERROR30
    MOVE.L  (A3)+,6(A4)
    CMP.W   #COM,(A3)+
    BEQ .1
    TST.W   -2(A3)
    BNE iERROR0
    CLR.W   BUSYRAISE       ; !
    BRA REBACK
;*-*
RAISENUM:   DC.W    0

OSVERSION:  DC.W    33
WARNINGS:   DC.L    0
CODEPREFS:  DC.L    0
CODESTACK:  DC.L    0
MAXREGALLOC:DC.W    0
ConstTemp:  DC.L    0

ICODEPREFS: DC.L    0
PCPU:       DC.W    0       ; 603, 604
GENERATE_PPC:
            DC.W    0       ; TRUE if generating PPC output.

TCPU:       DC.W    0       ; 000, 010, 020, 030, 040, 060.
ECPU:       DC.W    0       ; 0,1,2 = 000+, 020+, 040+
EFPU:       DC.W    0       ; 0,1,2 = FFP, 881/882, 040
EMMU:       DC.W    0       ; 0,1,2 = [-], 851, 020+
ASMCPU:     DC.W    0       ; BITSET

CPUTAB:
    DC.W    000,0,0,0,0       ;
    DC.W    010,0,0,0,0       ; CPU,ECPUTYPE,EFPUTYPE,ASMCPUBIT#,EMMUTYPE
    DC.W    020,1,0,1,2
    DC.W    030,1,0,2,2
    DC.W    040,2,2,3,2
    DC.W    060,2,3,7,2
    DC.W    881,0,1,8,0
    DC.W    882,0,1,9,0
    DC.W    851,0,0,10,1
    DC.W    -1

STARTUP_CODE:
    DC.L    0
STARTUP_SIZE:
    DC.L    0
STARTUP_XTNS:
    DC.L    0
STARTUP_NODE:
    DC.L    0
;*-*

PARSEGETINSNR:            ; GETS INSNR OUTOF WORKBUF
    MOVEQ   #0,D0           ; INSNR (MUST BE <>0 AT END)
    MOVE.W  CURINS(PC),-(A7)
    MOVEQ   #-1,D1
    MOVE.W  D1,CURINS
    MOVE.L  HASHVAL(PC),D7
    LSL.L   #2,D7
    ADD.L   #KEYHASH,D7
    MOVE.L  D7,A6
.LOOP:  MOVE.L  (A6),A6         ; A6 _NOW_ POINTS TO ENTRY IN INSTABLE
    MOVE.L  A6,D6
    BEQ.S   .4
    MOVE.L  WORK,A4
    MOVE.L  (A6)+,A5
.2: CMPM.B  (A4)+,(A5)+
    BNE.S   .LOOP
    TST.B   -1(A4)
    BNE.S   .2
    SUB.L   #INSTABLE,D6
    LSR.L   #3,D6
    MOVE.L  D6,D1
    MOVE.W  #IOFF,D0
    ADD.W   D1,D0           ; STAY: 0..20, 50..59, 62..
    CMP.W   #IOFF+20,D0
    BPL.S   .4
    MOVE.W  D1,CURINS
.3: ADDQ.L  #2,A7
    RTS
.4: CMP.W   #IOFF+50,D0
    BPL.S   .5
.6: MOVE.W  (A7)+,CURINS
    RTS
.5: CMP.W   #IOFF+60,D0
    BPL.S   .7
    BRA.S   .3
.7: CMP.W   #IOFF+62,D0
    BPL.S   .3
    BRA.S   .6

MAKEKEYHASH:
    MOVE.L  #KEYHASH,A0     ; A0=TABEL
    LEA INSTABLE,A1
    MOVE.L  A0,A2
    MOVE.L  #255,D0
.CL:    CLR.L   (A2)+           ; CLEAR TABLE
    DBRA    D0,.CL
.LOOP:  MOVE.L  (A1),D0
    BEQ.S   .EX
    MOVE.L  D0,A2
    HASH    A2,D0,D1
    LSL.L   #2,D0

    LEA 0(A0,D0.L),A2
    MOVE.L  (A2),D0         ; READ PREVIOUS LIST (OR NIL)
    MOVE.L  A1,(A2)         ; PUT ENTRY IN LIST
    MOVE.L  D0,4(A1)        ; AND LINK TAIL TO IT
    ADDQ.L  #8,A1
    BRA.S   .LOOP
.EX:    RTS

CURINS: DC.W    0
;*-*

