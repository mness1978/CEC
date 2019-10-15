;; Errors/Warnings/Messages/Dos support
; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The Error, Message And DosSupport Part          ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;

ERROR:                ; D0=ERRORNR.
    BSR PREPMACROERROR
    MOVE.W  #5,CURSPOT
    MOVE.L  A0,SAVEDREGS
    MOVE.L  A3,SAVEDREGS+4
    MOVE.L  D0,D7
    BSR SHOWIDENT
    MOVE.L  #ERTEXT,D2
    MOVE.L  #ERTEXTEND-ERTEXT,D3
    BSR.W   WRITECON        ; WRITE 'ERROR: '
    MOVE.W  D7,D0
    ASL.W   #2,D0
    LEA ERRORTAB,A0
    MOVE.L  0(A0,D0.W),D2       ; FIND ERSTRING
    MOVE.L  D2,A0
.5: TST.B   (A0)+
    BNE .5
    SUBQ.L  #1,A0
    MOVE.L  A0,D3
    SUB.L   D2,D3
    BSR.W   WRITECON        ; WRITE 'YO SYNTAX SUCKS'
    BSR.W   WRITELN
    MOVE.W  LINENUM2(PC),D4
    BNE.S   .6
    MOVE.W  LINENUM,D4
    CMP.W   #-1,D4
    BEQ.S   .4
.6: MOVE.W  D4,RETERROR
    MOVE.L  ECODE(PC),A0
    MOVE.W  D4,D2
    BEQ.S   .1
    BMI.S   .1
    SUBQ.W  #1,D2
    MOVEQ   #10,D0
.XL:CMP.B   (A0)+,D0        ; FIND LINE
    BNE.S   .XL
    DBRA    D2,.XL
.1:
    BSR RECONSTRUCT     ; !!!!

    MOVE.L  A0,D0           ; BACKUP A0
.1B:    CMPI.B  #0,(A0)         ; find eol
    BEQ.S   .2
    CMPI.B  #10,(A0)+
    BNE.S   .1B
.3: CLR.B   -(A0)
    MOVE.L  A0,D3
    SUB.L   D0,D3
    LEA ERARGS(PC),A1
    MOVE.L  D0,4(A1)
    LEA ERLINE(PC),A0
    ADDQ.W  #1,D4
    EXT.L   D4
    MOVE.L  D4,(A1)
    BSR.W   WRITEFORMAT
.4: MOVE.L  ERROROBJ(PC),D0
    BEQ.S   .X
    LEA ERARGS(PC),A1       ; WRITE 'WITH: '
    MOVE.L  D0,(A1)
    LEA EROBJLINE(PC),A0
    BSR WRITEFORMAT
.X: BSR PREPWRITEMACROLINE
    RTS
.2: ADDQ.L  #1,A0
    BRA.S   .3

RECONSTRUCT:          ; GETS START LINE IN A0, RETURNS NEW
    MOVE.L  A0,LINEOFF      ; ADR. IN A0, TRASHES NOTHING
    MOVEM.L D0-D4/A1-A4,-(A7)
    MOVE.W  ERRWHERE(PC),D0
    BEQ .EXIT
    CMP.W   #1,D0
    BEQ.S   .LA0
    BSR CONSTRUCT
    TST.L   D0
    BEQ.S   .GOTP
    BRA .EXIT
.LA0:   MOVE.L  SAVEDREGS(PC),ERPOINT   ; ERROR IS IN LEX
.GOTP:  MOVE.L  ERPOINT(PC),D0
    MOVE.L  ECODE,D1
    SUB.L   D0,D1
    NEG.L   D1
    MOVE.L  D1,BYTEOFF
    MOVE.L  LINEOFF(PC),A0
    MOVE.L  ERBUF(PC),A1
    MOVEQ   #0,D1           ; 0=SEARCH,1=ON,2=AFTER ERPOS
.1: CMP.L   A0,D0
    BNE.S   .2
    LEA .ON(PC),A2      ; START OF ERPOS
.3: MOVE.B  (A2)+,(A1)+
    BNE.S   .3
    SUBQ.L  #1,A1
    MOVEQ   #1,D1
.2: MOVE.B  (A0)+,D2        ; READ BYTE
    BEQ.S   .NL         ; NL ?
    CMP.B   #10,D2
    BEQ.S   .NL         ; NL ?
    MOVE.B  D2,(A1)+
    CMP.W   #1,D1
    BNE.S   .1
    MOVEQ   #2,D1           ; END OF ERPOS
    CMP.B   #9,-1(A1)
    BNE.S   .NT
    MOVE.B  #" ",-1(A1)     ; A TAB? --> EXTRA SPACE
    MOVE.B  #9,(A1)+
.NT:    LEA .OFF(PC),A2
.5: MOVE.B  (A2)+,(A1)+
    BNE.S   .5
    SUBQ.L  #1,A1
    BRA.S   .1
.NL:    CMP.W   #1,D1
    BMI.S   .NL0
    BEQ.S   .NL1
    CLR.B   (A1)+           ; END OF STRING + ER
    MOVE.L  ERBUF(PC),LINEOFF
    BRA.S   .EXIT
.NL0:   MOVE.L  ERBUF(PC),A1        ; NEXT STRING
    BRA.S   .1
.NL1:   MOVE.B  #" ",(A1)+      ; END OF STRING
    LEA .OFF(PC),A2
.4: MOVE.B  (A2)+,(A1)+
    BNE.S   .4
    MOVE.L  ERBUF(PC),LINEOFF
.EXIT:  MOVEM.L (A7)+,D0-D4/A1-A4
    MOVE.L  LINEOFF(PC),A0
    RTS
.ON:    DC.B    27,"[43;32m",0
.OFF:   DC.B    27,"[0m",0

LINEOFF:    DC.L    0
ERRWHERE:   DC.W    0       ; 0=?, 1=A0.ASC, 2==A3.INT
INTERMED:   DC.L    0
ERPOINT:    DC.L    0
SAVEDREGS:  DC.L    0,0     ; A0,A3
XTRALINES:  DC.W    0
ERBUF:      DC.L    ESTACKBUF
BYTEOFF:    DC.L    0

CONSTRUCT:
    MOVE.L  SAVEDREGS+4(PC),D3  ; D3=INTERIM ERPOINT
    BTST    #0,D3
    BEQ.S   .EVEN
    ADDQ.L  #1,D3
.EVEN:  MOVE.L  INTERMED(PC),A3     ; A3=INTERIM START
    MOVE.L  LINEOFF(PC),A0      ; A0=ASCII START
.LOOP:  CMP.L   D3,A3
    BPL .EXIT
.EAT:   MOVE.B  (A0)+,D0        ; COLLECT WHITESPACE
    BEQ .ERR
    CMP.B   #" ",D0
    BEQ.S   .EAT
    CMP.B   #9,D0
    BEQ.S   .EAT
    CMP.B   #10,D0
    BEQ.S   .EAT
    CMP.B   #"-",D0
    BEQ.S   .SCOM
    CMP.B   #"/",D0
    BNE.S   .TOKEN
    CMP.B   #"*",(A0)
    BNE.S   .TOKEN
    ADDQ.L  #1,A0
    MOVEQ   #1,D1           ; COMMENT COUNT
    MOVE.L  ENDECODE,D2
.COML:  TST.L   D1
    BEQ.S   .EAT
    MOVE.B  (A0)+,D0
    CMP.B   #"*",D0
    BNE.S   .1
    CMP.B   #"/",(A0)
    BNE.S   .COML
    ADDQ.L  #1,A0
    SUBQ.L  #1,D1
    BRA.S   .COML
.1: CMP.B   #"/",D0
    BNE.S   .2
    CMP.B   #"*",(A0)
    BNE.S   .COML
    ADDQ.L  #1,A0
    ADDQ.L  #1,D1
    BRA.S   .COML
.2: CMP.B   #10,D0
    BNE.S   .COML
    CMP.L   D2,A0
    BPL.S   .ERR
    BRA.S   .COML           ;?
.SCOM:  CMP.B   #">",(A0)
    BNE.S   .TOKEN
    MOVEQ   #10,D0
.SCL:   CMP.B   (A0)+,D0
    BNE.S   .SCL
    BRA.S   .EAT
.TOKEN: SUBQ.L  #1,A0
    MOVEQ   #0,D1           ; ATTRIBUTE
    MOVE.W  (A3)+,D0        ; TOKEN
    CMP.W   #$100,D0
    BPL.S   .ASM
    CMP.W   #IOFF,D0
    BMI.S   .3
    BSR .ID
    BRA .LOOP
.3: LSL.L   #3,D0
    EXT.L   D0
    LEA .TAB(PC),A1
    ADD.L   D0,A1
    MOVE.L  4(A1),D1
    MOVE.L  (A1),A1
    JSR (A1)
    BRA .LOOP
.ASM:   CMP.W   #$400,D0
    BPL.S   .ERR            ; FOR NOW.
    BSR .ID
    CMP.B   #".",(A0)
    BNE.S   .XX
    CMP.B   #10,1(A0)
    BEQ.S   .XX
    ADDQ.L  #2,A0
.XX:    BRA .LOOP
.EXIT:  MOVE.L  A0,ERPOINT
    MOVEQ   #0,D0
    RTS
.ERR:   MOVEQ   #1,D0           ; !SHOULD BE 1 LATER!
    MOVE.L  A0,ERPOINT      ; !SHOULD BE DELETED LATER!
    RTS
.TAB:
    DC.L    .EX,0,.NUM,4,.ID,12,.ONE,",",.TWO,":="      ; 0-4
    DC.L    .ID,4,.STR,0,.ONE,"+",.ONE,"-",.ONE,"*"     ; 5-9
    DC.L    .ONE,"/",.ONE,"=",.ONE,">",.ONE,"<",.TWO,">="   ; 10-14
    DC.L    .TWO,"<=",.TWO,"<>",.ONE,"(",.ONE,")",.ONE,":"  ; 15-19
    DC.L    .ER,0,.EX,0,.ID,4,.ONE,"{",.ONE,"}"     ; 20-24
    DC.L    .ID,8,.TWO,".W",.TWO,".L",.TWO,".B",.ONE4,"["   ; 25-29
    DC.L    .ONE,"]",.ID,4,.ONE,"^",.TWO,"++",.TWO,"--" ; 30-34
    DC.L    .ONE,".",.ONE,"`",.ONE,"!",.ID,4,.ID,4      ; 35-39
    DC.L    .THREE,"<=>",.TWO,"::",.ID,4,.NUM,4,.ONE,"@"    ; 40-44
    DC.L    .ID,4,.ONE,"|",.ID,2,.ID,6,.TWO,"<<",.TWO,">>",.ER,0  ; 45-51
    DC.L    .ONE,"?",.THREE,':=:',.ID,4
.ER:    ADDQ.L  #4,A7
    BRA .ERR
.EX:    ADDQ.L  #4,A7
    BRA .EXIT
.ID:    MOVE.B  (A0)+,D2
    CMP.B   #"_",D2
    BEQ.S   .ID
    CMP.B   #"0",D2
    BMI.S   .6
    CMP.B   #"9"+1,D2
    BMI.S   .ID
    CMP.B   #"A",D2
    BMI.S   .6
    CMP.B   #"Z"+1,D2
    BMI.S   .ID
    CMP.B   #"a",D2
    BMI.S   .6
    CMP.B   #"z"+1,D2
    BMI.S   .ID
.6: SUBQ.L  #1,A0
    ADD.L   D1,A3
    RTS
.NUM:   MOVE.B  (A0),D2
    CMP.B   #"A",D2         ; SEE IF IT'S A CONSTANT
    BMI.S   .10
    CMP.B   #"Z"+1,D2
    BMI.S   .ID
.10:    CMP.B   #'"',D2
    BEQ.S   .SSTR
    MOVEQ   #0,D4           ; HEXFLAG
.8: MOVE.B  (A0)+,D2
    CMP.B   #"$",D2
    BEQ.S   .7
    CMP.B   #"%",D2
    BEQ.S   .8
    CMP.B   #".",D2         ; FOR FLOATS?
    BEQ.S   .8
    CMP.B   #"0",D2
    BMI.S   .9
    CMP.B   #"9"+1,D2
    BMI.S   .8
    TST.L   D4
    BEQ.S   .9
    CMP.B   #"A",D2
    BMI.S   .9
    CMP.B   #"G",D2
    BMI.S   .8
    CMP.B   #"a",D2
    BMI.S   .9
    CMP.B   #"z"+1,D2
    BMI.S   .8
.9: SUBQ.L  #1,A0
    ADD.L   D1,A3
    RTS
.7: MOVEQ   #1,D4
    BRA.S   .8
.SSTR:  ADDQ.L  #1,A0
.SSL:   CMP.B   #'"',(A0)+
    BNE.S   .SSL
    ADD.L   D1,A3
    RTS
.ONE:   CMP.B   (A0)+,D1
    BEQ.S   .4
    ADDQ.L  #4,A7           ; STACK BACK
    BRA .ERR
.4: RTS
.ONE4:  CMP.B   (A0)+,D1        ; AS ONE, BUT NOW SKIPS A LONG
    BEQ.S   .4B
    ADDQ.L  #4,A7           ; STACK BACK
    BRA .ERR
.4B:    ADDQ.L  #4,A3
    RTS
.TWO:   MOVE.L  D1,D2
    LSR.L   #8,D2
    CMP.B   (A0)+,D2
    BNE.S   .5
    CMP.B   (A0)+,D1
    BNE.S   .5
    RTS
.THREE: MOVE.L  D1,D2
    SWAP    D2
    CMP.B   (A0)+,D2
    BNE.S   .5
    MOVE.L  D1,D2
    LSR.L   #8,D2
    CMP.B   (A0)+,D2
    BNE.S   .5
    CMP.B   (A0)+,D1
    BNE.S   .5
    RTS
.5: ADDQ.L  #4,A7           ; STACK BACK
    BRA .ERR
.STR:   ADDQ.L  #2,A3
    MOVE.W  (A3)+,D2
    EXT.L   D2
    LSL.L   #1,D2
    ADD.L   D2,A3
    CMP.B   #"'",(A0)+
    BNE.S   .SE
.SL:    MOVE.B  (A0)+,D2
    CMP.B   #10,D2
    BEQ.S   .SE
    CMP.B   #"'",D2
    BNE.S   .SL
    CMP.B   #"'",(A0)
    BNE.S   .SEX
    ADDQ.L  #1,A0
    BRA.S   .SL
.SEX:   RTS
.SE:    ADDQ.L  #4,A7           ; STACK BACK
    BRA .ERR


WRITECON:             ; ADR D2, LEN D3
    MOVE.L  STOUT(PC),D1
    MOVE.L  DOSBASE(PC),A6
    JSR -48(A6)
    RTS

WRITELN:
    MOVEQ   #1,D3
    MOVE.L  #RETURNOUT,D2
    BSR.S   WRITECON
    RTS

WRITEFORMAT:          ; A0=STRING, A1=DATA
    MOVE.L  #PRINTBUF,A3        ; USES D0-D3/A0-A3/A6
    LEA .1(PC),A2       ; TRASH DIRNAME BUF....
    MOVE.L  A3,D2
    MOVE.L  4.W,A6
    JSR -522(A6)
    MOVE.L  D2,A0
.2: TST.B   (A0)+
    BNE.S   .2
    SUBQ.L  #1,A0
    MOVE.L  A0,D3
    SUB.L   D2,D3
    BSR WRITECON
    RTS
.1: MOVE.B  D0,(A3)+
    RTS

WRITELINENUM:
    MOVE.W  LINENUM(PC),D7
    CMP.W   LINEWRITE(PC),D7
    BPL.S   .1
    RTS
.1: BTST    #7,CODEPREFS+2
    BNE.S   .11
    MOVEM.L D0-D3/A0-A3,-(A7)
    LEA .2(PC),A0
    LEA .3(PC),A1
    MOVE.W  LINEWRITE(PC),D0
    EXT.L   D0
    MOVE.L  D0,(A1)
    BSR WRITEFORMAT
    ADD.W   #100,LINEWRITE
    MOVEM.L (A7)+,D0-D3/A0-A3
.11:    RTS
.2: DC.B    '%ld',13,0
    EVEN
.3: DC.L    0

NOERROR:
    ADDQ.W  #1,ASSLINE
    BTST    #4,CODEPREFS+3
    BNE.S   .X
    BTST    #3,CODEPREFS+3
    BEQ.S   .3
    BCLR    #0,WARNINGS+3
.3: MOVE.L  WARNINGS(PC),D7
    MOVEQ   #0,D6           ; BITCOUNT
    LEA WARNINGTAB,A2
.XL:CMP.W   #32,D6
    BEQ.S   .X
    BTST    D6,D7
    BEQ.S   .1
    MOVEQ   #9,D3
    MOVE.L  #WARNINGMESSY,D2
    BSR WRITECON
    MOVE.L  D6,D5
    LSL.L   #2,D5
    LEA     WARNINGTAB,A0
    MOVE.L  (A0,D5.L),D2
    MOVE.L  D2,A0

;.2:    TST.B   (A0)+
;   BNE.S   .2
;   SUBQ.L  #1,A0
;   MOVE.L  A0,D3
;   SUB.L   D2,D3
;   BSR WRITECON

    MOVEM.L D0-D3/A0-A3,-(A7)
    LEA ASSLINE(PC),A1
    BSR WRITEFORMAT
    MOVEM.L (A7)+,D0-D3/A0-A3

    BSR WRITELN

.1: ADDQ.L  #1,D6
    BRA.S   .XL
.X: BTST    #7,CODEPREFS+2
    BNE.S   .11
    MOVE.L  #NOERMESSAGE,D2
    MOVE.L  #NEMEND-NOERMESSAGE,D3
    BSR WRITECON
    BSR WRITELN
.11:    RTS
;*-*

