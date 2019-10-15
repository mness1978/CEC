;; Cache Handler
PORTCOOKIE  = $DEADBEEF

CACHENAME:    DC.B    "EmoduleCache",0,0
PORTADR:      DC.L    0   ; MUST BE NIL FOR NO_CACHE
CACHENAMESTART:   DC.L    0
CACHENAMEEND: DC.L    0

FLUSHMODULE:          ; D0=MODPTR
    MOVEM.L D1/D2/D3/A0-A3/A6,-(A7)
    MOVE.L  D0,D1           ; D1=MODNAME
    MOVE.L  D1,A0
.SL:    MOVE.B  (A0)+,D2        ; CHOMP OFF PATH
    CMP.B   #":",D2
    BEQ.S   .SP
    CMP.B   #"/",D2
    BEQ.S   .SP
    TST.B   D2
    BNE.S   .SL
    MOVE.L  PORTADR(PC),D0
    BEQ.S   .X
    MOVE.L  D0,A0
    LEA 38(A0),A0       ; A1=PREV, A0=CUR
.XL:MOVE.L  A0,A1
    MOVE.L  (A0),A0
    MOVE.L  A0,D0
    BEQ.S   .X
    MOVE.L  8(A0),D2
    MOVE.L  D2,A2
.SL2:   MOVE.B  (A2)+,D3        ; CHOMP OFF PATH HERE TOO
    CMP.B   #":",D3
    BEQ.S   .SP2
    CMP.B   #"/",D3
    BEQ.S   .SP2
    TST.B   D3
    BNE.S   .SL2
    MOVE.L  D2,A2
    MOVE.L  D1,A3
.CL:    CMPM.B  (A2)+,(A3)+     ; SPECIAL STRCMP: ENDS MUST MATCH
    BNE.S   .XL
    TST.B   -1(A2)
    BNE.S   .CL
    MOVE.L  (A0),(A1)       ; PREV.NEXT:=CUR.NEXT
    MOVE.L  4(A0),D0
    EXG.L   A0,A1
    MOVE.L  4.W,A6
    MOVEM.L D1/A0,-(A7)
    JSR -210(A6)
    MOVEM.L (A7)+,D1/A0
    BRA.S   .XL
.X: MOVEM.L (A7)+,D1/D2/D3/A0-A3/A6
    RTS
.SP:    MOVE.L  A0,D1
    BRA.S   .SL
.SP2:   MOVE.L  A2,D2
    BRA.S   .SL2

SEARCHINCACHE:            ; RETURNS D0=MODPTR OR NIL
    MOVEM.L D1/A0-A3,-(A7)
    MOVE.L  PORTADR(PC),D0
    BEQ.S   .X
    MOVE.L  D0,A0
    MOVE.L  38(A0),A0
    MOVE.L  CACHENAMESTART(PC),A1
    MOVE.L  CACHENAMEEND(PC),D1
    SUB.L   A1,D1
.XL:MOVE.L  A0,D0
    BEQ.S   .X
    CMP.L   12(A0),D1       ; SEE IF LENGTHS MATCH
    BNE.S   .N
    MOVE.L  8(A0),A2
    MOVE.L  A1,A3
.CL:    CMPM.B  (A2)+,(A3)+     ; STRCMP
    BNE.S   .N
    TST.B   -1(A2)
    BNE.S   .CL
    MOVE.L  16(A0),D0       ; EXIT WITH MODPTR
    BRA.S   .X
.N: MOVE.L  (A0),A0
    BRA.S   .XL
.X: MOVEM.L (A7)+,D1/A0-A3
    RTS

NEWCACHE:
    TST.L   PORTADR         ; DO WE CACHE?
    BEQ NEW
    MOVEM.L D1-D3/A0-A3/A6,-(A7)    ; SIZE-->D0-->ADDR
    MOVE.L  CACHENAMESTART(PC),A2
    MOVE.L  CACHENAMEEND(PC),D3
    SUB.L   A2,D3
    MOVE.L  D0,A3           ; TEMP
    ADD.L   #50,D0
    ADD.L   D3,D0
    MOVE.L  D0,D2
    MOVEQ   #0,D1
    MOVE.L  4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .EXX
    MOVE.L  A3,D1
    MOVE.L  D0,A0
    MOVE.L  PORTADR(PC),A3      ; LINK INTO LIST
    MOVE.L  38(A3),(A0)
    MOVE.L  A0,38(A3)
    MOVE.L  D2,4(A0)        ; SET MEM-LEN
    MOVE.L  D3,12(A0)
    LEA 32(A0),A3
    MOVE.L  A3,8(A0)        ; SET NAME
.XL:MOVE.B  (A2)+,D2
    CMP.B   #"A",D2         ; LOWERCASE'M
    BMI.S   .C
    CMP.B   #"Z"+1,D2
    BPL.S   .C
    ADD.B   #32,D2
.C: MOVE.B  D2,(A3)+
    BNE.S   .XL
    MOVE.L  A3,D3
    BTST    #0,D3
    BEQ.S   .S
    ADDQ.L  #1,A3
.S: MOVE.L  A3,16(A0)
    MOVE.L  D1,20(A0)
    MOVE.L  A3,D0
.EXX:   MOVEM.L (A7)+,D1-D3/A0-A3/A6
    RTS

GETCACHE:
    BTST    #1,CODEPREFS+1      ; IF OPT=-c THEN DON'T USE CACHE
    BEQ.S   .1
    RTS
.1: MOVE.L  4.W,A6
    JSR -$84(A6)        ; FORBID
    LEA CACHENAME(PC),A1
    JSR -390(A6)        ; FINDPORT
    TST.L   D0
    BNE.S   .FOUND
    MOVEQ   #1,D1           ; get ourselves mem for port
    SWAP    D1
    MOVEQ   #100,D0
    MOVE.L  4.W,A6
    JSR -198(A6)
    TST.L   D0
    BEQ.S   .FAIL
    MOVE.L  D0,A5           ; A5=MESSAGEPORT
    LEA CACHENAME(PC),A0
    LEA 50(A5),A1
    MOVE.L  A1,D0
.XL:MOVE.B  (A0)+,(A1)+
    BNE.S   .XL
    MOVE.L  D0,10(A5)
    MOVE.B  #4,8(A5)        ; this is a msgport
    MOVE.B  #1,9(A5)        ; pri = 1
    MOVE.B  #2,14(A5)       ; pa_ignore
    MOVE.L  #PORTCOOKIE,34(A5)
    MOVE.L  A5,A1
    JSR -354(A6)
    BRA.S   .PORT
.FOUND: MOVE.L  D0,A5
.PORT:  MOVE.L  #DEBUGDATA,42(A5)
    TST.W   46(A5)
    BNE.S   .FAIL
    CMP.L   #PORTCOOKIE,34(A5)  ; HERE WE HAVE VALID PORT IN A5
    BNE.S   .FAIL
    MOVE.L  A5,PORTADR
    MOVE.W  #1,46(A5)
.FAIL:  JSR -$8A(A6)        ; PERMIT
    RTS

UNLOCKCACHE:
    MOVE.L  4.W,A6
    JSR -$84(A6)        ; FORBID
    MOVE.L  PORTADR(PC),D0
    BEQ.S   .1
    MOVE.L  D0,A0
    CLR.W   46(A0)
.1: JSR -$8A(A6)        ; PERMIT
    RTS

;     RESCHECK:             ; PRESERVES ALL REGS IF OK
;   TST.W   .1
;   BNE.S   .X
;   MOVE.W  #-1,.1
;   RTS
;.1:    DC.W    0
;.X:    MOVE.L  INITSTACK,A7        ; EMERGENCY EXIT
;   MOVEQ   #20,D0
;   RTS
;*-*

