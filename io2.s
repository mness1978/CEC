;; Write file
;; WriteExe
WRITEEXE:
    MOVE.W  #8,CURSPOT
    TST.L   ARGLEN
    BNE.S   .1
    RTS
.1: TSTMOD
    BNE WRITEMODULE
    MOVE.L  A7,.STACK
    MOVE.L  #NAMEBUF,D1
    MOVE.L  LIBINFO(PC),D0
    BEQ.S   .NL
    MOVE.L  D0,A0
    MOVE.L  (A0),D1
.NL:    MOVE.L  #1006,D2
    MOVE.L  DOSBASE(PC),A6
    JSR -30(A6)
    MOVE.L  D0,HANDLE
    BEQ iERROR17


    MOVE.L  NumHunks,-(A7)
    SUBQ.L  #1,(A7)
    PEA $0.W
    MOVE.L  NumHunks,-(A7)
    PEA $0.W
    PEA $3f3.W
    MOVE.L  A7,D2
    MOVE.L  #20,D3
    BSR WRITEFILE
    LEA 20(A7),A7
    bra .DL
.OF:pea $3e7.W
    pea $0.W
    move.l  a7,d2
    moveq   #8,d3
    bsr     WRITEFILE
    addq.l  #8,a7

.DL:MOVE.L  HunkList,D0
    MOVEQ   #0,D3
.SL:MOVE.L  D0,A0
    ADDQ.L  #1,D3
    MOVE.L  H_END(A0),D0
    SUB.L   H_ADDR(A0),D0
    MOVE.L  D0,D2
    and.l   #3,d2
    tst.l   d2
    sne     d2
    and.l   #1,d2
    LSR.L   #2,D0
    add.l   d2,d0
    move.l  H_TYPE(A0),D2
    and.l   #$E0000000,D2
    or.l    d2,d0
    MOVE.L  D0,-(A7)
    MOVE.L  H_NEXT(A0),D0
    BNE     .SL

    MOVE.L  A0,A5

    LSL.L   #2,D3
    MOVE.L  A7,D2
    BSR     WRITEFILE

    MOVE.L  A5,D0
.SW:MOVE.L  D0,A5
    MOVE.L  D0,D2
    ADD.L   #H_TYPE,D2
    MOVEQ   #4,D3
    BSR WRITEFILE
    MOVE.L  H_END(A5),D0
    SUB.L   H_ADDR(A5),D0
    MOVE.W  D0,D2
    AND.L   #3,D2
    TST.L   D2
    Sne     D2
    AND.L   #1,D2
    LSR.L   #2,D0
    ADD.L   D2,D0
    MOVE.L  D0,-(A7)
    MOVE.L  A7,D2
    MOVEQ   #4,D3
    BSR     WRITEFILE
    MOVE.L  (A7)+,D3
    LSL.L   #2,D3
    MOVE.L  H_ADDR(A5),D2
    BSR WRITEFILE
    BSR .DORELOCS
    PEA $3F2.W
    MOVE.L  A7,D2
    MOVEQ   #4,D3
    BSR WRITEFILE
    ADDQ.L  #4,A7
    MOVE.L  H_PREV(A5),D0
    BNE     .SW
    MOVE.L  HANDLE(PC),D1
    MOVE.L  #-4,D2
    MOVE.L  #0,D3
    MOVE.L  DOSBASE(PC),A6
    JSR     -66(A6)
    BRA     .DALEJ

.DORELOCS:              ; A5=hunk entry! :)
    TST.L   H_RELO(A5)
    BNe     .SKIP
    RTS
.SKIP:
    BTST    #5,ICODEPREFS+3
    BNE     .OPTIRELOC
.OPTIBACK
    PEA     $3EC.W
    MOVE.L  A7,D2
    MOVEQ   #4,D3
    BSR     WRITEFILE
    ADDQ.L  #4,A7
    MOVE.L  H_RELO(A5),D0
    MOVEQ   #0,D4           ; current dest reloc hunk
    moveq   #0,d5           ; num relocs
.LOOP:
    MOVE.L  D0,A4
    cmp.w   (a4),d4
    bne     .NEXT
    addq.l  #1,d5
    move.l  2(a4),-(a7)
.NEXT:
    MOVE.L  R_NEXT(A4),D0
    BNE     .LOOP
    TST.L   d5
    beq     .NXT2
    move.l  d4,-(a7)
    move.l  d5,-(a7)
    move.l  d5,d3
    addq.l  #2,d3
    lsl.l   #2,d3
    move.l  d3,d5
    move.l  a7,d2
    bsr     WRITEFILE
    add.l   d5,a7
.NXT2:
    moveq   #0,d5
    addq.l  #1,d4
    MOVE.L  H_RELO(A5),D0
    cmp.l   NumHunks,d4
    ble     .LOOP

    PEA     $0.W
    MOVE.L  A7,D2
    MOVEQ   #4,D3
    BSR     WRITEFILE
    ADDQ.L  #4,A7
    RTS




.OPTIRELOC:
    MOVE.L  A7,D6
    MOVE.L  H_RELO(A5),D0
    MOVEQ   #0,D4           ; current dest reloc hunk
    moveq   #0,d5           ; num relocs
    moveq   #0,d7
.LOOP2:
    MOVE.L  D0,A4
    cmp.w   (a4),d4
    bne     .NEXT2
    MOVE.L  2(A4),D3
    SWAP    D3
    TST.W   D3
    BNE     .NEXT2
    SWAP    D3
    addq.l  #1,d5
    move.w  D3,-(a7)
    moveq   #-1,d7
.NEXT2:
    MOVE.L  R_NEXT(A4),D0
    BNE     .LOOP2
    TST.L   d5
    beq     .NXT3
    move.w  d4,-(a7)
    move.w  d5,-(a7)
    pea $3f7.w
    move.l  a7,d2
    move.l  d6,d3
    sub.l   a7,d3
    bsr     WRITEFILE
    move.l  d6,a7
.NXT3:
    moveq   #0,d5
    addq.l  #1,d4
    MOVE.L  H_RELO(A5),D0
    cmp.l   NumHunks,d4
    ble     .LOOP2

    tst.l   d7
    beq     .OPTI2
    PEA     $0.W
    MOVE.L  A7,D2
    MOVEQ   #2,D3
    BSR     WRITEFILE

    MOVE.L  HANDLE,D1
    CLR.L   D2
    CLR.L   D3
    JSR     -66(A6) ;seek
    BTST    #1,D0
    BEQ     .EOPTI
    MOVE.L  A7,D2
    MOVEQ   #2,D3
    BSR WRITEFILE
.EOPTI:
    ADDQ.L  #4,A7
.OPTI2:
; here the code continues, this time for large relocs

    MOVE.L  A7,D6
    MOVE.L  H_RELO(A5),D0
    MOVEQ   #0,D4           ; current dest reloc hunk
    moveq   #0,d5           ; num relocs
    moveq   #0,d7
.LOOP3:
    MOVE.L  D0,A4
    cmp.w   (a4),d4
    bne     .NEXT3
    MOVE.L  2(A4),D3
    SWAP    D3
    TST.W   D3
    BEQ     .NEXT3
    SWAP    D3
    addq.l  #1,d5
    move.l  D3,-(a7)
    moveq   #-1,d7
.NEXT3:
    MOVE.L  R_NEXT(A4),D0
    BNE     .LOOP3
    TST.L   d5
    beq     .NXT4
    move.l  d4,-(a7)
    move.l  d5,-(a7)
    pea     $3ec.w
    move.l  a7,d2
    move.l  d6,d3
    sub.l   a7,d3
    bsr     WRITEFILE
    move.l  d6,a7
.NXT4:
    moveq   #0,d5
    addq.l  #1,d4
    MOVE.L  H_RELO(A5),D0
    cmp.l   NumHunks,d4
    ble     .LOOP3

    tst.l   d7
    beq     .EXIT
    PEA     $0.W
    MOVE.L  A7,D2
    MOVEQ   #4,D3
    BSR     WRITEFILE
    ADDQ.L  #4,A7
.EXIT:
    RTS

.STACK:
    DC.L    0


.DALEJ:
    move.l  SYMADDR(PC),D2
    beq     .dalej2
    move.l  SYMEND(PC),D3
    SUB.l   d2,d3
    bsr WRITEFILE
.dalej2:
    MOVE.L  LINEBUF(PC),D2
    BEQ.S   .2
    MOVE.L  LINEBUFCUR(PC),D3
    CMP.L   LINEBUFCURSTART(PC),D3  ; ONLY IF LINEDEBUGSIZE<>0
    BEQ.S   .2
    SUB.L   D2,D3
    MOVE.L  D3,D4
    SUBQ.L  #8,D4
    LSR.L   #2,D4
    MOVE.L  D2,A0
    MOVE.L  D4,4(A0)
    BSR WRITEFILE

    BSR WRITEEVARONLY
    LEA DEBUGLIST(PC),A5
.XL:MOVE.L  (A5),D0
    BEQ.S   .2
    MOVE.L  D0,A5
    MOVE.L  4(A5),D2
    MOVE.L  D2,A0
    MOVE.L  4(A0),D3
    MOVE.L  8(A5),D0
    OR.L    #$4C000000,D0
    MOVE.L  D0,12(A0)
    LSL.L   #2,D3
    ADDQ.L  #8,D3
    CMP.L   #$3F1,0(A0,D3.L)
    BNE.S   .NEVAR
    MOVE.L  4(A0,D3.L),D0       ; ADD EVAR HUNK
    ADDQ.L  #2,D0
    LSL.L   #2,D0
    ADD.L   D0,D3
.NEVAR: BSR WRITEFILE
    BRA.S   .XL
.2:
    MOVE.L  #ENDHUNK,D2
    MOVEQ   #4,D3
    BSR WRITEFILE
    MOVE.L  DOSBASE(PC),A6
    MOVE.L  HANDLE(PC),D1
    JSR -36(A6)
    MOVE.L  .STACK(PC),A7
    RTS
SYMADDR:
    DC.L    0
SYMEND:
    DC.L    0
;*-*
;; WriteModule
WRITEMODULE:
    MOVE.L  #NAMEBUF,A2     ; FIX '.M'
    ADD.L   ARGLEN(PC),A2       ; A2=MIDARG
    MOVE.B  #'.',(A2)
    MOVE.B  #'m',1(A2)
    MOVE.B  #0,2(A2)

    MOVE.L  #NAMEBUF,D1     ; OPEN MODULE FOR WRITING
    MOVE.L  #1006,D2
    MOVE.L  DOSBASE(PC),A6
    JSR -30(A6)
    MOVE.L  D0,HANDLE
    BEQ iERROR17

    MOVE.L  #.1,D2          ; WRITE "EMOD" HEADER
    MOVEQ   #4,D3
    BSR WRITEFILE

    BSR WRITESYSINFOS
    BSR WRITECODEINFOS
    BSR WRITERELOCINFOS
    BSR WRITEPROCINFOS
    BSR WRITEGLOBINFOS
    BSR WRITEOBJECTINFOS
    BSR WRITECONSTINFOS
    BSR WRITEMODINFOS
    BSR WRITEDEBUGINFOS
    JSR PREPSAVEMACROS

    MOVE.L  #.2,D2          ; WRITE END
    MOVEQ   #2,D3
    BSR WRITEFILE

    MOVE.L  DOSBASE(PC),A6
    MOVE.L  HANDLE(PC),D1
    JSR -36(A6)

    MOVE.L  #NAMEBUF,D0     ; FLUSH THE MODULE
    MOVE.L  D0,A0
    LOWER   A0,D1

    BSR FLUSHMODULE

    RTS
.1: DC.L    "EMOD"          ; HEADER
.2: DC.W    0           ; END


WRITEFILE:            ; D2=ADR,D3=LEN
    MOVE.L  HANDLE(PC),D1
    MOVE.L  DOSBASE(PC),A6
    JSR -48(A6)
    RTS

CODEAMOUNT:   DC.L    0

HUNK:
    DC.L    1011            ; hunkheader
    DC.L    0,1,0,0         ; endofnames,1hunk,firsthunkno,lasthn
CODESIZE:
    DC.L    0           ; CODESIZE/4 hunklentab
    DC.L    1001            ; hunkcode
CODESIZE2:
    DC.L    0           ; CODESIZE/4
ENDHUNK:
    DC.L    1010            ; last
LHUNK:
    DC.L    $3e7
    DC.L    0,$3e9
CODESIZE3:
    DC.L    0

;; Evars
WRITEEVARONLY:
    MOVE.L  #ESTACKBUF,A4       ; A4=BUF,D4=BEGIN
    MOVE.L  A4,D4
    MOVE.L  #$3F1,(A4)+     ; HUNK_DEBUG
    CLR.L   (A4)+           ; HUNKLEN
    CLR.L   (A4)+
    MOVE.L  #"EVAR",(A4)+       ; "EVAR"
    LEA DBLIST(PC),A5       ; A5=LIST
    TST.L   (A5)
    BEQ.W   .OUT
.XL:MOVE.L  (A5),D0
    BEQ.S   .X
    MOVE.L  D0,A5
    MOVEQ   #0,D3           ; D3=WLEN
    CMP.W   #4,4(A5)
    BNE.S   .1
    MOVE.W  6(A5),D3
    LSR.W   #1,D3
    ADDQ.L  #1,D3
    BRA.S   .2
.1: MOVE.W  8(A5),D3
    ADDQ.L  #2,D3
.2: LEA 4(A5),A0
.CL:    MOVE.W  (A0)+,(A4)+
    DBRA    D3,.CL
    CHESTB  A4,D0,3,iERROR37

    CMP.W   #4,4(A5)
    BNE.S   .NSELF
    CMP.W   #5,(A0)
    BNE.S   .NSELF
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
.NSELF:
    BRA.S   .XL
.X: MOVE.L  A4,D3
    SUB.L   D4,D3
    BTST    #1,D3
    BEQ.S   .LE
    CLR.W   (A4)+
    ADDQ.L  #2,D3
.LE:    MOVE.L  D4,A0
    MOVE.L  D3,D0
    SUBQ.L  #8,D0
    LSR.L   #2,D0
    MOVE.L  D0,4(A0)
    MOVE.L  D4,D2
    BSR WRITEFILE
.OUT:   RTS
;*-*
;; Debug infos
WRITEDEBUGINFOS:
    TST.L   LINEBUF
    BEQ.S   .2
    MOVE.L  LINEBUFCUR(PC),D3   ; TEST EMPTYNESS
    CMP.L   LINEBUFCURSTART(PC),D3
    BEQ.S   .2
    MOVE.L  #.4,D2          ; WRITE DEBUGHEAD
    MOVE.L  #2,D3
    BSR WRITEFILE
    MOVE.L  LINEBUF(PC),D2
    MOVE.L  LINEBUFCUR(PC),D3   ; WRITE LINE-DEBUG HUNK
    SUB.L   D2,D3
    MOVE.L  D3,D4
    SUBQ.L  #8,D4
    LSR.L   #2,D4
    MOVE.L  D2,A0
    MOVE.L  D4,4(A0)        ; LEN
    BSR WRITEFILE
    BSR WRITEEVARONLY
    MOVE.L  #.3,D2          ; WRITE LEN NO FURTHER INFOS
    MOVE.L  #4,D3
    BSR WRITEFILE
.2: RTS
.3: DC.L    0
.4: DC.W    10
;*-*
;; Code infos
WRITECODEINFOS:
    MOVE.L  CODEAMOUNT(PC),D4
    BEQ.S   .X
    LSR.L   #2,D4
    MOVE.L  D4,.1+2
    MOVE.L  #.1,D2
    MOVEQ   #6,D3
    BSR WRITEFILE
    MOVE.L  ACODE(PC),D2        ; WRITE CODE
    MOVE.L  CODEAMOUNT(PC),D3
    BSR WRITEFILE
.X: RTS
.1: DC.W    3
    DC.L    0
;*-*
;; Reloc infos
RELOCTABSTART:    DC.L    0

WRITERELOCINFOS:
    MOVE.L  NUMRELOC(PC),D4
    BEQ.S   .X
    MOVEQ   #2,D3
    MOVE.L  #.1,D2
    BSR WRITEFILE
    LSL.L   #2,D4
    ADDQ.L  #4,D4           ; WRITE NUM ALSO
    MOVE.L  RELOCTABSTART(PC),A0
    MOVE.L  -4(A0),(A0)
    MOVE.L  A0,D2
    MOVE.L  D4,D3
    BSR WRITEFILE
.X: RTS
.1: DC.W    7
;*-*
;; Object infos
WRITEOBJECTINFOS:     ; for objects & members -SAME-
    MOVE.L  #ESTACKBUF,D5   ; EStackBuffer
    MOVE.L  D5,A5           ; D5,A5=BUF
    LEA OLIST+4,A3          ; A3=ObjectLIST
    MOVEQ   #0,D7           ; D7=OBJECT COUNT
.XL:MOVE.L  ONEXT(A3),A3    ; Next object
    MOVE.L  A3,D0           ; End of list?
    BEQ.W   .E              ; .E(nd)
    TST.L   OACC(A3)        ; Access -> TEMP: HAVE TO EXPORT
    BNE.S   .1              ; we have to.
    BTST    #0,OFLAGS(A3)   ; #0 -> OPT EXPORT?
    BEQ.S   .XL             ; No; skip the object, get another one.
.1: BTST    #1,OFLAGS(A3)   ;
    BNE.S   .XL             ;
    ADDQ.L  #1,D7           ; Ok, let's parse this object.
    MOVE.W  #2,(A5)+        ; #2 = OBJECT JOB
    CLR.L   (A5)+           ; SKIPSIZE
    MOVE.W  OSIZE(A3),D1    ; D1=OHEADSIZE
    MOVE.W  #-1,OSIZE(A3)   ;
    MOVE.L  A3,A0           ;
    BSR .DOREC              ;
    LEA OMEMB+4(A3),A4      ; list of members in A4
.ML:MOVE.L  ONEXT(A4),A4    ; next member; ONEXT=-4!!!! Look at previous command
    MOVE.L  A4,D0           ; Reached last one?
    BEQ.W   .EOO            ; Yes.
    MOVE.L  A4,A0           ;
    BTST    #0,OFLAGS(A0)   ; EXPORT?
    BNE.S   .ML             ; No.
    BSR .DOREC              ; Yes -> copy member name
    BTST    #1,OFLAGS(A0)   ; Object typed?
    BEQ.S   .NT             ; No.
    MOVE.L  OPTRTYPE(A0),D0 ; pointer to objectname
    MOVE.L  D0,D4           ;
    AND.L   #$FFFFFFF0,D4   ; ptr to object or LONG/INT/CHAR?
    BEQ.S   .NO             ; no objects->jump
    MOVE.L  D0,A2           ;
    MOVEQ   #-1,D0          ;
.NO:
    MOVE.W  D0,(A5)+        ;
    TST.L   D0              ; temporarily set name to -1
    BPL.S   .NTS            ;
    CLR.W   (A5)+           ;
    MOVE.L  A5,D4           ;
    MOVE.L  OASCII(A2),A2   ; object name (ptr to objectname)
.CL:MOVE.B  (A2)+,(A5)+
    BNE.S   .CL
    MOVE.L  A5,D0           ;
    BTST    #0,D0           ; even length
    BEQ.S   .EE             ;
    CLR.B   (A5)+           ;
.EE:MOVE.L  A5,D0
    SUB.L   D4,D0           ;
    MOVE.L  D4,A2           ;
    MOVE.W  D0,-(A2)        ; Set ptrname length
    BRA.S   .NTS            ;
.NT:CLR.W   (A5)+           ; object not typed and no name copied
.NTS:
    CHESTB  A5,D0,5,iERROR37 ;
    BRA.W   .ML             ;
.E: MOVE.L  D5,D2           ;
    MOVE.L  A5,D3           ;
    SUB.L   D2,D3           ;
    MOVE.L  D5,A5           ;
    MOVE.L  D3,D5           ;
    SUBQ.L  #6,D5           ;
    MOVE.L  D5,2(A5)        ; ptr to <another>
    TST.L   D7              ;
    BEQ.S   .LEAVE          ;
    BSR WRITEFILE           ;
.LEAVE: RTS                 ;


.DOREC: MOVE.W  #-1,(A5)+       ; len=-1 (no id)
    MOVE.W  OSIZE(A0),(A5)+     ; copy object size
    MOVE.W  OOFF(A0),(A5)+      ; copy delegates offset
    MOVE.L  A5,A1               ;
    MOVE.L  OASCII(A0),A2       ; object ascii name
    MOVE.L  A2,D0               ; or <NIL>
    BEQ.S   .PRIV               ; if private
.DL:
    MOVE.B  (A2)+,(A5)+         ; copy object name
    BNE.S   .DL                 ;
    MOVE.L  A5,D0               ;
    BTST    #0,D0               ; must be word aligned
    BEQ.S   .EQ                 ;
    CLR.B   (A5)+               ;
.EQ:MOVE.L  A5,D0               ; id len
    SUB.L   A1,D0               ;
    MOVE.W  D0,-6(A1)           ; it's the thing we've set to "-1" at start of this part
.PRIV:  RTS                     ;


; FOLLOWING CODE IS USED ONLY WHEN OBJECT HAS A METHODS!


.EOO:   CLR.W   (A5)+           ; "END OF OBJECTS"
    MOVE.W  D1,(A5)+            ; ???

    ; LEAVE A3,A5, D5,D7

    MOVE.W  ODEL(A3),(A5)+      ; DELSIZE:INT
    BEQ.W   .ECL
    MOVE.L  ODCODE(A3),D0
    SUB.L   ACODE,D0
    MOVE.L  D0,(A5)+            ; DELCODE:OFF
    CLR.W   (A5)+               ; SUPERASCLEN:INT
    MOVE.W  ODELOFF(A3),(A5)+   ; ODELOFF:INT
    MOVE.W  ODESTR(A3),(A5)+    ; ODESTR:INT
    LEA OMETHOD(A3),A0
.MEL:   MOVE.L  (A0),D0
    BEQ.S   .MEX
    MOVE.L  D0,A0               ; (A0=METHOD)
    MOVE.L  M_TYPE(A0),(A5)+    ; TYP:CHAR,FL:CHAR,OFF:INT
    ADDQ.L  #2,A5               ; ASCLEN:INT
    MOVE.L  A5,A1               ; (A1=PATCHBACK)
    MOVE.L  M_NAME(A0),A2
.MLL:   MOVE.B  (A2)+,(A5)+     ; ASCBYTES
    BNE.S   .MLL
    MOVE.L  A5,D0
    ADDQ.L  #1,D0
    BCLR    #0,D0
    MOVE.L  D0,A5
    SUB.L   A1,D0
    MOVE.W  D0,-2(A1)
    MOVE.L  M_PROC(A0),A1       ; (A1=PROC)
    MOVE.W  (A1),(A5)+          ; NARGS:INT


    MOVE.L  6(A1),D0            ; LISTLEN:INT, LIST:LONGS
    BEQ.S   .DZERO
    MOVE.L  D0,A2
    MOVE.W  (A2)+,D0
    MOVE.W  D0,(A5)+
    BEQ.S   .DDONE
    SUBQ.W  #1,D0
.DEFAL: MOVE.L  (A2)+,(A5)+
    DBRA    D0,.DEFAL
    BRA.S   .DDONE
.DZERO: CLR.W   (A5)+
.DDONE:
    BRA.S   .MEL
.MEX:   MOVE.W  #-1,(A5)+       ; END -> TYP+FL = -1

    LEA OACC(A3),A0
.AL:MOVE.L  (A0),D0         ; LISTOF CODE:OFF, TYP:INT
    BEQ.S   .AX
    MOVE.L  D0,A0
    MOVE.W  8(A0),(A5)+
    MOVE.L  4(A0),D0
    SUB.L   ACODE(PC),D0
    MOVE.L  D0,(A5)+
    BRA.S   .AL
.AX:    MOVE.W  #-1,(A5)+       ; END -> -1:INT

.ECL:
    BRA .XL
;*-*
;; Sys infos
WRITESYSINFOS:
    MOVE.L  #ESTACKBUF,D2
    MOVE.L  D2,A5           ; D5,A5=BUF
    MOVE.W  #5,(A5)+
    MOVE.L  MINSTACK(PC),(A5)+
    MOVE.W  OSVERSION(PC),(A5)+
    MOVE.L  CODEPREFS(PC),(A5)+
    MOVE.L  ECPU(PC),(A5)+      ; CPU+FPU
    MOVE.W  ASMCPU(PC),(A5)+
    MOVE.W  #CURVERSION,(A5)+
    CLR.L   (A5)+
    MOVE.L  ICODEPREFS(PC),(A5)+
    MOVEQ   #28,D3          ; HEAD+22
    BSR WRITEFILE
    RTS
;*-*
;; Const infos
WRITECONSTINFOS:
    MOVE.L  #ESTACKBUF,D5
    MOVE.L  D5,A5           ; D5,A5=BUF
    MOVE.W  #1,(A5)+
    CLR.L   (A5)+           ; SKIPSIZE
    MOVE.L  #CONSTHASH,A3
    MOVEQ   #0,D7           ; #OF CONST WRITTEN
    
    MOVE.L  ConstsList,D3
.a: TST.L   D3
    BEQ     .X

    MOVE.L  D3,A3
    MOVe.L  (A3),D3
    MOVE.L  4(A3),A3
    BTST    #0,CFLAGS(A3)
    BEQ     .a
    BTST    #1,CFLAGS(A3)
    bne     .STR
    ADDQ.L  #1,D7
    MOVe.L  A5,A4
    ADDQ.L  #2,A5
    MOVE.L  CVAL(A3),(A5)+
    MOVe.L  CASCII(A3),A2
.d: MOVE.B  (A2)+,(A5)+
    BNE.S   .d
    MOVE.L  A5,D4
    BTST    #0,D4
    BEQ     .c
    CLR.B   (A5)+
.c: MOVe.L  A5,D0
    SUB.L   A4,D0
    SUBQ.L  #6,D0
    MOVE.W  D0,(A4)
.lx:
    CHESTB  A5,D0,5,iERROR37
    BRA     .a

.STR:
    move.b  #-1,(a5)+
    clr.b   (a5)+
    ADDQ.L  #1,D7
    MOVe.L  A5,A4
    ADDQ.L  #2,A5
    moveq   #0,d4
    move.l  CASCII(a3),a2
.CP1:
    addq.l  #1,d4
    move.b  (a2)+,(a5)+
    bne     .CP1
    btst    #0,d4
    beq     .NF1
    clr.b   (a5)+
    addq.l  #1,d4
.NF1:
    move.w  d4,(a4)
    move.l  CVAL(a3),a2
    move.w  (a2)+,d4
    move.w  d4,(a5)+
.CP2:
    move.b  (a2)+,(a5)+
    dbf     d4,.CP2
    move.l  a5,d4
    btst    #0,d4
    beq     .NF2
    clr.b   (a5)+
.NF2:
    bra     .lx


.X: CLR.W   (A5)+
    MOVE.L  D5,D2
    MOVE.L  A5,D3
    SUB.L   D2,D3
    MOVE.L  D5,A5
    MOVE.L  D3,D5
    SUBQ.L  #6,D5
    MOVE.L  D5,2(A5)
    TST.L   D7
    BEQ.S   .LEAVE
    BSR WRITEFILE
.LEAVE: RTS
;*-*
;; Proc infos
WRITEPROCINFOS:
    MOVEQ   #0,D1           ; D1=PROC COUNT
    MOVE.L  #ESTACKBUF,D5
    MOVE.L  D5,A5           ; D5,A5=BUF
    MOVE.W  #4,(A5)+
    MOVE.L  LABM+8,A0       ; A0=LABELBUF
    MOVE.L  ACODE,D3        ; D3=ACODE
    MOVE.L  #IDENTHASH+4,D6
    MOVE.L  #IDENTHASH+1028,D7  ; D6,D7=IDENTBUF,END
.OLOOP: MOVE.L  D6,A4
    ADDQ.L  #4,D6
    CMP.L   D6,D7
    BEQ .EX
.LOOP:  MOVE.L  -(A4),A4
    MOVE.L  A4,D4
    BEQ.S   .OLOOP
    CMP.B   #LAB,4(A4)
    BNE.S   .LOOP
    BTST    #2,5(A4)        ; MUST BE EXPORT
    BEQ.S   .LOOP
    BTST    #4,5(A4)        ; MUSN'T BE METHOD
    BNE.S   .LOOP
    ADDQ.L  #1,D1
    MOVE.L  (A4),A3         ; ident record
    MOVE.L  A5,A2
    ADDQ.L  #2,A5
.COPY:  MOVE.B  (A3)+,(A5)+ ; COPY IDENTIFIER
    BNE.S   .COPY
    MOVE.L  A5,D0
    SUB.L   A2,D0
    SUBQ.L  #2,D0
    BTST    #0,D0
    BEQ.S   .1
    CLR.B   (A5)+
    ADDQ.L  #1,D0
.1: MOVE.W  D0,(A2)         ; set lenght

    CHESTB  A5,D0,5,iERROR37

    MOVEQ   #0,D0
    MOVE.W  10(A4),D0       ; LABEL-ID
    LSL.L   #2,D0
    MOVE.L  0(A0,D0.L),D0
    SUB.L   D3,D0
    MOVE.L  D0,(A5)+

    MOVE.L  6(A4),D0
    BEQ     .LAB
    MOVE.W  #1,(A5)+        ; THIS IS A PROC
    MOVE.L  D0,A3
    MOVE.W  (A3),(A5)+      ; NRARG
    MOVE.W  4(A3),(A5)+     ; NRLOC (NEG)*4

    BTST    #5,2(A3)
    BEQ     .NOREGS
    MOVE.W  #-1,(A5)+
    moveq   #0,d0
    MOVE.W  (A3),D0
    MOVE.W  D0,(A5)+
    MOVE.L   6(a3),a2
    addq.l  #2,a2
    subq.l  #1,d0
.LOCAL1:
    MOVE.W  (a2)+,(a5)+
    dbf d0,.LOCAL1
    move.l  #0,(a5)+
    BRA .LOOP
.NOREGS:

    BTST    #6,2(A3)
    BNE     .SETUPCLEANUP
    BTST    #7,2(A3)
    BEQ     .OTHER_STUFF
.SETUPCLEANUP:
    MOVE.W  #-2,(A5)+
    MOVE.B  2(A3),D0
    and.l   #192,d0
    MOVE.W  D0,(A5)+
    CLR.L   (A5)+
    BRA .LOOP


.OTHER_STUFF:
    MOVE.L  6(A3),D0
    BEQ.S   .DZERO
    MOVE.L  D0,A3
    MOVE.W  (A3)+,D0
    MOVE.W  D0,(A5)+
    BEQ.S   .NDEFA
    SUBQ.W  #1,D0
.DEFAL: MOVE.L  (A3)+,(A5)+
    DBRA    D0,.DEFAL
.NDEFA: MOVE.W  (A3)+,D0
    MOVE.W  D0,(A5)+
    BEQ.W   .LOOP
    SUBQ.W  #1,D0
    MOVE.L  A5,A1
.NL:    MOVE.L  (A3)+,A2
.NL2:   MOVE.B  (A2)+,(A5)+
    BNE.S   .NL2
    MOVE.B  #",",-1(A5)
    DBRA    D0,.NL
    CLR.B   -1(A5)
    MOVE.L  A5,D0
    BTST    #0,D0
    BEQ.S   .11
    CLR.B   (A5)+
.11:    MOVE.L  A5,D0
    SUB.L   A1,D0
    MOVE.W  D0,-2(A1)
    BRA .LOOP
.LAB:   MOVE.W  #2,(A5)+        ; THIS ONE IS A LABEL
    BRA .LOOP
.DZERO: CLR.L   (A5)+
    BRA .LOOP
.EX:    MOVE.W  #-1,(A5)+
    MOVE.L  D5,D2
    MOVE.L  A5,D3
    SUB.L   D2,D3
    TST.L   D1
    BEQ.S   .LEAVE
    BSR WRITEFILE
.LEAVE: RTS
;*-*
;; Globals infos
WRITEGLOBINFOS:
    MOVEQ   #0,D1           ; D1=GLOB COUNT
    MOVE.L  #ESTACKBUF,D5
    MOVE.L  D5,A5           ; D5,A5=BUF
    MOVE.W  #8,(A5)+
    MOVE.W  #SKIPMARK,(A5)+
    CLR.L   (A5)+
    MOVE.L  ACODE,D3        ; D3=ACODE
    MOVE.L  #IDENTHASH+4,D6
    MOVE.L  #IDENTHASH+1028,D7  ; D6,D7=IDENTBUF,END
.OLOOP: MOVE.L  D6,A4
    ADDQ.L  #4,D6
    CMP.L   D6,D7
    BEQ .EX
.LOOP:  MOVE.L  -(A4),A4
    MOVE.L  A4,D4
    BEQ.S   .OLOOP
    CMP.B   #GLOBV,4(A4)
    BNE.S   .LOOP
    MOVE.L  6(A4),D0
    BEQ.S   .LOOP
    MOVE.L  A5,A1           ; A1=BACKUP A5
    MOVE.L  D0,A0           ; A0=GLOBINFO
    ADDQ.L  #1,D1
    BTST    #2,5(A4)
    BEQ.S   .NNAME
    MOVE.L  (A0),A3         ; ASCIIPTR
    MOVE.L  A5,A2
    ADDQ.L  #2,A5
.COPY:  MOVE.B  (A3)+,(A5)+     ; COPY IDENTIFIER
    BNE.S   .COPY
    MOVE.L  A5,D0
    SUB.L   A2,D0
    SUBQ.L  #2,D0
    BTST    #0,D0
    BEQ.S   .1
    CLR.B   (A5)+
    ADDQ.L  #1,D0
.1: MOVE.W  D0,(A2)         ; set lenght
    BRA.S   .2
.NNAME: CLR.W   (A5)+
.2: CHESTB  A5,D0,5,iERROR37
    MOVE.L  4(A0),A0
    MOVE.L  A0,D0
    BEQ.S   .DEL
    BRA.S   .GLT
.GLL:   MOVE.L  4(A0),D0
    SUB.L   D3,D0
    MOVE.L  D0,(A5)+
    MOVE.W  8(A0),(A5)+     ; ADD OPERSIZE (V10+ ONLY)
    MOVE.L  (A0),A0
.GLT:   MOVE.L  A0,D0
    BNE.S   .GLL
    CLR.L   (A5)+
    BRA .LOOP
.EX:    MOVE.W  #-1,(A5)+
    TST.L   D1
    BEQ.S   .LEAVE
    MOVE.L  D5,D2
    MOVE.L  A5,D3
    SUB.L   D2,D3
    MOVE.L  D5,A0
    MOVE.L  D3,D0
    SUBQ.L  #8,D0           ; CHUNKSIZE-HEAD(2)-SKIP(6)
    MOVE.L  D0,4(A0)
    BSR WRITEFILE
.LEAVE: RTS
.DEL:   SUBQ.L  #1,D1
    MOVE.L  A1,A5
    BRA.W   .LOOP
;*-*
;; Modules infos
WRITEMODINFOS:
    MOVEQ   #0,D1           ; D1=MOD COUNT
    MOVE.L  #ESTACKBUF,D5
    MOVE.L  D5,A5           ; D5,A5=BUF
    MOVE.W  #9,(A5)+
    CLR.L   (A5)+
    MOVE.L  ACODE,D3        ; D3=ACODE
    LEA MODINFOLIST,A0
.XL:MOVE.L  (A0),D0
    BEQ.W   .EX
    MOVE.L  D0,A0           ; A0=MODINFO
    MOVE.L  MI_LIST(A0),D0
    BEQ.S   .XL
    MOVE.L  D0,A1           ; A1=PROCCLASS

    MOVE.L  MI_NAMEPTR(A0),A3
    MOVE.L  A5,A6           ; A6=BEGIN ONE MOD
    ADDQ.L  #2,A5
.COPY:  MOVE.B  (A3)+,(A5)+     ; COPY MODNAME
    BNE.S   .COPY
    MOVE.L  A5,D0
    SUB.L   A6,D0
    SUBQ.L  #2,D0
    BTST    #0,D0
    BEQ.S   .1
    CLR.B   (A5)+
    ADDQ.L  #1,D0
.1: MOVE.W  D0,(A6)         ; set lenght
    MOVEQ   #0,D4           ; D4=NUMPROCCLASS

.L2:    ;TST.L  PC_ACC(A1)
    ;BEQ.S  .NEXT
    MOVE.W  PC_TYPE(A1),D0
    MOVE.L  PC_INFO(A1),A4      ; A4=ID OR CLASS
    CMP.W   #2,D0
    BNE.S   .3
    MOVE.L  (A4),A3         ; A3=ASC
    TST.L   PC_ACC(A1)      ; PROCC NOACC CHECK
    BEQ.W   .NEXT
    BRA.S   .4
.3: MOVE.L  OASCII(A4),A3
    TST.L   OACC(A4)        ; CLASS NOACC CHECK
    BEQ.W   .NEXT
.4:
    MOVE.W  D0,(A5)+
    ADDQ.L  #1,D4
    ADDQ.L  #1,D1

    MOVE.L  A5,A2
    ADDQ.L  #2,A5
.COPY2: MOVE.B  (A3)+,(A5)+     ; COPY PROCNAME
    BNE.S   .COPY2
    MOVE.L  A5,D0
    SUB.L   A2,D0
    SUBQ.L  #2,D0
    BTST    #0,D0
    BEQ.S   .2
    CLR.B   (A5)+
    ADDQ.L  #1,D0
.2: MOVE.W  D0,(A2)         ; set lenght

    CMP.W   #2,PC_TYPE(A1)
    BNE.S   .5
    MOVE.L  6(A4),D0
    BEQ.S   .7
    MOVE.L  D0,A3
    MOVE.W  (A3),(A5)+
    BRA.S   .8
.7: MOVE.W  #-1,(A5)+
.8: MOVE.L  PC_ACC(A1),A3
    ADDQ.L  #2,A5
    MOVE.L  A5,A4
.L3:    MOVE.L  4(A3),D2
    SUB.L   D3,D2
    MOVE.L  D2,(A5)+
    MOVE.L  (A3),D0
    MOVE.L  D0,A3
    TST.L   D0
    BNE.S   .L3
    MOVE.L  A5,D0
    SUB.L   A4,D0
    LSR.L   #2,D0
    MOVE.W  D0,-2(A4)
    BRA.W   .6

.5: MOVE.L  PC_INFO(A1),A3      ; WE GET HERE FOR CLASS
    MOVE.L  A5,A4
    ADDQ.L  #2,A5
    LEA OACC(A3),A3
    MOVE.L  ACODE(PC),D2
.CLL:   MOVE.L  (A3),D0
    BEQ.S   .CLE
    MOVE.L  D0,A3
    MOVE.L  4(A3),D0
    SUB.L   D2,D0
    MOVE.L  D0,(A5)+
    MOVE.W  8(A3),(A5)+
    BRA.S   .CLL
.CLE:   MOVE.L  A5,D0
    SUB.L   A4,D0
    DIVU    #6,D0
    MOVE.W  D0,(A4)

.6:
.NEXT:  MOVE.L  (A1),D0
    MOVE.L  D0,A1
    CHESTB  A5,D2,3,iERROR37
    TST.L   D0
    BNE.W   .L2
    TST.L   D4
    BNE.S   .10
    MOVE.L  A6,A5
    BRA.S   .11
.10:    CLR.W   (A5)+
.11:    BRA.W   .XL

.EX:    CLR.W   (A5)+
    TST.L   D1
    BEQ.S   .LEAVE
    MOVE.L  D5,D2
    MOVE.L  A5,D3
    SUB.L   D2,D3
    MOVE.L  D5,A0
    MOVE.L  D3,D0
    SUBQ.L  #6,D0           ; CHUNKSIZE-HEAD(2)-SKIP(4)
    MOVE.L  D0,2(A0)
    BSR WRITEFILE
.LEAVE: RTS
;*-*
;*-*
;*-*

