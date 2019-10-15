APPEND_LISTS:
    MOVE.L      #.MESSY,D2
    MOVE.L      #.ENDMESSY,D3
    SUB.L       D2,D3
    BTST    #7,CODEPREFS+2
    BNE.S   .1
    JSR         WRITECON
.1:
    BTST        #2,CODEPREFS+3
    BEQ         .append
    CMP.L       #1,NumHunks
    BEQ         .append
    MOVE.L      HunkList,A0
    MOVE.L      A4,H_END(A0)
    MOVE.L      a0,-(a7)
    move.l      a4,-(a7)
    bsr         APPEND
    move.l      (a7)+,d7
    move.l      (a7)+,d6
    cmp.l       a4,d7
    beq         .nothingtodo
    GETM        A0
    MOVE.L      HunkList,(A0)+
    MOVE.L      A0,HunkList
    exg.L       A0,D6
    move.l      d6,H_PREV(A0)
    exg.l       a0,d6
    MOVE.L      d7,(A0)+
    move.l      #$3ea,(a0)+
    move.l      a4,(a0)+
    clr.l       (a0)+
    clr.l       (a0)+
    DONEM       A0
    ADDQ.L      #1,NumHunks
.nothingtodo:
    MOVE.l      A4,CURACODE
    rts

.append:
    bsr         APPEND
    MOVE.L      HunkList,A0
    MOVE.L      A4,H_END(A0)
    rts

.MESSY:
    DC.B        'linking arrays ...',$A
.ENDMESSY:
    EVEN


APPEND:
    lea         IMMLH,A0
.LOOP2:
    move.l      a4,d0
    btst        #0,d0
    beq         .SK1
    clr.b       (a4)+
.SK1:
    MOVE.l      (A0),D0
    BEQ         .END2
    MOVE.l      D0,A0
    move.w      4(a0),d0
    MOVE.L      A0,-(a7)
    addq.l      #2,a4       ; look below
    bsr         ADDLABEL
    move.l      (a7)+,a0
    move.l      a4,-(a7)
    subq.l      #2,a4       ; look above
    move.l      6(a0),a1
    move.l      10(a0),d0
    subq.l      #1,d0
.11:move.b      (a1)+,(a4)+
    dbf         d0,.11

    lea         14(a0),a1   ; ref list
    move.l      a0,d7
.1L:move.l      a4,a6
    move.l      (a1),d0     ; next
    beq         .1e         ; if any
    move.l      d0,a1       ;
    move.l      (a7),a4     ;
    add.w       10(a1),a4   ;
    move.w      8(a1),d0    ;
    bsr         ADDBRANCHRELOC
    move.l      a6,a4       ;
    move.w      8(a1),d0    ;
    bsr         ADDLABEL
    move.l      4(a1),a0
    move.w      (a0)+,d0
.1c:tst.w       d0
    beq         .1L
    move.b      (a0)+,(a4)+
    subq.l      #1,d0
    bra         .1c

.1e:move.l      d7,a0
    addq.l      #4,a7
    bra         .LOOP2
.END2:
    MOVE.l      A4,CURACODE
    rts

