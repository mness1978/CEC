;; Linker
; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The Unit Linking Subroutines.                               ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;

HunkList:
    DC.L    0
NumHunks:
    DC.L    1
bgName:
    DC.L    0
bgPri:
    DC.l    0
SetupMod:
    DC.L    0
CleanupMod:
    DC.L    0
InitError:     ; [internal]
    DC.W    0

;; Link module
MMODE:    DC.W    0       ; =D1
THISMOD:  DC.L    0

MINIMUMMODULE:
    MOVEQ   #2,D1           ; MODULE IN MAIN FROM OTHER MODULE
    BRA.S   MODULESTART
MODULEJOBENTRY:
    MOVEQ   #0,D1           ; NORMAL MODULE IN MAIN
    TSTMOD
    BEQ.S   MODULESTART
    MOVEQ   #1,D1           ; MODULE IN MODULE
MODULESTART:
    MOVE.W  D1,MMODE
    CLR.W   LOADEDVERSION
DOMODULEJOB:          ; A0=PTR JOBCODE
    MOVE.W  MMODE(PC),D1
    MOVE.W  (A0)+,D0        ; CAN TRASH ANYTHING
    BEQ.S   .1
    CMP.W   #1,D0
    BEQ ADDCONST
    CMP.W   #2,D0
    BEQ ADDOBJECT
    CMP.W   #3,D0
    BEQ ADDCODE
    CMP.W   #4,D0
    BEQ ADDPROCS
    CMP.W   #5,D0
    BEQ ADDSYSCONST
    CMP.W   #6,D0
    BEQ BINDOTHERLIB
    CMP.W   #7,D0
    BEQ ADDRELOC
    CMP.W   #8,D0
    BEQ ADDGLOBSENTRY
    CMP.W   #9,D0
    BEQ ADDMODULES
    CMP.W   #10,D0
    BEQ.S   ADDDEBUG
    CMP.W   #11,D0
    BEQ.S   ADDMACROS
    BRA ERROR36         ; SUPPOSED TO BE OTHER JOBCODES
.1: RTS
;; AddMacros
ADDMACROS:
    JSR PREPREADMACROS
    BRA DOMODULEJOB
;*-*
;; AddDebug
ADDDEBUG:
    MOVE.L  A0,A6           ; A6=DEBUGHUNK
    MOVE.L  4(A6),D0
    ADDQ.L  #2,D0
    LSL.L   #2,D0
    ADD.L   D0,A0           ; skip LINE debug
    CMP.W   #1,D1
    BEQ.S   .NL
    GETM    A2
    MOVE.L  CODELIST(PC),D0
    BNE.S   .NN
    MOVE.L  A2,CODELIST
    BRA.S   .C
.NN:    MOVE.L  CODETAIL(PC),A1
    MOVE.L  A2,(A1)
.C: MOVE.L  A2,CODETAIL
    CLR.L   (A2)+
    MOVE.W  #8,(A2)+
    MOVE.L  DEBUGLIST,(A2)
    MOVE.L  A2,DEBUGLIST
    ADDQ.L  #4,A2
    MOVE.L  A6,(A2)+
    CLR.L   (A2)+
    DONEM   A2
.NL:    CMP.L   #$3F1,(A0)
    BNE.S   .NL2
    MOVE.L  4(A0),D0
    ADDQ.L  #2,D0
    LSL.L   #2,D0
    ADD.L   D0,A0           ; skip EVAR debug
.NL2:   TST.L   (A0)+           ; BLANK TERMINATOR
    BNE ERROR36
    BRA DOMODULEJOB
;*-*
;; AddModules
ADDMODULES:
    MOVE.L  (A0)+,D0
    CMP.W   #1,D1
    BNE.S   .XL
    ADD.L   D0,A0
    BRA.W   DOMODULEJOB
.XL:MOVE.W  (A0)+,D0
    BEQ.W   .X
    MOVE.L  A0,A1           ; A1=MODNAME
    ADD.W   D0,A0
    MOVE.L  A1,A2
.C: TST.B   (A2)+
    BNE.S   .C
    MOVE.L  A2,CACHENAMEEND
    MOVE.L  A1,CACHENAMESTART
    JSR SEARCHINMODLIST
    GETM    A3
    TST.L   D0
    BNE.S   .L2
    ADDQ.W  #1,AGAIN        ; SIGNAL NEW MODULES
    MOVE.L  MODINFOLIST,(A3)    ; MI_NEXT
    MOVE.L  A3,MODINFOLIST
    ADDQ.L  #4,A3
    MOVE.W  #$100,(A3)+     ; MI_FLAGS
    SUB.L   A1,A2
    MOVE.L  A2,(A3)+        ; MI_NAMELEN
    CLR.L   (A3)+           ; MI_MOD
    CLR.L   (A3)+           ; MI_LIST
    MOVE.L  A1,(A3)+        ; MI_NAMEPTR
.L2:    MOVE.W  (A0)+,D0
    BEQ.S   .D
    MOVE.W  (A0)+,D1
    MOVE.L  A0,A1           ; A1=PROCCLASS NAME
    ADD.W   D1,A0
    MOVE.L  CODELIST(PC),D1     ; HOOKUP TAIL OF CODEREM
    BNE.S   .NN
    MOVE.L  A3,CODELIST
    BRA.S   .CC
.NN:    MOVE.L  CODETAIL(PC),A2
    MOVE.L  A3,(A2)
.CC:    MOVE.L  A3,CODETAIL
    CLR.L   (A3)+
    CMP.W   #1,D0
    BEQ.S   .CL
    MOVE.W  #5,(A3)+        ; PROC/LAB ACCESS
    MOVE.L  A1,(A3)+
    MOVE.L  A0,(A3)+
    ADDQ.L  #2,A0
    MOVE.W  (A0)+,D0
    LSL.W   #2,D0
    ADD.W   D0,A0
    BRA.S   .L2
.CL:    MOVE.W  #6,(A3)+        ; CLASS ACCESS
    MOVE.L  A1,(A3)+
    MOVE.L  A0,(A3)+
    MOVE.W  (A0)+,D0
    MULU    #6,D0
    ADD.W   D0,A0
    BRA.S   .L2
.D: DONEH   A3
    BRA.W   .XL
.X: BRA.W   DOMODULEJOB
;*-*
;; AddGlobals
ADDGLOBSENTRY:
    MOVEQ   #0,D2
    CMP.W   #SKIPMARK,(A0)
    BNE.S   .1
    ADDQ.L  #2,A0
    MOVE.L  (A0)+,D2
.1: CMP.W   #1,D1
    BNE.S   ADDGLOBS
    ADD.L   D2,A0
    TST.L   D2
    BNE.W   DOMODULEJOB
    INTERN  103
ADDGLOBS:
    MOVE.W  (A0)+,D0        ; D0=LEN
    BMI.W   DOMODULEJOB
    MOVE.L  A0,A1           ; A1=NAME
    ADD.W   D0,A0
    MOVE.L  A0,A2           ; A2=OFFSETSTAB
    MOVE.W  LOADEDVERSION(PC),D2
    SUB.W   #10,D2
.XL:TST.L   (A0)+
    BEQ.S   .OUT
    TST.W   D2
    BMI.S   .XL
    ADDQ.L  #2,A0
    BRA.S   .XL
.OUT:   TST.W   D0          ; NOW FIND OFFSET
    BNE.S   VREF
    GETM    A5          ; NOT LINKED INTO HASH TABLE!
    MOVE.L  #.NAME,(A5)+        ; ANONYMOUS VARIABLE
    MOVE.B  #GLOBV,(A5)+        ; SET GLOBAL(=2)
    MOVE.B  #5,(A5)+        ; REF+EXPORT
    CLR.L   (A5)+
    SUBQ.W  #4,NRGLOB
    MOVE.W  NRGLOB,D2
    MOVE.W  D2,(A5)+        ; SET OFFS
    EXT.L   D2
    DONEM   A5
    MOVE.L  A5,D2
    SUBQ.L  #8,D2
    SUBQ.L  #4,D2
    BRA ODONE
.NAME:  DC.B    0,0
VREF:   MOVE.L  A1,A3           ; SEARCH FOR VAR
    HASH    A3,D2,D1
    LSL.L   #2,D2
    ADD.L   #IDENTHASH,D2
    MOVE.L  D2,A3           ; A3=HASHENTRYPTR
    LEA 4(A3),A4
.SL:    MOVE.L  -(A4),A4
    MOVE.L  A4,D2
    BEQ.S   .NOTF
    MOVE.L  (A4),A5
    MOVE.L  A1,A6
.CL:    CMPM.B  (A5)+,(A6)+
    BNE.S   .SL
    TST.B   -1(A5)
    BNE.S   .CL
    MOVE.L  A4,D2           ; JAAAH, BESTAAT REEDS!
    BRA.S   ODONE
.NOTF:  GETM    A5          ; LINKED INTO HASH TABLE!
    MOVE.L  (A3),(A5)+
    MOVE.L  A5,(A3)
    MOVE.L  A1,(A5)+        ; ASCIIPTR
    MOVE.B  #GLOBV,(A5)+        ; SET GLOBAL(=2)
    MOVE.B  #5,(A5)+        ; REF+EXPORT
    CLR.L   (A5)+
    SUBQ.W  #4,NRGLOB
    MOVE.W  NRGLOB,D2
    MOVE.W  D2,(A5)+        ; SET OFFS
    MOVE.L  A5,D2
    SUBQ.L  #8,D2
    SUBQ.L  #4,D2
    DONEM   A5
    ;BRA    ODONE
ODONE:  GETM    A3          ; GOT VAR IN D2, NOW HANG IN CODEREM LIST
    MOVE.L  CODELIST(PC),D1
    BNE.S   .NN
    MOVE.L  A3,CODELIST
    BRA.S   .C
.NN:    MOVE.L  CODETAIL(PC),A4
    MOVE.L  A3,(A4)
.C: MOVE.L  A3,CODETAIL
    CLR.L   (A3)+
    MOVE.W  #4,(A3)+
    MOVE.L  A2,(A3)+
    MOVE.L  D2,(A3)+
    MOVE.W  LOADEDVERSION(PC),(A3)+
    DONEM   A3
    BRA ADDGLOBS
;*-*
;; AddCode
ADDCODE:
    MOVE.L  (A0)+,D7
    CMP.W   #1,D1
    BEQ.S   .SKIP
    GETM    A2
    MOVE.L  CODELIST(PC),D0
    BNE.S   .NN
    MOVE.L  A2,CODELIST
    BRA.S   .C
.NN:    MOVE.L  CODETAIL(PC),A1
    MOVE.L  A2,(A1)
.C: MOVE.L  A2,CODETAIL
    CLR.L   (A2)+
    MOVE.W  #2,(A2)+
    MOVE.L  A0,(A2)+
    MOVE.L  D7,(A2)+
    LSL.L   #2,D7
    ADD.L   D7,A0
    DONEM   A2
    BRA DOMODULEJOB
.SKIP:  LSL.L   #2,D7
    ADD.L   D7,A0
    BRA DOMODULEJOB
;*-*
CODELIST:     DC.L    0
CODETAIL:     DC.L    0
;; AddReloc
ADDRELOC:
    MOVE.L  (A0)+,D7
    CMP.W   #1,D1
    BEQ.S   .SKIP
    GETM    A2
    MOVE.L  CODELIST(PC),D0
    BNE.S   .NN
    MOVE.L  A2,CODELIST
    BRA.S   .C
.NN:    MOVE.L  CODETAIL(PC),A1
    MOVE.L  A2,(A1)
.C: MOVE.L  A2,CODETAIL
    CLR.L   (A2)+
    MOVE.W  #3,(A2)+
    MOVE.L  A0,(A2)+
    MOVE.L  D7,(A2)+
    CLR.L   (A2)+
    LSL.L   #2,D7
    ADD.L   D7,A0
    DONEM   A2
    BRA DOMODULEJOB
.SKIP:  LSL.L   #2,D7
    ADD.L   D7,A0
    BRA DOMODULEJOB
;*-*
;; AddSystem
ADDSYSCONST:
    MOVE.L  (A0)+,D0
    CMP.W   #1,D1
    BEQ.S   .S1
    ADD.L   D0,MINSTACK     ; ADD USED TO STACK
.S1:    MOVE.W  (A0)+,D0
    CMP.W   OSVERSION,D0        ; HIGHEST OSVERSION
    BLE.S   .1
    MOVE.W  D0,OSVERSION
    BSET    #4,WARNINGS+3
.1: MOVE.L  (A0)+,D0
    AND.L   #%11011011000110000000000000000000,D0  ; ONLY INTERESTING BITS
    OR.L    D0,CODEPREFS
    MOVE.W  (A0)+,D0
    CMP.W   ECPU,D0         ; BEST CPU
    BLE.S   .2
    MOVE.W  D0,ECPU
    BSET    #4,WARNINGS+3
.2: MOVE.W  (A0)+,D0        ; BEST FPU
    CMP.W   EFPU,D0
    BLE.S   .3
    MOVE.W  D0,EFPU
    BSET    #4,WARNINGS+3
.3: MOVE.W  (A0)+,D0        ; OR BITSET
    MOVE.W  ASMCPU,D3
    OR.W    D3,D0
    CMP.W   D3,D0
    BEQ.S   .4
    MOVE.W  D0,ASMCPU
    BSET    #4,WARNINGS+3
.4: MOVE.W  (A0)+,D0        ; VERSION
    BNE.S   .5
    ADDQ.W  #1,D0           ; 0 is version 1
.5: MOVE.W  D0,LOADEDVERSION
    CMP.W   #CURVERSION,D0
    BHI ERROR51
    ADDQ.L  #4,A0           ; RESERVED
    CMP.W   #11,LOADEDVERSION
    BLT     .x
    MOVE.L  (A0)+,D0
    AND.L   #%1,D0
    OR.L    D0,ICODEPREFS
.x:
    JSR     PATCHER
    BRA DOMODULEJOB
;*-*
;; AddProcs
ADDPROCS:
.XL:MOVE.W  (A0)+,D0
    CMP.W   #-1,D0
    BEQ DOMODULEJOB
    GETM    A4
    EXT.L   D0
    MOVE.L  A0,A1
    HASH    A1,D1,D2
    LSL.L   #2,D1
    MOVE.L  #IDENTHASH,A1
    ADD.L   D1,A1
    MOVE.L  (A1),(A4)+
    MOVE.L  A4,(A1)
    MOVE.L  A4,D6           ; REMIND THIS IDENT=D6
    MOVE.L  A0,(A4)+

;    MOVEM.L D0-A6,-(A7)
;    MOVE.L  A0,D2
;.xx:TST.B   (A0)+
;    BNE     .xx             ; this section
;    MOVE.L  A0,D3           ; writes down
;    SUB.L   D2,D3           ; the procedures
;    JSR WRITECON
;    MOVE.L  #CLINE,D2
;    MOVEQ   #2,D3
;    JSR WRITECON
;    MOVEM.L (A7)+,D0-A6


    MOVE.B  #LAB,(A4)+
    MOVE.B  #1,(A4)+        ; FLAGS: USED  (NEVER EXPORT!)
    ADD.L   D0,A0           ; skip name
    MOVE.L  (A0)+,D7        ; D7=OFFSET
    CMP.W   #1,(A0)+        ; label/proc
    BNE     .LAB            ;
    LEA 14(A4),A3           ; note: 14
    MOVE.L  A3,(A4)+        ; Proc ptr
    MOVE.W  #-2,(A4)+       ; type [proc]
    CLR.L   (A4)+           ; heavy
    MOVE.L  PROCLIST,(A4)+  ; now proc:
    MOVE.L  A4,PROCLIST     ;
    MOVE.W  (A0)+,(A4)+     ; args
    CLR.W   (A4)+           ; FLAGS,REGVARS
    MOVE.W  (A0)+,(A4)+     ; nr loc vars
    CMP.W   #-1,(A0)
    BEQ     .REGSONLY
    CMP.W   #-2,(A0)
    BEQ     .MORE_STUFF
    MOVE.L  A0,(A4)+        ; DEFARGS
.REGSBACK:
    CLR.L   (A4)+           ; of_object
    MOVE.L  D6,(A4)+        ; IDENT
    CLR.L   (A4)+           ; SELF
    CLR.L   (A4)+           ; METHOD
    MOVE.W  (A0)+,D0
    LSL.W   #2,D0
    ADD.W   D0,A0
    MOVE.W  (A0)+,D0
    ADD.W   D0,A0
    BRA     .CODE
.REGSONLY:
    ADDQ.L  #2,A0
    moveq   #0,d0
    MOVE.W  (A0),D0
    ADD.L   D0,D0
    BSET    #5,-4(a4)
    move.l  a0,(a4)+
    ADD.L   D0,A0
    ADDQ.L  #2,A0
    bra     .REGSBACK
.MORE_STUFF:
    ADDQ.L  #2,A0
    MOVEM.L D0-D7/A1-A6,-(A7)
    MOVEQ   #8,D0
    JSR NEW
    MOVE.L  D0,A1
    cmp.w   #64,(a0)+
    BEQ     .M2
    MOVE.L  CleanupMod(PC),(A1)+
    MOVe.L  A1,CleanupMod
    BSET    #7,-4(A4)
    BRA     .M1
.M2:MOVE.L  SetupMod(PC),(A1)+
    MOVe.L  A1,SetupMod
    BSET    #6,-4(A4)
.M1:MOVE.L  A4,(A1)
    SUB.L   #6,(A1)+
    MOVEM.L (A7)+,D0-D7/A1-A6
    CLR.L   (A4)+
    BRA .REGSBACK

.LAB:   CLR.L   (A4)+           ; LABEL
    MOVE.W  #-1,(A4)+
    CLR.L   (A4)+           ; EXTRA!!
.CODE:  CMP.W   #1,MMODE        ; CHECK IF MOD IN MOD
    BEQ.S   .1
    MOVE.L  CODETAIL(PC),A1
    MOVE.L  A4,(A1)
    MOVE.L  A4,CODETAIL
    CLR.L   (A4)+
    MOVE.W  #1,(A4)+
    MOVE.L  D6,(A4)+
    MOVE.L  D7,(A4)+
.2: DONEM   A4
    BRA .XL
.EX:    BRA DOMODULEJOB
.1: MOVE.L  THISMOD(PC),A1      ; A1=MODINFO
    MOVE.L  D6,A2           ; A2=ID
    SUBQ.W  #2,10(A2)
    MOVE.L  MI_LIST(A1),(A4)    ; PC_NEXT
    MOVE.L  A4,MI_LIST(A1)
    MOVE.L  A4,VARHEAVY(A2)
    ADDQ.L  #4,A4
    MOVE.W  #2,(A4)+        ; PC_TYPE
    MOVE.L  D6,(A4)+        ; PC_INFO
    CLR.L   (A4)+           ; PC_ACC
    BRA.S   .2
;*-*
;; FixProcCode
FIXPROCCODE:          ; RESOLVE ADDRESSES OF CODE IN MODULES
    MOVEM.L A0-A3/D0-D4/A5,-(A7)    ; EXPECTS CODE IN A4
    LEA CODELIST(PC),A0     ; A0=CODEREM
    MOVE.L  LABM+8,A2       ; A2=LABELS
.XL:MOVE.L  (A0),A0
    MOVE.L  A0,D0
    BEQ .E
    MOVE.W  4(A0),D0
    CMP.W   #2,D0
    BNE.S   .ID
    MOVE.L  6(A0),A1        ; CODE + LEN
    MOVE.L  10(A0),D1
    SUBQ.L  #1,D1
    MOVE.L  A4,D2           ; D2=COPIED CODEBASE
    MOVE.L  D2,10(A0)       ; SET LEN FIELD TO CODEBASE!

    MOVE.L  D1,D0           ;
    LSL.L   #2,D0           ;
    ADD.L   A4,D0           ; CHECK CODEBUF
    MOVE.L  D0,CURACODE     ;
    JSR CHECK3          ;
    MOVE.L  A4,CURACODE     ;

.C: MOVE.L  (A1)+,(A4)+
    DBRA    D1,.C
    BRA.S   .XL
.ID:    CMP.W   #1,D0
    BNE.S   .REL
    MOVE.L  6(A0),A3
    MOVEQ   #0,D3
    MOVE.W  10(A3),D3
    LSL.L   #2,D3
    MOVE.L  10(A0),D1       ; OFFSET
    ADD.L   D2,D1
    MOVE.L  D1,0(A2,D3.L)       ; SET LABEL
    BRA.S   .XL

.REL:   CMP.W   #3,D0
    BNE.S   .GLOB
    MOVE.L  A4,-(A7)
    MOVE.L  6(A0),A3
    MOVE.L  10(A0),D3
    SUBQ.L  #1,D3
    MOVE.L  A0,-(A7)
    MOVE.L  #EFUNCBYTE,A1
    MOVE.L  D2,D4
    SUB.L   ACODE,D4        ; D4=MOD OFFSET
.RELL:
    MOVE.L  (A3),D0
    BPL.S   .NEXT
    MOVEQ   #31,D1
    BCLR    D1,D0
    MOVE.L  D0,A4
    ADD.L   D2,A4
    MOVE.W  2(A4),D0
    JSR ADDBRANCHRELOC
    MOVE.B  #-1,-10(A1,D0)

    ADDQ.L  #4,A3
    DBRA    D3,.RELL
    BRA.S   .Q
.NEXT:
    ADDQ.L  #4,A3           ; WAS: ADD.L    D4,(A3)+
    ADD.L   D2,D0
    MOVE.L  D0,A5
    ADD.L   D4,(A5)         ; PATCH NEW CODE
    DBRA    D3,.RELL
.Q: MOVE.L  (A7)+,A0
    MOVE.L  (A7)+,A4
    BRA .XL

.GLOB:  CMP.W   #4,D0
    BNE.S   .CLSSX
    MOVE.L  10(A0),A3
    MOVE.W  10(A3),D0       ; D0.W=A4OFFSET
    MOVE.L  6(A0),A3
    CMP.W   #10,14(A0)
    BPL.S   .NG
.GLL:   MOVE.L  (A3)+,D1
    BEQ .XL
    MOVE.L  D1,A1
    ADD.L   D2,A1
    MOVE.W  D0,(A1)
    BRA .GLL
.NG:    ADDQ.W  #4,D0           ; VERSION WITH EXTRA OPERSIZE
.NGLL:  MOVE.L  (A3)+,D1
    BEQ .XL
    MOVE.L  D1,A1
    ADD.L   D2,A1
    MOVE.W  D0,D3
    SUB.W   (A3)+,D3
    MOVE.W  D3,(A1)
    BRA .NGLL

.CLSSX: CMP.W   #7,D0           ; CLASSX ACC RESOLVE HERE
    BNE.W   .CLASS
    MOVE.L  10(A0),D0
    ADD.L   D2,D0           ; D0=ABS DELCODE
    MOVE.L  6(A0),A1        ; A1=OBJH
    MOVE.L  D0,ODCODE(A1)
    MOVE.L  14(A0),A3       ; A3=ACCVALS
    LEA OACC(A1),A1     ; A1=ACCLIST
    MOVE.L  A0,-(A7)
.CLL:   MOVE.W  (A3)+,D0
    CMP.W   #-1,D0
    BEQ.S   .CLLX
    GETM    A0
    MOVE.L  (A1),(A0)
    MOVE.L  A0,(A1)
    ADDQ.L  #4,A0
    MOVE.L  (A3)+,D1
    ADD.L   D2,D1
    MOVE.L  D1,(A0)+
    MOVE.W  D0,(A0)+
    DONEM   A0
    BRA.S   .CLL
.CLLX:  MOVE.L  (A7)+,A0
    BRA .XL

.DEBUG: CMP.W   #8,D0
    BNE.S   .PROC
    MOVE.L  D2,D1
    SUB.L   ACODE,D1
    MOVE.L  D1,14(A0)
    BRA .XL

.PROC:  CMP.W   #5,D0
    ;BNE.S  .           ; PROC ACC RESOLVE HERE
    BRA .XL

.CLASS: CMP.W   #6,D0           ; CLASS ACC RESOLVE HERE
    BNE.S   .DEBUG
    MOVE.L  6(A0),D1
    JSR FINDOBJ
    TST.L   D0
    BEQ.S   .NOTF
    MOVE.L  D0,A1           ; A1=OBJ
    LEA OACC(A1),A1
    MOVE.L  10(A0),A3       ; A3=ACCL
    MOVE.W  (A3)+,D0
    SUBQ.L  #1,D0
.CAL:   GETM    A6
    MOVE.L  (A1),(A6)
    MOVE.L  A6,(A1)
    ADDQ.L  #4,A6
    MOVE.L  (A3)+,D1
    ADD.L   D2,D1
    MOVE.L  D1,(A6)+
    MOVE.W  (A3)+,(A6)+
    DONEM   A6
    DBRA    D0,.CAL
    BRA .XL
.NOTF:  MOVE.L  D1,ERROROBJ
    BRA ERROR57

.E: MOVEM.L (A7)+,A0-A3/D0-D4/A5
    RTS
;*-*
;; AddConst
ADDCONST:
    CMP.W   #6,LOADEDVERSION
    BMI.S   .XL
    MOVE.L  (A0)+,D0
    CMP.W   #2,D1
    BNE.S   .XL
    ADD.L   D0,A0
    BRA DOMODULEJOB
.XL:cmp.b   #-1,(a0)
    beq     .TYPED
    MOVE.W  (A0)+,D0        ; D0=LEN ASC
    BEQ.S   .1
    GETM    A5
    MOVE.L  (A0)+,D1
    MOVE.L  A0,A1
    bsr     .H
    LSL.L   #2,D2
    ADD.L   #CONSTHASH,D2
    MOVE.L  D2,A1
    MOVE.L  (A1),(A5)
    MOVE.L  A5,(A1)
    ADDQ.L  #4,A5
    MOVE.L  A0,(A5)+
    MOVE.L  D1,(A5)+
    CLR.W   (A5)+
    EXT.L   D0
    ADD.L   D0,A0
    DONEM   A5
    BRA.S   .XL
.1: BRA DOMODULEJOB

.H: HASH A1,D2,D3
    RTS

.TYPED:
    addq.l  #2,a0
    MOVE.W  (A0)+,D0        ; D0=LEN ASC
    BEQ.S   .1
    GETM    A5
    MOVE.L  A0,A1
    bsr     .H
    LSL.L   #2,D2
    ADD.L   #CONSTHASH,D2
    MOVE.L  D2,A1
    MOVE.L  (A1),(A5)
    MOVE.L  A5,(A1)
    ADDQ.L  #4,A5
    MOVE.L  A0,(A5)+
    ext.l   d0
    add.l   d0,a0
    move.w  (a0),d0
    addq.l  #3,d0
    MOVE.L  a0,(A5)+
    move.b  #2,(a5)+
    CLR.B   (A5)+
    ext.l   d0
    ADD.L   D0,A0
    move.l  a0,d0
    btst    #0,d0
    beq     .CC
    addq.l  #1,d0
.CC:DONEM   A5
    BRA     .XL

;*-*
;; AddObject
ADDOBJECT:
    CMP.W   #6,LOADEDVERSION
    BMI.S   .1
    MOVE.L  (A0)+,D0
    ;CMP.W  #2,D1           ; ever?
    ;BNE.S  .1
    ;ADD.L  D0,A0
    ;BRA    DOMODULEJOB
.1: ADDQ.W  #1,OBJCOUNT
    MOVE.W  OBJCOUNT(PC),D5
    GETM    A5
    MOVE.W  (A0)+,D0        ; D0=LEN ASC
    BEQ ERROR39             ; module crashed
    MOVE.W  (A0)+,D1        ;
    CMP.W   #-1,D1          ;
    BNE ERROR39             ; must be object header
    MOVE.L  OLIST,(A5)+     ;
    MOVE.L  A5,OLIST        ;
    ADDQ.L  #2,A0           ; OFF MAKES NO SENSE FOR HEAD
    CLR.W   (A5)+           ; DELEGATESIZE=0
    MOVE.W  #%1000000000,(A5)+  ; SET IMPORT
    MOVE.L  A5,A4           ; A4=SIZE.W OBJ
    CLR.W   (A5)+           ; STILL 0
    MOVE.W  D5,(A5)+
    MOVE.L  A0,(A5)+

    cmp.l   #"exec",(A0)
    bne     .0
    CMP.L   #"base",4(a0)
    bne     .0
    cmp.b   #0,8(a0)
    bne     .0
    move.l  OLIST,OBJ_EXECBASE
.0:
    cmp.l   #"intu",(a0)
    bne     .01
    cmp.l   #"itio",4(a0)
    bne     .01
    cmp.l   #"nbas",8(a0)
    bne     .01
    cmp.w   #"e"*256,12(a0)
    bne     .01
    move.l  OLIST,OBJ_INTUIBASE
.01:
    cmp.l   #"gfxb",(a0)
    bne     .02
    cmp.l   #"ase"*256,4(a0)
    bne     .02
    move.l  OLIST,OBJ_GFXBASE
.02:
    cmp.l   #"dosb",(a0)
    bne     .03
    cmp.l   #"ase"*256,4(a0)
    bne     .03
    move.l  OLIST,OBJ_DOSBASE
.03:
    cmp.l   #"wbst",(a0)
    bne     .04
    cmp.l   #"artu",4(a0)
    bne     .04
    cmp.w   #"p"*256,8(a0)
    bne     .04
    move.l  OLIST,OBJ_WBMESSAGE
.04:
    cmp.l   #"rast",(a0)
    bne     .05
    cmp.l   #"port",4(a0)
    bne     .05
    tst.b   8(a0)
    bne     .05
    move.l  OLIST,OBJ_RASTPORT
.05:
    MOVE.L  A5,A6           ; A6=_TAIL_ MEMBER LIST
    CLR.L   (A5)+           ; MEMBERLIST=0
    CLR.L   (A5)+           ; METHODLIST=0
    CLR.L   (A5)+           ; SUPERCLASS=0
    CLR.L   (A5)+           ; DELEGATECODE=NIL
    CLR.L   (A5)+           ; ACCESLIST
    MOVE.L  #-1,(A5)+       ; DELOFFSET+DESTRUCTOFF
    EXT.L   D0
    ADD.L   D0,A0
    DONEM   A5
ADDOBJECTLOOP:
    MOVE.W  (A0)+,D0        ; D0=LEN ASC
    BEQ     .1              ; last one
    GETM    A5              ;
    MOVE.W  (A0)+,D1        ;
    CLR.L   (A5)+           ; ONEXT
    MOVE.L  A5,(A6)         ;
    LEA ONEXT(A5),A6        ;
    CLR.L   (A6)            ;
    MOVE.W  (A0)+,(A5)+     ; OOFF
    CLR.W   (A5)+           ; OFLAGS
    MOVE.W  D1,(A5)+        ; OSIZE
    MOVE.W  D5,(A5)+        ; OID
    TST.W   D0
    BMI.S   .PRIV
    MOVE.L  A0,(A5)+        ; OASCII
    EXT.L   D0
    ADD.L   D0,A0
    BRA.S   .3
.PRIV:
    CLR.L   (A5)+           ; OASCII
.3: CMP.W   #6,LOADEDVERSION
    BMI.S   .4
    MOVEQ   #0,D0
    MOVE.W  (A0)+,D0
    BEQ.S   .4
    BPL.S   .5
    MOVE.W  (A0)+,D0
    ; HERE WE CAN SEARCH FOR AN OBJECT THAT EQUALS A0
    MOVE.L  A0,(A5)+
    ADD.W   D0,A0
    MOVE.L  OBJTYPELIST(PC),D0
    MOVE.L  A5,OBJTYPELIST
.5: MOVE.L  D0,(A5)+
    BSET    #1,OFLAGS+4(A6)
.4: DONEM   A5
    BRA     ADDOBJECTLOOP
.1: MOVE.W  (A0)+,D0
    BTST    #0,D0
    BEQ.S   .2
    ADDQ.W  #1,D0
.2: MOVE.W  D0,(A4)         ; SET OBJSIZE

    CMP.W   #7,LOADEDVERSION
    BMI.W   .NC
    MOVE.W  (A0)+,D0
    BEQ.W   .NC
    MOVE.L  OLIST,A1        ; A1=OBJECTHEAD
    MOVE.W  D0,ODEL(A1)
    MOVE.L  (A0)+,D0        ; DELCODE
    CMP.W   #1,MMODE
    BEQ.S   .NDC

    GETM    A3
    MOVE.L  CODELIST(PC),D1     ; HOOKUP TAIL OF CODEREM
    BNE.S   .NN
    MOVE.L  A3,CODELIST
    BRA.S   .CC
.NN:    MOVE.L  CODETAIL(PC),A2
    MOVE.L  A3,(A2)
.CC:    MOVE.L  A3,CODETAIL
    MOVE.L  A3,D7           ; D7=CODEREM_STRUCT
    CLR.L   (A3)+
    MOVE.W  #7,(A3)+
    MOVE.L  A1,(A3)+
    MOVE.L  D0,(A3)+
    CLR.L   (A3)+
    DONEM   A3
    BRA.S   .CONT

.NDC:
    MOVEM.L A6/A5,-(A7)
    MOVE.L  THISMOD(PC),A6      ; A6=MODINFO
    CLR.L   ODCODE(A1)
    GETM    A5
    MOVE.L  MI_LIST(A6),(A5)    ; PC_NEXT
    MOVE.L  A5,MI_LIST(A6)
    ADDQ.L  #4,A5
    MOVE.W  #1,(A5)+        ; PC_TYPE
    MOVE.L  A1,(A5)+        ; PC_INFO
    CLR.L   (A5)+           ; PC_ACC
    DONEM   A5
    MOVEM.L (A7)+,A6/A5

.CONT:  ADD.W   (A0)+,A0        ; SKIP SUPER
    MOVE.L  (A0)+,ODELOFF(A1)   ; DELOFF+DESTR
    LEA OMETHOD(A1),A3      ; A3=METHODLIST
.ML:    MOVE.W  (A0)+,D0
    CMP.W   #-1,D0
    BEQ.S   .MX
    GETM    A2          ; A2=MEM
    MOVE.L  A2,A4           ; A4=METHOD
    MOVE.L  (A3),(A2)       ; M_NEXT
    MOVE.L  A4,(A3)
    ADDQ.L  #8,A2
    MOVE.W  D0,(A2)+        ; M_TYPE+M_FLAGS
    MOVE.W  (A0)+,(A2)+     ; M_OFF
    MOVE.W  (A0)+,D0
    MOVE.L  A0,(A2)+        ; M_NAME
    ADD.W   D0,A0
    MOVE.L  A2,4(A4)        ; M_PROC
    MOVE.W  (A0)+,(A2)+     ; PROC_NARGS
    MOVE.L  #$2000000,(A2)+     ; flags=method
    CLR.L   (A2)+
    MOVE.W  (A0)+,D0
    BEQ.S   .NDA
    MOVE.L  A0,-4(A2)       ; PROC_DEFARGS
    SUBQ.L  #2,-4(A2)
    LSL.W   #2,D0
    ADD.W   D0,A0
.NDA:   MOVE.L  A1,(A2)+        ; PROC_OFOBJECT
    CLR.L   (A2)+
    CLR.L   (A2)+
    MOVE.L  A4,(A2)+        ; PROC_METHOD
    DONEM   A2
    BRA.S   .ML
.MX:
    CMP.W   #1,MMODE
    BEQ.S   .AS
    MOVE.L  D7,A2
    MOVE.L  A0,14(A2)
.AS:
.AL:    CMP.W   #-1,(A0)+
    BEQ.S   .AX
    ADDQ.L  #4,A0
    BRA.S   .AL
.AX:
.NC:    BRA DOMODULEJOB
;*-*
OBJTYPELIST:  DC.L    0

;; FixObjTypes
FIXOBJTYPES:
    LEA OBJTYPELIST(PC),A6
.XL:MOVE.L  (A6),D0
    BEQ.S   .X
    MOVE.L  D0,A6
    MOVE.L  -4(A6),D1
    JSR FINDOBJ
    TST.L   D0
    BEQ.S   .ER
.R: MOVE.L  D0,-4(A6)
    BRA.S   .XL
.X: RTS
.ER:    MOVEQ   #4,D0
    BRA.S   .R

;   MOVE.L  -4(A6),ERROROBJ
;   BRA ERROR62
;*-*
;; BindOtherLib
BINDOTHERLIB:
    CMP.W   #2,D1           ; JUST DONE IF LIBINFOS IN SECONDARY
    BNE.S   .1
    RTS
.1: TST.B   (A0)+           ; SKIP LIBNAME FOR NOW
    BNE.S   .1
    MOVE.L  A0,A1
    HASH    A1,D0,D1
    LSL.L   #2,D0
    ADD.L   #IDENTHASH,D0
    MOVE.L  D0,A1

    MOVE.L  A1,A4
    ADDQ.L  #4,A4
.SL:    MOVE.L  -(A4),D0
    BEQ.S   .SD
    MOVE.L  D0,A4
    MOVE.L  (A4),A5
    MOVE.L  A0,A6
.SC:    CMPM.B  (A5)+,(A6)+
    BNE.S   .SL
    TST.B   -1(A5)
    BNE.S   .SC
    CMP.B   #GLOBV,4(A4)
    BNE.S   .SL
    MOVE.W  10(A4),D7
    BRA.W   BLABLABLA

.SD:    GETM    A5          ; ADD BASE AS VAR
    MOVE.L  (A1),(A5)+
    MOVE.L  A5,(A1)
    MOVE.L  A0,(A5)+        ; SET ASCIIPTR
    MOVE.B  #GLOBV,(A5)+        ; SET GLOBAL(=2)
    MOVE.B  #4,(A5)+        ; UNREF+SYSTEMVAR+EXPORT
    CLR.L   (A5)+
    SUBQ.W  #4,NRGLOB
    MOVE.W  NRGLOB,(A5)+        ; SET OFFS
    DONEM   A5
BLURP:
    LEA -12(A5),A5
    TSTMOD
    BEQ.S   .3          ; REMEMBER OFFSET
    GETM    A1
    MOVE.L  OFFSLIST(PC),(A1)+
    MOVE.L  A5,(A1)+
    DONEM   A1
    SUBQ.L  #8,A1
    MOVE.L  A1,OFFSLIST
.3: GINFO   A0,A1,A5
    MOVE.W  NRGLOB,D7       ; BASE OF LIB

BLABLABLA:
.2: TST.B   (A0)+
    BNE.S   .2
    MOVE.L  A0,A5
    MOVE.L  A0,-(A7)
    BSR.W   ADDLIBS2
    MOVE.L  (A7)+,A0
    RTS
OFFSLIST: DC.L    0

DOOFF:                ; GETS OFF IN D0, PRESERVES D0
    TSTMOD
    BEQ.S   .1
    CMP.W   #GLOBOFF+8,D0
    BPL.S   .1
    MOVEM.L A0/A1/D1,-(A7)
    LEA OFFSLIST(PC),A0
.XL:MOVE.L  (A0),A0
    MOVE.L  A0,D1
    BEQ.S   .3
    MOVE.L  4(A0),A1
    CMP.W   10(A1),D0
    BNE.S   .XL
    GENGI   A1,D1
    MOVEM.L (A7)+,A0/A1/D1
.1: RTS
.3: INTERN  101
;*-*

SETUPLIBS:
    BSR ADDLIBS
    RTS

SETUPUTIL:
    LEA UTIL,A5
    MOVE.W  #-124,D7
    BSR     ADDLIBS2
    RTS

ADDLIBS:
    LEA EXEC,A5
    MOVE.W  #-40,D7
    BSR.S   ADDLIBS2
    LEA DOS,A5
    MOVE.W  #-44,D7
    BSR.S   ADDLIBS2
    LEA INTUI,A5
    MOVE.W  #-48,D7
    BSR.S   ADDLIBS2
    LEA GFX,A5
    MOVE.W  #-52,D7
    BSR.S   ADDLIBS2
;   LEA MATH(PC),A5
;   MOVE.W  #-56,D7
;   BSR.S   ADDLIBS2
    RTS

ADDLIBS2:             ; PAR: A5=LIB DATA, D7=OFFSET OF CUR. LIB.
    MOVE.W  #-30,D6         ; D6=OFFSET OF CUR. CALL
    MOVE.L  CURLIBASC,A4        ; A4=LIBSTRUCTS
    JSR CHECK3
.XL:CMP.B   #-1,(A5)
    BEQ.S   .OUT
    MOVE.L  A5,(A4)+        ; ptr to ascii
    MOVE.W  D7,(A4)+        ; xbase offset
    MOVE.W  D6,(A4)+        ; own offset
    MOVE.W  #-1,(A4)+       ; EXCEPTION INDEX
    SUBQ.W  #6,D6
.F: CMP.B   #32,(A5)+       ; NOW GET TO THE NEXT
    BPL .F
    CMP.B   #16,-1(A5)
    BEQ.S   .XL
.F2:    TST.B   (A5)
    BMI.S   .XL
    CMP.B   #16,(A5)
    BPL.S   .XL
    ADDQ.L  #1,A5
    BRA.S   .F2
.OUT:   MOVE.L  A4,CURLIBASC
    CLR.L   (A4)
    CLR.L   4(A4)
    CLR.L   8(A4)
    RTS
;*-*
;; ParseObjectFile
PARSEOBJECTFILE:
; Gets start of "module" in D0,
; it's len in d1,
; it's real pos in d2
; and real len in d3
;
; d2 and d3 are for the binaries, when d0 and d1 are for plain text!!!

    MOVEM.L D0-D7/A0-A6,-(A7)
    MOVE.L  D0,A0   ;start
    MOVe.L  A0,A1
    ADD.L   D1,A1   ;end
    move.l  d2,a2
    move.l  a2,a3
    add.l   d3,a3   ;if not enough, raise error
.LOOP1:
    CMP.L   A1,A0
    BPL     .EXIT
    moveq   #0,d0

    MOVE.B  (A0)+,D0
    BEQ     .EXIT
    CMP.b   #" ",D0
    BEQ     .LOOP1
    CMP.b   #10,D0
    BEQ     .LOOP1
    CMP.b   #9,D0
    BEQ     .LOOP1
    CMP.b   #"/",D0
    BEQ     .TRYCOM
    CMP.b   #"-",D0
    BEQ     .TRYCOM2
    CMP.b   #"P",D0
    BEQ     .PROC
    CMP.b   #"I",D0
    BEQ     .INLINE
    CMP.b   #"S",D0
    BEQ     .STARTUPCODE
    BRA     ERROR88
.EXIT:
    MOVEM.L (A7)+,D0-D7/A0-A6
    RTS

;; parse /* */ //
.TRYCOM:
    CMP.B   #"/",(A0)
    BEQ     .SHORT
    CMP.B   #"*",(A0)
    BNE     ERROR88
.LOOP2:
    CMPA.L  A1,A0
    BPL     ERROR88
    CMP.B   #"*",(A0)+
    BNE     .LOOP2
    CMP.B   #"/",(A0)
    BNE     .LOOP2
    ADDQ.L  #1,A0
    BRA     .LOOP1
;*-*
;; parse ->
.TRYCOM2:
    CMP.B   #">",(A0)
    BEQ     .SHORT
    BRA     ERROR88
;*-*
;; do // ->
.SHORT:
    CMPA.L  A1,A0
    BPL     ERROR88
    CMP.B   #10,(A0)+
    BNE     .SHORT
    BRA     .LOOP1
;*-*
;; proc
.PROC:
    MOVE.L  A1,-(A7)
    LEA     .HEAVYTAB,A6
    cmp.b   #"R",(A0)+
    Bne     ERROR88
    cmp.b   #"O",(a0)+
    bne     ERROR88
    cmp.b   #"C",(a0)+
    BNE     ERROR88

    BSR .SKIPWHITE

    GETM    A4
    move.l  a4,a5           ; a5 = name

    move.b  (a0)+,D0
    MOVE.B  D0,(A4)+
    MOVE.B  (A6,D0),D0
    CMP.B   #2,D0
    BNE     ERROR88


.LP:

    MOVE.B  (A0)+,D0
    mOVE.b  D0,(a4)+
    TST.B   (A6,D0)
    BNE .LP
    subq.l  #1,a0

    MOVE.B  #0,-1(A4)       ; NULL-TERMINATED NAME OF COMMAND
    move.l  a4,d0
    btst    #0,d0
    beq     .LS1
    clr.b   (a4)+
.LS1:
    CLR.L   (A4)+
    move.l  a5,LIB_NAME(a4)        ;

    BSR LIBCHASH

    BSR .SKIPWHITE

    CMP.b   #"(",(A0)+
    BNE ERROR88
    MOVEQ   #0,D5           ; NO OF ARGS

    BSR .SKIPWHITE

    MOVEQ   #0,D0

    MOVE.B  (A0)+,D0
    CMP.B   #")",D0
    BEQ     .LX
    SUBQ.L  #1,A0

.NEXT:
    BSR .SKIPWHITE
    MOVE.L  A0,D2
    MOVE.B  (A0)+,D0
    TST.B   (A6,D0)
    BEQ     ERROR88
    ADDQ.L  #1,D5
.LP3:
    MOVE.B  (A0)+,D0
    TST.B   (A6,D0)
    BNE     .LP3

    subq.l  #1,a0

    BSR     .SKIPWHITE
    CMP.B   #",",(A0)+
    BEQ     .NEXT
    CMP.B   #":",-1(A0)
    BEQ .LY
    CMP.B   #")",-1(A0)
    BNE     ERROR88
    BRA .LX
.LY:
    LEA .STREAM(PC),A6
    BSR .CHECK
    NEG.L   D5
    bsr .SKIPWHITE
    CMP.B   #")",(A0)+
    BNE ERROR88

.LX:
    MOVE.W  D5,LIB_ARGS(A4)
    CLR.W   LIB_TYPE(A4)       ;type
    clr.l   LIB_CODE(a4)
    CLR.L   LIB_CLEN(A4)
    CLR.L   LIB_USED(A4)       ;code-rel/use
    CLR.W   LIB_CPU(A4)
    CLR.W   LIB_FPU(A4)
    CLR.W   LIB_MMU(A4)
    MOVE.W  #33,LIB_OSVERS(A4)
    BSR     .FINDCODE
    ADD.L   #LIB_SIZE,A4
    DONEM   A4
    MOVE.L  (A7)+,A1
    BRA     .LOOP1
;*-*
;; inline
.INLINE:
    MOVE.L  A1,-(A7)
    LEA .INL_NAME(PC),A6
    BSR .CHECK
    BSR .SKIPWHITE

    LEA .HEAVYTAB(PC),A6

    GETM    A4
    move.l  a4,a5           ; a5 = name

    move.b  (a0)+,D0
    MOVE.B  D0,(A4)+
    MOVE.B  (A6,D0),D0
    CMP.B   #2,D0
    BLT     ERROR88

;    movem.l d0-a6,-(A7)
;    move.l  a0,d2
;    move.l  #6,d3
;    jsr WRITECON
;    movem.l (a7)+,d0-a6


.INL_LP:

    MOVE.B  (A0)+,D0
    mOVE.b  D0,(a4)+
    TST.B   (A6,D0)
    BNE .INL_LP
    subq.l  #1,a0

    MOVE.B  #0,-1(A4)       ; NULL-TERMINATED NAME OF COMMAND
    move.l  a4,d0
    btst    #0,d0
    beq     .INL_LS1
    clr.b   (a4)+
.INL_LS1:
    clr.l   (a4)+
    move.l  a5,LIB_NAME(a4)        ;

    BSR LIBCHASH

    BSR .SKIPWHITE

    CMP.b   #"(",(A0)+
    BNE ERROR88
    MOVEQ   #0,D5           ; NO OF ARGS

    BSR .SKIPWHITE

    MOVEQ   #0,D0

    MOVE.B  (A0)+,D0
    CMP.B   #")",D0
    BEQ     .INL_LX
    SUBQ.L  #1,A0

.INL_NEXT:
    BSR .SKIPWHITE
    MOVEQ   #0,D2
    MOVE.B  (A0)+,D0
    TST.B   (A6,D0)
    BEQ     ERROR88
    ADDQ.L  #1,D5
.INL_LP3:
    MOVE.B  (A0)+,D0
    TST.B   (A6,D0)
    BNE     .INL_LP3

    subq.l  #1,a0

    CMP.B   #":",(A0)+
    BNE     ERROR88
    MOVE.B  (A0)+,D0
    CMP.B   #"A",D0
    BNE     .INL_SKXX1
    ADDQ.L  #4,D2
    BRA     .INL_SKXX2
.INL_SKXX1:
    CMP.B   #"D",D0
    BNE     ERROR88
.INL_SKXX2:
    MOVE.B  (A0)+,D0
    CMP.B   #"0",D0
    BLT     ERROR88
    CMP.B   #"3",D0
    BGT     ERROR88
    SUB.L   #"0",D0
    AND.L   #3,d0
    add.b   d0,d2
    CMP.B   #3,d2
    beq     ERROR88
    move.b  d2,LIB_TARR-1(a4,d5); Dx/Ax reg, e.g. INLINE Whatever(what:D0,where,A0)

    BSR     .SKIPWHITE

    CMP.B   #",",(A0)+
    BEQ     .INL_NEXT
    CMP.B   #")",-1(A0)
    BNE     ERROR88

.INL_LX:
    MOVE.W  D5,LIB_ARGS(A4)
    MOVE.W  #1,LIB_TYPE(A4) ; type
    CLR.W   LIB_CPU(A4)
    CLR.W   LIB_FPU(A4)
    CLR.W   LIB_MMU(A4)
    mOVE.W  #33,LIB_OSVERS(A4)
    BSR     .FINDCODE
    CLR.L   LIB_USED(A4)    ;code-rel/use

    ADD.L   #LIB_SIZE,A4
    DONEM   A4
    MOVE.L  (A7)+,A1
    BRA     .LOOP1
;*-*
;; startup code
.STARTUPCODE:
    MOVE.L  A1,-(A7)
    LEA .STC_NAME(PC),A6
    BSR .CHECK
    BSR .SKIPWHITE

    LEA .HEAVYTAB(PC),A6

    GETM    A4
    move.l  a4,a5           ; a5 = name

    move.b  (a0)+,D0
    MOVE.B  D0,(A4)+
    MOVE.B  (A6,D0),D0
    CMP.B   #2,D0           ; only capital letters..
    BNE     ERROR88

.STC_LP:

    MOVE.B  (A0)+,D0
    mOVE.b  D0,(a4)+
    CMP.B   #2,(A6,D0)      ; CAPITAL LETTERS ONLY
    BEQ .STC_LP
    subq.l  #1,a0

    MOVE.B  #0,-1(A4)       ; NULL-TERMINATED NAME OF CODE
    move.l  a4,d0
    btst    #0,d0
    beq     .STC_LS1
    clr.b   (a4)+
.STC_LS1:
    move.l  LIBLIST(PC),(A4)+
    move.l  A4,LIBLIST
    move.l  a5,LIB_NAME(a4)        ;

    BSR .SKIPWHITE

    MOVE.W  #0,LIB_ARGS(A4)
    MOVE.W  #2,LIB_TYPE(A4) ; type
    BSR     .FINDCODE

    ADD.L   #LIB_SIZE,A4
    DONEM   A4
    MOVE.L  (A7)+,A1
    BRA     .LOOP1
;*-*

;; Check
; A0 - src
; A6 - dest
.CHECK:
    CMPM.B  (A0)+,(A6)+
    BEQ     .CHECK
    TST.B   -1(A6)
    BNE     ERROR88
    sUBQ.L  #1,A0
    CMP.B   #32,(A0)
    BEQ     .CHX
    CMP.B   #")",(A0)
    BEQ     .CHX
    CMP.B   #9,(A0)
    BNE     ERROR88
.CHX:
    RTS
;*-*
;; SKIP WHITESPACES
.SKIPWHITE
    cmp.b   #" ",(a0)+
    beq     .SKIPWHITE
    cmp.b   #9,-1(a0)
    beq     .SKIPWHITE
    SUBQ.L  #1,A0
    RTS
;*-*

.HEAVYTAB:
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0
    DC.B    1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2 ; -O
    DC.B    2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,2 ; -_
    DC.B    0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3 ; -o
    DC.B    3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,0 ; -DEL
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DC.B    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

.INL_NAME:
    DC.B    'NLINE',0
.STC_NAME:
    DC.B    'TARTUP',0
.STREAM:
    DC.B    'STREAM',0
    EVEN

;; code
;; FindCode
.FINDCODE
    MOVEM.L D0-D7/A0-A1/A4-A6,-(A7)
    CMPA.L  A3,A2       ; check if enough code
    BPL     ERROR28     ;

    cmp.l   #$3e7,(a2)  ; we're doing a link object
    bne     ERROR28     ; !
.LC1:
    CMPA.L  A3,A2
    BPL     ERROR28     ; file truncated?

    MOVe.L  (A2)+,D0
    CMP.L   #$3F2,D0
    BEQ     .X

    MOVe.L  (A2)+,D1    ; size of hunk/whatever

    sub.l   #$3e7,d0
    lsl.w   #2,d0
    move.l  .TABLE(PC,d0),A0
    JSR     (a0)
    bra     .LC1
.X: MOVEM.L (A7)+,D0-D7/A0-A1/a4-A6
    RTS
;*-*
.TABLE:
    DC.L    .SKIP, .SKIP, .CODE, .ERRR, .ERRR, .RELO, .ERRR, .ERRR
    DC.L    .XTNS, .ERRR, .SKIP, .ERRR, .ERRR, .ERRR, .ERRR, .ERRR
    DC.L    .ERRR, .ERRR, .ERRR, .ERRR, .ERRR, .ERRR, .ERRR, .ERRR
.CURRENT:
    DC.L    0
;; code
.CODE:
    LSL.L   #2,D1
    MOVE.L  D1,LIB_CLEN(A4)
    MOVE.L  A2,LIB_CODE(A4)
    CLR.L   LIB_RELO(A4)
    ADD.L   D1,A2
    RTS
;*-*
;; skip
.SKIP:
    LSL.L   #2,D1
    ADD.L   D1,A2
    rTS
;*-*
;; extn
.XTNS:
    MOVE.L  A2,LIB_XTNS(A4)
    SUB.L   #4,LIB_XTNS(A4)
.EXTN:
    MOVE.L  d1,d2
    beq .XIT

    MOVE.L  A2,A5
    LOWER A2,D0
    MOVe.L  A5,A2

    rol.l   #8,d2
    and.l   #$FF,D2
    and.l   #$FFFFFF,d1
    cmp.w   #$83,d2
    beq     .0x83
    cmp.w   #$02,d2
    beq     .0x02
.EXT_1:
    lsl.l   #2,d1
    add.l   d1,a2
    move.l  (a2)+,d1
    cmp.w   #$80,d2
    bmi .EXT_2
    lsl.l   #2,d1
    add.l   d1,a2
.EXT_2:
    move.l  (a2)+,d1
    bra .EXTN
.XIT:
    rts
;*-*
;; errr
.ERRR:
    BRA ERROR28
;*-*
;; relo
.RELO:
    SUBQ.L  #8,A2       ;also reloc type!
    MOVE.L  A2,LIB_RELO(A4)
    addq.l  #2,d1
    LSL.L   #2,D1
    ADD.L   D1,A2
    addq.l  #8,a2       ; hunk 1 hunk 0

    rTS
;*-*

;; 0xXX
;; 0x83:
.0x83:
    MOVE.L  A5,A2       ; dos base
    MOVEQ   #-44,D3
    LEA .DBASE,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVEQ   #-48,D3     ; intuition base
    MOVE.L  A5,A2
    LEA .IBASE,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVEQ   #-52,d3     ; gfx base
    MOVE.L  A5,A2
    LEA .GBASE,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVEQ   #-8,d3      ; stdout
    MOVE.L  A5,A2
    LEA .SOUT,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVEQ   #-16,d3     ; stdrast
    MOVE.L  A5,A2
    LEA .SRAST,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVEQ   #-32,d3     ; arg
    MOVE.L  A5,A2
    LEA .ARG,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVEQ   #-36,d3     ; wbmessage
    MOVE.L  A5,A2
    LEA .WBMSG,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVEQ   #-56,D3     ; mathieeesingbas base
    MOVE.L  A5,A2
    LEA .SBBASE,A6
    BSR .CHECK2
    BNE .0x83_skp1
    BSET    #3,CODEPREFS+1
    BRA .0x83_FILL
.0x83_skp1:
    MOVEQ   #-60,D3     ; mathieeesingtrans base
    MOVE.L  A5,A2
    LEA .STBASE,A6
    BSR .CHECK2
    BNE .0x83_skp2
    BSET    #4,CODEPREFS+1
    BRA .0x83_FILL
.0x83_skp2:
    MOVE.L  #-84,D3     ; exception
    MOVE.L  A5,A2
    LEA .XCP,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVE.L  #-96,D3     ; exceptioninfo
    MOVE.L  A5,A2
    LEA .XCPI,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVE.L  #-92,D3     ; stdin
    MOVE.L  A5,A2
    LEA .SIN,A6
    BSR .CHECK2
    BEQ .0x83_FILL

    MOVE.L  #-120,D3    ; pool
    MOVE.L  A5,A2
    LEA .POOL,A6
    BSR .CHECK2
    BNE .0x83_skp3
    BSET    #6,CODEPREFS
    BRA .0x83_FILL
.0x83_skp3:
    MOVE.L  #-124,D3    ; utillib
    MOVE.L  A5,A2
    LEA .UBASE,A6
    BSR .CHECK2
    BNE .0x83_skp4
    BSET    #7,CODEPREFS
    BRA .0x83_FILL
.0x83_skp4:
    CMP.W   #2,LIB_TYPE(A4)
    BEQ .0x83_skp5      ; startup code - additional fields below
    BRA ERROR28
.0x83_skp5:
    MOVE.L  #0,D3
    BRA .0x83_FILL

;; Fill
.0x83_FILL:
    MOVE.L  A5,A2
    LSL.L   #2,D1
    ADD.L   D1,A2
    MOVE.L  (A2)+,D1
    SUBQ.L  #1,D1
    MOVE.L  LIB_CODE(A4),A6
.0x83_DBLOOP:
    MOVE.L  (A2)+,D0
    MOVE.W  D3,(A6,D0.L)
    DBF D1,.0x83_DBLOOP
    MOVE.L  (A2)+,D1
    BRA .EXTN
;*-*
;*-*
;; 0x02:
.0x02:
    MOVE.L  #LIB_CPU,D2
    MOVE.L  A5,A2       ; cpu
    MOVEQ   #-44,D3
    LEA .CPU,A6
    BSR .CHECK2
    BEQ .0x02_FILL

    MOVE.L  #LIB_FPU,D2
    MOVE.L  A5,A2       ; fpu
    MOVEQ   #-44,D3
    LEA .FPU,A6
    BSR .CHECK2
    BEQ .0x02_FILL

    MOVE.L  #LIB_MMU,D2
    MOVE.L  A5,A2       ; mmu
    MOVEQ   #-44,D3
    LEA .MMU,A6
    BSR .CHECK2
    BEQ .0x02_FILL

    MOVE.L  #LIB_OSVERS,D2
    MOVE.L  A5,A2       ; osversion
    MOVEQ   #-44,D3
    LEA .OSV,A6
    BSR .CHECK2
    BEQ .0x02_FILL

    BRA ERROR22
;; sub codes
.0x02_FILL:
    lsl.l   #2,d1
    move.l  a5,a2
    add.l   d1,a2
    move.l  (a2)+,d3
    cmp.l   #LIB_CPU,D2
    beq .A
    cmp.l   #LIB_FPU,d2
    beq .B_
    cmp.l   #LIB_MMU,D2
    beq .C
    move.w  D3,(A4,D2)
._EXIT:
    move.l  (a2)+,d1
    bra .EXTN
;; cpu
.A:
    CMP.L   #$68000,d3
    bpl     .A1
    cmp.l   #$10,d3
    bpl     .A2
    MULU.W  #20,D3
    MOVE.W  D3,(A4,D2)
    BRA ._EXIT
.A2:
    MOVE.W  D3,(A4,D2)
    BRA ._EXIT
.A1:
    SUB.L   #$68000,D3
    MOVE.W  D3,(A4,D2)
    BRA ._EXIT
;*-*
;; fpu
.B_:
    cMP.l   #$68000,d3
    bpl .B1
    cmp.l   #$040,d3
    bpl .B2
    move.w  d3,(A4,D2)
    bra ._EXIT
.B1:
    cmp.l   #$68881,d3
    bpl .B11
    move.w  #2,(a4,d2)
    bra ._EXIT
.B11:
    move.w  #1,(a4,d2)
    bra ._EXIT
.B2:
    cmp.l   #$881,d3
    bpl .B21
    move.w  #2,(a4,d2)
    bra ._EXIT
.B21:
    move.w  #1,(a4,d2)
    bra ._EXIT
;*-*
;; mmu
.C:
    cmp.l   #$68851,d3
    beq .C1
    move.w  #2,(a4,d2)
    bra ._EXIT
.C1:
    move.w  #1,(a4,d2)
    bra ._EXIT
;*-*
;*-*
;*-*
;; Check2
; A2 - src
; A6 - dest
.CHECK2:
    MOVEQ   #1,D0
    CMPM.B  (A2)+,(A6)+
    BNE     .CH2X
    TST.B   -1(A2)
    BNE .CHECK2

    BNE     .CH2X
    MOVEQ   #0,D0
.CH2X:
    TST.L   D0
    RTS
;*-*
;; 0x83 names
.DBASE:
    DC.B    '_dosbase',0
.IBASE:
    DC.B    '_intuitionbase',0
.GBASE:
    DC.B    '_gfxbase',0
.SOUT:
    DC.B    '_stdout',0
.SRAST:
    DC.B    '_stdrast',0
.ARG:
    DC.B    '_arg',0
.WBMSG:
    DC.B    '_wbmessage',0
.SBBASE:
    DC.B    '_mathieeesingbasbase',0
.STBASE:
    DC.B    '_mathieeesingtransbase',0
.XCP:
    DC.B    '_exception',0
.XCPI:
    Dc.B    '_exceptioninfo',0
.SIN:
    DC.B    '_stdin',0
.UBASE:
    DC.B    '_utilitybase',0
.POOL:
    DC.B    '_pool',0

    EVEN
;*-*
;; 0x02 names
.CPU:
    DC.B    '__cpu',0
.FPU:
    DC.B    '__fpu',0
.MMU:
    DC.B    '__mmu',0
.OSV:
    DC.B    '__osversion',0
;*-*
;*-*
;*-*
;; hash
LIBCHASH:
    MOVEM.L D0-A6,-(A7)
    MOVE.L  #LIBHASH,A0     ; A0=TABLE
    HASH    A5,D0,D1
    LSL.L   #2,D0
    LEA     0(A0,D0.L),A2
    MOVE.L  (A2),LIB_NEXT(A4)     ; READ PREVIOUS LIST (OR NIL)
    MOVE.L  A4,(A2)         ; PUT ENTRY IN LIST
    MOVEM.L (A7)+,D0-A6
    RTS
;*-*
LIBLIST:
    DC.L    0

;*-*
;HEADER
;; Exec
; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The InitCode and ExitCode Part ....                         ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;


STACKPOKE:        DC.L    0,0
AFTERDELEGATESLEA:    DC.L    0
BEFORESECONDLEA:      DC.L    0
TOTALDELSIZE:     DC.L    0

BUILDDELEGATECONSTRUCTORS:
    MOVE.L  CURACODE,A4     ; A4=CODE
    MOVEQ   #6,D0
    JSR ADDLABEL

    LEA OLIST+4,A5      ; A5=OLIST
    MOVEQ   #1,D6           ; D6=NUM
.CO:    MOVE.L  -(A5),D0
    BEQ.S   .COX
    MOVE.L  D0,A5
    TST.W   ODEL(A5)
    BEQ.S   .CO
    ADDQ.L  #1,D6
    BRA.S   .CO
.COX:   LSL.L   #2,D6
    MOVEQ   #4,D4           ; D4=CUR_DEL_PTR_OFF

    LEA OLIST+4,A5      ; A5=OLIST
.XL:MOVE.L  -(A5),D0
    BEQ.W   .X
    MOVE.L  D0,A5
    MOVE.W  ODEL(A5),D7     ; D7=DELSIZE
    BEQ.S   .XL

    GETM    A1
    MOVE.L  .GET(PC),(A4)+
    MOVE.L  D6,(A4)+
    MOVE.W  .GET2(PC),(A4)+
    MOVE.L  OACC(A5),(A1)       ; ADD TO ACCESSLIST OF THIS OBJECT
    MOVE.L  A1,OACC(A5)
    ADDQ.L  #4,A1
    MOVE.L  A4,(A1)+
    CLR.W   (A1)+
    DONEM   A1
    CLR.W   (A4)+

    EXT.L   D7
    MOVE.L  ODCODE(A5),A3       ; A3=CODE
    MOVE.L  A3,D0
    BEQ ERROR50

    MOVE.L  A3,D0
    SUB.L   A4,D0
    CMP.L   #-32000,D0
    BMI .LL
    SUBQ.L  #2,D0
    MOVE.W  .BSR(PC),(A4)+
    MOVE.W  D0,(A4)+
    BRA.S   .LD
.LL:    ADD.L   A4,D0
    SUB.L   ACODE,D0
    MOVE.W  .JSR(PC),(A4)+
    MOVE.L  D0,D1
    MOVE.L  A4,D0
    JSR ADDDIVRELOC
    MOVE.L  D1,(A4)+
.LD:
    BSR .ACC
    ADD.L   D7,D6
    ADDQ.L  #4,D4
    BRA.W   .XL
.X: SUBQ.L  #4,D6
    MOVE.L  D6,TOTALDELSIZE
    BSR MATHINITETC
    MOVE.W  .RTS(PC),(A4)+
    MOVE.L  A4,CURACODE
    JSR CHECK3
.RTS:   RTS
.BSR:   BSR .X
.JSR:   JSR .X
.GET:   MOVE.L  A4,A0
    ADD.L   #1,A0
.GET2:  MOVE.L  A0,4(A4)

.ACC:   LEA OACC(A5),A3
    MOVE.L  ODCODE(A5),D5
    SUB.L   ACODE,D5
.AL:    MOVE.L  (A3),D0
    BEQ.S   .AX
    MOVE.L  D0,A3
    MOVE.L  4(A3),A2
    MOVE.W  8(A3),D0
    BNE.S   .AA
    MOVE.W  D4,(A2)
    BRA.S   .AN
.AA:    MOVE.L  D5,(A2)
    MOVE.L  A2,D0
    BSR ADDDIVRELOC
.AN:    BRA.S   .AL
.AX:    RTS


MAKEEXITCODE:
    MOVE.W  #11,CURSPOT
    BTST    #3,CODEPREFS+3
    BNE .EX
    TSTMOD
    BNE .EX
    BTST    #5,CODEPREFS
    BNE .EX
    BTST    #2,CODEPREFS
    BNE     OTHEREXITCODE
    BSR BUILDDELEGATECONSTRUCTORS
    BSR BUILDLIBFUNS
    MOVE.L  TOTALDELSIZE(PC),D1 ; PATCH DELSIZE
    MOVE.L  D1,D2
    MOVE.L  BEFORESECONDLEA(PC),D0
    BEQ.S   .1
    MOVE.L  D0,A0
    MOVE.L  D2,2(A0)
.1: NEG.L   D2
    MOVE.L  AFTERDELEGATESLEA(PC),A0
    MOVE.L  D2,-4(A0)
    MOVE.L  MINSTACK,D0         ; GLOBAL + ALL_LOCAL + DELEGATES
    ADD.L   GLOBSTACK,D0
    ADD.L   D1,D0
    MOVE.L  CODESTACK,D1
    BNE.S   .S
    ADD.L   #MINSTACKSIZE,D0
    MOVE.L  LIBINFO,D1
    BEQ.S   .NL
    SUB.L   #MINSTACKSIZE-MINSTACKSIZELIB,D0
.NL:    MOVE.L  D0,CODESTACK
    BSET    #0,CODEPREFS+3
    BRA.S   .NS
.S: CMP.L   D0,D1
    BPL.S   .S2
    BSET    #2,WARNINGS+3
.S2:    ADD.L   #4000,D0
    CMP.L   D0,D1
    BPL.S   .S3
    BSET    #1,WARNINGS+3
    BCLR    #2,WARNINGS+3
.S3:
.NS:    MOVE.L  CODESTACK,D0
    LEA STACKPOKE(PC),A0
    MOVE.L  (A0)+,A1
    MOVE.L  D0,(A1)
    MOVE.L  (A0),A1
    MOVE.L  D0,(A1)
.EX:    RTS



MAKEINITCODE:
    MOVE.W  #3,CURSPOT
    MOVE.L  CURACODE,A4

    GETM    A0                  ; fill the 0-hunk structure!
    clr.l   (a0)+               ; next
    MOVE.L  A0,HunkList         ;
    MOVE.L  A4,(A0)+            ; start
    MOVE.L  #$3e9,(A0)+         ; type
    clr.l   (a0)+               ; end
    clr.l   (a0)+               ; prev
    clr.l   (a0)+               ; reloc
    DONEM   A0                  ; We'll use it for writing the stuff :)

    BTST    #3,CODEPREFS+3
    BNE .EX
    TSTMOD
    BNE .EX
    BTST    #5,CODEPREFS
    BNE .EX
    BTST    #2,CODEPREFS
    BNE OTHERINITCODE
    TST.L   LIBINFO
    BNE LMAKEINITCODE
    BTST    #4,CODEPREFS
    BNE NSMAKEINITCODE

    BTST    #4,ICODEPREFS+3
    BEQ     .nobg
    BSR BGCODE
.nobg:
    LEA FIRSTSTART(PC),A0   ; NEW STACK
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    MOVE.L  A4,STACKPOKE
    ADDQ.L  #4,A4
    ADDQ.L  #4,A0
    MOVE.L  #LINKC2,D0
    BSR COPYC
    MOVE.L  A4,AFTERDELEGATESLEA    ; PATCH IN SIZE LATER
    MOVE.W  LINKC2(PC),(A4)+
    MOVE.W  NRGLOB,D7
    MOVE.W  D7,(A4)+
    LEA REALSTART(PC),A0
    MOVE.L  A4,A6           ; BACKUP
    MOVE.L  #MIDC,D0
    BSR COPYC
    LEA POKESTRING-REALSTART(A6),A6
    MOVE.L  ARGBYTE,A5
    BTST    #0,(A5)
    BNE.S   .ARGU
    MOVE.L  #$4E714E71,(A6)
    MOVE.W  #$4E71,4(A6)
.ARGU:
;    BTST    #0,CODEPREFS+2
;    BEQ.S   .NVERS
;    MOVE.W  OSVERSION,D0
;    MOVE.B  D0,POKE37-POKESTRING+1(A6)
.NVERS: MOVE.W  GLOBJUMP(PC),(A4)+
    MOVEQ   #5,D0
    JSR ADDBRANCHRELOC
    MOVE.W  EXITC(PC),(A4)+     ; DELEGATES
    MOVEQ   #8,D0
    JSR ADDBRANCHRELOC
    LEA EXITC2(PC),A0
    MOVE.L  #ENDC2,D0
    BSR COPYC
    LEA ENDC2(PC),A0
    MOVE.L  A4,BEFORESECONDLEA
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  A4,STACKPOKE+4
    ADDQ.L  #4,A4
    ADDQ.L  #4,A0
    MOVE.L  #ST2,D0
    BSR COPYC
    MOVE.L  ST2,(A4)+


    TST.L   bgName
    BEQ     .MICX
    MOVE.L  bgName(PC),A0
.MICC:
    MOVE.B  (A0)+,(A4)+
    BNE .MICC
    MOVE.L  A4,D0
    BTST    #0,D0
    BEQ     .MICX
    CLR.B   (A4)+
.MICX:
    BSR FIXPROCCODE
    MOVE.L  A4,CURACODE
.EX:    RTS

MAINRTS:    RTS




FIRSTSTART:           ; CHANGES ALSO TO LFIRSTART!!!
    MOVE.L  4.W,A6
    MOVE.L  A0,A5           ; SAVE A0/D0 TO A5/D7
    MOVE.L  D0,D7
    MOVE.L  #20000,D0       ; THIS .L IS CHANGED!
    MOVE.L  #$10000,D1
    MOVE.L  D0,D2
    JSR -198(A6)
    TST.L   D0
    BNE.S   EXE_1
    MOVEQ   #20,D0
    RTS
A4STORAGE_EXE:
    DC.L    0
EXE_1:
    MOVE.L  D0,D6           ; D6=BOTTOMSTACK
    ADD.L   D2,D0
    JSR -132(A6)        ; --> FORBID
    EXG.L   D0,A7
    MOVE.L  D0,-(A7)
    ADD.L   #1,A7           ; SIZE IS PATCHED IN
LINKC2:
    LINK    A4,#-4
REALSTART:
    JSR -138(A6)        ; --> PERMIT

    MOVE.L  D6,-64(A4)
    SUBA.L  A1,A1       ; check for wbstartup
    JSR -294(A6)        ; findtasK
    MOVE.L  D0,A3
    TST.L   172(A3)
    BNE.S   NOWBST
    LEA 92(A3),A0
    JSR -384(A6)        ; waitport
    LEA 92(A3),A0
    JSR -372(A6)        ; getmsg
    MOVE.L  D0,-36(A4)
    LEA ZEROSPOT(PC),A0
    MOVE.L  A0,-32(A4)      ; WB=NO ARGS
    BRA.S   NOARGSET
NOWBST:
    MOVE.L  A5,A0
    MOVE.L  D7,D0
    MOVE.L  A0,-32(A4)
POKESTRING:
    MOVE.B  #0,-1(A0,D0.W)
NOARGSET:
    MOVE.L  A6,-40(A4)      ; INIT VARS
    LEA EXITC(PC),A0
    MOVE.L  A0,-24(A4)

    LEA INITINTUI(PC),A1    ; OPEN LIBS
POKE37:
    MOVEQ   #33,D0          ; OR V37!
    JSR -552(A6)
    MOVE.L  D0,-48(A4)
    BEQ CLINT

    LEA INITGFX(PC),A1
    MOVEQ   #33,D0
    JSR -552(A6)
    MOVE.L  D0,-52(A4)
    BEQ CLGFX

    LEA INITDOS(PC),A1
    MOVEQ   #33,D0          ; WE WILL USE FUNCTIONS NOT IN V32
    JSR -552(A6)
    MOVE.L  D0,-44(A4)
    BEQ CLDOS

    MOVE.L  D0,A6
    JSR -60(A6)         ; GET STDOUT, MAY BE 0
    MOVE.L  D0,-8(A4)
    JSR -54(A6)         ; GET STDIN
    MOVE.L  D0,-92(A4)
    MOVE.L  A7,-4(A4)
    LEA     A4STORAGE_EXE(PC),A1
    MOVE.L  A4,(A1)
    BRA.S   GLOBJUMP
INITDOS:      DC.B    "dos.library",0
INITINTUI:    DC.B    "intuition.library",0
INITGFX:      DC.B    "graphics.library",0
ZEROSPOT:     DC.B    0
    EVEN
MIDC:
GLOBJUMP:
    JSR GLOBJUMP
EXITC:
    JSR EXITC
EXITC2:
    MOVE.L  -4(A4),A7
    MOVE.L  4.W,A6
    MOVE.L  -20(A4),A2      ; CLEANUP MEMORY
    MOVE.L  A2,D0
    BEQ.S   .3
.4: MOVE.L  A2,A1
    MOVE.L  4(A2),D0
    MOVE.L  (A2),A2
    JSR -210(A6)
    MOVE.L  A2,D0
    BNE.S   .4
.3:
    MOVE.L  -44(A4),A1
    JSR -414(A6)        ; closedos
CLDOS:  MOVE.L  -52(A4),A1
    JSR -414(A6)        ; closegfx
CLGFX:  MOVE.L  -48(A4),A1
    JSR -414(A6)        ; closeintui
CLINT:  MOVE.L  -36(A4),D3
    BEQ.S   .1
    JSR -132(A6)        ; no unloadseg until we're done
    MOVE.L  D3,A1
    JSR -378(A6)        ; reply WB message
.1: MOVE.L  -28(A4),D2      ; EXITCODE
    UNLK    A4
ENDC2:
    ADD.L   #1,A7
    MOVE.L  #$20000,D0
    MOVE.L  (A7)+,A1
    EXG.L   A1,A7
    SUBA.L  D0,A1
    JSR -210(A6)
ST2:
    MOVE.L  D2,D0
    RTS
ENDC:
;*-*
;; library
; sample.library

VERSION         = 37
REVISION        = 1

LIBB_DELEXP     = 3
LIB_NEGSIZE     = $10
LIB_POSSIZE     = $12
LIB_OPENCNT     = $20

TASKTABSIZE     = 16
NUMTASKTAB      = 8
NUMTASKS        = TASKTABSIZE*NUMTASKTAB
TASKTABSTOREMASK    = %1110000000 ; 16*8BYTES=128=7BIT CLEAR

sb_Flags        = 34    ; LIB_SIZE
sb_pad          = 35
sb_SysLib       = 36
sb_DosLib       = 40
sb_SegList      = 44
sb_TASKS        = 48            ; TASKID+A4
SampleBase_SIZEOF   = NUMTASKS*8+sb_TASKS

Remove          = -$FC
FreeMem         = -$D2

PoolSize        = 256*10+4  ; 256 places for [task:LONG,a4:LONG,open:INT]+4bytes for link

Start:  MOVEQ   #-1,d0
    rts

RomTag: DC.W    $4AFC       ; UWORD RT_MATCHWORD
    DC.L    RomTag-Start    ; APTR  RT_MATCHTAG [R]
    DC.L    EndCode-Start   ; APTR  RT_ENDSKIP [R]
    DC.B    $80     ; UBYTE RT_FLAGS
    DC.B    VERSION     ; UBYTE RT_VERSION
    DC.B    9       ; UBYTE RT_TYPE
    DC.B    0       ; BYTE  RT_PRI
    DC.L    LibName-Start   ; APTR  RT_NAME [R]
    DC.L    IDString-Start  ; APTR  RT_IDSTRING [R]
    DC.L    InitTable-Start ; APTR  RT_INIT  table for InitResident()

InitTable:
    DC.L    34+10;SampleBase_SIZEOF   ; size of library base data space
    DC.L    0               ; pointer to function initializers
    DC.L    dataTable-Start     ; pointer to data initializers
    DC.L    initRoutine-Start   ; routine to run

dataTable:
    DC.W    -$5FF8
    DC.B    $9,0
    DC.W    -$7FF6
    DC.L    LibName-Start
    DC.W    -$5FF2,$600,-$6FEC,VERSION,-$6FEA,REVISION,-$7FE8
    DC.L    IDString-Start,0

;; init
initRoutine:
    movem.l a1/a5,-(sp)
    move.l  d0,a5
PatchLib1:
    LEA     34(A5),A1
    bset    #0,9(a1)
    MOVE.L  A0,(A1)         ; seg list
    CLR.L   4(A1)        ; task list
    move.l  a5,d0
    movem.l (sp)+,a1/a5
    rts
;*-*
;; open
Open:
    addq.w  #1,LIB_OPENCNT(A6)  ; ( libptr:a6, version:d0 )
    BSR.W   GETTC
    CMPA.L  #0,A0
    BNE     .EXIT
    BSR     NEWTC
    BRA     .OKAY
.FAIL:
    SUBQ.W  #1,LIB_OPENCNT(A6)  ;
    MOVEQ   #0,D0               ; AAARGH!!! ERROR!!
    RTS
.OKAY:
    MOVEM.L D2-D7/A0/A2-A6,-(A7)
    MOVE.L  A0,A5               ; AO POS BEWAREN OM A4 ..
    BSR LFIRSTSTART
    MOVEM.L (A7)+,D2-D7/A0/A2-A6
    TST.L   D0
    BEQ.S   .FAIL
.EXIT:
    ADD.W   #1,4(A0)
    move.l  a6,d0
    rts
;*-*
;; close
Close:
    BSR     GETTC
    CMPA.L  #0,A0
    BEQ     .1
    SUB.W   #1,4(A0)
    MOVEQ   #0,D0
    TST.W   4(A0)
    BNE     .1

    MOVEM.L D2-D7/A2-A6,-(A7)
    MOVE.L  (A0),A4
    CLR.L   (A0)
    CLR.L   -(A0)               ; change is multitask safe
    BSR LEXITC
    MOVEM.L (A7)+,D2-D7/A2-A6
    moveq   #0,d0               ; ( libptr:a6 )
    subq.w  #1,LIB_OPENCNT(a6)
    bne.s   .1
    bsr Expung
.1: rts
;*-*
;; Expunge
Expung:
    movem.l d2/d7/a4/a5/a6,-(sp)          ; ( libptr: a6 )
PatchLib2:
    LEA     34(a6),a4
    move.l  a6,a5
    tst.w   LIB_OPENCNT(a5)
    bne.s   .00                         ; not closed yet
    btst    #0,9(a4)                    ; avoid self-expunge
    beq.s   .1
    bclr    #0,9(a4)
.00:moveq   #0,d0                       ;
    bra.s   ExpEnd                      ;
.1: move.l  (a4),d7                     ; seg list
    MOVE.L  $4,A6
    move.l  4(a4),a4
.L1:MOVE.L  A4,a1
    move.l  (a1),a4
    move.l  #PoolSize,D0
    JSR     -210(a6)                    ; free mem
    cmpa.l  #0,a4
    bne     .L1

    move.l  a5,a1                       ; library
    jsr Remove(A6)                      ; remove
    moveq   #0,d0                       ;
    move.l  a5,a1                       ;
    move.w  LIB_NEGSIZE(a5),d0
    sub.l   d0,a1
    add.w   LIB_POSSIZE(a5),d0
    jsr FreeMem(A6)                     ; free library extra fields
    move.l   d7,d0
ExpEnd:
    movem.l (sp)+,d2/d7/a4/a5/a6        ; bye
    rts
;*-*

Null:   moveq   #0,d0
    rts


;; GETTC
GETTC:
    LEA     38(a6),a0
    move.l  (a0),a0
    MOVE.L  4.W,A1              ; COPY IN GETA4!!!!!!!!
    MOVE.L  276(A1),d0          ; A1=THISTASK
.L1:
    cmpa.l  #0,a0
    BEQ     .LX
    move.l  a0,a1               ; current
    move.l  (a1),a0             ; next
    addq.l  #8,a1
    move.l  #255,d1
.L2:
    cmp.l   -4(a1),d0
    beq     .X
    lea     10(a1),a1
    dbf     d1,.L2
    BRA     .L1
.LX:rts
.X: MOVE.L  A1,A0
    RTS
;*-*
;; NEWTC
NEWTC:
    LEA 38(a6),a0
    move.l  (a0),a0
    move.l  $4.W,A1
    MOVE.L  276(a1),d0
.L1:
    cmpa.l  #$0,A0
    BEQ     .LX
    move.l  a0,a1               ; current
    move.l  (a1),a0             ; next
    addq.l  #8,a1
    move.l  #255,d1
.L2:
    tst.l   -4(a1)
    beq     PatchLibX
    lea     10(a1),a1
    dbf     d1,.L2
    bra     .L1
.LX:
    movem.l d0/a6,-(a7)
    move.l  $4.W,A6
    move.l  #PoolSize,D0
    move.l  #$10001,d1
    jsr     -198(a6)
    move.l  d0,a0
    movem.l (a7)+,d0/a6
PatchLib3:
    lea     38(a6),a1
    move.l  (a1),(a0)
    move.l  a0,(a1)
    addq.l  #8,a0
    move.l  d0,-4(a0)
    rts
PatchLibX:
    MOVE.L  A1,A0
    move.l  d0,-4(a0)
    RTS
;*-*

EndCode:

; same thing, only now for libraries

LFIRSTSTART:          ; a5 here = ptr to put a4 in
    MOVE.L  A7,A3           ; A3 = ORIG STACK
    MOVE.L  4.W,A6
    MOVE.L  #20000,D0       ; THIS .L IS CHANGED!
    MOVE.L  #$10000,D1
    MOVE.L  D0,D2
    JSR -198(A6)
    TST.L   D0
    BEQ.W   LNOMEM
    MOVE.L  D0,D6           ; D6=BOTTOMSTACK
    ADD.L   D2,D0
    JSR -132(A6)        ; --> FORBID
    MOVE.L  D0,A7
    ADD.L   #1,A7           ; SIZE IS PATCHED IN
LLINKC2:
    LINK    A4,#-4
LREALSTART:
    JSR -138(A6)        ; --> PERMIT
    MOVE.L  A3,-4(A4)
    MOVE.L  D6,-64(A4)
    MOVE.L  A6,-40(A4)      ; INIT VARS
    LEA LINITINTUI(PC),A1   ; OPEN LIBS
LPOKE37:
    MOVEQ   #33,D0          ; OR V37!
    JSR -552(A6)
    MOVE.L  D0,-48(A4)
    BEQ LCLINT

    LEA LINITGFX(PC),A1
    MOVEQ   #33,D0
    JSR -552(A6)
    MOVE.L  D0,-52(A4)
    BEQ LCLGFX
    LEA LINITDOS(PC),A1
    MOVEQ   #33,D0          ; WE WILL USE FUNCTIONS NOT IN V32
    JSR -552(A6)
    MOVE.L  D0,-44(A4)
    BEQ LCLDOS
    MOVE.L  A4,(A5)         ; PUT A4 IN TABLE
    BRA.S   LGLOBJUMP
LINITDOS:     DC.B    "dos.library",0
LINITINTUI:   DC.B    "intuition.library",0
LINITGFX:     DC.B    "graphics.library",0
LZEROSPOT:    DC.B    0
    EVEN
LMIDC:
LGLOBJUMP:
    JMP LGLOBJUMP
LNOMEM:
LRTS:
    RTS
LEXITC:               ; START OF CLOSE, A4=A4
    JSR LEXITC
LEXITC3:
    JSR LRTS
LEXITC2:
    MOVE.L  A7,-4(A4)
    MOVE.L  4.W,A6
    MOVE.L  -20(A4),A2      ; CLEANUP MEMORY
    MOVE.L  A2,D0
    BEQ.S   .3
.4: MOVE.L  A2,A1
    MOVE.L  4(A2),D0
    MOVE.L  (A2),A2
    JSR -210(A6)
    MOVE.L  A2,D0
    BNE.S   .4
.3: MOVE.L  -44(A4),A1
    JSR -414(A6)        ; closedos
LCLDOS: MOVE.L  -52(A4),A1
    JSR -414(A6)        ; closegfx
LCLGFX: MOVE.L  -48(A4),A1
    JSR -414(A6)        ; closeintui
LCLINT:
LENDC2:
    MOVE.L  #$20000,D0
    MOVE.L  -64(A4),A1
    MOVE.L  -4(A4),A7
    JSR -210(A6)
    MOVEQ   #0,D0           ; FAIL (OPEN ONLY)
    RTS
LENDC:

LibName:
IDString:

ENDFUNTAB:

COPYC:                ; A0=START,D0=END,A4=DEST
    SUB.L   A0,D0
    LSR.L   #1,D0
    SUBQ.L  #1,D0
.1: MOVE.W  (A0)+,(A4)+
    DBRA    D0,.1
    RTS

COPYRELOC:
    MOVE.L  A4,D0
    MOVE.L  (A0)+,(A4)+
    BSR ADDDIVRELOC
    RTS

LMAKEINITCODE:
    MOVE.L  LIBINFO,A6      ; A6=LIBINFO
    MOVE.L  12(A6),D2
    SUB.L   (A6),D2
    LEA Start(PC),A0
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    BSR.S   COPYRELOC       ; MATCHTAG
    BSR.S   COPYRELOC       ; ENDSKIP
    MOVE.B  (A0)+,(A4)+
    ADDQ.L  #1,A0
    MOVE.B  7(A6),(A4)+     ; VERSION
    MOVE.W  (A0)+,(A4)+
    BSR.S   COPYRELOC       ; LIBNAME
    BSR.S   COPYRELOC       ; IDSTRING
    ADD.L   D2,-4(A4)
    BSR.S   COPYRELOC       ; INIT
    MOVE.L  (A0)+,(A4)+
    MOVE.L  A4,FUNTABADR
    ADDQ.L  #4,A0
    CLR.L   (A4)+           ; FUN INIT
    BSR.S   COPYRELOC       ; DATA INIT
    BSR.S   COPYRELOC       ; RUN
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    BSR.S   COPYRELOC       ; LIBNAME
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    ADDQ.L  #2,A0
    MOVE.W  6(A6),(A4)+     ; VERSION
    MOVE.W  (A0)+,(A4)+
    ADDQ.L  #2,A0
    MOVE.W  10(A6),(A4)+        ; REVISION
    MOVE.W  (A0)+,(A4)+
    BSR.S   COPYRELOC       ; IDSTRING
    ADDQ.L  #4,A0
    CLR.L   (A4)+
    ADD.L   D2,-4(A4)
    MOVE.L  #EndCode,D0
    BSR.S   COPYC
    LEA LFIRSTSTART(PC),A0  ; NEW STACK
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  A4,STACKPOKE
    ADDQ.L  #4,A4
    ADDQ.L  #4,A0
    MOVE.L  #LLINKC2,D0
    BSR.W   COPYC
    MOVE.L  A4,AFTERDELEGATESLEA    ; PATCH IN SIZE LATER
    MOVE.W  LLINKC2(PC),(A4)+
    MOVE.W  NRGLOB,D7
    MOVE.W  D7,(A4)+
    LEA LREALSTART(PC),A0
    MOVE.L  A4,A6           ; BACKUP
    MOVE.L  #LMIDC,D0
    BSR.W   COPYC
;    BTST    #0,CODEPREFS+2
;    BEQ.S   .NVERS
;    LEA LPOKE37-LMIDC+1(A4),A6
;    MOVE.W  OSVERSION,D0
;    MOVE.B  D0,(A6)
.NVERS: LEA LGLOBJUMP(PC),A2
    MOVE.W  (A2),(A4)+
    MOVEQ   #5,D0
    JSR ADDBRANCHRELOC
    MOVE.W  6(A2),(A4)+
    MOVE.W  LEXITC(PC),(A4)+    ; DELEGATES
    MOVEQ   #8,D0
    JSR ADDBRANCHRELOC

    MOVE.W  LEXITC3(PC),(A4)+   ; this bit links in close()
    MOVEM.L D3-D4/A1-A3,-(A7)
    LEA .CLOSE(PC),A3
    HASH    A3,D3,D4
    LEA IDENTHASH,A3
    LSL.L   #2,D3
    ADD.L   D3,A3
    ADDQ.L  #4,A3
.CL:    MOVE.L  -(A3),A3        ; A3=IDENT
    MOVE.L  A3,D4
    BEQ.S   .NF
    MOVE.L  (A3),A2
    LEA .CLOSE(PC),A1
.IL:    CMPM.B  (A2)+,(A1)+
    BNE.S   .CL
    TST.B   -1(A2)
    BNE.S   .IL
    CMPI.B  #LAB,4(A3)
    BNE.S   .CL
    MOVE.L  6(A3),D4        ; NO LABELS
    BEQ.S   .CL
    MOVE.L  D4,A2           ; A2=PROC
    BTST    #1,2(A2)
    BNE ERROR70         ; NO METHODS
    TST.W   (A2)
    BNE ERROR70         ; NO ARGS
    BSET    #0,5(A3)
    MOVE.W  10(A3),D0
    JSR ADDBRANCHRELOC
    BRA.S   .CN
.CLOSE: DC.B    "close",0
.NF:    MOVE.L  A4,D0
    BSR ADDDIVRELOC
    MOVE.L  A4,D3
    SUB.L   #LEXITC3-LRTS+2,D3
    SUB.L   ACODE,D3
    MOVE.L  D3,(A4)+
.CN:    MOVEM.L (A7)+,D3-D4/A1-A3

    LEA LEXITC2(PC),A0
    MOVE.L  #LENDC2,D0
    BSR COPYC
    LEA LENDC2(PC),A0
    MOVE.W  (A0)+,(A4)+
    MOVE.L  A4,STACKPOKE+4
    ADDQ.L  #4,A4
    ADDQ.L  #4,A0
    MOVE.L  #LENDC,D0
    BSR COPYC
    MOVE.L  LIBINFO,A6
    MOVE.L  (A6),A0
.1: MOVE.B  (A0)+,(A4)+
    BNE.S   .1
    MOVE.L  12(A6),A0
.2: MOVE.B  (A0)+,(A4)+
    BNE.S   .2
    MOVE.L  A4,D0
    BTST    #0,D0
    BEQ.S   .3
    CLR.B   (A4)+
.3: BSR FIXPROCCODE
    MOVE.L  A4,CURACODE
    RTS

FUNTABADR:    DC.L    0

funcTable:
    dc.l    Open-Start
    dc.l    Close-Start
    dc.l    Expung-Start
    dc.l    Null-Start

;; GETA4
GETA4:
    MOVEM.L D0/D1/A0/A1,-(A7)
    LEA     38(a6),a0
    move.l  (a0),a0
    MOVE.L  4.W,A1              ; COPY IN GETA4!!!!!!!!
    MOVE.L  276(A1),d0          ; A1=THISTASK
.L1:
    move.l  a0,a1               ; current
    move.l  (a1),a0             ; next
    addq.l  #8,a1
    move.l  #255,d1
.L2:
    cmp.l   -4(a1),d0
    beq     .X
    lea     10(a1),a1
    dbf     d1,.L2
    cmpa.l  #0,a0
    bne     .L1
    suba.l  a4,a4               ; not found
    rts
.X: MOVE.L  (A1),A4
    MOVEM.L (A7)+,D0/D1/A0/A1
    RTS
ENDGETA4:
;*-*

BUILDLIBFUNS:
    MOVE.L  LIBINFO,D6
    BEQ.W   .X
    MOVE.L  D6,A6           ; A6 = LIBINFO
    MOVE.L  CURACODE,A4     ; A4 = CODE
    MOVE.L  FUNTABADR(PC),A5
    MOVE.L  A4,D0
    SUB.L   ACODE,D0
    MOVE.L  D0,(A5)
    MOVE.L  A5,D0
    BSR ADDDIVRELOC
    LEA funcTable(PC),A0
    BSR COPYRELOC
    BSR COPYRELOC
    BSR COPYRELOC
    BSR COPYRELOC
    MOVE.L  A4,A2           ; A2 = LIBTAB
    MOVE.W  18(A6),D0
    LSL.W   #2,D0
    ADD.W   D0,A4
    MOVE.L  #-1,(A4)+
    MOVE.L  A4,D7           ; D7 = GETA4ADDR
    LEA GETA4(PC),A0
    MOVE.L  #ENDGETA4,D0
    BSR COPYC
    LEA 20(A6),A6       ; A6 = FUNTAB
.XL:MOVE.L  (A6)+,D0
    BEQ.W   .C
    ADD.W   #20-4,A6
    MOVE.L  D0,A5           ; A5 = IDENT
    CMP.B   #LAB,4(A5)
    BNE ERROR70
    BTST    #4,5(A5)
    BNE ERROR70
    MOVE.L  6(A5),D0
    BEQ ERROR70
    MOVE.L  D0,A3           ; A3 = PROC
    TST.L   10(A3)
    BNE ERROR70
    MOVE.L  A2,D0
    BSR ADDDIVRELOC
    MOVE.L  A4,D0
    SUB.L   ACODE,D0
    MOVE.L  D0,(A2)+
    MOVE.L  .M1(PC),(A4)+
    MOVE.W  -2(A6),D4
    BMI.S   .NS
    CMP.W   (A3),D4         ; BOTH LIBFUN DECLS NARGS
    BNE ERROR70
.NS:    MOVE.W  (A3),D4
    BEQ.W   .R
    CMP.W   #13,D4
    BPL ERROR70
    TST.W   -4(A6)
    BMI.S   .NS2

    MOVEM.L D0/D1/A0,-(A7)
    LEA -16(A6),A0
    MOVE.W  -2(A6),D0
    SUBQ.W  #1,D0
    MOVEQ   #0,D1
.RML:   MOVE.B  (A0)+,D1
    MOVE.W  .MV1(PC),(A4)
    OR.W    D1,(A4)+
    DBRA    D0,.RML
    MOVEM.L (A7)+,D0/D1/A0

    BRA.S   .R
.NS2:   CMP.W   #1,D4
    BEQ.W   .ONE
    SUBQ.W  #2,D4
    LSL.W   #1,D4
    MOVE.W  .M1(PC),(A4)+
    MOVE.W  .TAB(PC,D4.W),(A4)+
.R: MOVE.W  .BSR(PC),(A4)+      ; 2XENTRY
    MOVE.L  D7,D0
    SUB.L   A4,D0
    MOVE.W  D0,(A4)+
    MOVE.W  10(A5),D0
    MOVE.W  .JSR(PC),NEWOP
    MOVE.L  .BSR(PC),(A4)+
    JSR ADDBRANCH
    MOVE.W  (A3),D4
    BEQ.S   .R2
    CMP.W   #1,D4
    BEQ.S   .ONEA
    CMP.W   #2,D4
    BEQ.S   .TWOA
    LSL.W   #2,D4
    MOVE.W  .LEA(PC),(A4)+
    MOVE.W  D4,(A4)+
.R2:    MOVE.L  .M2(PC),(A4)+
    MOVE.W  .RTS(PC),(A4)+
    BRA.W   .XL
.C: MOVE.L  A4,CURACODE
.X: RTS
.TAB:   DC.W    %1000000010000000   ; 2
    DC.W    %1100000010000000   ; 3
    DC.W    %1100000011000000   ; 4
    DC.W    %1110000011000000   ; 5
    DC.W    %1110000011100000   ; 6
    DC.W    %1111000011100000   ; 7
    DC.W    %1111000011110000   ; 8
    DC.W    %1111100011110000   ; 9
    DC.W    %1111110011110000   ; 10
    DC.W    %1111111011110000   ; 11
    DC.W    %1111111111110000   ; 12
.M1:    MOVEM.L D2/A2-A6,-(A7)
.M2:    MOVEM.L (A7)+,D2/A2-A6
.MV1:   MOVE.L  D0,-(A7)
.RTS:   RTS
.BSR:   BSR .X
.JSR:   JSR .X
.LEA:   LEA 4(A7),A7
.ADD1:  ADDQ.L  #4,A7
.ADD2:  ADDQ.L  #8,A7
.ONE:   MOVE.W  .MV1(PC),(A4)+
    BRA.W   .R
.ONEA:  MOVE.W  .ADD1(PC),(A4)+
    BRA.S   .R2
.TWOA:  MOVE.W  .ADD2(PC),(A4)+
    BRA.S   .R2

WRITELIBMOD:
    MOVE.L  LIBINFO,D0
    BEQ.W   .X
    MOVE.L  D0,A5           ; A5=LIBINFO
    MOVE.L  (A5),D1         ; D1/A2=LIBNAME
    MOVE.L  D1,A2
    MOVE.L  D1,A3           ; A3=DOTLOCATION
.XL:TST.B   (A3)
    BEQ.S   .F
    CMP.B   #".",(A3)+
    BNE.S   .XL
.F: MOVE.B  #"m",(A3)+
    CLR.B   (A3)+
    MOVE.L  D1,D0
    JSR FLUSHMODULE
    MOVE.L  #1006,D2
    MOVE.L  DOSBASE,A6
    JSR -30(A6)
    MOVE.L  D0,HANDLE
    BEQ ERROR17
    CLR.B   1(A3)
    MOVE.B  #"e",(A3)
    MOVE.B  #"s",-(A3)
    MOVE.B  #"a",-(A3)
    MOVE.B  #"b",-(A3)
    MOVE.L  #ESTACKBUF,A4       ; A4=BUF,D4=BEGIN
    MOVE.L  A4,D4
    MOVE.L  #"EMOD",(A4)+
    MOVE.W  #6,(A4)+
    CLR.B   (A4)+
.BL:    MOVE.B  (A2)+,(A4)+
    BNE.S   .BL
    LEA 20(A5),A5
.NL:    MOVE.L  (A5)+,D0
    BEQ.W   .FL
    ADD.W   #20-4,A5
    MOVE.L  D0,A3           ; A3=IDENT
    MOVE.L  (A3),A2
    MOVE.B  (A2)+,D0
    CMP.B   #"a",D0
    BPL.W   .UP
.UPB:   MOVE.B  D0,(A4)+
.CL:    MOVE.B  (A2)+,(A4)+
    BNE.S   .CL
    SUBQ.L  #1,A4

    MOVE.L  6(A3),A3        ; A3=PROC
    MOVE.W  (A3),D0
    BEQ.W   .NRG

    TST.W   -2(A5)
    BMI.S   .NRA
    LEA -16(A5),A3
    SUBQ.L  #1,D0
.CRA:   MOVE.B  (A3)+,(A4)+
    DBRA    D0,.CRA
    BRA.S   .NRGB

.NRA:   MOVE.W  D0,D1
    MOVEQ   #0,D2
    LSR.W   #1,D1
    TST.W   D1
    BEQ.S   .DR
    CMP.W   #5,D1
    BMI.S   .AR
    MOVEQ   #4,D1
.AR:    MOVE.W  D1,D2
    SUBQ.W  #1,D1
.ARL:   MOVE.B  D1,D3
    ADD.B   #8,D3
    MOVE.B  D3,(A4)+
    DBRA    D1,.ARL
.DR:    SUB.W   D2,D0
    SUBQ.W  #1,D0
.DRL:   MOVE.B  D0,(A4)+
    DBRA    D0,.DRL

.NRGB:  CHESTB  A4,D0,3,ERROR37
    BRA.W   .NL

.FL:    MOVE.B  #-1,(A4)+
    MOVE.L  A4,D3
    MOVE.L  D4,D2
    SUB.L   D2,D3
    JSR WRITEFILE
    MOVE.L  DOSBASE,A6
    MOVE.L  HANDLE,D1
    JSR -36(A6)
.X: RTS
.UP:    CMP.B   #"z"+1,D0
    BPL.W   .UPB
    SUB.B   #32,D0
    BRA.W   .UPB
.NRG:   MOVE.B  #16,(A4)+
    BRA.S   .NRGB
;*-*
;; Nostartup
; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   and again mit kein startup routines
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;

NSMAKEINITCODE:
    MOVE.W  #3,CURSPOT
    MOVE.L  CURACODE,A4
    BTST    #3,CODEPREFS+3
    BNE .EX
    TSTMOD
    BNE .EX
    LEA NSFIRSTSTART(PC),A0   ; NEW STACK
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.W  (A0)+,(A4)+
    MOVE.L  A4,STACKPOKE
    ADDQ.L  #4,A4
    ADDQ.L  #4,A0
    MOVE.L  #NSLINKC2,D0
    BSR COPYC
    MOVE.L  A4,AFTERDELEGATESLEA    ; PATCH IN SIZE LATER
    MOVE.W  NSLINKC2(PC),(A4)+
    MOVE.W  NRGLOB,D7
    MOVE.W  D7,(A4)+
    LEA NSREALSTART(PC),A0
    MOVE.L  A4,A6           ; BACKUP
    MOVE.L  #NSMIDC,D0
    BSR COPYC
    MOVE.W  NSGLOBJUMP(PC),(A4)+
    MOVEQ   #5,D0
    JSR ADDBRANCHRELOC
    MOVE.W  NSEXITC(PC),(A4)+     ; DELEGATES
    MOVEQ   #8,D0
    JSR ADDBRANCHRELOC
    LEA NSEXITC2(PC),A0
    MOVE.L  #NSENDC2,D0
    BSR COPYC
    LEA NSENDC2(PC),A0
    MOVE.L  A4,BEFORESECONDLEA
    MOVE.L  (A0)+,(A4)+
    MOVE.L  (A0)+,(A4)+
    MOVE.L  A4,STACKPOKE+4
    ADDQ.L  #4,A4
    ADDQ.L  #4,A0
    MOVE.L  #NSST2,D0
    BSR COPYC
    MOVE.L  NSST2,(A4)+
    BSR FIXPROCCODE
    MOVE.L  A4,CURACODE
.EX:    RTS

NSFIRSTSTART:         ; CHANGES ALSO TO LFIRSTART!!!
    MOVE.L  4.W,A6
    MOVE.L  A0,A5           ; SAVE A0/D0 TO A5/D7
    MOVE.L  D0,D7
    MOVE.L  #20000,D0       ; THIS .L IS CHANGED!
    MOVE.L  #$10000,D1
    MOVE.L  D0,D2
    JSR     -198(A6)
    TST.L   D0
    BNE.S   NOST_1
    MOVEQ   #20,D0          ; NO FUCKIN' MEM !
    RTS
A4STORAGE_NOST:
    DC.L    0
NOST_1:
    MOVE.L  D0,D6           ; D6=BOTTOMSTACK
    ADD.L   D2,D0
    JSR -132(A6)        ; --> FORBID
    EXG.L   D0,A7
    MOVE.L  D0,-(A7)
    ADD.L   #1,A7           ; SIZE IS PATCHED IN
NSLINKC2:
    LINK    A4,#-4
NSREALSTART:
    JSR -138(A6)        ; --> PERMIT
    MOVE.L  D6,-64(A4)

    MOVE.L  A6,-40(a4)
    MOVE.L  A7,-4(A4)
    LEA     A4STORAGE_NOST(PC),A1
    MOVE.L  A4,(A1)

NSMIDC:
NSGLOBJUMP:
    JSR NSGLOBJUMP
NSEXITC:
    JSR NSEXITC
NSEXITC2:
    MOVE.L  -4(A4),A7
    MOVE.L  4.W,A6
    MOVE.L  -20(A4),A2      ; CLEANUP MEMORY
    MOVE.L  A2,D0
    BEQ.S   .3
.4: MOVE.L  A2,A1
    MOVE.L  4(A2),D0
    MOVE.L  (A2),A2
    JSR -210(A6)
    MOVE.L  A2,D0
    BNE.S   .4
.3:
    MOVE.L  -28(A4),D2      ; EXITCODE
    UNLK    A4
NSENDC2:
    ADD.L   #1,A7
    MOVE.L  #$20000,D0
    MOVE.L  (A7)+,A1
    EXG.L   A1,A7
    SUBA.L  D0,A1
    JSR -210(A6)
NSST2:
    MOVE.L  D2,D0
    RTS
NSENDC:
;*-*
;; Other!

OTHEREXITCODE:
    MOVE.W  #11,CURSPOT
    BSR BUILDDELEGATECONSTRUCTORS
;    BSR FIXSTARTUPCODE
    RTS


OTHERINITCODE:
    MOVEM.L D0-A6,-(A7)
    LEA STCNAME,A0
    LEA LIBLIST+4,A1
.L1:
    MOVe.L  A0,A3
    MOVE.L  LIB_NEXT(A1),D0
    BEQ ERROR42
    MOVE.L  D0,A1
    CMP.W   #2,LIB_TYPE(A1)
    BNE .L1
    MOVE.L  LIB_NAME(A1),A2
.L2:
    CMP.B   (A2)+,(A3)+
    BNE .L1
    TST.B   -1(A3)
    BNE .L2

    MOVE.L  LIB_CODE(A1),STARTUP_CODE
    MOVE.L  LIB_CLEN(A1),STARTUP_SIZE
    MOVE.L  LIB_XTNS(A1),STARTUP_XTNS
    MOVE.L  A1,STARTUP_NODE
    MOVEM.L (A7)+,D0-A6

    MOVE.L  CURACODE,A4
    MOVE.L  A4,A6
    MOVEM.L D0-A3,-(A7)
    BTST    #4,ICODEPREFS+3
    BEQ     .z
    BSR BGCODE
.z:
    MOVEM.L (A7)+,D0-A3
    MOVE.L  A4,CURACODE

    MOVE.L  STARTUP_CODE,A0
    MOVE.L  STARTUP_SIZE,D0
    LSR.W   #1,D0
    SUBQ.L  #1,D0
.LOOP:
    MOVE.W  (A0)+,(A4)+
    DBF D0,.LOOP

    BSR FIXSTARTUPCODE2

    BSR FIXPROCCODE

    MOVEM.L  A0/A6,-(A7)
    MOVE.L  CURACODE,A6
    MOVE.L  STARTUP_NODE,A0
    BSR FIXRELOC
    MOVEM.L (A7)+,A0/A6

    MOVE.L  A4,CURACODE
    RTS

    IFEQ 1
;; FIXSTARTUPCODE
FIXSTARTUPCODE:
    MOVEM.L D0-A6,-(A7)
    MOVE.L  CURACODE,A0
    MOVE.L  STARTUP_XTNS,A1
.X_LOOP:
    MOVE.L  (A1)+,D0
    BNE.S   .X_CONT
    MOVEM.L (A7)+,D0-A6
    RTS
.X_CONT:
    MOVE.L  D0,D1
    ROL.L   #8,D1
    AND.L   #255,D1
    AND.L   #$FFFFFF,D0

    MOVE.L  A1,A5
    LOWER2 A1,D2
    MOVE.L  A5,A1

    CMP.W   #$81,D1
    BEQ     .x81
    CMP.W   #$83,D1
    BEQ     .x83

    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D0
    CMP.W   #$80,D1
    BMI .X_LOOP
    LSL.L   #2,D0
    ADD.L   D0,A0
    BRA     .X_LOOP
;; Check
.CHECKNAME:
    MOVE.L  A1,-(A7)
    MOVEQ   #0,D2
.CHECKTST:
    CMPM.B  (A1)+,(A2)+
    BNE .CHECKF
    TST.B   -1(A1)
    BNE .CHECKTST
    MOVEQ   #-1,D2
.CHECKF:
    MOVE.L  (A7)+,A1
    RTS
;*-*
;; 0x81
.x81:
    LEA ._SETUP(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_SETUP

    LEA ._DELMEM(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_DELMEM

    LEA ._STKSIZE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_STKSIZE

    MOVEQ   #0,D1
    MOVE.W  OSVERSION,D1
    LEA .OSVERSION(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_FILL

    MOVE.W  ASMCPU,D1
    LEA .CPUFLAGS(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_FILL

    LEA ._CLEANUP(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_CLEANUP

    CLR.W   ERRWHERE
    BRA ERROR42

;; names
._SETUP:
    DC.B    '_setup',0
._CLEANUP:
    DC.B    '_cleanup',0
._DELMEM:
    DC.B    '_delmem',0
._STKSIZE:
    DC.B    '_stksize',0
    EVEN
;*-*
;; subcodes
;; Fill
.x81_FILL:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
.x81_FILL_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVE.L  D1,(A4)
    DBF D2,.x81_FILL_LOOP
    BRA .X_LOOP
;*-*
;; SETUP
.x81_SETUP:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
.x81_SETUP_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVEQ   #5,D0
    MOVEm.L  D1/A0,-(A7)
    JSR ADDBRANCHRELOC
    MOVEm.L  (a7)+,d1/a0
    DBF D2,.x81_SETUP_LOOP
    BRA .X_LOOP
;*-*
;; CLEANUP
.x81_CLEANUP:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
    BSET    #6,ICODEPREFS+3
.x81_CLEANUP_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVEQ   #8,D0
    MOVEm.L  D1/A0,-(A7)
    JSR ADDBRANCHRELOC
    MOVEm.L  (a7)+,d1/a0
    DBF D2,.x81_CLEANUP_LOOP
    BRA .X_LOOP
;*-*
;; DELMEM
.x81_DELMEM:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
.x81_DELMEM_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVE.L  TOTALDELSIZE(PC),(A4)
    DBF D2,.x81_DELMEM_LOOP
    BRA .X_LOOP
;*-*
;; STKSIZE
.x81_STKSIZE:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
    MOVE.L  TOTALDELSIZE(PC),D1
    ADD.L   MINSTACK,D1
    ADD.L   GLOBSTACK,D1
    BSR .x81_STKSIZE_CHECK
.x81_STKSIZE_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVE.L  D1,(A4)
    DBF D2,.x81_STKSIZE_LOOP
    BRA .X_LOOP

.x81_STKSIZE_CHECK:
    MOVE.L  CODESTACK,D3
    BEQ     .x81_STKSIZE_EXIT2
    CMP.L   D1,D3
    BPL.S   .x81_STKSIZE_CHECK2
    BSET    #2,WARNINGS+3
.x81_STKSIZE_CHECK2:
    ADD.L   #4000,D1
    CMP.L   D1,D3
    BPL.S   .x81_STKSIZE_EXIT
    BSET    #1,WARNINGS+3
    BCLR    #2,WARNINGS+3
.x81_STKSIZE_EXIT:
    RTS
.x81_STKSIZE_EXIT2:
    ADD.L   #MINSTACKSIZE,D1
    RTS
;*-*
;*-*
;*-*
;; 0x83
.x83:
    MOVE.W  NRGLOB,D1
    LEA ._STKFRAME(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-44,D1
    LEA .DBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-48,D1
    LEA .IBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-52,d1
    LEA .GBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-8,d1
    LEA .SOUT(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-16,d1
    LEA .SRAST(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-32,d1
    LEA .ARG(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-36,d1
    LEA .WBMSG(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-56,D1
    LEA .SBBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-60,D1
    LEA .STBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-96,D1
    LEA .XCPI(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-92,D1
    LEA .SIN(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-120,D1
    LEA .POOL(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-124,D1
    LEA .UBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-4,D1
    LEA .STKRET(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-20,D1
    LEA .MEMLIST(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-24,D1
    LEA .EXITCODE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-40,D1
    LEA .EBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-28,D1
    LEA .RETVAL(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-64,D1
    LEA .BTMSTK(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-128,D1
    LEA .FILELIST(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #0,D1
    MOVE.W  OSVERSION,D1
    LEA .OSVERSION(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.W  ASMCPU,D1
    LEA .CPUFLAGS(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    CLR.W   ERRWHERE
    BRA ERROR42
;; names
.STKRET:
    DC.B    '_stkreturn',0
.MEMLIST:
    Dc.b    '_memlist',0
.EXITCODE:
    DC.B    '_exitcode',0
.BTMSTK:
    dc.b    '_bottomstk',0
.FILELIST:
    DC.B    '_filelist',0
.RETVAL:
    DC.B    '_clireturnvalue',0
._STKFRAME:
    DC.B    '_stkframe',0
.DBASE:
    DC.B    '_dosbase',0
.IBASE:
    DC.B    '_intuitionbase',0
.GBASE:
    DC.B    '_gfxbase',0
.EBASE:
    DC.B    '_execbase',0
.SOUT:
    DC.B    '_stdout',0
.SRAST:
    DC.B    '_stdrast',0
.ARG:
    DC.B    '_arg',0
.WBMSG:
    DC.B    '_wbmessage',0
.SBBASE:
    DC.B    '_mathieeesingbasbase',0
.STBASE:
    DC.B    '_mathieeesingtransbase',0
.XCP:
    DC.B    '_exception',0
.XCPI:
    Dc.B    '_exceptioninfo',0
.SIN:
    DC.B    '_stdin',0
.UBASE:
    DC.B    '_utilitybase',0
.POOL:
    DC.B    '_pool',0
.OSVERSION:
    DC.B    '_osversion',0
.CPUFLAGS:
    DC.B    '_cpuflags',0
    EVEN
;*-*
;; subcode(s)
;; Fill
.x83_FILL:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
.x83_FILL_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVE.W  D1,(A4)
    DBF D2,.x83_FILL_LOOP
    BRA .X_LOOP
;*-*
;*-*
;*-*
;*-*
    ENDC
;; FIXSTARTUPCODE2
FIXSTARTUPCODE2:
    MOVEM.L D0-A6,-(A7)
    MOVE.L  CURACODE,A0
    MOVE.L  STARTUP_XTNS,A1
.X_LOOP:
    MOVE.L  (A1)+,D0
    BNE.S   .X_CONT
    MOVEM.L (A7)+,D0-A6
    RTS
.X_CONT:
    MOVE.L  D0,D1
    ROL.L   #8,D1
    AND.L   #255,D1
    AND.L   #$FFFFFF,D0

    MOVE.L  A1,A5
    LOWER2  A1,D2
    MOVE.L  A5,A1

    BTST    #7,D1
    BEQ     .x01
    CMP.W   #$81,D1
    BEQ     .x81
    CMP.W   #$83,D1
    BEQ     .x83

    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D0
    LSL.L   #2,D0
    ADD.L   D0,A1
    BRA     .X_LOOP
;; Check
.CHECKNAME:
    MOVE.L  A1,-(A7)
    MOVEQ   #0,D2
.CHECKTST:
    CMPM.B  (A1)+,(A2)+
    BNE .CHECKF
    TST.B   -1(A1)
    BNE .CHECKTST
    MOVEQ   #-1,D2
.CHECKF:
    MOVE.L  (A7)+,A1
    RTS
;*-*
;; x01
.x01:
    LEA ._A4STORAGE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x01_A4STORAGE

    CLR.W   ERRWHERE
    BRA ERROR42
;; names
._A4STORAGE:
    DC.B    '_a4storage',0
    EVEN
;*-*
;; subcodes
;; A4STORAGE
.x01_A4STORAGE:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,A4STORAGEADR
    BRA .X_LOOP
;*-*
;*-*
;*-*
;; x81
.x81:
    LEA ._SETUP(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_SETUP

    LEA ._DELMEM(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_DELMEM

    LEA ._STKSIZE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_STKSIZE

    MOVEQ   #0,D1
    MOVE.W  OSVERSION,D1
    LEA .OSVERSION(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_FILL

    MOVE.W  ASMCPU,D1
    LEA .CPUFLAGS(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_FILL

    LEA ._CLEANUP(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x81_CLEANUP

    CLR.W   ERRWHERE
    BRA ERROR42

;; names
._SETUP:
    DC.B    '_setup',0
._CLEANUP:
    DC.B    '_cleanup',0
._DELMEM:
    DC.B    '_delmem',0
._STKSIZE:
    DC.B    '_stksize',0
    EVEN
;*-*
;; subcodes
;; Fill
.x81_FILL:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
.x81_FILL_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVE.L  D1,(A4)
    DBF D2,.x81_FILL_LOOP
    BRA .X_LOOP
;*-*
;; SETUP
.x81_SETUP:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
.x81_SETUP_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVEQ   #5,D0
    MOVEm.L  D1/A0,-(A7)
    JSR ADDBRANCHRELOC
    MOVEm.L  (a7)+,d1/a0
    DBF D2,.x81_SETUP_LOOP
    BRA .X_LOOP
;*-*
;; CLEANUP
.x81_CLEANUP:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
    BSET    #6,ICODEPREFS+3
.x81_CLEANUP_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVEQ   #8,D0
    MOVEm.L  D1/A0,-(A7)
    JSR ADDBRANCHRELOC
    MOVEm.L  (a7)+,d1/a0
    DBF D2,.x81_CLEANUP_LOOP
    BRA .X_LOOP
;*-*
;; DELMEM
.x81_DELMEM:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
.x81_DELMEM_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVE.L  TOTALDELSIZE(PC),(A4)
    DBF D2,.x81_DELMEM_LOOP
    BRA .X_LOOP
;*-*
;; STKSIZE
.x81_STKSIZE:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
    MOVE.L  TOTALDELSIZE(PC),D1
    ADD.L   MINSTACK,D1
    ADD.L   GLOBSTACK,D1
    BSR .x81_STKSIZE_CHECK
.x81_STKSIZE_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVE.L  D1,(A4)
    DBF D2,.x81_STKSIZE_LOOP
    BRA .X_LOOP

.x81_STKSIZE_CHECK:
    MOVE.L  CODESTACK,D3
    BEQ     .x81_STKSIZE_EXIT2
    CMP.L   D1,D3
    BPL.S   .x81_STKSIZE_CHECK2
    BSET    #2,WARNINGS+3
.x81_STKSIZE_CHECK2:
    ADD.L   #4000,D1
    CMP.L   D1,D3
    BPL.S   .x81_STKSIZE_EXIT
    BSET    #1,WARNINGS+3
    BCLR    #2,WARNINGS+3
.x81_STKSIZE_EXIT:
    RTS
.x81_STKSIZE_EXIT2:
    ADD.L   #MINSTACKSIZE,D1
    RTS
;*-*
;*-*
;*-*
;; x83
.x83:
    MOVE.W  NRGLOB,D1
    LEA ._STKFRAME(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-44,D1
    LEA .DBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-48,D1
    LEA .IBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-52,d1
    LEA .GBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-8,d1
    LEA .SOUT(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-16,d1
    LEA .SRAST(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-32,d1
    LEA .ARG(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-36,d1
    LEA .WBMSG(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-56,D1
    LEA .SBBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #-60,D1
    LEA .STBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-96,D1
    LEA .XCPI(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-92,D1
    LEA .SIN(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-120,D1
    LEA .POOL(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-124,D1
    LEA .UBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-4,D1
    LEA .STKRET(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-20,D1
    LEA .MEMLIST(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-24,D1
    LEA .EXITCODE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-40,D1
    LEA .EBASE(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-28,D1
    LEA .RETVAL(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-64,D1
    LEA .BTMSTK(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.L  #-128,D1
    LEA .FILELIST(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVEQ   #0,D1
    MOVE.W  OSVERSION,D1
    LEA .OSVERSION(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    MOVE.W  ASMCPU,D1
    LEA .CPUFLAGS(PC),A2
    BSR .CHECKNAME
    TST.L   D2
    BNE .x83_FILL

    CLR.W   ERRWHERE
    BRA ERROR42
;; names
.STKRET:
    DC.B    '_stkreturn',0
.MEMLIST:
    Dc.b    '_memlist',0
.EXITCODE:
    DC.B    '_exitcode',0
.BTMSTK:
    dc.b    '_bottomstk',0
.FILELIST:
    DC.B    '_filelist',0
.RETVAL:
    DC.B    '_clireturnvalue',0
._STKFRAME:
    DC.B    '_stkframe',0
.DBASE:
    DC.B    '_dosbase',0
.IBASE:
    DC.B    '_intuitionbase',0
.GBASE:
    DC.B    '_gfxbase',0
.EBASE:
    DC.B    '_execbase',0
.SOUT:
    DC.B    '_stdout',0
.SRAST:
    DC.B    '_stdrast',0
.ARG:
    DC.B    '_arg',0
.WBMSG:
    DC.B    '_wbmessage',0
.SBBASE:
    DC.B    '_mathieeesingbasbase',0
.STBASE:
    DC.B    '_mathieeesingtransbase',0
.XCP:
    DC.B    '_exception',0
.XCPI:
    Dc.B    '_exceptioninfo',0
.SIN:
    DC.B    '_stdin',0
.UBASE:
    DC.B    '_utilitybase',0
.POOL:
    DC.B    '_pool',0
.OSVERSION:
    DC.B    '_osversion',0
.CPUFLAGS:
    DC.B    '_cpuflags',0
    EVEN
;*-*
;; subcode(s)
;; Fill
.x83_FILL:
    LSL.L   #2,D0
    ADD.L   D0,A1
    MOVE.L  (A1)+,D2
    SUBQ.L  #1,D2
.x83_FILL_LOOP:
    MOVE.L  A0,A4
    ADD.L   (A1)+,A4
    MOVE.W  D1,(A4)
    DBF D2,.x83_FILL_LOOP
    BRA .X_LOOP
;*-*
;*-*
;*-*
;*-*
;*-*

;; background
BGCODE:
    bra     dobg
BGS:
    MOVE.L  $4.W,A6
    move.l  a0,a5
    move.l  d0,d7
    SUB.L   A1,A1
    JSR -294(A6)
    MOVE.L  D0,A1
    TST.L   $AC(A1)
    BNE.S   BGRUNCODE
BGFILL:
    DC.W    $4EF9,0,0
BGRUNCODE
    LEA BGS(PC),A1          ; start of next hunk
    MOVE.L  -(A1),D3            ; pick and
    CLR.L   (A1)                ; clear
    LEA     $17A(A6),A0         ; find dos.library
    LEA     DOSNAME(PC),A1
    JSR     -276(A6)
    move.l  d0,-(a7)            ; and store
    LEA     BGTAGS(PC),A4      ;
    MOVE.L  A4,D6
    BRA.S   BGSKIP
BGTAGS:
    DC.L    $800003e9,0    ; seglist
    DC.L    $800003fd,0    ; args
    DC.L    $800003fA,1    ; cli
    DC.L    $00000000,0    ; tag_done
BGSKIP:
    ADDQ.L  #4,A4
    MOVE.L  D3,(A4)+
    ADDQ.L  #4,A4
    MOVE.L  A5,(A4)+
    MOVE.L  D6,D1
    MOVE.L  (A7)+,A6
    JSR     -498(a6)
    moveq   #0,d0
    rts
DOSNAME:
    dc.b    'dos.library',0,0,0
    even
BGE:

dobg:
    LEA BGS(PC),A0
    MOVE.L #BGFILL,D0
    ADDQ.L #2,D0
    BSR COPYC
    MOVE.L #BGE-BGS,D0
    TST.L   bgName
    BEQ .bg1
    ADDQ.L  #8,D0
.bg1:
    TST.L   bgPri
    BEQ .bg2
    ADDQ.L  #8,D0
.bg2:
    MOVE.L  D0,D7
    MOVE.L A0,-(A7)
    JSR FORCEABSHERE
    MOVE.L (A7)+,A0
    ADDQ.L  #4,A0
    MOVE.L  #BGTAGS+24,D0
    BSR COPYC
    MOVE.L  A4,A6
    SUB.L   #25,A6

    TST.L   bgName
    BEQ     .dobg2
    MOVE.L  #$80000000+1012,(a4)+
    MOVE.L  D7,D0
    ADD.L   #ENDC-FIRSTSTART,D0
    ADDQ.B  #8,(A6)
    ADDQ.W  #8,-15(A6)
    MOVE.L  A0,-(A7)
    JSR FORCEABSHERE
    MOVE.L  (A7)+,A0
.dobg2:
    TST.L   bgPri
    BEQ     .dobg3
    MOVE.L  #$80000000+1013,(a4)+
    ADDQ.B  #8,(A6)
    ADDQ.W  #8,-15(A6)
    move.l  bgPri(PC),(A4)+
.dobg3:
    MOVE.L #BGE,D0
    BSR COPYC

    MOVE.L HunkList(PC),A0
    MOVE.L A4,H_END(A0)
    MOVE.L A0,D0
    ADDQ.L #1,NumHunks

    GETM    A0                  ; fill the 0-hunk structure!
    MOVE.L  HunkList(PC),(A0)+  ;
    MOVE.L  A0,HunkList         ;
    EXG.L   A0,D0
    MOVE.L  D0,H_PREV(A0)
    EXG.L   A0,D0
    MOVE.L  A4,(A0)+            ; start
    MOVE.L  #$3e9,(A0)+         ; type
    clr.l   (a0)+               ; end
    clr.l   (a0)+               ; prev
    clr.l   (a0)+               ; reloc
    DONEM   A0                  ; We'll use it for writing the stuff :)
    RTS
;*-*

A4STORAGEADR:
    DC.L    A4STORAGE_NOST-NSFIRSTSTART

; NEEDS TO GEN T/F IN D2
;; Additional code - init
MATHINITETC:          ; A4=CODE
    BTST    #5,CODEPREFS
    BEQ     ._1
    RTS
._1:
    BTST    #6,ICODEPREFS+3
    BNE .0
    BTST    #2,CODEPREFS
    BEQ.S   .0
    RTS
.0: MOVE.W  .T(PC),(A4)+
    BTST    #3,CODEPREFS+1
    BEQ.S   .1
    LEA OPENMATHBAS(PC),A0
    MOVE.L  #OPENBASEND,D0
    BSR CCOPY
.1: BTST    #4,CODEPREFS+1
    BEQ.S   .2
    LEA OPENMATHTRANS(PC),A0
    MOVE.L  #OPENTRANSEND,D0
    BSR CCOPY
.2: BTST    #7,CODEPREFS
    BEQ     .3
    BTST    #4,CODEPREFS
    BNE     ERROR50
    LEA OPENUTIL(PC),A0
    MOVE.L  #OPENUTILEND,D0
    BSR CCOPY
.3: BTST    #6,CODEPREFS
    BEQ     .4
    BTST    #4,CODEPREFS
    BNE     ERROR50
    LEA OPENPOOL(PC),A0
    MOVE.L  #OPENPOOLEND,D0
    BSR CCOPY
.4: BTST    #3,CODEPREFS
    BEQ     .5
    LEA OPENFILES(PC),A0
    MOVE.L  #OPENFILESEND,D0
    BSR CCOPY
.5: CMP.W   #37,OSVERSION
    BLT     .6
    BTST    #4,CODEPREFS
    BNE     .6
    LEA OPENCON37P(PC),A0
    MOVE.L  #OPENCON37PEND,D0
    BSR CCOPY
.6: TST.W   ASMCPU
    BEQ     .7
    BTST    #4,CODEPREFS
    BNE     .7
    LEA CHECK_CPU_FPU(PC),A0
    MOVE.W  ASMCPU,2(A0)
    MOVE.L  #LK0000,D0
    BSR CCOPY
    JSR NEWLABEL
    JSR ADDBRANCH
    MOVE.L D0,-(A7)
    LEA LK0000(PC),A0
    MOVE.L  #CHECK_CPU_FPU_END,D0
    BSR CCOPY
    BSR ADDERR
    MOVE.L (A7)+,D0
    JSR ADDLABEL
.7: BTST    #0,CODEPREFS+2
    BEQ     .8
    BTST    #4,CODEPREFS
    BNE     .8
    LEA CHECK_OSVERSION(PC),A0
    MOVE.W  OSVERSION,2(A0)
    MOVE.L  #LK0001,D0
    BSR CCOPY
    JSR NEWLABEL
    JSR ADDBRANCH
    MOVE.L D0,-(A7)
    LEA LK0001(PC),A0
    MOVE.L  #CHECK_OSVERSION_END,D0
    BSR CCOPY
    BSR ADDERR
    MOVE.L (A7)+,D0
    JSR ADDLABEL
.8: BTST    #0,CODEPREFS
    BEQ     .9
    LEA OPEN_POWERPC(PC),A0
    MOVE.L  #OPEN_POWERPC_END,D0
    BSR CCOPY
    BSR ADDERR
.9:
    LEA  SetupMod+4(PC),A0
.A: MOVE.L  -4(A0),D0
    BEQ     .C
    MOVE.L  D0,A0
    MOVE.W  .U(PC),(A4)+
    MOVE.L  A0,-(A7)
    MOVE.L  (A0),A0
    MOve.L  14(A0),A0
    MOVEQ   #0,D0
    MOVE.W  10(A0),D0
    JSR     ADDBRANCHRELOC
    MOVE.L  (A7)+,A0
    BRA     .A
.C:
    RTS
.T: MOVEQ   #-1,D2
.U: JSR     .U

ADDERR:
    MOVE.W  InitError(PC),D0
    BNE.S   .AddErr2
    JSR     NEWLABEL
    MOVE.W  D0,InitError
.AddErr2:
    MOVE.W  .JSR,NEWOP
.JSR:
    JSR     ADDBRANCH
    RTS
ADDERRCODE:
    MOVE.W  InitError,D0
    BEQ     .EXIT
    JSR     ADDLABEL
    LEA     InitializationError(PC),A0
    MOVe.L  #InitializationErrorEnd,D0
    BSR     CCOPY
.EXIT:
    RTS
;*-*
;; Additional code - exit
MATHEXITETC:
    TSTMOD
    BNE .EX
    BTST    #5,CODEPREFS
    BNE .EX
    BTST    #6,ICODEPREFS+3
    BNE .0
    BTST    #2,CODEPREFS
    BNE .EX
.0:
    MOVE.L  CURACODE,A4     ; A4=CODE
    MOVEQ   #8,D0
    JSR ADDLABEL

    
    LEA  CleanupMod+4(PC),A0
.8: MOVE.L  -4(A0),D0
    BEQ     .9
    MOVE.L  D0,A0
    MOVE.W  .JSR(PC),(A4)+
    MOVE.L  A0,-(A7)
    MOVE.L  (A0),A0
    MOve.L  14(A0),A0
    MOVEQ   #0,D0
    MOVE.W  10(A0),D0
    JSR     ADDBRANCHRELOC
    MOVE.L  (A7)+,A0
    BRA     .8
.9:


    BTST    #3,CODEPREFS+1
    BEQ.S   .1
    LEA CLOSEMATHBAS(PC),A0
    MOVE.L  #CLOSEBASEND,D0
    BSR CCOPY
.1: BTST    #4,CODEPREFS+1
    BEQ.S   .2
    LEA CLOSEMATHTRANS(PC),A0
    MOVE.L  #CLOSETRANSEND,D0
    BSR CCOPY
.2: BTST    #7,CODEPREFS
    BEQ     .3
    BTST    #4,CODEPREFS
    BNE     ERROR50
    LEA CLOSEUTIL(PC),A0
    MOVE.L  #CLOSEUTILEND,D0
    BSR CCOPY
.3: BTST    #6,CODEPREFS
    BEQ     .4
    BTST    #4,CODEPREFS
    BNE     ERROR50
    LEA CLOSEPOOL(PC),A0
    MOVE.L  #CLOSEPOOLEND,D0
    BSR CCOPY
.4: BTST    #3,CODEPREFS
    BEQ     .5
    LEA CLOSEFILES(PC),A0
    MOVE.L  #CLOSEFILESEND,D0
    BSR CCOPY
.5: CMP.W   #37,OSVERSION
    BLT     .5_5
    LEA CLOSECON37P(PC),A0
    MOVE.L  #CLOSECON37PEND,D0
    BSR CCOPY
    BRA .6
.5_5:
    LEA CLOSECON33_37(PC),A0
    MOVE.L  #CLOSECON33_37END,D0
    BSR CCOPY
.6: BTST    #0,CODEPREFS
    BEQ     .7
    LEA CLOSE_POWERPC(PC),A0
    MOVE.L  #CLOSE_POWERPC_END,D0
    BSR CCOPY
.7:
    MOVE.W  .RTS(PC),(A4)+
    BSR     ADDERRCODE
    MOVE.L  A4,CURACODE
    JSR CHECK3
.EX:
.RTS:   RTS
.JSR:   JSR .JSR
;*-*

;; Copy
CCOPY:
    SUB.L   A0,D0
    LSR.L   #1,D0
    SUBQ.L  #1,D0
.XL:MOVE.W  (A0)+,(A4)+
    DBRA    D0,.XL
    RTS
;*-*

;; Check CPU/FPU
CHECK_CPU_FPU:
    MOVE.W  #0,D0
    BRA.S   LM1
LM0:DC.B    'This program requires a better CPU/FPU',10,0
LM1:MOVE.L  $4.W,A6
    AND.W   296(A6),D0
    BNE     CHECK_CPU_FPU_END
LK0000:
    LEA     LM0(PC),A0
    MOVEQ   #2,D7
    BRA     CHECK_CPU_FPU_END
CHECK_CPU_FPU_END:
;*-*
;; Check OSVersion
CHECK_OSVERSION:
    MOVE.W  #0,D0
    BRA.B   LM3
LM2:DC.B    'This program requires a newer OS',10,0
LM3:MOVE.L  $4.W,A6
    CMP.W   20(A6),D0
    BLE     CHECK_OSVERSION_END
LK0001:
    MOVE.L  #$38001,D7
    LEA     LM2(PC),A0
    BRA     CHECK_OSVERSION_END
CHECK_OSVERSION_END:
;*-*
;; OpenCon37+
OPENCON37P:
    BRA.S   .X
    DC.B    'CON:0/11/640/80/Output/AUTO/CLOSE/WAIT',0
    EVEN
.X: MOVE.L  -44(A4),A6
    LEA     OPENCON37P+2(PC),A0
    MOVE.L  A0,D1
    MOVE.L  #1006,d2
    JSR     -30(a6)
    MOVE.L  D0,-12(A4)
    TST.L   -8(A4)
    BNE.S   OPENCON37PEND
    MOVE.L  D0,-8(A4)
OPENCON37PEND:
;*-*
;; OpenPool
OPENPOOL:
    MOVE.L  #$10001,D0
    MOVE.L  #10240,D1
    MOVE.L  #256,D2
    MOVE.L  $4,A6
    JSR     -696(A6)
    MOVE.L  D0,-120(A4)
OPENPOOLEND:
;*-*
;; OpenUtil
OPENUTIL:
    BRA.S   OPENUTIL2
UTILNAME:
    DC.B    "utility.library",0
    EVEN
OPENUTIL2:
    MOVE.L  $4.W,A6
    LEA UTILNAME(PC),A1
    MOVEQ   #33,D0
    JSR -552(A6)
    MOVE.L  D0,-124(A4)
    BNE.S   OPENUTILEND
    MOVEQ   #0,D2
OPENUTILEND:
;*-*
;; OpenMathBas
OPENMATHBAS:
    BRA.S   BASCONT
INITMATH:
    DC.B    "mathieeesingbas.library",0
    EVEN
BASCONT:
    MOVE.L  4.W,A6
    LEA INITMATH(PC),A1
    MOVEQ   #33,D0
    JSR -552(A6)
    MOVE.L  D0,-56(A4)
    BNE.S   OPENBASEND
    MOVEQ   #0,D2
OPENBASEND:
;*-*
;; OpenMathTrans
OPENMATHTRANS:
    BRA.S   TRANSCONT
INITMATHT:
    DC.B    "mathieeesingtrans.library",0
    EVEN
TRANSCONT:
    MOVE.L  4.W,A6
    LEA INITMATHT(PC),A1
    MOVEQ   #33,D0
    JSR -552(A6)
    MOVE.L  D0,-60(A4)
    BNE.S   OPENTRANSEND
    MOVEQ   #0,D2
OPENTRANSEND:
;*-*
;; OpenFiles
OPENFILES:
    CLR.L   -128(A4)
OPENFILESEND:
;*-*
;; OpenPowerPC
OPEN_POWERPC:
    LEA .LB(PC),A1
    MOVEQ   #0,D0
    MOVE.L  $4.W,A6
    JSR -552(A6)
    MOVE.L  D0,-132(a4)
    BNE.S   OPEN_POWERPC_END
    BRA.S   .X
.M: DC.B    'This program requires '
.LB:DC.B    'powerpc.library'
.Z: DC.B    0,0,0
.X: LEA     .M(PC),A0
    LEA     .Z(PC),A1
    MOVE.B  #10,(A1)
    BRA     OPEN_POWERPC_END
OPEN_POWERPC_END:
;*-*

;; InitError
InitializationError:
    TST.L   -8(A4)
    BEQ.S   .ALERT
    MOVE.L  A0,D2
.c: TST.B   (A0)+
    BNE     .c
    MOVe.L  A0,D3
    SUB.L   D2,D3
    MOVe.L  -8(A4),D1
    MOVE.L  -44(A4),A6
    JSR     -48(A6)
    BRA.S   .QUIT
.ALERT:
    MOVE.L  -40(A4),A6
    JSR     -108(A6)
.QUIT:
    MOVE.L  -24(A4),A0
    move.l  #20,-28(a4)
    JMP     (A0)
InitializationErrorEnd:
;*-*

;; ClosePool
CLOSEPOOL:
    MOVE.L  $4.W,A6
    MOVE.L  -120(A4),A0
    JSR     -702(A6)
CLOSEPOOLEND:
;*-*
;; CloseUtil
CLOSEUTIL:
    MOVE.L $4.W,A6
    MOVE.L -124(A4),D0
    BEQ CLOSEUTILEND
    MOVE.L D0,A1
    JSR -414(A6)
CLOSEUTILEND:
;*-*
;; CloseMathBas
CLOSEMATHBAS:
    MOVE.L  4.W,A6
    MOVE.L  -56(A4),D0
    BEQ.S   CLOSEBASEND
    MOVE.L  D0,A1
    JSR -414(A6)
CLOSEBASEND:
;*-*
;; CloseMathTrans
CLOSEMATHTRANS:
    MOVE.L  4.W,A6
    MOVE.L  -60(A4),D0
    BEQ.S   CLOSETRANSEND
    MOVE.L  D0,A1
    JSR -414(A6)
CLOSETRANSEND:
;*-*
;; CloseFiles
CLOSEFILES:
    move.l  -128(a4),d0
    beq     CLOSEFILESEND
.1: move.l  d0,a0
    move.l  (a0),d4
    move.l  4(a0),d5
    move.l  a0,a1
    moveq   #8,d0
    move.l  -40(a4),a6
    jsr     -210(a6)
    move.l  d5,d1
    move.l  -44(a4),a6
    jsr     -36(a6)
    move.l  d4,d0
    bne     .1
CLOSEFILESEND:
;*-*
;; CloseCon37+
CLOSECON37P:
    MOVE.L  -12(A4),D1
    BEQ     CLOSECON37PEND
    MOVE.L  -44(a4),A6
    JSR     -36(A6)
    CLR.L   -12(A4)
CLOSECON37PEND:
;*-*
;; CloseCon33-37
CLOSECON33_37:
    MOVE.L  -12(A4),D1
    BEQ     CLOSECON33_37END
    MOVE.L  -44(a4),A6
    MOVE.L  D1,D4
    MOVEQ   #0,D2
    MOVEQ   #0,D3
    JSR     -42(A6)
    MOVE.L  D4,D1
    JSR     -36(A6)
    CLR.L   -12(A4)
CLOSECON33_37END:
;*-*
;; ClosePowerPC
CLOSE_POWERPC:
    MOVE.L $4.W,A6
    MOVE.L -132(A4),A1
    JSR -414(A6)
CLOSE_POWERPC_END:
;*-*

;; Add global vars
ADDGLOBVARS:
    BTST    #3,CODEPREFS+3
    BEQ.S   .C
    RTS
.C: GETM    A5
    LEA GLOBVARTAB(PC),A4
.1: MOVE.L  (A4)+,D7
    MOVE.L  (A4)+,D6
    BEQ     .2
    CMP.L   #GLOBVARTAB+24,A4
    BNE.S   .NARG
    LEA 9(A5),A1
    MOVE.L  A1,ARGBYTE
.NARG:
    CMP.L   #GLOBVARTAB+32,A4
    BNE.S   .NEXC
    MOVE.L  A5,VAR_EXECBASE
    ADDQ.L  #4,VAR_EXECBASE
.NEXC:
    CMP.L   #GLOBVARTAB+40,A4
    BNE.S   .NDOS
    MOVE.L  A5,VAR_DOSBASE
    ADDQ.L  #4,VAR_DOSBASE
.NDOS:
    CMP.L   #GLOBVARTAB+48,A4
    BNE.S   .NINT
    MOVE.L  A5,VAR_INTUIBASE
    ADDQ.L  #4,VAR_INTUIBASE
.NINT:
    CMP.L   #GLOBVARTAB+56,A4
    BNE.S   .NGFX
    MOVE.L  A5,VAR_GFXBASE
    ADDQ.L  #4,VAR_GFXBASE
.NGFX:
    CMP.L   #GLOBVARTAB+64,A4
    BNE.S   .NRST
    MOVE.L  A5,VAR_RASTPORT
    ADDQ.L  #4,VAR_RASTPORT
.NRST:
    CMP.L   #GLOBVARTAB+72,A4
    BNE.S   .NWBM
    MOVE.L  A5,VAR_WBMESSAGE
    ADDQ.L  #4,VAR_WBMESSAGE
.NWBM:
.FILL:
    MOVE.L  D7,A1
    HASH    A1,D0,D1
    LSL.L   #2,D0
    MOVE.L  #IDENTHASH,A1
    ADD.L   D0,A1
    MOVE.L  (A1),(A5)+
    MOVE.L  A5,(A1)

    MOVE.L  #PSEUDOA4,A1
    MOVE.L  A5,(A1,D6)

    MOVE.L  D7,(A5)+        ; SET ASCIIPTR
    MOVE.B  #GLOBV,(A5)+    ; SET GLOBAL(=2)
    MOVE.B  #2,(A5)+        ; UNREF+SYSTEMVAR
    CLR.L   (A5)+
    MOVE.W  D6,(A5)+        ; SET OFFS
    BRA     .1
.2: DONEH   A5
;*-*
;; Add global consts
BURP:   MOVEQ   #NRSYSCONST,D0
    SUBQ.W  #1,D0
    GETM    A0
    LEA SYSCONST(PC),A1
.3: MOVE.L  (A1)+,A2
    MOVE.L  A2,A3
    HASH    A3,D1,D2
    LSL.L   #2,D1
    ADD.L   #CONSTHASH,D1
    MOVE.L  D1,A3
    MOVE.L  (A3),(A0)
    MOVE.L  A0,(A3)
    ADDQ.L  #4,A0
    MOVE.L  A2,(A0)+
    CMPA.L  #SYSCONST+36,A1
    BNE.S   .SL
    MOVE.L  A0,STRLENADR
.SL:    MOVE.L  (A1)+,(A0)+
    CLR.W   (A0)+
    DBRA    D0,.3
    DONEH   A0
    RTS
;*-*
;; Consts
NRSYSCONST  = 20
STRLENADR:  DC.L    0

SYSCONST:
    DC.L    .1,-1,.2,0,.3,0,.4,-1,.5,0,.6,120,.7,1005,.8,1006,.9,0,.10,0
    DC.L    .11,0,.12,1,.13,2,.14,.3,.15,$80000000,.16,-1,.17,-1,.18,0,.19,1,.20,1004

.1: DC.B    "TRUE",0
.2: DC.B    "FALSE",0
.3: DC.B    "NIL",0
.4: DC.B    "ALL",0
.5: DC.B    "STRLEN",0      ; MUST BE #5
.6: DC.B    "GADGETSIZE",0
.7: DC.B    "OLDFILE",0
.8: DC.B    "NEWFILE",0
.9: DC.B    "EMPTY",0
.10:DC.B    "TAG_DONE",0
.11:DC.B    "TAG_END",0
.12:DC.B    "TAG_IGNORE",0
.13:DC.B    "TAG_MORE",0
.14:DC.B    "TAG_SKIP",0
.15:DC.B    "TAG_USER",0
.16:DC.B    "OFFSET_BEGINNING",0
.17:DC.B    "OFFSET_BEGINING",0
.18:DC.B    "OFFSET_CURRENT",0
.19:DC.B    "OFFSET_END",0
.20:DC.B    "READWRITE",0
    EVEN
;*-*
;; Vars
ARGBYTE:  DC.L    0       ; EXACT BYTE LOC OF "arg".FLAGS

GLOBVARTAB:
    DC.L    .1,-8
    DC.L    .2,-12
    DC.L    .3,-32          ; must be 16..24 bytes offset
    DC.L    .4,-40          ; must be 24..32 bytes offset!!!
    DC.L    .5,-44          ; keep the places!!!
    DC.L    .6,-48          ; _UNCHANGED_
    DC.L    .7,-52          ; those vars refer to special
    DC.L    .8,-16          ; pointers!
    DC.L    .9,-36
;   DC.L    .10,-56
    DC.L    .11,-84
    DC.L    .12,-92
    DC.L    .13,-96
    DC.L    .14,-120
    DC.L    .15,-124
    DC.L    0,0

.1: DC.B    "stdout",0
.2: DC.B    "conout",0
.3: DC.B    "arg",0         ; MUST BE 3RD
.4: DC.B    "execbase",0
.5: DC.B    "dosbase",0
.6: DC.B    "intuitionbase",0
.7: DC.B    "gfxbase",0
.8: DC.B    "stdrast",0
.9: DC.B    "wbmessage",0
;.10:   DC.B    "mathieeesingbasbase",0
.11:    DC.B    "exception",0
.12:    DC.B    "stdin",0
.13:    DC.B    "exceptioninfo",0
.14:    DC.B    "__pool",0
.15:    DC.B    "utilitybase",0
    EVEN
;*-*

;; Init funcs
INITFUNCS:            ; init efuncs
    MOVE.L  #EFUNCBYTE,A0
    MOVE.L  #EFUNCRAISE,A1
    MOVE.W  #NREFUNC,D0
    SUBQ.W  #1,D0
    MOVEQ   #0,D1
.1: MOVE.B  D1,(A0)+
    MOVE.W  D1,(A1)+
    ADDQ.L  #8,A1
    DBRA    D0,.1
    RTS
;*-*
;; Link delegate constructors
LINKDELEGATECONSTRUCTORS:
    LEA OLIST+4,A0
.XL:MOVE.L  -(A0),D0
    BEQ .X
    MOVE.L  D0,A0
    TST.W   ODEL(A0)
    BEQ.S   .XL
    BTST    #1,OFLAGS(A0)
    BNE.S   .XL
    MOVE.L  CURACODE,A4     ; 32K VERWEG !
    MOVE.L  A4,ODCODE(A0)       ; SET DCODE FOR THIS OBJECT
    MOVE.L  OSUPER(A0),D0
    BEQ.S   .NSUP
    MOVE.L  D0,A2           ; A2=SUPER
    TST.W   (A2)            ; CHECK IF SUPER HAS DELCONSTRUCTOR
    BEQ.S   .NSUP
    GETM    A1
    MOVE.L  OACC(A2),(A1)       ; ADD TO ACCESSLIST OF SUPER
    MOVE.L  A1,OACC(A2)
    ADDQ.L  #4,A1
    MOVE.W  .J(PC),(A4)+        ; MAKE JSR SUPERDELEGATECONSTR.
    MOVE.L  A4,(A1)+
    MOVE.W  #1,(A1)+
    DONEM   A1
    CLR.L   (A4)+
.NSUP:  MOVE.W  .GL(PC),(A4)+
    MOVEQ   #0,D0
    MOVE.W  OSIZE(A0),D0
    MOVE.L  D0,(A4)+
    MOVE.L  OMETHOD(A0),A1      ; A1=METHODLIST
.ML:    BTST    #0,M_FLAGS(A1)
    BNE.S   .N
    MOVE.L  M_PROC(A1),A2
    MOVE.L  14(A2),A2       ; A2=IDENT
    MOVE.L  .MLEA(PC),(A4)+
    MOVE.W  .MLEA2(PC),NEWOP
    MOVE.W  10(A2),D0
    MOVE.L  A0,A3
    JSR ADDBRANCH
    MOVE.L  A3,A0
    MOVE.W  .MM(PC),(A4)+
    MOVE.W  M_OFF(A1),(A4)+
.N: MOVE.L  (A1),A1
    MOVE.L  A1,D0
    BNE.S   .ML
    MOVE.W  .RTS(PC),(A4)+
    MOVE.L  A4,CURACODE
    JSR CHECK3
    BRA .XL
.X: RTS
.J: JSR .X
.MLEA:  LEA .X(PC),A1
.MLEA2: LEA .X,A1
.MM:    MOVE.L  A1,4(A0)
.RTS:   RTS
.GL:    MOVE.L  #1,(A0)
;*-*
;; Link funcs
LINKFUNCS:
    MOVEM.L A6/D7,-(A7)
    TSTMOD
    BNE.S   .X
    LEA     EFUNCFLAGSTAB,A6
    MOVE.L  #EFUNCBYTE,A3
    MOVE.W  #NREFUNC,D2
    SUBQ.W  #1,D2
    MOVE.L  #EFUNCTAB,D4
    MOVEQ   #0,D3
    MOVE.L  CURACODE,A4     ; 32K VERWEG !
.1: TST.B   (A3)+
    BEQ.S   .2
    MOVE.W  D3,D0           ;
    LSL.W   #1,D0           ;
    MOVE.W  0(A6,D0),D7     ;

    MOVE.L  D4,A1
    MOVE.L  D3,D0
    MULU    #EFUNCENTRYSIZE,D0
    ADD.L   D0,A1
    MOVE.L  4(A1),A2       ; BEG CODE
    MOVE.L  8(A1),D1       ; END CODE
    MOVE.L  D3,D0
    ADD.L   #10,D0
    JSR ADDLABEL
    SUB.L   A2,D1
    LSR.L   #1,D1
    SUBQ.L  #1,D1
.3: MOVE.W  (A2)+,(A4)+
    DBRA    D1,.3
.2: ADDQ.L  #1,D3
    MOVE.L  A4,CURACODE
    JSR CHECK3
    DBRA    D2,.1
.X: MOVEM.L (A7)+,A6/D7
    RTS
;*-*
;; Link lib
LINKLIB:
    MOVEM.L D6/D7/A0-A2/A5/A6,-(A7)
    MOVE.L  #LIBHASH,A2
    MOVE.l  #255,D6
    MOVE.L  CURACODE,A4
.1:
    MOVE.L  (A2)+,D0
.LOOP:
    TST.L   D0
    BEQ.S   .EXIT
    MOVE.L  D0,A0
    MOVE.L  LIB_NEXT(A0),D0
    TST.L   LIB_USED(A0)
    BEQ     .LOOP
    CMP.W   #1,LIB_TYPE(A0)
    BEQ     .LOOP
    CMP.W   #2,LIB_TYPE(A0)
    BEQ     .LOOP

    MOVE.L  A4,A6
    MOVE.L  A4,LIB_USED(A0)
    MOVE.L  LIB_CLEN(A0),d7
    move.l  LIB_CODE(a0),a1

    lsr.l   #2,d7
    subq.l  #1,d7
    move.l  a4,a6
.COPY:
    move.l  (a1)+,(a4)+
    dbf     d7,.COPY
    bsr     FIXRELOC
    bra     .LOOP
.EXIT:
    DBRA    D6,.1
    MOVE.L  A4,CURACODE
    movem.l (a7)+,D6/d7/A0-A2/A5/a6
    rts
;*-*

;; FixReloc
FIXRELOC:
; A6 - copied code start
; A0 - code node
    MOVEM.L D0-A6,-(A7)
    MOVE.L  LIB_RELO(A0),d1       ; reloc
    BEQ     .EXIT
    MOVE.L  D1,A1

    cmp.l   #$3EC,(A1)+
    BNE     ERROR51
    move.l  (a1)+,d3
    addq.l  #4,a1           ; code
    subq.l  #1,d3
.LOOP:
    MOVE.L  (a1)+,d2
    move.l  a6,a4
    add.l   d2,a4
    move.l  (a4),d0

    add.l   a6,d0
    sub.l   ACODE,d0
    jsr     FORCEABSHERE
    dbf D3,.LOOP
.EXIT:
    MOVEM.L (A7)+,D0-A6
    RTS
;*-*
;*-*
;; GenVars
; THESE ARE SHARED!

GVA0D0D5_0:
    GENVAR  0,A0,D0,D5      ; IIII
    RTS


GVA0D0_0:
    GENVAR  0,A0,10(A0),D0      ; III
    RTS
GVA1D0_0:
    GENVAR  0,A1,10(A1),D0      ; III
    RTS
GVA0D1_0:
    GENVAR  0,A0,10(A0),D1      ; IIIII
    RTS
GVA0D2_0:
    MOVE.L  D1,-(A7)
    MOVE.L  D2,D1
    BSR     GVA0D1_0        ; II
    MOVE.L  (A7)+,D1
    RTS
GVA0D3_0:
    MOVE.L  D1,-(A7)
    MOVE.L  D3,D1
    BSR.W   GVA0D1_0        ; I
    MOVE.L  (A7)+,D1
    RTS
GVA0D4_0:
    MOVE.L  D1,-(A7)
    MOVE.L  D4,D1
    BSR.W   GVA0D1_0        ; I
    MOVE.L  (A7)+,D1
    RTS
GVA1D2_0:
    GENVAR  0,A1,10(A1),D2      ; I
    RTS
GVA6D3_0:
    GENVAR  0,A6,10(A6),D3      ; I
    RTS


GVA0D0_9:
    GENVAR  9,A0,10(A0),D0      ; II
    RTS
GVA0D2_9:
    MOVE.L  D0,-(A7)
    MOVE.L  D2,D0
    BSR.W   GVA0D0_9        ; I
    MOVE.L  (A7)+,D0
    RTS
GVA0D4_9:
    MOVE.L  D0,-(A7)
    MOVE.L  D4,D0
    BSR.W   GVA0D0_9        ; II
    MOVE.L  (A7)+,D0
    RTS
GVA0D5_9:
    MOVE.L  D0,-(A7)
    MOVE.L  D5,D0
    BSR.W   GVA0D0_9        ; I
    MOVE.L  (A7)+,D0
    RTS
GVA0D7_9:
    MOVE.L  D0,-(A7)
    MOVE.L  D7,D0
    BSR.W   GVA0D0_9        ; I
    MOVE.L  (A7)+,D0
    RTS

GVA1D2_9:
    GENVAR  9,A1,10(A1),D2      ; I
    RTS
GVA6D0_9:
    GENVAR  9,A6,10(A6),D0      ; I
    RTS
GVA6D3_9:
    MOVE.L  D0,-(A7)
    MOVE.L  D3,D0
    BSR.W   GVA6D0_9        ; II
    MOVE.L  (A7)+,D0
    RTS
;*-*


