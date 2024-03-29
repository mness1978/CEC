;'''''''''''''''''''''''''''''''''''''''''''''''';
; PowerPC assembler command set                  ;
; for use with m68k macro assemblera             ;
;                                                ;
; performed by Tomasz Wiszkowski [error/bla²]    ;
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,;

;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\;
;\/ every command and keyword starts with "_"  \/;
;/\ to be recognized as a ppc related stuff    /\;
;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/;

;ŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻŻ;
; of You're using a golded, set the folding      ;
; sentences to ";;" and ";*-*" to fold this file ;
;________________________________________________;


;; gprs
_r0 equ 0
_r1 equ 1
_r2 equ 2
_r3 equ 3
_r4 equ 4
_r5 equ 5
_r6 equ 6
_r7 equ 7
_r8 equ 8
_r9 equ 9
_r10 equ 10
_r11 equ 11
_r12 equ 12
_r13 equ 13
_r14 equ 14
_r15 equ 15
_r16 equ 16
_r17 equ 17
_r18 equ 18
_r19 equ 19
_r20 equ 20
_r21 equ 21
_r22 equ 22
_r23 equ 23
_r24 equ 24
_r25 equ 25
_r26 equ 26
_r27 equ 27
_r28 equ 28
_r29 equ 29
_r30 equ 30
_r31 equ 31
;*-*
;; fprs
_f0 equ 0
_f1 equ 1
_f2 equ 2
_f3 equ 3
_f4 equ 4
_f5 equ 5
_f6 equ 6
_f7 equ 7
_f8 equ 8
_f9 equ 9
_f10 equ 10
_f11 equ 11
_f12 equ 12
_f13 equ 13
_f14 equ 14
_f15 equ 15
_f16 equ 16
_f17 equ 17
_f18 equ 18
_f19 equ 19
_f20 equ 20
_f21 equ 21
_f22 equ 22
_f23 equ 23
_f24 equ 24
_f25 equ 25
_f26 equ 26
_f27 equ 27
_f28 equ 28
_f29 equ 29
_f30 equ 30
_f31 equ 31
;*-*
;; conditions
_cond_true equ 12
_cond_false equ 4
_cond_always equ 20
_cond_dz_eq equ 18
_cond_dnz equ 16
_cond_dz_true equ 10
_cond_dnz_true equ 8
_cond_dz_false equ 2
_cond_dnz_false equ 0

_cond_lt equ 0
_cond_le equ 1
_cond_eq equ 2
_cond_ov equ 3
_cr0 equ 0
_cr1 equ 4
_cr2 equ 8
_cr3 equ 12
_cr4 equ 16
_cr5 equ 20
_cr6 equ 24
_cr7 equ 28
;*-*
;; trap conditions
_trap_gt equ 1
_trap_lt equ 2
_trap_eq equ 4
_trap_ugt equ 8
_trap_ult equ 16
;*-*

;; add      / addo      / add.      / addo.
_add: macro
    dc.l (266*2)+(31*(1<<(31-5)))+(\1*(1<<(31-10)))+(\2*(1<<(31-15)))+(\3*(1<<(31-20)))
    endm
_addo: macro
    dc.l (266*2)+(31*(1<<(31-5)))+(\1*(1<<(31-10)))+(\2*(1<<(31-15)))+(\3*(1<<(31-20)))+1<<(31-21)
    endm
_add_: macro
    dc.l (266*2)+(31*(1<<(31-5)))+(\1*(1<<(31-10)))+(\2*(1<<(31-15)))+(\3*(1<<(31-20)))+1
    endm
_addo_: macro
    dc.l (266*2)+(31*(1<<(31-5)))+(\1*(1<<(31-10)))+(\2*(1<<(31-15)))+(\3*(1<<(31-20)))+1<<(31-21)+1
    endm
;*-*
;; addc     / addco     / addc.     / addco.
_addc: macro
    dc.l (10*2)+(31*(1<<(31-5)))+(\1*(1<<(31-10)))+(\2*(1<<(31-15)))+(\3*(1<<(31-20)))
    endm
_addco: macro
    dc.l (10*2)+(31*(1<<(31-5)))+(\1*(1<<(31-10)))+(\2*(1<<(31-15)))+(\3*(1<<(31-20)))+1<<(31-21)
    endm
_addc_: macro
    dc.l (10*2)+(31*(1<<(31-5)))+(\1*(1<<(31-10)))+(\2*(1<<(31-15)))+(\3*(1<<(31-20)))+1
    endm
_addco_: macro
    dc.l (10*2)+(31*(1<<(31-5)))+(\1*(1<<(31-10)))+(\2*(1<<(31-15)))+(\3*(1<<(31-20)))+1<<(31-21)+1
    endm
;*-*
;; adde     / addeo     / adde.     / addeo.
_adde: macro
    dc.l (138*2)+(31*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_addeo: macro
    dc.l (138*2)+(31*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1<<(31-21)
    endm
_adde_: macro
    dc.l (138*2)+(31*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
_addeo_: macro
    dc.l (138*2)+(31*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1<<(31-21)+1
    endm
;*-*
;; addi     / li        / subi
_addi: macro
    dc.l    (14*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+\3
    endm
_li: macro
    _addi \1,0,\2
    endm
_subi: macro
    _addi \1,\2,-\3
    endm
;*-*
;; addic    / subic     / addic.    / subic.
_addic: macro
    dc.l    (12*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+\3
    endm
_subic: macro
    _addic \1,\2,-\3
    endm
_addic_: macro
    dc.l    (13*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+\3
    endm
_subic_: macro
    _addic \1,\2,-\3
    endm
;*-*
;; addis    / lis       / subis
_addis: macro
    dc.l    (15*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+\3
    endm
_lis: macro
    _addis  \1,0,\2
    endm
_subis: macro
    _addis \1,\2,-\3
    endm
;*-*
;; addme    / addmeo    / addme.    / addmeo.
_addme: macro
    dc.l    (31*1<<(31-5))+(234*2)+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
_addmeo: macro
    dc.l    (31*1<<(31-5))+(234*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(1<<(31-21))
    endm
_addme_: macro
    dc.l    (31*1<<(31-5))+(234*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+1
    endm
_addmeo_: macro
    dc.l    (31*1<<(31-5))+(234*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(1<<(31-21))+1
    endm
;*-*
;; addze    / addzeo    / addze.    / addzeo.
_addze: macro
    dc.l    (31*1<<(31-5))+(202*2)+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
_addzeo: macro
    dc.l    (31*1<<(31-5))+(202*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(1<<(31-21))
    endm
_addze_: macro
    dc.l    (31*1<<(31-5))+(202*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+1
    endm
_addzeo_: macro
    dc.l    (31*1<<(31-5))+(202*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(1<<(31-21))+1
    endm
;*-*
;; and      / and.
_and: macro
    dc.l    (31*1<<(31-5))+(28*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_and_: macro
    dc.l    (31*1<<(31-5))+(28*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; andc     / andc.
_andc: macro
    dc.l    (31*1<<(31-5))+(60*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_andc_: macro
    dc.l    (31*1<<(31-5))+(60*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; andi.
_andi_: macro
    dc.l (28*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+\3
    endm
;*-*
;; andis.
_andis_: macro
    dc.l (29*1<<(31-5))+(\1*1<<(31-10))+(\2*1<<(31-15))+\3
    endm
;*-*
;; b        / ba        / bl        / bla
_b: macro
    dc.l ((18*1<<(31-5))+(\1-*))
    endm
_ba: macro
    dc.l \1+((18*1<<(31-5))+2)
    endm
_bl: macro
    dc.l (18*1<<(31-5))+(\1-*)+1
    endm
_bla: macro
    dc.l (18*1<<(31-5))+\1+3
    endm
;*-*
;; bc       / bca       / bcl       / bcla
_bc: macro
    dc.l ((16*1<<(31-5))+(\3-*))+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
_bca: macro
    dc.l \3+((16*1<<(31-5))+2)+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
_bcl: macro
    dc.l (16*1<<(31-5))+(\3-*)+1+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
_bcla: macro
    dc.l (16*1<<(31-5))+\3+3+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
;*-*
;; bcctr    / bcctrl
_bcctr: macro
    dc.l (19*1<<(31-5))+(528*2)+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
_bcctrl: macro
    dc.l (19*1<<(31-5))+(528*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+1
    endm
;*-*
;; bclr     / bclrl
_bclr: macro
    dc.l (19*1<<(31-5))+(16*2)+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
_bclrl: macro
    dc.l (19*1<<(31-5))+(16*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+1
    endm
;*-*
;; cmp      / cmpd      / cmpw
_cmp: macro
    dc.l    (31*1<<(31-5))+((\1>>2)*1<<(31-8))+(\2*1<<(31-10))+(\3*1<<(31-15))+(\4*1<<(31-20))
    endm
_cmpd: macro
    _cmp \1,1,\2,\3
    endm
_cmpw: macro
    _cmp \1,0,\2,\3
    endm
;*-*
;; cmpi     / cmpdi     / cmpwi
_cmpi: macro
    dc.l    (11*1<<(31-5))+((\1>>2)*1<<(31-8))+(\2*1<<(31-10))+(\3*1<<(31-15))+\4
    endm
_cmpdi: macro
    _cmpi \1,1,\2,\3
    endm
_cmpwi: macro
    _cmpi \1,0,\2,\3
    endm
;*-*
;; cmpl     / cmpld     / cmplw
_cmpl: macro
    dc.l    (31*1<<(31-5))+(32*2)+((\1>>2)*1<<(31-8))+(\2*1<<(31-10))+(\3*1<<(31-15))+(\4*1<<(31-20))
    endm
_cmpld: macro
    _cmpl \1,1,\2,\3
    endm
_cmplw: macro
    _cmpl \1,0,\2,\3
    endm
;*-*
;; cmpli    / cmpldi    / cmplwi
_cmpli: macro
    dc.l    (10*1<<(31-5))+((\1>>2)*1<<(31-8))+(\2*1<<(31-10))+(\3*1<<(31-15))+\4
    endm
_cmpldi: macro
    _cmpli \1,1,\2,\3
    endm
_cmplwi: macro
    _cmpli \1,0,\2,\3
    endm
;*-*
;; cntlzw   / cntlzw.
_cntlzw: macro
    dc.l (31*1<<(31-5))+(26*2)+(\1*1<<(31-10))+(\2*1<<(31-15))
    endm
_cntlzw_: macro
    dc.l (31*1<<(31-5))+(26*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+1
    endm
;*-*
;; crand
_crand: macro
    dc.l (19*1<<(31-5))+(257*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; crandc
_crandc: macro
    dc.l (19*1<<(31-5))+(129*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; creqv    / crset
_creqv: macro
    dc.l (19*1<<(31-5))+(289*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_crset: macro
    _creqv \1,\1,\1
    endm
;*-*
;; crnand
_crnand: macro
    dc.l (19*1<<(31-5))+(225*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; crnor    / crnot
_crnor: macro
    dc.l (19*1<<(31-5))+(33*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_crnot: macro
    _crandc \1,\2,\2
    endm
;*-*
;; cror     / crmove
_cror: macro
    dc.l (19*1<<(31-5))+(449*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_crmove: macro
    _crandc \1,\2,\2
    endm
;*-*
;; crorc
_crorc: macro
    dc.l (19*1<<(31-5))+(417*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; crxor    / crclr
_crxor: macro
    dc.l (19*1<<(31-5))+(193*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_crclr: macro
    _crxor \1,\1,\1
    endm
;*-*
;; dcba
_dcba: macro
    dc.l    (31*1<<(31-5))+(758*2)+(\1*1<<(31-15))+(\2*1<<(31-20))
    endm
;*-*
;; dcbf
_dcbf: macro
    dc.l    (31*1<<(31-5))+(86*2)+(\1*1<<(31-15))+(\2*1<<(31-20))
    endm
;*-*
;; dcbi
_dcbi: macro
    dc.l    (31*1<<(31-5))+(470*2)+(\1*1<<(31-15))+(\2*1<<(31-20))
    endm
;*-*
;; dcbst
_dcbst: macro
    dc.l    (31*1<<(31-5))+(54*2)+(\1*1<<(31-15))+(\2*1<<(31-20))
    endm
;*-*
;; dcbt
_dcbt: macro
    dc.l    (31*1<<(31-5))+(278*2)+(\1*1<<(31-15))+(\2*1<<(31-20))
    endm
;*-*
;; dcbtst
_dcbtst: macro
    dc.l    (31*1<<(31-5))+(246*2)+(\1*1<<(31-15))+(\2*1<<(31-20))
    endm
;*-*
;; dcbz
_dcbz: macro
    dc.l    (31*1<<(31-5))+(1014*2)+(\1*1<<(31-15))+(\2*1<<(31-20))
    endm
;*-*
;; divw     / divwo     / divw.     / divwo.
_divw: macro
    dc.l    (31*1<<(31-5))+(491*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_divwo: macro
    dc.l    (31*1<<(31-5))+(491*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+(1<<(31-21))
    endm
_divw_: macro
    dc.l    (31*1<<(31-5))+(491*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
_divwo_: macro
    dc.l    (31*1<<(31-5))+(491*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+(1<<(31-21))+1
    endm
;*-*
;; divwu    / divwuo    / divwu.    / divwuo.
_divwu: macro
    dc.l    (31*1<<(31-5))+(459*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_divwuo: macro
    dc.l    (31*1<<(31-5))+(459*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+(1<<(31-21))
    endm
_divwu_: macro
    dc.l    (31*1<<(31-5))+(459*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
_divwuo_: macro
    dc.l    (31*1<<(31-5))+(459*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+(1<<(31-21))+1
    endm
;*-*
;; eciwx
_eciwx: macro
    dc.l    (31*1<<(31-5))+(310*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; ecowx
_ecowx: macro
    dc.l    (31*1<<(31-5))+(438*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; eieio
_eieio: macro
    dc.l    (31*1<<(31-5))+(854*2)
    endm
;*-*
;; eqv      / eqv.
_eqv: macro
    dc.l    (31*1<<(31-5))+(284*2)+(\2*1<<(31-10))+(\1*1<<(31-15))+(\3*1<<(31-20))
    endm
_eqv_: macro
    dc.l    (31*1<<(31-5))+(284*2)+(\2*1<<(31-10))+(\1*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; extsb    / extsb.
_extsb: macro
    dc.l (31*1<<(31-5))+(954*2)+(\1*1<<(31-15))+(\2*1<<(31-10))
    endm
_extsb_: macro
    dc.l (31*1<<(31-5))+(954*2)+(\1*1<<(31-15))+(\2*1<<(31-10))+1
    endm
;*-*
;; extsh    / extsh.
_extsh: macro
    dc.l (31*1<<(31-5))+(922*2)+(\1*1<<(31-15))+(\2*1<<(31-10))
    endm
_extsh_: macro
    dc.l (31*1<<(31-5))+(922*2)+(\1*1<<(31-15))+(\2*1<<(31-10))+1
    endm
;*-*
;; fabs     / fabs.
_fabs: macro
    dc.l    (63*1<<(31-5))+(264*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fabs_: macro
    dc.l    (63*1<<(31-5))+(264*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fadd     / fadd.
_fadd: macro
    dc.l (63*1<<(31-5))+(21*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_fadd_: macro
    dc.l (63*1<<(31-5))+(21*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; fadds    / fadds.
_fadds: macro
    dc.l (59*1<<(31-5))+(21*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_fadds_: macro
    dc.l (59*1<<(31-5))+(21*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; fcmpo
_fcmpo: macro
    dc.l    (63*1<<(31-5))+(32*2)+(\1>>2*1<<(31-8))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; fcmpu
_fcmpu: macro
    dc.l    (63*1<<(31-5))+(\1>>2*1<<(31-8))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; fctiw    / fctiw.
_fctiw: macro
    dc.l    (63*1<<(31-5))+(14*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fctiw_: macro
    dc.l    (63*1<<(31-5))+(14*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fctiwz   / fctiwz.
_fctiwz: macro
    dc.l    (63*1<<(31-5))+(15*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fctiwz_: macro
    dc.l    (63*1<<(31-5))+(15*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fdiv     / fdiv.
_fdiv: macro
    dc.l    (63*1<<(31-5))+(18*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_fdiv_: macro
    dc.l    (63*1<<(31-5))+(18*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; fdivs    / fdivs.
_fdivs: macro
    dc.l    (59*1<<(31-5))+(18*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_fdivs_: macro
    dc.l    (59*1<<(31-5))+(18*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; fmadd    / fmadd.
_fmadd: macro
    dc.l (63*1<<(31-5))+(29*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fmadd_: macro
    dc.l (63*1<<(31-5))+(29*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fmadds   / fmadds.
_fmadds: macro
    dc.l (59*1<<(31-5))+(29*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fmadds_: macro
    dc.l (59*1<<(31-5))+(29*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fmr      / fmr.
_fmr: macro
    dc.l (63*1<<(31-5))+(72*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fmr_: macro
    dc.l (63*1<<(31-5))+(72*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fmsub    / fmsub.
_fmsub: macro
    dc.l (63*1<<(31-5))+(28*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fmsub_: macro
    dc.l (63*1<<(31-5))+(28*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fmsubs   / fmsubs.
_fmsubs: macro
    dc.l (59*1<<(31-5))+(28*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fmsubs_: macro
    dc.l (59*1<<(31-5))+(28*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fmul     / fmul.
_fmul: macro
    dc.l    (63*1<<(31-5))+(25*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))
    endm
_fmul_: macro
    dc.l    (63*1<<(31-5))+(25*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+1
    endm
;*-*
;; fmuls    / fmuls.
_fmuls: macro
    dc.l    (59*1<<(31-5))+(25*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))
    endm
_fmuls_: macro
    dc.l    (59*1<<(31-5))+(25*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+1
    endm
;*-*
;; fnabs    / fnabs.
_fnabs: macro
    dc.l (63*1<<(31-5))+(136*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fnabs_: macro
    dc.l (63*1<<(31-5))+(136*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fneg     / fneg.
_fneg: macro
    dc.l (63*1<<(31-5))+(40*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fneg_: macro
    dc.l (63*1<<(31-5))+(40*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fnmadd   / fnmadd.
_fnmadd: macro
    dc.l (63*1<<(31-5))+(31*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fnmadd_: macro
    dc.l (63*1<<(31-5))+(31*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fnmadds  / fnmadds.
_fnmadds: macro
    dc.l (59*1<<(31-5))+(31*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fnmadds_: macro
    dc.l (59*1<<(31-5))+(31*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fnmsub   / fnmsub.
_fnmsub: macro
    dc.l (63*1<<(31-5))+(30*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fnmsub_: macro
    dc.l (63*1<<(31-5))+(30*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fnmsubs  / fnmsubs.
_fnmsubs: macro
    dc.l (59*1<<(31-5))+(30*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fnmsubs_: macro
    dc.l (59*1<<(31-5))+(30*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fres     / fres.
_fres: macro
    dc.l (59*1<<(31-5))+(24*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fres_: macro
    dc.l (59*1<<(31-5))+(24*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; frsp     / frsp.
_frsp: macro
    dc.l (63*1<<(31-5))+(12*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_frsp_: macro
    dc.l (63*1<<(31-5))+(12*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fsqrte   / fsqrte.
_fsqrte: macro
    dc.l (63*1<<(31-5))+(26*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fsqrte_: macro
    dc.l (63*1<<(31-5))+(26*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fsel     / fsel.
_fsel: macro
    dc.l (63*1<<(31-5))+(23*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))
    endm
_fsel_: macro
    dc.l (63*1<<(31-5))+(3*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-25))+(\4*1<<(31-20))+1
    endm
;*-*
;; fsqrt    / fsqrt.
_fsqrt: macro
    dc.l (63*1<<(31-5))+(22*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fsqrt_: macro
    dc.l (63*1<<(31-5))+(22*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fsqrts   / fsqrts.
_fsqrts: macro
    dc.l (59*1<<(31-5))+(22*2)+(\1*1<<(31-10))+(\2*1<<(31-20))
    endm
_fsqrts_: macro
    dc.l (59*1<<(31-5))+(22*2)+(\1*1<<(31-10))+(\2*1<<(31-20))+1
    endm
;*-*
;; fsub     / fsub.
_fsub: macro
    dc.l (63*1<<(31-5))+(20*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_fsub_: macro
    dc.l (63*1<<(31-5))+(20*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; fsubs    / fsubs.
_fsubs: macro
    dc.l (59*1<<(31-5))+(20*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_fsubs_: macro
    dc.l (59*1<<(31-5))+(20*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))+1
    endm
;*-*
;; icbi
_icbi: macro
    dc.l    (31*1<<(31-5))+(982*2)+(\1*1<<(31-15))+(\2*1<<(31-20))
    endm
;*-*
;; isync
_isnyc: macro
    dc.l    (19*1<<(31-5))+(150*2)
    endm
;*-*
;; lbz      / lbzu      / lbzux     / lbzx
_lbz: macro
    dc.l    (34*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lbzu: macro
    dc.l    (35*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lbzux: macro
    dc.l    (31*1<<(31-5))+(119*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_lbzx: macro
    dc.l    (31*1<<(31-5))+(87*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; lfd      / lfdu      / lfdux     / lfdx
_lfd: macro
    dc.l    (50*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lfdu: macro
    dc.l    (51*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lfdux: macro
    dc.l    (31*1<<(31-5))+(631*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_lfdx: macro
    dc.l    (31*1<<(31-5))+(599*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; lfs      / lfsu      / lfsux     / lfsx
_lfs: macro
    dc.l    (48*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lfsu: macro
    dc.l    (49*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lfsux: macro
    dc.l    (31*1<<(31-5))+(567*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_lfsx: macro
    dc.l    (31*1<<(31-5))+(535*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; lha      / lhau      / lhaux     / lhax
_lha: macro
    dc.l    (42*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lhau: macro
    dc.l    (43*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lhaux: macro
    dc.l    (31*1<<(31-5))+(375*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_lhax: macro
    dc.l    (31*1<<(31-5))+(343*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; lhbrx
_lhbrx: macro
    dc.l    (31*1<<(31-5))+(790*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; lhz      / lhzu      / lhzux     / lhzx
_lhz: macro
    dc.l    (40*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lhzu: macro
    dc.l    (41*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lhzux: macro
    dc.l    (31*1<<(31-5))+(311*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_lhzx: macro
    dc.l    (31*1<<(31-5))+(279*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; lmw
_lmw: macro
    dc.l    (46*1<<(31-5))+(\1<<(31-10))+\2+(\3<<(31-15))
    endm
;*-*
;; lswi     / lswx
_lswi: macro
    dc.l    (31<<(31-5))+(597*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_lswx: macro
    dc.l    (31<<(31-5))+(533*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
;*-*
;; lwarx    / lwbrx
_lwarx: macro
    dc.l    (31<<(31-5))+(20*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_lwbrx: macro
    dc.l    (31<<(31-5))+(534*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
;*-*
;; lwz      / lwzu      / lwzux     / lwzx
_lwz: macro
    dc.l    (32*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lwzu: macro
    dc.l    (33*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_lwzux: macro
    dc.l    (31*1<<(31-5))+(55*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_lwzx: macro
    dc.l    (31*1<<(31-5))+(23*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; mcrf     / mcrfs     / mcrxr
_mcrf: macro
    dc.l    (19<<(31-5))+(\1>>2<<(31-8))+(\2>>2<<(31-13))
    endm
_mcrfs: macro
    dc.l    (63<<(31-5))+(64*2)+(\1>>2<<(31-8))+(\2>>2<<(31-13))
    endm
_mcrxr: macro
    dc.l    (31<<(31-5))+(512*2)+(\1>>2<<(31-8))
    endm
;*-*
;; mfcr     / mffs      / mffs.     / mfmsr
_mfcr: macro
    dc.l    (31<<(31-5))+(19*2)+(\1<<(31-10))
    endm
_mffs: macro
    dc.l    (63<<(31-5))+(583*2)+(\1<<(31-10))
    endm
_mffs_: macro
    dc.l    (63<<(31-5))+(583*2)+(\1<<(31-10))+1
    endm
_mfmsr: macro
    dc.l    (31<<(31-5))+(83*2)+(\1<<(31-10))
    endm
;*-*
;; mfspr    / mfsr      / mfsrin    / mftb
_mfspr: macro
    dc.l    (31<<(31-5))+(339*2)+(\1<<(31-10))+(\3<<(31-15))+(\2<<(31-20))
    endm
_mfsr: macro
    dc.l    (31<<(31-5))+(595*2)+(\1<<(31-10))+(\2<<(31-15))
    endm
_mfsrin: macro
    dc.l    (31<<(31-5))+(659*2)+(\1<<(31-10))+(\2<<(31-20))
    endm
_mftb: macro
    dc.l    (31<<(31-5))+(371*2)+(\1<<(31-10))+(\2<<(31-20))+(\3<<(31-15))
    endm
;*-*
;; mtcrf    / mtcr
_mtcrf: macro
    dc.l    (31<<(31-5))+(144*2)+(\2<<(31-10))+(\1<<(31-19))
    endm
_mtcr: macro
    _mtcrf 255,\1
;*-*
;; mtfsb0   / mtfsb0.   / mtfsb1    / mtfsb1.
_mtfsb0: macro
    dc.l    (63<<(31-5))+(70*2)+(\1<<(31-10))
    endm
_mtfsb0_: macro
    dc.l    (63<<(31-5))+(70*2)+(\1<<(31-10))+1
    endm
_mtfsb1: macro
    dc.l    (63<<(31-5))+(38*2)+(\1<<(31-10))
    endm
_mtfsb1_: macro
    dc.l    (63<<(31-5))+(38*2)+(\1<<(31-10))+1
    endm
;*-*
;; mtfsf    / mtfsf.    / mtfsfi    / mtfsfi.
_mtfsf: macro
    dc.l    (63<<(31-5))+(711*2)+(\1<<(31-14))+(\2<<(31-20))
    endm
_mtfsf_: macro
    dc.l    (63<<(31-5))+(711*2)+(\1<<(31-14))+(\2<<(31-20))+1
    endm
_mtfsfi: macro
    dc.l    (63<<(31-5))+(134*2)+(\1>>2<<(31-8))+(\2<<(31-19))
    endm
_mtfsfi_: macro
    dc.l    (63<<(31-5))+(134*2)+(\1>>2<<(31-8))+(\2<<(31-19))+1
    endm
;*-*
;; mtmsr
_mtmsr: macro
    dc.l    (31<<(31-5))+(146*2)+(\1<<(31-10))
    endm
;*-*
;; mtspr
_mtspr: macro
    dc.l    (31<<(31-5))+(467*2)+(\1<<(31-20))+(\2<<(31-15))+(\3<<(31-10))
    endm
;*-*
;; mtsr     / mtsrin
_mtsr: macro
    dc.l    (31<<(31-5))+(210*2)+(\1<<(31-15))+(\2<<(31-10))
    endm
_mtsrin: macro
    dc.l    (31<<(31-5))+(242*2)+(\1<<(31-10))+(\2<<(31-20))
    endm
;*-*
;; mulhw    / mulhw.    / mulhwu    / mulhwu.
_mulhw: macro
    dc.l    (31<<(31-5))+(75*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_mulhw_: macro
    dc.l    (31<<(31-5))+(75*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1
    endm
_mulhwu: macro
    dc.l    (31<<(31-5))+(11*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_mulhwu_: macro
    dc.l    (31<<(31-5))+(11*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1
    endm
;*-*
;; mulli
_mulli: macro
    dc.l    (7<<(31-5))+(\1<<(31-10))+(\2<<(31-15))+\3
    endm
;*-*
;; mullw    / mullwo    / mullw.    / mullwo.
_mullw: macro
    dc.l    (31<<(31-5))+(235*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_mullwo: macro
    dc.l    (31<<(31-5))+(235*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+(1<<(31-21))
    endm
_mullw_: macro
    dc.l    (31<<(31-5))+(235*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1
    endm
_mullwo_: macro
    dc.l    (31<<(31-5))+(235*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+(1<<(31-21))+1
    endm
;*-*
;; nand     / nand.
_nand: macro
    dc.l (31<<(31-5))+(476*2)+(\2<<(31-10))+(\1<<(31-15))+(\3<<(31-20))
    endm
_nand_: macro
    dc.l (31<<(31-5))+(476*2)+(\2<<(31-10))+(\1<<(31-15))+(\3<<(31-20))+1
    endm
;*-*
;; neg      / nego      / neg.      / nego.
_neg: macro
    dc.l    (31<<(31-5))+(104*2)+(\1<<(31-10))+(\2<<(31-15))
    endm
_nego: macro
    dc.l    (31<<(31-5))+(104*2)+(\1<<(31-10))+(\2<<(31-15))+(1<<(31-21))
    endm
_neg_: macro
    dc.l    (31<<(31-5))+(104*2)+(\1<<(31-10))+(\2<<(31-15))+1
    endm
_nego_: macro
    dc.l    (31<<(31-5))+(104*2)+(\1<<(31-10))+(\2<<(31-15))+(1<<(31-21))+1
    endm
;*-*
;; nor      / nor.
_nor: macro
    dc.l    (31<<(31-5))+(124*2)+(\2<<(31-10))+(\1<<(31-15))+(\3<<(31-20))
    endm
_nor_: macro
    dc.l    (31<<(31-5))+(124*2)+(\2<<(31-10))+(\1<<(31-15))+(\3<<(31-20))+1
    endm
;*-*
;; or       / or.
_or: macro
    dc.l    (31<<(31-5))+(444*2)+(\2<<(31-10))+(\1<<(31-15))+(\3<<(31-20))
    endm
_or_: macro
    dc.l    (31<<(31-5))+(444*2)+(\2<<(31-10))+(\1<<(31-15))+(\3<<(31-20))+1
    endm
;*-*
;; orc      / orc.
_orc: macro
    dc.l    (31<<(31-5))+(412*2)+(\2<<(31-10))+(\1<<(31-15))+(\3<<(31-20))
    endm
_orc_: macro
    dc.l    (31<<(31-5))+(412*2)+(\2<<(31-10))+(\1<<(31-15))+(\3<<(31-20))+1
    endm
;*-*
;; ori      / nop
_ori: macro
    dc.l (24<<(31-5))+(\2<<(31-10))+(\1<<(31-15))+\3
    endm
_nop: macro
    _ori 0,0,0
    endm
;*-*
;; oris
_oris: macro
    dc.l (25<<(31-5))+(\2<<(31-10))+(\1<<(31-15))+\3
    endm
;*-*
;; rfi
_rfi: macro
    dc.l    (19<<(31-5))+(50*2)
    endm
;*-*
;; rlwimi   / rlwimi.   / inslwi    / insrwi
_rlwimi: macro
    dc.l    (20<<(31-5))+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+(\4<<(31-25))+(\5<<(31-30))
    endm
_rlwimi_: macro
    dc.l    (20<<(31-5))+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+(\4<<(31-25))+(\5<<(31-30))+1
    endm
_inslwi: macro
    _rlwimi \1,\2,32-\4,\4,\4+\3-1
    endm
_insrwi: macro
    _rlwimi \1,\2,32-(\3+\4),\4,(\3+\4)-1
    endm
;*-*
;; rlwinm   / rlwinm.   / extlwi    / extrwi
_rlwinm: macro
    dc.l    (21<<(31-5))+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+(\4<<(31-25))+(\5<<(31-30))
    endm
_rlwinm_: macro
    dc.l    (21<<(31-5))+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+(\4<<(31-25))+(\5<<(31-30))+1
    endm
_extlwi: macro
    _rlwinm \1,\2,\4,0,(\3-1)
    endm
_extrwi: macro
    _rlwinm \1,\2,(\3+\4),32-\3,31
    endm
;*-*
;; rotlwi   / rotrwi    / slwi      / srwi
_rotlwi: macro
    _rlwinm \1,\2,\3,0,31
    endm
_rotrwi: macro
    _rlwinm \1,\2,(32-\3),0,31
    endm
_slwi: macro
    _rlwinm \1,\2,\3,0,(31-\3)
    endm
_srwi: macro
    _rlwinm \1,\2,(32-\3),\3,31
    endm
;*-*
;; clrlwi   / clrrwi    / clrlslwi
_clrlwi: macro
    _rlwinm \1,\2,0,\3,31
    endm
_clrrwi: macro
    _rlwinm \1,\2,0,0,(31-\3)
    endm
_clrlslwi: macro
    _rlwinm \1,\2,\4,(\3-\4),(31-\4)
    endm
;*-*
;; rlwnm    / rlwnm.    / rotlw
_rlwnm: macro
    dc.l (23<<(31-5))+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+(\4<<(31-25))+(\5<<(31-30))
    endm
_rlwnm_: macro
    dc.l (23<<(31-5))+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+(\4<<(31-25))+(\5<<(31-30))+1
    endm
_rotlw: macro
    _rlwnm \1,\2,\3,0,31
    endm
;*-*
;; sc
_sc: macro
    dc.l (17<<(31-5))+2
    endm
;*-*
;; slw      / slw.
_slw: macro
    dc.L (31<<(31-5))+(24*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))
    endm
_slw_: macro
    dc.L (31<<(31-5))+(24*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+1
    endm
;*-*
;; sraw     / sraw.     / srawi     / srawi.
_sraw: macro
    dc.l (31<<(31-5))+(792*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))
    endm
_sraw_: macro
    dc.l (31<<(31-5))+(792*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+1
    endm
_srawi: macro
    dc.l (31<<(31-5))+(824*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))
    endm
_srawi_: macro
    dc.l (31<<(31-5))+(824*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+1
    endm
;*-*
;; srw      / srw.
_srw: macro
_srw: macro
    dc.l (31<<(31-5))+(536*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))
    endm
_srw_: macro
    dc.l (31<<(31-5))+(536*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+1
    endm
;*-*
;; stb      / stbu      / stbux     / stbx
_stb: macro
    dc.l    (38*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_stbu: macro
    dc.l    (39*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_stbux: macro
    dc.l    (31*1<<(31-5))+(247*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_stbx: macro
    dc.l    (31*1<<(31-5))+(215*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; stfd     / stfdu     / stfdux    / stfdx
_stfd: macro
    dc.l    (54*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_stfdu: macro
    dc.l    (55*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_stfdux: macro
    dc.l    (31*1<<(31-5))+(759*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_stfdx: macro
    dc.l    (31*1<<(31-5))+(727*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; stfiwx
_stfiwx: macro
    dc.l (31<<(31-5))+(983*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
;*-*
;; stfs     / stfsu     / stfsux    / stfsx
_stfs: macro
    dc.l    (52*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_stfsu: macro
    dc.l    (53*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_stfsux: macro
    dc.l    (31*1<<(31-5))+(695*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_stfsx: macro
    dc.l    (31*1<<(31-5))+(663*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; sthbrx
_sthbrx: macro
    dc.l (31<<(31-5))+(918*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
;*-*
;; sth      / sthu      / sthux     / sthx
_sth: macro
    dc.l    (44*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_sthu: macro
    dc.l    (45*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_sthux: macro
    dc.l    (31*1<<(31-5))+(439*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_sthx: macro
    dc.l    (31*1<<(31-5))+(407*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; stmw
_stmw: macro
    dc.l (47<<(31-5))+(\1<<(31-10))+\2+(\3<<(31-15))
    endm
;*-*
;; stswi    / stswx
_stswi: macro
    dc.l (31<<(31-5))+(725*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_stswx: macro
    dc.l (31<<(31-5))+(661*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
;*-*
;; stw      / stwu      / stwux     / stwx
_stw: macro
    dc.l    (36*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_stwu: macro
    dc.l    (37*1<<(31-5))+(\1*1<<(31-10))+\2+(\3*1<<(31-15))
    endm
_stwux: macro
    dc.l    (31*1<<(31-5))+(183*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
_stwx: macro
    dc.l    (31*1<<(31-5))+(151*2)+(\1*1<<(31-10))+(\2*1<<(31-15))+(\3*1<<(31-20))
    endm
;*-*
;; stwbrx   / stwcx.
_stwbrx: macro
    dc.l (31<<(31-5))+(662*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_stwcx_: macro
    dc.l (31<<(31-5))+(150*2)+1+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
;*-*
;; subf     / subf.     / subfo     / subfo.    / sub
_subf: macro
    dc.l (31<<(31-5))+(40*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_subf_: macro
    dc.l (31<<(31-5))+(40*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1
    endm
_subfo: macro
    dc.l (31<<(31-5))+(40*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+(1<<(31-21))
    endm
_subfo_: macro
    dc.l (31<<(31-5))+(40*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1+(1<<(31-21))
    endm
_sub: macro
    _subf \1,\3,\2
    endm
;*-*
;; subfc    / subfc.    / subfco    / subfco.   / subc
_subfc: macro
    dc.l (31<<(31-5))+(8*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_subfc_: macro
    dc.l (31<<(31-5))+(8*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1
    endm
_subfco: macro
    dc.l (31<<(31-5))+(8*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+(1<<(31-21))
    endm
_subfco_: macro
    dc.l (31<<(31-5))+(8*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1+(1<<(31-21))
    endm
_subc: macro
    _subfc \1,\3,\2
    endm
;*-*
;; subfe    / subfe.    / subfeo    / subfeo.
_subfe: macro
    dc.l (31<<(31-5))+(136*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_subfe_: macro
    dc.l (31<<(31-5))+(136*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1
    endm
_subfeo: macro
    dc.l (31<<(31-5))+(136*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+(1<<(31-21))
    endm
_subfeo_: macro
    dc.l (31<<(31-5))+(136*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))+1+(1<<(31-21))
    endm
;*-*
;; subfic
_subfic: macro
    dc.l (8<<(31-5))+(\1<<(31-10))+(\2<<(31-15))+\3
    endm
;*-*
;; subfme   / subfme.   / subfmeo   / subfmeo.
_subfme: macro
    dc.l (31<<(31-5))+(232*2)+(\1<<(31-10))+(\2<<(31-15))
    endm
_subfme_: macro
    dc.l (31<<(31-5))+(232*2)+(\1<<(31-10))+(\2<<(31-15))+1
    endm
_subfmeo: macro
    dc.l (31<<(31-5))+(232*2)+(\1<<(31-10))+(\2<<(31-15))+(1<<(31-21))
    endm
_subfmeo_: macro
    dc.l (31<<(31-5))+(232*2)+(\1<<(31-10))+(\2<<(31-15))+1+(1<<(31-21))
    endm
;*-*
;; subfze   / subfze.   / subfzeo   / subfzeo.
_subfze: macro
    dc.l (31<<(31-5))+(200*2)+(\1<<(31-10))+(\2<<(31-15))
    endm
_subfze_: macro
    dc.l (31<<(31-5))+(200*2)+(\1<<(31-10))+(\2<<(31-15))+1
    endm
_subfzeo: macro
    dc.l (31<<(31-5))+(200*2)+(\1<<(31-10))+(\2<<(31-15))+(1<<(31-21))
    endm
_subfzeo_: macro
    dc.l (31<<(31-5))+(200*2)+(\1<<(31-10))+(\2<<(31-15))+1+(1<<(31-21))
    endm
;*-*
;; sync
_sync: macro
    dc.l (31<<(31-5))+(598*2)
    endm
;*-*
;; tlbia    / tlbie     / tlbsync
_tlbia: macro
    dc.l (31<<(31-5))+(370*2)
    endm
_tlbie: macro
    dc.l (31<<(31-5))+(306*2)+(\1<<(31-20))
    endm
_tlbsync: macro
    dc.l (31<<(31-5))+(566*2)
    endm
;*-*
;; tw       / twi
_tw: macro
    dc.l (31<<(31-5))+(4*2)+(\1<<(31-10))+(\2<<(31-15))+(\3<<(31-20))
    endm
_twi: macro
    dc.l (3<<(31-5))+(\1<<(31-10))+(\2<<(31-15))+\3
    endm
;*-*
;; xor      / xor.      / xori      / xoris
_xor: macro
    dc.l    (31<<(31-5))+(316*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))
    endm
_xor_: macro
    dc.l    (31<<(31-5))+(316*2)+(\1<<(31-15))+(\2<<(31-10))+(\3<<(31-20))+1
    endm
_xori: macro
    dc.l    (26<<(31-5))+(\1<<(31-15))+(\2<<(31-10))+\3
    endm
_xoris: macro
    dc.l    (27<<(31-5))+(\1<<(31-15))+(\2<<(31-10))+\3
    endm
;*-*

