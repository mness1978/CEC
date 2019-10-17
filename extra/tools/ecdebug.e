-> EC debugger

OBJECT debuginfo
  objs,procs,identhash,dbugval,dbugadr,modinfolist,codelist,memlist,heap,macrohash
ENDOBJECT

PROC main()
  DEF s[10]:STRING,c=1,port,db:PTR TO debuginfo
  WriteF('EC structure debugger\n')
  port:=FindPort('EmoduleCache')
  IF port=NIL
    WriteF('no Cache!\n')
    RETURN 10
  ENDIF
  WHILE c
    WriteF('show which datastructure? (<lf> to quit)\n')
    WriteF('["O"bjects,"P"procs,"I"dents,"V"alue,"A"dr,"M"odInfo,"C"odeRem,\n"K"ookiecheck,"H"eap,mac"R"os]: ')
    ReadStr(stdin,s)
    LowerStr(s)
    lf()
    c:=s[]
    db:=Long(port+42)
    do(c,db)
    lf()
  ENDWHILE
ENDPROC

PROC do(c,db:PTR TO debuginfo) HANDLE
  SELECT c
    CASE "o"; object(Long(db.objs))
    CASE "p"; proc(Long(db.procs))
    CASE "m"; modinfo(Long(db.modinfolist))
    CASE "c"; coderem(Long(db.codelist))
    CASE "k"; cookie(Long(db.memlist))
    CASE "h"; heaps(db.heap)
    CASE "i"; identhash(db.identhash)
    CASE "v"; val(db.dbugval)
    CASE "a"; mem(db.dbugadr)
    CASE "r"; macros(db.macrohash)
    CASE "\0"; WriteF('\ndone.\n')
    DEFAULT; WriteF('Unsupported datastructure\n')
  ENDSELECT
EXCEPT
  WriteF(IF exception="INV" THEN 'terminated because of invalid address\n' ELSE 'exception!')
ENDPROC

PROC valid(a)
  IF (a<$20000) OR (a>$A00000)        -> based on 2meg chip and 8meg fast
    IF a
      WriteF('[INVALID:$\z\h[8]]',a)
      RETURN FALSE
    ENDIF
  ENDIF
ENDPROC TRUE

PROC string(s) IS IF valid(s) THEN WriteF(IF s THEN 'str="\s"; ' ELSE 'str=NIL; ',s) ELSE 0
PROC str(name,s) IS IF valid(s) THEN IF s<>NIL THEN WriteF('\s="\s"; ',name,s) ELSE 0 ELSE 0
PROC flags(b) IS WriteF('flags=$\h; ',b)
PROC int(s,i) IS WriteF('\s=\d; ',s,i)
PROC lf() IS WriteF('\n')
PROC t() IS WriteF('  ')
PROC negnext(o) IS Long(o-4)

OBJECT objectheader
  ->onext:LONG			-> -4
  odel:INT			-> 0, delegate size
  otype:CHAR			-> 2, flags: bit 0=export
  dummy:CHAR			-> 3, _empty_spot_
  osize:INT			-> 4, sizeof object
  oid:INT			-> 6, object ID
  oascii:LONG			-> 8
  omemb:LONG			-> 12, memberlist
  omethod:LONG			-> 16, methodlist
  osuper:LONG			-> 20, superclass
  odcode			-> 24
  oacc				-> 28
  odeloff:INT			-> 32
  odestr:INT			-> 34
ENDOBJECT

PROC object(a:PTR TO objectheader)
  DEF s:PTR TO objectheader
  IF valid(a)
    IF a
      IF a.otype AND 2 = 0
        string(a.oascii)
        flags(a.otype)
        int('sizeof',a.osize)
        int('id',a.oid)
        int('delsize',a.odel)
        int('deloff',a.odeloff)
        int('destr',a.odestr)
        s:=a.osuper
        IF s THEN s:=s.oascii
        WriteF('superclass:')
        string(s)
        WriteF('\nmemberlist:\n')
        member(a.omemb)
        WriteF('methodlist:\n')
        method(a.omethod)
        lf()
        object(negnext(a))
      ENDIF
    ENDIF
  ENDIF
ENDPROC

OBJECT member
  -> onext:PTR TO member	-> -4
  ooff:INT			-> 0, offset
  oflags:CHAR			-> 2, SET PRIVATE,HASPTRTYPE
  dummy:CHAR			-> 3, _empty_spot_
  osize:INT			-> 4, fieldsize [1,2,4,0=array]
  oid:INT			-> 6, object ID
  oascii:LONG			-> 8, NIL if PRIVATE
  optrtype:LONG			-> 12, only if HASPTRTYPE
ENDOBJECT

PROC member(m:PTR TO member)
  IF m
    IF valid(m)
      t(); string(m.oascii)
      int('offset',m.ooff)
      int('size',m.osize)
      int('id',m.oid)
      flags(m.oflags)
      IF m.oflags AND 2
        IF m.optrtype AND $FFFFFFF0 = 0
          int('ptrtype',m.optrtype)
        ELSE
          WriteF('ptrtype=<object>')
        ENDIF
      ENDIF
      lf()
      member(negnext(m))
    ENDIF
  ENDIF
ENDPROC

OBJECT proc
  -> next:PTR TO proc		-> -4
  nrargs:INT			-> 0
  flags:CHAR			-> 2, SET COMPILE_RTD,METHOD
  numregvars:CHAR		-> 3, 0..3
  nrloc:INT			-> 4, -(nrloc*4)
  defargs:LONG			-> 6
  of_object:LONG		-> 10
  ident:LONG			-> 14
  self				-> 18
  method			-> 22
  regtab:PTR TO LONG		-> 26
ENDOBJECT

OBJECT method
  m_next:LONG			-> 0
  m_proc:LONG			-> 4
  m_type:CHAR			-> 8 [0=MT_METHOD, 1=MT_FROZEN, 2=MT_SHARED]
  m_flags:CHAR			-> 9 SET INHERITED
  m_off:INT			-> 10 delegate offset
  m_name:LONG			-> 12
ENDOBJECT

PROC method(m:PTR TO method)
  DEF p:PTR TO proc
  IF m
    IF valid(m)
      t(); string(m.m_name)
      int('type',m.m_type)
      flags(m.m_flags)
      int('deloff',m.m_off)
      WriteF(' -> proc: ')
      p:=m.m_proc
      int('nargs',p.nrargs)
      lf()
      method(m.m_next)
    ENDIF
  ENDIF
ENDPROC

PROC val(x)
  WriteF('value = \d, $\h\n',x,x)
ENDPROC

PROC mem(a:PTR TO LONG)
  DEF b,c
  c:=a
  WriteF('dump ($\h) = ',a)
  IF valid(a)
    FOR b:=1 TO 16 DO WriteF('\z$\h[8] ',a[]++)
  ENDIF
  WriteF('\n"\s"\n',c)
  lf()
ENDPROC

PROC identhash(p:PTR TO LONG)
  DEF x
  IF valid(p)
    IF p
      FOR x:=1 TO 256 DO IF p[] THEN ident(p[]++) ELSE p++
    ENDIF
  ENDIF
ENDPROC

OBJECT ident

-> flags = SET USED,SYS_VAR,EXPORT(in main var is uit module),REG,REAL|METHOD
-> etype = PTR TO [1,2,4] | PTR TO <object>
-> offset = +arg, -localvar

  ->next:LONG			-> -4
  etype:LONG			-> 0, [type,type,ascii]
  type:CHAR			-> 4, [local,global,lab] = [1,2,3]
  flags:CHAR			-> 5
  pr:LONG			-> 6, [proc,PTR TO globinfo|NIL,proc|NIL if lab]
  info:INT			-> 10, [offset/regnum,offset,ID=label]
  heavy:LONG			-> VARHEAVY, only for local+reg_alloc
ENDOBJECT

PROC ident(p:PTR TO ident)
  DEF t,n=NIL,pr=NIL,tn,f,fs[100]:STRING,type=NIL
  IF valid(p)
    IF (p.flags AND 2)=0 AND p
      t:=p.type
      f:=p.flags
      IF f AND 4 THEN StrAdd(fs,'EXPORT')
      IF f AND 8 THEN StrAdd(fs,'REG')
      IF f AND 16 THEN StrAdd(fs,'METHOD/REAL')
      IF EstrLen(fs)=0 THEN fs:=NIL
      SELECT t
        CASE 1; tn:='LOCAL'; pr:=Long(Long(p.pr+14))
        CASE 2; tn:='GLOB'
        CASE 3; tn:='PROC'; IF p.pr=NIL THEN tn:='LAB'; n:=p.etype
        DEFAULT; tn:='UNDEF'
      ENDSELECT
      IF (t=1) OR (t=2)
        type:=p.etype
        SELECT type
          CASE 1; type:=NIL
          CASE 2; type:='INT'
          CASE 4; type:='LONG'
          DEFAULT; type:=Long(p.etype+8)
        ENDSELECT
      ENDIF
      WriteF('  ')
      str('proc',pr)
      str('name',n)
      str('sort',tn)
      str('flags',fs)
      str('ptr_to',type)
      WriteF('info=\d; heavy=$\h\n',p.info,p.heavy)
      ident(negnext(p))
    ENDIF
  ENDIF
ENDPROC

PROC proc(p:PTR TO proc)
  DEF i:PTR TO ident, s:PTR TO objectheader, rt:PTR TO LONG
  IF valid(p)
    IF p
      WriteF('at:$\h; ',p)
      int('args',p.nrargs)
      int('locals',p.nrloc)
      int('numregvars',p.numregvars)
      flags(p.flags)
      WriteF('ident:')
      i:=p.ident
      IF i AND valid(i) THEN string(i.etype)
      IF i:=p.self
        IF valid(i)
          WriteF('\n    self:')
          IF valid(s:=i.etype) THEN string(s.oascii)
        ENDIF
      ENDIF
      lf()
      IF rt:=p.regtab
        WriteF('\tregalloc:')
        WHILE rt[]
          WriteF(' \d',Shr(Long(rt[]+12),8)-1)
          rt++
        ENDWHILE
        lf()
      ENDIF
      proc(negnext(p))
    ENDIF
  ENDIF
ENDPROC

OBJECT modinfo
  next,flags:INT,namelen,mod,list,name
ENDOBJECT

OBJECT procclass
  next,type:INT,info,acc
ENDOBJECT

OBJECT acc
  next,code
ENDOBJECT

PROC modinfo(p:PTR TO modinfo)
  IF valid(p)
    IF p
      str('mod',p.name)
      int('flags',p.flags)
      lf()
      procclass(p.list)
      modinfo(p.next)
    ENDIF
  ENDIF
ENDPROC

PROC procclass(p:PTR TO procclass)
  DEF x=0,l:PTR TO acc
  IF valid(p)
    IF p
      int('type',p.type)
      l:=p.acc
      WHILE l
        x++
        l:=l.next
      ENDWHILE
      IF p.type=2 THEN str('lab',Long(p.info))
      int('numacc',x)
      lf()
      procclass(p.next)
    ENDIF
  ENDIF
ENDPROC

OBJECT coderem
  next,type:INT,info1,info2
ENDOBJECT

PROC coderem(p:PTR TO coderem)
  IF valid(p)
    IF p
      int('type',p.type)
      IF (p.type=5) OR (p.type=6) THEN str('ref_name',p.info1)
      lf()
      coderem(p.next)
    ENDIF
  ENDIF
ENDPROC

PROC template(p:PTR TO proc)
  IF valid(p)
    IF p
      -> ...
      template(negnext(p))
    ENDIF
  ENDIF
ENDPROC

CONST COOKIE=$BE

OBJECT memlist
  next,size
ENDOBJECT

PROC cookie(meml:PTR TO memlist)
  DEF a
  WHILE meml
    IF valid(meml)
      a:=meml+meml.size-1
      IF a[]<>COOKIE
        DisplayBeep(NIL)
        DisplayBeep(NIL)
        DisplayBeep(NIL)
        WriteF('cookie check failed!!!!!!!!!\n')
        RETURN
      ENDIF
      meml:=meml.next
    ELSE
      RETURN
    ENDIF
  ENDWHILE
ENDPROC

PROC heaps(h:PTR TO LONG)
  WHILE CtrlC()=FALSE
    IF (h[2]-100>h[]) OR (h[2]+13000<h[])
      WriteF('!!!!\n')
      DisplayBeep(NIL)
      Delay(10)
    ENDIF
  ENDWHILE
ENDPROC

OBJECT macro
  next:PTR TO macro
  name,body
  nargs:INT
  flags:CHAR
ENDOBJECT

PROC macros(m:PTR TO LONG)
  DEF a,l:PTR TO macro
  FOR a:=0 TO 255
    l:=m[a]
    WHILE l
      WriteF('\s/\d = "\s"\n',l.name,l.nargs,l.body)
      l:=l.next
    ENDWHILE
  ENDFOR
ENDPROC
