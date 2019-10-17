/* ShowModule.e; dumps all the infos in a '.m' binary file */

ENUM JOB_DONE,JOB_CONST,JOB_OBJ,JOB_CODE,JOB_PROCS,
     JOB_SYS,JOB_LIB,JOB_RELOC,JOB_GLOBS,JOB_MODINFO,JOB_DEBUG,JOB_MACROS

ENUM ER_NONE,ER_FILE,ER_MEM,ER_USAGE,ER_JOBID,
     ER_BREAK,ER_FILETYPE,ER_TOONEW

CONST MODVERS=10,     -> upto which version we understand
      SKIPMARK=$FFFF8000

DEF flen,o:PTR TO INT,mem,handle=NIL,file[250]:STRING,thisvers=0,cmode=FALSE

PROC main() HANDLE
  DEF a,b,ae
  PrintF('ShowModule v1.\d (c) 1992 $#%!\n',MODVERS)
  IF StrCmp(arg,'',1) OR StrCmp(arg,'?',2)
    Raise(ER_USAGE)
  ELSE
    IF (arg[]="-") AND (arg[1]="c") AND (arg[2]=" ")
      arg:=arg+3
      WHILE arg[]=" " DO arg++
      cmode:=TRUE
    ENDIF
    ae:=arg+StrLen(arg)
    WHILE (ae[-1]=" ") AND (ae>arg) DO ae[]--:=0
    StrCopy(file,arg)
    LowerStr(file)
    IF (file[EstrLen(file)-2]<>".") OR (file[EstrLen(file)-1]<>"m") THEN StrAdd(file,'.m')
    PrintF('now showing: "\s"\n',file)
    PrintF('NOTE: don\at use this output in your code, use the module instead.\n\n')
    flen:=FileLength(file)
    handle:=Open(file,OLDFILE)
    IF (flen<8) OR (handle=NIL)
      Raise(ER_FILE)
    ELSE
      mem:=New(flen+10)
      IF mem=NIL
        Raise(ER_MEM)
      ELSE
        IF Read(handle,mem,flen)<>flen THEN Raise(ER_FILE)
        Close(handle)
        a:=mem+flen
        FOR b:=1 TO 6 DO a[]++:=0
        handle:=NIL
        process()
      ENDIF
    ENDIF
  ENDIF
EXCEPT
  IF handle THEN Close(handle)
  PrintF('\n')
  SELECT exception
    CASE ER_FILE;     PrintF('Could not read file "\s" !\n',file)
    CASE ER_MEM;      PrintF('No memory for loading module!\n')
    CASE ER_USAGE;    PrintF('USAGE: ShowModule [-c] <module>\n')
    CASE ER_JOBID;    PrintF('Illegal job id!\n')
    CASE ER_BREAK;    PrintF('User interupted ShowModule\n')
    CASE ER_FILETYPE; PrintF('Not an E module file.\n')
    CASE ER_TOONEW;   PrintF('You need a newer version of ShowModule to view this module\n')
  ENDSELECT
ENDPROC

PROC process()
  DEF end,job,len,val,f,off,types:PTR TO LONG,c,r,c2,l,narg,priv,darg:PTR TO LONG
  o:=mem
  end:=o+flen
  types:=['substructure','CHAR','INT','','LONG']
  IF ^o++<>"EMOD" THEN Raise(ER_FILETYPE)
  WHILE o<end
    IF CtrlC() THEN Raise(ER_BREAK)
    job:=o[]++
    SELECT job
      CASE JOB_CONST
        IF thisvers>=6 THEN o:=o+4
        len:=o[]++; f:=TRUE
        WHILE len
          val:=^o++
          PrintF(IF cmode THEN '#define ' ELSE IF f THEN 'CONST ' ELSE '      ')
          PrintF(IF cmode THEN '\s ' ELSE '\s=',o)
          IF (val>=-$20) AND (val<$20) THEN PrintF('\d',val) ELSE PrintF(IF cmode THEN '0x\h' ELSE '$\h',val)
          o:=o+len; len:=o[]++; f:=FALSE
          PrintF(IF len THEN (IF cmode THEN '\n' ELSE ',\n') ELSE '\n\n')
          IF CtrlC() THEN Raise(ER_BREAK)
        ENDWHILE
      CASE JOB_OBJ
        IF thisvers>=6 THEN o:=o+4
        priv:=0
        l:=o[]++;
        PrintF('(----) \s \s\s\n',IF cmode THEN 'struct' ELSE 'OBJECT',o+4,IF cmode THEN ' {' ELSE '')
        o:=o+4+l
        WHILE l:=o[]++
          val:=o[]++
          off:=o[]++
          IF l>0
            PrintF('(\d[4])   \s',off,o)
            o:=o+l
            priv:=0
          ELSE
            IF priv++=0 THEN PrintF('(----)   /* private member(s) here */\n')
          ENDIF
          IF thisvers>=6
            IF (c:=o[]++)>=0
              IF c=0
                PrintF(':\s\n',types[val])
              ELSE
                PrintF(IF val THEN '\s:PTR TO \s\n' ELSE '[\d]:ARRAY OF \s\n',IF val THEN '' ELSE Int(o+IF o[] THEN 4 ELSE 2)-off/c,ListItem(['','CHAR','INT','','LONG'],c))
              ENDIF
            ELSE
              l:=o[]++
              PrintF(IF val THEN ':PTR TO \s\n' ELSE ':\s (or ARRAY OF \s)\n',o,o)
              o:=o+l
            ENDIF
          ELSE
             PrintF(':\s\n',types[val])
          ENDIF
          IF CtrlC() THEN Raise(ER_BREAK)
        ENDWHILE
        val:=o[]++
        IF thisvers>=7
          IF o[]++
            o:=o+4
            l:=o[]++
            o:=o+l+4
            WHILE (c:=o[]++)<>-1
              o++; l:=o[]++
              PrintF('         \s(',o)
              o:=o+l
              IF l:=o[]++ THEN FOR off:=1 TO l DO PrintF(IF off=l THEN '\c' ELSE '\c,',off+96)
              PutStr(')\n')
              l:=o[]++; o:=l*4+o
            ENDWHILE
            WHILE o[]++<>-1 DO o:=o+4
          ENDIF
        ENDIF
        PrintF('(----) \s     /* SIZEOF=',IF cmode THEN '}' ELSE 'ENDOBJECT')
        PrintF(IF val<>-1 THEN '\d */\n\n' ELSE 'NONE !!! */\n\n',val)
      CASE JOB_CODE
        l:=^o++*4
        PrintF('/* this module contains \d bytes of code! */\n\n',l)
        o:=l+o
      CASE JOB_PROCS
        WHILE (l:=o[]++)>0
          c:=o
          o:=o+l+4
          IF o[]++=1
            PrintF('PROC \s(',c)
            narg:=o[]++
            o++
            c2:=o[]++
            darg:=o
            o:=c2*4+o
            c:=o[]++
            IF c
              IF c2
                l:=o
                FOR r:=1 TO narg
                  WHILE l[]>"0" DO FputC(stdout,l[]++)
                  IF narg-r<c2 THEN PrintF('=\d',darg[]++)
                  PutStr(IF r<>narg THEN ',' ELSE ')\n')
                  l++
                ENDFOR
              ELSE
                PrintF('\s)\n',o)
              ENDIF
            ELSE
              IF narg THEN FOR c2:=1 TO narg DO PrintF(IF c2=narg THEN '\c' ELSE '\c,',c2+96)
              PrintF(')\n')
            ENDIF
            o:=o+c
          ELSE
            PrintF('\s:\n',c)
          ENDIF
        ENDWHILE
        PrintF('\n')
      CASE JOB_SYS
        o:=o+4
        f:=FALSE
        IF c:=o[]++
          f:=TRUE
          PrintF('/* osvers: \d+  ',c)
        ENDIF
        o:=o+4
        IF c:=o[]++
          IF f=FALSE THEN PrintF('/* ')
          f:=TRUE
          PrintF('cpu: \s+  ',ListItem(['68020/030','68040/060'],c-1))
        ENDIF
        IF c:=o[]++
          IF f=FALSE THEN PrintF('/* ')
          f:=TRUE
          PrintF('fpu: \s+  ',ListItem(['68881/2','68040/060'],c-1))
        ENDIF
        o:=o+2
        IF (thisvers:=o[]++)>MODVERS THEN Raise(ER_TOONEW)
        o:=o+4
        IF f THEN PrintF('*/\n\n')
      CASE JOB_LIB
        c:=o
        WHILE c[]++ DO NOP
        PrintF(IF cmode THEN '##base _\s\n##bias 30\n##public\n' ELSE 'LIBRARY \s         /* informal notation */\n',c)
        WHILE c[]++ DO NOP
        off:=-30
        WHILE (c[]<>$FF) AND (c<end)
          IF c[]=16
            INC c
          ELSE
            c2:=c
            WHILE c[]++>" " DO NOP; c--
            r:=c[]; c[]++:=0
            PrintF(IF cmode THEN '\s' ELSE '  \s',c2)
            IF cmode THEN dargs(r,c)
            PrintF('(')
            IF r<>16
              WHILE r<16
                IF r<8 THEN PrintF('D\d',r) ELSE PrintF('A\d',r-8)
                r:=c[]++
                IF r<16 THEN PrintF(',')
              ENDWHILE
              c--
            ENDIF
            PrintF(IF cmode THEN ')\n' ELSE ')     /* \d (\h) */\n',off,Abs(off))
          ENDIF
          off:=off-6
        ENDWHILE
        PrintF(IF cmode THEN '##end\n\n' ELSE 'ENDLIBRARY\n\n')
        o:=end
      CASE JOB_RELOC
        c:=^o++
        o:=c*4+o
        PrintF('/* ... and \d reloc entries */\n\n',c)
      CASE JOB_DONE
        o:=end
      CASE JOB_GLOBS
        c:=0; f:=TRUE
        IF o[]=SKIPMARK THEN o:=o+6
        WHILE (len:=o[]++)>=0
          IF len
            IF f
              PrintF('DEF ')
              f:=FALSE
            ELSE
              PrintF(',')
            ENDIF
            PrintF('\s',o)
            o:=o+len
          ELSE
            c++
          ENDIF
          WHILE ^o++ DO IF thisvers>=10 THEN o++
        ENDWHILE
        IF f=FALSE THEN PrintF('\n')
        IF c THEN PrintF('/* \d private global variable(s) in this module */\n',c)
        PrintF('\n')
      CASE JOB_MODINFO
        o:=o+4
        PutStr('/*\n')
        WHILE len:=o[]++
          PrintF('  code from module "\s" used:\n',o)
          o:=o+len
          WHILE c:=o[]++
            len:=o[]++
            c2:=o
            o:=o+len
            IF c=2
              f:=o[]++
              PrintF(IF f<>-1 THEN '    \s\c)/\d' ELSE '    \s:',c2,"(",f)
              PrintF(' (\dx)\n',c:=o[]++)
              o:=c*4+o
            ELSE
              c:=o[]++
              PrintF('  OBJECT \s [\d acc]\n',c2,c)
              o:=c*6+o
            ENDIF
          ENDWHILE
        ENDWHILE
        PutStr('*/\n')
      CASE JOB_DEBUG
        WHILE ^o++=$3F1
          len:=^o++
          o:=o+4
          c:=^o++
          o:=len*4+o-8
          PrintF('/* This module contains \d bytes \s DEBUG infos! */\n\n',
            len*4,IF c="EVAR" THEN 'EVAR' ELSE 'LINE')
        ENDWHILE
      CASE JOB_MACROS
        WHILE len:=o[]++
          PrintF('#define \s',o)
          o:=o+len
          PrintF('/\d\n',o[]++)
          o++
          o:=o[]+++o
        ENDWHILE
        PutStr('\n')
      DEFAULT
        Raise(ER_JOBID)
    ENDSELECT
  ENDWHILE
ENDPROC

PROC dargs(r,c)
  DEF ch="a"
  PrintF('(')
  IF r<>16
    WHILE r<16
      PrintF('\c',ch++)
      r:=c[]++
      IF r<16 THEN PrintF(',')
    ENDWHILE
  ENDIF
  PrintF(')')
ENDPROC
