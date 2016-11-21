      SUBROUTINE step
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006), to-8999
      STOP 'Invalid "to" reference in "step".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501), from- 9500
      STOP 'Invalid "from" reference in "step".'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=isto(env+6)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
                 ndx2=isto(env+7)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
      xprndx=isto(env+8)
      IF(.NOT.(xprndx.GT.0)) GOTO 5001
       to=9000
       CALL xpr1
       a=rsto(rptr)
       rptr=rptr-1
       ndx3=idnint(a)
       GOTO 5000
 5001 CONTINUE
       ndx3=iinf
 5000 CONTINUE
      stpl1=ndx1
      stpc1=ndx2
      stpi1=ndx3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(ishenv+10)
      useenv=0
      i1=isto(env+56)
      IF(.NOT.(i1.GT.0)) GOTO 5011
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=i1
 3001  IF(.NOT.(env.GT.0 .AND. useenv.EQ.0
     1 )) GOTO 3000
        IF(.NOT.(isto(env+3).EQ.ndx1
     1  .AND. isto(env+4).EQ.ndx2
     2  .AND. isto(env+5).EQ.ndx3
     3  )) GOTO 5021
         useenv=env
         GOTO 5020
 5021   CONTINUE
         i2=env
         env=isto(env+0)
 5020   CONTINUE
        GOTO 3001
 3000  CONTINUE
       IF(.NOT.(useenv.EQ.0)) GOTO 5031
        env=i2
        isto(env+0)=iptr+1
        GOTO 5030
 5031  CONTINUE
 5030  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5010
 5011 CONTINUE
       isto(env+56)=iptr+1
 5010 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(useenv.EQ.0)) GOTO 5041
       useenv=iptr+1
       to=9000
       CALL build1
       GOTO 5040
 5041 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=useenv
       isto(ishenv+61)=isto(env+6)
       isto(ishenv+62)=isto(env+7)
       env=isto(sptr)
       sptr=sptr+1
 5040 CONTINUE
      ndx3=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9001 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+11)
 3011 IF(.NOT.(env.GT.0)) GOTO 3010
       IF(.NOT.(isto(env+1).EQ.6)) GOTO 5051
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+2)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+0)
 3021   IF(.NOT.(env.GT.0)) GOTO 3020
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9501
         GOTO 9002
 9501    from=isto(sptr)
         sptr=sptr+1
         env=isto(env+0)
         GOTO 3021
 3020   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5050
 5051  CONTINUE
 5050  CONTINUE
       env=isto(env+0)
       GOTO 3011
 3010 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9002 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+2)
      stpl1=isto(env+6)
      stpc1=isto(env+7)
      ndx1=isto(env+8)
      ndx2=isto(env+9)
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      ndx3=ndx6-ndx4+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+17)
                 isto(ishenv+17)=datenv
      to=9002
      CALL xpr2
      isto(ishenv+17)=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+1)
      stpl1=isto(env+6)
      stpc1=isto(env+7)
      ndx1=isto(env+8)
      ndx2=isto(env+9)
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=datenv
                 datenv=isto(ishenv+11)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base3
                 base3=base13
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base5
                 base5=base15
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base4
                 base4=base14
      to=9037
      CALL xpr2
      xprcnt=ndx3
      to=9000
      CALL xpr2
      base4=isto(sptr)
      sptr=sptr+1
      base5=isto(sptr)
      sptr=sptr+1
      base3=isto(sptr)
      sptr=sptr+1
      datenv=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9003 CONTINUE
      GOTO 9500
 9004 CONTINUE
      GOTO 9500
 9005 CONTINUE
      GOTO 9500
 9006 CONTINUE
      GOTO 9500
      END
