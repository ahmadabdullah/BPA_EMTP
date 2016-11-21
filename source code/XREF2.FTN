      SUBROUTINE xref2
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010,
     1 9011, 9012, 9013, 9014, 9015,
     1 9016), to-8999
      STOP 'Invalid "to" reference in "xref2".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501,  9502,  9503,  9504,  9505,
     1  9506,  9507,  9508,  9509,  9510,
     1  9511,  9512,  9513,  9514,  9515,
     1  9516,  9517,  9518,  0002,  0002,
     1  0002,  0002,  9523,  9524,  9525), from- 9500
 0002 STOP 'Invalid "from" reference in "xref2".'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9501
      GOTO 9001
 9501 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9502
      GOTO 9003
 9502 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9503
      GOTO 9005
 9503 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9504
      GOTO 9006
 9504 from=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9001 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt
                 cnt=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
                 ndx5=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+35)
 3001 IF(.NOT.(env.GT.0)) GOTO 3000
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9525
        GOTO 9016
 9525   from=isto(sptr)
        sptr=sptr+1
       k=isto(env+1)
       IF(.NOT.(k.EQ.381 .OR. k.EQ.383)) GOTO 5001
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+2)
        ndx3=isto(env+7)
        ndx4=isto(env+8)
        ndx1=isto(env+1)
        env=isto(sptr)
        sptr=sptr+1
        ndx=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9505
        GOTO 9002
 9505   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(ndx.EQ.0)) GOTO 5011
         IF(.NOT.(ndx5.EQ.0)) GOTO 5221
          ndx5=1
          GOTO 5220
 5221    CONTINUE
 5220    CONTINUE
         isto(iptr+1)=ndx3
         isto(iptr+2)=ndx4
         isto(iptr+3)=ndx1
         iptr=iptr+3
         cnt=cnt+1
         isto(env+3)=cnt
         GOTO 5010
 5011   CONTINUE
         isto(env+3)=ndx
 5010   CONTINUE
        GOTO 5000
 5001  CONTINUE
 5000  CONTINUE
       env=isto(env+0)
       GOTO 3001
 3000 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+42)=cnt
      IF(.NOT.(cnt.GT.0)) GOTO 5021
       isto(env+43)=ptr1
       GOTO 5020
 5021 CONTINUE
       isto(env+43)=0
 5020 CONTINUE
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9002 CONTINUE
      k=ptr1-1
      i=1
 3011 IF(.NOT.(i.LE.cnt
     1.AND. isto(k+3*i).NE.ndx1
     2)) GOTO 3010
       i=i+1
       GOTO 3011
 3010 CONTINUE
      IF(.NOT.(i.LE.cnt)) GOTO 5031
       ndx=i
       GOTO 5030
 5031 CONTINUE
 5030 CONTINUE
      GOTO 9500
 9003 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt
                 cnt=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
                 ndx5=0
      isto(env+18)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+16)
 3021 IF(.NOT.(env.GT.0)) GOTO 3020
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+6)
       ndx0=isto(env+2)
       ndx3=isto(env+6)
       ndx4=isto(env+7)
       env=isto(sptr)
       sptr=sptr+1
       isto(env+1)=ndx0
       ndx=0
       IF(.NOT.(cnt.GT.0)) GOTO 5041
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=datenv
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9506
        GOTO 9004
 9506   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5040
 5041  CONTINUE
 5040  CONTINUE
       IF(.NOT.(ndx.GT.0)) GOTO 5051
        isto(env+2)=ndx
        GOTO 5050
 5051  CONTINUE
        IF(.NOT.(ndx5.EQ.0)) GOTO 5231
         ndx5=1
         GOTO 5230
 5231   CONTINUE
 5230   CONTINUE
        cnt=cnt+1
        isto(env+2)=cnt
        isto(iptr+1)=ndx3
        isto(iptr+2)=ndx4
        isto(iptr+3)=ndx0
        iptr=iptr+3
 5050  CONTINUE
       env=isto(env+0)
       GOTO 3021
 3020 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+17)=cnt
      IF(.NOT.(cnt.EQ.0)) GOTO 5061
       isto(env+18)=0
       GOTO 5060
 5061 CONTINUE
 5060 CONTINUE
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9004 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+18)
      i=1
 3031 IF(.NOT.(i.LE.cnt
     1.AND. isto(env+2).NE.ndx0)) GOTO 3030
       i=i+1
       env=env+3
       GOTO 3031
 3030 CONTINUE
      IF(.NOT.(i.LE.cnt)) GOTO 5071
       ndx=i
       GOTO 5070
 5071 CONTINUE
 5070 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9005 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+22)
 3041 IF(.NOT.(env.GT.0
     1.AND. stpflg.EQ.0)) GOTO 3040
       IF(.NOT.(isto(env+2).GT.0)) GOTO 5081
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+4)
        ndx0=isto(env+2)
        ndx3=isto(env+6)
        ndx4=isto(env+7)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=datenv
        cnt=isto(env+17)
        ndx=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9507
        GOTO 9004
 9507   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(ndx.GT.0)) GOTO 5091
         isto(env+3)=ndx
         GOTO 5090
 5091   CONTINUE
         stpflg=74
 5090   CONTINUE
        GOTO 5080
 5081  CONTINUE
        isto(env+3)=0
 5080  CONTINUE
       env=isto(env+0)
       GOTO 3041
 3040 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+19)
 3051 IF(.NOT.(env.GT.0
     1.AND. stpflg.EQ.0)) GOTO 3050
       IF(.NOT.(isto(env+2).GT.0)) GOTO 5101
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+3)
        ndx0=isto(env+2)
        ndx3=isto(env+6)
        ndx4=isto(env+7)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=datenv
        cnt=isto(env+17)
        ndx=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9508
        GOTO 9004
 9508   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(ndx.GT.0)) GOTO 5111
         isto(env+1)=ndx
         GOTO 5110
 5111   CONTINUE
         stpflg=76
 5110   CONTINUE
        GOTO 5100
 5101  CONTINUE
        isto(env+1)=0
 5100  CONTINUE
       env=isto(env+0)
       GOTO 3051
 3050 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+21)
 3061 IF(.NOT.(env.GT.0
     1.AND. stpflg.EQ.0)) GOTO 3060
       IF(.NOT.(isto(env+3).GT.0)) GOTO 5121
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+4)
        ndx0=isto(env+2)
        ndx3=isto(env+6)
        ndx4=isto(env+7)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+5)
        cnt=isto(env+17)
        ndx=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9509
        GOTO 9004
 9509   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(ndx.GT.0)) GOTO 5131
         isto(env+1)=ndx
         GOTO 5130
 5131   CONTINUE
         stpflg=75
 5130   CONTINUE
        GOTO 5120
 5121  CONTINUE
        isto(env+1)=0
 5120  CONTINUE
       env=isto(env+0)
       GOTO 3061
 3060 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+35)
 3071 IF(.NOT.(env.GT.0
     1.AND. stpflg.EQ.0)) GOTO 3070
       IF(.NOT.(isto(env+4).GT.0)) GOTO 5141
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+2)
        ndx0=isto(env+2)
        ndx3=isto(env+7)
        ndx4=isto(env+8)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+5)
        cnt=isto(env+17)
        ndx=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9510
        GOTO 9004
 9510   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(ndx.GT.0)) GOTO 5151
         isto(env+5)=ndx
         GOTO 5150
 5151   CONTINUE
         stpflg=77
 5150   CONTINUE
        GOTO 5140
 5141  CONTINUE
        isto(env+5)=0
 5140  CONTINUE
       env=isto(env+0)
       GOTO 3071
 3070 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+20)
 3081 IF(.NOT.(env.GT.0
     1.AND. stpflg.EQ.0)) GOTO 3080
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+2)
       ndx0=isto(env+2)
       ndx3=isto(env+6)
       ndx4=isto(env+7)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=datenv
       cnt=isto(env+17)
       ndx=0
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9511
       GOTO 9004
 9511  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(ndx.GT.0)) GOTO 5161
        isto(env+1)=ndx
        GOTO 5160
 5161  CONTINUE
        stpflg=78
 5160  CONTINUE
       env=isto(env+0)
       GOTO 3081
 3080 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(stpflg.GT.0)) GOTO 5171
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5170
 5171 CONTINUE
 5170 CONTINUE
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9006 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+41)
 3091 IF(.NOT.(env.GT.0)) GOTO 3090
       j=isto(env+2)
       k=j+isto(env+1)-1
       ptr1=iptr-1
       isto(env+9)=iptr+1
       iptr=iptr +2*isto(env+8)
       isto(ishenv+56)=0
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
 3101  IF(.NOT.(env.GT.0)) GOTO 3100
        k=isto(env+4)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+5)
        IF(.NOT.(k.EQ.1)) GOTO 5181
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9512
         GOTO 9007
 9512    from=isto(sptr)
         sptr=sptr+1
         GOTO 5180
 5181   IF(.NOT.(k.EQ.2)) GOTO 5182
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9513
         GOTO 9012
 9513    from=isto(sptr)
         sptr=sptr+1
         GOTO 5180
 5182   IF(.NOT.(k.EQ.3)) GOTO 5183
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9514
         GOTO 9013
 9514    from=isto(sptr)
         sptr=sptr+1
         GOTO 5180
 5183   CONTINUE
 5180   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        isto(env+2)=ndx1
        isto(env+3)=ndx2
        env=isto(env+0)
        GOTO 3101
 3100  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3091
 3090 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9007 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+0)
      ndx1=isto(env+2)
      ndx2=isto(env+8)
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+2)
      IF(.NOT.(ndx.EQ.2)) GOTO 5191
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9515
       GOTO 9008
 9515  from=isto(sptr)
       sptr=sptr+1
       GOTO 5190
 5191 IF(.NOT.(ndx.EQ.4)) GOTO 5192
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9516
       GOTO 9009
 9516  from=isto(sptr)
       sptr=sptr+1
       GOTO 5190
 5192 IF(.NOT.(ndx.EQ.5)) GOTO 5193
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9517
       GOTO 9010
 9517  from=isto(sptr)
       sptr=sptr+1
       GOTO 5190
 5193 IF(.NOT.(ndx.EQ.3 .OR. ndx.EQ.7)) GOTO 5194
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9518
       GOTO 9011
 9518  from=isto(sptr)
       sptr=sptr+1
       GOTO 5190
 5194 CONTINUE
 5190 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9008 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+0)
 3111 IF(.NOT.(env.GT.0)) GOTO 3110
       i1=isto(env+4)
       IF(.NOT.(i1.GT.0)) GOTO 5201
        ptr1=ptr1+2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=i1
        isto(ptr1)=isto(env+2)
        isto(ptr1+1)=isto(env+8)
        env=isto(sptr)
        sptr=sptr+1
        isto(ishenv+56)=isto(ishenv+56)+1
        isto(env+2)=isto(ishenv+56)
        GOTO 5200
 5201  CONTINUE
 5200  CONTINUE
       env=isto(env+0)
       GOTO 3111
 3110 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9009 CONTINUE
      ptr1=ptr1+2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+4)
      isto(ptr1)=isto(env+2)
      isto(ptr1+1)=isto(env+8)
      env=isto(sptr)
      sptr=sptr+1
      isto(ishenv+56)=isto(ishenv+56)+1
      isto(env+2)=isto(ishenv+56)
      GOTO 9500
 9010 CONTINUE
      ptr1=ptr1+2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+6)
      isto(ptr1)=isto(env+2)
      isto(ptr1+1)=isto(env+8)
      env=isto(sptr)
      sptr=sptr+1
      isto(ishenv+56)=isto(ishenv+56)+1
      isto(env+4)=isto(ishenv+56)
      GOTO 9500
 9011 CONTINUE
      ptr1=ptr1+2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=env+6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+15)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+2)
      isto(ptr1)=isto(env+1)
      isto(ptr1+1)=isto(env+9)
      env=isto(sptr)
      sptr=sptr+1
      isto(ishenv+56)=isto(ishenv+56)+1
      isto(env+8)=isto(ishenv+56)
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9012 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+4)
      ndx1=isto(env+2)
      ndx2=isto(env+8)
      env=isto(sptr)
      sptr=sptr+1
      ptr1=ptr1+2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+5)
      isto(ptr1)=isto(env+2)
      isto(ptr1+1)=isto(env+8)
      env=isto(sptr)
      sptr=sptr+1
      isto(ishenv+56)=isto(ishenv+56)+1
      isto(env+3)=isto(ishenv+56)
      GOTO 9500
 9013 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+4)
      ndx1=isto(env+2)
      ndx2=isto(env+8)
      env=isto(sptr)
      sptr=sptr+1
      ptr1=ptr1+2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+5)
      isto(ptr1)=isto(env+2)
      isto(ptr1+1)=isto(env+8)
      env=isto(sptr)
      sptr=sptr+1
      isto(ishenv+56)=isto(ishenv+56)+1
      isto(env+3)=isto(ishenv+56)
      GOTO 9500
 9014 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr0
                 ptr0=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9523
      GOTO 9015
 9523 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+8)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9524
      GOTO 9015
 9524 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9015 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+9)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+2)
 3121 IF(.NOT.(env.GT.0)) GOTO 3120
       IF(.NOT.(isto(env+8).EQ.0
     1 .AND. isto(env+5).EQ.ptr0
     2 )) GOTO 5241
        i5=isto(env+2)
        i4=isto(base4+i5)
        i6=isto(base6+i5)
        IF(.NOT.(i4.NE.1 .OR. i6.NE.1
     1  )) GOTO 5251
         stpflg=130
         stpi1=i4
         stpi2=i6
         stpl1=isto(env+6)
         stpc1=isto(env+7)
         CONTINUE
         CALL errstp
         GOTO 5250
 5251   CONTINUE
 5250   CONTINUE
        GOTO 5240
 5241  CONTINUE
 5240  CONTINUE
       env=isto(env+0)
       GOTO 3121
 3120 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+3)
 3131 IF(.NOT.(env.GT.0)) GOTO 3130
       i5=isto(env+1)
       IF(.NOT.(isto(env+9).EQ.0
     1 .AND. isto(env+5).EQ.ptr0
     2 .AND. isto(env+13).NE.7
     3 .AND. i5.GT.0
     4 )) GOTO 5261
        i4=isto(base4+i5)
        i6=isto(base6+i5)
        IF(.NOT.(i4.NE.1 .OR. i6.NE.1
     1  )) GOTO 5271
         stpflg=130
         stpi1=i4
         stpi2=i6
         stpl1=isto(env+7)
         stpc1=isto(env+8)
         CONTINUE
         CALL errstp
         GOTO 5270
 5271   CONTINUE
 5270   CONTINUE
        GOTO 5260
 5261  CONTINUE
 5260  CONTINUE
       env=isto(env+0)
       GOTO 3131
 3130 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9016 CONTINUE
      i0=isto(env+1)
      stpl1=isto(i0+1)
      stpc1=isto(i0)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+2)
      k=isto(env+2)
      IF(.NOT.(k.NE.6 .AND. k.NE.7
     1)) GOTO 5281
       stpflg=133
       stpl2=isto(env+7)
       stpc2=isto(env+8)
       CONTINUE
       CALL errstp
       GOTO 5280
 5281 CONTINUE
 5280 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
