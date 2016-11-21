      SUBROUTINE xpr1
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      isto(sptr)=from
                 from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010,
     1 9011, 9012, 9013, 9014, 9015,
     1 9016, 9017, 9018, 9019, 9020,
     1 9021, 9022, 9023, 9024, 9025,
     1 9026, 9027, 9028), to-8999
      STOP 'Invalid "to" reference in "xpr1".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501,  9502,  9503,  9504,  9505,
     1  9506,  9507,  9508,  9509,  9510,
     1  9511,  9512,  9513,  9514,  9515,
     1  9516,  9517,  9518,  9519,  9520,
     1  9521,  9522,  9523,  9524,  9525,
     1  9526,  9527,  9528,  9529,  9530,
     1  9531,  9532,  9533,  9534,  9535,
     1  9536,  9537,  9538,  9539,  9540,
     1  9541,  9542,  9543,  9544,  9545,
     1  9546,  9547,  9548,  9549,  9550,
     1  9551,  9552,  9553,  9554,  9555), from-9500
      STOP 'Invalid "from" reference in "xpr1".'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
  930 stpflg=44
      stpi1=rptr
      stpi2=rlen-rptr
      CONTINUE
      CALL errstp
 9000 CONTINUE
      IF(sptr-100.LE.iptr) GOTO 910
      IF(rptr+100.GE.rsptr) GOTO 930
      sptr=sptr-1
      isto(sptr)=env
                 env=xprndx
      sptr=sptr-1
      isto(sptr)=xprknd
                 xprknd=isto(env+1)
      sptr=sptr-1
      isto(sptr)=xprsub
                 xprsub=isto(env+2)
      sptr=sptr-1
      isto(sptr)=cnt0
                 cnt0=0
      sptr=sptr-1
      isto(sptr)=ptr0
                 ptr0=rptr
      sptr=sptr-1
      isto(sptr)=cnt1
      k=isto(env+3)
      sptr=sptr-1
      isto(sptr)=env
                 env=env+4
      IF(.NOT.(k.GT.0)) GOTO 6041
       sptr=sptr-1
       isto(sptr)=cnt6
       sptr=sptr-1
       isto(sptr)=from
       from=9555
       GOTO 9008
 9555  from=isto(sptr)
       sptr=sptr+1
       cnt0=cnt6
       cnt6=isto(sptr)
       sptr=sptr+1
       GOTO 6040
 6041 CONTINUE
 3001  IF(.NOT.(env.GT.0)) GOTO 3000
        sptr=sptr-1
        isto(sptr)=from
        from=9501
        GOTO 9001
 9501   from=isto(sptr)
        sptr=sptr+1
        to=9003
        CALL xpr2
        env=isto(env+0)
        GOTO 3001
 3000  CONTINUE
 6040 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(isto(env+0).EQ.2)) GOTO 5001
       i=0
 3011  IF(.NOT.(i.LT.cnt0)) GOTO 3010
        i=i +1
        IF(.NOT.(rsto(ptr0+i).GT.rmargn)) GOTO 5011
         a=one
         GOTO 5010
 5011   CONTINUE
         a=zero
 5010   CONTINUE
        rsto(ptr0+i)=a
        GOTO 3011
 3010  CONTINUE
       GOTO 5000
 5001 CONTINUE
 5000 CONTINUE
      xprcnt=cnt0
      cnt1=isto(sptr)
      ptr0=isto(sptr+1)
      cnt0=isto(sptr+2)
      xprsub=isto(sptr+3)
      xprknd=isto(sptr+4)
      env=isto(sptr+5)
      sptr=sptr+6
      GOTO 9500
 9001 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      isto(sptr)=cnt2
      cnt1=0
      sptr=sptr-1
      isto(sptr)=env
                 env=env+1
 3021 IF(.NOT.(env.GT.0)) GOTO 3020
       sptr=sptr-1
       isto(sptr)=from
       from=9502
       GOTO 9002
 9502  from=isto(sptr)
       sptr=sptr+1
       to=9004
       CALL xpr2
       env=isto(env+0)
       GOTO 3021
 3020 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      ptr1=isto(sptr+1)
      sptr=sptr+2
      GOTO 9500
 9002 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr2
                 ptr2=rptr
      sptr=sptr-1
      isto(sptr)=cnt3
      sptr=sptr-1
      isto(sptr)=env
                 env=env+3
      sptr=sptr-1
      isto(sptr)=from
      from=9503
      GOTO 9003
 9503 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      cnt2=cnt3
      i=isto(env+1)
      IF(.NOT.(i.GT.0)) GOTO 5021
       sptr=sptr-1
       isto(sptr)=ndx
       sptr=sptr-1
       isto(sptr)=env
                  env=i
       sptr=sptr-1
       isto(sptr)=from
       from=9504
       GOTO 9003
 9504  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       rptr=rptr-cnt3
       ndx=isto(env+2)
       IF(.NOT.(cnt2.EQ.1)) GOTO 5031
        a=rsto(ptr2+1)
        i=0
 3031   IF(.NOT.(i.LT.cnt3)) GOTO 3030
         i=i+1
         b=rsto(rptr+i)
         c=zero
         to=9005
         CALL xpr2
         rsto(ptr2+i)=c
         GOTO 3031
 3030   CONTINUE
        cnt2=cnt3
        GOTO 5030
 5031  IF(.NOT.(cnt3.EQ.1)) GOTO 5032
        b=rsto(rptr+1)
        i=0
 3041   IF(.NOT.(i.LT.cnt2)) GOTO 3040
         i=i+1
         a=rsto(ptr2+i)
         c=zero
         to=9005
         CALL xpr2
         rsto(ptr2+i)=c
         GOTO 3041
 3040   CONTINUE
        GOTO 5030
 5032  CONTINUE
        c=zero
        IF(.NOT.(cnt2.NE.cnt3)) GOTO 5041
         IF(.NOT.(ndx.EQ.2)) GOTO 5051
          c=one
          GOTO 5050
 5051    CONTINUE
 5050    CONTINUE
         GOTO 5040
 5041   CONTINUE
         i=0
         k=1
 3051    IF(.NOT.(k.GT.0 .AND. i.LT.cnt2
     1   ))GOTO 3050
          i=i+1
          a=rsto(ptr2+i)
          b=rsto(rptr+i)
          p=dmax1(rmargn,dabs(rmargn*b))
          IF(.NOT.(a.LT.b-p .OR. a.GT.b+p
     1    )) GOTO 5061
           k=0
           GOTO 5060
 5061     CONTINUE
 5060     CONTINUE
          GOTO 3051
 3050    CONTINUE
         IF(.NOT.((k.GT.0 .AND. ndx.EQ.1)
     1   .OR. (k.EQ.0 .AND. ndx.EQ.2)
     2   )) GOTO 5071
          c=one
          GOTO 5070
 5071    CONTINUE
 5070    CONTINUE
 5040   CONTINUE
        rsto(ptr2+1)=c
        cnt2=1
 5030  CONTINUE
       rptr=ptr2+cnt2
       ndx=isto(sptr)
       sptr=sptr+1
       GOTO 5020
 5021 CONTINUE
 5020 CONTINUE
      cnt3=isto(sptr)
      ptr2=isto(sptr+1)
      sptr=sptr+2
      GOTO 9500
 9003 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr3
                 ptr3=rptr
      sptr=sptr-1
      isto(sptr)=cnt4
                 cnt3=0
      sptr=sptr-1
      isto(sptr)=env
                 env=env+2
 3061 IF(.NOT.(env.GT.0)) GOTO 3060
       sptr=sptr-1
        isto(sptr)=from
       from=9505
       GOTO 9006
 9505  from=isto(sptr)
       sptr=sptr+1
       to=9006
       CALL xpr2
       env=isto(env+0)
       GOTO 3061
 3060 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      isto(sptr)=from
      from=9506
      GOTO 9004
 9506 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      isto(sptr)=from
      from=9507
      GOTO 9005
 9507 from=isto(sptr)
      sptr=sptr+1
      cnt4=isto(sptr)
      ptr3=isto(sptr+1)
      sptr=sptr+2
      GOTO 9500
 9004 CONTINUE
      xprndx=isto(env+0)
      IF(.NOT.(xprndx.GT.0)) GOTO 5081
       sptr=sptr-1
       isto(sptr)=from
       from=9508
       GOTO 9000
 9508  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       i=0
       IF(.NOT.(xprcnt.EQ.1)) GOTO 5091
        a=rsto(rptr+1)
        p=dmax1(rmargn,dabs(rmargn*a))
 3071   IF(.NOT.(i.LT.cnt3)) GOTO 3070
         i=i+1
         IF(.NOT.(rsto(ptr3+i).LT.a-p
     1   .AND. rsto(ptr3+i).NE.rnull
     1   )) GOTO 5101
          rsto(ptr3+i)=a
          GOTO 5100
 5101    CONTINUE
 5100    CONTINUE
         GOTO 3071
 3070   CONTINUE
        GOTO 5090
 5091  CONTINUE
        IF(.NOT.(xprcnt.NE.cnt3)) GOTO 5111
         stpflg=33
         stpi1=cnt3
         stpi2=xprcnt
         stpi3=xprknd
         stpi4=xprsub
         CONTINUE
         CALL errstp
         GOTO 5110
 5111   CONTINUE
 3081    IF(.NOT.(i.LT.cnt3)) GOTO 3080
          i=i+1
          a=rsto(rptr+i)
          p=dmax1(rmargn,dabs(rmargn*a))
          IF(.NOT.(rsto(ptr3+i).LT.a-p
     1    .AND. rsto(ptr3+i).NE.rnull
     1    )) GOTO 5121
           rsto(ptr3+i)=a
           GOTO 5120
 5121     CONTINUE
 5120     CONTINUE
          GOTO 3081
 3080    CONTINUE
 5110   CONTINUE
 5090  CONTINUE
       GOTO 5080
 5081 CONTINUE
 5080 CONTINUE
      GOTO 9500
 9005 CONTINUE
      xprndx=isto(env+1)
      IF(.NOT.(xprndx.GT.0)) GOTO 5131
       sptr=sptr-1
       isto(sptr)=from
       from=9509
       GOTO 9000
 9509  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       i=0
       IF(.NOT.(xprcnt.EQ.1)) GOTO 5141
        a=rsto(rptr+1)
        p=dmax1(rmargn,dabs(rmargn*a))
 3091   IF(.NOT.(i.LT.cnt3)) GOTO 3090
         i=i+1
         IF(.NOT.(rsto(ptr3+i).GT.a+p
     1   .AND. rsto(ptr3+i).NE.rnull
     1   )) GOTO 5151
          rsto(ptr3+i)=a
          GOTO 5150
 5151    CONTINUE
 5150    CONTINUE
         GOTO 3091
 3090   CONTINUE
        GOTO 5140
 5141  CONTINUE
        IF(.NOT.(xprcnt.NE.cnt3)) GOTO 5161
         stpflg=34
         stpi1=cnt3
         stpi2=xprcnt
         stpi3=xprknd
         stpi4=xprsub
         CONTINUE
         CALL errstp
         GOTO 5160
 5161   CONTINUE
 3101    IF(.NOT.(i.LT.cnt3)) GOTO 3100
          i=i+1
          a=rsto(rptr+i)
          p=dmax1(rmargn,dabs(rmargn*a))
          IF(.NOT.(rsto(ptr3+i).GT.a+p
     1    .AND. rsto(ptr3+i).NE.rnull
     1    )) GOTO 5171
           rsto(ptr3+i)=a
           GOTO 5170
 5171     CONTINUE
 5170     CONTINUE
          GOTO 3101
 3100    CONTINUE
 5160   CONTINUE
 5140  CONTINUE
       GOTO 5130
 5131 CONTINUE
 5130 CONTINUE
      GOTO 9500
 9006 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr4
                 ptr4=rptr
      sptr=sptr-1
      isto(sptr)=cnt5
      cnt4=0
      sptr=sptr-1
      isto(sptr)=env
                 env=env+2
 3111 IF(.NOT.(env.GT.0)) GOTO 3110
       sptr=sptr-1
       isto(sptr)=from
       from=9510
       GOTO 9007
 9510  from=isto(sptr)
       sptr=sptr+1
       to=9007
       CALL xpr2
       env=isto(env+0)
       GOTO 3111
 3110 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(isto(env+1).EQ.2
     1)) GOTO 5181
       i=0
 3121  IF(.NOT.(i.LT.cnt4)) GOTO 3120
        i=i+1
        rsto(ptr4+i)= -rsto(ptr4+i)
        GOTO 3121
 3120  CONTINUE
       GOTO 5180
 5181 CONTINUE
 5180 CONTINUE
      cnt5=isto(sptr)
      ptr4=isto(sptr+1)
      sptr=sptr+2
      GOTO 9500
 9007 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr5
                 ptr5=rptr
      sptr=sptr-1
      isto(sptr)=cnt6
      sptr=sptr-1
      isto(sptr)=env
                 env=env+4
      sptr=sptr-1
      isto(sptr)=from
      from=9511
      GOTO 9008
 9511 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      cnt5=cnt6
      i=isto(env+3)
      IF(.NOT.(i.GT.0)) GOTO 5191
       sptr=sptr-1
       isto(sptr)=env
                  env=i
       sptr=sptr-1
       isto(sptr)=from
       from=9512
       GOTO 9008
 9512  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       isto(sptr)=ndx1
                  ndx1=isto(env+2)
       rptr=rptr -cnt6
       i=0
       IF(.NOT.(cnt5.EQ.1)) GOTO 5201
        a=rsto(rptr)
 3131   IF(.NOT.(i.LT.cnt6)) GOTO 3130
         i=i+1
         b=rsto(rptr+i)
         IF(.NOT.(ndx1.EQ.3)) GOTO 5211
          to=9008
          CALL xpr2
          GOTO 5210
 5211    CONTINUE
          to=9009
          CALL xpr2
 5210    CONTINUE
         rsto(ptr5+i)=c
         GOTO 3131
 3130   CONTINUE
        cnt5=cnt6
        GOTO 5200
 5201  IF(.NOT.(cnt6.EQ.1)) GOTO 5202
        b=rsto(rptr+1)
 3141   IF(.NOT.(i.LT.cnt5)) GOTO 3140
         i=i+1
         a=rsto(ptr5+i)
         IF(.NOT.(ndx1.EQ.3)) GOTO 5221
          to=9008
          CALL xpr2
          GOTO 5220
 5221    CONTINUE
          to=9009
          CALL xpr2
 5220    CONTINUE
         rsto(ptr5+i)=c
         GOTO 3141
 3140   CONTINUE
        GOTO 5200
 5202  CONTINUE
        IF(.NOT.(ndx1.EQ.3)) GOTO 5231
         stpflg=80
         GOTO 5230
 5231   CONTINUE
         stpflg=81
 5230   CONTINUE
        stpi1=cnt5
        stpi2=cnt6
        CONTINUE
        CALL errstp
 5200  CONTINUE
       rptr=ptr5+cnt5
       ndx1=isto(sptr)
       sptr=sptr+1
       GOTO 5190
 5191 CONTINUE
 5190 CONTINUE
      IF(.NOT.(isto(env+1).EQ.2)) GOTO 5241
       i=0
 3151  IF(.NOT.(i.LT.cnt5)) GOTO 3150
        i=i+1
        a=rsto(ptr5+i)
        rptr=rptr+1
        rsto(rptr)=a
        to=9010
        CALL xpr2
        a=rsto(rptr)
        rptr=rptr-1
        rsto(ptr5+i)=a
        GOTO 3151
 3150  CONTINUE
       GOTO 5240
 5241 CONTINUE
 5240 CONTINUE
      cnt6=isto(sptr)
      ptr5=isto(sptr+1)
      sptr=sptr+2
      GOTO 9500
 9008 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr6
                 ptr6=rptr
      cnt6=0
      i=isto(env+0)
      sptr=sptr-1
      isto(sptr)=env
                 env=env+2
      IF(.NOT.(i.EQ.1)) GOTO 5251
       sptr=sptr-1
       isto(sptr)=env
                  env=isto(env+0)
 3161  IF(.NOT.(env.GT.0)) GOTO 3160
        xprndx=env+1
        sptr=sptr-1
        isto(sptr)=from
        from=9513
        GOTO 9000
 9513   from=isto(sptr)
        sptr=sptr+1
        cnt6=cnt6 +xprcnt
        env=isto(env+0)
        GOTO 3161
 3160  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5250
 5251 IF(.NOT.(i.EQ.2)) GOTO 5252
       rptr=rptr+1
       cnt6=1
       j=isto(env+2)
       rsto(rptr)=rsto(j)
       GOTO 5250
 5252 IF(.NOT.(i.EQ.3)) GOTO 5253
       xprndx=isto(env+0)
       sptr=sptr-1
       isto(sptr)=from
       from=9514
       GOTO 9000
 9514  from=isto(sptr)
       sptr=sptr+1
       cnt6=xprcnt
       GOTO 5250
 5253 CONTINUE
       sptr=sptr-1
       isto(sptr)=from
       from=9515
       GOTO 9009
 9515  from=isto(sptr)
       sptr=sptr+1
 5250 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      k=isto(env+1)
      IF(.NOT.(k.GT.0)) GOTO 5261
       i=0
 3171  IF(.NOT.(i.LT.cnt6)) GOTO 3170
        i=i+1
        a=rsto(ptr6+i)
        IF(.NOT.(k.EQ.1)) GOTO 5271
         IF(.NOT.(a.GT.rmargn)) GOTO 5281
          a=zero
          GOTO 5280
 5281    CONTINUE
          a=one
 5280    CONTINUE
         GOTO 5270
 5271   IF(.NOT.(k.EQ.2)) GOTO 5272
         a=-a
         GOTO 5270
 5272   CONTINUE
 5270   CONTINUE
        rsto(ptr6+i)=a
        GOTO 3171
 3170  CONTINUE
       GOTO 5260
 5261 CONTINUE
 5260 CONTINUE
      ptr6=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9009 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx1
                 ndx1=isto(env+2)
      sptr=sptr-1
      isto(sptr)=ndx2
                 ndx2=isto(env+3)
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
                 ndx5=isto(env+1)
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=stpl1
      stpl1=isto(env+7)
      sptr=sptr-1
      isto(sptr)=stpc1
      stpc1=isto(env+8)
      k=isto(env+13)
      cnt6=1
      IF(.NOT.(k.EQ.7)) GOTO 5291
       sptr=sptr-1
       isto(sptr)=env
                  env=isto(env+15)
       sptr=sptr-1
       isto(sptr)=from
       from=9516
       GOTO 9010
 9516  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5290
 5291 IF(.NOT.(ndx1.EQ.8 .OR. ndx1.EQ.9
     1)) GOTO 5292
       k=isto(base5+ndx5)
       a=rsto(base3+k)
       rptr=rptr+1
       rsto(rptr)=a
       GOTO 5290
 5292 IF(.NOT.(ndx5.LT.0)) GOTO 5293
       a=rsto(base7-ndx5)
       IF(.NOT.(a.EQ.rnull
     1 .AND. ndx5.NE.-20)) GOTO 5301
        stpflg=83
        GOTO 5300
 5301  CONTINUE
 5300  CONTINUE
       rptr=rptr+1
       rsto(rptr)=a
       GOTO 5290
 5293 CONTINUE
       sptr=sptr-1
       isto(sptr)=ndx1
                  ndx1=isto(env+9)
       sptr=sptr-1
       isto(sptr)=ndx2
                  ndx2=isto(env+10)
       sptr=sptr-1
       isto(sptr)=from
       from=9517
       GOTO 9020
 9517  from=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       isto(sptr)=base4
       sptr=sptr-1
       isto(sptr)=base5
       sptr=sptr-1
       isto(sptr)=base6
       IF(.NOT.(isto(env+5).LT.isto(env+6)
     1 )) GOTO 6051
        base4=base14
        base5=base15
        base6=base16
        GOTO 6050
 6051  CONTINUE
 6050  CONTINUE
       to=9037
       CALL xpr2
       base6=isto(sptr)
       base5=isto(sptr+1)
       base4=isto(sptr+2)
       ndx2=isto(sptr+3)
       ndx1=isto(sptr+4)
       sptr=sptr+5
       sptr=sptr-1
       isto(sptr)=isto(ishenv+17)
                  isto(ishenv+17)=isto(env+5)
       to=9002
       CALL xpr2
       isto(ishenv+17)=isto(sptr)
       sptr=sptr+1
       cnt6=ndx6 -ndx4 +1
 5290 CONTINUE
      IF(.NOT.(stpflg.GT.0)) GOTO 5311
       CONTINUE
       CALL errstp
       GOTO 5310
 5311 CONTINUE
 5310 CONTINUE
      stpc1=isto(sptr)
      stpl1=isto(sptr+1)
      ndx6=isto(sptr+2)
      ndx5=isto(sptr+3)
      ndx4=isto(sptr+4)
      ndx2=isto(sptr+5)
      ndx1=isto(sptr+6)
      sptr=sptr+7
      GOTO 9500
 9010 CONTINUE
      i=isto(env+0)
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+1)
      IF(.NOT.(i.EQ.1)) GOTO 5321
       k=isto(env+1)
       IF(.NOT.(k.GE.399 .AND. k.LE.409
     1 )) GOTO 5331
        to=9012
        CALL xpr2
        GOTO 5330
 5331  CONTINUE
        sptr=sptr-1
        isto(sptr)=from
        from=9518
        GOTO 9011
 9518   from=isto(sptr)
        sptr=sptr+1
 5330  CONTINUE
       GOTO 5320
 5321 IF(.NOT.(i.EQ.2)) GOTO 5322
       sptr=sptr-1
       isto(sptr)=from
       from=9519
       GOTO 9015
 9519  from=isto(sptr)
       sptr=sptr+1
       GOTO 5320
 5322 CONTINUE
 5320 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9011 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx
                 ndx=isto(env+1)
      sptr=sptr-1
      isto(sptr)=ipol
      sptr=sptr-1
      isto(sptr)=ndx0
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx3
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx7
      sptr=sptr-1
      isto(sptr)=ndx8
      sptr=sptr-1
      isto(sptr)=ndx9
      sptr=sptr-1
      isto(sptr)=ndx10
      sptr=sptr-1
      isto(sptr)=ndx11
      sptr=sptr-1
      isto(sptr)=ndx12
      sptr=sptr-1
      isto(sptr)=ndx13
      sptr=sptr-1
      isto(sptr)=ndx14
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+2)
      stpl2=isto(env+7)
      stpc2=isto(env+8)
      ndx5=isto(env+1)
      ndx0=isto(env+5)
      sptr=sptr-1
      isto(sptr)=base4
      sptr=sptr-1
      isto(sptr)=base6
      IF(.NOT.(ndx0.NE.datenv)) GOTO 5341
       base4=base14
       base6=base16
       GOTO 5340
 5341 CONTINUE
 5340 CONTINUE
      ndx2=isto(base4+ndx5)
      ndx3=isto(base6+ndx5)
      base6=isto(sptr)
      sptr=sptr+1
      base4=isto(sptr)
      sptr=sptr+1
      xprndx=isto(env+9)
      IF(.NOT.(xprndx.GT.0)) GOTO 5351
       sptr=sptr-1
       isto(sptr)=from
       from=9520
       GOTO 9000
 9520  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(xprcnt.GT.1)) GOTO 5361
        stpflg=18
        stpi1=xprcnt
        CONTINUE
        CALL errstp
        GOTO 5360
 5361  CONTINUE
 5360  CONTINUE
       a=rsto(rptr)
       rptr=rptr-1
       ndx4=idnint(a)
       IF(.NOT.(ndx4.LT.ndx2
     1 .OR. ndx4.GT.ndx3)) GOTO 5371
        stpflg=19
        stpi1=ndx4
        stpi2=ndx2
        stpi3=ndx3
        CONTINUE
        CALL errstp
        GOTO 5370
 5371  CONTINUE
 5370  CONTINUE
       GOTO 5350
 5351 CONTINUE
       ndx4=1
 5350 CONTINUE
      i1=isto(env+2)
      ndx1=isto(env+3)
      env=isto(sptr)
      sptr=sptr+1
      i2=isto(env+4)
      sptr=sptr-1
      isto(sptr)=env
      IF(.NOT.(ndx0.EQ.datenv)) GOTO 5381
       env=useenv
       GOTO 5380
 5381 CONTINUE
       env=isto(ishenv+10)
 5380 CONTINUE
      IF(.NOT.(i1.EQ.6)) GOTO 5391
       i3=4
       i4=isto(env+29)
       GOTO 5390
 5391 CONTINUE
       i3=3
       i4=isto(env+36)
 5390 CONTINUE
      i0=i4 -1 +i3*(ndx1-1)
      IF(.NOT.(i2.GT.0)) GOTO 5401
       i3=isto(i0+3)
       i7=i3 +4*(ndx4-ndx2)
       ndx11=isto(i7+1)
       ndx14=isto(i7+3)
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5400
 5401 CONTINUE
       i3=isto(i0+2)
       ndx12=i3 +(ndx4-ndx2)
       i4=isto(i0+1)
       ndx13=i4 +7*(ndx4-ndx2)
       ndx8=isto(ndx13)
       ndx9=isto(ndx13+5)
       ndx10=isto(ndx13+2)
       ndx11=isto(ndx13+3)
       ndx14=isto(ndx13+6)
       env=isto(sptr)
       sptr=sptr+1
       ndx7=base3 +isto(base5+ndx5) -ndx2 +ndx4
       xprndx=isto(env+7)
       IF(.NOT.(xprndx.GT.0)) GOTO 5411
        sptr=sptr-1
        isto(sptr)=from
        from=9521
        GOTO 9000
 9521   from=isto(sptr)
        sptr=sptr+1
        a=rsto(rptr)
        rptr=rptr-1
        ipol=idnint(a)
        GOTO 5410
 5411  CONTINUE
        ipol=1
 5410  CONTINUE
       IF(.NOT.(ipol.GT.ndx9/2
     1 .AND. ndx.GT.379 .AND. ndx.LE.391
     2 .AND. ndx.NE.387)) GOTO 5421
        stpflg=20
        stpi1=ipol
        stpi2=ndx9/2
        CONTINUE
        CALL errstp
        GOTO 5420
 5421  CONTINUE
 5420  CONTINUE
       xprndx=isto(env+6)
       IF(.NOT.(xprndx.GT.0)) GOTO 5431
        sptr=sptr-1
        isto(sptr)=from
        from=9522
        GOTO 9000
 9522   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(xprcnt.GT.1)) GOTO 5441
         stpflg=21
         stpi1=xprcnt
         CONTINUE
         CALL errstp
         GOTO 5440
 5441   CONTINUE
 5440   CONTINUE
        GOTO 5430
 5431  CONTINUE
 5430  CONTINUE
       r1=rsto(base3+7)
       r2=rsto(ndx8+ndx10)
       r3=rsto(base3+8)
       r4=rsto(ndx7)
 5400 CONTINUE
      stpi1=ndx4
      IF(.NOT.(ndx.EQ.379)) GOTO 5451
       to=9014
       CALL xpr2
       GOTO 5450
 5451 IF(.NOT.(ndx.EQ.381)) GOTO 5452
       sptr=sptr-1
       isto(sptr)=from
       from=9523
       GOTO 9012
 9523  from=isto(sptr)
       sptr=sptr+1
       GOTO 5450
 5452 IF(.NOT.(ndx.EQ.383)) GOTO 5453
       to=9013
       CALL xpr2
       GOTO 5450
 5453 IF(.NOT.(ndx.EQ.385)) GOTO 5454
       to=9015
       CALL xpr2
       GOTO 5450
 5454 IF(.NOT.(ndx.EQ.387)) GOTO 5455
       rptr=rptr+1
       rsto(rptr)=r2
       GOTO 5450
 5455 IF(.NOT.(ndx.EQ.389)) GOTO 5456
       to=9016
       CALL xpr2
       GOTO 5450
 5456 IF(.NOT.(ndx.EQ.391)) GOTO 5457
       to=9017
       CALL xpr2
       GOTO 5450
 5457 IF(.NOT.(ndx.EQ.393
     1.OR. ndx.EQ.395)) GOTO 5458
       sptr=sptr-1
       isto(sptr)=from
       from=9524
       GOTO 9013
 9524  from=isto(sptr)
       sptr=sptr+1
       GOTO 5450
 5458 IF(.NOT.(ndx.EQ.411)) GOTO 5459
       sptr=sptr-1
       isto(sptr)=from
       from=9525
       GOTO 9014
 9525  from=isto(sptr)
       sptr=sptr+1
       GOTO 5450
 5459 IF(.NOT.(ndx.EQ.413)) GOTO 5460
       rsptr=rsptr-1
       rsto(rsptr)=rsto(base7+1)
       rsto(base7+1)=rsto(rptr)
       rptr=rptr-1
       sptr=sptr-1
       isto(sptr)=from
       from=9526
       GOTO 9014
 9526  from=isto(sptr)
       sptr=sptr+1
       rsto(base7+1)=rsto(rsptr)
       rsptr=rsptr+1
       GOTO 5450
 5460 CONTINUE
 5450 CONTINUE
      ndx14=isto(sptr)
      ndx13=isto(sptr+1)
      ndx12=isto(sptr+2)
      ndx11=isto(sptr+3)
      sptr=sptr+4
      ndx10=isto(sptr)
      ndx9=isto(sptr+1)
      ndx8=isto(sptr+2)
      ndx7=isto(sptr+3)
      ndx5=isto(sptr+4)
      ndx4=isto(sptr+5)
      sptr=sptr+6
      ndx3=isto(sptr)
      ndx2=isto(sptr+1)
      ndx1=isto(sptr+2)
      ndx0=isto(sptr+3)
      ipol=isto(sptr+4)
      ndx=isto(sptr+5)
      sptr=sptr+6
      GOTO 9500
 9012 CONTINUE
      sptr=sptr-1
      isto(sptr)=flg
      sptr=sptr-1
      isto(sptr)=ndx
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=ptr0
      r5=rsto(rptr)
      rptr=rptr-1
      p=dmax1(rmargn,dabs(rmargn*r3))
      IF(.NOT.(r5.GT.r3+p)) GOTO 6011
       stpflg=28
       stpr1=r3
       stpr2=r5
       CONTINUE
       CALL errstp
       GOTO 6010
 6011 CONTINUE
 6010 CONTINUE
      IF(.NOT.(r5.GE.r3-p)) GOTO 5471
       rptr=rptr+1
       rsto(rptr)=r4
       GOTO 5470
 5471 CONTINUE
       sptr=sptr-1
       isto(sptr)=env
                  env=useenv
       ndx4=isto(env+14)
       env=isto(sptr)
       sptr=sptr+1
       flg=0
 3181  IF(.NOT.(ndx4.GT.0 .AND. flg.EQ.0
     1 )) GOTO 3180
        ndx=0
        ndx6=ndx4+2
        ptr0=iptr
        to=9032
        CALL xpr2
        ndx1=isto(ptr0+1)
        ndx2=isto(ptr0+2)
        ndx4=isto(ptr0+3)
        ndx=0
        ndx6=ndx4+ndx12
        ptr0=rptr
        to=9033
        CALL xpr2
        a=rsto(ptr0+1)
        b=rsto(ptr0+1 +ndx12)
        p=dmax1(rmargn,dabs(rmargn*r5))
        IF(.NOT.(a.LE.r5+p)) GOTO 5481
         flg=1
         GOTO 5480
 5481   CONTINUE
         ndx4=ndx2
 5480   CONTINUE
        GOTO 3181
 3180  CONTINUE
       p=dmax1(rmargn,dabs(rmargn*r5))
       IF(.NOT.(flg.EQ.0)) GOTO 5491
        rptr=rptr+1
        rsto(rptr)=rsto(base7+1)
        rsto(base7+1)=r5
        sptr=sptr-1
        isto(sptr)=from
        from=9527
        GOTO 9014
 9527   from=isto(sptr)
        sptr=sptr+1
        a=rsto(rptr)
        rptr=rptr-1
        rsto(base7+1)=rsto(rptr)
        rptr=rptr-1
        rptr=rptr+1
        rsto(rptr)=a
        GOTO 5490
 5491  IF(.NOT.(a.GE.r5-p)) GOTO 5492
        rptr=rptr+1
        rsto(rptr)=b
        GOTO 5490
 5492  CONTINUE
        ndx4=ndx1
        sptr=sptr-1
        isto(sptr)=ptr0
                   ptr0=rptr+1
        sptr=sptr-1
        isto(sptr)=ptr1
        sptr=sptr-1
        isto(sptr)=ptr2
        sptr=sptr-1
        isto(sptr)=cnt
                   cnt=0
        sptr=sptr-1
        isto(sptr)=ndx0
                   ndx0=0
        rptr=rptr+1
        rsto(rptr)=r5
        to=9019
        CALL xpr2
        to=9020
        CALL xpr2
        rptr=ptr0
        ndx0=isto(sptr)
        cnt=isto(sptr+1)
        ptr2=isto(sptr+2)
        ptr1=isto(sptr+3)
        ptr0=isto(sptr+4)
        sptr=sptr+5
 5490  CONTINUE
 5470 CONTINUE
      ptr0=isto(sptr)
      ndx6=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      ndx=isto(sptr+5)
      flg=isto(sptr+6)
      sptr=sptr+7
      GOTO 9500
 9013 CONTINUE
      rptr=rptr+1
      rsto(rptr)=r1
      rptr=rptr+1
      rsto(rptr)=r2
      rptr=rptr+1
      rsto(rptr)=r3
      rptr=rptr+1
      rsto(rptr)=r4
      sptr=sptr-1
      isto(sptr)=flg
                 flg=1
      sptr=sptr-1
      isto(sptr)=ndx
      IF(.NOT.(ndx.EQ.393)) GOTO 5501
       ndx=1
       GOTO 5500
 5501 CONTINUE
       ndx=2
 5500 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      isto(sptr)=ptr2
                 ptr2=0
      sptr=sptr-1
      isto(sptr)=ptr3
                 ptr3=ndx13
      sptr=sptr-1
      isto(sptr)=from
      from=9528
      GOTO 9024
 9528 from=isto(sptr)
      sptr=sptr+1
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      r4=rsto(rptr)
      rptr=rptr-1
      r3=rsto(rptr)
      rptr=rptr-1
      r2=rsto(rptr)
      rptr=rptr-1
      r1=rsto(rptr)
      rptr=rptr-1
      d=b*r4 +a
      rptr=rptr+1
      rsto(rptr)=d
      ptr3=isto(sptr)
      ptr2=isto(sptr+1)
      ptr1=isto(sptr+2)
      ndx=isto(sptr+3)
      flg=isto(sptr+4)
      sptr=sptr+5
      GOTO 9500
 9014 CONTINUE
      xprndx=ndx11
      IF(.NOT.(xprndx.GT.0)) GOTO 5511
       sptr=sptr-1
       isto(sptr)=from
       from=9529
       GOTO 9000
 9529  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       IF(.NOT.(ndx14.GT.xprcnt
     1 .AND. xprcnt.GT.1)) GOTO 5521
        stpflg=115
        stpi2=ndx14
        stpi3=xprcnt
        CONTINUE
        CALL errstp
        GOTO 5520
 5521  IF(.NOT.(xprcnt.GT.1)) GOTO 5522
        rsto(rptr+1)=rsto(rptr+ndx14)
        GOTO 5520
 5522  CONTINUE
 5520  CONTINUE
       rptr=rptr+1
       GOTO 5510
 5511 CONTINUE
       stpflg=24
       stpi2=ndx4
       CONTINUE
       CALL errstp
 5510 CONTINUE
      GOTO 9500
 9015 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx
                 ndx=isto(env+1)
      sptr=sptr-1
      isto(sptr)=cnt
                 cnt=0
      sptr=sptr-1
      isto(sptr)=ndx1
                 ndx1=isto(env+4)
      IF(.NOT.(ndx1.EQ.1)) GOTO 5531
       sptr=sptr-1
       isto(sptr)=env
                  env=isto(env+5)
       k=isto(env+2)
       env=isto(sptr)
       sptr=sptr+1
       i=0
 3191  IF(.NOT.(i.LT.k)) GOTO 3190
        i=i+1
        rsto(ptr6+i)=rnull
        GOTO 3191
 3190  CONTINUE
       GOTO 5530
 5531 CONTINUE
 5530 CONTINUE
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+7)
 3201 IF(.NOT.(env .GT.0)) GOTO 3200
       cnt=cnt+1
       xprndx=isto(env+1)
       IF(.NOT.(xprndx.EQ.0)) GOTO 5541
        IF(.NOT.(ndx1.EQ.1)) GOTO 5551
         a=rnull
         rptr=rptr+1
         rsto(rptr)=a
         GOTO 5550
 5551   CONTINUE
         stpflg=16
         stpi1=cnt
         CONTINUE
         CALL errstp
 5550   CONTINUE
        GOTO 5540
 5541  CONTINUE
        sptr=sptr-1
        isto(sptr)=from
        from=9530
        GOTO 9000
 9530   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(xprcnt.GT.1)) GOTO 5561
         stpflg=89
         stpi1=cnt
         stpi2=xprcnt
         CONTINUE
         CALL errstp
         GOTO 5560
 5561   CONTINUE
 5560   CONTINUE
 5540  CONTINUE
       env=isto(env+0)
       GOTO 3201
 3200 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx.EQ.325)) GOTO 6021
       cnt=1
       a=rnull
       rptr=rptr+1
       rsto(rptr)=a
       GOTO 6020
 6021 CONTINUE
 6020 CONTINUE
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+5)
      IF(.NOT.(ndx1.EQ.0)) GOTO 5571
       to=9021
       CALL xpr2
       GOTO 5570
 5571 IF(.NOT.(ndx1.EQ.1)) GOTO 5572
       sptr=sptr-1
       isto(sptr)=from
       from=9531
       GOTO 9016
 9531  from=isto(sptr)
       sptr=sptr+1
       GOTO 5570
 5572 IF(.NOT.(ndx1.EQ.2)) GOTO 5573
       sptr=sptr-1
       isto(sptr)=from
       from=9532
       GOTO 9017
 9532  from=isto(sptr)
       sptr=sptr+1
       GOTO 5570
 5573 IF(.NOT.(ndx1.EQ.3)) GOTO 5574
       sptr=sptr-1
       isto(sptr)=from
       from=9533
       GOTO 9019
 9533  from=isto(sptr)
       sptr=sptr+1
       GOTO 5570
 5574 CONTINUE
 5570 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      cnt=isto(sptr+1)
      ndx=isto(sptr+2)
      sptr=sptr+3
      GOTO 9500
 9016 CONTINUE
      sptr=sptr-1
      isto(sptr)=cnt1
                 cnt1=0
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+3)
 3211 IF(.NOT.(env.GT.0)) GOTO 3210
       cnt1=cnt1+1
       IF(.NOT.(cnt1.GT.cnt)) GOTO 5581
        a=rnull
        GOTO 5580
 5581  CONTINUE
        a=rsto(ptr6+cnt1)
 5580  CONTINUE
       IF(.NOT.(a.EQ.rnull)) GOTO 5591
        stpl2=isto(env+1)
        stpc2=isto(env+2)
        xprndx=isto(env+6)
        IF(.NOT.(xprndx.EQ.0)) GOTO 5601
         stpflg=94
         CONTINUE
         CALL errstp
         GOTO 5600
 5601   CONTINUE
 5600   CONTINUE
        sptr=sptr-1
        isto(sptr)=from
        from=9534
        GOTO 9000
 9534   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(xprcnt.GT.1)) GOTO 5611
         stpflg=92
         stpi1=xprcnt
         CONTINUE
         CALL errstp
         GOTO 5610
 5611   CONTINUE
 5610   CONTINUE
        rsto(ptr6+cnt1)=rsto(rptr)
        rptr=rptr-1
        GOTO 5590
 5591  CONTINUE
 5590  CONTINUE
       env=isto(env+0)
       GOTO 3211
 3210 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      k=base5+isto(env+1)+1
      k=base3+isto(k)-1
      i=0
 3221 IF(.NOT.(i.LT.cnt1)) GOTO 3220
       i=i+1
       rsto(k+i)=rsto(ptr6+i)
       GOTO 3221
 3220 CONTINUE
      xprndx=isto(env+4)
      sptr=sptr-1
      isto(sptr)=from
      from=9535
      GOTO 9000
 9535 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(xprcnt.GT.1)) GOTO 5621
       stpflg=93
       stpi1=xprcnt
       CONTINUE
       CALL errstp
       GOTO 5620
 5621 CONTINUE
 5620 CONTINUE
      rsto(ptr6+1)=rsto(rptr)
      rptr=ptr6+1
      cnt1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9017 CONTINUE
      sptr=sptr-1
      isto(sptr)=ipol
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=flg
      IF(.NOT.(cnt.GT.2)) GOTO 5631
       stpflg=25
       stpi1=cnt
       CONTINUE
       CALL errstp
       GOTO 5630
 5631 CONTINUE
 5630 CONTINUE
      IF(.NOT.(cnt.EQ.2)) GOTO 5641
       b=rsto(ptr6+2)
       ipol=idnint(b)
       GOTO 5640
 5641 CONTINUE
       ipol=1
 5640 CONTINUE
      ndx1=isto(env+2)
      sptr=sptr-1
      isto(sptr)=env
                 env=ndx1
      xprndx=isto(env+2)
      sptr=sptr-1
      isto(sptr)=from
      from=9536
      GOTO 9000
 9536 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(xprcnt.GT.1)) GOTO 5651
       stpflg=117
       stpi1=xprcnt
       CONTINUE
       CALL errstp
       GOTO 5650
 5651 CONTINUE
 5650 CONTINUE
      b=rsto(rptr)
      rptr=rptr-1
      p=dmax1(rmargn,dabs(rmargn*b))
      a=rsto(ptr6+1)
      IF(.NOT.(a.GT.b+p)) GOTO 5661
       stpflg=26
       stpr1=a
       stpr2=b
       CONTINUE
       CALL errstp
       GOTO 5660
 5661 CONTINUE
 5660 CONTINUE
      IF(.NOT.(a.GE.b-p)) GOTO 5671
       xprndx=isto(env+3)
       sptr=sptr-1
       isto(sptr)=from
       from=9537
       GOTO 9000
 9537  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(xprcnt.GT.1)) GOTO 5681
        stpflg=117
        stpi1=xprcnt
        CONTINUE
        CALL errstp
        GOTO 5680
 5681  CONTINUE
 5680  CONTINUE
       GOTO 5670
 5671 CONTINUE
       flg=0
       ndx1=isto(env+1)
 3231  IF(.NOT.(ndx1.GT.0 .AND. flg.EQ.0)) GOTO 3230
        env=ndx1
        xprndx=isto(env+2)
        sptr=sptr-1
        isto(sptr)=from
        from=9538
        GOTO 9000
 9538   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(xprcnt.GT.1)) GOTO 5691
         stpflg=117
         stpi1=xprcnt
         CONTINUE
         CALL errstp
         GOTO 5690
 5691   CONTINUE
 5690   CONTINUE
        b=rsto(rptr)
        rptr=rptr-1
        p=dmax1(rmargn,dabs(rmargn*b))
        a=rsto(ptr6+1)
        IF(.NOT.(a.GE.b-p)) GOTO 5701
         flg=1
         GOTO 5700
 5701   CONTINUE
         ndx1=isto(env+1)
 5700   CONTINUE
        GOTO 3231
 3230  CONTINUE
       IF(.NOT.(flg.EQ.0)) GOTO 5711
        stpflg=27
        stpr1=a
        stpr2=b
        CONTINUE
        CALL errstp
        GOTO 5710
 5711  CONTINUE
 5710  CONTINUE
       IF(.NOT.(a.LE.b+p
     1 .OR. ipol.EQ.0)) GOTO 5721
        xprndx=isto(env+3)
        sptr=sptr-1
        isto(sptr)=from
        from=9539
        GOTO 9000
 9539   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(xprcnt.GT.1)) GOTO 5731
         stpflg=117
         stpi1=xprcnt
         CONTINUE
         CALL errstp
        GOTO 5730
 5731   CONTINUE
 5730   CONTINUE
       GOTO 5720
 5721  CONTINUE
        ndx1=isto(env+0)
        sptr=sptr-1
        isto(sptr)=ptr0
                   ptr0=rptr+1
        sptr=sptr-1
        isto(sptr)=ptr1
        sptr=sptr-1
        isto(sptr)=ptr2
        sptr=sptr-1
        isto(sptr)=cnt
                   cnt=0
        rptr=rptr+1
        rsto(rptr)=a
        sptr=sptr-1
        isto(sptr)=from
        from=9540
        GOTO 9018
 9540   from=isto(sptr)
        sptr=sptr+1
        to=9020
        CALL xpr2
        rptr=ptr6+1
        rsto(rptr)=rsto(ptr0)
        cnt=isto(sptr)
        ptr2=isto(sptr+1)
        ptr1=isto(sptr+2)
        ptr0=isto(sptr+3)
        sptr=sptr+4
 5720  CONTINUE
 5670 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      ndx1=isto(sptr+1)
      ipol=isto(sptr+2)
      sptr=sptr+3
      GOTO 9500
 9018 CONTINUE
      i1=2*ipol+1
      IF(.NOT.(i1.EQ.1)) GOTO 5741
       i1=2
       GOTO 5740
 5741 CONTINUE
 5740 CONTINUE
      ptr1=rptr
      ptr2=rptr+i1
      rptr=ptr2+i1
      cnt=0
 3241 IF(.NOT.(ndx1.GT.0 .AND. cnt.LT.i1
     1)) GOTO 3240
       cnt=cnt+1
       env=ndx1
       xprndx=isto(env+2)
       sptr=sptr-1
       isto(sptr)=from
       from=9541
       GOTO 9000
 9541  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(xprcnt.GT.1)) GOTO 5751
        stpflg=117
        stpi1=xprcnt
        CONTINUE
        CALL errstp
        GOTO 5750
 5751  CONTINUE
 5750  CONTINUE
       b=rsto(rptr)
       rptr=rptr-1
       rsto(ptr1+cnt)=b
       xprndx=isto(env+3)
       sptr=sptr-1
       isto(sptr)=from
       from=9542
       GOTO 9000
 9542  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(xprcnt.GT.1)) GOTO 5761
        stpflg=117
        stpi1=xprcnt
        CONTINUE
        CALL errstp
        GOTO 5760
 5761  CONTINUE
 5760  CONTINUE
       c=rsto(rptr)
       rptr=rptr-1
       rsto(ptr2+cnt)=c
       ndx1=isto(env+1)
       GOTO 3241
 3240 CONTINUE
      GOTO 9500
 9019 CONTINUE
      stpflg=95
      CONTINUE
      CALL errstp
      GOTO 9500
 9020 CONTINUE
      IF(.NOT.(ndx1.GT.0)) GOTO 5771
       xprndx=ndx1
       sptr=sptr-1
       isto(sptr)=from
       from=9543
       GOTO 9000
 9543  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(xprcnt.GT.1)) GOTO 5781
        stpflg=90
        stpi1=xprcnt
        CONTINUE
        CALL errstp
        GOTO 5780
 5781  CONTINUE
 5780  CONTINUE
       a=rsto(rptr)
       rptr=rptr-1
       ndx4=idnint(a)
       IF(.NOT.(ndx2.GT.0)) GOTO 5791
        xprndx=ndx2
        sptr=sptr-1
        isto(sptr)=from
        from=9544
        GOTO 9000
 9544   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(xprcnt.GT.1)) GOTO 5801
         stpflg=91
         stpi1=xprcnt
         CONTINUE
         CALL errstp
         GOTO 5800
 5801   CONTINUE
 5800   CONTINUE
        a=rsto(rptr)
        rptr=rptr-1
        ndx6=idnint(a)
        GOTO 5790
 5791  CONTINUE
        ndx6=ndx4
 5790  CONTINUE
       GOTO 5770
 5771 CONTINUE
       ndx4=1
       ndx6=1
 5770 CONTINUE
      GOTO 9500
 9021 CONTINUE
      xprndx=ndx1
      IF(.NOT.(xprndx.GT.0)) GOTO 5811
       sptr=sptr-1
       isto(sptr)=from
       from=9545
       GOTO 9000
 9545  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       a=rsto(rptr+1)
       p=dmax1(rmargn,dabs(rmargn*a))
       IF(.NOT.(rsto(rptr).LT.a-p
     1 .AND. rsto(rptr).NE.rnull
     1 )) GOTO 5821
        rsto(rptr)=a
        GOTO 5820
 5821  CONTINUE
 5820  CONTINUE
       GOTO 5810
 5811 CONTINUE
 5810 CONTINUE
      xprndx=ndx2
      IF(.NOT.(xprndx.GT.0)) GOTO 5831
       sptr=sptr-1
       isto(sptr)=from
       from=9546
       GOTO 9000
 9546  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       a=rsto(rptr+1)
       p=dmax1(rmargn,dabs(rmargn*a))
       IF(.NOT.(rsto(rptr).GT.a+p
     1 .AND. rsto(rptr).NE.rnull
     1 )) GOTO 5841
        rsto(rptr)=a
        GOTO 5840
 5841  CONTINUE
 5840  CONTINUE
       GOTO 5830
 5831 CONTINUE
 5830 CONTINUE
      GOTO 9500
 9022 CONTINUE
      sptr=sptr-1
      isto(sptr)=flg
                 flg=0
      xprndx=ndx1
      IF(.NOT.(xprndx.GT.0)) GOTO 5851
       sptr=sptr-1
       isto(sptr)=from
       from=9547
       GOTO 9000
 9547  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       a=rsto(rptr+1)
       p=dmax1(rmargn,dabs(rmargn*a))
       IF(.NOT.(rsto(rptr).LT.a-p
     1 .AND. rsto(rptr).NE.rnull
     1 )) GOTO 5861
        flg=1
        rsto(rptr)=a
        GOTO 5860
 5861  CONTINUE
 5860  CONTINUE
       GOTO 5850
 5851 CONTINUE
 5850 CONTINUE
      xprndx=ndx2
      IF(.NOT.(xprndx.GT.0)) GOTO 5871
       sptr=sptr-1
       isto(sptr)=from
       from=9548
       GOTO 9000
 9548  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       a=rsto(rptr+1)
       p=dmax1(rmargn,dabs(rmargn*a))
       IF(.NOT.(rsto(rptr).GT.a+p
     1 .AND. rsto(rptr).NE.rnull
     1 )) GOTO 5881
        flg=1
        rsto(rptr)=a
        GOTO 5880
 5881  CONTINUE
 5880  CONTINUE
       GOTO 5870
 5871 CONTINUE
 5870 CONTINUE
      IF(.NOT.(flg.GT.0)) GOTO 5891
       isto(ptr3+4)=1
       GOTO 5890
 5891 CONTINUE
 5890 CONTINUE
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9023 CONTINUE
      sptr=sptr-1
      isto(sptr)=flg
                 flg=0
      xprndx=ndx1
      IF(.NOT.(xprndx.GT.0)) GOTO 5901
       sptr=sptr-1
       isto(sptr)=from
       from=9549
       GOTO 9000
 9549  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       a=rsto(rptr+1)
       p=dmax1(rmargn,dabs(rmargn*a))
       IF(.NOT.(rsto(rptr).LT.a-p
     1 .AND. rsto(rptr).NE.rnull
     4 )) GOTO 5911
        flg=1
        rsto(rptr)=a
        GOTO 5910
 5911  CONTINUE
 5910  CONTINUE
       GOTO 5900
 5901 CONTINUE
 5900 CONTINUE
      xprndx=ndx2
      IF(.NOT.(xprndx.GT.0)) GOTO 5921
       sptr=sptr-1
       isto(sptr)=from
       from=9550
       GOTO 9000
 9550  from=isto(sptr)
       sptr=sptr+1
       rptr=rptr-xprcnt
       a=rsto(rptr+1)
       p=dmax1(rmargn,dabs(rmargn*a))
       IF(.NOT.(rsto(rptr).GT.a+p
     1 .AND. rsto(rptr).NE.rnull
     1 )) GOTO 5931
        flg=1
        rsto(rptr)=a
        GOTO 5930
 5931  CONTINUE
 5930  CONTINUE
       GOTO 5920
 5921 CONTINUE
 5920 CONTINUE
      IF(.NOT.(flg.GT.0)) GOTO 5941
       isto(ptr2+2)=isto(ptr2+2) +2
       i2=isto(ptr2)+1
       rsto(i2)=rsto(rptr)
       GOTO 5940
 5941 CONTINUE
 5940 CONTINUE
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9024 CONTINUE
      sptr=sptr-1
      isto(sptr)=from
      from=9551
      GOTO 9025
 9551 from=isto(sptr)
      sptr=sptr+1
      f=one
      i0=ptr1+ndx+2
      e=rsto(i0)
      j1=i0+ndx
      i2=ndx+2
      i1=rptr
      i=0
 3251 IF(.NOT.(i.LT.ndx)) GOTO 3250
       i=i+1
       i2=i2-1
       i1=i1+i2
       a=f/(e-rsto(i0+i))
       rsto(rptr+i)=a
       rsto(i1)=a
       GOTO 3251
 3250 CONTINUE
      b=rsto(ptr1+2)
      r1=rsto(ptr1+1) +b*rsto(rptr+1)
      r2=-b*rsto(rptr+ndx+1)*rsto(j1+1)
      i2=ndx+2
      i1=rptr
      j=1
 3261 IF(.NOT.(j.LT.ndx)) GOTO 3260
       j=j+1
       f=f+one
       e=rsto(i0+j-1)
       i2=i2-1
       i1=i1+i2
       i4=i2
       i3=i1
       i=j-1
 3271  IF(.NOT.(i.LT.ndx)) GOTO 3270
        i=i+1
        i4=i4-1
        i3=i3+i4
        a=f/(e-rsto(i0+i))
        rsto(i3)=a*rsto(i3)
        rsto(i1-j+i+1)=a *rsto(i1)
        i6=ndx+2
        i5=rptr
        k=-1
 3281   IF(.NOT.(k.LT.j-2)) GOTO 3280
         k=k+1
         b=rsto(i5-k+i)
         b=a*(rsto(i5-k+j-1) -b)
         rsto(i5-k+i)=b
         i6=i6-1
         i5=i5+i6
         GOTO 3281
 3280   CONTINUE
        GOTO 3271
 3270  CONTINUE
       a=rsto(ptr1+j+1)
       r1=r1 +a*rsto(rptr+j)
       b=zero
       c=one
       i6=ndx+2
       i5=rptr
       k=0
 3291  IF(.NOT.(k.LT.j)) GOTO 3290
        k=k+1
        i6=i6-1
        i5=i5+i6
        c=-c
        b=b +c*rsto(i5-k+j)*rsto(j1+k)
        GOTO 3291
 3290  CONTINUE
       r2=r2 +b*a
       GOTO 3261
 3260 CONTINUE
      rptr=ptr1+2
      IF(.NOT.(ndx.EQ.0)) GOTO 6031
       rsto(ptr1+2)=zero
       GOTO 6030
 6031 CONTINUE
       rsto(ptr1+1)=r1
       rsto(ptr1+2)=r2
 6030 CONTINUE
      GOTO 9500
 9025 CONTINUE
      IF(.NOT.(flg.EQ.0)) GOTO 5951
       sptr=sptr-1
       isto(sptr)=from
       from=9552
       GOTO 9026
 9552  from=isto(sptr)
       sptr=sptr+1
       GOTO 5950
 5951 CONTINUE
       sptr=sptr-1
       isto(sptr)=from
       from=9553
       GOTO 9027
 9553  from=isto(sptr)
       sptr=sptr+1
 5950 CONTINUE
      a=rsto(base3+8)
      rptr=rptr+1
      rsto(rptr)=a
      sptr=sptr-1
      isto(sptr)=env
                 env=useenv
      i0=isto(env+51)
      j=isto(env+50)
      k=isto(env+49)+1
      i=0
 3301 IF(.NOT.(i.LT.ndx)) GOTO 3300
       i=i+1
       k=k-1
       IF(.NOT.(k.LT.0)) GOTO 5961
        k=k+j
        GOTO 5960
 5961  CONTINUE
 5960  CONTINUE
       a=rsto(i0+k)
       rptr=rptr+1
       rsto(rptr)=a
       GOTO 3301
 3300 CONTINUE
      i0=isto(ptr3)
      j=isto(ptr3+5)
      k=isto(ptr3+2) +1
      i=0
 3311 IF(.NOT.(i.LT.ndx)) GOTO 3310
       i=i+1
       k=k-1
       IF(.NOT.(k.LT.0)) GOTO 5971
        k=k+j
        GOTO 5970
 5971  CONTINUE
 5970  CONTINUE
       a=rsto(i0+k)
       rptr=rptr+1
       rsto(rptr)=a
       GOTO 3311
 3310 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9026 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr0
                 ptr0=rptr+1
      rptr=ptr0+ndx
      i=-1
 3321 IF(.NOT.(i.LT.ndx)) GOTO 3320
       i=i+1
       rsto(ptr0+i)=zero
       GOTO 3321
 3320 CONTINUE
      sptr=sptr-1
      isto(sptr)=env
                 env=ptr2
 3331 IF(.NOT.(env.GT.0)) GOTO 3330
       xprndx=isto(env+3)
       IF(.NOT.(xprndx.GT.0)) GOTO 5981
        sptr=sptr-1
        isto(sptr)=from
        from=9554
        GOTO 9000
 9554   from=isto(sptr)
        sptr=sptr+1
        rptr=rptr-xprcnt
        a=rsto(rptr+1)
        GOTO 5980
 5981  CONTINUE
        a=one
 5980  CONTINUE
       IF(.NOT.(isto(env+1).EQ.2)) GOTO 5991
        a=-a
        GOTO 5990
 5991  CONTINUE
 5990  CONTINUE
       i=ptr0+isto(env+2)
       rsto(i)=rsto(i) +a
       env=isto(env+0)
       GOTO 3331
 3330 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9027 CONTINUE
      a=zero
      rptr=rptr+1
      rsto(rptr)=a
      IF(.NOT.(ndx.EQ.1)) GOTO 6001
       a=one
       rptr=rptr+1
       rsto(rptr)=a
       GOTO 6000
 6001 CONTINUE
       rptr=rptr+1
       rsto(rptr)=a
       a=one
       rptr=rptr+1
       rsto(rptr)=a
 6000 CONTINUE
      GOTO 9500
 9028 CONTINUE
      a=half*rsto(base3+6)
      rptr=rptr+1
      rsto(rptr)=a
      i3=isto(ptr3+2)
      i4=isto(ptr3)
      i5=isto(ptr2)
      b=rsto(i5) +rsto(i4+i3)*a
      rptr=rptr+1
      rsto(rptr)=b
      GOTO 9500
      END
