      SUBROUTINE xpr2
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010,
     1 9011, 9012, 9013, 9014, 9015,
     1 9016, 9017, 9018, 9019, 9020,
     1 9021, 9022, 9023, 9024, 9025,
     1 9026, 9027, 9028, 9029, 9030,
     1 9031, 9032, 9033, 9034, 9035,
     1 9036, 9037), to-8999
      STOP 'Invalid "to" reference in "xpr2".'
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
     1  9526,  9527,  9528,  9529), from-9500
      STOP 'Invalid "from" reference in "xpr2".'
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
      rptr=rptr-xprcnt
      k=ndx6-ndx4+1
      IF(.NOT.(k.LT.1)) GOTO 5001
       stpflg=29
       GOTO 5000
 5001 IF(.NOT.(k.NE.xprcnt .AND. xprcnt.GT.1
     1)) GOTO 5002
       stpflg=30
       GOTO 5000
 5002 CONTINUE
 5000 CONTINUE
      IF(.NOT.(stpflg.GT.0)) GOTO 5011
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=datenv
       i0=isto(env+1) +2*(ndx5-1)
       env=isto(sptr)
       sptr=sptr+1
       stpl1=isto(i0)
       stpc1=isto(i0+1)
       stpi1=ndx4
       stpi2=ndx6
       stpi3=xprcnt
       CONTINUE
       CALL errstp
       GOTO 5010
 5011 CONTINUE
 5010 CONTINUE
      i0=base3 +isto(base5+ndx5)
     1   -isto(base4+ndx5) +ndx4 -1
      i=0
      IF(.NOT.(xprcnt.EQ.1)) GOTO 5021
       a=rsto(rptr+1)
 3001  IF(.NOT.(i.LT.k)) GOTO 3000
        i=i+1
        rsto(i0+i)=a
        GOTO 3001
 3000  CONTINUE
       GOTO 5020
 5021 CONTINUE
 3011  IF(.NOT.(i.LT.xprcnt)) GOTO 3010
        i=i+1
        rsto(i0+i)=rsto(rptr+i)
        GOTO 3011
 3010  CONTINUE
 5020 CONTINUE
      GOTO 9500
 9001 CONTINUE
      rptr=rptr-xprcnt
      k=ndx6-ndx4+1
      IF(.NOT.(k.LT.1
     1.OR. (k.NE.xprcnt .AND. xprcnt.GT.1)
     2)) GOTO 5031
       stpflg=103
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=datenv
       i0=isto(env+1) +2*(ndx5-1)
       env=isto(sptr)
       sptr=sptr+1
       stpl1=isto(i0)
       stpc1=isto(i0+1)
       stpi1=ndx4
       stpi2=ndx6
       stpi3=xprcnt
       CONTINUE
       CALL errstp
       GOTO 5030
 5031 CONTINUE
 5030 CONTINUE
      i0=base3 +isto(base5+ndx5)
     1    -isto(base4+ndx5) +ndx4 -1
      i=0
      IF(.NOT.(xprcnt.EQ.1)) GOTO 5041
       a=rsto(rptr+1)
 3021  IF(.NOT.(i.LT.k)) GOTO 3020
        i=i+1
        IF(.NOT.(rsto(i0+i).EQ.rnull)) GOTO 5051
         rsto(i0+i)=a
         GOTO 5050
 5051   CONTINUE
 5050   CONTINUE
        GOTO 3021
 3020  CONTINUE
       GOTO 5040
 5041 CONTINUE
 3031  IF(.NOT.(i.LT.xprcnt)) GOTO 3030
      i=i+1
        IF(.NOT.(rsto(i0+i).EQ.rnull)) GOTO 5061
         rsto(i0+i)=rsto(rptr+i)
         GOTO 5060
 5061   CONTINUE
 5060   CONTINUE
        GOTO 3031
 3030  CONTINUE
 5040 CONTINUE
      GOTO 9500
 9002 CONTINUE
      stpi1=ndx4
      stpi2=ndx6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base6
      IF(.NOT.(isto(ishenv+17).NE.datenv)) GOTO 5071
       base3=base13
       base5=base15
       base4=base14
       base6=base16
       GOTO 5070
 5071 CONTINUE
 5070 CONTINUE
      j=isto(base4+ndx5)
      k=isto(base6+ndx5)
      stpi3=j
      stpi4=k
      IF(.NOT.(k.EQ.-iinf)) GOTO 5081
       stpflg=84
       GOTO 5080
 5081 IF(.NOT.(ndx6.LT.ndx4)) GOTO 5082
       stpflg=85
       GOTO 5080
 5082 IF(.NOT.(ndx4.LT.j .OR. ndx4.GT.k)) GOTO 5083
       stpflg=86
       GOTO 5080
 5083 IF(.NOT.(ndx6.LT.j .OR. ndx6.GT.k)) GOTO 5084
       stpflg=87
       GOTO 5080
 5084 CONTINUE
       k=ndx6 -ndx4 +1
       i0=base3 +isto(base5+ndx5) -j +ndx4 -1
       i=0
 3041  IF(.NOT.(i.LT.k .AND. stpflg.EQ.0)) GOTO 3040
        i=i+1
        a=rsto(i0+i)
        IF(.NOT.(a.EQ.rnull)) GOTO 5091
         stpflg=88
         stpi5=i0+i
         GOTO 5090
 5091   CONTINUE
 5090   CONTINUE
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=a
        GOTO 3041
 3040  CONTINUE
 5080 CONTINUE
      base6=isto(sptr)
      sptr=sptr+1
      base4=isto(sptr)
      sptr=sptr+1
      base5=isto(sptr)
      sptr=sptr+1
      base3=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9003 CONTINUE
      IF(.NOT.(cnt0.EQ.0)) GOTO 5101
       cnt0=cnt1
       GOTO 5100
 5101 CONTINUE
       rptr=rptr-cnt1
       IF(.NOT.(cnt0.NE.cnt1)) GOTO 5111
        stpflg=31
        stpi1=cnt0
        stpi2=cnt1
        stpi3=xprknd
        stpi4=xprsub
        CONTINUE
        CALL errstp
        GOTO 5110
 5111  CONTINUE
        i=0
 3051   IF(.NOT.(i.LT.cnt0)) GOTO 3050
         i=i+1
         IF(.NOT.(rsto(ptr0+i).LE.rmargn)) GOTO 5121
          rsto(ptr0+i)=rsto(rptr+i)
          GOTO 5120
 5121    CONTINUE
 5120    CONTINUE
         GOTO 3051
 3050   CONTINUE
 5110  CONTINUE
 5100 CONTINUE
      GOTO 9500
 9004 CONTINUE
      IF(.NOT.(cnt1.EQ.0)) GOTO 5131
       cnt1=cnt2
       GOTO 5130
 5131 CONTINUE
       rptr=rptr-cnt2
       IF(.NOT.(cnt1.NE.cnt2)) GOTO 5141
        stpflg=32
        stpi1=cnt1
        stpi2=cnt2
        stpi3=xprknd
        stpi4=xprsub
        CONTINUE
        CALL errstp
        GOTO 5140
 5141  CONTINUE
        i=0
 3061   IF(.NOT.(i.LT.cnt1)) GOTO 3060
         i=i+1
         IF(.NOT.(rsto(ptr1+i).GT.rmargn)) GOTO 5151
          rsto(ptr1+i)=rsto(rptr+i)
          GOTO 5150
 5151    CONTINUE
 5150    CONTINUE
         GOTO 3061
 3060   CONTINUE
 5140  CONTINUE
 5130 CONTINUE
      GOTO 9500
 9005 CONTINUE
      p=dmax1(rmargn,dabs(rmargn*b))
      IF(.NOT.((ndx.EQ.3
     1  .AND. a.LT.b-p)
     2.OR.(ndx.EQ.4
     3  .AND. a.LE.b+p)
     4.OR.(ndx.EQ.5
     5  .AND. a.GT.b+p)
     6.OR.(ndx.EQ.6
     7  .AND. a.GE.b-p)
     8.OR.(ndx.EQ.1
     9  .AND. a.GE.b-p
     a  .AND. a.LE.b+p)
     b.OR.(ndx.EQ.2
     c  .AND. (a.LT.b-p
     d    .OR. a.GT.b+p))
     e)) GOTO 5161
       c=one
       GOTO 5160
 5161 CONTINUE
 5160 CONTINUE
      GOTO 9500
 9006 CONTINUE
      IF(.NOT.(cnt3.EQ.0)) GOTO 5231
       cnt3=cnt4
       GOTO 5230
 5231 CONTINUE
       rptr=rptr-cnt4
       IF(.NOT.(cnt3.NE.cnt4)) GOTO 5241
        stpflg=35
        stpi1=cnt3
        stpi2=cnt4
        stpi3=xprknd
        stpi4=xprsub
        CONTINUE
        CALL errstp
        GOTO 5240
 5241  CONTINUE
        i=0
 3071   IF(.NOT.(i.LT.cnt3)) GOTO 3070
         i=i+1
         rsto(ptr3+i)=rsto(ptr3+i) +rsto(rptr+i)
         GOTO 3071
 3070   CONTINUE
 5240  CONTINUE
 5230 CONTINUE
      GOTO 9500
 9007 CONTINUE
      IF(.NOT.(cnt4.EQ.0)) GOTO 5251
       cnt4=cnt5
       GOTO 5250
 5251 CONTINUE
       rptr=rptr-cnt5
       i=0
       IF(.NOT.(cnt4.EQ.1)) GOTO 5261
        a=rsto(rptr)
 3081   IF(.NOT.(i.LT.cnt5)) GOTO 3080
         i=i+1
         rsto(ptr4+i)=rsto(rptr+i) *a
         GOTO 3081
 3080   CONTINUE
        cnt4=cnt5
        GOTO 5260
 5261  IF(.NOT.(cnt5.EQ.1)) GOTO 5262
        a=rsto(rptr+1)
 3091   IF(.NOT.(i.LT.cnt4)) GOTO 3090
         i=i+1
         rsto(ptr4+i)=rsto(ptr4+i) *a
         GOTO 3091
 3090   CONTINUE
        GOTO 5260
 5262  CONTINUE
        stpflg=79
        stpi1=cnt4
        stpi2=cnt5
        CONTINUE
        CALL errstp
 5260  CONTINUE
       rptr=ptr4+cnt4
 5250 CONTINUE
      GOTO 9500
 9008 CONTINUE
      IF(.NOT.(dabs(a).LT.rmargn
     1.OR. dabs(b).LT.divzro
     2)) GOTO 5271
       c=zero
       GOTO 5270
 5271 CONTINUE
       c=a -dint(a/b) *b
 5270 CONTINUE
      GOTO 9500
 9009 CONTINUE
      IF(.NOT.(dabs(a).LT.rmargn)) GOTO 5281
       c=zero
       GOTO 5280
 5281 IF(.NOT.(dabs(b).LT.rmargn)) GOTO 5282
       c=one
       GOTO 5280
 5282 IF(.NOT.(a.LT.zero)) GOTO 5283
       d=dnint(b)
       e=dabs(d-b)
       IF(.NOT.(e.GT.rmargn)) GOTO 5871
        stpflg=132
        stpr1=a
        stpr2=b
        CONTINUE
        CALL errstp
        GOTO 5870
 5871  CONTINUE
        k=idnint(b)
        c=a**k
 5870  CONTINUE
       GOTO 5280
 5283 CONTINUE
       c=a**b
 5280 CONTINUE
      GOTO 9500
 9010 CONTINUE
      a=rsto(rptr)
      IF(.NOT.(dabs(a).LT.divzro)) GOTO 5291
       stpflg=82
       stpr1=a
       CONTINUE
       CALL errstp
       GOTO 5290
 5291 CONTINUE
       rsto(rptr)=one /a
 5290 CONTINUE
      GOTO 9500
 9011 CONTINUE
      a=rsto(rptr)
      IF(.NOT.(dabs(a).LT.divzro)) GOTO 5301
       flg=1
       GOTO 5300
 5301 CONTINUE
       flg=0
       rsto(rptr)=one /a
 5300 CONTINUE
      GOTO 9500
 9012 CONTINUE
      stpflg=121
      CONTINUE
      CALL errstp
      GOTO 9500
 9013 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
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
      isto(sptr)=ndx6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr0
      r6=rsto(rptr)
      rptr=rptr-1
      p=dmax1(rmargn,dabs(rmargn*r4))
      IF(.NOT.(r6.LT.r4-p)) GOTO 5311
       ndx3=-1
       GOTO 5310
 5311 IF(.NOT.(r6.GT.r4+p)) GOTO 5312
       ndx3=1
       GOTO 5310
 5312 CONTINUE
       ndx3=0
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=r3
 5310 CONTINUE
      IF(.NOT.(ndx3.NE.0)) GOTO 6311
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=useenv
       ndx4=isto(env+14)
       env=isto(sptr)
       sptr=sptr+1
       flg=0
 3101  IF(.NOT.(ndx4.GT.0 .AND. flg.EQ.0)) GOTO 3100
        ndx=0
        ndx6=ndx4+2
        ptr0=iptr
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9501
        GOTO 9032
 9501   from=isto(sptr)
        sptr=sptr+1
        ndx1=isto(ptr0+1)
        ndx2=isto(ptr0+2)
        ndx4=isto(ptr0+3)
        ndx=0
        ndx6=ndx4+ndx12
        ptr0=rptr
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9502
        GOTO 9033
 9502   from=isto(sptr)
        sptr=sptr+1
        a=rsto(ptr0+1)
        b=rsto(ptr0+1 +ndx12)
        p=dmax1(rmargn,dabs(rmargn*r6))
        IF(.NOT.((ndx3.LT.0
     1     .AND. b.LE.r6+p)
     2  .OR. (ndx3.GT.0
     3     .AND. b.GE.r6-p)
     4  )) GOTO 5321
         flg=1
         GOTO 5320
 5321   CONTINUE
         ndx4=ndx2
 5320   CONTINUE
        GOTO 3101
 3100  CONTINUE
       p=dmax1(rmargn,dabs(rmargn*r6))
       IF(.NOT.(flg.EQ.0)) GOTO 5331
        r5=rnull
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=r5
        GOTO 5330
 5331  IF(.NOT.((ndx3.LT.0
     1     .AND. b.GE.r6-p)
     2 .OR. (ndx3.GT.0
     3     .AND. b.LE.r6+p)
     4 )) GOTO 5332
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=a
        GOTO 5330
 5332  CONTINUE
        ndx4=ndx1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr0
                   ptr0=rptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=cnt
                   cnt=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ndx0
                   ndx0=1
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=r6
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9503
        GOTO 9019
 9503   from=isto(sptr)
        sptr=sptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9504
        GOTO 9020
 9504   from=isto(sptr)
        sptr=sptr+1
        rptr=ptr0
        ndx0=isto(sptr)
        sptr=sptr+1
        cnt=isto(sptr)
        sptr=sptr+1
        ptr2=isto(sptr)
        sptr=sptr+1
        ptr1=isto(sptr)
        sptr=sptr+1
        ptr0=isto(sptr)
        sptr=sptr+1
 5330  CONTINUE
       GOTO 6310
 6311 CONTINUE
 6310 CONTINUE
      ptr0=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9014 CONTINUE
      r5=rsto(rptr)
      rptr=rptr-1
      p=dmax1(rmargn,dabs(rmargn*r3))
      IF(.NOT.(r5.LT.r3-p)) GOTO 5341
       stpflg=22
       stpr1=r5
       stpr2=r3
       CONTINUE
       CALL errstp
       GOTO 5340
 5341 IF(.NOT.(r5.LE.r3+p
     1.OR. ipol.EQ.0)) GOTO 5342
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=r4
       GOTO 5340
 5342 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr0
                  ptr0=rptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr2
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=cnt
                  cnt=0
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx0
                  ndx0=0
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=r5
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9505
       GOTO 9018
 9505  from=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9506
       GOTO 9020
 9506  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.((r2-r4)*(rsto(ptr0)-r4).GT.zero
     1 )) GOTO 6131
        rsto(ptr0)=r4
        GOTO 6130
 6131  CONTINUE
 6130  CONTINUE
       rptr=ptr0
       ndx0=isto(sptr)
       sptr=sptr+1
       cnt=isto(sptr)
       sptr=sptr+1
       ptr2=isto(sptr)
       sptr=sptr+1
       ptr1=isto(sptr)
       sptr=sptr+1
       ptr0=isto(sptr)
       sptr=sptr+1
 5340 CONTINUE
      GOTO 9500
 9015 CONTINUE
      r6=rsto(rptr)
      rptr=rptr-1
      p=dmax1(rmargn,dabs(rmargn*r4))
      IF(.NOT.(dabs(r6-r4).LE.p
     1.OR. dabs(r2-r4).LE.p)) GOTO 6141
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=r3
       GOTO 6140
 6141 CONTINUE
       IF(.NOT.(ipol.EQ.0)) GOTO 5351
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=rnull
        GOTO 5350
 5351  IF(.NOT.((r2-r4)*(r6-r4).GT.zero
     1 )) GOTO 5352
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=r3
        GOTO 5350
 5352  CONTINUE
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr0
                   ptr0=rptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=cnt
                   cnt=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ndx0
                   ndx0=1
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=r6
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9507
        GOTO 9018
 9507   from=isto(sptr)
        sptr=sptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9508
        GOTO 9020
 9508   from=isto(sptr)
        sptr=sptr+1
        p=dmax1(rmargn,dabs(rmargn*r3))
        IF(.NOT.(rsto(ptr0).LE.r3+p
     1  )) GOTO 5931
         rsto(ptr0)=r3
         GOTO 5930
 5931   IF(.NOT.(rsto(ptr0).GT.r3+rsto(base3+6)
     1  )) GOTO 5932
         rsto(ptr0)=r3+rsto(base3+6)
         GOTO 5930
 5932   CONTINUE
 5930   CONTINUE
        rptr=ptr0
        ndx0=isto(sptr)
        sptr=sptr+1
        cnt=isto(sptr)
        sptr=sptr+1
        ptr2=isto(sptr)
        sptr=sptr+1
        ptr1=isto(sptr)
        sptr=sptr+1
        ptr0=isto(sptr)
        sptr=sptr+1
 5350  CONTINUE
 6140 CONTINUE
      GOTO 9500
 9016 CONTINUE
      r5=rsto(rptr)
      rptr=rptr-1
      p=dmax1(rmargn,dabs(rmargn*r5))
      IF(.NOT.(r5.LT.r1-p
     1.OR. r5.GT.r3+p
     2)) GOTO 5361
       stpflg=23
       stpr1=r1
       stpr2=r3
       stpr3=r5
       CONTINUE
       CALL errstp
       GOTO 5360
 5361 IF(.NOT.(r5.LE.r1+p
     1.OR. ipol.EQ.0)) GOTO 5362
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=r2
       GOTO 5360
 5362 IF(.NOT.(r5.GE.r3-p)) GOTO 5363
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=r4
       GOTO 5360
 5363 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr0
                  ptr0=rptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr2
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=cnt
                  cnt=0
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx0
                  ndx0=0
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=r5
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9509
       GOTO 9018
 9509  from=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9510
       GOTO 9020
 9510  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.((r2-r4)*(rsto(ptr0)-r4).LT.zero
     1 )) GOTO 6151
        rsto(ptr0)=r4
        GOTO 6150
 6151  IF(.NOT.((r2-r4)*(r2-rsto(ptr0)).LT.zero
     1 )) GOTO 6152
        rsto(ptr0)=r2
        GOTO 6150
 6152  CONTINUE
 6150  CONTINUE
       rptr=ptr0
       ndx0=isto(sptr)
       sptr=sptr+1
       cnt=isto(sptr)
       sptr=sptr+1
       ptr2=isto(sptr)
       sptr=sptr+1
       ptr1=isto(sptr)
       sptr=sptr+1
       ptr0=isto(sptr)
       sptr=sptr+1
 5360 CONTINUE
      GOTO 9500
 9017 CONTINUE
      r6=rsto(rptr)
      rptr=rptr-1
      p=dmax1(rmargn,dabs(rmargn*r4))
      IF(.NOT.(dabs(r6-r4).LE.p
     1.OR. dabs(r2-r4).LE.p)) GOTO 5951
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=r3
       GOTO 5950
 5951 IF(.NOT.(dabs(r6-r2)
     1     .LE.dmax1(rmargn,dabs(rmargn*r2))
     1)) GOTO 5952
       IF(.NOT.(ipol.EQ.0)) GOTO 6161
        a=r3-dmax1(rmargn,dabs(rmargn*r3))
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=a
        GOTO 6160
 6161  CONTINUE
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=r1
 6160  CONTINUE
       GOTO 5950
 5952 CONTINUE
       IF(.NOT.(ipol.EQ.0)) GOTO 5371
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=rnull
        GOTO 5370
 5371  IF(.NOT.((r2-r4)*(r6-r4).LT.zero
     1 )) GOTO 5372
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=r3
        GOTO 5370
 5372  IF(.NOT.((r2-r4)*(r2-r6).LT.zero
     1 )) GOTO 5373
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=r1
        GOTO 5370
 5373  CONTINUE
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr0
                   ptr0=rptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=cnt
                   cnt=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ndx0
                   ndx0=1
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=r6
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9511
        GOTO 9018
 9511   from=isto(sptr)
        sptr=sptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9512
        GOTO 9020
 9512   from=isto(sptr)
        sptr=sptr+1
        p=dmax1(rmargn,dabs(rmargn*rsto(ptr0)))
        IF(.NOT.(rsto(ptr0).GE.r3-p
     1  )) GOTO 5941
         rsto(ptr0)=r3
         GOTO 5940
 5941   IF(.NOT.(rsto(ptr0).LE.r1+p
     1  )) GOTO 5942
         rsto(ptr0)=r1
         GOTO 5940
 5942   CONTINUE
 5940   CONTINUE
        rptr=ptr0
        ndx0=isto(sptr)
        sptr=sptr+1
        cnt=isto(sptr)
        sptr=sptr+1
        ptr2=isto(sptr)
        sptr=sptr+1
        ptr1=isto(sptr)
        sptr=sptr+1
        ptr0=isto(sptr)
        sptr=sptr+1
 5370  CONTINUE
 5950 CONTINUE
      GOTO 9500
 9018 CONTINUE
      i1=2*ipol+1
      IF(.NOT.(i1.EQ.1)) GOTO 5381
       i1=2
       GOTO 5380
 5381 CONTINUE
 5380 CONTINUE
      ptr1=rptr
      ptr2=rptr+i1
      rptr=ptr2+i1
      rsto(ptr1+1)=r3
      rsto(ptr1+2)=r1
      rsto(ptr2+1)=r4
      rsto(ptr2+2)=r2
      cnt=2
      IF(.NOT.(cnt.LT.i1)) GOTO 5391
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=useenv
       i4=isto(env+50)
       i2=isto(env+49)
       i3=isto(env+51)
       env=isto(sptr)
       sptr=sptr+1
       k=ndx10
       j=i2
 3111  IF(.NOT.(cnt.LT.i1)) GOTO 3110
        cnt=cnt+1
        j=j-1
        IF(.NOT.(j.LT.0)) GOTO 5401
         j=j+i4
         GOTO 5400
 5401   CONTINUE
 5400   CONTINUE
        rsto(ptr1+cnt)=rsto(i3+j)
        k=k-1
        IF(.NOT.(k.LT.0)) GOTO 5411
         k=k+ndx9
         GOTO 5410
 5411   CONTINUE
 5410   CONTINUE
        rsto(ptr2+cnt)=rsto(ndx8+k)
        GOTO 3111
 3110  CONTINUE
       GOTO 5390
 5391 CONTINUE
 5390 CONTINUE
      IF(.NOT.(ndx0.GT.0)) GOTO 5421
       k=ptr1
       ptr1=ptr2
       ptr2=k
       GOTO 5420
 5421 CONTINUE
 5420 CONTINUE
      GOTO 9500
 9019 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt1
                 cnt1=ipol+1
      IF(.NOT.(cnt1.EQ.1)) GOTO 5431
       cnt1=2
       GOTO 5430
 5431 CONTINUE
 5430 CONTINUE
      ptr1=rptr
      ptr2=rptr+cnt1
      rptr=ptr2+cnt1
 3121 IF(.NOT.(ndx4.GT.0 .AND. cnt.LT.cnt1)) GOTO 3120
       cnt=cnt+1
       ndx=0
       ndx6=ndx4+2
       ptr0=iptr
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9513
       GOTO 9032
 9513  from=isto(sptr)
       sptr=sptr+1
       ndx1=isto(ptr0+1)
       ndx2=isto(ptr0+2)
       ndx4=isto(ptr0+3)
       ndx=0
       ndx6=ndx4+ndx12
       ptr0=rptr
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9514
       GOTO 9033
 9514  from=isto(sptr)
       sptr=sptr+1
       rsto(ptr1+cnt)=rsto(ptr0+1)
       rsto(ptr2+cnt)=rsto(ptr0+1+ndx12)
       ndx4=ndx2
       GOTO 3121
 3120 CONTINUE
      IF(.NOT.(ndx0.GT.0)) GOTO 5441
       k=ptr1
       ptr1=ptr2
       ptr2=k
       GOTO 5440
 5441 CONTINUE
 5440 CONTINUE
      cnt1=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9020 CONTINUE
      k1=ptr1+1
      k2=ptr2+1
      n=cnt-1
      g=one
      IF(.NOT.(n.GT.0)) GOTO 5961
       a=rsto(ptr0)
       b=rsto(k1)
       c=rsto(k1+1)
       p=dmax1(rmargn,dabs(rmargn*b))
       IF(.NOT.(a*c.LE.zero
     1 .OR. a*b.LE.zero)) GOTO 6121
        n=1
        GOTO 6120
 6121  CONTINUE
        IF(.NOT.(dabs(a).LE.p)) GOTO 6091
         n=1
         GOTO 6090
 6091   CONTINUE
         g=dmax1(g,dabs(dlog10(dabs(a/b))))
         n=min0(n,2*idint(6.0D0/g))
         n=max0(n,1)
 6090   CONTINUE
        IF(.NOT.(c.GT.b)) GOTO 5971
         i1=1
         GOTO 5970
 5971   IF(.NOT.(c.LT.b)) GOTO 5972
         i1=-1
         GOTO 5970
 5972   CONTINUE
 5970   CONTINUE
 6120  CONTINUE
       c=b
       i=0
 3311  IF(.NOT.(i.LT.n
     1 .AND. n.GT.1)) GOTO 3310
        i=i+1
        b=c
        c=rsto(k1+i)
        p=dmax1(rmargn,dabs(rmargn*b))
        IF(.NOT.((c.GT.b .AND. i1.LT.0)
     1  .OR. (c.LT.b .AND. i1.GT.0)
     2  )) GOTO 6021
         n=i-1
         GOTO 6020
 6021   CONTINUE
 6020   CONTINUE
        IF(.NOT.(dabs(c-b).LE.p)) GOTO 6031
         n=i-1
         GOTO 6030
 6031   CONTINUE
         g=dmax1(g,dabs(dlog10(dabs((c-b)/b))))
 6030   CONTINUE
        IF(.NOT.(b*c.LE.zero)) GOTO 6101
         n=i-1
         GOTO 6100
 6101   CONTINUE
 6100   CONTINUE
        IF(.NOT.(dabs(c).LE.p)) GOTO 6111
         n=i-1
         GOTO 6110
 6111   CONTINUE
         g=dmax1(g,dabs(dlog10(dabs(c/b))))
 6110   CONTINUE
        n=min0(n,2*idint(6.0D0/g))
        n=max0(n,1)
        GOTO 3311
 3310  CONTINUE
       GOTO 5960
 5961 CONTINUE
 5960 CONTINUE
      IF(.NOT.(n.EQ.0)) GOTO 6041
       f=rsto(k2)
       GOTO 6040
 6041 CONTINUE
       IF(.NOT.(n.EQ.1)) GOTO 6061
        i4=1
        GOTO 6060
 6061  CONTINUE
        i4=0
        n=2*(n/2)
        i3=n/2
 6060  CONTINUE
       k3=rptr+1
       k4=k3+n+1
       i=-1
 3321  IF(.NOT.(i.LT.n)) GOTO 3320
        i=i+1
        c=rsto(k1+i)
        rsto(k3+i)=a-c
        rsto(k4+i)=rsto(k2+i)
        IF(.NOT.(i4.EQ.0)) GOTO 6071
         rsto(k4+i)=rsto(k4+i)*(c**i3)
         GOTO 6070
 6071   CONTINUE
 6070   CONTINUE
        GOTO 3321
 3320  CONTINUE
       j=0
 3331  IF(.NOT.(j.LT.n)) GOTO 3330
        j=j+1
        c=rsto(k3+j-1)
        d=rsto(k4+j-1)
        i=j-1
 3341   IF(.NOT.(i.LT.n)) GOTO 3340
         i=i+1
         b=rsto(k3+i)
         e=b*d -c*rsto(k4+i)
         rsto(k4+i)=e/(b-c)
         GOTO 3341
 3340   CONTINUE
        GOTO 3331
 3330  CONTINUE
       f=rsto(k4+n)
       IF(.NOT.(i4.EQ.0)) GOTO 6081
        f=f/(a**i3)
        GOTO 6080
 6081  CONTINUE
 6080  CONTINUE
 6040 CONTINUE
      rsto(ptr0)=f
      GOTO 9500
 9021 CONTINUE
      IF(.NOT.(ndx.EQ.351)) GOTO 5461
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9515
       GOTO 9023
 9515  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5461 IF(.NOT.(ndx.EQ.353)) GOTO 5462
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9516
       GOTO 9024
 9516  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5462 IF(.NOT.(ndx.EQ.355)) GOTO 5463
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9517
       GOTO 9025
 9517  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5463 IF(.NOT.(ndx.EQ.357)) GOTO 5464
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9518
       GOTO 9026
 9518  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5464 IF(.NOT.(ndx.EQ.359)) GOTO 5465
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9519
       GOTO 9027
 9519  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5465 IF(.NOT.(ndx.EQ.361)) GOTO 5466
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9520
       GOTO 9028
 9520  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5466 IF(.NOT.(ndx.EQ.365)) GOTO 5467
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9521
       GOTO 9029
 9521  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5467 IF(.NOT.(ndx.EQ.367)) GOTO 5468
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9522
       GOTO 9030
 9522  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5468 IF(.NOT.(ndx.EQ.369)) GOTO 5469
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9523
       GOTO 9031
 9523  from=isto(sptr)
       sptr=sptr+1
       GOTO 5460
 5469 IF(.NOT.(ndx.EQ.371)) GOTO 5470
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9528
       GOTO 9029
 9528  from=isto(sptr)
       sptr=sptr+1
       rsto(rptr)=-rsto(rptr)+one
       GOTO 5460
 5470 IF(.NOT.(ndx.EQ.373)) GOTO 5471
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9529
       GOTO 9030
 9529  from=isto(sptr)
       sptr=sptr+1
       rsto(rptr)=-rsto(rptr)+one
       GOTO 5460
 5471 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9524
       GOTO 9022
 9524  from=isto(sptr)
       sptr=sptr+1
 5460 CONTINUE
      GOTO 9500
 9022 CONTINUE
      IF(.NOT.(cnt.NE.1)) GOTO 5861
       stpflg=17
       stpi1=cnt
       CONTINUE
       CALL errstp
       GOTO 5860
 5861 CONTINUE
 5860 CONTINUE
      a=rsto(ptr6+1)
      IF(.NOT.(ndx.EQ.297)) GOTO 5481
       c=dabs(a)
       GOTO 5480
 5481 IF(.NOT.(ndx.EQ.299)) GOTO 5482
       IF(.NOT.(a.LT.zero)) GOTO 5821
        stpflg=1
        GOTO 5820
 5821  IF(.NOT.(a.EQ.zero)) GOTO 5822
        c=zero
        GOTO 5820
 5822  CONTINUE
        c=dsqrt(a)
 5820  CONTINUE
       GOTO 5480
 5482 IF(.NOT.(ndx.EQ.301)) GOTO 5483
       c=dexp(a)
       GOTO 5480
 5483 IF(.NOT.(ndx.EQ.303)) GOTO 5484
       IF(.NOT.(a.LT.zero)) GOTO 5511
        stpflg=2
        GOTO 5510
 5511  IF(.NOT.(a.EQ.zero)) GOTO 5512
        c=-rinf
        GOTO 5510
 5512  CONTINUE
        c=dlog(a)
 5510  CONTINUE
       GOTO 5480
 5484 IF(.NOT.(ndx.EQ.305)) GOTO 5485
       IF(.NOT.(a.LT.zero)) GOTO 5521
        stpflg=3
        GOTO 5520
 5521  IF(.NOT.(a.EQ.zero)) GOTO 5522
        c=-rinf
        GOTO 5520
 5522  CONTINUE
        c=dlog(a)/dlog(ten)
 5520  CONTINUE
       GOTO 5480
 5485 IF(.NOT.(ndx.EQ.307)) GOTO 5486
       IF(.NOT.(a.LT.zero)) GOTO 5531
        stpflg=4
        GOTO 5530
 5531  IF(.NOT.(a.EQ.zero)) GOTO 5532
        c=-rinf
        GOTO 5530
 5532  CONTINUE
        c=dlog(a)/dlog(two)
 5530  CONTINUE
       GOTO 5480
 5486 IF(.NOT.(ndx.EQ.309)) GOTO 5487
       IF(.NOT.(dabs(a).LE.divzro)) GOTO 5541
        stpflg=5
        GOTO 5540
 5541  CONTINUE
        c=one/a
 5540  CONTINUE
       GOTO 5480
 5487 IF(.NOT.(ndx.EQ.311)) GOTO 5488
       IF(.NOT.(a.LT.-half)) GOTO 5551
        stpflg=6
        GOTO 5550
 5551  CONTINUE
        b=one
        c=one
 3161   IF(.NOT.(a.GE.b+half)) GOTO 3160
         b=b+one
         c=c*b
         GOTO 3161
 3160   CONTINUE
 5550  CONTINUE
       GOTO 5480
 5488 IF(.NOT.(ndx.EQ.313)) GOTO 5489
       c=dint(a)
       GOTO 5480
 5489 IF(.NOT.(ndx.EQ.315)) GOTO 5490
       c=a -dint(a)
       GOTO 5480
 5490 IF(.NOT.(ndx.EQ.317)) GOTO 5491
       c=dnint(a)
       GOTO 5480
 5491 IF(.NOT.(ndx.EQ.319)) GOTO 5492
       IF(.NOT.(a.LT.rmargn)) GOTO 5561
        c=-one
        GOTO 5560
 5561  IF(.NOT.(a.GT.rmargn)) GOTO 5562
        c=one
        GOTO 5560
 5562  CONTINUE
        c=zero
 5560  CONTINUE
       GOTO 5480
 5492 IF(.NOT.(ndx.EQ.321)) GOTO 5493
       c=a /180.0 *pi
       GOTO 5480
 5493 IF(.NOT.(ndx.EQ.323)) GOTO 5494
       c=a *180.0 /pi
       GOTO 5480
 5494 IF(.NOT.(ndx.EQ.325)) GOTO 5495
       c=randnm(zero)    !WSM + THL
       GOTO 5480
 5495 IF(.NOT.(ndx.EQ.327)) GOTO 5496
       c=dsin(a)
       GOTO 5480
 5496 IF(.NOT.(ndx.EQ.329)) GOTO 5497
       c=dcos(a)
       GOTO 5480
 5497 IF(.NOT.(ndx.EQ.331)) GOTO 5498
       c=dtan(a)
       GOTO 5480
 5498 IF(.NOT.(ndx.EQ.333)) GOTO 5499
       IF(.NOT.(a.LT.-one-rmargn)) GOTO 5571
        stpflg=7
        GOTO 5570
 5571  IF(.NOT.(a.LT.-one)) GOTO 5572
        a=-one
        GOTO 5570
 5572  IF(.NOT.(a.GT.one+rmargn)) GOTO 5573
        stpflg=7
        GOTO 5570
 5573  IF(.NOT.(a.GT.one)) GOTO 5574
        a=one
        GOTO 5570
 5574  CONTINUE
 5570  CONTINUE
       IF(.NOT.(stpflg.EQ.0)) GOTO 5581
        c=dasin(a)
        GOTO 5580
 5581  CONTINUE
 5580  CONTINUE
       GOTO 5480
 5499 IF(.NOT.(ndx.EQ.335)) GOTO 5500
       IF(.NOT.(a.LT.-one-rmargn)) GOTO 5591
        stpflg=8
        GOTO 5590
 5591  IF(.NOT.(a.LT.-one)) GOTO 5592
        a=-one
        GOTO 5590
 5592  IF(.NOT.(a.GT.one+rmargn)) GOTO 5593
        stpflg=8
        GOTO 5590
 5593  IF(.NOT.(a.GT.one)) GOTO 5594
        a=one
        GOTO 5590
 5594  CONTINUE
 5590  CONTINUE
       IF(.NOT.(stpflg.EQ.0)) GOTO 5601
        c=dacos(a)
        GOTO 5600
 5601  CONTINUE
 5600  CONTINUE
       GOTO 5480
 5500 IF(.NOT.(ndx.EQ.337)) GOTO 5501
       c=datan(a)
       GOTO 5480
 5501 IF(.NOT.(ndx.EQ.339)) GOTO 5502
       c=dsinh(a)
       GOTO 5480
 5502 IF(.NOT.(ndx.EQ.341)) GOTO 5503
       c=dcosh(a)
       GOTO 5480
 5503 IF(.NOT.(ndx.EQ.343)) GOTO 5504
       c=dtanh(a)
       GOTO 5480
 5504 IF(.NOT.(ndx.EQ.345)) GOTO 5505
       c=rnull
       GOTO 5480
 5505 IF(.NOT.(ndx.EQ.347)) GOTO 5506
       IF(.NOT.(a.LT.one-rmargn)) GOTO 5611
        stpflg=9
        GOTO 5610
 5611  IF(.NOT.(a.LT.one)) GOTO 5612
        c=zero
        GOTO 5610
 5612  CONTINUE
        c=rnull
 5610  CONTINUE
       GOTO 5480
 5506 IF(.NOT.(ndx.EQ.349)) GOTO 5507
       IF(.NOT.(a.LT.-one-rmargn)) GOTO 5621
        stpflg=10
        GOTO 5620
 5621  IF(.NOT.(a.LT.-one)) GOTO 5622
        a=-one
        GOTO 5620
 5622  IF(.NOT.(a.GT.one+rmargn)) GOTO 5623
        stpflg=10
        GOTO 5620
 5623  IF(.NOT.(a.GT.one)) GOTO 5624
        a=one
        GOTO 5620
 5624  CONTINUE
 5620  CONTINUE
       IF(.NOT.(stpflg.EQ.0)) GOTO 5631
        c=rnull
        GOTO 5630
 5631  CONTINUE
 5630  CONTINUE
       GOTO 5480
 5507 IF(.NOT.(ndx.EQ.363)) GOTO 5508
       IF(.NOT.(a.GT.rmargn)) GOTO 5641
        c=one
        GOTO 5640
 5641  CONTINUE
        c=zero
 5640  CONTINUE
       GOTO 5480
 5508 CONTINUE
 5480 CONTINUE
      IF(.NOT.(stpflg.GT.0)) GOTO 5651
       stpr1=rsto(ptr6+1)
       CONTINUE
       CALL errstp
       GOTO 5650
 5651 CONTINUE
 5650 CONTINUE
      rsto(ptr6+1)=c
      GOTO 9500
 9023 CONTINUE
      IF(.NOT.(cnt.NE.2)) GOTO 5661
       stpflg=11
       stpi1=cnt
       CONTINUE
       CALL errstp
       GOTO 5660
 5661 CONTINUE
 5660 CONTINUE
      a=rsto(ptr6+1)
      b=rsto(ptr6+2)
      IF(.NOT.(dabs(a).LE.rmargn
     1.AND. dabs(a).LE.rmargn)) GOTO 5851
       stpflg=131
       stpr1=a
       stpr2=b
       CONTINUE
       CALL errstp
       GOTO 5850
 5851 CONTINUE
 5850 CONTINUE
      c=datan(a/b)
      IF(.NOT.(b.LT.zero)) GOTO 5671
       IF(.NOT.(c.LT.zero)) GOTO 5681
        c=c+pi
        GOTO 5680
 5681  CONTINUE
        c=c-pi
 5680  CONTINUE
       GOTO 5670
 5671 CONTINUE
 5670 CONTINUE
      rptr=ptr6+1
      rsto(rptr)=c
      GOTO 9500
 9024 CONTINUE
      IF(.NOT.(cnt.NE.2)) GOTO 5691
       stpflg=12
       stpi1=cnt
       CONTINUE
       CALL errstp
       GOTO 5690
 5691 CONTINUE
 5690 CONTINUE
      a=rsto(rptr)
      a=dnint(a)
      rptr=rptr-1
      b=rsto(rptr)
      b=dnint(b)
      IF(.NOT.(a.LT.zero .OR. b.LE.zero
     1.OR. a.GT.b)) GOTO 5701
       stpflg=13
       stpr1=rsto(rptr)
       stpr2=rsto(rptr+1)
       CONTINUE
       CALL errstp
       GOTO 5700
 5701 CONTINUE
 5700 CONTINUE
      IF(.NOT.(a.EQ.zero .OR. a.EQ.b)) GOTO 5711
       c=one
       GOTO 5710
 5711 CONTINUE
       d=b-a
       IF(.NOT.(a.GT.d)) GOTO 5721
        e=a
        a=d
        d=e
        GOTO 5720
 5721  CONTINUE
 5720  CONTINUE
       c=one
 3171  IF(.NOT.(d.LT.b)) GOTO 3170
        d=d+one
        c=c*d
        GOTO 3171
 3170  CONTINUE
 3181  IF(.NOT.(a.GT.one)) GOTO 3180
        c=c/a
        a=a-one
        GOTO 3181
 3180  CONTINUE
 5710 CONTINUE
      rsto(rptr)=c
      GOTO 9500
 9025 CONTINUE
      IF(.NOT.(cnt.NE.2)) GOTO 5731
       stpflg=14
       stpi1=cnt
       CONTINUE
       CALL errstp
       GOTO 5730
 5731 CONTINUE
 5730 CONTINUE
      a=rsto(rptr)
      a=dnint(a)
      rptr=rptr-1
      b=rsto(rptr)
      b=dnint(b)
      IF(.NOT.(a.LT.zero .OR. b.LE.zero
     1.OR. a.GT.b)) GOTO 5741
       stpflg=15
       stpr1=rsto(rptr)
       stpr2=rsto(rptr+1)
       CONTINUE
       CALL errstp
       GOTO 5740
 5741 CONTINUE
 5740 CONTINUE
      d=b-a
      c=one
 3191 IF(.NOT.(d.LT.b)) GOTO 3190
       d=d+one
       c=c*d
       GOTO 3191
 3190 CONTINUE
      rsto(rptr)=c
      GOTO 9500
 9026 CONTINUE
      a=rsto(rptr)
 3201 IF(.NOT.(rptr.GT.ptr6+1)) GOTO 3200
       rptr=rptr-1
       b=rsto(rptr)
       IF(.NOT.(b.LT.a)) GOTO 5751
        a=b
        GOTO 5750
 5751  CONTINUE
 5750  CONTINUE
       GOTO 3201
 3200 CONTINUE
      rsto(rptr)=a
      GOTO 9500
 9027 CONTINUE
      a=rsto(rptr)
 3211 IF(.NOT.(rptr.GT.ptr6+1)) GOTO 3210
       rptr=rptr-1
       b=rsto(rptr)
       IF(.NOT.(b.GT.a)) GOTO 5761
        a=b
        GOTO 5760
 5761  CONTINUE
 5760  CONTINUE
       GOTO 3211
 3210 CONTINUE
      rsto(rptr)=a
      GOTO 9500
 9028 CONTINUE
      a=rsto(rptr)
      a=a*a
 3221 IF(.NOT.(rptr.GT.ptr6+1)) GOTO 3220
       rptr=rptr-1
       b=rsto(rptr)
       a=a +b*b
       GOTO 3221
 3220 CONTINUE
      rsto(rptr)=dsqrt(a)
      GOTO 9500
 9029 CONTINUE
      a=rsto(rptr)
 3231 IF(.NOT.(rptr.GT.ptr6+1 .AND. a.GT.rmargn
     1)) GOTO 3230
       rptr=rptr-1
       a=rsto(rptr)
       GOTO 3231
 3230 CONTINUE
      IF(.NOT.(a.GT.rmargn)) GOTO 5771
       a=zero
       GOTO 5770
 5771 CONTINUE
       a=one
 5770 CONTINUE
      rptr=ptr6+1
      rsto(rptr)=a
      GOTO 9500
 9030 CONTINUE
      a=rsto(rptr)
 3241 IF(.NOT.(rptr.GT.ptr6+1 .AND. a.LE.rmargn
     1)) GOTO 3240
       rptr=rptr-1
       a=rsto(rptr)
       GOTO 3241
 3240 CONTINUE
      IF(.NOT.(a.LE.rmargn)) GOTO 5781
       a=one
       GOTO 5780
 5781 CONTINUE
       a=zero
 5780 CONTINUE
      rptr=ptr6+1
      rsto(rptr)=a
      GOTO 9500
 9031 CONTINUE
      k=0
 3251 IF(.NOT.(rptr.GE.ptr6+1)) GOTO 3250
       IF(.NOT.(rsto(rptr).GT.rmargn)) GOTO 5791
        k=k+1
        GOTO 5790
 5791  CONTINUE
 5790  CONTINUE
       rptr=rptr-1
       GOTO 3251
 3250 CONTINUE
      IF(.NOT.(k.EQ.1)) GOTO 5801
       a=one
       GOTO 5800
 5801 CONTINUE
       a=zero
 5800 CONTINUE
      rptr=ptr6+1
      rsto(rptr)=a
      GOTO 9500
 9032 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(ishenv+7)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx7
                 ndx7=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9525
      GOTO 9034
 9525 from=isto(sptr)
      sptr=sptr+1
      ndx7=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9033 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(ishenv+8)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx7
                 ndx7=2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9526
      GOTO 9034
 9526 from=isto(sptr)
      sptr=sptr+1
      ndx7=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9034 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
      cnt0=isto(env+1)
      ndx0=((ndx4-1)/cnt0)*cnt0
      ndx1=((ndx6-1)/cnt0)*cnt0
      ndx2=ndx4
      ndx3=ndx6
 3351 IF(.NOT.(ndx0.LE.ndx1)) GOTO 3350
       IF(.NOT.(ndx0.EQ.ndx1)) GOTO 6171
        ndx6=ndx3
        GOTO 6170
 6171  CONTINUE
        ndx6=ndx0 +cnt0
 6170  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9527
       GOTO 9035
 9527  from=isto(sptr)
       sptr=sptr+1
       ndx0=ndx0 +cnt0
       ndx4=ndx0 +1
       GOTO 3351
 3350 CONTINUE
      ndx4=ndx2
      ndx6=ndx3
      ndx3=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      cnt0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9035 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                 ptr2=isto(env+3)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr7
                 ptr7=isto(env+0)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
      env=env+6+5*(ptr2-1)
      ptr3=isto(env+1)
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx0.NE.ptr3)) GOTO 6181
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=env+1
       k=0
       i=0
 3361  IF(.NOT.(i.LT.ptr7 .AND. k.EQ.0)) GOTO 3360
        i=i+1
        env=env+5
        IF(.NOT.(ndx0.EQ.isto(env+1))) GOTO 6191
         k=1
         ptr2=i
         GOTO 6190
 6191   CONTINUE
 6190   CONTINUE
        GOTO 3361
 3360  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(k.EQ.0)) GOTO 6201
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                  env=env+1
        i=0
        k=iinf
 3371   IF(.NOT.(i.LT.ptr7)) GOTO 3370
         i=i+1
         env=env+5
         j=isto(env+3)
         IF(.NOT.(j.LT.k)) GOTO 6211
          k=j
          ptr2=i
          GOTO 6210
 6211    CONTINUE
 6210    CONTINUE
         GOTO 3371
 3370   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
        env=env+6+5*(ptr2-1)
        k=isto(env+0)
        IF(.NOT.(isto(env+4).GT.0)) GOTO 6221
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=env
                    env=isto(env+2)
         ptr4=isto(env+2)
         env=isto(sptr)
         sptr=sptr+1
         IF(.NOT.(ndx7.EQ.1)) GOTO 6231
          WRITE(bkfile, rec=ptr4)
     1           (isto(k+i),i=1,cnt0)
          GOTO 6230
 6231    IF(.NOT.(ndx7.EQ.2)) GOTO 6232
          WRITE(bkfile, rec=ptr4)
     1           (rsto(k+i),i=1,cnt0)
          GOTO 6230
 6232    CONTINUE
 6230    CONTINUE
         GOTO 6220
 6221   CONTINUE
 6220   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        ptr5=ndx0/cnt0 +1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                  env=isto(env+4)
        k1=0
 3381   IF(.NOT.(env.GT.0 .AND. k1.EQ.0)) GOTO 3380
         IF(.NOT.(isto(env+1).EQ.ptr5)) GOTO 6241
          k1=1
          ptr4=isto(env+2)
          ptr6=env
          GOTO 6240
 6241    CONTINUE
 6240    CONTINUE
         env=isto(env+0)
         GOTO 3381
 3380   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(k1.GT.0)) GOTO 6251
         IF(.NOT.(ndx7.EQ.1)) GOTO 6261
          READ(bkfile, rec=ptr4)
     1           (isto(k+i),i=1,cnt0)
          GOTO 6260
 6261    IF(.NOT.(ndx7.EQ.2)) GOTO 6262
          READ(bkfile, rec=ptr4)
     1           (rsto(k+i),i=1,cnt0)
          GOTO 6260
 6262    CONTINUE
 6260    CONTINUE
         GOTO 6250
 6251   CONTINUE
         ptr4=isto(ishenv+9)+1
         isto(ishenv+9)=ptr4
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=env
                    env=isto(env+5)
         isto(env+0)=iptr+1
         env=iptr+1
         iptr=iptr+3
         isto(env+0)=0
         isto(env+1)=ptr5
         isto(env+2)=ptr4
         ptr6=env
         env=isto(sptr)
         sptr=sptr+1
         isto(env+5)=ptr6
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=env
         env=env+6+5*(ptr2-1)
         k=isto(env+0)
         i=0
         IF(.NOT.(ndx7.EQ.1)) GOTO 6301
 3421     IF(.NOT.(i.LT.cnt0)) GOTO 3420
           i=i+1
           isto(k+i)=inull
           GOTO 3421
 3420     CONTINUE
          GOTO 6300
 6301    CONTINUE
 3391     IF(.NOT.(i.LT.cnt0)) GOTO 3390
           i=i+1
           rsto(k+i)=rnull
           GOTO 3391
 3390     CONTINUE
 6300    CONTINUE
         env=isto(sptr)
         sptr=sptr+1
 6250   CONTINUE
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
        env=env+6+5*(ptr2-1)
        isto(env+1)=ndx0
        isto(env+2)=ptr6
        isto(env+4)=0
        env=isto(sptr)
        sptr=sptr+1
        GOTO 6200
 6201  CONTINUE
 6200  CONTINUE
       GOTO 6180
 6181 CONTINUE
 6180 CONTINUE
      k=isto(env+2)
      k=k+1
      isto(env+2)=k
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
      env=env+6+5*(ptr2-1)
      isto(env+3)=k
      k1=ndx6 -ndx4 +1
      k2=isto(env+0) -ndx0 +ndx4 -1
      k3=ptr0+ndx4-ndx2
      i=0
      IF(.NOT.(ndx.EQ.0)) GOTO 6271
 3401  IF(.NOT.(i.LT.k1)) GOTO 3400
        i=i+1
        IF(.NOT.(ndx7.EQ.1)) GOTO 6281
         isto(k3+i)=isto(k2+i)
         GOTO 6280
 6281   IF(.NOT.(ndx7.EQ.2)) GOTO 6282
         rsto(k3+i)=rsto(k2+i)
         GOTO 6280
 6282   CONTINUE
 6280   CONTINUE
        GOTO 3401
 3400  CONTINUE
       GOTO 6270
 6271 CONTINUE
       isto(env+4)=1
 3411  IF(.NOT.(i.LT.k1)) GOTO 3410
        i=i+1
        IF(.NOT.(ndx7.EQ.1)) GOTO 6291
         isto(k2+i)=isto(k3+i)
         GOTO 6290
 6291   IF(.NOT.(ndx7.EQ.2)) GOTO 6292
         rsto(k2+i)=rsto(k3+i)
         GOTO 6290
 6292   CONTINUE
 6290   CONTINUE
        GOTO 3411
 3410  CONTINUE
 6270 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ptr7=isto(sptr)
      sptr=sptr+1
      ptr6=isto(sptr)
      sptr=sptr+1
      ptr5=isto(sptr)
      sptr=sptr+1
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9036 CONTINUE
      WRITE(cbuff,'(i12)') ndx
      i=0
 3261 IF(.NOT.(i.LT.12)) GOTO 3260
       i=i+1
       char1=cbuff(i:i)
       IF(.NOT.(char1.NE.csto(64))) GOTO 5831
        cptr=cptr+1
        csto(cptr)=char1
        GOTO 5830
 5831  CONTINUE
 5830  CONTINUE
       GOTO 3261
 3260 CONTINUE
      GOTO 9500
 9037 CONTINUE
      i4=isto(base4+ndx5)
      i6=isto(base6+ndx5)
      IF(.NOT.(ndx4.LT.i4
     1.OR. ndx6.GT.i6
     2)) GOTO 5841
       stpflg=129
       stpi1=ndx4
       stpi2=ndx6
       stpi3=i4
       stpi4=i6
       CONTINUE
       CALL errstp
       GOTO 5840
 5841 CONTINUE
 5840 CONTINUE
      GOTO 9500
      END
