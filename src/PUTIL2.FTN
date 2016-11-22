      SUBROUTINE putil2
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9200,
     1 9201, 9202, 9203, 9204, 9205,
     1 9206, 9207, 9208, 0002, 9210,
     1 9211, 9212, 9213, 9214), to-9199
 0002 STOP 'Invalid "to" reference in "putil2".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501,  9502,  9503,  9504,  9505,
     1  9506,  9507,  0003,  0003,  0003,
     1  0003,  0003,  9513,  9514,  9515,
     1  9516,  9517,  9518,  9519,  9520,
     1  9521,  9522,  9523,  0003,  0003,
     1  0003,  0003,  0003,  0003,  0003,
     1  9531,  9532,  9533,  9534,  9535,
     1  9536,  9537,  9538), from-9500
 0003 STOP 'Invalid "from" reference in "putil2".'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9200 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9501
      GOTO 9210
 9501 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(mflg.EQ.0)) GOTO 5001
       isto(ishenv+54)=53
       CONTINUE
       CALL synstp
       GOTO 5000
 5001 CONTINUE
 5000 CONTINUE
 3001 IF(.NOT.(mflg.GT.0)) GOTO 3000
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9502
       GOTO 9210
 9502  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.EQ.0)) GOTO 5011
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9503
        GOTO 9211
 9503   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.EQ.0)) GOTO 5021
         mndx=73
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9504
         GOTO 9208
 9504    from=isto(sptr)
         sptr=sptr+1
         GOTO 5020
 5021   CONTINUE
 5020   CONTINUE
        GOTO 5010
 5011  CONTINUE
 5010  CONTINUE
       GOTO 3001
 3000 CONTINUE
      GOTO 9500
 9201 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3011 IF(.NOT.(lpflg.GT.0)) GOTO 3010
       mndx=75
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9505
       GOTO 9208
 9505  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.GT.0)) GOTO 5031
        lpflg=0
        GOTO 5030
 5031  CONTINUE
        dptr=dptr+1
 5030  CONTINUE
       GOTO 3011
 3010 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9202 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3021 IF(.NOT.(lpflg.GT.0)) GOTO 3020
       mndx=77
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9506
       GOTO 9208
 9506  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.GT.0)) GOTO 5041
        lpflg=0
        GOTO 5040
 5041  CONTINUE
        dptr=dptr+1
 5040  CONTINUE
       GOTO 3021
 3020 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9203 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=-1
      mflg=1
 3031 IF(.NOT.(mflg.GT.0)) GOTO 3030
       ndx=ndx+1
       mndx=121
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9507
       GOTO 9208
 9507  from=isto(sptr)
       sptr=sptr+1
       GOTO 3031
 3030 CONTINUE
      IF(.NOT.(ndx.GT.0)) GOTO 5101
       mflg=1
       GOTO 5100
 5101 CONTINUE
       mflg=0
 5100 CONTINUE
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9204 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=mflg
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9513
      GOTO 9203
 9513 from=isto(sptr)
      sptr=sptr+1
      mflg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9205 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9514
      GOTO 9208
 9514 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(mflg.GT.0)) GOTO 5111
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9515
       GOTO 9204
 9515  from=isto(sptr)
       sptr=sptr+1
       GOTO 5110
 5111 CONTINUE
 5110 CONTINUE
      GOTO 9500
 9206 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=dptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9516
      GOTO 9208
 9516 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(mflg.GT.0)) GOTO 5121
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9517
       GOTO 9203
 9517  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.EQ.0)) GOTO 5131
        dptr=ptr1
        GOTO 5130
 5131  CONTINUE
 5130  CONTINUE
       GOTO 5120
 5121 CONTINUE
 5120 CONTINUE
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9207 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=dptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9518
      GOTO 9208
 9518 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(mflg.GT.0)) GOTO 5141
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9519
       GOTO 9203
 9519  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.EQ.0)) GOTO 5151
        mndx=83
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9520
        GOTO 9208
 9520   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.EQ.0)) GOTO 5161
         mndx=79
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9521
         GOTO 9208
 9521    from=isto(sptr)
         sptr=sptr+1
         IF(.NOT.(mflg.EQ.0)) GOTO 5171
          mndx=91
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9522
          GOTO 9208
 9522     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(mflg.EQ.0)) GOTO 5181
           mndx=93
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=from
           from=9523
           GOTO 9208
 9523      from=isto(sptr)
           sptr=sptr+1
           GOTO 5180
 5181     CONTINUE
 5180     CONTINUE
          GOTO 5170
 5171    CONTINUE
 5170    CONTINUE
         GOTO 5160
 5161   CONTINUE
 5160   CONTINUE
        GOTO 5150
 5151  CONTINUE
 5150  CONTINUE
       GOTO 5140
 5141 CONTINUE
 5140 CONTINUE
      dptr=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9208 CONTINUE
      j=base1 +mndx
      k=base2 +isto(j) -1
      j=isto(j+1)
      i=1
 3041 IF(.NOT.(i.LE.j
     1.AND. csto(k+i).EQ.csto(dptr+i)
     2)) GOTO 3040
       i=i+1
       GOTO 3041
 3040 CONTINUE
      IF(.NOT.(i.GT.j)) GOTO 5191
        mflg=1
        dptr=dptr+j
       GOTO 5190
 5191 CONTINUE
       mflg=0
 5190 CONTINUE
      GOTO 9500
 9210 CONTINUE
      k=base2 +isto(base1+1) -1
      i=0
      mflg=0
 3081 IF(.NOT.(i.LT.26 .AND. mflg.EQ.0)) GOTO 3080
       i=i+1
       IF(.NOT.(csto(k+i).EQ.csto(dptr+1))) GOTO 5241
        mflg=1
        GOTO 5240
 5241  CONTINUE
 5240  CONTINUE
       GOTO 3081
 3080 CONTINUE
      IF(.NOT.(mflg.GT.0)) GOTO 5251
       dptr=dptr+1
       GOTO 5250
 5251 CONTINUE
 5250 CONTINUE
      GOTO 9500
 9211 CONTINUE
      k=base2 +isto(base1+53) -1
      i=0
      mflg=0
 3091 IF(.NOT.(i.LT.10 .AND. mflg.EQ.0)) GOTO 3090
       i=i+1
       IF(.NOT.(csto(k+i).EQ.csto(dptr+1))) GOTO 5261
        mflg=1
        GOTO 5260
 5261  CONTINUE
 5260  CONTINUE
       GOTO 3091
 3090 CONTINUE
      IF(.NOT.(mflg.GT.0)) GOTO 5271
       dptr=dptr+1
       GOTO 5270
 5271 CONTINUE
 5270 CONTINUE
      GOTO 9500
 9212 CONTINUE
      k=base2 +isto(base1+53)
      ndx=-1
      i1=0
 3101 IF(.NOT.(ndx.LT.9 .AND. i1.EQ.0)) GOTO 3100
       ndx=ndx+1
       IF(.NOT.(csto(k+ndx).EQ.csto(dptr+1))) GOTO 5281
        i1=1
        GOTO 5280
 5281  CONTINUE
 5280  CONTINUE
       GOTO 3101
 3100 CONTINUE
      IF(.NOT.(i1.GT.0)) GOTO 5291
       dptr=dptr+1
       GOTO 5290
 5291 CONTINUE
       ndx=-1
 5290 CONTINUE
      GOTO 9500
 9213 CONTINUE
      k=base2 +isto(base1+53)
      i=-1
      a=-one
      mflg=0
 3111 IF(.NOT.(i.LT.9 .AND. mflg.EQ.0)) GOTO 3110
       i=i+1
       a=a +one
       IF(.NOT.(csto(k+i).EQ.csto(dptr+1))) GOTO 5301
        mflg=1
        GOTO 5300
 5301  CONTINUE
 5300  CONTINUE
       GOTO 3111
 3110 CONTINUE
      IF(.NOT.(mflg.GT.0)) GOTO 5311
       dptr=dptr+1
       rptr=rptr+1
       rsto(rptr)=a
       GOTO 5310
 5311 CONTINUE
 5310 CONTINUE
      GOTO 9500
 9214 CONTINUE
      rptr=rptr+4
      rsto(rptr)=zero
      rsto(rptr-1)=one
      rsto(rptr-2)=one
      rsto(rptr-3)=zero
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3121 IF(.NOT.(lpflg.GT.0)) GOTO 3120
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9531
       GOTO 9213
 9531  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.GT.0)) GOTO 5321
        a=rsto(rptr)
        rptr=rptr-1
        rsto(rptr)=10.0D0 *rsto(rptr) +a
        GOTO 5320
 5321  CONTINUE
        lpflg=0
 5320  CONTINUE
       GOTO 3121
 3120 CONTINUE
      mndx=113
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9532
      GOTO 9208
 9532 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(mflg.GT.0)) GOTO 5331
       lpflg=1
 3131  IF(.NOT.(lpflg.GT.0)) GOTO 3130
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9533
        GOTO 9213
 9533   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.GT.0)) GOTO 5341
         a=rsto(rptr)
         rptr=rptr-1
         rsto(rptr-1)=0.1 *rsto(rptr-1)
         rsto(rptr)=rsto(rptr) +rsto(rptr-1) *a
         GOTO 5340
 5341   CONTINUE
         lpflg=0
 5340   CONTINUE
        GOTO 3131
 3130  CONTINUE
       GOTO 5330
 5331 CONTINUE
 5330 CONTINUE
      mndx=9
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9534
      GOTO 9208
 9534 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(mflg.GT.0)) GOTO 5351
       mndx=91
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9535
       GOTO 9208
 9535  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.EQ.0)) GOTO 5361
        mndx=93
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9536
        GOTO 9208
 9536   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.GT.0)) GOTO 5371
         rsto(rptr-2)=-one
         GOTO 5370
 5371   CONTINUE
 5370   CONTINUE
        GOTO 5360
 5361  CONTINUE
 5360  CONTINUE
       lpflg=1
 3141  IF(.NOT.(lpflg.GT.0)) GOTO 3140
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9537
        GOTO 9213
 9537   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.GT.0)) GOTO 5381
         a=rsto(rptr)
         rptr=rptr-1
         rsto(rptr-3)=10.0D0 *rsto(rptr-3) +a
         GOTO 5380
 5381   CONTINUE
         lpflg=0
 5380   CONTINUE
        GOTO 3141
 3140  CONTINUE
       GOTO 5350
 5351 CONTINUE
 5350 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9538
      GOTO 9204
 9538 from=isto(sptr)
      sptr=sptr+1
      b=dlog10(rinf)
      c=dlog10(rmargn)
      e=rsto(rptr-2) *rsto(rptr-3)
      IF(.NOT.(e.GE.b)) GOTO 5391
       a=rinf
       GOTO 5390
 5391 IF(.NOT.(e.LE.c)) GOTO 5392
       a=zero
       GOTO 5390
 5392 CONTINUE
       a=rsto(rptr)*(10.0D0**e)
 5390 CONTINUE
      IF(.NOT.(flg1.GT.0)) GOTO 5401
       a=-a
       GOTO 5400
 5401 CONTINUE
 5400 CONTINUE
      rptr=rptr-3
      rsto(rptr)=a
      GOTO 9500
      END
