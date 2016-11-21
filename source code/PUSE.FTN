      SUBROUTINE puse
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO 9000
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1 0002, 9502, 9503, 9504, 9505,
     1 9506, 9507, 9508, 9509, 9510,
     1 9511, 9512, 9513), from-9500
 0002 STOP 'Invalid from reference in puse.'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
                 ptr3=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=0
      iptr=iptr+12
      ipn=isto(ishenv+25)
      isto(ipn)=env
                  isto(ishenv+25)=env
      isto(env+0)=0
      isto(env+5)=dptr+1
      to=9200
      CALL putil2
      isto(env+4)=dptr -isto(env+5)+1
      to=9204
      CALL putil2
      mndx=255
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5071
       isto(ishenv+54)=38
       CONTINUE
       CALL synstp
       GOTO 5070
 5071 CONTINUE
 5070 CONTINUE
      isto(env+7)=dptr+1
      to=9200
      CALL putil2
      isto(env+6)=dptr -isto(env+7)+1
      to=9204
      CALL putil2
      mndx=79
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5081
       isto(env+8)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=43
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       mndx=81
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5091
        isto(ishenv+54)=39
        CONTINUE
        CALL synstp
        GOTO 5090
 5091  CONTINUE
 5090  CONTINUE
       GOTO 5080
 5081 CONTINUE
       isto(env+8)=0
 5080 CONTINUE
      i3=isto(env+4)
      i4=isto(env+5)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                  ndx=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+26)
 3021 IF(.NOT.(env.GT.0 .AND. ndx.EQ.0)) GOTO 3020
       i1=isto(env+2)
       IF(.NOT.(i1.EQ.i3)) GOTO 5101
        i=0
        i2=isto(env+3)
 3031   IF(.NOT.(i.LT.i3
     1  .AND. csto(i4+i).EQ.csto(i2+i)
     2  )) GOTO 3030
         i=i+1
         GOTO 3031
 3030   CONTINUE
        IF(.NOT.(i.EQ.i3)) GOTO 5111
         ndx=isto(env+4)
         ptr1=isto(env+5)
         GOTO 5110
 5111   CONTINUE
 5110   CONTINUE
        GOTO 5100
 5101  CONTINUE
 5100  CONTINUE
       env=isto(env+0)
       GOTO 3021
 3020 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx.EQ.0)) GOTO 5121
       stpflg=98
       stpl1=i3
       stpc1=i4
       CONTINUE
       CALL errstp
       GOTO 5120
 5121 CONTINUE
 5120 CONTINUE
      isto(env+1)=ndx
      isto(env+3)=ptr1
      isto(ishenv+12)=ptr1
      ptr1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      ptr8=ptr8+1
      isto(env+2)=ptr8
      isto(env+11)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3041 IF(.NOT.(lpflg.GT.0)) GOTO 3040
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9502
       GOTO 9002
 9502  from=isto(sptr)
       sptr=sptr+1
       mndx=241
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5131
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5130
 5131  CONTINUE
        isto(env+0)=0
        lpflg=0
 5130  CONTINUE
       GOTO 3041
 3040 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      isto(ishenv+12)=0
      isto(env+9)=ptr3
      isto(env+10)=ptr4
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9002 CONTINUE
      iptr=iptr+3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=dptr
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      mndx=259
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5141
       mndx=263
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5151
        ndx=2
        GOTO 5150
 5151  CONTINUE
        ndx=1
 5150  CONTINUE
       mndx=265
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5161
        mndx=255
        to=9206
        CALL putil2
        GOTO 5160
 5161  CONTINUE
 5160  CONTINUE
       GOTO 5140
 5141 CONTINUE
       mndx=261
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5171
        mndx=263
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5181
         ndx=4
         GOTO 5180
 5181   CONTINUE
         ndx=3
 5180   CONTINUE
        mndx=265
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5191
         mndx=251
         to=9206
         CALL putil2
         GOTO 5190
 5191   CONTINUE
 5190   CONTINUE
        GOTO 5170
 5171  CONTINUE
 5170  CONTINUE
 5140 CONTINUE
      IF(.NOT.(mflg.GT.0)) GOTO 5201
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9503
       GOTO 9003
 9503  from=isto(sptr)
       sptr=sptr+1
       GOTO 5200
 5201 CONTINUE
       ndx=0
       dptr=ptr1
       mndx=181
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5211
        ndx=5
        GOTO 5210
 5211  CONTINUE
        mndx=183
        to=9206
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5221
         ndx=6
         GOTO 5220
 5221   CONTINUE
         mndx=177
         to=9206
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5231
          ndx=7
          GOTO 5230
 5231    CONTINUE
          mndx=185
          to=9206
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5241
           ndx=8
           GOTO 5240
 5241     CONTINUE
           mndx=187
           to=9206
           CALL putil2
           IF(.NOT.(mflg.GT.0)) GOTO 5251
            ndx=9
            GOTO 5250
 5251      CONTINUE
            mndx=189
            to=9206
            CALL putil2
            IF(.NOT.(mflg.GT.0)) GOTO 5261
             ndx=10
             GOTO 5260
 5261       CONTINUE
 5260       CONTINUE
 5250      CONTINUE
 5240     CONTINUE
 5230    CONTINUE
 5220   CONTINUE
 5210  CONTINUE
       IF(.NOT.(ndx.EQ.0)) GOTO 5271
        isto(ishenv+54)=40
        CONTINUE
        CALL synstp
        GOTO 5270
 5271  CONTINUE
 5270  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9504
       GOTO 9004
 9504  from=isto(sptr)
       sptr=sptr+1
 5200 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(env+1)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9003 CONTINUE
      iptr=iptr+7
      IF(.NOT.(ndx.LT.3)) GOTO 5281
       ipn=isto(ishenv+26)
       isto(ipn)=env
                  isto(ishenv+26)=env
       GOTO 5280
 5281 CONTINUE
       ipn=isto(ishenv+27)
       isto(ipn)=env
                  isto(ishenv+27)=env
 5280 CONTINUE
      isto(env+0)=0
      isto(env+1)=ndx
      isto(env+3)=dptr+1
      to=9200
      CALL putil2
      isto(env+2)=dptr -isto(env+3)+1
      to=9204
      CALL putil2
      mndx=79
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5291
       isto(env+4)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=44
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       mndx=81
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5301
        isto(ishenv+54)=41
        CONTINUE
        CALL synstp
        GOTO 5300
 5301  CONTINUE
 5300  CONTINUE
       GOTO 5290
 5291 CONTINUE
       isto(env+4)=0
 5290 CONTINUE
      mndx=267
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5311
       isto(ishenv+54)=42
       CONTINUE
       CALL synstp
       GOTO 5310
 5311 CONTINUE
 5310 CONTINUE
      isto(env+5)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=45
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9004 CONTINUE
      iptr=iptr+1
      isto(env+0)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3051 IF(.NOT.(lpflg.GT.0)) GOTO 3050
       IF(.NOT.(ndx.EQ.5)) GOTO 5321
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9505
        GOTO 9006
 9505   from=isto(sptr)
        sptr=sptr+1
        GOTO 5320
 5321  IF(.NOT.(ndx.EQ.6)) GOTO 5322
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9506
        GOTO 9007
 9506   from=isto(sptr)
        sptr=sptr+1
        GOTO 5320
 5322  IF(.NOT.(ndx.EQ.7)) GOTO 5323
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9507
        GOTO 9008
 9507   from=isto(sptr)
        sptr=sptr+1
        GOTO 5320
 5323  IF(.NOT.(ndx.EQ.8)) GOTO 5324
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9508
        GOTO 9009
 9508   from=isto(sptr)
        sptr=sptr+1
        GOTO 5320
 5324  IF(.NOT.(ndx.EQ.9)) GOTO 5325
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9509
        GOTO 9010
 9509   from=isto(sptr)
        sptr=sptr+1
        GOTO 5320
 5325  IF(.NOT.(ndx.EQ.10)) GOTO 5326
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9510
        GOTO 9011
 9510   from=isto(sptr)
        sptr=sptr+1
        GOTO 5320
 5326  CONTINUE
 5320  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9511
       GOTO 9005
 9511  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.EQ.0)) GOTO 5331
        IF(.NOT.(ndx.EQ.8)) GOTO 5591
         isto(env+2)=iptr+1
         GOTO 5590
 5591   CONTINUE
         isto(env+0)=iptr+1
 5590   CONTINUE
        env=iptr+1
        GOTO 5330
 5331  CONTINUE
        IF(.NOT.(ndx.EQ.8)) GOTO 5601
         isto(env+2)=0
         GOTO 5600
 5601   CONTINUE
         isto(env+0)=0
 5600   CONTINUE
        lpflg=0
 5330  CONTINUE
       GOTO 3051
 3050 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9005 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=dptr
      mndx=241
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5341
       mndx=181
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5351
        mndx=183
        to=9206
        CALL putil2
        IF(.NOT.(mflg.EQ.0)) GOTO 5361
         mndx=177
         to=9206
         CALL putil2
         IF(.NOT.(mflg.EQ.0)) GOTO 5371
          mndx=185
          to=9206
          CALL putil2
          IF(.NOT.(mflg.EQ.0)) GOTO 5381
           mndx=187
           to=9206
           CALL putil2
           IF(.NOT.(mflg.EQ.0)) GOTO 5391
            mndx=189
            to=9206
            CALL putil2
            IF(.NOT.(mflg.EQ.0)) GOTO 5401
             mndx=259
             to=9205
             CALL putil2
             IF(.NOT.(mflg.GT.0)) GOTO 5411
              mndx=263
              to=9205
              CALL putil2
              mndx=265
              to=9205
              CALL putil2
              IF(.NOT.(mflg.GT.0)) GOTO 5421
               mndx=255
               to=9206
               CALL putil2
               GOTO 5420
 5421         CONTINUE
 5420         CONTINUE
              GOTO 5410
 5411        CONTINUE
              mndx=261
              to=9205
              CALL putil2
              IF(.NOT.(mflg.GT.0)) GOTO 5431
               mndx=263
               to=9205
               CALL putil2
               mndx=265
               to=9205
               CALL putil2
               IF(.NOT.(mflg.GT.0)) GOTO 5441
                mndx=251
                to=9206
                CALL putil2
                GOTO 5440
 5441          CONTINUE
 5440          CONTINUE
               GOTO 5430
 5431         CONTINUE
 5430         CONTINUE
 5410        CONTINUE
             GOTO 5400
 5401       CONTINUE
 5400       CONTINUE
            GOTO 5390
 5391      CONTINUE
 5390      CONTINUE
           GOTO 5380
 5381     CONTINUE
 5380     CONTINUE
          GOTO 5370
 5371    CONTINUE
 5370    CONTINUE
         GOTO 5360
 5361   CONTINUE
 5360   CONTINUE
        GOTO 5350
 5351  CONTINUE
 5350  CONTINUE
       GOTO 5340
 5341 CONTINUE
 5340 CONTINUE
      dptr=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9006 CONTINUE
      iptr=iptr+3
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=16
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=46
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+17)
                 isto(ishenv+17)=isto(ishenv+12)
      to=9120
      CALL putil1
      isto(ishenv+17)=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5451
       isto(ishenv+54)=43
       CONTINUE
       CALL synstp
       GOTO 5450
 5451 CONTINUE
 5450 CONTINUE
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=47
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9007 CONTINUE
      iptr=iptr+3
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=17
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=48
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5461
       isto(ishenv+54)=44
       CONTINUE
       CALL synstp
       GOTO 5460
 5461 CONTINUE
 5460 CONTINUE
      isto(env+2)=iptr+1
      ndx=18
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=49
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+17)
                 isto(ishenv+17)=isto(ishenv+12)
      to=9120
      CALL putil1
      isto(ishenv+17)=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9008 CONTINUE
      iptr=iptr+3
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=19
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=50
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+17)
                 isto(ishenv+17)=isto(ishenv+12)
      to=9120
      CALL putil1
      isto(ishenv+17)=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5471
       isto(ishenv+54)=45
       CONTINUE
       CALL synstp
       GOTO 5470
 5471 CONTINUE
 5470 CONTINUE
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=51
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9009 CONTINUE
      iptr=iptr+6
      ipn=isto(ishenv+40)
      isto(ipn)=env
                  isto(ishenv+40)=env
      isto(env+0)=0
      sptr=sptr-1
      isto(sptr)=ptr1
                 ptr1=dptr
      mndx=411
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5481
       mndx=83
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5491
        isto(ishenv+54)=46
        CONTINUE
        CALL synstp
        GOTO 5490
 5491  CONTINUE
 5490  CONTINUE
       GOTO 5480
 5481 CONTINUE
       dptr=ptr1
       ptr1=0
 5480 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      isto(env+4)=iptr+1
      isto(ishenv+50)=18
      to=9100
      CALL putil1
      isto(env+3)=ndx1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ptr1.GT.0)) GOTO 5611
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5501
        isto(ishenv+54)=46
        CONTINUE
        CALL synstp
        GOTO 5500
 5501  CONTINUE
 5500  CONTINUE
       GOTO 5610
 5611 CONTINUE
 5610 CONTINUE
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5511
       isto(ishenv+54)=46
       CONTINUE
       CALL synstp
       GOTO 5510
 5511 CONTINUE
 5510 CONTINUE
      isto(env+5)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=52
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9010 CONTINUE
      iptr=iptr+3
      isto(env+2)=iptr+1
      isto(ishenv+50)=53
      to=9127
      CALL putil1
      IF(.NOT.(mflg.GT.0)) GOTO 5521
       isto(env+1)=1
       ptr3=isto(env+2)
       GOTO 5520
 5521 CONTINUE
       isto(ishenv+50)=54
       to=9128
       CALL putil1
       IF(.NOT.(mflg.GT.0)) GOTO 5531
        isto(env+1)=2
        ptr4=isto(env+2)
        GOTO 5530
 5531  CONTINUE
        isto(ishenv+54)=19
        CONTINUE
        CALL synstp
 5530  CONTINUE
 5520 CONTINUE
      GOTO 9500
 9011 CONTINUE
      iptr=iptr+3
      mndx=191
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5541
       isto(ishenv+54)=20
       CONTINUE
       CALL synstp
       GOTO 5540
 5541 CONTINUE
 5540 CONTINUE
      mndx=83
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5551
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9512
       GOTO 9012
 9512  from=isto(sptr)
       sptr=sptr+1
       mndx=105
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5561
        isto(ishenv+54)=20
        CONTINUE
        CALL synstp
        GOTO 5560
 5561  CONTINUE
 5560  CONTINUE
       isto(env+2)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=55
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5550
 5551 CONTINUE
       isto(env+2)=iptr+1
       isto(ishenv+50)=56
       to=9126
       CALL putil1
       IF(.NOT.(mflg.EQ.0)) GOTO 5571
        isto(ishenv+54)=20
        CONTINUE
        CALL synstp
        GOTO 5570
 5571  CONTINUE
 5570  CONTINUE
       isto(env+1)=0
 5550 CONTINUE
      GOTO 9500
 9012 CONTINUE
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3061 IF(.NOT.(lpflg.GT.0)) GOTO 3060
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9513
       GOTO 9013
 9513  from=isto(sptr)
       sptr=sptr+1
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5581
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5580
 5581  CONTINUE
        isto(env+0)=0
        lpflg=0
 5580  CONTINUE
       GOTO 3061
 3060 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9013 CONTINUE
      iptr=iptr+2
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=20
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=57
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+17)
                 isto(ishenv+17)=isto(ishenv+12)
      to=9120
      CALL putil1
      isto(ishenv+17)=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
