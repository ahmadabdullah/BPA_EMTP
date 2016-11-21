      SUBROUTINE putil1
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9100,
     1 9101, 9102, 9103, 9104, 9105,
     1 9106, 9107, 9108, 9109, 9110,
     1 9111, 9112, 9113, 9114, 9115,
     1 9116, 9117, 9118, 9119, 9120,
     1 9121, 9122, 9123, 9124, 9125,
     1 9126, 9127, 9128, 9129, 9130), to-9099
      WRITE(UNIT06,99)
   99 FORMAT(' Invalid "to" reference in putil1.')
      STOP
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
     1  9551,  9552,  9553), from-9500
      WRITE(UNIT06,98)
   98 FORMAT(' Invalid "from" reference in putil1.')
      STOP
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9100 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
      mndx=247
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5001
       mndx=83
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5011
        isto(ishenv+54)=18
        CONTINUE
        CALL synstp
        GOTO 5010
 5011  CONTINUE
 5010  CONTINUE
       flg=1
       ndx1=1
       GOTO 5000
 5001 CONTINUE
       ndx1=0
       flg=0
 5000 CONTINUE
      IF(.NOT.(ndx.EQ.4)) GOTO 5021
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                 env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx2
                 ndx2=397
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9501
       GOTO 9112
 9501  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(ndx1.GT.5)) GOTO 5031
        isto(ishenv+54)=31
        CONTINUE
        CALL synstp
        GOTO 5030
 5031  CONTINUE
 5030  CONTINUE
       ndx2=isto(sptr)
       sptr=sptr+1
       ndx1=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5020
 5021 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=isto(ishenv+17)
       IF(.NOT.(ndx.EQ.3)) GOTO 5041
        isto(ishenv+17)=isto(ishenv+12)
        GOTO 5040
 5041  CONTINUE
 5040  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9502
       GOTO 9120
 9502  from=isto(sptr)
       sptr=sptr+1
       isto(ishenv+17)=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
 5020 CONTINUE
      IF(.NOT.(flg.GT.0)) GOTO 5051
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5061
        isto(ishenv+54)=18
        CONTINUE
        CALL synstp
        GOTO 5060
 5061  CONTINUE
 5060  CONTINUE
       GOTO 5050
 5051 CONTINUE
 5050 CONTINUE
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9101 CONTINUE
      iptr=iptr+4
      isto(env+1)=isto(ishenv+50)
      isto(env+2)=isto(ishenv+51)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr12
                 ptr12=isto(ishenv+56)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr13
                 ptr13=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr14
                 ptr14=rptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr15
                 ptr15=isto(ishenv+45)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr16
                 ptr16=isto(ishenv+44)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr17
                 ptr17=isto(ishenv+23)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr18
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr19
                 ptr19=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3001 IF(.NOT.(lpflg.GT.0)) GOTO 3000
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9503
       GOTO 9102
 9503  from=isto(sptr)
       sptr=sptr+1
       mndx=269
       to=9207
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5071
        mndx=269
        to=9205
        CALL putil2
        ndx0=2
        ptr19=0
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5070
 5071  CONTINUE
        isto(env+0)=0
        lpflg=0
 5070  CONTINUE
       GOTO 3001
 3000 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      isto(env+0)=ndx0
      IF(.NOT.(ptr19.GT.0)) GOTO 6041
       iptr=env+3
       isto(env+3)=iptr+1
       rptr=ptr14
       isto(ishenv+56)=ptr12
       isto(ishenv+45)=ptr15
       isto(ishenv+44)=ptr16
       isto(ishenv+23)=ptr17
       dptr=ptr18
       IF(.NOT.(isto(ishenv).GT.0)) GOTO 6051
        i=0
 3141   IF(.NOT.(i.LT.ptr13)) GOTO 3140
         i=i+1
         BACKSPACE 12
         GOTO 3141
 3140   CONTINUE
        GOTO 6050
 6051  CONTINUE
 6050  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9553
       GOTO 9107
 9553  from=isto(sptr)
       sptr=sptr+1
       mndx=117
       to=9208
       CALL putil2
       env=isto(sptr)
       sptr=sptr+1
       GOTO 6040
 6041 CONTINUE
       isto(env+3)=0
 6040 CONTINUE
      ptr19=isto(sptr)
      sptr=sptr+1
      ptr18=isto(sptr)
      sptr=sptr+1
      ptr17=isto(sptr)
      sptr=sptr+1
      ptr16=isto(sptr)
      sptr=sptr+1
      ptr15=isto(sptr)
      sptr=sptr+1
      ptr14=isto(sptr)
      sptr=sptr+1
      k=ptr13
      ptr13=isto(sptr)
      sptr=sptr+1
      ptr13=ptr13+k
      ptr12=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9102 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3011 IF(.NOT.(lpflg.GT.0)) GOTO 3010
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9504
       GOTO 9103
 9504  from=isto(sptr)
       sptr=sptr+1
       mndx=271
       to=9207
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5081
        mndx=271
        to=9205
        CALL putil2
        ptr19=0
        ndx0=2
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5080
 5081  CONTINUE
        isto(env+0)=0
        lpflg=0
 5080  CONTINUE
       GOTO 3011
 3010 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9103 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=0
      ptr13=ptr13+1
      iptr=iptr+3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9505
      GOTO 9104
 9505 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      mndx=107
      to=9208
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5091
       ndx=1
       GOTO 5090
 5091 CONTINUE
       mndx=111
       to=9208
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5101
        mndx=109
        to=9208
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5111
         ndx=2
         GOTO 5110
 5111   CONTINUE
         mndx=107
         to=9208
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5121
          ndx=4
          GOTO 5120
 5121    CONTINUE
          ndx=3
 5120    CONTINUE
 5110   CONTINUE
        GOTO 5100
 5101  CONTINUE
        mndx=109
        to=9208
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5131
         mndx=107
         to=9208
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5141
          ndx=6
          GOTO 5140
 5141    CONTINUE
          ndx=5
 5140    CONTINUE
         GOTO 5130
 5131   CONTINUE
 5130   CONTINUE
 5100  CONTINUE
 5090 CONTINUE
      isto(env+2)=ndx
      IF(.NOT.(ndx.GT.0)) GOTO 5151
       to=9204
       CALL putil2
       ptr19=0
       ndx0=2
       isto(env+1)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9506
       GOTO 9104
 9506  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5150
 5151 CONTINUE
       isto(env+1)=0
 5150 CONTINUE
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9104 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+2
      mndx=91
      to=9205
      CALL putil2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=1
 3021 IF(.NOT.(ndx.GT.0)) GOTO 3020
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9507
       GOTO 9105
 9507  from=isto(sptr)
       sptr=sptr+1
       mndx=91
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5161
        ndx=1
        GOTO 5160
 5161  CONTINUE
        mndx=93
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5171
         ndx=2
         GOTO 5170
 5171   CONTINUE
         ndx=0
 5170   CONTINUE
 5160  CONTINUE
       IF(.NOT.(ndx.GT.0)) GOTO 5181
        ptr19=0
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5180
 5181  CONTINUE
        isto(env+0)=0
 5180  CONTINUE
       GOTO 3021
 3020 CONTINUE
      ndx=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
                 ptr3=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=0
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5191
       ptr19=0
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9508
       GOTO 9123
 9508  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(isto(ishenv+55).GT.0
     1 .AND. isto(ishenv+50).EQ.27
     2 )) GOTO 5201
        IF(.NOT.(isto(ishenv+57).GT.0)) GOTO 5211
         isto(ishenv+54)=72
         CONTINUE
         CALL synstp
         GOTO 5210
 5211   CONTINUE
 5210   CONTINUE
        isto(ishenv+57)=isto(ishenv+58)
        GOTO 5200
 5201  CONTINUE
 5200  CONTINUE
       GOTO 5190
 5191 CONTINUE
       mndx=117
       to=9208
       CALL putil2
 5190 CONTINUE
      isto(env+0)=ptr3
      isto(env+1)=ptr4
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9105 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+2
      isto(env+1)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=1
 3031 IF(.NOT.(ndx.GT.0)) GOTO 3030
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9509
       GOTO 9106
 9509  from=isto(sptr)
       sptr=sptr+1
       mndx=95
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5221
        ndx=1
        GOTO 5220
 5221  CONTINUE
        mndx=97
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5231
         ndx=2
         GOTO 5230
 5231   CONTINUE
         ndx=0
 5230   CONTINUE
 5220  CONTINUE
       IF(.NOT.(ndx.GT.0)) GOTO 5241
        ptr19=0
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5240
 5241  CONTINUE
        isto(env+0)=0
 5240  CONTINUE
       GOTO 3031
 3030 CONTINUE
      ndx=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9106 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+4
      isto(env+1)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9510
      GOTO 9107
 9510 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      mndx=99
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5251
       isto(env+2)=2
       GOTO 5250
 5251 CONTINUE
       mndx=275
       to=9207
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5261
        mndx=275
        to=9205
        CALL putil2
        isto(env+2)=3
        GOTO 5260
 5261  CONTINUE
 5260  CONTINUE
 5250 CONTINUE
      IF(.NOT.(mflg.GT.0)) GOTO 5271
       ptr19=0
       isto(env+3)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9511
       GOTO 9107
 9511  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5270
 5271 CONTINUE
       isto(env+3)=0
       isto(env+2)=1
 5270 CONTINUE
      GOTO 9500
 9107 CONTINUE
      ptr13=ptr13+1
      ptr18=dptr
      iptr=iptr+2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg1
                 flg1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=dptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      mndx=79
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5281
       ndx1=1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9512
       GOTO 9108
 9512  from=isto(sptr)
       sptr=sptr+1
       GOTO 5280
 5281 CONTINUE
       to=9211
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5291
        dptr=dptr-1
        ndx1=2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9513
        GOTO 9110
 9513   from=isto(sptr)
        sptr=sptr+1
        GOTO 5290
 5291  CONTINUE
        mndx=91
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5301
         to=9211
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5311
          dptr=dptr-1
          ndx1=2
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9514
          GOTO 9110
 9514     from=isto(sptr)
          sptr=sptr+1
          GOTO 5310
 5311    CONTINUE
          isto(ishenv+54)=47
          CONTINUE
          CALL synstp
 5310    CONTINUE
         GOTO 5300
 5301   CONTINUE
         mndx=93
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5321
          to=9211
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5331
           dptr=dptr-1
           flg1=1
           ndx1=2
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=from
           from=9515
           GOTO 9110
 9515      from=isto(sptr)
           sptr=sptr+1
           GOTO 5330
 5331     CONTINUE
           ndx=2
           mndx=83
           to=9205
           CALL putil2
           IF(.NOT.(mflg.GT.0)) GOTO 5341
            ndx1=3
            sptr=sptr-1
            IF(sptr.EQ.iptr) GOTO 910
            isto(sptr)=from
            from=9516
            GOTO 9111
 9516       from=isto(sptr)
            sptr=sptr+1
            GOTO 5340
 5341      CONTINUE
            sptr=sptr-1
            IF(sptr.EQ.iptr) GOTO 910
            isto(sptr)=ndx2
                       ndx2=0
            sptr=sptr-1
            IF(sptr.EQ.iptr) GOTO 910
            isto(sptr)=from
            from=9517
            GOTO 9112
 9517       from=isto(sptr)
            sptr=sptr+1
            ndx2=isto(sptr)
            sptr=sptr+1
 5340      CONTINUE
 5330     CONTINUE
          GOTO 5320
 5321    CONTINUE
          mndx=83
          to=9205
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5351
           ndx1=3
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=from
           from=9518
           GOTO 9111
 9518      from=isto(sptr)
           sptr=sptr+1
           GOTO 5350
 5351     CONTINUE
           mndx=273
           to=9207
           CALL putil2
           IF(.NOT.(mflg.GT.0)) GOTO 5361
            mndx=273
            to=9205
            CALL putil2
            mndx=83
            to=9205
            CALL putil2
            IF(.NOT.(mflg.GT.0)) GOTO 5371
             ndx=1
             ndx1=3
             sptr=sptr-1
             IF(sptr.EQ.iptr) GOTO 910
             isto(sptr)=from
             from=9519
             GOTO 9111
 9519        from=isto(sptr)
             sptr=sptr+1
             GOTO 5370
 5371       CONTINUE
 5370       CONTINUE
            GOTO 5360
 5361      CONTINUE
 5360      CONTINUE
           IF(.NOT.(ndx1.EQ.0)) GOTO 5381
            dptr=ptr1
            mndx=273
            to=9206
            CALL putil2
            IF(.NOT.(mflg.GT.0)) GOTO 5391
             ndx=1
             GOTO 5390
 5391       CONTINUE
 5390       CONTINUE
            sptr=sptr-1
            IF(sptr.EQ.iptr) GOTO 910
            isto(sptr)=ndx2
                       ndx2=0
            sptr=sptr-1
            IF(sptr.EQ.iptr) GOTO 910
            isto(sptr)=from
            from=9520
            GOTO 9112
 9520       from=isto(sptr)
            sptr=sptr+1
            ndx2=isto(sptr)
            sptr=sptr+1
            GOTO 5380
 5381      CONTINUE
 5380      CONTINUE
 5350     CONTINUE
 5320    CONTINUE
 5300   CONTINUE
 5290  CONTINUE
 5280 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+0)=ndx1
      isto(env+1)=ndx
      ptr1=isto(sptr)
      sptr=sptr+1
      flg1=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9108 CONTINUE
      ptr13=ptr13+1
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
 3041 IF(.NOT.(lpflg.GT.0)) GOTO 3040
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9521
       GOTO 9109
 9521  from=isto(sptr)
       sptr=sptr+1
       mndx=81
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5401
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5400
 5401  CONTINUE
        isto(env+0)=0
        lpflg=0
 5400  CONTINUE
       GOTO 3041
 3040 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9109 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+2
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+51)
                 isto(ishenv+51)=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9522
      GOTO 9101
 9522 from=isto(sptr)
      sptr=sptr+1
      isto(ishenv+51)=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9110 CONTINUE
      iptr=iptr+3
      isto(env+1)=dptr+1
      to=9214
      CALL putil2
      isto(env+2)=rptr
      isto(env+0)=dptr -isto(env+1) +1
      ptr13=ptr13+1
      GOTO 9500
 9111 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+1
      isto(env+0)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+51)
                 isto(ishenv+51)=2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9523
      GOTO 9101
 9523 from=isto(sptr)
      sptr=sptr+1
      isto(ishenv+51)=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5501
       isto(ishenv+54)=48
       CONTINUE
       CALL synstp
       GOTO 5500
 5501 CONTINUE
 5500 CONTINUE
      GOTO 9500
 9112 CONTINUE
      iptr=iptr+16
      ipn=isto(ishenv+23)
      isto(ipn)=env
                 isto(ishenv+23)=env
      isto(env+0)=0
      isto(env+4)=isto(ishenv+50)
      isto(env+5)=datenv
      isto(env+6)=isto(ishenv+12)
      isto(env+11)=ptr6
      isto(env+12)=ptr10
      isto(env+14)=ndx2
      isto(env+8)=dptr+1
      to=9200
      CALL putil2
      isto(env+7)=dptr -isto(env+8) +1
      ptr13=ptr13+1
      to=9204
      CALL putil2
      isto(env+15)=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                 ptr2=0
      mndx=79
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5531
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9524
       GOTO 9121
 9524  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(ptr2.GT.0)) GOTO 5541
        ndx1=6
        GOTO 5540
 5541  CONTINUE
        ndx1=5
 5540  CONTINUE
       GOTO 5530
 5531 CONTINUE
       mndx=83
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5551
        ndx1=7
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ndx3
                   ndx3=isto(env+7)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ndx4
                   ndx4=isto(env+8)
        isto(env+15)=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9525
        GOTO 9113
 9525   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        ndx4=isto(sptr)
        sptr=sptr+1
        ndx3=isto(sptr)
        sptr=sptr+1
        GOTO 5550
 5551  CONTINUE
        ndx1=4
 5550  CONTINUE
 5530 CONTINUE
      isto(env+9)=ptr1
      isto(env+10)=ptr2
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(env+13)=ndx1
      GOTO 9500
 9113 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9526
      GOTO 9114
 9526 from=isto(sptr)
      sptr=sptr+1
      isto(env+1)=iptr+1
      IF(.NOT.((ndx.GE.379 .AND. ndx.LE.395)
     1.OR. (ndx.GE.411 .AND. ndx.LE.413)
     2)) GOTO 5581
       isto(env+0)=1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9527
       GOTO 9116
 9527  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5580
 5581 CONTINUE
       isto(env+0)=2
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9528
       GOTO 9117
 9528  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
 5580 CONTINUE
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9114 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=1
      ndx5=isto(env+1) -2 +base1
      ndx6=isto(env+0)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9529
      GOTO 9115
 9529 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx.EQ.0)) GOTO 5591
       ndx5=isto(env+3) -2 +base1
       ndx6=isto(env+2)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9530
       GOTO 9115
 9530  from=isto(sptr)
       sptr=sptr+1
       GOTO 5590
 5591 CONTINUE
 5590 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9115 CONTINUE
      i0=0
      k=0
 3081 IF(.NOT.(i0.LT.ndx6 .AND. k.EQ.0)) GOTO 3080
       i0=i0+1
       ndx5=ndx5+2
       i1=isto(ndx5+1)
       IF(.NOT.(i1.EQ.ndx3)) GOTO 5601
        i=0
        i2=base2 +isto(ndx5)
 3091   IF(.NOT.(i.LT.ndx3
     1  .AND. csto(i2+i).EQ.csto(ndx4+i)
     2  )) GOTO 3090
         i=i+1
         GOTO 3091
 3090   CONTINUE
        IF(.NOT.(i.EQ.ndx3)) GOTO 5611
         k=1
         GOTO 5610
 5611   CONTINUE
 5610   CONTINUE
        GOTO 5600
 5601  CONTINUE
 5600  CONTINUE
       GOTO 3081
 3080 CONTINUE
      IF(.NOT.(k.GT.0)) GOTO 5621
       ndx=ndx5-base1
       GOTO 5620
 5621 CONTINUE
       ndx=0
 5620 CONTINUE
      GOTO 9500
 9116 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+9
      isto(env+1)=ndx
      isto(env+3)=0
      isto(env+8)=0
      IF(.NOT.(isto(ishenv+55).GT.0
     1.AND. isto(ishenv+50).EQ.27
     2)) GOTO 5631
       isto(ishenv+56)=isto(ishenv+56)+1
       GOTO 5630
 5631 CONTINUE
 5630 CONTINUE
      ipn=isto(ishenv+45)
      isto(ipn)=env
                 isto(ishenv+45)=env
      isto(env+0)=0
      isto(env+6)=0
      isto(env+7)=0
      isto(env+4)=0
      isto(env+2)=iptr+1
      IF(.NOT.(ndx.EQ.411)) GOTO 5641
       IF(.NOT.(isto(ishenv+50).NE.15
     1 .AND. isto(ishenv+50).NE.25
     2 .AND. isto(ishenv+50).NE.52
     3 )) GOTO 5651
        isto(ishenv+54)=64
        CONTINUE
        CALL synstp
        GOTO 5650
 5651  CONTINUE
        k=1
 5650  CONTINUE
       GOTO 5640
 5641 IF(.NOT.(ndx.EQ.413)) GOTO 5642
       k=1
       GOTO 5640
 5642 IF(.NOT.(isto(ishenv+52).GT.0)) GOTO 5643
       isto(ishenv+54)=90
       CONTINUE
       CALL synstp
       GOTO 5640
 5643 CONTINUE
       k=0
 5640 CONTINUE
      IF(.NOT.(k.GT.0)) GOTO 5661
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx
                  ndx=4
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9531
       GOTO 9100
 9531  from=isto(sptr)
       sptr=sptr+1
       isto(env+4)=ndx1
       ndx1=isto(sptr)
       sptr=sptr+1
       ndx=isto(sptr)
       sptr=sptr+1
       GOTO 5660
 5661 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx2
                  ndx2=ndx
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9532
       GOTO 9112
 9532  from=isto(sptr)
       sptr=sptr+1
       ndx2=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(ndx1.GT.5)) GOTO 5671
        isto(ishenv+54)=31
        CONTINUE
        CALL synstp
        GOTO 5670
 5671  CONTINUE
 5670  CONTINUE
       ndx1=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
 5660 CONTINUE
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5681
       isto(env+6)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=isto(ishenv+51)
                  isto(ishenv+51)=3
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9533
       GOTO 9101
 9533  from=isto(sptr)
       sptr=sptr+1
       isto(ishenv+51)=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5691
        isto(env+7)=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=isto(ishenv+51)
                   isto(ishenv+51)=4
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9534
        GOTO 9101
 9534   from=isto(sptr)
        sptr=sptr+1
        isto(ishenv+51)=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        mndx=85
        to=9205
        CALL putil2
        IF(.NOT.(mflg.EQ.0)) GOTO 5701
         isto(ishenv+54)=51
         CONTINUE
         CALL synstp
         GOTO 5700
 5701   CONTINUE
 5700   CONTINUE
        GOTO 5690
 5691  CONTINUE
 5690  CONTINUE
       GOTO 5680
 5681 CONTINUE
 5680 CONTINUE
      IF(.NOT.(isto(env+6).EQ.0
     1.AND. ((ndx.GE.379 .AND. ndx.LE.391
     2       .AND. ndx.NE.387)
     3     .OR. ndx.EQ.413)
     4)) GOTO 5711
       isto(ishenv+54)=49
       CONTINUE
       CALL synstp
       GOTO 5710
 5711 IF(.NOT.(isto(env+6).GT.0
     1.AND. (ndx.EQ.387 .OR. ndx.EQ.411)
     2)) GOTO 5712
       isto(ishenv+54)=73
       CONTINUE
       CALL synstp
       GOTO 5710
 5712 IF(.NOT.(isto(env+7).GT.0
     1.AND. ndx.EQ.413
     2)) GOTO 5713
       isto(ishenv+54)=76
       CONTINUE
       CALL synstp
       GOTO 5710
 5713 CONTINUE
 5710 CONTINUE
      GOTO 9500
 9117 CONTINUE
      iptr=iptr+8
      ipn=isto(ishenv+44)
      isto(ipn)=env
                 isto(ishenv+44)=env
      isto(env+0)=0
      isto(env+1)=ndx
      isto(env+2)=ndx3
      isto(env+3)=ndx4
      ptr13=ptr13+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=0
      IF(.NOT.(ndx.EQ.325
     1)) GOTO 5721
       isto(env+7)=0
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5731
        isto(ishenv+54)=52
        CONTINUE
        CALL synstp
        GOTO 5730
 5731  CONTINUE
 5730  CONTINUE
       GOTO 5720
 5721 CONTINUE
       isto(env+7)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=lpflg
                  lpflg=1
 3101  IF(.NOT.(lpflg.GT.0)) GOTO 3100
        ndx1=ndx1+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9535
        GOTO 9118
 9535   from=isto(sptr)
        sptr=sptr+1
        mndx=85
        to=9205
        CALL putil2
        IF(.NOT.(mflg.EQ.0)) GOTO 5741
         isto(env+0)=iptr+1
         env=iptr+1
         GOTO 5740
 5741   CONTINUE
         isto(env+0)=0
         lpflg=0
 5740   CONTINUE
        GOTO 3101
 3100  CONTINUE
       lpflg=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
 5720 CONTINUE
      isto(env+6)=ndx1
      ndx1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9118 CONTINUE
      ptr13=ptr13+1
      iptr=iptr+2
      mndx=279
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5751
       isto(env+1)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=isto(ishenv+51)
                  isto(ishenv+51)=5
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9536
       GOTO 9101
 9536  from=isto(sptr)
       sptr=sptr+1
       isto(ishenv+51)=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5750
 5751 CONTINUE
       isto(env+1)=0
 5750 CONTINUE
      GOTO 9500
 9119 CONTINUE
      iptr=iptr+4
      isto(env+1)=dptr+1
      to=9200
      CALL putil2
      isto(env+0)=dptr -isto(env+1) +1
      to=9204
      CALL putil2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                  ptr1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                  ptr2=0
      mndx=79
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5761
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9537
       GOTO 9121
 9537  from=isto(sptr)
       sptr=sptr+1
       GOTO 5760
 5761 CONTINUE
 5760 CONTINUE
      isto(env+2)=ptr1
      isto(env+3)=ptr2
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9120 CONTINUE
      iptr=iptr+10
      ipn=isto(ishenv+22)
      isto(ipn)=env
                 isto(ishenv+22)=env
      isto(env+0)=0
      isto(env+1)=ndx
      isto(env+5)=isto(ishenv+17)
      isto(env+7)=dptr+1
      to=9200
      CALL putil2
      isto(env+6)=dptr -isto(env+7) +1
      to=9204
      CALL putil2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                 ptr2=0
      mndx=79
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5771
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9538
       GOTO 9121
 9538  from=isto(sptr)
       sptr=sptr+1
       GOTO 5770
 5771 CONTINUE
 5770 CONTINUE
      IF(.NOT.(ndx.GE.9 .AND. ndx.LE.15
     1.AND. ptr2.GT.0)) GOTO 5781
       isto(ishenv+54)=66
       CONTINUE
       CALL synstp
       GOTO 5780
 5781 CONTINUE
 5780 CONTINUE
      isto(env+8)=ptr1
      isto(env+9)=ptr2
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9121 CONTINUE
      ptr1=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+51)
                 isto(ishenv+51)=6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9539
      GOTO 9101
 9539 from=isto(sptr)
      sptr=sptr+1
      isto(ishenv+51)=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      mndx=115
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5791
       dptr=dptr-1
       mndx=115
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5801
        dptr=dptr+1
        GOTO 5800
 5801  CONTINUE
 5800  CONTINUE
       GOTO 5790
 5791 CONTINUE
 5790 CONTINUE
      IF(.NOT.(mflg.GT.0)) GOTO 5811
       ptr2=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=isto(ishenv+51)
                  isto(ishenv+51)=7
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9540
       GOTO 9101
 9540  from=isto(sptr)
       sptr=sptr+1
       isto(ishenv+51)=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5810
 5811 CONTINUE
 5810 CONTINUE
      mndx=81
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5821
       isto(ishenv+54)=54
       CONTINUE
       CALL synstp
       GOTO 5820
 5821 CONTINUE
 5820 CONTINUE
      GOTO 9500
 9122 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3111 IF(.NOT.(lpflg.GT.0)) GOTO 3110
       ptr1=iptr+1
       isto(ishenv+50)=59
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9541
       GOTO 9127
 9541  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.GT.0)) GOTO 5831
        ptr3=ptr1
        GOTO 5830
 5831  CONTINUE
        isto(ishenv+50)=60
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9542
        GOTO 9128
 9542   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.GT.0)) GOTO 5841
         ptr4=ptr1
         GOTO 5840
 5841   CONTINUE
         isto(ishenv+54)=55
         CONTINUE
         CALL synstp
 5840   CONTINUE
 5830  CONTINUE
       mndx=89
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5851
        lpflg=0
        GOTO 5850
 5851  CONTINUE
 5850  CONTINUE
       GOTO 3111
 3110 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9123 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3121 IF(.NOT.(lpflg.GT.0)) GOTO 3120
       ptr1=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=isto(ishenv+51)
                   isto(ishenv+51)=8
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9543
       GOTO 9127
 9543  from=isto(sptr)
       sptr=sptr+1
       isto(ishenv+51)=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.GT.0)) GOTO 5861
        ptr3=ptr1
        GOTO 5860
 5861  CONTINUE
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=isto(ishenv+51)
                     isto(ishenv+51)=9
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9544
        GOTO 9128
 9544   from=isto(sptr)
        sptr=sptr+1
        isto(ishenv+51)=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.GT.0)) GOTO 5871
         ptr4=ptr1
         GOTO 5870
 5871   CONTINUE
         isto(ishenv+54)=55
         CONTINUE
         CALL synstp
 5870   CONTINUE
 5860  CONTINUE
       mndx=89
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5881
        lpflg=0
        GOTO 5880
 5881  CONTINUE
 5880  CONTINUE
       GOTO 3121
 3120 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9124 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3131 IF(.NOT.(lpflg.GT.0)) GOTO 3130
       ptr1=iptr+1
       isto(ishenv+50)=61
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9545
       GOTO 9129
 9545  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.GT.0)) GOTO 5891
        ptr3=ptr1
        GOTO 5890
 5891  CONTINUE
        isto(ishenv+50)=62
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9546
        GOTO 9130
 9546   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.GT.0)) GOTO 5901
         ptr4=ptr1
         GOTO 5900
 5901   CONTINUE
         isto(ishenv+54)=56
         CONTINUE
         CALL synstp
 5900   CONTINUE
 5890  CONTINUE
       mndx=89
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5911
        lpflg=0
        GOTO 5910
 5911  CONTINUE
 5910  CONTINUE
       GOTO 3131
 3130 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9125 CONTINUE
      mndx=277
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5921
       mndx=105
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5931
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9547
        GOTO 9101
 9547   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5930
 5931  CONTINUE
        isto(ishenv+54)=57
        CONTINUE
        CALL synstp
 5930  CONTINUE
       mflg=1
       GOTO 5920
 5921 CONTINUE
 5920 CONTINUE
      GOTO 9500
 9126 CONTINUE
      mndx=279
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5941
       mndx=105
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5951
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9548
        GOTO 9101
 9548   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5950
 5951  CONTINUE
        isto(ishenv+54)=58
        CONTINUE
        CALL synstp
 5950  CONTINUE
       mflg=1
       GOTO 5940
 5941 CONTINUE
 5940 CONTINUE
      GOTO 9500
 9127 CONTINUE
      mndx=357
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5961
       mndx=105
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5971
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9549
        GOTO 9101
 9549   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5970
 5971  CONTINUE
        isto(ishenv+54)=59
        CONTINUE
        CALL synstp
 5970  CONTINUE
       mflg=1
       GOTO 5960
 5961 CONTINUE
 5960 CONTINUE
      GOTO 9500
 9128 CONTINUE
      mndx=359
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5981
       mndx=105
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5991
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9550
        GOTO 9101
 9550   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5990
 5991  CONTINUE
        isto(ishenv+54)=60
        CONTINUE
        CALL synstp
 5990  CONTINUE
       mflg=1
       GOTO 5980
 5981 CONTINUE
 5980 CONTINUE
      GOTO 9500
 9129 CONTINUE
      mndx=281
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 6001
       mndx=105
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 6011
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9551
        GOTO 9101
 9551   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 6010
 6011  CONTINUE
        isto(ishenv+54)=61
        CONTINUE
        CALL synstp
 6010  CONTINUE
       mflg=1
       GOTO 6000
 6001 CONTINUE
 6000 CONTINUE
      GOTO 9500
 9130 CONTINUE
      mndx=283
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 6021
       mndx=105
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 6031
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9552
        GOTO 9101
 9552   from=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 6030
 6031  CONTINUE
        isto(ishenv+54)=62
        CONTINUE
        CALL synstp
 6030  CONTINUE
       mflg=1
       GOTO 6020
 6021 CONTINUE
 6020 CONTINUE
      GOTO 9500
      END
