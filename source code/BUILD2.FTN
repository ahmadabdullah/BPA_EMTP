      SUBROUTINE build2
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO (
     1       9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010,
     1 9011, 9012, 9013, 9014, 9015,
     1 9016, 9017, 9018, 9019, 9020,
     1 9021, 9022, 9023, 9024, 9025,
     1 9026, 9027), to-9001
      STOP 'Invalid "to" reference in "build".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1              9523, 9524, 9525,
     1  9526, 9527, 9528, 9529, 9530,
     1  9531, 9532, 9533, 9534, 9535,
     1  9536), from-9522
 0002 STOP 'Invalid "from" reference in "build".'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9002 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=cptr
      isto(env+7)=ptr1+1
      IF(.NOT.(isto(ishenv+10).GT.0)) GOTO 5121
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(ishenv+10)
       i1=isto(env+6)
       i2=isto(env+7)-1
       env=isto(sptr)
       sptr=sptr+1
       i=0
 3131  IF(.NOT.(i.LT.i1)) GOTO 3130
        i=i+1
        csto(cptr+i)=csto(i2+i)
        GOTO 3131
 3130  CONTINUE
       cptr=cptr+i1+1
       mndx=113
       j=base1+mndx
       k=base2 +isto(j)
       csto(cptr)=csto(k)
       GOTO 5120
 5121 CONTINUE
 5120 CONTINUE
      i3=isto(env+3)
      i4=isto(env+4)-1
      i=0
 3141 IF(.NOT.(i.LT.i3)) GOTO 3140
       i=i+1
       csto(cptr+i)=csto(i4+i)
       GOTO 3141
 3140 CONTINUE
      cptr=cptr+i3
      i5=isto(env+5)
      IF(.NOT.(i5.LT.iinf)) GOTO 5131
       cptr=cptr+1
       mndx=79
       j=base1+mndx
       k=base2 +isto(j)
       csto(cptr)=csto(k)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx
                  ndx=i5
       to=9036
       CALL xpr2
       ndx=isto(sptr)
       sptr=sptr+1
       cptr=cptr+1
       mndx=81
       j=base1+mndx
       k=base2 +isto(j)
       csto(cptr)=csto(k)
       GOTO 5130
 5131 CONTINUE
 5130 CONTINUE
      isto(env+6)=cptr-ptr1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9003 CONTINUE
      i6=isto(base6+ndx5)
      IF(.NOT.(i6.EQ.-iinf)) GOTO 5141
       i4=ndx4
       i6=ndx4
       isto(base4+ndx5)=ndx4
       isto(base6+ndx5)=ndx4
       ndx7=1
       GOTO 5140
 5141 CONTINUE
       i4=isto(base4+ndx5)
       ndx7=i6-i4+1
 5140 CONTINUE
      i3=ndx6-i6
      IF(.NOT.(i3.LT.0)) GOTO 5681
       i3=0
       GOTO 5680
 5681 CONTINUE
 5680 CONTINUE
      i2=i4-ndx4
      IF(.NOT.(i2.LT.0)) GOTO 5691
       i2=0
       GOTO 5690
 5691 CONTINUE
 5690 CONTINUE
      i1=i2+i3
      cnt1=cnt1+i1
      cnt2=cnt2+i1
      rptr=rptr+i1
      IF(.NOT.(ndx4.LT.i4)) GOTO 5151
       i4=ndx4
       isto(base4+ndx5)=i4
       GOTO 5150
 5151 CONTINUE
 5150 CONTINUE
      IF(.NOT.(ndx6.GT.i6)) GOTO 5161
       i6=ndx6
       isto(base6+ndx5)=i6
       GOTO 5160
 5161 CONTINUE
 5160 CONTINUE
      ndx8=i6-i4+1
      IF(.NOT.(i1.GT.0 .AND. ndx5.LT.ndx0)) GOTO 5171
       i=cnt1+1
       k=isto(base5+ndx5+1)
 3151  IF(.NOT.(i.GT.k)) GOTO 3150
        i=i-1
        a=rsto(base3+i)
        rsto(base3+i+i1)=a
        GOTO 3151
 3150  CONTINUE
       i=ndx5
 3161  IF(.NOT.(i.LT.ndx0)) GOTO 3160
        i=i+1
        k=isto(base5+i)
        isto(base5+i)=k+i1
        GOTO 3161
 3160  CONTINUE
       GOTO 5170
 5171 CONTINUE
 5170 CONTINUE
      IF(.NOT.(i1.GT.0)) GOTO 5181
       i0=base3+isto(base5+ndx5)-1
       i=ndx7+1
 3171  IF(.NOT.(i.GT.1)) GOTO 3170
        i=i-1
        a=rsto(i0+i)
        rsto(i0+i+i2)=a
        GOTO 3171
 3170  CONTINUE
       GOTO 5180
 5181 CONTINUE
 5180 CONTINUE
      IF(.NOT.(i3.GT.0)) GOTO 5191
       i0=base3+isto(base5+ndx5)-i4+i6-i3
       i=0
 3181  IF(.NOT.(i.LT.i3)) GOTO 3180
        i=i+1
        rsto(i0+i)=rnull
        GOTO 3181
 3180  CONTINUE
       GOTO 5190
 5191 CONTINUE
 5190 CONTINUE
      IF(.NOT.(i2.GT.0)) GOTO 5201
       i0=base3+isto(base5+ndx5)-1
       i=0
 3191  IF(.NOT.(i.LT.i2)) GOTO 3190
        i=i+1
        rsto(i0+i)=rnull
        GOTO 3191
 3190  CONTINUE
       GOTO 5200
 5201 CONTINUE
 5200 CONTINUE
      GOTO 9500
 9004 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+6)
 3201 IF(.NOT.(env.GT.0)) GOTO 3200
       ndx5=isto(env+2)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       stpl1=isto(env+0)
       stpc1=isto(env+1)
       ndx1=isto(env+2)
       ndx2=isto(env+3)
       to=9020
       CALL xpr1
       env=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9523
       GOTO 9003
 9523  from=isto(sptr)
       sptr=sptr+1
       xprndx=isto(env+5)
       to=9000
       CALL xpr1
       to=9000
       CALL xpr2
       env=isto(env+0)
       GOTO 3201
 3200 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9005 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+8)
 3211 IF(.NOT.(env.GT.0)) GOTO 3210
       ndx5=isto(env+2)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       stpl1=isto(env+0)
       stpc1=isto(env+1)
       ndx1=isto(env+2)
       ndx2=isto(env+3)
       to=9020
       CALL xpr1
       env=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9524
       GOTO 9003
 9524  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3211
 3210 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9006 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+10)
 3221 IF(.NOT.(env.GT.0)) GOTO 3220
       ndx5=isto(env+2)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       stpl1=isto(env+0)
       stpc1=isto(env+1)
       ndx1=isto(env+2)
       ndx2=isto(env+3)
       to=9020
       CALL xpr1
       env=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9525
       GOTO 9003
 9525  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3221
 3220 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9007 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+12)
 3231 IF(.NOT.(env.GT.0)) GOTO 3230
       ndx5=isto(env+2)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       stpl1=isto(env+0)
       stpc1=isto(env+1)
       ndx1=isto(env+2)
       ndx2=isto(env+3)
       to=9020
       CALL xpr1
       env=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9526
       GOTO 9003
 9526  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3231
 3230 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9008 CONTINUE
      STOP 'No buildftndata routine.'
 9009 CONTINUE
      STOP 'No buildftninput routine.'
 9010 CONTINUE
      STOP 'No buildftnvar routine.'
 9011 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+10)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+11)
 3241 IF(.NOT.(env.GT.0)) GOTO 3240
       IF(.NOT.(isto(env+1).EQ.7)) GOTO 5211
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                  env=isto(env+2)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+0)
 3251   IF(.NOT.(env.GT.0)) GOTO 3250
         xprndx=isto(env+2)
         to=9000
         CALL xpr1
         ndx3=xprcnt
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=env
                    env=isto(env+1)
         ndx5=isto(env+2)
         ndx1=isto(env+8)
         ndx2=isto(env+9)
         env=isto(sptr)
         sptr=sptr+1
         to=9020
         CALL xpr1
         to=9037
         CALL xpr2
         xprcnt=ndx3
         to=9000
         CALL xpr2
         env=isto(env+0)
         GOTO 3251
 3250   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5210
 5211  CONTINUE
 5210  CONTINUE
       env=isto(env+0)
       GOTO 3241
 3240 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9012 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+8)
 3261 IF(.NOT.(env.GT.0)) GOTO 3260
       ndx5=isto(env+2)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       stpl1=isto(env+0)
       stpc1=isto(env+1)
       ndx1=isto(env+2)
       ndx2=isto(env+3)
       to=9020
       CALL xpr1
       to=9037
       CALL xpr2
       env=isto(sptr)
       sptr=sptr+1
       xprndx=isto(env+5)
       IF(.NOT.(xprndx.GT.0)) GOTO 5221
        to=9000
        CALL xpr1
        to=9001
        CALL xpr2
        GOTO 5220
 5221  CONTINUE
 5220  CONTINUE
       env=isto(env+0)
       GOTO 3261
 3260 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9013 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+48)
 3271 IF(.NOT.(env.GT.0)) GOTO 3270
       IF(.NOT.(isto(env+1).EQ.12)) GOTO 5231
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+2)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+0)
 3281   IF(.NOT.(env.GT.0)) GOTO 3280
         xprndx=isto(env+2)
         to=9000
         CALL xpr1
         a=rsto(rptr)
         rptr=rptr-1
         cnt4=idnint(a)
         IF(.NOT.(cnt4.LT.0 .OR. cnt4.GT.4
     1   )) GOTO 5241
          stpflg=113
          stpi1=cnt4
          CONTINUE
          CALL errstp
          GOTO 5240
 5241    CONTINUE
 5240    CONTINUE
         k=isto(env+0)
         IF(.NOT.(k.GT.0)) GOTO 5251
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=env
                     env=k
 3291     IF(.NOT.(env.GT.0)) GOTO 3290
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=env
                      env=isto(env+1)
           ndx5=isto(env+2)
           stpl1=isto(env+6)
           stpc1=isto(env+7)
           ndx1=isto(env+8)
           ndx2=isto(env+9)
           to=9020
           CALL xpr1
           to=9037
           CALL xpr2
           env=isto(sptr)
           sptr=sptr+1
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=from
           from=9527
           GOTO 9014
 9527      from=isto(sptr)
           sptr=sptr+1
           env=isto(env+0)
           GOTO 3291
 3290     CONTINUE
          env=isto(sptr)
          sptr=sptr+1
          GOTO 5250
 5251    CONTINUE
          cnt5=cnt4
 5250    CONTINUE
         env=isto(env+1)
         GOTO 3281
 3280   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5230
 5231  CONTINUE
 5230  CONTINUE
       env=isto(env+0)
       GOTO 3271
 3270 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9014 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      i1=isto(env+29)+4*(ndx5-cnt6)
      i2=isto(i1+3)
      i4=isto(base4+ndx5)
      i3=i2+5*(ndx4-i4-1)
      k=ndx6-ndx4+1
      i=0
 3301 IF(.NOT.(i.LT.k)) GOTO 3300
       i=i+1
       i3=i3+5
       IF(.NOT.(cnt4.GT.isto(i3+1))) GOTO 5261
        isto(i3+1)=cnt4
        GOTO 5260
 5261  CONTINUE
 5260  CONTINUE
       GOTO 3301
 3300 CONTINUE
      IF(.NOT.(cnt4.GT.cnt8)) GOTO 5271
       cnt8=cnt4
       GOTO 5270
 5271 CONTINUE
 5270 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9015 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+10)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+11)
 3311 IF(.NOT.(env.GT.0)) GOTO 3310
       IF(.NOT.(isto(env+1).EQ.10)) GOTO 5281
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+2)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+0)
 3321   IF(.NOT.(env.GT.0)) GOTO 3320
         xprndx=isto(env+2)
         to=9000
         CALL xpr1
         a=rsto(rptr)
         rptr=rptr-1
         cnt4=idnint(a)
         IF(.NOT.(cnt4.LT.0 .OR. cnt4.GT.4
     1   )) GOTO 5291
          stpflg=114
          stpi1=cnt4
          CONTINUE
          CALL errstp
          GOTO 5290
 5291    CONTINUE
 5290    CONTINUE
         k=isto(env+1)
         IF(.NOT.(k.GT.0)) GOTO 5301
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=env
                     env=k
 3331     IF(.NOT.(env.GT.0)) GOTO 3330
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=env
                      env=isto(env+1)
           ndx5=isto(env+2)
           stpl1=isto(env+6)
           stpc1=isto(env+7)
           ndx1=isto(env+8)
           ndx2=isto(env+9)
           to=9020
           CALL xpr1
           to=9037
           CALL xpr2
           env=isto(sptr)
           sptr=sptr+1
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=from
           from=9528
           GOTO 9014
 9528      from=isto(sptr)
           sptr=sptr+1
           env=isto(env+0)
           GOTO 3331
 3330     CONTINUE
          env=isto(sptr)
          sptr=sptr+1
          GOTO 5300
 5301    CONTINUE
          cnt5=cnt4
 5300    CONTINUE
         env=isto(env+0)
         GOTO 3321
 3320   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5280
 5281  CONTINUE
 5280  CONTINUE
       env=isto(env+0)
       GOTO 3311
 3310 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9016 CONTINUE
      i1=isto(env+32)-5
      k=isto(env+30)
      i=0
 3341 IF(.NOT.(i.LT.k)) GOTO 3340
       i=i+1
       i1=i1+5
       IF(.NOT.(isto(i1+1).LT.0)) GOTO 5311
        isto(i1+1)=cnt5
        IF(.NOT.(cnt5.GT.cnt8)) GOTO 5671
         cnt8=cnt5
         GOTO 5670
 5671   CONTINUE
 5670   CONTINUE
        GOTO 5310
 5311  CONTINUE
 5310  CONTINUE
       GOTO 3341
 3340 CONTINUE
      GOTO 9500
 9017 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+35)
 3351 IF(.NOT.(env.GT.0)) GOTO 3350
       k=isto(env+1)
       IF(.NOT.(k.EQ.379 .OR. k.EQ.385
     1 .OR. k.EQ.389 .OR. k.EQ.391
     2 )) GOTO 5321
        xprndx=isto(env+7)
        IF(.NOT.(xprndx.GT.0)) GOTO 5331
         to=9000
         CALL xpr1
         a=rsto(rptr)
         rptr=rptr-1
                 cnt4=idnint(a)
         cnt3=2*cnt4
         IF(.NOT.(cnt3.EQ.0)) GOTO 5341
          cnt3=1
          GOTO 5340
 5341    CONTINUE
 5340    CONTINUE
         IF(.NOT.(cnt4.LT.0 .OR. cnt4.GT.4
     1   )) GOTO 5351
          stpflg=112
          k=isto(env+1)
          stpl1=isto(base1+k+1)
          stpc1=base2+isto(base1+k)
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=env
                     env=isto(env+2)
          stpl2=isto(env+7)
          stpc2=isto(env+8)
          env=isto(sptr)
          sptr=sptr+1
          stpi1=cnt4
          CONTINUE
          CALL errstp
          GOTO 5350
 5351    CONTINUE
 5350    CONTINUE
         GOTO 5330
 5331   CONTINUE
         cnt4=1
         cnt3=2
 5330   CONTINUE
        GOTO 5320
 5321  IF(.NOT.(k.EQ.387 .OR. k.EQ.393
     1 )) GOTO 5322
        cnt3=1
        cnt4=-1
        GOTO 5320
 5322  IF(.NOT.(k.EQ.395)) GOTO 5323
        cnt3=2
        cnt4=-1
        GOTO 5320
 5323  CONTINUE
        cnt3=0
 5320  CONTINUE
       IF(.NOT.(cnt3.GT.0)) GOTO 5361
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+2)
        ndx5=isto(env+1)
        stpl1=isto(env+7)
        stpc1=isto(env+8)
        ndx1=isto(env+9)
        ndx2=ndx1
        to=9020
        CALL xpr1
        to=9037
        CALL xpr2
        env=isto(sptr)
        sptr=sptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9529
        GOTO 9018
 9529   from=isto(sptr)
        sptr=sptr+1
        GOTO 5360
 5361  CONTINUE
 5360  CONTINUE
       env=isto(env+0)
       GOTO 3351
 3350 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9018 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      i4=isto(base4+ndx5)
      IF(.NOT.(ndx5.GE.cnt7)) GOTO 5371
       i1=isto(env+36)+3*(ndx5-cnt7)
       i2=isto(i1)
       i3=i2+7*(ndx4-i4)
       GOTO 5370
 5371 CONTINUE
       i1=isto(env+29)+4*(ndx5-cnt6)
       i2=isto(i1)
       i3=i2+7*(ndx4-i4)
 5370 CONTINUE
      IF(.NOT.(cnt3.GT.isto(i3+5))) GOTO 5381
       isto(i3+5)=cnt3
       GOTO 5380
 5381 CONTINUE
 5380 CONTINUE
      IF(.NOT.(cnt4.GT.isto(i3+1))) GOTO 5391
       isto(i3+1)=cnt4
       GOTO 5390
 5391 CONTINUE
 5390 CONTINUE
      IF(.NOT.(cnt3.GT.cnt9)) GOTO 5401
       cnt9=cnt3
       GOTO 5400
 5401 CONTINUE
 5400 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9019 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+13)
 3361 IF(.NOT.(env.GT.0)) GOTO 3360
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       ndx5=isto(env+2)
       stpl1=isto(env+6)
       stpc1=isto(env+7)
       ndx1=isto(env+8)
       ndx2=ndx1
       to=9020
       CALL xpr1
       to=9037
       CALL xpr2
       env=isto(sptr)
       sptr=sptr+1
       cnt3=isto(env+1)
       cnt4=-1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9530
       GOTO 9018
 9530  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3361
 3360 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9020 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+14)
 3371 IF(.NOT.(env.GT.0)) GOTO 3370
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       ndx5=isto(env+2)
       stpl1=isto(env+6)
       stpc1=isto(env+7)
       ndx1=isto(env+8)
       ndx2=ndx1
       to=9020
       CALL xpr1
       to=9037
       CALL xpr2
       env=isto(sptr)
       sptr=sptr+1
       cnt3=isto(env+2)
       cnt4=-1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9531
       GOTO 9018
 9531  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3371
 3370 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9021 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+15)
 3381 IF(.NOT.(env.GT.0)) GOTO 3380
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+5)
       ndx5=isto(env+2)
       stpl1=isto(env+6)
       stpc1=isto(env+7)
       ndx1=isto(env+8)
       ndx2=ndx1
       to=9020
       CALL xpr1
       to=9037
       CALL xpr2
       env=isto(sptr)
       sptr=sptr+1
       cnt3=isto(env+1)
       cnt4=-1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9532
       GOTO 9018
 9532  from=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       ndx5=isto(env+2)
       stpl1=isto(env+6)
       stpc1=isto(env+7)
       ndx1=isto(env+8)
       ndx2=ndx1
       to=9020
       CALL xpr1
       to=9037
       CALL xpr2
       env=isto(sptr)
       sptr=sptr+1
       cnt3=isto(env+2)
       cnt4=-1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9533
       GOTO 9018
 9533  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3381
 3380 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9022 CONTINUE
      i1=isto(env+29)
      i2=isto(env+36)
      i=0
 3391 IF(.NOT.(i.LT.ndx10)) GOTO 3390
       i=i+1
       ndx9=ndx9+3
       ndx5=isto(ndx9+2)
       ndx4=isto(base4+ndx5)
       ndx6=isto(base6+ndx5)
       IF(.NOT.(ndx5.GE.cnt7)) GOTO 5411
        i3=i2+3*(ndx5-cnt7)
        GOTO 5410
 5411  CONTINUE
        i3=i1+4*(ndx5-cnt6)
 5410  CONTINUE
       i4=isto(i3)-7
       isto(i3+2)=iptr+1
       k=ndx6-ndx4+1
       j=0
 3401  IF(.NOT.(j.LT.k)) GOTO 3400
        j=j+1
        i4=i4+7
        IF(.NOT.(isto(i4+5).LT.1)) GOTO 5421
         isto(i4+5)=1
         GOTO 5420
 5421   CONTINUE
 5420   CONTINUE
        isto(iptr+1)=rptr+1
        isto(iptr+2)=0
        isto(iptr+3)=1
        isto(iptr+4)=0
        iptr=iptr+4
        rptr=rptr+2
        GOTO 3401
 3400  CONTINUE
       GOTO 3391
 3390 CONTINUE
      GOTO 9500
 9023 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      i0=isto(env+43)
      i1=isto(env+42)
      env=isto(sptr)
      sptr=sptr+1
      i2=isto(env+28)
      i9=isto(env+29)-4
      i3=1
      i=0
 3411 IF(.NOT.(i.LT.i2)) GOTO 3410
       i=i+1
       i5=cnt6+i-1
       i9=i9+4
       i7=i0-3
       j=0
       k=0
 3421  IF(.NOT.(j.LT.i1 .AND. k.EQ.0)) GOTO 3420
        j=j+1
        i7=i7+3
        IF(.NOT.(isto(i7+2).EQ.i5)) GOTO 5431
         k=1
         isto(i9+1)=i3
         i4=isto(base4+i5)
         i6=isto(base6+i5)
         i8=i6-i4+1
         i3=i3+i8
         GOTO 5430
 5431   CONTINUE
 5430   CONTINUE
        GOTO 3421
 3420  CONTINUE
       IF(.NOT.(k.EQ.0)) GOTO 5441
        isto(i9+1)=0
        GOTO 5440
 5441  CONTINUE
 5440  CONTINUE
       GOTO 3411
 3410 CONTINUE
      i2=isto(env+35)
      i9=isto(env+36)-3
      i=0
 3431 IF(.NOT.(i.LT.i2)) GOTO 3430
       i=i+1
       i5=cnt7+i-1
       i9=i9+3
       i7=i0-3
       j=0
       k=0
 3441  IF(.NOT.(j.LT.i1 .AND. k.EQ.0)) GOTO 3440
        j=j+1
        i7=i7+3
        IF(.NOT.(isto(i7+2).EQ.i5)) GOTO 5451
         k=1
         isto(i9+1)=i3
         i4=isto(base4+i5)
         i6=isto(base6+i5)
         i8=i6-i4+1
         i3=i3+i8
         GOTO 5450
 5451   CONTINUE
 5450   CONTINUE
        GOTO 3441
 3440  CONTINUE
       IF(.NOT.(k.EQ.0)) GOTO 5461
        isto(i9+1)=0
        GOTO 5460
 5461  CONTINUE
 5460  CONTINUE
       GOTO 3431
 3430 CONTINUE
      isto(env+13)=i3-1
      GOTO 9500
 9024 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+10)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+11)
 3451 IF(.NOT.(env.GT.0)) GOTO 3450
       IF(.NOT.(isto(env+1).EQ.8)) GOTO 5471
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+2)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+0)
 3461   IF(.NOT.(env.GT.0)) GOTO 3460
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=env
                    env=isto(env+4)
         ndx5=isto(env+2)
         stpl1=isto(env+6)
         stpc1=isto(env+7)
         ndx1=isto(env+8)
         ndx2=isto(env+9)
         to=9020
         CALL xpr1
         to=9037
         CALL xpr2
         env=isto(sptr)
         sptr=sptr+1
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=ptr1
                    ptr1=isto(env+5)
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=flg1
                    flg1=0
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=flg2
                    flg2=isto(env+3)
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9534
         GOTO 9026
 9534    from=isto(sptr)
         sptr=sptr+1
         flg2=isto(sptr)
         sptr=sptr+1
         flg1=isto(sptr)
         sptr=sptr+1
         ptr1=isto(sptr)
         sptr=sptr+1
         env=isto(env+2)
         GOTO 3461
 3460   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5470
 5471  CONTINUE
 5470  CONTINUE
       env=isto(env+0)
       GOTO 3451
 3450 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9025 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+48)
 3471 IF(.NOT.(env.GT.0)) GOTO 3470
       IF(.NOT.(isto(env+1).EQ.10)) GOTO 5481
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+2)
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+0)
 3481   IF(.NOT.(env.GT.0)) GOTO 3480
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=env
                    env=isto(env+4)
         ndx5=isto(env+2)
         stpl1=isto(env+6)
         stpc1=isto(env+7)
         ndx1=isto(env+8)
         ndx2=isto(env+9)
         to=9020
         CALL xpr1
         to=9037
         CALL xpr2
         env=isto(sptr)
         sptr=sptr+1
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=ptr1
                    ptr1=isto(env+5)
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=flg1
                    flg1=1
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=flg2
                    flg2=isto(env+2)
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9535
         GOTO 9026
 9535    from=isto(sptr)
         sptr=sptr+1
         flg2=isto(sptr)
         sptr=sptr+1
         flg1=isto(sptr)
         sptr=sptr+1
         ptr1=isto(sptr)
         sptr=sptr+1
         env=isto(env+1)
         GOTO 3481
 3480   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5480
 5481  CONTINUE
 5480  CONTINUE
       env=isto(env+0)
       GOTO 3471
 3470 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9026 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      i4=isto(base4+ndx5)
      k=ndx6-ndx4+1
      IF(.NOT.(ndx5.GE.cnt7)) GOTO 5491
       i1=isto(env+36)+3*(ndx5-cnt7)
       GOTO 5490
 5491 CONTINUE
       i1=isto(env+29)+4*(ndx5-cnt6)
 5490 CONTINUE
      IF(.NOT.(flg2.GT.0)) GOTO 5501
       i9=isto(i1+2)
       i10=i9+4*(ndx4-i4-1)
       i=0
 3491  IF(.NOT.(i.LT.k)) GOTO 3490
        i=i+1
        i10=i10+4
        IF(.NOT.(flg1.EQ.0 .OR. isto(i10+1).EQ.0
     1  )) GOTO 5511
         isto(i10+1)=ptr1
         isto(i10+3)=i
         GOTO 5510
 5511   CONTINUE
 5510   CONTINUE
        GOTO 3491
 3490  CONTINUE
       GOTO 5500
 5501 CONTINUE
       i2=isto(i1)
       i3=i2+7*(ndx4-i4-1)
       i=0
 3501  IF(.NOT.(i.LT.k)) GOTO 3500
        i=i+1
        i3=i3+7
        IF(.NOT.(flg1.EQ.0 .OR. isto(i3+3).EQ.0
     1  )) GOTO 5521
         isto(i3+3)=ptr1
         isto(i3+6)=i
         GOTO 5520
 5521   CONTINUE
 5520   CONTINUE
        GOTO 3501
 3500  CONTINUE
       IF(.NOT.(ndx5.LT.cnt7)) GOTO 5531
        i7=isto(i1+3)
        i8=i7+5*(ndx4-i4-1)
        i=0
 3511   IF(.NOT.(i.LT.k)) GOTO 3510
         i=i+1
         i8=i8+5
         IF(.NOT.(flg1.EQ.0 .OR. isto(i8+3).EQ.0
     1   )) GOTO 5541
          isto(i8+3)=ptr1
          isto(i8+4)=i
          GOTO 5540
 5541    CONTINUE
 5540    CONTINUE
         GOTO 3511
 3510   CONTINUE
        GOTO 5530
 5531  CONTINUE
 5530  CONTINUE
 5500 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9027 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+22)
 3521 IF(.NOT.(env.GT.0)) GOTO 3520
       IF(.NOT.(isto(env+2).EQ.0)) GOTO 5701
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=isto(env+4)
        ndx5=isto(env+2)
        stpl1=isto(env+6)
        stpc1=isto(env+7)
        ndx1=isto(env+8)
        ndx2=ndx1
        to=9020
        CALL xpr1
        to=9037
        CALL xpr2
        env=isto(sptr)
        sptr=sptr+1
        cnt3=1
        cnt4=-1
        ndx4=ndx4-1
        k=ndx6-ndx4
        i=0
 3531   IF(.NOT.(i.LT.k)) GOTO 3530
         i=i+1
         ndx4=ndx4+1
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9536
         GOTO 9018
 9536    from=isto(sptr)
         sptr=sptr+1
         GOTO 3531
 3530   CONTINUE
        GOTO 5700
 5701  CONTINUE
 5700  CONTINUE
        env=isto(env+0)
       GOTO 3521
 3520 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
