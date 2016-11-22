      SUBROUTINE calc
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 0002, 9010,
     1 9011, 9012, 9013, 9014, 9015,
     1 9016), to-8999
 0002 STOP 'Invalid "to" reference in "calc".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501,  9502,  9503,  9504,  9505,
     1  9506,  9507,  9508,  9509), from-9500
      STOP 'Invalid "from" reference in "calc".'
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      i0=isto(env+31)-1
      k=isto(env+30)
      env=isto(sptr)
      sptr=sptr+1
      i=0
 3001 IF(.NOT.(i.LT.k)) GOTO 3000
       i=i+1
       rsto(i0+i)=rnull
       GOTO 3001
 3000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+11)
 3011 IF(.NOT.(env.GT.0)) GOTO 3010
       IF(.NOT.(isto(env+1).EQ.5)) GOTO 5001
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
         GOTO 9001
 9501    from=isto(sptr)
         sptr=sptr+1
         env=isto(env+0)
         GOTO 3021
 3020   CONTINUE
        env=isto(sptr)
        sptr=sptr+1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5000
 5001  CONTINUE
 5000  CONTINUE
       env=isto(env+0)
       GOTO 3011
 3010 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+10)
 3031 IF(.NOT.(env.GT.0)) GOTO 3030
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9502
       GOTO 9002
 9502  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3031
 3030 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9503
      GOTO 9003
 9503 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9001 CONTINUE
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
      xprndx=isto(env+2)
      to=9000
      CALL xpr1
      ndx3=xprcnt
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
      to=9037
      CALL xpr2
      xprcnt=ndx3
      to=9000
      CALL xpr2
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
      xprndx=isto(env+5)
      IF(.NOT.(xprndx.GT.0)) GOTO 5011
       to=9000
       CALL xpr1
       ndx3=xprcnt
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       stpl1=isto(env+0)
       stpc1=isto(env+1)
       ndx1=isto(env+2)
       ndx2=isto(env+3)
       env=isto(sptr)
       sptr=sptr+1
       ndx5=isto(env+2)
       to=9020
       CALL xpr1
       to=9037
       CALL xpr2
       xprcnt=ndx3
       to=9001
       CALL xpr2
       GOTO 5010
 5011 CONTINUE
 5010 CONTINUE
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
      i0=isto(env+31) -1
      i5=isto(env+20)+isto(env+22)+isto(env+25)
      i2=i5+isto(env+28)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      i3=isto(env+1) +2*(i5-1)
      env=isto(sptr)
      sptr=sptr+1
 3041 IF(.NOT.(i5.LT.i2)) GOTO 3040
       i5=i5+1
       i3=i3+2
       stpl1=isto(i3)
       stpc1=isto(i3+1)
       i4=isto(base4+i5)
       i6=isto(base6+i5)
       i1=base3 +isto(base5+i5) -1
       k=i6-i4+1
       i=0
 3051  IF(.NOT.(i.LT.k)) GOTO 3050
        i=i+1
        IF(.NOT.(rsto(i1+i).EQ.rnull)) GOTO 5021
         stpflg=104
         stpi1=i4
         stpi2=i6
         stpi3=i4+i-1
         CONTINUE
         CALL errstp
         GOTO 5020
 5021   CONTINUE
 5020   CONTINUE
        GOTO 3051
 3050  CONTINUE
       GOTO 3041
 3040 CONTINUE
      GOTO 9500
 9004 CONTINUE
      xprndx=isto(env+9)
      IF(.NOT.(xprndx.EQ.0)) GOTO 5031
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=datenv
       xprndx=isto(env+44)
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5030
 5031 CONTINUE
 5030 CONTINUE
      IF(.NOT.(xprndx.GT.0)) GOTO 5041
       to=9000
       CALL xpr1
       GOTO 5040
 5041 CONTINUE
       a=zero
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=a
 5040 CONTINUE
      GOTO 9500
 9005 CONTINUE
      xprndx=isto(env+10)
      IF(.NOT.(xprndx.EQ.0)) GOTO 5051
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=datenv
       xprndx=isto(env+45)
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5050
 5051 CONTINUE
 5050 CONTINUE
      IF(.NOT.(xprndx.GT.0)) GOTO 5061
       to=9000
       CALL xpr1
       GOTO 5060
 5061 CONTINUE
       a=rinf
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=a
 5060 CONTINUE
      GOTO 9500
 9006 CONTINUE
      j=isto(env+47)
      k=isto(env+46)+1
      IF(.NOT.(k.GE.j)) GOTO 5071
       k=0
       GOTO 5070
 5071 CONTINUE
 5070 CONTINUE
      isto(env+46)=k
      i0=isto(env+48)
      rsto(i0+k)=c
      GOTO 9500
 9007 CONTINUE
      i2=isto(env+31)-1
      i3=isto(env+30)
      i0=isto(env+32)
      i=0
 3061 IF(.NOT.(i.LT.i3)) GOTO 3060
       i=i+1
       a=rsto(i2+i)
       i1=2*isto(i0+1)
       IF(.NOT.(i1.EQ.0)) GOTO 5081
       i1=1
        GOTO 5080
 5081  CONTINUE
 5080  CONTINUE
       i5=isto(i0+2)+1
       IF(.NOT.(i5.GT.i1)) GOTO 5091
       i5=0
        GOTO 5090
 5091  CONTINUE
 5090  CONTINUE
       isto(i0+2)=i5
       i4=isto(i0)
       rsto(i4+i5)=a
       i0=i0+5
       GOTO 3061
 3060 CONTINUE
      GOTO 9500
 9008 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=isto(env+30)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
                 ndx3=isto(env+31)-1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+34)-1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
                 ndx5=isto(env+47)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx6
                 ndx6=isto(env+46)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx7
                 ndx7=isto(env+48)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+32)-5
      IF(.NOT.(flg.LT.0)) GOTO 5101
 3071  IF(.NOT.(ndx.LT.ndx0)) GOTO 3070
        ndx=ndx+1
        env=env+5
        i2=isto(env+0)+isto(env+2)
        rsto(ndx3+ndx)=rsto(i2)
        GOTO 3071
 3070  CONTINUE
       GOTO 5100
 5101 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr0
                  ptr0=rptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr1
                  ptr1=ptr0
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr2
                  ptr2=ptr1+ndx5
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=cnt
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ipol
       a=rsto(base3+8)
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=a
       j=ndx6+1
       i=0
 3081  IF(.NOT.(i.LT.ndx5)) GOTO 3080
        i=i+1
        j=j-1
        IF(.NOT.(j.LT.0)) GOTO 5111
         j=j+ndx5
         GOTO 5110
 5111   CONTINUE
 5110   CONTINUE
        rsto(ptr1+i)=rsto(ndx7+j)
        GOTO 3081
 3080  CONTINUE
       rptr=ptr1+ndx5
 3091  IF(.NOT.(ndx.LT.ndx0)) GOTO 3090
        ndx=ndx+1
        env=env+5
        ipol=isto(env+1)
        cnt=2*ipol+1
        IF(.NOT.(cnt.EQ.1)) GOTO 5121
         cnt=2
         GOTO 5120
 5121   CONTINUE
 5120   CONTINUE
        ndx2=isto(env+0)
        k=isto(env+2)+1
        i=0
 3101   IF(.NOT.(i.LT.cnt)) GOTO 3100
         i=i+1
         k=k-1
         IF(.NOT.(k.LT.0)) GOTO 5131
          k=k+cnt
          GOTO 5130
 5131    CONTINUE
 5130    CONTINUE
         a=rsto(ndx2+k)
         IF(.NOT.(a.EQ.rnull)) GOTO 5141
          stpflg=40
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=env
                     env=useenv
          i1=isto(env+31)-base3 -1
          i=1
 3111     IF(.NOT.(isto(base5+i)-i1.LE.ndx
     1    )) GOTO 3110
           i=i+1
           GOTO 3111
 3110     CONTINUE
          i5=i-1
          stpi1=isto(base4+i5) +ndx
     1           - (isto(base5+i5) -i1)
          env=isto(sptr)
          sptr=sptr+1
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=env
                     env=datenv
          i2=isto(env+1)-2
          stpl1=isto(i2+2*i5)
          stpc1=isto(i2+2*i5+1)
          env=isto(sptr)
          sptr=sptr+1
          CONTINUE
          CALL errstp
          GOTO 5140
 5141    CONTINUE
          rsto(ptr2+i)=a
 5140    CONTINUE
         GOTO 3101
 3100   CONTINUE
        IF(.NOT.(ipol.EQ.0)) GOTO 5241
         rsto(ndx3+ndx)=rsto(ptr2+2)
         GOTO 5240
 5241   CONTINUE
         rptr=ptr2+cnt
         to=9020
         CALL xpr2
         rsto(ndx3+ndx)=rsto(ptr0)
 5240   CONTINUE
        GOTO 3091
 3090  CONTINUE
       rptr=ptr0-1
       ipol=isto(sptr)
       sptr=sptr+1
       cnt=isto(sptr)
       sptr=sptr+1
       ptr2=isto(sptr)
       sptr=sptr+1
       ptr1=isto(sptr)
       sptr=sptr+1
       ptr0=isto(sptr)
       sptr=sptr+1
 5100 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx7=isto(sptr)
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
      ndx0=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9010 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      IF(.NOT.(isto(env+43).GT.0)) GOTO 5151
       ptr1=isto(env+45)-1
       ptr0=isto(env+29)-4
       cnt1=isto(env+28)
       cnt2=isto(env+20)+isto(env+22)+isto(env+25)
       cnt4=base4+cnt2
       cnt5=base5+cnt2
       cnt6=base6+cnt2
       ndx1=0
 3131  IF(.NOT.(ndx1.LT.cnt1)) GOTO 3130
        ndx1=ndx1+1
        ptr0=ptr0+4
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9504
        GOTO 9011
 9504   from=isto(sptr)
        sptr=sptr+1
        GOTO 3131
 3130  CONTINUE
       ptr0=isto(env+36)-3
       cnt2=cnt2+cnt1
       cnt1=isto(env+35)
       cnt4=base4+cnt2
       cnt5=base5+cnt2
       cnt6=base6+cnt2
       ndx1=0
 3141  IF(.NOT.(ndx1.LT.cnt1)) GOTO 3140
        ndx1=ndx1+1
        ptr0=ptr0+3
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9505
        GOTO 9011
 9505   from=isto(sptr)
        sptr=sptr+1
        GOTO 3141
 3140  CONTINUE
       GOTO 5150
 5151 CONTINUE
 5150 CONTINUE
      ndx1=isto(sptr)
      sptr=sptr+1
      cnt6=isto(sptr)
      sptr=sptr+1
      cnt5=isto(sptr)
      sptr=sptr+1
      cnt4=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9011 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx6
      ndx4=isto(cnt4+ndx1)
      ndx5=isto(cnt5+ndx1)
      ndx6=isto(cnt6+ndx1)
      cnt=ndx6-ndx4+1
      i=isto(ptr0+2)
      IF(.NOT.(i.GT.0)) GOTO 5161
       ptr2=i-4
       ptr3=isto(ptr0)-7
       ndx2=0
 3151  IF(.NOT.(ndx2.LT.cnt)) GOTO 3150
        ndx2=ndx2+1
        ptr2=ptr2+4
        ptr3=ptr3+7
        i1=isto(ptr2+2)
        IF(.NOT.(i1.GT.1)) GOTO 5171
         i2=isto(ptr2)
         rsto(i2)=rsto(i2+1)
         GOTO 5170
 5171   IF(.NOT.(i1.EQ.0)) GOTO 5172
         to=9028
         CALL xpr1
         a=rsto(rptr)
         rptr=rptr-1
         b=rsto(rptr)
         rptr=rptr-1
         i0=base3 +ndx5 -1 +ndx2
         c=rsto(i0)
         d=b*c +a
         i2=isto(ptr2)
         rsto(i2)=d
         GOTO 5170
 5172   CONTINUE
 5170   CONTINUE
        isto(ptr2+2)=0
        GOTO 3151
 3150  CONTINUE
       GOTO 5160
 5161 CONTINUE
 5160 CONTINUE
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9012 CONTINUE
      j=isto(env+50)
      k=isto(env+49)+1
      IF(.NOT.(k.GE.j)) GOTO 5181
       k=0
       GOTO 5180
 5181 CONTINUE
 5180 CONTINUE
      isto(env+49)=k
      i=isto(env+51)
      rsto(i+k)=rsto(base3 +8)
      GOTO 9500
 9013 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      ptr1=isto(env+42)-1
      ptr0=isto(env+29)-4
      cnt1=isto(env+28)
      cnt2=isto(env+20)+isto(env+22)+isto(env+25)
      cnt4=base4+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3161 IF(.NOT.(ndx1.LT.cnt1)) GOTO 3160
       ndx1=ndx1+1
       ptr0=ptr0+4
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9506
       GOTO 9014
 9506  from=isto(sptr)
       sptr=sptr+1
       GOTO 3161
 3160 CONTINUE
      ptr0=isto(env+36)-3
      cnt2=cnt2+cnt1
      cnt1=isto(env+35)
      cnt4=base4+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3171 IF(.NOT.(ndx1.LT.cnt1)) GOTO 3170
       ndx1=ndx1+1
       ptr0=ptr0+3
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9507
       GOTO 9014
 9507  from=isto(sptr)
       sptr=sptr+1
       GOTO 3171
 3170 CONTINUE
      ndx1=isto(sptr)
      sptr=sptr+1
      cnt6=isto(sptr)
      sptr=sptr+1
      cnt4=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9014 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt
      i4=isto(cnt4+ndx1)
      i6=isto(cnt6+ndx1)
      cnt=i6-i4+1
      i=isto(ptr0)
      IF(.NOT.(i.GT.0)) GOTO 5191
       i0=i-7
       i5=cnt2+ndx1
       i1=base3+isto(base5+i5)-1
       i=0
 3181  IF(.NOT.(i.LT.cnt)) GOTO 3180
        i=i+1
        i0=i0+7
        j=isto(i0+5)
        IF(.NOT.(j.GT.0)) GOTO 5231
         i2=isto(i0)
         a=rsto(i1+i)
         IF(.NOT.(isto(i0+4).GT.0)) GOTO 5201
          isto(i0+4)=0
          i3=-1
 3191     IF(.NOT.(i3.LT.j-1)) GOTO 3190
           i3=i3+1
           rsto(i2+i3)=a
           GOTO 3191
 3190     CONTINUE
          GOTO 5200
 5201    CONTINUE
          i3=isto(i0+2)+1
          IF(.NOT.(i3.GE.j)) GOTO 5211
           i3=0
           GOTO 5210
 5211     CONTINUE
 5210     CONTINUE
          isto(i0+2)=i3
          rsto(i2+i3)=a
 5200    CONTINUE
         GOTO 5230
 5231   CONTINUE
 5230   CONTINUE
        GOTO 3181
 3180  CONTINUE
       GOTO 5190
 5191 CONTINUE
 5190 CONTINUE
      cnt=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9015 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr0
                 ptr0=iptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=isto(env+14)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx6
      isto(iptr+1)=ibkptr+1
      ndx=1
      ndx4=ptr1
      ndx6=ndx4
      to=9032
      CALL xpr2
      isto(iptr+1)=0
      isto(iptr+2)=ptr1
      isto(iptr+3)=rbkptr+1
      ndx=1
      ndx4=ibkptr+1
      ndx6=ndx4+2
      to=9032
      CALL xpr2
      isto(env+14)=ndx4
      ibkptr=ndx6
      ptr0=rptr
      ptr1=rptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      a=rsto(base3+8)
      rptr=rptr+1
      IF(rptr.EQ.rsptr) GOTO 930
      rsto(rptr)=a
      ptr2=isto(env+29)-4
      cnt1=isto(env+28)
      cnt2=isto(env+20)+isto(env+22)+isto(env+25)
      cnt4=base4+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3201 IF(.NOT.(ndx1.LT.cnt1)) GOTO 3200
       ndx1=ndx1+1
       ptr2=ptr2+4
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9508
       GOTO 9016
 9508  from=isto(sptr)
       sptr=sptr+1
       GOTO 3201
 3200 CONTINUE
      ptr2=isto(env+36)-3
      cnt2=cnt2+cnt1
      cnt1=isto(env+35)
      cnt4=base4+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3211 IF(.NOT.(ndx1.LT.cnt1)) GOTO 3210
       ndx1=ndx1+1
       ptr2=ptr2+3
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9509
       GOTO 9016
 9509  from=isto(sptr)
       sptr=sptr+1
       GOTO 3211
 3210 CONTINUE
      ndx1=isto(sptr)
      sptr=sptr+1
      cnt6=isto(sptr)
      sptr=sptr+1
      cnt4=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ndx=1
      ndx4=rbkptr+1
      ndx6=ndx4+rptr-ptr1
      to=9033
      CALL xpr2
      rbkptr=ndx6
      rptr=ptr1-1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9016 CONTINUE
      i4=isto(cnt4+ndx1)
      i6=isto(cnt6+ndx1)
      cnt=i6-i4+1
      i=isto(ptr2+1)
      IF(.NOT.(i.GT.0)) GOTO 5221
       i5=cnt2+ndx1
       i1=base3 +isto(base5+i5) -1
       i=0
 3221  IF(.NOT.(i.LT.cnt)) GOTO 3220
        i=i+1
        a=rsto(i1+i)
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=a
        GOTO 3221
 3220  CONTINUE
        GOTO 5220
 5221 CONTINUE
 5220 CONTINUE
      GOTO 9500
      END
