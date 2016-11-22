      SUBROUTINE init
      INCLUDE  'tacsto.ins'
      real*8     ATIM(2)      ! WSM + THL manual modification for BPA EMTP
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010), to-8999
      STOP 'Invalid "to" reference in "init".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501,  9502,  9503,  9504,  9505,
     1  9506), from- 9500
      STOP 'Invalid "from" reference in "init".'
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
      to=9004
      CALL calc
      to=9005
      CALL calc
      b=rsto(rptr)
      rptr=rptr-1
      a=rsto(rptr)
      rptr=rptr-1
      IF(.NOT.(a.LT.rmargn)) GOTO 5001
       a=zero
       GOTO 5000
 5001 CONTINUE
 5000 CONTINUE
      IF(.NOT.(b.LE.rmargn)) GOTO 5011
       stpflg=118
       GOTO 5010
 5011 CONTINUE
 5010 CONTINUE
      p=dmax1(rmargn,dabs(rmargn*b))
      IF(.NOT.(a.GT.b+p)) GOTO 5021
       stpflg=119
       GOTO 5020
 5021 CONTINUE
 5020 CONTINUE
      IF(.NOT.(stpflg.GT.0)) GOTO 5031
       stpr1=b
       stpr2=a
       CONTINUE
       CALL errstp
       GOTO 5030
 5031 CONTINUE
 5030 CONTINUE
      IF(.NOT.(a.GT.half*b+p)) GOTO 5041
       g=half*b
       GOTO 5040
 5041 CONTINUE
       g=a
 5040 CONTINUE
      r1=rsto(base13+6)
      c=rsto(base13+8) -r1
      e=r1
      IF(.NOT.(e.GT.b+p)) GOTO 5051
       f=e/dnint(e/b+half)
       GOTO 5050
 5051 CONTINUE
       f=e
 5050 CONTINUE
      rsto(base3+2)=g
      rsto(base3+3)=b
      rsto(base3+4)=c
      rsto(base3+5)=r1
      rsto(base3+6)=f
      rsto(base3+7)=c-f
      rsto(base3+8)=c
      stpt=c
      GOTO 9500
 9001 CONTINUE
      a=rsto(base3+8)
      b=rsto(base3+5)
      i1=isto(env+48)
      i2=isto(env+47)
      rsto(i1)=a
      i=i1+i2
 3001 IF(.NOT.(i.GT.i1+1)) GOTO 3000
       i=i-1
       a=a-b
       rsto(i)=a
       GOTO 3001
 3000 CONTINUE
      GOTO 9500
 9002 CONTINUE
      i1=isto(env+51)
      i2=isto(env+50)
      IF(.NOT.(i2.GT.0)) GOTO 5251
       a=rsto(base3+8)
       b=rsto(base3+6)
       rsto(i1)=a
       i=i1+i2
 3011  IF(.NOT.(i.GT.i1+1)) GOTO 3010
        i=i-1
        a=a-b
        rsto(i)=a
        GOTO 3011
 3010  CONTINUE
       GOTO 5250
 5251 CONTINUE
 5250 CONTINUE
      GOTO 9500
 9003 CONTINUE
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
      isto(sptr)=ndx7
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx8
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx9
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx10
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx11
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx12
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx13
      rptr=rptr+1
      IF(rptr.EQ.rsptr) GOTO 930
      rsto(rptr)=rsto(base7+1)
      ndx1=isto(env+28)
      ndx2=isto(env+20)+isto(env+22)
     1      +isto(env+25)
      ndx9=isto(env+29)-4
      ndx13=4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9501
      GOTO 9004
 9501 from=isto(sptr)
      sptr=sptr+1
      ndx2=ndx2+ndx1
      ndx1=isto(env+35)
      ndx9=isto(env+36)-3
      ndx13=3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9502
      GOTO 9004
 9502 from=isto(sptr)
      sptr=sptr+1
      rsto(base7+1)=rsto(rptr)
      rptr=rptr-1
      ndx13=isto(sptr)
      sptr=sptr+1
      ndx12=isto(sptr)
      sptr=sptr+1
      ndx11=isto(sptr)
      sptr=sptr+1
      ndx10=isto(sptr)
      sptr=sptr+1
      ndx9=isto(sptr)
      sptr=sptr+1
      ndx8=isto(sptr)
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
      ndx1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9004 CONTINUE
      ndx3=0
 3021 IF(.NOT.(ndx3.LT.ndx1)) GOTO 3020
       ndx3=ndx3+1
       ndx5=ndx2+ndx3
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=datenv
       i1=isto(env+1)+2*(ndx5-1)
       stpl1=isto(i1)
       stpc1=isto(i1+1)
       env=isto(sptr)
       sptr=sptr+1
       ndx4=isto(base4+ndx5)
       ndx6=isto(base6+ndx5)
       ndx7=ndx6-ndx4+1
       ndx9=ndx9+ndx13
       ndx10=isto(ndx9)
       IF(.NOT.(ndx10.GT.0)) GOTO 5061
        ndx10=ndx10-7
        GOTO 5060
 5061  CONTINUE
 5060  CONTINUE
       ndx11=isto(ndx9+2)
       IF(.NOT.(ndx11.GT.0)) GOTO 5071
        ndx11=ndx11-4
        GOTO 5070
 5071  CONTINUE
 5070  CONTINUE
       IF(.NOT.(ndx13.EQ.4)) GOTO 5081
        ndx12=isto(ndx9+3)-5
        GOTO 5080
 5081  CONTINUE
        ndx12=0
 5080  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9503
       GOTO 9005
 9503  from=isto(sptr)
       sptr=sptr+1
       GOTO 3021
 3020 CONTINUE
      GOTO 9500
 9005 CONTINUE
      ndx8=0
 3031 IF(.NOT.(ndx8.LT.ndx7)) GOTO 3030
       ndx8=ndx8+1
       stpi1=ndx8
       IF(.NOT.(ndx10.GT.0)) GOTO 5091
        ndx10=ndx10+7
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9504
        GOTO 9006
 9504   from=isto(sptr)
        sptr=sptr+1
        i2=isto(ndx10)
        IF(.NOT.(ndx13.EQ.3
     1  .AND. i2.GT.0)) GOTO 5101
         i1=base3+isto(base5+ndx5)-1+ndx8
         IF(.NOT.(rsto(i1).EQ.rnull)) GOTO 5111
          rsto(i1)=rsto(i2)
          GOTO 5110
 5111    CONTINUE
 5110    CONTINUE
         GOTO 5100
 5101   CONTINUE
 5100   CONTINUE
        GOTO 5090
 5091  CONTINUE
 5090  CONTINUE
       IF(.NOT.(ndx11.GT.0)) GOTO 5121
        ndx11=ndx11+4
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9505
        GOTO 9007
 9505   from=isto(sptr)
        sptr=sptr+1
        GOTO 5120
 5121  CONTINUE
 5120  CONTINUE
       IF(.NOT.(ndx12.GT.0)) GOTO 5131
        ndx12=ndx12+5
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9506
        GOTO 9008
 9506   from=isto(sptr)
        sptr=sptr+1
        GOTO 5130
 5131  CONTINUE
 5130  CONTINUE
       GOTO 3031
 3030 CONTINUE
      GOTO 9500
 9006 CONTINUE
      IF(.NOT.(isto(ndx10+5).GT.0)) GOTO 5261
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=cnt1
                  cnt1=1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=cnt2
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=cnt3
                  cnt3=0
       xprndx=isto(ndx10+3)
       IF(.NOT.(xprndx.GT.0)) GOTO 5141
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=rsto(base7+1)
        cnt2=isto(ndx10+5)
        a=rsto(base3+8)+rsto(base3+6)
        rsto(base7+1)=a
 3041   IF(.NOT.(cnt3.LT.cnt2)) GOTO 3040
         cnt3=cnt3+1
         a=rsto(base7+1)-rsto(base3+6)
         rsto(base7+1)=a
         xprndx=isto(ndx10+3)
         to=9000
         CALL xpr1
         i1=isto(ndx10+6)
         IF(.NOT.(xprcnt.LT.i1
     1   .AND. xprcnt.GT.1)) GOTO 5151
          stpflg=120
          stpi2=i1
          stpi3=xprcnt
          CONTINUE
          CALL errstp
          GOTO 5150
 5151    CONTINUE
 5150    CONTINUE
         rptr=rptr-xprcnt
         cnt1=cnt1-1
         IF(.NOT.(cnt1.LT.0)) GOTO 5161
          cnt1=cnt1+cnt2
          GOTO 5160
 5161    CONTINUE
 5160    CONTINUE
         i2=isto(ndx10)
         IF(.NOT.(xprcnt.GT.1)) GOTO 5271
          a=rsto(rptr+i1)
          GOTO 5270
 5271    CONTINUE
          a=rsto(rptr+1)
 5270    CONTINUE
         rsto(i2+cnt1)=a
         GOTO 3041
 3040   CONTINUE
        rsto(base7+1)=rsto(rptr)
        rptr=rptr-1
        GOTO 5140
 5141  CONTINUE
 5140  CONTINUE
       cnt3=isto(sptr)
       sptr=sptr+1
       cnt2=isto(sptr)
       sptr=sptr+1
       cnt1=isto(sptr)
       sptr=sptr+1
       GOTO 5260
 5261 CONTINUE
 5260 CONTINUE
      GOTO 9500
 9007 CONTINUE
      xprndx=isto(ndx11+1)
      IF(.NOT.(xprndx.GT.0)) GOTO 5171
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=rsto(base7+1)
       rsto(base7+1)=rsto(base3+8)+rsto(base3+6)
       to=9000
       CALL xpr1
       i1=isto(ndx11+3)
       IF(.NOT.(xprcnt.LT.i1
     1 .AND. xprcnt.GT.1)) GOTO 5181
        stpflg=120
        stpi2=i1
        stpi3=xprcnt
        CONTINUE
        CALL errstp
        GOTO 5180
 5181  CONTINUE
 5180  CONTINUE
       rptr=rptr-xprcnt
       i2=isto(ndx11)
       IF(.NOT.(xprcnt.GT.1)) GOTO 5281
        a=rsto(rptr+i1)
        GOTO 5280
 5281  CONTINUE
        a=rsto(rptr+1)
 5280  CONTINUE
       rsto(i2)=a
       rsto(base7+1)=rsto(rptr)
       rptr=rptr-1
       GOTO 5170
 5171 CONTINUE
 5170 CONTINUE
      GOTO 9500
 9008 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt1
                 cnt1=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt3
                 cnt3=0
      xprndx=isto(ndx12+3)
      IF(.NOT.(xprndx.GT.0)) GOTO 5191
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=rsto(base7+1)
       cnt2=2*isto(ndx12+1)+1
       IF(.NOT.(cnt2.LT.2)) GOTO 5201
        cnt2=2
        GOTO 5200
 5201  CONTINUE
 5200  CONTINUE
       a=rsto(base3+8)+rsto(base3+5)
       rsto(base7+1)=a
 3051  IF(.NOT.(cnt3.LT.cnt2)) GOTO 3050
        cnt3=cnt3+1
        a=rsto(base7+1)-rsto(base3+5)
        rsto(base7+1)=a
        xprndx=isto(ndx12+3)
        to=9000
        CALL xpr1
        i1=isto(ndx12+4)
        IF(.NOT.(xprcnt.LT.i1
     1  .AND. xprcnt.GT.1)) GOTO 5211
         stpflg=120
         stpi2=i1
         stpi3=xprcnt
         CONTINUE
         CALL errstp
         GOTO 5210
 5211   CONTINUE
 5210   CONTINUE
        rptr=rptr-xprcnt
        cnt1=cnt1-1
        IF(.NOT.(cnt1.LT.0)) GOTO 5221
         cnt1=cnt1+cnt2
         GOTO 5220
 5221   CONTINUE
 5220   CONTINUE
        i2=isto(ndx12)
        IF(.NOT.(xprcnt.GT.1)) GOTO 5291
         a=rsto(rptr+i1)
         GOTO 5290
 5291   CONTINUE
         a=rsto(rptr+1)
 5290   CONTINUE
        rsto(i2+cnt1)=a
        GOTO 3051
 3050  CONTINUE
       rsto(base7+1)=rsto(rptr)
       rptr=rptr-1
       GOTO 5190
 5191 CONTINUE
 5190 CONTINUE
      cnt3=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9009 CONTINUE
CCCC      a=seedz(a)         ! WSM + THL manual modification for BPA EMTP
      CALL TIME44 ( ATIM(1) )    ! WSM + THL manual modification for BPA EMTP
      CALL RUNTYM ( D1, D2 )     ! WSM + THL manual modification for BPA EMTP
      A = SEEDY ( ATIM(1) ) + 1000. * ( D1 + D2 )  ! WSM + THL manual modification for BPA EMTP
      a=randnM ( a )         ! WSM + THL manual modification for BPA EMTP
      rsto(base7+10)=pi
      rsto(base7+11)=rinf
      rsto(base7+12)=zero
      rsto(base7+13)=one
      rsto(base7+14)=zero
      rsto(base7+15)=one
      rsto(base7+16)=zero
      rsto(base7+17)=one
      rsto(base7+18)=zero
      rsto(base7+19)=one
      rsto(base7+20)=rnull
      rsto(base7+1)=etime
      rsto(base7+2)=etime
      rsto(base7+3)=estop
      rsto(base7+4)=estep
      rsto(base3+1)=rnull
      rsto(base3+2)=zero
      rsto(base3+3)=rinf
      rsto(base3+4)=etime
      rsto(base3+5)=estep
      rsto(base3+6)=estep
      rsto(base3+7)=etime-estep
      rsto(base3+8)=etime
      stpt=etime
      GOTO 9500
 9010 CONTINUE
      useenv=iptr+1
      isto(env+3)=useenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      i=8
 3061 IF(.NOT.(i.LT.57)) GOTO 3060
       i=i+1
       isto(iptr+i)=0
       GOTO 3061
 3060 CONTINUE
      iptr=iptr+57
      isto(env+0)=0
      isto(env+1)=1
      isto(env+2)=1
      isto(env+3)=4
      isto(env+4)=146
      isto(env+5)=iinf
      isto(env+6)=4
      isto(env+7)=146
      isto(ishenv+61)=isto(env+6)
      isto(ishenv+62)=isto(env+7)
      isto(env+9)=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      i4=isto(env+0)
      i5=isto(env+4)
      i8=isto(env+9)
      i9=isto(env+11)
      env=isto(sptr)
      sptr=sptr+1
      isto(env+15)=i4
      isto(env+16)=i4
      isto(env+20)=i5
      isto(env+28)=i8
      isto(env+30)=i8
      isto(env+35)=i9
      isto(env+37)=i9
      isto(env+52)=iptr+1
      i=0
 3071 IF(.NOT.(i.LT.i4)) GOTO 3070
       i=i+1
       isto(iptr+i)=i
       GOTO 3071
 3070 CONTINUE
      iptr=iptr+i4
      isto(env+53)=iptr+1
      i=0
 3081 IF(.NOT.(i.LT.i4)) GOTO 3080
       i=i+1
       isto(iptr+i)=1
       GOTO 3081
 3080 CONTINUE
      iptr=iptr+i4
      isto(env+54)=iptr+1
      i=0
 3091 IF(.NOT.(i.LT.i4)) GOTO 3090
       i=i+1
       isto(iptr+i)=1
       GOTO 3091
 3090 CONTINUE
      iptr=iptr+i4
      base5=isto(env+52)-1
      base4=isto(env+53)-1
      base6=isto(env+54)-1
      base7=rptr
      i=0
 3101 IF(.NOT.(i.LT.24)) GOTO 3100
       i=i+1
       rsto(rptr+i)=rnull
       GOTO 3101
 3100 CONTINUE
      rptr=rptr+24
      base3=rptr
      isto(env+21)=base3+1
      IF(.NOT.(isto(env+28).GT.0)) GOTO 5231
      isto(env+31)=isto(env+21)+isto(env+20)
       GOTO 5230
 5231 CONTINUE
 5230 CONTINUE
      IF(.NOT.(isto(env+35).GT.0)) GOTO 5241
      isto(env+38)=isto(env+21)+isto(env+20)
     1           +isto(env+28)
       GOTO 5240
 5241 CONTINUE
 5240 CONTINUE
      i=0
 3111 IF(.NOT.(i.LT.i4)) GOTO 3110
       i=i+1
       rsto(rptr+i)=rnull
       GOTO 3111
 3110 CONTINUE
      rptr=rptr+i4
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
