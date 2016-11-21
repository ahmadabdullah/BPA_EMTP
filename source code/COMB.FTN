      SUBROUTINE comb
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      isto(sptr)=from
                 from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010,
     1 9011, 9012, 9013, 9014, 9015,
     1 9016, 9017, 9018, 9019, 9020,
     1 9021, 9022), to-8999
      STOP 'Invalid "to" reference in "comb".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501,  9502,  9503,  9504,  9505,
     1  9506,  9507,  9508,  9509,  9510,
     1  9511,  9512,  9513,  9514,  9515,
     1  0002,  0002,  0002,  0002,  9520,
     1  9521,  9522,  9523,  9524,  9525,
     1  9526), from- 9500
 0002 STOP 'Invalid "from" reference in "comb".'
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
      IF(sptr-50.LE.iptr) GOTO 910
      IF(rptr+20.GE.rsptr) GOTO 930
      sptr=sptr-1
      isto(sptr)=cnt1
                 cnt1=isto(env+7)
      sptr=sptr-1
      isto(sptr)=cnt2
      sptr=sptr-1
      isto(sptr)=cnt3
                 cnt3=isto(env+8)
      sptr=sptr-1
      isto(sptr)=ndx1
                 ndx1=isto(env+1)
      sptr=sptr-1
      isto(sptr)=ndx2
                 ndx2=isto(env+2)
      sptr=sptr-1
      isto(sptr)=ndx3
      sptr=sptr-1
      isto(sptr)=ptr0
                 ptr0=env
      sptr=sptr-1
      isto(sptr)=ptr1
      sptr=sptr-1
      isto(sptr)=ptr2
      sptr=sptr-1
      isto(sptr)=ptr3
      sptr=sptr-1
      isto(sptr)=ptr4
      sptr=sptr-1
      isto(sptr)=ptr5
      sptr=sptr-1
      isto(sptr)=ndx0
      xprndx=isto(env+3)
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
      isto(sptr)=from
      from=9501
      GOTO 9001
 9501 from=isto(sptr)
      sptr=sptr+1
      i1=ptr2-cnt2
      i=0
 3001 IF(.NOT.(i.LT.cnt1)) GOTO 3000
       i=i+1
       rsto(ptr1+i)=zero
       i1=i1+cnt2
       j=0
 3011  IF(.NOT.(j.LT.cnt2)) GOTO 3010
        j=j+1
        rsto(i1+j)=zero
        GOTO 3011
 3010  CONTINUE
       GOTO 3001
 3000 CONTINUE
      ndx0=0
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+4)
 3021 IF(.NOT.(env.GT.0)) GOTO 3020
       ndx0=ndx0+1
       k=isto(env+4)
       sptr=sptr-1
       isto(sptr)=env
                  env=isto(env+5)
       IF(.NOT.(k.EQ.1)) GOTO 5011
        sptr=sptr-1
        isto(sptr)=from
        from=9502
        GOTO 9004
 9502   from=isto(sptr)
        sptr=sptr+1
        GOTO 5010
 5011  IF(.NOT.(k.EQ.2)) GOTO 5012
        sptr=sptr-1
        isto(sptr)=from
        from=9503
        GOTO 9009
 9503   from=isto(sptr)
        sptr=sptr+1
        GOTO 5010
 5012  IF(.NOT.(k.EQ.3)) GOTO 5013
        sptr=sptr-1
        isto(sptr)=from
        from=9504
        GOTO 9010
 9504   from=isto(sptr)
        sptr=sptr+1
        GOTO 5010
 5013  CONTINUE
 5010  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3021
 3020 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      isto(sptr)=from
      from=9505
      GOTO 9011
 9505 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.GT.0)) GOTO 5021
       stpflg=109
       stpi2=flg
       CONTINUE
       CALL ERRSTP
       GOTO 5020
 5021 CONTINUE
 5020 CONTINUE
      flg=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      isto(sptr)=from
      from=9506
      GOTO 9012
 9506 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      isto(sptr)=from
      from=9507
      GOTO 9013
 9507 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(isto(env+6).GT.0)) GOTO 5031
       sptr=sptr-1
       isto(sptr)=from
       from=9508
       GOTO 9015
 9508  from=isto(sptr)
       sptr=sptr+1
       GOTO 5030
 5031 CONTINUE
 5030 CONTINUE
      sptr=sptr-1
      isto(sptr)=from
      from=9509
      GOTO 9014
 9509 from=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      ptr5=isto(sptr+1)
      ptr4=isto(sptr+2)
      ptr3=isto(sptr+3)
      ptr2=isto(sptr+4)
      ptr1=isto(sptr+5)
      ptr0=isto(sptr+6)
      sptr=sptr+7
      ndx3=isto(sptr)
      ndx2=isto(sptr+1)
      ndx1=isto(sptr+2)
      cnt3=isto(sptr+3)
      cnt2=isto(sptr+4)
      cnt1=isto(sptr+5)
      sptr=sptr+6
      GOTO 9500
 9001 CONTINUE
      cnt2=0
      sptr=sptr-1
      isto(sptr)=env
                 env=useenv
      i1=isto(env+55)
      IF(.NOT.(i1.GT.0)) GOTO 5041
       sptr=sptr-1
       isto(sptr)=env
                  env=i1
 3031  IF(.NOT.(env.GT.0 .AND. cnt2.EQ.0)) GOTO 3030
        IF(.NOT.(isto(env+1).EQ.ndx1
     1  .AND. isto(env+2).EQ.ndx2
     2  .AND. isto(env+3).EQ.ndx3
     3  )) GOTO 5051
         cnt2=isto(env+9)
         ptr1=isto(env+4)-1
         ptr2=isto(env+5)-1
         ptr3=isto(env+7)-1
         ptr4=isto(env+6)-1
         ptr5=isto(env+10)-1
         GOTO 5050
 5051   CONTINUE
         i2=env
         env=isto(env+0)
 5050   CONTINUE
        GOTO 3031
 3030  CONTINUE
       IF(.NOT.(cnt2.EQ.0)) GOTO 5061
        env=i2
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5060
 5061  CONTINUE
 5060  CONTINUE
       GOTO 5040
 5041 CONTINUE
       isto(env+55)=iptr+1
       sptr=sptr-1
       isto(sptr)=env
                  env=iptr+1
 5040 CONTINUE
      IF(.NOT.(cnt2.EQ.0)) GOTO 5071
       sptr=sptr-1
       isto(sptr)=from
       from=9510
       GOTO 9002
 9510  from=isto(sptr)
       sptr=sptr+1
       GOTO 5070
 5071 CONTINUE
 5070 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9002 CONTINUE
      IF(.NOT.(ndx3.EQ.iinf)) GOTO 5261
       GOTO 5260
 5261 CONTINUE
 5260 CONTINUE
      iptr=iptr+11
      isto(env+0)=0
      isto(env+1)=ndx1
      isto(env+2)=ndx2
      isto(env+3)=ndx3
      isto(env+8)=cnt1
      ptr5=iptr
      isto(env+10)=iptr+1
      iptr=iptr+cnt1
      sptr=sptr-1
      isto(sptr)=env
                 env=ptr0
      i0=isto(env+6)
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(i0.GT.0)) GOTO 5351
       k=cnt1-1
       isto(ptr5+cnt1)=cnt1
       GOTO 5350
 5351 CONTINUE
       k=cnt1
 5350 CONTINUE
      i=0
 3181 IF(.NOT.(i.LT.k)) GOTO 3180
       i=i+1
       isto(ptr5+i)=k-i+1
       GOTO 3181
 3180 CONTINUE
      ptr4=iptr
      isto(env+6)=iptr+1
      iptr=iptr+cnt3
      ptr3=iptr
      isto(env+7)=ptr3+1
      sptr=sptr-1
      isto(sptr)=from
      from=9511
      GOTO 9003
 9511 from=isto(sptr)
      sptr=sptr+1
      iptr=iptr+cnt2
      isto(env+9)=cnt2
      ptr1=rptr
      isto(env+4)=ptr1+1
      rptr=rptr+cnt1
      ptr2=rptr
      isto(env+5)=ptr2+1
      rptr=rptr+ cnt1*cnt2
      GOTO 9500
 9003 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx1
                 ndx1=0
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx3
                 ndx3=0
      sptr=sptr-1
      isto(sptr)=env
                 env=ptr0
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+4)
 3041 IF(.NOT.(env.GT.0)) GOTO 3040
       ndx1=ndx1+1
       xprndx=isto(env+3)
       IF(.NOT.(xprndx.GT.0)) GOTO 5081
        to=9000
        CALL xpr1
        a=rsto(rptr)
        rptr=rptr-1
        k=idnint(a)
        GOTO 5080
 5081  CONTINUE
        k=1
 5080  CONTINUE
       i5=isto(env+2)
       i2=isto(base5+i5)-isto(base4+i5)+k
       j=0
 3051  IF(.NOT.(j.LT.ndx1-1)) GOTO 3050
        j=j+1
        IF(.NOT.(isto(ptr3+j).EQ.i2)) GOTO 5091
         stpflg=110
         sptr=sptr-1
         isto(sptr)=env
                    env=datenv
         i3=isto(env+1) +2*(j-1)
         stpl2=isto(i3)
         stpc2=isto(i3+1)
         env=isto(sptr)
         sptr=sptr+1
         stpi2=k
         CONTINUE
         CALL ERRSTP
         GOTO 5090
 5091   CONTINUE
 5090   CONTINUE
        GOTO 3051
 3050  CONTINUE
       isto(ptr3+ndx1)=i2
       env=isto(env+0)
       GOTO 3041
 3040 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      cnt2=cnt1
      ndx2=isto(env+9)-2
 3061 IF(.NOT.(ndx3.LT.cnt3)) GOTO 3060
       ndx3=ndx3+1
       ndx2=ndx2+2
       xprndx=isto(ndx2+1)
       IF(.NOT.(xprndx.GT.0)) GOTO 5101
        to=9000
        CALL xpr1
        a=rsto(rptr)
        rptr=rptr-1
        k=idnint(a)
        GOTO 5100
 5101  CONTINUE
        k=1
 5100  CONTINUE
       i5=isto(ndx2)
       i2=isto(base5+i5)-isto(base4+i5)+k
       j=1
 3071  IF(.NOT.(j.LE.cnt2
     1 .AND. isto(ptr3+j).NE.i2
     2 )) GOTO 3070
        j=j+1
        GOTO 3071
 3070  CONTINUE
       IF(.NOT.(j.GT.cnt2)) GOTO 5111
        cnt2=cnt2+1
        isto(ptr3+j)=i2
        GOTO 5110
 5111  CONTINUE
 5110  CONTINUE
       isto(ptr4+ndx3)=j
       GOTO 3061
 3060 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      ndx2=isto(sptr+1)
      ndx1=isto(sptr+2)
      sptr=sptr+3
      GOTO 9500
 9004 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx
                 ndx=isto(env+1)
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+2)
      IF(.NOT.(ndx.EQ.2)) GOTO 5121
       sptr=sptr-1
       isto(sptr)=from
       from=9512
       GOTO 9005
 9512  from=isto(sptr)
       sptr=sptr+1
       GOTO 5120
 5121 IF(.NOT.(ndx.EQ.4)) GOTO 5122
       sptr=sptr-1
       isto(sptr)=from
       from=9513
       GOTO 9006
 9513  from=isto(sptr)
       sptr=sptr+1
       GOTO 5120
 5122 IF(.NOT.(ndx.EQ.5)) GOTO 5123
       sptr=sptr-1
       isto(sptr)=from
       from=9514
       GOTO 9007
 9514  from=isto(sptr)
       sptr=sptr+1
       GOTO 5120
 5123 IF(.NOT.(ndx.EQ.3 .OR. ndx.EQ.7)) GOTO 5124
       sptr=sptr-1
       isto(sptr)=from
       from=9515
       GOTO 9008
 9515  from=isto(sptr)
       sptr=sptr+1
       GOTO 5120
 5124 CONTINUE
 5120 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      i2=ptr2 +(ndx0-1)*cnt2
      rsto(i2+ndx0)=rsto(i2+ndx0)+one
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+0)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9003
      CALL use2
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9005 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=ndx7
      ndx7=ptr2 +(ndx0-1)*cnt2
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+0)
 3081 IF(.NOT.(env.GT.0)) GOTO 3080
       xprndx=isto(env+3)
       IF(.NOT.(xprndx.GT.0)) GOTO 5131
        to=9000
        CALL xpr1
        GOTO 5130
 5131  CONTINUE
        a=one
        rptr=rptr+1
        rsto(rptr)=a
 5130  CONTINUE
       IF(.NOT.(isto(env+1).EQ.2)) GOTO 5141
        rsto(rptr)=-rsto(rptr)
        GOTO 5140
 5141  CONTINUE
 5140  CONTINUE
       i3=isto(env+4)
       IF(.NOT.(i3.GT.0)) GOTO 5151
        sptr=sptr-1
        isto(sptr)=env
                   env=i3
        ndx1=isto(env+8)
        ndx2=ndx1
        ndx5=isto(env+2)
        to=9020
        CALL xpr1
        to=9037
        CALL xpr2
        env=isto(sptr)
        sptr=sptr+1
        i1=isto(env+2)
        i2=isto(ptr4+i1)
        a=rsto(rptr)
        rptr=rptr-1
        b=rsto(ndx7+i2)
        rsto(ndx7+i2)=b-a
        GOTO 5150
 5151  CONTINUE
        a=rsto(rptr)
        rptr=rptr-1
        b=rsto(ptr1+ndx0)
        rsto(ptr1+ndx0)=b+a
 5150  CONTINUE
       env=isto(env+0)
       GOTO 3081
 3080 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx7=isto(sptr)
      ndx6=isto(sptr+1)
      ndx5=isto(sptr+2)
      ndx4=isto(sptr+3)
      ndx2=isto(sptr+4)
      ndx1=isto(sptr+5)
      sptr=sptr+6
      GOTO 9500
 9006 CONTINUE
      sptr=sptr-1
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      isto(sptr)=ndx
                 ndx=isto(env+1)
      sptr=sptr-1
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      isto(sptr)=ptr2
                 ptr2=isto(env+3)
      sptr=sptr-1
      isto(sptr)=ptr3
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+4)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9006
      CALL use2
      env=isto(sptr)
      sptr=sptr+1
      to=9024
      CALL xpr1
      ndx6=isto(sptr)
      ndx5=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      sptr=sptr+5
      ptr3=isto(sptr)
      ptr2=isto(sptr+1)
      ptr1=isto(sptr+2)
      ndx=isto(sptr+3)
      flg=isto(sptr+4)
      sptr=sptr+5
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      c=rsto(ptr1+ndx0) +a
      rsto(ptr1+ndx0)=c
      i2=ptr2 +(ndx0-1)*cnt2
      i1=isto(env+2)
      i3=isto(ptr4+i1)
      d=rsto(i2+i3) -b
      rsto(i2+i3)=d
      GOTO 9500
 9007 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      isto(sptr)=ptr2
      sptr=sptr-1
      isto(sptr)=ptr3
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+6)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9006
      CALL use2
      to=9008
      CALL use2
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(isto(ptr2+2).GT.0)) GOTO 5161
       sptr=sptr-1
       isto(sptr)=env
                  env=useenv
       env=isto(sptr)
       sptr=sptr+1
       i2=isto(ptr2)
       a=rsto(i2)
       b=zero
       rptr=rptr+1
       rsto(rptr)=b
       rptr=rptr+1
       rsto(rptr)=a
       GOTO 5160
 5161 CONTINUE
       isto(ptr2+2)=0
       to=9028
       CALL xpr1
 5160 CONTINUE
      ndx6=isto(sptr)
      ndx5=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      ptr3=isto(sptr+5)
      ptr2=isto(sptr+6)
      ptr1=isto(sptr+7)
      sptr=sptr+8
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      c=rsto(ptr1+ndx0) +a
      rsto(ptr1+ndx0)=c
      i2=ptr2 +(ndx0-1)*cnt2
      i1=isto(env+4)
      i3=isto(ptr4+i1)
      d=rsto(i2+i3) -b
      rsto(i2+i3)=d
      GOTO 9500
 9008 CONTINUE
      sptr=sptr-1
      isto(sptr)=env
      IF(.NOT.(isto(env+3).GT.0)) GOTO 5271
       env=env+6
       GOTO 5270
 5271 CONTINUE
       env=env+18
 5270 CONTINUE
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+15)
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+1)
      sptr=sptr-1
      isto(sptr)=flg
                 flg=1
      sptr=sptr-1
      isto(sptr)=ndx
      IF(.NOT.(ndx.EQ.3)) GOTO 5171
       ndx=1
       GOTO 5170
 5171 CONTINUE
       ndx=2
 5170 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      isto(sptr)=ptr2
                 ptr2=0
      sptr=sptr-1
      isto(sptr)=ptr3
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+2)
      ndx1=isto(env+9)
      ndx2=ndx1
      IF(.NOT.(ndx1.GT.0)) GOTO 5181
       xprndx=ndx1
       to=9000
       CALL xpr1
       IF(.NOT.(xprcnt.GT.1)) GOTO 5191
        stpflg=90
        stpi1=xprcnt
        CONTINUE
        CALL ERRSTP
        GOTO 5190
 5191  CONTINUE
 5190  CONTINUE
       a=rsto(rptr)
       rptr=rptr-1
       ndx4=idnint(a)
       GOTO 5180
 5181 CONTINUE
       ndx4=1
 5180 CONTINUE
      ndx6=ndx4
      ndx5=isto(env+1)
      i1=isto(env+3)
      k=isto(env+2)
      sptr=sptr-1
      isto(sptr)=env
                 env=useenv
      IF(.NOT.(k.EQ.6)) GOTO 5281
       i0=isto(env+29) +4*(i1-1)
       GOTO 5280
 5281 CONTINUE
       i0=isto(env+36) +3*(i1-1)
 5280 CONTINUE
      i2=isto(i0)
      IF(.NOT.(i2.GT.0)) GOTO 5291
       k=ndx4-isto(base4+ndx5)
       ptr3=i2+7*k
       GOTO 5290
 5291 CONTINUE
       ptr3=0
 5290 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      to=9024
      CALL xpr1
      i1=isto(env+8)
      i3=isto(ptr4+i1)
      ndx6=isto(sptr)
      ndx5=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      sptr=sptr+5
      ptr3=isto(sptr)
      ptr2=isto(sptr+1)
      ptr1=isto(sptr+2)
      ndx=isto(sptr+3)
      flg=isto(sptr+4)
      sptr=sptr+5
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      c=rsto(ptr1+ndx0) +a
      rsto(ptr1+ndx0)=c
      i2=ptr2 +(ndx0-1)*cnt2
      d=rsto(i2+i3) -b
      rsto(i2+i3)=d
      GOTO 9500
 9009 CONTINUE
      sptr=sptr-1
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      isto(sptr)=ndx
                 ndx=isto(env+2)
      sptr=sptr-1
      isto(sptr)=ptr1
      sptr=sptr-1
      isto(sptr)=ptr2
                 ptr2=isto(env+1)
      sptr=sptr-1
      isto(sptr)=ptr3
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+4)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9006
      CALL use2
      isto(ptr3+4)=0
      env=isto(sptr)
      sptr=sptr+1
      ptr1=rptr
      to=9024
      CALL xpr1
      ndx6=isto(sptr)
      ndx5=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      sptr=sptr+5
      ptr3=isto(sptr)
      ptr2=isto(sptr+1)
      ptr1=isto(sptr+2)
      ndx=isto(sptr+3)
      flg=isto(sptr+4)
      sptr=sptr+5
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      c=rsto(ptr1+ndx0) -a
      rsto(ptr1+ndx0)=c
      i2=ptr2 +(ndx0-1)*cnt2
      rsto(i2+ndx0)=rsto(i2+ndx0)+b
      i1=isto(env+3)
      i3=isto(ptr4+i1)
      rsto(i2+i3)=rsto(i2+i3)-1
      GOTO 9500
 9010 CONTINUE
      sptr=sptr-1
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      isto(sptr)=ndx
                 ndx=isto(env+1)
      sptr=sptr-1
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      isto(sptr)=ptr2
                 ptr2=isto(env+8)
      sptr=sptr-1
      isto(sptr)=ptr3
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+5)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9006
      CALL use2
      env=isto(sptr)
      sptr=sptr+1
      to=9024
      CALL xpr1
      ndx=isto(env+2)
      ptr1=rptr
      ptr2=isto(env+9)
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+4)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9006
      CALL use2
      isto(ptr3+4)=0
      env=isto(sptr)
      sptr=sptr+1
      to=9024
      CALL xpr1
      ndx6=isto(sptr)
      ndx5=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      sptr=sptr+5
      ptr3=isto(sptr)
      ptr2=isto(sptr+1)
      ptr1=isto(sptr+2)
      ndx=isto(sptr+3)
      flg=isto(sptr+4)
      sptr=sptr+5
      i2=ptr2 +(ndx0-1)*cnt2
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      c=rsto(ptr1+ndx0) -a
      rsto(ptr1+ndx0)=c
      rsto(i2+ndx0)=b
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      c=rsto(ptr1+ndx0) +a
      rsto(ptr1+ndx0)=c
      i1=isto(env+3)
      i3=isto(ptr4+i1)
      d=rsto(i2+i3) -b
      rsto(i2+i3)=d
      GOTO 9500
 9011 CONTINUE
      IF(iptr+cnt1+1.GE.sptr) GOTO 910
      IF(rptr+cnt2.GE.rsptr) GOTO 930
      i=0
 3191 IF(.NOT.(i.LT.cnt1)) GOTO 3190
       i=i+1
       isto(iptr+i)=isto(ptr5+i)
       GOTO 3191
 3190 CONTINUE
      i6=0
      i7=0
      i8=0
      i9=0
      sptr=sptr-1
      isto(sptr)=env
                 env=ptr0
      i10=isto(env+6)
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      isto(sptr)=lpflg
 3201 IF(.NOT.(i7.LT.cnt1
     1.AND. flg.EQ.0)) GOTO 3200
       lpflg=1
 3211  IF(.NOT.(lpflg.GT.0)) GOTO 3210
        i8=i8+1
        IF(.NOT.(i8.EQ.cnt1 .AND. cnt1.GT.1
     1  .AND. i7.LT.cnt1-1 .AND. i10.GT.0
     2  )) GOTO 5361
         i8=i8+1
         GOTO 5360
 5361   CONTINUE
 5360   CONTINUE
        IF(.NOT.(i8.GT.cnt1)) GOTO 5301
         i8=1
         i9=i6
         GOTO 5300
 5301   CONTINUE
 5300   CONTINUE
        i0=isto(iptr+i8)
        IF(.NOT.(i0.GT.0)) GOTO 5311
         lpflg=0
         GOTO 5310
 5311   CONTINUE
 5310   CONTINUE
        GOTO 3211
 3210  CONTINUE
       i1=ptr2+(i0-1)*cnt2
       i=0
 3221  IF(.NOT.(i.LT.cnt2)) GOTO 3220
        i=i+1
        rsto(rptr+i)=rsto(i1+i)
        GOTO 3221
 3220  CONTINUE
       k1=0
 3231  IF(.NOT.(k1.LT.i7)) GOTO 3230
        k1=k1+1
        k=isto(ptr5+k1)
        i2=i1+k1
        a=rsto(i2)
        IF(.NOT.(dabs(a).GT.rmargn)) GOTO 5321
         i3=ptr2+(k-1)*cnt2
         j=k1
 3241    IF(.NOT.(j.LT.cnt2)) GOTO 3240
          j=j+1
          i4=i1+j
          rsto(i4)=rsto(i4)-a*rsto(i3+j)
          GOTO 3241
 3240    CONTINUE
         GOTO 5320
 5321   CONTINUE
 5320   CONTINUE
        GOTO 3231
 3230  CONTINUE
       i2=i1+i7+1
       a=rsto(i2)
       IF(.NOT.(dabs(a).GT.rmargn)) GOTO 5331
        a=one/a
        rsto(i2)=a
        j=i7+1
 3251   IF(.NOT.(j.LT.cnt2)) GOTO 3250
         j=j+1
         i3=i1+j
         rsto(i3)=rsto(i3)*a
         GOTO 3251
 3250   CONTINUE
        i7=i7+1
        isto(ptr5+i7)=i0
        isto(iptr+i8)=0
        GOTO 5330
 5331  CONTINUE
        i6=i0
        IF(.NOT.(i6.EQ.i9)) GOTO 5341
         flg=i6
         GOTO 5340
 5341   CONTINUE
 5340   CONTINUE
        i=0
 3261   IF(.NOT.(i.LT.cnt2)) GOTO 3260
         i=i+1
         rsto(i1+i)=rsto(rptr+i)
         GOTO 3261
 3260   CONTINUE
 5330  CONTINUE
       GOTO 3201
 3200 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9012 CONTINUE
      i7=0
 3131 IF(.NOT.(i7.LT.cnt1)) GOTO 3130
       i7=i7+1
       i=isto(ptr5+i7)
       i1=ptr1+i
       a=rsto(i1)
       i2=ptr2+(i-1)*cnt2
       k1=0
 3141  IF(.NOT.(k1.LT.i7-1)) GOTO 3140
        k1=k1+1
        k=isto(ptr5+k1)
        a=a -rsto(i2+k1)*rsto(ptr1+k)
        GOTO 3141
 3140  CONTINUE
       rsto(i1)=a*rsto(i2+i7)
       GOTO 3131
 3130 CONTINUE
      GOTO 9500
 9013 CONTINUE
      i=isto(ptr5+cnt1)
      a=rsto(ptr1+i)
      i1=ptr2 +(i-1)*cnt2
      k=cnt1
 3151 IF(.NOT.(k.LT.cnt2)) GOTO 3150
       k=k+1
       i2=isto(ptr3+k)
       b=rsto(base3+i2)
       a=a -rsto(i1+k)*b
       GOTO 3151
 3150 CONTINUE
      i2=isto(ptr3+cnt1)
      rsto(base3+i2)=a
      GOTO 9500
 9014 CONTINUE
      i7=cnt1
 3161 IF(.NOT.(i7.GT.1)) GOTO 3160
       i7=i7-1
       i=isto(ptr5+i7)
       a=rsto(ptr1+i)
       i1=ptr2+(i-1)*cnt2
       k1=i7
 3171  IF(.NOT.(k1.LT.cnt2)) GOTO 3170
        k1=k1+1
        i2=isto(ptr3+k1)
        b=rsto(base3+i2)
        a=a -rsto(i1+k1)*b
        GOTO 3171
 3170  CONTINUE
       i2=isto(ptr3+i7)
       rsto(base3+i2)=a
       GOTO 3161
 3160 CONTINUE
      GOTO 9500
 9015 CONTINUE
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+5)
      k=isto(env+4)
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+5)
      IF(.NOT.(k.EQ.1)) GOTO 5231
       sptr=sptr-1
       isto(sptr)=from
       from=9520
       GOTO 9016
 9520  from=isto(sptr)
       sptr=sptr+1
       GOTO 5230
 5231 IF(.NOT.(k.EQ.2)) GOTO 5232
       sptr=sptr-1
       isto(sptr)=from
       from=9521
       GOTO 9021
 9521  from=isto(sptr)
       sptr=sptr+1
       GOTO 5230
 5232 IF(.NOT.(k.EQ.3)) GOTO 5233
       sptr=sptr-1
       isto(sptr)=from
       from=9522
       GOTO 9022
 9522  from=isto(sptr)
       sptr=sptr+1
       GOTO 5230
 5233 CONTINUE
 5230 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9016 CONTINUE
      k=isto(env+1)
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+2)
      IF(.NOT.(k.EQ.2)) GOTO 5241
       sptr=sptr-1
       isto(sptr)=from
       from=9523
       GOTO 9017
 9523  from=isto(sptr)
       sptr=sptr+1
       GOTO 5240
 5241 IF(.NOT.(k.EQ.4)) GOTO 5242
       sptr=sptr-1
       isto(sptr)=from
       from=9524
       GOTO 9018
 9524  from=isto(sptr)
       sptr=sptr+1
       GOTO 5240
 5242 IF(.NOT.(k.EQ.5)) GOTO 5243
       sptr=sptr-1
       isto(sptr)=from
       from=9525
       GOTO 9019
 9525  from=isto(sptr)
       sptr=sptr+1
       GOTO 5240
 5243 IF(.NOT.(k.EQ.3 .OR. k.EQ.7)) GOTO 5244
       sptr=sptr-1
       isto(sptr)=from
       from=9526
       GOTO 9020
 9526  from=isto(sptr)
       sptr=sptr+1
       GOTO 5240
 5244 CONTINUE
 5240 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9017 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx0
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      ndx0=isto(ptr5+cnt1)
      ndx1=isto(env+1)
      ndx2=isto(env+2)
      i2=isto(ptr3+ndx0)
      a=rsto(base3+i2)
      rptr=rptr+1
      rsto(rptr)=a
      to=9021
      CALL xpr1
      a=rsto(rptr)
      rptr=rptr-1
      i2=isto(ptr3+ndx0)
      rsto(base3+i2)=a
      ndx2=isto(sptr)
      ndx1=isto(sptr+1)
      ndx0=isto(sptr+2)
      sptr=sptr+3
      GOTO 9500
 9018 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx0
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      ndx0=isto(ptr5+cnt1)
      ndx1=isto(env+5)
      ndx2=isto(env+6)
      i2=isto(ptr3+ndx0)
      a=rsto(base3+i2)
      rptr=rptr+1
      rsto(rptr)=a
      to=9021
      CALL xpr1
      a=rsto(rptr)
      rptr=rptr-1
      i2=isto(ptr3+ndx0)
      rsto(base3+i2)=a
      ndx2=isto(sptr)
      ndx1=isto(sptr+1)
      ndx0=isto(sptr+2)
      sptr=sptr+3
      GOTO 9500
 9019 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr2
      sptr=sptr-1
      isto(sptr)=ndx0
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+6)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9008
      CALL use2
      env=isto(sptr)
      sptr=sptr+1
      i1=isto(ptr2+2)
      IF(.NOT.(i1.GT.1)) GOTO 5251
       isto(ptr2+2)=i1-2
       GOTO 5250
 5251 CONTINUE
 5250 CONTINUE
      ndx0=isto(ptr5+cnt1)
      ndx1=isto(env+7)
      ndx2=isto(env+8)
      i2=isto(ptr3+ndx0)
      a=rsto(base3+i2)
      rptr=rptr+1
      rsto(rptr)=a
      to=9023
      CALL xpr1
      a=rsto(rptr)
      rptr=rptr-1
      i2=isto(ptr3+ndx0)
      rsto(base3+i2)=a
      ndx6=isto(sptr)
      ndx5=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      ndx0=isto(sptr+5)
      ptr2=isto(sptr+6)
      sptr=sptr+7
      GOTO 9500
 9020 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx0
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=env
                 env=env+8
      ndx0=isto(ptr5+cnt1)
      ndx1=isto(env+1)
      ndx2=isto(env+2)
      env=isto(sptr)
      sptr=sptr+1
      i2=isto(ptr3+ndx0)
      a=rsto(base3+i2)
      rptr=rptr+1
      rsto(rptr)=a
      to=9021
      CALL xpr1
      a=rsto(rptr)
      rptr=rptr-1
      i2=isto(ptr3+ndx0)
      rsto(base3+i2)=a
      ndx2=isto(sptr)
      ndx1=isto(sptr+1)
      ndx0=isto(sptr+2)
      sptr=sptr+3
      GOTO 9500
 9021 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr4
                 ptr4=ptr3
      sptr=sptr-1
      isto(sptr)=ptr3
      sptr=sptr-1
      isto(sptr)=ndx0
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+4)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9006
      CALL use2
      env=isto(sptr)
      sptr=sptr+1
      ndx0=isto(ptr5+cnt1)
      ndx1=isto(env+6)
      ndx2=isto(env+7)
      i2=isto(ptr4+ndx0)
      a=rsto(base3+i2)
      rptr=rptr+1
      rsto(rptr)=a
      to=9022
      CALL xpr1
      a=rsto(rptr)
      rptr=rptr-1
      i2=isto(ptr4+ndx0)
      rsto(base3+i2)=a
      ndx6=isto(sptr)
      ndx5=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      ndx0=isto(sptr+5)
      ptr3=isto(sptr+6)
      ptr4=isto(sptr+7)
      sptr=sptr+8
      GOTO 9500
 9022 CONTINUE
      sptr=sptr-1
      isto(sptr)=ptr4
                 ptr4=ptr3
      sptr=sptr-1
      isto(sptr)=ptr3
      sptr=sptr-1
      isto(sptr)=ndx0
      sptr=sptr-1
      isto(sptr)=ndx1
      sptr=sptr-1
      isto(sptr)=ndx2
      sptr=sptr-1
      isto(sptr)=ndx4
      sptr=sptr-1
      isto(sptr)=ndx5
      sptr=sptr-1
      isto(sptr)=ndx6
      sptr=sptr-1
      isto(sptr)=env
                 env=isto(env+4)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      to=9006
      CALL use2
      env=isto(sptr)
      sptr=sptr+1
      ndx0=isto(ptr5+cnt1)
      ndx1=isto(env+6)
      ndx2=isto(env+7)
      i2=isto(ptr4+ndx0)
      a=rsto(base3+i2)
      rptr=rptr+1
      rsto(rptr)=a
      to=9022
      CALL xpr1
      a=rsto(rptr)
      rptr=rptr-1
      i2=isto(ptr4+ndx0)
      rsto(base3+i2)=a
      ndx6=isto(sptr)
      ndx5=isto(sptr+1)
      ndx4=isto(sptr+2)
      ndx2=isto(sptr+3)
      ndx1=isto(sptr+4)
      ndx0=isto(sptr+5)
      ptr3=isto(sptr+6)
      ptr4=isto(sptr+7)
      sptr=sptr+8
      GOTO 9500
      END
