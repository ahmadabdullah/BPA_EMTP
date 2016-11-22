      SUBROUTINE use2
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010,
     1 9011), to-8999
      STOP 'Invalid "to" reference in "use2".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501,  9502,  9503,  9504,  9505,
     1  9506,  9507,  9508,  9509,  9510,
     1  0002,  0002,  0002,  9514,  9515,
     1  9516), from- 9500
 0002 STOP 'Invalid "from" reference in "use2".'
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
      isto(sptr)=ndx1
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+2)
      ndx1=isto(env+8)
      ndx2=isto(env+9)
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9501
      GOTO 9008
 9501 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      xprndx=isto(env+3)
      to=9000
      CALL xpr1
      rptr=rptr-xprcnt
      k=ndx6-ndx4+1
      IF(.NOT.(k.LT.1)) GOTO 5001
       stpflg=29
       GOTO 5000
 5001 IF(.NOT.(k.NE.xprcnt
     1.AND. xprcnt.GT.1)) GOTO 5002
       stpflg=41
       GOTO 5000
 5002 CONTINUE
 5000 CONTINUE
      IF(.NOT.(stpflg.GT.0)) GOTO 5011
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=datenv
       i0=isto(env+1)+2*(ndx5-1)
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
      a=rsto(rptr+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      i1=ptr2-4
      i2=ndx6-ndx4+1
      i=0
 3001 IF(.NOT.(i.LT.i2)) GOTO 3000
       i=i+1
       i1=i1+4
       isto(i1+2)=1
       i3=isto(i1)
       IF(.NOT.(xprcnt.EQ.1)) GOTO 5021
        rsto(i3)=a
        GOTO 5020
 5021  CONTINUE
        rsto(i3)=rsto(rptr+i)
 5020  CONTINUE
       GOTO 3001
 3000 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9001 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=isto(env+4)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+3)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      env=isto(sptr)
      sptr=sptr+1
      k=isto(env+2)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+3)
      IF(.NOT.(k.GT.0)) GOTO 5031
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9502
       GOTO 9008
 9502  from=isto(sptr)
       sptr=sptr+1
       isto(ptr2+1)=ptr4
       isto(ptr2+3)=1
       GOTO 5030
 5031 CONTINUE
       i1=isto(env+4)
       k=isto(env+3)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=useenv
       IF(.NOT.(k.EQ.6)) GOTO 5041
        i0=isto(env+29) +4*(i1-1)
        GOTO 5040
 5041  CONTINUE
        i0=isto(env+36) +3*(i1-1)
 5040  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       i2=isto(i0)
       k=ndx6-ndx4+1
       i3=ndx4-isto(base4+ndx5)
       ptr3=i2 +7*(i3-1)
       i=0
 3011  IF(.NOT.(i.LT.k)) GOTO 3010
        i=i+1
        ptr3=ptr3+7
        isto(ptr3+3)=ptr4
        isto(ptr3+6)=i
        GOTO 3011
 3010  CONTINUE
 5030 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
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
      i=isto(env+1)
      k=isto(env+2)
      IF(.NOT.(i.EQ.1 .OR. i.EQ.3 .OR. i.EQ.7
     1)) GOTO 5051
       xprndx=k
       to=9000
       CALL xpr1
       ndx3=xprcnt
       GOTO 5050
 5051 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=k
       IF(.NOT.(i.EQ.2)) GOTO 5061
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9503
        GOTO 9004
 9503   from=isto(sptr)
        sptr=sptr+1
        GOTO 5060
 5061  IF(.NOT.(i.EQ.4)) GOTO 5062
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9504
        GOTO 9005
 9504   from=isto(sptr)
        sptr=sptr+1
        GOTO 5060
 5062  IF(.NOT.(i.EQ.5)) GOTO 5063
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9505
        GOTO 9007
 9505   from=isto(sptr)
        sptr=sptr+1
        GOTO 5060
 5063  CONTINUE
 5060  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       ndx3=1
 5050 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+0)
      ndx1=isto(env+8)
      ndx2=isto(env+9)
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9506
      GOTO 9003
 9506 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      xprcnt=ndx3
      to=9000
      CALL xpr2
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9507
      GOTO 9006
 9507 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ptr3.GT.0)) GOTO 5071
       i3=ptr3-7
       k=ndx6-ndx4+1
       i=0
 3021  IF(.NOT.(i.LT.k)) GOTO 3020
        i=i+1
        i3=i3+7
        isto(i3+4)=0
        GOTO 3021
 3020  CONTINUE
       GOTO 5070
 5071 CONTINUE
 5070 CONTINUE
      ptr3=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9004 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
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
      a=zero
      rptr=rptr+1
      IF(rptr.EQ.rsptr) GOTO 930
      rsto(rptr)=a
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+0)
 3031 IF(.NOT.(env.GT.0)) GOTO 3030
       xprndx=isto(env+3)
       IF(.NOT.(xprndx.GT.0)) GOTO 5081
        to=9000
        CALL xpr1
        GOTO 5080
 5081  CONTINUE
        a=one
        rptr=rptr+1
        IF(rptr.EQ.rsptr) GOTO 930
        rsto(rptr)=a
 5080  CONTINUE
       IF(.NOT.(isto(env+1).EQ.2)) GOTO 5091
        rsto(rptr)=-rsto(rptr)
        GOTO 5090
 5091  CONTINUE
 5090  CONTINUE
       i1=isto(env+4)
       IF(.NOT.(i1.GT.0)) GOTO 5101
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=i1
        ndx1=isto(env+8)
        ndx2=ndx1
        ndx5=isto(env+2)
        to=9020
        CALL xpr1
        to=9037
        CALL xpr2
        env=isto(sptr)
        sptr=sptr+1
        a=rsto(rptr)
        rptr=rptr-1
        k=base3 +isto(base5+ndx5)
     1    -isto(base4+ndx5) +ndx4
        rsto(rptr)=rsto(rptr) +a*rsto(k)
        GOTO 5100
 5101  CONTINUE
        a=rsto(rptr)
        rptr=rptr-1
        rsto(rptr)=rsto(rptr) +a
 5100  CONTINUE
       env=isto(env+0)
       GOTO 3031
 3030 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx1=isto(env+1)
      ndx2=isto(env+2)
      to=9021
      CALL xpr1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9005 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                 ptr2=isto(env+3)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+4)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9508
      GOTO 9006
 9508 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      to=9024
      CALL xpr1
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      i0=base3 +isto(base5+ndx5)
     1   -isto(base4+ndx5) +ndx4
      c=rsto(i0)
      d=b*c +a
      rptr=rptr+1
      IF(rptr.EQ.rsptr) GOTO 930
      rsto(rptr)=d
      ndx1=isto(env+5)
      ndx2=isto(env+6)
      to=9021
      CALL xpr1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9006 CONTINUE
      i1=isto(env+4)
      k=isto(env+3)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      IF(.NOT.(k.EQ.6)) GOTO 5111
       i0=isto(env+29) +4*(i1-1)
       GOTO 5110
 5111 CONTINUE
       i0=isto(env+36) +3*(i1-1)
 5110 CONTINUE
      i2=isto(i0)
      IF(.NOT.(i2.GT.0)) GOTO 5121
       k=ndx4-isto(base4+ndx5)
       ptr3=i2+7*k
       GOTO 5120
 5121 CONTINUE
       ptr3=0
 5120 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9007 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+6)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9509
      GOTO 9006
 9509 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9510
      GOTO 9008
 9510 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      i1=isto(ptr2+2)
      IF(.NOT.(i1.GT.1)) GOTO 5131
       i1=i1-2
       isto(ptr2+2)=i1
       GOTO 5130
 5131 CONTINUE
 5130 CONTINUE
      IF(.NOT.(i1.EQ.1)) GOTO 5141
       i2=isto(ptr2)
       a=rsto(i2)
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=a
       GOTO 5140
 5141 CONTINUE
       to=9028
       CALL xpr1
       a=rsto(rptr)
       rptr=rptr-1
       b=rsto(rptr)
       rptr=rptr-1
       i0=base3 +isto(base5+ndx5)
     1    -isto(base4+ndx5) +ndx4
       c=rsto(i0)
       d=b*c +a
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=d
 5140 CONTINUE
      ndx1=isto(env+7)
      ndx2=isto(env+8)
      to=9023
      CALL xpr1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9008 CONTINUE
      i1=isto(env+4)
      k=isto(env+3)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      IF(.NOT.(k.EQ.6)) GOTO 5151
       i0=isto(env+29) +4*(i1-1)
       GOTO 5150
 5151 CONTINUE
       i0=isto(env+36) +3*(i1-1)
 5150 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      i2=isto(i0+2)
      k=ndx4 -isto(base4+ndx5)
      ptr2=i2 +4*k
      GOTO 9500
 9009 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=isto(env+2)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                 ptr2=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx7
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      ndx7=isto(env+1)
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx7.EQ.2)) GOTO 5261
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+5)
       stpl1=isto(env+6)
       stpc1=isto(env+7)
       ndx1=isto(env+8)
       ndx2=ndx1
       ndx5=isto(env+2)
       to=9020
       CALL xpr1
       to=9037
       CALL xpr2
       env=isto(sptr)
       sptr=sptr+1
       k=isto(base5+ndx5)-isto(base4+ndx5)+ndx4
       a=rsto(base3+k)
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=a
       GOTO 5260
 5261 CONTINUE
 5260 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+4)
      stpl1=isto(env+6)
      stpc1=isto(env+7)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9514
      GOTO 9006
 9514 from=isto(sptr)
      sptr=sptr+1
      isto(ptr3+4)=0
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx7.EQ.2)) GOTO 5271
       ptr1=rptr
       to=9024
       CALL xpr1
       b=rsto(ptr1+1)
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=b
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=flg
       to=9011
       CALL xpr2
       IF(.NOT.(flg.GT.0)) GOTO 5191
        stpflg=107
        stpi1=ndx4
        CONTINUE
        CALL errstp
        GOTO 5190
 5191  CONTINUE
 5190  CONTINUE
       flg=isto(sptr)
       sptr=sptr+1
       c=rsto(rptr)
       rptr=rptr-1
       a=rsto(ptr1+2)
       e=rsto(ptr1)
       rptr=ptr1
       rsto(rptr)=(e-a)*c
       ndx1=isto(env+6)
       ndx2=isto(env+7)
       to=9022
       CALL xpr1
       GOTO 5270
 5271 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx11
                  ndx11=isto(ptr3+3)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx14
                  ndx14=isto(ptr3+6)
       stpl2=stpl1
       stpc2=stpc1
       stpl1=6
       stpc1=304
       stpi1=ndx4
       to=9014
       CALL xpr1
       ndx14=isto(sptr)
       sptr=sptr+1
       ndx11=isto(sptr)
       sptr=sptr+1
 5270 CONTINUE
      i0=base3 +isto(base5+ndx5)
     1   -isto(base4+ndx5) +ndx4
      rsto(i0)=rsto(rptr)
      rptr=rptr-1
      ndx7=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9010 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=rptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                 ptr2=isto(env+8)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx7
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      ndx7=isto(env+1)
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx7.EQ.2)) GOTO 5241
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+5)
       ndx1=isto(env+8)
       ndx2=ndx1
       ndx5=isto(env+2)
       to=9020
       CALL xpr1
       to=9037
       CALL xpr2
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9515
       GOTO 9006
 9515  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       to=9024
       CALL xpr1
       a=rsto(rptr)
       rptr=rptr-1
       b=rsto(rptr)
       rptr=rptr-1
       i0=base3 +isto(base5+ndx5)
     1    -isto(base4+ndx5) +ndx4
       c=rsto(i0)
       d=b*c +a
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=d
       ndx=isto(env+2)
       ptr1=rptr
       ptr2=isto(env+9)
       GOTO 5240
 5241 CONTINUE
 5240 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+4)
      stpl1=isto(env+6)
      stpc1=isto(env+7)
      ndx1=isto(env+8)
      ndx2=ndx1
      ndx5=isto(env+2)
      to=9020
      CALL xpr1
      to=9037
      CALL xpr2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9516
      GOTO 9006
 9516 from=isto(sptr)
      sptr=sptr+1
      isto(ptr3+4)=0
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx7.EQ.2)) GOTO 5251
       to=9024
       CALL xpr1
       b=rsto(ptr1+1)
       rptr=rptr+1
       IF(rptr.EQ.rsptr) GOTO 930
       rsto(rptr)=b
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=flg
                  flg=0
       to=9011
       CALL xpr2
       IF(.NOT.(flg.GT.0)) GOTO 5201
        stpflg=108
        stpi1=ndx4
        CONTINUE
        CALL errstp
        GOTO 5200
 5201  CONTINUE
 5200  CONTINUE
       flg=isto(sptr)
       sptr=sptr+1
       c=rsto(rptr)
       rptr=rptr-1
       a=rsto(ptr1+2)
       e=rsto(ptr1)
       rptr=ptr1
       rsto(rptr)=(e-a)*c
       ndx1=isto(env+6)
       ndx2=isto(env+7)
       to=9022
       CALL xpr1
       GOTO 5250
 5251 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx11
                  ndx11=isto(ptr3+3)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx14
                  ndx14=isto(ptr3+6)
       stpl2=stpl1
       stpc2=stpc1
       stpl1=7
       stpc1=310
       stpi1=ndx4
       to=9014
       CALL xpr1
       ndx14=isto(sptr)
       sptr=sptr+1
       ndx11=isto(sptr)
       sptr=sptr+1
 5250 CONTINUE
      i0=base3 +isto(base5+ndx5)
     1   -isto(base4+ndx5) +ndx4
      rsto(i0)=rsto(rptr)
      rptr=rptr-1
      ndx7=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9011 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt1
                 cnt1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+0)
 3041 IF(.NOT.(env.GT.0)) GOTO 3040
       i1=isto(env+1)
       IF(.NOT.(i1.GT.0)) GOTO 5211
        k=isto(env+2)-1
        i=0
 3061   IF(.NOT.(i.LT.i1)) GOTO 3060
         i=i+1
         cbuff(cnt1+i:cnt1+i)=csto(k+i)
         GOTO 3061
 3060   CONTINUE
        cnt1=cnt1+i1
        GOTO 5210
 5211  CONTINUE
        xprndx=isto(env+3)
        to=9000
        CALL xpr1
        rptr=rptr-xprcnt
        a=rsto(rptr+1)
        WRITE(cbuff(cnt1+1:cnt1+15),1001)
 1001   FORMAT('               ')
        WRITE(cbuff(cnt1+1:cnt1+15),1002) a
 1002   FORMAT(G15.8)
        k=cnt1
        i=0
 3071   IF(.NOT.(i.LT.15)) GOTO 3070
         i=i+1
         IF(.NOT.(cbuff(cnt1+i:cnt1+i).NE.csto(64)
     1   )) GOTO 5231
          k=k+1
          cbuff(k:k)=cbuff(cnt1+i:cnt1+i)
          GOTO 5230
 5231    CONTINUE
 5230    CONTINUE
         GOTO 3071
 3070   CONTINUE
        cnt1=k
 5210  CONTINUE
       env=isto(env+0)
       GOTO 3041
 3040 CONTINUE
      WRITE(UNIT06,1000) cbuff(1:cnt1)
 1000 FORMAT(1x,a)
      env=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
