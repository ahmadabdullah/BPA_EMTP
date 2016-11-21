      SUBROUTINE build1
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9000,
     1 9001), to-8999
      STOP 'Invalid "to" reference in "build".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1  9501), from- 9500
      STOP 'Invalid "from" reference in "build".'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
                 ndx5=isto(env+3)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr0
                 ptr0=env
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=useenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9501
      GOTO 9001
 9501 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9001 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt5
                 cnt5=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt7
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt8
                 cnt8=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt9
                 cnt9=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
      iptr=iptr+57
      isto(env+0)=0
      isto(env+1)=0
      isto(env+2)=ndx4
      isto(env+3)=ndx1
      isto(env+4)=ndx2
      isto(env+5)=ndx3
      to=9002
      CALL build2
      j=isto(env+6)
      k=isto(env+7)
      isto(ishenv+61)=j
      isto(ishenv+62)=k
      isto(env+8)=isto(ishenv+10)
      isto(env+9)=ndx5
      isto(env+10)=ptr0
      isto(env+55)=0
      isto(env+56)=0
      IF(.NOT.(ndx4.NE.2)) GOTO 5001
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=ndx5
       i1=isto(env+46)
       i2=isto(env+47)
       i4=isto(env+0)
       i5=isto(env+4)
       i6=isto(env+5)
       i7=isto(env+7)
       i8=isto(env+9)
       i9=isto(env+11)
       i10=isto(env+23)
       i11=isto(env+34)
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5000
 5001 CONTINUE
       i1=0
       i2=0
       i4=4
       i5=isto(env+20)
       i6=0
       i7=1
       i8=1
       i9=2
       i10=0
       i11=0
 5000 CONTINUE
      isto(env+11)=i1
      isto(env+12)=i2
      isto(env+13)=0
      isto(env+14)=0
      isto(env+15)=i4
      isto(env+20)=i5
      isto(env+22)=i6
      isto(env+25)=i7
      isto(env+28)=i8
      isto(env+35)=i9
      isto(env+18)=i10
      isto(env+19)=i11
      isto(env+52)=iptr+1
      i=0
 3001 IF(.NOT.(i.LT.i4)) GOTO 3000
       i=i+1
       isto(iptr+i)=i
       GOTO 3001
 3000 CONTINUE
      iptr=iptr+i4
      isto(env+53)=iptr+1
      i=0
 3011 IF(.NOT.(i.LT.i4)) GOTO 3010
       i=i+1
       isto(iptr+i)=1
       GOTO 3011
 3010 CONTINUE
      iptr=iptr+i4
      isto(env+54)=iptr+1
      i=0
 3021 IF(.NOT.(i.LT.i5)) GOTO 3020
       i=i+1
       isto(iptr+i)=1
       GOTO 3021
 3020 CONTINUE
      k=i4-i10-i11
 3031 IF(.NOT.(i.LT.k)) GOTO 3030
       i=i+1
       isto(iptr+i)=-iinf
       GOTO 3031
 3030 CONTINUE
 3041 IF(.NOT.(i.LT.i4)) GOTO 3040
       i=i+1
       isto(iptr+i)=1
       GOTO 3041
 3040 CONTINUE
      iptr=iptr+i4
      isto(env+21)=rptr+1
      i=0
 3051 IF(.NOT.(i.LT.i4)) GOTO 3050
       i=i+1
       rsto(rptr+i)=rnull
       GOTO 3051
 3050 CONTINUE
      rptr=rptr+i4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base3
                 base3=isto(env+21)-1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base5
                 base5=isto(env+52)-1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base4
                 base4=isto(env+53)-1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=base6
                 base6=isto(env+54)-1
      cnt1=i4
      ndx0=i4
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
      isto(env+24)=isto(env+21)+isto(env+20)
      cnt2=isto(env+22)
      IF(.NOT.(isto(env+2).NE.2
     1.AND. cnt2.GT.0)) GOTO 5011
       to=9004
       CALL build2
       GOTO 5010
 5011 CONTINUE
       isto(env+24)=0
 5010 CONTINUE
      isto(env+23)=cnt2
      isto(env+27)=isto(env+21)
     1 +isto(env+20)+cnt2
      cnt2=isto(env+25)
      IF(.NOT.(isto(env+2).NE.2
     1.AND. cnt2.GT.0)) GOTO 5021
       to=9005
       CALL build2
       GOTO 5020
 5021 IF(.NOT.(isto(env+2).EQ.2)) GOTO 5022
       to=9008
       CALL build2
       GOTO 5020
 5022 CONTINUE
       isto(env+27)=0
 5020 CONTINUE
      isto(env+26)=cnt2
      IF(.NOT.(cnt2.GT.0)) GOTO 5671
       to=9011
       CALL build2
       to=9012
       CALL build2
       GOTO 5670
 5671 CONTINUE
 5670 CONTINUE
      isto(env+31)=isto(env+21)
     1 +isto(env+20)+isto(env+23)+cnt2
      cnt2=isto(env+28)
      IF(.NOT.(isto(env+2).NE.2
     1.AND. cnt2.GT.0)) GOTO 5031
       to=9006
       CALL build2
       GOTO 5030
 5031 IF(.NOT.(isto(env+2).EQ.2)) GOTO 5032
       to=9009
       CALL build2
       GOTO 5030
 5032 CONTINUE
       isto(env+31)=0
 5030 CONTINUE
      isto(env+30)=cnt2
      isto(env+38)=isto(env+21)
     1 +isto(env+20)+isto(env+23)
     2 +isto(env+26)+cnt2
      cnt2=isto(env+35)
      IF(.NOT.(isto(env+2).NE.2
     1.AND. cnt2.GT.0)) GOTO 5041
       to=9007
       CALL build2
       GOTO 5040
 5041 IF(.NOT.(isto(env+2).EQ.2)) GOTO 5042
       to=9010
       CALL build2
       GOTO 5040
 5042 CONTINUE
       isto(env+38)=0
 5040 CONTINUE
      isto(env+37)=cnt2
      isto(env+16)=cnt1
      to=9014
      CALL xref2
      isto(env+29)=iptr+1
      iptr=iptr+4*isto(env+28)
      isto(env+36)=iptr+1
      iptr=iptr+3*isto(env+35)
      isto(env+32)=iptr+1
      k=isto(env+30)
      iptr=iptr+5*k
      isto(env+40)=iptr+1
      iptr=iptr+7*(k+isto(env+37))
      i0=isto(env+28)
      i1=isto(env+40)
      i2=isto(env+29)-4
      i3=isto(env+32)
      i5=isto(env+20)+isto(env+22)+isto(env+25)
      i=0
 3061 IF(.NOT.(i.LT.i0)) GOTO 3060
       i=i+1
       i2=i2+4
       isto(i2)=i1
       isto(i2+1)=0
       isto(i2+2)=0
       isto(i2+3)=i3
       i4=isto(base4+i5+i)
       i6=isto(base6+i5+i)
       k=i6-i4+1
       i1=i1+7*k
       i3=i3+5*k
       GOTO 3061
 3060 CONTINUE
      i0=isto(env+35)
      i2=isto(env+36)-3
      i5=i5+isto(env+28)
      i=0
 3071 IF(.NOT.(i.LT.i0)) GOTO 3070
       i=i+1
       i2=i2+3
       isto(i2)=i1
       isto(i2+1)=0
       isto(i2+2)=0
       i4=isto(base4+i5+i)
       i6=isto(base6+i5+i)
       k=i6-i4+1
       i1=i1+7*k
       GOTO 3071
 3070 CONTINUE
      i0=isto(env+30)
      i2=isto(env+32)-5
      i=0
 3081 IF(.NOT.(i.LT.i0)) GOTO 3080
       i=i+1
       i2=i2+5
       isto(i2)=0
       isto(i2+1)=-1
       isto(i2+2)=0
       isto(i2+3)=0
       isto(i2+4)=0
       GOTO 3081
 3080 CONTINUE
      i0=i0+isto(env+37)
      i2=isto(env+40)-7
      i=0
 3091 IF(.NOT.(i.LT.i0)) GOTO 3090
       i=i+1
       i2=i2+7
       isto(i2)=0
       isto(i2+1)=-1
       isto(i2+2)=0
       isto(i2+3)=0
       isto(i2+4)=0
       isto(i2+5)=0
       isto(i2+6)=0
       GOTO 3091
 3090 CONTINUE
      isto(env+39)=i0
      cnt6=isto(env+20)
     1 +isto(env+22)+isto(env+25)+1
      cnt7=cnt6+isto(env+28)
      IF(.NOT.(isto(env+2).NE.2)) GOTO 5051
       to=9013
       CALL build2
       GOTO 5050
 5051 CONTINUE
 5050 CONTINUE
      to=9015
      CALL build2
      to=9016
      CALL build2
      IF(.NOT.(isto(env+2).NE.2)) GOTO 5061
       to=9017
       CALL build2
       to=9019
       CALL build2
       to=9020
       CALL build2
       to=9021
       CALL build2
       to=9027
       CALL build2
       GOTO 5060
 5061 CONTINUE
 5060 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      ndx9=isto(env+18)-3
      ndx10=isto(env+17)
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(isto(env+2).NE.2
     1.AND. ndx10.GT.0)) GOTO 5071
       isto(env+44)=iptr+1
       isto(env+45)=rptr+1
       to=9022
       CALL build2
       isto(env+43)=
     1  (iptr+1-isto(env+44))/4
       GOTO 5070
 5071 CONTINUE
       isto(env+44)=0
       isto(env+45)=0
       isto(env+43)=0
 5070 CONTINUE
      isto(env+34)=rptr+1
      i1=rptr+1
      i2=isto(env+32)-5
      i3=isto(env+30)
      i=0
 3101 IF(.NOT.(i.LT.i3)) GOTO 3100
       i=i+1
       i2=i2+5
       isto(i2)=i1
       k=2*isto(i2+1)+1
       IF(.NOT.(k.LT.2)) GOTO 5081
        k=2
        GOTO 5080
 5081  CONTINUE
 5080  CONTINUE
       i1=i1+k
       GOTO 3101
 3100 CONTINUE
      isto(env+33)=i1-isto(env+34)
      isto(env+42)=i1
      i2=isto(env+40)-7
      i3=isto(env+39)
      i=0
 3111 IF(.NOT.(i.LT.i3)) GOTO 3110
       i=i+1
       i2=i2+7
       k=isto(i2+5)
       IF(.NOT.(k.GT.0)) GOTO 5661
        isto(i2)=i1
        i1=i1+k
        GOTO 5660
 5661  CONTINUE
 5660  CONTINUE
       GOTO 3111
 3110 CONTINUE
      rptr=i1-1
      isto(env+41)=i1-isto(env+42)
      isto(env+46)=0
      k=2*cnt8+1
      IF(.NOT.(k.EQ.1)) GOTO 5091
       k=2
       GOTO 5090
 5091 CONTINUE
 5090 CONTINUE
      isto(env+47)=k
      isto(env+48)=rptr+1
      rptr=rptr+k
      isto(env+49)=0
      isto(env+50)=cnt9
      isto(env+51)=rptr+1
      rptr=rptr+cnt9
      IF(.NOT.(isto(env+2).NE.2)) GOTO 5101
       to=9023
       CALL build2
       GOTO 5100
 5101 CONTINUE
 5100 CONTINUE
      to=9024
      CALL build2
      IF(.NOT.(isto(env+2).NE.2)) GOTO 5111
       to=9025
       CALL build2
       GOTO 5110
 5111 CONTINUE
 5110 CONTINUE
      k=isto(env+47)+isto(env+50)
     1  +isto(env+33)+isto(env+41)
     2  +2*isto(env+43)
      isto(env+17)=k
      j=isto(env+21)+isto(env+16)-1
      i=0
 3121 IF(.NOT.(i.LT.k)) GOTO 3120
       i=i+1
       rsto(j+i)=rnull
       GOTO 3121
 3120 CONTINUE
      k=isto(env+24)
      IF(.NOT.(k.GT.0)) GOTO 5551
       GOTO 5550
 5551 CONTINUE
 5550 CONTINUE
      k=isto(env+27)
      IF(.NOT.(k.GT.0)) GOTO 5561
       GOTO 5560
 5561 CONTINUE
 5560 CONTINUE
      k=isto(env+31)
      IF(.NOT.(k.GT.0)) GOTO 5571
       GOTO 5570
 5571 CONTINUE
 5570 CONTINUE
      k=isto(env+38)
      IF(.NOT.(k.GT.0)) GOTO 5581
       GOTO 5580
 5581 CONTINUE
 5580 CONTINUE
      j=isto(env+21)+isto(env+16)-isto(env+19)
      IF(.NOT.(isto(env+18).GT.0)) GOTO 5591
       GOTO 5590
 5591 CONTINUE
 5590 CONTINUE
      IF(.NOT.(isto(env+19).GT.0)) GOTO 5601
       GOTO 5600
 5601 CONTINUE
 5600 CONTINUE
      k=isto(env+45)
      IF(.NOT.(k.GT.0)) GOTO 5611
       GOTO 5610
 5611 CONTINUE
 5610 CONTINUE
      IF(.NOT.(isto(env+33).GT.0)) GOTO 5621
       GOTO 5620
 5621 CONTINUE
 5620 CONTINUE
      IF(.NOT.(isto(env+41).GT.0)) GOTO 5631
       GOTO 5630
 5631 CONTINUE
 5630 CONTINUE
      IF(.NOT.(isto(env+47).GT.0)) GOTO 5641
       GOTO 5640
 5641 CONTINUE
 5640 CONTINUE
      IF(.NOT.(isto(env+50).GT.0)) GOTO 5651
       GOTO 5650
 5651 CONTINUE
 5650 CONTINUE
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
      base6=isto(sptr)
      sptr=sptr+1
      base4=isto(sptr)
      sptr=sptr+1
      base5=isto(sptr)
      sptr=sptr+1
      base3=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      cnt9=isto(sptr)
      sptr=sptr+1
      cnt8=isto(sptr)
      sptr=sptr+1
      cnt7=isto(sptr)
      sptr=sptr+1
      cnt6=isto(sptr)
      sptr=sptr+1
      cnt5=isto(sptr)
      sptr=sptr+1
      cnt4=isto(sptr)
      sptr=sptr+1
      cnt3=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
