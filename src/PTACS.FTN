      SUBROUTINE ptacs
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
     1 9501, 9502, 9503, 9504, 9505,
     1 9506, 9507, 9508, 9509, 9510,
     1 9511), from-9500
      STOP 'Invalid "from" reference in "ptacs".'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr6
                 ptr6=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr7
                 ptr7=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr8
                 ptr8=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr10
                 ptr10=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr14
                 ptr14=0
      isto(ishenv+13)=iptr+1
      env=iptr+1
      iptr=iptr+5
      isto(env+1)=0
      isto(env+3)=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
      to=9204
      CALL putil2
      mndx=141
      to=9206
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5001
       isto(env+2)=0
       isto(env+4)=0
       lpflg=0
       GOTO 5000
 5001 CONTINUE
       isto(ishenv+16)=0
       isto(ishenv+12)=0
       datenv=iptr+1
       isto(ishenv+17)=iptr+1
       isto(env+2)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       i=0
 3001  IF(.NOT.(i.LT.49)) GOTO 3000
        i=i+1
        isto(iptr+i)=0
        GOTO 3001
 3000  CONTINUE
       iptr=iptr+49
       isto(ishenv+22)=env+2
       isto(ishenv+23)=env+3
       isto(ishenv+24)=env+26
       isto(ishenv+25)=env+28
       isto(ishenv+26)=env+39
       isto(ishenv+27)=env+40
       isto(ishenv+30)=env+10
       isto(ishenv+31)=env+36
       isto(ishenv+40)=env+21
       isto(ishenv+44)=env+31
       isto(ishenv+45)=env+35
       env=isto(sptr)
       sptr=sptr+1
       isto(ishenv+48)=env+1
       isto(env+4)=iptr+1
       lpflg=1
 5000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
 3011 IF(.NOT.(lpflg.GT.0)) GOTO 3010
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9501
       GOTO 9001
 9501  from=isto(sptr)
       sptr=sptr+1
       mndx=141
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5011
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5010
 5011  CONTINUE
        isto(env+0)=0
        lpflg=0
 5010  CONTINUE
       GOTO 3011
 3010 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      lpflg=isto(sptr)
      sptr=sptr+1
      k=isto(env+2)
      IF(.NOT.(k.GT.0)) GOTO 5021
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+2)
       isto(env+25)=ptr7
       isto(env+27)=ptr8
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9502
       GOTO 9008
 9502  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5020
 5021 CONTINUE
 5020 CONTINUE
      isto(env+0)=ptr14
      ptr14=isto(sptr)
      sptr=sptr+1
      ptr10=isto(sptr)
      sptr=sptr+1
      ptr8=isto(sptr)
      sptr=sptr+1
      ptr7=isto(sptr)
      sptr=sptr+1
      ptr6=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9001 CONTINUE
      iptr=iptr+3
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      mndx=181
      to=9206
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5031
       ndx=1
       GOTO 5030
 5031 CONTINUE
       mndx=183
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5041
        ndx=2
        GOTO 5040
 5041  CONTINUE
        mndx=143
        to=9206
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5051
         ndx=3
         GOTO 5050
 5051   CONTINUE
 5050   CONTINUE
 5040  CONTINUE
 5030 CONTINUE
      IF(.NOT.(ndx.GT.0)) GOTO 5061
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9503
       GOTO 9002
 9503  from=isto(sptr)
       sptr=sptr+1
       GOTO 5060
 5061 CONTINUE
       mndx=147
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5071
        ndx=4
        to=9003
        CALL pmodl
        GOTO 5070
 5071  CONTINUE
        mndx=239
        to=9206
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5081
         ndx=5
         CONTINUE
         CALL puse
         GOTO 5080
 5081   CONTINUE
         isto(ishenv+54)=74
         CONTINUE
         CALL synstp
 5080   CONTINUE
 5070  CONTINUE
 5060 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+1)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9002 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3021 IF(.NOT.(lpflg.GT.0)) GOTO 3020
       IF(.NOT.(ndx.EQ.1)) GOTO 5091
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9504
        GOTO 9004
 9504   from=isto(sptr)
        sptr=sptr+1
        GOTO 5090
 5091  IF(.NOT.(ndx.EQ.2)) GOTO 5092
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9505
        GOTO 9005
 9505   from=isto(sptr)
        sptr=sptr+1
        GOTO 5090
 5092  IF(.NOT.(ndx.EQ.3)) GOTO 5093
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9506
        GOTO 9006
 9506   from=isto(sptr)
        sptr=sptr+1
        GOTO 5090
 5093  CONTINUE
 5090  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9507
       GOTO 9003
 9507  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.GT.0)) GOTO 5101
        lpflg=0
        isto(env+1)=0
        GOTO 5100
 5101  CONTINUE
        isto(env+1)=iptr+1
        env=iptr+1
 5100  CONTINUE
       GOTO 3021
 3020 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9003 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=dptr
      mndx=181
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5111
       mndx=183
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5121
        mndx=143
        to=9206
        CALL putil2
        IF(.NOT.(mflg.EQ.0)) GOTO 5131
         mndx=147
         to=9206
         CALL putil2
         IF(.NOT.(mflg.EQ.0)) GOTO 5141
          mndx=239
          to=9206
          CALL putil2
          IF(.NOT.(mflg.EQ.0)) GOTO 5151
           mndx=141
           to=9206
           CALL putil2
           GOTO 5150
 5151     CONTINUE
 5150     CONTINUE
          GOTO 5140
 5141    CONTINUE
 5140    CONTINUE
         GOTO 5130
 5131   CONTINUE
 5130   CONTINUE
        GOTO 5120
 5121  CONTINUE
 5120  CONTINUE
       GOTO 5110
 5111 CONTINUE
 5110 CONTINUE
      dptr=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9004 CONTINUE
      iptr=iptr+7
      ipn=isto(ishenv+30)
      isto(ipn)=env
                 isto(ishenv+30)=env
      isto(env+0)=0
      isto(env+4)=dptr+1
      to=9200
      CALL putil2
      isto(env+3)=dptr-isto(env+4)+1
      to=9204
      CALL putil2
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5161
       isto(ishenv+54)=80
       CONTINUE
       CALL synstp
       GOTO 5160
 5161 CONTINUE
 5160 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=397
      mflg=0
 3031 IF(.NOT.(ndx.LT.409 .AND. mflg.EQ.0)) GOTO 3030
       ndx=ndx+2
       mndx=ndx
       to=9205
       CALL putil2
       IF(.NOT.(ndx.EQ.401 .AND. mflg.GT.0
     1 )) GOTO 5381
        sptr=sptr-1
        isto(sptr)=dptr
        mndx=83
        to=9205
        CALL putil2
        dptr=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(mflg.EQ.0)) GOTO 5391
         dptr=dptr-1
         GOTO 5390
 5391   CONTINUE
 5390   CONTINUE
        GOTO 5380
 5381  CONTINUE
 5380  CONTINUE
       GOTO 3031
 3030 CONTINUE
      IF(.NOT.(mflg.EQ.0)) GOTO 5171
       isto(ishenv+54)=81
       CONTINUE
       CALL synstp
       GOTO 5170
 5171 CONTINUE
 5170 CONTINUE
      ndx=(ndx-397)/2
      isto(env+2)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=83
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5181
       isto(ishenv+54)=82
       CONTINUE
       CALL synstp
       GOTO 5180
 5181 CONTINUE
 5180 CONTINUE
      isto(env+6)=dptr+1
      to=9200
      CALL putil2
      isto(env+5)=dptr-isto(env+6)+1
      IF(.NOT.(isto(env+5).GT.6)) GOTO 5191
       isto(ishenv+54)=85
       CONTINUE
       CALL synstp
       GOTO 5190
 5191 CONTINUE
 5190 CONTINUE
      to=9204
      CALL putil2
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5201
       isto(ishenv+54)=83
       CONTINUE
       CALL synstp
       GOTO 5200
 5201 CONTINUE
 5200 CONTINUE
      mndx=89
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5211
       isto(ishenv+54)=84
       CONTINUE
       CALL synstp
       GOTO 5210
 5211 CONTINUE
 5210 CONTINUE
      GOTO 9500
 9005 CONTINUE
      iptr=iptr+4
      ipn=isto(ishenv+31)
      isto(ipn)=env
                 isto(ishenv+31)=env
      isto(env+0)=0
      isto(env+3)=dptr+1
      to=9200
      CALL putil2
      isto(env+2)=dptr-isto(env+3)+1
      IF(.NOT.(isto(env+2).GT.6)) GOTO 5361
       isto(ishenv+54)=78
       CONTINUE
       CALL synstp
       GOTO 5360
 5361 CONTINUE
 5360 CONTINUE
      to=9204
      CALL putil2
      GOTO 9500
 9006 CONTINUE
      iptr=iptr+7
      ipn=isto(ishenv+48)
      isto(ipn)=env
                 isto(ishenv+48)=env
      isto(env+0)=0
      ptr14=ptr14+1
      isto(env+3)=iinf
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9508
      GOTO 9007
 9508 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      mndx=255
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5221
       isto(ishenv+54)=77
       CONTINUE
       CALL synstp
       GOTO 5220
 5221 CONTINUE
 5220 CONTINUE
      isto(env+5)=dptr+1
      to=9200
      CALL putil2
      isto(env+4)=dptr -isto(env+5) +1
      IF(.NOT.(isto(env+4).GT.6)) GOTO 5231
       isto(ishenv+54)=78
       CONTINUE
       CALL synstp
       GOTO 5230
 5231 CONTINUE
 5230 CONTINUE
      to=9204
      CALL putil2
      mndx=139
      to=9206
      CALL putil2
      GOTO 9500
 9007 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3041 IF(.NOT.(lpflg.GT.0)) GOTO 3040
       iptr=iptr+4
       isto(env+2)=dptr+1
       to=9200
       CALL putil2
       isto(env+1)=dptr -isto(env+2) +1
       to=9204
       CALL putil2
       mndx=79
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5241
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=flg1
        mndx=93
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5251
         flg1=1
         GOTO 5250
 5251   CONTINUE
         flg1=0
 5250   CONTINUE
        to=9214
        CALL putil2
        a=rsto(rptr)
        rptr=rptr-1
        isto(env+3)=idnint(a)
        flg1=isto(sptr)
        sptr=sptr+1
        mndx=81
        to=9205
        CALL putil2
        IF(.NOT.(mflg.EQ.0)) GOTO 5261
         isto(ishenv+54)=79
         CONTINUE
         CALL synstp
         GOTO 5260
 5261   CONTINUE
 5260   CONTINUE
        GOTO 5240
 5241  CONTINUE
        isto(env+3)=iinf
 5240  CONTINUE
       mndx=113
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5271
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5270
 5271  CONTINUE
        isto(env+0)=0
        lpflg=0
 5270  CONTINUE
       GOTO 3041
 3040 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9008 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt
                 cnt=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=0
      isto(env+1)=ptr1
      i0=base1 +417
      k=8
      i=0
 3051 IF(.NOT.(i.LT.k)) GOTO 3050
       i=i+1
       i0=i0+2
       cnt=cnt+1
       isto(iptr+1)=isto(i0+1)
       isto(iptr+2)=isto(i0)
       iptr=iptr+2
       GOTO 3051
 3050 CONTINUE
      isto(env+4)=k
      cnt=k
      ndx0=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+10)
 3061 IF(.NOT.(env.GT.0)) GOTO 3060
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9509
       GOTO 9009
 9509  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3061
 3060 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+9)=cnt-ndx0
      ndx0=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+36)
 3071 IF(.NOT.(env.GT.0)) GOTO 3070
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9510
       GOTO 9010
 9510  from=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3071
 3070 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+11)=cnt-ndx0
      isto(env+37)=cnt-ndx0
      isto(env+0)=cnt
      ndx0=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      to=9012
      CALL xref1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9511
      GOTO 9011
 9511 from=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9009 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
                 ndx3=isto(env+3)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+4)
      to=9005
      CALL xref1
      IF(.NOT.(flg.EQ.0)) GOTO 5281
       to=9006
       CALL xref1
       GOTO 5280
 5281 CONTINUE
 5280 CONTINUE
      IF(.NOT.(flg.GT.0)) GOTO 5291
       stpflg=46
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5290
 5291 CONTINUE
 5290 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
                 ndx2=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=ptr1-2
      to=9003
      CALL xref1
      ndx0=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.GT.0)) GOTO 5301
       IF(.NOT.(ndx1.GT.ndx0)) GOTO 5311
        stpflg=99
        GOTO 5310
 5311  CONTINUE
        stpflg=122
 5310  CONTINUE
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5300
 5301 CONTINUE
       cnt=cnt+1
       isto(iptr+1)=ndx3
       isto(iptr+2)=ndx4
       iptr=iptr+2
 5300 CONTINUE
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
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
      isto(sptr)=ndx3
                 ndx3=isto(env+2)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+3)
      to=9005
      CALL xref1
      IF(.NOT.(flg.EQ.0)) GOTO 5321
       to=9006
       CALL xref1
       GOTO 5320
 5321 CONTINUE
 5320 CONTINUE
      IF(.NOT.(flg.GT.0)) GOTO 5331
       stpflg=46
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5330
 5331 CONTINUE
 5330 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
                 ndx2=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=ptr1-2
      to=9003
      CALL xref1
      ndx0=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.GT.0)) GOTO 5341
       IF(.NOT.(ndx1.GT.ndx0)) GOTO 5351
        stpflg=123
        GOTO 5350
 5351  IF(.NOT.(ndx1.GT.8)) GOTO 5352
        stpflg=124
        GOTO 5350
 5352  CONTINUE
        stpflg=125
 5350  CONTINUE
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5340
 5341 CONTINUE
       cnt=cnt+1
       isto(iptr+1)=ndx3
       isto(iptr+2)=ndx4
       iptr=iptr+2
 5340 CONTINUE
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9011 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+31)
 3081 IF(.NOT.(env.GT.0)) GOTO 3080
       IF(.NOT.(isto(env+1).GT.0)) GOTO 5371
        isto(env+4)=0
        isto(env+5)=0
        GOTO 5370
 5371  CONTINUE
        stpflg=133
        stpl1=isto(env+2)
        stpc1=isto(env+3)
        CONTINUE
        CALL errstp
 5370  CONTINUE
       env=isto(env+0)
       GOTO 3081
 3080 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
