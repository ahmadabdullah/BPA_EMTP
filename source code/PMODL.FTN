      SUBROUTINE pmodl
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO (
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010,
     1 9011, 9012, 9013, 9014, 9015,
     1 9016, 9017, 9018, 9019, 9020,
     1 9021, 9022, 9023, 9024, 9025,
     1 9026, 9027), to-9000
      STOP 'Invalid "to" reference in "pmodl".'
 9500 IF(.NOT.(from.EQ.0)) GOTO 0001
       from=isto(sptr)
       sptr=sptr+1
       RETURN
 0001 GOTO (
     1 0002, 9502, 9503, 9504, 9505,
     1 9506, 9507, 9508, 9509, 9510,
     1 9511, 9512, 9513, 9514, 9515,
     1 9516, 9517, 9518, 9519, 9520,
     1 9521, 9522, 9523, 9524, 9525,
     1 9526, 9527, 9528), from-9500
 0002 STOP 'Invalid "from" reference in "pmodl".'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9001 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
                 ptr3=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr5
                 ptr5=0
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
      isto(sptr)=ptr9
                 ptr9=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr10
                 ptr10=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr11
                 ptr11=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr12
                 ptr12=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr13
                 ptr13=0
      isto(ishenv+16)=isto(ishenv+16)+1
      isto(ishenv+63)=0
      iptr=iptr+49
      isto(env+2)=0
                 isto(ishenv+22)=env+2
      isto(env+3)=0
                 isto(ishenv+23)=env+3
      isto(env+26)=0
                 isto(ishenv+24)=env+26
      isto(env+28)=0
                 isto(ishenv+25)=env+28
      isto(env+39)=0
                 isto(ishenv+26)=env+39
      isto(env+40)=0
                 isto(ishenv+27)=env+40
      isto(env+6)=0
                 isto(ishenv+28)=env+6
      isto(env+8)=0
                 isto(ishenv+29)=env+8
      isto(env+10)=0
                 isto(ishenv+30)=env+10
      isto(env+12)=0
                 isto(ishenv+33)=env+12
      isto(env+16)=0
                 isto(ishenv+34)=env+16
      isto(env+13)=0
                 isto(ishenv+35)=env+13
      isto(env+14)=0
                 isto(ishenv+36)=env+14
      isto(env+15)=0
                 isto(ishenv+37)=env+15
      isto(env+36)=0
                 isto(ishenv+31)=env+36
      isto(env+22)=0
                 isto(ishenv+32)=env+22
      isto(env+19)=0
                 isto(ishenv+38)=env+19
      isto(env+20)=0
                 isto(ishenv+39)=env+20
      isto(env+21)=0
                 isto(ishenv+40)=env+21
      isto(env+24)=0
                 isto(ishenv+41)=env+24
      isto(env+41)=0
                 isto(ishenv+42)=env+41
      isto(env+30)=0
                 isto(ishenv+43)=env+30
      isto(env+32)=0
                 isto(ishenv+46)=env+32
      isto(env+33)=0
                 isto(ishenv+47)=env+33
      isto(env+31)=0
                 isto(ishenv+44)=env+31
      isto(env+35)=0
                 isto(ishenv+45)=env+35
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
      mndx=149
      to=9206
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5821
       isto(env+48)=0
       lpflg=0
       GOTO 5820
 5821 CONTINUE
       isto(env+48)=iptr+1
       lpflg=1
 5820 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
 3011 IF(.NOT.(lpflg.GT.0)) GOTO 3010
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9502
       GOTO 9002
 9502  from=isto(sptr)
       sptr=sptr+1
       mndx=149
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
      isto(env+44)=ptr3
      isto(env+45)=ptr4
      isto(env+23)=ptr5
      isto(env+25)=ptr7
      isto(env+27)=ptr8
      isto(env+29)=ptr9
      isto(env+34)=ptr11
      isto(env+46)=ptr12
      isto(env+47)=ptr13
      ptr13=isto(sptr)
      sptr=sptr+1
      ptr12=isto(sptr)
      sptr=sptr+1
      ptr11=isto(sptr)
      sptr=sptr+1
      ptr10=isto(sptr)
      sptr=sptr+1
      ptr9=isto(sptr)
      sptr=sptr+1
      ptr8=isto(sptr)
      sptr=sptr+1
      ptr7=isto(sptr)
      sptr=sptr+1
      ptr6=isto(sptr)
      sptr=sptr+1
      ptr5=isto(sptr)
      sptr=sptr+1
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      to=9000
      CALL xref1
      to=9000
      CALL xref2
      isto(ishenv+16)=isto(ishenv+16)-1
      GOTO 9500
 9002 CONTINUE
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
      mndx=147
      to=9206
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5021
       ndx=1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9503
       GOTO 9003
 9503  from=isto(sptr)
       sptr=sptr+1
       GOTO 5020
 5021 CONTINUE
       mndx=159
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5031
        ndx=2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9504
        GOTO 9007
 9504   from=isto(sptr)
        sptr=sptr+1
        GOTO 5030
 5031  CONTINUE
        mndx=167
        to=9206
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5041
         ndx=3
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9505
         GOTO 9014
 9505    from=isto(sptr)
         sptr=sptr+1
         GOTO 5040
 5041   CONTINUE
         mndx=171
         to=9206
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5051
          ndx=4
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9506
          GOTO 9015
 9506     from=isto(sptr)
          sptr=sptr+1
          GOTO 5050
 5051    CONTINUE
 5050    CONTINUE
 5040   CONTINUE
 5030  CONTINUE
 5020 CONTINUE
      IF(.NOT.(ndx.EQ.0)) GOTO 5061
       mndx=175
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5071
        ndx=5
        GOTO 5070
 5071  CONTINUE
        mndx=177
        to=9206
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5081
         ndx=6
         GOTO 5080
 5081   CONTINUE
         mndx=179
         to=9206
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5091
          ndx=7
          GOTO 5090
 5091    CONTINUE
          mndx=181
          to=9206
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5101
           ndx=8
           GOTO 5100
 5101     CONTINUE
           mndx=183
           to=9206
           CALL putil2
           IF(.NOT.(mflg.GT.0)) GOTO 5111
            ndx=9
            GOTO 5110
 5111      CONTINUE
            mndx=185
            to=9206
            CALL putil2
            IF(.NOT.(mflg.GT.0)) GOTO 5121
             ndx=10
             GOTO 5120
 5121       CONTINUE
             mndx=187
             to=9206
             CALL putil2
             IF(.NOT.(mflg.GT.0)) GOTO 5131
              ndx=11
              GOTO 5130
 5131        CONTINUE
              mndx=189
              to=9206
              CALL putil2
              IF(.NOT.(mflg.GT.0)) GOTO 5141
               ndx=12
               GOTO 5140
 5141         CONTINUE
 5140         CONTINUE
 5130        CONTINUE
 5120       CONTINUE
 5110      CONTINUE
 5100     CONTINUE
 5090    CONTINUE
 5080   CONTINUE
 5070  CONTINUE
       IF(.NOT.(ndx.GT.0)) GOTO 5151
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9507
        GOTO 9016
 9507   from=isto(sptr)
        sptr=sptr+1
        GOTO 5150
 5151  CONTINUE
 5150  CONTINUE
       GOTO 5060
 5061 CONTINUE
 5060 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(ndx.EQ.0)) GOTO 5161
       isto(ishenv+54)=2
       CONTINUE
       CALL synstp
       GOTO 5160
 5161 CONTINUE
 5160 CONTINUE
      isto(env+1)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9003 CONTINUE
      iptr=iptr+6
      ipn=isto(ishenv+24)
      isto(ipn)=env
                 isto(ishenv+24)=env
      isto(env+0)=0
      isto(env+3)=dptr+1
      to=9200
      CALL putil2
      isto(env+2)=dptr -isto(env+3)+1
      to=9203
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5171
       isto(ishenv+54)=1
       CONTINUE
       CALL synstp
       GOTO 5170
 5171 CONTINUE
 5170 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
                 ndx3=isto(env+2)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+3)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9508
      GOTO 9004
 9508 from=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      ptr7=ptr7+1
      isto(env+1)=ptr7
      isto(env+5)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      mndx=151
      to=9206
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5181
       ndx=2
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9509
       GOTO 9005
 9509  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5180
 5181 CONTINUE
       ndx=1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=datenv
                  datenv=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=isto(ishenv+17)
                  isto(ishenv+17)=datenv
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       sptr=sptr-27
       IF(sptr.LE.iptr) GOTO 910
       i0=sptr+27
       i1=ishenv+21
       i=0
 3131  IF(.NOT.(i.LT.27)) GOTO 3130
        i=i+1
        isto(i0-i)=isto(i1+i)
        GOTO 3131
 3130  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9510
       GOTO 9001
 9510  from=isto(sptr)
       sptr=sptr+1
       i0=sptr+27
       i1=ishenv+21
       i=0
 3141  IF(.NOT.(i.LT.27)) GOTO 3140
        i=i+1
        isto(i1+i)=isto(i0-i)
        GOTO 3141
 3140  CONTINUE
       sptr=sptr+27
       env=isto(sptr)
       sptr=sptr+1
       isto(ishenv+17)=isto(sptr)
       sptr=sptr+1
       datenv=isto(sptr)
       sptr=sptr+1
 5180 CONTINUE
      isto(env+4)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9004 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+26)
      stpflg=0
 3021 IF(.NOT.(env.GT.0
     1.AND. env.LT.isto(ishenv+24)
     2.AND. stpflg.EQ.0)) GOTO 3020
       i1=isto(env+2)
       IF(.NOT.(i1.EQ.ndx3)) GOTO 5191
         i=0
         i2=isto(env+3)
 3031    IF(.NOT.(i.LT.ndx3
     1   .AND. csto(i2+i).EQ.csto(ndx4+i)
     2   )) GOTO 3030
          i=i+1
          GOTO 3031
 3030    CONTINUE
         IF(.NOT.(i.EQ.ndx3)) GOTO 5201
          stpflg=96
          stpl1=ndx3
          stpc1=ndx4
          CONTINUE
          CALL errstp
          GOTO 5200
 5201    CONTINUE
 5200    CONTINUE
        GOTO 5190
 5191  CONTINUE
 5190  CONTINUE
       env=isto(env+0)
       GOTO 3021
 3020 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9005 CONTINUE
      iptr=iptr+2
      isto(env+0)=dptr+1
      mndx=275
      to=9208
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5211
       isto(ishenv+54)=4
       CONTINUE
       CALL synstp
       GOTO 5210
 5211 CONTINUE
 5210 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      ndx1=0
 3121 IF(.NOT.(ndx1.LT.3)) GOTO 3120
       ndx1=ndx1+1
       to=9211
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5221
        isto(ishenv+54)=4
        CONTINUE
        CALL synstp
        GOTO 5440
 5221  CONTINUE
 5220  CONTINUE
       GOTO 3121
 3120 CONTINUE
      ndx1=isto(sptr)
      sptr=sptr+1
      to=9204
      CALL putil2
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5231
       isto(ishenv+54)=5
       CONTINUE
       CALL synstp
       GOTO 5230
 5231 CONTINUE
 5230 CONTINUE
      isto(env+1)=iptr+1
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
       from=9511
       GOTO 9006
 9511  from=isto(sptr)
       sptr=sptr+1
       mndx=89
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5241
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5240
 5241  CONTINUE
        isto(env+0)=0
        lpflg=0
 5240  CONTINUE
       GOTO 3041
 3040 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9006 CONTINUE
      iptr=iptr+3
      mndx=145
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5251
       k=1
       GOTO 5250
 5251 CONTINUE
       mndx=153
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5261
        k=2
        GOTO 5260
 5261  CONTINUE
        mndx=155
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5271
         k=3
         GOTO 5270
 5271   CONTINUE
         mndx=157
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5281
          k=4
          GOTO 5280
 5281    CONTINUE
          isto(ishenv+54)=5
          CONTINUE
          CALL synstp
 5280    CONTINUE
 5270   CONTINUE
 5260  CONTINUE
 5250 CONTINUE
      isto(env+1)=k
      mndx=105
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5291
       isto(ishenv+54)=6
       CONTINUE
       CALL synstp
       GOTO 5290
 5291 CONTINUE
 5290 CONTINUE
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=1
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9007 CONTINUE
      iptr=iptr+6
      ipn=isto(ishenv+43)
      isto(ipn)=env
                 isto(ishenv+43)=env
      isto(env+0)=0
      isto(env+3)=dptr+1
      to=9200
      CALL putil2
      isto(env+2)=dptr -isto(env+3)+1
      to=9204
      CALL putil2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
                 ndx3=isto(env+2)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+3)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9512
      GOTO 9008
 9512 from=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      ptr9=ptr9+1
      isto(env+1)=ptr9
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      isto(env+5)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      mndx=83
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5301
       ndx=1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9513
       GOTO 9009
 9513  from=isto(sptr)
       sptr=sptr+1
       GOTO 5300
 5301 CONTINUE
       mndx=161
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5311
        ndx=2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9514
        GOTO 9011
 9514   from=isto(sptr)
        sptr=sptr+1
        GOTO 5310
 5311  CONTINUE
        mndx=151
        to=9206
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5321
         ndx=3
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9515
         GOTO 9013
 9515    from=isto(sptr)
         sptr=sptr+1
         GOTO 5320
 5321   CONTINUE
         isto(ishenv+54)=7
         CONTINUE
         CALL synstp
 5320   CONTINUE
 5310  CONTINUE
 5300 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+4)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9008 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=datenv
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+30)
      stpflg=0
 3051 IF(.NOT.(env.GT.0
     1.AND. env.LT.isto(ishenv+43)
     2.AND. stpflg.EQ.0)) GOTO 3050
       i1=isto(env+2)
       IF(.NOT.(i1.EQ.ndx3)) GOTO 5331
        i=0
        i2=isto(env+3)
 3061   IF(.NOT.(i.LT.ndx3
     1  .AND. csto(i2+i).EQ.csto(ndx4+i)
     2  )) GOTO 3060
         i=i+1
         GOTO 3061
 3060   CONTINUE
        IF(.NOT.(i.EQ.ndx3)) GOTO 5341
         stpflg=97
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
         GOTO 5340
 5341   CONTINUE
 5340   CONTINUE
        GOTO 5330
 5331  CONTINUE
 5330  CONTINUE
       env=isto(env+0)
       GOTO 3051
 3050 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9009 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=ptr11
      iptr=iptr+5
      ipn=isto(ishenv+46)
      isto(ipn)=env
                 isto(ishenv+46)=env
      isto(env+0)=0
      ptr10=env
      isto(env+3)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3071 IF(.NOT.(lpflg.GT.0)) GOTO 3070
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9516
       GOTO 9010
 9516  from=isto(sptr)
       sptr=sptr+1
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5351
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5350
 5351  CONTINUE
        isto(env+0)=0
        lpflg=0
 5350  CONTINUE
       GOTO 3071
 3070 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      isto(env+2)=ptr11-ndx0
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5361
       isto(ishenv+54)=8
       CONTINUE
       CALL synstp
       GOTO 5360
 5361 CONTINUE
 5360 CONTINUE
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=2
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ptr10=0
      ndx0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9010 CONTINUE
      ptr11=ptr11+1
      iptr=iptr+7
      isto(env+2)=dptr+1
      to=9200
      CALL putil2
      isto(env+1)=dptr -isto(env+2)+1
      to=9204
      CALL putil2
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5371
       isto(env+6)=iptr+1
       isto(ishenv+50)=3
       to=9126
       CALL putil1
       IF(.NOT.(mflg.EQ.0)) GOTO 5381
        isto(ishenv+54)=9
        CONTINUE
        CALL synstp
        GOTO 5380
 5381  CONTINUE
 5380  CONTINUE
       mndx=89
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5391
        isto(ishenv+54)=9
        CONTINUE
        CALL synstp
        GOTO 5390
 5391  CONTINUE
 5390  CONTINUE
       GOTO 5370
 5371 CONTINUE
       isto(env+6)=0
 5370 CONTINUE
      GOTO 9500
 9011 CONTINUE
      iptr=iptr+3
      ipn=isto(ishenv+47)
      isto(ipn)=env
                 isto(ishenv+47)=env
      isto(env+0)=0
      mndx=83
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5401
       isto(ishenv+54)=10
       CONTINUE
       CALL synstp
       GOTO 5400
 5401 CONTINUE
 5400 CONTINUE
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(env+1)=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3081 IF(.NOT.(lpflg.GT.0)) GOTO 3080
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9517
       GOTO 9012
 9517  from=isto(sptr)
       sptr=sptr+1
       mndx=83
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5411
        isto(env+0)=iptr+1
        k=env
        env=iptr+1
        isto(env+1)=k
        GOTO 5410
 5411  CONTINUE
        isto(env+0)=0
        lpflg=0
 5410  CONTINUE
       GOTO 3081
 3080 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      k=env
      env=isto(sptr)
      sptr=sptr+1
      isto(env+2)=k
      GOTO 9500
 9012 CONTINUE
      iptr=iptr+4
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=4
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      isto(env+3)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=5
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5421
       isto(ishenv+54)=11
       CONTINUE
       CALL synstp
       GOTO 5420
 5421 CONTINUE
 5420 CONTINUE
      GOTO 9500
 9013 CONTINUE
      iptr=iptr+2
      isto(env+0)=dptr+1
      mndx=163
      to=9208
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5431
       isto(ishenv+54)=12
       CONTINUE
       CALL synstp
       GOTO 5430
 5431 CONTINUE
 5430 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      ndx1=0
 3111 IF(.NOT.(ndx1.LT.3)) GOTO 3110
       ndx1=ndx1+1
       to=9211
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5441
        isto(ishenv+54)=12
        CONTINUE
        CALL synstp
        GOTO 5440
 5441  CONTINUE
 5440  CONTINUE
       GOTO 3111
 3110 CONTINUE
      ndx1=isto(sptr)
      sptr=sptr+1
      to=9204
      CALL putil2
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5451
       isto(ishenv+54)=13
       CONTINUE
       CALL synstp
       GOTO 5450
 5451 CONTINUE
 5450 CONTINUE
      mndx=165
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5461
       mndx=105
       to=9205
       CALL putil2
       GOTO 5460
 5461 CONTINUE
 5460 CONTINUE
      IF(.NOT.(mflg.EQ.0)) GOTO 5471
       isto(ishenv+54)=13
       CONTINUE
       CALL synstp
       GOTO 5470
 5471 CONTINUE
 5470 CONTINUE
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=6
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      mndx=89
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5481
       isto(ishenv+54)=13
       CONTINUE
       CALL synstp
       GOTO 5480
 5481 CONTINUE
 5480 CONTINUE
      GOTO 9500
 9014 CONTINUE
      iptr=iptr+1
      isto(ishenv+53)=1
      isto(ishenv+52)=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      CONTINUE
      CALL pstmt
      ptr12=ptr1
      isto(env+0)=ptr1
      ptr1=isto(sptr)
      sptr=sptr+1
      mndx=169
      to=9206
      CALL putil2
      isto(ishenv+52)=0
      GOTO 9500
 9015 CONTINUE
      iptr=iptr+1
      isto(ishenv+53)=2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      CONTINUE
      CALL pstmt
      ptr13=ptr1
      isto(env+0)=ptr1
      ptr1=isto(sptr)
      sptr=sptr+1
      mndx=173
      to=9206
      CALL putil2
      GOTO 9500
 9016 CONTINUE
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
 3091 IF(.NOT.(lpflg.GT.0)) GOTO 3090
       IF(.NOT.(ndx.EQ.5)) GOTO 5491
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9518
        GOTO 9018
 9518   from=isto(sptr)
        sptr=sptr+1
        GOTO 5490
 5491  IF(.NOT.(ndx.EQ.6)) GOTO 5492
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9519
        GOTO 9019
 9519   from=isto(sptr)
        sptr=sptr+1
        GOTO 5490
 5492  IF(.NOT.(ndx.EQ.7)) GOTO 5493
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9520
        GOTO 9020
 9520   from=isto(sptr)
        sptr=sptr+1
        GOTO 5490
 5493  IF(.NOT.(ndx.EQ.8)) GOTO 5494
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9521
        GOTO 9021
 9521   from=isto(sptr)
        sptr=sptr+1
        GOTO 5490
 5494  IF(.NOT.(ndx.EQ.9)) GOTO 5495
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9522
        GOTO 9022
 9522   from=isto(sptr)
        sptr=sptr+1
        GOTO 5490
 5495  IF(.NOT.(ndx.EQ.10)) GOTO 5496
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9523
        GOTO 9023
 9523   from=isto(sptr)
        sptr=sptr+1
        GOTO 5490
 5496  IF(.NOT.(ndx.EQ.11)) GOTO 5497
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9524
        GOTO 9024
 9524   from=isto(sptr)
        sptr=sptr+1
        GOTO 5490
 5497  IF(.NOT.(ndx.EQ.12)) GOTO 5498
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9525
        GOTO 9025
 9525   from=isto(sptr)
        sptr=sptr+1
        GOTO 5490
 5498  CONTINUE
 5490  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9526
       GOTO 9017
 9526  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.EQ.0)) GOTO 5501
        isto(env+1)=iptr+1
        env=iptr+1
        GOTO 5500
 5501  CONTINUE
        isto(env+1)=0
        lpflg=0
 5500  CONTINUE
       GOTO 3091
 3090 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9017 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=dptr
      mndx=149
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5511
       mndx=147
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5521
        mndx=159
        to=9206
        CALL putil2
        IF(.NOT.(mflg.EQ.0)) GOTO 5531
         mndx=167
         to=9206
         CALL putil2
         IF(.NOT.(mflg.EQ.0)) GOTO 5541
          mndx=171
          to=9206
          CALL putil2
          IF(.NOT.(mflg.EQ.0)) GOTO 5551
           mndx=175
           to=9206
           CALL putil2
           IF(.NOT.(mflg.EQ.0)) GOTO 5561
            mndx=177
            to=9206
            CALL putil2
            IF(.NOT.(mflg.EQ.0)) GOTO 5571
             mndx=179
             to=9206
             CALL putil2
             IF(.NOT.(mflg.EQ.0)) GOTO 5581
              mndx=181
              to=9206
              CALL putil2
              IF(.NOT.(mflg.EQ.0)) GOTO 5591
               mndx=183
               to=9206
               CALL putil2
               IF(.NOT.(mflg.EQ.0)) GOTO 5601
                mndx=185
                to=9206
                CALL putil2
                IF(.NOT.(mflg.EQ.0)) GOTO 5611
                 mndx=187
                 to=9206
                 CALL putil2
                 IF(.NOT.(mflg.EQ.0)) GOTO 5621
                  mndx=189
                  to=9206
                  CALL putil2
                  GOTO 5620
 5621            CONTINUE
 5620            CONTINUE
                 GOTO 5610
 5611           CONTINUE
 5610           CONTINUE
                GOTO 5600
 5601          CONTINUE
 5600          CONTINUE
               GOTO 5590
 5591         CONTINUE
 5590         CONTINUE
              GOTO 5580
 5581        CONTINUE
 5580        CONTINUE
             GOTO 5570
 5571       CONTINUE
 5570       CONTINUE
            GOTO 5560
 5561      CONTINUE
 5560      CONTINUE
           GOTO 5550
 5551     CONTINUE
 5550     CONTINUE
          GOTO 5540
 5541    CONTINUE
 5540    CONTINUE
         GOTO 5530
 5531   CONTINUE
 5530   CONTINUE
        GOTO 5520
 5521  CONTINUE
 5520  CONTINUE
       GOTO 5510
 5511 CONTINUE
 5510 CONTINUE
      dptr=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9018 CONTINUE
      iptr=iptr+6
      ipn=isto(ishenv+28)
      isto(ipn)=env
                 isto(ishenv+28)=env
      isto(env+0)=0
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=7
      to=9119
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5631
       isto(ishenv+54)=14
       CONTINUE
       CALL synstp
       GOTO 5630
 5631 CONTINUE
 5630 CONTINUE
      isto(env+5)=iptr+1
      isto(ishenv+50)=8
      to=9125
      CALL putil1
      IF(.NOT.(mflg.EQ.0)) GOTO 5641
       isto(ishenv+54)=14
       CONTINUE
       CALL synstp
       GOTO 5640
 5641 CONTINUE
 5640 CONTINUE
      mndx=89
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5651
       isto(ishenv+54)=14
       CONTINUE
       CALL synstp
       GOTO 5650
 5651 CONTINUE
 5650 CONTINUE
      GOTO 9500
 9019 CONTINUE
      iptr=iptr+6
      ipn=isto(ishenv+29)
      isto(ipn)=env
                 isto(ishenv+29)=env
      isto(env+0)=0
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=9
      to=9119
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5661
       isto(env+5)=iptr+1
       isto(ishenv+50)=10
       to=9126
       CALL putil1
       IF(.NOT.(mflg.EQ.0)) GOTO 5671
        isto(ishenv+54)=15
        CONTINUE
        CALL synstp
        GOTO 5670
 5671  CONTINUE
 5670  CONTINUE
       mndx=89
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5681
        isto(ishenv+54)=15
        CONTINUE
        CALL synstp
        GOTO 5680
 5681  CONTINUE
 5680  CONTINUE
       GOTO 5660
 5661 CONTINUE
       isto(env+5)=0
 5660 CONTINUE
      GOTO 9500
 9020 CONTINUE
      iptr=iptr+5
      ipn=isto(ishenv+33)
      isto(ipn)=env
                 isto(ishenv+33)=env
      isto(env+0)=0
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=11
      to=9119
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9021 CONTINUE
      iptr=iptr+6
      ipn=isto(ishenv+30)
      isto(ipn)=env
                 isto(ishenv+30)=env
      isto(env+0)=0
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=12
      to=9119
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5691
       isto(env+5)=iptr+1
       isto(ishenv+50)=13
       to=9126
       CALL putil1
       IF(.NOT.(mflg.EQ.0)) GOTO 5701
        isto(ishenv+54)=16
        CONTINUE
        CALL synstp
        GOTO 5700
 5701  CONTINUE
 5700  CONTINUE
       mndx=89
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5711
        isto(ishenv+54)=16
        CONTINUE
        CALL synstp
        GOTO 5710
 5711  CONTINUE
 5710  CONTINUE
       GOTO 5690
 5691 CONTINUE
       isto(env+5)=0
 5690 CONTINUE
      GOTO 9500
 9022 CONTINUE
      iptr=iptr+4
      ipn=isto(ishenv+31)
      isto(ipn)=env
                 isto(ishenv+31)=env
      isto(env+0)=0
      isto(env+3)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=14
      to=9119
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9023 CONTINUE
      iptr=iptr+6
      ipn=isto(ishenv+32)
      isto(ipn)=env
                 isto(ishenv+32)=env
      isto(env+0)=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      isto(env+4)=iptr+1
      isto(ishenv+50)=16
      to=9100
      CALL putil1
      isto(env+2)=ndx1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5721
       isto(env+5)=iptr+1
       isto(ishenv+50)=15
       to=9126
       CALL putil1
       IF(.NOT.(mflg.EQ.0)) GOTO 5731
        isto(ishenv+54)=17
        CONTINUE
        CALL synstp
        GOTO 5730
 5731  CONTINUE
 5730  CONTINUE
       mndx=89
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5741
        isto(ishenv+54)=17
        CONTINUE
        CALL synstp
        GOTO 5740
 5741  CONTINUE
 5740  CONTINUE
       GOTO 5720
 5721 CONTINUE
       isto(env+5)=0
 5720 CONTINUE
      GOTO 9500
 9024 CONTINUE
      iptr=iptr+3
      isto(env+2)=iptr+1
      isto(ishenv+50)=20
      to=9127
      CALL putil1
      IF(.NOT.(mflg.GT.0)) GOTO 5751
       isto(env+0)=1
       ptr3=isto(env+2)
       GOTO 5750
 5751 CONTINUE
       isto(ishenv+50)=21
       to=9128
       CALL putil1
       IF(.NOT.(mflg.GT.0)) GOTO 5761
        isto(env+0)=2
        ptr4=isto(env+2)
        GOTO 5760
 5761  CONTINUE
        isto(ishenv+54)=19
        CONTINUE
        CALL synstp
 5760  CONTINUE
 5750 CONTINUE
      GOTO 9500
 9025 CONTINUE
      iptr=iptr+3
      mndx=191
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5771
       isto(ishenv+54)=20
       CONTINUE
       CALL synstp
       GOTO 5770
 5771 CONTINUE
 5770 CONTINUE
      mndx=83
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5781
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9527
       GOTO 9026
 9527  from=isto(sptr)
       sptr=sptr+1
       mndx=105
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5791
        isto(ishenv+54)=20
        CONTINUE
        CALL synstp
        GOTO 5790
 5791  CONTINUE
 5790  CONTINUE
       isto(env+2)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=22
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5780
 5781 CONTINUE
       isto(env+0)=0
       isto(env+2)=iptr+1
       isto(ishenv+50)=23
       to=9126
       CALL putil1
       IF(.NOT.(mflg.EQ.0)) GOTO 5801
        isto(ishenv+54)=20
        CONTINUE
        CALL synstp
        GOTO 5800
 5801  CONTINUE
 5800  CONTINUE
 5780  CONTINUE
      GOTO 9500
 9026 CONTINUE
      isto(env+0)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3101 IF(.NOT.(lpflg.GT.0)) GOTO 3100
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9528
       GOTO 9027
 9528  from=isto(sptr)
       sptr=sptr+1
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5811
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5810
 5811  CONTINUE
        isto(env+0)=0
        lpflg=0
 5810  CONTINUE
       GOTO 3101
 3100 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9027 CONTINUE
      iptr=iptr+2
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=7
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=24
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
