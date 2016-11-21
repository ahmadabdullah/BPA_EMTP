      SUBROUTINE calsto
      INCLUDE  'tacsto.ins'
      from=0
      GOTO 9000
 9500 IF(from.EQ.0) RETURN
      GOTO (
     1 9501, 9502), from-9500
      STOP 'Invalid "from" reference in "calsto".'
 9000 CONTINUE
      stpflg=0
      to=0
      from=0
      stpt=-rinf
      ishenv=10+strcnt+1
      isto(ishenv+51)=0
      isto(ishenv+52)=0
      isto(ishenv+60)=0
      isto(ishenv+55)=0
      isto(ishenv+56)=0
      isto(ishenv+57)=0
      isto(ishenv+15)=0
      isto(ishenv+61)=0
      isto(ishenv+ 0)=ndx0
      iptr=10+strcnt+ishcnt+iempty+intcnt
      split=6.0d0
      pgsize=(stoflt-40)/64
      cnt4=2
      cnt5=4
      cnt7=0
      sptr=(stoflt-40)/(two*bratio)
      rptr=(sptr*bratio)+2
      to=9204
      CALL putil2
      mndx=507
      to=9206
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5061
       sptr=sptr-1
       from=9501
       GOTO 9001
 9501  from=isto(sptr)
       sptr=sptr+1
       GOTO 5060
 5061 CONTINUE
 5060 CONTINUE
      cnt2=pgsize/intlen
      cnt3=pgsize/fltlen
      a=(stoflt-40)*bratio/(split+bratio)
      rsize=idint(a)
      isize=idint(a*split)
      csize=stocha
      ilen=isize-cnt4*cnt2
      rlen=stoflt-40-cnt5*cnt3
      clen=stocha
      rbase=stoflt-40-rsize
      IF(.NOT.(ilen.LT.2000)) GOTO 5001
       stpflg=134
       stpi2=cnt2
       stpi3=isize
       stpi4=ilen
       stpi5=stoflt
       GOTO 5000
 5001 IF(.NOT.(rlen-rbase.LT.200)) GOTO 5002
       stpflg=135
       stpi2=cnt3
       stpi3=rsize
       stpi4=rlen
       stpi5=stoflt
       GOTO 5000
 5002 IF(.NOT.(clen.LT.1800)) GOTO 5003
       stpflg=136
       stpi2=0
       stpi3=csize
       stpi4=clen
       stpi5=stocha
       GOTO 5000
 5003 CONTINUE
 5000 CONTINUE
      IF(.NOT.(stpflg.GT.0)) GOTO 5011
       stpi1=pgsize
       stpr1=split
       CONTINUE
       CALL errstp
       GOTO 5010
 5011 CONTINUE
 5010 CONTINUE
      rptr=rbase
      sptr=ilen+1
      rsptr=rlen+1
      ibkptr=0
      rbkptr=0
      do 100 i=iptr+1,isize
  100 isto(i)=inull
      do 101 i=rptr+1,stoflt-40
  101 rsto(i)=rnull
      isto(ishenv+7)=iptr+1
      env=iptr+1
      iptr=iptr+6
      isto(env+0)=cnt4
      isto(env+1)=cnt2
      isto(env+2)=0
      IF(.NOT.(cnt4.GT.0)) GOTO 5021
       isto(env+3)=1
       sptr=sptr-1
       isto(sptr)=env
       cnt0=ilen
       cnt6=0
       i=0
 3001  IF(.NOT.(i.LT.cnt4)) GOTO 3000
        i=i+1
        env=iptr+1
        iptr=iptr+5
        isto(env+0)=cnt0
        cnt0=cnt0+cnt2
        isto(env+1)=cnt6
        cnt6=cnt6+cnt2
        isto(env+3)=0
        isto(env+4)=0
        GOTO 3001
 3000  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5020
 5021 CONTINUE
       isto(env+3)=0
 5020 CONTINUE
      IF(.NOT.(cnt4.GT.0)) GOTO 5051
       cnt1=0
       isto(env+4)=iptr+1
       sptr=sptr-1
       isto(sptr)=env
                  env=env+1
       i=0
 3031  IF(.NOT.(i.LT.cnt4)) GOTO 3030
        i=i+1
        env=env+5
        isto(env+2)=iptr+1
        sptr=sptr-1
        isto(sptr)=env
                   env=iptr+1
        iptr=iptr+3
        isto(env+0)=iptr+1
        cnt1=cnt1+1
        isto(env+1)=cnt1
        cnt7=cnt7+1
        isto(env+2)=cnt7
        env=isto(sptr)
        sptr=sptr+1
        GOTO 3031
 3030  CONTINUE
       isto(iptr-2)=0
       env=isto(sptr)
       sptr=sptr+1
       isto(env+5)=iptr-2
       GOTO 5050
 5051 CONTINUE
       isto(env+4)=0
       isto(env+5)=0
 5050 CONTINUE
      isto(ishenv+8)=iptr+1
      env=iptr+1
      iptr=iptr+6
      isto(env+0)=cnt5
      isto(env+1)=cnt3
      isto(env+2)=0
      IF(.NOT.(cnt5.GT.0)) GOTO 5031
       isto(env+3)=1
       sptr=sptr-1
       isto(sptr)=env
       cnt0=rlen
       cnt6=0
       i=0
 3011  IF(.NOT.(i.LT.cnt5)) GOTO 3010
        i=i+1
        env=iptr+1
        iptr=iptr+5
        isto(env+0)=cnt0
        cnt0=cnt0+cnt3
        isto(env+1)=cnt6
        cnt6=cnt6+cnt3
        isto(env+3)=0
        isto(env+4)=0
        GOTO 3011
 3010  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5030
 5031 CONTINUE
       isto(env+3)=0
 5030 CONTINUE
      IF(.NOT.(cnt5.GT.0)) GOTO 5041
       cnt1=0
       isto(env+4)=iptr+1
       sptr=sptr-1
       isto(sptr)=env
                  env=env+1
       i=0
 3021  IF(.NOT.(i.LT.cnt5)) GOTO 3020
        i=i+1
        env=env+5
        isto(env+2)=iptr+1
        sptr=sptr-1
        isto(sptr)=env
                   env=iptr+1
        iptr=iptr+3
        isto(env+0)=iptr+1
        cnt1=cnt1+1
        isto(env+1)=cnt1
        cnt7=cnt7+1
        isto(env+2)=cnt7
        env=isto(sptr)
        sptr=sptr+1
        GOTO 3021
 3020  CONTINUE
       isto(iptr-2)=0
       env=isto(sptr)
       sptr=sptr+1
       isto(env+5)=iptr-2
       GOTO 5040
 5041 CONTINUE
       isto(env+4)=0
       isto(env+5)=0
 5040 CONTINUE
      isto(ishenv+9)=cnt7
      GOTO 9500
 9001 CONTINUE
      sptr=sptr-1
      isto(sptr)=lpflg
      lpflg=1
 3041 IF(.NOT.(lpflg.GT.0)) GOTO 3040
       mndx=509
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5071
        lpflg=0
        GOTO 5070
 5071  CONTINUE
        sptr=sptr-1
        isto(sptr)=from
        from=9502
        GOTO 9002
 9502   from=isto(sptr)
        sptr=sptr+1
 5070  CONTINUE
       GOTO 3041
 3040 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9002 CONTINUE
      sptr=sptr-1
      isto(sptr)=ndx0
      ndx0=0
      sptr=sptr-1
      isto(sptr)=flg1
      flg1=0
      mndx=499
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5081
       mndx=495
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5091
        mndx=97
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5101
         mndx=497
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5111
          mndx=105
          to=9205
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5121
           ndx0=1
           GOTO 5120
 5121     CONTINUE
 5120     CONTINUE
          GOTO 5110
 5111    CONTINUE
 5110    CONTINUE
         GOTO 5100
 5101   CONTINUE
 5100   CONTINUE
        IF(.NOT.(ndx0.NE.1)) GOTO 5131
         isto(ishenv+54)=92
         CONTINUE
         CALL synstp
         GOTO 5130
 5131   CONTINUE
 5130   CONTINUE
        GOTO 5090
 5091  CONTINUE
        mndx=105
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5141
         ndx0=1
         GOTO 5140
 5141   CONTINUE
         isto(ishenv+54)=92
         CONTINUE
         CALL synstp
 5140   CONTINUE
 5090  CONTINUE
       GOTO 5080
 5081 CONTINUE
 5080 CONTINUE
      IF(.NOT.(ndx0.EQ.1)) GOTO 5151
       to=9214
       CALL putil2
       split=rsto(rptr)
       rptr=rptr-1
       GOTO 5150
 5151 CONTINUE
       mndx=501
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5161
        mndx=97
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5171
         mndx=503
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5181
          mndx=105
          to=9205
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5191
           ndx0=2
           to=9214
           CALL putil2
           a=rsto(rptr)
           rptr=rptr-1
           pgsize=idnint(a)
           GOTO 5190
 5191     CONTINUE
 5190     CONTINUE
          GOTO 5180
 5181    CONTINUE
 5180    CONTINUE
         GOTO 5170
 5171   CONTINUE
 5170   CONTINUE
        IF(.NOT.(ndx0.NE.2)) GOTO 5201
         isto(ishenv+54)=93
         CONTINUE
         CALL synstp
         GOTO 5200
 5201   CONTINUE
 5200   CONTINUE
        GOTO 5160
 5161  CONTINUE
        mndx=495
        to=9206
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5211
         mndx=505
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5221
          mndx=105
          to=9205
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5231
           ndx0=3
           to=9214
           CALL putil2
           a=rsto(rptr)
           rptr=rptr-1
           cnt4=idnint(a)
           GOTO 5230
 5231     CONTINUE
 5230     CONTINUE
          GOTO 5220
 5221    CONTINUE
 5220    CONTINUE
         IF(.NOT.(ndx0.NE.3)) GOTO 5241
          isto(ishenv+54)=94
          CONTINUE
          CALL synstp
          GOTO 5240
 5241    CONTINUE
 5240    CONTINUE
         GOTO 5210
 5211   CONTINUE
         mndx=497
         to=9206
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5251
          mndx=505
          to=9205
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5261
           mndx=105
           to=9205
           CALL putil2
           IF(.NOT.(mflg.GT.0)) GOTO 5271
            ndx0=4
            to=9214
            CALL putil2
            a=rsto(rptr)
            rptr=rptr-1
            cnt5=idnint(a)
            GOTO 5270
 5271      CONTINUE
 5270      CONTINUE
           GOTO 5260
 5261     CONTINUE
 5260     CONTINUE
          IF(.NOT.(ndx0.NE.4)) GOTO 5281
           isto(ishenv+54)=95
           CONTINUE
           CALL synstp
           GOTO 5280
 5281     CONTINUE
 5280     CONTINUE
          GOTO 5250
 5251    CONTINUE
          isto(ishenv+54)=96
          CONTINUE
          CALL synstp
 5250    CONTINUE
 5210   CONTINUE
 5160  CONTINUE
 5150 CONTINUE
      flg1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
      END
