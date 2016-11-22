      SUBROUTINE xref1
      INCLUDE  'tacsto.ins'
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=0
      GOTO ( 9000,
     1 9001, 9002, 9003, 9004, 9005,
     1 9006, 9007, 9008, 9009, 9010,
     1 9011, 9012, 9013, 9014, 9015,
     1 9016, 9017, 9018, 9019, 9020,
     1 9021), to-8999
      STOP 'Invalid "to" reference in "xref1".'
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
     1  9546,  9547,  9548,  9549,  9550), from-9500
      STOP 'Invalid "from" reference in xref1.'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9501
      GOTO 9001
 9501 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9502
      GOTO 9010
 9502 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9503
      GOTO 9012
 9503 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9504
      GOTO 9019
 9504 from=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9001 CONTINUE
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
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=0
      isto(env+1)=ptr1
      j=isto(env-3)
      k=j+isto(env-4)-1
      i0=base1 +417
      k=8
      i=0
 3001 IF(.NOT.(i.LT.k)) GOTO 3000
       i=i+1
       i0=i0+2
       cnt=cnt+1
       isto(iptr+1)=isto(i0+1)
       isto(iptr+2)=isto(i0)
       iptr=iptr+2
       GOTO 3001
 3000 CONTINUE
      isto(env+4)=k
      cnt=k
      ndx0=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+6)
 3011 IF(.NOT.(env.GT.0)) GOTO 3010
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9505
       GOTO 9002
 9505  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       isto(env+2)=ndx
       isto(env+3)=ndx-ndx0
       env=isto(env+0)
       GOTO 3011
 3010 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+5)=cnt-ndx0
      ndx0=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+8)
 3021 IF(.NOT.(env.GT.0)) GOTO 3020
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9506
       GOTO 9002
 9506  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       isto(env+2)=ndx
       isto(env+3)=ndx-ndx0
       env=isto(env+0)
       GOTO 3021
 3020 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+7)=cnt-ndx0
      ndx0=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+10)
 3031 IF(.NOT.(env.GT.0)) GOTO 3030
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9507
       GOTO 9002
 9507  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       isto(env+2)=ndx
       isto(env+3)=ndx-ndx0
       env=isto(env+0)
       GOTO 3031
 3030 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+9)=cnt-ndx0
      ndx0=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+12)
 3041 IF(.NOT.(env.GT.0)) GOTO 3040
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9508
       GOTO 9002
 9508  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       isto(env+2)=ndx
       isto(env+3)=ndx-ndx0
       env=isto(env+0)
       GOTO 3041
 3040 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+11)=cnt-ndx0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
                 ndx5=isto(env+4)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx6
                 ndx6=ndx0
      ndx0=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+24)
 3051 IF(.NOT.(env.GT.0)) GOTO 3050
       ndx1=cnt
       isto(env+1)=ndx1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+3)
 3061  IF(.NOT.(env.GT.0)) GOTO 3060
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9509
        GOTO 9008
 9509   from=isto(sptr)
        sptr=sptr+1
        isto(env+1)=ndx
        isto(env+2)=ndx-ndx0
        isto(env+3)=ndx-ndx1
        env=isto(env+0)
        GOTO 3061
 3060  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3051
 3050 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx0=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+32)
 3071 IF(.NOT.(env.GT.0)) GOTO 3070
       ndx1=cnt
       isto(env+1)=ndx1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+3)
 3081  IF(.NOT.(env.GT.0)) GOTO 3080
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9510
        GOTO 9009
 9510   from=isto(sptr)
        sptr=sptr+1
        isto(env+3)=ndx
        isto(env+4)=ndx-ndx0
        isto(env+5)=ndx-ndx1
        env=isto(env+0)
        GOTO 3081
 3080  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       env=isto(env+0)
       GOTO 3071
 3070 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      isto(env+0)=cnt
      ndx0=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9002 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
                 ndx3=isto(env+0)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9511
      GOTO 9005
 9511 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.EQ.0)) GOTO 5001
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9512
       GOTO 9006
 9512  from=isto(sptr)
       sptr=sptr+1
       GOTO 5000
 5001 CONTINUE
 5000 CONTINUE
      IF(.NOT.(flg.GT.0)) GOTO 5011
       stpflg=46
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5010
 5011 CONTINUE
 5010 CONTINUE
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9513
      GOTO 9003
 9513 from=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.GT.0)) GOTO 5021
       IF(.NOT.(ndx1.LE.ndx0)) GOTO 5031
        stpflg=48
        stpl1=ndx3
        stpc1=ndx4
        CONTINUE
        CALL errstp
        GOTO 5030
 5031  CONTINUE
        ndx=ndx1
 5030  CONTINUE
       GOTO 5020
 5021 CONTINUE
       cnt=cnt+1
       ndx=cnt
       iptr=iptr+2
       isto(iptr-1)=ndx3
       isto(iptr  )=ndx4
 5020 CONTINUE
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
 9003 CONTINUE
 3091 IF(.NOT.(ndx1.LT.ndx2
     1.AND. flg.EQ.0)) GOTO 3090
       ndx1=ndx1+1
       ndx0=ndx0+2
       IF(.NOT.(isto(ndx0).EQ.ndx3)) GOTO 5041
        j=0
        i2=isto(ndx0+1)
 3101   IF(.NOT.(j.LT.ndx3
     1  .AND. csto(i2+j).EQ.csto(ndx4+j)
     2  )) GOTO 3100
         j=j+1
         GOTO 3101
 3100   CONTINUE
        IF(.NOT.(j.EQ.ndx3)) GOTO 5051
         flg=1
         GOTO 5050
 5051   CONTINUE
 5050   CONTINUE
        GOTO 5040
 5041  CONTINUE
 5040  CONTINUE
       GOTO 3091
 3090 CONTINUE
      GOTO 9500
 9004 CONTINUE
 3111 IF(.NOT.(ndx1.LT.ndx2 .AND. flg.EQ.0)) GOTO 3110
       ndx1=ndx1+1
       ndx0=ndx0+2
       IF(.NOT.(isto(ndx0+1).EQ.ndx3)) GOTO 5061
        j=0
        i2=isto(ndx0)
 3121   IF(.NOT.(j.LT.ndx3
     1  .AND. csto(i2+j).EQ.csto(ndx4+j)
     2  )) GOTO 3120
         j=j+1
         GOTO 3121
 3120   CONTINUE
        IF(.NOT.(j.EQ.ndx3)) GOTO 5071
         flg=1
         GOTO 5070
 5071   CONTINUE
 5070   CONTINUE
        GOTO 5060
 5061  CONTINUE
 5060  CONTINUE
       GOTO 3111
 3110 CONTINUE
      GOTO 9500
 9005 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=1
      ndx0=base1 +isto(env+7) -2
      ndx2=isto(env+6)
      env=isto(sptr)
      sptr=sptr+1
      ndx1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9514
      GOTO 9004
 9514 from=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9006 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=1
      ndx0=base1 +isto(env+9) -2
      ndx2=isto(env+8)
      env=isto(sptr)
      sptr=sptr+1
      ndx1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9515
      GOTO 9004
 9515 from=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9007 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=1
      ndx0=base1 +isto(env+5) -2
      ndx2=isto(env+4)
      env=isto(sptr)
      sptr=sptr+1
      ndx1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9516
      GOTO 9004
 9516 from=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9008 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
                 ndx3=isto(env+4)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+5)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9517
      GOTO 9005
 9517 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.EQ.0)) GOTO 5081
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9518
       GOTO 9006
 9518  from=isto(sptr)
       sptr=sptr+1
       GOTO 5080
 5081 CONTINUE
 5080 CONTINUE
      IF(.NOT.(flg.EQ.0)) GOTO 5091
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9519
       GOTO 9007
 9519  from=isto(sptr)
       sptr=sptr+1
       GOTO 5090
 5091 CONTINUE
 5090 CONTINUE
      IF(.NOT.(flg.GT.0)) GOTO 5101
       stpflg=49
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5100
 5101 CONTINUE
 5100 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=ptr1+2*ndx5-2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=ndx5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
                 ndx2=ndx6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9520
      GOTO 9003
 9520 from=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.GT.0)) GOTO 5111
       stpflg=50
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5110
 5111 CONTINUE
 5110 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=ptr1+2*ndx1-2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
                 ndx2=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9521
      GOTO 9003
 9521 from=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.GT.0)) GOTO 5121
       stpflg=51
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5120
 5121 CONTINUE
 5120 CONTINUE
      cnt=cnt+1
      ndx=cnt
      iptr=iptr+2
      isto(iptr-1)=ndx3
      isto(iptr  )=ndx4
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
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
                 ndx3=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
                 ndx4=isto(env+2)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9522
      GOTO 9005
 9522 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.EQ.0)) GOTO 5131
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9523
       GOTO 9006
 9523  from=isto(sptr)
       sptr=sptr+1
       GOTO 5130
 5131 CONTINUE
 5130 CONTINUE
      IF(.NOT.(flg.EQ.0)) GOTO 5141
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9524
       GOTO 9007
 9524  from=isto(sptr)
       sptr=sptr+1
       GOTO 5140
 5141 CONTINUE
 5140 CONTINUE
      IF(.NOT.(flg.GT.0)) GOTO 5151
       stpflg=47
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5150
 5151 CONTINUE
 5150 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=ptr1+2*ndx1-2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
                 ndx2=cnt
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9525
      GOTO 9003
 9525 from=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.GT.0)) GOTO 5161
       stpflg=52
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5160
 5161 CONTINUE
 5160 CONTINUE
      cnt=cnt+1
      ndx=cnt
      isto(iptr+1)=ndx3
      isto(iptr+2)=ndx4
      iptr=iptr+2
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
      isto(sptr)=ptr1
                 ptr1=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                 ptr2=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
       ndx2=isto(env+4) +isto(env+5)
     1       +isto(env+7) +isto(env+9)
     2       +isto(env+11)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt
                 cnt=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+36)
      IF(.NOT.(env.GT.0)) GOTO 5781
       GOTO 5780
 5781 CONTINUE
 5780 CONTINUE
 3131 IF(.NOT.(env.GT.0)) GOTO 3130
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9526
       GOTO 9011
 9526  from=isto(sptr)
       sptr=sptr+1
       isto(env+2)=ndx
       env=isto(env+0)
       GOTO 3131
 3130 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+37)=cnt
      IF(.NOT.(cnt.EQ.0)) GOTO 5171
       isto(env+38)=0
       GOTO 5170
 5171 CONTINUE
       isto(env+38)=ptr2
 5170 CONTINUE
      ndx=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9011 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx4
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+3)
      ndx3=isto(env+0)
      ndx4=isto(env+1)
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=ptr1-2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9527
      GOTO 9003
 9527 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(flg.EQ.0)) GOTO 5181
       stpflg=53
       stpl1=ndx3
       stpc1=ndx4
       CONTINUE
       CALL errstp
       GOTO 5180
 5181 CONTINUE
 5180 CONTINUE
      ndx=ndx1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      i0=ptr2-1
      i=0
      flg=0
 3141 IF(.NOT.(i.LT.cnt .AND. flg.EQ.0)) GOTO 3140
       i=i+1
       i0=i0+3
       IF(.NOT.(isto(i0).EQ.ndx)) GOTO 5191
        flg=1
        GOTO 5190
 5191  CONTINUE
 5190  CONTINUE
       GOTO 3141
 3140 CONTINUE
      IF(.NOT.(flg.EQ.0)) GOTO 5201
       cnt=cnt+1
       isto(iptr+1)=ndx3
       isto(iptr+2)=ndx4
       isto(iptr+3)=ndx
       iptr=iptr+3
       GOTO 5200
 5201 CONTINUE
 5200 CONTINUE
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9012 CONTINUE
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
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt8
                 cnt8=isto(env+4)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt9
                 cnt9=cnt8+isto(env+5)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt10
                 cnt10=cnt9+isto(env+7)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt11
                 cnt11=cnt10+isto(env+9)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt12
                 cnt12=cnt11+isto(env+11)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=cnt13
                 cnt13=cnt12+isto(env+23)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr2
                 ptr2=isto(env+1)
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
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
      isto(sptr)=from
      from=9528
      GOTO 9013
 9528 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9529
      GOTO 9014
 9529 from=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx3=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      cnt13=isto(sptr)
      sptr=sptr+1
      cnt12=isto(sptr)
      sptr=sptr+1
      cnt11=isto(sptr)
      sptr=sptr+1
      cnt10=isto(sptr)
      sptr=sptr+1
      cnt9=isto(sptr)
      sptr=sptr+1
      cnt8=isto(sptr)
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
 9013 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+3)
 3151 IF(.NOT.(env.GT.0)) GOTO 3150
       IF(.NOT.(isto(env+13).NE.7
     1 )) GOTO 5211
        ndx3=isto(env+7)
        ndx4=isto(env+8)
        ndx=0
        flg=0
        ndx5=isto(env+4)
        k=isto(env+6)
        IF(.NOT.(k.GT.0)) GOTO 5221
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=env
                    env=k
         cnt1=isto(env+4)
         cnt2=cnt1+isto(env+5)
         cnt3=cnt2+isto(env+7)
         cnt4=cnt3+isto(env+9)
         cnt5=cnt4+isto(env+11)
         ptr1=isto(env+1)
         env=isto(sptr)
         sptr=sptr+1
         IF(.NOT.(ndx5.EQ.44
     1   .OR. ndx5.EQ.45
     2   .OR. ndx5.EQ.52)) GOTO 5231
          ndx0=ptr1-2
          ndx1=0
          ndx2=cnt1
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9530
          GOTO 9003
 9530     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5241
           ndx=ndx1
           GOTO 5240
 5241     CONTINUE
 5240     CONTINUE
          GOTO 5230
 5231    CONTINUE
 5230    CONTINUE
         IF(.NOT.(flg.EQ.0
     1   .AND. (ndx5.EQ.18
     2    .OR. (ndx5.GE.44 .AND. ndx5.LE.57))
     3   )) GOTO 5251
          ndx0=ptr1+2*cnt2-2
          ndx1=cnt2
          ndx2=cnt3
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9531
          GOTO 9003
 9531     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5261
           ndx=ndx1
           GOTO 5260
 5261     CONTINUE
 5260     CONTINUE
          GOTO 5250
 5251    CONTINUE
 5250    CONTINUE
         IF(.NOT.(flg.EQ.0 .AND.
     1    (ndx5.EQ.44 .OR. ndx5.EQ.45
     2     .OR. ndx5.EQ.47 .OR. ndx5.EQ.52
     3     .OR. ndx5.EQ.53 .OR. ndx5.EQ.54)
     4   )) GOTO 5271
          ndx0=ptr1+2*cnt3-2
          ndx1=cnt3
          ndx2=cnt4
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9532
          GOTO 9003
 9532     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5281
           ndx=ndx1
           GOTO 5280
 5281     CONTINUE
 5280     CONTINUE
          GOTO 5270
 5271    CONTINUE
 5270    CONTINUE
         IF(.NOT.(flg.EQ.0 .AND. ndx5.EQ.52)) GOTO 5291
          ndx0=ptr1+2*cnt4-2
          ndx1=cnt4
          ndx2=cnt5
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9533
          GOTO 9003
 9533     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5301
           ndx=ndx1
           GOTO 5300
 5301     CONTINUE
 5300     CONTINUE
          GOTO 5290
 5291    CONTINUE
 5290    CONTINUE
         IF(.NOT.(flg.GT.0)) GOTO 5311
          isto(env+5)=isto(env+6)
          isto(env+1)=ndx
          IF(.NOT.(ndx.GT.cnt4)) GOTO 5321
           ndx=ndx-cnt4
           k=7
           GOTO 5320
 5321     IF(.NOT.(ndx.GT.cnt3)) GOTO 5322
           ndx=ndx-cnt3
           k=6
           GOTO 5320
 5322     IF(.NOT.(ndx.GT.cnt2)) GOTO 5323
           ndx=ndx-cnt2
           k=5
           GOTO 5320
 5323     CONTINUE
           k=3
 5320     CONTINUE
          isto(env+3)=ndx
          isto(env+2)=k
          GOTO 5310
 5311    CONTINUE
 5310    CONTINUE
         GOTO 5220
 5221   CONTINUE
 5220   CONTINUE
        IF(.NOT.(flg.EQ.0)) GOTO 5331
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=env
                    env=1
         ndx0=base1 +isto(env+9) -2
         ndx2=9+isto(env+8)
         env=isto(sptr)
         sptr=sptr+1
         ndx1=9
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9534
         GOTO 9004
 9534    from=isto(sptr)
         sptr=sptr+1
         IF(.NOT.(flg.EQ.0 .AND.
     1   ((ndx5.GE.2 .AND. ndx5.LE.5)
     2    .OR. ndx5.EQ.13 .OR. ndx5.EQ.15
     3    .OR. ndx5.EQ.20 .OR. ndx5.EQ.21
     4    .OR. ndx5.EQ.25
     5    .OR. (ndx5.GE.27 .AND. ndx5.LE.45)
     6    .OR. ndx5.EQ.47 .OR. ndx5.EQ.48
     7    .OR. ndx5.EQ.52 .OR. ndx5.EQ.53
     8    .OR. ndx5.EQ.54
     9    .OR. (ndx5.GE.59 .AND. ndx5.LE.66
     9        .AND. ndx5.NE.63))
     9   )) GOTO 5341
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=env
                     env=1
          ndx0=base1 +isto(env+7) -2
          ndx2=isto(env+6)
          env=isto(sptr)
          sptr=sptr+1
          ndx1=0
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9535
          GOTO 9004
 9535     from=isto(sptr)
          sptr=sptr+1
          GOTO 5340
 5341    CONTINUE
 5340    CONTINUE
         IF(.NOT.(flg.GT.0)) GOTO 5351
          isto(env+1)=-ndx1
          IF(.NOT.(ndx1.GT.9)) GOTO 5361
           ndx1=ndx1-9
           k=2
           GOTO 5360
 5361     CONTINUE
           k=1
 5360     CONTINUE
          isto(env+3)=ndx1
          isto(env+2)=k
          GOTO 5350
 5351    CONTINUE
 5350    CONTINUE
         GOTO 5330
 5331   CONTINUE
 5330   CONTINUE
        IF(.NOT.(flg.EQ.0)) GOTO 5371
         cnt1=cnt8
         cnt2=cnt9
         cnt3=cnt10
         cnt4=cnt11
         cnt5=cnt12
         cnt6=cnt13
         ptr1=ptr2
         IF(.NOT.((ndx5.EQ.2 .OR. ndx5.EQ.3)
     1   .AND. isto(env+12).GT.0
     2   )) GOTO 5381
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=env
                     env=isto(env+12)
          ndx0=ptr1 +2*isto(env+1) -2
          ndx1=isto(env+1)
          ndx2=ndx1+isto(env+2)
          env=isto(sptr)
          sptr=sptr+1
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9536
          GOTO 9003
 9536     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5391
           ndx=ndx1
           GOTO 5390
 5391     CONTINUE
 5390     CONTINUE
          GOTO 5380
 5381    CONTINUE
 5380    CONTINUE
         IF(.NOT.(flg.EQ.0
     1   .AND. isto(env+11).GT.0 .AND.
     2    (ndx5.EQ.4 .OR. ndx5.EQ.5
     3    .OR. (ndx5.GE.25 .AND. ndx5.LE.45)
     4    .OR. ndx5.EQ.47 .OR. ndx5.EQ.48
     5    .OR. ndx5.EQ.53 .OR. ndx5.EQ.54
     6    .OR. (ndx5.GE.59 .AND. ndx5.LE.66
     7        .AND. ndx5.NE.63))
     8   )) GOTO 5401
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=env
                     env=isto(env+11)
          ndx0=ptr1 +2*isto(env+1) -2
          ndx1=isto(env+1)
          ndx2=ndx1+isto(env+2)
          env=isto(sptr)
          sptr=sptr+1
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9537
          GOTO 9003
 9537     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5411
           ndx=ndx1
           GOTO 5410
 5411     CONTINUE
 5410     CONTINUE
          GOTO 5400
 5401    CONTINUE
 5400    CONTINUE
         IF(.NOT.(flg.EQ.0 .AND.
     1    (ndx5.EQ.4 .OR. ndx5.EQ.5
     2     .OR. ndx5.EQ.13 .OR. ndx5.EQ.15
     3     .OR. ndx5.EQ.20 .OR. ndx5.EQ.21
     4     .OR. ndx5.EQ.25
     5     .OR. (ndx5.GE.27 .AND. ndx5.LE.43)
     6     .OR. ndx5.EQ.47 .OR. ndx5.EQ.53
     7     .OR. ndx5.EQ.54
     9     .OR. (ndx5.GE.59 .AND. ndx5.LE.66
     9         .AND. ndx5.NE.63))
     9   )) GOTO 5421
          ndx0=ptr1-2
          ndx1=0
          ndx2=cnt1
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9538
          GOTO 9003
 9538     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5431
           ndx=ndx1
           GOTO 5430
 5431     CONTINUE
 5430     CONTINUE
          GOTO 5420
 5421    CONTINUE
 5420    CONTINUE
         IF(.NOT.(flg.EQ.0
     1   .AND. ndx5.GE.4 .AND. ndx5.LE.66
     2   )) GOTO 5441
          ndx0=ptr1+2*cnt1-2
          ndx1=cnt1
          ndx2=cnt2
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9539
          GOTO 9003
 9539     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5451
           ndx=ndx1
           GOTO 5450
 5451     CONTINUE
 5450     CONTINUE
          GOTO 5440
 5441    CONTINUE
 5440    CONTINUE
         IF(.NOT.(flg.EQ.0 .AND.
     1    ((ndx5.GE.4 .AND. ndx5.LE.6)
     2     .OR. (ndx5.GE.9 .AND. ndx5.LE.66))
     3   )) GOTO 5461
          ndx0=ptr1+2*cnt2-2
          ndx1=cnt2
          ndx2=cnt3
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9540
          GOTO 9003
 9540     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5471
           ndx=ndx1
           GOTO 5470
 5471     CONTINUE
 5470     CONTINUE
          GOTO 5460
 5461    CONTINUE
 5460    CONTINUE
         IF(.NOT.(flg.EQ.0 .AND.
     1    (ndx5.EQ.4 .OR. ndx5.EQ.5
     2     .OR. ndx5.EQ.13 .OR. ndx5.EQ.15
     3     .OR. ndx5.EQ.20 .OR. ndx5.EQ.21
     4     .OR. (ndx5.GE.25 .AND. ndx5.LE.45)
     5     .OR. ndx5.EQ.47 .OR. ndx5.EQ.48
     6     .OR. ndx5.EQ.52 .OR. ndx5.EQ.53
     7     .OR. ndx5.EQ.54
     8     .OR. (ndx5.GE.59 .AND. ndx5.LE.66
     9         .AND. ndx5.NE.63))
     9   )) GOTO 5481
          ndx0=ptr1+2*cnt3-2
          ndx1=cnt3
          ndx2=cnt4
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9541
          GOTO 9003
 9541     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5491
           ndx=ndx1
           GOTO 5490
 5491     CONTINUE
 5490     CONTINUE
          GOTO 5480
 5481    CONTINUE
 5480    CONTINUE
         IF(.NOT.(flg.EQ.0 .AND.
     1    (ndx5.EQ.4 .OR. ndx5.EQ.5
     2     .OR. (ndx5.GE.25 .AND. ndx5.LE.45)
     3     .OR. ndx5.EQ.47 .OR. ndx5.EQ.48
     4     .OR. ndx5.EQ.52 .OR. ndx5.EQ.53
     5     .OR. ndx5.EQ.54
     6     .OR. (ndx5.GE.59 .AND. ndx5.LE.66
     7         .AND. ndx5.NE.63))
     8   )) GOTO 5501
          ndx0=ptr1+2*cnt4-2
          ndx1=cnt4
          ndx2=cnt5
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9542
          GOTO 9003
 9542     from=isto(sptr)
          sptr=sptr+1
          IF(.NOT.(flg.GT.0)) GOTO 5511
           ndx=ndx1
           GOTO 5510
 5511     CONTINUE
 5510     CONTINUE
          GOTO 5500
 5501    CONTINUE
 5500    CONTINUE
         IF(.NOT.(flg.GT.0)) GOTO 5521
          isto(env+1)=ndx1
          IF(.NOT.(ndx.GT.cnt6)) GOTO 5531
           ndx=ndx-cnt6
           k=9
           GOTO 5530
 5531     IF(.NOT.(ndx.GT.cnt5)) GOTO 5532
           ndx=ndx-cnt5
           k=8
           GOTO 5530
 5532     IF(.NOT.(ndx.GT.cnt4)) GOTO 5533
           ndx=ndx-cnt4
           k=7
           GOTO 5530
 5533     IF(.NOT.(ndx.GT.cnt3)) GOTO 5534
           ndx=ndx-cnt3
           k=6
           GOTO 5530
 5534     IF(.NOT.(ndx.GT.cnt2)) GOTO 5535
           ndx=ndx-cnt2
           k=5
           GOTO 5530
 5535     IF(.NOT.(ndx.GT.cnt1)) GOTO 5536
           ndx=ndx-cnt1
           k=4
           GOTO 5530
 5536     CONTINUE
           k=3
 5530     CONTINUE
          isto(env+3)=ndx
          isto(env+2)=k
          GOTO 5520
 5521    CONTINUE
 5520    CONTINUE
         GOTO 5370
 5371   CONTINUE
 5370   CONTINUE
        IF(.NOT.(flg.EQ.0)) GOTO 5541
         stpflg=54
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
         GOTO 5540
 5541   CONTINUE
 5540   CONTINUE
        GOTO 5210
 5211  CONTINUE
 5210  CONTINUE
       env=isto(env+0)
       GOTO 3151
 3150 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9014 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+2)
 3161 IF(.NOT.(env.GT.0)) GOTO 3160
       ndx3=isto(env+6)
       ndx4=isto(env+7)
       ndx5=isto(env+1)
       ndx=0
       flg=0
       k=isto(env+5)
       IF(.NOT.(k.NE.datenv)) GOTO 5551
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=k
        cnt1=isto(env+4)
        cnt2=cnt1+isto(env+5)
        cnt3=cnt2+isto(env+7)
        cnt4=cnt3+isto(env+9)
        env=isto(sptr)
        sptr=sptr+1
        GOTO 5550
 5551  CONTINUE
        cnt1=cnt8
        cnt2=cnt9
        cnt3=cnt10
        cnt4=cnt11
 5550  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=k
       IF(.NOT.(ndx5.EQ.18)) GOTO 5561
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9543
        GOTO 9015
 9543   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(flg.EQ.0)) GOTO 5571
         stpflg=55
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
         GOTO 5570
 5571   CONTINUE
 5570   CONTINUE
        GOTO 5560
 5561  IF(.NOT.(ndx5.EQ.19)) GOTO 5562
        flg=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9544
        GOTO 9016
 9544   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(flg.EQ.0)) GOTO 5581
         stpflg=56
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
         GOTO 5580
 5581   CONTINUE
 5580   CONTINUE
        GOTO 5560
 5562  IF(.NOT.(ndx5.EQ.7
     1 .OR. ndx5.EQ.16
     2 .OR. ndx5.EQ.20)) GOTO 5563
        flg=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9545
        GOTO 9017
 9545   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(flg.EQ.0)) GOTO 5591
         IF(.NOT.(ndx5.EQ.16)) GOTO 5601
          stpflg=57
          GOTO 5600
 5601    IF(.NOT.(ndx5.EQ.7)) GOTO 5602
          stpflg=58
          GOTO 5600
 5602    IF(.NOT.(ndx5.EQ.20)) GOTO 5603
          stpflg=72
          GOTO 5600
 5603    CONTINUE
 5600    CONTINUE
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
         GOTO 5590
 5591   CONTINUE
 5590   CONTINUE
        GOTO 5560
 5563  IF(.NOT.(ndx5.EQ.8
     1 .OR. ndx5.EQ.13
     2 .OR. ndx5.EQ.14
     3 .OR. (ndx5.EQ.17
     4     .AND. isto(ishenv+16).GT.0)
     5 )) GOTO 5564
        flg=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9546
        GOTO 9018
 9546   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(flg.EQ.0)) GOTO 5611
         IF(.NOT.(ndx5.EQ.8)) GOTO 5621
          stpflg=59
          GOTO 5620
 5621    IF(.NOT.(ndx5.EQ.13)) GOTO 5622
          stpflg=60
          GOTO 5620
 5622    IF(.NOT.(ndx5.EQ.14)) GOTO 5623
          stpflg=61
          GOTO 5620
 5623    IF(.NOT.(ndx5.EQ.17)) GOTO 5624
          stpflg=62
          GOTO 5620
 5624    CONTINUE
 5620    CONTINUE
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
         GOTO 5610
 5611   CONTINUE
 5610   CONTINUE
        GOTO 5560
 5564  IF(.NOT.(ndx5.EQ.17)) GOTO 5565
        flg=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9550
        GOTO 9021
 9550   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(flg.EQ.0)) GOTO 5841
         stpflg=126
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
         GOTO 5840
 5841   CONTINUE
 5840   CONTINUE
        GOTO 5560
 5565  CONTINUE
        flg=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9547
        GOTO 9017
 9547   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(flg.EQ.0)) GOTO 5631
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9548
         GOTO 9018
 9548    from=isto(sptr)
         sptr=sptr+1
         GOTO 5630
 5631   CONTINUE
 5630   CONTINUE
        IF(.NOT.(flg.EQ.0)) GOTO 5641
         IF(.NOT.(ndx5.EQ.1)) GOTO 5651
          stpflg=63
          GOTO 5650
 5651    IF(.NOT.(ndx5.EQ.2)) GOTO 5652
          stpflg=64
          GOTO 5650
 5652    IF(.NOT.(ndx5.EQ.3)) GOTO 5653
          stpflg=65
          GOTO 5650
 5653    IF(.NOT.(ndx5.EQ.9)) GOTO 5654
          stpflg=66
          GOTO 5650
 5654    IF(.NOT.(ndx5.EQ.10)) GOTO 5655
          stpflg=67
          GOTO 5650
 5655    IF(.NOT.(ndx5.EQ.11)) GOTO 5656
          stpflg=68
          GOTO 5650
 5656    IF(.NOT.(ndx5.EQ.21)) GOTO 5657
          stpflg=69
          GOTO 5650
 5657    IF(.NOT.(ndx5.EQ.15)) GOTO 5658
          stpflg=70
          GOTO 5650
 5658    IF(.NOT.(ndx5.EQ.12)) GOTO 5659
          stpflg=71
          GOTO 5650
 5659    CONTINUE
 5650    CONTINUE
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
         GOTO 5640
 5641   CONTINUE
 5640   CONTINUE
 5560  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       isto(env+2)=ndx
       IF(.NOT.(ndx.GT.cnt4)) GOTO 5661
        ndx=ndx-cnt4
        k=7
        GOTO 5660
 5661  IF(.NOT.(ndx.GT.cnt3)) GOTO 5662
        ndx=ndx-cnt3
        k=6
        GOTO 5660
 5662  IF(.NOT.(ndx.GT.cnt2)) GOTO 5663
        ndx=ndx-cnt2
        k=5
        GOTO 5660
 5663  IF(.NOT.(ndx.GT.cnt1)) GOTO 5664
        ndx=ndx-cnt1
        k=4
        GOTO 5660
 5664  CONTINUE
        k=3
 5660  CONTINUE
       isto(env+4)=ndx
       isto(env+3)=k
       env=isto(env+0)
       GOTO 3161
 3160 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9015 CONTINUE
      k=isto(env+37)
      i=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+38)
 3171 IF(.NOT.(i.LT.k .AND. flg.EQ.0)) GOTO 3170
       i=i+1
       IF(.NOT.(isto(env+0).EQ.ndx3)) GOTO 5791
        j=0
        i2=isto(env+1)
 3241   IF(.NOT.(j.LT.ndx3
     1  .AND. csto(i2+j).EQ.csto(ndx4+j)
     2  )) GOTO 3240
         j=j+1
         GOTO 3241
 3240   CONTINUE
         IF(.NOT.(j.EQ.ndx3)) GOTO 5671
         ndx=isto(env+2)
         flg=1
         GOTO 5670
 5671   CONTINUE
 5670   CONTINUE
        GOTO 5790
 5791  CONTINUE
 5790  CONTINUE
       env=env+3
       GOTO 3171
 3170 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9016 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+8)
 3181 IF(.NOT.(env.GT.0 .AND. flg.EQ.0)) GOTO 3180
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       IF(.NOT.(isto(env+0).EQ.ndx3)) GOTO 5681
        j=0
        i2=isto(env+1)
 3251   IF(.NOT.(j.LT.ndx3
     1  .AND. csto(i2+j).EQ.csto(ndx4+j)
     2  )) GOTO 3250
         j=j+1
         GOTO 3251
 3250   CONTINUE
         IF(.NOT.(j.EQ.ndx3)) GOTO 5801
         flg=1
         GOTO 5800
 5801   CONTINUE
 5800   CONTINUE
        GOTO 5680
 5681  CONTINUE
 5680  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(flg.GT.0)) GOTO 5691
        ndx=isto(env+2)
        GOTO 5690
 5691  CONTINUE
 5690  CONTINUE
       env=isto(env+0)
       GOTO 3181
 3180 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9017 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+10)
 3191 IF(.NOT.(env.GT.0 .AND. flg.EQ.0)) GOTO 3190
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       IF(.NOT.(isto(env+0).EQ.ndx3)) GOTO 5701
        j=0
        i2=isto(env+1)
 3261   IF(.NOT.(j.LT.ndx3
     1  .AND. csto(i2+j).EQ.csto(ndx4+j)
     2  )) GOTO 3260
         j=j+1
         GOTO 3261
 3260   CONTINUE
         IF(.NOT.(j.EQ.ndx3)) GOTO 5811
         flg=1
         GOTO 5810
 5811   CONTINUE
 5810   CONTINUE
        GOTO 5700
 5701  CONTINUE
 5700  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(flg.GT.0)) GOTO 5711
        ndx=isto(env+2)
        GOTO 5710
 5711  CONTINUE
 5710  CONTINUE
       env=isto(env+0)
       GOTO 3191
 3190 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9018 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+12)
 3201 IF(.NOT.(env.GT.0 .AND. flg.EQ.0)) GOTO 3200
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=isto(env+4)
       IF(.NOT.(isto(env+0).EQ.ndx3)) GOTO 5721
        j=0
        i2=isto(env+1)
 3271   IF(.NOT.(j.LT.ndx3
     1  .AND. csto(i2+j).EQ.csto(ndx4+j)
     2  )) GOTO 3270
         j=j+1
         GOTO 3271
 3270   CONTINUE
         IF(.NOT.(j.EQ.ndx3)) GOTO 5821
         flg=1
         GOTO 5820
 5821   CONTINUE
 5820   CONTINUE
        GOTO 5720
 5721  CONTINUE
 5720  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(flg.GT.0)) GOTO 5731
        ndx=isto(env+2)
        GOTO 5730
 5731  CONTINUE
 5730  CONTINUE
       env=isto(env+0)
       GOTO 3201
 3200 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9019 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+31)
 3211 IF(.NOT.(env.GT.0)) GOTO 3210
       IF(.NOT.(isto(env+1).GT.0)) GOTO 5741
        isto(env+4)=0
        isto(env+5)=0
        GOTO 5740
 5741  CONTINUE
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
        isto(sptr)=flg
                   flg=0
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ndx1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=ptr1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9549
        GOTO 9020
 9549   from=isto(sptr)
        sptr=sptr+1
        IF(.NOT.(flg.GT.0)) GOTO 5751
         isto(env+4)=ndx1
         isto(env+5)=ptr1
         GOTO 5750
 5751   CONTINUE
         stpflg=73
         stpl1=ndx3
         stpc1=ndx4
         CONTINUE
         CALL errstp
 5750   CONTINUE
        ptr1=isto(sptr)
        sptr=sptr+1
        ndx1=isto(sptr)
        sptr=sptr+1
        flg=isto(sptr)
        sptr=sptr+1
        ndx4=isto(sptr)
        sptr=sptr+1
        ndx3=isto(sptr)
        sptr=sptr+1
 5740  CONTINUE
       env=isto(env+0)
       GOTO 3211
 3210 CONTINUE
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
                  env=isto(env+30)
 3221 IF(.NOT.(env.GT.0 .AND. flg.EQ.0)) GOTO 3220
       IF(.NOT.(isto(env+2).EQ.ndx3)) GOTO 5761
        i=0
        i2=isto(env+3)
 3231   IF(.NOT.(i.LT.ndx3
     1  .AND. csto(i2+i).EQ.csto(ndx4+i)
     2  )) GOTO 3230
         i=i+1
         GOTO 3231
 3230   CONTINUE
        IF(.NOT.(i.EQ.ndx3)) GOTO 5771
         flg=1
         ndx1=isto(env+4)
         ptr1=isto(env+5)
         GOTO 5770
 5771   CONTINUE
 5770   CONTINUE
        GOTO 5760
 5761  CONTINUE
 5760  CONTINUE
       env=isto(env+0)
       GOTO 3221
 3220 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9021 CONTINUE
      i0=isto(env+4)+isto(env+9)
      i1=isto(env+1)+2*(i0-1)
      i3=isto(env+11)
      i=0
 3281 IF(.NOT.(i.LT.i3)) GOTO 3280
       i=i+1
       i1=i1+2
       IF(.NOT.(isto(i1).EQ.ndx3)) GOTO 5851
        j=0
        i2=isto(i1+1)
 3291   IF(.NOT.(j.LT.ndx3
     1  .AND. csto(i2+j).EQ.csto(ndx4+j)
     2  )) GOTO 3290
         j=j+1
         GOTO 3291
 3290   CONTINUE
         IF(.NOT.(j.EQ.ndx3)) GOTO 5861
         flg=1
         ndx=i0+i
         GOTO 5860
 5861   CONTINUE
 5860   CONTINUE
        GOTO 5850
 5851  CONTINUE
 5850  CONTINUE
       GOTO 3281
 3280 CONTINUE
      GOTO 9500
      END
