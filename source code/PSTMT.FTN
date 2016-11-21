      SUBROUTINE pstmt
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
     1  9501,  9502,  9503,  9504,  9505,
     1  9506,  9507,  9508,  9509,  9510,
     1  9511,  9512,  9513,  9514,  9515,
     1  9516,  9517,  9518,  9519,  9520,
     1  9521,  9522,  9523,  9524,  0002,
     1  9526,  9527,  9528,  9529,  9530,
     1  9531,  9532,  9533,  9534,  9535,
     1  9536,  9537,  9538,  9539,  9540,
     1  9541,  9542,  9543,  9544), from- 9500
 0002 STOP 'Invalid from reference in pstmt.'
  910 stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      CONTINUE
      CALL errstp
 9000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9501
      GOTO 9001
 9501 from=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(mflg.EQ.0)) GOTO 5001
       ptr1=iptr+1
       GOTO 5000
 5001 CONTINUE
       ptr1=0
 5000 CONTINUE
 3001 IF(.NOT.(mflg.EQ.0)) GOTO 3000
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9502
       GOTO 9002
 9502  from=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9503
       GOTO 9001
 9503  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.EQ.0)) GOTO 5011
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5010
 5011  CONTINUE
        isto(env+0)=0
 5010  CONTINUE
       GOTO 3001
 3000 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9001 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=dptr
      IF(.NOT.(isto(ishenv+53).EQ.1)) GOTO 5021
       mndx=169
       to=9206
       CALL putil2
       GOTO 5020
 5021 IF(.NOT.(isto(ishenv+53).EQ.2)) GOTO 5022
       mndx=173
       to=9206
       CALL putil2
       GOTO 5020
 5022 IF(.NOT.(isto(ishenv+53).EQ.3)) GOTO 5023
       mndx=207
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5031
        mndx=205
        to=9206
        CALL putil2
        IF(.NOT.(mflg.EQ.0)) GOTO 5041
         mndx=203
         to=9206
         CALL putil2
         GOTO 5040
 5041   CONTINUE
 5040   CONTINUE
        GOTO 5030
 5031  CONTINUE
 5030  CONTINUE
       GOTO 5020
 5023 IF(.NOT.(isto(ishenv+53).EQ.4)) GOTO 5024
       mndx=207
       to=9206
       CALL putil2
       GOTO 5020
 5024 IF(.NOT.(isto(ishenv+53).EQ.5)) GOTO 5025
       mndx=211
       to=9206
       CALL putil2
       GOTO 5020
 5025 IF(.NOT.(isto(ishenv+53).EQ.6)) GOTO 5026
       mndx=215
       to=9206
       CALL putil2
       GOTO 5020
 5026 IF(.NOT.(isto(ishenv+53).EQ.7)) GOTO 5027
       mndx=219
       to=9206
       CALL putil2
       GOTO 5020
 5027 IF(.NOT.(isto(ishenv+53).EQ.8)) GOTO 5028
       mndx=229
       to=9206
       CALL putil2
       GOTO 5020
 5028 IF(.NOT.(isto(ishenv+53).EQ.9)) GOTO 5029
       mndx=237
       to=9206
       CALL putil2
       GOTO 5020
 5029 CONTINUE
 5020 CONTINUE
      dptr=isto(sptr)
      sptr=sptr+1
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
      isto(sptr)=ptr1
                 ptr1=dptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      mndx=411
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5051
       mndx=83
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5061
        IF(.NOT.(isto(ishenv+52).GT.0
     1  )) GOTO 5071
         ndx=1
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9504
         GOTO 9003
 9504    from=isto(sptr)
         sptr=sptr+1
         GOTO 5070
 5071   CONTINUE
         isto(ishenv+54)=63
         CONTINUE
         CALL synstp
 5070   CONTINUE
        GOTO 5060
 5061  CONTINUE
 5060  CONTINUE
       GOTO 5050
 5051 CONTINUE
 5050 CONTINUE
      IF(.NOT.(ndx.EQ.0)) GOTO 5081
       dptr=ptr1
       mndx=247
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5091
        mndx=83
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5101
         ndx=15
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9505
         GOTO 9004
 9505    from=isto(sptr)
         sptr=sptr+1
         GOTO 5100
 5101   CONTINUE
 5100   CONTINUE
        GOTO 5090
 5091  CONTINUE
 5090  CONTINUE
       IF(.NOT.(ndx.EQ.0)) GOTO 5111
        dptr=ptr1
        mndx=195
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5121
         mndx=83
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5131
          IF(.NOT.(isto(ishenv+52).EQ.0
     1    )) GOTO 6251
           ndx=3
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=from
           from=9506
           GOTO 9014
 9506      from=isto(sptr)
           sptr=sptr+1
           GOTO 6250
 6251     CONTINUE
           isto(ishenv+54)=87
           CONTINUE
           CALL synstp
 6250     CONTINUE
          GOTO 5130
 5131    CONTINUE
 5130    CONTINUE
         GOTO 5120
 5121   CONTINUE
 5120   CONTINUE
        IF(.NOT.(ndx.EQ.0)) GOTO 5141
         dptr=ptr1
         mndx=197
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5151
          mndx=83
          to=9205
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5161
           IF(.NOT.(isto(ishenv+52).EQ.0
     1    )) GOTO 6261
            ndx=4
            sptr=sptr-1
            IF(sptr.EQ.iptr) GOTO 910
            isto(sptr)=from
            from=9507
            GOTO 9015
 9507       from=isto(sptr)
            sptr=sptr+1
            GOTO 6260
 6261      CONTINUE
            isto(ishenv+54)=88
            CONTINUE
            CALL synstp
 6260      CONTINUE
           GOTO 5160
 5161     CONTINUE
 5160     CONTINUE
          GOTO 5150
 5151    CONTINUE
 5150    CONTINUE
         IF(.NOT.(ndx.EQ.0)) GOTO 5171
          dptr=ptr1
          mndx=199
          to=9206
          CALL putil2
          IF(.NOT.(mflg.GT.0)) GOTO 5181
           ndx=5
           sptr=sptr-1
           IF(sptr.EQ.iptr) GOTO 910
           isto(sptr)=from
           from=9508
           GOTO 9017
 9508      from=isto(sptr)
           sptr=sptr+1
           GOTO 5180
 5181     CONTINUE
           mndx=209
           to=9206
           CALL putil2
           IF(.NOT.(mflg.GT.0)) GOTO 5191
            ndx=6
            sptr=sptr-1
            IF(sptr.EQ.iptr) GOTO 910
            isto(sptr)=from
            from=9509
            GOTO 9018
 9509       from=isto(sptr)
            sptr=sptr+1
            GOTO 5190
 5191      CONTINUE
            mndx=213
            to=9206
            CALL putil2
            IF(.NOT.(mflg.GT.0)) GOTO 5201
             ndx=7
             sptr=sptr-1
             IF(sptr.EQ.iptr) GOTO 910
             isto(sptr)=from
             from=9510
             GOTO 9019
 9510        from=isto(sptr)
             sptr=sptr+1
             GOTO 5200
 5201       CONTINUE
             mndx=217
             to=9206
             CALL putil2
             IF(.NOT.(mflg.GT.0)) GOTO 5211
              ndx=8
              sptr=sptr-1
              IF(sptr.EQ.iptr) GOTO 910
              isto(sptr)=from
              from=9511
              GOTO 9022
 9511         from=isto(sptr)
              sptr=sptr+1
              GOTO 5210
 5211        CONTINUE
              mndx=221
              to=9206
              CALL putil2
              IF(.NOT.(mflg.GT.0)) GOTO 5221
               ndx=9
               IF(.NOT.(isto(ishenv+63).EQ.0)) GOTO 6241
                isto(ishenv+54)=75
                CONTINUE
                CALL synstp
                GOTO 6240
 6241          CONTINUE
 6240          CONTINUE
               GOTO 5220
 5221         CONTINUE
               mndx=223
               to=9206
               CALL putil2
               IF(.NOT.(mflg.GT.0)) GOTO 5231
                ndx=10
                sptr=sptr-1
                IF(sptr.EQ.iptr) GOTO 910
                isto(sptr)=from
                from=9512
                GOTO 9023
 9512           from=isto(sptr)
                sptr=sptr+1
                GOTO 5230
 5231          CONTINUE
                mndx=227
                to=9206
                CALL putil2
                IF(.NOT.(mflg.GT.0)) GOTO 5241
                 ndx=11
                 sptr=sptr-1
                 IF(sptr.EQ.iptr) GOTO 910
                 isto(sptr)=from
                 from=9513
                 GOTO 9025
 9513            from=isto(sptr)
                 sptr=sptr+1
                 GOTO 5240
 5241           CONTINUE
                 mndx=231
                 to=9205
                 CALL putil2
                 IF(.NOT.(mflg.GT.0
     1           )) GOTO 5251
                  mndx=83
                  to=9205
                  CALL putil2
                  IF(.NOT.(mflg.GT.0
     1            )) GOTO 6311
                   ndx=12
                   sptr=sptr-1
                   IF(sptr.EQ.iptr) GOTO 910
                   isto(sptr)=from
                   from=9514
                   GOTO 9026
 9514              from=isto(sptr)
                   sptr=sptr+1
                   GOTO 6310
 6311             CONTINUE
 6310             CONTINUE
                  GOTO 5250
 5251            CONTINUE
 5250            CONTINUE
                 IF(.NOT.(ndx.EQ.0)) GOTO 6321
                  dptr=ptr1
                  mndx=235
                  to=9206
                  CALL putil2
                  IF(.NOT.(mflg.GT.0)) GOTO 5261
                   ndx=13
                    sptr=sptr-1
                    IF(sptr.EQ.iptr) GOTO 910
                    isto(sptr)=from
                    from=9544
                    GOTO 9028
 9544               from=isto(sptr)
                    sptr=sptr+1
                   GOTO 5260
 5261             CONTINUE
                   mndx=239
                   to=9206
                   CALL putil2
                   IF(.NOT.(mflg.GT.0)) GOTO 5271
                    IF(.NOT.(isto(ishenv+52).EQ.0
     1              )) GOTO 6271
                     ndx=14
                     CONTINUE
                     CALL puse
                     GOTO 6270
 6271               CONTINUE
                     isto(ishenv+54)=89
                     CONTINUE
                     CALL synstp
 6270               CONTINUE
                    GOTO 5270
 5271              CONTINUE
                    ndx=2
                    sptr=sptr-1
                    IF(sptr.EQ.iptr) GOTO 910
                    isto(sptr)=from
                    from=9515
                    GOTO 9005
 9515               from=isto(sptr)
                    sptr=sptr+1
 5270              CONTINUE
 5260             CONTINUE
                  GOTO 6320
 6321            CONTINUE
 6320            CONTINUE
 5240           CONTINUE
 5230          CONTINUE
 5220         CONTINUE
 5210        CONTINUE
 5200       CONTINUE
 5190      CONTINUE
 5180     CONTINUE
          GOTO 5170
 5171    CONTINUE
 5170    CONTINUE
         GOTO 5140
 5141   CONTINUE
 5140   CONTINUE
        GOTO 5110
 5111  CONTINUE
 5110  CONTINUE
       GOTO 5080
 5081 CONTINUE
 5080 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(env+1)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9003 CONTINUE
      iptr=iptr+5
      ipn=isto(ishenv+38)
      isto(ipn)=env
                 isto(ishenv+38)=env
      isto(env+0)=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      isto(env+3)=iptr+1
      isto(ishenv+50)=17
      to=9100
      CALL putil1
      isto(env+2)=ndx1
      ndx1=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5281
       isto(ishenv+54)=21
       CONTINUE
       CALL synstp
       GOTO 5280
 5281 CONTINUE
 5280 CONTINUE
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5291
       isto(ishenv+54)=3
       CONTINUE
       CALL synstp
       GOTO 5290
 5291 CONTINUE
 5290 CONTINUE
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=25
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9004 CONTINUE
      iptr=iptr+4
      ipn=isto(ishenv+39)
      isto(ipn)=env
                 isto(ishenv+39)=env
      isto(env+0)=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=21
      isto(ishenv+50)=63
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5301
       isto(ishenv+54)=68
       CONTINUE
       CALL synstp
       GOTO 5300
 5301 CONTINUE
 5300 CONTINUE
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5311
       isto(ishenv+54)=67
       CONTINUE
       CALL synstp
       GOTO 5310
 5311 CONTINUE
 5310 CONTINUE
      isto(env+3)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=64
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9005 CONTINUE
      iptr=iptr+3
      isto(env+0)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=8
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=26
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5321
       isto(ishenv+54)=22
       CONTINUE
       CALL synstp
       GOTO 5320
 5321 CONTINUE
 5320 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=0
      isto(env+2)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9516
      GOTO 9006
 9516 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      isto(env+1)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9006 CONTINUE
      ndx=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=dptr
      mndx=243
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5331
       mndx=83
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5341
        ndx=2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9517
        GOTO 9008
 9517   from=isto(sptr)
        sptr=sptr+1
        GOTO 5340
 5341  CONTINUE
 5340  CONTINUE
       GOTO 5330
 5331 CONTINUE
 5330 CONTINUE
      IF(.NOT.(ndx.EQ.0)) GOTO 5351
       dptr=ptr1
       mndx=245
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5361
        mndx=249
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5371
         mndx=83
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5381
          ndx=4
          sptr=sptr-1
          IF(sptr.EQ.iptr) GOTO 910
          isto(sptr)=from
          from=9518
          GOTO 9010
 9518     from=isto(sptr)
          sptr=sptr+1
          GOTO 5380
 5381    CONTINUE
 5380    CONTINUE
         GOTO 5370
 5371   CONTINUE
 5370   CONTINUE
        GOTO 5360
 5361  CONTINUE
 5360  CONTINUE
       GOTO 5350
 5351 CONTINUE
 5350 CONTINUE
      IF(.NOT.(ndx.EQ.0)) GOTO 5391
       dptr=ptr1
       mndx=247
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5401
        mndx=83
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5411
         ndx=5
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9519
         GOTO 9013
 9519    from=isto(sptr)
         sptr=sptr+1
         GOTO 5410
 5411   CONTINUE
 5410   CONTINUE
        GOTO 5400
 5401  CONTINUE
 5400  CONTINUE
       GOTO 5390
 5391 CONTINUE
 5390 CONTINUE
      IF(.NOT.(ndx.EQ.0)) GOTO 5421
       dptr=ptr1
       ndx=1
       isto(ishenv+50)=27
       to=9101
       CALL putil1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9520
       GOTO 9007
 9520  from=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(ndx.EQ.1 .AND. isto(ishenv+55).GT.0
     1 )) GOTO 5431
        isto(ishenv+54)=71
        CONTINUE
        CALL synstp
        GOTO 5430
 5431  CONTINUE
 5430  CONTINUE
       GOTO 5420
 5421 CONTINUE
 5420 CONTINUE
      ptr1=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9007 CONTINUE
      IF(.NOT.(isto(env+3).GT.0)) GOTO 5441
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=env+4
       IF(.NOT.(isto(env+0).EQ.7)) GOTO 5471
        env=env+2
        env=isto(env+15)
        IF(.NOT.(isto(env+0).EQ.1)) GOTO 5481
         env=env+2
         k=isto(env+1)
         IF(.NOT.(k.EQ.393)) GOTO 5491
          ndx=3
          GOTO 5490
 5491    IF(.NOT.(k.EQ.395)) GOTO 5492
          ndx=7
          GOTO 5490
 5492    CONTINUE
 5490    CONTINUE
         GOTO 5480
 5481   CONTINUE
 5480   CONTINUE
        GOTO 5470
 5471  CONTINUE
 5470  CONTINUE
       env=isto(sptr)
       sptr=sptr+1
       GOTO 5440
 5441 CONTINUE
 5440 CONTINUE
      GOTO 9500
 9008 CONTINUE
      iptr=iptr+3
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=1
      isto(env+0)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3011 IF(.NOT.(lpflg.GT.0)) GOTO 3010
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9521
       GOTO 9009
 9521  from=isto(sptr)
       sptr=sptr+1
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5501
        isto(env+0)=0
        lpflg=0
        GOTO 5500
 5501  CONTINUE
        mndx=91
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5511
         ndx=1
         GOTO 5510
 5511   CONTINUE
         mndx=93
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5521
          ndx=2
          GOTO 5520
 5521    CONTINUE
          isto(ishenv+54)=23
          CONTINUE
          CALL synstp
 5520    CONTINUE
 5510   CONTINUE
        isto(env+0)=iptr+1
        env=iptr+1
 5500  CONTINUE
       GOTO 3011
 3010 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
                 ptr3=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=0
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5531
       to=9122
       CALL putil1
       IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5541
        IF(.NOT.(isto(ishenv+57).GT.0)) GOTO 5551
         isto(ishenv+54)=72
         CONTINUE
         CALL synstp
         GOTO 5550
 5551   CONTINUE
 5550   CONTINUE
        isto(ishenv+57)=isto(ishenv+58)
        GOTO 5540
 5541  CONTINUE
 5540  CONTINUE
       GOTO 5530
 5531 CONTINUE
 5530 CONTINUE
      isto(env+1)=ptr3
      isto(env+2)=ptr4
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9009 CONTINUE
      iptr=iptr+5
      isto(env+1)=ndx
      isto(env+2)=0
      mndx=101
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5561
       isto(env+3)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=28
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       mndx=101
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5571
        isto(ishenv+54)=24
        CONTINUE
        CALL synstp
        GOTO 5570
 5571  CONTINUE
 5570  CONTINUE
       GOTO 5560
 5561 CONTINUE
       isto(env+3)=0
 5560 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=dptr
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5581
       mndx=91
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5591
        mndx=93
        to=9205
        CALL putil2
        GOTO 5590
 5591  CONTINUE
 5590  CONTINUE
       GOTO 5580
 5581 CONTINUE
 5580 CONTINUE
      dptr=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(mflg.EQ.0)) GOTO 5601
       IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5611
        isto(ishenv+56)=isto(ishenv+56)+1
        GOTO 5610
 5611  CONTINUE
 5610  CONTINUE
       isto(env+4)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx
                  ndx=9
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=29
       to=9120
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       ndx=isto(sptr)
       sptr=sptr+1
       GOTO 5600
 5601 CONTINUE
       isto(env+4)=0
 5600 CONTINUE
      GOTO 9500
 9010 CONTINUE
      iptr=iptr+7
      ipn=isto(ishenv+35)
      isto(ipn)=env
                 isto(ishenv+35)=env
      isto(env+0)=0
      isto(env+2)=0
      IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5621
       isto(ishenv+56)=isto(ishenv+56)+1
       GOTO 5620
 5621 CONTINUE
 5620 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=0
      isto(env+3)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9522
      GOTO 9011
 9522 from=isto(sptr)
      sptr=sptr+1
      isto(env+1)=ndx1
      ndx1=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      mndx=101
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5631
       isto(ishenv+54)=25
       CONTINUE
       CALL synstp
       GOTO 5630
 5631 CONTINUE
 5630 CONTINUE
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=10
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=30
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
                 ptr3=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=0
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5641
       to=9122
       CALL putil1
       IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5651
        IF(.NOT.(isto(ishenv+57).GT.0)) GOTO 5661
         isto(ishenv+54)=72
         CONTINUE
         CALL synstp
         GOTO 5660
 5661   CONTINUE
 5660   CONTINUE
        isto(ishenv+57)=isto(ishenv+58)
        GOTO 5650
 5651  CONTINUE
 5650  CONTINUE
       GOTO 5640
 5641 CONTINUE
 5640 CONTINUE
      isto(env+5)=ptr3
      isto(env+6)=ptr4
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9011 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3021 IF(.NOT.(lpflg.GT.0)) GOTO 3020
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9523
       GOTO 9012
 9523  from=isto(sptr)
       sptr=sptr+1
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5671
        isto(env+0)=0
        lpflg=0
        GOTO 5670
 5671  CONTINUE
        mndx=91
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 5681
         ndx=1
         GOTO 5680
 5681   CONTINUE
         mndx=93
         to=9205
         CALL putil2
         IF(.NOT.(mflg.GT.0)) GOTO 5691
          ndx=2
          GOTO 5690
 5691    CONTINUE
          isto(ishenv+54)=26
          CONTINUE
          CALL synstp
 5690    CONTINUE
 5680   CONTINUE
        isto(env+0)=iptr+1
        env=iptr+1
 5670  CONTINUE
       GOTO 3021
 3020 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9012 CONTINUE
      iptr=iptr+4
      isto(env+1)=ndx
      mndx=101
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5701
       isto(env+3)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=31
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       mndx=101
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5711
        isto(ishenv+54)=24
        CONTINUE
        CALL synstp
        GOTO 5710
 5711  CONTINUE
 5710  CONTINUE
       GOTO 5700
 5701 CONTINUE
       isto(env+3)=0
 5700 CONTINUE
      IF(.NOT.(flg.EQ.1)) GOTO 5721
       mndx=7
       GOTO 5720
 5721 IF(.NOT.(flg.EQ.2)) GOTO 5722
       mndx=37
       GOTO 5720
 5722 IF(.NOT.(flg.EQ.3)) GOTO 5723
       mndx=17
       GOTO 5720
 5723 CONTINUE
 5720 CONTINUE
      to=9205
      CALL putil2
      IF(.NOT.(flg.EQ.2 .AND. mflg.EQ.0)) GOTO 6231
       mndx=31
       to=9205
       CALL putil2
       GOTO 6230
 6231 CONTINUE
 6230 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx2
      IF(.NOT.(mflg.GT.0)) GOTO 5731
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx
       to=9212
       CALL putil2
       IF(.NOT.(ndx.LT.0)) GOTO 5741
        ndx2=1
        GOTO 5740
 5741  CONTINUE
        ndx2=ndx
 5740  CONTINUE
       ndx=isto(sptr)
       sptr=sptr+1
       to=9204
       CALL putil2
       GOTO 5730
 5731 CONTINUE
       ndx2=0
 5730 CONTINUE
      isto(env+2)=ndx2
      IF(.NOT.(ndx2.GT.ndx1)) GOTO 5751
       ndx1=ndx2
       GOTO 5750
 5751 CONTINUE
 5750 CONTINUE
      ndx2=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9013 CONTINUE
      iptr=iptr+9
      ipn=isto(ishenv+34)
      isto(ipn)=env
                 isto(ishenv+34)=env
      isto(env+0)=0
      isto(env+4)=0
      IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5761
       isto(ishenv+56)=isto(ishenv+56)+1
       GOTO 5760
 5761 CONTINUE
 5760 CONTINUE
      isto(env+3)=1
      isto(env+5)=0
      isto(env+6)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=11
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=32
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5771
       isto(ishenv+54)=27
       CONTINUE
       CALL synstp
       GOTO 5770
 5771 CONTINUE
 5770 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
                 ptr3=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=0
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5781
       to=9124
       CALL putil1
       IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5791
        IF(.NOT.(isto(ishenv+57).GT.0)) GOTO 5801
         isto(ishenv+54)=72
         CONTINUE
         CALL synstp
         GOTO 5800
 5801   CONTINUE
 5800   CONTINUE
        isto(ishenv+57)=isto(ishenv+58)
        GOTO 5790
 5791  CONTINUE
 5790  CONTINUE
       GOTO 5780
 5781 CONTINUE
 5780 CONTINUE
      isto(env+7)=ptr3
      isto(env+8)=ptr4
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9014 CONTINUE
      iptr=iptr+8
      ipn=isto(ishenv+36)
      isto(ipn)=env
                 isto(ishenv+36)=env
      isto(env+0)=0
      isto(env+3)=0
      IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 6291
       isto(ishenv+56)=isto(ishenv+56)+1
       GOTO 6290
 6291 CONTINUE
 6290 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
                 ndx1=0
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9524
      GOTO 9011
 9524 from=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=isto(env+1)
      k=1
 3081 IF(.NOT.(env.GT.0 .AND. k.NE.0
     1)) GOTO 3080
       k=isto(env+2)
       env=isto(env+0)
       GOTO 3081
 3080 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      IF(.NOT.(k.NE.0)) GOTO 6281
       isto(ishenv+54)=91
       CONTINUE
       CALL synstp
       GOTO 6280
 6281 CONTINUE
 6280 CONTINUE
      isto(env+2)=ndx1
      ndx1=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      mndx=101
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5811
       isto(ishenv+54)=25
       CONTINUE
       CALL synstp
       GOTO 5810
 5811 CONTINUE
 5810 CONTINUE
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=13
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=34
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
                 ptr3=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=0
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5821
       to=9124
       CALL putil1
       IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5831
        IF(.NOT.(isto(ishenv+57).GT.0)) GOTO 5841
         isto(ishenv+54)=72
         CONTINUE
         CALL synstp
         GOTO 5840
 5841   CONTINUE
 5840   CONTINUE
        isto(ishenv+57)=isto(ishenv+58)
        GOTO 5830
 5831  CONTINUE
 5830  CONTINUE
       GOTO 5820
 5821 CONTINUE
 5820 CONTINUE
      isto(env+6)=ptr3
      isto(env+7)=ptr4
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5851
       isto(ishenv+54)=30
       CONTINUE
       CALL synstp
       GOTO 5850
 5851 CONTINUE
 5850 CONTINUE
      isto(env+5)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=12
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=34
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9015 CONTINUE
      iptr=iptr+10
      ipn=isto(ishenv+37)
      isto(ipn)=env
                 isto(ishenv+37)=env
      isto(env+0)=0
      isto(env+3)=0
      IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5861
       isto(ishenv+56)=isto(ishenv+56)+1
       GOTO 5860
 5861 CONTINUE
 5860 CONTINUE
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=14
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=35
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      mndx=97
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5871
       isto(ishenv+54)=28
       CONTINUE
       CALL synstp
       GOTO 5870
 5871 CONTINUE
 5870 CONTINUE
      isto(env+5)=iptr+1
      ndx=15
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=36
      to=9120
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      mndx=85
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5881
       isto(ishenv+54)=29
       CONTINUE
       CALL synstp
       GOTO 5880
 5881 CONTINUE
 5880 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr3
                 ptr3=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr4
                 ptr4=0
      mndx=87
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5891
       to=9124
       CALL putil1
       IF(.NOT.(isto(ishenv+55).GT.0)) GOTO 5901
        IF(.NOT.(isto(ishenv+57).GT.0)) GOTO 5911
         isto(ishenv+54)=72
         CONTINUE
         CALL synstp
         GOTO 5910
 5911   CONTINUE
 5910   CONTINUE
        isto(ishenv+57)=isto(ishenv+58)
        GOTO 5900
 5901  CONTINUE
 5900  CONTINUE
       GOTO 5890
 5891 CONTINUE
 5890 CONTINUE
      isto(env+6)=ptr3
      isto(env+7)=ptr4
      ptr4=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5921
       isto(ishenv+54)=32
       CONTINUE
       CALL synstp
       GOTO 5920
 5921 CONTINUE
 5920 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=flg
                 flg=2
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx1
      isto(env+8)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9526
      GOTO 9016
 9526 from=isto(sptr)
      sptr=sptr+1
      isto(env+1)=ndx1
      mndx=97
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5931
       isto(ishenv+54)=33
       CONTINUE
       CALL synstp
       GOTO 5930
 5931 CONTINUE
 5930 CONTINUE
      isto(env+9)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9527
      GOTO 9016
 9527 from=isto(sptr)
      sptr=sptr+1
      isto(env+2)=ndx1
      ndx1=isto(sptr)
      sptr=sptr+1
      flg=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9016 CONTINUE
      ndx1=0
      mndx=83
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5941
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9528
       GOTO 9011
 9528  from=isto(sptr)
       sptr=sptr+1
       GOTO 5940
 5941 CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ndx
                  ndx=1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                 env=iptr+1
       isto(env+0)=0
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9529
       GOTO 9012
 9529  from=isto(sptr)
       sptr=sptr+1
       env=isto(sptr)
       sptr=sptr+1
       ndx=isto(sptr)
       sptr=sptr+1
 5940 CONTINUE
      GOTO 9500
 9017 CONTINUE
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
 3031 IF(.NOT.(lpflg.GT.0)) GOTO 3030
       iptr=iptr+3
       isto(env+1)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=37
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       mndx=201
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 5951
        isto(ishenv+54)=34
        CONTINUE
        CALL synstp
        GOTO 5950
 5951  CONTINUE
 5950  CONTINUE
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=isto(ishenv+53)
                  isto(ishenv+53)=3
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9530
       GOTO 9000
 9530  from=isto(sptr)
       sptr=sptr+1
       isto(env+2)=ptr1
       ptr1=isto(sptr)
       sptr=sptr+1
       isto(ishenv+53)=isto(sptr)
       sptr=sptr+1
       mndx=205
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5961
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5960
 5961  CONTINUE
        isto(env+0)=0
        lpflg=0
 5960  CONTINUE
       GOTO 3031
 3030 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      mndx=203
      to=9206
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 5971
       isto(env+0)=iptr+1
       env=iptr+1
       iptr=iptr+3
       isto(env+1)=0
       isto(env+0)=0
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=isto(ishenv+53)
                  isto(ishenv+53)=4
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=ptr1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9531
       GOTO 9000
 9531  from=isto(sptr)
       sptr=sptr+1
       isto(env+2)=ptr1
       ptr1=isto(sptr)
       sptr=sptr+1
       isto(ishenv+53)=isto(sptr)
       sptr=sptr+1
       GOTO 5970
 5971 CONTINUE
 5970 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      mndx=207
      to=9206
      CALL putil2
      GOTO 9500
 9018 CONTINUE
      iptr=iptr+2
      isto(env+0)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=38
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      mndx=217
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 5981
       isto(ishenv+54)=35
       CONTINUE
       CALL synstp
       GOTO 5980
 5981 CONTINUE
 5980 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+53)
                 isto(ishenv+53)=5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9532
      GOTO 9000
 9532 from=isto(sptr)
      sptr=sptr+1
      isto(env+1)=ptr1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(ishenv+53)=isto(sptr)
      sptr=sptr+1
      mndx=211
      to=9206
      CALL putil2
      GOTO 9500
 9019 CONTINUE
      iptr=iptr+5
      ipn=isto(ishenv+41)
      isto(ipn)=env
                 isto(ishenv+41)=env
      isto(env+0)=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx0
                 ndx0=ptr5
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr6
                 ptr6=env
      isto(env+3)=iptr+1
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
       from=9533
       GOTO 9020
 9533  from=isto(sptr)
       sptr=sptr+1
       mndx=213
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 5991
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 5990
 5991  CONTINUE
        isto(env+0)=0
        lpflg=0
 5990  CONTINUE
       GOTO 3041
 3040 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      mndx=217
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 6001
       isto(ishenv+54)=36
       CONTINUE
       CALL synstp
       GOTO 6000
 6001 CONTINUE
 6000 CONTINUE
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+53)
                 isto(ishenv+53)=6
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9534
      GOTO 9000
 9534 from=isto(sptr)
      sptr=sptr+1
      isto(env+4)=ptr1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(ishenv+53)=isto(sptr)
      sptr=sptr+1
      mndx=215
      to=9206
      CALL putil2
      isto(env+2)=ptr5 -ndx0
      ptr6=isto(sptr)
      sptr=sptr+1
      ndx0=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9020 CONTINUE
      ptr5=ptr5 +1
      iptr=iptr+7
      isto(env+5)=dptr+1
      to=9200
      CALL putil2
      isto(env+4)=dptr -isto(env+5)+1
      to=9204
      CALL putil2
      mndx=103
      to=9205
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 6011
       isto(ishenv+54)=37
       CONTINUE
       CALL synstp
       GOTO 6010
 6011 CONTINUE
 6010 CONTINUE
      isto(env+6)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3051 IF(.NOT.(lpflg.GT.0)) GOTO 3050
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9535
       GOTO 9021
 9535  from=isto(sptr)
       sptr=sptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=dptr
       mndx=217
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 6021
        mndx=213
        to=9206
        CALL putil2
        GOTO 6020
 6021  CONTINUE
 6020  CONTINUE
       dptr=isto(sptr)
       sptr=sptr+1
       IF(.NOT.(mflg.EQ.0)) GOTO 6031
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 6030
 6031  CONTINUE
        isto(env+0)=0
        lpflg=0
 6030  CONTINUE
       GOTO 3051
 3050 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9021 CONTINUE
      iptr=iptr+4
      isto(env+1)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(ishenv+50)=39
      to=9101
      CALL putil1
      env=isto(sptr)
      sptr=sptr+1
      isto(env+2)=0
      isto(env+3)=0
      mndx=251
      to=9206
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 6041
       isto(env+2)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=40
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       mndx=253
       to=9206
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 6051
        isto(env+3)=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        isto(ishenv+50)=41
        to=9101
        CALL putil1
        env=isto(sptr)
        sptr=sptr+1
        GOTO 6050
 6051  CONTINUE
 6050  CONTINUE
       GOTO 6040
 6041 CONTINUE
 6040 CONTINUE
      GOTO 9500
 9022 CONTINUE
      iptr=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+53)
                 isto(ishenv+53)=7
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+63)
                isto(ishenv+63)=1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9536
      GOTO 9000
 9536 from=isto(sptr)
      sptr=sptr+1
      isto(env+0)=ptr1
      isto(ishenv+63)=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(ishenv+53)=isto(sptr)
      sptr=sptr+1
      mndx=219
      to=9206
      CALL putil2
      GOTO 9500
 9023 CONTINUE
      iptr=iptr+10
      ipn=isto(ishenv+42)
      isto(ipn)=env
                 isto(ishenv+42)=env
      isto(env+0)=0
      mndx=255
      to=9206
      CALL putil2
      IF(.NOT.(mflg.EQ.0)) GOTO 6061
       isto(ishenv+54)=69
       CONTINUE
       CALL synstp
       GOTO 6060
 6061 CONTINUE
 6060 CONTINUE
      isto(env+2)=dptr+1
      to=9200
      CALL putil2
      isto(env+1)=dptr -isto(env+2)+1
      to=9204
      CALL putil2
      mndx=79
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 6071
       isto(env+3)=iptr+1
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=env
                  env=iptr+1
       isto(ishenv+50)=65
       to=9101
       CALL putil1
       env=isto(sptr)
       sptr=sptr+1
       mndx=81
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 6081
        isto(ishenv+54)=70
        CONTINUE
        CALL synstp
        GOTO 6080
 6081  CONTINUE
 6080  CONTINUE
       GOTO 6070
 6071 CONTINUE
       isto(env+3)=0
 6070 CONTINUE
      isto(ishenv+55)=0
      isto(ishenv+56)=0
      isto(ishenv+57)=0
      isto(env+4)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      isto(env+1)=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=lpflg
                 lpflg=1
 3061 IF(.NOT.(lpflg.GT.0)) GOTO 3060
       isto(ishenv+55)=isto(ishenv+55)+1
       isto(ishenv+58)=env
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9537
       GOTO 9024
 9537  from=isto(sptr)
       sptr=sptr+1
       mndx=225
       to=9206
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 6091
        isto(env+0)=iptr+1
        k=env
        env=iptr+1
        isto(env+1)=k
        GOTO 6090
 6091  CONTINUE
        isto(env+0)=0
        lpflg=0
 6090  CONTINUE
       GOTO 3061
 3060 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      i3=env
      k=0
      IF(.NOT.(isto(ishenv+57).EQ.0
     1.OR. isto(ishenv+57).EQ.env)) GOTO 6101
       i4=i3
       GOTO 6100
 6101 CONTINUE
       i4=isto(ishenv+57)
       isto(env+0)=isto(ishenv+57)
       env=isto(ishenv+57)
       i1=isto(env+0)
       i2=isto(env+1)
       isto(env+1)=i3
       isto(env+0)=0
       env=i1
       isto(env+1)=i2
       IF(.NOT.(i2.GT.0)) GOTO 6111
        env=i2
        isto(env+0)=i1
        GOTO 6110
 6111  CONTINUE
        k=1
 6110  CONTINUE
 6100 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      isto(env+5)=i4
      IF(.NOT.(k.GT.0)) GOTO 6121
       isto(env+4)=i1
       GOTO 6120
 6121 CONTINUE
 6120 CONTINUE
      isto(env+7)=isto(ishenv+55)
      isto(env+8)=isto(ishenv+56)
      IF(.NOT.(isto(ishenv+57).GT.0)) GOTO 6131
       isto(env+6)=1
       GOTO 6130
 6131 CONTINUE
       isto(env+6)=0
 6130 CONTINUE
      isto(ishenv+55)=0
      isto(ishenv+56)=0
      isto(ishenv+57)=0
      GOTO 9500
 9024 CONTINUE
      iptr=iptr+6
      isto(env+5)=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ndx
                 ndx=0
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
                 ptr1=dptr
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=env
                 env=iptr+1
      mndx=195
      to=9205
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 6141
       mndx=83
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 6151
        ndx=2
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9538
        GOTO 9014
 9538   from=isto(sptr)
        sptr=sptr+1
        GOTO 6150
 6151  CONTINUE
 6150  CONTINUE
       GOTO 6140
 6141 CONTINUE
 6140 CONTINUE
      IF(.NOT.(ndx.EQ.0)) GOTO 6161
       dptr=ptr1
       mndx=197
       to=9205
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 6171
        mndx=83
        to=9205
        CALL putil2
        IF(.NOT.(mflg.GT.0)) GOTO 6181
         ndx=3
         sptr=sptr-1
         IF(sptr.EQ.iptr) GOTO 910
         isto(sptr)=from
         from=9539
         GOTO 9015
 9539    from=isto(sptr)
         sptr=sptr+1
         GOTO 6180
 6181   CONTINUE
 6180   CONTINUE
        GOTO 6170
 6171  CONTINUE
 6170  CONTINUE
       IF(.NOT.(ndx.EQ.0)) GOTO 6191
        dptr=ptr1
        ndx=1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=from
        from=9540
        GOTO 9005
 9540   from=isto(sptr)
        sptr=sptr+1
        GOTO 6190
 6191  CONTINUE
 6190  CONTINUE
       GOTO 6160
 6161 CONTINUE
 6160 CONTINUE
      env=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(env+4)=ndx
      ndx=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9025 CONTINUE
      iptr=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+53)
                 isto(ishenv+53)=8
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9541
      GOTO 9000
 9541 from=isto(sptr)
      sptr=sptr+1
      isto(env+0)=ptr1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(ishenv+53)=isto(sptr)
      sptr=sptr+1
      mndx=229
      to=9206
      CALL putil2
      GOTO 9500
 9026 CONTINUE
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
 3071 IF(.NOT.(lpflg.GT.0)) GOTO 3070
       sptr=sptr-1
       IF(sptr.EQ.iptr) GOTO 910
       isto(sptr)=from
       from=9542
       GOTO 9027
 9542  from=isto(sptr)
       sptr=sptr+1
       mndx=85
       to=9205
       CALL putil2
       IF(.NOT.(mflg.EQ.0)) GOTO 6201
        isto(env+0)=iptr+1
        env=iptr+1
        GOTO 6200
 6201  CONTINUE
        isto(env+0)=0
        lpflg=0
 6200  CONTINUE
       GOTO 3071
 3070 CONTINUE
      lpflg=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      GOTO 9500
 9027 CONTINUE
      iptr=iptr+4
      isto(env+2)=dptr+2
      isto(env+1)=0
      isto(env+3)=0
      mndx=75
      to=9208
      CALL putil2
      IF(.NOT.(mflg.GT.0)) GOTO 6211
       to=9201
       CALL putil2
       isto(env+1)=dptr -isto(env+2)
       to=9204
       CALL putil2
       GOTO 6210
 6211 CONTINUE
       mndx=77
       to=9208
       CALL putil2
       IF(.NOT.(mflg.GT.0)) GOTO 6221
        to=9202
        CALL putil2
        isto(env+1)=dptr -isto(env+2)
        to=9204
        CALL putil2
        GOTO 6220
 6221  CONTINUE
        isto(env+2)=0
        isto(env+3)=iptr+1
        sptr=sptr-1
        IF(sptr.EQ.iptr) GOTO 910
        isto(sptr)=env
                   env=iptr+1
        isto(ishenv+50)=42
        to=9101
        CALL putil1
        env=isto(sptr)
        sptr=sptr+1
 6220  CONTINUE
 6210 CONTINUE
      GOTO 9500
 9028 CONTINUE
      iptr=iptr+1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=isto(ishenv+53)
                 isto(ishenv+53)=9
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=ptr1
      sptr=sptr-1
      IF(sptr.EQ.iptr) GOTO 910
      isto(sptr)=from
      from=9543
      GOTO 9000
 9543 from=isto(sptr)
      sptr=sptr+1
      isto(env+0)=ptr1
      ptr1=isto(sptr)
      sptr=sptr+1
      isto(ishenv+53)=isto(sptr)
      sptr=sptr+1
      mndx=237
      to=9206
      CALL putil2
      GOTO 9500
      END
