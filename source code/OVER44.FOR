      SUBROUTINE SUBR44                                                 M22.6091
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK44.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      DIMENSION  LLTEMP(20)                                             M22.6093
      DIMENSION  STG(1)                                                 M31.6300
      EQUIVALENCE (STG(1), KARRAY(1) )                                  M31.6301
C     LIST-ZERO "KARRAY" IS ALWAYS 1ST, AND MAYBE "OVER29":             M31.6302
      IF ( IPRSUP  .GE.  1 )                                            M32, 290
     1 WRITE ( LUNIT6, 4567 )                                           M32, 291
 4567 FORMAT ( 24H  "BEGIN MODULE SUBR44." )                            M32, 292
      N8 = NCHAIN                                                       M31.6303
      IF ( KBURRO  .EQ.  1 )   N8 = 29                                  M31.6304
      CALL DIMENS ( LLTEMP(1), N8, TRASH, TRASH )                       M31.6305
      DO 1003  J=1, 9999, 2                                             M35.9052
      IF ( LLTEMP(J)  .EQ.  0 )   GO TO 5636                            M35.9053
 1003 CONTINUE                                                          M35.9054
      WRITE (LUNIT6, 5632)  LLTEMP(1), KBURRO, NCHAIN                   M31.6307
 5632 FORMAT (  29H TEMP ERROR STOP IN "SUBR44".,  3I8 )                M31.6308
      CALL STOPTP                                                               
 5636 N7 = LLTEMP(J+1) * NBYTE(4) / NBYTE(3)                            M35.9055
      CALL DIMENS ( LLTEMP(1), NCHAIN, TRASH, TRASH )                   M22.6094
      N3 = 0                                                            M35.9056
      DO 5654  I=1, 9999, 2                                             M35.9057
      IF ( N3  .GE.  2 )   GO TO 5655                                   M35.9058
      IF ( LLTEMP(I)  .NE.  71 )   GO TO 5641                           M22.6098
      LPHASE = LLTEMP(I+1)                                              M32, 293
      N3 = N3 + 1                                                       M35.9059
 5641 IF ( LLTEMP(I)  .NE.  74 )   GO TO 5654                           M22.6100
      LGDBD = LLTEMP(I+1)                                               M32, 294
      N3 = N3 + 1                                                       M35.9060
 5654 CONTINUE                                                          M22.6102
      CALL STOPTP                                                               
 5655 LPHPL1 = LPHASE + 1                                               M35.9062
      LPHD2 = LPHASE / 2                                                M22.6104
      WRITE (KUNIT6, 2456)  LPHASE                                      M35.9063
 2456 FORMAT (  44H+REQUEST FOR LINE-CONSTANTS SUPPORTING PROG.,        M35.9064
     1          I6   )                                                  M35.9065
      NDIM = LPHASE                                                     M36.3583
      NTRI = NDIM * (NDIM + 1) / 2                                      M31.6315
      NSQR = NDIM * NDIM                                                M31.6316
      NSQR2 = 2 * NSQR                                                  M31.6317
      IOFARR = 1                                                        M36.3584
      IOFXWC = IOFARR + NTRI                                            M31.6327
      IOFXWY = IOFXWC + NTRI                                            M31.6328
      IOFYZR = IOFXWY + NTRI                                            M31.6329
      IOFYZI = IOFYZR + NSQR                                            M31.6330
      IOFTII = IOFYZI + NSQR                                            M31.6331
      IOFTIR = IOFTII + NSQR                                            M31.6332
      IOFTVI = IOFTIR + NSQR                                            M43.3309
      IOFTVR = IOFTVI + NSQR                                            M43.3310
      IOFER  = IOFTVR + NSQR                                            M43.3311
      IOFEI  = IOFER  + NDIM                                            M31.6334
      IOFTHE = IOFEI  + NDIM                                            M31.6335
      IOFXTR = IOFTHE + NDIM                                            M31.6336
      IOFXTI = IOFXTR + NDIM                                            M31.6337
      IOFZSU = IOFXTI + NDIM                                            M31.6338
      IOFDUM = IOFZSU + NDIM                                            M31.6339
      IOFDUR = IOFDUM + NDIM                                            M31.6340
      IOFTIX = IOFDUR + NDIM                                            M31.6341
      IOFWOR = IOFTIX + NSQR2                                           M31.6342
      N5 = IOFWOR + NSQR2                                               M36.3585
      IF ( N5 .LT. N7 )  GO TO 10                                       M36.3586
      KILL = 82                                                         M36.3587
      LSTAT(19) = 10                                                    M36.3588
      LSTAT(15) = LPHASE                                                M36.3589
      LSTAT(18) = NCHAIN                                                M36.3590
      LASTOV = NCHAIN                                                   M36.3591
      NCHAIN = 51                                                       M36.3592
      GO TO 99999                                                       M36.3593
   10 CALL GUTS44( STG(IOFARR), STG(IOFXWC), STG(IOFXWY), STG(IOFYZR),  M36.3594
     1 STG(IOFYZI), STG(IOFTII), STG(IOFTIR),STG(IOFTVI),               M43.3312
     2 STG(IOFTVR), STG(IOFER), STG(IOFEI), STG(IOFTHE), STG(IOFXTR),   M43.3313
     3   STG(IOFXTI), STG(IOFZSU), STG(IOFDUM), STG(IOFDUR),STG(IOFTIX),M31.6346
     4   STG(IOFWOR), NDIM, NTRI, NSQR2 )                               M31.6347
      IF ( IPRSUP  .GE.  1 )                                            M32, 296
     1 WRITE ( LUNIT6, 4568 )                                           M32, 297
 4568 FORMAT ( 24H  "EXIT  MODULE SUBR44." )                            M32, 298
99999 RETURN                                                            M31.6348
      END                                                               M31.6349
      SUBROUTINE GUTS44(ARRAY,XWC,XWY,YZR,YZI,TII,TIR,TVI,TVR,ER,       M43.3314
     1          EI,THETA2,XTIR,XTII,ZSURGE,DUMMI,DUMMR,TIXF,WORK1,      M31.6351
     2          NDIM,NTRI,NSQR2)                                        M31.6352
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK44.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      INCLUDE  'VOLT45.FOR;/NOLIST'    
      COMMON  /LINEMODEL/ KEXACT, NSOLVE, FMINSV, NUMRUN,NPHLMT
      COMMON / LINEMODEL / CHAR80, CHLMFS(18)
      CHARACTER*6 CHLMFS     ! 9-PHASE AS LIMIT FOR LMFS TEST
      CHARACTER*80 CHAR80

      REAL*8        TEXT1, TEXT2,  FMETRC, ENGLIS, BUFSEM               M31.6357
      REAL*8        TEXT3, TEXT4,  TEXT5                                M43.3315
      DIMENSION  BUFSEM(14), JPRMAT(16)                                 M31.6359
      DIMENSION ARRAY(1)                                                M31.6360
      DIMENSION XWC(NTRI),XWY(NTRI),YZI(NDIM,NDIM),YZR(NDIM,NDIM)       M31.6361
      DIMENSION TII(NDIM,NDIM),TIR(NDIM,NDIM)                           M31.6362
      DIMENSION ER(NDIM),EI(NDIM),THETA2(NDIM)                          M31.6363
      DIMENSION XTIR(NDIM),XTII(NDIM),ZSURGE(NDIM),DUMMI(NDIM)          M31.6364
      DIMENSION DUMMR(NDIM),TIXF(NSQR2),WORK1(NSQR2)                    M31.6365
      EQUIVALENCE   ( JPRMAT(1), J1 ),     ( JPRMAT(2), J2 )            M31.6366
      EQUIVALENCE   ( JPRMAT(3), J3 ),     ( JPRMAT(4), J4 )            M31.6367
      EQUIVALENCE   ( JPRMAT(5), J5 ),     ( JPRMAT(6), J6 )            M31.6368
      EQUIVALENCE   ( JPRMAT(7), J7 ),     ( JPRMAT(8), J8 )            M31.6369
      EQUIVALENCE   ( JPRMAT(9), J9 ),     ( JPRMAT(10), J10 )          M31.6370
      EQUIVALENCE   ( JPRMAT(11), J11 ),    ( JPRMAT(12), J12 )         M31.6371
      EQUIVALENCE   ( JPRMAT(13), J13 ),    ( JPRMAT(14), J14)          M31.6372
      EQUIVALENCE   ( JPRMAT(15), J15 ),    ( JPRMAT(16), J16)          M31.6373
      DATA  TEXT1  / 6HCHANGE /                                         M31.6374
      DATA  TEXT2  / 6HBRANCH /                                         M31.6375
      DATA  TEXT3  / 6HFREQUE /                                         M31.6376
      DATA  TEXT4  / 6HSPECIA /                                         M43.3316
      DATA  TEXT5  / 6HL DOUB /                                         M43.3317
      DATA  TEXT6  / 6HUNTRAN /                                         M44. 820
      DATA  TEXT7  / 6HSPOSED /                                         M44. 821
      DATA  TEXT8  / 6HTRANSP /                                         M44. 822
      DATA  TEXT9  / 6HOSED   /                                         M44. 823
      DATA FMETRC  / 6HMETRIC /                                         M31.6377
      DATA  ENGLIS   /  6HENGLIS  /                                     M31.6378
      DATA  NRP / 0 /                                                   M43.3318
      DATA  MRR / 0 /                                                   M43.3319
      MFRQPR = 0                                                        M31.6379
      IF ( KEXACT .NE. 88333 )  GO TO 423
      NFQPL1 = 0
      CLOSE (UNIT=LUNIT2, STATUS='DELETE' )
      OPEN (UNIT=LUNIT2, STATUS='SCRATCH',FORM='FORMATTED')
  423 REWIND  LUNIT9          
      REWIND LUNIT2                                                     M17.1263
      REWIND LUNIT3                                                     M17.1264
      REWIND LUNT13
      IF ( IALTER  .NE.  2 )   GO TO 7407                               M16.4423
      L5SAVE = LUNIT5                                                   M16.4424
      LUNIT5 = LUNIT2                                                   M16.4425
 7407 LDISFR  =  LOCF( FLSTAT(1) )     -     LOCF( VOLTBC(1) )          M31.6380
      METRIK=0                                                          M14.2816
      FINPCM = UNITY / 2.5400D0                                         M43.3321
      FTPM = 100. * FINPCM / 12.                                        M14.2818
      FMIPKM = FTPM * 1000. / 5280.                                     M14.2819
      D8 = 0.3048D0                                                     M28.7180
      FSPAC = .024D0 * TWOPI * ALOGZ(D8)                                M28.7181
C     FSPAC NEEDED FOR CONVERSION OF REACTANCE AT 1 METER               M28.7182
C     SPACING TO 1 FOOT SPACING.                                        M28.7183
C              START OF CONSTANTS DEFINITION.                           M14.2820
      VALU1    =  558 4596.2 D0                                         M14.2821
      VALU2    =  .0008 5646 541 D0                                     M14.2822
      VALU3    =  .0006 4373 888  D0                                    M14.2823
      VALU4    =  .6159 3152 D0                                         M14.2824
      VALU5    =  8.6858 8964 D0                                        M14.2825
      VALU6    =  .0005000000 D0                                        M14.2826
      VALU7 = 3.0                                                       M14.2827
      VALU7    =  ONEHAF * SQRTZ( VALU7 )                               M18. 927
      VALU8    =  .0040 4473 06  D0                                     M14.2829
      VALU9    =  .2113 9217 D0                                         M14.2830
      VALU10   =  .5772 1566 4901 5328 606  D0                          M14.2831
      VALU11   =  .3926 991  D0                                         M14.2832
      VALU12   =  .3989 4228 0401 433 D0                                M14.2833
      VALU13   =  1.2533 1413 7315 5   D0                               M14.2834
      VALU14 = 2.3025 8509 3000 D0                                      M14.2835
      AAA1    = .785398163397448 D0                                     M14.2836
      AAA2    = .318309886183791 D0                                     M14.2837
      SQRT2 = 1.414 2135 6237 3095 0488 D0                              M14.2838
      CCARS(2)=1.365 9315 1565 8412 4488 D0                             M14.2839
      LL0   =   0                                                       M14.2840
      LL1   =   1                                                       M14.2841
      LL2   =   2                                                       M14.2842
      LL3   =   3                                                       M14.2843
      LL5   =   5                                                       M14.2844
      LL6   =   6                                                       M14.2845
      LL7   =   7                                                       M14.2846
      LL8   =   8                                                       M14.2847
      LL9   =   9                                                       M14.2848
      LL10  =   10                                                      M14.2849
      LIU = 0                                                           M19.2422
      NFREQ = 0                                                         M30.1239
      NBUNDL = 0                                                                
      PI = TWOPI * ONEHAF                                               M14.2850
      PICON = 360. / TWOPI                                              M14.2851
      CORCHK = UNITY - TENM6/10.                                        M14.2853
C              BEGIN CALCULATE CONSTANTS FOR CARSON % BESSEL.           M14.2854
      BCARS(1)=SQRT2/6.                                                 M14.2855
      BCARS(2)=UNITY/16.                                                M14.2856
      DCARS(2)=BCARS(2)*PI/4.                                           M14.2857
      DO 6603 I=3,30                                                    M14.2858
      ISN=(-1) ** ((I-1)/2)                                             M14.2859
      BCARS(I) =-BCARS(I-2)/(I * I + 2. * I)*ISN                        M14.2860
 6603 DCARS(I)=BCARS(I)*PI/4.                                           M14.2861
      CCARS(1)=UNITY/SQRT2                                              M14.2862
      CCARS(3)=CCARS(1)                                                 M14.2863
      CCARS(5)=3.*SQRT2/2.                                              M14.2864
      CCARS(7)=-45.*ONEHAF*SQRT2                                        M14.2865
      DO 64  I=1,29,2                                                   M14.2866
      IF ( I .GT. 8 ) CCARS(I) = 0.0                                    M14.2867
   64 DCARS(I) = CCARS(I)                                               M14.2868
      DCARS(3) = -DCARS(3)                                              M22.6108
      DCARS(7) = -DCARS(7)                                              M22.6109
      DO 46  I=4,30,2                                                   M14.2869
   46 CCARS(I)=CCARS(I-2)+UNITY/I + UNITY/(I+2.)                        M14.2870
      FBE(1)=16.                                                        M14.2871
      FBED(1)=-4.                                                       M14.2872
      D1 = UNITY - VALU10                                               M14.2873
      FKE(1)=FBE(1)*D1                                                  M14.2874
      DO 811  I=2, 14                                                   M15.1553
      ISN= (-1)**I                                                      M14.2876
      FBE(I)=FBE(I-1)*(16./(I*I))*(-ISN)                                M14.2877
      FBED(I)=FBE(I)/(2.*I+2.)*ISN                                      M14.2878
      D1=D1+UNITY/I                                                     M14.2879
      FKE(I)=FBE(I)*D1                                                  M14.2880
  811 FKED(I-1) = FKE(I) * I / 32.                                      M16.4427
      FKED(14) = 0.0                                                    M16.4428
      VALU9=FKE(1)/32.                                                  M14.2882
      FBE(15) = .011 048 60 D0                                          M15.1554
      FBE(16) = 0.0                                                     M15.1555
      FBE(17) = -.000 090 60 D0                                         M15.1556
      FBE(18) = -.000 025 20 D0                                         M15.1557
      FBE(19) = -.000 003 40 D0                                         M15.1558
      FBE(20) =  .000 000 60 D0                                         M15.1559
      FBED(15) = -.011 048 50 D0                                        M15.1560
      FBED(16) = -.000 976 50 D0                                        M15.1561
      FBED(17) = -.000 090 10 D0                                        M15.1562
      FBED(18) =  .0                                                    M15.1563
      FBED(19) = .000 005 10 D0                                         M15.1564
      FBED(20) = .000 001 90 D0                                         M15.1565
      FKE(15) = -.062 500 10 D0                                         M15.1566
      FKE(16) = -.001 381 30 D0                                         M15.1567
      FKE(17) =  .000 000 50 D0                                         M15.1568
      FKE(18) =  .000 034 60 D0                                         M15.1569
      FKE(19) =  .000 011 70 D0                                         M15.1570
      FKE(20) = .000 001 60 D0                                          M15.1571
      FKED(15) = -.000 000 10 D0                                        M15.1572
      FKED(16) =  .001 381 10 D0                                        M15.1573
      FKED(17) =  .000 245 20 D0                                        M15.1574
      FKED(18) =  .000 033 80 D0                                        M15.1575
      FKED(19) = -.000 002 40 D0                                        M15.1576
      FKED(20) = -.000 003 20 D0                                        M15.1577
      IF(IPRSUP.GE.2)WRITE(LUNIT6,333)(FBE(I),I=1,20),(FBED(I),I=1,20), M17.1265
     1 (FKE(I),I=1,20),(FKED(I),I=1,20),(BCARS(I),I=1,30),              M14.2884
     2 (DCARS(I),I=1,30),(CCARS(I),I=1,30)                              M14.2885
  333 FORMAT(1X,10HDATA FBE /  ,/,4(1X,5E25.15,/),/,12H DATA FBED / ,/  M14.2886
     1 4(1X,5E25.15,/),/,11H DATA FKE / ,/,4(1X,5E25.15,/),/,           M14.2887
     2 12H DATA FKED /  ,/,4(1X,5E25.15,/),/                            M14.2888
     3 ,13H DATA BCARS / ,/,6(1X,5E25.15,/),/,13H DATA DCARS / ,/,6(1X,5M14.2889
     4 E25.15,/),/,13H DATA CCARS / ,/,6(1X,5E25.15,/),/,1X)            M15.1578
      IF ( IPRSUP  .GE.  1 )                                            M14.2891
     1WRITE (LUNIT6, 3866)  PI, PICON, SQRT2                            M17.1266
 3866 FORMAT ( /,  9H AT 3866  , 3E25.15  )                             M14.2893
    2 M = 1                                                             M35.9066
      DO 13866  I = 1, 40                                               M31.6381
13866 BRNAME(I) = BLANK                                                 M29.4153
      MSPEDB = 0                                                                
      MUNTRN = 0                                                                
      MTRNSP = 0                                                                
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4154
      if (lastov .eq. 1)  rewind lunit2                                 M44. 827
      REWIND LUNT13
C     READ INPUT CARD USING CIMAGE                                      M14.2895
 7403 CALL CIMAGE                                                       M16.4429
 7413 CONTINUE                                                          M29.4156
      DECODE (80, 4230, ABUFF)  BUS1                                    M29.4157
      IF ( BUS1 .NE. TEXT2)  GO TO 37403                                M29.4158
C     OPTIONAL "BRANCH" CARD, WHICH SPECIES  A6  BRANCH NAMES           M29.4159
      N1 = M + 11                                                       M29.4160
      DECODE (80, 17403, ABUFF) (BRNAME(I),I=M,N1)                      M29.4161
17403 FORMAT (8X, 12A6)                                                 M29.4162
      M = M + 12                                                        M29.4163
      WRITE (KUNIT6, 27403)                                             M35.9068
27403 FORMAT (26H+BUS NAMES FOR EACH PHASE.)                            M29.4165
      GO TO 7403                                                        M29.4166
37403 CONTINUE                                                          M29.4167
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M29.4168
 4230 FORMAT ( 13A6, A2 )                                               M16.4431
      IF ( IALTER  .NE.  2 )                                            M16.4432
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1268
      BUS1 = BUFSEM(1)                                                  M16.4434
      IF ( BUS1 .NE. FMETRC ) GOTO 4258                                 M14.2899
      METRIK = 1                                                        M14.2900
      WRITE (KUNIT6, 4252)                                              M35.9069
 4252 FORMAT ( 44H+REQUEST CARD FOR METRIC  UNITS ON ALL DATA.    )     M20.5072
      GO TO 7403                                                        M16.4435
 4258 IF ( BUS1  .NE.  ENGLIS )   GO TO 24258                           M15.1581
      METRIK = 0                                                        M15.1582
      WRITE (KUNIT6, 14258 )                                            M35.9070
14258 FORMAT (  44H+REQUEST CARD FOR ENGLISH UNITS ON ALL DATA.   )     M15.1584
      GO TO 7403                                                        M16.4436
24258 IF ( BUS1  .NE.  TEXT3 )   GO TO 34258                            M31.6382
      WRITE (KUNIT6, 44258)                                             M35.9071
44258 FORMAT ( 37H+REQUEST FOR FREQUENCY-LOOP PRINTOUT.  )              M31.6384
      MFRQPR = 1                                                        M31.6385
      GO TO 7403                                                        M31.6386
34258 IF ( BUFSEM(1) .NE. TEXT4 )  GO TO 54258                          M43.3324
      IF ( BUFSEM(2) .NE. TEXT5 )  GO TO 54258                          M43.3325
      MSPEDB = 1                                                        M43.3326
      WRITE (KUNIT6, 5010)                                              M43.3327
 5010 FORMAT (47H+REQUEST FOR SPECIAL DOUBLE CIRCUIT TRANSPOSED.  )     M43.3328
      GO TO 7403                                                        M43.3329
54258 IF ( BUFSEM(1) .NE. TEXT6 )  GO TO 5020                           M44. 828
      IF ( BUFSEM(2) .NE. TEXT7 )  GO TO 5020                           M44. 829
      MUNTRN = 1                                                        M44. 830
      WRITE (KUNIT6, 5015)                                              M44. 831
 5015 FORMAT (40H+REQUEST FOR UNTRANSPOSED LINE MODELING.  )            M44. 832
      GO TO 7403                                                        M44. 833
 5020 IF ( BUFSEM(1) .NE. TEXT8 )  GO TO 5029                           M44. 834
      IF ( BUFSEM(2) .NE. TEXT9 )  GO TO 5029                           M44. 835
      MTRNSP = 1                                                        M44. 836
      WRITE (KUNIT6,5027)                                               M44. 837
 5027 FORMAT (38H+REQUEST FOR TRANSPOSED LINE MODELING.  )              M44. 838
      GO TO 7403                                                        M44. 839
 5029 DO 7436  J=1, 14                                                  M44. 840
      IF ( BUFSEM(J)  .NE.  BLANK )   GO TO 4251                        M16.4438
 7436 CONTINUE                                                          M16.4439
      WRITE (KUNIT6, 4244 )                                             M35.9072
 4244 FORMAT (  45H+BLANK CARD TERMINATING LINE-CONSTANTS CASES.  )     M14.2907
      CALL INTERP                                                       M35.9073
      IF ( LASTOV  .EQ.  1 )   GO TO 7439                               M16.4440
      N1 = LASTOV                                                       M16.4441
      LASTOV = NCHAIN                                                   M16.4442
      NCHAIN = N1                                                       M16.4443
      IF ( IALTER  .EQ.  2 )                                            M16.4444
     1 LUNIT5 = L5SAVE                                                  M16.4445
      IF ( IPUNCH  .EQ.  0 )   GO TO 7496                               M16.4446
      D1 = 0.0                                                          M16.4447
      WRITE (LUNIT9)  D1, D1, D1                                        M17.1272
      IF ( IPRSUP .GE. 1   .OR.   LASTOV .EQ. 39 )                      M36.3595
     1 WRITE (LUNIT6, 14244)  D1, D1, D1, IPUNCH                        M19.2424
14244 FORMAT (21H LAST RECORD ON UNIT9, 3E15.6,/, 1X, 8HIPUNCH =,I10)   M19.2425
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4169
      REWIND LUNIT9                                                     M17.1273
 7496 GO TO 9900                                                        M16.4450
 7439 LASTOV = NCHAIN                                                   M16.4452
      NCHAIN = 51                                                       M22.6110
      GO TO 9900                                                        M14.2909
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4170
 4251 REWIND LUNT13                                  
      IK = 0                                                            M16.4453
      IF ( BUS1  .NE.  TEXT1 )   GO TO 4260                             M14.2911
      WRITE (KUNIT6, 4257)                                              M35.9074
 4257 FORMAT (  37H+REQUEST CARD FOR CHANGE-CASE OPTION.   )            M14.2913
      GO TO 500                                                         M14.2914
 4260 I = 1                                                             M14.2915
      ASSIGN 4236 TO MOON                                               M14.2916
      GO TO 4164                                                        M14.2917
    5 I = I + 1                                                         M14.2918
C     READ INPUT CARD USING CIMAGE                                      M14.2919
      CALL CIMAGE                                                       M14.2920
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.4454
      IF ( IALTER  .NE.  2 )                                            M16.4455
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1276
 4164 IF ( KOLBEG  .GT.  0 )   GO TO 8712                               M22.6111
      DECODE (80, 11, ABUFF(1) )  ITBIC(I), TBTB2(I),                   M14.2922
     1                            TBR(I),ITBTB3(I),TBG(I),TBD(I),       M14.2923
     1   TBX(I), H1, H2, D8, D9, TBTEXT(I), I3                          M14.2924
   11 FORMAT ( I3, F5.4, F8.5, I2, 2F8.5, 3F8.3, F8.5, F6.2, A6, I2 )   M14.2925
      GO TO 8715                                                        M22.6112
 8712 NFRFLD = 1                                                        M22.6113
      CALL FREONE ( D11 )                                               M29.4172
      ITBIC(I) = D11                                                    M22.6115
      CALL FREFLD ( TBTB2(I) )                                          M22.6116
      CALL FREFLD ( TBR(I) )                                            M22.6117
      CALL FREONE ( D11 )                                               M29.4173
      ITBTB3(I) = D11                                                   M22.6119
      CALL FREFLD ( TBG(I) )                                            M22.6120
      CALL FREFLD ( TBD(I) )                                            M22.6121
      CALL FREFLD ( TBX(I) )                                            M22.6122
      CALL FREONE ( H1 )                                                M29.4174
      CALL FREONE ( H2 )                                                M29.4175
      CALL FREONE ( D8 )                                                M29.4176
      CALL FREONE ( D9 )                                                M29.4177
      NRIGHT = -1                                                       M22.6127
      CALL FREONE ( D1 )                                                M29.4178
      NRIGHT = 0                                                        M22.6129
      TBTEXT(I) = TEXTA6(1)                                             M22.6130
      CALL FREONE ( D11 )                                               M29.4179
      I3 = D11                                                          M22.6132
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M22.6133
      GO TO 4235                                                        M22.6134
 8715 IF ( I  .EQ.  1 )   GO TO 4320                                    M22.6135
 4280 IF( ITBIC(I)  .NE.  0 )  GO TO 4285                               M14.2927
      DECODE (80, 4281, ABUFF) BUS1                                     M14.2928
 4281 FORMAT ( A3 )                                                     M14.2929
      IF ( BUS1  .NE.  BLANK )   GO TO 4285                             M14.2930
      ITBIC(I) = ITBIC(I-1)                                             M14.2931
 4285 IF ( TBTB2(I)  .NE.  0.0 )  GO TO 4291                            M14.2932
      DECODE (80, 4286, ABUFF)  BUS1                                    M14.2933
 4286 FORMAT ( 3X, A5 )                                                 M14.2934
      IF( BUS1  .NE.  BLANK )  GO TO 4291                               M14.2935
      TBTB2(I) = TBTB2(I-1)                                             M14.2936
 4291 IF( TBR(I)  .NE.  0.0 )  GO TO 4296                               M14.2937
      DECODE (80, 4292, ABUFF)  BUS1, BUS2                              M14.2938
 4292 FORMAT ( 8X, 2A4 )                                                M14.2939
      IF( BUS1 .NE. BLANK )  GO TO 4296                                 M14.2940
      IF( BUS2 .NE. BLANK )  GO TO 4296                                 M14.2941
      TBR(I) = TBR(I-1)                                                 M14.2942
 4296 IF ( ITBTB3(I)  .NE.  0 )  GO TO 4305                             M14.2943
      DECODE (80, 4297, ABUFF)  BUS1                                    M14.2944
 4297 FORMAT ( 16X, A2 )                                                M14.2945
      IF ( BUS1  .NE.  BLANK )  GO TO 4305                              M14.2946
      ITBTB3(I) = ITBTB3(I-1)                                           M14.2947
 4305 IF( TBG(I)  .NE.  0.0 )  GO TO 4312                               M14.2948
      DECODE (80, 4306, ABUFF)  BUS1, BUS2                              M14.2949
 4306 FORMAT ( 18X, 2A4 )                                               M14.2950
      IF( BUS1  .NE.  BLANK )  GO TO 4312                               M14.2951
      IF( BUS2  .NE.  BLANK )  GO TO 4312                               M14.2952
      TBG(I) = TBG(I-1)                                                 M14.2953
 4312 IF ( TBD(I)  .NE.  0.0 )  GO TO 4320                              M14.2954
      DECODE (80, 4313, ABUFF)  BUS1, BUS2                              M14.2955
 4313 FORMAT ( 26X, 2A4 )                                               M14.2956
      IF ( BUS1  .NE.  BLANK )  GO TO 4320                              M14.2957
      IF ( BUS2  .NE.  BLANK )  GO TO 4320                              M14.2958
      TBD(I) = TBD(I-1)                                                 M14.2959
 4320 IF ( H1  .NE.  0.0 )  GO TO 4227                                  M14.2960
      DECODE (80, 4321, ABUFF)  BUS1, BUS2                              M14.2961
 4321 FORMAT ( 42X, 2A4 )                                               M14.2962
      IF( BUS1  .NE.  BLANK )  GO TO 4227                               M14.2963
      IF( BUS2  .NE.  BLANK )  GO TO 4227                               M14.2964
      H1 = H2                                                           M14.2965
 4227 IF ( H2  .NE.  0.0 )  GO TO 4235                                  M14.2966
      DECODE (80, 4228, ABUFF)  BUS1, BUS2                              M14.2967
 4228 FORMAT ( 50X, 2A4 )                                               M14.2968
      IF ( BUS1  .NE.  BLANK )  GO TO 4235                              M14.2969
      IF ( BUS2  .NE.  BLANK )  GO TO 4235                              M14.2970
      H2 = H1                                                           M14.2971
 4235 TBY(I) = (H1 +H2 +H2) / 3.0                                       M14.2972
      GO TO MOON, (4236, 711)                                           M14.2973
 4236 IF( TBX(I)  .NE.  0.0 )  GO TO 4239                               M14.2974
      IF( TBY(I)  .NE.  0.0 )  GO TO 4239                               M14.2975
      WRITE (KUNIT6, 4268)                                              M35.9075
 4268 FORMAT (  40H+BLANK CARD TERMINATING CONDUCTOR CARDS.   )         M14.2977
      GO TO 6                                                           M14.2978
 4239 WRITE (KUNIT6, 4240)   TBTB2(I), TBR(I), ITBTB3(I)                M35.9076
 4240 FORMAT ( 21H+LINE-CONDUCTOR CARD.,  2E11.3, I6  )                 M20.5074
      IF (I3 .LE. 1)   GO TO 706                                        M14.2981
      NBUNDL = 1                                                                
      I4 = 1                                                            M14.2982
      XX = TBX(I)                                                       M14.2983
      YY = TBY(I)                                                       M14.2984
      ANGL = (PI - TWOPI  / I3) / 2.0                                   M14.2985
      DANGL = TWOPI  / I3                                               M14.2986
      RADIUS = D8 / (24.0 * COSZ(ANGL))                                 M18. 928
      IF( METRIK .EQ. 1 ) RADIUS = RADIUS * .12D0                       M32, 300
      D9R = D9 / PICON                                                  M14.2988
  703 TBX(I) = XX +RADIUS * COSZ(D9R -DANGL * I4)                       M18. 929
      TBY(I) = YY +RADIUS * SINZ(D9R -DANGL * I4)                       M18. 930
      IF (I4 .EQ. I3)   GO TO 706                                       M14.2991
      I4 = I4 +1                                                        M14.2992
      I = I +1                                                          M14.2993
      ITBIC(I) = ITBIC(I-1)                                             M14.2994
      TBTB2(I) = TBTB2(I-1)                                             M14.2995
      TBR(I) = TBR(I-1)                                                 M14.2996
      ITBTB3(I) = ITBTB3(I-1)                                           M14.2997
      TBG(I) = TBG(I-1)                                                 M14.2998
      TBD(I) = TBD(I-1)                                                 M14.2999
      TBTEXT(I) = TBTEXT(I-1)                                           M14.3000
      GO TO 703                                                         M14.3001
  706 IF (I .LE. LPHASE )   GO TO 5                                     M14.3003
    7 KILL = 82                                                         M14.3004
      LSTAT(15) = LPHASE                                                M36.3596
      LSTAT(19) = 7                                                     M14.3005
      GO TO 9200                                                        M14.3006
C                                         SORTING OF CONDUCTOR DATA     M14.3008
    6 TBD(I) = 0.0                                                      M14.3009
      IF( IPRSUP .GE. 1 )                                               M14.3010
     1 WRITE (LUNIT6, 3625) (ITBIC(K), ITBTB3(K), TBTB2(K), TBR(K),     M17.1279
     2 TBG(K), TBD(K), TBX(K), TBY(K), TBTEXT(K), K=1, I )              M14.3012
 3625 FORMAT ( //,  25H UNSORTED CONDUCTOR TABLE   ,/,                  M14.3013
     1 ( 1X, 2I6, 6E15.5, 5X, A6 ) )                                    M14.3014
C              '      '      '    CONVERT FROM SI UNITS FOR CALCULATION.M14.3015
      IF (METRIK .NE. 1 ) GOTO 981                                      M14.3016
      DO 982 K = 1 , I                                                  M14.3017
      TBD(K) = TBD(K) * FINPCM                                          M14.3018
      TBX(K) = TBX (K) * FTPM                                           M14.3019
      TBY(K) = TBY(K) * FTPM                                            M14.3020
      TBR(K) = TBR(K) / FMIPKM                                          M14.3021
      JB = ITBTB3(K)                                                    M14.3022
      IF ( JB  .LT.  2 )   TBG(K) = (TBG(K) + FSPAC) / FMIPKM           M28.7184
      IF ( JB .EQ. 2 ) TBG(K) = TBG(K) * FINPCM                         M14.3024
  982 CONTINUE                                                          M14.3025
  981 CALL MOVE0 ( IC(1), LPHASE )                                      M14.3026
      KTOT=0                                                            M14.3027
      KCIRCT=0                                                          M14.3028
      K=0                                                               M14.3029
   10 K=K+1                                                             M14.3030
      IF(TBD(K).EQ.0.) GO TO 15                                         M14.3031
      I1=ITBIC(K)                                                       M14.3032
      IF(I1.EQ.0) GO TO 12                                              M14.3033
      IF(I1.LT.0) GO TO 10                                              M14.3034
      IF(I1.GT.KCIRCT) KCIRCT=I1                                        M14.3035
      I = LPHPL1 - I1                                                   M14.3036
      IF(IC(I).EQ.0) GO TO 13                                           M14.3037
   12 KTOT=KTOT+1                                                       M14.3038
      I=KTOT                                                            M14.3039
      IF(I1.EQ.0) GO TO 13                                              M14.3040
      IP=KTOT                                                           M14.3041
   18 J=I-1                                                             M14.3042
      IF(J.EQ.0) GO TO 13                                               M14.3043
      IF(IC(J).GT.0) GO TO 13                                           M14.3044
      I=J                                                               M14.3045
      IC(IP)=IC(I)                                                      M14.3046
      TB2(IP)=TB2(I)                                                    M14.3047
      ITB3(IP)=ITB3(I)                                                  M14.3048
      R(IP)=R(I)                                                        M14.3049
      D(IP)=D(I)                                                        M14.3050
      GMD(IP)=GMD(I)                                                    M14.3051
      X(IP)=X(I)                                                        M14.3052
      Y(IP)=Y(I)                                                        M14.3053
      TEXT(IP)=TEXT(I)                                                  M14.3054
      IP=IP-1                                                           M14.3055
      GO TO 18                                                          M14.3056
   13 KFULL=KTOT+KCIRCT                                                 M14.3057
      IF(KFULL.GT.LPHASE ) GO TO 7                                      M14.3058
      IC(I)=I1                                                          M14.3059
      TB2(I)=TBTB2(K)                                                   M14.3060
      ITB3(I)=ITBTB3(K)                                                 M14.3061
      R(I)=TBR(K)                                                       M14.3062
      D(I)=TBD(K)                                                       M14.3063
      GMD(I)=TBG(K)                                                     M14.3064
      X(I)=TBX(K)                                                       M14.3065
      Y(I)=TBY(K)                                                       M14.3066
      TEXT(I)=TBTEXT(K)                                                 M14.3067
      GO TO 10                                                          M14.3068
   15 IPRINT=0                                                          M14.3069
      NTOL = 0                                                          M43.3332
   16 IF ( IK  .GT.  0 )   GO TO 3006                                   M16.4457
C     READ INPUT CARD USING CIMAGE                                      M14.3070
33316 CALL CIMAGE                                                       M16.4458
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4180
      REWIND LUNT13                                 
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.4459
      IF ( IALTER  .NE.  2 )                                            M16.4460
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1280
      DECODE (80, 17, ABUFF)                                            M14.3072
     1           REARTH,FREQ,CORR,J1,J2,J3,J4,J5,J6,J7,J8,J9,J10,J11,J12M14.3073
     2 , IW,DIST,J13,J14,J15,J16,ISEGM,MUTUAL,IK,IPS,J17,IMODAL,ITRNSF, M43.3333
     3   CONDUC                                                                 
   17 FORMAT(F8.2,F10.2,F10.6,I2,5I1,I2,5I1,I2,F8.3,I2,5I1,3I3,         M29.4183
     1  2I2, E8.2)                                                      M43.3335
      IF (METRIK .EQ. 1)  CONDUC = CONDUC /FMIPKM                               
      IF ( CONDUC .EQ. 0 )  CONDUC = 3.22D-9                                    
      IF ( MUNTRN .EQ. 1 )  IMODAL = 1                                          
      IF ( MTRNSP .EQ. 1  .OR.  MSPEDB .EQ. 1 )  IMODAL = 0             M44. 842
      IF ( KEXACT .NE. 88333 )  GO TO 3168
      FREQ = FMINFS
      IK = ALOG1Z ( FMAXFS / FMINFS ) + EPSILN
      IPS = VALU14 / ALOGZ(-DELFFS)  + EPSILN
      DISTSV = DIST
      GO TO 217
 3168 IF ( LASTOV .EQ. 1  .OR. IMODAL .EQ. 1 )  GO TO 217               
      IF ( LASTOV .NE. 39 )  GO TO 217                                  M43.3339
      GMODE = CONDUC                                                            
      IF ( METRIK .EQ. 1 )  GMODE = GMODE * FMIPKM                      M43.3341
  217 JSPECL = J1 + J2 + J3 + J4 + J5 + J6 + J7 + J8 + J9               M43.3342
     1        +J10 + J11 + J12                                          M43.3343
      NFREQ = NFREQ + 1                                                 M30.1240
      IF ( LASTOV .NE. 1 .AND. IMODAL .EQ. 0 .AND. NFREQ .EQ. 2 .AND.   M44. 843
     1  IK .EQ. 0 .AND. IPS .EQ. 0 .AND. LASTOV .NE. 45) GO TO 33316            
      J5OUT = J5                                                        M28.7185
      J8OUT = J8                                                        M28.7186
      J2OUT = J2                                                        M29.4185
      J4OUT = J4                                                        M29.4186
      J6OUT = J6                                                        M31.6390
      J9OUT = J9                                                        M28.7190
      IF ( IMODAL .NE. 0 ) GO TO 117                                    M43.3344
      J5 = 1                                                            M43.3345
      J9 = 1                                                            M43.3346
  117 IF (J17 .NE. 44)  GO TO 28725                                     M43.3347
      J5 = 1                                                            M30.1242
      J8 = 1                                                            M30.1243
      IW = 1                                                            M30.1244
28725 IF (IMODAL .LE. 0 ) GO TO 18725                                   M30.1245
      J2 = 1                                                            M29.4192
      J8 = 1                                                            M29.4193
18725 IF ( IK  .LE.  0 )   GO TO 8726                                   M29.4194
      J6 = 1                                                            M28.7192
      J9 = 1                                                            M28.7193
 8726 IF ( CK1  .NE.  -FLTINF )   GO TO 7446                            M28.7194
      IF ( DIST  .LE.  0.0 )   GO TO 7446                               M16.4463
      CK1 = DIST                                                        M16.4464
      IF( METRIK .EQ. 1 ) CK1 = CK1 * FMIPKM                            M22.6170
 7446 IF ( CI1  .NE.  -FLTINF )   GO TO 7449                            M16.4465
      IF ( REARTH  .LE.  0.0 )   GO TO 7449                             M16.4466
      CI1 = REARTH                                                      M16.4467
 7449 IF ( MUTUAL  .GT.  0 )   J8 = 1                                   M16.4468
      IF ( KOLBEG  .GT.  0 )   GO TO 4264                               M22.6171
      IF ( CORR  .NE.  0.0 )  GO TO 4264                                M14.3077
      DECODE (80, 4256, ABUFF)  BUS1, BUS2                              M14.3078
 4256 FORMAT ( 18X, A6, A4 )                                            M14.3079
      IF( BUS1 .NE. BLANK )  GO TO 4264                                 M14.3080
      IF( BUS2 .NE. BLANK )  GO TO 4264                                 M14.3081
      CORR = TENM6                                                      M14.3082
 4264 IPRINT = IPRINT + 1                                               M14.3083
      IF ( REARTH  .NE.  0.0 )   GO TO 4267                             M16.4469
      WRITE (KUNIT6, 4266)                                              M35.9077
 4266 FORMAT (  40H+BLANK CARD TERMINATING FREQUENCY CARDS.   )         M14.3086
      IF ( KEXACT .NE. 88333)    GO TO 2 
      LASTOV = 44
C      NCHAIN = 8
      NCHAIN = 2
      GO TO 9900      
 4267 IF ( IALTER .EQ. 2)  REARTH = CI1                                 M28.7195
      WRITE (KUNIT6, 3618)   REARTH, FREQ, DIST                         M35.9078
 3618 FORMAT ( 16H+FREQUENCY CARD.,  3E11.3  )                          M20.5077
      CALL INTERP                                                       M35.9079
      IF( METRIK .EQ. 1 ) DIST = DIST * FMIPKM                          M22.6172
      IPUNCH = J17                                                      M19.2427
      JPRALT = 1                                                        M16.4471
      IF ( IALTER  .EQ.  0 )   GO TO 3729                               M16.4472
      DO 3721  JJ=1, 16                                                 M29.4195
      IF (JPRMAT (JJ) .NE. 0)   GO TO 13721                             M19.2428
 3721 CONTINUE                                                          M16.4475
      JPRALT = 0                                                        M16.4476
      IF (IALTER .EQ. 3)   GO TO 13723                                  M19.2429
 3723 JPRMAT(5) = 1                                                     M16.4477
      JPRMAT(8) = 1                                                     M16.4478
      IW = 0                                                            M16.4479
      GO TO 3729                                                        M19.2430
13721 IF (IALTER .NE. 3)   GO TO 3723                                   M19.2431
13723 JPRMAT (6) = 1                                                    M19.2432
      JPRMAT(9) = 1                                                     M19.2433
 3729 KKK = 0                                                           M16.4480
      IF ( IPRSUP  .GE.  1 )                                            M16.4481
     1 WRITE (LUNIT6, 7454)  IPRINT, LDISFR, IALTER                     M17.1283
 7454 FORMAT ( /,  44H NEW FREQUENCY CARD.  IPRINT  LDISFR  IALTER, /,  M16.4483
     1 20X, 3I8  )                                                      M16.4484
      DO 3731  JJ = 1, 16                                               M29.4196
      IF (JPRMAT (JJ) .NE. 0)  GO TO 3733                               M22.6174
 3731 CONTINUE                                                          M22.6175
      GO TO 33316                                                       M22.6176
 3733 IF (FREQ .EQ. 0.0)  FREQ = STATFR                                 M22.6177
      IF ( IK  .GT.  0 )   GO TO 7456                                   M16.4488
      IF ( IALTER  .EQ.  3 )   GO TO 7453                               M28.7197
      IF ( LASTOV  .EQ.  1 )   GO TO 7453                               M31.6391
      VOLTI(IPRINT) = REARTH                                            M16.4490
      IF ( IALTER .EQ. 2 )  GO TO 7453                                  M28.7198
      VOLTK(IPRINT) = FREQ                                              M16.4491
      IF ( IPRINT  .LE.  LDISFR )   GO TO 7453                          M16.4492
      KILL = 170                                                        M16.4493
      LSTAT(14) = LDISFR                                                M16.4494
      LSTAT(19) = 7456                                                  M16.4495
      GO TO 9200                                                        M16.4496
 7453 GO TO 3007                                                        M16.4497
 7456 IF ( IPS  .EQ.  0 )   IPS = 1                                     M16.4498
      IF (IPS*IK .LE. 399)   GO TO 7458                                 M19.2434
C     THIS CHECK REALLY BELONGS IN "SEMLYEN SETUP" (OVERLAY 45):        M31.6392
      IF ( LASTOV  .NE.  45 )   GO TO 7458                              M31.6393
      KILL = 160                                                        M19.2435
      LSTAT(14) = 399                                                   M19.2436
      LSTAT(19) = 7458                                                  M19.2437
      GO TO 9200                                                        M19.2438
 7458 CONTINUE                                                          M19.2439
      FACTOR = IPS                                                      M34. 185
      FACTOR = VALU14 / FACTOR                                          M34. 186
      FDECAD=FREQ                                                       M14.3094
      IF ( LASTOV .EQ. 1  .AND. IMODAL .NE. 0)  GO TO 7459              M39.3589
      DIST=0.                                                           M14.3097
 7459 III=0                                                             M39.3590
      VOLTBC(1) = FREQ                                                  M31.6394
      VOLTBC(2) = FREQ * 10.**IK                                        M31.6395
      VOLTBC(3) = EXPZ(FACTOR)                                          M31.6396
      D13 = VOLTBC(2) / VOLTBC(1)                                       M31.6397
      VOLTBC(4) = ALOGZ(D13) / ALOGZ(VOLTBC(3)) + 1.5                   M31.6398
      VOLTBC(5) = IK                                                    M32, 303
      VOLTBC(6) = IPS                                                   M32, 304
      ICHECK = IPRINT - 1                                               M16.4502
      IPRINT = ICHECK                                                   M16.4503
      KTAB = KCIRCT                                                     M39.3591
      NMODE = 2                                                         M44. 845
      IF ( KCIRCT .LT. 2 )  NMODE = 1                                   M44. 846
      IF ( IPRSUP  .GE.  1 )                                            M16.4505
     1 WRITE (LUNIT6, 7457) ICHECK, KTAB, (VOLTBC(JJ), JJ=1, 3)         M39.3592
 7457 FORMAT ( /, 39H LOGARITHMIC FREQUENCY.  ICHECK    KTAB,           M39.3593
     1 8X,  7HVOLT(1),  8X,  7HVOLT(2),  8X,  7HVOLT(3),  /,            M16.4508
     2  23X,  2I8,  3E15.6  )                                           M16.4509
      IF ( MFRQPR  .GT.  0 )                                            M31.6400
     1 WRITE (LUNIT6, 3004)                                             M31.6401
      GO TO 3006                                                        M16.4510
 3005 III=III+1                                                         M14.3101
CCCC      IF ( III  .GE.  IK )   GO TO 33316                            M43.3348
      IF (III .GE. IK) GO TO 8801                                       M43.3349
      GO TO 8802                                                        M43.3350
 8801 IF ( KEXACT .NE. 88333 )  GO TO 8701
      IF ( NFQPL1 .EQ. 1 )  GO TO 8701
      NFQPL1 = 1
      GO TO 8802
 8701 IF (NFREQ .NE. 3) GO TO 8800         
CC    ** IDENTICAL EIGENVALUE ? **                                      M43.3352
      IF (NRP.GT.0 .OR. ITRNSF .EQ. 0) GO TO 8800                               
      write (*,*) '$$  MRR,NTOL,NFREQ ARE  : ', MRR,NTOL,NFREQ          M43.3354
      RTIO=(MRR*1.0)/NTOL                                               M43.3355
      IF (RTIO.LT.0.75) GO TO 8800                                      M43.3356
      NRP=1                                                             M43.3357
      FREQ=FDECAD                                                       M43.3358
      DO 8804 I=1,NTOL*KTAB                                             M43.3359
      BACKSPACE LUNIT9                                                  M43.3360
 8804 CONTINUE                                                          M43.3361
      NTOL=0                                                            M43.3367
      GO TO 16                                                          M43.3368
 8800 GO TO 33316                                                       M43.3369
 8802 FDECAD=FDECAD*10.0                                                M43.3370
      KKK = 1                                                           M16.4512
 3006 PKKK = KKK                                                        M34. 187
      FREQ = FDECAD * EXPZ( PKKK * FACTOR )                             M34. 188
      IPRINT=IPRINT+1                                                   M14.3106
      IF ( IPRSUP  .GE.  1 )                                            M16.4513
     1 WRITE (LUNIT6, 23006)  IPRINT, KKK, III, FREQ, CI1, CK1          M17.1285
23006 FORMAT ( /,                                                       M16.4515
     1 63H NEXT LOGARITHMICALLY-SPACED FREQUENCY.  IPRINT     KKK     IIM16.4516
     2I, 11X, 4HFREQ, 12X, 3HCI1, 12X, 3HCK1,  /,                       M16.4517
     3 39X, 3I8, 3E15.6  )                                              M16.4518
      IF(KKK.GT.IPS) GO TO 3005                                         M14.3108
      IF ( FREQ+EPSILN .LT. FMAXFS  .OR. KEXACT .NE. 88333)  GO TO 8806
      IF ( NFQPL1 .EQ. 1 )  KKK = IPS
 8806 KKK = KKK + 1                                                   
 3007 OMEGA=TWOPI    *FREQ                                              M14.3109
      IF ( IK  .GT.  0 )   GO TO 3008                                   M16.4520
      WRITE (LUNIT6, 4)                                                 M17.1286
    4 FORMAT ( //,  59H LINE-CONDUCTOR TABLE AFTER SORTING AND INITIAL PM14.3111
     1ROCESSING.    ,/, 1X, 5HTABLE, 3X, 5HPHASE, 3X, 11HSKIN EFFECT,   M14.3112
     2 4X, 10HRESISTANCE, 3X, 28HREACTANCE-DATA SPECIFICATION, 5X,      M14.3113
     3 8HDIAMETER, 3X, 10HHORIZONTAL, 3X, 10HAVG HEIGHT, 3X,            M14.3114
     4 9HCONDUCTOR  )                                                   M15.1607
    8 FORMAT( 3X, 3HROW, 2X, 6HNUMBER, 8X,                              M14.3116
     5 6HR-TYPE, 4X, 10HR (OHM/MI), 3X, 6HX-TYPE, 6X,                   M14.3117
     6 16HX(OHM/MI) OR GMR, 5X, 8H(INCHES), 5X, 8HX (FEET),             M14.3118
     7 5X, 8HY (FEET), 8X, 4HNAME   )                                   M14.3119
      IF ( METRIK .EQ. 0 ) WRITE(LUNIT6,8)                              M17.1287
      IF ( METRIK .EQ. 1 ) WRITE(LUNIT6,9)                              M17.1288
    9 FORMAT(3X, 3HROW, 2X, 6HNUMBER, 8X,                               M14.3122
     5 6HR-TYPE, 4X, 10HR (OHM/KM), 3X, 6HX-TYPE, 6X,                   M14.3123
     6 16HX(OHM/KM) OR GMR, 5X, 8H(  CM  ), 5X, 8HX (MTRS),             M14.3124
     7 5X, 8HY (MTRS), 8X, 4HNAME   )                                   M14.3125
 3008 IDIST = 1                                                         M16.4521
      IF ( DIST  .EQ.  0.0  .AND. IPUNCH  .NE. 44 )  GO TO 4271         M30.1247
      IF ( KCIRCT  .EQ.  0 )  GO TO 4271                                M14.3128
      IF ( KCIRCT  .LT.  LPHD2 )  GO TO 4275                            M14.3129
 4271 IDIST = 0                                                         M14.3130
 4275 J56 = J5+J6 + IDIST                                               M14.3131
C                                                                       M14.3132
C                                                  FORMATION OF P-MATRIXM14.3133
      IP=0                                                              M14.3134
      K=0                                                               M14.3135
      F1=UNITY/OMEGA                                                    M14.3136
      IF(IW.GT.0) F1=UNITY                                              M14.3137
      F1 = F1 * VALU1                                                   M14.3138
      F2=F1*2.0                                                         M14.3139
   20 K=K+1                                                             M14.3140
      IF(K.GT.KFULL) GO TO 30                                           M14.3141
      IF(K.LE.KCIRCT) GO TO 28                                          M14.3142
      J=K-KCIRCT                                                        M14.3143
   21 X1=X(J)                                                           M14.3144
      Y1=Y(J)                                                           M14.3145
      D1=D(J)                                                           M14.3146
      IF(IPRINT.NE.1) GO TO 222                                         M14.3147
      IF ( METRIK .EQ. 0 ) GOTO 221                                     M14.3148
      RJ = R(J) * FMIPKM                                                M14.3149
      YJ = Y(J) / FTPM                                                  M14.3150
      XJ = X(J) / FTPM                                                  M14.3151
      DJ = D(J) / FINPCM                                                M14.3152
      GMDJ = GMD(J)                                                     M28.7199
      JB = ITB3(J)                                                      M28.7200
      IF ( JB  .LT.  2 )   GMDJ = GMDJ * FMIPKM - FSPAC                 M28.7201
      IF ( JB  .EQ.  2 )   GMDJ = GMDJ / FINPCM                         M28.7202
      WRITE(LUNIT6,22) K,IC(J),TB2(J),RJ,JB, GMDJ, DJ,XJ,YJ,TEXT(J)     M28.7203
      GOTO 225                                                          M14.3154
  221 WRITE(LUNIT6,22) K,IC(J),TB2(J),R(J),ITB3(J),GMD(J),D(J),X(J),Y(J)M17.1290
     1  , TEXT(J)                                                       M28.7204
   22 FORMAT ( 1X, I5, I8, 2F14.5, I9, F22.6, F13.5, 2F13.3, 6X, A6 )   M14.3157
  225 IF(ITB3(J).NE.4) GO TO 222                                        M14.3158
      IF(TB2(J).GT.0.) GO TO 224                                        M14.3159
      KILL = 83                                                         M14.3160
      LSTAT(15) = J                                                     M14.3161
      LSTAT(19) = 224                                                   M14.3162
      GO TO 9200                                                        M14.3163
  224 GMD(J)=D(J)                                                       M14.3164
  222 I=0                                                               M14.3165
   23 I=I+1                                                             M14.3166
      IP=IP+1                                                           M14.3167
      IF(I.EQ.K) GO TO 24                                               M14.3168
      J=I-KCIRCT                                                        M14.3169
      IF ( I .LE. KCIRCT )  J = LPHPL1 - I                              M14.3170
      DX=(X(J)-X1)**2                                                   M14.3171
      H1=Y(J)-Y1                                                        M14.3172
      H2=Y(J)+Y1                                                        M14.3173
      IF(DX.EQ.0.0 .AND. H1.EQ.0.0) GO TO 26                            M14.3174
      R1=ALOGZ((DX+H2*H2)/(DX+H1*H1))                                   M18. 934
      P(IP)=R1*F1                                                       M14.3176
      Z(IP)=R1                                                          M14.3177
      GO TO 23                                                          M14.3178
   24 P(IP)=ALOGZ(48.*Y1/D1)*F2                                         M18. 935
      GO TO 20                                                          M14.3180
   26 KILL = 84                                                         M14.3181
      LSTAT(15) = I                                                     M14.3182
      LSTAT(19) = 28                                                    M14.3183
      GO TO 9200                                                        M14.3184
   28 J = LPHPL1 - K                                                    M14.3185
      IF(IC(J).NE.0) GO TO 21                                           M14.3186
      KILL = 85                                                         M14.3187
      LSTAT(19) = 30                                                    M14.3188
      LSTAT(15) = K                                                     M14.3189
      GO TO 9200                                                        M14.3190
   30 IF (KKK .GT. 0 .AND. LASTOV .NE. 1) GO TO 3020                    M43.3371
      IF ( JSPECL .EQ. 0 )  GO TO 7025                                  M43.3372
      WRITE(LUNIT6,25)REARTH,FREQ,CORR                                  M17.1292
   25 FORMAT(//, 46H0FOLLOWING MATRICES ARE FOR EARTH RESISTIVITY=,F8.2,M14.3193
     121H OHM-M AND FREQUENCY=,F13.2,23H HZ. CORRECTION FACTOR=,        M14.3194
     1 F10.6)                                                           M14.3195
 7025 IF ( KKK .GT. 1 )  GO TO 3020                                     M43.3373
      IF(ISEGM.GT.0) WRITE(LUNIT6,12345)                                M17.1293
12345 FORMAT(1H ,30X,53H************EARTH WIRES WILL BE SEGMENTED*******M14.3197
     1*****)                                                            M14.3198
      IF(IK.GT.0) WRITE(LUNIT6,3004)                                    M17.1294
 3004 FORMAT(1H0,6X,47H-----------------ZERO SEQUENCE-----------------, M37.9093
     113X,47H---------------POSITIVE SEQUENCE---------------,/,4X,      M14.3201
     25HALPHA,8X,4HBETA,8X,1HR,8X,8HL(MILLI-,4X,8HC(MICRO-,6X,5HALPHA,8XM14.3202
     3,4HBETA,8X,1HR,8X,8HL(MILLI-,4X,8HC(MICRO-,4X,9HFREQUENCY,/,      M14.3203
     4 59H NEPER/MILE  RADIAN/MILE  OHM/MILE  HENRY/MILE) FARAD/MILE)   M14.3204
     5,61H  NEPER/MILE  RADIAN/MILE  OHM/MILE   HENRY/MILE) FARAD/MILE) M43.3374
     6 5X,2HHZ )                                                        M14.3206
 3020 WRITE (LUNT13)  (Z(I),I=1,IP)                        
      IF ( IPRSUP  .GE.  2 )                                            M14.3208
     1 WRITE (LUNIT6, 4427)  ( Z(I), I=1, IP )                          M17.1296
 4427 FORMAT ( /, 28H AT 4427.  (Z(I), I=1, IP) .   ,/,                 M14.3210
     1 ( 1X, 8E16.6 ) )                                                 M14.3211
      IF ( IPRSUP  .GE.  1 )                                            M14.3212
     1 WRITE (LUNIT6, 4428)  J1, J4, KCIRCT, J2, J3, J56, KFULL         M17.1297
 4428 FORMAT ( /, 9H AT 4428.  , 7I12 )                                 M14.3214
C                                                                       M14.3215
C                                   REDUCTIONS AND INVERSIONS P-MATRIX  M14.3216
      SWITCH=-UNITY                                                     M14.3217
      IF(J1.GT.0) GO TO 36                                              M14.3218
   31 IF(J4.GT.0) GO TO 37                                              M14.3219
   32 IF(KCIRCT.EQ.0) GO TO 80                                          M14.3220
      IF(J2+J3+J56  .EQ.0) GO TO 80                                     M14.3221
C                          BEGIN ELIMINATION OF EARTH WIRES AND BUNDLINGM14.3222
   49 K=KCIRCT                                                          M14.3223
   50 K=K+1                                                             M14.3224
      IF(K.GT.KFULL) GO TO 56                                           M14.3225
      I=K-KCIRCT                                                        M14.3226
      I=IC(I)                                                           M14.3227
      IF(I.EQ.0) GO TO 50                                               M14.3228
      I2=I*(I-1)/2                                                      M14.3229
      K2=K*(K-1)/2                                                      M14.3230
      KK=K2+I                                                           M14.3231
      H1=P(KK)                                                          M14.3232
      H2=Z(KK)                                                          M14.3233
      KK=K2+K                                                           M14.3234
      L=0                                                               M14.3235
   51 IF(L.LT.I) GO TO 54                                               M14.3236
      I2=I2+L                                                           M14.3237
   52 IF(L.LT.K) GO TO 55                                               M14.3238
      K2=K2+L                                                           M14.3239
   53 L=L+1                                                             M14.3240
      P(K2)=P(K2)-P(I2)                                                 M14.3241
      IF(SWITCH.GT.0.)Z(K2)=Z(K2)-Z(I2)                                 M14.3242
      IF(L.NE.KFULL) GO TO 51                                           M14.3243
      P(KK)=P(KK)-H1                                                    M14.3244
      IF(SWITCH.GT.0.) Z(KK)=Z(KK)-H2                                   M14.3245
      GO TO 50                                                          M14.3246
   54 I2=I2+1                                                           M14.3247
      GO TO 52                                                          M14.3248
   55 K2=K2+1                                                           M14.3249
      GO TO 53                                                          M14.3250
   56 KP=KCIRCT*(KCIRCT+1)/2                                            M14.3251
      IF(SWITCH.GT.0.) GO TO 202                                        M14.3252
      CALL REDU44 ( P(1), WORKR1(1), KFULL, KCIRCT )                    M14.3253
CCCC      write (*,*) ' After REDU44 on C.  P(1:6) =',                  M43.3375
CCCC     1                                ( P(I), I=1, 6 )              M43.3376
      IF( KILL .GT. 0 )  GO TO 9200                                     M14.3254
C                            END ELIMINATION OF EARTH WIRES AND BUNDLINGM14.3255
CCCC      write (*,*) ' Branch to 38?  j2, j3 =',  j2, j3               M43.3377
      IF(J2+J3.GT.0) GO TO 38                                           M14.3256
   33 IF(J56.GT.0) GO TO 39                                             M14.3257
C                                                                       M14.3258
C                                         FORMATION OF Z-MATRIX         M14.3259
   80 SWITCH=+UNITY                                                     M14.3260
      J56=J8+J9+J11+J12+IDIST                                           M14.3261
      IF(J56+J7+J10.EQ.0) GO TO 600                                     M14.3262
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4198
      REWIND LUNT13                                    
      K=0                                                               M14.3264
      IF(REARTH.EQ.0.) GO TO 100                                        M14.3265
      F1 = VALU2 * SQRTZ( FREQ/REARTH )                                 M18. 936
  100 F2 = OMEGA * VALU3                                                M14.3267
      IMAX=30                                                           M14.3268
      IF(CORR.EQ.0. .OR.REARTH.EQ.0.) IMAX=-1                           M14.3269
      IF ( CORR   .GT.   CORCHK )   IMAX = CORR - ONEHAF                M14.3270
      IF(IMAX.GT.31) IMAX=30                                            M14.3271
      READ (LUNT13)  (P(I),I=1,IP)                
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4201
      REWIND LUNT13                                 
      IP=0                                                              M14.3274
  120 K=K+1                                                             M14.3275
      IF(K.GT.KFULL) GO TO 200                                          M14.3276
      IF(K.LE.KCIRCT) GO TO 129                                         M14.3277
      J=K-KCIRCT                                                        M14.3278
      GO TO 121                                                         M14.3279
  129 J = LPHPL1 - K                                                    M14.3280
  121 X1=X(J)                                                           M14.3281
      Y1=Y(J)                                                           M14.3282
      R1=R(J)                                                           M14.3283
      H1=TB2(J)                                                         M14.3284
      H2=UNITY-2.0*H1                                                   M14.3285
      IF(H1.GT.0.) CALL SKIN(H2,R1,FREQ,R1,H1)                          M14.3286
      G1=GMD(J)                                                         M14.3287
      IX=ITB3(J)                                                        M14.3288
      IZERO=1                                                           M14.3289
      XM=0.                                                             M14.3290
      IF(IC(J).EQ.0 .AND. ISEGM.GT.0) IZERO=0                           M14.3291
      IF(IX.LT.2) GO TO 150                                             M14.3292
      IF(IX.EQ.3) G1=G1*D(J)/2.0                                        M14.3293
      XS=ALOGZ(24.0*Y1/G1)*ONEHAF                                       M18. 937
  122 IF(IX.NE.4) GO TO 125                                             M14.3295
      XS = XS + H1/F2 - ONEHAF*ALOGZ( D(J)*ONEHAF/G1 )                  M18. 938
  125 I=0                                                               M14.3297
  123 I=I+1                                                             M14.3298
      IP=IP+1                                                           M14.3299
      IF(I.EQ.K) GO TO 124                                              M14.3300
      J=I-KCIRCT                                                        M14.3301
      IF ( I  .LE.  KCIRCT )                                            M15.1608
     1 J = LPHPL1- I                                                    M15.1609
      RM=0.                                                             M14.3303
      IF(IZERO.EQ.0) GO TO 126                                          M14.3304
      XM=P(IP)/4.0                                                      M14.3305
      IF(IMAX.LT.0) GO TO 126                                           M14.3306
      DX=ABSZ(X(J)-X1)                                                  M18. 939
      H2=Y(J)+Y1                                                        M14.3308
      S=SQRTZ(H2*H2+DX*DX)                                              M18. 940
      Z1=S*F1                                                           M14.3310
      IF(Z1.GT.5.0) GO TO 151                                           M14.3311
C                    BEGIN CARSON SERIES FOR MUTUAL IMPEDANCE IF Z.LE.5.M14.3312
      RM = TWOPI / 16.                                                  M14.3313
      ZL=ALOGZ(Z1)                                                      M18. 941
      XM = XM  +  ( VALU4 - ZL ) * ONEHAF                               M14.3315
      IF(IMAX.EQ.0)GO TO 126                                            M14.3316
      S1=DX/S                                                           M14.3317
      C1=H2/S                                                           M14.3318
      CS=C1*Z1                                                          M14.3319
      SN=S1*Z1                                                          M14.3320
      IF (IMAX .GT. 1)   PHI = ASINZ(S1)                                M18. 942
      M5=0                                                              M14.3322
      ERROR=0.                                                          M14.3323
 1110 K5=0                                                              M14.3324
      I5=M5*4                                                           M14.3325
 1111 K5=K5+1                                                           M14.3326
      I5=I5+1                                                           M14.3327
      IF(K5.EQ.1.OR.K5.EQ.3) GO TO 1113                                 M14.3328
      DELTAP=((CCARS(I5)-ZL)*CS+PHI*SN)*BCARS(I5)                       M14.3329
      H1=-DCARS(I5)*CS                                                  M14.3330
      IF(K5.EQ.4) GO TO 1114                                            M14.3331
      DELTAQ=H1                                                         M14.3332
 1004 RM=RM+DELTAP                                                      M14.3333
      XM=XM+DELTAQ                                                      M14.3334
      IF(I5.EQ.IMAX) GO TO 126                                          M14.3335
      IF ( CORR   .GT.   CORCHK )   GO TO 1005                          M14.3336
      IF(ABSZ(DELTAP).LT.CORR .AND. ABSZ(DELTAQ).LT.CORR) GO TO 1115    M18. 943
      ERROR = 0.                                                        M14.3338
 1005 H1=SN*C1+CS*S1                                                    M14.3339
      CS=(CS*C1-SN*S1)*Z1                                               M14.3340
      SN=H1*Z1                                                          M14.3341
      IF(K5.LT.4) GO TO 1111                                            M14.3342
      M5=M5+1                                                           M14.3343
      GO TO 1110                                                        M14.3344
 1113 DELTAQ=BCARS(I5)*CS                                               M14.3345
      DELTAP=DELTAQ                                                     M14.3346
      IF(K5.EQ.1) DELTAP=-DELTAP                                        M14.3347
      GO TO 1004                                                        M14.3348
 1114 DELTAQ=-DELTAP                                                    M14.3349
      DELTAP=H1                                                         M14.3350
      GO TO 1004                                                        M14.3351
 1115 IF ( ERROR  .GT.  ONEHAF )   GO TO 126                            M14.3352
      ERROR = UNITY                                                     M14.3353
      GO TO 1005                                                        M14.3354
C                      END CARSON SERIES FOR MUTUAL IMPEDANCE IF Z.LE.5.M14.3355
C                                                                       M14.3356
C                    BEGIN CARSON SERIES FOR MUTUAL IMPEDANCE IF Z.GT.5.M14.3357
  151 S=S*Z1                                                            M14.3358
      SN=DX/S                                                           M14.3359
      CS=H2/S                                                           M14.3360
      S2=SN*CS*2.0                                                      M14.3361
      C2=CS*CS-SN*SN                                                    M14.3362
      RM=-C2                                                            M14.3363
      DO 1006 I5=1,7,2                                                  M14.3364
      RM=RM+CCARS(I5)*CS                                                M14.3365
      XM=XM+DCARS(I5)*CS                                                M14.3366
      H1=CS*S2+SN*C2                                                    M14.3367
      CS=CS*C2-SN*S2                                                    M14.3368
 1006 SN=H1                                                             M14.3369
C                      END CARSON SERIES FOR MUTUAL IMPEDANCE IF Z.GT.5.M14.3370
  126 P(IP)=RM*F2                                                       M14.3371
      Z(IP)=XM*F2                                                       M14.3372
      IF ( IPRSUP  .GE.  4 )                                            M14.3373
     1 WRITE (LUNIT6, 4439)  I, K, KFULL, KCIRCT, I5, IP, P(IP), Z(IP)  M17.1301
 4439 FORMAT ( 9H AT 4439   , 6I10, 2E16.6  )                           M14.3375
      GO TO 123                                                         M14.3376
  124 IF(IMAX.LT.0) GO TO 128                                           M14.3377
      R1=R1/F2                                                          M14.3378
      Z1=Y1*F1*2.0                                                      M14.3379
      IF (Z1.GT.5.0) GO TO 152                                          M14.3380
C                    BEGIN CARSON SERIES FOR SELF IMPEDANCE IF Z.LE.5.0 M14.3381
      R1 = R1 + TWOPI/16.                                               M14.3382
      ZL=ALOGZ(Z1)                                                      M18. 944
      XS = XS  +  ( VALU4 - ZL ) * ONEHAF                               M14.3384
      IF(IMAX.EQ.0) GO TO 127                                           M14.3385
      M5=0                                                              M14.3386
      CS=Z1                                                             M14.3387
      ERROR=0.                                                          M14.3388
 2110 K5=0                                                              M14.3389
      I5=M5*4                                                           M14.3390
 2111 K5=K5+1                                                           M14.3391
      I5=I5+1                                                           M14.3392
      IF(K5.EQ.1 .OR. K5.EQ.3) GO TO 2113                               M14.3393
      DELTAP=(CCARS(I5)-ZL)*BCARS(I5)*CS                                M14.3394
      H1=-DCARS(I5)*CS                                                  M14.3395
      IF(K5.EQ.4) GO TO 2114                                            M14.3396
      DELTAQ=H1                                                         M14.3397
 2004 R1=R1+DELTAP                                                      M14.3398
      XS=XS+DELTAQ                                                      M14.3399
      IF(I5.EQ.IMAX) GO TO 127                                          M14.3400
      IF ( CORR   .GT.   CORCHK )   GO TO 2005                          M14.3401
      IF(ABSZ(DELTAP).LT.CORR .AND. ABSZ(DELTAQ).LT.CORR) GO TO 2115    M18. 945
      ERROR = 0.                                                        M14.3403
 2005 CS=CS*Z1                                                          M14.3404
      IF(K5.LT.4) GO TO 2111                                            M14.3405
      M5=M5+1                                                           M14.3406
      GO TO 2110                                                        M14.3407
 2113 DELTAQ=BCARS(I5)*CS                                               M14.3408
      DELTAP=DELTAQ                                                     M14.3409
      IF(K5.EQ.1) DELTAP=-DELTAP                                        M14.3410
      GO TO 2004                                                        M14.3411
 2114 DELTAQ=-DELTAP                                                    M14.3412
      DELTAP=H1                                                         M14.3413
      GO TO 2004                                                        M14.3414
 2115 IF ( ERROR  .GT.  ONEHAF )   GO TO 127                            M14.3415
      ERROR = UNITY                                                     M14.3416
      GO TO 2005                                                        M14.3417
C                      END CARSON SERIES FOR SELF IMPEDANCE IF Z.LE.5.0 M14.3418
C                                                                       M14.3419
C                    BEGIN CARSON SERIES FOR SELF IMPEDANCE IF Z.GT.5.0 M14.3420
  152 CS=UNITY/Z1                                                       M14.3421
      C2=CS*CS                                                          M14.3422
      R1=R1-C2                                                          M14.3423
      DO 2006 I5=1,7,2                                                  M14.3424
      R1=R1+CCARS(I5)*CS                                                M14.3425
      XS=XS+DCARS(I5)*CS                                                M14.3426
 2006 CS=CS*C2                                                          M14.3427
C                      END CARSON SERIES FOR SELF IMPEDANCE IF Z.GT.5.0 M14.3428
  127 R1=R1*F2                                                          M14.3429
  128 Z(IP)=XS*F2                                                       M14.3430
      P(IP)=R1                                                          M14.3431
      IF ( IPRSUP  .GE.  4 )                                            M14.3432
     1 WRITE (LUNIT6, 4440)  I, K, KFULL, KCIRCT, I5, IP, P(IP), Z(IP)  M17.1302
 4440 FORMAT ( 9H AT 4440   , 6I10, 2E16.6  )                           M14.3434
      GO TO 120                                                         M14.3435
  150 XS=G1/F2                                                          M14.3436
      IF(IX.EQ.1) XS=XS*FREQ/60.0                                       M14.3437
      XS=XS+ALOGZ(Y1*2.0)* ONEHAF                                       M18. 946
      G1=24.0*Y1/EXPZ(2.0*XS)                                           M18. 947
      GO TO 122                                                         M14.3440
C                                                                       M14.3441
C                                  ROUTINES FOR Z-PRINTING AND INVERSIONM14.3442
C     IMPEDANCE MATRIX FOR PHYSICAL CONDUCTORS IS IN P(1),...P(IP) (REALM14.3443
C     PART) AND Z(1)....Z(IP) (IMAGINARY PART).                         M14.3444
  200 IF ( J7  .GT.  0 )                                                M14.3445
     1 CALL OUTPUT ( METRIK, P(1), Z(1), UNITY, KFULL, LL6, LL1 )       M14.3446
      IF ( IPRSUP  .GE.  1 )                                            M14.3447
     1 WRITE (LUNIT6, 4434)  J7, J10, J56, KCIRCT, J8, MUTUAL, J11, J12,M17.1303
     2 IDIST, J9, KP, J2, J3, J5, J6                                    M14.3449
 4434 FORMAT ( /,  27H AT 4234.   MISC. INTEGERS.   ,/, ( 1X, 10I12 ) ) M14.3450
      IF(J10.GT.0) GO TO 250                                            M14.3451
      IF(J56.EQ.0) GO TO 600                                            M14.3452
  201 IF(KCIRCT.EQ.0) GO TO 600                                         M14.3453
      GO TO 49                                                          M14.3454
  202 CALL CXRED2( P(1), Z(1), WORKR1(1), WORKR2(1), KFULL, KCIRCT )    M14.3455
C     IMPEDANCE MATRIX FOR EQUIVALENT PHASE CONDUCTORS IS IN P(1),...   M14.3456
C     P(KP) (REAL PART) AND Z(1),...Z(KP) (IMAGINARY PART).             M14.3457
      IF ( J8OUT  .GT.  0 )                                             M28.7205
     1 CALL OUTPUT ( METRIK, P(1), Z(1), UNITY, KCIRCT, LL6, LL2 )      M14.3459
      IF ( IPUNCH .NE. 44 )  GO TO 8202                                 M30.1248
      CALL PUNPIE ( KCIRCT )                                            M43.3378
 8202 IF ( IPRSUP  .GE.  1 )                                            M31.6402
     1  WRITE (LUNIT6, 3202) IMODAL, DIST                               M31.6403
 3202 FORMAT (  15H IMODAL, DIST =,  I5, E17.5  )                       M31.6404
      IF ( LASTOV .NE. 39 )   GO TO 3203                                M31.6405
      IF ( NFREQ   .NE.  1  )   GO TO 3203                              M31.6406
C     "MARTI SETUP" ASSUMES DISTANCE IN KILOMETERS IS ON 9:             M31.6407
      DISTKM = DIST / FMIPKM                                            M32, 306
      WRITE (LUNIT9) IMODAL, METRIK, DISTKM, MSPEDB, ITRNSF             M43.3379
      IF (IPRSUP .GT. 1)                                                M43.3380
     1  write (LUNIT6, *) ' MSPEDB written on LUNIT9 =', MSPEDB         M43.3381
 3203 IF ( IMODAL .LE. 0 )  GO TO 2202                                  M31.6410
      IF( LASTOV .EQ. 1  .AND.  ITRNSF .NE. -1  .AND.                   M39.3594
     1 ITRNSF .NE. -9 )  ITRNSF = 0                                             
      N8 = KFULL                                                                
      IF ( NBUNDL .EQ. 1 )  N8 = KCIRCT                                         
      IF ( KCIRCT .LE. NDIM)  GO TO 1202                                M32, 307
      KILL = 221                                                        M30.1270
      LSTAT(19) = 1202                                                  M29.4209
      LSTAT(14) = KCIRCT                                                M29.4210
      GO TO 9200                                                        M29.4211
 1202 IF ( NFREQ .EQ. 3 )  NTOL = NTOL + 1                              M43.3384
      IF ( KEXACT .EQ. 88333) DIST = DISTSV
      CALL MODAL(ARRAY,XWC,XWY,YZR,YZI,TII,TIR,TVI,TVR,ER,EI,THETA2,    M43.3385
     1     XTIR,XTII,ZSURGE,DUMMI,DUMMR,TIXF,WORK1,FREQ,KCIRCT,IW,      M31.6412
     2     DIST,METRIK,FMIPKM,NDIM,NTRI,NSQR2,ITRNSF,KFULL,MRR,         M43.3386
     3     NRP,NTOL,CONDUC)                                             M43.3387
      IF( KILL .NE. 0 ) GO TO 9900                                      M32, 309
 2202 IF (LASTOV.EQ.39 .OR. LASTOV.EQ.45 .OR. IMODAL .NE.0) GO TO 8777  M43.3388
      IF ( KEXACT .EQ. 88333 )  GO TO 45454
      KOUNT = 0                                                         M43.3389
      REWIND LUNIT1                                                     M43.3390
      IF ( IPUNCH  .EQ.  0 )                                            M43.3391
     1 WRITE (LUNIT7, 8769)  TCLOCK, DATE1                              M43.3392
 8769 FORMAT (    42HC   PUNCHED CARD OUTPUT OF TRANSPOSED LINE,        M43.3393
     1            15H WHICH BEGAN AT,  2X,  2A4,  2X,  2A4  )           M43.3394
      IF ( IPUNCH .NE. 0 )  GO TO 8770                                  M44. 847
      IF ( IMODAL .EQ. 0 .AND. MSPEDB .EQ. 0) WRITE (LUNIT7, 8780) freq M44. 848
 8780 FORMAT (56HC   ***** TRANSPOSED K.C. LEE LINE SEGMENT CALCULATED AM44. 849
     1T, 2X, E10.3, 10H HZ. *****   )                                   M44. 850
      IF ( IMODAL .EQ. 0 .AND. MSPEDB .EQ. 1) WRITE (LUNIT7, 8785) FREQ M44. 851
 8785 FORMAT (59HC **** SPECIAL DOUBLE CIRCUIT TRANSPOSED LINE CALCULATEM44. 852
     1D AT, 2X, E10.3, 9H HZ. ****         )                            M44. 853
 8770 REWIND LUNIT2                                                     M44. 854
      N5 = 0                                                            M43.3396
C     IF ( JDATCS .GT. 0 )  GO TO 8777                                  M43.3397
      DO 8774  N12=1, 9999                                              M43.3398
      READ (LUNIT2, 8771)  ( TEXTA6(I), I=1, 14 )                       M43.3399
 8771 FORMAT ( 13A6, A2 )                                               M43.3400
      IF ( IPUNCH  .EQ.  0 )                                            M43.3401
     1 WRITE (LUNIT7, 8772)  ( TEXTA6(I), I=1, 13 )                     M43.3402
 8772 FORMAT ( 2HC ,  13A6 )                                            M43.3403
      IF (IDEBUG.EQ.0) GO TO 8100                                       M43.3404
      WRITE (LUNIT1, 8773)  ( TEXTA6(I), I=1, 14 )                      M43.3405
 8773 FORMAT ( 3H C ,  13A6,  A2 )                                      M43.3406
      KOUNT = KOUNT+1                                                   M43.3407
 8100 IF ( TEXTA6(1) .EQ. BLANK     .AND.                               M43.3408
     1     TEXTA6(2) .EQ. BLANK )   N5 = N5 + 1                         M43.3409
      IF ( N5  .GE.  1 )   GO TO 8777                                   M43.3410
 8774 CONTINUE                                                          M43.3411
 8777 IF ( IALTER  .LE.  0 )   GO TO 3734                               M43.3412
      IF ( LASTOV  .EQ.  39 )  GO TO 3734                               M36.3598
      WRITE (LUNIT3)  ( P(JJ), JJ=1, KP )                               M17.1304
      WRITE (LUNIT3)  ( Z(JJ), JJ=1, KP )                               M17.1305
      IF ( IPRSUP  .EQ.  0 )   GO TO 3734                               M16.4525
      WRITE (LUNIT6, 7423)  KP,  ( P(JJ), JJ=1, KP ),  ( Z(JJ), JJ=1, KPM17.1306
     + )                                                                M17.1307
 7423 FORMAT ( /,  39H OUTPUT OF  (R)  AND  (L)  FOR SEMLYEN,    ,      M16.4527
     1 I6,   13H  CELLS EACH.,  /,  ( 1X,  8E16.4 ) )                   M16.4528
 3734 IF ( MUTUAL  .GT.  0 )                                            M16.4529
     1 CALL OUTSPC(P(1), Z(1),KCIRCT,METRIK,FMIPKM)                     M15.1610
      J56 = J11+J12+IDIST                                               M14.3462
      IF ( LASTOV .EQ. 39 )  GO TO 253                                  M44. 855
      IF(J9.GT.0 .AND. KCIRCT.GE.2) GO TO 253                           M14.3463
  203 IF(J56.EQ.0) GO TO 600                                            M14.3464
      DO 204 I=1,KP                                                     M14.3465
      P(I)=-P(I)                                                        M14.3466
  204 Z(I) = -Z(I)                                                      M14.3467
      CALL CXRED2 ( P(1), Z(1), WORKR1(1), WORKR2(1), KCIRCT, LL0 )     M14.3468
C     INVERTED IMPEDANCE MATRIX FOR EQUIVALENT PHASE CONDUCTORS IS IN   M14.3469
C     P(1),...P(KP) (REAL PART) AND Z(1),...Z(KP) (IMAGINARY PART).     M14.3470
      IF ( J11  .GT.  0 )                                               M14.3471
     1 CALL OUTPUT ( METRIK, P(1), Z(1), UNITY, KCIRCT, LL5, LL2 )      M14.3472
      IF(IDIST.EQ.0)  GO TO 207                                         M14.3473
      DO 206 I=1,KP                                                     M14.3474
      GD(I)=P(I)                                                        M14.3475
  206 BD(I)=Z(I)                                                        M14.3476
  207 IF(J12.EQ.0 .OR. KCIRCT.LT.2) GO TO 600                           M14.3477
      CALL SYMM ( P(1), Z(1), UNITY, KCIRCT, KK )                       M14.3478
C     INVERTED IMPEDANCE MATRIX FOR SYMMETRICAL COMPONENTS OF EQUIVALENTM14.3479
C     PHASE CONDUCTORS IS IN P(1),...P(KK*(KK+1)/2) (REAL PART) AND Z(1)M14.3480
C     ,...Z(KK*(KK+1)/2) (IMAGINARY PART). DEFINITION OF KK SAME AS FOR M14.3481
C     INVERSE SUSCEPTANCE MATRIX                                        M14.3482
      CALL OUTPUT ( METRIK, P(1), Z(1), UNITY, KK, LL5, LL3 )           M14.3483
      GO TO 600                                                         M14.3484
  250 IF(J56.EQ.0) GO TO 251                                            M14.3485
      WRITE (LUNT13)  (P(I),Z(I),I=1,IP)               
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4227
      REWIND LUNT13                                  
  251 DO  252 I=1,IP                                                    M14.3488
      P(I) = -P(I)                                                      M14.3489
  252 Z(I) = -Z(I)                                                      M14.3490
      CALL CXRED2 ( P(1), Z(1), WORKR1(1), WORKR2(1), KFULL, LL0 )      M14.3491
C     INVERTED IMPEDANCE MATRIX FOR PHYSICAL CONDUCTORS IS IN P(1),...  M14.3492
C     P(IP) (REAL PART) AND Z(1),...Z(IP) (IMAGINARY PART).             M14.3493
      CALL OUTPUT ( METRIK, P(1), Z(1), UNITY, KFULL, LL5, LL1 )        M14.3494
      IF(J56.EQ.0) GO TO 600                                            M14.3495
      READ (LUNT13)  (P(I),Z(I),I=1,IP)              
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4230
      REWIND LUNT13                                  
      GO TO 201                                                         M14.3498
  253 IF(J56.EQ.0) GO TO 254                                            M14.3499
      WRITE (LUNT13)  (P(I),Z(I),I=1,KP)                 
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4233
      REWIND LUNT13                                  
  254 GO TO 45454                                                       M43.3413
C  254 IF ( LASTOV .EQ. 39  .OR.                                        M43.3414
C     1          KCIRCT .GT. 3 ) GO TO 45454                            M43.3415
  256 CALL SYMM ( P(1), Z(1), UNITY, KCIRCT, KK )                       M43.3416
C     IMPEDANCE MATRIX FOR SYMMETRICAL COMPONENTS OF EQUIVALENT PHASE   M14.3503
C     CONDUCTORS IS IN P(1),...P(KK*(KK+1)/2) (REAL PART) AND Z(1),...  M14.3504
C     Z(KK*(KK+1)/2) (IMAGINARY PART). DEFINITION OF KK SAME AS FOR     M14.3505
C     INVERSE SUSCEPTANCE MATRIX.                                       M14.3506
      IF ( J9OUT  .GT.  0 )                                             M28.7206
     1 CALL OUTPUT ( METRIK, P(1), Z(1), UNITY, KK, LL6, LL3 )          M28.7207
      IF(J6.EQ.0) GO TO 9998                                            M14.3508
      RZERO=P(1)                                                        M14.3509
      XZERO=Z(1)                                                        M14.3510
      RPOS = P(LL5)                                                     M35.9080
      XPOS = Z(LL5)                                                     M35.9081
      ZO=SQRTZ(RZERO**2+XZERO**2)                                       M18. 948
      Z1=SQRTZ(RPOS**2+XPOS**2)                                         M18. 949
      ZSO=SQRTZ(ZO/YZERO)                                               M18. 950
      ZS1=SQRTZ(Z1/YPOS)                                                M18. 951
      ZOANG=ATAN2Z(XZERO,RZERO)* ONEHAF                                 M18. 952
      Z1ANG=ATAN2Z(XPOS,RPOS)* ONEHAF                                   M18. 953
      PROPO=SQRTZ(ZO*YZERO)                                             M18. 954
      PROP1=SQRTZ(Z1*YPOS)                                              M18. 955
      ZSODRG=ZOANG*PICON - 45.                                          M14.3521
      ZS1DRG=Z1ANG*PICON - 45.                                          M14.3522
      D2 = TWOPI / 8.0                                                  M14.3523
      ALPHAO=PROPO*COSZ(ZOANG+ D2)                                      M18. 956
      ALPHA1=PROP1*COSZ(Z1ANG+ D2)                                      M18. 957
      BETO  =PROPO*SINZ(ZOANG+ D2 )                                     M18. 958
      BET1=  PROP1*SINZ(Z1ANG+ D2 )                                     M18. 959
      GO TO 2254                                                        M43.3417
45454 D13 = TWOPI * FREQ                                                M31.6415
CCCC      write (*,*) ' Ready for Z avg.  P(1:6) =',                    M43.3418
CCCC     1                              ( P(I), I=1, 6 )                M43.3419
CCCC      write (*,*) ' Ready for Z avg.  Z(1:6) =',                    M43.3420
CCCC     1                              ( Z(I), I=1, 6 )                M43.3421
CCCC      write (*,*) ' imodal, nfreq =',  imodal, nfreq                M43.3422
C      IF (IMODAL .NE. 0)    GO TO 2254                                 M43.3423
      IF ( LASTOV .EQ. 39 )  GO TO 6256
      IF (IMODAL .NE. 0)    GO TO 256                                   M43.3424
      GO TO 6276
 6256 IF ( NFREQ .EQ. 1 )  GO TO 9998
      IF ( IMODAL .EQ. 0 )  GO TO 6276
      IF ( NFREQ .LT. 3 )  GO TO 9998
      GO TO 3006
C     IF (LASTOV .EQ. 39  .AND.  NFREQ  .EQ. 1)    GO TO 9998           M43.3425
CCCC  DOR = P(1)                                                        M32, 312
CCCC  DOI = Z(1)                                                        M32, 313
CCCC  D1R = P(LL5)                                                      M35.9082
CCCC  D1I = Z(LL5)                                                      M35.9083
 6276 RDIAG = 0                                      
      XDIAG = 0                                                         M43.3427
      ROFF = 0                                                          M43.3428
      XOFF = 0                                                          M43.3429
      N55 = 1                                                           M43.3430
      IF ( KCIRCT .LT. 2 )  GO TO 5059                                  M44. 856
      IF ( MSPEDB .NE. 1 )  GO TO 5090                                  M43.3431
      RCOUP = 0.                                                                
      XCOUP = 0.                                                                
      DO 5030  I = 1, 6                                                 M43.3434
      RDIAG = RDIAG + P(N55)                                            M43.3435
      XDIAG = XDIAG + Z(N55)                                            M43.3436
      IF ( N55 .EQ. 10 )  GO TO 5028                                    M43.3437
      JM = N55 - I + 1                                                  M43.3438
      IF ( N55 .GE. 15 )  JM = JM + 3                                   M43.3439
      DO 5025  JP= JM, N55-1                                            M43.3440
      ROFF = ROFF + P(JP)                                               M43.3441
 5025 XOFF = XOFF + Z(JP)                                               M43.3442
 5028 N55 = N55 + I + 1                                                 M43.3443
 5030 CONTINUE                                                          M43.3444
      RDIAG = RDIAG / 6                                                 M43.3445
      XDIAG = XDIAG / 6                                                 M43.3446
      ROFF = ROFF / 6                                                   M43.3447
      XOFF = XOFF / 6                                                   M43.3448
      N55 = 6                                                           M43.3449
      DO 5050  JP = 4, 6                                                M43.3450
      J = N55 + 1                                                       M43.3451
      DO 5040  I = J, J+2                                               M43.3452
      RCOUP = RCOUP + P(I)                                              M43.3453
 5040 XCOUP = XCOUP + Z(I)                                              M43.3454
      N55 = N55 + JP                                                    M43.3455
 5050 CONTINUE                                                          M43.3456
      RCOUP = RCOUP / 9                                                 M43.3457
      XCOUP = XCOUP / 9                                                 M43.3458
      XTIR(1) = RDIAG + 2*ROFF + 3*RCOUP                                        
      XTII(1) = XDIAG + 2*XOFF + 3*XCOUP                                        
      XTIR(2) = XTIR(1) - 6*RCOUP                                               
      XTII(2) = XTII(1) - 6*XCOUP                                               
      XTIR(3) = RDIAG - ROFF                                                    
      XTII(3) = XDIAG - XOFF                                                    
      IF ( LASTOV .EQ. 1 )  GO TO 5080                                  M43.3465
      IF ( LASTOV .NE. 39 .AND. KEXACT .NE. 88333)  GO TO 9998                    
      DO 5053  I = 1, 3                                                 M43.3467
      ZMMAG = SQRTZ ( XTIR(I) ** 2 + XTII(I) ** 2 )                     M43.3468
      ZMANG = ATAN2Z ( XTII(I), XTIR(I) )                               M43.3469
      YMMAG = SQRTZ ( GMODE ** 2  + DUMMI(I) ** 2 )                     M43.3470
      YMANG = ATAN2Z ( DUMMI(I), GMODE  )                               M43.3471
      ZSURGE(I) = SQRTZ ( YMMAG / ZMMAG )                                       
      THETA2(I) = ( YMANG - ZMANG ) / 2.                                        
      D1 = SQRTZ ( ZMMAG * YMMAG )                                              
      D2 = ( ZMANG + YMANG ) / 2.                                               
      ER(I) = D1 * COSZ(D2) * FMIPKM                                            
      EI(I) = D1 * SINZ(D2) * FMIPKM  
      IF ( KEXACT .NE. 88333 )  GO TO 5053
      ER(I) = D1 * COSZ(D2) * DISTSV
      EI(I) = D1 * SINZ(D2) * DISTSV
 5053 CONTINUE                                                          M43.3478
      DO  5060   I = 1, KCIRCT                                          M43.3479
      IF ( I .GT. 2 )  GO TO 5058                                       M43.3480
      WRITE (LUNIT9) D13,ZSURGE(I),THETA2(I),ER(I),EI(I)                M43.3481
      GO TO 5060                                                        M43.3482
 5058 WRITE (LUNIT9) D13,ZSURGE(3),THETA2(3),ER(3),EI(3)                M43.3483
 5060 CONTINUE                                                          M43.3484
      IF (IPRSUP .GE. 1 )                                               M43.3485
     1  WRITE (LUNIT6, 5065) FREQ, KCIRCT, (ZSURGE(I),THETA2(I),        M43.3486
     2                       ER(I), EI(I), I = 1,3 )                    M43.3487
 5065 FORMAT ('  AT 5065, FREQ,KCIRCT,ZSURGE,THETA2,ER,EI ARE: ',/,     M43.3488
     1 E10.3,I3,12E9.2 )                                                M43.3489
      GO TO 9998                                                        M43.3490
 5080 WRITE (LUNIT7,228)                                                M43.3491
      DO 5081  I = 1, 3                                                 M43.3492
      ZSURGE(I) = SQRTZ( XTII(I) / DUMMI(I) )                                   
      ER(I) = D13 / SQRTZ( XTII(I) * DUMMI(I) )                                 
 5081 CONTINUE                                                          M43.3495
      IF (IPRSUP .GE. 1 )                                               M43.3496
     1 write (LUNIT6,*) ' ZSURGE, ER = ',(ZSURGE(I),ER(I),I=1,3)        M43.3497
      DO 5088  I = 1, KCIRCT                                            M43.3498
      IF ( I .GT. 1 )  GO TO 5082                                       M43.3499
      WRITE (LUNIT7,140) I, BRNAME(2*I-1), BRNAME(2*I),                 M43.3500
     1      XTIR(I), ZSURGE(I), ER(I), DIST                             M43.3501
      GO TO 5088                                                        M43.3502
 5082 IF ( I .GT. 2 )  GO TO 5084                                       M43.3503
      WRITE (LUNIT7,140) I, BRNAME(2*I-1), BRNAME(2*I),                 M43.3504
     1     XTIR(3), ZSURGE(3), ER(3), DIST                              M43.3505
      GO TO 5088                                                        M43.3506
 5084 IF ( I .GT. 3 )  GO TO 5086                                       M43.3507
      WRITE (LUNIT7,140) I, BRNAME(2*I-1), BRNAME(2*I),                 M43.3508
     1     XTIR(2), ZSURGE(2), ER(2), DIST                              M43.3509
      GO TO 5088                                                        M43.3510
 5086 IF ( I .LE. 9 )
     1 WRITE (LUNIT7,1475) I, BRNAME(2*I-1), BRNAME(2*I)                 
      IF ( I .GT. 9 )
     1 WRITE (LUNIT7,9475) I, BRNAME(2*I-1), BRNAME(2*I)
 9475 FORMAT ( I2, 2A6 )
 5088 CONTINUE                                                          M43.3512
      WRITE (LUNIT7, 29)                                                M43.3513
      GO TO 9998                                                        M43.3514
 5059 XTIR(1) = P(1)                                                    M44. 857
      XTII(1) = Z(1)                                                    M44. 858
      GO TO 480                                                         M44. 859
CCCC      write (*,*) ' Enter DO 478 loop.   KCIRCT =',  KCIRCT         M43.3515
 5090 DO 478  I=1, KCIRCT                                               M43.3516
      RDIAG = RDIAG + P(N55)                                            M43.3517
      XDIAG = XDIAG + Z(N55)                                            M43.3518
      DO 479  JP=N55-I+1, N55-1                                         M43.3519
      ROFF = ROFF + P(JP)                                               M43.3520
  479 XOFF = XOFF + Z(JP)                                               M43.3521
  478 N55 = N55 + I + 1                                                 M43.3522
      RDIAG = RDIAG / KCIRCT                                            M43.3523
      XDIAG = XDIAG / KCIRCT                                            M43.3524
      ROFF = ROFF / ( KCIRCT * ( KCIRCT - 1 ) / 2 )                     M43.3525
      XOFF = XOFF / ( KCIRCT * ( KCIRCT - 1 ) / 2 )                     M43.3526
CCCC      write (*,*) ' Average of diagonals,  RDIAG, XDIAG =',  RDIAG, M43.3527
CCCC      write (*,*) ' Average of off-diag,  ROFF, XOFF =',  ROFF, XOFFM43.3528
      XTIR(1) = RDIAG + ( KCIRCT - 1 ) * ROFF                                   
      XTII(1) = XDIAG + ( KCIRCT - 1 ) * XOFF                                   
      XTIR(2) = RDIAG -  ROFF                                                   
      XTII(2) = XDIAG -  XOFF                                                   
      IF ( LASTOV .EQ. 39 .OR. LASTOV .EQ. 45  .OR. KEXACT .EQ. 88333)  
     1   GO TO 480              
      IF ( IPRSUP .LT. 1  )  GO TO 1228                                 M43.3534
      write (LUNIT6,*) ' GMODE,XTIR, XTII, , DUMMI =',                  M43.3535
     1  GMODE, (XTIR(I),XTII(I),DUMMI(I), I = 1, 2)                     M43.3536
 1228 WRITE (LUNIT7,228)                                                M43.3537
  228 FORMAT (11H$VINTAGE, 1)                                           M43.3538
      DO 5092  I = 1, 2                                                 M43.3539
      ZSURGE(I) = SQRTZ( XTII(I) / DUMMI(I) )                                   
      ER(I) = D13 / SQRTZ( XTII(I) * DUMMI(I) )                                 
 5092 CONTINUE                                                          M43.3542
      DO 1480  I = 1, KCIRCT                                            M43.3543
      IF ( I .GT. 2 )  GO TO 1473                                       M43.3544
      WRITE (LUNIT7,140) I, BRNAME(2*I-1), BRNAME(2*I),                 M43.3545
     1     XTIR(I), ZSURGE(I), ER(I), DIST                              M43.3546
  140 FORMAT (1H-,I1,2A6,12X,4E12.5,2H 1,2X)                            M43.3547
      GO TO 1480                                                        M43.3548
 1473 IF ( I .LE. 9 )
     1 WRITE (LUNIT7,1475) I, BRNAME(2*I-1), BRNAME(2*I)                 
      IF ( I .GT. 9 )
     1 WRITE (LUNIT7,9475) I, BRNAME(2*I-1), BRNAME(2*I)
 1475 FORMAT (1H-,I1,2A6)                                               M43.3550
 1480 CONTINUE                                                          M43.3551
      WRITE (LUNIT7, 29)                                                M43.3552
   29 FORMAT (11H$VINTAGE, 0)                                           M43.3553
      GO TO 256                                                         M43.3554
  480 DO 5093  I = 1, NMODE                                             M44. 860
      ZMMAG = SQRTZ ( XTIR(I) ** 2 + XTII(I) ** 2 )                     M43.3556
      ZMANG = ATAN2Z ( XTII(I), XTIR(I) )                               M43.3557
      YMMAG = SQRTZ ( GMODE ** 2  + DUMMI(I) ** 2 )                     M43.3558
      YMANG = ATAN2Z ( DUMMI(I), GMODE  )                               M43.3559
      ZSURGE(I) = SQRTZ ( YMMAG / ZMMAG )                                       
      THETA2(I) = ( YMANG - ZMANG ) / 2.                                        
      D1 = SQRTZ ( ZMMAG * YMMAG )                                              
      D2 = ( ZMANG + YMANG ) / 2.                                               
      ER(I) = D1 * COSZ(D2) * FMIPKM                                            
      EI(I) = D1 * SINZ(D2) * FMIPKM  
      IF ( KEXACT .NE. 88333 )  GO TO 5093
      ER(I) = D1 * COSZ(D2) * DISTSV
      EI(I) = D1 * SINZ(D2) * DISTSV
 5093 CONTINUE                                                          M43.3566
      DO  5094  I = 1, KCIRCT                                           M43.3567
      IF ( I .GT. 1 )  GO TO 5098                                       M43.3568
      WRITE (LUNIT9) D13,ZSURGE(I),THETA2(I),ER(I),EI(I)                M43.3569
      GO TO 5094                                                        M43.3570
 5098 WRITE (LUNIT9) D13,ZSURGE(2),THETA2(2),ER(2),EI(2)                M43.3571
 5094 CONTINUE                                                          M43.3572
      IF (IPRSUP .GE. 1 )                                               M43.3573
     1  WRITE (LUNIT6, 5095) FREQ, KCIRCT, (ZSURGE(I),THETA2(I),        M43.3574
     2                       ER(I), EI(I), I = 1,2 )                    M43.3575
 5095 FORMAT ('  AT 5095, FREQ,KCIRCT,ZSURGE,THETA2,ER,EI ARE: ',/,     M43.3576
     1 E10.3,I3,8E12.3 )                                                M43.3577
      GO TO 9998                                                        M43.3578
 2254 RZERO1 = RZERO                                                    M30.1306
      RPOS1 = RPOS                                                      M28.7209
      XZERO1 = XZERO                                                    M22.6178
      XPOS1 = XPOS                                                      M22.6179
      YZERO1 = YZERO                                                    M22.6180
      YPOS1 = YPOS                                                      M22.6181
      ZO=1000./OMEGA                                                    M14.3529
      Z1=1000000./OMEGA                                                 M14.3530
      XZERO=XZERO*ZO                                                    M14.3531
      XPOS=XPOS*ZO                                                      M14.3532
      YZERO=YZERO*Z1                                                    M14.3533
      YPOS=YPOS*Z1                                                      M14.3534
      IF (IPRSUP .GE. 1)                                                M19.2440
     1 WRITE (LUNIT6, 13012)  IPUNCH                                    M19.2441
13012 FORMAT (19H AT 13012, IPUNCH =, I10)                              M19.2442
      IF ( IPUNCH  .EQ.  0  .OR.  IPUNCH .EQ. 44)  GO TO 13039          M30.1307
      IF ( IPUNCH  .NE.  1 )   GO TO 13011                              M15.1612
      WRITE (LUNIT7, 3011)  ALPHAO, BETO, ALPHA1, BET1, FREQ            M17.1316
 3011 FORMAT(5E15.5)                                                    M14.3539
      GO TO 13039                                                       M15.1614
13011 IF ( MFRQPR  .GT.  0 )                                            M31.6422
     1 WRITE (LUNIT6, 3012)  ALPHAO, BETO, RZERO, XZERO,                M31.6423
     2  YZERO, ALPHA1, BET1, RPOS, XPOS, YPOS, FREQ                     M31.6424
 3012 FORMAT ( 1X, E11.4, 9E12.4, E12.5 )                               M31.6425
      IF ( IPUNCH  .NE. 2 )   GO TO 13019                               M31.6426
      WRITE (LUNIT7, 13015)  RZERO, XZERO, FREQ                         M17.1317
13015 FORMAT ( 3E16.8 )                                                 M15.1617
      GO TO 13039                                                       M15.1618
13019 IF ( IPUNCH  .NE.  3 )   GO TO 13024                              M15.1619
      WRITE (LUNIT7, 13015)  RPOS, XPOS, FREQ                           M17.1318
      GO TO 13039                                                       M15.1621
13024 IF ( IPUNCH  .NE.  88 )   GO TO 13031                             M15.1622
      IF (METRIK .EQ. 0 ) GO TO 13026                                   M28.7210
      RZERO = RZERO * FMIPKM                                            M28.7211
      XZERO = XZERO * FMIPKM                                            M28.7212
13026 WRITE (LUNIT9)  RZERO, XZERO, FREQ                                M28.7213
      IF (IPRSUP .GE. 1)                                                M19.2443
     1 WRITE (LUNIT6, 23024)  RZERO, XZERO, FREQ                        M19.2444
23024 FORMAT (31H RZERO, XZERO AND FREQ ON UNIT9, 3E15.6)               M19.2445
      GO TO 13039                                                       M15.1624
13031 IF ( IPUNCH  .EQ.  89 )   GO TO 13035                             M15.1625
      KILL = 162                                                        M15.1626
      LSTAT(19) = 13031                                                 M15.1627
      GO TO 9200                                                        M15.1628
13035 IF (METRIK .EQ. 0) GO TO 13037                                    M28.7214
      RPOS = RPOS * FMIPKM                                              M28.7215
      XPOS = XPOS * FMIPKM                                              M28.7216
13037 WRITE (LUNIT9)  RPOS, XPOS, FREQ                                  M28.7217
13039 IF (IK .GT. 0)   GO TO 3006                                       M19.2446
 3010 VOL=OMEGA/BETO                                                    M14.3541
      ALPHAO=ALPHAO*VALU5                                               M14.3542
      ALPHA1=ALPHA1*VALU5                                               M14.3543
      V1L=OMEGA/BET1                                                    M14.3544
      WAVEO= TWOPI /BETO                                                M14.3545
      WAVE1= TWOPI /BET1                                                M14.3546
      IF( METRIK .EQ. 1 ) GO TO 9992                                    M22.6182
      WRITE(LUNIT6,9991)ZSO,ZSODRG,ALPHAO,VOL,WAVEO,RZERO,XZERO1,YZERO1,M31.6427
     1                  ZS1,ZS1DRG,ALPHA1,V1L,WAVE1,RPOS,XPOS1,YPOS1    M22.6184
 9991 FORMAT(9H0SEQUENCE,6X,15HSURGE IMPEDANCE,7X,76HATTENUATION   VELOCM14.3549
     1ITY    WAVELENGTH   RESISTANCE    REACTANCE   SUSCEPTANCE/9X,     M14.3550
     2102HMAGNITUDE(OHM) ANGLE(DEGR.)   DB/MILE      MILES/S       MILESM14.3551
     3      OHM/MILE     OHM/MILE     MHO/MILE/9H   ZERO  ,8E13.5/      M14.3552
     4 9H POSITIVE,8E13.5)                                              M14.3553
      GO TO 9998                                                        M22.6185
 9992 ALPHAO = ALPHAO * FMIPKM                                          M22.6186
      ALPHA1 = ALPHA1 * FMIPKM                                          M22.6187
      VOL = VOL / FMIPKM                                                M22.6188
      V1L = V1L / FMIPKM                                                M22.6189
      WAVEO = WAVEO / FMIPKM                                            M22.6190
      WAVE1 = WAVE1 / FMIPKM                                            M22.6191
      RZERO1 = RZERO1 * FMIPKM                                          M28.7218
      RPOS1  = RPOS1  * FMIPKM                                          M28.7219
      XZERO1 = XZERO1 * FMIPKM                                          M22.6198
      XPOS1 = XPOS1 * FMIPKM                                            M22.6199
      YZERO1 = YZERO1 * FMIPKM                                          M22.6200
      YPOS1 = YPOS1 * FMIPKM                                            M22.6201
      WRITE(LUNIT6,9993)ZSO,ZSODRG,ALPHAO,VOL,WAVEO,RZERO1,XZERO1,      M31.6428
     1           YZERO1,ZS1,ZS1DRG,ALPHA1,V1L,WAVE1,RPOS1,XPOS1,YPOS1   M28.7221
 9993 FORMAT(9H0SEQUENCE,6X,15HSURGE IMPEDANCE,7X,76HATTENUATION   VELOCM22.6204
     1ITY    WAVELENGTH   RESISTANCE    REACTANCE   SUSCEPTANCE/9X,     M22.6205
     2102HMAGNITUDE(OHM) ANGLE(DEGR.)    DB/KM         KM/S          KM M34. 189
     3       OHM/KM       OHM/KM       MHO/KM /9H   ZERO  ,8E13.5/      M22.6207
     4 9H POSITIVE,8E13.5 )                                             M22.6208
 9998 IF ( KEXACT .NE. 88333 )  GO TO 9996
      N1 = 1
      N9SQ = KCIRCT ** 2 
      IF ( MSPEDB .EQ. 1 )  GO TO 9540
      K = 1
      DO 5960  J=1, KCIRCT
      DO 5955  I=1, KCIRCT  
      DUMMR(K) = 1.  
      IF ( I .EQ. J .AND. I .NE. 1 )   DUMMR(K) = 1 - I  
      IF ( I .GT. J .AND. J .NE. 1 )   DUMMR(K) = 0.  
 5955 K = K + 1
 5960 CONTINUE 
      DO 5970  I =1,N9SQ,KCIRCT
      K = I + KCIRCT - 1 
      TEMP = 0.
      DO 5965  J=I, K                         
 5965 TEMP = TEMP + DUMMR(J) ** 2
      TEMP = SQRTZ(TEMP)
      DO 5968  J=I, K
      TIXF(N1) = DUMMR(J)/TEMP 
      WORK1(N1) = 0.0  
 5968 N1 = N1 + 1 
 5970 CONTINUE 
      GO TO 9994
 9540 D1 = 2.0 
      D2 = 6.0 
      D3 = 1.0/SQRTZ(D1)
      D4 = 1.0/SQRTZ(D2)
      DO 25965  J = 1, 36  
      IF ( J .GT. 9 )  GO TO 15975  
      TIXF(N1) = D4
      GO TO 25960 
15975 IF ( J .GT. 12 )  GO TO 15980 
      TIXF(N1) = -D4  
      GO TO 25960 
15980 IF ( J .GT. 13 )  GO TO 15985 
      TIXF(N1) = D3
      GO TO 25960 
15985 IF ( J .GT. 14 )  GO TO 15990 
      TIXF(N1) = -D3  
      GO TO 25960 
15990 IF ( J .GT. 18 )  GO TO 15991 
      TIXF(N1) = 0.0  
      GO TO 25960 
15991 IF ( J .GT. 20 )  GO TO 15992 
      TIXF(N1) = D4
      GO TO 25960 
15992 IF ( J .GT. 21)  GO TO 15993  
      TIXF(N1) = - D1 * D4  
      GO TO 25960 
15993 IF ( J .GT. 27)  GO TO 15994  
      TIXF(N1) = 0.0  
      GO TO 25960 
15994 IF ( J .GT. 28 )  GO TO 15995 
      TIXF(N1) = D3
      GO TO 25960 
15995 IF ( J .GT. 29 )  GO TO 15996 
      TIXF(N1) = -D3  
      GO TO 25960 
15996 IF ( J .GT. 33 )  GO TO 15997 
      TIXF(N1) = 0.0  
      GO TO 25960 
15997 IF ( J .GT. 35 )  GO TO 15999 
      TIXF(N1) = D4
      GO TO 25960 
15999 TIXF(N1) = - D1 * D4  
25960 WORK1(N1) = 0.0  
      N1 = N1 + 1 
25965 CONTINUE 
 9994 WRITE(LUNIT9) (TIXF(I),I=1,N9SQ), (WORK1(I),I=1,N9SQ)
 9996 IF(J56.EQ.0) GO TO 600                                            
      READ (LUNT13)  (P(I),Z(I),I=1,KP)              
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4236
      REWIND LUNT13                                    
      GO TO 203                                                         M14.3557
C                          END OF ROUTINES FOR Z-PRINTING AND INVERSIONSM14.3558
C                                                                       M14.3559
C                                ROUTINES FOR P-PRINTING AND INVERSIONS M14.3560
   36 I2=1                                                              M14.3561
C     INVERSE SUSCEPTANCE OR CAPACITANCE MATRIX FOR PHYSICAL CONDUCTORS M14.3562
C     IS IN P(1),...P(IP).                                              M14.3563
      IF (IW.EQ.0) I2 = I2+1                                            M14.3564
      CALL OUTPUT ( METRIK, P(1), Z(1), SWITCH, KFULL, I2, LL1 )        M14.3565
      GO TO 31                                                          M14.3566
   37 I2=3                                                              M14.3567
      IF ( IPRSUP  .GE.  1 )                                            M14.3568
     1WRITE (LUNIT6, 4444)  IW, IP, LL0, LL1                            M17.1324
 4444 FORMAT ( /,  28H AT 4444.  IW, IP, LL0, LL1=, 4I10 )              M14.3570
      IF (IW.EQ.0) I2 = I2+1                                            M14.3571
      DO 371 I=1,IP                                                     M14.3572
  371 Z(I)=-P(I)                                                        M14.3577
      CALL REDU44 ( Z(1), WORKR1(1), KFULL, LL0 )                       M14.3578
      IF( KILL .GT. 0 )  GO TO 9200                                     M14.3579
C     SUSCEPTANCE OR CAPACITANCE MATRIX FOR PHYSICAL CONDUCTORS IS IN   M14.3580
C     Z(1),...Z(IP).                                                    M14.3581
      CALL OUTPUT ( METRIK, Z(1), P(1), SWITCH, KFULL, I2, LL1 )        M14.3582
      GO TO 32                                                          M14.3583
   38 I2=1                                                              M14.3584
C     INVERSE SUSCEPTANCE OR CAPACITANCE MATRIX FOR EQUIVALENT PHASE    M14.3585
C     CONDUCTORS IS IN P(1),...P(KP)                                    M14.3586
C     TO STORE WC(INV) OR C(INV) FOR MODAL ANALYSIS                     M29.4238
      IF ( IPRSUP .GE. 1 )                                              M43.3579
     1  write (*,*) ' At S.N. 38,  P(1) =',  P(1)                       M43.3580
      IF ( IMODAL .LE. 0 )  GO TO 403                                   M29.4239
      DO 404  I = 1, KP                                                 M29.4240
  404 XWC(I) = P(I)                                                     M29.4241
  403 IF (IW.EQ.0) I2=I2+1                                              M29.4242
      IF ( J2OUT  .GT.  0 )                                             M29.4243
     1 CALL OUTPUT ( METRIK, P(1), Z(1), SWITCH, KCIRCT, I2, LL2 )      M14.3589
      IF ( IPRSUP  .GE.  4 )                                            M22.6210
     1 WRITE (LUNIT6, 2654)  J3, KCIRCT, J56, KP, IW, I2, J2,           M22.6211
     2                       ( P(I), I=1, KP )                          M22.6212
 2654 FORMAT ( /,  36H BEFORE 'C'-MATRIX CALL TO  'SYMM' .,             M22.6213
     1 32H      J3  KCIRCT     J56      KP,                             M22.6214
     2 24H      IW      I2      J2  ,/,  36X,  7I8  ,/,                 M22.6215
     3  29H (P(I), I=1, KP)  FOLLOW ....  ,/,  ( 1X,  8E16.7 )  )       M22.6216
      IF(J3.EQ.0 .OR. KCIRCT.LT.2) GO TO 33                             M14.3590
      IF (J56.EQ.0) GO TO 40                                            M14.3591
      WRITE (LUNT13)   ( P(I), I=1, KP )               
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.4245
      REWIND LUNT13                                  
   40 CALL SYMM ( P(1), Z(1), SWITCH, KCIRCT, KK )                      M14.3594
      I2=1                                                              M14.3595
      IF (IW.EQ.0) I2=I2+1                                              M14.3596
C     INVERSE SUSCEPTANCE OR CAPACITANCE MATRIX FOR SYMMETRICAL COMPO-  M14.3597
C     NENTS OF EQUIVALENT PHASE CONDUCTORS (GROUPED AS 3-PHASE CIRCUITS M14.3598
C     IN ORDER 1-2-3, 4-5-6 ETC.) IS IN P(1),...P(KK*(KK+1)/2) FOR REAL M14.3599
C     PART AND Z(1),...Z(KK*(KK+1)/2) FOR IMAGINARY PART WITH KK=HIGHESTM14.3600
C     NUMBER OF EQUIVALENT CONDUCTOR BEING A MULTIPLE OF 3.             M14.3601
      IF(IK.LE.0)CALLOUTPUT ( METRIK, P(1), Z(1), UNITY, KK, I2, LL3 )  M14.3602
      IF (J56.EQ.0) GO TO 33                                            M14.3603
      READ (LUNT13)  D1                              
      READ (LUNT13)  (P(I),I=1,KP)                      
      GO TO 33                                                          M14.3606
   39 I2=3                                                              M14.3607
      IF(IW.EQ.0) I2=I2+1                                               M14.3608
      DO 391 I=1,KP                                                     M14.3609
  391 P(I) = -P(I)                                                      M14.3610
      CALL REDU44 ( P(1), WORKR1(1), KCIRCT, LL0 )                      M14.3611
      IF( KILL .GT. 0 )  GO TO 9200                                     M14.3612
C     SUSCEPTANCE OR CAPACITANCE MATRIX FOR EQUIVALENT PHASE CONDUCTORS M14.3613
C     IS IN P(1),...P(KP).                                              M14.3614
      IF ( IMODAL .EQ. 1 )  GO TO 1378                                          
      IF ( IPRSUP .LT. 1 )  GO TO 5124                                  M43.3581
      write (*,*) ' Ready for [C] avg.  P(1:6) =',                      M43.3582
     1                                ( P(I), I=1, 6 )                  M43.3583
      write (LUNIT6,*) ' Begin averaging of capacitance.'               M43.3584
      write (LUNIT6,*) ' 1st diagonal,  P(1) =',  P(1)                  M43.3585
      write (LUNIT6,*) ' 1st off-diagonal,  P(2) =',  P(2)              M43.3586
      write (LUNIT6,*) ' =======  IW, KCIRCT, freq =',                  M43.3587
     1                       IW, KCIRCT, freq                           M43.3588
 5124 CDIAG = 0                                                         M43.3589
      COFF = 0                                                          M43.3590
      N55 = 1                                                           M43.3591
      IF (MSPEDB .NE. 1 )  GO TO 5190                                   M43.3592
      CCOUP = 0.                                                                
      DO 5130  I = 1, 6                                                 M43.3594
      CDIAG = CDIAG + P(N55)                                            M43.3595
      IF ( N55 .EQ. 10 )  GO TO 5128                                    M43.3596
      JM = N55 - I + 1                                                  M43.3597
      IF ( N55 .GE. 15 )  JM = JM + 3                                   M43.3598
      DO 5125  JP= JM, N55-1                                            M43.3599
      COFF = COFF + P(JP)                                               M43.3600
 5125 CONTINUE                                                          M43.3601
 5128 N55 = N55 + I + 1                                                 M43.3602
 5130 CONTINUE                                                          M43.3603
      CDIAG = CDIAG / 6                                                 M43.3604
      COFF = COFF / 6                                                   M43.3605
      IF ( IPRSUP .GE. 1 )                                              M43.3606
     1  write (LUNIT6,*) ' CDIAG, COFF are ', CDIAG, COFF               M43.3607
      N55 = 6                                                           M43.3608
      DO 5150  JP = 4, 6                                                M43.3609
      J = N55 + 1                                                       M43.3610
      DO 5140  I = J, J+2                                               M43.3611
      CCOUP = CCOUP + P(I)                                              M43.3612
 5140 CONTINUE                                                          M43.3613
      N55 = N55 + JP                                                    M43.3614
 5150 CONTINUE                                                          M43.3615
      CCOUP = CCOUP / 9                                                 M43.3616
      DUMMI(1) = CDIAG + 2*COFF + 3*CCOUP                                       
      DUMMI(2) = DUMMI(1) - 6*CCOUP                                             
      DUMMI(3) = CDIAG - COFF                                                   
      IF ( IPRSUP .GE. 1)                                               M43.3620
     1  write (LUNIT6,*) ' AT 5150, DUMMI =', (DUMMI(I),I= 1,3)         M43.3621
      IF ( IW .EQ. 0 )  GO TO 1378                                      M43.3622
      D13 = TWOPI * FREQ                                                M43.3623
      DO 1260  I = 1,3                                                  M43.3624
      DUMMI(I) = DUMMI(I) * D13                                                 
 1260 CONTINUE                                                          M43.3626
      GO TO 1378                                                        M43.3627
 5190 if ( kcirct .lt. 2 )  go to 1379                                          
      DO 378  I=1, KCIRCT                                               M44. 863
      CDIAG = CDIAG + P(N55)                                            M43.3629
      DO 379  JP=N55-I+1, N55-1                                         M43.3630
  379 COFF = COFF + P(JP)                                               M43.3631
  378 N55 = N55 + I + 1                                                 M43.3632
      CDIAG = CDIAG / KCIRCT                                            M43.3633
      COFF = COFF / ( KCIRCT * ( KCIRCT - 1 ) / 2 )                     M43.3634
C     write (*,*) ' Average of diagonals,  CDIAG =',  CDIAG             M43.3635
C     write (*,*) ' Average of off-diag,   COFF =',   COFF              M43.3636
      DUMMI(1) = CDIAG + ( KCIRCT - 1 ) * COFF                          M43.3637
      DUMMI(2) = CDIAG -  COFF                                          M43.3638
      go to 2379                                                        M44. 864
 1379 dummi(1) = p(1)                                                   M44. 865
      dummi(2) = 0.                                                     M44. 866
 2379 IF ( IW .EQ. 0 )  GO TO 1378                                              
      D13 = TWOPI * FREQ                                                M43.3642
      DUMMI(1) = DUMMI(1) * D13                                         M43.3643
      DUMMI(2) = DUMMI(2) * D13                                         M43.3644
 1378 IF ( J5OUT  .GT.  0 )                                             M43.3645
     1 CALL OUTPUT ( METRIK, P(1), Z(1), SWITCH, KCIRCT, I2, LL2 )      M14.3616
      ZERO = 0.0                                                        M16.4530
      IF ( IALTER  .LE.  0 )   GO TO 3741                               M16.4531
      WRITE (LUNIT3)  ( ZERO, JJ=1, KP )                                M17.1329
      WRITE (LUNIT3)  ( P(JJ), JJ=1, KP )                               M17.1330
      IF ( IPRSUP  .LE.  0 )   GO TO 3741                               M16.4534
      WRITE (LUNIT6, 7428)  ( P(JJ),  JJ=1, KP )                        M17.1331
 7428 FORMAT ( /, 26H OUTPUT  (C)  FOR SEMLYEN.  ,/, ( 1X, 8E16.4 ) )   M16.4536
 3741 IF ( IDIST  .EQ.  0 ) GO TO 393                                   M16.4537
      DO 392 I=1,KP                                                     M14.3618
  392 YD(I)=P(I)                                                        M14.3619
  393 IF(J6.EQ.0 .OR. KCIRCT.LT.2) GO TO 80                             M14.3620
      CALL SYMM ( P(1), Z(1), SWITCH, KCIRCT, KK )                      M14.3621
      I2=3                                                              M14.3622
      IF(IW.EQ.0) I2=I2+1                                               M14.3623
C     SUSCEPTANCE OR CAPACITANCE MATRIX FOR SYMMETRICAL COMPONENTS OF   M14.3624
C     EQUIVALENT PHASE CONDUCTORS IS IN P(1),...P(KK*(KK+1)/2) FOR REAL M14.3625
C     PART AND Z(1),...Z(KK*(KK+1)/2) FOR IMAGINARY PART. DEFINITION OF M14.3626
C     KK SAME AS FOR INVERSE SUSCEPTANCE MATRIX                         M14.3627
      IF ( J6OUT  .GT.  0 )                                             M28.7223
     1 CALL OUTPUT ( METRIK, P(1), Z(1), UNITY, KK, I2, LL3 )           M28.7224
      IF (IPUNCH .LT.  88 )   GO TO 399                                 M22.6217
      IF (IPUNCH .GT.  89 )   GO TO 399                                 M22.6218
      IF (IK .GT. 0)   GO TO 399                                        M19.2448
      IF (LIU .NE. 0)   GO TO 399                                       M19.2449
      CZERO = P(1)/ TENM6                                               M19.2450
      CPOS = P(LL5) / TENM6                                             M35.9084
      IF (IW .NE. 0)  GO TO 394                                         M22.6220
      CZERO = CZERO/OMEGA                                               M22.6221
      CPOS = CPOS / OMEGA                                               M22.6222
  394 IF (IPUNCH .NE. 88)  GO TO 395                                    M22.6223
      IF ( METRIK .EQ. 1 )  CZERO = CZERO * FMIPKM                      M28.7225
      WRITE (LUNIT9)   CZERO                                            M19.2452
      IF (IPRSUP .GE. 1)                                                M19.2454
     1 WRITE (LUNIT6, 98765)  CZERO                                     M19.2455
98765 FORMAT (15H CZERO ON UNIT9, E15.6)                                M19.2456
      GO TO 396                                                         M22.6224
  395 IF (METRIK .EQ. 1 )  CPOS = CPOS * FMIPKM                         M28.7226
      WRITE(LUNIT9)  CPOS                                               M28.7227
      IF (IPRSUP .GE. 1)                                                M22.6226
     1 WRITE (LUNIT6, 397)  CPOS                                        M22.6227
  397 FORMAT ( 14H CPOS ON UNIT9, E15.6)                                M22.6228
  396 LIU = 1                                                           M22.6229
  399 YZERO = P(1)                                                      M19.2457
      YPOS = P(LL5)                                                     M35.9085
      IF(IW.EQ.0) GO TO 80                                              M14.3631
      YZERO=YZERO*OMEGA                                                 M14.3632
      YPOS=YPOS*OMEGA                                                   M14.3633
      GO TO 80                                                          M14.3634
C                          END OF ROUTINES FOR P-PRINTING AND INVERSIONSM14.3635
C                                                                       M14.3636
C                                        CHANGES IN CONDUCTOR DATA      M14.3637
  500 ICOUNT=0                                                          M14.3638
      K=0                                                               M14.3639
      IF(KCIRCT.EQ.0) GO TO 503                                         M14.3640
      I2 = LPHPL1 - KCIRCT                                              M14.3641
      I = LPHASE                                                        M14.3642
      KK=-1                                                             M14.3643
  501 K=K+1                                                             M14.3644
      ITBIC(K)=IC(I)                                                    M14.3645
      TBTB2(K)=TB2(I)                                                   M14.3646
      ITBTB3(K)=ITB3(I)                                                 M14.3647
      TBR(K)=R(I)                                                       M14.3648
      TBD(K)=D(I)                                                       M14.3649
      TBG(K)=GMD(I)                                                     M14.3650
      TBX(K)=X(I)                                                       M14.3651
      TBY(K)=Y(I)                                                       M14.3652
      TBTEXT(K)=TEXT(I)                                                 M14.3653
      IF(I.EQ.I2) GO TO 502                                             M14.3654
      I=I+KK                                                            M14.3655
      GO TO 501                                                         M14.3656
  502 IF(ICOUNT.GT.0) GO TO 504                                         M14.3657
  503 ICOUNT=1                                                          M14.3658
      I=1                                                               M14.3659
      I2=KTOT                                                           M14.3660
      KK=1                                                              M14.3661
      IF(KTOT.GT.0) GO TO 501                                           M14.3662
C     READ INPUT CARD USING CIMAGE                                      M14.3663
  504 CALL CIMAGE                                                       M14.3664
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.4538
      IF ( IALTER  .NE.  2 )                                            M16.4539
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1332
      DECODE (80, 11, ABUFF)                                            M14.3665
     1 I1, Y1, R1, I2, G1, D1, X1, H1, H2, D8, D9, BUS1, I3             M14.3666
      IF( I1 .NE. 0 )  GO TO 4423                                       M14.3667
      IF( Y1 .NE. 0 )  GO TO 4423                                       M14.3668
      IF( R1 .NE. 0 )  GO TO 4423                                       M14.3669
      IF( I2 .NE. 0 )  GO TO 4423                                       M14.3670
      IF( G1 .NE. 0.0 )  GO TO 4423                                     M14.3671
      IF( D1 .NE. 0.0 )  GO TO 4423                                     M14.3672
      IF( X1 .NE. 0.0 )  GO TO 4423                                     M14.3673
      IF( H1 .NE. 0.0 )  GO TO 4423                                     M14.3674
      IF( H2 .NE. 0.0 )  GO TO 4423                                     M14.3675
      IF( D8 .NE. 0.0 )  GO TO 4423                                     M14.3676
      IF( D9 .NE. 0.0 )  GO TO 4423                                     M14.3677
      IF( BUS1 .EQ. BLANK )  GO TO 507                                  M14.3678
 4423 ISW = 1                                                           M14.3679
      I = 0                                                             M14.3680
 1505 I = I +1                                                          M14.3681
      IF (I .GT. KFULL)   GO TO 505                                     M14.3682
      IF ( TBTEXT(I)  .EQ.  BUS1 )  GO TO 506                           M14.3683
      GO TO 1505                                                        M14.3684
  505 IF (ISW .EQ. 2)   GO TO 504                                       M14.3685
      KFULL=KFULL+1                                                     M14.3686
      IF( KFULL .GT. LPHASE )  GO TO 7                                  M14.3687
      I=KFULL                                                           M14.3688
      ITBIC(I) = I1                                                     M14.3689
      TBTB2(I) = Y1                                                     M14.3690
      TBR(I) = R1                                                       M14.3691
      ITBTB3(I) = I2                                                    M14.3692
      TBG(I) = G1                                                       M14.3693
      TBD(I) = D1                                                       M14.3694
      TBX(I) = X1                                                       M14.3695
      TBTEXT(I) = BUS1                                                  M14.3696
      ASSIGN 711 TO MOON                                                M14.3697
      GO TO 4280                                                        M14.3698
  506 CONTINUE                                                          M14.3699
      DECODE (80, 4281, ABUFF(1) )  BUS1                                M14.3700
      IF( BUS1  .NE.  BLANK )  ITBIC(I) = I1                            M14.3701
      DECODE (80, 4286, ABUFF)  BUS1                                    M14.3702
      IF ( BUS1  .NE.  BLANK )  TBTB2(I) = Y1                           M14.3703
      DECODE (80, 4292, ABUFF)  BUS1, BUS2                              M14.3704
      IF ( BUS1  .NE.  BLANK )  TBR(I) = R1                             M14.3705
      IF ( BUS2  .NE.  BLANK )  TBR(I) = R1                             M14.3706
      DECODE (80, 4297, ABUFF)  BUS1                                    M14.3707
      IF ( BUS1  .NE.  BLANK )  ITBTB3(I) = I2                          M14.3708
      DECODE (80, 4306, ABUFF)  BUS1, BUS2                              M14.3709
      IF ( BUS1  .NE.  BLANK )  TBG(I) = G1                             M14.3710
      IF ( BUS2  .NE.  BLANK )  TBG(I) = G1                             M14.3711
      DECODE (80, 4313, ABUFF)  BUS1, BUS2                              M14.3712
      IF ( BUS1  .NE.  BLANK )  TBD(I) = D1                             M14.3713
      IF ( BUS2  .NE.  BLANK )  TBD(I) = D1                             M14.3714
      ISW = 2                                                           M14.3715
      GO TO 1505                                                        M14.3716
  711 IF (I3 .LE. 1)   GO TO 716                                        M14.3717
      I4 = 1                                                            M14.3718
      XX = TBX(I)                                                       M14.3719
      YY = TBY(I)                                                       M14.3720
      ANGL = (PI - TWOPI  / I3) / 2.0                                   M14.3721
      DANGL = TWOPI / I3                                                M14.3722
      RADIUS = D8 / (24.0 * COSZ(ANGL))                                 M18. 960
      D9R = D9 / PICON                                                  M14.3724
  713 TBX(I) = XX +RADIUS * COSZ(D9R -DANGL * I4)                       M18. 961
      TBY(I) = YY +RADIUS * SINZ(D9R -DANGL * I4)                       M18. 962
      IF (I4 .EQ. I3)   GO TO 716                                       M14.3727
      I4 = I4 +1                                                        M14.3728
      I = I +1                                                          M14.3729
      ITBIC(I) = ITBIC(I-1)                                             M14.3730
      TBTB2(I) = TBTB2(I-1)                                             M14.3731
      TBR(I) = TBR(I-1)                                                 M14.3732
      ITBTB3(I) = ITBTB3(I-1)                                           M14.3733
      TBG(I) = TBG(I-1)                                                 M14.3734
      TBD(I) = TBD(I-1)                                                 M14.3735
      TBTEXT(I) = TBTEXT(I-1)                                           M14.3736
      GO TO 713                                                         M14.3737
  716 KFULL = I                                                         M14.3738
      GO TO 504                                                         M14.3739
  507 I = KFULL + 1                                                     M14.3740
      GO TO 6                                                           M14.3741
C                                                                       M14.3742
C                            EXTENDING THE MATRICES FOR SPECIFIED LENGTHM14.3743
  600 IF(DIST.EQ.0.) GO TO 16                                           M14.3744
      J1516=J15+J16                                                     M14.3745
      IF(J13+J14+J1516.EQ.0) GO TO 16                                   M14.3746
      IF( METRIK .EQ. 1 ) GO TO 610                                     M22.6230
      WRITE(LUNIT6,601) DIST                                            M17.1333
  601 FORMAT (/, 27H MATRICES FOR LINE LENGTH =, E13.6, 7H MILES.)      M22.6231
      GO TO 612                                                         M22.6232
  610 DISTM = DIST / FMIPKM                                             M22.6233
      WRITE( LUNIT6,611 ) DISTM                                         M22.6234
  611 FORMAT( /, 27H MATRICES FOR LINE LENGTH =, F8.3,   4H KM. )       M22.6235
  612 CONTINUE                                                          M22.6236
      IF(IDIST.EQ.1) GO TO 603                                          M14.3749
      WRITE(LUNIT6,602) KCIRCT                                          M17.1334
  602 FORMAT(1H+,41X,58HCANNOT BE CALCULATED WITH NUMBER OF EQUIVALENT CM14.3751
     1ONDUCTORS=,I3)                                                    M14.3752
      GO TO 16                                                          M14.3753
  603 IP=0                                                              M14.3754
      D1 = FLTINF                                                       M14.3755
      K=0                                                               M14.3756
  604 K=K+1                                                             M14.3757
      IF(K.GT.KCIRCT) GO TO 605                                         M14.3758
      IP=IP+K                                                           M14.3759
      H1= VALU6*BD(IP)/YD(IP)                                           M14.3760
      IF(ABSZ(H1).LT.D1) D1=ABSZ(H1)                                    M18. 963
      GO TO 604                                                         M14.3762
  605 IF(IW.GT.0) D1=D1/OMEGA                                           M14.3763
      D1=SQRTZ(D1*2.0)                                                  M18. 964
      I1=DIST/D1+ ONEHAF                                                M14.3765
      I2=1                                                              M14.3766
      DO 606 ISEC=1,34                                                  M14.3767
      IF(I1.LE.I2) GO TO 608                                            M14.3768
  606 I2=I2*2                                                           M14.3769
      WRITE(LUNIT6,607)                                                 M17.1335
  607 FORMAT(1H+,41X,60HCANNOT BE CALCULATED WITH NUMBER OF NECESSARY SEM14.3771
     1CTIONS=2**33)                                                     M14.3772
      GO TO 16                                                          M14.3773
  608 X1=I2                                                             M14.3774
      I1=ISEC-1                                                         M14.3775
      DELTAD=DIST/X1                                                    M14.3776
      IF( METRIK .EQ. 1 ) GO TO 613                                     M22.6237
      WRITE(LUNIT6,609) I1,DELTAD                                       M17.1336
  609 FORMAT (38H COMPUTED BY CONNECTING IN CASCADE 2**,I2,18H EQUAL SECM22.6238
     1TIONS OF, E13.6, 12H MILES EACH.)                                 M22.6239
      GO TO 615                                                         M22.6240
  613 DELTAM = DELTAD / FMIPKM                                          M22.6241
      WRITE( LUNIT6,614 ) I1, DELTAM                                    M22.6242
  614 FORMAT(1H+,41X,36HCOMPUTED BY CONNECTING IN SERIES 2**,I2,18H EQUAM22.6243
     1L SECTIONS OF,F9.4,9H KM EACH. )                                  M22.6244
  615 CONTINUE                                                          M22.6245
      F1=UNITY/DELTAD                                                   M14.3780
      F2=DELTAD* ONEHAF                                                 M14.3781
      IF(IW.GT.0) F2=OMEGA*F2                                           M14.3782
      KP=KCIRCT*(KCIRCT+1)/2                                            M14.3783
      KCIR2=KCIRCT+KCIRCT                                               M14.3784
      IF (I1 .EQ. 0)  GO TO 670                                         M22.6246
      DO 621 I=1,KP                                                     M14.3785
      R1=F1*GD(I)                                                       M14.3786
      X1=F1*BD(I)                                                       M14.3787
      G1=F2*YD(I)                                                       M14.3788
      D1=X1+G1                                                          M14.3789
      P(I)=R1                                                           M14.3790
      Z(I)=D1                                                           M14.3791
      GD(I)=R1+R1                                                       M14.3792
  621 BD(I)=X1+D1                                                       M14.3793
C                                  BEGIN OF LOOP FOR CONNECTING SECTIONSM14.3794
      ICOUNT=0                                                          M14.3795
  622 ICOUNT=ICOUNT+1                                                   M14.3796
      IF(ICOUNT.EQ.ISEC) GO TO 650                                      M14.3797
C                                       EXPANDING MATRIX                M14.3798
      IP=0                                                              M14.3799
      I3=KP                                                             M14.3800
      K=0                                                               M14.3801
  623 K=K+1                                                             M14.3802
      IF(K.GT.KCIRCT) GO TO 626                                         M14.3803
      I2=I3+KCIRCT                                                      M14.3804
      L4=KP+K                                                           M14.3805
      I=0                                                               M14.3806
  624 I=I+1                                                             M14.3807
      L1=IP+I                                                           M14.3808
      L2=I2+I                                                           M14.3809
      L3=I3+I                                                           M14.3810
      R1=P(L1)                                                          M14.3811
      X1=Z(L1)                                                          M14.3812
      H1=R1-GD(L1)                                                      M14.3813
      H2=X1-BD(L1)                                                      M14.3814
      P(L2)=R1*2.0                                                      M14.3815
      Z(L2)=X1*2.0                                                      M14.3816
      P(L3)=H1                                                          M14.3817
      Z(L3)=H2                                                          M14.3818
      GD(L1)=R1                                                         M14.3819
      BD(L1)=X1                                                         M14.3820
      IF(I.EQ.K) GO TO 625                                              M14.3821
      P(L4)=H1                                                          M14.3822
      Z(L4)=H2                                                          M14.3823
      L4=L4+I+KCIRCT                                                    M14.3824
      GO TO 624                                                         M14.3825
  625 IP=IP+K                                                           M14.3826
      I3=I2+K                                                           M14.3827
      GO TO 623                                                         M14.3828
  626 CALL CXRED2( P(1), Z(1), WORKR1(1), WORKR2(1), KCIR2, KCIRCT )    M14.3829
      GO TO 622                                                         M14.3830
C                                       OUTPUT OF MATRICES FOR LENGTH   M14.3831
  650 DO 651 I=1,KP                                                     M14.3832
      GD(I)=GD(I)-P(I)                                                  M14.3833
      BD(I)=BD(I)-Z(I)                                                  M14.3834
      P(I) = (P(I) - GD(I)) * 2.                                        M22.6247
  651 Z(I) = (Z(I) - BD(I)) * 2.                                        M22.6248
  673 IF (J1516 .GT. 0)  GO TO 660                                      M22.6249
  652 IF(J13.EQ.0) GO TO 653                                            M14.3838
C     TRANSFER ADMITTANCE MATRIX FOR EQUIVALENT COND. IS IN GD+J*BD.    M14.3839
      CALL OUTPUT ( LL0, GD(1), BD(1), UNITY, KCIRCT, LL7, LL2 )        M22.6250
C     SHUNT ADMITTANCE MATRIX FOR EQUIVALENT COND. IS IN P+J*Z.         M14.3841
      CALL OUTPUT ( LL0,  P(1),  Z(1), UNITY, KCIRCT, LL8, LL2 )        M22.6251
  653 IF(J14.EQ.0) GO TO 654                                            M14.3843
      CALL SYMM ( GD(1), BD(1), UNITY, KCIRCT, KK )                     M14.3844
C     TRANSFER ADMITTANCE MATRIX FOR SYMMETRICAL COMP. IS IN GD+J*BD.   M14.3845
      CALL OUTPUT ( LL0, GD(1), BD(1), UNITY, KK, LL7, LL3 )            M22.6252
      CALL SYMM ( P(1), Z(1), UNITY, KCIRCT, KK )                       M14.3847
C     SHUNT ADMITTANCE MATRIX FOR SYMMETRICAL COMP. IS IN P+J*Z.        M14.3848
      CALL OUTPUT ( LL0,  P(1),  Z(1), UNITY, KK, LL8 , LL3 )           M22.6253
  654 IF(J1516.EQ.0) GO TO 16                                           M14.3850
      DO 655 I=1,KP                                                     M14.3851
      N1 = 2 * LGDBD + 1 + I                                            M20.5078
      N2 = I + LGDBD                                                    M20.5079
      GD(I) = -P(N2)                                                    M20.5080
      BD(I) = -Z(N2)                                                    M20.5081
      P(I) = -P(N1)                                                     M20.5082
  655 Z(I) = -Z(N1)                                                     M20.5083
      CALL CXRED2 ( GD(1), BD(1), WORKR1(1), WORKR2(1), KCIRCT, LL0 )   M14.3857
      CALL CXRED2 ( P(1), Z(1), WORKR1(1), WORKR2(1), KCIRCT, LL0 )     M14.3858
      IF(J15.EQ.0) GO TO 656                                            M14.3859
C     TRANSFER IMPEDANCE MATRIX FOR EQUIVALENT CONDUCTORS IS IN GD+J*BD.M14.3860
      CALL OUTPUT ( LL0, GD(1), BD(1), UNITY, KCIRCT, LL9, LL2 )        M22.6254
C     SHUNT IMPEDANCE MATRIX FOR EQUIVALENT CONDUCTORS IS IN P+J*Z.     M14.3862
      CALL OUTPUT ( LL0,  P(1),  Z(1), UNITY, KCIRCT, LL10, LL2 )       M22.6255
  656 IF(J16.EQ.0) GO TO 16                                             M14.3864
      CALL SYMM ( GD(1), BD(1), UNITY, KCIRCT, KK )                     M14.3865
C     TRANSFER IMPEDANCE MATRIX FOR SYMMETRICAL COMP. IS IN GD+J*BD.    M14.3866
      CALL OUTPUT ( LL0, GD(1), BD(1), UNITY, KK, LL9, LL3 )            M22.6256
      CALL SYMM ( P(1), Z(1), UNITY, KCIRCT, KK )                       M14.3868
C     SHUNT IMPEDANCE MATRIX FOR SYMMETRICAL COMP. IS IN P+J*Z.         M14.3869
      CALL OUTPUT ( LL0,  P(1),  Z(1), UNITY, KK, LL10, LL3 )           M22.6257
      GO TO 16                                                          M14.3871
  660 I = LGDBD + 1                                                     M14.3872
      CALL MOVER ( GD(1), P(I), KP )                                    M14.3873
      CALL MOVER ( BD(1), Z(I), KP )                                    M14.3874
      I = 2 * I                                                         M14.3875
      CALL MOVER ( P(1), P(I), KP )                                     M14.3876
      CALL MOVER ( Z(1), Z(I), KP )                                     M14.3877
      GO TO 652                                                         M14.3878
  670 F2 = F2 * 2.                                                      M22.6258
      DO 671 I=1, KP                                                    M22.6259
      P(I) = 0.                                                         M22.6260
      Z(I) = F2 * YD(I)                                                 M22.6261
      GD(I) = F1 * GD(I)                                                M22.6262
  671 BD(I) = F1 * BD(I)                                                M22.6263
      GO TO 673                                                         M22.6264
 9200 LSTAT(18) = NCHAIN                                                M16.4541
      IF ( IALTER  .EQ.  2 )                                            M16.4542
     1 LUNIT5 = L5SAVE                                                  M16.4543
      LASTOV = NCHAIN                                                   M15.1631
      NCHAIN = 51                                                       M14.3880
 9900 RETURN                                                            M14.3881
      END                                                               M14.3882
      SUBROUTINE PUNPIE ( KCIRCT )                                      M42.8327
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      INCLUDE  'DECK44.FOR;/NOLIST'                                           
      IF ( IPRSUP .GE. 1 )  WRITE (*,*) ' Top "PUNPIE".'                M42.8331
      WRITE (LUNIT7, 1201)                                              M42.8332
 1201 FORMAT ( 11H$VINTAGE, 1)                                          M42.8333
      J = 1                                                             M42.8334
      DO 8201  I = 1, KCIRCT                                            M42.8335
      K = I + J - 1                                                     M42.8336
      DO 7201   KK = J, K                                               M42.8337
      YD(KK) = YD(KK) / TENM6                                           M42.8338
      IF ( KK .EQ. J )                                                  M42.8339
     1  WRITE (LUNIT7, 5201) I, BRNAME(2*I-1), BRNAME(2*I), P(KK),      M42.8340
     2                       Z(KK), YD(KK)                              M42.8341
 5201 FORMAT ( I2, 2A6, 12X, 3E16.5)                                    M42.8342
      IF ( KK .NE. J)                                                   M42.8343
     1  WRITE (LUNIT7, 6201) P(KK), Z(KK), YD(KK)                       M42.8344
 6201 FORMAT ( 26X, 3E16.5)                                             M42.8345
 7201 CONTINUE                                                          M42.8346
      J = K + 1                                                         M42.8347
 8201 CONTINUE                                                          M42.8348
      WRITE (LUNIT7, 2201)                                              M42.8349
 2201 FORMAT ( 11H$VINTAGE, 0)                                          M42.8350
      if ( iprsup .ge. 1 )  WRITE (*,*) ' Exit "PUNPIE".'               M42.8351
      RETURN                                                            M42.8352
      END                                                               M42.8353
      SUBROUTINE MODAL(ARRAY,XWC,XWY,YZR,YZI,TII,TIR,TVI,TVR,ER,EI,     M43.3646
     1 THETA2,XTIR,XTII,ZSURGE,DUMMI,DUMMR,TIXF,WORK1,FREQ,M,           M31.6430
     2 IW,DIST,METRIK,FMIPKM,NDIM,NTRI,NSQR2,ITRNSF,KFULL,              M39.3597
     3 MRR,NRP,NTOL,CONDUC)                                             M43.3647
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      INCLUDE  'DECK44.FOR;/NOLIST' 
      COMMON  /LINEMODEL/ KEXACT, NSOLVE, FMINSV, KBRNUM, NUMRUN
      REAL*8        TEXT1, TEXT2, TEXT3, TEXT4, TEXT5, TEXT6            M32, 327
      REAL*8        TEXT7, TEXT8, TEXT9, TEXT10                         M32, 328
      DIMENSION ARRAY(1)                                                M31.6433
      DIMENSION XWC(NTRI),XWY(NTRI),YZI(NDIM,NDIM),YZR(NDIM,NDIM)       M31.6434
      DIMENSION TII(NDIM,NDIM),TIR(NDIM,NDIM),TVI(NDIM,NDIM)            M43.3648
      DIMENSION ER(NDIM),EI(NDIM),THETA2(NDIM),TVR(NDIM,NDIM)           M43.3649
      DIMENSION XTIR(NDIM),XTII(NDIM),ZSURGE(NDIM),DUMMI(NDIM)          M31.6437
      DIMENSION DUMMR(NDIM),TIXF(NSQR2),WORK1(NSQR2)                    M31.6438
      DIMENSION PING(200),ISEQ(15),KMAX(15)                             M43.3650
      DIMENSION PP1(20,20), PP2(20,20), EE1(15), EE2(15)                M43.3651
      DIMENSION TEMPR(20,20), TEMPI(20,20)                              M43.3652
      DATA  TEXT2  /  1HA  /                                            M32, 329
      DATA  TEXT3  /  1HB  /                                            M32, 330
      DATA  TEXT4  /  1HC  /                                            M32, 331
      DATA  TEXT5  /  1HD  /                                            M32, 332
      DATA  TEXT6  /  1HE  /                                            M32, 333
      DATA  TEXT7  /  1HF  /                                            M32, 334
      DATA  TEXT8  /  1HG  /                                            M32, 335
      DATA  TEXT9  /  1HH  /                                            M32, 336
      DATA  TEXT10 /  1HI  /                                            M32, 337
      DATA  ISEQ  /  15*0  /                                            M43.3653
      IF ( KEXACT .EQ. 88333 )  GO TO 100
      IF (ITRNSF .EQ. 1  .AND. NFREQ .EQ. 1) RETURN                             
      IF (ITRNSF .EQ. 1  .AND. NFREQ .EQ. 2) RETURN                             
      IF (LASTOV .EQ. 39 .AND. NFREQ .EQ. 2) RETURN                             
  100 IF ( IPRSUP .GE. 1 )                                              
     1 WRITE (LUNIT6,*) ' Top of modal.   NDIM =',  NDIM                M43.3658
      LL0 = 0                                                           M43.3659
      MM=M*(M+1)/2                                                      M29.4258
      C=UNITY                                                           M29.4259
      IF (IPRSUP .GE. 1 )                                               M29.4260
     1 WRITE (LUNIT6, 300) M, MM, (P(I),Z(I), I=1, MM)                  M31.6439
  300 FORMAT ( 34H AT THE TOP OF MODAL, M AND MM ARE, 2I10,             M29.4262
     1  /,  28H (P(I),  Z (I), I=1, MM) ARE,/, (1X, 8E15.6) )           M31.6440
      D13 = TWOPI * FREQ                                                M31.6441
      IF ( IW .EQ. 1 ) C = C / D13                                      M43.3660
      DO 27 I=1,MM                                                      M29.4265
      XWC(I) = XWC(I) * C                                               M31.6443
   27 XWY(I)=-XWC(I)                                                    M32, 339
      CALL REDU44(XWY(1),WORKR1(1) ,M,LL0)                              M32, 340
C    Const. [T], MARTI SETUP WILL BYPASS THE EXACT DIAGONALIZATION      M43.3661
C     AT EACH LOOPING FREQUENCY                                         M43.3662
      IF (NFREQ .NE. 1  .AND.  LASTOV .EQ. 39                           M43.3663
     1                  .AND. ITRNSF .NE. 1) GO TO 2520                 M43.3664
      IF ( LASTOV .EQ. 39 .AND. NFREQ .EQ. 3 )  GO TO 7030              M43.3665
      IF ( KEXACT .EQ. 88333 )  GO TO 7030
      WRITE(LUNIT6,45)FREQ                                              M29.4268
      WRITE(LUNIT6,44)                                                  M29.4269
      IF(METRIK.EQ.1) GO TO 7029                                        M29.4270
      WRITE(LUNIT6,65)                                                  M29.4271
      GO TO 7030                                                        M29.4272
 7029 CONTINUE                                                          M29.4273
      WRITE(LUNIT6,7031)                                                M29.4274
 7030 CONTINUE                                                          M29.4275
   45 FORMAT(//,28H MODAL PARAMETERS AT FREQ = ,E13.5,3H HZ)            M29.4276
C ***** CREATE Y * Z MATRIX FROM ARRAY FROM LCP MAIN                    M29.4278
      K=0                                                               M29.4279
    3 K=K+1                                                             M29.4280
      DO 50 J=1,M                                                       M29.4281
C     N=MAX0Z(J,K)                                                      M36.3600
      N = J                                                             M36.3601
      IF ( N .LT. K )  N = K                                            M36.3602
      N=N*(N-3)/2+J+K                                                   M29.4283
      ER(J)=P(N)                                                        M31.6449
   50 EI(J)=Z(N)                                                        M31.6450
C **** THE K-TH COLUMN OF R+XWL IS STORED                               M29.4286
      I=0                                                               M29.4287
    2 I=I+1                                                             M29.4288
      CR=0.0                                                            M29.4289
      CL=0.0                                                            M29.4290
      DO 1 J=1,M                                                        M29.4291
C     N=MAX0Z(J,I)                                                      M36.3603
      N = J                                                             M36.3604
      IF ( N .LT. I )  N = I                                            M36.3605
      N=N*(N-3)/2+J+I                                                   M29.4293
      CC=XWY(N)                                                         M31.6451
      IF ( LASTOV .EQ. 1 )  CONDUC = 0.0                                        
      CR=CR+CC*ER(J)+CONDUC*EI(J)                                       M32, 343
    1 CL=CL-CC*EI(J)+CONDUC*ER(J)                                       M32, 344
C **** THE ELEMENT OF ROW I AND COLUMN K OF YZ IS OBTAINED              M29.4297
      YZR(I,K)=CL                                                       M29.4298
      YZI(I,K)=CR                                                       M29.4299
      IF (IPRSUP .GE. 1 )                                               M29.4300
     1 WRITE (LUNIT6, 350) I,K,YZR(I,K),YZI(I,K)                        M29.4301
  350 FORMAT ( 46H       I       K       YZR(I,K)       YZI(I,K)        M29.4302
     1  ,/,  2I8,  2E15.6 )                                             M29.4303
      IF(I.LT.M)GO TO 2                                                 M29.4304
C **** YZ IS OBTAINED IN ORDER OF (1,1),(2,1),...,(1,2),(2,2),COLUMNWISEM29.4305
      IF(K.LT.M)GO TO 3                                                 M29.4306
C *** FIND EIGENVALUES + EIGENVECTORS ***                               M29.4307
      IF ( KEXACT .EQ. 88333 )  GO TO 6999
      IF ( ITRNSF .EQ. -1 )  GO TO 459                                  M32, 345
      IF ( LASTOV .EQ. 1 .OR. ITRNSF .NE. 1)  GO TO 7011                M43.3667
C $$$$  CHANGE TO ZY-MATRIX i.e. TRANSFORMATION MATRIX IS TV   $$$$     M43.3668
 6999 DO 7001 I=1,M                                                     
      DO 7001 J=1,M                                                     M43.3670
      TEMPR(I,J)=YZR(J,I)                                               M43.3671
      TEMPI(I,J)=YZI(J,I)                                               M43.3672
 7001 CONTINUE                                                          M43.3673
      DO 7008 I=1,M                                                     M43.3674
      DO 7008 J=1,M                                                     M43.3675
      YZR(I,J) = TEMPR(I,J)                                             M43.3676
      YZI(I,J) = TEMPI(I,J)                                             M43.3677
 7008 CONTINUE                                                          M43.3678
      CALL DCEIGN(YZR,YZI,TVI,TVR,ER,EI,M,M,IERROR,LL1,LL0,LUNIT6       M43.3679
     1, IPRSUP,NDIM)                                                            
C      write(*,*)(ER(i), i=1,m)                                         M43.3681
C      write(*,*)(EI(i), i=1,m)                                         M43.3682
      GO TO 7012                                                        M43.3683
 7011 CALL DCEIGN(YZR,YZI,TII,TIR,ER,EI,M,M,IERROR,LL1,LL0,LUNIT6       M43.3684
     1, IPRSUP,NDIM)                                                            
C      write(*,*)(ER(i), i=1,m)                                         M43.3686
C      write(*,*)(EI(i), i=1,m)                                         M43.3687
      GO TO 7072                                                        M43.3688
 7012 KTHL=1                                                            M43.3689
      PING(KTHL)=ALOG1Z(FREQ)                                           M43.3690
      D55 = 1.0 / FREQ                                                  M43.3691
      D56 = 1.0 / SQRTZ( FREQ )                                         M43.3692
      DO 5600 I=1,M                                                     M43.3693
      EM=SQRTZ( ER(I)*ER(I)+EI(I)*EI(I) )                               M43.3694
      EA=ATAN2Z(EI(I),ER(I))                                            M43.3695
      P(I) = SQRTZ(EM)                                                          
      Z(I) = EA /2 
      IF ( KEXACT .EQ. 88333 )  GO TO 5600                                                             
      CC=SQRTZ(EM)*COSZ(EA/2.)                                          M43.3698
      D14=SQRTZ(EM)*SINZ(EA/2.)                                         M43.3699
      VMODE=D13/D14                                                     M43.3700
C $$$$  PUT DATA IN PING VECTOR  $$$$                                   M43.3701
      PING(KTHL+1) = EM*D55                                                     
      PING(KTHL+2) = EA/(TWOPI/360.)                                            
      PING(KTHL+3) = VMODE                                                      
      PING(KTHL+4) = CC*D56                                                     
      KTHL = KTHL + 4                                                           
 5600 CONTINUE                                                          M43.3707
      IF ( IPRSUP .LT. 1 )  GO TO 5602                                  M43.3708
      WRITE(LUNIT6,*) ' Tv before UNWIND. '                             M43.3709
      DO 2411  I = 1, M                                                 M43.3710
      WRITE (LUNIT6,59) ( TVR(I,J), J=1,M )                             M43.3711
      WRITE( LUNIT6,59) ( TVI(I,J), J=1,M )                             M43.3712
 2411 CONTINUE                                                          M43.3713
 5602 IF ( KEXACT .EQ. 88333 )  GO TO 5620
      CALL UNWIND( PING,KTHL,MRR,NRP,NTOL,ISEQ )                        M43.3714
      IF ( IPRSUP .LT. 1 )  GO TO 5605                                  M43.3715
      WRITE(LUNIT6, *) ' ISEQ after UNWIND are, ', (ISEQ(I),I=1,M)      M43.3716
      WRITE(LUNIT6, *) ' PING after UNWIND are, ', (PING(I),I=1,KTHL)   M43.3717
 5605 CONTINUE                                                          M43.3718
C $$$$  BEGIN THE PROCESS OF EIGENVECTORS  $$$$                         M43.3719
      DO 5613 I=1,M                                                     M43.3720
      EE1(I)=P(I)                                                       M43.3721
      EE2(I)=Z(I)                                                       M43.3722
      DO 5613 J=1,M                                                     M43.3723
c $$$$  use tv  $$$$                                                    M43.3724
      PP1(I,J)=TVR(I,J)                                                 M43.3725
      PP2(I,J)=TVI(I,J)                                                 M43.3726
 5613 CONTINUE                                                          M43.3727
      DO 5612 I=1,M                                                     M43.3728
      P(I)=EE1(ISEQ(I))                                                 M43.3729
      Z(I)=EE2(ISEQ(I))                                                 M43.3730
      DO 5612 J=1,M                                                     M43.3731
c $$$$ use tv  $$$$                                                     M43.3732
      TVR(I,J)=PP1(I,ISEQ(J))                                           M43.3733
      TVI(I,J)=PP2(I,ISEQ(J))                                           M43.3734
 5612 CONTINUE                                                          M43.3735
C $$$$ new code to get ti from tv-t $$$$                                M43.3736
 5620 LL=1                                                              
      DO 7002 I=1,M                                                     M43.3738
      DO 7003 J=1,M                                                     M43.3739
      TIXF(LL)=TVR(I,J)                                                 M43.3740
      TIXF(LL+1)=TVI(I,J)                                               M43.3741
      LL=LL+2                                                           M43.3742
 7003 CONTINUE                                                          M43.3743
 7002 CONTINUE                                                          M43.3744
      CALL COMINV(TIXF(1),WORK1(1),M,FREQ)                              M43.3745
      LL=1                                                              M43.3746
 7766 IF (IPRSUP .GE. 3)                                                M43.3747
     1 WRITE(LUNIT6,*) ' Before DO 7004.  M, NDIM, FREQ =',             M43.3748
     1                               M, NDIM, FREQ                      M43.3749
      DO 7004 J=1,M                                                     M43.3750
      DO 7005 I=1,M                                                     M43.3751
      TIR(I,J)=WORK1(LL)                                                M43.3752
      TII(I,J)=WORK1(LL+1)                                              M43.3753
      LL=LL+2                                                           M43.3754
 7005 CONTINUE                                                          M43.3755
 7004 CONTINUE                                                          M43.3756
      IF ( KEXACT .EQ. 88333 )  GO TO 7072
      DO 5611 I=1,M                                                     M43.3758
      DO 5621 J=1,M                                                     M43.3759
      PP1(I,J)=SQRTZ( (TIR(I,J))**2+(TII(I,J))**2 )                     M43.3760
      PP2(I,J)=ATAN2Z( TII(I,J) , TIR(I,J) )                            M43.3761
 5621 CONTINUE                                                          M43.3762
 5611 CONTINUE                                                          M43.3763
C                                                                       M43.3764
      IF ( NTOL.NE.1 ) GO TO 5812                                       M43.3765
      DO 5801 J=1,M                                                     M43.3766
      KMAX(J)=1                                                         M43.3767
      DO 5802 I=1,M                                                     M43.3768
      IF ( PP1(I,J) .LE. PP1(KMAX(J),J) ) GO TO 5802                    M43.3769
      KMAX(J)=I                                                         M43.3770
 5802 CONTINUE                                                          M43.3771
 5801 CONTINUE                                                          M43.3772
      WRITE (LUNIT9) ( KMAX(J), J = 1,M )                               M43.3773
 5812 DO 5614 J=1,M                                                     M43.3774
      IF ( PP1(KMAX(J),J) .EQ. 0.0 ) GO TO 5614                         M43.3775
      DV=1.0/PP1(KMAX(J),J)                                             M43.3776
      DA=PP2(KMAX(J),J)                                                 M43.3777
      IF (IPRSUP .GE. 3)                                                M43.3778
     1  write(*,*) 'In do loop 5614, j=, kmax(j)=,dv=,da=',             M43.3779
     1                             j, kmax(j), dv, da                   M43.3780
      DO 5624 I=1,M                                                     M43.3781
      IF (IPRSUP .GE. 3)                                                M43.3782
     1 write(*,*) 'In do loop 5624, i, j, pp1(i,j), pp2(i,j)',          M43.3783
     1                              i, j, pp1(i,j), pp2(i,j)            M43.3784
      PP1(I,J)=PP1(I,J)*DV                                              M43.3785
      PP2(I,J)=PP2(I,J)-DA                                              M43.3786
C     *** to keep angles within principal value region                  M43.3787
      IF ( PP2(I,J) .GT. TWOPI/2. )  PP2(I,J)=PP2(I,J) - TWOPI          M43.3788
      IF ( PP2(I,J) .LT.-TWOPI/2. )  PP2(I,J)=PP2(I,J) + TWOPI          M43.3789
      IF (IPRSUP .GE. 3)                                                M43.3790
     1 write(*,*) 'just before 5624, i, j, pp1(i,j), pp2(i,j)',         M43.3791
     1                              i, j, pp1(i,j), pp2(i,j)            M43.3792
 5624 CONTINUE                                                          M43.3793
 5614 CONTINUE                                                          M43.3794
CC    ** fix the error of using  TI as TV  **                           M43.3795
      DO 6001 I=1,M                                                     M43.3796
      DO 6001 J=1,M                                                     M43.3797
      TIR(I,J)=(PP1(I,J))*COSZ(PP2(I,J))                                M43.3798
      TII(I,J)=(PP1(I,J))*SINZ(PP2(I,J))                                M43.3799
 6001 CONTINUE                                                          M43.3800
 7120 IF ( IPRSUP .GE. 1 )                                              M43.3801
     1 WRITE (LUNIT6, 400) FREQ, ((TIR(I,J),TII(I,J),J=1,M),I=1,M)      M43.3802
  400 FORMAT (' AT ', E12.5, 'Hz, EIGENVECTORS TIR AND TII AFTER SCALINGM43.3803
     1 ARE ' ,/,   (1X, 8E15.6) )                                       M43.3804
 7072 IF(IERROR.EQ.0)GO TO 463                                          M43.3805
      KILL = 221                                                        M29.4311
      LSTAT(19) = 72                                                    M29.4312
      LSTAT(14) = IERROR                                                M29.4313
      GO TO 9200                                                        M29.4314
  459 DO  462  J = 1, M                                                 M32, 347
         DO  462  I = 1, M, 3                                           M32, 348
            IJ = I + 2                                                  M32, 349
            CALL CIMAGE                                                 M32, 350
            DECODE( 80, 469, ABUFF(1) )                                 M32, 351
     1               ( TIR(J,IBK), TII(J,IBK), IBK = I, IJ )            M32, 352
  469 FORMAT( 6E13.0 )                                                  M32, 353
  462 CONTINUE                                                          M32, 354
CC   $$$$ skip the rotation and normalization  for freq-dep [T] ??      M43.3806
  463 IF( LASTOV.EQ.39 .AND. ITRNSF.EQ. 1  .OR. KEXACT .EQ. 88333) 
     1 GO TO 2500                          
      IF ( ITRNSF .EQ. -9  .OR.  ITRNSF .EQ. -1 )  GO TO 464            M36.3607
      IF ( IPRSUP .LT. 4 ) GO TO 88                                     M43.3808
      WRITE (LUNIT6, 1462)                                              M43.3809
 1462 FORMAT( 65H CURRENT TRANSFORMATION MATRIX BEFORE ZEROING THE IMAGIM32, 357
     1NARY PART: )                                                      M32, 358
      WRITE (LUNIT6, 39)                                                M32, 359
      DO 1472  K = 1, M                                                 M32, 360
      WRITE(LUNIT6,9) (TIR(K,J),J=1,M)                                  M32, 361
 1472 CONTINUE                                                          M32, 362
      WRITE (LUNIT6, 60)                                                M32, 363
      DO 1482  K = 1, M                                                 M32, 364
      WRITE (LUNIT6,9) (TII(K,J),J=1,M)                                 M32, 365
 1482 CONTINUE                                                          M32, 366
   88 CONTINUE                                                          M43.3810
      DO 465 J = 1, M                                                   M32, 368
      CR = 0.                                                           M32, 369
      FNORM = 0.                                                        M32, 370
      DO  466  I = 1, M                                                 M32, 371
      FNORM = FNORM +(TIR(I,J)**2 - TII(I,J)**2)                        M32, 372
      CR = CR + TIR(I,J) * TII(I,J)                                     M32, 373
 466  CONTINUE                                                          M32, 374
      CR = -2.0 * CR                                                    M32, 375
      THETA = ATAN2Z( CR, FNORM) / 2.                                   M32, 376
      CR = COSZ( THETA )                                                M32, 377
      CL = SINZ( THETA )                                                M32, 378
      DO 467 I = 1, M                                                   M32, 379
      TIR(I,J) = TIR(I,J) * CR - TII(I,J) * CL                          M32, 380
 467  TII(I,J) = 0.                                                     M32, 381
 465  CONTINUE                                                          M32, 382
 464  DO  5  I = 1, M                                                   M32, 383
      FNORM=0.0                                                         M29.4322
      DO 26 J=1,M                                                       M29.4323
   26 FNORM=FNORM+TIR(J,I)**2+TII(J,I)**2                               M29.4324
      FNORM=1./SQRTZ(FNORM)                                             M29.4325
      DO 5 J=1,M                                                        M29.4326
      TII(J,I)=TII(J,I)*FNORM                                           M29.4327
      TIR(J,I)=TIR(J,I)*FNORM                                           M31.6454
    5 CONTINUE                                                          M31.6455
C $$$$  CALCULATION OF TV FROM TI  $$$$                                 M43.3811
 2500 LL=1                                                              M43.3812
      DO 6002 I=1,M                                                     M43.3813
      DO 6003 J=1,M                                                     M43.3814
      TIXF(LL)=TIR(I,J)                                                         
      TIXF(LL+1)=TII(I,J)                                               M43.3816
      LL=LL+2                                                           M43.3817
 6003 CONTINUE                                                          M43.3818
 6002 CONTINUE                                                          M43.3819
      CALL COMINV(TIXF(1),WORK1(1),M,FREQ)                              M43.3820
      LL=1                                                              M43.3821
      DO 6004 J=1,M                                                     M43.3822
      DO 6005 I=1,M                                                     M43.3823
      TVR(I,J)=WORK1(LL)                                                        
      TVI(I,J)=WORK1(LL+1)                                              M43.3825
      LL=LL+2                                                           M43.3826
 6005 CONTINUE                                                          M43.3827
 6004 CONTINUE                                                          M43.3828
CC    ** Now, we have both TI and TV **                                 M43.3829
      IF ( IPRSUP .GE. 1 )                                              M43.3830
     1 WRITE (LUNIT6, 403)  ((TVR(I,J),TVI(I,J),J=1,M),I=1,M)           M43.3831
  403 FORMAT (' EIGENVECTORS TVR AND TVI ARE ',/,(1X, 8E15.6))          M43.3832
      IF (LASTOV .EQ. 39 .AND. ITRNSF .NE. 1 )  GO TO 166               M43.3833
      IF ( LASTOV  .NE.  1 )   GO TO 2520                               M43.3834
      WRITE (LUNIT7, 8769)  TCLOCK, DATE1                               M36.3609
 8769 FORMAT (    40HC    PUNCHED OUTPUT OF K. C. LEE'S LINE ,          M36.3610
     1            15H WHICH BEGAN AT,  2X,  2A4,  2X,  2A4  )           M36.3611
      WRITE (LUNIT7, 8770)  FREQ                                        M44. 868
 8770 FORMAT ( 'C  ***** UNTRANSPOSED K.C. LEE LINE SEGMENT CALCULATED AM44. 869
     1T', 2X, E10.3,  ' HZ. *****')                                     M44. 870
      REWIND LUNIT2                                                             
      N5 = KFULL+NFREQ+1                                                        
      DO 8774  N12=1, N5                                                M39.3603
      READ (LUNIT2, 8771, END=8775)  ( TEXTA6(I), I=1, 14 )             M43.3835
 8771 FORMAT ( 13A6, A2 )                                               M39.3605
      WRITE (LUNIT7, 8772)  ( TEXTA6(I), I=1, 13 )                      M39.3606
 8772 FORMAT ( 2HC ,  13A6 )                                            M39.3607
 8774 CONTINUE                                                          M39.3608
 8775 WRITE (LUNIT7, 28)                                                M43.3836
   28 FORMAT (11H$VINTAGE, 1)                                           M29.4331
C ****** TO GET MODAL Y *********                                       M43.3837
 2520 DO 40 I=1,M                                                       M43.3838
      DO 41 J=1,M                                                       M29.4334
C     $$$ Now, using TV to get Ymode $$$                                M43.3839
      XTIR(J)=TVR(J,I)                                                  M43.3840
      XTII(J)=TVI(J,I)                                                  M43.3841
   41 CONTINUE                                                          M31.6458
      CALL MULT(XWY,XTIR,DUMMI,M,LL0)                                   M32, 384
      B = 0.0                                                           M29.4338
      CALL MULT(XWY,XTII,DUMMR,M,LL0)                                   M32, 385
C *** XWY  IS J(WC) ******                                              M32, 386
      DO 3141 J = 1, M                                                  M32, 387
      DUMMR(J) = - DUMMR(J)                                             M32, 388
 3141 CONTINUE                                                          M32, 389
      DO 3151 J = 1, M                                                  M32, 390
      DUMMI(J) = DUMMI(J) + CONDUC * XTII(J)                            M32, 391
      DUMMR(J) = DUMMR(J) + CONDUC * XTIR(J)                            M32, 392
 3151 CONTINUE                                                          M32, 393
      G = 0.0                                                           M29.4341
      DO 43 IG=1,M                                                      M29.4342
      C= DUMMR(IG)                                                      M31.6462
      D1= DUMMI(IG)                                                     M32, 394
      XA=XTIR(IG)                                                       M29.4345
      XB=XTII(IG)                                                       M29.4346
      B=B+XA*D1+XB*C                                                            
   43 G=G+XA*C-XB*D1                                                            
      IF ( IPRSUP .GE. 1 )                                              M43.3842
     1 WRITE (LUNIT6, *) ' Modal admittance Ym for mode ', I, G, B      M43.3843
   44 FORMAT(//,106H MODE   RESISTANCE  REACTANCE  SUSCEPTANCE          M29.4352
     1   SURGE IMPEDANCE(OHM)          VELOCITY  ATTENUATION)           M29.4353
   65 FORMAT(104H          OHM/MILE    OHM/MILE   S/MILE        REAL    M29.4354
     1     IMAG       LOSSLESS     MILE/SEC  NEPER/MILE)                M29.4355
 7031 FORMAT(102H          OHM/KM      OHM/KM     S/KM         REAL     M29.4356
     1     IMAG      LOSSLESS     KM/SEC     NEPER/KM)                  M29.4357
   33 FORMAT(I5,3X,9E12.5)                                              M29.4358
C TO GET ROTATION FOR  Y=0+XWC                                          M29.4359
      YA = ATAN2Z(B,G)
      THETA=PI*ONEHAF- YA
      THETA2(I)=-THETA*ONEHAF                                           M29.4361
      DUMM1=SQRTZ(G*G+B*B)                                              M29.4362
      CONTINUE                                                          M43.3844
C **** TO GET ZMODAL, ZSURGE, VMODE FROM YMODAL & EIGENVALUES ****      M29.4363
      IF ( LASTOV .EQ. 1 )  GO TO 2900                                  M36.3617
CC  ** NEW CODING **                                                    M43.3845
      IF ( ITRNSF .NE. 1 .AND. KEXACT .NE. 88333 )  GO TO 7041                                 
      GAMMA = P(I)                                                              
      EA = Z(I)                                                                 
 1232 YCHARM = DUMM1 / GAMMA                                                       
      YCHARA = YA - EA                                                  M43.3852
C  ***********************************                                  M43.3853
C     Comment the following two lines if Ych is to be fitted            M43.3854
C      YCHARM = 1./YCHARM    !  Zch = 1 / Ych                           M43.3855
C      YCHARA = - YCHARA                                                M43.3856
C   ********************************                                    M43.3857
      ALPHA = GAMMA * COSZ(EA)                                          M43.3858
      BETA = GAMMA * SINZ(EA)                                           M43.3859
      IF ( KEXACT .NE. 88333 )  GO TO 7033
      IF ( METRIK .EQ. 1 )  DIST = DIST * FMIPKM
      ALPHA = ALPHA * DIST
      BETA = BETA * DIST
      GO TO 155
 7033 ALPHA = ALPHA * FMIPKM                                                    
      BETA = BETA * FMIPKM                                              M43.3861
      GO TO 155                                                         M43.3862
 7041 ZR = 0.0                                                          M43.3863
      ZI = 0.0                                                          M43.3864
      KK1 = 2 * M                                                       M39.3609
      KI1 = 2 * I - KK1 - 1                                             M39.3610
      DO 2800 JJ = 1, M                                                 M31.6470
      KK = KK1 * JJ + KI1                                               M39.3611
      DO 2700  L = 1, M                                                 M31.6472
      CR = 0.0                                                          M31.6473
      CI = 0.0                                                          M31.6474
      DO 2600  J = 1, M                                                 M31.6475
C     N = MAX0Z(J,L)                                                    M36.3622
      N = J                                                             M36.3623
      IF ( N .LT. L )  N = L                                            M36.3624
      N = N * (N-3) / 2 + J + L                                         M31.6477
      ER(J) = P(N)                                                      M31.6478
      EI(J) = Z(N)                                                      M31.6479
      K = KK1 * J + KI1                                                 M39.3612
      CR = CR + ER(J)*TIXF(K) - EI(J)*TIXF(K+1)                         M43.3865
      CI = CI + ER(J)*TIXF(K+1) + EI(J)*TIXF(K)                         M43.3866
 2600 CONTINUE                                                          M31.6483
      DUMMR(L) = CR                                                     M31.6484
      DUMMI(L) = CI                                                     M31.6485
 2700 CONTINUE                                                          M31.6486
      ZR = ZR + TIXF(KK)*DUMMR(JJ) - TIXF(KK+1)*DUMMI(JJ)               M43.3867
      ZI = ZI + TIXF(KK)*DUMMI(JJ) + TIXF(KK+1)*DUMMR(JJ)               M43.3868
 2800 CONTINUE                                                          M31.6489
 2899 DR = ZR * G - ZI * B                                              M43.3869
      DI = ZR * B + ZI * G                                              M43.3870
      EM = SQRTZ( DR*DR + DI*DI )                                       M43.3871
      GAMMA = SQRTZ( EM )                                               M43.3872
      EA = ATAN2Z( DI, DR ) / 2.                                        M43.3873
      YM=SQRTZ( G**2 + B**2 )                                           M43.3874
      YA=ATAN2Z( B, G )                                                 M43.3875
      GO TO 1232                                                        M43.3876
 2900 G=EI(I)/DUMM1                                                     M31.6491
      B=-ER(I)/DUMM1                                                    M29.4365
   58 FORMAT(2X,10E11.4)                                                M29.4366
 2950 ZSURGE(I)=SQRTZ(B/DUMM1)                                          M31.6492
      CC=SQRTZ(G**2+B**2)                                               M29.4368
      THETA=ATAN2Z(-G,B)*ONEHAF                                         M29.4369
      ZCHARR=SQRTZ(CC/DUMM1)                                            M29.4370
      ZCHARI=ZCHARR*SINZ(THETA)                                         M29.4371
      ZCHARR=ZCHARR*COSZ(THETA)                                         M29.4372
      THETA=THETA+PI*ONEHAF                                             M29.4373
      CC=SQRTZ(CC*DUMM1)                                                M29.4374
      D9 = CC * SINZ(THETA)                                             M30.1330
      VMODE = D13 / D9                                                  M31.6493
      CC=CC*COSZ(THETA)                                                 M29.4376
      IF (METRIK .EQ. 0)  GO TO 137                                     M30.1332
      G = G * FMIPKM                                                    M30.1333
      B = B * FMIPKM                                                    M30.1334
      DUMM1 = DUMM1 * FMIPKM                                            M30.1335
      VMODE = VMODE / FMIPKM                                            M30.1336
      CC = CC * FMIPKM                                                  M30.1337
      D9 = D9 * FMIPKM                                                  M30.1338
  137 WRITE(LUNIT6,33)I,G,B,DUMM1,ZCHARR,ZCHARI,ZSURGE(I),VMODE,CC      M43.3877
      DISTM = -DIST                                                     M29.4378
      IF ( M  .LT.  10     .AND.     LASTOV  .EQ.  1 )                  M32, 412
     1 WRITE (LUNIT7,140) I, BRNAME(2*I-1), BRNAME(2*I),                M31.6495
     1             G,ZSURGE(I),VMODE,DISTM,M                            M29.4380
  140 FORMAT (1H-,I1,2A6,12X,4E12.5,2H 1,2X,I1)                         M29.4381
      IF ( M .GE. 10  .AND.  LASTOV .EQ. 1)  GO TO 141                  M32, 413
      GO TO 40                                                          M43.3878
  141 IF ( M .EQ. 10 )  TEXT1 = TEXT2                                   M32, 415
      IF ( M .EQ. 11 )  TEXT1 = TEXT3                                   M32, 416
      IF ( M .EQ. 12 )  TEXT1 = TEXT4                                   M32, 417
      IF ( M .EQ. 13 )  TEXT1 = TEXT5                                   M32, 418
      IF ( M .EQ. 14 )  TEXT1 = TEXT6                                   M32, 419
      IF ( M .EQ. 15 )  TEXT1 = TEXT7                                   M32, 420
      IF ( M .EQ. 16 )  TEXT1 = TEXT8                                   M32, 421
      IF ( M .EQ. 17 )  TEXT1 = TEXT9                                   M32, 422
      IF ( M .EQ. 18 )  TEXT1 = TEXT10                                  M32, 423
      IF ( I .LE. 9 )                                                   M32, 424
     1WRITE (LUNIT7,143) I, BRNAME(2*I-1), BRNAME(2*I),                 M32, 425
     1             G,ZSURGE(I),VMODE,DISTM,TEXT1                        M32, 426
  143 FORMAT (1H-,I1,2A6,12X,4E12.5,2H 1,2X,A1)                         M32, 427
      IF ( I .GT. 9 )                                                   M32, 428
     1WRITE (LUNIT7,145) I, BRNAME(2*I-1), BRNAME(2*I),                 M32, 429
     1             G,ZSURGE(I),VMODE,DISTM,TEXT1                        M32, 430
  145 FORMAT (I2,2A6,12X,4E12.5,2H 1,2X,A1)                             M32, 431
      GO TO 40                                                          M43.3879
  155 WRITE(LUNIT9) D13,YCHARM,YCHARA,ALPHA,BETA                        M43.3880
      IF ( IPRSUP .GE. 1 )                                              M43.3881
     1  WRITE (LUNIT6, 158) D13, I, YCHARM,YCHARA,ALPHA,BETA            M43.3882
  158 FORMAT (' AT ', E12.5,' Hz,YCHARM,YCHARA,ALPHA,BETA FOR           M43.3883
     1MODE',  I5, ' ARE', 5X, 4E12.5)                                   M43.3884
      IF ( KEXACT .EQ. 88333 )  GO TO 40
      IF ( ITRNSF .NE. 1 )  GO TO 40                                    M43.3885
      WRITE(LUNIT9) (PP1(J,I),PP2(J,I), J=1,M )                                 
   40 CONTINUE                                                          M29.4382
      IF ( KEXACT .NE. 88333 )  GO TO 119
      LL = 1
      DO 115 J = 1, M
      DO 115 IJ = 1, M
      TIXF(LL) = TIR(IJ,J)
      WORK1(LL) = TII(IJ,J)
      LL = LL + 1
  115 CONTINUE
      N9SQ = M * M
      WRITE(LUNIT9) ( TIXF(K),K=1,N9SQ), (WORK1(K),K=1,N9SQ)
      GO TO 9900
  119 IF ( LASTOV .EQ. 1 )  WRITE (LUNIT7,240)                          M30.1356
  240 FORMAT (11H$VINTAGE, 0)                                           M29.4384
      IF ( NFREQ .NE. 1  .AND.  LASTOV .EQ. 39 )  GO TO 9900            M43.3887
      DO 37 I=1,M                                                       M29.4385
      C=COSZ(THETA2(I))                                                 M29.4386
      D1=SINZ(THETA2(I))                                                M31.6503
      DO 37 J=1,M                                                       M43.3888
      CC=TIR(J,I)                                                       M29.4389
      DD=TII(J,I)                                                       M29.4390
      TIR(J,I)=CC*C-DD*D1                                               M31.6504
      TII(J,I)=CC*D1+DD*C                                               M31.6505
   37 CONTINUE                                                          M31.6511
C ***** END OF ROTATION OF TI MATRIX FOR K.C.LEE'S CONST.PARAM.*******  M32, 449
   39 FORMAT(//,33H      REAL COMPONENTS, ROW BY ROW)                   M29.4394
  166 WRITE(LUNIT6,66)                                                  M30.1361
   66 FORMAT(////,69H EIGENVECTOR MATRIX TI FOR CURRENT TRANSFORMATION IM29.4396
     1(PHASE)=TI*I(MODE))                                               M29.4397
      WRITE(LUNIT6,39)                                                  M29.4398
    9 FORMAT(2X,6E12.5)                                                 M29.4399
      DO 57 K=1,M                                                       M30.1362
      WRITE(LUNIT6,9)(TIR(K,J),J=1,M)                                   M29.4401
   59 FORMAT (6F12.8)                                                   M31.6516
   57 CONTINUE                                                          M29.4404
      WRITE(LUNIT6,60)                                                  M29.4405
   60 FORMAT(//,37H     IMAGINARY COMPONENTS, ROW BY ROW)               M29.4406
      DO 61 K=1,M                                                       M29.4407
   61 WRITE(LUNIT6,9)(TII(K,J),J=1,M)                                   M29.4408
      DO 1204  I =1, M                                                  M31.6517
      IF ( LASTOV .EQ. 39 )  GO TO 161                                  M43.3889
      WRITE (LUNIT7,59) ( TIR(I,J), J=1,M )                             M31.6520
      WRITE (LUNIT7,59) ( TII(I,J), J=1,M )                             M31.6521
      GO TO 1204                                                        M43.3890
  161 WRITE (LUNIT9)   (TIR(I,J), TII(I,J), J=1, M )                    M43.3891
 1204 CONTINUE                                                          M31.6530
      IF ( LASTOV .EQ. 39 )  GO TO 9900                                 M43.3892
C TO GET ZSURGE MATRIX IN PHASE DOMAIN                                  M29.4409
   75 DO 80 I=1,M                                                       M30.1363
      C = 1./ZSURGE(I)                                                  M43.3893
      DO 80 J=1,M                                                       M29.4412
   80 YZR(I,J)=TIR(J,I) * C                                             M43.3894
      N=0                                                               M29.4414
      DO 82 I=1,M                                                       M29.4415
      DO 82 K=1,I                                                       M29.4416
      C = 0.0                                                           M29.4417
      DO 81 J=1,M                                                       M29.4418
   81 C=C-TIR(I,J)*YZR(J,K)                                             M43.3895
      N=N+1                                                             M29.4420
   82 XWC(N)=C                                                          M31.6533
      CALL REDU44(XWC(1),WORKR1(1),M,LL0)                               M43.3896
      IF( KILL .NE. 0 ) GO TO 9200                                      M32, 455
      WRITE(LUNIT6,83)                                                  M43.3897
   83 FORMAT(/,' PHASE DOMAIN ZSURGE (REAL; THE IMAG. OF Ti IGNORED)')  M43.3898
      N=1                                                               M29.4426
      DO 84 I=1,M                                                       M29.4427
      K=N+I-1                                                           M29.4428
      WRITE(LUNIT6,9)(XWC(J),J=N,K)                                     M31.6535
   84 N=N+I                                                             M29.4430
      GO TO 9900                                                        M29.4431
 9200 LSTAT(18) = NCHAIN                                                M29.4432
      LASTOV = NCHAIN                                                   M29.4435
      NCHAIN = 51                                                       M29.4436
 9900 RETURN                                                            M29.4437
      END                                                               M29.4438
      SUBROUTINE COMINV(A,B,M,FREQ)                                     M32, 457
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C   THIS SUBROUTINE PERFORMS INVERSION OF A COMPLEX MATRIX              M31.6537
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      DIMENSION A(1), B(1), IJ(50)                                      M31.6540
      EPSPV2 = 1.0D-16                                                  M31.6541
      DO 110  I=1, M                                                    M31.6542
  110 IJ(I) = 0                                                         M31.6543
      NPH2 = M + M                                                      M31.6544
      N22 = 2 * M * M                                                   M31.6545
      NPHPI2 = NPH2 + 2                                                 M31.6546
      DO 120 J=1, N22                                                   M31.6547
  120 B(J) = A(J)                                                       M31.6548
      L = 0                                                             M31.6549
  130 L = L + 1                                                         M31.6550
      IF (L .GT. M) RETURN                                              M31.6551
      T = 0.                                                            M31.6552
      N1 = 1                                                            M31.6553
      DO 140 J=1, M                                                     M31.6554
      IF (IJ(J) .GT. 0) GO TO 140                                       M31.6555
      U = B(N1)*B(N1) + B(N1+1)*B(N1+1)                                 M31.6556
      IF (U .LE. T) GO TO 140                                           M31.6557
      T = U                                                             M31.6558
      N2 = J                                                            M31.6559
      K = N1                                                            M31.6560
  140 N1 = N1 + NPHPI2                                                  M31.6561
      IJ(N2) = 1                                                        M31.6562
      IF (T .GT. EPSPV2) GO TO  145                                     M31.6563
      KILL = 199                                                        M31.6564
      FLSTAT(15) = FREQ                                                 M32, 458
      FLSTAT(13) = T                                                    M31.6566
      FLSTAT(14) = EPSPV2                                               M31.6567
      LSTAT(13) = L                                                     M31.6568
      LSTAT(14) = M                                                     M31.6569
      LSTAT(19) = 145                                                   M31.6570
      RETURN                                                            M31.6571
  145 U = B(K)/T                                                        M31.6572
      T = -B(K+1)/T                                                     M31.6573
      B(K) = U                                                          M31.6574
      B(K+1) = T                                                        M31.6575
      N3 = (N2 - 1)*NPH2 + 1                                            M31.6576
      N2 = 2*N2 - 1                                                     M31.6577
      DO 160 I=1, NPH2, 2                                               M31.6578
      IF (I .EQ. N2) GO TO 160                                          M31.6579
      N1 = I + (N2 - 1)*M                                               M31.6580
      V = U*B(N1) - T*B(N1+1)                                           M31.6581
      D9 = U * B(N1+1)  +  T * B(N1)                                    M31.6582
      DO 150 J=1, N22, NPH2                                             M31.6583
      IF (J .EQ. N3) GO TO 150                                          M31.6584
      K = I + J                                                         M31.6585
      N4 = J + N2                                                       M31.6586
      B(K-1) = B(K-1)  -  B(N4-1) * V  +  B(N4) * D9                    M31.6587
      B(K) = B(K)  -  B(N4-1) * D9  -  B(N4) * V                        M31.6588
  150 CONTINUE                                                          M31.6589
  160 CONTINUE                                                          M31.6590
      K = N2                                                            M31.6591
      DO 170 I=1, NPH2, 2                                               M31.6592
      IF (I .EQ. N2) GO TO 170                                          M31.6593
      N4 = N3 + I                                                       M31.6594
      V = B(N4-1)*U - B(N4)*T                                           M31.6595
      B(N4) = B(N4-1)*T + B(N4)*U                                       M31.6596
      B(N4-1) = V                                                       M31.6597
      V = B(K+1)*T - B(K)*U                                             M31.6598
      B(K+1) = -B(K)*T - B(K+1)*U                                       M31.6599
      B(K) = V                                                          M31.6600
  170 K = K + NPH2                                                      M31.6601
      GO TO 130                                                         M31.6602
      END                                                               M31.6603
      SUBROUTINE DCEIGN(AR,AI,VI,VR,ER,EI,N,NM,IERR,NV,NB,LUNIT6        M31.6604
     1,IPRSUP,NDIM)                                                     M31.6605
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION AR(NDIM,NDIM),AI(NDIM,NDIM),ER(NDIM),EI(NDIM)           M31.6606
     1,VI(NDIM,NDIM),VR(NDIM,NDIM)                                      M31.6607
      DIMENSION SCALE(20),INT(20),IORD(20)                              M43.3899
      IF ( IPRSUP .GE. 1 )                                              M43.3900
     1 write (*,*) ' Top of DCEIGN.  Input matrix AR ....'              M43.3901
      do 2222 i=1,n                                                     M43.3902
      IF ( IPRSUP .GE. 1 )                                              M43.3903
     1 write(*,*) (ar(i,j), j=1,n)                                      M43.3904
 2222 continue                                                          M43.3905
      IF ( IPRSUP .GE. 1 )                                              M43.3906
     1 write (*,*) '                 Input matrix AI ....'              M43.3907
      do 2223 i=1,n                                                     M43.3908
      IF ( IPRSUP .GE. 1 )                                              M43.3909
     1 write(*,*) (ai(i,j), j=1,n)                                      M43.3910
 2223 continue                                                          M43.3911
      IF ( N .GT. NM ) GO TO 90                                         M43.3912
      LOW=1                                                             M29.4444
      NUPP=N                                                            M29.4445
      IF(NB.EQ.0) GO TO 1                                               M29.4446
C  DO BALANCING                                                         M29.4447
      CALL CBAL(NM,N,AR,AI,LOW,NUPP,SCALE,NDIM)                         M31.6609
C  DO TRANSFORMATION TO UPPER HESSENBERG FORM                           M29.4449
1     CONTINUE                                                          M29.4450
      IF (IPRSUP .GE. 3 )                                               M29.4451
     1  WRITE (LUNIT6, 300) NM,N,LOW,NUPP,NB,NV,((AR(I,J),AI(I,J),J=1,  M29.4452
     2                      NM),I=1,NM)                                 M29.4453
  300 FORMAT ( 49H BEFORE CALL COMHES, NM, N, LOW, NUPP, NB, NV ARE,    M29.4454
     1  6I5,/, 38H ((AR(I,J),AI(I,J),J=1,NM),I=1,NM) ARE,/, (1X,8E15.6))M29.4455
      CALL COMHES(NM,N,LOW,NUPP,AR,AI,INT,LUNIT6,IPRSUP,NDIM,IORD)      M31.6610
      IF(NV.EQ.0) GO TO 12                                              M29.4457
C  CALCULATE VALUES AND VECTORS                                         M29.4458
      IF ( IPRSUP .GE. 3 )                                              M29.4459
     1  WRITE (LUNIT6, 350) ((AR(I,J),AI(I,J),J=1,NM),I=1,NM)           M29.4460
  350 FORMAT ( 43H AFTER CALL COMHES, AR(I,J) AND AI(I,J) ARE,/,        M29.4461
     1     (1X, 8E15.6) )                                               M29.4462
      CALL COMLR2(NM,N,LOW,NUPP,INT,AR,AI,VI,VR,ER,EI,IERR,NDIM         M43.3913
     1,IORD)                                                            M43.3914
      IF (IPRSUP .LT. 3 ) GO TO 410                                     M43.3915
      WRITE (LUNIT6,400)  (ER(I),EI(I),I=1,NM)                          M43.3916
  400 FORMAT ( 49H AFTER CALL COMLR2 IN DCEIGN, THE EIGENVALUES ARE,/,  M29.4467
     1     ( 1X, 8E15.6), / )                                           M43.3917
      WRITE (LUNIT6, 405)  ((VR(I,J),VI(I,J),J=1,NM),I=1,NM)            M43.3918
  405 FORMAT ( 21H AND EIGENVECTORS ARE,/ (1X,8E15.6) )                 M43.3919
  410 IF(IERR.NE.0) GO TO 5                                             M43.3920
C  TRANSFORM VECTORS TO VECTORS OF ORIGINAL MATRIX                      M29.4470
      IF(NB.EQ.0) RETURN                                                M29.4471
      CALL CBABK2(NM,N,LOW,NUPP,SCALE,N,VI,VR,NDIM)                     M31.6612
      RETURN                                                            M29.4473
C  CALCULATE VALUES ONLY                                                M29.4474
12    CONTINUE                                                          M29.4475
      CALL COMLR(NM,N,LOW,NUPP,AR,AI,ER,EI,IERR,NDIM)                   M31.6613
      IF(IERR.NE.0) GO TO 5                                             M29.4477
      RETURN                                                            M29.4478
C  OUTPUT ERROR MESSAGES                                                M29.4479
5     WRITE(LUNIT6,6) IERR                                              M29.4480
    6 FORMAT ( 31H ***WARNING-DCEIGN:  EIGENVALUE,  I4,                 M29.4481
     1       18H DID NOT CONVERGE.)                                     M29.4482
      RETURN                                                            M29.4483
90    IERR=-1                                                           M29.4484
      WRITE(LUNIT6,91) N,NM                                             M29.4485
   91 FORMAT(52H **ERROR-DCEIGN:  ORDER OF MATRIX GREATER THAN FIRST,   M32, 459
     1 20H DIMENSION OF MATRIX,/,18X,6HORDER=,I5,                       M29.4487
     2 17H FIRST DIMENSION=, I5)                                        M29.4488
      write (*,*) 'output matrix AR '                                   M43.3921
      do 2224 i=1,n                                                     M43.3922
      write(*,*) (ar(i,j), j=1,n)                                       M43.3923
 2224 continue                                                          M43.3924
      write (*,*) 'output matrix AI '                                   M43.3925
      do 2225 i=1,n                                                     M43.3926
      write(*,*) (ai(i,j), j=1,n)                                       M43.3927
 2225 continue                                                          M43.3928
      RETURN                                                            M29.4489
      END                                                               M29.4490
      SUBROUTINE CBAL(NM,N,AR,AI,LOW,IGH,SCALE,NDIM)                    M31.6614
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION AR(NDIM,NDIM),AI(NDIM,NDIM),SCALE(20)                   M31.6615
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE           M29.4493
C     CBALANCE, WHICH IS A COMPLEX VERSION OF BALANCE,                  M29.4494
C     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH.              M29.4495
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).   M29.4496
C                                                                       M29.4497
C     THIS SUBROUTINE BALANCES A COMPLEX MATRIX AND ISOLATES            M29.4498
C     EIGENVALUES WHENEVER POSSIBLE.                                    M29.4499
C                                                                       M29.4500
C     ON INPUT:                                                         M29.4501
C                                                                       M29.4502
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         M29.4503
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          M29.4504
C          DIMENSION STATEMENT;                                         M29.4505
C                                                                       M29.4506
C        N IS THE ORDER OF THE MATRIX;                                  M29.4507
C                                                                       M29.4508
C        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.4509
C          RESPECTIVELY, OF THE COMPLEX MATRIX TO BE BALANCED.          M29.4510
C                                                                       M29.4511
C     ON OUTPUT:                                                        M29.4512
C                                                                       M29.4513
C        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.4514
C          RESPECTIVELY, OF THE BALANCED MATRIX;                        M29.4515
C                                                                       M29.4516
C        LOW AND IGH ARE TWO INTEGERS SUCH THAT AR(I,J) AND AI(I,J)     M29.4517
C          ARE EQUAL TO ZERO IF                                         M29.4518
C           (1) I IS GREATER THAN J AND                                 M29.4519
C           (2) J=1,...,LOW-1 OR I=IGH+1,...,N;                         M29.4520
C                                                                       M29.4521
C        SCALE CONTAINS INFORMATION DETERMINING THE                     M29.4522
C           PERMUTATIONS AND SCALING FACTORS USED.                      M29.4523
C                                                                       M29.4524
C     SUPPOSE THAT THE PRINCIPAL SUBMATRIX IN ROWS LOW THROUGH IGH      M29.4525
C     HAS BEEN BALANCED, THAT P(J) DENOTES THE INDEX INTERCHANGED       M29.4526
C     WITH J DURING THE PERMUTATION STEP, AND THAT THE ELEMENTS         M29.4527
C     OF THE DIAGONAL MATRIX USED ARE DENOTED BY D(I,J).  THEN          M29.4528
C        SCALE(J) = P(J),    FOR J = 1,...,LOW-1                        M29.4529
C                 = D(J,J)       J = LOW,...,IGH                        M29.4530
C                 = P(J)         J = IGH+1,...,N.                       M29.4531
C     THE ORDER IN WHICH THE INTERCHANGES ARE MADE IS N TO IGH+1,       M29.4532
C     THEN 1 TO LOW-1.                                                  M29.4533
C                                                                       M29.4534
C     NOTE THAT 1 IS RETURNED FOR IGH IF IGH IS ZERO FORMALLY.          M29.4535
C                                                                       M29.4536
C     THE ALGOL PROCEDURE EXC CONTAINED IN CBALANCE APPEARS IN          M29.4537
C     CBAL  IN LINE.  (NOTE THAT THE ALGOL ROLES OF IDENTIFIERS         M29.4538
C     K,L HAVE BEEN REVERSED.)                                          M29.4539
C                                                                       M29.4540
C     ARITHMETIC IS REAL THROUGHOUT.                                    M29.4541
C                                                                       M29.4542
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,        M29.4543
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY         M29.4544
C                                                                       M29.4545
C     ------------------------------------------------------------------M29.4546
C                                                                       M29.4547
C     :::::::::: RADIX IS A MACHINE DEPENDENT PARAMETER SPECIFYING      M29.4548
C                THE BASE OF THE MACHINE FLOATING POINT REPRESENTATION. M29.4549
C                RADIX = 16.0D0 FOR LONG FORM ARITHMETIC                M29.4550
C                ON S360 ::::::::::                                     M29.4551
C     DATA RADIX/Z4210000000000000/                                     M29.4552
C                                                                       M29.4553
C     &&&&&&&&&&&&&&&   TEMPORARY PATCH FOR F4V VAX COMPILER:           M29.4554
      RADIX = 16.0D0                                                    M29.4555
C     DATA STATEMENT JUST ABOVE IS DISABLED (ERROR FOR F4V). &&&        M29.4556
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&  END TEMPORARY PATCH.  23 JAN        M29.4557
      B2 = RADIX * RADIX                                                M29.4558
      K = 1                                                             M29.4559
      L = N                                                             M29.4560
      GO TO 100                                                         M29.4561
C     :::::::::: IN-LINE PROCEDURE FOR ROW AND                          M29.4562
C                COLUMN EXCHANGE ::::::::::                             M29.4563
   20 SCALE(M) = J                                                      M29.4564
      IF (J .EQ. M) GO TO 50                                            M29.4565
      DO 30 I = 1, L                                                    M29.4566
         F = AR(I,J)                                                    M29.4567
         AR(I,J) = AR(I,M)                                              M29.4568
         AR(I,M) = F                                                    M29.4569
         F = AI(I,J)                                                    M29.4570
         AI(I,J) = AI(I,M)                                              M29.4571
         AI(I,M) = F                                                    M29.4572
   30 CONTINUE                                                          M29.4573
      DO 40 I = K, N                                                    M29.4574
         F = AR(J,I)                                                    M29.4575
         AR(J,I) = AR(M,I)                                              M29.4576
         AR(M,I) = F                                                    M29.4577
         F = AI(J,I)                                                    M29.4578
         AI(J,I) = AI(M,I)                                              M29.4579
         AI(M,I) = F                                                    M29.4580
   40 CONTINUE                                                          M29.4581
   50 GO TO (80,130), IEXC                                              M29.4582
C     :::::::::: SEARCH FOR ROWS ISOLATING AN EIGENVALUE                M29.4583
C                AND PUSH THEM DOWN ::::::::::                          M29.4584
   80 IF (L .EQ. 1) GO TO 280                                           M29.4585
      L = L - 1                                                         M29.4586
C     :::::::::: FOR J=L STEP -1 UNTIL 1 DO -- ::::::::::               M29.4587
  100 DO 120 JJ = 1, L                                                  M29.4588
         J = L + 1 - JJ                                                 M29.4589
         DO 110 I = 1, L                                                M29.4590
            IF (I .EQ. J) GO TO 110                                     M29.4591
            IF (AR(J,I) .NE. 0.0 .OR. AI(J,I) .NE. 0.0) GO TO 120       M29.4592
  110    CONTINUE                                                       M29.4593
         M = L                                                          M29.4594
         IEXC = 1                                                       M29.4595
         GO TO 20                                                       M29.4596
  120 CONTINUE                                                          M29.4597
      GO TO 140                                                         M29.4598
C     :::::::::: SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE             M29.4599
C                AND PUSH THEM LEFT ::::::::::                          M29.4600
  130 K = K + 1                                                         M29.4601
  140 DO 170 J = K, L                                                   M29.4602
         DO 150 I = K, L                                                M29.4603
            IF (I .EQ. J) GO TO 150                                     M29.4604
            IF (AR(I,J) .NE. 0.0 .OR. AI(I,J) .NE. 0.0) GO TO 170       M29.4605
  150    CONTINUE                                                       M29.4606
         M = K                                                          M29.4607
         IEXC = 2                                                       M29.4608
         GO TO 20                                                       M29.4609
  170 CONTINUE                                                          M29.4610
C     :::::::::: NOW BALANCE THE SUBMATRIX IN ROWS K TO L ::::::::::    M29.4611
      DO 180 I = K, L                                                   M29.4612
  180 SCALE(I) = 1.0                                                    M29.4613
C     :::::::::: ITERATIVE LOOP FOR NORM REDUCTION ::::::::::           M29.4614
  190 NOCONV = 0                                                        M29.4615
      DO 270 I = K, L                                                   M29.4616
         C = 0.0                                                        M29.4617
         R = 0.0                                                        M29.4618
         DO 200 J = K, L                                                M29.4619
            IF (J .EQ. I) GO TO 200                                     M29.4620
            C = C + ABSZ(AR(J,I)) + ABSZ(AI(J,I))                       M29.4621
            R = R + ABSZ(AR(I,J)) + ABSZ(AI(I,J))                       M29.4622
  200    CONTINUE                                                       M29.4623
C     :::::::::: GUARD AGAINST ZERO C OR R DUE TO UNDERFLOW ::::::::::  M29.4624
         IF (C .EQ. 0.0 .OR. R .EQ. 0.0) GO TO 270                      M29.4625
         G = R / RADIX                                                  M29.4626
         F = 1.0                                                        M29.4627
         S = C + R                                                      M29.4628
  210    IF (C .GE. G) GO TO 220                                        M29.4629
         F = F * RADIX                                                  M29.4630
         C = C * B2                                                     M29.4631
         GO TO 210                                                      M29.4632
  220    G = R * RADIX                                                  M29.4633
  230    IF (C .LT. G) GO TO 240                                        M29.4634
         F = F / RADIX                                                  M29.4635
         C = C / B2                                                     M29.4636
         GO TO 230                                                      M29.4637
C     :::::::::: NOW BALANCE ::::::::::                                 M29.4638
  240    C1 = 0.95                                                      M29.4639
         IF ((C + R) / F .GE.  C1 * S) GO TO 270                        M29.4640
         G = 1.0 / F                                                    M29.4641
         SCALE(I) = SCALE(I) * F                                        M29.4642
         NOCONV = 1                                                     M29.4643
         DO 250 J = K, N                                                M29.4644
            AR(I,J) = AR(I,J) * G                                       M29.4645
            AI(I,J) = AI(I,J) * G                                       M29.4646
  250    CONTINUE                                                       M29.4647
         DO 260 J = 1, L                                                M29.4648
            AR(J,I) = AR(J,I) * F                                       M29.4649
            AI(J,I) = AI(J,I) * F                                       M29.4650
  260    CONTINUE                                                       M29.4651
  270 CONTINUE                                                          M29.4652
      IF (NOCONV .EQ. 1) GO TO 190                                      M29.4653
  280 LOW = K                                                           M29.4654
      IGH = L                                                           M29.4655
      RETURN                                                            M29.4656
      END                                                               M29.4657
      SUBROUTINE CBABK2(NM,N,LOW,IGH,SCALE,M,ZR,ZI,NDIM)                M31.6616
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION SCALE(20),ZR(NDIM,NDIM),ZI(NDIM,NDIM)                   M31.6617
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE           M29.4660
C     CBABK2, WHICH IS A COMPLEX VERSION OF BALBAK,                     M29.4661
C     NUM. MATH. 13, 293-304(1969) BY PARLETT AND REINSCH.              M29.4662
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).   M29.4663
C                                                                       M29.4664
C     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX GENERAL       M29.4665
C     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING            M29.4666
C     BALANCED MATRIX DETERMINED BY  CBAL.                              M29.4667
C                                                                       M29.4668
C     ON INPUT:                                                         M29.4669
C                                                                       M29.4670
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         M29.4671
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          M29.4672
C          DIMENSION STATEMENT;                                         M29.4673
C                                                                       M29.4674
C        N IS THE ORDER OF THE MATRIX;                                  M29.4675
C                                                                       M29.4676
C        LOW AND IGH ARE INTEGERS DETERMINED BY  CBAL;                  M29.4677
C                                                                       M29.4678
C        SCALE CONTAINS INFORMATION DETERMINING THE PERMUTATIONS        M29.4679
C          AND SCALING FACTORS USED BY  CBAL;                           M29.4680
C                                                                       M29.4681
C        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED;        M29.4682
C                                                                       M29.4683
C        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.4684
C          RESPECTIVELY, OF THE EIGENVECTORS TO BE                      M29.4685
C          BACK TRANSFORMED IN THEIR FIRST M COLUMNS.                   M29.4686
C                                                                       M29.4687
C     ON OUTPUT:                                                        M29.4688
C                                                                       M29.4689
C        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.4690
C          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS                M29.4691
C          IN THEIR FIRST M COLUMNS.                                    M29.4692
C                                                                       M29.4693
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,        M29.4694
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY         M29.4695
C                                                                       M29.4696
C     ------------------------------------------------------------------M29.4697
C                                                                       M29.4698
      IF (M .EQ. 0) GO TO 200                                           M29.4699
      IF (IGH .EQ. LOW) GO TO 120                                       M29.4700
      DO 110 I = LOW, IGH                                               M29.4701
         S = SCALE(I)                                                   M29.4702
C     :::::::::: LEFT HAND EIGENVECTORS ARE BACK TRANSFORMED            M29.4703
C                IF THE FOREGOING STATEMENT IS REPLACED BY              M29.4704
C                S=1.0/SCALE(I). ::::::::::                             M29.4705
         DO 100 J = 1, M                                                M29.4706
            ZR(I,J) = ZR(I,J) * S                                       M29.4707
            ZI(I,J) = ZI(I,J) * S                                       M29.4708
  100    CONTINUE                                                       M29.4709
  110 CONTINUE                                                          M29.4710
C     :::::::::: FOR I=LOW-1 STEP -1 UNTIL 1,                           M29.4711
C                IGH+1 STEP 1 UNTIL N DO -- ::::::::::                  M29.4712
  120 DO 140 II = 1, N                                                  M29.4713
         I = II                                                         M29.4714
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 140                     M29.4715
         IF (I .LT. LOW) I = LOW - II                                   M29.4716
         K = SCALE(I)                                                   M29.4717
         IF (K .EQ. I) GO TO 140                                        M29.4718
         DO 130 J = 1, M                                                M29.4719
            S = ZR(I,J)                                                 M29.4720
            ZR(I,J) = ZR(K,J)                                           M29.4721
            ZR(K,J) = S                                                 M29.4722
            S = ZI(I,J)                                                 M29.4723
            ZI(I,J) = ZI(K,J)                                           M29.4724
            ZI(K,J) = S                                                 M29.4725
  130    CONTINUE                                                       M29.4726
  140 CONTINUE                                                          M29.4727
  200 RETURN                                                            M29.4728
      END                                                               M29.4729
      SUBROUTINE COMHES(NM,N,LOW,IGH,AR,AI,INT,LUNIT6,IPRSUP,NDIM       M31.6618
     1,IORD)                                                            M44.2305
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION AR(NDIM,NDIM), AI(NDIM,NDIM), INT(20), IORD(20)         M44.2306
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE COMHES,   M29.4734
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.             M29.4735
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   M29.4736
C                                                                       M29.4737
C     GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE                   M29.4738
C     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS                  M29.4739
C     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY                       M29.4740
C     STABILIZED ELEMENTARY SIMILARITY TRANSFORMATIONS.                 M29.4741
C                                                                       M29.4742
C     ON INPUT:                                                         M29.4743
C                                                                       M29.4744
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         M29.4745
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          M29.4746
C          DIMENSION STATEMENT;                                         M29.4747
C                                                                       M29.4748
C        N IS THE ORDER OF THE MATRIX;                                  M29.4749
C                                                                       M29.4750
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING           M29.4751
C          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED,              M29.4752
C          SET LOW=1, IGH=N;                                            M29.4753
C                                                                       M29.4754
C        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.4755
C          RESPECTIVELY, OF THE COMPLEX INPUT MATRIX.                   M29.4756
C                                                                       M29.4757
C     ON OUTPUT:                                                        M29.4758
C                                                                       M29.4759
C        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.4760
C          RESPECTIVELY, OF THE HESSENBERG MATRIX.  THE                 M29.4761
C          MULTIPLIERS WHICH WERE USED IN THE REDUCTION                 M29.4762
C          ARE STORED IN THE REMAINING TRIANGLES UNDER THE              M29.4763
C          HESSENBERG MATRIX;                                           M29.4764
C                                                                       M29.4765
C        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS               M29.4766
C          INTERCHANGED IN THE REDUCTION.                               M29.4767
C          ONLY ELEMENTS LOW THROUGH IGH ARE USED.                      M29.4768
C                                                                       M29.4769
C     ARITHMETIC IS REAL EXCEPT FOR THE REPLACEMENT OF THE ALGOL        M29.4770
C     PROCEDURE CDIV BY COMPLEX DIVISION USING SUBROUTINE CMPLXZ.       M29.4771
C                                                                       M29.4772
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,        M29.4773
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY         M29.4774
C                                                                       M29.4775
C     ------------------------------------------------------------------M29.4776
C                                                                       M29.4777
CCCC      FLZERO = 1.D-12       !REPLACED BY EPSCMH }                   M44.2307
      EPSCMH = 1.E-15                                                   M44.2308
      DO 121 I=1, 20                                                    M44.2309
      IORD(I) = I                                                       M44.2310
  121 CONTINUE                                                          M44.2311
      LA = IGH - 1                                                      M29.4778
      KP1 = LOW + 1                                                     M29.4779
      IF (IPRSUP .GE. 3 )                                               M29.4780
     1  WRITE (LUNIT6, 20 )  IGH, LOW, LA, KP1                          M29.4781
   20 FORMAT ( 47H AT THE TOP OF COMHES, IGH, LOW, LA AND KP1 ARE, 4I10)M29.4782
      D13=EPSCMH                                                        M44.2312
      IF (LA .LT. KP1) GO TO 200                                        M29.4783
      DO 180 M = KP1, LA                                                M29.4784
         MM1 = M - 1                                                    M29.4785
         XR = 0.0                                                       M29.4786
         XI = 0.0                                                       M29.4787
         I = M                                                          M29.4788
         DO 100 J = M, IGH                                              M29.4789
CCCC            IF (ABSZ(AR(J,MM1)) + ABSZ(AI(J,MM1))                   M29.4790
CCCC     1         .LE. ABSZ(XR) + ABSZ(XI)) GO TO 100                  M29.4791
            IF ((AR(J,MM1)**2) + (AI(J,MM1)**2)                         M44.2313
     1         .LE. (XR*XR) + (XI*XI)) GO TO 100                        M44.2314
            XR = AR(J,MM1)                                              M29.4792
            XI = AI(J,MM1)                                              M29.4793
            I = J                                                       M29.4794
  100    CONTINUE                                                       M29.4795
         INT(M) = I                                                     M29.4796
         IF (I .EQ. M) GO TO 130                                        M29.4797
C     :::::::::: INTERCHANGE ROWS AND COLUMNS OF AR AND AI ::::::::::   M29.4798
      IT=IORD(I)                                                        M44.2315
      IORD(I) = IORD(M)                                                 M44.2316
      IORD(M) = IT                                                      M44.2317
         DO 110 J = MM1, N                                              M29.4799
            YR = AR(I,J)                                                M29.4800
            AR(I,J) = AR(M,J)                                           M29.4801
            AR(M,J) = YR                                                M29.4802
            YI = AI(I,J)                                                M29.4803
            AI(I,J) = AI(M,J)                                           M29.4804
            AI(M,J) = YI                                                M29.4805
  110    CONTINUE                                                       M29.4806
         DO 120 J = 1, IGH                                              M29.4807
            YR = AR(J,I)                                                M29.4808
            AR(J,I) = AR(J,M)                                           M29.4809
            AR(J,M) = YR                                                M29.4810
            YI = AI(J,I)                                                M29.4811
            AI(J,I) = AI(J,M)                                           M29.4812
            AI(J,M) = YI                                                M29.4813
  120    CONTINUE                                                       M29.4814
C     :::::::::: END INTERCHANGE ::::::::::                             M29.4815
C  130    IF (XR .LT. D13 .AND. XI .LT. D13)  GO TO 180                 M44.2318
CC  130    IF (ABSZ(XR) .LT. D13 .AND. ABSZ(XI) .LT. D13)  GO TO 180    M44.2319
  130    IF (XR .EQ. 0.0 .AND. XI .EQ. 0.0)  GO TO 180                  M44.2320
         MP1 = M + 1                                                    M29.4817
          DO 160 I = MP1, IGH                                           M29.4818
            YR = AR(I,MM1)                                              M29.4819
            YI = AI(I,MM1)                                              M29.4820
C      IF (YR .LT. D13 .AND. YI .LT. D13) GO TO 160                     M44.2321
CC    IF (ABSZ(YR) .LT. D13 .AND. ABSZ(YI) .LT. D13) GO TO 160          M44.2322
      IF (YR .EQ. 0.0 .AND. YI .EQ. 0.0) GO TO 160                      M44.2323
      D1 = XR * XR + XI * XI                                            M31.6620
      D2 = (XR*YR + YI*XI) / D1                                         M31.6621
      D3 = (XR*YI - XI*YR) / D1                                         M31.6622
      YR = D2                                                           M31.6623
      YI = D3                                                           M31.6624
            AR(I,MM1) = YR                                              M29.4825
            AI(I,MM1) = YI                                              M29.4826
             DO 140 J = M, N                                            M29.4827
               AR(I,J) = AR(I,J) - YR * AR(M,J) + YI * AI(M,J)          M29.4828
               AI(I,J) = AI(I,J) - YR * AI(M,J) - YI * AR(M,J)          M29.4829
  140       CONTINUE                                                    M29.4830
            DO 150 J = 1, IGH                                           M29.4831
               AR(J,M) = AR(J,M) + YR * AR(J,I) - YI * AI(J,I)          M29.4832
               AI(J,M) = AI(J,M) + YR * AI(J,I) + YI * AR(J,I)          M29.4833
  150       CONTINUE                                                    M29.4834
  160    CONTINUE                                                       M29.4835
      IF ( IPRSUP .GE. 3 )                                              M29.4836
     1 WRITE (LUNIT6, 170) M, IGH, INT(M),(AR(J,M),AI(J,M),J=1,IGH)     M29.4837
  170 FORMAT ( 29H M, IGH AND INT(M) AT 170 ARE, 3I10,/,                M29.4838
     1  31H (AR(J,M),AI(J,M),J=1,IGH), ARE,/, (1X,8E15.6) )             M29.4839
  180 CONTINUE                                                          M29.4840
  200 RETURN                                                            M29.4841
      END                                                               M29.4842
      SUBROUTINE COMLR(NM,N,LOW,IGH,HR,HI,WR,WI,IERR,NDIM)              M31.6625
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION HR(NDIM,NDIM),HI(NDIM,NDIM),WR(NDIM),WI(NDIM)           M31.6626
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE COMLR,    M29.4848
C     NUM. MATH. 12, 369-376(1968) BY MARTIN AND WILKINSON.             M29.4849
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971).   M29.4850
C                                                                       M29.4851
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A COMPLEX                M29.4852
C     UPPER HESSENBERG MATRIX BY THE MODIFIED LR METHOD.                M29.4853
C                                                                       M29.4854
C     ON INPUT:                                                         M29.4855
C                                                                       M29.4856
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         M29.4857
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          M29.4858
C          DIMENSION STATEMENT;                                         M29.4859
C                                                                       M29.4860
C        N IS THE ORDER OF THE MATRIX;                                  M29.4861
C                                                                       M29.4862
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING           M29.4863
C          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED,              M29.4864
C          SET LOW=1, IGH=N;                                            M29.4865
C                                                                       M29.4866
C        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.4867
C          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.        M29.4868
C          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN THE      M29.4869
C          MULTIPLIERS WHICH WERE USED IN THE REDUCTION BY  COMHES,     M29.4870
C          IF PERFORMED.                                                M29.4871
C                                                                       M29.4872
C     ON OUTPUT:                                                        M29.4873
C                                                                       M29.4874
C        THE UPPER HESSENBERG PORTIONS OF HR AND HI HAVE BEEN           M29.4875
C          DESTROYED.  THEREFORE, THEY MUST BE SAVED BEFORE             M29.4876
C          CALLING  COMLR  IF SUBSEQUENT CALCULATION OF                 M29.4877
C          EIGENVECTORS IS TO BE PERFORMED;                             M29.4878
C                                                                       M29.4879
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.4880
C          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR               M29.4881
C          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT              M29.4882
C          FOR INDICES IERR+1,...,N;                                    M29.4883
C                                                                       M29.4884
C        IERR IS SET TO                                                 M29.4885
C          ZERO       FOR NORMAL RETURN,                                M29.4886
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN               M29.4887
C                     DETERMINED AFTER 30 ITERATIONS.                   M29.4888
C                                                                       M29.4889
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,        M29.4894
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY         M29.4895
C                                                                       M29.4896
C     ------------------------------------------------------------------M29.4897
C                                                                       M29.4898
C     :::::::::: EPMACH IS A MACHINE DEPENDENT PARAMETER SPECIFYING     M29.4899
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.   M29.4900
C                EPMACH = 16.0D0**(-13) FOR LONG FORM ARITHMETIC        M29.4901
C                ON S360 ::::::::::                                     M29.4902
C     DATA EPMACH/Z3410000000000000/                                    M29.4903
C     EPMACH WILL BE SET TO EQUAL TO THE SYSTEM DEPENDENT FLOATING      M29.4904
C     POINT ZERO VARIABLE  "FLZERO"                                     M29.4905
      EPMACH = FLZERO                                                   M29.4906
      IERR = 0                                                          M29.4907
C     :::::::::: STORE ROOTS ISOLATED BY CBAL ::::::::::                M29.4908
      DO 200 I = 1, N                                                   M29.4909
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 200                     M29.4910
         WR(I) = HR(I,I)                                                M29.4911
         WI(I) = HI(I,I)                                                M29.4912
  200 CONTINUE                                                          M29.4913
      IEN = IGH                                                         M29.4914
      TR = 0.0                                                          M29.4915
      TI = 0.0                                                          M29.4916
C     :::::::::: SEARCH FOR NEXT EIGENVALUE ::::::::::                  M29.4917
  220 IF (IEN .LT. LOW) GO TO 1001                                      M29.4918
      ITS = 0                                                           M29.4919
      IENM1 = IEN - 1                                                   M29.4920
C     :::::::::: LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT             M29.4921
C                FOR L=IEN STEP -1 UNTIL LOW D0 -- ::::::::::           M29.4922
  240 DO 260 LL = LOW, IEN                                              M29.4923
         L = IEN + LOW - LL                                             M29.4924
         IF (L .EQ. LOW) GO TO 300                                      M29.4925
         IF (ABSZ(HR(L,L-1)) + ABSZ(HI(L,L-1)) .LE.                     M29.4926
     1      EPMACH * (ABSZ(HR(L-1,L-1)) + ABSZ(HI(L-1,L-1))             M29.4927
     1             + ABSZ(HR(L,L)) + ABSZ(HI(L,L)))) GO TO 300          M29.4928
  260 CONTINUE                                                          M29.4929
C     :::::::::: FORM SHIFT ::::::::::                                  M29.4930
  300 IF (L .EQ. IEN) GO TO 660                                         M29.4931
      IF (ITS .EQ. 30) GO TO 1000                                       M29.4932
      IF (ITS .EQ. 10 .OR. ITS .EQ. 20) GO TO 320                       M29.4933
      SR = HR(IEN,IEN)                                                  M29.4934
      SI = HI(IEN,IEN)                                                  M29.4935
      XR = HR(IENM1,IEN) * HR(IEN,IENM1) - HI(IENM1,IEN) * HI(IEN,IENM1)M29.4936
      XI = HR(IENM1,IEN) * HI(IEN,IENM1) + HI(IENM1,IEN) * HR(IEN,IENM1)M29.4937
      IF (XR .EQ. 0.0 .AND. XI .EQ. 0.0) GO TO 340                      M29.4938
      YR = (HR(IENM1,IENM1) - SR) * ONEHAF                              M29.4939
      YI = (HI(IENM1,IENM1) - SI) * ONEHAF                              M29.4940
      D1 = YR**2 - YI**2 + XR                                           M31.6627
      D2 = 2.0 * YR * YI + XI                                           M31.6628
      D3 = SQRTZ( D1 ** 2 + D2 ** 2 )                                   M31.6629
      D3 = SQRTZ( D3 )                                                  M31.6630
      D4 = ONEHAF * ATAN2Z(D2,D1)                                       M31.6631
      ZZR = D3 * COSZ (D4)                                              M31.6632
      ZZI = D3 * SINZ (D4)                                              M31.6633
      IF (YR * ZZR + YI * ZZI .GE. 0.0) GO TO 310                       M29.4944
      ZZR = -ZZR                                                        M29.4945
      ZZI = -ZZI                                                        M29.4946
  310 D1 = (YR+ZZR) **2 + (YI+ZZI) **2                                  M31.6634
      D2 = ( XR*(YR+ZZR) + XI*(YI+ZZI) ) / D1                           M31.6635
      D3 = ( XI*(YR+ZZR) - XR*(YI+ZZI) ) / D1                           M31.6636
      SR = SR - D2                                                      M31.6637
      SI = SI - D3                                                      M31.6638
      GO TO 340                                                         M29.4950
C     :::::::::: FORM EXCEPTIONAL SHIFT ::::::::::                      M29.4951
  320 SR = ABSZ(HR(IEN,IENM1)) + ABSZ(HR(IENM1,IEN-2))                  M29.4952
      SI = ABSZ(HI(IEN,IENM1)) + ABSZ(HI(IENM1,IEN-2))                  M29.4953
  340 DO 360 I = LOW, IEN                                               M29.4954
         HR(I,I) = HR(I,I) - SR                                         M29.4955
         HI(I,I) = HI(I,I) - SI                                         M29.4956
  360 CONTINUE                                                          M29.4957
      TR = TR + SR                                                      M29.4958
      TI = TI + SI                                                      M29.4959
      ITS = ITS + 1                                                     M29.4960
C     :::::::::: LOOK FOR TWO CONSECUTIVE SMALL                         M29.4961
C                SUB-DIAGONAL ELEMENTS ::::::::::                       M29.4962
      XR = ABSZ(HR(IENM1,IENM1)) + ABSZ(HI(IENM1,IENM1))                M29.4963
      YR = ABSZ(HR(IEN,IENM1)) + ABSZ(HI(IEN,IENM1))                    M29.4964
      ZZR = ABSZ(HR(IEN,IEN)) + ABSZ(HI(IEN,IEN))                       M29.4965
C     :::::::::: FOR M=IEN-1 STEP -1 UNTIL L DO -- ::::::::::           M29.4966
      DO 380 MM = L, IENM1                                              M29.4967
         M = IENM1 + L - MM                                             M29.4968
         IF (M .EQ. L) GO TO 420                                        M29.4969
         YI = YR                                                        M29.4970
         YR = ABSZ(HR(M,M-1)) + ABSZ(HI(M,M-1))                         M29.4971
         XI = ZZR                                                       M29.4972
         ZZR = XR                                                       M29.4973
         XR = ABSZ(HR(M-1,M-1)) + ABSZ(HI(M-1,M-1))                     M29.4974
         IF (YR .LE. EPMACH * ZZR / YI * (ZZR + XR + XI)) GO TO 420     M29.4975
  380 CONTINUE                                                          M29.4976
C     :::::::::: TRIANGULAR DECOMPOSITION H=L*R ::::::::::              M29.4977
  420 MP1 = M + 1                                                       M29.4978
      DO 520 I = MP1, IEN                                               M29.4979
         IM1 = I - 1                                                    M29.4980
         XR = HR(IM1,IM1)                                               M29.4981
         XI = HI(IM1,IM1)                                               M29.4982
         YR = HR(I,IM1)                                                 M29.4983
         YI = HI(I,IM1)                                                 M29.4984
         IF (ABSZ(XR) + ABSZ(XI) .GE. ABSZ(YR) + ABSZ(YI)) GO TO 460    M29.4985
C     :::::::::: INTERCHANGE ROWS OF HR AND HI ::::::::::               M29.4986
         DO 440 J = IM1, IEN                                            M29.4987
            ZZR = HR(IM1,J)                                             M29.4988
            HR(IM1,J) = HR(I,J)                                         M29.4989
            HR(I,J) = ZZR                                               M29.4990
            ZZI = HI(IM1,J)                                             M29.4991
            HI(IM1,J) = HI(I,J)                                         M29.4992
            HI(I,J) = ZZI                                               M29.4993
  440    CONTINUE                                                       M29.4994
      D1 = YR ** 2 + YI ** 2                                            M31.6639
      D2 = ( XR*YR + XI*YI ) / D1                                       M31.6640
      D3 = ( YR*XI - XR*YI ) / D1                                       M31.6641
         WR(I) = 1.0                                                    M29.4996
         GO TO 480                                                      M29.4997
  460 D1 = XR ** 2 + XI ** 2                                            M31.6642
      D2 = ( YR*XR + YI*XI ) / D1                                       M31.6643
      D3 = ( XR*YI - YR*XI ) / D1                                       M31.6644
         WR(I) = -1.0                                                   M29.4999
  480    ZZR = D2                                                       M31.6645
         ZZI = D3                                                       M31.6646
         HR(I,IM1) = ZZR                                                M29.5002
         HI(I,IM1) = ZZI                                                M29.5003
         DO 500 J = I, IEN                                              M29.5004
            HR(I,J) = HR(I,J) - ZZR * HR(IM1,J) + ZZI * HI(IM1,J)       M29.5005
            HI(I,J) = HI(I,J) - ZZR * HI(IM1,J) - ZZI * HR(IM1,J)       M29.5006
  500    CONTINUE                                                       M29.5007
  520 CONTINUE                                                          M29.5008
C     :::::::::: COMPOSITION R*L=H ::::::::::                           M29.5009
      DO 640 J = MP1, IEN                                               M29.5010
         XR = HR(J,J-1)                                                 M29.5011
         XI = HI(J,J-1)                                                 M29.5012
         HR(J,J-1) = 0.0                                                M29.5013
         HI(J,J-1) = 0.0                                                M29.5014
C     :::::::::: INTERCHANGE COLUMNS OF HR AND HI,                      M29.5015
C                IF NECESSARY ::::::::::                                M29.5016
         IF (WR(J) .LE. 0.0) GO TO 580                                  M29.5017
         DO 540 I = L, J                                                M29.5018
            ZZR = HR(I,J-1)                                             M29.5019
            HR(I,J-1) = HR(I,J)                                         M29.5020
            HR(I,J) = ZZR                                               M29.5021
            ZZI = HI(I,J-1)                                             M29.5022
            HI(I,J-1) = HI(I,J)                                         M29.5023
            HI(I,J) = ZZI                                               M29.5024
  540    CONTINUE                                                       M29.5025
  580    DO 600 I = L, J                                                M29.5026
            HR(I,J-1) = HR(I,J-1) + XR * HR(I,J) - XI * HI(I,J)         M29.5027
            HI(I,J-1) = HI(I,J-1) + XR * HI(I,J) + XI * HR(I,J)         M29.5028
  600    CONTINUE                                                       M29.5029
  640 CONTINUE                                                          M29.5030
      GO TO 240                                                         M29.5031
C     :::::::::: A ROOT FOUND ::::::::::                                M29.5032
  660 WR(IEN) = HR(IEN,IEN) + TR                                        M29.5033
      WI(IEN) = HI(IEN,IEN) + TI                                        M29.5034
      IEN = IENM1                                                       M29.5035
      GO TO 220                                                         M29.5036
C     :::::::::: SET ERROR -- NO CONVERGENCE TO AN                      M29.5037
C                EIGENVALUE AFTER 30 ITERATIONS ::::::::::              M29.5038
 1000 IERR = IEN                                                        M29.5039
 1001 RETURN                                                            M29.5040
C     :::::::::: LAST CARD OF COMLR ::::::::::                          M29.5041
      END                                                               M29.5042
      SUBROUTINE COMLR2(NM,N,LOW,IGH,INT,HR,HI,ZI,ZR,WR,WI,IERR,NDIM    M31.6647
     1,IORD)                                                            M43.3948
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION HR(NDIM,NDIM),HI(NDIM,NDIM),WR(NDIM),WI(NDIM)           M31.6648
      DIMENSION ZR(NDIM,NDIM),ZI(NDIM,NDIM)                             M31.6649
      DIMENSION INT(20),IORD(20),LSEQ(20)                               M31.6650
      DIMENSION UMR(20), UMI(20)                                        M43.3949
      DIMENSION EIM(20),VTR(20),VTI(20),PPR(20,20),PPI(20,20)           M43.3950
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE COMLR2,   M29.5049
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.             M29.5050
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   M29.5051
C                                                                       M29.5052
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS            M29.5053
C     OF A COMPLEX UPPER HESSENBERG MATRIX BY THE MODIFIED LR           M29.5054
C     METHOD.  THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX             M29.5055
C     CAN ALSO BE FOUND IF  COMHES  HAS BEEN USED TO REDUCE             M29.5056
C     THIS GENERAL MATRIX TO HESSENBERG FORM.                           M29.5057
C                                                                       M29.5058
C     ON INPUT:                                                         M29.5059
C                                                                       M29.5060
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL         M29.5061
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM          M29.5062
C          DIMENSION STATEMENT;                                         M29.5063
C                                                                       M29.5064
C        N IS THE ORDER OF THE MATRIX;                                  M29.5065
C                                                                       M29.5066
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING           M29.5067
C          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED,              M29.5068
C          SET LOW=1, IGH=N;                                            M29.5069
C                                                                       M29.5070
C        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS INTERCHANGED  M29.5071
C          IN THE REDUCTION BY  COMHES, IF PERFORMED.  ONLY ELEMENTS    M29.5072
C          LOW THROUGH IGH ARE USED.  IF THE EIGENVECTORS OF THE HESSEN-M29.5073
C          BERG MATRIX ARE DESIRED, SET INT(J)=J FOR THESE ELEMENTS;    M29.5074
C                                                                       M29.5075
C        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.5076
C          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.        M29.5077
C          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN THE      M29.5078
C          MULTIPLIERS WHICH WERE USED IN THE REDUCTION BY  COMHES,     M29.5079
C          IF PERFORMED.  IF THE EIGENVECTORS OF THE HESSENBERG         M29.5080
C          MATRIX ARE DESIRED, THESE ELEMENTS MUST BE SET TO ZERO.      M29.5081
C                                                                       M29.5082
C     ON OUTPUT:                                                        M29.5083
C                                                                       M29.5084
C        THE UPPER HESSENBERG PORTIONS OF HR AND HI HAVE BEEN           M29.5085
C          DESTROYED, BUT THE LOCATION HR(1,1) CONTAINS THE FNORM       M29.5086
C          OF THE TRIANGULARIZED MATRIX;                                M29.5087
C                                                                       M29.5088
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.5089
C          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR               M29.5090
C          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT              M29.5091
C          FOR INDICES IERR+1,...,N;                                    M29.5092
C                                                                       M29.5093
C        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,                M29.5094
C          RESPECTIVELY, OF THE EIGENVECTORS.  THE EIGENVECTORS         M29.5095
C          ARE UNNORMALIZED.  IF AN ERROR EXIT IS MADE, NONE OF         M29.5096
C          THE EIGENVECTORS HAS BEEN FOUND;                             M29.5097
C                                                                       M29.5098
C        IERR IS SET TO                                                 M29.5099
C          ZERO       FOR NORMAL RETURN,                                M29.5100
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN               M29.5101
C                     DETERMINED AFTER 30 ITERATIONS.                   M29.5102
C                                                                       M29.5103
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,        M29.5108
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY         M29.5109
C                                                                       M29.5110
C     ------------------------------------------------------------------M29.5111
C                                                                       M29.5112
C     :::::::::: EPMACH IS A MACHINE DEPENDENT PARAMETER SPECIFYING     M29.5113
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.   M29.5114
C                EPMACH = 16.0D0**(-13) FOR LONG FORM ARITHMETIC        M29.5115
C                ON S360 ::::::::::                                     M29.5116
C     DATA EPMACH/Z3410000000000000/                                    M29.5117
C     LIKE IN SUBROUTINE COMLR, EPMACH IS SET TO BE FLZERO              M29.5118
CCCC      EPMACH = 1.0E-50      ! NOW IN CHOICE.DAT }                   M43.3951
      EPMACH = 1.E-32                                                   M43.3952
      ITSMAX = 300                                                      M43.3953
      IERR = 0                                                          M29.5120
C     :::::::::: INITIALIZE EIGENVECTOR MATRIX ::::::::::               M29.5121
      DO 100 I = 1, N                                                   M29.5122
         DO 100 J = 1, N                                                M29.5123
            ZR(I,J) = 0.0                                               M29.5124
            ZI(I,J) = 0.0                                               M29.5125
            IF (I .EQ. J) ZR(I,J) = 1.0                                 M29.5126
  100 CONTINUE                                                          M29.5127
C     :::::::::: FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS         M29.5128
C                FROM THE INFORMATION LEFT BY COMHES ::::::::::         M29.5129
      IEND = IGH - LOW - 1                                              M29.5130
      IF (IEND .LE. 0) GO TO 180                                        M29.5131
C     :::::::::: FOR I=IGH-1 STEP -1 UNTIL LOW+1 DO -- ::::::::::       M29.5132
      DO 160 II = 1, IEND                                               M29.5133
         I = IGH - II                                                   M29.5134
         IP1 = I + 1                                                    M29.5135
         DO 120 K = IP1, IGH                                            M29.5136
            ZR(K,I) = HR(K,I-1)                                         M29.5137
            ZI(K,I) = HI(K,I-1)                                         M29.5138
  120    CONTINUE                                                       M29.5139
         J = INT(I)                                                     M29.5140
         IF (I .EQ. J) GO TO 160                                        M29.5141
         DO 140 K = I, IGH                                              M29.5142
            ZR(I,K) = ZR(J,K)                                           M29.5143
            ZI(I,K) = ZI(J,K)                                           M29.5144
            ZR(J,K) = 0.0                                               M29.5145
            ZI(J,K) = 0.0                                               M29.5146
  140    CONTINUE                                                       M29.5147
         ZR(J,I) = 1.0                                                  M29.5148
  160 CONTINUE                                                          M29.5149
      IF ( IPRSUP .GE. 3 )                                              M29.5150
     1 WRITE (LUNIT6, 170) ((ZR(I,J),ZI(I,J),J=1,IGH),I=1,IGH)          M29.5151
  170 FORMAT ( 69H AFTER DO 160 LOOP IN COMLR2, ((ZR(I,J),ZI(I,J),J=1,IGM29.5152
     1H),I=1,IGH) ARE,/, (1X,8E15.6) )                                  M29.5153
C     :::::::::: STORE ROOTS ISOLATED BY CBAL ::::::::::                M29.5154
  180 DO 200 I = 1, N                                                   M29.5155
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 200                     M29.5156
         WR(I) = HR(I,I)                                                M29.5157
         WI(I) = HI(I,I)                                                M29.5158
  200 CONTINUE                                                          M29.5159
      IEN = IGH                                                         M29.5160
      TR = 0.0                                                          M29.5161
      TI = 0.0                                                          M29.5162
C     :::::::::: SEARCH FOR NEXT EIGENVALUE ::::::::::                  M29.5163
  220 IF (IEN .LT. LOW) GO TO 680                                       M29.5164
      ITS = 0                                                           M29.5165
      IENM1 = IEN - 1                                                   M29.5166
C     :::::::::: LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT             M29.5167
C                FOR L=IEN STEP -1 UNTIL LOW DO -- ::::::::::           M29.5168
  240 DO 260 LL = LOW, IEN                                              M29.5169
         L = IEN + LOW - LL                                             M29.5170
         IF (L .EQ. LOW) GO TO 300                                      M29.5171
         IF (ABSZ(HR(L,L-1)) + ABSZ(HI(L,L-1)) .LE.                     M29.5172
     1      EPMACH * (ABSZ(HR(L-1,L-1)) + ABSZ(HI(L-1,L-1))             M29.5173
     1             + ABSZ(HR(L,L)) + ABSZ(HI(L,L)))) GO TO 300          M29.5174
  260 CONTINUE                                                          M29.5175
C     :::::::::: FORM SHIFT ::::::::::                                  M29.5176
  300 IF (L .EQ. IEN) GO TO 660                                         M29.5177
      IF (ITS .EQ. ITSMAX ) GO TO 1000                                  M43.3954
CCCC      IF ((ITS/10)*10 .EQ. ITS ) GO TO 320                          M43.3955
CCCCC     IF (ITS .EQ. 10 .OR. ITS .EQ. 20) GO TO 320                   M29.5179
      SR = HR(IEN,IEN)                                                  M29.5180
      SI = HI(IEN,IEN)                                                  M29.5181
      XR = HR(IENM1,IEN) * HR(IEN,IENM1) - HI(IENM1,IEN) * HI(IEN,IENM1)M29.5182
      XI = HR(IENM1,IEN) * HI(IEN,IENM1) + HI(IENM1,IEN) * HR(IEN,IENM1)M29.5183
      IF ( IPRSUP .GE. 3)                                               M29.5184
     1 WRITE (LUNIT6, 305) SR, SI, XR, XI                               M29.5185
  305 FORMAT ( 29H SR, SI, XR AND XI AT 305 ARE, 4E15.6 )               M29.5186
      IF (XR .EQ. 0.0 .AND. XI .EQ. 0.0) GO TO 340                      M29.5187
      YR = (HR(IENM1,IENM1) - SR) * ONEHAF                              M29.5188
      YI = (HI(IENM1,IENM1) - SI) * ONEHAF                              M29.5189
      D1 = YR**2 - YI**2 + XR                                           M31.6651
      D2 = 2.0 * YR * YI + XI                                           M31.6652
      D3 = SQRTZ( D1 ** 2 + D2 ** 2 )                                   M31.6653
      D3 = SQRTZ( D3 )                                                  M31.6654
      D4 = ONEHAF * ATAN2Z(D2,D1)                                       M31.6655
      ZZR = D3 * COSZ (D4)                                              M31.6656
      ZZI = D3 * SINZ (D4)                                              M31.6657
      IF (YR * ZZR + YI * ZZI .GE. 0.0) GO TO 310                       M29.5193
      ZZR = -ZZR                                                        M29.5194
      ZZI = -ZZI                                                        M29.5195
  310 D1 = (YR+ZZR) **2 + (YI+ZZI) **2                                  M31.6658
      D2 = ( XR*(YR+ZZR) + XI*(YI+ZZI) ) / D1                           M31.6659
      D3 = ( XI*(YR+ZZR) - XR*(YI+ZZI) ) / D1                           M31.6660
      IF ( IPRSUP .GE. 3 )                                              M29.5197
     1 WRITE (LUNIT6, 315) YR, YI, ZZR, ZZI, D2,D3                      M31.6661
  315 FORMAT ( 38H YR, YI, ZZR, ZZI AND D2,D3 AT 315 ARE, 6E15.6)       M31.6662
      SR = SR - D2                                                      M31.6663
      SI = SI - D3                                                      M31.6664
      GO TO 340                                                         M29.5202
C     :::::::::: FORM EXCEPTIONAL SHIFT ::::::::::                      M29.5203
CCCC  320 SR = ABSZ(HR(IEN,IENM1)) + ABSZ(HR(IENM1,IEN-2))              M29.5204
CCCC      SI = ABSZ(HI(IEN,IENM1)) + ABSZ(HI(IENM1,IEN-2))              M29.5205
  340 DO 360 I = LOW, IEN                                               M29.5206
         HR(I,I) = HR(I,I) - SR                                         M29.5207
         HI(I,I) = HI(I,I) - SI                                         M29.5208
      IF ( IPRSUP .GE. 3 )                                              M29.5209
     1 WRITE (LUNIT6, 350) I, SR, SI, HR(I,I),HI(I,I)                   M29.5210
  350 FORMAT ( 43H I, SR, SI, HR(I,I), AND HI(I,I) AT 350 ARE,/,        M29.5211
     1        10X, I8, 4E15.6)                                          M29.5212
  360 CONTINUE                                                          M29.5213
      TR = TR + SR                                                      M29.5214
      TI = TI + SI                                                      M29.5215
      ITS = ITS + 1                                                     M29.5216
C     :::::::::: LOOK FOR TWO CONSECUTIVE SMALL                         M29.5217
C                SUB-DIAGONAL ELEMENTS ::::::::::                       M29.5218
      XR = ABSZ(HR(IENM1,IENM1)) + ABSZ(HI(IENM1,IENM1))                M29.5219
      YR = ABSZ(HR(IEN,IENM1)) + ABSZ(HI(IEN,IENM1))                    M29.5220
      ZZR = ABSZ(HR(IEN,IEN)) + ABSZ(HI(IEN,IEN))                       M29.5221
      IF ( IPRSUP .GE. 3 )                                              M29.5222
     1 WRITE (LUNIT6, 370) XR, YR, ZZR                                  M29.5223
  370 FORMAT ( 27H XR, YR, AND ZZR AT 370 ARE, 3E15.6)                  M29.5224
C     :::::::::: FOR M=IEN-1 STEP -1 UNTIL L DO -- ::::::::::           M29.5225
      DO 380 MM = L, IENM1                                              M29.5226
         M = IENM1 + L - MM                                             M29.5227
         IF (M .EQ. L) GO TO 420                                        M29.5228
         YI = YR                                                        M29.5229
         YR = ABSZ(HR(M,M-1)) + ABSZ(HI(M,M-1))                         M29.5230
         XI = ZZR                                                       M29.5231
         ZZR = XR                                                       M29.5232
         XR = ABSZ(HR(M-1,M-1)) + ABSZ(HI(M-1,M-1))                     M29.5233
      IF ( IPRSUP .GE. 3 )                                              M31.6665
     1 WRITE (LUNIT6, 375) MM, YR, ZZR, YI, XR, XI, EPMACH              M29.5235
  375 FORMAT ( 47H AT 375, MM, YR, ZZR, YI, XR, XI AND EPMACH ARE,/,    M29.5236
     1         10X, I8, 6E15.6)                                         M29.5237
         IF (YR .LE. EPMACH * ZZR / YI * (ZZR + XR + XI)) GO TO 420     M29.5238
  380 CONTINUE                                                          M29.5239
C     :::::::::: TRIANGULAR DECOMPOSITION H=L*R ::::::::::              M29.5240
  420 MP1 = M + 1                                                       M29.5241
      DO 520 I = MP1, IEN                                               M29.5242
         IM1 = I - 1                                                    M29.5243
         XR = HR(IM1,IM1)                                               M29.5244
         XI = HI(IM1,IM1)                                               M29.5245
         YR = HR(I,IM1)                                                 M29.5246
         YI = HI(I,IM1)                                                 M29.5247
         IF (ABSZ(XR) + ABSZ(XI) .GE. ABSZ(YR) + ABSZ(YI)) GO TO 460    M29.5248
C     :::::::::: INTERCHANGE ROWS OF HR AND HI ::::::::::               M29.5249
         DO 440 J = IM1, N                                              M29.5250
            ZZR = HR(IM1,J)                                             M29.5251
            HR(IM1,J) = HR(I,J)                                         M29.5252
            HR(I,J) = ZZR                                               M29.5253
            ZZI = HI(IM1,J)                                             M29.5254
            HI(IM1,J) = HI(I,J)                                         M29.5255
            HI(I,J) = ZZI                                               M29.5256
  440    CONTINUE                                                       M29.5257
C     IT=IORD(I)                                                        M43.3956
      ITTEMP = IORD(I)
      IORD(I) = IORD(IM1)                                               M43.3957
C     IORD(IM1) = IT                                                    M43.3958
      IORD(IM1) = ITTEMP
  441 CONTINUE                                                          M43.3959
      D1 = YR ** 2 + YI ** 2                                            M31.6666
      D2 = ( XR*YR + XI*YI ) / D1                                       M31.6667
      D3 = ( YR*XI - XR*YI ) / D1                                       M31.6668
         WR(I) = 1.0                                                    M29.5259
         GO TO 480                                                      M29.5260
  460 D1 = XR ** 2 + XI ** 2                                            M31.6669
      D2 = ( YR*XR + YI*XI ) / D1                                       M31.6670
      D3 = ( XR*YI - YR*XI ) / D1                                       M31.6671
         WR(I) = -1.0                                                   M29.5262
  480 ZZR = D2                                                          M31.6672
      ZZI = D3                                                          M31.6673
         HR(I,IM1) = ZZR                                                M29.5265
         HI(I,IM1) = ZZI                                                M29.5266
         DO 500 J = I, N                                                M29.5267
            HR(I,J) = HR(I,J) - ZZR * HR(IM1,J) + ZZI * HI(IM1,J)       M29.5268
            HI(I,J) = HI(I,J) - ZZR * HI(IM1,J) - ZZI * HR(IM1,J)       M29.5269
  500    CONTINUE                                                       M29.5270
      IF ( IPRSUP .GE. 3 )                                              M29.5271
     1 WRITE (LUNIT6, 510) I,N, (HR(I,J), HI(I,J), J=1,N)               M29.5272
  510 FORMAT ( 42H AT 510, (HR(I,J), HI(I,J), J=1,N) FOR I =, I8,       M29.5273
     1   10 H, AND N = , I8,  2X, 3HARE,/, (1X, 8E15.6) )               M29.5274
  520 CONTINUE                                                          M29.5275
C     :::::::::: COMPOSITION R*L=H ::::::::::                           M29.5276
      DO 640 J = MP1, IEN                                               M29.5277
         XR = HR(J,J-1)                                                 M29.5278
         XI = HI(J,J-1)                                                 M29.5279
         HR(J,J-1) = 0.0                                                M29.5280
         HI(J,J-1) = 0.0                                                M29.5281
C     :::::::::: INTERCHANGE COLUMNS OF HR, HI, ZR, AND ZI,             M29.5282
C                IF NECESSARY ::::::::::                                M29.5283
         IF (WR(J) .LE. 0.0) GO TO 580                                  M29.5284
         DO 540 I = 1, J                                                M29.5285
            ZZR = HR(I,J-1)                                             M29.5286
            HR(I,J-1) = HR(I,J)                                         M29.5287
            HR(I,J) = ZZR                                               M29.5288
            ZZI = HI(I,J-1)                                             M29.5289
            HI(I,J-1) = HI(I,J)                                         M29.5290
            HI(I,J) = ZZI                                               M29.5291
  540    CONTINUE                                                       M29.5292
         DO 560 I = LOW, IGH                                            M29.5293
            ZZR = ZR(I,J-1)                                             M29.5294
            ZR(I,J-1) = ZR(I,J)                                         M29.5295
            ZR(I,J) = ZZR                                               M29.5296
            ZZI = ZI(I,J-1)                                             M29.5297
            ZI(I,J-1) = ZI(I,J)                                         M29.5298
            ZI(I,J) = ZZI                                               M29.5299
  560    CONTINUE                                                       M29.5300
  580    DO 600 I = 1, J                                                M29.5301
            HR(I,J-1) = HR(I,J-1) + XR * HR(I,J) - XI * HI(I,J)         M29.5302
            HI(I,J-1) = HI(I,J-1) + XR * HI(I,J) + XI * HR(I,J)         M29.5303
  600    CONTINUE                                                       M29.5304
C     :::::::::: ACCUMULATE TRANSFORMATIONS ::::::::::                  M29.5305
         DO 620 I = LOW, IGH                                            M29.5306
            ZR(I,J-1) = ZR(I,J-1) + XR * ZR(I,J) - XI * ZI(I,J)         M29.5307
            ZI(I,J-1) = ZI(I,J-1) + XR * ZI(I,J) + XI * ZR(I,J)         M29.5308
  620    CONTINUE                                                       M29.5309
       IF ( IPRSUP .GE. 3 )                                             M29.5310
     1 WRITE ( LUNIT6, 630) J, LOW, IGH, (HR(I,J),HI(I,J),I=1,J)        M29.5311
     2, (ZR(I,J),ZI(I,J),I=LOW,IGH)                                     M29.5312
  630 FORMAT ( 26H J, LOW AND IGH AT 630 ARE, 3I8, 60H (HR(I,J),HI(I,J),M29.5313
     1I=1,J) AND (ZR(I,J),ZI(I,J),I=LOW,IGH) ARE, /, (1X,8E15.6) )      M29.5314
  640 CONTINUE                                                          M29.5315
      GO TO 240                                                         M29.5316
C     :::::::::: A ROOT FOUND ::::::::::                                M29.5317
  660 HR(IEN,IEN) = HR(IEN,IEN) + TR                                    M29.5318
      WR(IEN) = HR(IEN,IEN)                                             M29.5319
      HI(IEN,IEN) = HI(IEN,IEN) + TI                                    M29.5320
      WI(IEN) = HI(IEN,IEN)                                             M29.5321
      IEN = IENM1                                                       M29.5322
      IF ( IPRSUP .GE. 3 )                                              M29.5323
     1 WRITE (LUNIT6, 670) IEN,HR(IEN,IEN),HI(IEN,IEN),WR(IEN),WI(IEN)  M29.5324
  670 FORMAT ( 67H     IEN    HR(IEN,IEN)    HI(IEN,IEN)       WR(IEN)  M29.5325
     1      WI(IEN),/, I8, 4E15.6)                                      M29.5326
      GO TO 220                                                         M29.5327
  680 CONTINUE                                                          M43.3960
      DO 671 ID=LOW,IGH                                                 M43.3961
      J = IORD(ID)                                                      M43.3962
      UMR(J) = WR(ID)                                                   M43.3963
      UMI(J) = WI(ID)                                                   M43.3964
  671 CONTINUE                                                          M43.3965
C     :::::::::: ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND               M29.5328
C                VECTORS OF UPPER TRIANGULAR FORM ::::::::::            M29.5329
      FNORM = 0.0                                                       M29.5330
      DO 720 I = 1, N                                                   M29.5331
         DO 720 J = I, N                                                M29.5332
            FNORM = FNORM + ABSZ(HR(I,J)) + ABSZ(HI(I,J))               M29.5333
  720 CONTINUE                                                          M29.5334
      HR(1,1) = FNORM                                                   M29.5335
      IF (N .EQ. 1 .OR. FNORM .EQ. 0.0) GO TO 1001                      M29.5336
C     :::::::::: FOR IEN=N STEP -1 UNTIL 2 DO -- ::::::::::             M29.5337
      DO 800 NN = 2, N                                                  M29.5338
         IEN = N + 2 - NN                                               M29.5339
         XR = WR(IEN)                                                   M29.5340
         XI = WI(IEN)                                                   M29.5341
         IENM1 = IEN - 1                                                M29.5342
C     :::::::::: FOR I=IEN-1 STEP -1 UNTIL 1 DO -- ::::::::::           M29.5343
         DO 780 II = 1, IENM1                                           M29.5344
            I = IEN - II                                                M29.5345
            ZZR = HR(I,IEN)                                             M29.5346
            ZZI = HI(I,IEN)                                             M29.5347
            IF (I .EQ. IENM1) GO TO 760                                 M29.5348
            IP1 = I + 1                                                 M29.5349
            DO 740 J = IP1, IENM1                                       M29.5350
               ZZR = ZZR + HR(I,J) * HR(J,IEN) - HI(I,J) * HI(J,IEN)    M29.5351
               ZZI = ZZI + HR(I,J) * HI(J,IEN) + HI(I,J) * HR(J,IEN)    M29.5352
  740       CONTINUE                                                    M29.5353
  760       YR = XR - WR(I)                                             M29.5354
            YI = XI - WI(I)                                             M29.5355
            IF (YR .EQ. 0.0 .AND. YI .EQ. 0.0) YR = EPMACH * FNORM      M29.5356
      D1 = YR ** 2 + YI ** 2                                            M31.6674
      D2 = ( ZZR*YR + ZZI*YI ) / D1                                     M31.6675
      D3 = ( YR*ZZI - YI*ZZR ) / D1                                     M31.6676
            HR(I,IEN) = D2                                              M31.6677
            HI(I,IEN) = D3                                              M31.6678
      IF ( IPRSUP .GE. 3 )                                              M29.5360
     1 WRITE (LUNIT6, 770)  NN,N,IEN,II,I,IENM1,ZZR,ZZI,YR,YI,D2,D3     M31.6679
     2, HR(I,IEN),HI(I,IEN)                                             M29.5362
  770 FORMAT ( 87H NN, N, IEN, II, I, IENM1, ZZR, ZZI, YR, YI, D2, D3, HM31.6680
     1R(I,IEN) AND HI(I,IEN) AT 770 ARE,/, 1X, 6I5, 8E12.5)             M31.6681
  780    CONTINUE                                                       M29.5365
  800 CONTINUE                                                          M29.5366
C     :::::::::: END BACKSUBSTITUTION ::::::::::                        M29.5367
      IENM1 = N - 1                                                     M29.5368
C     :::::::::: VECTORS OF ISOLATED ROOTS ::::::::::                   M29.5369
      DO 840 I = 1, IENM1                                               M29.5370
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 840                     M29.5371
         IP1 = I + 1                                                    M29.5372
         DO 820 J = IP1, N                                              M29.5373
            ZR(I,J) = HR(I,J)                                           M29.5374
            ZI(I,J) = HI(I,J)                                           M29.5375
  820    CONTINUE                                                       M29.5376
  840 CONTINUE                                                          M29.5377
C     :::::::::: MULTIPLY BY TRANSFORMATION MATRIX TO GIVE              M29.5378
C                VECTORS OF ORIGINAL FULL MATRIX.                       M29.5379
C                FOR J=N STEP -1 UNTIL LOW+1 DO -- ::::::::::           M29.5380
      DO 880 JJ = LOW, IENM1                                            M29.5381
         J = N + LOW - JJ                                               M29.5382
C        M = MIN0Z(J-1,IGH)                                             M36.3634
         M = J - 1                                                      M36.3635
         IF ( M .GT. IGH )  M = IGH                                     M36.3636
         DO 880 I = LOW, IGH                                            M29.5384
            ZZR = ZR(I,J)                                               M29.5385
            ZZI = ZI(I,J)                                               M29.5386
            DO 860 K = LOW, M                                           M29.5387
               ZZR = ZZR + ZR(I,K) * HR(K,J) - ZI(I,K) * HI(K,J)        M29.5388
               ZZI = ZZI + ZR(I,K) * HI(K,J) + ZI(I,K) * HR(K,J)        M29.5389
  860       CONTINUE                                                    M29.5390
            ZR(I,J) = ZZR                                               M29.5391
            ZI(I,J) = ZZI                                               M29.5392
      IF (IPRSUP .GE. 3 )                                               M29.5393
     1 WRITE (LUNIT6, 870) JJ, J, M, ZR(I,J),ZI(I,J)                    M31.6682
  870 FORMAT ( 62H AT THE END OF DO 880 LOOP, JJ, J,  M, ZR(I,J) AND ZI(M31.6683
     1I,J) ARE,5X,3I8,2E15.6)                                           M29.5396
  880 CONTINUE                                                          M29.5397
      GO TO 1001                                                        M29.5398
C     :::::::::: SET ERROR -- NO CONVERGENCE TO AN                      M29.5399
C                EIGENVALUE AFTER 30 ITERATIONS ::::::::::              M29.5400
 1000 IERR = IEN                                                        M29.5401
 1001 CONTINUE                                                          M43.3966
CCCC      DO 1002 I=LOW, IGH                                            M43.3967
CCCC      WR(I) = UMR(I)                                                M43.3968
CCCC      WI(I) = UMI(I)                                                M43.3969
 1002 CONTINUE                                                          M43.3970
CC     ** SORTING THE EIGENVALUES WITH MAGNITUDE **                     M43.3971
CC        ALSO REORDERING THE EIGENVECTOR                               M43.3972
      DO 1003 I=1,20                                                    M43.3973
      LSEQ(I)=I                                                         M43.3974
 1003 CONTINUE                                                          M43.3975
      DO 1004 I=1,N                                                     M43.3976
      VTR(I)=WR(I)                                                      M43.3977
      VTI(I)=WI(I)                                                      M43.3978
      DO 1004 J=1,N                                                     M43.3979
      PPR(I,J)=ZR(I,J)                                                  M43.3980
      PPI(I,J)=ZI(I,J)                                                  M43.3981
 1004 CONTINUE                                                          M43.3982
      DO 1005 I=1,N                                                     M43.3983
      EIM(I) = WR(I)*WR(I) + WI(I)*WI(I)                                M43.3984
 1005 CONTINUE                                                          M43.3985
      DO 1006 J=1, N                                                    M43.3986
      L=N-J                                                             M43.3987
      DO 1007 I=1, L                                                    M43.3988
      IF ( EIM(I) .GE. EIM(I+1) ) GO TO 1007                            M43.3989
      T=EIM(I)                                                          M43.3990
      EIM(I)=EIM(I+1)                                                   M43.3991
      EIM(I+1)=T                                                        M43.3992
      T=LSEQ(I)                                                         M43.3993
      LSEQ(I)=LSEQ(I+1)                                                 M43.3994
      LSEQ(I+1)=T                                                       M43.3995
 1007 CONTINUE                                                          M43.3996
 1006 CONTINUE                                                          M43.3997
      DO 1008 I=1,N                                                     M43.3998
      WR(I)=VTR(LSEQ(I))                                                M43.3999
      WI(I)=VTI(LSEQ(I))                                                M43.4000
      DO 1009 J=1,N                                                     M43.4001
      ZR(I,J)=PPR(I,LSEQ(J))                                            M43.4002
      ZI(I,J)=PPI(I,LSEQ(J))                                            M43.4003
 1009 CONTINUE                                                          M43.4004
 1008 CONTINUE                                                          M43.4005
 1100 RETURN                                                            M29.5402
C     :::::::::: LAST CARD OF COMLR2 ::::::::::                         M29.5403
      END                                                               M29.5404
      SUBROUTINE CXRED2( A, C, B, D, N, M )                             M14.3889
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    ELIMINATION OF VARIABLES M+1,...N IN SYMMETRIC COMPLEX MATRIX WITHM15.1632
C)    A=REAL PART, C=IMAGINARY PART. A AND C ARE                        M15.1633
C)    STORED AS TRIANGLE (1 ELEMENT FOR 1.COLUMN,2 FOR 2.COLUMN ETC.).  M15.1634
C)    RESULT IS REDUCED MATRIX IN COLUMNS 1,...M IN CASE OF REDUCTION   M15.1635
C)    (M UNEQUAL 0) OR NEGATIVE INVERSE MATRIX IN COLUMNS 1,...N IN CASEM15.1636
C)    OF INVERSION (M=0).                                               M15.1637
      DIMENSION A(1), C(1), B(1), D(1)                                  M14.3896
      J = N + 1                                                         M14.3897
      W=1.0                                                             M14.3898
      IF(M.GT.0) W=-W                                                   M14.3899
      IJ=N*J/2                                                          M14.3900
    3 J=J-1                                                             M14.3901
      IF(J.EQ.M) RETURN                                                 M14.3902
      H1=A(IJ)                                                          M14.3903
      G1=C(IJ)                                                          M14.3904
      X=1.0/(H1*H1+G1*G1)                                               M14.3905
      H1=-H1*X                                                          M14.3906
      G1=G1*X                                                           M14.3907
      B(J)=H1                                                           M14.3908
      D(J)=G1                                                           M14.3909
      IJ=IJ-J                                                           M14.3910
      K=0                                                               M14.3911
      IK=0                                                              M14.3912
C                                   BEGIN K-LOOP                        M14.3913
    4 IK=IK+K                                                           M14.3914
      I1=IK+1                                                           M14.3915
      K=K+1                                                             M14.3916
      IF(K.GT.N) GO TO 3                                                M14.3917
      IF(K.LT.J) GO TO 9                                                M14.3918
      IF(W.LT.0.) GO TO 3                                               M14.3919
      IF(K.EQ.J) GO TO 7                                                M14.3920
      I=IK+J                                                            M14.3921
    5 H2=A(I)                                                           M14.3922
      G2=C(I)                                                           M14.3923
      B(K) = H2*H1-G2*G1                                                M14.3924
      D(K)=H2*G1+G2*H1                                                  M14.3925
C                                   BEGIN I-LOOP                        M14.3926
      I2=IK+K                                                           M14.3927
      L=0                                                               M14.3928
      DO 6 I=I1,I2                                                      M14.3929
      L=L+1                                                             M14.3930
      X=B(L)                                                            M14.3931
      Y=D(L)                                                            M14.3932
      A(I)=A(I)+X*H2-Y*G2                                               M14.3933
    6 C(I)=C(I)+X*G2+Y*H2                                               M14.3934
      IF(K.LT.J) GO TO 4                                                M14.3935
      I=IK+J                                                            M14.3936
      A(I)=B(K)                                                         M14.3937
      C(I)=D(K)                                                         M14.3938
      GO TO 4                                                           M14.3939
C                                   END I-LOOP                          M14.3940
    7 I=IJ                                                              M14.3941
      DO 8 L=1,J                                                        M14.3942
      I=I+1                                                             M14.3943
      C(I)=D(L)                                                         M14.3944
    8 A(I)=B(L)                                                         M14.3945
      GO TO 4                                                           M14.3946
C                                   END K-LOOP                          M14.3947
    9 I=IJ+K                                                            M14.3948
      GO TO 5                                                           M14.3949
      END                                                               M14.3950
      SUBROUTINE SYMM(P,Z,SWITCH,KCIRCT,KK)                             M14.3951
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION P(1), Z(1)                                              M14.3952
      DIMENSION AR(3,3),AI(3,3),FR(3),FI(3)                             M14.3953
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      IF(KCIRCT.EQ.2) GO TO 100                                         M14.3956
      FR(1) = UNITY                                                     M14.3957
      FI(1) = 0.                                                        M14.3958
      FR(2) = - ONEHAF                                                  M14.3959
      FI(2) = VALU7                                                     M14.3960
      FR(3) = - ONEHAF                                                  M14.3961
      FI(3) = - FI(2)                                                   M14.3962
      KK=0                                                              M14.3963
   63 KI=KK*(KK+1)/2                                                    M14.3964
      J=KI+KK                                                           M14.3965
      KOLD=KK                                                           M14.3966
      KK=KK+3                                                           M14.3967
      IF(KK.GT.KCIRCT) GO TO 79                                         M14.3968
   75 L=KI+1                                                            M14.3969
      K=0                                                               M14.3970
   64 K=K+1                                                             M14.3971
      IF(KI.EQ.J) GO TO 76                                              M14.3972
   65 L3 = L+2                                                          M14.3973
   66 L2 = L+1                                                          M14.3974
   67 DO 681 I=1,3                                                      M14.3975
      F1 = P(L) + FR(I)*(P(L2)+P(L3))                                   M14.3976
      F2 = FI(I)*(P(L2) - P(L3))                                        M14.3977
      IF(SWITCH.LT.0.0) GO TO 68                                        M14.3978
      F1 = F1 + FI(I)*(Z(L3)-Z(L2))                                     M14.3979
      F2 = F2+Z(L)+FR(I)*(Z(L2)+Z(L3))                                  M14.3980
   68 AR(I,K) = F1                                                      M14.3981
  681 AI(I,K) = F2                                                      M14.3982
      IF (K.EQ.3) GO TO 69                                              M14.3983
      L = L+KOLD+K                                                      M14.3984
      GO TO 64                                                          M14.3985
   69 L=KI                                                              M14.3986
      K=0                                                               M14.3987
   70 K=K+1                                                             M14.3988
      DO 72 I=1,3                                                       M14.3989
      M=L+I                                                             M14.3990
      IF(KI.LT.J) GO TO 71                                              M14.3991
      IF(I.GT.K) GO TO 73                                               M14.3992
   71 P(M) =(AR(I,1)+FR(K)*(AR(I,2)+AR(I,3))+FI(K)*(AI(I,3)-AI(I,2)) )  M14.3993
     1 / 3.0                                                            M14.3994
   72 Z(M) =(AI(I,1)+FR(K)*(AI(I,2)+AI(I,3))+FI(K)*(AR(I,2)-AR(I,3)) )  M14.3995
     1 / 3.0                                                            M14.3996
   73 IF (K.EQ.3) GO TO 74                                              M14.3997
      L = L+KOLD+K                                                      M14.3998
      GO TO 70                                                          M14.3999
   74 KI = KI+3                                                         M14.4000
      IF (KI.GT.J) GO TO 63                                             M14.4001
      GO TO 75                                                          M14.4002
   76 IF(K.EQ.3) GO TO 65                                               M14.4003
      L2 = J+KK-1                                                       M14.4004
      L3 = L2+KK                                                        M14.4005
      IF (K.EQ.2) GO TO 66                                              M14.4006
      L3 = L3-1                                                         M14.4007
      GO TO 67                                                          M14.4008
   79 KK=KK-3                                                           M14.4009
C                                   END OF SYMMETRICAL COMPONENTS MATRIXM14.4010
      RETURN                                                            M14.4011
  100 F1=(P(1)+P(3))* ONEHAF                                            M14.4012
      F2=P(2)                                                           M14.4013
      P(2)=(P(1)-P(3))* ONEHAF                                          M14.4014
      P(1)=F1+F2                                                        M14.4015
      P(3)=F1-F2                                                        M14.4016
      P(5)=P(3)                                                         M14.4017
      KK=2                                                              M14.4018
      IF(SWITCH.LT. 0.) RETURN                                          M14.4019
      F1=(Z(1)+Z(3))* ONEHAF                                            M14.4020
      F2=Z(2)                                                           M14.4021
      Z(2)=(Z(1)-Z(3))* ONEHAF                                          M14.4022
      Z(1)=F1+F2                                                        M14.4023
      Z(3)=F1-F2                                                        M14.4024
      Z(5)=Z(3)                                                         M14.4025
      RETURN                                                            M14.4026
      END                                                               M14.4027
      SUBROUTINE SKIN (S,R,FREQ,RF,XF)                                  M14.4028
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      DOUBLE PRECISION  VSMALL                                          M32, 460
      VSMALL = 1.E-37                                                   M32, 461
      CALL UNDRFL(JJJ)                                                  M33.2366
      S2 = S*S                                                          M14.4031
      S3=(UNITY-S2)*R                                                   M14.4032
      R2 = FREQ * VALU8 / S3                                            M14.4033
      RF=R                                                              M14.4034
      XF=0.                                                             M14.4035
      IF(R2.EQ.0.) GO TO 9900                                           M32, 462
      QREMB = 0.0                                                       M21. 404
      IF ( S  .LT.  TENM6 )   GO TO 5                                   M14.4037
      Q2 = R2*S2                                                        M14.4038
      IF (S2 .LT. 0.8)  GO TO 11                                        M21. 405
      IF (Q2 .LE. 64.0 .AND. R2 .GT. 64.0)  WRITE(LUNIT6,10) Q2, R2     M21. 406
   10 FORMAT (52H RESULTS FROM SUBROUTINE SKIN UNRELIABLE WITH MQ**2=,  M21. 407
     1  F9.4, 11H AND MR**2=, F9.4)                                     M21. 408
   11 IF (Q2 .GT. 64.0)  QREMB=SQRTZ(Q2) * SQRT2                        M21. 409
      X = SQRTZ(Q2)                                                     M18. 965
      X2=X*X/64.0                                                       M14.4040
      IBACK = 2                                                         M14.4041
      IF (X2.LE.UNITY) GO TO 100                                        M14.4042
      GO TO 200                                                         M14.4043
    4 A = - BERD                                                        M21. 410
      B = - BEID                                                        M21. 411
      AREMB = GERD                                                      M21. 412
      BREMB = GEID                                                      M21. 413
      XREMB = X                                                         M21. 414
    5 X = SQRTZ(R2)                                                     M18. 967
      X2=X*X/64.0                                                       M14.4049
      IBACK = 1                                                         M14.4050
      IF (X2.LE.UNITY) GO TO 100                                        M14.4051
      GO TO 200                                                         M14.4052
    6 G = BER                                                           M14.4053
      H = BEI                                                           M14.4054
      E = BERD                                                          M14.4055
      F = BEID                                                          M14.4056
      IF ( S  .LT.  TENM6 )   GO TO 7                                   M14.4057
      G = A*GER - B*GEI + AREMB*BER - BREMB *BEI                        M21. 415
      H = A*GEI + B*GER + AREMB*BEI + BREMB*BER                         M21. 416
      E = A*GERD - B*GEID + AREMB*BERD - BREMB*BEID                     M21. 417
      F = A*GEID + B*GERD + AREMB*BEID + BREMB*BERD                     M21. 418
    7 E2F2 = E**2 + F**2                                                M21. 419
      S2 =  X * S3 * ONEHAF / E2F2                                      M21. 420
      IF (IPRSUP .GE. 1)                                                M21. 421
     1 WRITE (LUNIT6, 320)  E, F, E2F2, S2                              M21. 422
  320 FORMAT (27H E, F,  E2F2, AND S2 AT 320, 4E16.6)                   M21. 423
      RF = (-H*E+G*F)*S2                                                M14.4063
      XF = (G*E+H*F)*S2                                                 M14.4064
      GO TO 9900                                                        M32, 463
C      CALCULATION OF KELVIN-FUNCTIONS FOR ARGUMENT.LE.8.               M14.4066
  100 Z = X2                                                            M14.4067
      BER = UNITY                                                       M14.4068
      BEI = 0.0                                                         M14.4069
      BERD = 0.0                                                        M14.4070
      BEID = ONEHAF                                                     M14.4071
      GERD = 0.0                                                        M14.4072
      GEID =  VALU9                                                     M14.4073
      GER = - VALU10                                                    M14.4074
      GEI = 0.0                                                         M14.4075
      IALT = 1                                                          M14.4076
      DO 103 K=1,14                                                     M14.4077
      IF(IALT.EQ.1) GO TO 101                                           M14.4078
      BER = BER+FBE(K)*Z                                                M14.4079
      BEID = BEID+FBED(K)*Z                                             M14.4080
      IF ( S  .LT.  TENM6 )   GO TO 102                                 M14.4081
      GEID = GEID+FKED(K)*Z                                             M14.4082
      IF(IBACK.EQ.2) GO TO 102                                          M14.4083
      GER = GER+FKE(K)*Z                                                M14.4084
      GO TO 102                                                         M14.4085
  101 BEI = BEI+FBE(K)*Z                                                M14.4086
      BERD = BERD+FBED(K)*Z                                             M14.4087
      IF ( S  .LT.  TENM6 )   GO TO 102                                 M14.4088
      GERD = GERD+FKED(K)*Z                                             M14.4089
      IF(IBACK.EQ.2) GO TO 102                                          M14.4090
      GEI = GEI+FKE(K)*Z                                                M14.4091
  102 Z = Z*X2                                                          M14.4092
  103 IALT = -IALT                                                      M14.4093
      BEID = BEID*X                                                     M14.4094
      BERD = BERD*X                                                     M14.4095
      IF ( S  .LT.  TENM6 )   GO TO 104                                 M14.4096
      XL = ALOGZ(X*ONEHAF )                                             M18. 968
      GERD = -XL*BERD-BER/X+BEID* AAA1            +X*GERD               M14.4098
      GEID = -XL*BEID-BEI/X-BERD* AAA1            +X*GEID               M14.4099
      IF (IBACK.EQ.2) GO TO 104                                         M14.4100
      GER = -XL*BER+BEI* AAA1            +GER                           M14.4101
      GEI = -XL*BEI-BER* AAA1            +GEI                           M14.4102
  104 GO TO (6 , 4 ),IBACK                                              M14.4103
C      CALCULATIONS OF KELVIN-FUNCTIONS FOR ARGUMENT.GT.8.              M14.4104
  200 X2 = 8.0  /X                                                      M14.4105
      Z = X2                                                            M14.4106
      BER = 0.0                                                         M14.4107
      BEI = - VALU11                                                    M14.4108
      BERD = BER                                                        M14.4109
      BEID = BEI                                                        M14.4110
      GER = UNITY / SQRT2                                               M14.4111
      GEI = GER                                                         M14.4112
      GERD = GER                                                        M14.4113
      GEID = GEI                                                        M14.4114
      IALT = 1                                                          M14.4115
      DO 203  K=1, 6                                                    M14.4116
      THETAR = FBE(K+14) * Z                                            M14.4117
      THETAI = FBED(K+14)*Z                                             M14.4118
      PHIR = FKE(K+14)*Z                                                M14.4119
      PHII = FKED(K+14)*Z                                               M14.4120
      BER = BER+THETAR                                                  M14.4121
      BEI = BEI+THETAI                                                  M14.4122
      GER = GER+PHIR                                                    M14.4123
      GEI = GEI+PHII                                                    M14.4124
      IF (IALT.EQ.1) GO TO 201                                          M14.4125
      BERD = BERD+THETAR                                                M14.4126
      BEID = BEID+THETAI                                                M14.4127
      GERD = GERD+PHIR                                                  M14.4128
      GEID = GEID+PHII                                                  M14.4129
      GO TO 202                                                         M14.4130
  201 BERD = BERD-THETAR                                                M14.4131
      BEID = BEID-THETAI                                                M14.4132
      GERD = GERD-PHIR                                                  M14.4133
      GEID = GEID-PHII                                                  M14.4134
  202 IALT = -IALT                                                      M14.4135
  203 Z = Z*X2                                                          M14.4136
      XL=X* SQRT2                                                       M14.4137
      IF (QREMB .LT. 1.0)  GO TO 204                                    M21. 424
      XL = XL - QREMB                                                   M21. 425
  204 THETAR = - XL + BERD                                              M21. 426
      THETAI = -XL+BEID                                                 M14.4139
      Z = SQRTZ(X)                                                      M18. 969
      X2 =  VALU12 / Z                                                  M14.4141
      Z = VALU13 / Z* EXPZ(THETAR)                                      M18. 970
      FR = Z* COSZ(THETAI)                                              M18. 971
      FI = Z* SINZ(THETAI)                                              M18. 972
      X2=X2* EXPZ(BER)                                                  M18. 973
      THETAR=X2* COSZ(BEI)                                              M18. 974
      THETAI=X2* SINZ(BEI)                                              M18. 975
      Z = -FR*GERD+FI*GEID                                              M14.4148
      GEID = -FR*GEID-FI*GERD                                           M14.4149
      GERD = Z                                                          M14.4150
      Z = AAA2 * EXPZ(-QREMB)                                           M21. 427
      GR = Z*SINZ(QREMB)                                                M21. 428
      GI = Z*COSZ(QREMB)                                                M21. 429
      BERD = THETAR*GER - THETAI*GEI + GERD*GR - GEID*GI                M21. 430
      BEID = THETAR*GEI + THETAI*GER + GERD*GI + GEID*GR                M21. 431
      GER = FR                                                          M14.4153
      GEI = FI                                                          M14.4154
      BER = THETAR + GER*GR - GEI*GI                                    M21. 432
      BEI = THETAI + GER*GI + GEI*GR                                    M21. 433
      GO TO ( 6 , 4 ),IBACK                                             M14.4157
 9900 CONTINUE                                                          M32, 464
      IF( JJJ .EQ. 3 ) Z = VSMALL                                       M32, 465
      RETURN                                                            M32, 466
      END                                                               M14.4158
      SUBROUTINE UNDRFL ( N1 )                                          M33.2367
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     DUMMY IMITATION OF ONTARIO HYDRO UNIVAC MODULE.                   M33.2368
      N1 = -7654                                                        M33.2369
      RETURN                                                            M33.2370
      END                                                               M33.2371
      SUBROUTINE WRTE ( P, I2, I3, IFLAG, L, UNIT, LUNIT6 )             M16.4545
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION P(1),R(10)                                              M14.4160
      J=0                                                               M14.4161
      DO 30 I=I2,I3                                                     M14.4162
      J=J+1                                                             M14.4163
   30 R(J)=P(I)*UNIT                                                    M14.4164
      GOTO (1,2,3),IFLAG                                                M14.4165
    1 WRITE(LUNIT6,11)(R(I), I=1,J)                                     M17.1337
      RETURN                                                            M14.4167
    2 WRITE(LUNIT6,22)(R(I),I=1,J)                                      M17.1338
      RETURN                                                            M14.4169
    3 WRITE(LUNIT6,33) L, (R(I),I=1,J)                                  M17.1339
      RETURN                                                            M14.4171
   11 FORMAT(1H0,3X,9E14.5)                                             M14.4172
   22 FORMAT(4X,9E14.5)                                                 M14.4173
   33 FORMAT(1H0,I3,9E14.5)                                             M14.4174
      END                                                               M14.4175
      SUBROUTINE REDU44 ( A, B, N, M )                                  M14.4176
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    ELIMINATION OF VARIABLES M+1,...N IN SYMMETRIC MATRIX A. A IS     M15.1653
C)    STORED AS TRIANGLE (1 ELEMENT FOR 1.COLUMN,2 FOR 2.COLUMN ETC.).  M15.1654
C)    RESULT IS REDUCED MATRIX IN COLUMNS 1,...M IN CASE OF REDUCTION   M15.1655
C)    (M UNEQUAL 0) OR NEGATIVE INVERSE MATRIX IN COLUMNS 1,...N IN CASEM15.1656
C)    OF INVERSION (M=0).                                               M15.1657
      DIMENSION A(1), B(1)                                              M14.4182
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      J = N + 1                                                         M14.4190
      W=UNITY                                                           M14.4191
      IF(M.GT.0) W=-W                                                   M14.4192
      IJ=N*J/2                                                          M14.4193
      IF ( IPRSUP  .GE.  4 )                                            M22.6266
     1 WRITE (LUNIT6, 2692)  N, M, IJ,  ( A(K), K=1, IJ )               M29.5405
 2692 FORMAT ( /,  24H AT START OF  'REDU44' .,                         M22.6268
     1 24H       N       M      IJ  ,/,                                 M29.5406
     2 24X,  3I8  ,/,  29H (A(K), K=1, IJ)  FOLLOW ....  ,/,            M29.5407
     3 ( 1X,  8E16.7 )  )                                               M22.6271
    3 J=J-1                                                             M14.4194
      IF(J.EQ.M) RETURN                                                 M14.4195
      H1=A(IJ)                                                          M14.4196
      IF ( ABSZ(H1)  .GT.  EPSILN )  GO TO 6421                         M18. 976
      KILL = 86                                                         M14.4198
      LSTAT(19) = 6421                                                  M14.4199
      LSTAT(18) = 51                                                    M14.4200
      LSTAT(13) = N                                                     M14.4201
      LSTAT(14) = M                                                     M14.4202
      LSTAT(15) = J                                                     M14.4203
      FLSTAT(15) = EPSILN                                               M14.4204
      FLSTAT(16) = H1                                                   M14.4205
      RETURN                                                            M14.4206
 6421 H1=-UNITY/H1                                                      M14.4207
      B(J)=H1                                                           M14.4208
      IJ=IJ-J                                                           M14.4209
      K=0                                                               M14.4210
      IK=0                                                              M14.4211
C                                   BEGIN K-LOOP                        M14.4212
    4 IK=IK+K                                                           M14.4213
      I1=IK+1                                                           M14.4214
      K=K+1                                                             M14.4215
      IF(K.GT.N) GO TO 3                                                M14.4216
      IF(K.LT.J) GO TO 9                                                M14.4217
      IF(W.LT.0.) GO TO 3                                               M14.4218
      IF(K.EQ.J) GO TO 7                                                M14.4219
      I=IK+J                                                            M14.4220
    5 H2=A(I)                                                           M14.4221
      B(K)=H2*H1                                                        M14.4222
C                                   BEGIN I-LOOP                        M14.4223
      I2=IK+K                                                           M14.4224
      L=0                                                               M14.4225
      DO 6 I=I1,I2                                                      M14.4226
      L=L+1                                                             M14.4227
    6 A(I)=A(I)+B(L)*H2                                                 M14.4228
      IF(K.LT.J) GO TO 4                                                M14.4229
      I=IK+J                                                            M14.4230
      A(I)=B(K)                                                         M14.4231
      GO TO 4                                                           M14.4232
C                                   END I-LOOP                          M14.4233
    7 I=IJ                                                              M14.4234
      DO 8 L=1,J                                                        M14.4235
      I=I+1                                                             M14.4236
    8 A(I)=B(L)                                                         M14.4237
      GO TO 4                                                           M14.4238
C                                   END K-LOOP                          M14.4239
    9 I=IJ+K                                                            M14.4240
      GO TO 5                                                           M14.4241
      END                                                               M14.4242
      SUBROUTINE OUTPUT(METRIK,P,Z,SWITCH,KMAX,IS,K2)                   M14.4243
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      DIMENSION P(1), Z(1)                                              M14.4244
      REAL*8       TXTMI,TXTKM,TXTUNT                                   M14.4245
      DATA TXTMI/ 6HMILES  /                                            M14.4246
      DATA TXTKM / 6H  KM    /                                          M14.4247
      IF ( JPRALT  .EQ.  0 )   RETURN                                   M16.4548
      LL1 = 1                                                           M16.4549
      LL2 = 2                                                           M16.4550
      LL3 = 3                                                           M16.4551
      TXTUNT = TXTMI                                                    M14.4248
      UNIT = 1.0                                                        M14.4249
      IF ( METRIK .EQ. 1 ) TXTUNT = TXTKM                               M14.4250
      IF ( METRIK .EQ. 1 ) UNIT = 5280. * 12. *2.54D0/ 100000.D0        M14.4251
      CHECK=SWITCH                                                      M14.4252
      WRITE(LUNIT6,66)                                                  M17.1341
   66 FORMAT(1H0)                                                       M14.4254
      GO TO (305,306,307,308,332,333,336,337,340,341), IS               M14.4255
  305 WRITE(LUNIT6,301) TXTUNT                                          M17.1342
  301 FORMAT(1H0,12X,35HINVERTED CAPACITANCE MATRIX (DARAF- ,A4, 1H) )  M14.4257
      GO TO 5                                                           M14.4258
  306 WRITE(LUNIT6,302) TXTUNT                                          M17.1343
  302 FORMAT(1H0,14X,33HINVERTED SUSCEPTANCE MATRIX (OHM- ,A4, 1H) )    M14.4260
      GO TO 5                                                           M14.4261
  307 WRITE(LUNIT6,303) TXTUNT                                          M17.1344
  303 FORMAT(1H0,21X,26HCAPACITANCE MATRIX (FARAD/ ,A4, 1H) )           M14.4263
      UNIT = 1.0 / UNIT                                                 M14.4264
      GO TO 5                                                           M14.4265
  308 WRITE(LUNIT6,304) TXTUNT                                          M17.1345
  304 FORMAT(1H0,23X,24HSUSCEPTANCE MATRIX (MHO/ ,A4, 1H) )             M14.4267
      UNIT = 1.0 / UNIT                                                 M14.4268
      GO TO 5                                                           M14.4269
  332 WRITE(LUNIT6,330) TXTUNT                                          M17.1346
  330 FORMAT(1H0,16X,31HINVERTED IMPEDANCE MATRIX (MHO- ,A4, 1H) )      M14.4271
      GO TO 5                                                           M14.4272
  333 WRITE(LUNIT6,331) TXTUNT                                          M17.1347
  331 FORMAT(1H0,25X,22HIMPEDANCE MATRIX (OHM/ ,A4, 1H) )               M14.4274
      UNIT = 1.0 / UNIT                                                 M14.4275
      GO TO 5                                                           M14.4276
  336 WRITE(LUNIT6,334)                                                 M17.1348
  334 FORMAT(1H0,19X,33HTRANSFER ADMITTANCE MATRIX (MHOS))              M14.4278
      GO TO 5                                                           M14.4279
  337 WRITE(LUNIT6,335)                                                 M17.1349
  335 FORMAT(1H0, 16X, 36HTWICE SHUNT ADMITTANCE MATRIX (MHOS))         M22.6272
      GO TO 5                                                           M14.4282
  340 WRITE(LUNIT6,338)                                                 M17.1350
  338 FORMAT(1H0,20X,32HTRANSFER IMPEDANCE MATRIX (OHMS))               M14.4284
      GO TO 5                                                           M14.4285
  341 WRITE(LUNIT6,339)                                                 M17.1351
  339 FORMAT(1H0, 11X, 41HONE HALF OF SHUNT IMPEDANCE MATRIX (OHMS))    M22.6273
    5 GO TO (320,321,322),K2                                            M14.4288
  320 WRITE(LUNIT6,309)                                                 M17.1352
  309 FORMAT(1H+,53X,37HFOR THE SYSTEM OF PHYSICAL CONDUCTORS)          M14.4290
      GO TO 6                                                           M14.4291
  321 WRITE(LUNIT6,310)                                                 M17.1353
  310 FORMAT(1H+,53X,45HFOR THE SYSTEM OF EQUIVALENT PHASE CONDUCTORS)  M14.4293
      GO TO 6                                                           M14.4294
  322 WRITE(LUNIT6,311)                                                 M17.1354
  311 FORMAT(1H+,53X,65HFOR THE SYMMETRICAL COMPONENTS OF THE EQUIVALENTM14.4296
     1 PHASE CONDUCTORS)                                                M14.4297
    6 IF (K2.EQ.3  ) GO TO 3                                            M14.4298
      WRITE (LUNIT6,312)                                                M17.1355
  312 FORMAT(1H ,30X,54HROWS AND COLUMNS PROCEED IN SAME ORDER AS SORTEDM14.4300
     1 INPUT)                                                           M14.4301
    4 K=0                                                               M14.4302
      IF (IS .EQ. 8)  WRITE(LUNIT6, 350)                                M22.6274
      IF (IS .EQ.10)  WRITE(LUNIT6, 350)                                M22.6275
  350 FORMAT(124H SUM OF TWO EQUAL SHUNT ADMITTANCES AT BOTH TERMINALS, M22.6276
     1OR ITS INVERSE, PRINTED TO CONFORM TO TRANSIENTS PROGRAM INPUT FORM22.6277
     2MAT)                                                              M22.6278
      KK=0                                                              M14.4303
      KI = 0                                                            M14.4304
    1 K = K+1                                                           M14.4305
      IF(K.GT.KMAX) RETURN                                              M14.4306
      ICOUNT=0                                                          M14.4307
      I2 = KI + 1                                                       M14.4308
      I3=I2+8                                                           M14.4309
      KI = KI+K                                                         M14.4310
    7 IF(I3.GT.KI) I3=KI                                                M14.4311
      IF(ICOUNT.EQ.0) GO TO 8                                           M14.4312
      CALL WRTE ( P(1), I2, I3, LL1, L, UNIT, LUNIT6 )                  M16.4552
   10 IF ( CHECK  .GT.  0.0 )                                           M16.4553
     1 CALL WRTE ( Z(1), I2, I3, LL2, L, UNIT, LUNIT6 )                 M16.4554
      IF(I3.EQ.KI) GO TO 1                                              M14.4315
      I2=I3+1                                                           M14.4316
      I3=I2+8                                                           M14.4317
      GO TO 7                                                           M14.4318
    8 ICOUNT=1                                                          M14.4319
      IF(K2.EQ.3) GO TO 20                                              M14.4320
      L=K                                                               M14.4321
   21 CALL WRTE ( P(1), I2, I3, LL3, L, UNIT, LUNIT6 )                  M16.4555
      GO TO 10                                                          M14.4323
    3 IF(KMAX.EQ.2) GO TO 400                                           M14.4324
      WRITE(LUNIT6,313)                                                 M17.1356
  313 FORMAT(1H ,25X,92HROWS PROCEED IN SEQUENCE 0,1,2, 0,1,2 ETC. AND CM14.4326
     1OLUMNS PROCEED IN SEQUENCE 0,2,1, 0,2,1 ETC.)                     M14.4327
      GO TO 4                                                           M14.4328
  400 WRITE(LUNIT6,401)                                                 M17.1357
  401 FORMAT(1H ,38X,66HTHIS IS A TWO-PHASE LINE. ROWS AND COLUMNS PROCEM14.4330
     1ED IN SEQUENCE 0,1)                                               M14.4331
      IF(IS.LE.4) CHECK=-1.0                                            M14.4332
      GO TO 4                                                           M14.4333
   20 L=KK                                                              M14.4334
      KK=KK+1                                                           M14.4335
      IF(KK.EQ.3) KK=0                                                  M14.4336
      GO TO 21                                                          M14.4337
      END                                                               M14.4338
      SUBROUTINE OUTSPC(P   , Z   ,KMAX  ,METRIK,FMIPKM)                M15.1658
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION P(1), Z(1)                                              M14.4340
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL44.FOR;/NOLIST'                                           
      IF(KMAX.EQ.4) GO TO 999                                           M14.4343
      WRITE(LUNIT6,222)                                                 M17.1358
  222 FORMAT ( /, 55H SPECIAL OUTPUT FOR MUTUALS NOT APPLICABLE TO THIS M14.4345
     1CASE   )                                                          M14.4346
      RETURN                                                            M14.4347
  999 AA= VALU7 *(Z(8)-Z(9))                                            M14.4348
      BB= -VALU7 *(P(8)-P(9))                                           M14.4349
      A2=P(7)-ONEHAF*(P(8)+P(9))                                        M14.4350
      B2=Z(7)-ONEHAF*(Z(8)+Z(9))                                        M14.4351
      A1=A2+AA                                                          M14.4352
      B1=B2+BB                                                          M14.4353
      A2=A2-AA                                                          M14.4354
      B2=B2-BB                                                          M14.4355
      A0=P(7)+P(8)+P(9)                                                 M14.4356
      B0=Z(7)+Z(8)+Z(9)                                                 M14.4357
      C0=SQRTZ(A0**2+B0**2)                                             M18. 977
      C1=SQRTZ(A1**2+B1**2)                                             M18. 978
      C2=SQRTZ(A2**2+B2**2)                                             M18. 979
      IF(METRIK .EQ. 1)GO TO 1120                                       M15.1659
      WRITE(LUNIT6,1111) C1,C2,C0                                       M17.1359
 1111 FORMAT(28H MUTUAL IMPEDANCE  POSITIVE=,F8.5,20H OHM/MILE  NEGATIVEM14.4362
     1=,F8.5,16H OHM/MILE  ZERO=,F8.4,9H OHM/MILE)                      M14.4363
      RETURN                                                            M14.4364
 1120 C1 = C1 * FMIPKM                                                  M15.1660
      C2 = C2 * FMIPKM                                                  M15.1661
      C0 = C0 * FMIPKM                                                  M15.1662
      WRITE(LUNIT6, 1121) C1, C2, C0                                    M15.1663
 1121 FORMAT(28H MUTUAL IMPEDANCE  POSITIVE=,F8.5,20H OHM/KM    NEGATIVEM15.1664
     1=,F8.5,16H OHM/KM    ZERO=,F8.4,9H OHM/KM   )                     M15.1665
      RETURN                                                            M15.1666
      END                                                               M14.4365
