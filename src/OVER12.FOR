      SUBROUTINE OVER12
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR/NOLIST'    ! WSM + THL
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      DIMENSION  WK1(1)                                                 M43.1155
      EQUIVALENCE ( SEMAUX(1), WK1(1) )                                 M43.1156
      DIMENSION  AKEY(1), TSTAT(1)                                      M33.1025
      EQUIVALENCE ( AKEY(1), ADELAY(1) ), ( TSTAT(1), CRIT(1) )         M35.1292
C     DC-48 HAD "LASTSW" CLOBBERED ON 2ND ENERGIZATION; CHANGE          M32.3940
C     DUMMY USAGE OF THIS VECTOR TO "LASTXX" TEMPORARILY (12 SEP        M32.3941
      DIMENSION  LASTXX(50)                                                     
      DIMENSION  CMR(1), CMI(1)                                            10627
      EQUIVALENCE   ( KKS(1), CMR(1) ),    ( KKNONL(1), CMI(1) )           10628
      EQUIVALENCE  (MONCAR(1),    KNT),   (MONCAR(2),  KBASE)           M37.4098
      EQUIVALENCE  (MONCAR(3), LTDELT),   (MONCAR(4),    ISW)           M37.4099
      EQUIVALENCE  (MONCAR(5),  IDIST),   (MONCAR(6),  ITEST)           M37.4100
      EQUIVALENCE                         (MONCAR(8), JSEEDR)           M37.4101
      EQUIVALENCE  (MONCAR(9), KLOAEP),   (MONCAR(10), MTAPE)           M37.4102
      COMMON /FDQLCL/ KOFF1,KOFF2,KOFF3,KOFF4,KOFF5,KOFF6,KOFF7,KOFF8,  M44.1477
     1 KOFF9,KOFF10,KOFF13,KOFF14,KOFF15,KOFF16,KOFF17,KOFF18,          M44.1478
     2 KOFF19,KOFF20,KOFF21,KOFF22,KOFF23,KOFF24,KOFF25,INOFF1,INOFF2,  M44.1479
     3 INOFF3,INOFF4,INOFF5,NQTT, LCBL, LMODE, NQTW                     M44.1480
      REAL*8         ATIM                                               M11. 125
      DIMENSION ATIM(2)                                                    10634
      COMMON  /SYSTEMATIC/  LINSYS                                      M42.6577
      IF ( IPRSUP  .GE.  1 )                                            M32.3943
     1 WRITE ( LUNIT6, 4567 )                                           M32.3944
 4567 FORMAT ( 24H  "BEGIN MODULE OVER12." )                            M32.3945
      NPH = 0                                                           M44. 398
      NQTT = 0                                                                  
      NQTW = 0                                                          M44. 400
      JGLNN = 0                                                         M44. 401
      ISECTI  = 400    ! This variable should be in COMMON,thl.3/21/90
      SEEDR = 7 * TWOPI                                                         
      INECHO = 0                                                        M20.3237
      LL0 = 0                                                           M22.3549
      LL4 = 4                                                           M22.3550
      LL6 = 6                                                           M23.2044
      ONE = 1.0                                                         M22.3551
C     INITIALIZE COUNTERS FOR THE -666 BRANCHES*   *   *   *   *   *   *M32.3946
      IKF = 0                                                           M32.3947
      ISFD = 0                                                          M32.3948
      IF (IPRSUP .GE. 1)                                                M32.3949
     1 WRITE (LUNIT6, 2716)  KSWTCH, KBASE, ISW, NENERG,                M19.1131
     2                       ( KDEPSW(I), I=1, KSWTCH )                 M19.1132
 2716 FORMAT (/,  25H AT BEGINNING  'OVER12' .,                         M19.1133
     1 32H  KSWTCH   KBASE     ISW  NENERG,  /,                         M19.1134
     2 25X,  4I8  ,/,   37H (KDEPSW(I), I=1, KSWTCH)  FOLLOW ...   ,/,  M19.1135
     3 ( 1X,  20I6 )  )                                                 M19.1136
C     DEFINE LSTAT(15) AS A FLAG FOR OMIT BASE CASE IN A STAT. RUN      M36.1413
      IF ( KBASE .EQ. INTINF)   LSTAT(15) = INTINF                      M36.1414
      IF (LASTOV .EQ. 1) GO TO 13000                                    M22.3552
      IF ( IPRSUP  .GE.  1 )                                            M13.1408
     1 WRITE (LUNIT6, 4624)  KNT, KBASE, KLOAEP, MTAPE, NENERG, JSEEDR, M17. 599
     2 LTDELT, KSWTCH                                                   M13.1410
      IF ( KBASE  .EQ.  2 )   GO TO 164                                 M19.1137
  163 CALL RUNTYM(D1,D2)                                                        
      FLSTAT(3) = FLSTAT(3) + D1                                        M20.3250
      FLSTAT(4) = FLSTAT(4) + D2                                        M20.3251
      FLSTAT(5) = FLSTAT(5) - D1                                        M20.3252
      FLSTAT(6) = FLSTAT(6) - D2                                        M20.3253
  164 IF ( ISW  .EQ.  4444 )   GO TO 4088                               M15. 790
      IF (KBASE .NE. 2)   GO TO 76403                                   M13.1411
      IF (IPRSUP .GE. 1)                                                M22.3556
     1     WRITE (LUNIT6, 2721) N4, NWORD1, NWORD2, LTLABL              M22.3557
      CALL TABLES                                                       M25. 311
      REWIND  LUNT10                                                    M25. 312
      READ (LUNT10)  N8                                                 M25. 313
      IF ( N8  .LE.  0 )   GO TO 7246                                   M25. 314
      DO 7245  I=1, N8                                                  M25. 315
      READ (LUNT10)  J, AKEY(J), TSTAT(J), TOPEN(J + LSWTCH)            M32.3950
      IF ( IPRSUP  .GE.  2 )                                            M25. 317
     1 WRITE (LUNIT6, 7242) I,J,AKEY(J),TSTAT(J),TOPEN(J+LSWTCH)        M32.3951
 7242 FORMAT ( 20H STAT. OPEN.  I, J =,  2I5,  5X,  3E15.6 )            M28.4295
 7245 CONTINUE                                                          M25. 320
 7246 INECHO = 0                                                        M32.3952
      REWIND LUNT12                                                     M32.3953
      IF ( KNT .EQ. 1 ) MTAPE = LUNIT5                                  M32.3954
      LUNIT5 = LUNT12                                                   M32.3955
      REWIND LUNIT5                                                     M32.3956
      IF (KNT .GT. 1)   GO TO  409                                      M32.3957
  166 IPLOT = -1                                                        M32.3958
      REWIND  LUNIT3                                                    M17. 600
      DO 167 I = 1 , KSWTCH                                             M32.3959
      J = LSWTCH + I                                                    M32.3960
  167 WRITE (LUNIT3) KMSWIT(I), KMSWIT(J), AKEY(I), TSTAT(I),           M32.3961
     1 TOPEN(J), KDEPSW(I)                                              M32.3962
      WRITE (LUNIT3) KLOAEP                                             M32.3963
      GO TO 409                                                         M32.3964
76403 IF ( LOOPSS(1) .NE. 7766 )  GO TO 13000                           M32.3965
      IF ( IPRSUP .GE. 1 )                                              M33.1027
     1 WRITE (LUNIT6, 7474)  NUMUM, IT1, IBR                            M33.1028
 7474 FORMAT ( 37H IN "OVER12".  B4 SNEAKY RESTORATION.,                M33.1029
     1         20H   NUMUM, IT1, IBR =,  3I8  )                         M33.1030
      IF ( NUMUM .GT. 0 )  IT1 = IBR                                    M33.1031
      IF ( NUMUM .EQ. 0 )  IBR = IT1                                    M33.1032
      LSTAT(22) = IBR                                                   M35.1293
13000 N1 = NCHAIN                                                       M22.3562
      IF ( KBASE .EQ. 0 )  GO TO 13010                                  M32.3967
      REWIND LUNIT9                                                     M32.3968
      WRITE (LUNIT9)  NTOT, NENERG                                      M32.3969
      IF (JFLSOS .GT. 0)  WRITE (LUNIT9) PU                             M35.1294
      INECHO = LUNT12                                                   M32.3970
      REWIND LUNT12                                                     M39.1798
13010 IF ( KBASE .GE. 2  .AND.  KBASE .NE. INTINF )   GO TO 156         M32.3971
      IF (KTAB .EQ. 0) GO TO 156                                        M22.3564
      INDSTP = 1                                                        M22.3565
      write (*,*) ' Over12 ready for TACS.  newtac =', newtac
      if ( newtac .ne. 1 )
     1 CALL TACS2                                                        M22.3566
      if ( newtac .eq. 1 )
     1 call ntacs2
      write (*,*) ' Done with TACS, back in over12.  ioutin =', ioutin
      IOUT = IOUTIN                                                     M22.3567
      IF (IPRSUP .GE. 1)                                                M22.3568
     1     WRITE (LUNIT6, 2719)  IOUT, INDSTP, MULTPR, KPRCHG           M22.3569
 2719 FORMAT(/, 28H IN 'OVER12' AFTER 'TACS2' .,                        M37.4104
     1  33H    IOUT  INDSTP  MULTPR  KPRCHG  ,/,  31X, 4I8  )           M33.1033
      IF (KILL .GT. 0) GO TO 9200                                       M22.3572
      IF (NCHAIN .NE. N1) GO TO 9800                                    M33.1034
  156 IFTAIL = 0                                                        M22.3574
      IF ( KBASE  .EQ.  0 )   GO TO 598                                 M23.2047
      IF ( KBASE .NE. INTINF ) CALL TABLES                              M28.4297
      IF (KTAB .EQ. 0) IPROUT = 0                                       M22.3577
      IF (IPRSUP .GE. 1)                                                M22.3582
     1     WRITE (LUNIT6, 2721) N4, NWORD1, NWORD2, LTLABL,             M27.1099
     2     KTAB, ISW, LUNIT2, IV, LBRNCH                                M39.1799
 2721 FORMAT ( /,  27H UNIT 2 READ/WRITE INDICES.,                      M25. 323
     1  32H      N4  NWORD1  NWORD2  LTLABL,                            M25. 324
     2  32H    KTAB     ISW  LUNIT2      IV,                            M25. 325
     3   8H  LBRNCH  ,/,  27X,  9I8  )                                  M39.1800
      REWIND LUNT10                                                     M25. 327
      N8 = 0                                                            M25. 328
      DO 7256  I=1, KSWTCH                                              M25. 329
      IF ( AKEY(I)  .NE.  -44444. )   GO TO 7256                        M25. 330
      N8 = N8 + 1                                                       M25. 331
 7256 CONTINUE                                                          M25. 332
      WRITE (LUNT10)  N8                                                M25. 333
      DO 7259  I=1, KSWTCH                                              M25. 334
      IF ( AKEY(I)  .NE.  -44444. )   GO TO 7259                        M25. 335
      J = I + LSWTCH                                                    M32.3975
      WRITE (LUNT10)  I, AKEY(I), TSTAT(I), TOPEN(J)                    M32.3976
 7259 CONTINUE                                                          M25. 337
      IF ( KBASE .NE. INTINF )  GO TO 7263                              M28.4298
      KNT = 1                                                           M28.4300
      GO TO 166                                                         M28.4301
 7263 NENERG = 0                                                        M28.4302
      IF ( IPRSUP  .GE.  1 )                                            M13.1441
     1 WRITE (LUNIT6, 4624)  KNT, KBASE, KLOAEP, MTAPE                  M17. 608
      IF ( IPRSUP  .GE.  4 )                                            M13.1443
     1 WRITE (LUNIT6, 76)  KSWTCH, KLOAEP, ( KDEPSW(IP), IP=1, LL6 )    M23.2053
   76 FORMAT (/, 6H AT 76, 2I10, /, 1X, 6I5)                            M13.1445
      DO 42630  J = 1, KSWTCH                                           M13.1446
      IF ( ABSZ(AKEY(J)) .NE. 44444.)   GO TO 42630                     M24. 622
      N = KDEPSW(J)                                                     M13.1448
      IF ( N  .EQ.  0 )   GO TO 42300                                   M13.1449
      TMA = TCLOSE(N)                                                   M43.1158
      IF (AKEY(N) .EQ. -44444.) TMA = TOPEN(N)                          M43.1159
      TMB = TCLOSE(J)                                                   M43.1160
      IF (AKEY(J) .EQ. -44444.) TMB = TOPEN(J)                          M43.1161
      TMC = TMA + TMB                                                   M43.1162
      IF (AKEY(J) .EQ.  44444.) TCLOSE(J) = TMC                         M43.1163
      IF (AKEY(J) .EQ. -44444.) TOPEN(J) = TMC                          M43.1164
      GO TO 42500                                                       M13.1451
42300 IF ( KLOAEP  .LE.  0 )  GO TO 42500                               M32.3979
      TMT = TCLOSE(KLOAEP)                                              M43.1165
      IF ( AKEY(KLOAEP) .EQ. -44444.) TMT = TOPEN(KLOAEP)               M43.1166
      IF ( AKEY(J) .EQ. 44444. )  TCLOSE(J) = TMT                       M43.1167
      IF ( AKEY (J) .EQ. -44444. )  TOPEN(J) = TMT                      M43.1168
42500 CRIT(J) = 0.0                                                     M32.3982
      IF ( AKEY(J)  .EQ.  -44444. )   GO TO 42630                       M32.3983
      TOPEN(J) = FLTINF                                                 M13.1455
      AKEY(J) = 0.0                                                     M32.3984
42630 CONTINUE                                                          M13.1457
      IF (IPRSUP .LT. 1)  GO TO  598                                    M32.3985
      WRITE (LUNIT6, 52630)  (TCLOSE(J), J=1, KSWTCH)                   M32.3986
52630 FORMAT (31H AT 52630, TCLOSE AFTER SORTING  ,/,(1X,8E15.6))       M19.1158
      WRITE (LUNIT6, 5263)  (TOPEN(J), J=1, KSWTCH)                     M32.3987
 5263 FORMAT (29H AT 5263, TOPEN AFTER SORTING  ,/,(1X,8E15.6))         M32.3988
      GO TO 598                                                         M13.1458
 4088 II = 0                                                            M14.1389
      WRITE (LUNIT3)  (BUS(I), I= 1, NTOT)                              M17. 610
      DO 4098 I = 1 , KSWTCH                                            M32.3989
      J = LSWTCH + I                                                    M32.3990
 4098 WRITE (LUNIT3) KMSWIT(I), KMSWIT(J), AKEY(I), TSTAT(I),           M32.3991
     1 TOPEN(J), KDEPSW(I)                                              M32.3992
      WRITE (LUNIT3) KLOAEP                                             M32.3993
      NCOMPT = 2 * (SIGMAX/AINCR +FLZERO)                               M14.1393
      NHALF = NCOMPT / 2                                                M14.1394
      N3 = 0                                                            M14.1395
      DO 4090  I = 1, KSWTCH                                            M14.1396
      IF ( ABSZ(AKEY(I)) .NE. 44444.)   GO TO 4090                      M24. 624
      N3 = N3 + 1                                                       M14.1398
 4090 CONTINUE                                                          M14.1399
      N4 = NBYTE(3) / NBYTE(4)                                          M22.3589
      IOFCNT = N4 * NCOMPT                                              M14.1401
      IOFTAB = N4 * 3 * NCOMPT                                          M14.1402
      MXPAIR = N3 * (N3 - 1) / 2                                        M14.1403
      N2 = (3 + MXPAIR) * NCOMPT                                        M14.1404
      IF ( KBURRO  .EQ.  1 )   GO TO 4093                               M22.3590
      N1 = LOCINT( KSWTYP(1) )  -  LOCINT( IRANDN(1) )                  M32.3994
      CALL MOVE0  (IRANDN(1), N1)                                       M22.3592
      GO TO 4097                                                        M22.3593
 4093 CALL MOVE0 ( IRANDN(1), LSIZ23 )                                  M22.3594
      N1 = LSIZ23                                                       M30. 216
 4097 IF ( IPRSUP  .GE.  1 ) GO TO 4095                                 M32.3995
      GO TO 4099                                                        M32.3996
 4095 WRITE (LUNIT6, 2345)                                              M32.3997
 2345 FORMAT ( /, 20H AT 2345 OF OVER12  )                              M32.3998
      DO 4096 I = 1 , KSWTCH                                            M32.3999
      J = LSWTCH + I                                                    M32.4000
 4096 WRITE (LUNIT6, 61234) KMSWIT(I), KMSWIT(J), AKEY(I),              M32.4001
     1 TSTAT(I), TOPEN(J)                                               M32.4002
61234 FORMAT   (1X,2I6,3E15.6)                                          M32.4003
 4099 IF (N1 .GE. N2)   GO TO 4100                                      M32.4004
      KILL = 151                                                        M14.1408
      LSTAT(19) = 4090                                                  M14.1409
      LSTAT(13) = NCOMPT                                                M14.1410
      LSTAT(14) = N1                                                    M14.1411
      LSTAT(15) = N2                                                    M14.1412
      LSTAT(16) = N3                                                    M14.1413
      GO TO 9200                                                        M14.1414
 4100 DO 4120  L=1, NCOMPT                                              M14.1415
      IF (L .GT. NHALF)   GO TO 4110                                    M14.1416
      FRANDN(L) = - AINCR * (NHALF - L)                                 M22.3598
      GO TO 4120                                                        M14.1418
 4110 FRANDN(L) = AINCR * (L - NHALF)                                   M22.3599
 4120 CONTINUE                                                          M14.1420
  409 IF (NENERG .LT. 0)   GO TO 188                                    M13.1459
      CALL TIME44(ATIM(1))                                              M14.1421
      CALL RUNTYM (D1,D2)                                                  10660
      SEED = SEEDY(ATIM(1)) + 1000.*(D1+D2)                                10661
      SEED = 2.0*SEED                                                      10662
      IF ( JSEEDR  .GE.  NENERG )   SEED = SEEDR                        M09.  12
      D8 = RANDNM(SEED)                                                 M22.3600
      IF ( IPRSUP  .GE.  1 )                                            M10. 215
     1 WRITE (LUNIT6, 646)  KNT, KSWTCH, JSEEDR, NENERG, ITEST, IDIST,  M17. 614
     2 SEED, D8                                                         M22.3601
  646 FORMAT ( /,  48H IN  =OVER12=  JUST AFTER 1ST USE OF  =RANDNM= . ,M10. 218
     1 /, 1X, 48H     KNT  KSWTCH  JSEEDR  NENERG   ITEST   IDIST,      M10. 219
     2 16X, 4HSEED, 18X,  2HD8  ,/,  1X,  6I8,  2E20.10 )               M22.3602
      BIAS = 0.0
      IF ( ITEST .EQ. 1 )  GO TO 1646
      IF ( KNT .GT. 1 )  GO TO 1615
      NSWTPE = 0
      DO 1605  I = 1, KSWTCH
      IF ( ABSZ(AKEY(I)) .NE. 44444. )  GO TO 1605
      IF ( AKEY(I) .EQ. 44444. )  GO TO 1607
 1605 CONTINUE
      GO TO 1608
 1607 NSWTPE = NSWTPE + 1
 1608 DO 1610  I = 1, KSWTCH
      IF ( ABSZ(AKEY(I)) .NE. 44444. )  GO TO 1610
      IF ( AKEY(I) .EQ. -44444. )  GO TO 1612
 1610 CONTINUE
      GO TO 1615
 1612 NSWTPE = NSWTPE + 1
 1615 SECFRQ = (1.0/STATFR)/360.                          
      WINDOW = (DEGMAX-DEGMIN) * SECFRQ                                 M01.  23
      IF(WINDOW.LT.0.0) WINDOW = 360. * SECFRQ                          M01.  24
      ZEROFL = 0.0                                                      M11. 126
      D14 = KNT                                                         M42.6578
      D14 = D14 / NENERG                                                M42.6579
      DO 1620  J = 1, NSWTPE
      IF ( LINSYS .NE. 1 )                                              M42.6580
     1 D14 = RANDNM ( ZEROFL )                                          M42.6581
      RANOFF = D14 * WINDOW   +   DEGMIN * SECFRQ                       M42.6582
C     write (*,*) ' knt, nenerg, window, degmin, secfrq, d14, ranoff =',
C    1              knt, nenerg, window, degmin, secfrq, d14, ranoff
      IF ( J .GT. 1 )  GO TO 1619
C     BIAS = RANOFF                                                     M32.4006
      DUM9 = RANOFF
      ANGLE = RANOFF * 360. * STATFR
      GO TO 1620
 1619 BIAS2 = RANOFF
      ANGTPE = RANOFF * 360. * STATFR
 1620 CONTINUE
 1646 DO  700  KSW = 1, KSWTCH                                          M13.1462
      IF ( ABSZ(AKEY(KSW))  .NE. 44444.)   GO TO 700                    M24. 625
      NDXI = KSW + LSWTCH                                               M32.4008
      IF ( ITEST .EQ. 1 )  go to 664
      if ( itest .ge. 2 )  go to 1663
      if ( itest .eq. 0  .and. nswtpe .lt. 2 )  go to 1630
      if ( akey(ksw) .eq. 44444. )  go to 1630
      bias = bias2
      go to 664
 1630 bias = DUM9
      go to 664
 1663 if( ITEST .GT. 2 )  GO TO  662          
      IF ( AKEY(KSW) .EQ. +44444. ) GO TO 663                           M32.4011
      BIAS = 0.0                                                        M32.4012
      GO TO 664                                                         M32.4013
  663 BIAS = RANOFF                                                     M32.4014
      GO TO 664                                                         M32.4015
  662 IF ( AKEY(KSW) .EQ. -44444. )  GO TO 663                          M32.4016
      BIAS = 0.0                                                        M32.4017
  664 TIMEV = TSTAT(KSW)                                                M32.4018
      N = KDEPSW(KSW)                                                   M13.1465
      IF ( KLOAEP  .EQ.  0 )   GO TO 665                                M13.1466
      IF ( N  .GT.  0 )   GO TO 665                                     M13.1467
      TIMEV = TSTAT(KLOAEP)                                             M13.1468
  665 RNG = RANDNM(ZEROFL)                                              M11. 128
      IF ( IPRSUP  .GE.  2 )                                            M10. 221
     1 WRITE (LUNIT6, 666 )  KSW, RNG, BIAS, ANGLE                      M32.4019
  666 FORMAT ( /,  49H AT 666  OF  "OVER12" .   KSW, RNG, BIAS, ANGLE =,M32.4020
     1 1I10, 3E16.6  )                                                  M10. 224
      IF ( IDIST  .GT.  0 )  GO TO 675                                  M13.1469
      IF( TOPEN(NDXI) .LT. 0.0 ) GO TO 675                              M32.4021
      D9 = RNG                                                          M22.3603
      IF ( D9  .EQ.  0.0 )   GO TO 665                                  M22.3604
      IF ( D9  .EQ.  1.0 )   GO TO 665                                  M22.3605
      IF ( 2.0*D9  .GT.  1.0 )   D9 = 1.0 - D9                          M22.3606
      D7 = ALOGZ( 1.0/(D9*D9) )                                         M22.3607
      EH = SQRTZ(D7)                                                    M22.3608
      XN=2.515517+EH*(0.802853+0.010328*EH)                                10683
      XD=1.0+EH*(1.432788+EH*(0.189269+0.001308*EH))                       10684
      XQ=EH-XN/XD                                                          10685
      IF ( XQ  .GT.  SIGMAX )                                           M13.1470
     1 XQ = SIGMAX                                                      M13.1471
      IF ( 2.0*RNG  .LT.  1.0 )   XQ = -XQ                              M05. 486
      RNG = TIMEV + TOPEN(NDXI)*XQ + BIAS                               M32.4022
      GO TO 680                                                            10688
  675 D1 = 3.0                                                          M12. 427
      RNG = TIMEV + BIAS + SQRTZ(D1)*ABSZ(TOPEN(NDXI))*(2.0*RNG - 1.0)  M32.4023
  680 IF ( AKEY(KSW) .EQ. -44444. )   GO TO 7680                        M32.4024
      TCLOSE(KSW) = RNG                                                 M32.4025
      IF ( N .EQ. 0) GO TO 700                                          M13.1473
      IF ( AKEY(N) .EQ.  44444. )                                       M43.1170
     1 TCLOSE(KSW) = TCLOSE(N) + TCLOSE(KSW)  -  BIAS                   M43.1171
      IF ( AKEY(N) .EQ. -44444. )                                       M43.1172
     1 TCLOSE(KSW) = TOPEN(N) + TCLOSE(KSW)  -  BIAS                    M43.1173
      IF (TCLOSE(KSW).LT.DELTAT) GO TO 690                                 10695
      GO TO 700                                                         M13.1475
 7680 TOPEN(KSW) = RNG                                                  M32.4026
      IF ( N .EQ. 0) GO TO 700                                          M43.1174
      IF ( AKEY(N) .EQ.  44444. )                                       M43.1175
     1 TOPEN(KSW) = TCLOSE(N) + TOPEN(KSW)  -  BIAS                     M43.1176
      IF ( AKEY(N) .EQ. -44444. )                                       M43.1177
     1 TOPEN(KSW) = TOPEN(N) + TOPEN(KSW)  -  BIAS                      M43.1178
      IF (TOPEN(KSW).LT.DELTAT) GO TO 690                               M32.4029
      GO TO 700                                                         M32.4030
  690 CALL TIME44(ATIM(1))                                                 10697
      CALL RUNTYM (D1,D2)                                                  10698
      SEED = SEEDY(ATIM(1)) + 1000.*(D1+D2)                                10699
      SEED = 2.0*SEED                                                      10700
      D8 = RANDNM(SEED)                                                 M22.3609
      RNG = RANDNM(ZEROFL)                                              M11. 129
      LTDELT=LTDELT+1                                                      10703
      IF ( AKEY(KSW) .EQ. 44444. )  TCLOSE(KSW) = 0.0                   M32.4031
      IF ( AKEY(KSW) .EQ. -44444. )  TOPEN(KSW) = 0.0                   M32.4032
  700 CONTINUE                                                             10707
      IF ( MONCAR(7) .LE. 0 )  GO TO 300                                M37.4105
C     NOW READ USER-SUPPLIED TCLOSE OF "USER SUPPLIED SWITCH TIMES"     M37.4106
      N17 = MONCAR(7)                                                   M37.4107
      READ (N17, 4372)  ( TCLOSE(KSW), KSW=1, KSWTCH )                  M37.4108
      READ (N17, 4372)  (  TOPEN(KSW), KSW=1, KSWTCH )                  M37.4109
 4372 FORMAT ( 5E15.6 )                                                 M37.4110
  300 WRITE (LUNIT3)  RANOFF, (TCLOSE(K1), K1 = 1, KSWTCH)              M32.4033
      WRITE (LUNIT3)  RANOFF, (TOPEN(K1), K1 = 1, KSWTCH)               M32.4034
      IF ( IPRSUP  .GE.  1 )                                            M13.1477
     1 WRITE (LUNIT6, 4624)  KNT, KBASE, KLOAEP, MTAPE, NENERG, JSEEDR, M17. 617
     2 LTDELT, KSWTCH,  SEEDR, RANOFF,                                  M32.4035
     3 ( TCLOSE(K1), K1=1, KSWTCH )  , ( TOPEN(K), K = 1, KSWTCH )      M32.4036
 4624 FORMAT ( /,  23H AT 4624 OF  'OVER12' .  ,                        M13.1481
     1 64H     KNT   KBASE  KLOAEP   MTAPE  NENERG  JSEEDR  LTDELT  KSWTM13.1482
     2CH   ,  10X, 5HSEEDR,  9X, 6HRANOFF,  /, 23X, 8I8, 2E15.5, /,     M32.4037
     3 ( 4X, 8E16.6 ) )                                                 M27.1100
      IF (ISW .NE. 4444)   GO TO 703                                    M14.1422
      NPAIR = 0                                                         M14.1423
      IRBASE = IOFTAB                                                   M14.1424
      IF (IDIST .GT. 0)   GO TO 840                                     M14.1425
      DO 830  J = 1 , KSWTCH                                            M14.1426
      IF ( ABSZ(AKEY(J)) .NE. 44444.)   GO TO 830                       M24. 626
      K = J + LSWTCH                                                    M32.4038
      IF (TOPEN(K) .LT. 0)   GO TO 830                                  M32.4039
      IF (KLOAEP .EQ. 0)  GO TO 752                                     M14.1429
      IF (KDEPSW(J) .EQ. 0)  GO TO 751                                  M14.1430
      TSTATJ = TSTAT(J) + TSTAT(KLOAEP)                                 M14.1431
      GO TO 754                                                         M14.1432
  751 TSTATJ = TSTAT(KLOAEP)                                            M14.1433
      GO TO 754                                                         M14.1434
  752 IF (KDEPSW(J) .EQ. 0)  GO TO 753                                  M14.1435
      N = KDEPSW(J)                                                     M14.1436
      TSTATJ = TSTAT(J) + TSTAT(N)                                      M14.1437
      GO TO 754                                                         M14.1438
  753 TSTATJ = TSTAT(J)                                                 M14.1439
  754 J1 = J - 1                                                        M14.1440
      DO 820  I=1, J1                                                   M14.1441
      IF ( ABSZ ( AKEY(I) ) .NE. 44444.)   GO TO 820                    M24. 627
      IF ( I .EQ. J )  GO TO 820                                        M14.1443
      NI = I + LSWTCH                                                   M32.4040
      IF ( TOPEN(NI) .LT. 0 )  GO  TO 820                               M32.4041
      NPAIR = NPAIR + 1                                                 M14.1445
      TDIFF = TCLOSE(I) - TCLOSE(J)                                     M32.4042
      IF ( KLOAEP .EQ. 0 )  GO TO 762                                   M14.1447
      IF ( KDEPSW(I) .EQ. 0)   GO TO 761                                M14.1448
      TSTATI = TSTAT(I) + TSTAT(KLOAEP)                                 M14.1449
      GO TO 764                                                         M14.1450
  761 TSTATI = TSTAT(KLOAEP)                                            M14.1451
      GO TO 764                                                         M14.1452
  762 IF (KDEPSW(I) .EQ. 0)   GO TO 763                                 M14.1453
      N = KDEPSW(I)                                                     M14.1454
      TSTATI = TSTAT(I) + TSTAT(N)                                      M14.1455
      GO TO 764                                                         M14.1456
  763 TSTATI = TSTAT(I)                                                 M14.1457
  764 TMEAN = TSTATI - TSTATJ                                           M14.1458
      TDIFF = TDIFF - TMEAN                                             M14.1459
      SUMDT = SQRTZ(TOPEN(NI)*TOPEN(NI) + TOPEN(K) *TOPEN(K))           M32.4043
      FSIGMA = SIGMAX * SUMDT                                           M14.1461
      IF (ABSZ(TDIFF) .LE. FSIGMA)   GO TO 800                          M18. 693
      II = II + 1                                                       M14.1463
      GO TO 820                                                         M14.1464
  800 SUMDT = SUMDT * AINCR                                             M14.1465
      K1 = TDIFF/SUMDT                                                  M32.4044
      IF (IPRSUP .GE. 1)                                                M14.1467
     1WRITE (LUNIT6, 803)  NPAIR, TDIFF, SUMDT, K1                      M32.4045
  803 FORMAT (22H NPAIR,TDIFF,SUMDT,K1=, 10X, I6, 2E15.6, I6)           M32.4046
      IF (TDIFF .LT. 0.0)   GO TO 805                                   M14.1470
      K1 = K1 + NHALF + 1                                               M32.4047
      GO TO 810                                                         M14.1472
  805 K1 = K1 + NHALF                                                   M32.4048
  810 K1 = K1 *N4 - 1                                                   M32.4049
      N6 = IRBASE + K1                                                  M32.4050
      IRANDN(N6) = IRANDN(N6) + 1                                       M22.3610
      IRBASE = IRBASE + IOFCNT                                          M27.1103
  820 CONTINUE                                                          M14.1476
  830 CONTINUE                                                          M14.1477
      KNT = KNT + 1                                                     M14.1478
      IF ( KNT  .LE.  NENERG )   GO TO 409                              M22.3611
  840 CONTINUE                                                          M14.1481
      NPAIR = 0                                                         M14.1482
      IRBASE = IOFTAB                                                   M14.1483
      DO 895  J = 1, KSWTCH                                             M14.1484
      IF ( ABSZ(AKEY(J)) .NE. 44444.)   GO TO 895                       M24. 628
      J1 = J - 1                                                        M14.1486
      DO 890  I = 1, J1                                                 M14.1487
      IF ( ABSZ(AKEY(I)) .NE. 44444.)   GO TO 890                       M24. 629
      IF (I .EQ. J)   GO TO 890                                         M14.1489
      NPAIR = NPAIR + 1                                                 M14.1490
      K1 = KMSWIT(J)                                                    M32.4051
      NDX1 = LSWTCH + J                                                 M32.4052
      K2 = KMSWIT(NDX1)                                                 M32.4053
      K3 = KMSWIT(I)                                                    M32.4054
      NDX2 = LSWTCH + I                                                 M32.4055
      K4 = KMSWIT(NDX2)                                                 M32.4056
      BUS1 = BUS(K1)                                                    M14.1495
      BUS2 = BUS(K2)                                                    M14.1496
      BUS3 = BUS(K3)                                                    M14.1497
      BUS4 = BUS(K4)                                                    M14.1498
      WRITE (LUNIT6, 845)  NPAIR, BUS1, BUS2, BUS3, BUS4                M17. 619
  845 FORMAT (//, 12H SWITCH PAIR, I4, 15X, 3H  ', A6, 8H'  TO  ', A6,  M14.1500
     1 11H'   AND   ', A6, 8H'  TO  ', A6, 1H')                         M14.1501
      IF (IDIST .GT. 0)   GO TO 850                                     M14.1502
      IF (TOPEN(NDX1) .LT. 0)   GO TO 850                               M32.4057
      IF (TOPEN(NDX2) .LT. 0)   GO TO 850                               M32.4058
      GO TO 858                                                         M14.1505
  850 WRITE (LUNIT6, 855)                                               M17. 620
  855 FORMAT(129H IN THIS PAIR EITHER ONE OR BOTH SWITCH CLOSING TIMES AM14.1507
     1RE GENERATED ACCORDING TO THE UNIFORM DISTRIBUTION AND IT WILL BE M14.1508
     2SKIPPED.)                                                         M14.1509
      GO TO 890                                                         M14.1510
  858 N3 = NCOMPT- 1                                                    M14.1511
      DO 868  L = 1, N3                                                 M14.1512
      K = L + 1                                                         M14.1513
      K1 = K * N4 - 1                                                   M27.1104
      L1 = L * N4 - 1                                                   M27.1105
      N7 = IRBASE + K1                                                  M27.1106
      N8 = IRBASE + L1                                                  M27.1107
      IRANDN(N7) = IRANDN(N8) + IRANDN(N7)                              M22.3612
      TOTAL = NENERG                                                    M14.1515
      M = NCOMPT + K                                                    M27.1108
      FRANDN(M) = IRANDN(N7) / TOTAL                                    M22.3613
      D9 = (K - NHALF) * AINCR                                          M22.3614
      AZ = ABSZ ( D9 )                                                  M22.3615
      T = 1.0 / (1.0 + .2316419 * AZ)                                   M14.1520
      D1 = .3989423 * EXPZ ( -D9*D9*ONEHAF )                            M22.3616
      D8 = 1.0 - D1* T * ((((1.330274 *T -1.821256) *                   M22.3617
     1     T + 1.781478) * T                                            M43.1179
     2  - .3565638) * T + .3193815 )                                    M14.1523
      IF ( D9  .GE.  0.0 )  GO TO 864                                   M22.3618
      D8 = 1.0 - D8                                                     M22.3619
  864 N9 = M + NCOMPT                                                   M16.2359
      FRANDN(N9) = D8                                                   M22.3620
  868 CONTINUE                                                          M14.1527
      IRBASE = IRBASE + IOFCNT                                          M27.1109
      N1 = 1                                                            M14.1529
      N2 = 14                                                           M14.1530
      N3 = 2 * NCOMPT                                                   M14.1531
  869 IF (N2 .GT. NCOMPT)   N2 = NCOMPT                                 M14.1532
      WRITE (LUNIT6, 870)  (FRANDN(L), L= N1, N2)                       M22.3621
  870 FORMAT (1X, 4HTIME, 10X, 14F8.4)                                  M14.1534
      N6 = N1 + NCOMPT                                                  M16.2361
      N7 = N2 + NCOMPT                                                  M16.2362
      WRITE (LUNIT6, 871)  (FRANDN(L), L = N6, N7)                      M22.3622
  871 FORMAT (1X, 6HSAMPLE, 8X, 14F8.4)                                 M14.1536
      N8 = N1 + N3                                                      M16.2364
      N9 = N2 + N3                                                      M16.2365
      WRITE (LUNIT6, 872)  (FRANDN(L), L = N8, N9)                      M22.3623
  872 FORMAT (1X,11HTHEORETICAL,3X, 14F8.4,/,1X)                        M22.3624
      N1 = N1 + 14                                                      M14.1539
      IF (N1 .GT. NCOMPT)   GO TO 890                                   M14.1540
      N2 = N2 + 14                                                      M14.1541
      GO TO 869                                                         M14.1542
  890 CONTINUE                                                          M14.1543
      IF (II .EQ. 0)   GO TO 895                                        M14.1544
      WRITE (LUNIT6, 893)  II, NENERG, SIGMAX                           M17. 624
  893 FORMAT (/, I6, 7H OUT OF, I6, 36H SWITCH CLOSING TIMES FALLING BEYM14.1546
     1OND, F8.4, 30H TIMES THE STANDARD DEVIATION.)                     M14.1547
  895 CONTINUE                                                          M14.1548
      LSTAT(32) = KSWTCH                                                M14.1549
      XMAXMX = 1.0                                                      M14.1550
      AINCR = 1.0                                                       M14.1551
      NUMNVO = 1                                                        M14.1552
      NC = 1                                                            M14.1553
      CALL RUNTYM ( D1, D2 )                                            M21.  87
      FLSTAT(5) = FLSTAT(5) + D1                                        M21.  88
      FLSTAT(6) = FLSTAT(6) + D2                                        M21.  89
      FLSTAT(9) = D1                                                    M21.  90
      FLSTAT(10) = D2                                                   M21.  91
      LASTOV = 12                                                       M25. 338
      NCHAIN = 29                                                       M14.1554
      IF ( IPRSUP  .GE.  1 )                                            M32.4059
     1 WRITE ( LUNIT6, 4568 )                                           M32.4060
      GO TO 99999                                                       M14.1555
  703 IF (ISW .EQ. 0)   GO TO 702                                       M14.1556
      IF(KNT.EQ.1) GO TO 702                                               10709
      N6 = 0                                                            M32.4062
      DO 4216  K1 =1, KSWTCH                                            M32.4063
      IF ( IABS( KPOS(K1) )  .EQ.  2 )   GO TO 4208                     M32.4064
      IF ( AKEY(K1) .NE.  44444. )   GO TO 4216                         M32.4065
      VOLTI(N6+1) = TCLOSE(K1)                                          M32.4066
      GO TO 4213                                                        M32.4067
 4208 IF ( AKEY(K1) .NE. -44444. )   GO TO 4216                         M32.4068
      VOLTI(N6+1) = TOPEN(K1)                                           M32.4069
 4213 N6 = N6 + 1                                                       M32.4070
      IF ( N6  .LT.  2*LSIZ26 )   GO TO 4215                            M32.4071
      LSTAT(19) = 4213                                                  M32.4072
      IPRINT = 26                                                       M32.4073
      GO TO 9200                                                        M32.4074
 4215 LASTXX(N6) = K1                                                   M32.4075
 4216 CONTINUE                                                          M32.4076
      WRITE (LUNIT6, 701) KNT, (LASTXX(K1), VOLTI(K1), K1=1, N6 )       M32.4077
  701 FORMAT (/, 32X, 46HRANDOM SWITCHING TIMES FOR ENERGIZATION NUMBER,M32.4078
     1 I4, /,  ( 32X,   5(I4, E16.6)   )   )                            M01.  31
  702 IF (KNT .EQ. 1)   GO TO 598                                       M38.2385
      N9 = IPNTV(11)                                                    M32.4079
      IF ( N9  .LE.  0 )  GO TO 6666                                    M35.1295
      DO 2702  I = 1, N9                                                M32.4080
      READ (LUNIT5, 1702)  D1                                           M32.4081
 1702 FORMAT (A1)                                                       M32.4082
 2702 CONTINUE                                                          M32.4083
 6666 LASTOV = 12                                                       M35.1296
      NCHAIN = 16                                                          10715
      GO TO 9800                                                        M33.1035
  188 KLOROW = IPNTV(1)                                                 M13.1487
      ILOROW = 1                                                        M13.1488
      IF (KLOAEP .EQ. 0)   GO TO 190                                    M13.1489
      KLOROW = KLOAEP                                                   M13.1490
      ILOROW = ILOAEP                                                   M13.1491
  190 NJ = KLOROW + LSWTCH                                              M33.1036
      NS2 = TDNS(KLOROW)                                                M33.1037
      IF (NUMREF .LE. 1)   GO TO 201                                    M13.1493
      IF (KNT .EQ. 1)   INDTV(ILOROW) = 1                               M13.1494
      J = 0                                                             M13.1495
  198 J = J + 1                                                         M13.1496
      IF (J .NE. ILOROW)   GO TO 195                                    M13.1497
      IF (J .EQ. NUMREF)   GO TO 201                                    M13.1498
      GO TO 199                                                         M13.1499
  195 INDTV(J) = INDTV(J) + 1                                           M13.1500
      N1 = IPNTV(J)                                                     M19.1159
      NS1 = TDNS(N1)                                                    M19.1160
      IF (INDTV(J) .LE. NS1)   GO TO 200                                M13.1502
      INDTV(J) = 1                                                      M13.1503
      IF (J .EQ. NUMREF)   GO TO 201                                    M13.1504
      GO TO 199                                                         M13.1505
  201 INDTV(ILOROW) = INDTV(ILOROW) + 1                                 M13.1506
      IF (INDTV(ILOROW) .GT. NS2)   GO TO 702                           M13.1507
      GO TO 200                                                         M13.1508
  199 IF (J .LT. NUMREF )   GO TO 198                                   M13.1509
  200 TSTBEG = TSTAT( KLOROW )                                          M33.1038
      IF (ITEST .GT. 0)   GO TO 203                                     M33.1039
      TSTBEG = TSTBEG - 0.5 *                                           M33.1040
     1 ( TDNS(KLOROW) - 1.0 ) * TOPEN(NJ)                               M43.1180
  203 IF ( AKEY(KLOROW) .EQ. 44444. )                                   M33.1041
     1 TCLOSE(KLOROW) = TSTBEG +                                        M32.4089
     2 (INDTV(ILOROW) - 1) * TOPEN(NJ)                                  M43.1181
      IF ( AKEY(KLOROW) .EQ. -44444. )                                  M32.4090
     1 TOPEN(KLOROW) = TSTBEG +                                         M32.4091
     2 (INDTV(ILOROW) - 1) * TOPEN(NJ)                                  M43.1182
      K = 1                                                             M33.1042
      DO 230  I = 1, KSWTCH                                             M13.1516
      IF ( K  .NE.  ILOROW )   GO TO 207                                M13.1517
      K = K + 1                                                         M13.1518
      IF ( INDTV(K)  .EQ.  0 )  INDTV(K) = 1                            M13.1519
  207 IF ( I  .EQ.  KLOROW )   GO TO 230                                M13.1520
      IF (IPNTV(K) .EQ. 0)   GO TO 210                                  M13.1521
      IF (I .EQ. IPNTV(K))   GO TO 220                                  M13.1522
  210 IF ( ABSZ(AKEY(I)) .NE. 44444.)   GO TO 230                       M24. 630
      N = KDEPSW(I)                                                     M13.1524
      IF ( AKEY(I) .EQ. 44444. )  TCLOSE(I) = TCLOSE(N) + TSTAT(I)      M32.4096
      IF ( AKEY(I) .EQ. -44444. )  TOPEN(I) = TOPEN(N) + TSTAT(I)       M32.4097
      GO TO 230                                                         M13.1526
  220 J = I + LSWTCH                                                    M32.4098
      IF (KLOAEP .EQ. 0)   GO TO 225                                    M32.4099
      N2 = IPNTV(K)                                                     M19.1161
      IF ( AKEY(I) .EQ. 44444. )                                        M32.4100
     1 TCLOSE(I) = TCLOSE(KLOAEP) + TOPEN(J)*                           M33.1043
     2 (INDTV(K)-.5-.5*TDNS(N2))                                        M43.1183
      IF ( AKEY(I) .EQ. -44444. )                                       M32.4102
     1 TOPEN(I) = TOPEN(KLOAEP) + TOPEN(J)*                             M33.1044
     2 (INDTV(K)-.5-.5*TDNS(N2))                                        M43.1184
      GO TO 228                                                         M13.1529
  225 TSTBEG = TSTAT(I)                                                 M33.1045
      IF (ITEST .GT. 0)   GO TO 227                                     M33.1046
      TSTBEG = TSTAT(I) - 0.5 * ( TDNS(I) - 1.0 ) * TOPEN(J)            M33.1047
  227 IF ( AKEY(I) .EQ. 44444. )                                        M33.1048
     1 TCLOSE(I) = TSTBEG + (INDTV(K) - 1) * TOPEN(J)                   M32.4106
      IF ( AKEY(I) .EQ. -44444. )                                       M32.4107
     1 TOPEN(I) = TSTBEG + (INDTV(K) - 1) * TOPEN(J)                    M32.4108
  228 K = K + 1                                                         M13.1535
      IF (INDTV(K) .EQ. 0)   INDTV(K) = 1                               M13.1536
  230 CONTINUE                                                          M13.1537
      GO TO 300                                                         M13.1538
C                                                         PREPARING R,X,   10717
  598 D2 = 1.0 / (DELTAT * 500. )                                       M13.1539
      IF ( M4PLOT .NE. 1 )  GO TO 5763                                          
      IF ( KBRSER .NE. 1 )  GO TO 5763                                          
      N24 = KBREAK                                                              
      KBRSER = 2                                                                
      KBREAK = 1                                                                
      CALL EMTSPY                                                               
      KBREAK = N24                                                              
 5763 IF ( IPRSUP  .LT.  2 )  GO TO 6300                                M35.1304
      WRITE (LUNIT6,6)   NENERG                                         M32.4114
    6 FORMAT (/, 32H AT S.N. 6 OF OVER12.   NENERG =, I3 )              M32.4115
      DO 6200 I = 1 , KSWTCH                                            M32.4116
      NDX3 = LSWTCH + I                                                 M32.4117
 6200 WRITE (LUNIT6, 9) KMSWIT(I), KMSWIT(NDX3), AKEY(I),               M32.4118
     1 TSTAT(I), TOPEN(NDX3)                                            M32.4119
    9 FORMAT (1X, 2I6, 3E15.6)                                          M32.4120
 6300 IF( NUMSM  .LE.  0 )   GO TO 901                                  VAX. 102
      CALL ELECyy
      CALL PREMEC                                                       M20.3266
  901 IF(IT.EQ.0) GO TO 433                                             M35.1307
      IF( IPRSUP .GE. 1 )                                                  10723
     1 WRITE(LUNIT6, 65435)  ( TR(I), TX(I), R(I), C(I), I=1, IT )      M17. 628
65435 FORMAT ( /,  39H LUMPED-ELEMENT BRANCH-PARAMETER VALUES,          M35.1308
     1             34H IN "OVER12", PRIOR TO PROCESSING.   ,/,          M35.1309
     2 18X, 2HTR, 18X, 2HTX, 19X, 1HR, 19X, 1HC, /,                        10727
     1 ( 1X, 4E20.10 ) )                                                   10728
      K = 1                                                                     
 5527 J = LENGTH(K)                                                             
      XOPT = XOPTBR(K)                                                  VAX. 103
      COPT = COPTBR(K)                                                  VAX. 104
      D22 = D2                                                          VAX. 105
      CI1 = D2 / 1000.                                                  VAX. 106
      IF( XOPT .GT. 0.0 )  D22 = D2 * 1000. / ( TWOPI * XOPT )          VAX. 107
      IF( COPT .GT. 0.0 )  CI1 = CI1 / ( TWOPI * COPT )                 VAX. 108
C     IF (KODSEM(K) .NE. 0  .AND. IMODEL(K) .NE. -2                     M32.4122
C    1                      .OR.  IMODEL(K) .NE. -4) J = KODEBR(K)      M43.1185
      IF (KODSEM(K) .NE. 0  .AND. IMODEL(K) .GE. 0 ) J = KODEBR(K)      M43.1186
      IF( J .EQ. 0 )  J = 1                                                10731
      J = IABS(J)                                                               
      IF( KBUS(K) .LT. 0 )                                                 10733
     1                    GO TO 5539                                            
      IF( KODEBR(K) .GT. 0 )  GO TO 5539                                   10734
      IF( LENGTH(K) .LT. 0 )  GO TO 5539                                   10735
      N1 = IABS( NR(K) )                                                   10736
      N2 = N1 - 1  +  J*(J+1)/2                                            10737
      IF( IPRSUP .GE. 2 )                                                  10738
     1 WRITE(LUNIT6, 5528)  K, LENGTH(K), NR(K), N1, N2, D22            VAX. 109
 5528 FORMAT( /,   9H AT 5528.   , 5I10, E15.5 )                           10740
C     CHECK FOR THE PRESENCE OF THE -666 BRANCHES  *   *   *   *   *   *M32.4123
      IF( LENGTH(K+1) .NE. -666 ) GO TO 905                             M32.4124
  903 CALL DTEQIV( IKF, ISFD, D22, AZR, AZI )                           VAX. 110
C     CONVERT MODAL IMPEDANCES TO PHASE FRAME OF REFERENCE *   *   *   *M32.4126
      CZ = J                                                            M34. 133
      AZR = ( AZR - AZI ) / CZ                                          M34. 134
      LB = N1                                                           M32.4129
      KA = 1                                                            M32.4130
C     LOAD THE EQUIVALENT IMPEDANCES INTO THE  TR  TABLES  *   *   *   *M32.4131
      DO  904  KB = N1, N2                                              M32.4132
         TR(KB) = AZR                                                   M32.4133
         TX(KB) = 0.                                                    M32.4134
         IF( LB .NE. KB ) GO TO 904                                     M32.4135
         TR(LB) = TR(LB) + AZI                                          M32.4136
         KA = KA + 1                                                    M32.4137
         LB = LB + KA                                                   M32.4138
  904 CONTINUE                                                          M32.4139
  905 DO  5534  I = N1, N2                                              M32.4140
      C(I) = C(I) * CI1                                                 VAX. 111
      X(I) = TR(I)  +  TX(I) * D22                                      VAX. 112
 5534 TX(I) = -X(I)                                                        10743
 5539 K = K + J                                                            10744
      IF( K .LE. IBR )  GO TO 5527                                         10745
      IF( IPRSUP .GE. 1 )                                               M32.4141
     1 WRITE(LUNIT6, 5543)  ( X(I), C(I), I=1, IT )                     M17. 630
 5543 FORMAT ( /,  ' (X(I), C(I), I=1, IT)   AFTER CONVERSION',            10748
     1             ' TO  R + 2*L/DELTAT .'   ,/, ( 1X, 8E16.7 ) )          10749
  433 IF ( KSWTCH  .EQ.  0 )   GO TO 430                                M32.4142
      IF ( IPRSUP  .GE.  1 )   GO TO 455                                M32.4143
      GO TO 477                                                         M32.4144
  455 WRITE (LUNIT6, 2666)                                              M32.4145
 2666 FORMAT ( /,  68H SWITCH TABLE VECTORS, BEFORE SWITCH TABLE PROCESSM32.4146
     1ING IN  'OVER12' .       ,/,                                      M32.4147
     2 40H     ROW   BUS1    BUS2     KPOS  ISOURC,  14X,               M37.4111
     3 6HTCLOSE,  15X,  5HTOPEN,  16X,  4HCRIT,  14X,                   M32.4149
     4 6HFKPOSI )                                                       M32.4150
      DO 466 K = 1, KSWTCH                                              M32.4151
      NDX4 = LSWTCH + K                                                 M32.4152
  466 WRITE (LUNIT6,2713) K, KMSWIT(K), KMSWIT(NDX4), KPOS(K),          M32.4153
     1 ISOURC(K), KSWTYP(K), TCLOSE(K), TOPEN(K), CRIT(K)               M32.4154
 2713 FORMAT  ( 6I8,  3E20.10 )                                         M32.4155
  477 IF ( IPRSUP  .GE.  1 )                                            M32.4156
     1 WRITE (LUNIT6, 2714)  ( K, ENERGY(K),                            M32.4157
     1 ADELAY(K), K=1, KSWTCH )                                         M32.4158
 2714 FORMAT ( /, 38H CONTINUATION OF SWITCH TABLE VECTORS.  ,/,        M25. 342
     1 5X, 3HROW, 9X, 6HENERGY,  9X,                                    M32.4159
     2 9X, 6HADELAY, /, ( I8, 2E15.6 ) )                                M32.4160
      DO 422 K=1,KSWTCH                                                    10768
      N1 = KMSWIT(K)                                                    M32.4161
      NDX5 = LSWTCH + K                                                 M32.4162
      N2 = KMSWIT(NDX5)                                                 M32.4163
      ICHECK = KSWTYP(K)                                                M32.4164
      IF ( ABSZ(ADELAY(K)) .EQ. 44444.)   GO TO 422                     M24. 631
      IF(ADELAY(K).LE.0.) GO TO 428                                     M32.4165
      ADELAY(NDX5) = ADELAY(K)                                          M32.4166
      GO TO 422                                                         M32.4167
  428 IF(ICHECK.LE.0) GO TO 422                                            10820
      IF(ISTEAD.EQ.0) GO TO 422                                            10821
      IF (ICHECK .EQ. 9976)  GO TO 422                                  M23.2056
      NN1=KBUS(ICHECK)                                                     10822
      N15 = NN1                                                         M31.2699
      IF ( N15  .EQ.  1 )   N15 = N2                                    M31.2700
      N16 = IABS ( KSSFRQ(N15) )                                        M31.2701
      OMEGA = TWOPI * SFREQ(N16)                                        M31.2702
      CK1=(F(NN1)-F(N2))/OMEGA                                             10823
      IF ( NOUTPR  .EQ.  0 )                                            M20.3278
     1 WRITE(LUNIT6,427) BUS(NN1),BUS(N2),CK1                           M20.3279
  427 FORMAT( 23H INITIAL FLUX IN COIL ', A6, 6H' TO ', A6,                10825
     1 3H' =, E13.5 )                                                      10826
      CK(ICHECK)=CK1-(E(NN1)-E(N2))*DELTA2                                 10827
      IF(ABSZ(CK1).GT.TOPEN(K)     .AND.                                M20.3280
     1     NOUTPR  .EQ.  0 )                                            M20.3281
     2 WRITE(LUNIT6,426)                                                M20.3282
  426 FORMAT ( 132H WARNING.  ASSUMPTION THAT AC STEADY STATE HAS FUNDAM   10830
     1ENTAL FREQUENCY ONLY IS QUESTIONABLE WITH PRECEDING FLUX OUTSIDE     10831
     2LINEAR REGION)                                                       10832
  422 CONTINUE                                                             10833
C                                      INVERSION OF MATRICES OF COUPLED    10834
C                                           PRESETTING TABLES FOR LOSSLE   10835
  430 K=1                                                                       
      ITADD = IT + 1                                                            
      N1 = IBR+1                                                        M14.1557
      IF ( N1  .LT.  10 )   N1 = 10                                             
      IF ( IPRSUP  .GE.  2 )                                            M14.1559
     1 WRITE (LUNIT6, 5306)  ( I, KBUS(I), MBUS(I), LENGTH(I),          M17. 640
     1                    KODEBR(I), KODSEM(I), INDHST(I),              M23.2058
     2                    CIK(I), CKI(I),CKKJM(I), I=1, N1 )            M31.2703
 5306 FORMAT ( /,  56H     ROW    KBUS    MBUS  LENGTH  KODEBR  KODSEM  M14.1563
     1INDSEM, 12X, 3HCIK, 12X, 3HCKI, 10X, 5HCKKJM,/, ( 7I8, 3E15.6 ) ) M31.2704
      N5=K                                                              M14.1565
      ILINE=1                                                              10837
      IPRINT=8                                                             10838
  500 IF(KBUS(K).LT.0) GO TO 535                                                
      IT2=LENGTH(K)                                                             
      IF(IT2.LT.0) GO TO 520                                                    
      I=NR(K)                                                                   
      IF(IT2.LE.1) GO TO 510                                                    
      IF( KODEBR(K) .LE. 0 )  GO TO 4460                                   10844
      L = NR(K)                                                            10845
      JT =  IT2 * (IT2+1) / 2                                              10846
      CALL MOVER( TX(L),CMR(1),JT )                                             
      D1 = -1.0 / DELTA2                                                   10848
      CALL ADDMXD( CMR(1), D1, CMR(1), IT2 )                                    
      CALL REDUCT( CMR(1), IT2, LL0 )                                   M22.3628
      II = L + JT - 1                                                      10851
      IF( IPRSUP .GE. 2 )                                                  10852
     1WRITE(LUNIT6, 4435)  ( CMR(I), I=1, JT )                          M17. 641
 4435 FORMAT( /,  31H AT 4435.  INTERMED. MATRIX (D)  ,/, ( 1X,6E20.10))   10854
      CALL MULTMX( CMR(1), TR(L), X(L), CMI(1), IT2 )                      10855
      CALL MULTMX( CMR(1), TX(L), R(L), CMI(1), IT2 )                      10856
      CALL ADDMXD( R(L),  ONE , R(L), IT2 )                             M22.3629
      DO 4442  I=L, II                                                     10858
 4442 R(I) = 2.0 * R(I)                                                    10859
      IF( IPRSUP .GE. 2 )                                                  10860
     1WRITE(LUNIT6, 4451)  K, L, IT2, ( X(I), R(I), I=L, II )           M17. 642
 4451 FORMAT( /, 40H AT 4451.  FINAL (G), (H) FROM (A), (B).   ,           10862
     1 3I10, /, ( 1X, 6E20.10) )                                           10863
      GO TO 520                                                            10864
 4460 CALL REDUCT ( TX(I), IT2, LL0 )                                   M22.3630
      II=I-1                                                               10866
      N2=1                                                                      
  501 DO 502 L=1,IT2                                                            
      IF(L.LE.N2) GO TO 504                                                     
      N1=N1+L-1                                                                 
      GO TO 502                                                                 
  504 N1=II+L                                                                   
  502 VOLT(L)=TX(N1)                                                       10873
      CALL MULT(TR(I),VOLT,VOLTI,IT2,LL0)                               M22.3631
      CALL MULT(TX(I),VOLTI,VOLT,IT2,LL0)                               M22.3632
      DO 503 L=1,N2                                                        10876
      II=II+1                                                              10877
  503 R(II)=(TX(II)-VOLT(L))*2.0                                           10878
      N2=N2+1                                                              10879
      IF(N2.LE.IT2) GO TO 501                                              10880
      GO TO 520                                                            10881
  530 I=IABS(I)                                                            10882
      GUS1=C(I)                                                         M05. 490
      IF(GUS1.NE.0.) GUS1=1.0/GUS1                                      M05. 491
      GUS2=1.0/(X(I)+GUS1)                                              M05. 492
      R(I)=GUS2*(TR(I)*2.0-X(I)+GUS1)                                   M05. 493
      X(I)=GUS2                                                         M05. 494
      C(I)=GUS1                                                         M05. 495
      GO TO 520                                                            10889
  535 IT2=IABS(LENGTH(K))                                                       
      IF ( KODSEM(K) .EQ. 0  .OR.                                       M43.1189
     1     IMODEL(K) .EQ. -2 ) GO TO 5349                               M43.1190
      IF ( KODSEM(K) .NE. 0                                             M43.1191
     1     .AND.  IMODEL(K) .EQ. -4 ) GO TO 5349                        M44. 403
      N1 = K                                                            M23.2060
      N3 = 1                                                            M23.2061
 5350 IF (CKI(N1) .LT. 0.0)                                             M23.2062
     1  GO TO 5353                                                              
      N1 = N1 + 1                                                               
      N3 = N3 + 1                                                               
      GO TO 5350                                                        M23.2065
 5353 N2 = K + N3 - 1                                                           
      LENGTH(N2) = N3                                                   M23.2067
      IT2 = IABS(KODEBR(K))                                             M23.2068
      N1 = K + IT2 - 1                                                          
      N2 = ITADD                                                        M23.2070
      DO 5348 I=K, N1                                                           
      NR(I) = N2                                                        M23.2072
      N2 = N2 + 1                                                       M23.2073
 5348 CONTINUE                                                          M23.2074
      K = K + N3 - IT2                                                          
      GO TO 520                                                         M23.2076
 5349 A = IT2                                                                   
      II = 1                                                            M16.2400
  534 IF (CK(K) .GE. 0.0) GO TO 6482                                            
      KF = NR(K)                                                        M22.3634
      CIK(K) = KF                                                       M22.3635
      I = NHIST(KF)                                                     M22.3636
      NR(K) = I                                                         M22.3637
      IF( IPRSUP .GE. 2 )                                               M22.3638
     1 WRITE(LUNIT6, 20601)  KF, I, K, II, ILINE, IT2, KBUS(K), MBUS(K) M22.3639
20601 FORMAT ( /,  22H FREQ. DEPENDENT MODE.,                           M22.3640
     1 32H      KF       I       K      II,                             M22.3641
     2 32H   ILINE     IT2    KBUS    MBUS  ,/,  22X,  8I8  )           M22.3642
      IF( ISKIP(KF) .GT. 0 )                                            M22.3643
     1                    GO TO 537                                             
      H2 = 0.0                                                          M22.3644
      GO TO 11536                                                               
 6482 H2 = CIK(K)                                                               
      H3 = H2 / DELTAT                                                  M23.2079
      I = H3                                                            M05. 496
CL                                                                      M31.2706
      IF(IMODEL(K).EQ.-2) GO TO 50001                                   M31.2707
      IF ( IMODEL(K) .EQ. -4 ) GO TO 50006                              M43.1195
      GO TO 50005                                                       M31.2708
C                                                                       M43.1196
50006 I=-2                                                              M43.1197
      INOFF1 = LBRNCH                                                           
      INOFF2 = 2 * LBRNCH                                                       
      INOFF3 = 3 * LBRNCH                                                       
      INOFF4 = 4 * LBRNCH                                                       
      INOFF5 = 5 * LBRNCH                                                       
      KOFF1 = 900                                                               
      KOFF2 = KOFF1 +ISECTI                                                     
      KOFF3  = KOFF2  +ISECTI                                                   
      KOFF4  = KOFF3  +ISECTI                                                   
      KOFF5  = KOFF4  +ISECTI                                                   
      KOFF6  = KOFF5  +ISECTI                                                   
      KOFF7  = KOFF6  +ISECTI                                                   
      KOFF8  = KOFF7  +ISECTI                                                   
      KOFF9  = KOFF8  +ISECTI                                                   
      KOFF10 = KOFF9  +ISECTI                                                   
      KOFF13 = KOFF10 +ISECTI                                           M43.1209
      KOFF14 = KOFF13 +ISECTI                                           M43.1210
      KOFF15 = KOFF14 +ISECTI                                           M43.1211
      KOFF16 = KOFF15 +ISECTI                                           M43.1212
      KOFF17 = KOFF16 +ISECTI                                                   
      KOFF18 = KOFF17 +ISECTI                                                   
      KOFF19 = KOFF18 +ISECTI                                                   
      KOFF20 = KOFF19                                                           
      IF (JGLNN  .NE. 0 ) GO TO 8893                                    M44. 409
      DO 1007 JLK =   1 , IBR                                           M44. 410
      IF ( KODSEM(JLK) .EQ. 0 ) GO TO 1007                              M44. 411
      NWWW =   LENGTH(JLK)                                              M44. 412
      NQTW = NQTW +  NWWW **2                                           M44. 413
      NPH = NPH + NWWW                                                  M44. 414
      JGLNN = JGLNN + 1                                                 M44. 415
 1007 CONTINUE                                                          M44. 416
      IF( NPH .le. ISECTI ) go to 8891                                  M44. 418
      WRITE( lunit6, 8892) ISECTI, NPH   
 8892 FORMAT( ' In OVER12, offset variable ISECTI =', I5, 
     1 ' is too samll.  It should be increased to the value of ',I5,
     2 /, 'which is the number of coupled phases currently modelled',
     3 ' with new MARTI frequency-dependent format.     STOP.')
      STOP                                                              M44. 422
 8891 DO 1988 IQ =1, NQTW                                                       
      NQTT = NQTT +  WK1(KOFF20+IQ)                                             
 1988 CONTINUE                                                          M43.1222
 8893 KOFF21 = KOFF20 + NQTT                                                    
      KOFF22 = KOFF21 + LBRNCH                                                  
      KOFF23 = KOFF22 + NQTW                                                    
      KOFF24 = KOFF23 + NQTW                                                    
      KOFF25 = KOFF24 + 288                                             M44. 423
      NR(K)=I                                                                   
      N1 = KODSEM(K)                                                            
      NRZ = CKI(K)                                                              
      NRA = CKKJM(K)                                                            
      N8 = N1 + 5*NRZ + 2*NRA+1                                                 
      N9 = N8 + 1                                                               
      SCONST(N9) = SCONST(N1)                                                   
      SCONST(N8) = 0.0                                                  M43.1237
      NN8 = N1+5*NRZ+5*NRA+3                                                    
      NN9 = NN8+1                                                               
      NK1 = NN9 + 1                                                     M44. 424
      SCONST(NN8) = 0.0                                                 M43.1240
      SCONST(NN9) = 0.0                                                 M43.1241
      DO 50007  J = 1, NRZ                                                      
      N2 = N1 + J                                                               
      N3 = N2 + NRZ                                                             
      FAC1 = SCONST(N3) * DELTAT                                                
      FAC2 = SCONST(N2) / SCONST(N3)                                            
      N5 = N1 + 2*NRZ + 2*NRA + J                                               
      SCONST(N5) = EXPZ(-FAC1)                                                  
      HI = (ONE - SCONST(N5))/FAC1                                              
      N6 = N5 + NRZ                                                             
      SCONST(N6) = FAC2 * (ONE - HI)                                            
      N7 = N6 + NRZ                                                             
      SCONST(N7) = -FAC2 * (SCONST(N5) - HI)                                    
      SCONST(N8) = SCONST(N8) + SCONST(N7)                                      
      SCONST(N9) = SCONST(N9)                                           M43.1255
     1      + SCONST(N6)                                                        
      WK1(KOFF21+K) = SCONST(N9)                                        M43.1257
      IF ( IPRSUP .GT. 0 )                                              M43.1258
     1 write(*,5487)J,NRZ,SCONST(N5),SCONST(N6),                        M43.1259
     2         SCONST(N7),SCONST(N8),SCONST(N9)                         M43.1260
5487  FORMAT(30H J,NRZ,CJ,DJ,EJ,ESUM,D0SUM=Y05,                      
     1 /,1X,I3,2X,I3,3X,E14.5,3X,E14.5,3X,E14.5,                        M43.1262
     2                        3X,E14.5,3X,E14.5)                        M43.1263
50007 CONTINUE                                                          M43.1264
      JGL = 0                                                           M44. 425
C     IF ( IPRSUP .GT. 0 )                                              M44. 426
C    1 write(LUNIT6,4445)                                          
C  445  FORMAT(19H   J  NRA  SCST(N5),                
C    130H  SCST(N6)  SCST(N7)  SST(NN8),                                M44. 429
C    330H  SCT(NN9)  ST(NN10)  ST(NN11),                                M44. 430
C    430H  ST(NN12)  ST(NN13)  ST(NN14),                                M44. 431
C    530H  ST(NN15)      FAC3      FAC4,                                M44. 432
C    630H      FAC5      SK1R      SK1I,                                M44. 433
C    710H       SWD)                                                    M44. 434
      DO 50008 J = 1, NRA                                                       
      N2 = N1 + 2*NRZ + J                                                       
      N3 = N2 + NRA                                                             
      N5 = N1 + 5*NRZ + 2 *NRA + 2 + J                                          
      N6 = N5 + NRA                                                             
      N7 = N6 + NRA                                                             
      IF ( ABSZ( SCONST(N2) ) .GE. 1.E+13 ) GO TO 5324                  M44. 435
      IF ( ABSZ( SCONST(N2) ) .EQ. 0.0 ) GO TO 5324                     M44. 436
      FAC1 = SCONST(N3) * DELTAT                                                
      FAC2 = SCONST(N2) / SCONST(N3)                                            
      SCONST(N5) = EXPZ(-FAC1)                                                  
      HI = (ONE - SCONST(N5)) / FAC1                                            
      SCONST(N6) = FAC2 * ( ONE - HI )                                          
      SCONST(N7) = -FAC2*(SCONST(N5)-HI)                                        
      SCONST(NN8)=SCONST(NN8)+SCONST(N7)                                        
      SCONST(NN9)=SCONST(NN9)+SCONST(N6)                                        
      GO TO 5325                                                        M44. 437
 5324 JGL =  JGL + 1                                                    M44. 438
      IF ( JGL .EQ. 2) GO TO 5486                                       M44. 439
      FAC1 = SCONST(N3)*DELTAT/1.E15                                            
      SCONST(N5) = EXPZ(-FAC1)                                                  
      DBLPR2 =  SCONST(N5)                                              M44. 442
      FAC3 =- SCONST(N3+1) * DELTAT / 1.E15                                     
      FAC4 = SCONST(N3+1) / 1.E15                                               
      FAC5 = SCONST(N3) / 1.E15                                                 
      CALL DCOSZ(FAC3, DBLPR3)                                                  
      CALL DSINZ(FAC3, DBLPR4)                                                  
      DBLPR1 = DBLPR2 * DBLPR3                                                  
      DBLPR2 = DBLPR2 * DBLPR4                                                  
      D18 = FAC4*FAC4 + FAC5*FAC5                                               
      NN10 = NK1  + 1                                                           
      NN11 = NN10 + 1                                                           
      NN12 = NN11 + 1                                                           
      NN13 = NN12 + 1                                                           
      NN14 = NN13 + 1                                                           
      NN15 = NN14 + 1                                                           
      NK1 = NK1 + 6                                                     M44. 457
      SK1R = SCONST(N2)/1.E+15                                          M44. 458
      SK1I = SCONST(N2+1)/1.E+15                                        M44. 459
      RLL = (  (UNITY - DBLPR1) * FAC5 - DBLPR2 * FAC4 )                M44. 460
      SLL =- (  DBLPR2 * FAC5 + (UNITY - DBLPR1) * FAC4 )               M44. 461
      YLL = SK1R * RLL - SK1I * SLL                                             
      XLL = SK1R * DBLPR1 - SK1I * DBLPR2                               M44. 463
      SCONST(N5) = DBLPR1                                               M44. 464
      SCONST(N6) = SK1R - YLL/(DELTAT*D18)                              M44. 465
      SCONST(N7) = -( XLL - YLL/(DELTAT*D18) )                          M44. 466
      SCONST(NN8)=SCONST(NN8)+SCONST(N7)                                        
      SCONST(NN9)=SCONST(NN9)+SCONST(N6)                                        
C 5325 IF ( IPRSUP .GT. 0 )                                             
C    1 write(*,5488)J,NRA,SCONST(N5),SCONST(N6),                        M43.1280
C    2       SCONST(N7),SCONST(NN8),SCONST(NN9),                        M43.1281
C    3   SCONST(NN10), SCONST(NN11), SCONST(NN12),                      M44. 469
C    4   SCONST(NN13), SCONST(NN14), SCONST(NN15),                      M44. 470
C    5   FAC3, FAC4, FAC5, SK1R, SK1I, SWD                              M44. 471
C  488  FORMAT(1X,I3,2X,I3, 17E10.3 )                                   
 5325 IF ( ABSZ(SCONST(N2)) .LE. 1.E+14 ) GO TO 50008                  
      IF ( JGL .EQ. 1 ) GO TO 50008                                     M44. 473
5486  JGL = 0                                                           M44. 474
50008 CONTINUE                                                                  
      GO TO 5335                                                        M43.1286
C                                                                       M43.1287
50001 I=-2                                                              M31.2709
      NR(K)=I                                                                   
      N1 = KODSEM(K)                                                            
      NRZ = CKI(K)                                                              
      NRA = CKKJM(K)                                                            
      N8 = N1 + 5*NRZ + 2*NRA + 1                                       M31.2714
      N9 = N8 + 1                                                       M31.2715
      SCONST(N9) = SCONST(N1)                                                   
      SCONST(N8) = 0.0                                                  M31.2717
      NN8 = N1 + 5*NRZ + 5*NRA + 3                                      M31.2718
      NN9 = NN8 + 1                                                     M31.2719
      SCONST(NN8) = 0.0                                                 M31.2720
      SCONST(NN9) = 0.0                                                 M31.2721
      DO 50003  J = 1, NRZ                                                      
      N2 = N1 + J                                                               
      N3 = N2 + NRZ                                                             
      FAC1 = SCONST(N3) * DELTAT                                                
      FAC2 = SCONST(N2) / SCONST(N3)                                            
      N5 = N1 + 2*NRZ + 2*NRA + J                                               
      SCONST(N5) = EXPZ(-FAC1)                                                  
      HI = (ONE - SCONST(N5))/FAC1                                              
      N6 = N5 + NRZ                                                             
      SCONST(N6) = FAC2 * (ONE - HI)                                            
      N7 = N6 + NRZ                                                             
      SCONST(N7) = -FAC2 * (SCONST(N5)-HI)                                      
      SCONST(N8) = SCONST(N8) + SCONST(N7)                                      
      SCONST(N9) = SCONST(N9) + SCONST(N6)                                      
50003 CONTINUE                                                                  
      DO 50004 J = 1, NRA                                                       
      N2 = N1 + 2*NRZ + J                                               M31.2738
      N3 = N2 + NRA                                                     M31.2739
      FAC1 = SCONST(N3) * DELTAT                                        M31.2740
      FAC2 = SCONST(N2) / SCONST(N3)                                            
      N5 = N1 + 5*NRZ + 2 *NRA + 2 + J                                  M31.2742
      SCONST(N5) = EXPZ(-FAC1)                                                  
      HI = (ONE - SCONST(N5)) / FAC1                                    M31.2744
      N6 = N5 + NRA                                                             
      SCONST(N6) = FAC2 * ( ONE - HI )                                  M31.2746
      N7 = N6 + NRA                                                     M31.2747
      SCONST(N7) = -FAC2 * ( SCONST(N5) - HI)                                   
      SCONST(NN8) = SCONST(NN8)                                         M31.2749
     1                        + SCONST(N6)                                      
      SCONST(NN9) = SCONST(NN9)                                         M31.2750
     1                        + SCONST(N7)                                      
50004 CONTINUE                                                                  
c                                                                       M43.1290
c     Should put the calculation of Ti integer here using               M43.1291
c     same way as Zc & A1 had been done above                           M43.1292
c                   LJG. April 20, 1987                                 M43.1293
c                                                                       M43.1294
      CI(K)=ONE/(SCONST(N9)*A)                                                  
      CK(K)=1.0                                                         M31.2753
      CIK(K)=1.0                                                        M31.2754
      GO TO 5335                                                        M31.2755
50005 IF(ISTEAD.GT.0) GO TO 531                                                 
      IF (H2 .LE. TMAX) GO TO 531                                               
      I = -2                                                                    
  531 N4 = H3                                                                   
      D1 = N4                                                                   
      CIK(K) = H3 - D1                                                          
      IF(I.NE.0) GO TO 537                                                      
11536 KILL = 29                                                         M12. 431
      LSTAT(19) = 531                                                      10907
      FLSTAT(16) = H2                                                      10908
12536 N1 = IABS( KBUS(K) )                                                      
      N2 = IABS( MBUS(K) )                                              M12. 433
      BUS1 = BUS(N1)                                                    M12. 434
      BUS2 = BUS(N2)                                                    M12. 435
      GO TO 9200                                                           10911
  537 IF(II.EQ.1)                                                               
     1 LENGTH(K)=ILINE                                                          
      ILINE=ILINE+I+2                                                           
      NR(K) = I                                                                 
 5537 IF (ILINE .LT. LPAST) GO TO 536                                           
      LSTAT(19) = 537                                                      10916
      GO TO 9000                                                           10917
  536 IF (CK(K) .GE. 0.0)                                               M23.2082
     1 GO TO 5074                                                               
      CK(K) = -1.0                                                              
      YX = ZINF(KF)                                                             
      GO TO 533                                                         M22.3682
 5074 CK1 = CK(K) * ONEHAF                                                      
      YX=CI(K)                                                                  
      IF(YX.LT.0.) GO TO 532                                                    
      YX=YX+CK1 / 2.0                                                           
      CK(K)=(YX-CK1)/YX                                                         
      IF ( CK(K)  .GT.  0.0 )   GO TO 533                               M12. 436
      KILL = 102                                                        M12. 437
      LSTAT(19) = 536                                                   M12. 438
      LSTAT(15) = K                                                     M12. 439
      FLSTAT(14) = CK1 * 2.0                                            M12. 440
      FLSTAT(15) = YX - 0.5*CK1                                         M12. 441
      GO TO 12536                                                       M12. 442
  532 CK(K)=EXPZ(CK1/YX)                                                        
  533 CI(K)=1.0/(YX*A)                                                          
 5331 IF ( IPRSUP  .LT.  4 )   GO TO 5335                               M16.2507
      IF (NOUTPR .EQ. 0)                                                M20.3292
     1 WRITE (LUNIT6, 5334)  K, II, LENGTH(K), ILINE, IT2, I, N3, H3,   M20.3293
     2                       CIK(K)                                     M20.3294
 5334 FORMAT ( /,  35H AFTER PROCESSING NEW MODE OF LINE.   ,/,         M16.2509
     1 56H       K      II  LENGTH   ILINE     IT2       I      N3, 13X,M16.2510
     2 2HH3,  9X,  6HCIK(K)   ,/,  7I8,  2E15.6  )                      M16.2511
 5335 IF ( II .LT. IT2 )                                                        
     1   GO TO 5375                                                             
      IT2 = 1                                                           M16.2513
      GO TO 520                                                                 
 5375 IF ( II  .GT.  1 ) GO TO 538                                              
      LENGTH(K+1)=LENGTH(K)                                                     
      LENGTH(K)=-IT2                                                            
  538 II=II+1                                                                   
      K=K+1                                                                     
      GO TO 534                                                                 
  510 IF(I.LT.0) GO TO 530                                                      
      TX(I)=1.0/X(I)                                                            
      R(I)=2.0*(X(I)-TR(I))/X(I)**2                                             
C                                                                       M43.1298
C                                                                       M43.1299
  520 IF ( KBUS(K) .GE. 0 )  GO TO 81520                                M31.2757
      IF ( KODSEM(K) .NE. 0  .AND. IMODEL(K) .EQ. -4)  GO TO 5698       M44. 475
      IF ( KODSEM(K) .NE. 0  .AND. IMODEL(K) .NE. -2)  GO TO 81515      M31.2758
 5698 ITADD = ITADD + II * (II+1) / 2                                   M31.2759
      GO TO 81520                                                       M31.2760
81515 ITADD = ITADD + IT2 * (IT2+1)/2                                           
81520 K = K + IABS(IT2)                                                         
      N5 = K                                                            M14.1656
      IF(K.LE.IBR) GO TO 500                                                    
      LASTOV = 12                                                               
      NCHAIN = 13                                                               
      IF ( IPRSUP  .LE.  2 )   GO TO 9800                               M33.1049
      WRITE (LUNIT6, 208)   ( K, KBUS(K), MBUS(K), NR(K), KODEBR(K),    M17. 651
     1                 LENGTH(K), KODSEM(K), INDHST(K), K=1, IBR )      M16.2544
  208 FORMAT ( /,  47H BRANCH-TABLE INTEGER VECTORS AT END 'OVER12' . , M16.2545
     1 /,   64H     ROW    KBUS    MBUS      NR  KODEBR  LENGTH  KODSEM M16.2546
     2 INDHST   ,/,  ( 8I8 ) )                                          M16.2547
      WRITE (LUNIT6, 209)   ( K, BUS(K), CI(K), CK(K), CIK(K), CKI(K),  M17. 652
     1                   CKKJM(K), K=1, IBR )                           M31.2763
  209 FORMAT ( /, 5X, 3HROW, 7X, 3HBUS, 13X, 2HCI, 13X, 2HCK,           M16.2550
     1 12X, 3HCIK, 12X, 3HCKI,10X,5HCKKJM,/,( I8,4X,A6,5E15.6 ) )       M31.2764
      WRITE (LUNIT6, 4394) (SCONST(I), I=1, IFSEM)                      M23.2086
 4394 FORMAT(/, 24H (SCONST(I), I=1, IFSEM)   ,/, 8(1X,E15.7))          M23.2087
      WRITE (LUNIT6, 4395) (CNVHST(I), I=1, IFX)                        M23.2088
 4395 FORMAT(/,22H (CNVHST(I), I=1, IFX)   ,/, 8(1X,E15.7))             M23.2089
      GO TO 9800                                                        M33.1050
 9000 LSTAT(16) = IPRINT                                                   10957
      KILL = 1                                                             10958
 9200 LASTOV = 12                                                       M25. 346
      NCHAIN = 51                                                       M15. 866
      LSTAT(18) = 12                                                       10960
 9800 IF ( IPRSUP  .GE.  1 )  WRITE ( LUNIT6, 4568 )                    M33.1051
 4568 FORMAT ( 22H EXIT MODULE "OVER12".  )                             M33.1052
99999 RETURN                                                               10961
      END                                                                  10962
      SUBROUTINE DTEQIV( IKF, ISFD, D2, AZR, AZI )                      M32.4174
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION  UR(2)                                                  M32.4177
C     THIS ROUTINE PRODUCES THE COMPANION MODEL TO BE INSERTED INTO THE M32.4178
C     TR TABLES. IT ALSO NORMALIZES THE L AND C DATA TO HENRY AND FARAD M32.4179
      IDK = 2 * IKF                                                     M32.4180
      IKF = IKF + 1                                                     M32.4181
      IF( IPRSUP .GT. 0 )                                               M32.4182
     1    WRITE( LUNIT6, 1 ) IKF, ISFD, IMFD(IDK+1), IMFD(IDK+2)        M32.4183
    1 FORMAT(41H INTEGER COUNTERS AT START OF DTEQIV.....,17X,3HIKF,    M32.4184
     16X,4HISFD,6X,4HIZKF,6X,4HIPKF,/,51X,4I10)                         M32.4185
      D21 = D2 * DELTA2                                                 M32.4186
      DO  4  KB = 1, 2                                                  M32.4187
         UR(KB) = 0.                                                    M32.4188
         KC = KB + IDK                                                  M32.4189
         ISF = IMFD(KC)                                                 M32.4190
         IST = ISFD + 1                                                 M32.4191
         ISU = ISFD + ISF * 5                                           M32.4192
C     PROCESS BRANCH DATA FOR THE APPROPRIATE MODE *   *   *   *   *   *M32.4193
         DO  3  KA = IST, ISU, 5                                        M32.4194
            AR1 = RMFD(KA)                                              M32.4195
            ARL = RMFD(KA+3)                                            M32.4196
C     NORMALIZE L AND C DATA   *   *   *   *   *   *   *   *   *   *   *M32.4197
            AC1 = RMFD(KA+2) * CI1                                      M32.4198
            AL1 = RMFD(KA+1) * D2                                       M32.4199
            RMFD(KA+1) = RMFD(KA+1) * D21                               M32.4200
            RMFD(KA+2) = AC1 * DELTA2                                   M32.4201
            IF( AC1 .NE. 0.0 ) AC1 = 1. / AC1                           M32.4202
C     CALCULATE BRANCH IMPEDANCE   *   *   *   *   *   *   *   *   *   *M32.4203
C     PROCESS THE PARALLEL R AND L CONNECTION  *   *   *   *   *   *   *M32.4204
            A1 = AL1                                                    M32.4205
            IF( ARL .EQ. 0. .OR. AL1 .EQ. 0. ) GO TO 3                  M32.4206
            A1 = ( AL1 * ARL ) / ( AL1 + ARL )                          M32.4207
C     ACCUMULATE BRANCH ADMITTANCE *   *   *   *   *   *   *   *   *   *M32.4208
    3       UR(KB) = UR(KB) + 1. / ( AR1 + AC1 + A1 )                   M32.4209
         ISFD = ISU                                                     M32.4210
C     INVERT THE EQUIVALENT BRANCH ADMITTANCE  *   *   *   *   *   *   *M32.4211
    4    UR(KB) = 1. / UR(KB)                                           M32.4212
      AZR = UR(1)                                                       M32.4213
      AZI = UR(2)                                                       M32.4214
      IF( IPRSUP .GT. 0 )                                               M32.4215
     1    WRITE( LUNIT6, 5 ) AZR, AZI                                   M32.4216
    5 FORMAT(37H EQUIVALENT BRANCH IMPEDANCES (MODAL),4X,2E22.10)       M32.4217
      RETURN                                                            M32.4218
      END                                                               M32.4219
      SUBROUTINE REDUCT(A,N,M)                                             11019
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THIS SUBROUTINE CAN BE USED FOR EITHER PARTIAL REDUCTION OR FOR   M15. 913
C)    COMPLETE INVERSION OF A REAL  N BY N  SYMMETRIC MATRIX  'A' .     M15. 914
C)    STORAGE FOR MATRIX ELEMENTS IS IN THE IMPLIED ORDER   (1,1),      M15. 915
C)    (1,2), (2,2), (1,3), (2,3), (3,3), (1,4),  ETC.,  FOR THE         M15. 916
C)    COMPLETE UPPER TRIANGLE.   VARIABLES  M+1, ...., N  ARE TO BE     M15. 917
C)    ELIMINATED (WHICH IS THE DEFINITION OF VARIABLE  'M', NOTE).      M15. 918
C)    RESULT IS REDUCED MATRIX IN COLUMNS 1,...M IN CASE OF REDUCTION   M15. 919
C)    (M UNEQUAL 0) OR NEGATIVE INVERSE MATRIX IN COLUMNS 1,...N IN CASEM15. 920
C)    OF INVERSION (M=0).                                               M15. 921
      DIMENSION  A(1), B(100)                                              11029
      J = N + 1                                                            11030
      W=1.0                                                                11031
      IF(M.GT.0) W=-W                                                      11032
      IJ=N*J/2                                                             11033
    3 J=J-1                                                                11034
      IF(J.EQ.M) RETURN                                                    11035
      H1=-1.0/A(IJ)                                                        11036
      B(J)=H1                                                              11037
      IJ=IJ-J                                                              11038
      K=0                                                                  11039
      IK=0                                                                 11040
C                                   BEGIN K-LOOP                           11041
    4 IK=IK+K                                                              11042
      I1=IK+1                                                              11043
      K=K+1                                                                11044
      IF(K.GT.N) GO TO 3                                                   11045
      IF(K.LT.J) GO TO 9                                                   11046
      IF(W.LT.0.) GO TO 3                                                  11047
      IF(K.EQ.J) GO TO 7                                                   11048
      I=IK+J                                                               11049
    5 H2=A(I)                                                              11050
      B(K)=H2*H1                                                           11051
C                                   BEGIN I-LOOP                           11052
      I2=IK+K                                                              11053
      L=0                                                                  11054
      DO 6 I=I1,I2                                                         11055
      L=L+1                                                                11056
    6 A(I)=A(I)+B(L)*H2                                                    11057
      IF(K.LT.J) GO TO 4                                                   11058
      I=IK+J                                                               11059
      A(I)=B(K)                                                            11060
      GO TO 4                                                              11061
C                                   END I-LOOP                             11062
    7 I=IJ                                                                 11063
      DO 8 L=1,J                                                           11064
      I=I+1                                                                11065
    8 A(I)=B(L)                                                            11066
      GO TO 4                                                              11067
C                                   END K-LOOP                             11068
    9 I=IJ+K                                                               11069
      GO TO 5                                                              11070
      END                                                                  11071
      SUBROUTINE  TACS2                                                 M13.1548
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      REAL*8        DUMJ, ALNODE, TEXNAM(5)                             M39.1801
      DIMENSION DUM9(10), DUMJ(9)                                       M38.2387
      EQUIVALENCE  (MONCAR(2),  KBASE),   (MONCAR(3), LTDELT)           M37.4112
 1000 IF ( IPRSUP  .GE.  1 )                                            M32.4220
     1 WRITE (LUNIT6, 1001)  LASTOV, M4PLOT                             M32.4221
 1001 FORMAT (   33H ENTER "TACS2".  LASTOV, M4PLOT =,  2I6  )          M32.4222
      DO 99 I = 1, 8                                                    M39.1802
   99 LSTAT( I) = LSTAT(I+60)                                           M39.1803
      PICON = 360. / TWOPI                                              M39.1804
      EPSLON = EPSILN * 100.                                            M39.1805
      ITE = 0                                                           M39.1806
      NIUNRS = IUTY( KIUTY + 1 )                                        M28.4309
      MA1 = IUTY(KIUTY+7)                                               M42.6583
      MA2 = IUTY(KIUTY+8)                                               M42.6584
      IF ( IPRSUP  .LT.  1 )   GO TO 6219                               M13.1553
      WRITE (LUNIT6, 6213) IOUTCS, IPLOT, IPRSUP, NTCSEX                M39.1807
     1  NSTACS, KTAB, NIU, NUK, NENERG, IUTY(KIUTY+11)                  M39.1808
 6213 FORMAT ( /,  1X,                                                  M19.1192
     1 48H  IOUTCS   IPLOT  IPRSUP  NTCSEX  NSTACS    KTAB,             M39.1809
     2 32H     NIU     NUK  NENERG  INFEXP, /, 10I8  )                  M39.1810
      WRITE (LUNIT6, 6215)  T, TWOPI, FLTINF, XOPT, COPT                M37.4119
 6215 FORMAT ( 1X, 14X, 1HT,  10X, 5HTWOPI, 9X, 6HFLTINF,               M37.4120
     1       11X, 4HXOPT,  11X, 4HCOPT   ,/,                            M37.4121
     2 1X, 8E15.6  )                                                    M13.1566
 6219 KXAI = KXAR + LSTAT(68)                                           M39.1811
      KBTCS = KATCS + LSTAT(62)                                         M39.1812
      KBWKCS = KAWKCS + LSTAT(68)                                       M39.1813
      KJSUP = KINSUP + LSTAT(65)                                        M39.1814
      KKSUP = KJSUP  + LSTAT(65)                                        M39.1815
      NDY5 = KUD1 - 5                                                   M37.4122
      NDX1 = KXTCS + NUK                                                M39.1816
      XTCS(NDX1+ 3) =  DELTAT                                           M39.1817
      XTCS(NDX1+ 6) =  0.0                                              M39.1818
      XTCS(NDX1+ 7) =  1.0                                              M39.1819
      XTCS(NDX1+ 8) = -1.0                                              M39.1820
      XTCS(NDX1+ 9) =  1.0                                              M39.1821
      XTCS(NDX1+10) =  FLTINF                                           M39.1822
      XTCS(NDX1+11) =  TWOPI / 2.0                                      M39.1823
      DO 6223  I = NIUNRS, NIU                                          M29.1706
      NDY5 = NDY5 + 5                                                   M37.4123
      IF (IUTY(KIUTY+I) .NE. 14 )  GO TO 6223                           M39.1824
      XTCS(NDX1+4) = UD1(NDY5+3)                                        M39.1825
      XTCS(NDX1+5) = TWOPI * UD1( NDY5 + 3 )                            M39.1826
      GO TO 6224                                                        M29.1712
 6223 CONTINUE                                                          M29.1713
C                                               $$$  STEADY - STATE  $$$M13.1568
 6224 D1TTTT = T                                                        M29.1714
      N1TTTT = ISTEP                                                    M20.3296
      ISTEP = 0                                                         M13.1569
      T = 0.0                                                           M13.1570
C     ******  LINKAGE FOR  'TACS EMTP SOURCES'  NAMES   ********        M13.1603
      IF ( NSTACS .EQ. 0 )  GO TO 480                                   M39.1827
      DO 482 I=1,NSTACS                                                 M13.1605
      IF ( VSTACS(I) .EQ. BLANK )  GO TO 483                            M13.1606
      DO 484 J=1,KTAB                                                   M13.1607
      NDX1 = ILNTAB( KLNTAB + J )                                       M38.2405
      IF ( VSTACS(I) .EQ. TEXVEC(NDX1) )  GO TO 490                     M38.2406
  484 CONTINUE                                                          M13.1609
      KILL = 124                                                        M13.1610
      BUS1 = VSTACS(I)                                                  M13.1611
      LSTAT(14) = I                                                     M13.1612
      LSTAT(19) = 490                                                   M13.1613
      WRITE(LUNIT6,166) VSTACS(I), I                                    M17. 661
  166 FORMAT(22H0ERROR.  EMTP SOURCE =,A6,8H= (TYPE ,I2,46H) IS NOT A REM13.1615
     1COGNIZABLE VARIABLE NAME IN TACS.)                                M13.1616
      GO TO 9000                                                        M13.1617
  490 VSTACS(I) = J                                                     M13.1618
      GO TO 482                                                         M13.1619
  483 VSTACS(I) = 0.0                                                   M13.1620
  482 CONTINUE                                                          M13.1621
  480 NUKI = KISBLK - 8                                                 M39.1828
      NUKR = KRSBLK - 4                                                 M39.1829
      DO 215 I = 1, NUK                                                 M39.1830
      NUKI = NUKI + 8                                                   M39.1831
      NUKR = NUKR + 4                                                   M39.1832
      IF ( ISBLK(NUKI+2)  .EQ.  1 )  GO TO 215                          M39.1833
      J = ISBLK( NUKI + 3 )                                             M39.1834
      K = ISBLK( NUKI + 2 )                                             M39.1835
      N1 = J + 8                                                        M39.1836
      N2 = J - 1 + K * 6                                                M39.1837
      F2 = 2.0 / DELTAT                                                 M39.1838
      F1 = 1.0                                                          M39.1839
      NDX2 = KPRSUP + J + 2                                             M39.1840
      IF ( PARSUP( NDX2) .EQ. 0.0 )  GO TO 4061                         M39.1841
      RSBLK(NUKR+1) = PARSUP(NDX2+1) / PARSUP(NDX2)                     M39.1842
 4061 DO 4063  LL = N1, N2, 6                                           M39.1843
      F1 = F1 * F2                                                      M39.1844
      NDX1 = KPRSUP  + LL                                               M39.1845
      PARSUP(NDX1) = PARSUP(NDX1) * F1                                  M39.1846
      PARSUP(NDX2) = PARSUP(NDX2) + PARSUP(NDX1)                        M39.1847
      PARSUP(NDX1+1) = PARSUP(NDX1+1) * F1                              M39.1848
      PARSUP(NDX2+1) = PARSUP(NDX2+1) + PARSUP(NDX1+1)                  M39.1849
      N3 = ( LL - J )  / 6                                              M39.1850
 4063 DUM9(N3) = 1.0                                                    M39.1851
      F1 = 1.0                                                          M39.1852
      K2 = J + 2                                                        M39.1853
      K1 = J + 8                                                        M39.1854
      I1 = K                                                            M39.1855
 4072 I1 = I1 - 1                                                       M39.1856
      IF ( I1 .EQ. 0 )  GO TO 215                                       M39.1857
      F1 = -2.0 * F1                                                    M39.1858
      K3 = K2                                                           M39.1859
      K2 = K1                                                           M39.1860
      K1 = K1 + 6                                                       M39.1861
      NDX1 = KPRSUP  + K2                                               M39.1862
      IF ( I1 .EQ. 1 )  GO TO 4091                                      M39.1863
      DO 4085  LL=2, I1                                                 M39.1864
 4085 DUM9(LL) = DUM9(LL) + DUM9(LL-1)                                  M39.1865
      DO 4088 KJ= K1, N2, 6                                             M39.1866
      MM = ( KJ - K3 ) / 6                                              M39.1867
      NDX3 = KPRSUP  + KJ                                               M39.1868
      PARSUP(NDX1) = PARSUP(NDX1) + DUM9(MM) * PARSUP(NDX3)             M39.1869
 4088 PARSUP(NDX1+1) = PARSUP(NDX1+1) + DUM9(MM) * PARSUP(NDX3+1)       M39.1870
 4091 NDX2 = KPRSUP  + K3                                               M39.1871
      PARSUP(NDX1) = PARSUP(NDX2) + F1 * PARSUP(NDX1)                   M39.1872
      PARSUP(NDX1+1) = PARSUP(NDX2+1) + F1 * PARSUP(NDX1+1)             M39.1873
      GO TO 4072                                                        M39.1874
  215 CONTINUE                                                          M39.1875
C     ******  T A C S   S O U R C E S   90  AND  91  ******             M13.1622
      IF ( IPRSUP  .GE.  1 )                                            M39.1876
     1 WRITE (LUNIT6, 6523)  ( VSTACS(I), I=1, NSTACS )                 M35.1310
 6523 FORMAT ( /,  40H (VSTACS(I), I=1, NSTACS)  IN  'TACS2' .   ,/,    M35.1311
     1 ( 1X, 10E13.4 ) )                                                M20.3300
      IDCTCS = 0                                                        M37.4128
      IAC = 0                                                           M37.4129
      IF ( NIU .LT. NIUNRS )  GO TO 405                                 M37.4135
      IF ( IPRSUP  .GE.  2 )                                            M18. 700
     1 WRITE (LUNIT6, 3678)  NTOTAC,                                    M18. 701
     2                       (ISMTAC(N6), N6=1, NTOTAC)                 M38.2407
 3678 FORMAT ( /,   9H NTOTAC =,  I4,                                   M18. 703
     1  49H .   ( ISMTAC( J ),  J = 1, NTOTAC )  FOLLOWS .       ,/,    M38.2408
     2 (  12( 1X, I5 ) ) )                                              M38.2409
      NDY5 = KUD1                                                       M38.2410
      DO 474  I = NIUNRS, NIU                                           M28.4317
      NDX1 = ILNTAB( KALIU + I )                                        M38.2411
      BUS1 = TEXVEC( NDX1 )                                             M38.2412
      N1 = IUTY( KIUTY + I )                                            M38.2413
      IF ( N1  .NE. 90 )  GO TO 475                                     M13.1627
      DO 476 J=2,NTOT                                                   M13.1628
      IF ( BUS1  .EQ. BUS(J) )  GO TO 479                               M13.1629
  476 CONTINUE                                                          M13.1630
      KILL = 125                                                        M13.1631
      LSTAT(19) = 475                                                   M13.1632
      WRITE(LUNIT6,168) BUS1                                            M38.2414
  168 FORMAT(25H0ERROR.  TYPE 90 SOURCE =,A6,42H= IS NOT A RECOGNIZABLE M13.1634
     1NODE NAME IN EMTP.)                                               M13.1635
      GO TO 9000                                                        M13.1636
  475 IF ( N1 .NE. 91  .AND.  N1 .NE. 93 )  GO TO 54474                 M14.1774
      DO 478 J=1,KSWTCH                                                 M13.1638
      K = IABS(KMSWIT(J))                                               M32.4223
      NDX2 = LSWTCH + J                                                 M32.4224
      M = IABS( KMSWIT(NDX2))                                           M32.4225
      IF ( BUS1  .EQ.  BUS(K) )   GO TO 479                             M13.1641
      IF ( BUS1  .EQ.  BUS(M) )   GO TO 479                             M13.1642
  478 CONTINUE                                                          M13.1643
      KILL = 126                                                        M13.1644
      IF ( N1 .EQ. 93 )                                                 M14.1775
     1 KILL = 144                                                       M14.1776
      LSTAT(19) = 479                                                   M13.1645
      GO TO 9000                                                        M13.1649
54474 GO TO 474                                                         M38.2415
  479 UD1( NDY5+2 ) = J                                                 M38.2416
  474 NDY5 = NDY5 + 5                                                           
C     000  CHECK FOR ACTIVE SOURCES IN STEADY-STATE  0000000000000000000M14.1778
      NDY5 = KUD1                                                       M38.2418
      DO 407  I = NIUNRS, NIU                                           M28.4319
      NDX1 = KIUTY  +  I                                                M22.3718
      J = IUTY(NDX1)                                                    M22.3719
      IF ( UD1(NDY5+4) .GE. 0.0 )  GO TO 407                            M37.4138
      IF ( J.EQ.11  .OR.  J.EQ.92  .OR.  J.EQ.93 )                      M39.1877
     1 IDCTCS = 1                                                       M14.1784
      IF (J .EQ. 14 )  IAC = 1                                          M39.1878
      IF ( J .NE. 90  .AND.  J .NE. 91 ) GO TO 407                      M37.4140
      IF ( UD1(NDY5+1)  .EQ.  0.0 )  IAC = 1                            M37.4141
      IF ( UD1(NDY5+1)  .EQ.  1.0 )  IDCTCS = 1                         M37.4142
 407  NDY5 = NDY5 + 5                                                           
      IF ( IAC .EQ. 1 )  GO TO 3000                                     M37.4143
  405 IF ( IAC .EQ. 0  .OR.  NUK .EQ. 0 ) GO TO 414                     M39.1879
      NUKI = KISBLK                                                     M38.2420
      DO 3738 I = 1, NUK                                                M37.4145
      IF ( ISBLK(NUKI+2)  .EQ.  1 )  GO TO 3738                         M38.2421
      N1 = KPRSUP + ISBLK( NUKI + 3 )                                   M39.1880
      IF ( PARSUP(N1+1)  .NE.  0.0 )  GO TO 3738                        M39.1881
      J = ISBLK( NUKI + 4 )                                             M38.2423
      XTCS(KXTCS+J) = XTCS(KXTCS+J) - XAR(KXAR+J)                               
 3738 NUKI = NUKI + 8                                                           
  414 IF ( IDCTCS .EQ. 1 )  GO TO 256                                   M37.4152
C     000  FORM COEFFICIENTS TO S-BLOCKS FOR TRANSIENTS  000000000000000M39.1882
  406 IF ( NUK .EQ. 0 )  GO TO 271                                      M39.1883
      NUKI = KISBLK - 8                                                 M39.1884
      DO 4747 I = 1, NUK                                                M39.1885
      NUKI = NUKI + 8                                                   M39.1886
      IF ( ISBLK(NUKI+2)  .EQ.  1 )  GO TO 4747                         M39.1887
      J1 = KPRSUP + ISBLK( NUKI + 3 )                                   M39.1888
      K = ISBLK( NUKI + 2 ) - 1                                         M39.1889
      J2 = J1 + K * 6                                                   M39.1890
      DO 5454 J = J1, J2, 6                                             M39.1891
      PARSUP(J) = PARSUP( J + 2 )                                       M39.1892
 5454 PARSUP(J+1) = PARSUP( J + 3 )                                     M39.1893
 4747 CONTINUE                                                          M39.1894
      IF (IDCTCS .EQ. 0 .AND. KXIC .EQ. 0 .AND. IAC .EQ. 0) GO TO 271   M39.1895
C     000  INITIALIZE HISTORY ON S-BLOCKS FOR DC STEADY-STATE  000000000M14.1806
      IDCTCS = 0                                                        M39.1896
      NUKI = KISBLK - 8                                                 M38.2435
      NUKR = KRSBLK - 4                                                 M38.2436
      DO 238  I = 1, NUK                                                M37.4183
      NUKI = NUKI + 8                                                   M38.2437
      NUKR = NUKR + 4                                                   M38.2438
      IF ( ISBLK(NUKI+2)  .EQ.  1 )  GO TO  238                         M38.2439
      J = ISBLK( NUKI + 4 )                                             M38.2440
      PRX = XTCS( KXTCS + J )                                           M38.2441
      PRU = RSBLK(NUKR+1)  *  PRX                                       M38.2442
      IF ( RSBLK(NUKR+1) .NE. 999999.99 )  GO TO 245                    M38.2443
      PRX = 0.0                                                         M28.4354
      PRU = 0.0                                                         M28.4355
      J = IABS( ISBLK(NUKI+1) )                                         M42.6585
      K = IABS( ISBLK(NUKI+9) ) - 1                                     M42.6586
      IF ( I .EQ. NUK )  K = NSU                                        M37.4188
      DO 239  M = J, K                                                  M28.4360
        NDX1 = KALKSU + M                                               M28.4361
        NDX2 = KXTCS + KSUS( NDX1)                                      M38.2446
        NDX1 = KKSUS + M                                                M28.4363
        PRU = PRU + XTCS( NDX2)  *  KSUS( NDX1)                         M28.4364
  239   CONTINUE                                                        M28.4365
  245 K = ISBLK( NUKI + 2 ) - 1                                         M38.2447
      J = KPRSUP + ISBLK( NUKI + 3 ) + K * 6                            M39.1897
      PARSUP(J-2) = PARSUP(J) * PRU  -  PARSUP(J+1) * PRX               M39.1898
 4510 IF ( K .EQ. 1 )  GO TO 270                                        M39.1899
      K = K - 1                                                         M28.4377
      J = J - 6                                                         M39.1900
      PARSUP(J-2) = (PARSUP(J)*PRU - PARSUP(J+1)*PRX + PARSUP(J+4)) / 2 M39.1901
      GO TO 4510                                                        M39.1902
  270 IF ( IPRSUP  .LT.  2 )  GO TO 238                                 M39.1903
      N1 = KPRSUP + ISBLK( NUKI + 3 )                                   M39.1904
      N2 = N1 + ISBLK( NUKI + 2 ) * 6 - 6                               M39.1905
      WRITE ( LUNIT6, 4412 ) I, ( PARSUP(N), N = N1, N2, 6 )            M39.1906
 4412 FORMAT(18H   FUNCTION NUMBER, I8 ,/, 8H  PN    , 8E15.6)          M39.1907
      WRITE ( LUNIT6, 4414 ) ( PARSUP(N+1), N = N1, N2, 6 )             M39.1908
 4414 FORMAT ( 8H  PD    , 8E15.6 )                                     M39.1909
      N2 = N2 - 6                                                       M39.1910
      WRITE ( LUNIT6, 4416 ) ( PARSUP(N+2), N = N1, N2, 6 )             M39.1911
 4416 FORMAT( 8H  HST   , 8E15.6 )                                      M39.1912
  238 CONTINUE                                                          M28.4384
  271 KXIC = KIVARB                                                     M39.1913
      IF ( NSUP  .EQ.  0 )  GO TO 2222                                  M39.1914
      DO 2121 I = 1, NSUP                                               M39.1915
      N1 = - INSUP( KJSUP + I )                                         M39.1916
      IF ( N1  .LT.  0 )  GO TO 2121                                    M39.1917
      N2 = INSUP( KKSUP + I )                                           M39.1918
      IF ( N2 .NE. 66 )  GO TO 2121                                     M39.1919
      NN = IVARB( N1 )                                                  M39.1920
      N = IVARB( N1 + 3 )                                               M39.1921
      B = 0.0                                                           M39.1922
      DO 2929 J = 1, N                                                  M39.1923
      J1 = NN + J                                                       M39.1924
      PARSUP(J1) = PARSUP(J1) * PARSUP(J1)                              M39.1925
 2929 B = B + PARSUP(J1)                                                M39.1926
      N = KXTCS + NUK + I + LSTAT(64)                                   M39.1927
      XTCS(N) = SQRTZ( B * PARSUP(NN) )                                 M39.1928
 2121 CONTINUE                                                          M39.1929
 2222 IF ( IPRSUP .LT. 1  .AND.  KSSOUT .EQ. 0 ) GO TO 2566             M39.1930
      WRITE ( LUNIT6, 4949 )                                            M39.1931
 4949 FORMAT( /, 42H TACS DC STEADY-STATE SOLUTION FOLLOWS ...          M39.1932
     1    ,/,  5( 3X,  6H-NAME-,   11X, 5HVALUE  )  )                   M39.1933
      MTOT = IOUTCS                                                     M39.1934
      IF ( KSSOUT .EQ. 1 )  MTOT = KTAB                                 M39.1935
      J1 = NUK + NIU                                                    M39.1936
      J2 = NUK + LSTAT(64)                                              M39.1937
      N14 = 0                                                                   
      DO 4848 I = 1, MTOT                                               M39.1939
      J = JOUT( KJOUT + I )                                             M39.1940
      IF ( KSSOUT .EQ. 1 ) J = I                                        M39.1941
      IF ( J .GT. J1  .AND.  J .LE. J2 ) GO TO 4848                     M39.1942
      I1 = ILNTAB( KLNTAB + J )                                         M39.1943
      N14 = N14 + 1                                                             
      TEXNAM(N14) = TEXVEC(I1)                                          M39.1945
      VOLTI(N14) = XTCS(KXTCS+J)                                        M39.1946
      IF ( N14 .LT. 5 )  GO TO 4848                                     M39.1947
      WRITE (LUNIT6, 4737) (TEXNAM(IP), VOLTI(IP), IP=1, N14)           M39.1948
 4737 FORMAT( 5( 3X, A6, E16.7 ) )                                      M39.1949
      N14 = 0                                                           M39.1950
 4848 CONTINUE                                                          M39.1951
      IF ( N14 .GT. 0 )                                                 M39.1952
     1 WRITE (LUNIT6, 4737) (TEXNAM(IP), VOLTI(IP), IP=1, N14)          M39.1953
 2566 IF ( LASTOV  .GT.  1 )   GO TO 256                                M39.1954
      N = 9                                                             M39.1955
      IF ( IOUTCS .LT. 9 )  N = IOUTCS                                  M39.1956
      DO 1212 I = 1, N                                                  M39.1957
      K1 = JOUT(KJOUT+I)                                                M39.1958
      K1 = ILNTAB( KLNTAB + K1 )                                        M39.1959
 1212 DUMJ(I) = TEXVEC(K1)                                              M39.1960
      WRITE (LUNIT6, 234) (DUMJ(I), I = 1, N)                           M39.1961
  234 FORMAT ( /,  5H STEP,  5X, 4HTIME, 3X, 9(6X, A6)  )               M39.1962
32201 IF ( N .GE. IOUTCS ) GO TO 233                                    M39.1963
      K = N + 1                                                         M39.1964
      N = N + 9                                                         M39.1965
      IF ( N .GT. IOUTCS )  N = IOUTCS                                  M39.1966
      J = 0                                                             M39.1967
      DO 2323 I = K, N                                                  M39.1968
      J = J + 1                                                         M39.1969
      K1 = JOUT( KJOUT + I )                                            M39.1970
      K1 = ILNTAB( KLNTAB + K1 )                                        M39.1971
 2323 DUMJ(J) = TEXVEC(K1)                                              M39.1972
      WRITE (LUNIT6, 235) ( DUMJ(I), I = 1, J )                         M39.1973
  235 FORMAT ( /,  17X,  9(6X, A6)  )                                   M39.1974
      GO TO 32201                                                       M39.1975
  233 WRITE (LUNIT6, 235)                                               M39.1976
C                                         $$$  FORM  SPARSE  MATRIX  $$$M13.1773
  256 IF ( IAC  .EQ.  1  .AND.  IDCTCS  .EQ.  0 )   GO TO 3042          M13.1774
      IA = 0                                                            M13.1775
      NUKI = KISBLK - 8                                                 M38.2449
      NUKR = KRSBLK - 4                                                 M38.2450
      DO 260 I = 1, NUK                                                 M39.1977
      NUKI = NUKI + 8                                                   M38.2451
      NUKR = NUKR + 4                                                   M38.2452
      ISBLK(NUKI+8) = IA + 1                                            M38.2453
      NDX1 = KAWKCS + 1                                                 M22.3790
      CALL MOVER0 ( AWKCS(NDX1), KTAB )                                 M37.4197
      NUKJ = KISBLK - 8                                                 M38.2454
      NUKS = KRSBLK - 4                                                 M38.2455
      DO 222  J=1, NUK                                                  M37.4198
      NUKJ = NUKJ + 8                                                   M38.2456
      NUKS = NUKS + 4                                                   M38.2457
      IF ( ISBLK( NUKJ + 4 )  .NE.  I ) GO TO 222                       M38.2458
      J1 = IABS( ISBLK(NUKJ+1) )                                        M42.6587
      K1 = IABS( ISBLK(NUKJ+9) ) - 1                                    M42.6588
      IF ( J .EQ. NUK )  K1 = NSU                                       M37.4203
      IF ( ISBLK(NUKJ+2)  .EQ.  1 ) GO TO 218                           M38.2461
      N = KPRSUP + ISBLK( NUKJ + 3 )                                    M39.1978
      NDX1 = KAWKCS + I                                                 M22.3796
      DPD = PARSUP( N + 1 )                                             M39.1979
      IF ( DPD .NE. 0.0  .OR.  IDCTCS .EQ. 0 )  GO TO 2911              M37.4207
      NDX1 = KROWCS + I                                                 M37.4208
      ISBLK(NUKI+8) = 0                                                 M38.2463
      GO TO 260                                                         M37.4210
 2911 AWKCS(NDX1) = DPD                                                 M37.4211
      DO 219  M=J1, K1                                                  M13.1787
      L = KSUS( KALKSU + M )                                            M38.2464
      NDX1 = KAWKCS + L                                                 M22.3805
      NDX3 = KKSUS  + M                                                 M22.3807
  219 AWKCS(NDX1) = -PARSUP(N) * KSUS(NDX3)                             M39.1980
      GO TO 221                                                         M13.1790
  218 NDX1 = KAWKCS + I                                                 M37.4214
      AWKCS(NDX1) = 1.0                                                 M22.3812
      DO 223  M=J1, K1                                                  M13.1798
      L = KSUS( KALKSU + M )                                            M38.2465
      NDX1 = KAWKCS + L                                                 M22.3819
      NDX3 = KKSUS  + M                                                 M22.3821
  223 AWKCS(NDX1) = -RSBLK(NUKS+1) * KSUS(NDX3)                         M38.2466
      GO TO 221                                                         M13.1801
  222 CONTINUE                                                          M13.1802
  221 IF ( IPRSUP  .GE.  4 )                                            M22.3823
     1  WRITE (LUNIT6, 2021) I, (J, AWKCS(KAWKCS+J), J=1, KTAB)         M37.4218
 2021 FORMAT ( 5H0ROW , I3, (1X, 6(I3, 2H: , E13.6,2X) ) )              M31.2766
C                                    $$$  TRIANGULARIZE  EACH  ROW  $$$ M13.1806
      D1 = AWKCS( KAWKCS + I )                                          M37.4219
      IF ( D1  .EQ.  0.0 )   GO TO 7261                                 M37.4220
      IF ( I  .EQ.  1 )   GO TO 261                                     M13.1808
      M = I - 1                                                         M13.1809
      DO 262  K=1, M                                                    M13.1810
      NDX1 = KAWKCS + K                                                 M22.3827
      IF ( AWKCS(NDX1)  .EQ.  0.0 )   GO TO 262                         M22.3828
      N = K + 1                                                         M39.1981
      NDX1 = KISBLK + K * 8                                             M38.2467
      K1 = ISBLK(NDX1) - 1                                                      
      IF ( K1  .LT.  0 ) GO TO 262                                      M37.4221
 4545 K2 = ISBLK(NDX1+8)                                                M38.2469
      IF ( K2  .GT.  0 )  GO TO 263                                     M38.2470
      NDX1 = NDX1 + 8                                                   M38.2471
      GO TO 4545                                                        M38.2472
  263 K1 = K1 + 1                                                       M13.1815
      IF ( K1  .EQ.  K2 )   GO TO 262                                   M13.1816
      J = ICOLCS( KCOLCS + K1 )                                         M37.4222
      JLK = ISBLK( KISBLK + J * 8 )                                     M38.2473
      IF ( J .LT. N  .AND.  JLK .NE. 0 )  GO TO 263                     M37.4224
  264 NDX1 = KAWKCS + J                                                 M22.3837
      NDX3 = KAWKCS + K                                                 M22.3838
      NDX4 = KATCS  + K1                                                M22.3839
      AWKCS(NDX1) = AWKCS(NDX1)  -  AWKCS(NDX3)*ATCS(NDX4)              M22.3840
      GO TO 263                                                         M37.4225
  262 CONTINUE                                                          M13.1824
  261 NDX1 = KAWKCS + I                                                 M22.3843
      D2 = AWKCS(NDX1)                                                  M22.3844
      IF ( ABSZ(D2/D1)   .GT.   EPSILN )   GO TO 265                    M18. 707
 7261 KILL = 128                                                        M13.1828
      NDX1 = ILNTAB( KLNTAB + I )                                       M38.2474
      BUS1 = TEXVEC(NDX1)                                               M38.2475
      LSTAT(14) = I                                                     M13.1830
      FLSTAT(14) = D1                                                   M13.1831
      FLSTAT(15) = D2                                                   M13.1832
      LSTAT(19) = 7261                                                  M13.1833
      WRITE ( LUNIT6, 267 ) BUS1                                        M38.2476
  267 FORMAT ( 95H0ERROR.  YOUR SYSTEM IS UNSTABLE.  YOU WILL FIND THE EM13.1835
     1RROR IN A FEEDBACK LOOP INVOLVING BLOCK =, A6, 2H=.   )           M13.1836
      GO TO 9000                                                        M13.1837
  265 NDX3 = KAWKCS + I                                                 M37.4226
      AWKCS(NDX3) = 1.0 / AWKCS(NDX3)                                   M37.4227
C                               $$$  APPEND  TO  COMPACTED  VECTOR  $$$ M13.1839
      DO 268  J = 1, KTAB                                               M37.4228
      IF ( AWKCS(KAWKCS+J)  .EQ.  0.0 )   GO TO 268                     M37.4229
      IA = IA + 1                                                       M13.1842
      IF ( IA  .LE.  LSTAT(62) )   GO TO 8122                           M39.1982
      KILL = 122                                                        M13.1844
      LSTAT( 16) = LSTAT(62)                                            M39.1983
      LSTAT( 17) = 2                                                    M39.1984
      LSTAT(19) = 8122                                                  M13.1847
      GO TO 9000                                                        M13.1848
 8122 NDX1 = KCOLCS + IA                                                M22.3853
      ICOLCS(NDX1) = J                                                  M22.3854
      NDX1 = KATCS  + IA                                                M22.3855
      NDX2 = KAWKCS + J                                                 M22.3856
      ATCS(NDX1) = AWKCS(NDX2)                                          M22.3857
      JLK = ISBLK( KISBLK + J * 8 )                                     M38.2479
      IF ( J .LE. I  .AND.  JLK .NE. 0 )   GO TO 268                    M37.4233
      ATCS(NDX1) = ATCS(NDX1) * AWKCS(NDX3)                             M22.3860
  268 CONTINUE                                                          M13.1853
  260 CONTINUE                                                          M13.1854
      IF ( IPRSUP  .LE.  1  )   GO TO 250                               M13.1855
      WRITE (LUNIT6, 2023)  IA, (ISBLK(KISBLK+N*8), N=1, NUK)           M38.2480
 2023 FORMAT(1H0/4H IA=,I4,8X,27HNEXT IS IROWCS(1,2,...,NUK)/           M13.1857
     1(1X,30I4))                                                        M13.1858
      WRITE (LUNIT6, 2024) (ICOLCS(KCOLCS+M),                           M37.4235
     1                      ATCS(KATCS+M), M=1, IA)                     M37.4236
 2024 FORMAT (16H0ICOLCS AND ATCS ,/, (1X, 6(I3,E15.6,2X)))             M30. 217
C                                       $$$  UPDATE  INPUT  SOURCES  $$$M13.1861
  250 NDX1 = KXTCS + NUK                                                M28.4385
      XTCS(NDX1+1) = T                                                  M39.1985
      XTCS(NDX1+2) = ISTEP                                              M39.1986
      IF ( IDCTCS.EQ.0  .AND.  T.EQ.0.0 )  GO TO 317                    M13.1863
C     ---  I.C. ON DEVICES  ---                                         M28.4409
      IF ( ISTEP .NE. 1 .OR. NSUP .EQ. 0 )  GO TO 25050                 M39.1987
      DO 5051  I = 1, NSUP                                              M39.1988
      N1 = - INSUP( KJSUP + I )                                         M39.1989
      IF ( N1 .LT. 0 ) GO TO  5051                                      M39.1990
      N2 = INSUP(KKSUP+I) - 49                                          M39.1991
      NN = IVARB(N1)                                                    M39.1992
      J = NUK + LSTAT(64) + I                                           M39.1993
      GO TO ( 5051, 5051, 5051, 5051, 5051, 5051, 5051, 5051,           M39.1994
     1   658, 659, 5051, 5051, 662, 5051, 664, 664, 5051, 5051 ), N2    M39.1995
  658 PARSUP(NN) = (PARSUP(NN+1) - PARSUP(NN+2)) /2.0 * XTCS(KXTCS+J)   M39.1996
      GO TO  5051                                                       M39.1997
  659 J = IVARB( N1 + 1 )                                               M39.1998
      K = IVARB( N1 + 2 )                                               M39.1999
      B = 0.0                                                           M39.2000
      DO 5061  N = J, K                                                 M39.2001
      M = KXTCS + KSUS( KALKSU + N )                                    M39.2002
 5061 B = B + XTCS(M)  *  KSUS(KKSUS+N)                                 M39.2003
      PARSUP(NN+1) = B                                                  M39.2004
      GO TO 5051                                                        M39.2005
  662 PARSUP(NN+2) = XTCS( KXTCS + J )                                  M39.2006
      GO TO  5051                                                       M39.2007
  664 PARSUP(NN) = XTCS( KXTCS + J )                                    M39.2008
 5051 CONTINUE                                                          M39.2009
25050 IF ( IDCTCS .EQ. 1 )  GO TO 6513                                  M28.4463
      IF ( NTCSEX+NSTACS  .LE.  0 )   GO TO 6513                        M20.3305
      T = D1TTTT                                                        M20.3306
      ISTEP = N1TTTT                                                    M20.3307
      INDSTP = 1                                                        M21. 149
      XTCS(NDX1+1) = 0.0                                                M39.2010
      XTCS(NDX1+2) = 0.0                                                M39.2011
      IF ( NSUP .EQ. 0  .OR.  NUMSM .EQ. 0 ) GO TO 9000                 M39.2012
      N33 = 0                                                           M39.2013
      DO 909 I = 1, NUMSM                                               M39.2014
      I30 = 30 * I - 29                                                 M39.2015
      JMSET = ISMDAT( I30 + 15 )                                        M39.2016
      IF ( JMSET .LE. 0 ) GO TO 909                                     M39.2017
      JMSET = -INSUP( KINSUP + LSTAT(65) + JMSET - LSTAT(64) - NUK )    M39.2018
      JMSET = IVARB( JMSET )                                            M39.2019
      PARSUP(JMSET) = CU( N33 + 11 )                                    M39.2020
  909  N33 = N33 + 24                                                   M39.2021
      DO 5457 I = 1, NSUP                                               M39.2022
      N1 = - INSUP( KJSUP + I )                                         M39.2023
      IF ( N1 .LE. 0 ) GO TO 5457                                       M39.2024
      N2 = INSUP( KKSUP + I )                                           M39.2025
      IF ( N2 .NE. 67 )  GO TO 5457                                     M39.2026
      NN = IVARB( N1 )                                                  M39.2027
      IF ( PARSUP(NN) .GE. PARSUP(NN+1) ) GO TO 4499                    M39.2028
 4477 KILL = 500                                                        M39.2029
      WRITE ( LUNIT6, 4488 )                                            M39.2030
 4488 FORMAT( 51H ***** THE CREST OF FIELD WINDING VOLTAGE SOURCE OF,   M39.2031
     1        39H #59 MACHINE IS BEYOND ITS LIMIT *****                 M39.2032
     2  ,/,   45H STOP AT TACS2 OF OVER12 NEAR STATEMENT 4499  )        M39.2033
      GO TO 9000                                                        M39.2034
 4499 IF ( PARSUP(NN) .GT. PARSUP(NN+2) ) GO TO 4477                    M39.2035
      B = 0.0                                                           M39.2036
      J = IVARB( N1 + 1 )                                               M39.2037
      K = IVARB( N1 + 2 )                                               M39.2038
      DO 1166 MJ = J, K                                                 M39.2039
      N = KKSUS + MJ                                                    M39.2040
      IF ( KSUS(N)  .EQ.  9 ) GO TO 1166                                M39.2041
      M = KALKSU + MJ                                                   M39.2042
      BB = XTCS( KXTCS + KSUS(M) ) * KSUS(N)                            M39.2043
      NJ = MJ                                                           M39.2044
 1144 NJ = NJ - 1                                                       M39.2045
      N = N - 1                                                         M39.2046
      M = M - 1                                                         M39.2047
      IF ( NJ  .LT.  J ) GO TO 1155                                     M39.2048
      IF ( KSUS(N)  .NE.  9 ) GO TO 1155                                M39.2049
      BB = BB * XTCS( KXTCS + KSUS(M) )                                 M39.2050
      GO TO 1144                                                        M39.2051
 1155 B = B + BB                                                        M39.2052
 1166 CONTINUE                                                          M39.2053
      PARSUP(NN+3) = 1.0 / B                                            M39.2054
      NDX1 = KXTCS + NUK + LSTAT(64) + I                                M39.2055
      XTCS(NDX1) = 1.0                                                  M39.2056
 5457 CONTINUE                                                          M39.2057
      GO TO 9000                                                        M20.3308
 6513 CONTINUE                                                          M20.3309
      IF ( NIU .LT. NIUNRS )  GO TO 505                                 M28.4464
      IF ( LTDELT  .NE.  -6789 )   GO TO 1768                           M28.4465
C     POSTPROCESSING USAGE DEFINES TACS SOURCES FROM OLD                M28.4466
C     PLOT FILE THAT IS CONNECTED TO UNIT "LUNIT2" .                    M28.4467
      DO 1763  I=1, IOFBND                                              M28.4468
      IF ( M4PLOT  .EQ.  0 )   GO TO 1761                               M32.4226
      CALL PLTLU2 ( D1, VOLTI(1) )                                      M32.4227
      GO TO 1762                                                        M32.4228
 1761 READ (LUNIT2)  D1, ( VOLTI(J), J=1, IOFGND )                      M32.4229
 1762 IF ( D1  .EQ.  -9999. )   GO TO 900                               M32.4230
 1763 CONTINUE                                                          M29.1717
      T = D1                                                            M28.4471
      NDX1 = KXTCS + NUK                                                M28.4472
      XTCS(NDX1+1) = T                                                  M28.4473
      XTCS(NDX1+3) = T - EPSUBA                                         M28.4474
      EPSUBA = T                                                        M28.4475
      NDX1 = NDX1 + NIUNRS                                              M28.4476
      CALL MOVER ( VOLTI(1), XTCS(NDX1), IOFGND )                       M28.4477
      GO TO 505                                                         M28.4478
 1768 NDY5 = KUD1 - 5                                                   M37.4242
      DO 500  I = NIUNRS, NIU                                           M37.4243
      NDY5 = NDY5 + 5                                                   M37.4244
      J = I + NUK                                                       M13.1868
  510 NDXI = KXTCS + J                                                  M37.4245
      XTCS(NDXI) = 0.0                                                  M37.4246
      NDX1 = KIUTY  +  I                                                M22.3869
      N1 = IUTY(NDX1)                                                   M22.3870
      IF ( T .LT. UD1(NDY5+4) - FLZERO * 10. )   GO TO 500              M37.4247
      IF ( T .GE. UD1(NDY5+5) - FLZERO * 10. )   GO TO 500              M37.4248
      IF ( T .EQ. 0.0 .AND. UD1(NDY5+4)  .GE. 0.0 )  GO TO 500          M37.4249
      IF ( IAC .EQ. 1  .AND.  N1 .EQ. 14 )  GO TO 500                   M39.2058
      IF ( N1 .GE. 90  .AND.  T .GT. 0.0 )  GO TO 500                   M14.1817
      IF ( N1 .LT. 90 )  GO TO 501                                      M14.1818
      K = UD1( NDY5 + 2 )                                               M38.2489
      IF ( N1 .GT. 93 )  GO TO 500                                      M14.1821
      N2 = N1 - 89                                                      M14.1822
      GO TO ( 502, 508, 504, 506), N2                                   M37.4250
  502 IF ( UD1(NDY5+1)  .EQ.  1.0 ) XTCS(NDXI) = E( K)                  M37.4251
      GO TO 500                                                         M14.1825
  504 XTCS(NDXI) = ETAC( K)                                             M37.4252
      GO TO 500                                                         M14.1827
  506 IF ( NEXTSW(K) .EQ. 87 )  XTCS(NDXI) = 1.0                        M39.2059
      GO TO 500                                                         M14.1829
  508 IF (NEXTSW(K) .EQ. 87 .AND. UD1(NDY5+1) .EQ. 1.0)                 M37.4254
     1    XTCS(NDXI) = TCLOSE(K)                                        M37.4255
      GO TO 500                                                         M37.4256
  501 XTCS(NDXI) = UD1(NDY5+1)                                          M37.4257
      IF ( N1 .EQ. 11 )  GO TO 500                                      M39.2060
      IF ( N1 .NE. 14 )  GO TO 511                                      M39.2061
      XTCS(NDXI) = XTCS(NDXI) * COSZ(TWOPI * UD1(NDY5+3) * T  +         M37.4258
     1     UD1(NDY5+2)  )                                               M37.4259
  511 IF ( T .EQ. 0.0 )  GO TO 500                                      M28.4484
      IF ( N1 .NE. 23 )  GO TO 512                                      M39.2062
      IF (T .LT. UD1(NDY5+4) +UD1(NDY5+2) -FLZERO*10.) GO TO 500        M37.4260
      UD1(NDY5+4) = UD1(NDY5+4) + UD1(NDY5+3)                           M37.4261
      GO TO 510                                                         M28.4491
  512 IF ( N1 .NE. 24 )  GO TO 500                                      M39.2063
      IF (T .LT. UD1(NDY5+4) +UD1(NDY5+3)-FLZERO*10.) GO TO 513         M37.4262
      UD1(NDY5+4) = UD1(NDY5+4) + UD1(NDY5+3)                           M37.4263
      GO TO 512                                                         M28.4497
  513 XTCS(NDXI) = XTCS(NDXI) * (T - UD1(NDY5+4)) / UD1(NDY5+3)         M37.4264
      IF ( ABSZ(XTCS(NDXI)) .LE. 10.*FLZERO ) XTCS(NDXI) = 0.0          M37.4265
  500 CONTINUE                                                          M13.1877
  505 L = IUTY( KIUTY + 4 )                                             M39.2064
      IF ( T  .GT.  0.0 ) GO TO 5555                                    M39.2065
  555 IF ( L  .GT.  0 ) CALL CSUPDC(L)                                  M39.2066
      IPASS = 0                                                         M39.2067
      ITE = ITE + 1                                                     M39.2068
      IF ( ITE .LT. 100 ) GO TO 327                                     M39.2069
      WRITE ( LUNIT6, 7798 ) ITE                                        M39.2070
 7798 FORMAT( 26H ---- ITERATION IS BEYOND , I4, 15H TIMES, BUT THE,    M39.2071
     1 58H RESULT IS NOT CONVERGENT YET, PROGRAM WILL CONTINUE  ---- )  M39.2072
      GO TO 3177                                                        M39.2073
 5555 IF ( L  .GT.  0 ) CALL CSUP(L)                                    M39.2074
C                                                 $$$  FORM  RHSIDE  $$$M13.1881
      NUKR = KRSBLK - 4                                                 M38.2491
      DO 2324 I = 1, NUK                                                M38.2492
      NUKR = NUKR + 4                                                   M38.2493
 2324 RSBLK(NUKR+4) = 0.0                                               M38.2494
      NUKI = KISBLK - 8                                                 M38.2495
      DO 304 I = 1, NUK                                                 M37.4270
      NUKI = NUKI + 8                                                   M38.2496
      IF ( ISBLK(NUKI+2)  .EQ.  1 ) GO TO 304                           M38.2497
      J = KRSBLK + ISBLK(NUKI+4) * 4                                    M38.2498
      K = KPRSUP + ISBLK(NUKI+3) + 4                                    M39.2075
      RSBLK(J) = RSBLK(J) + PARSUP(K)                                   M39.2076
  304 CONTINUE                                                          M37.4275
C                                          $$$  FORWARD  ON  RHSIDE  $$$M13.1888
      NUKI = KISBLK - 8                                                 M38.2501
      NUKR = KRSBLK - 4                                                 M38.2502
      DO 305 I=1,NUK                                                    M13.1889
      NUKI = NUKI + 8                                                   M38.2503
      NUKR = NUKR + 4                                                   M38.2504
      M = ISBLK(NUKI+8)                                                 M38.2505
  301 NDX1 = KCOLCS + M                                                 M22.3904
      K = ICOLCS(NDX1)                                                  M22.3905
      IF (K .GE. I) GO TO 306                                           M22.3906
      NDX3 = KATCS  + M                                                 M22.3908
      NDX4 = KRSBLK + K * 4                                             M38.2506
      RSBLK(NUKR+4) = RSBLK(NUKR+4) - ATCS(NDX3) * RSBLK(NDX4)          M38.2507
      M = M + 1                                                         M13.1894
      GO TO 301                                                         M13.1895
  306 NDX3 = KATCS  + M                                                 M38.2508
  305 RSBLK(NUKR+4) = RSBLK(NUKR+4) * ATCS(NDX3)                        M38.2509
C                                           $$$  BACKWARD  ON  XTCS  $$$M13.1897
  327 MM = 1                                                            M37.4276
      NUKI = KISBLK - 8                                                 M38.2510
      NUKR = KRSBLK - 4                                                 M38.2511
      DO 3088 I=1,NUK                                                   M37.4278
      NUKI = NUKI + 8                                                   M38.2512
      NUKR = NUKR + 4                                                   M38.2513
      J = NUK + 1 - I                                                   M13.1899
      NUKJ = KISBLK + J * 8 - 8                                         M38.2514
      NUKS = KRSBLK + J * 4 - 4                                         M38.2515
      M = ISBLK( NUKJ + 8 )                                             M39.2077
      NDX1 = KXTCS + J                                                  M39.2078
      XTCSAV = XTCS(NDX1)                                               M39.2079
      IF ( M  .EQ.  0 )  GO TO 308                                      M37.4283
      N = IA                                                            M13.1908
      XTCS(NDX1) = RSBLK( NUKS + 4 )                                    M38.2517
      IF ( J .EQ. NUK )  GO TO 309                                      M14.1839
      NKK = ISBLK(NUKJ+16)                                              VAX.1381
      NKK1 = 1                                                          VAX.1382
      IF ( NKK .GT. 0 ) GO TO 3091                                      VAX.1383
 2850 IF ( J .EQ. NUK - NKK1 ) GO TO 309                                VAX.1384
      NKK = ISBLK(NUKJ+NKK1*8+16)                                       VAX.1385
      IF ( NKK .GT. 0 ) GO TO 3091                                      VAX.1386
      NKK1 = NKK1 + 1                                                   VAX.1387
      GO TO 2850                                                        VAX.1388
 3091 N = NKK - 1                                                       VAX.1389
  309 IF ( M  .GT.  N ) GO TO 324                                       M37.4287
      K = ICOLCS( KCOLCS + M )                                          M37.4288
      ILK = ISBLK( KISBLK + K * 8 )                                     M38.2519
      IF ( K .LE. J  .AND.  ILK .NE. 0 )  GO TO 334                     M37.4290
      NDX3 = KATCS  + M                                                 M22.3927
      NDX4 = KXTCS  + K                                                 M22.3928
      XTCS(NDX1) = XTCS(NDX1) - ATCS(NDX3) * XTCS(NDX4)                 M22.3929
  334 M = M + 1                                                         M37.4291
      GO TO 309                                                         M37.4292
  324 IF ( IAC .EQ. 1 )  GO TO 308                                      M14.1842
      J1 = ISBLK( NUKJ + 5 )                                            M38.2520
      K1 = ISBLK( NUKJ + 6 )                                            M38.2521
      NDX2 = KXTCS  + J1                                                M22.3935
      IF ( J1 .GT. 0 )  RSBLK(NUKS+2)  = XTCS(NDX2)                     M38.2522
      NDX2 = KXTCS  + K1                                                M22.3938
      IF ( K1 .GT. 0 )  RSBLK(NUKS+3)  = XTCS(NDX2)                     M38.2523
      IF ( T .GT. 0.0 )  GO TO 337                                      M13.1922
      IF(XTCS(NDX1).GE.RSBLK(NUKS+2) .AND. XTCS(NDX1).LE.RSBLK(NUKS+3)) M38.2524
     1     GO TO 308                                                    M22.3944
      IF ( IUTY(KIUTY+3) .EQ. 0 ) GO TO 337                             M42.6589
      IUTY(KIUTY+3) = IUTY(KIUTY+3) - 1                                 M42.6590
      NDX2 = ILNTAB( KLNTAB + J )                                       M39.2080
      WRITE(LUNIT6,3041) TEXVEC(NDX2)                                   M39.2081
 3041 FORMAT ( 18H0WARNING.  BLOCK ', A6, 69H' HAS ITS LIMITER OPERATINGM14.1844
     1 DURING THE TACS DC STEADY-STATE SOLUTION.   ,/,                  M14.1845
     2  11X,104HDOUBLE-CHECK THE PROGRAM OUTPUT FOR  T = 0.0  FOR ANY MIM14.1846
     3SUNDERSTANDING BETWEEN THE PROGRAM AND THE USER.   /1X )          M14.1847
  337 IF ( XTCS(NDX1) .LT. RSBLK(NUKS+2)  )  XTCS(NDX1) = RSBLK(NUKS+2) M39.2082
      IF ( XTCS(NDX1) .GT. RSBLK(NUKS+3)  )  XTCS(NDX1) = RSBLK(NUKS+3) M38.2528
      IF ( RSBLK(NUKS+2) .LE. RSBLK(NUKS+3) ) GO TO 308                 M39.2083
      IF ( IUTY(KIUTY+3)  .EQ.  0 )  GO TO 308                          M42.6591
      NDX2 = ILNTAB( KLNTAB + J )                                       M39.2085
      WRITE(LUNIT6,198) TEXVEC(NDX2)                                    M39.2086
  198 FORMAT(27H0WARNING. LIMITS AT BLOCK =,A6,49H= HAVE CRISS-CROSSED. M13.1929
     1   EXPECT PUZZLING RESULTS./74H THIS WARNING MESSAGE WILL NOT BE RM13.1930
     2EPEATED.  REFER TO TACS USER'S MANUAL.   )                        M32.4231
      IUTY(KIUTY+3) = IUTY(KIUTY+3) - 1                                 M42.6592
  308 L = ISBLK( NUKJ + 7 )                                             M39.2088
      IF ( T .GT. 0.0 )  GO TO 3077                                     M39.2089
      IF ( L  .GT.  0 )  CALL CSUPDC(L)                                 M39.2090
      DEFR = ABSZ( XTCSAV - XTCS(NDX1) )                                M39.2091
      CRITIA = ABSZ( XTCS(NDX1) ) * 0.001                               M39.2092
      IF ( CRITIA .LT. EPSLON ) CRITIA = EPSLON                         M39.2093
      IF ( DEFR .GT. CRITIA ) IPASS = 1                                 M39.2094
      GO TO 3088                                                        M39.2095
 3077 IF ( L  .GT.  0 )  CALL CSUP(L)                                   M39.2096
 3088 CONTINUE                                                          M37.4296
      IF ( IPASS .EQ. 1 .AND. IUTY(KIUTY+9) .EQ. 1 ) GO TO 505          M39.2097
C                                                       $$$  OUTPUT  $$$M13.1934
 3177 IF ( T.EQ.0.0  .AND.  IAC.EQ.1 )  GO TO 4066                      M39.2098
  317 DO 1010 I =1, NSUP                                                        
      N1 = - INSUP( KJSUP + I )                                                 
      IF ( N1 .LT. 0 ) GO TO 1010                                       M39.2101
      IF (INSUP(KKSUP+I) .NE. 53) GO TO 1010                                    
      J = IVARB( N1 + 1 )                                                       
      K = IVARB( N1 + 2 )                                                       
      B = 0.0                                                                   
      DO 601 N= J, K                                                    M37.4306
      M = KSUS( KALKSU + N )                                                    
  601 B = B + XTCS(KXTCS+M) * KSUS(KKSUS+N)                                     
      NN = IVARB(N1)                                                    M39.2105
      N5 = PARSUP(NN)                                                           
      N6 = PARSUP( NN + 2 )                                                     
      N7 = IVARB( N1 + 4 )                                                      
      NDX6 = KPRSUP + N7                                                        
      PARSUP( NDX6) = B                                                         
      N7 = N7 + 1                                                               
      IF (N7 .EQ. N5+N6) N7 = N5                                                
      IVARB(N1+4) = N7                                                          
 1010 CONTINUE                                                          M37.4318
      IF ( IOUTCS  .EQ.  0 )   GO TO 310                                M13.1939
      IF ( T .GT. DELTAT )  GO TO 335                                   M38.2536
      IF ( T .GT. 0.0 ) GO TO 3335                                      M38.2537
      IF ( BEGMAX(1)  .EQ.  1.0 )   GO TO 316                           M31.2767
      IF ( ISPRIN  .EQ.  0 )   GO TO 316                                M13.1942
      IF ( ISPLOT  .GT.  0 )   GO TO 310                                M13.1943
  316 DO 336 I=1,IOUTCS                                                 M13.1944
      J = JOUT( KJOUT + I )                                             M38.2538
      NDX1 = KBTCS + I                                                  M38.2539
  336 ATCS(NDX1) = XTCS( KXTCS + J )                                    M39.2112
      GO TO 3318                                                        M38.2541
 3335 DO 3331 I = 1, IOUTCS                                             M38.2542
      NDX1 = KBTCS + I                                                  M38.2543
      NDX2 = KXAR  + I                                                  M38.2544
      NDX3 = KXAI  + I                                                  M38.2545
      NDX4 = KAWKCS + I                                                 M38.2546
      NDX5 = KBWKCS + I                                                 M38.2547
      XAR(NDX2) = ATCS(NDX1)                                            M39.2113
      XAR(NDX3) = ATCS(NDX1)                                            M39.2114
      AWKCS(NDX4) = 0.0                                                 M38.2550
 3331 AWKCS(NDX5) = 0.0                                                 M39.2115
  335 DO 318 I=1,IOUTCS                                                 M13.1948
      J = JOUT( KJOUT + I )                                             M38.2552
      NDX1 = KBTCS  + I                                                 M38.2553
      NDX2 = KXAR + I                                                   M38.2554
      NDX3 = KXAI + I                                                   M38.2555
      ATCS(NDX1) = XTCS( KXTCS + J )                                    M39.2116
      IF ( ATCS(NDX1) .GE. XAR(NDX3) )  GO TO 319                       M39.2117
      XAR(NDX3) = ATCS(NDX1)                                            M39.2118
      NDX4 = KBWKCS + I                                                 M38.2559
      GO TO 3333                                                        M38.2560
  319 IF ( ATCS(NDX1) .LE. XAR(NDX2) )  GO TO 318                       M39.2119
      XAR(NDX2) = ATCS(NDX1)                                            M39.2120
      NDX4 = KAWKCS + I                                                 M38.2563
 3333 AWKCS(NDX4) = T                                                   M38.2564
  318 CONTINUE                                                          M13.1957
 3318 IF ( ISTEP  .LT.  LIMSTP )   GO TO 4321                           M38.2565
      ISPRIN = 0                                                        M19.1199
      IOUT = MULTPR(INDSTP)                                             M19.1200
      INDSTP = INDSTP + 1                                               M19.1201
      LIMSTP = KPRCHG(INDSTP)                                           M19.1202
 4321 IF ( ISPRIN  .GT.  0 )   GO TO 320                                M19.1203
      IF ( LASTOV  .GT.  1 )   GO TO 320                                M18. 709
      ISPRIN = IOUT                                                     M22.3986
      NDX1 = KBTCS + 1                                                  M38.2566
      NDX2 = KBTCS + 9                                                  M38.2567
      IF (IOUTCS .LT. 9) NDX2 = KBTCS + IOUTCS                          M38.2568
      WRITE (LUNIT6, 321) ISTEP, T, (ATCS(I), I=NDX1, NDX2)             M39.2121
  321 FORMAT ( 1X, I5, E13.5, 9E12.5 )                                  M28.4506
      NDX1 = KBTCS + IOUTCS                                             M38.2570
32819 IF (NDX2 .GE. NDX1) GO TO 320                                     M22.3992
      NDX3 = NDX2 + 1                                                   M22.3993
      NDX2 = NDX2 + 9                                                   M22.3994
      IF (NDX2 .GT. NDX1) NDX2 = NDX1                                   M22.3995
      WRITE (LUNIT6, 322) (ATCS(I), I=NDX3, NDX2)                       M39.2122
  322 FORMAT ( 19X, 9E12.5 )                                            M28.4508
      GO TO 32819                                                       M22.3997
  320 IF ( ISPLOT  .GT.  0 )   GO TO 310                                M13.1967
      IF ( NTOT  .GE.  2 )   GO TO 310                                  M13.1968
      NDX1 = KBTCS + 1                                                  M38.2572
      IF ( M4PLOT  .EQ.  0 )   GO TO 1648                               M32.4232
      VOLTI(1) = T                                                      M32.4233
      CALL MOVER ( ATCS(NDX1), VOLTI(2), IOUTCS )                       M39.2123
      NDX2 = IOUTCS + 1                                                 M32.4235
      CALL PLTFIL ( NDX2 )                                              M32.4236
      GO TO 1649                                                        M32.4237
 1648 NDX2 = KBTCS + IOUTCS                                             M38.2574
      WRITE (LUNIT4) T, (ATCS(I), I=NDX1, NDX2)                         M39.2124
 1649 ISPLOT = IPLOT                                                    M32.4239
C                                                $$$  TIME  CONTROL  $$$M13.1972
  310 IF ( T  .GT.  TMAX )   GO TO 900                                  M13.1973
      IF ( IPRSUP  .GE.  3 )                                            M20.3310
     1 WRITE (LUNIT6, 8461)   ISTEP, ISPRIN, ISPLOT, IAC, IDCTCS        M20.3311
 8461 FORMAT ( /,  37H IN  'TACS2' ,   INCREMENT  'ISTEP' .,            M20.3312
     1 40H   ISTEP  ISPRIN  ISPLOT     IAC  IDCTCS   ,/,                M20.3313
     2  37X,  7I8  )                                                    M20.3314
      ISTEP = ISTEP + 1                                                 M13.1974
      T = ISTEP * DELTAT                                                M13.1975
      IF ( T .LT. SPTACS(29) ) GO TO 1945                               M42.6593
      SPTACS(29) = FLTINF                                               M42.6594
      IUTY(KIUTY+3) = IUTY(KIUTY+2)                                     M42.6595
 1945 ISPRIN = ISPRIN - 1                                               M42.6596
      ISPLOT = ISPLOT - 1                                               M13.1977
      IF ( IAC .EQ. 1 )  GO TO 350                                      M13.1978
      IF ( IDCTCS .EQ. 0 )  GO TO 328                                   M13.1979
 4066 IF ( NSUP .EQ. 0 )  GO TO 406                                     M39.2125
      DO 3037 I=1, NSUP                                                 M39.2126
      N1 = - INSUP( KJSUP + I )                                         M39.2127
      IF ( N1 .LT. 0 )  GO TO 3037                                      M39.2128
      MJM = INSUP( KKSUP + I )                                          M39.2129
      IF ( MJM .NE. 53  .AND.  MJM .NE. 66 )  GO TO 3037                M39.2130
      J = IVARB( N1 + 1 )                                               M39.2131
      K = IVARB( N1 + 2 )                                               M39.2132
      B = 0.0                                                           M39.2133
      DO 3038 N= J, K                                                   M39.2134
      M = KSUS( KALKSU + N )                                            M39.2135
      N2 = KSUS( KKSUS + N )                                            M39.2136
 3038 B = B + XTCS( KXTCS + M ) * N2                                    M39.2137
      NN = IVARB(N1)                                                    M39.2138
      M = PARSUP(NN)                                                    M39.2139
      N = PARSUP( NN + 2 )                                              M39.2140
      IF ( MJM  .EQ.  53 )  GO TO 4058                                  M39.2141
      N = IVARB( N1 + 3 )                                               M39.2142
 4058 DO 3039 J=1, N                                                    M39.2143
      NDX1 = KPRSUP + M + N - J                                         M39.2144
      IF ( MJM .EQ. 66 ) NDX1 = NN + N - J + 1                          M39.2145
 3039 PARSUP(NDX1) = PARSUP(NDX1) + B                                   M39.2146
 3037 CONTINUE                                                          M39.2147
      GO TO 406                                                         M39.2148
  350 IAC = 0                                                           M13.1981
      GO TO 256                                                         M13.1982
C                                                  $$$  UPDATE  HST  $$$M13.1983
  328 NUKI = KISBLK - 8                                                 M38.2576
      NUKR = KRSBLK - 4                                                 M42.6597
      DO 3111 I = 1, NUK                                                M39.2149
      NUKI = NUKI + 8                                                   M38.2578
      NUKR = NUKR + 4                                                   M42.6598
      IF ( ISBLK(NUKI+2)  .EQ.  1 ) GO TO 3111                          M39.2150
      L = ISBLK( NUKI + 4 )                                             M38.2580
      PRX = XTCS( KXTCS + L )                                           M37.4322
      J = IABS( ISBLK(NUKI+1) )                                         M42.6599
      K = IABS( ISBLK(NUKI+9) ) - 1                                     M42.6600
      IF ( I .EQ. NUK ) K = NSU                                         M37.4325
      IF ( ISBLK(NUKI+1) .GT. 0 )  GO TO 2345                           M42.6601
      DO 3456 I1 = MA1, MA2                                             M42.6602
      IF ( IVARB(I1) .NE. -I )  GO TO 3456                              M42.6603
      MA3 = I1 + 1                                                      M42.6604
      DO 4567 I2 = MA3, MA2                                             M42.6605
      IF ( IVARB(I2) .LT. 0 ) GO TO 2345                                M42.6606
 4567 MA4 = I2                                                          M42.6607
      GO TO 2345                                                        M42.6608
 3456 CONTINUE                                                          M42.6609
 2345 PRU = 0.0                                                         M42.6610
      DO 312 M=J,K                                                      M13.1991
      N = KSUS( KALKSU + M )                                            M38.2583
      IF ( ISBLK(NUKI+1) .GT. 0 ) GO TO 5678                            M42.6611
      N1 = N - LSTAT(64) - NUK                                          M42.6612
      DO 6789 I3 = MA3, MA4                                             M42.6613
      IF ( IVARB(I3) .NE. N1 ) GO TO 6789                               M42.6614
      N = N + LSTAT(68)                                                 M42.6615
      GO TO 5678                                                        M42.6616
 6789 CONTINUE                                                          M42.6617
 5678 NDX1 = KXTCS  + N                                                 M42.6618
      NDX2 = KKSUS  + M                                                 M22.4012
      PRU = PRU + XTCS(NDX1) * KSUS(NDX2)                               M22.4013
  312 CONTINUE                                                          M13.1994
      JCM = KPRSUP + ISBLK( NUKI + 3 )                                  M39.2151
      K = ISBLK(NUKI+2) - 1                                             M39.2152
      J = JCM + K * 6 - 2                                               M39.2153
      NDX1 = KRSBLK + L * 4 - 2                                         M38.2584
      NDX2 = NDX1 + 1                                                   M38.2585
      IF ( PRX .EQ. RSBLK( NDX1)  .OR.  PRX .EQ. RSBLK( NDX2) )         M42.6619
     1 GO TO 2810                                                       M42.6620
      IF ( K .EQ. 1 )  GO TO 330                                        M13.1998
      N22 = JCM + 6                                                             
      DO 331 N = N22, J, 6                                              M39.2155
  331 PARSUP(N-2)=PARSUP(N)*PRU-PARSUP(N+1)*PRX-PARSUP(N-2)+PARSUP(N+4) M39.2156
  330 PARSUP(J) = PARSUP(J+2) * PRU - PARSUP(J+3) * PRX                 M39.2157
      GO TO 311                                                         M42.6621
C                               $$$  DYNAMIC  LIMITER  HST  $$$         M42.6622
 2810 N = J                                                             M42.6623
      PARSUP(J) = PARSUP(J+2) * PRU  -  PARSUP(J+3) * PRX               M42.6624
 2840 IF ( K .EQ. 1 )  GO TO 311                                        M42.6625
      K = K - 1                                                         M42.6626
      N = N - 6                                                         M42.6627
      PARSUP(N) = (PARSUP(N+2)*PRU-PARSUP(N+3)*PRX+PARSUP(N+6))/2.0     M42.6628
      GO TO 2840                                                        M42.6629
  311 IF ( IPRSUP  .GE.  4 )  WRITE ( LUNIT6, 2020 )                    M39.2163
     1   I, ( PARSUP(N+4), N = JCM, J, 6 )                              M39.2164
 2020 FORMAT( 10H  FUNCTION , I5, 6H  HST , 7E14.6 )                    M39.2165
 3111 CONTINUE                                                          M39.2166
      NDX1 = KXTCS + NUK + LSTAT(64) + 1                                M42.6630
      NDX2 = NDX1 + LSTAT(68)                                           M42.6631
      CALL MOVER ( XTCS(NDX1), XTCS(NDX2), NSUP )                       M42.6632
      GO TO 250                                                         M13.2010
C                                                  $$$  TERMINATION  $$$M13.2011
  900 IF ( IPRSUP  .GE.  1 )                                            M13.2012
     1 WRITE(LUNIT6,901)                                                M28.4539
  901 FORMAT(37H0NORMAL TERMINATION WITHIN  'TACS2' .   )               M13.2014
      IF ( ISTEP .EQ. 1 )  GO TO 903                                    M13.2021
      IF ( IOUTCS .EQ. 0 )  GO TO 903                                   M13.2022
      IF ( BEGMAX(1)  .LE.  0.0 )   GO TO 903                           M31.2768
      IF ( NOUTPR  .NE.  0 )   GO TO 903                                M20.3315
      IF ( BEGMAX(1)  .EQ.  0.0 )                                       M35.1312
     1 WRITE (LUNIT6, 904)                                              M35.1313
  904 FORMAT ( /, 47H MINIMA AND MAXIMA OF ALL TACS OUTPUT VARIABLES  ) M13.2025
      IF ( BEGMAX(1)  .EQ.  1.0 )                                       M31.2769
     1 WRITE (LUNIT6, 42905)                                            M35.1314
42905 FORMAT ( /, 37H MINIMA AND MAXIMA OF ALL TACS OUTPUT,             M35.1315
     1            29H VARIABLES  (EXTREMA OVER ALL,                     M35.1316
     2            40H SOLUTION POINTS, SINCE  'MAXOUT'  IS 1)   )       M35.1317
      IF ( BEGMAX(1)  .EQ.  2.0 )                                       M31.2770
     1 WRITE (LUNIT6, 52905)                                            M35.1318
52905 FORMAT ( /, 37H MINIMA AND MAXIMA OF ALL TACS OUTPUT,             M35.1319
     1            30H VARIABLES  (EXTREMA OVER JUST,                    M35.1320
     2            33H PRINTED OR PLOTTED POINTS, SINCE,                 M35.1321
     3            15H"MAXOUT"  IS 2)      )                             M35.1322
      WRITE (LUNIT6, 62905)                                             M17. 674
62905 FORMAT ( 7X, 3HROW, 6X, 4HNAME, 8X, 7HMAXIMUM, 8X,                M13.2035
     1 7HMINIMUM, 9X, 11HTIME OF MAX, 4X,  11HTIME OF MIN    )          M13.2036
      DO 905 I=1,IOUTCS                                                 M13.2037
      NDX1 = JOUT( KJOUT + I )                                          M38.2593
      NDX1 = ILNTAB( KLNTAB + NDX1 )                                    M38.2594
      NDX2 = KXAR  + I                                                  M38.2595
      NDX3 = KXAI  + I                                                  M38.2596
      NDX4 = KAWKCS + I                                                 M38.2597
      NDX5 = KBWKCS + I                                                 M38.2598
  905 WRITE(LUNIT6,906) I, TEXVEC(NDX1),XAR(NDX2),XAR(NDX3),            M39.2167
     1     AWKCS(NDX4),AWKCS(NDX5)                                      M39.2168
  906 FORMAT ( I10, 4X, A6, 2E15.6, 5X, 2F15.6  )                       M13.2039
  903 LASTOV = NCHAIN                                                   M15. 925
      NCHAIN = 20                                                       M15. 926
      LSTAT(32) = IOUTCS                                                M19.1204
      NENERG = 0                                                        M19.1205
      KBASE = 0                                                         M19.1206
      BEGMAX(1) = 0.0                                                   M31.2771
      IPUNCH = 0                                                        M19.1208
      GO TO 9000                                                        M20.3316
C                                           $$$  AC  STEADY - STATE  $$$M13.2048
C     ***  FORM  PAC  ***                                               M13.2049
 3000 MDY5 = KUD1 - 5                                                   M39.2169
      DO 8888 MJM = NIUNRS, NIU                                         M39.2170
      MDY5 = MDY5 + 5                                                   M39.2171
      IF ( UD1(MDY5+4) .GE. 0.0 ) GO TO 8888                            M39.2172
      IF ( UD1(MDY5+3) .LE. 0.0 ) GO TO 8888                            M39.2173
      MRM = IUTY( KIUTY + MJM )                                         M39.2174
      IF (MRM .NE. 14 .AND. MRM .NE. 90 .AND. MRM .NE. 91) GO TO 8888   M39.2175
      FREQHZ = UD1( MDY5 + 3 )                                          M39.2176
      OMEGAR = FREQHZ * TWOPI                                           M39.2177
      NDX1 = KXAR   + 1                                                 M39.2178
      NDX2 = LSTAT(68) + LSTAT(68)                                      M39.2179
      CALL MOVER0 ( XAR(NDX1), NDX2 )                                   M38.2602
      NUKI = KISBLK - 8                                                 M38.2604
      NUKR = KRSBLK - 4                                                 M38.2605
      DO 3002 I = 1, NUK                                                M37.4351
      NUKI = NUKI + 8                                                   M38.2606
      NUKR = NUKR + 4                                                   M38.2607
      IF ( ISBLK(NUKI+2)  .EQ.  1 )  GO TO 3002                         M38.2608
      J = KPRSUP + ISBLK(NUKI+3)                                        M39.2180
      M = ISBLK(NUKI+2) * 6 + J - 4                                     M39.2181
      NDX1 = KXAR + I                                                   M37.4355
      XAR(NDX1) = PARSUP(J)                                             M39.2182
      NDX3 = KXAI + I                                                   M37.4357
      XAR(NDX3) = 0.0                                                   M39.2183
      NDX2 = NDX1 + NUK                                                 M37.4359
      XAR(NDX2) = PARSUP(J+1)                                           M39.2184
      NDX4 = NDX3 + NUK                                                 M37.4361
      XAR(NDX4) = 0.0                                                   M39.2185
      N1 = 0                                                            M13.2059
      D1 = 1.0                                                          M13.2060
      N22 = J + 6                                                               
      DO 3332 K = N22, M, 6                                             M39.2187
      D1 = D1 * OMEGAR                                                  M37.4364
      IF ( N1  .EQ.  0 )   GO TO 13000                                  M13.2063
      N1 = 0                                                            M13.2064
      D1 = -D1                                                          M13.2065
      XAR(NDX1) = XAR(NDX1)  +  PARSUP(K) * D1                          M39.2188
      XAR(NDX2) = XAR(NDX2)  +  PARSUP(K+1) * D1                        M39.2189
      GO TO 3332                                                        M39.2190
13000 N1 = 1                                                            M13.2069
      XAR(NDX3) = XAR(NDX3)  +  PARSUP(K) * D1                          M39.2191
      XAR(NDX4) = XAR(NDX4)  +  PARSUP(K+1) * D1                        M39.2192
 3332 CONTINUE                                                          M39.2193
 3002 CONTINUE                                                          M13.2072
      IF ( IPRSUP  .LE.  1 )   GO TO 3004                               M13.2073
      WRITE (LUNIT6, 1372)  NUK, ( I, ISBLK(NUKI+3),ISBLK(NUKI+2),      M38.2611
     1   XAR(KXAR+I), XAR(KXAI+I),                                      M39.2194
     2  XAR(KXAR+NUK+I), XAR(KXAI+NUK+I), I = 1,  NUK )                 M39.2195
 1372 FORMAT ( /, 46H 'TACS2' TABLES BEFORE AC STEADY-STATE.  NUK =,    M37.4372
     1  I4, /, 1X, 24H     ROW    KFST     KNI, 10X, 5HXAR-1, 10X,      M37.4373
     2  5HXAI-1, 10X, 5HXAR-2, 10X, 5HXAI-2, /, ( 1X, 3I8, 4E15.6 ) )   M37.4374
C     ***  FORM  SPARSE  MATRIX  ***                                    M13.2082
 3004 IA = 0                                                            M37.4375
      NUKI = KISBLK - 8                                                 M38.2612
      DO 3260  I=1, NUK                                                 M13.2086
      NUKI = NUKI + 8                                                   M38.2613
      ISBLK(NUKI+8) = IA + 1                                            M38.2614
      NDX1 = KAWKCS + 1                                                 M22.4074
      CALL MOVER0 ( AWKCS(NDX1), KTAB )                                 M37.4376
      NDX1 = KBWKCS + 1                                                 M22.4076
      CALL MOVER0 ( AWKCS(NDX1), KTAB )                                 M39.2196
      NUKJ = KISBLK - 8                                                 M38.2615
      NUKS = KRSBLK - 4                                                 M38.2616
      DO 3222  J = 1, NUK                                               M37.4378
      NUKJ = NUKJ + 8                                                   M38.2617
      NUKS = NUKS + 4                                                   M38.2618
      K1 = ISBLK( NUKJ + 4 )                                            M38.2619
      IF ( K1  .NE.  I )   GO TO 3222                                   M37.4380
      J1 = IABS( ISBLK(NUKJ+1) )                                        M42.6633
      K1 = IABS( ISBLK(NUKJ+9) ) - 1                                    M42.6634
      IF ( J  .EQ.  NUK )  K1 = NSU                                     M37.4383
      IF ( ISBLK(NUKJ+2) .EQ. 1 ) GO TO 3218                            M38.2622
      NDX1 = KAWKCS + I                                                 M22.4080
      NDX2 = KXAR + NUK +  J                                            M37.4385
      AWKCS(NDX1) = XAR(NDX2)                                           M37.4386
      NDX1 = KBWKCS + I                                                 M22.4083
      NDX2 = KXAI + NUK + J                                             M37.4387
      AWKCS(NDX1) = XAR(NDX2)                                           M39.2197
      NDY1 = KXAR + J                                                   M39.2198
      NDY2 = KXAI + J                                                   M39.2199
      DO 3219  M=J1, K1                                                 M13.2098
      L = KSUS( KALKSU + M )                                            M38.2623
      D1 = KSUS( KKSUS + M )                                            M37.4390
      NDX1 = KAWKCS + L                                                 M22.4094
      AWKCS(NDX1) = -XAR(NDY1)*D1                                       M39.2200
      NDX1 = KBWKCS + L                                                 M22.4097
 3219 AWKCS(NDX1) = -XAR(NDY2)*D1                                       M39.2201
      GO TO 3221                                                        M37.4395
 3218 NDX1 = KAWKCS + I                                                 M37.4396
      AWKCS(NDX1) = 1.0                                                 M22.4103
      NDX1 = KBWKCS + I                                                 M22.4104
      AWKCS(NDX1) = 0.0                                                 M39.2202
      D1 = RSBLK( NUKS + 1 )                                            M38.2624
      DO 3223  M=J1, K1                                                 M13.2117
      L = KSUS( KALKSU + M )                                            M38.2625
      NDX1 = KAWKCS + L                                                 M22.4114
      NDX2 = KKSUS  + M                                                 M22.4115
      AWKCS(NDX1) = -D1 * KSUS(NDX2)                                    M22.4116
      NDX1 = KBWKCS + L                                                 M22.4117
 3223 AWKCS(NDX1) = 0.0                                                 M39.2203
      GO TO 3221                                                        M13.2125
 3222 CONTINUE                                                          M13.2126
 3221 IF ( IPRSUP  .GE.  2 )                                            M37.4399
     1 WRITE (LUNIT6, 43219)  I, J, IA, KTAB, NUK, J1, K1               M37.4400
43219 FORMAT ( /,  9H AT 43219,  56H       I       J      IA    KTAB    M37.4401
     1 NUK      J1      K1   ,/, 9X, 7I8 )                              M37.4402
      IF ( IPRSUP  .GE.  2 )                                            M37.4403
     1 WRITE (LUNIT6, 2022) I, (J, AWKCS(KAWKCS+J), AWKCS(KBWKCS+J),    M39.2204
     2     J = 1, KTAB )                                                M37.4405
 2022 FORMAT(5H0ROW ,I3,(1X,3(I3,2H: ,E13.6,6H  + J ,E13.6)))           M31.2772
C     ***  TRIANGULARIZE EACH ROW  ***                                  M13.2130
 3224 NDX1 = KAWKCS + I                                                 M22.4122
      NDX2 = KBWKCS + I                                                 M22.4123
      D4 = AWKCS(NDX1)**2  +  AWKCS(NDX2)**2                            M39.2205
      IF ( I  .EQ.  1 )   GO TO 3261                                    M13.2132
      M = I - 1                                                         M13.2133
      DO 3262  K=1, M                                                   M13.2134
      NDX1 = KAWKCS + K                                                 M22.4125
      IF ( AWKCS(NDX1)  .NE.  0.0 )   GO TO 13263                       M22.4126
      NDX1 = KBWKCS + K                                                 M22.4127
      IF ( AWKCS(NDX1)  .EQ.  0.0 )   GO TO 3262                        M39.2206
13263 N = K + 1                                                         M13.2137
      NDX1 = KISBLK + K * 8                                             M38.2626
      K1 = ISBLK(NDX1) - 1                                              M38.2627
      K2 = ISBLK(NDX1+8)                                                M38.2628
 3263 K1 = K1 + 1                                                       M13.2140
      IF ( K1  .EQ.  K2 )   GO TO 3262                                  M13.2141
      NDX1 = KCOLCS + K1                                                M22.4133
      IF ( ICOLCS(NDX1)  .LT. N )   GO TO 3263                          M22.4134
      NDX1 = KCOLCS + K1                                                M22.4135
      J = ICOLCS(NDX1)                                                  M22.4136
      NDX1 = KAWKCS + K                                                 M22.4137
      D1 = AWKCS(NDX1)                                                  M22.4138
      NDX1 = KBWKCS + K                                                 M22.4139
      D2 = AWKCS(NDX1)                                                  M39.2207
 3264 NDX1 = KAWKCS + J                                                 M22.4141
      NDX3 = KATCS  + K1                                                M22.4142
      NDX4 = KBTCS  + K1                                                M22.4143
      AWKCS(NDX1)  =  AWKCS(NDX2)  -  ( D1*ATCS(NDX3) - D2*ATCS(NDX4) ) M39.2208
      NDX1 = KBWKCS + J                                                 M22.4145
      AWKCS(NDX1)  =  AWKCS(NDX1)  -  ( D1*ATCS(NDX3) + D2*ATCS(NDX4) ) M39.2209
      K1 = K1 + 1                                                       M13.2148
      IF ( K1  .EQ.  K2 )   GO TO 3262                                  M13.2149
      NDX1 = KCOLCS + K1                                                M22.4147
      J = ICOLCS(NDX1)                                                  M22.4148
      GO TO 3264                                                        M13.2151
 3262 CONTINUE                                                          M13.2152
 3261 NDX1 = KAWKCS + I                                                 M22.4149
      NDX2 = KBWKCS + I                                                 M22.4150
      D2 = AWKCS(NDX1)**2  +  AWKCS(NDX2)**2                            M39.2210
      IF ( D4  .EQ.  0.0 )   GO TO 13261                                M13.2154
      IF ( D2/D4  .GT.  TOLMAT )   GO TO 3265                           M13.2155
13261 KILL = 129                                                        M13.2156
      NDX1 = ILNTAB( KLNTAB + I )                                       M38.2629
      BUS1 = TEXVEC(NDX1)                                               M38.2630
      LSTAT(14) = I                                                     M13.2158
      FLSTAT(14) = D4                                                   M13.2159
      FLSTAT(15) = D2                                                   M13.2160
      LSTAT(19) = 3265                                                  M13.2161
      WRITE (LUNIT6, 3267)  BUS1                                        M38.2631
 3267 FORMAT (111H0ERROR.  YOUR SYSTEM IS UNSTABLE IN STEADY-STATE.  YOUM13.2163
     1 WILL FIND THE ERROR IN A FEEDBACK LOOP INVOLVING BLOCK =,A6,2H=.)M13.2164
      GO TO 9000                                                        M13.2165
 3265 NDX1 = KAWKCS + I                                                 M22.4156
      D3 =  AWKCS(NDX1) / D2                                            M22.4157
      NDX1 = KBWKCS + I                                                 M22.4158
      D4 = -AWKCS(NDX1) / D2                                            M39.2211
      NDX1 = KAWKCS + I                                                 M22.4160
      AWKCS(NDX1) = D3                                                  M22.4161
      NDX1 = KBWKCS + I                                                 M22.4162
      AWKCS(NDX1) = D4                                                  M39.2212
C     ***  APPEND TO COMPACTED VECTOR  ***                              M13.2170
      DO 3268  J = 1, KTAB                                              M37.4406
      NDX1 = KAWKCS + J                                                 M22.4164
      IF ( AWKCS(NDX1)  .NE.  0.0 )   GO TO 13265                       M22.4165
      NDX1 = KBWKCS + J                                                 M22.4166
      IF ( AWKCS(NDX1)  .EQ.  0.0 )   GO TO 3268                        M39.2213
13265 IA = IA + 1                                                       M13.2174
      IF ( IA  .LE.  LSTAT(62) )   GO TO 8127                           M39.2214
      KILL = 122                                                        M13.2176
      LSTAT( 16) = LSTAT(62)                                            M39.2215
      LSTAT( 17) = 2                                                    M39.2216
      LSTAT(19) = 8127                                                  M13.2179
      GO TO 9000                                                        M13.2180
 8127 NDX1 = KCOLCS + IA                                                M22.4168
      ICOLCS(NDX1) = J                                                  M22.4169
      NDX1 = KATCS  + IA                                                M22.4170
      NDX2 = KAWKCS + J                                                 M22.4171
      ATCS(NDX1) = AWKCS(NDX2)                                          M22.4172
      NDX1 = KBTCS  + IA                                                M22.4173
      NDX2 = KBWKCS + J                                                 M22.4174
      ATCS(NDX1) = AWKCS(NDX2)                                          M39.2217
      IF ( J  .LE.  I )   GO TO 3268                                    M13.2184
      NDX1 = KATCS  + IA                                                M22.4176
      NDX2 = KBTCS  + IA                                                M22.4177
      D1        =  ATCS(NDX1) * D3  -  ATCS(NDX2) * D4                  M39.2218
      ATCS(NDX2)  =  ATCS(NDX1) * D4  +  ATCS(NDX2) * D3                M39.2219
      ATCS(NDX1)  =  D1                                                 M22.4180
 3268 CONTINUE                                                          M13.2188
 3260 CONTINUE                                                          M13.2189
      IF ( IPRSUP  .LE.  1 )   GO TO 3019                               M13.2190
      WRITE (LUNIT6, 2023)  IA, (ISBLK(KISBLK+N*8), N=1, NUK)           M38.2634
      WRITE (LUNIT6, 2025)  ( ICOLCS(KCOLCS+M), ATCS(KATCS+M),          M37.4410
     1    ATCS(KBTCS+M),  M=1, IA )                                     M39.2220
 2025 FORMAT(/,  19H ICOLCS, ATCS, BTCS   ,/,                           M13.2193
     1                           (1X,3(I3,2H: ,E13.6,6H  + J ,E13.6)))  M31.2773
      WRITE (LUNIT6, 13019)                                             M17. 682
13019 FORMAT ( //,  35H CALC. OF PHASOR FORCING FUNCTIONS.   ,/,        M13.2196
     1  1X, 32H     ROW       J  IDCTCS     NIU,                        M39.2221
     2 9X, 6HXAR(J), 9X, 6HXAI(J)    )                                  M13.2198
C     ***  SET  INPUTS  ***                                             M13.2199
 3019 NDX1 = KXAR   + 1                                                 M37.4412
      CALL MOVER0 ( XAR(NDX1), KTAB )                                   M37.4413
      NDX1 = KXAI   + 1                                                 M37.4414
      CALL MOVER0 ( XAR(NDX1), KTAB )                                   M39.2222
      NDY5 = KUD1 - 5                                                   M37.4416
      DO 3020  I = NIUNRS, NIU                                          M28.4559
      NDY5 = NDY5 + 5                                                   M37.4417
      IF ( UD1(NDY5+4) .GE. 0.0 )  GO TO 3020                           M37.4418
      IF ( UD1(NDY5+3) .NE. FREQHZ ) GO TO 3020                         M39.2223
      UD1(NDY5+3) = - UD1(NDY5+3)                                       M39.2224
      J = I + NUK                                                       M13.2201
      NDXI = IUTY( KIUTY + I )                                          M37.4419
      NDX2 = KXAI + J                                                   M37.4420
      NDX3 = KXAR + J                                                   M37.4421
      IF ( NDXI .EQ. 14 ) GO TO 3003                                    M39.2225
      IF ( NDXI .EQ. 90  .OR.  NDXI .EQ. 91 ) GO TO 3001                M37.4423
      GO TO 3020                                                        M13.2205
 3001 K = UD1( NDY5 + 2 )                                               M39.2226
      IF ( NDXI  .EQ.  91 ) GO TO 3131                                  M37.4426
      XAR(NDX3) = E( K)                                                 M37.4427
      XAR(NDX2) = F( K)                                                 M39.2227
      GO TO 3030                                                        M37.4429
 3131 IF ( NEXTSW(K)  .NE.  87 )  GO TO 3020                            M37.4430
      XAR(NDX3) = TCLOSE(K)                                             M37.4431
      XAR(NDX2) = ENERGY(K)                                             M39.2228
      GO TO 3030                                                        M37.4433
 3003 D1 = UD1( NDY5 + 1 )                                              M37.4434
      D2 = UD1( NDY5 + 2 )                                              M37.4435
      XAR(NDX3) = D1 * COSZ(D2)                                         M37.4436
      XAR(NDX2) = D1 * SINZ(D2)                                         M39.2229
 3030 IF ( IPRSUP  .GE.  1 )  WRITE ( LUNIT6, 13001 )                   M37.4438
     1   I, J, IDCTCS, NIU, XAR(NDX3), XAR(NDX2)                        M39.2230
13001 FORMAT ( 1X, 4I8, 2E15.6 )                                        M39.2231
 3020 CONTINUE                                                          M13.2213
      IF ( IPRSUP  .GE.  1 )                                            M22.4217
     1  WRITE (LUNIT6, 13020) (XAR(KXAR+I), XAR(KXAI+I), I=1, KTAB)     M39.2232
13020 FORMAT ( /,  28H (XAR(I), XAI(I), I=1, KTAB)  ,/,  (1X, 8E16.6) ) M39.2233
 3412 ITE = ITE + 1                                                     M39.2234
      IPASS = 0                                                         M39.2235
      IF ( ITE  .LE.  100 ) GO TO 3515                                  M39.2236
      WRITE ( LUNIT6, 7798 ) ITE                                        M39.2237
      GO TO 3618                                                        M39.2238
 3515 L = IUTY( KIUTY + 4 )                                             M39.2239
      IF ( L  .GT.  0 ) CALL CSUPAC( L, OMEGAR )                        M39.2240
C     ***  BACKWARD ON XA  ***                                          M13.2221
      NUKI = KISBLK - 8                                                 M38.2637
      DO 7224 I=1,NUK                                                   M39.2241
      NUKI = NUKI + 8                                                   M38.2638
      J = NUK + 1 - I                                                   M13.2223
      NUKJ = KISBLK + J * 8 - 8                                         M38.2639
 3329 NDX1 = KXAR   + J                                                 M22.4219
      XARSAV = XAR(NDX1)                                                M39.2242
      XAR(NDX1) = 0.0                                                   M22.4220
      NDX2 = KXAI   + J                                                 M39.2243
      XAISAV = XAR(NDX2)                                                M39.2244
      XAR(NDX2) = 0.0                                                   M39.2245
      M = ISBLK( NUKJ + 8 )                                             M38.2640
      N = IA                                                            M13.2232
      IF ( J .NE. NUK )  N = ISBLK(NUKJ + 16) - 1                       M38.2641
 3309 NDX3 = KCOLCS + M                                                 M39.2246
      IF ( ICOLCS(NDX3) .EQ. J )  GO TO 3334                            M39.2247
      M = M + 1                                                         M13.2235
      GO TO 3309                                                        M13.2236
 3334 M = M + 1                                                         M13.2237
      IF ( M .GT. N )  GO TO 3024                                       M14.1869
      K = ICOLCS( KCOLCS + M )                                          M39.2248
      IF ( K .GT. NUK + NIU )  GO TO 3024                               M14.1870
      D1 = XAR( KXAR + K )                                              M39.2249
      D2 = XAR( KXAI + K )                                              M39.2250
      NDX3 = KATCS  + M                                                 M22.4236
      NDX4 = KBTCS  + M                                                 M22.4237
      XAR(NDX1) = XAR(NDX1) - ( ATCS(NDX3) * D1 - ATCS(NDX4) * D2 )     M39.2251
      XAR(NDX2) = XAR(NDX2) - ( ATCS(NDX3) * D2 + ATCS(NDX4) * D1 )     M39.2252
      IF ( IPRSUP  .GE.  2 )                                            M22.4245
     1      WRITE (LUNIT6, 13334)  I, J, K, M, N, D1, D2, XAR(NDX1),    M22.4246
     2     XAR(NDX2)                                                    M39.2253
13334 FORMAT ( 9H AT 13334, 5I8, 4E20.6 )                               M13.2246
      GO TO 3334                                                        M13.2247
 3024 L = ISBLK( NUKJ + 7 )                                             M39.2254
      IF ( L  .GT.  0 ) CALL CSUPAC( L, OMEGAR )                        M39.2255
      DEFR = ABSZ( XARSAV - XAR(NDX1) )                                 M39.2256
      DEFI = ABSZ( XAISAV - XAR(NDX2) )                                 M39.2257
      CRITIA = 0.001 * SQRTZ( XAR(NDX1)**2 + XAR(NDX2)**2 )             M39.2258
      IF ( CRITIA .LT. EPSLON ) CRITIA = EPSLON                         M39.2259
      IF ( DEFR .LE. CRITIA .AND. DEFI .LE. CRITIA ) GO TO 7224         M39.2260
      IPASS = 1                                                         M39.2261
 7224 CONTINUE                                                          M39.2262
      IF ( IPASS .EQ. 1 .AND. IUTY(KIUTY+9) .EQ. 1 ) GO TO 3412         M39.2263
 3618 IF ( IPRSUP .LT. 1  .AND.  KSSOUT .EQ. 0 ) GO TO 2389             M39.2264
      WRITE ( LUNIT6, 1357 ) FREQHZ                                     M39.2265
 1357 FORMAT( /, 42H   AC STEADY STATA SOLUTION,  FREQUENCY = , E16.6   M39.2266
     1       ,/, 42H    NAME         REAL PART  IMAGINARY PART,         M39.2267
     2           32H       MAGNITUDE           ANGLE  )                 M39.2268
      MTOT = IOUTCS                                                     M39.2269
      IF ( KSSOUT .EQ. 1 ) MTOT = KTAB                                  M39.2270
      J1 = NUK + NIU                                                    M39.2271
      J2 = NUK + LSTAT(64)                                              M39.2272
      DO 2378 I = 1, MTOT                                               M39.2273
      J = JOUT( KJOUT + I )                                             M39.2274
      IF ( KSSOUT .EQ. 1 ) J = I                                        M39.2275
      IF ( J .GT. J1  .AND.  J .LE. J2 ) GO TO 2378                     M39.2276
      I1 = ILNTAB( KLNTAB + J )                                         M39.2277
      REAL = XAR( KXAR + J )                                            M39.2278
      RIMA = XAR( KXAI + J )                                            M39.2279
      AMAX = SQRTZ( REAL**2 + RIMA**2 )                                 M39.2280
      ANGL = 0.0                                                        M39.2281
      IF ( AMAX .GT. 0.0 ) ANGL = PICON * ATAN2Z( RIMA, REAL )          M39.2282
      WRITE (LUNIT6, 2489) TEXVEC(I1), REAL, RIMA, AMAX, ANGL           M39.2283
 2489 FORMAT ( 2X, A6, 3E16.6, F16.6 )                                  M39.2284
 2378 CONTINUE                                                          M39.2285
 2389 NDX1 = KXTCS + LSTAT(68)                                          M39.2286
      DO 5599 I = 1, KTAB                                               M39.2287
      NDX2 = NDX1 + I                                                   M39.2288
 5599 XTCS(NDX2) = XTCS(NDX2) + XAR( KXAR + I )                         M39.2289
C     ***  HISTORIES  ***                                               M13.2263
      D1 = OMEGAR * DELTAT                                              M37.4444
      D2 = COSZ(D1)                                                     M18. 712
      D3 =-SINZ(D1)                                                     M18. 713
      D1 = D2 + 1.0                                                     M13.2268
      D4 = D1**2  +  D3**2                                              M13.2269
      D5 = D1/D4                                                        M13.2270
      D6 = -D3/D4                                                       M13.2271
      IF ( IPRSUP  .GE.  1 )                                            M13.2272
     1 WRITE (LUNIT6, 1304)  KTAB, OMEGAR, DELTAT, D2, D3, D5, D6       M39.2290
 1304 FORMAT (/, 5X, 4HKTAB, 9X, 6HOMEGAR, 9X, 6HDELTAT, 13X, 2HD2,     M39.2291
     1 13X, 2HD3, 13X, 2HD5, 13X, 2HD6, /, 1X, I8, 6E15.6, /, 1X )      M39.2292
      NUKI = KISBLK - 8                                                 M38.2642
      DO 3032 I=1,NUK                                                   M37.4447
      NUKI = NUKI + 8                                                   M38.2643
      IF ( ISBLK(NUKI+2) .EQ. 1)  GO TO 3032                            M38.2644
      K1 = ISBLK( NUKI + 4 )                                            M38.2645
      J = IABS( ISBLK(NUKI+1) )                                         M42.6635
      K = IABS( ISBLK(NUKI+9) ) - 1                                     M42.6636
      IF ( I .EQ. NUK )  K = NSU                                        M37.4452
      NDXI = KAWKCS + I                                                 M37.4453
      AWKCS(NDXI) = 0.0                                                 M37.4454
      NDXJ = KBWKCS + I                                                 M37.4455
      AWKCS(NDXJ) = 0.0                                                 M39.2293
      IF ( IPRSUP  .GE.  4 )                                            M13.2283
     1 WRITE (LUNIT6, 43033)                                            M17. 689
43033 FORMAT (/, 9X, 40H       I       M       J       K       N, 18X,  M13.2285
     1 2HD1, 12X, 8HAWKCS(I), 12X, 8HBWKCS(I)     )                     M37.4457
      DO 3033 M=J,K                                                     M13.2287
      N = KSUS( KALKSU + M )                                            M38.2648
      D1 = KSUS( KKSUS + M )                                            M37.4459
      AWKCS(NDXI) = AWKCS(NDXI) + XAR(KXAR+N) * D1                      M37.4460
      AWKCS(NDXJ) = AWKCS(NDXJ) + XAR(KXAI+N) * D1                      M39.2294
      IF ( IPRSUP  .GE.  4 )                                            M22.4274
     1      WRITE (LUNIT6, 13033)  I, M, J, K, N, D1, AWKCS(NDXI),      M37.4462
     2     AWKCS(NDXJ)                                                  M39.2295
13033 FORMAT ( 9H AT 13033, 5I8, 3E20.6 )                               M13.2294
 3033 CONTINUE                                                          M13.2295
      KH = ISBLK( NUKI + 2 )                                            M39.2296
      K = KH - 1                                                        M39.2297
      J = ISBLK( NUKI + 3 ) + K * 6                                     M39.2298
      IF ( IPRSUP  .GE.  4 )  WRITE ( LUNIT6, 53033 )                   M37.4468
53033 FORMAT (/, 9X, 48H       I       M       J       K      J1      M1M13.2301
     1  , 12X, 8HHSTR(M1), 12X, 8HHSTI(M1)    )                         M37.4469
      DO 3034 M = 1, K                                                  M37.4470
      NDY1 = KPRSUP + J                                                 M39.2299
      D7 = PARSUP( NDY1 + 2 )                                           M39.2300
      D8 = PARSUP( NDY1 + 3 )                                           M39.2301
      NDX1 = KATCS + KH - M                                             M39.2302
      NDX3 = KXAR   + K1                                                M22.4289
      ATCS(NDX1)  =  D7 * AWKCS(NDXI)  -  D8 * XAR(NDX3)                M37.4474
      NDX4 = KBTCS  + KH - M                                            M39.2303
      NDX3 = KXAI   + K1                                                M22.4293
      ATCS(NDX4)  =  D7 * AWKCS(NDXJ)  -  D8 * XAR(NDX3)                M39.2304
      IF ( IPRSUP  .GE.  4 )                                            M22.4297
     1 WRITE (LUNIT6, 23033) I, J, K, KH, ATCS(NDX1), ATCS(NDX4)        M39.2305
23033 FORMAT ( 9H AT 23033, 4I8, 2E20.6 )                               M37.4478
      J = J - 6                                                         M39.2306
      IF ( M .EQ. 1 )  GO TO 3034                                       M37.4480
      NDX2 = NDX1 + 1                                                   M37.4481
      D7 = ATCS(NDX2)                                                   M37.4482
      NDX3 = NDX4 + 1                                                   M37.4483
      D8 = ATCS(NDX3)                                                   M39.2307
      D1 =  ATCS(NDX1)  +  D7*D2  -  D8*D3                              M37.4485
      D4 =  ATCS(NDX4)  +  D7*D3  +  D8*D2                              M39.2308
      ATCS(NDX1)  =  D1*D5  -  D4*D6                                    M37.4487
      ATCS(NDX4)  =  D1*D6  +  D4*D5                                    M39.2309
      IF ( IPRSUP  .GE.  4 )                                            M22.4314
     1      WRITE (LUNIT6, 33033)  ATCS(NDX1), ATCS(NDX4)               M39.2310
33033 FORMAT ( 57X, 2E20.6 )                                            M13.2322
 3034 PARSUP(NDY1-1) = PARSUP(NDY1-1) + ATCS(NDX1)                      M39.2311
 3032 CONTINUE                                                          M13.2324
      IF ( NSUP .EQ. 0 )  GO TO 8888                                    M39.2312
      DO 7037 I=1, NSUP                                                 M39.2313
      N1 = - INSUP( KJSUP + I )                                         M39.2314
      IF ( N1 .LT. 0 )  GO TO 7037                                      M39.2315
      MK = INSUP( KKSUP + I )                                           M39.2316
      IF ( MK .NE. 53 .AND. MK .NE. 66) GO TO 7037                      M39.2317
      J = IVARB( N1 + 1 )                                               M39.2318
      K = IVARB( N1 + 2 )                                               M39.2319
      BR = 0.0                                                          M39.2320
      BI = 0.0                                                          M39.2321
      DO 7038 N= J, K                                                   M39.2322
      M = KSUS( KALKSU + N )                                            M39.2323
      N2 = KSUS( KKSUS + N )                                            M39.2324
      BR = BR + XAR( KXAR + M ) * N2                                    M39.2325
 7038 BI = BI + XAR( KXAI + M ) * N2                                    M39.2326
      NN = IVARB(N1)                                                    M39.2327
      M = PARSUP(NN)                                                    M39.2328
      N = PARSUP( NN + 2 )                                              M39.2329
      A = OMEGAR * DELTAT                                               M39.2330
      IF ( MK .EQ. 66 ) N = IVARB( N1 + 3 )                             M39.2331
      DO 7039 J=1, N                                                    M39.2332
      NDX1 = KPRSUP + M + N - J                                         M39.2333
      IF ( MK .EQ. 66 ) NDX1 = NN + N - J + 1                           M39.2334
 7039 PARSUP(NDX1) = PARSUP(NDX1) + BR*COSZ(A*J) + BI*SINZ(A*J)         M39.2335
 7037 CONTINUE                                                          M39.2336
 8888 ITE = 0                                                           M39.2337
      MDY5 = KUD1 - 5                                                   M39.2338
      DO 8743 I = NIUNRS, NIU                                           M39.2339
      MDY5 = MDY5 + 5                                                   M39.2340
 8743 IF ( UD1(MDY5+3) .LT. 0.0 ) UD1(MDY5+3) = - UD1(MDY5+3)           M39.2341
      GO TO 405                                                         M39.2342
C     ***  SUPERIMPOSING DC STEADY-STATE  ***                           M39.2343
 3042 NDX1 = KXTCS + LSTAT(68)                                          M39.2344
      DO 5578 I = 1, KTAB                                               M39.2345
      NDX2 = KXTCS + I                                                  M39.2346
 5578 XTCS(NDX2) = XTCS(NDX2) + XTCS( NDX1 + I )                        M39.2347
      NUKI = KISBLK - 8                                                 M39.2348
      DO 6678 I = 1, NUK                                                M39.2349
      NUKI = NUKI + 8                                                   M39.2350
      K = ISBLK(NUKI+2) - 1                                             M39.2351
      IF ( K .EQ. 0 ) GO TO 6678                                        M39.2352
      NDY1 = KPRSUP + ISBLK( NUKI + 3 )                                 M39.2353
      NDY0 = NDY1 + 4                                                   M39.2354
      DO 6666 J = 1, K                                                  M39.2355
      PARSUP(NDY1+4) = PARSUP(NDY1+4) + PARSUP(NDY1+5)                  M39.2356
 6666 NDY1 = NDY1 + 6                                                   M39.2357
      IF ( IPRSUP .GT. 2 ) WRITE ( LUNIT6, 6890 )                       M39.2358
     1    J, ( PARSUP(N), N = NDY0, NDY1, 6 )                           M39.2359
 6890 FORMAT ( 11H  FUNCTION , I5, 6H  HST , 7E14.6  )                  M39.2360
 6678 CONTINUE                                                          M39.2361
      NUKR = KRSBLK - 4                                                 M39.2362
      DO 3035 I=1, NUK                                                  M38.2653
      NUKR = NUKR + 4                                                   M38.2654
      NDX1 = KXTCS  + I                                                 M22.4352
      IF ( XTCS(NDX1) .GE. RSBLK(NUKR+2)  .AND.                         M38.2655
     1   XTCS(NDX1) .LE.   RSBLK(NUKR+3) )   GO TO 3035                 M38.2656
      NDX1 = ILNTAB( KLNTAB +  I )                                      M38.2657
      IF ( IUTY(KIUTY+3) .EQ. 0 )  GO TO 3035                           M42.6637
      IUTY(KIUTY+3) = IUTY(KIUTY+3) - 1                                 M42.6638
      IF ( NOUTPR  .EQ.  0 ) WRITE ( LUNIT6, 3040 ) TEXVEC(NDX1)        M38.2658
 3040 FORMAT( 26H0WARNING.  TACS VARIABLE ', A6,  76H' IS ALLOWED TO OPEM14.1900
     1RATE OUTSIDE ITS LIMITS DURING THE STEADY-STATE SOLUTION. /1X)    M14.1901
 3035 CONTINUE                                                          M14.1902
      GO TO 317                                                         M13.2336
 9000 IF ( IPRSUP  .GE.  1 )                                            M20.3319
     1 WRITE (LUNIT6, 6518)   ISTEP, NCHAIN, IOUTCS, T                  M20.3320
 6518 FORMAT ( /,  16H EXIT  'TACS2' .,                                 M20.3321
     1 24H   ISTEP  NCHAIN  IOUTCS,  14X,  1HT  ,/,                     M20.3322
     2 16X,  3I8,  E15.5  )                                             M20.3323
      RETURN                                                            M20.3324
      END                                                               M13.2338
      SUBROUTINE CSUPDC( L )                                            M39.2363
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      KJSUP = KINSUP + LSTAT(65)                                        M39.2367
      KKSUP = KJSUP  + LSTAT(65)                                        M39.2368
 2001 IF ( IPRSUP .LT. 6 )  GO TO 1000                                  M39.2369
      WRITE(LUNIT6,1001) T, NSUP, KARG, KPAR                            M39.2370
 1001 FORMAT(36H0ENTERING SUBROUTINE  CSUPDC  AT  T=, E13.6   ,/,       M39.2371
     1 8H0E NSUP=, I6, 8H   KARG=, I8,  8H   KPAR=, I6 )                M39.2372
      WRITE (LUNIT6,1002) ( I,  ILNTAB(I+KSPVAR),                       M39.2373
     1  INSUP(I+KJSUP), INSUP(I+KKSUP), I = 1, NSUP )                   M39.2374
 1002 FORMAT( 32H  NUMBER  SUPVAR    JSUP    KSUP ,/, (4I8) )           M39.2375
      WRITE ( LUNIT6, 1033 ) KARG                                       M39.2376
 1033 FORMAT( 9H  KARG = , I8 ,/, 24H       N  IOPSUP  IFNSUP,          M39.2377
     1  48H  IRGSUP    IDEV     KDJ     KDK  ILDEV1  ILDEV2  )          M39.2378
      DO 2034  I = 1, NSUP                                              M39.2379
      N1 = INSUP( KJSUP + I )                                           M39.2380
      IF (  N1  .LT.  0 )  GO TO 2014                                   M39.2381
      N2 = INSUP( KKSUP + I )                                           M39.2382
      WRITE(LUNIT6,2008) (N,IVARB(N+1),IVARB(N+2),                      M39.2383
     2 IVARB(N+2), N = N1, N2, 3)                                       M39.2384
 2008 FORMAT( 4I8 )                                                     M39.2385
      GO TO 2034                                                        M39.2386
 2014 N1 = -N1                                                          M39.2387
      WRITE( LUNIT6, 2022 ) N1, IVARB(N1), IVARB(N1+1),                 M39.2388
     1 IVARB(N1+2), IVARB(N1+3), IVARB(N1+4)                            M39.2389
 2022 FORMAT( I8, 24X, 5I8 )                                            M39.2390
 2034 CONTINUE                                                          M39.2391
      IF ( KPAR .NE. 0 )                                                M39.2392
     1      WRITE (LUNIT6,1004) ( I, PARSUP(I+KPRSUP), I=1, KPAR )      M39.2393
 1004 FORMAT( 2H0E, 5X, 10HPARSUP ...            ,/,                    M39.2394
     1 (3H E , 5(I3,  1X, E15.6, 3X)))                                  M39.2395
 1000 NNN = KXTCS + NUK + LSTAT(64)                                     M39.2396
      I = L                                                             M39.2397
 1234 A = 0.0                                                           M39.2398
      N1 = -INSUP( KJSUP + I )                                          M39.2399
      N2 =  INSUP( KKSUP + I )                                          M39.2400
      IF ( N1 .LT. 0 )  GO TO 10                                        M39.2401
      NN = IVARB(N1)                                                    M39.2402
      IF ( N2 .EQ. 60  .OR.  N2 .EQ. 61 )  GO TO 602                    M39.2403
      IF ( N2 .EQ. 63  .OR.  N2 .EQ. 67 )  GO TO 602                    M39.2404
      J = IVARB( N1 + 1 )                                               M39.2405
      K = IVARB( N1 + 2 )                                               M39.2406
      B = 0.0                                                           M39.2407
      DO 601 N= J, K                                                    M39.2408
      M = KXTCS + KSUS( KALKSU + N )                                    M39.2409
  601 B = B + XTCS(M) * KSUS( KKSUS + N )                               M39.2410
  602 N2 = N2 - 49                                                      M39.2411
      GO TO ( 10, 651,651,653, 10,655,656, 10, 10, 10,                  M39.2412
     1        660,661,662, 10,664, 10,666,667 ), N2                     M39.2413
C     ---  RELAYS AND LEVEL-TRIGGERS  ---                               M39.2414
  651 IF ( PARSUP(NN) .NE. 0.0 )  B = B * PARSUP(NN)                    M39.2415
      IF ( PARSUP(NN+2) .GT. -1. )  GO TO 10                            M39.2416
      A = B                                                             M39.2417
      GO TO 11                                                          M39.2418
C     ---  VARIABLE TRANSPORT DELAY  ---                                M39.2419
  653 IF ( PARSUP(NN+3) .EQ. 0.0 ) GO TO 10                             M39.2420
      A = B                                                             M39.2421
      GO TO 11                                                          M39.2422
C     ---  DIGITIZER  ---                                               M39.2423
  655 IF ( PARSUP(NN+1) .NE. 1.0 ) GO TO 10                             M39.2424
      M = IVARB( N1 + 3 )                                               M39.2425
      N = IVARB( N1 + 4 )                                               M39.2426
      IF ( PARSUP(NN) .NE. 0.0 )  B = B * PARSUP(NN)                    M39.2427
      NDX1 = KPRSUP +  M                                                M39.2428
      A = PARSUP(NDX1)                                                  M39.2429
      IF ( M .EQ. N )  GO TO 11                                         M39.2430
      J = M + 1                                                         M39.2431
      DO 65500 K= J, N                                                  M39.2432
      M = N - K + J                                                     M39.2433
      NDX1 = KPRSUP +  M                                                M39.2434
      IF ( B .GE. PARSUP(NDX1))  GO TO 65501                            M39.2435
65500 CONTINUE                                                          M39.2436
      GO TO 11                                                          M39.2437
65501 NDX1 = KPRSUP +  M                                                M39.2438
      A = PARSUP(NDX1)                                                  M39.2439
      GO TO 11                                                          M39.2440
C     ---  POINT-BY-POINT NON-LINEARITY  ---                            M39.2441
  656 IF ( PARSUP(NN+1) .NE. 1.0 ) GO TO 10                             M39.2442
      M = IVARB( N1 + 3 )                                               M39.2443
      N = IVARB( N1 + 4 )                                               M39.2444
      IF ( PARSUP(NN) .NE. 0.0 )  B = B * PARSUP(NN)                    M39.2445
      NDX1 = KPRSUP +  M + 1                                            M39.2446
      A = PARSUP(NDX1)                                                  M39.2447
      IF ( N .LE. M+1 )  GO TO 11                                       M39.2448
      NDX1 = KPRSUP +  M                                                M39.2449
      IF ( B .LE. PARSUP(NDX1))  GO TO 11                               M39.2450
      J = M + 2                                                         M39.2451
      DO 65600 K=J,N,2                                                  M39.2452
      NDX1 = KPRSUP +  K                                                M39.2453
      IF ( B .LE. PARSUP(NDX1))  GO TO 65601                            M39.2454
65600 CONTINUE                                                          M39.2455
      NDX1 = KPRSUP +  N                                                M39.2456
      A = PARSUP(NDX1)                                                  M39.2457
      GO TO 11                                                          M39.2458
65601 NDX1 = KPRSUP + K+1                                               M39.2459
      NDX2 = KPRSUP + K-1                                               M39.2460
      NDX3 = KPRSUP + K                                                 M39.2461
      NDX4 = KPRSUP + K-2                                               M39.2462
      D9 = ( PARSUP(NDX1)-PARSUP(NDX2) )/( PARSUP(NDX3)-PARSUP(NDX4))   M39.2463
      A = PARSUP(NDX2) + D9 *( B - PARSUP(NDX4) )                       M39.2464
      GO TO 11                                                          M39.2465
C     ---  INPUT  IF - BLOCK  ---                                       M39.2466
  660 N7 = PARSUP( NN + 1 )                                             M39.2467
      IF ( N7 .LT. 1  .OR.  N7 .GT. 3 ) GO TO 10                        M39.2468
      J = IVARB(N1+1) - N7 + 3                                          M39.2469
      NDX4 = KXTCS + KSUS( KALKSU + J )                                 M39.2470
      A = XTCS( NDX4)  *  KSUS( KKSUS + J )                             M39.2471
      GO TO 11                                                          M39.2472
C     ---  INPUT  SIGNAL  SELECTOR  ---                                 M39.2473
  661 N7 = PARSUP(NN)                                                   M39.2474
      IF ( N7 .LE. 0  .OR.  N7 .GT. 8 ) GO TO 10                        M39.2475
      GO TO ( 6611, 6611, 6611, 6611, 6611, 6616, 6617, 6618 ), N7      M39.2476
 6611 J = IVARB(N1+1) - N7 + 5                                          M39.2477
      M = KSUS( KALKSU + J )                                            M39.2478
      IF ( M .EQ. 0 )  GO TO 11                                         M39.2479
      A = XTCS( KXTCS + M ) * KSUS( KKSUS + J )                         M39.2480
      GO TO 11                                                          M39.2481
 6616 NDX3 = IVARB( N1 + 3 )                                            M39.2482
      IF ( NDX3 .EQ. 0 )  GO TO 11                                      M39.2483
      A = XTCS( KXTCS + NDX3 )                                          M39.2484
      GO TO 11                                                          M39.2485
 6617 A = PARSUP( NN + 2 )                                              M39.2486
      GO TO 11                                                          M39.2487
 6618 A = PARSUP( NN + 1 )                                              M39.2488
      GO TO 11                                                          M39.2489
C     ---  TRACK  AND  SAMPLE  ---                                      M39.2490
  662 CONTSS = PARSUP( NN + 1 )                                         M39.2491
      IF ( CONTSS  .LT.  0.5 ) GO TO 10                                 M39.2492
      A = PARSUP( NN + 2 )                                              M39.2493
      IF ( CONTSS  .GT.  1.5 )  A = B                                   M39.2494
      GO TO 11                                                          M39.2495
C     --- MIN/MAX TRACKING, CONTROLLED ACCUMULATOR OR COUHTER ---       M39.2496
  664 IF ( PARSUP(NN)  .EQ.  0.0 )  GO TO 10                            M39.2497
      IF ( PARSUP(NN)  .EQ.  99999. ) PARSUP(NN) = 0.0                  M39.2498
      A = B + PARSUP(NN) * PARSUP(NN+1)                                 M39.2499
      GO TO 11                                                          M39.2500
  666 DO 6666 JI = 1, K                                                 M39.2501
 6666 A = A + PARSUP( NN + JI )                                         M39.2502
      A = SQRTZ( A * PARSUP(NN) + B * B )                               M39.2503
      GO TO 11                                                          M39.2504
  667 A = 1.0                                                           M39.2505
   11 NDX1 = NNN + I                                                    M39.2506
      XTCS(NDX1) = A                                                    M39.2507
   10 I = INSUP(KINSUP+I)                                               M39.2508
      IF ( I .GT. 0 ) GO TO 1234                                        M39.2509
 9999 RETURN                                                            M39.2510
      END                                                               M39.2511
      SUBROUTINE CSUPAC( L, OMEGAR )                                    M39.2512
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      KJSUP = KINSUP + LSTAT(65)                                        M39.2516
      KKSUP = KJSUP  + LSTAT(65)                                        M39.2517
 1000 NNN = KXAR + NUK + LSTAT(64)                                      M39.2518
      I = L                                                             M39.2519
 1234 A = 0.0                                                           M39.2520
      AI = 0.0                                                          M39.2521
      N1 = -INSUP( KJSUP + I )                                          M39.2522
      N2 =  INSUP( KKSUP + I )                                          M39.2523
      IF ( N1 .LT. 0 )  GO TO 10                                        M39.2524
      NN = IVARB(N1)                                                    M39.2525
      IF ( N2 .EQ. 60  .OR.  N2 .EQ. 61 )  GO TO 602                    M39.2526
      IF ( N2 .EQ. 63  .OR.  N2 .EQ. 67 )  GO TO 602                    M39.2527
      J = IVARB( N1 + 1 )                                               M39.2528
      K = IVARB( N1 + 2 )                                               M39.2529
      B = 0.0                                                           M39.2530
      BI = 0.0                                                          M39.2531
      DO 601 N= J, K                                                    M39.2532
      M = KXAR + KSUS( KALKSU + N )                                     M39.2533
      MI = M + LSTAT(68)                                                M39.2534
      B = B + XAR(M) * KSUS( KKSUS + N )                                M39.2535
  601 BI = BI + XAR(MI) * KSUS( KKSUS + N )                             M39.2536
  602 N2 = N2 - 49                                                      M39.2537
      GO TO ( 10, 651,651,653, 10, 10, 10, 10, 10,659,                  M39.2538
     1        660,661,662, 10,664, 10, 10, 10 ), N2                     M39.2539
C     ---  RELAYS AND LEVEL-TRIGGERS  ---                               M39.2540
  651 IF ( PARSUP(NN+2) .GT. -1. )  GO TO 10                            M39.2541
      IF ( PARSUP(NN) .EQ. 0.0 )  GO TO 6511                            M39.2542
      B = B * PARSUP(NN)                                                M39.2543
      BI = BI * PARSUP(NN)                                              M39.2544
 6511 A = B                                                             M39.2545
      AI = BI                                                           M39.2546
      GO TO 11                                                          M39.2547
C     ---  VARIABLE TRANSPORT DELAY  ---                                M39.2548
  653 IF ( PARSUP(NN+3) .EQ. 0.0 ) GO TO 10                             M39.2549
      AA = OMEGAR * DELTAT * PARSUP(NN+2)                               M39.2550
      A  = B  * COSZ(AA) + BI * SINZ(AA)                                M39.2551
      AI = BI * COSZ(AA) - B  * SINZ(AA)                                M39.2552
      GO TO 11                                                          M39.2553
C       --- SIMPLE DERIVATIVE ---                                       M39.2554
  659 IF ( PARSUP(NN+2) .NE. 1.0 ) GO TO 10                             M39.2555
      A = - BI * OMEGAR                                                 M39.2556
      AI =  B  * OMEGAR                                                 M39.2557
      GO TO 11                                                          M39.2558
C     ---  INPUT  IF - BLOCK  ---                                       M39.2559
  660 N7 = PARSUP( NN + 1 )                                             M39.2560
      IF ( N7 .LT. 1  .OR.  N7 .GT. 3 ) GO TO 10                        M39.2561
      J = IVARB(N1+1) - N7 + 3                                          M39.2562
      NDX4 = KXAR + KSUS( KALKSU + J )                                  M39.2563
      NDX5 = NDX4 + LSTAT(68)                                           M39.2564
      A  = XAR( NDX4 )  *  KSUS( KKSUS + J )                            M39.2565
      AI = XAR( NDX5 )  *  KSUS( KKSUS + J )                            M39.2566
      GO TO 11                                                          M39.2567
C     ---  INPUT  SIGNAL  SELECTOR  ---                                 M39.2568
  661 N7 = PARSUP(NN)                                                   M39.2569
      IF ( N7 .LE. 0  .OR.  N7 .GT. 8 ) GO TO 10                        M39.2570
      GO TO ( 6611, 6611, 6611, 6611, 6611, 6616, 6617, 6618 ), N7      M39.2571
 6611 J = IVARB(N1+1) - N7 + 5                                          M39.2572
      M = KSUS( KALKSU + J )                                            M39.2573
      IF ( M .EQ. 0 )  GO TO 11                                         M39.2574
      A  = XAR( KXAR + M ) * KSUS( KKSUS + J )                          M39.2575
      AI = XAR( KXAR + M + LSTAT(68) ) * KSUS( KKSUS + J )              M39.2576
      GO TO 11                                                          M39.2577
 6616 NDX3 = IVARB( N1 + 3 )                                            M39.2578
      IF ( NDX3 .EQ. 0 )  GO TO 11                                      M39.2579
      A = XAR( KXAR + NDX3 )                                            M39.2580
      AI = XAR( KXAR + NDX3 + LSTAT(68) )                               M39.2581
      GO TO 11                                                          M39.2582
 6617 A = PARSUP( NN + 2 )                                              M39.2583
      GO TO 11                                                          M39.2584
 6618 A = PARSUP( NN + 1 )                                              M39.2585
      GO TO 11                                                          M39.2586
C     ---  TRACK  AND  SAMPLE  ---                                      M39.2587
  662 CONTSS = PARSUP( NN + 1 )                                         M39.2588
      IF ( CONTSS  .LT.  1.5 ) GO TO 10                                 M39.2589
      A = B                                                             M39.2590
      AI = BI                                                           M39.2591
   11 NDX1 = NNN + I                                                    M39.2592
      NDX2 = NDX1 + LSTAT(68)                                           M39.2593
      XAR(NDX1) = A                                                     M39.2594
      XAR(NDX2) = AI                                                    M39.2595
      GO TO 10                                                          M39.2596
C     --- MIN/MAX TRACKING, CONTROLLED ACCUMULATOR OR COUHTER ---       M39.2597
  664 IF ( PARSUP(NN)  .NE.  99999. )  GO TO 10                         M39.2598
      PARSUP(NN) = SQRTZ( B * B + BI * BI )                             M39.2599
   10 I = INSUP(KINSUP+I)                                               M39.2600
      IF ( I .GT. 0 ) GO TO 1234                                        M39.2601
 9999 RETURN                                                            M39.2602
      END                                                               M39.2603
      SUBROUTINE PREMEC                                                 M20.3325
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED ONLY BY BRANDWAJN (TYPE-59) S.M. MODEL        M31.2774
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
C     CONSTRUCT THE CONSTANT MATRICES OF COEFFICIENTS  **************** M37.4503
      IMK = 0                                                                   
      N17 = 0                                                                   
      DO 1 ILK = 1, NST                                                 M38.2661
      IF ( IPRSUP  .GE.  2 )                                            M20.3335
     1 WRITE (LUNIT6, 5289)  ILK                                        M20.3336
 5289 FORMAT ( /,  38H BEGIN NEXT GEN IN  'PREMEC' .     ILK,           M20.3337
     1 /, 30X,  I8   )                                                  M20.3338
      I30 = 30 * ILK - 29                                                       
      NUMASK = ISMDAT( I30+11 )                                         M37.4510
      NUM2 = NUMASK  +  NUMASK                                          M37.4511
      NUM4 = NUM2 + NUM2                                                M37.4512
      NUM = NUM2 + NUM4                                                         
      NUM8 = NUM4 + NUM4                                                        
      IML = IMK + 1                                                     M30. 230
      CALL MOVER0( SHP(IML), NUM2 )                                     M38.2665
      IML = IML + NUM8                                                          
      CALL MOVER0( SHP(IML), NUM4 )                                     M38.2667
      IF( IPRSUP .GT. 0 )                                               M30. 235
     1  WRITE( LUNIT6, 90 ) NUMASK, NUM2, NUM4, IMK, NST                M38.2668
 90   FORMAT(2X,16HIN PREMEC B4 708,/,2X,13I10)                         M30. 237
C     LOWER SUBDIAGONAL AND DIAGONAL   * * * * * * * * * * * * * * * * *M30. 239
      L2 = IMK                                                                  
      L4 = L2 + NUM8                                                            
      IC = L4                                                                   
      N2 = L2 + NUM2                                                            
      N21 = N2 + NUMASK                                                 M37.4519
      N22 = N21 + NUM2                                                  M37.4520
      N24 = N21 + NUM4                                                  M37.4521
      IF( NUMASK .EQ. 1 )   GO    TO  710                                       
      N4 = N22 - 1                                                      M37.4523
      DO  708 K = 2, NUMASK                                             M30. 243
      L4 = L4 + 4                                                       M37.4524
      L2 = L2 + 2                                                       M37.4525
      N3 = N4 + K                                                       M30. 245
      ZYN = -( SHP( N3 ) * DELTA2 + SHP( N3-NUMASK ) )                  M38.2672
      SHP(  L2  )  =  ZYN                                                       
      SHP( L2+1 )  = -ZYN                                                       
      SHP( L4-2 )  = -2.0 * SHP( N3 )                                           
      SHP(  L4  )  =  ZYN                                                       
      SHP( L4+1 )  =  -SHP( L4-2 )                                              
      SHP( L4+3 )  =  -ZYN                                                      
 708  CONTINUE                                                                  
C     DIAGONAL   ****************************************************** M37.4534
      N6 = NUMASK - 1                                                   M30. 254
      L2 = IMK                                                          M37.4535
      L4 = IC                                                           M37.4536
      DO 709  K = 1, N6                                                 M30. 258
      L2 = L2 + 2                                                       M37.4537
      L4 = L4 + 4                                                       M37.4538
      SHP( L2-1 ) = SHP( L2-1 ) - SHP(  L2  )                                   
      SHP( L4-3 ) = SHP( L4-3 ) - SHP( L4-2 )                                   
      SHP( L4-1 ) = SHP( L4-1 ) - SHP(  L4  )                                   
 709  CONTINUE                                                                  
C     COMPLETE THE CALCULATION OF THE DIAGONAL ELEMENTS  * * * * * * * *M30. 267
 710  L2 = IMK                                                          M37.4543
      L4 = IC                                                           M37.4544
      DO 711 K = 1, NUMASK                                              M30. 270
      N24 = N24 + 1                                                     M37.4545
      N22 = N21 + K                                                     M37.4546
      N23 = N24 - NUMASK                                                M37.4547
      L2 = L2 + 2                                                       M37.4548
      L4 = L4 + 4                                                       M37.4549
      SHP( L2-1 ) = SHP( L2-1 ) + SHP( N23 ) + SHP( N24 )  +            M38.2682
     1  SHP( N22 ) / DELTA2                                             M38.2683
      SHP( L4-1 ) = SHP( L4-1 ) + SHP( N23 ) + SHP( N24 )  -            M38.2684
     1  SHP( N22 ) / DELTA2                                             M38.2685
 711  CONTINUE                                                                  
      IF( IPRSUP  .LE.  0 )   GO   TO   712                             M30. 276
      WRITE(LUNIT6,901) ILK                                             M30. 277
 901  FORMAT(2X,35HCONSTANT MATRICES FOR GENERATOR NO.,I5)              M30. 278
      ID = IC + 1                                                       M30. 279
      IK = IC + NUM4                                                    M38.2686
      WRITE( LUNIT6, 902 )  ID, IK, ( SHP(II), II = ID, IK )            M38.2687
 902  FORMAT(2X,16HMATRIX Y(I), I =,I5,2X,2HTO,I5,/,2X,(2X,6E21.12))    M37.4556
      ID = IMK + 1                                                      M38.2688
      IK = IMK + NUM2                                                   M38.2689
      WRITE( LUNIT6, 902 )  ID, IK, ( SHP(II), II = ID, IK )            M38.2690
 712  N7  = N17 + NUMASK + NUM4                                                 
      N25 = N24 - NUMASK                                                        
      I26 = 101 * ILK - 75                                              M38.2693
      CSP = OMEGA / ELP( I26 )                                          M37.4561
      DO  713 K = 1, NUMASK                                             M30. 289
      N8 = N7 + K                                                       M30. 290
      HISTQ( N8 ) = HISTQ( N8 ) * CSP                                           
      N8 = N25 + K                                                      M38.2695
 713  SHP( N8 ) = SHP( N8 ) * 2.0 * CSP                                         
      IMK = IMK + NUM + NUM                                             M38.2697
      N17 = N17 + NUM                                                           
      IF( IPRSUP .LT. 1 )   GO  TO  1                                   M32.4240
      N7 = N17 - NUMASK + 1                                             M38.2699
      WRITE( LUNIT6,905 )  N7, N17, ( HISTQ( K ),  K = N7, N17 )        M38.2700
 905  FORMAT(2X,16HARRAY POWER FROM,I6,4H  TO,I6,/,(2X,6E20.11))        M30. 299
 1    CONTINUE                                                          M20.3506
      RETURN                                                            M20.3507
      END                                                               M20.3508
      SUBROUTINE MUL(Y,Z,XAY,L1,M,N)                                    M20.3509
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    MULTIPLICATION OF TWO MATRICES STORED AS VECTORS BY COLUMNS       M20.3510
      DIMENSION XAY(1),Z(1),Y(1)                                        M20.3511
      L=1                                                               M20.3512
 2    J=1                                                               M20.3513
      NM=-N                                                             M20.3514
   4  NM=NM+N                                                           M20.3515
      NL=NM+L                                                           M20.3516
      A=0.0                                                             M20.3517
      I=1                                                               M20.3518
      N1=-M                                                             M20.3519
      N2=NM                                                             M20.3520
    3 N1=N1+M                                                           M20.3521
      NL1=N1+L                                                          M20.3522
      N2=N2+1                                                           M20.3523
      A=A+Y(NL1)*Z(N2)                                                  M20.3524
      I=I+1                                                             M20.3525
      IF(I.LE.M) GO TO 3                                                M20.3526
      XAY(NL)=A                                                         M20.3527
      J=J+1                                                             M20.3528
      IF(J.LE.N) GO TO 4                                                M20.3529
      L=L+1                                                             M20.3530
      IF(L.LE.L1) GO TO 2                                               M20.3531
      RETURN                                                            M20.3532
      END                                                               M20.3533
      SUBROUTINE ELECyy
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED ONLY BY BRANDWAJN (TYPE-59) S.M. MODEL        M31.2776
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
C     THIS ROUTINE CALCULATES THE MATRICES OF THE ELECTRICAL PART  *****M20.3540
      IBR = IBROLD                                                      M20.3543
      IT = ITOLD                                                        M20.3544
      IMECH = 0                                                         M36.1417
      ILK = 0                                                           M38.2701
      N07 = 0                                                           M33.1071
      NWD = 24                                                                  
      LL36 = 36                                                         M37.4570
      FACT = 1.0 / ( OMEGA * DELTA2 )                                   M37.4571
      AKM = EPSUBA                                                      M32.4241
      IF( AKM .LT. 2. )   AKM = 100.0                                   M37.4572
      AKM = 1. / AKM                                                    M32.4243
      AKN = 1. / ( 1. + AKM )                                           M32.4244
      AKM = 1. - 2.0 * AKN * AKM                                        M32.4245
      FACT = FACT * AKN                                                 M32.4246
      DAMRAT = AKM                                                      M33.1073
      FACTOM = FACT * OMEGA                                             M33.1074
      OM2 = OMEGA * ONEHAF                                              M37.4573
      J30 = 1                                                                   
      DO 700 JUK = 1, NUMSM                                             M38.2703
      K1 = ISMDAT( J30 )                                                        
C     BUILD BRANCH TERMINAL INFORMATION ******************************* M39.2604
      IDELTA = ISMDAT( J30+1 )                                                  
      IF( IDELTA .EQ. 1 )   GO  TO  401                                         
      ISMDAT( J30+5 ) =  1                                                      
      ISMDAT( J30+6 ) =  1                                                      
      ISMDAT( J30+7 ) =  1                                                      
      GO TO 402                                                         M39.2610
 401  ISMDAT( J30+5 ) = ISMDAT( J30+3 )                                         
      ISMDAT( J30+6 ) = ISMDAT( J30+4 )                                         
      ISMDAT( J30+7 ) = ISMDAT( J30+2 )                                         
 402  ILK = ILK + 1                                                             
      I26 = ILK * 101 - 100                                                     
      I75 = I26 + 26                                                            
      I30 = ILK * 30 - 29                                                       
      K1  = K1 - 1                                                      M38.2708
C     FILL-IN THE A11 SUBMATRIX OF GENERATOR NO. ILK   *****************M20.3553
      CALL MOVER0( Z(1), LL36 )                                         M39.2615
      IF ( IPRSUP  .GE.  1 )                                            M20.3556
     1 WRITE( LUNIT6, 6701 )  ILK, NUMSM                                M39.2616
 6701 FORMAT( /,42H COMPANION NETWORK MODEL FOR GENERATOR NO., I5, I8 ) M39.2617
C     CHANGE  STORAGE  ARRANGEMENT   FOR  THE   INDUCTANCES, I.E. STORE*M23.2095
C     SEPARATELY LEAKAGES AND MAIN REACTANCES ************************* M37.4581
      XL = ELP( I26+18 )                                                        
      ELP( I26 ) = ELP( I26 ) - XL                                              
      ELP( I26+8 ) = ELP( I26+8 ) - XL                                          
      Z( 65 ) = ELP( I26+1 ) * FACT                                             
      Z( 66 ) = ELP( I26+3 ) * FACT                                             
      ELP( I26+4 ) = ELP( I26+4 ) * FACT                                        
      Z( 67 ) = ELP( I26+9 ) * FACT                                             
      Z( 68 ) = ELP( I26+11 ) * FACT                                            
      ELP( I26+12 ) = ELP( I26+12 ) * FACT                                      
      RAT1 = ELP( I26+20 )                                                      
      Z( 61 ) = Z( 65 ) * RAT1                                          M37.4592
      Z( 62 ) = Z( 66 ) * RAT1                                          M37.4593
      Z( 63 ) = Z( 67 ) * RAT1                                          M37.4594
      Z( 64 ) = Z( 68 ) * RAT1                                          M37.4595
      ELP( I26+2 ) = ELP( I26+2 ) * FACT - Z( 61 )                              
      ELP( I26+5 ) = ELP( I26+5 ) * FACT - Z( 62 )                              
      ELP( I26+10 ) = ELP( I26+10 ) * FACT - Z( 63 )                            
      ELP( I26+13 ) = ELP( I26+13 ) * FACT - Z( 64 )                            
      A = ELP( I26+16 ) * FACT                                                  
      ELP( I26+16 ) = A + ELP( I26+17 )                                         
      ELP( I26+17 ) = A - ELP( I26+17 ) * AKM                                   
      ACDE = 1.0                                                        M23.2116
      ACDF = 1.0                                                        M23.2117
      RAT1 = 1.0 / RAT1                                                 M37.4603
      ELP( I26+20 ) = RAT1                                              M37.4604
      B6 = ELP( I26+8 ) / ELP( I26 )                                            
      ISMDAT( I30+9 ) = 0                                                       
      ISMDAT( I30+10 ) = 0                                                      
      IF( ISMDAT( I30+8 ) .EQ. 0 )   GO  TO  404                                
      SF3 = CU( N07+1 ) * RAT1 + CU( N07+4 ) + CU( N07+5 )              M37.4611
      SF2 =( CU( N07+2 ) * RAT1 + CU( N07+6 ) + CU( N07+7 ) ) * B6      M37.4612
      SF3 = SQRTZ( SF3**2 + SF2**2)                                     M32.4254
      SF8 = ELP( I26+21 )                                                       
      IF( SF8 .GE. SF3 )   GO  TO  403                                  M39.2620
C     D - AXIS SATURATED  **********************************************M23.2126
      D7 = SF3 / SF8 - 0.9                                              M33.1078
      IS = 10.0 * D7                                                    M37.4615
      IS = ( IS + 1 ) / 2                                               M37.4616
      ISMDAT( I30+9 )  =  IS                                                    
      SB = IS                                                           M37.4618
      SB = SB * 0.1                                                     M34. 137
      SF4 = SF8 * ( 0.9 + SB )                                          M33.1079
      SF5 = SF8 * ( 1.1 + SB )                                          M37.4619
      SF9 = ELP( I26+22 )                                                       
      SF6 = SF4 / ( 1.0 + SF9 * ( SF4 - SF8 ) )                         M33.1082
      SF7 = SF5 / ( 1.0 + SF9 * ( SF5 - SF8 ) )                         M33.1083
      ACDE = ( SF7 - SF6  ) / ( SF5 - SF4 )                             M23.2133
 403  SF8 = ELP( I26+23 )                                                       
      IF( SF8 .GE. SF3 )  GO  TO  404                                   M39.2622
C     Q - AXIS SATURATED  **********************************************M23.2138
      D7 = SF3 / SF8 - 0.9                                              M33.1086
      IS = 10.0 * D7                                                    M37.4623
      IS = ( IS + 1 ) / 2                                               M37.4624
      ISMDAT( I30+10 ) = IS                                                     
      SB = IS                                                           M37.4626
      SB = SB * 0.1                                                     M34. 140
      SF4 = SF8 * ( 0.9 + SB )                                          M33.1087
      SF5 = SF8 * ( 1.1 + SB )                                          M37.4627
      SF9 = ELP( I26+24 )                                                       
      SF6 = SF4 / ( 1.0 + SF9 * ( SF4 - SF8 ) )                         M33.1090
      SF7 = SF5 / ( 1.0 + SF9 * ( SF5 - SF8 ) )                         M33.1091
      ACDF = ( SF7 - SF6  ) / ( SF5 - SF4 )                             M23.2145
 404  WRITE( LUNIT6, 6702 )  ILK, ISMDAT( I30+9 ), ISMDAT( I30+10 )     M39.2623
 6702 FORMAT( 1X, 3H***, 10X, 11HMACHINE NO., I6, 10X,                  M39.2624
     1 31HBEGIN OPERATION ON SEGMENTS NO., 2X, 2I10 )                   M39.2625
      Z( 50 ) = ( ELP( I26 ) * ACDE + XL ) * FACT                       M37.4631
      Z( 51 ) = ( ELP( I26+8 ) * ACDF + XL )  * FACT                    M37.4632
      Z( 52 ) =  ELP( I26+2 ) + Z( 61 ) * ACDE                          M38.2709
      Z( 53 ) =  ELP( I26+5 ) + Z( 62 ) * ACDE                          M38.2710
      Z( 54 ) =  ELP( I26+10 ) + Z( 63 ) * ACDF                         M37.4635
      Z( 55 ) =  ELP( I26+13 ) + Z( 64 ) * ACDF                         M37.4636
C     FILL IN  THE  THE  TRANSFORMER TERMS   OF  THE RESISTIVE HISTORY**M23.2155
C     MATRIX OF  THE  GENERATOR*****************************************M23.2156
      RA = ELP( I26+19 )                                                M37.4637
      Z( 1 ) = Z( 50 ) + RA                                             M37.4638
      Z( 8 ) = Z( 51 ) + RA                                             M37.4639
      Z( 15 ) = Z( 52 ) + ELP( I26+6 )                                  M37.4640
      Z( 22 ) = Z( 53 ) + ELP( I26+7 )                                  M37.4641
      Z( 29 ) = Z( 54 ) + ELP( I26+14 )                                 M37.4642
      Z( 36 ) = Z( 55 ) + ELP( I26+15 )                                 M37.4643
      Z( 11 ) =  Z( 67 ) * ACDF                                         M37.4644
      Z( 26 ) = Z( 11 )                                                 M23.2164
      Z( 12 ) = Z( 68 ) *  ACDF                                         M23.2165
      Z( 32 ) = Z( 12 )                                                 M37.4645
      Z( 30 ) = ELP( I26+12 ) * ACDF                                            
      Z( 35 ) = Z( 30 )                                                 M23.2168
      Z( 3 ) = Z( 65 ) * ACDE                                           M23.2169
      Z( 13 ) = Z( 3 )                                                  M23.2170
      Z( 4 ) = Z( 66 ) * ACDE                                           M23.2171
      Z( 19 ) = Z( 4 )                                                  M23.2172
      Z( 16 ) = ELP( I26+4 ) * ACDE                                             
      Z( 21 ) = Z( 16 )                                                 M23.2174
C     CHANGE REACTANCES (AT STATFR HZ ) TO  INDUCTANCES ****************M33.1092
      ELP(  I26  ) = ELP(  I26  ) / OMEGA                                       
      ELP( I26+1 ) = ELP( I26+1 ) / OMEGA                                       
      ELP( I26+3 ) = ELP( I26+3 ) / OMEGA                                       
      ELP( I26+8 ) = ELP( I26+8 ) / OMEGA                                       
      ELP( I26+9 ) = ELP( I26+9 ) / OMEGA                                       
      ELP( I26+11 ) = ELP( I26+11 ) / OMEGA                                     
      XL = XL / OMEGA                                                   M37.4654
      ELP( I26+18 ) = XL                                                M37.4655
      IF(  IPRSUP  .LT.  1  )   GO  TO  406                             M39.2626
      DO 405 K = 1, 6                                                   M39.2627
      N2 = 6 * K                                                        M38.2712
      N1 = N2 - 5                                                       M38.2713
 405  WRITE( LUNIT6, 6703 )   K,  ( Z(I), I=N1, N2 )                    M39.2628
 6703 FORMAT ( /,   11H NEW COLUMN,  I6  ,/,  ( 1X, 7E17.8 )  )         M39.2629
 406  CALL MOVER( Z(1), X1(1), LL36)                                    M39.2630
      LL7 = 6                                                           M23.2195
      LL3 = 2                                                           M23.2196
      CALL REDUCE (X1(1), LL7,LL3)                                      M20.3604
      IF ( IPRSUP  .GE.  1 )                                            M20.3605
     1 WRITE( LUNIT6, 6704 )  ILK                                       M39.2631
 6704 FORMAT( /, 23H REDUCED NETWORK MODEL., I10 )                      M39.2632
      IF( IPRSUP .LT. 1 )   GO  TO  408                                 M39.2633
      DO 407  K = 1, 6                                                  M39.2634
      N2 = 6 * K                                                        M38.2714
      N1 = N2 - 5                                                       M38.2715
 407  WRITE( LUNIT6, 6703 )   K,  ( X1(I), I=N1, N2 )                   M39.2635
C     STORE CONSTANT TERMS FOR INSERTION INTO THE TRANSIENTS PROGRAM    M20.3617
 408  A = X1( 1 )                                                       M39.2636
      B = X1( 8 )                                                       M23.2200
      SF8 = A                                                           M33.1098
      SF9 = B                                                           M33.1099
      SUM = A                                                                   
      X1( 8 ) = B - A                                                           
C     SUM=(A+B) * ONEHAF                                                M37.4664
C     A=A-SUM                                                           M37.4665
C     X1(1)=A                                                           M37.4666
C     X1( 8 ) = -A                                                      M37.4667
      ETOT = ELP( I26+16 )                                                      
      ELP( I26+16 ) = 1.0 / ETOT                                                
C     THE VALUES ''A'' AND ''B'' ARE TO BE INSERTED INTO THE TRANSIENTS M20.3625
      B = ( ETOT-SUM ) / 3.0                                            M39.2637
      A = ( ETOT+2.0*SUM ) / 3.0                                        M39.2638
      IF ( IPRSUP  .GE.  2 )                                            M20.3628
     1 WRITE( LUNIT6, 6705 )  A, B                                      M39.2639
 6705 FORMAT( /,  18H CORRECTION TERMS.   ,/,  1X,  6E20.12  )          M39.2640
C     ADD TO THE BRANCH TABLES TR,TX,C,KBUS,MBUS,LENGTH,NR * * * * * * *M20.3631
C     NOTE THAT THE INVERSE MATRIX AI HAS BEEN CALCULATED IN PARK'S     M20.3632
C     DQ0  COORDINATES.THIS SIMPLIFIES THE CALCULATIONS                 M20.3633
      IT=IT+1                                                           M20.3635
      TR(IT)=A                                                          M20.3636
      TX(IT)=0.0                                                        M20.3637
      C(IT)=0.0                                                         M20.3638
      LENGTH( IBR+1 ) = 3                                               M38.2716
      NR( IBR+1 ) = IT                                                  M38.2717
      CIK( IBR+1 ) = 0.0                                                M38.2718
      CI( IBR+1 ) = 0.0                                                 M38.2719
      CK( IBR+1 ) = 0.0                                                 M38.2720
      KBUS( IBR+1 ) = ISMDAT( J30+2 )                                           
      MBUS( IBR+1 ) = ISMDAT( J30+5 )                                           
      IT=IT+1                                                           M20.3646
      KBUS( IBR+2 ) = ISMDAT( J30+3 )                                           
      MBUS( IBR+2 ) = ISMDAT( J30+6 )                                           
      NR( IBR+2 ) = IT                                                  M38.2725
      LENGTH( IBR+2 ) = -555                                            M38.2726
      CK( IBR+2 ) = 0.0                                                 M38.2727
      CI( IBR+2 ) = 0.0                                                 M38.2728
      CIK( IBR+2 ) = 0.0                                                M38.2729
      TR(IT)=B                                                          M20.3655
      TX(IT)=0.0                                                        M20.3656
      C(IT)=0.0                                                         M20.3657
      IT=IT+1                                                           M20.3658
      TR(IT)=A                                                          M20.3659
      TX(IT)=0.0                                                        M20.3660
      C(IT)=0.0                                                         M20.3661
      IT=IT+1                                                           M20.3662
      KBUS( IBR+3 ) = ISMDAT( J30+4 )                                           
      MBUS( IBR+3 ) = ISMDAT( J30+7 )                                           
      NR( IBR+3 ) = IT                                                  M38.2732
      CIK( IBR+3 ) = 0.0                                                M38.2733
      CK( IBR+3 ) = 0.0                                                 M38.2734
      CI( IBR+3 ) = 0.0                                                 M38.2735
      LENGTH( IBR+3 ) = -2                                              M38.2736
      IBR = IBR + 3                                                             
      TR(IT)=B                                                          M20.3671
      TX(IT)=0.0                                                        M20.3672
      C(IT)=0.0                                                         M20.3673
      IT=IT+1                                                           M20.3674
      TR(IT)=B                                                          M20.3675
      TX(IT)=0.0                                                        M20.3676
      C(IT)=0.0                                                         M20.3677
      IT=IT+1                                                           M20.3678
      TR(IT)=A                                                          M20.3679
      TX(IT)=0.0                                                        M20.3680
      C(IT)=0.0                                                         M20.3681
      IF( IPRSUP .LT. 1 )  GO  TO  1408                                 M39.2641
      LL1 = IBR - 2                                                     M33.1101
      WRITE( LUNIT6, 6706 )  ( NR( I ), I = LL1, IBR )                  M39.2642
 6706 FORMAT( 8X, 5I10 )                                                M39.2643
      WRITE( LUNIT6, 6706 )  ( LENGTH( I ), I = LL1, IBR )              M39.2644
      LL1 = IT - 5                                                      M33.1104
      WRITE( LUNIT6, 6707 )  ( TR( I ), I = LL1, IT )                   M39.2645
 6707 FORMAT( 3X, 8E15.7 )                                              M39.2646
 1408 A = 1.0 / SUM                                                     M39.2647
      B = 1.0 / ETOT                                                    M38.2738
      SUM = ( B + 2.0 * A ) / 3.0                                       M38.2739
      ETOT = ( B - A ) / 3.0                                            M39.2648
      ELP(  I75  ) = SUM                                                        
      ELP( I75+1 ) = ETOT                                                       
      ELP( I75+53 ) = A * ATHTW                                                 
      ELP( I75+54 ) = B * ASQRT3                                                
      IF( IPRSUP .GT. 0 )                                               M33.1108
     1 WRITE( LUNIT6, 6707 )   ELP(I75), ELP(I75+1), ELP(I75+53),       M39.2649
     2 ELP(I75+54)                                                      M38.2745
C     CORRECTION TERMS FOR VOLTAGE AND CURRENT CALCULATIONS*************M20.3705
C     NOTE THAT ONLY THE NONZERO TERMS ARE TO BE STORED *************** M20.3706
      ELP( I75+4 ) = B6                                                         
      ELP( I75+5 ) = X1( 3 )                                                    
      ELP( I75+6 ) = X1( 4 )                                                    
      ELP( I75+7 ) = X1( 11 )                                                   
      ELP( I75+8 ) = X1( 12 )                                                   
      ELP( I75+9 ) = X1( 15 )                                                   
      ELP( I75+10 ) = X1( 21 )                                                  
      ELP( I75+11 ) = X1( 16 )                                                  
      ELP( I75+12 ) = X1( 22 )                                                  
      ELP( I75+13 ) = X1( 29 )                                                  
      ELP( I75+14 ) = X1( 35 )                                                  
      ELP( I75+15 ) = X1( 30 )                                                  
      ELP( I75+16 ) = X1( 36 )                                                  
      ELP( I75+43 ) = X1( 13 )                                                  
      ELP( I75+44 ) = X1( 19 )                                                  
      ELP( I75+45 ) = X1( 26 )                                                  
      ELP( I75+46 ) = X1( 32 )                                                  
      ELP( I75+47 ) = 1.0 / SF8                                         M38.2763
      ELP( I75+48 ) = XL  / SF8                                         M38.2764
      ELP( I75+49 ) = SF9                                               M38.2765
      ELP( I75+50 ) = 0.0                                                       
      ELP( I75+51 ) = 0.0                                                       
      ELP( I75+52 ) = X1( 8 )                                                   
      IF( IPRSUP .LT. 2 )  GO  TO  409                                  M39.2650
      WRITE( LUNIT6, 6708 )  ILK                                        M39.2651
 6708 FORMAT( 5X,20H ARRAYS FOR MACH NO.,I5,17H  IN ORDER OF AC ,       M39.2652
     1 1X,4HA21 ,1X,4HA22 ,1X )                                         M38.2769
      WRITE( LUNIT6, 6707 )  ELP(I75+2), ELP(I75+3), ELP(I75+51),       M39.2653
     1 ELP(I75+52)                                                      M38.2771
      IDT = I75 + 5                                                     M38.2773
      IDS = IDT + 3                                                     M38.2774
      WRITE( LUNIT6, 6707 )    ( ELP( IU ),  IU = IDT, IDS )            M39.2654
      IDS = IDS + 1                                                     M38.2776
      IDT = IDS + 7                                                     M38.2777
      WRITE( LUNIT6, 6707 )    ( ELP( IU ),  IU = IDS, IDT )            M39.2655
C     CALCULATE THE RESISTIVE HISTORY MATRIX ************************** M38.2779
 409  ELP( I75+17 ) = Z( 50 ) - RA * AKM                                        
      ELP( I75+18 ) = Z( 13 )                                                   
      ELP( I75+19 ) = Z( 19 )                                                   
      ELP( I75+20 ) = Z( 51 ) - RA * AKM                                        
      ELP( I75+21 ) = Z( 26 )                                                   
      ELP( I75+22 ) = Z( 32 )                                                   
      ELP( I75+23 ) = Z( 3  )                                                   
      ELP( I75+24 ) = Z( 52 ) - ELP( I26+6 ) * AKM                              
      ELP( I75+25 ) = Z( 21 )                                                   
      ELP( I75+26 ) = Z( 4  )                                                   
      ELP( I75+27 ) = Z( 16 )                                                   
      ELP( I75+28 ) = Z( 53 ) - ELP( I26+7 ) * AKM                              
      ELP( I75+29 ) = Z( 11 )                                                   
      ELP( I75+30 ) = Z( 54 ) - ELP(I26+14)  * AKM                              
      ELP( I75+31 ) = Z( 35 )                                                   
      ELP( I75+32 ) = Z( 12 )                                                   
      ELP( I75+33 ) = Z( 30 )                                                   
      ELP( I75+34 ) = Z( 55 ) - ELP(I26+15)  * AKM                              
C     STORE ADDITIONAL ELEMENTS FOR FUTURE CALCULTIONS OF INCREMENTAL***M23.2295
C     INDUCTANCES AND UPDATING OF THE MATRIX /YS/ **********************M23.2296
      DO 410  II = 35, 42                                               M39.2657
 410  ELP( I75+II ) = Z( II+26 )                                        M39.2658
C     STORE FLUX LINKAGES FOR SYNCHRONOUS FRAME*************************M36.1418
      A = ELP( I26 ) * CU( N07+1 ) + ELP( I26+1 ) * CU( N07+4 ) +       M38.2800
     1 ELP( I26+3 ) * CU( N07+5 )                                       M38.2801
      AFD = A * ELP( I75+2 ) + XL * CU( N07+1 )                                 
      A = ELP( I26+8 ) * CU( N07+2 ) + ELP( I26+9 ) * CU( N07+6 ) +     M38.2803
     1 ELP( I26+11 ) * CU( N07+7 )                                      M38.2804
      AFQ = A * ELP( I75+3 ) + XL * CU( N07+2 )                                 
      CU( N07+15 ) = AFD                                                        
      CU( N07+16 ) = AFD                                                        
      CU( N07+17 ) = AFQ                                                        
      CU( N07+18 ) = AFQ                                                        
C     STORE MECHANICAL VARIABLES FOR THE ITERATION LOOP *************** M38.2810
      IDT = IMECH + ISMDAT( I30+12 )                                    M37.4717
      CU( N07+19 ) = HISTQ( IDT ) * ELP( I26+25 )                               
      CU( N07+20 ) = COSZ( CU( N07+19 ) )                               M38.2812
      CU( N07+21 ) = SINZ( CU( N07+19 ) )                               M38.2813
      IMECH = IMECH + 6 * ISMDAT( I30+11 )                                      
C     STORE ROTOR POSITION 1 AND 2 TIME-STEPS BACK IN TIME**************M37.4720
      CU( N07+22 ) = CU( N07+19 ) - OMDT                                        
      CU( N07+23 ) = CU( N07+22 ) - OMDT                                        
      CU( N07+24 ) = OMEGA                                                      
      N07 = N07 + NWD                                                           
      IF( IPRSUP  .LT.  1  )   GO   TO   412                            M39.2659
      WRITE( LUNIT6, 6709 )  ILK                                        M39.2660
 6709 FORMAT ( /,   33H HISTORY MATRIX FOR GENERATOR NO.,  I5  )        M39.2661
      IN = I75 + 9                                                      M38.2819
      DO 411 K = 1, 5                                                   M39.2662
      IN = IN + 7                                                       M20.3789
      N2 = IN + 7                                                       M20.3790
      N1 = IN + 1                                                       M20.3791
 411  WRITE( LUNIT6, 6710 )   IN, ( ELP( I ),  I = N1, N2 )             M39.2663
 6710 FORMAT( 1X, 12HNEW  ROW    , I6, /, ( 1X, 7E17.9 ) )              M39.2664
 412  IF( K1 .GT. 0 )    GO  TO  402                                    M39.2665
 700  J30 = J30 + 30                                                            
      IF( IPRSUP .LT. 2 )    GO TO 2500                                 M38.2825
      IBK = 0                                                           M38.2826
      DO 2400 IK = 1, ILK                                               M38.2827
      IBL = IBK + 1                                                     M38.2828
      IBK = IBK + 101                                                   M38.2829
      WRITE( LUNIT6, 6711 )    ( ELP( I ), I = IBL, IBK )               M39.2666
 6711 FORMAT( /, ( 1X, 6E20.12 ) )                                      M39.2667
 2400 CONTINUE                                                          M38.2832
 2500 RETURN                                                            M38.2833
      END                                                               M20.3796
      SUBROUTINE REDUCE(X1,M,N)                                         M20.3797
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION X1(1),A1(20)                                            M20.3798
C     GAUSS-JORDAN ELIMINATION PROCESS PERFORMED ON A SQUARE MATRIX X   M20.3799
C)    THIS ROUTINE CAN ALSO BE USED FOR MATRIX INVERSION * * * * * * * *M20.3800
C     MAXIMUM ORDER OF MATRIX X IS 20                                   M20.3801
      J=M                                                               M20.3802
      IK=M**2                                                           M20.3803
      NK=IK-M                                                           M20.3804
 1    C=X1(IK)                                                          M20.3805
      C=1.0/C                                                           M20.3806
      CALL MOVER ( X1(NK+1), A1(1), M )                                 M22.4366
      K=1                                                               M20.3809
    4 MK=(K-1)*M                                                        M20.3810
      N9 = MK + J                                                       M22.4367
      B = -X1(N9) * C                                                   M22.4368
      I=1                                                               M20.3812
 3    MK=MK+1                                                           M20.3813
      X1(MK)=X1(MK)+B*A1(I)                                             M20.3814
      I=I+1                                                             M20.3815
      IF(I.LE.M) GO TO 3                                                M20.3816
      N9 = MK + J - M                                                   M22.4369
      X1(N9) = B                                                        M22.4370
      K=K+1                                                             M20.3818
      IF(K.EQ.J) K=K+1                                                  M20.3819
      IF(K.LE.M) GO TO 4                                                M20.3820
      DO 5 K=1,M                                                        M20.3821
      N9 = NK + K                                                       M22.4371
    5 X1(N9) = A1(K) * C                                                M22.4372
      X1(IK)=C                                                          M20.3823
      J=J-1                                                             M20.3824
      IK=IK-M-1                                                         M20.3825
      NK=NK-M                                                           M20.3826
      IF(J.GT.N) GO TO 1                                                M20.3827
      RETURN                                                            M20.3828
      END                                                               M20.3829
