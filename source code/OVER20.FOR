      SUBROUTINE OVER20
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR/NOLIST'                                           
      INCLUDE  'LABCOM.FOR/NOLIST'                                           
      EQUIVALENCE  (MONCAR(1),    KNT),   (MONCAR(2),  KBASE)           M37.6131
      EQUIVALENCE  (MONCAR(3), LTDELT),   (MONCAR(4),    ISW)           M37.6132
      EQUIVALENCE                         (MONCAR(10), MTAPE)           M37.6133
      IF ( IPRSUP  .GE.  1 )                                            M32.6485
     1 WRITE ( LUNIT6, 4567 )                                           M32.6486
 4567 FORMAT ( 23H BEGIN MODULE "OVER20".  )                            M35.1656
      ZERO = 0.0                                                        M27.2619
      LL1 = 1                                                           M29.2664
      LL9  =  9                                                         M23.5294
      LL10 = 10                                                         M23.5295
      CALL RUNTYM(D1,D2)                                                M20.4822
      N10 = 1                                                           M22.5380
      IF ( IPRSUP  .GE.  1 )                                            M21. 367
     1 WRITE (LUNIT6, 1324)    IPLOT, NENERG, KBASE,                    M35.1657
     2              M4PLOT,  MTAPE, ICAT, LSTAT(32), T, AINCR           M35.1658
 1324 FORMAT ( /,  1X,   23H   IPLOT  NENERG  KBASE,                    M37.6134
     1                   32H  M4PLOT   MTAPE    ICAT LSTAT32,           M35.1660
     2         14X, 1HT, 10X, 5HAINCR  ,/,  1X, 7I8, 2E15.5  )          M35.1661
      IF ( MAX99M .LT. 0 )                                              M43.2888
     1 write (lunit6, 9345)  max99m                                     M43.2889
 9345 format (  ' ++++  NUMBER OF SUPPRESSED TYPE-99 FLASHOVER',        M43.2890
     1  ' OR CLEARING MESSAGES IS NEGATIVE OF',  I7  )                  M43.2891
      IF ( PEAKND(1)  .EQ.  0.0 )   GO TO 5019                          M35.1662
      N6 = PEAKND(3)                                                    M25.1206
      WRITE (LUNIT6, 5011)  PEAKND(1), PEAKND(2), BUS(N6)               M25.1207
 5011 FORMAT ( 8X,  38HOVERALL SIMULATION PEAK NODE VOLTAGE =,          M25.1208
     1  E15.6,      17H .   TIME (SEC) =,                               M25.1209
     2  E14.5,      12H .   BUS = ',   A6,   3H' .    )                 M25.1210
 5019 K = LSTAT(32) + 1                                                 M25.1211
      IF( IPLOT .LT. 0 )  GO TO 8005                                       13979
      VOLTI(1) = -9999.                                                 M29.2665
      IF ( M4PLOT  .EQ.  0 )   GO TO 5022                               M34. 165
      CALL PLTFIL ( K )                                                 M32.6491
      GO TO 8005                                                        M29.2668
 5022 WRITE (LUNIT4)  ( VOLTI(I), I=1, K )                              M29.2669
 8005 IF ( ICAT  .GT.  0     .OR.      MEMSAV  .GT.  0 )                M28.6577
     1 CALL KATALG                                                      M22.5384
      CLOSE (UNIT=79)                                                   M43.  15
      K = LSTAT(32)                                                     M28.6578
C      BOTH MAX AND MIN WILL BE PRINTED FOR EACH ENERGIZATION WHEN      M27.2620
C      'AINCR' IS SET TO BE GREATER THAN OR EQUAL TO '55.0'.            M27.2621
      IF ( KBASE .EQ. 2  .AND. AINCR .LT. 55.0) GO TO 3614              M27.2622
      IF ( BEGMAX(1)  .LE.  0.0 )   GO TO 3614                          M31.3606
      WRITE (LUNIT6, 8002)   ( XMAX(L), L=1, K )                        M27.2623
 8002 FORMAT (/, 106H MAXIMA AND MINIMA WHICH OCCURRED DURING THE SIMULAM27.2624
     1TION FOLLOW.   THE ORDER AND COLUMN POSITIONING ARE THE     ,/,   M27.2625
     2  49H SAME AS FOR THE REGULAR PRINTED OUTPUT VS. TIME.      ,/,   M27.2626
     3  18H VARIABLE MAXIMA :   ,/,  ( 15X, 9E13.6 )  )                 M27.2627
C     EXTREMA VECTOR  "XMAX"  ACTUALLY HAS FOUR PARTITIONS,             M27.2628
C     EACH THE SIZE OF LIST 12:  (XMAX, TMAX, XMIN, TMIN) .             M27.2629
      NDX1 = LSIZ12 + 1                                                 M38.3545
      NDX2 = LSIZ12 + K                                                 M38.3546
      WRITE (LUNIT6, 8003)  ( XMAX(L), L=NDX1, NDX2 )                   M27.2632
 8003 FORMAT ( 18H TIMES OF MAXIMA :  ,/,  ( 15X, 9E13.6 )  )           M27.2633
      NDX1 = NDX1 + LSIZ12                                              M38.3547
      NDX2 = NDX2 + LSIZ12                                              M38.3548
      WRITE (LUNIT6, 8004)   ( XMAX(L), L=NDX1, NDX2 )                  M27.2636
 8004 FORMAT ( 18H VARIABLE MINIMA :   ,/,  ( 15X, 9E13.6 )  )          M27.2637
      NDX1 = NDX1 + LSIZ12                                              M38.3549
      NDX2 = NDX2 + LSIZ12                                              M38.3550
      WRITE (LUNIT6, 18005)  ( XMAX(L), L=NDX1, NDX2 )                  M27.2640
18005 FORMAT ( 18H TIMES OF MINIMA :  ,/,  ( 15X, 9E13.6 )  )           M27.2641
 3614 IF (NENERG .NE. 0)   GO TO 605                                    M27.2642
      FLSTAT(9) = D1                                                    M20.4851
      FLSTAT(10) = D2                                                   M20.4852
      IF ( BEGMAX(1)  .NE.  0.0   .AND.                                 M37.6135
     1     BEGMAX(2)  .NE.  0.0 )                                       M37.6136
     1 WRITE (LUNIT6, 8006)  BEGMAX(2)                                  M31.3609
 8006 FORMAT (  56X,  32HSEARCH FOR EXTREMA BEGAN AT TIME,              M37.6137
     1  E15.6,   7H   SEC.    )                                         M23.5302
      WRITE (LUNIT6, 8007)                                              M37.6138
 8007 FORMAT ( 1X )                                                     M27.2644
      IF ( IPUNCH  .LE.  0 )   GO TO 9800                               M22.5385
 5759 KCOUNT = 2                                                           14002
      IOLD=3                                                               14003
      IF ( N10  .NE.  0 )                                               M22.5386
     1 WRITE (LUNIT6, 7009)  T                                          M20.4855
 7009 FORMAT ( /,  74H PRINTOUT OF THE SAVING OF TERMINAL CONDITIONS FORM22.5387
     1 ALL COMPONENTS, AT TIME,    E15.7,     11H   SECONDS.       ,/,  M22.5388
     2  30H BEGIN WITH ALL NODE VOLTAGES.                          ,/,  M22.5389
     3  3( 21X,  4HNODE )    ,/,  19X,  6HNUMBER,  21X,  4HNAME,        M23.5307
     4  18X,  7HVOLTAGE      )                                          M23.5308
      DO 7010 K=2,NTOT                                                     14006
      IF ( N10  .NE.  0 )                                               M22.5392
     1 WRITE(LUNIT6,7012) BUS(K), E(K), ZERO, K                         M23.5309
 7012 FORMAT( 1H , A6, 2E13.5, 41X, I6 )                                M23.5310
 7010 WRITE(LUNIT7,7011) KCOUNT, BUS(K), E(K), ZERO, ZERO, K            M23.5311
 7011 FORMAT( I2, A6, 3E15.8, 21X, I6 )                                 M23.5312
      IF ( N10  .NE.  0 )                                               M22.5393
     1 WRITE (LUNIT6, 7013)                                             M20.4859
 7013 FORMAT ( /,  59H LINEAR BRANCH TABLE STATE VARIABLES ( 'CURRENTS' M23.5313
     1) FOLLOW.  ,/,                                                    M23.5314
     2  7X,  3HROW,  9X,  11H'FROM' NODE,  11X,  9H'TO' NODE,  9X,      M23.5315
     3  66HLINEAR BRANCH PARAMETERS, IDENTIFIED BY THE FORTRAN VECTOR STM23.5316
     4ORAGE  ,/,  9X,  1HI,  8X,  12HBUS(KBUS(I)),  8X,                 M23.5317
     5  12HBUS(MBUS(I)),  19X,  6HCIK(I),  20X,  5HCI(I),               M23.5318
     6  20X,  5HCK(I)    )                                              M23.5319
      K=1                                                                  14013
 7014 IT2=1                                                                14014
      IF(KBUS(K).LT.0) GO TO 7024                                          14015
      IF(NR(K).GE.0) GO TO 7022                                            14016
      CI1=CI(K)                                                            14017
      CK1=CK(K)                                                            14018
      IF(CI1.EQ.0. .AND. CK1.EQ.0.) GO TO 7025                             14019
      L=KBUS(K)                                                            14020
      M=IABS(MBUS(K))                                                      14021
      WRITE(LUNIT7,7020) IOLD, BUS(L), BUS(M), CI1, CK1, ZERO, ZERO, K  M23.5320
 7020 FORMAT( I2, 2A6, 4E15.8, I6)                                      M23.5321
      IF( N10 .NE. 0 )                                                  M23.5322
     1 WRITE(LUNIT6,7021) BUS(L), BUS(M), CI1, CK1, ZERO, ZERO, K       M23.5323
 7021 FORMAT( 1H , A6, 1X, A6, 4E13.5, I6 )                             M23.5324
      GO TO 7025                                                           14026
 7022 IT2=IABS(LENGTH(K))                                                  14027
      IT1=K+IT2-1                                                          14028
      DO 7023 I=K,IT1                                                      14029
      N1=KBUS(I)                                                           14030
      N2=IABS(MBUS(I))                                                     14031
      IF ( N10  .NE.  0 )                                               M22.5397
     1 WRITE(LUNIT6,7021) BUS(N1),BUS(N2), CIK(I), CI(I), CK(I), ZERO,I M23.5325
 7023 WRITE(LUNIT7,7020) IOLD, BUS(N1), BUS(N2), CIK(I), CI(I), CK(I),  M23.5326
     1 ZERO, I                                                          M23.5327
      GO TO 7025                                                           14034
 7024 N1 = IABSZ( KBUS(K) )                                             M32.6495
      N2 = IABSZ( MBUS(K) )                                             M32.6496
      WRITE( LUNIT6, 70241 ) BUS(N1), BUS(N2)                           M32.6497
70241 FORMAT ( 42H  ****  WARNING.   ****   NO CURRENTS WILL,           M32.6498
     1         36H BE PUNCHED FOR THE DISTRIBUTED LINE,                 M32.6499
     2         20H CONNECTING NODES  ",  A6,  9H"  AND  ",              M32.6500
     3   A6,  3H" .   )                                                 M32.6501
      IT2 = LENGTH(K)                                                   M32.6502
      IF( IT2 .LT. 0 ) IT2 = -IT2                                       M32.6503
      IF( LENGTH(K) .GT. 0 ) IT2 = 1                                    M32.6504
      IF( KODSEM( K ) .NE. 0  .AND. IMODEL( K ) .NE. 2 )                M39.3148
     1 IT2 = IABSZ( KODEBR(K) )                                         M39.3149
 7025 K=K+IT2                                                              14039
      IF(K.LE.IBR) GO TO 7014                                              14040
      IF (INONL .EQ. 0) GO TO 9207                                      M20.4864
      IF ( N10  .NE.  0 )                                               M23.5328
     1 WRITE (LUNIT6, 4688)                                             M23.5329
 4688 FORMAT ( /, 48H NONLINEAR ELEMENT TABLE STATE VARIABLES FOLLOW. ) M23.5330
      IOLD=4                                                               14042
      DO 7035 K=1,INONL                                                    14043
      IF(NONLE(K).GE.0) GO TO 7035                                         14044
      N1=NONLK(K)                                                          14045
      N2=IABS(NONLM(K))                                                    14046
      IF ( NLTYPE(K)  .NE.  -96 )  GO TO 7031                           M23.5331
      N5 = NONLAD(K)                                                    M23.5332
      N6 = N5 + 1                                                       M23.5333
      N7 = N5 + 2                                                       M23.5334
      N8 = N5 + 3                                                       M23.5335
      WRITE(LUNIT7, 7020) IOLD, BUS(N1), BUS(N2), VCHAR(N7), CCHAR(N8)  M23.5336
      N9 = CCHAR(N6)                                                    M23.5337
      N11 = CCHAR(N7)                                                   M23.5338
      WRITE (LUNIT7, 7018)  N9, N11,  ( VCHAR(IPP), IPP=N5, N8 )        M23.5339
 7018 FORMAT ( 2I10, 4E15.8)                                            M23.5340
      N12 = N5 + 4                                                      M25.1212
      N13 = N5 + 5                                                      M25.1213
      N14 = CCHAR(N12)                                                  M25.1214
      WRITE(LUNIT7,7026) N14, VCHAR(N12), VCHAR(N13),                   M25.1215
     1                   GSLOPE(N12), GSLOPE(N13)                       M25.1216
 7026 FORMAT ( I10 , 4E15.8 )                                           M25.1217
      IF ( N10  .EQ.  0)  GO TO 7035                                    M23.5341
      WRITE ( LUNIT6, 7021) K, BUS(N1), BUS(N2), VCHAR(N7), CCHAR(N8)   M23.5342
      WRITE ( LUNIT6, 7019)  N9, N11,  ( VCHAR(IPP), IPP=N5, N8 )       M23.5343
 7019 FORMAT ( 1H ,I10, 2X, I10, 4( 3X, E15.8) )                        M23.5344
      WRITE(LUNIT6,7027)  N14, VCHAR(N13), VCHAR(N14),                  M25.1218
     1                    GSLOPE(N12), GSLOPE(N13)                      M25.1219
 7027 FORMAT ( 1H , I10 , 4(3X,E15.8) )                                 M25.1220
      GO TO 7035                                                        M23.5345
 7031 A = CURR(K)                                                       M23.5346
      IF(A.EQ.0.) GO TO 7035                                               14048
      WRITE(LUNIT7,7020) IOLD,BUS(N1),BUS(N2),A                         M17. 947
      IF ( N10  .NE.  0 )                                               M22.5399
     1 WRITE (LUNIT6, 7021)  BUS(N1), BUS(N2), A                        M20.4866
 7035 CONTINUE                                                             14051
 9207 IF (KSWTCH .EQ. 0) GO TO 4020                                     M20.4870
      IF ( N10  .NE.  0 )                                               M22.5400
     1 WRITE (LUNIT6, 7348)                                             M22.5401
 7348 FORMAT ( /,  38H STATUS VARIABLES FOR SWITCHES FOLLOW.       ,/,  M22.5402
     1 1X,  6HBUS(L),  1X,  6HBUS(M),  3X,  7HKPOS(K),  2X,             M22.5403
     2 8H KODE(L),  2X,  8H KODE(M),  14X,  6HTCLOSE,  14X,             M32.6506
     3 6HADELAY,  14X,  6HENERGY,  16X,  4HCRIT   )                     M22.5405
      DO 4010 K=1, KSWTCH                                               M20.4871
      L = IABS(KMSWIT(K))                                               M32.6507
      KN1 = LSWTCH + K                                                  M32.6508
      M = IABS(KMSWIT(KN1))                                             M32.6509
      NPL = NEXTSW(K)                                                   M35.1663
      IF ( NPL  .NE.  0 )  NPL = 87                                     M35.1664
      WRITE (LUNIT7,4003) BUS(L), BUS(M), KPOS(K), KODE(L), KODE(M),    M32.6510
     1   TCLOSE(K), ADELAY(K), ENERGY(K), CRIT(K), NPL                  M35.1665
 4003 FORMAT(2H 5,2A6,3I4,4E13.6 ,/, 14X, I4)                           M35.1666
      IF ( N10  .EQ.  0 )   GO TO 4010                                  M22.5406
      WRITE (LUNIT6,4004) BUS(L), BUS(M), KPOS(K), KODE(L), KODE(M),    M32.6513
     1     TCLOSE(K), ADELAY(K), ENERGY(K), CRIT(K), NEXTSW(K)          M32.6514
 4004 FORMAT(1X,A6,1X,A6,3I10,4E20.10 ,/, 14X, I10)                     M32.6515
 4010 CONTINUE                                                          M20.4881
 4020 GO TO 9800                                                        M35.1667
C     CODE BELOW IS JUST FOR  'STATISTICS'  OR  'SYSTEMATIC'  RUNS. ----M22.5409
  605 NSTAT = K                                                            14070
      DO 8010  L=1, NSTAT                                               M27.2645
      NDX1 = 2 * LSIZ12  +  L                                           M38.3551
      IF ( -XMAX(NDX1)  .LE.  XMAX(L) )   GO TO 8010                    M28.6581
      XMAX(L) = XMAX(NDX1)                                              M28.6582
      NDX3 = LSIZ12 + L                                                 M38.3552
      NDX2 = NDX1 + LSIZ12                                              M38.3553
      XMAX(NDX3) = XMAX(NDX2)                                           M28.6585
 8010 CONTINUE                                                          M27.2648
      IF (IPRSUP .GT. 0)                                                M20.4887
     1WRITE (LUNIT6,8903) ISW, NSTAT, KNT, NENERG, (XMAX(I), I=1, NSTAT)M20.4888
 8903 FORMAT(27H ISW, NSTAT, KNT, NENERG = ,4I10,/,                     M20.4889
     1   25H (XMAX(I), I=1, NSTAT) =   /  6(5X,E15.8))                  M20.4890
      KNT = KNT + 1                                                     M13.2797
      WRITE (LUNIT9)  ( XMAX(L), L=1, NSTAT )                           M23.5350
      IF ( BEGMAX(1)  .EQ.  0.0 )   GO TO 627                           M31.3611
      IF(NSTAT.LE.9) GO TO 609                                             14078
      IF (NENERG .LT. 0)   GO TO 612                                    M13.2800
      IF ( NSWTPE .LE. 1 )  GO TO 1610
      WRITE (LUNIT6, 1510) ANGLE, ANGTPE, ( XMAX(L),L=1,LL9)
 1510 FORMAT ( 1X, F6.2, 1X, F6.2, 1X, 9E13.6 )
      GO TO 611
 1610 WRITE (LUNIT6, 617)  ANGLE,  ( XMAX(L), L=1, LL9 )                M23.5351
  611 WRITE (LUNIT6, 618)  ( XMAX(L), L=LL10, NSTAT )                   M23.5352
      GO TO 624                                                            14081
  612 WRITE (LUNIT6, 618)  ( XMAX(L), L=1, LL9 )                        M23.5353
      GO TO 611                                                         M13.2803
  609 IF (NENERG .LT. 0)   GO TO 614                                    M13.2804
      IF ( NSWTPE .LE. 1 )  GO TO 1620
      WRITE (LUNIT6, 1510) ANGLE, ANGTPE, (XMAX(L), L = 1, NSTAT)
      GO TO 624
 1620 WRITE (LUNIT6, 617)  ANGLE, (XMAX(L), L = 1, NSTAT)               M17. 955
      GO TO 624                                                         M13.2806
  614 WRITE (LUNIT6, 618)   (XMAX(L), L = 1, NSTAT)                     M17. 956
  617 FORMAT ( 1X, F9.4, 5X, 9E13.6 )                                   M11. 309
  618 FORMAT ( 15X, 9E13.6 )                                            M11. 310
  624 NDX1 = LSIZ12 + 1                                                 M38.3554
      NDX2 = LSIZ12 + NSTAT                                             M38.3555
      IF ( BEGMAX(1)  .GT.  0.0 )                                       M31.3612
     1 WRITE (LUNIT6, 8003)  ( XMAX(L), L=NDX1, NDX2 )                  M27.2652
  627 IF ( KNT  .GT.  IABS(NENERG) )   GO TO 610                        M19.2019
      LASTOV = NCHAIN                                                   M22.5410
      NCHAIN = 12                                                       M22.5411
      IF ( IPRSUP  .GE.  1 )                                            M32.6520
     1 WRITE ( LUNIT6, 4568 )                                           M32.6521
 4568 FORMAT ( 15H EXIT "OVER20".   )                                   M35.1668
      GO TO 99999                                                          14086
  610 WRITE (LUNIT6, 654 )                                              M17. 957
  654 FORMAT ( //,1X)                                                   M15.1330
      LUNIT5 = MTAPE                                                            
C     RESET NUMDCD COUNTER IF IT IS NOT A OMIT BASE CASE                M36.1668
      IF ( LSTAT(15) .NE. INTINF )    NUMDCD = NUMDCD - IPNTV(11)       M36.1669
      WRITE (LUNIT6, 659 )                                              M17. 958
  659 FORMAT ( 132H ----------------------------------------------------M02.   6
     1------------------------------------------------------------------M02.   7
     2-------------   )                                                 M02.   8
      WRITE (LUNIT6, 616)  NENERG                                       M17. 959
  616 FORMAT (  30H SIMULATION OF ALL  'NENERG' =,  I4, 75H  ENERGIZATIOM01.  99
     1NS IS NOW COMPLETE, AND THE EMTP IS READY TO BEGIN STATISTICAL ,/,M28.6586
     2 108H PROCESSING OF THE VOLTAGE SOLUTIONS.   BUT BEFORE DOING SO, M01. 101
     3TWO CAUTIONS ABOUT DISCRETIZATION OF CONTINUOUS    ,/,            M01. 102
     4  61H VARIABLES ARE PROBABLY APPROPRIATE TO MENTION AT THIS POINT.M01. 103
     5  ,/, 5X,  109H1.  THE SWITCH CLOSING TIMES WHICH ARE PRINTED ALONM01. 104
     6G WITH PEAK VOLTAGES OF EACH ENERGIZATION ARE ONLY DESIRED   ,/,  M01. 105
     7 9X, 113HTIMES, JUST AS WITH ALL SUCH TIMES WHICH ARE PUNCHED ON DM01. 106
     8ATA CARDS WHICH DEFINE SWITCHES UPON DATA INPUT.   SINCE   )      M01. 107
      WRITE (LUNIT6, 644)                                               M17. 960
  644 FORMAT(9X,112HTIME IS DISCRETIZED IN MULTIPLES OF TIME-STEP-SIZE  M01. 109
     1'DELTAT' ,   ACTUAL CLOSURE WILL OCCUR AT THE FIRST DISCRETE   ,/,M01. 110
     2 9X,  47HSTEP WHICH DOES NOT PRECEDE THE REQUESTED TIME.    ,/,   M01. 111
     3 5X, 112H2.  IN THE TABULATION OF VOLTAGE DISTRIBUTIONS WHICH FOLLM01. 112
     4OW, AN ARBITRARY DECISION HAD TO BE MADE CONCERNING THE   ,/,     M01. 113
     5 9X, 108HCOLUMN WHICH IS LABELED  'FREQUENCY' .    THE CONTINUOUS M01. 114
     6VARIABLE VOLTAGE HAS BEEN DISCRETIZED, DIVIDED INTO   ,/,         M01. 115
     7 9X, 114HCOMPARTMENTS OF SIZE  'AINCR'  (READ FROM COLUMNS 25-32 OM01. 116
     8F THE SPECIAL STATISTICS MISC. DATA CARD).   FOR AN ENTRY   )     M01. 117
      WRITE (LUNIT6, 648)                                               M17. 961
  648 FORMAT ( 9X, 105HWHICH IS PRINTED IN A ROW MARKED AS BEING FOR VOLM01. 119
     1TAGE  'VROW' ,   THE ASSOCIATED COMPARTMENT CONTAINS ALL  ,/,     M01. 120
     2 9X,  73HVOLTAGES  'V'  WHICH SATISFY       VROW  .LE.  V  .LT.  (M01. 121
     3VROW + AINCR)  .    ,/,                                           M28.6587
     4  5X, 44H3.  FLOATING-POINT COUNTING (T = T + DELTAT),            M28.6588
     5      45H IS USED TO INCREMENT TIME.   SWITCHING TIMES,           M28.6589
     6      28H WHICH ARE AN EXACT MULTIPLE    ,/,  9X,                 M28.6590
     7      47HOF THE TIME-STEP THUS ARE AMBIGUOUS;  DIFFERENT,         M28.6591
     8      43H COMPUTERS MAY SWITCH ONE STEP LATER, THEN.   )          M28.6592
      IF ( LTDELT  .LE.  0 )   GO TO 7314                               M01. 123
      WRITE (LUNIT6, 7305)                                              M17. 962
 7305 FORMAT ( //,1X)                                                   M15.1331
      WRITE (LUNIT6, 7304)                                              M17. 963
 7304 FORMAT ( 60H *****************************************************M01. 127
     1******  , 12H LOOK, LOOK ,                                        M01. 128
     2 60H************************************************************ )M01. 129
      WRITE (LUNIT6, 7309)  LTDELT                                      M17. 964
 7309 FORMAT ( /,  28H DURING THIS RUN, A TOTAL OF, I4,  73H  RANDOM SWIM01. 131
     1TCH CLOSINGS LESS THAN TIME ZERO WERE GENERATED BY THE RANDOM  ,/,M01. 132
     2 118H NUMBER GENERATOR.   BUT THE EMTP HAS NO WAY OF HANDLING SUCHM28.6593
     3CLOSURES.   ALL SUCH ILLEGAL CLOSING TIMES WERE CONVERTED  ,/,    M01. 134
     4 112H TO TIME ZERO (THEY SHOULD SHOW UP IN THE PRINTOUT THAT WAY) M01. 135
     5FOR SIMULATION PURPOSES.   THE IMPLICATIONS OF THIS   ,/,         M01. 136
     6 115H MODIFICATION SHOULD BE UNDERSTOOD BY THE USER.   IF IN ANY DM01. 137
     7OUBT, THE USER IS STRONGLY ADVISED TO SEEK EXPERIENCED   ,/,      M01. 138
     8  25H COUNSEL ON THIS SUBJECT. ,/,1X)                             M15.1332
      WRITE (LUNIT6, 7304)                                              M17. 965
      WRITE (LUNIT6, 7305)                                              M17. 966
 7314 WRITE (LUNIT6, 659 )                                              M17. 967
      FLSTAT(9) = D1                                                    M20.4893
      FLSTAT(10) = D2                                                   M20.4894
      LASTOV = NCHAIN                                                   M15.1333
      NCHAIN = 29                                                          14100
      FLSTAT(7) = FLSTAT(7) + D1                                        M22.5412
      FLSTAT(8) = FLSTAT(8) + D2                                        M22.5413
      IF ( IPRSUP  .GE.  1 )                                            M32.6522
     1 WRITE ( LUNIT6, 4568 )                                           M32.6523
      GO TO 99999                                                       M22.5414
 9800 IF ( M4PLOT  .NE.  1 )   GO TO 9810                               M35.1669
      IF ( KBASE .EQ. 1 )  GO TO 9810                                           
C     VAX SIMULATOR RETURN TO "OVER16" AFTER TABLE-SAVING:              M30.1008
      LASTOV = NCHAIN                                                   M30.1009
      NCHAIN = 16                                                       M30.1010
      IF ( IPRSUP  .GE.  1 )                                            M32.6524
     1 WRITE ( LUNIT6, 4568 )                                           M32.6525
      GO TO 99999                                                       M30.1011
 9810 IF ( LASTOV  .GT.  NCHAIN )   GO TO 9850                          M30.1012
      FLSTAT(7) = FLSTAT(7) + D1                                        M22.5416
      FLSTAT(8) = FLSTAT(8) + D2                                        M22.5417
 9850 LASTOV = NCHAIN                                                   M22.5418
      NCHAIN = 31                                                       M22.5419
      IF ( IPRSUP  .GE.  1 )                                            M32.6526
     1 WRITE ( LUNIT6, 4568 )                                           M32.6527
99999 RETURN                                                               14104
      END                                                                  14105
      SUBROUTINE KATALG                                                    14106
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     VAX-11/780  INSTALLATION-DEPENDENT MODULE WHICH IS USED           M28.6594
C     TO HONOR THE MISCELLANEOUS DATA CARD REQUEST FOR EMTP TABLE       M28.6595
C     SAVING, FOR LATER  "START AGAIN"  USAGE.   A CALL TO THE          M28.6596
C     UNIVERSAL MODULE  "TABLES"  ACTUALLY DUMPS MEMORY ONTO DISK.      M28.6597
C     LOGIC ASSOCIATED WITH OTHER THAN  MEMSAV=0  OR  MEMSAV=1          M30.1013
C     CAN GENERALLY BE IGNORED BY OTHER SYSTEMS;  IT APPLIES            M30.1014
C     ONLY TO RTM (REAL TIME MONITOR) USE OF BPA VAX.                   M30.1015
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*132  ANSI132                                            M40.5778
      COMMON  /COMLOCK/  LOCKER(2)   ! Share with "OVER1" only          M40.5779
      IF ( IPRSUP  .GE.  1 )                                            M28.6599
     1 WRITE (LUNIT6, 2467)  ICAT, MEMSAV, LUNIT2, LTLABL               M28.6600
 2467 FORMAT ( /,  18H ENTER  "KATALG" .,                               M28.6601
     1             32H    ICAT  MEMSAV  LUNIT2  LTLABL                  M28.6602
     2        ,/,  18X,  10I8  )                                        M28.6603
      IF ( MEMSAV .EQ. 0 )  GO TO 9800  ! NO TABLE MOVING AT ALL        M35.1671
      IF ( M4PLOT .EQ. 1 )   ! IF INTERACTIVE EMTP CONTROL, THEN        M35.1672
     1 TMAX = FLTINF      ! MAKE END-TIME OF SIMULATION INFINITE        M35.1673
      IF ( MEMSAV  .EQ.  1016 )   GO TO 2634   ! "RESTORE"   USE        M30.1046
      WRITE (LUNIT6, 2472)                                              M35.1674
 2472 FORMAT ( /,  20X,  '----- "MEMSAV = 1  REPRESENTS',               M28.6606
     1                   ' REQUEST FOR TABLE DUMPING ON DISK.' )        M28.6607
      CLOSE ( UNIT=LUNIT2 )                                             M28.6608
      OPEN  ( UNIT=LUNIT2, TYPE='NEW', FORM='UNFORMATTED',              M28.6609
     1        NAME='TPTABLES.BIN' )                                     M28.6610
      CALL TABLES                                                       M28.6611
      write (LUNIT2)  locker                                            M40.5780
      REWIND 79                                                         M40.5781
      DO 2476  J=1, 9999                                                M40.5782
      READ (79, 2474, END=2479)  ANSI132                                M40.5783
 2474 FORMAT ( A132 )                                                   M40.5784
 2476 WRITE (LUNIT2) ANSI132                                            M40.5785
 2479 CLOSE ( UNIT=LUNIT2, DISP='SAVE' )                                M28.6612
      KARRAY(1) = INDBUF                                                M30.1052
      KARRAY(2) = MFLUSH                                                M30.1053
      KARRAY(3) = NEWVEC                                                M30.1054
      WRITE (LUNIT6, 2483)  LTLABL                                      M35.1675
 2483 FORMAT (  26X,  'SUCCESSFUL SAVING OF EMTP',                      M28.6614
     1         ' TABLES AS FILE  "TPTABLES.BIN" .',                     M28.6615
     2         '    LTLABL  =',  I8  )                                  M28.6616
      IF ( MEMSAV .EQ. 1 )  GO TO 9800  ! EXIT MODULE (NO SPY)          M35.1676
      GO TO 9700     ! EXIT MODULE AFTER "EMTSPY" LINKAGE SET           M35.1677
C     FOLLOWING CODE SERVICES  "RESTORE"  OF  "EMTSPY".  MEMORY         M35.1678
C     IS RESTORED FROM DUMPING OF PREVIOUS "SLEEP" OR "SAVE":           M35.1679
 2634 WRITE (LUNIT6, 2637)  LTLABL                                      M30.1060
 2637 FORMAT (  ' Begin EMTP table restoration.  LTLABL =',             M30.1061
     1  I7,  ' .  Wait for completion.'  )                              M32.6528
      CLOSE ( UNIT=LUNIT2 )                                             M30.1063
      OPEN  ( UNIT=LUNIT2, TYPE='OLD', FORM='UNFORMATTED',              M30.1064
     1        NAME='TPTABLES.BIN' )                                     M30.1065
      CALL TABLES                                                       M30.1066
      CLOSE ( UNIT=LUNIT2, DISP='SAVE' )                                M30.1067
C     NEXT, FOR RTM USE, RESTORE KEY "OVDRIVKOM.FOR" VARIABLES:         M30.1068
      INDBUF = KARRAY(1)                                                M30.1069
      MFLUSH = KARRAY(2)                                                M30.1070
      NEWVEC = KARRAY(3)                                                M30.1071
      WRITE (LUNIT6, 2642)                                              M30.1072
 2642 FORMAT (  ' Restoration Complete, user can now',                  M30.1073
     1      ' begin  DEPOSITing  EMTP  variables via  SPY .' )          M30.1074
      MEMSAV = 0     !  ?????????????  TABLE CHANGES DONE.              M35.1680
 9700 KBREAK = 1   ! LOCK FLAG FOR "EMTSPY" DIALOGUE IN "OVER16"        M35.1681
 9800 RETURN                                                            M13.2995
      END                                                               M13.2996
      SUBROUTINE EMTSPY                                                 M35.1682
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M35.1683
C     IF NO INTERACTIVE USE, CONVERT TO DUMMY MODULE ("RETURN").        M35.1684
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      SAVE                                                                      
      DATA  N18   /   0   /                                                     
      N18 = N18 + 1                                                             
      IF ( N18 .LT. MAXFLG ) GO TO 9008                                         
      N18 = 0                                                                   
      IF ( IPRSPY .LT. 9 )  GO TO 5611                                          
      WRITE (MUNIT6, 5608)  KBREAK, KWTSPY, NBREAK, LOCKBR,             M38.3563
     1                      NCHAIN, JJROLL, T, TBREAK                   M38.3564
 5608 FORMAT ( ' TOP "EMTSPY".  KBREAK, KWTSPY, NBREAK, LOCKBR',        M38.3565
     1   ' JJROLL, NCHAIN =',  6I4,  '    T, TBREAK =',  2E14.5  )      M38.3566
      CALL WINDOW                                                               
 5611 IF ( JJROLL .GT. 0 )  GO TO 5632                                          
      IF ( KBREAK .EQ. 1 ) GO TO 5613                                           
      IF ( LOCKBR .EQ. 1 ) GO TO 5623                                           
      IF ( T .LT. TBREAK ) GO TO 5623                                           
      IF ( NCHAIN .NE. NBREAK ) GO TO 5623                                      
C     OK, NOW WE SERVICE "BREAK" OF SPY (TIME AND PLACE TO DO IT):      M38.3570
      TBREAK = 8877.E33                                                         
 5613 LOCKBR = 1                                                                
      WRITE (MUNIT6, 5615)  NCHAIN, T                                   M38.3572
 5615 FORMAT ( '   // Start "BREAK" service in "EMTSPY".',              M38.3573
     1         '   NCHAIN =', I3,  '   T =', E14.5  )                   M38.3574
      CALL WINDOW                                                               
      IF ( KFILE5 .EQ. 1 ) GO TO 6258                                           
 5617 WRITE (PROM80, 5618)                                                      
 5618 FORMAT ( ' SPY:' )                                                M38.3578
      CALL PROMPT                                                               
 5623 IF ( KSMSPY(3) .EQ. 1 ) GO TO 5632                                        
      CALL FLAGER                                                               
      IF ( KWTSPY .EQ. 1 )  GO TO 5632                                          
      IF ( KFILE5 .EQ. 1 )  GO TO 6258                                          
      GO TO 9000                                                                
 5632 KWTSPY = 0                                                                
      CALL SPYING                                                               
      IF ( JJROLL .GT. 0 ) GO TO 9000                                           
      IF ( LOCKBR .EQ. 1 )  GO TO 5623                                          
      GO TO 9000                                                                
C     ENTRY POINT FOR PRE-DEFINED SPY COMMANDS ($SPY OR "@"):           M38.3590
 6258 IF ( KOMADD .EQ. 0 )  GO TO 6260                                          
      KOMADD = KOMADD + 1                                                       
      BUFF77 = FILE6(KOMADD)                                                    
      IF ( BUFF77(1:3) .EQ. 'EOF' ) GO TO 6278                                  
      GO TO 6264                                                                
 6260 READ (MUNIT5, 6261, END=6274)  BUFF77                                     
 6261 FORMAT ( A80 )                                                    M38.3592
 6264 IF ( KFILE5 .EQ. 1 )                                                      
     1 CALL PERCNT ( BUFF77, 80 )                                               
      IF ( KILPER  .NE.  0 )  GO TO 6274                                        
 6266 CALL SPYING                                                               
      IF ( LOCKBR .EQ. 1 )  GO TO 6258                                          
      GO TO 9000                                                                
C     END-OF-FILE DURING DISK READ, SO SWITCH TO KEYBOARD INPUT:        M38.3599
 6274 CLOSE ( UNIT=MUNIT5 )                                                     
 6278 MUNIT5 = MUNTSV(1)                                                        
      KFILE5 = 0                                                                
      KILPER = 0                                                                
      IF ( MUNTSV(2) .NE. 2288 )                                                
     1 GO TO 5617                                                               
      MUNTSV(2) = 0                                                             
 9000 IF ( IPRSPY .LT. 1 )  GO TO 9008                                          
      WRITE (MUNIT6, 9007)  KBREAK, NCHAIN, LASTOV, M4PLOT              M37.6145
 9007 FORMAT (   ' EXIT "EMTSPY".  KBREAK, NCHAIN, LASTOV,',            M35.1697
     1           ' M4PLOT =',  4I6  )                                   M35.1698
      CALL WINDOW                                                               
 9008 RETURN                                                            M37.6147
      END                                                               M35.1700
      SUBROUTINE SPYING                                                 M35.1701
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M35.1702
C     THIS IS THE PRINCIPLE MODULE, CALLED BY "EMTSPY".                 M35.1703
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*8   CHARD7                                              M37.6148
      REAL*8         SPYTIM(2), SPDATE(2)                               M36.1679
      SAVE                                                              M35.1712
      IF ( IPRSPY .LT. 1 ) GO TO 31006                                          
      WRITE (MUNIT6, 1006)  NCHAIN, JJROLL, KBREAK, LOCKBR,             M38.3608
     1                      NEXMOD, BUFF77(1:20)                        M38.3609
 1006 FORMAT ( ' ENTER "SPYING".  NCHAIN, JJROLL, KBREAK,',             M38.3610
     1  ' LOCKBR, NEXMOD =',  5I5,  '    BUFF77(1:20) =', A20 )         M38.3611
      CALL WINDOW                                                               
      WRITE (*,*)  ' TOP SPYING, KSMSPY(1:3) =',  KSMSPY                M38.3612
31006 IF ( BUFF77(1:4) .NE. 'SPY ' )                                            
     1 GO TO 51006                                                              
      NEXMOD = 0                                                                
      KSMSPY(1) = 2                                                             
      KSMSPY(3) = 0                                                             
51006 IF ( NEXMOD .NE. 2 ) GO TO 1007                                           
      NEXMOD = 0                                                                
      GO TO NEXTSN                                                              
 1007 IF ( NEXMOD .EQ. 3 ) GO TO 8500                                           
      IF ( NEXMOD .EQ. 1 ) GO TO 3208                                           
      IF ( NEXMOD .EQ. 7 ) GO TO 1320                                           
      IF ( NEXMOD .GE. 4 ) GO TO 1319                                           
      IF ( JJROLL .GT. 0 ) GO TO 1520                                           
      IF ( KBRSER .NE. 2 )  GO TO 1009                                  M36.1684
      JWORD = 52                                                                
      GO TO 8500                                                                
 1009 MEMKAR = LOCINT ( KAR1(1) )                                               
      IF ( KFILE5 .NE. 2 )  GO TO 1240                                          
      KFILE5 = 0                                                                
      GO TO 31269                                                               
 1240 ASSIGN 31269 TO NEXTSN                                                    
      WRITE (PROM80, 1260)                                              M38.3631
 1260 FORMAT (  ' SPY:'  )                                              M35.1718
51269 GO TO 9800                                                                
31269 ANSW80 = BUFF77                                                   M36.1693
      IF ( IPRSPY .LE. 9 ) GO TO 39843                                          
      WRITE (MUNIT6, 29843)  ANSW80                                     M37.6156
29843 FORMAT ( '  --- JUST READ ANSW80:',  A80 )                        M35.1722
      CALL WINDOW                                                               
39843 IF ( ANSW80(1:4)  .EQ.  'TYPE'  )   GO TO 2506                            
      IF (ANSW80(1:2) .NE. 'C ') GO TO 1275                                     
      IF ( KFILE5  .EQ.  1    .AND.                                             
     1    KVERFY  .EQ.  0 )                                                     
     2 GO TO 51269                                                              
      IF ( ICOMM .NE. 0 )  GO TO 51269                                          
      WRITE (MUNIT6, 1273)  ANSW80                                              
 1273 FORMAT ( ' COMMENT:',  A80  )                                     M35.1734
      CALL WINDOW                                                               
      GO TO 51269                                                               
 1275 IF (ANSW80(1:1) .NE. '@')  GO TO 1289                                     
C     $$$$$$$  KEY WORD NO. 19:  "@?"       $$$$  $$$$  $$$$  $$$$  $$$$M35.1737
C     NOW PROCESS USER REQUEST FOR DISK-FILE CONNECTION TO KEYBOARD (5):M35.1738
      IF ( KFILE5 .NE. 1 ) GO TO 1278                                           
      WRITE (MUNIT6, 1277)                                              M35.1740
 1277 FORMAT (  '  === Reject  "@"  usage;  one such',                  M38.3638
     1          ' file is presently connected.  Try again.'  )          M38.3639
      CALL WINDOW                                                               
      GO TO 1240                                                        M35.1743
 1278 IF ( ANSW80(2:2) .NE. '/' ) GO TO 51278                                   
      ANSW80(1:2) = '  '                                                        
      CALL FREIN1 ( ANSW80, KOMADD )                                            
      KOMADD = KOMADD - 1                                                       
      N13 = 6                                                                   
      GO TO 1276                                                                
51278 ANSI32 = 'INCLSPY .DAT                    '                               
      KOMADD = 0                                                                
      N26 = 2                                                                   
      GO TO 2511                                                                
C     OK, SO FILE IS LEGAL;  NEXT CHECK FOR ARGUMENT LIST;  EXTRACT ANY:M35.1747
 1276 N6 = N13 + 1                                                              
      MAXARG = 0                                                                
      DO 4531  J=N6, 80                                                         
      IF (ANSW80(J:J) .NE. ' ')  GO TO 4535                                     
 4531 CONTINUE                                                                  
      GO TO 1271                                                                
 4535 MAXARG = 0                                                                
      DO 4549  IDMXXX=1, 10                                                     
      M = 0                                                                     
      ANSI8(1:8) = BLAN80(1:8)                                                  
      DO 4541  L=1, 80                                                          
      IF ( N6  .GT.  80 )   GO TO 4542                                          
      IF ( IPRSPY .LT. 1 )  GO TO 8900                                          
      WRITE (MUNIT6, 8899)  L, M, N6, ANSW80(N6:N6)                     M37.6165
 8899 FORMAT ( ' NEXT CHARACTER. L, M, N6, ANSW80(N6) =',               M35.1762
     1  3I5, 1X, A1 )                                                   M35.1763
      CALL WINDOW                                                               
 8900 IF ( M  .EQ.  0     .AND.                                                 
     1  ANSW80(N6:N6)  .EQ.  '(' )                                              
     2  ANSW80(N6:N6) = ' '                                                     
      IF ( ANSW80(N6:N6)  .EQ.  ')' )   GO TO 4544                              
      IF ( ANSW80(N6:N6)  .EQ.  ' ' )   GO TO 4541                              
      IF ( ANSW80(N6:N6)  .EQ.  ',' )   GO TO 4544                              
      M = M + 1                                                                 
      IF ( M  .LE.  8 )  GO TO 4540                                             
      WRITE (MUNIT6, 4539)  IDMXXX                                      M35.1772
 4539 FORMAT (  '  ??? ARGUMENT NUMBER',  I3,                           M35.1773
     1          '  OF  "@?"  HAS OVER 8 DIGITS.  TRY AGAIN.'   )        M35.1774
      CALL WINDOW                                                               
      GO TO 1240                                                                
 4540 ANSI8(M:M) = ANSW80(N6:N6)                                                
      IF ( ANSI8(M:M)  .EQ.  '#' )                                              
     1 ANSI8(M:M) = ' '                                                         
 4541 N6 = N6 + 1                                                               
C     NO ")" ON "@?"  ARGUMENT LIST MEANS WE RAN THROUGH COLUMN 80:     M35.1780
 4542 IF ( M  .EQ.  0 )   GO TO 1271                                            
C     ONE OR MORE NON-BLANK CHARACTERS SINCE LAST "," IS FINAL ARGUMENT:M35.1782
      MAXARG = MAXARG + 1                                                       
      TEXPAR(MAXARG) = ANSI8                                                    
      IF ( IPRSPY .LT. 1 )  GO TO 1271                                          
      WRITE (MUNIT6, 4543)  MAXARG                                      M37.6174
 4543 FORMAT ( ' MAXARG =',  I8 )                                       M36.1695
      CALL WINDOW                                                               
      GO TO 1271                                                                
 4544 MAXARG = MAXARG + 1                                                       
      READ  (ANSI8, 1360)  TEXPAR(MAXARG)                               M37.6176
 1360 FORMAT ( 15A1 )                                                   M37.6177
      IF ( ANSW80(N6:N6)  .EQ.  ')' )   GO TO 1271                              
 4549 N6 = N6 + 1                                                               
      WRITE (MUNIT6, 4551)                                              M35.1791
 4551 FORMAT (  '   ? ? ?  Illegal  "@?"  use.',                        M38.3640
     1          '   Over 10 arguments.   Try again.'  )                 M38.3641
      CALL WINDOW                                                               
      GO TO 1240                                                                
C     ANY ARGUMENTS HAVE BEEN SUCCESSFULLY FOUND;  NOW USE FILE         M35.1795
 1271 KFILE5 = 1                                                                
      ITEXP = 0                                                                 
      GO TO 51269                                                               
C       FORMER EOF S.N. 1286 WITH FILE CLOSE AND OTHER RESETTING        M38.3643
 1289 IF ( ANSW80(1:8)  .NE.  BLAN80(1:8) )   GO TO 1293                M35.1801
C     BLANK (JUST <CR>) REPRESENTS REQUEST FOR REPEAT "EXAMINE"         M38.3644
      GO TO 1520                                                                
C     CHECK FOR  "%%%%%%%%"  FIELD OF  "@?"  ARGUMENTS;  REPLACE ANY:   M35.1804
C     NEXT CHECK CARD IMAGE FOR ONE OF THE "NUMKEY" KEY-WORDS:          M35.1805
 1293 N13 = 0                                                                   
      IF ( ANSW80(1:5) .NE. 'WAKE4' )  GO TO 1296                       M35.1807
      N13 = 1                                                                   
      ANSW80(5:5) = ' '                                                         
 1296 DO 1302  JWORD=1, NUMKEY                                                  
      IF ( ANSW80(1:8)  .EQ.  SPYKWD(JWORD) )   GO TO 1307                      
 1302 CONTINUE                                                          M35.1811
C     WE REACH HERE IF ILLEGAL RESPONSE TO "SPY:" PROMPT HAS BEEN READ: M35.1812
 1303 WRITE (MUNIT6, 1304)  ANSW80(1:8)                                 M35.1813
 1304 FORMAT ( '   ? ? ?  Illegal SPY command  "',  A,                  M38.3646
     1         '" .   Try again  .....'   )                             M38.3647
      CALL WINDOW                                                               
      GO TO 1240                                                                
 1307 IF ( IPRSPY .LT. 1 ) GO TO 1309                                           
      WRITE (MUNIT6, 1308)  JWORD                                       M37.6181
 1308 FORMAT ( '   KEYWORD FOUND IS  JWORD =',  I8 )                    M36.1710
      CALL WINDOW                                                               
 1309 JWDSAV = JWORD                                                            
      IF ( JWORD .GT. 63 )  GO TO 71309                                         
      GO TO (                                                           M35.1819
C       HEADING  STOP    PLOT    HELP  EXAMINE  DEPOSIT  SWITCH  (1-7)  M35.1820
     1   1500,   9000,   1319,   1311,   1337,   1460,   8500,          M36.1711
C       APPEND   SAVE   RESTORE   GO     ECHO    FIND    LIST    (8-14) M39.3167
     2   1320,   2206,   2317,   2402,   3673,   2040,   2092,          M38.3649
C         SPY    BREAK   WHEN  COMMENT    @?     ROLL   TYPE?   (15-21) M35.1823
     3   1240,   2124,   2430,   2486,   1303,   1424,   1303,          M35.1824
C       VERIFY   FILES   SLEEP  SOURCE   EDIT    WAKE  LANGUAGE (22-28) M35.1825
     4   2494,   2563,   2591,   8500,   8500,   2604,   8500,          M36.1712
C      CATALOG  BEGIN    STEP    DEBUG   DATA    RAMP    TIME   (29-35) M35.1826
     5   2907,   2926,   2937,   2964,   2974,   3095,   3153,          M35.1827
C         TEK   BRANCH   YFORM    NOY   FACTOR    NOF    RLC    (36-42) M35.1828
     6   3174,   3256,   3357,   3381,   3394,   3406,   8500,          M36.1713
C        WIDTH    BUS    SIZE   LIMIT    IOUT    NODE   NONLIN  (43-49) M35.1829
     7   8500,   8500,   8500,   8500,   8500,   8500,   8500,          M36.1714
C        SPACE  LUNIT4  SERIES   LOCK     [Y]     [F]   NOROLL  (50-56) M36.1715
     8   8500,   8500,   8500,   8500,   8500,   8500,   8500,          M36.1716
C        OPEN    CLOSE    SM     HONK   CHOICE   TACS    WAIT   (57-63) M38.3650
     9   3568,   3584,   3606,   3623,   8500,   8500,   3644), JWORD   M37.6186
      CALL STOPTP                                                               
71309 N14 = JWORD - 63                                                          
      GO TO (                                                           M37.6189
C         V-I                                                   (64-64) M37.6190
     1   8500 ), N14                                                    M37.6191
      CALL STOPTP                                                               
C     $$$$$$$  KEY WORD NO.  4:  "HELP"     $$$$  $$$$  $$$$  $$$$  $$$$M35.1831
 1311 WRITE (MUNIT6, 11312 )                                            M35.1832
11312 FORMAT (  '    Available key-word responses to the  "SPY:"',      M35.1833
     3          '  prompt are as follows:'  )                           M35.1834
      CALL WINDOW                                                               
      DO 21313  J=1, NUMKEY, 8                                          M38.3651
      N15 = J + 7                                                               
      IF ( N15 .GT. NUMKEY )  N15 = NUMKEY                                      
      WRITE (MUNIT6, 1313)  (SPYKWD(K), K=J, N15)                               
 1313 FORMAT ( 5X,  8A9 )                                               M35.1835
      CALL WINDOW                                                               
21313 CONTINUE                                                          M37.6199
      JWORD = JWDSAV                                                            
21314 WRITE (PROM80, 1314)                                              M35.1837
 1314 FORMAT (  '       KEY-WORD OF INTEREST (ALL, <CR>,',              M38.3652
     1          ' TOP, BOT, BACK):'  )                                  M35.1839
      ASSIGN 1315 TO NEXTSN                                                     
      GO TO 9800                                                                
 1315 SPYCD2 = BUFF77(1:35)                                                     
      IF ( SPYCD2(1:4) .EQ. '    ')   GO TO 11315                               
      IF ( SPYCD2(1:4) .EQ. 'END ' )  GO TO 1240                                
      IF ( SPYCD2(1:4) .NE. 'BOT ')  GO TO 51315                        M35.1849
      JWORD = NUMKEY                                                            
      GO TO 1317                                                                
51315 IF ( SPYCD2(1:4) .NE. 'BACK' )  GO TO 61315                       M35.1852
      JWORD = JWORD - 1                                                         
      IF ( JWORD .GT. 0 )  GO TO 1317                                           
      JWORD = NUMKEY                                                            
      WRITE (MUNIT6, 31315)                                             M36.1720
31315 FORMAT ( '      //// wrap around, beginning to end ////' )        M36.1721
      CALL WINDOW                                                               
      GO TO 1317                                                                
61315 IF ( SPYCD2(1:4) .EQ. 'TOP ' )   GO TO 41315                              
      IF ( SPYCD2(1:4) .NE. 'NEXT' )   GO TO 21315                              
11315 JWORD = JWORD + 1                                                         
      IF ( JWORD  .LE.  NUMKEY )  GO TO 1317                                    
      IF ( SPYCD2(1:4) .EQ. 'ALL ' )  GO TO 21314                               
      WRITE (MUNIT6, 81315)                                             M36.1722
81315 FORMAT ( '      //// wrap around, end to beginning ////' )        M36.1723
      CALL WINDOW                                                               
41315 JWORD = 1                                                                 
      GO TO 1317                                                                
21315 JWORD = 1                                                                 
      IF ( SPYCD2(1:4) .EQ. 'ALL ' )     GO TO 1317                             
      DO 1316  JWORD=1, NUMKEY                                                  
      IF ( SPYCD2(1:8)  .EQ.  SPYKWD(JWORD) )    GO TO 1317                     
 1316 CONTINUE                                                                  
      WRITE (MUNIT6, 1760)  SPYCD2                                              
      CALL WINDOW                                                               
      GO TO 1318                                                                
 1317 N17 = KBEGTX(JWORD)                                                       
      N18 = KBEGTX(JWORD+1) - 1                                                 
      DO 91315  J=N17, N18                                                      
      MUNIT6(1:81) = ' '//TEXSPY(J)                                             
      CALL WINDOW                                                               
91315 CONTINUE                                                                  
      IF ( JWORD .NE. 4 )  GO TO 61317                                  M37.6209
      WRITE (MUNIT6, 11312)                                                     
      CALL WINDOW                                                               
      DO 51317  J=1, NUMKEY, 8                                                  
      WRITE (MUNIT6, 1313) ( SPYKWD(K), K=J, J+7)                               
      CALL WINDOW                                                               
51317 CONTINUE                                                          M37.6215
61317 JWDSAV = JWORD                                                            
      IF ( SPYCD2(1:4) .NE. 'ALL ' )  GO TO 1318                                
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 11315                                          
      KWTSPY = 0                                                                
 1318 GO TO 21314                                                               
C     $$$$$$$  KEY WORD NO.  3:  "PLOT"     $$$$  $$$$  $$$$  $$$$  $$$$M35.1879
 1319 CALL RTMPLT                                                               
      IF ( NEXMOD .GE. 4 )                                                      
     1 GO TO 9803                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO.  8:  "APPEND"   $$$$  $$$$  $$$$  $$$$  $$$$M39.3168
 1320 CALL APPEND                                                               
      IF ( NEXMOD .EQ. 7 )                                                      
     1 GO TO 9803                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO.  5:  "EXAMINE"  $$$$  $$$$  $$$$  $$$$  $$$$M35.1885
 1337 NUMEX = 0                                                         M35.1886
      KHEAD = 2                                                                 
      HEDING = ' '                                                              
      GO TO 1460                                                                
C     ENCODE HEADING BUFFER WITH VARIABLE REQUEST NOW KNOWN:            M35.1889
 1340 NUMEX = NUMEX + 1                                                 M35.1890
      INTOUT(NUMEX) = INTYPE                                            M35.1891
      IF ( NCHD2  .LT.  6 )                                                     
     1 SPYCD2(NCHD2+1:6) = BLAN80(NCHD2+1:6)                                    
      N14 = N1                                                                  
 1354 IF ( N14  .GT.  N2 )   GO TO 1460                                 M35.1897
      HEDING(KHEAD:KHEAD+14) = BLAN80(KHEAD:KHEAD+14)                   M37.6220
C     REALS OR VECTORS OCCUPY 15 COLS, SO CAN BE SHIFTED:               M35.1902
      IF ( INTYPE .EQ. 0  .OR.  IVEC(IND) .EQ. 1   .OR.                 M35.1903
     1     N2     .NE. 1 )   KHEAD = KHEAD + 2                          M35.1904
C     REAL SCALARS CAN BE SHIFTED 3 MORE COLUMNS:                       M35.1905
      IF ( INTYPE .EQ. 0  .AND.  IVEC(IND) .EQ. 0   .AND.               M35.1906
     1     N2     .EQ. 1 )   KHEAD = KHEAD + 3                          M35.1907
C     NEXT SEE IF SYMBOL CAN BE PROFITABLY CENTERED IN 6-COLUMN SLOT:   M35.1908
      N15 = 6                                                           M35.1909
      IF ( NCHD2  .LE.  4 )   GO TO 1373                                M35.1910
      IF ( NCHD2  .EQ.  6 )   GO TO 1377                                M35.1911
      IF ( HEDING(KHEAD-1:KHEAD-1)  .EQ.  ' ' )   GO TO 1377            M37.6221
      N17 = 1                                                           M35.1913
      GO TO 1374                                                        M35.1914
 1373 N17 = ( 6 - NCHD2 ) / 2                                           M35.1915
 1374 KHEAD = KHEAD + N17                                               M35.1916
      N15 = N15 - N17                                                   M35.1917
 1377 HEDING(KHEAD:KHEAD+5) = SPYCD2(1:6)                               M37.6222
      KHEAD = KHEAD + N15                                               M35.1921
C     REAL SCALARS END UP WITH FOUR BLANKS ON THE RIGHT:                M35.1922
      IF ( INTYPE .EQ. 0  .AND.  IVEC(IND) .EQ. 0   .AND.               M35.1923
     1     N2     .EQ. 1 )   KHEAD = KHEAD + 4                          M35.1924
      LOCOUT(NUMEX) = IND                                               M35.1925
      IMIN(NUMEX) = N1                                                          
      IMAX(NUMEX) = N2                                                          
C     IF ( IVEC(IND)  .EQ.  0 )   GO TO 1460    ! ORIGINAL RECORD.      M35.1928
C     NEXT WE WANT TO EXECUTE S.N. 1394 IF SUBSCRIPTING IS INVOLVED:    M35.1929
      IF ( IVEC(IND)  .EQ.  1 )   GO TO 1394                            M35.1930
      IF ( N1 .EQ. 1   .AND.   N2 .EQ. 1 )   GO TO 1460                 M35.1931
 1394 WRITE (ANSI8, 1400)  N14                                          M35.1933
 1400 FORMAT ( 1H(,  I4,  2H)    )                                      M35.1934
      HEDING(KHEAD:KHEAD+6) = ANSI8(1:7)                                M37.6223
      KHEAD = KHEAD + 7                                                 M35.1936
      N14 = N14 + 1                                                     M35.1937
      GO TO 1354                                                        M35.1938
C     $$$$$$$  KEY WORD NO. 20:  "ROLL"     $$$$  $$$$  $$$$  $$$$  $$$$M35.1939
 1424 JJROLL = 1                                                                
      GO TO 1496                                                                
C     $$$$$$$  KEY WORD NO.  6:  "DEPOSIT"  $$$$  $$$$  $$$$  $$$$  $$$$M35.1943
 1460 WRITE (PROM80, 1480)                                              M35.1944
 1480 FORMAT ( '  SEND EMTP VARIABLE:' )                                M35.1945
      ASSIGN 1485 TO NEXTSN                                                     
      GO TO 9800                                                                
 1485 SPYCD2 = BUFF77(1:35)                                                     
      IF ( SPYCD2(1:1) .NE. 'C'   .OR.                                  M35.1954
     1     SPYCD2(2:2) .NE. ' ' )  GO TO 1491                                   
      IF ( ICOMM .NE. 0 )  GO TO 1460                                           
      WRITE (MUNIT6, 1487)  SPYCD2                                              
 1487 FORMAT ( ' COMMENT:',  A35 )                                      M35.1958
      CALL WINDOW                                                               
      GO TO 1460                                                                
 1491 IF ( SPYCD2(1:4)  .NE.  'END ' )   GO TO 1720                     M35.1960
C     IF PROCESSING "DEPOSIT" = SPYKWD(6), THEN BACK TO "SPY:":         M36.1728
 1496 IF ( ANSW80(1:8)  .EQ.  SPYKWD(6) )    GO TO 1240                 M36.1729
C     IF INTERNAL "DEPOSIT" ISSUED FROM "RAMP"= SPYKWD(34), RETURN:     M36.1730
      IF ( ANSW80(1:8)  .EQ.  SPYKWD(34) )  GO TO 3138                          
C     IF INTERNAL "DEPOSIT", ISSUED FROM "RESTORE", THEN BACK TO "SPY:":M35.1961
      IF ( ANSW80(1:8)  .EQ.  SPYKWD(10) )   GO TO 1240                 M36.1732
C     OK, THIS IS "EXAMINE";  FIRST, BLANK UNUSED RIGHT PORTION:        M38.3680
      IF ( KHEAD .LT. 132 )                                                     
     1 HEDING(KHEAD+1:132) = ' '                                                
C     IF DISK FILE IS CONNECTED TO UNIT 5, WE WANT RETURN TO  "SPY:" :  M35.1962
      IF ( KFILE5  .EQ.  1 )   GO TO 1240                                       
C     $$$$$$$  KEY WORD NO.  1:  "HEADING"  $$$$  $$$$  $$$$  $$$$  $$$$M35.1964
 1500 MUNIT6 = HEDING                                                           
      CALL WINDOW                                                               
C     FOLLOWING CODE OUTPUTS TABLE DUMPING IN ACCORD WITH EARLIER       M35.1969
C     SETUP USING  "EXAMINE"  SPECIFICATION.                            M35.1970
 1520 CALL EXAMIN                                                               
      IF ( JJROLL  .EQ.  0 )  GO TO 1682                                        
      JJROLL = JJROLL + 1                                                       
      IF ( JJROLL .LE. 2 )  GO TO 1669                                          
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 1523                                           
      JJROLL = 0                                                                
      GO TO 1240                                                                
 1523 IF ( OUTSAV .EQ. OUTLIN ) GO TO 1684                                      
 1669 OUTSAV = OUTLIN                                                           
C     OUTPUT LINE OF "EXAMINE" IS ALL BUILT;  DISPLAY IT:               M37.6236
 1682 IF ( KFILE5 .NE. 1  .AND.                                                 
     1     JJROLL .EQ. 0  .AND.                                                 
     2     KOLOUT .LE. 81 ) GO TO 1685                                          
      MUNIT6 = OUTLIN                                                           
      CALL WINDOW                                                               
      PROM80(1:8) = BLAN80(1:8)                                                 
 1684 IF ( JJROLL .GT. 0 )  GO TO 9833                                          
      GO TO 1714                                                                
 1685 PROM80 = OUTLIN(1:KOLOUT-1)                                               
 1714 ASSIGN 31269 TO NEXTSN                                                    
      GO TO 9800                                                                
C     PROCESS EMTP SYMBOL NAME (SPYCD2) WHICH HAS JUST BEEN READ;       M35.2093
 1720 NCHD2 = 0                                                         M35.2094
      N13 = 0                                                           M35.2095
      DO 1723  J=1, 8                                                   M35.2096
      IF ( SPYCD2(J:J)  .EQ.  '('  )   GO TO 1726                       M35.2097
      IF ( SPYCD2(J:J)  .EQ.  ','  )   GO TO 1726                       M35.2098
      IF ( SPYCD2(J:J)  .NE.  ' ' )   GO TO 1721                        M35.2099
      IF ( NCHD2  .EQ.  0 )   NCHD2 = J - 1                             M35.2100
      GO TO 1723                                                        M35.2101
 1721 IF ( SPYCD2(J:J)  .NE.  '='  )   GO TO 1723                       M35.2102
      N13 = J                                                           M35.2103
      IF ( NCHD2  .EQ.  0 )   NCHD2 = J - 1                             M35.2104
 1723 CONTINUE                                                          M35.2105
      N1 = 1                                                            M35.2106
      N2 = 1                                                            M35.2107
      J = N13                                                           M35.2108
      IF ( N13  .EQ.  0 )   J = 7                                       M35.2109
      GO TO 1735                                                        M35.2110
 1726 IF ( NCHD2  .EQ.  0 )   NCHD2 = J - 1                             M35.2111
      N9 = 0                                                            M35.2112
      DO 1733  K=J, 20                                                  M35.2113
      IF ( SPYCD2(K:K)  .NE.  '-' )   GO TO 1727                        M35.2114
      N12 = -1                                                          M35.2115
      GO TO 1733                                                        M35.2116
 1727 IF ( SPYCD2(K:K)  .EQ.  ' ' )   GO TO 1733                        M35.2117
      IF ( SPYCD2(K:K)  .NE.  '='  )   GO TO 21727                      M35.2118
      N13 = K                                                                   
      GO TO 1734                                                                
21727 DO 1728  I=1, 10                                                  M35.2121
      IF ( SPYCD2(K:K)  .EQ.  DIGIT(I) )   GO TO 1730                   M35.2122
 1728 CONTINUE                                                          M35.2123
      IF ( N9  .GT.  0 )   LIMARR(N9) = LIMARR(N9) * N12                        
C     NON-DIGIT MEANS WE INITIALIZE FOR NEXT NUMBER TO BE BUILT:        M35.2125
      N9 = N9 + 1                                                               
      N12 = 1                                                                   
      LIMARR(N9) = 0                                                            
      GO TO 1733                                                        M35.2129
 1730 IF ( I  .EQ.  10 )   I = 0                                                
      LIMARR(N9) = 10 * LIMARR(N9)  +  I                                        
 1733 CONTINUE                                                          M35.2132
 1734 N1 = LIMARR(1)                                                    M35.2133
      N2 = LIMARR(2)                                                    M35.2134
      IF ( N9  .LE.  2 )   N2 = N1                                      M35.2135
 1735 SPYCD2(J:8) = BLAN80(J:8)                                                 
C     NEXT IDENTIFY THE EMTP NAME INVOLVED IN THIS REQUEST:             M35.2138
 1738 DO 1740  IND=1, NUMSYM                                            M35.2139
      IF ( SPYCD2(1:8)  .EQ.  SYMB(IND) )   GO TO 1780                  M35.2140
 1740 CONTINUE                                                          M35.2141
      WRITE (MUNIT6, 1760)  SPYCD2                                      M35.2142
 1760 FORMAT ( '   ? ?  Sorry, no symbol match for  "', A6, 3H" . )     M38.3698
      CALL WINDOW                                                               
      GO TO 1460                                                        M35.2144
 1780 INTYPE = 0                                                        M35.2145
      IF ( SPYCD2(1:1)  .EQ.  'I' )   INTYPE = 1                        M35.2146
      IF ( SPYCD2(1:1)  .EQ.  'J' )   INTYPE = 1                        M35.2147
      IF ( SPYCD2(1:1)  .EQ.  'K' )   INTYPE = 1                        M35.2148
      IF ( SPYCD2(1:1)  .EQ.  'L' )   INTYPE = 1                        M35.2149
      IF ( SPYCD2(1:1)  .EQ.  'M' )   INTYPE = 1                        M35.2150
      IF ( SPYCD2(1:1)  .EQ.  'N' )   INTYPE = 1                        M35.2151
      IF ( IVEC(IND) .EQ. 1   .OR.   N2 .LE. N1 )  GO TO 31807          M37.6245
      WRITE (MUNIT6, 1807)                                              M37.6246
 1807 FORMAT ( '    Note :  This is a vector dump using a scalar',      M38.3699
     1         ' EMTP variable.'  )                                     M38.3700
      CALL WINDOW                                                               
31807 IF ( ANSW80(1:8)  .EQ.  SPYKWD(5) )   GO TO 1340                          
      IF ( ANSW80(1:8)  .EQ.  SPYKWD(34) )   GO TO 3134                         
C     FOR THE CASE OF "DEPOSIT", THE USER-DESIRED VALUE MUST BE READ:   M35.2156
      IF ( N13  .EQ.  0 )   GO TO 1837                                          
      IF ( IASCII(IND)  .EQ.  1 )   GO TO 1819                                  
      N13 = N13 + 1                                                             
      N22 = 0                                                                   
      DO 1813  J=N13, 35                                                        
      IF ( SPYCD2(J:J)  .EQ.  ' ' )   GO TO 1813                                
      N22 = N22 + 1                                                             
      IF ( N22  .LE.  20 )   GO TO 1808                                         
      WRITE (MUNIT6, 51807)                                             M36.1735
51807 FORMAT ( ' Sorry, SPY buffer overflow.  Try again ....' )         M38.3701
      CALL WINDOW                                                               
      GO TO 1837                                                                
 1808 BYTBUF(N22:N22) = SPYCD2(J:J)                                             
 1813 CONTINUE                                                                  
      IF ( N22 .LT. 20 )                                                        
     1 BYTBUF(N22+1:20) = BLAN80(N22+1:20)                                      
C     FOLLOWING CODE IS FOR ALPHANUMERIC EMTP SYMBOL AFTER "=" :        M35.2172
 1819 ANSI8(1:6) = SPYCD2(N13+1:N13+6)                                          
      IF ( IPRSPY .LT. 1 ) GO TO 1824                                           
      WRITE (MUNIT6, 1820)  ANSI8                                       M37.6253
 1820 FORMAT ( ' ANSI8 AFTER ALPHANUMERIC TRANSFER =',  A )             M37.6254
      CALL WINDOW                                                               
 1824 GO TO 1846                                                        M38.3703
C     NO EQUAL SIGN MEANS THAT WE PROMPT FOR SEPARATE NUMBER:           M35.2179
 1837 WRITE (PROM80, 1840)                                              M35.2180
 1840 FORMAT ( 4X,  'NEW VALUE:' )                                      M35.2181
      ASSIGN 1842 TO NEXTSN                                                     
      GO TO 9800                                                                
 1842 BYTBUF = BUFF77(1:20)                                                     
      IF ( IASCII(IND)  .EQ.  0 )   GO TO 1844                                  
      ANSI8(1:6) = BYTBUF(1:6)                                          M37.6256
      GO TO 1846                                                                
C     FOLLOWING CODE FIRST CHECKS 20-COLUMN "BYTBUF" WORKING VECTOR     M35.2192
C     FOR A LEGAL FORTRAN NUMBER;  THEN FREE-FORMAT WRITE/READ GETS IT: M35.2193
 1844 CALL NUMCHK ( BYTBUF, 20, N33 )                                           
      IF ( N33  .EQ.  1 )   GO TO 1837                                          
      CALL FREFP1 ( BYTBUF, D4 )                                                
 1846 CALL DEPOSI ( IND, INTYPE, N1, N2, D4 )                                   
      GO TO 1460                                                                
C     $$$$$$$  KEY WORD NO. 13:  "FIND"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2222
 2040 WRITE (MUNIT6, 2042)                                              M37.6259
 2042 FORMAT ( 1X,  40H    Symbol      Word   Address      Next   )     M38.3707
      CALL WINDOW                                                               
 2043 WRITE (PROM80, 2044)                                              M38.3708
 2044 FORMAT ( '      index   address    change      symbol :' )        M38.3709
 2050 ASSIGN 2048 TO NEXTSN                                                     
      GO TO 9800                                                                
 2048 SPYCD2 = BUFF77(1:35)                                                     
      IF ( SPYCD2(1:8)  .EQ.  'END     '  )   GO TO 1240                        
      IF ( SPYCD2(1:8)  .EQ.  'STOP    '   )   GO TO 1240                       
      IF ( SPYCD2(1:8)  .NE.  BLAN80(1:8)   )   GO TO 2052              M35.2228
      WRITE (PROM80, 2051)                                              M35.2229
 2051 FORMAT ( '   ? ? ?  Blank EMTP symbol is clearly wrong.',         M38.3714
     1         '  Try again :'  )                                       M38.3715
      GO TO 2050                                                                
C     FIRST WE MUST SEE IF  "SPYCD2"  HAS ANY WILD-CARD CHARACTERS:     M35.2234
 2052 DO 2053  I=1, 8                                                           
      IF ( SPYCD2(I:I)  .EQ.  '*'  )   GO TO 2069                               
 2053 CONTINUE                                                          M35.2237
C     SIMPLE CASE WITH NO WILD-CARD CHARACTERS:                         M35.2238
      DO 2055  IND=1, NUMSYM                                                    
      IF ( SPYCD2(1:8)  .EQ.  SYMB(IND) )   GO TO 2063                          
 2055 CONTINUE                                                          M35.2241
      WRITE (MUNIT6, 2088)                                                      
      CALL WINDOW                                                               
      GO TO 2050                                                                
 2063 N4 = LOCATE(IND) - N5                                                     
      N5 = LOCATE(IND)                                                          
      WRITE (PROM80, 2066)  IND, N5, N4                                         
 2066 FORMAT ( 1X,  3I10,  4X, 1H: )                                    M38.3716
      CALL PROMPT                                                               
      GO TO 2050                                                                
C     BEGIN SEARCH FOR CASE WHERE "D2" HAS ONE OR MORE WILD CARDS ("*"):M35.2248
 2069 N4 = 0                                                                    
      DO 2087  IND=1, NUMSYM                                                    
      CHARD7 = SYMB(IND)                                                        
      N7 = 1                                                                    
      N2 = 1                                                                    
C     ENTER LOOP WHICH COMPARES  CHARD2(N2)  AGAINST  CHARD7(N7):       M35.2254
 2071 IF ( SPYCD2(N2:N2) .NE. '*' )  GO TO 2079                                 
C     ENTER CODE WHERE "*" IMPLIES SKIPPING INDETERMINATE NUMBER OF     M35.2256
C     CHARACTERS IN  "D7" :                                             M35.2257
 2072 N2 = N2 + 1                                                               
      IF ( N2  .GE.  9 )  GO TO 2081                                            
      IF ( SPYCD2(N2:N2)  .EQ.  '*' )   GO TO 2072                              
      IF ( SPYCD2(N2:N2)  .EQ.  ' ' )   GO TO 2081                              
C     NONBLANK  SPYCD2(N2:N2) MEANS WE MUST SEARCH "D7" FOR IT:         M35.2262
      DO 2074  I=N7, 6                                                          
      IF ( CHARD7(I:I) .EQ. SPYCD2(N2:N2) )  GO TO 2077                         
 2074 CONTINUE                                                          M35.2265
      GO TO 2086                                                                
 2077 N2 = N2 + 1                                                               
      N7 = I + 1                                                                
      IF ( N2  .GE.  9 )  GO TO 2081                                            
      GO TO 2071                                                                
C     NOW WE ARE READY TO CHECK CHARACTERS OF TWO WORDS FOR MATCH:      M35.2271
 2079 IF ( SPYCD2(N2:N2) .NE. CHARD7(N7:N7) )  GO TO 2086                       
      N2 = N2 + 1                                                               
      N7 = N7 + 1                                                               
      IF ( N2  .GE.  9 )   GO TO 2081                                           
      IF ( SPYCD2(N2:N2) .NE. ' ' )  GO TO 2071                                 
C     BLANK IN "D2" MEANS SUCCESS;  WE HAVE FOUND A MATCH.  NOW OUTPUT: M35.2277
 2081 N4 = LOCATE(IND) - N5                                                     
      N6 = LOCATE(IND)                                                          
      WRITE (MUNIT6, 2083)  IND, N6, N4, CHARD7                         M38.3717
 2083 FORMAT ( 1X,  3I10,  6X,  A6,  ' = Name  '  )                     M38.3718
      CALL WINDOW                                                               
 2086 CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 2087                                           
      KWTSPY = 0                                                                
      GO TO 2040                                                                
 2087 CONTINUE                                                                  
      IF ( N4 .NE. 0 )  GO TO 2043                                              
      WRITE (PROM80, 2088)                                              M38.3722
 2088 FORMAT ( '    Sorry, no matches.   Try again :' )                 M38.3723
      GO TO 2050                                                        M35.2292
C     $$$$$$$  KEY WORD NO. 14:  "LIST"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2293
 2092 WRITE (MUNIT6, 2093)  NUMSYM                                      M35.2294
 2093 FORMAT (                                                          M38.3724
     1  '    Row    Symbol    Word    Vector?    Ascii?   Next',        M38.3725
     2      5X,  I4,  ' Symbols'   )                                    M38.3726
      CALL WINDOW                                                               
      WRITE (PROM80, 2095)                                              M38.3727
 2095 FORMAT (                                                          M38.3728
     1  '  Number    Name   Address   (Yes=1)   (Yes=1)   :'  )         M38.3729
      N33 = 0                                                                   
 2099 ASSIGN 2101 TO NEXTSN                                                     
      GO TO 9800                                                                
 2101 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .EQ.  'STOP' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .NE.  '    '   .AND.                                    
     1     BYTBUF(1:4)  .NE.  'NEXT' )  GO TO 2102                              
      N1 = N2 + 1                                                               
      N2 = N1                                                                   
      GO TO 2108                                                                
 2102 IF ( BYTBUF(1:4) .NE. 'TOP ' )  GO TO 2103                        M38.3738
      N1 = 1                                                            M36.1752
      N2 = 1                                                                    
      GO TO 2109                                                        M36.1754
 2103 IF ( BYTBUF(1:4) .NE. 'BOT ' )  GO TO 2105                        M36.1755
      N1 = NUMSYM                                                       M36.1756
      N2 = NUMSYM                                                       M36.1757
      GO TO 2109                                                        M36.1758
 2105 IF ( BYTBUF(1:4) .NE. 'ALL ' )  GO TO 2107                        M36.1759
      N1 = 1                                                            M36.1760
      N2 = NUMSYM                                                       M36.1761
      GO TO 2109                                                        M36.1762
 2107 CALL FREIN2 ( BYTBUF, N1, N2 )                                    M36.1763
 2108 IF ( N1 .LE. 0 )  N1 = 1                                                  
      IF ( N1 .GT. NUMSYM )  N1 = NUMSYM                                        
      IF ( N2 .LE. N1 )  N2 = N1                                                
      IF ( N2 .GT. NUMSYM )  N2 = NUMSYM                                        
 2109 DO 2116  I=N1, N2-1                                                       
      WRITE (MUNIT6, 2113)  I, SYMB(I), LOCATE(I), IVEC(I),             M38.3743
     1                      IASCII(I)                                   M36.1768
 2113 FORMAT ( I8, 4X, A6, I9, I8, I10 )                                M38.3744
      CALL WINDOW                                                               
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 2116                                           
      KWTSPY = 0                                                                
      GO TO 2092                                                                
 2116 CONTINUE                                                                  
      WRITE (PROM80, 3217)  N2, SYMB(N2), LOCATE(N2), IVEC(N2),         M38.3751
     1                      IASCII(N2)                                  M38.3752
 3217 FORMAT ( I8, 4X, A6, I9, I8, I10, 4X,  1H:  )                     M38.3753
      GO TO 2099                                                        M35.2313
C     $$$$$$$  KEY WORD NO. 16:  "BREAK"    $$$$  $$$$  $$$$  $$$$  $$$$M35.2314
 2124 WRITE (PROM80, 2125)                                              M35.2315
 2125 FORMAT (  '   SEND  T-break  IN',                                 M38.3754
     1          ' SEC. ("-" MEANS STEP #):'  )                          M38.3755
      ASSIGN 2126 TO NEXTSN                                                     
      GO TO 9800                                                                
 2126 CALL FREFP1 ( BUFF77, TBREAK )                                            
      NBREAK = 16                                                               
      IF ( TBREAK .GT. 0.0 )  GO TO 2142                                        
      IF ( TBREAK .EQ. 0.0 )  GO TO 2131                                        
      D13 =  - TBREAK * DELTAT                                                  
      WRITE (MUNIT6, 2128)  DELTAT, D13                                 M35.2322
 2128 FORMAT ( '    Ok, using  DELTAT =',  E12.3,                       M36.1774
     1         '  this gives  T-break =',  E13.5  )                     M36.1775
      CALL WINDOW                                                               
      TBREAK = D13                                                              
      GO TO 2142                                                                
 2131 WRITE (PROM80, 2136)                                              M36.1778
 2136 FORMAT (  '    SEND UTPF OVERLAY NUMBER NCHAIN FOR',              M36.1779
     1          ' THE BREAK :'   )                                      M36.1780
      ASSIGN 2139 TO NEXTSN                                                     
      GO TO 9800                                                                
 2139 CALL FREIN1 ( BUFF77, NBREAK )                                            
      IF ( NBREAK .LT. -1   .OR.                                                
     1     NBREAK .GT. 20 )  GO TO 2131                                         
      IF ( NBREAK .NE. 16 )                                                     
     1 TBREAK = -9876.E33                                                       
 2142 GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO.  9:  "SAVE"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2323
 2206 TMAX = T - EPSILN                                                         
      IF ( TWHEN  .GT.  FLZERO )   TMAX = TWHEN                                 
      MEMSAV = 16                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 10:  "RESTORE"  $$$$  $$$$  $$$$  $$$$  $$$$M35.2328
 2317 TMAX = T - EPSILN                                                         
      IF ( TWHEN  .GT.  FLZERO )   TMAX = TWHEN                                 
      MEMSAV = 1016                                                             
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 11:  "GO"       $$$$  $$$$  $$$$  $$$$  $$$$M35.2333
 2402 LOCKBR = 0                                                                
      IF ( IPRSPY .LT. 1 )  GO TO 2409                                          
      WRITE (MUNIT6, 2407)                                              M37.6271
 2407 FORMAT ( ' "GO" ACKNOWLEDGED, SET LOCKBR = 0.'  )                 M36.1790
      CALL WINDOW                                                               
 2409 GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 17:  "WHEN"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2338
 2430 WRITE (PROM80, 2433)  TWHEN                                       M38.3765
 2433 FORMAT (  '  SUPPLY TMAX FOR "SAVE" & "RESTORE"  [',  F8.4,       M35.2341
     1          ' ] :'   )                                              M35.2342
      ASSIGN 2436 TO NEXTSN                                                     
      GO TO 9800                                                                
 2436 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4)  .EQ.  '    ' )   GO TO 2439                             
      CALL FREFP1 ( BYTBUF, D34 )                                               
      IF ( D34  .GT.  0.0 )   TWHEN = D34                               M35.2350
 2439 GO TO 1240                                                        M35.2351
C     $$$$$$$  KEY WORD NO. 18:  "COMMENT"  $$$$  $$$$  $$$$  $$$$  $$$$M35.2352
 2486 IF ( ICOMM  .EQ.  0 )   GO TO 2491                                        
      ICOMM = 0                                                                 
      WRITE (MUNIT6, 2488)                                              M36.1792
 2488 FORMAT (                                                          M36.1793
     1    '   Begin displaying comment cards of  "@"  usage.' )         M38.3769
      CALL WINDOW                                                               
      GO TO 1240                                                                
 2491 ICOMM = 1                                                                 
      WRITE (MUNIT6, 2492)                                              M36.1795
 2492 FORMAT (                                                          M36.1796
     1     '   Stop showing comment cards of  "@"  usage.' )            M38.3770
      CALL WINDOW                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 22:  "VERIFY"   $$$$  $$$$  $$$$  $$$$  $$$$M35.2358
 2494 IF ( KVERFY  .EQ.  0 )   GO TO 2497                                       
      KVERFY = 0                                                                
      WRITE (MUNIT6, 2495)                                              M36.1798
 2495 FORMAT ( '   Begin echoing commands of  "@"  file usage.' )       M38.3771
      CALL WINDOW                                                               
      GO TO 1240                                                                
 2497 KVERFY = 1                                                                
      WRITE (MUNIT6, 2499)                                              M36.1800
 2499 FORMAT ( '   Stop echoing commands of  "@"  file usage.' )        M38.3772
      CALL WINDOW                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 21:  "TYPE?"    $$$$  $$$$  $$$$  $$$$  $$$$M35.2364
 2506 N16 = 0                                                                   
      N26 = 5                                                                   
      ANSI32 = 'INCLSPY .DAT                    '                               
      IF ( IPRSPY .LT. 1 )  GO TO 2509                                          
      WRITE (MUNIT6, 2508)   ANSW80(1:20)                               M37.6278
 2508 FORMAT ( '   Start "TYPE" service.  ANSW80(1:20) =', A20 )        M38.3774
      CALL WINDOW                                                               
 2509 IF ( ANSW80(5:7) .EQ. '?  ' ) N16 = 1                                     
      IF ( ANSW80(5:7) .EQ. ' ? ' ) N16 = 1                                     
      IF ( ANSW80(5:7) .EQ. '  ?' ) N16 = 1                                     
      IF ( ANSW80(1:8) .EQ. 'TYPE    '  )   N16 = 1                             
      IF ( N16 .EQ. 0 ) GO TO 2511                                              
      N18 = 0                                                                   
      GO TO 2548                                                                
 2511 N12 = 0                                                                   
      N13 = 0                                                                   
      DO 2517  J=N26, 32                                                        
      IF ( N12 .GT. 0 )  GO TO 2514                                             
      IF ( ANSW80(J:J) .EQ. ' ' )  GO TO 2517                                   
      N12 = J                                                                   
 2514 IF ( ANSW80(J:J) .EQ. ' ' ) GO TO 2519                                    
      N13 = J                                                                   
 2517 CONTINUE                                                                  
 2519 IF ( IPRSPY .LT. 1 )  GO TO 32521                                         
      WRITE (MUNIT6, 2521)  N12, N13                                    M37.6282
 2521 FORMAT ( ' FILE NAME BOUNDED.  N12, N13 =',  2I5 )                M36.1803
      CALL WINDOW                                                               
32521 IF ( N13 .NE. N12 )  GO TO 2524                                           
      ANSI32(8:8) = ANSW80(N12:N12)                                             
      DO 2522  I=1, 9                                                           
      IF ( ANSW80(N12:N12) .EQ. DIGIT(I)  .AND.                                 
     1     FILEXT(I) .EQ. 'X' )                                                 
     2 GO TO 2536                                                               
 2522 CONTINUE                                                                  
      WRITE (MUNIT6, 2526) ANSW80(N12:N12)                                      
      CALL WINDOW                                                               
      GO TO 1240                                                                
 2524 INQUIRE (FILE=ANSW80(N12:N13), EXIST=LOGVAR)                      M35.2394
      IF ( LOGVAR ) GO TO 2529                                                  
      WRITE (MUNIT6, 2526) ANSW80(N12:N13)                                      
 2526 FORMAT ( '   ?? ??  Sorry, no such file named : ', A32 )          M38.3778
      CALL WINDOW                                                               
      GO TO 1240                                                                
 2529 ANSI32 = BLAN80(1:32)                                                     
      ANSI32(1:N13-N12+1) = ANSW80(N12:N13)                                     
 2532 IF ( ANSW80(1:1) .EQ. '@' )  GO TO 2536                                   
      WRITE (MUNIT6, 2534)  ANSI32                                              
 2534 FORMAT ( 20X,  ' ******  Listing of file : ',  A32 )              M38.3779
      CALL WINDOW                                                               
 2536 MUNTSV(1) = MUNIT5                                                M36.1804
      MUNIT5 = MUNTSV(2)                                                M36.1805
 2537 OPEN (UNIT=MUNIT5, STATUS='OLD', FILE=ANSI32)                             
      IF ( ANSW80(1:1) .EQ. '@' ) GO TO 1276                                    
      DO 2542  I=1, 9999                                                        
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 )  GO TO 2538                                          
      KWTSPY = 0                                                                
      GO TO 2545                                                                
 2538 READ (MUNIT5, 1269, END=2545)  BUFF77                                     
 1269 FORMAT ( A80 )                                                    M38.3781
      WRITE (MUNIT6, 2539)  I, BUFF77                                           
 2539 FORMAT ( ' Line', I4,  ': ',  A80 )                               M38.3782
      CALL WINDOW                                                               
 2542 CONTINUE                                                                  
 2545 CLOSE (UNIT=MUNIT5, STATUS='KEEP')                                        
      IF ( N16 .EQ. 0 ) GO TO 2551                                              
 2548 N18 = N18 + 1                                                             
      IF ( N18 .EQ. 10 )  GO TO 2551                                            
      IF ( FILEXT(N18) .NE. 'X' )  GO TO 2548                                   
      ANSI32(8:8) = DIGIT(N18)                                                  
      GO TO 2537                                                                
 2551 MUNIT5 = MUNTSV(1)                                                        
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 23:  "FILES"    $$$$  $$$$  $$$$  $$$$  $$$$M35.2419
C     "FILES"  RESULTS IN A DISPLAY OF THE STATUS OF "@?" FILES, AS     M35.2420
C     PREVIOUSLY DETERMINED BY  "INITSP"  AS SIMULATION BEGAN.          M35.2421
 2563 WRITE (MUNIT6, 2572)  (J, J=1, 9)                                 M37.6291
 2572 FORMAT (  '   File number :',  9I5  )                             M38.3783
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2576)  ( FILEXT(J), J=1, 9 )                       M37.6294
 2576 FORMAT (  '   INCLSPY?.DAT:',  9(4X, A1) )                        M38.3784
      CALL WINDOW                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 24:  "SLEEP"    $$$$  $$$$  $$$$  $$$$  $$$$M35.2426
C     "SLEEP"  PROVIDES FOR EXIT TO OVERLAY 20, FOLLOWED BY THE         M35.2427
C     SAVING OF EMTP TABLES ON DISK.   THE SUBSEQUENT USE OF            M35.2428
C     "WAKE" CAN REVIVE THE SUSPENDED SIMULATION.                       M35.2429
 2591 TMAX = -9999.                                                             
      MEMSAV = 2016                                                             
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 25:  "WAKE"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2433
 2604 WRITE (MUNIT6, 32604)                                             M36.1811
32604 FORMAT ( '     Begin  "WAKE"  processing.'  )                     M38.3785
      CALL WINDOW                                                               
      IF ( N13 .NE. 1 )  GO TO 2611                                             
C     REGENERATE HEADER OF PLOT FILE ATTACHED TO UNIT LUNIT4:           M35.2435
      WRITE (MUNIT6, 2605)                                              M36.1813
 2605 FORMAT ( '    Start regenerating LUNIT4 header.'  )               M38.3786
      CALL WINDOW                                                               
      REWIND LUNIT4                                                     M35.2436
      READ (LUNIT4)  DATE1, TCLOCK, NUMNAM, NUMNVO, NUMBCO,             M35.2437
     1               NUMBRN                                             M35.2438
      IF ( LBUS  .GE.  NUMNAM    .AND.                                  M35.2439
     1     LSIZ12  .GE.  NUMNVO    .AND.                                M38.3787
     2     LBRNCH .GE. NUMBRN )   GO TO 2608                            M35.2441
      WRITE (MUNIT6, 2606)                                              M37.6299
 2606 FORMAT (  '   $$$$  Overflow.  EMTP List Sizes',                  M38.3788
     1          ' are too small to regenerate the header',              M38.3789
     2          ' information of the plot file.'  )                     M38.3790
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2607)  NUMNAM, NUMNVO, NUMBRN                      M37.6302
 2607 FORMAT (  '       Abort this command.  Sorry.',                   M38.3791
     1          '   NUMNAM, NUMNVO, NUMBRN =', 3I5 )                    M37.6304
      CALL WINDOW                                                               
      GO TO 1240                                                                
 2608 REWIND LUNIT4                                                     M35.2448
      READ (LUNIT4)  DATE1, TCLOCK, NUMNAM, NUMNVO, NUMBCO,             M35.2449
     1               NUMBRN, ( BUS(J), J=1, NUMNAM )                    M35.2450
      IF ( NUMNVO  .GT.  0 )                                            M35.2451
     1 READ (LUNIT4)  ( IBSOUT(J), J=1, NUMNVO )                        M35.2452
      IF ( NUMBRN  .GT.  0 )                                            M35.2453
     1 READ (LUNIT4)  ( KBUS(J), MBUS(J), J=1, NUMBRN )                 M35.2454
      REWIND LUNIT4                                                     M35.2455
      WRITE (LUNIT4)  DATE1, TCLOCK, NUMNAM, NUMNVO, NUMBCO,            M35.2456
     1                NUMBRN, ( BUS(J), J=1, NUMNAM )                   M35.2457
      IF ( NUMNVO  .GT.  0 )                                            M35.2458
     1 WRITE (LUNIT4)  ( IBSOUT(J), J=1, NUMNVO )                       M35.2459
      IF ( NUMBRN  .GT.  0 )                                            M35.2460
     1 WRITE (LUNIT4)  ( KBUS(J), MBUS(J), J=1, NUMBRN )                M35.2461
 2611 CALL RUNTYM ( D1, D2 )                                            M35.2462
      CALL PFATCH                                                       M35.2463
      CALL TABLES                                                       M35.2464
      FLSTAT(1) = -D1                                                   M35.2465
      FLSTAT(2) = -D2                                                   M35.2466
      IF ( LSTAT(16) .EQ. LTLABL )  GO TO 2614                          M35.2467
      WRITE (MUNIT6, 2612)                                              M35.2468
 2612 FORMAT ( '   ????  Sorry, present EMTP dimensions',               M38.3792
     1         ' do not agree with those of the disk file' )            M38.3793
      CALL WINDOW                                                               
      WRITE (MUNIT6, 32612)                                             M37.6308
32612 FORMAT ( 9X, 'just attached.  Sleeping simulation can not',       M38.3794
     1         ' be awakened.  Abort the command.  Sorry.'  )           M38.3795
      CALL WINDOW                                                               
      GO TO 1240                                                                
 2614 KILL = 7733                                                               
      LSTAT(15) = 16                                                            
      GO TO 2402                                                                
C     $$$$$$$  KEY WORD NO. 29:  "CATALOG"  $$$$  $$$$  $$$$  $$$$  $$$$M35.2478
 2907 WRITE (PROM80, 2910)                                              M35.2479
 2910 FORMAT ( ' SEND COMPLETE, LEGAL DISK-FILE NAME:'  )               M35.2480
      ASSIGN 2913 TO NEXTSN                                                     
      GO TO 9800                                                                
 2913 ANSI32 = BUFF77(1:32)                                             M38.3798
      IF ( ANSI32 .EQ. BLAN80(1:32) )                                           
     1 ANSI32 = 'SPYDATA                         '                              
      CLOSE (UNIT=LUNTSP)                                               M35.2486
      OPEN (UNIT=LUNTSP, STATUS='NEW', FILE=ANSI32)                     M36.1816
      N1 = 1                                                            M35.2487
      N2 = 80                                                           M35.2488
      DO 2914  J=1, NUMCRD                                                      
 2914 WRITE (LUNTSP, 1269)  FILE6(J)                                            
      WRITE (MUNIT6, 2917)  LUNTSP, NUMCRD                              M35.2490
 2917 FORMAT ( '    ---- Write to disk via unit',  I4,                  M38.3799
     1         '   is now complete.  Number of cards =',  I5  )         M38.3800
      CALL WINDOW                                                               
      CLOSE (UNIT=LUNTSP, STATUS='KEEP')                                        
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 30:  "BEGIN"    $$$$  $$$$  $$$$  $$$$  $$$$M35.2494
 2926 KILL = 7733                                                               
      LLBUFF = -3333                                                            
      LSTAT(15) = 1                                                             
      INDBUF = 0                                                                
      MFLUSH = 0                                                                
      GO TO 2402                                                                
C     $$$$$$$  KEY WORD NO. 31:  "STEP"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2501
 2937 IF ( KBREAK .EQ. 0 )  GO TO 2944                                          
      WRITE (MUNIT6, 2941)                                                      
 2941 FORMAT ( '   ---  Toggle to "off"  (no more STEPping)'  )         M38.3801
      CALL WINDOW                                                               
      GO TO 2946                                                        M37.6317
 2944 WRITE (MUNIT6, 2945)                                                      
 2945 FORMAT ( '   ---  Toggle to "on"  (begin STEPping)'  )            M38.3802
      CALL WINDOW                                                               
 2946 KBREAK = KBREAK + 1                                                       
      IF ( KBREAK .GE. 2 )  KBREAK = 0                                          
      IF ( KBREAK .EQ. 1 )                                                      
     1 LOCKBR = 1                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 32:  "DEBUG"    $$$$  $$$$  $$$$  $$$$  $$$$M35.2513
 2964 WRITE (PROM80, 2967)   IPRSPY                                     M38.3803
 2967 FORMAT ( '   SEND NEW "IPRSPY" VALUE  [',  I3,  '  ] :'  )        M38.3804
      ASSIGN 2969 TO NEXTSN                                                     
      GO TO 9800                                                                
 2969 CALL FREIN1 ( BUFF77, IPRSPY )                                            
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 33:  "DATA"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2522
 2974 N12 = 1                                                                   
      N13 = 9999                                                                
      N14 = 1                                                                   
      N15 = 0                                                                   
 2975 WRITE (PROM80, 2976)                                              M35.2527
 2976 FORMAT ( '   SEND EMTP DATA FILE NAME (CONTROL) :' )              M38.3808
      ASSIGN 2979 TO NEXTSN                                                     
      GO TO 9800                                                                
 2979 ANSI32 = BUFF77(1:32)                                             M38.3811
      IF ( ANSI32(1:8) .NE. 'CONTROL ' )                                        
     1 GO TO 2984                                                               
      WRITE (PROM80, 2981)                                              M35.2535
 2981 FORMAT ( ' SEND DISK CARD NUMBERS N-beg AND N-end :'  )           M35.2536
      ASSIGN 32981 TO NEXTSN                                                    
      GO TO 9800                                                                
32981 CALL FREIN2 ( BUFF77, N12, N13 )                                          
      WRITE (PROM80, 2982)                                              M35.2540
 2982 FORMAT (  ' SEND N-beg IN EMTP, LUNIT5 OFFSET :'  )               M35.2541
      ASSIGN 2983 TO NEXTSN                                                     
      GO TO 9800                                                                
 2983 CALL FREIN2 ( BUFF77, N14, N15 )                                          
      GO TO 2975                                                                
 2984 IF ( IPRSPY .LT. 1 ) GO TO 32985                                          
      WRITE (MUNIT6, 2985)  N12, N13, N14, N15                          M37.6322
 2985 FORMAT ( ' PREPARE TO OPEN.  N12, N13, N14, N15 =', 4I8 )         M35.2548
      CALL WINDOW                                                               
32985 CLOSE (LUNT14)                                                    M37.6324
      OPEN (UNIT=LUNT14, STATUS='UNKNOWN', FILE=ANSI32)                 M38.3818
      IF ( IPRSPY .LT. 1 ) GO TO 2987                                           
      WRITE (MUNIT6, 2986)                                              M37.6326
 2986 FORMAT ( ' AFTER FILE OPEN (IT WORKED).'  )                       M36.1820
      CALL WINDOW                                                               
 2987 DO 2994  L=1, N13                                                         
      IF ( L .GE. N12 )  GO TO 2988                                             
      READ (LUNT14, 2991)  ANSI8                                                
      GO TO 2994                                                                
 2988 READ (LUNT14, 2991, END= 2998)  FILE6(N14)                        M35.2556
 2991 FORMAT ( A80 )                                                    M35.2557
      IF ( FILE6(N14)(1:4) .EQ. 'EOF ' )  GO TO 2998                    M37.6329
      N14 = N14 + 1                                                             
      IF ( N14 .LE. LIMCRD )  GO TO 2994                                        
      WRITE (MUNIT6, 2191)                                              M37.6330
 2191 FORMAT ( '   ****  WARNING.   CARD IMAGE BUFFER HAS',             M37.6331
     1         ' FILLED.  DISK READ IS TRUNCATED.'      )               M37.6332
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2192)  LIMCRD                                      M37.6334
 2192 FORMAT ( '                    STORAGE CAPACITY IN',               M37.6335
     3         ' CARDS = LIMCRD =',  I5  )                              M35.2564
      CALL WINDOW                                                               
      GO TO 2998                                                                
 2994 CONTINUE                                                                  
      WRITE (MUNIT6, 2996)  N13                                         M35.2567
 2996 FORMAT ( '    NOTE :  NO  END-OF-FILE  ENCOUNTERED.',             M35.2568
     1         '   STOP READING AFTER CARD NUMBER',  I5  )              M35.2569
      CALL WINDOW                                                               
 2998 IF ( NUMCRD .LT. N14 )                                                    
     1 NUMCRD = N14 - 1                                                         
      WRITE (MUNIT6, 2999)  NUMCRD                                      M35.2572
 2999 FORMAT (  I9,  '  =  NUMCRD  (upper bound on card-image',         M38.3820
     1               ' storage in FILE6 cache).'  )                     M38.3821
      CALL WINDOW                                                               
      IF ( L .GT. 1 )  GO TO 3006                                               
      WRITE (MUNIT6, 3004)                                              M38.3823
 3004 FORMAT ( '  ? ? ? ? ?   Warning.  Data file is empty.',           M38.3824
     1         '   Did user misspell the file name?'  )                 M38.3825
      CALL WINDOW                                                               
 3006 CLOSE ( UNIT=LUNT14 )                                                     
      NUMDCD = N15                                                              
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 34:  "RAMP"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2578
 3095 IF ( NUMRMP .LE. 0 ) GO TO 3099                                           
C     BEGINNING OF LOOP OVER RAMPS HAVING SAME BEGIN AND ENDING TIMES:  M35.2580
 3096 WRITE (PROM80, 3098)                                              M35.2581
 3098 FORMAT ( ' SEND "T-begin", "T-end" (END, SHOW, REWIND) :' )       M35.2582
      ASSIGN 33098 TO NEXTSN                                                    
      GO TO 9800                                                                
33098 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4)  .EQ.  'END ' )  GO TO 1240                              
      IF ( BYTBUF(1:4) .EQ. 'STOP' )                                            
     1 CALL STOPTP                                                              
      IF ( BYTBUF(1:6) .NE. 'REWIND' )  GO TO 3100                      M35.2589
 3099 NUMRMP = 0                                                                
      KANAL = 0                                                                 
      TMAXRP = -1.E20                                                           
      TMINRP =  1.E20                                                           
      GO TO 3096                                                                
 3100 IF ( BYTBUF(1:4)  .NE.  'SHOW' )   GO TO 3116                             
C     FOLLOWING CODE PROVIDES CONFIRMATION OF THE RAMPED DEPOSITS:      M35.2596
      WRITE (MUNIT6, 3102)                                              M37.6341
 3102 FORMAT (  ' Row  Name   Index   Num    T-begin        T-end',     M38.3831
     1          '        F-begin        F-end'     )                    M38.3832
      CALL WINDOW                                                               
      DO 3104  J=1, NUMRMP                                              M37.6344
      WRITE (MUNIT6, 3103) J, SYMBRP(J), INDXRP(J), LOOPRP(J),          M37.6345
     1             TBEGRP(J), TENDRP(J), FBEGRP(J), FENDRP(J)           M37.6346
 3103 FORMAT ( 1X, I2,  2X, A6, 2I6,  4E14.5  )                         M37.6347
      CALL WINDOW                                                               
 3104 CONTINUE                                                          M37.6349
      WRITE (MUNIT6, 3105)  TMINRP, TMAXRP                              M35.2602
 3105 FORMAT ( '         No RAMPing before',  E13.4,  '  or after',     M38.3833
     1         E13.4,  '  sec.'  )                                      M38.3834
      CALL WINDOW                                                               
      GO TO 3096                                                                
 3116 CALL FREFP2 ( BYTBUF, TIM1RP, TIM2RP )                                    
      IF ( TIM1RP  .EQ.  0.0 )                                                  
     1 TIM1RP = T                                                               
      IF ( TIM2RP  .LT.  0.0 )                                                  
     1 TIM2RP = TIM1RP + ABS ( TIM2RP )                                         
C     BEGINNING OF LOOP OVER FUNCTION VALUES (FOR GIVEN TIMES):         M35.2611
 3122 WRITE (PROM80, 3125)                                              M35.2612
 3125 FORMAT ( ' SEND "F-begin", "F-end" (END) : '  )                   M35.2613
      ASSIGN 3128 TO NEXTSN                                                     
      GO TO 9800                                                                
 3128 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 3096                             
      IF ( BYTBUF(1:4) .EQ. 'STOP' )                                            
     1 CALL STOPTP                                                              
      CALL FREFP2 ( BYTBUF, VAL1RP, VAL2RP )                                    
C     BEGINNING OF LOOP OVER VARIABLES (FOR GIVEN F VS. T RAMPS):       M35.2620
 3131 GO TO 1460                                                                
C     WE RETURN TO 3134 AFTER "DEPOSIT" LOGIC HAS FOUND VARIABLE INFO.  M35.2622
C     ACTUAL TRANSFER BACK HERE IS FROM JUST BELOW 1807.                M35.2623
 3134 N8 = LOCATE(IND) + 2*(N1-1)                                               
      N9 = ( N8 - MEMKAR ) / 2                                                  
      N10 = MEMKAR + 2*N9 - N8                                                  
      IF ( N10  .NE.  0     .AND.                                               
     1   N9 .LT. 0 )  N9 = N9 -1                                                
      NUMRMP = NUMRMP + 1                                                       
      KANAL = 1                                                                 
      IF ( NUMRMP  .LE.  20 )   GO TO 3135                                      
      WRITE (MUNIT6, 23134)                                             M36.1823
23134 FORMAT (   '  ** **  Table overflow.   Erase all',                M38.3840
     1           '  "RAMP"  entries, and start again ....' )            M38.3841
      CALL WINDOW                                                               
      GO TO 3095                                                                
 3135 MEMRMP(NUMRMP) = N9 + 1                                                   
      N10RMP(NUMRMP) = N10                                                      
      LOOPRP(NUMRMP) = N2 - N1 + 1                                              
      TBEGRP(NUMRMP) = TIM1RP                                                   
      IF ( TIM1RP .LT. TMINRP )  TMINRP = TIM1RP                                
      TENDRP(NUMRMP) = TIM2RP                                           M35.2638
      IF ( TIM2RP .GT. TMAXRP )  TMAXRP = TIM2RP                                
      FBEGRP(NUMRMP) = VAL1RP                                           M35.2640
      FENDRP(NUMRMP) = VAL2RP                                           M35.2641
      SYMBRP(NUMRMP) = SYMB(IND)                                                
      INDXRP(NUMRMP) = N1                                                       
      RAMPSL(NUMRMP) = (VAL2RP - VAL1RP) / (TIM2RP - TIM1RP)            M35.2644
      RAMPCN(NUMRMP)  =   VAL2RP  -  RAMPSL(NUMRMP) * TIM2RP            M35.2645
      WRITE (PROM80, 3136)                                              M35.2646
 3136 FORMAT (  '     [Y] CHANGE? (0=NO, 1=YES) :'  )                   M35.2647
      ASSIGN 43137 TO NEXTSN                                                    
      GO TO 9800                                                                
43137 CALL FREIN1 ( BUFF77, N8 )                                                
      KYRAMP(NUMRMP) = N8                                                       
      IF ( N8 .EQ. 0 )  GO TO 3131                                              
      TENDRP(NUMRMP) = TIM2RP + DELTAT                                          
      IF ( TMAXRP .LT. TENDRP(NUMRMP) )                                         
     1 TMAXRP = TENDRP(NUMRMP)                                                  
      IF ( N1 .GT. IBR )  GO TO 3131                                            
      WRITE (MUNIT6, 3137)  N1, IBR                                     M36.1833
 3137 FORMAT ( '  %%  %%  Cancel last "RAMP" request.',                 M38.3845
     1    '  Row number',  I4,  '  does not exceed IBR =', I4 )         M38.3846
      CALL WINDOW                                                               
      NUMRMP = NUMRMP - 1                                                       
      GO TO 3131                                                                
C     WE RETURN TO 3138 FROM BELOW 1496, IF "END" READ DURING "DEPOSIT" M35.2651
 3138 GO TO 3122                                                                
C     $$$$$$$  KEY WORD NO. 35:  "TIME"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2653
 3153 CALL TIME44 ( SPYTIM(1) )                                                 
      CALL DATE44 ( SPDATE(1) )                                                 
      WRITE (MUNIT6, 3158)  T, TMAX, DELTAT, SPYTIM, SPDATE             M35.2656
 3158 FORMAT ( 3X,  3HT =,  E13.6,  3X,  6HTMAX =,  E11.4,  3X,         M35.2657
     1      8HDELTAT =,  E11.4,  3X,  2A4,  2X,  2A4 )                  M35.2658
      CALL WINDOW                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 36:  "TEK"      $$$$  $$$$  $$$$  $$$$  $$$$M35.2660
 3174 WRITE (MUNIT6, 3177)                                              M35.2661
 3177 FORMAT ( '  >< To tamper with the ROLLing vector plot,  send',    M38.3847
     1         ' choice.'    )                                          M38.3848
      CALL WINDOW                                                               
 3178 WRITE (PROM80, 3179)                                              M38.3849
 3179 FORMAT ( '  >< OPTION (MARK, DELAY, INNER, OVERLAP,',             M38.3850
     1         ' END, HELP) : ' )                                       M38.3851
      ASSIGN 3180 TO NEXTSN                                                     
      GO TO 9800                                                                
 3180 ANSI8 = BUFF77(1:8)                                               M38.3854
      IF ( ANSI8 .NE. 'HELP    ' ) GO TO 3186                                   
      WRITE (MUNIT6, 3181)                                              M35.2670
 3181 FORMAT (  '     MARK  ---- for instantaneous marking of curves',  M38.3855
     1                         ' on Tek screen;'                   )    M38.3856
      CALL WINDOW                                                       M38.3857
      WRITE (MUNIT6, 33181)                                             M38.3858
33181 FORMAT (  '     DELAY  --- to control how simultaneous the',      M38.3859
     1                         ' ROLLing is to be;'                )    M38.3860
      CALL WINDOW                                                       M38.3861
      WRITE (MUNIT6, 43181)                                             M38.3862
43181 FORMAT (  '     INNER  ---- to  CALL TIMVAL  (the "INNER:"',      M38.3863
     1                     ' level of PLOTing);'           )            M38.3864
      CALL WINDOW                                                       M38.3865
      WRITE (MUNIT6, 53181)                                             M38.3866
53181 FORMAT (  '     OVERLAP  -- to modify the percent overlap',       M38.3867
     1                     ' for new-page plot;'           )            M38.3868
      CALL WINDOW                                                       M38.3869
      WRITE (MUNIT6, 3182)                                              M35.2681
 3182 FORMAT (  '     END   ---- for return to  "SPY:"  prompt.'   )    M35.2682
      CALL WINDOW                                                               
      GO TO 3178                                                                
 3186 IF ( ANSI8 .NE. 'MARK    ' )   GO TO 3193                         M38.3871
      WRITE (PROM80, 3189)  KSYMBL                                      M38.3872
 3189 FORMAT (  '    SEND REPEAT FREQUENCY AFTER',                      M38.3873
     1          ' IMMEDIATE 1ST ONE [',  I6,  ' ] :'  )                 M38.3874
      ASSIGN 3191 TO NEXTSN                                                     
      GO TO 9800                                                                
 3191 CALL FREIN1 ( BUFF77, KSYMBL )                                            
      GO TO 3178                                                                
 3193 IF ( ANSI8 .NE. 'DELAY   ' ) GO TO 3207                           M38.3879
      WRITE (PROM80, 3202)  KSLOWR                                      M38.3880
 3202 FORMAT  ( '  SEND NEW ROLLING FREQUENCY [', I5, ' ] :' )          M38.3881
      ASSIGN 3204 TO NEXTSN                                                     
      GO TO 9800                                                                
 3204 CALL FREIN1 ( BUFF77, KSLOWR )                                    M38.3884
      GO TO 3178                                                                
 3207 IF ( ANSI8 .NE. 'INNER   ' )  GO TO 3209                          M38.3886
 3208 MONITR = 2345                                                             
      CALL TIMVAL                                                               
      MONITR = 1                                                                
      IF ( NEXMOD .EQ. 0 ) GO TO 3178                                           
      NEXMOD = 1                                                                
      GO TO 9803                                                                
 3209 IF ( ANSI8 .NE. 'OVERLAP ' )  GO TO 3214                          M38.3893
      WRITE (PROM80, 3211)  INCHLP                                      M38.3894
 3211 FORMAT ( '    SEND TIME-OVERLAP PERCENTAGE [',  I3,  ' ] :'  )    M38.3895
      ASSIGN 3204 TO NEXTSN                                                     
      GO TO 9800                                                                
 3212 CALL FREIN1 ( BUFF77, INCHLP )                                    M38.3898
      GO TO 3178                                                                
 3214 IF ( ANSI8 .EQ. 'END     ' )  GO TO 1240                                  
      WRITE (MUNIT6, 3227)                                              M35.2714
 3227 FORMAT ( '   Sorry,  SPY  does not understand.   Come again ...' )M35.2715
      CALL WINDOW                                                               
      GO TO 3178                                                                
C     $$$$$$$  KEY WORD NO. 37:  "BRANCH"   $$$$  $$$$  $$$$  $$$$  $$$$M35.2717
 3256 BYTBUF(1:4) = ANSW80(1:4)                                                 
      N33 = 0                                                                   
      N24 = 0                                                                   
      GO TO 3308                                                                
 3306 ASSIGN 3307 TO NEXTSN                                                     
      GO TO 9800                                                                
 3307 BYTBUF = BUFF77(1:20)                                                     
 3308 IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .EQ.  'STOP' )   GO TO 1240                             
      IF ( BYTBUF(1:6) .NE. 'SERIES' )  GO TO 3309                      M35.2729
      N24 = 1                                                                   
      GO TO 3306                                                                
 3309 IF ( BYTBUF(1:4) .NE. 'EXTR' ) GO TO 3317                                 
      N33 = N33 + 1                                                             
      IF ( N33 .GE. 2 )  N33 = 0                                                
 3310 IF ( N33 .EQ. 0 )  GO TO 3314                                             
      WRITE (PROM80, 3311)                                                      
 3311 FORMAT ( 4H Row, 13X, 2HCI,  13X, 2HCK,  12X, 3HCIK,              M38.3912
     1         12X, 3HCIK,  10X, 5HCKKJM,  1H:  )                       M38.3913
      GO TO 3306                                                                
 3314 WRITE (PROM80, 3315)                                                      
 3315 FORMAT ( ' Row  name  name-K name-M KBUS MBUS LGTH  NR',          M42.6771
     1         ' KODEBR KODSEM LITYPE IMODEL INDHST:'  )                M35.2742
      GO TO 3306                                                                
 3317 IF ( BYTBUF(1:4) .EQ. SPYKWD(37)(1:4)  .OR.                               
     1     BYTBUF(1:4) .EQ. SPYKWD(1)(1:4) )                                    
     2  GO TO 3310                                                              
C     SINCE NOT KEY-WORD RESPONSE, WE EXTRACT (N1,N2) INTEGER           M38.3919
C     PAIR (FREE-FORMAT); "IBR" IS MAXIMUM; "N17" IS ERROR FLAG:        M38.3920
 3322 CALL INTPAR ( IBR, N1, N2, N17 )                                          
      IF ( N17  .GT.  0 )   GO TO 3306                                          
      N27 = 1                                                                   
      I = N1                                                                    
 3323 CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 3325                                           
      KWTSPY = 0                                                                
      GO TO 3306                                                                
 3325 N5 = IABS ( KBUS(I) )                                                     
      N6 = IABS ( MBUS(I)  )                                                    
      BUS1 = BUS(N5)                                                    M37.6372
      IF ( N5 .EQ. 1 )  BUS1 = TERRA                                    M37.6373
      BUS2 = BUS(N6)                                                    M37.6374
      IF ( N6 .EQ. 1 )  BUS2 = TERRA                                    M37.6375
      IF ( N24 .EQ. 0 )  GO TO 3327                                             
      N27 = LENGTH(I)                                                           
      IF ( N5 .LT. 0 )  GO TO 3334                                              
      IF ( KODEBR(I) .GT. 0 )  GO TO 3334                                       
      IF ( N27 .NE. 1 )  GO TO 3334                                             
      IF ( NR(I) .GE. 0 )  GO TO 3334                                           
 3327 IF ( N33 .NE. 0 )  GO TO 3329                                             
      N7 = NAMEBR(I)                                                            
      WRITE (MUNIT6, 3328) I, TEXVEC(N7), BUS1, BUS2, KBUS(I), MBUS(I), M42.6773
     1                 LENGTH(I), NR(I), KODEBR(I), KODSEM(I),          M35.2769
     2                 LITYPE(I), IMODEL(I), INDHST(I)                  M35.2770
 3328 FORMAT ( 1X, I3, 1X, A6, 1X, A6, 1X, A6,  3I5,  I4,  5I7 )        M35.2771
      CALL WINDOW                                                               
 3329 IF ( N33 .NE. 1 )  GO TO 3334                                             
      WRITE (MUNIT6, 3331) I, CI(I), CK(I), CIK(I), CKI(I), CKKJM(I)    M38.3926
 3331 FORMAT ( 1X, I3,  5E15.6  )                                       M38.3927
      CALL WINDOW                                                               
 3334 I = I + N27                                                               
      IF ( I .LE. N2 )  GO TO 3323                                              
      PROM80 = BLAN80                                                           
      GO TO 3306                                                                
C     $$$$$$$  KEY WORD NO. 38:  "YFORM"    $$$$  $$$$  $$$$  $$$$  $$$$M35.2780
 3357 KANSAV = KANAL                                                            
      KANAL = 2                                                                 
      GO TO 3394                                                                
C     $$$$$$$  KEY WORD NO. 39:  "NOY"      $$$$  $$$$  $$$$  $$$$  $$$$M35.2784
 3381 KANAL = KANSAV                                                            
      GO TO 3406                                                                
C     $$$$$$$  KEY WORD NO. 40:  "FACTOR"   $$$$  $$$$  $$$$  $$$$  $$$$M35.2787
 3394 IALTER = 2                                                                
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 41:  "NOF"      $$$$  $$$$  $$$$  $$$$  $$$$M35.2790
 3406 IF ( IALTER .EQ. 2 )                                                      
     1 IALTER = 1                                                               
      GO TO 1240                                                                
C     $$$$$$$  SERVICE 7:SWITCH, 25:SOURCE, 26:EDIT, 28:LANGUAGE,       M36.1851
C              42:RLC, 43:WIDTH, 44:BUS, 45:SIZE, 46:LIMIT,             M36.1852
C              47:IOUT, 48:NODE, 49:NONLIN, 50:SPACE, 51:LUNIT4,        M36.1853
C              52:SERIES, 53:LOCK, 54:[Y], 55:[F], 56:NOROLL            M36.1854
 8500 CALL SPYINK                                                               
      IF ( NEXMOD .NE. 0 )                                                      
     1 GO TO 9803                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 57:  "OPEN"     $$$$  $$$$  $$$$  $$$$  $$$$M36.1857
 3568 WRITE (PROM80, 3571)                                              M36.1858
 3571 FORMAT ( '    NEXT UNIT, NAME (I2, A32 ) :'  )                    M36.1859
      ASSIGN 3573 TO NEXTSN                                                     
      GO TO 9800                                                                
 3573 READ (BUFF77, 3576)  N7, ANSI32                                           
 3576 FORMAT ( I2, A32 )                                                M36.1862
      IF ( N7 .LE. 0 )  GO TO 1240                                              
      OPEN (UNIT=N7, STATUS='UNKNOWN', FILE=ANSI32)                     M37.6377
      GO TO 3568                                                                
C     $$$$$$$  KEY WORD NO. 58:  "CLOSE"    $$$$  $$$$  $$$$  $$$$  $$$$M36.1866
 3584 WRITE (PROM80, 3587)                                              M36.1867
 3587 FORMAT ( '    NEXT UNIT, STATUS (I2, A32) :' )                    M36.1868
      ASSIGN 3591 TO NEXTSN                                                     
      GO TO 9800                                                                
 3591 READ (BUFF77, 3576)  N7, ANSI32                                           
      IF ( N7 .LE. 0 )  GO TO 1240                                              
      IF ( ANSI32(1:6) .EQ. 'KEEP  ' ) GO TO 3596                               
      IF ( ANSI32(1:6) .EQ. 'DELETE' ) GO TO 3596                               
      CLOSE ( UNIT=N7 )                                                         
      GO TO 3584                                                                
 3596 CLOSE ( UNIT=N7, STATUS=ANSI32 )                                  M38.3942
      GO TO 3584                                                                
C     $$$$$$$  KEY WORD NO. 59:  "SM"       $$$$  $$$$  $$$$  $$$$  $$$$M38.3944
 3606 IF ( NCHAIN .GE. 16   .AND.                                               
     1     NST .GT. 0 ) GO TO 43606                                             
      WRITE (MUNIT6, 3607)                                              M38.3947
 3607 FORMAT ( '   ?? ??  No S.M. data present or not yet',             M38.3948
     1         ' in time-step loop.'  )                                 M38.3949
      CALL WINDOW                                                               
      GO TO 1240                                                                
43606 KSMSPY(3) = 1                                                             
      LOCKBR = 0                                                                
 3608 KSMSPY(1) = -1                                                            
      KSMSPY(2) = 0                                                             
      MAXFLG = 1                                                                
      ASSIGN 3609 TO NEXTSN                                                     
      PROM80(1:8) = '        '                                                  
      GO TO 9800                                                                
 3609 IF ( IPRSPY .LT. 1 ) GO TO 83609                                          
      WRITE (MUNIT6, 23609)  KSMSPY(2)                                  M38.3961
23609 FORMAT ( ' AT S.N. 3609 OF "SPYING".  KSMSPY(2) =', I4 )          M38.3962
      CALL WINDOW                                                               
83609 IF ( KSMSPY(2) .EQ. 0 )                                                   
     1 GO TO 3608                                                               
      KSMSPY(3) = 0                                                             
      WRITE (PROM80, 43609)  KSMSPY(2)                                  M38.3967
43609 FORMAT ( '  S.M.', I3,  '  WANTED (Y OR N) ? :'  )                M38.3968
      KSMSPY(1) = +1                                                            
      ASSIGN 3610 TO NEXTSN                                                     
      GO TO 9800                                                                
 3610 IF ( BUFF77(1:1) .EQ. 'N' )  GO TO 43606                                  
      KSMSPY(1) = +1                                                            
 3611 WRITE (PROM80, 3612)                                              M38.3974
 3612 FORMAT ( '     SEND S.M. CLASS (ELEC, MECH, ELVA, MEVA,',         M38.3975
     1         ' ALL, END) :'  )                                        M38.3976
      ASSIGN 3613 TO NEXTSN                                             M38.3977
      GO TO 9800                                                                
 3613 N14 = KSMSPY(2)                                                           
      IF ( BUFF77(1:4) .EQ. 'END' )  GO TO 43606                                
      IF ( BUFF77(1:4) .NE. 'ELEC'   .AND.                              M38.3981
     1     BUFF77(1:4) .NE. 'ALL ' )  GO TO 43615                       M38.3982
      K2 = ( N14-1 ) * 101                                                      
      WRITE (MUNIT6, 33614)                                             M38.3984
33614 FORMAT ( '             LD            LAF             LF',         M39.3172
     1 '           LAKD           LFKD' )                               M39.3173
      CALL WINDOW                                                               
      XL = ELP( K2+20 )                                                         
      D3 = ELP( K2+1 ) + XL                                                     
      D4 = ELP( K2+2 )                                                  M39.3176
      D5 = ( ELP( K2+3 ) + ELP( K2+62 ) ) / FACTOM                              
      D6 = ELP( K2+4 )                                                  M39.3178
      D8 = ELP( K2+5 )                                                  M39.3179
      WRITE (MUNIT6, 53614)   D3, D4, D5, D6, D8                        M39.3180
53614 FORMAT( 5E15.7 )                                                  M38.3991
      CALL WINDOW                                                               
      WRITE ( MUNIT6, 63614 )                                           M38.3993
63614 FORMAT ( '            LKD             RF            RKD',         M39.3181
     1 '             LQ            LAG' )                               M39.3182
      CALL WINDOW                                                               
      D3 = ( ELP( K2+6 ) + ELP( K2+63 ) ) / FACTOM                              
      D4 = ELP( K2+7 )                                                  M39.3184
      D5 = ELP( K2+8 )                                                  M39.3185
      D6 = ELP( K2+9 ) + XL                                                     
      D8 = ELP( K2+10 )                                                 M39.3187
      WRITE (MUNIT6, 53614)  D3, D4, D5, D6, D8                         M39.3188
      CALL WINDOW                                                               
      WRITE (MUNIT6, 73614)                                             M38.4001
73614 FORMAT ( '             LG           LAKQ           LGKQ',         M39.3189
     1 '            LKQ             RG' )                               M39.3190
      CALL WINDOW                                                               
      D3 = ( ELP( K2+11 ) + ELP( K2+64 ) ) / FACTOM                             
      D4 = ELP( K2+12 )                                                 M39.3192
      D5 = ELP( K2+13 )                                                 M39.3193
      D6 = ( ELP( K2+14 ) + ELP( K2+65 ) ) / FACTOM                             
      D8 = ELP( K2+15 )                                                 M39.3195
      WRITE (MUNIT6, 53614)  D3, D4, D5, D6, D8                         M39.3196
      CALL WINDOW                                                               
      WRITE( MUNIT6, 83614 )                                            M38.4009
83614 FORMAT ( '            RKQ             L0             R0',         M39.3197
     1 '         AGLINE             RA' )                               M38.4011
      CALL WINDOW                                                               
      D3 = ELP( K2+16 )                                                 M39.3198
      D4 = 1.0 / ELP( K2+17 )                                           M39.3199
      D5 = ELP( K2+18 )                                                 M39.3200
      D5 = ( D4 -D5 ) / ( 1.0 + DAMRAT )                                        
      D4 = ( D4 - D5 ) / FACTOM                                                 
      D8 = ELP( K2+20 )                                                 M39.3203
      WRITE (MUNIT6, 53614)  D3, D4, D5, XL, D8                         M39.3204
      CALL WINDOW                                                               
      WRITE (MUNIT6, 23615)                                             M38.4017
23615 FORMAT ( '           RAT1         DSAT10         DSAT12',         M38.4018
     1 '         QSAT10         QSAT12' )                               M38.4019
      CALL WINDOW                                                               
      K3 = K2 + 21                                                      M39.3205
      K2 = K2 + 25                                                      M39.3206
      WRITE (MUNIT6, 53614)  ( ELP( K1 ), K1 = K3, K2 )                 M38.4023
      CALL WINDOW                                                               
43615 IF ( BUFF77(1:4) .NE. 'MECH'   .AND.                              M38.4025
     1     BUFF77(1:4) .NE. 'ALL ' )  GO TO 13617                       M39.3207
      K2 = ( N14-1 ) * 101 + 26                                                 
      K3 = ( N14-1 ) * 30  + 12                                                 
      NUMASK = ISMDAT( K3 )                                                     
      WRITE ( MUNIT6, 53615 )                                           M38.4030
53615 FORMAT ( '  NUMASK   NLOCG   NLOCE     CNP' )                     M38.4031
      CALL WINDOW                                                               
      WRITE (MUNIT6, 63615)  NUMASK, ISMDAT(K3+1), ISMDAT(K3+2),        M38.4033
     1                       ELP(K2)                                    M38.4034
63615 FORMAT ( 3I8, F8.1 )                                              M38.4035
      CALL WINDOW                                                               
      WRITE ( MUNIT6, 83615 )                                           M38.4037
83615 FORMAT ( '           HICO            DSM            HSP',         M38.4038
     1 '            DSR' )                                              M38.4039
      CALL WINDOW                                                               
      K2 = 0                                                            M39.3208
      KP = 0                                                            M38.4042
      DO 3616 K1 = 1, N14                                               M38.4043
      K2 = K2 + ISMDAT( KP+12 )                                                 
 3616 KP = KP + 30                                                      M39.3210
      NUM2 = NUMASK + NUMASK                                            M38.4046
      NUM3 = NUMASK + NUM2                                              M38.4047
      NUM5 = NUM3 + NUM2                                                M38.4048
      K2 = ( K2 - NUMASK ) * 12                                         M39.3211
      K2 = K2 + NUM2                                                            
      DO 43616 K1 = 1, NUMASK                                                   
      K2 = K2 + 1                                                       M38.4051
      WRITE (MUNIT6, 53614)  SHP( K2+NUMASK ), SHP( K2+NUM2 ),          M38.4052
     1 SHP( K2+NUM3 ), SHP( K2+NUM5 )                                   M38.4053
      CALL WINDOW                                                               
43616 CONTINUE                                                          M38.4055
13617 IF ( BUFF77(1:4) .NE. 'MEVA'   .AND.                              M39.3213
     1     BUFF77(1:4) .NE. 'ALL ' )  GO TO 13618                       M39.3214
      WRITE( MUNIT6, 23617 )                                            M39.3215
23617 FORMAT( ' MECHANICAL ANGLES IN UNITS OF RADIANS ' )               M39.3216
      CALL WINDOW                                                               
      K3 = ( N14-1 ) * 30 + 12                                                  
      NUMASK = ISMDAT( K3 )                                             M39.3219
      K2 = 0                                                            M39.3220
      KP = 0                                                            M39.3221
      DO 3617 K1 = 1, N14                                                       
      K2 = K2 + ISMDAT( KP+12 )                                         M39.3223
 3617 KP = KP + 30                                                              
      K2 = ( K2 - NUMASK ) * 6                                          M39.3225
      K3 = K2 + 1                                                       M39.3226
      K2 = K2 + NUMASK                                                  M39.3227
      WRITE (MUNIT6, 53614)  ( HISTQ( K1 ), K1 = K3, K2 )               M39.3228
      CALL WINDOW                                                               
      K3 = K2 + 1                                                       M39.3230
      K2 = K2 + NUMASK                                                  M39.3231
      WRITE( MUNIT6, 33617 )                                            M39.3232
33617 FORMAT( ' MECHANICAL SPEEDS IN UNITS OF  RADIANS/SEC ' )          M39.3233
      CALL WINDOW                                                               
      WRITE (MUNIT6, 53614)  ( HISTQ( K1 ), K1 = K3, K2 )               M39.3235
      CALL WINDOW                                                               
13618 IF ( BUFF77(1:4) .NE. 'ELVA'  .AND.                               M39.3237
     1     BUFF77(1:4) .NE. 'ALL ' )  GO TO 3611                        M39.3238
      WRITE( MUNIT6, 23618 )                                            M39.3239
23618 FORMAT( '             ID             IQ             I0',          M39.3240
     1 '             IF            IKD' )                               M39.3241
      CALL WINDOW                                                               
      K2 = ( N14-1 ) * 24                                                       
      K3 = K2 + 1                                                       M39.3244
      K2 = K2 + 5                                                       M39.3245
      WRITE (MUNIT6, 53614)  ( CU( K1 ), K1 = K3, K2 )                  M39.3246
      CALL WINDOW                                                               
      WRITE( MUNIT6, 33618 )                                            M39.3248
33618 FORMAT( '             IG            IKQ             IA',          M39.3249
     1 '             IB             IC' )                               M39.3250
      CALL WINDOW                                                               
      K3 = K2 + 1                                                       M39.3252
      K2 = K2 + 2                                                       M39.3253
      WRITE (MUNIT6, 53614)  CU( K3 ), CU( K2 ), ( SMOUTV( K1 ),        M39.3254
     1 K1 = 1, 3 )                                                      M39.3255
      CALL WINDOW                                                               
      WRITE( MUNIT6, 43618 )                                            M39.3257
43618 FORMAT( '             VD             VQ             V0',          M39.3258
     1 '             VF             VA' )                               M39.3259
      CALL WINDOW                                                               
      WRITE (MUNIT6, 53614)  ( SMOUTV( K1 ), K1 = 4, 8 )                M39.3261
      CALL WINDOW                                                               
      WRITE( MUNIT6, 53618 )                                            M39.3263
53618 FORMAT( '             VB             VC            TEQ',          M39.3264
     1 '           TEXC' )                                              M39.3265
      CALL WINDOW                                                               
      WRITE (MUNIT6, 53614)  ( SMOUTV( K1 ), K1 = 9, 12 )               M39.3267
      CALL WINDOW                                                               
 3619 CONTINUE                                                          M38.4057
      GO TO 3611                                                                
C     $$$$$$$  KEY WORD NO. 60:  "HONK"     $$$$  $$$$  $$$$  $$$$  $$$$M36.1877
 3623 WRITE (PROM80, 3626)                                              M36.1878
 3626 FORMAT ( '   SEND SEVERITY LEVEL OF ALERT (1 TO 10) :' )          M38.4059
      ASSIGN 3629 TO NEXTSN                                                     
      GO TO 9800                                                                
 3629 CALL FREIN1 ( BUFF77, N24 )                                               
      CALL HONKER ( N24 )                                                       
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 63:  "WAIT"     $$$$  $$$$  $$$$  $$$$  $$$$M37.6379
 3644 WRITE (PROM80, 3647)                                              M37.6380
 3647 FORMAT ( '     SEND DESIRED HIBERNATION TIME IN SECONDS :' )      M37.6381
      ASSIGN 3649 TO NEXTSN                                                     
      GO TO 9800                                                                
 3649 CALL FREFP1 ( BUFF77, D13 )                                               
      CALL TDELAY ( D13 )                                                       
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 12:  "ECHO"     $$$$  $$$$  $$$$  $$$$  $$$$M38.4066
 3673 WRITE (PROM80, 3676)                                              M38.4067
 3676 FORMAT ( '    SEND DESIRED OPERATION (BEGIN, FILE, SHOW) :' )     M38.4068
      ASSIGN 3681 TO NEXTSN                                                     
      GO TO 9800                                                                
 3681 IF ( BUFF77(1:6) .NE. 'BEGIN ' )  GO TO 3686                      M38.4071
      KSPSAV = LIMCRD + 1                                                       
      N23 = LIMCRD - NUMCRD                                                     
      WRITE (MUNIT6, 3683)  N23                                         M38.4074
 3683 FORMAT ( '         Ok, up to a maximum of',  I7,                  M38.4075
     1     '   commands can be accumulated, if no more data.' )         M38.4076
      CALL WINDOW                                                               
      GO TO 1240                                                                
 3686 IF ( BUFF77(1:5) .NE. 'FILE ' )  GO TO 3702                       M38.4079
      WRITE (*, *)  ' TO BE COMPLETED LATER.  ?????????'                M38.4080
      GO TO 1240                                                                
 3702 IF ( BUFF77(1:5) .NE. 'SHOW ' )  GO TO 1303                               
      WRITE (*, *)  ' TO BE COMPLETED LATER.  ??????'                   M38.4083
      GO TO 3673                                                                
C     $$$$$$$  KEY WORD NO.  2:  "STOP"     $$$$  $$$$  $$$$  $$$$  $$$$M35.2794
 9000 CALL TIME44 ( SPYTIM(1) )                                                 
      CALL DATE44 ( SPDATE(1) )                                                 
      WRITE (MUNIT6, 3158)  T, TMAX, DELTAT, SPYTIM, SPDATE             M37.6389
      CALL WINDOW                                                               
      CALL STOPTP                                                               
 9800 NEXMOD = 2                                                                
 9803 IF ( IPRSPY .LT. 1 ) GO TO 9811                                           
      WRITE (MUNIT6, 9807) JJROLL, KBREAK, LOCKBR, NCHAIN,              M38.4087
     1                     PROM80(1:20)                                 M38.4088
 9807 FORMAT (  ' EXIT "SPYING".  JJROLL, KBREAK, LOCKBR,',             M38.4089
     1      ' NCHAIN =',  4I5,  '   PROM80(1:20) =',  A20  )            M38.4090
      CALL WINDOW                                                               
 9811 IF ( PROM80(1:8) .EQ. '        ' )  GO TO 9833                    M38.4091
      IF ( KFILE5 .EQ. 1 ) GO TO 9833                                           
      CALL PROMPT                                                               
 9833 RETURN                                                            M38.4094
      END                                                               M35.2800
      SUBROUTINE SPYINK                                                 M36.1886
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M36.1887
C     THIS IS THE 2ND HALF OF PRINCIPLE MODULE "SPYING".                M36.1888
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      DIMENSION     MMHOLD(20)                                          M36.1893
      REAL*8          TEXT1                                             M36.1894
      SAVE                                                              M36.1895
      DATA  TEXT1   /  6HTAMPER  /                                      M36.1896
      IF ( IPRSPY .LT. 1 ) GO TO 1003                                           
      WRITE (MUNIT6, 1002) NCHAIN, JJROLL, KBREAK, LOCKBR, NEXMOD       M38.4096
 1002 FORMAT ( ' ENTER "SPYING".  NCHAIN  JJROLL  KBREAK',              M38.4097
     1         '  LOCKBR, NEXMOD =',  5I5 )                             M38.4098
      CALL WINDOW                                                               
 1003 IF ( NEXMOD .NE. 3 ) GO TO 1004                                           
      NEXMOD = 0                                                                
      GO TO NEXTSN                                                              
 1004 IF ( JWORD .GT. 63 )     GO TO 1007                                       
      GO TO (                                                           M36.1897
C       HEADING  STOP    PLOT    HELP  EXAMINE  DEPOSIT  SWITCH  (1-7)  M36.1898
     1   9999,   9999,   9999,   9999,   9999,   9999,   2804,          M36.1899
C        REST    SAVE   RESTORE   GO    BLANK    FIND    LIST    (8-14) M36.1900
     2   9999,   9999,   9999,   9999,   9999,   9999,   9999,          M36.1901
C         SPY    BREAK   WHEN  COMMENT    @?     ROLL   TYPE?   (15-21) M36.1902
     3   9999,   9999,   9999,   9999,   9999,   9999,   9999,          M36.1903
C       VERIFY   FILES   SLEEP  SOURCE   EDIT    WAKE  LANGUAGE (22-28) M36.1904
     4   9999,   9999,   9999,   2856,   2617,   9999,   2782,          M38.4104
C      CATALOG  BEGIN    STEP    DEBUG   DATA    RAMP    TIME   (29-35) M36.1906
     5   9999,   9999,   9999,   9999,   9999,   9999,   9999,          M36.1907
C         TEK   BRANCH   YFORM    NOY   FACTOR    NOF    RLC    (36-42) M36.1908
     6   9999,   9999,   9999,   9999,   9999,   9999,   3456,          M36.1909
C        WIDTH    BUS    SIZE   LIMIT    IOUT    NODE   NONLIN  (43-49) M36.1910
     7   3526,   3541,   3635,   3647,   3662,   3684,   3752,          M36.1911
C        SPACE  LUNIT4  SERIES   LOCK     [Y]     [F]   NOROLL  (50-56) M36.1912
     8   3842,   4002,   4163,   4436,   4471,   4512,   4563,          M38.4105
C        OPEN    CLOSE  UNLOCK   HONK   CHOICE   TACS    WAIT   (57-63) M37.6400
     9   9999,   9999,   9999,   9999,   4592,   4716,   9999), JWORD   M37.6401
      CALL STOPTP                                                               
 1007 N14 = JWORD - 63                                                          
      GO TO (                                                           M37.6404
C         V-I                                                   (64-64) M37.6405
     1   4823 ),  N14                                                   M37.6406
      CALL STOPTP                                                               
C     $$$$$$$  KEY WORD NO. 26:  "EDIT"     $$$$  $$$$  $$$$  $$$$  $$$$M36.1916
C     "EDIT"  COMMAND ALLOWS THE USER TO LOOK AT THE EMTP DATA CASE     M36.1917
C     WHICH IS STORED IN THE RTM DATA BASE; DISPLAY COMMANDS ARE        M36.1918
C     SIMILAR TO  VAX/VMS  EDITOR  "SOS" :                              M36.1919
 2617 LINNOW = 0                                                                
 2618 WRITE (PROM80, 2621)                                                      
 2621 FORMAT (  ' *'  )                                                 M36.1921
      ASSIGN 2620 TO NEXTSN                                                     
      GO TO 9800                                                                
 2620 READ (BUFF77, 2623)  CHAR1, BYTBUF                                        
 2623 FORMAT ( A1, A20 )                                                M36.1924
      IF ( IPRSPY .LT. 1 )  GO TO 2626                                          
      WRITE (MUNIT6, 2622)  CHAR1,  BYTBUF(IP:IP+9)                     M37.6409
 2622 FORMAT ( ' CHAR1, BYTBUF(1:10) AFTER READ:', A2, 1X, A10 )        M36.1927
      CALL WINDOW                                                               
 2626 IF ( CHAR1  .EQ.  'F' )   GO TO 2669                                      
      IF ( CHAR1  .EQ.  'S'  )   GO TO 2669                                     
      IF ( CHAR1  .EQ.  'E' )    GO TO 1240                                     
      IF ( CHAR1  .EQ.  '8' )   GO TO 2734                                      
      IF ( CHAR1  .EQ.  'C'    .AND.                                            
     1   BYTBUF(1:1)  .EQ.  'O'  .AND.                                          
     2   BYTBUF(2:2)  .EQ.  'L' )   GO TO 2756                                  
      IF ( CHAR1  .EQ.  'P' )   GO TO 2629                                      
      IF ( CHAR1  .EQ.  'D' )   GO TO 2629                                      
      IF ( CHAR1  .EQ.  'I' )   GO TO 2629                                      
      IF ( CHAR1  .EQ.  'R' )   GO TO 2629                                      
      IF ( CHAR1  .EQ.  ' '  )   GO TO 2629                                     
      WRITE (MUNIT6, 42622)                                             M36.1940
42622 FORMAT (  '     Sorry, 1st character is meaningless.',            M36.1941
     1                  '   Try again .... '     )                      M36.1942
      CALL WINDOW                                                               
      GO TO 2618                                                                
 2629 CALL SOSRNG ( N17 )                                                       
      IF ( IPRSPY .LT. 2 )  GO TO 2633                                          
      WRITE (MUNIT6, 2632)  N17, LIDNT1, LIDNT2                         M37.6414
 2632 FORMAT ( ' N17, LIDNT1, LIDNT2 =',  3I8 )                         M37.6415
      CALL WINDOW                                                               
 2633 IF ( N17  .GT.  0 )   GO TO 2618                                          
      IF ( CHAR1  .EQ.  'I' )  GO TO 2695                                       
      IF ( CHAR1  .EQ.  'D' )   GO TO 2719                                      
      IF ( CHAR1  .EQ.  'R' )   GO TO 2719                                      
C     WE REACH HERE READY TO PRINT LINES  (LIDNT1, LIDNT2) :  *P  CASE  M36.1952
 2634 DO 2665  J=LIDNT1, LIDNT2                                                 
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 2639                                           
      KWTSPY = 0                                                                
      LINNOW = J - 1                                                            
      LIDNT1 = J - 1                                                            
      LIDNT2 = J - 1                                                            
      GO TO 2618                                                                
 2639 WRITE (MUNIT6, 2667)  J, FILE6(J)                                         
 2667 FORMAT ( 1X, I4,  1X,  A80 )                                      M36.1962
      CALL WINDOW                                                               
 2665 CONTINUE                                                                  
      LINSPN = LIDNT2 - LIDNT1                                                  
      LINNOW = LIDNT2                                                           
      GO TO 2618                                                                
C     FOLLOWING CODE IS FOR  "*F[string]@"  LOCATE REQUEST (LIKE SOS):  M36.1966
 2669 N4 = 0                                                                    
      N5 = 0                                                                    
      DO 2670  I=2, 20                                                          
      IF ( BYTBUF(I:I) .EQ.  '@' )   N4 = 1                                     
      IF ( BYTBUF(I:I) .NE.  ' ' )   N5 = 1                                     
 2670 CONTINUE                                                          M36.1972
      IF ( N5  .EQ.  0 )   GO TO 2671                                           
      IF ( N4  .EQ.  1 )   GO TO 2673                                           
      WRITE (MUNIT6, 32670)                                             M36.1975
32670 FORMAT (                                                          M36.1976
     1  '  Illegal "*F" --- string not terminated by "@".' )            M36.1977
      CALL WINDOW                                                               
      GO TO 2618                                                                
 2671 IF ( MSTRNG  .EQ.  1 ) GO TO 2672                                         
      WRITE (MUNIT6, 22671)                                             M36.1980
22671 FORMAT (  '  Illegal "*F" --- blank string, but no',              M36.1981
     1          ' earlier usage for reference.'  )                      M36.1982
      CALL WINDOW                                                               
      GO TO 2618                                                                
C2672 CALL MOVERS ( BYTFND(1), BYTBUF(1), 20 )    ! RESTORE OLD STRING  M36.1984
 2672 BYTBUF = BYTFND                                                   M36.1985
      LINNOW = LINNOW + 1                                                       
 2673 N6 = LINNOW                                                               
      DO 2683  J=N6, NUMCRD                                                     
      L = 1                                                                     
      DO 2677  K=1, 80                                                          
      IF ( FILE6(J)(K:K) .EQ. BYTBUF(L:L) )   GO TO 2674                        
      L = 1                                                                     
      GO TO 2677                                                                
 2674 IF ( BYTBUF(L+1:L+1) .NE. '@' )  GO TO 2676                               
      LINNOW = J                                                                
      LIDNT2 = J                                                                
      IF ( CHAR1 .EQ. 'S' )  GO TO 2763                                         
      GO TO 2691                                                                
 2676 L = L + 1                                                                 
 2677 CONTINUE                                                                  
 2683 CONTINUE                                                                  
      L = 0                                                                     
      WRITE (MUNIT6, 2686)                                              M36.2003
 2686 FORMAT (  ' String not found, search failed'  )                   M36.2004
      CALL WINDOW                                                               
      LINNOW = LINNOW - 1                                                       
 2691 IF ( L .LE. 0 )  GO TO 2693                                               
      WRITE (MUNIT6, 2692)  J,  FILE6(J)                                        
 2692 FORMAT ( I5, 1X,  A80 )                                           M36.2008
      CALL WINDOW                                                               
 2693 BYTFND = BYTBUF                                                   M37.6428
      MSTRNG = 1                                                                
      GO TO 2618                                                                
C     FOLLOWING CODE IS TO PROCESS  "*I"  COMMAND:                      M36.2012
C     FOLLOWING CODE IS FOR  "*I"  REQUEST :                            M36.2013
 2695 N1 = 0                                                                    
      LINNOW = LIDNT1                                                           
 2701 IF ( N1  .LT. 20 )   GO TO 2703                                           
      WRITE (MUNIT6, 52701)                                             M36.2017
52701 FORMAT ( '    Filled working storage of 20 lines.',               M36.2018
     1         '   Truncated insertion now in progress.'  )             M36.2019
      CALL WINDOW                                                               
      GO TO 2704                                                        M36.2020
 2703 LINNOW = LINNOW + 1                                                       
      WRITE (PROM80, 2702)  LINNOW                                              
 2702 FORMAT ( 1X, I4,  1H   )                                          M36.2023
      ASSIGN 52703 TO NEXTSN                                                    
      GO TO 9800                                                                
52703 IF ( BUFF77(1:1) .NE. '@'  )  GO TO 2711                                  
 2704 J = NUMCRD                                                                
 2706 FILE6(J+N1) = FILE6(J)                                                    
      J = J - 1                                                                 
      IF ( J .GT. LIDNT1 )  GO TO 2706                                          
      DO 2708  J=1, N1                                                          
 2708 FILE6(LINNOW+J) = FILE6B(J)                                               
      LINNOW = LINNOW - 1                                                       
      LIDNT1 = LINNOW                                                           
      LIDNT2 = LINNOW                                                           
      NUMCRD = NUMCRD + N1/10                                                   
      GO TO 2618                                                                
 2711 N1 = N1 + 1                                                               
      FILE6B(N1) = BUFF77                                               M36.2040
      GO TO 2701                                                                
C     FOLLOWING CODE IS FOR  "*D"  OR  "*R"  REQUEST:                   M36.2042
 2719 N2 = LIDNT1                                                               
      J  = LIDNT2 + 1                                                           
 2723 FILE6(N2) = FILE6(J)                                                      
      N2 = N2 + 1                                                               
      J = J + 1                                                                 
      IF ( J .LE. NUMCRD )  GO TO 2723                                          
      NUMCRD = NUMCRD - ( LIDNT2 - LIDNT1 + 1 )                                 
      LINNOW = LIDNT1 - 1                                                       
      LIDNT1 = LINNOW                                                           
      LIDNT2 = LINNOW                                                           
      IF ( CHAR1 .NE. 'R' )  GO TO 2618                                         
      GO TO 2695                                                                
C     FOLLOWING CODE IS FOR  "*8"  REQUEST :                            M36.2055
 2734 IF ( BYTBUF(1:4)  .NE.  '    ' )  GO TO 2743                              
      J = LINNOW                                                                
 2736 IF ( FILE6(J)(80:80) .EQ.  ' ' )  GO TO 2738                              
      WRITE (MUNIT6, 2692)  J,  FILE6(J)                                        
      CALL WINDOW                                                               
      READ (MUNIT5, 2623)  BYTBUF                                               
      IF ( BYTBUF(1:1) .EQ. 'E' )  GO TO 2618                                   
      IF ( BYTBUF(1:1) .EQ. ' ' )   GO TO 2738                                  
      BYTBUF(2:2) = FILE6(J)(80:80)                                             
      IF ( BYTBUF(1:1) .EQ. '0' ) FILE6B(J)(80:80) = ' '                        
      IF ( BYTBUF(1:1) .EQ.  '1'   .OR.                                         
     1     BYTBUF(1:1) .EQ.  '2'   .OR.                                         
     2     BYTBUF(1:1) .EQ.  '3'   .OR.                                         
     3     BYTBUF(1:1) .EQ.  '4') FILE6B(J)(80:80) = BYTBUF(1:1)        M36.2068
      IF ( BYTBUF(2:2) .NE. FILE6(J)(80:80)) GO TO 2738                         
      WRITE (MUNIT6, 2737)  BYTBUF(1:1)                                         
 2737 FORMAT (  '   Illegal col. 80 value  "',  A1,                     M36.2071
     1          '" .   Cancel substitution.'   )                        M36.2072
      CALL WINDOW                                                               
 2738 J = J + 1                                                                 
      IF ( J  .LE.  NUMCRD )   GO TO 2736                                       
      LINNOW = NUMCRD                                                   M36.2075
      GO TO 2618                                                                
C     FOLLOWING CODE IS FOR  "*8,?"  CASE, TO DEPOSIT IN COLUMN 80:     M36.2077
 2743 IF ( BYTBUF(1:1) .EQ. ',' )  GO TO 2749                                   
 2744 WRITE (MUNIT6, 2746)                                              M36.2079
 2746 FORMAT (   ' Sorry,  "*8"  must be followed by a',                M36.2080
     1           ' comma and digit 1-4, or blanks.'     )               M36.2081
      CALL WINDOW                                                               
      GO TO 2618                                                                
 2749 FILE6B(LINNOW)(80:80) = BYTBUF(2:2)                                       
      LIDNT1 = LINNOW                                                   M36.2084
      LIDNT2 = LINNOW                                                   M36.2085
      GO TO 2634                                                                
C     FOLLOWING CODE IS FOR  "*RULE"  REQUEST FOR COLUMN MARKING:       M36.2087
 2756 IF ( BYTBUF(3:3) .EQ.  ' ' )  GO TO 2758                                  
      WRITE (MUNIT6, 2759)                                                      
 2759 FORMAT ( 1X,  '1234567890123456789012345678901234567890',         M36.2090
     1              '1234567890123456789012345678901234567890'  )       M36.2091
      CALL WINDOW                                                               
      GO TO 2761                                                                
 2758 WRITE (MUNIT6, 2760)                                                      
 2760 FORMAT ( 5X,  '1234567890123456789012345678901234567890',         M36.2094
     1              '1234567890123456789012345678901234567890'  )       M36.2095
      CALL WINDOW                                                               
 2761 GO TO 2618                                                                
C     THE FOLLOWING IS FOR  "*S"  PROCESSING, AFTER BRANCH FROM THE     M36.2097
C     MIDDLE OF  "*F"  (WHERE STRING WAS FOUND):                        M36.2098
 2763 K = K + 1                                                                 
      IF ( IPRSPY .LT. 1 ) GO TO 32764                                          
      WRITE (MUNIT6, 2764)  K, L                                        M37.6441
 2764 FORMAT (   ' *S EXITED TO 2763.  K, L =',  2I8  )                 M36.2102
      CALL WINDOW                                                               
32764 N7 = K - L                                                                
      N12 = 81 - K                                                              
      BUFF77(1:N12) = FILE6(LINNOW)(K:80)                                       
      BUFF77(N12+1:N12+20) = BLAN80(1:20)                                       
      L = L + 2                                                                 
      GO TO 2767                                                                
 2765 FILE6(LINNOW)(N7:N7) = BYTBUF(L:L)                                        
      IF ( N7 .EQ. 80 )  GO TO 2781                                             
      N7 = N7 + 1                                                               
      L = L + 1                                                                 
      IF ( L .GT. 20 )  GO TO 2769                                              
 2767 IF ( BYTBUF(L:L) .NE. '@' )  GO TO 2765                                   
      N14 = 81 - N7                                                             
      FILE6(LINNOW)(N7:80) = BUFF77(1:N14)                                      
      IF ( IPRSPY .LT. 1 )  GO TO 2781                                          
      WRITE (MUNIT6, 2768)                                              M37.6445
 2768 FORMAT (   ' DONE WITH STRING SUB.  EXIT TO 2772.'  )             M36.2119
      CALL WINDOW                                                               
      GO TO 2781                                                                
 2769 WRITE (MUNIT6, 2772)                                              M36.2121
 2772 FORMAT (   '   Illegal  "*S" ---- 2nd  "@"  missing.'  )          M36.2122
      CALL WINDOW                                                               
      GO TO 2618                                                                
 2781 J = LINNOW                                                                
      IF ( IPRSPY .LT. 1 )  GO TO 2691                                          
      WRITE (MUNIT6, 62781)  J, N3, N4                                  M37.6449
62781 FORMAT (  ' AT 2781.  J, N3, N4 =',  3I8  )                       M36.2127
      CALL WINDOW                                                               
      GO TO 2691                                                                
C     $$$$$$$  KEY WORD NO. 28:  "LANGUAGE" $$$$  $$$$  $$$$  $$$$  $$$$M36.2129
 2782 WRITE (PROM80, 2785)                                              M36.2130
 2785 FORMAT ( '   NEXT COMMAND LANGUAGE OPERATION (SINGLE,',           M36.2131
     1         ' ENTIRE, SHOW, SPY :'  )                                M36.2132
      ASSIGN 2786 TO NEXTSN                                                     
      GO TO 9800                                                                
 2786 ANSI8 = BUFF77(1:8)                                                       
      IF ( ANSI8 .NE. 'ENTIRE  ' ) GO TO 32788                          M37.6451
      N13 = 1                                                                   
      N14 = 10                                                                  
32786 ASSIGN 2787 TO NEXTSN                                                     
      GO TO 9800                                                                
 2787 READ (BUFF77, 2788) ( SPYKWD(J), J=N13, N14 )                     M38.4121
 2788 FORMAT ( 10A8 )                                                   M38.4122
      IF ( N14 .EQ. NUMKEY )  GO TO 2782                                        
      N13 = N13 + 10                                                            
      N14 = N14 + 10                                                            
      IF ( N14 .GT. NUMKEY )  N14 = NUMKEY                                      
      GO TO 32786                                                               
32788 IF ( ANSI8 .NE. 'SHOW    ' )   GO TO 2790                         M37.6454
      DO 32789  J=1, NUMKEY, 10                                                 
      WRITE (MUNIT6, 2789) ( SPYKWD(IP), IP=J, J+9 )                            
 2789 FORMAT ( 1X, 10A8 )                                               M36.2141
      CALL WINDOW                                                               
32789 CONTINUE                                                                  
      GO TO 2782                                                                
 2790 IF ( ANSI8 .NE. 'SINGLE  ' )  GO TO 2782                                  
 2791 WRITE (PROM80, 2792)                                              M36.2143
 2792 FORMAT (   '      SEND NEXT OLD AND NEW SYMBOLS',                 M36.2144
     1           ' AS 2A8 (END) : '  )                                  M38.4128
      CALL PROMPT                                                               
      ASSIGN 2793 TO NEXTSN                                                     
      GO TO 9800                                                                
 2793 ANSI8 = BUFF77(1:8)                                                       
      BUFF77(1:8) = BUFF77(9:16)                                                
      IF ( ANSI8 .EQ. 'END     ' )  GO TO 2782                                  
      DO 2794  J=1, NUMKEY                                                      
      IF ( ANSI8 .EQ. SPYKWD(J) )  GO TO 2798                                   
 2794 CONTINUE                                                                  
      WRITE (MUNIT6, 2796)  ANSI8                                       M36.2153
 2796 FORMAT ( '    ? ? ?   Sorry,  no such symbol found.',             M38.4133
     1         '   Try again ...'  )                                    M38.4134
      CALL WINDOW                                                               
      GO TO 2791                                                                
 2798 SPYKWD(J) = BUFF77(1:8)                                                   
      GO TO 2791                                                                
C     $$$$$$$  KEY WORD NO.  7:  "SWITCH"   $$$$  $$$$  $$$$  $$$$  $$$$M36.2159
 2804 BYTBUF(1:4) = ANSW80(1:4)                                                 
      N33 = 0                                                                   
      IF ( KSWTCH .GT. 0 )                                                      
     1 GO TO 2808                                                               
      WRITE (MUNIT6, 2805)                                              M36.2164
 2805 FORMAT ( '   ---  Sorry, no such elements available in',          M36.2165
     1         ' this data case.  Try another key word ...'  )          M36.2166
      CALL WINDOW                                                               
      GO TO 1240                                                                
 2806 ASSIGN 2807 TO NEXTSN                                                     
      GO TO 9800                                                                
 2807 BYTBUF = BUFF77(1:20)                                                     
 2808 IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .EQ.  'STOP' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .NE.  'EXTR' )   GO TO 2814                             
 2810 WRITE (PROM80, 2811)                                                      
 2811 FORMAT ( 7H  Name , 10X, 4HAKEY, 10X, 4HCRIT, 8X, 6HENERGY,       M42.6774
     1         '  KSWTYP  MODSWT  NEXTSW',  2H :   )                    M36.2179
      N33 = 1                                                                   
      GO TO 2806                                                                
 2814 IF ( BYTBUF(1:4) .NE. SPYKWD(7)(1:4)  .AND.                               
     1     BYTBUF(1:4) .NE. SPYKWD(1)(1:4)  )                                   
     2  GO TO 2822                                                              
      WRITE (PROM80, 2815)                                                      
 2815 FORMAT ( ' Row name-K name-M K(KMSWIT)M KPOS KENTNB',             M38.4139
     1  3X, 6HTCLOSE,  7X,  6HADELAY,  7X, 6HTOPEN:  )                  M36.2188
      CALL PROMPT                                                               
      IF ( N33  .GT.  0 )   GO TO 2810                                          
      GO TO 2806                                                                
C     WE GET TO 2822 IF NO KNOWN KEY WORD;  EXTRACT INTEGER PAIR.   THISM36.2192
C     IS DEC FREE-FORMAT; "KSWTCH" IS MAXIMUM;  "N17" IS ERROR FLAG:    M36.2193
 2822 CALL INTPAR ( KSWTCH, N1, N2, N17 )                                       
      IF ( N17  .GT.  0 )   GO TO 2806                                          
      DO 2834  I=N1, N2                                                         
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 2825                                           
      KWTSPY = 0                                                                
      GO TO 2806                                                                
 2825 N5 = IABS ( KMSWIT(I) )                                                   
      N6 = IABS ( KMSWIT(LSWTCH+I)  )                                           
      D12 = BUS(N5)                                                     M36.2203
      IF ( N5 .EQ. 1 )  D12 = TERRA                                     M36.2204
      D13 = BUS(N6)                                                     M36.2205
      IF ( N6 .EQ. 1 )  D13 = TERRA                                     M36.2206
      WRITE (MUNIT6, 2828)  I, D12, D13,  KMSWIT(I), KMSWIT(LSWTCH+I),  M38.4141
     1          KPOS(I), KENTNB(I), TCLOSE(I), ADELAY(I), TOPEN(I)      M36.2208
 2828 FORMAT ( 1X, I3,  1X, A6,  1X, A6, I6, I5,  I5, I7,               M36.2209
     1  3E13.4   )                                                      M36.2210
      CALL WINDOW                                                               
      IF ( N33  .NE.  1 )  GO TO 2834                                           
      N7 = NAMESW(I)                                                            
      WRITE (MUNIT6, 2831)  TEXVEC(N7), ADELAY(I), CRIT(I),             M42.6776
     1               ENERGY(I), KSWTYP(I), MODSWT(I), NEXTSW(I)         M37.6465
 2831 FORMAT ( 1X,  A6,  3E14.5,  3I8 )                                 M37.6466
      CALL WINDOW                                                               
 2834 CONTINUE                                                                  
      GO TO 2806                                                                
C     $$$$$$$  KEY WORD NO. 25:  "SOURCE"   $$$$  $$$$  $$$$  $$$$  $$$$M36.2218
 2856 N33 = 0                                                                   
 2859 PROM80 = '        '                                                       
      ASSIGN 42859 TO NEXTSN                                                    
      GO TO 9800                                                                
42859 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .EQ.  'STOP' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .NE.  'TACS'  )   GO TO 2862                            
      N33 = 1                                                                   
      N14 = KXTCS + NUK                                                         
      WRITE (MUNIT6, 2860)                                              M36.2232
 2860 FORMAT (  ' Row  A6 TACS  Source value',                          M38.4149
     1  3X, 12HData field-A,  4X, 12HData field-B,                      M38.4150
     2  4X, 12HData field-C      )                                      M38.4151
      CALL WINDOW                                                               
      WRITE (PROM80, 2861)  N14, KUD1, KUD3, KUD2                       M36.2236
 2861 FORMAT ( ' no.   name',                                           M38.4152
     1  4X,  6Hoffset, I6,  3X, 6Hoffset, I6,                           M38.4153
     2  4X,  6Hoffset, I6,  4X, 6Hoffset, I6,  2H :  )                  M38.4154
      GO TO 2859                                                                
 2862 IF ( BYTBUF(1:4)  .NE.  'ELEC'  )   GO TO 2868                            
C     "ELEC" IS THE COMMAND FOR A NEW HEADING FOR ELECTRIC-NETWORK      M36.2243
C     SOURCE TABLE:                                                     M36.2244
      N33 = 0                                                                   
 2863 WRITE (PROM80, 2865)                                                      
 2865 FORMAT (  22H Row  name  NODE IFORM,  6X, 5HCREST,                M38.4155
     1  9X, 5HSFREQ,  10X, 5HTIME1,  9X, 8HTSTART :  )                  M36.2248
      GO TO 2859                                                                
 2868 IF ( N33  .EQ.  1 )   GO TO 2884                                          
C     FOLLOWING IS DISPLAY CODE FOR ELECTRIC-NETWORK SOURCE TABLE:      M36.2252
      CALL INTPAR ( KCONST, N1, N2, N17 )                                       
      IF ( N17 .GT. 0 )   GO TO 2859                                            
      DO 2876  I=N1, N2                                                         
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 2873                                           
      KWTSPY = 0                                                                
      GO TO 2859                                                                
 2873 N5 = IABS ( NODE(I) )                                                     
      WRITE (MUNIT6, 2878)  I, BUS(N5), NODE(I), IFORM(I), CREST(I),    M38.4157
     1           SFREQ(I), TIME1(I), TSTART(I)                          M36.2262
 2878 FORMAT ( 1X, I3,  1X, A6,  I5,  I6,  2E14.5,  2E15.6  )           M36.2263
      CALL WINDOW                                                               
 2876 CONTINUE                                                                  
      PROM80(1:8) = BLAN80(1:8)                                                 
      GO TO 2859                                                                
C     WE REACH 2884 WITH REQUEST TO OUTPUT ANOTHER SET OF TACS SOURCES: M36.2267
 2884 CALL INTPAR ( NIU,  N1,  N2,  N17 )                                       
      IF ( N17  .GT.  0 )   GO TO 2859                                          
      DO 2888  I=N1, N2                                                         
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 2887                                           
      KWTSPY = 0                                                                
      GO TO 2859                                                                
 2887 ANSI8 = BLAN80(1:8)                                                       
      IF ( IUTY(KIUTY+I) .LT. 90 )                                              
     1 WRITE (ANSI8, 3691) SPTACS(KALIU+I)                                      
      WRITE (MUNIT6, 2891)  I,  ANSI8,   SPTACS(KXTCS+NUK+I),           M38.4161
     1    SPTACS(KUD1+I),  SPTACS(KUD3+I),  SPTACS(KUD2+I)              M36.2279
 2891 FORMAT ( 1X,  I3,  1X, A6,  4E16.6  )                             M36.2280
      CALL WINDOW                                                               
 2888 CONTINUE                                                                  
      PROM80(1:8) = BLAN80(1:8)                                                 
      GO TO 2859                                                                
C     $$$$$$$  KEY WORD NO. 42:  "RLC"      $$$$  $$$$  $$$$  $$$$  $$$$M36.2284
 3456 BYTBUF(1:4) = ANSW80(1:4)                                                 
      N33 = 0                                                                   
      GO TO 3467                                                                
 3462 ASSIGN 3464 TO NEXTSN                                                     
      GO TO 9800                                                                
 3464 BYTBUF = BUFF77(1:20)                                                     
 3467 IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .EQ.  'STOP' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .NE.  'EXTR' ) GO TO 3490                               
      N33 = N33 + 1                                                             
      IF ( N33 .GE. 2 )  N33 = 0                                                
 3471 IF ( N33 .EQ. 1 )                                                         
     1 WRITE (PROM80, 3478)                                                     
 3478 FORMAT ( ' NO EXTENSION YET IN USE.  BUT STRUCTURE ALLOWS IT.' )  M36.2297
      IF ( N33 .EQ. 0 )                                                         
     1 WRITE (PROM80, 3488)                                                     
 3488 FORMAT (  4H Row,  9X, 5HTR(I),  9X, 5HTX(I),  10X, 4HR(I),       M38.4175
     1         10X, 4HC(I),  10X, 5HX(I):  )                            M36.2303
      GO TO 3462                                                                
 3490 IF ( BYTBUF(1:4) .EQ. SPYKWD(42)(1:4)  .OR.                               
     1     BYTBUF(1:4) .EQ. SPYKWD(1)(1:4) )                                    
     2 GO TO 3471                                                               
C     WE GET HERE IF NO KNOWN KEY WORD;  EXTRACT INTEGER PAIR.   THIS   M38.4179
C     IS DEC FREE-FORMAT; "IT" IS MAXIMUM;  "N17" IS ERROR FLAG:        M36.2308
      CALL INTPAR ( IT, N1, N2, N17 )                                           
      IF ( N17  .GT.  0 )   GO TO 3462                                          
      DO 3513  I=N1, N2                                                         
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 3497                                           
      KWTSPY = 0                                                                
      GO TO 3462                                                                
 3497 WRITE (MUNIT6, 3503)  I, TR(I), TX(I), R(I), C(I), X(I)           M38.4182
 3503 FORMAT ( 1X, I3,  5E14.5  )                                       M36.2317
      CALL WINDOW                                                               
 3513 CONTINUE                                                                  
      PROM80(1:8) = BLAN80(1:8)                                                 
      GO TO 3462                                                                
C     $$$$$$$  KEY WORD NO. 43:  "WIDTH"    $$$$  $$$$  $$$$  $$$$  $$$$M36.2321
 3526 N13 = KOL132                                                              
      IF ( N13 .NE. 80 )  GO TO 3529                                            
      WRITE (MUNIT6, 3527)                                                      
 3527 FORMAT ( '   ---  Switch from 80- to 132-column',                 M38.4185
     1         ' LUNIT6 EMTP output.'  )                                M38.4186
      CALL WINDOW                                                               
      GO TO 3532                                                                
 3529 WRITE (MUNIT6, 3528)                                                      
 3528 FORMAT ( '   ---  Switch from 132- to 80-column',                 M38.4187
     1         ' LUNIT6 EMTP output.'   )                               M38.4188
      CALL WINDOW                                                               
 3532 KOL132 = 132                                                              
      IF ( N13 .EQ. 132 )  KOL132 = 80                                          
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 44:  "BUS"      $$$$  $$$$  $$$$  $$$$  $$$$M36.2330
 3541 BYTBUF(1:4) = ANSW80(1:4)                                                 
      N33 = 0                                                                   
      GO TO 3567                                                                
 3562 ASSIGN 3564 TO NEXTSN                                                     
      GO TO 9800                                                                
 3564 BYTBUF = BUFF77(1:20)                                                     
 3567 IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .EQ.  'STOP' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .NE.  'EXTR' ) GO TO 3590                               
      N33 = N33 + 1                                                             
      IF ( N33 .GE. 2 )  N33 = 0                                                
 3572 IF ( N33 .EQ. 1 )                                                         
     1 WRITE (PROM80, 3578)                                                     
 3578 FORMAT ( 5H  Row,  8X,  5HFINIT,   6X, 14HZNONL-A KKNONL,         M38.4198
     1  6X, 14HZNONL-B KKNONL,  6X, 14HZNONL-C KKNONL ,  2H :  )        M38.4199
      IF ( N33 .EQ. 0 )                                                         
     1 WRITE (PROM80, 3588)                                                     
 3588 FORMAT (  ' node  name  KODE  KSSFRQ    KK     KKS',              M38.4202
     1         12X, 4HE(I),  12X, 6HF(I) :  )                           M36.2351
      GO TO 3562                                                                
 3590 IF ( BYTBUF(1:4)  .EQ.  SPYKWD(44)(1:4)  .OR.                             
     1     BYTBUF(1:4)  .EQ.  SPYKWD(1)(1:4) )                                  
     2 GO TO 3572                                                               
C     WE GET HERE IF NO KNOWN KEY WORD;  EXTRACT INTEGER PAIR.   THIS   M38.4206
C     IS DEC FREE-FORMAT; "IT" IS MAXIMUM;  "N17" IS ERROR FLAG:        M36.2355
      CALL INTPAR ( NTOT, N1, N2, N17 )                                         
      IF ( N17  .GT.  0 )   GO TO 3562                                          
      DO 3613  I=N1, N2                                                         
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 3597                                           
      KWTSPY = 0                                                                
      GO TO 3562                                                                
 3597 IF ( N33 .EQ. 0 )                                                         
     1 WRITE (MUNIT6, 3603)  I, BUS(I), KODE(I), KSSFRQ(I), KK(I),      M38.4210
     1                      KKS(I), E(I), F(I)                          M36.2364
 3603 FORMAT ( 1X, I4,  2X, A6,  I4, I8, I6, I8,  2E16.6  )             M36.2365
      N22 = NTOT + I                                                            
      N23 = NTOT + N22                                                          
      IF ( N33 .EQ. 1 )                                                         
     1 WRITE (MUNIT6, 3609)  I,  FINIT(I),  ZNONL(I), KKNONL(I),        M38.4212
     2               ZNONL(N22), KKNONL(N22),  ZNONL(N23), KKNONL(N23)  M36.2371
 3609 FORMAT ( 1X, I4, F13.4,  3( F14.5, I6)  )                         M38.4213
      CALL WINDOW                                                               
 3613 CONTINUE                                                                  
      PROM80(1:8) = BLAN80(1:8)                                                 
      GO TO 3562                                                                
C     $$$$$$$  KEY WORD NO. 45:  "SIZE"     $$$$  $$$$  $$$$  $$$$  $$$$M36.2375
 3635 WRITE (MUNIT6, 3638)  ( LSTAT(I+20),  I=1, 10 )                   M37.6482
 3638 FORMAT (   14H   Size  1-10:,  10I6  )                            M38.4215
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3639)  ( LSTAT(I+20), I=11, 20 )                   M37.6485
 3639 FORMAT (   14H   Size 11-20:,  10I6  )                            M38.4216
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3640)  ( LSTAT(I+20), I=21, 27 )                   M38.4217
 3640 FORMAT (   14H   Size 21-on:,  10I6   )                           M38.4218
      CALL WINDOW                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 46:  "LIMIT"    $$$$  $$$$  $$$$  $$$$  $$$$M36.2382
 3647 WRITE (MUNIT6, 3649)  LBUS, LBRNCH, LDATA, LEXCT, LYMAT,          M36.2383
     1                      LSWTCH, LSIZE7, LPAST, LNONL, LCHAR         M38.4219
 3649 FORMAT (  15H   Limit  1-10:,  10I6 )                             M38.4220
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3650)  LSMOUT, LSIZ12, LFDEP, LWT, LTAILS,         M38.4221
     1                    LIMASS, LSYN, MAXPE, LTACST, LFSEM            M38.4222
 3650 FORMAT (  15H   Limit 11-20:,  10I6 )                             M38.4223
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3651)  LFD, LHIST, LSIZ23, LCOMP, LSPCUM,          M38.4224
     1                      LSIZ26, LSIZ27                              M38.4225
 3651 FORMAT (  15H   Limit 21-ON:,  10I6 )                             M38.4226
      CALL WINDOW                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 47:  "IOUT"     $$$$  $$$$  $$$$  $$$$  $$$$M36.2392
 3662 WRITE (PROM80, 3664)  IOUT                                                
 3664 FORMAT (  '    SEND NEW PRINTOUT FREQUENCY [', I5,  ' ] : '  )    M36.2394
      ASSIGN 3665 TO NEXTSN                                                     
      GO TO 9800                                                                
 3665 CALL FREIN1 ( BUFF77, N6 )                                                
      LIMSTP = 999999                                                           
      IF ( N6 .LE. 0 )  N6 = 1                                                  
      CALL COPYI ( N6, MULTPR(1), 5 )                                           
      N7 = ISTEP - 1                                                            
      DO 3668  J=1, 9999                                                        
      N7 = N7 + 1                                                               
      N8 = N7 / N6                                                              
      IF ( N6*N8 .NE. N7 )  GO TO 3668                                          
      IF ( IPRSUP .LT. 1 )  GO TO 3667                                          
      WRITE (MUNIT6, 3666)  N7                                          M37.6502
 3666 FORMAT ( ' NEW ROUND STOP.  N7 =',  I8  )                         M36.2408
      CALL WINDOW                                                               
 3667 IF ( N6 .LT. 20 )  N7 = N7 + N6                                           
      IF ( N7 .LT. ISTEP+20 ) N7 = N7 + N6                                      
      KPRCHG(2) = N7                                                            
      KPRCHG(5) = 999999                                                        
      KPRCHG(4) = 999999                                                        
      KPRCHG(3) = 999999                                                        
      GO TO 3675                                                                
 3668 CONTINUE                                                                  
 3675 IOUT = N6                                                                 
      IF ( ISPRIN .GT. IOUT )                                                   
     1 ISPRIN = IOUT                                                            
      LIMSTP = N7                                                               
      INDSTP = 2                                                                
      IF ( IPRSPY .LT. 1 ) GO TO 3679                                           
      WRITE (MUNIT6, 3678) LIMSTP, INDSTP, ISTEP, IOUT, ISPRIN          M37.6506
 3678 FORMAT ( ' LIMSTP, INDSTP, ISTEP, IOUT, ISPRIN =', 5I8 )          M36.2424
      CALL WINDOW                                                               
 3679 GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 48:  "NODE"     $$$$  $$$$  $$$$  $$$$  $$$$M36.2426
 3684 WRITE (PROM80, 3687)                                              M36.2427
 3687 FORMAT (  '       SEND  A6  BUS NAME (CONNECT) :'  )              M38.4230
 3688 ASSIGN 3690 TO NEXTSN                                                     
      GO TO 9800                                                                
 3690 SPYCD2 = BUFF77(1:35)                                                     
      IF ( SPYCD2(1:8) .EQ. 'STOP    '   .OR.                           M38.4234
     1     SPYCD2(1:8) .EQ. 'END     '   )  GO TO 1240                          
      IF ( SPYCD2(1:8) .EQ. 'CONNECT '   )  GO TO 3701                          
      DO 3693  J=1, NTOT                                                        
      WRITE (BROBUS, 3691)  BUS(J)                                      M36.2437
 3691 FORMAT ( A6, 2X )                                                 M36.2438
      IF ( BROBUS .NE. SPYCD2(1:8) )  GO TO 3693                                
      WRITE (PROM80, 3692)  J                                           M38.4235
 3692 FORMAT (  '       >>> Node =',  I4,  15X,  1H:  )                 M38.4236
      GO TO 3688                                                                
 3693 CONTINUE                                                                  
      WRITE (MUNIT6, 3695)                                              M36.2445
 3695 FORMAT (                                                          M36.2446
     1  '        ????  Sorry, no such bus.  Try again ...'  )           M36.2447
      CALL WINDOW                                                               
      GO TO 3684                                                                
 3701 MMFIND = J                                                                
      N6 = 0                                                                    
      DO 3705  K=1, IBR                                                         
      IF ( MMFIND  .NE.  IABS(KBUS(K))   .AND.                          M36.2452
     1     MMFIND  .NE.  IABS(MBUS(K)) )   GO TO 3705                   M36.2453
      IF ( N6 .LT. 20 ) GO TO 3703                                              
      WRITE (MUNIT6, 3708)  ( MMHOLD(IP), IP=1, N6 )                    M38.4238
      CALL WINDOW                                                               
      N6 = 0                                                                    
 3703 N6 = N6 + 1                                                               
      MMHOLD(N6) = K                                                    M36.2455
 3705 CONTINUE                                                          M36.2456
      IF ( N6 .LE. 0 )  GO TO 3709                                              
      WRITE (MUNIT6, 3708)  ( MMHOLD(K), K=1, N6 )                      M37.6511
 3708 FORMAT (  '    %% Linear branches :',  20I5  )                    M38.4242
      CALL WINDOW                                                               
 3709 N6 = 0                                                                    
      DO 3719  K=1, KSWTCH                                                      
      IF ( MMFIND .NE. IABS(KMSWIT(K))   .AND.                          M36.2462
     1     MMFIND .NE. IABS(KMSWIT(LSWTCH+K) ) )   GO TO 3719           M36.2463
      N6 = N6 + 1                                                               
      MMHOLD(N6) = K                                                    M36.2465
 3719 CONTINUE                                                                  
      IF ( N6 .LE. 0 )  GO TO 3722                                              
      WRITE (MUNIT6, 3721)  ( MMHOLD(K), K=1, N6 )                      M37.6515
 3721 FORMAT (  '    %% EMTP  switches  :',  20I5  )                    M38.4243
      CALL WINDOW                                                               
 3722 N6 = 0                                                                    
      DO 3726  K=1, INONL                                                       
      IF ( MMFIND .NE. IABS(NONLK(K))   .AND.                           M36.2472
     1     MMFIND .NE. IABS(NONLM(K)) )  GO TO 3726                     M36.2473
      N6 = N6 + 1                                                               
      MMHOLD(N6) = K                                                    M36.2475
 3726 CONTINUE                                                          M36.2476
      IF ( N6 .LE. 0 )  GO TO 3731                                              
      WRITE (MUNIT6, 3730)  ( MMHOLD(K), K=1, N6 )                      M37.6519
 3730 FORMAT (  '    %% Nonlinear elem. :',  20I5  )                    M38.4244
      CALL WINDOW                                                               
 3731 N6 = 0                                                                    
      DO 3734  K=1, KCONST                                                      
      IF ( MMFIND .NE. IABS(NODE(K)) )  GO TO 3734                      M36.2482
      N6 = N6 + 1                                                               
      MMHOLD(N6) = K                                                    M36.2484
 3734 CONTINUE                                                          M36.2485
      IF ( N6 .LE. 0 )  GO TO 3739                                              
      WRITE (MUNIT6, 3738)  ( MMHOLD(K), K=1, N6 )                      M37.6523
 3738 FORMAT (  '    %% EMTP  sources  : ',  20I5  )                    M38.4245
      CALL WINDOW                                                               
 3739 MMFIND = 0                                                                
      GO TO 3684                                                                
C     $$$$$$$  KEY WORD NO. 49:  "NONLIN"   $$$$  $$$$  $$$$  $$$$  $$$$M36.2491
 3752 BYTBUF(1:4) = ANSW80(1:4)                                                 
      N33 = 0                                                                   
      IF ( INONL .GT. 0 )                                                       
     1 GO TO 3762                                                               
      WRITE (MUNIT6, 2805)                                                      
      CALL WINDOW                                                               
      GO TO 1240                                                                
 3758 ASSIGN 3760 TO NEXTSN                                                     
      GO TO 9800                                                                
 3760 BYTBUF = BUFF77(1:20)                                                     
 3762 IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .EQ.  'STOP' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .NE.  'EXTR' )   GO TO 3774                             
 3766 WRITE (PROM80, 3769)                                                      
 3769 FORMAT (  9X, 5HANONL,  8X, 5HVZERO,  8X, 5HVNONL,                M36.2507
     1    9X, 4HCURR,  7X, 6HVECNL1,  7X,  7HVECNL1:   )                M36.2508
      N33 = 1                                                                   
      GO TO 3758                                                                
 3774 IF ( BYTBUF(1:4) .NE. SPYKWD(49)(1:4)  )                                  
     1  GO TO 3781                                                              
      WRITE (PROM80, 3777)                                                      
 3777 FORMAT ( ' Row  name  name-K name-M  NONLK  NONLM NLTYPE',        M42.6777
     1     ' NONLAD NONLE ILAST KUPL NLSUB :'  )                        M36.2516
      CALL PROMPT                                                               
      IF ( N33  .GT.  0 )   GO TO 3766                                          
      PROM80(1:8) = BLAN80(1:8)                                                 
      GO TO 3758                                                                
C     WE GET TO 3781 IF NO KNOWN KEY WORD;  EXTRACT INTEGER PAIR.   THISM36.2520
C     IS DEC FREE-FORMAT; "INONL" IS MAXIMUM;  "N17" IS ERROR FLAG:     M36.2521
 3781 CALL INTPAR ( INONL, N1, N2, N17 )                                        
      IF ( N17  .GT.  0 )   GO TO 3758                                          
      DO 3796  I=N1, N2                                                         
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 3785                                           
      KWTSPY = 0                                                                
      GO TO 3758                                                                
 3785 N5 = IABS ( NONLK(I) )                                                    
      D12 = BUS(N5)                                                             
      IF ( N5 .EQ. 1 )  D12 = TERRA                                             
      N6 = IABS ( NONLM(I) )                                                    
      D13 = BUS(N6)                                                             
      IF ( N6 .EQ. 1 )  D13 = TERRA                                             
      N7 = NAMENL(I)                                                            
      WRITE (MUNIT6, 3786) I, TEXVEC(N7), D12, D13, NONLK(I), NONLM(I), M42.6779
     1                 NLTYPE(I), NONLAD(I), NONLE(I), ILAST(I),        M36.2536
     2                 KUPL(I),  NLSUB(I)                               M36.2537
 3786 FORMAT ( 1X, I3,  3( 1X, A6),  4I7, 2I6, I5, I6  )                M38.4254
      CALL WINDOW                                                               
      IF ( N33 .NE. 1 ) GO TO 3796                                              
      WRITE (MUNIT6, 3792)  ANONL(I), VZERO(I), VNONL(I), CURR(I),      M38.4256
     2                  VECNL1(I), VECNL2(I)                            M36.2542
 3792 FORMAT ( 1X,  6E13.4 )                                            M38.4257
      CALL WINDOW                                                               
 3796 CONTINUE                                                                  
      PROM80(1:8) = BLAN80(1:8)                                                 
      GO TO 3758                                                                
C     $$$$$$$  KEY WORD NO. 50:  "SPACE"    $$$$  $$$$  $$$$  $$$$  $$$$M36.2547
 3842 D14 = PLTBUF(INDBEG+1)                                                    
      KPTPLT = LSTAT(32)                                                        
      N77 = 0                                                                   
      N22 = KPTPLT + 1                                                          
      WRITE (MUNIT6, 3843)                                              M37.6528
 3843 FORMAT ( 2X, 40H Current values.  INDBUF  INDBEG  LIMBUF,         M38.4261
     1             32H  MFLUSH  NUMDCD  NUMCRD  LIMCRD   )              M37.6530
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3844)  INDBUF, INDBEG, LIMBUF, MFLUSH,             M37.6532
     1                      NUMDCD, NUMCRD, LIMCRD                      M37.6533
 3844 FORMAT ( 18X, 7I8 )                                               M37.6534
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3845)  D14, T                                      M37.6536
 3845 FORMAT ( 3X,                                                      M37.6537
     1    'Plot timespan now in memory (in sec) =',  2E15.6 )           M38.4262
      CALL WINDOW                                                               
      IF ( MFLUSH .LT. 1000 )  GO TO 3848                                       
 3846 BUFF77(1:6) = 'FLUSH '                                                    
      D17 = -1                                                                  
      D18 = FLTINF                                                              
      N4 = 1                                                                    
      GO TO 3876                                                                
 3848 IF ( LASTOV .NE. 9911 )                                                   
     1 GO TO 3851                                                               
      LASTOV = NCHAIN - 1                                                       
      GO TO 3857                                                                
 3851 WRITE (PROM80, 3854)                                              M36.2565
 3854 FORMAT ( '    CHOOSE CLASS (CARDS, PLOT) :'  )                    M38.4267
      ASSIGN 3855 TO NEXTSN                                                     
      GO TO 9800                                                                
 3855 IF ( BUFF77(1:4) .NE. 'PLOT' ) GO TO 3934                                 
C     BEGIN LOGIC TO FREE SOME STORAGE SPACE FOR EMTP PLOT DATA:        M36.2571
 3857 WRITE (PROM80, 3858)                                              M36.2572
 3858 FORMAT ( '   OPERATION (WRITE, THIN, FLUSH,',                     M36.2573
     1         ' DELETE, AUTO, READ, OUT, HELP) :'  )                   M38.4271
      ASSIGN 43858 TO NEXTSN                                                    
      GO TO 9800                                                                
43858 IF ( BUFF77(1:6) .EQ. 'DELETE' )  N77 = 1                                 
      IF ( BUFF77(1:6) .EQ. 'READ  ' )  N77 = 2                                 
      IF ( BUFF77(1:6) .EQ. 'THIN  ' )  N77 = 3                                 
      IF ( BUFF77(1:6) .EQ. 'WRITE ' )  N77 = 4                                 
      IF ( BUFF77(1:6) .EQ. 'FLUSH ' )  N77 = 5                                 
      IF ( BUFF77(1:4) .NE. 'HELP' )  GO TO 3861                        M38.4279
      WRITE (MUNIT6, 3859)                                              M36.2578
 3859 FORMAT ( '        To free memory for more simulation',            M38.4280
     1         ' plot points,  use one of these:'      )                M38.4281
      CALL WINDOW                                                               
      WRITE (MUNIT6, 33859)                                             M37.6542
33859 FORMAT ( '        Either: 1) Begin by saving',                    M38.4282
     1         ' points on disk, prior to loss ("WRITE");'  )           M38.4283
      CALL WINDOW                                                               
      WRITE (MUNIT6, 43859)                                             M37.6546
43859 FORMAT ( '                2) thin out present',                   M38.4284
     1         ' storage by regular omission ("THIN");'  )              M38.4285
      CALL WINDOW                                                               
      WRITE (MUNIT6, 53859)                                             M37.6550
53859 FORMAT ( '                3) Delete older',                       M38.4286
     1         ' points, and shift the remainder ("DELETE") :' )        M38.4287
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3860)                                              M36.2588
 3860 FORMAT (                                                          M36.2589
     1     '                4) combined "WRITE" and',                   M38.4288
     2         ' "THIN", with common IPLOT ("FLUSH").'    )             M38.4289
      CALL WINDOW                                                               
      WRITE (MUNIT6, 23860)                                             M37.6556
23860 FORMAT ( '        Send  "READ"  to load memory',                  M38.4290
     1         ' with plot data now on disk (LUNIT4).'  )               M38.4291
      CALL WINDOW                                                               
      WRITE (MUNIT6, 63860)                                             M37.6560
63860 FORMAT (  '        Send  "OUT"  to switch from plot',             M38.4292
     1          ' file to cards,  or  "SPY"  to abort.'  )              M38.4293
      CALL WINDOW                                                               
      WRITE (MUNIT6, 73860)                                             M37.6564
73860 FORMAT ( '        Finally, send  "AUTO"  for automatic',          M38.4294
     1         ' full flush to disk from now on.'   )                   M38.4295
      CALL WINDOW                                                               
      GO TO 3857                                                                
 3861 IF ( BUFF77(1:4) .NE. 'AUTO' )  GO TO 3862                        M36.2599
      IF ( MFLUSH .LT. 1000 ) MFLUSH = MFLUSH + 1000                            
      GO TO 3846                                                                
 3862 IF ( BUFF77(1:4) .EQ. 'OUT ') GO TO 3851                                  
      WRITE (PROM80, 3863)                                              M36.2604
 3863 FORMAT ( '     SEND [Tmin, Tmax]',                                M36.2605
     1         ' (ALL, HALF) :'  )                                      M38.4296
      ASSIGN 3865 TO NEXTSN                                                     
      GO TO 9800                                                                
 3865 ANSW80 = BUFF77                                                   M38.4299
      IF ( ANSW80(1:4) .NE. 'ALL ' )  GO TO 3867                        M36.2610
      D17 = -1.                                                                 
      D18 = FLTINF                                                              
      GO TO 3874                                                                
 3867 IF ( ANSW80(1:4) .NE. 'HALF' )  GO TO 3871                        M36.2614
      N1 =  ( INDBUF - INDBEG ) / 2                                             
      N2 = ( N1 / N22 ) * N22                                                   
      N3 = INDBEG + N2 + 1                                                      
      D17 = -1.                                                                 
      D18 = PLTBUF(N3)                                                          
      IF ( IPRSPY .GE. 1 )                                              M38.4301
     1 WRITE (*, 3869)  N22, N3, D18                                    M38.4302
 3869 FORMAT ( ' "HALF" computed.  N22, N3, D18 =',  2I8, E14.5 )       M38.4303
      GO TO 3874                                                                
 3871 CALL FREFP2 ( ANSW80, D17, D18 )                                          
 3874 N4 = INTINF                                                               
      IF ( N77 .EQ. 1 )                                                         
     1  GO TO 3876                                                              
      WRITE (PROM80, 3875)                                              M36.2625
 3875 FORMAT ( '      PROCESS EVERY  N-TH  STEP.  SEND N:'  )           M36.2626
      ASSIGN 43875 TO NEXTSN                                                    
      GO TO 9800                                                                
43875 PROM80 = BUFF77                                                   M38.4307
      CALL FREIN1 ( PROM80, N4 )                                        M36.2629
      IF ( N4 .EQ. 0 ) N4 = 1                                                   
      IF ( N4 .LT. 0 )  GO TO 3857                                              
      IF ( N77 .EQ. 2 )  GO TO 3915                                             
 3876 N13 = INDBEG + 1                                                          
 3877 IF ( PLTBUF(N13) .GE. D17 )  GO TO 3886                                   
      N13 = N13 + N22                                                           
      IF ( N13 .LT. INDBUF )  GO TO 3877                                        
      WRITE (MUNIT6, 3881)                                              M36.2637
 3881 FORMAT ( '       ?? ??  Sorry, no plot points in the',            M36.2638
     1         ' requested time span.  Abort.'  )                       M36.2639
      CALL WINDOW                                                               
      GO TO 1240                                                                
 3886 IF ( N77 .NE. 4   .AND.                                                   
     1     N77 .NE. 5 )  GO TO 3904                                             
      MFLUSH = MFLUSH + 1                                                       
      WRITE (LUNIT6, 3891)  MFLUSH, N6, N4, D17, D18                    M36.2648
 3891 FORMAT ( '   ++++  Begin plot-data copy from',                    M38.4313
     1         ' memory to disk.   MFLUSH, N6,',                        M38.4314
     2         ' N4, Tbeg, Tend =',  I3, 2I8, 2E14.5  )                 M38.4315
      GO TO 3906                                                                
 3904 WRITE (LUNIT6, 3905)  N4, D17, D18                                M36.2680
 3905 FORMAT ( '   ****  Begin thinning of plot data',                  M38.4317
     1         ' points, to reduce memory burden.',                     M38.4318
     2         '   N4, Tbeg, Tend =',  I10, 2E14.5  )                   M36.2683
 3906 IP = 0                                                                    
      N8 = N13 + KPTPLT                                                         
      N6 = N13                                                                  
      DO 3910  L=1, 999999                                                      
      IF ( PLTBUF(N13) .GT. D18 )                                               
     1  GO TO 3911                                                              
      IP = IP + 1                                                               
      IF ( IP .LT. N4 ) GO TO 3908                                              
      IP = 0                                                                    
      IF ( N77 .NE. 4   .AND.                                                   
     1     N77 .NE. 5 )  GO TO 3907                                             
      WRITE (LUNIT4)  ( PLTBUF(M), M=N13, N8 )                                  
      IF ( N77 .EQ. 4 )  GO TO 3908                                             
 3907 CALL MOVESP ( PLTBUF(N13), PLTBUF(N6), N22 )                      M38.4324
      N6 = N6 + N22                                                             
 3908 N13 = N13 + N22                                                           
      IF ( N13 .GE. INDBUF )  GO TO 3911                                        
 3910 N8 = N8 + N22                                                             
 3911 IF ( N77 .EQ. 4 )  GO TO 3914                                             
      N14 = INDBUF - N13 + 1                                                    
      IF ( N14 .GT. KPTPLT )                                                    
     1 CALL MOVESP ( PLTBUF(N13), PLTBUF(N6), N14 )                             
      INDBUF = N6 + N14 - 1                                                     
      IASCII(1000) = N13 - N6                                                   
      IF ( N77 .NE. 5 ) GO TO 3914                                              
      N5 = LIMBUF - INDBUF                                                      
      WRITE (LUNIT6, 3913)  N5                                          M38.4333
 3913 FORMAT ( 9X,  'Completed "FLUSH", leaving',                       M38.4334
     1       I8,    '  free cells for new plot points.'  )              M38.4335
      IF ( MFLUSH .GE. 1000 ) GO TO 1240                                        
 3914 GO TO 3857                                                                
C     BEGIN "READ" SERVICING (PLOT DATA GOES DISK TO MEMORY):           M36.2703
 3915 WRITE (PROM80, 3916)                                              M36.2704
 3916 FORMAT ( '      DESTINATION (0--START; 1--AS NOW) :'  )           M36.2705
      ASSIGN 3917 TO NEXTSN                                                     
      GO TO 9800                                                                
 3917 PROM80 = BUFF77                                                   M38.4340
      CALL FREIN1 ( PROM80, N3 )                                        M36.2708
      N13 = INDBUF                                                              
      IF ( N3 .EQ. 0 )  N13 = INDBEG                                            
      IP = 0                                                                    
      N8 = N13 + KPTPLT                                                         
      N11 = 0                                                                   
      DO 3925  J=1, 999999                                                      
      IP = IP + 1                                                               
      IF ( IP .GE. N4 )  GO TO 3919                                             
      READ (LUNIT4, END=3926) D15                                               
      IF ( D15 .GT. D18 )  GO TO 3928                                           
      GO TO 3921                                                                
 3919 READ (LUNIT4, END=3926) ( PLTBUF(L), L=N13, N8 )                  M36.2720
      IF ( PLTBUF(N13) .GT. D18 )  GO TO 3934                                   
      N11 = N11 + 1                                                             
 3921 N8 = N8 + N22                                                             
      IF ( N8 .LE. LIMBUF )  GO TO 3925                                         
      WRITE (MUNIT6, 3923)  PLTBUF(N13)                                 M36.2725
 3923 FORMAT ( '   ###  Squish.  Memory space for plot data',           M36.2726
     1         ' has run out after storing T=',  E14.5  )               M36.2727
      CALL WINDOW                                                               
      GO TO 3928                                                                
 3925 N13 = N13 + N22                                                           
      J = 999999                                                                
      GO TO 3928                                                                
 3926 WRITE (MUNIT6, 3927)  D18                                                 
 3927 FORMAT ( '     EOF during READ (LUNIT4).  Quit search',           M36.2733
     1         ' for Tend =',  E13.4  )                                 M36.2734
      CALL WINDOW                                                               
 3928 INDBUF = N13                                                              
      N6 = LIMBUF - INDBUF                                                      
      WRITE (MUNIT6, 3931)  J, N11, N6                                  M36.2737
 3931 FORMAT ( '     Steps read and retained =',  I7, I6,               M36.2738
     1         '   Remaining free words =',  I8  )                      M36.2739
      CALL WINDOW                                                               
      GO TO 3857                                                                
C     BEGIN "CARDS" SERVICING (FILE6 CHARACTER STORAGE):                M36.2741
 3934 WRITE (PROM80, 3937)                                              M36.2742
 3937 FORMAT ( '   CHOOSE OPERATION (MOVE, BLANK,',                     M36.2743
     1         ' OUT, SPACE) :'  )                                      M38.4341
      ASSIGN 3938 TO NEXTSN                                                     
      GO TO 9800                                                                
 3938 IF ( BUFF77(1:4) .EQ. 'OUT '   )  GO TO 3851                              
      IF ( BUFF77(1:6) .EQ. 'SPACE ' )  GO TO 3851                              
      N66 = 0                                                                   
      IF ( BUFF77(1:6) .EQ. 'BLANK ' )  N66 = 1                                 
      WRITE (PROM80, 3941)                                              M36.2750
 3941 FORMAT ( '      NUMBER OF CARDS TO BE HANDLED :'  )               M36.2751
      ASSIGN 3942 TO NEXTSN                                                     
      GO TO 9800                                                                
 3942 CALL FREIN1 ( BUFF77, N11 )                                               
      IF ( N11 .LE. 0 )  GO TO 3851                                             
      IF ( N66 .EQ. 1 )  GO TO 3955                                             
      WRITE (PROM80, 3944)                                                      
 3944 FORMAT ( '      CARD ADDRESSES N-from and N-to :'  )              M36.2758
      ASSIGN 3947 TO NEXTSN                                                     
      GO TO 9800                                                                
 3947 CALL FREIN1 ( BUFF77, N11 )                                               
      IF ( N17 .LE. 0   )  GO TO 3934                                           
      IF ( N18 .LT. N17 )  GO TO 3934                                           
      DO 3952  J=1, N11                                                         
      FILE6(N18) = FILE6(N17)                                                   
      N17 = N17 + 1                                                             
 3952 N18 = N18 + 1                                                             
      GO TO 3934                                                                
 3955 WRITE (PROM80, 3956)                                                      
 3956 FORMAT ( '      CARD ADDRESS :'  )                                M36.2770
      ASSIGN 3959 TO NEXTSN                                                     
      GO TO 9800                                                                
 3959 CALL FREIN1 ( BUFF77, N17 )                                               
      IF ( N17 .LE. 0 )  GO TO 3851                                             
      DO 3962  J=1, N11                                                         
      FILE6(N17) = BLAN80                                                       
 3962 N17 = N17 + 1                                                             
      GO TO 3934                                                                
C     $$$$$$$  KEY WORD NO. 51:  "LUNIT4"   $$$$  $$$$  $$$$  $$$$  $$$$M36.2778
 4002 FORBYT(1) = -FLTINF                                                       
      KPTPLT = LSTAT(32)                                                        
      D24 = FLTINF                                                              
 4008 WRITE (PROM80, 4010)                                              M36.2781
 4010 FORMAT ( '   OPERATION (OPEN, CLOSE,',                            M36.2782
     1         ' TOP, BOT, NEXT, BACK, TIME) :'  )                      M38.4360
      ASSIGN 4011 TO NEXTSN                                                     
      GO TO 9800                                                                
 4011 IF ( BUFF77(1:5) .NE. 'CLOSE' )  GO TO 4022                       M38.4363
      WRITE (PROM80, 4013)                                              M36.2788
 4013 FORMAT ( '     SAVE PERMANENTLY? (Y OR N) :'  )                   M36.2789
      ASSIGN 4016 TO NEXTSN                                                     
      GO TO 9800                                                                
 4016 ANSI8 = 'KEEP    '                                                M38.4366
      IF ( BUFF77(1:1) .EQ. 'N' )  ANSI8 = 'DELETE  '                   M38.4367
      CLOSE (UNIT=LUNIT4, STATUS=ANSI8)                                 M36.2794
      GO TO 4008                                                                
 4022 IF ( BUFF77(1:5) .NE. 'OPEN ' )  GO TO 4036                       M36.2796
      WRITE (PROM80, 4025)                                              M36.2797
 4025 FORMAT ( '     DESIRED DISK FILE NAME :'  )                       M36.2798
      ASSIGN 4026 TO NEXTSN                                                     
      GO TO 9800                                                                
 4026 ANSI32 = BUFF77(1:32)                                                     
      WRITE (PROM80, 4028)                                              M36.2802
 4028 FORMAT ( '     DESIRED STATUS (NEW, OLD) :'  )                    M36.2803
      ASSIGN 4029 TO NEXTSN                                                     
      GO TO 9800                                                                
 4029 OPEN (UNIT=LUNIT4, STATUS=BUFF77, FILE=ANSI32,                    M38.4374
     1      FORM='UNFORMATTED' )                                        M36.2808
      GO TO 4008                                                                
 4036 IF ( BUFF77(1:4) .NE. 'TOP ' )  GO TO 4051                        M38.4376
 4037 REWIND LUNIT4                                                             
      READ (LUNIT4)  DATE1, TCLOCK, N1, N2, N3, N4                      M36.2812
      IF ( NCHAIN .LE. 16  .OR.                                                 
     1     NCHAIN .GT. 20 )                                                     
     2 GO TO 4044                                                               
      IF ( N2 .EQ. NUMNVO  .AND.                                                
     2     N3 .EQ. NC-NV   .AND.                                                
     3     N4 .EQ. NC )  GO TO 4044                                             
      WRITE (MUNIT6, 4038)                                              M37.6574
 4038 FORMAT ( '   ++++  Error.  Inconsistent LUNIT4 plot',             M38.4378
     1         ' file.  Automatic close.  Try again.'   )               M38.4379
      CALL WINDOW                                                               
      WRITE (MUNIT6, 4039)  DATE1, TCLOCK, N1, N2, N3, N4               M36.2819
 4039 FORMAT (  '         DATE1, TCLOCK =', 2( 1X, 2A4),                M37.6578
     3         5X,  'N1:N4 =',  4I8  )                                  M36.2823
      CALL WINDOW                                                               
      CLOSE (LUNIT4)                                                    M36.2824
      GO TO 4008                                                                
 4044 IF ( N2 .GT. 0 )  READ (LUNIT4)  N5                                       
      IF ( N4 .GT. 0 )  READ (LUNIT4)  N6                                       
      KPTPLT = NUMNVO + NC                                                      
      IF ( BUFF77(1:5) .EQ. 'TIME ' ) GO TO 24085                               
      GO TO 4008                                                                
 4051 IF ( BUFF77(1:4) .NE. 'BOT ' )  GO TO 4068                        M38.4385
      N15 = 0                                                                   
      DO 4059  K=1, 100                                                         
      DO 4053  J=1, 1000                                                M36.2833
 4053 READ (LUNIT4, END=4062)  FORBYT(1)                                        
      N15 = N15 + 1000                                                          
      WRITE (MUNIT6, 4056)  N15                                         M36.2836
 4056 FORMAT ( '+      Still reading.',  I8  )                          M38.4387
      CALL WINDOW                                                               
 4059 CONTINUE                                                                  
      GO TO 4008                                                                
 4062 N15 = N15 + J                                                             
      WRITE (MUNIT6, 4063)  FORBYT(1), N15                              M38.4390
 4063 FORMAT ( '       Ok, at end LUNIT4.  T-end =',                    M38.4391
     1          E15.6,  '   Steps taken =',  I8  )                      M38.4392
      CALL WINDOW                                                               
      GO TO 4008                                                                
 4068 IF ( BUFF77(1:4) .NE. 'NEXT' )  GO TO 4079                        M36.2844
 4069 READ (LUNIT4, END=4075)  ( FORBYT(J), J=1, KPTPLT+1 )             M38.4394
      WRITE (MUNIT6, 4071)     ( FORBYT(J), J=1, KPTPLT+1 )             M38.4395
 4071 FORMAT ( 1X, 10E13.4 )                                            M36.2847
      CALL WINDOW                                                               
      IF ( FORBYT(1) .LT. D24 )  D24 = FORBYT(1)                                
      GO TO 4008                                                                
 4075 WRITE (MUNIT6, 4076)                                              M36.2849
 4076 FORMAT ( '     ===   Sorry,  no more  LUNIT4  data.',             M38.4398
     1         '   EOF hit.'  )                                         M38.4399
      CALL WINDOW                                                               
      GO TO 4008                                                                
 4079 IF ( BUFF77(1:4) .NE. 'BACK' )  GO TO 4083                        M36.2852
      IF ( NOBACK .NE. 1 )  GO TO 4082                                          
      WRITE (MUNIT6, 4080)                                              M38.4402
 4080 FORMAT ( '    Ok,  but wait for  "TOP"  and  "TIME"',             M38.4403
     1         '  using  t = t - 2*DELTAT'  )                           M38.4404
      CALL WINDOW                                                               
      BUFF77(1:5) = 'TIME '                                                     
      D8 = FORBYT(1) - 2 * DELTAT                                               
      GO TO 4037                                                                
 4082 BACKSPACE LUNIT4                                                          
      READ (LUNIT4, END=4075)   ( FORBYT(J), J=1, KPTPLT+1 )            M38.4410
      WRITE (MUNIT6, 4071)      ( FORBYT(J), J=1, KPTPLT+1 )            M38.4411
      CALL WINDOW                                                               
      IF ( FORBYT(1) .GT. D24 )                                                 
     1 BACKSPACE LUNIT4                                                         
      GO TO 4008                                                                
 4083 IF ( BUFF77(1:4) .NE. 'TIME' )  GO TO 4098                        M36.2858
      WRITE (PROM80, 4084)  FORBYT(1)                                   M38.4415
 4084 FORMAT ( '       SEND DESIRED TIME [',  E15.6,  ' ] :'  )         M38.4416
      ASSIGN 4085 TO NEXTSN                                                     
      GO TO 9800                                                                
 4085 CALL FREFP1 ( BUFF77, D8 )                                        M38.4419
24085 N15 = 0                                                                   
      DO 4089  K=1, 100                                                         
      DO 4086  J=1, 1000                                                M36.2865
      READ (LUNIT4, END=4090) FORBYT(1)                                         
      IF ( FORBYT(1) .GE. D8 ) GO TO 4094                                       
 4086 CONTINUE                                                                  
      N15 = N15 + 1000                                                          
      WRITE (MUNIT6, 4056)  N15                                                 
      CALL WINDOW                                                               
 4089 CONTINUE                                                                  
      GO TO 4008                                                                
 4090 N15 = N15 + J                                                             
      WRITE (MUNIT6, 4091)   N15                                        M38.4424
 4091 FORMAT (  '      ? ? ?   Search fails with EOF after',            M38.4425
     1     I8,  '   steps.'  )                                          M38.4426
      CALL WINDOW                                                               
      GO TO 4008                                                                
 4094 N15 = N15 + J                                                             
      WRITE (MUNIT6, 4095)  FORBYT(1), N15                              M38.4428
 4095 FORMAT ( '      Ok, record of  t =',  E15.6,                      M38.4429
     1         '  just read on try number',  I7  )                      M38.4430
      CALL WINDOW                                                               
      GO TO 4008                                                                
C     PUT ANY LATER OPERATION CHOICES HERE AND BELOW:                   M36.2882
 4098 WRITE (MUNIT6, 2796)                                                      
      CALL WINDOW                                                               
      GO TO 4008                                                                
C     $$$$$$$  KEY WORD NO. 52:  "SERIES"   $$$$  $$$$  $$$$  $$$$  $$$$M36.2884
 4163 IF ( NCHAIN .LE. 11 ) KBRSER = 1                                          
      N42 = LSTAT(22)                                                           
      N43 = LSTAT(23)                                                           
      IF ( KBRSER .NE. 2 )  GO TO 4165                                          
      WRITE (MUNIT6, 4164)                                              M36.2889
 4164 FORMAT ( '   @ @ @ @ @ @   Ok, guy, this is the',                 M36.2890
     1         ' "OVER12" break for "SERIES" usage.'   )                M36.2891
      CALL WINDOW                                                               
      KBRSER = 0                                                                
 4165 WRITE (PROM80, 4166)                                              M36.2893
 4166 FORMAT ( '  OPERATION (SHOW, EXTRA, CHANGE,',                     M36.2894
     1         ' STEP, REWIND) :'  )                                    M38.4435
      ASSIGN 4169 TO NEXTSN                                                     
      GO TO 9800                                                                
 4169 ANSI32 = BUFF77(1:32)                                                     
      IF ( ANSI32(1:6) .NE. 'REWIND' )  GO TO 4172                      M36.2899
      KSERLC = 0                                                                
      LSERLC = 0                                                                
      GO TO 4165                                                                
 4172 IF ( ANSI32(1:5) .NE. 'SHOW ' )  GO TO 4184                       M36.2903
      WRITE (MUNIT6, 4174)                                              M37.6589
 4174 FORMAT (  '  MEMORY:  LIST-2 ---',  2I5,                          M37.6590
     1     '     LIST-3 ---',  2I5,  '    LSERLC =',  I4  )             M37.6591
      CALL WINDOW                                                               
      WRITE (MUNIT6, 4175)  N42, LBRNCH, N43, LDATA, LSERLC             M36.2904
 4175 FORMAT ( ' num row use  bus-K  bus-M   name ????',                M38.4439
     1   5X, 9Hpresent-R,  5X, 9Hpresent-L,  5X, 9Hpresent-C  )         M38.4440
      CALL WINDOW                                                               
      DO 4180  J=1, LSERLC                                                      
      NDX1 = N42 + J                                                            
      NDX2 = N43 + J                                                            
      K = KBUS(NDX1)                                                            
      IF ( K .EQ. 0 )  GO TO 4180                                       M36.2913
      M = MBUS(NDX1)                                                            
      NDX3 = NAMEBR(NDX1)                                               M42.6780
      WRITE (MUNIT6, 4179)  J, LITYPE(NDX1), NR(NDX1), BUS(K),          M36.2915
     1      BUS(M), NAMEBR(NDX3),  CI(NDX1), CK(NDX1), CIK(NDX1)        M42.6781
 4179 FORMAT ( 1X, I3, 2I4, 3( 1X, A6 ), 5X, 3E13.4 )                   M36.2917
      CALL WINDOW                                                               
 4180 CONTINUE                                                                  
      GO TO 4165                                                                
 4184 IF ( ANSI32(1:5) .NE. 'EXTRA' )  GO TO 4197                       M36.2920
      WRITE (MUNIT6, 4187)                                              M36.2921
 4187 FORMAT ( 4X, 10Hstarting-R, 3X,  10Hstarting-L,                   M38.4441
     1        3X, 10Hstarting-C,                                        M38.4442
     2        7X, 6Hnext-R,  7X, 6Hnext-L,  7X, 6Hnext-C  )             M38.4443
      CALL WINDOW                                                               
      DO 4192  J=1, LSERLC                                                      
      NDX1 = N42 + J                                                            
      IF ( KBUS(NDX1) .EQ. 0 )  GO TO 4192                                      
      NDX2 = N43 + J                                                            
      WRITE (MUNIT6, 4191)  CKI(NDX1), CKKJM(NDX1), R(NDX2),            M36.2929
     1                      TR(NDX2), TX(NDX2), C(NDX2)                 M36.2930
 4191 FORMAT ( 1X, 6E13.4 )                                             M36.2931
      CALL WINDOW                                                               
 4192 CONTINUE                                                                  
      GO TO 4165                                                                
 4197 IF ( ANSI32(1:5) .NE. 'STEP ' )  GO TO 4211                       M36.2934
      KSERLC = 1                                                                
      GO TO 4165                                                                
 4211 IF ( ANSI32(1:6) .NE. 'CHANGE' )  GO TO 4165                              
 4214 WRITE (PROM80, 4215)                                              M36.2938
 4215 FORMAT ( '    CHANGE TYPE (DATA, MOVE, BLANK, USE, VALUE,',       M36.2939
     1         ' END) :'  )                                             M38.4444
      ASSIGN 34215 TO NEXTSN                                                    
      GO TO 9800                                                                
34215 ANSI32 = BUFF77(1:32)                                                     
      IF ( ANSI32(1:4) .EQ. 'END ' ) GO TO 4165                                 
      IF ( ANSI32(1:6) .EQ. 'SERIES')GO TO 4165                                 
      IF ( ANSI32(1:5) .NE. 'MOVE ' )  GO TO 4223                       M36.2946
 4216 WRITE (PROM80, 4217)                                              M36.2947
 4217 FORMAT ( '    SEND  N-from, N-to (1, 2, ...) :'  )                M36.2948
      ASSIGN 4219 TO NEXTSN                                                     
      GO TO 9800                                                                
 4219 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4) .EQ. 'END ' ) GO TO 4214                                 
      CALL FREIN2 ( BYTBUF, N13, N14 )                                          
      IF ( N14 .GT. LSERLC )  LSERLC = N14                                      
      N13 = N13 + N42                                                           
      N14 = N14 + N42                                                           
      KBUS(N14) =  KBUS(N13)                                                    
      MBUS(N14) =  MBUS(N13)                                                    
      N24 = 0                                                                   
      CALL NAMEA6 ( TEXT1, N24 )                                                
      NAMEBR(N14) = N24                                                         
      LITYPE(N14) = LITYPE(N13)                                                 
      IMODEL(N14) = IMODEL(N13)                                                 
      NR(N14) = NR(N13)                                                         
      CI(N14)  = CI(N13)                                                        
      CK(N14)  = CK(N13)                                                        
      CIK(N14) = CIK(N13)                                                       
      CKI(N14) = CKI(N13)                                                       
      CKKJM(N14) = CKKJM(N13)                                                   
      N13 = N13 - N42 + N43                                                     
      N14 = N14 - N42 + N43                                                     
      TR(N14) = TR(N13)                                                         
      TX(N14) = TX(N13)                                                         
      R(N14)  = R(N13)                                                          
      C(N14)  = C(N13)                                                          
      GO TO 4216                                                                
 4223 IF ( ANSI32(1:5) .NE. 'DATA ' )  GO TO 4232                       M36.2975
 4225 WRITE (PROM80, 4217)                                              M36.2976
      ASSIGN 4227 TO NEXTSN                                                     
      GO TO 9800                                                                
 4227 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4) .EQ. 'END ' ) GO TO 4214                                 
      CALL FREIN2 ( BYTBUF, N13, N14 )                                          
      IF ( N14 .GT. LSERLC )  LSERLC = N14                                      
      N14 = N14 + N42                                                           
      KBUS(N14) = IABS ( KBUS(N13) )                                            
      MBUS(N14) = IABS ( MBUS(N13) )                                            
      NR(N14) = 0                                                               
      N24 = 0                                                                   
      CALL NAMEA6 ( TEXT1, N24 )                                                
      NAMEBR(N14) = N24                                                         
      LITYPE(N14) = N13                                                         
      N13 = IABS ( NR(N13) )                                                    
      CKI(N14)   = TR(N13)                                                      
      CKKJM(N14) = TX(N13)                                                      
      CI(N14)    = TR(N13)                                                      
      CK(N14)    = TX(N13)                                                      
      CIK(N14)   = C(N13)                                                       
      N14 = N14 - N42 + N43                                                     
      TR(N14)    = TR(N13)                                                      
      TX(N14)    = TX(N13)                                                      
      C(N14)     = C(N13)                                                       
      R(N14)     = C(N13)                                                       
      GO TO 4225                                                                
 4232 IF ( ANSI32(1:5) .NE. 'BLANK' )  GO TO 4245                       M36.3001
 4235 WRITE (PROM80, 4236)                                              M36.3002
 4236 FORMAT ( '    SEND N-beg AND N-end LINE NUMBERS :'   )            M36.3003
      ASSIGN 4237 TO NEXTSN                                                     
      GO TO 9800                                                                
 4237 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4) .EQ. 'END ' ) GO TO 4214                                 
      CALL FREIN2 ( BYTBUF, N13, N14 )                                          
      IF ( N14 .LT. LSERLC )  GO TO 4239                                M36.3009
      LSERLC = N13 - 1                                                          
      GO TO 4235                                                                
 4239 DO 4241  J=N13, N14                                               M36.3012
 4241 KBUS(J+N42) = 0                                                           
      GO TO 4235                                                                
 4245 IF ( ANSI32(1:4) .NE. 'USE ' )  GO TO 4256                        M36.3015
      WRITE (MUNIT6, 4248)  ( NR(J+N42), J=1, LSERLC )                  M36.3016
 4248 FORMAT ( '    NR:',  10I7  )                                      M36.3017
      CALL WINDOW                                                               
 4251 WRITE (PROM80, 4253)                                              M36.3018
 4253 FORMAT ( '    SEND NUMBER TO TOGGLE (0=END) :'  )                 M36.3019
      ASSIGN 4254 TO NEXTSN                                                     
      GO TO 9800                                                                
 4254 IF ( BUFF77(1:4) .EQ. 'END ' ) GO TO 4214                                 
      CALL FREIN1 ( BUFF77, N6 )                                                
      IF ( N6 .LE. 0 )  GO TO 4214                                              
      N7 = N6 + N42                                                     M36.3026
      NR(N7) = NR(N7) + 1                                               M36.3027
      IF ( NR(N7) .GE. 2 )  NR(N7) = 0                                  M36.3028
      GO TO 4251                                                        M36.3029
C     PLACE ANY ADDITIONAL SUBCOMMANDS HERE:                            M36.3030
 4256 GO TO 4214                                                                
C     $$$$$$$  KEY WORD NO. 53:  "LOCK"     $$$$  $$$$  $$$$  $$$$  $$$$M36.3032
 4436 LOCKBR = 1                                                                
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 54:  "[Y]"      $$$$  $$$$  $$$$  $$$$  $$$$M36.3035
 4471 J = 1                                                                     
 4473 WRITE (PROM80, 4474)                                              M36.3037
 4474 FORMAT ( '   SEND ROW NAME OR NUMBER (END) :'  )                  M38.4461
      ASSIGN 54474 TO NEXTSN                                                    
      GO TO 9800                                                                
54474 PROM80 = BUFF77                                                           
      IF ( PROM80(1:4) .EQ. 'END ' ) GO TO 1240                                 
      IF ( PROM80(1:6) .NE. '      '  .AND.                             M36.3043
     1     PROM80(1:6) .NE. 'NEXT  ' )  GO TO 4475                      M36.3044
      J = J + 1                                                                 
      GO TO 4494                                                                
 4475 IF ( PROM80(1:4) .NE. 'TOP ' ) GO TO 4476                         M36.3047
      J = 2                                                                     
      GO TO 4494                                                                
 4476 IF ( PROM80(1:4) .NE. 'BOT ' ) GO TO  4477                        M36.3050
      J = -9999                                                                 
      GO TO 4494                                                                
 4477 DO 4478  J=1, 10                                                  M36.3053
      IF ( PROM80(1:1) .EQ. DIGIT(J) )  GO TO 4491                      M36.3054
 4478 CONTINUE                                                          M36.3055
      READ (PROM80, 1380)  TEXT2                                                
 1380 FORMAT ( A6 )                                                     M36.3057
      DO 4483  J=1, KPARTB                                                      
      IF ( TEXT2 .EQ. BUS(J) )  GO TO 4494                              M36.3059
 4483 CONTINUE                                                          M36.3060
 4486 WRITE (MUNIT6, 4487)                                              M36.3061
 4487 FORMAT ( '  %% %% == ++  Sorry, no such row.  Try again.' )       M36.3062
      CALL WINDOW                                                               
      GO TO 4473                                                        M36.3063
 4491 CALL FREIN1 ( PROM80, J )                                                 
 4494 IF ( ANSW80(1:3) .EQ. '[F]' ) GO TO 4515                                  
      IF ( J .LE. 1 )  J = KPARTB                                               
      IF ( J .GT. KPARTB )  J = 2                                               
      N17 = KKS(J) - 1                                                          
      N16 = KKS(J+1)                                                            
      IF ( J .EQ. KPARTB )  N16 = KKS(1)                                        
      WRITE (MUNIT6, 4496)  J, BUS(J), N16, N17                         M37.6601
 4496 FORMAT ( ' Row',  I4,  '   Name = ',  A6,  '   Limits =',         M38.4465
     1   2I5,  '    nonzero  (m, Ykm)  follow ....'  )                  M37.6603
      CALL WINDOW                                                               
      DO 4502  L=N16, N17, 4                                            M37.6605
      N23 = L + 3                                                               
      IF ( N23 .GT. N17 )  N23 = N17                                            
      WRITE (MUNIT6, 4497)   ( KM(M), YKM(M), M=L, N23 )                M37.6608
 4497 FORMAT ( 1X,  4( I5, E15.6 )  )                                   M37.6609
      CALL WINDOW                                                               
 4502 CONTINUE                                                          M37.6611
      GO TO 4473                                                                
C     $$$$$$$  KEY WORD NO. 55:  "[F]"      $$$$  $$$$  $$$$  $$$$  $$$$M36.3077
 4512 GO TO 4473                                                                
 4515 IF ( J .LE. 1 )  J = KPARTB                                               
      IF ( J .GT. KPARTB )  J = 2                                               
      N17 = KK(J-1) + 1                                                         
      WRITE (MUNIT6, 4521)  J, BUS(J), N17, KK(J)                       M37.6612
 4521 FORMAT ( ' Row',  I4,  '   Name = ',  A6,  '   Limits =',         M38.4466
     1   2I5,  '    nonzero  (m, Fkm)  follow ....'  )                  M37.6614
      CALL WINDOW                                                               
      DO 4524  L=N17, KK(J), 4                                          M37.6616
      N23 = L + 3                                                               
      IF ( N23 .GT. KK(J) )  N23 = KK(J)                                        
      WRITE (MUNIT6, 4497)   ( KM(M), YKM(M), M=L, N23 )                M37.6619
      CALL WINDOW                                                               
 4524 CONTINUE                                                          M37.6621
      GO TO 4512                                                                
C     $$$$$$$  KEY WORD NO. 56:  "NOROLL"   $$$$  $$$$  $$$$  $$$$  $$$$M36.3088
 4563 IF ( MONITS .EQ. 0 ) GO TO 4571                                           
      MONITS = 0                                                                
      WRITE (MUNIT6, 4566)                                                      
 4566 FORMAT ( 6X, '===  Cancel previous rolling printer plot.' )       M38.4467
      CALL WINDOW                                                               
 4571 IF ( MONITR .EQ. 0 ) GO TO 1240                                           
      MONITR = 0                                                                
      WRITE (MUNIT6, 4573)                                                      
 4573 FORMAT ( 6X, '===  Cancel previous rolling vector plot.' )        M38.4468
      CALL WINDOW                                                               
      GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 61:  "CHOICE"   $$$$  $$$$  $$$$  $$$$  $$$$M37.6624
 4592 N17 = NC - NV                                                             
      N24 = NUMNVO + NC + NSMOUT + IOUTCS + NUMOUT                              
      WRITE (MUNIT6, 4595)  N24                                         M37.6627
 4595 FORMAT ( '     Total number of EMTP outputs (sum of 5',           M37.6628
     1         ' classes) =',  I5   )                                   M37.6629
      CALL WINDOW                                                               
      WRITE (MUNIT6, 4596)                                              M38.4469
 4596 FORMAT ( '     Ordered class limits :   NUMNVO      NV',          M37.6631
     1         '   NC-NV  NSMOUT  IOUTCS   NUMOUT'  )                   M37.6632
      CALL WINDOW                                                               
      WRITE (MUNIT6, 4597) NUMNVO, NV, N17, NSMOUT, IOUTCS, NUMOUT      M37.6633
 4597 FORMAT ( 28X, 6I8 )                                               M37.6634
      CALL WINDOW                                                               
      WRITE (MUNIT6, 4608) NUMNVO                                       M37.6636
 4608 FORMAT ( '     Selective node voltage outputs (cells  1',         M37.6637
     1         '  through',  I3,  ' ) :'   )                            M37.6638
      CALL WINDOW                                                               
      DO 4611  J=1, NUMNVO, 7                                           M37.6640
      N17 = J + 6                                                               
      IF ( N17 .GT. NUMNV0 )  N17 = NUMNVO                                      
      WRITE (MUNIT6, 4609)  ( BUS(IBSOUT(M)), M=J, N17 )                M38.4473
 4609 FORMAT ( 7X, 10A7 )                                               M37.6642
      CALL WINDOW                                                               
 4611 CONTINUE                                                          M37.6644
      N7 = NUMNVO + 1                                                           
      N8 = NUMNVO + NV                                                          
      IF ( N8 .LT. N7 )  GO TO 4631                                             
      WRITE (MUNIT6, 4615)  N7, N8                                      M37.6648
 4615 FORMAT ( '     Branch voltage node-name pairs (cells',            M37.6649
     1                   I3,   '  through',  I3,  ' ) :' )              M37.6650
      CALL WINDOW                                                               
      N9 = 7878                                                                 
      CALL SPYOUT ( N9, N9 )                                                    
      DO 4623  J=1, NV                                                          
 4623 CALL SPYOUT ( IBRNCH(J), JBRNCH(J) )                                      
      N9 = -7878                                                                
      CALL SPYOUT ( N9, N9 )                                                    
 4631 N7 = NUMNVO + NV + 1                                                      
      N8 = NUMNVO + NC                                                          
      WRITE (MUNIT6, 4636)  N7, N8                                      M37.6660
 4636 FORMAT ( '     Element current node-name pairs (cells',           M37.6661
     1         I3,  '  through',  I3,  ' ) :'  )                        M37.6662
      CALL WINDOW                                                               
      IF ( N17 .LE. 0 ) GO TO 4658                                              
      N9 = 7878                                                                 
      CALL SPYOUT ( N9, N9 )                                                    
      DO 4639  J=1, KSWTCH                                                      
      IF ( KMSWIT(LSWTCH+J) .GT. 0 )  GO TO 4639                                
      CALL SPYOUT ( KMSWIT(J), KMSWIT(LSWTCH+J) )                               
 4639 CONTINUE                                                                  
      DO 4644  J=1, INONL                                                       
      IF ( NONLM(J) .GT. 0 )  GO TO 4644                                        
      CALL SPYOUT ( NONLK(J), NONLM(J) )                                        
 4644 CONTINUE                                                                  
      DO 4649  J=1, IBR                                                         
      IF ( MBUS(J) .GT. 0 )  GO TO 4649                                         
      CALL SPYOUT ( KBUS(J), MBUS(J) )                                          
 4649 CONTINUE                                                                  
      N9 = -7878                                                                
      CALL SPYOUT ( N9, N9 )                                                    
 4658 GO TO 1240                                                                
C     $$$$$$$  KEY WORD NO. 62:  "TACS"     $$$$  $$$$  $$$$  $$$$  $$$$M37.6682
 4716 WRITE (PROM80, 4717)                                              M37.6683
 4717 FORMAT ( ' SEND CONTROL (REWIND, SOURCE,',                        M37.6684
     1         ' SUPPLEMENTAL, PATCH, SHOW, END) :'  )                  M38.4474
      ASSIGN 4719 TO NEXTSN                                                     
      GO TO 9800                                                                
 4719 PROM80 = BUFF77                                                           
      IF ( PROM80(1:4) .EQ. 'END ' ) GO TO 1240                                 
      IF ( PROM80(1:6) .NE. 'REWIND' )  GO TO 4724                      M37.6690
      NIU = KONSCE                                                              
      NSUP = KONSUP                                                             
      KTAB = KOFSCE                                                     M37.6693
      KONCUR = 0                                                                
      GO TO 4716                                                                
 4724 IF ( PROM80(1:6) .NE. 'SOURCE' )  GO TO 4726                      M37.6696
      KONCUR = 1                                                                
      GO TO 4734                                                                
 4726 IF ( PROM80(1:6) .NE. 'SUPPLE' )  GO TO 4752                      M37.6699
      KONCUR = 2                                                                
 4734 N23 = LUNIT5                                                              
      LUNIT5 = MUNIT5                                                           
C     READ INPUT DATA CARD USING CIMAGE.                                M37.6703
 4738 CALL CIMAGE                                                               
      DECODE (80, 2788, ABUFF) ANSI8                                            
      IF ( ANSI8(1:4) .NE. 'END ' ) GO TO 4741                          M37.6706
      LUNIT5 = N23                                                              
      GO TO 4716                                                                
 4741 IF ( ANSI8(1:4) .NE. 'SPY ' ) GO TO 4745                          M37.6709
      LUNIT5 = N23                                                              
      GO TO 1240                                                                
 4745 IF ( KONCUR .EQ. 1 )  CALL TACS1                                          
      IF ( KONCUR .EQ. 2 )  CALL TACS1A                                         
      GO TO 4738                                                                
 4752 IF ( PROM80(1:6) .NE. 'PATCH ' )  GO TO 4792                      M37.6715
      N11 = LOCINT ( KAR1(1) )                                                  
      N12 = LOCINT ( VOLTI(1) )                                                 
      N13 = ( N12 - N11 ) / 2                                                   
      IF ( IPRSPY .LE. 1 )  GO TO 34753                                         
      WRITE (MUNIT6, 4753)  N11, N12, N13                               M38.4479
 4753 FORMAT ( ' VOLTI MEMORY INDICES.  N11, N12, N13 =',               M38.4480
     1          3I8,  '    FKAR1(J), J=N13+1, N13+9) ....'  )           M38.4481
      CALL WINDOW                                                               
      WRITE (MUNIT6, 24753)   (FKAR1(J), J=N13+1, N13+9)                M38.4483
24753 FORMAT ( 9E14.4 )                                                 M38.4484
      CALL WINDOW                                                               
34753 N14 = 0                                                                   
 4754 WRITE (PROM80, 4757)                                              M37.6723
 4757 FORMAT ( ' SEND SOURCE NAME, ADDRESS (A6, I6), OR "END":' )       M37.6724
      ASSIGN 4759 TO NEXTSN                                                     
      GO TO 9800                                                                
 4759 PROM80 = BUFF77                                                           
      IF ( PROM80(1:4) .EQ. 'END ' ) GO TO 4776                                 
      READ (PROM80, 4763)  BUS1, N7                                             
 4763 FORMAT ( A6, I6, E8.0 )                                           M37.6730
      DO 4769  J=1, NIU                                                         
      NDX3 = ILNTAB( KALIU + J )                                                
      BUS2 = TEXVEC(NDX3)                                                       
      IF ( BUS1 .NE. BUS2 ) GO TO 4769                                          
      N14 = N14 + 1                                                             
      NDX4 = J + KXTCS + NUK                                                    
      KONTAC(N14) = NDX4                                                        
      EPSKON(N14) = 0.0                                                         
      KONADD(N14) = N7                                                          
      GO TO 4754                                                                
 4769 CONTINUE                                                                  
      WRITE (MUNIT6, 4771)                                              M37.6742
 4771 FORMAT ( '  ? ? ?   Sorry, no such TACS source named  "',         M37.6743
     1         A6,  '".  Try again ....'  )                             M37.6744
      CALL WINDOW                                                               
      GO TO 4754                                                                
 4776 WRITE (PROM80, 4777)                                              M37.6747
 4777 FORMAT ( ' SEND USAGE NAME, ADDRESS, TOLERANCE',                  M37.6748
     1         ' (A6, I6, E8.0), OR "END":'  )                          M37.6749
      ASSIGN 4778 TO NEXTSN                                                     
      GO TO 9800                                                                
 4778 PROM80 = BUFF77                                                           
      IF ( PROM80(1:4) .NE. 'END ' ) GO TO 4780                                 
      KONTOT = N14                                                              
      GO TO 4716                                                                
 4780 READ (PROM80, 4763)  BUS1, N7, D13                                        
      DO 4785  J=1, KTAB                                                        
      NDX1 = KXTCS + J                                                          
      NDX2 = ILNTAB(KLNTAB+J)                                                   
      IF ( BUS1 .NE. TEXVEC(NDX2) ) GO TO 4785                                  
      IF ( KONCUR .EQ. 0 )  KONCUR = N14                                        
      N14 = N14 + 1                                                             
      KONTAC(N14) = NDX1                                                        
      KONADD(N14) = N7                                                          
      EPSKON(N14) = D13                                                         
      GO TO 4776                                                                
 4785 CONTINUE                                                                  
      WRITE (MUNIT6, 4788)                                              M37.6768
 4788 FORMAT ( '  ? ? ?   Sorry, no such TACS output named  "',         M37.6769
     1         A6,  '".  Try again ....'  )                             M37.6770
      CALL WINDOW                                                               
      GO TO 4776                                                                
 4792 IF ( PROM80(1:5) .NE. 'SHOW ' )  GO TO 4807                       M37.6773
      WRITE (MUNIT6, 4794) KONCUR, KONTOT                               M37.6774
 4794 FORMAT ( ' Concurrent sequential processing (CSP)',               M38.4497
     1   ' controls.  KONCUR =',  I4,  5X,  'KONTOT =', I4 )            M38.4498
      CALL WINDOW                                                               
      WRITE (MUNIT6, 4795)                                              M37.6778
 4795 FORMAT (                                                          M37.6779
     1   '     row    name   index  memory    threshold'  )             M38.4499
      CALL WINDOW                                                               
      DO 4801  I=1, KONTOT                                                      
      N13 = ILNTAB( KONTAC(I) + KLNTAB - KXTCS )                        M38.4500
      WRITE (MUNIT6, 4798)  I, TEXVEC(N13), KONTAC(I),                  M38.4501
     1                      KONADD(I), EPSKON(I)                        M37.6785
 4798 FORMAT ( I8, 2X, A6, 2I8, E13.4 )                                 M37.6786
      CALL WINDOW                                                               
 4801 CONTINUE                                                                  
C     ANY ADDITIONAL RESPONSES TO TACS PROMPT GO HERE:                  M37.6789
 4807 GO TO 4716                                                                
C     $$$$$$$  KEY WORD NO. 64:  "V-I"      $$$$  $$$$  $$$$  $$$$  $$$$M37.6791
 4823 N17 = 0                                                                   
      N8 = 0                                                                    
      WRITE (MUNIT6, 4825)  INONL, ICHAR                                M37.6794
 4825 FORMAT ( ' LISTS 9, 10 =',  2I4,                                  M37.6795
     1   '      (NEXT, <CR>, LAST, ALL, MODE).' )                       M38.4502
      CALL WINDOW                                                               
 4824 WRITE (PROM80, 4827)                                                      
 4827 FORMAT ( 1X, ' ROW        VCHAR        CCHAR       GSLOPE',       M37.6799
     1             ' ROW NLTYPE NONLAD NONLE  CLASS :'  )               M37.6800
      ASSIGN 4828 TO NEXTSN                                                     
      GO TO 9800                                                                
 4828 BYTBUF = BUFF77(1:20)                                                     
      IF ( BYTBUF(1:4)  .EQ.  'END ' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .EQ.  'STOP' )   GO TO 1240                             
      IF ( BYTBUF(1:4)  .NE.  'NEXT'  .AND.                             M37.6809
     1     BYTBUF(1:4)  .NE.  '    ' )  GO TO 4829                      M37.6810
      N17 = N17 + 1                                                             
      IF ( N17 .GT. INONL )  N17 = 1                                            
      GO TO 4844                                                                
 4829 IF ( BYTBUF(1:4) .NE. 'LAST' )  GO TO 4831                        M37.6814
      N17 = N17 - 1                                                             
      IF ( N17 .LE. 0 )  N17 = INONL                                            
      GO TO 4844                                                                
 4831 IF ( BYTBUF(1:4) .NE. 'ALL ' )  GO TO 4832                        M37.6818
      N1 = 1                                                                    
      N2 = ICHAR                                                                
      GO TO 4854                                                                
 4832 IF ( BYTBUF(1:4) .NE. 'MODE' )  GO TO 4834                        M37.6822
      N8 = N8 + 1                                                               
      IF ( N8 .GE. 2 )  N8 = 0                                                  
      WRITE (MUNIT6, 4833)  N8                                          M37.6825
 4833 FORMAT ( '    ----  New mode flag N8 =',  I2 )                    M38.4506
      CALL WINDOW                                                               
      GO TO 4824                                                                
 4834 CALL FREIN2 ( BYTBUF, N1, N2 )                                            
      IF ( N8 .EQ. 1 )  GO TO 4854                                              
      DO 4837  N17=1, INONL                                                     
      IF ( IABSZ(NONLAD(N17)) .LT. N1 )  GO TO 4837                             
      IF ( IABSZ(NONLAD(N17)) .LE. N2 )  GO TO 4844                             
 4837 CONTINUE                                                                  
      GO TO 4824                                                                
 4844 N1 = IABSZ ( NONLAD(N17) )                                                
      N2 = IABSZ ( NONLE(N17) )                                                 
      ANSI8(1:6) = 'PSEUDO'                                                     
      IF ( NLTYPE(N17) .GT. 0 )  ANSI8(1:6) = ' TRUE '                  M37.6839
      WRITE (MUNIT6, 4851) N1, VCHAR(N1), CCHAR(N1), GSLOPE(N1),        M37.6840
     1    N17, NLTYPE(N17), NONLAD(N17), NONLE(N17), ANSI8(1:6)         M37.6841
 4851 FORMAT ( 1X, I4, 3E13.4, I4, 2I7, I6, 1X, A6 )                    M37.6842
      CALL WINDOW                                                               
      N1 = N1 + 1                                                               
 4854 DO 4858  J=N1, N2                                                         
      WRITE (MUNIT6, 4851)  J, VCHAR(J), CCHAR(J), GSLOPE(J)            M37.6846
      CALL WINDOW                                                               
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 0 ) GO TO 4858                                           
      KWTSPY = 0                                                                
      GO TO 4824                                                                
 4858 CONTINUE                                                                  
      GO TO 4824                                                                
 1240 NEXMOD = 0                                                                
      GO TO 9804                                                                
 9800 NEXMOD = 3                                                                
 9804 IF ( IPRSPY .LT. 1 ) GO TO 9999                                           
      WRITE (MUNIT6, 9807) JJROLL, KBREAK, LOCKBR, NCHAIN,              M38.4512
     1                     PROM80(1:20)                                 M38.4513
 9807 FORMAT (  ' EXIT "SPYINK".  JJROLL, KBREAK, LOCKBR,',             M38.4514
     1      ' NCHAIN =',  4I5,  '   PROM80(1:20) =',  A20  )            M38.4515
      CALL WINDOW                                                               
 9999 RETURN                                                            M36.3107
      ENTRY SPYTAC                                                      M37.6859
C     CALLED ONLY BY "SUBTS3", ONCE, FOR TACS CSP APPLICATION.          M37.6860
      DO 3472  J=1, KONCUR                                                      
      N7 = KONTAC(J)                                                            
      N13 = KONADD(J)                                                           
 3472 XTCS(N7) = FKAR1(N13)                                                     
      CALL TACS3                                                                
      DO 3486  J=KONCUR+1, KONTOT                                               
      N7 = KONTAC(J)                                                            
      N13 = KONADD(J)                                                           
      D8 = FKAR1(N13) - XTCS(N7)                                                
      IF (ABSZ(D8) .LE. EPSKON(J)) GO TO 3486                                   
      FKAR1(N13) = XTCS(N7)                                                     
      IALTER = 1                                                                
 3486 CONTINUE                                                                  
      IF ( IPRSPY .LT. 1 ) GO TO 3496                                           
      WRITE (MUNIT6, 3493)  KONCUR, KONTOT, IALTER                      M37.6875
 3493 FORMAT ( ' EXIT "SPYTAC" AFTER CSP.  KONCUR,',                    M37.6876
     1         ' KONTOT, IALTER =',  3I5  )                             M37.6877
      CALL WINDOW                                                               
 3496 RETURN                                                            M37.6879
      END                                                               M36.3108
      SUBROUTINE INITSP                                                 M35.2801
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M35.2802
C     IF NO INTERACTIVE EMTP USE, THIS MODULE CAN BE DELETED.           M36.3109
C     UNIVERSAL INITIALIZATION MODULE FOR "SPYING".   CONSTANTS         M36.3110
C     MUST BE SET ONLY ONCE, AT THE BEGINNING OF EXECUTION ONLY.        M36.3111
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*8  TEXTAY(75)                                                   
C     NEXT COME ALL POSSIBLE KEY-WORD RESPONSES TO "SPY:" PROMPT:       M36.3113
      DATA  TEXTAY(1)   /  'HEADING '  /                                M36.3114
      DATA  TEXTAY(2)   /  'STOP    '  /                                M36.3115
      DATA  TEXTAY(3)   /  'PLOT    '  /                                M36.3116
      DATA  TEXTAY(4)   /  'HELP    '  /                                M36.3117
      DATA  TEXTAY(5)   /  'EXAMINE '  /                                M36.3118
      DATA  TEXTAY(6)   /  'DEPOSIT '  /                                M36.3119
      DATA  TEXTAY(7)   /  'SWITCH  '  /                                M36.3120
      DATA  TEXTAY(8)   /  'APPEND  '  /                                M39.3269
      DATA  TEXTAY(9)   /  'SAVE    '  /                                M36.3122
      DATA  TEXTAY(10)  /  'RESTORE '  /                                M36.3123
      DATA  TEXTAY(11)  /  'GO      '  /                                M36.3124
      DATA  TEXTAY(12)  /  'ECHO    '  /                                M38.4519
      DATA  TEXTAY(13)  /  'FIND    '  /                                M36.3126
      DATA  TEXTAY(14)  /  'LIST    '  /                                M36.3127
      DATA  TEXTAY(15)  /  'SPY     '  /                                M36.3128
      DATA  TEXTAY(16)  /  'BREAK   '  /                                M36.3129
      DATA  TEXTAY(17)  /  'WHEN    '  /                                M36.3130
      DATA  TEXTAY(18)  /  'COMMENT '  /                                M36.3131
      DATA  TEXTAY(19)  /  '@?      '  /                                M36.3132
      DATA  TEXTAY(20)  /  'ROLL    '  /                                M36.3133
      DATA  TEXTAY(21)  /  'TYPE?   '  /                                M36.3134
      DATA  TEXTAY(22)  /  'VERIFY  '  /                                M36.3135
      DATA  TEXTAY(23)  /  'FILES   '  /                                M36.3136
      DATA  TEXTAY(24)  /  'SLEEP   '  /                                M36.3137
      DATA  TEXTAY(25)  /  'SOURCE  '  /                                M36.3138
      DATA  TEXTAY(26)  /  'EDIT    '  /                                M36.3139
      DATA  TEXTAY(27)  /  'WAKE    '  /                                M36.3140
      DATA  TEXTAY(28)  /  'LANGUAGE'  /                                M36.3141
      DATA  TEXTAY(29)  /  'CATALOG '  /                                M36.3142
      DATA  TEXTAY(30)  /  'BEGIN   '  /                                M36.3143
      DATA  TEXTAY(31)  /  'STEP    '  /                                M36.3144
      DATA  TEXTAY(32)  /  'DEBUG   '  /                                M36.3145
      DATA  TEXTAY(33)  /  'DATA    '  /                                M36.3146
      DATA  TEXTAY(34)  /  'RAMP    '  /                                M36.3147
      DATA  TEXTAY(35)  /  'TIME    '  /                                M36.3148
      DATA  TEXTAY(36)  /  'TEK     '  /                                M36.3149
      DATA  TEXTAY(37)  /  'BRANCH  '  /                                M36.3150
      DATA  TEXTAY(38)  /  'YFORM   '  /                                M36.3151
      DATA  TEXTAY(39)  /  'NOY     '  /                                M36.3152
      DATA  TEXTAY(40)  /  'FACTOR  '  /                                M36.3153
      DATA  TEXTAY(41)  /  'NOF     '  /                                M36.3154
      DATA  TEXTAY(42)  /  'RLC     '  /                                M36.3155
      DATA  TEXTAY(43)  /  'WIDTH   '  /                                M36.3156
      DATA  TEXTAY(44)  /  'BUS     '  /                                M36.3157
      DATA  TEXTAY(45)  /  'SIZE    '  /                                M36.3158
      DATA  TEXTAY(46)  /  'LIMIT   '  /                                M36.3159
      DATA  TEXTAY(47)  /  'IOUT    '  /                                M36.3160
      DATA  TEXTAY(48)  /  'NODE    '  /                                M36.3161
      DATA  TEXTAY(49)  /  'NONLIN  '  /                                M36.3162
      DATA  TEXTAY(50)  /  'SPACE   '  /                                M36.3163
      DATA  TEXTAY(51)  /  'LUNIT4  '  /                                M36.3164
      DATA  TEXTAY(52)  /  'SERIES  '  /                                M36.3165
      DATA  TEXTAY(53)  /  'LOCK    '  /                                M36.3166
      DATA  TEXTAY(54)  /  '[Y]     '  /                                M36.3167
      DATA  TEXTAY(55)  /  '[F]     '  /                                M36.3168
      DATA  TEXTAY(56)  /  'NOROLL  '  /                                M36.3169
      DATA  TEXTAY(57)  /  'OPEN    '  /                                M36.3170
      DATA  TEXTAY(58)  /  'CLOSE   '  /                                M36.3171
      DATA  TEXTAY(59)  /  'SM      '  /                                M38.4520
      DATA  TEXTAY(60)  /  'HONK    '  /                                M36.3173
      DATA  TEXTAY(61)  /  'CHOICE  '  /                                M37.6880
      DATA  TEXTAY(62)  /  'TACS    '  /                                M37.6881
      DATA  TEXTAY(63)  /  'WAIT    '  /                                M37.6882
      DATA  TEXTAY(64)  /  'V-I     '  /                                M37.6883
      DATA  TEXTAY(65)  /  '        '  /                                        
      N3 = 29                                                                   
      CALL DIMENS ( MEMRMP(1), N3, BUS1, BUS1 )                                 
      LIMBUF = MEMRMP(2)                                                        
      IF ( IPRSPY .LT. 1 )  GO TO 1144                                          
      WRITE (MUNIT6, 1143)  LIMBUF                                      M37.6886
 1143 FORMAT ( ' NEAR TOP OF "INITSP".  LIMBUF =',  I8 )                M35.2808
      CALL WINDOW                                                               
 1144 LOCKBR = 1                                                                
      KBREAK = 0                                                                
      INDBUF = 0                                                                
      NOBACK = 0                                                                
      MONITR = 0                                                                
      MONITS = 0                                                                
      INCHLP = 0                                                                
      KSLOWR = 5                                                                
      MAXFLG = 1                                                                
      TBREAK = 8877.E33                                                         
      KSPSAV = 0                                                                
      NUMRMP = 0                                                                
      KBRSER = 0                                                                
      KSERLC = 0                                                                
      LSERLC = 0                                                                
      KOMADD = 0                                                                
      BLAN80 = ' '                                                              
      DO 1148  J=1, 9999                                                        
      IF ( TEXTAY(J) .EQ. '        ' )  GO TO 1152                      M36.3178
 1148 SPYKWD(J) = TEXTAY(J)                                                     
 1152 NUMKEY = J - 1                                                            
      NEXMOD = 0                                                                
      JUNKER = 'JUNK    '                                                       
      DIGIT(1) = '1'                                                    M35.2824
      DIGIT(2) = '2'                                                    M35.2825
      DIGIT(3) = '3'                                                    M35.2826
      DIGIT(4) = '4'                                                    M35.2827
      DIGIT(5) = '5'                                                    M35.2828
      DIGIT(6) = '6'                                                    M35.2829
      DIGIT(7) = '7'                                                    M35.2830
      DIGIT(8) = '8'                                                    M35.2831
      DIGIT(9) = '9'                                                    M35.2832
      DIGIT(10) = '0'                                                   M35.2833
      CALL LOCATN                                                               
      DO 2795  J=1, 9                                                           
      FILEXT(J) =  ' '                                                          
      WRITE (ANSI32, 1283)          J                                   M36.3181
 1283 FORMAT ( 7HINCLSPY,  I1 , 4H.DAT,  13X )                          M35.2837
      INQUIRE (FILE=ANSI32, EXIST=LOGVAR)                                       
      IF ( LOGVAR )  FILEXT(J) = 'X'                                            
      IF ( IPRSPY .LT. 1 )  GO TO 2795                                          
      WRITE (MUNIT6, 2789)  J, ANSI32                                   M37.6890
 2789 FORMAT ( ' NEXT USE OF "INQUIRE".  J, ANSI32 =',                  M36.3184
     1         I5, 1X, A32 )                                            M36.3185
      CALL WINDOW                                                               
 2795 CONTINUE                                                                  
      KWTSPY = 0                                                                
      JJROLL = 0                                                                
      TDROLL = 1.0                                                              
      KFILE5 = 0                                                                
      LIDNT1 = 1                                                                
      LIDNT2 = 1                                                                
      LINSPN = 1                                                                
      RETURN                                                            M35.2849
      END                                                               M35.2850
      SUBROUTINE FLAGER                                                 M35.2851
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M35.2852
C     IF NO INTERACTIVE USE, CONVERT TO DUMMY MODULE ("RETURN").        M35.2853
C       VAX-11 INSTALLATION-DEPENDENT EMTP MODULE WHICH SERVES          M35.2854
C       TO READ SPY COMMAND FROM MUNIT5 IF: 1) CTRL-C INTERRUPT         M38.4529
C       HAS OCCURRED, OR 2) IF  LOCKBR = 1  UPON ENTRY.                 M38.4530
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      COMMON /COMKWT/  KWTVAX    ! MAGIC BLOCK FOR CTRL-C TRAPPING      M35.2857
      DIMENSION  IDUM(3)   !  DUMMY VECTOR FOR CTRL-C HANDLING          M35.2858
      EXTERNAL KWITER       ! NEEDED FOR CTRL-C INITIALIZATION          M35.2859
      IF ( IPRSPY .LT. 10 ) GO TO 3456  ! JUMP AROUND DIAGNOSTIC        M37.6893
      WRITE (MUNIT6, 3409)  ISTEP, KWTSPY, ITYPE, LASTOV                M38.4531
 3409 FORMAT ( ' TOP FLAGER.  ISTEP, KWTSPY, ITYPE, LASTOV =',          M38.4532
     1         4I6  )                                                   M38.4533
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37.6896
 3456 IF ( LASTOV .NE. 9911 )  GO TO 3614   ! NOT "PLTFIL" OVERFLOW     M38.4534
      BUFF77 = 'SPACE'   ! NEXT SPY COMMAND WE WANT TO EXECUTE          M38.4535
      KWTSPY = 1    ! SET FLAG FOR GLOBAL EMTP USE (REMEMBER)           M38.4536
      KFILE5 = 2     ! SIGNAL TO "SPYING" THAT BUFF77 NOW READ          M38.4537
      GO TO 3651        ! EXIT MODULE AFTER POSSIBLE DIAGNOSTIC         M38.4538
 3614 IF ( KWTVAX .EQ. 0 ) GO TO 3642  ! NO USER-KEYED INTERRUPT        M38.4539
      WRITE (PROM80, 3462)      ! PROMPT FOR SPY KEYBOARD INPUT         M38.4540
 3462 FORMAT ( ' SPY:' )                                                M38.4541
      CALL PROMPT   ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)          M38.4542
      KWTVAX = 0    ! RESET FLAG OF CTRL-C INTERCEPTION FOR NEXT        M35.2864
C     CALL ENABLE_CTRL_C ( KWITER, IDUM(1) )  ! RE-ENABLE IT            M35.2865
      IF ( IPRSPY .LT. 1 )  GO TO 3491  ! JUMP AROUND DIAGNOSTIC        M38.4543
      WRITE (MUNIT6, 3487)                                              M37.6899
 3487 FORMAT ( ' ENABLE VAX CTRL-C INTERCEPTION IN "FLAGER".'  )        M35.2868
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37.6900
 3491 GO TO 3643     ! JUMP TO READ FROM UNIT MUNIT5 (KEYBOARD)         M38.4544
 3642 IF ( LOCKBR .NE. 1 )  GO TO 3651        ! NO FORCED INPUT         M38.4545
      IF ( KFILE5 .EQ. 1 )  GO TO 3651  ! "@" READ OUT IN EMTSPY        M38.4546
 3643 READ (MUNIT5, 3647)  BUFF77         ! READ NEXT SPY INPUT         M38.4547
 3647 FORMAT ( A80 )                                                    M38.4548
      KWTSPY = 1    ! SET FLAG FOR GLOBAL EMTP USE (REMEMBER)           M38.4549
      KFILE5 = 2     ! SIGNAL TO "SPYING" THAT BUFF77 NOW READ          M38.4550
      IF ( KSPSAV .EQ. 0 )  GO TO 3650  ! NO ECHOING OF SPY COMMAND     M38.4551
      IF ( BUFF77(1:7) .NE. 'CANCEL ' )  GO TO 3648                     M38.4552
      KSPSAV = KSPSAV + 1   ! ERASE PREVIOUS, ERRONEOUS COMMAND         M38.4553
      GO TO 3643      ! LOOP BACK FOR ANOTHER, A REAL, COMMAND          M38.4554
 3648 KSPSAV = KSPSAV - 1  ! NEXT FILE6 CELL TO SAVE BUFF77 IN          M38.4555
      IF ( KSPSAV .GT. NUMCRD )  GO TO 3649   ! NO OVERLAP YET          M38.4556
      WRITE (*, *)  ' ERROR STOP; OVERFLOW IN "FLAGER".'                M38.4557
      CALL STOPTP     ! INSTALLATION-DEPENDENT FORTRAN STOP             M38.4558
 3649 FILE6(KSPSAV) = BUFF77   ! ACCUMULATE USER-KEYED SPY INPUT        M38.4559
 3650 GO TO 3651    ! EXIT MODULE WITH 80-COL. BUFF77 CARD NOW READ     M38.4560
 3651 IF ( IPRSPY .LT. 9 )  GO TO 9000                                  M38.4561
      WRITE (MUNIT6, 9004)  KWTSPY, BUFF77                              M38.4562
 9004 FORMAT ( ' EXIT "FLAGER".  KWTSPY =',  I4,                        M38.4563
     1         '   BUFF77 = ',  A80  )                                  M38.4564
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M38.4565
 9000 RETURN                                                            M35.2869
      END                                                               M38.4566
      SUBROUTINE QUITER                                                 M38.4567
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M38.4568
C     IF NO INTERACTIVE USE, CONVERT TO DUMMY MODULE ("RETURN").        M38.4569
C     THIS MODULE PROVIDES A SPECIAL-PURPOSE CONNECTION TO THE          M38.4570
C     MORE GENERAL "FLAGER".  HERE, WE ONLY WANT TO SENSE A             M38.4571
C     USER-KEYED INTERRUPT (NO SPY INPUT IS TO BE READ).                M38.4572
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      N24 = LOCKBR   ! SAVE CURRENT TIME-SLICING FLAG VALUE             M38.4574
      N25 = KFILE5   ! SAVE CURRENT STATUS OF INPUT CONNECTION          M38.4575
      LOCKBR = 0         ! TEMPORARILY TURN TIME-SHARING ON             M38.4576
      CALL FLAGER    ! CHECK FOR USER-KEYED INTERRUPT, AND RETURN       M38.4577
      LOCKBR = N24   ! RESTORE ORIGINAL VALUE OF B4 FLAGER USE          M38.4578
      KFILE5 = N25   ! RESTORE ORIGINAL VALUE OF B4 FLAGER USE          M38.4579
      RETURN                                                            M38.4580
      END                                                               M35.2870
      SUBROUTINE HONKER ( KLEVEL )                                      M36.3186
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M36.3187
C     IF NO INTERACTIVE USE, THIS MODULE CAN BE DELETED.                M36.3188
C       VAX-11 INSTALLATION-DEPENDENT EMTP MODULE WHICH ISSUES          M36.3189
C       AN AUDIBLE ALARM (VIA A TERMINAL SPEAKER) OF INTENSITY          M36.3190
C       CONTROLLED BY ARGUMENT KLEVEL.  THIS IS ON A SCALE OF           M36.3191
C       ONE TO TEN, WITH ZERO MEANING NO NOISE AT ALL, AND TEN          M36.3192
C       CORRESPONDING TO A DISASTER ALERT (E.G., AIR RAID SIRENS).      M36.3193
C     THIS VAX-11 MODULE IS DESIGNED FOR DEC VT100 TERMINAL.  THE       M37.6901
C     IDEA IS BY ALBERT H. SCHMIDT, BPA ROUTE EOGA.  AT THE KEYBOARD,   M37.6902
C     CTRL-G WILL RING THE BELL, AND THE ASCII CHARACTER 7 IS           M37.6903
C     EQUIVALENT WITHIN A PROGRAM.  SHORTAGE OF 1 BELL IS DUE TO        M37.6904
C     CARRIAGE CONTROL, MAYBE (SEE N8 = J + 1, RATHER THAN J, BELOW).   M37.6905
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      REAL*8        SPYTIM(2), SPDATE(2)                                M37.6908
      D13 = 2.0           ! INITIALIZE TIME DELAY AT TWO SECONDS        M37.6909
      CALL TIME44 ( SPYTIM(1) )    ! EMTP WALL-CLOCK TIME               M37.6910
      CALL DATE44 ( SPDATE(1) )    ! EMTP DATE UTILITY                  M37.6911
      BUFF77(1:1) = CHAR(7)  ! ASCII "7" = CTRL-G OF KEYBOARD = 1 BELL  M37.6912
      BUFF77(2:2) = BUFF77(1:1)     ! DEFINE 2ND BELL, IF NEEDED        M37.6913
      BUFF77(3:3) = BUFF77(1:1)     ! DEFINE 3RD BELL, IF NEEDED        M37.6914
      BUFF77(4:6) = BUFF77(1:3)     ! DEFINE BELLS NUMBER 4 TO 6        M37.6915
      BUFF77(7:10) = BUFF77(1:4)      ! DEFINE BELLS NUMBER 7-10        M37.6916
 3608 WRITE (MUNIT6, 3627) SPYTIM, SPDATE, KLEVEL, BUFF77(1:KLEVEL)     M37.6917
 3627 FORMAT ( ' AUDIBLE ALARM BEGAN AT  ',  2A4,  2X,  2A4,            M37.6918
     1         5X,  I3,  ' BELLS.',  A )                                M37.6919
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37.6920
      IF ( KLEVEL .LT. 10 ) GO TO 9000  ! EXIT MODULE (NO LOOP)         M37.6921
      CALL TDELAY ( D13 )           ! NOW STALL FOR D13 SECONDS         M37.6922
      IF ( D13 .EQ. -7654 ) GO TO 9000   ! INTERRUPT IN "TDELAY"        M37.6923
      CALL FLAGER               ! CHECK FOR USER-KEYED INTERRUPT SIGNAL M37.6924
      IF ( KWTSPY .EQ. 0 ) GO TO 3643   ! NO USER ABORT OF ALARM        M37.6925
      KWTSPY = 0        ! RESET INTERRUPT INDICATOR AS WE BEGIN SERVICE M37.6926
      GO TO 9000                     ! JUMP OUT OF LOOP (ABORT HONKING) M37.6927
 3643 D13 = 1.5 * D13   ! LENGTHEN DELAY AFTER NEXT TEN BELLS           M37.6928
      GO TO 3608       ! LOOP BACK TO REPEAT STRING OF 10 BELLS         M37.6929
 9000 RETURN                                                            M37.6930
      END                                                               M37.6931
      SUBROUTINE TDELAY ( D8 )                                          M37.6932
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M37.6933
C     IF NO INTERACTIVE USE, THIS MODULE CAN BE DELETED.                M37.6934
C       VAX-11   MODULE DESIGNED TO STALL FOR  D8  SECONDS.             M37.6935
C       PRESENT USE OF DISK WRITES IS TEMPORARY ONLY, AND SHOULD        M37.6936
C       LATER BE REPLACED BY A LESS-WASTEFUL, TRUE HIBERNATION.         M37.6937
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      N23 = D8    ! INTEGER NUMBER OF SECONDS FOR HIBERNATION           M37.6939
      DO 3641  J=1, N23    ! LOOP ONCE FOR EACH SECOND OF DELAY         M37.6940
      DO 3624  K=1, 6      ! ONE SECOND = ABOUT 6 WRITES TO DISK        M37.6941
      REWIND 36             ! REWIND UNUSED, DUMMY I/O CHANNEL 36       M37.6942
 3624 WRITE (36, 3629)   K                 ! DUMMY WRITE TO DISK        M37.6943
 3629 FORMAT ( I2 )                                                     M37.6944
      CALL QUITER       ! CHECK FOR USER-KEYED INTERRUPT SIGNAL         M38.4581
      IF ( KWTSPY .EQ. 0 ) GO TO 3641   ! NO USER ABORT OF ALARM        M37.6946
      KWTSPY = 0        ! RESET INTERRUPT INDICATOR AS WE BEGIN SERVICE M37.6947
      D8 = -7654  ! ARGUMENT FLAG REMEMBERING ABORT (FOR HONKER)        M37.6948
      GO TO 9000        ! JUMP OUT OF TIME-DELAY LOOP, TO RETURN        M37.6949
 3641 CONTINUE                 ! END  DO 3641  LOOP OVER SECONDS        M37.6950
 9000 RETURN                                                            M37.6951
      END                                                               M36.3196
      SUBROUTINE KWITER ( IDUM )                                        M35.2871
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C       VAX-11  INSTALLATION-DEPENDENT EMTP MODULE WHICH SERVES         M35.2872
C       CONTROL INTERACTIVE USAGE.  IF NONE, DESTROY THE MODULE.        M35.2873
C       PURPOSE IS TO SENSE USER-KEYED INTERRUPT, AND SET FLAG.         M35.2874
C       NAME "COMKWT" IS RESERVED (CONNECTED TO "CONTROLC.OBJ")         M35.2875
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      COMMON /COMKWT/  KWTVAX                                                   
      DIMENSION  IDUM(3)                                                M35.2878
      KWTVAX = 1                                                                
      RETURN                                                            M35.2880
      END                                                               M35.2881
      SUBROUTINE PERCNT ( VBYTE, N7 )                                   M35.3915
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M35.3916
C     UTILITY WHICH SERVES TO REPLACE  "%%%%%%%%"  STRINGS OF DISK      M35.3917
C     FILES BY PARAMETERS OF  "@?"  CALL.   COLUMNS 1, ... N7  ARE      M35.3918
C     SEARCHED, OF CHARACTER VECTOR  "VBYTE" .                          M35.3919
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.3920
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*1  VBYTE(1)                                             M35.3922
      DO 1297  K=1, N7                                                          
      IF ( VBYTE(K)  .NE.  '%' )   GO TO 1297                                   
      DO 1253  J=1, 7                                                           
      IF ( VBYTE(K+J) .NE.  '%' )   GO TO 1297                                  
 1253 CONTINUE                                                          M35.3928
C     WE EXIT  DO 1253  WITH STRING OF 8 "%" BEGINNING IN COLUMN K      M35.3929
      ITEXP = ITEXP + 1                                                         
      IF ( ITEXP  .LE.  MAXARG )   GO TO 1284                                   
      WRITE (MUNIT6, 1274)  MAXARG                                      M37.7025
 1274 FORMAT ( ' ????  TROUBLE.  "@?"  USAGE ONLY DEFINED',             M37.7026
     1    I4,  '   ARGUMENTS, WHILE THE DISK'     )                     M37.7027
      CALL WINDOW                                                               
      WRITE (MUNIT6, 1275)                                              M37.7029
 1275 FORMAT (  '                 FILE HAS MORE  %-STRINGS.',           M37.7030
     1          '   TROUBLE DETECTED IN FOLLOWING:'   )                 M37.7031
      CALL WINDOW                                                               
      WRITE (MUNIT6, 1277)  ( VBYTE(I), I=1, N7 )                       M37.7033
 1277 FORMAT ( '              >>>',  80A1  )                            M37.7034
      CALL WINDOW                                                               
      KILPER = 1                                                                
      GO TO 1313                                                                
 1284 ANSI8 = TEXPAR(ITEXP)                                             M36.3383
      READ (ANSI8, 1296)  ( VBYTE(K+J-1), J=1, 8 )                      M36.3384
 1296 FORMAT ( 80A1 )                                                   M35.3940
 1297 CONTINUE                                                          M35.3941
      IF ( KVERFY .NE. 0 )  GO TO 1313                                          
      WRITE (MUNIT6, 1306)  ( VBYTE(J), J=1, N7 )                       M37.7038
 1306 FORMAT ( ' @>>>',  80A1 )                                         M35.3944
      CALL WINDOW                                                               
 1313 RETURN                                                            M37.7040
      END                                                               M35.3946
      SUBROUTINE NUMCHK ( VBYTE, NCHAR, KILL )                          M35.3947
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M35.3948
C     THIS UTILITY SERVES TO SCRUTINIZE THE INPUT CHARACTER STRING      M35.3949
C     (VBYTE(J), J=1, NCHAR)  TO SEE IF IT IS A LEGAL FLOATING-POINT    M35.3950
C     NUMBER.   IF SO,  "KILL"  IS TO BE SET TO ZERO;  IF NUMBER IS     M35.3951
C     STRUCTURALLY DEFICIENT,  "KILL"  IS TO BE SET POSITIVE.           M35.3952
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.3953
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*1   VBYTE(1)                                            M35.3955
      KILL = 0                                                                  
      KOLDIG = 0                                                                
      NPER = 0                                                                  
      NSIGN = 0                                                                 
      NUME = 0                                                                  
      KK = 0                                                                    
      DO 3481  I=1, NCHAR                                               M35.3962
      IF ( VBYTE(I)  .EQ.  ' '  )  GO TO 3481                                   
      KK = KK + 1                                                               
      IF ( VBYTE(I)  .NE.  '+'     .AND.                                M35.3965
     1     VBYTE(I)  .NE.  '-'  )   GO TO 3412                          M35.3966
C     FOLLOWING CODE CONSIDERS RAMIFICATIONS OF JUST-FOUND "+" OR "-" : M35.3967
      NSIGN = NSIGN + 1                                                 M35.3968
      IF ( NSIGN  .GT.  2 )   KILL = 1                                  M35.3969
      IF ( KK  .EQ.  1 )   GO TO 3481                                           
      IF ( KOLEXP  .EQ.  I-1 )   GO TO 3481                                     
      KILL = 1                                                                  
      GO TO 3481                                                        M35.3973
 3412 IF ( VBYTE(I)  .NE.  'E'     .AND.                                M35.3974
     1     VBYTE(I)  .NE.  'D'  )   GO TO 3425                          M35.3975
C     FOLLOWING CODE CONSIDERS RAMIFICATIONS OF JUST-FOUND "D" OR "E":  M35.3976
      NUME = NUME + 1                                                           
      KOLEXP = I                                                                
      IF ( NUME .GT. 1 )   KILL = 1                                             
      GO TO 3481                                                        M35.3980
 3425 IF ( VBYTE(I)  .NE.  '.' )   GO TO 3428                           M35.3981
C     FOLLOWING CODE CONSIDERS RAMIFICATIONS OF JUST-FOUND DECIMAL POINTM35.3982
      NPER = NPER + 1                                                   M35.3983
      IF ( NPER .GT. 1 )  KILL = 1                                              
      KOLPER = I                                                                
      GO TO 3481                                                        M35.3986
 3428 DO 3431  J=1, 10                                                          
      IF ( VBYTE(I)  .EQ.  DIGIT(J) )   GO TO 3438                              
 3431 CONTINUE                                                                  
      KILL = 1                                                                  
      GO TO 3481                                                        M35.3991
C     FOLLOWING CODE CONSIDERS RAMIFICATIONS OF JUST-FOUND DIGIT:       M35.3992
 3438 KOLDIG = I                                                                
 3481 CONTINUE                                                          M35.3994
      IF ( NUME  .EQ.  1     .AND.                                              
     1     KOLEXP  .GT.  KOLDIG )                                               
     2 KILL = 1                                                                 
      IF ( NUME  .EQ.  1     .AND.                                              
     1   KOLPER  .GT.  KOLEXP )                                                 
     2 KILL = 1                                                                 
      IF ( KILL  .EQ.  0 )   RETURN                                             
      WRITE (MUNIT6, 3492)  ( VBYTE(J), J=1, NCHAR )                            
 3492 FORMAT ( ' ??? SORRY, ILLEGAL NUMERIC JUST READ:',  50A1 )        M35.4003
      WRITE (MUNIT6, 3493)                                              M35.4004
 3493 FORMAT ( '            MAKE A SECOND TRY, PLEASE ....'  )          M35.4005
      RETURN                                                            M35.4006
      END                                                               M35.4007
      SUBROUTINE GETNUM ( NUM )                                         M35.4008
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M35.4009
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4010
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*1   C4                                                  M35.4012
      IF ( IPRSPY .LT. 1 )  GO TO 4204                                          
      WRITE (MUNIT6, 4203)  IBEGCL, BYTBUF(IBEGCL:IBEGCL)               M37.7042
 4203 FORMAT ( ' BEGIN  "GETNUM".   IBEGCL =',                          M37.7043
     1   I5,   '      BYTBUF(IBEGCL) =',  A1 )                          M37.7044
      CALL WINDOW                                                               
 4204 N1 = 1                                                            M37.7046
      NUM = 0                                                           M35.4018
      DO 4265  I=IBEGCL, 20                                             M35.4019
      C4 = BYTBUF(I:I)                                                  M35.4020
      IF ( C4  .EQ.  ' ' )   GO TO 4286                                 M35.4021
      IF ( C4  .EQ.  ':'  )   GO TO 4286                                M35.4022
      IF ( C4  .EQ.  '#' )   GO TO 4286                                 M36.3385
      IF ( C4  .EQ.  ','  )   GO TO 4286                                M35.4023
      IF ( C4  .EQ.  '+'  )   GO TO 4265                                M35.4024
      IF ( C4  .NE.  '-'  )   GO TO 4218                                M35.4025
      N1 = -1                                                           M35.4026
      GO TO 4265                                                        M35.4027
 4218 DO 4234  J=1, 10                                                  M35.4028
      IF ( C4  .EQ.  DIGIT(J) )   GO TO 4256                            M35.4029
 4234 CONTINUE                                                          M35.4030
      WRITE (MUNIT6, 4239)  I, C4                                       M35.4031
 4239 FORMAT (  '    -- Illegal byte in "NUMBER".',                     M35.4032
     1  1X,  I4,  3X,  A1,  3X,  'Try again ...'  )                     M35.4033
      CALL WINDOW                                                               
      NUM = -87654                                                      M35.4034
      GO TO 4294                                                                
 4256 IF ( J  .EQ.  10 )   J = 0                                        M35.4036
      NUM = 10 * NUM  +  J                                              M35.4037
      IF ( IPRSPY .LT. 2 )  GO TO 4265                                          
      WRITE (MUNIT6, 4259)  J, NUM                                      M37.7050
 4259 FORMAT ( ' NEXT DIGIT.  J, NUM =',  2I8 )                         M36.3387
      CALL WINDOW                                                               
 4265 CONTINUE                                                          M35.4039
      WRITE (MUNIT6, 4271)  NUM                                         M36.3388
 4271 FORMAT ( ' STOP AT 4271 OF "GETNUM".  NUM =',  I8 )               M36.3389
      CALL WINDOW                                                               
      CALL STOPTP                                                               
 4286 IF ( N1  .LT.  0 )   NUM = -NUM                                   M35.4041
      IENDCL = I - 1                                                    M35.4042
      IF ( IPRSPY .LT. 1 )  GO TO 4294                                          
      WRITE (MUNIT6, 4293)  IENDCL, NUM                                 M37.7055
 4293 FORMAT (  ' EXIT "NUMBER".   IENDCL, NUM =',  2I8  )              M36.3390
      CALL WINDOW                                                               
 4294 RETURN                                                            M37.7057
      END                                                               M35.4046
      SUBROUTINE MOVERS ( FROM, TO, NUM )                               M35.4047
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M35.4048
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4049
      CHARACTER*1   FROM(1), TO(1)                                      M35.4050
      DO 1763  J=1, NUM                                                 M35.4051
 1763 TO(J) = FROM(J)                                                   M35.4052
      RETURN                                                            M35.4053
      END                                                               M35.4054
      SUBROUTINE MOVERL ( FROM, TO, NUM )                               M35.4055
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M35.4056
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4057
      REAL*8   FROM(1), TO(1)                                           M35.4058
      DO 1763  J=1, NUM                                                 M35.4059
 1763 TO(J) = FROM(J)                                                   M35.4060
      RETURN                                                            M35.4061
      END                                                               M35.4062
      SUBROUTINE WINDOW                                                 M37.7058
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M37.7059
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M37.7060
C     FOR CHARACTER*132 SPY DISPLAY CHANNEL MUNIT6, THIS SERVES         M37.7061
C     TO ACTUALLY OUTPUT THE LINE TO THE SPY DISPLAY DEVICE.            M37.7062
C       VAX-11  INSTALLATION-DEPENDENT EMTP MODULE.                     M37.7063
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
C     TEMPORARILY, UNTIL WE LEARN HOW TO WRITE TO A 2ND CRT FOR         M37.7066
C     VAX/VMS, WE WILL JUST WRITE TO LUNIT6:                            M37.7067
      IF ( IABS(KVERFY) .EQ. 34543 ) GO TO 9000  ! NO SPY WINDOWS       M38.4582
      DO 5614  J=1, 132   ! SEARCH LINE FOR RIGHT MOST NON-BLANK        M37.7068
      K = 133 - J      ! REVERSE INDEX (STEP FROM RIGHT TO LEFT)        M37.7069
      IF ( MUNIT6(K:K) .NE. ' ' )  GO TO 5621      ! END OF LINE        M37.7070
 5614 CONTINUE   ! END  DO 5614  LOOP TO FIND RIGHT EDGE OF LINE        M37.7071
 5621 WRITE (LUNIT6, 5624)  MUNIT6(1:K)   ! OUTPUT NONBLANK PART        M37.7072
 5624 FORMAT ( A )                                                      M37.7073
 9000 RETURN                                                            M38.4583
      END                                                               M37.7074
      SUBROUTINE SPYLIN                                                 M37.7075
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M37.7076
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M37.7077
C     ONE BLANK LINE IS WRITTEN ON SPY SCREEN BY THIS MODULE.           M37.7078
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
C     TEMPORARILY, UNTIL WE LEARN HOW TO WRITE TO A 2ND CRT FOR         M37.7081
C     VAX/VMS, WE WILL JUST WRITE TO LUNIT6 IN UNIVERSAL FORM:          M37.7082
 5621 WRITE (LUNIT6, 5624)                                              M37.7083
 5624 FORMAT ( 1X )                                                     M37.7084
      END                                                               M37.7085
      SUBROUTINE SPYOUT ( N1, N2 )                                      M37.7086
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M37.7087
C     IF NO INTERACTIVE USE, THIS MODULE CAN BE DELETED.                M37.7088
C     ARGUMENTS N1 AND N2 ARE NODE NOS., WITH POSSIBLE "-" SIGNS.       M37.7089
C     PURPOSE IS TO LOAD AND PRINT CORRESPONDING NAMES FOR "OUTPUT"     M37.7090
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      SAVE                                                                      
      REAL*8         TEXT10(10), TERRA                                  M37.7094
      DATA  TERRA  / 6HTERRA  /                                                 
      N11 = N1                                                                  
      N12 = N2                                                                  
C     FIRST CHECK FOR SPECIAL INITIALIZATION OR FLUSHING CALLS:         M37.7098
      IF ( N11 .NE. 7878 ) GO TO 4618                                           
      K = 0                                                                     
      GO TO 9000                                                                
 4618 IF ( N11 .NE. -7878 ) GO TO 4626                                          
 4621 IF ( K .LE. 0 )  GO TO 9000                                               
      WRITE (MUNIT6, 4629) (TEXT10(J), J=1, K)                                  
 4629 FORMAT ( 7X, 10A7 )                                               M37.7104
      CALL WINDOW                                                               
      K = 0                                                                     
      GO TO 9000                                                                
C     INSERT FIRST NODE NAME INTO BUFFER:                               M37.7108
 4626 IF ( N11 .LT. 0 ) N11 = -N11                                              
      K = K + 1                                                                 
      TEXT10(K) = BUS(N11)                                                      
      IF ( N11 .EQ. 1 ) TEXT10(K) = TERRA                                       
      IF ( K .EQ. 10 )  GO TO 4621                                              
      IF ( N12 .EQ. 0 ) GO TO 9000                                              
      IF ( N12 .LT. 0 ) N12 = -N12                                              
      K = K + 1                                                                 
      TEXT10(K) = BUS(N12)                                                      
      IF ( N12 .EQ. 1 ) TEXT10(K) = TERRA                                       
      IF ( K .EQ. 10 )  GO TO 4621                                              
 9000 IF ( IPRSPY .LT. 3 )  GO TO 9007                                          
      WRITE (MUNIT6, 9004)  N1, N2, K, N11, N12                         M37.7121
 9004 FORMAT ( ' EXIT "SPYOUT".  N1, N2, K, N11, N12 =', 5I6 )          M37.7122
      CALL WINDOW                                                               
 9007 RETURN                                                                    
      END                                                               M37.7125
      SUBROUTINE EXAMIN                                                 M37.7126
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M37.7127
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M37.7128
C     THIS NEAR-UNIVERSAL MODULE SERVES TO BUILD THE CHARACTER*132      M37.7129
C     OUTPUT VECTOR OUTLIN OF THE "EXAMINE" COMMAND OF SPY.             M37.7130
C     COMPUTERS WITH INDEX PROBLEMS (E.G., PRIME) NEED REPLACEMENT      M37.7131
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      IF ( IPRSPY .LT. 1 ) GO TO 1718                                           
      WRITE (MUNIT6, 1707)  NUMEX, IMIN(1), LOCOUT(1)                   M37.7134
 1707 FORMAT (' TOP OF "EXAMIN".  NUMEX, IMIN(1), LOCOUT(1) =', 3I6 )   M37.7135
      CALL WINDOW                                                               
 1718 JJ = 0                                                                    
      OUTLIN(1:1) = ' '                                                         
      KOLOUT = 2                                                                
      IF ( NUMEX .LE. 0 )  GO TO 9000                                           
 1540 JJ = JJ + 1                                                               
      N5 = IMIN(JJ)                                                             
      N3 = LOCOUT(JJ)                                                           
      N8 = LOCATE(N3) + N5-1                                                    
      IF ( INTOUT(JJ)  .EQ.  0 )   N8 = N8 + N5 - 1                             
      IF ( IPRSPY .LT. 3 )  GO TO 1560                                          
      WRITE (MUNIT6, 1544)  JJ, N5, N3, N8, INTOUT(JJ)                  M37.7147
 1544 FORMAT ( ' NEXT  EXAMINE.  JJ, N5, N3, N8, INTOUT(JJ)',  5I8 )    M37.7148
      CALL WINDOW                                                               
 1560 IF ( INTOUT(JJ)  .EQ.  0 )   GO TO 1600                                   
      N9 =  N8 - MEMKAR                                                         
      N8 = N8 + 1                                                               
      IF ( IVEC(N3)  .EQ.  1 )   GO TO 1577                                     
      IF ( N5 .EQ. 1  .AND.  IMAX(JJ) .EQ. 1 )  GO TO 1640                      
 1577 WRITE (ANSI16, 1580)  KAR1(1+N9)                                  M37.7155
 1580 FORMAT ( I10 )                                                    M37.7156
      OUTLIN(KOLOUT:KOLOUT+9) = ANSI16(1:10)                            M37.7157
      OUTLIN(KOLOUT+10:KOLOUT+14) = BLAN80(1:5)                                 
      KOLOUT = KOLOUT + 15                                              M37.7159
      GO TO 1664                                                        M37.7160
 1600 N9 = ( N8 - MEMKAR ) / 2                                                  
      N10 = MEMKAR + 2 * N9  -  N8                                      M38.4588
      N8 = N8 + 2                                                               
      IF ( N10  .NE.  0 )   GO TO 1628                                  M37.7164
C     FOLLOWING REAL*8 EXTRACTIONS LINE UP (NO NEED FOR 4-BYTE SHIFT):  M37.7165
      IF ( IASCII(N3) .EQ. 0 )                                                  
     1 WRITE (ANSI16, 1620)  FKAR1(1+N9)                                        
 1620 FORMAT ( E15.6 )                                                  M37.7168
      IF ( IASCII(N3) .EQ. 1 )                                                  
     1 WRITE (ANSI16, 1624)  FKAR1(1+N9)                                        
 1624 FORMAT ( 4X,  1H",  A6,  1H",  3X )                               M37.7171
      GO TO 1633                                                                
C     FOLLOWING REAL*8 EXTRACTIONS REQUIRE A 4-BYTE SHIFT (FKAR2 USE).  M37.7173
C     FKAR2 HAS HIGHER MEMORY ADDRESS THAN FKAR1, SO IT IS CORRECT WITH M37.7174
C     N9 OFFSET IF N9 IS POSITIVE.   BUT IF N9 IS NEGATIVE, THE HALF    M37.7175
C     WORD SHIFT REALLY SHOULD BE TOWARD LOWER ADDRESSES, WHICH WE      M37.7176
C     COMPENSATE FOR BY THE CONTINUED USE OF FKAR2 BUT WITH ONE SMALLER M37.7177
C     N9 VALUE:                                                         M37.7178
 1628 IF ( N9 .LT. 0 )  N9 = N9 - 1                                             
      IF ( IASCII(N3) .EQ. 0 )                                                  
     1 WRITE (ANSI16, 1620)  FKAR2(1+N9)                                        
      IF ( IASCII(N3) .EQ. 1 )                                                  
     1 WRITE (ANSI16, 1624)  FKAR2(1+N9)                                        
 1633 OUTLIN(KOLOUT:KOLOUT+14) = ANSI16(1:15)                                   
      KOLOUT = KOLOUT + 15                                              M37.7185
      GO TO 1664                                                        M37.7186
 1640 WRITE (ANSI8, 1660)  KAR1(1+N9)                                   M37.7187
 1660 FORMAT ( I6 )                                                     M37.7188
      OUTLIN(KOLOUT:KOLOUT+5) = ANSI8(1:6)                              M37.7189
      IF ( IPRSPY .LT. 3 ) GO TO 1662                                           
      WRITE (MUNIT6, 1661)  N9,  KAR1(1+N9)                             M37.7191
 1661 FORMAT (' I6 INTEGER ENCODED FOR EXAMINE.  N9,KAR1(1+N9) =', 2I8 )M37.7192
      CALL WINDOW                                                               
 1662 KOLOUT = KOLOUT + 6                                               M37.7194
 1664 N5 = N5 + 1                                                       M37.7195
      IF ( N5  .LE.  IMAX(JJ) )   GO TO 1560                                    
      IF ( JJ .LT. NUMEX ) GO TO 1540                                           
      IF ( KOLOUT .LT. 132 )                                                    
     1 OUTLIN(KOLOUT:132) = ' '                                                 
      IF ( IPRSPY .LT. 1 ) GO TO 9000                                           
      N17 = KOLOUT                                                              
      IF ( N17 .GT. 80 )  N17 = 80                                              
      WRITE (MUNIT6, 8872)  KOLOUT, OUTLIN(1:N17)                       M38.4593
 8872 FORMAT (' EXIT "EXAMIN".   KOLOUT =',  I5,                        M37.7200
     1        '    OUTLIN(1:80) =',  A  )                               M38.4594
      CALL WINDOW                                                               
 9000 RETURN                                                            M37.7203
      END                                                               M37.7204
      SUBROUTINE DEPOSI ( IND, INTYPE, N1, N2, D4 )                     M37.7205
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M37.7206
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M37.7207
C     THIS NEAR-UNIVERSAL MODULE SERVICES "DEPOSIT", TO ACTUALLY        M37.7208
C     PERFORM THE TAMPERING.   SYMBOL IS IN ROW IND.  IF IT IS          M37.7209
C     OF TYPE ALPHANUMERIC, ANSI8(1:6) IN "DEKSPY" CARRIES THE          M37.7210
C     NEW CONTENTS.  OTHERWISE, D4 IS THE NUMERIC VALUE TO BE           M37.7211
C     DEPOSITED, AND INTYPE GIVES THE MODE (1=INTEGER, 0=REAL).         M37.7212
C     BOUNDING SUBSCRIPTS ON THE DEPOSIT ARE N1 AND N2, RESPECTIVELY.   M37.7213
C     COMPUTERS WITH INDEX PROBLEMS (E.G., PRIME) NEED REPLACEMENT      M37.7214
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      IF ( IPRSPY .LT. 1 ) GO TO 1846                                           
      WRITE (MUNIT6, 1707)  IND, INTYPE, IASCII(IND), D4, ANSI8         M37.7217
 1707 FORMAT (' TOP OF "DEPOSI".   IND, INTYPE, IASCII(IND),',          M37.7218
     1        ' D4, ANSI8 =',  3I8, E15.4, 3X, A8 )                     M37.7219
      CALL WINDOW                                                               
 1846 N8 = LOCATE(IND)                                                          
C     FOLLOWING USE OF REAL*8 DEPOSIT LOGIC TO ALSO HANDLE              M37.7222
C     ALPHANUMERIC ONLY WORKS FOR COMPUTERS WITH THIS EQUALITY:         M37.7223
      IF ( IASCII(IND) .EQ. 1 )                                                 
     1 READ (ANSI8, 1849)  D4                                                   
 1849 FORMAT ( A6 )                                                     M37.7226
      IF ( INTYPE .EQ. 0 ) GO TO 1880                                           
      N9 = N8 - MEMKAR + N1                                                     
C     ENTER LOOP OF INTEGER DEPOSITS, SUBSCRIPTS N1 THROUGH N2:         M37.7229
 1854 KAR1(N9) = D4                                                             
      N1 = N1 + 1                                                               
      N9 = N9 + 1                                                               
      IF ( N1 .LE. N2 ) GO TO 1854                                              
      GO TO 8000                                                                
C     OUR VECTOR IS  REAL*8, SO REQUIRES THE FOLLOWING SPECIAL DEPOSIT: M37.7235
 1880 N9 = (N8 - MEMKAR)/2 + N1                                                 
      N10 = MEMKAR + 2*(N9-N1) - N8                                             
C     ENTER LOOP OF REAL DEPOSITS, SUBSCRIPTS N1 THROUGH N2:            M37.7238
 1904 IF ( N10 .GT. 0 )                                                         
     1 FKAR2(N9) = D4                                                           
      IF ( N10 .EQ. 0 )                                                         
     1 FKAR1(N9) = D4                                                           
      N1 = N1 + 1                                                               
      N9 = N9 + 1                                                               
      IF ( N1  .LE.  N2 )   GO TO 1904                                          
 8000 IF ( IPRSPY .LT. 1 ) GO TO 9000                                           
      WRITE (MUNIT6, 8872)                                              M37.7247
 8872 FORMAT (' EXIT "DEPOSI".' )                                       M37.7248
      CALL WINDOW                                                               
 9000 RETURN                                                            M37.7250
      END                                                               M37.7251
      SUBROUTINE APPEND                                                 M39.3271
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE CONNECTED TO KEY WORD "APPEND" OF SPY.  OTHERS             M39.3272
C     CAN PERFECT AND USE THEIR OWN INSTALLATION-DEPENDENT              M39.3273
C     (AND PERHAPS PROPRIETARY) EXTENSIONS VIA THIS MODULE.             M39.3274
      RETURN                                                            M39.3275
      END                                                               M39.3276
      SUBROUTINE INTPAR ( MAX, N1, N2, KILL )                           M35.4063
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M35.4064
C     THIS MODULE IS DESIGNED TO EXTRACT TWO FREE-FORMAT ROW NUMBERS    M35.4065
C     FROM  BYTBUF(20)  INPUT BUFFER OF COMMON.   THESE NUMBERS MUST    M35.4066
C     BE POSITIVE, ORDERED (1ST LESS THAN OR EQUAL TO 2ND), AND NOT     M35.4067
C     IN EXCESS OF THE LAST TABLE ROW "MAX".   THE TO ROW NUMBERS ARE   M35.4068
C     PASSED BACK AS  "N1"  AND  "N2"  ARGUMENTS.   IF THIS EXTRACTION  M35.4069
C     WAS SUCCESSFUL,  "KILL"  IS SET TO ZERO;  IF IT FAILED,  KILL = 1.M35.4070
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4071
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      KILL = 0                                                                  
C     CHECK FOR ENGLISH REQUEST "TOP" (FOR THE TOP OF THE FILE):        M35.4074
      IF ( BYTBUF(1:1)   .NE.  'T' )  GO TO 2071                        M35.4075
      N1 = 1                                                            M35.4076
      N2 = 1                                                            M35.4077
      GO TO 2110                                                        M35.4078
C     CHECK FOR ENGLISH REQUEST "BOT" (FOR THE BOTTOM OF THE FILE):     M35.4079
 2071 IF ( BYTBUF(1:1)  .NE. 'B' )   GO TO 2075                         M35.4080
      N1 = MAX                                                          M35.4081
      N2 = MAX                                                          M35.4082
      GO TO 2110                                                        M35.4083
C     CHECK FOR ENGLISH REQUEST "ALL" (FOR THE ENTIRE FILE):            M35.4084
 2075 IF ( BYTBUF(1:1)  .NE.  'A' )   GO TO 2088                        M35.4085
      N1 = 1                                                                    
      N2 = MAX                                                                  
      GO TO 2110                                                                
 2088 DO 2046  I=1, 20                                                  M35.4089
      N13 = 21 - I                                                      M35.4090
      IF ( BYTBUF(N13:N13) .EQ. ' ' )  GO TO 2046                       M35.4091
      IF ( BYTBUF(N13:N13) .NE. ',' )   GO TO 2054                              
      N6 = 20 - I                                                               
      DO 2037  J=1, N6                                                  M35.4094
      N13 = 21 - I + J                                                  M35.4095
 2037 BYTBUF(N13:N13) = BYTBUF(J:J)                                     M35.4096
      GO TO 2054                                                                
 2046 CONTINUE                                                          M35.4098
 2054 N22 = 0                                                                   
      N13 = 0                                                                   
      DO 2104  I=1, 20                                                          
      N17 = I - 1                                                       M35.4102
      IF ( BYTBUF(I:I)  .NE.  ' ' )   GO TO 2091                                
C     BLANK IS IGNORED IF BEFORE 1ST DIGIT, OF IF NOT 1ST OF STRING:    M35.4104
      IF ( N22 .EQ. 0 )  GO TO 2104                                             
      IF ( BYTBUF(N17:N17) .EQ. ' ' )  GO TO 2104                               
      IF ( BYTBUF(N17:N17) .EQ. ',' )  GO TO 2104                               
      N13 = N13 + 1                                                             
      GO TO 2104                                                                
C     BEGIN PROCESSING OF NON-BLANK CHARACTER  BYTBUF(I) :              M35.4110
 2091 N22 = N22 + 1                                                             
      IF ( BYTBUF(I:I)  .NE.  ',' )  GO TO 2093                                 
      IF ( BYTBUF(N17:N17) .EQ. ' ' )  GO TO 2104                               
      N13 = N13 + 1                                                             
      GO TO 2104                                                                
 2093 DO 2097  J=1, 10                                                          
      IF ( BYTBUF(I:I)  .EQ.  DIGIT(J) )   GO TO 2104                           
 2097 CONTINUE                                                                  
 2104 CONTINUE                                                                  
      IF ( N22  .GT.  0 )   GO TO 2109                                          
C     BLANK RESPONSE IS INTERPRETED AS A REQUEST FOR MORE OF SAME:      M35.4123
      N1 = N2 + 1                                                       M35.4124
      IF ( N1  .LE.  MAX )  GO TO 2107                                          
      WRITE (MUNIT6, 2105)                                              M36.3396
 2105 FORMAT ( '    ---- Wrap around, end to beginning ----' )          M36.3397
      CALL WINDOW                                                               
      N1 = 1                                                                    
 2107 N2 = N1 + N33                                                             
      GO TO 2110                                                        M35.4128
 2109 IF ( N13  .EQ. 2 )   GO TO 2132                                           
 2113 WRITE (MUNIT6, 2115)                                              M37.7254
 2115 FORMAT (  ' ????  ILLEGAL DATA (NOT BLANK OR TWO DEC',            M37.7255
     1          ' FREE-FORMAT INTEGERS.   TRY AGAIN.'   )               M37.7256
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2118)                                              M37.7258
 2118 FORMAT (  '       IS IT POSSIBLE USER"S DATA HAS NO',             M37.7259
     3          ' SUCH TABLE (OTHER CAUSE OF MESSAGE)?'  )              M37.7260
      CALL WINDOW                                                               
      KILL = 1                                                                  
      GO TO 9000                                                                
 2132 CALL FREIN2 ( BYTBUF, N1, N2 )                                    M37.7264
      N33 = N2 - N1                                                     M35.4130
 2110 IF ( N1  .LE.  0  )       N1 = 1                                  M35.4131
      IF ( N2  .GT.  MAX )   N2 = MAX                                   M35.4132
 9000 IF ( IPRSPY .LT. 1 ) GO TO 9006                                           
      WRITE (MUNIT6, 9002)  MAX, N1, N2, KILL                           M37.7266
 9002 FORMAT (  ' RETURN FROM "INTPAR".  MAX, N1, N2, KILL =', 4I6 )    M38.4598
      CALL WINDOW                                                               
 9006 RETURN                                                            M37.7268
      END                                                               M35.4138
      SUBROUTINE SOSRNG ( KILL )                                        M35.4139
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M35.4140
C     THIS MODULE SERVES TO EXTRACT A BEGINNING AND ENDING LINE NUMBER  M35.4141
C     FOR SOS-LIKE EDITING OPERATIONS OF  "EDIT"  COMMAND.   THESE TWO  M35.4142
C     INTEGER OUTPUTS ARE  (LIDNT1, LIDNT2)  OF COMMON.   THE ONLY      M35.4143
C     ARGUMENT IS  "KILL",  WHICH TELLS WHETHER THE OPERATION WAS A     M35.4144
C     SUCCESS:  "0" MEANS SUCCESS,  "1" MEANS ERROR.                    M35.4145
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4146
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      IF ( IPRSPY .LT. 1 )  GO TO 2615                                          
      WRITE (MUNIT6, 2613)   BYTBUF                                     M37.7270
 2613 FORMAT ( ' TOP "SOSRANGE".  BYTBUF(A20) =',   A20  )              M35.4150
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2614)  NUMSYM, LIDNT1, LINNOW, CHAR1               M37.7272
 2614 FORMAT ( ' NUMSYM, LIDNT1, LINNOW, CHAR1 =',  3I5, 2X, A1 )       M35.4153
      CALL WINDOW                                                               
 2615 KILL = 0                                                                  
      IF ( CHAR1  .NE.  ' ' )   GO TO 2629                                      
C     BLANK MEANS WE JUST CONTINUE DISPLAYING AS MANY LINES AS LAST TIMEM35.4156
      N12 = LINSPN                                                              
      LIDNT1 = LIDNT2 + 1                                               M35.4158
      LIDNT2 = LIDNT1 + N12                                             M35.4159
      GO TO 2662                                                        M35.4160
 2629 IF ( BYTBUF(1:1)  .NE.  '#' )   GO TO 2632                        M36.3399
C     FOLLOWING CODE PROCESSES A COMMAND LIKE  "*P22" :                 M35.4161
      IBEGCL = 2                                                        M35.4162
      CALL GETNUM ( N1 )                                                M35.4163
      IF ( N1  .NE.  -87654 )  GO TO 2630                                       
 2621 KILL = 1                                                                  
      GO TO 9000                                                                
 2630 LIDNT1 = LINNOW + 1                                               M35.4167
      LIDNT2 = LINNOW + N1                                              M35.4168
      GO TO 2662                                                                
 2632 IF ( BYTBUF(1:1)  .NE.  ' ' )   GO TO 2635                        M35.4170
C     FOLLOWING CODE PROCESS SIMPLE "*P" ---- NEXT 16 LINES, LIKE SOS:  M35.4171
      IF ( LINNOW  .LT.  NUMCRD )   GO TO 2634                                  
      PRINT *,   ' No such lines exist'                                 M35.4173
      GO TO 2621                                                                
 2634 N1 = 16                                                                   
      GO TO 2630                                                                
 2635 IBEGCL = 0                                                        M35.4177
      N13 = 0                                                           M35.4178
      LIDNT1 = 0                                                        M35.4179
      LIDNT2 = 0                                                        M35.4180
 2636 IBEGCL = IBEGCL + 1                                               M35.4181
      N24 = 0                                                           M35.4182
      IF ( IPRSPY  .LT.  2 )  GO TO 2644                                M37.7275
      WRITE (MUNIT6, 2640)                                              M37.7276
 2640 FORMAT (  ' BEGIN NEXT LIDNT.',                                   M37.7277
     1  '    IBEG     N13  LIDNT1  JPOINT      N1     N24'  )           M37.7278
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2641) IBEGCL, N13, LIDNT1, LINNOW, N1, N24         M37.7280
 2641 FORMAT (  '                  ',   6I8  )                          M37.7281
      CALL WINDOW                                                               
 2644 IF ( BYTBUF(IBEGCL:IBEGCL)  .NE.  '.' )   GO TO 2645              M37.7283
      N24 = LINNOW                                                      M35.4190
      GO TO 2654                                                        M35.4191
 2645 IF ( BYTBUF(IBEGCL:IBEGCL)  .NE.  '^' )   GO TO 2648              M35.4192
      N24 = 1                                                           M35.4193
      GO TO 2654                                                        M35.4194
 2648 IF ( BYTBUF(IBEGCL:IBEGCL)  .NE.  '*' )   GO TO 2657              M35.4195
      N24 = NUMCRD                                                              
 2654 IBEGCL = IBEGCL + 1                                               M35.4197
 2657 CALL GETNUM ( N1 )                                                M35.4198
      IF ( N1  .EQ.  -87654 )   GO TO 2621                                      
      N13 = N13 + 1                                                     M35.4200
      IF ( N13  .EQ.  1 )   LIDNT1 = N24 + N1                           M35.4201
      IF ( N13  .EQ.  2 )   LIDNT2 = N24 + N1                           M35.4202
      IF ( N13  .EQ.  2 )   GO TO 2662                                  M35.4203
      IBEGCL = IENDCL + 1                                               M35.4204
      IF ( BYTBUF(IBEGCL:IBEGCL)  .EQ.  ':' )   GO TO 2636              M35.4205
      IF ( BYTBUF(IBEGCL:IBEGCL)  .NE.  '#' )   GO TO 2659                      
C     WE GET HERE WITH A REQUEST LIKE  "*P200!5",  AFTER "200" IS KNOWN M35.4206
      IBEGCL = IBEGCL + 1                                                       
      CALL GETNUM ( N1 )                                                        
      IF ( N1  .EQ.  -87654 )   GO TO 2621                                      
      LIDNT2 = LIDNT1 + N1 - 1                                                  
      GO TO 2662                                                                
C     WE REACH 2659 IF NOTHING FOLLOWS FIRST NUMBER (E.G., "*P200"):    M35.4212
 2659 LIDNT2 = LIDNT1                                                   M35.4213
C     NOW WE DISPLAY LINES  LIDNT1, ...., LIDNT2,  AFTER LIMIT CHECK:   M35.4214
 2662 IF ( LIDNT1  .GE.  1 )   GO TO 2663                               M35.4215
      PRINT *,   ' Illegal syntax of command'                           M35.4216
      GO TO 2621                                                                
 2663 IF ( LIDNT1  .LE.  NUMCRD )   GO TO 2664                          M35.4218
      PRINT *,   ' Range given does not contain any lines'              M35.4219
      GO TO 2621                                                                
 2664 IF ( LIDNT2 .GT. NUMCRD )  LIDNT2 = NUMCRD                        M35.4221
 9000 IF ( IPRSPY .LT. 1 )  GO TO 9006                                          
      WRITE (MUNIT6, 9003)  LIDNT1, LIDNT2,  KILL, CHAR2                M37.7285
 9003 FORMAT ( ' EXIT "SOSRNG".  LIDNT1, LIDNT2, KILL, CHAR2 =',        M35.4224
     1   3I6,  2X,  A1  )                                               M35.4225
      CALL WINDOW                                                               
 9006 RETURN                                                                    
      END                                                               M35.4227
      SUBROUTINE MOVESP ( FROM, TO, N15 )                                       
C     MODULE OF INTERACTIVE EMTP USAGE ONLY, WHICH SERVICES "EMTSPY".   M35.4229
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4230
C     THIS ROUTINE TRANSFERS SINGLE-PRECISION FROM(1:N15) TO            M35.4231
C     TO(1:N15).  EXCEPT FOR MISSING IMPLICIT, IT EQUALS "MOVER".       M35.4232
      DIMENSION FROM(1), TO(1)                                          M35.4233
      DO 4829  J=1, N15                                                 M35.4234
 4829 TO(J) = FROM(J)                                                   M35.4235
      RETURN                                                            M35.4236
      END                                                               M35.4237
      SUBROUTINE PROMPT                                                 M35.4238
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C       VAX-11  INSTALLATION-DEPENDENT EMTP MODULE USED ONLY            M35.4239
C     FOR INTERACTIVE EMTP ("EMTSPY").  INPUT IS PROGRAM                M35.4240
C     PROMPT IN CHARACTER*80 VARIABLE PROM80 OF DECK "DEKSPY".          M35.4241
C     THE PROMPT MUST END WITH COLON (":").  THEN LINE FEED             M35.4242
C     WILL BE SUPPRESSED, SO SUBSEQUENT READ IS TO RIGHT OF ":".        M35.4243
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4244
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      N2 = 80                                                           M35.4246
      DO 1394  J=1, 80                                                  M35.4247
      IF ( PROM80(N2:N2) .NE. ' ' )  GO TO 1426                         M35.4248
 1394 N2 = N2 - 1                                                       M35.4249
      RETURN                                                            M35.4250
C     FOLLOWING LUNIT6 SHOULD REALLY GO TO 2ND SCREEN ("WINDOW")        M37.7289
 1426 WRITE (LUNIT6, 1475)  PROM80(1:N2)                                M37.7290
 1475 FORMAT ( A, $ )    ! ",$" IS DEC MAGIC TO HOLD CURSOR             M35.4252
      RETURN                                                            M35.4253
      END                                                               M35.4254
      SUBROUTINE FREFP1 ( ANSI, D12 )                                   M35.4255
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE (WORKS FOR ANY COMPUTER) USED ONLY FOR THE       M35.4256
C     INTERACTIVE EMTP ("EMTSPY").  IT IS CALLED TO DECODE A            M35.4257
C     SINGLE FLOATING POINT NUMBER.  THE INPUT IS CHARACTER*80          M35.4258
C     VARIABLE  ANSI,  AND THE OUTPUT IS DOUBLE PRECISION D12.          M35.4259
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4260
      CHARACTER*80  ANSI                                                M35.4261
      N8 = 1                                                                    
      CALL FREFIX ( ANSI, N8 )                                                  
      READ (ANSI, 3892)  D12                                            M35.4264
 3892 FORMAT ( E20.0 )                                                  M35.4265
      RETURN                                                            M35.4266
      END                                                               M35.4267
      SUBROUTINE FRESP1 ( ANSI, D12 )                                           
C     UNIVERSAL MODULE (WORKS FOR ANY COMPUTER) USED ONLY FOR THE       M38.4600
C     INTERACTIVE EMTP ("EMTSPY").  IT IS CALLED TO DECODE ONE          M38.4601
C     FLOATING POINT NUMBERS D12 FROM CHARACTER*80 ANSI.   THIS         M38.4602
C     IS IDENTICAL TO "FREFP1", EXCEPT FOR SINGLE PRECISION.            M38.4603
      CHARACTER*80  ANSI                                                M38.4604
      N8 = 1                                                                    
      CALL FREFIX ( ANSI, N8 )                                                  
      READ (ANSI, 3892)  D12                                            M38.4607
 3892 FORMAT ( E20.0 )                                                  M38.4608
      RETURN                                                            M38.4609
      END                                                               M38.4610
      SUBROUTINE FREFP2 ( ANSI, D12, D13 )                              M35.4268
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE (WORKS FOR ANY COMPUTER) USED ONLY FOR THE       M35.4269
C     INTERACTIVE EMTP ("EMTSPY").  IT IS CALLED TO DECODE TWO          M35.4270
C     FLOATING POINT NUMBERS D12 AND D13 FROM CHARACTER*80 ANSI.        M35.4271
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4272
      CHARACTER*80  ANSI                                                M35.4273
      N8 = 2                                                                    
      CALL FREFIX ( ANSI, N8 )                                                  
      READ (ANSI, 3892)  D12, D13                                       M35.4276
 3892 FORMAT ( 2E20.0 )                                                 M35.4277
      RETURN                                                            M35.4278
      END                                                               M35.4279
      SUBROUTINE FRESP2 ( ANSI, D12, D13 )                                      
C     UNIVERSAL MODULE (WORKS FOR ANY COMPUTER) USED ONLY FOR THE       M38.4612
C     INTERACTIVE EMTP ("EMTSPY").  IT IS CALLED TO DECODE TWO          M38.4613
C     FLOATING POINT NUMBERS D12 AND D13 FROM CHARACTER*80 ANSI.        M38.4614
C     THIS IS IDENTICAL TO "FREFP2", EXCEPT FOR SINGLE PRECISION.       M38.4615
      CHARACTER*80  ANSI                                                M38.4616
      N8 = 2                                                                    
      CALL FREFIX ( ANSI, N8 )                                                  
      READ (ANSI, 3892)  D12, D13                                       M38.4619
 3892 FORMAT ( 2E20.0 )                                                 M38.4620
      RETURN                                                            M38.4621
      END                                                               M38.4622
      SUBROUTINE FREFP3 ( ANSI, D12, D13, D14 )                         M35.4280
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE (WORKS FOR ANY COMPUTER) USED ONLY FOR THE       M35.4281
C     INTERACTIVE EMTP ("EMTSPY").  IT IS CALLED TO DECODE THREE        M35.4282
C     FLOATING POINT NUMBERS D12,D13,D14 FROM CHARACTER*80 ANSI.        M35.4283
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4284
      CHARACTER*80  ANSI                                                M35.4285
      N8 = 3                                                                    
      CALL FREFIX ( ANSI, N8 )                                                  
      READ (ANSI, 3892)  D12, D13, D14                                  M35.4288
 3892 FORMAT ( 3E20.0 )                                                 M35.4289
      RETURN                                                            M35.4290
      END                                                               M35.4291
      SUBROUTINE FRESP3 ( ANSI, D12, D13, D14 )                                 
C     UNIVERSAL MODULE (WORKS FOR ANY COMPUTER) USED ONLY FOR THE       M38.4624
C     INTERACTIVE EMTP ("EMTSPY").  IT IS CALLED TO DECODE THREE        M38.4625
C     FLOATING POINT NUMBERS D12,D13,D13 FROM CHARACTER*80 ANSI.        M38.4626
C     THIS IS IDENTICAL TO "FREFP3", EXCEPT FOR SINGLE PRECISION.       M38.4627
      CHARACTER*80  ANSI                                                M38.4628
      N8 = 3                                                                    
      CALL FREFIX ( ANSI, N8 )                                                  
      READ (ANSI, 3892)  D12, D13, D14                                  M38.4631
 3892 FORMAT ( 3E20.0 )                                                 M38.4632
      RETURN                                                            M38.4633
      END                                                               M38.4634
      SUBROUTINE FREIN1 ( ANSI, N12 )                                   M35.4292
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE (WORKS FOR ANY COMPUTER) USED ONLY FOR THE       M35.4293
C     INTERACTIVE EMTP ("EMTSPY").  IT IS CALLED TO DECODE A            M35.4294
C     SINGLE INTEGER NUMBER N12 FROM CHARACTER*80 INPUT ANSI.           M35.4295
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4296
      CHARACTER*80  ANSI                                                M35.4297
      N8 = 1                                                                    
      CALL FREFIX ( ANSI, N8 )                                                  
      READ (ANSI, 3892)  D12                                            M35.4300
 3892 FORMAT ( 3E20.0 )                                                 M35.4301
      N12 = D12                                                                 
      RETURN                                                            M35.4303
      END                                                               M35.4304
      SUBROUTINE FREIN2 ( ANSI, N12, N13 )                              M35.4305
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE (WORKS FOR ANY COMPUTER) USED ONLY FOR THE       M35.4306
C     INTERACTIVE EMTP ("EMTSPY").  IT IS CALLED TO DECODE TWO          M35.4307
C     INTEGER NUMBERS N12 AND N13 FROM CHARACTER*80 INPUT ANSI.         M35.4308
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.4309
      CHARACTER*80  ANSI                                                M35.4310
      N8 = 2                                                                    
      CALL FREFIX ( ANSI, N8 )                                                  
      READ (ANSI, 3892)  D12, D13                                       M35.4313
 3892 FORMAT ( 3E20.0 )                                                 M35.4314
      N12 = D12                                                                 
      N13 = D13                                                                 
      RETURN                                                            M35.4317
      END                                                               M35.4318
      SUBROUTINE FREFIX ( ANSI, N8 )                                    M35.4319
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE USED ONLY BY INTERACTIVE EMTP ("EMTSPY").        M35.4320
C     FOR NON-INTERACTIVE USE ONLY, IT CAN BE DESTROYED.                M35.4321
C     THIS MODULE IS CALLED BY "FREFP1", "FREIN1", ETC. --- ALL         M35.4322
C     OF THE FREE-FIELD INPUT MODULES OF SPY USAGE.  IT CHECKS          M35.4323
C     FOR LEGAL NUMERICAL DATA, AND WILL RE-PROMPT FOR MORE IF          M35.4324
C     THE USER HAS MADE A MISTAKE, THEREBY AVOIDING POSSIBLE            M35.4325
C     DEATH OF INTERACTIVE EMTP EXECUTION DUE TO TYPING ERROR.          M35.4326
C     ANSI IS THE INPUT BEING PROCESSED, FOR N8 NUMBERS.  UPON          M35.4327
C     EXIT, ANSI WILL BE CONVERTED TO 4E20.0 FIXED-FORMAT DATA.         M35.4328
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*80  ANSI, HOLD                                          M35.4330
      IF ( IPRSPY .LT. 3 )  GO TO 3582                                          
      WRITE (MUNIT6, 3579)  N8, ANSI(1:40)                              M37.7292
 3579 FORMAT ( ' TOP OF "FREFIX".  N8 =',  I5,                          M37.7293
     1         '   ANSI(1:40) = ',  A40  )                              M37.7294
      CALL WINDOW                                                               
 3582 HOLD(1:80) = ' '                                                          
      KK = 0                                                                    
      I = 0                                                                     
      GO TO 3648                                                                
 3612 IF ( ANSI(I:I) .EQ. ' '  .OR.                                             
     1     ANSI(I:I) .EQ. ','  .OR.                                             
     2     I .GE. 80 )  GO TO 3615                                              
      I = I + 1                                                                 
      GO TO 3612                                                                
 3615 N5 = I - 1                                                                
      KK = KK + 1                                                               
      N13 = 0                                                                   
      N14 = 0                                                                   
      DO 3642  J=N3, N5                                                         
      IF ( ANSI(J:J) .NE. '+'  .AND.                                    M35.4348
     1     ANSI(J:J) .NE. '-' )  GO TO 3617                             M35.4349
      IF ( J .EQ. N3 )  GO TO 3642                                              
      N16 = J - 1                                                               
      IF ( ANSI(N16:N16) .EQ. 'E'  .OR.                                         
     1     ANSI(N16:N16) .EQ. 'D' ) GO TO 3642                                  
      GO TO 3758                                                                
 3617 IF ( ANSI(J:J) .NE. '.' )  GO TO 3626                             M35.4355
      N14 = N14 + 1                                                             
      IF ( N14 .LE. 1 )  GO TO 3642                                             
      GO TO 3758                                                                
 3626 IF ( ANSI(J:J) .NE. 'D'   .AND.                                   M35.4359
     1     ANSI(J:J) .NE. 'E' )   GO TO 3634                            M35.4360
      N13 = N13 + 1                                                             
      IF ( N13 .LE. 1 )  GO TO 3642                                             
      GO TO 3758                                                                
 3634 DO 3638  K=1, 10                                                          
      IF ( ANSI(J:J) .EQ. DIGIT(K) )  GO TO 3642                                
 3638 CONTINUE                                                                  
      GO TO 3758                                                                
 3642 CONTINUE                                                                  
      N2 = 20 * KK                                                              
      N1 = N2 - ( N5 - N3 )                                                     
      HOLD(N1:N2) = ANSI(N3:N5)                                                 
      IF ( KK .GE. N8 )  GO TO 4100                                             
 3648 I = I + 1                                                                 
      IF ( I .GT. 80 )  GO TO 3758                                              
      IF ( ANSI(I:I) .EQ. ' ' )  GO TO 3648                                     
      N3 = I                                                                    
      GO TO 3612                                                                
 3758 WRITE (PROM80, 3761)  J, ANSI(J-2:J+2)                            M35.4378
 3761 FORMAT ( '   @@@  Illegal data in column', I3,                    M35.4379
     1         ' [',  A5,  '].  Resend line or END :'  )                M35.4380
      CALL PROMPT                                                               
      WRITE (*,*) ' OK, "FREFIX" CALL TO "FLAGER" BEGINS.'              M38.4635
      LOCKBR = 1                                                                
      CALL FLAGER                                                               
      WRITE (*,*) ' OK, BACK IN "FREFIX" WITH BUFF77 AGAIN.'            M38.4638
      ANSI = BUFF77                                                             
      IF ( ANSI(1:4) .NE. 'END ' )  GO TO 3582                                  
      ANSI(1:80) = ' '                                                          
      GO TO 4103                                                                
 4100 ANSI(1:N2) = HOLD(1:N2)                                                   
 4103 IF ( IPRSPY .LT. 2 )  GO TO 4109                                          
      WRITE (MUNIT6, 4108)  ANSI(1:N2)                                  M37.7297
 4108 FORMAT ( ' EXIT "FREFIX".  ANSI(1:N2) =',  A80 )                  M35.4390
      CALL WINDOW                                                               
 4109 RETURN                                                            M37.7299
      END                                                               M35.4392
      SUBROUTINE LOCATN                                                 M35.4393
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'FIXCOM.FOR;/NOLIST'                                           
      LOCATE(   1)  =  LOCINT ( BUS1   )                                M35.4401
      LOCATE(   2)  =  LOCINT ( BUS2   )                                M35.4402
      LOCATE(   3)  =  LOCINT ( BUS3   )                                M35.4403
      LOCATE(   4)  =  LOCINT ( BUS4   )                                M35.4404
      LOCATE(   5)  =  LOCINT ( BUS5   )                                M35.4405
      LOCATE(   6)  =  LOCINT ( BUS6   )                                M35.4406
      LOCATE(   7)  =  LOCINT ( TRASH  )                                M35.4407
      LOCATE(   8)  =  LOCINT ( BLANK  )                                M35.4408
      LOCATE(   9)  =  LOCINT ( TERRA  )                                M35.4409
      LOCATE(  10)  =  LOCINT ( USERID )                                M35.4410
      LOCATE(  11)  =  LOCINT ( BRANCH )                                M35.4411
      LOCATE(  12)  =  LOCINT ( COPY   )                                M35.4412
      LOCATE(  13)  =  LOCINT ( CSEPAR )                                M35.4413
      LOCATE(  14)  =  LOCINT ( CHCONT )                                M35.4414
      LOCATE(  15)  =  LOCINT ( TEXCOL )                                M35.4415
      LOCATE(  16)  =  LOCINT ( TEXTA6 )                                M35.4416
      LOCATE(  17)  =  LOCINT ( DATE1  )                                M35.4417
      LOCATE(  18)  =  LOCINT ( TCLOCK )                                M35.4418
      LOCATE(  19)  =  LOCINT ( VSTACS )                                M35.4419
      LOCATE(  20)  =  LOCINT ( ABUFF  )                                M35.4420
      LOCATE(  21)  =  LOCINT ( CI1    )                                M35.4421
      LOCATE(  22)  =  LOCINT ( CK1    )                                M35.4422
      LOCATE(  23)  =  LOCINT ( DELTAT )                                M35.4423
      LOCATE(  24)  =  LOCINT ( DELTA2 )                                M35.4424
      LOCATE(  25)  =  LOCINT ( FREQCS )                                M35.4425
      LOCATE(  26)  =  LOCINT ( EPSILN )                                M35.4426
      LOCATE(  27)  =  LOCINT ( XUNITS )                                M35.4427
      LOCATE(  28)  =  LOCINT ( AINCR  )                                M35.4428
      LOCATE(  29)  =  LOCINT ( XMAXMX )                                M35.4429
      LOCATE(  30)  =  LOCINT ( ZNVREF )                                M35.4430
      LOCATE(  31)  =  LOCINT ( EPSZNO )                                M35.4431
      LOCATE(  32)  =  LOCINT ( EPWARN )                                M35.4432
      LOCATE(  33)  =  LOCINT ( EPSTOP )                                M35.4433
      LOCATE(  34)  =  LOCINT ( T      )                                M35.4434
      LOCATE(  35)  =  LOCINT ( HERTZ  )                                M35.4435
      LOCATE(  36)  =  LOCINT ( TOLMAT )                                M35.4436
      LOCATE(  37)  =  LOCINT ( TWOPI  )                                M35.4437
      LOCATE(  38)  =  LOCINT ( TMAX   )                                M35.4438
      LOCATE(  39)  =  LOCINT ( OMEGA  )                                M35.4439
      LOCATE(  40)  =  LOCINT ( COPT   )                                M35.4440
      LOCATE(  41)  =  LOCINT ( XOPT   )                                M35.4441
      LOCATE(  42)  =  LOCINT ( SZPLT  )                                M35.4442
      LOCATE(  43)  =  LOCINT ( SZBED  )                                M35.4443
      LOCATE(  44)  =  LOCINT ( SGLFIR )                                M35.4444
      LOCATE(  45)  =  LOCINT ( SIGMAX )                                M35.4445
      LOCATE(  46)  =  LOCINT ( EPSUBA )                                M35.4446
      LOCATE(  47)  =  LOCINT ( EPDGEL )                                M35.4447
      LOCATE(  48)  =  LOCINT ( EPOMEG )                                M35.4448
      LOCATE(  49)  =  LOCINT ( FMINFS )                                M35.4449
      LOCATE(  50)  =  LOCINT ( DELFFS )                                M35.4450
      LOCATE(  51)  =  LOCINT ( FMAXFS )                                M35.4451
      LOCATE(  52)  =  LOCINT ( TENERG )                                M35.4452
      LOCATE(  53)  =  LOCINT ( BEGMAX )                                M35.4453
      LOCATE(  54)  =  LOCINT ( TENM3  )                                M35.4454
      LOCATE(  55)  =  LOCINT ( TENM6  )                                M35.4455
      LOCATE(  56)  =  LOCINT ( UNITY  )                                M35.4456
      LOCATE(  57)  =  LOCINT ( ONEHAF )                                M35.4457
      LOCATE(  58)  =  LOCINT ( PEAKND )                                M35.4458
      LOCATE(  59)  =  LOCINT ( FLTINF )                                M35.4459
      LOCATE(  60)  =  LOCINT ( FLZERO )                                M35.4460
      LOCATE(  61)  =  LOCINT ( DEGMIN )                                M35.4461
      LOCATE(  62)  =  LOCINT ( DEGMAX )                                M35.4462
      LOCATE(  63)  =  LOCINT ( STATFR )                                M35.4463
      LOCATE(  64)  =  LOCINT ( VOLTBC )                                M35.4464
      LOCATE(  65)  =  LOCINT ( FLSTAT )                                M35.4465
      LOCATE(  66)  =  LOCINT ( DTNEXT )                                M35.4466
      LOCATE(  67)  =  LOCINT ( ANGLE  )                                M35.4467
      LOCATE(  68)  =  LOCINT ( PU     )                                M35.4468
      LOCATE(  69)  =  LOCINT ( SEEDR  )                                M35.4469
      LOCATE(  70)  =  LOCINT ( SPEEDL )                                M35.4470
      LOCATE(  71)  =  LOCINT ( KSTART )                                M35.4471
      LOCATE(  72)  =  LOCINT ( KNT    )                                M35.4472
      LOCATE(  73)  =  LOCINT ( KBASE  )                                M35.4473
      LOCATE(  74)  =  LOCINT ( LTDELT )                                M35.4474
      LOCATE(  75)  =  LOCINT ( UNUSED )                                M37.7300
      LOCATE(  76)  =  LOCINT ( MTAPE  )                                M35.4476
      LOCATE(  77)  =  LOCINT ( LUNIT1 )                                M35.4477
      LOCATE(  78)  =  LOCINT ( LUNIT2 )                                M35.4478
      LOCATE(  79)  =  LOCINT ( LUNIT3 )                                M35.4479
      LOCATE(  80)  =  LOCINT ( LUNIT4 )                                M35.4480
      LOCATE(  81)  =  LOCINT ( LUNIT5 )                                M35.4481
      LOCATE(  82)  =  LOCINT ( LUNIT6 )                                M35.4482
      LOCATE(  83)  =  LOCINT ( LUNIT7 )                                M35.4483
      LOCATE(  84)  =  LOCINT ( LUNIT8 )                                M35.4484
      LOCATE(  85)  =  LOCINT ( LUNIT9 )                                M35.4485
      LOCATE(  86)  =  LOCINT ( LUNT10 )                                M35.4486
      LOCATE(  87)  =  LOCINT ( LUNT11 )                                M35.4487
      LOCATE(  88)  =  LOCINT ( LUNT12 )                                M35.4488
      LOCATE(  89)  =  LOCINT ( LUNT13 )                                M35.4489
      LOCATE(  90)  =  LOCINT ( LUNT14 )                                M35.4490
      LOCATE(  91)  =  LOCINT ( LUNT15 )                                M35.4491
      LOCATE(  92)  =  LOCINT ( NEXOUT )                                M35.4492
      LOCATE(  93)  =  LOCINT ( NRIGHT )                                M35.4493
      LOCATE(  94)  =  LOCINT ( NFRFLD )                                M35.4494
      LOCATE(  95)  =  LOCINT ( KOLBEG )                                M35.4495
      LOCATE(  96)  =  LOCINT ( KPRCHG )                                M35.4496
      LOCATE(  97)  =  LOCINT ( MULTPR )                                M35.4497
      LOCATE(  98)  =  LOCINT ( IPNTV  )                                M35.4498
      LOCATE(  99)  =  LOCINT ( INDTV  )                                M35.4499
      LOCATE( 100)  =  LOCINT ( LSTAT  )                                M35.4500
      LOCATE( 101)  =  LOCINT ( NBYTE  )                                M35.4501
      LOCATE( 102)  =  LOCINT ( LUNSAV )                                M35.4502
      LOCATE( 103)  =  LOCINT ( IPRSOV )                                M35.4503
      LOCATE( 104)  =  LOCINT ( ICHECK )                                M35.4504
      LOCATE( 105)  =  LOCINT ( UNUSED )                                M37.7301
      LOCATE( 106)  =  LOCINT ( IEND   )                                M35.4506
      LOCATE( 107)  =  LOCINT ( ILINE  )                                M35.4507
      LOCATE( 108)  =  LOCINT ( INONL  )                                M35.4508
      LOCATE( 109)  =  LOCINT ( IOLD   )                                M35.4509
      LOCATE( 110)  =  LOCINT ( IOUT   )                                M35.4510
      LOCATE( 111)  =  LOCINT ( IPRINT )                                M35.4511
      LOCATE( 112)  =  LOCINT ( IPUNCH )                                M35.4512
      LOCATE( 113)  =  LOCINT ( IREAD  )                                M35.4513
      LOCATE( 114)  =  LOCINT ( KOL132 )                                M35.4514
      LOCATE( 115)  =  LOCINT ( ISTEP  )                                M35.4515
      LOCATE( 116)  =  LOCINT ( UNUSED )                                M37.7302
      LOCATE( 117)  =  LOCINT ( ITYPE  )                                M35.4517
      LOCATE( 118)  =  LOCINT ( IT1    )                                M35.4518
      LOCATE( 119)  =  LOCINT ( IT2    )                                M35.4519
      LOCATE( 120)  =  LOCINT ( IUPPER )                                M35.4520
      LOCATE( 121)  =  LOCINT ( IZERO  )                                M35.4521
      LOCATE( 122)  =  LOCINT ( KCOUNT )                                M35.4522
      LOCATE( 123)  =  LOCINT ( ISTEAD )                                M35.4523
      LOCATE( 124)  =  LOCINT ( UNUSED )                                M37.7303
      LOCATE( 125)  =  LOCINT ( LDATA  )                                M35.4525
      LOCATE( 126)  =  LOCINT ( LBRNCH )                                M35.4526
      LOCATE( 127)  =  LOCINT ( LIMTXF )                                M35.4527
      LOCATE( 128)  =  LOCINT ( MDEBUG )                                M35.4528
      LOCATE( 129)  =  LOCINT ( LEXCT  )                                M35.4529
      LOCATE( 130)  =  LOCINT ( LBUS   )                                M35.4530
      LOCATE( 131)  =  LOCINT ( LYMAT  )                                M35.4531
      LOCATE( 132)  =  LOCINT ( LSWTCH )                                M35.4532
      LOCATE( 133)  =  LOCINT ( LNONL  )                                M35.4533
      LOCATE( 134)  =  LOCINT ( LCHAR  )                                M35.4534
      LOCATE( 135)  =  LOCINT ( M4PLOT )                                M35.4535
      LOCATE( 136)  =  LOCINT ( LPAST  )                                M35.4536
      LOCATE( 137)  =  LOCINT ( LSMAT  )                                M35.4537
      LOCATE( 138)  =  LOCINT ( IPLOT  )                                M35.4538
      LOCATE( 139)  =  LOCINT ( NCOMP  )                                M35.4539
      LOCATE( 140)  =  LOCINT ( NV     )                                M35.4540
      LOCATE( 141)  =  LOCINT ( LCOMP  )                                M35.4541
      LOCATE( 142)  =  LOCINT ( NUMSM  )                                M35.4542
      LOCATE( 143)  =  LOCINT ( IFDEP  )                                M35.4543
      LOCATE( 144)  =  LOCINT ( LTAILS )                                M35.4544
      LOCATE( 145)  =  LOCINT ( LFDEP  )                                M35.4545
      LOCATE( 146)  =  LOCINT ( LWT    )                                M35.4546
      LOCATE( 147)  =  LOCINT ( LAST   )                                M35.4547
      LOCATE( 148)  =  LOCINT ( NPOWER )                                M35.4548
      LOCATE( 149)  =  LOCINT ( MAXPE  )                                M35.4549
      LOCATE( 150)  =  LOCINT ( LPEAK  )                                M35.4550
      LOCATE( 151)  =  LOCINT ( NOUT   )                                M35.4551
      LOCATE( 152)  =  LOCINT ( IV     )                                M35.4552
      LOCATE( 153)  =  LOCINT ( INEOF  )                                M35.4553
      LOCATE( 154)  =  LOCINT ( KTRLSW )                                M35.4554
      LOCATE( 155)  =  LOCINT ( NUM99  )                                M35.4555
      LOCATE( 156)  =  LOCINT ( KPARTB )                                M35.4556
      LOCATE( 157)  =  LOCINT ( LLBUFF )                                M35.4557
      LOCATE( 158)  =  LOCINT ( KANAL  )                                M35.4558
      LOCATE( 159)  =  LOCINT ( NSMTH  )                                M35.4559
      LOCATE( 160)  =  LOCINT ( NTCSEX )                                M35.4560
      LOCATE( 161)  =  LOCINT ( NSTACS )                                M35.4561
      LOCATE( 162)  =  LOCINT ( KLOAEP )                                M35.4562
      LOCATE( 163)  =  LOCINT ( LASTOV )                                M35.4563
      LOCATE( 164)  =  LOCINT ( LTACST )                                M35.4564
      LOCATE( 165)  =  LOCINT ( LHIST  )                                M35.4565
      LOCATE( 166)  =  LOCINT ( IFX    )                                M35.4566
      LOCATE( 167)  =  LOCINT ( NDELTA )                                M35.4567
      LOCATE( 168)  =  LOCINT ( IDELTA )                                M35.4568
      LOCATE( 169)  =  LOCINT ( INECHO )                                M35.4569
      LOCATE( 170)  =  LOCINT ( NOUTPR )                                M35.4570
      LOCATE( 171)  =  LOCINT ( KTAB   )                                M35.4571
      LOCATE( 172)  =  LOCINT ( JFLSOS )                                M35.4572
      LOCATE( 173)  =  LOCINT ( NUMDCD )                                M35.4573
      LOCATE( 174)  =  LOCINT ( NUMUM  )                                M35.4574
      LOCATE( 175)  =  LOCINT ( LSPCUM )                                M35.4575
      LOCATE( 176)  =  LOCINT ( NPHCAS )                                M35.4576
      LOCATE( 177)  =  LOCINT ( LOCZ11 )                                M35.4577
      LOCATE( 178)  =  LOCINT ( LOCBR1 )                                M35.4578
      LOCATE( 179)  =  LOCINT ( IALTER )                                M35.4579
      LOCATE( 180)  =  LOCINT ( ICHAR  )                                M35.4580
      LOCATE( 181)  =  LOCINT ( KTREF  )                                M35.4581
      LOCATE( 182)  =  LOCINT ( KPH    )                                M35.4582
      LOCATE( 183)  =  LOCINT ( KREQAB )                                M35.4583
      LOCATE( 184)  =  LOCINT ( KSAT   )                                M35.4584
      LOCATE( 185)  =  LOCINT ( MEMSAV )                                M35.4585
      LOCATE( 186)  =  LOCINT ( LISOFF )                                M35.4586
      LOCATE( 187)  =  LOCINT ( LSPOV4 )                                M35.4587
      LOCATE( 188)  =  LOCINT ( KBURRO )                                M35.4588
      LOCATE( 189)  =  LOCINT ( IAVERG )                                M35.4589
      LOCATE( 190)  =  LOCINT ( LSIZ23 )                                M35.4590
      LOCATE( 191)  =  LOCINT ( LSIZ26 )                                M35.4591
      LOCATE( 192)  =  LOCINT ( NUMOUT )                                M35.4592
      LOCATE( 193)  =  LOCINT ( MOLDAT )                                M35.4593
      LOCATE( 194)  =  LOCINT ( LSIZ27 )                                M35.4594
      LOCATE( 195)  =  LOCINT ( LTLABL )                                M35.4595
      LOCATE( 196)  =  LOCINT ( IWT    )                                M35.4596
      LOCATE( 197)  =  LOCINT ( IFDEP2 )                                M35.4597
      LOCATE( 198)  =  LOCINT ( IDOUBL )                                M35.4598
      LOCATE( 199)  =  LOCINT ( IOUTIN )                                M35.4599
      LOCATE( 200)  =  LOCINT ( IPUN   )                                M35.4600
      LOCATE( 201)  =  LOCINT ( JST    )                                M35.4601
      LOCATE( 202)  =  LOCINT ( JST1   )                                M35.4602
      LOCATE( 203)  =  LOCINT ( UNUSED )                                M37.7304
      LOCATE( 204)  =  LOCINT ( NUMSUB )                                M35.4604
      LOCATE( 205)  =  LOCINT ( MAXZNO )                                M35.4605
      LOCATE( 206)  =  LOCINT ( KALPLT )                                M35.4606
      LOCATE( 207)  =  LOCINT ( NIOMAX )                                M35.4607
      LOCATE( 208)  =  LOCINT ( NIAMAX )                                M35.4608
      LOCATE( 209)  =  LOCINT ( IBR1   )                                M35.4609
      LOCATE( 210)  =  LOCINT ( IFSEM  )                                M35.4610
      LOCATE( 211)  =  LOCINT ( LFSEM  )                                M35.4611
      LOCATE( 212)  =  LOCINT ( IADD   )                                M35.4612
      LOCATE( 213)  =  LOCINT ( LFD    )                                M35.4613
      LOCATE( 214)  =  LOCINT ( LAUX   )                                M35.4614
      LOCATE( 215)  =  LOCINT ( IOFGND )                                M35.4615
      LOCATE( 216)  =  LOCINT ( IOFBND )                                M35.4616
      LOCATE( 217)  =  LOCINT ( UNUSED )                                M37.7305
      LOCATE( 218)  =  LOCINT ( JSEEDR )                                M35.4618
      LOCATE( 219)  =  LOCINT ( MODOUT )                                M35.4619
      LOCATE( 220)  =  LOCINT ( IFTAIL )                                M35.4620
      LOCATE( 221)  =  LOCINT ( IPOINT )                                M35.4621
      LOCATE( 222)  =  LOCINT ( LPAST2 )                                M35.4622
      LOCATE( 223)  =  LOCINT ( NCURR  )                                M35.4623
      LOCATE( 224)  =  LOCINT ( IOFFD  )                                M35.4624
      LOCATE( 225)  =  LOCINT ( ISPLOT )                                M35.4625
      LOCATE( 226)  =  LOCINT ( ISPRIN )                                M35.4626
      LOCATE( 227)  =  LOCINT ( MAXOUT )                                M35.4627
      LOCATE( 228)  =  LOCINT ( IPOS   )                                M35.4628
      LOCATE( 229)  =  LOCINT ( UNUSED )                                M37.7306
      LOCATE( 230)  =  LOCINT ( UNUSED )                                M37.7307
      LOCATE( 231)  =  LOCINT ( KILL   )                                M35.4631
      LOCATE( 232)  =  LOCINT ( IVOLT  )                                M35.4632
      LOCATE( 233)  =  LOCINT ( NCHAIN )                                M35.4633
      LOCATE( 234)  =  LOCINT ( IPRSUP )                                M35.4634
      LOCATE( 235)  =  LOCINT ( UNUSED )                                M37.7308
      LOCATE( 236)  =  LOCINT ( INTINF )                                M35.4636
      LOCATE( 237)  =  LOCINT ( KCONST )                                M35.4637
      LOCATE( 238)  =  LOCINT ( KSWTCH )                                M35.4638
      LOCATE( 239)  =  LOCINT ( IT     )                                M35.4639
      LOCATE( 240)  =  LOCINT ( NTOT   )                                M35.4640
      LOCATE( 241)  =  LOCINT ( IBR    )                                M35.4641
      LOCATE( 242)  =  LOCINT ( LCOM10 )                                M35.4642
      LOCATE( 243)  =  LOCINT ( LTRNST )                                M35.4643
      LOCATE( 244)  =  LOCINT ( LSYN   )                                M35.4644
      LOCATE( 245)  =  LOCINT ( KSSOUT )                                M35.4645
      LOCATE( 246)  =  LOCINT ( LOOPSS )                                M35.4646
      LOCATE( 247)  =  LOCINT ( INFEXP )                                M35.4647
      LOCATE( 248)  =  LOCINT ( NUMREF )                                M35.4648
      LOCATE( 249)  =  LOCINT ( NWORD1 )                                M35.4649
      LOCATE( 250)  =  LOCINT ( NWORD2 )                                M35.4650
      LOCATE( 251)  =  LOCINT ( ILOAEP )                                M35.4651
      LOCATE( 252)  =  LOCINT ( LNPIN  )                                M35.4652
      LOCATE( 253)  =  LOCINT ( NTOT1  )                                M35.4653
      LOCATE( 254)  =  LOCINT ( LIMSTP )                                M35.4654
      LOCATE( 255)  =  LOCINT ( INDSTP )                                M35.4655
      LOCATE( 256)  =  LOCINT ( NC     )                                M35.4656
      LOCATE( 257)  =  LOCINT ( UNUSED )                                M37.7309
      LOCATE( 258)  =  LOCINT ( UNUSED )                                M37.7310
      LOCATE( 259)  =  LOCINT ( ICAT   )                                M35.4659
      LOCATE( 260)  =  LOCINT ( NUMNVO )                                M35.4660
      LOCATE( 261)  =  LOCINT ( UNUSED )                                M37.7311
      LOCATE( 262)  =  LOCINT ( NENERG )                                M35.4662
      LOCATE( 263)  =  LOCINT ( ISW    )                                M35.4663
      LOCATE( 264)  =  LOCINT ( ITEST  )                                M35.4664
      LOCATE( 265)  =  LOCINT ( IDIST  )                                M35.4665
      LOCATE( 266)  =  LOCINT ( X      )                                M35.4666
      LOCATE( 267)  =  LOCINT ( YKM    )                                M35.4667
      LOCATE( 268)  =  LOCINT ( KM     )                                M35.4668
      LOCATE( 269)  =  LOCINT ( XK     )                                M35.4669
      LOCATE( 270)  =  LOCINT ( XM     )                                M35.4670
      LOCATE( 271)  =  LOCINT ( WEIGHT )                                M35.4671
      LOCATE( 272)  =  LOCINT ( IWTENT )                                M35.4672
      LOCATE( 273)  =  LOCINT ( CON1   )                                M35.4673
      LOCATE( 274)  =  LOCINT ( ISKIP  )                                M35.4674
      LOCATE( 275)  =  LOCINT ( ZINF   )                                M35.4675
      LOCATE( 276)  =  LOCINT ( ETA    )                                M35.4676
      LOCATE( 277)  =  LOCINT ( NHIST  )                                M35.4677
      LOCATE( 278)  =  LOCINT ( STAILM )                                M35.4678
      LOCATE( 279)  =  LOCINT ( STAILK )                                M35.4679
      LOCATE( 280)  =  LOCINT ( XMAX   )                                M35.4680
      LOCATE( 281)  =  LOCINT ( KOUTVP )                                M35.4681
      LOCATE( 282)  =  LOCINT ( BNRG   )                                M35.4682
      LOCATE( 283)  =  LOCINT ( SCONST )                                M35.4683
      LOCATE( 284)  =  LOCINT ( CNVHST )                                M35.4684
      LOCATE( 285)  =  LOCINT ( SFD    )                                M35.4685
      LOCATE( 286)  =  LOCINT ( QFD    )                                M35.4686
      LOCATE( 287)  =  LOCINT ( SEMAUX )                                M35.4687
      LOCATE( 288)  =  LOCINT ( IBSOUT )                                M35.4688
      LOCATE( 289)  =  LOCINT ( BVALUE )                                M35.4689
      LOCATE( 290)  =  LOCINT ( SPTACS )                                M35.4690
      LOCATE( 291)  =  LOCINT ( KSWTYP )                                M35.4691
      LOCATE( 292)  =  LOCINT ( MODSWT )                                M35.4692
      LOCATE( 293)  =  LOCINT ( KBEGSW )                                M35.4693
      LOCATE( 294)  =  LOCINT ( LASTSW )                                M35.4694
      LOCATE( 295)  =  LOCINT ( KENTNB )                                M35.4695
      LOCATE( 296)  =  LOCINT ( NBHDSW )                                M35.4696
      LOCATE( 297)  =  LOCINT ( TOPEN  )                                M35.4697
      LOCATE( 298)  =  LOCINT ( CRIT   )                                M35.4698
      LOCATE( 299)  =  LOCINT ( KDEPSW )                                M35.4699
      LOCATE( 300)  =  LOCINT ( TDNS   )                                M35.4700
      LOCATE( 301)  =  LOCINT ( ISOURC )                                M35.4701
      LOCATE( 302)  =  LOCINT ( ENERGY )                                M35.4702
      LOCATE( 303)  =  LOCINT ( IARDUB )                                M35.4703
      LOCATE( 304)  =  LOCINT ( ARDUBE )                                M35.4704
      LOCATE( 305)  =  LOCINT ( NONLAD )                                M35.4705
      LOCATE( 306)  =  LOCINT ( NONLE  )                                M35.4706
      LOCATE( 307)  =  LOCINT ( VNONL  )                                M35.4707
      LOCATE( 308)  =  LOCINT ( CURR   )                                M35.4708
      LOCATE( 309)  =  LOCINT ( ANONL  )                                M35.4709
      LOCATE( 310)  =  LOCINT ( VECNL1 )                                M35.4710
      LOCATE( 311)  =  LOCINT ( VECNL2 )                                M35.4711
      LOCATE( 312)  =  LOCINT ( BRNONL )                                M35.4712
      LOCATE( 313)  =  LOCINT ( VZERO  )                                M35.4713
      LOCATE( 314)  =  LOCINT ( ILAST  )                                M35.4714
      LOCATE( 315)  =  LOCINT ( NLTYPE )                                M35.4715
      LOCATE( 316)  =  LOCINT ( KUPL   )                                M35.4716
      LOCATE( 317)  =  LOCINT ( NLSUB  )                                M35.4717
      LOCATE( 318)  =  LOCINT ( CURSUB )                                M35.4718
      LOCATE( 319)  =  LOCINT ( CCHAR  )                                M35.4719
      LOCATE( 320)  =  LOCINT ( VCHAR  )                                M35.4720
      LOCATE( 321)  =  LOCINT ( GSLOPE )                                M35.4721
      LOCATE( 322)  =  LOCINT ( KK     )                                M35.4722
      LOCATE( 323)  =  LOCINT ( C      )                                M35.4723
      LOCATE( 324)  =  LOCINT ( TR     )                                M35.4724
      LOCATE( 325)  =  LOCINT ( TX     )                                M35.4725
      LOCATE( 326)  =  LOCINT ( R      )                                M35.4726
      LOCATE( 327)  =  LOCINT ( NR     )                                M35.4727
      LOCATE( 328)  =  LOCINT ( LENGTH )                                M35.4728
      LOCATE( 329)  =  LOCINT ( CIK    )                                M35.4729
      LOCATE( 330)  =  LOCINT ( CI     )                                M35.4730
      LOCATE( 331)  =  LOCINT ( CK     )                                M35.4731
      LOCATE( 332)  =  LOCINT ( SWNAME )                                M35.4732
      LOCATE( 333)  =  LOCINT ( IBRNCH )                                M35.4733
      LOCATE( 334)  =  LOCINT ( JBRNCH )                                M35.4734
      LOCATE( 335)  =  LOCINT ( TSTOP  )                                M35.4735
      LOCATE( 336)  =  LOCINT ( NONLK  )                                M35.4736
      LOCATE( 337)  =  LOCINT ( NONLM  )                                M35.4737
      LOCATE( 338)  =  LOCINT ( SPUM   )                                M35.4738
      LOCATE( 339)  =  LOCINT ( KKS    )                                M35.4739
      LOCATE( 340)  =  LOCINT ( KKNONL )                                M35.4740
      LOCATE( 341)  =  LOCINT ( ZNONL  )                                M35.4741
      LOCATE( 342)  =  LOCINT ( ZNONLB )                                M35.4742
      LOCATE( 343)  =  LOCINT ( ZNONLC )                                M35.4743
      LOCATE( 344)  =  LOCINT ( FINIT  )                                M35.4744
      LOCATE( 345)  =  LOCINT ( KSUB   )                                M35.4745
      LOCATE( 346)  =  LOCINT ( MSUB   )                                M35.4746
      LOCATE( 347)  =  LOCINT ( ISUBEG )                                M35.4747
      LOCATE( 348)  =  LOCINT ( LITYPE )                                M35.4748
      LOCATE( 349)  =  LOCINT ( IMODEL )                                M35.4749
      LOCATE( 350)  =  LOCINT ( KBUS   )                                M35.4750
      LOCATE( 351)  =  LOCINT ( MBUS   )                                M35.4751
      LOCATE( 352)  =  LOCINT ( KODEBR )                                M35.4752
      LOCATE( 353)  =  LOCINT ( CKI    )                                M35.4753
      LOCATE( 354)  =  LOCINT ( CKKJM  )                                M35.4754
      LOCATE( 355)  =  LOCINT ( INDHST )                                M35.4755
      LOCATE( 356)  =  LOCINT ( KODSEM )                                M35.4756
      LOCATE( 357)  =  LOCINT ( BRNAME )                                M35.4757
      LOCATE( 358)  =  LOCINT ( IFORM  )                                M35.4758
      LOCATE( 359)  =  LOCINT ( NODE   )                                M35.4759
      LOCATE( 360)  =  LOCINT ( CREST  )                                M35.4760
      LOCATE( 361)  =  LOCINT ( TIME1  )                                M35.4761
      LOCATE( 362)  =  LOCINT ( TIME2  )                                M35.4762
      LOCATE( 363)  =  LOCINT ( TSTART )                                M35.4763
      LOCATE( 364)  =  LOCINT ( SFREQ  )                                M35.4764
      LOCATE( 365)  =  LOCINT ( KMSWIT )                                M35.4765
      LOCATE( 366)  =  LOCINT ( NEXTSW )                                M35.4766
      LOCATE( 367)  =  LOCINT ( RMFD   )                                M35.4767
      LOCATE( 368)  =  LOCINT ( CIKFD  )                                M35.4768
      LOCATE( 369)  =  LOCINT ( IMFD   )                                M35.4769
      LOCATE( 370)  =  LOCINT ( TCLOSE )                                M35.4770
      LOCATE( 371)  =  LOCINT ( ADELAY )                                M35.4771
      LOCATE( 372)  =  LOCINT ( KPOS   )                                M35.4772
      LOCATE( 373)  =  LOCINT ( E      )                                M35.4773
      LOCATE( 374)  =  LOCINT ( F      )                                M35.4774
      LOCATE( 375)  =  LOCINT ( KSSFRQ )                                M35.4775
      LOCATE( 376)  =  LOCINT ( KODE   )                                M35.4776
      LOCATE( 377)  =  LOCINT ( KPSOUR )                                M35.4777
      LOCATE( 378)  =  LOCINT ( VOLTI  )                                M35.4778
      LOCATE( 379)  =  LOCINT ( VOLTK  )                                M35.4779
      LOCATE( 380)  =  LOCINT ( VOLT   )                                M35.4780
      LOCATE( 381)  =  LOCINT ( BUS    )                                M35.4781
      LOCATE( 382)  =  LOCINT ( ELD    )                                M35.4782
      LOCATE( 383)  =  LOCINT ( ELAF   )                                M35.4783
      LOCATE( 384)  =  LOCINT ( ELF    )                                M35.4784
      LOCATE( 385)  =  LOCINT ( ELAKD  )                                M35.4785
      LOCATE( 386)  =  LOCINT ( ELFKD  )                                M35.4786
      LOCATE( 387)  =  LOCINT ( ELKD   )                                M35.4787
      LOCATE( 388)  =  LOCINT ( ELQ    )                                M35.4788
      LOCATE( 389)  =  LOCINT ( ELAG   )                                M35.4789
      LOCATE( 390)  =  LOCINT ( ELG    )                                M35.4790
      LOCATE( 391)  =  LOCINT ( ELAKQ  )                                M35.4791
      LOCATE( 392)  =  LOCINT ( ELGKQ  )                                M35.4792
      LOCATE( 393)  =  LOCINT ( ELKQ   )                                M35.4793
      LOCATE( 394)  =  LOCINT ( EL0    )                                M35.4794
      LOCATE( 395)  =  LOCINT ( RA     )                                M35.4795
      LOCATE( 396)  =  LOCINT ( RF     )                                M35.4796
      LOCATE( 397)  =  LOCINT ( RKD    )                                M35.4797
      LOCATE( 398)  =  LOCINT ( RG     )                                M35.4798
      LOCATE( 399)  =  LOCINT ( RKQ    )                                M35.4799
      LOCATE( 400)  =  LOCINT ( R0     )                                M35.4800
      LOCATE( 401)  =  LOCINT ( AGLINE )                                M35.4801
      LOCATE( 402)  =  LOCINT ( RAT1   )                                M35.4802
      LOCATE( 403)  =  LOCINT ( SMOUTP )                                M35.4803
      LOCATE( 404)  =  LOCINT ( SMOUTQ )                                M35.4804
      LOCATE( 405)  =  LOCINT ( TEG    )                                M35.4805
      LOCATE( 406)  =  LOCINT ( TEXC   )                                M35.4806
      LOCATE( 407)  =  LOCINT ( CNP    )                                M35.4807
      LOCATE( 408)  =  LOCINT ( A22    )                                M35.4808
      LOCATE( 409)  =  LOCINT ( A12    )                                M35.4809
      LOCATE( 410)  =  LOCINT ( A21    )                                M35.4810
      LOCATE( 411)  =  LOCINT ( AC     )                                M35.4811
      LOCATE( 412)  =  LOCINT ( AI     )                                M35.4812
      LOCATE( 413)  =  LOCINT ( AT     )                                M35.4813
      LOCATE( 414)  =  LOCINT ( AH     )                                M35.4814
      LOCATE( 415)  =  LOCINT ( XAY    )                                M35.4815
      LOCATE( 416)  =  LOCINT ( CU     )                                M35.4816
      LOCATE( 417)  =  LOCINT ( CV     )                                M35.4817
      LOCATE( 418)  =  LOCINT ( DSAT   )                                M35.4818
      LOCATE( 419)  =  LOCINT ( QSAT   )                                M35.4819
      LOCATE( 420)  =  LOCINT ( ACR    )                                M35.4820
      LOCATE( 421)  =  LOCINT ( CE     )                                M35.4821
      LOCATE( 422)  =  LOCINT ( DSR    )                                M35.4822
      LOCATE( 423)  =  LOCINT ( DSD    )                                M35.4823
      LOCATE( 424)  =  LOCINT ( HICO   )                                M35.4824
      LOCATE( 425)  =  LOCINT ( DSM    )                                M35.4825
      LOCATE( 426)  =  LOCINT ( HSP    )                                M35.4826
      LOCATE( 427)  =  LOCINT ( POWER  )                                M35.4827
      LOCATE( 428)  =  LOCINT ( EXTRS  )                                M35.4828
      LOCATE( 429)  =  LOCINT ( HISTQ  )                                M35.4829
      LOCATE( 430)  =  LOCINT ( HISTR  )                                M35.4830
      LOCATE( 431)  =  LOCINT ( YFOR   )                                M35.4831
      LOCATE( 432)  =  LOCINT ( ZSK    )                                M35.4832
      LOCATE( 433)  =  LOCINT ( Y      )                                M35.4833
      LOCATE( 434)  =  LOCINT ( TORK   )                                M35.4834
      LOCATE( 435)  =  LOCINT ( TEMP   )                                M35.4835
      LOCATE( 436)  =  LOCINT ( Z      )                                M35.4836
      LOCATE( 437)  =  LOCINT ( X1     )                                M35.4837
      LOCATE( 438)  =  LOCINT ( SQRT3  )                                M35.4838
      LOCATE( 439)  =  LOCINT ( ASQRT3 )                                M35.4839
      LOCATE( 440)  =  LOCINT ( SQRT32 )                                M35.4840
      LOCATE( 441)  =  LOCINT ( THTW   )                                M35.4841
      LOCATE( 442)  =  LOCINT ( ATHTW  )                                M35.4842
      LOCATE( 443)  =  LOCINT ( RADEG  )                                M35.4843
      LOCATE( 444)  =  LOCINT ( OMDT   )                                M35.4844
      LOCATE( 445)  =  LOCINT ( FACTOM )                                M35.4845
      LOCATE( 446)  =  LOCINT ( DAMRAT )                                M35.4846
      LOCATE( 447)  =  LOCINT ( ISAT   )                                M35.4847
      LOCATE( 448)  =  LOCINT ( ISED   )                                M35.4848
      LOCATE( 449)  =  LOCINT ( ISEQ   )                                M35.4849
      LOCATE( 450)  =  LOCINT ( IMDUAL )                                M35.4850
      LOCATE( 451)  =  LOCINT ( ICONFG )                                M35.4851
      LOCATE( 452)  =  LOCINT ( KMAC   )                                M35.4852
      LOCATE( 453)  =  LOCINT ( KEXC   )                                M35.4853
      LOCATE( 454)  =  LOCINT ( NUMAS  )                                M35.4854
      LOCATE( 455)  =  LOCINT ( NODMA  )                                M35.4855
      LOCATE( 456)  =  LOCINT ( NODMB  )                                M35.4856
      LOCATE( 457)  =  LOCINT ( NODMC  )                                M35.4857
      LOCATE( 458)  =  LOCINT ( JASMIT )                                M35.4858
      LOCATE( 459)  =  LOCINT ( JSMTOR )                                M35.4859
      LOCATE( 460)  =  LOCINT ( JEXCIT )                                M35.4860
      LOCATE( 461)  =  LOCINT ( ISLOC  )                                M35.4861
      LOCATE( 462)  =  LOCINT ( NOUTSM )                                M35.4862
      LOCATE( 463)  =  LOCINT ( ISMOUT )                                M35.4863
      LOCATE( 464)  =  LOCINT ( MFIRST )                                M35.4864
      LOCATE( 465)  =  LOCINT ( LIMASS )                                M35.4865
      LOCATE( 466)  =  LOCINT ( NST    )                                M35.4866
      LOCATE( 467)  =  LOCINT ( ITOLD  )                                M35.4867
      LOCATE( 468)  =  LOCINT ( IBROLD )                                M35.4868
      LOCATE( 469)  =  LOCINT ( BUSUM  )                                M35.4869
      LOCATE( 470)  =  LOCINT ( PTHETA )                                M35.4870
      LOCATE( 471)  =  LOCINT ( ZTHEVR )                                M35.4871
      LOCATE( 472)  =  LOCINT ( VINP   )                                M35.4872
      LOCATE( 473)  =  LOCINT ( ZTHEVS )                                M35.4873
      LOCATE( 474)  =  LOCINT ( UMCUR  )                                M35.4874
      LOCATE( 475)  =  LOCINT ( CON    )                                M35.4875
      LOCATE( 476)  =  LOCINT ( DUMVEC )                                M35.4876
      LOCATE( 477)  =  LOCINT ( DUMMAT )                                M35.4877
      LOCATE( 478)  =  LOCINT ( DATE   )                                M35.4878
      LOCATE( 479)  =  LOCINT ( CLOCK  )                                M35.4879
      LOCATE( 480)  =  LOCINT ( PI     )                                M35.4880
      LOCATE( 481)  =  LOCINT ( SROOT2 )                                M35.4881
      LOCATE( 482)  =  LOCINT ( SROOT3 )                                M35.4882
      LOCATE( 483)  =  LOCINT ( OMEGRF )                                M35.4883
      LOCATE( 484)  =  LOCINT ( INPU   )                                M35.4884
      LOCATE( 485)  =  LOCINT ( NUMBUS )                                M35.4885
      LOCATE( 486)  =  LOCINT ( NCLTOT )                                M35.4886
      LOCATE( 487)  =  LOCINT ( NDUM   )                                M35.4887
      LOCATE( 488)  =  LOCINT ( INITUM )                                M35.4888
      LOCATE( 489)  =  LOCINT ( IUREAC )                                M35.4889
      LOCATE( 490)  =  LOCINT ( IUGPAR )                                M35.4890
      LOCATE( 491)  =  LOCINT ( IUFPAR )                                M35.4891
      LOCATE( 492)  =  LOCINT ( IUHIST )                                M35.4892
      LOCATE( 493)  =  LOCINT ( IUUMRP )                                M35.4893
      LOCATE( 494)  =  LOCINT ( IUNOD1 )                                M35.4894
      LOCATE( 495)  =  LOCINT ( IUNOD2 )                                M35.4895
      LOCATE( 496)  =  LOCINT ( IUJCLT )                                M35.4896
      LOCATE( 497)  =  LOCINT ( IUJCLO )                                M35.4897
      LOCATE( 498)  =  LOCINT ( IUJTYP )                                M35.4898
      LOCATE( 499)  =  LOCINT ( IUNODO )                                M35.4899
      LOCATE( 500)  =  LOCINT ( IUJTMT )                                M35.4900
      LOCATE( 501)  =  LOCINT ( IUHISM )                                M35.4901
      LOCATE( 502)  =  LOCINT ( IUOMGM )                                M35.4902
      LOCATE( 503)  =  LOCINT ( IUOMLD )                                M35.4903
      LOCATE( 504)  =  LOCINT ( IUTHAM )                                M35.4904
      LOCATE( 505)  =  LOCINT ( IUREDU )                                M35.4905
      LOCATE( 506)  =  LOCINT ( IUREDS )                                M35.4906
      LOCATE( 507)  =  LOCINT ( IUFLDS )                                M35.4907
      LOCATE( 508)  =  LOCINT ( IUFLDR )                                M35.4908
      LOCATE( 509)  =  LOCINT ( IUREQU )                                M35.4909
      LOCATE( 510)  =  LOCINT ( IUFLQS )                                M35.4910
      LOCATE( 511)  =  LOCINT ( IUFLQR )                                M35.4911
      LOCATE( 512)  =  LOCINT ( IUJCDS )                                M35.4912
      LOCATE( 513)  =  LOCINT ( IUJCQS )                                M35.4913
      LOCATE( 514)  =  LOCINT ( IUFLXD )                                M35.4914
      LOCATE( 515)  =  LOCINT ( IUFLXQ )                                M35.4915
      LOCATE( 516)  =  LOCINT ( IUNPPA )                                M35.4916
      LOCATE( 517)  =  LOCINT ( IUROTM )                                M35.4917
      LOCATE( 518)  =  LOCINT ( IUNCLD )                                M35.4918
      LOCATE( 519)  =  LOCINT ( IUNCLQ )                                M35.4919
      LOCATE( 520)  =  LOCINT ( IUJTQO )                                M35.4920
      LOCATE( 521)  =  LOCINT ( IUJOMO )                                M35.4921
      LOCATE( 522)  =  LOCINT ( IUJTHO )                                M35.4922
      LOCATE( 523)  =  LOCINT ( IUREQS )                                M35.4923
      LOCATE( 524)  =  LOCINT ( IUEPSO )                                M35.4924
      LOCATE( 525)  =  LOCINT ( IUDCOE )                                M35.4925
      LOCATE( 526)  =  LOCINT ( IUKCOI )                                M35.4926
      LOCATE( 527)  =  LOCINT ( IUVOLT )                                M35.4927
      LOCATE( 528)  =  LOCINT ( IUANGL )                                M35.4928
      LOCATE( 529)  =  LOCINT ( IUNODF )                                M35.4929
      LOCATE( 530)  =  LOCINT ( IUNODM )                                M35.4930
      LOCATE( 531)  =  LOCINT ( IUKUMO )                                M35.4931
      LOCATE( 532)  =  LOCINT ( IUJUMO )                                M35.4932
      LOCATE( 533)  =  LOCINT ( IUUMOU )                                M35.4933
      LOCATE( 534)  =  LOCINT ( NCLFIX )                                M35.4934
      LOCATE( 535)  =  LOCINT ( NUMFIX )                                M35.4935
      LOCATE( 536)  =  LOCINT ( IOTFIX )                                M35.4936
      LOCATE( 537)  =  LOCINT ( IBSFIX )                                M35.4937
      LOCATE( 538)  =  LOCINT ( KSUBUM )                                M35.4938
      LOCATE( 539)  =  LOCINT ( NSMACH )                                M35.4939
      LOCATE( 540)  =  LOCINT ( ISTART )                                M35.4940
      LOCATE( 541)  =  LOCINT ( KARRAY )                                M35.4941
      LOCATE( 542)  =  LOCINT ( RAMPCN )                                M35.4942
      LOCATE( 543)  =  LOCINT ( RAMPSL )                                M35.4943
      LOCATE( 544)  =  LOCINT ( KYRAMP )                                M35.4944
      LOCATE( 545)  =  LOCINT ( TEXPAR )                                M35.4945
      LOCATE( 546)  =  LOCINT ( FENDRP )                                M35.4946
      LOCATE( 547)  =  LOCINT ( TMINRP )                                M35.4947
      LOCATE( 548)  =  LOCINT ( TMAXRP )                                M35.4948
      LOCATE( 549)  =  LOCINT ( TBEGRP )                                M35.4949
      LOCATE( 550)  =  LOCINT ( TENDRP )                                M35.4950
      LOCATE( 551)  =  LOCINT ( FBEGRP )                                M35.4951
      LOCATE( 552)  =  LOCINT ( TBREAK )                                M35.4952
      LOCATE( 553)  =  LOCINT ( INDXRP )                                M35.4953
      LOCATE( 554)  =  LOCINT ( IVEC   )                                M35.4954
      LOCATE( 555)  =  LOCINT ( IASCII )                                M35.4955
      LOCATE( 556)  =  LOCINT ( NUMSYM )                                M35.4956
      LOCATE( 557)  =  LOCINT ( JJROLL )                                M35.4957
      LOCATE( 558)  =  LOCINT ( ITEXP  )                                M35.4958
      LOCATE( 559)  =  LOCINT ( LABELS )                                M35.4959
      LOCATE( 560)  =  LOCINT ( MAXARG )                                M35.4960
      LOCATE( 561)  =  LOCINT ( KILPER )                                M35.4961
      LOCATE( 562)  =  LOCINT ( KFILE5 )                                M35.4962
      LOCATE( 563)  =  LOCINT ( KVERFY )                                M35.4963
      LOCATE( 564)  =  LOCINT ( IBEGCL )                                M35.4964
      LOCATE( 565)  =  LOCINT ( IENDCL )                                M35.4965
      LOCATE( 566)  =  LOCINT ( LIDNT1 )                                M35.4966
      LOCATE( 567)  =  LOCINT ( LIDNT2 )                                M35.4967
      LOCATE( 568)  =  LOCINT ( LINNOW )                                M35.4968
      LOCATE( 569)  =  LOCINT ( LINSPN )                                M35.4969
      LOCATE( 570)  =  LOCINT ( NUMCRD )                                M35.4970
      LOCATE( 571)  =  LOCINT ( MUNIT5 )                                M35.4971
      LOCATE( 572)  =  LOCINT ( INDBUF )                                M35.4972
      LOCATE( 573)  =  LOCINT ( INDBEG )                                M35.4973
      LOCATE( 574)  =  LOCINT ( MFLUSH )                                M35.4974
      LOCATE( 575)  =  LOCINT ( NEWVEC )                                M35.4975
      LOCATE( 576)  =  LOCINT ( MUNIT6 )                                M35.4976
      LOCATE( 577)  =  LOCINT ( LSERLC )                                M35.4977
      LOCATE( 578)  =  LOCINT ( KSERLC )                                M35.4978
      LOCATE( 579)  =  LOCINT ( KBRSER )                                M35.4979
      LOCATE( 580)  =  LOCINT ( LOCKBR )                                M35.4980
      LOCATE( 581)  =  LOCINT ( IPRSPY )                                M35.4981
      LOCATE( 582)  =  LOCINT ( MONITR )                                M35.4982
      LOCATE( 583)  =  LOCINT ( MONITS )                                M35.4983
      LOCATE( 584)  =  LOCINT ( LOCATE )                                M35.4984
      LOCATE( 585)  =  LOCINT ( NLINE  )                                M35.4985
      LOCATE( 586)  =  LOCINT ( KWTSPY )                                M35.4986
      LOCATE( 587)  =  LOCINT ( KBREAK )                                M35.4987
      LOCATE( 588)  =  LOCINT ( LIMBUF )                                M35.4988
      LOCATE( 589)  =  LOCINT ( INCHLP )                                M35.4989
      LOCATE( 590)  =  LOCINT ( KSYMBL )                                M35.4990
      LOCATE( 591)  =  LOCINT ( KOPYIT )                                M35.4991
      LOCATE( 592)  =  LOCINT ( KSLOWR )                                M35.4992
      LOCATE( 593)  =  LOCINT ( LIMCRD )                                M35.4993
      LOCATE( 594)  =  LOCINT ( LOOPRP )                                M35.4994
      LOCATE( 595)  =  LOCINT ( N10RMP )                                M35.4995
      LOCATE( 596)  =  LOCINT ( MEMRMP )                                M35.4996
      LOCATE( 597)  =  LOCINT ( KAR1   )                                M35.4997
      LOCATE( 598)  =  LOCINT ( KAR2   )                                M35.4998
      LOCATE( 599)  =  LOCINT ( NUMRMP )                                M35.4999
      LOCATE( 600)  =  LOCINT ( LUNTSP )                                M35.5000
      LOCATE( 601)  =  LOCINT ( LOGVAR )                                M35.5001
      LOCATE( 602)  =  LOCINT ( FILEXT )                                M35.5002
      LOCATE( 603)  =  LOCINT ( SYMB   )                                M35.5003
      LOCATE( 604)  =  LOCINT ( COL    )                                M35.5004
      LOCATE( 605)  =  LOCINT ( BYTFND )                                M35.5005
      LOCATE( 606)  =  LOCINT ( CHAR1  )                                M35.5006
      LOCATE( 607)  =  LOCINT ( SYMBRP )                                M35.5007
      LOCATE( 608)  =  LOCINT ( CHARD4 )                                M35.5008
      LOCATE( 609)  =  LOCINT ( BYTBUF )                                M35.5009
      LOCATE( 610)  =  LOCINT ( BUFF77 )                                M35.5010
      LOCATE( 611)  =  LOCINT ( FILE6B )                                M35.5011
      LOCATE( 612)  =  LOCINT ( FILE6  )                                M35.5012
      LOCATE( 613)  =  LOCINT ( BLAN80 )                                M35.5013
      LOCATE( 614)  =  LOCINT ( PROM80 )                                M35.5014
      LOCATE( 615)  =  LOCINT ( DIGIT  )                                M35.5015
      LOCATE( 616)  =  LOCINT ( IAC    )                                M35.5016
      LOCATE( 617)  =  LOCINT ( IDCTCS )                                M35.5017
      LOCATE( 618)  =  LOCINT ( IPL    )                                M35.5018
      LOCATE( 619)  =  LOCINT ( IPR    )                                M35.5019
      LOCATE( 620)  =  LOCINT ( IXR    )                                M35.5020
      LOCATE( 621)  =  LOCINT ( JPL    )                                M35.5021
      LOCATE( 622)  =  LOCINT ( JPR    )                                M35.5022
      LOCATE( 623)  =  LOCINT ( KINT   )                                M35.5023
      LOCATE( 624)  =  LOCINT ( KOUT   )                                M35.5024
      LOCATE( 625)  =  LOCINT ( NDS    )                                M35.5025
      LOCATE( 626)  =  LOCINT ( NKN    )                                M35.5026
      LOCATE( 627)  =  LOCINT ( NMAX   )                                M35.5027
      LOCATE( 628)  =  LOCINT ( NUK    )                                M35.5028
      LOCATE( 629)  =  LOCINT ( KWRITE )                                M35.5029
      LOCATE( 630)  =  LOCINT ( KPR    )                                M35.5030
      LOCATE( 631)  =  LOCINT ( KPL    )                                M35.5031
      LOCATE( 632)  =  LOCINT ( MXTACW )                                M35.5032
      LOCATE( 633)  =  LOCINT ( IPTACW )                                M35.5033
      LOCATE( 634)  =  LOCINT ( NHST   )                                M35.5034
      LOCATE( 635)  =  LOCINT ( KVIN   )                                M35.5035
      LOCATE( 636)  =  LOCINT ( KVOU   )                                M35.5036
      LOCATE( 637)  =  LOCINT ( KVXX   )                                M35.5037
      LOCATE( 638)  =  LOCINT ( ICSUP  )                                M35.5038
      LOCATE( 639)  =  LOCINT ( NXIC   )                                M35.5039
      LOCATE( 640)  =  LOCINT ( KKSJ   )                                M35.5040
      LOCATE( 641)  =  LOCINT ( KKSK   )                                M35.5041
      LOCATE( 642)  =  LOCINT ( KKFST  )                                M35.5042
      LOCATE( 643)  =  LOCINT ( KKNI   )                                M35.5043
      LOCATE( 644)  =  LOCINT ( KKHST  )                                M35.5044
      LOCATE( 645)  =  LOCINT ( KIFLS  )                                M35.5045
      LOCATE( 646)  =  LOCINT ( KIDUM  )                                M35.5046
      LOCATE( 647)  =  LOCINT ( KSLIM1 )                                M35.5047
      LOCATE( 648)  =  LOCINT ( KSLIM2 )                                M35.5048
      LOCATE( 649)  =  LOCINT ( KSLIM3 )                                M35.5049
      LOCATE( 650)  =  LOCINT ( KPAC1R )                                M35.5050
      LOCATE( 651)  =  LOCINT ( KPAC1I )                                M35.5051
      LOCATE( 652)  =  LOCINT ( KPAC2R )                                M35.5052
      LOCATE( 653)  =  LOCINT ( KPAC2I )                                M35.5053
      LOCATE( 654)  =  LOCINT ( KALKSX )                                M35.5054
      LOCATE( 655)  =  LOCINT ( KILMS1 )                                M35.5055
      LOCATE( 656)  =  LOCINT ( KILMS2 )                                M35.5056
      LOCATE( 657)  =  LOCINT ( KDUMJ  )                                M35.5057
      LOCATE( 658)  =  LOCINT ( KDUMK  )                                M35.5058
      LOCATE( 659)  =  LOCINT ( KKZJ   )                                M35.5059
      LOCATE( 660)  =  LOCINT ( KKZK   )                                M35.5060
      LOCATE( 661)  =  LOCINT ( KIFLZ  )                                M35.5061
      LOCATE( 662)  =  LOCINT ( KGNZ   )                                M35.5062
      LOCATE( 663)  =  LOCINT ( KZLIM1 )                                M35.5063
      LOCATE( 664)  =  LOCINT ( KZLIM2 )                                M35.5064
      LOCATE( 665)  =  LOCINT ( KALKZX )                                M35.5065
      LOCATE( 666)  =  LOCINT ( KILMZ1 )                                M35.5066
      LOCATE( 667)  =  LOCINT ( KILMZ2 )                                M35.5067
      LOCATE( 668)  =  LOCINT ( KKSUS  )                                M35.5068
      LOCATE( 669)  =  LOCINT ( KALKSU )                                M35.5069
      LOCATE( 670)  =  LOCINT ( KIUTY  )                                M35.5070
      LOCATE( 671)  =  LOCINT ( KUD1   )                                M35.5071
      LOCATE( 672)  =  LOCINT ( KUD2   )                                M35.5072
      LOCATE( 673)  =  LOCINT ( KUD3   )                                M35.5073
      LOCATE( 674)  =  LOCINT ( KUD4   )                                M35.5074
      LOCATE( 675)  =  LOCINT ( KUD5   )                                M35.5075
      LOCATE( 676)  =  LOCINT ( KALIU  )                                M35.5076
      LOCATE( 677)  =  LOCINT ( KTYSUP )                                M35.5077
      LOCATE( 678)  =  LOCINT ( KJSUP  )                                M35.5078
      LOCATE( 679)  =  LOCINT ( KKSUP  )                                M35.5079
      LOCATE( 680)  =  LOCINT ( KSPVAR )                                M35.5080
      LOCATE( 681)  =  LOCINT ( KOPSUP )                                M35.5081
      LOCATE( 682)  =  LOCINT ( KFNSUP )                                M35.5082
      LOCATE( 683)  =  LOCINT ( KRGSUP )                                M35.5083
      LOCATE( 684)  =  LOCINT ( KPRSUP )                                M35.5084
      LOCATE( 685)  =  LOCINT ( KTYPDV )                                M35.5085
      LOCATE( 686)  =  LOCINT ( KKDJ   )                                M35.5086
      LOCATE( 687)  =  LOCINT ( KKDK   )                                M35.5087
      LOCATE( 688)  =  LOCINT ( KGNDEV )                                M35.5088
      LOCATE( 689)  =  LOCINT ( KDEV1  )                                M35.5089
      LOCATE( 690)  =  LOCINT ( KDEV2  )                                M35.5090
      LOCATE( 691)  =  LOCINT ( KLDEV1 )                                M35.5091
      LOCATE( 692)  =  LOCINT ( KLDEV2 )                                M35.5092
      LOCATE( 693)  =  LOCINT ( KKDUS  )                                M35.5093
      LOCATE( 694)  =  LOCINT ( KALKDU )                                M35.5094
      LOCATE( 695)  =  LOCINT ( KTBDEV )                                M35.5095
      LOCATE( 696)  =  LOCINT ( KPN    )                                M35.5096
      LOCATE( 697)  =  LOCINT ( KPD    )                                M35.5097
      LOCATE( 698)  =  LOCINT ( KXHST  )                                M35.5098
      LOCATE( 699)  =  LOCINT ( KHSCR  )                                M35.5099
      LOCATE( 700)  =  LOCINT ( KHSCI  )                                M35.5100
      LOCATE( 701)  =  LOCINT ( KILIM1 )                                M35.5101
      LOCATE( 702)  =  LOCINT ( KILIM2 )                                M35.5102
      LOCATE( 703)  =  LOCINT ( KROWCS )                                M35.5103
      LOCATE( 704)  =  LOCINT ( KRHSDE )                                M35.5104
      LOCATE( 705)  =  LOCINT ( KVLIM1 )                                M35.5105
      LOCATE( 706)  =  LOCINT ( KVLIM2 )                                M35.5106
      LOCATE( 707)  =  LOCINT ( KKXIC  )                                M35.5107
      LOCATE( 708)  =  LOCINT ( KAWKCS )                                M35.5108
      LOCATE( 709)  =  LOCINT ( KXAR   )                                M35.5109
      LOCATE( 710)  =  LOCINT ( KXAI   )                                M35.5110
      LOCATE( 711)  =  LOCINT ( KBWKCS )                                M35.5111
      LOCATE( 712)  =  LOCINT ( KXTCS  )                                M35.5112
      LOCATE( 713)  =  LOCINT ( KLNTAB )                                M35.5113
      LOCATE( 714)  =  LOCINT ( KLMXIC )                                M35.5114
      LOCATE( 715)  =  LOCINT ( KCOLCS )                                M35.5115
      LOCATE( 716)  =  LOCINT ( KATCS  )                                M35.5116
      LOCATE( 717)  =  LOCINT ( KBTCS  )                                M35.5117
      LOCATE( 718)  =  LOCINT ( KJOUT  )                                M35.5118
      LOCATE( 719)  =  LOCINT ( KKOUT  )                                M35.5119
      LOCATE( 720)  =  LOCINT ( KXMNCS )                                M35.5120
      LOCATE( 721)  =  LOCINT ( KTXMN  )                                M35.5121
      LOCATE( 722)  =  LOCINT ( KXMXCS )                                M35.5122
      LOCATE( 723)  =  LOCINT ( KTXMX  )                                M35.5123
      LOCATE( 724)  =  LOCINT ( KLNOUT )                                M35.5124
      LOCATE( 725)  =  LOCINT ( EKBUF  )                                M35.5125
      LOCATE( 726)  =  LOCINT ( EKTEMP )                                M35.5126
      LOCATE( 727)  =  LOCINT ( ERRCHK )                                M35.5127
      LOCATE( 728)  =  LOCINT ( SOLRSV )                                M35.5128
      LOCATE( 729)  =  LOCINT ( SOLISV )                                M35.5129
      LOCATE( 730)  =  LOCINT ( NITERA )                                M35.5130
      LOCATE( 731)  =  LOCINT ( NEKREQ )                                M35.5131
      LOCATE( 732)  =  LOCINT ( NEKCOD )                                M35.5132
      RETURN                                                            M35.5133
      END                                                               M35.5134
      BLOCK DATA                                                        M35.5135
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      DATA SYMB(  1) / 'BUS1  '/,  IVEC(  1) /  0/,  IASCII(  1) /1/    M35.5137
      DATA SYMB(  2) / 'BUS2  '/,  IVEC(  2) /  0/,  IASCII(  2) /1/    M35.5138
      DATA SYMB(  3) / 'BUS3  '/,  IVEC(  3) /  0/,  IASCII(  3) /1/    M35.5139
      DATA SYMB(  4) / 'BUS4  '/,  IVEC(  4) /  0/,  IASCII(  4) /1/    M35.5140
      DATA SYMB(  5) / 'BUS5  '/,  IVEC(  5) /  0/,  IASCII(  5) /1/    M35.5141
      DATA SYMB(  6) / 'BUS6  '/,  IVEC(  6) /  0/,  IASCII(  6) /1/    M35.5142
      DATA SYMB(  7) / 'TRASH '/,  IVEC(  7) /  0/,  IASCII(  7) /1/    M35.5143
      DATA SYMB(  8) / 'BLANK '/,  IVEC(  8) /  0/,  IASCII(  8) /1/    M35.5144
      DATA SYMB(  9) / 'TERRA '/,  IVEC(  9) /  0/,  IASCII(  9) /1/    M35.5145
      DATA SYMB( 10) / 'USERID'/,  IVEC( 10) /  0/,  IASCII( 10) /1/    M35.5146
      DATA SYMB( 11) / 'BRANCH'/,  IVEC( 11) /  0/,  IASCII( 11) /1/    M35.5147
      DATA SYMB( 12) / 'COPY  '/,  IVEC( 12) /  0/,  IASCII( 12) /1/    M35.5148
      DATA SYMB( 13) / 'CSEPAR'/,  IVEC( 13) /  0/,  IASCII( 13) /1/    M35.5149
      DATA SYMB( 14) / 'CHCONT'/,  IVEC( 14) /  0/,  IASCII( 14) /1/    M35.5150
      DATA SYMB( 15) / 'TEXCOL'/,  IVEC( 15) /  1/,  IASCII( 15) /1/    M35.5151
      DATA SYMB( 16) / 'TEXTA6'/,  IVEC( 16) /  1/,  IASCII( 16) /1/    M35.5152
      DATA SYMB( 17) / 'DATE1 '/,  IVEC( 17) /  1/,  IASCII( 17) /1/    M35.5153
      DATA SYMB( 18) / 'TCLOCK'/,  IVEC( 18) /  1/,  IASCII( 18) /1/    M35.5154
      DATA SYMB( 19) / 'VSTACS'/,  IVEC( 19) /  1/,  IASCII( 19) /1/    M35.5155
      DATA SYMB( 20) / 'ABUFF '/,  IVEC( 20) /  1/,  IASCII( 20) /1/    M35.5156
      DATA SYMB( 21) / 'CI1   '/,  IVEC( 21) /  0/,  IASCII( 21) /0/    M35.5157
      DATA SYMB( 22) / 'CK1   '/,  IVEC( 22) /  0/,  IASCII( 22) /0/    M35.5158
      DATA SYMB( 23) / 'DELTAT'/,  IVEC( 23) /  0/,  IASCII( 23) /0/    M35.5159
      DATA SYMB( 24) / 'DELTA2'/,  IVEC( 24) /  0/,  IASCII( 24) /0/    M35.5160
      DATA SYMB( 25) / 'FREQCS'/,  IVEC( 25) /  0/,  IASCII( 25) /0/    M35.5161
      DATA SYMB( 26) / 'EPSILN'/,  IVEC( 26) /  0/,  IASCII( 26) /0/    M35.5162
      DATA SYMB( 27) / 'XUNITS'/,  IVEC( 27) /  0/,  IASCII( 27) /0/    M35.5163
      DATA SYMB( 28) / 'AINCR '/,  IVEC( 28) /  0/,  IASCII( 28) /0/    M35.5164
      DATA SYMB( 29) / 'XMAXMX'/,  IVEC( 29) /  0/,  IASCII( 29) /0/    M35.5165
      DATA SYMB( 30) / 'ZNVREF'/,  IVEC( 30) /  0/,  IASCII( 30) /0/    M35.5166
      DATA SYMB( 31) / 'EPSZNO'/,  IVEC( 31) /  0/,  IASCII( 31) /0/    M35.5167
      DATA SYMB( 32) / 'EPWARN'/,  IVEC( 32) /  0/,  IASCII( 32) /0/    M35.5168
      DATA SYMB( 33) / 'EPSTOP'/,  IVEC( 33) /  0/,  IASCII( 33) /0/    M35.5169
      DATA SYMB( 34) / 'T     '/,  IVEC( 34) /  0/,  IASCII( 34) /0/    M35.5170
      DATA SYMB( 35) / 'HERTZ '/,  IVEC( 35) /  0/,  IASCII( 35) /0/    M35.5171
      DATA SYMB( 36) / 'TOLMAT'/,  IVEC( 36) /  0/,  IASCII( 36) /0/    M35.5172
      DATA SYMB( 37) / 'TWOPI '/,  IVEC( 37) /  0/,  IASCII( 37) /0/    M35.5173
      DATA SYMB( 38) / 'TMAX  '/,  IVEC( 38) /  0/,  IASCII( 38) /0/    M35.5174
      DATA SYMB( 39) / 'OMEGA '/,  IVEC( 39) /  0/,  IASCII( 39) /0/    M35.5175
      DATA SYMB( 40) / 'COPT  '/,  IVEC( 40) /  0/,  IASCII( 40) /0/    M35.5176
      DATA SYMB( 41) / 'XOPT  '/,  IVEC( 41) /  0/,  IASCII( 41) /0/    M35.5177
      DATA SYMB( 42) / 'SZPLT '/,  IVEC( 42) /  0/,  IASCII( 42) /0/    M35.5178
      DATA SYMB( 43) / 'SZBED '/,  IVEC( 43) /  0/,  IASCII( 43) /0/    M35.5179
      DATA SYMB( 44) / 'SGLFIR'/,  IVEC( 44) /  0/,  IASCII( 44) /0/    M35.5180
      DATA SYMB( 45) / 'SIGMAX'/,  IVEC( 45) /  0/,  IASCII( 45) /0/    M35.5181
      DATA SYMB( 46) / 'EPSUBA'/,  IVEC( 46) /  0/,  IASCII( 46) /0/    M35.5182
      DATA SYMB( 47) / 'EPDGEL'/,  IVEC( 47) /  0/,  IASCII( 47) /0/    M35.5183
      DATA SYMB( 48) / 'EPOMEG'/,  IVEC( 48) /  0/,  IASCII( 48) /0/    M35.5184
      DATA SYMB( 49) / 'FMINFS'/,  IVEC( 49) /  0/,  IASCII( 49) /0/    M35.5185
      DATA SYMB( 50) / 'DELFFS'/,  IVEC( 50) /  0/,  IASCII( 50) /0/    M35.5186
      DATA SYMB( 51) / 'FMAXFS'/,  IVEC( 51) /  0/,  IASCII( 51) /0/    M35.5187
      DATA SYMB( 52) / 'TENERG'/,  IVEC( 52) /  0/,  IASCII( 52) /0/    M35.5188
      DATA SYMB( 53) / 'BEGMAX'/,  IVEC( 53) /  1/,  IASCII( 53) /0/    M35.5189
      DATA SYMB( 54) / 'TENM3 '/,  IVEC( 54) /  0/,  IASCII( 54) /0/    M35.5190
      DATA SYMB( 55) / 'TENM6 '/,  IVEC( 55) /  0/,  IASCII( 55) /0/    M35.5191
      DATA SYMB( 56) / 'UNITY '/,  IVEC( 56) /  0/,  IASCII( 56) /0/    M35.5192
      DATA SYMB( 57) / 'ONEHAF'/,  IVEC( 57) /  0/,  IASCII( 57) /0/    M35.5193
      DATA SYMB( 58) / 'PEAKND'/,  IVEC( 58) /  1/,  IASCII( 58) /0/    M35.5194
      DATA SYMB( 59) / 'FLTINF'/,  IVEC( 59) /  0/,  IASCII( 59) /0/    M35.5195
      DATA SYMB( 60) / 'FLZERO'/,  IVEC( 60) /  0/,  IASCII( 60) /0/    M35.5196
      DATA SYMB( 61) / 'DEGMIN'/,  IVEC( 61) /  0/,  IASCII( 61) /0/    M35.5197
      DATA SYMB( 62) / 'DEGMAX'/,  IVEC( 62) /  0/,  IASCII( 62) /0/    M35.5198
      DATA SYMB( 63) / 'STATFR'/,  IVEC( 63) /  0/,  IASCII( 63) /0/    M35.5199
      DATA SYMB( 64) / 'VOLTBC'/,  IVEC( 64) /  1/,  IASCII( 64) /0/    M35.5200
      DATA SYMB( 65) / 'FLSTAT'/,  IVEC( 65) /  1/,  IASCII( 65) /0/    M35.5201
      DATA SYMB( 66) / 'DTNEXT'/,  IVEC( 66) /  1/,  IASCII( 66) /0/    M35.5202
      DATA SYMB( 67) / 'ANGLE '/,  IVEC( 67) /  0/,  IASCII( 67) /0/    M35.5203
      DATA SYMB( 68) / 'PU    '/,  IVEC( 68) /  0/,  IASCII( 68) /0/    M35.5204
      DATA SYMB( 69) / 'SEEDR '/,  IVEC( 69) /  0/,  IASCII( 69) /0/    M35.5205
      DATA SYMB( 70) / 'SPEEDL'/,  IVEC( 70) /  0/,  IASCII( 70) /0/    M35.5206
      DATA SYMB( 71) / 'KSTART'/,  IVEC( 71) /  0/,  IASCII( 71) /0/    M35.5207
      DATA SYMB( 72) / 'KNT   '/,  IVEC( 72) /  0/,  IASCII( 72) /0/    M35.5208
      DATA SYMB( 73) / 'KBASE '/,  IVEC( 73) /  0/,  IASCII( 73) /0/    M35.5209
      DATA SYMB( 74) / 'LTDELT'/,  IVEC( 74) /  0/,  IASCII( 74) /0/    M35.5210
      DATA SYMB( 75) / 'UNUSED'/,  IVEC( 75) /  0/,  IASCII( 75) /0/    M37.7312
      DATA SYMB( 76) / 'MTAPE '/,  IVEC( 76) /  0/,  IASCII( 76) /0/    M35.5212
      DATA SYMB( 77) / 'LUNIT1'/,  IVEC( 77) /  0/,  IASCII( 77) /0/    M35.5213
      DATA SYMB( 78) / 'LUNIT2'/,  IVEC( 78) /  0/,  IASCII( 78) /0/    M35.5214
      DATA SYMB( 79) / 'LUNIT3'/,  IVEC( 79) /  0/,  IASCII( 79) /0/    M35.5215
      DATA SYMB( 80) / 'LUNIT4'/,  IVEC( 80) /  0/,  IASCII( 80) /0/    M35.5216
      DATA SYMB( 81) / 'LUNIT5'/,  IVEC( 81) /  0/,  IASCII( 81) /0/    M35.5217
      DATA SYMB( 82) / 'LUNIT6'/,  IVEC( 82) /  0/,  IASCII( 82) /0/    M35.5218
      DATA SYMB( 83) / 'LUNIT7'/,  IVEC( 83) /  0/,  IASCII( 83) /0/    M35.5219
      DATA SYMB( 84) / 'LUNIT8'/,  IVEC( 84) /  0/,  IASCII( 84) /0/    M35.5220
      DATA SYMB( 85) / 'LUNIT9'/,  IVEC( 85) /  0/,  IASCII( 85) /0/    M35.5221
      DATA SYMB( 86) / 'LUNT10'/,  IVEC( 86) /  0/,  IASCII( 86) /0/    M35.5222
      DATA SYMB( 87) / 'LUNT11'/,  IVEC( 87) /  0/,  IASCII( 87) /0/    M35.5223
      DATA SYMB( 88) / 'LUNT12'/,  IVEC( 88) /  0/,  IASCII( 88) /0/    M35.5224
      DATA SYMB( 89) / 'LUNT13'/,  IVEC( 89) /  0/,  IASCII( 89) /0/    M35.5225
      DATA SYMB( 90) / 'LUNT14'/,  IVEC( 90) /  0/,  IASCII( 90) /0/    M35.5226
      DATA SYMB( 91) / 'LUNT15'/,  IVEC( 91) /  0/,  IASCII( 91) /0/    M35.5227
      DATA SYMB( 92) / 'NEXOUT'/,  IVEC( 92) /  1/,  IASCII( 92) /0/    M35.5228
      DATA SYMB( 93) / 'NRIGHT'/,  IVEC( 93) /  0/,  IASCII( 93) /0/    M35.5229
      DATA SYMB( 94) / 'NFRFLD'/,  IVEC( 94) /  0/,  IASCII( 94) /0/    M35.5230
      DATA SYMB( 95) / 'KOLBEG'/,  IVEC( 95) /  0/,  IASCII( 95) /0/    M35.5231
      DATA SYMB( 96) / 'KPRCHG'/,  IVEC( 96) /  1/,  IASCII( 96) /0/    M35.5232
      DATA SYMB( 97) / 'MULTPR'/,  IVEC( 97) /  1/,  IASCII( 97) /0/    M35.5233
      DATA SYMB( 98) / 'IPNTV '/,  IVEC( 98) /  1/,  IASCII( 98) /0/    M35.5234
      DATA SYMB( 99) / 'INDTV '/,  IVEC( 99) /  1/,  IASCII( 99) /0/    M35.5235
      DATA SYMB(100) / 'LSTAT '/,  IVEC(100) /  1/,  IASCII(100) /0/    M35.5236
      DATA SYMB(101) / 'NBYTE '/,  IVEC(101) /  1/,  IASCII(101) /0/    M35.5237
      DATA SYMB(102) / 'LUNSAV'/,  IVEC(102) /  1/,  IASCII(102) /0/    M35.5238
      DATA SYMB(103) / 'IPRSOV'/,  IVEC(103) /  1/,  IASCII(103) /0/    M35.5239
      DATA SYMB(104) / 'ICHECK'/,  IVEC(104) /  0/,  IASCII(104) /0/    M35.5240
      DATA SYMB(105) / 'UNUSED'/,  IVEC(105) /  0/,  IASCII(105) /0/    M37.7313
      DATA SYMB(106) / 'IEND  '/,  IVEC(106) /  0/,  IASCII(106) /0/    M35.5242
      DATA SYMB(107) / 'ILINE '/,  IVEC(107) /  0/,  IASCII(107) /0/    M35.5243
      DATA SYMB(108) / 'INONL '/,  IVEC(108) /  0/,  IASCII(108) /0/    M35.5244
      DATA SYMB(109) / 'IOLD  '/,  IVEC(109) /  0/,  IASCII(109) /0/    M35.5245
      DATA SYMB(110) / 'IOUT  '/,  IVEC(110) /  0/,  IASCII(110) /0/    M35.5246
      DATA SYMB(111) / 'IPRINT'/,  IVEC(111) /  0/,  IASCII(111) /0/    M35.5247
      DATA SYMB(112) / 'IPUNCH'/,  IVEC(112) /  0/,  IASCII(112) /0/    M35.5248
      DATA SYMB(113) / 'IREAD '/,  IVEC(113) /  0/,  IASCII(113) /0/    M35.5249
      DATA SYMB(114) / 'KOL132'/,  IVEC(114) /  0/,  IASCII(114) /0/    M35.5250
      DATA SYMB(115) / 'ISTEP '/,  IVEC(115) /  0/,  IASCII(115) /0/    M35.5251
      DATA SYMB(116) / 'UNUSED'/,  IVEC(116) /  0/,  IASCII(116) /0/    M37.7314
      DATA SYMB(117) / 'ITYPE '/,  IVEC(117) /  0/,  IASCII(117) /0/    M35.5253
      DATA SYMB(118) / 'IT1   '/,  IVEC(118) /  0/,  IASCII(118) /0/    M35.5254
      DATA SYMB(119) / 'IT2   '/,  IVEC(119) /  0/,  IASCII(119) /0/    M35.5255
      DATA SYMB(120) / 'IUPPER'/,  IVEC(120) /  0/,  IASCII(120) /0/    M35.5256
      DATA SYMB(121) / 'IZERO '/,  IVEC(121) /  0/,  IASCII(121) /0/    M35.5257
      DATA SYMB(122) / 'KCOUNT'/,  IVEC(122) /  0/,  IASCII(122) /0/    M35.5258
      DATA SYMB(123) / 'ISTEAD'/,  IVEC(123) /  0/,  IASCII(123) /0/    M35.5259
      DATA SYMB(124) / 'UNUSED'/,  IVEC(124) /  0/,  IASCII(124) /0/    M37.7315
      DATA SYMB(125) / 'LDATA '/,  IVEC(125) /  0/,  IASCII(125) /0/    M35.5261
      DATA SYMB(126) / 'LBRNCH'/,  IVEC(126) /  0/,  IASCII(126) /0/    M35.5262
      DATA SYMB(127) / 'LIMTXF'/,  IVEC(127) /  0/,  IASCII(127) /0/    M35.5263
      DATA SYMB(128) / 'MDEBUG'/,  IVEC(128) /  0/,  IASCII(128) /0/    M35.5264
      DATA SYMB(129) / 'LEXCT '/,  IVEC(129) /  0/,  IASCII(129) /0/    M35.5265
      DATA SYMB(130) / 'LBUS  '/,  IVEC(130) /  0/,  IASCII(130) /0/    M35.5266
      DATA SYMB(131) / 'LYMAT '/,  IVEC(131) /  0/,  IASCII(131) /0/    M35.5267
      DATA SYMB(132) / 'LSWTCH'/,  IVEC(132) /  0/,  IASCII(132) /0/    M35.5268
      DATA SYMB(133) / 'LNONL '/,  IVEC(133) /  0/,  IASCII(133) /0/    M35.5269
      DATA SYMB(134) / 'LCHAR '/,  IVEC(134) /  0/,  IASCII(134) /0/    M35.5270
      DATA SYMB(135) / 'M4PLOT'/,  IVEC(135) /  0/,  IASCII(135) /0/    M35.5271
      DATA SYMB(136) / 'LPAST '/,  IVEC(136) /  0/,  IASCII(136) /0/    M35.5272
      DATA SYMB(137) / 'LSMAT '/,  IVEC(137) /  0/,  IASCII(137) /0/    M35.5273
      DATA SYMB(138) / 'IPLOT '/,  IVEC(138) /  0/,  IASCII(138) /0/    M35.5274
      DATA SYMB(139) / 'NCOMP '/,  IVEC(139) /  0/,  IASCII(139) /0/    M35.5275
      DATA SYMB(140) / 'NV    '/,  IVEC(140) /  0/,  IASCII(140) /0/    M35.5276
      DATA SYMB(141) / 'LCOMP '/,  IVEC(141) /  0/,  IASCII(141) /0/    M35.5277
      DATA SYMB(142) / 'NUMSM '/,  IVEC(142) /  0/,  IASCII(142) /0/    M35.5278
      DATA SYMB(143) / 'IFDEP '/,  IVEC(143) /  0/,  IASCII(143) /0/    M35.5279
      DATA SYMB(144) / 'LTAILS'/,  IVEC(144) /  0/,  IASCII(144) /0/    M35.5280
      DATA SYMB(145) / 'LFDEP '/,  IVEC(145) /  0/,  IASCII(145) /0/    M35.5281
      DATA SYMB(146) / 'LWT   '/,  IVEC(146) /  0/,  IASCII(146) /0/    M35.5282
      DATA SYMB(147) / 'LAST  '/,  IVEC(147) /  0/,  IASCII(147) /0/    M35.5283
      DATA SYMB(148) / 'NPOWER'/,  IVEC(148) /  0/,  IASCII(148) /0/    M35.5284
      DATA SYMB(149) / 'MAXPE '/,  IVEC(149) /  0/,  IASCII(149) /0/    M35.5285
      DATA SYMB(150) / 'LPEAK '/,  IVEC(150) /  0/,  IASCII(150) /0/    M35.5286
      DATA SYMB(151) / 'NOUT  '/,  IVEC(151) /  0/,  IASCII(151) /0/    M35.5287
      DATA SYMB(152) / 'IV    '/,  IVEC(152) /  0/,  IASCII(152) /0/    M35.5288
      DATA SYMB(153) / 'INEOF '/,  IVEC(153) /  0/,  IASCII(153) /0/    M35.5289
      DATA SYMB(154) / 'KTRLSW'/,  IVEC(154) /  1/,  IASCII(154) /0/    M35.5290
      DATA SYMB(155) / 'NUM99 '/,  IVEC(155) /  0/,  IASCII(155) /0/    M35.5291
      DATA SYMB(156) / 'KPARTB'/,  IVEC(156) /  0/,  IASCII(156) /0/    M35.5292
      DATA SYMB(157) / 'LLBUFF'/,  IVEC(157) /  0/,  IASCII(157) /0/    M35.5293
      DATA SYMB(158) / 'KANAL '/,  IVEC(158) /  0/,  IASCII(158) /0/    M35.5294
      DATA SYMB(159) / 'NSMTH '/,  IVEC(159) /  0/,  IASCII(159) /0/    M35.5295
      DATA SYMB(160) / 'NTCSEX'/,  IVEC(160) /  0/,  IASCII(160) /0/    M35.5296
      DATA SYMB(161) / 'NSTACS'/,  IVEC(161) /  0/,  IASCII(161) /0/    M35.5297
      DATA SYMB(162) / 'KLOAEP'/,  IVEC(162) /  0/,  IASCII(162) /0/    M35.5298
      DATA SYMB(163) / 'LASTOV'/,  IVEC(163) /  0/,  IASCII(163) /0/    M35.5299
      DATA SYMB(164) / 'LTACST'/,  IVEC(164) /  0/,  IASCII(164) /0/    M35.5300
      DATA SYMB(165) / 'LHIST '/,  IVEC(165) /  0/,  IASCII(165) /0/    M35.5301
      DATA SYMB(166) / 'IFX   '/,  IVEC(166) /  0/,  IASCII(166) /0/    M35.5302
      DATA SYMB(167) / 'NDELTA'/,  IVEC(167) /  0/,  IASCII(167) /0/    M35.5303
      DATA SYMB(168) / 'IDELTA'/,  IVEC(168) /  0/,  IASCII(168) /0/    M35.5304
      DATA SYMB(169) / 'INECHO'/,  IVEC(169) /  0/,  IASCII(169) /0/    M35.5305
      DATA SYMB(170) / 'NOUTPR'/,  IVEC(170) /  0/,  IASCII(170) /0/    M35.5306
      DATA SYMB(171) / 'KTAB  '/,  IVEC(171) /  0/,  IASCII(171) /0/    M35.5307
      DATA SYMB(172) / 'JFLSOS'/,  IVEC(172) /  0/,  IASCII(172) /0/    M35.5308
      DATA SYMB(173) / 'NUMDCD'/,  IVEC(173) /  0/,  IASCII(173) /0/    M35.5309
      DATA SYMB(174) / 'NUMUM '/,  IVEC(174) /  0/,  IASCII(174) /0/    M35.5310
      DATA SYMB(175) / 'LSPCUM'/,  IVEC(175) /  0/,  IASCII(175) /0/    M35.5311
      DATA SYMB(176) / 'NPHCAS'/,  IVEC(176) /  0/,  IASCII(176) /0/    M35.5312
      DATA SYMB(177) / 'LOCZ11'/,  IVEC(177) /  0/,  IASCII(177) /0/    M35.5313
      DATA SYMB(178) / 'LOCBR1'/,  IVEC(178) /  0/,  IASCII(178) /0/    M35.5314
      DATA SYMB(179) / 'IALTER'/,  IVEC(179) /  0/,  IASCII(179) /0/    M35.5315
      DATA SYMB(180) / 'ICHAR '/,  IVEC(180) /  0/,  IASCII(180) /0/    M35.5316
      DATA SYMB(181) / 'KTREF '/,  IVEC(181) /  0/,  IASCII(181) /0/    M35.5317
      DATA SYMB(182) / 'KPH   '/,  IVEC(182) /  0/,  IASCII(182) /0/    M35.5318
      DATA SYMB(183) / 'KREQAB'/,  IVEC(183) /  0/,  IASCII(183) /0/    M35.5319
      DATA SYMB(184) / 'KSAT  '/,  IVEC(184) /  0/,  IASCII(184) /0/    M35.5320
      DATA SYMB(185) / 'MEMSAV'/,  IVEC(185) /  0/,  IASCII(185) /0/    M35.5321
      DATA SYMB(186) / 'LISOFF'/,  IVEC(186) /  0/,  IASCII(186) /0/    M35.5322
      DATA SYMB(187) / 'LSPOV4'/,  IVEC(187) /  0/,  IASCII(187) /0/    M35.5323
      DATA SYMB(188) / 'KBURRO'/,  IVEC(188) /  0/,  IASCII(188) /0/    M35.5324
      DATA SYMB(189) / 'IAVERG'/,  IVEC(189) /  0/,  IASCII(189) /0/    M35.5325
      DATA SYMB(190) / 'LSIZ23'/,  IVEC(190) /  0/,  IASCII(190) /0/    M35.5326
      DATA SYMB(191) / 'LSIZ26'/,  IVEC(191) /  0/,  IASCII(191) /0/    M35.5327
      DATA SYMB(192) / 'NUMOUT'/,  IVEC(192) /  0/,  IASCII(192) /0/    M35.5328
      DATA SYMB(193) / 'MOLDAT'/,  IVEC(193) /  0/,  IASCII(193) /0/    M35.5329
      DATA SYMB(194) / 'LSIZ27'/,  IVEC(194) /  0/,  IASCII(194) /0/    M35.5330
      DATA SYMB(195) / 'LTLABL'/,  IVEC(195) /  0/,  IASCII(195) /0/    M35.5331
      DATA SYMB(196) / 'IWT   '/,  IVEC(196) /  0/,  IASCII(196) /0/    M35.5332
      DATA SYMB(197) / 'IFDEP2'/,  IVEC(197) /  0/,  IASCII(197) /0/    M35.5333
      DATA SYMB(198) / 'IDOUBL'/,  IVEC(198) /  0/,  IASCII(198) /0/    M35.5334
      DATA SYMB(199) / 'IOUTIN'/,  IVEC(199) /  0/,  IASCII(199) /0/    M35.5335
      DATA SYMB(200) / 'IPUN  '/,  IVEC(200) /  0/,  IASCII(200) /0/    M35.5336
      DATA SYMB(201) / 'JST   '/,  IVEC(201) /  0/,  IASCII(201) /0/    M35.5337
      DATA SYMB(202) / 'JST1  '/,  IVEC(202) /  0/,  IASCII(202) /0/    M35.5338
      DATA SYMB(203) / 'UNUSED'/,  IVEC(203) /  0/,  IASCII(203) /0/    M37.7316
      DATA SYMB(204) / 'NUMSUB'/,  IVEC(204) /  0/,  IASCII(204) /0/    M35.5340
      DATA SYMB(205) / 'MAXZNO'/,  IVEC(205) /  0/,  IASCII(205) /0/    M35.5341
      DATA SYMB(206) / 'KALPLT'/,  IVEC(206) /  0/,  IASCII(206) /0/    M35.5342
      DATA SYMB(207) / 'NIOMAX'/,  IVEC(207) /  0/,  IASCII(207) /0/    M35.5343
      DATA SYMB(208) / 'NIAMAX'/,  IVEC(208) /  0/,  IASCII(208) /0/    M35.5344
      DATA SYMB(209) / 'IBR1  '/,  IVEC(209) /  0/,  IASCII(209) /0/    M35.5345
      DATA SYMB(210) / 'IFSEM '/,  IVEC(210) /  0/,  IASCII(210) /0/    M35.5346
      DATA SYMB(211) / 'LFSEM '/,  IVEC(211) /  0/,  IASCII(211) /0/    M35.5347
      DATA SYMB(212) / 'IADD  '/,  IVEC(212) /  0/,  IASCII(212) /0/    M35.5348
      DATA SYMB(213) / 'LFD   '/,  IVEC(213) /  0/,  IASCII(213) /0/    M35.5349
      DATA SYMB(214) / 'LAUX  '/,  IVEC(214) /  0/,  IASCII(214) /0/    M35.5350
      DATA SYMB(215) / 'IOFGND'/,  IVEC(215) /  0/,  IASCII(215) /0/    M35.5351
      DATA SYMB(216) / 'IOFBND'/,  IVEC(216) /  0/,  IASCII(216) /0/    M35.5352
      DATA SYMB(217) / 'UNUSED'/,  IVEC(217) /  0/,  IASCII(217) /0/    M37.7317
      DATA SYMB(218) / 'JSEEDR'/,  IVEC(218) /  0/,  IASCII(218) /0/    M35.5354
      DATA SYMB(219) / 'MODOUT'/,  IVEC(219) /  0/,  IASCII(219) /0/    M35.5355
      DATA SYMB(220) / 'IFTAIL'/,  IVEC(220) /  0/,  IASCII(220) /0/    M35.5356
      DATA SYMB(221) / 'IPOINT'/,  IVEC(221) /  0/,  IASCII(221) /0/    M35.5357
      DATA SYMB(222) / 'LPAST2'/,  IVEC(222) /  0/,  IASCII(222) /0/    M35.5358
      DATA SYMB(223) / 'NCURR '/,  IVEC(223) /  0/,  IASCII(223) /0/    M35.5359
      DATA SYMB(224) / 'IOFFD '/,  IVEC(224) /  0/,  IASCII(224) /0/    M35.5360
      DATA SYMB(225) / 'ISPLOT'/,  IVEC(225) /  0/,  IASCII(225) /0/    M35.5361
      DATA SYMB(226) / 'ISPRIN'/,  IVEC(226) /  0/,  IASCII(226) /0/    M35.5362
      DATA SYMB(227) / 'MAXOUT'/,  IVEC(227) /  0/,  IASCII(227) /0/    M35.5363
      DATA SYMB(228) / 'IPOS  '/,  IVEC(228) /  0/,  IASCII(228) /0/    M35.5364
      DATA SYMB(229) / 'UNUSED'/,  IVEC(229) /  0/,  IASCII(229) /0/    M37.7318
      DATA SYMB(230) / 'UNUSED'/,  IVEC(230) /  0/,  IASCII(230) /0/    M37.7319
      DATA SYMB(231) / 'KILL  '/,  IVEC(231) /  0/,  IASCII(231) /0/    M35.5367
      DATA SYMB(232) / 'IVOLT '/,  IVEC(232) /  0/,  IASCII(232) /0/    M35.5368
      DATA SYMB(233) / 'NCHAIN'/,  IVEC(233) /  0/,  IASCII(233) /0/    M35.5369
      DATA SYMB(234) / 'IPRSUP'/,  IVEC(234) /  0/,  IASCII(234) /0/    M35.5370
      DATA SYMB(235) / 'UNUSED'/,  IVEC(235) /  0/,  IASCII(235) /0/    M37.7320
      DATA SYMB(236) / 'INTINF'/,  IVEC(236) /  0/,  IASCII(236) /0/    M35.5372
      DATA SYMB(237) / 'KCONST'/,  IVEC(237) /  0/,  IASCII(237) /0/    M35.5373
      DATA SYMB(238) / 'KSWTCH'/,  IVEC(238) /  0/,  IASCII(238) /0/    M35.5374
      DATA SYMB(239) / 'IT    '/,  IVEC(239) /  0/,  IASCII(239) /0/    M35.5375
      DATA SYMB(240) / 'NTOT  '/,  IVEC(240) /  0/,  IASCII(240) /0/    M35.5376
      DATA SYMB(241) / 'IBR   '/,  IVEC(241) /  0/,  IASCII(241) /0/    M35.5377
      DATA SYMB(242) / 'LCOM10'/,  IVEC(242) /  0/,  IASCII(242) /0/    M35.5378
      DATA SYMB(243) / 'LTRNST'/,  IVEC(243) /  0/,  IASCII(243) /0/    M35.5379
      DATA SYMB(244) / 'LSYN  '/,  IVEC(244) /  0/,  IASCII(244) /0/    M35.5380
      DATA SYMB(245) / 'KSSOUT'/,  IVEC(245) /  0/,  IASCII(245) /0/    M35.5381
      DATA SYMB(246) / 'LOOPSS'/,  IVEC(246) /  1/,  IASCII(246) /0/    M35.5382
      DATA SYMB(247) / 'INFEXP'/,  IVEC(247) /  0/,  IASCII(247) /0/    M35.5383
      DATA SYMB(248) / 'NUMREF'/,  IVEC(248) /  0/,  IASCII(248) /0/    M35.5384
      DATA SYMB(249) / 'NWORD1'/,  IVEC(249) /  0/,  IASCII(249) /0/    M35.5385
      DATA SYMB(250) / 'NWORD2'/,  IVEC(250) /  0/,  IASCII(250) /0/    M35.5386
      DATA SYMB(251) / 'ILOAEP'/,  IVEC(251) /  0/,  IASCII(251) /0/    M35.5387
      DATA SYMB(252) / 'LNPIN '/,  IVEC(252) /  0/,  IASCII(252) /0/    M35.5388
      DATA SYMB(253) / 'NTOT1 '/,  IVEC(253) /  0/,  IASCII(253) /0/    M35.5389
      DATA SYMB(254) / 'LIMSTP'/,  IVEC(254) /  0/,  IASCII(254) /0/    M35.5390
      DATA SYMB(255) / 'INDSTP'/,  IVEC(255) /  0/,  IASCII(255) /0/    M35.5391
      DATA SYMB(256) / 'NC    '/,  IVEC(256) /  0/,  IASCII(256) /0/    M35.5392
      DATA SYMB(257) / 'UNUSED'/,  IVEC(257) /  0/,  IASCII(257) /0/    M37.7321
      DATA SYMB(258) / 'UNUSED'/,  IVEC(258) /  0/,  IASCII(258) /0/    M37.7322
      DATA SYMB(259) / 'ICAT  '/,  IVEC(259) /  0/,  IASCII(259) /0/    M35.5395
      DATA SYMB(260) / 'NUMNVO'/,  IVEC(260) /  0/,  IASCII(260) /0/    M35.5396
      DATA SYMB(261) / 'UNUSED'/,  IVEC(261) /  0/,  IASCII(261) /0/    M37.7323
      DATA SYMB(262) / 'NENERG'/,  IVEC(262) /  0/,  IASCII(262) /0/    M35.5398
      DATA SYMB(263) / 'ISW   '/,  IVEC(263) /  0/,  IASCII(263) /0/    M35.5399
      DATA SYMB(264) / 'ITEST '/,  IVEC(264) /  0/,  IASCII(264) /0/    M35.5400
      DATA SYMB(265) / 'IDIST '/,  IVEC(265) /  0/,  IASCII(265) /0/    M35.5401
      DATA SYMB(266) / 'X     '/,  IVEC(266) /  1/,  IASCII(266) /0/    M35.5402
      DATA SYMB(267) / 'YKM   '/,  IVEC(267) /  1/,  IASCII(267) /0/    M35.5403
      DATA SYMB(268) / 'KM    '/,  IVEC(268) /  1/,  IASCII(268) /0/    M35.5404
      DATA SYMB(269) / 'XK    '/,  IVEC(269) /  1/,  IASCII(269) /0/    M35.5405
      DATA SYMB(270) / 'XM    '/,  IVEC(270) /  1/,  IASCII(270) /0/    M35.5406
      DATA SYMB(271) / 'WEIGHT'/,  IVEC(271) /  1/,  IASCII(271) /0/    M35.5407
      DATA SYMB(272) / 'IWTENT'/,  IVEC(272) /  1/,  IASCII(272) /0/    M35.5408
      DATA SYMB(273) / 'CON1  '/,  IVEC(273) /  1/,  IASCII(273) /0/    M35.5409
      DATA SYMB(274) / 'ISKIP '/,  IVEC(274) /  1/,  IASCII(274) /0/    M35.5410
      DATA SYMB(275) / 'ZINF  '/,  IVEC(275) /  1/,  IASCII(275) /0/    M35.5411
      DATA SYMB(276) / 'ETA   '/,  IVEC(276) /  1/,  IASCII(276) /0/    M35.5412
      DATA SYMB(277) / 'NHIST '/,  IVEC(277) /  1/,  IASCII(277) /0/    M35.5413
      DATA SYMB(278) / 'STAILM'/,  IVEC(278) /  1/,  IASCII(278) /0/    M35.5414
      DATA SYMB(279) / 'STAILK'/,  IVEC(279) /  1/,  IASCII(279) /0/    M35.5415
      DATA SYMB(280) / 'XMAX  '/,  IVEC(280) /  1/,  IASCII(280) /0/    M35.5416
      DATA SYMB(281) / 'KOUTVP'/,  IVEC(281) /  1/,  IASCII(281) /0/    M35.5417
      DATA SYMB(282) / 'BNRG  '/,  IVEC(282) /  1/,  IASCII(282) /0/    M35.5418
      DATA SYMB(283) / 'SCONST'/,  IVEC(283) /  1/,  IASCII(283) /0/    M35.5419
      DATA SYMB(284) / 'CNVHST'/,  IVEC(284) /  1/,  IASCII(284) /0/    M35.5420
      DATA SYMB(285) / 'SFD   '/,  IVEC(285) /  1/,  IASCII(285) /0/    M35.5421
      DATA SYMB(286) / 'QFD   '/,  IVEC(286) /  1/,  IASCII(286) /0/    M35.5422
      DATA SYMB(287) / 'SEMAUX'/,  IVEC(287) /  1/,  IASCII(287) /0/    M35.5423
      DATA SYMB(288) / 'IBSOUT'/,  IVEC(288) /  1/,  IASCII(288) /0/    M35.5424
      DATA SYMB(289) / 'BVALUE'/,  IVEC(289) /  1/,  IASCII(289) /0/    M35.5425
      DATA SYMB(290) / 'SPTACS'/,  IVEC(290) /  1/,  IASCII(290) /0/    M35.5426
      DATA SYMB(291) / 'KSWTYP'/,  IVEC(291) /  1/,  IASCII(291) /0/    M35.5427
      DATA SYMB(292) / 'MODSWT'/,  IVEC(292) /  1/,  IASCII(292) /0/    M35.5428
      DATA SYMB(293) / 'KBEGSW'/,  IVEC(293) /  1/,  IASCII(293) /0/    M35.5429
      DATA SYMB(294) / 'LASTSW'/,  IVEC(294) /  1/,  IASCII(294) /0/    M35.5430
      DATA SYMB(295) / 'KENTNB'/,  IVEC(295) /  1/,  IASCII(295) /0/    M35.5431
      DATA SYMB(296) / 'NBHDSW'/,  IVEC(296) /  1/,  IASCII(296) /0/    M35.5432
      DATA SYMB(297) / 'TOPEN '/,  IVEC(297) /  1/,  IASCII(297) /0/    M35.5433
      DATA SYMB(298) / 'CRIT  '/,  IVEC(298) /  1/,  IASCII(298) /0/    M35.5434
      DATA SYMB(299) / 'KDEPSW'/,  IVEC(299) /  1/,  IASCII(299) /0/    M35.5435
      DATA SYMB(300) / 'TDNS  '/,  IVEC(300) /  1/,  IASCII(300) /0/    M35.5436
      DATA SYMB(301) / 'ISOURC'/,  IVEC(301) /  1/,  IASCII(301) /0/    M35.5437
      DATA SYMB(302) / 'ENERGY'/,  IVEC(302) /  1/,  IASCII(302) /0/    M35.5438
      DATA SYMB(303) / 'IARDUB'/,  IVEC(303) /  1/,  IASCII(303) /0/    M35.5439
      DATA SYMB(304) / 'ARDUBE'/,  IVEC(304) /  1/,  IASCII(304) /0/    M35.5440
      DATA SYMB(305) / 'NONLAD'/,  IVEC(305) /  1/,  IASCII(305) /0/    M35.5441
      DATA SYMB(306) / 'NONLE '/,  IVEC(306) /  1/,  IASCII(306) /0/    M35.5442
      DATA SYMB(307) / 'VNONL '/,  IVEC(307) /  1/,  IASCII(307) /0/    M35.5443
      DATA SYMB(308) / 'CURR  '/,  IVEC(308) /  1/,  IASCII(308) /0/    M35.5444
      DATA SYMB(309) / 'ANONL '/,  IVEC(309) /  1/,  IASCII(309) /0/    M35.5445
      DATA SYMB(310) / 'VECNL1'/,  IVEC(310) /  1/,  IASCII(310) /0/    M35.5446
      DATA SYMB(311) / 'VECNL2'/,  IVEC(311) /  1/,  IASCII(311) /0/    M35.5447
      DATA SYMB(312) / 'BRNONL'/,  IVEC(312) /  1/,  IASCII(312) /1/    M35.5448
      DATA SYMB(313) / 'VZERO '/,  IVEC(313) /  1/,  IASCII(313) /0/    M35.5449
      DATA SYMB(314) / 'ILAST '/,  IVEC(314) /  1/,  IASCII(314) /0/    M35.5450
      DATA SYMB(315) / 'NLTYPE'/,  IVEC(315) /  1/,  IASCII(315) /0/    M35.5451
      DATA SYMB(316) / 'KUPL  '/,  IVEC(316) /  1/,  IASCII(316) /0/    M35.5452
      DATA SYMB(317) / 'NLSUB '/,  IVEC(317) /  1/,  IASCII(317) /0/    M35.5453
      DATA SYMB(318) / 'CURSUB'/,  IVEC(318) /  1/,  IASCII(318) /0/    M35.5454
      DATA SYMB(319) / 'CCHAR '/,  IVEC(319) /  1/,  IASCII(319) /0/    M35.5455
      DATA SYMB(320) / 'VCHAR '/,  IVEC(320) /  1/,  IASCII(320) /0/    M35.5456
      DATA SYMB(321) / 'GSLOPE'/,  IVEC(321) /  1/,  IASCII(321) /0/    M35.5457
      DATA SYMB(322) / 'KK    '/,  IVEC(322) /  1/,  IASCII(322) /0/    M35.5458
      DATA SYMB(323) / 'C     '/,  IVEC(323) /  1/,  IASCII(323) /0/    M35.5459
      DATA SYMB(324) / 'TR    '/,  IVEC(324) /  1/,  IASCII(324) /0/    M35.5460
      DATA SYMB(325) / 'TX    '/,  IVEC(325) /  1/,  IASCII(325) /0/    M35.5461
      DATA SYMB(326) / 'R     '/,  IVEC(326) /  1/,  IASCII(326) /0/    M35.5462
      DATA SYMB(327) / 'NR    '/,  IVEC(327) /  1/,  IASCII(327) /0/    M35.5463
      DATA SYMB(328) / 'LENGTH'/,  IVEC(328) /  1/,  IASCII(328) /0/    M35.5464
      DATA SYMB(329) / 'CIK   '/,  IVEC(329) /  1/,  IASCII(329) /0/    M35.5465
      DATA SYMB(330) / 'CI    '/,  IVEC(330) /  1/,  IASCII(330) /0/    M35.5466
      DATA SYMB(331) / 'CK    '/,  IVEC(331) /  1/,  IASCII(331) /0/    M35.5467
      DATA SYMB(332) / 'SWNAME'/,  IVEC(332) /  1/,  IASCII(332) /1/    M35.5468
      DATA SYMB(333) / 'IBRNCH'/,  IVEC(333) /  1/,  IASCII(333) /0/    M35.5469
      DATA SYMB(334) / 'JBRNCH'/,  IVEC(334) /  1/,  IASCII(334) /0/    M35.5470
      DATA SYMB(335) / 'TSTOP '/,  IVEC(335) /  1/,  IASCII(335) /0/    M35.5471
      DATA SYMB(336) / 'NONLK '/,  IVEC(336) /  1/,  IASCII(336) /0/    M35.5472
      DATA SYMB(337) / 'NONLM '/,  IVEC(337) /  1/,  IASCII(337) /0/    M35.5473
      DATA SYMB(338) / 'SPUM  '/,  IVEC(338) /  1/,  IASCII(338) /0/    M35.5474
      DATA SYMB(339) / 'KKS   '/,  IVEC(339) /  1/,  IASCII(339) /0/    M35.5475
      DATA SYMB(340) / 'KKNONL'/,  IVEC(340) /  1/,  IASCII(340) /0/    M35.5476
      DATA SYMB(341) / 'ZNONL '/,  IVEC(341) /  1/,  IASCII(341) /0/    M35.5477
      DATA SYMB(342) / 'ZNONLB'/,  IVEC(342) /  1/,  IASCII(342) /0/    M35.5478
      DATA SYMB(343) / 'ZNONLC'/,  IVEC(343) /  1/,  IASCII(343) /0/    M35.5479
      DATA SYMB(344) / 'FINIT '/,  IVEC(344) /  1/,  IASCII(344) /0/    M35.5480
      DATA SYMB(345) / 'KSUB  '/,  IVEC(345) /  1/,  IASCII(345) /0/    M35.5481
      DATA SYMB(346) / 'MSUB  '/,  IVEC(346) /  1/,  IASCII(346) /0/    M35.5482
      DATA SYMB(347) / 'ISUBEG'/,  IVEC(347) /  1/,  IASCII(347) /0/    M35.5483
      DATA SYMB(348) / 'LITYPE'/,  IVEC(348) /  1/,  IASCII(348) /0/    M35.5484
      DATA SYMB(349) / 'IMODEL'/,  IVEC(349) /  1/,  IASCII(349) /0/    M35.5485
      DATA SYMB(350) / 'KBUS  '/,  IVEC(350) /  1/,  IASCII(350) /0/    M35.5486
      DATA SYMB(351) / 'MBUS  '/,  IVEC(351) /  1/,  IASCII(351) /0/    M35.5487
      DATA SYMB(352) / 'KODEBR'/,  IVEC(352) /  1/,  IASCII(352) /0/    M35.5488
      DATA SYMB(353) / 'CKI   '/,  IVEC(353) /  1/,  IASCII(353) /0/    M35.5489
      DATA SYMB(354) / 'CKKJM '/,  IVEC(354) /  1/,  IASCII(354) /0/    M35.5490
      DATA SYMB(355) / 'INDHST'/,  IVEC(355) /  1/,  IASCII(355) /0/    M35.5491
      DATA SYMB(356) / 'KODSEM'/,  IVEC(356) /  1/,  IASCII(356) /0/    M35.5492
      DATA SYMB(357) / 'BRNAME'/,  IVEC(357) /  1/,  IASCII(357) /1/    M35.5493
      DATA SYMB(358) / 'IFORM '/,  IVEC(358) /  1/,  IASCII(358) /0/    M35.5494
      DATA SYMB(359) / 'NODE  '/,  IVEC(359) /  1/,  IASCII(359) /0/    M35.5495
      DATA SYMB(360) / 'CREST '/,  IVEC(360) /  1/,  IASCII(360) /0/    M35.5496
      DATA SYMB(361) / 'TIME1 '/,  IVEC(361) /  1/,  IASCII(361) /0/    M35.5497
      DATA SYMB(362) / 'TIME2 '/,  IVEC(362) /  1/,  IASCII(362) /0/    M35.5498
      DATA SYMB(363) / 'TSTART'/,  IVEC(363) /  1/,  IASCII(363) /0/    M35.5499
      DATA SYMB(364) / 'SFREQ '/,  IVEC(364) /  1/,  IASCII(364) /0/    M35.5500
      DATA SYMB(365) / 'KMSWIT'/,  IVEC(365) /  1/,  IASCII(365) /0/    M35.5501
      DATA SYMB(366) / 'NEXTSW'/,  IVEC(366) /  1/,  IASCII(366) /0/    M35.5502
      DATA SYMB(367) / 'RMFD  '/,  IVEC(367) /  1/,  IASCII(367) /0/    M35.5503
      DATA SYMB(368) / 'CIKFD '/,  IVEC(368) /  1/,  IASCII(368) /0/    M35.5504
      DATA SYMB(369) / 'IMFD  '/,  IVEC(369) /  1/,  IASCII(369) /0/    M35.5505
      DATA SYMB(370) / 'TCLOSE'/,  IVEC(370) /  1/,  IASCII(370) /0/    M35.5506
      DATA SYMB(371) / 'ADELAY'/,  IVEC(371) /  1/,  IASCII(371) /0/    M35.5507
      DATA SYMB(372) / 'KPOS  '/,  IVEC(372) /  1/,  IASCII(372) /0/    M35.5508
      DATA SYMB(373) / 'E     '/,  IVEC(373) /  1/,  IASCII(373) /0/    M35.5509
      DATA SYMB(374) / 'F     '/,  IVEC(374) /  1/,  IASCII(374) /0/    M35.5510
      DATA SYMB(375) / 'KSSFRQ'/,  IVEC(375) /  1/,  IASCII(375) /0/    M35.5511
      DATA SYMB(376) / 'KODE  '/,  IVEC(376) /  1/,  IASCII(376) /0/    M35.5512
      DATA SYMB(377) / 'KPSOUR'/,  IVEC(377) /  1/,  IASCII(377) /0/    M35.5513
      DATA SYMB(378) / 'VOLTI '/,  IVEC(378) /  1/,  IASCII(378) /0/    M35.5514
      DATA SYMB(379) / 'VOLTK '/,  IVEC(379) /  1/,  IASCII(379) /0/    M35.5515
      DATA SYMB(380) / 'VOLT  '/,  IVEC(380) /  1/,  IASCII(380) /0/    M35.5516
      DATA SYMB(381) / 'BUS   '/,  IVEC(381) /  1/,  IASCII(381) /1/    M35.5517
      DATA SYMB(382) / 'ELD   '/,  IVEC(382) /  1/,  IASCII(382) /0/    M35.5518
      DATA SYMB(383) / 'ELAF  '/,  IVEC(383) /  1/,  IASCII(383) /0/    M35.5519
      DATA SYMB(384) / 'ELF   '/,  IVEC(384) /  1/,  IASCII(384) /0/    M35.5520
      DATA SYMB(385) / 'ELAKD '/,  IVEC(385) /  1/,  IASCII(385) /0/    M35.5521
      DATA SYMB(386) / 'ELFKD '/,  IVEC(386) /  1/,  IASCII(386) /0/    M35.5522
      DATA SYMB(387) / 'ELKD  '/,  IVEC(387) /  1/,  IASCII(387) /0/    M35.5523
      DATA SYMB(388) / 'ELQ   '/,  IVEC(388) /  1/,  IASCII(388) /0/    M35.5524
      DATA SYMB(389) / 'ELAG  '/,  IVEC(389) /  1/,  IASCII(389) /0/    M35.5525
      DATA SYMB(390) / 'ELG   '/,  IVEC(390) /  1/,  IASCII(390) /0/    M35.5526
      DATA SYMB(391) / 'ELAKQ '/,  IVEC(391) /  1/,  IASCII(391) /0/    M35.5527
      DATA SYMB(392) / 'ELGKQ '/,  IVEC(392) /  1/,  IASCII(392) /0/    M35.5528
      DATA SYMB(393) / 'ELKQ  '/,  IVEC(393) /  1/,  IASCII(393) /0/    M35.5529
      DATA SYMB(394) / 'EL0   '/,  IVEC(394) /  1/,  IASCII(394) /0/    M35.5530
      DATA SYMB(395) / 'RA    '/,  IVEC(395) /  1/,  IASCII(395) /0/    M35.5531
      DATA SYMB(396) / 'RF    '/,  IVEC(396) /  1/,  IASCII(396) /0/    M35.5532
      DATA SYMB(397) / 'RKD   '/,  IVEC(397) /  1/,  IASCII(397) /0/    M35.5533
      DATA SYMB(398) / 'RG    '/,  IVEC(398) /  1/,  IASCII(398) /0/    M35.5534
      DATA SYMB(399) / 'RKQ   '/,  IVEC(399) /  1/,  IASCII(399) /0/    M35.5535
      DATA SYMB(400) / 'R0    '/,  IVEC(400) /  1/,  IASCII(400) /0/    M35.5536
      DATA SYMB(401) / 'AGLINE'/,  IVEC(401) /  1/,  IASCII(401) /0/    M35.5537
      DATA SYMB(402) / 'RAT1  '/,  IVEC(402) /  1/,  IASCII(402) /0/    M35.5538
      DATA SYMB(403) / 'SMOUTP'/,  IVEC(403) /  1/,  IASCII(403) /0/    M35.5539
      DATA SYMB(404) / 'SMOUTQ'/,  IVEC(404) /  1/,  IASCII(404) /0/    M35.5540
      DATA SYMB(405) / 'TEG   '/,  IVEC(405) /  1/,  IASCII(405) /0/    M35.5541
      DATA SYMB(406) / 'TEXC  '/,  IVEC(406) /  1/,  IASCII(406) /0/    M35.5542
      DATA SYMB(407) / 'CNP   '/,  IVEC(407) /  1/,  IASCII(407) /0/    M35.5543
      DATA SYMB(408) / 'A22   '/,  IVEC(408) /  1/,  IASCII(408) /0/    M35.5544
      DATA SYMB(409) / 'A12   '/,  IVEC(409) /  1/,  IASCII(409) /0/    M35.5545
      DATA SYMB(410) / 'A21   '/,  IVEC(410) /  1/,  IASCII(410) /0/    M35.5546
      DATA SYMB(411) / 'AC    '/,  IVEC(411) /  1/,  IASCII(411) /0/    M35.5547
      DATA SYMB(412) / 'AI    '/,  IVEC(412) /  1/,  IASCII(412) /0/    M35.5548
      DATA SYMB(413) / 'AT    '/,  IVEC(413) /  1/,  IASCII(413) /0/    M35.5549
      DATA SYMB(414) / 'AH    '/,  IVEC(414) /  1/,  IASCII(414) /0/    M35.5550
      DATA SYMB(415) / 'XAY   '/,  IVEC(415) /  1/,  IASCII(415) /0/    M35.5551
      DATA SYMB(416) / 'CU    '/,  IVEC(416) /  1/,  IASCII(416) /0/    M35.5552
      DATA SYMB(417) / 'CV    '/,  IVEC(417) /  1/,  IASCII(417) /0/    M35.5553
      DATA SYMB(418) / 'DSAT  '/,  IVEC(418) /  1/,  IASCII(418) /0/    M35.5554
      DATA SYMB(419) / 'QSAT  '/,  IVEC(419) /  1/,  IASCII(419) /0/    M35.5555
      DATA SYMB(420) / 'ACR   '/,  IVEC(420) /  1/,  IASCII(420) /0/    M35.5556
      DATA SYMB(421) / 'CE    '/,  IVEC(421) /  1/,  IASCII(421) /0/    M35.5557
      DATA SYMB(422) / 'DSR   '/,  IVEC(422) /  1/,  IASCII(422) /0/    M35.5558
      DATA SYMB(423) / 'DSD   '/,  IVEC(423) /  1/,  IASCII(423) /0/    M35.5559
      DATA SYMB(424) / 'HICO  '/,  IVEC(424) /  1/,  IASCII(424) /0/    M35.5560
      DATA SYMB(425) / 'DSM   '/,  IVEC(425) /  1/,  IASCII(425) /0/    M35.5561
      DATA SYMB(426) / 'HSP   '/,  IVEC(426) /  1/,  IASCII(426) /0/    M35.5562
      DATA SYMB(427) / 'POWER '/,  IVEC(427) /  1/,  IASCII(427) /0/    M35.5563
      DATA SYMB(428) / 'EXTRS '/,  IVEC(428) /  1/,  IASCII(428) /0/    M35.5564
      DATA SYMB(429) / 'HISTQ '/,  IVEC(429) /  1/,  IASCII(429) /0/    M35.5565
      DATA SYMB(430) / 'HISTR '/,  IVEC(430) /  1/,  IASCII(430) /0/    M35.5566
      DATA SYMB(431) / 'YFOR  '/,  IVEC(431) /  1/,  IASCII(431) /0/    M35.5567
      DATA SYMB(432) / 'ZSK   '/,  IVEC(432) /  1/,  IASCII(432) /0/    M35.5568
      DATA SYMB(433) / 'Y     '/,  IVEC(433) /  1/,  IASCII(433) /0/    M35.5569
      DATA SYMB(434) / 'TORK  '/,  IVEC(434) /  1/,  IASCII(434) /0/    M35.5570
      DATA SYMB(435) / 'TEMP  '/,  IVEC(435) /  1/,  IASCII(435) /0/    M35.5571
      DATA SYMB(436) / 'Z     '/,  IVEC(436) /  1/,  IASCII(436) /0/    M35.5572
      DATA SYMB(437) / 'X1    '/,  IVEC(437) /  1/,  IASCII(437) /0/    M35.5573
      DATA SYMB(438) / 'SQRT3 '/,  IVEC(438) /  0/,  IASCII(438) /0/    M35.5574
      DATA SYMB(439) / 'ASQRT3'/,  IVEC(439) /  0/,  IASCII(439) /0/    M35.5575
      DATA SYMB(440) / 'SQRT32'/,  IVEC(440) /  0/,  IASCII(440) /0/    M35.5576
      DATA SYMB(441) / 'THTW  '/,  IVEC(441) /  0/,  IASCII(441) /0/    M35.5577
      DATA SYMB(442) / 'ATHTW '/,  IVEC(442) /  0/,  IASCII(442) /0/    M35.5578
      DATA SYMB(443) / 'RADEG '/,  IVEC(443) /  0/,  IASCII(443) /0/    M35.5579
      DATA SYMB(444) / 'OMDT  '/,  IVEC(444) /  0/,  IASCII(444) /0/    M35.5580
      DATA SYMB(445) / 'FACTOM'/,  IVEC(445) /  0/,  IASCII(445) /0/    M35.5581
      DATA SYMB(446) / 'DAMRAT'/,  IVEC(446) /  0/,  IASCII(446) /0/    M35.5582
      DATA SYMB(447) / 'ISAT  '/,  IVEC(447) /  1/,  IASCII(447) /0/    M35.5583
      DATA SYMB(448) / 'ISED  '/,  IVEC(448) /  1/,  IASCII(448) /0/    M35.5584
      DATA SYMB(449) / 'ISEQ  '/,  IVEC(449) /  1/,  IASCII(449) /0/    M35.5585
      DATA SYMB(450) / 'IMDUAL'/,  IVEC(450) /  1/,  IASCII(450) /0/    M35.5586
      DATA SYMB(451) / 'ICONFG'/,  IVEC(451) /  1/,  IASCII(451) /0/    M35.5587
      DATA SYMB(452) / 'KMAC  '/,  IVEC(452) /  1/,  IASCII(452) /0/    M35.5588
      DATA SYMB(453) / 'KEXC  '/,  IVEC(453) /  1/,  IASCII(453) /0/    M35.5589
      DATA SYMB(454) / 'NUMAS '/,  IVEC(454) /  1/,  IASCII(454) /0/    M35.5590
      DATA SYMB(455) / 'NODMA '/,  IVEC(455) /  1/,  IASCII(455) /0/    M35.5591
      DATA SYMB(456) / 'NODMB '/,  IVEC(456) /  1/,  IASCII(456) /0/    M35.5592
      DATA SYMB(457) / 'NODMC '/,  IVEC(457) /  1/,  IASCII(457) /0/    M35.5593
      DATA SYMB(458) / 'JASMIT'/,  IVEC(458) /  1/,  IASCII(458) /0/    M35.5594
      DATA SYMB(459) / 'JSMTOR'/,  IVEC(459) /  1/,  IASCII(459) /0/    M35.5595
      DATA SYMB(460) / 'JEXCIT'/,  IVEC(460) /  1/,  IASCII(460) /0/    M35.5596
      DATA SYMB(461) / 'ISLOC '/,  IVEC(461) /  1/,  IASCII(461) /0/    M35.5597
      DATA SYMB(462) / 'NOUTSM'/,  IVEC(462) /  1/,  IASCII(462) /0/    M35.5598
      DATA SYMB(463) / 'ISMOUT'/,  IVEC(463) /  1/,  IASCII(463) /0/    M35.5599
      DATA SYMB(464) / 'MFIRST'/,  IVEC(464) /  0/,  IASCII(464) /0/    M35.5600
      DATA SYMB(465) / 'LIMASS'/,  IVEC(465) /  0/,  IASCII(465) /0/    M35.5601
      DATA SYMB(466) / 'NST   '/,  IVEC(466) /  0/,  IASCII(466) /0/    M35.5602
      DATA SYMB(467) / 'ITOLD '/,  IVEC(467) /  0/,  IASCII(467) /0/    M35.5603
      DATA SYMB(468) / 'IBROLD'/,  IVEC(468) /  0/,  IASCII(468) /0/    M35.5604
      DATA SYMB(469) / 'BUSUM '/,  IVEC(469) /  1/,  IASCII(469) /1/    M35.5605
      DATA SYMB(470) / 'PTHETA'/,  IVEC(470) /  1/,  IASCII(470) /0/    M35.5606
      DATA SYMB(471) / 'ZTHEVR'/,  IVEC(471) /  1/,  IASCII(471) /0/    M35.5607
      DATA SYMB(472) / 'VINP  '/,  IVEC(472) /  1/,  IASCII(472) /0/    M35.5608
      DATA SYMB(473) / 'ZTHEVS'/,  IVEC(473) /  1/,  IASCII(473) /0/    M35.5609
      DATA SYMB(474) / 'UMCUR '/,  IVEC(474) /  1/,  IASCII(474) /0/    M35.5610
      DATA SYMB(475) / 'CON   '/,  IVEC(475) /  1/,  IASCII(475) /0/    M35.5611
      DATA SYMB(476) / 'DUMVEC'/,  IVEC(476) /  1/,  IASCII(476) /0/    M35.5612
      DATA SYMB(477) / 'DUMMAT'/,  IVEC(477) /  1/,  IASCII(477) /0/    M35.5613
      DATA SYMB(478) / 'DATE  '/,  IVEC(478) /  1/,  IASCII(478) /0/    M35.5614
      DATA SYMB(479) / 'CLOCK '/,  IVEC(479) /  1/,  IASCII(479) /0/    M35.5615
      DATA SYMB(480) / 'PI    '/,  IVEC(480) /  0/,  IASCII(480) /0/    M35.5616
      DATA SYMB(481) / 'SROOT2'/,  IVEC(481) /  0/,  IASCII(481) /0/    M35.5617
      DATA SYMB(482) / 'SROOT3'/,  IVEC(482) /  0/,  IASCII(482) /0/    M35.5618
      DATA SYMB(483) / 'OMEGRF'/,  IVEC(483) /  0/,  IASCII(483) /0/    M35.5619
      DATA SYMB(484) / 'INPU  '/,  IVEC(484) /  0/,  IASCII(484) /0/    M35.5620
      DATA SYMB(485) / 'NUMBUS'/,  IVEC(485) /  0/,  IASCII(485) /0/    M35.5621
      DATA SYMB(486) / 'NCLTOT'/,  IVEC(486) /  0/,  IASCII(486) /0/    M35.5622
      DATA SYMB(487) / 'NDUM  '/,  IVEC(487) /  1/,  IASCII(487) /0/    M35.5623
      DATA SYMB(488) / 'INITUM'/,  IVEC(488) /  0/,  IASCII(488) /0/    M35.5624
      DATA SYMB(489) / 'IUREAC'/,  IVEC(489) /  0/,  IASCII(489) /0/    M35.5625
      DATA SYMB(490) / 'IUGPAR'/,  IVEC(490) /  0/,  IASCII(490) /0/    M35.5626
      DATA SYMB(491) / 'IUFPAR'/,  IVEC(491) /  0/,  IASCII(491) /0/    M35.5627
      DATA SYMB(492) / 'IUHIST'/,  IVEC(492) /  0/,  IASCII(492) /0/    M35.5628
      DATA SYMB(493) / 'IUUMRP'/,  IVEC(493) /  0/,  IASCII(493) /0/    M35.5629
      DATA SYMB(494) / 'IUNOD1'/,  IVEC(494) /  0/,  IASCII(494) /0/    M35.5630
      DATA SYMB(495) / 'IUNOD2'/,  IVEC(495) /  0/,  IASCII(495) /0/    M35.5631
      DATA SYMB(496) / 'IUJCLT'/,  IVEC(496) /  0/,  IASCII(496) /0/    M35.5632
      DATA SYMB(497) / 'IUJCLO'/,  IVEC(497) /  0/,  IASCII(497) /0/    M35.5633
      DATA SYMB(498) / 'IUJTYP'/,  IVEC(498) /  0/,  IASCII(498) /0/    M35.5634
      DATA SYMB(499) / 'IUNODO'/,  IVEC(499) /  0/,  IASCII(499) /0/    M35.5635
      DATA SYMB(500) / 'IUJTMT'/,  IVEC(500) /  0/,  IASCII(500) /0/    M35.5636
      DATA SYMB(501) / 'IUHISM'/,  IVEC(501) /  0/,  IASCII(501) /0/    M35.5637
      DATA SYMB(502) / 'IUOMGM'/,  IVEC(502) /  0/,  IASCII(502) /0/    M35.5638
      DATA SYMB(503) / 'IUOMLD'/,  IVEC(503) /  0/,  IASCII(503) /0/    M35.5639
      DATA SYMB(504) / 'IUTHAM'/,  IVEC(504) /  0/,  IASCII(504) /0/    M35.5640
      DATA SYMB(505) / 'IUREDU'/,  IVEC(505) /  0/,  IASCII(505) /0/    M35.5641
      DATA SYMB(506) / 'IUREDS'/,  IVEC(506) /  0/,  IASCII(506) /0/    M35.5642
      DATA SYMB(507) / 'IUFLDS'/,  IVEC(507) /  0/,  IASCII(507) /0/    M35.5643
      DATA SYMB(508) / 'IUFLDR'/,  IVEC(508) /  0/,  IASCII(508) /0/    M35.5644
      DATA SYMB(509) / 'IUREQU'/,  IVEC(509) /  0/,  IASCII(509) /0/    M35.5645
      DATA SYMB(510) / 'IUFLQS'/,  IVEC(510) /  0/,  IASCII(510) /0/    M35.5646
      DATA SYMB(511) / 'IUFLQR'/,  IVEC(511) /  0/,  IASCII(511) /0/    M35.5647
      DATA SYMB(512) / 'IUJCDS'/,  IVEC(512) /  0/,  IASCII(512) /0/    M35.5648
      DATA SYMB(513) / 'IUJCQS'/,  IVEC(513) /  0/,  IASCII(513) /0/    M35.5649
      DATA SYMB(514) / 'IUFLXD'/,  IVEC(514) /  0/,  IASCII(514) /0/    M35.5650
      DATA SYMB(515) / 'IUFLXQ'/,  IVEC(515) /  0/,  IASCII(515) /0/    M35.5651
      DATA SYMB(516) / 'IUNPPA'/,  IVEC(516) /  0/,  IASCII(516) /0/    M35.5652
      DATA SYMB(517) / 'IUROTM'/,  IVEC(517) /  0/,  IASCII(517) /0/    M35.5653
      DATA SYMB(518) / 'IUNCLD'/,  IVEC(518) /  0/,  IASCII(518) /0/    M35.5654
      DATA SYMB(519) / 'IUNCLQ'/,  IVEC(519) /  0/,  IASCII(519) /0/    M35.5655
      DATA SYMB(520) / 'IUJTQO'/,  IVEC(520) /  0/,  IASCII(520) /0/    M35.5656
      DATA SYMB(521) / 'IUJOMO'/,  IVEC(521) /  0/,  IASCII(521) /0/    M35.5657
      DATA SYMB(522) / 'IUJTHO'/,  IVEC(522) /  0/,  IASCII(522) /0/    M35.5658
      DATA SYMB(523) / 'IUREQS'/,  IVEC(523) /  0/,  IASCII(523) /0/    M35.5659
      DATA SYMB(524) / 'IUEPSO'/,  IVEC(524) /  0/,  IASCII(524) /0/    M35.5660
      DATA SYMB(525) / 'IUDCOE'/,  IVEC(525) /  0/,  IASCII(525) /0/    M35.5661
      DATA SYMB(526) / 'IUKCOI'/,  IVEC(526) /  0/,  IASCII(526) /0/    M35.5662
      DATA SYMB(527) / 'IUVOLT'/,  IVEC(527) /  0/,  IASCII(527) /0/    M35.5663
      DATA SYMB(528) / 'IUANGL'/,  IVEC(528) /  0/,  IASCII(528) /0/    M35.5664
      DATA SYMB(529) / 'IUNODF'/,  IVEC(529) /  0/,  IASCII(529) /0/    M35.5665
      DATA SYMB(530) / 'IUNODM'/,  IVEC(530) /  0/,  IASCII(530) /0/    M35.5666
      DATA SYMB(531) / 'IUKUMO'/,  IVEC(531) /  0/,  IASCII(531) /0/    M35.5667
      DATA SYMB(532) / 'IUJUMO'/,  IVEC(532) /  0/,  IASCII(532) /0/    M35.5668
      DATA SYMB(533) / 'IUUMOU'/,  IVEC(533) /  0/,  IASCII(533) /0/    M35.5669
      DATA SYMB(534) / 'NCLFIX'/,  IVEC(534) /  0/,  IASCII(534) /0/    M35.5670
      DATA SYMB(535) / 'NUMFIX'/,  IVEC(535) /  0/,  IASCII(535) /0/    M35.5671
      DATA SYMB(536) / 'IOTFIX'/,  IVEC(536) /  0/,  IASCII(536) /0/    M35.5672
      DATA SYMB(537) / 'IBSFIX'/,  IVEC(537) /  0/,  IASCII(537) /0/    M35.5673
      DATA SYMB(538) / 'KSUBUM'/,  IVEC(538) /  0/,  IASCII(538) /0/    M35.5674
      DATA SYMB(539) / 'NSMACH'/,  IVEC(539) /  0/,  IASCII(539) /0/    M35.5675
      DATA SYMB(540) / 'ISTART'/,  IVEC(540) /  0/,  IASCII(540) /0/    M35.5676
      DATA SYMB(541) / 'KARRAY'/,  IVEC(541) /  1/,  IASCII(541) /0/    M35.5677
      DATA SYMB(542) / 'RAMPCN'/,  IVEC(542) /  1/,  IASCII(542) /0/    M35.5678
      DATA SYMB(543) / 'RAMPSL'/,  IVEC(543) /  1/,  IASCII(543) /0/    M35.5679
      DATA SYMB(544) / 'KYRAMP'/,  IVEC(544) /  1/,  IASCII(544) /0/    M35.5680
      DATA SYMB(545) / 'TEXPAR'/,  IVEC(545) /  1/,  IASCII(545) /0/    M35.5681
      DATA SYMB(546) / 'FENDRP'/,  IVEC(546) /  1/,  IASCII(546) /0/    M35.5682
      DATA SYMB(547) / 'TMINRP'/,  IVEC(547) /  0/,  IASCII(547) /0/    M35.5683
      DATA SYMB(548) / 'TMAXRP'/,  IVEC(548) /  0/,  IASCII(548) /0/    M35.5684
      DATA SYMB(549) / 'TBEGRP'/,  IVEC(549) /  1/,  IASCII(549) /0/    M35.5685
      DATA SYMB(550) / 'TENDRP'/,  IVEC(550) /  1/,  IASCII(550) /0/    M35.5686
      DATA SYMB(551) / 'FBEGRP'/,  IVEC(551) /  1/,  IASCII(551) /0/    M35.5687
      DATA SYMB(552) / 'TBREAK'/,  IVEC(552) /  0/,  IASCII(552) /0/    M35.5688
      DATA SYMB(553) / 'INDXRP'/,  IVEC(553) /  1/,  IASCII(553) /0/    M35.5689
      DATA SYMB(554) / 'IVEC  '/,  IVEC(554) /  1/,  IASCII(554) /0/    M35.5690
      DATA SYMB(555) / 'IASCII'/,  IVEC(555) /  1/,  IASCII(555) /0/    M35.5691
      DATA SYMB(556) / 'NUMSYM'/,  IVEC(556) /  0/,  IASCII(556) /0/    M35.5692
      DATA SYMB(557) / 'JJROLL'/,  IVEC(557) /  0/,  IASCII(557) /0/    M35.5693
      DATA SYMB(558) / 'ITEXP '/,  IVEC(558) /  0/,  IASCII(558) /0/    M35.5694
      DATA SYMB(559) / 'LABELS'/,  IVEC(559) /  1/,  IASCII(559) /0/    M35.5695
      DATA SYMB(560) / 'MAXARG'/,  IVEC(560) /  0/,  IASCII(560) /0/    M35.5696
      DATA SYMB(561) / 'KILPER'/,  IVEC(561) /  0/,  IASCII(561) /0/    M35.5697
      DATA SYMB(562) / 'KFILE5'/,  IVEC(562) /  0/,  IASCII(562) /0/    M35.5698
      DATA SYMB(563) / 'KVERFY'/,  IVEC(563) /  0/,  IASCII(563) /0/    M35.5699
      DATA SYMB(564) / 'IBEGCL'/,  IVEC(564) /  0/,  IASCII(564) /0/    M35.5700
      DATA SYMB(565) / 'IENDCL'/,  IVEC(565) /  0/,  IASCII(565) /0/    M35.5701
      DATA SYMB(566) / 'LIDNT1'/,  IVEC(566) /  0/,  IASCII(566) /0/    M35.5702
      DATA SYMB(567) / 'LIDNT2'/,  IVEC(567) /  0/,  IASCII(567) /0/    M35.5703
      DATA SYMB(568) / 'LINNOW'/,  IVEC(568) /  0/,  IASCII(568) /0/    M35.5704
      DATA SYMB(569) / 'LINSPN'/,  IVEC(569) /  0/,  IASCII(569) /0/    M35.5705
      DATA SYMB(570) / 'NUMCRD'/,  IVEC(570) /  0/,  IASCII(570) /0/    M35.5706
      DATA SYMB(571) / 'MUNIT5'/,  IVEC(571) /  0/,  IASCII(571) /0/    M35.5707
      DATA SYMB(572) / 'INDBUF'/,  IVEC(572) /  0/,  IASCII(572) /0/    M35.5708
      DATA SYMB(573) / 'INDBEG'/,  IVEC(573) /  0/,  IASCII(573) /0/    M35.5709
      DATA SYMB(574) / 'MFLUSH'/,  IVEC(574) /  0/,  IASCII(574) /0/    M35.5710
      DATA SYMB(575) / 'NEWVEC'/,  IVEC(575) /  0/,  IASCII(575) /0/    M35.5711
      DATA SYMB(576) / 'MUNIT6'/,  IVEC(576) /  0/,  IASCII(576) /0/    M35.5712
      DATA SYMB(577) / 'LSERLC'/,  IVEC(577) /  0/,  IASCII(577) /0/    M35.5713
      DATA SYMB(578) / 'KSERLC'/,  IVEC(578) /  0/,  IASCII(578) /0/    M35.5714
      DATA SYMB(579) / 'KBRSER'/,  IVEC(579) /  0/,  IASCII(579) /0/    M35.5715
      DATA SYMB(580) / 'LOCKBR'/,  IVEC(580) /  0/,  IASCII(580) /0/    M35.5716
      DATA SYMB(581) / 'IPRSPY'/,  IVEC(581) /  0/,  IASCII(581) /0/    M35.5717
      DATA SYMB(582) / 'MONITR'/,  IVEC(582) /  0/,  IASCII(582) /0/    M35.5718
      DATA SYMB(583) / 'MONITS'/,  IVEC(583) /  0/,  IASCII(583) /0/    M35.5719
      DATA SYMB(584) / 'LOCATE'/,  IVEC(584) /  1/,  IASCII(584) /0/    M35.5720
      DATA SYMB(585) / 'NLINE '/,  IVEC(585) /  1/,  IASCII(585) /0/    M35.5721
      DATA SYMB(586) / 'KWTSPY'/,  IVEC(586) /  0/,  IASCII(586) /0/    M35.5722
      DATA SYMB(587) / 'KBREAK'/,  IVEC(587) /  0/,  IASCII(587) /0/    M35.5723
      DATA SYMB(588) / 'LIMBUF'/,  IVEC(588) /  0/,  IASCII(588) /0/    M35.5724
      DATA SYMB(589) / 'INCHLP'/,  IVEC(589) /  0/,  IASCII(589) /0/    M35.5725
      DATA SYMB(590) / 'KSYMBL'/,  IVEC(590) /  0/,  IASCII(590) /0/    M35.5726
      DATA SYMB(591) / 'KOPYIT'/,  IVEC(591) /  0/,  IASCII(591) /0/    M35.5727
      DATA SYMB(592) / 'KSLOWR'/,  IVEC(592) /  0/,  IASCII(592) /0/    M35.5728
      DATA SYMB(593) / 'LIMCRD'/,  IVEC(593) /  0/,  IASCII(593) /0/    M35.5729
      DATA SYMB(594) / 'LOOPRP'/,  IVEC(594) /  1/,  IASCII(594) /0/    M35.5730
      DATA SYMB(595) / 'N10RMP'/,  IVEC(595) /  1/,  IASCII(595) /0/    M35.5731
      DATA SYMB(596) / 'MEMRMP'/,  IVEC(596) /  1/,  IASCII(596) /0/    M35.5732
      DATA SYMB(597) / 'KAR1  '/,  IVEC(597) /  1/,  IASCII(597) /0/    M35.5733
      DATA SYMB(598) / 'KAR2  '/,  IVEC(598) /  1/,  IASCII(598) /0/    M35.5734
      DATA SYMB(599) / 'NUMRMP'/,  IVEC(599) /  0/,  IASCII(599) /0/    M35.5735
      DATA SYMB(600) / 'LUNTSP'/,  IVEC(600) /  0/,  IASCII(600) /0/    M35.5736
      DATA SYMB(601) / 'LOGVAR'/,  IVEC(601) /  0/,  IASCII(601) /0/    M35.5737
      DATA SYMB(602) / 'FILEXT'/,  IVEC(602) /  1/,  IASCII(602) /0/    M35.5738
      DATA SYMB(603) / 'SYMB  '/,  IVEC(603) /  1/,  IASCII(603) /0/    M35.5739
      DATA SYMB(604) / 'COL   '/,  IVEC(604) /  1/,  IASCII(604) /0/    M35.5740
      DATA SYMB(605) / 'BYTFND'/,  IVEC(605) /  0/,  IASCII(605) /0/    M35.5741
      DATA SYMB(606) / 'CHAR1 '/,  IVEC(606) /  0/,  IASCII(606) /0/    M35.5742
      DATA SYMB(607) / 'SYMBRP'/,  IVEC(607) /  1/,  IASCII(607) /0/    M35.5743
      DATA SYMB(608) / 'CHARD4'/,  IVEC(608) /  0/,  IASCII(608) /0/    M35.5744
      DATA SYMB(609) / 'BYTBUF'/,  IVEC(609) /  0/,  IASCII(609) /0/    M35.5745
      DATA SYMB(610) / 'BUFF77'/,  IVEC(610) /  0/,  IASCII(610) /0/    M35.5746
      DATA SYMB(611) / 'FILE6B'/,  IVEC(611) /  1/,  IASCII(611) /0/    M35.5747
      DATA SYMB(612) / 'FILE6 '/,  IVEC(612) /  1/,  IASCII(612) /0/    M35.5748
      DATA SYMB(613) / 'BLAN80'/,  IVEC(613) /  0/,  IASCII(613) /0/    M35.5749
      DATA SYMB(614) / 'PROM80'/,  IVEC(614) /  0/,  IASCII(614) /0/    M35.5750
      DATA SYMB(615) / 'DIGIT '/,  IVEC(615) /  1/,  IASCII(615) /0/    M35.5751
      DATA SYMB(616) / 'IAC   '/,  IVEC(616) /  0/,  IASCII(616) /0/    M35.5752
      DATA SYMB(617) / 'IDCTCS'/,  IVEC(617) /  0/,  IASCII(617) /0/    M35.5753
      DATA SYMB(618) / 'IPL   '/,  IVEC(618) /  0/,  IASCII(618) /0/    M35.5754
      DATA SYMB(619) / 'IPR   '/,  IVEC(619) /  0/,  IASCII(619) /0/    M35.5755
      DATA SYMB(620) / 'IXR   '/,  IVEC(620) /  0/,  IASCII(620) /0/    M35.5756
      DATA SYMB(621) / 'JPL   '/,  IVEC(621) /  0/,  IASCII(621) /0/    M35.5757
      DATA SYMB(622) / 'JPR   '/,  IVEC(622) /  0/,  IASCII(622) /0/    M35.5758
      DATA SYMB(623) / 'KINT  '/,  IVEC(623) /  0/,  IASCII(623) /0/    M35.5759
      DATA SYMB(624) / 'KOUT  '/,  IVEC(624) /  0/,  IASCII(624) /0/    M35.5760
      DATA SYMB(625) / 'NDS   '/,  IVEC(625) /  0/,  IASCII(625) /0/    M35.5761
      DATA SYMB(626) / 'NKN   '/,  IVEC(626) /  0/,  IASCII(626) /0/    M35.5762
      DATA SYMB(627) / 'NMAX  '/,  IVEC(627) /  0/,  IASCII(627) /0/    M35.5763
      DATA SYMB(628) / 'NUK   '/,  IVEC(628) /  0/,  IASCII(628) /0/    M35.5764
      DATA SYMB(629) / 'KWRITE'/,  IVEC(629) /  0/,  IASCII(629) /0/    M35.5765
      DATA SYMB(630) / 'KPR   '/,  IVEC(630) /  0/,  IASCII(630) /0/    M35.5766
      DATA SYMB(631) / 'KPL   '/,  IVEC(631) /  0/,  IASCII(631) /0/    M35.5767
      DATA SYMB(632) / 'MXTACW'/,  IVEC(632) /  0/,  IASCII(632) /0/    M35.5768
      DATA SYMB(633) / 'IPTACW'/,  IVEC(633) /  0/,  IASCII(633) /0/    M35.5769
      DATA SYMB(634) / 'NHST  '/,  IVEC(634) /  0/,  IASCII(634) /0/    M35.5770
      DATA SYMB(635) / 'KVIN  '/,  IVEC(635) /  0/,  IASCII(635) /0/    M35.5771
      DATA SYMB(636) / 'KVOU  '/,  IVEC(636) /  0/,  IASCII(636) /0/    M35.5772
      DATA SYMB(637) / 'KVXX  '/,  IVEC(637) /  0/,  IASCII(637) /0/    M35.5773
      DATA SYMB(638) / 'ICSUP '/,  IVEC(638) /  0/,  IASCII(638) /0/    M35.5774
      DATA SYMB(639) / 'NXIC  '/,  IVEC(639) /  0/,  IASCII(639) /0/    M35.5775
      DATA SYMB(640) / 'KKSJ  '/,  IVEC(640) /  0/,  IASCII(640) /0/    M35.5776
      DATA SYMB(641) / 'KKSK  '/,  IVEC(641) /  0/,  IASCII(641) /0/    M35.5777
      DATA SYMB(642) / 'KKFST '/,  IVEC(642) /  0/,  IASCII(642) /0/    M35.5778
      DATA SYMB(643) / 'KKNI  '/,  IVEC(643) /  0/,  IASCII(643) /0/    M35.5779
      DATA SYMB(644) / 'KKHST '/,  IVEC(644) /  0/,  IASCII(644) /0/    M35.5780
      DATA SYMB(645) / 'KIFLS '/,  IVEC(645) /  0/,  IASCII(645) /0/    M35.5781
      DATA SYMB(646) / 'KIDUM '/,  IVEC(646) /  0/,  IASCII(646) /0/    M35.5782
      DATA SYMB(647) / 'KSLIM1'/,  IVEC(647) /  0/,  IASCII(647) /0/    M35.5783
      DATA SYMB(648) / 'KSLIM2'/,  IVEC(648) /  0/,  IASCII(648) /0/    M35.5784
      DATA SYMB(649) / 'KSLIM3'/,  IVEC(649) /  0/,  IASCII(649) /0/    M35.5785
      DATA SYMB(650) / 'KPAC1R'/,  IVEC(650) /  0/,  IASCII(650) /0/    M35.5786
      DATA SYMB(651) / 'KPAC1I'/,  IVEC(651) /  0/,  IASCII(651) /0/    M35.5787
      DATA SYMB(652) / 'KPAC2R'/,  IVEC(652) /  0/,  IASCII(652) /0/    M35.5788
      DATA SYMB(653) / 'KPAC2I'/,  IVEC(653) /  0/,  IASCII(653) /0/    M35.5789
      DATA SYMB(654) / 'KALKSX'/,  IVEC(654) /  0/,  IASCII(654) /0/    M35.5790
      DATA SYMB(655) / 'KILMS1'/,  IVEC(655) /  0/,  IASCII(655) /0/    M35.5791
      DATA SYMB(656) / 'KILMS2'/,  IVEC(656) /  0/,  IASCII(656) /0/    M35.5792
      DATA SYMB(657) / 'KDUMJ '/,  IVEC(657) /  0/,  IASCII(657) /0/    M35.5793
      DATA SYMB(658) / 'KDUMK '/,  IVEC(658) /  0/,  IASCII(658) /0/    M35.5794
      DATA SYMB(659) / 'KKZJ  '/,  IVEC(659) /  0/,  IASCII(659) /0/    M35.5795
      DATA SYMB(660) / 'KKZK  '/,  IVEC(660) /  0/,  IASCII(660) /0/    M35.5796
      DATA SYMB(661) / 'KIFLZ '/,  IVEC(661) /  0/,  IASCII(661) /0/    M35.5797
      DATA SYMB(662) / 'KGNZ  '/,  IVEC(662) /  0/,  IASCII(662) /0/    M35.5798
      DATA SYMB(663) / 'KZLIM1'/,  IVEC(663) /  0/,  IASCII(663) /0/    M35.5799
      DATA SYMB(664) / 'KZLIM2'/,  IVEC(664) /  0/,  IASCII(664) /0/    M35.5800
      DATA SYMB(665) / 'KALKZX'/,  IVEC(665) /  0/,  IASCII(665) /0/    M35.5801
      DATA SYMB(666) / 'KILMZ1'/,  IVEC(666) /  0/,  IASCII(666) /0/    M35.5802
      DATA SYMB(667) / 'KILMZ2'/,  IVEC(667) /  0/,  IASCII(667) /0/    M35.5803
      DATA SYMB(668) / 'KKSUS '/,  IVEC(668) /  0/,  IASCII(668) /0/    M35.5804
      DATA SYMB(669) / 'KALKSU'/,  IVEC(669) /  0/,  IASCII(669) /0/    M35.5805
      DATA SYMB(670) / 'KIUTY '/,  IVEC(670) /  0/,  IASCII(670) /0/    M35.5806
      DATA SYMB(671) / 'KUD1  '/,  IVEC(671) /  0/,  IASCII(671) /0/    M35.5807
      DATA SYMB(672) / 'KUD2  '/,  IVEC(672) /  0/,  IASCII(672) /0/    M35.5808
      DATA SYMB(673) / 'KUD3  '/,  IVEC(673) /  0/,  IASCII(673) /0/    M35.5809
      DATA SYMB(674) / 'KUD4  '/,  IVEC(674) /  0/,  IASCII(674) /0/    M35.5810
      DATA SYMB(675) / 'KUD5  '/,  IVEC(675) /  0/,  IASCII(675) /0/    M35.5811
      DATA SYMB(676) / 'KALIU '/,  IVEC(676) /  0/,  IASCII(676) /0/    M35.5812
      DATA SYMB(677) / 'KTYSUP'/,  IVEC(677) /  0/,  IASCII(677) /0/    M35.5813
      DATA SYMB(678) / 'KJSUP '/,  IVEC(678) /  0/,  IASCII(678) /0/    M35.5814
      DATA SYMB(679) / 'KKSUP '/,  IVEC(679) /  0/,  IASCII(679) /0/    M35.5815
      DATA SYMB(680) / 'KSPVAR'/,  IVEC(680) /  0/,  IASCII(680) /0/    M35.5816
      DATA SYMB(681) / 'KOPSUP'/,  IVEC(681) /  0/,  IASCII(681) /0/    M35.5817
      DATA SYMB(682) / 'KFNSUP'/,  IVEC(682) /  0/,  IASCII(682) /0/    M35.5818
      DATA SYMB(683) / 'KRGSUP'/,  IVEC(683) /  0/,  IASCII(683) /0/    M35.5819
      DATA SYMB(684) / 'KPRSUP'/,  IVEC(684) /  0/,  IASCII(684) /0/    M35.5820
      DATA SYMB(685) / 'KTYPDV'/,  IVEC(685) /  0/,  IASCII(685) /0/    M35.5821
      DATA SYMB(686) / 'KKDJ  '/,  IVEC(686) /  0/,  IASCII(686) /0/    M35.5822
      DATA SYMB(687) / 'KKDK  '/,  IVEC(687) /  0/,  IASCII(687) /0/    M35.5823
      DATA SYMB(688) / 'KGNDEV'/,  IVEC(688) /  0/,  IASCII(688) /0/    M35.5824
      DATA SYMB(689) / 'KDEV1 '/,  IVEC(689) /  0/,  IASCII(689) /0/    M35.5825
      DATA SYMB(690) / 'KDEV2 '/,  IVEC(690) /  0/,  IASCII(690) /0/    M35.5826
      DATA SYMB(691) / 'KLDEV1'/,  IVEC(691) /  0/,  IASCII(691) /0/    M35.5827
      DATA SYMB(692) / 'KLDEV2'/,  IVEC(692) /  0/,  IASCII(692) /0/    M35.5828
      DATA SYMB(693) / 'KKDUS '/,  IVEC(693) /  0/,  IASCII(693) /0/    M35.5829
      DATA SYMB(694) / 'KALKDU'/,  IVEC(694) /  0/,  IASCII(694) /0/    M35.5830
      DATA SYMB(695) / 'KTBDEV'/,  IVEC(695) /  0/,  IASCII(695) /0/    M35.5831
      DATA SYMB(696) / 'KPN   '/,  IVEC(696) /  0/,  IASCII(696) /0/    M35.5832
      DATA SYMB(697) / 'KPD   '/,  IVEC(697) /  0/,  IASCII(697) /0/    M35.5833
      DATA SYMB(698) / 'KXHST '/,  IVEC(698) /  0/,  IASCII(698) /0/    M35.5834
      DATA SYMB(699) / 'KHSCR '/,  IVEC(699) /  0/,  IASCII(699) /0/    M35.5835
      DATA SYMB(700) / 'KHSCI '/,  IVEC(700) /  0/,  IASCII(700) /0/    M35.5836
      DATA SYMB(701) / 'KILIM1'/,  IVEC(701) /  0/,  IASCII(701) /0/    M35.5837
      DATA SYMB(702) / 'KILIM2'/,  IVEC(702) /  0/,  IASCII(702) /0/    M35.5838
      DATA SYMB(703) / 'KROWCS'/,  IVEC(703) /  0/,  IASCII(703) /0/    M35.5839
      DATA SYMB(704) / 'KRHSDE'/,  IVEC(704) /  0/,  IASCII(704) /0/    M35.5840
      DATA SYMB(705) / 'KVLIM1'/,  IVEC(705) /  0/,  IASCII(705) /0/    M35.5841
      DATA SYMB(706) / 'KVLIM2'/,  IVEC(706) /  0/,  IASCII(706) /0/    M35.5842
      DATA SYMB(707) / 'KKXIC '/,  IVEC(707) /  0/,  IASCII(707) /0/    M35.5843
      DATA SYMB(708) / 'KAWKCS'/,  IVEC(708) /  0/,  IASCII(708) /0/    M35.5844
      DATA SYMB(709) / 'KXAR  '/,  IVEC(709) /  0/,  IASCII(709) /0/    M35.5845
      DATA SYMB(710) / 'KXAI  '/,  IVEC(710) /  0/,  IASCII(710) /0/    M35.5846
      DATA SYMB(711) / 'KBWKCS'/,  IVEC(711) /  0/,  IASCII(711) /0/    M35.5847
      DATA SYMB(712) / 'KXTCS '/,  IVEC(712) /  0/,  IASCII(712) /0/    M35.5848
      DATA SYMB(713) / 'KLNTAB'/,  IVEC(713) /  0/,  IASCII(713) /0/    M35.5849
      DATA SYMB(714) / 'KLMXIC'/,  IVEC(714) /  0/,  IASCII(714) /0/    M35.5850
      DATA SYMB(715) / 'KCOLCS'/,  IVEC(715) /  0/,  IASCII(715) /0/    M35.5851
      DATA SYMB(716) / 'KATCS '/,  IVEC(716) /  0/,  IASCII(716) /0/    M35.5852
      DATA SYMB(717) / 'KBTCS '/,  IVEC(717) /  0/,  IASCII(717) /0/    M35.5853
      DATA SYMB(718) / 'KJOUT '/,  IVEC(718) /  0/,  IASCII(718) /0/    M35.5854
      DATA SYMB(719) / 'KKOUT '/,  IVEC(719) /  0/,  IASCII(719) /0/    M35.5855
      DATA SYMB(720) / 'KXMNCS'/,  IVEC(720) /  0/,  IASCII(720) /0/    M35.5856
      DATA SYMB(721) / 'KTXMN '/,  IVEC(721) /  0/,  IASCII(721) /0/    M35.5857
      DATA SYMB(722) / 'KXMXCS'/,  IVEC(722) /  0/,  IASCII(722) /0/    M35.5858
      DATA SYMB(723) / 'KTXMX '/,  IVEC(723) /  0/,  IASCII(723) /0/    M35.5859
      DATA SYMB(724) / 'KLNOUT'/,  IVEC(724) /  0/,  IASCII(724) /0/    M35.5860
      DATA SYMB(725) / 'EKBUF '/,  IVEC(725) /  1/,  IASCII(725) /0/    M35.5861
      DATA SYMB(726) / 'EKTEMP'/,  IVEC(726) /  1/,  IASCII(726) /0/    M35.5862
      DATA SYMB(727) / 'ERRCHK'/,  IVEC(727) /  0/,  IASCII(727) /0/    M35.5863
      DATA SYMB(728) / 'SOLRSV'/,  IVEC(728) /  1/,  IASCII(728) /0/    M35.5864
      DATA SYMB(729) / 'SOLISV'/,  IVEC(729) /  1/,  IASCII(729) /0/    M35.5865
      DATA SYMB(730) / 'NITERA'/,  IVEC(730) /  0/,  IASCII(730) /0/    M35.5866
      DATA SYMB(731) / 'NEKREQ'/,  IVEC(731) /  0/,  IASCII(731) /0/    M35.5867
      DATA SYMB(732) / 'NEKCOD'/,  IVEC(732) /  1/,  IASCII(732) /0/    M35.5868
      DATA  NUMSYM   / 732 /                                            M35.5869
      END                                                               M35.5870
      SUBROUTINE SYSPLT ( LUNIT4 )                                              
C     MODULE OF INTERACTIVE EMTP ONLY, WHICH SERVICES "EMTSPY".         M36.3401
C     IF NO INTERACTIVE EMTP USE, THIS MODULE CAN BE DELETED.           M36.3402
C     IT IS CALLED ONLY BY "SYSDEP", FOR STORAGE IN "DEKPLT".           M35.5872
C     SEPARATE MODULE NEEDED BECAUSE "DEKPLT" HAS NO IMPLICIT.          M35.5873
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      L4PLOT = LUNIT4                                                           
      RETURN                                                            M35.5876
      END                                                               M35.5877
      SUBROUTINE STOPIN                                                 M37. 830
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE OF INTERACTIVE EMTP (SPY OF "EMTSPY").           M37. 831
C     IF NON-INTERACTIVE VERSION, MODULE CAN BE DESTROYED.  THIS        M37. 832
C     MODULE IS CALLED ONLY TO DISPLAY ERRONEOUS FILE6(ISTEP), &        M37. 833
C     PROMPT USER TO SEND A CORRECTED COPY.  CALLED BY "DATAIN".        M37. 834
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      WRITE (MUNIT6, 1218)  ISTEP                                       M37. 837
 1218 FORMAT ( '   ? ? ?   Trouble with input data.  last card',        M37. 838
     1         ' number',  I5,  '   is in error.'   )                   M37. 839
      CALL WINDOW                                                               
      WRITE (MUNIT6, 1219)                                              M37. 841
 1219 FORMAT ( '     1234567890123456789012345678901234567890',         M37. 842
     1              '1234567890123456789012345678901234567890' )        M37. 843
      CALL WINDOW                                                               
      WRITE (MUNIT6, 1221)  FILE6(ISTEP)                                M37. 845
 1221 FORMAT ( A80 )                                                    M37. 846
      CALL WINDOW                                                               
      IF ( M4PLOT .EQ. 1 )  GO TO 1227                                          
      KILL = 79                                                                 
      LSTAT(19) = 1218                                                          
      GO TO 9000                                                                
 1227 WRITE (PROM80, 1232)                                              M37. 852
 1232 FORMAT ( ' SEND CORRECTED CARD (SPY, STOP) :' )                   M37. 853
      CALL PROMPT                                                               
      READ (MUNIT5, 1236)  BUFF77                                               
 1236 FORMAT ( A80 )                                                    M37. 856
      IF ( BUFF77(1:4) .EQ. 'STOP' )                                            
     1 CALL STOPTP                                                              
      IF ( BUFF77(1:4) .NE. 'SPY' )  GO TO 1244                         M37. 859
      CALL SPYING                                                               
      GO TO 9000                                                                
 1244 FILE6(ISTEP) = BUFF77                                                     
 9000 RETURN                                                                    
      END                                                               M37. 864
      SUBROUTINE  RTMPLT                                                        
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.5927
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.5928
      CALL TPPLOT                                                       M35.5929
      END                                                               M35.5930
      BLOCK DATA BLKPLT                                                 M35.5931
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.5932
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.5933
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      DATA TEXFNT  /  'F7X13.B '  /                                             
      DATA  XYTITL(1:24)  /  '                        '  /              M35.5936
      DATA  HEADL(1:16)   /  '                '  /                      M35.5937
      DATA  VERTL(1:16)   /  '                '  /                      M35.5938
      DATA  HORZL(1)    /  'DEGREES BASED ON 60 HZ  '  /                M35.5939
      DATA  HORZL(2)    /  'CYCLES BASED ON 60 HZ   '  /                M35.5940
      DATA  HORZL(3)    /  'SECONDS                 '  /                M35.5941
      DATA  HORZL(4)    /  'MILLISECONDS            '  /                M35.5942
      DATA  HORZL(5)    /  'MICROSECONDS            '  /                M35.5943
      DATA  HORZL(6)    /  'FREQUENCY IN HERTZ      '  /                M35.5944
      DATA  HORZL(7)    /  'LOG10 FREQUENCY IN HERTZ'  /                M35.5945
      DATA  HORZL(8)    /  '1ST VARIABLE OF X-Y PAIR'  /                M35.5946
      DATA  CURREN     /  'CURRENT '  /                                 M35.5947
      DATA  VOLTAG     /  'VOLTAGE '  /                                 M35.5948
C     BEGIN COMMAND-WORD DEFINITIONS.   ^^^^^^  ^^^^^^   ^^^^^^   ^^^^^ M35.5949
      DATA  CHOICE     /  'CHOICE  '  /                                 M35.5950
      DATA  STOP       /  'STOP    '  /                                 M35.5951
      DATA  PURGE      /  'PURGE   '  /                                 M35.5952
      DATA  HELP       /  'HELP    '  /                                 M35.5953
      DATA  SMOOTH     /  'SMOOTH  '  /                                 M35.5954
      DATA  SIZE       /  'SIZE    '  /                                 M35.5955
      DATA  SHOW       /  'SHOW    '  /                                 M35.5956
      DATA  LINEZZ     /  'LINE    '  /                                 M35.5957
      DATA  PHOTO      /  'COPY    '  /                                 M35.5958
      DATA  END        /  'END     '  /                                 M35.5959
      DATA  REPEAT     /  'REPEAT  '  /                                 M35.5960
      DATA  FLUSH      /  'FLUSH   '  /                                 M35.5961
      DATA  PLAYBA     /  'PLAYBACK'  /                                 M35.5962
      DATA  PEN        /  'PEN     '  /                                 M35.5963
      DATA  MULTIP     /  'FACTOR  '  /                                 M35.5964
      DATA  OFFSET     /  'OFFSET  '  /                                 M35.5965
      DATA  LIMITS     /  'LIMITS  '  /                                 M35.5966
      DATA  TIME       /  'TIME    '  /                                 M35.5967
      DATA  TIMESP     /  'TIMESPAN'  /                                 M35.5968
      DATA  DEBUG      /  'DEBUG   '  /                                 M35.5969
      DATA  TEK        /  'TEK     '  /                                 M35.5970
      DATA  STACK      /  'STACK   '  /                                 M35.5971
      DATA  PRINTE     /  'PRINTER '  /                                 M35.5972
      DATA  METRIC     /  'METRIC  '  /                                 M35.5973
      DATA  ALLTIM     /  'ALL TIME'  /                                 M35.5974
      DATA  COLUMN     /  'COLUMN  '  /                                 M35.5975
      DATA  SETCOL     /  'SET COLU'  /                                 M35.5976
      DATA  OUT        /  'OUT     '  /                                 M35.5977
      DATA  LONGER     /  'LONGER  '  /                                 M35.5978
      DATA  AVERAG     /  'AVERAGE '  /                                 M35.5979
      DATA  INNER      /  'IN      '  /                                 M35.5980
      DATA  RESCAL     /  'RESCALE '  /                                 M35.5981
      DATA  LASTPL     /  'LAST    '  /                                 M35.5982
      DATA  BATCH      /  'BATCH   '  /                                 M35.5983
      DATA  PUNCH      /  'PUNCH   '  /                                 M35.5984
      DATA  EXTREM     /  'EXTREMA '  /                                 M35.5985
      DATA  LEVEL      /  'LEVEL   '  /                                 M35.5986
      DATA  NOPLOT     /  'NO PLOT '  /                                 M35.5987
      DATA  MESSAG     /  'MESSAGE '  /                                 M35.5988
      DATA  TIMEUN     /  'TIME UNI'  /                                 M35.5989
      DATA  LABEL      /  'LABEL   '  /                                 M35.5990
      DATA  CURSOR     /  'CURSOR  '  /                                 M35.5991
      DATA  XYPLOT     /  'X-Y PLOT'  /                                 M35.5992
      DATA  SLOPE      /  'SLOPE   '  /                                 M35.5993
      DATA  BACK       /  'BACK    '  /                                 M35.5994
      DATA  REFILE     /  'REFILE  '  /                                 M35.5995
      DATA  TEXBLK     /  'BLANK   '  /                                 M35.5996
      DATA  SETDAT     /  'SET DATA'  /                                 M35.5997
C     END OF COMMAND DEFINITIONS  ^^^^^^   ^^^^^^   ^^^^^^   ^^^^^^     M35.5998
      DATA  TOLRCE     /  8.E-5  /                                      M35.5999
      DATA  FINFIN     /  1.E12  /                                      M35.6000
      DATA  TIMBEG     /   0.0   /                                      M35.6001
      DATA  TIMEND     /  1.E20  /                                      M35.6002
      DATA  PAPLIM     /  36.    /                                      M35.6003
      DATA  VS      /  1.0  /                                           M35.6004
      DATA  VL      /  5.0  /                                           M35.6005
      DATA  VH      /  6.0  /                                           M35.6006
      DATA  NSMPLT  /  50   /                                           M35.6007
      DATA  KSLOWR  /   3   /                                                   
      DATA  LIMFIX  /   0   /                                           M35.6008
      DATA  KLEVL   /   0   /                                           M35.6009
      DATA  KEXTR   /   0   /                                           M35.6010
      DATA  JHMSP   /   0   /                                           M35.6011
      DATA  TAXISL  /  5.0  /                                                   
      DATA  MU6STD  /  6  /                                                     
      DATA  HTAX    /  4.0  /                                           M35.6013
      DATA  LIMCOL  /  79   /                                           M35.6014
      DATA  LTEK    /  1   /                                                    
      DATA  NUMTEK  /   0   /                                           M35.6016
      DATA  INWAIT  /   1   /                                           M35.6017
C     BEGIN PARAMETERS OF TEKTRONIX SCREEN                              M35.6018
      DATA  NXINCH   /    74   /                                        M35.6019
      DATA  NYINCH   /    68   /                                        M35.6020
      DATA  NXOFF    /    100  /                                        M35.6021
      DATA  NYOFF    /    40   /                                        M35.6022
      DATA  NXVERN   /    30   /                                        M35.6023
      DATA  INCHPX   /    2    /                                        M35.6024
      DATA  INCHPY   /    2    /                                        M35.6025
      DATA  LOOK     /    6    /                                        M35.6026
      DATA  NYMAX    /   800   /                                        M35.6027
      DATA  NXMAX    /   800   /                                        M35.6028
      DATA  LCHID    /    2    /                                        M35.6029
      DATA  LCHSUP   /    1    /                                        M35.6030
      DATA  LCHTIT   /    2    /                                        M35.6031
      DATA  LCHXAX   /    0    /                                        M35.6032
      DATA  LCHYAX   /    0    /                                        M35.6033
      DATA  IZGR1    /    0    /                                        M35.6034
      DATA  IZGR2    /    0    /                                        M35.6035
      DATA  LDSHG1   /    1    /                                        M35.6036
      DATA  LDSHG2   /    1    /                                        M35.6037
      DATA  IZXAX    /    0    /                                        M35.6038
      DATA  IZYAX    /    0    /                                        M35.6039
      DATA  IZID     /    0    /                                        M35.6040
      DATA  ITERM    /    2    /                                        M35.6041
      DATA  LTIC     /    7    /                                        M35.6042
      DATA  IZTIT    /    0    /                                        M35.6043
      DATA  NXID6    /   10   /                                                 
      DATA  NYID6    /   330  /                                                 
      DATA  NXEND    /   512   /                                        M35.6046
      DATA  NYEND    /    50   /                                        M35.6047
      DATA  ICURSE   /    0    /                                        M35.6048
      DATA  ICHREF   /   1HP   /                                        M35.6049
      DATA  ICHEND   /   1HE   /                                        M35.6050
      DATA  VAXISL   /   4.0   /                                                
      DATA  FXNUMV   /   1.5   /                                        M35.6052
      DATA  FXNUMH   /   5.0   /                                        M35.6053
      DATA  FXVERT   /   0.0   /                                        M35.6054
      DATA  FSYMB    /   .83   /                                        M35.6055
      DATA  LSYMB    /    1    /                                        M35.6056
      DATA  LCHFIL   /    0    /                                        M35.6057
      DATA  LCHLIM   /    0    /                                        M35.6058
      DATA  IBAUD    /   960   /                                                
C     END PARAMETERS OF TEKTRONIX SCREEN                                M35.6060
      DATA  LU7PLT   /  7  /                                            M35.6061
      DATA  LINEPR   /  9  /                                            M35.6062
      DATA  MXYPL    /  0  /                                            M35.6063
      DATA  NUMFLT   /  0  /                                            M35.6064
      DATA  NUMTIT   /  0  /                                            M35.6065
      DATA  XTIT     /  0.5  /                                          M35.6066
      DATA  YTIT     /  8.5  /                                          M35.6067
      DATA  SIZTIT   /  .12  /                                          M35.6068
      DATA  XSUPER   /  1.0  /                                          M35.6069
      DATA  YSUPER   /  9.0  /                                          M35.6070
      DATA  SIZSUP   /  0.3  /                                          M35.6071
      DATA  NCHSUP   /   0   /                                          M35.6072
      DATA  NCHVER   /   0   /                                          M35.6073
      DATA  DXGRD1   /  1.0  /                                          M35.6074
      DATA  DYGRD1   /  1.0  /                                          M35.6075
      DATA  DXGRD2   /  0.2  /                                          M35.6076
      DATA  DYGRD2   /  0.2  /                                          M35.6077
      DATA  FILL1    /  1.0  /                                          M35.6078
      DATA  FILL2    /  1.0  /                                          M35.6079
      DATA  NCUT1    /   1   /                                          M35.6080
      DATA  NCUT2    /   1   /                                          M35.6081
      DATA  MAXSYM   /   3   /                                          M35.6082
      DATA  FLINE    /  1.7  /                                          M35.6083
      DATA  SIZID    /  .12  /                                          M35.6084
      DATA  XID      /  0.5  /                                          M35.6085
      DATA  YID      /  .75  /                                          M35.6086
      DATA  FACT     /  1.0  /                                          M35.6087
      DATA  FHTAX    /  0.5  /                                          M35.6088
      DATA  FXSUP    /  0.3  /                                          M35.6089
      DATA  FYSUP    /  -.03 /                                          M35.6090
      DATA  FXTIT    /  .10  /                                          M35.6091
      DATA  FYTIT    /  .15  /                                          M35.6092
      DATA  FXID     /  .05  /                                          M35.6093
      DATA  FYID     /  .10  /                                          M35.6094
      DATA  FTCARR   /  1.0  /                                          M35.6095
      DATA  FVAXTT   /  -7.5  /                                         M35.6096
      DATA  MTIT     /  3  /                                            M35.6097
      DATA  MAXISX   /  3  /                                            M35.6098
      DATA  MAXISY   /  3  /                                            M35.6099
      DATA  MGRID1   /  2  /                                            M35.6100
      DATA  MGRID2   /  1  /                                            M35.6101
      DATA  MSUPER   /  5  /                                            M35.6102
      DATA  MID      /  3  /                                            M35.6103
      DATA  MLINE    /  3  /                                            M35.6104
      DATA  KILLPL   /  0  /                                            M35.6105
      END                                                               M35.6106
      SUBROUTINE TPPLOT                                                         
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.6108
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.6109
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      SAVE                                                              M35.6112
      IF ( NEXMOD .GT. 4 ) GO TO 3769                                           
      IF ( NEXMOD .NE. 4 ) GO TO 1742                                           
      NEXMOD = 0                                                                
      GO TO NEXTSN                                                              
 1742 HMAX = 0.0                                                                
      HMIN = 0.0                                                                
      IHS = 3                                                                   
      TMULT = 1.0                                                       M35.6117
      MAXEV = LOCINT ( BX(1) )     -     LOCINT ( EV(1) )               M35.6118
      MAXEW = LOCINT ( FINFIN )    -     LOCINT ( EW(1) )               M35.6119
      MAXEW = MAXEW - 50                                                M35.6120
      DO 1751  J=1, 6                                                   M35.6126
 1751 SEXT(J) = BLAN80                                                  M38.4688
      DO 1765  I=1, 20                                                  M35.6130
      AAA(I) = 1.0                                                      M35.6131
      BBB(I) = 0.0                                                      M35.6132
      MMM(I) = 0                                                        M35.6133
      YLEVEL(I) = 0.0                                                   M35.6134
 1765 SLOT1(I) = '        '                                             M35.6135
  563 DO 564  J=1, 20                                                   M35.6136
  564 MCURVE(J) = MLINE                                                 M35.6137
 1000 WRITE (PROM80, 5000)                                              M35.6138
 5000 FORMAT (  ' --- OUTER :'  )                                       M35.6139
      ASSIGN 5003 TO NEXTSN                                                     
      GO TO 9800                                                                
 5003 BUFFIN = BUFF77                                                           
      KILL = 0                                                          M35.6141
      IF ( BUFFIN(1:8)  .NE.  PURGE )   GO TO 2716                      M35.6144
      CLOSE ( UNIT=14,  STATUS='DELETE' )                               M35.6145
      GO TO 1000                                                        M35.6146
 2716 IF ( BUFFIN(1:8)  .NE.  SETDAT  )   GO TO 2757                    M35.6147
      CALL SETRTM                                                       M35.6148
      GO TO 563                                                         M35.6149
 2757 IF ( BUFFIN(1:8)  .NE.  DEBUG )   GO TO 7368                      M35.6150
      WRITE (PROM80, 7358)  IPRSPY                                      M36.3415
 7358 FORMAT (  '  SUPPLY LEVEL-NUMBER  IPRSPY  (',                     M35.6151
     1          I3,  ' ) :'  )                                          M35.6152
      ASSIGN 7361 TO NEXTSN                                                     
      GO TO 9800                                                                
 7361 CALL FREIN1 ( BUFF77, IPRSPY )                                    M38.4694
      GO TO 1000                                                        M35.6153
 7368 IF ( IPRSPY .LT. 1 )  GO TO 3877                                          
      WRITE (MUNIT6, 3876)  BUFFIN                                      M37.7332
 3876 FORMAT ( '  BUFFIN vector =',  A80  )                             M37.7333
      CALL WINDOW                                                               
 3877 IF( BUFFIN(1:8) .EQ.  STOP ) GO TO 3818                           M38.4695
      IF ( BUFFIN(1:8)  .NE.  TEK )   GO TO 5136                        M38.4696
      IF ( LTEK .EQ. 1 )  GO TO 7387                                            
      WRITE (MUNIT6, 7385)                                              M37.7337
 7385 FORMAT ( '  ---- Switch from character to vector-',               M37.7338
     1         'graphic plotting.'  )                                   M37.7339
      CALL WINDOW                                                               
      GO TO 7388                                                                
 7387 WRITE (MUNIT6, 7386)                                                      
 7386 FORMAT ( '  ---- Switch from vector-graphic to',                  M37.7343
     1         ' character plotting'  )                                 M37.7344
      CALL WINDOW                                                               
 7388 LTEK = LTEK + 1                                                   M37.7346
      IF ( LTEK  .EQ.  2 )   LTEK = 0                                   M35.6176
      GO TO 1000                                                        M35.6177
 5136 IF ( BUFFIN(1:8)  .NE.  COLUMN )   GO TO 5143                     M35.6178
      N1 = LIMCOL                                                       M35.6179
      IF ( N1  .NE.  131 )   LIMCOL = 131                               M35.6180
      IF ( N1  .NE.  79)   LIMCOL =  79                                 M35.6181
      WRITE (MUNIT6, 5139)  N1                                          M37.7347
 5139 FORMAT (  '   Character-plot column width was',  I5,              M37.7348
     1          '  columns.'   )                                        M37.7349
      CALL WINDOW                                                               
      WRITE (MUNIT6, 5140)  LIMCOL                                      M37.7351
 5140 FORMAT (  '   It is now being toggled to',  I5,  ' .'  )          M37.7352
      CALL WINDOW                                                               
      GO TO 1000                                                        M35.6186
 5143 IF ( BUFFIN(1:8)  .NE.  SETCOL )   GO TO 5154                     M35.6187
      WRITE (PROM80, 5146)  LIMCOL                                      M36.3427
 5146 FORMAT (  '  SUPPLY PRINTER-PLOT COLUMN WIDTH (',  I4,            M35.6188
     1          ' ) :'  )                                               M35.6189
      ASSIGN 5147 TO NEXTSN                                                     
      GO TO 9800                                                                
 5147 CALL FREIN1 ( BUFF77, LIMCOL )                                    M38.4699
      WRITE (MUNIT6, 5148)  LIMCOL                                      M35.6190
 5148 FORMAT (  '   Confirmation.   New value =',  I4,  ' .'  )         M37.7354
      CALL WINDOW                                                               
      GO TO 1000                                                        M35.6192
 5154 IF ( BUFFIN(1:8)  .EQ.  INNER )   GO TO 3769                      M38.4700
      IF ( BUFFIN(1:4) .EQ. 'STOP' )  RETURN                                    
      IF ( BUFFIN(1:5)  .NE.  HELP(1:5) )   GO TO 5200                  M37.7356
      CALL HELPER ( 1 )                                                 M35.6201
      GO TO 1000                                                        M35.6202
 5200 NEWFIL = 0                                                        M35.6204
      TSTEP = -1.0                                                      M35.6205
      REWIND L4PLOT                                                     M35.6206
      READ (L4PLOT)  DATEPL, TCLOPL, NUMNAM, NUMNVO, NUMBCO,            M35.6207
     1     NUMBRN,  ( BUSLST(I), I=1, NUMNAM )                          M35.6208
      IF ( IPRSPY .LT. 2 )  GO TO 5739                                          
      WRITE (MUNIT6, 1001)  DATEPL, TCLOPL,                             M37.7358
     1                      NUMNAM, NUMNVO, NUMBCO, NUMBRN              M37.7359
 1001 FORMAT ( 23H Plot-file header info.,  2(1X, 2A4), 4I6 )           M37.7360
      CALL WINDOW                                                               
      DO 5736  J=1, NUMNAM, 10                                                  
      WRITE (MUNIT6, 5733)  ( BUSLST(K), K=J, J+9 )                             
 5733 FORMAT ( 1X, 10A7 )                                               M37.7364
      CALL WINDOW                                                               
 5736 CONTINUE                                                          M37.7366
 5739 KPTPLT = NUMNVO + NUMBRN                                                  
      IF ( NUMNVO  .GT.  0 )                                            M35.6215
     1 READ (L4PLOT)  ( IBSOUT(J), J=1, NUMNVO )                        M35.6216
      IF ( NUMBRN  .GT.  0 )   READ (L4PLOT)                            M35.6217
     1      (IBRNCH(J), J=1, NUMBRN), (JBRNCH(J), J=1, NUMBRN)          M35.6218
      IF ( IPRSPY .LT. 2 ) GO TO 5754                                           
      DO 5742  J=1, NUMNVO, 10                                                  
      WRITE (MUNIT6, 1002)  ( IBSOUT(K), K=J, J+9 )                             
 1002 FORMAT (  ' Node numbers IBSOUT:',  10I5  )                       M37.7371
      CALL WINDOW                                                               
 5742 CONTINUE                                                          M37.7373
      DO 5747  J=1, NUMBRN, 5                                                   
      WRITE (MUNIT6, 5744)  (IBRNCH(K), JBRNCH(K), K=J, J+4)            M37.7375
 5744 FORMAT ( ' Branch node pairs:',  5( 2X, 2I4)  )                   M37.7376
      CALL WINDOW                                                               
 5747 CONTINUE                                                          M37.7378
 5754 NUMOUT = NUMNVO + 2 * NUMBRN                                      M37.7379
      NV = NUMBRN - NUMBCO                                              M35.6225
      JBEGBV = NUMNVO + 1                                               M35.6226
      JBEGBC = JBEGBV + 2 * NV                                          M35.6227
      NT2 = NUMBRN * 2                                                  M35.6228
      I = 1                                                             M35.6229
 1008 IF ( I  .GT.  NUMNVO )   GO TO 1009                               M35.6230
      J = IBSOUT(I)                                                     M35.6231
      BBUS(I) = BUSLST(J)                                               M35.6232
      I = I + 1                                                         M35.6233
      GO TO 1008                                                        M35.6234
 1009 J = 1                                                             M35.6235
 1012 IF ( J  .GT.  NUMBRN )   GO TO 1013                               M35.6236
      K = IBRNCH(J)                                                     M35.6237
      L = JBRNCH(J)                                                     M35.6238
      BBUS(I) = BUSLST(K)                                               M35.6239
      I = I + 1                                                         M35.6240
      BBUS(I) = BUSLST(L)                                               M35.6241
      I = I + 1                                                         M35.6242
      J = J + 1                                                         M35.6243
      GO TO 1012                                                        M35.6244
 1013 IF ( IPRSPY .LT. 2 ) GO TO 3769                                           
      WRITE (MUNIT6, 1014)  NUMOUT, NUMNVO, NV, NT2, I                  M37.7381
 1014 FORMAT ( ' NUMOUT, NUMNVO, NV, NT2, I =',  5I6 )                  M37.7382
      CALL WINDOW                                                               
      DO 5766  J=1, NUMOUT, 10                                                  
      WRITE (MUNIT6, 5763) ( BBUS(M), M=J, J+9 )                                
 5763 FORMAT ( 1X, 10A6 )                                               M37.7386
      CALL WINDOW                                                               
 5766 CONTINUE                                                          M37.7388
 3769 CALL PLTVAR                                                       M35.6249
      IF ( NEXMOD .GT. 0 ) GO TO 9835                                           
      IF ( BUFFIN(1:4) .EQ. 'STOP' )  RETURN                                    
C     IF ( BUFFIN(1:4) .EQ. 'SPY ' )  RETURN  ! ABORT "PLOT" USE        M38.4702
      IF ( KILL  .EQ.  0 )   GO TO 1000                                 M35.6252
 3818 CLOSE ( UNIT=14,  STATUS='KEEP' )                                 M37.7389
C     NOW STOP.   IF THERE WERE TEKTRONIX CRT PLOTS, CALL FOR           M38.4703
C     BUFFER FLUSHING IN  #TEKPLT# .                                    M35.6256
      IF ( NUMTEK  .GT.  0 )   CALL TEKPLT                              M35.6258
 9800 IF ( IPRSPY .LT. 1 ) GO TO 9817                                           
      WRITE (MUNIT6, 9807)   PROM80(1:40)                               M38.4705
 9807 FORMAT (  ' EXIT "TPPLOT".   PROM80(1:40) =',  A40  )             M38.4706
      CALL WINDOW                                                               
 9817 NEXMOD = 4                                                                
 9835 RETURN                                                            M38.4709
      END                                                               M35.6260
      SUBROUTINE HELPER ( N1 )                                                  
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.6262
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.6263
C     THIS MODULE DOES NOTHING OTHER THAN SERVICE THE "HELP"            M37.7390
C     SUBCOMMAND OF THE "PLOT" COMMAND OF SPY.                          M37.7391
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      N8 = N1                                                                   
      IF ( BUFFIN(5:10) .NE. '      ' )  GO TO 3618                     M37.7394
 3613 K = NUMKEY + N8 + 1                                                       
      N23 = KBEGTX(K)                                                           
      N24 = KBEGTX(K+1) - 1                                                     
      GO TO 3673                                                                
 3618 IF ( BUFFIN(6:10) .NE. 'OUTER' ) GO TO 3622                       M37.7399
      N8 = 1                                                            M37.7400
      GO TO 3613                                                        M37.7401
 3622 IF ( BUFFIN(6:11) .NE. 'MIDDLE' )  GO TO 3627                     M37.7402
      N8 = 2                                                            M37.7403
      GO TO 3613                                                        M37.7404
 3627 IF ( BUFFIN(6:10) .NE. 'INNER' )  GO TO 3634                      M37.7405
      N8 = 3                                                            M37.7406
      GO TO 3613                                                        M37.7407
 3634 N23 = KBEGTX(NUMKEY+1)                                            M37.7408
      N24 = KBEGTX(NUMKEY+5) - 1                                        M37.7409
      IF ( BUFFIN(6:8) .EQ. 'ALL' )  GO TO 3673                                 
      DO 3642  J=N23, N24                                                       
      IF ( TEXSPY(J)(1:3) .EQ. '   ' )  GO TO 3642                      M37.7412
      IF ( TEXSPY(J)(3:10) .EQ. BUFFIN(6:13) )  GO TO 3649              M37.7413
 3642 CONTINUE                                                          M37.7414
      WRITE (MUNIT6, 3645)                                              M37.7415
 3645 FORMAT ( '    ? ? ?   Sorry, no such plot command.',              M37.7416
     1         '   Try again ... '  )                                   M37.7417
      CALL WINDOW                                                               
      GO TO 9000                                                                
 3649 N23 = J                                                                   
      DO 3653  J=N23+1, N24                                                     
      IF ( TEXSPY(J)(1:3) .NE. '   ' )  GO TO 3658                      M37.7422
 3653 CONTINUE                                                          M37.7423
 3658 N24 = J - 1                                                               
 3673 DO 3681  J=N23, N24                                                       
      WRITE (MUNIT6, 3677)  TEXSPY(J)                                   M37.7426
 3677 FORMAT ( A80 )                                                    M37.7427
      CALL WINDOW                                                               
 3681 CONTINUE                                                          M37.7429
 9000 IF ( IPRSPY .LT. 1 ) GO TO 9011                                           
      WRITE (MUNIT6, 9004)  N1, N8, N23, N24                            M37.7431
 9004 FORMAT ( ' EXIT  "HELPER".  N1, N8, N23, N24 =',  4I6 )           M37.7432
      CALL WINDOW                                                               
 9011 RETURN                                                            M37.7434
      END                                                               M35.6599
      SUBROUTINE PLTVAR                                                         
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.6601
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.6602
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      IF ( IPRSPY .LT. 1 ) GO TO 1003                                           
      WRITE (MUNIT6, 1002)  NEXMOD                                      M38.4711
 1002 FORMAT ( ' ENTER "PLTVAR".  NEXMOD =',  I4 )                      M38.4712
      CALL WINDOW                                                               
 1003 IF ( NEXMOD .EQ. 6 ) GO TO 1550                                           
      IF ( NEXMOD .NE. 5 )  GO TO 1005                                          
      NEXMOD = 0                                                                
      GO TO NEXTSN                                                              
 1005 TOLRCE = 0.0                                                              
      GO TO 1008                                                        M35.6606
 1053 WRITE(PROM80,1045)                                                M35.6607
 1045 FORMAT (  ' --- MIDDLE :'  )                                      M35.6608
      ASSIGN 1048 TO NEXTSN                                                     
      GO TO 9800                                                                
 1048 BUFFIN = BUFF77                                                           
      IF ( BUFFIN(1:4) .EQ. 'STOP' )  RETURN                                    
C     IF ( BUFFIN(1:4) .EQ. 'SPY ' )  RETURN  ! ABORT "PLOT" USE        M38.4722
      IF ( BUFFIN(1:8)  .NE.  REFILE )   GO TO 1007                     M35.6614
      WRITE (PROM80, 1004)                                              M37.7435
 1004 FORMAT (  ' FILE :' )                                             M35.6616
      ASSIGN 1052 TO NEXTSN                                                     
      GO TO 9800                                                                
 1052 BUFFIN = BUFF77                                                           
      NEWFIL = 1                                                        M35.6619
      GO TO 1053                                                        M35.6620
 1007 IF ( BUFFIN(1:8)  .EQ.  INNER )   GO TO 1550                      M35.6621
      IF ( BUFFIN(1:8)  .NE.  TIMESP )   GO TO 5681                     M35.6622
 1008 TIMBEG = PLTBUF(NEWVEC)                                           M35.6623
      N4 = (INDBUF - NEWVEC ) / (KPTPLT + 1 )                           M35.6624
      IF ( IPRSPY .LT. 1 ) GO TO 5673                                           
      WRITE (MUNIT6, 5672)  INDBUF, NEWVEC, KPTPLT, N4                  M37.7438
 5672 FORMAT (' Timespan calc.  INDBUF, NEWVEC, KPTPLT, N4 =', 4I8)     M37.7439
      CALL WINDOW                                                               
 5673 N4 = NEWVEC + N4 * (KPTPLT + 1)                                   M37.7441
      TIMEND = PLTBUF(N4)                                               M35.6629
      IF ( IPRSPY .LT. 1 ) GO TO 35675                                          
      WRITE (MUNIT6, 5675)  NEWVEC, N4, TIMBEG, TIMEND                  M37.7443
 5675 FORMAT ( ' PLTBUF cells', 2I8,    '     TMIN, TMAX',              M37.7444
     1         ' (SEC) =',  2E14.5  )                                   M35.6633
      CALL WINDOW                                                               
35675 IF ( IPRSPY .LT. 5 )  GO TO 3218                                          
      DO 3214  M=NEWVEC, N4, 8                                                  
      WRITE (MUNIT6, 5676) J, (PLTBUF(IP), IP=J, J+7)                           
 5676 FORMAT ( ' PLTBUF(',  I6,  2H:),  8E14.5  )                       M37.7449
      CALL WINDOW                                                               
 3214 CONTINUE                                                          M37.7451
 3218 WRITE (MUNIT6, 5677)  TIMBEG, TIMEND                              M37.7452
 5677 FORMAT ( '   Time limits are :',  2E14.6  )                       M37.7453
      CALL WINDOW                                                               
      IF ( IHS  .EQ.  0 )   IHS = 3                                     M35.6640
      GO TO 7531                                                        M35.6641
 5681 IF (  BUFFIN(1:8)  .NE.  CHOICE )   GO TO 1957                    M35.6642
      CALL SPYLIN                                                               
      I = 1                                                             M35.6643
      NC = 2 * NV                                                       M35.6644
      WRITE (MUNIT6, 1041)                                              M37.7456
 1041 FORMAT ( 32H Type-1 entries (node voltages).   )                  M37.7457
      CALL WINDOW                                                               
      DO 3227  J=1, NUMNVO, 10                                                  
      N13 = J+9                                                                 
      IF ( N13 .GT. NUMNVO )  N13 = NUMNVO                                      
      WRITE (MUNIT6, 3223) ( BBUS(IP), IP=J, N13 )                              
 3223 FORMAT ( 1X, 10A7 )                                               M37.7463
      CALL WINDOW                                                               
 3227 CONTINUE                                                          M37.7465
      I = I + NUMNVO                                                    M35.6648
      JJ = I + NC - 1                                                   M35.6650
      WRITE (MUNIT6, 1047)                                              M37.7466
 1047 FORMAT ( 44H Type-8 entries (branch voltages or powers).  )       M37.7467
      CALL WINDOW                                                               
      DO 3236  J=I, JJ, 8                                                       
      N13 = J+7                                                                 
      IF ( N13 .GT. JJ )  N13 = JJ                                              
      WRITE (MUNIT6, 3232) ( BBUS(IP), IP=J, N13 )                              
 3232 FORMAT ( 1X,  4(2A7, 3X)  )                                       M37.7473
      CALL WINDOW                                                               
 3236 CONTINUE                                                          M37.7475
      I = I + NC                                                        M35.6654
      WRITE (MUNIT6, 1051)                                              M37.7476
 1051 FORMAT ( 46H Type-9 entries (branch currents or energies).  )     M37.7477
      CALL WINDOW                                                               
      DO 3247  J=I, NUMOUT, 8                                                   
      N13 = J+7                                                                 
      IF ( N13 .GT. NUMOUT )  N13 = NUMOUT                                      
      WRITE (MUNIT6, 3232) ( BBUS(IP), IP=J, N13 )                              
      CALL WINDOW                                                               
 3247 CONTINUE                                                          M37.7484
      GO TO 1053                                                        M35.6659
 1957 IF ( BUFFIN(1:8)  .NE.  STOP )   GO TO 4423                       M35.6660
      KILL = 99                                                         M35.6661
      RETURN                                                            M35.6662
 4423 IF ( BUFFIN(1:8) .NE. PURGE ) GO TO 1958                          M35.6663
      CLOSE ( UNIT=14,  STATUS='DELETE' )                               M35.6664
      RETURN                                                            M35.6665
 1958 IF ( BUFFIN(1:8) .NE. OUT )GO TO 1931                             M35.6666
      RETURN                                                            M35.6667
 1931 IF ( BUFFIN(1:5) .NE. HELP(1:5) ) GO TO 1059                      M37.7485
      CALL HELPER ( 2 )                                                 M35.6669
      GO TO 1053                                                        M35.6670
 1059 IF ( BUFFIN(1:8)  .EQ.  LABEL    )   GO TO 1210                   M35.6671
      IF ( BUFFIN(1:8)  .NE.  TIMEUN )   GO TO 1167                     M35.6672
 1155 WRITE (PROM80, 1164)  IHS                                         M36.3435
 1164 FORMAT (  '   SEND TIME-UNITS CODE (',  I2,  ' ) :'  )            M35.6673
      ASSIGN 1165 TO NEXTSN                                                     
      GO TO 9800                                                                
 1165 CALL FREIN1 ( BUFF77, N4 )                                                
      IF ( N4  .GT.  0 )   IHS = N4                                     M35.6674
      IF ( IHS  .GT.  0     .AND.     IHS  .LT.  8 )                    M35.6675
     1 GO TO 7531                                                       M35.6676
      WRITE (MUNIT6, 4163)                                              M35.6677
 4163 FORMAT (  '  ? ? ?  Illegal value.  Try again ...' )              M37.7486
      CALL WINDOW                                                               
      GO TO 1155                                                        M35.6679
 7531 IF ( IHS  .EQ.  1 )   TMULT = 21600.                              M35.6680
      IF ( IHS  .EQ.  2 )   TMULT = 60.                                 M35.6681
      IF ( IHS  .EQ.  3 )   TMULT = 1.0                                 M35.6682
      IF ( IHS  .EQ.  4 )   TMULT = 1000.                               M35.6683
      IF ( IHS  .EQ.  5 )   TMULT = 1.E6                                M35.6684
      IF ( IHS  .EQ.  6 )   TMULT = 1.0                                 M35.6685
      IF ( IHS  .EQ.  7 )   TMULT = 1.0                                 M35.6686
      GO TO 1053                                                        M35.6687
 1167 L = 1                                                             M35.6688
      ICP = 4                                                           M35.6689
      JPLT = 0                                                          M35.6690
 7338 WRITE (PROM80, 7341)  SLOT1(L)                                    M35.6691
 7341 FORMAT (  '   SEND NODE NAME OR END (',  A6,                      M35.6692
     1          ') :'   )                                               M35.6693
      ASSIGN 7342 TO NEXTSN                                                     
      GO TO 9800                                                                
 7342 TEXTD1 = BUFF77(1:8)                                                      
      IF ( TEXTD1  .EQ.  LASTPL )   GO TO 1175                          M35.6696
 7344 IF ( TEXTD1  .NE.  END  )   GO TO 7348                            M35.6697
      ICP = 8                                                           M35.6698
      BRCLAS = VOLTAG                                                   M35.6699
      GO TO 1168                                                        M35.6700
 7348 IF ( TEXTD1  .EQ.  '        ' )   TEXTD1 = SLOT1(L)               M35.6701
      IF ( TEXTD1  .EQ.  BACK  )   GO TO 1053                           M35.6702
      IF ( L  .EQ.  1     .AND.     JPLT  .GT.  0                       M35.6703
     1     .AND.   TEXTD1  .EQ.  REPEAT  )   GO TO 1210                 M35.6704
      K = 0                                                             M35.6705
 1600 K=K+1                                                             M35.6706
      IF ( K  .LE.  NUMNVO )   GO TO 1640                               M35.6707
      WRITE (MUNIT6,1620) TEXTD1                                        M35.6708
 1620 FORMAT ( ' ??? List of node voltages does not include  "',        M37.7488
     1  A6,  '" .   Try again ....'  )                                  M37.7489
      CALL WINDOW                                                               
      GO TO 7338                                                        M35.6711
 1640 WRITE (ANSI, 1641)  BBUS(K)                                               
 1641 FORMAT ( A6, 2X )                                                 M35.6713
      IF ( TEXTD1 .NE. ANSI )  GO TO 1600                               M35.6714
      JPLT = L                                                          M35.6715
      L = L + 1                                                         M35.6716
      MPLOT(JPLT)=K                                                     M35.6717
      SLOT1(JPLT) = TEXTD1                                              M35.6718
      GO TO 7338                                                        M35.6719
 1168 WRITE (PROM80, 1169)  BRCLAS,  SLOT1(L), SLOT1(L+1)               M35.6720
 1169 FORMAT (  '   SEND BRANCH ',  A7,  ' NAMES OR END (',             M35.6721
     1  A6,  1H,,  A6,  ' ) :'   )                                      M35.6722
      ASSIGN 31169 TO NEXTSN                                                    
      GO TO 9800                                                                
31169 TEXTD1 = BUFF77(1:6)                                                      
      TEXTD2 = BUFF77(7:12)                                                     
      IF ( TEXTD1  .EQ.  BACK  )   GO TO 1053                           M35.6726
      IF ( TEXTD1  .EQ.  END  )   GO TO 1175                            M35.6727
      IF ( TEXTD1  .EQ.  LASTPL )   GO TO 1175                          M35.6728
      IF ( TEXTD1  .EQ.  '        '     .AND.                           M35.6729
     1     TEXTD2  .EQ.  '        '  )   GO TO 1173                     M35.6730
      SLOT1(L) = TEXTD1                                                 M35.6731
      SLOT1(L+1) = TEXTD2                                               M35.6732
 1173 IF ( SLOT1(L)  .EQ.  '        '     .AND.                         M35.6733
     1   SLOT1(L+1)  .EQ.  '        '  )   GO TO 1168                   M35.6734
      IF ( ICP  .EQ.  9 )   GO TO 1720                                  M35.6735
      IB = JBEGBV                                                       M35.6736
      IL = JBEGBC                                                       M35.6737
      N1 = IB                                                           M35.6738
      GO TO 1740                                                        M35.6739
 1720 IB=JBEGBC                                                         M35.6740
      IL=NUMOUT                                                         M35.6741
      N1 = JBEGBV + NV                                                  M35.6742
 1740 IF (IB.LT.IL) GO TO 1780                                          M35.6743
      WRITE (MUNIT6, 1760)  BRCLAS, SLOT1(L), SLOT1(L+1)                M35.6744
 1760 FORMAT (  ' ??? Branch ',  A7,  ' list does not include',         M37.7491
     1          ' an entry from  "',  A6,  '"  to  "',  A6,             M37.7492
     2          ' " .   Try again ....'  )                              M37.7493
      CALL WINDOW                                                               
      GO TO 1168                                                        M35.6748
 1780 WRITE (ANSI, 1641)  BBUS(IB)                                              
      IF ( SLOT1(L) .NE. ANSI )   GO TO 1800                            M35.6750
      WRITE (ANSI, 1641)  BBUS(IB+1)                                            
      IF ( SLOT1(L+1) .NE. ANSI )   GO TO 1820                          M35.6752
C               NODE PAIR FOUND - SIGN CORRECT                          M35.6753
      MPLOT(JPLT+1)= N1                                                 M35.6754
      GO TO 1840                                                        M35.6755
 1800 WRITE (ANSI, 1641)  BBUS(IB)                                              
      IF ( SLOT1(L+1) .NE. ANSI )   GO TO 1820                          M35.6757
      WRITE (ANSI, 1641)  BBUS(IB+1)                                            
      IF ( SLOT1(L) .NE. ANSI )   GO TO 1820                            M35.6759
C               NODE PAIR FOUND - SIGN NEGATIVE                         M35.6760
      MPLOT(JPLT+1)=-N1                                                 M35.6761
      GO TO 1840                                                        M35.6762
 1820 IB=IB+2                                                           M35.6763
      N1 = N1 + 1                                                       M35.6764
      GO TO 1740                                                        M35.6765
 1840 JPLT=JPLT+1                                                       M35.6766
      L = L + 2                                                         M35.6767
      GO TO 1168                                                        M35.6768
 1175 NAMVAR = L - 1                                                    M35.6769
      IF ( IPRSPY .LT. 1 ) GO TO 1218                                           
      WRITE (MUNIT6, 1217)  JPLT, ICP, NAMVAR                           M37.7496
 1217 FORMAT ( ' Done class.    JPLT, ICP, NAMVAR =',  3I8 )            M37.7497
      CALL WINDOW                                                               
 1218 IF ( IPRSPY .LT. 1 )  GO TO 3252                                          
      N14 = JPLT                                                        M37.7500
      IF ( N14 .GT. 20 )  N14 = 20                                              
      WRITE (MUNIT6, 3251) ( MPLOT(I), I=1, N14 )                       M37.7502
 3251 FORMAT ( ' MPLOT:', 20I6 )                                        M37.7503
      CALL WINDOW                                                               
 3252 IF ( TEXTD1  .NE.  LASTPL     .AND.                               M37.7505
     1     ICP     .LT.   9   )   GO TO 1204                            M35.6775
      IF ( JPLT  .GT.  0 )   GO TO 1210                                 M35.6776
      WRITE (MUNIT6, 1202)                                              M35.6777
 1202 FORMAT (  '  ???  Error.   No valid plot variables of',           M37.7506
     1          ' any type were specified.   Try again ....' )          M37.7507
      CALL WINDOW                                                               
      GO TO 1167                                                        M35.6780
 1204 ICP = 9                                                           M35.6781
      BRCLAS = CURREN                                                   M35.6782
      GO TO 1168                                                        M35.6783
 1210 NC = NAMVAR + 1                                                   M35.6784
      DO 1215  I=NC, 20                                                 M35.6785
 1215 SLOT1(I) = '        '                                             M35.6786
      IEVSW=0                                                           M35.6787
      NC = NT2 / 2  -  NV                                               M35.6788
      IF ( IPRSPY .LT. 1 ) GO TO 1880                                           
      WRITE (MUNIT6, 4217)  KPTPLT, NUMNVO, NUMBRN                      M37.7510
 4217 FORMAT ( ' Assign KPTPLT.  KPTPLT, NUMNVO, NUMBRN =', 3I10 )      M37.7511
      CALL WINDOW                                                               
 1880 JPLT1=JPLT+1                                                      M35.6792
      KPLT=0                                                            M35.6793
      WRITE (PROM80, 1882)  HEADL(1:16)                                 M35.6794
 1882 FORMAT (  '   SEND SUPER-TITLE (',  A16,  '...) :'   )            M35.6795
      ASSIGN 31882 TO NEXTSN                                                    
      GO TO 9800                                                                
31882 ALPHA = BUFF77                                                            
      IF ( ALPHA(1:8)  .EQ.  REPEAT  )   GO TO 1550                     M35.6798
      IF ( ALPHA(1:8)  .EQ.  TEXBLK )   GO TO 1536                      M35.6799
      DO 1535  J=1, 80                                                  M35.6800
      N3 = 81 - J                                                       M35.6801
      IF ( ALPHA(N3:N3)  .EQ.  ' ' )   GO TO 1535                       M35.6802
      HEADL = ALPHA                                                     M35.6803
      NCHSUP = N3                                                       M35.6804
      GO TO 1885                                                        M35.6805
 1535 CONTINUE                                                          M35.6806
 1536 NCHSUP = 0                                                        M35.6807
 1885 WRITE (PROM80, 1886)  VERTL(1:16)                                 M35.6808
 1886 FORMAT (  '   SEND VERTICAL AXIS LABEL (',                        M35.6809
     1  A16,  '...) :'    )                                             M35.6810
      ASSIGN 1887 TO NEXTSN                                                     
      GO TO 9800                                                                
 1887 VERTL = BUFF77                                                            
      IF ( VERTL(1:8)  .EQ.  REPEAT )   GO TO 1550                      M35.6814
      IF ( VERTL(1:8)  .EQ.  TEXBLK )  GO TO 1545                       M35.6815
      DO 1544  J=1, 78                                                          
      N3 = 81 - J                                                               
      IF ( VERTL(N3:N3) .EQ. ' ' )  GO TO 1544                                  
      NCHVER = N3                                                               
      GO TO 1888                                                                
 1544 CONTINUE                                                                  
 1545 NCHVER = 0                                                                
 1888 N3 = NUMTIT  +  1                                                 M35.6823
      WRITE (PROM80, 1891)  N3,  SEXT(N3)(1:16)                         M35.6824
 1891 FORMAT (  '   SEND CASE-TITLE LINE',  I2,  ' (',  A16,            M35.6825
     1          '...) :'   )                                            M35.6826
      ASSIGN 1892 TO NEXTSN                                                     
      GO TO 9800                                                                
 1892 ALPHA = BUFF77                                                            
      IF ( ALPHA(1:8)  .NE.  FLUSH  )   GO TO 1894                      M35.6830
      NUMTIT = 0                                                        M35.6831
      GO TO 1888                                                        M35.6832
 1894 IF ( ALPHA(1:8)  .NE.  PLAYBA  )   GO TO 1896                     M35.6833
      WRITE (MUNIT6, 1895)                                              M37.7517
 1895 FORMAT ( 6X, 'Playback of total title (80A1 FORMAT) ...' )        M37.7518
      CALL WINDOW                                                               
      DO 3267  IP=1, NUMTIT                                                     
      WRITE (MUNIT6, 3264)  SEXT(IP)                                            
 3264 FORMAT ( 1X, A80 )                                                M37.7522
      CALL WINDOW                                                               
 3267 CONTINUE                                                          M37.7524
      GO TO 1888                                                        M35.6838
 1896 IF ( ALPHA(1:8)  .EQ.  '        ' )   GO TO 1899                  M35.6839
      IF ( ALPHA(1:8)  .EQ.  END  )   GO TO 1550                        M35.6840
      IF ( ALPHA(1:8)  .NE.  TEXBLK )   GO TO 1897                      M35.6841
      ALPHA(1:8) = '        '                                                   
 1897 SEXT(N3) = ALPHA                                                          
 1899 NUMTIT = N3                                                               
      IF ( NUMTIT .LT. 5 )  GO TO 1901                                          
      WRITE (MUNIT6, 1547)                                              M37.7526
 1547 FORMAT (  ' ** Warning.  Title storage is now full.',             M37.7527
     1          '   No additional lines can be accepted.'  )            M37.7528
      CALL WINDOW                                                               
 1901 GO TO 1888                                                        M37.7530
 1550 CALL TIMVAL                                                       M35.6849
      IF ( NEXMOD .GT. 0 ) GO TO 9835                                           
      IF ( KILL  .EQ.  99 )  GO TO 9835                                 M38.4747
      IF ( BUFFIN(1:4) .EQ. 'STOP' ) GO TO 9835                                 
C     IF ( BUFFIN(1:4) .EQ. 'SPY ' ) GO TO 9835 ! ABORT "PLOT" USE      M38.4749
      GO TO 1053                                                        M35.6853
 9800 IF ( IPRSPY .LT. 1 ) GO TO 9817                                           
      WRITE (MUNIT6, 9807)   PROM80(1:40)                               M38.4751
 9807 FORMAT (  ' EXIT "PLTVAR".   PROM80(1:40) =',  A40  )             M38.4752
      CALL WINDOW                                                               
 9817 NEXMOD = 5                                                                
 9835 RETURN                                                            M38.4755
      END                                                               M35.6854
      SUBROUTINE TIMVAL                                                         
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.6856
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.6857
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      MAXEVK = MAXEV - 30                                               M35.6860
      IF ( IPRSPY .LT. 1 ) GO TO 1549                                           
      WRITE (MUNIT6, 1386)  MONITR, LIMFIX, VMIN, VMAX                  M38.4757
 1386 FORMAT (  ' Begin  "TIMVAL".  MONITR, LIMFIX =',  2I6,            M38.4758
     1          '    VMIN, VMAX =',  2E13.4  )                          M38.4759
      CALL WINDOW                                                               
      N16 = JPLT                                                        M37.7535
      IF ( N16 .GT. 20 )  N16 = 20                                      M37.7536
      WRITE (MUNIT6, 3274) ( MPLOT(I), I=1, N16 )                       M37.7537
 3274 FORMAT ( ' MPLOT:',  20I6 )                                       M37.7538
      CALL WINDOW                                                               
 1549 IF ( NEXMOD .NE. 6 ) GO TO 31549                                          
      NEXMOD = 0                                                                
      GO TO NEXTSN                                                              
31549 IF ( MONITR .EQ. 8765 ) GO TO 1926                                        
 1550 WRITE(PROM80,1930)                                                M35.6865
 1930 FORMAT (  ' --- INNER :'  )                                       M35.6866
      ASSIGN 1556 TO NEXTSN                                                     
      GO TO 9800                                                                
 1556 BUFFIN = BUFF77                                                           
C     IF ( BUFFIN(1:4) .EQ. 'SPY ' ) GO TO 9835   ! EXIT MODULE         M38.4767
      IF ( BUFFIN(1:8)  .NE.  SETDAT )   GO TO 2713                     M35.6871
      CALL SETRTM                                                       M35.6872
      GO TO 1550                                                        M35.6873
 2713 IF ( BUFFIN(1:8)  .NE.  'ROLLV   '     )   GO TO 2715             M36.3441
      MONITR = MONITR + 1                                               M35.6874
      IF ( MONITR  .GE.  2 )   MONITR = 0                               M35.6875
      IF ( MONITR .NE. 0 )  WRITE (MUNIT6, 42713)                       M38.4768
42713 FORMAT (  '            ===  Begin  ROLLing  of',                  M38.4769
     1          ' vector-graphic  PLOT.'  )                             M38.4770
      IF ( MONITR .EQ. 0 )  WRITE (MUNIT6, 2714)                        M38.4771
 2714 FORMAT (  '            ===  Previous  ROLLing  of',               M37.7542
     1          ' vector plot is hereby cancelled.  ==='  )             M37.7543
      CALL WINDOW                                                               
      GO TO 1550                                                        M35.6876
 2715 IF ( BUFFIN(1:8) .NE. 'ROLLC   ' )  GO TO 42715                   M36.3445
      MONITS = MONITS + 1                                               M36.3446
      IF ( MONITS .GE. 2 )  MONITS = 0                                  M36.3447
      IF ( MONITS .NE. 0 )  GO TO 1550                                          
      WRITE (MUNIT6, 22714)                                             M37.7546
22714 FORMAT (  '            ===  Previous  ROLLing  of',               M37.7547
     1          ' character plot is hereby cancelled.  ==='  )          M37.7548
      CALL WINDOW                                                               
      GO TO 1550                                                                
42715 IF ( BUFFIN(1:8)  .NE.  'SPAN    '     )   GO TO 2718             M36.3452
      WRITE (PROM80, 2716)                                              M35.6877
 2716 FORMAT ( ' SEND DESIRED TMAX-TMIN:'  )                            M35.6878
      ASSIGN 52716 TO NEXTSN                                                    
      GO TO 9800                                                                
52716 CALL FRESP1 ( BUFF77, D3 )                                                
 2717 N4 = (INDBUF - NEWVEC ) / (KPTPLT + 1 )                           M35.6880
      IF ( IPRSPY .LT. 1 ) GO TO 82717                                          
      WRITE (MUNIT6, 42717)  INDBUF, NEWVEC, KPTPLT, N4                 M37.7551
42717 FORMAT (   ' Timespan calculation.',                              M37.7552
     1            '  INDBUF, NEWVEC, KPTPLT, N4=',   4I8  )             M36.3458
      CALL WINDOW                                                               
82717 N4 = NEWVEC + N4 * (KPTPLT + 1)                                   M37.7554
      DIN2 = PLTBUF(N4)                                                         
      DIN1 = DIN2 - D3                                                          
      GO TO 1927                                                                
 2718 IF ( BUFFIN(1:8)  .NE.  'FRONT   '    )   GO TO 2720              M35.6885
      D3 = SPSAVE                                                               
      GO TO 2717                                                                
 2720 IF ( BUFFIN(1:8)  .NE.  XYPLOT )   GO TO 2728                     M35.6888
      MXYPL = MXYPL + 1                                                 M35.6889
      IF ( MXYPL  .GE.  2 )   MXYPL = 0                                 M35.6890
      IF ( MXYPL  .EQ.  0 )   GO TO 2724                                M35.6891
      WRITE (PROM80, 2721)  XYTITL(1:16)                                M37.7555
 2721 FORMAT (  ' SEND X-AXIS LABEL (',  A16,  '...) :'  )              M35.6893
      ASSIGN 42721 TO NEXTSN                                                    
      GO TO 9800                                                                
42721 BUFFIN = BUFF77                                                           
      IF ( BUFFIN(1:8)  .EQ.  '        ' )   GO TO 2724                 M35.6895
      IF ( BUFFIN(1:8)  .EQ.  TEXBLK )   GO TO 2722                     M35.6896
      XYTITL(1:24) = BUFFIN(1:24)                                       M35.6897
      GO TO 2724                                                        M35.6898
 2722 XYTITL = '                        '                               M35.6899
 2724 GO TO 1550                                                        M35.6900
 2728 IF ( BUFFIN(1:8)  .NE.  CURSOR )   GO TO 2733                     M35.6901
      ICURSE = ICURSE + 1                                               M35.6902
      IF ( ICURSE  .EQ.  2 )   ICURSE = 0                               M35.6903
      GO TO 1550                                                        M35.6904
 2733 IF ( BUFFIN(1:8)  .NE.  DEBUG )   GO TO 2742                      M35.6905
      WRITE (PROM80, 2738)  IPRSPY                                      M36.3459
 2738 FORMAT (  '  SUPPLY LEVEL-NUMBER  IPRSPY  (',                     M35.6906
     1  I3,  ' ) :'  )                                                  M35.6907
      ASSIGN 2740 TO NEXTSN                                                     
      GO TO 9800                                                                
 2740 CALL FREIN1 ( BUFF77, IPRSPY )                                            
      GO TO 1550                                                        M35.6908
 2742 IF ( BUFFIN(1:8)  .NE.  EXTREM )   GO TO 4774                     M38.4781
      KEXTR = KEXTR + 1                                                 M35.6940
      IF ( KEXTR  .GT.  1 )   KEXTR = 0                                 M35.6941
      GO TO 1550                                                        M35.6942
 4774 IF ( BUFFIN(1:8)  .NE.  LEVEL )   GO TO 4781                      M35.6943
      KLEVL = KLEVL + 1                                                 M35.6944
      IF ( KLEVL  .GT.  1 )   KLEVL = 0                                 M35.6945
      IF ( KLEVL  .EQ.  0 )   GO TO 4777                                M35.6946
      WRITE (MUNIT6, 4776)  ( YLEVEL(J), J=1, JPLT )                    M35.6947
 4776 FORMAT (  '   INPUT VECTOR OF LEVELS :'  )                        M37.7561
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3284)  ( YLEVEL(J), J=1, JPLT )                    M37.7563
 3284 FORMAT ( 1X, 7E10.2 )                                             M37.7564
      CALL WINDOW                                                               
      IF ( LTEK .LE. 0 )  GO TO 34776                                           
      WRITE (MUNIT6, 3303)                                                      
      CALL WINDOW                                                               
34776 READ (MUNIT5, *)  ( YLEVEL(J), J=1, JPLT )                        M37.7569
 4777 GO TO 1550                                                        M35.6951
 4781 IF ( BUFFIN(1:8)  .NE.  STOP )   GO TO 4784                       M35.6952
      KILL = 99                                                         M35.6953
      GO TO 9835                                                                
 4784 IF ( BUFFIN(1:8)  .NE.  SMOOTH )   GO TO 4805                     M35.6955
      WRITE (PROM80, 4791)  TOLRCE                                      M36.3464
 4791 FORMAT (  ' SUPPLY NEW TOLERANCE IN',                             M35.6956
     1  ' INCHES**2 (',  E9.2,  ' ) :'  )                               M35.6957
      ASSIGN 4794 TO NEXTSN                                                     
      GO TO 9800                                                                
 4794 CALL FRESP1 ( BUFF77, TOLRCE )                                            
      GO TO 1550                                                        M35.6958
 4805 IF ( BUFFIN(1:8)  .NE.  SIZE )   GO TO 4823                       M35.6959
      WRITE (PROM80, 4808)  TAXISL                                      M36.3468
 4808 FORMAT (  ' SUPPLY NEW TIME-AXIS LENGTH',                         M35.6960
     1   ' IN INCHES (',  F5.1,  ' ) :'  )                              M35.6961
      ASSIGN 4811 TO NEXTSN                                                     
      GO TO 9800                                                                
 4811 CALL FRESP1 ( BUFF77, D1 )                                                
      IF ( D1  .GT.  2.0 )   TAXISL = D1                                M35.6962
      GO TO 1550                                                        M35.6963
 4823 IF ( BUFFIN(1:8)  .NE.  SHOW )   GO TO 4841                       M35.6964
      N1 = LIMCOL + 1                                                   M35.6965
      WRITE (MUNIT6, 4831)                                              M37.7570
 4831 FORMAT (  ' Parameter settings follow ....'  )                    M37.7571
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2291)  N1                                          M37.7573
 2291 FORMAT (  I15,  ' = Number of columns in character plot' )        M37.7574
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2293)  IPRSPY                                      M37.7576
 2293 FORMAT (  I15,  ' = Diagnostic (DEBUG) printout level' )          M37.7577
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2295)  TAXISL                                      M37.7579
 2295 FORMAT (  E15.3,  ' = Length of time axis in inches' )            M37.7580
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2297)  TOLRCE                                      M37.7582
 2297 FORMAT (  E15.3,  ' = smoothing tolerance in inches' )            M37.7583
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2299)  LINEPR                                      M37.7585
 2299 FORMAT ( I15, ' = Logical unit number of line printer.' )         M37.7586
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2303)  LU7PLT                                      M37.7588
 2303 FORMAT ( I15,  ' = Logical unit number of current output' )       M37.7589
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2305)  LTEK                                        M37.7591
 2305 FORMAT ( I15,  ' = LTEK (1 if vector plot, 0 otherwise)' )        M37.7592
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2307)  IHS                                         M37.7594
 2307 FORMAT ( I15,  ' = Time units code IHS'   )                       M37.7595
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2309)  IBAUD                                       M37.7597
 2309 FORMAT ( I15,  ' = IBAUD (Tektronix PLOT10 characters/sec)' )     M37.7598
      CALL WINDOW                                                               
      WRITE (MUNIT6, 2311)  NXMAX                                       M37.7600
 2311 FORMAT ( I15,  ' = NXMAX (vector points horizontally)' )          M37.7601
      CALL WINDOW                                                               
      GO TO 1550                                                        M35.6980
 4841 IF ( BUFFIN(1:5) .NE. HELP(1:5) ) GO TO 1990                      M38.4789
      CALL HELPER ( 3 )                                                 M35.6988
      GO TO 1550                                                        M35.6989
 1990 IF ( IPRSPY .LT. 1 )  GO TO 8992                                          
      WRITE (MUNIT6, 8991)  KPTPLT, NUMNVO, NUMBRN                      M37.7606
 8991 FORMAT (  ' KPTPLT, NUMNVO, NUMBRN below 1990 =', 3I8 )           M37.7607
      CALL WINDOW                                                               
 8992 IF ( BUFFIN(1:8)  .NE.  OUT  )GO TO 1921                          M37.7609
      GO TO 9835                                                                
 1921 IF ( BUFFIN(1:8)  .NE.  MULTIP )   GO TO 1960                     M35.6995
      WRITE (MUNIT6, 1954)                                              M37.7610
 1954 FORMAT (  '   INPUT VECTOR OF MULTIPLIERS :'  )                   M37.7611
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3317)  ( AAA(J), J=1, JPLT )                       M37.7613
 3317 FORMAT ( 1X,  7E10.2 )                                            M37.7614
      CALL WINDOW                                                               
      IF ( LTEK .LE. 0 )  GO TO 1957                                            
      WRITE (MUNIT6, 1956)                                              M37.7617
 1956 FORMAT ( 1X )                                                     M35.7000
      CALL WINDOW                                                               
 1957 ASSIGN 1958 TO NEXTSN                                                     
      GO TO 9800                                                                
 1958 READ (BUFF77, 1959)  ( AAA(J), J=1, JPLT )                        M38.4793
 1959 FORMAT ( 10E8.0 )                                                 M38.4794
      GO TO 1550                                                        M35.7002
 1960 IF ( BUFFIN(1:8)  .NE.  OFFSET )   GO TO 1970                     M35.7003
      WRITE (MUNIT6, 1964)                                              M37.7620
 1964 FORMAT (  '   INPUT VECTOR OF OFFSETS :'  )                       M37.7621
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3317)  ( BBB(J), J=1, JPLT )                       M37.7623
      CALL WINDOW                                                               
      IF ( LTEK .LE. 0 )  GO TO 1969                                            
      WRITE (MUNIT6, 1956)                                                      
      CALL WINDOW                                                               
 1969 ASSIGN 31969 TO NEXTSN                                                    
      GO TO 9800                                                                
31969 READ (BUFF77, 1959)  ( BBB(J), J=1, JPLT )                        M38.4797
      GO TO 1550                                                        M35.7009
 1970 IF ( BUFFIN(1:8)  .NE.  LIMITS )   GO TO 1980                     M35.7010
      WRITE (PROM80, 1973)  VMIN, VMAX                                  M35.7011
 1973 FORMAT (  '   VERTICAL MIN & MAX (',                              M35.7012
     1  2E12.3,  ' ) :'   )                                             M35.7013
      ASSIGN 1975 TO NEXTSN                                                     
      GO TO 9800                                                                
 1975 CALL FRESP2 ( BUFF77, VMIN, VMAX )                                        
      LIMFIX = 1                                                        M35.7014
      IF ( VMIN  .EQ.  0.0     .AND.                                    M35.7015
     1     VMAX  .EQ.  0.0 )   LIMFIX = 0                               M35.7016
      GO TO 1550                                                        M35.7017
 1980 IF ( BUFFIN(1:8)  .NE.  AVERAG )   GO TO 1989                     M35.7018
      WRITE (PROM80, 1982)  NSMPLT                                      M35.7019
 1982 FORMAT (  '   SUPPLY CONSECUTIVE-OSCILLATION LIMIT (',            M35.7020
     1   I6,    ' ) :'   )                                              M35.7021
      ASSIGN 1985 TO NEXTSN                                                     
      GO TO 9800                                                                
 1985 CALL FREIN1 ( BUFF77, N1 )                                                
      IF ( N1  .GT.  0 )   NSMPLT = N1                                  M35.7023
      GO TO 1550                                                        M35.7024
 1989 IF ( BUFFIN(1:8)  .NE.  RESCAL )   GO TO 1992                     M35.7025
      LIMFIX = 0                                                        M35.7026
      DO 1991  J=1, 20                                                  M35.7027
      AAA(J) = 1.0                                                      M35.7028
 1991 BBB(J) = 0.0                                                      M35.7029
      GO TO 1550                                                        M35.7030
 1992 IF ( BUFFIN(1:8)  .NE.  TIME )   GO TO 5618                       M35.7031
      DIN1 = HMIN * TMULT                                               M35.7032
      DIN2 = HMAX * TMULT                                               M35.7033
      WRITE (PROM80, 1925)  DIN1, DIN2                                  M35.7034
 1925 FORMAT (  '   SUPPLY TMIN & TMAX (',                              M35.7035
     1  2E12.3,  ' ) :'   )                                             M35.7036
      ASSIGN 31925 TO NEXTSN                                                    
      GO TO 9800                                                                
31925 CALL FRESP2 ( BUFF77, DIN1, DIN2 )                                        
      IF ( DIN1  .NE.  0.0 )   GO TO 1927                               M35.7038
      IF ( DIN2  .NE.  0.0 )   GO TO 1927                               M35.7039
      GO TO 1928                                                        M35.7040
C     AUTO INTERNAL "TIME", CALLED FROM "PLTFIL" IF ROLLING PLOT NEEDS  M38.4807
C     A NEW PAGE:                                                       M38.4808
 1926 D6 = DIN2 - DIN1                                                          
      D7 = INCHLP                                                               
      DIN1 = DIN2 - (D7/100.) * D6                                              
      DIN2 = DIN1 + D6                                                          
      IF ( IPRSPY .LE. 0 )  GO TO 1927                                          
      WRITE (MUNIT6, 41927)  LIMFIX, INCHLP, VMIN, VMAX, DIN1, DIN2     M38.4814
41927 FORMAT ( ' "TIMVAL", S.N. 1926.  LIMFIX, INCHLP, VMIN,',          M38.4815
     1         ' VMAX, DIN1, DIN2 =', 2I8,  4E13.4  )                   M38.4816
      CALL WINDOW                                                       M38.4817
 1927 HMIN = DIN1                                                       M35.7041
      HMAX = DIN2                                                       M35.7042
      SPSAVE = DIN2 - DIN1                                              M35.7043
      HMIN=HMIN/TMULT                                                   M35.7044
      HMAX=HMAX/TMULT                                                   M35.7045
      HPI = (HMAX - HMIN ) / TAXISL                                     M35.7046
 1928 IF ( IPRSPY  .LT.  1 )  GO TO 1942                                        
      WRITE (MUNIT6, 1934)  KPLT, JPLT                                  M37.7630
 1934 FORMAT (  ' B4 read points.  KPLT, JPLT =',  2I8 )                M37.7631
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3324)  HMIN, HMAX, HPI, TAXISL, TMULT              M37.7633
 3324 FORMAT (  '         HMIN, HMAX, HPI, TAXISL, TMULT =',            M37.7634
     1          5E15.6  )                                               M37.7635
      CALL WINDOW                                                               
 1942 IF ( MONITR .NE. 2345 )  GO TO 5648                                       
      GO TO 1550                                                                
 5618 IF ( BUFFIN(1:8)  .NE.  '        '     .AND.                      M35.7054
     1     BUFFIN(1:8)  .NE.  INNER )   GO TO 4431                      M35.7055
      IF ( HMAX  .GT.  HMIN )   GO TO 5648                              M35.7056
      GO TO 4437                                                        M35.7057
 4431 IF ( BUFFIN(1:8)  .NE.  ALLTIM )   GO TO 5623                     M35.7058
 4437 HMIN = TIMBEG                                                     M35.7059
      HMAX = TIMEND                                                     M35.7060
      HPI = ( HMAX - HMIN ) / TAXISL                                    M35.7061
      GO TO 5648                                                        M35.7062
 5623 WRITE (MUNIT6, 5624)                                              M35.7063
 5624 FORMAT (  ' ??? Unrecognizable data.  Try again ....'  )          M37.7637
      CALL WINDOW                                                               
      GO TO 1550                                                        M35.7065
 5648 KPLT = 0                                                          M35.7066
      IF ( IPRSPY .LT. 1 ) GO TO 75648                                          
      WRITE (MUNIT6, 35648)   MPLOT                                     M37.7640
35648 FORMAT ( ' Begin points extraction.  MPLOT =', 20I4 )             M37.7641
      CALL WINDOW                                                               
75648 IF ( LIMFIX  .EQ.  1 )   GO TO 5647                               M37.7643
      VMIN=-FINFIN                                                      M35.7069
      VMAX=FINFIN                                                       M35.7070
 5647 VMAXR=VMIN+(VMAX-VMIN)*(VH-VS)/(VL-VS)                            M35.7071
      VMINR=VMIN-(VMAX-VMIN)*VS/(VL-VS)                                 M35.7072
      MFAKE = 0                                                         M35.7073
      DO 5645  J=1, JPLT                                                M35.7074
      MMM(J) = 1                                                        M35.7075
      IF ( AAA(J)  .EQ.  0.0 )   GO TO 5646                             M35.7076
      IF ( AAA(J)  .EQ.  1.0 )   GO TO 5646                             M35.7077
      GO TO 5645                                                        M35.7078
 5646 IF ( BBB(J)  .NE.  0.0 )   GO TO 5645                             M35.7079
      MMM(J) = 0                                                        M35.7080
      MFAKE = MFAKE + 1                                                 M35.7081
 5645 CONTINUE                                                          M35.7082
 5649 INDEXP = NEWVEC - (KPTPLT + 1)                                    M35.7083
      IF ( IPRSPY .LT. 1 ) GO TO 1940                                           
      WRITE (MUNIT6, 45649)   NEWVEC, KPTPLT                            M37.7645
45649 FORMAT ( ' Reinitialize pointer:  NEWVEC, KPTPLT =', 2I9 )        M37.7646
      CALL WINDOW                                                               
 1940 INDEXP = INDEXP + KPTPLT + 1                                      M35.7085
      IF ( IPRSPY .LT. 3 ) GO TO 81940                                          
      WRITE (MUNIT6, 61940)   INDEXP                                    M37.7649
61940 FORMAT ( ' INDEXP at the time test =',  I9 )                      M37.7650
      CALL WINDOW                                                               
81940 IF ( INDEXP  .GE.  INDBUF )   GO TO 2200                          M37.7652
      TSTEP = PLTBUF(INDEXP)                                            M35.7088
      IF (TSTEP.GE.HMIN) GO TO 2060                                     M35.7089
      GO TO 1940                                                        M35.7091
 2060 IF (TSTEP.GT.HMAX) GO TO 2205                                     M35.7092
      IF ( INDEXP  .GT.  INDBUF )   GO TO 2200                          M35.7093
      IF (KPLT.LE.MAXEVK) GO TO 2100                                    M35.7094
      WRITE (MUNIT6, 2068)  TSTEP                                       M35.7095
 2068 FORMAT ( 2X,  17HPlot truncated to,  E11.3,                       M37.7653
     1  32H seconds due to filled storage.  )                           M37.7654
      CALL WINDOW                                                               
      GO TO 2205                                                        M35.7098
 2100 KPLT=KPLT+1                                                       M35.7099
      EV(KPLT)=TSTEP                                                    M35.7100
      DO 2140 J=1,JPLT                                                  M35.7101
      J1=MPLOT(J)                                                       M35.7102
      IF (J1.GE.0) GO TO 2120                                           M35.7103
      J1=-J1                                                            M35.7104
      BX(J1)=-BX(J1)                                                    M35.7105
 2120 IF ( MXYPL  .EQ.  0 )   GO TO 2126                                M35.7106
      IF ( J  .EQ.  1 )   GO TO 2127                                    M35.7107
 2126 KPLT=KPLT+1                                                       M35.7108
 2127 EV(KPLT) = PLTBUF(INDEXP+J1)                                      M35.7109
      IF ( MPLOT(J)  .LT.  0 )   EV(KPLT) = -EV(KPLT)                   M35.7110
      IF ( MMM(J)  .EQ.  0 )   GO TO 2140                               M35.7111
      IF ( AAA(J)  .NE.  0.0 )                                          M35.7112
     1 EV(KPLT) = EV(KPLT) * AAA(J)                                     M35.7113
      EV(KPLT) = EV(KPLT) + BBB(J)                                      M35.7114
 2140 CONTINUE                                                          M35.7115
      GO TO 1940                                                        M35.7116
 2200 TSTEP = 1.E+30                                                    M35.7117
 2205 IF ( IPRSPY .LT. 4 ) GO TO 3342                                           
      DO 3338  J=1, KPLT, 8                                                     
      WRITE (MUNIT6, 3336) J, ( EV(IP), IP=J, J+7 )                             
 3336 FORMAT ( ' EV(', I5,  3H:)=,  8E15.6  )                           M37.7659
      CALL WINDOW                                                               
 3338 CONTINUE                                                          M37.7661
 3342 IND1 = INDEXP - ( KPTPLT + 1 )                                    M37.7662
      IF ( IPRSPY .LT. 1 )  GO TO 3343                                          
      WRITE (MUNIT6, 82205)  IND1                                       M37.7664
82205 FORMAT ( ' Another IND1 change.  IND1 =',  I8  )                  M37.7665
      CALL WINDOW                                                               
 3343 IF ( MXYPL  .EQ.  0 )   GO TO 2218                                M37.7667
      K = 0                                                             M35.7124
      N7 = 0                                                            M35.7125
      N8 = 2 * KPLT / JPLT  +  4                                        M35.7126
      GXMIN = FINFIN                                                    M35.7127
      GYMIN = FINFIN                                                    M35.7128
      GXMAX = -GXMIN                                                    M35.7129
      GYMAX = -GYMIN                                                    M35.7130
 8303 IF ( K  .GE.  KPLT )   GO TO 8317                                 M35.7131
      KK = N7                                                           M35.7132
      DO 8310  J=1, JPLT, 2                                             M35.7133
      IF ( EV(K+1)  .LT.  GXMIN )   GXMIN = EV(K+1)                     M35.7134
      IF ( EV(K+1)  .GT.  GXMAX )   GXMAX = EV(K+1)                     M35.7135
      IF ( EV(K+2)  .LT.  GYMIN )   GYMIN = EV(K+2)                     M35.7136
      IF ( EV(K+2)  .GT.  GYMAX )   GYMAX = EV(K+2)                     M35.7137
      EW(KK+1) = EV(K+1)                                                M35.7138
      EW(KK+2) = EV(K+2)                                                M35.7139
      K = K + 2                                                         M35.7140
 8310 KK = KK + N8                                                      M35.7141
      N7 = N7 + 2                                                       M35.7142
      GO TO 8303                                                        M35.7143
 8317 WRITE (MUNIT6, 8322)  GXMIN, GXMAX, GYMIN, GYMAX                  M35.7144
 8322 FORMAT (  '  X-MIN, Y-MIN =',  2E12.4,                            M37.7668
     1          '  Y-MIN, Y-MAX =', 2E12.4  )                           M37.7669
      CALL WINDOW                                                               
      WRITE (PROM80, 3356)                                              M37.7671
 3356 FORMAT (  ' SEND AXIS LIMITS :' )                                 M37.7672
      ASSIGN 43356 TO NEXTSN                                                    
      GO TO 9800                                                                
43356 READ (BUFF77, 1959)  D1, D2, D3, D4                               M38.4823
      IF ( D1  .NE.  0.0 )   GXMIN = D1                                 M35.7149
      IF ( D2  .NE.  0.0 )   GXMAX = D2                                 M35.7150
      IF ( D3  .NE.  0.0 )   GYMIN = D3                                 M35.7151
      IF ( D4  .NE.  0.0 )   GYMAX = D4                                 M35.7152
      VMIN = GYMIN                                                      M35.7153
      VMAX = GYMAX                                                      M35.7154
      MFAKE = MFAKE / 2                                                 M35.7155
      L = 0                                                             M35.7156
      KK = N8                                                           M35.7157
 8371 DO 8376  J=1, JPLT, 2                                             M35.7158
      L = L + 1                                                         M35.7159
      MSTART(L) = KK - 4                                                M35.7160
      EW(KK-3) = GXMIN                                                  M35.7161
      EW(KK-2) = GYMIN                                                  M35.7162
      EW(KK-1) = ( GXMAX - GXMIN ) / TAXISL                             M35.7163
      EW(KK)   = ( GYMAX - GYMIN ) / 8.0                                M35.7164
 8376 KK = KK + N8                                                      M35.7165
      KK = KK - N8                                                      M35.7166
      JPLT = L                                                          M35.7167
      IF ( IPRSPY .LT. 1 )  GO TO 8382                                          
      WRITE (MUNIT6, 8381)  JPLT,  ( MSTART(J), J=1, JPLT )             M37.7675
 8381 FORMAT (  ' Done with X-Y pack.   JPLT =',  I3,                   M37.7676
     1  5X,  'MSTART(J) :',  5I5  )                                     M35.7171
      CALL WINDOW                                                               
 8382 IF ( IPRSPY .LT. 9 ) GO TO 7309                                           
      DO 3363  J=1, KK, 8                                               M37.7679
      WRITE (MUNIT6, 3361) J, ( EW(IP), IP=J, J+7 )                             
 3361 FORMAT ( ' EW(', I5,  3H:)=,  8E15.6  )                           M37.7681
      CALL WINDOW                                                               
 3363 CONTINUE                                                          M37.7683
      GO TO 7309                                                        M37.7684
 2218 DX = HPI                                                          M35.7176
      EVMX=0.0                                                          M35.7177
      K9=KPLT+JPLT1+JPLT1                                               M35.7178
      DO 2340 J=1,JPLT                                                  M35.7179
      EVH=0.0                                                           M35.7180
      EVDH=0.0                                                          M35.7181
      ISW=0                                                             M35.7182
      ITIMES = 0                                                        M35.7183
      DO 2320 I=1,KPLT,JPLT1                                            M35.7184
      IPJ=I+J                                                           M35.7185
      EVP=EV(IPJ)                                                       M35.7186
      IF (ISW.GT.0) GO TO 2240                                          M35.7187
      EVDP=EVP-EVH                                                      M35.7188
      IF (EVDP*EVDH.GE.0.0) GO TO 2250                                  M35.7189
      IF (ISWW.EQ.1) GO TO 2255                                         M35.7190
      ITIMES=ITIMES+1                                                   M35.7191
      IF ( ITIMES .LE. NSMPLT )   GO TO 2260                            M35.7192
      ISW=1                                                             M35.7193
 2240 EV(IPJ)=(EVH+EVP)/2.0                                             M35.7194
      GO TO 2260                                                        M35.7195
 2250 ISWW=1                                                            M35.7196
      GO TO 2260                                                        M35.7197
 2255 ITIMES=1                                                          M35.7198
      ISWW=0                                                            M35.7199
 2260 EVDH=EVDP                                                         M35.7200
      EVH=EVP                                                           M35.7201
      EVMX=AMAX1(EVMX,ABS(EV(IPJ)))                                     M35.7202
      IF (EV(IPJ).LT.VMINR) GO TO 2280                                  M35.7203
      IF (EV(IPJ).GT.VMAXR) GO TO 2300                                  M35.7204
      GO TO 2320                                                        M35.7205
 2280 EV(IPJ)=VMINR                                                     M35.7206
      GO TO 2320                                                        M35.7207
 2300 EV(IPJ)=VMAXR                                                     M35.7208
 2320 CONTINUE                                                          M35.7209
 2340 CONTINUE                                                          M35.7210
      IF ( IPRSPY .LT. 4 ) GO TO 3369                                           
      WRITE (MUNIT6, 2344)                                              M37.7686
 2344 FORMAT (  ' After points discard.',                               M37.7687
     1  11X,  4HEVMX,  11X,  4HVMAX,  11X,  4HVMIN,                     M35.7214
     2  9X,  6HFINFIN,  13X,  2HVL,   13X,  2HVS   )                    M37.7688
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3366)  EVMX, VMAX, VMIN, FINFIN, VL, VS            M37.7690
 3366 FORMAT ( 22X,  6E15.5  )                                          M37.7691
      CALL WINDOW                                                               
 3369 IF ( LIMFIX  .EQ.  1 )   GO TO 2540                               M37.7693
      IF (EVMX.EQ.0.0) EVMX=1.0                                         M35.7218
 2357 A=8.0                                                             M35.7219
      EVMXF=EVMX*0.8                                                    M35.7220
 2360 D1=EVMXF-A                                                        M35.7221
      IF (D1.EQ.0.0) GO TO 2400                                         M35.7222
      IF (D1.GT.0.0) GO TO 2380                                         M35.7223
      A=A/10.                                                           M35.7224
      GO TO 2360                                                        M35.7225
 2380 A=A*10.                                                           M35.7226
      D1=EVMXF-A                                                        M35.7227
      IF (D1.EQ.0.0) GO TO 2400                                         M35.7228
      IF (D1.GT.0.0) GO TO 2380                                         M35.7229
      D1=EVMXF-0.5*A                                                    M35.7230
      IF (D1.EQ.0.0) GO TO 2440                                         M35.7231
      IF (D1.LT.0.0) GO TO 2420                                         M35.7232
 2400 VMAX=A                                                            M35.7233
      GO TO 2520                                                        M35.7234
 2420 D1=EVMXF-.25*A                                                    M35.7235
      IF (D1.LT.0.0) GO TO 2460                                         M35.7236
      IF (D1.EQ.0.0) GO TO 2480                                         M35.7237
 2440 VMAX=A/2.0                                                        M35.7238
      GO TO 2520                                                        M35.7239
 2460 IF (EVMXF-A/8.0.LE.0.0) GO TO 2500                                M35.7240
 2480 VMAX=A/4.0                                                        M35.7241
      GO TO 2520                                                        M35.7242
 2500 VMAX=A/8.0                                                        M35.7243
 2520 VMIN=-VMAX                                                        M35.7244
      IF ( VMAX  .GE.  0.99*EVMX )   GO TO 2540                         M35.7245
      EVMX = EVMX * 1.1                                                 M35.7246
      GO TO 2357                                                        M35.7247
 2540 DY=(VMAX-VMIN)/(VL-VS)                                            M35.7248
      IF ( KPLT  .GT.  0 )   GO TO 2543                                 M35.7249
      WRITE (MUNIT6, 2542)  HMIN, HMAX                                  M35.7250
 2542 FORMAT (  '   ** Sorry.  No data available between times',        M37.7694
     1  2E13.4,  'sec.'  )                                              M37.7695
      CALL WINDOW                                                               
      GO TO 1550                                                        M35.7253
 2543 IPL=0                                                             M35.7254
      KPLTQ=KPLT/JPLT1                                                  M35.7255
      ISTORE = 0                                                        M35.7256
      IF ( IPRSPY  .LT. 4 )  GO TO 3382                                         
      WRITE (MUNIT6, 2548)                                              M37.7698
 2548 FORMAT ( ' Dump cells 1-20 of EV at S.N. 2548 . . . .' )          M37.7699
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3374)  ( EV(I), I=1,  10 )                         M37.7701
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3374)  ( EV(I), I=11, 20 )                         M37.7703
 3374 FORMAT ( 1X, 10E13.4 )                                            M37.7704
      CALL WINDOW                                                               
 3382 DO 2567  I=1, JPLT                                                M37.7706
      YYMIN(I) = 1.E30                                                  M35.7262
      YYMAX(I) = -YYMIN(I)                                              M35.7263
      MLEVEL(I) = 0                                                     M35.7264
      IF ( KLEVL  .EQ.  0 )   GO TO 2567                                M35.7265
      MLEVEL(I) = -1                                                    M35.7266
      TTLEV(I) = -9999.                                                 M35.7267
 2567 CONTINUE                                                          M35.7268
      DO 3240 I=1,JPLT                                                  M35.7269
      ISTOLD = ISTORE                                                   M39.3281
      IBASE=1                                                           M35.7271
      NUM=0                                                             M35.7272
      N13 = 1+JPLT1+I                                                   M35.7273
      VOLD = EV(N13)                                                    M35.7274
      ISTORE = ISTORE + 1                                               M35.7275
 3000 EVBASX=EV(IBASE)                                                  M35.7276
      EVBASY=EV(IBASE+I)                                                M35.7277
      EW(ISTORE)=EVBASX                                                 M35.7278
      EW(ISTORE+1)=EVBASY                                               M35.7279
      ISTORE=ISTORE+2                                                   M35.7280
      IF ( ISTORE  .LE.  MAXEW )   GO TO 3004                           M35.7281
      WRITE (MUNIT6, 3001)                                              M37.7707
 3001 FORMAT ( ' Sorry, tables have overflowed during smoothing.' )     M37.7708
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3392)                                              M37.7710
 3392 FORMAT ( ' Either try a larger tolerance (to discard more' )      M37.7711
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3394)                                              M37.7713
 3394 FORMAT ( ' points), or decrease the time span, or expand' )       M37.7714
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3396)  MAXEW                                       M37.7716
 3396 FORMAT ( ' array  EW(',  I6,  ')  in deck "DEKSPY" .'  )          M37.7717
      CALL WINDOW                                                               
      GO TO 1550                                                        M35.7287
 3004 IF ( KEXTR  .EQ.  0 )   GO TO 2949                                M35.7288
      IF ( EVBASY  .LE.  YYMAX(I) )   GO TO 2941                        M35.7289
      YYMAX(I) = EVBASY                                                 M35.7290
      TTMAX(I) = EVBASX                                                 M35.7291
 2941 IF ( EVBASY  .GE.  YYMIN(I) )   GO TO 2949                        M35.7292
      YYMIN(I) = EVBASY                                                 M35.7293
      TTMIN(I) = EVBASX                                                 M35.7294
 2949 IF ( MLEVEL(I)  .EQ.  0 )   GO TO 3010                            M35.7295
      D1 = EVBASY - YLEVEL(I)                                           M35.7296
      IF ( D1*DYOLD(I)  .GT.  0.0 )   GO TO 2956                        M35.7297
      IF ( MLEVEL(I)  .EQ.  -1 )   GO TO 2954                           M35.7298
      D3 = D1 * ( EVBASX - EW(ISTORE-4) )                               M35.7299
      TTLEV(I) = EVBASX  -  D3 / ( D1 - DYOLD(I) )                      M35.7300
      MLEVEL(I) = 0                                                     M35.7301
      GO TO 3010                                                        M35.7302
 2954 MLEVEL(I) = 1                                                     M35.7303
 2956 DYOLD(I) = D1                                                     M35.7304
 3010 HVEC=(EV(IBASE+JPLT1)-EVBASX)/DX                                  M35.7305
      N13 = IBASE+JPLT1+I                                               M35.7306
      VVEC = ( EV(N13) - EVBASY ) / DY                                  M35.7307
      DENOM=HVEC*HVEC+VVEC*VVEC                                         M35.7308
      IPONTR=IBASE+JPLT1                                                M35.7309
 3020 IPONTR=IPONTR+JPLT1                                               M35.7310
      IF (IPONTR.GT.KPLT) GO TO 3140                                    M35.7311
      HDIF=(EV(IPONTR)-EVBASX)/DX                                       M35.7312
      VNEW=EV(IPONTR+I)                                                 M35.7313
      VDIF=(VNEW-EVBASY)/DY                                             M35.7314
      DISQR=HDIF*HDIF+VDIF*VDIF-(HVEC*HDIF+VVEC*VDIF)**2/DENOM          M35.7315
      IF (DISQR.GT.TOLRCE) GO TO 3040                                   M35.7316
      VCHNGE=VNEW-VOLD                                                  M35.7317
      VOLD=VNEW                                                         M35.7318
      IF(VVEC.EQ.0.0.AND.VCHNGE.EQ.0.0)GO TO 3020                       M35.7319
      IF (VVEC*VCHNGE.GT.0.0) GO TO 3020                                M35.7320
 3040 VOLD=VNEW                                                         M35.7321
      IBASE=IPONTR-JPLT1                                                M35.7322
      GO TO 3000                                                        M35.7323
 3140 IPONTR=IPONTR-JPLT1                                               M35.7324
      EW(ISTORE)=EV(IPONTR)                                             M35.7325
      EW(ISTORE+1)=EV(IPONTR+I)                                         M35.7326
      MSTART(I) = ISTORE + 1                                            M35.7327
      ISTORE = ISTORE + 1                                               M35.7328
      EW(ISTORE+1) = HMIN                                               M35.7329
      EW(ISTORE+3) = HPI                                                M35.7330
      EW(ISTORE+2) = VMIN                                               M35.7331
      EW(ISTORE+4) = (VMAX - VMIN ) / 8.0                               M35.7332
      N5 = ISTORE + 4                                                   M35.7333
      IF ( IPRSPY .LT. 2 )  GO TO 3406                                          
      WRITE (MUNIT6, 3176)                                              M37.7720
 3176 FORMAT (  ' Store CalComp scaling.',                              M37.7721
     1  16H       I  ISTORE,  10X,  '(EW(J), J=ISTORE, N5)'  )          M37.7722
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3402)  I, ISTORE, ( EW(KL), KL=ISTORE, N5 )        M37.7724
 3402 FORMAT (  23X,  2I8,  5E15.6  )                                   M37.7725
      CALL WINDOW                                                               
 3406 ISTORE = N5                                                       M37.7727
      IF ( IPRSPY .LT. 1 )  GO TO 3235                                          
      WRITE (MUNIT6,3234)  I, ISTORE                                    M37.7729
 3234 FORMAT (  ' Done smoothing next curve.  I, ISTORE =', 2I8 )       M37.7730
      CALL WINDOW                                                               
 3235 NUMPTS(I) = ( ISTORE - ISTOLD ) / 2                               M39.3282
 3240 CONTINUE                                                          M35.7345
 3263 IF ( KEXTR  .EQ.  1 )   GO TO 3270                                M35.7346
      IF ( KLEVL  .EQ.  1 )   GO TO 3288                                M35.7347
      GO TO 7309                                                        M37.7733
 3270 WRITE (MUNIT6, 3271)                                              M35.7349
 3271 FORMAT (  ' MPLOT  NAME-1  NAME-2',  7X,                          M37.7734
     1  7HMINIMUM,  7X,  7HMAXIMUM,  6X,  8HT OF MIN,                   M35.7351
     2  6X,  8HT OF MAX  )                                              M35.7352
      CALL WINDOW                                                               
      L = 1                                                             M35.7353
      DO 3279  J=1, JPLT                                                M35.7354
      TEXTD1 = SLOT1(L)                                                 M35.7355
      TEXTD2 = '        '                                               M35.7356
      L = L + 1                                                         M35.7357
      IF ( IABS( MPLOT(J) )  .LE.  NUMNVO )   GO TO 3279                M35.7358
      TEXTD2 = SLOT1(L)                                                 M35.7359
      L = L + 1                                                         M35.7360
      WRITE (MUNIT6, 3282)  MPLOT(J), TEXTD1, TEXTD2,                   M37.7736
     1  YYMIN(J), YYMAX(J), TTMIN(J), TTMAX(J)                          M35.7362
 3282 FORMAT ( 1X,  I5,  2( 2X, A6 ),  4E14.5  )                        M35.7363
      CALL WINDOW                                                               
 3279 CONTINUE                                                          M37.7738
      IF ( KLEVL  .EQ.  0 )   GO TO 3296                                M35.7364
 3288 WRITE (MUNIT6, 3294)  ( YLEVEL(J), J=1, JPLT )                    M35.7365
 3294 FORMAT (  ' Levels sought :',  4E15.4  )                          M37.7739
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3295)  ( TTLEV(J), J=1, JPLT )                     M35.7367
 3295 FORMAT (  ' 1ST hit time :',  4E16.6  )                           M37.7741
      CALL WINDOW                                                               
 3296 IF ( LTEK .LE. 0 ) GO TO 43305                                            
      WRITE (MUNIT6, 3303)                                              M37.7744
 3303 FORMAT ( 1H    )                                                  M36.3490
      CALL WINDOW                                                               
43305 READ (MUNIT5, 3305)  TEXTD1                                       M37.7746
 3305 FORMAT ( A8 )                                                     M35.7377
      IF ( TEXTD1  .EQ.  '        ' )   GO TO 7309                      M37.7747
      IF ( TEXTD1  .NE.  LINEZZ )   GO TO 3306                          M35.7379
      MU6STD = LINEPR                                                           
      GO TO 3263                                                        M35.7381
 3306 IF ( TEXTD1  .EQ.  NOPLOT )   GO TO 1550                          M35.7382
 7309 IF ( LTEK  .NE.  0 )   CALL TEKPLT                                M38.4824
      IF ( LTEK  .EQ.  0 )   CALL CHRPLT                                M35.7384
      IF ( MONITR .NE. 8765 )  GO TO 1550                                       
C     CHECK IF SPECIAL "PLTVAR" CALL FOR ROLLING PLOT PAGING:           M38.4826
      IF ( IPRSUP .LE. 0 )  GO TO 7324                                          
      WRITE (MUNIT6, 7318)                                              M38.4828
 7318 FORMAT ( ' DONE WITH "TEKPLT", MONITR = 8765 SO',                 M38.4829
     1         ' EXIT "TIMVAL".'  )                                     M38.4830
      CALL WINDOW                                                       M38.4831
 7324 GO TO 9835                                                                
 9800 IF ( IPRSPY .LT. 1 ) GO TO 9817                                           
      WRITE (MUNIT6, 9807)   PROM80(1:40)                               M38.4834
 9807 FORMAT (  ' EXIT "TIMVAL".   PROM80(1:40) =',  A40  )             M38.4835
      CALL WINDOW                                                               
 9817 NEXMOD = 6                                                                
 9835 RETURN                                                            M38.4838
      END                                                               M35.7386
      SUBROUTINE BACK14                                                         
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.7388
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.7389
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      IF ( IPRSPY .LT. 1 ) GO TO 1483                                           
      WRITE (MUNIT6, 1478)  TSTEP                                       M37.7751
 1478 FORMAT ( ' TOP OF  "BACK14" .   TSTEP =', E15.5 )                 M37.7752
      CALL WINDOW                                                               
 1483 TSTEP = -1.0                                                      M37.7754
      RETURN                                                            M35.7396
      END                                                               M35.7397
      SUBROUTINE SETRTM                                                         
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.7399
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.7400
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      SAVE                                                              M35.7403
      LUNT15 = 15                                                               
      CLOSE ( UNIT=LUNT15 )                                             M35.7404
      OPEN (UNIT=LUNT15, STATUS='OLD', FILE='TPPARAM')                  M39.3283
      IF ( IPRSPY .LT. 1 ) GO TO 2719                                           
      WRITE (MUNIT6, 2718)                                              M37.7757
 2718 FORMAT ( ' AFTER UNIT LUNT15 CONNECTION TPPARAM.' )               M39.3284
      CALL WINDOW                                                               
 2719 N22 = 0                                                                   
      N23 = 0                                                                   
      REWIND LUNT15                                                     M35.7408
      READ (LUNT15, 5100, END=2729)  BUFFIN                             M35.7409
 5100 FORMAT ( A80 )                                                    M35.7410
      REWIND LUNT15                                                     M35.7411
      IF ( BUFFIN(1:8)  .NE.  SETDAT )   GO TO 2732                     M35.7412
      WRITE (PROM80, 2724)                                              M35.7413
 2724 FORMAT (  '  SEND SUBSET NUMBER :'  )                             M35.7414
      CALL PROMPT                                                               
      READ (MUNIT5, 5100)  PROM80                                       M36.3491
      CALL FREIN1 ( PROM80, N1 )                                                
      IF ( N1  .EQ.  0 )   N1 = 1                                       M35.7416
      DO 2728  J=1, 9999                                                M35.7417
      READ (LUNT15, 5151, END=2729)  BUFFIN                             M35.7418
 5151 FORMAT ( A16 )                                                    M35.7419
      IF ( BUFFIN(1:8)  .NE.  SETDAT )   GO TO 2728                     M35.7420
      WRITE (ANSI, 5100)  BUFFIN(9:16)                                  M35.7421
      READ (ANSI, 2726)  N2                                             M35.7422
 2726 FORMAT ( I2 )                                                     M35.7423
      IF ( N2  .EQ.  N1 )   GO TO 2732                                  M35.7424
 2728 CONTINUE                                                          M35.7425
 2729 WRITE (MUNIT6, 2730)  N1                                          M35.7426
 2730 FORMAT (  ' ** ERROR.   SUBSET NUMBER',  I4,                      M35.7427
     1          '  DOES NOT EXIST.   TRY AGAIN.'   )                    M35.7428
      CALL WINDOW                                                               
      GO TO 2751                                                        M35.7429
 2732 READ (LUNT15, 5100)  PROM80                                       M36.3493
      CALL FRESP2 ( PROM80, D6, D1 )                                            
      JJ = D6                                                                   
      IF ( JJ  .EQ.  0 )   GO TO 2741                                   M35.7430
      IF ( JJ .GT. N23 )  N23 = JJ                                              
      IF ( IPRSPY .LT. 5 ) GO TO 2736                                           
      WRITE (MUNIT6, 2735)  JJ, FVCOM(JJ), D1                           M37.7764
 2735 FORMAT (  ' NEXT REAL DATUM.  JJ, FVCOM, D1 =',                   M35.7433
     1          I6,  2E14.5  )                                          M35.7434
      CALL WINDOW                                                               
 2736 FVCOM(JJ) = D1                                                    M37.7766
      GO TO 2732                                                        M35.7436
 2741 READ (LUNT15, 5100)  PROM80                                       M36.3496
      CALL FREIN2 ( PROM80, JJ, N1 )                                            
      IF ( JJ  .EQ.  0 )   GO TO 2745                                   M35.7437
      IF ( JJ .GT. N22 )  N22 = JJ                                              
      IF ( IPRSPY .LT. 5 ) GO TO 2744                                           
      WRITE (MUNIT6, 2743)  JJ, IVCOM(JJ), N1                           M37.7769
 2743 FORMAT ( ' NEXT INTEGER DATUM.  JJ, IVCOM, N1 =',  3I8  )         M35.7440
      CALL WINDOW                                                               
 2744 IVCOM(JJ) = N1                                                    M37.7771
      GO TO 2741                                                        M35.7442
 2745 READ (LUNT15, 2746)  JJ, TEXTD1                                   M35.7443
 2746 FORMAT ( I2, A8 )                                                 M35.7444
      IF ( JJ  .LE.  0 )   GO TO 2751                                   M35.7445
      IF ( IPRSPY .LT. 5 ) GO TO 2748                                           
      WRITE (MUNIT6, 2747)  JJ, ANPLT(JJ), TEXTD1                       M37.7773
 2747 FORMAT (  ' NEXT KEY WORD.  J =',  I3,  1X, A8,  1X, A8  )        M35.7448
      CALL WINDOW                                                               
 2748 ANPLT(JJ) = TEXTD1                                                M37.7775
      GO TO 2745                                                        M35.7450
 2751 IF ( IPRSPY .LT. 3 ) GO TO 2763                                           
      WRITE (MUNIT6, 2753)                                              M37.7777
 2753 FORMAT (  ' FINAL REAL DATA FVCOM THROUGH THE LAST',              M37.7778
     1          ' MODIFIED CELL FOLLOWS ....'  )                        M37.7779
      CALL WINDOW                                                               
      DO 3417  J=1, N23, 6                                                      
      WRITE (MUNIT6, 3413) ( FVCOM(IP), IP=J, J+5 )                             
 3413 FORMAT ( 1X, 6E13.4 )                                             M37.7783
      CALL WINDOW                                                               
 3417 CONTINUE                                                          M37.7785
      WRITE (MUNIT6, 2754)                                              M37.7786
 2754 FORMAT ( ' FINAL INTEGER DATA IVCOM THROUGH THE LAST',            M37.7787
     1         ' MODIFIED CELL FOLLOWS ....' )                          M37.7788
      CALL WINDOW                                                               
      DO 3427  J=1, N22, 10                                                     
      WRITE (MUNIT6, 3424) ( IVCOM(IP), IP=J, J+9 )                             
 3424 FORMAT ( 1X, 10I7 )                                               M37.7792
      CALL WINDOW                                                               
 3427 CONTINUE                                                          M37.7794
 2763 CLOSE ( UNIT=LUNT15, STATUS='KEEP' )                              M37.7795
      RETURN                                                            M35.7460
      END                                                               M35.7461
      SUBROUTINE CHRPLT                                                         
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.7463
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.7464
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      DIMENSION   TEMP(6)                                               M35.7467
      CHARACTER*9  KUNIT6                                               M35.7468
      CHARACTER*1   DOL(131), LETTER(20)                                M36.3498
      SAVE                                                              M35.7469
      DATA  LETTER(1)   /  'A'  /                                       M36.3499
      DATA  LETTER(2)   /  'B'  /                                       M36.3500
      DATA  LETTER(3)   /  'C'  /                                       M36.3501
      DATA  LETTER(4)   /  'D'  /                                       M36.3502
      DATA  LETTER(5)   /  'E'  /                                       M36.3503
      DATA  LETTER(6)   /  'F'  /                                       M36.3504
      DATA  LETTER(7)   /  'G'  /                                       M36.3505
      DATA  LETTER(8)   /  'H'  /                                       M36.3506
      DATA  LETTER(9)   /  'I'  /                                       M36.3507
      DATA  LETTER(10)  /  'J'  /                                       M36.3508
      DATA  LETTER(11)  /  'K'  /                                       M36.3509
      DATA  LETTER(12)  /  'L'  /                                       M36.3510
      DATA  LETTER(13)  /  'M'  /                                       M36.3511
      DATA  LETTER(14)  /  'N'  /                                       M36.3512
      DATA  LETTER(15)  /  'O'  /                                       M36.3513
      DATA  LETTER(16)  /  'P'  /                                       M36.3514
      DATA  LETTER(17)  /  'Q'  /                                       M36.3515
      DATA  LETTER(18)  /  'R'  /                                       M36.3516
      DATA  LETTER(19)  /  'S'  /                                       M36.3517
      DATA  LETTER(20)  /  'T'  /                                       M36.3518
      IF ( MONITS .EQ. 1 )  GO TO 2513                                          
      VSPAN = VMAX - VMIN                                               M35.7471
      D3 = LIMCOL / VSPAN                                               M35.7472
      KZERO = -VMIN * D3                                                M35.7473
      IF ( KZERO  .EQ.  0 )   KZERO = 1                                 M35.7474
      IF ( KZERO  .LT.  0 )   KZERO = 0                                 M35.7475
      KP(1) = 1                                                         M35.7476
      DY = VSPAN / (0.1*LIMCOL)                                         M35.7477
      IF ( MFAKE  .EQ.  JPLT )   GO TO 6413                             M35.7486
      WRITE (MUNIT6, 6403)                                              M37.7796
 6403 FORMAT (  ' Hidden variable scalings follow :'  )                 M37.7797
      CALL WINDOW                                                               
      WRITE (MUNIT6, 6411)  ( AAA(J), J=1, JPLT )                       M35.7487
 6411 FORMAT (  '  FACTOR :',  10E11.3  )                               M37.7799
      CALL WINDOW                                                               
      WRITE (MUNIT6, 6412)  ( BBB(J), J=1, JPLT )                       M35.7490
 6412 FORMAT (  '  OFFSET :',  10E11.3  )                               M35.7491
      CALL WINDOW                                                               
 6413 DIN1 = HMIN * TMULT                                               M38.4840
      DIN2 = HMAX * TMULT                                               M35.7495
      DO 6427  IP=1, NUMTIT                                                     
      WRITE (MUNIT6, 6423)   SEXT(IP)                                   M37.7803
 6423 FORMAT (  ' Graph title : ',  A80  )                              M37.7804
      CALL WINDOW                                                               
 6427 CONTINUE                                                          M37.7806
      IF ( MXYPL  .GT.  0 )   GO TO 6582                                M35.7504
      IF ( JPLT  .LE.  1 )   GO TO 6447                                 M35.7505
      DO 6431  K=2, JPLT                                                M35.7506
 6431 KP(K) = MSTART(K-1) + 5                                           M35.7507
 6447 DT = HPI / 6.0                                                    M35.7508
      TEMP(1) = VMIN + DY                                               M35.7509
      D2 = 2.0 * DY                                                     M35.7510
      N4 = 6                                                            M35.7511
      IF ( LIMCOL  .LE.  79 )   N4 = 4                                  M35.7512
      DO 6449  K=2, N4                                                  M35.7513
 6449 TEMP(K) = TEMP(K-1) + D2                                          M35.7514
      WRITE (MUNIT6, 6450)  ( TEMP(K), K=1, N4 )                        M35.7515
 6450 FORMAT ( 3X,  6( E14.5, 6X )   )                                  M37.7807
      CALL WINDOW                                                               
      IF ( LIMCOL  .LE. 79 )  GO TO 6453                                M37.7809
      WRITE (MUNIT6, 6451)                                              M37.7810
 6451 FORMAT (  1X,                                                     M36.3519
     2       '1---------1---------1---------1',                         M35.7519
     1  '---------1---------1---------1---------1',                     M35.7520
     2  '---------1---------1---------1---------1',                     M35.7521
     3  '---------1---------1'  )                                       M35.7522
      CALL WINDOW                                                               
      GO TO 6456                                                        M37.7812
 6453 WRITE (MUNIT6, 6454)                                              M37.7813
 6454 FORMAT (  1X,                                                     M36.3520
     1      '1---------1---------1---------1---------1',                M35.7525
     2       '---------1---------1---------1---------1'  )              M35.7526
      CALL WINDOW                                                               
 6456 IF ( IPRSPY  .LT. 1 )  GO TO 6460                                 M37.7815
      WRITE (MUNIT6, 6458)                                              M37.7816
 6458 FORMAT (  ' Begin character plot.',  11X, 4HHMIN,                 M37.7817
     1    11X, 4HHMAX,  13X, 2HDT,  11X, 4HVMIN,  11X, 4HVMAX )         M37.7818
      CALL WINDOW                                                               
      WRITE (MUNIT6, 6459)  HMIN, HMAX, DT, VMIN, VMAX                  M37.7820
 6459 FORMAT ( 22X, 5E15.6 )                                            M37.7821
      CALL WINDOW                                                               
 6460 INCH = 0                                                          M37.7823
      IDAT = JPLT                                                       M35.7533
      T = HMIN                                                          M35.7534
 6461 T = T + DT                                                        M35.7535
      INCH = INCH + 1                                                   M35.7536
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 1 )  GO TO 9000                                          
      IF ( T  .GT.  HMAX )   GO TO 6582                                 M35.7537
      DO 6478  K=1, LIMCOL                                              M35.7538
 6478 DOL(K) = ' '                                                      M36.3523
      DO 6531  K=1, JPLT                                                M35.7539
      L = KP(K)                                                         M35.7540
      IF ( IPRSPY  .NE.  34 )  GO TO 6483                               M37.7824
      WRITE (MUNIT6, 6481)  K, L, MSTART(K), EW(L), T                   M37.7825
 6481 FORMAT ( 1X,  24H       K       L  MSTART,  10X,                  M35.7543
     1  5HEW(L),  14X,  1HT  )                                          M37.7826
      CALL WINDOW                                                               
      WRITE (MUNIT6, 6482)  K, L, MSTART(K), EW(L), T                   M37.7828
 6482 FORMAT ( 1X, 3I8, 2E15.6 )                                        M37.7829
      CALL WINDOW                                                               
 6483 IF ( L  .EQ.  0 )   GO TO 6531                                    M37.7831
 6485 IF ( EW(L)  .LE.  T )   GO TO 6493                                M35.7546
      KP(K) = L                                                         M35.7547
      GO TO 6531                                                        M35.7548
 6493 D1 = EW(L+1)                                                      M35.7549
      IF ( D1  .GT.  VMIN )   GO TO 6503                                M35.7550
      DOL(1) = LETTER(K)                                                M35.7551
      GO TO 6518                                                        M35.7552
 6503 IF ( D1  .LT.  VMAX )   GO TO 6508                                M35.7553
      DOL(LIMCOL) = LETTER(K)                                           M35.7554
      GO TO 6518                                                        M35.7555
 6508 M = ( D1 - VMIN ) * D3                                            M35.7556
      DOL(M) = LETTER(K)                                                M35.7557
      IF ( IPRSPY .NE. 34 ) GO TO 6518                                          
      WRITE (MUNIT6, 6511)  M, DOL(M)                                   M37.7833
 6511 FORMAT (  ' In-bounds insert..  M, DOL(M) =',  I4, 1X, A1 )       M37.7834
      CALL WINDOW                                                               
 6518 L = L + 2                                                         M35.7561
      IF ( L  .LT.  MSTART(K) )   GO TO 6485                            M35.7562
      KP(K) = 0                                                         M35.7563
      IDAT = IDAT - 1                                                   M35.7564
 6531 CONTINUE                                                          M35.7565
      IF ( INCH  .GE.  6 )   GO TO 6548                                 M35.7566
      IF ( KZERO  .LE.  9 )   GO TO 6542                                M35.7567
      IF ( DOL(KZERO)  .EQ.  ' '  )  DOL(KZERO) = '1'                   M36.3524
 6542 WRITE (MUNIT6, 6543)  ( DOL(K), K=1, LIMCOL )                     M35.7568
 6543 FORMAT ( 1X, 131A1 )                                              M35.7569
      CALL WINDOW                                                               
      GO TO 6559                                                        M35.7570
 6548 IF ( T  .LE.  9.999999 )                                          M35.7571
     1 WRITE (KUNIT6, 6553)  T                                          M35.7572
 6553 FORMAT ( 1H , F8.6 )                                              M35.7573
      IF ( T  .GE.  10.0 )                                              M35.7574
     1 WRITE (KUNIT6, 6554)  T                                          M35.7575
 6554 FORMAT ( 1H , F8.5 )                                              M35.7576
      IF ( KZERO  .LE.  9 )  GO TO 6555                                 M35.7577
      IF ( DOL(KZERO)  .EQ.  ' '  )  DOL(KZERO) = '1'                   M36.3525
 6555 WRITE (MUNIT6, 6556) KUNIT6, ( DOL(K), K=10, LIMCOL )             M35.7578
 6556 FORMAT ( 1X, A9, 123A1 )                                          M35.7579
      CALL WINDOW                                                               
      INCH = 0                                                          M35.7580
 6559 IF ( IDAT  .GT.  0 )                                              M35.7581
     1 GO TO 6461                                                       M35.7582
 6582 IF ( MU6STD .EQ. 6 ) GO TO 2694                                           
      MU6STD = 6                                                                
      WRITE (MUNIT6, 2687)  LINEPR                                      M35.7585
 2687 FORMAT (  '   ***  Line printer copy completed on',               M37.7840
     1             ' unit',  I3,  ' .  ***'   )                         M37.7841
      CALL WINDOW                                                               
 2694 IF ( MONITS  .EQ.  0 )   GO TO 9000                               M35.7588
      IF ( IPRSPY .LT. 1 )  GO TO 2501                                  M37.7843
      WRITE (MUNIT6, 7832)  IND1                                        M37.7844
 7832 FORMAT (  ' Begin rolling.  IND1 =',  I8 )                        M37.7845
      CALL WINDOW                                                               
 2501 T = T + DT                                                        M35.7590
      CALL QUITER                                                               
      IF ( KWTSPY .EQ. 1 )  GO TO 9000                                          
      INCH = INCH + 1                                                   M35.7592
      DO 2504  K=1, LIMCOL                                              M35.7593
 2504 DOL(K) = ' '                                                      M36.3529
 2507 IF ( IND1 .LE. INDBUF-KPTPLT )  GO TO 2513                                
      GO TO 9000                                                                
 2513 IF ( IPRSPY .LT. 3 ) GO TO 2517                                           
      WRITE (MUNIT6, 2516)  IND1, JPLT, PLTBUF(IND1), T                 M37.7848
 2516 FORMAT (  ' Past 1/6 inch? IND1, JPLT, PLTBUF(IND1), T =',        M37.7849
     1  2I7, 2E15.5  )                                                  M35.7599
      CALL WINDOW                                                               
 2517 IF ( PLTBUF(IND1)  .GT.  T )   GO TO 2564                         M37.7851
      DO 2542  K=1, JPLT                                                M35.7601
      J1 = IABS( MPLOT(K) )                                             M35.7602
      D1 = PLTBUF(IND1+J1)                                              M35.7603
      IF ( MPLOT(K)  .LT.  0 )   D1 = -D1                               M35.7604
      IF ( D1  .GT.  VMIN )   GO TO 2532                                M35.7605
      DOL(1) = LETTER(K)                                                M35.7606
      GO TO 2542                                                        M35.7607
 2532 IF ( D1  .LT.  VMAX )   GO TO 2537                                M35.7608
      DOL(LIMCOL) = LETTER(K)                                           M35.7609
      GO TO 2542                                                        M35.7610
 2537 M = (D1 - VMIN ) * D3                                             M35.7611
      DOL(M) = LETTER(K)                                                M35.7612
 2542 CONTINUE                                                          M35.7613
      IND1 = IND1 + KPTPLT + 1                                          M35.7614
      GO TO 2507                                                        M35.7615
 2564 IF ( INCH  .LT.  6 )   GO TO 2571                                 M35.7616
      WRITE (ANSI8, 2569)  T                                            M36.3530
 2569 FORMAT ( 1X, F7.4 )                                               M35.7617
      READ (ANSI8, 2566)  ( DOL(IP), IP=1, 8 )                          M36.3531
 2566 FORMAT ( 8A1 )                                                    M36.3532
      INCH = 0                                                          M35.7618
      GO TO 2574                                                        M35.7619
 2571 IF ( KZERO  .EQ.  0 )   GO TO 2574                                M35.7620
      IF ( DOL(KZERO)  .EQ.  ' ' )   DOL(KZERO) = '1'                   M36.3533
 2574 WRITE (MUNIT6, 6543)  ( DOL(K), K=1, LIMCOL )                     M35.7621
      CALL WINDOW                                                               
      GO TO 2501                                                        M35.7622
 9000 IF ( IPRSPY .LT. 1 )  GO TO 9008                                          
      WRITE (MUNIT6, 9003)                                              M37.7854
 9003 FORMAT (  ' Exit "CHRPLT".'  )                                    M37.7855
      CALL WINDOW                                                               
 9008 RETURN                                                            M37.7857
      END                                                               M35.7627
      SUBROUTINE TEKPLT                                                         
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FLATBD               ! NO IMPLICIT                     M35.8163
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.8164
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.8165
C     THE ONE AND ONLY USE IS TO PROVIDE CALCOMP COPIES OF WHAT         M35.8166
C     IS ON THE SCREEN (FOR "PLOT" COMMAND OF "EMTSPY"), WITH           M35.8167
C     BPA PSEUDO-CALCOMP VERSATEK CALLS ACTUALLY USED.                  M35.8168
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'DEKPLT.FOR;/NOLIST'                                           
      IF ( KILLPL  .EQ.  0 )   GO TO 2005                               M35.8171
      CALL PLOT ( 0.0, 0.0, 999 )                                       M35.8172
      RETURN                                                            M35.8173
 2005 D1 = HPI * TMULT                                                  M35.8174
      WRITE (PROM80, 2010)  D1                                          M35.8175
 2010 FORMAT (  ' SEND T-AXIS UNITS/INCH (',  E12.3,                    M35.8176
     1          ' ) :'   )                                              M35.8177
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8178
      READ (MUNIT5, 7241)  BUFF77   ! READ A80 CHARACTER INPUT          M35.8179
      IF ( BUFF77(1:8)  .NE.  METRIC )   GO TO 7236                     M35.8180
      FACT = .7874                                                      M35.8181
      DXGRD1 = 0.5                                                      M35.8182
      DYGRD1 = DXGRD1                                                   M35.8183
      DXGRD2 = 0.1                                                      M35.8184
      DYGRD2 = DXGRD2                                                   M35.8185
      GO TO 2005                                                        M35.8186
 7236 IF ( BUFF77(1:8)  .NE.  PEN )   GO TO 7261                        M35.8187
      WRITE (MUNIT6, 7238)                                              M35.8188
 7238 FORMAT (                                                          M35.8189
     1   ' Respond to each pen-width request with integer'  ,/,         M35.8190
     1   ' information.  Blank or zero implies that there'  ,/,         M35.8191
     2   ' will be no change from the present value, while' ,/,         M35.8192
     3   ' a value of  -1  suppresses the printout, and'    ,/,         M35.8193
     4   ' values of  1  through  5  are Versatek dot'      ,/,         M35.8194
     5   ' widths (VAX "CalComp" at BPA is Versatek).'      )           M35.8195
      WRITE (PROM80, 7240)  MTIT                                        M35.8196
 7240 FORMAT ( 3X,  '80-COL. CASE-TITLE (',  I2,  ' ) :'   )            M35.8197
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8198
      READ (MUNIT5, 7241)  PROM80                                       M35.8199
 7241 FORMAT ( A80 )                                                    M35.8200
      CALL FREIN1 ( PROM80, N1 )   ! DECODE FREE-FIELD N1               M35.8201
      IF ( N1  .NE.  0 )   MTIT = N1                                    M35.8202
      WRITE (PROM80, 7242)  MAXISX                                      M35.8203
 7242 FORMAT ( 3X,  'X-AXIS STRUCTURE (',  I2,  ' ) :'    )             M35.8204
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8205
      READ (MUNIT5, 7241)  PROM80                                       M35.8206
      CALL FREIN1 ( PROM80, N1 )   ! DECODE FREE-FIELD N1               M35.8207
      IF ( N1  .NE.  0 )   MAXISX = N1                                  M35.8208
      WRITE (PROM80, 7244)  MAXISY                                      M35.8209
 7244 FORMAT ( 3X,  'Y-AXIS STRUCTURE (',  I2,  ' ) :'    )             M35.8210
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8211
      READ (MUNIT5, 7241)  PROM80                                       M35.8212
      CALL FREIN1 ( PROM80, N1 )   ! DECODE FREE-FIELD N1               M35.8213
      IF ( N1  .NE.  0 )   MAXISY = N1                                  M35.8214
      WRITE (PROM80, 7246)  MGRID1                                      M35.8215
 7246 FORMAT ( 3X,  'BIG BACKGROUND GRID (',  I2,  ' ) :'    )          M35.8216
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8217
      READ (MUNIT5, 7241)  PROM80                                       M35.8218
      CALL FREIN1 ( PROM80, N1 )   ! DECODE FREE-FIELD N1               M35.8219
      IF ( N1  .NE.  0 )   MGRID1 = N1                                  M35.8220
      WRITE (PROM80, 7247)  MGRID2                                      M35.8221
 7247 FORMAT ( 3X,  'FINE INNER GRID (',  I2,  ' ) :'    )              M35.8222
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8223
      READ (MUNIT5, 7241)  PROM80                                       M35.8224
      CALL FREIN1 ( PROM80, N1 )   ! DECODE FREE-FIELD N1               M35.8225
      IF ( N1  .NE.  0 )   MGRID2 = N1                                  M35.8226
      WRITE (PROM80, 7250)  MSUPER                                      M35.8227
 7250 FORMAT ( 3X,  'SUPER-TITLE (',  I2,  ' ) :'    )                  M35.8228
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8229
      READ (MUNIT5, 7241)  PROM80                                       M35.8230
      CALL FREIN1 ( PROM80, N1 )   ! DECODE FREE-FIELD N1               M35.8231
      IF ( N1  .NE.  0 )   MSUPER = N1                                  M35.8232
      WRITE (PROM80, 7251)  MID                                         M35.8233
 7251 FORMAT ( 3X,  'DATE, TIME, ETC. (',  I2,  ' ) :'    )             M35.8234
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8235
      READ (MUNIT5, 7241)  PROM80                                       M35.8236
      CALL FREIN1 ( PROM80, N1 )   ! DECODE FREE-FIELD N1               M35.8237
      IF ( N1  .NE.  0 )   MID = N1                                     M35.8238
      WRITE (MUNIT6, 7254)  ( MCURVE(J), J=1, JPLT )                    M35.8239
 7254 FORMAT ( 3X,  'VECTOR OF CURVE WEIGHTS (',  20I3,  ' ) :'  )      M35.8240
      READ (MUNIT5, *)  ( KP(J), J=1, JPLT )                            M35.8241
      DO 7255  J=1, JPLT                                                M35.8242
      IF ( KP(J)  .NE.  0 )   MCURVE(J) = KP(J)                         M35.8243
 7255 CONTINUE                                                          M35.8244
      GO TO 2005                                                        M35.8245
 7261 IF ( BUFF77(1:8)  .NE.  SHOW  )   GO TO 7287                      M35.8246
      WRITE (MUNIT6, 3421)  MTIT, MAXISX, MAXISY,                       M35.8247
     1                 MGRID1, MGRID2, MSUPER, MID                      M35.8248
 3421 FORMAT ( /,  ' BEGIN WITH  #PEN#  PARAMETERS :'  ,/,              M35.8249
     1  I5,  ' = PEN FOR 80-COLUMN CASE TITLE LINES;'  ,/,              M35.8250
     2  I5,  ' = PEN FOR X-AXIS STRUCTURE;'            ,/,              M35.8251
     3  I5,  ' = PEN FOR Y-AXIS STRUCTURE;'            ,/,              M35.8252
     4  I5,  ' = PEN FOR BIG BACKGROUND GRID;'         ,/,              M35.8253
     5  I5,  ' = PEN FOR FINE INNER GRID;'             ,/,              M35.8254
     6  I5,  ' = PEN FOR 1-LINE SUPER-TITLE;'          ,/,              M35.8255
     7  I5,  ' = PEN FOR DATE, TIME, ETC.'    )                         M35.8256
      WRITE (MUNIT6, 3428)  ( MCURVE(J), J=1, JPLT )                    M35.8257
 3428 FORMAT ( 8X,  'PENS FOR DRAWING INDIVIDUAL CURVES FOLLOW ...'     M35.8258
     1  ,/,  ( 8X,  10I5 )  )                                           M35.8259
      WRITE (MUNIT6, 3453)  XTIT, YTIT, SIZTIT, XID, YID, SIZID,        M35.8260
     1                 XSUPER, YSUPER, SIZSUP                           M35.8261
 3453 FORMAT ( /,  ' NEXT COME THE  #SIZE#  PARAMETERS :'  ,/,          M35.8262
     1  4X,  7HX-BEGIN,  3X,  7HY-BEGIN,  3X,  6HHEIGHT    ,/,          M35.8263
     2  1X,  3F10.2,  '  --- 80-COL. CASE TITLE (1ST LINE)'    ,/,      M35.8264
     3  1X,  3F10.2,  '  --- DATE, TIME, ETC. (TOP LINE)'      ,/,      M35.8265
     4  1X,  3F10.2,  '  --- ONE LINE SUPER-TITLE'    )                 M35.8266
      WRITE (MUNIT6, 3459)  HTAX, FACT, NUMSYM                          M35.8267
 3459 FORMAT ( /,                                                       M35.8268
     1  1X,  F14.3,  '  --- HEIGHT OF TIME AXIS;'               ,/,     M35.8269
     2  1X,  F14.6,  '  --- GRAPH MAGNIFICATION FACTOR',                M35.8270
     3                      ' (.7874 FOR METRIC);'              ,/,     M35.8271
     4  1X,  I14,    '  --- NUMBER OF SYMBOLS MARKING EACH CURVE.'  )   M35.8272
      WRITE (MUNIT6, 3464)  FILL1, FILL2, NCUT1, NCUT2                  M35.8273
 3464 FORMAT ( /,                                                       M35.8274
     1  11H MAJOR GRID,  4X,  10HMINOR GRID  ,/,                        M35.8275
     2  1X,  F10.3,  F14.3,  '  --- FILL-IN FRACTIONS (1.0 FOR',        M35.8276
     3                          ' SOLID GRID);'                 ,/,     M35.8277
     4  1X,    I10,    I14,  '  --- NUMBER OF DASHES PER GRID LINE;'  ) M35.8278
      WRITE (MUNIT6, 3472)  DXGRD1, DXGRD2, DYGRD1, DYGRD2              M35.8279
 3472 FORMAT (                                                          M35.8280
     1  1X,  F10.3,  F14.3,  '  --- SPACING BETWEEN VERTICAL',          M35.8281
     2                          ' GRID LINES;'                  ,/,     M35.8282
     3  1X,  F10.3,  F14.3,  '  --- SPACING BETWEEN HORIZONTAL',        M35.8283
     4                          ' GRID LINES;'   ,/, 1X  )              M35.8284
      GO TO 2005                                                        M35.8285
 7287 IF ( BUFF77(1:8)  .NE.  SIZE )   GO TO 7328                       M35.8286
      WRITE (MUNIT6, 7288)                                              M35.8287
 7288 FORMAT (  ' Respond to each request with revised',                M35.8288
     1          ' values.   A blank or zero means no'   ,/,             M35.8289
     2          ' change from the value shown within',                  M35.8290
     3          ' square brackets.'                       )             M35.8291
      WRITE (PROM80, 7291)  XTIT, YTIT, SIZTIT                          M35.8292
 7291 FORMAT ( '   X,Y COORDINATES AND SIZE OF 79-CHAR. TITLE  (',      M35.8293
     1         3F6.2,  ' ) :'  )                                        M35.8294
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8295
      READ (MUNIT5, 7241)  PROM80                                       M35.8296
      CALL FRESP3 ( PROM80, D1, D2, D3 )     ! DECODE FREE-FIELD        M38.5392
      IF ( D1  .NE.  0.0 )   XTIT = D1                                  M35.8298
      IF ( D2  .NE.  0.0 )   YTIT = D2                                  M35.8299
      IF ( D3  .NE.  0.0 )   SIZTIT = D3                                M35.8300
      WRITE (PROM80, 7304)  XID, YID, SIZID                             M35.8301
 7304 FORMAT ( '   LIKEWISE FOR DATE, TIME, ETC.  (',                   M35.8302
     1         3F6.2,  ' ) :'  )                                        M35.8303
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8304
      READ (MUNIT5, 7241)  PROM80                                       M35.8305
      CALL FRESP3 ( PROM80, D1, D2, D3 )     ! DECODE FREE-FIELD        M38.5393
      IF ( D1  .NE.  0.0 )   XID = D1                                   M35.8307
      IF ( D2  .NE.  0.0 )   YID = D2                                   M35.8308
      IF ( D3  .NE.  0.0 )   SIZID = D3                                 M35.8309
      WRITE (PROM80, 7309)  XSUPER, YSUPER, SIZSUP                      M35.8310
 7309 FORMAT ( '   LIKEWISE FOR 16-CHAR. SUPER-TITLE  (',               M35.8311
     1         3F6.2,  ' ) :'  )                                        M35.8312
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8313
      READ (MUNIT5, 7241)  PROM80                                       M35.8314
      CALL FRESP3 ( PROM80, D1, D2, D3 )     ! DECODE FREE-FIELD        M38.5394
      IF ( D1  .NE.  0.0 )   XSUPER = D1                                M35.8316
      IF ( D2  .NE.  0.0 )   YSUPER = D2                                M35.8317
      IF ( D3  .NE.  0.0 )   SIZSUP = D3                                M35.8318
      WRITE (PROM80, 7315)  HTAX                                        M35.8319
 7315 FORMAT (  '   HEIGHT OF TIME-AXIS  (',  F6.2,  ' ) :'   )         M35.8320
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8321
      READ (MUNIT5, 7241)  PROM80                                       M35.8322
      CALL FRESP1 ( PROM80, D1 )   ! DECODE FREE-FIELD D1               M38.5395
      IF ( D1  .NE.  0.0 )   HTAX = D1                                  M35.8324
      WRITE (PROM80, 7319)  FACT                                        M35.8325
 7319 FORMAT (  '   BLOWUP FACTOR.  METRIC=.7874  (',                   M35.8326
     1          F6.4,  ' ) :'    )                                      M35.8327
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8328
      READ (MUNIT5, 7241)  PROM80                                       M35.8329
      CALL FRESP1 ( PROM80, D1 )   ! DECODE FREE-FIELD D1               M38.5396
      IF ( D1  .NE.  0.0 )   FACT = D1                                  M35.8331
      WRITE (PROM80, 7320)  FILL1, FILL2                                M35.8332
 7320 FORMAT (  '   FILL-IN FRACTIONS FOR MAJOR AND MINOR GRIDS  (',    M35.8333
     1  2F6.3,  ' ) :'  )                                               M35.8334
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8335
      READ (MUNIT5, 7241)  PROM80                                       M35.8336
      CALL FRESP2 ( PROM80, D1, D2 )  ! DECODE FREE-FIELD D1, D2        M38.5397
      IF ( D1  .NE.  0.0 )   FILL1 = D1                                 M35.8338
      IF ( D2  .NE.  0.0 )   FILL2 = D2                                 M35.8339
      WRITE (PROM80, 7321)  NCUT1, NCUT2                                M35.8340
 7321 FORMAT (  '   NUMBER OF BREAKS FOR MAJOR AND MINOR GRIDS  (',     M35.8341
     1  I4,  '.0 ',  I4,  '.0 ) :'   )                                  M35.8342
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8343
      READ (MUNIT5, 7241)  PROM80                                       M35.8344
      CALL FRESP2 ( PROM80, D1, D2 )  ! DECODE FREE-FIELD D1, D2        M38.5398
      IF ( D1  .NE.  0.0 )   NCUT1 = D1                                 M35.8346
      IF ( D2  .NE.  0.0 )   NCUT2 = D2                                 M35.8347
      WRITE (PROM80, 7322)  DXGRD1, DYGRD1                              M35.8348
 7322 FORMAT (  '   X,Y SPACING BETWEEN MAJOR GRID LINES  (',           M35.8349
     1          2F6.3,  ' ) :'  )                                       M35.8350
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8351
      READ (MUNIT5, 7241)  PROM80                                       M35.8352
      CALL FRESP2 ( PROM80, D1, D2 )  ! DECODE FREE-FIELD D1, D2        M38.5399
      IF ( D1  .GT.  0.0 )   DXGRD1 = D1                                M35.8354
      IF ( D2  .GT.  0.0 )   DYGRD1 = D2                                M35.8355
      WRITE (PROM80, 7323)  DXGRD2, DYGRD2                              M35.8356
 7323 FORMAT (  '   X,Y SPACING BETWEEN MINOR GRID LINES  (',           M35.8357
     1          2F6.3,  ' ) :'  )                                       M35.8358
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8359
      READ (MUNIT5, 7241)  PROM80                                       M35.8360
      CALL FRESP2 ( PROM80, D1, D2 )  ! DECODE FREE-FIELD D1, D2        M38.5400
      IF ( D1  .GT.  0.0 )   DXGRD2 = D1                                M35.8362
      IF ( D2  .GT.  0.0 )   DYGRD2 = D2                                M35.8363
      WRITE (PROM80, 7325)  NUMSYM                                      M35.8364
 7325 FORMAT (  '   NUMBER OF MARKING SYMBOLS PER CURVE  (',            M35.8365
     1          I3,  ' ) :'   )                                         M35.8366
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35.8367
      READ (MUNIT5, 7241)  PROM80                                       M35.8368
      CALL FREIN1 ( PROM80, N2 )   ! DECODE FREE-FIELD N2               M35.8369
      IF ( N2  .NE.  0 )   NUMSYM = N2                                  M35.8370
      GO TO 2005                                                        M35.8371
 7328 READ (BUFF77, 2030)  D3                                           M35.8372
 2030 FORMAT ( 10E6.0 )                                                 M35.8373
      IF ( D3  .EQ.  0.0 )   GO TO 2060                                 M35.8374
      HPI = D3 / TMULT                                                  M35.8375
 2060 D1 = ( HMAX - HMIN ) / HPI                                        M35.8376
      IF(D1 .LE. 72.0) GO TO 2090                                       M35.8377
      WRITE (MUNIT6, 2070)                                              M35.8378
 2070 FORMAT ( 64H ERROR. ----- REQUESTED FLATBED PLOT WOULD EXCEED 72 IM35.8379
     1NCH LIMIT.   ,/,  32H SEND ANOTHER TIME-AXIS SCALING.   )         M35.8380
      GO TO 2005                                                        M35.8381
 2090 IF ( IPRSRT  .GE.  1 )                                            M35.8382
     1 WRITE (MUNIT6, 2103)  JPLT, IHS, NUMFLT, VMIN, VMAX,             M35.8383
     2                  HMIN, HMAX, HPI                                 M35.8384
 2103 FORMAT ( /,  ' BEGIN FLATBED.',                                   M35.8385
     1  24H    JPLT     IHS  NUMFLT  ,/,  15X,  3I8  ,/,                M35.8386
     2  1X,  11X,  4HVMIN,  11X,  4HVMAX,  11X,  4HHMIN,                M35.8387
     3  11X,  4HHMAX,  12X,  3HHPI  ,/,  1X,  5E15.6  )                 M35.8388
      IF ( NUMFLT  .GT.  0 )   GO TO 2108                               M35.8389
      CALL PLOTS ( 0, 0, 0 )                                            M35.8390
      CALL PLOT ( 1.0, 1.0, -3 )                                        M35.8391
      CALL FACTOR ( FACT )                                              M35.8392
 2108 NUMFLT = NUMFLT + 1                                               M35.8393
      WRITE (MUNIT6, 2109)                                              M35.8394
 2109 FORMAT ( '    Ready to draw CalComp titles.'  )                   M35.8395
      N3 = NUMTIT / 10                                                  M35.8396
      IF ( N3  .LE.  0 )   GO TO 2114                                   M35.8397
      IF ( MTIT  .EQ.  -1 )   GO TO 2114                                M35.8398
      D2 = YTIT                                                         M35.8399
      N4 = 1                                                            M35.8400
      IF ( YTIT  .GT.  10 )   GO TO 2114                                M35.8401
      CALL NEWPEN ( MTIT )                                              M35.8402
      DO 2113  J=1, N3                                                  M35.8403
      CALL SYMBOL ( XTIT, D2, SIZTIT, SEXT(N4), 0.0, 80 )               M35.8404
      D2 = D2 - FLINE * SIZTIT                                          M35.8405
 2113 N4 = N4 + 10                                                      M35.8406
 2114 N1 = 3 * ( IHS - 1 )  +  1                                        M35.8407
C     ONLY UNITS OF  #SECONDS#  CAN BE PRESENTLY HONORED.               M35.8408
      N1 = 7                                                            M35.8409
      IF ( MXYPL  .EQ.  1 )   N1 = 22                                   M35.8410
      D2 = (VMAX - VMIN ) / 8.0                                         M35.8411
      CALL NEWPEN ( MAXISX )                                            M35.8412
      WRITE (MUNIT6, 2115)                                              M35.8413
 2115 FORMAT ( '   Ready to draw CalComp axes.'  )                      M35.8414
      IF ( MAXISX  .GT.  0 )                                            M35.8415
     1 CALL AXIS ( 0.0,  HTAX, HORZL(N1),  -24,  D1,  0.0, HMIN,  HPI ) M35.8416
      CALL NEWPEN ( MAXISY )                                            M35.8417
      IF ( MAXISY  .GT.  0 )                                            M35.8418
     1 CALL AXIS ( 0.0,  0.0,  VERTL,   NCHVER,  8.,  90.,              M35.8419
     2             VMIN,  D2 )                                          M35.8420
      N8 = D1 / DXGRD1  +  0.5                                          M35.8421
      N9 = 8.0 / DYGRD1  +  0.5                                         M35.8422
      FILL = FILL1                                                      M35.8423
      NCUT = NCUT1                                                      M35.8424
      CALL NEWPEN ( MGRID1 )                                            M35.8425
      WRITE (MUNIT6, 2116 )                                             M35.8426
 2116 FORMAT (  '   Ready to call CalComp grid number 1.'  )            M35.8427
      IF ( MGRID1  .GT.  0 )                                            M35.8428
     1 CALL GRID ( 0.0, 0.0, N8, DXGRD1, N9, DYGRD1, LMASK1 )           M35.8429
      WRITE (MUNIT6, 2117 )                                             M35.8430
 2117 FORMAT ( '   Done with CalComp grid number 1.'  )                 M35.8431
      CALL NEWPEN ( MGRID2 )                                            M35.8432
      FILL = FILL2                                                      M35.8433
      NCUT = NCUT2                                                      M35.8434
      N8  =  D1 / DXGRD2  +  0.5                                        M35.8435
      N9  =  8.0/ DYGRD2  +  0.5                                        M35.8436
      IF ( MGRID2  .GT.  0 )                                            M35.8437
     1 CALL GRID ( 0.0, 0.0, N8, DXGRD2, N9, DYGRD2, LMASK2 )           M35.8438
      CALL NEWPEN ( MSUPER )                                            M35.8439
      WRITE (MUNIT6, 2118)                                              M35.8440
 2118 FORMAT ( '   Done with CalComp grid number 2.'  )                 M35.8441
      IF ( MSUPER  .GT.  0 )                                            M35.8442
     1 CALL SYMBOL ( XSUPER, YSUPER, SIZSUP, HEADL, 0.0, NCHSUP )       M35.8443
      D6 = YID                                                          M35.8444
C     ENCODE (18, 2119, ALPHA(1) )   DATE1, TCLOCK                      M35.8445
      WRITE (ANSI32, 2119)  DATEPL, TCLOPL                              M37.7916
 2119 FORMAT ( 2A4, 2X, 2A4 )                                           M35.8447
      CALL NEWPEN ( MID )                                               M35.8448
      IF ( MID  .EQ.  -1 )   GO TO 2127                                 M35.8449
      CALL SYMBOL ( XID, D6, SIZID, ANSI32, 0.0, 18 )                   M35.8450
      D6 = D6 - FLINE * SIZID                                           M35.8451
C     ENCODE (14, 2122, ALPHA(1) )   ICP                                M35.8452
      WRITE (ANSI16, 2122)  ICP                                         M35.8453
 2122 FORMAT ( 9HPLOT TYPE, I5 )                                        M35.8454
      CALL SYMBOL ( XID, D6, SIZID, ANSI16, 0.0, 14 )                   M35.8455
      D6 = D6 - FLINE * SIZID                                           M35.8456
      IF ( MFAKE  .EQ.  JPLT )   GO TO 3149                             M35.8457
      N5 = JPLT                                                         M35.8458
      IF ( JPLT  .GT.  6 )   N5 = 6                                     M35.8459
C     ENCODE (80, 3146, ALPHA(1) )  ( AAA(J), J=1, N5 )                 M35.8460
      WRITE (BUFF77, 3146)  ( AAA(J), J=1, N5 )                         M35.8461
 3146 FORMAT (  9H FACTOR :,  6E11.3  )                                 M35.8462
      CALL SYMBOL ( XID, D6, SIZID, BUFF77, 0.0, 80 )                   M35.8463
      D6 = D6 - FLINE * SIZID                                           M35.8464
C     ENCODE (80, 3147, ALPHA(1) )  ( BBB(J), J=1, N5 )                 M35.8465
      WRITE (BUFF77, 3147)  ( BBB(J), J=1, N5 )                         M35.8466
 3147 FORMAT (  9H OFFSET :,  6E11.3  )                                 M35.8467
      CALL SYMBOL ( XID, D6, SIZID, BUFF77, 0.0, 80 )                   M35.8468
      D6 = D6 - FLINE * SIZID                                           M35.8469
 3149 WRITE (MUNIT6, 3150)                                              M35.8470
 3150 FORMAT ( '   Ready for SLOT1 ENCODE.'  )                          M35.8471
      N5 = 4                                                            M35.8472
      IF ( NAMVAR  .LT.  4 )   N5 = NAMVAR                              M35.8473
C     ENCODE (36, 2125, ALPHA(1) )   ( SLOT1(J), J=1, N5 )              M35.8474
      WRITE (BUFF77, 2125)  ( SLOT1(J), J=1, N5 )                       M35.8475
 2125 FORMAT ( 8HNAMES : ,  4A7  )                                      M35.8476
      CALL SYMBOL ( XID, D6, SIZID, BUFF77, 0.0, 36 )                   M35.8477
      IF ( NAMVAR  .LE.  4 )   GO TO 2127                               M35.8478
      DO 2140  J=5, NAMVAR, 4                                           M35.8479
      N5 = J + 3                                                        M35.8480
C     ENCODE (36, 2137, ALPHA(1) )                                      M35.8481
      WRITE (BUFF77, 2137)                                              M35.8482
 2137 FORMAT ( 36X )                                                    M35.8483
C     ENCODE (36, 2138, ALPHA(1) )   ( SLOT1(I), I=J, N5 )              M35.8484
      WRITE (BUFF77, 2137)                                              M35.8485
 2138 FORMAT ( 8X,  4A7 )                                               M35.8486
      D6 = D6 - FLINE * SIZID                                           M35.8487
 2140 CALL SYMBOL ( XID, D6, SIZID, BUFF77, 0.0, 36 )                   M35.8488
 2127 N4 = 0                                                            M35.8489
      DO 2132  J=1, JPLT                                                M35.8490
      N5 = KSTART(J)                                                    M35.8491
      N6 = ( N5 - N4 ) / 2                                              M35.8492
      N7 = N6 / NUMSYM                                                  M35.8493
      IF ( NUMSYM  .LT.  0 )   N7 = 99999                               M35.8494
      IF ( NUMSYM  .GT.  N6 )  N7 = 1                                   M35.8495
      EW(N5+3) = HPI                                                    M35.8496
      N8 = N4 + 2*N6 + 8                                                M35.8497
      IF ( IPRSRT  .GE.  5 )                                            M35.8498
     1 WRITE (MUNIT6, 3117)  J, N4, N5, N6, N7, (EW(MM), MM=N4, N8)     M35.8499
 3117 FORMAT ( /,  ' READY TO CALL LINE.',                              M35.8500
     1  40H       J      N4      N5      N6      N7  ,/,                M35.8501
     2  20X,  5I8  ,/,  ( 1X,  8E15.6  )  )                             M35.8502
      CALL NEWPEN ( MCURVE(J) )                                         M35.8503
      SX = 1.0 / EW(N5+3)                                               M35.8504
      SY = 1.0 / EW(N5+4)                                               M35.8505
      XMIN = EW(N5+1)                                                   M35.8506
      YMIN = EW(N5+2)                                                   M35.8507
      D1 = ( EW(N4+1) - XMIN ) * SX                                     M35.8508
      D2 = ( EW(N4+2) - YMIN ) * SY                                     M35.8509
      CALL PLOT ( D1, D2, 3 )                                           M35.8510
      N6 = N7                                                           M35.8511
      IF ( IPRSRT  .GE.  2 )                                            M35.8512
     1 WRITE (MUNIT6, 4122)  XMIN, YMIN, SX, SY, D1, D2                 M35.8513
 4122 FORMAT ( /, ' ENTER CURVE-LOOP.  XMIN, YMIN, SX, SY, D1, D2 =',   M35.8514
     1  /, 1X, 6E13.4 )                                                 M35.8515
 2121 N4 = N4 + 2                                                       M35.8516
      IF ( N4  .GE.  N5 )   GO TO 2132                                  M35.8517
      N6 = N6 - 1                                                       M35.8518
      IF ( N6  .GT.  0 )   GO TO 8243                                   M35.8519
      CALL SYMBOL ( D1, D2, SIZID, J, 0.0, -1 )                         M35.8520
      N6 = N7                                                           M35.8521
 8243 D1 = (EW(N4+1) - XMIN ) * SX                                      M35.8522
      D2 = (EW(N4+2) - YMIN ) * SY                                      M35.8523
      IF ( IPRSRT  .GE.  9 )                                            M35.8524
     1 WRITE (MUNIT6, 2123)  N4, N6, D1, D2                             M35.8525
 2123 FORMAT ( ' NEXT POINT DRAWN.  N4, N6, D1, D2 =',  2I5, 2E13.4 )   M35.8526
      CALL PLOT ( D1, D2, 2 )                                           M35.8527
      GO TO 2121                                                        M35.8528
 2132 N4 = N5 + 4                                                       M35.8529
      D1 = D1 + 1.0                                                     M35.8530
      CALL PLOT ( D1, 0.0, -3 )                                         M35.8531
      IF ( IPRSPY  .GE.  1 )                                            M35.8532
     1 WRITE (MUNIT6, 9006)                                             M35.8533
 9006 FORMAT ( '   Done with CalComp copy of screen.'  )                M35.8534
      RETURN                                                            M35.8535
      END                                                               M35.8536
      SUBROUTINE TGRID(IX,IY,NX,IDELX,NY,IDELY,LDASH) ! NO IMPLICIT     M35.8546
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE USED ONLY FOR INTERACTIVE EMTP (SERVICE TO "EMTSPY").      M35.8547
C     FOR NON-INTERACTIVE EMTP, THIS MODULE CAN BE DESTROYED.           M35.8548
C       RELIANCE UPON TEKTRONIX PLOT10 MAKES THIS INSTALLATION-         M36.3560
C       DEPENDENT (THIS IS VAX-11 MODULE, ACTUALLY).                    M36.3561
      IF ( NX  .LE.  0 )   RETURN                                       M35.8549
      IF ( NY  .LE.  0 )   RETURN                                       M35.8550
      CALL MOVABS ( IX, IY )                                            M35.8551
      N8 = IX  +  ( NX - 1 ) * IDELX                                    M35.8552
      N9 = IY  +  ( NY - 1 ) * IDELY                                    M35.8553
      N3 = N9                                                           M35.8554
      N2 = IX                                                           M35.8555
      DO 1341  J=1, NX                                                  M35.8556
      CALL DSHABS ( N2, N3, LDASH )                                     M35.8557
      N2 = N2 + IDELX                                                   M35.8558
      IF ( J  .EQ.  NX )   GO TO 1356                                   M35.8559
      CALL MOVABS ( N2, N3 )                                            M35.8560
      N4 = N3                                                           M35.8561
      N3 = N9                                                           M35.8562
      IF ( N4  .EQ.  N9 )   N3 = IY                                     M35.8563
 1341 CONTINUE                                                          M35.8564
 1356 CALL MOVABS ( N8, N9 )                                            M35.8565
      N2 = IX                                                           M35.8566
      N3 = N9                                                           M35.8567
      DO 1374  J=1, NY                                                  M35.8568
      CALL DSHABS ( N2, N3, LDASH )                                     M35.8569
      IF ( J  .EQ.  NY )   GO TO 1382                                   M35.8570
      N3 = N3 - IDELY                                                   M35.8571
      CALL MOVABS ( N2, N3 )                                            M35.8572
      N4 = N2                                                           M35.8573
      N2 = N8                                                           M35.8574
      IF ( N4  .EQ.  N8 )   N2 = IX                                     M35.8575
 1374 CONTINUE                                                          M35.8576
 1382 RETURN                                                            M35.8577
      END                                                               M35.8578
