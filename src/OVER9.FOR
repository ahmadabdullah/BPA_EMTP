      SUBROUTINE OVER9 
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      INCLUDE  'DECK10.FOR;/NOLIST'                                           
      EQUIVALENCE ( IOFKOL, IOFGND ), ( IOFKOR, IOFBND )                M33. 869
C     FOLLOWING CARRIES "NEXT" AMONG OVER6, INSERT, OVER7, & OVER9:     M37.3694
      EQUIVALENCE  ( LOOPSS(11), NEXT )                                         
      LOCATN(I,J)=(J*J-J)/2+I                                           M42.5880
      IF ( IPRSUP  .GE.  1 )                                            M32.3293
     1 WRITE ( LUNIT6, 4567 )                                           M32.3294
 4567 FORMAT ( 23H  "BEGIN MODULE OVER9." )                             M32.3295
      IF( IPRSUP .GT. 0 )                                               M42.5881
     1 WRITE(LUNIT6, 1001)  KCONST, LAST                                M31.2427
 1001 FORMAT ( 36H STEADY-STATE PHASOR SOLUTION BEGINS,                 M31.2428
     1         35H (TOP OF "OVER9").   KCONST, LAST =,  2I8 )           M31.2429
      IF ( ISTEP  .NE. -4567 )  GO TO 1024                                      
      NKR = 0                                                           M39.1456
      LSTAT(70) = KCONST                                                M39.1457
 1111 CALL CIMAGE                                                               
      DECODE ( 80, 1133, ABUFF ) NK, BUS1                               M39.1459
 1133 FORMAT ( I2, A6 )                                                 M39.1460
      IF ( BUS1  .EQ.  BLANK )  GO TO 2200                                      
      NKR = NKR + 1                                                             
      IF ( NKR  .GE.  LEXCT )  GO TO 9393                               M42.5882
      DECODE (80, 1983, ABUFF)   NEKFIX(NKR), FIXBU1(NKR),              M39.1464
     1 FIXBU2(NKR), FIXBU3(NKR), FIXBU5(NKR), FIXBU8(NKR),              M39.1465
     2 FIXBU7(NKR), FIXBU9(NKR), FIXBU4(NKR), FIXBU6(NKR)               M39.1466
 1983 FORMAT (  I2, 3A6, 2E16.0, 2E8.0, 2E6.0 )                         M39.1467
      IF ( FIXBU9(NKR) .EQ. 0.0 ) FIXBU9(NKR) = FLTINF                  M39.1468
      IF ( FIXBU4(NKR) .EQ. 0.0 ) FIXBU4(NKR) = - FLTINF                M39.1469
      IF ( FIXBU6(NKR) .EQ. 0.0 ) FIXBU6(NKR) = FLTINF                  M39.1470
      WRITE (KUNIT6, 1984) FIXBU5(NKR), FIXBU8(NKR),                    M39.1471
     1                     FIXBU7(NKR), FIXBU9(NKR)                     M39.1472
 1984 FORMAT ( 5H+FIX.,  4E11.3 )                                       M39.1473
      GO TO 1111                                                                
 9393 KILL = 227                                                        M39.1475
      WRITE ( LUNIT6, 8944)  NKR                                        M39.1476
 8944 FORMAT( 26H THE NUMBER OF FIX SOURCES, I5, 2X,                    M39.1477
     1 46H SHOULD BE LESS THAN THE NUMBER OF ALL SOURCES  )             M39.1478
      GO TO 9999                                                        M39.1479
 2200 LSTAT(69) = NKR                                                   M39.1480
      IF ( TMAX .GT. 0.0 )      GO TO 1024                              M39.1481
      DO 3535 I = 1, NKR                                                M39.1482
      TEXVEC(1) = FIXBU1(I)                                             M39.1483
      DO 1016  K=1, KCONST                                                      
      K1 = IABS( NODE(K) )                                              M39.1485
      IF ( BUS(K1) .EQ. TEXVEC(1) ) GO TO 3131                          M39.1486
 1016 CONTINUE                                                                  
      N1 = 1                                                            M39.1488
      KCONST = KCONST + 1                                               M39.1489
      CREST(KCONST) = ( FIXBU7(I) + FIXBU9(I) ) * 0.5                   M39.1490
      TIME1(KCONST) = ( FIXBU4(I) + FIXBU6(I) ) * TWOPI / 720.          M39.1491
 1911 DO 4949 J = 2, NTOT                                               M39.1492
      IF ( BUS(J) .EQ. TEXVEC(1) ) GO TO 5757                           M39.1493
 4949 CONTINUE                                                          M39.1494
      KILL = 500                                                        M39.1495
      GO TO 9999                                                        M39.1496
 5757 NODE(KCONST) = J                                                  M39.1497
      J1 = KSSFRQ(J)                                                    M39.1498
      SFREQ(KCONST) = SFREQ(J1)                                                 
      IFORM(KCONST) = 14                                                M39.1500
      TSTOP(KCONST) = 1.0                                               M39.1501
      TSTART(KCONST) = -1.0                                             M39.1502
      GO TO ( 3131, 3333, 3535 ), N1                                    M39.1503
 3131 TEXVEC(1) = FIXBU2(I)                                             M39.1504
      IF ( TEXVEC(1) .EQ. BLANK ) GO TO 3535                            M39.1505
      DO 1036  K = 1, KCONST                                            M39.1506
      K1 = IABS( NODE(K) )                                              M39.1507
      IF ( BUS(K1) .EQ. FIXBU2(I) ) GO TO 3333                          M39.1508
 1036 CONTINUE                                                          M39.1509
      N1 = 2                                                            M39.1510
      KCONST = KCONST + 1                                               M39.1511
      CREST(KCONST) = CREST(KCONST-1)                                   M39.1512
      TIME1(KCONST) = TIME1(KCONST-1) - TWOPI/3.0                       M39.1513
      GO TO 1911                                                        M39.1514
 3333 TEXVEC(1) = FIXBU3(I)                                             M39.1515
      IF ( TEXVEC(1) .EQ. BLANK ) GO TO 3535                            M39.1516
      DO 1056  K = 1, KCONST                                            M39.1517
      K1 = IABS( NODE(K) )                                              M39.1518
      IF ( BUS(K1) .EQ. FIXBU3(I) ) GO TO 3535                          M39.1519
 1056 CONTINUE                                                          M39.1520
      N1 = 3                                                            M39.1521
      KCONST = KCONST + 1                                               M39.1522
      CREST(KCONST) = CREST(KCONST-1)                                   M39.1523
      TIME1(KCONST) = TIME1(KCONST-1) - TWOPI/3.0                       M39.1524
      GO TO 1911                                                        M39.1525
 3535 CONTINUE                                                          M39.1526
 1024 IF ( KCONST  .EQ.  0 )   GO TO 1034                               M39.1527
      DO 1030 I = 1, KCONST                                             M42.5883
      IF ( IABS( IFORM(I) )  .NE.  14 )   GO TO 1030                    M22.3245
      IF ( TSTART(I)  .GE.  0.0 )   GO TO 1030                          M22.3246
      GO TO 1050                                                        M31.2430
 1030 CONTINUE                                                          M42.5884
 1034 WRITE (LUNIT6, 1041)                                              M19. 874
 1041 FORMAT ( /, 106H COMMENT ---- NO SINUSOIDAL SOURCES REQUESTED FOR M42.5885
     1STEADY-STATE SOLUTION.   THUS THIS SOLUTION IS BYPASSED.   )      M42.5886
      LASTOV = NCHAIN                                                   M15. 754
      NCHAIN = 12                                                       M42.5887
      GO TO 9990                                                        M32.3296
 1050 CONTINUE                                                          M42.5888
      NEXT = 1                                                          M42.5889
      JBRT = LAST                                                       M42.5890
      ISUBS1 =  IOFKOR+1                                                M16.1386
      CALL MOVE0 ( KORDER(ISUBS1)  , JBRT )                             M16.1387
      CALL MOVE0 ( LOC(1), NTOT )                                       M42.5891
      I = 1                                                             M42.5892
      GO TO 1160                                                        M42.5893
 1150 I = II + 1                                                        M42.5894
 1160 IF ( I  .GT.  IBR )   GO TO 1500                                  M31.2431
      II = I +IABS(LENGTH(I)) -1                                        M42.5895
C      IF (KODSEM(I) .NE. 0  .AND. IMODEL(I) .NE. -2                    M31.2432
C     1                      .AND. IMODEL(I) .NE. -4)                   M43.1110
      IF (KODSEM(I) .NE. 0  .AND. IMODEL(I) .GE. 0)                     M43.1111
     1 II = I + IABS(KODEBR(I)) - 1                                     M31.2433
      ISWBOB = 1                                                        M42.5896
      J = I                                                             M42.5897
       GO TO 1250                                                       M42.5898
 1220 J = J + 1                                                         M42.5899
      IF(ISWBOB.EQ.0) GO TO 1320                                        M42.5900
      IF (J .GT. II) GO TO 1330                                         M42.5901
 1250 K = IABS(KBUS(J))                                                 M42.5902
 1260 IF (K .EQ. 0) GO TO 1220                                          M42.5903
      JSW = 1                                                           M42.5904
      L = I                                                             M42.5905
      GO TO 1290                                                        M42.5906
 1280 L = L + 1                                                         M42.5907
 1290 IF (L .GT. II) GO TO 1350                                         M42.5908
      M = IABS(KBUS(L))                                                 M42.5909
      IF (M .EQ. K  .OR.  M .EQ. 0)   GO TO 1280                        M42.5910
      GO TO 1400                                                        M42.5911
 1320 IF (J .GT. II) GO TO 1150                                         M42.5912
      GO TO 1340                                                        M42.5913
 1330 ISWBOB = 0                                                        M42.5914
      J = I                                                             M42.5915
 1340 K = IABS(MBUS(J))                                                 M42.5916
      GO TO 1260                                                        M42.5917
 1350 JSW = 2                                                           M42.5918
      L = I                                                             M42.5919
      GO TO 1370                                                        M42.5920
 1360 L = L + 1                                                         M42.5921
 1370 IF (L .GT. II) GO TO 1220                                         M42.5922
      M = IABS(MBUS(L))                                                 M42.5923
      IF (M .EQ. K  .OR.  M .LT. 1)   GO TO 1360                        M42.5924
 1400 KF = LOC(K)                                                       M42.5925
      IF (KF .EQ. 0) GO TO 1420                                         M42.5926
      GO TO 1440                                                        M42.5927
 1420 LOC(K) = NEXT                                                     M42.5928
      GO TO 1480                                                        M42.5929
 1430 ISUBS1 =  IOFKOR+KF                                               M16.1388
      KF = KORDER(ISUBS1)                                               M16.1389
 1440 ISUBS1 =  IOFKOL+KF                                               M16.1390
      N1 = M - KOLUM(ISUBS1)                                            M16.1391
      IF( N1 .GT. 0 )  GO TO 1460                                       M42.5930
      IF( N1 .EQ. 0 )  GO TO 1490                                       M42.5931
      ISUBS1 =  IOFKOL+KF                                               M16.1392
      MA = KOLUM(ISUBS1)                                                M16.1393
      ISUBS1 =  IOFKOL+KF                                               M16.1394
      KOLUM(ISUBS1)    = M                                              M16.1395
      M = MA                                                            M42.5932
 1460 ISUBS1 =  IOFKOR+KF                                               M16.1396
      IF (KORDER(ISUBS1)    .NE. 0) GO TO 1430                          M16.1397
      ISUBS1 =  IOFKOR+KF                                               M16.1398
      KORDER(ISUBS1)    = NEXT                                          M16.1399
 1480 ISUBS1 =  IOFKOL+NEXT                                             M16.1400
      KOLUM(ISUBS1)      = M                                            M16.1401
      IF (NEXT .LT. LAST)   GO TO 1495                                  M42.5933
      KILL = 220                                                        M30. 157
      LSTAT(19) = 1495                                                  M42.5934
      LSTAT(15) = I                                                     M30. 158
      GO TO 9999                                                        M42.5935
 1495 NEXT = NEXT + 1                                                   M42.5936
 1490 GO TO (1280, 1360),JSW                                            M42.5937
 1500 DO 1550  I=2, NTOT                                                M42.5938
      IK = 0                                                            M42.5939
      IL = LOC(I)                                                       M42.5940
 1530 IF (IL .EQ. 0) GO TO 1550                                         M42.5941
      IK = IK + 1                                                       M42.5942
      ISUBS1 =  IOFKOR+IL                                               M16.1402
      IL = KORDER(ISUBS1)                                               M16.1403
      GO TO 1530                                                        M42.5943
 1550 KOWNT(I) = IK                                                     M42.5944
C     PREVENT VAX I/O INTERRUPT ON KOWNT(1) GARBAGE (NOV, 81):          M31.2434
      IF ( IABS(KOWNT(1))  .GT.  99999 )  KOWNT(1) = 0                  M31.2435
      IF( IPRSUP .GE. 3 )                                               M42.5945
     1 WRITE(LUNIT6, 1554)  ( KOWNT(I), I=1, NTOT )                     M17. 491
 1554 FORMAT ( /, 109H (KOWNT(I), I=1, NTOT)   IN  'OVER9' ,  AFTER BRANM42.5946
     1CH TABLE HAS BEEN PUT INTO STEADY-STATE RENUMBERING ARRAYS.  ,/,  M42.5947
     1 ( 1X, 20I6 ) )                                                   M42.5948
      LASTM1 = LAST -1                                                  M42.5949
      DO 1570 I = NEXT, LASTM1                                          M42.5950
      ISUBS1 =  IOFKOR+I                                                M16.1404
 1570 KORDER(ISUBS1)   = I + 1                                          M16.1405
      ISUBS1 =  IOFKOR+LAST                                             M16.1406
      KORDER(ISUBS1)      = 0                                           M16.1407
      CALL MOVE0 ( KODE(1), NTOT )                                      M42.5951
      IF (KSWTCH .EQ. 0) GO TO 1940                                     M42.5952
      I = 0                                                             M19. 875
 1631 I = I + 1                                                         M19. 876
      IF ( ADELAY(I)  .EQ.  -44444. )   GO TO 1650                      M24. 619
      IF (TCLOSE(I) .GE. 0.) GO TO 1640                                 M42.5953
      IF ( ADELAY(I)   .EQ.  44444. )   GO TO 1640                      M19. 877
      GO TO 1650                                                        M42.5954
 1640 IF ( I  .LT.  KSWTCH )   GO TO 1631                               M19. 878
      GO TO 1940                                                        M42.5955
 1650 J = KMSWIT(I)                                                     M32.3297
      NDX1 = LSWTCH + I                                                 M32.3298
      K = KMSWIT(NDX1)                                                  M32.3299
      IF ( K .GT. J) GO TO 1655                                         M32.3300
      NDX2 = K                                                          M32.3301
      K = J                                                             M32.3302
      J = NDX2                                                          M32.3303
 1655 IF( KODE(J) .NE. 0 )  GO TO 1760                                  M32.3304
      IF( KODE(K) .NE. 0 )  GO TO 1690                                  M42.5956
      KODE(J) = K                                                       M42.5957
      KODE(K) = J                                                       M42.5958
      GO TO 1640                                                        M42.5959
 1690 L = K                                                             M42.5960
      GO TO 1710                                                        M42.5961
 1700 L = KODE(L)                                                       M42.5962
 1710 IF( KODE(L) .GT. L )  GO TO 1700                                  M42.5963
      M = KODE(L)                                                       M42.5964
 1730 IF( M .GT. J )  GO TO 1750                                        M42.5965
      L = M                                                             M42.5966
      M = KODE(M)                                                       M42.5967
      GO TO 1730                                                        M42.5968
 1750 KODE(J) = M                                                       M42.5969
      KODE(L) = J                                                       M42.5970
      GO TO 1640                                                        M42.5971
 1760 IF( KODE(K) .NE. 0 )  GO TO 1820                                  M42.5972
      L = J                                                             M42.5973
      GO TO 1790                                                        M42.5974
 1780 L = KODE(L)                                                       M42.5975
 1790 IF( KODE(L) .GE. K )  GO TO 1810                                  M42.5976
      IF( KODE(L) .GT. L )  GO TO 1780                                  M42.5977
 1810 M = KODE(L)                                                       M42.5978
      KODE(L) = K                                                       M42.5979
      KODE(K) = M                                                       M42.5980
      GO TO 1640                                                        M42.5981
 1820 IF( KODE(J) .LE. J )  GO TO 1840                                  M42.5982
      J = KODE(J)                                                       M42.5983
      GO TO 1820                                                        M42.5984
 1840 IF( KODE(K) .LE. K )  GO TO 1860                                  M42.5985
      K = KODE(K)                                                       M42.5986
      GO TO 1840                                                        M42.5987
 1860 L = J                                                             M42.5988
      IF( L .LT. K )  L = K                                             M42.5989
      J = KODE(J)                                                       M42.5990
      K = KODE(K)                                                       M42.5991
 1870 IF( J .GT. K )  GO TO 1910                                        M42.5992
      IF( J .EQ. K )  GO TO 1640                                        M42.5993
      KODE(L) = J                                                       M42.5994
      L = J                                                             M42.5995
      J = KODE(J)                                                       M42.5996
      IF (J .GT. L) GO TO 1870                                          M42.5997
      J = 10000                                                         M42.5998
      GO TO 1870                                                        M42.5999
 1910 KODE(L) = K                                                       M42.6000
      L = K                                                             M42.6001
      K = KODE(K)                                                       M42.6002
      IF (K .GT. L) GO TO 1870                                          M42.6003
      K = 10000                                                         M42.6004
      GO TO 1870                                                        M42.6005
 1940 I = 1                                                             M42.6006
      IF( IPRSUP .GE. 3 )                                               M42.6007
     1 WRITE(LUNIT6, 1943)  ( KODE(K), NODE(K), K=1, NTOT )             M17. 492
 1943 FORMAT ( /,  84H (KODE(I), NODE(I), I=1, NTOT)   AFTER STEADY-STATM42.6008
     1E SWITCH-CHAIN SETUP IN  'OVER9' .   ,/, ( 1X, 20I6 ) )           M42.6009
      K = 0                                                             M42.6010
 1950 IF ( IABS( IFORM(I) )  .NE.  14 )   GO TO 2040                    M31.2436
      IF ( IFORM(I+1)  .EQ.  18 )         GO TO 2040                    M32.3305
      IF ( TSTART(I)  .NE.  5432. )   GO TO 1972                        M31.2437
C     CURRENT SOURCE TREATED AS VOLTAGE SOURCE FOR S.S. :               M31.2438
      J = IABS( NODE(I) )                                               M31.2439
      IF ( IPRSUP  .GE.  1 )                                            M31.2440
     1 WRITE (LUNIT6, 1967)  I, K, J, KODE(J), KOWNT(J)                 M31.2441
 1967 FORMAT ( 37H CHARGE. I, K, J, KODE(J), KOWNT(J) =, 5I6 )          M31.2442
      GO TO 1980                                                        M31.2443
 1972 IF( TSTART(I).GE. 0.0) GO TO 2040                                 M31.2444
      J = IABS(NODE(I))                                                 M42.6011
      IF( NODE(I) .LE. 0 )  GO TO 2040                                  M42.6012
 1980 IF( KODE(J) .GT. 0 )  GO TO 2020                                  M42.6013
      IF( KODE(J) .LT. 0 )  GO TO 2000                                  M42.6014
      KODE(J) = -J                                                      M42.6015
      KOWNT(J) = -1                                                     M42.6016
      K = K + 1                                                         M42.6017
      GO TO 2040                                                        M42.6018
 2000 ISTATE = 2000                                                     M42.6019
      JJ = NODE(I)                                                      M42.6020
      WRITE (LUNIT6,2001)   BUS(JJ)                                     M17. 493
 2001 FORMAT( /,  74H NOTICE. ---- TWO OR MORE SINUSOIDAL VOLTAGE SOURCEM42.6021
     1S ARE PRESENT ON NODE ', A6,  25H' DURING THE STEADY-STATE  ,/,   M42.6022
     2 14X, 106HSOLUTION.   AS PER THE USER'S MANUAL, THESE VOLTAGES WILM42.6023
     3L ALL BE ADDED TOGETHER TO GET A TOTAL NODE VALUE.   )            M42.6024
      GO TO 2040                                                        M42.6025
 2010 J=L                                                               M42.6026
 2020 L = KODE(J)                                                       M42.6027
      KODE(J) = -L                                                      M42.6028
      KOWNT(J) = -1                                                     M42.6029
      K = K + 1                                                         M42.6030
      IF( KODE(L) .GT. 0 )  GO TO 2010                                  M42.6031
 2040 IF( I .GT. KCONST )  GO TO 2050                                   M42.6032
      I = I +1                                                          M42.6033
      IF( I .LE. KCONST )  GO TO 1950                                   M42.6034
      J = 1                                                             M42.6035
      GO TO 1980                                                        M42.6036
 2050 NCURR = NTOT - K                                                  M42.6037
      IF( IPRSUP .GE. 3 )                                               M42.6038
     1 WRITE (LUNIT6, 2052)  ( KOWNT(I), I=1, NTOT )                    M17. 494
 2052 FORMAT ( /,  82H (KOWNT(I), I=1, NTOT)   IN  'OVER9' ,  AFTER KNOWM42.6039
     1N VOLTAGE NODES ARE FORCED LAST.   ,/,  ( 1X, 20I6 ) )            M42.6040
      I = 3 * LBUS                                                      M42.6041
      IF( I .LT. INTINF )  GO TO 2054                                   M42.6042
      KILL = 80                                                         M42.6043
      LSTAT(19) = 2054                                                  M42.6044
      GO TO 9999                                                        M42.6045
 2054 II = 2 * LBUS                                                     M42.6046
      IF ( KCONST  .EQ.  0 )   GO TO 2113                               M19. 879
      DO 2110  I=1, KCONST                                              M42.6047
      IF ( IABS( IFORM(I) )  .NE.  14 )   GO TO 2110                    M22.3248
      IF( TSTART(I).GE. 0.0) GO TO 2110                                 M42.6048
      J = IABS(NODE(I))                                                 M42.6049
      IF( NODE(I) .GT. 0 )  GO TO 2110                                  M42.6050
      IF( KODE(J) .GE. 0 )  GO TO 2110                                  M42.6051
      JJ = IABS(NODE(I))                                                M42.6052
      WRITE (LUNIT6,2101)   BUS(JJ)                                     M17. 495
 2101 FORMAT( /,  18H NOTE. ---- NODE ', A6,  80H' HAS BOTH VOLTAGE AND M42.6053
     1CURRENT SOURCES ON IT.   THE CURRENT SOURCE HAS NO EFFECT ,/,     M42.6054
     2   12X,  44HON THE SOLUTION, THEN, AND COULD BE OMITTED.   )      M42.6055
      NODE(I) = NODE(I) - II                                            M42.6056
 2110 CONTINUE                                                          M42.6057
 2113 IF ( KSWTCH  .EQ.  0 )   GO TO 3000                               M19. 880
      DO 2730  II = 1, KSWTCH                                           M42.6058
      IF ( ADELAY(II)  .EQ.  -44444. )   GO TO 2523                     M24. 620
      IF (TCLOSE(II) .GE. 0.0) GO TO 2730                               M42.6059
      IF ( ADELAY(II)  .EQ.  44444. )   GO TO 2730                      M19. 881
 2523 I = KMSWIT(II)                                                    M32.3306
      NDX3 = LSWTCH + II                                                M32.3307
      J = KMSWIT(NDX3)                                                  M32.3308
      IF ( I .GT. J )  I = J                                            M32.3309
 2530 J = IABS(KODE(I))                                                 M42.6060
 2540 L = LOC(I)                                                        M42.6061
      IF (L .NE. 0)   GO TO 2560                                        M42.6062
      ISUBS1 =  IOFKOR+NEXT                                             M16.1408
      NX = KORDER(ISUBS1)                                               M16.1409
      ISUBS1 =  IOFKOL+NEXT                                             M16.1410
      KOLUM(ISUBS1)      = J                                            M16.1411
      ISUBS1 =  IOFKOR+NEXT                                             M16.1412
      KORDER(ISUBS1)      = 0                                           M16.1413
      LOC(I) = NEXT                                                     M42.6063
      NEXT = NX                                                         M42.6064
      IF (KODE(I) .GE. 0)   KOWNT(I) = 1                                M42.6065
      GO TO 2610                                                        M42.6066
 2550 ISUBS1 =  IOFKOR+L                                                M16.1414
      L = KORDER(ISUBS1)                                                M16.1415
 2560 ISUBS1 =  IOFKOL+L                                                M16.1416
      K = KOLUM(ISUBS1)                                                 M16.1417
      IF( K .GT. J )  GO TO 2590                                        M42.6067
      IF( K .EQ. J )  GO TO 2690                                        M42.6068
      ISUBS1 =  IOFKOR+L                                                M16.1418
      IF (KORDER(ISUBS1)   .NE. 0)   GO TO 2550                         M16.1419
      ISUBS1 =  IOFKOR+NEXT                                             M16.1420
      NX = KORDER(ISUBS1)                                               M16.1421
      ISUBS1 =  IOFKOL+NEXT                                             M16.1422
      KOLUM(ISUBS1)      = J                                            M16.1423
      ISUBS1 =  IOFKOR+NEXT                                             M16.1424
      KORDER(ISUBS1)      = 0                                           M16.1425
      ISUBS1 =  IOFKOR+L                                                M16.1426
      KORDER(ISUBS1)   = NEXT                                           M16.1427
      NEXT = NX                                                         M42.6069
      GO TO 2600                                                        M42.6070
 2590 ISUBS1 =  IOFKOR+NEXT                                             M16.1428
      NX = KORDER(ISUBS1)                                               M16.1429
      ISUBS1 =  IOFKOL+NEXT                                             M16.1430
      ISUBS2 =  IOFKOL+L                                                M16.1431
      KOLUM(ISUBS1)      = KOLUM(ISUBS2)                                M16.1432
      ISUBS1 =  IOFKOR+NEXT                                             M16.1433
      ISUBS2 =  IOFKOR+L                                                M16.1434
      KORDER(ISUBS1)      = KORDER(ISUBS2)                              M16.1435
      ISUBS1 =  IOFKOR+L                                                M16.1436
      KORDER(ISUBS1)   = NEXT                                           M16.1437
      ISUBS1 =  IOFKOL+L                                                M16.1438
      KOLUM(ISUBS1)   = J                                               M16.1439
      NEXT = NX                                                         M42.6071
 2600 IF( KOWNT(I)  .NE.  (-1) )  KOWNT(I) = KOWNT(I) + 1               M42.6072
 2610 L = LOC(J)                                                        M42.6073
      IF (L .NE. 0)   GO TO 2630                                        M42.6074
      ISUBS1 =  IOFKOR+NEXT                                             M16.1440
      NX = KORDER(ISUBS1)                                               M16.1441
      ISUBS1 =  IOFKOL+NEXT                                             M16.1442
      KOLUM(ISUBS1)      = I                                            M16.1443
      ISUBS1 =  IOFKOR+NEXT                                             M16.1444
      KORDER(ISUBS1)      = 0                                           M16.1445
      LOC(J) = NEXT                                                     M42.6075
      NEXT = NX                                                         M42.6076
      IF (KODE(J) .GE. 0)   KOWNT(J) = 1                                M42.6077
      GO TO 2690                                                        M42.6078
 2620 ISUBS1 =  IOFKOR+L                                                M16.1446
      L = KORDER(ISUBS1)                                                M16.1447
 2630 ISUBS1 =  IOFKOL+L                                                M16.1448
      K = KOLUM(ISUBS1)                                                 M16.1449
      IF( K .GT. I )  GO TO 2670                                        M42.6079
      IF( K .LT. I )  GO TO 2650                                        M42.6080
      KILL = 15                                                         M42.6081
      LSTAT(19) = 2650                                                  M15. 755
      LSTAT(16) = II                                                    M42.6082
      LSTAT(15) = NEXT                                                  M42.6083
      GO TO 9999                                                        M42.6084
 2650 ISUBS1 =  IOFKOR+L                                                M16.1450
      IF (KORDER(ISUBS1)   .NE. 0)   GO TO 2620                         M16.1451
      ISUBS1 =  IOFKOR+NEXT                                             M16.1452
      NX = KORDER(ISUBS1)                                               M16.1453
      ISUBS1 =  IOFKOL+NEXT                                             M16.1454
      KOLUM(ISUBS1)      = I                                            M16.1455
      ISUBS1 =  IOFKOR+NEXT                                             M16.1456
      KORDER(ISUBS1)      = 0                                           M16.1457
      ISUBS1 =  IOFKOR+L                                                M16.1458
      KORDER(ISUBS1)   = NEXT                                           M16.1459
      NEXT = NX                                                         M42.6085
      GO TO 2680                                                        M42.6086
 2670 ISUBS1 =  IOFKOR+NEXT                                             M16.1460
      NX = KORDER(ISUBS1)                                               M16.1461
      ISUBS1 =  IOFKOL+NEXT                                             M16.1462
      ISUBS2 =  IOFKOL+L                                                M16.1463
      KOLUM(ISUBS1)      = KOLUM(ISUBS2)                                M16.1464
      ISUBS1 =  IOFKOR+NEXT                                             M16.1465
      ISUBS2 =  IOFKOR+L                                                M16.1466
      KORDER(ISUBS1)      = KORDER(ISUBS2)                              M16.1467
      ISUBS1 =  IOFKOR+L                                                M16.1468
      KORDER(ISUBS1)   = NEXT                                           M16.1469
      ISUBS1 =  IOFKOL+L                                                M16.1470
      KOLUM(ISUBS1)   = I                                               M16.1471
      NEXT = NX                                                         M42.6087
 2680 IF( KOWNT(J)  .NE.  (-1) )  KOWNT(J) = KOWNT(J) + 1               M42.6088
 2690 IF (IABS(KODE(J)) .LE. J )  GO TO 2710                            M42.6089
      J = IABS(KODE(J))                                                 M42.6090
      GO TO 2540                                                        M42.6091
 2710 I = IABS(KODE(I))                                                 M42.6092
      IF (I .NE. J) GO TO 2530                                          M42.6093
 2730 CONTINUE                                                          M42.6094
      IF( IPRSUP .GT. 0 )                                               M42.6095
     1WRITE(LUNIT6, 47889)  NEXT, LAST                                  M17. 496
47889 FORMAT ( /,  60H AT END OF  'OVER9' ,  JUST BEFORE STEADY-STATE REM42.6096
     1NUMBERING,      , I5,                                             M42.6097
     1   13H OF TOTAL OF  , I5,   34H CELLS OF KOLUM, KORDER ARE FILLED)M42.6098
 3000 ICAS = 1                                                          M15. 756
      LASTOV = NCHAIN                                                   M15. 757
      NCHAIN = 7                                                        M42.6099
      GO TO 9990                                                        M32.3310
 9999 LASTOV = NCHAIN                                                   M15. 758
      NCHAIN = 51                                                       M15. 759
      LSTAT(18) = 9                                                     M42.6100
 9990 IF ( IPRSUP  .GE.  1 )                                            M32.3311
     1 WRITE ( LUNIT6, 4568 )                                           M32.3312
 4568 FORMAT ( 23H  "EXIT  MODULE OVER9." )                             M32.3313
99999 RETURN                                                            M42.6101
      END                                                               M42.6102
