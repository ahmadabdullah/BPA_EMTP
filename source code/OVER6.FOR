      SUBROUTINE OVER6 
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      REAL*8           BUFF                                             M11. 111
      DIMENSION BUFF(20)                                                M42.4433
      EQUIVALENCE  ( IOFKOL, IOFGND ), ( IOFKOR, IOFBND )               M33. 594
      DIMENSION  INTEGX(1)                                              M33. 595
      EQUIVALENCE  ( X(1), INTEGX(1) )                                  M33. 596
C     FOLLOWING CARRIES "NEXT" AMONG OVER6, INSERT, OVER7, & OVER9:     M37.2893
      EQUIVALENCE  ( LOOPSS(11), NEXT )                                         
      IF ( IPRSUP  .GE.  1 )                                            M32.2934
     1 WRITE ( LUNIT6, 4567 )                                           M32.2935
 4567 FORMAT ( 23H  "BEGIN MODULE OVER6." )                             M32.2936
      NTOT1 = NTOT - 1                                                  M42.4434
      N3 = 0                                                            M42.4435
      D1 = 1.0 / ( 100. * FLZERO )                                      M22.3181
C     1ST REMOVE MINUS SIGN OF "KPARTB" IF PRESENT AS FLAG              M32.2937
C     INDICATING "RENUMBER BYPASS".   THEN REDEFINE VALUE OF            M32.2938
C     THIS LOFTY RESISTANCE IF USER HAD A "HIGH RESISTANCE"             M32.2939
C     REQUEST (KNOWN BY N7 .NE. DEFAULT VALUE OF 1000 IN OVER1):        M32.2940
      N7 = IABS ( KPARTB )                                              M32.2941
      IF ( N7  .LT.  1000 )                                             M32.2942
     1 D1 = 10.0 ** N7                                                  M32.2943
      I = INONL                                                         M42.4436
54147 IF( I .EQ. 0 )  GO TO 54180                                       M42.4437
      IF( NLTYPE(I) .GT. 0 )  GO TO 54174                               M42.4438
      K = NONLK(I)                                                      M42.4439
      M = IABS( NONLM(I) )                                              M42.4440
      DO 54155  J=1, IBR                                                M42.4441
      N1 = IABS( KBUS(J) )                                              M42.4442
      N2 = IABS( MBUS(J) )                                              M42.4443
      IF( N1 .NE. K )  GO TO 54154                                      M42.4444
      IF( N2 .EQ. M )  GO TO 54174                                      M42.4445
      GO TO 54155                                                       M42.4446
54154 IF( N2 .NE. K )  GO TO 54155                                      M42.4447
      IF( N1 .EQ. M )  GO TO 54174                                      M42.4448
54155 CONTINUE                                                          M42.4449
      IBR = IBR + 1                                                     M42.4450
      IF ( IBR  .LE.  LBRNCH )   GO TO 4716                             M16.1103
      LSTAT(19) = 4716                                                  M16.1104
      KILL = 1                                                          M16.1105
      LSTAT(16) = 2                                                     M16.1106
      GO TO 9200                                                        M16.1107
 4716 KBUS(IBR) = K                                                     M16.1108
      MBUS(IBR) = M                                                     M42.4451
      IF( N3 .GT. 0 )  GO TO 54162                                      M42.4452
      J1 = K                                                            M42.4453
      J2 = M                                                            M42.4454
      IT = IT + 1                                                       M42.4455
      TR(IT) = D1                                                       M22.3184
      TX(IT) = 0.0                                                      M42.4456
      C(IT) = 0.0                                                       M42.4457
54162 NR(IBR) = - IT                                                    M42.4458
      LENGTH(IBR) = 1                                                   M42.4459
      IF(N3 .GT. 0)LENGTH(IBR) = -1                                     M42.4460
      N7 = -NLTYPE(I)                                                   M20.2133
      IF ( N7  .NE.  98 )  N7 = 99                                      M21.  44
      IF ( NOUTPR  .EQ.  0   .AND.                                      M35.1069
     1     N3  .GT.  0 )                                                M35.1070
     2 WRITE (LUNIT6, 54164)  N7, I, BUS(K), BUS(M),                    M35.1071
     3                               BUS(J1), BUS(J2)                   M35.1072
54164 FORMAT (  37H HIGH RES. ADDED BY EMTP ACROSS TYPE-,  I2,          M20.2136
     1 6H ELEM.,  I3,  1X,  2X,  3H1 0,  4A6  )                         M35.1073
      IF ( N3  .EQ.  0     .AND.                                        M20.2140
     1     NOUTPR  .EQ.  0 )                                            M20.2141
     2 WRITE (LUNIT6, 54165)  N7, I, BUS(K), BUS(M), TR(IT)             M35.1074
54165 FORMAT (  37H HIGH RES. ADDED BY EMTP ACROSS TYPE-,  I2,          M35.1075
     1 6H ELEM.,  I3,  1X,  2X,  3H1 0,  2A6, 12X, E6.1 )               M35.1076
      N3 = 1                                                            M42.4461
54174 I = I - 1                                                         M42.4462
      IF( I .GT. 0 )  GO TO 54147                                       M42.4463
54180 LSTAT(24) = KCONST                                                M42.4464
      LSTAT(23) = IT                                                    M32.2944
      IT1 = IBR                                                         M31. 902
      IV  =  LOCINT( C(1) )     -     LOCINT( X(1) )                    M32.2945
      IF ( KBURRO  .EQ.  1 )   GO TO 4802                               M22.3195
      LAST = ( IV - 1 ) / ( 3 * NBYTE(3) / NBYTE(4) )                   M32.2946
      LSTAT(14) = LSIZ23                                                        
      LSIZ23 = LAST                                                     M22.3198
      IOFGND = LAST                                                     M22.3199
      IOFBND = 2 * LAST                                                 M22.3200
      GO TO 4803                                                        M22.3201
 4802 LAST = LSIZ23                                                     M28.4249
      IOFGND = 0                                                        M22.3203
      IOFBND = 0                                                        M22.3204
 4803 N11 = 1                                                           M32.2947
      IF( IPRSUP .GE. 1 )                                               M32.2948
     1 WRITE (LUNIT6, 55414)  KBURRO, IV, LAST                          M32.2949
55414 FORMAT ( /,  21H IN "OVER6", KBURRO =,  I3,                       M32.2950
     1             23H .   COMPUTE IV, LAST =,  2I8  )                  M32.2951
      REWIND LUNIT2                                                     M34. 123
      CALL TAPSAV ( INTEGX(1), LUNIT2, IV, N11 )                        M34. 124
      N17 = 0                                                           M32.2953
      CALL VECRSV ( VOLT(1), N17, N17 )                                 M32.2954
      CALL VECRSV (  C(1), IT, N11 )                                    M32.2955
      CALL VECRSV ( TR(1), IT, N11 )                                    M32.2956
      CALL VECRSV ( TX(1), IT, N11 )                                    M32.2957
      CALL VECRSV (  R(1), IT, N11 )                                    M32.2958
      CALL VECISV (     NR(1), IBR, N11 )                               M32.2959
      CALL VECISV ( LENGTH(1), IBR, N11 )                               M32.2960
      KTRLSW(7) = IT                                                    M32.2961
      KTRLSW(8) = IBR                                                   M32.2962
      CALL MOVE0 ( KOLUM(1), IBR )                                      M32.2963
C     OUTPUT NETWORK-CONNECTIVITY TABLE IF SO REQUESTED (IDOUBL=1).     M42.4465
      IF( IDOUBL  .LE.  0 )  GO TO 5324                                 M31. 904
      CALL MOVE0 ( LOC(1), NTOT )                                       M42.4466
      NEXT = 1                                                          M42.4467
      I = 1                                                             M42.4468
 5010 IF( I .GT. IBR )  GO TO 5040                                      M42.4469
      K = IABS( KBUS(I) )                                               M42.4470
      M = IABS( MBUS(I) )                                               M42.4471
      IF( BUS(K) .EQ. TRASH )  GO TO 5020                               M42.4472
      IF( BUS(M) .EQ. TRASH )  GO TO 5020                               M42.4473
      ASSIGN 5020 TO MOON                                               M42.4474
      GO TO 5200                                                        M42.4475
 5020 I = I + 1                                                         M42.4476
      GO TO 5010                                                        M42.4477
 5040 I = 1                                                             M42.4478
 5045 IF( I .GT. INONL )  GO TO 5060                                    M42.4479
      K = IABS( NONLK(I) )                                              M42.4480
      M = IABS( NONLM(I) )                                              M42.4481
      ASSIGN 5050 TO MOON                                               M42.4482
      GO TO 5200                                                        M42.4483
 5050 I = I + 1                                                         M42.4484
      GO TO 5045                                                        M42.4485
 5060 I = 1                                                             M42.4486
 5065 IF( I .GT. KSWTCH )  GO TO 5250                                   M42.4487
      NDX1 = LSWTCH + I                                                 M32.2964
      K = KMSWIT(I)                                                     M32.2965
      M = KMSWIT(NDX1)                                                  M32.2966
      LTEST = ISOURC(I)                                                 M42.4488
      IF( LTEST .LE. 0 )  GO TO 5080                                    M42.4489
      K = IABS( NODE(LTEST+1) )                                         M42.4490
 5080 IF( BUS(K) .EQ. TRASH )  GO TO 5085                               M42.4491
      IF( BUS(M) .EQ. TRASH )  GO TO 5085                               M42.4492
      ASSIGN 5085 TO MOON                                               M42.4493
      GO TO 5200                                                        M42.4494
 5085 I = I + 1                                                         M42.4495
      GO TO 5065                                                        M42.4496
 5200 IF( K .GT. NTOT  .OR.  M .GT. NTOT )  GO TO 5240                  M42.4497
      IPASS = 0                                                         M42.4498
 5204 LLEFT = LOC(K)                                                    M42.4499
      IF( LLEFT .GT. 0 )  GO TO 5214                                    M42.4500
      LTEST = 0                                                         M42.4501
 5208 LOC(K) = NEXT                                                     M42.4502
      GO TO 5235                                                        M42.4503
 5214 ISUBS1 =  IOFKOL+LLEFT                                            M16.1109
      JLEFT = KOLUM(ISUBS1)                                             M16.1110
      IF( M .GT. JLEFT )  GO TO 5222                                    M42.4504
      LTEST = LLEFT                                                     M42.4505
      GO TO 5208                                                        M42.4506
 5222 ISUBS1 =  IOFKOR+LLEFT                                            M16.1111
      LTEST = KORDER(ISUBS1)                                            M16.1112
      IF( LTEST .EQ. 0 )  GO TO 5228                                    M42.4507
      ISUBS1 =  IOFKOL+LTEST                                            M16.1113
      JTEST = KOLUM(ISUBS1)                                             M16.1114
      IF( JTEST .GE. M )  GO TO 5228                                    M42.4508
      LLEFT = LTEST                                                     M42.4509
      GO TO 5222                                                        M42.4510
 5228 ISUBS1 =  IOFKOR+LLEFT                                            M16.1115
      KORDER(ISUBS1)       = NEXT                                       M16.1116
 5235 ISUBS1 =  IOFKOL+NEXT                                             M16.1117
      KOLUM(ISUBS1)      = M                                            M16.1118
      ISUBS1 =  IOFKOR+NEXT                                             M16.1119
      KORDER(ISUBS1)      = LTEST                                       M16.1120
      IF ( NEXT  .LT.  LAST )   GO TO 5237                              M28.4252
      KILL = 192                                                        M19. 774
      LSTAT(19) = 5237                                                  M19. 775
      GO TO 9200                                                        M19. 776
 5237 NEXT = NEXT + 1                                                   M19. 777
      IF( IPASS .GT. 0 )  GO TO 5240                                    M42.4511
      IPASS = K                                                         M42.4512
      K = M                                                             M42.4513
      M = IPASS                                                         M42.4514
      GO TO 5204                                                        M42.4515
 5240 GO TO MOON, ( 5020, 5050, 5085)                                   M42.4516
 5250 WRITE(LUNIT6, 5254)                                               M17. 429
 5254 FORMAT( //,  46H LIST OF INPUT ELEMENTS CONNECTED TO EACH BUS.    M42.4517
     1 ,/, 10X, 106H1) ONLY THE PHYSICAL CONNECTIONS OF MULTIPHASE LINESM42.4518
     2 ARE SHOWN (CAPACITIVE AND INDUCTIVE COUPLING IGNORED)    ,/,     M42.4519
     3 10X, 46H2) REPEATED ENTRIES IMPLY PARALLEL CONNECTIONS   ,/,     M42.4520
     4 10X,  41H3) SOURCES ARE OMITTED, ALTHOUGH SWITCHES,              M33. 597
     5       14H ARE INCLUDED;    ,/,                                   M32.2968
     6 10X,  41H4) U.M. USAGE PRODUCES EXTRA, INTERNALLY-,              M32.2969
     7       44HDEFINED NODES "UM????" (1ST 2 LETTERS "UM").  )         M32.2970
      WRITE(LUNIT6, 5261)                                               M17. 430
 5261 FORMAT( 45H FROM BUS NAME 1 NAMES OF ALL ADJACENT BUSSES )        M28.4253
      WRITE(LUNIT6, 5266)                                               M17. 431
 5266 FORMAT(  122H --------------+-------------------------------------M35.1077
     1------------------------------------------------------------------M42.4521
     2---    )                                                          M42.4522
      N22 = KWTSPY                                                              
      IPASS = 0                                                         M42.4523
      K = 2                                                             M42.4524
 5268 IF( LOC(K) .EQ. 0 )  GO TO 5294                                   M42.4525
      BUS5 = BUS(K)                                                     M28.4256
 5270 LTEST = LOC(K)                                                    M42.4526
      IF( LTEST  .EQ.  0 )  GO TO 5294                                  M42.4527
 5272 ISUBS1 =  IOFKOL+LTEST                                            M16.1121
      M = KOLUM(ISUBS1)                                                 M16.1122
      IPASS = IPASS + 1                                                 M42.4528
      BUFF(IPASS) = BUS(M)                                              M42.4529
      IF( M .EQ. 1 )  BUFF(IPASS) = TERRA                               M42.4530
      IF( IPASS .LT. 15 )  GO TO 5285                                   M42.4531
 5277 WRITE(LUNIT6, 5278)  BUS5, (BUFF(M), M=1, IPASS)                  M28.4257
 5278 FORMAT( 9X, A6, 1H1, 15(A6, 1H* )  )                              M28.4258
      BUS5 = BLANK                                                      M28.4259
      IPASS = 0                                                         M42.4532
      IF( LTEST .EQ. 0 )  GO TO 5294                                    M42.4533
 5285 ISUBS1 =  IOFKOR+LTEST                                            M16.1123
      LTEST = KORDER(ISUBS1)                                            M16.1124
      IF( LTEST .GT. 0 )  GO TO 5272                                    M42.4534
      GO TO 5277                                                        M42.4535
 5294 K = K + 1                                                         M42.4536
      IF ( IDOUBL .EQ. 0 )  GO TO 5324                                          
      IF ( M4PLOT .EQ. 1 ) CALL EMTSPY                                          
      IF( K .EQ. 2 )  GO TO 5320                                        M42.4537
      IF( K .LE. NTOT )  GO TO 5268                                     M42.4538
      BUS5 = TERRA                                                      M28.4260
      K = 1                                                             M42.4539
      GO TO 5270                                                        M42.4540
 5320 WRITE(LUNIT6, 5266)                                               M17. 438
      WRITE (LUNIT6, 1337)                                              M28.4261
      IF ( N22 .GE. 2 )                                                         
     1 KWTSPY = 1                                                               
 5324 IF( TMAX .GT. 0.0 )  GO TO 5344                                   M32.2971
      DO 5340  I = 1, NTOT                                              M32.2972
      ICH1(I) = I                                                       M32.2973
 5340 NORDER(I) = I                                                     M32.2974
      GO TO 40013                                                       M32.2975
C     RENUMBER NODES BASED ON SPARSITY OF COEFF MATRIX OF TRANSIENT     M42.4541
C     NETWORK SOLUTION.                                                 M42.4542
 5344 IF( IPRSUP .GT. 0 )                                               M32.2976
     1WRITE(LUNIT6, 73524)  ( BUS(I), I=1, NTOT )                       M17. 440
73524 FORMAT( /, 10H BUS NAMES   ,/, ( 1X, 10A12 ) )                    M42.4543
      IF ( KPARTB  .GT.  0 )   GO TO 5351                               M32.2977
C     NEGATIVE SIGN ON "KPARTB" IS FOR "RENUMBER BYPASS":               M32.2978
      NORDER(1) = 1                                                     M32.2979
      N9 = 1                                                            M32.2980
      DO 5345  I=2, NTOT                                                M32.2981
      IF ( KODE(I) .LT. 0 )  GO TO 5345                                 M32.2982
      N9 = N9 + 1                                                       M32.2983
      NORDER(I) = N9                                                    M32.2984
      ICH1(N9) = I                                                      M32.2985
 5345 CONTINUE                                                          M32.2986
      DO 5346  I=2, NTOT                                                M32.2987
      IF ( KODE(I) .EQ. 0 )  GO TO 5346                                 M32.2988
      IF ( TSTART(I) .NE. -9988. )  GO TO 5346                          M32.2989
      N9 = N9 + 1                                                       M32.2990
      NORDER(I) = N9                                                    M32.2991
      ICH1(N9) = I                                                      M32.2992
 5346 CONTINUE                                                          M32.2993
      KPARTB = N9                                                       M33. 598
      DO 5348 I = 2, NTOT                                               M32.2994
      IF ( KODE(I) .EQ. 0 ) GO TO 5348                                  M32.2995
      IF ( TSTART(I) .EQ. -9988. )  GO TO 5348                          M32.2996
      N9 = N9 + 1                                                       M32.2997
      NORDER(I) = N9                                                    M32.2998
      ICH1(N9) = I                                                      M32.2999
 5348 CONTINUE                                                          M32.3000
      GO TO 40013                                                       M19. 781
 5351 NEXT = 1                                                          M19. 782
      NZ = 0                                                            M42.4544
      CALL MOVE0 ( LOC(1), NTOT )                                       M42.4545
      CALL MOVE0 ( KOWNT(1), NTOT )                                     M42.4546
      I = 1                                                             M42.4547
41001 L = IABS(LENGTH(I))                                               M42.4548
C      IF (KODSEM(I) .NE. 0 .AND. IMODEL(I) .NE. -2                     M31. 905
C     1                     .AND. IMODEL(I) .NE. -4) L = IABS(KODEBR(I))M43. 850
      IF (KODSEM(I) .NE. 0 .AND. IMODEL(I) .GE. 0) L = IABS(KODEBR(I))  M43. 851
      IF (L.GT.1) GO TO 41002                                           M42.4549
      CALL INSERT(KBUS(I),IABS(MBUS(I)))                                M42.4550
      GO TO 40004                                                       M42.4551
41002 L1 = I+L-2                                                        M42.4552
      L3 = L1+1                                                         M42.4553
      DO 40014 J1=I,L1                                                  M42.4554
      K = IABS(KBUS(J1))                                                M42.4555
      M = IABS(MBUS(J1))                                                M42.4556
      L2 = J1+1                                                         M42.4557
      DO 40014 J2=L2,L3                                                 M42.4558
      CALL INSERT(K,IABS(KBUS(J2)) )                                    M42.4559
      CALL INSERT(M,IABS(MBUS(J2)) )                                    M42.4560
40014 CONTINUE                                                          M42.4561
      IF (KBUS(I).LT.0) GO TO 40004                                     M42.4562
      DO 40024 J1=I,L3                                                  M42.4563
      K = IABS(KBUS(J1))                                                M42.4564
      DO 40024 J2=I,L3                                                  M42.4565
      CALL INSERT(K,IABS(MBUS(J2)) )                                    M42.4566
40024 CONTINUE                                                          M42.4567
40004 IF ( KILL  .EQ.  0 )   GO TO 5372                                 M21.  45
      LSTAT(19) = 5372                                                  M21.  46
      LSTAT(14) = I                                                     M21.  47
      GO TO 9200                                                        M21.  48
 5372 I = I + L                                                         M21.  49
      IF (I.LE.IBR) GO TO 41001                                         M42.4568
      IF( NUM99 .LE. 0 )  GO TO 73535                                   M42.4569
      DO 73530  I=1, INONL                                              M42.4570
      IF( NLTYPE(I) .GT. 0 )  GO TO 73530                               M42.4571
      K = NONLK(I)                                                      M42.4572
      M = IABS( NONLM(I) )                                              M42.4573
      CALL INSERT( K, M )                                               M42.4574
73530 CONTINUE                                                          M42.4575
73535 CONTINUE                                                          M42.4576
      IF( IPRSUP .GE. 1 )                                               M42.4577
     1 WRITE (LUNIT6, 1335)  NZ, IOFKOL, IOFKOR                         M19. 783
 1335 FORMAT ( /,  37H IN  'OVER6' ,   NZ, IOFKOL, IOFKOR =, 3I10, /,   M42.4578
     1 1X, 38H   ROW            KODE           KOLUM,                   M43.   2
     2           32H           KODER           KOWNT  )                 M43.   3
      DO 1339  I=1, 10                                                  M19. 785
      IL = I + IOFKOL                                                   M19. 786
      IR = I + IOFKOR                                                   M19. 787
      IF ( IPRSUP  .GE.  3 )                                            M19. 788
     1 WRITE (LUNIT6, 1337) I,KODE(I),KOLUM(IL),KORDER(IR),KOWNT(I)     M32.3002
 1337 FORMAT ( I6, 4I16 )                                               M43.   4
 1339 CONTINUE                                                          M19. 791
      IF (KSWTCH.LT.1) GO TO 40015                                      M42.4579
      DO 40005 I=1,KSWTCH                                               M42.4580
      NDX2 = LSWTCH + I                                                 M32.3003
      K = KMSWIT(I)                                                     M32.3004
      M = KMSWIT(NDX2)                                                  M32.3005
      CALL INSERT(K,M)                                                  M42.4581
40005 CONTINUE                                                          M42.4582
40015 DO 40006 I=2,NTOT                                                 M42.4583
      IF (KODE(I).EQ.0) GO TO 40006                                     M32.3006
      IF(KOWNT(I).EQ.(-1)) GO TO 40006                                  M42.4584
      NZ = NZ+1                                                         M42.4585
      KOWNT(I)=-1                                                       M42.4586
40006 CONTINUE                                                          M42.4587
      KOWNT(1) = 0                                                      M42.4588
      IF ( KILL  .EQ.  0 )   GO TO 5379                                 M21.  50
      LSTAT(19) = 5379                                                  M21.  51
      LSTAT(14) = 0                                                     M21.  52
      GO TO 9200                                                        M21.  53
 5379 KPARTB = NTOT - NZ                                                M32.3007
      DO 40026  I=NEXT, LAST                                            M32.3008
      ISUBS1 =  IOFKOL+I                                                M16.1125
      KOLUM(ISUBS1)   = 0                                               M16.1126
      ISUBS1 =  IOFKOR+I                                                M16.1127
40026 KORDER(ISUBS1)   = I+1                                            M16.1128
      ISUBS1 =  IOFKOR+LAST                                             M16.1129
      KORDER(ISUBS1)      = 0                                           M16.1130
      ICAS = 0                                                          M42.4589
      IF( IPRSUP .GE. 2 )                                               M42.4590
     1 WRITE(LUNIT6, 54230)  NEXT, LAST, NTOT, KSWTCH, KCONST,          M28.4262
     2  NV, IBR, INONL,  NPOWER, IT, ISTEAD, NUM99,  NZ, IV             M28.4263
54230 FORMAT ( /,  29H SCALARS AT END OF  'OVER6' .   ,/,               M42.4591
     1   40H    NEXT    LAST    NTOT  KSWTCH  KCONST,                   M28.4264
     2   40H      NV     IBR   INONL  NPOWER      IT,                   M28.4265
     3   32H  ISTEAD   NUM99      NZ      IV   ,/,  1X, 14I8 )          M28.4266
      NCURR = NTOT - NZ                                                 M42.4592
      LASTOV = NCHAIN                                                   M15. 634
      NCHAIN = 7                                                        M42.4593
      GO TO 9988                                                        M32.3009
40013 LASTOV = NCHAIN                                                   M15. 635
      NCHAIN = NCHAIN + 2                                               M15. 636
      IF ( IPRSUP  .GE.  1 )                                            M32.3010
     1 WRITE ( LUNIT6, 4568 )                                           M32.3011
      GO TO 99999                                                       M16.1131
 9200 LASTOV = NCHAIN                                                   M16.1132
      LSTAT(18) = 6                                                     M16.1133
      NCHAIN = 51                                                       M16.1134
 9988 IF ( IPRSUP  .GE.  1 )                                            M32.3012
     1 WRITE ( LUNIT6, 4568 )                                           M32.3013
 4568 FORMAT ( 23H  "EXIT  MODULE OVER6." )                             M32.3014
99999 RETURN                                                            M42.4594
      END                                                               M42.4595
      SUBROUTINE RINFIN                                                 VAX. 923
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      N3 = 0                                                            M42.4435
      D1 = 1.0 / ( 100. * FLZERO )                                      M22.3181
C     1ST REMOVE MINUS SIGN OF "KPARTB" IF PRESENT AS FLAG              M32.2937
C     INDICATING "RENUMBER BYPASS".   THEN REDEFINE VALUE OF            M32.2938
C     THIS LOFTY RESISTANCE IF USER HAD A "HIGH RESISTANCE"             M32.2939
C     REQUEST (KNOWN BY N7 .NE. DEFAULT VALUE OF 1000 IN OVER1):        M32.2940
      N7 = IABS ( KPARTB )                                              M32.2941
      IF ( N7  .LT.  1000 )                                             M32.2942
     1 D1 = 10.0 ** N7                                                  M32.2943
      I = INONL                                                         M42.4436
54147 IF( I .EQ. 0 )  GO TO 54180                                       M42.4437
      IF( NLTYPE(I) .GT. 0 )  GO TO 54174                               M42.4438
      K = NONLK(I)                                                      M42.4439
      M = IABS( NONLM(I) )                                              M42.4440
      DO 54155  J=1, IBR                                                M42.4441
      N1 = IABS( KBUS(J) )                                              M42.4442
      N2 = IABS( MBUS(J) )                                              M42.4443
      IF( N1 .NE. K )  GO TO 54154                                      M42.4444
      IF( N2 .EQ. M )  GO TO 54174                                      M42.4445
      GO TO 54155                                                       M42.4446
54154 IF( N2 .NE. K )  GO TO 54155                                      M42.4447
      IF( N1 .EQ. M )  GO TO 54174                                      M42.4448
54155 CONTINUE                                                          M42.4449
      IBR = IBR + 1                                                     M42.4450
      IF ( IBR  .LE.  LBRNCH )   GO TO 4716                             M16.1103
      LSTAT(19) = 4716                                                  M16.1104
      KILL = 1                                                          M16.1105
      LSTAT(16) = 2                                                     M16.1106
C      GO TO 9200                                                       VAX. 924
 4716 KBUS(IBR) = K                                                     M16.1108
      MBUS(IBR) = M                                                     M42.4451
      IF( N3 .GT. 0 )  GO TO 54162                                      M42.4452
      J1 = K                                                            M42.4453
      J2 = M                                                            M42.4454
      IT = IT + 1                                                       M42.4455
      TR(IT) = D1                                                       M22.3184
      TX(IT) = 0.0                                                      M42.4456
      C(IT) = 0.0                                                       M42.4457
54162 NR(IBR) = - IT                                                    M42.4458
      LENGTH(IBR) = 1                                                   M42.4459
      IF(N3 .GT. 0)LENGTH(IBR) = -1                                     M42.4460
      N7 = -NLTYPE(I)                                                   M20.2133
      IF ( N7  .NE.  98 )  N7 = 99                                      M21.  44
      IF ( NOUTPR  .EQ.  0   .AND.                                      M35.1069
     1     N3  .GT.  0 )                                                M35.1070
     2 WRITE (LUNIT6, 54164)  N7, I, BUS(K), BUS(M),                    M35.1071
     3                               BUS(J1), BUS(J2)                   M35.1072
54164 FORMAT (  37H HIGH RES. ADDED BY EMTP ACROSS TYPE-,  I2,          M20.2136
     1 6H ELEM.,  I3,  1X,  2X,  3H1 0,  4A6  )                         M35.1073
      IF ( N3  .EQ.  0     .AND.                                        M20.2140
     1     NOUTPR  .EQ.  0 )                                            M20.2141
     2 WRITE (LUNIT6, 54165)  N7, I, BUS(K), BUS(M), TR(IT)             M35.1074
54165 FORMAT (  37H HIGH RES. ADDED BY EMTP ACROSS TYPE-,  I2,          M35.1075
     1 6H ELEM.,  I3,  1X,  2X,  3H1 0,  2A6, 12X, E6.1 )               M35.1076
      N3 = 1                                                            M42.4461
54174 I = I - 1                                                         M42.4462
      IF( I .GT. 0 )  GO TO 54147                                       M42.4463
54180 RETURN                                                            M42.4464
      END                                                               VAX. 925
      SUBROUTINE INSERT(IRRR,ICC)                                       M42.4596
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
C     FOLLOWING CARRIES "NEXT" AMONG OVER6, INSERT, OVER7, & OVER9:     M37.2895
      EQUIVALENCE  ( LOOPSS(11), NEXT )                                         
      IF( IRRR .LE. 1 )  RETURN                                         M42.4599
      IF( ICC  .LE. 1 )  RETURN                                         M42.4600
      IF( IRRR .EQ. ICC )  RETURN                                       M42.4601
      IF ( KILL  .GT.  0 )   RETURN                                     M21.  55
      I = 0                                                             M42.4602
      IR = IRRR                                                         M42.4603
      IRR = IRRR                                                        M42.4604
      IC = ICC                                                          M42.4605
   99 IRN = LOC(IR)                                                     M42.4606
      IF (IRN.EQ.0) GO TO 1                                             M42.4607
      N2 = IRN + IOFGND                                                 M33. 599
      N1 = IC - KOLUM(N2)                                               M19. 794
      IF( N1 .GT. 0 )  GO TO 2                                          M42.4608
      IF( N1 .EQ. 0 )  GO TO 5                                          M42.4609
    1 LOC(IR) = NEXT                                                    M42.4610
      GO TO 4                                                           M42.4611
    2 IR = IRN                                                          M42.4612
      ISUBS1 =  IOFBND+IR                                               M33. 600
      IRN = KORDER(ISUBS1)                                              M16.1136
      IF (IRN.EQ.0) GO TO 3                                             M42.4613
      N2 = IRN + IOFGND                                                 M33. 601
      N1 = IC - KOLUM(N2)                                               M19. 796
      IF( N1 .GT. 0 )  GO TO 2                                          M42.4614
      IF( N1 .EQ. 0 )  GO TO 5                                          M42.4615
    3 ISUBS1 =  IOFBND+IR                                               M33. 602
      KORDER(ISUBS1)    = NEXT                                          M16.1138
    4 ISUBS1 =  IOFBND+NEXT                                             M33. 603
      KORDER(ISUBS1)      = IRN                                         M16.1140
      ISUBS1 =  IOFGND+NEXT                                             M33. 604
      KOLUM(ISUBS1)      = IC                                           M16.1142
      IF ( NEXT  .LT.  LAST )   GO TO 2486                              M21.  56
      KILL = 200                                                        M21.  57
      RETURN                                                            M21.  58
 2486 NEXT = NEXT + 1                                                   M21.  59
      IF( KOWNT(IRR)  .LT.  0 )  GO TO 5                                M42.4616
      KOWNT(IRR) = KOWNT(IRR)+1                                         M42.4617
    5 IF (I.EQ.1) RETURN                                                M42.4618
      I = 1                                                             M42.4619
      IR = ICC                                                          M42.4620
      IRR = ICC                                                         M42.4621
      IC = IRRR                                                         M42.4622
      GO TO 99                                                          M42.4623
      END                                                               M42.4624
