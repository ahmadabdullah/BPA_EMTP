      SUBROUTINE SUBR45                                                 M22.6282
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     CODE CONNECTED TO  'SEMLYEN SETUP'  SPECIAL REQUEST CARD.         M15.1668
C)  THIS OVERLAY IS USED TO CALCULATE                                   M16.4556
C)     1)  PROPAGATION STEP RESPONSE AS APPROXIMATED BY AN ANALYTIC     M16.4557
C)         (TWO EXPONEBTIALS AND A CONSTANT) FUNCTION.                  M16.4558
C)     2)  ADMITTANCE STEP RESPONSE AS APPROXIMATED BY AN ANALYTIC      M16.4559
C)         (TWO EXPONENTIALS AND A CONSTANT) FUNCTION.                  M16.4560
C)     3)  STEADY STATE MODAL DISTRIBUTED IMPEDANCE AND ADMITTANCE OF   M16.4561
C)         THE LINE.                                                    M16.4562
C)     4)  PHASE TO MODE TRANSFORMATION MATRICES.                       M16.4563
C)  THIS ROUTINE PUNCHES THE BRANCH CARDS TO BE READ IN AS DATA WHEN    M16.4564
C)  USING THE 'SEMLYEN RECURSIVE CONVOLUTION' FREQUENCY DEPENDENT LINE. M16.4565
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK45.FOR;/NOLIST'                                           
      INCLUDE  'LABL45.FOR;/NOLIST'                                           
      DIMENSION STG(11)                                                 M39.3617
      DIMENSION  LLTEMP(20)                                             M22.6284
      DOUBLE PRECISION ZIFT(11)                                         M39.3618
      EQUIVALENCE( ZIFT(1), KARRAY(1) )                                 M35.9089
      EQUIVALENCE ( STG(1), KARRAY(1) )                                 M22.6285
      EQUIVALENCE (INDTV(1),NPOINT)                                     M20.5087
      IF (LASTOV .EQ. 1) GO TO 20                                       M20.5088
      NPH = KTAB                                                        M39.3619
      NTRI = NPH*(NPH + 1)/2                                            M20.5090
      NPHSQ = NPH * NPH                                                 M20.5091
      NPH2 = NPH + NPH                                                  M20.5092
      NPHPI2 = NPH2 + 2                                                 M20.5093
      N22 = NPHSQ + NPHSQ                                               M20.5094
      D13 = VOLTBC(2) / VOLTBC(1)                                       M31.6684
      NFR = ALOGZ(D13) / ALOGZ(VOLTBC(3)) + 1.5                         M31.6685
      NFR1 = NFR + 1                                                    M20.5096
      N8 = NCHAIN                                                       M28.7228
      IF ( KBURRO .EQ. 1)  N8 =29                                       M28.7229
      CALL DIMENS (LLTEMP(1),N8,TRASH,TRASH)                            M28.7230
      DO 5654  I=1, 9999, 2                                             M35.9090
      IF ( LLTEMP(I)  .NE.  0 )   GO TO 5654                            M22.6290
      LSEM = LLTEMP(I+1) * NBYTE(4)/NBYTE(3)                            M28.7231
      GO TO 5655                                                        M35.9091
 5654 CONTINUE                                                          M22.6292
      CALL STOPTP                                                               
 5655 LX = NPH * ( 22*NPH + 3*NFR1 + 30 ) + 4*NFR1 + 3*NPOINT + 3       M35.9093
      IF (NPH .LE. 50 .AND. LX .LT. LSEM) GO TO 10                      M20.5099
      KILL = 198                                                        M20.5100
      LSTAT(15) = LSEM                                                  M20.5101
      LSTAT(16) = LX                                                    M20.5102
      LSTAT(19) = 10                                                    M20.5103
      LSTAT(18) = 45                                                    M20.5104
      LASTOV = NCHAIN                                                   M20.5105
      NCHAIN = 51                                                       M20.5106
      GO TO 9999                                                        M29.5409
   10 LPBUF = 1                                                         M22.6293
      LCQ = LPBUF + 14*NPH                                              M20.5109
      LZ = LCQ + N22                                                    M20.5110
      LY = LZ + N22                                                     M20.5111
      LZY = LY + N22                                                    M20.5112
      LZYA = LZY + N22                                                  M20.5113
      LZYB = LZYA + N22                                                 M20.5114
      LZYC = LZYB + N22                                                 M20.5115
      LZYD = LZYC + N22                                                 M20.5116
      LCQT = LZYD + N22                                                 M20.5117
      LQ = LCQT + N22                                                   M20.5118
      LQI = LQ + NPHSQ                                                  M20.5119
      LG = LQI + NPHSQ                                                  M20.5120
      LG60 = LG + NPH2                                                  M20.5121
      LYO = LG60 + NPH2                                                 M20.5122
      LXR = LYO + NPH2                                                  M20.5123
      LXL = LXR + NTRI                                                  M20.5124
      LXG = LXL + NTRI                                                  M20.5125
      LXC = LXG + NTRI                                                  M20.5126
      LUM = LXC + NTRI                                                  M20.5127
      LSI = LUM + 3*NPH2                                                M20.5128
      LVRESP = LSI + NFR1 + 1                                           M20.5129
      LZIFT = LVRESP / 2 + 1                                            M35.9094
      IF ( LOCINT(STG(11)) - LOCINT(STG(1)) .EQ.                        M35.9095
     1    LOCINT(ZIFT(11)) - LOCINT(ZIFT(1)) )                          M35.9096
     1     LZIFT = LVRESP                                               M35.9097
      LYM = LVRESP + ( 3*NPH + 3 ) * NFR1 + 2                           M35.9098
      LFV = LYM + NPH2                                                  M32, 469
      LHHM = LFV + NPOINT                                               M20.5131
      LHHN = LHHM + NPOINT                                              M20.5132
   20 CALL GUTS45 ( STG(LPBUF), STG(LCQ), STG(LZ), STG(LY), STG(LZY),   M22.6294
     1     STG(LZYA), STG(LZYB), STG(LZYC), STG(LZYD), STG(LCQT),       M20.5134
     2     STG(LQ), STG(LQI), STG(LG), STG(LG60), STG(LYO), STG(LXR),   M20.5135
     3     STG(LXL), STG(LXG), STG(LXC), STG(LUM), STG(LSI),            M20.5136
     4    STG(LVRESP),STG(LYM),STG(LFV),STG(LHHM),STG(LHHN),ZIFT(LZIFT))M35.9099
      IF ( KILL .GT. 0 )     GO TO 9999                                 M29.5410
      IF ( IALTER .NE. 3 )   GO TO 9999                                 M29.5411
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.5412
      REWIND LUNIT3                                                     M29.5413
      GO TO 5655                                                        M29.5414
 9999 RETURN                                                            M29.5415
      END                                                               M20.5139
      SUBROUTINE GUTS45 ( PBUF, CQ, Z, Y, ZY, ZYA, ZYB, ZYC, ZYD, CQT,  M22.6295
     1     Q, QI, G, G60, YO, XR, XL, XG, XC, UM, SI, VRESP, YMIN, FV,  M32, 471
     2     HHM, HHN, ZCOS )                                             M35.9100
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL45.FOR;/NOLIST'                                           
      INCLUDE  'VOLT45.FOR;/NOLIST'                                           
      DIMENSION PBUF(1), CQ(1), Z(1), Y(1), ZY(1), ZYA(1), ZYB(1)       M20.5143
      DIMENSION ZYC(1), ZYD(1), CQT(1), Q(1), QI(1), G(1), G60(1)       M20.5144
      DIMENSION YO(1), XR(1), XL(1), XG(1), XC(1), UM(1), SI(1)         M20.5145
      DIMENSION VRESP(1), YMIN(1), FV(1), HHM(1), HHN(1)                M32, 473
      DIMENSION YGR(6), WW(3), MFRECV(10), B(4), E(4), ITEMP(12)        M21. 434
      DIMENSION RUSS(5)                                                 M29.5416
      DIMENSION AMARTI(5)                                               M35.9101
      EQUIVALENCE (VIM(1),EPS),(VIM(2),EPS1),(VIM(3),FIT2Z)             M16.4570
      EQUIVALENCE (VIM(4),PIVTHR),(VIM(5),EPSRSE)                       M20.5148
      EQUIVALENCE (VIM(7),FT2EMX),(VIM(8),EPSEIG),(VIM(9),EPSPV2)       M20.5149
      EQUIVALENCE (VIM(10),FMED),(VIM(11),EPSYC),(VIM(12),EPSN)         M16.4573
      EQUIVALENCE (VIM(13),FFINP)                                       M20.5150
      EQUIVALENCE (IPNTV(1),NFIT),(IPNTV(2),NITER1),(IPNTV(3),NITER)    M16.4575
      EQUIVALENCE (IPNTV(4),NITERY),(IPNTV(5),NIEIG)                    M16.4576
      EQUIVALENCE (IPNTV(10),NOO),(INDTV(1),NPOINT),(INDTV(2),NSS)      M16.4577
      EQUIVALENCE (INDTV(3),KFIT),(INDTV(4),KPS),(INDTV(5),KYC)         M16.4578
      EQUIVALENCE (INDTV(6),IDOC),(INDTV(7),IOTX),(INDTV(8),IOSS)       M16.4579
      EQUIVALENCE (INDTV(9),IOFL),(INDTV(10),NPAN)                      M20.5151
      REAL*8        TEXT1, TEXT2, TEXT3,      TEXT5, TEXT6              M28.7232
      REAL*8                           TEXT10, TEXT11, TEXT12           M28.7233
      REAL*8        TEXT13, TEXT14, TEXT15, TEXT16, TEXT17              M20.5154
      DOUBLE PRECISION DEN1,DEN2,HSSR,HSSI,YSSR,YSSI,HAMP,HANG,ALNH,W2  M32, 474
      DOUBLE PRECISION DD11, ZCOS(1)                                    M35.9102
      DATA TEXT1 / 6HBRANCH /                                           M16.4584
      DATA  TEXT2   /  6HNEW RH  /                                      M16.4585
      DATA  TEXT3   /  6HO       /                                      M16.4586
      DATA  TEXT5   /  6HOLD DA  /                                      M16.4587
      DATA  TEXT6   /  6HTA      /                                      M16.4588
      DATA TEXT10 / 6HTOLERA /                                          M16.4592
      DATA TEXT11 / 6HNCES   /                                          M16.4593
      DATA TEXT12 / 6HLINE C /                                          M16.4594
      DATA TEXT13 / 6HONSTAN /                                          M16.4595
      DATA TEXT14 / 6HTS     /                                          M16.4596
      DATA TEXT15 / 6HCABLE  /                                          M16.4597
      DATA TEXT16 / 6HCONSTA /                                          M16.4598
      DATA TEXT17 / 6HNTS    /                                          M16.4599
      LL6 = 6                                                           M22.6296
      IF ( LASTOV  .EQ.  1 )   GO TO 3107                               M16.4600
      IF ( IPRSUP  .GE.  1 )                                            M16.4601
     1 WRITE (LUNIT6, 3106)  LUNIT5, IALTER, LASTOV, NSS, ICHECK,       M17.1360
     2            IOTX, IOSS, IOFL, KTAB, CI1, CK1, VOLTBC(1),          M39.3620
     3                  VOLTK(1), VOLTI(1)                              M16.4604
 3106 FORMAT ( /,  99H AT BEGINNING OF 'OVER45' .  LUNIT5  IALTER  LASTOM16.4605
     1V     NSS  ICHECK    IOTX    IOSS    IOFL    KTAB, /,             M39.3621
     2 27X,  9I8, /,                                                    M16.4607
     3 13X, 3HCI1, 12X, 3HCK1, 6X, 9HVOLTBC(1), 7X, 8HVOLTK(1),         M31.6688
     4 7X, 8HVOLTI(1),    /,                                            M16.4609
     5 1X, 5E15.6     )                                                 M16.4610
      IF ( IALTER  .EQ.  3 )   GO TO 10140                              M29.5417
      GO TO 10180                                                       M16.4611
C     DEFAULT DEFINITION OF  'TOLERANCES'  PARAMETERS.                  M16.4612
 3107 EPS = 0.5D-4                                                      M20.5155
      EPS1 = .5D-2                                                      M16.4614
      FIT2Z = 1.0D-1                                                    M16.4615
      PIVTHR = 1.0D-5                                                   M16.4616
      EPSRSE = 1.0D-2                                                   M20.5156
      FT2EMX = 1.0D-4                                                   M16.4619
      FMED = 5000.                                                      M16.4620
      EPSYC = 1.0D-3                                                    M16.4621
      EPSN = 1.0D-3                                                     M16.4622
      EPSPV2 = 1.0D-16                                                  M20.5157
      FFINP = 1.0                                                       M20.5158
      EPSEIG = 1.0D-7                                                   M16.4625
      NIEIG = 30                                                        M16.4626
      NFIT = 10                                                         M16.4627
      NITER = 10                                                        M16.4628
      NITER1 = 10                                                       M16.4629
      NITERY = 10                                                       M16.4630
      M = 1                                                             M20.5159
      DO  10014 I=1, 24                                                 M28.7234
10014 VSTACS(I) = BLANK                                                 M28.7235
C     READ INPUT CARD USING CIMAGE                                      M16.4633
 2946 CALL CIMAGE                                                       M21. 435
10015 IALTER = 1                                                        M16.4635
      DECODE (80,10020,ABUFF) BUS1                                      M16.4636
10020 FORMAT(13A6,A2)                                                   M16.4637
      IF (BUS1 .NE. TEXT1) GO TO 10060                                  M16.4638
C     OPTIONAL  'BRANCH'  CARD, WHICH SPECIFIES  A6  BRANCH NAMES.      M16.4639
      N1 = M + 11                                                       M20.5161
      DECODE (80,10040,ABUFF) (VSTACS(I), I=M, N1)                      M28.7236
10040 FORMAT(8X,12A6)                                                   M16.4641
      M = M + 12                                                        M21. 436
      WRITE (KUNIT6, 10050)                                             M35.9103
10050 FORMAT (26H+BUS NAMES FOR EACH PHASE.)                            M16.4643
      GO TO 2946                                                        M21. 437
10060 CONTINUE                                                          M16.4646
      DECODE (80, 10020, ABUFF(1) )  BUS1, BUS2                         M16.4647
      IF (BUS1 .NE. TEXT10) GO TO 10090                                 M16.4648
      IF (BUS2 .NE. TEXT11) GO TO 10090                                 M16.4649
C     OPTIONAL  'TOLERANCES'  CARD, TO REDEFINE OVERLAY TOLERANCES.     M16.4650
      DECODE (80, 10065, ABUFF)  ( MFRECV(I), RUSS(I), I=1, 5 )         M29.5418
10065 FORMAT(12X,5(I2,E11.0))                                           M20.5165
      WRITE (KUNIT6, 10070)  ( MFRECV(I), I=1, 5 )                      M35.9104
10070 FORMAT ( 24H+NEW SEMLYEN TOLERANCES.,  5I4  )                     M21. 439
      DO 10080 I=1, 5                                                   M20.5166
      N1 = MFRECV(I)                                                    M16.4656
      IF ( N1  .LE.  0 )   GO TO 10080                                  M21. 440
      IF ( N1  .GT.  18 )   GO TO 10080                                 M21. 441
      IF (N1 .GT. 13) GO TO 10075                                       M16.4658
      VIM(N1) = RUSS(I)                                                 M29.5419
      GO TO 10080                                                       M20.5168
10075 IPNTV(N1-13) = RUSS(I)                                            M29.5420
10080 CONTINUE                                                          M16.4662
      GO TO 2946                                                        M29.5421
C     READ  'SEMLYEN SETUP'  MISCELLANEOUS DATA PARAMETERS.             M16.4664
10090 CONTINUE                                                          M16.4665
      DECODE (80,10100,ABUFF) BUS1,BUS2,NOO,NPOINT,NSS,KFIT,KPS,KYC,    M16.4666
     1          IPUN,IDOC,IOTX,IOSS,IOFL,NPAN                           M20.5170
10100 FORMAT(A6,A2,2I5,6I2,3I6,24X,I3)                                  M20.5171
      WRITE (KUNIT6, 10095)  NOO, NPOINT, NSS, KFIT, KPS, KYC, IPUN     M35.9105
10095 FORMAT ( 12H+MISC. DATA.,  7I5  )                                 M21. 444
      IF ( IPRSUP  .GE.  1 )                                            M16.4671
     1 WRITE (LUNIT6, 10097)  NOO, NPOINT, NSS, KFIT, KPS, KYC, IPUN, IDM17.1362
     +OC                                                                M17.1363
10097 FORMAT ( /,  20H SEMLYEN MISC. DATA.,                             M16.4673
     1 64H     NOO  NPOINT     NSS    KFIT     KPS     KYC    IPUN    IDM16.4674
     2OC   ,/,  20X,  8I8  )                                            M16.4675
      IF ( BUS1  .NE.  TEXT5 )   GO TO 10120                            M16.4676
      IF ( BUS2  .NE.  TEXT6 )   GO TO 10120                            M16.4677
      IALTER = 3                                                        M16.4678
      IF ( NSS + ICHECK .EQ. 2 ) GO TO 10132                            M29.5422
      KILL = 182                                                        M29.5423
      LSTAT(14) = ICHECK                                                M29.5424
      LSTAT(15) = NSS                                                   M29.5425
      LSTAT(19) = 10132                                                 M29.5426
      GO TO 9200                                                        M29.5427
10132 CONTINUE                                                          M29.5428
      DECODE ( 80,10130,ABUFF(1) ) RUSS(1), CK1                         M29.5429
10130 FORMAT( 48X,2E12.5 )                                              M29.5430
      IF ( CK1 .EQ. 0. ) CK1 = SLL                                      M29.5431
      IF ( RUSS(1) .EQ. CI1 .OR. RUSS(1) .EQ. 0. ) GO TO 10131          M29.5432
      IALTER = 2                                                        M29.5433
      CI1 = RUSS(1)                                                     M29.5434
      GO TO 10140                                                       M29.5435
10131 REWIND LUNIT3                                                     M31.6689
      GO TO 9999                                                        M31.6690
10120 IF ( BUS1  .NE.  TEXT2 )   GO TO 10136                            M16.4683
      IF ( BUS2  .NE.  TEXT3 )   GO TO 10136                            M16.4684
      IALTER = 2                                                        M16.4685
      IF ( NSS + ICHECK  .EQ.  2 )   GO TO 10133                        M29.5437
      KILL = 182                                                        M29.5438
      LSTAT(14) = ICHECK                                                M29.5439
      LSTAT(15) = NSS                                                   M29.5440
      LSTAT(19) = 10133                                                 M29.5441
      GO TO 9200                                                        M29.5442
10133 CONTINUE                                                          M29.5443
      DECODE ( 80, 10130, ABUFF(1) ) CI1, CK1                           M29.5444
      IF ( CK1 .EQ. 0. ) CK1 = SLL                                      M29.5445
      GO TO 10140                                                       M16.4687
10136 CI1 = -FLTINF                                                     M16.4688
      CK1 = -FLTINF                                                     M16.4689
10140 IF ( IOTX  .NE.  -1 )   GO TO 1774                                M21. 445
      DO 1765  I=1, 3                                                   M21. 446
      N7 = 0                                                            M21. 447
      N10 = 0                                                           M21. 448
C     READ INPUT CARD USING CIMAGE                                      M21. 449
      CALL CIMAGE                                                       M21. 450
      DECODE (80, 1704, ABUFF(1) )   ( ITEMP(N8), N8=1, 12 )            M21. 451
 1704 FORMAT ( 20I4 )                                                   M21. 452
      DO 1734  J=1, 4                                                   M21. 453
      N8 = 4                                                            M21. 454
      N9 = 0                                                            M21. 455
      DO 1729  L=1, 3                                                   M21. 456
      N7 = N7 + 1                                                       M21. 457
      IF ( ITEMP(N7)  .EQ.  0 )   GO TO 1729                            M21. 458
      N9 = N9 + N8                                                      M21. 459
 1729 N8 = N8 / 2                                                       M21. 460
 1734 N10 = N10 * 10  +  N9                                             M21. 461
      IF ( I  .GT.  1 )   GO TO 1743                                    M21. 462
      IOTX = N10                                                        M21. 463
      WRITE (KUNIT6, 1739)  IOTX                                        M35.9106
 1739 FORMAT ( 38H+  'IOTX'  COMPONENTS.   OCTAL VALUE =,  I5   )       M21. 465
      GO TO 1765                                                        M21. 466
 1743 IF ( I  .GT.  2 )   GO TO 1753                                    M21. 467
      IOSS = N10                                                        M21. 468
      WRITE (KUNIT6, 1748)  IOSS                                        M35.9107
 1748 FORMAT ( 38H+  'IOSS'  COMPONENTS.   OCTAL VALUE =,  I5   )       M21. 470
      GO TO 1765                                                        M21. 471
 1753 IOFL = N10                                                        M21. 472
      WRITE (KUNIT6, 1757)  IOFL                                        M35.9108
 1757 FORMAT ( 38H+  'IOFL'  COMPONENTS.   OCTAL VALUE =,  I5   )       M21. 474
 1765 CONTINUE                                                          M21. 475
 1774 DO 10170  I=7, 9                                                  M21. 476
      N1 = IABS(INDTV(I))                                               M16.4691
      N2 = 0                                                            M16.4692
      N3 = 1                                                            M16.4693
10150 N4 = N1/10                                                        M16.4694
      N1 = N1 - 10*N4                                                   M16.4695
      IF (N1 .LT. 8) GO TO 10160                                        M16.4696
      KILL = 154                                                        M16.4697
      LSTAT(13) = I-6                                                   M16.4698
      LSTAT(14) = INDTV(I)                                              M16.4699
      LSTAT(19) = 10160                                                 M16.4700
      GO TO 9200                                                        M16.4701
10160 N2 = N2 + N3*N1                                                   M16.4702
      N3 = 8*N3                                                         M16.4703
      N1 = N4                                                           M16.4704
      IF (N1 .GT. 0) GO TO 10150                                        M16.4705
      INDTV(I) = N2                                                     M16.4706
10170 CONTINUE                                                          M16.4707
      IF (IALTER .GT. 2) GO TO 10310                                    M16.4708
      IF (IALTER .EQ. 2)  GO TO 10172                                   M28.7241
      IF ( LASTOV  .EQ.  1 )   GO TO 3121                               M16.4709
      IF (LASTOV .EQ. 44)  GO TO 3121                                   M22.6297
      IF (LASTOV .EQ. 47)  GO TO 3121                                   M22.6298
10172 N1 = NCHAIN                                                       M28.7242
      NCHAIN = LASTOV                                                   M16.4711
      LASTOV = N1                                                       M16.4712
      GO TO 9900                                                        M29.5446
C     READ INPUT CARD USING CIMAGE.                                     M16.4714
 3121 CALL CIMAGE                                                       M16.4715
      DECODE (80, 10020, ABUFF(1) )   BUS3, BUS4, BUS5                  M16.4716
      IF ( BUS3  .NE.  TEXT12 )   GO TO 3128                            M16.4717
      IF ( BUS4  .NE.  TEXT13 )   GO TO 3128                            M16.4718
      IF ( BUS5  .NE.  TEXT14 )   GO TO 3128                            M16.4719
      IF ( IPRSUP .GE. 1 )                                              M35.9109
     1 WRITE (LUNIT6, 3123)                                             M35.9110
 3123 FORMAT ( 32H TRANSFER TO  'LINE CONSTANTS' .     )                M35.9111
      KREQAB = 0                                                        M16.4722
      LASTOV = NCHAIN                                                   M16.4723
      NCHAIN = 44                                                       M16.4724
      GO TO 9900                                                        M29.5447
 3128 IF ( BUS3  .NE.  TEXT15 )   GO TO 3136                            M16.4726
      IF ( BUS4  .NE.  TEXT16 )   GO TO 3136                            M16.4727
      IF ( BUS5  .NE.  TEXT17 )   GO TO 3136                            M16.4728
      WRITE (KUNIT6, 3132)                                              M35.9112
 3132 FORMAT (  33H+TRANSFER TO  'CABLE CONSTANTS' .   )                M16.4730
      KREQAB = 1                                                        M16.4731
      LASTOV = NCHAIN                                                   M16.4732
      NCHAIN = 47                                                       M16.4733
      GO TO 9900                                                        M29.5448
 3136 KILL = 187                                                        M16.4735
      LSTAT(19) = 3136                                                  M16.4736
      GO TO 9200                                                        M16.4737
10180 IF (NSS+ICHECK .EQ. 2) GO TO 10190                                M16.4738
      KILL = 182                                                        M16.4739
      LSTAT(14) = ICHECK                                                M16.4740
      LSTAT(15) = NSS                                                   M16.4741
      LSTAT(19) = 10190                                                 M16.4742
      GO TO 9200                                                        M16.4743
10190 DO 10200 I=1, ICHECK                                              M16.4744
      IF ( VOLTI(I)  .EQ.  CI1 )   GO TO 10200                          M16.4745
      KILL = 183                                                        M16.4746
      LSTAT(13) = I                                                     M16.4747
      LSTAT(19) = 10200                                                 M16.4748
      GO TO 9200                                                        M16.4749
10200 CONTINUE                                                          M16.4750
      LCIN = 0                                                          M16.4751
      N1 = 1                                                            M16.4752
      REWIND LUNIT2                                                     M16.4753
      REWIND LUNIT3                                                     M16.4754
      N1 = 0                                                            M44.2324
      DO 10210  N12 = 1, 9999                                           M44.2325
      READ (LUNIT2,10290) (PL(I),I=1,14)                                M44.2326
10290 FORMAT(13A6,A2)                                                   M16.4756
      LCIN = LCIN + 1                                                   M16.4757
      IF ( IPRSUP  .GE.  1 )                                            M16.4758
     1     WRITE (LUNIT6,3148) LCIN, (PL(I),I=1,14)                     M20.5173
 3148 FORMAT ( /, 39H 80-COL. CARD IMAGE FROM 'LUNIT2' , NO.,  I3,      M16.4760
     1  1H.,  13A6,  A2 )                                               M16.4761
      IF (PL(1) .EQ. BLANK  .AND.                                       M44.2327
     1    PL(2) .EQ. BLANK ) N1 = N1 + 1                                M44.2328
      IF ( N1 .GE. 1 .AND. KREQAB .EQ. 1)                               M44.2329
     1    GO TO 10310                                                   M44.2330
      IF ( N1 .GE. 2 .AND. KREQAB .EQ. 0 )                              M44.2331
     1    GO TO 10310                                                   M44.2332
10210 CONTINUE                                                          M44.2333
10310 SLL = CK1                                                         M20.5181
      IF (NPAN .GT. 0) XPAN = TWOPI / NPAN                              M20.5182
      SI(1) = 0.                                                        M20.5183
      ISS = 0                                                           M16.4795
      COLD = 0.                                                         M16.4796
      IF (KREQAB .EQ. 1)   GO TO 10320                                  M16.4797
      SPDLT = 186283.5807D0                                             M16.4798
      GO TO 10330                                                       M16.4799
10320 SPDLT = SPEEDL                                                    M16.4800
10330 D1 = 10.                                                          M16.4801
      CONV5 = 20. / ALOGZ(D1)                                           M18. 982
      NFRPH = NFR1 * NPH                                                M20.5184
      RATIO = VOLTBC(3)                                                 M31.6691
      DPLU = RATIO + 1.0                                                M20.5185
      DMIN = RATIO - 1.0                                                M20.5186
      LCOSI = 3 * NFRPH + 1                                             M20.5187
      KPREC = 1                                                         M35.9113
      IF( LOCINT( PBUF(11)) - LOCINT(PBUF(1)) .EQ.                      M35.9114
     1  LOCINT(ZCOS(11)) - LOCINT(ZCOS(1)) )                            M35.9115
     1KPREC = 2                                                         M35.9116
      LCOSD = LCOSI / 2 + 1                                             M35.9117
      IF( KPREC .EQ. 2 ) LCOSD = LCOSI                                  M35.9118
      LIFT = LCOSI + NFR1 * 2 + 1                                       M35.9119
      IF( KPREC .EQ. 2 ) LIFT = LCOSI + NFR1                            M35.9120
      IPUNM = IPUN / 10                                                 M35.9121
      IPUN = IPUN - 10 * IPUNM                                          M35.9122
      IF (IOTX .GT. 0)                                                  M16.4805
     1 WRITE (LUNIT6,10490) NSS                                         M16.4806
10490 FORMAT(1H1,5X,73HCALCULATION OF TRANSFORMATION MATRICES.  CONTINUOM16.4807
     1US TRANSPOSITION FLAG = ,I2)                                      M16.4808
C              ***   AT THE TRANSFORMATION FREQUENCY   ***              M16.4809
      IF (NSS .EQ. 1) GO TO 10520                                       M16.4810
      IF (IOTX .LT. 128) GO TO 10500                                    M16.4811
      KILL = 185                                                        M16.4812
      LSTAT(13) = IOTX                                                  M16.4813
      LSTAT(14) = 127                                                   M16.4814
      LSTAT(19) = 10500                                                 M16.4815
      GO TO 9200                                                        M16.4816
10500 IWORK = 2                                                         M16.4817
      F = VOLTK(1)                                                      M16.4818
      W = TWOPI*F                                                       M16.4819
      CALL FRQDOM ( IOTX, PBUF, Z, Y, ZY, ZYA, ZYB, ZYC, ZYD, CQ, CQT,  M20.5189
     1     Q, QI, G, G60, YO, XR, XL, XG, XC)                           M20.5190
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M16.4821
      J = 1                                                             M16.4822
      DO 10510 I=1, N22, 2                                              M16.4823
      Q(J) = CQ(I)                                                      M16.4824
      QI(J) = ZYA(I)                                                    M16.4825
10510 J= J + 1                                                          M16.4826
      IF( IPUNM .LT. 1 ) GO TO 10605                                    M35.9123
      ZERO = 0.                                                         M35.9124
      DO 6001 I = 1, NPH                                                M35.9125
      WRITE(LUNIT7,6002) ( QI(J), J = I, NPHSQ, NPH )                   M35.9126
 6001 WRITE(LUNIT7,6002) ( ZERO, J=1, NPH )                             M35.9127
 6002 FORMAT(6F12.9)                                                    M35.9128
      GO TO 10605                                                       M16.4827
10520 K=1                                                               M16.4828
      DO 10540 J=1, NPH                                                 M16.4829
      DO 10530 I=1, NPH                                                 M16.4830
      Q(K) = 1.                                                         M16.4831
      IF (I .EQ. J .AND. I .NE. 1) Q(K) =       1 - I                   M16.4832
      IF (I .GT. J .AND. J .NE. 1) Q(K) = 0.                            M16.4833
10530 K = K + 1                                                         M16.4834
10540 CONTINUE                                                          M16.4835
      DO 10565 I=1, NPHSQ, NPH                                          M16.4836
      K = I + NPH - 1                                                   M16.4837
      TEMP = 0.                                                         M16.4838
      DO 10550 J=I, K                                                   M16.4839
10550 TEMP = TEMP + Q(J)**2                                             M16.4840
      TEMP = SQRTZ(TEMP)                                                M18. 983
      DO 10560 J=I, K                                                   M16.4842
      Q(J) = Q(J)/TEMP                                                  M16.4843
10560 QI(J) = Q(J)                                                      M16.4844
10565 CONTINUE                                                          M16.4845
      IOTX = IOTX/2                                                     M16.4846
      N1 = IOTX/2                                                       M16.4847
      IF (N1+N1 .EQ. IOTX) GO TO 10605                                  M16.4848
      IF (NPH .LE. 6) GO TO 5025                                        M20.5191
      WRITE (LUNIT6,5020)                                               M20.5192
 5020 FORMAT(1H0,41X,21HMODE TO PHASE VOLTAGE   ,/,                     M20.5193
     1     42X, 21HTRANSFORMATION MATRIX       )                        M20.5194
      DO 5022 I=1, NPH                                                  M20.5195
      WRITE (LUNIT6,5021) (Q(J), J=I, NPHSQ, NPH)                       M20.5196
 5021 FORMAT(/ (1X,12(2X,F8.6)))                                        M20.5197
 5022 CONTINUE                                                          M20.5198
      WRITE (LUNIT6,5023)                                               M20.5199
 5023 FORMAT(1H0,41X,21HMODE TO PHASE CURRENT   ,/,                     M20.5200
     1     42X, 21HTRANSFORMATION MATRIX   )                            M20.5201
      DO 5024 I=1, NPH                                                  M20.5202
 5024 WRITE (LUNIT6,5021) (QI(J), J=I, NPHSQ, NPH)                      M20.5203
      GO TO 10605                                                       M20.5204
 5025 WRITE (LUNIT6,10570)                                              M20.5205
10570 FORMAT(1H0,21X,21HMODE TO PHASE VOLTAGE,44X,21HMODE TO PHASE CURREM16.4850
     1NT,/,22X,21HTRANSFORMATION MATRIX,45X,21HTRANSFORMATION MATRIX//) M16.4851
      DO 10580 I=1, NPH                                                 M16.4852
      WRITE (LUNIT6,10590) (Q(J),J=I,NPHSQ,NPH)                         M16.4853
10590 FORMAT(65X,1H*/1X,6(2X,F8.6))                                     M16.4854
10580 WRITE (KUNIT6, 10600) (QI(J),J=I,NPHSQ,NPH)                       M35.9129
10600 FORMAT(1H+,64X,1H*,1X,6(2X,F8.6))                                 M16.4856
C              ***   AT THE STEADY-STATE FREQUENCY   ***                M16.4857
10605 F = VOLTK(ICHECK)                                                 M16.4858
      W = TWOPI*F                                                       M16.4859
      IADM = 1                                                          M16.4860
      ISS = 1                                                           M16.4861
      WW(IADM) = W                                                      M16.4862
      IF ( IOSS  .GT.  0 )                                              M16.4863
     1 WRITE (LUNIT6, 10610 )  F                                        M17.1368
10610 FORMAT ( //,  56H CALCULATION OF STEADY-STATE PARAMETERS, FOR FREQM16.4865
     1UENCY =,  E15.3,  6H   HZ.    )                                   M16.4866
      IWORK = 896                                                       M16.4867
      CALL FRQDOM ( IOSS, PBUF, Z, Y, ZY, ZYA, ZYB, ZYC, ZYD, CQ, CQT,  M20.5206
     1     Q, QI, G, G60, YO, XR, XL, XG, XC)                           M20.5207
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M16.4869
      ISS = 0                                                           M16.4870
      DO 10615 I=1, NPH2                                                M16.4871
10615 UM(I) = YO(I)                                                     M16.4872
      J = 1                                                             M32, 476
      DO 10616 I = 1, NPH2, 2                                           M32, 477
      JP1 = J + 1                                                       M32, 478
      YMIN(I) = SQRTZ ( Y( JP1) / Z ( JP1 ) )                           M32, 479
      YMIN(I+1) = SLL * G(I+1) / W                                      M35.9130
10616 J = J + NPHPI2                                                    M32, 480
10620 WO = TWOPI*VOLTBC(1)                                              M31.6692
      WM = WO*RATIO**(NFR-1)                                            M16.4880
      SI(NFR1 + 1) = WM * RATIO                                         M20.5208
      DO 10630 I=1, 8                                                   M16.4883
10630 MFRECV(I) = I*NFR/10                                              M16.4884
      MFRECV(9) = NFR - 1                                               M16.4885
      MFRECV(10) = NFR                                                  M16.4886
      WRITE(LUNIT6,10640) NFR, ( VOLTBC(I), I=1,3 )                     M35.9131
10640 FORMAT(1H1,2X,10HNO.FREQ = ,I3,5X,18HFIRST FREQUENCY = ,F10.5,5X, M16.4888
     116HLAST FREQUENCY = , F10.1, 5X, 12HGEO. RATIO = , F5.3 )         M35.9132
      DO 10650 I=1, NPH                                                 M16.4892
      N1 = I*NFR1 - NFR                                                 M16.4893
10650 VRESP(N1) = FFINP                                                 M20.5209
      KMIN = 640                                                        M20.5210
      IF (KFIT .LE. 9 .AND. KYC .LE. 9) KMIN = 128                      M29.5449
      KYPR = KYC / 10                                                   M20.5212
      KYC = KYC - 10 * KYPR                                             M20.5213
      JXQ = 0                                                           M16.4895
      IF (KYC .NE. 3)  GO TO 3178                                       M16.4896
      D13 = 2.0 * VOLTBC(1) * TT                                        M31.6694
      JXQ  =   - ALOGZ(D13) /  ALOGZ(RATIO)                             M31.6695
      JXQ = JXQ + 1                                                     M16.4898
C     THIS LOOP CALCULATES THE LINE STEP RESPONSE FOR BOTH THE          M16.4899
C     PROPAGATION FUNCTION(NFR POINTS) AND FOR THE ADMITTANCE FUNCTION  M16.4900
C     (3 POINTS) IN THE FREQUENCY DOMAIN.                               M16.4901
 3178 IADM = 2                                                          M16.4902
      IL = 1                                                            M16.4903
      W = WO                                                            M16.4904
      CONSTM = 0.62137                                                  M35.9133
      IF( KREQAB .EQ. 1 ) CONSTM = 1000.                                M35.9134
      DO 10690 JM=1, NFR                                                M16.4905
      SI(JM+1) = W                                                      M20.5214
      F = W/TWOPI                                                       M16.4907
C     FACTOR FOR SURPRESSION OF THE GIBB'S PHENOMENON.                  M16.4908
C     NOTE.  1./W FACTOR CONVERTS IMPULSE TO STEP RESPONSE.             M16.4909
      SIF = W*TWOPI/WM/2.                                               M16.4910
      SIF = SINZ(SIF)/SIF/W                                             M18. 986
      IF (JM .NE. JXQ) GO TO 10653                                      M16.4915
      N1 = IOFL                                                         M16.4916
      IWORK = 640                                                       M16.4917
      D1 = 1.0/(2.*TT)                                                  M16.4918
      IF (N1 .NE. 0)                                                    M16.4919
     1 WRITE (LUNIT6,10651) D1                                          M16.4920
10651 FORMAT(1H1,10X,98HTHIS FREQUENCY IS CLOSEST(FROM BELOW) TO THE NATM16.4921
     1URAL FREQUENCY, F = (SPEED OF LIGHT)/(2*LENGTH) = ,F10.2 /11X,111HM16.4922
     2IT WILL BE USED TO CALCULATE THE CONSTANT CHARACTERISTIC ADMITTANCM16.4923
     3E FOR USE IN THE TIME-STEP LOOP CONVOLUTION.   )                  M16.4924
      WW(1) = W                                                         M16.4925
      DO 10652 I=1, NPH2                                                M16.4926
10652 UM(I) = YO(I)                                                     M16.4927
      CALL FRQDOM ( N1, PBUF, Z, Y, ZY, ZYA, ZYB, ZYC, ZYD, CQ, CQT,    M20.5215
     1     Q, QI, G, G60, YO, XR, XL, XG, XC)                           M20.5216
      GO TO 10656                                                       M16.4928
10653 IWORK = KMIN                                                      M20.5217
      N1 =  0                                                           M16.4930
      IF (JM .NE. MFRECV(IL)) GO TO 10655                               M16.4931
      N1 = IOFL                                                         M16.4932
      IWORK = 640                                                       M16.4933
      IF (N1 .NE. 0 .AND. JM .NE. 1)                                    M16.4934
     1 WRITE (LUNIT6,10654)                                             M16.4935
10654 FORMAT( //, 1X )                                                  M16.4936
10655 CALL FRQDOM ( N1, PBUF, Z, Y, ZY, ZYA, ZYB, ZYC, ZYD, CQ, CQT,    M20.5218
     1     Q, QI, G, G60, YO, XR, XL, XG, XC )                          M20.5219
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M16.4938
      IF( IPUNM .LT. 1 ) GO TO 10656                                    M35.9135
      AMARTI(1) = W                                                     M35.9136
      J = 1                                                             M35.9137
      DO 6004 I = 1, NPH                                                M35.9138
      AMARTI(2) = Y(J) * CONSTM                                         M35.9139
      AMARTI(3) = Y(J+1) * CONSTM                                       M35.9140
      AMARTI(4) = Z(J) * CONSTM                                         M35.9141
      AMARTI(5) = Z(J+1) * CONSTM                                       M35.9142
      WRITE(LUNIT7,6005) AMARTI                                         M35.9143
 6004 J = J + NPHPI2                                                    M35.9144
 6005 FORMAT( 5E16.9 )                                                  M35.9145
10656 IX = JM + 1                                                       M16.4939
      J = 1                                                             M32, 482
      DO 10657 I = 1, NPH2, 2                                           M32, 483
      JP1 = J + 1                                                       M32, 484
      TTCHK = SQRTZ( Y(JP1) / Z(JP1) )                                  M35.9146
      IF( TTCHK .LT. YMIN(I) ) YMIN(I) = TTCHK                          M35.9147
10657 J = J + NPHPI2                                                    M32, 486
10658 DO 10660 I = 1, NPH2, 2                                           M32, 487
      D1 = SIF * EXPZ(-G(I) * SLL)                                      M20.5220
      D2 = G(I + 1) * SLL                                               M20.5221
      TTCHK = D2 / W                                                    M35.9148
      IF( TTCHK .GT. YMIN(I+1) ) YMIN(I+1) = TTCHK                      M35.9149
      VRESP(IX) = D1 * COSZ( D2 )                                       M20.5222
      N1 = IX + NFRPH                                                   M20.5223
      VRESP(N1) = D1 * SINZ( D2 )                                       M20.5224
      N1 = N1 + NFRPH                                                   M20.5225
      VRESP(N1) = YO(I)                                                 M20.5226
10660 IX = IX + NFR1                                                    M16.4943
      IF (JM .NE. MFRECV(IL))GO TO 10690                                M16.4944
      IL = IL + 1                                                       M16.4945
      CST = W/TWOPI                                                     M16.4946
      IF (CST .LT. FMED) GO TO 10670                                    M16.4947
      IADM = 3                                                          M16.4948
10670 WW(IADM) = W                                                      M16.4949
      N1 = NPH2 * (IADM - 1)                                            M20.5227
      DO 10680 I=1, NPH2                                                M16.4951
      N2 = N1 + I                                                       M16.4952
10680 UM(N2) = YO(I)                                                    M16.4953
10690 W = WO * RATIO**JM                                                M28.7243
      W = TWOPI * VOLTK(ICHECK)                                         M16.4955
      IF (IPRSUP .LT. 3) GO TO 70140                                    M16.4956
      WRITE (LUNIT6,70550) (I,I=1, NPH)                                 M16.4957
70550 FORMAT ( 1H0, 5X,                                                 M28.7244
     1  50HPROPAGATION STEP RESPONSE IN THE FREQUENCY DOMAIN.           M28.7245
     2   ,/,  1H0,  5X,  5HOMEGA,  3( 21X, 5HMODE , I2, 6X)             M28.7246
     3   ,/, (11X,  3(21X, 5HMODE , I2, 6X)  )  )                       M28.7247
      JMAX = NFRPH                                                      M32, 488
      DO 70130 I=1, NFR1                                                M16.4961
70130 WRITE(LUNIT6,70510) SI(I), ( VRESP(N2), VRESP(N2+NFRPH),          M32, 489
     1 N2 = I, JMAX, NFR1 )                                             M32, 490
      IF ( KFIT .LE. 9 .AND. KYPR .LE. 0 ) GO TO 70140                  M32, 491
      WRITE( LUNIT6,70131) ( I, I=1,NPH )                               M32, 492
70131 FORMAT(1H1, 5X, 49HADMITTANCE STEP RESPONSE IN THE FREQUENCY DOMAIM32, 493
     1N. ,/, 1H0, 5X, 5HOMEGA, 3( 21X, 5HMODE ,I2, 6X ) ,/,             M32, 494
     2 ( 11X, 3( 21X, 5HMODE ,I2, 6X ) ) )                              M32, 495
      JMAX = 3 * NFRPH                                                  M32, 496
      DO 70551 I = 1, NFR1                                              M32, 497
      N1 = 2 * NFRPH + I                                                M32, 498
70551 WRITE(LUNIT6,70511) SI(I), ( VRESP(N2), N2 = N1, JMAX, NFR1)      M32, 499
70511 FORMAT( 4X, E12.5, 3( 10X, E13.5, 9X ) ,/,                        M32, 500
     1            ( 16X, 3( 10X, E13.5, 9X ) ) )                        M32, 501
70140 CONTINUE                                                          M16.4963
      PI2 = 4./TWOPI                                                    M16.4964
      DO 70150 I=1, 91                                                  M16.4967
70150 PL(I) = BLANK                                                     M16.4968
70510 FORMAT ( 4X, E12.5,  6( 2X, E13.5, 2X )   ,/,                     M28.7251
     1         ( 16X,  6( 2X, E13.5, 2X )  )  )                         M28.7252
      KFYS = KFIT / 10                                                  M20.5229
      KFPS = KFIT - 10 * KFYS                                           M20.5230
      IF (KFYS .EQ. 0 .AND. KYPR .EQ. 0) GO TO 230                      M29.5450
      N1 = NFRPH + NFRPH + 1                                            M20.5232
      DO 200 I=1, NPH2, 2                                               M20.5233
      VRESP(N1) = YO(I) - VRESP(N1+1)                                   M20.5234
  200 N1 = N1 + NFR1                                                    M20.5235
C     CALCULATE FREQUENCY DOMAIN SEQUENCE USED TO PRODUCE THE CHAR-     M20.5236
C     ACTERISTIC ADMITTANCE STEP REPONSE IN THE TIME DOMAIN             M20.5237
      DO 220 JM=2, NFR1                                                 M20.5238
      W = SI(JM)                                                        M20.5239
      SIF = ONEHAF * TWOPI * W / WM                                     M20.5240
      SIF = SINZ(SIF) / SIF / W                                         M20.5241
      N1 = NFRPH + NFRPH + JM                                           M20.5242
      DO 210 I=1, NPH2, 2                                               M20.5243
      VRESP(N1) = SIF * (YO(I) - VRESP(N1))                             M20.5244
  210 N1 = N1 + NFR1                                                    M20.5245
  220 CONTINUE                                                          M20.5246
  230 D1 = FLTINF                                                       M20.5248
      N1 = 1                                                            M20.5249
      DO 344  I=1, NPH                                                  M20.5250
      IF (D1 .LT. UM(N1)) GO TO 344                                     M20.5251
      D1 = UM(N1)                                                       M20.5252
      KMIN = I                                                          M20.5253
  344 N1 = N1 + 2                                                       M20.5254
      IF (KYC .EQ. 1 .OR. KYC .EQ. 3) KMIN = NPH + NPH                  M20.5255
      IY = 2                                                            M20.5256
      IY3 = 3*IY                                                        M20.5257
  240 IF (IPRSUP .LE. 5) GO TO 640                                      M20.5258
      WRITE (LUNIT6,610)                                                M20.5259
  610 FORMAT ( 1H0, 5X, 73HDATA FOR FITTING OF CHARACTERISTIC ADMITTANCEM20.5260
     1 EXPONENTIAL APPROXIMATION.  ,/,  10X,  9HFREQUENCY,  17X,        M20.5261
     2  31H... CMPLX. CHAR. ADMITTANCE ...   )                          M20.5262
      DO 620 I=1, NPH                                                   M20.5263
      N1 = (I - 1) * NPH2 + 1                                           M20.5264
      N2 = N1 - 1 + NPH2                                                M20.5265
      D1 = WW(I)/TWOPI                                                  M20.5266
  620 WRITE (LUNIT6,630) D1, (UM(J),J=N1,N2)                            M20.5267
  630 FORMAT( /,  1X,  7(5X, E12.5)  ,/,  (18X,  6(5X, E12.5) )  )      M20.5268
  640 WRITE (LUNIT6,10654)                                              M20.5269
      IF( IPUNM .EQ. 2 ) GO TO 437                                      M35.9150
C     LOOP OVER ALL MODES TO CALCULATE(VIA INVERSE FOURIER              M16.4969
C     TRANSFORMATION)   AND FIT(VIA LEAST SQUARES) THE PROPAGATION STEP M16.4970
C     RESPONSE IN THE TIME DOMAIN.                                      M16.4971
      W = TWOPI * VOLTK(ICHECK)                                         M29.5451
      DO 40200 I1=1, NPH                                                M16.4972
      LSTAT(14) = I1                                                    M29.5452
      WRITE (LUNIT6,40060) I1                                           M16.4973
40060 FORMAT(1H1,10X,50HFITTING OF THE PROPAGATION STEP RESPONSE FOR MODM16.4974
     1E  ,  I2,  ///  1X)                                               M16.4975
      IX = (I1 - 1)*NFR1 + 1                                            M16.4976
      I2 = I1 + I1                                                      M20.5270
      I3 = 14 * I1                                                      M20.5271
      I4 = NFRPH + NFRPH + IX                                           M20.5272
      TRETRD = SLL * G(I2) / WM                                         M20.5274
      TT = YMIN(I2) - TRETRD                                            M35.9151
      FFIN = FFINP                                                      M20.5275
      TSTRT = TRETRD * 0.98                                             M28.7253
      TSTEP = TT / NOO                                                  M35.9152
      FLSTAT(11) = TSTEP                                                M28.7255
      VRESP(LIFT) = VRESP(IX)                                           M28.7256
      DO 40020  J=1, NFR                                                M28.7257
      N1 = IX + J                                                       M28.7258
      N2 = N1 + NFRPH                                                   M28.7259
      N3 = LIFT + J                                                     M28.7260
      SIF = TSTRT * SI(J+1)                                             M28.7261
      VRESP(N3) = VRESP(N1) * COSZ(SIF) + VRESP(N2) * SINZ(SIF)         M28.7262
      IF ( IPRSUP  .GE. 6 )                                             M28.7263
     1 WRITE (LUNIT6, 40002)  J, SIF, VRESP(N1), VRESP(N2),             M28.7264
     2                        VRESP(N3)                                 M28.7265
40002 FORMAT ( 5X, I10,  4( 5X,  E15.7 )  )                             M28.7266
40020 CONTINUE                                                          M28.7267
      ICTRL = KPS                                                       M20.5278
      IF (KFPS .EQ. 0) GO TO 40040                                      M20.5279
      I5 = LIFT                                                         M28.7268
      D1 = 0.02 * TRETRD                                                M28.7269
      D2 = D1                                                           M20.5281
      D3 = 1. / 3.                                                      M20.5282
      D4 = 2. / 3.                                                      M20.5283
      CALL RISE( D2, D4, TSTEP, VRESP(I5), SI, ZCOS( LCOSD ) )          M35.9153
      IF( KILL .LE. 0 ) GO TO 40026                                     M23.5725
      LSTAT(10) = NPOINT                                                M23.5726
      LSTAT(13) = KFPS                                                  M23.5727
      LSTAT(15) = KFIT                                                  M23.5729
      LSTAT(19) = 40026                                                 M35.9154
      FLSTAT(16) = D4                                                   M28.7271
      GO TO 9200                                                        M23.5732
40026 D1 = D2                                                           M28.7272
      TSTEP =  4. * D2 / NPOINT                                         M28.7273
      IF ( KFPS .EQ. 1 ) GO TO 40040                                    M28.7274
      TSTEP = D2 / 10.                                                  M28.7275
      FLSTAT(11) = TSTEP                                                M28.7276
      CALL RISE( D1, D3, TSTEP, VRESP(I5), SI, ZCOS( LCOSD ) )          M35.9155
      IF( KILL .LE. 0 ) GO TO 40027                                     M23.5734
      LSTAT(10) = NPOINT                                                M23.5735
      LSTAT(13) = KFPS                                                  M23.5736
      LSTAT(14) = I1                                                    M23.5737
      LSTAT(19) = 40027                                                 M35.9156
      FLSTAT(16) = D3                                                   M28.7278
      GO TO 9200                                                        M23.5741
40027 TSTEP = 20. * ( D2 - D1 ) / NPOINT                                M28.7279
40040 I5 = LIFT                                                         M28.7280
      SHIFTI = EXPZ( - G60(I2 - 1) * SLL)                               M20.5314
      D1 = G60(I2) * SLL                                                M20.5315
      SHIFTR = SHIFTI * COSZ( D1 )                                      M20.5316
      SHIFTI = SHIFTI * SINZ( D1 )                                      M20.5317
      CALL TDFIT( VRESP(I5), SI, FV, HHM, HHN, ZCOS( LCOSD ) )          M35.9157
      IF (KILL .GT. 0) GO TO 9200                                       M20.5319
      PBUF(I3 - 4) = D                                                  M20.5320
      PBUF(I3 - 3) = X(1)                                               M20.5321
      PBUF(I3 - 0) = X(3)                                               M25.1325
      PBUF(I3 - 2) = FFIN - D                                           M25.1326
      PBUF(I3 - 1) = X(2)                                               M25.1327
C     TIME DOMAIN FITTING OF THE CHARACTERISTIC ADMITTANCE STEP         M20.5325
C     RESPONSE FUNCTION                                                 M20.5326
      TSTRT = 0.0                                                       M20.5327
      FFIN = YO( I2 - 1 ) - YMIN( I2 - 1 )                              M32, 502
      IF (KFYS .EQ. 1) GO TO 40070                                      M29.5454
C     FITTING OF CHARACTERISTIC ADMITTANCE STEP IN THE FREQUENCY        M20.5329
C     DOMAIN.   TWO POINTS ARE USED TO FIT THE TRANSFORM OF             M20.5330
C      YC(I) = Y0 + E(1)*EXP(-T/B(1))  +  E(2)*EXP(-T/B(2))             M20.5331
C     ITERATIVELY.                                                      M20.5332
      IF ( KYC .EQ. 0 ) GO TO 401                                       M32, 503
      IF (KMIN .EQ. I1) GO TO 401                                       M20.5334
      PBUF(I3-9) = 0.0                                                  M27.3822
      PBUF(I3-8) = 1.0                                                  M20.5336
      PBUF(I3-7) = 0.0                                                  M20.5337
      PBUF(I3-6) = 1.0                                                  M20.5338
      PBUF(I3-5) = UM(I2 - 1)                                           M20.5339
      WRITE (LUNIT6, 400)  I1, PBUF(I3 - 5)                             M20.5340
  400 FORMAT ( /, 5X, 5HMODE ,  I2,                                     M20.5341
     1 37HCONSTANT CHARACTERISTIC ADMITTANCE = , E12.5  )               M37.9095
      GO TO 40090                                                       M20.5343
  401 N1 = I1 + I1                                                      M32, 504
      N2 = N1 + NPH2 + NPH2                                             M20.5345
      YGR(1) = UM(N2 - 1) - UM(N1 - 1)                                  M20.5346
      YGR(2) = UM(N2    ) - UM(N1    )                                  M20.5347
      YGR(3) = WW(1)                                                    M20.5348
      N1 = N1 + NPH2                                                    M20.5349
      YGR(4) = UM(N2 - 1) - UM(N1 - 1)                                  M20.5350
      YGR(5) = UM(N2    ) - UM(N1    )                                  M20.5351
      YGR(6) = WW(2)                                                    M20.5352
      YSURG = YMIN ( I2-1 )                                             M32, 505
  403 CALL SETPLT                                                       M32, 506
      WRITE( LUNIT6, 404 ) I1                                           M32, 507
  404 FORMAT ( //// 5X, 46HFITTING FOR CHARACTERISTIC ADMITTANCE MODE NOM32, 508
     1   ,  I2  )                                                       M20.5364
      IF (IPRSUP .GT. 0)                                                M20.5365
     1    WRITE (LUNIT6,99887) KMIN, N1, N2, KYC, (YGR(J), J=1, 6)      M20.5366
99887 FORMAT(/,5X,51HKMIN, N1, N2, KYC, YGR(J), J=1, 6) FOLLOW .....    M21. 477
     2 ,/,  4I10, 6E15.8 )                                              M20.5368
      DO 405 J=1, IY                                                    M20.5369
      E(J) = 0.0                                                        M20.5370
      B(J) = 0.0                                                        M20.5371
      E(J+2) = 1.0                                                      M20.5372
  405 B(J+2) = 1.0                                                      M20.5373
      ITER = 0                                                          M20.5374
  406 ITER = ITER + 1                                                   M20.5375
      N1 = 1                                                            M20.5376
      DO 408  J=1, IY3, 3                                               M20.5377
      ZR = YGR(J)                                                       M20.5378
      ZI = YGR(J+1)                                                     M20.5379
      N2 = 1                                                            M20.5380
      DO 407  K=1, IY3, 3                                               M20.5381
      IF ( J  .EQ.  K )   GO TO 407                                     M20.5382
      D1 = YGR(J+2) * B(N2)                                             M20.5383
      D2 = 1.0  +  D1 * D1                                              M20.5384
      ZR = ZR + E(N2) / D2                                              M22.6302
      ZI = ZI - E(N2) * D1 / D2                                         M22.6303
  407 N2 = N2 + 1                                                       M20.5387
      B(N1) = - ZI / ( YGR(J+2) * ZR )                                  M20.5388
      E(N1) = -(ZR * ZR  +  ZI * ZI) / ZR                               M22.6304
  408 N1 = N1 + 1                                                       M20.5390
      DO 409  J=1, IY                                                   M20.5391
      IF ( ABSZ( (E(J) - E(J+2) ) / E(J+2) )     .GT.   EPSYC )         M20.5392
     1 GO TO 412                                                        M20.5393
      IF ( ABSZ( (B(J) - B(J+2) ) / B(J+2) )     .GT.   EPSYC )         M20.5394
     1 GO TO 412                                                        M20.5395
  409 CONTINUE                                                          M20.5396
      DO 411  J=1, IY3, 3                                               M20.5397
      Y(J) = YGR(J)                                                     M20.5398
      Y(J+1) = YGR(J+1)                                                 M20.5399
      DO 410 K=1, IY                                                    M20.5400
      D1 = YGR(J+2) * B(K)                                              M20.5401
      D2 = 1.0 + D1*D1                                                  M20.5402
      Y(J) = Y(J) + E(K) / D2                                           M22.6305
  410 Y(J+1) = Y(J+1) - E(K) * D1 / D2                                  M22.6306
      ZR = Y(J) * Y(J) + Y(J+1) * Y(J+1)                                M20.5405
      IF ( SQRTZ(ZR)  .GT.  EPSN )   GO TO 412                          M20.5406
  411 CONTINUE                                                          M20.5407
      GO TO 415                                                         M20.5408
  412 DO 413  J=1, IY                                                   M20.5409
      B(J+2) = B(J)                                                     M20.5410
  413 E(J+2) = E(J)                                                     M20.5411
      IF ( IPRSUP  .GT.  5 )                                            M20.5412
     1 WRITE (LUNIT6, 414)  ITER,  ( E(J), B(J), J=1, IY )              M20.5413
  414 FORMAT ( /,  20H ADMITTANCE FITTING.,  5X,  I5,                   M20.5414
     1 6(5X, E12.5)  )                                                  M20.5415
      IF ( ITER  .LT.  NITERY )   GO TO 406                             M20.5416
      KILL = 155                                                        M20.5417
      LSTAT(13) = NITERY                                                M29.5456
      FLSTAT(13) = EPSYC                                                M20.5420
      FLSTAT(14) = EPSN                                                 M20.5421
      LSTAT(19) = 415                                                   M20.5422
      GO TO 9200                                                        M20.5423
  415 WRITE (LUNIT6, 416)  ITER                                         M20.5424
  416 FORMAT ( /, 5X,  20HNO. OF ITERATIONS = ,  I3,  //  )             M20.5425
      DO 417  J=1, IY3, 3                                               M20.5426
      D1 = YGR(J+2) / TWOPI                                             M20.5427
      IF ( YSURG .EQ. 0. )                                              M32, 509
     1 D1  =  -1.0 / ( TWOPI * YGR(J+2) )                               M20.5429
  417 WRITE(LUNIT6, 418) D1, YGR(J), YGR(J+1), Y(J), Y(J+1)             M21. 478
  418 FORMAT ( 2X,  12HFREQUENCY = ,  E12.5,  5X,                       M24. 772
     1 10HIMPULSE = ,  E12.5,  2X,  E12.5,  5X,  8HERROR =  ,           M20.5432
     2 E12.5,  2X,  E12.5  )                                            M20.5433
      N1 = I3 - 9                                                       M20.5434
      PBUF(I3 - 5) = YSURG                                              M22.6307
      DO 419  J=1, IY                                                   M20.5436
      IF ( YSURG .NE. 0. ) GO TO 4183                                   M32, 510
      B(J) = 1.0 / B(J)                                                 M22.6309
C     GO TO 4187                                                        M27.3824
 4183 E(J) = - E(J)                                                     M22.6311
      PBUF(I3 - 5) = PBUF(I3 - 5) + E(J)                                M22.6312
 4187 PBUF(N1) = E(J)                                                   M22.6313
      PBUF(N1 + 1) = B(J)                                               M22.6314
      IF (B(J) .LE. 0.0) PBUF(N1) = 0.0                                 M22.6315
  419 N1 = N1 + 2                                                       M20.5442
      WRITE (LUNIT6, 420)  ( E(J), J=1, IY )                            M20.5443
  420 FORMAT ( 5X,  13HAMPLITUDES = ,   3(E12.5, 5X)   )                M20.5444
      WRITE (LUNIT6, 421)  ( B(J), J=1, IY )                            M20.5445
  421 FORMAT ( 5X, 13HTIME CONST = ,   3(E12.5, 5X)   )                 M20.5446
      WRITE (LUNIT6, 422)  PBUF(I3-5)                                   M20.5447
  422 FORMAT ( 5X,  45HINITIAL VALUE OF CHARACTERISTIC ADMITTANCE = ,   M20.5448
     1 E12.5  )                                                         M20.5449
40090 IF ( KYPR .EQ. 0   .AND.   KFYS .EQ. 0 )   GO TO 40200            M29.5457
      IF( KYPR .EQ. 1 .AND. KFYS .EQ. 0 ) FFIN = PBUF(I3-9) + PBUF(I3-7)M32, 511
      D = PBUF(I3 - 9)                                                  M20.5452
      X(1) = PBUF(I3 - 8)                                               M20.5453
      X(2) = PBUF(I3 - 6)                                               M20.5454
      X(3) = 0.0                                                        M20.5455
      ICTRL = -10                                                       M20.5456
      TSTEP = PBUF(I3 - 8)                                              M20.5457
      TT = TSTEP                                                        M32, 512
      IF (TSTEP .LT. PBUF(I3 - 6)) TSTEP = PBUF(I3 - 6)                 M20.5458
      IF ( TT .GT. PBUF(I3-6) ) TT = PBUF(I3-6)                         M32, 513
      TT = TT * 2.                                                      M32, 514
      TSTEP = TSTEP / 10.                                               M32, 515
      IF ( TT .GT. TSTEP ) TT = TSTEP                                   M32, 516
      TSTEP = TT / NPOINT                                               M20.5461
      IF ( KFYS .LE. 0 )  GO TO 40030                                   M29.5460
40070 WRITE (LUNIT6,40050) I1                                           M20.5463
40050 FORMAT(1H1, 10X, 72HTIME DOMAIN FITTING OF CHARACTERISTIC ADMITTANM20.5464
     1CE STEP RESPONSE FOR NODE , I2 )                                  M35.9158
      ICTRL = -KYC - 1                                                  M29.5461
      PBUF(I3 - 5) = YO(I2 - 1)                                         M20.5468
      SHIFTR = YO(I2 - 1) - UM(I2 - 1)                                  M20.5469
      SHIFTI = YO(I2    ) - UM(I2    )                                  M20.5470
      IF (KFYS .GT. 1) GO TO 40030                                      M29.5462
      TT = .005                                                         M35.9159
      TSTEP = TT / 200.                                                 M35.9160
40080 D1 = TSTEP                                                        M29.5466
      TARG = 0.5                                                        M32, 518
      FLSTAT(11) = TSTEP                                                M29.5468
      IF( IPRSUP .GT. 3 )                                               M35.9161
     1WRITE( LUNIT6,40082 ) D1, TARG, TSTEP, FFIN, TT, YO(I2-1)         M35.9162
40082 FORMAT( 5X, 89HAT ENTRY TO RISE, INITIAL TIME, TARGET, TIMESTEP,  M35.9163
     1FINAL VALUE, TT, AND INITIAL VALUE ARE ,/, 25X, 6E12.5 )          M35.9164
      CALL RISE( D1, TARG, TSTEP, VRESP(I4), SI, ZCOS( LCOSD ) )        M35.9165
      IF( KILL .LE. 0 ) GO TO 40081                                     M23.5745
      LSTAT(10) = NPOINT                                                M23.5746
      LSTAT(13) = KFYS                                                  M23.5747
      LSTAT(15) = KFIT                                                  M23.5749
      LSTAT(19) = 40080                                                 M23.5750
      FLSTAT(13) = TT * 2                                               M29.5470
      FLSTAT(16) = TARG                                                 M29.5471
      GO TO 9200                                                        M23.5752
40081 TSTEP = D1 / NPOINT * 2.                                          M29.5472
      TT = NPOINT * TSTEP                                               M29.5474
      WRITE( LUNIT6, 422 ) PBUF(I3-5)                                   M32, 519
40030 CALL TDFIT( VRESP(I4), SI, FV, HHM, HHN, ZCOS( LCOSD ) )          M35.9166
      IF (KILL .GT. 0) GO TO 9200                                       M20.5477
      TT = SLL / SPDLT                                                  M20.5478
      PBUF(I3 - 9) = D                                                  M20.5479
      PBUF(I3 - 8) = X(1)                                               M20.5480
      PBUF(I3 - 7) = FFIN - D                                           M20.5481
      PBUF(I3 - 6) = X(2)                                               M20.5482
40200 CONTINUE                                                          M20.5483
      W = TWOPI * VOLTK(ICHECK)                                         M32, 520
      W2 = W * W                                                        M32, 521
      WRITE( LUNIT6,706 ) W                                             M35.9167
  706 FORMAT(//, 15X, F5.1,  54HHZ STEADY STATE MODAL PARAMETERS IN OHMSM35.9168
     1 AND MICROMHOS  ,/,  15X,  60HAS CALCULATED FROM FITTED EXPONENTIAM35.9169
     2LS VIA FOURIER TRANSFORM ,//, 25X, 46HMODE     R           X      M35.9170
     3     G           B )                                              M35.9171
      DO 701 I1 = 1, NPH                                                M32, 522
      I3 = 14 * I1                                                      M32, 523
      I2 = I3-13                                                        M32, 524
      IF( IPRSUP .GE. 3 )                                               M35.9172
     1 WRITE( LUNIT6,702 ) I1, I2, I3, ( PBUF(I), I=I2,I3 )             M35.9173
  702 FORMAT ( 14H PBUF FOR MODE, I3, 5H  I2=, I3, 5H  I3=, I3          M34. 190
     1       ,/, (1X,  8E16.8 ) )                                       M34. 191
      DEN1 = 1.0 / PBUF(I3-3)                                           M32, 527
      DEN1 = DEN1 * DEN1 + W2                                           M32, 528
      DEN2 = 1.0 / PBUF(I3-1)                                           M32, 529
      DEN2 = DEN2 * DEN2 + W2                                           M32, 530
      HSSR = 1.0 - PBUF(I3-4) * W2 / DEN1 - PBUF(I3-2) * W2 / DEN2      M32, 531
      HSSI = - PBUF(I3-4) / PBUF(I3-3) * W / DEN1                       M32, 532
     1 - PBUF(I3-2) / PBUF(I3-1) * W / DEN2                             M32, 533
      DEN1 = 1.0 / PBUF(I3-8)                                           M32, 534
      DEN1 = DEN1 * DEN1 + W2                                           M32, 535
      DEN2 = 1.0 / PBUF(I3-6)                                           M32, 536
      DEN2 = DEN2 * DEN2 + W2                                           M32, 537
      YSSR = PBUF(I3-5) -(PBUF(I3-9) + PBUF(I3-7)  )                    M32, 538
     1 + PBUF(I3-9) * W2 / DEN1 + PBUF(I3-7) * W2 / DEN2                M32, 539
      YSSI = PBUF(I3-9) / PBUF(I3-8) * W / DEN1                         M32, 540
     1     + PBUF(I3-7) / PBUF(I3-6) * W / DEN2                         M32, 541
      HAMP = ( HSSR * HSSR + HSSI * HSSI )**.5                          M32, 542
      CALL DATN2Z(HSSI,HSSR,DD11)                                       M33.2374
      HANG = W * PBUF(I3) - DD11                                        M33.2375
      CALL DLOGZ(HAMP, DD11)                                            M33.2376
      ALNH = -DD11                                                      M33.2377
      PBUF(I3-11) = ALNH * YSSR - HANG * YSSI                           M32, 545
      PBUF(I3-10) = ALNH * YSSI + HANG * YSSR                           M32, 546
      DEN1 = YSSR * YSSR + YSSI * YSSI                                  M32, 547
      PBUF(I3-13) = ( ALNH * YSSR + HANG * YSSI ) / DEN1                M32, 548
      PBUF(I3-12) = ( HANG * YSSR - ALNH * YSSI ) / DEN1                M32, 549
      PBUF(I3-10) = PBUF(I3-10) / TENM6                                 M32, 550
      PBUF(I3-11) = PBUF(I3-11) / TENM6                                 M32, 551
      I4 = I3-10                                                        M32, 552
      IF( IPRSUP .GE. 3 )                                               M35.9174
     1 WRITE(LUNIT6,703) I1, I2, I4, HSSR, HSSI, YSSR, YSSI, HAMP, HANG,M35.9175
     1 (PBUF(I),I=I2,I4)                                                M32, 554
  703 FORMAT(55H DURING CALCULATION OF STEADY STATE PARAMETERS FOR MODE,M37.9096
     1 I3,48H  HSSR, HSSI, YSSR, YSSI, HAMP, HANG, PBUF(I),I=, I3, 1H,, M33.2379
     2 I3, /,1X, 10E13.4 )                                              M34. 193
      WRITE( LUNIT6,705 ) I1, ( PBUF(I), I=I2, I4 )                     M35.9176
  705 FORMAT( 26X, I2, 4E12.5 )                                         M35.9177
  701 CONTINUE                                                          M32, 558
C     DOCUMENTATION OF OUTPUT CARDS TO BE PUNCHED.                      M16.5489
      IF (IPUN .NE. 1)                                                  M16.5490
     1 WRITE (LUNIT6,425)                                               M16.5491
  425 FORMAT(//,27X, 34HSEQUENTIAL LIST OF PUNCHED OUTPUT.     ,/,      M20.5484
     1  27X,34(1H-),//,4X, 81H0         1         2         3         4 M20.5485
     2        5         6         7         8  ,/,   9(4X,1H0,5X)       M20.5486
     3     ,/,   4X,  81( 1H- )  ,/,   1X   )                           M20.5487
      NSS = IABS(NSS)                                                   M16.5494
      KFIT = IABS(KFIT)                                                 M16.5495
      KPS = IABS(KPS)                                                   M16.5496
      KYC = IABS(KYC) + IABS(KYPR) * 10                                 M29.5475
      F = VOLTK(ICHECK)                                                 M16.5498
      IF( KREQAB .EQ. 1 ) GO TO 704                                     M35.9178
      IF (IPUN .NE. 1)                                                  M16.5499
     1 WRITE (LUNIT6,449) SLL,CI1,F,NSS,KFIT,KPS,KYC                    M16.5500
  449 FORMAT(5X,4HC L=,F8.1,11HMILES, RHO=,F8.1,10H, SS FREQ=,F8.2,     M28.7283
     1  6H, NSS=,I1,7H, KFIT=,I2,6H, KPS=,I1,6H, KYC=,I2)               M29.5476
      IF (IPUN .NE. 2)                                                  M16.5503
     1 WRITE (LUNIT7,448) SLL,CI1,F,NSS,KFIT,KPS,KYC                    M16.5504
  448 FORMAT (  4HC L=,F8.1,11HMILES, RHO=,F8.1,10H, SS FREQ=,F8.2,     M28.7284
     1  6H, NSS=,I1,7H, KFIT=,I2,6H, KPS=,I1,6H, KYC=,I2)               M29.5477
      GO TO 452                                                         M35.9179
  704 IF( IPUN .NE. 1 )                                                 M35.9180
     1 WRITE( LUNIT6,453 ) SLL, CI1, F, NSS, KFIT, KPS, KYC             M35.9181
  453 FORMAT(5X,4HC L=, F8.0, 12HMETERS, RHO=, F8.1, 10H, SS FREQ=,F8.2,M35.9182
     1 6H, NSS= , I1, 7H, KFIT= , I2, 6H, KPS= , I1, 6H, KYC= , I2 )    M35.9183
      IF( IPUN .NE. 2 )                                                 M35.9184
     1 WRITE(LUNIT7,451) SLL, CI1, F, NSS, KFIT, KPS, KYC               M35.9185
  451 FORMAT(   4HC L=, F8.0, 12HMETERS, RHO=, F8.1, 10H, SS FREQ=,F8.2,M35.9186
     1 6H, NSS= , I1, 7H, KFIT= , I2, 6H, KPS= , I1, 6H, KYC= , I2 )    M35.9187
  452 IF ( IDOC .EQ. 0 ) GO TO 428                                      M35.9188
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.5478
      REWIND LUNIT2                                                     M16.5508
      DO 429 J=1, LCIN                                                  M16.5509
      READ (LUNIT2,447) (PL(I),I=1,14)                                  M20.5489
      IF (IPUN .NE. 1) WRITE (LUNIT6,446) (PL(I),I=1,14)                M20.5490
      IF (IPUN .NE. 2) WRITE (LUNIT7,445) (PL(I),I=1,13)                M20.5491
  429 CONTINUE                                                          M16.5513
  441 FORMAT(5X,2H-1,2A6,12X,2E12.5,12X,2I3,8H  2  2  ,I2/              M23.5754
     1      7X,5E15.8,/, (7X, 6E12.5))                                  M28.7285
  440 FORMAT(2H-1,2A6,12X,2E12.5,12X,2I3,8H  2  2  ,I2/                 M23.5756
     1      2X,5E15.8,/, (2X,6E12.5))                                   M28.7286
  443 FORMAT(7X,6E12.5)                                                 M23.5758
  444 FORMAT(2X,6E12.5)                                                 M23.5759
  445 FORMAT(2HC , 13A6)                                                M16.5520
  446 FORMAT(5X, 2HC , 14A6)                                            M20.5492
  447 FORMAT(13A6, A2)                                                  M16.5522
  428 D1 = 0.0                                                          M23.5760
C     PUNCH MODAL DATA                                                  M16.5524
      DO 430 I=1, NPH                                                   M16.5525
      KMAX1 = 14*I                                                      M16.5526
      KMIN1 = KMAX1 - 4                                                 M16.5527
      KMAX2 = KMAX1 - 6                                                 M16.5528
      KMIN2 = KMAX1 - 9                                                 M16.5529
      KMIN = KMAX1 - 13                                                 M16.5530
      KMAX = KMAX1 - 10                                                 M16.5531
      PBUF(KMIN + 2) = PBUF(KMIN + 2) * TENM6                           M25.1328
      PBUF(KMIN + 3) = PBUF(KMIN + 3) * TENM6                           M23.5762
      PBUF(KMIN1 + 1) = 1.0 / PBUF(KMIN1 + 1)                           M23.5763
      PBUF(KMIN1 + 3) = 1.0 / PBUF(KMIN1 + 3)                           M23.5764
      PBUF(KMIN2 + 0) = - PBUF(KMIN2 + 0)                               M23.5765
      PBUF(KMIN2 + 1) = 1.0 / PBUF(KMIN2 + 1)                           M23.5766
      PBUF(KMIN2 + 2) = - PBUF(KMIN2 + 2)                               M23.5767
      PBUF(KMIN2 + 3) = 1.0 / PBUF(KMIN2 + 3)                           M23.5768
      IF (IPUN .NE. 1) WRITE (LUNIT6, 441) VSTACS(2*I-1), VSTACS(2*I),  M28.7287
     1     PBUF(KMAX2+1), PBUF(KMIN1+4), I, I, NPH,                     M25.1329
     2     (PBUF(K), K=KMIN, KMAX), VOLTK(ICHECK),                      M31.6696
     2     D1, PBUF(KMIN1+1), PBUF(KMIN1+0),                            M23.5772
     3     D1, PBUF(KMIN1+3), PBUF(KMIN1+2),                            M23.5773
     4     D1, PBUF(KMIN2+1), PBUF(KMIN2+0),                            M23.5774
     5     D1, PBUF(KMIN2+3), PBUF(KMIN2+2)                             M23.5775
      IF (IPUN .NE. 2) WRITE (LUNIT7, 440) VSTACS(2*I-1), VSTACS(2*I),  M28.7289
     1     PBUF(KMAX2+1), PBUF(KMIN1+4), I, I, NPH,                     M25.1330
     1     (PBUF(K), K=KMIN, KMAX), VOLTK(ICHECK),                      M31.6697
     2     D1, PBUF(KMIN1+1), PBUF(KMIN1+0),                            M23.5779
     3     D1, PBUF(KMIN1+3), PBUF(KMIN1+2),                            M23.5780
     4     D1, PBUF(KMIN2+1), PBUF(KMIN2+0),                            M23.5781
     5     D1, PBUF(KMIN2+3), PBUF(KMIN2+2)                             M23.5782
  430 CONTINUE                                                          M16.5542
C     PUNCH MODE TO PHASE VOLTAGE & CURRENT TRANSFORMATION MATRICES.    M27.3825
      DO 433 I=1, NPH                                                   M23.5783
      IF (IPUN .NE. 1) WRITE (LUNIT6, 443)                              M23.5784
     1          (Q(K), D1, K=I, NPHSQ, NPH)                             M23.5785
      IF (IPUN .NE. 2) WRITE (LUNIT7, 444)                              M23.5786
     1          (Q(K), D1, K=I, NPHSQ, NPH)                             M23.5787
  433 CONTINUE                                                          M23.5788
      DO 436 I=1, NPH                                                   M23.5789
      IF (IPUN .NE. 1) WRITE (LUNIT6, 443)                              M23.5790
     1          (QI(K), D1, K=I, NPHSQ, NPH)                            M23.5791
      IF (IPUN .NE. 2) WRITE (LUNIT7, 444)                              M23.5792
     1          (QI(K), D1, K=I, NPHSQ, NPH)                            M23.5793
  436 CONTINUE                                                          M23.5794
  437 WRITE( LUNIT6,10654 )                                             M35.9189
C     READ INPUT CARD USING CIMAGE                                      M16.5567
      CALL CIMAGE                                                       M16.5568
      DECODE (80,10290,ABUFF) (VSTACS(I),I=1,14)                        M28.7291
      DO 450 I=1, 14                                                    M16.5570
      IF (VSTACS(I) .NE. BLANK)  GO TO 516                              M22.6319
  450 CONTINUE                                                          M16.5572
      WRITE (KUNIT6, 470)                                               M35.9190
  470 FORMAT ( 43H+BLANK CARD TERMINATING   'SEMLYEN SETUP' .   )       M16.5574
      CALL INTERP                                                       M35.9191
      NCHAIN = 51                                                       M22.6320
      IALTER = 0                                                        M29.5479
      GO TO 9900                                                        M29.5480
  516 DO 523  I=1, 14                                                   M22.6321
  523 VSTACS(I) = BLANK                                                 M28.7292
      M = 1                                                             M28.7293
      GO TO 10015                                                       M22.6323
 9200 NCHAIN = 51                                                       M16.5577
      LSTAT(18) = 45                                                    M16.5578
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,480) KILL,LSTAT(19),LSTAT(10)    M16.5579
  480 FORMAT(31H0 OVER45 EXIT WITH KILL CODE = ,I5,5X,2I10)             M37.9097
 9900 LASTOV = 45                                                       M29.5481
 9999 RETURN                                                            M29.5482
      END                                                               M16.5583
      SUBROUTINE CXC(A,B,C,KODE)                                        M18.1017
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)  THIS SUBROUTINE PERFORMS SOME OF THE MATRIX MANIPULATION PROCEDURES M16.5585
C)  USED BY OVER45 ON LINEARIZED COMPLEX MATRICES STORED IN COLUMN      M16.5586
C)  ORDER.  THESE PROCESSES INCLUDE MULTIPLICATION BY COMPLEX MTX, BY   M16.5587
C)  REAL PART OF COMPLEX MTX, BY TRANSPOSE OF REAL PART OC COMPLEX MTX  M16.5588
C)  (IN EITHER ORDER FOR LAST TWO) AND INVERSION OF COMPLEX MTX.        M16.5589
C)  THESE OPERATIONS EACH HAVE A SEPARATE ENTRY, WHERE THE ARGUMENTS AREM16.5590
C)  FIRST, FOLLOWED BY THE RESULT OF THE PROCEDURE.                     M16.5591
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL45.FOR;/NOLIST'                                           
      INCLUDE  'VOLT45.FOR;/NOLIST'                                           
      DIMENSION A(1), B(1), C(1), IJ(50)                                M20.5536
      EQUIVALENCE (VIM(9),EPSPV2)                                       M20.5537
      GO TO (1000, 2000, 3000, 4000, 5000, 6000), KODE                  M18.1018
 1000 N3 = 1                                                            M18.1019
      IR = 1                                                            M16.5596
      N4 = 0                                                            M16.5597
      DO 30 J=1, NPH                                                    M16.5598
      N4 = N4 + NPH2                                                    M16.5599
      DO 20 I=1, NPH2, 2                                                M16.5600
      N1 = I                                                            M16.5601
      IM = IR + 1                                                       M16.5602
      C(IR) = 0.                                                        M16.5603
      C(IM) = 0.                                                        M16.5604
      DO 10 N2=N3, N4, 2                                                M16.5605
      C(IR) = C(IR) + A(N1)*B(N2) - A(N1+1)*B(N2+1)                     M16.5606
      C(IM) = C(IM) + A(N1)*B(N2+1) + A(N1+1)*B(N2)                     M16.5607
   10 N1 = N1 + NPH2                                                    M16.5608
   20 IR = IR + 2                                                       M16.5609
   30 N3 = N3 + NPH2                                                    M16.5610
      RETURN                                                            M16.5611
 2000 IROW = 2                                                          M18.1020
      ICOL = NPH2                                                       M16.5614
   40 IR = 1                                                            M16.5615
      N3 = 1                                                            M16.5616
      DO 70 J=1, NPH                                                    M16.5617
      DO 60 I=1, NPH2, 2                                                M16.5618
      IM = IR + 1                                                       M16.5619
      N2 = N3                                                           M16.5620
      C(IR) = 0.                                                        M16.5621
      C(IM) = 0.                                                        M16.5622
      DO 50 N1=I, N22, NPH2                                             M16.5623
      C(IR) = C(IR) + A(N1)*B(N2)                                       M16.5624
      C(IM) = C(IM) + A(N1+1)*B(N2)                                     M16.5625
   50 N2 = N2 + IROW                                                    M16.5626
   60 IR = IR + 2                                                       M16.5627
   70 N3 = N3 + ICOL                                                    M16.5628
      RETURN                                                            M16.5629
 3000 IROW = NPH2                                                       M18.1021
      ICOL = 2                                                          M16.5632
      GO TO 40                                                          M16.5633
 4000 IROW = 2                                                          M18.1022
      ICOL = NPH2                                                       M16.5636
   75 N3 = 1                                                            M16.5637
      DO 100 J=1, NPH2, 2                                               M16.5638
      IR = J                                                            M16.5639
      DO 90 I=1, N22, NPH2                                              M16.5640
      N1 = N3                                                           M16.5641
      N4 = I + NPH2 - 2                                                 M16.5642
      IM = IR + 1                                                       M16.5643
      C(IR) = 0.                                                        M16.5644
      C(IM) = 0.                                                        M16.5645
      DO 80 N2=I, N4, 2                                                 M16.5646
      C(IR) = C(IR) + A(N1)*B(N2)                                       M16.5647
      C(IM) = C(IM) + A(N1)*B(N2+1)                                     M16.5648
   80 N1 = N1 + ICOL                                                    M16.5649
   90 IR = IR + NPH2                                                    M16.5650
  100 N3 = N3 + IROW                                                    M16.5651
      RETURN                                                            M16.5652
 5000 IROW = NPH2                                                       M18.1023
      ICOL = 2                                                          M16.5655
      GO TO 75                                                          M16.5656
 6000 DO 110  I=1, NPH                                                  M18.1024
  110 IJ(I) = 0                                                         M16.5659
      DO 120 J=1, N22                                                   M16.5660
  120 B(J) = A(J)                                                       M16.5661
      L = 0                                                             M16.5662
  130 L = L + 1                                                         M16.5663
      IF (L .GT. NPH) RETURN                                            M16.5664
      T = 0.                                                            M16.5665
      N1 = 1                                                            M16.5666
      DO 140 J=1, NPH                                                   M16.5667
      IF (IJ(J) .GT. 0) GO TO 140                                       M16.5668
      U = B(N1)*B(N1) + B(N1+1)*B(N1+1)                                 M16.5669
      IF (U .LE. T) GO TO 140                                           M16.5670
      T = U                                                             M16.5671
      N2 = J                                                            M16.5672
      K = N1                                                            M16.5673
  140 N1 = N1 + NPHPI2                                                  M16.5674
      IJ(N2) = 1                                                        M16.5675
      IF (T .GT. EPSPV2) GO TO  145                                     M16.5676
      KILL = 199                                                        M20.5538
      FLSTAT(15) = F                                                    M20.5539
      FLSTAT(13) = T                                                    M16.5678
      FLSTAT(14) = EPSPV2                                               M16.5679
      LSTAT(13) = L                                                     M16.5680
      LSTAT(14) = NPH                                                   M16.5681
      LSTAT(19) = 145                                                   M16.5682
      RETURN                                                            M16.5683
  145 U = B(K)/T                                                        M16.5684
      T = -B(K+1)/T                                                     M16.5685
      B(K) = U                                                          M16.5686
      B(K+1) = T                                                        M16.5687
      N3 = (N2 - 1)*NPH2 + 1                                            M16.5688
      N2 = 2*N2 - 1                                                     M16.5689
      DO 160 I=1, NPH2, 2                                               M16.5690
      IF (I .EQ. N2) GO TO 160                                          M16.5691
      N1 = I + (N2 - 1)*NPH                                             M16.5692
      V = U*B(N1) - T*B(N1+1)                                           M16.5693
      D9 = U * B(N1+1)  +  T * B(N1)                                    M22.6324
      DO 150 J=1, N22, NPH2                                             M16.5695
      IF (J .EQ. N3) GO TO 150                                          M16.5696
      K = I + J                                                         M16.5697
      N4 = J + N2                                                       M16.5698
      B(K-1) = B(K-1)  -  B(N4-1) * V  +  B(N4) * D9                    M22.6325
      B(K) = B(K)  -  B(N4-1) * D9  -  B(N4) * V                        M22.6326
  150 CONTINUE                                                          M16.5701
  160 CONTINUE                                                          M16.5702
      K = N2                                                            M16.5703
      DO 170 I=1, NPH2, 2                                               M16.5704
      IF (I .EQ. N2) GO TO 170                                          M16.5705
      N4 = N3 + I                                                       M16.5706
      V = B(N4-1)*U - B(N4)*T                                           M16.5707
      B(N4) = B(N4-1)*T + B(N4)*U                                       M16.5708
      B(N4-1) = V                                                       M16.5709
      V = B(K+1)*T - B(K)*U                                             M16.5710
      B(K+1) = -B(K)*T - B(K+1)*U                                       M16.5711
      B(K) = V                                                          M16.5712
  170 K = K + NPH2                                                      M16.5713
      GO TO 130                                                         M16.5714
      END                                                               M16.5715
      SUBROUTINE FRQDOM(IOUTP, PBUF, Z, Y, ZY, ZYA, ZYB, ZYC, ZYD, CQ,  M20.5540
     1     CQT, Q, QI, G, G60, YO, XR, XL, XG, XC)                      M20.5541
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)  THIS SUBROUTINE IS USED TO PROCESS LINE/CABLE CONSTANTS DATA AT A   M16.5717
C)  SPECIFIC FREQUENCY.  THE R, L, C MATRICES (LOW. TRI, COL ORDER) ARE M16.5718
C)  READ FROM TAPE3.  EACH MATRIX IS A SEPARATE RECORD.  THE ROUTINE CANM16.5719
C)  SELECTIVELY CALCULATE AND/OR OUTPUT THE FOLLOWING QUANTITIES.       M16.5720
C)                                                                      M16.5721
C)     1.  MODAL TRANSFORMATION MARRICES(EXACT ONLY).                   M16.5722
C)     2.  ALPHA, BETA, ATTENUATION % VELOCITY FOR EACH MODE.           M16.5723
C)     3.  MODAL DISTRIBUTED IMPEDANCE AND SHUNT ADMITTANCE.            M16.5724
C)     4.  MODAL CHARACTERISTIC IMPEDANCE.                              M16.5725
C)     5.  MODAL EQUIVALENT PI IMPEDANCE AND ADMITANCE.                 M16.5726
C)     6.  PHASE EQUIVALENT PI MATRICES.                                M16.5727
C)                                                                      M16.5728
C)  THESE VALUES MAY BE CALCULATED ON THE BASIS OF EXACT DIAGONAL-      M16.5729
C)  IZATION OF ZY MATRIX OR BY USE OF PRE-CALCULATED TRANSFORMATION     M16.5730
C)  MATRICES.                                                           M16.5731
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL45.FOR;/NOLIST'                                           
      INCLUDE  'VOLT45.FOR;/NOLIST'                                           
      EQUIVALENCE (VIM(8),EPSEIG),(IPNTV(5),NIEIG),(INDTV(2),NSS)       M16.5734
      DIMENSION PBUF(1), Z(1), Y(1), ZY(1), ZYA(1), ZYB(1), ZYC(1)      M20.5542
      DIMENSION ZYD(1), CQ(1), CQT(1), Q(1), QI(1), G(1), G60(1)        M20.5543
      DIMENSION YO(1), XR(1), XL(1), XG(1), XC(1)                       M20.5544
      LL1 = 1                                                           M18.1025
      LL2 = 2                                                           M18.1026
      LL3 = 3                                                           M18.1027
      LL4 = 4                                                           M18.1028
      LL5 = 5                                                           M18.1029
      LL6 = 6                                                           M18.1030
      I = IOUTP                                                         M16.5735
      J = IWORK                                                         M16.5736
      K = 1                                                             M16.5737
   61 N1 = I/2                                                          M16.5738
      N2 = J/2                                                          M16.5739
      IF (I .NE. N1 + N1 .AND. J .EQ. N2 + N2) IWORK = IWORK + K        M16.5740
      K = K + K                                                         M16.5741
      I = N1                                                            M16.5742
      J = N2                                                            M16.5743
      IF (I .NE. 0) GO TO 61                                            M16.5744
C     READ MATRICES  (G), (B), (R), AND (X)  FROM UNIT 3.               M16.5745
      IF ( IPRSUP  .GE.  1 )                                            M16.5746
     1 WRITE (LUNIT6, 3614)  IOUTP, IWORK, NPH, NTRI, NPH2, NSS, COLD, FM17.1375
     +, W                                                               M17.1376
 3614 FORMAT ( /,  20H AT BEGIN 'FRQDOM' .,                             M16.5748
     1 48H   IOUTP   IWORK     NPH    NTRI    NPH2     NSS    ,         M16.5749
     2 11X,  4HCOLD,  14X,  1HF,  14X,  1HW   ,/,                       M16.5750
     3 20X, 6I8, 3E15.5  )                                              M16.5751
      IF ( KREQAB  .EQ.  0 )   GO TO 3603                               M16.5752
      N1 = 2 * NPH * NPH                                                M16.5753
      READ (LUNIT3)  ( Y(I), I=1, N1 )                                  M17.1377
      READ (LUNIT3)  ( Z(I), I=1, N1 )                                  M17.1378
      M=1                                                               M16.5756
      DO 3615  I = 1, NPH                                               M25.1331
      KCOL = 2 * NPH * (I-1) + 1                                        M25.1332
      KTRI = KCOL + 2 * ( I-1 )                                         M25.1333
      DO 888  J= KCOL, KTRI, 2                                          M25.1334
      XG(M) = Y(J)                                                      M25.1335
      XR(M) = Z(J)                                                      M25.1336
      XC(M) = Y(J+1)                                                    M25.1337
      XL(M) = Z(J+1)                                                    M25.1338
  888 M = M + 1                                                         M25.1339
 3615 CONTINUE                                                          M16.5764
      D1 = Y(2) / F                                                     M16.5765
      GO TO 3622                                                        M16.5766
 3603 READ (LUNIT3) (XG(I),I=1,NTRI)                                    M17.1379
      READ (LUNIT3) (XC(I),I=1,NTRI)                                    M17.1380
      READ (LUNIT3) (XR(I),I=1,NTRI)                                    M17.1381
      READ (LUNIT3) (XL(I),I=1,NTRI)                                    M17.1382
      IF ( IPRSUP  .GE.  4 )                                            M16.5771
     1 WRITE (LUNIT6, 3617)  XG(1), XC(1), XR(1), XL(1)                 M17.1383
 3617 FORMAT ( /,  19H AFTER UNIT 3 READ.  ,  10X,  5HXG(1),            M16.5773
     2 10X, 5HXC(1), 10X, 5HXR(1), 10X, 5HXL(1)  ,/,                    M16.5774
     3 19X,  4E15.5  )                                                  M16.5775
      D1 = XC(1) / F                                                    M16.5776
 3622 IF ( COLD  .EQ.  0.0 )   GO TO 1                                  M16.5777
      D2 = ( D1 - COLD ) / D1                                           M16.5778
      D3 = EPSILN * 100.                                                M24. 780
      IF ( ABSZ(D2)  .LE.  D3 )   GO TO 1                               M25.1340
      WRITE (LUNIT6, 3629)  F, COLD, D1, D2                             M17.1384
 3629 FORMAT ( /, 9H AT 3629     ,  4E20.10  )                          M16.5781
      KILL = 188                                                        M16.5782
      FLSTAT(13) = D1                                                   M16.5783
      FLSTAT(14) = F                                                    M16.5784
      FLSTAT(15) = COLD                                                 M16.5785
      FLSTAT(19) = 1                                                    M16.5786
      RETURN                                                            M16.5787
    1 COLD = D1                                                         M16.5788
      IF (IOUTP .NE. 0) WRITE (LUNIT6,136) F                            M16.5789
  136 FORMAT( ///, 54X, 12HFREQUENCY = , E13.4  ,/,  54X,               M16.5790
     1     25(1H*) ,/, 1X )                                             M20.5545
      IF ( KREQAB  .GT.  0 )   GO TO 3637                               M16.5792
      L = 1                                                             M16.5793
      DO 3 I=1, NPH                                                     M16.5794
      J = (I - 1)*NPH2 + 1                                              M16.5795
      K = I + I - 1                                                     M16.5796
      DO 2 N1=1, I                                                      M16.5797
      Z(J) = XR(L)                                                      M16.5798
      Z(K) = XR(L)                                                      M16.5799
      Y(J) = XG(L)                                                      M16.5800
      Y(K) = XG(L)                                                      M16.5801
      Z(J+1) = XL(L)                                                    M16.5802
      Z(K+1) = XL(L)                                                    M16.5803
      Y(J+1) = XC(L)                                                    M16.5804
      Y(K+1) = XC(L)                                                    M16.5805
      J = J + 2                                                         M16.5806
      K = K + NPH2                                                      M16.5807
    2 L = L + 1                                                         M16.5808
    3 CONTINUE                                                          M16.5809
      IF (NSS .EQ. 0)   GO TO 3637                                      M16.5810
      IF (NPH .EQ. 1)   GO TO 3637                                      M27.3826
      ZOFF = 0.0                                                        M16.5811
      ZOFF1 = 0.0                                                       M16.5812
      ZDIA = 0.0                                                        M16.5813
      ZDIA1 = 0.0                                                       M16.5814
      YOFF = 0.0                                                        M16.5815
      YOFF1 = 0.0                                                       M16.5816
      YDIA = 0.0                                                        M16.5817
      YDIA1 = 0.0                                                       M16.5818
      DO 90   I=1, NPH2, 2                                              M16.5819
      K = ( I + 1) / 2                                                  M16.5820
      L = (K-1) * NPHPI2                                                M16.5821
      N = L + 1                                                         M16.5822
      J = 1                                                             M16.5823
      IF (J .EQ. K)   GO TO 75                                          M16.5824
      DO 70   M=I, L, NPH2                                              M16.5825
      M1 = M + 1                                                        M16.5826
      ZOFF = ZOFF + Z(M)                                                M16.5827
      YOFF = YOFF + Y(M)                                                M16.5828
      ZOFF1 = ZOFF1 + Z(M1)                                             M16.5829
      YOFF1 = YOFF1 + Y(M1)                                             M16.5830
   70 CONTINUE                                                          M16.5831
      GO TO 80                                                          M16.5832
   75 N1 = N + 1                                                        M16.5833
      ZDIA = ZDIA + Z(N)                                                M16.5834
      YDIA = YDIA + Y(N)                                                M16.5835
      ZDIA1 = ZDIA1 + Z(N1)                                             M16.5836
      YDIA1 = YDIA1 + Y(N1)                                             M16.5837
   80 IF (I .EQ. 1)   GO TO 90                                          M16.5838
      J = J + K - 1                                                     M16.5839
      IF (J .LE. K)   GO TO 75                                          M16.5840
   90 CONTINUE                                                          M16.5841
      NOFF = NPH * (NPH - 1) / 2                                        M16.5846
      ZOFF = ZOFF/NOFF                                                  M21. 479
      YOFF = YOFF/NOFF                                                  M21. 480
      ZOFF1 = ZOFF1/NOFF                                                M21. 481
      YOFF1 = YOFF1/NOFF                                                M21. 482
      ZDIA = ZDIA/NPH                                                   M21. 483
      YDIA = YDIA/NPH                                                   M21. 484
      ZDIA1 = ZDIA1/NPH                                                 M21. 485
      YDIA1 = YDIA1/NPH                                                 M21. 486
      DO 95   I=1, NPH                                                  M16.5851
      J = (I-1) * NPHPI2 + 1                                            M16.5852
      L = (I - 1) * NPH2+ 1                                             M16.5853
      L1 = I *NPH2                                                      M16.5854
      DO 94   K = L, L1, 2                                              M16.5855
      K1 = K + 1                                                        M16.5856
      IF (K .EQ. J)   GO TO 93                                          M16.5857
      Z(K) = ZOFF                                                       M16.5858
      Y(K) = YOFF                                                       M16.5859
      Z(K1) = ZOFF1                                                     M16.5860
      Y(K1) = YOFF1                                                     M16.5861
      GO TO 94                                                          M16.5862
   93 Z(K) = ZDIA                                                       M16.5863
      Y(K) = YDIA                                                       M16.5864
      Z(K1) = ZDIA1                                                     M16.5865
      Y(K1) = YDIA1                                                     M16.5866
   94 CONTINUE                                                          M16.5867
   95 CONTINUE                                                          M16.5868
 3637 IWORK = -IWORK                                                    M16.5869
      IOUT1 = IOUTP                                                     M16.5870
      IOUTP = IOUTP/2                                                   M16.5871
      IF (IOUTP + IOUTP .EQ. IOUT1) GO TO 13                            M16.5872
      IF( KREQAB .EQ. 1 ) GO TO 34                                      M35.9192
      WRITE (LUNIT6,131)                                                M16.5873
  131 FORMAT ( /, 10X, 16HIMPEDANCE MATRIX, 25X,  17HADMITTANCE MATRIX  M16.5874
     1 ,/, 13X,  10H(OHM/MILE),  31X,  10H(MHO/MILE)   )                M16.5875
      GO TO 27                                                          M16.5876
   34 WRITE( LUNIT6,134 )                                               M35.9193
  134 FORMAT( /,  10X, 16HIMPEDANCE MATRIX, 25X,  17HADMITTANCE MATRIX  M35.9194
     1 ,/, 14X,   8H(OHM/M ) ,  33X,   8H(MHO/M )  )                    M35.9195
      GO TO 27                                                          M35.9196
   30 IWORK = IWORK/2                                                   M16.5877
      N1 = IWORK/64                                                     M16.5878
      N2 = IOUTP/64                                                     M16.5879
      IF (64*N1 .EQ. IWORK .OR. NSS .EQ. 1) GO TO 35                    M16.5880
      IF (64*N2 .NE. IOUTP) WRITE (LUNIT6,139)                          M17.1385
  139 FORMAT ( /, 10X,  82HTHE FOLLOWING VALUES ARE COMPUTED ON THE BASIM16.5882
     1S OF EXACT DIAGONALIZATION OF ZY MTX.    )                        M16.5883
      DX = -(SPDLT/W)**2                                                M16.5884
      J = 1                                                             M16.5885
      DO 14 I=1, N22                                                    M16.5886
      ZYA(I) = DX*ZY(I)                                                 M16.5887
      IF (I .NE. J) GO TO 14                                            M16.5888
      ZYA(I) = ZYA(I) - UNITY                                           M16.5889
      J = J + NPHPI2                                                    M16.5890
   14 CONTINUE                                                          M16.5891
C     CALCULATION OF EIGENVALUES AND EIGENVECTORS BY THE POWER METHOD.  M16.5892
      DO 41 I=1, N22                                                    M16.5893
   41 ZYB(I) = ZYA(I)                                                   M16.5894
      L = 0                                                             M16.5895
   42 L = L + 1                                                         M16.5896
      IQ  = 0                                                           M16.5897
      IF (L .GE. NPH) GO TO 414                                         M16.5898
   43 IQ = IQ + 1                                                       M16.5899
      IF (IQ .LE. NIEIG) GO TO 44                                       M16.5900
      LSTAT(19) = 44                                                    M16.5901
      KILL = 165                                                        M35.9197
      LSTAT(19) = 44                                                    M16.5903
      RETURN                                                            M16.5904
   44 DO 45 I=1, N22                                                    M16.5905
   45 ZYC(I) = ZYB(I)                                                   M16.5906
      CALL CXC(ZYC,ZYC,ZYB,LL1)                                         M18.1032
      DIAGX = 0.                                                        M16.5908
      J = 1                                                             M16.5909
      DO 46 I=1, NPH2, 2                                                M16.5910
      DX = ZYB(J)*ZYB(J) + ZYB(J+1)*ZYB(J+1)                            M16.5911
      IF (DX .LE. DIAGX) GO TO 46                                       M16.5912
      DIAGX = DX                                                        M16.5913
      N1 = J                                                            M16.5914
      N2 = I                                                            M16.5915
   46 J = J + NPHPI2                                                    M16.5916
      ZYR = ZYB(N1)/DIAGX                                               M16.5917
      ZYI = - ZYB(N1+1)/DIAGX                                           M16.5918
      DO 47 I=1, N22, 2                                                 M16.5919
      DX = ZYR*ZYB(I) - ZYI*ZYB(I+1)                                    M16.5920
      ZYB(I+1) = ZYR*ZYB(I+1) + ZYI*ZYB(I)                              M16.5921
   47 ZYB(I) = DX                                                       M16.5922
      DIAG = 0.                                                         M16.5923
      J = (N2 - 1)*NPH + 1                                              M16.5924
      K = J + NPH2 - 2                                                  M16.5925
      DO 48 I=J, K, 2                                                   M16.5926
      IF (I .EQ. N1) GO TO 48                                           M16.5927
      DX = ZYB(I)*ZYB(I) + ZYB(I+1)*ZYB(I+1)                            M16.5928
      IF (DX .LE. DIAG) GO TO 48                                        M16.5929
      DIAG = DX                                                         M16.5930
      N3 = I                                                            M16.5931
   48 CONTINUE                                                          M16.5932
      IF (DIAG .GT. 0.) GO TO 410                                       M20.5546
      DO 49 I=N2, N22, NPH2                                             M16.5934
      IF (I .EQ. N1) GO TO 49                                           M16.5935
      DX = ZYB(I) * ZYB(I) + ZYB(I+1) * ZYB(I+1)                        M20.5547
      IF (DX .LE. DIAG) GO TO 49                                        M20.5548
      DIAG = DX                                                         M16.5936
      N3 = I                                                            M16.5937
   49 CONTINUE                                                          M16.5938
  410 DX = (ZYC(N1)*ZYC(N1) + ZYC(N1+1)*ZYC(N1+1))/(ZYC(N3)*ZYC(N3) +   M16.5939
     1     ZYC(N3+1)*ZYC(N3+1))                                         M16.5940
      IF (IPRSUP .GT. 6)                                                M20.5549
     1     WRITE (LUNIT6,10101) N1, N2, N3, J, IQ, DX, DIAG, DIAGX,     M20.5550
     1     EPSEIG, (ZYB(I), I=J, K), (ZYB(I), I=N2, N22, NPH2)          M20.5551
10101 FORMAT(/, 53H IN EIGEN POWER LOOP.  N1, N2, N3,  J, IQ, DX, DIAG, M20.5552
     1,61HDIAGX, EPSIEG, (ZYB(I), I=J, K), (ZYB(I), I=N2, N22, NPH2) =  M20.5553
     2 ,/, 1X, I9, 4I10, 4E20.11  ,/,  ( 1X, 6E20.11 )  )               M22.6327
      IF (ABSZ(SQRTZ(DX*DIAG) - 1.0) .GT. EPSEIG) GO TO 43              M18.1033
      L2 = L*NPH2 + 1                                                   M16.5942
      N3 = L2 - NPH2                                                    M16.5943
      K = (N2 - 1)*NPH + 1                                              M16.5944
      ZYR = 0.                                                          M16.5945
      ZR = 0.                                                           M16.5946
      ZYI = 0.                                                          M16.5947
      ZI = 0.                                                           M16.5948
      DO 411 I=N2, N22, NPH2                                            M16.5949
      ZYR = ZYR + ZYA(I)*ZYB(K) - ZYA(I+1)*ZYB(K+1)                     M16.5950
      ZYI = ZYI + ZYA(I)*ZYB(K+1) + ZYA(I+1)*ZYB(K)                     M16.5951
      ZR = ZR + ZYB(I)*ZYB(K) - ZYB(I+1)*ZYB(K+1)                       M16.5952
      ZI = ZI + ZYB(I)*ZYB(K+1) + ZYB(I+1)*ZYB(K)                       M16.5953
      CQ(N3) = ZYB(K)                                                   M16.5954
      CQ(N3+1) = ZYB(K+1)                                               M16.5955
      CQ(L2) = ZYB(I)                                                   M16.5956
      CQ(L2+1) = ZYB(I+1)                                               M16.5957
      K = K + 2                                                         M16.5958
      N3 = N3 + 2                                                       M16.5959
  411 L2 = L2 + 2                                                       M16.5960
      L2 = L + L                                                        M16.5961
      G(L2-1) = ZYR                                                     M16.5962
      G(L2) = ZYI                                                       M16.5963
      DX = ZR*ZR + ZI*ZI                                                M16.5964
      ZYR = (ZR*G(L2-1) + ZI*G(L2))/DX                                  M16.5965
      ZYI = (ZR*G(L2) - ZI*G(L2-1))/DX                                  M16.5966
      K = 1                                                             M16.5967
      N2 = L*NPH2 + 1                                                   M16.5968
      N1 = N2 - NPH2                                                    M16.5969
      N3 = N2 - 2                                                       M16.5970
      N4 = N3 + NPH2                                                    M16.5971
      DO 413 I=N2, N4, 2                                                M16.5972
      ZR = CQ(I)*ZYR - CQ(I+1)*ZYI                                      M16.5973
      ZI = CQ(I)*ZYI + CQ(I+1)*ZYR                                      M16.5974
      DO 412 J=N1, N3, 2                                                M16.5975
      ZYB(K) = ZYA(K) - CQ(J)*ZR + CQ(J+1)*ZI                           M16.5976
      ZYB(K+1) = ZYA(K+1) - CQ(J)*ZI - CQ(J+1)*ZR                       M16.5977
      ZYA(K) = ZYB(K)                                                   M16.5978
      ZYA(K+1) = ZYB(K+1)                                               M16.5979
  412 K = K + 2                                                         M16.5980
  413 CONTINUE                                                          M16.5981
      GO TO 42                                                          M16.5982
  414 DIAGX = 0.                                                        M16.5983
      J = 1                                                             M16.5984
      DO 415 I=1, NPH2, 2                                               M16.5985
      DX = ZYA(J)*ZYA(J) + ZYA(J+1)*ZYA(J+1)                            M16.5986
      IF (DX .LE. DIAGX) GO TO 415                                      M16.5987
      DIAGX = DX                                                        M16.5988
      N1 = J                                                            M16.5989
      N2 = I                                                            M16.5990
  415 J= J + NPHPI2                                                     M16.5991
      N3 = (L-1)*NPH2 + 1                                               M16.5992
      K = (N2 - 1)*NPH + 1                                              M16.5993
      ZYR = 0.                                                          M16.5994
      ZYI = 0.                                                          M16.5995
      ZR = ZYA(N1)/DIAGX                                                M16.5996
      ZI = -ZYA(N1+1)/DIAGX                                             M16.5997
      DO 416 I=N2, N22, NPH2                                            M16.5998
      ZYR = ZYR + ZYA(I)*ZYA(K) - ZYA(I+1)*ZYA(K+1)                     M16.5999
      ZYI = ZYI + ZYA(I)*ZYA(K+1) + ZYA(I+1)*ZYA(K)                     M16.6000
      CQ(N3) = ZR*ZYA(K) - ZYA(K+1)*ZI                                  M16.6001
      CQ(N3+1) = ZI*ZYA(K) + ZYA(K+1)*ZR                                M16.6002
      N3 = N3 + 2                                                       M16.6003
  416 K = K + 2                                                         M16.6004
      L2 = L + L                                                        M16.6005
      G(L2-1) = ZR*ZYR - ZI*ZYI                                         M16.6006
      G(L2) = ZR*ZYI + ZI*ZYR                                           M16.6007
      IF (IPRSUP .LT. 5) GO TO 418                                      M16.6008
      WRITE (LUNIT6,135) (G(I),I=1,NPH2)                                M16.6009
  135 FORMAT ( /, 5X,  31HRAW EIGENVALUES % VECTOR OUTPUT  ,/,          M16.6010
     1 ( /,  10E12.5 ) )                                                M16.6011
      DO 417 I=1, N22, NPH2                                             M16.6012
      J = I + NPH2 - 1                                                  M16.6013
  417 WRITE (LUNIT6,107) (CQ(K),K=I,J)                                  M16.6014
  107 FORMAT ( 1H0, 5X,  6(5X, E12.5) ,/, 6X,  6(5X, E12.5 ) )          M16.6015
  418 CONTINUE                                                          M16.6016
      DX = W/SPDLT                                                      M16.6017
      DO 16 I=1, NPH2, 2                                                M16.6018
      G(I) = G(I) + 1.0                                                 M16.6019
      ZYR = SQRTZ(G(I)*G(I) + G(I+1)*G(I+1))                            M18.1034
      IF (G(I) .LT. 0.0)   GO TO 200                                    M19.2478
      ZR = SQRTZ((G(I) + ZYR)/2.)                                       M18.1035
      ZI = G(I+1) / ZR / 2.                                             M19.2479
      GO TO 201                                                         M19.2480
  200 ZI = SQRTZ((-G(I) +ZYR) / 2.)                                     M19.2481
      IF (G(I+1) .LT. 0.0) ZI = -ZI                                     M16.6023
      ZR = G(I+1) / ZI /2.                                              M19.2482
  201 G(I) = - DX * ZI                                                  M19.2483
      G(I+1) = DX*ZR                                                    M16.6025
   16 CONTINUE                                                          M16.6026
C     CALCULATION OF CURRENT TRANSFORMATION MATRIX                      M16.6027
      CALL CXC(CQ,ZYB,ZYA,LL6)                                          M18.1037
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M16.6029
      N1 = 1                                                            M16.6030
      DO 56 I=1, NPH2, 2                                                M16.6031
      K = N1                                                            M16.6032
      DO 55 J=I, N22, NPH2                                              M16.6033
      ZYA(K) = ZYB(J)                                                   M16.6034
      ZYA(K+1) = ZYB(J+1)                                               M16.6035
   55 K = K + 2                                                         M16.6036
   56 N1 = N1 + NPH2                                                    M16.6037
      IWORK = IWORK / 2                                                 M16.6038
      IOUT1 = IOUTP                                                     M16.6039
      IOUTP = IOUTP / 2                                                 M16.6040
      IF ( IOUTP + IOUTP   .EQ.   IOUT1 )   GO TO 18                    M16.6041
C     OUTPUT THE TRANSFORMATION MATRICES.                               M16.6042
      IF (NPH .LE. 6) GO TO 127                                         M20.5555
      WRITE (LUNIT6, 120)                                               M20.5556
  120 FORMAT(//,42X,21HMODE TO PHASE VOLTAGE   ,/,                      M20.5557
     1     42X, 21HTRANSFORMATION MATRIX ,/, 1X    )                    M20.5558
      N1 = 5 * NPH2                                                     M20.5559
      DO 124 I=1, NPH2, 2                                               M20.5560
      N2 = I                                                            M20.5561
      WRITE (LUNIT6, 121)                                               M20.5562
  121 FORMAT (1X)                                                       M20.5563
  122 N3 = N2 + N1                                                      M20.5564
      IF (N3 .GT. N22) N3 = N22                                         M20.5565
      N4 = N2 + 1                                                       M20.5566
      N5 = N3 + 1                                                       M20.5567
      WRITE (LUNIT6, 121)                                               M20.5568
      WRITE (LUNIT6, 123) (CQ(J), J=N2, N3, NPH2)                       M20.5569
      WRITE (LUNIT6, 123) (CQ(J), J=N4, N5, NPH2)                       M20.5570
      N2 = N3 + NPH2                                                    M20.5571
      IF (N3 .LT. N22) GO TO 122                                        M20.5572
  123 FORMAT (1X, 12(2X,F8.2))                                          M20.5573
  124 CONTINUE                                                          M20.5574
      WRITE( LUNIT6,119 )                                               M35.9198
  119 FORMAT(//, 42X,  21HMODE TO PHASE CURRENT  ,/,                    M35.9199
     1    42X,  21HTRANSFORMATION MATRIX  ,/,  1X  )                    M35.9200
      DO 126 I=1, NPH2, 2                                               M20.5575
      N2 = I                                                            M20.5576
      WRITE (LUNIT6,121)                                                M20.5577
  125 N3 = N2 + N1                                                      M20.5578
      IF (N3 .GT. N22) N3 = N22                                         M20.5579
      N4 = N2 + 1                                                       M20.5580
      N5 = N3 + 1                                                       M20.5581
      WRITE (LUNIT6, 121)                                               M20.5582
      WRITE (LUNIT6, 123) (ZYA(J), J=N2, N3, NPH2)                      M20.5583
      WRITE (LUNIT6, 123) (ZYA(J), J=N4, N5, NPH2)                      M20.5584
      N2 = N3 + NPH2                                                    M20.5585
      IF (N3 .LT. N22) GO TO 125                                        M20.5586
  126 CONTINUE                                                          M20.5587
  127 WRITE (LUNIT6, 141 )                                              M20.5588
  141 FORMAT ( /,  22X,  21HMODE TO PHASE VOLTAGE,  44X,                M16.6044
     1 21HMODE TO PHASE CURRENT,  /,  22X,  21HTRANSFORMATION MATRIX,   M16.6045
     2 44X,  21HTRANSFORMATION MATRIX,  /,  1X  )                       M16.6046
      DO 17  I=1, NPH2, 2                                               M16.6047
      WRITE (LUNIT6, 116)                                               M16.6048
  116 FORMAT ( 65X, 1H*  )                                              M16.6049
      WRITE (LUNIT6, 109)  ( CQ(J), J=I, N22, NPH2 )                    M16.6050
  109 FORMAT ( 1X,  6(2X, F8.5)  )                                      M16.6051
      WRITE (LUNIT6, 110)  ( ZYA(J), J=I, N22, NPH2 )                   M16.6052
  110 FORMAT ( 1H+, 64X, 1H*, 1X, 6(2X, F8.5)  )                        M16.6053
      K = I + 1                                                         M16.6054
      WRITE (LUNIT6, 109)  ( CQ(J), J=K, N22, NPH2 )                    M16.6055
   17 WRITE (LUNIT6, 110)  ( ZYA(J), J=K, N22, NPH2 )                   M16.6056
C     OUTPUT EIGENVECTORS, ATTENUATION, AND VELOCITY                    M16.6057
   18 IOUT1 = IOUTP                                                     M16.6058
      IOUTP = IOUTP / 2                                                 M16.6059
      IF ( IOUTP + IOUTP   .EQ.   IOUT1 )   GO TO 21                    M16.6060
      IF (KREQAB .EQ. 1)   GO TO 39138                                  M16.6061
      WRITE (LUNIT6, 138)                                               M16.6062
  138 FORMAT ( /, 20X,  5HALPHA,  13X,  4HBETA,  9X,                    M16.6063
     1 11HATTENUATION,  8X,  8HVELOCITY,  /,  9X,  4HMODE,  5X,         M16.6064
     2 10H(NEPER/MI),  6X,  12H(RADIANS/MI),  7X,  7H(DB/MI),           M16.6065
     3 9X,  10H(MILE/SEC)    ,/,  1X  )                                 M16.6066
      GO TO 39168                                                       M16.6067
39138 WRITE (LUNIT6, 39148)                                             M16.6068
39148 FORMAT ( /, 20X,  5HALPHA,  13X,  4HBETA,  9X,                    M16.6069
     1 11HATTENUATION,  8X,  8HVELOCITY,  /,  9X,  4HMODE,  5X,         M16.6070
     2 10H(NEPER/M ),  6X,  12H(RADIANS/M ),  7X,  7H(DB/M ),           M16.6071
     3 9X, 10H  (M/SEC)     ,/, 1X)                                     M16.6072
39168 DO 20  I = 1, NPH                                                 M16.6073
      J = I + I                                                         M16.6074
      ZYR = CONV5 * G(J-1)                                              M16.6075
      ZYI = W / G(J)                                                    M16.6076
   20 WRITE (LUNIT6, 115)  I, G(J-1), G(J), ZYR, ZYI                    M16.6077
  115 FORMAT ( 10X, I2,  4(5X, E12.5)  )                                M16.6078
   21 IWORK = IWORK / 2                                                 M16.6079
      N2 = IOUTP / 16                                                   M16.6080
      N1 = IWORK / 16                                                   M16.6081
      IF ( 16*N1   .EQ.   IWORK )   GO TO 35                            M16.6082
      N = 1                                                             M16.6083
      DO 2156   J = 1, N22, NPH2                                        M16.6084
      K = J + NPH2 - 1                                                  M16.6085
      L = N                                                             M16.6086
      DO 2056  I = J, K, 2                                              M16.6087
      CQT(L) = CQ(I)                                                    M16.6088
      CQT(L+1) = CQ(I+1)                                                M16.6089
      L = L + NPH2                                                      M16.6090
 2056 CONTINUE                                                          M16.6091
      N = N + 2                                                         M16.6092
 2156 CONTINUE                                                          M16.6093
      CALL CXC(Z,ZYA,ZYD,LL1)                                           M18.1038
      CALL CXC(Y,CQ,ZYC,LL1)                                            M18.1039
      CALL CXC(ZYB,ZYD,Z,LL1)                                           M18.1040
      CALL CXC(CQT,ZYC,Y,LL1)                                           M18.1041
      J = 1                                                             M16.6098
      DO 19121 I = 1, NPH                                               M16.6099
      Y(J) = 0.0                                                        M16.6100
19121 J = J + NPHPI2                                                    M16.6101
      IF ( ISS  .EQ.  0 )   GO TO 23                                    M16.6102
      DO 1976  I=1, NPH2                                                M16.6103
 1976 G60(I) = G(I)                                                     M16.6104
      J = 1                                                             M16.6105
      DO 22  I=1, N22, NPHPI2                                           M16.6106
      PBUF(J) = SLL*Z(I)                                                M16.6107
      PBUF(J+1) = SLL*Z(I+1)                                            M16.6108
      PBUF(J+2) = SLL*Y(I)                                              M16.6109
      PBUF(J+3) = SLL * Y(I+1)/TENM6                                    M16.6110
   22 J= J + 14                                                         M16.6111
   23 I = IWORK / 2                                                     M16.6112
      N1 = I - 8 * (I/8)                                                M16.6113
      I = I / 2                                                         M16.6114
      N2 = I - 4 * ( I / 4 )                                            M16.6115
      I = 2 * ( IOUTP / 2 )                                             M16.6116
      N3 = IOUTP - I                                                    M16.6117
      J = 4 * ( I / 4 )                                                 M16.6118
      N4 = I - J                                                        M16.6119
      N5 = J - 8 * ( J / 8 )                                            M16.6120
      I1 = N3 + N4 + N5                                                 M16.6121
      IF (I1 .EQ. 0)   GO TO 49500                                      M16.6122
      IF (KREQAB .EQ. 1)   GO TO 49410                                  M16.6123
      WRITE (LUNIT6, 137)                                               M16.6124
  137 FORMAT (/, 9X,  18HDISTRIBUTED SERIES,  7X,  17HDISTRIBUTED SHUNT M16.6125
     1  ,10X,  14HCHARACTERISTIC,  8X,  20HEQUIVALENT PI SERIES,        M16.6126
     2 5X,  19HHALF EQUIV PI SHUNT,  /,  9X,                            M16.6127
     3 19HIMPEDANCE(OHM/MILE),  5X,  20HADMITTANCE(MHO/MILE),           M16.6128
     4 8X,  15HIMPEDANCE(OHMS),  9X,  15HIMPEDANCE(OHMS),               M16.6129
     5 10X,  16HADMITTANCE(MHOS),  /,  5H MODE,                         M16.6130
     6 5(5X, 4HREAL, 8X, 4HIMAG, 4X)    ,/,  1X  )                      M16.6131
      GO TO 49500                                                       M16.6132
49410 WRITE (LUNIT6, 49420)                                             M16.6133
49420 FORMAT (/, 9X,  18HDISTRIBUTED SERIES,  7X,  17HDISTRIBUTED SHUNT M16.6134
     1  ,10X,  14HCHARACTERISTIC,  8X,  20HEQUIVALENT PI SERIES,        M16.6135
     2 5X,  19HHALF EQUIV PI SHUNT,  /,  9X,                            M16.6136
     3 19H IMPEDANCE(OHM/M)  , 5X, 20H ADMITTANCE(MHO/M)  ,             M16.6137
     4 8X,  15HIMPEDANCE(OHMS),  9X,  15HIMPEDANCE(OHMS),               M16.6138
     5 10X,  16HADMITTANCE(MHOS),  /,  5H MODE,                         M16.6139
     6 5(5X, 4HREAL, 8X, 4HIMAG, 4X)    ,/,  1X  )                      M16.6140
49500 J = 1                                                             M16.6141
      CALL MOVER0 ( ZYB, N22 )                                          M16.6142
      CALL MOVER0 ( ZYC, N22 )                                          M16.6143
      DO 24  I=1, NPH                                                   M16.6144
      IF (I1 .NE. 0) WRITE (LUNIT6,111) I                               M16.6145
  111 FORMAT ( 3X, I1 )                                                 M16.6146
      IF ( N3  .NE.  0 )                                                M16.6147
     1 WRITE (LUNIT6, 112 )  Z(J), Z(J+1), Y(J), Y(J+1)                 M16.6148
  112 FORMAT ( 1H+, 5X, 2E12.5, 1X, 2E12.5 )                            M16.6149
      IF ( N1  .EQ.  0 )   GO TO 24                                     M16.6150
      L = 2 * I - 1                                                     M24. 781
      ZR = G(L+1)/Y(J+1)                                                M24. 782
      ZI = - G(L)/Y(J+1)                                                M24. 783
  205 K = I + I                                                         M19.2489
      DX  =  ZR * ZR  +  ZI * ZI                                        M16.6159
      YO(K-1) = ZR/DX                                                   M16.6160
      YO(K) = -ZI/DX                                                    M16.6161
      IF ( N4  .NE.  0 )                                                M16.6162
     1 WRITE (LUNIT6, 113 )  ZR, ZI                                     M16.6163
  113 FORMAT ( 1H+, 55X, 2E12.5 )                                       M16.6164
      IF ( N2  .EQ.  0 )   GO TO 24                                     M16.6165
      D1 = Z(J)*Y(J)  -  Z(J+1)*Y(J+1)                                  M16.6166
      D2 = Z(J)*Y(J+1)  +  Z(J+1)*Y(J)                                  M16.6167
      DX = SQRTZ ( D1*D1  +  D2*D2 )                                    M18.1045
      IF (D1 .LT. 0.0)   GO TO 202                                      M19.2490
      D3 = SQRTZ((D1 + DX) / 2.)                                        M19.2491
      D4 = D2 / D3 / 2.                                                 M19.2492
      GO TO 203                                                         M19.2493
  202 D4 = SQRTZ((-D1 +DX) / 2.)                                        M19.2494
      IF ( D2  .LT.  0.0 )     D4 = -D4                                 M16.6171
      D3 = D2 / D4 /2.                                                  M19.2495
  203 D3 = SLL * D3                                                     M19.2496
      D4 = SLL * D4                                                     M19.2497
      DX = EXPZ ( D3 )                                                  M18.1048
      D3 = DX * COSZ(D4 )                                               M18.1049
      D4 = DX * SINZ(D4 )                                               M18.1050
      DX = D3*D3 + D4*D4                                                M16.6175
      D1 = ( D3 - D3/DX ) / 2.0                                         M16.6176
      D2 = ( D4 + D4/DX ) / 2.0                                         M16.6177
      ZYB(J) = D1*ZR - D2*ZI                                            M16.6178
      ZYB(J+1) = D1*ZI + ZR*D2                                          M16.6179
      DIAG = D3 - 1.0                                                   M16.6180
      D1 = DIAG*DIAG - D4*D4                                            M16.6181
      D2 = 2.0 * DIAG * D4                                              M16.6182
      DX = 2.0 * DX                                                     M16.6183
      ZYR  =  ( D1*D3 + D2*D4 ) / DX                                    M16.6184
      ZYI = ( D2*D3 - D1*D4 ) / DX                                      M16.6185
      DX  =  ZYB(J) * ZYB(J)  +  ZYB(J+1) * ZYB(J+1)                    M16.6186
      ZYC(J)  =  ( ZYR*ZYB(J)  +  ZYI * ZYB(J+1) ) / DX                 M16.6187
      ZYC(J+1)  =  ( ZYI * ZYB(J)  -  ZYR * ZYB(J+1) ) / DX             M16.6188
      IF ( N5  .NE.  0 )                                                M16.6189
     1 WRITE (LUNIT6, 114)  ZYB(J), ZYB(J+1), ZYC(J), ZYC(J+1)          M16.6190
  114 FORMAT ( 1H+, 80X, 2E12.5, 1X, 2E12.5 )                           M16.6191
   24 J = J + NPHPI2                                                    M16.6192
      IWORK = IWORK / 8                                                 M16.6193
      N2 = IOUTP / 16                                                   M16.6194
      N1 = IWORK / 2                                                    M16.6195
      IF ( N1+N1  .EQ.  IWORK )   GO TO 35                              M16.6196
      CALL CXC(CQ,ZYB,Y,LL4)                                            M18.1051
      CALL CXC(Y,CQ,Z,LL3)                                              M18.1052
      CALL CXC(ZYA,ZYC,ZYB,LL4)                                         M18.1053
      CALL CXC(ZYB,ZYA,Y,LL3)                                           M18.1054
      IOUTP = IOUTP / 8                                                 M16.6201
      IOUT1 = IOUTP                                                     M16.6202
      IOUTP = IOUTP / 2                                                 M16.6203
      IF ( IOUTP + IOUTP   .EQ.   IOUT1 )   GO TO 13                    M16.6204
      WRITE (LUNIT6, 132 )                                              M16.6205
  132 FORMAT ( /, 9X,  20HPI EQUIVALENT SERIES, 23X,                    M16.6206
     1 19HHALF PI EQUIV SHUNT   ,/,  8X,  22HIMPEDANCE MATRIX(OHMS),    M16.6207
     2 20X, 23HADMITTANCE MATRIX(MHOS)   )                              M16.6208
   27 IF (NPH .LE. 6) GO TO 157                                         M20.5589
      NPHPI = NPH + 1                                                   M20.5590
      N1 = 5 * NPH2                                                     M20.5591
      DO 154 I=1, NPH2, 2                                               M20.5592
      N2 = I                                                            M20.5593
      N3 = (I - 1) * NPHPI + 1                                          M20.5594
      WRITE (LUNIT6, 121)                                               M20.5595
  151 N4 = N2 + N1                                                      M20.5596
      IF (N4 .LT. N3) N4 = N3                                           M20.5597
      WRITE (LUNIT6,152) (Z(J), J=N2, N4, NPH2)                         M20.5598
      N5 = N2 + 1                                                       M20.5599
      N6 = N4 + 1                                                       M20.5600
      WRITE (LUNIT6, 153) (Z(J), J=N5, N6, NPH2)                        M20.5601
      N2 = N4 + NPH2                                                    M20.5602
      IF ( N4 .LT. N3) GO TO 151                                        M20.5603
  152 FORMAT(/8(3X,E12.5))                                              M20.5604
  153 FORMAT( 8(3X,E12.5))                                              M20.5605
  154 CONTINUE                                                          M20.5606
      WRITE (LUNIT6, 121)                                               M20.5607
      DO 156 I=1, NPH2, 2                                               M20.5608
      N2 = I                                                            M20.5609
      N3 = (I - 1) * NPHPI + 1                                          M20.5610
      WRITE (LUNIT6, 121)                                               M20.5611
  155 N4 = N2 + N1                                                      M20.5612
      WRITE (LUNIT6, 152) (Y(J), J=N2, N4, NPH2)                        M20.5613
      N5 = N2 + 1                                                       M20.5614
      N6 = N4 + 1                                                       M20.5615
      WRITE (LUNIT6, 153) (Y(J), J=N5, N4, NPH2)                        M20.5616
      N2 = N4 + NPH2                                                    M20.5617
      IF (N4 .LT. N3) GO TO 155                                         M20.5618
  156 CONTINUE                                                          M20.5619
      GO TO 13                                                          M20.5620
  157 N2 = 1                                                            M20.5621
      N3 = 1                                                            M16.6210
      DO 12  N1=1, NPH                                                  M16.6211
      WRITE (LUNIT6, 108 )                                              M16.6212
  108 FORMAT ( 1X )                                                     M16.6213
    4 GO TO (5, 6, 7, 8, 9, 10 ), N1                                    M16.6214
    5 WRITE (LUNIT6, 101)   ( Z(I), I=N2, N3, NPH2 ),                   M16.6215
     1                 ( Y(I), I=N3, N22, NPH2 )                        M16.6216
  101 FORMAT ( 1(3X, E12.5), 25X, 6(3X, E12.5)  )                       M16.6217
      GO TO 11                                                          M16.6218
    6 WRITE (LUNIT6, 102)   ( Z(I), I=N2, N3, NPH2 ),                   M16.6219
     1                 ( Y(I), I=N3, N22, NPH2 )                        M16.6220
  102 FORMAT ( 2(3X, E12.5), 25X, 5(3X, E12.5)  )                       M16.6221
      GO TO 11                                                          M16.6222
    7 WRITE (LUNIT6, 103)   ( Z(I), I=N2, N3, NPH2 ),                   M16.6223
     1                 ( Y(I), I=N3, N22, NPH2 )                        M16.6224
  103 FORMAT ( 3(3X, E12.5), 25X, 4(3X, E12.5)  )                       M16.6225
      GO TO 11                                                          M16.6226
    8 WRITE (LUNIT6, 104)   ( Z(I), I=N2, N3, NPH2 ),                   M16.6227
     1                 ( Y(I), I=N3, N22, NPH2 )                        M16.6228
  104 FORMAT ( 4(3X, E12.5), 25X, 3(3X, E12.5)  )                       M16.6229
      GO TO 11                                                          M16.6230
    9 WRITE (LUNIT6, 105)   ( Z(I), I=N2, N3, NPH2 ),                   M16.6231
     1                 ( Y(I), I=N3, N22, NPH2 )                        M16.6232
  105 FORMAT ( 5(3X, E12.5), 25X, 2(3X, E12.5)  )                       M16.6233
      GO TO 11                                                          M16.6234
   10 WRITE (LUNIT6, 106)   ( Z(I), I=N2, N3, NPH2 ),                   M16.6235
     1                 ( Y(I), I=N3, N22, NPH2 )                        M16.6236
  106 FORMAT ( 6(3X, E12.5), 25X, 1(3X, E12.5)  )                       M16.6237
   11 N2 = N2 + 1                                                       M16.6238
      N3 = N3 + 1                                                       M16.6239
      IF ( N2  .LE.  N1+N1 )   GO TO 4                                  M16.6240
   12 N3 = N3 + NPH2                                                    M16.6241
   13 IF ( IWORK  .EQ.  0 )   RETURN                                    M16.6242
      IF ( IWORK  .GT.  0 )   GO TO 31                                  M16.6243
      IWORK = -IWORK                                                    M16.6244
      CALL CXC(Z,Y,ZY,LL1)                                              M18.1055
      IF ( IPRSUP  .LT.  5 )   GO TO 29                                 M16.6246
      WRITE (LUNIT6, 133 )                                              M16.6247
  133 FORMAT ( /, 5X,   22HTRANSPOSE OF ZY MATRIX    ,/,  1X  )         M16.6248
      DO 28  I=1, N22, NPH2                                             M16.6249
      J = I + NPH2 - 1                                                  M16.6250
   28 WRITE (LUNIT6, 107)  ( ZY(K),K=I, J)                              M16.6251
   29 GO TO 30                                                          M16.6252
   31 CALL MOVER0 ( CQ(1), N22 )                                        M16.6253
      CALL MOVER0 ( ZYA(1), N22 )                                       M16.6254
      I = 1                                                             M16.6255
      DO  32  J=1, N22, 2                                               M16.6256
      CQ(J) = Q(I)                                                      M16.6257
      ZYA(J) = QI(I)                                                    M16.6258
   32 I = I + 1                                                         M16.6259
      CALL CXC(ZYA,ZY,ZYB,LL5)                                          M18.1056
      CALL CXC(ZYB,CQ,ZYC,LL2)                                          M18.1057
      I = 1                                                             M16.6262
      DO 33  J=1, NPH2, 2                                               M16.6263
      DX = SQRTZ( ZYC(I) * ZYC(I) + ZYC(I+1) * ZYC(I+1) )               M18.1058
      IF (ZYC(I) .LT. 0.0)   GO TO 206                                  M19.2498
      G(J) = SQRTZ ( ( ZYC(I) + DX ) / 2. )                             M18.1059
      G(J+1) = ZYC(I+1)/G(J)/2.                                         M19.2499
      GO TO 33                                                          M19.2500
  206 G(J+1) = SQRTZ((-ZYC(I) + DX) /2.)                                M19.2501
      IF ( ZYC(I+1) .LT. 0. ) G(J+1) = - G(J+1)                         M16.6267
      G(J) = ZYC(I+1)/G(J+1)/2.                                         M19.2502
   33 I = I + NPHPI2                                                    M16.6268
      IF (IOUTP .NE. 0)                                                 M16.6269
     1 WRITE (LUNIT6, 140 )                                             M16.6270
  140 FORMAT ( /,  10X,   77HTHE FOLLOWING VALUES ARE COMPUTED BY USE OFM16.6271
     1 CONSTANT TRANSFORMATION MATRICES.   )                            M16.6272
      CALL CXC(CQ,ZYB,ZYA,LL6)                                          M18.1061
      IF (NSS .EQ. 1)   GO TO 19763                                     M16.6273
      L = 1                                                             M16.6274
      DO 63 I=1, NPH                                                    M16.6275
      J = (I-1) * NPH2 + 1                                              M16.6276
      K = 2 * I - 1                                                     M16.6277
      DO 62  N1=1, I                                                    M16.6278
      Z(J) = XR(L)                                                      M16.6279
      Z(K) = XR(L)                                                      M16.6280
      Y(J) = XG(L)                                                      M16.6281
      Y(K) = XG(L)                                                      M16.6282
      Z(J+1) = XL(L)                                                    M16.6283
      Z(K+1) = Z(J+1)                                                   M16.6284
      Y(J+1) = XC(L)                                                    M16.6285
      Y(K+1) = Y(J+1)                                                   M16.6286
      J = J + 2                                                         M16.6287
      K = K + NPH2                                                      M16.6288
   62 L = L + 1                                                         M16.6289
   63 CONTINUE                                                          M16.6290
19763 IF (ISS .EQ. 0)   GO TO 18                                        M16.6292
      DO 57 I=1, NPH2                                                   M16.6293
   57 G60(I) = G(I)                                                     M16.6294
      GO TO 18                                                          M16.6295
   35 IWORK = N1                                                        M16.6296
      IOUTP = N2                                                        M16.6297
      GO TO 13                                                          M16.6298
 9200 RETURN                                                            M16.6299
      END                                                               M16.6300
      FUNCTION XIFT( TSP, OMEGAS, FUNW, COSI )                          M32, 559
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL45.FOR;/NOLIST'                                           
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
C  THIS ROUTINE CALCULATES THE INVERSE FOURIER TRANSFORM OF A GEOMETRIC M20.5625
C     SERIES OF FREQUENCIES IN OMEGAS  WITH THE POINTS IN THE FREQUENCY M20.5626
C     DOMAIN IN FUNW(NFR1).  LINEAR INTERPOLATION BETWEEN THE FREQUENCY M20.5627
C     SEQUENCE IS USED IN ORDER TO PERFORM THE NUMERICAL INTEGRATION.   M20.5628
C     NOTE.  THE FIRST POINT IN 'FUNW' CORRESPONDS TO ZERO PREQUENCY.   M20.5629
C     THIS VALUE, OF COURSE, IS NOT MULTIPLIED BY (1/(J*W)) TO CONVERT  M20.5630
C     FROM IMPULSE TO STEP RESPONSE.  THE LIMIT OF SIN(W*T)/W FOR W = 0.M20.5631
C     BEING EQUAL TO T IS USED.                                         M20.5632
      DIMENSION OMEGAS(1), FUNW(1)                                      M32, 560
      DOUBLE PRECISION XTMP, TTFT, TIME, DTEMP, COSI(1)                 M35.9201
      EQUIVALENCE (INDTV(10), NPAN)                                     M20.5634
      DATA N1 / 3 /                                                     M20.5635
      TIME = TSP                                                        M32, 562
      IF (NPAN .GT. 0) GO TO 40004                                      M20.5636
      TTFT = TIME * TIME                                                M20.5637
      DO 40002 J = 1, NFR                                               M32, 563
      XTMP = TIME * OMEGAS( J+1 )                                       M32, 564
      NW =  XTMP / TWOPI                                                M32, 565
      XTMP =  XTMP - NW * TWOPI                                         M32, 566
40002 CALL DSINZ( XTMP, COSI(J) )                                       M35.9202
      XTMP = OMEGAS(2) * TIME                                           M32, 568
      CALL DCOSZ( XTMP, DTEMP )                                         M35.9203
      XTMP = XTMP * ONEHAF * ( FUNW(1) * TTFT + FUNW(2) * TIME *        M32, 569
     1 ( 2. * DTEMP / XTMP + COSI(1) ) )                                M35.9204
     2 - RATIO / DMIN * FUNW(2) * ( COSI(2) - COSI(1) ) / OMEGAS(3)     M32, 571
      DO 40003 J = 3, NFR                                               M32, 572
      XTMP = XTMP + FUNW(J) * ( DPLU * COSI(J-1) - RATIO * COSI(J-2)    M32, 573
     1   - COSI(J) ) / ( DMIN * OMEGAS(J) )                             M32, 574
40003 CONTINUE                                                          M20.5647
      XIFT = PI2 * XTMP / TTFT                                          M32, 575
      RETURN                                                            M20.5649
40004 IF (IPRSUP .EQ. 0) N1 = 0                                         M20.5650
      XIFT = 0.0                                                        M20.5651
      W  = 0.0                                                          M20.5652
      D4 = FUNW(1)                                                      M20.5653
      D3 = OMEGAS(1)                                                    M20.5654
      D2 = FUNW(2)                                                      M20.5655
      D1 = OMEGAS(2)                                                    M20.5656
      D7 = D2 * D1                                                      M20.5657
      D5 = (D7 - D4) / (D1 - D3)                                        M20.5658
      D6 = D4 - D5 * D3                                                 M20.5659
      D7 = ONEHAF * TIME * D4                                           M20.5660
      DELTAW = (OMEGAS(2) - OMEGAS(1)) / NPAN                           M20.5661
      DO 5 I=1, NPAN                                                    M20.5662
      XIFT = XIFT + D7                                                  M20.5663
      W = W + DELTAW                                                    M20.5664
      XTMP = W * TIME                                                   M35.9205
      NW = XTMP / TWOPI                                                 M35.9206
      D7 = XTMP - NW * TWOPI                                            M35.9207
    5 D7 = ( D5 * W + D6 ) * SINZ( D7 ) / W                             M35.9208
      D7 = ONEHAF * D7                                                  M20.5666
      XIFT = DELTAW * (XIFT + D7)                                       M20.5667
      D4 = (OMEGAS(3) - OMEGAS(2)) / NPAN                               M20.5668
      DELTAW = XPAN / ABSZ(TSP)                                         M35.9209
      I = 1                                                             M20.5670
      IF (N1 .GT. 0) WRITE (LUNIT6,100) NPAN, XPAN, PI2, DELTAW, TIME,  M20.5671
     1     XIFT, D1, D2, D3, D4, D5, D6, D7, (OMEGAS(I), FUNW(I), I=1,5)M20.5672
  100 FORMAT(/,51H BEFORE INTEGRATION LOOP.  NPAN, XPAN, PI2, DELTAW,  ,M20.5673
     1   60HTIME, XIFT, D1, D2, D3, D4, D5, D6, D7, (OMEGAS(I), FUNW(I),M20.5674
     2  ,11H I=1, 5) =   ,/,I10,6(1X,E20.10)/(6(1X,E20.10)))            M20.5675
      IF (DELTAW .LE. D4) GO TO 40                                      M20.5676
      DO 30 I=2, NFR                                                    M20.5677
      D3 = 0.                                                           M20.5678
      D5 = (FUNW(I+1) - D2) / (OMEGAS(I+1) - D1)                        M20.5679
      D6 = D2 - D5*D1                                                   M20.5680
      D1 = OMEGAS(I+1)                                                  M20.5681
      D2 = FUNW(I+1)                                                    M20.5682
      DO 10 K=1, NPAN                                                   M20.5683
      D3 = D3 + D7                                                      M20.5684
      W = W + D4                                                        M20.5685
      XTMP = W * TIME                                                   M35.9210
      NW = XTMP / TWOPI                                                 M35.9211
      D7 = XTMP - NW * TWOPI                                            M35.9212
   10 D7 = ( D5 * W + D6 ) * SINZ( D7 )                                 M35.9213
      D7 = ONEHAF * D7                                                  M20.5687
      XIFT = XIFT + D4 * (D3 + D7)                                      M20.5688
      IF (N1 .GT. 0) WRITE (LUNIT6, 200) I, W, DELTAW, OMEGAS(I),       M20.5689
     1     FUNW(I), XIFT, D1, D2, D3, D4, D5, D6, D7                    M20.5690
  200 FORMAT(/,52H INSIDE IFT LOOP.  I, W, DELTAW, OMEGAS(I), FUNW(I), ,M20.5691
     1   36HXIFT, D1, D2, D3, D4, D5, D6, D7 =    ,/,                   M20.5692
     3     I10,6(1X,E20.10)/(6(1X,E20.10)))                             M20.5693
      D4 = (OMEGAS(I+2) - D1) / NPAN                                    M20.5694
      IF (D4 .GT. DELTAW) GO TO 40                                      M20.5695
   30 CONTINUE                                                          M20.5696
      GO TO 70                                                          M20.5697
   40 I = I + 1                                                         M20.5698
      D3 = D7                                                           M20.5699
      DO 60 J=I, NFR                                                    M20.5700
      D5 = (FUNW(J+1) - D2) / (OMEGAS(J+1) - D1)                        M20.5701
      D6 = D2 - D5 * D1                                                 M20.5702
      D1 = OMEGAS(J+1)                                                  M20.5703
      D2 = FUNW(J+1)                                                    M20.5704
      IF (N1 .GT. 0) WRITE (LUNIT6, 200) I, W, DELTAW, OMEGAS(I),       M20.5705
     1     FUNW(I), XIFT, D1, D2, D3, D4, D5, D6, D7                    M20.5706
   50 IF (W .GT. D1) GO TO 60                                           M20.5707
      D3 = D3 + D7                                                      M20.5708
      W = W + DELTAW                                                    M20.5709
      XTMP = W * TIME                                                   M32, 576
      NW = XTMP / TWOPI                                                 M32, 577
      D7 = XTMP - NW * TWOPI                                            M35.9214
      D7 = ( D5 * W + D6 ) * SINZ( D7 )                                 M35.9215
      GO TO 50                                                          M20.5711
   60 CONTINUE                                                          M20.5712
      XIFT = XIFT + DELTAW * (D3 - ONEHAF * D7)                         M20.5713
   70 XIFT = PI2 * XIFT                                                 M20.5714
      IF (N1 .GT. 0) WRITE (LUNIT6,300) TIME, XIFT                      M20.5715
  300 FORMAT (/,24H  IFT OF FUNW(W) AT T = ,E20.10,4H IS ,E20.10)       M20.5716
      IF (N1 .GT. 0) N1 = N1 - 1                                        M20.5717
      RETURN                                                            M20.5718
      END                                                               M20.5719
      SUBROUTINE RISE(TIME, THR, T2, VRESP, SI, COSI)                   M28.7300
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS ROUTINE IS USED TO ESTIMATE THE TIME AT WHICH THE STEP       M20.5721
C     RESPONSE IN THE TIME DOMAIN IS AT VALUE 'THR'(0.0 TO 1.0).        M20.5722
C     THE INITIAL GUESS IS IN 'TIME' WHEN THE  'RISE' FUNCTION IS CALLEDM20.5723
C     AND THE FINAL APPROXIMATION IS RETURNED THRU THE SAME ARGUMENT.   M20.5724
C     THE PRINCIPLE OF OPERATION IS LINEAR INTERPOLATION/EXTRAPOLATION  M20.5725
C     TO FIND A NEW TIME CLOSER TO 'THR'.                               M20.5726
C                                                                       M20.5727
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL45.FOR;/NOLIST'                                           
      INCLUDE  'VOLT45.FOR;/NOLIST'                                           
      DIMENSION SI(1), VRESP(1)                                         M35.9216
      DOUBLE PRECISION COSI(1)                                          M35.9217
      EQUIVALENCE (IPNTV(1), NFIT),  (VIM(5), EPSRSE)                   M28.7301
      N1 = NFIT                                                         M20.5732
      TEMP = TIME                                                       M29.5483
      MITT = 0                                                          M29.5484
 7403 X1 = XIFT ( TIME, SI, VRESP, COSI )                               M29.5485
      TARGET = FFIN * ABSZ(THR)                                         M23.5808
      CLOSE = FFIN * EPSRSE                                             M20.5737
   15 T1 = TIME + T2                                                    M23.5809
      IF( X1 .GT. TARGET  ) T1 = TIME - T2                              M29.5486
      IF ( T1 .LE. 0.)  T1 = TIME / 10.                                 M29.5487
      X2 = XIFT(T1, SI, VRESP, COSI)                                    M20.5739
      IF ( IPRSUP .GE. 4)                                               M29.5488
     1  WRITE (LUNIT6, 1)                                               M29.5489
    1 FORMAT ( /, 10X,2HX2,13X,2HT1,13X,2HX1,12X,4HTIME)                M29.5490
      IF ( IPRSUP .GE. 4)                                               M29.5491
     1 WRITE (LUNIT6, 2) X2, T1, X1, TIME                               M29.5492
    2 FORMAT ( 4E15.5)                                                  M29.5493
      IF ( (X2-X1)/(T1-TIME)  .LT.  0. )   GO TO 30                     M28.7303
      IF( X1 .GE. TARGET .AND. X2 .LE. TARGET ) GO TO 5                 M23.5812
      IF( X1 .LE. TARGET .AND. X2 .GE. TARGET ) GO TO 5                 M23.5813
   30 TIME = T1                                                         M28.7304
      X1 = X2                                                           M23.5815
      IF( TIME .LT. 0. .OR. TIME .GT. 2. * TT ) GO TO 25                M23.5816
      GO TO 15                                                          M23.5817
   25 IF ( MITT .GT. 3 .OR. ICTRL .GE. 0 ) GO TO 26                     M29.5494
      THR = THR * ONEHAF                                                M29.5495
      TIME = TEMP                                                       M29.5496
      MITT = MITT + 1                                                   M29.5497
      GO TO 7403                                                        M29.5498
   26 KILL = 156                                                        M29.5499
      LSTAT(11) = NFIT                                                  M23.5819
      LSTAT(12) = 1                                                     M23.5820
      RETURN                                                            M23.5821
    5 T2 = ( TIME + T1 ) / 2.                                           M28.7305
      X3 = XIFT( T2, SI, VRESP, COSI )                                  M28.7306
      IF ( IPRSUP .GT. 3 )                                              M28.7307
     1 WRITE (LUNIT6,10) N1, X1, X2, X3, TIME, T1, T2                   M28.7308
   10 FORMAT ( /, 41H IN RISE.  N1, X1, X2, X3, TIME, T1, T2 =          M28.7309
     1  ,/,  I10,  6E20.5 )                                             M28.7310
      IF ( ABSZ(X3 - TARGET)  .GT.  CLOSE ) GO TO 40                    M28.7311
      TIME = T2                                                         M28.7312
      RETURN                                                            M28.7313
   40 IF ( X2 .GT. TARGET  .AND.  X3 .LT. TARGET )   GO TO 41           M28.7314
      IF ( X2 .LT. TARGET  .AND.  X3 .GT. TARGET )   GO TO 41           M28.7315
      T1 = T2                                                           M28.7316
      X2 = X3                                                           M28.7317
      N1 = N1 - 1                                                       M28.7318
      GO TO 42                                                          M28.7319
   41 TIME = T2                                                         M28.7320
      X1 = X3                                                           M28.7321
      N1 = N1 - 1                                                       M28.7322
   42 IF ( N1 .GE. NFIT / 2 )   GO TO 5                                 M28.7323
   43 T2 = TIME + ( T1 - TIME ) * ( TARGET - X1 ) / ( X2 - X1 )         M28.7324
      X3 = XIFT( T2, SI, VRESP, COSI )                                  M28.7325
      IF ( IPRSUP .GT. 3 )                                              M28.7326
     1 WRITE (LUNIT6, 10)  N1, X1, X2, X3, TIME, T1, T2                 M28.7327
      IF ( X2 .GT. TARGET .AND. X3 .LT. TARGET )   GO TO 20             M28.7328
      IF ( X2 .LT. TARGET .AND. X3 .GT. TARGET )   GO TO 20             M28.7329
      T1 = TIME                                                         M28.7330
      X2 = X1                                                           M20.5746
   20 TIME = T2                                                         M20.5747
      X1 = X3                                                           M28.7331
      N1 = N1 - 1                                                       M20.5749
      IF (ABSZ(X1 - TARGET) .LT. CLOSE) RETURN                          M20.5750
      IF ( N1 .GT. 0 )   GO TO 43                                       M28.7332
      KILL = 156                                                        M20.5752
      LSTAT(11) =  NFIT                                                 M20.5756
      LSTAT(12) = NFIT - N1 + 1                                         M23.5822
      RETURN                                                            M20.5758
      END                                                               M20.5759
      SUBROUTINE TDFIT(VRESP, SI, FV, HHM, HHN, COSI)                   M20.5760
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS ROUTINE CALCULATES THE TIME DOMAIN SEQUENCE OF POINTS        M20.5761
C     REPRESENTING THE STEP RESPONSE IN 'FV' BY INVERSE FOURIER TRANS-  M20.5762
C     FORMATION OF 'VRESP'.  THIS SEQUENCE IS THEN FITTED BY A LEAST-   M20.5763
C     SQUARES NEWTON-RAPHSON ALGORITHM TO THE FUNCTION                  M20.5764
C                                                                       M20.5765
C     Z(T) = D*(1.0 - EXP(-(T - X(3))/X(1))) + (FFIN - D) *             M20.5766
C                          (1.0 -EXP(-(T-X(3))/X(2)))                   M21. 487
C                                                                       M20.5768
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL45.FOR;/NOLIST'                                           
      INCLUDE  'VOLT45.FOR;/NOLIST'                                           
      EQUIVALENCE (VIM(1),EPS),(VIM(2),EPS1),(VIM(3),FIT2Z)             M20.5771
      EQUIVALENCE (VIM(4),PIVTHR),(VIM(7),FT2EMX),(VIM(9),EPSPV2)       M20.5772
      EQUIVALENCE (IPNTV(2),NITER1),(IPNTV(3),NITER),(INDTV(1),NPOINT)  M29.5500
      DIMENSION VRESP(1), SI(1), FV(1), HHM(1), HHN(1)                  M35.9218
      DOUBLE PRECISION COSI(1)                                          M35.9219
      DIMENSION B(4), DB(3,3), HAC(3,3), E(4), EOLD(3), XOLD(3)         M20.5775
      REAL*8        TEXT7, TEXT8, TEXT9                                 M21. 488
      DATA  TEXT7  / 6H.      /                                         M20.5776
      DATA  TEXT8  / 6H0      /                                         M20.5777
      DATA  TEXT9  / 6H*      /                                         M20.5778
      KTDIV = 0                                                         M32, 580
      KTZER = 0                                                         M32, 581
      KCRIT = NPOINT * 0.8                                              M29.5501
40066 NO = TT / TSTEP + ONEHAF                                          M29.5502
      KMAX = 0                                                          M29.5503
      TSTEP = TT / NO                                                   M20.5780
      KZERO = 0                                                         M28.7333
C     CALCULATE (BY IFT) STEP RESPONSE IN TIME DOMAIN                   M20.5781
      IF (IPRSUP .GT. 3)                                                M20.5782
     1     WRITE (LUNIT6, 70560)                                        M20.5783
70560 FORMAT(/,5X,13HOUTPUT OF IFT,/,9X,1HT,11X,5HFV(K),/,1X)           M20.5784
      T = TSTEP                                                         M20.5785
      IF( ICTRL .LT. 0 ) T = 5. * TSTEP                                 M35.9220
      FVMX = - FLTINF                                                   M20.5786
      FVMIN = FFIN/1000.                                                M35.9221
      KMP = 1                                                           M28.7334
40063 DO 40050  K = KMP, NPOINT                                         M28.7335
      FV(K) = XIFT(T, SI, VRESP, COSI)                                  M20.5788
      IF ( FV(K) .LE. FVMX ) GO TO 40065                                M29.5504
      FVMX = FV(K)                                                      M29.5505
      KMAX = K                                                          M29.5506
40065 IF ( FV(K) .GE. FVMIN )  GO TO 40064                              M35.9222
      KZERO = K                                                         M28.7337
      FVMX = -FLTINF                                                    M28.7338
40064 IF ( IPRSUP .GT. 3 )   WRITE (LUNIT6, 70510)  T, FV(K)            M28.7339
      T = T + TSTEP                                                     M20.5791
40050 CONTINUE                                                          M20.5792
      IF( ICTRL .LT. 0 ) GO TO 40061                                    M35.9223
      IF ( KZERO  .LE.  0 )   GO TO 40061                               M28.7340
      KTZER = KTZER + 1                                                 M32, 582
      IF ( KTZER .LE. 5 ) GO TO 40069                                   M32, 583
      WRITE ( LUNIT6, 40070 )                                           M32, 584
40070 FORMAT ( 86H TROUBLE IN TDFIT - - PORTIONS OF IFT RESULTS HAVE BEEM32, 585
     2N DISCARDED MORE THAN FIVE TIMES ,/,  29H IN SEARCH FOR POSITIVE DM32, 586
     3ATA.,  35H   TEMPORARY ERROR STOP IN "TDFIT".   )                 M35.9224
      CALL STOPTP                                                               
40069 KM = NPOINT - KZERO                                               M32, 589
      KMAX = KMAX - KZERO                                               M29.5508
      DO 40062 K = 1, KM                                                M28.7342
      KP = K + KZERO                                                    M28.7343
40062 FV(K) = FV(KP)                                                    M28.7344
      KMP = KM + 1                                                      M28.7345
      TSTRT = TSTRT  +  KZERO * TSTEP                                   M28.7346
      KZERO = 0                                                         M28.7347
      GO TO 40063                                                       M28.7348
40061 IF ( ICTRL .GE. 0 .OR. KMAX .GE. KCRIT ) GO TO 40067              M29.5509
      TT = KMAX * TSTEP / 2.                                            M29.5510
      TSTEP = TT / NO                                                   M29.5511
      KTDIV = KTDIV + 1                                                 M32, 590
      IF ( KTDIV .LE. 5 ) GO TO 40066                                   M32, 591
      WRITE ( LUNIT6, 40068 )                                           M32, 592
40068 FORMAT (  69H TROUBLE IN TDFIT - - TIME SPAN HAS BEEN REDUCED MOREM32, 593
     1 THAN FIVE TIMES  ,/, 37H IN SEARCH OF LEGITIMATE IFT RESULTS.    M35.9226
     2  ,/,  35H   TEMPORARY ERROR STOP IN "TDFIT".    )                M35.9227
      CALL STOPTP                                                               
40067 CONTINUE                                                          M29.5513
      IF (ICTRL .EQ. -10) GO TO 40500                                   M20.5793
C     THIS ROUTINE FITS TWO EXPONENTIALS WITH AN ARTIFICIAL TIME DELAY  M20.5794
C     TO THE LINE PROP OR ADM STEP RESPONSE USING A LEAST SQUARE ERROR  M35.9229
C     MINIMIZING TECHNIQUE SOLVED BY A NEWTON-RAPHSON ALGORITHM.        M20.5796
C     LIST OF VARIABLES.                                                M20.5797
C        FV -    VECTOR CONTAINING NUMERICAL VALUES OF THE CURVE        M20.5798
C               TO BE FITTED                                            M20.5799
C        NVAR - NUMBER OF VARIABLES TO BE DETERMINED BY NEWTON-RAPHSON  M20.5800
C               ALGORITHM. THE SUBROUTINE FIXES BY ITSELF THE VALUE     M20.5801
C               OF "NVAR" TO 3 FOR PROPAGATION AND 2 FOR ADMITTANCE.    M35.9230
C        X - VECTOR CONTAINING PARAMETERS OF THE EXPONENTIAL FITTING 'Z'M20.5803
C            IN THE FORM'                                               M20.5804
C                                                                       M20.5805
C     Z(T) = D * (1.0 - EXP(-(T - X(3)) / X(1))) + (FFIN - D) *         M20.5806
C           (1.0-EXP(-(T-X(3))/X(2)))                                   M21. 489
C                                                                       M20.5808
C             WHERE'                                                    M20.5809
C        FFIN - FINAL VALUE FOR THE CURVE 'Y'                           M20.5810
C        X(3) - ARTIFICAL TIME DELAY  (EQUAL TO ZERO FOR ADMITTANCE)    M35.9231
C        E - RIGHT HAND SIDE OF SYSTEM OF EQUATIONS TO BE SOLVED        M20.5812
C            FOR INCREMENT 'DX' BY A NEWTON RAPHSON ALGORITHM.          M20.5813
C            AFTER SOLVING THE SOLUTION 'DX' WILL BE STORED IN E.       M20.5814
C        JAC - JACOBIAN MATRIX.                                         M20.5815
C        B,DB - AUXILIARY MATRICES FOR CALCULATING JACOBIAN 'JAC'.      M20.5816
C        EPS - THE LEAST SQUARE ERROR DESIRED TO BE ATTAINED            M20.5817
C              IN THE FITTING ROCESS.                                   M20.5818
C        EPS1 - THE FITTING IS STOPPED IF THE RELATIVE INCREMENT 'DX/X' M20.5819
C               IS LESS THAN EPS1                                       M20.5820
C        NPOINT - NUMBER OF KNOWN POINTS FOR THE STEP RESPONSE          M20.5821
C                 TO BE USED FOR FITTING.                               M20.5822
C        NITER - MAXIMUM NUMBER OF ITERATIONS ALLOWED.                  M20.5823
      JGRAD=0                                                           M20.5824
      ITER=0                                                            M20.5825
      NVAR=3                                                            M20.5826
      ERROR = 100.                                                      M20.5827
      DB(1,2)=0.                                                        M20.5828
C     CALCULATE INITIAL GUESS FOR 'X'.                                  M20.5829
      TEMP = FFIN * FIT2Z                                               M20.5830
      IF ( ICTRL .LT. 0 ) TEMP = TEMP * 0.1                             M29.5514
      DO 60050 I=1, NPOINT                                              M20.5831
      IF (FV(I) .LT. TEMP) GO TO 60050                                  M20.5832
      D1 = I - 1                                                        M20.5833
      X(3)=  D1 / NO                                                    M20.5834
      GO TO 60051                                                       M20.5835
60050 CONTINUE                                                          M20.5836
      KILL = 157                                                        M20.5837
      LSTAT(14) = I1                                                    M20.5838
      LSTAT(15) = NPOINT                                                M20.5839
      FLSTAT(14) = FV(NPOINT)                                           M20.5840
      FLSTAT(15) = TEMP                                                 M20.5841
 4521 LSTAT(19) = 4521                                                  M20.5842
      GO TO 9200                                                        M20.5843
60051 DO 60052 K=I, NPOINT                                              M20.5844
      IF (K .EQ. 1) GO TO 60053                                         M20.5845
      Y1 = FV(K+1) - FV(K)                                              M20.5846
      Y2 = FV(K) - FV(K-1)                                              M20.5847
      IF (Y1 .GT. Y2) GO TO 60052                                       M20.5848
      NCOUNT=K                                                          M20.5849
      GO TO 60054                                                       M20.5850
60053 IF (FV(K+1) - FV(K) .GT. FV(K)) GO TO 60052                       M20.5851
      NCOUNT=K                                                          M20.5852
      GO TO 60054                                                       M20.5853
60052 CONTINUE                                                          M20.5854
      KILL = 157                                                        M20.5855
      LSTAT(14) = 0                                                     M20.5856
      LSTAT(15) = NPOINT                                                M20.5857
      LSTAT(16) = I                                                     M20.5858
      LSTAT(17) = K                                                     M20.5859
      FLSTAT(14) = FV(K-1)                                              M20.5860
      FLSTAT(15) = FV(K)                                                M20.5861
      FLSTAT(16) = FV(K+1)                                              M20.5862
 4533 LSTAT(19) = 4533                                                  M20.5863
      GO TO 9200                                                        M20.5864
60054 NTRD = NPOINT / 3                                                 M29.5515
      DY = ( FV(NPOINT) - FV(NPOINT - NTRD) ) * NO / NTRD               M35.9232
      IF (DY .LE. 0.) DY = PIVTHR                                       M20.5866
      X(2) = DY/(FFIN - FV(NPOINT))                                     M20.5867
      X(2)=ABSZ(X(2))                                                   M20.5868
      IF ( NCOUNT .EQ. 1 ) NCOUNT = 2                                   M32, 596
      IF ( NCOUNT .EQ. 1 )   NCOUNT = 2                                 M32, 597
      DY = (FV(NCOUNT) - FV(NCOUNT-1))*NO                               M20.5870
      X(1) = DY / FV(NCOUNT) / 2.                                       M35.9233
      D1 = NCOUNT - ONEHAF                                              M20.5872
      X(3) =  D1 / NO   -  ONEHAF *(FV(NCOUNT)+FV(NCOUNT-1))/DY         M20.5873
      X1=X(1)                                                           M20.5874
      IF (ICTRL .LT. 0) GO TO 60055                                     M20.5875
      WRITE(LUNIT6,60200)                                               M20.5876
60200 FORMAT ( /,  2X,  8HNO.ITER.,  8X,  5HAMPL.,  13X,  4HX(1),       M20.5877
     1 13X,  4HX(2),  13X,  4HX(3),   13X,  5HERROR,  /,  1X  )         M20.5878
      GO TO 60012                                                       M20.5879
60055 X(1) = NO * FV(2) * ONEHAF / FVMX                                 M29.5518
      NVAR=2                                                            M20.5881
      X1 = X(2) / 1.2                                                   M29.5519
      X(3)=0.                                                           M20.5883
      WRITE (LUNIT6,60400)                                              M20.5885
60400 FORMAT ( /,  2X,  8HNO.ITER.,  8X,  5HAMPL.,  13X,  4HX(1),       M20.5886
     1 13X,  4HX(2),  13X,  5HERROR  ,/,  1X )                          M20.5887
C     NEWTON-RAPHSON ITERATIVE SOLUTION PROCESS BEGINS HERE.            M20.5888
60012 ITER = ITER + 1                                                   M20.5889
      ITER2 = 0                                                         M29.5520
      CNT = 1.0                                                         M20.5890
      ERROR1=ERROR                                                      M20.5891
60014 JGRAD = JGRAD + 1                                                 M20.5892
      IF ( JGRAD .GT. 20 ) GO TO 62500                                  M29.5521
      IF ( ICTRL .GE. 0 ) X(1) = X1 * ( 1. + FIT2Z * ( JGRAD - 1 ) )    M29.5522
      IF ( ICTRL .LT. 0 ) X(2) = X1 * ( 1. + FIT2Z * ( JGRAD - 1 ) )    M29.5523
62500 ERROR = 0.                                                        M20.5895
      IF ( JGRAD .NE. 21 ) GO TO 62501                                  M29.5524
      IF ( ICTRL .GE. 0 ) X(1) = X11                                    M29.5525
      IF ( ICTRL .LT. 0 ) X(2) = X11                                    M29.5526
62501 IF ( X(3) .LT. 0. ) X(3) = 0.                                     M29.5527
      IF (X(2) .LT. 0.) GO TO 60019                                     M20.5898
      IF (X(1) .LT. 0.) GO TO 60019                                     M20.5899
      DD=0.                                                             M20.5900
      IF (IPRSUP .GE. 4)                                                M20.5901
     1 WRITE (LUNIT6,80002) ITER,X(1),X(2),X(3)                         M20.5902
80002 FORMAT(8H  CALC D,5X,I5,3(5X,E20.10 ))                            M20.5903
      D=0.                                                              M20.5904
      DO 60003 J=1, NVAR                                                M20.5905
      E(J)=0.                                                           M20.5906
      DO 60003 K=1, NVAR                                                M20.5907
60003 HAC(J,K) = 0.                                                     M20.5908
C     CALCULATION OF THE FIRST EXPONENTIAL AMPLITUDE 'D'.               M20.5909
      NN2 = X(3)*NO + 1                                                 M20.5910
      DO 60001 I=NN2, NPOINT                                            M20.5911
      PPI = I                                                           M34. 194
      PNO = NO                                                          M34. 195
      T2 = PPI / PNO                                                    M34. 196
      DDT = T2 -X(3)                                                    M20.5913
      HHM(I)=X(1)*DDT                                                   M20.5914
      IF(HHM(I).GT.80.)HHM(I)=80.                                       M20.5915
      HHM(I)=EXPZ(-HHM(I))                                              M20.5916
      HHN(I)=X(2)*DDT                                                   M20.5917
      IF(HHN(I).GT.80.)HHN(I)=80.                                       M20.5918
      HHN(I)=EXPZ(-HHN(I))                                              M20.5919
      D = D + (HHM(I) - HHN(I))*(FFIN*(1.-HHN(I))-FV(I))                M20.5920
      DD=DD+(HHM(I)-HHN(I))**2                                          M20.5921
60001 CONTINUE                                                          M20.5922
      D=D/DD                                                            M20.5924
C     CALCULATION OF THE FITTED CURVE 'Z' AND SQUARE ERROR 'ERROR'.     M20.5925
      DD = FFIN - D                                                     M20.5926
      DO 60016 I=NN2, NPOINT                                            M20.5927
      ZP = FFIN - D*HHM(I) - DD*HHN(I)                                  M20.5928
      A = ZP - FV(I)                                                    M20.5929
      ERROR=ERROR+A*A                                                   M20.5930
      IF ( JGRAD .LE. 20 ) GO TO 60016                                  M29.5528
      D1 = I                                                            M20.5932
      T2 =  D1 / NO                                                     M20.5933
      DDT = T2 - X(3)                                                   M20.5934
      B(1)=DDT*HHM(I)*D                                                 M20.5935
      B(2) = DDT*HHN(I)*DD                                              M20.5936
      B(3) = -HHM(I)*D*X(1) - HHN(I)*DD*X(2)                            M20.5937
      DB(1,1)=-DDT*DDT*HHM(I)*D                                         M20.5938
      DB(1,3)=-HHM(I)*(1.-X(1)*DDT)*D                                   M20.5939
      DB(2,2) = -DDT*DDT*HHN(I)*DD                                      M20.5940
      DB(2,3) = -HHN(I)*(1.0 - X(2)*DDT)*DD                             M20.5941
      DB(3,3) = -X(1)*X(1)*D*HHM(I) - X(2)*X(2)*DD*HHN(I)               M20.5942
      DO 60002 J=1, NVAR                                                M20.5943
      DO 60002 K=1, J                                                   M20.5944
60002 DB(J,K) = DB(K,J)                                                 M20.5945
C     FORM THE JACOBIAN MATRIX IN 'HAC'                                 M20.5946
      DO 60017 J=1, NVAR                                                M20.5947
      E(J)=E(J)-A*B(J)                                                  M20.5948
      DO 60017 K=1, NVAR                                                M20.5949
      HAC(J,K)=HAC(J,K)+B(J)*B(K)+A*DB(J,K)                             M20.5950
60017 CONTINUE                                                          M20.5951
60016 CONTINUE                                                          M20.5952
      ERROR = ERROR / (NPOINT * FFIN * FFIN)                            M20.5953
      WRITE(LUNIT6,60300) ITER,D,(X(J),J=1,NVAR),ERROR                  M20.5954
60300 FORMAT(4X,I2,4X,5(5X ,E12.5))                                     M20.5955
      IF ( JGRAD .GT. 20 .AND. ERROR .GT. ERROR1 ) GO TO 60019          M29.5529
      IF ( ERROR .GE. ERROR1 ) GO TO 60306                              M29.5530
      ERROR1 = ERROR                                                    M29.5531
      X11 = X(1)                                                        M29.5532
      IF ( ICTRL .LT. 0 ) X11 = X(2)                                    M29.5533
60306 IF ( JGRAD .LE. 20 ) GO TO 60014                                  M29.5534
      IF ( ITER .LT. NITER ) GO TO 60302                                M29.5535
      WRITE ( LUNIT6,60301 )NITER                                       M29.5536
60301 FORMAT( 26H REACHED MAX ITTERATIONS = , I3 )                      M29.5537
      GO TO 60010                                                       M29.5538
60302 IF ( ERROR .GE. EPS ) GO TO 60304                                 M29.5539
      WRITE ( LUNIT6,60303 )                                            M29.5540
60303 FORMAT ( 23H SUCCESSFUL CONVERGENCE )                             M29.5541
      GO TO 60010                                                       M29.5542
C     SOLVE THE SYSTEM OF EQUATIONS FOR INCREMENTING VARIABLES 'X'.     M20.5963
60304 IF ( IPRSUP .GE. 4 )                                              M29.5543
     1 WRITE (LUNIT6,80003) ((HAC(I,J),J=I,3),E(I),I=1,3)               M20.5965
80003 FORMAT (9(2X,E12.5))                                              M22.6328
      DO 50009 I=1, NVAR                                                M20.5967
      IPI = I + 1                                                       M20.5968
      IF (ABSZ(HAC(I,I)).GE. EPSPV2)   GO TO 50004                      M20.5969
      IF (IPI .GT. NVAR) GO TO 50003                                    M20.5970
      DO 50002 J=IPI, NVAR                                              M20.5971
      IF (ABSZ(HAC(J,I)).LT. EPSPV2)   GO TO 50002                      M37.9100
      DO 50001 K=I, NVAR                                                M20.5973
      TEMP = HAC(J,K)                                                   M20.5974
      HAC(J,K) = HAC(I,K)                                               M20.5975
50001 HAC(I,K) = TEMP                                                   M20.5976
      GO TO 50004                                                       M20.5977
50002 CONTINUE                                                          M20.5978
C     KILL THE CASE IF JACOBIAN MATRIX IS SINGULAR.                     M20.5979
50003 KILL = 156                                                        M29.5544
 4545 LSTAT(19) = 4545                                                  M20.5981
      GO TO 9200                                                        M20.5982
50004 TEMP = HAC(I,I)                                                   M20.5983
      E(I) = E(I)/TEMP                                                  M20.5984
      IF (IPI .GT. NVAR) GO TO 50006                                    M20.5985
      DO 50005 K=IPI, NVAR                                              M20.5986
50005 HAC(I,K) = HAC(I,K)/TEMP                                          M20.5987
50006 DO 50008 J=1, NVAR                                                M20.5988
      IF (J .EQ. I) GO TO 50008                                         M20.5989
      TEMP = HAC(J,I)                                                   M20.5990
      E(J) = E(J) - TEMP*E(I)                                           M20.5991
      IF (IPI .GT. NVAR) GO TO 50008                                    M20.5992
      DO 50007 K=IPI, NVAR                                              M20.5993
50007 HAC(J,K) = HAC(J,K) - TEMP*HAC(I,K)                               M20.5994
50008 CONTINUE                                                          M20.5995
50009 CONTINUE                                                          M20.5996
      GO TO 60015                                                       M20.5997
C                           TYPE 3 ITERATION                            M20.5998
C                                                                       M20.5999
C     THE SUM OF THE SQUARE ERROR HAS INCREASED OVER THE LAST ITERATION.M20.6000
C     RECOVER THE ORIGINAL INCREMENT TO THE X VECTOR AND USE A SCALED   M20.6001
C     MULTIPLE OF THIS INCREMENT TO FORM THE NEW X VECTOR.              M20.6002
60019 CNT = CNT / 10.                                                   M29.5545
      ITER2 = ITER2 + 1                                                 M29.5546
      IF ( JGRAD .LE. 20 ) GO TO 4557                                   M29.5547
      DO 60020 J=1, NVAR                                                M20.6005
60020 X(J) = XOLD(J) + CNT * XOLD(J) * EOLD(J) / ABSZ(EOLD(J))          M20.6006
      IF ( ITER2 .LE. NITER ) GO TO 62500                               M29.5548
C     PROGRAM STUCK IN 'TYPE 3' LOOP.   KILL THE RUN.                   M20.6008
 4557 KILL = 156                                                        M29.5549
      LSTAT(19) = 4557                                                  M20.6010
      GO TO 9200                                                        M20.6011
C     UPDATE VARIABLES 'X'                                              M20.6012
60015 DO 60005 J=1, NVAR                                                M20.6013
      XOLD(J) = X(J)                                                    M20.6014
      EOLD(J) = E(J)                                                    M20.6015
60005 X(J) = X(J) + E(J)                                                M20.6016
      DO 60013 J=1, NVAR                                                M20.6017
      IF (ABSZ(E(J)/X(J)) .GT. EPS1) GO TO 60012                        M20.6018
60013 CONTINUE                                                          M20.6019
      WRITE ( LUNIT6,60305 )                                            M29.5550
60305 FORMAT ( 74H TERMINATION OF ITTERATION BECAUSE RELATIVE CORRECTIONM29.5551
     1S ARE LESS THAN EPS1 )                                            M29.5552
60010 X(3) = X(3) * TT + TSTRT                                          M20.6020
      X(1)=TT/X(1)                                                      M20.6021
      IF ( ICTRL .LT. 0 ) ICTRL = - ICTRL - 1                           M29.5553
      D1 = W * X(3)                                                     M29.5554
      D2 = SINZ(D1)                                                     M29.5555
      D1 = COSZ(D1)                                                     M29.5556
      D3 = SHIFTR * D1 + SHIFTI * D2                                    M29.5557
      SHIFTI = SHIFTR * D2 - SHIFTI * D1                                M29.5558
      SHIFTR = D3                                                       M29.5559
      WRITE (LUNIT6, 60406)  SHIFTR, SHIFTI                             M29.5560
60406 FORMAT(/, 5X, 72HSTEADY STATE FREQUENCY IMPULSE RESPONSE, ROTATED M29.5561
     1THROUGH DELAY TIME X(3)  ,/, 5X, 72HFOR USE IN ADJUSTING BY HAND FM29.5562
     2OR A PRECISE FIT AT STEADY STATE FREQUENCY  ,/, 5X, 2E14.5)       M29.5563
      IF (ICTRL .EQ. 0) GO TO 60205                                     M20.6022
C     ADJUST FITTING TO AGREE EXACTLY AT STEADY STATE                   M20.6023
70510 FORMAT(8(2X,E12.5))                                               M20.6024
      WRITE(LUNIT6,60401)                                               M20.6025
60401 FORMAT (/, 2X, 54HITERATIONS TO ADJUST FITTING AT STEADY STATE FREM29.5564
     1QUENCY  ,/,                                                       M29.5565
     1 18X,  6HAMPL 1,  11X,  6HAMPL 2,  9X,  10HTIME CONST,  8X,       M20.6027
     2 13HERROR AT 60HZ  ,/,  1X )                                      M20.6028
      ERROR = TT / X(2)                                                 M29.5566
      IF ( ERROR / X(1)  .LT.  10. )  GO TO 60205                       M29.5567
      X1=D                                                              M20.6029
      A = FFIN - D                                                      M20.6030
      IITER=0                                                           M20.6031
      DDT = W * X(1)                                                    M20.6038
      ZP = UNITY + DDT * DDT                                            M20.6039
      DDT = DDT / ZP                                                    M20.6040
      TEMP = FT2EMX * SQRTZ(SHIFTR * SHIFTR + SHIFTI * SHIFTI)          M22.6329
C     THIS LOOP ADJUSTS D AND X(2) TO MAKE THE ANALYTICALLY COMPUTED    M20.6042
C     FOURIER TRANSFORM OF THE ANALYTICAL APPROXIMATION AGREE EXACTLY   M20.6043
C     WITH THE DATA AT STEADY STATE FREQUENCY.                          M20.6044
C                                                                       M20.6045
C     FOURIERTX((D/DT)Z(T)) = CEXP(CMPLX(0,-X(3) * W)) *                M20.6046
C          (D/CMPLX(1,W / X(1)) + (FFIN - D)/CMPLX(1,W / X(2)))         M20.6047
      IF ( IPRSUP .GE. 3)                                               M29.5568
     1 WRITE (LUNIT6, 40001) SHIFTR, SHIFTI, DDT, TT, W, X(1),          M29.5569
     2                       X(2), ZP, X1, A, ERROR                     M29.5570
40001 FORMAT(//,105H PRIOR TO STEADY STATE ITERATION IN TDFIT, SHIFTR,  M29.5571
     1SHIFTI, DDT, TT, W, X(1), X(2), ZP, X1, A, ERROR ARE ,/,          M29.5572
     2  (2X, 6E12.5) )                                                  M29.5573
61002 ZR = SHIFTR - X1 / ZP                                             M20.6049
      ZI = SHIFTI + X1 * DDT                                            M21. 492
      D1 = W * ERROR                                                    M20.6051
      D2 = 1.0  +  D1 * D1                                              M20.6052
      D3 = ZR - A / D2                                                  M20.6053
      D4 = ZI + A * D1/D2                                               M22.6330
      ERROR1 = SQRTZ ( D3*D3 + D4*D4 )                                  M20.6055
      IF (ERROR1 .LT. FT2EMX)  GO TO 61000                              M22.6331
      IITER=IITER+1                                                     M20.6056
      IF (IITER .LT. NITER1) GO TO 61001                                M20.6057
      IF ( ICTRL .EQ. 2 )  GO TO 60205                                  M29.5574
      WRITE(LUNIT6, 61006)                                              M20.6058
61006 FORMAT( 5X, 96HWARNING---ITERATION TO ADJUST TIME DOMAIN STEP RESPM29.5575
     1ONSE TO FIT EXACTLY AT STEADY STATE FREQUENCY  ,/, 5X, 59HHAS FAILM29.5576
     2ED TO CONVERGE,  LAST STEADY STATE FITTING ASSUMED.  )            M29.5577
      GO TO 61004                                                       M20.6060
61001 ERROR = -ZI / ZR / W                                              M20.6063
      IF ( ERROR  .LE.  0.0 )   GO TO 60205                             M20.6064
      A = ( ZR*ZR + ZI*ZI ) / ZR                                        M20.6065
      X1 = FFIN - A                                                     M20.6066
      IF ( IPRSUP .GE. 3)                                               M29.5578
     1 WRITE (LUNIT6, 40002) IITER, ZR, ZI, D1, D2, D3, D4, ERROR1,     M29.5579
     2                       ERROR, A, X1                               M29.5580
40002 FORMAT(//,58H IN THE STEADY STATE ITERATION LOOP IN TDFIT, AT ITERM29.5581
     1ATION,I3,49H ZR, ZI, D1, D2, D3, D4, ERROR1, ERROR, A, X1 ARE,    M29.5582
     2      /, 2X, 10E12.5)                                             M29.5583
      WRITE (LUNIT6,60300) IITER,X1,A,ERROR,ERROR1                      M20.6067
      GO TO 61002                                                       M20.6068
61000 WRITE (LUNIT6,61003) IITER,ERROR1                                 M20.6069
61003 FORMAT( /, 5X, 43H STEADY STATE FREQUENCY FITTING ITERATIONS=,    M29.5584
     1  I3, 20X,                                                        M29.5585
     1 21H60 HZ FITTING ERROR =, E14.5, /, 1X )                         M20.6071
61004 X(2) = ERROR                                                      M20.6072
      D=X1                                                              M20.6073
      GO TO 60206                                                       M20.6074
60205 A = FFIN - D                                                      M20.6075
      X(2)=TT/X(2)                                                      M20.6076
      IF (ICTRL .EQ. 0) GO TO 60206                                     M20.6077
      WRITE (LUNIT6,60207)                                              M20.6078
60207 FORMAT( 5X,   100HWARNING---PROGRAM CAN NOT ADJUST TIME DOMAIN STEM20.6079
     1P RESPONSE TO FIT EXACTLY AT STEADY STATE FREQUENCY.  ,/,         M20.6080
     2 15X,  24HINITIAL FITTING ASSUMED.    )                           M20.6081
60206 CALL SETPLT                                                       M32, 598
      WRITE (LUNIT6, 60203)                                             M35.9234
60203 FORMAT ( ///// )                                                  M35.9235
      WRITE( LUNIT6, 60201 ) D, X(1), A, X(2), X(3)                     M32, 600
60201 FORMAT ( //,  30X,  23HFIRST AMPLITUDE      = ,  E15.6  ,/,       M20.6083
     1 30X,  23HFIRST TIME CONSTANT  = ,  E15.6  ,/,                    M20.6084
     2 30X,  23HSECOND AMPLITUDE     = ,  E15.6  ,/,                    M20.6085
     3 30X,  23HSECOND TIME CONSTANT = ,  E15.6  ,/,                    M20.6086
     4 30X,  23HDELAY                = ,  E15.6,  /,  1X  )             M20.6087
C     OUTPUT PRINTER PLOT OF STEP FUNCTION AND ITS ANALYTICAL APPROX-   M20.6088
C     IMATION AND CALCUALTE THE FINAL ERROR.                            M20.6089
40500 X3 = X(3) - TSTRT                                                 M20.6090
      A = FFIN - D                                                      M20.6091
      ERROR1=0.                                                         M20.6092
      IF (FVMX .LT. FFIN) FVMX = FFIN                                   M20.6093
      WRITE (LUNIT6,60405) I1,TSTEP,FVMX                                M20.6094
60405 FORMAT(1H0,5X,5HMODE ,I1,5X,12HTIME STEP = ,E12.5,57H SEC.(. = EXPM20.6095
     1. APPROX., 0 = IFT OUTPUT, * = INTERSECTION)  ,/,                 M20.6096
     2 8X,  5HERROR,  11X,  4HTIME,  5X,  3H0.0,                        M20.6097
     3 80X,  E12.5  ,/,  33X,  2H.1,  10(9H........1)  )                M20.6098
      FVMX = 90./FVMX                                                   M20.6099
      DO 60500 I=1, NPOINT                                              M20.6100
      T2 = I*TSTEP - X3                                                 M20.6101
      IF (T2 .LT. 0.0) GO TO 60403                                      M20.6102
      HHM(I) = T2/X(1)                                                  M20.6103
      IF (HHM(I) .GT. 30.) HHM(I) = 30.                                 M20.6104
      HHM(I) = EXPZ(-HHM(I))                                            M20.6105
      HHN(I) = T2/X(2)                                                  M20.6106
      IF (HHN(I) .GT. 30.)   HHN(I) = 30.                               M20.6107
      HHN(I) = EXPZ(-HHN(I))                                            M20.6108
      ZP = D*(1.0 - HHM(I)) + A*(1.0 - HHN(I))                          M20.6109
      ERROR = ZP - FV(I)                                                M20.6110
      GO TO 60402                                                       M20.6111
60403 ZP = 0.0                                                          M20.6112
      ERROR = 0.0                                                       M20.6113
60402 ERROR1 = ERROR1 + ERROR * ERROR                                   M20.6114
      J = FVMX*ZP + 1.5                                                 M20.6115
      K = FVMX*FV(I) + 1.5                                              M20.6116
      IF (J .GE. 1 .AND. J .LE. 91) PL(J) = TEXT7                       M20.6117
      IF (K .GE. 1 .AND. K .LE. 91) PL(K) = TEXT8                       M20.6118
      IF (J .EQ. K .AND. J .GE. 1 .AND. K .LE. 91) PL(J) = TEXT9        M20.6119
      T2 = T2 + X(3)                                                    M20.6120
      WRITE (LUNIT6,60600) ERROR,T2,(PL(L),L=1,91)                      M20.6121
60600 FORMAT(5X,E12.5,3X,E12.5,2H .,91A1)                               M20.6122
      IF (K .GE. 1 .AND. K .LE. 91) PL(K) = BLANK                       M20.6123
      IF (J .GE. 1 .AND. J .LE. 91) PL(J) = BLANK                       M20.6124
60500 CONTINUE                                                          M20.6125
      ERROR1 = ERROR1 / (NPOINT * FFIN * FFIN)                          M20.6126
      WRITE (LUNIT6,60204) ITER, ERROR1                                 M20.6127
60204 FORMAT ( 2X,  13HITERATIONS = ,  I3,  60X,                        M20.6128
     1     36HNORMALIZED SQUARE ERROR PER POINT = ,E12.5,/,1X)          M20.6129
      CALL SETSTD                                                       M32, 601
 9200 RETURN                                                            M20.6130
      END                                                               M20.6131
