      SUBROUTINE OVER41
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      REAL*8          TEXT1, TEXT2, TEXT3, T1, T2, T3                      15411
      REAL*8          TEXT6, TEXT7, TEXT8, TEXT9                        M22.5718
      REAL*8          TEXT10, TEXT11, TEXT12, AUPPER, ALOWER, TYPEC     M22.5719
      DIMENSION RR(6),XX(6),V(3),RIK(3),XIK(3),TEXT1(3),TEXT2(3),          15412
     1 TEXT3(3), TEMP1(3), TEMP2(3), TEMP3(3), TEXT7(3), TEXT8(3)       M20.4947
      DIMENSION  AUPPER(20), ALOWER(20), TYPEC(20), OHM(20), HENRY(20)  M22.5720
      DIMENSION  VOLT(20), VOLTI(20), VOLTK(20)                         M31.6282
      DATA T1/6HHIGH  /                                                    15414
      DATA T2/6HMEDIUM/                                                    15415
      DATA T3/6HLOW   /                                                    15416
      DATA  TEXT9   /  6HBRANCH  /                                      M20.4948
      DATA  TEXT10  /  6H0       /                                      M22.5721
      DATA  TEXT11  /  6H1       /                                      M22.5722
      DATA  TEXT12  /  6HTRANSF  /                                      M22.5723
      DATA  TYPEC(1)   /  6H51      /                                   M22.5724
      DATA  TYPEC(2)   /  6H52      /                                   M22.5725
      DATA  TYPEC(3)   /  6H53      /                                   M22.5726
      DATA  TYPEC(4)   /  6H54      /                                   M22.5727
      DATA  TYPEC(5)   /  6H55      /                                   M22.5728
      DATA  TYPEC(6)   /  6H56      /                                   M22.5729
      DATA  TYPEC(7)   /  6H57      /                                   M22.5730
      DATA  TYPEC(8)   /  6H58      /                                   M22.5731
      DATA  TYPEC(9)   /  6H59      /                                   M22.5732
      DATA  TYPEC(10)  /  6H60      /                                   M22.5733
      DATA  TYPEC(11)  /  6H61      /                                   M22.5734
      DATA  TYPEC(12)  /  6H62      /                                   M22.5735
      DATA  TYPEC(13)  /  6H63      /                                   M22.5736
      DATA  TYPEC(14)  /  6H64      /                                   M22.5737
      DATA  TYPEC(15)  /  6H65      /                                   M22.5738
      DATA  TYPEC(16)  /  6H66      /                                   M22.5739
      DATA  TYPEC(17)  /  6H67      /                                   M22.5740
      DATA  TYPEC(18)  /  6H68      /                                   M22.5741
      DATA  TYPEC(19)  /  6H69      /                                   M22.5742
      DATA  TYPEC(20)  /  6H70      /                                   M22.5743
      IF ( IPRSUP  .GE.  1 )                                            M32, 135
     1 WRITE ( LUNIT6, 4567 )                                           M32, 136
 4567 FORMAT ( 24H  "BEGIN MODULE OVER41." )                            M32, 137
      IF ( IOFBND .NE. 33666 ) GO TO 4574                               M42.7953
      CALL CRDCHG                                                       M42.7954
      GO TO 9200                                                        M42.7955
 4574 TEXT1(1) = T1                                                     M42.7956
      TEXT2(1) = T1                                                        15418
      TEXT2(2) = T1                                                        15419
      TEXT1(2) = T2                                                        15420
      TEXT2(3) = T2                                                        15421
      TEXT3(1) = T2                                                        15422
      TEXT1(3) = T3                                                        15423
      TEXT3(2) = T3                                                        15424
      TEXT3(3) = T3                                                        15425
      LL0 = 0                                                           M20.4949
      LL2 = 2                                                           M20.4950
      LL3 = 3                                                           M20.4951
      DECODE (80, 7108, ABUFF(1) )   STATFR, XOPT                       M22.5744
 7108 FORMAT ( 32X,  2E8.0 )                                            M22.5745
      IF ( STATFR  .NE.  44. )   GO TO 7119                             M29.3454
C     ENTER HERMANN'S UBC 3-PHASE TRANSFORMER ROUTINE  "BCTRAN"         M29.3455
      CALL BCTRAN                                                       M29.3456
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M29.3457
      GO TO 3872                                                        M29.3458
 7119 OMEGA = TWOPI * STATFR                                            M29.3459
      IF ( STATFR  .GT.  0.0 )   GO TO 7122                             M22.5747
    5 DO 3854  I=1, 3                                                   M20.4953
      TEXT7(I) = BLANK                                                  M20.4954
 3854 TEXT8(I) = BLANK                                                  M20.4955
C     READ INPUT CARD USING CIMAGE                                      M22.5748
 3857 CALL CIMAGE                                                       M20.4956
      N4 = KOLBEG                                                       M22.5749
      IF ( KOLBEG  .GT.  0 )   GO TO 1723                               M22.5750
      DECODE (80, 3861, ABUFF(1) )  BUS1                                M20.4957
 3861 FORMAT ( A6, 2X, 12A6 )                                           M20.4958
      GO TO 1725                                                        M22.5751
 1723 NFRFLD = 1                                                        M22.5752
      NRIGHT = -1                                                       M22.5753
      CALL FREONE ( D1 )                                                M29.3460
      NRIGHT = 0                                                        M22.5755
      BUS1 = TEXTA6(1)                                                  M22.5756
 1725 IF ( BUS1  .NE.  TEXT9 )   GO TO 3869                             M22.5757
      WRITE (KUNIT6, 3864)                                              M35.8958
 3864 FORMAT (  37H+NODE NAMES FOR PUNCHED BRANCH CARDS.    )           M20.4961
      IF ( KOLBEG  .GT.  0 )   GO TO 1733                               M22.5758
      DECODE (80, 3861, ABUFF(1) )  BUS1, ( TEXT7(I), TEXT8(I), I=1, 3) M20.4962
      GO TO 1735                                                        M22.5759
 1733 NFRFLD = 6                                                        M22.5760
      NRIGHT = -1                                                       M22.5761
      CALL FREONE ( D1 )                                                M29.3461
      NRIGHT = 0                                                        M22.5763
      IF ( IPRSUP  .GE.  1 )                                            M32, 138
     1 WRITE ( LUNIT6, 4568 )                                           M32, 139
 4568 FORMAT ( 24H  "EXIT  MODULE OVER41." )                            M32, 140
      IF ( KILL  .GT.  0 )   GO TO 99999                                M22.5764
      TEXT7(1) = TEXTA6(1)                                              M22.5765
      TEXT8(1) = TEXTA6(2)                                              M22.5766
      TEXT7(2) = TEXTA6(3)                                              M22.5767
      TEXT8(2) = TEXTA6(4)                                              M22.5768
      TEXT7(3) = TEXTA6(5)                                              M22.5769
      TEXT8(3) = TEXTA6(6)                                              M22.5770
 1735 GO TO 3857                                                        M22.5771
 3869 KOLBEG = N4                                                       M22.5772
      IF ( KOLBEG  .GT.  0 )   GO TO 1743                               M22.5773
      DECODE (80, 6, ABUFF(1) )   IWIND, CMAG, PMAG, IPUNCH             M20.4965
    6 FORMAT ( I1, E9.0, E10.0, I10 )                                   M20.4966
      GO TO 1745                                                        M22.5774
 1743 NFRFLD = 1                                                        M22.5775
      CALL FREONE ( D11 )                                               M29.3462
      IWIND = D11                                                       M22.5777
      CALL FREONE ( CMAG )                                              M29.3463
      CALL FREONE ( PMAG )                                              M29.3464
      CALL FREONE ( D11 )                                               M29.3465
      IPUNCH = D11                                                      M22.5781
      IF ( IPRSUP  .GE.  1 )                                            M32, 141
     1 WRITE ( LUNIT6, 4568 )                                           M32, 142
      IF ( KILL  .GT.  0 )   GO TO 99999                                M32, 143
 1745 IF ( IWIND  .EQ.  2 )   GO TO 7                                   M22.5783
      IF(IWIND.EQ.3) GO TO 7                                               15431
 3870 WRITE (KUNIT6, 3871)                                              M35.8959
 3871 FORMAT (  42H+BLANK CARD TERMINATING  'XFORMER'  CASES.   )       M20.4968
      CALL INTERP                                                       M35.8960
      CALL INTERP                                                       M35.8961
 3872 LASTOV = NCHAIN                                                   M27.2664
      NCHAIN = 51                                                       M22.5785
      IF ( IPRSUP  .GE.  1 )                                            M32, 144
     1 WRITE ( LUNIT6, 4568 )                                           M32, 145
      GO TO 99999                                                          15433
    7 WRITE (KUNIT6, 3892)   IWIND, CMAG, PMAG, IPUNCH                  M35.8962
 3892 FORMAT ( 12H+NEW DEVICE.,  I6,  2E12.3,  I7  )                    M20.4970
      IF(IWIND.EQ.2) GO TO 500                                             15440
      TEXT1(2)=T2                                                          15441
      RSUM=0.                                                              15442
      XSUM=0.                                                              15443
      DO 30 I=1,3                                                          15445
      CALL CIMAGE                                                       M20.4971
C     READ INPUT CARD USING CIMAGE                                      M20.4972
      IF ( KOLBEG  .GT.  0 )   GO TO 1753                               M22.5786
      DECODE (80, 10, ABUFF(1) )   VTAP, PLOSS, Z, PRATG                M20.4973
   10 FORMAT ( 5E10.0 )                                                 M22.5787
      GO TO 1755                                                        M22.5788
 1753 NFRFLD = 1                                                        M22.5789
      CALL FREONE ( VTAP )                                              M29.3466
      CALL FREONE ( PLOSS )                                             M29.3467
      CALL FREONE ( Z )                                                 M29.3468
      CALL FREONE ( PRATG )                                             M29.3469
      IF ( IPRSUP  .GE.  1 )                                            M32, 146
     1 WRITE ( LUNIT6, 4568 )                                           M32, 147
      IF ( KILL  .GT.  0 )   GO TO 99999                                M22.5794
 1755 WRITE (KUNIT6, 3904)  VTAP, PLOSS, Z, PRATG                       M35.8963
 3904 FORMAT ( 6H+WIND.,  4E11.3  )                                     M20.4975
      CALL INTERP                                                       M35.8964
      TEMP1(I) = PLOSS                                                  M20.4976
      TEMP2(I) = Z                                                      M20.4977
      TEMP3(I) = PRATG                                                  M20.4978
      RESIST=PLOSS / ( PRATG * 1000. )                                  M05. 792
      V(I)=VTAP                                                            15451
      RIK(I)=RESIST/PRATG                                                  15452
      XIK(I) = SQRTZ ( Z**2/10000.  -  RESIST**2 ) / PRATG              M18. 883
      IF ( Z  .LT.  0.0 )   XIK(I) = -XIK(I)                            M05. 794
      RSUM=RSUM+RIK(I)                                                     15454
   30 XSUM=XSUM+XIK(I)                                                     15455
      WRITE (LUNIT6, 15)  IWIND, CMAG, PMAG                             M20.4979
   15 FORMAT ( /, 13H SINGLE-PHASE, I2, 33H-WINDING TRANSFORMER.   'IMAGM20.4980
     1N' =, F8.5,  18H PER CENT BASED ON, F8.3,  4H MVA   ,/,           M20.4981
     2  23H VOLTAGE ACROSS WINDING, 15X,                                M20.4982
     3  26HLOSSES  IMPEDANCE BASED ON,/,                                M20.4983
     4 9X, 4H(KV), 26X,  23H(KW)   (PER CENT) (MVA)      )              M20.4984
      DO 3907  I=1, 3                                                   M20.4985
 3907 WRITE (LUNIT6, 20)  TEXT1(I), V(I), TEXT2(I), TEXT3(I),           M20.4986
     1 TEMP1(I), TEMP2(I), TEMP3(I)                                     M20.4987
   20 FORMAT ( 1X, A6, F7.2, 6X, A6, 4H TO , A6, F8.2, F10.4, F10.3 )   M20.4988
      CMAG = CMAG * PMAG / 300.                                         M20.4989
      RSUM=RSUM / 2.0                                                   M05. 795
      XSUM=XSUM / 2.0                                                   M05. 796
      AR=RSUM-RIK(1)                                                       15458
      AI=XSUM-XIK(1)                                                       15459
      CR=RIK(2)*RIK(3)-XIK(2)*XIK(3)-AR**2+AI**2                           15460
      CII=RIK(2)*XIK(3)+XIK(2)*RIK(3)-2.0*AR*AI                            15461
      AI=CR**2+CII**2                                                      15462
      AR=CR/AI                                                             15463
      AI=-CII/AI                                                           15464
      KI=0                                                                 15465
      I=0                                                                  15466
      J=0                                                                  15467
   40 I=I+1                                                                15468
      IF(I.GT.3) GO TO 70                                                  15469
      K=0                                                                  15470
   50 K=K+1                                                                15471
      KI=KI+1                                                              15472
      VTAP=V(I)*V(K)                                                       15473
      IF(K.EQ.I) GO TO 60                                                  15474
      J=J+1                                                                15475
      CR=(RIK(J)-RSUM)/VTAP                                                15476
      CII=(XIK(J)-XSUM)/VTAP                                               15477
      RR(KI)=CII*AI-CR*AR                                                  15478
      XX(KI)=-CII*AR-CR*AI                                                 15479
      GO TO 50                                                             15480
   60 N=4-I                                                                15481
      CR=RIK(N)/VTAP                                                       15482
      CII=XIK(N)/VTAP                                                      15483
      RR(KI)=CII*AI-CR*AR                                                  15484
      CR=-CII*AR-CR*AI                                                     15485
      CII=CMAG/VTAP                                                        15486
      IF ( CII  .GT.  ABSZ(CR)*EPSILN )   GO TO 65                      M20.4990
   62 WRITE (LUNIT6, 61)  EPSILN, CII, CR                               M20.4991
   61 FORMAT ( /, 107H PROBLEM.  ----  ADMITTANCE MATRIX FOR TRANSFORMERM20.4992
     1 IS TOO CLOSE TO BEING SINGULAR TO BE INVERTIBLE.   HENCE    ,/,  M20.4993
     2    17X,     81HTHE INVERSION WILL BE SKIPPED.   PARAMETERS WHICH M20.4994
     3DOCUMENT THIS BREAKDOWN FOLLOW.   ,/,                             M20.4995
     4 20X,  9X,  6HEPSILN,  12X,  3HCII,  13X,  2HCR  ,/,              M20.4996
     5 20X,  3E15.6  )                                                  M20.4997
      GO TO 600                                                            15491
   65 XX(KI)=CR+CII                                                        15492
      GO TO 40                                                             15493
   70 CALL CXRED1 ( RR(1), XX(1), LL3, LL0 )                            M20.4998
   75 WRITE (LUNIT6, 90)                                                M17.1183
   90 FORMAT ( /,  90H IMPEDANCE MATRIX AS REQUIRED FOR EMTP STUDIES (WIM28.7028
     1TH  'X'  IN OHMS AT THE POWER FREQUENCY)   ,/,                       15497
     2  3( 14X, 1HR, 14X, 1HX)   )                                         15498
      I=0                                                                  15499
      N=1                                                                  15500
   95 I=I+1                                                                15501
      IF(I.GT.IWIND) GO TO 110                                             15502
      KI=N+I-1                                                             15503
      WRITE (LUNIT6, 100)  TEXT1(I),   ( RR(K), XX(K), K=N, KI )        M17.1184
  100 FORMAT(1H ,A6,          6E15.7)                                      15505
      N=N+I                                                                15506
      GO TO 95                                                             15507
  110 WRITE (LUNIT6, 113)                                               M20.4999
  113 FORMAT ( /, 1X )                                                  M20.5000
  120 FORMAT ( 112H WARNING. ----- IF NOTHING IS CONNECTED TO WINDINGS W   15509
     1HICH ARE DELTA-CONNECTED, EITHER ADD CAPACITANCES OR GROUND  ,/,     15510
     2 16X, 100HONE TERMINAL OF THE DELTA.   OTHERWISE THE DELTA CONNECT   15511
     3ION IS FLOATING, WITH VOLTAGE INDETERMINATE.     )                   15512
      IF ( IPUNCH  .GT.  0 )   GO TO 4273                               M20.5001
      WRITE (LUNIT6, 4201)  ( I, I=1, 8 )                               M20.5002
 4201 FORMAT ( //,  92H 80-COLUMN CARD-IMAGE LISTING OF PUNCHED-CARD OUTM20.5003
     1PUT FOLLOWS (TYPE-51-53 EMTP BRANCH CARDS).    ,/,                M20.5004
     2 1X,  80(1H-)  ,/,  1X,  8(9X, I1)  ,/,                           M20.5005
     3 1X,  8(9X, 1H0)  ,/,  1X,  80(1H-)   )                           M20.5006
      DO 4270  I=1, IWIND                                               M20.5007
      IF ( I  .GT.  1 )   GO TO 4228                                    M20.5008
      WRITE (LUNIT7, 4219)  TEXT7(I), TEXT8(I), RR(1), XX(1)            M29.3470
 4219 FORMAT ( 3H51,,  A6, 1H,,A6, 10H,  ,  ,      ,                    M37.7995
     1  2( E22.13, 2H , ),  1X,  5H,,,,   )                             M37.7996
      WRITE (LUNIT6, 4220)  TEXT7(I), TEXT8(I), RR(1), XX(1)            M29.3473
 4220 FORMAT ( 1X,  3H51,,  A6, 1H,,A6, 10H,  ,  ,      ,               M37.7997
     1  2( E22.13, 2H , ),  1X,  5H,,,,   )                             M37.7998
      GO TO 4270                                                        M20.5015
 4228 IF ( I  .GT.  2 )   GO TO 4238                                    M20.5016
      WRITE (LUNIT7, 4229)  TEXT7(I), TEXT8(I), RR(2), XX(2),           M29.3476
     1                      RR(3), XX(3)                                M29.3477
 4229 FORMAT ( 3H52,, A6, 1H,,A6, 10H,  ,  ,       ,                    M37.7999
     1  E22.13,  2H ,,  E22.13,  2H $  ,/,                              M29.3479
     2  26X, 2( E22.13, 2H , ), 1X,  5H,,,,    )                        M37.8000
      WRITE (LUNIT6, 4230)  TEXT7(I), TEXT8(I), RR(2), XX(2),           M29.3481
     1                      RR(3), XX(3)                                M29.3482
 4230 FORMAT ( 1X,  3H52,, A6, 1H,,A6, 10H,  ,  ,       ,               M37.8001
     1  E22.13,  2H ,,  E22.13,  2H $  ,/,                              M29.3484
     2  27X, 2( E22.13, 2H , ), 1X,  5H,,,,    )                        M37.8002
      GO TO 4270                                                        M20.5025
 4238 WRITE (LUNIT7, 4239)  TEXT7(I), TEXT8(I),                         M29.3486
     1                      ( RR(J), XX(J), J=4, 6 )                    M29.3487
 4239 FORMAT ( 3H53,, A6, 1H,,A6, 10H,  ,  ,       ,                    M37.8003
     1        E22.13,  2H ,,  E22.13,  2H $  ,/,                        M29.3489
     2  26X,  E22.13,  2H ,,  E22.13,  2H $  ,/,                        M29.3490
     3  26X, 2( E22.13, 2H , ), 1X,  5H,,,,    )                        M37.8004
      WRITE (LUNIT6, 4240)  TEXT7(I), TEXT8(I),                         M29.3492
     1                      ( RR(J), XX(J), J=4, 6 )                    M29.3493
 4240 FORMAT ( 1X,  3H53,, A6, 1H,,A6, 10H,  ,  ,       ,               M37.8005
     1        E22.13,  2H ,,  E22.13,  2H $  ,/,                        M29.3495
     2  27X,  E22.13,  2H ,,  E22.13,  2H $  ,/,                        M32, 149
     3  27X, 2( E22.13, 2H , ), 1X,  5H,,,,    )                        M37.8006
 4270 CONTINUE                                                          M20.5040
      WRITE (LUNIT6, 4271)                                              M20.5041
 4271 FORMAT ( 1X,  80(1H-)  ,//,  1X  )                                M20.5042
 4273 ICHCK = 0                                                         M20.5043
      WRITE (LUNIT6, 130)                                               M17.1186
  130 FORMAT ( /, 101H SHORT-CIRCUIT INPUT IMPEDANCES WHICH ARE OBTAINED   15515
     1 FROM THE JUST-PRINTED IMPEDANCE MATRIX, BY REVERSE     ,/,          15516
     2  59H COMPUTATION.   THIS IS SORT OF A CHECK ON THE COMPUTATION. )   15517
  135 AR=RR(2)**2-XX(2)**2                                                 15518
      AI=2.0*RR(2)*XX(2)                                                   15519
      CR=RR(3)**2+XX(3)**2                                                 15520
      RSUM=RR(3)/CR                                                        15521
      XSUM=-XX(3)/CR                                                       15522
      CR=RR(1)-AR*RSUM+AI*XSUM                                             15523
      CII=XX(1)-AR*XSUM-AI*RSUM                                            15524
      IF(IWIND.EQ.2) GO TO 145                                             15525
      WRITE (LUNIT6, 140)   TEXT2(1), TEXT3(1), CR, CII                 M17.1187
  140 FORMAT ( 1X, A6, 4H TO , A6, 2F15.5  )                               15527
      CR=RR(6)**2+XX(6)**2                                                 15528
      RSUM=RR(6)/CR                                                        15529
      XSUM=-XX(6)/CR                                                       15530
      AR=RR(4)**2-XX(4)**2                                                 15531
      AI=2.0*RR(4)*XX(4)                                                   15532
      CR=RR(1)-AR*RSUM+AI*XSUM                                             15533
      CII=XX(1)-AR*XSUM-AI*RSUM                                            15534
      WRITE (LUNIT6, 140)  TEXT2(2), TEXT3(2), CR, CII                  M17.1188
      AR=RR(5)**2-XX(5)**2                                                 15536
      AI=2.0*RR(5)*XX(5)                                                   15537
      CR=RR(3)-AR*RSUM+AI*XSUM                                             15538
      CII=XX(3)-AR*XSUM-AI*RSUM                                            15539
      WRITE (LUNIT6, 140)  TEXT2(3), TEXT3(3), CR, CII                  M17.1189
      GO TO 146                                                            15541
  145 WRITE (LUNIT6, 140)  TEXT1(1), TEXT1(3), CR, CII                  M17.1190
  146 IF(ICHCK.GT.0) GO TO 600                                             15543
      J=(IWIND-1)*3                                                        15544
      DO 150 I=1,J                                                         15545
      IF ( XX(I)  .LT.  0.0 )                                              15546
     1 WRITE (LUNIT6, 155)                                              M17.1191
  155 FORMAT (  74H INPUT DATA LOOKS SUSPICIOUS, BECAUSE ONE OR MORE IND   15548
     1UCTANCES IS NEGATIVE.   )                                            15549
      CALL ROUND ( RR(I) )                                                 15550
  150 CALL ROUND ( XX(I) )                                                 15551
      WRITE (LUNIT6, 160)                                               M17.1192
  160 FORMAT ( 105H REPEAT OF PRECEDING CALCULATION, ONLY THIS TIME THE    15553
     1STARTING POINT WILL BE THE IMPEDANCE MATRIX WITH ALL    ,/,          15554
     2  41H ELEMENTS ROUNDED TO FIVE DECIMAL DIGITS.      )                15555
      ICHCK=1                                                              15556
      GO TO 135                                                            15557
C     READ INPUT CARD USING CIMAGE                                      M20.5044
  500 CALL CIMAGE                                                       M20.5045
      IF ( KOLBEG  .GT.  0 )   GO TO 1763                               M22.5796
      DECODE (80, 10, ABUFF(1) )   V1, V2, PLOSS, Z, PRATG              M20.5046
      GO TO 1765                                                        M22.5797
 1763 NFRFLD = 1                                                        M22.5798
      CALL FREONE ( V1 )                                                M29.3498
      CALL FREONE ( V2 )                                                M29.3499
      CALL FREONE ( PLOSS )                                             M29.3500
      CALL FREONE ( Z )                                                 M29.3501
      CALL FREONE ( PRATG )                                             M29.3502
      IF ( IPRSUP  .GE.  1 )                                            M32, 151
     1 WRITE ( LUNIT6, 4568 )                                           M32, 152
      IF ( KILL  .GT.  0 )   GO TO 99999                                M22.5804
 1765 WRITE (KUNIT6, 3904)  V1, V2, PLOSS, Z                            M35.8965
      CALL INTERP                                                               
      WRITE (LUNIT6, 15)  IWIND, CMAG, PMAG                             M20.5048
      WRITE (LUNIT6, 20)  TEXT1(1), V1, TEXT1(1), TEXT1(3), PLOSS,      M20.5049
     1                    Z, PRATG                                      M20.5050
      TEXT1(2)=T3                                                          15560
      CMAG=CMAG  * PMAG / 200.                                          M05. 799
      RSUM=PLOSS / ( PRATG * 1000. )                                    M05. 800
      XSUM = SQRTZ ( Z**2/10000.  -  RSUM**2 )                          M18. 885
      IF ( Z  .LT.  0.0 )   XSUM = -XSUM                                M05. 802
      CII=(RSUM**2+XSUM**2)/PRATG                                          15565
      AR=RSUM/CII                                                          15566
      AI=-XSUM/CII                                                         15567
      IF ( CMAG  .GT.  ABSZ(AI)*EPSILN )   GO TO 3926                   M20.5051
      CII = CMAG                                                        M20.5052
      CR = AI                                                           M20.5053
      GO TO 62                                                          M20.5054
 3926 VTAP = V1**2                                                      M20.5055
      RR(1)=-AR/VTAP                                                       15570
      XX(1)=(CMAG-AI)/VTAP                                                 15571
      VTAP=V1*V2                                                           15572
      RR(2)=AR/VTAP                                                        15573
      XX(2)=AI/VTAP                                                        15574
      VTAP=V2**2                                                           15575
      RR(3)=-AR/VTAP                                                       15576
      XX(3)=(CMAG-AI)/VTAP                                                 15577
      CALL CXRED1 ( RR(1), XX(1), LL2, LL0 )                            M20.5056
      GO TO 75                                                             15579
  600 WRITE (LUNIT6, 601)                                               M17.1197
  601 FORMAT ( /,1X)                                                    M15.1504
      GO TO 5                                                              15582
C     BEGIN CODE FOR THE CONVERSION OF SATURABLE  'TRANSFORMER'         M22.5806
C     DATA INTO  (R)  AND  (L)  SPECIFICATION (TYPE 51, 52, .. ).       M22.5807
C     LET  'ZK'  DENOTE THE COMPLEX (PHASOR) LEAKAGE IMPEDANCE FOR      M22.5808
C     WINDING  'K' ,   AND LET  'ZMAG'  DENOTE THE COMPLEX              M22.5809
C     MAGNETIZING IMPEDANCE.   FURTHER, LET  'TK'  BE THE RELATIVE      M22.5810
C     NUMBER OF TURNS FOR WINDING  'K' ,   AND LET  (Z)  BE THE         M22.5811
C     DESIRED OUTPUT MATRIX, WHERE    (Z) = (R) + JW(L)    AT           M22.5812
C     SINUSOIDAL FREQUENCE  'W' .   THEN THE RELEVANT CONVERSION        M22.5813
C     FORMULAS ARE  .....                                               M22.5814
C          Z(K,K)  =  ZK  +  ZMAG * (TK/T1)**2                          M22.5815
C          Z(K,J)  =  ZMAG * TK * TJ / T1**2      (FOR  J .NE. K )      M22.5816
C     READ INPUT CARD USING CIMAGE                                      M22.5817
 7122 CALL CIMAGE                                                       M22.5818
 7123 CONTINUE                                                          M22.5819
      DECODE (80, 7129, ABUFF(1) )   BUS5, D1, D2, D3                   M22.5820
 7129 FORMAT ( 2X, A6, 18X, 2E6.2, 6X, E6.2 )                           M22.5821
      IF ( BUS5  .EQ.  TEXT12 )   GO TO 7132                            M22.5822
      LSTAT(19) = 7132                                                  M22.5823
      LSTAT(14) = 1                                                     M22.5824
      GO TO 9000                                                        M22.5825
 7132 WRITE (KUNIT6, 7134)  D1, D2, D3                                  M35.8967
 7134 FORMAT ( 16H+SAT. XFORMER.  ,  3E11.3  )                          M22.5827
      D4 = D2 / D1                                                      M22.5828
      D5 = D4 * OMEGA                                                   M22.5829
      DO 7151  I=1, 99                                                  M22.5830
C     READ INPUT CARD USING CIMAGE                                      M22.5831
      CALL CIMAGE                                                       M22.5832
      DECODE (80, 7142, ABUFF(1) )   BUS5, BUS6                         M22.5833
 7142 FORMAT ( 2A1 )                                                    M22.5834
      IF ( BUS6  .NE.  TEXT11 )   GO TO 7151                            M22.5835
      IF ( BUS5  .EQ.  TEXT10 )   GO TO 7157                            M22.5836
      IF ( BUS5  .EQ.  BLANK )   GO TO 7157                             M22.5837
 7151 WRITE (KUNIT6, 7152)                                              M35.8968
 7152 FORMAT ( 46H+IGNORE CHARACTERISTIC IN SEARCH FOR WINDINGS.   )    M22.5839
      LSTAT(19) = 7152                                                  M22.5840
      LSTAT(14) = 2                                                     M22.5841
      GO TO 9000                                                        M22.5842
 7157 DO 7172  I=1, 20                                                  M22.5843
      IF ( I  .EQ.  1 )   GO TO 7161                                    M22.5844
C     READ INPUT CARD USING CIMAGE                                      M22.5845
      CALL CIMAGE                                                       M22.5846
 7161 CONTINUE                                                          M22.5847
      DECODE (80, 7163, ABUFF(1) )   KK, AUPPER(I), ALOWER(I),          M22.5848
     1                               OHM(I), HENRY(I), VOLT(I)          M22.5849
 7163 FORMAT ( I2,  2A6,  12X,  3E6.2  )                                M22.5850
      IF ( KK  .NE.  I )   GO TO 7175                                   M22.5851
      IWIND = KK                                                        M22.5852
      WRITE (KUNIT6, 7171)  I, OHM(I), HENRY(I), VOLT(I)                M35.8969
 7171 FORMAT ( 10H+  WINDING,  I2,  2H. ,  3E12.4  )                    M22.5854
 7172 CONTINUE                                                          M22.5855
      LSTAT(19) = 7175                                                  M22.5856
      LSTAT(14) = 3                                                     M22.5857
      GO TO 9000                                                        M22.5858
 7175 WRITE (KUNIT6, 7178)                                              M35.8970
 7178 FORMAT (  45H+DATA CARD FOR FOLLOWING TRANSFORMER, IF ANY.   )    M22.5860
      WRITE (LUNIT6, 7182)  IWIND,  STATFR,  XOPT, D4,  D5              M22.5861
 7182 FORMAT ( //,112H SATURABLE TRANSFORMER INPUT DATA IS NOW COMPLETE.M22.5862
     1   RELEVANT SCALAR PARAMETERS MAY BE SUMMARIZED AS FOLLOWS ...    M22.5863
     2 ,/,  1X,  I19,  35H  =  NUMBER OF TRANSFORMER WINDINGS     ,/,   M22.5864
     3 E20.6,  75H  =  POWER FREQUENCY IN HERTZ (EQUIVALENCE WILL BE EXAM22.5865
     4CT AT THIS FREQUENCY)                                       ,/,   M22.5866
     5 E20.6,  64H  =  'XOPT'  (EMTP INDUCTANCE/REACTANCE SPECIFICATION M22.5867
     6PARAMETER)                                                  ,/,   M22.5868
     7 E20.8,  53H  =  ASSUMED LINEAR MAGNETIZING INDUCTANCE IN HENRIES M22.5869
     8 ,/,  E20.8,  58H  =  MAGNETIZING REACTANCE AT THE POWER FREQUENCYM22.5870
     9, IN OHMS          )                                              M22.5871
      IF ( D3  .GT.  0.0 )   GO TO 7206                                 M22.5872
      D9 = 0.0                                                          M22.5873
      D10 = D5                                                          M22.5874
      GO TO 7216                                                        M22.5875
 7206 D6 = D3**2                                                        M22.5876
      D7 = D5**2                                                        M22.5877
      D8 = D6 + D7                                                      M22.5878
      D9 = D3 * D7 / D8                                                 M22.5879
      D10 = D6 * D4 / D8                                                M22.5880
 7216 IF ( XOPT  .EQ.  0.0 )                                            M22.5881
     1 D10 = D10 * 1000. / OMEGA                                        M22.5882
      IF ( XOPT  .GT.  0.0 )                                            M22.5883
     1 D10 = D10 * XOPT / STATFR                                        M22.5884
      WRITE (LUNIT6, 4201)  ( I,  I=1, 8 )                              M22.5885
      DO 7274  I=1, IWIND                                               M22.5886
      N1 = I - 1                                                        M22.5887
      D11 = VOLT(I) / VOLT(1)                                           M22.5888
      TEXT6 = TYPEC(I)                                                  M22.5889
      BUS1 = AUPPER(I)                                                  M22.5890
      BUS2 = ALOWER(I)                                                  M22.5891
      IF ( N1  .LE.  0 )  GO TO 7253                                    M22.5892
      DO 7245  J=1, N1                                                  M22.5893
      D12 = D11 * VOLT(J) / VOLT(1)                                     M22.5894
      D13 = D12 * D9                                                    M22.5895
      D14 = D12 * D10                                                   M22.5896
      WRITE (LUNIT7, 7231) TEXT6, BUS1, BUS2, D13, CSEPAR, D14, CHCONT  M22.5897
 7231 FORMAT ( A2,  2A6,  12X,  2(E22.13, 1X, A1),  1X,  A1,  6X )      M22.5898
      WRITE (LUNIT6, 7234) TEXT6, BUS1, BUS2, D13, CSEPAR, D14, CHCONT  M22.5899
 7234 FORMAT ( 1X,  A2,  2A6,  12X,  2(E22.13, 1X, A1),  1X,  A1,  6X ) M22.5900
      BUS1 = BLANK                                                      M22.5901
      BUS2 = BLANK                                                      M22.5902
 7245 TEXT6 = BLANK                                                     M22.5903
 7253 D12 = D11**2                                                      M22.5904
      VOLTI(I) = D12 * D9                                               M22.5905
      VOLTK(I) = D12 * D10                                              M22.5906
      D13 = OHM(I)  +  VOLTI(I)                                         M22.5907
      D14 = HENRY(I)  +  VOLTK(I)                                       M22.5908
      WRITE (LUNIT7, 7262)  TEXT6, BUS1, BUS2, D13, CSEPAR,             M22.5909
     1                      D14,  ( CSEPAR,  M=1, 5 )                   M22.5910
 7262 FORMAT ( A2,  2A6,  12X,  2( E22.13, 1X, A1 ),  1X,  5A1,  2X )   M22.5911
      WRITE (LUNIT6, 7265)  TEXT6, BUS1, BUS2, D13, CSEPAR,             M22.5912
     1                      D14,  ( CSEPAR,  M=1, 5 )                   M22.5913
 7265 FORMAT ( 1X,                                                      M22.5914
     1         A2,  2A6,  12X,  2( E22.13, 1X, A1 ),  1X,  5A1,  2X )   M22.5915
 7274 CONTINUE                                                          M22.5916
      WRITE (LUNIT6, 4271)                                              M22.5917
      WRITE (LUNIT6, 7277)  ( I, OHM(I), HENRY(I), VOLTI(I),            M22.5918
     1                        VOLTK(I), I=1, IWIND )                    M22.5919
 7277 FORMAT ( 112H COMPONENTS WHICH ARE ADDED TO FORM DIAGONALS OF THE M22.5920
     1MATRIX.   COMPUTER WORD-LENGTH SHOULD BE ABLE TO ACCURATELY  ,/,  M22.5921
     2 116H HANDLE THIS ADDITION WITHOUT THE LEAKAGE BEING LOST IN THE RM22.5922
     3OUNDOFF.   IF NOT, THE RESULTING  (R)  AND  (L)  ARE OF     ,/,   M22.5923
     4 115H QUESTIONABLE VALUE FOR PURPOSES OF EMTP SIMULATION.   COLUMNM22.5924
     5  1  IS TO BE ADDED TO COLUMN  3 ,   AND COLUMN  2  IS      ,/,   M22.5925
     6   52H TO BE ADDED TO COLUMN  4  TO PRODUCE THE DIAGONALS.  ,/,   M22.5926
     7 7X,  3HROW,  11X,  9HLEAKAGE R,  6X,  14HLEAKAGE L OR X,         M22.5927
     8 12X, 13HMAGNETIZING R,  6X,  14HMAGNET. L OR X,  16X       ,/,   M22.5928
     9  ( I10,  2E20.10, 5X,  2E20.10 )   )                             M22.5929
      DECODE (80, 7284, ABUFF(1) )   ( AUPPER(I), I=1, 14 )             M22.5930
 7284 FORMAT ( 13A6, A2 )                                               M22.5931
      WRITE (LUNIT6, 7288)  ( AUPPER(I), I=1, 14 )                      M22.5932
 7288 FORMAT ( ///, 107H ANOTHER SATURABLE TRANSFORMER HAS NOW BEEN SUCCM22.5933
     1ESSFULLY CONVERTED TO  (R), (L)  FORMAT.   THAT COMPUTATION ,/,   M22.5934
     2 110H IS NOW COMPLETE.  THE EMTP NOW BEGINS TO PROCESS THE FOLLOWIM22.5935
     3NG CASE (IF ANY) BY RE-LISTING THE LAST-READ DATA           ,/,   M22.5936
     4  54H CARD (WHICH DID NOT BELONG TO THE LAST CASE, RECALL).  ,/,  M22.5937
     5 1X,  131( 1H- )  ,/,   1H+,  50X,  1H1  ,/,                      M22.5938
     6 51X,  1H1,  13A6,  A2  )                                         M22.5939
      DECODE (80, 7281, ABUFF(1) )   BUS5                               M22.5940
 7281 FORMAT ( 2X,  A6  )                                               M22.5941
      IF ( BUS5  .EQ.  TEXT12 )   GO TO 7123                            M22.5942
      IF ( BUS5  .EQ.  BLANK )   GO TO 3870                             M22.5943
      LSTAT(14) = 1                                                     M22.5944
      LSTAT(19) = 7288                                                  M22.5945
 9000 KILL = 158                                                        M22.5946
 9200 LSTAT(18) = NCHAIN                                                M27.2665
      LASTOV = NCHAIN                                                   M22.5948
      NCHAIN = 51                                                       M22.5949
      IF ( IPRSUP  .GE.  1 )                                            M32, 153
     1 WRITE ( LUNIT6, 4568 )                                           M32, 154
99999 RETURN                                                               15583
      END                                                                  15584
      SUBROUTINE CRDCHG                                                 M42.7957
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C                                                                       M42.7958
C     FORTRAN77 USED TO PROVIDE A SUBROUTINE TO                         M42.7959
C     INPUT OLD DATA CARDS AND CHANGE THEIR FORMAT;                     M42.7960
C     THIS INCLUDES DOING ANY APPROPRIATE CALCULATIONS.                 M42.7961
C     OLD 91,92,93 TYPES CHANGED TO NEW 91,99,98 TYPES                  M42.7962
C     RESPECTIVELY.                                                     M42.7963
C                                                                       M42.7964
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*80 CARD, BLANKS, WORK                                   M42.7967
      REAL*8         TEXT(14)                                           M42.7968
C                                                                       M42.7969
      BLANKS = ' '                                                      M42.7970
      WORK = ' '                                                        M42.7971
C                                                                       M42.7972
      WRITE( LUNIT6, 20 )                                               M42.7973
20    FORMAT( ' This code will read data types 91-switched',            M42.7974
     +        ' resistor, 92-Switched Resistance, and',                 M42.7975
     +        ' 93-Switched Inductor.' )                                M42.7976
      WRITE( LUNIT6, 26 )                                               M42.7977
26    FORMAT(  ' The data will be converted, formated,',                M42.7978
     +         ' and written out to meet the new EMTP',                 M42.7979
     +         ' rule book''s requirements.' )                          M42.7980
C                                                                       M42.7981
      OPEN( UNIT = 7,                                                   M42.7982
     +STATUS = 'NEW',                                                   M42.7983
     +ERR = 40)                                                         M42.7984
C                                                                       M42.7985
      GO TO 50                                                          M42.7986
C                                                                       M42.7987
40    CONTINUE                                                          M42.7988
      STOP ' ERROR OPENING OUTPUT FILE '                                M42.7989
C                                                                       M42.7990
50    CONTINUE                                                          M42.7991
C                                                                       M42.7992
      NOUTPR = 1                                                        M42.7993
      DO 200 ICOUNT = 1, 9999                                           M42.7994
C                                                                       M42.7995
         CALL CIMAGE                                                    M42.7996
         DECODE( 80, 100, ABUFF) TEXT                                   M42.7997
         WRITE( CARD, 100 ) TEXT                                        M42.7998
100      FORMAT( 13A6,A2 )                                              M42.7999
C                                                                       M42.8000
C     *** CASE '91' ***                                                 M42.8001
C                                                                       M42.8002
         IF( CARD(1:2) .EQ. '91' ) THEN                                 M42.8003
            IF( CARD(39:44) .EQ. ' 3333.' ) GO TO 200                   M42.8004
            WRITE( LUNIT6, 625 ) CARD                                   M42.8005
625         FORMAT( ' TYPE-91:  PIECE-WISE LINEAR RESISTOR',            M42.8006
     +                 14X, 1H1, A80 )                                  M42.8007
1000        CONTINUE                                                    M42.8008
            WRITE( 7, 1175 ) CARD(1:78)                                 M42.8009
1175        FORMAT( 'C ', A78 )                                         M42.8010
            WRITE( 7, 1100 ) CARD(1:26), CARD(80:80)                    M42.8011
1100        FORMAT( A26, 12X, ' 3333.', 37X, A1 )                       M42.8012
            WRITE( LUNIT9, 1150 ) CARD(1:26), CARD(80:80)               M42.8013
1150        FORMAT( A26, 12X, ' 3333.', 37X, A1 )                       M42.8014
C                                                                       M42.8015
            WORK(1:25) = BLANKS(1:19)//CARD(27:32)                      M42.8016
C                                                                       M42.8017
            WRITE( 7, 1160 )                                            M42.8018
1160        FORMAT( 'C', 18X, 'VSTART' )                                M42.8019
            WRITE( 7, 1200 ) WORK                                       M42.8020
1200        FORMAT( A80 )                                               M42.8021
            WRITE( LUNIT9, 1225 ) WORK                                  M42.8022
1225        FORMAT( A80 )                                               M42.8023
C                                                                       M42.8024
            WRITE( 7, 1185 )                                            M42.8025
1185        FORMAT( 'C ', 14X, 'TR IN SEC.', 4X, 'R(TR) IN OHMS' )      M42.8026
C                                                                       M42.8027
1250        CONTINUE                                                    M42.8028
            IF( CARD(13:16) .NE. '9999' ) THEN                          M42.8029
               CALL CIMAGE                                              M42.8030
               DECODE( 80, 1400, ABUFF) TEXT                            M42.8031
               WRITE( CARD, 1400 ) TEXT                                 M42.8032
1400           FORMAT( 13A6,A2 )                                        M42.8033
               WORK(1:25) = BLANKS(1:9)//CARD(1:16)                     M42.8034
               WORK(26:50) = BLANKS(1:9)//CARD(17:32)                   M42.8035
C                                                                       M42.8036
               WRITE( 7, 1415 ) CARD(1:78)                              M42.8037
1415           FORMAT( 'C ', A78 )                                      M42.8038
               WRITE( 7, 1413 ) WORK                                    M42.8039
1413           FORMAT( A80 )                                            M42.8040
               WRITE( LUNIT6, 1300 ) CARD(1:32), WORK                   M42.8041
1300           FORMAT( 1X, 'CHARACTERISTICS', A32, 3X, '1',  A80 )      M42.8042
               WRITE( LUNIT9, 1325 ) WORK                               M42.8043
1325           FORMAT( A80 )                                            M42.8044
C                                                                       M42.8045
               GO TO 1250                                               M42.8046
            END IF                                                      M42.8047
         END IF                                                         M42.8048
C                                                                       M42.8049
         IF( CARD .EQ. BLANKS ) GO TO 500                               M42.8050
         IF( CARD(1:5) .EQ. 'BLANK' ) GO TO 500                         M42.8051
C                                                                       M42.8052
200   CONTINUE                                                          M42.8053
C                                                                       M42.8054
500   DO 700 KCOUNT = 1, 9999                                           M42.8055
C                                                                       M42.8056
         CALL CIMAGE                                                    M42.8057
         DECODE( 80, 600, ABUFF) TEXT                                   M42.8058
         WRITE( CARD, 600 ) TEXT                                        M42.8059
600      FORMAT( 13A6,A2 )                                              M42.8060
         IF( CARD .EQ. BLANKS ) GO TO 10000                             M42.8061
         IF( CARD(1:5) .EQ. 'BLANK' ) GO TO 10000                       M42.8062
C                                                                       M42.8063
C     *** CASE '92' ***                                                 M42.8064
C                                                                       M42.8065
         IF( CARD(1:2) .EQ. '92' ) THEN                                 M42.8066
            WRITE( LUNIT6, 650 ) CARD                                   M42.8067
650         FORMAT( ' TYPE-92:  SWITCHED RESISTANCE', 21X, 1H1, A80 )   M42.8068
            WORK = ' '                                                  M42.8069
            WRITE( 7, 3100 ) CARD(1:78)                                 M42.8070
3100        FORMAT( 'C ', A78 )                                         M42.8071
C                                                                       M42.8072
            READ( CARD, 3110 ) D7, D8                                   M42.8073
3110        FORMAT( 34X, 2E10.6 )                                       M42.8074
            N6 = 6                                                      M42.8075
            CALL FLTOPT( D7, N6 )                                       M42.8076
            WORK(1:6) = ANSI32(27:32)                                   M42.8077
            N6 = 6                                                      M42.8078
            CALL FLTOPT( D8, N6 )                                       M42.8079
            WRITE( 7, 3300 ) CARD(3:14), WORK(1:6), ANSI32(27:32),      M42.8080
     +      WORK(13:79), CARD(80:80)                                    M42.8081
3300        FORMAT( '99', A12, 12X, A6, 6X, '     2', A6, A30, A1 )     M42.8082
C                                                                       M42.8083
            WRITE( 7, 3400 )                                            M42.8084
3400        FORMAT( 'C', 8X, 'CURRENT', 12X, 'VOLT', 50X )              M42.8085
            WRITE( 7, 3500 ) CARD(35:44)                                M42.8086
3500        FORMAT( 7X, 9H0.0000001, 6X, A10, 50X )                     M42.8087
            WRITE( LUNIT9, 3525 ) CARD(3:14), WORK(1:6), ANSI32(27:32), M42.8088
     +      WORK(13:79), CARD(80:80)                                    M42.8089
3525        FORMAT( '99', A12, 12X, A6, 6X, '     2', A6, A30, A1 )     M42.8090
C                                                                       M42.8091
            WRITE( LUNIT9, 3535 ) CARD(35:44)                           M42.8092
3535        FORMAT( 7X, 9H0.0000001, 6X, A10, 50X )                     M42.8093
C                                                                       M42.8094
            READ( CARD(15:24), 3550 ) RESIST                            M42.8095
3550        FORMAT( E10.6 )                                             M42.8096
            READ( CARD(39:44), 3575 ) SEAL                              M42.8097
3575        FORMAT( E10.6 )                                             M42.8098
            FLASH = RESIST + SEAL                                       M42.8099
            WRITE( WORK(13:44), 3600 ) FLASH                            M42.8100
3600        FORMAT( 13X, 3H1.0, E16.9 )                                 M42.8101
            WRITE( 7, 3650 ) WORK(13:80)                                M42.8102
3650        FORMAT( A64 )                                               M42.8103
            WRITE( 7, 3700 )                                            M42.8104
3700        FORMAT( 12X, '9999' )                                       M42.8105
            WRITE( LUNIT9, 3725 ) WORK(13:80)                           M42.8106
3725        FORMAT( A64 )                                               M42.8107
            WRITE( LUNIT9, 3750 )                                       M42.8108
3750        FORMAT( 12X, '9999' )                                       M42.8109
            GO TO 700                                                   M42.8110
         END IF                                                         M42.8111
C                                                                       M42.8112
C     *** CASE '93' ***                                                 M42.8113
C                                                                       M42.8114
         IF( CARD(1:2) .EQ. '93' ) THEN                                 M42.8115
            WRITE( LUNIT6, 675 ) CARD                                   M42.8116
675         FORMAT( ' TYPE-93:  SWITCHED INDUCTANCE', 21X, 1H1, A80 )   M42.8117
            WORK = ' '                                                  M42.8118
            WRITE( 7, 5025 ) CARD(1:78)                                 M42.8119
5025        FORMAT( 'C ', A78 )                                         M42.8120
C                                                                       M42.8121
            READ( CARD, 5100 ) WL1, WL2, YS                             M42.8122
5100        FORMAT( 24X, 3E10.6 )                                       M42.8123
            IF( XOPT .EQ. 0.0 ) THEN                                    M42.8124
               WL1 = WL1 / 1000.0                                       M42.8125
               WL2 = WL2 / 1000.0                                       M42.8126
            ELSE                                                        M42.8127
               WL1 = WL1 / ( TWOPI * XOPT )                             M42.8128
               WL2 = WL2 / ( TWOPI * XOPT )                             M42.8129
            END IF                                                      M42.8130
            N6 = 6                                                      M42.8131
            CALL FLTOPT( YS, N6 )                                       M42.8132
            WRITE( 7, 5250 ) CARD(3:14), ANSI32(27:32), CARD(80:80)     M42.8133
5250        FORMAT( '98', A12, 12X, '   1.0', A6, 42X, A1 )             M42.8134
            WRITE( LUNIT9, 5275 ) CARD(3:14), ANSI32(27:32), CARD(80:80)M42.8135
5275        FORMAT( '98', A12, 12X, '   1.0', A6, 42X, A1 )             M42.8136
            WRITE( 7, 5150 )                                            M42.8137
5150        FORMAT( 'C', 8X, 'CURRENT', 12X, 'FLUX', 50X )              M42.8138
            CUR1 = YS / WL1                                             M42.8139
            CUR2 = CUR1 * 2                                             M42.8140
            Y = YS + ( WL2 * (CUR2 - CUR1))                             M42.8141
            WRITE( WORK(23:70), 5300 ) CUR1, CUR2, Y                    M42.8142
5300        FORMAT( 3E16.7 )                                            M42.8143
            WRITE( 7, 5400 ) WORK(23:38), CARD(45:54)                   M42.8144
5400        FORMAT( A16, 6X, A10, 47X )                                 M42.8145
            WRITE( 7, 5450 ) WORK(39:70)                                M42.8146
5450        FORMAT( A32, 47X )                                          M42.8147
            WRITE( 7, 5500 )                                            M42.8148
5500        FORMAT( 12X, '9999', 65X )                                  M42.8149
            WRITE( LUNIT9, 5600 ) WORK(23:38), CARD(45:54)              M42.8150
5600        FORMAT( A16, 6X, A10, 47X )                                 M42.8151
            WRITE( LUNIT9, 5700 ) WORK(39:70)                           M42.8152
5700        FORMAT( A32, 47X )                                          M42.8153
            WRITE( LUNIT9, 5800 )                                       M42.8154
5800        FORMAT( 12X, '9999', 65X )                                  M42.8155
       END IF                                                           M42.8156
C                                                                       M42.8157
700   CONTINUE                                                          M42.8158
C                                                                       M42.8159
10000 REWIND LUNIT9                                                     M42.8160
      WRITE (LUNIT6,425)                                                M42.8161
425   FORMAT(/,27X, 34HSEQUENTIAL LIST OF PUNCHED OUTPUT.     ,/,       M42.8162
     + 81(1H-),/,1X, '         1         2         3         4',        M42.8163
     + '         5         6         7         8',/, 1X,                M42.8164
     +                    '1234567890123456789012345678901234567890',   M42.8165
     + '1234567890123456789012345678901234567890',                      M42.8166
     + /,   1X,  81( 1H- )  ,/,   1X   )                                M42.8167
       DO 10150 I = 1, 9999                                             M42.8168
         READ( LUNIT9, 10050, END = 10200 ) CARD                        M42.8169
10050    FORMAT( A80 )                                                  M42.8170
         WRITE( LUNIT6, 10100 ) CARD                                    M42.8171
10100    FORMAT( 1X, A80 )                                              M42.8172
10150 CONTINUE                                                          M42.8173
10200 STOP                                                              M42.8174
      END                                                               M42.8175
      SUBROUTINE FLTOPT ( D8, N14 )                                     M42.8176
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C       This module services         , and is designed to optimally     M42.8177
C       encode the floating number  D8  into  ANSI32  of  "DEKSPY".  By M42.8178
C       optimal, we mean a showing of the greatest precision within the M42.8179
C       minimum space.  On input,  N14  is the maximum column width, butM42.8180
C       on output, it signals the beginning column number.  That is, theM42.8181
C       character string  ANSI32(N14:32)  is to be displayed.           M42.8182
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      CHARACTER*1  ONEBYT                                               M42.8185
      CHARACTER*5  ANSI5                                                M42.8186
      IF ( IPRSUP .GE. 1 )                                              M42.8187
     1 WRITE (*, *)  ' TOP "FLTOPT".  D8, N14 =',  D8, N14              M42.8188
      N15 = N14                                                                 
      IF ( D8 .NE. 0.0 )  GO TO 4118                                            
      ANSI32(30:32) = '0.0'                                                     
      N14 = 30                                                                  
      GO TO 4321                                                                
 4118 D9 = ABSZ ( D8 )                                                          
      IF ( D8 .LT. 0.0 )  N15 = N15 - 1                                         
      N5 = 0                                                                    
      IF ( D9 .LT. 1.E6   .AND.                                                 
     1     D9 .GE. 1.E-3 ) GO TO 4169                                           
C       USE OF E-FIELD REQUIRES MASSAGE AND REMOVAL OF EXPONENT FIRST:  M42.8199
      WRITE (SPYCD2, 4126)  D9                                                  
 4126 FORMAT ( E35.22 )                                                 M42.8201
      IF ( IPRSUP .GE. 1 )                                              M42.8202
     1 WRITE (*,*)  ' E-FIELD SPYCD2 BEFORE MASSAGE:', SPYCD2           M42.8203
      N7 = INDEX ( SPYCD2, 'E' )                                                
      IF ( N7 .GT. 0 )  GO TO 4133                                              
      ANSI32(26:32) = 'GARBAGE'                                                 
      N14 = 26                                                                  
      GO TO 4321                                                                
 4133 ANSI32 = SPYCD2(4:35)                                             M42.8209
      K = INDEX ( SPYCD2, 'E+' )                                                
      IF ( K .EQ. 0 )  GO TO 4145                                               
 4138 SPYCD2(4:K+1) = ' '//ANSI32(1:K-3)                                        
      GO TO 4133                                                                
 4145 K = INDEX ( SPYCD2, 'E0' )                                                
      IF ( K .NE. 0  .AND.                                                      
     1     K .LT. 34 ) GO TO 4138                                               
      K = INDEX ( SPYCD2, 'E-0' )                                               
      IF ( K .EQ. 0 ) GO TO 4157                                                
      SPYCD2(4:K+2) = ' '//ANSI32(1:K-2)                                        
      GO TO 4133                                                                
C     WE REACH S.N.4157 HAVING FULLY SIMPLIFIED E-FIELD EXPONENT:       M42.8221
 4157 K = INDEX ( SPYCD2, 'E' )                                                 
      ANSI5 = SPYCD2(K:35)//BLAN80(1:K-31)                                      
      N5 = 36 - K                                                               
      N15 = N15 - N5                                                            
      ANSI32 = SPYCD2(1:K-1)                                                    
      SPYCD2 = '   '//ANSI32                                                    
      READ (SPYCD2, 4172)  D9                                                   
      GO TO 4184                                                                
C     USE OF F-FIELD BEGINS WITH ENCODING OF ABSOLUTE VALUE:            M42.8230
 4169 WRITE (SPYCD2, 4172) D9                                                   
 4172 FORMAT ( F35.24 )                                                 M42.8232
 4184 IF ( IPRSUP .GE. 1 )  WRITE (*, 4188)  SPYCD2                     M42.8233
 4188 FORMAT ( ' AFTER F35.24 ENCODE, SPYCD2(A35) =', A35 )             M42.8234
      N17 = 0                                                                   
      IF ( D9 .LT. 1.0 )                                                        
     1 N17 = INDEX ( SPYCD2, '.' )                                              
      N18 = 0                                                                   
      DO 4252  J=1, 35                                                          
      K = 36 - J                                                                
      IF ( N18 .GT. 0 )  GO TO 4245                                             
      IF ( SPYCD2(K:K) .EQ. '0' ) GO TO 4252                                    
      N18 = K                                                                   
      IF ( N17 .GT. 0 )  GO TO 4256                                             
      GO TO 4252                                                                
 4245 IF ( SPYCD2(K:K) .NE. ' ' ) GO TO 4252                                    
      N17 = K + 1                                                               
      GO TO 4256                                                                
 4252 CONTINUE                                                                  
      CALL STOPTP                                                               
 4256 N4 = N18 - N17 + 1                                                        
      IF ( N4 .LE. N15 ) GO TO 4288                                             
      N18 = N17 + N15 - 1                                                       
C     HAVING CHOPPED NUMBER, WE MUST CHECK FOR "0000.." OR "9999..":    M42.8254
      ONEBYT = SPYCD2(N18+1:N18+1)                                              
      READ (ONEBYT, 4262)  N3                                                   
 4262 FORMAT ( I1 )                                                     M42.8257
      IF ( N3 .GE. 5 ) GO TO 4273                                               
C     ENTER LOOP FOR CASE OF "0000.." (SEARCH LEFT FOR NON-ZERO):       M42.8259
 4264 IF ( SPYCD2(N18:N18) .NE. '0' ) GO TO 4256                                
      N18 = N18 - 1                                                             
      GO TO 4264                                                                
C     ENTER LOOP FOR CASE OF "9999.." (MOVE LEFT, ROUNDING UPWARD):     M42.8263
 4273 ONEBYT = SPYCD2(N18:N18)                                                  
      IF ( ONEBYT .EQ. '.' ) GO TO 4286                                         
      READ (ONEBYT, 4262)  N3                                                   
      N3 = N3 + 1                                                               
      SPYCD2(N18:N18) = DIGIT(N3)                                               
      IF ( N3 .EQ. 10 ) GO TO 4286                                              
      N3 = INDEX ( SPYCD2, '.' )                                                
      IF ( N17 .GT. N18 )  N17 = N18                                            
      IF ( N18 .LT. N3 )  N18 = N3                                              
      GO TO 4293                                                                
 4286 N18 = N18 - 1                                                             
      GO TO 4273                                                                
 4288 IF ( N4 .GE. N15 ) GO TO 4293                                             
      N15 = N4                                                                  
 4293 IF ( IPRSUP .GE. 1 )                                              M42.8278
     1 WRITE (*, 4296)  N4, N17, N18, N15, N14, ANSI32(N14:32)          M42.8279
 4296 FORMAT ( ' F-FIELD.  N4, N17, N18, N15, N14 =', 5I6,              M42.8280
     1         '    ANSI32(N14:32) =',  A  )                            M42.8281
      IF ( N5 .EQ. 0 )  GO TO 4306                                              
      IF ( N18+N5 .GT. 35 )  CALL STOPTP                                M42.8283
      N4 = N18 + N5                                                             
      SPYCD2(N18+1:N4) = ANSI5(1:N5)                                            
      N18 = N4                                                                  
 4306 IF ( D8 .GE. 0.0 )  GO TO 4314                                            
      N17 = N17 - 1                                                             
      SPYCD2(N17:N17) = '-'                                                     
 4314 N17 = N17 - 1                                                             
      SPYCD2(N17:N17) = ' '                                                     
      N14 = 32 - (N18 - N17)                                                    
      ANSI32(N14:32) = SPYCD2(N17:N18)                                          
 4321 ANSI32(1:N14-1) = BLAN80(1:N14-1)                                         
      IF ( IPRSUP .GE. 1 )  WRITE (*, 4387)  N14, ANSI32(N14:32)        M42.8295
 4387 FORMAT ( ' EXIT "FLTOPT".  N14 =', I3, '   ANSI32(N14:32) =', A ) M42.8296
      RETURN                                                            M42.8297
      END                                                               M42.8298
      SUBROUTINE STORE(I,J,N,D2R,D2X,D1R,D1X)                           M27.3402
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      COMMON  /ZPRINT/  ZOUTR(120),ZOUTX(120)                           M28.7048
      IF ( IPRSUP  .GE.  5 )                                            M35.8995
     1 WRITE (LUNIT6, 2472)  I, J, N, D2R, D2X, D1R, D1X                M35.8996
 2472 FORMAT ( 30H I, J, N, D2R, D2X, D1R, D1X =,  3I5, 4E18.8 )        M35.8997
      KIJ = J + I * ( I - 1 ) / 2                                       M27.3404
      K1 = ( N + I - 1 ) * ( N + I ) / 2 + N + J                        M27.3405
      K16 = 2 * N                                                       M27.3406
      K2 = ( K16 + I - 1 ) * (K16 + I ) / 2 + K16 + J                   M27.3407
      KM1 = ( N + J - 1 ) * ( N + J ) / 2 + I                           M27.3408
      KM2 = ( K16 + J - 1 ) * ( K16 + J ) / 2 + I                       M27.3409
      KM3 = KM2 + N                                                     M27.3410
      KM4 = ( N + I - 1 ) * ( N + I ) / 2 + J                           M27.3411
      KM5 = ( K16 + I - 1 ) * ( K16 + I ) / 2 + J                       M27.3412
      KM6 = KM5 + N                                                     M27.3413
      BR = ( D2R + 2.*D1R ) / 3.0                                       M27.3414
      BX = ( D2X + 2.*D1X ) / 3.0                                       M27.3415
      ZOUTR( KIJ ) = BR                                                 M27.3416
      ZOUTR(  K1 ) = BR                                                 M27.3417
      ZOUTR(  K2 ) = BR                                                 M27.3418
      ZOUTX( KIJ ) = BX                                                 M27.3419
      ZOUTX(  K1 ) = BX                                                 M27.3420
      ZOUTX(  K2 ) = BX                                                 M27.3421
      BR = ( D2R - D1R ) / 3.0                                          M27.3422
      BX = ( D2X - D1X ) / 3.0                                          M27.3423
      ZOUTR( KM1 ) = BR                                                 M27.3424
      ZOUTR( KM2 ) = BR                                                 M27.3425
      ZOUTR( KM3 ) = BR                                                 M27.3426
      ZOUTR( KM4 ) = BR                                                 M27.3427
      ZOUTR( KM5 ) = BR                                                 M27.3428
      ZOUTR( KM6 ) = BR                                                 M27.3429
      ZOUTX( KM1 ) = BX                                                 M27.3430
      ZOUTX( KM2 ) = BX                                                 M27.3431
      ZOUTX( KM3 ) = BX                                                 M27.3432
      ZOUTX( KM4 ) = BX                                                 M27.3433
      ZOUTX( KM5 ) = BX                                                 M27.3434
      ZOUTX( KM6 ) = BX                                                 M27.3435
      RETURN                                                            M27.3436
      END                                                               M27.3437
      SUBROUTINE ROUND ( X )                                               15585
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C                                                                          15586
C)    THE PURPOSE OF THIS MODULE IS TO NUMERICALLY ROUND THE ARGUMENT   M15.1505
C)    'X'  TO FIVE DECIMAL DIGITS (OR THEREABOUTS), AND THEN PASS THIS  M15.1506
C)    ROUNDED NUMBER BACK TO THE CALLING MODULE.   THIS VERSION WILL    M15.1507
C)    WORK ON ANY COMPUTER SYSTEM.   YET IF  ENCODE  AND  DECODE  ARE   M15.1508
C     AVAILABLE, IT IS PERHAPS PREFERABLE TO USE THESE, AS PER  ....    M15.1509
C                  DIMENSION  ADUM(5)                                   M15.1510
C                  ENCODE (20, 5540, ADUM(1) )  X                       M15.1511
C                  DECODE (20, 5540, ADUM(1) )  X                       M15.1512
C             5540 FORMAT ( 1E20.5 )                                    M15.1513
      A      = ABSZ(X)                                                  M18. 887
      IF ( A  .EQ.  0.0 )   GO TO 1492                                  M15.1515
      I10    = ALOG1Z(A)                                                M18. 888
      A10    = 10.0 ** ( I10 - 5 )                                      M15.1517
      A      = A / A10                                                  M15.1518
      N1 = A + 0.5                                                      M15.1519
      A = N1                                                            M15.1520
      A = A * A10                                                       M15.1521
      IF ( X  .LT.  0.0 )   A = -A                                      M15.1522
 1492 X = A                                                             M15.1523
      RETURN                                                               15605
      END                                                                  15606
      SUBROUTINE BCTRAN                                                 M29.3507
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MULTI-PHASE TRANSFORMER DATA GENERATOR THAT WAS HANDED OVER       M29.3508
C     TO US AT BPA BY PROF. HERMANN W. DOMMEL OF THE UNIVERSITY OF      M29.3509
C     BRITISH COLUMBIA (VANCOUVER, B.C., CANADA) ON DECEMBER            M29.3510
C     18, 1980.   THIS USES  [A], [B]  REPRESENTATION.   ORIGINAL       M29.3511
C     FILE IS  [SCOTT]HWD.FOR;1 ,   AS READ FROM HERMANN'S CARDS.       M29.3512
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
C     IF NUMBER OF WINDINGS IS TO BE INCREASED FROM 10 TO N,            M29.3514
C     THEN SIMPLY CHANGE 10 TO N AND 55 TO N*(N+1)/2, 30 TO 3*N         M29.3515
C     AND 60 TO 6*N IN NEXT 6 LINES AND IN ONE LINE BETWEEN STATEMENT   M29.3516
C     NUMBER 1 AND 4.                                                   M29.3517
      REAL*8        NAME(60),INAME(6)                                   M29.3518
      DIMENSION  KB(55),KE(55)                                          M29.3519
      DIMENSION X(3),ROUT(30),RMVA(55),Y(30),XO(55)                     M29.3520
      DIMENSION XP(55),ZS(55),ZM(55),ZSD(10),ZMD(10),                   M29.3521
     1 V(10),NAD(55),RWIN2(10)                                          M29.3522
      COMMON /NEWT1/ RWIN(10),ZHL,ZHT,ZLT,K,M,IDELT, LOGSIX             M29.3523
      DATA  MAXIT   /  200  /                                           M29.3525
      LOGSIX = LUNIT6                                                   M32, 255
      ERR = FLZERO * 10.                                                M29.3526
      AQ = UNITY / 3.0                                                  M29.3527
      D1 = 3.0                                                          M29.3528
      OTHREE = UNITY / SQRTZ ( D1 )                                     M29.3529
      ETWO = 1.0D-2                                                     M29.3530
      ETHREE = TENM3                                                    M29.3531
      THREE = 3.0                                                       M29.3532
C     READ INPUT CARD USING CIMAGE.                                     M29.3533
  100 CALL CIMAGE                                                       M29.3534
      DECODE (80, 4001, ABUFF(1))                                       M29.3535
     1       IWIN, FREQ, BEXS, RATPOS, GEXS, BEXM, RATZER, GEXM,        M29.3536
     2       NPHASE, ITEST, IPUT, IPRINT                                M29.3537
 4001 FORMAT (I2, 7E10.2, 4I2 )                                         M29.3538
      IF(IWIN.LE.0) GO TO 312                                           M29.3539
      IF(IWIN.GT.10 .OR. IWIN.EQ.1)GO TO 120                            M29.3540
      IF(NPHASE.NE.1) GO TO 36                                          M29.3541
      BEXM = BEXS                                                       M29.3542
      RATZER = RATPOS                                                   M29.3543
      GEXM = GEXS                                                       M29.3544
      WRITE (LUNIT6, 37)                                                M29.3545
   37 FORMAT(48H THREE PHASE BANK CONSISTS OF SINGLE PHASE UNITS)       M29.3546
   36 WRITE (LUNIT6, 4004)                                              M29.3547
     1      IWIN,FREQ,IPRINT,BEXS,RATPOS,GEXS,BEXM,RATZER,GEXM          M29.3548
 4004 FORMAT(1H0,I6,28H-WINDING TRANSFORMER. FREQ.=,F6.2,2HHZ,          M29.3549
     1  10X,15HOUTPUT OPTION =,I3,//,                                   M29.3550
     27X,57HEXCITING CURRENT(PERCENT) RATING(MVA) EXCITATION LOSS(KW),/,M29.3551
     3 10H POS. SEQ.,F13.5,F18.2,F19.4,/,10H ZERO SEQ.,F13.5,F18.2,     M29.3552
     4  F19.4,/,1X,32HWINDING NO. VOLTAGE(KV)   R(OHM),16X,             M29.3553
     5  35HNODE NAME PAIRS FOR LEGS I, II, III)                         M29.3554
      HFREQ = FREQ                                                      M29.3555
      FREQ = FREQ * TWOPI                                               M29.3556
      K1 = IWIN - 1                                                     M29.3557
      IN = (IWIN * K1) / 2                                              M29.3558
      DO 155 J=1,IN                                                     M29.3559
  155 NAD(J) = 0                                                        M29.3560
      DO 11 K=1,IWIN                                                    M29.3561
C     READ INPUT CARD USING CIMAGE.                                     M29.3562
      CALL CIMAGE                                                       M29.3563
      DECODE (80, 701, ABUFF(1))  M1, Z1, ZO, ( INAME(I), I=1, 6 )      M29.3564
  701 FORMAT(I3, 2E10.2, 1X, 6A6)                                       M29.3565
      WRITE (LUNIT6, 4009) M1, Z1, ZO, (INAME(I), I=1, 6)               M29.3566
 4009 FORMAT (1H , I6, F16.3, F10.5, 6(4X, A6)  )                       M29.3567
      M5 = (M1-1) * 6                                                   M29.3568
      DO 860 I=1, 6                                                     M29.3569
      M5 = M5 + 1                                                       M29.3570
  860 NAME(M5) = INAME(I)                                               M29.3571
      RWIN(M1) = ZO * AQ / Z1**2                                        M29.3572
      RWIN2(K) = 0.0                                                    M29.3573
   11 V(M1) = OTHREE / Z1                                               M29.3574
      WRITE (LUNIT6, 4008)  ITEST, IPUT                                 M29.3575
 4008 FORMAT (34H EXCITATION TEST MADE FROM WINDING, I3,                M29.3576
     1 39H. MAGN. IMPEDANCE PLACED ACROSS WINDING, I3 )                 M29.3577
      GEXS = GEXS * ETHREE                                              M29.3578
      GEXM = GEXM * ETHREE                                              M29.3579
      BEXS = (BEXS*RATPOS*ETWO)**2  -  GEXS**2                          M29.3580
      IF(BEXS.LT. 0.0) GO TO 850                                        M29.3581
      BEXS = SQRTZ ( BEXS )                                             M29.3582
      BEXM = (BEXM*RATZER*ETWO)**2  -  GEXM**2                          M29.3583
      IF(BEXM.LT. 0.0) GO TO 850                                        M29.3584
      BEXM = SQRTZ ( BEXM )                                             M29.3585
      NN1 = ITEST                                                       M29.3586
      NN2 = IPUT                                                        M29.3587
      IMAGN = 0                                                         M29.3588
      IF(ITEST.LT.IPUT) GO TO 12                                        M29.3589
      NN1 = IPUT                                                        M29.3590
      NN2 = ITEST                                                       M29.3591
   12 I = 0                                                             M29.3592
      IF(NN1.LE.0 .AND. NN2.GT.0) GO TO 320                             M29.3593
      WRITE (LUNIT6, 4005)                                              M29.3594
 4005 FORMAT(1H0,24X,17HPOSITIVE SEQUENCE,25X,13HZERO SEQUENCE,10X,     M29.3595
     113H CLOSED DELTA,/,35H FROM  TO  LOAD LOSS(KW)  IMPEDANCE,        M29.3596
     243H(PERCENT)  RATING(MVA)   IMPEDANCE(PERCENT),                   M29.3597
     312H RATING(MVA),5X,2HIN)                                          M29.3598
C     READ INPUT CARD USING CIMAGE.                                     M29.3599
   25 CALL CIMAGE                                                       M29.3600
      DECODE (80, 4202, ABUFF(1))                                       M29.3601
     1      IBG, IEN, H1, XPOS, RATPOS, XZERO, RATZER, IDELT, K3        M29.3602
 4202 FORMAT(2I2, 5E10.2, 2I2)                                          M29.3603
      IF(I.EQ.0) ILOSS = K3                                             M29.3604
      IF(IBG.LE.0.OR.IEN.LE.0) GO TO 10                                 M29.3605
      IF(NPHASE.NE.1)GO TO 38                                           M29.3606
      IDELT = 0                                                         M29.3607
      XZERO = XPOS                                                      M29.3608
      RATZER = RATPOS                                                   M29.3609
   38 WRITE (LUNIT6, 4007)                                              M29.3610
     1       IBG, IEN, H1, XPOS, RATPOS, XZERO, RATZER, IDELT           M29.3611
 4007 FORMAT (1H , 2I4, 2F15.5, F17.2, F16.5, F14.2, 7X, I3 )           M29.3612
      IF(IBG.EQ.IEN) GO TO 300                                          M29.3613
      IF(IBG.GT.IWIN.OR.IEN.GT.IWIN) GO TO 300                          M29.3614
      IF(IBG.LT.IEN) GO TO 20                                           M29.3615
      IDELT = -IDELT                                                    M29.3616
      K3 = IBG                                                          M29.3617
      IBG = IEN                                                         M29.3618
      IEN = K3                                                          M29.3619
   20 IF(I.EQ.0) GO TO 15                                               M29.3620
      DO 35 M=1, I                                                      M29.3621
      IF(KB(M).EQ.IBG.AND.KE(M).EQ.IEN) GO TO 300                       M29.3622
   35 CONTINUE                                                          M29.3623
   15 I = I + 1                                                         M29.3624
      KB(I) = IBG                                                       M29.3625
      KE(I) = IEN                                                       M29.3626
      NAD(I) = IDELT                                                    M29.3627
      RMVA(I) = RATZER / ETWO                                           M29.3628
      IF(IBG.EQ.NN1 .AND. IEN.EQ.NN2) IMAGN = I                         M29.3629
      Z1 = (RWIN(IBG) + RWIN(IEN))**2                                   M29.3630
      IF(H1.LT.EPSILN) GO TO 16                                         M29.3631
      H1 = H1*ETHREE / (RATPOS**2)                                      M29.3632
      Z1 = H1**2                                                        M29.3633
      IF(ILOSS.EQ.0.OR.IWIN.GT.3) GO TO 18                              M29.3634
      H1 = H1 * ONEHAF                                                  M29.3635
      RWIN2(IBG) = RWIN2(IBG) + H1                                      M29.3636
      RWIN2(IEN) = RWIN2(IEN) + H1                                      M29.3637
      K3 = 6 - IBG - IEN                                                M29.3638
      IF(K3.LE.0) K3 = 4                                                M29.3639
      RWIN2(K3) = RWIN2(K3) - H1                                        M29.3640
      GO TO 18                                                          M29.3641
   16 ILOSS = 0                                                         M29.3642
   18 XPOS = (XPOS*ETWO / RATPOS)**2 - Z1                               M29.3643
      IF(XPOS.LT. 0.0) GO TO 301                                        M29.3644
      XP(I) = -SQRTZ ( XPOS )                                           M29.3645
      XO(I) = -XZERO * ETWO / RATZER                                    M29.3646
      IF(IABS(IDELT).GT.0)GO TO 25                                      M29.3647
      XZERO =  (XZERO*ETWO / RATZER)**2 - Z1                            M29.3648
      IF(XZERO.LT. 0.0)GO TO 301                                        M29.3649
      XO(I) = -SQRTZ ( XZERO )                                          M29.3650
      GO TO 25                                                          M29.3651
   10 IF(IN.NE.I) GO TO 310                                             M29.3652
      WRITE (LUNIT6, 901) ILOSS                                         M29.3653
  901 FORMAT(8H ILOSS =, I3)                                            M29.3654
      IF(ILOSS.EQ.0) GO TO 303                                          M29.3655
      IF(IWIN.GT.3) GO TO 303                                           M29.3656
      DO 181 I=1, IWIN                                                  M29.3657
      B = RWIN2(I)                                                      M29.3658
      IF(B.GT. 0.0) GO TO 181                                           M29.3659
      WRITE (LUNIT6, 890) I, B                                          M29.3660
  890 FORMAT(22H RESISTANCE OF WINDING,I3,18H WAS CALCULATED AS,E15.6,/,M29.3661
     124HAND IS SET EQUAL TO ZERO)                                      M29.3662
      B = 0.0                                                           M29.3663
  181 RWIN(I) = B                                                       M29.3664
      WRITE (LUNIT6, 182)                                               M29.3665
  182 FORMAT(42H RESISTANCE MATRIX VALUES CALCULATED FROM ,             M29.3666
     111HLOAD LOSSES)                                                   M29.3667
C     PROCESS INPUT DATA TO ACCOUNT FOR THE CLOSED DELTA EFFECT IN THE  M29.3668
C     ZERO  SEQUENCE  SHORT-CIRCUIT TESTS****************************** M29.3669
   19 I = 0                                                             M29.3670
  800 I = I + 1                                                         M29.3671
      IF(I.GT.IN) GO TO 888                                             M29.3672
      IAD = NAD(I)                                                      M29.3673
      IF(IAD.EQ.0) GO TO 800                                            M29.3674
      IDELT = IABS(IAD)                                                 M29.3675
      K = KB(I)                                                         M29.3676
      M = KE(I)                                                         M29.3677
      IF(IDELT.EQ.K.OR.IDELT.EQ.M) GO TO 830                            M29.3678
      KK2 = 0                                                           M29.3679
      KK3 = 0                                                           M29.3680
      KDELTA = IDELT                                                    M29.3681
      MDELTA = IDELT                                                    M29.3682
      K3 = K                                                            M29.3683
      M1 = M                                                            M29.3684
      IF(K.LT.IDELT) GO TO 801                                          M29.3685
      K3 = IDELT                                                        M29.3686
      KDELTA = K                                                        M29.3687
  801 IF(M.LT.IDELT) GO TO 802                                          M29.3688
      M1 = IDELT                                                        M29.3689
      MDELTA = M                                                        M29.3690
  802 DO 805 J=1, IN                                                    M29.3691
      IF(KB(J).EQ.K3.AND.KE(J).EQ.KDELTA) KK2 = J                       M29.3692
      IF(KB(J).EQ.M1.AND.KE(J).EQ.MDELTA) KK3 = J                       M29.3693
  805 CONTINUE                                                          M29.3694
      IF(KK2.EQ.0.OR.KK3.EQ.0) GO TO 830                                M29.3695
      IF(IAD.GT.0) GO TO 222                                            M29.3696
      K3 = K                                                            M29.3697
      K = M                                                             M29.3698
      M = K3                                                            M29.3699
      K3 = KK2                                                          M29.3700
      KK2 = KK3                                                         M29.3701
      KK3 = K3                                                          M29.3702
  222 ZHT = -XO(KK2)                                                    M29.3703
      ZLT = -XO(KK3)                                                    M29.3704
      ZHL = -XO(I)                                                      M29.3705
      X(1) = ONEHAF * (ZHL + ZHT - ZLT)                                 M29.3706
      K2 = NAD(KK2)                                                     M29.3707
      K3 = NAD(KK3)                                                     M29.3708
      IF(K2.EQ.0.AND.K3.EQ.0) GO TO 210                                 M29.3709
      IF(K2.EQ.0) GO TO 833                                             M29.3710
      IF(K3.NE.0) GO TO 832                                             M29.3711
      IK=KB(KK2)                                                        M29.3712
      IF(K2.LT.0) IK = KE(KK2)                                          M29.3713
      IF(IK.NE.K) GO TO 833                                             M29.3714
      GO TO 212                                                         M29.3715
  210 CALL NEWTON(X, MAXIT, ERR)                                        M29.3716
      X(2) = ZLT - ZHT + X(1)                                           M29.3717
      X(3) = ZHT - X(1)                                                 M29.3718
      GO TO 180                                                         M29.3719
  212 A = ONEHAF * ZLT                                                  M29.3720
      X(2) = A                                                          M29.3721
      X(3) = A                                                          M29.3722
      A1 = RWIN(IDELT)                                                  M29.3723
      A2 = RWIN(M)                                                      M29.3724
      RTOT = A1 + A2                                                    M29.3725
      DNOM = RTOT**2 + ZLT**2                                           M29.3726
      D = A1 *A2  -  A**2                                               M29.3727
      E = A * RTOT                                                      M29.3728
      B = (D * RTOT  +  E * ZLT) / DNOM                                 M29.3729
      C = (RTOT * E  -  D * ZLT) / DNOM                                 M29.3730
      A = ZHL**2 - ( RWIN(K) + B )**2                                   M29.3731
      IF(A.LT. 0.0) GO TO 837                                           M29.3732
      X(1) = SQRTZ ( A )  -  C                                          M29.3733
  172 FORMAT(32H0ZERO SEQUENCE TEST DATA BETWEEN, I3, 4H AND, I3,       M29.3734
     130H IS MODIFIED FOR OPEN DELTA IN, I3, /,                         M29.3735
     250HWITH DELTA CLOSED AGAIN, MODIFIED DATA PRODUCES Z=,            M29.3736
     3E14.6,  46H PERCENT, WHICH SHOULD AGREE WITH INPUT VALUE.  )      M29.3737
C     TO CHECK IMPEDANCES                                               M29.3738
  180 A1 = RWIN(K)                                                      M29.3739
      A2 = RWIN(M)                                                      M29.3740
      A3 = RWIN(IDELT)                                                  M29.3741
      B1 = X(1)                                                         M29.3742
      B2 = X(2)                                                         M29.3743
      B3 = X(3)                                                         M29.3744
      RTOT = A2 + A3                                                    M29.3745
      XTOT = B2 + B3                                                    M29.3746
      A = (A1 * RTOT - B1 * XTOT + A2 * A3 - B2 *                       M29.3747
     1 B3)**2 + (A1*XTOT + B1*RTOT + A2*B3 + A3*B2)**2                  M29.3748
      B = RTOT**2 + XTOT**2                                             M29.3749
      ZHL = A / B                                                       M29.3750
      IF(ZHL.LT. 0.0) GO TO 950                                         M29.3751
      ZHL = SQRTZ (ZHL) *RMVA(I)                                        M29.3752
      WRITE (LUNIT6, 172)K, M, IDELT, ZHL                               M29.3753
      XO(I) =  - B1 - B2                                                M29.3754
      NAD(I) = 0                                                        M29.3755
      IF(K2.EQ.0) GO TO 800                                             M29.3756
      WRITE (LUNIT6, 173)K, IDELT, K, M, M, IDELT                       M29.3757
  173 FORMAT(49H0INPUT VALUE OF ZERO SEQUENCE SHORT-CIRCUIT IMPED,      M29.3758
     19HANCE FROM,I3,3H TO,I3,34H IS IGNORED AND SET EQUAL TO VALUE,    M29.3759
     25H FROM,I3,3H TO,I3,/,40HBECAUSE BOTH IMPEDANCES MUST BE EQUAL IF,M29.3760
     327H THERE ARE CLOSED DELTAS IN, I3, 4H AND, I3)                   M29.3761
      XO(KK2) =  - B1 - B3                                              M29.3762
      NAD(KK2) = 0                                                      M29.3763
      GO TO 800                                                         M29.3764
 888  DO 156 J=1, IN                                                    M29.3765
 156  NAD(J) = 0                                                        M29.3766
C     END  OF  INPUT  DATA  PROCESSING *********************************M29.3767
      IF(NN1.EQ.0) GO TO 154                                            M29.3768
      A = RWIN(ITEST)                                                   M29.3769
      Z1 = 0.0                                                          M29.3770
      ZO = 0.0                                                          M29.3771
      IF(IMAGN.EQ.0) GO TO 158                                          M29.3772
      Z1 = -XP(IMAGN)                                                   M29.3773
      ZO = -XO(IMAGN)                                                   M29.3774
  158 H1 = (GEXS**2 + BEXS**2)*A                                        M29.3775
      IF(GEXS.GE.H1) GO TO 845                                          M29.3776
      GEXS = H1                                                         M29.3777
      WRITE (LUNIT6, 846) GEXS                                          M29.3778
  846 FORMAT(38H0POS. SEQ. EXCITATION LOSSES RAISED TO, E15.6,          M32, 256
     1       3H MW)                                                     M32, 257
  845 H1 = GEXS**2 + BEXS**2                                            M29.3780
      IF(H1.LT.EPSILN) GO TO 157                                        M29.3781
      H11 = GEXS / H1 - A                                               M29.3782
      H1 = BEXS / H1 - Z1                                               M29.3783
      WW = H11**2 + H1**2                                               M29.3784
      GEXS = H11 / WW                                                   M29.3785
      BEXS = H1 / WW                                                    M29.3786
  157 H1 = (GEXM**2 + BEXM**2)*A                                        M29.3787
      IF(GEXM.GE.H1) GO TO 847                                          M29.3788
      GEXM = H1                                                         M29.3789
      WRITE (LUNIT6, 848) GEXM                                          M29.3790
  848 FORMAT(38H0ZERO SEQ. EXCITATION LOSSES RAISED TO, E15.6,          M32, 258
     1       3H MW)                                                     M32, 259
  847 H1 = GEXM**2 + BEXM**2                                            M29.3792
      IF(H1.LT.EPSILN) GO TO 154                                        M29.3793
      H11 = GEXM / H1 - A                                               M29.3794
      H1 = BEXM / H1 - ZO                                               M29.3795
      WW = H11**2 + H1**2                                               M29.3796
      GEXM = H11 / WW                                                   M29.3797
      BEXM = H1 / WW                                                    M29.3798
  154 WRITE (LUNIT6, 853)                                               M29.3799
  853 FORMAT(1H0, 15X, 40HSHUNT RESISTANCES FOR REPRESENTATION OF ,     M29.3800
     118HEXCITATION LOSSES%)                                            M29.3801
      IF (GEXM .GE. GEXS)  GO TO 861                                    M32, 260
      GEXM = GEXS                                                       M32, 261
      WRITE (LUNIT6, 862)                                               M32, 262
  862 FORMAT(45H ZERO SEQUENCE SHUNT RESISTANCE REDUCED TO BE,          M32, 263
     1 34H EQUAL TO POSITIVE SEQUENCE VALUE. )                          M32, 264
  861 IF(ABSZ(GEXM).LT.EPSILN.OR.ABSZ(GEXS).LT.EPSILN) GO TO 852        M32, 265
      GEXM = GEXS / GEXM                                                M29.3803
      GEXS = 1.0 / GEXS                                                 M29.3804
      GEXM = (GEXM - UNITY) * GEXS * AQ                                 M29.3805
      GEXS = GEXM + GEXS                                                M29.3806
      IF(NN1.EQ.0) GO TO 854                                            M29.3807
      Q = V(IPUT)**2                                                    M29.3808
      B = GEXS / Q                                                      M29.3809
      BB = GEXM / Q                                                     M29.3810
      WRITE (LUNIT6, 855) IPUT, B, BB                                   M29.3811
  855 FORMAT(45H PLACE SHUNT RESISTANCE MATRIX ACROSS WINDING, I3,      M29.3812
     118H WITH R(SELF/OHM)=, E15.6, 5X, 19H AND R(MUTUAL/OHM)=, E15.6)  M29.3813
      GO TO 153                                                         M29.3814
  854 WRITE (LUNIT6, 858)                                               M29.3815
  858 FORMAT(51H PLACE SHUNT RESISTANCE MATRIX ACROSS ALL TERMINALS,    M29.3816
     1  27H WITH THE FOLLOWING VALUES%  ,/,                             M29.3817
     2  11HWINDING NO., 10X, 11HR(SELF/OHM), 10X, 13HR(MUTUAL/OHM)  )   M29.3818
      DO 856 I=1, IWIN                                                  M29.3819
      Q = V(I)**2                                                       M29.3820
      B = GEXS * IWIN / Q                                               M29.3821
      BB = GEXM * IWIN / Q                                              M29.3822
  856 WRITE (LUNIT6, 857) I, B, BB                                      M29.3823
  857 FORMAT(I6, E28.6, E21.6)                                          M29.3824
      GO TO 153                                                         M29.3825
  852 WRITE (LUNIT6, 859)                                               M29.3826
  859 FORMAT(46H LEAVE OFF, BECAUSE SERIES RESISTANCES ALREADY,         M29.3827
     155H PRODUCE LOSSES WHICH ARE GREATER THAN INPUT VALUES OF ,       M29.3828
     218HEXCITATION LOSSES.)                                            M29.3829
  153 CONTINUE                                                          M29.3830
      DO 80 I=1, IN                                                     M29.3831
      Z1 = XP(I) * AQ                                                   M29.3832
      ZO = XO(I) * AQ                                                   M29.3833
      ZSS = ZO + 2.0 * Z1                                               M29.3834
      ZMM = ZO - Z1                                                     M29.3835
      IF(KE(I).EQ.IWIN) GO TO 85                                        M29.3836
      XP(I) = -ZSS                                                      M29.3837
      XO(I) = -ZMM                                                      M29.3838
      GO TO 80                                                          M29.3839
   85 M = KB(I)                                                         M29.3840
      ZSD(M) = ZSS                                                      M29.3841
      ZMD(M) = ZMM                                                      M29.3842
      MZ = (M*(M + 1)) / 2                                              M29.3843
      ZS(MZ) = ZSS                                                      M29.3844
      ZM(MZ) = ZMM                                                      M29.3845
      NAD(M) = MZ                                                       M29.3846
   80 CONTINUE                                                          M29.3847
      DO 90 N=1, IN                                                     M29.3848
      IF(KE(N).EQ.IWIN) GO TO 90                                        M29.3849
      IB = KB(N)                                                        M29.3850
      IE = KE(N)                                                        M29.3851
      ZSS = (ZSD(IB) + XP(N) + ZSD(IE)) * ONEHAF                        M29.3852
      ZMM = (ZMD(IB) + XO(N) + ZMD(IE)) * ONEHAF                        M29.3853
      IA = (IE*(IE-1)) / 2                                              M29.3854
      MK = IA + IB                                                      M29.3855
      ZS(MK) = ZSS                                                      M29.3856
      ZM(MK) = ZMM                                                      M29.3857
   90 CONTINUE                                                          M29.3858
      J = IWIN                                                          M29.3859
      IJ = (K1*J) / 2                                                   M29.3860
      IAGAIN = 0                                                        M29.3861
   33 J = J - 1                                                         M29.3862
      IF(J.EQ.0) GO TO 200                                              M29.3863
      H1 = ZS(IJ)                                                       M29.3864
      H11 = ZM(IJ)                                                      M29.3865
      IF(ABSZ(H1).LT.EPSILN) GO TO 110                                  M29.3866
      ZPOS = H1 - H11                                                   M29.3867
      WW = 2.0 * H11                                                    M29.3868
      AZ = ZPOS / (H1 + WW)                                             M29.3869
      ZPOS = 1.0 / ZPOS                                                 M29.3870
      VAR1 = ZPOS * AQ                                                  M29.3871
      H1 = -(2.0 + AZ) * VAR1                                           M29.3872
      H11 =  - (AZ - 1.0) * VAR1                                        M29.3873
      XP(J) = H1                                                        M29.3874
      XO(J) = H11                                                       M29.3875
      IJ = IJ - J                                                       M29.3876
      K = 0                                                             M29.3877
      IK = 0                                                            M29.3878
   44 IK = IK + K                                                       M29.3879
      I1 = IK + 1                                                       M29.3880
      K = K + 1                                                         M29.3881
      IF(K.GT.K1) GO TO 33                                              M29.3882
      IF(K.LT.J) GO TO 99                                               M29.3883
      IF(K.EQ.J) GO TO 77                                               M29.3884
      I = IK + J                                                        M29.3885
   55 H2 = ZS(I)                                                        M29.3886
      H22 = ZM(I)                                                       M29.3887
      DM2 = H22 * H11                                                   M29.3888
      XP(K) = H2 * H1  +  2.0 * DM2                                     M29.3889
      XO(K) = H2 * H11  +  H1 * H22  +  DM2                             M29.3890
      I2 = IK + K                                                       M29.3891
      L = 0                                                             M29.3892
      DO 6 I=I1, I2                                                     M29.3893
      L = L + 1                                                         M29.3894
      DM3 = XP(L)                                                       M29.3895
      DM4 = XO(L) * H22                                                 M29.3896
      ZS(I) = ZS(I)  +  DM3 * H2  +  2.0 * DM4                          M29.3897
    6 ZM(I) = ZM(I)  +  DM3 * H22  +  XO(L) * H2  +  DM4                M29.3898
      IF(K.LT.J) GO TO 44                                               M29.3899
      I = IK + J                                                        M29.3900
      ZS(I) = XP(K)                                                     M29.3901
      ZM(I) = XO(K)                                                     M29.3902
      GO TO 44                                                          M29.3903
   77 I = IJ                                                            M29.3904
      DO 81 L=1, J                                                      M29.3905
      I = I + 1                                                         M29.3906
      ZS(I) = XP(L)                                                     M29.3907
   81 ZM(I) = XO(L)                                                     M29.3908
      GO TO 44                                                          M29.3909
   99 I = IJ + K                                                        M29.3910
      GO TO 55                                                          M29.3911
  200 J = 0                                                             M29.3912
      IF(IAGAIN.EQ.1) GO TO 503                                         M29.3913
      K = 0                                                             M29.3914
      N2 = IN                                                           M29.3915
      N1 = IN + 1                                                       M29.3916
   51 K = K + 1                                                         M29.3917
      I = NAD(K)                                                        M29.3918
      R = -ZS(I)                                                        M29.3919
      S = -ZM(I)                                                        M29.3920
      IF(K.EQ.1) GO TO 48                                               M29.3921
      DO 49 I=N1, N2                                                    M29.3922
      J = J + 1                                                         M29.3923
      A = ZS(J)                                                         M29.3924
      B = ZM(J)                                                         M29.3925
      ZS(I) = ZS(I) - A                                                 M29.3926
      ZM(I) = ZM(I) - B                                                 M29.3927
      R = R - A                                                         M29.3928
   49 S = S - B                                                         M29.3929
   48 J = J + 1                                                         M29.3930
      N2 = N2 + 1                                                       M29.3931
      ZS(N2) = R                                                        M29.3932
      ZM(N2) = S                                                        M29.3933
      IF(K.LT.K1) GO TO 51                                              M29.3934
      A = 0.0                                                           M29.3935
      B = 0.0                                                           M29.3936
      DO 52 I=N1, N2                                                    M29.3937
      A = A - ZS(I)                                                     M29.3938
   52 B = B - ZM(I)                                                     M29.3939
      N2 = N2 + 1                                                       M29.3940
      ZS(N2) = A                                                        M29.3941
      ZM(N2) = B                                                        M29.3942
      BEXM = (BEXM - BEXS) * AQ                                         M29.3943
      BEXS = BEXM + BEXS                                                M29.3944
      IF(IPUT.EQ.0) GO TO 201                                           M29.3945
      NN1 = IPUT*(IPUT + 1) / 2                                         M29.3946
      ZS(NN1) = ZS(NN1) + BEXS                                          M29.3947
      ZM(NN1) = ZM(NN1) + BEXM                                          M29.3948
      GO TO 203                                                         M29.3949
  201 NN1 = 0                                                           M29.3950
      A = UNITY / IWIN                                                  M29.3951
      DO 202 I=1, IWIN                                                  M29.3952
      NN1 = NN1 + I                                                     M29.3953
      ZS(NN1) = ZS(NN1)  +  BEXS * A                                    M29.3954
  202 ZM(NN1) = ZM(NN1)  +  BEXM * A                                    M29.3955
  203 M = 0                                                             M29.3956
      DO 71 K=1, IWIN                                                   M29.3957
      A = V(K) * FREQ                                                   M29.3958
      DO 72 I=1, K                                                      M29.3959
      M = M + 1                                                         M29.3960
      C = V(I) * A                                                      M29.3961
      ZS(M) = ZS(M) * C                                                 M29.3962
   72 ZM(M) = ZM(M) * C                                                 M29.3963
   71 CONTINUE                                                          M29.3964
      ISS = 0                                                           M29.3965
  716 IF(IPRINT.GT.0) GO TO 752                                         M29.3966
      WRITE (LUNIT6, 715)                                               M29.3967
  715 FORMAT(1H0 ,42HBRANCH DATA - RESISTANCE MATRIX (OHMS) AND,        M29.3968
     1  38H INVERSE INDUCTANCE MATRIX (1/HENRIES)  )                    M29.3969
      IF(ISS.LE.0) GO TO 730                                            M29.3970
 506  WRITE (LUNIT6, 709) HFREQ                                         M29.3971
  709 FORMAT(1H0 ,42HBRANCH DATA - RESISTANCE MATRIX (OHMS) AND,        M29.3972
     1  27H REACTANCE MATRIX (OHMS) AT, F6.2, 3H HZ  )                  M29.3973
  730 IB = 3 * IWIN                                                     M29.3974
      ICOUNT = 0                                                        M29.3975
      IF (NPHASE .EQ. 1 )  IB = IWIN                                    M29.3976
  952 DO 508 I=1, IB                                                    M29.3977
 508  ROUT(I) = 0.0                                                     M29.3978
      DO 502 I=1, IWIN                                                  M29.3979
      A = RWIN(I) / V(I)**2                                             M29.3980
      ROUT(I) = A                                                       M29.3981
      IW1 = IWIN + I                                                    M29.3982
      IW2 = IWIN + IW1                                                  M29.3983
      ROUT(IW1) = A                                                     M29.3984
      ROUT(IW2) = A                                                     M29.3985
  502 CONTINUE                                                          M29.3986
      DO 706 II = 1, IB                                                 M29.3987
      IP = ( II - 1 ) / IWIN + 1                                        M29.3988
      IW = II - ( ( II - 1 ) / IWIN ) * IWIN                            M29.3989
      NF = ( IW - 1 ) * 6 + ( IP - 1 ) * 2 + 1 + ICOUNT                 M29.3990
      NT = NF + 1                                                       M29.3991
      DO 708 I = 1, IB                                                  M29.3992
  708 Y(I) = 0.0                                                        M29.3993
      DO 705 JJ = 1, II                                                 M29.3994
      JP = ( JJ - 1 ) / IWIN + 1                                        M29.3995
      JW = JJ - ( ( JJ - 1 ) / IWIN ) * IWIN                            M29.3996
      IF( IW .LT. JW ) GO TO 710                                        M29.3997
      K = ( IW - 1 ) * IW / 2 + JW                                      M29.3998
      GO TO 711                                                         M29.3999
  710 K = ( JW - 1 ) * JW / 2 + IW                                      M29.4000
  711 IF( IP .EQ. JP ) GO TO 703                                        M29.4001
      Y(JJ) = ZM(K)                                                     M29.4002
      GO TO 705                                                         M29.4003
  703 Y(JJ) = ZS(K)                                                     M29.4004
  705 CONTINUE                                                          M29.4005
      II50 = II                                                         M32, 266
      IF(II.EQ.1) GO TO 722                                             M29.4007
      WRITE (LUNIT6, 721)                                               M29.4008
     1        II50, NAME(NF), NAME(NT), (ROUT(I), Y(I), I=1, II)        M29.4009
 721  FORMAT (1H , I2, 2A6, 12X, 2E17.10, /, (1H , 26X, 2E17.10) )      M29.4010
      WRITE (LUNIT7, 726)                                               M29.4011
     1      II50, NAME(NF), NAME(NT), (ROUT(I), Y(I), I=1, II)          M29.4012
 726  FORMAT (I2, 2A6, 12X, 2E16.10, /, (26X, 2E16.10) )                M32, 267
      GO TO 724                                                         M29.4014
  722 WRITE (LUNIT6, 723)                                               M29.4015
     1      II50, NAME(NF), NAME(NT), (ROUT(I), Y(I), I=1, II)          M29.4016
  723 FORMAT (1H , I2, 2A6, 12X, 2E17.10 )                              M29.4017
      WRITE (LUNIT7, 725)                                               M29.4018
     1       II50, NAME(NF), NAME(NT), (ROUT(I), Y(I), I=1, II)         M29.4019
  725 FORMAT (I2, 2A6, 12X, 2E16.10 )                                   M32, 268
  724 ROUT(II) = 0.0                                                    M29.4021
  706 CONTINUE                                                          M29.4022
      IF (NPHASE .NE. 1 ) ICOUNT = 5                                    M29.4023
      ICOUNT = ICOUNT + 2                                               M29.4024
      IF (ICOUNT .LE. 4 ) GO TO 952                                     M29.4025
      IF(ISS.EQ.1.OR.IPRINT.EQ.0) GO TO 100                             M29.4026
  752 IAGAIN = 1                                                        M29.4027
      J = IWIN + 1                                                      M29.4028
      IJ = IWIN * J / 2                                                 M29.4029
      K1 = IWIN                                                         M29.4030
      GO TO 33                                                          M29.4031
  503 M = 0                                                             M29.4032
      DO 505 K=1, IWIN                                                  M29.4033
      DO 504 I=1, K                                                     M29.4034
      M = M + 1                                                         M29.4035
      ZS(M) = -ZS(M) * FREQ                                             M29.4036
  504 ZM(M) = -ZM(M) * FREQ                                             M29.4037
  505 CONTINUE                                                          M29.4038
      ISS = 1                                                           M29.4039
      GO TO 506                                                         M29.4040
  120 WRITE(LUNIT6, 121) IWIN                                           M29.4041
  121 FORMAT(21H NUMBER OF WINDINGS =, I3)                              M29.4042
      CALL STOPTP                                                               
  300 WRITE(LUNIT6, 309) IBG, IEN                                       M29.4044
  309 FORMAT (1H ,2I3,22H WRONG WINDING NUMBERS  )                      M29.4045
      CALL STOPTP                                                               
  301 WRITE(LUNIT6, 302) IBG, IEN                                       M29.4047
  302 FORMAT (45H LOAD LOSSES OR WINDING RESISTANCES TOO LARGE, 2I3 )   M29.4048
      CALL STOPTP                                                               
  303 WRITE(LUNIT6, 304)                                                M29.4050
  304 FORMAT(42H RESISTANCE MATRIX VALUES ARE THOSE WHICH ,             M29.4051
     1  12HWERE READ IN  )                                              M29.4052
      GO TO 19                                                          M29.4053
  310 WRITE(LUNIT6, 311) I, IN                                          M29.4054
  311 FORMAT(5H ONLY, I3, 30H SHORT-CIRCUIT TESTS SPECIFIED,            M29.4055
     1  4H BUT, I3, 11H ARE NEEDED  )                                   M29.4056
      CALL STOPTP                                                               
  320 WRITE(LUNIT6, 321) ITEST, IPUT                                    M29.4058
  321 FORMAT(15H EITHER ITEST =, I3, 10H OR IPUT =, I3,                 M29.4059
     1  14H NOT PERMITTED   )                                           M29.4060
      CALL STOPTP                                                               
  110 WRITE(LUNIT6, 101) J                                              M29.4062
  101 FORMAT(24H DIAGONAL ELEMENT IN ROW, I3,                           M29.4063
     114H CLOSE TO ZERO)                                                M29.4064
      CALL STOPTP                                                               
  830 WRITE(LUNIT6, 840) IDELT, K, M                                    M29.4066
  840 FORMAT(8H IDELTA=, I2, 36H WRONG IN SHORT-CIRCUIT TEST BETWEEN,   M29.4067
     1  I3, 4H AND, I3  )                                               M29.4068
      CALL STOPTP                                                               
  832 KCODE = 1                                                         M29.4070
  835 WRITE(LUNIT6, 834) K, M, KCODE                                    M29.4071
      CALL STOPTP                                                               
  833 KCODE = 2                                                         M29.4073
      GO TO 835                                                         M29.4074
  837 KCODE = 3                                                         M29.4075
      GO TO 835                                                         M29.4076
C 839 KCODE = 4   15 FEB 1983 "M34" IBM EMTP FEEDBACK FROM              M35.8998
C     GO TO 835   PACIFIC POWER: UNREACHABLE S.N. 839.  WSM.            M35.8999
  950 KCODE = 5                                                         M29.4079
      GO TO 835                                                         M29.4080
  850 WRITE(LUNIT6, 851)                                                M29.4081
  851 FORMAT (47H P.U. EXCITATION LOSS LARGER THAN P.U. EXCITING,       M29.4082
     142H CURRENT (EITHER IN POS. OR ZERO SEQUENCE))                    M29.4083
  834 FORMAT (50H MODIFICATION OF ZERO SEQUENCE SHORT-CIRCUIT TEST ,    M29.4084
     1  7HBETWEEN, I3, 4H AND, I3, 27H NOT POSSIBLE. ERROR CODE =, I3)  M29.4085
  312 CALL STOPTP                                                               
      END                                                               M29.4087
      SUBROUTINE NEWTON (X, MAXIT, ERR)                                 M29.4088
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      COMMON /NEWT1/ RWIN(10),ZHL,ZHT,ZLT,K,M,IDELT, LOGSIX             M29.4089
      DIMENSION X(1)                                                    M29.4090
      I = 0                                                             M29.4091
  2   I = I + 1                                                         M29.4092
      IF(I.GE.MAXIT) GO TO 10                                           M29.4093
      A4 = RWIN(K)                                                      M29.4094
      A3 = X(1)                                                         M29.4095
      A2 = RWIN(M)                                                      M29.4096
      A1 = RWIN(IDELT)                                                  M29.4097
      RTOT = A2 + A1                                                    M29.4098
      ZDIF = ZLT - ZHT                                                  M29.4099
      A = A3**2 - 2.0 * ZHT * A3 + A4 * RTOT + A2 * A1 - ZHT * ZDIF     M29.4100
      B =  2.0 * A1 * A3 + A4 * ZLT + A1 * ZDIF + A2 * ZHT              M29.4101
      C = ZHL*ZHL*(RTOT**2 + ZLT**2)                                    M29.4102
      F = A**2 + B**2 - C                                               M29.4103
      DF =  2.0*A*( 2.0*X(1) -  2.0*ZHT) +  2.0* 2.0*A1*B               M29.4104
      DX = -F / DF                                                      M29.4105
      IF(ABSZ(DX).LE.ERR) GO TO 1                                       M29.4106
      X(1) = A3 + DX                                                    M29.4107
      GO TO 2                                                           M29.4108
   10 KCODE = 5                                                         M29.4109
      WRITE(LUNIT6, 12) K, M, KCODE                                     M29.4110
   12 FORMAT(50H MODIFICATION OF ZERO SEQUENCE SHORT-CIRCUIT TEST ,     M29.4111
     1  7HBETWEEN, I3, 4H AND, I3, 27H NOT POSSIBLE. ERROR CODE =, I3)  M29.4112
      CALL STOPTP                                                               
  1   RETURN                                                            M29.4114
      END                                                               M29.4115
      SUBROUTINE CXRED1( AS, CS, N, M )                                 M32, 269
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    FOR AN EXPLANATION  AS TO THE CALLING SEQUENCE OF THIS MODULE,    M15.1524
C)    REFER TO THE SAME SUBROUTINE OF OVERLAY 3.                        M15.1525
C     DEFINE DOUBLE PRECISION FOR SINGLE PRECISION UNIVAC               M32, 270
      DOUBLE PRECISION  A(100), B(100), C(100), D(100)                  M32, 271
      DOUBLE PRECISION  G1, G2, H1, H2, X, Y                            M32, 272
      DIMENSION  AS(1), CS(1)                                           M32, 273
      NNN = N * (N+1) / 2                                               M33.2360
      DO  50  J = 1, NNN                                                M33.2361
         A(J) = AS(J)                                                   M32, 275
   50    C(J) = CS(J)                                                   M32, 276
      J = N + 1                                                            15611
      W=1.0                                                                15612
      IF(M.GT.0) W=-W                                                      15613
      IJ=N*J/2                                                             15614
    3 J=J-1                                                                15615
      IF( J .EQ. M ) GO TO 60                                           M32, 277
      H1=A(IJ)                                                             15617
      G1=C(IJ)                                                             15618
      X=1.0/(H1*H1+G1*G1)                                                  15619
      H1=-H1*X                                                             15620
      G1=G1*X                                                              15621
      B(J)=H1                                                              15622
      D(J)=G1                                                              15623
      IJ=IJ-J                                                              15624
      K=0                                                                  15625
      IK=0                                                                 15626
C                                   BEGIN K-LOOP                           15627
    4 IK=IK+K                                                              15628
      I1=IK+1                                                              15629
      K=K+1                                                                15630
      IF(K.GT.N) GO TO 3                                                   15631
      IF(K.LT.J) GO TO 9                                                   15632
      IF(W.LT.0.) GO TO 3                                                  15633
      IF(K.EQ.J) GO TO 7                                                   15634
      I=IK+J                                                               15635
    5 H2=A(I)                                                              15636
      G2=C(I)                                                              15637
      B(K) = H2*H1-G2*G1                                                   15638
      D(K)=H2*G1+G2*H1                                                     15639
C                                   BEGIN I-LOOP                           15640
      I2=IK+K                                                              15641
      L=0                                                                  15642
      DO 6 I=I1,I2                                                         15643
      L=L+1                                                                15644
      X=B(L)                                                               15645
      Y=D(L)                                                               15646
      A(I)=A(I)+X*H2-Y*G2                                                  15647
    6 C(I)=C(I)+X*G2+Y*H2                                                  15648
      IF(K.LT.J) GO TO 4                                                   15649
      I=IK+J                                                               15650
      A(I)=B(K)                                                            15651
      C(I)=D(K)                                                            15652
      GO TO 4                                                              15653
C                                   END I-LOOP                             15654
    7 I=IJ                                                                 15655
      DO 8 L=1,J                                                           15656
      I=I+1                                                                15657
      C(I)=D(L)                                                            15658
    8 A(I)=B(L)                                                            15659
      GO TO 4                                                              15660
C                                   END K-LOOP                             15661
    9 I=IJ+K                                                               15662
      GO TO 5                                                              15663
C     NNN = N * (N+1) / 2 ! DEFINED AT TOP, SO UNNEDED HERE. WSM        M33.2362
   60 DO  70  J = 1, NNN                                                M33.2363
         AS(J) = A(J)                                                   M32, 279
   70    CS(J) = C(J)                                                   M32, 280
      RETURN                                                            M32, 281
      END                                                                  15664
