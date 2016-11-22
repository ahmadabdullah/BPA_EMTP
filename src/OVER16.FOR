      SUBROUTINE OVER16
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      DIMENSION  XX(1)                                                     12586
      EQUIVALENCE (  XK(1), XX(1) )                                        12588
      DIMENSION  ISPUM(1)                                               M32.4947
      EQUIVALENCE  ( SPUM(1), ISPUM(1) )                                M32.4948
      IF ( IPRSUP  .GE.  1 )                                            M32.4949
     1 WRITE ( LUNIT6, 4567 )                                           M32.4950
 4567 FORMAT (24H  BEGIN MODULE "OVER16". )                             M32.4951
 2001 IF ( NUMUM   .EQ.  0 )   GO TO 2450                               M28.5334
      IF ( LASTOV  .EQ.  1 )   GO TO 2450                               M28.5335
      IF ( LASTOV  .EQ. 20 )   GO TO 2450                                       
      CALL SOLVUM ( SPUM(IUREAC),                                       M33.1467
     1 SPUM(IUGPAR), SPUM(IUFPAR), SPUM(IUHIST), SPUM(IUUMRP),          M33.1468
     2 ISPUM(IUNOD1), ISPUM(IUNOD2), ISPUM(IUJCLT), ISPUM(IUJCLO),      M32.4952
     3 ISPUM(IUJTYP), ISPUM(IUNODO), ISPUM(IUJTMT), SPUM(IUHISM),       M32.4953
     4 SPUM(IUOMGM), SPUM(IUOMLD), SPUM(IUTHAM), SPUM(IUREDU),          M27.1430
     5 SPUM(IUREDS), SPUM(IUFLDS), SPUM(IUFLDR), SPUM(IUREQU),          M27.1431
     6 SPUM(IUFLQS), SPUM(IUFLQR), ISPUM(IUJCDS), ISPUM(IUJCQS),        M32.4954
     7 SPUM(IUFLXD), SPUM(IUFLXQ), ISPUM(IUNPPA), SPUM(IUROTM),         M32.4955
     8 ISPUM(IUNCLD), ISPUM(IUNCLQ), ISPUM(IUJTQO), ISPUM(IUJOMO),      M32.4956
     9 ISPUM(IUJTHO), SPUM(IUREQS), SPUM(IUEPSO), SPUM(IUDCOE),         M32.4957
     9 ISPUM(IUKCOI), SPUM(IUVOLT), SPUM(IUANGL), ISPUM(IUNODF),        M32.4958
     9 ISPUM(IUNODM), ISPUM(IUKUMO), ISPUM(IUJUMO), SPUM(IUUMOU))       M32.4959
 2450 IF ( KILL  .EQ.  0 )   GO TO 2468                                 M22.4714
      NCHAIN = 51                                                       M22.4715
      IF ( IPRSUP  .GE.  1 )                                            M32.4960
     1 WRITE ( LUNIT6, 4568 )  KILL                                     M32.4961
 4568 FORMAT ( 32H EXIT MODULE  "OVER16".   KILL =,  I5  )              M32.4962
      GO TO 99999                                                       M22.4716
 2468 IF ( M4PLOT .EQ. 1 )  CALL EMTSPY                                         
      N1 = NCHAIN - 15                                                  M35.1505
      IPRSUP = IPRSOV(NCHAIN)                                           M28.5337
      IF ( N1  .GT.  4 )   GO TO 99999                                  M22.4718
      IF ( N1  .LE.  0 )   GO TO 99999                                  M22.4719
      GO TO  (3016, 3017, 3018, 3019), N1                               M22.4720
 3016 CALL SUBTS1                                                               
      GO TO 2450                                                        M22.4722
 3017 CALL SUBTS2                                                               
      GO TO 2450                                                        M22.4724
 3018 CALL SUBTS3                                                               
      GO TO 2450                                                        M22.4726
 3019 CALL SUBTS4                                                               
      GO TO 2450                                                        M22.4728
99999 RETURN                                                               12594
      END                                                                  12595
      SUBROUTINE SUBTS1                                                    12597
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR'   ! WSM + THL
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      COMMON  /a8sw/ A8SW(400)                                          M39.2798
      DIMENSION         NSUBKM(1), ISPUM(1), SWCOLD(100)                M42.6677
      EQUIVALENCE  (SPUM(1),  ISPUM(1)),  (KKNONL(1),  NSUBKM(1))       M39.2800
      EQUIVALENCE  (MONCAR(1),    KNT),       (MONCAR(2),  KBASE)       M37.5094
      EQUIVALENCE    (IPRSOV(35), IPOINT),  (IPRSOV(36), IUPPER)        M39.2801
      REAL*8          TEXT1, TEXT2, TEXT3, TEXT4, TEXT5, TEXT6          M28.5338
      REAL*8          TEXT7, TEXT8, TEXT9                               M35.1506
      REAL*8          TEXT10, TEXT11, TEXT12, TEXT13                    M38.3028
      DATA  TEXT1   / 6HVALVE  /                                        M13.2500
      DATA  TEXT2   / 6HDIODE  /                                        M13.2501
      DATA  TEXT3   / 6HOPENIN /                                        M13.2502
      DATA  TEXT4   / 6HCLOSIN /                                        M13.2503
      DATA  TEXT5   / 6HGAP    /                                        M28.5339
      DATA  TEXT6   / 6HSWITCH /                                        M28.5340
      DATA  TEXT8   / 6HCLOSED /                                        M35.1507
      DATA  TEXT9   / 6H OPEN  /                                        M35.1508
      DATA  TEXT10   /  6HSPY     /                                             
      DATA  TEXT11   /  6HPASS    /                                             
      DATA  TEXT12   /  6HSTOP    /                                             
      DATA  TEXT13   /  6HYKK     /                                             
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M32.4966
      DATA  NWARN   /   0   /                                           M32.4967
      IF ( IPRSUP  .GE.  1 )                                            M32.4969
     1 WRITE (LUNIT6, 548)  ISTEP, ISPRIN, ISPLOT, KSWTCH, INONL, NUM99,M17. 833
     2                      IUPPER, KNT, NENERG                         M32.4970
  548 FORMAT ( ' BEGIN 1ST PIECE OF TIME-STEP LOOP.   ISTEP  ISPRIN',   M32.4971
     1     '  ISPLOT  KSWTCH   INONL   NUM99  IUPPER     KNT  NENERG',  M32.4972
     2         /, 35X, 10I8 )                                           M32.4973
      IF ( NUMSM  .GT.  0 )                                             M35.1509
     1 CALL UPDATE                                                      M20.4198
      IF ( KSWTCH .LE. 0 )                                              M32.4974
     1 KCOUNT = NV                                                      M43.2247
 1000 IF ( KSWTCH .LE. 0 ) GO TO 1009                                   M32.4975
      IF ( ISTEP .GT. 0 )   GO TO 3780                                  M35.1510
C     ADD CODE FOR RANDOM OPENING WITH CURRENT MARGIN (FROM OVER5):     M35.1511
      NDX1 = LSWTCH + 1                                                         
      DO 3756  J=1, KSWTCH                                                      
      NDX1 = NDX1 - 1                                                           
      IF ( CRIT(NDX1) .EQ. 0.0 )  GO TO 3756                                    
      IF ( IPRSUP .GE. 1 )                                              M36.1482
     1  WRITE (LUNIT6, 3752)  J, CRIT(J), CRIT(NDX1), TOPEN(J)          M36.1483
 3752 FORMAT ( 23H RANDOM OPEN, SET CRIT.,                              M35.1516
     1         36H  J, CRIT(J), CRIT(NDX1), TOPEN(J) =,                 M35.1517
     2         I6, 3E15.5  )                                            M35.1518
      CRIT(J) = CRIT(NDX1)                                                      
 3756 CONTINUE                                                                  
 3780 CONTINUE                                                                  
C                                   CHECKING SWITCH-POSITIONS FOR CHANGE   12609
      IF ( IPRSUP  .GE.  3 )                                            M25. 574
     1 WRITE (LUNIT6, 8253)  LASTOV, KBASE, NTOT,                       M32.4976
     2                       KPARTB, NCOMP, T, TENERG, DELTAT           M25. 576
 8253 FORMAT ( /, 39H MORE SCALARS.  LASTOV,   KBASE    NTOT,           M28.5341
     1 16H  KPARTB   NCOMP,  14X,  1HT, 9X, 6HTENERG,                   M32.4977
     2  9X,  6HDELTAT  ,/,  14X,  5I8,  3E15.6  )                       M32.4978
      IF ( IPRSUP  .LT.  4 )  GO TO 1326                                M32.4979
      WRITE (LUNIT6, 1314)                                              M32.4980
 1314 FORMAT ( /, 39H SWITCH TABLE AT BEGINNING OF 'SUBTS1'. ,/,        M32.4981
     1 40H     ROW    KPOS    BUS1    BUS2  KSWTYP,                     M39.2802
     2 9X, 6HTCLOSE, 9X, 6HADELAY, 10X, 5HTOPEN, 11X, 4HCRIT )          M32.4983
      DO 3333  K = 1, KSWTCH                                            M32.4984
      NDX1 = LSWTCH + K                                                 M32.4985
 3333 WRITE (LUNIT6, 4444)  K, KPOS(K), KMSWIT(K), KMSWIT(NDX1),        M32.4986
     1         KSWTYP(K), TCLOSE(K), ADELAY(K), TOPEN(K), CRIT(K)       M39.2803
 4444 FORMAT  ( 5I8,  4E15.6 )                                          M39.2804
 1326 IF ( ISTEP .LE. 0 )  GO TO 800                                    M32.4990
      N19 = 0                                                           M32.4991
      N18 = 0                                                           M32.4992
      K = KTRLSW(4)                                                     M32.4993
      IF ( K .EQ. 0 )  GO TO 1216                                       M32.4994
 1214 II = KPOS(K)                                                      M38.3033
      IT1 = KSWTYP(K)                                                   M32.4996
      I=IABS(II)                                                           12621
      VSL=0.                                                               12622
      IF ( I .EQ. 5) GO TO 1002                                            12623
      IF ( I .EQ. 0) GO TO 1002                                            12624
      NDX1 = LSWTCH + K                                                 M32.4997
      N1 = KMSWIT(K)                                                    M32.4998
      N2 = KMSWIT(NDX1)                                                 M32.4999
      TCL=TCLOSE(K)                                                        12629
      NN1=N1                                                               12630
      IF (IT1 .EQ. 9976)  GO TO 3718                                    M23.3783
      IF ( IT1 .GT. 0 )                                                    12631
     1     NN1 = KBUS(IT1)                                              M43.2248
      ISSS = ISOURC(K)                                                     12632
      IF ( ISSS .GT. 0 )                                                   12633
     1 NN1=-NODE(ISSS+1)                                                M43.2249
 3718 IF ( I  .EQ.  10 )  GO TO 2100                                    M13.2505
      IF (  I .EQ. 11 )   GO TO 2108                                    M32.5000
      IF ( I .EQ. 4 )     GO TO 2103                                       12636
      IF (IT1 .EQ. 9976)  GO TO 2110                                    M23.3790
      IF ( IT1 .LE. 0 )   GO TO 2110                                       12637
      GUS4 = (E(NN1)-E(N2)) * DELTA2                                    M05. 603
      CI1 = CK(IT1)                                                     M14.2200
      CK1 = CI1 + GUS4                                                  M14.2201
      CK(IT1)=CK1+GUS4                                                  M05. 605
      IF ( ABSZ(CK1)  .GE.  CRIT(K)   .OR.                              M18. 798
     1     ABSZ(CK1)  .GT.  ABSZ(CI1) )  GO TO 1002                     M18. 799
      GUS4 = GUS4 / DELTA2                                              M14.2204
      IIT1 = -NR(IT1)                                                   M14.2205
      CK1 = X(IIT1) * GUS4                                              M14.2206
      CIK(IT1+1)  =  CK1 + CIK(IT1)  +  CIK(IT1+1)                      M14.2207
      CIK(IT1) = -CK1                                                   M14.2208
      GO TO 2113                                                        M14.2209
 2103 CK1 = E(N2) - E(N1)                                                  12643
      A = (CK1+ENERGY(K)) / 2.0                                         M05. 606
      ENERGY(K) = CK1                                                      12645
      IF ( A .LT. 0.0 )  GO TO 1002                                     M28.5343
      I = 3                                                                12647
      GO TO 2105                                                           12648
C                          PART OF CHECKING SWITCH-POSITIONS FOR CHANGES   12649
 2100 IF ( T.LT.TCL      ) GO TO 1002                                      12650
      IF ( IT1 .LE. 0 ) GO TO 2101                                         12651
      IIT1 = -NR(IT1)                                                      12652
      E(N1) = E(N2)                                                        12653
      GUS4 = E(NN1) - E(N2)                                             M05. 607
      CIK(IT1) = -X(IIT1) * GUS4                                        M05. 608
      GUS4 = GUS4 * DELTA2                                              M05. 609
      CK1 = CK(IT1)                                                        12657
      A = CK1 + GUS4                                                    M05. 610
      CK(IT1) = A + GUS4                                                M05. 611
      IF ( ABSZ(A) .LT. TOPEN(K)  .OR.                                  M18. 800
     1     ABSZ(A) .LT. ABSZ(CK1) ) GO TO 1002                          M18. 801
      D2 = X(IIT1+1) / DELTA2                                           M14.2210
      CI1 = CIK(IT1+1) + D2 * GUS4                                      M14.2211
      CK1 =  X(IIT1) / (DELTA2 * D2 )                                   M14.2212
      D3 = A * ( CK1+1.0 )  -  CI1 / D2                                 M14.2213
      CRIT(K) = ABSZ( D3 / CK1 )                                        M18. 802
      GO TO 2102                                                           12663
 2101 CK1 = E(NN1) - E(N2)                                                 12664
      IF ( ABSZ(CK1) .LT. TOPEN(K)) GO TO 1002                          M18. 803
      ADELAY(K) = T + ADELAY(NDX1)                                      M32.5001
      IF (  ISSS .NE. 0 )  GO TO 8455                                      12667
      IF ( N2 .EQ. 1 ) E(NN1) = E(N2)                                   M43.2250
      IF ( N2 .NE. 1 ) E(N2) = E(NN1)                                      12670
      GO TO 2102                                                        M32.5002
 8455 CONTINUE                                                             12674
      D4 = ABSZ ( CREST(ISSS) )                                         M18. 804
      IF ( CK1 .LT. 0.0 ) D4 = -D4                                      M15.1186
      CK1 = D4                                                          M15.1187
      CREST(ISSS) = -CK1                                                   12676
      CREST(ISSS+1) = CK1                                                  12677
      TSTOP(ISSS) = FLTINF                                                 12678
      TSTOP(ISSS+1) = FLTINF                                               12679
      ENERGY(K) = 0.                                                       12680
 2102 I = 1                                                                12681
 2105 TCLOSE(K) = 0.                                                       12682
      IF ( TCL .GE. 0. ) TCL=T                                             12683
      IF (NENERG .NE. 0 ) GO TO 620                                     M13.2506
      TEXT7 = TEXT8                                                             
      TIMSWT = TCL                                                      M35.1524
  620 N19 = N19 + 1                                                     M32.5004
      MODSWT(N19) = K                                                   M32.5005
      GO TO 2115                                                           12690
 2108 IF ( T .LT. TCL ) GO TO 1002                                         12691
      I = 2                                                                12692
      IF ( TOPEN(K)  .EQ.  FLTINF )  GO TO 2105                         M21. 201
C     IF ( II.GT.0 .AND. TOPEN(K) .GT. TMAX) I=0                   ! DANM42.6678
      GO TO 2105                                                           12694
 2110 L = N1                                                            M32.5006
      IF (NEXTSW(K) .LT. 0 )  L = N2                                    M32.5007
      A = -F(L)                                                         M32.5008
      IT2 = KKS(L)                                                         12698
 2111 IT2 = IT2 - 1                                                     M32.5009
      J = KM(IT2)                                                       M32.5010
      IF ( J .LT. 0 ) GO TO 2116                                           12715
      A =  A  +  YKM(IT2) * E(J)                                        M32.5011
      GO TO 2111                                                           12717
 2116 J = IABS ( J )                                                       12718
      A =  A  +  YKM(IT2) * E ( J )                                     M32.5012
      IF (  IPRSUP .GE. 7 )                                                12720
     1 WRITE(LUNIT6, 3407 )  K, BUS(N1), BUS(N2), L, NEXTSW(K),         M32.5013
     2                       F(L), A                                    M32.5014
 3407 FORMAT (37H    K    BUS1    BUS2    L  NEXTSW(K),                 M37.5095
     1   7X,  4HF(L),  10X,  1HA  ,/,                                   M32.5016
     2   2X, I3, 2( 2X, A6 ), 2X, I3, I11,  2( 2X, E15.5 ) )            M32.5017
      IF ( IPRSUP  .GE.  5 )                                            M22.4730
     1 WRITE (LUNIT6, 8007)  K,  L, II, I, ISSS, IT1,                   M33.1469
     2 TCLOSE(K), T, TOPEN(K), CRIT(K), ADELAY(K)                       M32.5019
 8007 FORMAT ( /, 1X,                                                   M32.5020
     1 48H       K       L      II       I    ISSS     IT1, /, 1X,      M32.5021
     2 6I8  ,/,  7X,  9HTCLOSE(K),  14X,  1HT,  7X,  8HTOPEN(K),        M33.1470
     3 8X,  7HCRIT(K),  6X,  9HADELAY(K)  ,/,  1X,  5E15.6 )            M33.1471
      N4 = N1                                                           M32.5025
      IF ( N4 .EQ. L )  N4 = N2                                         M32.5026
      F(N4) = F(N4) - A                                                 M32.5027
      IF ( NEXTSW(K)  .GT.  0 )  A = -A                                 M32.5028
      GUS1 = A*TCLOSE(K)                                                M05. 612
      IF ( GUS1 .EQ. 0.0  .AND.                                         M32.5029
     1 TCLOSE(K) .NE. 0.)  GUS1 = -1.0                                  M43.2251
      TCLOSE(K) = A                                                        12726
      IF ( II .LE. 0 ) VSL = A                                             12728
      IF ( I .EQ.  3 )   GUS1 = -A                                      M12. 508
      IF ( ISSS .LE. 0 ) GO TO 2112                                        12729
      GUS4 = (E(NN1) - E(N2))*A                                         M05. 613
      ENERGY(K)=ENERGY(K) + GUS4                                        M05. 614
      IF ( GUS4 .LT. 0 ) GO TO 2113                                     M05. 615
 2112 IF ( I .GT. 1                                                        12733
     1     .AND. T.LT.TOPEN(K)) GO TO 1002                              M43.2252
      IF ( ABSZ(A) .LT. CRIT(K) )                                       M18. 805
     1 GUS1=-1.0                                                        M43.2253
      IF ( T  .LT.  ADELAY(K) )                                         M22.4738
     1 GUS1 = 1.0                                                       M43.2254
 2117 IF ( GUS1 .GE. 0.) GO TO 1002                                     M05. 618
 2113 I = I + 1                                                         M32.5030
      IF ( I .NE. 2 )  GO TO 2118                                          12742
      I = 10                                                               12743
      IF ( ISSS .EQ. 0 ) GO TO 2118                                        12744
      TSTOP(ISSS)  = 0.                                                    12745
      TSTOP(ISSS+1)  = 0.                                                  12746
      A = ENERGY(K) * DELTAT                                               12747
      IF ( NENERG .NE. 0 )  GO TO 2118                                  M13.2507
      WRITE (LUNIT6, 2119)  BUS(NN1), BUS(N2), A                        M17. 838
 2119 FORMAT ( 41H ENERGY ABSORBED IN SWITCHED RESISTANCE ',  A6,          12749
     1                                  6H' TO ',    A6, 1H', E15.6 )      12750
 2118 TCLOSE(K) = 0.                                                       12751
      IF ( I .EQ. 3 )  I=5                                                 12752
      ENERGY(K) = 0.0                                                      12753
      N19 = N19 + 1                                                     M32.5031
      MODSWT(N19) = -K                                                  M32.5032
      IF (NENERG .NE. 0)  GO TO 2115                                    M13.2508
      TEXT7 = TEXT9                                                             
      TIMSWT = T                                                        M35.1527
 2115 IF ( IALTER .EQ. 0 ) IALTER = 1                                   M32.5033
      IF (NENERG .NE. 0 )  GO TO 2107                                   M35.1528
      WRITE (LUNIT6, 2106)  BUS(NN1), BUS(N2), TEXT7, TIMSWT            M38.3034
 2106 FORMAT (  4H ***,  65X,  9HSWITCH  ",  A6,  8H"  TO  ",           M35.1531
     1          A6,  3H"  ,  A6,  6H AFTER,  E14.6,  5H SEC.  )         M35.1532
 2107 J = IABS( I )                                                     M35.1533
      IF ( II .LT. 0) J = -J                                               12762
      II = J                                                               12763
      KPOS(K) = II                                                         12764
 1002 IF ( N18 .NE. 0 )  GO TO 1221                                     M32.5034
      K = IABS( NEXTSW(K) )                                             M32.5035
      IF ( K .NE. KTRLSW(4) )  GO TO 1214                               M32.5036
 1216 N18 = 1                                                           M32.5037
      K = 0                                                             M32.5038
 1221 K = K + 1                                                         M32.5039
      IF ( K .GT. KSWTCH ) GO TO 1227                                   M32.5040
      IF ( NEXTSW(K) .EQ. 0 )  GO TO 1214                               M32.5041
      GO TO 1221                                                        M32.5042
 1227 KCOUNT = NV                                                       M32.5043
      DO 1239  K=1, KSWTCH                                              M32.5044
      IF ( KPOS(K)  .GE.  0 )   GO TO 1239                              M32.5045
      KCOUNT = KCOUNT + 1                                               M32.5046
      BVALUE(KCOUNT) = TCLOSE(K)                                        M32.5047
      IF ( NEXTSW(K) .EQ. 0 )                                           M32.5048
     1 BVALUE(KCOUNT) = 0.0                                             M43.2255
 1239 CONTINUE                                                                  
C          IF U.M. IMITATION OF S.M. MODELING (LOOPSS(1)=6644),  THEN   M32.5050
C          GO BACK INTO SOLVUM TO PASS MACHINE QUANTITIES (INCLUDING    M32.5051
C          JUST-CALCULATED TORQUE IN TCLOSE OF SWITCH) TO TACS:         M32.5052
 1009 IF ( LOOPSS(1)  .EQ.  6644 )                                      M32.5053
     1  CALL SOLVUM ( SPUM(IUREAC),                                     M33.1472
     1  SPUM(IUGPAR), SPUM(IUFPAR), SPUM(IUHIST), SPUM(IUUMRP),         M33.1473
     2  ISPUM(IUNOD1), ISPUM(IUNOD2), ISPUM(IUJCLT), ISPUM(IUJCLO),     M32.5056
     3  ISPUM(IUJTYP), ISPUM(IUNODO), ISPUM(IUJTMT), SPUM(IUHISM),      M32.5057
     4  SPUM(IUOMGM), SPUM(IUOMLD), SPUM(IUTHAM), SPUM(IUREDU),         M32.5058
     5  SPUM(IUREDS), SPUM(IUFLDS), SPUM(IUFLDR), SPUM(IUREQU),         M32.5059
     6  SPUM(IUFLQS), SPUM(IUFLQR), ISPUM(IUJCDS), ISPUM(IUJCQS),       M32.5060
     7  SPUM(IUFLXD), SPUM(IUFLXQ), ISPUM(IUNPPA), SPUM(IUROTM),        M32.5061
     8  ISPUM(IUNCLD), ISPUM(IUNCLQ), ISPUM(IUJTQO), ISPUM(IUJOMO),     M32.5062
     9  ISPUM(IUJTHO), SPUM(IUREQS), SPUM(IUEPSO), SPUM(IUDCOE),        M32.5063
     9  ISPUM(IUKCOI), SPUM(IUVOLT), SPUM(IUANGL), ISPUM(IUNODF),       M32.5064
     9  ISPUM(IUNODM), ISPUM(IUKUMO), ISPUM(IUJUMO), SPUM(IUUMOU))      M32.5065
      IF ( KILL  .GT.  0 )  GO TO 9200                                  M32.5066
CCCC  write (*,*) ' TACS?  ktab, istep, newtac =',  ktab, istep, newtac
      IF ( KTAB .le. 0                                                  M38.3035
     1     .or. ISTEP .le. 0 ) go to 3865
      if ( newtac .ne. 1 ) call tacs3
      if ( newtac .eq. 1 ) call ntacs3
C     ---------------------------------------  DIODE, VALVE, GAP  ------M28.5345
C     ------------------------------  AND TACS-CONTROLLED SWITCH  ------M28.5346
 3865 IF ( KSWTCH  .LE.  0 ) GO TO 800                                  M13.2514
      N13 = 0                                                           M42.6679
      DO 801  I=1, KSWTCH                                               M13.2515
      K1 = KDEPSW( LSWTCH + I )                                         M37.5096
      IF ( K1 .EQ. 8888  .OR.                                           M42.6680
     1     K1 .EQ. 8891 ) GO TO 4718                                    M42.6681
      IF ( K1 .NE. 8890 )  GO TO 801                                    M42.6682
      N13 = N13 + 1                                                     M42.6683
      IF ( N13 .LE. 100 ) GO TO 4718                                    M42.6684
      WRITE (6, *) ' Overflow TRIAC storage in SUBTS1.  STOP.'          M42.6685
      STOP                                                              M42.6686
 4718 NDX1 = LSWTCH + I                                                 M42.6687
      NDX2 = LSWTCH + NDX1                                              M35.1535
      NDX3 = LSWTCH + NDX2                                              M35.1536
      N1 = KMSWIT(I)                                                    M32.5069
      N2 = KMSWIT(NDX1)                                                 M35.1537
      N  = IARDUB(I)                                                    M35.1538
      N5 = IARDUB(NDX1)                                                 M35.1539
      I1 = IARDUB(NDX2)                                                 M35.1540
      D1 = ARDUBE(NDX3)                                                 M35.1541
      M = KPOS(I)                                                       M13.2536
      M1 = IABS( M)                                                     M14.2220
      BUS1 = BUS(N1)                                                    M13.2537
      BUS2 = BUS(N2)                                                    M13.2538
      BUS3 = TEXT1                                                      M33.1475
      IF ( N .EQ. 0 )  BUS3 = TEXT2                                     M28.5350
      IF ( K1 .EQ. 8890 )  BUS3 = TEXT5                                 M28.5351
      IF ( K1 .EQ. 8891 )  BUS3 = TEXT6                                 M28.5352
      IF ( IPRSUP  .EQ. 7 )                                             M20.4214
     1 WRITE (LUNIT6, 7218)  I, N, M, N1, N2, N3, N4,                   M33.1476
     2                       I1, M1, D1, BUS1, BUS2                     M20.4216
 7218 FORMAT ( /,  1X,  32H       I       N       M      N1,            M33.1477
     1             40H      N2      N3      N4      I1      M1,  13X,   M20.4218
     2 2HD1,  16H    BUS1    BUS2   ,/,  1X,  9I8,  E15.6,  4X,  2A8 )  M33.1478
      IF ( IPRSUP .EQ. 7 )                                              M42.6688
     1 WRITE (6,*) ' N5, K1, N13, SWCOLD(N13) =',                       M42.6689
     2               N5, K1, N13, SWCOLD(N13)                           M42.6690
C     ------  CHECK  OPEN/CLOSE  CLAMPING  ------                       M28.5353
      IF ( N5 .EQ. 0 )  GO TO 301                                       M28.5354
      NDX1 = KXTCS + N5                                                 M28.5355
      IF ( IPRSUP .EQ. 7 )                                              M42.6691
     1 WRITE (LUNIT6, *) ' NDX1, XTCS(NDX1) =',  NDX1, XTCS(NDX1)       M42.6692
      IF ( XTCS( NDX1) .LT. -FLZERO*10. )  GO TO 809                    M28.5356
      IF ( XTCS( NDX1) .LE. +FLZERO*10. )  GO TO 803                    M28.5357
      IF ( M1 .EQ. 2 )  GO TO 801                                       M14.2227
      GO TO 808                                                         M14.2228
  809 IF ( M1 .EQ. 2 )  GO TO 805                                       M14.2229
      GO TO 801                                                         M14.2230
  301 IF ( K1 .EQ. 8891 )  GO TO 801                                    M28.5358
      IF ( K1 .EQ. 8890                                                 M42.6693
     1     .AND.  N .EQ. 0 )  GO TO 801                                 M43.2257
  803 IF ( K1 .EQ. 8891 )  GO TO 809                                    M28.5359
      IF ( N .EQ. 0                                                     M42.6694
     1     .AND.  K1 .EQ. 8890 )  GO TO 801                             M43.2258
      IF ( M1 .NE. 2 )  GO TO 804                                       M28.5360
C                                         ------  WAS CLOSED  ------    M28.5361
      D5 = TCLOSE( I)                                                   M33.1479
      MK = -ADELAY(I)                                                   M39.2805
      IF ( IPRSUP .EQ. 7 )                                              M42.6695
     1 WRITE (6,*) ' MK, D5 =',  MK, D5                                 M42.6696
      IF ( MK  .LE.  0 ) GO TO 888                                      M39.2806
      IF ( D5 .GT. 0.0 ) GO TO 688                                      M39.2807
      IF ( A8SW(MK+6) .EQ. 0.0 ) GO TO 788                              M39.2808
      IF ( A8SW(MK+6)*D5 .GT. 0.0 ) GO TO 788                           M39.2809
      DELTI = A8SW(MK+6) - D5                                           M39.2810
      DIDT = DELTI / DELTAT                                             M39.2811
      A8SW(MK+3) = DIDT ** A8SW(MK+1) * A8SW(MK)                        M39.2812
      A8SW(MK+4) = A8SW(MK+3) / ( DIDT * A8SW(MK+2) )                   M39.2813
      A8SW(MK+5) = T + D5*DELTAT/DELTI +                                M39.2814
     1             A8SW(MK+4)*A8SW(MK+2)                                M43.2259
  788 IF ( T + DELTAT .LT. A8SW(MK+5) ) GO TO 871                       M39.2815
      ARDUBE(NDX2) = 9999.                                              M39.2816
      GO TO 805                                                         M39.2817
  688 DELTI = A8SW(MK+6) - D5                                           M39.2818
      IF ( DELTI  .LT.  D5 ) GO TO 871                                  M39.2819
      DIDT = DELTI / DELTAT                                             M39.2820
      A8SW(MK+3) = DIDT ** A8SW(MK+1) * A8SW(MK)                        M39.2821
      A8SW(MK+4) = A8SW(MK+3) / ( DIDT * A8SW(MK+2) )                   M39.2822
      A8SW(MK+5) = T + D5*DELTAT/DELTI +                                M39.2823
     1             A8SW(MK+4)*A8SW(MK+2)                                M43.2260
      GO TO 788                                                         M39.2824
  871 A8SW(MK+6) = D5                                                   M39.2825
      GO TO 801                                                         M39.2826
  888 IF ( K1 .NE. 8890 )  GO TO 4761                                   M39.2827
      NDX3 = KXTCS + N                                                  M42.6697
      IF ( IPRSUP .EQ. 7 )                                              M42.6698
     1 Write (6,*) ' Check  XTCS(NDX3) =',  XTCS(NDX3)                  M42.6699
      IF ( XTCS(NDX3) .GT. 10.*FLZERO )  GO TO 801                      M42.6700
      D14 = D5 * SWCOLD(N13)                                            M42.6701
      SWCOLD(N13) = D5                                                  M42.6702
      IF ( D14 .LT. 0.0 ) GO TO 805                                     M42.6703
      D5 = ABSZ ( D5 )                                                  M42.6704
 4761 IF ( D5 .GE. ARDUBE(NDX2) )  GO TO 801                            M42.6705
C                                               ------  OPENING  ------ M13.2551
  805 J = 5                                                             M13.2552
      IF ( M  .LT.  0 )   J = -J                                        M13.2553
      IF ( D1 .NE. 0.0 )  ARDUBE(I) = 0.0                               M35.1543
      KPOS(I) = J                                                       M13.2554
      N19 = N19 + 1                                                     M32.5071
      MODSWT(N19) = -I                                                  M32.5072
      IF ( IALTER .EQ. 0 )  IALTER = 1                                  M32.5073
      IF ( I1  .NE.  0 )                                                M13.2558
     1 WRITE (LUNIT6, 806)  BUS3, BUS1, BUS2, TEXT3, T                  M17. 841
  806 FORMAT ( 51X, A6, 2H ', A6, 6H' TO ', A6, 3H'  , A6,              M13.2560
     1 7HG AFTER, E12.5, 5H SEC.   )                                    M13.2561
      IF ( ISTEP .GT. 1  .OR.  K1 .NE. 8888 )   GO TO 801               M28.5365
      WRITE (LUNIT6, 1128)                                              M21. 203
 1128 FORMAT ( /,  26X,  94( 1H= )   ,/,                                M21. 204
     1 10X,    97HWARNING.  ----  THE JUST-OPENED VALVE OR DIODE WAS CLOM21. 205
     2SED DURING THE STEADY-STATE PHASOR SOLUTION                  ,/,  M21. 206
     3 26X,  94HFOR INITIAL CONDITIONS.   BUT THE RESULTING CURRENT AT TM21. 207
     4IME ZERO WAS FROM CATHODE TO ANODE, SO                       ,/,  M21. 208
     5 26X,  92HOPENING OCCURRED ON TIME-STEP NUMBER ONE.   THE USER MIGM21. 209
     6HT CONSIDER REMOVING  'CLOSED'  FROM                         ,/,  M21. 210
     7 26X,  46HCOLUMNS  55-60  OF THE ASSOCIATED SWITCH CARD.     ,/,  M21. 211
     8 26X,  94( 1H= )  ,/,  1X    )                                    M21. 212
      GO TO 801                                                         M13.2562
C     ------  WAS OPEN  ------                                          M28.5366
  804 ARDUBE(I) = ARDUBE(I) + DELTAT                                    M35.1544
      IF ( ARDUBE(I) .GT. D1 )  ARDUBE(I) = - FLTINF                    M35.1545
      D5 =  E( N1) - E( N2)                                             M33.1480
      NDX1 = LSWTCH + I                                                 M42.6706
      IF ( IPRSUP .EQ. 7 )                                              M42.6707
     1 WRITE (6, *) ' NDX1, ARDUBE(I), D1, D5, ARDUBE(NDX1) =',         M42.6708
     1                NDX1, ARDUBE(I), D1, D5, ARDUBE(NDX1)             M42.6709
      IF ( K1 .EQ. 8890 )  D5 = ABSZ( D5)                               M28.5368
      IF ( D5 .LT. ARDUBE(NDX1) )  GO TO 801                            M35.1546
      IF ( K1 .EQ. 8890 )  GO TO 807                                    M28.5370
      IF ( ARDUBE(I) .LT. 0.0 )  GO TO 807                              M35.1547
      ARDUBE(I) = - FLTINF                                              M35.1548
      GO TO 808                                                         M14.2239
  807 IF ( N .EQ. 0 )  GO TO 808                                        M42.6710
      NDX1 = KXTCS + N                                                  M28.5373
      IF ( XTCS( NDX1) .LE. FLZERO*10. )  GO TO 801                     M28.5374
C     ------  CLOSING  ------                                           M13.2573
  808 J = 2                                                             M14.2243
      IF ( M  .LT.  0 )   J = -J                                        M13.2575
      KPOS(I) = J                                                       M13.2576
      N19 = N19 + 1                                                     M32.5074
      MODSWT(N19) = I                                                   M32.5075
      IF ( IALTER .EQ. 0 )  IALTER = 1                                  M32.5076
      IF ( I1  .NE.  0 )                                                M13.2585
     1 WRITE (LUNIT6, 806)  BUS3, BUS1, BUS2, TEXT4, T                  M17. 842
  801 CONTINUE                                                          M13.2595
      KTRLSW(1) = N19                                                   M32.5077
C     ------------------------------------------------------------------M13.2596
  800 CONTINUE                                                          M13.2597
      IF ( INONL  .LE. 0 )  GO TO 3991                                  M13.2598
      DO 3990  I=1, INONL                                                  12770
      K = NONLK(I)                                                         12771
      M = IABS( NONLM(I) )                                                 12772
      VSL = E(K) - E(M)                                                    12773
      IF (  IPRSUP .GT. 4 )                                                12774
     1 WRITE (LUNIT6, 3906)  I, K, M, NLTYPE(I), NONLAD(I), CURR(I),    M33.1481
     2 E(K), E(M), VNONL(I)                                             M33.1482
 3906 FORMAT ( /, 9H AT  3906   , 5I10, F10.0, 3E15.5  )                   12777
      IF (  NLTYPE(I) .LT. 0 )  GO TO 83903                                12778
      GUS2 = CURR(I)                                                    M05. 619
      IF (  NONLE(I)  .GT.  0 )   GO TO 73960                           M25. 593
      VNONL(I) = VNONL(I) + DELTAT*VSL                                     12781
      GO TO 73960                                                          12782
83903 K9899 = 1                                                            12783
      J = 0                                                                12784
      IF ( NLTYPE(I)  .NE.  -96 )   GO TO 7642                          M23.3791
      N6 = NONLAD(I)                                                    M23.3792
      N7 = CCHAR(N6)                                                    M23.3793
      N9 = ILAST(I)                                                     M23.3794
      N10 = N9 + N7 - 1                                                 M23.3795
      N12 = N6 + 2                                                      M23.3797
      VNONL(I) = VNONL(I) + VSL*DELTA2                                  M23.3798
      CCHAR(N6+3) = VSL * GSLOPE(N6+1) + GSLOPE(N6)                     M23.3799
      GUS2 = CCHAR(N6+3)                                                M23.3800
      IF ((CCHAR(N6+4) .LT. 0) .AND. (VSL .GT. FLZERO) )                M25. 595
     1                                CCHAR(N6+4)=0                     M25. 596
      IF ( IPRSUP  .GE.  2 )                                            M23.3801
     1 WRITE (LUNIT6, 4317)  I, N6, N7,  N9, VNONL(I), GUS2, VSL        M25. 597
 4317 FORMAT ( /,  22H BEGIN TYPE-96 UPDATE.,                           M23.3803
     1  32H       I      N6      N7      N9,   8HVNONL(I),  16X,        M23.3804
     2  4HGUS2,  17X,  3HVSL  ,/,  22X,  4I8,  3E20.11 )                M25. 598
      IF ( VNONL(I)+FLZERO  .LT.  VCHAR(N6+2)     .AND.                 M23.3806
     1   CCHAR(N6+1)  .EQ.  1 )   GO TO 1100                            M23.3807
      IF ( VNONL(I)-FLZERO  .GT.  VCHAR(N6+2)     .AND.                 M23.3808
     1     CCHAR(N6+1)  .EQ.  -1 )   GO TO 1100                         M23.3809
C     THE LAST POINT WAS NOT A REVERSAL POINT OF HYSTERESIS TRAJECTORY. M23.3810
      IF ( ABSZ( VNONL(I) )  .LT.  VCHAR(N10) )   GO TO 1116            M25. 599
      VCHAR(N6) = 0.0                                                   M23.3812
      VCHAR(N6+1) = 0.0                                                 M23.3813
 1116 D7 = VCHAR(N6) * VNONL(I) + VCHAR(N6+1)                           M23.3814
      IF ( D7 .GE. 0.0) GO TO 1118                                      M25. 600
      D7 = 0.0                                                          M25. 601
      VCHAR(N6) = 0.0                                                   M25. 602
      VCHAR(N6+1) = 0.0                                                 M25. 603
 1118 IF ( CCHAR(N6+1) .EQ. 1 ) GO TO 1110                              M25. 604
C     WE ARE ON A DOWNER WITH NO REVERSAL.                              M25. 605
      IF (CCHAR(N6+5) .EQ. 1 ) GO TO 1127                               M28.5375
C     CALCULATE THE DISTANCE BETWEEN THE UPPER AND                      M28.5376
C     LOWER HALF OF THE MAJOR LOOP AT THE PRESENT                       M28.5377
C     OPERATING POINT                                                   M28.5378
      D8 = VNONL(I) + D7                                                M25. 606
      DO 1123 N11=N9,N10                                                M28.5379
      IF (CCHAR(N6+3) .GT. CCHAR(N11) ) GO TO 1123                      M28.5380
      CCHAR(N12) = N11                                                  M28.5381
      GO TO 1125                                                        M28.5382
 1123 CONTINUE                                                          M28.5383
      CCHAR(N12) = N10 + 1                                              M28.5384
 1125 N13 = CCHAR(N12) + N7 + 1                                         M28.5385
      D9 = VCHAR(N13)*CCHAR(N6+3) + CCHAR(N13)                          M28.5386
      D10 = D8 - D9                                                     M28.5387
C     COMPARE AVAILABLE DISTANCE AND NORMAL OPERATION DISTANCE          M28.5388
      IF ( D7 .LE. (D10+FLZERO) ) GO TO 1119                            M30. 516
      CCHAR(N6+5) = 1                                                   M28.5390
 1127 DO 1130 N11=N9,N10                                                M28.5391
      IF (VNONL(I) .GT. VCHAR(N11) ) GO TO 1130                         M28.5392
      CCHAR(N12) = N11                                                  M28.5393
      GO TO 1212                                                        M28.5394
 1130 CONTINUE                                                          M28.5395
      CCHAR(N12) = N10 + 1                                              M28.5396
      GO TO 1212                                                        M28.5397
 1119 DO 1120 N11=N9,N10                                                M28.5398
      IF (-D8 .GT. VCHAR(N11) ) GO TO 1120                              M25. 608
      CCHAR(N12) = N11                                                  M25. 609
      GO TO 1212                                                        M25. 610
 1120 CONTINUE                                                          M25. 611
      CCHAR(N12) = N10 + 1                                              M25. 612
      GO TO 1212                                                        M25. 613
C     WE ARE ON AN UPPER WITH NO REVERSAL.                              M25. 614
 1110 IF (CCHAR(N6+5) .EQ. 1 ) GO TO 1147                               M28.5399
C     CALCULATE THE DISTANCE BETWEEN THE UPPER AND                      M28.5400
C     LOWER HALF OF THE MAJOR LOOP AT THE PRESENT                       M28.5401
C     OPERATING POINT                                                   M28.5402
      D8 = VNONL(I) - D7                                                M28.5403
      DO 1140 N11=N9,N10                                                M28.5404
      IF (-CCHAR(N6+3) .GT. CCHAR(N11) ) GO TO 1140                     M28.5405
      CCHAR(N12) = N11                                                  M28.5406
      GO TO 1141                                                        M28.5407
 1140 CONTINUE                                                          M28.5408
      CCHAR(N12) = N10 + 1                                              M28.5409
 1141 N13 = CCHAR(N12) + N7 + 1                                         M28.5410
      D9 = VCHAR(N13)*CCHAR(N6+3) - CCHAR(N13)                          M28.5411
      D10 = D9 - D8                                                     M28.5412
C     COMPARE AVAILABLE DISTANCE AND NORMAL OPERATION DISTANCE          M28.5413
      IF ( D7 .LE. (D10+FLZERO) ) GO TO 1149                            M30. 517
      CCHAR(N6+5) = 1                                                   M28.5415
 1147 DO 1148 N11=N9,N10                                                M28.5416
      IF (-VNONL(I) .GT. VCHAR(N11) ) GO TO 1148                        M28.5417
      CCHAR(N12) = N11                                                  M28.5418
      GO TO 1312                                                        M28.5419
 1148 CONTINUE                                                          M28.5420
      CCHAR(N12) = N10 + 1                                              M28.5421
      GO TO 1312                                                        M28.5422
 1149 DO 1150 N11=N9,N10                                                M28.5423
      IF ( D8 .GT. VCHAR(N11) ) GO TO 1150                              M25. 617
      CCHAR(N12) = N11                                                  M25. 618
      GO TO 1312                                                        M25. 619
 1150 CONTINUE                                                          M25. 620
      CCHAR(N12) = N10 + 1                                              M25. 621
      GO TO 1312                                                        M25. 622
C     THE PREVIOUS POINT WAS A REVERSAL POINT ON HYSTERESIS TRAJECTORY. M25. 623
 1100 CCHAR(N6+1) = -CCHAR(N6+1)                                        M25. 624
      CCHAR(N6+4) = CCHAR(N6+4) + 1                                     M25. 625
      CCHAR(N6+5) = 0                                                   M28.5424
      IF ( CCHAR(N6+1) .NE. 1 ) GO TO 1195                              M25. 626
C     WE NOW SWITCH TO AN UPPER ( THERE WAS A REVERSAL).                M25. 627
      IF ( VCHAR(N6+2) .GT. -VCHAR(N10) ) GO TO 1580                    M25. 628
      VCHAR(N6+5) = VCHAR(N10)                                          M25. 629
      GSLOPE(N6+5) = CCHAR(N10)                                         M25. 630
      VCHAR(N6) = 0.0                                                   M25. 631
      VCHAR(N6+1) = 0.0                                                 M25. 632
      VCHAR(N6+4) = VCHAR(N9)                                           M25. 633
      GSLOPE(N6+4) = CCHAR(N9)                                          M25. 634
      CCHAR(N12) = N9                                                   M25. 635
      GO TO 1312                                                        M25. 636
 1580 IF ( CCHAR(N6+4) .GT. 1 ) GO TO 1581                              M25. 637
      VCHAR(N6+4) = VCHAR(N10)                                          M25. 638
      GSLOPE(N6+4) = CCHAR(N10)                                         M25. 639
      D6 = 0.0                                                          M25. 640
      GO TO 1575                                                        M25. 641
 1581 DO 1701 N11=N9,N10                                                M25. 642
      IF ( GSLOPE(N6+4) .GT. CCHAR(N11) ) GO TO 1701                    M25. 643
      CCHAR(N12) = N11                                                  M25. 644
      GO TO 1710                                                        M25. 645
 1701 CONTINUE                                                          M25. 646
      CCHAR(N12) = N10 + 1                                              M25. 647
 1710 N13 = CCHAR(N12) + N7 + 1                                         M25. 648
      D9 = VCHAR(N13) * GSLOPE(N6+4) + CCHAR(N13)                       M25. 649
      D6 = VCHAR(N6+4) - D9                                             M25. 650
 1575 DO 1750 N11=N9,N10                                                M25. 651
      IF ( VCHAR(N6+3) .GT. CCHAR(N11) ) GO TO 1750                     M25. 652
      CCHAR(N12) = N11                                                  M25. 653
      GO TO 1760                                                        M25. 654
 1750 CONTINUE                                                          M25. 655
      CCHAR(N12) = N10 + 1                                              M25. 656
 1760 N13 = CCHAR(N12) + N7 + 1                                         M25. 657
      D10 = VCHAR(N13) * VCHAR(N6+3) + CCHAR(N13)                       M25. 658
      D11 = VCHAR(N6+2) -D10                                            M25. 659
      DO 1800 N11=N9,N10                                                M25. 660
      IF ( GSLOPE(N6+5) .GT. CCHAR(N11) ) GO TO 1800                    M25. 661
      N14 = N11                                                         M25. 662
      GO TO 1810                                                        M25. 663
 1800 CONTINUE                                                          M25. 664
      N14 = N10 + 1                                                     M25. 665
 1810 N14 = N14 + N7 + 1                                                M25. 666
      D10 = VCHAR(N14) * GSLOPE(N6+5) + CCHAR(N14)                      M25. 667
      D13 = VCHAR(N6+5) - D10                                           M25. 668
      IF ( VCHAR(N6+2) .GE. VCHAR(N6+5) ) GO TO 1675                    M25. 669
      D14 = 0.0                                                         M25. 670
      IF ( ABSZ(D13) .GT. FLZERO ) D14=D11*D6/D13                       M25. 671
      D9 = D9 + D14                                                     M25. 672
      GO TO 1550                                                        M25. 673
C     WE NOW SWITCH TO A DOWNER ( THERE WAS A REVERSAL ).               M25. 674
 1195 IF ( VCHAR(N6+2) .LT. VCHAR(N10) ) GO TO 1590                     M25. 675
      VCHAR(N6+5) = VCHAR(N9)                                           M25. 676
      GSLOPE(N6+5) = CCHAR(N9)                                          M25. 677
      VCHAR(N6) = 0.0                                                   M25. 678
      VCHAR(N6+1) = 0.0                                                 M25. 679
      VCHAR(N6+4) = VCHAR(N10)                                          M25. 680
      GSLOPE(N6+4) = CCHAR(N10)                                         M25. 681
      CCHAR(N12) = N9                                                   M25. 682
      GO TO 1212                                                        M25. 683
 1590 IF ( CCHAR(N6+4) .GT. 1 ) GO TO 1591                              M28.5425
      VCHAR(N6+4) = VCHAR(N9)                                           M25. 685
      GSLOPE(N6+4) = CCHAR(N9)                                          M25. 686
      D6 = 0.0                                                          M25. 687
      GO TO 1515                                                        M25. 688
 1591 DO 1600 N11=N9,N10                                                M25. 689
      IF ( -GSLOPE(N6+4) .GT. CCHAR(N11) ) GO TO 1600                   M25. 690
      CCHAR(N12) = N11                                                  M25. 691
      GO TO 1610                                                        M25. 692
 1600 CONTINUE                                                          M25. 693
      CCHAR(N12) = N10 + 1                                              M25. 694
 1610 N13 = CCHAR(N12) + N7 + 1                                         M25. 695
      D9 = VCHAR(N13) * GSLOPE(N6+4) - CCHAR(N13)                       M25. 696
      D6 = D9 - VCHAR(N6+4)                                             M25. 697
 1515 DO 1650 N11=N9,N10                                                M25. 698
      IF ( -VCHAR(N6+3) .GT. CCHAR(N11) ) GO TO 1650                    M25. 699
      CCHAR(N12) = N11                                                  M25. 700
      GO TO 1660                                                        M25. 701
 1650 CONTINUE                                                          M25. 702
      CCHAR(N12) = N10 + 1                                              M25. 703
 1660 N13 = CCHAR(N12) + N7 + 1                                         M25. 704
      D10 = VCHAR(N13) * VCHAR(N6+3) - CCHAR(N13)                       M25. 705
      D11 = D10 - VCHAR(N6+2)                                           M25. 706
      DO 1850 N11=N9,N10                                                M25. 707
      IF ( -GSLOPE(N6+5) .GT. CCHAR(N11) ) GO TO 1850                   M25. 708
      N14 = N11                                                         M25. 709
      GO TO 1860                                                        M25. 710
 1850 CONTINUE                                                          M25. 711
      N14 = N10 + 1                                                     M25. 712
 1860 N14 = N14 + N7 + 1                                                M25. 713
      D10 = VCHAR(N14) * GSLOPE(N6+5) - CCHAR(N14)                      M25. 714
      D13 = D10 - VCHAR(N6+5)                                           M25. 715
      IF ( VCHAR(N6+2) .GT. VCHAR(N6+5) ) GO TO 1680                    M25. 716
 1675 VCHAR(N6+5) = VCHAR(N6+4)                                         M25. 717
      GSLOPE(N6+5) = GSLOPE(N6+4)                                       M25. 718
      D13 = D6                                                          M25. 719
      GO TO 1690                                                        M25. 720
 1680 D14 = 0.0                                                         M25. 721
      IF ( ABSZ(D13) .GT. FLZERO ) D14=D11*D6/D13                       M25. 722
      D9 = D9 - D14                                                     M25. 723
 1550 VCHAR(N6+5) = D9                                                  M25. 724
      GSLOPE(N6+5) = GSLOPE(N6+4)                                       M25. 725
      D13 = D14                                                         M25. 726
 1690 VCHAR(N6+4) = VCHAR(N6+2)                                         M25. 727
      GSLOPE(N6+4) = VCHAR(N6+3)                                        M25. 728
      D12 =D11                                                          M25. 729
      IF ( VCHAR(N6+4) .LT. (VCHAR(N6+5)-FLZERO)) GO TO 1700            M25. 730
      IF ( VCHAR(N6+4) .GT. (VCHAR(N6+5)+FLZERO)) GO TO 1700            M25. 731
      IF ( CCHAR(N6+1) .EQ. 1) GO TO 1720                               M25. 732
      VCHAR(N6+5) = VCHAR(N9)                                           M25. 733
      GSLOPE(N6+5) = CCHAR(N9)                                          M25. 734
      D13 = 0.0                                                         M25. 735
      GO TO 1700                                                        M25. 736
 1720 VCHAR(N6+5) = VCHAR(N10)                                          M25. 737
      GSLOPE(N6+5) = CCHAR(N10)                                         M25. 738
      D13 = 0.0                                                         M25. 739
 1700 D15 = D12*(VCHAR(N10)-VCHAR(N6+5)) / (VCHAR(N10)-VCHAR(N6+4))     M25. 740
      IF (CCHAR(N6+1) .EQ. -1. )                                        M27.1438
     1 D15 = D12*(-VCHAR(N10)-VCHAR(N6+5)) / (-VCHAR(N10)-VCHAR(N6+4))  M27.1439
      IF ( D13  .LE.  D15 )       GO TO 1705                            M27.1441
      D13 = D15                                                         M25. 742
      IF ( IPRSUP  .GE.  1 )                                            M29.2022
     1 WRITE (LUNIT6,1706) BUS(K), BUS(M), T                            M29.2023
 1706 FORMAT ( /,  5X,  36HNOTE ---- FOR THE TYPE-96 HYSTERETIC,        M25. 744
     1                  31H INDUCTOR WHICH CONNECTS BUS  ',             M25. 745
     2                    A6,  8H'  TO  ',  A6,                         M25. 746
     3                  22H' ,   A TRAJECTORY WAS          ,/,          M25. 747
     4            15X,  34HINITIALLY CREATED WHICH WOULD HAVE,          M25. 748
     5                  29H CAUSED OPERATION OUTSIDE THE,               M25. 749
     6                  28H MAJOR HYSTERESIS LOOP.  THE    ,/,          M25. 750
     7            15X,  31HTRAJECTORY HAS BEEN MODIFIED TO,             M25. 751
     8                  31H PREVENT THIS.  ACCURACY OF THE,             M25. 752
     9                  30H RESULTS SHOULD BE UNAFFECTED.  ,/,          M27.1444
     9                   5H  T =,  E15.6  )                             M27.1445
      IF ( IPRSUP  .GE.  1 )                                            M29.2024
     1 WRITE (LUNIT6, 7706)  I, D13, D15                                M29.2025
 7706 FORMAT (    15X,  32H ROW NUMBER OF  N.L.  ELEMENT IS,            M27.1447
     1             I4,  26H .   VARIABLES  D13, D15 =, 2E13.4 )         M29.2026
 1705 VCHAR(N6) = (D12 - D13) / (VCHAR(N6+4)-VCHAR(N6+5))               M25. 754
      VCHAR(N6+1) = D12 - VCHAR(N6) * VCHAR(N6+4)                       M25. 755
      IF ( CCHAR(N6+1) .EQ. 1 ) GO TO 1312                              M25. 756
 1212 N14 = CCHAR(N12)                                                  M25. 757
      N13 = N14 + N7 + 1                                                M25. 758
      IF (CCHAR(N6+5) .EQ. 1 ) GO TO 1213                               M28.5427
      D13 = GSLOPE(N14) * (1.0 + VCHAR(N6) )                            M25. 759
      D13 = 1.0 / D13                                                   M25. 760
      D14 = GSLOPE(N13) -  GSLOPE(N14) * VCHAR(N6+1)                    M25. 761
      D14 = D14 * D13                                                   M25. 762
      GO TO 1315                                                        M25. 763
 1213 D13 = VCHAR(N13)                                                  M28.5428
      D14 = CCHAR(N13)                                                  M28.5429
      GO TO 1315                                                        M28.5430
 1312 N14 = CCHAR(N12)                                                  M23.3882
      N13 = N14 + N7 + 1                                                M23.3883
      IF (CCHAR(N6+5) .EQ. 1 ) GO TO 1313                               M28.5431
      D13 = GSLOPE(N14) * ( 1.0 - VCHAR(N6) )                           M23.3884
      D13 = 1.0 / D13                                                   M23.3885
      D14 = GSLOPE(N14) * VCHAR(N6+1) - GSLOPE(N13)                     M23.3886
      D14 = D14 * D13                                                   M23.3887
      GO TO 1315                                                        M28.5432
 1313 D13 = VCHAR(N13)                                                  M28.5433
      D14 = -CCHAR(N13)                                                 M28.5434
 1315 IF ( GSLOPE(N12)  .EQ.  CCHAR(N6+1)     .AND.                     M23.3888
     1     GSLOPE(N6+3)  .EQ.  CCHAR(N12) )   GO TO 1319                M23.3889
      D15 = DELTA2 / D13                                                M23.3890
      A = D15 - GSLOPE(N6+1)                                            M23.3891
      GSLOPE(N6+1) = D15                                                M23.3892
      GO TO 1322                                                        M23.3893
 1319 A = 0.0                                                           M23.3894
 1322 D16 = VNONL(I) - D14 + DELTA2 * VSL                               M23.3895
      D16 = D16 / D13                                                   M23.3896
      GUS1 = D16 - GSLOPE(N6)                                           M23.3897
      GSLOPE(N6) = D16                                                  M23.3898
      GSLOPE(N12) = CCHAR(N6+1)                                         M23.3899
      GSLOPE(N6+3) = CCHAR(N12)                                         M23.3900
      VCHAR(N6+3) = CCHAR(N6+3)                                         M23.3901
      VCHAR(N12) = VNONL(I)                                             M23.3902
      VNONL(I) = VNONL(I) + VSL * DELTA2                                M23.3903
      N8 = N6 + 5                                                       M25. 764
      IF ( IPRSUP  .GE.  2 )                                            M23.3905
     1 WRITE (LUNIT6, 4372)  D14, A, GUS1,  ( IP, CCHAR(IP),            M23.3906
     2                       VCHAR(IP), GSLOPE(IP), IP=N6, N8 )         M23.3907
 4372 FORMAT ( /,  14H DONE TYPE-96.,  17X,  3HD14,  19X,  1HA,         M23.3908
     1  16X,  4HGUS1,  5X,  48H(I, CCHAR(I), VCHAR(I), GSLOPE(I), I=N6, M23.3909
     2N8) ...   ,/,  14X,  3E20.11  ,/,  ( 1X,  I5,  3E20.11,           M23.3910
     3  I5,  3E20.11 )  )                                               M23.3911
      IF ( A  .EQ.  0.0 )  GO TO 3975                                   M23.3912
      GO TO 3950                                                        M23.3913
 7642 IT1 = ABSZ( CURR(I) )                                             M23.3914
      IT2 = NONLAD(I) - 1                                               M33.1483
      GUS2 = 0.0                                                        M05. 620
      IF (  IT1 .NE. 0 )  GO TO 73910                                      12788
      IF (  NLTYPE(I) .NE. -97 )                                           12789
     1 GO TO 3900                                                       M43.2261
      IF (  T .LT. ANONL(I) )  GO TO 73960                                 12790
      IF (  ABSZ(VSL)  .LE.  VNONL(I) )                                 M33.1484
     1 GO TO 73960                                                      M43.2262
      ANONL(I) = T                                                         12792
      CURR(I) = 1.0                                                        12793
      IT2 = IT2 + 1                                                        12794
      A = GSLOPE(IT2)                                                      12795
      GUS1 = 0.0                                                        M05. 621
      GO TO 3950                                                           12797
 3900 IF (  ABSZ(VSL)  .LE.  VNONL(I) )                                 M33.1485
     1 GO TO 73960                                                      M43.2263
      J = 1                                                                12799
      NN1 = ANONL(I)                                                       12800
      IT2 = IT2 + NN1                                                      12801
      CURR(I) = NN1                                                        12802
      IF ( VSL  .LT.  0.0 )                                                12803
     1  CURR(I) = - CURR(I)                                             M43.2264
      VECNL2(I) = T + VECNL1(I)                                         M22.4753
      A = GSLOPE(IT2)                                                      12805
      GUS1 = CCHAR(IT2)                                                 M33.1486
73905 IF (  VSL .LT. 0.0 )                                              M05. 623
     1 GUS1 = -GUS1                                                     M43.2265
      GO TO 3950                                                           12808
73910 IT2 = IT2 + IT1                                                      12809
      GUS2 = VSL * GSLOPE(IT2)                                          M05. 624
      IF (  NLTYPE(I)  .NE.  -98 )  GO TO 73907                            12811
      K9899 = 0                                                            12812
      GUS2 = GUS2 + ANONL(I)                                            M05. 625
      GUS4 = DELTA2 * VSL                                               M05. 626
      VSL = VNONL(I) + GUS4                                             M05. 627
      GO TO 73908                                                          12816
73907 IF (  NLTYPE(I) .NE. -97 )  GO TO 43420                              12817
      IF (  IT2 .GE. NONLE(I) )  GO TO 73960                            M33.1487
      IF (  T-ANONL(I)   .LT.  CCHAR(IT2+1) )  GO TO 73960                 12819
      CURR(I) = CURR(I) + 1.0                                              12820
      A = GSLOPE(IT2+1)  -  GSLOPE(IT2)                                    12821
      GUS1 = 0.0                                                        M05. 628
      GO TO 3950                                                           12823
43420 GUS2 = ABSZ(GUS2) + CCHAR(IT2)                                    M33.1488
      IF (  VSL .LT. 0.0 )  GUS2 = - GUS2                               M05. 630
      IF ( T  .LT.  VECNL2(I) )   GO TO 73908                           M22.4754
      IF (  ABSZ(VSL)   .LT.   VZERO(I) )  GO TO 73942                  M18. 810
73908 A = CURR(I) * VSL                                                    12828
      IF (  A .GT. 0.0 )  GO TO 73913                                      12829
      IF (  IT1 .EQ. 1 )  GO TO 73937                                      12830
      WRITE (LUNIT6, 73912)  I, CURR(I), VSL, GUS2                      M17. 844
73912 FORMAT (  47H TROUBLE AT 73912 ON TYPE-99 OR 98 ELEM NUMBER ,        12832
     1  I2,  E10.1, 2E15.5  )                                              12833
      NN1 = IT2 - 1                                                        12834
      IF ( CURR(I)  .GT.  0.0 )   GO TO 73935                              12835
      GO TO 73915                                                          12836
73913 IF (    IT2    .GE.  NONLE(I) )  GO TO 73930                      M33.1489
      IF (  ABSZ(VSL)  .LE.  VCHAR(IT2) )  GO TO 73930                  M33.1490
      NN1 = IT2 + 1                                                        12839
      IF ( CURR(I)  .LT.  0.0 )   GO TO 73935                              12840
73915 CURR(I) = CURR(I) + 1.0                                              12841
73925 A = GSLOPE(NN1)  -  GSLOPE(IT2)                                      12842
      IF (  K9899 .GT. 0 )  GO TO 73927                                    12843
      GUS3 = CCHAR(NN1)                                                 M33.1491
      IF (  VSL .LT. 0.0 )  GUS3 = -GUS3                                M05. 633
      D1 = ( VSL + GUS4  -  GUS3 ) * GSLOPE(NN1) / DELTA2               M05. 634
      IF (  IPRSUP .GE. 1 )                                                12847
     1 WRITE(LUNIT6, 73926) I, ISTEP, D1, ANONL(I), GUS2, GUS3, VNONL(I)M17. 845
73926 FORMAT ( 23H TYPE-98 SEGMENT CHANGE   ,2I10, /, 1X, 5E25.15 )        12849
      GUS2 = ( ANONL(I) + D1 ) / 2.0                                    M05. 636
      GO TO 3950                                                           12851
73927 GUS1 = CCHAR(NN1)  -  CCHAR(IT2)                                  M33.1492
      GO TO 73905                                                          12853
73930 IF (  IT1 .GT. 1 )  GO TO 73932                                      12854
73931 IF (  K9899 .GT. 0 )  GO TO 73960                                    12855
      GO TO 3973                                                           12856
73932 NN1 = IT2 - 1                                                        12857
      IF (  ABSZ(VSL)  .GE.  VCHAR(NN1) )  GO TO 73931                  M33.1493
      IF ( CURR(I)  .LT.  0.0 )   GO TO 73915                              12859
73935 CURR(I) = CURR(I) - 1.0                                              12860
      GO TO 73925                                                          12861
73937 IF (  K9899 .GT. 0 )  GO TO 73940                                    12862
      CURR(I) = - CURR(I)                                                  12863
      GO TO 3973                                                           12864
73940 IF ( T  .GE.  VECNL2(I) )   GO TO 73942                           M22.4755
      CURR(I) = - CURR(I)                                                  12866
      GO TO 73978                                                          12867
73942 GUS1 = -CCHAR(IT2)                                                M33.1494
      IF ( CURR(I)  .LT.  0.0 )   GUS1 = - GUS1                         M05. 639
      J = 1                                                                12870
      A = - GSLOPE(IT2)                                                    12871
      CURR(I) = 0.0                                                        12872
      D1 = VCHAR(IT2) - SGLFIR                                          M33.1495
      IF ( ABSZ(D1)  .LT.  FLZERO )                                     M18. 813
     1 VNONL(I) = FLTINF                                                M33.1496
 3950 IF ( K  .LT.  M )   GO TO 4113                                    M25. 765
      N8 = M                                                            M25. 766
      N9 = K                                                            M25. 767
      GO TO 4114                                                        M25. 768
 4113 N8 = K                                                            M25. 769
      N9 = M                                                            M25. 770
 4114 IF (  N8 .EQ. 1 )  GO TO 3965                                     M25. 771
      IF ( N8  .GT.  KPARTB )   GO TO 73978                             M25. 772
      N3 = KKS(N8)                                                      M25. 773
 3963 N3 = N3 - 1                                                       M32.5078
      N4 = IABS( KM(N3) )                                               M32.5079
      IF (  N4 .NE. N8 )  GO TO 3964                                    M25. 774
      YKM(N3) = YKM(N3) + A                                             M32.5080
      GO TO  3963                                                          12880
 3964 IF (  N4 .NE. N9 )  GO TO 3963                                    M25. 775
      YKM(N3) = YKM(N3) - A                                             M32.5081
      GO TO 3967                                                           12883
 3965 IF (  N9 .GT. KPARTB )  GO TO 3974                                M25. 776
 3967 IF (  N9 .GT. KPARTB )  GO TO 3972                                M25. 777
      N1 = KKS(N9)                                                      M25. 778
 3970 N1 = N1 - 1                                                       M32.5082
      N2 = IABS( KM(N1) )                                               M32.5083
      IF (  N2 .NE. N9 )  GO TO 3971                                    M25. 779
      YKM(N1) = YKM(N1) + A                                             M32.5084
      GO TO 3972                                                           12891
 3971 IF (  N2 .NE. N8 )  GO TO 3970                                    M25. 780
      YKM(N1) = YKM(N1) - A                                             M32.5085
      GO TO 3970                                                           12894
 3972 IF ( IALTER .EQ. 0 )  IALTER = 1                                  M32.5086
 3974 IF (  K9899 .GT. 0 )  GO TO 3975                                     12896
 3973 GUS1 = 2.0 * ( GUS2 - ANONL(I) )                                  M05. 640
      VNONL(I) = VSL + GUS4                                             M05. 641
      ANONL(I) = ANONL(I) + GUS1                                        M05. 642
      IF (  IPRSUP .GE. 3 )                                                12900
     1 WRITE (LUNIT6, 1974)  I, K, M, CURR(I), GUS2, GUS4, VSL, GUS1,   M17. 846
     2 VNONL(I), ANONL(I), A                                               12902
 1974 FORMAT ( /, 30H TYPE-98 ELEM UPDATE AT  1974.    , 3I10,             12903
     1        F10.0  ,/,  ( 1X, 5E25.15 ) )                                12904
 3975 FINIT(K) = FINIT(K) - GUS1                                        M05. 644
      FINIT(M) = FINIT(M) + GUS1                                        M05. 645
73978 IT1 = CURR(I)                                                     M12. 512
      IF ( J  .LE.  0  ) GO TO 73960                                    M12. 513
      MAX99M = MAX99M - 1                                               M43.2266
      IF ( MAX99M .GE. 0  .OR.  IPRSUP  .GE.  3 )                       M12. 513
     1 WRITE (LUNIT6, 73953)  BUS(K), BUS(M), T, IT1                    M17. 847
73953 FORMAT ( 20X, 23HTYPE-99 N.L. V-I FROM ', A6, 6H' TO ', A6, 1H',     12909
     1  6H AT T=, E12.4,  28H BEGINS OPERATION ON SEGMENT, I4,  1H.  )  M12. 515
73960 IF (  NONLM(I) .GT. 0 )  GO TO 3990                                  12911
      KCOUNT = KCOUNT + 1                                                  12912
      BVALUE(KCOUNT) = GUS2                                             M05. 646
 3990 CONTINUE                                                             12914
 3991 CONTINUE                                                             12915
      IF ( M4PLOT .EQ. 1 )                                                      
     1 CALL YSERLC                                                              
      IF ( IALTER .EQ. 0 )  GO TO 2616                                  M36.1484
      KTRLSW(3) = KTRLSW(3) + 1                                         M32.5088
      IF ( KTRLSW(1) .GT. 0 )                                           M32.5089
     1 CALL SWITCH                                                      M32.5090
      IF ( KANAL  .EQ.  2 )                                             M32.5091
     1 CALL LAST14                                                      M32.5092
C       &&&&&&&&&& ENTER RETRIANGULARIZATION OF COMPLETE (Y)            M32.5093
      CALL MOVE0 ( KSSFRQ(1), NTOT )                                    M37.5097
      II = 0                                                            M32.5094
      L = 1                                                             M32.5095
      GO TO 2290                                                           12925
 2205 L=L+1                                                                12926
      IT2 = KODE (L)                                                    M32.5096
      IF ( IT2 .GT. L) GO TO 2290                                       M32.5097
      IF ( IT2 .EQ. 1 )  GO TO 2290                                     M32.5098
      F(1) = 0.0                                                        M32.5099
      N17 = 1                                                           M32.5100
      J=KKS(L)                                                             12930
 2220 J = J - 1                                                         M32.5101
      N13 = KM(J)                                                       M32.5102
      I = IABS ( N13 )                                                  M32.5103
      IF ( KODE(I) .EQ. 0 ) GO TO 2227                                  M32.5104
 2224 IF ( KODE(I) .LT. I ) GO TO 2222                                  M32.5105
      I = KODE(I)                                                       M32.5106
      GO TO 2224                                                        M32.5107
 2222 IF ( KODE(I) .EQ. 1 )  GO TO 2233                                 M32.5108
 2227 IF ( I .EQ. L )  GO TO 2237                                       M32.5109
      N18 = N17                                                         M32.5110
 2225 JJ = KSSFRQ(N17)                                                  M32.5111
      IF ( JJ .EQ. 0 ) GO TO 2231                                       M32.5112
      IF ( JJ .EQ. I ) GO TO 2229                                       M32.5113
      IF ( JJ .GT. I ) GO TO 2230                                       M32.5114
      N17 = JJ                                                          M32.5115
      GO TO 2225                                                        M32.5116
 2237 F(1) = F(1) + YKM(J)                                              M32.5117
      GO TO 2233                                                        M32.5118
 2229 F(I) = F(I) + YKM(J)                                              M32.5119
      GO TO 2232                                                        M32.5120
 2230 KSSFRQ( I ) = JJ                                                  M32.5121
 2231 KSSFRQ(N17) = I                                                   M32.5122
      F(I) = YKM(J)                                                     M32.5123
 2232 N17 = N18                                                         M32.5124
      IF ( KODE(I) .EQ. 0 )  N17 = I                                    M32.5125
 2233 IF ( N13 .GT. 0 ) GO TO 2220                                      M32.5126
      IF ( IT2 .EQ. 0 ) GO TO 2240                                      M32.5127
      IF ( IT2.EQ.L) GO TO 2240                                            12938
C     ADDITION OF LOWER NUMBERED ROW WHEN SWITCH IS CLOSED.                12939
      J=KKS(IT2)                                                           12940
      IT2 = KODE(IT2)                                                   M32.5128
      N17 = 1                                                           M32.5129
      GO TO 2220                                                           12942
C                       ADDITION OF ELEMENTS FOR CLOSED SWITCHES           12943
 2240 N17 = 1                                                           M32.5130
      ACHECK=ABSZ(F(1))*EPSILN                                          M32.5131
      IF (  ACHECK .NE. 0.0 .OR. KTRANS(L) .EQ. -664422 )  GO TO 2260   M43.2267
      IF (NWARN .LE. 0 )                                                M33.1497
     1 WRITE (LUNIT6, 2253)  BUS(L)                                     M32.5133
 2253 FORMAT ( 38H ZERO DIAGONAL ADMITTANCE;   ZERO YKK.,               M32.5134
     1         40H  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%              M32.5135
     2   ,/,   36H WHILE PREPARING TO ELIMINATE ROW  ",  A6,            M32.5136
     3         41H"  OF THE ADMITTANCE MATRIX,  IT IS FOUND             M32.5137
     4   ,/,   36H THAT THE DIAGONAL IS ZERO DUE TO NO,                 M32.5138
     5         41H CONNECTED ELEMENTS.   ADD HIGH IMPEDANCE             M32.5139
     6   ,/,   40H BRANCH TO EARTH (VALUE 10*FLZERO),  AND,             M32.5140
     7         10H CONTINUE.    )                                       M32.5141
      NWARN = -1                                                        M32.5142
      F(1) = FLZERO * 10.                                               M32.5143
 2260 K = KSSFRQ(N17)                                                   M32.5144
      IF (  K .GT. L  .OR.  K .EQ. 0 ) GO TO 2280                       M32.5145
      A=F(K)                                                               12959
      IF ( A .EQ. 0.) GO TO 2279                                        M32.5146
      N14 = K                                                           M32.5147
      I = KK(K)                                                         M32.5148
 2265 IF ( KM(I) .LT. 0 )  GO TO 2270                                   M32.5149
      I = I - 1                                                         M32.5150
      GO TO 2265                                                        M32.5151
 2270 I = I + 1                                                         M32.5152
 2278 IF ( I .GT. KK(K) )  GO TO 2279                                   M32.5153
      N1=KM(I)                                                          M32.5154
      IF ( N1 .EQ. L )  GO TO 2272                                      M32.5155
 2271 N15 = KSSFRQ(N14)                                                 M32.5156
      IF ( N15 .EQ. N1 )   GO TO 2273                                   M32.5157
      IF ( N15 .GT. N1 )  GO TO 2275                                    M32.5158
      IF ( N15 .EQ. 0 )  GO TO 2276                                     M32.5159
      N14 = N15                                                         M32.5160
      GO TO 2271                                                        M32.5161
 2272 F(1) = F(1) - A * YKM(I)                                          M32.5162
      GO TO 2270                                                        M32.5163
 2273 F(N1)=F(N1)-A*YKM(I)                                              M32.5164
      GO TO 2277                                                        M32.5165
 2275 KSSFRQ(N1) = N15                                                  M32.5166
 2276 KSSFRQ(N14) = N1                                                  M32.5167
      F(N1) = - A * YKM(I)                                              M32.5168
 2277 N14 = N1                                                          M32.5169
      GO TO 2270                                                        M32.5170
 2279 KSSFRQ(N17) = 0                                                   M32.5171
      N17 = K                                                           M32.5172
      GO TO 2260                                                        M32.5173
 2280 A=F(1)                                                            M32.5174
 1280 IF (  ABSZ(A) .GE. ACHECK ) GO TO 4312                                    
      IF ( NWARN .LE. 0 )                                               M33.1498
     1 WRITE (LUNIT6, 2281) BUS(L), A, ACHECK, EPSILN                   M32.5176
 2281 FORMAT ( 43H FLOATING SUBNETWORK.  FLOATING SUBNETWORK.,          M32.5177
     1         40H  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%              M32.5178
     2   ,/,   26H THE ELIMINATION OF ROW  ",  A6,                      M32.5179
     3         37H"  HAS PRODUCED A NEAR-ZERO DIAGNONAL                 M32.5180
     4   ,/,    9H OF VALUE,   E13.5,  15H  JUST PRIOR TO,              M32.5181
     5         43H RECIPROCATION.   THE ACCEPTABLE MINIMUM IS           M32.5182
     6   ,/,    E13.5,  32H  (EQUAL TO "EPSILN" TIMES YKK).,            M32.5183
     7         43H   SHORT NODE TO GROUND WITH  YKK = FLTINF.           M33.1499
     8   ,/,   45H FOR THE RECORD, MISCELLANEOUS DATA PARAMETER,        M33.1500
     9         19H "EPSILN" HAS VALUE,  E12.3,  2H .    )               M33.1501
      NWARN = -1                                                        M32.5185
      A = FLTINF                                                        M32.5186
      IF ( M4PLOT .NE. 1 )  GO TO 4312                                          
      NWARN = 0                                                                 
      WRITE (MUNIT6, 3688)                                              M38.3039
 3688 FORMAT (  39H   ++++  No, YKK grounding is optional.  )           M38.3040
      CALL WINDOW                                                               
      CALL HONKER( LL10 )                                                       
 3711 WRITE (PROM80, 3712)                                              M38.3043
 3712 FORMAT ( ' SEND REMEDY (SPY, YKK, PASS, STOP) :' )                M38.3044
      CALL PROMPT                                                               
      LOCKBR = 1                                                                
      CALL FLAGER                                                               
      READ (BUFF77, 3719)  BUS1                                                 
 3719 FORMAT ( A6 )                                                     M38.3049
      IF ( BUS1 .NE. TEXT10 )  GO TO 3726                                       
      CALL SPYING                                                               
      GO TO 3711                                                                
 3726 IF ( BUS1 .EQ. TEXT11 )  GO TO 4312                                       
      IF ( BUS1 .EQ. TEXT12 )                                                   
     1 CALL STOPTP                                                              
      IF ( BUS1 .NE. TEXT13 )  GO TO 3711                                       
      WRITE (PROM80, 3733)                                              M38.3057
 3733 FORMAT ( 44H SEND NEW YKK VALUE AT RECIPROCATION POINT : )        M38.3058
      CALL PROMPT                                                               
      CALL FLAGER                                                               
      CALL FREFP1 ( BUFF77, A )                                                 
      GO TO 1280                                                                
 4312 A = 1.0 / A                                                          12975
      II=II+1                                                              12976
      KM(II)=-L                                                            12977
      YKM(II)=A                                                            12978
      GO TO 2285                                                           12979
 2283 IF (  F(K) .EQ. 0.0 )  GO TO 2288                                 M32.5187
      II = II + 1                                                       M32.5188
      YKM(II)=F(K)*A                                                       12984
      KM(II)=K                                                             12985
 2288 KSSFRQ(N17) = 0                                                   M32.5189
      N17 = K                                                           M32.5190
      K = KSSFRQ(K)                                                     M32.5191
 2285 IF ( K .NE. 0 )  GO TO 2283                                       M32.5192
      KK(L) = II                                                        M32.5193
      IF (  IPRSUP .GT. 1 )                                                12987
     1 WRITE (LUNIT6, 2286)  L, II, KK(L), KPARTB, A                    M32.5194
 2286 FORMAT ( 38H DONE TRIANGULARIZING NEW ROW OF (Y) .,               M32.5195
     1 32H       L      II   KK(L)  KPARTB,  24X,  1HA  ,/,             M32.5196
     2 38X,  4I8,  E25.16 )                                             M32.5197
      IF ( II  .LT.  KKS(1) )   GO TO 2290                              M32.5198
      KILL = 1                                                          M19.1613
      LSTAT(13) = 2                                                     M32.5199
      LSTAT(15) = L                                                     M32.5200
      LSTAT(16) = 5                                                     M19.1615
      LSTAT(19) = 2290                                                     12992
      GO TO 9200                                                        M19.1616
 2290 IF (  L .LT. KPARTB )  GO TO 2205                                    12994
      IUPPER = II                                                       M32.5201
      IF ( NWARN .LT. 0 )  NWARN = 1                                    M32.5202
      N4 = IUPPER + LYMAT - KKS(1) + 1                                  M32.5203
      IF ( N4  .GT.  LSTAT(25) )   LSTAT(25) = N4                       M32.5204
      IF (  IPRSUP .GT. 0 )                                                12998
     1 WRITE (LUNIT6, 2291)  IUPPER, LYMAT, KKS(1)                      M32.5205
 2291 FORMAT ( /,  34H UPPER-TRIANGULAR FACTORS FOR (Y).,               M32.5206
     1    4X,  31H(I, KM(I), YKM(I), I=1, IUPPER)   ,                   M32.5207
     2 10X, 23HIUPPER, LYMAT, KKS(1) =,  3I6  )                         M32.5208
      IF (  IPRSUP .GT. 2 )                                                13003
     1 WRITE (LUNIT6, 2292 )  ( I, KM(I), YKM(I), I=1, IUPPER )         M32.5209
 2292 FORMAT ( 1X,   5( I6, I5,  E15.6 )   )                            M32.5210
      IF (  IPRSUP .GT. 1 )                                                13006
     1 WRITE (LUNIT6, 2293)  ( KK(I), I=1, NTOT )                       M17. 852
 2293 FORMAT ( 18H KK(I), I=1, NTOT   ,/, ( 1X, 20I6 ) )                   13008
      IF (  IPRSUP .GT. 3 )                                                13009
     1 WRITE (LUNIT6, 2296)  NUM99, INONL, NCOMP, ISTEP, INDSTP,        M21. 223
     2                       NTOT, LPAST, IPOINT, KSWTCH                M21. 224
 2296 FORMAT ( /,  1X,  40H   NUM99   INONL   NCOMP   ISTEP  INDSTP,    M21. 225
     1 32H    NTOT   LPAST  IPOINT  KSWTCH  ,/,  1X,  9I8  )            M21. 226
C     START.  FIND DIFFERENCES OF COLUMNS OF INVERSE FOR NONLINEARITIES*   13014
 2300 N13 = 0                                                           M35.1551
      J11 = 0                                                           M39.2829
      IF (  NCOMP .LE. 0 )  GO TO 2616                                  M35.1552
      J11 = NTOT * NCOMP                                                M42.6711
      CALL MOVER0 ( ZNONL(1), J11 )                                     M37.5162
      IF (  INONL .EQ. NUM99 )  GO TO 2321                                 13018
      DO 2320 I=1,INONL                                                    13019
      IF ( NLTYPE(I) .LT. 0 )  GO TO 2320                                  13020
      IF ( NLTYPE(I) .GT. 920 )  GO TO 2320                             M37.5163
      VZERO(I) = - FLTINF                                                  13021
      N1=NONLAD(I)                                                         13022
      ILAST(I)=IABS(N1)                                                    13023
      IF ( N1.LT.0) VZERO(I)=0.                                            13024
 2320 CONTINUE                                                          M25. 786
 2321 DO 2322  N2=1, NUMSUB                                             M42.6712
      N1 = 0                                                            M25. 789
      L = ISUBEG(N2)                                                    M37.5165
      IF ( L  .LE.  0 )  GO TO 2322                                     M37.5166
 2306 D8 = -1.0                                                         M25. 791
      J = NSUBKM(L+1)                                                   M37.5167
      DO 4323  N10=1, 2                                                 M25. 793
      IF ( KPSOUR(J)  .EQ.  0 )  GO TO 2315                             M37.5168
 4377 IF ( KODE(J) .LT. J ) GO TO 4319                                  M32.5212
      J = KODE(J)                                                       M32.5213
      GO TO 4377                                                        M32.5214
 4319 IF ( KODE(J)  .EQ.  1 )   GO TO 2315                              M33.1503
      N5 = J + N1                                                       M33.1504
      ZNONL(N5) = D8                                                    M25. 797
 2315 D8 = 1.0                                                          M37.5169
 4323 J = NSUBKM(L+2)                                                   M37.5170
      L = NSUBKM(L)                                                     M37.5171
      IF ( L  .EQ.  ISUBEG(N2) )   GO TO 2322                           M37.5172
      N1 = N1 + NTOT                                                    M25. 806
      GO TO 2306                                                        M25. 807
 2322 CONTINUE                                                          M25. 808
      IF (  IPRSUP .GE. 2 )                                                13044
     1 WRITE(LUNIT6, 2318)  ( ZNONL(I), I=1, J11 )                      M37.5173
 2318 FORMAT ( /, 40H (ZNONL(1:J11) BEFORE SOLUTION, AT 1313.     ,/,   M37.5174
     1       ( 1X, 8E15.5 ) )                                           M35.1556
      II = 1                                                               13072
      N4 = -KPARTB                                                      M25. 809
 2410 IF ( II .GT. IUPPER ) GO TO 2450                                  M32.5224
      L = IABS(KM(II))                                                     13077
      N8 = L                                                            M25. 810
      DO 2413  M=1, NCOMP                                               M25. 811
      VOLTBC(M) = ZNONL(N8)                                             M31.3186
      ZNONL(N8) = ZNONL(N8) * YKM(II)                                   M25. 813
 2413 N8 = N8 + NTOT                                                    M25. 814
      J=IABS(KK(L))                                                        13081
 2420 II=II+1                                                              13087
      IF ( II.GT.J) GO TO 2410                                             13088
      K=KM(II)                                                             13089
      IF ( K .LE. KPARTB ) GO TO 2422                                   M32.5225
      II = J + 1                                                        M32.5226
      GO TO 2410                                                        M32.5227
 2422 N8 = L                                                            M33.1505
      DO 2423  M=1, NCOMP                                               M33.1506
      ZNONL(K) = ZNONL(K)  -  VOLTBC(M) * YKM(II)                       M32.5229
      N8 = N8 + NTOT                                                    M37.5175
 2423 K = K + NTOT                                                      M32.5240
      GO TO 2420                                                           13113
 2450 IF ( IPRSUP  .GE.  2 )                                            M37.5176
     1 WRITE (LUNIT6, 2341)  ( ZNONL(K), K=1, J11 )                     M37.5177
 2341 FORMAT ( /, 25H (ZNONL(K), K=1, J11) ...  ,/,                     M37.5178
     1  ( 1X,  8E16.6 )  )                                              M25. 833
 2500 IF ( II.EQ.1) GO TO 2550                                             13156
      CALL MOVER0 ( VOLTBC(1), NCOMP )                                  M31.3188
 2510 II=II-1                                                              13161
      K=KM(II)                                                             13162
      IF ( K.LT.0) GO TO 2520                                              13163
      N8 = K                                                            M25. 864
      DO 2513  M=1, NCOMP                                               M25. 865
      VOLTBC(M) = VOLTBC(M)  -  ZNONL(N8) * YKM(II)                     M31.3189
 2513 N8 = N8 + NTOT                                                    M25. 867
      GO TO 2510                                                           13169
 2520 L=IABS(K)                                                            13170
      N8 = L                                                            M25. 868
      DO 2526  M=1, NCOMP                                               M25. 869
      ZNONL(N8) = ZNONL(N8) + VOLTBC(M)                                 M31.3190
 2526 N8 = N8 + NTOT                                                    M25. 871
      IF ( KODE(L) .EQ. 0 ) GO TO 2500                                  M32.5244
      J = KODE(L)                                                       M32.5245
 2528 N8 = J                                                            M32.5246
      N9 = L                                                            M25. 873
      DO 2531  M=1, NCOMP                                               M25. 874
      ZNONL(N8) = ZNONL(N9)                                             M25. 875
      N8 = N8 + NTOT                                                    M25. 876
 2531 N9 = N9 + NTOT                                                    M25. 877
      IF ( KODE(J) .EQ. L ) GO TO 2500                                  M32.5248
      J = KODE(J)                                                       M32.5249
      GO TO 2528                                                        M32.5250
C             FIND DIFFERENCES OF COLUMNS OF INVERSE FOR NONLINEARITIES    13185
 2550 IF (  INONL .LE. NUM99 )  GO TO 2616                                 13186
      DO 2600  I=1, INONL                                                  13187
      IF (  NLTYPE(I) .LT. 0 )  GO TO 2600                                 13188
      IF (  NLTYPE(I) .GT. 920 )  GO TO 2600                            M37.5179
      N1=NONLK(I)                                                          13189
      N2=IABS(NONLM(I))                                                    13190
      A=ZNONL(N1)-ZNONL(N2)                                                13191
      IF ( NLTYPE(I)  .EQ.  94 )   GO TO 2561                           M18. 816
      IF ( NONLE(I)  .GT.  0 )   GO TO 2561                             M14.2248
 2557 A = A * DELTA2                                                    M14.2249
 2561 ANONL(I) = A                                                      M14.2250
 2600 CONTINUE                                                             13194
 2616 IF (  IPRSUP .GE. 2 )                                             M33.1511
     1 WRITE(LUNIT6, 2603)  ( ZNONL(I), I=1, J11 )                      M37.5180
 2603 FORMAT ( /,  26H ZNONL AFTER SOLN, AT 2318  ,/,                      13197
     1       ( 1X, 8E15.5 ) )                                           M43.2268
      CALL MOVER ( FINIT(1), F(1), NTOT )                               M32.5258
      IF ( KSWTCH .EQ. 0 ) GO TO 5000                                   M39.2830
      DO 8801  I=1, KSWTCH                                              M39.2831
      K1 = KDEPSW( LSWTCH + I )                                         M39.2832
      IF ( K1.NE.8888 .AND.                                             M39.2833
     1     K1.NE.8890 .AND. K1.NE.8891 ) GO TO 8801                     M43.2269
      MK = - ADELAY(I)                                                  M39.2834
      IF ( MK .LE. 0 ) GO TO 8801                                       M39.2835
      M = IABS( KPOS(I) )                                               M39.2836
      IF ( M .NE. 5 ) GO TO 8801                                        M39.2837
      NDX2 = LSWTCH + LSWTCH + I                                        M39.2838
      IF ( ARDUBE(NDX2) .NE. 9999. ) GO TO 8801                         M39.2839
      N1 = KMSWIT(I)                                                    M39.2840
      N2 = KMSWIT( LSWTCH + I )                                         M39.2841
      BI = EXP( (A8SW(MK+5) - T - DELTAT) / A8SW(MK+4) )                M39.2842
      AI = A8SW(MK+3) * BI                                              M39.2843
      F(N1) = AI                                                        M39.2844
      F(N2) = -AI                                                       M39.2845
      IF ( AI .LE. A8SW(MK+7) ) ARDUBE(NDX2) = 0.0                      M39.2846
      IF ( BI .LE. 0.0001                                               M39.2847
     1    .AND. A8SW(MK+7) .EQ. 0.) ARDUBE(NDX2) = 0.                   M43.2270
 8801 CONTINUE                                                          M39.2848
C              CALL TO SOLVUM IF NO COMPENSATION OF UM POWER CIRCUITS : M33.1513
 5000 IF (LOOPSS(8) .EQ. 3)                                             M33.1514
     1 CALL SOLVUM ( SPUM(IUREAC),                                      M33.1515
     1 SPUM(IUGPAR), SPUM(IUFPAR), SPUM(IUHIST), SPUM(IUUMRP),          M33.1516
     2 ISPUM(IUNOD1), ISPUM(IUNOD2), ISPUM(IUJCLT), ISPUM(IUJCLO),      M33.1517
     3 ISPUM(IUJTYP), ISPUM(IUNODO), ISPUM(IUJTMT), SPUM(IUHISM),       M33.1518
     4 SPUM(IUOMGM), SPUM(IUOMLD), SPUM(IUTHAM), SPUM(IUREDU),          M33.1519
     5 SPUM(IUREDS), SPUM(IUFLDS), SPUM(IUFLDR), SPUM(IUREQU),          M33.1520
     6 SPUM(IUFLQS), SPUM(IUFLQR), ISPUM(IUJCDS), ISPUM(IUJCQS),        M33.1521
     7 SPUM(IUFLXD), SPUM(IUFLXQ), ISPUM(IUNPPA), SPUM(IUROTM),         M33.1522
     8 ISPUM(IUNCLD), ISPUM(IUNCLQ), ISPUM(IUJTQO), ISPUM(IUJOMO),      M33.1523
     9 ISPUM(IUJTHO), SPUM(IUREQS), SPUM(IUEPSO), SPUM(IUDCOE),         M33.1524
     9 ISPUM(IUKCOI), SPUM(IUVOLT), SPUM(IUANGL), ISPUM(IUNODF),        M33.1525
     9 ISPUM(IUNODM), ISPUM(IUKUMO), ISPUM(IUJUMO), SPUM(IUUMOU))       M33.1526
      IF ( KILL  .GT.  0 )  GO TO 9200                                  M33.1527
      IF ( IPRSUP .GT. 2 )                                                 13200
     1 WRITE (LUNIT6, 3992)  ( E(I), F(I), I=1, NTOT )                  M17. 868
 3992 FORMAT ( /,  79H (E(I), F(I), I=1, NTOT)   AT THE END OF THE FIRST   13202
     1 PIECE OF THE TIME-STEP LOOP.   ,/,                                  13203
     2       ( 1X, 10E13.3 ) )                                             13204
      IPOINT=IPOINT+1                                                      13205
      IF ( IPOINT.EQ.LPAST) IPOINT=0                                       13206
      LASTOV = NCHAIN                                                   M15.1191
      NCHAIN = 17                                                          13207
      GO TO 9900                                                        M33.1528
 7755 KILL = 229                                                        M37.5181
      LSTAT(19) = 7755                                                  M42.6713
      LSTAT(15) = NCOMP                                                 M37.5182
      LSTAT(16) = LCOMP                                                 M37.5183
      GO TO 9200                                                        M37.5184
 7766 N15  = ISUBEG(KNODE)                                              M37.5185
      NN15 = ISUBEG(MNODE)                                              M37.5186
      LSTAT(19) = 7766                                                  M42.6714
 7777 KILL = 229                                                        M37.5187
      LSTAT(19) = 7777                                                  M42.6715
      LSTAT(15) = NSUBKM (  N15 + 3 )                                   M37.5188
      LSTAT(16) = NSUBKM ( NN15 + 3 )                                   M37.5189
 9200 LSTAT(18) = 16                                                       13215
      LASTOV = NCHAIN                                                   M15.1192
      NCHAIN = 51                                                          13216
 9900 IF ( IPRSUP .GE. 1 )  WRITE (LUNIT6, 9903) KILL, LSTAT(19)        M33.1529
 9903 FORMAT (  34H EXIT "SUBTS1".  KILL, LSTAT(19) =, 2I8 )            M33.1530
99999 RETURN                                                               13219
      END                                                                  13220
      SUBROUTINE YSERLC                                                 M35.1557
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE OF INTERACTIVE EMTP ONLY, ASSOCIATED WITH "EMTSPY".        M36.1486
C     NON-INTERACTIVE VERSIONS CAN REPLACE BY A DUMMY MODULE.           M36.1487
C     THIS MODULE IS CALLED ONLY BY "SUBTS1" OF OVERLAY 16.             M36.1488
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M36.1489
      DATA  IXCOPT  / 0 /                                               M36.1490
      DATA  XCON  / 0.0 /                                               M36.1491
      DATA  CCON  / 0.0 /                                               M36.1492
      IF ( IPRSUP .GE. 4 )                                              M35.1561
     1 WRITE (LUNIT6, 8234)  LSERLC, KSERLC, KPARTB, IALTER             M35.1562
 8234 FORMAT ( ' TOP OF "YSERLC".  LSERLC, KSERLC KPARTB, IALTER =',    M35.1563
     1         4I8 )                                                    M35.1564
      IF ( LSERLC .LE. 0 )  GO TO 9000                                  M35.1565
      IF ( KSERLC .LE. 0 )  GO TO 9000                                  M35.1566
      IF( IXCOPT .GT. 0 )  GO  TO  3000                                 M36.1493
      CCON = 1.0 / ( DELTAT * 500000. )                                 M36.1494
      XCON = CCON * 1000.                                               M36.1495
      IF( XOPT .GT. 0.0 )                                               M36.1496
     1 XCON = XCON * 1000.0 / ( TWOPI * XOPT )                          M36.1497
      IF( COPT .GT. 0.0 )                                               M36.1498
     1 CCON = CCON / ( TWOPI * COPT )                                   M36.1499
      IXCOPT = 1                                                        M36.1500
 3000 DO 8276  J=1, LSERLC                                                      
      NDX1 = IBR + J                                                            
      IF ( KBUS(NDX1) .EQ. 0 )  GO TO 8276                                      
      IF ( NR(NDX1)   .EQ. 0 )  GO TO 8276                                      
      NR(NDX1) = 0                                                              
      NDX2 = IT + J                                                     M35.1571
      N1 = LITYPE(NDX1)                                                 M36.1502
      N1 = NR( N1 )                                                     M36.1503
      N1 = IABS( N1 )                                                           
      D23 = 1.0 / X( N1 )                                               M36.1505
      D44 = TR( NDX2 ) + XCON * TX( NDX2 )                              M36.1506
      GUS1 = C( NDX2 )                                                  M36.1507
      IF( GUS1 .NE. 0.0 )  GUS1 = 1.0 / ( GUS1 * CCON )                 M36.1508
      D33 = D44 + GUS1                                                  M36.1509
      IF ( D33 .GE. EPSILN*D23*10. )  GO TO 8253                        M36.1510
      WRITE (MUNIT6, 8239)  J, D33, D23, EPSILN                         M36.1511
 8239 FORMAT ( '  ZERO-DIVIDE TROUBLE.  EPSILN FAILURE.'  ,/,           M36.1512
     1         '  J, D33, D23, EPSILN =',  I8, 3E15.6       ,/,         M36.1513
     2         '  SEND REVISED DESIRED R,X,C-VALUES :'  )               M36.1514
      READ (MUNIT5, 8246)  BUFF77                                       M35.1572
 8246 FORMAT ( A80 )                                                    M35.1573
      CALL FREFP3 ( BUFF77,TR(NDX2),TX(NDX2),C(NDX2) )                          
      D44 = TR( NDX2 ) + XCON * TX( NDX2 )                              M36.1516
      GUS1 = C(NDX2)                                                    M36.1517
      IF( GUS1 .NE. 0.0 )   GUS1 = 1.0 / ( GUS1 * CCON )                M36.1518
      D33 = D44 + GUS1                                                  M36.1519
 8253 D23 = 1.0/D33  -  X( N1 )                                                 
      CI(NDX1) = TR(NDX2)                                                       
      CK(NDX1) = TX(NDX2)                                                       
      CIK(NDX1) = C(NDX2)                                                       
      X(N1) = D44                                                       M36.1524
      GUS2 = 1.0 / D33                                                  M36.1525
      R(N1) = GUS2 * (CI( NDX1 ) * 2.0 - X(N1) + GUS1)                  M36.1526
      X(N1) = GUS2                                                      M35.1574
      C( N1 ) = GUS1                                                    M36.1527
      IF ( KBUS(NDX1)  .LT.  MBUS(NDX1) )   GO TO 4107                  M35.1575
      N8 = MBUS(NDX1)                                                   M35.1576
      N9 = KBUS(NDX1)                                                   M35.1577
      GO TO 4114                                                        M35.1578
 4107 N8 = KBUS(NDX1)                                                   M35.1579
      N9 = MBUS(NDX1)                                                   M35.1580
 4114 IF ( IPRSUP .GE. 3 )                                              M35.1581
     1 WRITE (LUNIT6, 4116)  J, N8, N9, D23                             M35.1582
 4116 FORMAT ( ' NEXT [Y] CHANGE.  J, N8, N9, D23 =', 3I8, E14.5 )      M35.1583
      IF( N8 .EQ. 1 )  GO TO  5231                                              
      IF ( N8 .GT. KPARTB ) GO TO 8276                                          
      N3 = KKS(N8)                                                      M35.1586
 5217 N3 = N3 - 1                                                       M35.1587
      N4 = IABS( KM(N3) )                                               M35.1588
      IF( N4 .NE. N8 )  GO TO  5224                                     M35.1589
      YKM(N3) = YKM(N3) + D23                                           M35.1590
      GO TO  5217                                                       M35.1591
 5224 IF( N4 .NE. N9 )  GO TO  5217                                     M35.1592
      YKM(N3) = YKM(N3) - D23                                           M35.1593
      GO TO  5238                                                       M35.1594
 5231 IF( N9 .GT. KPARTB )  GO TO  8276                                 M35.1595
 5238 IF( N9 .GT. KPARTB )  GO TO  5252                                 M35.1596
      N1 = KKS(N9)                                                      M35.1597
 5244 N1 = N1 - 1                                                       M35.1598
      N2 = IABS( KM(N1) )                                               M35.1599
      IF( N2 .NE. N9 )  GO TO  5247                                     M35.1600
      YKM(N1) = YKM(N1) + D23                                           M35.1601
      GO TO  5252                                                       M35.1602
 5247 IF( N2 .NE. N8 )  GO TO  5244                                     M35.1603
      YKM(N1) = YKM(N1) - D23                                           M35.1604
      GO TO  5244                                                       M35.1605
 5252 IF ( IALTER .EQ. 0 )  IALTER = 1                                  M35.1606
 8276 CONTINUE                                                                  
 9000 RETURN                                                            M35.1608
      END                                                               M35.1609
      SUBROUTINE SWITCH                                                 M32.5259
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      EQUIVALENCE ( KTRLSW(1), N20 )                                            
C     OVERLAY-16 MODULE CALLED BY "SUBTS1" IF AND ONLY IF ONE OR MORE   M32.5263
C     SWITCHES HAS JUST CHANGED STATUS (IF KTRLSW(1) .GT. 0).   THE     M32.5264
C     PURPOSE IS TO UPDATE NEXTSW VECTOR THAT GIVES ORDER OF SWITCH     M32.5265
C     CURRENT CALCULATION.   GENERAL SOPHISTICATED LOGIC USES THE       M32.5266
C     FOLLOWING STORAGE:                                                M32.5267
C       MODSWT(LL) = J  MEANS THAT THE LL-TH SWITCHING OPERATION OF     M32.5268
C                    THIS TIME STEP INVOLVES SWITCH NUMBER J.   IF      M32.5269
C                    CONTENT (J) IS POSITIVE,  A NOW-OPEN SWITCH IS     M32.5270
C                    TO BE CLOSED;  IF CONTENT IS NEGATIVE (-J), THEN   M32.5271
C                    THE NOW-CLOSED SWITCH J IS TO BE OPENED.           M32.5272
C       NEXTSW(J) = K  MEANS THAT CURRENT IS TO BE CALCULATED IN SWITCH M32.5273
C                   NUMBER K AFTER THAT OF SWITCH NUMBER J.  ADDED MINUSM32.5274
C                   SIGN INDICATES KCL AT 2ND NODE RATHER THAN 1ST ONE. M32.5275
C                   POINTER IS CIRCULAR (NEVER-ENDING).                 M32.5276
C       LASTSW(K) = J  IS THE BACKWARD MAP (LIKE NEXTSW ABOVE,  ONLY IN M32.5277
C                   REVERSE ORDER.   IT, TOO, IS CIRCULAR.   ADDED MINUSM32.5278
C                   SIGN MARKS LAST SWITCH OF SWITCH GROUP.             M32.5279
C       KENTNB(I) = N  MEANS THAT SWITCHES (OPEN OR CLOSED) ADJACENT TO M32.5280
C                   SWITCH I CAN BE FOUND IN NBHDSW(N) ONWARD.  THE LASTM32.5281
C                   ADJACENT ONE IS IN CELL  KENTNB(I+1)-1 .            M32.5282
C       NBHDSW(J) --- STORAGE FOR ALL SWITCHES ADJACENT TO ANY OTHER    M32.5283
C                     SWITCHES.   KENTNB  LOCATES DESIRED ENTRIES.   IF M32.5284
C                     NO SWITCH IS ADJACENT,  THERE SHALL BE NO SUCH    M32.5285
C                     STORAGE (ENTRY POINT EXCEEDS TERMINAL POINT).     M32.5286
C       KBEGSW(J) = 0  MEANS THAT SWITCH J BEGINS A SWITCH GROUP.   IF  M32.5287
C                   CLOSED BUT NOT 1ST OF GROUP, THEN NONZERO STORED.   M32.5288
C       KODE(K)  =  M   MEANS THAT SWITCH NODES K AND M ARE SHORTED     M32.5289
C                   TOGETHER AS PART OF A SWITCH GROUP,  AND THAT WITHINM32.5290
C                   THE SWITCH GROUP,  M  IS THE NEXT HIGHER NODE AFTER M32.5291
C                   BUT POINTER CHAIN IS CIRCULAR,  SO THE EXCEPTION IS M32.5292
C                   THAT THE HIGHEST NODE POINTS TO THE LOWEST.         M32.5293
C       KTRLSW(6) --- FIXED COMMUNICATION VECTOR OF /BLANK/.   CELLS    M32.5294
C                     HAVE THE FOLLOWING MEANING:                       M32.5295
C                   (1) = NUMBER OF SWITCH CHANGES THIS TIME-STEP;      M32.5296
C                   (2) = CURRENT NUMBER OF CLOSED SWITCHES;            M32.5297
C                   (3) = CURRENT NUMBER OF TRIANGULARIZATIONS OF (Y);  M32.5298
C                   (4) = SWITCH NUMBER OF 1ST SWITCH OF 1ST GROUP;     M32.5299
C                   (5) = TOTAL NUMBER SWITCH STATUS CHANGES OF SOLUTIONM32.5300
C                   (6) = 0 OR 1 DEPENDING UPON WHETHER THE             M32.5301
C                         COMPLICATED OR SIMPLE LOGIC IN "SWITCH" IS    M32.5302
C                         TO BE USED.   SEE "SIMPLE SWITCH LOGIC" OF    M32.5303
C                         EMTP RULE BOOK (TO GET VALUE OF UNITY).       M32.5304
C     FOR CASES WITH FEW SWITCHES,  THIS IS A CASE OF OVERKILL,  AND    M32.5305
C     A USER WHO IS SHORT OF MEMORY MIGHT WANT TO ELIMINATE MOST OF     M32.5306
C     THIS SUBROUTINE.  SUCH IS POSSIBLE (SEE ABOVE S.N. 3459).         M32.5307
      IF ( IPRSUP  .GE.  1 )                                            M32.5308
     1 WRITE (LUNIT6, 3444)  KTRLSW                                     M32.5309
 3444 FORMAT ( 34H TOP OF "SWITCH".  KTRLSW VECTOR =,  10I7 )           M32.5310
      IF ( IPRSUP  .GE.  2 )                                            M32.5311
     1 WRITE (LUNIT6, 3447)  ( MODSWT(J), J=1, N20 )                    M32.5312
 3447 FORMAT (39H (MODSWT(J), J=1, KTRLSW(1)) FOLLOW ...,/, (1X, 20I6)) M32.5313
      IF ( IPRSUP  .LT.  3 )   GO TO 3459                               M32.5314
      WRITE (LUNIT6, 3451)  ( NEXTSW(I), I=1, KSWTCH )                  M32.5315
 3451 FORMAT ( 39H (NEXTSW(I), I=1, KSWTCH)  FOLLOWS ....  ,/,          M32.5316
     1  ( 1X, 20I5 ) )                                                  M32.5317
      WRITE (LUNIT6, 3452)  ( LASTSW(I), I=1, KSWTCH )                  M32.5318
 3452 FORMAT ( 39H (LASTSW(I), I=1, KSWTCH)  FOLLOWS ....  ,/,          M32.5319
     1  ( 1X, 20I5 ) )                                                  M32.5320
      WRITE (LUNIT6, 3453)  ( KBEGSW(I), I=1, KSWTCH )                  M32.5321
 3453 FORMAT ( 39H (KBEGSW(I), I=1, KSWTCH)  FOLLOWS ....  ,/,          M32.5322
     1  ( 1X, 20I5 ) )                                                  M32.5323
      WRITE (LUNIT6, 3454)  ( KODE(I), I=1, NTOT )                      M32.5324
 3454 FORMAT ( 36H (KODE(I), I=1, NTOT)  FOLLOWS  ....    ,/,           M32.5325
     1   ( 1X, 20I5 ) )                                                 M32.5326
C     CHOICE OF ALTERNATE SWITCH LOGIC IS NOW TO BE MADE.               M32.5327
C     IMMEDIATELY FOLLOWING IS SIMPLE LOGIC (MUCH SHORTER CODE),        M32.5328
C     WHICH IS FINE FOR CASES WITH FEW SWITCHES.   THOSE SHORT          M32.5329
C     OF MEMORY COULD DELETE ALL BUT THIS (DELETE BETWEEN               M32.5330
C     BOUNDING COMMENT CARDS WITH "%%%%%%").    IF THIS IS              M32.5331
C     DONE,  THEN ALSO DELETE FOLLOWING "IF"-STATEMENT:                 M32.5332
 3459 KTRLSW(5) = KTRLSW(5) + KTRLSW(1)                                         
      IF ( IPRSUP  .GE.  1 )                                            M32.5334
     1 WRITE (LUNIT6, 1004)  ISTEP, KTRLSW                              M32.5335
 1004 FORMAT ( 39H TOP OF "SWITCH".  ISTEP, KTRLSW(1:6) =, 10I6 )       M32.5336
      IF ( KTRLSW(6) .EQ. 0 )  GO TO 3605                                       
C     BEGIN SIMPLE, BRUTE-FORCE SWITCH LOGIC OF "SIMPLE SWITCH LOGIC"   M32.5338
C     SPECIAL-REQUEST CARD (SEE EMTP RULE BOOK):                        M32.5339
      CALL MOVE0 ( KBEGSW(1), KSWTCH )                                          
      CALL MOVE0 ( KODE(1), NTOT )                                              
      J = KTRLSW(4)                                                             
      IF ( J .LE. 0 )  GO TO 3478                                               
C     ENTER LOOP AROUND NEXTSW, AS WE NOTE CLOSED SWITCHES USING KBEGSW:M32.5344
 3473 KBEGSW(J) = 1                                                             
      J = IABS ( NEXTSW(J) )                                                    
      IF ( J .NE. KTRLSW(4) )  GO TO 3473                                       
 3478 CALL MOVE0 ( NEXTSW(1), KSWTCH )                                          
      N18 = KTRLSW(2)                                                   M32.5349
      DO 3485  LL=1, N20                                                        
      J = MODSWT(LL)                                                            
      IF ( J .GT. 0 )  GO TO 3482                                               
      J = -J                                                                    
      KBEGSW(J) = 0                                                             
      N18 = N18 - 1                                                             
      GO TO 3485                                                                
 3482 KBEGSW(J) = 1                                                             
      N18 = N18 + 1                                                             
 3485 CONTINUE                                                                  
      KTRLSW(2) = N18                                                           
      IF ( N18 .GT. 0 ) GO TO 3487                                              
      KTRLSW(4) = 0                                                             
      CALL MOVE0( NEXTSW(1), KSWTCH )                                           
      GO TO 4457                                                                
 3487 N13 = 0                                                                   
      N3 = 0                                                                    
C     ENTER LOOP  DO 3580  OVER ALL SWITCHES,  IN WHICH EACH IN TURN    M32.5367
C     IS TESTED TO SEE IF IT CAN BE USED FOR NEXT SWITCH-CURRENT        M32.5368
C     CALCULATION.   WE EXECUTE THIS LOOP UNTIL N13 = N18 (UNTIL ALL    M32.5369
C     CLOSED SWITCHES HAVE BEEN PROPERLY ORDERED IN NEXTSW:             M32.5370
 3488 DO 3580  I=1, KSWTCH                                                      
      IF ( KBEGSW(I) .EQ. 0 )  GO TO 3580                                       
      IF ( NEXTSW(I) .NE. 0 )  GO TO 3580                                       
      K = KMSWIT(I)                                                             
      NDX1 = LSWTCH + I                                                         
      M = KMSWIT(NDX1)                                                          
      N1 = KENTNB(I)                                                            
      N2 = KENTNB(I+1) - 1                                                      
      JJ = 0                                                                    
      MM = 0                                                                    
      N9 = 0                                                                    
      IF ( K .NE. 1   .AND.                                                     
     1     K .LE. KPARTB )                                                      
     2  GO TO 3491                                                              
C     KNOWN-VOLTAGE NODE K MUST BE DROPPED FROM CONSIDERATION:          M32.5385
      JJ = 1                                                                    
      N9 = N9 + 1                                                               
 3491 IF ( M .NE. 1   .AND.                                                     
     1     M .LE. KPARTB )                                                      
     2  GO TO 3493                                                              
C     KNOWN-VOLTAGE NODE M MUST BE DROPPED FROM CONSIDERATION:          M32.5391
      MM = 1                                                                    
      N9 = N9 + 1                                                               
 3493 IF ( N1 .GT. N2 )  GO TO 3506                                             
      DO 3503  LL=N1, N2                                                        
      J = NBHDSW(LL)                                                            
      IF ( KBEGSW(J) .EQ. 0 )  GO TO 3503                                       
      IF ( NEXTSW(J) .NE. 0 )  GO TO 3503                                       
      NDX2 = LSWTCH + J                                                         
      IF ( JJ .GT. 0 )  GO TO 3497                                              
      IF ( K .NE. KMSWIT(J)   .AND.                                             
     1     K .NE. KMSWIT(NDX2) )                                                
     2  GO TO 3497                                                              
      JJ = 1                                                                    
      GO TO 3501                                                                
 3497 IF ( MM .GT. 0 )  GO TO 3503                                              
      IF ( M .NE. KMSWIT(J)  .AND.                                              
     1     M .NE. KMSWIT(NDX2) )                                                
     2  GO TO 3503                                                              
      MM = 1                                                                    
 3501 N9 = N9 + 1                                                               
      IF ( N9 .EQ. 2 )  GO TO 3580                                              
 3503 CONTINUE                                                                  
C     WE DROP OUT OF ABOVE  DO 3503  LOOP ONLY IF ONE SIDE OF SWITCH #I M32.5414
C     CAN BE USED FOR KCL CALCULATION OF SWITCH CURRENT:                M32.5415
 3506 IF ( N13 .GT. 0 )                                                         
     1 NEXTSW(N17) = I * NEXTSW(N17)                                            
      N17 = I                                                                   
      IF ( JJ .GT. 0 )  GO TO 3511                                              
      NEXTSW(I) = 1                                                             
      GO TO 3518                                                                
 3511 NEXTSW(I) = -1                                                            
 3518 N13 = N13 + 1                                                             
      IF ( N13 .EQ. 1 )  N14 = I                                                
      IF ( KODE(K) .NE. 0 )  GO TO 3528                                         
      IF ( KODE(M) .NE. 0 )  GO TO 3528                                         
C     SWITCH NOW CLOSING IS CONNECTED TO NO OTHER CLOSED SWITCHES, SO   M32.5427
C     EACH END POINTS TO THE OTHER (TRIVIAL KODE CHANGE):               M32.5428
      KODE(K) = M                                                       M32.5429
      KODE(M) = K                                                       M32.5430
      GO TO 3563                                                                
 3528 N6 = K                                                                    
      IF ( KODE(K) .EQ. 0 )  N6 = M                                             
      N7 = K                                                                    
      IF ( N7 .EQ. N6 ) N7 = M                                                  
      N19 = N7                                                                  
C     N6 WAS JUST ONE NODE OF CIRCULAR CHAIN;  MOVE TILL HIGHEST NODE:  M32.5437
 3532 IF ( KODE(N6) .LT. N6 )  GO TO 3536                                       
      N6 = KODE(N6)                                                             
      GO TO 3532                                                                
 3536 N15 = KODE(N7)                                                            
      IF ( N7 .LE. N6   .AND.                                                   
     1     N7 .GE. KODE(N6) )                                                   
     2  GO TO 3541                                                              
C     INSERT NODE N7 AS NEW MAX OR NEW MIN:                             M32.5445
      N16 = KODE(N6)                                                            
      KODE(N6) = N7                                                             
      KODE(N7) = N16                                                            
      IF ( N7 .GT. N6 )  N6 = N7                                                
      GO TO 3556                                                                
C     NON-EXTREMA INSERTION.  1ST FIND INSERTION POINT:                 M32.5451
 3541 N9 = KODE(N6)                                                             
 3544 IF ( N7 .LT. KODE(N9) )  GO TO 3548                                       
      N9 = KODE(N9)                                                             
      GO TO 3544                                                                
C     NOW WE INSERT N7 AFTER N9 BUT BEFORE KODE(N9):                    M32.5456
 3548 N16 = KODE(N9)                                                            
      KODE(N9) = N7                                                             
      KODE(N7) = N16                                                            
C     WE REACH 3556 BELOW AFTER NODE N7 HAS BEEN INSERTED IN KODE:      M32.5460
 3556 IF ( N15 .EQ. 0 )  GO TO 3563                                             
      IF ( N15 .EQ. N19 ) GO TO 3563                                            
C     N7-END WAS A CHAIN (NOT ISOLATED NODE), AND WE'RE NOT YET DONE:   M32.5463
      N7 = N15                                                                  
      GO TO 3536                                                                
C     WE REACH 3563 WITH KODE CORRECTLY MODIFIED TO REFLECT ADDED SWITCHM32.5466
 3563 IF ( IPRSUP  .GE.  3 )                                            M32.5467
     1 WRITE (LUNIT6, 3564)  I                                          M32.5468
 3564 FORMAT ( 30H COMPLETE PROCESSING OF SWITCH,  I5  )                M32.5469
 3580 CONTINUE                                                                  
      IF ( IPRSUP  .GE.  1 )                                            M32.5471
     1 WRITE (LUNIT6, 3584)  N13, N17                                   M32.5472
 3584 FORMAT ( 47H DONE ANOTHER PASS OF ALL SWITCHES.  N13, N17 =,      M32.5473
     1  2I5  )                                                          M32.5474
      IF ( N13 .GT. N3 )  GO TO 3591                                            
      WRITE (LUNIT6, 3587)                                              M32.5476
 3587 FORMAT ( 47H TEMPORARY ERROR STOP IN "SWITCH" AT S.N. 3587.  )    M32.5477
      CALL STOPTP                                                               
 3591 N3 = N13                                                                  
      IF ( N13 .LT. N18 )  GO TO 3488                                           
      KTRLSW(4) = N14                                                           
C     WE ARE DONE,  HAVING ORDERED ALL N18 OF THE CLOSED SWITCHES.      M32.5482
C     NOW ONLY THE CIRCULAR END OF NEXTSW MUST BE PATCHED ON:           M32.5483
      NEXTSW(N17) = NEXTSW(N17) * N14                                           
      GO TO 4457                                                                
C     %%%%%%%%%%%%%%%%%%%  BEGIN DELETABLE SOPHISTICATED LOGIC  %%%%%%%%M32.5486
C     %%%%% NOTE: "LASTSW" OF DECK "LABCOM" CAN BE DELETED, TOO,        M32.5487
C     %%%%%       ALONG WITH THIS BLOCK OF SOPHISTICATED CODE.          M32.5488
C     BEGIN BY OPENING ALL SWITCHES SO ORDERED (NEGATIVE MODSWT):       M32.5489
 3605 DO 4100  LL=1, N20                                                        
      IF ( MODSWT(LL) .GT. 0 )                                                  
     1 GO TO 4100                                                               
      N17 = -MODSWT(LL)                                                         
      K = KMSWIT(N17)                                                           
      NDX1 = LSWTCH + N17                                                       
      M = KMSWIT(NDX1)                                                          
      N3 = IABS ( LASTSW(N17) )                                                 
      N4 = IABS ( NEXTSW(N17) )                                                 
C     FIRST TEST WHETHER SWITCH N17 CONSTITUTES A GROUP ALL BY ITSELF:  M32.5499
      IF ( LASTSW(N17) .GT. 0  .OR.                                             
     1     KBEGSW(N17) .NE. 0 )                                                 
     2 GO TO 3621                                                               
C     BEGIN SIMPLIFIED LOGIC FOR ISOLATED SWITCH N17 WHICH NOW OPENS:   M32.5503
      KODE(K) = 0                                                               
      KODE(M) = 0                                                               
      GO TO 3664                                                                
C     BEGIN LOGIC FOR GROUP OF 2 OR MORE SWITCHES.   IMPORTANT SPECIAL  M32.5507
C     CASE HERE INVOLVES TEST FOR WHETHER SWITCH N17 IS AT THE END OF   M32.5508
C     A RADIAL.   LOOK FOR SUCH FINGER-SWITCHES FIRST:                  M32.5509
 3621 N1 = KENTNB(N17)                                                          
      N2 = KENTNB(N17+1) - 1                                                    
      JJ = 0                                                                    
      MM = 0                                                                    
      N9 = 0                                                                    
      DO 3641  I=N1, N2                                                         
      N7 = NBHDSW(I)                                                            
      IF ( NEXTSW(N7)  .EQ.  0 )   GO TO 3641                                   
      NDX2 = LSWTCH + N7                                                        
      IF ( JJ  .GT.  0 )   GO TO 3627                                           
      IF ( K .NE. KMSWIT(N7)  .AND.                                             
     1     K .NE. KMSWIT(NDX2) )                                                
     2 GO TO 3627                                                               
      JJ = 1                                                                    
      N9 = N9 + 1                                                               
      GO TO 3635                                                                
 3627 IF ( MM .GT. 0 )  GO TO 3641                                              
      IF ( M .NE. KMSWIT(N7)  .AND.                                             
     1     M .NE. KMSWIT(NDX2) )                                                
     2 GO TO 3641                                                               
      MM = 1                                                                    
      N9 = N9 + 1                                                               
 3635 IF ( N9 .EQ. 2 )                                                          
     1 GO TO 3750                                                               
 3641 CONTINUE                                                                  
C     WE FALL OUT BOTTOM OF  DO 3641  ONLY IF SWITCH N17 IS AT END OF   M32.5535
C     RADIAL.   THIS IS FINGER-SWITCH, WITH SIMPLE LOGIC.    BEGIN      M32.5536
C     BY FINDING NODE N16 WHICH IS OUT AT END OF RADIAL:                M32.5537
      N16 = K                                                                   
      IF ( JJ .GT. 0 )  N16 = M                                                 
C     NEXT EXTRACT NODE N16 (ISOLATED END OF FINGER-SWITCH) FROM KODE:  M32.5540
      N8 = N16                                                                  
 3652 N9 = KODE(N8)                                                             
      IF ( N9 .EQ. N16 )  GO TO 3658                                            
      N8 = N9                                                                   
      GO TO 3652                                                                
 3658 KODE(N8) = KODE(N16)                                                      
      KODE(N16) = 0                                                             
C     REPAIR OF "-" FLAG ON LASTSW THAT MARKS END OF GROUP, IF NEEDED:  M32.5548
 3664 IF ( LASTSW(N17) .LT. 0 )                                                 
     1 LASTSW(N3) = -IABS( LASTSW(N3) )                                         
C     NEXT REPAIR ZERO KBEGSW THAT MARKS BEGINNING OF GROUP, IF NEEDED: M32.5551
      IF ( KBEGSW(N17) .EQ. 0 )                                                 
     1 KBEGSW(N4) = 0                                                           
C     NEXT ESTABLISH NEW FORWARD LINKAGE NEXTSW AROUND NOW-OPEN N17:    M32.5554
      N15 = NEXTSW(N3)                                                          
      NEXTSW(N3) = N4                                                           
      IF ( N15 .LT. 0 )                                                         
     1 NEXTSW(N3) = -N4                                                         
C     NEXT ESTABLISH NEW BACKWARD LINKAGE LASTSW AROUND NOW-OPEN N17:   M32.5559
      N15 = LASTSW(N4)                                                          
      LASTSW(N4) = N3                                                           
      IF ( N15 .LT. 0 )                                                         
     1 LASTSW(N4) = -N3                                                         
      IF ( KTRLSW(4) .EQ. N17 )  KTRLSW(4) = N4                                 
      IF ( KTRLSW(4) .EQ. N17 )  KTRLSW(4) = 0                                  
      GO TO 4095                                                                
C     BEGIN LOGIC FOR GROUP-SPLITTING (SWITCH N17 NOT AT END OF RADIAL):M32.5567
 3750 MO = KODE(K)                                                              
       IF ( MO .EQ. 0 ) GO TO 3744                                              
      KODE(K) = 0                                                               
      K = MO                                                                    
      GO TO 3750                                                                
 3744 M1 = IABS( LASTSW(N17) )                                                  
 3746 IF ( KBEGSW(M1) .EQ. 0 ) GO TO 3754                                       
      KBEGSW(M1) = 2                                                            
      M1 = LASTSW(M1)                                                           
      GO TO 3746                                                                
 3754 KBEGSW(M1) = 2                                                            
      L1 = LASTSW(M1)                                                           
      M2 = N17                                                                  
 3756 KBEGSW(M2) = 2                                                            
      IF ( LASTSW(M2) .LT. 0 ) GO TO 3758                                       
      M2 = IABS( NEXTSW(M2) )                                                   
      GO TO 3756                                                                
 3758 L2 = IABS( NEXTSW(M2) )                                                   
      M7 = 1                                                                    
      M3 = M2                                                                   
 3760 IF ( M3 .NE. N17 ) GO TO 3764                                             
      M3 = IABS( LASTSW(M3) )                                                   
 3764 KBEGSW(M3) = 0                                                            
      J1 = KMSWIT(M3)                                                           
      J2 = KMSWIT( M3 + LSWTCH )                                                
      N1 = KENTNB(M3)                                                           
      N2 = KENTNB(M3 + 1) - 1                                                   
      DO 3774 I = N1, N2                                                        
      N7 = NBHDSW(I)                                                            
      IF ( NEXTSW(N7) .EQ. 0 ) GO TO 3774                                       
      IF ( KBEGSW(N7) .EQ. 2 ) GO TO 3774                                       
      K1 = KMSWIT(N7)                                                           
      K2 = KMSWIT( N7 + LSWTCH )                                                
      JJ = J1                                                                   
      IF (J1 .EQ. K1 .OR. J1 .EQ. K2)  JJ = J2                                  
      MM = J1                                                                   
      IF ( MM .EQ. JJ )  MM = J2                                                
      GO TO  3788                                                               
 3774 CONTINUE                                                                  
C     PROCESSING SWITCHES WHICH ARE NO PROCESSED SWITCH CONNECTED       M32.5607
      KODE(J1) = J2                                                     M32.5608
      KODE(J2) = J1                                                     M32.5609
      IF ( M7 .NE. 1 )  GO TO 3776                                              
C     THIS IS IST PROCESSING SWITCH AND THE LAST SWITCH OF GROUP 1      M32.5611
      IF ( NEXTSW(M3) .LT. 0 )  M7 = -1                                         
      NEXTSW(M3) = M3 * M7                                                      
      M4 = LASTSW(M3)                                                           
      LASTSW(M3) = -M3                                                          
      M3 = IABS(M4)                                                             
      M7 = 0                                                                    
      GO TO 3760                                                                
 3776 N7 = 6666                                                                 
C     NOW WE BREAK PROCESSED SWITCH CHAIN AND PUT M3 IN THE CHAIN       M32.5620
      M4 = M2                                                                   
 3778 IF ( KBEGSW(M4) .EQ. 0 )  GO TO 3780                                      
      M4 = IABS( LASTSW(M4) )                                                   
      GO TO 3778                                                                
 3780 IF (N7 .NE. 6666) KBEGSW(M4)=1                                            
      M5 = NEXTSW(M3)                                                           
      NEXTSW(M3) = M4                                                           
      IF ( M5 .LT. 0 )  NEXTSW(M3) = -M4                                        
      M5 = LASTSW(M4)                                                           
      LASTSW(M4) = M3                                                           
      IF( M5 .LT. 0 )  LASTSW(M4) = -M3                                         
      M5 = IABS(M5)                                                             
      M4 = NEXTSW(M5)                                                           
      NEXTSW(M5) = M3                                                           
      IF ( M4 .LT. 0 ) NEXTSW(M5) = -M3                                         
      M6 = LASTSW(M3)                                                           
      LASTSW(M3) = M5                                                           
      IF ( N7 .EQ. 6666 )  LASTSW(M3) = -M5                                     
      IF ( M3 .EQ. M1 )  GO TO 3795                                             
      M3 = M6                                                                   
      GO TO 3760                                                                
C     M3 CONNECTS TO N7 AND BECOMES THE FIRST SWITCH IN N7 SWITCH GROUP M32.5642
 3788 M4 = N7                                                                   
 3784 NMIN = KODE(MM)                                                           
      IF ( NMIN .LT. MM ) GO TO 3790                                            
      MM = NMIN                                                                 
      GO TO 3784                                                                
C     M3 IS JUST AT THE END OF A RADIAL.  PUT NODE JJ IN THE KODE CHAIN M32.5648
 3790 IF ( JJ .LT. MM  .AND.  JJ .GT. NMIN )  GO TO 3792                M32.5649
C     IN THE "KODE" CHAIN,  NODE JJ IS LARGER THAN MM OR LESS THAN NMIN M32.5650
      KODE(MM) = JJ                                                     M32.5651
      KODE(JJ) = NMIN                                                   M32.5652
      GO TO 3778                                                                
 3792 MM = KODE(NMIN)                                                           
      IF ( MM .GT. JJ )  GO TO 3794                                             
      NMIN = MM                                                                 
      GO TO 3792                                                                
 3794 KODE(NMIN) = JJ                                                           
      KODE(JJ) = MM                                                             
      GO TO 3778                                                                
 3795 IF ( L2 .EQ. M1 )  GO TO 4095                                             
C     WE FOUND OTHER SWITCH GROUP. BREAK JUST PROCESSING SWITCH VETTORS M32.5662
C     AND FORM THE WHOLE SWICH VECTORS.   M1 IS STILL 1ST OF THE GROUP. M32.5663
      M2 = LASTSW(M1)                                                           
      LASTSW(M1) = L1                                                           
      IF( M2 .LT. 0 )  LASTSW(M1) = -L1                                         
      M2 = IABS(M2)                                                     M32.5667
      M1 = NEXTSW(M2)                                                           
      NEXTSW(M2) = L2                                                           
      IF ( M1 .LT. 0 )  NEXTSW(M2) = -L2                                        
      M1 = LASTSW(L2)                                                           
      LASTSW(L2) = M2                                                           
      IF ( M1 .LT. 0 )  LASTSW(L2) = - M2                                       
 4095 NEXTSW(N17) = 0                                                           
      LASTSW(N17) = 0                                                           
      KBEGSW(N17) = 1                                                           
      KTRLSW(2) = KTRLSW(2) - 1                                                 
      IF ( IPRSUP  .GE.  2 )                                            M32.5678
     1 WRITE (LUNIT6, 4096)  LL, N17, K, M                              M32.5679
 4096 FORMAT ( 44H DONE WITH ANOTHER OPENING.  LL, N17, K, M =,  10I5 ) M32.5680
 4100 CONTINUE                                                                  
C     NEXT CLOSE ALL SWITCHES SO ORDERED (POSITIVE MODSWT):             M32.5682
      DO 4450  LL=1, N20                                                        
      IF ( MODSWT(LL) .LT. 0 )  GO TO 4450                                      
      N17 = MODSWT(LL)                                                          
      K = KMSWIT(N17)                                                           
      NDX1 = LSWTCH + N17                                                       
      M = KMSWIT(NDX1)                                                          
      N1 = KENTNB(N17)                                                          
      N2 = KENTNB(N17+1) - 1                                                    
      MM = 0                                                                    
      N4 = 0                                                                    
      IF ( N2 .LT. N1 )  GO TO 4120                                     M32.5693
      DO 4118  I=N1, N2                                                         
      N7 = NBHDSW(I)                                                            
      IF ( NEXTSW(N7)  .EQ.  0 )   GO TO 4118                                   
      NDX2 = LSWTCH + N7                                                        
      IF ( N4  .GT.  0 )   GO TO 4113                                           
      JJ = N7                                                                   
      N5 = K                                                                    
      IF (K .EQ. KMSWIT(JJ)) N5 = M                                             
      IF ( K .EQ. KMSWIT(NDX2) )  N5 = M                                        
      N4 = K                                                                    
      IF ( N4 .EQ. N5 )  N4 = M                                                 
      GO TO 4118                                                                
 4113 IF ( KMSWIT(N7) .NE. N5  .AND.                                            
     1     KMSWIT(NDX2) .NE. N5 )  GO TO 4118                                   
C     WE HAVE FOUND GROUP 2 SWITCH (CONNECTED TO NODE N5):              M32.5708
      MM = N7                                                                   
      GO TO 4126                                                                
 4118 CONTINUE                                                                  
      IF ( N4 .GT. 0 )  GO TO 4300                                              
C     NO CONNECTED SWITCHES WHICH ARE CLOSED IS EASY SPECIAL CASE:      M32.5713
 4120 KODE(K) = M                                                               
      KODE(M) = K                                                       M32.5715
      M1 = KTRLSW(4)                                                            
      KTRLSW(4) = N17                                                           
      KBEGSW(N17) = 0                                                           
      IF ( M1 .NE. 0 )  GO TO 4122                                              
      NEXTSW(N17) = N17                                                         
      IF ( K .GT. KPARTB  .OR.  K .EQ. 1 )  NEXTSW(N17) = -N17          M32.5721
      LASTSW(N17) = -N17                                                        
      GO TO 4130                                                                
C     READY TO CHECK FOR ILLEGAL SWITCH LOOP, AND ALSO VOLTAGE SOURCE:  M32.5724
 4126 N6 = 0                                                                    
      N2 = N4                                                                   
C     ENTER LOOP OVER NODES IN GROUP 1, TO CHECK FOR LOOPS AND SOURCES: M32.5727
 4135 N2 = KODE(N2)                                                             
      IF ( N2 .EQ. 1 )       N6 = 1                                             
      IF ( N2 .GT. KPARTB )  N6 = 1                                             
      IF ( N2  .NE.  N5 )   GO TO 4147                                          
      KILL = 64                                                                 
      LSTAT(19) = 4147                                                          
      BUS1 = BUS(N2)                                                            
      GO TO 4457                                                                
 4147 IF ( N2 .NE. N4 )   GO TO 4135                                            
C     OK, SO NO SWITCH LOOPS.  BUT WE MIGHT HAVE TO SWAP GROUPS 1 AND   M32.5739
C     2 --- REQUIRED IF GROUP 1 HAD A VOLTAGE SOURCE:                   M32.5740
      IF ( N6 .EQ. 0 )  GO TO 4300                                              
C     FIRST, SWAP KNOWN SWITCHES IN GROUPS (JJ AND MM):                 M32.5742
      N12 = MM                                                          M32.5743
      MM = JJ                                                           M32.5744
      JJ = N12                                                          M32.5745
C     NEXT, SWAP NODES N4 AND N5 (GROUP 1 AND GROUP 2 CONNECTIONS):     M32.5746
      N12 = N5                                                          M32.5747
      N5 = N4                                                           M32.5748
      N4 = N12                                                          M32.5749
 4300 NMAX = N4                                                                 
      N3 = N5                                                                   
 4302 NMIN = KODE(NMAX)                                                         
      IF (NMIN .LT. NMAX)  GO TO 4304                                           
      NMAX = NMIN                                                               
      GO TO 4302                                                                
 4304 M5 = KODE(N3)                                                             
      IF ( N3 .LT. NMIN ) GO TO 4310                                            
      IF ( N3 .GT. NMAX ) GO TO 4310                                            
      NLESS = NMIN                                                              
 4306 NOVER = KODE(NLESS)                                                       
      IF ( NOVER .GT. N3 )  GO TO 4308                                          
      NLESS = NOVER                                                             
      GO TO 4306                                                                
 4308 KODE(NLESS) = N3                                                          
      KODE(N3)= NOVER                                                           
      GO TO 4312                                                                
 4310 KODE(NMAX) = N3                                                           
      KODE(N3) = NMIN                                                           
 4312 IF (M5 .EQ. 0) GO TO 4314                                                 
      IF ( M5 .EQ. N5 )  GO TO  4314                                            
      IF ( N3 .LT. NMIN )  NMIN = N3                                            
      IF ( N3 .GT. NMAX )  NMAX = N3                                            
      N3 = M5                                                                   
      GO TO 4304                                                                
 4314 M1 = JJ                                                                   
 4316 IF (KBEGSW(M1) .EQ. 0)  GO TO 4318                                        
      M1 = IABS( LASTSW(M1) )                                                   
      GO TO 4316                                                                
 4318 IF (MM .NE. 0) GO TO 532                                                  
      IF (N5 .GT. KPARTB .OR. N5 .EQ. 1) GO TO 532                              
C     N17 CAN BE 1ST SWITCH OF THE GROUP NOW, SO IT IS EASY TO PROCESS  M32.5781
      KBEGSW(N17) = 0                                                           
      KBEGSW(M1) = 1                                                            
      IF (KTRLSW(4) .EQ. M1) KTRLSW(4) = N17                                    
 4122 M3 = LASTSW(M1)                                                           
      M2 = IABS( M3 )                                                           
      NEXTSW(N17) = M1                                                          
      IF (N4 .EQ. 0)  GO TO 4124                                                
      IF (N5 .EQ. M) NEXTSW(N17) = -M1                                          
      LASTSW(N17) = M2                                                          
      GO TO 4128                                                                
 4124 IF (K .GT. KPARTB .OR. K .EQ. 1) NEXTSW(N17) = -M1                M32.5792
      LASTSW(N17) = -M2                                                         
 4128 LASTSW(M1) = N17                                                          
      IF (M3 .LT. 0) LASTSW(M1) = -N17                                          
      IF (NEXTSW(M2) .LT. 0)  N17 = -N17                                        
      NEXTSW(M2) = N17                                                          
      GO TO 4130                                                                
C     NOW ENTER GENERAL LOGIC TO PROCESS GROUP OF TWO OR MORE SWITCHES  M32.5799
  532 KBEGSW(N17) = 1                                                           
      N3 = M1                                                                   
 4322 N8 = N3                                                                   
      IF (NEXTSW(N3) .LT. 0) N8 = LSWTCH + N3                                   
      IF (KMSWIT(N8) .EQ. N4)  GO TO 4360                                       
      IF ( LASTSW(N3) .LT. 0 )  GO TO  4324                                     
      N3 = IABS( NEXTSW(N3) )                                                   
      GO TO 4322                                                                
C     ALL JJ SWITCHES OF JJ GROUP ARE PROCESSED .   NOW PROCESSING N17  M32.5808
 4324 LASTSW(N3) = -LASTSW(N3)                                                  
      M3 = NEXTSW(N3)                                                           
      M2 = IABS(M3)                                                             
      NEXTSW(N3) = N17                                                          
      IF (M3 .LT. 0)  NEXTSW(N3) = -N17                                         
C     N17 IS IN THE END OF A RADIAL.   WE BEGIN TO PROCESS SWITCH N17.  M32.5814
 4399 IF ( MM .NE. 0 ) GO TO 4330                                               
      LASTSW(N17) = -N3                                                         
      IF (M1 .EQ. M9 .AND. M1 .EQ. M2) M2=KTRLSW(4)                             
 4325 NEXTSW(N17)=M2                                                            
      IF (N4 .EQ. M)  NEXTSW(N17) = -M2                                         
      IF (LASTSW(M2) .LT. 0) N17 = -N17                                         
      LASTSW(M2) = N17                                                          
      GO TO 4130                                                                
C     NOW PROCESSING SWITCH N17  WHICH CONNECTED  ANOTHER SWITCH GROUP  M32.5823
 4330 LASTSW(N17) = N3                                                          
      L1 = MM                                                                   
 4332 IF (KBEGSW(L1) .EQ. 0) GO TO 4334                                         
      L1 = IABS( LASTSW(L1) )                                                   
      GO TO 4332                                                                
 4334 KBEGSW(L1) = 1                                                            
      IF (KTRLSW(4) .EQ. L1) KTRLSW(4) = M1                                     
      IF (M2 .EQ. L1) GO TO 4325                                                
C     A LITTLE TROUBLE.  FIRST EXTRACT 2ND GROUP FROM SWITCH CHAIN AND  M32.5832
C     THEN CONNECT THE THREE SWITCH GROUPS CAREFULLY.                   M32.5833
      L2 = MM                                                                   
 4336 IF (LASTSW(L2) .LT. 0) GO TO 4338                                         
      L2 = IABS( NEXTSW(L2) )                                                   
      GO TO 4336                                                                
 4338 L3 = IABS( LASTSW(L1) )                                                   
      L4 = IABS( NEXTSW(L2) )                                                   
      L5 = L4                                                                   
      IF (NEXTSW(L3) .LT. 0) L5 = -L5                                           
      NEXTSW(L3) = L5                                                           
      IF ( LASTSW(L4) .LT. 0 ) L3 = -L3                                         
      LASTSW(L4) = L3                                                           
      NEXTSW(N17) = L1                                                          
      IF (N4 .EQ. M)  NEXTSW(N17) = -L1                                         
      IF (LASTSW(L1) .LT. 0) N17 = -N17                                         
      LASTSW(L1) = N17                                                          
      L5 = M2                                                                   
      IF (NEXTSW(L2) .LT. 0)  L5 = -L5                                          
      NEXTSW(L2) = L5                                                           
      IF (LASTSW(M2) .LT. 0)  L2 = -L2                                          
      LASTSW(M2) = L2                                                           
      GO TO 4130                                                                
 4360 IF (LASTSW(N3) .GT. 0) GO TO 4364                                         
      NEXTSW(N3) = -NEXTSW(N3)                                                  
      GO TO 4324                                                                
C     PROCESS SWITCHES WHICH NEED CHANGING "NEXTSW" ORDER IN THE GROUP  M32.5858
 4364 NEXTSW(N17) = 1                                                           
      M3 = N3                                                                   
      M9 = M3                                                                   
      NN = 0                                                                    
 4368 KBEGSW(N3) = 2                                                            
      NN = NN + 1                                                               
      IF (LASTSW(N3) .LT. 0) GO TO 4370                                         
      N3 = IABS( NEXTSW(N3) )                                                   
      GO TO 4368                                                                
 4370 M2 = IABS(NEXTSW(N3))                                                     
      NEXTSW(N3)=M3                                                             
      KBEGSW(N17) = 2                                                           
      M4 = LASTSW(M3)                                                           
      KK2 = 0                                                                   
      LL2 = 1                                                                   
      IF ( NEXTSW(M4) .LT. 0 )  LL2 = -1                                        
      IF (M1 .EQ. M3 .AND. M1 .EQ. M2) M4 = 0                                   
      N3 = IABS(NEXTSW(M3))                                                     
 4374 K1 = KMSWIT(N3)                                                           
      K2 = KMSWIT( LSWTCH + N3 )                                                
      N1 = KENTNB(N3)                                                           
      N2 = KENTNB(N3+1)-1                                                       
      J1 = 0                                                                    
      J2 = 0                                                                    
      NUMB = 0                                                                  
      DO 4380 I = N1, N2                                                        
      K7 = NBHDSW(I)                                                            
      IF ( NEXTSW(K7) .EQ. 0 ) GO TO 4380                                       
      IF (KBEGSW(K7) .NE. 2) GO TO 4380                                         
      K3 = KMSWIT(K7)                                                           
      K4 = KMSWIT( LSWTCH + K7 )                                                
      IF ( J1 .EQ. 1 ) GO TO 4372                                               
      IF (K1 .NE. K3 .AND. K1 .NE. K4) GO TO 4372                               
      J1 = 1                                                                    
      NUMB = NUMB+1                                                             
      GO TO 4376                                                                
 4372 IF ( J2 .EQ. 1 ) GO TO 4380                                               
      IF (K2 .NE. K3 .AND. K2 .NE. K4) GO TO 4380                               
      J2 = 1                                                                    
      NUMB = NUMB+1                                                             
 4376 IF (NUMB .NE. 2) GO TO 4380                                               
      M3 = N3                                                                   
      GO TO 4384                                                                
 4380 CONTINUE                                                                  
      KK2 = KK2 + 1                                                             
      IF (M4 .NE. 0) GO TO 4382                                                 
      KTRLSW(4) = N3                                                            
      LASTSW(N3) = 0                                                            
      KBEGSW(N3) = 0                                                            
      GO TO 4383                                                                
 4382 KBEGSW(N3) = 1                                                            
      IF (M9 .EQ. M1 .AND. KK2 .EQ. 1) KBEGSW(N3) = 0                           
      LASTSW(N3) = M4                                                           
      NEXTSW(M4) = N3 * LL2                                                     
      LL2 = 1                                                                   
 4383 IF ( J1 .EQ. 1 )  LL2 = -1                                                
      IF (KK2 .EQ. NN) GO TO 4388                                               
      M4 = N3                                                                   
      NEXTSW(M3) = NEXTSW(N3)                                                   
 4384 N3 = IABS( NEXTSW(N3) )                                                   
      GO TO 4374                                                                
 4388 NEXTSW(N3) = N17*LL2                                                      
      KBEGSW(N17) = 1                                                           
      GO TO 4399                                                                
 4130 KTRLSW(2) = KTRLSW(2) + 1                                                 
      IF ( IPRSUP  .GE.  2 )                                            M32.5924
     1 WRITE (LUNIT6, 4264)  LL, N17, K, M                              M32.5925
 4264 FORMAT ( 44H DONE WITH ANOTHER CLOSURE.  LL, N17, K, M =,  10I5 ) M32.5926
 4450 CONTINUE                                                                  
C     %%%%%%%%%%%%%%%%%%%  END   DELETABLE SOPHISTICATED LOGIC  %%%%%%%%M32.5928
 4457 IF ( IPRSUP  .LT.  2 )   GO TO 4465                               M32.5929
      WRITE (LUNIT6, 3451)  ( NEXTSW(I), I=1, KSWTCH )                  M32.5930
      WRITE (LUNIT6, 3452)  ( LASTSW(I), I=1, KSWTCH )                  M32.5931
      WRITE (LUNIT6, 3453)  ( KBEGSW(I), I=1, KSWTCH )                  M32.5932
      WRITE (LUNIT6, 3454)  ( KODE(I), I=1, NTOT )                      M32.5933
 4465 IF ( IPRSUP  .GE.  1 )                                            M32.5934
     1 WRITE (LUNIT6, 4468)                                             M32.5935
 4468 FORMAT ( 15H EXIT "SWITCH".  )                                    M32.5936
      RETURN                                                            M32.5937
      END                                                               M32.5938
      SUBROUTINE  TACS3                                                 M13.2599
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
C     INTRINSIC  ABSZ, COSZ                                             M32.5939
      IF ( IPRSUP  .GE.  1 )                                            M32.5940
     1 WRITE ( LUNIT6, 4567 )                                           M32.5941
 4567 FORMAT ( 23H  "BEGIN MODULE TACS3." )                             M32.5942
      NIUNRS = IUTY( KIUTY + 1 )                                        M28.5438
      KJSUP = KINSUP + LSTAT(65)                                        M39.2849
      KKSUP = KJSUP  + LSTAT(65)                                        M39.2850
      MA1 = IUTY(KIUTY+7)                                               M42.6716
      MA2 = IUTY(KIUTY+8)                                               M42.6717
      IF ( IPRSUP  .LT.  2 )   GO TO 3219                               M13.2603
      WRITE (LUNIT6, 3213)  IOUTCS, ISPRIN, ISPLOT, LIMSTP, IOUT        M39.2851
 3213 FORMAT (   40H  IOUTCS  ISPRIN  ISPLOT  LIMSTP    IOUT            M39.2852
     2    ,/,  5I8 )                                                    M39.2853
      WRITE (LUNIT6, 3214) KTAB, NIU, NUK, NENERG, IUTY(KIUTY+11)       M39.2854
 3214 FORMAT ( 40H    KTAB     NIU     NUK  NENERG  INFEXP  ,/,         M37.5196
     2 1X, I7, 12I8 )                                                   M13.2612
      WRITE (LUNIT6, 3215)  T, TWOPI, FLTINF, XOPT, COPT                M37.5197
 3215 FORMAT ( 1X, 14X, 1HT,  10X, 5HTWOPI, 9X, 6HFLTINF,               M37.5198
     1        11X, 4HXOPT,  11X, 4HCOPT   ,/,                           M37.5199
     2 1X, 8E15.6  )                                                    M13.2616
C                                       $$$  UPDATE  INPUT  SOURCES  $$$M13.2618
 3219 IF ( NCHAIN  .EQ.  18 )  GO TO 900                                M37.5201
      NDX1 = KXTCS + NUK + 1                                            M37.5202
      XTCS( NDX1) = T                                                   M28.5440
      NDX1 = NDX1 + 1                                                   M28.5441
      XTCS( NDX1) = ISTEP                                               M28.5442
      NDX1 = NDX1 + 1                                                   M28.5443
      XTCS( NDX1) = DELTAT                                              M28.5444
      IF ( KONSCE .LT. NIUNRS )  GO TO 505                              M37.5203
      I = NIUNRS                                                        M37.5204
      NDY5 = KUD1                                                       M37.5205
 3456 N1 = IUTY(KIUTY+I)                                                M37.5206
      J = I + NUK                                                       M13.2623
  510 NDX1 = KXTCS + J                                                  M28.5447
      XTCS(NDX1) = 0.0                                                  M22.4777
       N13 = 0                                                          M37.5207
       D1 = ABSZ( UD1(NDY5+5) - UD1(NDY5+4) - ONEHAF )                  M37.5208
       IF ( D1 .LT. FLZERO )  N13 = 1                                   M37.5209
      IF (IPRSUP .GE. 6)  WRITE ( LUNIT6, 8637 )                        M37.5210
     1   ISTEP, I, UD1(NDY5+5), UD1(NDY5+4), D1                         M37.5211
 8637 FORMAT ( /,  1X,  16H   ISTEP       I,  17X,  3HUD5,  17X,        M23.3919
     1  3HUD4,  18X,  2HD1  ,/,  1X,  2I8,  3E20.11  )                  M23.3920
      IF ( T .LT. UD1(NDY5+4) - FLZERO *10. )   GO TO 500               M37.5212
      IF ( T .GE. UD1(NDY5+5) - FLZERO *10. )   GO TO 500               M37.5213
      IF ( N1  .LT.  90 )   GO TO 501                                   M13.2625
      K = UD1( NDY5 + 2 )                                               M38.3063
      L = IABS( KPOS( K))                                               M14.2255
      IF ( N1 .GT. 93 )  GO TO 500                                      M14.2256
      N2 = N1 - 89                                                      M14.2257
      GO TO (502, 503, 504, 506), N2                                    M14.2258
  502 XTCS(NDX1) = E( K)                                                M37.5215
      GO TO 500                                                         M14.2260
  503 IF ( N13 .EQ. 0 ) GO TO 509                                       M37.5216
      IF ( L .EQ. 11 ) XTCS(NDX1) = TCLOSE(K)                           M37.5217
      GO TO 500                                                         M37.5218
  509 IF ( L .LE. 3 )  XTCS(NDX1) = TCLOSE( K)                          M37.5219
      GO TO 500                                                         M14.2265
  504 XTCS(NDX1) = ETAC( K)                                             M37.5220
      GO TO 500                                                         M14.2267
  506 IF ( L .LE. 3 )  XTCS(NDX1) = 1.0                                 M37.5221
      GO TO 500                                                         M14.2269
  501 XTCS(NDX1) = UD1(NDY5+1)                                          M37.5222
      IF ( N1  .EQ.  11 )  GO TO 500                                    M39.2855
      IF ( N1  .NE.  14 )  GO TO 511                                    M39.2856
      XTCS(NDX1) = XTCS(NDX1) * COSZ(TWOPI * UD1(NDY5+3)  * T  +        M37.5223
     1     UD1(NDY5+2)  )                                               M37.5224
  511 IF ( N1 .NE. 23 )  GO TO 512                                      M39.2857
      IF ( T .LT. UD1(NDY5+4) + UD1(NDY5+2) - FLZERO * 10. )            M37.5225
     1  GO TO 500                                                       M28.5461
      UD1(NDY5+4) = UD1(NDY5+4) + UD1(NDY5+3)                           M37.5226
      GO TO 510                                                         M28.5464
  512 IF ( N1 .NE. 24 )  GO TO 500                                      M39.2858
      IF ( T .LT. UD1(NDY5+4)+ UD1(NDY5+3)- FLZERO*10. )  GO TO 513     M37.5227
      UD1(NDY5+4) = UD1(NDY5+4) + UD1(NDY5+3)                           M37.5228
      GO TO 512                                                         M28.5470
  513 NDX2 = KXTCS + J                                                  M28.5471
      XTCS(NDX2) = XTCS(NDX2) * ( T - UD1(NDY5+4) ) / UD1(NDY5+3)       M37.5229
      IF ( ABSZ( XTCS(NDX2) )  .LE.  10.*FLZERO )                       M28.5473
     1 XTCS(NDX2) = 0.0                                                 M28.5474
  500 I = I + 1                                                         M37.5230
      NDY5 = NDY5 + 5                                                   M37.5231
      IF ( I  .LE.  KONSCE )  GO TO 3456                                M37.5232
  505 IF ( IPRSUP  .LT.  6 )  GO TO 3030                                M37.5233
      WRITE ( LUNIT6, 3241 )                                            M37.5234
 3241 FORMAT ( /,  21H TABLES IN  'TACS3' .  ,/,                        M37.5235
     1 5X, 3HROW, 4X, 4HIUTY, 6X, 2HIU, 4X, 4HKPOS, 14X, 1HE,           M37.5236
     2 12X, 3HUD1, 12X, 3HUD2, 12X, 3HUD3, 9X, 6HTCLOSE, 11X, 4HXTCS )  M37.5237
      NDX5 = KUD1 - 5                                                   M37.5238
      DO  4040  K = 1, KONSCE                                           M37.5239
      NDX1 = ILNTAB( KALIU + K )                                        M38.3064
      IF (  K  .GT.  11 )  GO TO 4848                                   M37.5240
      WRITE ( LUNIT6, 3344 ) K, IUTY(KIUTY+K), TEXVEC(NDX1),            M38.3065
     1   KPOS(K), E(K), TCLOSE(K), XTCS(KXTCS+NUK+K)                    M37.5242
 3344 FORMAT ( 2I8, 2X, A6, I8, E15.6, 45X, 2E15.6 )                    M37.5243
      GO TO 4040                                                        M37.5244
 4848 NDX5 = NDX5 + 5                                                   M37.5245
      WRITE ( LUNIT6, 3244 )                                            M37.5246
     1 K,IUTY(K+KIUTY),TEXVEC(NDX1),KPOS(K),E(K),UD1(NDX5+1),           M38.3066
     2 UD1(NDX5+2),UD1(NDX5+3),TCLOSE(K),XTCS(KXTCS+NUK+K)              M37.5248
 3244 FORMAT (  2I8, 2X, A6, I8, 6E15.6  )                              M37.5249
 4040 CONTINUE                                                          M37.5250
 3030  L = IUTY( KIUTY + 4 )                                            M39.2859
      IF ( L .GT. 0 ) CALL CSUP( L )                                    M39.2860
C                                                 $$$  FORM  RHSIDE  $$$M13.2653
      NDX1 = NUK * 4                                                    M38.3068
      DO 3231 I = 4, NDX1, 4                                            M38.3069
 3231 RSBLK(KRSBLK+I) = 0.0                                             M38.3070
      NUKI = KISBLK - 8                                                 M38.3071
      DO 304 I = 1, NUK                                                 M37.5254
      NUKI = NUKI + 8                                                   M38.3072
      IF ( ISBLK(NUKI+2)  .EQ.  1 )  GO TO 304                          M38.3073
      K = KPRSUP + ISBLK(NUKI+3) + 4                                    M39.2861
      J = KRSBLK + ISBLK(NUKI+4) * 4                                    M38.3075
      RSBLK(J) = RSBLK(J) + PARSUP(K)                                   M39.2862
  304 CONTINUE                                                          M37.5259
C                                          $$$  FORWARD  ON  RHSIDE  $$$M13.2660
      NUKI = KISBLK - 8                                                 M38.3077
      NUKR = KRSBLK - 4                                                 M38.3078
      DO 305 I=1,NUK                                                    M13.2661
      NUKI = NUKI + 8                                                   M38.3079
      NUKR = NUKR + 4                                                   M38.3080
      M = ISBLK(NUKI+8)                                                 M38.3081
  301 K = ICOLCS( KCOLCS + M )                                          M38.3082
      IF (K .GE. I) GO TO 305                                           M38.3083
      NDX4 = KRSBLK + K * 4                                             M38.3084
      RSBLK(NUKR+4) = RSBLK(NUKR+4) - ATCS(KATCS+M)*RSBLK(NDX4)         M38.3085
      M = M + 1                                                         M13.2666
      GO TO 301                                                         M13.2667
  305 RSBLK(NUKR+4) = RSBLK(NUKR+4) * ATCS(KATCS+M)                     M38.3086
C                                           $$$  BACKWARD  ON  XTCS  $$$M37.5260
  327 MM = 1                                                            M37.5261
      NUKI = KISBLK + 8 * NUK                                           M38.3087
      NUKR = KRSBLK + 4 * NUK                                           M38.3088
      DO 3088 I=1,NUK                                                   M37.5263
      NUKI = NUKI - 8                                                   M38.3089
      NUKR = NUKR - 4                                                   M38.3090
      J = NUK + 1 - I                                                   M13.2671
  329 NDX1 = KXTCS  + J                                                 M22.4830
      XTCS(NDX1) = RSBLK(NUKR+4)                                        M38.3091
      M = ISBLK(NUKI+8)                                                 M38.3092
      N = IA                                                            M13.2680
      IF ( J .NE. NUK )  N = ISBLK(NUKI+16) - 1                         M38.3093
  309 IF ( ICOLCS(KCOLCS+M) .EQ. J )  GO TO 334                         M38.3094
      M = M + 1                                                         M13.2683
      GO TO 309                                                         M13.2684
  334 M = M + 1                                                         M13.2685
      IF ( M .GT. N )  GO TO 324                                        M13.2686
      K = ICOLCS( KCOLCS + M )                                          M38.3095
      NDX4 = KXTCS  + K                                                 M22.4843
      XTCS(NDX1) = XTCS(NDX1) - ATCS(KATCS+M) * XTCS(NDX4)              M38.3096
      GO TO 334                                                         M13.2689
  324 J1 = ISBLK(NUKI+5)                                                M38.3097
      K1 = ISBLK(NUKI+6)                                                M38.3098
      IF ( J1 .GT. 0 )  RSBLK(NUKR+2)  = XTCS( KXTCS + J1 )             M38.3099
      IF ( K1 .GT. 0 )  RSBLK(NUKR+3)  = XTCS( KXTCS + K1 )             M38.3100
      IF (XTCS(NDX1) .LT. RSBLK(NUKR+2)) XTCS(NDX1) = RSBLK(NUKR+2)     M38.3101
      IF (XTCS(NDX1) .GT. RSBLK(NUKR+3)) XTCS(NDX1) = RSBLK(NUKR+3)     M38.3102
      IF ( RSBLK(NUKR+2) .LE. RSBLK(NUKR+3) ) GO TO 308                 M39.2863
      IF ( IUTY(KIUTY+3)  .EQ.  0 )  GO TO 308                          M42.6718
      NDX1 = ILNTAB( KLNTAB + J )                                       M38.3104
      WRITE (LUNIT6, 198)  TEXVEC(NDX1), T                              M38.3105
  198 FORMAT( 5X,  94HWARNING. ---- THE VARIABLE LIMITS WHICH APPLY TO TM13.2698
     1HE TACS FUNCTION BLOCK WITH (OUTPUT) NAME  ', A6,  7H'  HAVE,  /, M13.2699
     2 19X,  83HCRISS-CROSSED.   THAT IS, THE UPPER LIMIT IS NOW LESS THM13.2700
     3AN THE LOWER LIMIT, AT TIME,   E14.5,  10H  SECONDS.    ,/,       M13.2701
     4 19X, 105HALTHOUGH THE SIMULATION WILL CONTINUE, STRANGE RESULTS MM13.2702
     5IGHT BE EXPECTED (BE SKEPTICAL).   THERE SHALL BE     ,/,         M13.2703
     6 19X,  85HNO WARNING MESSAGE FOR ANY SUBSEQUENT CRISS-CROSSING OF M13.2704
     7THE LIMITS OF ANY TACS BLOCK.      )                              M13.2705
      IUTY(KIUTY+3) = IUTY(KIUTY+3) - 1                                 M42.6719
  308 L = ISBLK( NUKI + 7 )                                             M39.2866
      IF ( L .GT. 0 )  CALL CSUP( L )                                   M39.2867
 3088 CONTINUE                                                          M37.5269
C                                                       $$$  OUTPUT  $$$M13.2708
  317 DO 1010 I =1, NSUP                                                        
      N1 = - INSUP( KJSUP + I )                                                 
      IF ( N1 .LT. 0 )  GO TO 1010                                      M39.2870
      IF ( INSUP(KKSUP+I) .NE. 53 )  GO TO 1010                                 
      J = IVARB( N1 + 1 )                                                       
      K = IVARB( N1 + 2 )                                                       
      B = 0.0                                                                   
      DO 601 N= J, K                                                    M37.5279
      M = KSUS( KALKSU + N )                                                    
  601 B = B + XTCS(KXTCS+M) * KSUS(KKSUS+N)                                     
      NN = IVARB(N1)                                                    M39.2874
      N5 = PARSUP(NN)                                                           
      N6 = PARSUP( NN + 2 )                                                     
      N7 = IVARB( N1 + 4 )                                                      
      NDX6 = KPRSUP + N7                                                        
      PARSUP( NDX6) = B                                                         
      N7 = N7 + 1                                                               
      IF (N7 .EQ. N5+N6) N7 = N5                                                
      IVARB(N1+4) = N7                                                          
 1010 CONTINUE                                                          M37.5291
      IF ( T  .GT.  TMAX )  GO TO 9900                                  M39.2881
C                                                  $$$  UPDATE  HST  $$$M13.2740
      IF ( NUK  .EQ.  0 )  GO TO 340                                    M38.3119
      NUKI = KISBLK - 8                                                 M38.3120
      NUKR = KRSBLK - 4                                                 M42.6720
      DO 3111 I = 1, NUK                                                M39.2882
      NUKI = NUKI + 8                                                   M38.3122
      NUKR = NUKR + 4                                                   M42.6721
      IF ( ISBLK(NUKI+2)  .EQ.  1 )  GO TO 3111                         M39.2883
      L = ISBLK( NUKI + 4 )                                             M38.3125
      PRX = XTCS( KXTCS + L )                                           M37.5296
      J = IABS( ISBLK(NUKI+1) )                                         M42.6722
      K = IABS( ISBLK(NUKI+9) ) - 1                                     M42.6723
      IF (  I  .EQ.  NUK )  K = NSU                                     M37.5299
      IF ( ISBLK(NUKI+1) .GT. 0 )  GO TO 2345                           M42.6724
      DO 5656 I1 = MA1, MA2                                             M42.6725
      IF ( IVARB(I1) .NE. -I )  GO TO 5656                              M42.6726
      MA3 = I1 + 1                                                      M42.6727
      DO 4545 I2 = MA3, MA2                                             M42.6728
      IF ( IVARB(I2) .LT. 0 ) GO TO 2345                                M42.6729
 4545 MA4 = I2                                                          M42.6730
      GO TO 2345                                                        M42.6731
 5656 CONTINUE                                                          M42.6732
 2345 PRU = 0.0                                                         M42.6733
      DO 312 M=J,K                                                      M42.6734
      N = KSUS( KALKSU + M )                                            M42.6735
      IF ( ISBLK(NUKI+1) .GT. 0 ) GO TO 5678                            M42.6736
      N1 = N - LSTAT(64) - NUK                                          M42.6737
      DO 6789 I3 = MA3, MA4                                             M42.6738
      IF ( IVARB(I3) .NE. N1 ) GO TO 6789                               M42.6739
      N = N + LSTAT(68)                                                 M42.6740
      GO TO 5678                                                        M42.6741
 6789 CONTINUE                                                          M42.6742
 5678 NDX1 = KXTCS  + N                                                 M42.6743
      PRU = PRU + XTCS(NDX1) * KSUS( KKSUS + M )                        M42.6744
  312 CONTINUE                                                          M13.2751
      JCM = KPRSUP + ISBLK( NUKI + 3 )                                  M39.2884
      K = ISBLK( NUKI + 2 ) - 1                                         M39.2885
      J = JCM + 6 * K - 2                                               M39.2886
      N1 = KRSBLK + L * 4 - 2                                           M38.3130
      N2 = N1 + 1                                                       M38.3131
      IF (PRX .EQ. RSBLK(N1) .OR. PRX .EQ. RSBLK(N2)) GO TO 2810        M42.6745
      IF ( K .EQ. 1 )  GO TO 330                                        M13.2755
      N22 = JCM + 6                                                             
      DO 331 N = N22, J, 6                                              M39.2888
      PPP  = PARSUP(N)*PRU - PARSUP(N+1) * PRX - PARSUP(N-2)            M39.2889
  331 PARSUP(N-2) = PPP + PARSUP(N+4)                                   M39.2890
  330 PARSUP(J) = PARSUP(J+2) * PRU - PARSUP(J+3) * PRX                 M39.2891
      GO TO 311                                                         M28.5480
C                               $$$  DYNAMIC  LIMITER  HST  $$$         M42.6746
 2810 N = J                                                             M42.6747
      PARSUP(J) = PARSUP(J+2) * PRU  -  PARSUP(J+3) * PRX               M42.6748
 2840 IF ( K .EQ. 1 )  GO TO 311                                        M42.6749
      K = K - 1                                                         M42.6750
      N = N - 6                                                         M42.6751
      PARSUP(N) = (PARSUP(N+2)*PRU-PARSUP(N+3)*PRX+PARSUP(N+6))/2.0     M42.6752
      GO TO 2840                                                        M42.6753
  311 IF ( IPRSUP  .GE.  4 )                                            M39.2897
     1   WRITE (LUNIT6, 2020)  I, ( PARSUP(N+4), N = JCM, J, 6 )        M39.2898
 2020 FORMAT( 10H  FUNCTION, I6, 6H  HST , 7E14.6 )                     M39.2899
 3111 CONTINUE                                                          M39.2900
      NDX1 = KXTCS + NUK + LSTAT(64) + 1                                M42.6754
      NDX2 = NDX1 + LSTAT(68)                                           M42.6755
      CALL MOVER ( XTCS(NDX1), XTCS(NDX2), NSUP )                       M42.6756
  340 RETURN                                                            M13.2768
C                                                  $$$  TERMINATION  $$$M13.2769
  900 L = NUK + LSTAT(64) + KONSUP + 1                                  M39.2901
      CALL CSUP( L )                                                    M39.2902
 9900 IF ( IPRSUP  .GE.  1 )                                            M37.5325
     1 WRITE (LUNIT6, 901)                                              M20.4237
  901 FORMAT( /,  44H NORMAL TERMINATION OF RUN WITHIN  'TACS3' .   )   M13.2772
      RETURN                                                            M28.5505
      END                                                               M13.2793
      SUBROUTINE SUBTS2                                                    13222
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
C     %INCLUDE  '//c/tsu/cables.INS.FTN'                                M44.1680
      EQUIVALENCE     (H1, SK),     (D2, SM )                              13227
      EQUIVALENCE     ( IPRSOV(35), IPOINT )                            M39.2903
C  WK1( KOFF1 + K ) << VM5DT( K ); WK1( KOFF2 + K ) << VK5DT( K )       M44.1681
C  WK1( KOFF3 + K ) <<<  FM5( K ); WK1( KOFF4 + K ) <<<  BK5( K )       M44.1682
C  WK1( KOFF5 + K ) <<<  VK5( K ); WK1( KOFF6 + K ) <<<  VM5( K )       M44.1683
C  WK1( KOFF7 + K ) <<<  HK2( K ); WK1( KOFF8 + K ) <<<  HM2( K )       M44.1684
C  WK1( KOFF9 + K ) <<<  HK3( K ); WK1( KOFF10 + K )<<<  HM3( K )       M44.1685
C  WK1( KOFF13 + K ) <<< HK1( K ); WK1( KOFF14 + K )<<<  HM1( K )       M44.1686
C  WK1( KOFF15 + K ) <<<  HK( K ); WK1( KOFF16 + K )<<<   HM( K )       M44.1687
C  WK1( KOFF17 + K ) <<<VKDT( K ); WK1( KOFF18 + K ) <<<VMDT( K )       M44.1688
      DIMENSION  WK1(1)                                                 M44.1689
      EQUIVALENCE ( SEMAUX(1), WK1(1) )                                 M44.1690
      DIMENSION INFDLI(1)                                               M44.1691
      EQUIVALENCE ( NAMEBR(1), INFDLI(1) )                                      
      COMMON /FDQLCL/ KOFF1,KOFF2,KOFF3,KOFF4,KOFF5,KOFF6,KOFF7,KOFF8,  M44.1693
     1 KOFF9,KOFF10,KOFF13,KOFF14,KOFF15,KOFF16,KOFF17,KOFF18,          M44.1694
     2 KOFF19,KOFF20,KOFF21,KOFF22,KOFF23,KOFF24,KOFF25,INOFF1,INOFF2,  M44.1695
     3 INOFF3,INOFF4,INOFF5,NQTT, LCBL, LMODE,NQTW                      M44.1696
C     INTRINSIC  IABSZ, ABSZ                                            M34. 151
      IF ( IPRSUP .GE. 1 )                                              M44.1697
     1 WRITE (LUNIT6, 3445) ( F(J), J=1, NTOT )                         M44.1698
 3445 FORMAT ( ' Top  SUBTS2.  F(1:NTOT) follows ...' ,/,               M44.1699
     1       ( 1X, 8E16.7 ) )                                           M44.1700
      NMODAL = KCOUNT                                                   M35.1610
      ISECTI = 400                                
      K=1                                                                       
      LLM1 = -1                                                         M22.4933
      LL0 = 0                                                           M22.4934
      LL1 = 1                                                           M22.4935
      IFTAIL = 0                                                           13230
C                                      INITIALIZE COUNTERS FOR THE -666 M32.5947
      IBF = 0                                                           M32.5948
      ISFD = 0                                                          M32.5949
      IKF = 0                                                           M32.5950
      KAA = 0                                                                   
C                                        ACCUMULATION OF NODE CURRENTS FM44.1702
C                                                          UPDATING LINEM44.1703
 1100 IT2=LENGTH(K)                                                        13231
      N1=KBUS(K)                                                                
      IF ( IPRSUP .GE. 1 )                                              M44.1704
     1 write(*,*) 'Top of SUBTS2 ,N1=', N1                              M44.1705
      IF(N1.LT.0) GO TO 1180                                                    
      IT2=IABS(IT2)                                                             
      IF(IT2.EQ.1) GO TO 1150                                                   
 1105 IT1=K                                                                     
      IF( LENGTH( K + 1 ) .EQ.  -555 )  GO TO 3535                              
      DO 1110 I=1,IT2                                                           
      N1=KBUS(IT1)                                                              
      N2=MBUS(IT1)                                                              
      VOLTI(I)=-E(N1)                                                           
      VOLTK(I)=-E(N2)                                                           
      VOLT(I)=VOLTK(I)-VOLTI(I)                                                 
 1110 IT1=IT1+1                                                                 
      N3=NR(K)                                                                  
      IF(T.GT.TMAX) GO TO 1190                                                  
C     CHECK FOR THE PRESENCE OF THE -666 BRANCHES  *   *   *   *   *   *M32.5951
      IF( LENGTH(K+1) .EQ. -666 ) GO TO 3531                            M32.5952
      CALL MULT(C(N3),VOLTI,CI(K),IT2,LLM1)                                     
      CALL MULT(C(N3),VOLTK,CK(K),IT2,LLM1)                                     
      IF( KODEBR(K) .LE. 0 )  GO TO 3530                                   13248
      CALL MOVER( CIK(K), VOLTI(1), IT2 )                                       
      CALL MULT( X(N3), VOLT(1), VOLTI(1), IT2, LL1 )                           
      CALL MULT( R(N3), VOLTI(1), CIK(K), IT2, LLM1 )                           
      DO 73529  I=1, 10                                                         
      IF( ISTEP .LE. 2   .AND.   IPRSUP .GE. 3 )                           13253
     1 WRITE(LUNIT6, 73530)  I, KBUS(I), MBUS(I), NR(I), LENGTH(I),     M17. 878
     2 KODEBR(I), R(I), X(I), TR(I), TX(I), C(I), VOLT(I), VOLTI(I),       13255
     3 CIK(I)                                                              13256
73530 FORMAT( /,  34H UPDATE OF  I(T-DELTAT)  AT 73530.   , 6I12, /,       13257
     1 ( 1X, 5E25.15) )                                                    13258
73529 CONTINUE                                                             13259
      GO TO 3535                                                           13260
 3531 CALL FDCINJ( IKF, ISFD, IBF)                                      M34. 152
      CALL MOVER( VOLT(1), CIK(K), IT2 )                                M32.5954
      GO TO 3535                                                        M32.5955
 3530 CALL MULT( X(N3), CIK(K), VOLT(1), IT2, LL1 )                     M22.4940
      CALL MULT( R(N3), VOLT(1), CIK(K), IT2, LLM1 )                    M22.4941
 3535 IT1 = K + IT2 - 1                                                    13263
      DO 1120 I=K,IT1                                                           
      N1=KBUS(I)                                                                
      N2=MBUS(I)                                                                
      F(N1)=F(N1)-CIK(I)-CI(I)                                                  
 1120 F(N2)=F(N2)+CIK(I)-CK(I)                                                  
C                                                                       M44.1706
C                                                                       M44.1707
C                                                                       M44.1708
C                                                                       M44.1709
      GO TO 1200                                                           13269
 1150 I=NR(K)                                                                   
      MSIGN=MBUS(K)                                                             
      N2=IABS(MSIGN)                                                            
      GUS1=E(N1)                                                                
      GUS2=E(N2)                                                                
      GUS3=GUS1-GUS2                                                            
      A=CIK(K)                                                                  
      CI1=CI(K)                                                                 
      CK1=CK(K)                                                                 
      IT1=IABS(I)                                                               
      CAP=C(IT1)                                                                
      IF ( IPRSUP .GE. 3 )                                              VAX. 125
     1 WRITE (LUNIT6, *)  ' IT1, N1, N2, K, I, T =',                    VAX. 126
     2                      IT1, N1, N2, K, I, T                        VAX. 127
      IF ( IPRSUP  .GE.  3 )                                            M32.5956
     1   WRITE (LUNIT6,5432) X(IT1), E(N1), E(N2), CIK(K), R(IT1)       M20.4243
 5432 FORMAT(40H X(IT1), E(N1), E(N2), CIK(K), R(IT1) = ,5E18.10,/)     M20.4244
      IF(I.LT.0) GO TO 1170                                                     
      IF(T.GT.TMAX) GO TO 1105                                             13282
      A=(GUS3+A*X(I))*R(I)-A                                                    
      GUS4=-CAP *GUS1-CI1                                                       
      CK1=-CAP *GUS2-CK1                                                        
      F(N1)=F(N1)-GUS4                                                          
      F(N2)=F(N2)-CK1                                                           
 1160 F(N1)=F(N1)-A                                                             
      F(N2)=F(N2)+A                                                             
      CIK(K)=A                                                                  
      CI(K)=GUS4                                                                
      CK(K)=CK1                                                                 
      GO TO 1200                                                           13293
 1170 GUS3=X(IT1)*GUS3                                                          
      GUS4=GUS3+A                                                               
      IF(CAP.EQ.0.) GO TO 1175                                                  
      CK1=CK1+CAP *(GUS4+CI1)                                           M05. 657
      GUS3=GUS3-2.0*X(IT1)*CK1                                          M05. 658
 1175 A=GUS3-R(IT1)*GUS4                                                M05. 659
      IF(MSIGN.GT.0) GO TO 1160                                                 
      GUS3=GUS4                                                         M05. 660
      D1 = N1                                                           M05. 661
      IF ( CK1  .EQ.  D1 )   GUS3 = GUS4 + CI(K-1)                              
      KCOUNT=KCOUNT+1                                                           
      BVALUE(KCOUNT)=GUS3                                                       
      GO TO 1160                                                                
 1180 N1 = -N1                                                                  
      N2=MBUS(K)                                                                
      IF ( IMODEL(K) .NE. -4 ) GO TO 4340                               M44.1712
      IF ( IT2 .LT. 0 ) GO TO 481                                               
  481 IT2 = -IT2                                                                
120   KY = K                                                                    
      KOFF11 = KOFF25 + 288                                                     
      DO 103 I = 1, ISECTI                                                      
      WK1( KOFF8 + I ) = 0.D0                                                   
      WK1( KOFF7 + I ) = 0.D0                                                   
      WK1( KOFF6 + I ) = 0.D0                                                   
      WK1( KOFF5 + I ) = 0.D0                                                   
      WK1( KOFF9 + I )  = 0.0                                                   
      WK1( KOFF10 + I ) = 0.0                                                   
      KY = KY + 1                                                       M44.1724
103   CONTINUE                                                          M44.1725
       IF ( IPRSUP .GT. 0 )                                             M44.1726
     1 write( *,*)'+++++BEFORE UPDATING HISTORY SOURCE++++ T=',         M44.1727
     2                                                     T            M44.1728
      KQ = INFDLI(INOFF1+K)                                             M44.1729
      KQK0 = INFDLI(INOFF2+K)                                                   
      KH = K                                                            M44.1731
      NNQ1 = INFDLI(INOFF3+k)                                                   
C      LJ = KQ                                 ! 1st pole of Qii for theM44.1754
c      IF ( KQ .GT. 1) LJ = ( KQ-1 ) * 2 + 1                            M44.1755
C      KILINE = LPAST                                  !next cell is freM44.  19
C      KILINE = NR(K)                                                   M44. 684
C      LJ = KILINE + LJ                                                 M44.  20
      LJ = NR(K)                                                        M44. 685
113   DO 104 J=1,IT2                                                            
      KY = K                                                                    
      DO 105 I=1,IT2 
      NN1 = -KBUS(KY)                                                           
      NN2 = IABS( MBUS(KY) )                                            
      GKT = SCONST(KQK0) * E(NN1)                                               
      GMT = SCONST(KQK0) * E(NN2)                                               
      NTEQ = WK1(KOFF20+KQ)                                                     
      IF (  LINT .EQ. 1 ) GO TO 9824                                                 
      N1 = KODSEM(KY)                                                           
      NRZ = CKI(KY)                                                             
      NRA = CKKJM(KY)                                                           
      NK1 = N1 + 5*NRZ + 5*NRA + 4 + 1                                  M44. 686
      NN17 = SCONST(NK1)                                                M44. 687
      NN5 = NK1 +1 +NN17                                   
      N6 = NN5 + 1                                                              
C      CALL CONVOL(VM(I),VMDT(I),VMJ5DT(IADRS),VMOUT,HMOUT              M44.1765
C     1           ,VK(I),VKDT(I),VKJ5DT(IADRS),VKOUT,HKOUT              M44.1766
C     2           ,CJQ(IADRS),DJQ(IADRS),EJQ(IADRS),QK0(KQ),ESUMQ(KQ)   M44.1767
C     3           ,NTEQ,0)                                              M44.1768
      NQ5 = NNQ1 + 3 * NTEQ + 1                                                 
      NQ6 = NQ5 + 1                                                             
      HKTDT = SCONST(NQ6) * SCONST(N6)                                          
      HMTDT = SCONST(NQ6) * SCONST(NN5)                                         
      IF ( IPRSUP .GT. 0 )                                              M44.1778
     1 write(*,4933)                                                    M44.1779
4933  FORMAT(30H KQ NTEQ   LJ  LJ1  CJQ(IADRS),                         M44.1780
     112H     QK0(KQ),                                                  M44.1781
     236H   ESUMQ(KQ)       HKTDT       HMTDT,                          M44.1782
     336H       VK(I)     VKDT(I)   VKJ5DT(J),                          M44.1783
     412H       VM(I))                                                  M44.1784
      LJ1 = LJ + NTEQ                                                           
      JKL = 0                                                           M44. 690
      DO 1009 JQ=1,NTEQ                                                 M44.1786
      NNQ2 = NNQ1 + JQ                                                          
      NNQ3 = NNQ2 + NTEQ                                                        
      NQ4 = NNQ3 + NTEQ                                                         
C     VKJ5DT(IADRS) = QKI(IADRS) * VKJ5DT(IADRS)                        M44.1790
C    1              + EJQ(IADRS) * SCONST(N6)                           M44.1791
C    2              + QPI(IADRS) * E(NN1)                               M44.1792
      IF ( ABS(SCONST(NNQ2)) .GE. 1.E+14 ) GO TO 1800                   M44. 691
      GO TO 1083                                                        M44. 692
 1800 JKL = JKL + 1                                                     M44. 693
      IF (JKL .EQ. 2) GO TO 1801                                        M44. 694
      SCONST(NNQ2) = SCONST(NNQ2) / 1.E+15                              M44. 695
 1083 XM(LJ1) = SCONST(NNQ2) * XM(LJ1)                                  M44.1793
     1        + SCONST(NQ4)  * SCONST(N6)                               M44.1794
     2        + SCONST(NNQ3) * E(NN1)                                   M44.1795
      XM(LJ)  = SCONST(NNQ2) * XM(LJ)                                   M44.1796
     1        + SCONST(NQ4)  * SCONST(NN5)                              M44.1797
     2        + SCONST(NNQ3) * E(NN2)                                   M44.1798
      GKT = GKT + XM(LJ1)                                               M44.1799
      GMT = GMT + XM(LJ)                                                M44.1800
      HKTDT = HKTDT + SCONST(NNQ2) * XM(LJ1)                                    
      HMTDT = HMTDT + SCONST(NNQ2) * XM(LJ)                                     
      IF ( JKL .EQ. 1 ) GO TO 1802                                      M44. 696
 1801 JKL = 0                                                           M44. 697
 1802 IF ( IPRSUP .GT. 0 )                                              M44.1803
     1 write(*,4333) KQ,NTEQ,LJ,LJ1,SCONST(NNQ2),                       M44.1804
     2      SCONST(KQK0),SCONST(NQ6),HKTDT,HMTDT,                       M44.1805
     3                             E(NN1), SCONST(N6),XM(LJ1),          M44.1806
     4                             E(NN2)                               M44.1807
4333  FORMAT ( 1X,I2,3X,I2,2X,I3,2X,I3, 9E12.5 )                        M44.1808
      LJ = LJ + 1                                                               
      LJ1 = LJ1 + 1                                                             
      IF ( lj .GE. LPAST+lsiz28)                                        M44.  21
     1 WRITE(*, 9835) lj, LPAST+lsiz28                                  M44.  22
 9835 FORMAT(5H+XM(), I5,27H exceed the limit of list28, I5,/,          M44.  23
     3 20H If you are going to/,                                        M44.  24
     4 58H run the case agian, you should have your programer change,/, M44.  25
     5 34H the dimenssion. Massage from LJG.)                           M44.  26
1009  CONTINUE                                                          M44.1812
9824  KQK0 = KQK0 + 2 * NTEQ + 1                                        M44.1813
      LJ = LJ + NTEQ                                                            
      WK1( KOFF8 + KH ) = WK1( KOFF8 + KH )                             M44.1815
     1                  + HMTDT                                                 
      WK1( KOFF6 + KH ) = WK1( KOFF6 + KH )                             M44.1817
     1                  + GMT                                                   
      WK1( KOFF7 + KH ) = WK1( KOFF7 + KH )                             M44.1819
     1                  + HKTDT                                                 
      WK1( KOFF5 + KH ) = WK1( KOFF5 + KH )                             M44.1821
     1                  + GKT                                                   
      NNQ1 = NNQ1 + 3 * NTEQ + 2                                        M44.1823
      KY = KY + 1                                                       M44.1824
      KQ=KQ+1                                                                   
105   CONTINUE                                                          M44.1826
      KH = KH + 1                                                       M44.1827
104   CONTINUE                                                          M44.1828
C                                                                       M44.1829
C        Evaluation of hm1(t+dt) and hk1(t+dt)                          M44.1830
C                                                                       M44.1831
      KY = K                                                            M44.1832
      N1 = KODSEM(KY)                                                           
115   DO 102 I=1, IT2                                                   M44.1835
C      CALL CONVOL(VM5(I),VM5DT(I),GMJDT(IADRS),GM,HM1(I)               M44.1836
C     1           ,VK5(I),VK5DT(I),GKJDT(IADRS),GK,HK1(I)               M44.1837
C     2           ,CJY(IADRS),DJY(IADRS),EJY(IADRS),YK0(I),ESUMY(I)     M44.1838
C     3           ,NTEY,0)                                              M44.1839
      IF ( IPRSUP .GT. 0 )                                              M44.1840
     1 write(*,*) 'Just before convolution,VM5dt(KY),Vk5dt(KY)dt=',     M44.1841
     2                               WK1(KOFF1+KY),WK1(KOFF2+KY)        M44.1842
      N4 = INDHST(KY)                                                   M44.1843
      N1 = KODSEM(KY)                                                           
      NRZ = CKI(KY)                                                             
      NRA = CKKJM(KY)                                                           
      N8 = N1 + 5*NRZ + 2*NRA + 1                                               
      N9 = N8 + 1                                                               
      GKT = SCONST(N1) * WK1( KOFF5 + KY )                                      
      GMT = SCONST(N1) * WK1( KOFF6 + KY )                                      
      WK1(KOFF13+KY) = SCONST(N8) * WK1( KOFF2 + KY )                           
      WK1(KOFF14+KY) = SCONST(N8) * WK1( KOFF1 + KY )                           
      IF ( IPRSUP .GT. 0 )                                              M44.1854
     1 write(*,4833)                                                    M44.1855
4833  FORMAT(21H KY  NRZ   CJY(IADRS),                                          
     127H  YK0(KQ) ESUY(KQ) WK1(K13),                                           
     227H WK1(K14)  VK5(KY) VK5DT(K),                                   M44.1858
     527H GKJDT(I)  VM5(KY) WK1(KF3),                                   M44.1859
     327H WK1(KF4)  SCN(N7)  SCN(N6),                                   M44.1860
     627H SCN(NN5)      GMT      GKT)                                   M44. 698
      JKL = 0                                                           M44. 699
      JQL = 1                                                           M44. 700
      NKYW = N4 + 20                                             
      DO 4009 J=1,NRZ                                                   M44.1861
      N5 = N1 + 2*NRZ + 2*NRA + J                                               
      N6 = N5 + NRZ                                                             
      N7 = N6 + NRZ                                                             
      NK1 = N1 + 5*NRA + 5*NRZ + 5                                          
      NN17 = SCONST(NK1)                                                M44. 704
      NN5 = Nk1 + J +4 +NN17                         
      NN6 = NN5 + NRZ                                                   M44.1866
      NKY = N1 + J                                                      M44. 705
      IF ( SCONST(NKY) .GT. 1.E+13 ) GO TO 2000                         M44. 706
      SCONST(NN6) = SCONST(N5) * SCONST(NN6)                                    
     1            + SCONST(N7) * WK1( KOFF2 + KY )                              
     2            + SCONST(N6) * WK1( KOFF5 + KY )                              
      SCONST(NN5) = SCONST(N5) * SCONST(NN5)                                    
     1            + SCONST(N7) * WK1( KOFF1 + KY )                              
     2            + SCONST(N6) * WK1( KOFF6 + KY )                              
      GKT=GKT+SCONST(NN6)                                                       
      GMT=GMT+SCONST(NN5)                                                       
      WK1(KOFF13+KY) = WK1(KOFF13+KY)                                   M44.1875
     1               + SCONST(N5)                                       M44.1876
     2               * SCONST(NN6)                                              
      WK1(KOFF14+KY) = WK1(KOFF14+KY)                                   M44.1878
     1               + SCONST(N5)                                       M44.1879
     2               * SCONST(NN5)                                              
      GO TO 2002                                                        M44. 707
 2000 JKL = JKL + 1                                                     M44. 708
      IF ( JKL .EQ. 2) GO TO 2001                                       M44. 709
      NKYW = NKYW + JQL                                                 M44. 710
      NN10 = NKYW  + 1                                                          
      NN11 = NN10 + 1                                                           
      SCONST(NN6) = CNVHST(NKYW) * SCONST(NN6)                                  
     1            + CNVHST(NN11) * WK1( KOFF2 + KY )                            
     2            + CNVHST(NN10) * WK1( KOFF5 + KY )                            
      SCONST(NN5) = CNVHST(NKYW) * SCONST(NN5)                                  
     1            + CNVHST(NN11) * WK1( KOFF1 + KY )                            
     2            + CNVHST(NN10) * WK1( KOFF6 + KY )                            
      GKT=GKT+SCONST(NN6)                                                       
      GMT=GMT+SCONST(NN5)                                                       
      WK1(KOFF13+KY) = WK1(KOFF13+KY)                                   M44. 721
     1               + SCONST(N5)                                       M44. 722
     2               * SCONST(NN6)                                              
      WK1(KOFF14+KY) = WK1(KOFF14+KY)                                   M44. 724
     1               + SCONST(N5)                                       M44. 725
     2               * SCONST(NN5)                                              
      JQL = JQL + 6                                                     M44. 727
 2002 IF ( IPRSUP .GT. 0 )                                              M44.1881
     1 write(*,5333)     KY,NRZ,SCONST(N5),                             M44.1882
     1                          SCONST(N1),SCONST(N8),                  M44.1883
     2  WK1(KOFF13+KY),WK1(KOFF14+KY),WK1(KOFF5+KY),WK1(KOFF2+KY),      M44.1884
     4  SCONST(NN6), WK1(KOFF6+KY) ,WK1( KOFF3 + KY ),WK1( KOFF4 + KY ),M44.1885
     3  SCONST(N7),  SCONST(N6),                                        M44.1860
     6  SCONST(NN5),      GMT  ,    GKT                                 M44. 728
 5333 FORMAT ( 1X,I2,3X,I2,16E9.3 )                                     M44.1886
      IF ( JKL .EQ. 1 ) GO TO 4009                                      M44. 729
 2001 JKL = 0                                                           M44. 730
 4009 CONTINUE                                                          M44.1887
C        Evaluate im'(t) = fm'(t) - gm(t)                               M44.1888
C                 ik'(t) = -bk'(t) + gk(t)                              M44.1889
C      AIM5(KY)=FM5(KY)-GMT                          !FM5() = A1*FK' froM44.1890
C      AIK5(KY)=-BK5(KY)+GKT                         !BK5() = A1*BM' froM44.1891
      CNVHST(N4+13) = WK1( KOFF3 + KY )                                         
     1              - GMT                                                       
      CNVHST(N4+14) = - WK1( KOFF4 + KY )                                       
     2              + GKT                                                       
      IF ( IPRSUP .GT. 0 )                                              M44.1896
C     1 write(*,3335) KY,HM1(KY),HK1(KY),AIM5(KY),AIK5(KY)              M44.1897
     1 write(*,3335) KY,WK1(KOFF14+KY),WK1(KOFF13+KY),                  M44.1898
     2                         CNVHST(N4+13),CNVHST(N4+14)              M44.1899
3335  FORMAT(30HHM1(t),hK1(t),Im5(KY),Ik5(KY)=,                         M44.1900
     1            1x,I1,4E14.5)                                         M44.1901
      KY = KY + 1                                                       M44.1902
102   CONTINUE                                                          M44.1903
C                                                                       M44.1904
C        Evaluation of hm3(t+dt) and hk3(t+dt)                          M44.1905
C        (from the convolutions: im(t+dt) = q(t+dt) # im'(t+dt)         M44.1906
C                                ik(t+dt) = q(t+dt) # ik'(t+dt) )       M44.1907
C                                                                       M44.1908
C                                       The calculation of Im(t) = Q(t) M44.1909
      if ( lint .eq. 1) go to 116                                       M44.1910
      KA = K                                                            M44.1911
      KQK0 = INFDLI(INOFF2+K)                                                   
      NNQ1 = INFDLI(INOFF3+K)                                                   
      KQ = INFDLI(INOFF1+K)                                             M44.1914
C      LJ = KQ                        ! 1st pole of Qii for the branch KM44.1915
      LJ = NR(K)                                                        M44.1916
c      IF ( KQ .GT. 1) LJ = ( KQ-1 ) * 2 + 1                            M44.1917
C      KILINE = LPAST                                  !next cell is freM44.  27
C      KILINE = NR(K)                                                   M44. 731
C      LJ = KILINE + LJ                                                 M44.  28
      DO 107 J=1, IT2                                                   M44.1918
      N4 = INDHST(KA)                                                   M44.1919
      N1 = KODSEM(KA)                                                           
      NRZ = CKI(KA)                                                             
      NRA = CKKJM(KA)                                                           
      NK1 = N1 + 5*NRA + 5*NRZ + 4 + 1                                  M44. 732
      NN17 = SCONST(NK1)                                                M44. 733
      NN5 = N1 + 5*NRZ + 5*NRA                                          M44.1923
     1               + 4 + 2 +1 +1 +NN17                                        
      N6 = NN5 + 1                                                      M44.1924
      KI = K                                                            M44.1925
      DO 108 I=1, IT2                                                   M44.1926
      NTEQ = WK1(KOFF20+KQ)                                                     
C      CALL CONVOL(AIM5(J),AIM5DT(J),AIMJDT(IADRS),VMOUT,HMOUT          M44.1928
C     1           ,AIK5(J),AIK5DT(J),AIKJDT(IADRS),VKOUT,HKOUT          M44.1929
C     2           ,CJQ(IADRS),DJQ(IADRS),EJQ(IADRS),QK0(K),ESUMQ(K)     M44.1930
C     3           ,NTEQ,0)                                              M44.1931
      GKT = SCONST(KQK0)                                                M44.1932
     1      * CNVHST(N4+14)                                                     
      GMT = SCONST(KQK0)                                                M44.1934
     1      * CNVHST(N4+13)                                                     
      NQ5 = NNQ1 + 3 * NTEQ + 1                                                 
      NQ6 = NQ5 + 1                                                             
      HKOUT = SCONST(NQ6) * SCONST(N6)                                          
      HMOUT = SCONST(NQ6) * SCONST(NN5)                                         
      IF ( IPRSUP .GT. 0 )                                              M44.1941
     1 write(*,4433)                                                    M44.1942
4433  FORMAT(27H KQ NTEQ IADRS    CJQ(IADS),                            M44.1943
     113H      QK0(KQ),                                                 M44.1944
     239H    ESUMQ(KQ)        HKOUT        HMOUT,                       M44.1945
     339H     AIK5(KA)    AIK5DT(J)   AIKJDT(IS),                       M44.1946
     413H     AIM5(KA))                                                 M44.1947
      LJ1 = LJ + NTEQ                                                   M44.1948
      JKL = 0                                                           M44. 735
      DO 6009 JQ=1,NTEQ                                                 M44.1949
      NNQ2 = NNQ1 + JQ                                                          
      NNQ3 = NNQ2 + NTEQ                                                        
      NQ4 = NNQ3 + NTEQ                                                         
      IF ( ABS(SCONST(NNQ2)) .GE. 1.E+14 ) GO TO 1804                   M44. 736
      GO TO 1805                                                        M44. 737
 1804 JKL = JKL + 1                                                     M44. 738
      IF (JKL .EQ. 2) GO TO 1806                                        M44. 739
      SCONST(NNQ2) = SCONST(NNQ2) / 1.E+15                              M44. 740
 1805 XK(LJ1) = SCONST(NNQ2) * XK(LJ1)                                          
     1        + SCONST(NQ4)  * SCONST(N6)                                       
     2        + SCONST(NNQ3) * CNVHST(N4+14)                                    
      XK(LJ)  = SCONST(NNQ2) * XK(LJ)                                           
     1        + SCONST(NQ4)  * SCONST(NN5)                              M44.1957
     2        + SCONST(NNQ3) * CNVHST(N4+13)                            M44.1958
      GKT = GKT + XK(LJ1)                                               M44.1959
      GMT = GMT + XK(LJ)                                                M44.1960
      HKOUT = HKOUT                                                     M44.1961
     1      + SCONST(NNQ2) * XK(LJ1)                                            
      HMOUT = HMOUT                                                     M44.1963
     1      + SCONST(NNQ2) * XK(LJ)                                             
      IF (JKL .EQ. 1 ) GO TO 1807                                       M44.1965
 1806 JKL = 0                                                           M44.1966
 1807 IF ( IPRSUP .GT. 0 )                                              M44.1967
     1 write(*,4773) KQ,NTEQ,IADRS,SCONST(NNQ2),                        M44.1968
     2   SCONST(KQK0),SCONST(NQ6),HKOUT,HMOUT,                          M44.1969
     3        CNVHST(N4+14),SCONST(N6),XK(LJ1),CNVHST(N4+13)            M44.1970
4773  FORMAT ( 1X,I2,3X,I2,3X,I3, 9E13.5 )                              M44.1971
C      IADRS = IADRS + 1                                                M44.1972
      LJ = LJ + 1                                                       M44.1973
      LJ1 = LJ1 + 1                                                     M44.1974
6009  CONTINUE                                                          M44.1975
      KQK0 = KQK0 + 2 * NTEQ + 1                                        M44.1976
      LJ = LJ + NTEQ                                                    M44.1977
      IF ( IPRSUP .GT. 0 )                                              M44.1978
     1 write( *,*)'hist.sour. with current  HM3(ki), HK3(ki)',          M44.1979
     2                         WK1(KOFF10+KI), WK1(KOFF9+KI)            M44.1980
      WK1(KOFF10+KI) = WK1(KOFF10+KI) + HMOUT                           M44.1981
      WK1(KOFF9+KI)  = WK1(KOFF9+KI)  + HKOUT                           M44.1982
      IF ( IPRSUP .GT. 0 )                                              M44.1983
     1 write( *,*)'hist.sour. with current  HM3(ki), HK3(ki)',          M44.1984
     2                         WK1(KOFF10+ki), WK1(KOFF9+ki)            M44.1985
      KI = KI + 1                                                       M44.1986
      KQ=KQ+1                                                           M44.1987
      NNQ1 = NNQ1 + 3 * NTEQ + 2                                        M44.1988
108   CONTINUE                                                          M44.1989
      KA = KA + 1                                                       M44.1990
107   CONTINUE                                                          M44.1991
C                         Evaluate fk'(t) and bm'(t):                   M44.1992
C                                                      fk'(t) = 2ik'(t) M44.1993
C                                                     bm'(t) = -2im'(t) M44.1994
  116 KCC = KOFF25 + 288                                                M44.1995
      KOFF11 = KOFF25 + 288                                             M44.1996
      DO 1097 I = 1, IBR                                                        
      IF (  KODSEM(I) .EQ. 0.0                                          M44.1998
     1        .OR. IMODEL(I) .NE. -4 ) GO TO 1097                       M44. 741
      KKTAU = INDHST(I)                                                         
      NTERM = CNVHST(KKTAU) / DELTAT + 1                                        
      KCC = KCC + 2 * NTERM                                             M44.2001
1097  CONTINUE                                                          M44.2002
      WK1(KCC+1) = 68556311.00                                          M44.2003
      KA = K                                                            M44.2005
      IF ( LMODE .EQ. 4 ) KOFF11  = LCBL                                        
      DO 109 I=1, IT2                                                           
      N4 = INDHST(KA)                                                   M44.2008
      FK5 =  2.D0 * CNVHST(N4+14) + WK1( KOFF4 + KA )                           
      BM5 = -2.D0 * CNVHST(N4+13) + WK1( KOFF3 + KA )                           
C                                       Update past history terms of fk'M44.2011
      KKTAU = INDHST(KA)                                                        
      NTERM = CNVHST(KKTAU) / DELTAT + 1                                        
      IF ( IPRSUP .GT. 0 )                                              M44.2014
     1 write(*,4143)                                                    M44.2015
4143  FORMAT(38Hnterm ktau    fkh5(ktau)           fk5,                 M44.2016
     128H    bmh5(ktau)           bm5,                                  M44.2017
     242H        fk5tdt        fk5tau        bm5tdt,                    M44.2018
     314H        bm5tau)                                                M44.2019
C      CALL UPDCBL(FKH5(KTAU),FK5,FK5TDT,FK5TAU                         M44.2020
C     1           ,BMH5(KTAU),BM5,BM5TDT,BM5TAU                         M44.2021
C     1           ,NTERM,NKILL(KA),TAU(KA),DELTAT)                      M44.2022
      NKLL = INFDLI(INOFF5+KA)                                                  
      KOFF11 = KOFF11 + NKLL                                                    
      KOFF12 = KOFF11 + NTERM                                                   
      FK5TAU = WK1(KOFF11)                                                      
      BM5TAU = WK1(KOFF12)                                                      
      WK1(KOFF11) = FK5                                                         
      WK1(KOFF12) = BM5                                                         
      NK1 = INFDLI(INOFF5+KA) - 1                                               
      IF( NK1 .EQ. 0 )                                                          
     1 KOFF11 = KOFF11 + NTERM                                                  
      IF( NK1 .EQ. 0 )                                                          
     1 KOFF12 = KOFF12 + NTERM                                          M44.2038
      NK2 = NK1 - 1                                                             
      IF( NK2 .EQ. 0 )                                                          
     1 NK3 = KOFF11 - 2 + NTERM                                                 
      IF( NK2 .EQ. 0 )                                                  M44.2043
     1 NK4 = KOFF12 - 2 + NTERM                                         M44.2044
      C2 = CNVHST(KKTAU) - NTERM * DELTAT                                       
      C1 = - C2 - DELTAT                                                        
      IF ( NK2 .NE. 0 )    GO TO 7601                                   M44.2049
      YK = WK1(KOFF11-1) * C1                                           M44.2051
     1   + WK1(NK3) * C2                                                        
      YM = WK1(KOFF12-1) * C1                                           M44.2053
     1   + WK1(NK4) * C2                                                        
      GO TO 7602                                                        M44.2055
7601  YK = WK1(KOFF11-1) * C1                                           M44.2057
     1   + WK1(KOFF11-2) * C2                                                   
      YM = WK1(KOFF12-1) * C1                                           M44.2060
     1   + WK1(KOFF12-2) * C2                                                   
7602  WK1(KOFF11-1) = - YK / DELTAT                                             
      WK1(KOFF12-1) = - YM / DELTAT                                             
      FK5TDT = WK1(KOFF11-1)                                                    
      BM5TDT = WK1(KOFF12-1)                                                    
      KAA = KAA + 2 * NTERM                                                     
C                                                      !get ready for neM44.2070
      KBB = KOFF25 + 288                                                M44.2071
     1                  + KAA  + 1                                              
      IF (WK1(KBB) .EQ. 68556311.00 )                                   M44.2073
     1                   KAA = 0                                                
C                                                  !get ready for next tM44.2075
      KOFF11 = KOFF25 + 288                                             M44.2076
     1       + KAA                                                              
      INFDLI(INOFF5+KA) = NK1                                                   
      IF( INFDLI(INOFF5+KA) .EQ. 0 )                                    M44.2080
     1                  INFDLI(INOFF5+KA) = NTERM                               
      IF ( IPRSUP .GT. 0 )                                              M44.2083
     1 write(*,5337) nterm, ktau,WK1(KOFF11),fk5,WK1(KOFF12),           M44.2084
     2  bm5,fk5tdt,fk5tau,bm5tdt,bm5tau                                 M44.2085
5337  FORMAT ( 3X,I2,3X,I2, 8E14.5 )                                    M44.2086
C                                 Evaluation of fm'(t+dt) = a1(t+dt) # fM44.2088
C                                               bk'(t+dt) = a1(t+dt) # bM44.2089
C      CALL CONVOL(FK5TDT,FK5TAU,FMJ5(IADRS),FM5(I),HKOUT               M44.2090
C     1           ,BM5TDT,BM5TAU,BKJ5(IADRS),BK5(I),HMOUT               M44.2091
C     2           ,CJA(IADRS),DJA(IADRS),EJA(IADRS),0.D0,0.D0,NTEA,1)   M44.2092
      N1 = KODSEM(KA)                                                           
      NRZ = CKI(KA)                                                             
      NRA = CKKJM(KA)                                                           
      WK1( KOFF3 + KA ) = 0.D0                                                  
      WK1( KOFF4 + KA ) = 0.D0                                                  
      IF ( IPRSUP .GT. 0 )                                              M44.2099
     1 write(*,4435)                                                    M44.2100
4435  FORMAT(19H KA  NRA CJA(IADRS),                                    M44.2101
     133H DJA(IADRS) EJA(IADRS)     FK5TDT,                             M44.2102
     333H     FK5TAU FMJ5(IARS)     BM5TDT,                             M44.2103
     433H     BM5TAU BKJ5(IARS)        FM5,                             M44.2104
     511H        BK5)                                                   M44.2105
      JGL = 0                                                           M44. 742
      DO 4204 J=1,NRA                                                           
      N5 = N1 + 5*NRZ + 2 *NRA + 2 + J                                          
      N6 = N5 + NRA                                                             
      N7 = N6 + NRA                                                             
      NKKK1 = N1 + 5*NRA + 5*NRZ + 4 + 1                                        
      NN17 = SCONST(NKKK1)                                                      
      NN5 = N1 + 7*NRZ + 5*NRA + 9 + J + NN17                                   
      NN6 = NN5 + NRA                                                           
      NK5 = N1 + 2*NRZ + J                                                      
      IF ( ABS( SCONST(NK5) ) .gE. 1.E13 )                              M44. 746
     1                                     JGL = JGL + 1                M44. 747
      IF ( JGL .EQ. 2) GO TO 5486                                       M44. 748
      SCONST(NN5) = SCONST(N5) * SCONST(NN5)                                    
     1            + SCONST(N7) * FK5TAU                                         
     2            + SCONST(N6) * FK5TDT                                         
      SCONST(NN6) = SCONST(N5) * SCONST(NN6)                                    
     1            + SCONST(N7) * BM5TAU                                         
     2            + SCONST(N6) * BM5TDT                                         
      WK1( KOFF3 + KA ) = WK1( KOFF3 + KA )                                     
     1                  + SCONST(NN5)                                   M44. 750
      WK1( KOFF4 + KA ) = WK1( KOFF4 + KA )                                     
     1                  + SCONST(NN6)                                   M44. 752
      IF ( JGL .EQ. 1 )  GO TO 4337                                     M44. 753
 5486 JGL = 0                                                           M44. 754
 4337 IF ( IPRSUP .GT. 0 )                                              M44.2120
     1 write(*,5334)   KA,NRA,SCONST(N5),SCONST(N6),                    M44.2121
     1  SCONST(N7),FK5TDT,FK5TAU,SCONST(NN5),BM5TDT,                    M44.2122
     2  BM5TAU,SCONST(NN6),WK1(KOFF3+KA),WK1(KOFF4+KA)                  M44.2123
 5334 FORMAT ( 1X,I2,3X,I2, 11E11.5 )                                   M44.2124
 4204 CONTINUE                                                          M44.2126
      KA = KA + 1                                                               
109   CONTINUE                                                          M44.2128
      LCBL = KOFF11                                                     M44.2129
      IF (KOFF11 .GT. KOFF25+288)                                       M44.2130
     1        LMODE = 4                                                         
      KA = K                                                            M44.2133
      NQ0K=INFDLI(INOFF1+K)                                                     
      DO 110 I=1, IT2                                                   M44.2135
      IF ( IPRSUP .GT. 0 )                                              M44.2136
     1write(*,*)'Y05(KA),WK1(KOFF8+KA),WK1(KOFF7+KA),HM1(KA),HK1(KA)',  M44.2137
     2           WK1(KOFF21+KA),WK1(KOFF8+KA),WK1(KOFF7+KA),            M44.2138
     3           WK1(KOFF14+KA),WK1(KOFF13+KA)                          M44.2139
C     VMDT(KA)=FM5(KA)-Y05(KA)*HM2(KA)-HM1(KA)                          M44.2140
C     VKDT(KA)=BK5(KA)-Y05(KA)*HK2(KA)-HK1(KA)                          M44.2141
C     WK1(KOFF18+KA) = WK1( KOFF3 + KA )               !VMDT(KA)=       M44.2142
C    1         - Y05(KA) * WK1( KOFF8 + KA )                            M44.2143
C    2         - WK1(KOFF14+KA)                                         M44.2144
      WK1(KOFF18+KA) = WK1( KOFF3 + KA )                                        
     1         - WK1(KOFF21+KA) * WK1( KOFF8 + KA )                             
     2         - WK1(KOFF14+KA)                                         M44.2147
      WK1(KOFF17+KA) = WK1( KOFF4 + KA )                                        
     1         - WK1(KOFF21+KA) * WK1( KOFF7 + KA )                     M44.2149
     2         - WK1(KOFF13+KA)                                         M44.2150
      IF ( IPRSUP .GT. 0 )                                              M44.2151
     1write(*,*)'KA,FM5(KA),HM1(KA),BK5(KA),HK1(KA),VMDT(KA),VKDT(KA)', M44.2152
     2           KA,WK1( KOFF3 + KA ),WK1(KOFF14+KA),WK1( KOFF4 + KA ), M44.2153
     3                    WK1(KOFF13+KA),WK1(KOFF18+KA),WK1(KOFF17+KA)  M44.2154
      KA = KA + 1                                                       M44.2155
110   CONTINUE                                                          M44.2156
C      CALL MVEC(WK1(KOFF22+NQ0K),WK1(KOFF18+K),                        M44.2157
C     1    WK1(KOFF16+K),WK1(KOFF17+K),WK1(KOFF15+K),IT2,0)             M44.2158
      DO 1007 IM = 1, IT2                                               M44.2160
      WK1(KOFF16+K-1+IM) = 0.D0                                         M44.2161
      WK1(KOFF15+K-1+IM) = 0.D0                                         M44.2162
1007  CONTINUE                                                          M44.2163
      DO 1017 JM = 1, IT2                                               M44.2164
      DO 1027 IM = 1, IT2                                               M44.2165
      WK1(KOFF16+K-1+IM) = WK1(KOFF16+K-1+IM)                           M44.2166
     1                 + WK1(KOFF22+NQ0K) * WK1(KOFF18+K-1+JM)          M44.2167
      WK1(KOFF15+K-1+IM) = WK1(KOFF15+K-1+IM)                           M44.2168
     1                 + WK1(KOFF22+NQ0K) * WK1(KOFF17+K-1+JM)          M44.2169
      NQ0K = NQ0K + 1                                                   M44.2170
1027  CONTINUE                                                          M44.2171
1017  CONTINUE                                                          M44.2172
      IF ( IPRSUP .GT. 0 )                                              M44.2174
     1 write( *,*)'##equi. voltage sour. HM(1), HK(1)',                 M44.2175
     2                   WK1(KOFF16+1), WK1(KOFF15+1)                   M44.2176
      IF ( LINT .EQ. 1 ) GO TO 119                                      M44.2178
      KA = K                                                            M44.2179
      DO 111 I=1, IT2                                                   M44.2180
      WK1(KOFF16+KA) = WK1(KOFF16+KA) + WK1( KOFF10 + KA )              M44.2181
      WK1(KOFF15+KA) = WK1(KOFF15+KA) -  WK1( KOFF9 + KA )              M44.2182
      KA = KA + 1                                                       M44.2183
111   CONTINUE                                                          M44.2184
C                                                                       M44.2185
C        Complete updating of remaining variables:                      M44.2186
C                VM5DT,VK5DT                                            M44.2187
C                AIM5DT,AIK5DT                                          M44.2188
C                VMDT,VKDT                                              M44.2189
C                                                                       M44.2190
119   KST = K                                                           M44.2191
      DO 112 I=1, IT2                                                   M44.2192
      WK1( KOFF1 + KST ) = WK1( KOFF6 + KST )                                   
      WK1( KOFF2 + KST ) = WK1( KOFF5 + KST )                                   
      N1 = KODSEM(KST)                                                          
      NRZ = CKI(KST)                                                            
      NRA = CKKJM(KST)                                                          
      NK1 = N1 + 5*NRA + 5*NRZ + 5
      NN17 = SCONST(NK1)                                                M44. 756
      NN5 =  NK1 + 3 +NN17                     
      N6 = NN5 + 1                                                      M44.2199
      N4 = INDHST(KST)                                                  M44.2200
      SCONST(NN5) = CNVHST(N4+13)                                               
      SCONST(N6)  = CNVHST(N4+14)                                               
      IF ( IPRSUP .GT. 0 )                                              M44.2203
     1 write( *,*)'##equi. current sour. AIM5DT(I), AIK5DT(I)',         M44.2204
     2                                   SCONST(NN5), SCONST(N6)        M44.2205
C      AIM5DT(I)=AIM5(I)                                                M44.2206
C      AIK5DT(I)=AIK5(I)                                                M44.2207
      NN5 = NN5 - 2                                
      N6 = NN5 + 1                                                              
C      VMDT(I)=VM(I)                                                    M44.2210
C      VKDT(I)=VK(I)                                                    M44.2211
C      SCONST(NN5) = VM(KST)                                    !Update M44.2212
C      SCONST(N6)  = VK(KST)                                    !Update M44.2213
      NN1 = -KBUS(KST)                                                          
      NN2 = IABS( MBUS(KST) )                                           M44.2215
      SCONST(NN5) = E(NN2)                                                      
      SCONST(N6)  = E(NN1)                                                      
      IF ( IPRSUP .GT. 0 )                                              M44.2218
     1 write( *,*)'##equi. voltage sour. VMDT(I), VKDT(I)',             M44.2219
     2                            SCONST(NN5), SCONST(N6)               M44.2220
C      HMP(KST) = WK1(KOFF16+KST)                                       M44.2221
C      HKP(KST) = WK1(KOFF15+KST)                                       M44.2222
C      IF ( IPRSUP .GT. 0 )                                             M44.2223
C     1 write( *,*)'##equi. voltage sour. HMP(I), HKP(I)',              M44.2224
C     2                                   HMP(KST), HKP(KST)            M44.2225
      IF ( IPRSUP .GT. 0 )                                              M44.2226
     1 write( *,*)'##equi. voltage sour. VM5DT(I), VK5DT(I)',           M44.2227
     2                  WK1( KOFF1 + KST), WK1( KOFF2 + KST )           M44.2228
      KST = KST + 1                                                     M44.2229
112   CONTINUE                                                          M44.2230
C                                                                       M44.2231
      DO 483 I=1, IT2                                                           
      N1 = -KBUS(K)                                                             
      N2 = MBUS(K)                                                              
      N2P = IABS ( N2 )                                                 M44.2235
      F(N1) = F(N1) + WK1(KOFF15+K)                                     M44.2236
      F(N2P) = F(N2P) + WK1(KOFF16+K)                                   M44.2237
      K = K + 1                                                         M44.2238
      IF ( N2 .GT. 0 ) GO TO 483                                        M44.2239
      KCOUNT = KCOUNT + 1                                               M44.2240
C      BVALUE(KCOUNT) = AIK(I)                                          M44.2241
      KCOUNT = KCOUNT + 1                                               M44.2242
C     BVALUE(KCOUNT) = -AIM(I)                                          M44.2243
 483  CONTINUE                                                          M44.2244
      IF ( IPRSUP .GT. 0 )                                              M44.2245
     1 write (*,*) ' subts2 cable done.  k, it2 =',  k, it2             M44.2246
      IT2 = 0                                                                   
      GO TO 1200                                                                
C                    End Time step loop for frequency-dependent branchesM44.2249
C  4340 IF ( KODSEM(K) .NE. 0  .AND.                                    M44.2251
C     1     IMODEL(K) .NE. -2 )  GO TO 11780    !if this is semlyen caseM44.2252
 4340 IF ( KODSEM(K) .NE. 0  .AND.                                      M44.2253
     1     IMODEL(K) .GE. 0 )  GO TO 11780                                      
      IF ( IT2 .GT. 0 )   IT2 = 1                                               
C              TRANSFORMATION TO MODE VOLTAGES, MULTIPLIED BY NUMBER OF    13357
 1185 IT2 = IABSZ ( IT2 )                                                       
      PHSINV = UNITY / IT2                                                      
      IT1=K+1                                                                   
      II=LENGTH(IT1)                                                            
      IF ( LENGTH(K) .GT. 0 )  II = LENGTH(K)                                   
      GUS1=IT2                                                                  
      N3 = LITYPE(K)                                                            
C                                            ** M*VMODE=M*QFD(TRANSPOSEDM31.3194
      IT1=K                                                                     
      IF ( IPRSUP .GE. 3 )                                              M29.2035
     1 WRITE (LUNIT6, 21194) GUS1, N3, IT1, IT2                         M29.2036
21194 FORMAT ( 35H GUS1, N3, IT1 AND IT2 AT 21194 ARE, E12.5, 3I10 )    M29.2037
      DO 1195 I=1,IT2                                                           
      N1=-KBUS(IT1)                                                             
      N2=MBUS(IT1)                                                              
      VOLT(I)=E(N1)                                                     M29.2041
      NDX1 = LSIZ26 + I                                                 M31.3195
      VOLT(NDX1)=E(N2)                                                  M31.3196
      IF (IPRSUP .GE. 1)                                                M30. 521
     1 WRITE (LUNIT6,31194) N1, N2, E(N1), E(N2)                        M30. 522
31194 FORMAT (     3H E(, I3, 9H), AND E(, I3, 14H) AT 31194 ARE,       M32.5957
     1          2E15.7)                                                 M30. 524
 1195 IT1=IT1+1                                                                 
      DO 1197 I=1,IT2                                                           
      CI1= 0.0                                                          M31.3197
      CK1 = 0.0                                                         M31.3198
      NDX1 = LSIZ26                                                     M31.3199
      DO 1196 J=1,IT2                                                           
      YX=QFD(N3)                                                        M31.3200
      CI1  =  CI1  +  YX * VOLT(J)                                      M29.2049
      NDX1 = NDX1 + 1                                                   M31.3201
      CK1  =  CK1  +  YX * VOLT(NDX1)                                   M31.3202
 1196 N3=N3+1                                                           M29.2051
      VOLTI(I)=CI1*GUS1                                                         
 1197 VOLTK(I)=CK1*GUS1                                                         
      IF (IPRSUP .GE. 1 )                                               M29.2054
     1 WRITE (LUNIT6, 21197) (VOLT(I),VOLTI(I),VOLTK(I),I=1,IT2)        M31.3203
21197 FORMAT (51H (VOLT(I),         VOLTI(I), VOLTK(I), I=1,IT2) ARE,   M31.3204
     1    /,  (1X, 9E14.6) )                                            M31.3205
      NDX1 = LSIZ26 + 1                                                 M31.3206
      NDX2 = LSIZ26 + IT2                                               M31.3207
      IF ( IPRSUP  .GE.  1 )                                            M31.3208
     1 WRITE (LUNIT6, 31197)  ( VOLT(I), I=NDX1, NDX2 )                 M31.3209
31197 FORMAT (  6H VOLT:,  8E16.8  )                                    M35.1612
      GUS1=UNITY                                                        M29.2058
C              UPDATE MODE QUANTITIES AND FIND PHASE VECTORS I FOR RIGHTM29.2059
      CI1=0                                                             M29.2060
      IF ( MODOUT  .LE.  0 )   GO TO 4641                               M11. 291
      DO 4633  I=1, MODOUT                                              M11. 294
      N7 = NMODAL + I                                                   M35.1613
      BVALUE(N7) = VOLTI(I) * PHSINV                                            
      N8 = N7 + MODOUT                                                  M16.2926
 4633 BVALUE(N8) = VOLTK(I) * PHSINV                                            
      IF ( IPRSUP  .GE.  1 )                                            M28.5506
     1 WRITE (LUNIT6, 4638)  NV, IT2, K,  (VOLTI(I), VOLTK(I), I=1,     M22.4942
     2 IT2 )                                                            M32.5958
 4638 FORMAT ( /,  29H MODE VOLTAGES IN  'SUBTS2' .  , 3I10, /,         M11. 299
     1 ( 1X, 8E16.5 ) )                                                 M11. 300
C              UPDATE MODE QUANTITIES AND FIND PHASE VECTORS I FOR RIGHT   13407
 4641 CK1=0.                                                                    
      MSIGN=K                                                                   
      I = 0                                                             M16.2928
41189 I = I + 1                                                                 
      IF (IMODEL(MSIGN) .EQ. -2)  GO TO 10125                           M31.3211
      N3=NR(MSIGN)                                                              
      IF(N3.LT.0) GO TO 1187                                                    
      IT1=II+IPOINT                                                             
      N4=IT1+N3                                                                 
      IF(IT1.GT.LPAST)IT1=IT1-LPAST                                             
      IF(N4.GT.LPAST)N4=N4-LPAST                                                
      IT21=IT1+1                                                                
      N41=N4+1                                                                  
      IF(IT21.GT.LPAST)IT21=1                                                   
      IF(N41.GT.LPAST)N41=1                                                     
      XMN4=XM(N4)                                                               
      A=CIK(MSIGN)                                                         13424
      D2=1.0-A                                                             13425
      GUS2=CI(MSIGN)                                                            
      H1=ABSZ(GUS2)                                                             
      H2 = ABSZ(CK(MSIGN))                                                      
c                                                                       M44.2256
c                                   current output for single distributeM44.2257
c                                                                       M44.2258
      IF( N2 .GT. 0 ) GO TO  3811                                               
      N2 = - N2                                                                 
      BUS4 = 1. + H2                                                    M44.2261
      IF( BUS2 .LT. 0. ) BUS4 = 2.0 * H2                                M44.2262
      BUS4 = H1*E(N1)/BUS4-XK(N4)                                       M44.2263
      KCOUNT = KCOUNT + 1                                                       
      BVALUE(KCOUNT)=BUS4                                                       
c                                                                       M44.2266
 3811 IF( IPRSUP .GT. 8 )                                                       
     1 WRITE(LUNIT6,31192) I, MSIGN, IT1, IT21, CIK(MSIGN), CI(MSIGN),  M32.5960
     2          CK(MSIGN), XM(IT1), XK(IT1), XM(IT21), XK(IT21)         M32.5961
31192 FORMAT(  47H0IN "SUBTS2".  I, MSIGN, IT1, IT21, CIK(MSIGN),,      M32.5962
     1         50H CI(MSIGN), CK(MSIGN), XM(IT1), XK(IT1), XM(IT21),,   M32.5963
     2         22H XK(IT21) FOLLOW .....   ,/,  1X,  4I5,  7E15.6 )     M32.5964
      XM(N4)=H1*VOLTI(I)-H2*XK(N4)                                              
      XK(N4)=H1*VOLTK(I)-H2*XMN4                                                
      IF (CK(MSIGN).LT.0.0) GO TO 20510                                         
      H1=XK(IT1)*A+XK(IT21 )*D2                                                 
      D2=XM(IT1)*A+XM(IT21 )*D2                                                 
      IF( GUS2 .LT. 0.0 )  GO TO 1192                                   M05. 681
      A = H1 + D2                                                               
      D2 = ( H1 - D2 ) * H2                                                13436
      H1 = ( A + D2 ) / 2.0                                             M05. 682
      D2 = ( A - D2 ) / 2.0                                             M05. 683
 1192 XK(N41 )=H1                                                          13439
      XM(N41)=D2                                                           13440
      IF( IPRSUP .GT. 6 )                                               M32.5965
     1 WRITE (LUNIT6,1193) I, MSIGN, IT1, N41, (XK(J),XM(J),J=IT1,N41)  M32.5966
 1193 FORMAT(  38H0IN "SUBTS2".  I, MSIGN, IT1, N41, AND,               M32.5967
     1         41H (XK(J), XM(J), J=IT1, N41)  FOLLOW  ....,            M37.5326
     2   5X,  4I5  ,/,  ( 1X,  5E25.16 )  )                             M32.5969
 1188 II = II + N3 + 2                                                          
CJ                                                                      M31.3213
      GO TO 10130                                                       M31.3214
C                                                                       M31.3215
C                  CALCULATE EQUIVALENT CURRENT SOURCES                 M31.3216
C                  AND UPDATE HISTORY VECTORS FOR F.D.                  M31.3217
C                  MULTI-PHASE LINES                                    M31.3218
C                                                                       M31.3219
10125 VKD=VOLTI(I) * PHSINV                                                     
      VMD=VOLTK(I) * PHSINV                                                     
      NRZ = CKI(MSIGN)                                                          
      NRA = CKKJM(MSIGN)                                                        
      N11 = INDHST(MSIGN)                                                       
      N1 = CNVHST(N11) * DLTINV + 2.0                                           
      N2 = KODSEM(MSIGN)                                                        
      NRAZ1 = NRA + NRZ                                                 M39.2910
      NRAZ2 = NRAZ1 + NRAZ1                                                     
      NRAZ3 = NRAZ2 + NRAZ1                                                     
      NRA3  = NRA * 3                                                   M39.2913
      NRZ2  = NRZ + NRZ                                                 M39.2914
      NRZ3  = NRZ2 + NRZ                                                M39.2915
      N31 = N2 + NRAZ2                                                          
      N32 = NRAZ3 + 4                                                   M39.2917
      N33 = N31 + N32                                                           
      N34 = N33 + NRZ2                                                          
      N3 = N31 + NRZ3 + 1                                                       
      N7 = N3 + NRA3                                                            
      N10 = N3 + 1                                                              
C                                           N10 is where first cell of CM44.2267
      IND = NR(MSIGN) + IPOINT - 1                                      M31.3230
      IF (IND .GT. LPAST)  IND = IND - LPAST                            M31.3231
      D23 = 1.0 / SCONST(N10)                                                   
      IF ( T .EQ. 0.0 )   GO TO  81000                                          
C                                                                     !tM44.2268
C     UPDATE HISTORY VECTORS                                            M31.3233
      AIKD = VKD * D23 - CNVHST(N11+13)                                         
      AIMD = VMD * D23 - CNVHST(N11+14)                                         
      DO 80110  II = 1, NRZ                                                     
      NN1 = N31 + II                                                            
      NN2 = NN1 + NRZ                                                           
      NN3 = NN2 + NRZ                                                           
      NN4 = NN1 + N32                                                           
      NN5 = NN4 + NRZ                                                           
C                                    eki= mi*eki(t-DT) + pi*ik(t) + qi*iM44.2269
C                                                      SCONST(NN4) was eM44.2270
C                                                     CNVHST(N11+5) was M44.2271
C                                                                 AIKD wM44.2272
C                                                                       M44.2273
C     ek(t) = SUM eki(t)                                                M44.2274
C           = SUM ! mi*eki(t-DT) + Pi*ik(t) + Qi*ik(t-DT) }             M44.2275
C                                                                       M44.2276
      SCONST(NN4) = SCONST(NN2) * AIKD + SCONST(NN3) * CNVHST(N11+5)    M31.3242
     1            + SCONST(NN1) * SCONST(NN4)                           M31.3243
      SCONST(NN5) = SCONST(NN2) * AIMD + SCONST(NN3) * CNVHST(N11+6)    M31.3244
     1            + SCONST(NN1) * SCONST(NN5)                           M31.3245
80110 CONTINUE                                                                  
      DO 80120  II = 1, NRA                                             M31.3247
      NN6 = N10 + II                                                            
      NN7 = NN6 + NRA                                                           
      NN8 = NN7 + NRA                                                           
      NN9 = N34 + II                                                            
      NN10 = NN9+NRA                                                            
c                                                                       M44.2277
c                                                                       M44.2278
c          bk(t) = SUM                                                          
c                                                                       M44.2280
c                                                                       M44.2281
      SCONST(NN9) = SCONST(NN6) * SCONST(NN9) + SCONST(NN7) *           M31.3253
     1              CNVHST(N11+8) + SCONST(NN8) * CNVHST(N11+7)         M31.3254
      SCONST(NN10) = SCONST(NN6) * SCONST(NN10) + SCONST(NN7) *         M31.3255
     1               CNVHST(N11+10) + SCONST(NN8) * CNVHST(N11+9)       M31.3256
80120 CONTINUE                                                          M31.3257
      CNVHST(N11+5) = AIKD                                                      
      CNVHST(N11+6) = AIMD                                                      
      NRF = IND + N1 - 1                                                M31.3260
      IF ( NRF .GT. LPAST)  NRF = NRF - LPAST                           M31.3261
      XK(NRF) = 2.0 * VKD - CNVHST(N11+11)                                      
      XM(NRF) = 2.0 * VMD - CNVHST(N11+12)                                      
      IF ( IPRSUP .EQ. 0)  GO TO 81000                                  M31.3264
      WRITE (LUNIT6,80135) T, I                                         M31.3265
80135 FORMAT ( 8H AT T = , E12.5, 17H FOR MARTI BRANCH,                 M31.3266
     1 I4,/, 16H HISTORY VECTORS )                                      M31.3267
      J1 = NN4 - NRZ + 1                                                M31.3268
      J2 = NN5 - NRZ + 1                                                M31.3269
      J3 = NN9 - NRA + 1                                                M31.3270
      J4 = NN10 - NRA + 1                                               M31.3271
      WRITE (LUNIT6, 80140) J1, NN4                                     M31.3272
80140 FORMAT( 12H SCONST FROM, I6, 6H    TO, I6 )                       M31.3273
      WRITE (LUNIT6, 80145) (SCONST(II),II=J1,NN4)                      M31.3274
80145 FORMAT (1X, 10E12.5)                                              M31.3275
      WRITE (LUNIT6, 80140)  J2, NN5                                    M31.3276
      WRITE (LUNIT6, 80145) (SCONST(II),II=J2,NN5)                      M31.3277
      WRITE (LUNIT6,80140)  J3, NN9                                     M31.3278
      WRITE (LUNIT6, 80145) (SCONST(II),II=J3,NN9)                      M31.3279
      WRITE (LUNIT6, 80140) J4, NN10                                    M31.3280
      WRITE (LUNIT6, 80145) (SCONST(II),II=J4,NN10)                     M31.3281
      WRITE (LUNIT6, 80150) IND, NRF                                    M31.3282
80150 FORMAT (/, 39H FORWARD FUNCTIONS XK AND XM; POSITIONS  ,          M31.3283
     1   I6, 4H  TO, I6)                                                M31.3284
      WRITE (LUNIT6,80145) (XK(II),II=IND,NRF),(XM(II),II=IND,NRF)      M31.3285
C                                                                       M44.2282
C                                                                       M44.2283
C     EQUIVALENT CURRENT SOURCES                                        M31.3286
81000 EKH = SCONST(N3) * CNVHST(N11+5)                                          
      EMH = SCONST(N3) * CNVHST(N11+6)                                          
      DO 81040  II = 1, NRZ                                                     
      N4 = N31 + II                                                             
      N5 = N33 + II                                                             
      N6 = N5 + NRZ                                                             
C                                              !ekhv(t)=SUM(mi)*eki(t-DtM44.2284
C                                              !emhv(t)=SUM(mi)*emi(t-DtM44.2285
      EKH = EKH + SCONST(N4) * SCONST(N5)                                       
      EMH = EMH + SCONST(N4) * SCONST(N6)                                       
81040 CONTINUE                                                                  
C                                                                       M44.2286
C     INTERPOLATION PROCESS FOR LAST POINT OF FORWARD FUNCTIONS         M31.3296
C                                                                       M44.2287
      TEMP1 = CNVHST(N11) * DLTINV                                              
      ITAUO = TEMP1                                                             
      FRAC = 1.0 - ( TEMP1 - ITAUO)                                             
      I1 = IND                                                          M31.3300
      I1P1 = I1 + 1                                                     M33.1535
      IF ( I1P1 .GT. LPAST )   I1P1 = I1P1 - LPAST                      M33.1536
      I1P2 = I1 + 2                                                     M33.1537
      IF ( I1P2 .GT. LPAST )   I1P2 = I1P2 - LPAST                      M33.1538
C                                                                       M44.2288
C                                        Xk=Fk>>>>(Vk+Zc*Ik)  coming froM44.2289
C                                        Xm=Fm>>>>(Vm+Zc*Im)  coming froM44.2290
C                                                                       M44.2291
      CNVHST(N11+7) = (XM(I1P1)-XM(I1)) * FRAC + XM(I1)                         
      CNVHST(N11+8) = (XM(I1P2)-XM(I1P1)) * FRAC + XM(I1P1)                     
      CNVHST(N11+9) = (XK(I1P1)-XK(I1)) * FRAC + XK(I1)                         
      CNVHST(N11+10) =(XK(I1P2)-XK(I1P1)) * FRAC + XK(I1P1)             M33.1542
C                                                                       M44.2292
C               bk(t) = SUMgi*bki(t-DT) + c*fm(t-Tau) + d*fm(t-Tau-DT)  M44.2293
C                         *************>>will be done in the loop 81050 M44.2294
C                                                                       M44.2295
      CNVHST(N11+11) = SCONST(N7+2) * CNVHST(N11+8)  +                          
     1                 SCONST(N7+3) * CNVHST(N11+7)                             
      CNVHST(N11+12) = SCONST(N7+2) * CNVHST(N11+10) +                          
     1                 SCONST(N7+3) * CNVHST(N11+9)                             
      DO 81050 II = 1, NRA                                                      
      N8 = N10 + II                                                             
      N9 = N34 + II                                                     M39.2936
C                                                                       M44.2296
C                bk(t) =SUB gi*bki(t-DT) + c*fm(t-Tau) + d*fm(t-Tau-DT) M44.2297
C                           *being done* + ^^^^^already done above^^^^^ M44.2298
C                                                                       M44.2299
      CNVHST(N11+11) = CNVHST(N11+11) + SCONST(N8) * SCONST(N9)         M31.3312
      CNVHST(N11+12) = CNVHST(N11+12) + SCONST(N8) * SCONST(N9+NRA)     M31.3313
81050 CONTINUE                                                                  
      CNVHST(N11+13) = (EKH + CNVHST(N11+11)) * D23                             
      CNVHST(N11+14) = (EMH + CNVHST(N11+12)) * D23                             
      H1 = CNVHST(N11+13)                                               M31.3317
      D2 = CNVHST(N11+14)                                               M31.3318
      IF (IPRSUP .EQ. 0)  GO TO 10130                                   M31.3319
      WRITE (LUNIT6,81060) T, I                                         M31.3320
81060 FORMAT(/, 8H AT T = , E12.5, 17H FOR MARTI BRANCH, I4,            M31.3321
     1  48H   EQUIV. CIRCUIT, VOLTAGES AND CURRENT SOURCES: )           M31.3322
      WRITE (LUNIT6, 81070) EKH, EMH, CNVHST(N11+11),                   M31.3323
     1         CNVHST(N11+12),CNVHST(N11+13),CNVHST(N11+14)             M31.3324
81070 FORMAT( 1X, 6E16.7)                                               M31.3325
10130 VOLTI(I)= H1*GUS1                                                         
      VOLTK(I)=D2*GUS1                                                          
      MSIGN=MSIGN+1                                                             
      GO TO 1189                                                                
 1187 H1=0.                                                                     
      D2=0.                                                                     
      GO TO 1188                                                           13450
20510 KF = MSIGN                                                                
      JF = IT1                                                                  
20520 KF = CIK(KF)                                                              
      IFDEP2 = 2*KF - 1                                                    13459
      JFDEP2 = 2 * LFDEP + IFDEP2                                       M35.1614
      KFDEP2 = 4 * LFDEP + IFDEP2                                       M35.1615
      SK = 0.0                                                                  
      SM = 0.0                                                                  
      IK = N4                                                              13463
      IM = IK + LPAST                                                      13464
      IKLIM = 0                                                            13465
 5140 L = IWTENT(IFDEP2)                                                        
      LBOUND = IWTENT(IFDEP2+1)                                                 
 5180 IK1 = IK - 1                                                              
      IM1 = IM - 1                                                              
      IF( IK1 .GT. IKLIM )  GO TO 5186                                          
      IK1 = IK1 + LPAST                                                         
      IM1 = IM1 + LPAST                                                    13472
 5186 IF ( IM  .LE.  IK )   GO TO 35186                                         
      XXIK = XK(IK)                                                             
      XXIM = XM(IK)                                                             
      GO TO 45186                                                       M28.5511
35186 XXIK = XM(IM)                                                     M28.5512
      XXIM = XK(IM)                                                     M28.5513
45186 SK = SK  +  WEIGHT(L) * XXIM                                              
      SM = SM  +  WEIGHT(L) * XXIK                                              
C     PRECEDING CODE REPLACES FOLLOWING TWO ORIGINAL RECORDS.           M28.5516
C     IT IS A PATCH, TO OBEY  COMMON  BOUNDARIES FOR BURROUGHS.         M28.5517
C5186 SK = SK + WEIGHT(L) * XX(IM)                                      M28.5518
C     SM = SM + WEIGHT(L) * XX(IK)                                      M28.5519
      IF( IPRSUP .GE. 8 )                                                  13475
     1WRITE(LUNIT6, 7129)  L, IK, IM, IK1, IM1, IKLIM, LBOUND, SK, SM,  M17. 881
     1 WEIGHT(L), XXIK, XXIM                                            M28.5520
 7129 FORMAT ( /,  13H PARTIAL SUM.,                                    M22.4944
     1 56H       L      IK      IM     IK1     IM1   IKLIM  LBOUND,     M22.4945
     2 /,  13X,  7I8  ,/,  13X,  18X,  2HSK,  18X,  2HSM,               M22.4946
     3 11X,  9HWEIGHT(L),  16X,  4HXXIK,  16X,  4HXXIM   ,/,            M28.5521
     4 13X,  5E20.11 )                                                  M22.4948
      L = L + 1                                                            13479
      IF( L .GE. LBOUND )  GO TO 5250                                      13480
      IK = IK1                                                             13481
      IM = IM1                                                             13482
      GO TO 5180                                                           13483
 5250 IF ( IM1  .LE.  IK1 )   GO TO 35250                               M28.5522
      XXIK1 = XK(IK1)                                                   M28.5523
      XXIM1 = XM(IK1)                                                   M28.5524
      GO TO 45250                                                       M28.5525
35250 XXIK1 = XM(IM1)                                                   M28.5526
      XXIM1 = XK(IM1)                                                   M28.5527
45250 IFTAIL = IFTAIL + 1                                               M28.5528
C     PRECEDING CARDS ARE REPLACEMENT FOR TWO CARDS BELOW.              M28.5529
C     TEMPORARY PATCH TO OBSERVE  COMMON  BOUNDARIES.                   M28.5530
C5250 IFTAIL = IFTAIL + 1                                               M28.5531
C     DK = CON1(IFDEP2) * XX(IM)  +  CON1(JFDEP2) * XX(IM1)  +          M35.1616
      DK = CON1(IFDEP2) * XXIM    +  CON1(JFDEP2) * XXIM1    +          M35.1617
     1  CON1(KFDEP2) * STAILK(IFTAIL)                                           
      IF( IPRSUP .GE. 8 )                                                  13487
     1WRITE(LUNIT6, 7131)  STAILK(IFTAIL), STAILM(IFTAIL), CON1(IFDEP2),M35.1619
     1 CON1(JFDEP2), CON1(KFDEP2), XXIK, XXIK1, XXIM, XXIM1             M35.1620
 7131 FORMAT ( /,  15H STAILK UPDATE.,  6X,  14HSTAILK(IFTAIL),         M22.4949
     1 6X,  14HSTAILM(IFTAIL),  8X,  12HCON1(IFDEP2),                   M22.4950
     2 8X,  12HCON1(JFDEP2),  8X,  12HCON1(KFDEP2)  ,/,                 M35.1621
     3 15X,  5E20.11  ,/,  15X,  16X,  4HXXIK,  15X,  5HXXIK1,          M28.5535
     4 16X,  4HXXIM,  15X,  5HXXIM1  ,/,  15X,  5E20.11 )               M28.5536
      STAILK(IFTAIL) = DK                                                       
      SK = SK + DK                                                              
C     DM = CON1(IFDEP2) * XX(IK)  +  CON2(IFDEP2) * XX(IK1)  +          M35.1622
      DM = CON1(IFDEP2) * XXIK    +  CON1(JFDEP2) * XXIK1    +          M35.1623
     1  CON1(KFDEP2) * STAILM(IFTAIL)                                   M35.1624
      STAILM(IFTAIL) = DM                                                       
      SM = SM + DM                                                              
      NDELT = T / DELTAT                                                   13497
      IF( NDELT .LT. 10     .AND.    IPRSUP .GE. 2 )                       13498
     1WRITE(LUNIT6, 5257)  K, IFDEP2, IFTAIL,  L, IK, IM,               M17. 883
     2 DK, DM, SK, SM                                                      13500
 5257 FORMAT ( /,  18H CONVOLUTION DONE.,                               M22.4954
     1 48H       K  IFDEP2  IFTAIL       L      IK      IM,             M22.4955
     2 14X,  2HDK,  14X,  2HDM,  14X,  2HSK,  14X,  2HSM  ,/,           M22.4956
     3 18X,  6I8,  4E16.7  )                                            M22.4957
      IFDEP2 = IFDEP2 + 1                                                  13502
      JFDEP2 = JFDEP2 + 1                                               M35.1625
      KFDEP2 = KFDEP2 + 1                                               M35.1626
      IF( IK .GT. IM )  GO TO 5300                                         13503
      IM = N4  -  ( ISKIP(KF)-1 )                                          13504
      IF ( IM  .LE.  0 )   IM = IM + LPAST                              M11. 301
      IK = IM + LPAST                                                      13505
      IKLIM = LPAST                                                        13506
      GO TO 5140                                                           13507
 5300 IF( NDELT .LT. 10    .AND.    IPRSUP .GE. 2 )                        13508
     1WRITE(LUNIT6, 5310)  L,    KF, IFDEP2, K, IFTAIL, SK, SM          M17. 884
 5310 FORMAT ( /,  16H DONE F.D. MODE.,                                 M22.4958
     1 40H       L      KF  IFDEP2       K  IFTAIL,  18X,               M22.4959
     2 2HSK,  18X,  2HSM  ,/,  16X,  5I8,  2E20.11  )                   M22.4960
       GO TO 1192                                                       M30. 525
 1189 IF ( I  .LT.  IT2 )   GO TO 41189                                         
      J = LITYPE(K)                                                             
C*** IPHASE=TI*IMODE                                                    M29.2062
      N3=J-1                                                            M29.2063
      DO 1203 J=1,IT2                                                           
      H1=0.                                                             M29.2065
      H2=0.                                                             M29.2066
      N3=N3+1                                                           M29.2067
      N4=N3                                                             M29.2068
      DO 1199 I=1,IT2                                                   M29.2069
      YX=QFD(N4)                                                                
      H1=H1+YX*VOLTI(I)                                                         
      H2=H2+YX*VOLTK(I)                                                         
 1199 N4=N4+IT2                                                         M29.2073
      N1=-KBUS(K)                                                               
      N2=MBUS(K)                                                                
      F(N1)=F(N1)+H1                                                            
      F(N2)=F(N2)+H2                                                            
      IF (IPRSUP .GE. 3 )                                               M29.2078
     1 WRITE (LUNIT6, 21199) J, IT2,N1, N2, H1, H2, F(N1),F(N2)         M29.2079
21199 FORMAT ( 96H       J     IT2      N1      N2              H1      M29.2080
     1        H2           F(N1)           F(N2),/, 4I8, 4(1X,E15.6) )  M29.2081
 1203 K=K+1                                                                     
C *** END OF CONVERSION TO IPHASE FOR UNTRANSPOSED OPTION               M29.2083
 1204 IT2=0                                                             M29.2084
      GO TO 1200                                                           13525
C                                                                       M23.3932
C  RECURSIVE CONVOLUTION MODELS                                         M23.3933
C                                                                       M23.3934
11780 IT2 = IABS(KODEBR(K))                                             M23.3935
      PHSINV = UNITY / IT2                                                      
      N2 = CIK(K)                                                       M23.3936
C  DEFINE THE TERMINAL VOLTAGE IN SEMAUX(0)(SEMAUX(IT2)) FOR KBUS(MBUS) M23.3937
      DO 11788 I=1, IT2                                                 M23.3938
      IF (N2 .LE. 0) GO TO 11784                                        M23.3939
      D1 = 0.0                                                          M23.3940
      D2 = 0.0                                                          M23.3941
      DO 11782 J=1, IT2                                                 M23.3942
      II = J + K - 1                                                    M23.3943
      N3 = - KBUS(II)                                                   M23.3944
      D1 = D1 + QFD(N2) * E(N3)                                         M23.3945
      N3 = IABS(MBUS(II))                                               M23.3946
      D2 = D2 + QFD(N2) * E(N3)                                         M23.3947
      N2 = N2 + 1                                                       M23.3948
11782 CONTINUE                                                          M23.3949
      GO TO 11786                                                       M23.3950
11784 II = I + K - 1                                                    M23.3951
      N3 = - KBUS(II)                                                   M23.3952
      D1 = E(N3)                                                        M23.3953
      N3 = IABSZ(MBUS(II))                                              M23.3954
      D2 = E(N3)                                                        M23.3955
11786 N3 = IT2 + I                                                      M23.3956
      SEMAUX(I) = D1                                                    M23.3957
      SEMAUX(N3) = D2                                                   M23.3958
      VOLT(I) = 0.0                                                     M23.3959
      VOLTI(I) = 0.0                                                    M23.3960
      VOLTK(I) = 0.0                                                    M23.3961
      NDX1 = LSIZ26 + I                                                 M31.3329
      VOLT(NDX1) = 0.0                                                  M31.3330
11788 CONTINUE                                                          M23.3963
      IF (IPRSUP .LT. 9) GO TO 23200                                    M23.3964
      N3 = IT2 + IT2                                                    M23.3965
      WRITE (LUNIT6, 23210) K, IT2, CIK(K), (SEMAUX(I), I=1, N3)        M23.3966
23210 FORMAT(/  64H  AFTER CALCULATION OF MODAL/PHASE VOLTAGE VECTOR IN M23.3967
     1'SUBTS2'.   ,I10,I10,F10.0,/,(8(1X,E15.7)))                       M23.3968
23200 II = K                                                            M23.3969
      IF (KODSEM(K) .GT. 0) GO TO 11824                                 M23.3970
C  CALCULATION OF BRANCH(VOLT) AND NORTON(VOLTI) I'S FOR LUMPED ELEMENTSM23.3971
11790 N2 = ABSZ(CKI(II)) - 1.0                                          M23.3972
      N3 = N2 / IT2                                                     M39.2940
      N2 = N2 - N3 * IT2 + 1                                            M39.2941
      N4 = CK(II)                                                       M23.3975
      N5 = INDHST(II)                                                   M23.3976
      N6 = - KODSEM(II)                                                 M23.3977
      N3 = N3 + 1                                                       M39.2942
      N7 = N3 + IT2                                                     M23.3978
      D1 = SEMAUX(N3) - SEMAUX(N7)                                      M23.3979
      D4 = SCONST(N6 + 1) * D1                                          M23.3980
      D2 = D4 - CNVHST(N5)                                              M23.3981
      D3 = - D2 - D4                                                    M23.3982
      CNVHST(N5) = - D3                                                 M23.3983
      D2 = SCONST(N6) * D1 + D2                                         M23.3984
      IF (N4) 11804, 11814, 11792                                       M23.3985
C  EXPONENTIAL REPRESENTATION OF THE TRANSFER FUNCTION.  COMPLETE CONVOLM23.3986
C  UTION TO GET CURRENT AT T=T0, THEN PREFORM PARTIAL CONV. TO GET I-NORM23.3987
11792 IF (IPRSUP .GE. 9)                                                M23.3988
     1     WRITE (LUNIT6, 23040) N2, N3, N4, N5, N6, D1, D2, D3         M23.3989
23040 FORMAT(//, 14H  AT 11792.    ,5I10,3(1X,E15.7))                   M23.3990
      DO 11802 J=1, N4                                                  M23.3991
      D4 = CNVHST(N5 + 1) + D1 * SCONST(N6 + 4)                         M23.3992
      IF (IPRSUP .GE. 9)                                                M23.3993
     1     WRITE (LUNIT6, 23030)  CNVHST(N5 + 1), D4                    M23.3994
23030 FORMAT( 57H  TOP OF LUMPED ELEMENT EXPONENTIAL CONVOLUTION LOOP.  M23.3995
     1    ,2(2X,E15.8))                                                 M23.3996
      IF (SCONST(N6 + 2)) 11794, 11796, 11798                           M23.3997
11794 CNVHST(N5 + 1) = - SCONST(N6 + 3) * CNVHST(N5) +                  M23.3998
     1          SCONST(N6 - 1) * D4 + SCONST(N6 + 5) * D1               M23.3999
      D4 = SCONST(N6 - 1) * CNVHST(N5) + SCONST(N6 + 3) * D4            M23.4000
     1          + SCONST(N6 + 1) * D1                                   M23.4001
      D3 = D3 + D4 + D4                                                 M23.4002
      CNVHST(N5) = D4                                                   M23.4003
      GO TO 11800                                                       M23.4004
11796 D2 = D2 + D4                                                      M23.4005
      D4 = D4 * SCONST(N6 + 3) + D1 * SCONST(N6 + 5)                    M23.4006
      D3 = D3 + D4                                                      M23.4007
      CNVHST(N5 + 1) = D4                                               M23.4008
      GO TO 11800                                                       M23.4009
11798 D2 = D2 + D4 + D4                                                 M23.4010
      CNVHST(N5 + 1) = D4                                               M23.4011
11800 N6 = N6 + 5                                                       M23.4012
      N5 = N5 + 1                                                       M23.4013
      IF (IPRSUP .GE. 9)                                                M23.4014
     1     WRITE (LUNIT6, 23020) J, N5, N6, CNVHST(N5), D2, D3, D4      M23.4015
23020 FORMAT( 23H  IN EXP. CONV. LOOP.   ,3I10, 4(1X,E15.7))            M23.4016
11802 CONTINUE                                                          M23.4017
      GO TO 11814                                                       M23.4018
C  PIECEWISE-LINEAR REPRESENTATION OF THE STEP FUNCTION.  COMPLETE CONV M23.4019
C  AT T=T0 TO GET NORTON I.  UPDATE CONV. HISTORY GO GET BRANCH I.      M23.4020
11804 D4 = CNVHST(N5 + 1) + D1 * SCONST(N6 + 3)                         M23.4021
      D2 = D2 + D4                                                      M23.4022
      N7 = NR(II)                                                       M23.4023
      N8 = IPOINT + IABS(N7) + LENGTH(II) - 3                           M23.4024
      IF (N8 .GT. LPAST) N8 = N8 - LPAST                                M23.4025
      N4 = N6 - N4 - N4 - 4                                             M23.4026
      D5 = 0.0                                                          M23.4027
      IF (N7 .LT. 0) GO TO 11808                                        M23.4028
C  PAST HISTORY OF CONVOLUTION INPUT IS STORED IN 'XK'.                 M23.4029
      XK(N8) = D1                                                       M23.4030
      N8 = N8 + 1                                                       M23.4031
      DO 11806 J=N6, N4, 2                                              M23.4032
      D6 = SCONST(J + 4) * DLTINV                                       M39.2943
      N7 = D6                                                           M23.4034
      D6 = D6 - N7                                                      M23.4035
      N7 = N8 - N7                                                      M23.4036
      IF (N7 .LE. 0) N7 = N7 + LPAST                                    M23.4037
      N9 = N7 - 1                                                       M23.4038
      IF (N9 .LE. 0) N9 = N9 + LPAST                                    M23.4039
      N10 = N9 - 1                                                      M23.4040
      IF (N10 .LE. 0) N10 = N10 + LPAST                                 M23.4041
      D6 = XK(N7) + XK(N9) - D6 * (XK(N7) - XK(N10))                    M23.4042
      D4 = D4 + SCONST(J + 3) * (D5 - D6)                               M23.4043
      D5 = D6                                                           M23.4044
11806 CONTINUE                                                          M23.4045
      IF (IPRSUP .LT. 9) GO TO 11812                                    M23.4046
      N7 = IABS(NR(II)) + IPOINT - 1                                    M23.4047
      IF (N7 .GT. LPAST) N7 = N7 - LPAST                                M23.4048
      WRITE (LUNIT6, 23010) N2, N3, N4, N5, N6, N7, N8, N9, N10, D1, D2,M23.4049
     1          D3, D4, D5, D6, CNVHST(N5), CNVHST(N5+1),               M23.4050
     2          (XK(J), J=N7, N8)                                       M23.4051
23010 FORMAT(/     62H  AFTER LINEAR CONV.  N2, N3, N4, N5, N6, N7, N8, M23.4052
     1 N9, N10 =  ,9I7/5X, 28H  D1, D2, D3, D4, D5, D6 =   ,6(1X,E15.7) M23.4053
     2  ,/, 53HCNVHST(N5), CNVHST(N5+1), (XK/XM(J) J=N7, N8)  ...   ,   M23.4054
     3   /,(8(1X,E15.7)))                                               M23.4055
      GO TO 11812                                                       M23.4056
C  PAST HISTORY OF CONVOLUTION INPUT IS STORED IN 'XM'.                 M23.4057
11808 XM(N8) = D1                                                       M23.4058
      N8 = N8 + 1                                                       M23.4059
      DO 11810 J=N6, N4, 2                                              M23.4060
      D6 = SCONST(J + 4) * DLTINV                                       M39.2944
      N7 = D6                                                           M23.4062
      D6 = D6 - N7                                                      M23.4063
      N7 = N8 - N7                                                      M23.4064
      IF (N7 .LE. 0) N7 = N7 + LPAST                                    M23.4065
      N9 = N7 - 1                                                       M23.4066
      IF (N9 .LE. 0) N9 = N9 + LPAST                                    M23.4067
      N10 = N9 - 1                                                      M23.4068
      IF (N10 .LE. 0) N10 = N10 + LPAST                                 M23.4069
      D6 = XM(N7) + XM(N9) - D6 * (XM(N7) - XM(N10))                    M23.4070
      D4 = D4 + SCONST(J + 3) * (D5 - D6)                               M23.4071
      D5 = D6                                                           M23.4072
11810 CONTINUE                                                          M23.4073
      IF (IPRSUP .LT. 9) GO TO 11812                                    M23.4074
      N7 = IABS(NR(II)) + IPOINT - 1                                    M23.4075
      IF (N7 .GT. LPAST) N7 = N7 - LPAST                                M23.4076
      WRITE (LUNIT6, 23010) N2, N3, N4, N5, N6, N7, N8, N9, N10, D1, D2,M23.4077
     1          D3, D4, D5, D6, CNVHST(N5), CNVHST(N5+1),               M23.4078
     2          (XM(J), J=N7, N8)                                       M23.4079
11812 D4 = D4 + SCONST(N4 + 5) * D5                                     M23.4080
      CNVHST(N5 + 1) = D4                                               M23.4081
      D3 = D3 + D4                                                      M23.4082
11814 VOLT(N2) = VOLT(N2) + D2                                          M23.4083
      VOLTI(N2) = VOLTI(N2) + D3                                        M23.4084
      II = II + 1                                                       M23.4085
      IF (CKI(II - 1) .GT. 0) GO TO 11790                               M23.4086
      IF (MODOUT .LE. 0) GO TO 11863                                    M29.2085
      DO 21863  I = 1, MODOUT                                           M29.2086
      N7 = NV + I                                                       M29.2087
      BVALUE(N7) = VOLT(I)                                              M29.2088
      N8 = N7 + MODOUT                                                  M29.2089
      BVALUE(N8) = VOLTI(I)                                             M29.2090
21863 CONTINUE                                                          M29.2091
C  INCREMENT F TO REFLECT NORTON CONTRIBUTION OF THIS BRANCH.  COMPUTE  M23.4087
C  BRANCH CURRENT IF REQUESTED BY MBUS(.)                               M23.4088
11863 N2 = CIK(K)                                                       M29.2092
      DO 11822 I=1, IT2                                                 M23.4090
      II = I + K - 1                                                    M23.4091
      N3 = - KBUS(II)                                                   M23.4092
      N4 = MBUS(II)                                                     M23.4093
      IF (N2 .GT. 0) GO TO 11818                                        M23.4094
      IF (N4 .GT. 0) GO TO 11816                                        M23.4095
      N4 = - N4                                                         M23.4096
      KCOUNT = KCOUNT + 1                                               M23.4097
      BVALUE(KCOUNT) = VOLT(I)                                          M23.4098
11816 F(N3) = F(N3) - VOLTI(I)                                          M23.4099
      F(N4) = F(N4) + VOLTI(I)                                          M23.4100
      GO TO 11822                                                       M23.4101
11818 IF (N4 .GT. 0) GO TO 11820                                        M23.4102
      N4 = - N4                                                         M23.4103
      D1 = 0.0                                                          M23.4104
      N5 = N2                                                           M23.4105
      DO 11819 J=1, IT2                                                 M23.4106
      D1 = D1 + QFD(N5) * VOLT(J)                                       M23.4107
      N5 = N5 + IT2                                                     M23.4108
11819 CONTINUE                                                          M23.4109
      KCOUNT = KCOUNT + 1                                               M23.4110
      BVALUE(KCOUNT) = D1                                               M23.4111
11820 N5 = N2                                                           M23.4112
      D1 = 0.0                                                          M23.4113
      DO 11821 J=1, IT2                                                 M23.4114
      D1= D1 - QFD(N5) * VOLTI(J)                                       M23.4115
      N5 = N5 + IT2                                                     M23.4116
11821 CONTINUE                                                          M23.4117
      IF (IPRSUP .GE. 9)                                                M23.4118
     1    WRITE (LUNIT6, 23050) N2, N3, N4, KCOUNT, D1, BVALUE(KCOUNT)  M23.4119
23050 FORMAT( 73H  PHASE CONVERSION AT 11822.  N2, N3, N4, KCOUNT, D1, BM23.4120
     1VALUE(KCOUNT) =   ,4I6,2(1X,E15.7))                               M23.4121
      F(N3) = F(N3) + D1                                                M23.4122
      F(N4) = F(N4) - D1                                                M23.4123
      N2 = N2 + 1                                                       M23.4124
11822 CONTINUE                                                          M23.4125
      IT2 = 1                                                           M23.4126
11823 IF (CKI(K) .LT. 0.0) GO TO 1200                                   M23.4127
      K = K + 1                                                         M23.4128
      GO TO 11823                                                       M23.4129
C  CALCULATION OF BRANCH VOLT(VOLTI) AND NORTON VOLTK(VIM) CURRENT      M23.4130
C  FOR TRANSMISSION LINE MODEL FOR KBUS(MBUS)                           M23.4131
11824 N2 = ABSZ(CKI(II)) - 1.0                                          M23.4132
      N3 = N2 * PHSINV + 1                                              M39.2945
      N2 = N2 - (N3 - 1) * IT2 + 1                                      M23.4134
      N4 = CK(II)                                                       M23.4135
      N5 = INDHST(II)                                                   M23.4136
      N6 = KODSEM(II) + 1                                               M23.4137
      N7 = NR(II) + IPOINT - 2                                          M23.4138
      N8 = CI(II)                                                       M23.4139
C  D1(D2) = INCIDENT VOLTAGE WAVE AT T=T0 FOR KBUS(MBUS)                M23.4141
C  D3(D4) = INCIDENT VOLTAGE WAVE AT T=T0+DELTAT FOR KBUS(MBUS)         M23.4142
C  EXPONENTIAL(SEMLYEN) REPRESENTATION OF PROPAGATION TRANSFER FUNCTION M23.4143
      D1 = 0.0                                                          M23.4144
      D2 = 0.0                                                          M23.4145
      D3 = 0.0                                                          M23.4146
      D4 = 0.0                                                          M23.4147
      IF (N8 .EQ. 0) GO TO 11847                                        M23.4148
      N7 = N7 + 2                                                       M23.4149
      IF (N7 .GT. LPAST) N7 = N7 - LPAST                                M23.4150
      N9 = N7 - 1                                                       M23.4151
      IF (N9 .LE. 0) N9 = N9 + LPAST                                    M23.4152
      N10 = N7 + 1                                                      M23.4153
      IF (N10 .GT. LPAST) N10 = N10 - LPAST                             M23.4154
      DO 11838 J=1, N8                                                  M23.4155
      IF (IPRSUP .GE. 9)                                                M23.4156
     1     WRITE (LUNIT6, 23060) N7, CNVHST(N5), CNVHST(N5+1),          M23.4157
     2          SCONST(N6)                                              M23.4158
23060 FORMAT(64H  TOP OF DO 11838.  N7, CNVHST(N5), CNVHST(N5+1), SCONSTM23.4159
     1(N6) =    ,I10,  3(1X,E15.7))                                     M23.4160
      IF (SCONST(N6)) 11830, 11826, 11828                               M23.4161
11826 D1 = D1 + CNVHST(N5 + 0)                                          M23.4162
      D2 = D2 + CNVHST(N5 + 1)                                          M23.4163
      CNVHST(N5 + 0) = CNVHST(N5 + 0) * SCONST(N6 + 1)                  M23.4164
      CNVHST(N5 + 1) = CNVHST(N5 + 1) * SCONST(N6 + 1)                  M23.4165
      GO TO 11830                                                       M23.4166
11828 D5 = CNVHST(N5 + 0)                                               M23.4167
      D6 = CNVHST(N5 + 1)                                               M23.4168
      D1 = D1 + D5 + D5                                                 M23.4169
      D2 = D2 + D6 + D6                                                 M23.4170
      CNVHST(N5 + 0) = D5 * SCONST(N6 + 1) - CNVHST(N5 + 2) *           M23.4171
     1          SCONST(N6 + 6)                                          M23.4172
      CNVHST(N5 + 1) = D6 * SCONST(N6 + 1) - CNVHST(N5 + 3) *           M23.4173
     1          SCONST(N6 + 6)                                          M23.4174
      CNVHST(N5 + 2) = D5 * SCONST(N6 + 6) + CNVHST(N5 + 2) *           M23.4175
     1          SCONST(N6 + 1)                                          M23.4176
      CNVHST(N5 + 3) = D6 * SCONST(N6 + 6) + CNVHST(N5 + 3) *           M23.4177
     1          SCONST(N6 + 1)                                          M23.4178
11830 CNVHST(N5 + 0) = CNVHST(N5 + 0) + SCONST(N6 + 2) * XM(N10) +      M23.4179
     1          SCONST(N6 + 3) * XM(N7) + SCONST(N6 + 4) * XM(N9)       M23.4180
      CNVHST(N5 + 1) = CNVHST(N5 + 1) + SCONST(N6 + 2) * XK(N10) +      M23.4181
     1          SCONST(N6 + 3) * XK(N7) + SCONST(N6 + 4) * XK(N9)       M23.4182
      IF (SCONST(N6)) 11836, 11832, 11834                               M23.4183
11832 D3 = D3 + CNVHST(N5 + 0)                                                  
      D4 = D4 + CNVHST(N5 + 1)                                          M23.4185
      GO TO 11836                                                       M23.4186
11834 D3 = D3 + 2.0 * CNVHST(N5 + 0)                                            
      D4 = D4 + 2.0 * CNVHST(N5 + 1)                                            
11836 N5 = N5 + 2                                                               
      N6 = N6 + 5                                                       M23.4190
      IF (IPRSUP .GE. 9)                                                M23.4191
     1     WRITE (LUNIT6, 23070) N5, N6, D3, D4, CNVHST(N5 - 2),        M23.4192
     2          CNVHST(N5 - 1)                                          M23.4193
23070 FORMAT( 65H  END OF DO 11838.  N5, N6, D3, D4, CNVHST(N5-2), CNVHSM23.4194
     1T(N5-1) =   ,2I5 ,4(1X,E13.6))                                    M23.4195
11838 CONTINUE                                                          M23.4196
      N7 = N7 + LENGTH(II) - 2                                          M23.4197
      IF (N7 .GT. LPAST) N7 = N7 - LPAST                                M23.4198
C  UPDATE PAST HISTORY OF OUTGOING VOLTAGE WAVES FROM EACH BUS          M23.4242
      N9 = N3 + IT2                                                     M39.2946
      XK(N7) = SEMAUX(N3) - D1                                          M23.4244
      XM(N7) = SEMAUX(N9) - D2                                          M23.4245
      D1 = XK(N7) - D1                                                  M23.4246
      D2 = XM(N7) - D2                                                  M23.4247
11847 N10 = KODSEM(II)                                                  M23.4248
      D5 = SCONST(N10) * D1                                             M23.4249
      D6 = SCONST(N10) * D2                                             M23.4250
      D3 = - D3 - D3                                                    M23.4251
      D4 = - D4 - D4                                                    M23.4252
      IF (IPRSUP .LT. 9) GO TO 23110                                    M23.4253
      N8 = NR(II) + IPOINT - 1                                          M23.4254
      IF (N8 .GT. LPAST) N8 = N8 - LPAST                                M23.4255
      WRITE (LUNIT6, 23100) N2, N3, N4, N5, N6, N7, N8, SEMAUX(N3),     M23.4256
     1          SEMAUX(N9), D1, D2, D3, D4, D5, D6,                     M23.4257
     2          (XK(J), XM(J), J=N8, N7)                                M23.4258
23100 FORMAT(/, 60H  AFTER VOLTAGE CONVOLUTION.  N2, N3, N4, N5, N6, N7,M23.4259
     1 N8  =   ,7I10,/, 83H   SEMAUX(N3), SEMAUX(N9), D1, D2, D3, D4, D5M23.4260
     2, D6, (XK(J), XM(J), J=N8, N7)  ....    /  (8(1X,E15.7)))         M23.4261
C  VOLT(VOLTI) = BRANCH CURRENT AT T=T0 AT KBUS(MBUS).                  M23.4263
C  VOLTK(VIM) = NORTON CURRENT VIA PARTIAL ADMITTANCE CONVOLUTION.      M23.4264
C  EXPONENTIAL(SEMLYEN) REPRESENTATION OF ADMITTANCE IMPULSE...         M23.4265
23110 D7 = SCONST(N10) * D3                                             M39.2947
      D8 = SCONST(N10) * D4                                             M23.4267
      IF (N4 .EQ. 0) GO TO 11862                                        M23.4268
      D11 = SEMAUX(N3)                                                  M23.4269
      D12 = SEMAUX(N9)                                                  M23.4270
      DO 11856 J=1, N4                                                  M23.4271
      D9 = CNVHST(N5 + 0) + D11 * SCONST(N6 + 2)                        M23.4272
      D10 = CNVHST(N5 + 1) + D12 * SCONST(N6 + 2)                       M23.4273
      IF (IPRSUP .GE. 9)                                                M23.4274
     1     WRITE (LUNIT6, 23120) N5, N6, CNVHST(N5), CNVHST(N5+1),      M23.4275
     2          D9, D10, SCONST(N6)                                     M23.4276
23120 FORMAT( 20H  TOP OF DO 11856.   ,2I10, 5(1X,E15.7))               M23.4277
      IF (SCONST(N6)) 11848, 11850, 11852                               M23.4278
11848 CNVHST(N5 + 0) = CNVHST(N5 - 2) * SCONST(N6 + 1)                  M23.4279
     1          + D9 * SCONST(N6 - 3) + D3 * SCONST(N6 + 2)             M23.4280
     2          + D1 * SCONST(N6 + 3)                                   M23.4281
      D9             = CNVHST(N5 - 2) * SCONST(N6 - 3)                  M23.4282
     1          - D9 * SCONST(N6 + 1) + D3 * SCONST(N6 - 2)             M23.4283
     2          + D1 * SCONST(N6 - 1)                                   M23.4284
      CNVHST(N5 + 1) = CNVHST(N5 - 1) * SCONST(N6 + 1)                  M23.4285
     1          + D10 * SCONST(N6 - 3) + D4 * SCONST(N6 + 2)            M23.4286
     2          + D2 * SCONST(N6 + 3)                                   M23.4287
      D10            = CNVHST(N5 - 1) * SCONST(N6 - 3)                  M23.4288
     1          - D10 * SCONST(N6 + 1) + D4 * SCONST(N6 - 2)            M23.4289
     2          + D2 * SCONST(N6 - 1)                                   M28.5539
      CNVHST(N5 - 2) = D9                                               M23.4291
      CNVHST(N5 - 1) = D10                                              M23.4292
      D7 = D7 + D9 + D9                                                 M23.4293
      D8 = D8 + D10 + D10                                               M23.4294
      GO TO 11854                                                       M23.4295
11850 D5 = D5 + D9                                                      M23.4296
      D6 = D6 + D10                                                     M23.4297
      D9 = D9 * SCONST(N6 + 1) + D3 * SCONST(N6 + 2)                    M23.4298
     1          + D1 * SCONST(N6 + 3)                                   M23.4299
      D10 = D10 * SCONST(N6 + 1) + D4 * SCONST(N6 + 2)                  M23.4300
     1          + D2 * SCONST(N6 + 3)                                   M23.4301
      CNVHST(N5 + 0) = D9                                               M23.4302
      CNVHST(N5 + 1) = D10                                              M23.4303
      D7 = D7 + D9                                                      M23.4304
      D8 = D8 + D10                                                     M23.4305
      GO TO 11854                                                       M23.4306
11852 D5 = D5 + D9 + D9                                                 M23.4307
      D6 = D6 + D10 + D10                                               M23.4308
      CNVHST(N5 + 0) = D9                                               M23.4309
      CNVHST(N5 + 1) = D10                                              M23.4310
11854 N5 = N5 + 2                                                       M23.4311
      N6 = N6 + 4                                                       M23.4312
      IF (IPRSUP .GE. 9)                                                M23.4313
     1     WRITE (LUNIT6, 23130) D5, D6, D7, D8, D9, D10                M23.4314
23130 FORMAT( 46H  END OF DO 11856.  D5, D6, D7, D8, D9, D10 =   ,      M23.4315
     1     6(1X,E13.6))                                                 M23.4316
11856 CONTINUE                                                          M23.4317
11862 VOLT(N2) = VOLT(N2) + D5                                          M23.4371
      VOLTI(N2) = VOLTI(N2) + D6                                        M23.4372
      VOLTK(N2) = VOLTK(N2) + D7                                        M23.4373
      NDX1 = LSIZ26 + N2                                                M31.3331
      VOLT(NDX1) = VOLT(NDX1) + D8                                      M31.3332
      II = II + 1                                                       M23.4375
      IF (CKI(II - 1) .GT. 0.0) GO TO 11824                             M23.4376
C  INCREMENT F TO INCLUDE THE NORTON CURRENTS OF THIS SET OF COUPLED    M23.4377
C  BRANCHES.  INCLUDE BRANCH CURRENT IN 'BVALUE' IF MBUS(.) .LT. 0.     M23.4378
      N2 = CIK(K)                                                       M23.4379
      DO 11874 I=1, IT2                                                 M23.4380
      II = I + K - 1                                                    M23.4381
      N3 = - KBUS(II)                                                   M23.4382
      N4 = MBUS(II)                                                     M23.4383
      IF (N2 .GT. 0) GO TO 11866                                        M23.4384
      IF (N4 .GT. 0) GO TO 11864                                        M23.4385
      N4 = - N4                                                         M23.4386
C  CURRENTS REQUESTED BY COL-80 PUNCH IS FROM 'KBUS' TO 'MBUS' AT 'KBUS'M23.4387
C  TERMINAL.  THE CORRESPONDING 'BVALUE(KCOUNT) =  ... ' CARD WHICH IS AM23.4388
C  COMMENT IS FOR THE CURRENT FROM 'MBUS' TO 'KBUS' AT 'MBUS' TERMINAL. M23.4389
      KCOUNT = KCOUNT + 1                                               M23.4390
      BVALUE(KCOUNT) = VOLT(I)                                          M23.4391
C     KCOUNT = KCOUNT + 1                                               M23.4392
C     BVALUE(KCOUNT) = VOLTI(I)                                         M23.4393
11864 F(N3) = F(N3) - VOLTK(I)                                          M23.4394
      NDX1 = LSIZ26 + I                                                 M31.3333
      F(N4) = F(N4) - VOLT(NDX1)                                        M31.3334
      GO TO 11874                                                       M23.4396
11866 IF (N4 .GT. 0) GO TO 11870                                        M23.4397
      N4 = - N4                                                         M23.4398
      D1 = 0.0                                                          M23.4399
      D2 = 0.0                                                          M23.4400
      N5 = N2                                                           M23.4401
      DO 11868 J=1, IT2                                                 M23.4402
      D1 = D1 + QFD(N5) * VOLT(J)                                       M23.4403
      D2 = D2 + QFD(N5) * VOLTI(J)                                      M23.4404
      N5 = N5 + IT2                                                     M23.4405
11868 CONTINUE                                                          M23.4406
      KCOUNT = KCOUNT + 1                                               M23.4407
      BVALUE(KCOUNT) = D1                                               M23.4408
C     KCOUNT = KCOUNT + 1                                               M23.4409
C     BVALUE(KCOUNT) = D2                                               M23.4410
11870 N5 = N2                                                           M23.4411
      D1 = 0.0                                                          M23.4412
      D2 = 0.0                                                          M23.4413
      NDX1 = LSIZ26                                                     M31.3335
      DO 11872 J=1, IT2                                                 M23.4414
      D1 = D1 - QFD(N5) * VOLTK(J)                                      M23.4415
      NDX1 = NDX1 + 1                                                   M31.3336
      D2 = D2 - QFD(N5) * VOLT(NDX1)                                    M31.3337
      N5 = N5 + IT2                                                     M23.4417
11872 CONTINUE                                                          M23.4418
      IF (IPRSUP .GE. 9)                                                M23.4419
     1     WRITE (LUNIT6, 23170) N2, N3, N4, KCOUNT, BVALUE(KCOUNT),    M23.4420
     1          D1, D2                                                  M23.4421
23170 FORMAT( 50H  AFTER PHASE CONVERSION.  N2, N3, N4, KCOUNT =   ,    M23.4422
     1     4I10,/, 30H   BVALUE(KCOUNT), D1, D2 =   ,3(1X,E15.7))       M23.4423
      F(N3) = F(N3) + D1                                                M23.4424
      F(N4) = F(N4) + D2                                                M23.4425
      N2 = N2 + 1                                                       M23.4426
11874 CONTINUE                                                          M23.4427
      IT2 = 1                                                           M23.4428
11876 IF (CKI(K) .LT. 0.0) GO TO 1200                                   M23.4429
      K = K + 1                                                         M23.4430
      GO TO 11876                                                       M23.4431
 1190 IF( IPUNCH .EQ. 0 )  GO TO 1200                                      13547
      IF( LENGTH(K) .LE. 0 )  GO TO 3564                                   13548
      IF( KODEBR(K) .GT. 0 )  GO TO 3564                                   13549
      CALL REDU17 ( X(N3), IT2, LL0 )                                   M22.4965
 3564 DO 1191  I=1, IT2                                                         
      VOLTI(I)=-VOLTI(I) * ONEHAF                                               
      VOLTK(I)=-VOLTK(I) * ONEHAF                                               
      IF( KODEBR(K) .GT. 0 )  GO TO 1191                                   13554
      VOLT(I) = -VOLT(I)                                                        
 1191 CONTINUE                                                                  
      CALL MULT(X(N3),VOLT,CIK(K),IT2,LL1)                                      
      CALL MULT(C(N3),VOLTI,CI(K),IT2,LL1)                                      
      CALL MULT(C(N3),VOLTK,CK(K),IT2,LL1)                                      
 1200 K=K+IT2                                                                   
      IF ( IPRSUP  .GE.  4 )                                            M20.4291
     1 WRITE (LUNIT6, 7342)  K, IBR,  ( F(I), I=1, NTOT )               M20.4292
 7342 FORMAT ( /,  9H K, IBR =,  2I6,  10X,                             M20.4293
     1  30H(F(I), I=1, NTOT )  FOLLOW ...    ,/,  (1X, 8E16.7)  )       M20.4294
      IF(K.LE.IBR) GO TO 1100                                                   
      LASTOV = NCHAIN                                                   M15.1296
      NCHAIN = 18                                                          13562
      IF ( IPRSUP  .GE.  4 )                                            M20.4295
     1 WRITE (LUNIT6, 7342)  K, IBR,  ( F(I), I=1, NTOT )               M20.4296
 9999 RETURN                                                               13563
      END                                                                  13565
      SUBROUTINE FDCINJ( IKF, ISFD, IBF )                               M32.5970
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION  UR(40)                                                 M32.5973
C     THIS ROUTINE UPDATES THE CURRENT INJECTIONS FOR THE INDIVIDUAL    M32.5974
C     BRANCHES AS WELL AS FOR THE EQUIVALENT  BRANCHES INSERTED INTO THEM32.5975
C     EMTP. IT ALSO UPDATES THE CAPACITOR AND INDUCTOR VOLTAGES AND     M32.5976
C     CALCULATES BRANCH CURRENTS FOR THE INDIVIDUAL BRANCHES   *   *   *M32.5977
      IDK = 2 * IKF                                                     M32.5979
      IKF = IKF + 1                                                     M32.5980
      ISF = ISFD + 1                                                    M32.5981
      ISC = IBF + 1                                                     M32.5982
      IF( IPRSUP .GT. 0 )                                               M32.5983
     1    WRITE( LUNIT6, 1 ) IKF, ISFD, IBF, IMFD(IDK+1), IMFD(IDK+2)   M32.5984
    1 FORMAT(41H INTEGER COUNTERS AT START OF FDCINJ.....,7X,3HIKF,     M32.5985
     16X,4HISFD,7X,3HIBF,6X,4HIZFD,6X,4HIPFD,/,41X,5I10)                M32.5986
C     CONVERT PHASE VOLTAGES TO MODAL FRAME OF REFERENCE   *   *   *   *M32.5987
      CZ = IT2                                                          M34. 155
      CZ = 1.0 / CZ                                                     M34. 156
      UR(1) = VOLT(1)                                                   M32.5989
      DO  2  KA = 2, IT2                                                M32.5990
         UR(1) = UR(1) + VOLT(KA)                                       M32.5991
    2    UR(KA) = ( VOLT(1) - VOLT(KA) ) * CZ                           M32.5992
      UR(1) = UR(1) * CZ                                                M32.5993
      IF( IPRSUP .GT. 0 )                                               M32.5994
     1    WRITE( LUNIT6, 3 ) IKF, ( UR(KA), KA = 1, IT2 )               M32.5995
    3 FORMAT(37H MODAL VOLTAGES IN FDCINJ FOR SET NO.,I6,/,             M32.5996
     1(2X,6E21.11))                                                     M32.5997
C     PROCESS FIRST THE 'ZERO' SEQUENCE DATA   *   *   *   *   *   *   *M32.5998
      AR = 0.                                                           M32.5999
      IST = ISF                                                         M32.6000
      ISU = ISFD + IMFD(IDK+1) * 5                                      M32.6001
      ISV = IBF - 2                                                     M32.6002
      DO  4  KA = IST, ISU, 5                                           M32.6003
         ISV = ISV + 3                                                  M32.6004
C     CALCULATE BRANCH CURRENT     *   *   *   *   *   *   *   *   *   *M32.6005
         AZR = RMFD(KA+1) * UR(1) + CIKFD(ISV)                          M32.6006
C     C A P A C I T O R   V O L T A G E    *   *   *   *   *   *   *   *M32.6007
         CIKFD(ISV+2) = CIKFD(ISV+2) + RMFD(KA+2)                       M32.6008
     1                 * ( AZR + CIKFD(ISV+1) )                         M32.6009
C     I N D U C T O R   V O L T A G E  *   *   *   *   *   *   *   *   *M32.6010
         AZI = UR(1) - AZR * RMFD(KA) - CIKFD(ISV+2)                    M32.6011
         AZI = -RMFD(KA+4) * AZI - 2. * CIKFD(ISV+2) + UR(1)            M32.6012
C     C U R R E N T   I N J E C T I O N    *   *   *   *   *   *   *   *M32.6013
         AZI = RMFD(KA+1) * AZI - RMFD(KA+3) * AZR                      M32.6014
         AR = AR + AZI                                                  M32.6015
         CIKFD(ISV) = AZI                                               M32.6016
    4    CIKFD(ISV+1) = AZR                                             M32.6017
      UR(1) = AR                                                        M32.6018
C     PROCESS THE REMAINING ( IT2 - 1 ) MODES  *   *   *   *   *   *   *M32.6019
      ISFD = ISU                                                        M32.6020
      IBF = ISV + 2                                                     M32.6021
      ISK = IMFD(IDK+2)                                                 M32.6022
      IST = ISFD + 1                                                    M32.6023
      ISU = ISFD + ISK * 5                                              M32.6024
C     LOOP ACROSS MODES            *   *   *   *   *   *   *   *   *   *M32.6025
      DO  6  KA = 2, IT2                                                M32.6026
         AR = 0.                                                        M32.6027
         UN = UR(KA)                                                    M32.6028
         DO  5  KB = IST, ISU, 5                                        M32.6029
            ISV = ISV + 3                                               M32.6030
C     CALCULATE BRANCH CURRENT     *   *   *   *   *   *   *   *   *   *M32.6031
            AZR =  RMFD(KB+1) * UN + CIKFD(ISV)                         M32.6032
C     C A P A C I T O R   V O L T A G E    *   *   *   *   *   *   *   *M32.6033
            CIKFD(ISV+2) = CIKFD(ISV+2) + RMFD(KB+2)                    M32.6034
     1                    * ( AZR + CIKFD(ISV+1) )                      M32.6035
C     I N D U C T O R   V O L T A G E  *   *   *   *   *   *   *   *   *M32.6036
            AZI = UN - AZR * RMFD(KB) - CIKFD(ISV+2)                    M32.6037
            AZI = -AZI * RMFD(KB+4) - 2. * CIKFD(ISV+2) + UN            M32.6038
            AZI = RMFD(KB+1) * AZI - RMFD(KB+3) * AZR                   M32.6039
            AR = AR + AZI                                               M32.6040
            CIKFD(ISV) = AZI                                            M32.6041
    5       CIKFD(ISV+1) = AZR                                          M32.6042
    6    UR(KA) = AR                                                    M32.6043
      IBF = IBF + ( IT2 - 1 ) * ISK * 3                                 M32.6044
      ISFD = ISU                                                        M32.6045
C     CONVERT MODAL CURRENT INJECTIONS TO PHASE COORDINATES    *   *   *M32.6046
      CZ = IT2                                                          M34. 157
      VOLT(1) = 0.                                                      M32.6048
      DO  7  KA = 1, IT2                                                M32.6049
    7    VOLT(1) = VOLT(1) + UR(KA)                                     M32.6050
      DO  8  KA = 2, IT2                                                M32.6051
    8    VOLT(KA) = VOLT(1) - CZ * UR(KA)                               M32.6052
      IF( IPRSUP .LE. 0 ) GO TO 15                                      M32.6053
      WRITE( LUNIT6, 9 ) ( VOLT(KA), KA = 1, IT2 )                      M32.6054
    9 FORMAT(35H PHASE CURRENT INJECTIONS CIK......,/,(2X,6E21.11))     M32.6055
      WRITE( LUNIT6, 10 ) ISC, IBF, ( CIKFD(KA), KA = ISC, IBF )        M32.6056
   10 FORMAT(31H UPDATED MODAL BRANCH DATA FROM,I6,4H  TO,I6,/,         M32.6057
     1(2X,6E21.11))                                                     M32.6058
   15 RETURN                                                            M32.6059
      END                                                               M32.6060
      SUBROUTINE UPDATE                                                 M20.4297
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED ONLY BY BRANDWAJN (TYPE-59) S.M. MODEL        M31.3338
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      DIMENSION VSMOUT(1)                                               M39.2950
      EQUIVALENCE ( ISMOUT(1), VSMOUT(1) )                              M39.2951
      DIMENSION MASSEX(1)                                               M38.3141
      EQUIVALENCE ( HISTQ(1), MASSEX(1) )                               M38.3142
C     INTRINSIC  COSZ, SINZ, ABSZ, SQRTZ, ATAN2Z                        M34. 158
C     THIS ROUTINE ADJUSTS THE CURRENT SOURCES TO BE INJECTED INTO      M20.4303
C     THE EQUIVALENT PI-CIRCUITS * * * * * * * * * * * * * * * * * * * *M20.4304
      IF ( IPRSUP  .GE.  1 )                                            M32.6062
     1 WRITE( LUNIT6, 4099 )                                            M39.2952
 4099 FORMAT ( 24H  "BEGIN MODULE UPDATE." )                            M39.2953
C     INITIALIZE COUNTERS     ******************************************M33.1543
      ILK = 0                                                           M33.1544
      LL0 = 0                                                           M33.1545
      LL1 = 1                                                           M33.1546
C     COUNTERS FOR ELECTRICAL VARIABLES ********************************M33.1547
      IBR =  IBROLD                                                     M33.1548
      NWD = 24                                                                  
      IKN = 1                                                                   
      I26 = 0                                                                   
      LL7 = 7                                                           M33.1554
C     COUNTERS FOR MECHANICAL VARIABLES ********************************M33.1556
      IU = 0                                                                    
      IB = 0                                                                    
C     CONTERS FOR TACS  INTERFACE AND OUTPUT. COMPUTATIONAL CONSTANTS   M33.1561
      LMSET = 0                                                         M33.1562
      ICNT = MSMOUT                                                             
      IPOUT = -2                                                                
CCCC  ACB = OMDT * ISTEP                                                VAX.2973
      ACB = OMEGA * T                                                   VAX.2974
      IF( IPRSUP .GT. 0 )                                               M30. 534
     1 WRITE( LUNIT6, 4100 ) IBR, NUMSM, IT                             M39.2955
 4100 FORMAT(17H BEGIN  'UPDATE' ,5X, 3HIBR, 5X, 5HNUMSM, 8X, 2HIT, /,  M39.2956
     1 15X, 3( 5X,I5 ) )                                                M39.2957
      J30 = 1                                                                   
      I30 = 1                                                                   
      J75 = 27                                                                  
      N22SPY = KSMSPY( 1 )                                                      
      DO 900 K = 1, NUMSM                                               M38.3152
      IM = ISMDAT( J30 )                                                        
      N1 = ISMDAT( J30+2 )                                                      
      N2 = ISMDAT( J30+5 )                                                      
      V1 = E( N1 ) - E( N2 )                                                    
      N1 = ISMDAT( J30+3 )                                                      
      N2 = ISMDAT( J30+6 )                                                      
      V2 = E( N1 ) - E( N2 )                                                    
      N1 = ISMDAT( J30+4 )                                                      
      N2 = ISMDAT( J30+7 )                                                      
      V3 = E( N1 ) - E( N2 )                                                    
      IDSAT = 0                                                         M23.4432
      IDELTA = ISMDAT( J30+1 )                                                  
      AP1 = 0.0                                                         M23.4433
      AP2 = 0.0                                                         M23.4434
      IF( IPRSUP  .GT.  0  )                                            M30. 535
     1 WRITE( LUNIT6, 4101 ) V1, ISMDAT(J30+2), V2, ISMDAT(J30+3),      M39.2965
     2 V3, ISMDAT(J30+4)                                                M37.5341
 4101 FORMAT ( 13X, 2HV1, 9X, 6HNODSMA, 13X, 2HV2, 9X, 6HNODSMB,        M39.2966
     1 13X, 2HV3, 9X, 6HNODSMC, /, 3(1X, E14.7, 10X, I5) )              M38.3156
 800  ILK = ILK + 1                                                             
      NUMASK = ISMDAT( I30+11 )                                                 
      NLOCG = ISMDAT( I30+12 )                                                  
      NLOCE = ISMDAT( I30+13 )                                                  
      NUM2 = NUMASK + NUMASK                                            M30. 538
      NUM4 = NUM2 + NUM2                                                M38.3158
      NUM6 = NUM4 + NUM2                                                M38.3159
      N26  = IU + NUM4                                                          
      N27  = N26 + NUMASK                                                       
      IKV = IU + NUMASK                                                         
      IKW = IKV + 1                                                     M30. 542
      IBU = IB + 1                                                              
      N22 = IB + NUM2                                                           
      KAG = IU + NLOCG                                                  M30. 544
      KAG2 = KAG + NUM2                                                 M37.5349
      KSG = KAG + NUMASK                                                M30. 545
      KSEX = IKV + NLOCE                                                M30. 546
      IKP = IKV + NUMASK                                                M37.5350
C     CALCULATE CURRENTS FOR TIME ''T-DELTAT''                          M20.4346
      I75 = I26 + 27                                                            
      A1 = ELP( I75 )                                                           
      A2 = ELP(I75+1)                                                           
      JUK = IBR + 1                                                     M37.5354
      D6 =  -A1 * V1 - A2 * (V2+V3) - CIK( JUK )                        M37.5355
      D7 =  -A1 * V2 - A2 * (V1+V3) - CIK( JUK+1 )                      M37.5356
      D8 =  -A1 * V3 - A2 * (V1+V2) - CIK( JUK+2 )                      M37.5357
      CV3 = ( V1 + V2 + V3 ) * ASQRT3                                   M36.1533
      A5 = - ( CV3 - CU( IKN+2 ) ) * ELP( I26+17 )                              
      IF( IPRSUP .GT. 0 )                                               M30. 550
     1 WRITE( LUNIT6, 4102 )  D6, D7, D8                                M39.2967
 4102 FORMAT( 21H FOR TIME "T-DELTAT"., 18X, 2HD6, 18X, 2HD7,           M39.2968
     1 18X, 2HD8, /, 21X, 3E20.11 )                                     M39.2969
C     TRANSFORM PHASE VARIABLES TO PARK"S COORDINATES  * * * * * * * * *M30. 554
      CZ = ELP( I26+26 )                                                        
      CZT = CZ * TENM6                                                  M32.6066
      ACDE = ELP( I75+2 )                                                       
      ACDF = ELP( I75+3 )                                                       
      ISPDR = 1                                                         M30. 556
      Q3 = -CU( IKN+10 ) * CU( IKN+11 ) * CZT                           M37.5362
      ETOT = CU( IKN+19 )                                                       
      SUM  = CU( IKN+20 )                                                       
      DSPED = HISTQ( KSG )                                                      
      N7 = ISMDAT( I30+16 )                                                     
C     ENTER THE ITERATION LOOP(ANGLE,SPEED,CURRENT)  * * * * * * * * * *M30. 570
 200  ETOT = ETOT * ATHTW                                               M36.1536
      SUM = SUM * ATHTW                                                 M36.1537
      AU = ETOT * ONEHAF                                                M38.3175
      AV = SUM * SQRT32                                                 M33.1570
      TSD = AV - AU                                                     M38.3176
      TSC =-AU - AV                                                     M38.3177
      A3  =  D6 * ETOT + D7 * TSD + D8 * TSC                            M38.3178
      CV1 =  V1 * ETOT + V2 * TSD + V3 * TSC                            M38.3179
      AU = AU * SQRT3                                                   M38.3180
      AV = SUM * ONEHAF                                                 M38.3181
      TSD = AU - AV                                                     M38.3182
      TSC =-AV - AU                                                     M38.3183
      A4  =  D6 * SUM + D7 * TSC + D8 * TSD                             M38.3184
      CV2 =  V1 * SUM + V2 * TSC + V3 * TSD                             M38.3185
C     CALCULATE ROTOR CURRENTS **************************************** M38.3186
      C1 = ELP( I75+5 ) * A3 + ELP( I75+9 ) * CU( IKN+3 ) +             M38.3187
     1 ELP( I75+10 ) * CU( IKN+4 )                                      M38.3188
      C2 = ELP( I75+6 ) * A3 + ELP( I75+10 ) * CU( IKN+3 ) +            M38.3189
     1 ELP( I75+12 ) * CU( IKN+4 )                                      M38.3190
      C3 = ELP( I75+7 ) * A4 + ELP( I75+13 ) * CU( IKN+5 ) +            M38.3191
     1 ELP( I75+14 ) * CU( IKN+6 )                                      M38.3192
      C4 = ELP( I75+8 ) * A4 + ELP( I75+14 ) * CU( IKN+5 ) +            M38.3193
     1 ELP( I75+16 ) * CU( IKN+6 )                                      M38.3194
      IF( IPRSUP .LT. 1 )   GO  TO  922                                 M32.6070
      WRITE( LUNIT6, 4103 )    A3, A4, A5, CV1, CV2, CV3                M39.2971
 4103 FORMAT( 40X, 17H UPDATED CURRENTS, 40X, 17H UPDATED VOLTAGES, /,  M39.2972
     1 3X, 3E18.10, 3X, 3E18.10 )                                       M39.2973
      WRITE( LUNIT6, 4104 )    C1, C2, C3, C4                           M39.2974
 4104 FORMAT( 23H UPDATED ROTOR CURRENTS, /, 2X, 4( E20.12,5X ) )       M39.2975
C     CALCULATE THE ELECTROMAGNETIC TORQUE***************************** M37.5374
 922  AC2 = ELP(I26+1)*A3 + ELP(I26+2)*C1 + ELP(I26+4)*C2                       
      AC1 = ELP(I26+9)*A4 + ELP(I26+10)*C3 + ELP(I26+12)*C4                     
      CD = ( AC2 * ACDE * A4 - AC1 * ACDF * A3 ) * CZT                          
C     RESOLVE THE MECHANICHAL EQUATIONS. NOTE THE  CONSTANT TERMS HAVE  M30. 589
C     BEEN CALCULATED IN THE PREVIOUS TIME-STEP* * * * * * * * * * * * *M30. 590
      CEXC = HISTQ( KSEX )                                              M37.5378
      JT = N27                                                                  
      IF( N7 .GT. 0 )   GO TO  220                                              
      DO 219 IK = IKW, IKP                                              M38.3198
      JT = JT + 1                                                       M38.3199
 219  HISTQ( IK ) = HISTQ( JT ) / HISTQ( IK )                           M38.3200
      GO TO 222                                                         M38.3201
 220  M1 = N7                                                           M38.3202
      DO 201 IK = IKW, IKP                                              M37.5380
      JT = JT + 1                                                               
      M1 = M1 - 1                                                               
      NDX1 = MASSEX( M1 )                                                       
      IF( NDX1 .GT. 0 )   GO  TO  221                                           
      Q2 = 1.0                                                                  
      GO TO 201                                                         M38.3208
 221  NDX1 = KXTCS + NDX1                                               M38.3209
      Q2 = XTCS( NDX1 )                                                         
 201  HISTQ( IK ) = ( HISTQ( JT ) * Q2 ) / HISTQ( IK )                  M38.3211
 222  HISTQ( KSG ) = HISTQ( KSG ) - CD                                  M38.3212
      IF( NLOCE .EQ. 0 )   GO  TO  202                                  M30. 596
      CEXC = ( Q3 * C1 ) / CEXC                                         M37.5384
      HISTQ( KSEX ) = HISTQ( KSEX ) - CEXC                              M37.5385
C     CALCULATE NEW ESTIMATES OF MECHANICAL SPEEDS AND ROTOR ANGLE * * *M30. 605
 202  JT = N26                                                                  
      DO 203 IK = IKW, IKP                                              M37.5387
      JT = JT + 1                                                       M37.5388
      HISTQ( JT ) = -HISTQ( IK )                                        M38.3214
 203  HISTQ( IK ) =  HISTQ( IK ) - HISTQ( IK+NUM2 )                     M38.3215
      CALL BANSOL( SHP(IBU), HISTQ(IKW), NUMASK )                       M38.3216
      SPDN = HISTQ( KSG )                                                       
C     CHECK THE CONVERGENCE OF ROTOR SPEED **************************** M37.5393
      IF( ABSZ(DSPED) .LE. FLZERO )  DSPED = FLZERO                     M30. 611
      SPDD = ABSZ((SPDN - DSPED) / DSPED )                              M30. 612
      IF( SPDD .GT. EPDGEL )  GO  TO  206                               M30. 613
C     SUCCESSFUL ITERATION (CONVERGENCE ACHIEVED ) * * * * * * * * * * *M30. 614
C     FINISH THE CALCULATION OF THE MECHANICAL ANGLES  * * * * * * * * *M30. 615
 204  KC = N22 + 5 * NUMASK                                                     
      DO 205 KA = 1, NUMASK                                             M38.3219
      JT = IU + KA                                                      M37.5394
      IK = IKV + KA                                                     M30. 618
      KB = N26 + KA                                                     M37.5395
      KD = KC + KA                                                      M38.3220
      HISTQ( IK+NUM2 ) = HISTQ( KB ) - SHP( KD )                        M38.3221
 205  HISTQ( JT ) = HISTQ( JT+NUM2 ) + DELTA2 * HISTQ( IK )             M37.5397
C     STORE THE CALCULATED CURRENTS AND VOLTAGES   * * * * * * * * * * *M30. 622
      CU( IKN ) = A3                                                    M38.3222
      CU(IKN+1) = A4                                                    M30. 624
      CU(IKN+2) = A5                                                    M30. 625
      CU(IKN+3) = C1                                                    M30. 626
      CU(IKN+4) = C2                                                    M30. 627
      CU(IKN+5) = C3                                                    M30. 628
      CU(IKN+6) = C4                                                    M30. 629
      D2 = SPDN * CZ                                                    M30. 630
      Q3 = CU( IKN+10 ) * CU( IKN+11 )                                          
      DANG = HISTQ( KAG ) * CZ                                          M36.1538
      GO   TO   208                                                     M30. 632
 206  DSPED = SPDN                                                      M30. 633
      ISPDR = ISPDR + 1                                                 M30. 634
      DANG = ( HISTQ( KAG2 ) + DELTA2 * SPDN ) * CZ                     M37.5399
      ETOT = COSZ( DANG )                                               M36.1540
      SUM = SINZ( DANG )                                                M36.1541
      IF( ISPDR .LE. IPRSOV(37) )  GO TO 200                            M39.2976
      IF( SPDD .GT. EPOMEG )  GO  TO  920                               M30. 636
      WRITE( LUNIT6, 4105 )   ILK, ISTEP, IPRSOV(37)                    VAX.2975
 4105 FORMAT( 2X, 7HWARNING,2X,  15(1H*), /,2X, 35HLACK OF CONVERGENCE FM39.2978
     1OR MACHINE NO., I5, 2X, 11HON STEP NO., I10                       M39.2979
     2          ,/,  ' Iteration limit  IPRSOV(37) =',  I10  )          VAX.2976
      GO TO 204                                                         M30. 640
 920  LSTAT(19) = 206                                                   M33.1575
      LSTAT(18) = NCHAIN                                                M33.1576
      K1 = ISMDAT( J30+2 )                                                      
      BUS1 = BUS( K1 )                                                  M38.3224
      FLSTAT(14) = SPDD                                                 M30. 643
      LSTAT(12) = ILK                                                   M30. 644
      KILL = 104                                                        M30. 645
      RETURN                                                            M30. 646
 208  IF( IPRSUP .EQ. 0 )  GO TO  50                                    M33.1577
      WRITE( LUNIT6, 4106 )   SPDN, HISTQ(KSEX), CD, CEXC               M39.2980
 4106 FORMAT( 6X, 23HAFTER SPEED CALCULATION, 5X, 5HROTOR, 13X,         M39.2981
     1 7HEXCITER, 17X, 3HTEG, 16X, 4HTEXC, /, 20X, 4E20.12 )            M33.1580
C     LOAD INTERNAL MACHINE VARIABLES INTO TACS VARIABLE 'ETAC' ******* M38.3226
 50   IF( N22SPY .NE. -1 )  GO TO 51                                            
      KSMSPY( 2 ) = ILK                                                         
      CALL EMTSPY                                                               
      IF( KSMSPY( 1 ) .EQ. -1 )  GO TO 51                                       
      IF( KSMSPY( 1 ) .EQ. 2 )  N22SPY = 0                              M38.3231
      IF( N22SPY .EQ. 0 )   GO TO 51                                            
      SMOUTV( 1 ) = D6                                                  M39.2983
      SMOUTV( 2 ) = D7                                                  M39.2984
      SMOUTV( 3 ) = D8                                                  M39.2985
      SMOUTV( 4 ) = CV1                                                 M39.2986
      SMOUTV( 5 ) = CV2                                                 M39.2987
      SMOUTV( 6 ) = CV3                                                 M39.2988
      SMOUTV( 7 ) = Q3                                                  M39.2989
      SMOUTV( 8 ) = V1                                                  M39.2990
      SMOUTV( 9 ) = V2                                                  M39.2991
      SMOUTV( 10 ) = V3                                                 M39.2992
      SMOUTV( 11 ) = CD                                                 M39.2993
      SMOUTV( 12 ) = CEXC                                               M39.2994
      GO TO 50                                                                  
 51   KMSET =  ISMDAT( I30+14 )                                                 
      IF ( KMSET  .EQ.  0 )   GO TO 8610                                        
      DO 8650 I = 1, KMSET                                                      
      LMSET = LMSET + 1                                                 M20.4521
      JMSET = ISMTAC( LMSET )                                                   
      IF( JMSET .GT. 0 )  GO  TO  1696                                          
      JMSET = -JMSET                                                            
      IF( JMSET .GT. 7 )  GO  TO  1680                                          
      ETAC( LMSET ) = CU( IKN+JMSET-1 )                                         
      GO  TO  8650                                                              
 1680 IF( JMSET .GT. 8 )  GO  TO  1681                                          
      ETAC( LMSET ) = CV1                                               M38.3243
      GO  TO  8650                                                      M38.3244
 1681 IF( JMSET .GT. 9 )  GO  TO  1682                                          
      ETAC( LMSET ) = CV2                                               M38.3246
      GO  TO  8650                                                      M38.3247
 1682 IF( JMSET .GT. 10 )  GO  TO  1683                                         
      ETAC( LMSET ) = CV3                                               M38.3249
      GO  TO  8650                                                      M38.3250
 1683 IF( JMSET .GT. 11 )  GO  TO  1684                                         
      ETAC( LMSET ) = Q3                                                M38.3252
      GO  TO  8650                                                      M38.3253
 1684 IF( JMSET .GT. 13 )  GO  TO  1686                                 M38.3254
      SF4 = A3 * ELP( I26+21 ) + C1 + C2                                M38.3255
      SF5 = ( A4 * ELP( I26+21 ) + C3 + C4 ) * ELP( I75+4 )             M38.3256
      IF( JMSET .EQ. 13 )  GO  TO  1685                                 M38.3257
      ETAC( LMSET ) = SQRTZ( SF4**2 + SF5**2 )                          M38.3258
      GO  TO  8650                                                      M38.3259
 1685 ETAC( LMSET ) = ATAN2Z( SF5, SF4 )                                M38.3260
      GO  TO  8650                                                      M38.3261
 1686 IF( JMSET .GT. 14 )  GO  TO  1687                                         
      ETAC( LMSET ) = CD                                                M38.3263
      GO  TO  8650                                                      M38.3264
 1687 IF( JMSET .GT. 15 )  GO  TO  1688                                         
      ETAC( LMSET ) = CEXC                                              M38.3266
      GO  TO  8650                                                      M38.3267
 1688 IF( JMSET .EQ. 17 )  GO  TO  1689                                         
      ETAC( LMSET ) = AC2 * ACDE + ELP( I26+19 ) * A3                           
      GO  TO 8650                                                       M38.3270
 1689 ETAC( LMSET ) = AC1 * ACDF + ELP( I26+19 ) * A4                           
      GO  TO  8650                                                      M38.3272
 1696 IF( JMSET .GT. NUM2 )  GO  TO  1697                                       
      N9 = IU + JMSET                                                   M25. 886
      ETAC( LMSET ) = HISTQ( N9 )                                       M37.5403
      GO  TO  8650                                                      M25. 888
 1697 JMSET1 = JMSET - NUM2                                             M38.3274
      M = IU + JMSET1                                                   M37.5405
      N5 = M + NUMASK                                                   M25. 896
      IDS = N22 +  JMSET                                                M37.5406
      ETAC(LMSET) = SHP(IDS+NUMASK) * ( HISTQ(M) - HISTQ(M+1) ) +       M38.3275
     1 SHP( IDS ) * ( HISTQ( N5 ) - HISTQ( N5+1 ) )                     M38.3276
 8650 CONTINUE                                                          M20.4528
 8610 N9 = ISMDAT( I30+17 )                                             M37.5409
      IF( N9 .LT. 0 )  GO  TO  209                                              
      IF( N9 .EQ. 0 )  GO  TO  8215                                     M33.1588
      N8 =  IKN - 1                                                     M33.1589
      DO 8200 KA = 1, N9                                                M33.1590
      IPOUT = IPOUT + 3                                                         
      ICNT =  ICNT + 1                                                  M33.1592
      N5 = ISMOUT( IPOUT )                                                      
      GO TO (8201,8201,8201,8201,8201,8201,8201,8202,8203,8204,8205,    M33.1594
     1 8206,8206,8208,8209,8207 ), N5                                   M33.1595
 8201 L = N5  + N8                                                      M33.1596
      VSMOUT( ICNT ) = CU( L )                                          M39.2995
      GO TO 8200                                                        M33.1598
 8202 VSMOUT( ICNT ) = D6                                               M39.2996
      GO TO 8200                                                        M33.1600
 8203 VSMOUT( ICNT ) = D7                                               M39.2997
      GO TO 8200                                                        M33.1602
 8204 VSMOUT( ICNT ) = D8                                               M39.2998
      GO TO 8200                                                        M33.1604
 8205 VSMOUT( ICNT ) = Q3                                               M39.2999
      GO TO 8200                                                        M33.1606
 8206 SF4 = A3 * ELP( I26+21 ) + C1 + C2                                M37.5411
      SF5 = ( A4 * ELP( I26+21 ) + C3 + C4 ) * ELP( I75+4 )             M38.3279
      IF( N5 .EQ. 13 ) GO TO 8207                                       M33.1609
      VSMOUT( ICNT ) = SQRTZ( SF4**2 + SF5**2 )                         M39.3000
      GO TO 8200                                                        M33.1611
 8207 VSMOUT( ICNT ) = ATAN2Z( SF5, SF4 )                               M39.3001
      GO TO 8200                                                        M33.1613
 8208 VSMOUT( ICNT ) = CD                                               M39.3002
      GO TO 8200                                                        M33.1615
 8209 VSMOUT( ICNT ) = CEXC                                             M39.3003
 8200 CONTINUE                                                          M33.1617
 8215 N9 = ISMDAT( I30+18 )                                             M39.3004
      IF( N9 .EQ. 0 )  GO  TO  8225                                     M33.1619
      D9 = ACB / CZ                                                     M33.1620
      DO 8220 KA = 1, N9                                                M33.1621
      IPOUT =  IPOUT  + 3                                                       
      ICNT = ICNT + 1                                                   M33.1623
      N5 = ISMOUT(  IPOUT ) + IU                                        M33.1624
      VSMOUT( ICNT ) = ( HISTQ( N5 ) - D9 ) * RADEG                     M39.3005
 8220 CONTINUE                                                          M33.1626
 8225 N9 = ISMDAT( I30+19 )                                             M37.5414
      IF( N9 .EQ. 0 )  GO  TO  8235                                     M33.1628
      D9 = OMEGA / CZ                                                   M33.1629
      N12 = IU + NUMASK                                                 M33.1630
      DO 8230 KA = 1, N9                                                M33.1631
      IPOUT = IPOUT + 3                                                         
      ICNT =  ICNT + 1                                                  M33.1633
      N13 = ISMOUT( IPOUT ) + N12                                       M33.1634
      VSMOUT( ICNT ) = HISTQ( N13 ) - D9                                M39.3006
 8230 CONTINUE                                                          M33.1636
 8235 N9 = ISMDAT( I30+20 )                                             M37.5415
      IF( N9 .EQ. 0 )  GO  TO  209                                              
      JT = N22 + NUM2                                                   M37.5417
      DO 8239 KA = 1, N9                                                M33.1639
      ICNT = ICNT + 1                                                   M33.1640
      IPOUT = IPOUT + 3                                                         
      N5 = ISMOUT(  IPOUT )                                             M33.1642
      MP = JT + N5                                                      M37.5418
      M = IU + N5                                                       M37.5419
      N5 = M + NUMASK                                                   M33.1645
      VSMOUT( ICNT ) = SHP(MP+NUMASK) * ( HISTQ(M) - HISTQ(M+1) ) +     M39.3007
     1 SHP( MP ) * ( HISTQ( N5 ) - HISTQ( N5+1 ) )                      M38.3284
 8239 CONTINUE                                                          M33.1648
C     PREDICT NEW ROTOR ANGLE AND SPEED. CALCULATE ALSO CONSTANT TERMS  M37.5422
C     FOR THE ITERATION LOOP IN THE NEXT TIME-STEP   * * * * * * * * * *M37.5423
 209  S1 = 0.0                                                          M37.5424
      IZ = IU + 1                                                               
      IY = IKV                                                                  
      IZY = IY + NUM2                                                           
      IK  = N22 + NUM6                                                          
      IB = IK                                                                   
      D11 = HISTQ( IZ )                                                         
      D12 = HISTQ( IY+1 )                                                       
      IF( NUMASK .EQ. 1 )  GO TO  211                                           
      IK1 = IKV - 1                                                             
      DO 210 KA = IZ, IK1                                               M38.3291
      IY = IY + 1                                                               
      IZY = IZY + 1                                                             
      IK = IK + 4                                                               
      D31 = HISTQ( KA+1 )                                                       
      D32 = HISTQ( IY+1 )                                                       
      D21 = SHP( IK-2 )                                                         
      D22 = SHP(  IK  )                                                         
      HISTQ(IZY) = HISTQ(IZY) + SHP(IK-3) * D11 + SHP(IK-1) * D12       M38.3297
     1  + D21 * D31 + D22 * D32 + S1                                    M37.5442
      S1 = D21 * D11 + D22 * D12                                        M37.5443
      D11 = D31                                                         M37.5444
      D12 = D32                                                         M37.5445
 210  CONTINUE                                                          M37.5446
      S1 = S1 + SHP( IK+1 ) * D11                                       M38.3298
 211  HISTQ( IZY+1 ) = HISTQ( IZY+1 ) +  SHP( IK+3 ) * D12  +  S1       M38.3299
 212  KC = N22                                                          M37.5449
      IK = IKV                                                          M37.5450
      IZY = IZ + NUM2                                                           
      DO 213 KA = IZ, IKV                                               M37.5452
      IK = IK + 1                                                       M37.5453
      HISTQ( IZY ) = HISTQ( KA ) + DELTA2 * HISTQ( IK )                 M37.5454
      KC = KC + 1                                                       M37.5455
      D6 = SHP( KC )                                                            
      SHP( KC ) = HISTQ( IK )                                           M38.3301
      HISTQ( IK ) = HISTQ( IK ) * 2.0 - D6                                      
 213  IZY = IZY + 1                                                     M37.5459
      IF( IPRSUP .LT. 1 )   GO   TO   214                               M37.5460
      IJ = IU + 1                                                       M37.5461
      IK = IU  + NUM6                                                   M38.3302
      WRITE( LUNIT6, 4107 )  IJ, IK, ( HISTQ( KA ), KA = IJ, IK )       M39.3008
 4107 FORMAT( 2X,  12HHISTQ AT 213,  2I8  ,/, ( 1X, 5E22.13) )          M39.3009
C     FINISH THE PREDICTION OF SPEED AND ANGLE   ********************** M37.5465
 214  ALPHA = 9.0 * ( CU( IKN+21 ) - DANG ) + CU( IKN+22 ) + DELTA6     M38.3303
     1 * ( D2  + CU( IKN+23 ) )                                                 
      CU( IKN+22 ) = CU( IKN+21 )                                               
      CU( IKN+21 ) = DANG                                                       
      CU( IKN+23 ) = D2                                                         
      IF( ISMDAT( I30+8 ) .EQ. 0 )   GO  TO 946                                 
C     UPDATE THE SATURATION STATUS VARIABLES   *   *   *   *   *   *   *M32.6080
      SF4 = A3 * ELP( I26+21 ) + C1 + C2                                M37.5472
      SF5 = ( A4 * ELP( I26+21 ) + C3 + C4 ) * ELP( I75+4 )             M38.3308
      SF4 = SQRTZ( SF4**2 + SF5**2 )                                    M32.6083
      IF( ELP( I26+22 ) .GE. SF4 )  GO  TO  938                         M37.5474
      D9 = SF4 / ELP( I26+22 ) - 0.9                                    M37.5475
      ISD =  10.0 * D9                                                  M34. 160
      ISD = ( ISD + 1 ) / 2                                             M37.5476
      ELP(I75+2) = 1.0 / ( 1.0 + ELP(I26+23) * ( SF4 - ELP(I26+22) ))   M38.3309
      GO TO 939                                                         M32.6088
 938  ISD = 0                                                           M32.6089
      ELP( I75+2 ) = 1.0                                                        
 939  IF( ELP( I26+24 ) .GE. SF4 )  GO  TO 941                          M37.5479
      D9 = SF4 / ELP( I26+24 ) - 0.9                                    M37.5480
      ISQ =  10.0 * D9                                                  M34. 161
      ISQ = ( ISQ + 1 ) / 2                                             M37.5481
      ELP(I75+3) = 1.0 / ( 1.0 + ELP(I26+25) * ( SF4 - ELP(I26+24) ))   M38.3311
      GO TO 942                                                         M32.6095
 941  ISQ = 0                                                           M32.6096
      ELP( I75+3 ) = 1.0                                                        
 942  IF( ISD .EQ. ISMDAT( I30+9 ) .AND. ISQ .EQ. ISMDAT( I30+10 ) )    M37.5484
     1  GO  TO  944                                                     M37.5485
      ISMDAT( I30+9 ) = ISD                                             M37.5486
      ISMDAT( I30+10 ) = ISQ                                            M37.5487
      CALL INCREM( ILK, SF4 )                                           M32.6101
      IALTER = 1                                                        M32.6102
      IDSAT = IDSAT + 1                                                 M32.6103
 944  ACDE = ELP( I75+2 )                                               M38.3313
      ACDF = ELP( I75+3 )                                               M38.3314
C     CALCULATE STATOR FLUX LINKAGES.  *   *   *   *   *   *   *   *   *M32.6106
 946  AC1  = -AC1 * ACDF  -  ELP( I26+19 ) * A4                                 
      AC2  =  AC2 * ACDE  +  ELP( I26+19 ) * A3                                 
C     PREDICT FOR THE NEXT TIME-STEP THE NEW STATOR  CURRENTS********** M37.5492
C     ACUR1 = 2.0 * A3 - CU( IKN+7 )    !D-AXIS PREDICTED CURRENT       M37.5493
C     CU( IKN+7 ) = A3                                                  M37.5494
C     ACUR2 = 2.0 * A4 - CU( IKN+8 )    !Q-AXIS PREDICTED CURRENT       M38.3315
      ACUR2 = ( 2.5 * A4 - 1.5 * CU( IKN+13 ) + CU( IKN+8 ) ) * ONEHAF  M38.3316
      CU( IKN+13 ) = CU( IKN+8 )                                                
      CU( IKN+8 ) = A4                                                          
C     UPDATE THE FIELD VOLTAGE ( IF TACS CONTROLLED )****************** M37.5497
      Q4 = CU( IKN+10 )                                                         
      JMSET = ISMDAT( I30+15 )                                                  
      IF( JMSET  .EQ.  0  )   GO  TO  948                                       
      NDX1 = KXTCS + JMSET                                              M32.6127
      CU( IKN+11 ) = XTCS( NDX1 )                                       M37.5501
      Q4 = Q4 * CU( IKN+11 )                                                    
C     CALCULATE HISTORY TERMS FOR THE ELECTRICAL PART  * * * * * * * * *M20.4645
 948  ACD = ELP(I75+17)*A3 + ELP(I75+18)*C1 + ELP(I75+19)*C2            M38.3319
      ACD = ACD + ( AC1 * D2 - CV1 ) * DAMRAT                           M38.3320
      ACQ = ELP(I75+20)*A4 + ELP(I75+21)*C3 + ELP(I75+22)*C4            M38.3321
      ACQ = ACQ + ( AC2 * D2 - CV2 ) * DAMRAT                           M38.3322
      X1( 3 ) = ELP( I26+18 ) * CU( IKN+2 ) - CV3 * DAMRAT              M38.3323
      X1( 4 ) = ELP(I75+23)*A3 + ELP(I75+24)*C1 + ELP(I75+25)*C2        M38.3324
      X1( 5 ) = ELP(I75+26)*A3 + ELP(I75+27)*C1 + ELP(I75+28)*C2        M38.3325
      X1( 6 ) = ELP(I75+29)*A4 + ELP(I75+30)*C3 + ELP(I75+31)*C4        M38.3326
      X1( 7 ) = ELP(I75+32)*A4 + ELP(I75+33)*C3 + ELP(I75+34)*C4        M38.3327
      IF( IPRSUP .LT. 1 )   GO   TO  952                                M37.5510
      WRITE( LUNIT6, 4108 )   ILK, ACD, ACQ, ( X1( ITQ ), ITQ = 3, 7 )  M39.3010
 4108 FORMAT( 2X, 29H AT 16 HISTORY TERMS MACH NO., I5,/,(2X,6E19.11))  M39.3011
      WRITE( LUNIT6, 4109 )  ACUR1, ACUR2                               M39.3012
 4109 FORMAT( 2X, 26H PREDICTED STATOR CURRENTS, 2X, 2E25.12 )          M39.3013
 952  X1( 4 ) = X1( 4 ) - ( Q3*DAMRAT + Q4 )                            M37.5514
C     ACCOUNT FOR THE INFLUENCE OF  ROTOR CIRCUITS  *   *   *   *   *   M32.6148
      SF4 = - ( ELP(I75+43) * X1( 4 ) + ELP(I75+44) * X1( 5 ) )         M38.3331
      SF5 = - ( ELP(I75+45) * X1( 6 ) + ELP(I75+46) * X1( 7 ) )         M38.3332
      X1( 1 ) = ACD + SF4                                               M37.5517
      X1( 2 ) = ACQ + SF5                                               M37.5518
      CALL MOVER( X1( LL1 ), CU( IKN ), LL7 )                           M37.5519
C     ADD CORRECTION TERMS( ACCOUNT FOR ASSYMETRY)     *****************M33.1680
C     SF4 = SF4 - AC( IL ) * ACUR1       !NO D-AXIS CORRECTION TERM     M38.3333
      SF5 = SF5 - ELP( I75+52 ) * ACUR2                                 M38.3334
C     PERFORM REMAINING OPERATIONS IN THE SYNCHRONOUS FRAME*************M36.1555
      A1 = CU( IKN+18 ) - DANG                                          M38.3335
      A2 = SINZ( A1 )                                                   M36.1557
      A1 = COSZ( A1 )                                                   M36.1558
      C1 = A1 * AC2 + A2 * AC1                                          M36.1559
      C2 = A2 * AC2 - A1 * AC1                                          M36.1560
C     PREDICT SPEED VOLTAGES******************************************* M36.1561
      AC2 = ( 2.5 * C1 - 1.5 * CU( IKN+14 ) + CU( IKN+15 ) ) * OM2      M38.3336
      AC1 = ( 2.5 * C2 - 1.5 * CU( IKN+16 ) + CU( IKN+17 ) ) * OM2      M38.3337
      CU( IKN+14 ) = CU( IKN+15 )                                               
      CU( IKN+15 ) = C1                                                         
      CU( IKN+16 ) = CU( IKN+17 )                                               
      CU( IKN+17 ) = C2                                                         
C     CONVERT REMAINING VARIABLES TO SYNCHRONOUS FRAME******************M36.1566
      Q3 = COSZ( ALPHA )                                                M38.3342
      Q4 = SINZ( ALPHA )                                                M38.3343
      CU( IKN+18 ) = CU( IKN+18 ) + OMDT                                        
      CU( IKN+19 ) = Q3                                                 M38.3345
      CU( IKN+20 ) = Q4                                                 M38.3346
      A5 = CU( IKN+18 )                                                         
      C1 = COSZ( A5 )                                                   M38.3348
      C2 = SINZ( A5 )                                                   M38.3349
      A3 = C1 * Q3 + C2 * Q4                                                    
      A4 = C2 * Q3 - C1 * Q4                                                    
      Q3 = A3 * SF4 - A4 * SF5                                          M38.3350
      Q4 = A4 * SF4 + A3 * SF5                                          M38.3351
C     CONVERT  VOLTAGE SOURCES TO CURRENT SOURCES  * * * * * * * * * * *M20.4683
      A3 = ( A1 * ACD - A2 * ACQ - AC1 + Q3 ) * ELP( I75+53 )           M38.3352
      A4 = ( A2 * ACD + A1 * ACQ + AC2 + Q4 ) * ELP( I75+53 )           M38.3353
      A5 = X1( 3 )  *  ELP( I75+54 )                                    M38.3354
C     CONVERT SYNCHRONOUS VARIABLES TO PHASE COORDINATES  **************M36.1582
      CIK( JUK ) = -A3 * C1 - A4 * C2 - A5                              M37.5533
      A1 = -C1 * ONEHAF                                                 M38.3355
      A2 = C2 * SQRT32                                                  M33.1683
      ETOT = A1 + A2                                                    M38.3356
      SUM = A1 - A2                                                     M38.3357
      A1 = -C2 * ONEHAF                                                 M38.3358
      A2 = C1 * SQRT32                                                  M33.1684
      C3 = A1 - A2                                                      M38.3359
      C4 = A1 + A2                                                      M38.3360
      CIK( JUK+1 ) = -ETOT * A3 - C3 * A4 - A5                          M37.5534
      CIK( JUK+2 ) = -SUM * A3 - C4 * A4 - A5                           M37.5535
      IBR = IBR + 3                                                     M23.4573
      IF ( IPRSUP  .GT.  0  )                                           M30. 722
     1 WRITE( LUNIT6, 4110 )   ALPHA, ( CIK(IP), IP = JUK, IBR )        M39.3014
 4110 FORMAT( 10X, E20.12, 4X, 3E20.12)                                 M39.3015
      A1 = ELP( I75 )                                                   M39.3016
      A2 = ELP(I75+1)                                                   M39.3017
      IF( IDELTA .EQ. 0 )   GO TO 897                                           
      A11 = ( A1 - A2 ) / 3.0                                           M39.3019
      A1 = A11 + A11                                                    M39.3020
      A2 = -A11                                                         M39.3021
 897  AP1 = AP1 + A1                                                            
      AP2 = AP2 + A2                                                            
C     END  OF  UPDATING  PROCEDURE * * *** * * * * * * * * * * * * * * *M20.4716
      IB = IB + NUM4                                                            
      IU = IKP + NUM4                                                           
      IKN = IKN + NWD                                                           
      I26 = I26 + 101                                                           
      I30 = I30 + 30                                                            
      IM = IM - 1                                                               
      IF ( IM .GT. 0 ) GO TO 800                                        M20.4718
      IF( IDSAT .EQ. 0 )    GO  TO   899                                M38.3370
C     UPDATE THE APPROPRIATE ELEMENTS OF THE MATRIX (YKM)  *************M32.6151
      IES = J75 + 54                                                            
      IFS = J30 + 20                                                    M37.5544
      N1 = ISMDAT( IFS+1 )                                              M38.3372
      YKM( N1 ) = ELP( IES+1 ) + AP1                                    M38.3373
      N1 = ISMDAT( IFS+4 )                                              M38.3374
      YKM( N1 ) = ELP( IES+4 ) + AP2                                    M38.3375
      N1 = ISMDAT( IFS+7 )                                              M38.3376
      YKM( N1 ) = ELP( IES+7 ) + AP2                                    M38.3377
      N1 = ISMDAT( IFS+2 )                                              M38.3378
      YKM( N1 ) = ELP( IES+2 ) + AP2                                    M38.3379
      N1 = ISMDAT( IFS+5 )                                              M38.3380
      YKM( N1 ) = ELP( IES+5 ) + AP1                                    M38.3381
      N1 = ISMDAT( IFS+8 )                                              M38.3382
      YKM( N1 ) = ELP( IES+8 ) + AP2                                    M38.3383
      N1 = ISMDAT( IFS+3 )                                              M38.3384
      YKM( N1 ) = ELP( IES+3 ) + AP2                                    M38.3385
      N1 = ISMDAT( IFS+6 )                                              M38.3386
      YKM( N1 ) = ELP( IES+6 ) + AP2                                    M38.3387
      N1 = ISMDAT( IFS+9 )                                              M38.3388
      YKM( N1 ) = ELP( IES+9 ) + AP1                                    M38.3389
 899  J75 = J75 + 101                                                           
 900  J30 = J30 + 30                                                            
      NEXMOD = 0                                                                
      LL3 = 3                                                                   
      CALL MOVE0 ( KSMSPY(1), LL3 )                                             
      IF ( IPRSUP  .GT.  0 )                                            M36.1591
     1 WRITE( LUNIT6, 4111 )                                            M39.3024
 4111 FORMAT ( 24H  "EXIT  MODULE UPDATE." )                            M39.3025
      RETURN                                                            M20.4721
      END                                                               M20.4722
      SUBROUTINE INCREM( ILK, SF3 )                                     M32.6164
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED ONLY BY BRANDWAJN (TYPE-59) S.M. MODEL        M31.3340
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      IF ( IPRSUP  .GE.  1 )                                            M32.6165
     1 WRITE( LUNIT6, 6000 )                                            M39.3026
 6000 FORMAT( 24H  "BEGIN MODULE INCREM." )                             M39.3027
      I30 = 30 * ILK - 29                                                       
      WRITE( LUNIT6, 6001 ) ILK, ISMDAT( I30+9 ), ISMDAT( I30+10 )      M39.3028
 6001 FORMAT( 4H ***, 10X, 11HMACHINE NO., I6, 10X,                     M39.3029
     1 31HBEGIN OPERATION ON SEGMENTS NO., 2X, 2I10 )                   M39.3030
      LL36 = 36                                                         M23.4610
      CALL MOVER0(Z(1),LL36)                                            M23.4611
      ACDE = 1.0                                                        M23.4612
      ACDF = 1.0                                                        M23.4613
      I26 = 101 * ILK - 100                                                     
      I75 = I26 + 26                                                            
C     START  WITH  INCREMENTAL  INDUCTANCES OF THE DIRECT AXIS**********M23.4615
      ASD = ELP( I26+21 )                                                       
      IF( SF3 .LE. ASD )     GO  TO  205                                M39.3031
C     D - AXIS  SATURATED **********************************************M23.4618
      SB = ISMDAT( I30+9 )                                              M37.5561
      SB = SB * 0.1                                                     M32.6172
      SF4 = ASD * ( .9 + SB )                                           M39.3032
      SF5 = ASD * ( 1.1 + SB )                                          M39.3033
      SF6 = SF4 / ( 1.0 + ELP( I26+22 ) * ( SF4 - ASD ) )               M37.5563
      SF7 = SF5 / ( 1.0 + ELP( I26+22 ) * ( SF5 - ASD ) )               M37.5564
      ACDE = ( SF7 - SF6  ) / ( SF5 - SF4 )                             M23.4624
C     CALCULATE INCREMENTAL INDUCTANCES FOR THE QUADRATURE AXIS ********M23.4626
 205  ASD = ELP( I26+23 )                                                       
      IF( SF3 .LE. ASD )   GO  TO  206                                  M33.1696
C     Q - AXIS  SATURATED **********************************************M23.4629
      SB = ISMDAT( I30+10 )                                             M37.5566
      SB = SB * 0.1                                                     M32.6179
      SF4 = ASD * ( .9 + SB )                                           M39.3034
      SF5 = ASD * ( 1.1 + SB )                                          M39.3035
      SF6 = SF4 / ( 1.0 + ELP( I26+24 ) * ( SF4 - ASD ) )               M37.5568
      SF7 = SF5 / ( 1.0 + ELP( I26+24 ) * ( SF5 - ASD ) )               M37.5569
      ACDF = ( SF7 - SF6  ) / ( SF5 - SF4 )                             M23.4635
 206  Z( 50 ) = ( ELP(  I26  ) * ACDE + ELP( I26+18 ) ) * FACTOM        M38.3398
      Z( 51 ) = ( ELP( I26+8 ) * ACDF + ELP( I26+18 ) ) * FACTOM        M38.3399
      Z( 52 ) = ELP( I26+2 ) + ELP( I75+35 ) * ACDE                     M38.3400
      Z( 53 ) = ELP( I26+5 ) + ELP( I75+36 ) * ACDE                     M38.3401
      Z( 54 ) = ELP( I26+10 ) + ELP( I75+37 ) * ACDF                    M38.3402
      Z( 55 ) = ELP( I26+13 ) + ELP( I75+38 ) * ACDF                    M38.3403
C     FILL IN  THE  THE  TRANSFORMER TERMS   OF  THE RESISTIVE HISTORY**M23.4642
C     MATRIX OF  THE  GENERATOR*****************************************M23.4643
      Z( 1 ) = Z( 50 ) + ELP( I26+19 )                                  M37.5576
      Z( 8 ) = Z( 51 ) + ELP( I26+19 )                                  M37.5577
      Z( 15 ) = Z( 52 ) + ELP( I26+6 )                                  M37.5578
      Z( 22 ) = Z( 53 ) + ELP( I26+7 )                                  M37.5579
      Z( 29 ) = Z( 54 ) + ELP( I26+14 )                                 M37.5580
      Z( 36 ) = Z( 55 ) + ELP( I26+15 )                                 M37.5581
      Z( 11 ) = ELP( I75+41 ) * ACDF                                    M38.3404
      Z( 26 ) = Z( 11 )                                                 M23.4651
      Z( 12 ) = ELP( I75+42 ) * ACDF                                    M38.3405
      Z( 32) = Z( 12 )                                                  M37.5582
      Z( 30 ) = ELP( I26+12 ) * ACDF                                    M37.5583
      Z( 35 ) = Z( 30 )                                                 M23.4655
      Z( 3 ) = ELP( I75+39 ) * ACDE                                     M38.3406
      Z( 13 ) = Z( 3 )                                                  M23.4657
      Z( 4 ) = ELP( I75+40 ) * ACDE                                     M38.3407
      Z( 19 ) = Z( 4 )                                                  M23.4659
      Z( 16 ) = ELP( I26+4 ) * ACDE                                     M37.5584
      Z( 21 ) = Z( 16 )                                                 M23.4661
      IF( IPRSUP .LT. 1 )    GO  TO  208                                M39.3036
      DO 407  K = 1, 6                                                  M39.3037
      N1 = ( K - 1 ) * 6 + 1                                            M38.3408
      N2 = N1 + 5                                                       M38.3409
 407  WRITE( LUNIT6, 6004 )    K, ( Z(I), I = N1, N2 )                  M39.3038
 208  CALL MOVER( Z(1), X1( 1 ), LL36 )                                 M39.3039
      LL7 = 6                                                           M23.4678
      LL3 = 2                                                           M23.4679
      CALL REDUSM( X1( 1 ), LL7, LL3 )                                  M38.3410
      IF( IPRSUP .LE. 0 )   GO  TO  210                                 M39.3040
      WRITE( LUNIT6, 6003 )   ILK                                       M39.3041
 6003 FORMAT ( /, 23H REDUCED NETWORK MODEL., I10 )                     M39.3042
      DO 409  K = 1, 6                                                  M39.3043
      N1 = ( K - 1 ) * 6 + 1                                            M38.3411
      N2 = N1 + 5                                                       M38.3412
 409  WRITE( LUNIT6, 6004 )   K, ( X1(I), I = N1, N2 )                  M39.3044
 6004 FORMAT ( /, 11H NEW COLUMN, I6, /, ( 1X, 7E17.8 )  )              M39.3045
 210  A = X1( 1 )                                                       M39.3046
      B = X1( 8 )                                                       M23.4691
      SF6 = A                                                           M33.1699
      SF7 = B                                                           M33.1700
C     SUM=(A+B) * ONEHAF                                                M37.5585
C     A=A-SUM                                                           M37.5586
C     X1(1)=A                                                           M37.5587
C     X1( 8 ) = -A                                                      M37.5588
      SUM = A                                                           M37.5589
      X1( 8 ) = B - A                                                   M37.5590
      ETOT = 1.0 / ELP( I26+16 )                                        M37.5591
      A = 1.0 / SUM                                                     M32.6182
      B = 1.0 / ETOT                                                    M23.4707
      SUM = ( B + 2.0 * A ) / 3.0                                       M38.3414
      ETOT  =  ( B - A ) / 3.0                                          M23.4709
      ELP( I75 ) = SUM                                                  M38.3415
      ELP(I75+1) = ETOT                                                 M38.3416
      ELP(I75+53) = A * ATHTW                                           M38.3417
      ELP(I75+54) = B * ASQRT3                                          M38.3418
      IF( IPRSUP .LE. 0 )   GO  TO  212                                 M39.3047
      WRITE( LUNIT6, 6005 )  ELP(I75), ELP(I75+1), ELP(I75+53),         M39.3048
     1 ELP(I75+54)                                                      M38.3420
 6005 FORMAT( 3X, 8E15.7 )                                              M39.3049
C     CORRECTION TERMS FOR VOLTAGE AND CURRENT CALCULATIONS*************M23.4718
 212  ELP( I75+5 ) = X1( 3 )                                            M39.3050
      ELP( I75+6 ) = X1( 4 )                                            M38.3422
      ELP( I75+7 ) = X1( 11 )                                           M38.3423
      ELP( I75+8 ) = X1( 12 )                                           M38.3424
      ELP( I75+9 ) = X1( 15 )                                           M38.3425
      ELP( I75+10 ) = X1( 21 )                                          M38.3426
      ELP( I75+11 ) = X1( 16 )                                          M38.3427
      ELP( I75+12 ) = X1( 22 )                                          M38.3428
      ELP( I75+13 ) = X1( 29 )                                          M38.3429
      ELP( I75+14 ) = X1( 35 )                                          M38.3430
      ELP( I75+15 ) = X1( 30 )                                          M38.3431
      ELP( I75+16 ) = X1( 36 )                                          M38.3432
      ELP( I75+43 ) = X1( 13 )                                          M38.3433
      ELP( I75+44 ) = X1( 19 )                                          M38.3434
      ELP( I75+45 ) = X1( 26 )                                          M38.3435
      ELP( I75+46 ) = X1( 32 )                                          M38.3436
      ELP( I75+47 ) = 1.0 / SF6                                         M38.3437
      ELP( I75+48 ) = ELP( I26+18 ) * ELP( I75+39 )                     M38.3438
      ELP( I75+49 ) = SF7                                               M38.3439
      ELP( I75+52 ) = X1( 8 )                                                   
      IF( IPRSUP .LT. 1 )   GO TO 214                                   M39.3051
      IDP = IN                                                          M23.4748
      IN = IN + 7                                                       M23.4749
      IDS =  IDT + 3                                                    M23.4750
      WRITE( LUNIT6, 6006 )  ILK                                        M39.3052
 6006 FORMAT( 5X, 20H ARRAYS FOR MACH NO., I5, 17H  IN ORDER OF AC ,    M39.3053
     1 1X, 4HA21,, 1X, 4HA22,, 1X )                                     M39.3054
      WRITE( LUNIT6, 6005 )  ELP(I75+2), ELP(I75+3), ELP(I75+51),       M39.3055
     1 ELP(I75+52)                                                      M38.3445
      IDT = I75 + 5                                                     M38.3447
      IDS = IDT + 3                                                     M38.3448
      WRITE( LUNIT6, 6005 )   ( ELP( IU ),  IU = IDT, IDS )             M39.3056
      IDS = IDS + 1                                                     M38.3450
      IDT = IDS + 7                                                     M38.3451
      WRITE( LUNIT6, 6005 )   ( ELP( IU ),  IU = IDS, IDT )             M39.3057
C     CALCULATE THE RESISTIVE MATRIX FOR HISTORY CALCULATIONS           M23.4759
C     STORE  CONSTANTS FOR FUTURE USE IN THE TIME-STEP LOOP ************M23.4760
 214  ELP( I75+17 ) = Z( 50 ) - ELP( I26+19 ) * DAMRAT                  M39.3058
      ELP( I75+18 ) = Z( 13 )                                           M38.3454
      ELP( I75+19 ) = Z( 19 )                                           M38.3455
      ELP( I75+20 ) = Z( 51 ) - ELP( I26+19 ) * DAMRAT                  M38.3456
      ELP( I75+21 ) = Z( 26 )                                           M38.3457
      ELP( I75+22 ) = Z( 32 )                                           M38.3458
      ELP( I75+23 ) = Z(  3 )                                           M38.3459
      ELP( I75+24 ) = Z( 52 ) - ELP( I26+6 ) * DAMRAT                   M38.3460
      ELP( I75+25 ) = Z( 21 )                                           M38.3461
      ELP( I75+26 ) = Z(  4 )                                           M38.3462
      ELP( I75+27 ) = Z( 16 )                                           M38.3463
      ELP( I75+28 ) = Z( 53 ) - ELP( I26+7 ) * DAMRAT                           
      ELP( I75+29 ) = Z( 11 )                                           M38.3465
      ELP( I75+30 ) = Z( 54 ) - ELP( I26+14 ) * DAMRAT                          
      ELP( I75+31 ) = Z( 35 )                                           M38.3467
      ELP( I75+32 ) = Z( 12 )                                           M38.3468
      ELP( I75+33 ) = Z( 30 )                                           M38.3469
      ELP( I75+34 ) = Z( 55 ) - ELP( I26+15 ) * DAMRAT                          
      IF( IPRSUP .LT. 1 )    GO TO  216                                 M39.3059
      WRITE( LUNIT6, 6007 )  ILK                                        M39.3060
 6007 FORMAT( /, 33H HISTORY MATRIX FOR GENERATOR NO.,  I5  )           M39.3061
      IN = I75 + 9                                                      M38.3471
      DO 410 K = 1, 5                                                   M39.3062
      IN = IN + 7                                                       M23.4826
      N2 = IN + 7                                                       M23.4827
      N1 = IN + 1                                                       M23.4828
 410  WRITE( LUNIT6, 6008 )    IN, ( ELP( I ), I = N1, N2 )             M39.3063
 6008 FORMAT( 1X, 12HNEW  ROW    ,I6, /, (1X, 7E17.8) )                 M39.3064
      WRITE( LUNIT6, 6009 )                                             M39.3065
 6009 FORMAT ( 24H  "EXIT  MODULE INCREM." )                            M39.3066
 216  RETURN                                                            M39.3067
      END                                                               M23.4832
      SUBROUTINE REDUSM(X,M,N)                                          M23.4833
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS ROUTINE IS A COPY  OF AN IDENTICAL ROUTINE USED IN OVER12****M23.4834
C     IT USES GAUSS-JORDAN ELIMINATION PROCESS FOR BOTH MATRIX INVERSIONM23.4835
C     AND MATRIX RDUCTION( ELIMINATION OF VARIABLES)********************M23.4836
      DIMENSION  X(1), A1(6)                                            M23.4837
      J = M                                                             M23.4838
      IK = M**2                                                         M23.4839
      NK = IK - M                                                       M23.4840
      M1=M + 1                                                          M23.4841
 1    C=1.0 / X( IK )                                                   M23.4842
      DO 2 K=1,M                                                        M23.4843
 2    A1(K)=X(NK+K)                                                     M23.4844
      K = 1                                                             M23.4845
    4 MK=(K-1)*M                                                        M23.4846
      B=-X(MK+J)*C                                                      M23.4847
      I = 1                                                             M23.4848
 3    MK=MK+1                                                           M23.4849
      X(MK)=X(MK)+B*A1(I)                                               M23.4850
      I = I + 1                                                         M23.4851
      IF(I.LE.M) GO TO 3                                                M23.4852
      X(MK-M+J)=B                                                       M23.4853
      K = K + 1                                                         M23.4854
      IF(K.EQ.J) K=K+1                                                  M23.4855
      IF(K.LE.M) GO TO 4                                                M23.4856
      DO 5 K=1,M                                                        M23.4857
  5   X(NK+K)=A1(K)*C                                                   M23.4858
      X(IK)=C                                                           M23.4859
      J=J-1                                                             M23.4860
      IK = IK - M1                                                      M23.4861
      NK=NK-M                                                           M23.4862
      IF(J.GT.N) GO TO 1                                                M23.4863
      RETURN                                                            M23.4864
      END                                                               M23.4865
      SUBROUTINE BANSOL( AB, X, N )                                     M37.5622
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS ROUTINE PERFORMS FORWARD AND BACKWARD SOLUTION WITH          M37.5623
C     WITH A TRIDIAGONAL SYMMETRIC MATRIX 'AB'. FOR DETAIL OF           M37.5624
C     STORAGE ARRANGEMENTS FOR 'AB' SEE SUBROUTINE 'BANDEL'.            M37.5625
      DIMENSION AB( 1 ), X( 1 )                                         M37.5626
C     INITIALIZATION ************************************************** M37.5627
      I2 = 2                                                            M37.5628
      I1 = 1                                                            M37.5629
C     FORWARD SUBSTITUTION ( DOWNWARDS ) ****************************** M37.5630
      D = X( 1 )                                                                
 10   X( I1 ) = D * AB( I2-1 )                                                  
      IF( I1  .EQ.  N )  GO TO 20                                               
      I1 = I1 + 1                                                       M37.5634
      D = X( I1 )  -  D * AB( I2 )                                              
      I2 = I2 + 2                                                       M37.5636
      GO TO 10                                                                  
C     BACK SUBTITUTION ( UPWARDS ) ************************************ M37.5638
 20   IF( I1  .EQ.  1 )  GO TO 40                                               
      I2 = I2 - 2                                                       M37.5640
      I1 = I1 - 1                                                       M37.5641
      X( I1 ) = X( I1 ) - X( I1+1 ) * AB( I2 )                                  
      GO TO 20                                                                  
 40   RETURN                                                            M37.5644
      END                                                               M37.5645
      SUBROUTINE REDU17 ( A, N, M )                                     M05. 689
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    FOR AN EXPLANATION ABOUT THE PARAMETERS OF THIS SUBROUTINE,       M15.1297
C)    SEE COMMENT CARDS IN SAME MODULE  'REDUCT'  OF OVERLAY 12 .       M15.1298
      DIMENSION  A(1), B(100)                                              13569
      J = N + 1                                                            13570
      W=1.0                                                                13571
      IF(M.GT.0) W=-W                                                      13572
      IJ=N*J/2                                                             13573
    3 J=J-1                                                                13574
      IF(J.EQ.M) RETURN                                                    13575
      H1=-1.0/A(IJ)                                                        13576
      B(J)=H1                                                              13577
      IJ=IJ-J                                                              13578
      K=0                                                                  13579
      IK=0                                                                 13580
C                                   BEGIN K-LOOP                           13581
    4 IK=IK+K                                                              13582
      I1=IK+1                                                              13583
      K=K+1                                                                13584
      IF(K.GT.N) GO TO 3                                                   13585
      IF(K.LT.J) GO TO 9                                                   13586
      IF(W.LT.0.) GO TO 3                                                  13587
      IF(K.EQ.J) GO TO 7                                                   13588
      I=IK+J                                                               13589
    5 H2=A(I)                                                              13590
      B(K)=H2*H1                                                           13591
C                                   BEGIN I-LOOP                           13592
      I2=IK+K                                                              13593
      L=0                                                                  13594
      DO 6 I=I1,I2                                                         13595
      L=L+1                                                                13596
    6 A(I)=A(I)+B(L)*H2                                                    13597
      IF(K.LT.J) GO TO 4                                                   13598
      I=IK+J                                                               13599
      A(I)=B(K)                                                            13600
      GO TO 4                                                              13601
C                                   END I-LOOP                             13602
    7 I=IJ                                                                 13603
      DO 8 L=1,J                                                           13604
      I=I+1                                                                13605
    8 A(I)=B(L)                                                            13606
      GO TO 4                                                              13607
C                                   END K-LOOP                             13608
    9 I=IJ+K                                                               13609
      GO TO 5                                                              13610
      END                                                                  13611
      SUBROUTINE SUBTS3                                                    13613
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      DIMENSION  XX(1),  VOLTA(1)                                       M31.3342
      DIMENSION  NSUBKM(1)                                              M37.5646
      EQUIVALENCE  ( KKNONL(1), NSUBKM(1) )                             M37.5647
      EQUIVALENCE     (H1, SK),     (D2, SM )                              13618
      EQUIVALENCE (  XK(1), XX(1) ),  ( VOLTI(1), VOLTA(1) )            M31.3343
      EQUIVALENCE   ( MONCAR(2), KBASE )                                        
      EQUIVALENCE   ( IPRSOV(36), IUPPER )                              M39.3068
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      DIMENSION VSMOUT(1)                                               M39.3069
      EQUIVALENCE ( ISMOUT(1), VSMOUT(1) )                              M39.3070
C     INTRINSIC  ABSZ, COSZ, EXPZ                                       M32.6197
      LL2 = 2                                                           M29.2093
      LL6 = 6                                                           M29.2094
      LL8 = 8                                                           M22.4996
      LL10 = 10                                                         M22.4997
      IF ( PEAKND(1)  .EQ.  0.0 )   GO TO 1003                          M25. 907
      IF ( BEGMAX(MAXOUT)  .GT.  T )   GO TO 1003                       M31.3345
      D1 = ABSZ( PEAKND(1) )                                            M25. 908
      DO 1002  J=2, NTOT                                                M25. 909
      IF ( ABSZ( E(J) )  .LE.  D1 )   GO TO 1002                        M25. 910
      PEAKND(1) = E(J)                                                  M25. 911
      D1 = ABSZ( E(J) )                                                 M25. 912
      PEAKND(2) = T                                                     M25. 913
      PEAKND(3) = J                                                     M25. 914
 1002 CONTINUE                                                          M25. 915
 1003 IF ( ISTEP  .LT.  LIMSTP )   GO TO 12                             M25. 916
      ISPRIN = 0                                                           13621
      IOUT = MULTPR(INDSTP)                                                13622
      IF (IOUT .GE. 0) GO TO 1010                                       M20.4753
      IOUT = -IOUT                                                      M20.4754
      DO 1005  J=1, 4                                                   M28.5545
      N13 = IPRSOV(J+15)                                                M28.5546
      IPRSOV(J+15) = IPRSOV(J+30)                                       M28.5547
 1005 IPRSOV(J+30) = N13                                                M28.5548
      IPRSUP = IPRSOV(NCHAIN)                                           M28.5549
 1010 INDSTP = INDSTP + 1                                               M20.4759
      LIMSTP = KPRCHG(INDSTP)                                              13624
   12 N7 = 0                                                            M20.4760
      IF ( T .LE. TMAX )  GO TO 14                                      M36.1592
      N7 = 1                                                            M36.1593
      IF ( NENERG  .NE.  0 )   GO TO 14                                 M22.4998
      ISPRIN = 0                                                        M20.4766
      ISPLOT = 0                                                        M20.4767
   14 IF ( BEGMAX(1)  .EQ.  1.0 )   GO TO 13                            M31.3346
      IF ( KONCUR .EQ. 1 ) GO TO 13                                             
      IF( NPOWER .GT. 0 )  GO TO 13                                        13626
      IF( ISPRIN .LE. 0 )  GO TO 13                                        13627
      IF( ISPLOT .GT. 0 )  GO TO 1660                                      13628
   13 IF( NV .EQ. 0 )  GO TO 1202                                          13629
      DO 1201 K=1,NV                                                       13630
      N1 = IBRNCH(K)                                                            
      N2 = JBRNCH(K)                                                            
 1201 BVALUE(K)=E(N1)-E(N2)                                                     
 1202 VOLTI(1) = T                                                         13634
      IF( IVOLT .EQ. 1 )  GO TO 1610                                            
      K = 1                                                                     
 1600 IF ( K  .GT.  NUMNVO )   GO TO 1630                                       
      I = IBSOUT(K)                                                        13638
      K = K + 1                                                            13639
      VOLTI(K) = E(I)                                                           
      GO TO 1600                                                           13641
 1610 K = NTOT                                                             13642
      CALL MOVER ( E(LL2), VOLTI(LL2), NTOT1 )                          M35.1627
 1630 IF ( NC  .EQ.  0 ) GO TO 1642                                             
      NODEV = K                                                            13645
      DO 1640 I=1,NC                                                            
      K=K+1                                                                     
 1640 VOLTI(K)=BVALUE(I)                                                        
 1642 IF (IAVERG .EQ. 0) GO TO 1652                                     M28.5551
      IF ( ISTEP  .EQ.  0 )   GO TO 1657                                M27.1463
      DO 1651 I = 2, K                                                  M27.1464
      SDLAT = VOLTI(I)                                                  M27.1465
      NDX1 = LSIZ26 + I                                                 M31.3347
      VOLTI(I) = ONEHAF * ( SDLAT + VOLTA(NDX1) )                       M31.3348
 1651 VOLTA(NDX1) = SDLAT                                               M31.3349
      GO TO 1652                                                        M27.1469
 1657 DO 1658 I = 2, K                                                  M27.1470
      NDX1 = LSIZ26 + I                                                 M31.3350
 1658 VOLTI(NDX1) = VOLTI(I)                                            M31.3351
 1652 IF (NPOWER .LE. 0) GO TO 54280                                    M22.5009
      DO 54272  I=1, NPOWER                                                     
      MPOWER = MAXPE + I                                                M35.1628
      J = KOUTVP(I)                                                        13651
      IF( J .GT. 0 )  GO TO 54262                                          13652
      J = -J                                                               13653
      N1 = IBRNCH(J)                                                       13654
      D4 = E(N1)                                                           13655
      GO TO 54265                                                          13656
54262 D4 = BVALUE(J)                                                       13657
54265 L = KOUTVP(MPOWER)                                                M35.1629
      D1 = D4 * BVALUE(L)                                                  13659
      BNRG(I) = BNRG(I) + D1*DELTAT                                        13660
      IF( ISTEP .eq. 0 )   BNRG(I) =  DELTA2  * D1         
      D2 = BNRG(I) - D1 * DELTA2                                           13662
      N5 = J + NODEV                                                    M16.3119
      N6 = L + NODEV                                                    M16.3120
      VOLTI(N5) = D1                                                    M16.3121
      VOLTI(N6) = D2                                                    M16.3122
      IF( ISTEP .LE. 5    .AND.    IPRSUP .GE. 2 )                         13665
     1 WRITE(LUNIT6, 54270)  I, K,   J  , L, D1, D2, BNRG(I), D4, BVALUEM17. 900
     +(L)                                                               M17. 901
54270 FORMAT( /,  9H AT 54270   , 4I10, 5E15.4 )                           13667
54272 CONTINUE                                                             13668
54280 IF ( NSMOUT  .EQ.  0 )   GO TO 54284                                      
      CALL MOVER ( VSMOUT(MSMOUT+1), VOLTI(K+1), NSMOUT )               M39.3071
      K = K + NSMOUT                                                    M16.3125
54284 IF ( IOUTCS  .EQ.  0 )   GO TO 1650                                       
      DO 318 J8 = 1, IOUTCS                                             M39.3072
      K = K + 1                                                         M39.3073
      NDX1 = KXTCS + JOUT( KJOUT + J8 )                                 M39.3074
  318 VOLTI(K) = XTCS( NDX1 )                                           M39.3075
 1650 IF ( NUMOUT  .EQ.  0 )   GO TO 1643                                       
      CALL MOVER ( SPUM(IUUMOU), VOLTI(K+1), NUMOUT )                   M27.1473
      K = K + NUMOUT                                                    M27.1474
 1643 IF( K .GT. 1 )  GO TO 1647                                                
      KILL = 44                                                            13671
      LSTAT(19) = 1650                                                     13672
      GO TO 9200                                                           13673
 1647 IF ( KONCUR .EQ. 1 )                                                      
     1 CALL SPYTAC                                                              
      IF( ISPLOT .GT. 0 )  GO TO 1653                                           
      IF ( M4PLOT  .EQ.  0 )   GO TO 1648                               M29.2095
      CALL PLTFIL ( K )                                                         
      GO TO 1649                                                        M29.2097
 1648 WRITE (LUNIT4) (VOLTI(I),I=1,K)                                   M29.2098
 1649 ISPLOT = IPLOT                                                            
 1653 IF( ISPRIN .GT. 0 )  GO TO 1654                                           
      IF (KBASE .GE. 2)   GO TO 1654                                            
      N1 = K                                                               13680
      IF( N1 .GT. 10 )  N1 = 10                                            13681
      IF ( ISTEP  .GT.  99999 )   ISTEP = ISTEP - 100000                M44.2301
      IF ( T  .GE.  UNITY )   GO TO 48                                  M15.1299
      IF ( DELTAT  .LT.  TENM6 )   GO TO 48                             M15.1300
      WRITE(LUNIT6, 37)  ISTEP, ( VOLTI(I), I=1, N1 )                   M17. 903
   37 FORMAT ( 1X, I5,  F9.6, 9E13.6 )                                  M11. 302
      GO TO 56                                                          M15.1301
   48 IF ( ISTEP  .LE.  9999 )   GO TO 39                               M20.4769
      WRITE (LUNIT6, 51)  ISTEP,  ( VOLTI(I), I=1, N1 )                 M20.4770
   51 FORMAT ( 1X, I5, E9.3, 9E13.6 )                                   M15.1303
      GO TO 56                                                          M20.4771
   39 WRITE (LUNIT6, 40)  ISTEP,  ( VOLTI(I), I=1, N1 )                 M20.4772
   40 FORMAT ( 1X,  I4,  E10.3,  9E13.6  )                              M20.4773
   56 IF ( K  .GT.  N1 )                                                M15.1304
     1 WRITE(LUNIT6, 87437)  ( VOLTI(I), I=11, K )                      M17. 905
87437 FORMAT ( 15X, 9E13.6 )                                            M11. 303
      ISPRIN = IOUT                                                        13687
 1654 IF( BEGMAX(1) .EQ. 0.0 ) GO TO 1660                                       
      IF ( T .LT. BEGMAX(MAXOUT) )  GO TO 1660                          M31.3353
C     NOW UPDATE EXTREMA VECTOR  "XMAX" .   THERE ARE 4                 M27.1476
C     PARTITIONS OF SIZE OF LIST 12:  (XMAX, TMAX, XMIN, TMIN).         M27.1477
      DO 1655  I=2, K                                                      13689
      L = I - 1                                                            13690
      NDX1 = 2 * LSIZ12 + L                                             M38.3478
      IF (VOLTI(I)  .GE.  XMAX(NDX1) )  GO TO 4655                      M27.1479
      XMAX(NDX1) = VOLTI(I)                                             M27.1480
      NDX1 = NDX1 + LSIZ12                                              M38.3479
      XMAX(NDX1) = T                                                    M27.1482
 4655 IF ( VOLTI(I)  .LE.  XMAX(L)  )  GO TO 1655                       M27.1483
      XMAX(L) = VOLTI(I)                                                M19.1619
      NDX1 = LSIZ12 + L                                                 M38.3480
      XMAX(NDX1) = T                                                    M27.1485
 1655 CONTINUE                                                             13692
      IF( IPRSUP .GE. 2 )                                               M31.3354
     1 WRITE (LUNIT6, 1656)  ( XMAX(I), I=1, L )                        M31.3355
 1656 FORMAT( /,  16H XMAX(I) AT 1656   ,/, ( 1X, 8E15.4 ) )               13695
 1660 IF ( N7  .EQ.  0 ) GO TO 1661                                             
      IF ( MEMSAV  .EQ.  0 ) GO TO 8000                                         
 1661 ISTEP = ISTEP + 1                                                         
      T = T + DELTAT                                                            
      IF ( T .GT. BEGMAX(MAXOUT+1) )   MAXOUT = MAXOUT + 2              M31.3357
      ISPLOT = ISPLOT - 1                                                       
      ISPRIN = ISPRIN - 1                                                       
      IF ( T .LT. SPTACS(29) ) GO TO 1945                                       
      SPTACS(29) = FLTINF                                               M42.6758
      IUTY(KIUTY+3) = IUTY(KIUTY+2)                                     M42.6759
 1945 IF ( IPRSUP  .GE.  4 )                                            M42.6760
     1 WRITE (LUNIT6, 7342)  K, IBR,  ( F(I), I=1, NTOT )               M20.4777
 7342 FORMAT ( /,  9H K, IBR =,  2I6,  10X,                             M20.4778
     1  30H(F(I), I=1, NTOT )  FOLLOW ...    ,/,  (1X, 8E16.7)  )       M20.4779
C                                 CONSTANT VOLTAGES AND CURRENTS           13702
      I=0                                                                  13703
      IF(IREAD.EQ.0) GO TO 1247                                                 
C     READ INPUT CARD USING CIMAGE.                                        13705
      CALL CIMAGE                                                          13706
      IF ( NCHAIN  .EQ.  16 )   GO TO 9999                              M29.2113
      WRITE (KUNIT6, 54169)                                             M35.1630
54169 FORMAT( 42H+ANOTHER INPUT CARD FOR TYPE 1-10 SOURCES.  )             13708
      IF ( KOLBEG  .GT.  0 )   GO TO 7823                               M22.5012
      DECODE(80, 10, ABUFF )  ( VOLTBC(J), J=1, 10 )                    M31.3358
   10 FORMAT(10E8.0)                                                       13710
      GO TO 7825                                                        M22.5013
 7823 NFRFLD = 10                                                       M22.5014
      CALL FREFLD ( VOLTBC(1) )                                         M31.3359
 7825 IF ( VOLTBC(1)  .NE.  9999. )   GO TO 11247                       M31.3360
      WRITE (KUNIT6, 2241)                                              M35.1631
 2241 FORMAT ( 42H+ANOTHER INPUT CARD FOR TYPE 1-10 SOURCES.,           M35.1632
     1   7H   END.    )                                                 M35.1633
      IREAD=0                                                              13712
 1247 CALL MOVER0 ( VOLTBC(1), LL10 )                                   M31.3361
11247 CALL INTERP                                                       M35.1634
      IF ( NSTACS  .EQ.  0 ) GO TO 11248                                        
      DO 21247  J=1, NSTACS                                             M12. 521
      N1 = VSTACS(J)                                                    M12. 522
      IF ( N1  .EQ.  0 )   GO TO 21247                                  M12. 523
      NDX1 = KXTCS  + N1                                                M22.5018
      VOLTBC(J) = XTCS(NDX1)                                            M31.3362
21247 CONTINUE                                                          M12. 525
11248 IF ( KANAL  .GT.  0 )                                                     
     1 CALL ANALYT                                                      M12. 527
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M12. 528
 1249 I=I+1                                                                     
      IF(I.GT.KCONST) GO TO 1300                                                
      IF ( IPRSUP  .GE.  4 )                                            M20.4782
     1 WRITE (LUNIT6, 7356)  I, KCONST,  ( F(M), M=1, NTOT )            M20.4783
 7356 FORMAT ( /, 12H I, KCONST =,  2I6,  10X,                          M20.4784
     1  30H(F(I), I=1, NTOT )  FOLLOW ...    ,/,  (1X, 8E16.7)  )       M20.4785
      N1=NODE(I)                                                                
      N2=IABS(N1)                                                               
      N3=IFORM(I)                                                               
      IF ( N3  .NE.  17 )  GO TO 7362                                           
      N8 = SFREQ(I)                                                             
      NDX1 = KXTCS  +  N8                                               M32.6201
      CREST(I+1) = CREST(I) * XTCS(NDX1)                                M32.6202
      GO TO 1249                                                        M32.6203
 7362 IF ( N3 .NE. 18 )   GO TO 7368                                            
C     FOLLOWING CODE IS FOR IDEAL TRANSFORMER AND SOURCE:               M32.6205
      N14 = TIME1(I)
      F(N14) = YX                                                       M32.6207
      GO TO 1249                                                        M32.6208
 7368 IF(N3.NE.16) GO TO 1251                                                   
      J=I+1                                                                13720
      N4=IABS(NODE(J))                                                     13721
      GUS1=E(N2)-E(N4)                                                  M05. 690
      N3=IFORM(J)                                                          13723
      GUS2 = TIME1(J)                                                   M10. 236
      GUS3 = TIME2(J)                                                   M10. 237
      CK1=CREST(J+1)-TSTOP(I)*GUS1                                      M05. 693
      GUS4=SFREQ(J)                                                     M05. 694
      SFREQ(J)=TSTART(J)+CREST(J)-CK1                                      13728
      IF(SFREQ(J).GT.GUS3) GO TO 1256                                   M05. 695
      IF(SFREQ(J).LT.GUS2) GO TO 1257                                   M05. 696
      TSTOP(J)=(SFREQ(J)-GUS4)/DELTA2-TSTOP(J)                          M05. 697
      IFORM(J)=1                                                           13732
 1258 TSTART(J)  =  SFREQ(I)*CK1  +  TIME1(I)*SFREQ(J)  +  TSTART(I)*   M12. 529
     1TSTOP(J)                                                             13734
      YX = CREST(I)+TSTART(J)                                              13735
      F(N4)=F(N4)-YX                                                       13736
      CREST(J+1)=YX                                                        13737
      I=J                                                                  13738
      GO TO 1250                                                           13739
 1256 SFREQ(J)=GUS3                                                     M05. 698
      TSTOP(J)=0.                                                          13741
      IFORM(J)=2                                                           13742
      GO TO 1258                                                           13743
 1257 SFREQ(J)=GUS2                                                     M05. 699
      TSTOP(J)=0.                                                          13745
      IFORM(J)=3                                                           13746
      GO TO 1258                                                           13747
 1251 YX=0.                                                                13748
      TS=T-TSTART(I)                                                       13749
      IF(TS.LT.0.) GO TO 1250                                              13750
      IF(T.GE.TSTOP(I)) GO TO 1250                                         13751
      K=IABS(N3)                                                           13752
      IF(K.EQ.14) GO TO 1270                                                    
      IF(K.EQ.15) GO TO 1286                                                    
      IF ( K  .LT.  60 )   GO TO 1387                                           
      N8 = SFREQ(I)                                                     M21. 311
      NDX1 = KXTCS  + N8                                                M22.5020
      YX = XTCS(NDX1)                                                   M22.5021
      GO TO 1250                                                        M21. 313
 1387 YX = CREST(I)                                                     M21. 314
      IF(K.GE.12) GO TO 1280                                                    
      IF(K.LT.11) YX=VOLTBC(K)                                          M31.3363
 1250 IF(N1.GT.0) GO TO 1260                                               13758
      F(N2)=F(N2)+YX                                                       13759
      GO TO 1249                                                           13760
 1260 IF(N3.LT.0)  YX=YX+E(N2)                                             13761
      E(N2)=YX                                                             13762
      GO TO 1249                                                           13763
 1270 YX = CREST(I) * COSZ(SFREQ(I) * TS + TIME1(I))                    M18. 824
      IF( IPRSUP .LT. 3 )  GO TO 1250                                      13765
      WRITE(LUNIT6, 1273)  I, KCONST, N1, N2, N3, NODE(I), IFORM(I),    M17. 908
     1 T, TSTART(I), TSTOP(I), CREST(I), SFREQ(I), TS,                     13767
     2 TIME1(I),YX,F(N2),E(N2)                                             13768
 1273 FORMAT( /, 9H AT 1273   , 7I12, //, ( 1X, 6E20.8 ) )                 13769
      GO TO 1250                                                           13770
 1280 IF(TS .GE. TIME1(I)) GO TO 1285                                      13771
      YX = TS / TIME1(I) * YX                                              13772
      GO TO 1250                                                           13773
 1285 IF(K.EQ.12) GO TO 1250                                                    
      YX = YX + SFREQ(I) * (TS - TIME1(I))                                 13775
      GO TO 1250                                                           13776
 1286 YX = CREST(I) * (EXPZ(SFREQ(I) * TS) - EXPZ(TIME1(I) * TS) )      M18. 825
      GO TO 1250                                                           13778
C                                         ELIMINATION PROCESS ON RIGHT S   13779
C                                         Solve for V = [Y] ** -1 * I   M44.2302
 1300 CALL MOVER ( F(1), E(1), KPARTB )                                 M32.6211
      N1 = 1                                                                    
 1301 E(N1) = 0.0                                                               
      IF ( KODE(N1) .LE. N1 )   GO TO 1302                              M32.6214
      N1 = KODE(N1)                                                     M32.6215
      GO TO 1301                                                        M32.6216
 1302 DO 1303  J = 2, KPARTB                                                    
      IF ( KODE(J) .EQ. 0 ) GO TO 1303                                          
      K = KODE(J)                                                               
      IF ( K .GT. KPARTB )  GO TO 1303                                          
      IF ( K .GT. J ) E(K) = E(K) + E(J)                                        
 1303 CONTINUE                                                          M32.6222
      II=1                                                                      
      IF( IPRSUP .LT. 4 )   GO  TO  1410                                M37.5653
      WRITE(LUNIT6, 41300)  ( E(L), L=1, NTOT )                         M37.5654
41300 FORMAT( /, 39H E(L), L=1, NTOT  AT BEGIN REPEAT SOLN.   ,/,       M32.6225
     1 ( 1X, 5E25.15 ) )                                                   13785
      WRITE(LUNIT6, 51300)  ( L, KM(L), YKM(L), L=1, IUPPER )           M37.5655
51300 FORMAT( /,  41H TABLE OF FACTORS USED.  L, KM(L), YKM(L)  ,/,        13788
     1 ( 2I10, E30.20 , 5X, 2I10, E30.20 ) )                            M37.5656
 1410 IF( II .GT. IUPPER ) GO TO 1450                                           
      L=IABS(KM(II))                                                            
      A = E(L)                                                                  
      E(L) = A*YKM(II)                                                          
      J = KK(L)                                                                 
 1420 II=II+1                                                                   
      IF( II .GT. J ) GO TO 1410                                                
      K=KM(II)                                                                  
      IF ( K .GT. KPARTB )   GO TO  1420                                        
      E(K) = E(K) - A * YKM(II)                                                 
      GO TO 1420                                                           13800
 1450 IF(IPRSUP .GE. 5)                                                 M32.6232
     1 WRITE (LUNIT6, 5203) (E(K), K = 1, NTOT)                         M32.6233
 5203 FORMAT ( 39H AFTER DOWNWARD,  ( E(K), K = 1, NTOT ),              M32.6234
     1 12H  FOLLOW ...  ,/,  ( 1X, 8E16.8 ) )                           M32.6235
C                                               BACKSUBSTITUTION           13823
      L = NTOT                                                                  
      GO TO 5211                                                        M32.6237
 1500 IF( II .EQ. 1 ) GO TO 1550                                                
      A=0.                                                                      
 1510 II=II-1                                                                   
      K=KM(II)                                                                  
      IF(K.LT.0) GO TO 1520                                                     
      A=A-E(K)*YKM(II)                                                          
      GO TO 1510                                                                
 1520 L=IABS(K)                                                                 
      E(L) = E(L) + A                                                           
 5211 IF ( KODE(L) .EQ. 0 ) GO TO 5218                                          
      IF ( KODE(L) .GT. L ) GO TO 5218                                  M32.6241
      J = L                                                             M32.6242
      N5 = KODE ( J )                                                   M32.6243
 5214 E(N5) = E ( J )                                                   M32.6244
      J = N5                                                            M32.6245
      N5 = KODE(J)                                                      M32.6246
      IF ( N5 .NE. L ) GO TO 5214                                       M32.6247
 5218 IF ( L .LE. KPARTB ) GO TO 1500                                   M32.6248
      L = L - 1                                                         M32.6249
      GO TO 5211                                                        M32.6250
 1550 IF( INONL .EQ. NUM99 )  GO TO 1570                                M32.6251
      DO 1559 I=1,INONL                                                    13838
      IF( NLTYPE(I) .LT. 0 )  GO TO 1559                                   13839
      NN15 = NLSUB(I)                                                   M37.5657
      N15 = ISUBEG(NN15)                                                M37.5658
      IF ( N15  .LE. 0 )  GO TO 1559                                    M37.5659
      IF ( NSUBKM(N15+4) .EQ. 0 ) GO TO 3534                            M37.5660
      IF ( NSUBKM(N15+3) .NE. I ) GO TO 1559                            M37.5661
C     PERFORM SIMULTANEOUS  ZNO  SOLUTION IN  "ZINCOX" .                M25. 919
      CALL ZINCOX ( NN15 )                                              M37.5662
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M25. 921
      GO TO 1559                                                        M25. 922
 3534 I1=NONLAD(I)                                                      M25. 923
      I2=NONLE(I)                                                          13841
      K=NONLK(I)                                                           13842
      L=IABS(NONLM(I))                                                     13843
      A2=ANONL(I)                                                          13844
      N1=ILAST(I)                                                          13845
      VZERO2=E(K)-E(L)                                                     13846
      IF ( NLTYPE(I)  .EQ.  94 )   GO TO 1521                           M14.2428
      I2=IABS(I2)                                                          13849
      VZERO2=     VZERO2*DELTA2 +VNONL(I)                                  13850
 1551 VDIFF=VZERO2-VZERO(I)                                                13851
      VZERO(I)=VZERO2                                                      13852
      IF( VDIFF .LT. 0.0 )  GO TO 3400                                     13853
      IF( VDIFF .EQ. 0.0 )  GO TO 1555                                     13854
      VSL = VZERO2 + A2*CCHAR(N1)                                          13855
      H1=VSL-VCHAR(N1)                                                     13856
      GO TO 3800                                                           13857
 3100 N1=N1+1                                                              13858
      IF(N1.LT.I2) GO TO 3200                                              13859
      LSTAT(19) = 3100                                                     13860
 4296 KILL = 35                                                            13861
      FLSTAT(13) = A2                                                      13862
      FLSTAT(11) = E(K)                                                    13863
      FLSTAT(12) = E(L)                                                 M11. 304
 4297 LSTAT(16) = I                                                        13865
      K = NONLK(I)                                                      M15.1313
      BUS1 = BUS(K)                                                        13866
      BUS2 = BUS(L)                                                        13867
      GO TO 9200                                                           13868
 3200 H1=H2                                                                13869
 3800 VSR=VZERO2+A2*CCHAR(N1+1)                                            13870
      H2=VSR-VCHAR(N1+1)                                                   13871
      IF(H1*H2.GT.0.) GO TO 3100                                           13872
 3300 DELTI=ABSZ(H1)*(CCHAR(N1+1)-CCHAR(N1))/(ABSZ(H1)+ABSZ(H2))        M18. 826
      CURR(I)=CCHAR(N1)+DELTI                                              13874
      GO TO 1555                                                           13875
 3500 N1=N1-1                                                              13876
      IF(N1.GE.I1) GO TO 3475                                              13877
      LSTAT(19) = 3500                                                     13878
      GO TO 4296                                                           13879
 3400 VSR=VZERO2+A2*CCHAR(N1+1)                                            13880
      H2=VSR-VCHAR(N1+1)                                                   13881
      GO TO 3450                                                           13882
 3475 H2=H1                                                                13883
 3450 VSL=VZERO2+A2*CCHAR(N1)                                              13884
      H1=VSL-VCHAR(N1)                                                     13885
      IF(H1*H2) 3300,3300,3500                                             13886
 1555 ILAST(I)=N1                                                          13887
      GO TO 8259                                                        M25. 924
 1521 I1=IABS(I1)                                                       M14.2429
      VZERO(I)=VZERO2                                                   M14.2430
      IF ( IPRSUP  .GE.  2 )                                            M14.2431
     1 WRITE (LUNIT6, 1524)  I, NONLE(I), I1, I2, VNONL(I),             M17. 912
     2                  VCHAR(I2+5), VZERO2                             M14.2433
 1524 FORMAT (/,  32H AEP ARRES. AT 1524 OF 'SUBTS3'., 4I10, 3E20.6 )   M32.6252
      IF ( VCHAR(I2+5)  .NE.  0.0 )   GO TO 1561                        M14.2435
      IF(ABSZ(VNONL(I)).GT.ABSZ(VZERO2)) GO TO 8259                     M25. 927
      TZ1=T-DELTAT                                                      M14.2437
      WRITE(LUNIT6,1562) BUS(K),BUS(L),TZ1                              M17. 913
 1562 FORMAT ( 60X, 11HARRESTER  ', A6, 8H'  TO  ', A6,                 M14.2439
     1         20H'  FLASHOVER AT TIME, E13.5, 6H  SEC.   )             M14.2440
 1561 VCHAR(I2) = VZERO2                                                M18. 830
      CALL ARREST ( CCHAR(I1), VCHAR(I2), ANONL(I), VZERO2, CURR(I)  )  M14.2442
      IF (VCHAR(I2+5) .NE. 0.0)   GO TO 8259                            M25. 928
      TZ1=T-DELTAT                                                      M14.2444
      WRITE(LUNIT6,1563) BUS(K),BUS(L),TZ1, VCHAR(I1+8)                 M17. 914
 1563 FORMAT ( 60X, 11HARRESTER  ', A6, 8H'  TO  ', A6,                 M14.2446
     1         20H'   CLEARED  AT TIME, E13.5, 6H  SEC.        ,/,      M14.2447
     2         70X, 28HPER UNIT ENERGY DISSIPATED =, E14.6     )        M14.2448
C     NEXT ASSIGN COMPENSATION CURRENT FOR NON-ZNO  COMPENSATION        M25. 929
C     ELEMENTS.  THIS IS USED IN  "SUBTS4"  SUPERPOSITION.              M25. 930
 8259 N8 = N15 / 5 + 1                                                  M37.5663
      CURSUB(N8) = CURR(I)                                              M25. 933
      F(K) = F(K) - CURR(I)                                             M32.6253
      F(L) = F(L) + CURR(I)                                             M32.6254
      IF ( IPRSUP  .GE.  4 )                                            M25. 934
     1 WRITE (LUNIT6, 8261)  I, NLTYPE(I), NN15, N8, K, L,              M37.5664
     2                       CURR(I), F(K), F(L)                        M32.6256
 8261 FORMAT (  20H N.L. ELEM. CURRENT.,  6I10, 3E15.6  )               M32.6257
 1559 CONTINUE                                                             13916
      IF( IPRSUP .GE. 3 )                                                  13917
     1 WRITE(LUNIT6, 1571)  ( CURR(I), I=1, INONL )                     M17. 915
 1571 FORMAT( /,  35H N.L. ELEMENT,  CURR(I), I=1, INONL   ,   6E15.5 )    13919
 1570 LASTOV = NCHAIN                                                   M15.1314
      NCHAIN = 19                                                       M15.1315
      GO TO 9999                                                           13921
 8000 LASTOV = NCHAIN                                                   M15.1316
      NCHAIN = 20                                                       M15.1317
      GO TO 9999                                                           13923
 9200 LSTAT(18) = 18                                                       13924
      LASTOV = NCHAIN                                                   M15.1318
      NCHAIN = 51                                                          13925
 9999 RETURN                                                               13927
      END                                                                  13929
      SUBROUTINE ZINCOX ( NS )                                          M37.5665
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      DIMENSION FOLD( 1 )                                               M37.5667
      DIMENSION KSING( 1 ), KINDEP( 1 )                                 M37.5668
      EQUIVALENCE ( KSING(1), CCHAR(1) )                                M37.5669
      EQUIVALENCE ( KINDEP(1), GSLOPE(1) )                              M37.5670
      EQUIVALENCE ( FOLD(1), VCHAR(1) )                                 M37.5671
      DIMENSION NSUBKM( 1 )                                             M37.5672
      EQUIVALENCE ( KKNONL(1), NSUBKM(1) )                              M37.5673
      REAL*8          TEXT1, TEXT2, TEXT3, TEXT4                        M37.5674
C     INTRINSIC  ABSZ                                                   M32.6260
      DATA  TEXT1   /  6HSPY     /                                      M37.5675
      DATA  TEXT2   /  6HSOLVE   /                                      M37.5676
      DATA  TEXT3   /  6HSTOP    /                                      M37.5677
      DATA  TEXT4   /  6HLOOK    /                                      M37.5678
      LL10 = 10                                                         M37.5679
      IOFZNR = NONLE( INONL )                                           M37.5680
      IOFZNR = IABSZ( IOFZNR )                                          M37.5681
      D12 = NBYTE(3)                                                            
      D12 = IOFZNR * D12 / NBYTE(4)                                             
      IOFZNI = D12 + 1.0                                                        
      IF ( IPRSUP  .GT.  0 )                                            M37.5685
     1 WRITE (LUNIT6, 918) NS, IOFZNR, IOFZNI, LCHAR                    M37.5686
  918 FORMAT (  19H TOP OF  "ZINCOX" .,  3X,                            M37.5687
     1          28H NS, IOFZNR, IOFZNI, LCHAR =,  4I8 )                 M37.5688
      N6 = ISUBEG( NS )                                                         
 3415 N5 = 1                                                            M25. 946
      N1 = 0                                                            M25. 947
      N8 = N6                                                           M37.5690
      LL1 = 1                                                           M37.5691
C     DETERMINE NUMBER OF ELEMENTS IN THE SUBNETWORK                    M37.5692
      N7 = 0                                                            M37.5693
      DO 10 J = 1, 99999                                                M37.5694
      N7 = N7 + 1                                                       M37.5695
      N10 = NSUBKM( N8 )                                                M37.5696
      IF( N10 .LE. N8 )   GO TO 20                                              
 10   N8 = N10                                                          M37.5698
C     CHECK FOR TABLE OVERFLOW****************************************  M37.5699
 20   N18 = N8                                                                  
      NDX7R = IOFZNR + N7                                               M37.5701
      IF( NDX7R .LE. LCHAR )  GO TO 3422                                M37.5702
      WRITE (LUNIT6, 3416)  N7, IOFZNR                                  M37.5703
 3416 FORMAT ( /,  22H OVERFLOW IN "ZINCOX".,   I5,                     M37.5704
     1 40H COUPLED ARRESTERS, IF ADDED TO IOFZNR =, I5,                 M37.5705
     2 19H OVERFLOW LIST 10. )                                          M37.5706
      KILL = 1                                                          M37.5707
      LSTAT(19) = 3422                                                  M37.5708
      LSTAT(16) = 10                                                    M37.5709
      GO TO 4567                                                        M37.5710
 3422 IF ( N7**2  .LT.  LSIZ26 )   GO TO 3424                           M37.5711
      WRITE (LUNIT6, 3423)  N7                                          M31.3384
 3423 FORMAT ( /,  22H OVERFLOW IN "ZINCOX".,   I5,                     M31.3385
     1  49H COUPLED ARRESTERS, IF SQUARED, OVERFLOW LIST 26. )          M31.3386
      KILL = 1                                                          M31.3387
      LSTAT(19) = 3424                                                  M37.5712
      LSTAT(16) = 26                                                    M35.1636
      GO TO 4567                                                        M33.1709
 3424 IF( N7*2 .LT. LSIZ26 )  GO  TO  3425                              M37.5713
      WRITE( LUNIT6, 3418 ) N7                                          M37.5714
 3418 FORMAT ( /,  22H OVERFLOW IN "ZINCOX".,   I5,                     M37.5715
     1  49H COUPLED ARRESTERS, IF DOUBLED, OVERFLOW LIST 26. )          M37.5716
      KILL = 1                                                          M37.5717
      LSTAT(19) = 3425                                                  M37.5718
      LSTAT(16) = 26                                                    M37.5719
      GO TO 4567                                                        M37.5720
 3425 NDX1R = IOFZNR + 1                                                M37.5721
      NDX1I = IOFZNI + 1                                                M37.5722
      NDX7I = IOFZNI + N7                                               M37.5723
C     EXTRACT ZTHEVENIN FROM ZNONL, DETERMINE ELEMENTS CONNECTED TO     M37.5724
C     KNOWN VOLTAGE NODES********************************************   M37.5725
      NDX2 = NDX1I                                                      M37.5726
      M5 = N6                                                                   
      DO 3431 L = 1, N7                                                 M37.5728
      N12 = 0                                                           M25. 954
      KSING( NDX2 ) = 0                                                 M37.5729
C     PROCESS A COLUMN AT A TIME**********************************      M37.5730
      DO 3426 N = 1, N7                                                 M37.5731
      K = NSUBKM( M5+1 ) + N1                                           M37.5732
      M = NSUBKM( M5+2 ) + N1                                           M37.5733
      VOLTI(N5) = ZNONL(K) - ZNONL(M)                                   M25. 960
      IF ( VOLTI(N5)  .NE.  0.0 )   N12 = 1                             M25. 961
      M5 = NSUBKM( M5 )                                                         
 3426 N5 = N5 + 1                                                       M25. 962
      IF ( N12  .EQ.  0 )   KSING(NDX2) = -1                            M37.5735
      NDX2 = NDX2 + 1                                                   M37.5736
 3431 N1 = N1 + NTOT                                                    M25. 964
      N5 = N5 - 1                                                       M25. 965
      IF ( IPRSUP  .GE.  6 )                                            M25. 966
     1 WRITE(LUNIT6, 3438) N6,NS,N7,N5, (VOLTI(L), L=1,N5)              M37.5737
 3438 FORMAT ( /,  34H THEVENIN MATRIX. N6, NS, N7, N5 =,  4I8          M37.5738
     1  ,/,  ( 1X,  8E16.7 )  )                                         M25. 969
C     EXTRACT AND STORE THEVENIN VOLTAGES****************************   M37.5739
      M = 0                                                             M25. 970
      DO 3452 L = 1, N7                                                 M37.5740
      N10 = NSUBKM( M5+1 )                                                      
      N11 = NSUBKM( M5+2 )                                                      
      INL = NSUBKM( M5+3 )                                                      
      VOLTK( M+1 ) = E( N10 ) - E( N11 )                                        
      VOLTK( M+2 ) = VZERO( INL )                                               
      VECNL2( INL ) = CURR( INL )                                               
      M5 = NSUBKM( M5 )                                                         
 3452 M = M + 2                                                         M37.5748
      IF ( IPRSUP  .GT.  1 )                                            M37.5749
     1 WRITE(LUNIT6, 3613)  ( KSING(L), L=NDX1I, NDX7I )                M37.5750
 3613 FORMAT ( /,  27H 'KSING'  B4 COPY CHECKING.,  20I5  )             M25. 985
 22   N15 = N7 -1                                                       M37.5751
      IF ( N15  .EQ.  0 )   GO TO 3638                                  M25. 987
      N10 = 0                                                           M25. 988
      NDX2 = IOFZNI                                                     M37.5752
      DO 3634  L=1, N15                                                 M25. 989
      NDX2 = NDX2 + 1                                                   M37.5753
      IF ( KSING(NDX2)  .EQ.  -1 )   GO TO 3634                         M37.5754
      N11 = N10 + N7                                                    M25. 991
      NDX3 = NDX2 + 1                                                   M37.5755
      DO 3628 M = NDX3, NDX7I                                           M37.5756
      IF ( KSING(M)  .NE.  0 )   GO TO 3628                             M25. 994
C     CHECK FOR IDENTICAL ROWS (COLUMNS) ***************************    M37.5757
      DO 3621  N12=1, N7                                                M25. 995
      N13 = N10 + N12                                                   M25. 996
      N14 = N11 + N12                                                   M25. 997
      IF ( VOLTI(N13)  .NE.  VOLTI(N14) )   GO TO 3628                  M25. 998
 3621 CONTINUE                                                          M25. 999
      KSING(M) = L                                                              
 3628 N11 = N11 + N7                                                    M25.1001
 3634 N10 = N10 + N7                                                    M25.1002
 3638 IF( IPRSUP .LT. 6 )  GO  TO 40                                    M37.5759
      WRITE(LUNIT6, 3458)                                               M37.5760
 3458 FORMAT( /, 1X, 42H    ROW  KSING         VTHEV          VOLD)     M37.5761
      M2 = -1                                                           M37.5762
      DO 35 L = 1, N7                                                   M37.5763
      M2 = M2 + 2                                                       M37.5764
      L1 = IOFZNI + L                                                   M37.5765
      WRITE( LUNIT6, 32 ) L, KSING(L1), VOLTK(M2), VOLTK(M2+1)          M37.5766
 32   FORMAT( (1X, 2I7, 2E14.5) )                                       M37.5767
 35   CONTINUE                                                          M37.5768
 40   N17 = 0                                                           M37.5769
      DO 3645 L = NDX1I, NDX7I                                          M37.5770
      KINDEP(L) = 0                                                     M25.1012
      IF ( KSING(L)  .NE.  0 )   GO TO 3645                             M25.1013
      N17 = N17 + 1                                                     M25.1014
      KINDEP(L) = N17                                                   M25.1015
 3645 CONTINUE                                                          M25.1016
      IF ( N17  .EQ.  0 )   GO TO 3522                                  M25.1017
      IF ( N17  .EQ.  N7 )   GO TO 3674                                 M25.1018
C     COLLAPSE ZTHEVENIN (REMOVE ZERO AND DEPENDENT ROWS )              M37.5771
      N11 = 1                                                           M25.1019
      N5 = 0                                                            M25.1020
      DO 3662 L = NDX1I, NDX7I                                          M37.5772
      DO 3657 M = NDX1I, NDX7I                                          M37.5773
      IF ( KSING(L)  .NE.  0 )   GO TO 3657                             M25.1023
      IF ( KSING(M)  .NE.  0 )   GO TO 3657                             M25.1024
      N5 = N5 + 1                                                       M25.1025
      VOLT(N5) = VOLTI(N11)                                             M25.1026
 3657 N11 = N11 + 1                                                     M25.1027
 3662 CONTINUE                                                          M25.1028
      CALL MOVER ( VOLT(1), VOLTI(1), N5 )                              M25.1029
      IF ( IPRSUP  .GE.  5 )                                            M25.1030
     1 WRITE (LUNIT6, 3668)  ( VOLT(L), L=1, N5 )                       M25.1031
 3668 FORMAT (     20H COLLAPSED  (ZTHEV):,  7E15.5  )                  M25.1032
 3674 CONTINUE                                                          M37.5774
C     ENTER NEWTON LOOP FOR ZNO ARRESTERS (NOT USER FORTRAN):           M31.3396
      CALL MOVER0 ( VOLT(1), N5 )                                       M31.3397
      NDX17R = IOFZNR + N17                                             M37.5775
      NDX17I = IOFZNI + N17                                             M37.5776
      N10 = 1                                                           M25.1034
      N12 = N17 + 1                                                     M25.1035
      DO 3465  L=1, N17                                                 M25.1036
      VOLT(N10) = 1.0                                                   M25.1037
 3465 N10 = N10 + N12                                                   M25.1038
      IF ( KILL  .GT.  0 )   GO TO 4567                                 M33.1712
C     CALCULATE EXPLICIT INVERSE OF THE COLLAPSED ZTHEV *************** M37.5777
      CALL DGELG ( VOLT(1), VOLTI(1), N17, N17, EPSILN, IER )           M25.1040
      IF ( IPRSUP  .GT.  5 )                                            M37.5778
     1 WRITE (LUNIT6, 3471)  IER,  ( VOLT(M), M=1, N5 )                 M25.1042
 3471 FORMAT ( /,  28H INVERSE OF  (ZTHEV).  IER =,  I2  ,/,            M25.1043
     1       ( 1X, 8E16.7 ) )                                           M25.1044
      NITER = 0                                                         M25.1045
      IF ( IER  .EQ.  0 )   GO TO 3693                                  M25.1046
      LSTAT(19) = 3471                                                  M25.1047
 3681 KILL = 209                                                        M25.1048
 3686 LSTAT(13) = N7                                                    M25.1049
      LSTAT(14) = NSUBKM( N18+3 )                                       M37.5779
      LSTAT(15) = NSUBKM( N6+3 )                                        M37.5780
      LSTAT(16) = N17                                                   M25.1053
      LSTAT(17) = NITER                                                 M25.1054
      N1 = NSUBKM( N6+1 )                                               M37.5781
      N2 = NSUBKM( N6+2 )                                               M37.5782
      BUS1 = BUS(N1)                                                    M25.1057
      BUS2 = BUS(N2)                                                    M25.1058
      FLSTAT(14) = D3                                                   M25.1059
      FLSTAT(15) = T                                                    M27.1493
      IPRSUP = 9                                                        M25.1060
      IF( M4PLOT  .NE.  1 )  GO  TO  3415                                       
      WRITE (MUNIT6, 3688)                                              M37.5784
 3688 FORMAT (  37H   ? ? ?   Singular Jacobian.  Newton,               M37.5785
     1          19H iteration stopped.    )                             M37.5786
      CALL WINDOW                                                               
 3691 WRITE( MUNIT6, 3692 )                                             M37.5788
 3692 FORMAT(  39H Trouble in  "ZINCOX".  KILL  LSTAT(19),              M37.5789
     1         27H   IER  NITER            D3  )                        M37.5790
      CALL WINDOW                                                               
      WRITE (MUNIT6, 3696)  KILL, LSTAT(19), IER, NITER, D3             M37.5792
 3696 FORMAT ( 22X, I6, I11, I6, I7, E14.5 )                            M37.5793
      CALL WINDOW                                                               
      DO 3702  IL = 1, N7                                               M37.5795
      INL = NSUBKM( M5+3 )                                                      
      CURR( INL ) = VECNL2( INL )                                               
      M5 = NSUBKM( M5 )                                                         
 3702 CONTINUE                                                          M37.5799
      CALL HONKER( LL10 )                                                       
 3711 WRITE (PROM80, 3712)                                              M38.3481
 3712 FORMAT ( ' SEND REMEDY (SPY, SOLVE, LOOK, STOP) :' )              M37.5802
      CALL PROMPT                                                               
      LOCKBR = 1                                                                
      CALL FLAGER                                                               
      READ (BUFF77, 3719)  BUS1                                                 
 3719 FORMAT ( A6 )                                                     M38.3486
      KILL = 0                                                                  
      IF ( BUS1 .NE. TEXT1 )  GO TO 3726                                        
      CALL SPYING                                                               
      GO TO 3711                                                                
 3726 IF ( BUS1 .EQ. TEXT2 )  GO TO 3415                                        
      IF ( BUS1 .EQ. TEXT3 )                                                    
     1 CALL STOPTP                                                              
      IF ( BUS1 .NE. TEXT4 )  GO TO 3752                                        
      WRITE (PROM80, 3731)                                              M38.3487
 3731 FORMAT ( ' SEND DIAGNOSTIC LEVEL IPRSUP (I2 FORMAT) :' )          M37.5815
      CALL PROMPT                                                               
      READ (BUFF77, 3734)  IPRSUP                                       M38.3489
 3734 FORMAT ( I2 )                                                     M37.5818
      GO TO 3711                                                                
 3752 GO TO 3711                                                                
 3693 D3 = FLTINF                                                       M25.1062
      CALL MOVER ( VOLT(1), VOLTI(1), N5 )                              M25.1063
 3474 NITER = NITER + 1                                                 M25.1064
      N11 = 1                                                           M25.1065
      CALL MOVER0 ( FOLD(NDX1R), N17 )                                  M37.5821
      NDX2 = IOFZNI                                                     M37.5822
      M2 = 0                                                            M37.5823
      DO 3482  L=1, N7                                                  M25.1067
      NDX2 = NDX2 + 1                                                   M37.5824
      M2 = M2 + 2                                                       M37.5825
      IF ( KSING(NDX2)  .EQ.  -1 )   GO TO 3482                         M37.5826
      N13 = L                                                           M25.1069
      IF( KSING(NDX2)  .GT.  0 )   N13 = KSING(NDX2)                    M37.5827
      NDX3 = N13 + IOFZNI                                               M37.5828
      N14 = KINDEP(NDX3)                                                M37.5829
      N10 = ( N14 - 1 ) * N17  +  N13                                   M25.1072
      INL = NSUBKM( M5+3 )                                                      
      ITYP = NSUBKM( M5+4 )                                                     
      ILS = ILAST( INL )                                                        
      IF( ITYP .EQ. 1 )  GO  TO  41                                     M37.5833
C     PROCESS PIECEWISE LINEAR AND TIME VARYING RESISTANCES************ M37.5834
      IF( ILS .GT. 0 )  GO TO  254                                              
      D4 = VOLTK( M2 )                                                  M37.5836
      IF( ITYP  .EQ.  3 )   D4 = T + VNONL( INL )                               
      D5 = D4                                                           M37.5838
      IF( ITYP .EQ. 2  .AND.  VCHAR( ICHR ) .EQ. 0.0 )   D5 = ABSZ(D5)  M37.5839
      ICHR = NONLAD( INL )                                                      
      ILS = -ILS                                                        M37.5841
      IF( D5 .GT. VCHAR( ILS ) )  GO TO  251                                    
      IBK = ILS                                                         M37.5843
      DO 250 JB = ICHR, ILS                                             M37.5844
      IF( D5 .LT. VCHAR( IBK ) ) GO TO 250                              M37.5845
      IK = IBK                                                          M37.5846
      GO TO 253                                                         M37.5847
 250  IBK = IBK - 1                                                     M37.5848
      IK = ICHR                                                         M37.5849
      GO TO 253                                                         M37.5850
 251  IBK = NONLE( INL )                                                M37.5851
      DO 252 JB = ILS, IBK                                              M37.5852
      IF( D5 .GT. VCHAR( JB ) )  GO TO 252                              M37.5853
      IK = JB - 1                                                       M37.5854
      GO TO 253                                                         M37.5855
 252  CONTINUE                                                          M37.5856
      IK = IBK                                                          M37.5857
C     SEGMENT HAS BEEN DETERMINED************************************** M37.5858
 253  ILAST( INL ) = -IK                                                M37.5859
      AZM = GSLOPE( IK )                                                M37.5860
      D11 = AZM * D5 + CCHAR( IK )                                              
      IF( ( D5 * D4 ) .LT. 0.0 )    D11 = -D11                                  
      IF( ITYP  .EQ.  2 )  GO  TO  261                                          
      IF( D11  .EQ.  0.0 )  D11 = EPSILN                                        
      AZM = 1.0 / D11                                                           
      D11 = AZM * VOLTK( M2 )                                                   
 261  CURR( INL ) = D11                                                 M37.5867
      VOLT( N10 ) = VOLT( N10 ) - AZM                                           
      GO TO 68                                                          M37.5869
 254  D11 = 0.0                                                         M37.5870
      CURR( INL ) = 0.0                                                 M37.5871
      GO TO 68                                                          M37.5872
C     PROCESS ZNO ARRESTERS******************************************** M37.5873
 41   D5 = ABSZ( VOLTK( M2 ) / ANONL( INL ) )                           M37.5874
      IF( ILS .LT. 0 )  GO TO 45                                                
C     SET LIMITS FOR SCANNING OF SEGMENT BOUNDARIES                     M37.5876
      IST = NONLAD( INL )                                                       
      IND =  ILS                                                                
      GO TO 55                                                          M37.5879
 45   IST = - ILS + 1                                                           
      IND = NONLE( INL )                                                        
C     START SCAN OF SEGMENT BOUNDARIES********************************* M37.5882
 55   IST = IST + 1                                                     M37.5883
      DO 60 JB = IST, IND                                                       
      IF( VCHAR( JB )  .LT.  D5 )  GO  TO  60                           M37.5885
      IK = JB - 1                                                       M37.5886
      GO TO 65                                                          M37.5887
 60   CONTINUE                                                          M37.5888
      IK = IND                                                          M37.5889
C     SEGMENT HAS BEEN DETERMINED************************************** M37.5890
 65   AZM = CCHAR( IK )                                                         
      IF( IK  .GE.  IST )   GO  TO  66                                  M37.5892
      D11 = AZM * VOLTK( M2 )                                                   
      CURR( INL ) = D11                                                 M37.5894
      VOLT( N10 ) = VOLT( N10 ) - AZM                                   M37.5895
      GO TO 68                                                          M37.5896
 66   AZX = GSLOPE( IK )                                                        
      D11 = AZM * ( D5**AZX )                                                   
      IF( VOLTK( M2 ) .LT. 0.0 )  D11 = -D11                            M37.5899
      CURR( INL ) = D11                                                         
      VOLT( N10 ) = VOLT( N10 ) - AZX * D11 / VOLTK( M2 )               M37.5901
 68   NDX4 = IOFZNR + N14                                               M37.5902
      IF( KSING(NDX2) .NE. 0 ) GO TO 75                                 M37.5903
      M22 = 0                                                           M37.5904
      DO 70 M = NDX1I, NDX7I                                            M37.5905
      IF( KSING( M ) .NE. 0 ) GO TO 70                                  M37.5906
      FOLD(NDX4) = FOLD(NDX4) - VOLTI(N11) *                            M37.5907
     1 ( VOLTK( M22+2 ) - VOLTK( M22+1) )                                       
      N11 = N11 + 1                                                     M37.5909
 70   M22 = M22 + 2                                                     M37.5910
 75   FOLD( NDX4 ) = FOLD( NDX4 ) + D11                                 M37.5911
 3482 M5 = NSUBKM( M5 )                                                         
      IF( D3 .LE. EPSZNO )  GO TO  3522                                         
      IF ( IPRSUP  .LT.  7 )  GO TO 3499                                M37.5914
      WRITE (LUNIT6, 3486)  ( FOLD(L), L=NDX1R,NDX17R )                 M37.5915
 3486 FORMAT ( /,  21H RHS VECTOR  'FOLD' .,   7E15.6 )                 M25.1091
      WRITE (LUNIT6, 3494)  ( VOLT(L), L=1, N5 )                        M37.5916
 3494 FORMAT (     19H JACOBIAN  'VOLT' .,   7E15.6 )                   M25.1095
 3499 CALL DGELG ( FOLD(NDX1R), VOLT(1), N17, LL1, EPSILN, IER )        M37.5917
      IF ( IER  .EQ.  0 )   GO TO 3501                                  M25.1098
      LSTAT(19) = 3501                                                  M25.1099
      GO TO 3681                                                        M25.1100
 3501 IF ( IPRSUP  .GT.  4 )                                            M37.5918
     1 WRITE (LUNIT6, 3502)  ( FOLD(L), L=NDX1R,NDX17R )                M37.5919
 3502 FORMAT (     16H DV CORRECTION :,  7E15.6 )                       M25.1103
      D3 = 0.0                                                          M25.1104
C     CHECK FOR TOO LARGE VOLTAGE CORRECTIONS*************************  M37.5920
      DO 80 JB = NDX1I, NDX7I                                           M37.5921
      IF ( KSING(JB)  .NE.  0 )   GO TO 80                              M37.5922
      N13 = KINDEP( JB ) + IOFZNR                                       M37.5923
      IL1 = NSUBKM( M5+3 )                                              M37.5924
      D4 = ABSZ( FOLD( N13 ) / ANONL( IL1 ) )                           M37.5925
      IF( D4 .GT. D3 )  D3 = D4                                         M37.5926
 80   M5 = NSUBKM( M5 )                                                         
C     SCALE CORRECTIONS, IF TOO LARGE********************************** M37.5928
      IF( D3 .LE. ZNOLIM(1) )  GO TO 90                                         
      AMULT = ZNOLIM(1) / D3                                            M37.5930
      DO 85 JB = NDX1R, NDX17R                                          M37.5931
      FOLD( JB ) = FOLD( JB ) * AMULT                                   M37.5932
 85   CONTINUE                                                          M37.5933
 90   IF( NITER .LT. 4 )  GO TO 92                                      M37.5934
C     CHECK FOR OSCILLATORY SOLUTION*********************************** M37.5935
      D35 = ABSZ( D3 - D35 ) / D35                                      M37.5936
      IF( D35 .GT. ( 10.0*EPSILN ) )    GO TO 92                        M37.5937
      AMULT = 0.1                                                       M37.5938
      DO 91 JB = NDX1R, NDX17R                                          M37.5939
      FOLD( JB ) = FOLD( JB ) * AMULT                                   M37.5940
 91   CONTINUE                                                          M37.5941
C     UPDATE VOLTAGES( ADD SCALED CORRECTIONS)************************* M37.5942
 92   M2 = 0                                                            M37.5943
      D4 = ZNOLIM( 2 )                                                          
      DO 100 L = NDX1I, NDX7I                                           M37.5945
      M2 = M2 + 2                                                       M37.5946
      INL = NSUBKM( M5+3 )                                                      
      IF ( KSING(L)  .EQ.  -1 )   GO TO 100                             M37.5948
      IF ( KSING(L)  .EQ.  0 )   GO TO 95                               M37.5949
      N12 = KSING(L) * 2                                                M37.5950
      VOLTK( M2 ) = VOLTK( N12 )                                        M37.5951
      GO TO 100                                                         M37.5952
 95   N13 = KINDEP( L ) + IOFZNR                                        M37.5953
      VOLTK( M2 ) = VOLTK( M2 ) + FOLD( N13 )                           M37.5954
      IF( NSUBKM( M5+4 )  .GT.  1 )  GO TO 100                                  
      D6 = VOLTK( M2 )                                                  M37.5956
      D5 = D4 * ANONL( INL )                                                    
      IF( D5 .GT. ABSZ( D6 ) )  GO TO 100                                       
      VOLTK( M2 ) = D5                                                          
      IF( D6 .LT. 0.0 )  VOLTK( M2 ) = -D5                              M37.5960
 100  M5 = NSUBKM( M5 )                                                         
      N4 = NSUBKM( N6+3 )                                               M37.5962
      IF ( IPRSUP  .GE.  3 )                                            M25.1116
     1 WRITE (LUNIT6, 3511)  N4, NITER, D3                              M25.1117
 3511 FORMAT (24H SUBSYSTEMS 1ST ARRESTER, I3, 5X, 7HNITER =, I3,       M37.5963
     1  5X,  11HMAX DEL-V =,  E13.4  )                                  M25.1119
      CALL MOVER ( VOLTI(1), VOLT(1), N5 )                              M25.1120
      D35 = D3                                                          M37.5964
      IF ( NITER  .LE.  MAXZNO )   GO TO 3474                           M25.1121
      IF ( D3  .LE.  EPWARN )   GO TO 3522                              M37.5965
      WRITE (LUNIT6, 3517)  NS, D3                                      M37.5966
 3517 FORMAT (   12H SUBSYSTEM =,  I3,                                  M37.5967
     1 47H  ITERATION LIMIT.   LARGEST ABS(DV / ZNVREF) =, E12.3 )      M28.5570
      IF ( D3  .LE.  EPSTOP )   GO TO 3522                              M37.5968
      KILL = 212                                                        M25.1128
      LSTAT(19) = 3522                                                  M37.5969
      IF( M4PLOT .NE. 1 )  GO TO 3686                                           
      WRITE (MUNIT6, 3519)                                              M37.5971
 3519 FORMAT (  42H   ? ? ?   Non-converged Newton iteration.,          M37.5972
     1          18H   Limit  MAXZNO =,  I5,  10H  reached.  )           M37.5973
      CALL WINDOW                                                               
      GO TO 3691                                                                
 3522 NDX2 = IOFZNI                                                     M37.5976
      M2 = 0                                                            M37.5977
      DO 200 L = 1, N7                                                  M37.5978
      NDX2 = NDX2 + 1                                                   M37.5979
      M2 = M2 + 2                                                       M37.5980
      INL = NSUBKM( M5+3 )                                              M37.5981
      ITYP = NSUBKM( M5+4 )                                                     
      ILS = ILAST( INL )                                                        
      ZNVREF = 1.0 / ANONL( INL )                                       M37.5984
      IF ( KSING(NDX2)  .NE.  -1 )   GO TO 180                          M37.5985
      D4 = VOLTK( M2-1 )                                                        
      IF( ITYP .EQ. 1 )  GO TO 139                                      M37.5987
C     PROCESS PIECEWISE LINEAR AND TIME VARYING  RESISTANCES**********  M37.5988
      D11 = 0.0                                                         M37.5989
      IF( ILS .GT. 0 )  GO TO 168                                               
      ICHR = NONLAD( INL )                                              M37.5991
      ILS = -ILS                                                        M37.5992
      IF( ITYP  .EQ.  3 )   D4 = T + VNONL( INL )                               
      D3 = D4                                                           M37.5994
      IF( ITYP .EQ. 2  .AND.  VCHAR( ICHR ) .EQ. 0.0 )  D3 = ABSZ(D3)   M37.5995
      IF( D3 .GT. VCHAR( ILS ) )  GO TO 351                                     
      IBK = ILS                                                         M37.5997
      DO 350 JB = ICHR, ILS                                             M37.5998
      IF( D3 .LT. VCHAR( IBK ) )  GO TO 350                             M37.5999
      IK = IBK                                                          M37.6000
      GO TO 353                                                         M37.6001
 350  IBK = IBK - 1                                                     M37.6002
      IK = IBK                                                          M37.6003
      GO TO 353                                                         M37.6004
 351  IBK = NONLE( INL )                                                M37.6005
      DO 352 JB = ILS, IBK                                              M37.6006
      IF( D3 .GT. VCHAR( JB ) )  GO TO 352                              M37.6007
      IK = JB - 1                                                       M37.6008
      GO TO 353                                                         M37.6009
 352  CONTINUE                                                          M37.6010
      IK = IBK                                                          M37.6011
C     SEGMENT HAS BEEN DETERMINED*************************************  M37.6012
 353  ILAST( INL ) = -IK                                                M37.6013
      ILS = -IK                                                         M37.6014
      AZM = GSLOPE( IK )                                                M37.6015
      D11 = AZM * D3 + CCHAR( IK )                                      M37.6016
      IF( ( D3 * D4 ) .LT. 0.0 )  D11 = -D11                                    
      IF( ITYP  .EQ.  2 )  GO TO 168                                            
      D4 = VOLTK( M2-1 )                                                        
      IF( D11  .EQ.  0.0 )  D11 = EPSILN                                M37.6020
      D11 = ( 1.0 / D11 ) * D4                                                  
      GO TO 168                                                         M37.6022
C     PROCESS ZNO ARRESTERS******************************************** M37.6023
 139  D5 = ABSZ( D4 * ZNVREF )                                          M37.6024
      IF( ILS .LT. 0 )  GO TO 145                                               
C     SET LIMITS FOR SCANNING OF SEGMENT BOUNDARIES                     M37.6026
      IST = NONLAD( INL )                                                       
      IND =  ILS                                                                
      GO TO 155                                                         M37.6029
 145  IST = -ILS + 1                                                            
 150  IND = NONLE( INL )                                                        
C     START SCAN OF SEGMENT BOUNDARIES********************************* M37.6032
 155  IST = IST + 1                                                     M37.6033
      DO 160 JB = IST, IND                                              M37.6034
      IF( VCHAR( JB )  .LT.  D5 )  GO  TO  160                          M37.6035
      IK = JB - 1                                                       M37.6036
      GO TO 165                                                         M37.6037
 160  CONTINUE                                                          M37.6038
      IK = IND                                                          M37.6039
C     SEGMENT HAS BEEN DETERMINED************************************** M37.6040
 165  AZM = CCHAR( IK )                                                         
      IF( IK  .GE.  IST )  GO  TO  166                                  M37.6042
      D11 = AZM * D4                                                            
      GO TO 168                                                         M37.6044
 166  AZX = GSLOPE( IK )                                                        
      D11 = AZM * ( D5**AZX )                                                   
      IF( D4 .LT. 0.0 )  D11 = -D11                                     M37.6047
 168  VOLTK( M2 ) = D4                                                          
      CURR( INL ) = D11                                                         
C     DETERMINE GAP STATUS FOR NEXT TIME STEP************************** M37.6050
 180  D6 = VNONL( INL )                                                 M37.6051
      IF( D6 .EQ. FLTINF )  GO  TO  196                                         
      IF( ILS .LT. 0 )  GO TO 190                                               
      D5 = ABSZ( VOLTK( M2 ) )                                          M37.6054
      IF( D5 .GT. D6 )  ILS = -ILS                                              
      IF( ITYP .EQ. 3  .AND. ILS .LT. 0 )                               M37.6056
     1 VNONL( INL ) = -( T + DELTAT )                                           
      GO  TO  195                                                       M37.6058
 190  IF( ITYP  .EQ.  3 )  GO TO 195                                            
      IF( CURR(INL)*VECNL2(INL) .LT. 0.0 )   ILS = -ILS                         
      IF( ITYP .EQ. 1  .OR.  ILS .LT. 0 )  GO  TO  195                  M37.6061
      GAP = VECNL1( INL )                                               M37.6062
      IF( GAP .EQ. 0.0 )  GO TO  195                                    M37.6063
      IF( GAP  .LT.  0.0 )  ILS = -ILS                                          
      IF( GAP  .GT.  0.0 )  VNONL( INL ) = FLTINF                               
 195  ILAST( INL ) = ILS                                                M37.6066
 196  VZERO( INL ) = VOLTK( M2 )                                                
      N13 = M5 / 5                                                      M37.6068
      CURSUB( N13+1 ) = CURR( INL )                                             
      K1 = NSUBKM( M5 + 1 )                                             M37.6070
      K2 = NSUBKM( M5 + 2 )                                             M37.6071
      F(K1) = F(K1) - CURR( INL )                                       M37.6072
      F(K2) = F(K2) + CURR( INL )                                       M37.6073
 200  M5 = NSUBKM( M5 )                                                         
 4567 IF ( IPRSUP  .GE.  1 )                                            M33.1895
     1 WRITE ( LUNIT6, 4568 )  KILL, LSTAT(19)                          M33.1896
 4568 FORMAT ( 41H EXIT MODULE "ZINCOX".  KILL, LSTAT(19) =, 2I8 )      M33.1897
 9999 RETURN                                                            M31.3568
      END                                                               M25.1147
      SUBROUTINE  ANALYT                                                M12. 530
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THIS MODULE IS CALLED BY SUBROUTINE  'SUBTS3'  OF OVERLAY 16 IF   M30. 956
C)    DATA CASE BEING SOLVED USES ONE OR MORE SOURCES OF TYPE 1 THROUGH M15.1321
C)    10, AND ALSO HAS A SPECIAL   'ANALYTIC SOURCES'   REQUEST CARD    M15.1322
C)    WHICH PRECEDES THE MISCELLANEOUS DATA CARDS.   IF THIS BE THE     M15.1323
C)    CASE, THE USER MUST REPLACE THIS DUMMY MODULE WITH ONE WHICH PUTS M15.1324
C)    ONE OR MORE SOURCE VALUES IN VECTOR  'VOLTBC'  AT EACH TIME STEP. M31.3589
C     RELEVANT VARIABLES TO USE IN THIS DEFINITION PROCESS ARE .....    M12. 538
C          T     ----- TIME IN SECONDS FOR WHICH SOURCE VALUES MUST BE  M12. 539
C                      ASSIGNED.                                        M12. 540
C          ISTEP ----- THE TIME-STEP NUMBER.                            M12. 541
C          DELTAT ---- THE TIME-STEP SIZE.   A CONSTRAINT AMONG THESE   M12. 542
C                      THREE VARIABLES IS    T = ISTEP*DELTAT  .        M12. 543
C          VOLTBC  --- VECTOR OF SOURCE VALUES.   FOR THE SOURCE HAVING M31.3590
C                      TYPE-CODE  'L' ,   VOLTBC(L)  MUST BE ASSIGNED   M31.3591
C                      TO EQUAL THE DESIRED SOURCE VALUE AT TIME  'T' . M12. 546
C     SINCE THE PRESENT DUMMY MODULE HAD BETTER NEVER BE CALLED, THE    M12. 547
C     FOLLOWING STATEMENTS WILL KILL THE RUN WITH AN APPROPRIATE ERROR  M12. 548
C     MESSAGE.   THE ONE EXCEPTION IS FOR UTPF TEST CASE NUMBER 14,     M14.2604
C     WHICH HAS   NTOT = 5  ,   IBR = 5  ,    AND   DELTAT = 0.1   .    M14.2605
C       PRECEDING COMMENTS APPLIED BEFORE THE SPRING OF 1981,           M30. 957
C       WHEN INTERACTIVE CONTROL WAS DEVISED.  RAMPING OF COMMON        M35.5878
C       VARIABLES IS DONE HERE (REMOTE CONTROL FROM "EMTSPY").          M35.5879
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
C     INTRINSIC  ABSZ                                                   M32.6463
      DATA  N16  / 0 /                                                  M35.5883
 1637 IF ( IPRSUP  .GE.  1 )                                            M35.5884
     1 WRITE (LUNIT6, 1641)  NUMRMP, NTOT, TMINRP, TMAXRP               M35.5885
 1641 FORMAT ( ' TOP OF "ANALYT".  NUMRMP, NTOT, TMINRP, TMAXRP =',     M35.5886
     1         2I8,  2E15.6 )                                           M35.5887
      IF ( NUMRMP  .LE.  0 )   GO TO 1847                                       
      IF ( T .LT. TMINRP )  GO TO 1830                                          
      IF ( T .GT. TMAXRP + DELTAT )  GO TO 1830                                 
      DO 1762  J=1, NUMRMP                                                      
      IF ( T .LT. TBEGRP(J) )  GO TO 1762                                       
      IF ( T .GT. TENDRP(J) + DELTAT )  GO TO 1762                              
      IF ( KYRAMP(J) .NE. 1 )  GO TO 1656                                       
      KSERLC = 1                                                                
      N17 = INDXRP(J)                                                           
      NR(N17) =1                                                                
      IF ( IPRSUP .LT. 2 )  GO TO 1675                                          
      WRITE (MUNIT6, 1674)  J, N17                                      M37.7325
 1674 FORMAT ( ' "ANALYT", R-L-C RAMP.  J, N17 =', 2I5 )                M36.3409
      CALL WINDOW                                                               
 1675 D6 = RAMPCN(J) + RAMPSL(J) * T                                    M37.7327
      IF( T .LT. TENDRP(J)-DELTAT ) GO TO 1758                          M36.3411
      D6 = FENDRP(J)                                                    M36.3412
      GO TO 1758                                                        M36.3413
 1656 D6 = RAMPCN(J) + RAMPSL(J) * T                                            
      IF ( T .GT. TENDRP(J) )  D6 = FENDRP(J)                                   
 1758 N4 = MEMRMP(J)                                                            
      IF ( N10RMP(J) .NE. 0 )  GO TO 1748                                       
      FKAR1(N4) = D6                                                            
      GO TO 1762                                                                
 1748 FKAR2(N4) = D6                                                            
      IF ( IPRSPY  .GE.  1 )                                            M35.5897
     1 WRITE (LUNIT6, 1769) J, N4, T, D6                                        
 1769 FORMAT ( ' DONE NEXT RAMP IN "ANALYT".  J, N4, T, D6 =',          M30. 990
     1  2I8,  2E15.6  )                                                 M30. 991
 1762 CONTINUE                                                                  
 1830 IF ( IPRSPY  .GE.  1 )                                            M30. 992
     1 WRITE (LUNIT6, 1833)  T, TMINRP, TMAXRP                          M35.5900
 1833 FORMAT ( ' EXIT "ANALYT".  T, TMINRP, TMAXRP =',                  M35.5901
     1   3E15.6  )                                                      M30. 995
      GO TO 9000                                                        M35.5902
 1847 IF ( KANAL  .NE.  2 )  GO TO 1942                                         
      IF ( N16  .EQ.  5678 ) GO TO 9000                                 M35.5904
      WRITE ( MUNIT6, 1854 )                                            M35.5905
 1854 FORMAT ( '   ###  WARNING.   [Y] IS BEING RE-FORMED',             M35.5906
     1         ' EACH DELTAT, BUT THERE IS NO RAMPING.'   )             M35.5907
      CALL WINDOW                                                               
      WRITE (PROM80, 1855)                                              M35.5908
 1855 FORMAT (  '                   IS IT OK TO CONTINUE',              M35.5909
     1         ' (ONE AND ONLY CHANCE IF "Y")? :'  )                    M35.5910
      CALL PROMPT                                                               
      READ ( MUNIT5, 1863 )  BUFF77(1:1)                                M35.5912
 1863 FORMAT ( A1 )                                                     M35.5913
      IF ( BUFF77(1:1)  .NE.  'Y' )  GO TO 1868                         M35.5914
      N16 = 5678                                                        M35.5915
      GO TO 9000                                                        M35.5916
 1868 LOCKBR = 1                                                                
      CALL SPYING                                                       M35.5918
      GO TO 1637                                                        M35.5919
 1942 IF (KANAL .EQ. 0) GO TO 9000                                              
      IF ( NTOT  .NE. 5 )   GO TO 8613                                  M35.5921
      IF ( IBR  .NE.  5 )   GO TO 8613                                  M14.2606
      D1 = (DELTAT - 1.0/10. ) * 100.                                   M14.2607
      IF ( ABSZ(D1)  .GT.  TENM3 )   GO TO 8613                         M18. 837
      IF ( VOLTBC(2)  .NE.  0.0 )   GO TO 2416                          M31.3592
      D1 = T - 0.25                                                     M12. 557
      VOLTBC(2)  =  100. - 100.*D1                                      M31.3593
      IF ( T  .GT.  0.75 )   VOLTBC(2) = 0.0                            M31.3594
 2416 RETURN                                                            M12. 560
 8613 KILL = 101                                                        M12. 561
      LSTAT(19) = 8613                                                  M12. 562
 9000 IF ( IPRSUP  .GE.  1 )                                            M35.5922
     1 WRITE (LUNIT6, 9013)  NTOT, IBR, NUMRMP, KILL                    M35.5923
 9013 FORMAT ( ' EXIT "ANALYT".  NTOT, IBR, NUMRMP, KILL =',            M35.5924
     1         4I8  )                                                   M35.5925
      RETURN                                                            M12. 563
      END                                                               M12. 564
      SUBROUTINE  ARREST ( A,B,SRT,SVT,CARST)                           M14.2449
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C   NUMBER   VARIABLE                DESCRIPTION                        M14.2450
C    A(1)       K        VALVE BLOCK RESISTANCE CONSTANT                M14.2451
C    A(2)       B        VALVE BLOCK CURRENT EXPONENT                   M14.2452
C    A(3)       L        VALVE VLOCK INDUCTANCE                         M14.2453
C    A(4)                                                               M14.2454
C                                                                       M14.2455
C    A(5)       KC       GAP CAPACITANCE CONSTANT           T0-T1       M14.2456
C    A(6)       IC       GAP CURRENT LIMIT FOR CAPACITANCE  T0-T1       M14.2457
C    A(7)       E1       GAP TRANSITION VOLTAGE  (1 TO 2)   T0-T1       M14.2458
C    A(8)       KR       GAP                                T1-T4       M14.2459
C    A(9)       K0       GAP                                T1-T4       M14.2460
C    A(10)      K1       GAP                                T1-T4       M14.2461
C    A(11)      K2       GAP                                T1-T3       M14.2462
C    A(12)      W2       GAP TRANSITION ENERGY  (2 TO 3)    T1-T2       M14.2463
C    A(13)      KD       GAP                                T2-T3       M14.2464
C    A(14)      W0       GAP                                T2-T3       M14.2465
C    A(15)      W1       GAP                                T2-T3       M14.2466
C    A(16)      K0P      GAP                                T3-T4       M14.2467
C    A(17)      K3       GAP                                T3-T4       M14.2468
C    A(18)      I3       GAP TRANSITION CURRENT  (3 TO 4)   T0-T4       M14.2469
C                                                                       M14.2470
C    B(1)       VA       ARRESTER VOLTAGE                               M14.2471
C    B(2)       VG       GAP VOLTAGE                                    M14.2472
C    B(3)       IA       ARRESTER CURRENT                               M14.2473
C    B(4)       IA       ARRESTER CURRENT (T-DT)                        M14.2474
C    B(5)       WGAP     ENERGY IN GAP FROM T1                          M14.2475
C    B(6)       XK       REGION CODE                                    M14.2476
C    B(7)       M        1/C INTEGRAL OF REGION 1                       M14.2477
C    B(8)       VB       BLOCK VOLTAGE                                  M14.2478
C    B(9)       WT       ACCUMULATED ENERGY SINCE FLASHOVER             M14.2479
C    B(10)      FV       VOLTAGE DIVISION FACTOR                        M14.2480
C    B(11)      FI       CURRENT DIVISION FACTOR                        M14.2481
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION  A(1), B(1)                                             M14.2483
      LL8 = 8                                                           M22.5022
      IF (IPRSUP .GE. 2)                                                M18. 832
     1 WRITE (LUNIT6, 20)  (A(I), B(I), I=1, 20)                        M18. 833
   20 FORMAT( (1X, 8E16.6) )                                            M18. 834
      IF(B(6) .GT.0) GO TO 100                                          M14.2484
      CALL MOVER0 ( B(2), LL8 )                                         M22.5023
      B(6)=1.0                                                          M14.2486
  100 CONTINUE                                                          M14.2487
C  TEST POLARITY OF ARRESTER VOLTAGE                                    M14.2488
      ISIGN=1                                                           M14.2490
      B(1)=B(1)/B(10)                                                   M14.2491
      B(3)=B(3)/B(11)                                                   M14.2492
      IF(B(1) .GT.0.0) GO TO 1                                          M14.2493
      B(1) = -B(1)                                                      M32.6282
      IF ( B(3) .LT. 0.0 )  B(3) = -B(3)                                M32.6283
      ISIGN=-1                                                          M14.2496
    1 CONTINUE                                                          M14.2497
C  CONSTRUCT VALVE BLOCK BRANCHES                                       M14.2498
      YLB = DELTA2 / A(3)                                               M19.1621
      CB=B(3) +YLB*B(8)                                                 M14.2500
      RB=0.0                                                            M14.2501
      IF(B(3) .GT. EPSILN) RB=A(2)*A(1)*B(3) **(A(2)-1.0)               M14.2502
      BE=(1.0-A(2))*A(1)*B(3) **A(2)                                    M14.2503
C  TEST GAP REGION TRANSITION                                           M14.2504
      D1=B(6)                                                           M14.2505
      IF(D1.GE.3.5) GO TO 50                                            M14.2506
   47 IF(B(3) -B(4) .GE.0.0) GO TO 44                                   M14.2507
      IF(B(3) .GT.A(18)) GO TO 44                                       M14.2508
      D1=4.0                                                            M14.2509
      GO TO 50                                                          M14.2510
   44 IF(D1.GT.1.5) GO TO 45                                            M14.2511
      IF(B(2) .LT.A(7)) GO TO 50                                        M14.2512
      D1=2.0                                                            M14.2513
      GO TO 46                                                          M14.2514
   45 IF(D1.GT.2.5) GO TO 50                                            M14.2515
   46 IF(B(5) .LT.A(12)) GO TO 50                                       M14.2516
      D1=3.0                                                            M14.2517
   50 B(6)=D1                                                           M14.2518
      IF ( IPRSUP  .GE.  2 )                                            M14.2519
     1 WRITE (LUNIT6, 5641)  ISIGN, D1, SRT, SVT, CARST,                M17. 916
     2                  ( A(I), B(I), I=1, 20 )                         M14.2521
 5641 FORMAT ( /,  23H IN  'ARREST' .   ISIGN, 13X, 2HD1, 10X,          M14.2522
     1         5HANONL, 9X, 6HVZERO2, 11X, 4HCURR,  /,                  M14.2523
     2         15X, I8, 4E15.6, /,  (1X, 8E16.6)  )                     M14.2524
C  SELECT REGION                                                        M14.2525
      IF(D1.LT.1.5) GO TO 200                                           M14.2526
      IF(D1.LT.2.5) GO TO 220                                           M14.2527
      IF(D1.LT.3.5) GO TO 240                                           M14.2528
      GO TO 260                                                         M14.2529
C  REGION (1) --- T0 TO T1                                              M14.2530
  200 CONTINUE                                                          M14.2531
      F0=B(7)*B(3)                                                      M14.2532
      DFDV=0.0                                                          M14.2533
      DFDI=B(7)                                                         M14.2534
      GO TO 99                                                          M14.2535
C  REGION (2) BRANCHES --- T1 TO T2                                     M14.2536
  220 CONTINUE                                                          M14.2537
      F1=A(8)*(A(9)-B(2))                                               M14.2538
      F2=A(10)+A(11)*B(3)                                               M14.2539
      F0=F1/F2                                                          M14.2540
      DFDV=-A(8)/F2                                                     M14.2541
      DFDI=-A(11)*F1/F2**2                                              M14.2542
      GO TO 99                                                          M14.2543
C  REGION (3) BRANCHES --- T2 TO T3                                     M14.2544
  240 CONTINUE                                                          M14.2545
      WX=A(14)+A(15)*B(5)                                               M14.2546
      F1=A(8)*(A(9)-B(2))                                               M14.2547
      F2=A(10)+A(11)*B(3)                                               M14.2548
      F0=F1/F2+WX*(A(13)-B(2))                                          M14.2549
      DFDV=-A(8)/F2-WX                                                  M14.2550
      DFDI=A(11)*F1/F2**2                                               M14.2551
      GO TO 99                                                          M14.2552
C  REGION (4) BRANCHES --- T3 TO T4                                     M14.2553
  260 CONTINUE                                                          M14.2554
      F1=A(8)*(A(16)-B(2))                                              M14.2555
      F2=A(10)+A(17)*B(3)**2                                            M14.2556
      F0=F1/F2                                                          M14.2557
      DFDV=-A(8)/F2                                                     M14.2558
      DFDI=-2.0*A(17)*F1*B(3)/F2**2                                     M14.2559
      GO TO 99                                                          M14.2560
   99 CONTINUE                                                          M14.2561
      IF(DFDI.EQ.0) DFDI= EPSILN                                        M14.2562
      GI=B(3) +(DFDV*B(2) -F0)/DFDI                                     M14.2563
      YG=(-DFDV+2.0/DELTAT)/DFDI                                        M14.2564
      CG=B(2) *(-DFDV-2.0/DELTAT)/DFDI-B(3) +GI                         M14.2565
      ART=1.0/YLB+RB+1.0/YG                                             M14.2566
      VBLOCK=-CB/YLB                                                    M14.2567
      VGAP=-(CG+GI)/YG                                                  M14.2568
      AVT=VBLOCK+BE+VGAP                                                M14.2569
      IF(ISIGN.LT.0) SVT=-SVT                                           M14.2570
C  CORRECT ARRESTER INTERNAL NODE VOLTAGES                              M14.2571
      CARST=(SVT-AVT*B(10))/(ART*B(10)/B(11)-SRT)                       M14.2572
      CURR=CARST/B(11)                                                  M14.2573
      IF(CARST.LE.0.0) GO TO 93                                         M14.2574
      VGAP=VGAP+CURR/YG                                                 M14.2575
      IF(B(6) .GT.1.5) GO TO 91                                         M14.2576
      CIP= (CURR+B(3)) * ONEHAF                                         M14.2577
      IF(CURR.GT.A(6)) CIP=A(6)                                         M14.2578
      B(7) =B(7) +A(5)*CIP*DELTAT                                       M14.2579
      GO TO 92                                                          M14.2580
   91 IF(B(6) .GT.3.5) GO TO 92                                         M14.2581
      B(5)=B(5)+ (VGAP*CURR+B(2)*B(3)) * DELTA2                         M14.2582
   92 B(4)=B(3)                                                         M14.2583
      B(3)=CARST                                                        M14.2584
      B(2)=VGAP                                                         M14.2585
      B(1)=B(10)*(AVT+ART*CURR)                                         M14.2586
      B(8)=VBLOCK+CURR/YLB                                              M14.2587
      IF(ISIGN.GT.0) GO TO 94                                           M14.2588
      B(3)=-B(3)                                                        M14.2589
      B(1)=-B(1)                                                        M14.2590
      CARST=-CARST                                                      M14.2591
   94 B(9) = B(9) + B(1) * B(3) * DELTAT                                M19.1622
      GO TO 5681                                                        M14.2593
   93 B(6)=0.0                                                          M14.2594
      B(3)=0.0                                                          M14.2595
      B(1)=0.0                                                          M14.2596
      CARST=0.0                                                         M14.2597
 5681 IF ( IPRSUP  .GE.  2 )                                            M14.2598
     1 WRITE (LUNIT6, 5684)  ART, AVT, CARST,  ( A(I), B(I), I=1, 20 )  M17. 917
 5684 FORMAT ( /, 19H AT END  'ARREST' ., 12X, 3HART, 12X, 3HAVT,       M14.2600
     1         10X, 5HCARST, /, 19X, 3E15.6, /,  (1X, 8E16.6)  )        M14.2601
      RETURN                                                            M14.2602
      END                                                               M14.2603
      SUBROUTINE SOLVUM (                                               M28.5572
     1 REACL, GPAR, FPAR, HIST,UMCURP, NODVO1, NODVO2, JCLTAC,          M33.1898
     2 JCLOUT,JTYPE,NODOM, JTMTAC, HISTOM, OMEGM, OMOLD, THETAM,        M33.1899
     3 REAMDU, REAMDS, FLXDS, FLXDR, REAMQU, FLXQS, FLXQR,              M28.5575
     4 JCDSAT, JCQSAT, FLXD, FLXQ, NPPAIR, ROTMOM, NCLD,                M28.5576
     5 NCLQ, JTQOUT, JOMOUT, JTHOUT, REAMQS, EPSOM, DCOEF,              M28.5577
     6 KCOIL, VOLTUM, ANGLUM, NODFUM, NODMUM,                           M31.3570
     7 KUMOUT, JUMOUT, UMOUTP)                                          M31.3571
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  REACL(1), GPAR(1), FPAR(1), HIST(1), UMCURP(1)         M33.1900
      DIMENSION  NODVO1(1), NODVO2(1), JCLTAC(1), JCLOUT(1)             M28.5580
      DIMENSION  JTYPE(1), NODOM(1), JTMTAC(1), HISTOM(1)               M28.5581
      DIMENSION  OMEGM(1), OMOLD(1), THETAM(1)                          M28.5582
      DIMENSION  REAMDU(1), REAMDS(1), FLXDS(1), FLXDR(1)               M28.5583
      DIMENSION  REAMQU(1), FLXQS(1), FLXQR(1)                          M28.5584
      DIMENSION  JCDSAT(1), JCQSAT(1), FLXD(1), FLXQ(1)                 M28.5585
      DIMENSION  NPPAIR(1), ROTMOM(1), NCLD(1)                          M28.5586
      DIMENSION  NCLQ(1), JTQOUT(1), JOMOUT(1), JTHOUT(1)               M28.5587
      DIMENSION  REAMQS(1), EPSOM(1), DCOEF(1), KCOIL(1)                M28.5588
      DIMENSION VOLTUM(1), ANGLUM(1), NODFUM(1), NODMUM(1)              M31.3572
      DIMENSION KUMOUT(1), JUMOUT(1), UMOUTP(1)                         M31.3573
      DIMENSION ZTHS3(3,3), SMAT(3,3), ZMAT(3,3)                        M28.5590
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
C   JCLTAC(KCL AND KCL+1) ARE INITIALIZED IN "UMRENU"                   M39.3076
C   JCLTAC(KCL+2) IS INITIALIZED IN "UMINIT".                           M39.3077
C   AT EVERY TIME-STEP T SOLVUM MAY BE CALLED THREE TIMES (AT           M33.1901
C   TIME = 0.0 THERE IS AN ADDITIONAL CALL IN OVER16 TO                 M33.1902
C   CALCULATE UM HISTORY ARRAYS AND PREDICT VALUES OF THE               M33.1903
C   UM POWER CURRENTS IF CASE OF NO COMPENSATION).                      M33.1904
C   THE SEQUENCE TO THESE CALLS AT A CERTAIN TIME T IS AS :             M33.1905
C   * CALL 1 : SOLVUM TIME-STEP CALCULATIONS (CALL IN SUBTS4).          M33.1906
C        INPUT CONDITION : IF NO TCLOSE READING, LOOPSS(1)=7766         M34. 162
C                          IF TCLOSE READING , LOOPSS(1) = 6644         M34. 163
C                          IF NO POWER COMP,  LOOPSS(8)=1               M33.1908
C        AT COMPLETION : IF TCLOSE READING, LOOPSS(1)=6644              M33.1909
C                        IF NO POWER COMP,  LOOPSS(8)=3                 M34. 164
C             NOTE : AT T=0.0, CALL TO TCLOSE READING IS SKIPPED        M33.1913
C             AND AT COMPLETION : LOOPSS(1)=7766, LOOPSS(8)=3           M36.1594
C   * CALL 2 : IF TACS TRANSFER WITH SM TYPE-59 DATA IS DESIRED,        M33.1915
C              THEN THE SWITCH CURRENT IS TO BE READ FROM TCLOSE        M33.1916
C              (CALL IS IN SUBTS1, FIRST CALL TO SOLVUM)                M33.1917
C        INPUT CONDITION : LOOPSS(1)=6644                               M33.1918
C        AT COMPLETION : LOOPSS(1)=7766                                 M33.1919
C   * CALL 3 : IF THE POWER CIRCUITS OF ALL UM IS REQUESTED TO          M33.1921
C              BE NON-COMPENSATED (TO FACILITATE MULTI-MACH).           M33.1922
C              THIS CALL IS TO INJECT THE F-ARRAY WITH THE              M33.1923
C              PREDICTED UM POWER VOLTAGES FOR CALCULATIONS OF          M36.1595
C              NEXT TIME-STEP.                                          M33.1925
C              (CALL IS IN SUBTS1, SECOND CALL TO SOLVUM)               M33.1926
C        INPUT CONDITION : LOOPSS(8)=3                                  M33.1927
C        AT COMPLETION : LOOPSS(1) UNCHANGED                            M33.1928
C                        LOOPSS(8)=1                                    M33.1929
C   NOW WE ARE READY FOR THE NEXT CALL 1.                               M33.1930
C     INTRINSIC  COSZ, SINZ, SQRTZ                                      M32.6285
      IF (IPRSUP .GE. 1)                                                M32.6286
     1 WRITE (LUNIT6,7240) KCONST,IBR                                   M36.1596
 7240 FORMAT( 24H  "BEGIN MODULE SOLVUM.",8X,8HKCONST =,I4,             M36.1597
     1         8X,5HIBR =,I4)                                           M36.1598
      IF ( IPRSUP  .GE.  1 )                                            M36.1599
     1 WRITE (LUNIT6, 7246)  ISTART,LOOPSS(1),LOOPSS(8),NCLTOT,         M36.1600
     2                       NUMUM,NUMOUT,T                             M36.1601
 7246 FORMAT ( /,  19H TOP OF  "SOLVUM" .,                              M36.1602
     1  48H  ISTART LOOPSS1 LOOPSS8  NCLTOT   NUMUM  NUMOUT,            M36.1603
     2  13X, 1HT ,/, 19X, 6I8,E14.5)                                    M36.1604
      IF (IPRSUP .GE. 1 .AND. ISTART .EQ. 0)                            M36.1605
     1 WRITE (LUNIT6,7248) (HIST(N1),N1=1,NCLTOT)                       M36.1606
 7248 FORMAT(/,17H HIST(1:NCLTOT) =,(1X,6E14.5) )                       M36.1607
C.  MASS ANGLE AND SPEED HISTORY FOR TYPE-59 DATA INPUT *******         M32.6289
      IF (ISTART .GT. 0) GO TO 17040                                    M32.6290
      IF (NSMACH .EQ. 0) GO TO 70                                       M32.6291
      IF (UMOUTP(NUMOUT+1) .NE. -9999.0) GO TO 70                       M32.6292
      N5 = UMOUTP(NUMOUT+3)                                             M36.1608
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,7250) NUMOUT,N5                   M36.1609
 7250 FORMAT(/, 9H NUMOUT =,I4,10X,17HNUMOUT+NSMTAC+3 =,I4)             M36.1610
      IF (IPRSUP .GE. 1)                                                M36.1611
     1 WRITE (LUNIT6,7252) (UMOUTP(N1),N1=1,N5)                         M36.1612
 7252 FORMAT( 46H TACS INTERFACE REQUESTED FOR SM-59 DATA INPUT,        M36.1613
     1        47H. THE UM OUTPUT TABLE UMOUTP(NUMOUT+NSMTAC+3) :        M36.1614
     2    ,/,6(6X,E14.5)/(6(6X,E14.5)) )                                M36.1615
      D1 = UMOUTP(NUMOUT+2)                                             M32.6293
      IF (D1 .EQ. 0.0) GO TO 17042                                      M32.6294
      JM = 0                                                            M32.6295
C  JM IS COUNTER OF MACHINES WITH SM TYPE-50 DATA INPUT                 M32.6296
      N5 = NUMOUT + 4                                                   M32.6297
C   N5 IS FIRST ENTRY OF UMOUTP USED FOR TACS TRANSFER PURPOSE          M32.6298
      N6 = UMOUTP(NUMOUT+3) - 2                                         M32.6299
C   UMOUTP(NUMOUT+3) IS LAST ENTRY OF UMOUTP USED FOR TACS TRANSF       M32.6300
      N6 = N6 - 2                                                       M32.6301
17000 IF (N5 .GT. N6) GO TO 17042                                       M32.6302
      IF (UMOUTP(N5) .LE. -300.0) GO TO 17010                           M32.6303
      N5 = N5 + 1                                                       M32.6304
      GO TO 17000                                                       M32.6305
17010 JM = JM + 1                                                       M32.6306
      N7 = UMOUTP(N5+4)                                                 M32.6307
C  N7 IS TOTAL NR OF MASSES FOR MACHINE JM                              M32.6308
      N8 = UMOUTP(N5+3)                                                 M32.6309
C  N8 IS TOTAL NR OF UMOUTP ENTRIES NEEDED FOR MASS HISTORY             M32.6310
C    CALCULATIONS OF MACHINE JM.                                        M32.6311
      KSWSTA = - UMOUTP(N5) - 300                                       M32.6312
      D5 = THETAM(JM) + TWOPI/(4.0*NPPAIR(JM))                          M32.6313
C  HISTORY OF MASS NR 1 OF MACHINE JM :                                 M32.6314
      UMOUTP(N5+3) = 0.0                                                M32.6315
      IF (N7 .EQ. 1) UMOUTP(N5+3) = D5                                  M32.6316
      UMOUTP(N5+4) = E(N5+1)                                            M32.6317
      IF (N7 .EQ. 1) GO TO 17034                                        M32.6318
C  HISTORY OF MASS NR 2 AND HIGHER FOR MACHINE JM :                     M32.6319
      DO 17020 N15 = 2,N7                                               M32.6320
      N19 = N5 + (N15 - 1)*5                                            M32.6321
C  N19 IS A POINTER TO UMOUTP FOR MASS NR N15                           M32.6322
      N12 = UMOUTP(N19+1)                                               M32.6323
C  N12 IS NODE NR OF MASS NR N15                                        M32.6324
      N18 = N5 + (N15 - 2)*5                                            M32.6325
C  N18 IS A POINTER TO UMOUTP FOR MASS NR (N15-1)                       M32.6326
      N11 = UMOUTP(N18+1)                                               M32.6327
C  N11 IS NODE NR OF MASS NR (N15-1)                                    M32.6328
      N20 = KSWSTA + N15 - 1                                            M32.6329
C  N20 IS SWITCH NR OF SWITCH FROM MASS NR (N15-1) TO (N15)             M32.6330
C  UMOUTP(N19+3) WAS LMUT OF MASS NR (N15-1) AND (N15)                  M32.6331
C  UMOUTP(N19+4) WAS LMUT/RMUT OF MASS NR (N15-1) AND (N15)             M32.6332
      UMOUTP(N19+3) = UMOUTP(N18+3) + UMOUTP(N19+3)*TCLOSE(N20)         M32.6333
      D1 = E(N11) - E(N12)                                              M32.6334
      UMOUTP(N19+3) = UMOUTP(N19+3) + UMOUTP(N19+4)*D1                  M32.6335
C  NOW UMOUTP(N19+3) HAS BEEN SET TO HISTORY OF MASS ANGLE.             M32.6336
      IF (N12 .NE. NODOM(JM)) GO TO 17015                               M32.6337
      N14 = N15                                                         M32.6338
      D3 = UMOUTP(N19+3) - D5                                           M32.6339
C  N14 IS THE GENERATOR MASS OF MACHINE JM :                            M32.6340
17015 UMOUTP(N19+4) = E(N12)                                            M32.6341
C  NOW UMOUTP(N19+4) HAS BEEN SET TO HISTORY OF MASS SPEED.             M32.6342
17020 CONTINUE                                                          M32.6343
C  SHIFT OF ANGLES TO HAVE GEN ANGLE = D5 = THETAM(JM)+TWOPI/NPPAIR:    M32.6344
      DO 17030 N15 = 1,N7                                               M32.6345
      N19 = N5 + (N15-1)*5                                              M32.6346
      UMOUTP(N19+3) = UMOUTP(N19+3) - D3                                M32.6347
      IF (N15 .EQ. N14) UMOUTP(N19+3) = D5                              M32.6348
17030 CONTINUE                                                          M32.6349
17034 N5 = N5 + N8                                                      M32.6350
      GO TO 17000                                                       M32.6351
C. TACS TRANSFER IN CASE OF SM TYPE-59 DATA INPUT**************         M32.6352
17040 IF (LOOPSS(1) .NE. 6644) GO TO 70                                 M32.6353
C  LOOPSS(1) = 6644, MEANS THAT ONE OF THE UM HAS SM TYPE-59            M32.6354
C    DATA INPUT (INTELLIGENCE IN MACH DO LOOP) AND THAT SOLVUM          M32.6355
C    HAS TO BE CALLED FOR THE SECOND TIME AT EACH TIME STEP             M32.6356
C    TO PROCESS THE SWITCH CURRENTS FOR PASSING TORQUES, FIELD          M32.6357
C    VARIABLES AND MASS SPEEDS AND ANGLES TO TACS                       M32.6358
      LOOPSS(1) = 7766                                                  M32.6359
      N1 = UMOUTP(NUMOUT+3)                                             M37.6075
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,7250) NUMOUT,N1                  M37.6076
      IF (IPRSUP .GE. 1)                                                M37.6077
     1 WRITE (LUNIT6,7252) (UMOUTP(N2),N2=1,N1)                         M37.6078
C  N5 IS FIRST ENTRY OF UMOUTP USED FOR TACS TRANSFER REQUEST:          M32.6360
17042 N5 = NUMOUT + 4                                                   M32.6361
C  UMOUTP(NUMOUT+3) IS LAST ENTRY OF UMOUTP USED FOR TACS TRANSF        M32.6362
      N6 = UMOUTP(NUMOUT+3) - 2                                         M32.6363
17050 IF (ISTART .NE. 0) GO TO 17060                                    M32.6364
      IF (N5 .GT. N6) GO TO 70                                          M32.6365
      GO TO 17070                                                       M32.6366
17060 IF (N5 .GT. N6) GO TO 21000                                       M32.6367
17070 N7 = UMOUTP(N5+1)                                                 M32.6368
C  N7 IS THE NODE NR OR THE SWITCH NR                                   M32.6369
      N8 = UMOUTP(N5+2)                                                 M32.6370
C  N8 IS THE ENTRY OF THE TACS TABLE ETAC                               M32.6371
      N10 = UMOUTP(N5)                                                  M32.6372
C  N10 IS THE CODE TYPE FOR THE VARIABLES TO BE TRANSFERED              M32.6373
C  TRANSFER OF SHAFT TORQUES :                                          M32.6374
      IF (N10 .NE. -1) GO TO 17100                                      M32.6375
      ETAC(N8) = - TCLOSE(N7)                                           M32.6376
      N5 = N5 + 3                                                       M32.6377
      GO TO 17050                                                       M32.6378
C  TRANSFER OF MASS SPEEDS :                                            M32.6379
17100 IF (N10 .NE. -2) GO TO 17110                                      M32.6380
      ETAC(N8) = E(N7)                                                  M32.6381
      N5 = N5 + 3                                                       M32.6382
      GO TO 17050                                                       M32.6383
C  TRANSFER OF MASS ANGLES :                                            M32.6384
17110 IF (N10 .LE. -300) GO TO 17112                                    M32.6385
      IF (N10 .NE. -3) GO TO 17116                                      M32.6386
17112 IF (N8 .NE. 0) GO TO 17114                                        M32.6387
      N5 = N5 + 5                                                       M32.6388
      GO TO 17050                                                       M32.6389
17114 D1 = DELTAT/2.0                                                   M32.6390
      ETAC(N8) = UMOUTP(N5+3) + (UMOUTP(N5+4) * E(N7))*D1               M32.6391
      UMOUTP(N5+3) = ETAC(N8)                                           M32.6392
      UMOUTP(N5+4) = D1 * E(N7)                                         M32.6393
      N5 = N5 + 5                                                       M32.6394
      GO TO 17050                                                       M32.6395
17116 IF (N10 .NE. -299) GO TO 17120                                    M32.6396
      N5 = N5 + 5                                                       M32.6397
      GO TO 17050                                                       M32.6398
C  TRANSFER OF EXCITER CURRENT                                          M32.6399
17120 IF (N10 .NE. -4) GO TO 17130                                      M32.6400
      ETAC(N8) = - TCLOSE(N7)                                           M32.6401
      N5 = N5 + 3                                                       M32.6402
      GO TO 17050                                                       M32.6403
C  TRANSFER OF EXCITER VOLTAGE                                          M32.6404
17130 IF (N10 .NE. -5) GO TO 17140                                      M32.6405
      ETAC(N8) = E(N7)                                                  M32.6406
      N5 = N5 + 3                                                       M32.6407
      GO TO 17050                                                       M32.6408
17140 WRITE (LUNIT6,17142)                                              M32.6409
17142 FORMAT(/ , 35H ERROR STOP.   SOMETHING WRONG WITH,                M32.6410
     1           37H SM TYPE-59 TRANSFER TO TACS. CONSULT               M32.6411
     2  ,/,      17H EMTP MANAGEMENT. )                                 M32.6412
      CALL STOPTP                                                               
C.DYNAMIC (START OF TIME-STEP LOOP) ****************************        M28.5595
   70 IF (LOOPSS(8) .EQ. 3) GO TO 1000                                  M36.1616
C     LOOPSS(8) = 3, MEANS THAT F-ARRAY IS TO BE INJECTED               M33.1932
C       IN SUBTS1 IN CASE OF NO COMPENSATION FOR POWER CIRCUITS.        M33.1933
C. INTIALIZATION OF FIRST ELEMENT OF CURSUB, I.E. FIRST                 M28.5596
C  USED COIL OF FIRST USED MACHINE.                                     M28.5597
      NCLCOM = ISUBEG(KSUBUM)                                           M33.1934
C. PROGRAM STOP IF SPEED CONVERGENCE FAILED IN PREVIOUS                 M28.5604
C. TIME STEP.                                                           M28.5605
   79 IF (INPU .NE. 50) GO TO 90                                        M32.6415
      WRITE(LUNIT6,80)                                                  M28.5607
   80 FORMAT(43H0PROGRAM STOP BECAUSE OF UM NONCONVERGENCE.,            M36.1617
     1       24H  POSSIBLE REASONS ARE :)                               M36.1618
      WRITE(LUNIT6,82)                                                  M28.5611
   82 FORMAT(21H * TIME STEP TOO HIGH)                                  M28.5612
      WRITE(LUNIT6,83)                                                  M28.5613
   83 FORMAT(35H * CONVERGENCE MARGIN EPSOM TOO LOW)                    M28.5614
      WRITE(LUNIT6,84)                                                  M28.5615
   84 FORMAT(25H * ERROR IN UM DATA INPUT)                              M28.5616
      CALL STOPTP                                                               
   90 SELTAT = DELTAT                                                   M28.5618
C. START OF FIRST MACHINE DO-LOOP:                                      M39.3078
 1000 NXOUT = 0                                                         M28.5624
      MSHARE = 0                                                                
      DO 99999 JM = 1,NUMUM                                             M32.6417
C  ADJUSTING OMEGRF TO ROTMOM(JM) IF MECH NETWORK OPTION USED:          M37.6081
      OMEGRF = TWOPI * STATFR                                           M37.6082
      IF (JTMTAC(JM) .NE. 0) GO TO 1004                                 M37.6083
      OMEGRF = ROTMOM(JM)                                               M37.6084
 1004 TAU = OMEGRF * T                                                  M38.3490
C  INJECTION OF PREDICTED UM POWER CURRENT TO F ARRAY IN SUBTS1.        M33.1945
      IF (LOOPSS(8) .NE. 3) GO TO 1100                                  M38.3491
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,1006) JM                          M36.1619
 1006 FORMAT(/,40H ***************************************,             M36.1620
     1         43H INJECTION OF PREDICTED POWER-COIL CURRENTS,          M36.1621
     2         14H FOR UM NUMBER,I4,2H :                                M36.1622
     3      ,/,40H ***************************************,             M36.1623
     4          2X,6HNODVO1,5X,9HF(NODVO1),                             M36.1624
     5          2X,6HNODVO2,5X,9HF(NODVO2) )                            M36.1625
      DO 1030 N1 = 1,3                                                  M36.1626
      N2 = 3 * (NUMUM + JM - 1) + N1                                    M33.1948
      N3 = KCOIL(JM) - 1 + N1                                           M33.1949
      N4 = NODVO1(N3)                                                   M33.1950
      IF (N4 .EQ. NODVO2(N3)) GO TO 1030                                M36.1627
      IF (N4 .LE. 1) GO TO 1010                                         M33.1952
      F(N4) = F(N4) + UMCURP(N2)                                        M33.1953
 1010 N5 = NODVO2(N3)                                                   M36.1628
      IF (N5 .LE. 1) GO TO 1020                                         M36.1629
      F(N5) = F(N5) - UMCURP(N2)                                        M36.1630
 1020 D4 = F(N4)                                                        M36.1631
      D5 = F(N5)                                                        M36.1632
      IF (N4 .LE. 1) D4 = 0.0                                           M36.1633
      IF (N5 .LE. 1) D5 = 0.0                                           M36.1634
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,1022) N3,N4,D4,N5,D5              M36.1635
 1022 FORMAT(10H *********,                                             M36.1636
     1       12H COIL NUMBER,I4,2H :,14X,I6,E14.5,2X,I6,E14.5)          M36.1637
 1030 CONTINUE                                                          M36.1638
      IF (JM .EQ. NUMUM) LOOPSS(8) = 1                                  M33.1958
      IF (JM .EQ. NUMUM) GO TO 21000                                    M33.1959
      GO TO 99999                                                       M33.1960
C     NCURPR IS SET TO 1 ON INITIATION OF PREDICTION CALCULATION        M33.1961
C     OF UM POWER VOLTAGES IF POWER CIRCUITS ARE NOT COMPENSATED        M36.1639
 1100 NCURPR = 0                                                        M33.1963
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,1110) JM                         M36.1640
 1110 FORMAT (/,40H ***************************************,            M36.1641
     1          36H DYNAMIC RUN OF SOLVUM FOR UM NUMBER,I4,2H :)        M36.1642
      IF (LOOPSS(8) .EQ. 1) FLXDMH = FLXD(JM)                           M33.1964
      IF (LOOPSS(8) .EQ. 1) FLXQMH = FLXQ(JM)                           M33.1965
      KCL = KCOIL(JM)                                                   M38.3492
C  IN CASE OF SM TYPE-59 DATA INPUT                                     M32.6418
      NTYP59 = 0                                                        M32.6419
      IF (JTYPE(JM) .NE. 13) GO TO 1200                                 M32.6420
      IF (UMOUTP(NUMOUT+1) .EQ. -9999.0) LOOPSS(1) = 6644               M32.6421
      IF (ISTART .EQ. 0) LOOPSS(1) = 7766                               M32.6422
      JTYPE(JM) = 1                                                     M32.6423
      NTYP59 = 1                                                        M32.6424
      INPUST = INPU                                                     M32.6425
      INPU = 0                                                          M32.6426
      NMEXC = 0                                                         M38.3493
      IF (FPAR(KCL+3) .GE. 0.0) GO TO 1200                              M38.3494
      KCONEX = - FPAR(KCL+3)                                                    
      NMEXC = FPAR(KCL+4)                                                       
      NEXC = FPAR(KCL+5)                                                        
 1200 IF (INPU .NE. 1) GO TO 1210                                       M32.6427
      SELTAT = OMEGRF * DELTAT                                          M32.6428
 1210 SELTA2 = SELTAT/2.0                                               M32.6429
      RPPAIR = NPPAIR(JM)                                               M32.6430
      IF (ISTART .NE. 0) GO TO 1220                                     M35.1637
      IF (JCDSAT(JM) .NE. 5) GO TO 1220                                 M35.1638
      REAMQU(JM) = REAMDU(JM)                                           M35.1639
      REAMQS(JM) = REAMDS(JM)                                           M35.1640
      FLXQR(JM) = FLXDR(JM)                                             M35.1641
      FLXQS(JM) = FLXDS(JM)                                             M35.1642
 1220 IF (INPU .EQ. 2) GO TO 1300                                       M35.1643
      FLXDRR = 0.0                                                      M28.5630
      FLXQRR = 0.0                                                      M28.5631
      GO TO 1600                                                        M28.5632
C  IF REMANENT FLUX OPTION IS REQUESTED (INPU = 2) :                    M35.1644
 1300 IF (ISTART .NE. 0) GO TO 1450                                     M30. 822
      N1 = JCDSAT(JM) + JCQSAT(JM)                                      M28.5634
      IF (N1 .NE. 0) GO TO 1400                                         M28.5635
      FLXDR(JM) = 0.0                                                   M28.5636
      FLXQR(JM) = 0.0                                                   M28.5637
 1400 IF (FLXDR(JM) .LT. 0.0) FLXDR(JM) = - FLXDR(JM)                   M28.5638
      IF (FLXQR(JM) .LT. 0.0) FLXQR(JM) = - FLXQR(JM)                   M30. 823
 1450 IF (JCDSAT(JM) .NE. 5) GO TO 1500                                 M30. 829
      FLXDRR = FLXDR(JM)                                                M30. 830
      FLXQRR = FLXDRR                                                   M30. 831
      FLXDA = 0.0                                                       M35.1645
      FLXQA = 0.0                                                       M35.1646
      GO TO 1999                                                        M30. 832
 1500 FLXDRR = FLXDR(JM)                                                M30. 833
      FLXQRR = FLXQR(JM)                                                M30. 834
C   CALCULATION OF FLXDA AND FLXQA (SATURATION LINE) IF NO              M35.1647
C     TOTAL SATURATION (IF JCDSAT = 5 , CALCULATION IS LATER):          M35.1648
 1600 IF (REAMDU(JM) .EQ. 0.0) FLXDA = 0.0                              M30. 835
      IF (REAMDU(JM) .EQ. 0.0) GO TO 1610                               M30. 836
      FLXDA = (REAMDS(JM)/REAMDU(JM)) * (FLXDRR-FLXDS(JM))              M28.5653
      FLXDA = FLXDA + FLXDS(JM)                                         M28.5654
      IF (JCDSAT(JM) .EQ. 0) FLXDA = 0.0                                M30. 837
 1610 FLXQA = (REAMQS(JM)/REAMQU(JM)) * (FLXQRR-FLXQS(JM))              M30. 838
      FLXQA = FLXQA + FLXQS(JM)                                         M28.5656
      IF (JCQSAT(JM) .EQ. 0) FLXQA = 0.0                                M30. 839
 1999 DO 2000 N1 = 1,10                                                 M28.5657
 2000 CON(N1) = 0.0                                                     M28.5658
      IF ( JTYPE(JM)  .GE.  13 )   GO TO 2001                           M30. 840
      N9 = JTYPE(JM)                                                    M30. 841
      GO TO (2001,2002,2003,2004,2005,2006,2007,2008,                   M28.5659
     V2009,2010,2011,2012),N9                                           M30. 842
 2001 CON(3) = 1.0                                                      M28.5661
      CON(5) = 1.0                                                      M28.5662
      GOTO 3000                                                         M28.5663
 2002 CON(2) = 1.0                                                      M28.5664
      CON(5) = 1.0                                                      M28.5665
      GOTO 3000                                                         M28.5666
 2003 CON(3) = 1.0                                                      M28.5667
      GO TO 3000                                                        M28.5668
 2004 CON(3) = 1.0                                                      M28.5669
      CON(6) = 1.0                                                      M28.5670
      GO TO 3000                                                        M28.5671
 2005 CON(2) = 1.0                                                      M28.5672
      GO TO 3000                                                        M28.5673
 2006 CON(1) = 1.0                                                      M28.5674
      GO TO 3000                                                        M28.5675
 2007 CON(10) = 1.0                                                     M28.5676
      GO TO 3000                                                        M28.5677
 2008 CON(4) = 1.0                                                      M28.5678
      GO TO 3000                                                        M28.5679
 2009 CON(4) = 1.0                                                      M28.5680
      CON(7) = 1.0                                                      M28.5681
      GO TO 3000                                                        M28.5682
 2010 CON(4) = 1.0                                                      M28.5683
      CON(7) = 1.0                                                      M28.5684
      GO TO 3000                                                        M28.5685
 2011 CON(4) = 1.0                                                      M28.5686
      CON(8) = 1.0                                                      M28.5687
      GO TO 3000                                                        M28.5688
 2012 CON(4) = 1.0                                                      M28.5689
      CON(8) = 1.0                                                      M28.5690
      CON(9) = 1.0                                                      M28.5691
C. COIL MARKINGS :                                                      M28.5692
 3000 KCLD1 = KCOIL(JM) + 3                                             M28.5693
      KCLDE = KCOIL(JM) + 2  + NCLD(JM)                                 M28.5694
      KCLQ1 = KCLDE + 1                                                 M28.5695
      KCLQE = KCLDE + NCLQ(JM)                                          M28.5696
      KCLE  = KCLQE                                                     M28.5697
      NCL   = 3 + NCLD(JM) + NCLQ(JM)                                   M28.5698
      IF (CON(6) .LT. 1.0) GO TO 3010                                   M28.5699
      NCL = NCL + 1                                                     M28.5700
      KCLE = KCLE + 1                                                   M28.5701
 3010 DO 3100 N1 = KCL,KCLE                                             M33.1966
 3100 FPAR(N1) = 1.0/(1.0 + GPAR(N1)*REACL(N1)/SELTA2)                  M28.5703
C. ZERO INITIALIZTION OF ALL THEV VARIABLES :                           M28.5704
      ZTHEV = 0.0                                                       M28.5705
      DO 3500 N1 = 1,NCL                                                M28.5706
      VINP(N1) = 0.0                                                    M28.5707
 3500 ZTHEVS(N1) = 0.0                                                  M28.5708
      DO 3520 N1 = 1,3                                                  M28.5709
      DO 3520 N2 = 1,3                                                  M28.5710
      ZTHEVR(N1,N2) = 0.0                                               M28.5711
 3520 ZTHS3(N1,N2) = 0.0                                                M28.5712
C. THEV VOLTAGES AND IMPEDANCES FOR UM-STATOR(EXCITATION):              M33.1967
      N15 = KCLE - KCL - 2                                              M28.5714
      IF (NCL .GT. 6) N15 = 3                                           M28.5715
      DO 4000 N1 = 1,9                                                  M28.5716
 4000 NDUM(N1) = 0                                                      M28.5717
      DO 4011 N1 = 1,N15                                                M28.5718
      N2 = KCL + 2 + N1                                                 M28.5719
      NDUM(N1) = NODVO1(N2)                                             M28.5720
      NDUM(N1+3) = NODVO2(N2)                                           M28.5721
      NDUM(N1+6) = NODVO1(N2) - NODVO2(N2)                              M28.5722
 4011 CONTINUE                                                          M28.5723
      DO 4022 N1 = 1,N15                                                M28.5724
      N2 = NDUM(N1)                                                     M28.5725
      N3 = NDUM(N1+3)                                                   M28.5726
      N4 = N1 + 6                                                       M28.5727
      IF (NDUM(N4) .EQ. 0) GO TO 4022                                   M28.5728
      VINP(N1+3) = - E(N2) + E(N3)                                      M28.5729
 4022 CONTINUE                                                          M28.5730
      IF (ISTART .EQ. 0) GO TO 4110                                     M28.5731
      N20 = 0                                                           M28.5732
      DO 4033 N1 = 1,N15                                                M28.5733
      N2 = NDUM(N1) + N20 * NTOT                                        M28.5734
      N3 = NDUM(N1+3) + N20 * NTOT                                      M28.5735
      N5 = NDUM(N1)                                                     M28.5736
      N6 = NDUM(N1+3)                                                   M28.5737
      N7 = N1 + 6                                                       M28.5738
      N8 = NDUM(1) + N20 * NTOT                                         M28.5739
      N9 = NDUM(4) + N20 * NTOT                                         M28.5740
      IF (NDUM(N7) .EQ. 0) GO TO 4033                                   M28.5741
      N20 = N20 + 1                                                     M28.5742
      ZTHS3(N1,N1) = - ZNONL(N2) + ZNONL(N3)                            M28.5743
      IF (N1 .NE. 1) GO TO 4030                                         M32.6431
      D1 = DCOEF(JM) * DCOEF(JM)                                        M32.6432
      IF (NTYP59 .EQ. 1) ZTHS3(1,1) = ZTHS3(1,1)*D1                     M32.6433
      IF (N15 .EQ. 1) GO TO 4200                                        M32.6434
      GO TO 4033                                                        M32.6435
 4030 IF (NDUM(7) .EQ. 0) GO TO 4033                                    M32.6436
      ZTHS3(1,N1) = - ZNONL(N8) + ZNONL(N9)                             M28.5747
      ZTHS3(N1,1) = - ZNONL(N5) + ZNONL(N6)                             M28.5748
 4033 CONTINUE                                                          M28.5749
      IF (N15 .LT. 3) GO TO 4200                                        M28.5750
      N19 = 0                                                           M28.5751
      N20 = 0                                                           M28.5752
      N1 = NDUM(8) * NDUM(9)                                            M28.5753
      IF (N1 .EQ. 0) GO TO 4110                                         M28.5754
      IF (NDUM(7) .NE. 0) N20 = 1                                       M28.5755
      N1 = 3                                                            M28.5756
 4044 N2 = NDUM(N1) + N20 * NTOT                                        M28.5757
      N3 = NDUM(N1+3) + N20 * NTOT                                      M28.5758
      IF (N19 .EQ. 1) GO TO 4045                                        M28.5759
      ZTHS3(3,2) = - ZNONL(N2) + ZNONL(N3)                              M28.5760
      N20 = N20 + 1                                                     M28.5761
      N19 = 1                                                           M28.5762
      N1 = 2                                                            M28.5763
      GO TO 4044                                                        M28.5764
 4045 ZTHS3(2,3) = - ZNONL(N2) + ZNONL(N3)                              M28.5765
 4110 IF (NCL .LE. 6) GO TO 4200                                        M28.5766
      N15 = KCLD1 + 3                                                   M28.5767
      DO 4120 N1 = N15,KCLE                                             M28.5768
      N2 = NODVO1(N1)                                                   M28.5769
      N3 = NODVO2(N1)                                                   M28.5770
      N10 = N1 - KCL + 1                                                M28.5771
      IF (NODVO1(N1) .EQ. NODVO2(N1)) GO TO 4120                        M28.5772
      IF (ISTART .EQ. 0) GO TO 4115                                     M28.5773
      ZTHEVS(N10) = - ZNONL(N2) + ZNONL(N3)                             M28.5774
 4115 VINP(N10) = - E(N2) + E(N3)                                       M28.5775
 4120 CONTINUE                                                          M28.5776
C. THEV VOLTAGES AND IMPEDANCES FOR ROTOR(POWER)-COILS :                M33.1968
 4200 DO 4211 N1 = 1,3                                                  M28.5778
      N2 = N1 + KCL - 1                                                 M28.5779
      NDUM(N1) = NODVO1(N2)                                             M28.5780
      NDUM(N1+3) = NODVO2(N2)                                           M28.5781
      NDUM(N1+6) = NODVO1(N2) - NODVO2(N2)                              M28.5782
 4211 CONTINUE                                                          M28.5783
      DO 4222 N1 = 1,3                                                  M28.5784
      N2 = NDUM(N1)                                                     M28.5785
      N3 = NDUM(N1+3)                                                   M28.5786
      N4 = N1 + 6                                                       M28.5787
      IF (NDUM(N4) .EQ. 0) GO TO 4222                                   M28.5788
      VINP(N1) = E(N2) - E(N3)                                          M28.5789
 4222 CONTINUE                                                          M28.5790
      IF (LOOPSS(8) .EQ. 1) GO TO 5000                                  M33.1969
      IF (ISTART .EQ. 0) GO TO 5000                                     M28.5791
      N20 = 0                                                           M28.5792
      DO 4233 N1 = 1,3                                                  M28.5793
      N2 = NDUM(N1) + N20 * NTOT                                        M28.5794
      N3 = NDUM(N1+3) + N20 * NTOT                                      M28.5795
      N5 = NDUM(N1)                                                     M28.5796
      N6 = NDUM(N1+3)                                                   M28.5797
      N7 = N1 + 6                                                       M28.5798
      N8 = NDUM(1) + N20 * NTOT                                         M28.5799
      N9 = NDUM(4) + N20 * NTOT                                         M28.5800
      IF (NDUM(N7) .EQ. 0) GO TO 4233                                   M28.5801
      N20 = N20 + 1                                                     M28.5802
      ZTHEVR(N1,N1) = - ZNONL(N2) + ZNONL(N3)                           M28.5803
      IF (N1 .EQ. 1) GO TO 4233                                         M28.5804
      IF (NDUM(7) .EQ. 0) GO TO 4233                                    M28.5805
      ZTHEVR(1,N1) = - ZNONL(N8) + ZNONL(N9)                            M28.5806
      ZTHEVR(N1,1) = - ZNONL(N5) + ZNONL(N6)                            M28.5807
 4233 CONTINUE                                                          M28.5808
      ZTHEV = CON(4) * ZTHEVR(3,3)                                      M28.5809
      ZTHEVR(3,3) = ZTHEVR(3,3) * (CON(2) + CON(3))                     M28.5810
      N19 = 0                                                           M28.5811
      N20 = 0                                                           M28.5812
      N1 = NDUM(8) * NDUM(9)                                            M28.5813
      IF (N1 .EQ. 0) GO TO 5000                                         M28.5814
      IF (NDUM(7) .NE. 0) N20 = 1                                       M28.5815
      N1 = 3                                                            M28.5816
 4244 N2 = NDUM(N1) + N20 * NTOT                                        M28.5817
      N3 = NDUM(N1+3) + N20 * NTOT                                      M28.5818
      IF (N19 .EQ. 1) GO TO 4245                                        M28.5819
      ZTHEVR(3,2) = - ZNONL(N2) + ZNONL(N3)                             M28.5820
      N20 = N20 + 1                                                     M28.5821
      N19 = 1                                                           M28.5822
      N1 = 2                                                            M28.5823
      GO TO 4244                                                        M28.5824
 4245 ZTHEVR(2,3) = - ZNONL(N2) + ZNONL(N3)                             M28.5825
C. GENERATE THE SCALAR PARAMETER RD2 FOR SPECIAL DM:                    M28.5826
 5000 RD2 = 0.0                                                         M28.5827
      IF (JTYPE(JM) .LT. 9) GO TO 6000                                  M28.5828
      IF (JTYPE(JM) .GT. 11) GO TO 6000                                 M28.5829
      IF (GPAR(KCL+4) .NE. 0.0) RD2 = 1.0/GPAR(KCL+4)                   M28.5830
C. UPDATE FOR SPECIAL DM-TYPES: CSC + CPC = 1                           M28.5831
 6000 D1 = CON(7) + CON(8)                                              M28.5832
      IF (D1 .EQ. 0.0) GO TO 7000                                       M28.5833
      REACL(KCL+2) = REACL(KCL+2) + D1 * REACL(KCL+4)                   M28.5834
      REACL(KCL+3) = REACL(KCL+3) + D1 * REACL(KCL+4)                   M28.5835
      FPAR(KCL +2) = 1.0/(1.0 + GPAR(KCL+2)*REACL(KCL+2)/SELTA2)        M28.5836
      FPAR(KCL+3) = 1.0/(1.0 + GPAR(KCL+3)*REACL(KCL+3)/SELTA2)         M28.5837
      D2 = D1 * ZTHEV + CON(8)*(REACL(KCL+4)+RD2)/SELTA2                M28.5838
      BETAQ = FPAR(KCL+2) * GPAR(KCL+2) * D2                            M28.5839
      BETAD1 = FPAR(KCL+3) * GPAR(KCL+3) * D2                           M28.5840
C. THEVENIN MECHANICAL IMPEDANCE AND SPEED:                             M28.5841
 7000 N1 = NODOM(JM)                                                    M28.5842
      NSHARE = JCLTAC(KCL) + JCLTAC(KCL+1)                              M39.3080
      IF (NSHARE .NE. 0) MSHARE = 1                                     M39.3081
      ZTHEVM = 0.0                                                      M28.5843
      OMTHEV = 0.0                                                      M28.5844
      ZTHEM2 = 0.0                                                      M39.3082
      ZTHEM3 = 0.0                                                      M39.3083
      TQGEN2 = 0.0                                                      M39.3084
      TQGEN3 = 0.0                                                      M39.3085
      IF (JTMTAC(JM) .NE. 0) GO TO 8000                                 M28.5845
      OMTHEV = E(N1)                                                    M39.3086
      IF (ISTART .EQ. 0) GO TO 8000                                     M39.3087
      ZTHEVM = - ZNONL(N1)                                              M39.3088
C  IF MORE UM'S SHARE THE SAME MECH NETWORK OF THIS UM NR. JM :         M39.3089
      IF (NSHARE .EQ. 0) GO TO 8000                                     M39.3090
      N12 = JCLTAC(KCL)                                                         
      TQGEN2 = VOLTUM(N12)                                                      
      N13 = JCLTAC(KCL+1)                                                       
      IF (N13 .EQ. 0) GO TO 7010                                                
      TQGEN3 = VOLTUM(N13)                                                      
 7010 N4 = 0                                                                    
      IF (JM .GT. N12 ) N4 = 1                                          M39.3097
      IF (JM .GT. N13 .AND. N13 .NE. 0) N4 = N4 + 1                     M39.3098
      N5 = 0                                                                    
      IF (N12 .GT. JM) N5 = 1                                           M39.3100
      IF (N12 .GT. N13 .AND. N13 .NE. 0) N5 = N5 + 1                    M39.3101
C  NOW POSITION THE CORRECT ENTRY TO THE TABLE ZNONL :                  M39.3102
      N1 = NODOM(JM) + N4*NTOT                                          M39.3103
      N2 = NODOM(JM) + N5*NTOT                                          M39.3104
      ZTHEVM = - ZNONL(N1)                                              M39.3105
      ZTHEM2 = - ZNONL(N2)                                              M39.3106
      IF (N13 .EQ. 0) GO TO 8000                                                
      N6 = 3 - (N4 + N5)                                                        
      N3 = NODOM(JM) + N6*NTOT                                          M39.3109
      ZTHEM3 = - ZNONL(N3)                                              M39.3110
C. TACS INPUT FOR UM-STATOR (EXCITATION COILS) :                        M38.3498
 8000 DO 8002 N1 = 4,NCL                                                        
      N2 = KCL - 1 + N1                                                 M28.5851
      N3 = JCLTAC(N2)                                                   M28.5852
      IF (N3 .EQ. 0) GO TO 8002                                         M28.5853
      IF (N1 .GT. 3) GO TO 8001                                         M28.5854
      VINP(N1) = VINP(N1) + XTCS(N3)                                    M28.5855
      GO TO 8002                                                        M28.5856
 8001 VINP(N1) = VINP(N1) - XTCS(N3)                                    M28.5857
 8002 CONTINUE                                                          M28.5858
      IF (NTYP59 .EQ. 1) VINP(4) = VINP(4)*DCOEF(JM)                    M32.6437
C. VOLTAGE INPUT FOR SPECIAL DM:                                        M28.5859
      IF (D1 .EQ. 0.0) GO TO 9000                                       M28.5860
      VINP(4) = - D1 * VINP(3)                                          M28.5861
      VINP(5) = 0.0                                                     M28.5862
C.TACS INPUT FOR MECHANICAL INPUT TORQUE:                               M28.5863
 9000 TUMTAC = 0.0                                                      M28.5864
      IF (JTMTAC(JM) .EQ. 0) GO TO 11000                                M28.5865
      N1 = JTMTAC(JM)                                                   M28.5866
      TUMTAC = XTCS(N1)                                                 M28.5867
C. DETERMINATION OF HISTORY AT TIME = 0.0 :                             M28.5868
11000 IF (ISTART .NE. 0) GO TO 12000                                    M28.5869
      THETAE = RPPAIR * (1.0-CON(1)-CON(4)) * THETAM(JM)                M32.6438
      OMEGE = RPPAIR * OMEGM(JM)                                        M32.6439
      GO TO 12200                                                       M28.5872
C  CALCULATION OF UM POWER CURRENTS FOR OUTPUT :                        M33.1971
12000 IF (LOOPSS(8) .EQ. 0) GO TO 12004                                 M33.1972
      N3 = 3 * (NUMUM + JM - 1)                                         M33.1973
      CURUM1 = UMCURP(N3+1) - GPAR(KCL+2)*VINP(1)                       M33.1974
      CURUM2 = UMCURP(N3+2) - GPAR(KCL+2)*VINP(2)                       M33.1975
      CURUM3 = UMCURP(N3+3) - GPAR(KCL+2)*VINP(3)                       M33.1976
      GO TO 12016                                                       M36.1643
C. STORING ZTHEVR AND VINP(1:3) FOR NITROM:                             M28.5873
12004 DO 12015 N1 = 1,3                                                 M33.1977
      ZTHEVS(N1) = VINP(N1)                                             M28.5875
      DO 12010 N2 = 1,3                                                 M28.5876
12010 ZMAT(N1,N2) = ZTHEVR(N1,N2)                                       M28.5877
12015 CONTINUE                                                          M28.5878
C. PREDICTION FOR OMEGA-ITERATION:                                      M28.5879
12016 OMEGMP = 2.0 * OMEGM(JM) - OMOLD(JM)                              M33.1978
      OMOLD(JM) = OMEGM(JM)                                             M28.5881
      THEOLD = THETAM(JM)                                               M30. 843
      NITROM = -1                                                       M28.5883
12020 NITROM = NITROM + 1                                               M28.5884
      IF (LOOPSS(8) .NE. 0) GO TO 12100                                 M36.1644
      DO 12035 N1 = 1,3                                                 M28.5885
      VINP(N1) = ZTHEVS(N1)                                             M28.5886
      DO 12030 N2 = 1,3                                                 M28.5887
12030 ZTHEVR(N1,N2) = ZMAT(N1,N2)                                       M28.5888
12035 CONTINUE                                                          M28.5889
12100 THETAM(JM) = THEOLD + SELTA2 * (OMOLD(JM) + OMEGMP)               M30. 844
      THETAE = RPPAIR * (1.0-CON(1)-CON(4))*THETAM(JM)                  M32.6440
      OMEGE = RPPAIR * OMEGMP                                           M32.6441
C.SET UP PARK TRANSFORMATION:                                           M28.5893
12200 PTHETA(1,1) = 1.0                                                 M28.5894
      PTHETA(2,2) = COSZ(THETAE)                                        M28.5895
      PTHETA(2,3) = - SINZ(THETAE)                                      M28.5896
      PTHETA(3,2) = - PTHETA(2,3)                                       M28.5897
      PTHETA(3,3) = PTHETA(2,2)                                         M28.5898
      DO 12210 N1 = 2,3                                                 M28.5899
      PTHETA(1,N1) = 0.0                                                M28.5900
12210 PTHETA(N1,1) = PTHETA(1,N1)                                       M28.5901
      DO 12220 N1 = 1,3                                                 M28.5902
12220 DUMMAT(1,N1) = CON(3)/SROOT3                                      M28.5903
      DUMMAT(2,1) = CON(3) * SROOT2/SROOT3                              M28.5904
      DUMMAT(2,3) = - CON(3)/(SROOT2*SROOT3)                            M28.5905
      DUMMAT(2,2) = DUMMAT(2,3) + CON(2) + CON(10)                      M28.5906
      DUMMAT(3,1) = 0.0                                                 M28.5907
      DUMMAT(3,2) = - CON(3)/SROOT2                                     M28.5908
      DUMMAT(3,3) = -DUMMAT(3,2)+CON(2)+CON(1)-CON(10)+CON(4)           M30. 845
      CALL MATMUL(PTHETA,DUMMAT)                                        M28.5910
      IF (NCURPR .EQ. 1) GO TO 14609                                    M33.1980
C. UPDATING FOR SPECIAL IM(3-PHASE ROTOR) AND SMAT :                    M28.5911
      IF (CON(6) .NE. 1.0) GO TO 12300                                  M28.5912
      IF (NITROM .GT. 0) GO TO 12300                                    M28.5913
      DO 12230 N1 = 1,3                                                 M28.5914
      N3 = N1 + 1                                                       M28.5915
      IF (N1 .EQ. 3) N3 = 1                                             M28.5916
      DO 12225 N2 = 1,3                                                 M28.5917
12225 SMAT(N1,N2) = DUMMAT(N3,N2)                                       M28.5918
12230 CONTINUE                                                          M28.5919
      DO 12233 N2 = 1,3                                                 M28.5920
      N3 = N2 - 1                                                       M28.5921
      DO 12232 N1 = 1,3                                                 M28.5922
      IF (N2 .NE. 1) GO TO 12231                                        M28.5923
      DUMVEC(N1) = SMAT(N1,N2)                                          M28.5924
      GO TO 12232                                                       M28.5925
12231 SMAT(N1,N3) = SMAT(N1,N2)                                         M28.5926
      IF (N2 .NE. 3) GO TO 12232                                        M28.5927
      SMAT(N1,N2) = DUMVEC(N1)                                          M28.5928
12232 CONTINUE                                                          M28.5929
12233 CONTINUE                                                          M28.5930
      DO 12240 N1 = 1,3                                                 M28.5931
      N2 = N1 + 3                                                       M28.5932
12240 DUMVEC(N1) = VINP(N2)                                             M28.5933
      CALL MATVEC(SMAT,DUMVEC)                                          M28.5934
      DO 12250 N1 = 1,3                                                 M28.5935
      N2 = N1 + 3                                                       M28.5936
12250 VINP(N2) = DUMVEC(N1)                                             M28.5937
C. TRANSFORM THE UM-ROTOR(POWER) VOLTAGES:                              M33.1981
12300 IF (LOOPSS(8) .NE. 0) GO TO 12304                                 M36.1645
      DO 12301 N1 = 1,3                                                 M36.1646
12301 DUMVEC(N1) = VINP(N1)                                             M28.5940
      CALL MATVEC(PTHETA,DUMVEC)                                        M28.5941
      DO 12302 N1 = 1,3                                                 M28.5942
12302 VINP(N1) = DUMVEC(N1)                                             M28.5943
C. DETERMINATION OF INITIAL CURRENTS IN UM DOMAIN :                     M28.5944
12304 IF (ISTART .NE. 0) GO TO 12400                                    M36.1647
      IF (INITUM .NE. 0) GO TO 12330                                    M31.3574
12310 DO 12320 N1 = 1,3                                                 M31.3575
      N2 = N1 + KCL - 1                                                 M28.5947
12320 DUMVEC(N1) = HIST(N2)                                             M28.5948
      CALL MATVEC(PTHETA,DUMVEC)                                        M28.5949
      DO 12325 N1 = 1,3                                                 M28.5950
12325 UMCUR(N1) = DUMVEC(N1)                                            M28.5951
      IF (CON(6) .NE. 1.0) GO TO 12335                                  M28.5952
      DUMVEC(1) = - HIST(KCL+5)                                         M28.5953
      DUMVEC(2) = - HIST(KCL+3)                                         M28.5954
      DUMVEC(3) = - HIST(KCL+4)                                         M28.5955
      CALL MATVEC(DUMMAT,DUMVEC)                                        M28.5956
      UMCUR(4) = DUMVEC(2)                                              M28.5957
      UMCUR(5) = DUMVEC(3)                                              M28.5958
      UMCUR(6) = DUMVEC(1)                                              M28.5959
      GO TO 12350                                                       M28.5960
12330 IF (INITUM .EQ. 0) GO TO 12350                                    M31.3576
      DO 12332 N1 = 1,3                                                 M31.3577
      N2 = N1 + KCL - 1                                                 M33.1982
12332 UMCUR(N1) = + HIST(N2)                                            M31.3579
      IF (JTYPE(JM) .NE. 4) GO TO 12335                                 M31.3580
      DO 12334 N1 = 4,6                                                 M31.3581
      N2 = N1 + KCL - 1                                                 M31.3582
12334 UMCUR(N1) = - HIST(N2)                                            M31.3583
      GO TO 12350                                                       M31.3584
12335 DO 12340 N2 = KCLD1,KCLE                                          M28.5961
      N1 = N2 - KCL + 1                                                 M28.5962
12340 UMCUR(N1) = - HIST(N2)                                            M28.5963
C. CALCULATION OF INITIAL FLXD(JM) AND FLXQ(JM):                        M28.5964
12350 D10 = UMCUR(2)                                                    M28.5965
      D15 = UMCUR(3)                                                    M28.5966
      IF (NCLD(JM) .LT. 1) GO TO 12360                                  M28.5967
      DO 12355 N2 = KCLD1,KCLDE                                         M28.5968
      N1 = N2 - KCL + 1                                                 M30. 846
12355 D10 = D10 + UMCUR(N1)                                             M30. 847
      IF (NCLD(JM) .LE. 1) GO TO 12360                                  M28.5970
      IF (JTYPE(JM) .NE. 12) GO TO 12360                                M28.5971
      D10 = D10 - UMCUR(5)                                              M28.5972
12360 IF (NCLQ(JM) .LT. 1) GO TO 12370                                  M28.5973
      DO 12365 N2 = KCLQ1,KCLQE                                         M28.5974
      N1 = N2 - KCL + 1                                                 M30. 848
12365 D15 = D15 + UMCUR(N1)                                             M30. 849
12370 CURMD = D10                                                       M38.3502
      CURMQ = D15                                                       M38.3503
      IF (JCDSAT(JM) .NE. 5) GO TO 12372                                M38.3504
      CURMT = D10*D10 + D15*D15                                         M30. 851
      IF (CURMT .EQ. 0.0) GO TO 12375                                   M30. 852
      CURMT = SQRTZ(CURMT)                                              M30. 853
      D19 = D10 * FLXDS(JM)/CURMT                                       M30. 854
      D20 = D15 * FLXQS(JM)/CURMT                                       M30. 855
      IF (D19 .LT. 0.0) D19 = - D19                                     M30. 856
      IF (D20 .LT. 0.0) D20 = - D20                                     M30. 857
      IF (REAMDU(JM) .EQ. 0.0) GO TO 12375                              M30. 858
      D18 = REAMDS(JM)/REAMDU(JM)                                       M30. 859
      FLXDA = D19 + D18*(FLXDRR - D19)                                  M30. 860
      FLXQA = D20 + D18*(FLXQRR - D20)                                  M30. 861
12372 IF (JCDSAT(JM) .EQ. 0) GO TO 12375                                M30. 862
      IF (REAMDU(JM) .EQ. 0.0) GO TO 12375                              M28.5977
      D11 = FLXDS(JM)/REAMDU(JM)                                        M28.5978
      D12 = - D11                                                       M28.5979
      IF (D10 .GT. D11) GO TO 12377                                     M28.5980
      IF (D10 .LT. D12) GO TO 12377                                     M28.5981
12375 FLXD(JM) = REAMDU(JM) * D10 + FLXDRR                              M28.5982
      IF (D10 .LT. 0.0) FLXD(JM) = FLXD(JM)-2.*FLXDRR                   M28.5983
      IF (JCDSAT(JM) .NE. 5) GO TO 12376                                M30. 863
      IF (CURMT .EQ. 0.0) GO TO 12385                                   M30. 864
12376 GO TO 12380                                                       M30. 865
12377 IF (D10 .GE. 0.0) FLXD(JM) = FLXDA + REAMDS(JM)*D10               M28.5985
      IF (D10 .LT. 0.0) FLXD(JM) = -FLXDA + REAMDS(JM)*D10              M28.5986
12380 IF (JCQSAT(JM) .EQ. 0) GO TO 12385                                M28.5987
      D16 = FLXQS(JM)/REAMQU(JM)                                        M28.5988
      D17 = - D16                                                       M28.5989
      IF (D15 .GT. D16) GO TO 12387                                     M28.5990
      IF (D15 .LT. D17) GO TO 12387                                     M28.5991
12385 FLXQ(JM) = REAMQU(JM) * D15 + FLXQRR                              M28.5992
      IF (D15 .LT. 0.0) FLXQ(JM) = FLXQ(JM)-2.*FLXQRR                   M28.5993
      GO TO 14200                                                       M28.5994
12387 IF (D15 .GE. 0.0) FLXQ(JM) = FLXQA + REAMQS(JM)*D15               M28.5995
      IF (D15 .LT. 0.0) FLXQ(JM) = -FLXQA + REAMQS(JM)*D15              M28.5996
      GO TO 14200                                                       M28.5997
C. TRANSFORM THE UM-ROTOR(POWER) THEVENIN IMPEDANCES:                   M33.1983
12400 IF (LOOPSS(8) .EQ. 1) GO TO 12465                                 M33.1984
      DO 12420 N1 = 1,3                                                 M33.1985
      DO 12410 N2 = 1,3                                                 M28.6000
12410 DUMMAT(N1,N2) = PTHETA(N2,N1)                                     M28.6001
12420 CONTINUE                                                          M28.6002
      CALL MATMUL(ZTHEVR,DUMMAT)                                        M28.6003
      DO 12440 N1 = 1,3                                                 M28.6004
      DO 12430 N2 = 1,3                                                 M28.6005
12430 DUMMAT(N1,N2) = PTHETA(N1,N2)                                     M28.6006
12440 CONTINUE                                                          M28.6007
      CALL MATMUL(DUMMAT,ZTHEVR)                                        M28.6008
      DO 12460 N1 = 1,3                                                 M28.6009
      DO 12450 N2 = 1,3                                                 M28.6010
12450 ZTHEVR(N1,N2) = DUMMAT(N1,N2)                                     M28.6011
12460 CONTINUE                                                          M28.6012
C. TRANSFORM THE UM-STATOR(EXCIT) IN CASE OF ABC-EXCITATION :           M33.1986
12465 IF (CON(6) .NE. 1.0) GO TO 12600                                  M33.1987
      IF (NITROM .GT. 0) GO TO 12600                                    M28.6015
      DO 12470 N1 = 1,3                                                 M28.6016
      DO 12470 N2 = 1,3                                                 M28.6017
12470 DUMMAT(N1,N2) = SMAT(N2,N1)                                       M28.6018
      CALL MATMUL(ZTHS3,DUMMAT)                                         M28.6019
      CALL MATMUL(SMAT,ZTHS3)                                           M28.6020
C. NOTE: FROM NOW ON SMAT IS IN BACK-TRANSFORMATION MODE :              M28.6021
C.                             (ONLY FOR T > 0.0)                       M28.6022
      DO 12480 N1 = 1,3                                                 M28.6023
      DO 12480 N2 = 1,3                                                 M28.6024
      ZTHS3(N1,N2) = SMAT(N1,N2)                                        M28.6025
12480 SMAT(N1,N2) = DUMMAT(N1,N2)                                       M28.6026
C. CALCULATION OF FLXD AND FLXQ:                                        M28.6027
C. CALCULATION OF JACOBIAN:                                             M28.6028
12600 FLXD(JM) = 0.0                                                    M28.6029
      FLXQ(JM) = 0.0                                                    M28.6030
      JACOB = 1                                                         M28.6031
      GO TO 13000                                                       M28.6032
12601 FJD = UMCUR(2)                                                    M28.6033
      FJQ = UMCUR(3)                                                    M28.6034
      IF (LOOPSS(8) .EQ. 1) FJD = 0.0                                   M33.1988
      IF (LOOPSS(8) .EQ. 1) FJQ = 0.0                                   M33.1989
      IF (KCLD1 .GT. KCLDE) GO TO 12603                                 M28.6035
      DO 12602 N1 = KCLD1,KCLDE                                         M28.6036
      N2 = N1 - KCL + 1                                                 M28.6037
12602 FJD = FJD + UMCUR(N2)                                             M28.6038
      IF (JTYPE(JM) .NE. 12) GO TO 12603                                M28.6039
      FJD = FJD - UMCUR(5)                                              M28.6040
12603 IF (KCLQ1 .GT. KCLQE) GO TO 12605                                 M28.6041
      DO 12604 N1 = KCLQ1,KCLQE                                         M28.6042
      N2 = N1 - KCL + 1                                                 M28.6043
12604 FJQ = FJQ + UMCUR(N2)                                             M28.6044
12605 FLXD(JM) = 1.0                                                    M28.6045
      FLXQ(JM) = 0.0                                                    M28.6046
      JACOB = 2                                                         M28.6047
      GO TO 13000                                                       M28.6048
12606 FJDD = UMCUR(2)                                                   M28.6049
      FJQD = UMCUR(3)                                                   M28.6050
      IF (LOOPSS(8) .EQ. 1) FJDD = 0.0                                  M33.1990
      IF (LOOPSS(8) .EQ. 1) FJQD = 0.0                                  M33.1991
      IF (KCLD1 .GT. KCLDE) GO TO 12608                                 M28.6051
      DO 12607 N1 = KCLD1,KCLDE                                         M28.6052
      N2 = N1 - KCL + 1                                                 M28.6053
12607 FJDD = FJDD + UMCUR(N2)                                           M28.6054
      IF (JTYPE(JM) .NE. 12) GO TO 12608                                M28.6055
      FJDD = FJDD - UMCUR(5)                                            M28.6056
12608 FJDD = FJDD - FJD                                                 M28.6057
      IF (KCLQ1 .GT. KCLQE) GO TO 12610                                 M28.6058
      DO 12609 N1 = KCLQ1,KCLQE                                         M28.6059
      N2 = N1 - KCL + 1                                                 M28.6060
12609 FJQD = FJQD + UMCUR(N2)                                           M28.6061
12610 FJQD = FJQD - FJQ                                                 M28.6062
      FLXD(JM) = 0.0                                                    M28.6063
      FLXQ(JM) = 1.0                                                    M28.6064
      JACOB = 3                                                         M28.6065
      GO TO 13000                                                       M28.6066
12611 FJDQ = UMCUR(2)                                                   M28.6067
      FJQQ = UMCUR(3)                                                   M28.6068
      IF (LOOPSS(8) .EQ. 1) FJDQ = 0.0                                  M33.1992
      IF (LOOPSS(8) .EQ. 1) FJQQ = 0.0                                  M33.1993
      IF (KCLD1 .GT. KCLDE) GO TO 12613                                 M28.6069
      DO 12612 N1 = KCLD1,KCLDE                                         M28.6070
      N2 = N1 - KCL + 1                                                 M30. 866
12612 FJDQ = FJDQ + UMCUR(N2)                                           M30. 867
      IF (JTYPE(JM) .NE. 12) GO TO 12613                                M28.6072
      FJDQ = FJDQ - UMCUR(5)                                            M28.6073
12613 FJDQ = FJDQ - FJD                                                 M28.6074
      IF (KCLQ1 .GT. KCLQE) GO TO 12615                                 M28.6075
      DO 12614 N1 = KCLQ1,KCLQE                                         M28.6076
      N2 = N1 - KCL + 1                                                 M28.6077
12614 FJQQ = FJQQ + UMCUR(N2)                                           M28.6078
12615 FJQQ = FJQQ - FJQ                                                 M28.6079
      IF (LOOPSS(8) .EQ. 1) FJD = FJD + UMCUR(2)                        M33.1994
      IF (LOOPSS(8) .EQ. 1) FJQ = FJQ + UMCUR(3)                        M33.1995
      JACOB = 4                                                         M28.6080
C. START FLXDS AND FLXQS CHECK:                                         M28.6081
C    CURMD AND CURMQ ARE REPEATEDLY CALCULATED TO FIND IF               M38.3505
C       REAMDU OR REAMDS AND REAMQU OR REAMQS NEEDS TO BE USED.         M38.3506
C    N1 = 1: FIRST TIME AND UNSATURATED TO FIND SFLXD AND SFLXQ         M38.3507
C    N1 = 5: COMMAND TO REPEAT CALCULATIONS IN SATURATED CONDITION      M38.3508
C            IF SATURATION OPTION IS REQUESTED                          M38.3509
C    N1 = 2: COMMAND TO EXIT CALCULATION                                M38.3510
C    N1 = 3: COMMAND TO REPEAT WITH Q-AXIS SATURATION                   M38.3511
C    N1 = 4: COMMAND TO REPEAT WITH D-AXIS SATURATION                   M38.3512
C    THE CONSTANT N2 IS USED FOR TOTAL SATURATION OPTION PURPOSE        M38.3513
12700 N1 = 1                                                            M28.6082
      N2 = 1                                                            M30. 868
      N5 = 1 + JCDSAT(JM) + 2*JCQSAT(JM)                                M28.6083
C  FIRST FIND SFLXD AND SFLXQ WITH NO SAT. AND RES. MAGN.               M30. 869
      D15 = 0.0                                                         M30. 870
      SFLXD = 0.0                                                       M30. 871
      SFLXQ = 0.0                                                       M30. 872
      GO TO 12702                                                       M30. 873
12701 IF ( N5  .EQ.  2 )   GO TO 12704                                  M30. 874
      IF ( N5  .EQ.  3 )   GO TO 12706                                  M30. 875
      IF ( N5  .EQ.  4 )   GO TO 12708                                  M30. 876
      IF (N5 .GE. 5) GO TO 12708                                        M30. 877
12702 D12 = 0.0                                                         M28.6085
      D13 = 0.0                                                         M28.6086
      GO TO 12710                                                       M28.6087
12704 D12 = 1.0                                                         M28.6088
      D13 = 0.0                                                         M28.6089
      GO TO 12710                                                       M28.6090
12706 D12 = 0.0                                                         M28.6091
      D13 = 1.0                                                         M28.6092
      GO TO 12710                                                       M28.6093
12708 D12 = 1.0                                                         M28.6094
      D13 = 1.0                                                         M28.6095
12710 D1 = SFLXD * ((1.0-D12)*D15*FLXDRR + D12*FLXDA)                   M30. 878
      D2 = SFLXQ * ((1.0-D13)*D15*FLXQRR + D13*FLXQA)                   M30. 879
      D3 = FJDD*D1 + FJDQ*D2 + FJD                                      M28.6098
      D4 = FJQQ*D2 + FJQD*D1 + FJQ                                      M28.6099
      D5 = REAMDU(JM)*(1.0-D12) + REAMDS(JM)*D12                        M28.6100
      D6 = REAMQU(JM)*(1.0-D13) + REAMQS(JM)*D13                        M28.6101
      D7 = 1.0 - FJDD*D5                                                M28.6102
      D8 = 1.0 - FJQQ*D6                                                M28.6103
      D9 = - FJDQ * D6                                                  M28.6104
      D10 = - FJQD * D5                                                 M28.6105
      D11 = D7*D8 - D9*D10                                              M28.6106
      CURMD = (D8*D3 - D9*D4)/D11                                       M28.6107
      CURMQ = (D7*D4 - D10*D3)/D11                                      M28.6108
      D1 = CURMD * CURMD                                                M30. 880
      D1 = D1 + CURMQ * CURMQ                                           M30. 881
      CURMT = SQRTZ(D1)                                                 M30. 882
      D19 = SFLXD * CURMD                                               M30. 883
      D20 = SFLXQ * CURMQ                                               M30. 884
C  DELETION OF 6 LINES TO THE M28 VERSION ON 28 DEC 1980,               M30. 885
C  THE DELETED LINE NUMBERS : M28.6109 TO M28.6114                      M30. 886
12722 IF ( N1  .GE.  5 )   GO TO 12725                                  M30. 887
      GO TO (12800,12900,12780,12790), N1                               M30. 888
12725 IF ( N5  .GE.  5 )   GO TO 12750                                  M30. 889
      GO TO (12900,12730,12740,12750,12750), N5                         M30. 890
12730 IF (D19 .GT. D17) GO TO 12900                                     M30. 891
      N1 = 2                                                            M30. 892
      N2 = 0                                                            M30. 893
      GO TO 12702                                                       M28.6122
12740 IF (D20 .GT. D18) GO TO 12900                                     M30. 894
      N1 = 2                                                            M30. 895
      N2 = 0                                                            M30. 896
      GO TO 12702                                                       M28.6125
12750 IF (N5 .NE. 5) GO TO 12755                                        M30. 897
      N2 = 0                                                            M30. 898
      IF (D19 .GT. D17) GO TO 12900                                     M30. 899
      IF (D20 .GT. D18) GO TO 12900                                     M30. 900
      N1 = 2                                                            M30. 901
      GO TO 12702                                                       M30. 902
12755 IF (D19 .LE. D17 .AND. D20 .LE. D18) GO TO 12790                  M38.3514
      IF (D19 .GT. D17 .AND. D20 .GT. D18) GO TO 12900                  M38.3515
      IF (D19 .GT. D17) GO TO 12770                                             
      GO TO 12760                                                               
12760 N1 = 3                                                            M28.6129
      N2 = 0                                                            M30. 906
      GO TO 12706                                                       M28.6130
12770 N1 = 4                                                            M28.6131
      N2 = 0                                                            M30. 907
      GO TO 12704                                                       M28.6132
12780 IF (D20 .GT. D18) GO TO 12900                                     M30. 908
      N1 = 2                                                            M30. 909
      N2 = 0                                                            M30. 910
      GO TO 12702                                                       M28.6135
12790 IF (D19 .GT. D17) GO TO 12900                                     M30. 911
      N1 = 2                                                            M30. 912
      N2 = 0                                                            M30. 913
      GO TO 12702                                                       M28.6138
12800 IF (N5 .EQ. 1) GO TO 12900                                        M30. 914
      N1 = 5                                                            M30. 915
      D15 = 1.0                                                         M30. 916
      SFLXD = 1.0                                                       M30. 917
      SFLXQ = 1.0                                                       M30. 918
      IF (CURMD .LT. 0.0) SFLXD = - 1.0                                 M30. 919
      IF (CURMQ .LT. 0.0) SFLXQ = - 1.0                                 M30. 920
      IF (REAMDU(JM) .EQ. 0.0) GO TO 12802                              M30. 921
      D17 = (FLXDS(JM) - FLXDRR)/REAMDU(JM)                             M30. 922
      D18 = (FLXQS(JM) - FLXQRR)/REAMQU(JM)                             M30. 923
12802 IF (JCDSAT(JM) .EQ. 5) GO TO 12804                                M30. 924
      GO TO 12701                                                       M30. 925
C   CALCULATION OF FLXDA AND FLXQA IF TOTAL SATURATION :                M35.1649
12804 IF (CURMT .NE. 0.0) GO TO 12806                                   M30. 926
      D1 = FLXDS(JM)                                                    M30. 927
      D2 = D1                                                           M30. 928
      GO TO 12808                                                       M30. 929
12806 D19 = SFLXD * CURMD                                               M30. 930
      D20 = SFLXQ * CURMQ                                               M30. 931
      D17 = D17 * D19/CURMT                                             M30. 932
      D18 = D18 * D20/CURMT                                             M30. 933
      D1 = D19 * FLXDS(JM)/CURMT                                        M30. 934
      D2 = D20 * FLXDS(JM)/CURMT                                        M30. 935
12808 D3 = REAMDS(JM)/REAMDU(JM)                                        M30. 936
      FLXDA = D1 + D3*(FLXDRR - D1)                                     M30. 937
      FLXQA = D2 + D3*(FLXQRR - D2)                                     M30. 938
      N5 = 5                                                            M30. 939
      GO TO 12701                                                       M30. 940
12900 FLXD(JM) = REAMDU(JM) * (1.0-D12) * CURMD                         M28.6139
      FLXQ(JM) = REAMQU(JM) * (1.0-D13) * CURMQ                         M28.6140
      IF (N5 .EQ. 1) GO TO 13000                                        M28.6141
      FLXD(JM) = FLXD(JM) + REAMDS(JM)*D12*CURMD                        M28.6142
      FLXD(JM) = FLXD(JM) + SFLXD*(1.0-D12)*FLXDRR                      M30. 941
      FLXD(JM) = FLXD(JM) + D12*SFLXD*FLXDA                             M28.6144
      FLXQ(JM) = FLXQ(JM) + REAMQS(JM)*D13*CURMQ                        M28.6145
      FLXQ(JM) = FLXQ(JM) + SFLXQ*(1.0-D13)*FLXQRR                      M30. 942
      FLXQ(JM) = FLXQ(JM) + D13*SFLXQ*FLXQA                             M28.6147
      IF (N2 .NE. 0) GO TO 13000                                        M30. 943
      IF (D12 .NE. 0.0) GO TO 12910                                     M30. 944
      D1 = FLXD(JM) * FLXD(JM)                                          M30. 945
      D2 = FLXDRR * FLXDRR                                              M30. 946
      IF (D1 .LE. D2) FLXD(JM) = SFLXD * FLXDRR                         M30. 947
12910 IF (D13 .NE. 0.0) GO TO 13000                                     M30. 948
      D1 = FLXQ(JM) * FLXQ(JM)                                          M30. 949
      D2 = FLXQRR * FLXQRR                                              M30. 950
      IF (D1 .LE. D2) FLXQ(JM) = SFLXQ * FLXQRR                         M30. 951
C. UMCUR-ROTOR(POWER) EQUATIONS:                                        M33.1996
C. FORMATION OF ACURR                                                   M28.6149
13000 IF (LOOPSS(8) .EQ. 0) GO TO 13008                                 M33.1997
      IF (JACOB .GT. 1) GO TO 13100                                     M33.1998
      UMCUR(1) = CURUM1                                                 M33.1999
      UMCUR(2) = CURUM2                                                 M33.2000
      UMCUR(3) = CURUM3                                                 M33.2001
      CALL MATVEC(PTHETA,UMCUR)                                         M33.2002
      IF (GPAR(KCL) .EQ. 0.0) UMCUR(1) = 0.0                            M33.2003
      GO TO 13100                                                       M33.2004
13008 DO 13020 N1 = 1,3                                                 M33.2005
      DO 13010 N2 = 1,3                                                 M28.6151
13010 DUMMAT(N1,N2) = 0.0                                               M28.6152
13020 CONTINUE                                                          M28.6153
      DUMMAT(1,1) = GPAR(KCL) * FPAR(KCL)                               M28.6154
      DUMMAT(2,2) = GPAR(KCL+1) * FPAR(KCL+1)                           M28.6155
      DUMMAT(3,3) = GPAR(KCL+2) * FPAR(KCL+2)                           M28.6156
      DO 13025 N1 = 2,3                                                 M28.6157
13025 DUMVEC(N1) = DUMMAT(N1,N1)                                        M28.6158
      CALL MATMUL(DUMMAT,ZTHEVR)                                        M28.6159
      D1 = ZTHEV + (CON(7)+CON(8)) *RD2                                 M28.6160
      DUMMAT(3,3) = DUMMAT(3,3)+DUMVEC(3)*D1+1.0                        M28.6161
      DUMMAT(2,2) = DUMMAT(2,2) + 1.0                                   M28.6162
      DUMMAT(1,1) = DUMMAT(1,1) + 1.0                                   M28.6163
      D1 = (1.0-CON(4))*OMEGE*DUMVEC(2)*REACL(KCL+2)                    M28.6164
      DUMMAT(2,3) = DUMMAT(2,3) + D1                                    M28.6165
      D1 = (1.0-CON(4))*OMEGE*DUMVEC(3)*REACL(KCL+1)                    M28.6166
      DUMMAT(3,2) = DUMMAT(3,2) - D1                                    M28.6167
C.FORMATION OF UMCUR-ROTOR(POWER):                                      M33.2006
      DO 13030 N1 =1,3                                                  M28.6169
      N2 = KCL - 1 + N1                                                 M28.6170
13030 DUMVEC(N1) = - GPAR(N2)*FPAR(N2)*VINP(N1) + HIST(N2)              M28.6171
      D2 = + GPAR(KCL+1) * FPAR(KCL+1)                                  M28.6172
      D3 = + GPAR(KCL+2) * FPAR(KCL+2)                                  M28.6173
      D5 = - D2 * (FLXD(JM)/SELTA2 + FLXQ(JM)*OMEGE)                    M28.6174
      DUMVEC(2) = DUMVEC(2) + D5                                        M28.6175
      D5 = D3 * (FLXD(JM)*OMEGE - FLXQ(JM)/SELTA2)                      M28.6176
      D6 = - D3 * FLXD(JM) * (CON(7)+CON(8))/SELTA2                     M28.6177
      DUMVEC(3) = DUMVEC(3) + D5 + D6                                   M28.6178
C. FINAL RESULT FOR UMCUR-ROTOR(POWER):                                 M33.2007
      CALL LINEQS(DUMMAT,DUMVEC)                                        M28.6180
      DO 13050 N1 = 1,3                                                 M28.6181
13050 UMCUR(N1) = DUMVEC(N1)                                            M28.6182
C.FORMATION OF UMCUR-STATOR(EXCIT) :                                    M33.2008
13100 DO 13110 N1 = KCLD1,KCLQE                                         M28.6184
      N2 = N1 - KCL + 1                                                 M28.6185
13110 UMCUR(N2) = - GPAR(N1)*FPAR(N1)*VINP(N2) + HIST(N1)               M28.6186
      IF (NCLD(JM) .EQ. 0) GO TO 13150                                  M28.6187
      DO 13120 N1 = KCLD1,KCLDE                                         M28.6188
      N2 = N1 - KCL + 1                                                 M28.6189
      D1 = - FLXD(JM)*GPAR(N1)*FPAR(N1)/SELTA2                          M28.6190
13120 UMCUR(N2) = UMCUR(N2) + D1                                        M28.6191
      D4 = GPAR(KCL+3) * FPAR(KCL+3)                                    M28.6192
      UMCUR(4) = UMCUR(4) + FLXD(JM)*CON(8)*D4/SELTA2                   M28.6193
13150 IF (NCLQ(JM) .EQ. 0) GO TO 13200                                  M28.6194
      DO 13160 N1 = KCLQ1,KCLQE                                         M28.6195
      N2 = N1 - KCL + 1                                                 M28.6196
      D1 = - FLXQ(JM)*GPAR(N1)*FPAR(N1)/SELTA2                          M28.6197
13160 UMCUR(N2) = UMCUR(N2) + D1                                        M28.6198
C. UMCUR-STATOR(EXCIT)  FOR UM=TYPE 4(C3IM):                            M33.2009
13200 IF (CON(6) .EQ. 0.0) GO TO 13210                                  M28.6200
      N1 =  KCLE - KCL +1                                               M28.6201
      UMCUR(N1) = HIST(KCLE) - GPAR(KCLE)*FPAR(KCLE)*VINP(N1)           M28.6202
C. IMPLEMENTING INFLUENCE OF THEV IMPEDANCES :                          M28.6203
13210 D1 = CON(7) + CON(8)                                              M28.6204
      IF (D1 .LT. 1.0) GO TO 13212                                      M28.6205
      ZTHS3(1,1) = D1 * ZTHEV + CON(8) * RD2                            M28.6206
      DUMMAT(1,1) = 1.0 + GPAR(KCLD1)*FPAR(KCLD1)*ZTHS3(1,1)            M28.6207
      GO TO 13230                                                       M28.6208
13212 N15 = KCLE                                                        M28.6209
      IF (NCL .GT. 6) N15 = KCL + 5                                     M28.6210
      DO 13215 N1 = 1,3                                                 M28.6211
      DO 13215 N2 = 1,3                                                 M28.6212
13215 DUMMAT(N1,N2) = 0.0                                               M28.6213
      DO 13220 N1 = KCLD1,N15                                           M28.6214
      N2 = N1 - KCL - 2                                                 M28.6215
13220 DUMMAT(N2,N2) = GPAR(N1) * FPAR(N1)                               M28.6216
      CALL MATMUL(DUMMAT,ZTHS3)                                         M28.6217
      DO 13225 N1 = 1,3                                                 M28.6218
13225 DUMMAT(N1,N1) = DUMMAT(N1,N1) + 1.0                               M28.6219
      N16 = N15 - KCL - 2                                               M28.6220
      IF ( N16  .EQ.  2 )   GO TO 13235                                 M30. 952
      IF ( N16  .EQ.  3 )   GO TO 13240                                 M30. 953
13230 UMCUR(4) = UMCUR(4)/DUMMAT(1,1)                                   M28.6222
      GO TO 13260                                                       M28.6223
13235 D1 = DUMMAT(1,1) * DUMMAT(2,2)                                    M28.6224
      D1 = D1 - DUMMAT(1,2) * DUMMAT(2,1)                               M28.6225
      D4 = UMCUR(4)                                                     M28.6226
      D5 = UMCUR(5)                                                     M28.6227
      UMCUR(4) = (DUMMAT(2,2)*D4 - DUMMAT(1,2)*D5)/D1                   M28.6228
      UMCUR(5) = (DUMMAT(1,1)*D5 - DUMMAT(2,1)*D4)/D1                   M28.6229
      GO TO 13260                                                       M28.6230
13240 DO 13245 N1 = 1,3                                                 M28.6231
      N2 = N1 + 3                                                       M28.6232
13245 DUMVEC(N1) = UMCUR(N2)                                            M28.6233
      CALL LINEQS(DUMMAT,DUMVEC)                                        M28.6234
      DO 13250 N1 = 1,3                                                 M28.6235
      N2 = N1 + 3                                                       M28.6236
13250 UMCUR(N2) = DUMVEC(N1)                                            M28.6237
13260 IF (NCL .LE. 6) GO TO 13300                                       M28.6238
      N15 = KCLD1 + 3                                                   M28.6239
      DO 13265 N1 = N15,KCLE                                            M28.6240
      N2 = N1 - KCL + 1                                                 M28.6241
      D1 = 1.0 + GPAR(N1) * FPAR(N1) * ZTHEVS(N2)                       M28.6242
13265 UMCUR(N2) = UMCUR(N2)/D1                                          M28.6243
C. UMCUR UPDATE FOR UM = SPECIAL DM-TYPES :                             M28.6244
13300 D1 = CON(7) + CON(8)                                              M28.6245
      IF (D1 .EQ. 0.0) GO TO 14100                                      M28.6246
      D2 = 1.0/(1.0 - BETAQ*BETAD1)                                     M28.6247
      D3 = UMCUR(3)                                                     M28.6248
      D4 = UMCUR(4)                                                     M28.6249
      UMCUR(3) = (D3 + BETAQ*D4) * D2                                   M28.6250
      UMCUR(4) = (BETAD1*D3 + D4) * D2                                  M28.6251
      IF (CON(9) .NE. 0.0) GO TO 14100                                  M28.6252
      UMCUR(5) = D1 * UMCUR(3) - CON(8) * UMCUR(4)                      M28.6253
C. PROGRAM CONTROL FOR CALCULATION OF JACOBIAN:                         M28.6254
14100 IF ( JACOB  .GE.  4 )   GO TO 14200                               M30. 954
      GO TO (12601, 12606, 12611),  JACOB                               M30. 955
C. CALCULATION OF TQGEN:                                                M28.6256
14200 D1 = 3.0*CON(3)+2.0*CON(2)+CON(1)+2.0*CON(10)+CON(4)              M28.6257
      D2 = (REACL(KCL+1)-REACL(KCL+2))*UMCUR(2)*UMCUR(3)                M28.6258
      D3 = UMCUR(2) * FLXQ(JM)                                          M28.6259
      D4 = UMCUR(3) * FLXD(JM)                                          M28.6260
      TQGEN = (D2 - D3 + D4) * RPPAIR                                   M32.6442
      IF (INPU .EQ. 1) TQGEN = TQGEN/D1                                 M32.6443
C. SOLVE OMEGM FROM TORQUE EQUATION:                                    M28.6264
14300 IF (JTMTAC(JM) .EQ. 0) GO TO 14310                                M28.6265
      TDAMP = DCOEF(JM) * OMEGM(JM)                                     M28.6266
      TCIN = (TUMTAC - TQGEN - TDAMP) * SELTA2/ROTMOM(JM)               M28.6267
      IF (ISTART .EQ. 0) GO TO 14510                                    M31.3585
      OMEGM(JM) = HISTOM(JM) + TCIN                                     M28.6269
      GO TO 14400                                                       M28.6270
14310 IF (ISTART .EQ. 0) GO TO 14510                                    M31.3586
      OMEGM(JM) = OMTHEV - ZTHEVM * TQGEN                               M28.6272
      IF (NSHARE .EQ. 0) GO TO 14400                                    M39.3112
      OMEGM(JM) = OMEGM(JM) - ZTHEM2*TQGEN2 - ZTHEM3*TQGEN3             M39.3113
C. CHECK CRITERIUM FOR OMEGM-ITERATION:                                 M28.6273
14400 D1 = OMEGM(JM) - OMEGMP                                           M28.6274
      D2 = + EPSOM(JM)                                                  M28.6275
      D3 = - D2                                                         M28.6276
      IF (D1 .GT. D2) GO TO 14410                                       M28.6277
      IF (D1 .GE. D3) GO TO 14500                                       M28.6278
14410 OMEGMP = OMEGM(JM)                                                M28.6279
      IF (NITROM .LE. 50) GO TO 14416                                   M28.6280
      WRITE(LUNIT6,14414)                                               M28.6281
14414 FORMAT ( 42H0ROTOR SPEED ITERATION FAILS  IN 50 STEPS.  )         M28.6282
      WRITE(LUNIT6,14415) JM,T                                          M28.6283
14415 FORMAT(25H FAILING MACHINE NUMBER :,I3,3X,6HTIME =,E14.5)         M28.6284
      INPU = 50                                                         M28.6285
      GO TO 79                                                          M32.6444
14416 GO TO 12020                                                       M28.6287
C. CALCULATION OF TORQUE ANGLE FOR SM:                                  M28.6288
14500 OMEGM(JM) = OMEGMP                                                M31.3587
14510 IF (JTYPE(JM) .GT. 2) GO TO 14600                                 M31.3588
      TANGLE = THETAE - (TAU + PI/2.0)                                  M28.6290
C. OUTPUT VECTORS FOR 3-AND 2-PHASE ROTORS(POWER COILS):                M33.2010
14600 NOUTST = NXOUT + 1                                                M28.6292
C. OUTPUT VECTOR FOR TORQUE:                                            M28.6293
      IF (JTQOUT(JM) .EQ. 0) GO TO 14602                                M28.6294
      NXOUT = NXOUT + 1                                                 M28.6295
      UMOUTP(NXOUT) = TQGEN                                             M28.6296
      IF (JTQOUT(JM) .LT. 2) GO TO 14602                                M38.3518
      NXOUT = NXOUT + 1                                                 M38.3519
      UMOUTP(NXOUT) = FLXD(JM)                                          M38.3520
      IF (JTQOUT(JM) .NE. 3) GO TO 14602                                M38.3521
      NXOUT = NXOUT + 1                                                 M38.3522
      UMOUTP(NXOUT) = CURMD                                             M38.3523
14602 IF (JOMOUT(JM) .EQ. 0) GO TO 14603                                M28.6298
      NXOUT = NXOUT + 1                                                 M28.6299
      UMOUTP(NXOUT) = OMEGM(JM)                                         M28.6300
      IF (JOMOUT(JM) .LT. 2) GO TO 14603                                M38.3524
      NXOUT = NXOUT + 1                                                 M38.3525
      UMOUTP(NXOUT) = FLXQ(JM)                                          M38.3526
      IF (JOMOUT(JM) .NE. 3) GO TO 14603                                M38.3527
      NXOUT = NXOUT + 1                                                 M38.3528
      UMOUTP(NXOUT) = CURMQ                                             M38.3529
14603 IF (JTHOUT(JM) .EQ. 0) GO TO 14606                                M28.6302
      NXOUT = NXOUT + 1                                                 M28.6303
      IF (JTYPE(JM) .GT. 2) GO TO 14604                                 M28.6304
      UMOUTP(NXOUT) = TANGLE                                            M28.6305
      IF (NTYP59 .NE. 1) GO TO 14606                                    M38.3530
      IF (JTHOUT(JM) .EQ. 1) UMOUTP(NXOUT)=TANGLE*360.0/TWOPI+90.0      M38.3531
      IF (JTHOUT(JM) .EQ. 3) UMOUTP(NXOUT) = - CREST(KCONEX)            M38.3532
      GO TO 14606                                                       M28.6306
14604 UMOUTP(NXOUT) = THETAM(JM)                                        M28.6307
C. OUTPUT VECTOR FOR UM-ROTOR(POWER):                                   M33.2011
14606 IF (LOOPSS(8) .EQ. 1  .AND.  ISTART .NE. 0) GO TO 14616           M33.2012
      DO 14607 N1 = 1,3                                                 M33.2013
14607 DUMVEC(N1) = UMCUR(N1)                                            M28.6310
14609 DO 14615 N1 = 1,3                                                 M33.2014
      DO 14610 N2 = 1,3                                                 M28.6312
14610 DUMMAT(N1,N2) = PTHETA(N2,N1)                                     M28.6313
14615 CONTINUE                                                          M28.6314
      CALL MATVEC(DUMMAT,DUMVEC)                                        M28.6315
      IF (NCURPR .EQ. 1) GO TO 18100                                    M33.2015
14616 IF (LOOPSS(8) .EQ. 0) GO TO 14619                                 M33.2016
      IF (ISTART .EQ. 0) GO TO 14619                                    M33.2017
14618 DUMVEC(1) = CURUM1                                                M33.2018
      DUMVEC(2) = CURUM2                                                M33.2019
      DUMVEC(3) = CURUM3                                                M33.2020
14619 IF (CON(4) .EQ. 0.0) GO TO 14620                                  M33.2021
      DUMVEC(3) = UMCUR(3) - (CON(7)+CON(8))*UMCUR(4)                   M28.6317
14620 DO 14626 N1 = 1,3                                                 M36.1648
      N2 = KCL - 1 + N1                                                 M28.6319
      IF (LOOPSS(8) .EQ. 1) GO TO 14622                                 M33.2022
      IF (NODVO1(N2) .EQ. NODVO2(N2)) GO TO 14622                       M33.2023
      CURSUB(NCLCOM) = - DUMVEC(N1)                                     M28.6321
      NCLCOM = NCLCOM + 1                                               M28.6322
      N10 = NODVO1(N2)                                                  M37.6087
      IF (N10 .GT. 1) F(N10) = F(N10) + DUMVEC(N1)                      M37.6088
      N10 = NODVO2(N2)                                                  M37.6089
      IF (N10 .GT. 1) F(N10) = F(N10) - DUMVEC(N1)                      M37.6090
14622 IF (JCLOUT(N2) .EQ. 0) GO TO 14625                                M33.2024
      NXOUT = NXOUT + 1                                                 M28.6324
      UMOUTP(NXOUT) = DUMVEC(N1)                                        M28.6325
14624 IF (JCLOUT(N2) .EQ. 2) UMOUTP(NXOUT) = UMCUR(N1)                  M33.2025
14625 IF ( IPRSUP  .GE.  1 )                                            M36.1649
     1 WRITE (LUNIT6, 6589)  N1,N2,NXOUT, JCLOUT(N2), UMCUR(N1),        M36.1650
     2                       UMOUTP(NXOUT)                              M36.1651
 6589 FORMAT ( 37H N1, N2, NXOUT, JCLOUT(N2), UMCUR(N1),                M36.1652
     1         16H UMOUTP(NXOUT) =, 4I8,2E14.5)                         M36.1653
14626 CONTINUE                                                          M36.1654
C. OUTPUT VECTOR FOR STATOR(EXCIT) IF UM = 3-PHASE IM:                  M33.2026
      IF (CON(6) .NE. 1.0) GO TO 14660                                  M28.6329
      IF (ISTART .NE. 0) GO TO 14629                                    M28.6330
      DO 14628 N1 = 1,3                                                 M28.6331
      DO 14628 N2 = 1,3                                                 M28.6332
14628 DUMMAT(N1,N2) = SMAT(N2,N1)                                       M28.6333
14629 DO 14630 N1 = 1,3                                                 M28.6334
14630 DUMVEC(N1) = UMCUR(N1+3)                                          M28.6335
      IF (ISTART .NE. 0) GO TO 14631                                    M28.6336
      CALL MATVEC(DUMMAT,DUMVEC)                                        M28.6337
      GO TO 14632                                                       M28.6338
14631 CALL MATVEC(SMAT,DUMVEC)                                          M28.6339
14632 DO 14655 N1 = 1,3                                                 M28.6340
      GO TO (14635,14640,14645),N1                                      M28.6341
14635 N2 = 4                                                            M28.6342
      N3 = 1                                                            M28.6343
      GO TO 14650                                                       M28.6344
14640 N2 = 5                                                            M28.6345
      N3 = 2                                                            M28.6346
      GO TO 14650                                                       M28.6347
14645 N2 = 6                                                            M28.6348
      N3 = 3                                                            M28.6349
14650 N4 = KCL - 1 + N2                                                 M28.6350
      IF (NODVO1(N4) .EQ. NODVO2(N4)) GO TO 14652                       M28.6351
      CURSUB(NCLCOM) = DUMVEC(N3)                                       M28.6352
      NCLCOM = NCLCOM + 1                                               M28.6353
      N10 = NODVO1(N4)                                                  M37.6091
      IF (N10 .GT. 1) F(N10) = F(N10) - DUMVEC(N3)                      M37.6092
      N10 = NODVO2(N4)                                                  M37.6093
      IF (N10 .GT. 1) F(N10) = F(N10) + DUMVEC(N3)                      M37.6094
14652 IF (JCLOUT(N4) .EQ. 0) GO TO 14655                                M28.6354
      NXOUT = NXOUT + 1                                                 M28.6355
      UMOUTP(NXOUT) = - DUMVEC(N3)                                      M28.6356
      IF ( IPRSUP  .GE.  1 )                                            M35.1650
     1 WRITE (LUNIT6, 6589)  N1,N2,NXOUT, JCLOUT(N2), UMCUR(N1),        M36.1655
     2                       UMOUTP(NXOUT)                              M36.1656
14655 CONTINUE                                                          M28.6357
      GO TO 14700                                                       M28.6358
C. OUTPUT VECTORS FOR STATOR(EXCIT) IN GENERAL:                         M33.2027
14660 DO 14665 N1 = 4,NCL                                               M37.6095
      D1 = 1.0                                                          M37.6096
      IF (N1 .EQ. 4 .AND. NTYP59 .EQ. 1) D1 = DCOEF(JM)                 M37.6097
      N2 = KCL - 1 + N1                                                 M28.6361
      IF (NODVO1(N2) .EQ. NODVO2(N2)) GO TO 14664                       M28.6362
      CURSUB(NCLCOM) = UMCUR(N1) * D1                                   M37.6098
      NCLCOM = NCLCOM + 1                                               M28.6364
      N10 = NODVO1(N2)                                                  M37.6099
      IF (N10 .GT. 1) F(N10) = F(N10) - UMCUR(N1)*D1                    M37.6100
      N10 = NODVO2(N2)                                                  M37.6101
      IF (N10 .GT. 1) F(N10) = F(N10) + UMCUR(N1)*D1                    M37.6102
14664 IF (JCLOUT(N2) .EQ. 0) GO TO 14665                                M28.6365
      NXOUT = NXOUT + 1                                                 M28.6366
      UMOUTP(NXOUT) = - UMCUR(N1)                                       M28.6367
      IF (NTYP59 .EQ. 1) UMOUTP(NXOUT) = + UMCUR(N1)*DCOEF(JM)          M36.1657
      IF ( IPRSUP  .GE.  1 )                                            M35.1654
     1 WRITE (LUNIT6, 6589)  N1,N2,NXOUT, JCLOUT(N2), UMCUR(N1),        M36.1658
     2                       UMOUTP(NXOUT)                              M36.1659
14665 CONTINUE                                                          M28.6368
C. OUTPUT VECTOR IF MECH. SYSTEM = ELECTR. NETWORK :                    M28.6369
14700 IF (JTMTAC(JM) .NE. 0) GO TO 15000                                M28.6370
      N10 = NODOM(JM)                                                   M39.3114
      F(N10) = F(N10) - TQGEN                                           M39.3115
      IF (JCLTAC(KCL+2) .LT. 0) GO TO 15000                                     
      CURSUB(NCLCOM) = TQGEN                                            M28.6371
      NCLCOM = NCLCOM + 1                                               M28.6372
C  IN CASE THIS UM IS LOWEST NUMBERED UM IN THE SET OF UM'S             M39.3117
C    SHARING A COMMON MECH NETWORK :                                    M39.3118
      IF (JCLTAC(KCL+2) .LE. 0) GO TO 15000                             M39.3119
      NCLCOM = NCLCOM + 1                                                       
      IF (JCLTAC(KCL+1) .NE. 0) NCLCOM = NCLCOM+1                               
C. E(K) AND VINP ADJUSTMENT FOR REM MAGN + OPEN POWER TERMINALS:        M33.2028
15000 IF (INPU .NE. 2) GO TO 15005                                      M28.6374
      IF (ISTART .NE. 0) GO TO 15005                                    M28.6375
      IF (JTYPE(JM) .EQ. 6) GO TO 15005                                 M28.6376
      IF (JTYPE(JM) .EQ. 7) GO TO 15005                                 M28.6377
      D1 = FLXDRR * FLXDRR + FLXQRR * FLXQRR                            M28.6378
      IF (D1 .EQ. 0.0) GO TO 15005                                      M28.6379
      DO 15001 N1 = 1,3                                                 M28.6380
      IF (UMCUR(N1) .NE. 0.0) GO TO 15005                               M28.6381
15001 CONTINUE                                                          M28.6382
      DUMVEC(1) = 0.0                                                   M28.6383
      DUMVEC(2) = - OMEGE * FLXQ(JM)                                    M28.6384
      DUMVEC(3) = + OMEGE * FLXD(JM)                                    M28.6385
      IF (JTYPE(JM) .GT. 7) DUMVEC(2) = 0.0                             M28.6386
      D1 = VINP(2)*VINP(2) + VINP(3)*VINP(3)                            M28.6387
      D2 = DUMVEC(2)*DUMVEC(2) + DUMVEC(3)*DUMVEC(3)                    M28.6388
      IF (D1 .GT. D2) GO TO 15005                                       M28.6389
      DO 15003 N1 = 1,3                                                 M28.6390
      VINP(N1) = DUMVEC(N1)                                             M28.6391
      DO 15002 N2 = 1,3                                                 M28.6392
15002 DUMMAT(N1,N2) = PTHETA(N2,N1)                                     M28.6393
15003 CONTINUE                                                          M28.6394
      CALL MATVEC(DUMMAT,DUMVEC)                                        M28.6395
      DO 15004 N1 = 1,3                                                 M28.6396
      N2 = KCL - 1 + N1                                                 M28.6397
      N4 = KCL + 1                                                      M28.6398
      N3 = NODVO1(N2)                                                   M28.6399
      IF (NODVO2(KCL) .NE. NODVO2(N4)) N3 = NODVO2(N2)                  M28.6400
      E(N3) = DUMVEC(N1)                                                M28.6401
      IF (NODVO2(KCL) .NE. NODVO2(N4)) E(N3) = - DUMVEC(N1)             M28.6402
15004 CONTINUE                                                          M28.6403
C. CALCULATION OF HISTORY VECTORS:                                      M28.6404
C.DETERMINATION OF ROTOR(POWER)-HIST:                                   M33.2029
15005 IF (LOOPSS(8) .NE. 0) GO TO 15100                                 M36.1660
      DO 15010 N1 = 1,3                                                 M36.1661
      DO 15006 N2 = 1,3                                                 M28.6407
15006 DUMMAT(N1,N2) = 0.0                                               M28.6408
15010 CONTINUE                                                          M28.6409
      IF (LOOPSS(8) .EQ. 1) GO TO 15100                                 M33.2030
      D11 = GPAR(KCL+1) * FPAR(KCL+1)                                   M28.6410
      D12 = GPAR(KCL+2) * FPAR(KCL+2)                                   M28.6411
      DUMMAT(1,1) = - GPAR(KCL) * FPAR(KCL)                             M28.6412
      DUMMAT(2,2) = - D11                                               M28.6413
      DUMMAT(3,3) = - D12                                               M28.6414
      CALL MATMUL(DUMMAT,ZTHEVR)                                        M28.6415
      D1 = ZTHEV + (CON(7) + CON(8)) * RD2                              M28.6416
      DUMMAT(3,3) = DUMMAT(3,3) - D1 * D12                              M28.6417
      DO 15015 N1 = 1,3                                                 M28.6418
      N2 = KCL - 1 + N1                                                 M28.6419
      D1 = (GPAR(N2)*REACL(N2)/SELTA2 - 1.0) * FPAR(N2)                 M28.6420
15015 DUMMAT(N1,N1) = DUMMAT(N1,N1) + D1                                M28.6421
      D1 = (1.0-CON(4))*OMEGE*D11*REACL(KCL+2)                          M28.6422
      DUMMAT(2,3) = DUMMAT(2,3) - D1                                    M28.6423
      D1 = (1.0-CON(4))*OMEGE*D12*REACL(KCL+1)                          M28.6424
      DUMMAT(3,2) = DUMMAT(3,2) + D1                                    M28.6425
      DO 15020 N1 = 1,3                                                 M28.6426
15020 DUMVEC(N1) = UMCUR(N1)                                            M28.6427
      CALL MATVEC(DUMMAT,DUMVEC)                                        M28.6428
      DO 15025 N1 = 1,3                                                 M28.6429
      N2 = KCL - 1 + N1                                                 M28.6430
15025 HIST(N2) = DUMVEC(N1) - GPAR(N2)*FPAR(N2)*VINP(N1)                M28.6431
      D5 = D11 * (FLXD(JM)/SELTA2 - FLXQ(JM)*OMEGE)                     M28.6432
      HIST(KCL+1) = HIST(KCL+1) + D5                                    M28.6433
      D5 = D12 * (FLXD(JM)*OMEGE + FLXQ(JM)/SELTA2)                     M28.6434
      HIST(KCL+2) = HIST(KCL+2) + D5                                    M28.6435
      D1 = (CON(7)+CON(8)) * D12                                        M28.6436
      HIST(KCL+2) = HIST(KCL+2) + FLXD(JM)*D1/SELTA2                    M28.6437
C. ROTOR(POWER)-HIST FOR SPECIAL DM:                                    M33.2031
      IF (CON(8) .NE. 1.0) GO TO 15050                                  M28.6439
      D9 = RD2 - REACL(KCL+4)/SELTA2                                    M28.6440
      D10 = ZTHEV + D9                                                  M28.6441
      HIST(KCL+2) = HIST(KCL+2) + CON(8)*D12*D10*UMCUR(5)               M28.6442
15050 IF (CON(7) .NE. 1.0) GO TO 15100                                  M28.6443
      HIST(KCL+2) = HIST(KCL+2) + CON(7)*D12*ZTHEV*UMCUR(5)             M28.6444
C. DETERMINATION OF STATOR(EXCIT)-HIST:                                 M33.2032
15100 DO 15105 N1 = KCLD1,KCLQE                                         M28.6446
      N2 = N1 - KCL + 1                                                 M28.6447
      D1 = GPAR(N1) * FPAR(N1)                                          M28.6448
      D2 = D1 * REACL(N1)/SELTA2                                        M28.6449
      D3 = (- FPAR(N1) + D2) * UMCUR(N2)                                M28.6450
15105 HIST(N1) = - D1*VINP(N2) + D3                                     M28.6451
      IF (NCLD(JM) .EQ. 0) GO TO 15115                                  M28.6452
      D13 = GPAR(KCL+3) * FPAR(KCL+3)                                   M28.6453
      DO 15110 N1 = KCLD1,KCLDE                                         M28.6454
      D1 = FLXD(JM) * GPAR(N1) * FPAR(N1)/SELTA2                        M28.6455
15110 HIST(N1) = HIST(N1) + D1                                          M28.6456
      HIST(KCL+3) = HIST(KCL+3)-FLXD(JM)*CON(8)*D13/SELTA2              M28.6457
15115 IF (NCLQ(JM) .EQ. 0) GO TO 15200                                  M28.6458
      DO 15120 N1 = KCLQ1,KCLQE                                         M28.6459
      D1 = FLXQ(JM) * GPAR(N1) * FPAR(N1)/SELTA2                        M28.6460
15120 HIST(N1) = HIST(N1) + D1                                          M28.6461
C. STATOR(EXCIT)-HIST FOR SPECIAL DM:                                   M33.2033
      IF (CON(8) .NE. 1.0) GO TO 15150                                  M28.6463
      HIST(KCL+3) = HIST(KCL+3) + CON(8)*D13*D10*UMCUR(3)               M28.6464
      D9 = (ZTHEV + RD2) * D13/SELTA2                                   M28.6465
      HIST(KCL+3) = HIST(KCL+3) - CON(8)*D9*UMCUR(4)                    M28.6466
15150 IF (CON(7) .NE. 1.0) GO TO 15200                                  M28.6467
      HIST(KCL+3) = HIST(KCL+3) + CON(7)*D13*ZTHEV*UMCUR(3)             M28.6468
      D9 = ZTHEV * D13/SELTA2                                           M28.6469
      HIST(KCL+3) = HIST(KCL+3) - CON(7)*D9*UMCUR(4)                    M28.6470
C. STATOR(EXCIT)-HIST FOR UM = TYPE 4 (C3IM):                           M33.2034
15200 IF (CON(6) .EQ. 0.0) GO TO 15210                                  M28.6472
      N1 = KCLE - KCL + 1                                               M28.6473
      D1 = GPAR(KCLE) * FPAR(KCLE)                                      M28.6474
      D2 = D1*REACL(KCLE)/SELTA2 - FPAR(KCLE)                           M28.6475
      HIST(KCLE) = D2*UMCUR(N1) - D1*VINP(N1)                           M28.6476
C. IMPLEMENTATION OF INFLUENCE OF THEV IMPEDANCES :                     M28.6477
15210 N15 = KCLE                                                        M28.6478
      IF (NCL .GT. 6) N15 = KCL + 5                                     M28.6479
      N16 = N15 - KCL - 2                                               M28.6480
      DO 15230 N1 = 1,3                                                 M28.6481
      IF (N1 .GT. N16) GO TO 15225                                      M28.6482
      DUMVEC(N1) = UMCUR(N1+3)                                          M28.6483
      GO TO 15230                                                       M28.6484
15225 DUMVEC(N1) = 0.0                                                  M28.6485
15230 CONTINUE                                                          M28.6486
      CALL MATVEC(ZTHS3,DUMVEC)                                         M28.6487
      DO 15235 N1 = KCLD1,N15                                           M28.6488
      N2 = N1 - KCL - 2                                                 M28.6489
      DUMVEC(N2) = GPAR(N1) * FPAR(N1) * DUMVEC(N2)                     M28.6490
15235 HIST(N1) = HIST(N1) - DUMVEC(N2)                                  M28.6491
      IF (NCL .LE. 6) GO TO 15300                                       M28.6492
      N15 = KCLD1 + 3                                                   M28.6493
      DO 15240 N1 = N15,KCLE                                            M28.6494
      N2 = N1 - KCL + 1                                                 M28.6495
      D1 = GPAR(N1) * FPAR(N1) * ZTHEVS(N2)                             M28.6496
15240 HIST(N1) = HIST(N1) - D1 * UMCUR(N2)                              M28.6497
C. DETERMINATION OF HISTOM:                                             M28.6498
15300 HISTOM(JM) = 0.0                                                  M28.6499
      IF (JTMTAC(JM) .EQ. 0) GO TO 16000                                M28.6500
      HISTOM(JM) = OMEGM(JM) + TCIN                                     M28.6501
C. OUTPUT STATEMENTS FOR UM:                                            M28.6502
16000 N1 = ISTART + JM - 1                                              M28.6503
      IF (N1 .NE. 0) GO TO 16008                                        M28.6504
      IF ( IPRSUP .GE. 1 )                                              M28.6505
     1 WRITE(LUNIT6,16005)                                              M28.6506
16005 FORMAT(50H0-------------------------------------------------)     M28.6507
      GO TO 16016                                                       M28.6508
16008 IF (ISTART .NE. 0) GO TO 16012                                    M28.6509
16011 GO TO 16020                                                       M28.6510
16012 ITOLTO = ITOL + IOUT                                              M28.6511
      IF (IOUT .EQ. 0) GO TO 16100                                      M39.3122
16014 IF (ISTART .NE. ITOLTO) GO TO 16100                               M39.3123
      IF (JM .GT. 1) GO TO 16020                                        M28.6514
16016 IF (IPRSUP .GE. 2 )                                               M28.6515
     1 WRITE(LUNIT6,16005)                                              M28.6516
      IF (IPRSUP .GE. 2 )                                               M28.6517
     1 WRITE (LUNIT6,16017) T, NCOMP                                    M28.6518
16017 FORMAT(7H0TIME =,E14.5,                                           M28.6519
     1  36H***************************  NCOMP =,  I3  )                 M28.6520
16020 IF (JM .EQ. NUMUM) ITOL = ISTART                                  M28.6521
C. MOMENTARY TEST OUTPUT :                                              M28.6522
      IF ( IPRSUP  .GE.  2 )                                            M28.6523
     1 WRITE(LUNIT6,16052)                                              M28.6524
16052 FORMAT(18H0*** TEST OUTPUT :)                                     M28.6525
      IF ( IPRSUP  .GE.  3 )                                            M28.6526
     1 WRITE(LUNIT6,16054) (UMCUR(N1),N1=1,3)                           M28.6527
16054 FORMAT(10H0UMCUR(1)=,E14.5,2X,9HUMCUR(2)=,E14.5,2X,               M28.6528
     V9HUMCUR(3)=,E14.5)                                                M28.6529
      IF (CON(6) .NE. 1.0) GO TO 16056                                  M28.6530
      IF ( IPRSUP  .GE.  3 )                                            M28.6531
     1 WRITE(LUNIT6,16055) (UMCUR(N1),N1=4,6)                           M28.6532
16055 FORMAT(10H UMCUR(4)=,E14.5,2X,9HUMCUR(5)=,E14.5,2X,               M28.6533
     V9HUMCUR(6)=,E14.5)                                                M28.6534
16056 IF ( IPRSUP  .GE.  2 )                                            M28.6535
     1 WRITE(LUNIT6,16057) FLXD(JM),FLXQ(JM),NITROM                     M28.6536
16057 FORMAT(10H     FLXD=,E14.5,2X,9H    FLXQ=,E14.5,2X,               M28.6537
     V9H  NITROM=,I3)                                                   M28.6538
      N2 = KCL + NCL - 1                                                M28.6539
      IF (ISTART .GT. 1) GO TO 16077                                    M28.6540
      IF ( IPRSUP .LE. 3 ) GO TO 16077                                  M28.6541
      WRITE(LUNIT6,16060) (NODVO1(N1),N1=KCL,N2)                        M28.6542
      WRITE(LUNIT6,16061) (NODVO2(N1),N1=KCL,N2)                        M28.6543
      WRITE(LUNIT6,16062) NODOM(JM)                                     M28.6544
16060 FORMAT(9H0NODVO1 :,3X,15I4)                                       M28.6545
16061 FORMAT(9H NODVO2 :,3X,15I4)                                       M28.6546
16062 FORMAT(9H NODOM  :,3X,15I4)                                       M28.6547
16077 IF (IPRSUP .GE. 2)                                                M28.6548
     1 WRITE(LUNIT6,16078) ZTHEVM                                       M28.6549
16078 FORMAT(10H0ZTHEVM = ,E14.5)                                       M28.6550
      IF (IPRSUP .GE. 2)                                                M28.6551
     1 WRITE(LUNIT6,16080) ((ZTHEVR(N1,N2),N2=1,3),N1=1,3)              M28.6552
16080 FORMAT(13H0ZTHEVR(3,3):,3X,3E14.5/(16X,3E14.5))                   M28.6553
      IF (IPRSUP .GE. 2)                                                M28.6554
     1 WRITE(LUNIT6,16082) ((ZTHS3(N1,N2),N2=1,3),N1=1,3)               M28.6555
16082 FORMAT(12H0ZTHS3(3,3):,3X,3E14.5/(15X,3E14.5))                    M28.6556
C. END MOMENTARY                                                        M28.6557
C  FINAL STATEMENTS OF MACHINE DO-LOOP *************************        M33.2035
16100 IF (NSHARE .NE. 0) FPAR(KCL+2) = TQGEN                                    
C. IN CASE OF SM TYPE-59 : STORING AND INJECT NEW EXCITER TORQUE        M38.3533
      IF (NTYP59 .NE. 1) GO TO 18000                                    M39.3125
      JTYPE(JM) = 13                                                    M38.3535
      INPU = INPUST                                                     M38.3536
      IF (NMEXC .EQ. 0) GO TO 18000                                     M38.3537
      D1 = E(NMEXC)                                                     M38.3538
      D2 = E(NEXC)                                                      M38.3539
      D3 = UMCUR(4) * DCOEF(JM)                                         M38.3540
      CREST(KCONEX) = - D2 * D3/D1                                      M38.3541
      FPAR(KCL+3) = - KCONEX                                            M38.3542
      FPAR(KCL+4) = NMEXC                                               M38.3543
      FPAR(KCL+5) = NEXC                                                M38.3544
C. PREDICTION OF FLUXES FOR INTERNAL UM VOLTAGE OF NEXT TIME-           M33.2036
C      STEP IF NO COMPENSATION FOR POWER COILS.                         M33.2037
C      UMCURP(1) TO STORE ZERO CURRENT                                  M33.2038
C      UMCURP(3NUMUM:6NUMUM) TO STORE VPI/RP FOR NEXT TIME-STEP.        M33.2039
18000 IF (LOOPSS(8) .NE. 1) GO TO 99999                                 M33.2040
      NCURPR = 1                                                        M33.2041
      D19 = GPAR(KCL) * FPAR(KCL)                                       M33.2042
      D20 = GPAR(KCL) * REACL(KCL)                                      M33.2043
      N1 = 3 * (JM -1)                                                  M33.2044
      N2 = 3 * (NUMUM+JM-1)                                             M33.2045
C      TRANSFORMATION TO ZERO-FREQ DOMAIN :                             M33.2046
      D10 = (TAU - THETAE) * (1.0-CON(1)-CON(4))                        M33.2047
      IF (INPU .EQ. 1) D10 = D10 * OMEGRF                               M33.2048
      D11 = COSZ(D10)                                                   M33.2049
      D12 = SINZ(D10)                                                   M33.2050
      D15 = FLXD(JM)                                                    M33.2051
      D16 = FLXQ(JM)                                                    M33.2052
      FLXD(JM) = D11*D15 - D12*D16                                      M33.2053
      FLXQ(JM) = D12*D15 + D11*D16                                      M33.2054
      D15 = UMCUR(2)                                                    M33.2055
      D16 = UMCUR(3)                                                    M33.2056
      UMCUR(2) = D11*D15 - D12*D16                                      M33.2057
      UMCUR(3) = D12*D15 + D11*D16                                      M33.2058
      FLXDD = FLXD(JM) + REACL(KCL+1)*UMCUR(2)                          M33.2059
      FLXQQ = FLXQ(JM) + REACL(KCL+2)*UMCUR(3)                          M33.2060
C      INITIALIZATION AT TIME = 0.0                                     M33.2061
      IF (ISTART .NE. 0) GO TO 18020                                    M33.2062
      FLXDMH = FLXD(JM)                                                 M33.2063
      FLXQMH = FLXQ(JM)                                                 M33.2064
      UMCURP(N1+1) = 0.0                                                M33.2065
      HIST(KCL) = 0.0                                                   M33.2066
      HIST(KCL+1) = FLXDD                                               M33.2067
      HIST(KCL+2) = FLXQQ                                               M33.2068
C      LINEAR PREDICTION OF FLUXES :                                    M33.2069
18020 FLXDDP = 2.0*FLXDD - HIST(KCL+1)                                  M33.2070
      FLXQQP = 2.0*FLXQQ - HIST(KCL+2)                                  M33.2071
C       CALCULATION OF VPI/RP FOR NEXT TIME-STEP                        M33.2072
C     DUMVEC(1) = D20 * (UMCURP(N1+1) - UMCUR(1))/SELTAT                M42.6761
      DUMVEC(1) = 0.0                                                   M42.6762
C     D1 = (HIST(KCL+1) - FLXDD)/SELTAT                                 M33.2075
      D1 = (FLXDMH - FLXD(JM))/SELTAT                                   M33.2076
      DUMVEC(2) = (D1 - OMEGRF*FLXQQP) * GPAR(KCL+1)                    M33.2077
C     D1 = (HIST(KCL+2) - FLXQQ)/SELTAT                                 M33.2078
      D1 = (FLXQMH - FLXQ(JM))/SELTAT                                   M33.2079
      DUMVEC(3) = (D1 + OMEGRF*FLXDDP) * GPAR(KCL+2)                    M33.2080
      D20 = FPAR(KCL) * (D20/SELTA2 - 1.0)                              M33.2081
      UMCURP(N1+1) = UMCUR(1)                                           M33.2083
      HIST(KCL+1) = FLXDD                                               M33.2084
      HIST(KCL+2) = FLXQQ                                               M33.2085
18030 THETAE = TAU + DELTAT*OMEGRF                                      M33.2086
      THETAE = THETAE * (1.0-CON(1)-CON(4))                             M33.2087
      IF (INPU .EQ. 1) THETAE = OMEGRF * THETAE                         M33.2088
      GO TO 12200                                                       M33.2089
18100 DO 18120 N1 = 1,3                                                 M33.2090
      N2 = 3 * (NUMUM+JM-1) + N1                                        M33.2091
18120 UMCURP(N2) = DUMVEC(N1)                                           M33.2092
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,18130) JM                         M37.6105
18130 FORMAT(/,39H **************************************,              M37.6106
     1         42H PREDICTED A,B,C POWER VOLTAGES/RESISTANCE,           M37.6107
     2         32H FOR NEXT TIME-STEP OF UM NUMBER, I4, 2H :)           M37.6108
      N1 = 3 * (NUMUM + JM - 1) + 1                                     M37.6109
      N2 = N1 + 2                                                       M37.6110
      IF (IPRSUP .GE. 1)                                                M37.6111
     1 WRITE (LUNIT6,18132) N1,N2,(UMCURP(N3),N3=N1,N2)                 M37.6112
18132 FORMAT(5X, 8H UMCURP(,I3,1H:,I3,3H) :,3(3X,E14.5))                M37.6113
      IF (JM .NE. NUMUM) GO TO 99999                                    M33.2093
      LOOPSS(8) = 3                                                     M33.2094
C. CONTINUATION OF MACHINE DO-LOOP:                                     M28.6558
99999 CONTINUE                                                          M32.6454
C  START SECOND MACH DO-LOOP IN CASE A SET OF UM'S SHARING              M39.3126
C   A COMMON MECHANICAL NETWORK IS PRESENT :                            M39.3127
30000 IF (MSHARE .EQ. 0) GO TO 20000                                    M39.3128
      DO 30010 JM = 1,NUMUM                                             M39.3129
      KCL = KCOIL(JM)                                                   M39.3130
      NSHARE = JCLTAC(KCL) + JCLTAC(KCL+1)                              M39.3131
      IF (NSHARE .EQ. 0) GO TO 30010                                    M39.3132
C  PREDICTING TORQUES OF HIGHER NUMBERED UM'S IN THE SET :              M39.3133
C    NOTE : FPAR(KCL+2) IS TQGEN AT TIME T                              M39.3134
C    NOW PREDICT TQGEN FOR NEXT TIME-STEP (FOR T+DELTAT)                M39.3135
      VOLTUM(JM) = 2.0 * FPAR(KCL+2) - ANGLUM(JM)                       M39.3136
      IF (ISTART .EQ. 0) VOLTUM(JM) = FPAR(KCL+2)                       M39.3137
      ANGLUM(JM) = FPAR(KCL+2)                                                  
C  SETTING  CURSUB FOR TQGEN OF HIGHER NUMBERED UM'S IN SET :           M39.3139
      IF (JCLTAC(KCL+2) .GE. 0) GO TO 30010                             M39.3140
      N1 = - JCLTAC(KCL+2)                                              M39.3141
      CURSUB(N1) = FPAR(KCL+2)                                          M39.3142
30010 CONTINUE                                                          M39.3143
20000 ISTART = ISTART + 1                                               M28.6560
21000 IF ( IPRSUP  .GE.  1 )                                            M36.1662
     1 WRITE (LUNIT6, 7892)  ISTART,LOOPSS(1),LOOPSS(8),NUMUM,T         M36.1663
 7892 FORMAT ( /,  33H EXIT  "SOLVUM" .  ISTART LOOPSS1,                M36.1664
     1  16H LOOPSS8   NUMUM,13X,  1HT  ,/,  17X, 4I8,  E14.5,/)         M36.1665
      RETURN                                                            M36.1666
      END                                                               M28.6566
      SUBROUTINE LINEQS(AUM,YUM)                                        M27.2526
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION BUM(3,4),AUM(3,3),YUM(15)                               M27.2527
      N5 = 3                                                            M27.2528
      N6 = N5 + 1                                                       M27.2529
C FORMATION OF AUGMENTED MATRIX                                         M27.2530
      DO 20 N10 = 1,N5                                                  M27.2531
      DO 10 N11 = 1,N5                                                  M27.2532
   10 BUM(N10,N11) = AUM(N10,N11)                                       M27.2533
      BUM(N10,N6)  = YUM(N10)                                           M27.2534
   20 CONTINUE                                                          M27.2535
C START GAUSSIAN ELIMINATION                                            M27.2536
      DO 40 N1 = 1,N5                                                   M27.2537
      N2 = N1 + 1                                                       M27.2538
      IF (N2 .GT. N5) GO TO 50                                          M27.2539
      DO 40 N10 = N2,N5                                                 M27.2540
      N8 = N6 - N1                                                      M27.2541
      DO 30 N9 = 1,N8                                                   M27.2542
      N11 = N6 + 1 - N9                                                 M27.2543
   30 BUM(N10,N11) = BUM(N10,N11)-BUM(N10,N1)*BUM(N1,N11)/BUM(N1,N1)    M27.2544
      N11 = N6 - N8                                                     M27.2545
      BUM(N10,N11) = 0.0                                                M27.2546
   40 CONTINUE                                                          M27.2547
C CALCULATION OF SOLUTION                                               M27.2548
   50 DO 70 N10 = 1,N5                                                  M27.2549
      N7 = N5 + 1 - N10                                                 M27.2550
      YUM(N7) = BUM(N7,N6)/BUM(N7,N7)                                   M27.2551
      DO 70 N12 = 1,N5                                                  M27.2552
      N13 = N7 + N12                                                    M27.2553
      IF (N13 .GT. N5) GO TO 70                                         M27.2554
   60 YUM(N7) = YUM(N7)-BUM(N7,N13)*YUM(N13)/BUM(N7,N7)                 M27.2555
   70 CONTINUE                                                          M27.2556
      RETURN                                                            M27.2557
      END                                                               M27.2558
      SUBROUTINE SUBTS4                                                    13931
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      DIMENSION  XX(1)                                                     13935
      DIMENSION  NSUBKM(1)                                              M37.6114
      EQUIVALENCE  ( KKNONL(1), NSUBKM(1) )                             M37.6115
      EQUIVALENCE (  XK(1), XX(1) )                                        13937
      DIMENSION  ISPUM(1)                                               M32.6464
      EQUIVALENCE  ( SPUM(1), ISPUM(1) )                                M32.6465
      EQUIVALENCE  (MONCAR(1),    KNT),   (MONCAR(2),  KBASE)           M37.6116
      EQUIVALENCE  (MONCAR(9), KLOAEP)                                  M37.6117
      IF ( IPRSUP.GE.6 )                                                M28.6567
     1 WRITE (LUNIT6, 1000) KSWTCH, INONL, NUM99, NCOMP, NTOT           M37.6118
 1000 FORMAT (  36H TOP "SUBTS4". KSWTCH, INONL, NUM99,,                M37.6119
     1          14H NCOMP, NTOT =,  10I8 )                              M37.6120
 1553 IF ( NUMUM  .LE.  0 )   GO TO 1742                                M27.2589
      CALL SOLVUM ( SPUM(IUREAC),                                       M33.2097
     1 SPUM(IUGPAR), SPUM(IUFPAR), SPUM(IUHIST), SPUM(IUUMRP),          M33.2098
     2 ISPUM(IUNOD1), ISPUM(IUNOD2), ISPUM(IUJCLT), ISPUM(IUJCLO),      M32.6466
     3 ISPUM(IUJTYP), ISPUM(IUNODO), ISPUM(IUJTMT), SPUM(IUHISM),       M32.6467
     4 SPUM(IUOMGM), SPUM(IUOMLD), SPUM(IUTHAM), SPUM(IUREDU),          M27.2594
     5 SPUM(IUREDS), SPUM(IUFLDS), SPUM(IUFLDR), SPUM(IUREQU),          M27.2595
     6 SPUM(IUFLQS), SPUM(IUFLQR), ISPUM(IUJCDS), ISPUM(IUJCQS),        M32.6468
     7 SPUM(IUFLXD), SPUM(IUFLXQ), ISPUM(IUNPPA), SPUM(IUROTM),         M32.6469
     8 ISPUM(IUNCLD), ISPUM(IUNCLQ), ISPUM(IUJTQO), ISPUM(IUJOMO),      M32.6470
     9 ISPUM(IUJTHO), SPUM(IUREQS), SPUM(IUEPSO), SPUM(IUDCOE),         M32.6471
     9 ISPUM(IUKCOI), SPUM(IUVOLT), SPUM(IUANGL), ISPUM(IUNODF),        M32.6472
     9 ISPUM(IUNODM), ISPUM(IUKUMO), ISPUM(IUJUMO), SPUM(IUUMOU))       M32.6473
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M27.2601
 1742 IF( NCOMP .LE. 0 )  GO TO 1570                                    M27.2602
      IF( IPRSUP .GE. 6 )                                               M28.6569
     1 WRITE(LUNIT6, 1554)  ( K, KKNONL(K), E(K), ZNONL(K), K=2, NTOT ) M28.6570
 1554 FORMAT(  41H N.L. ELEMENT COMP.   K, KKNONL, E, ZNONL   ,            13941
     1 2I10, 2E20.8 )                                                      13942
      DO 1560 K=2,NTOT                                                     13943
      N1 = KPSOUR(K)                                                    M42.6763
      IF(N1.EQ.0) GO TO 1560                                               13945
      N15 = ISUBEG(N1)                                                  M37.6122
      N4 = K                                                            M25.1151
 1111 M = N15 / 5 + 1                                                   M37.6123
      E(K) = E(K)  +  CURSUB(M) * ZNONL(N4)                             M25.1153
      N15 = NSUBKM(N15)                                                 M37.6124
      IF ( N15  .EQ. ISUBEG(N1) ) GO TO 2323                            M37.6125
 1557 N4 = N4 + NTOT                                                    M25.1154
      GO TO 1111                                                        M37.6126
 2323 N8 = N15 / 5 + 1                                                  M39.3144
      IF ( IPRSUP  .GE.  7 )                                            M39.3145
     1 WRITE (LUNIT6, 1558)  K, N1, N15, E(K),                          M37.6128
     2                       CURSUB(N8), ZNONL(K)                       M39.3146
 1558 FORMAT (  11H ROW SUPER.,                                         M25.1158
     1  24H       K      N1     N15,  14X,  1HE,                        M39.3147
     2  9X, 6HCURSUB, 10X, 5HZNONL ,/, 11X, 3I8, 3E15.6 )               M37.6130
 1560 CONTINUE                                                             13952
 1570 IF ( IALTER  .EQ.  1 )   IALTER = 0                               M32.6475
      IF( IPRSUP .GT. 6 )                                                  13957
     1 WRITE(LUNIT6, 1571)  ( E(K), K=1, NTOT )                         M17. 921
 1571 FORMAT( //,  44H VECTOR E(K) AT 1571, IMMEDIATELY AFTER CALC   ,     13959
     1 /, ( 1X, 5E25.15 ) )                                                13960
      IF ( TENERG  .LT.  0.0 )          GO TO 1609                      M23.4870
      IF ( KNT  .GT.  1 )               GO TO 1609                      M25.1161
      IF ( KBASE  .LT.  2 )             GO TO 1609                      M25.1162
      IF ( T + DELTAT  .LT.  TENERG )   GO TO 1609                      M23.4871
      IF ( IPRSUP  .GE.  -1 )                                           M23.4884
     1 WRITE (LUNIT6, 4624)  KNT, KBASE, KLOAEP, NENERG, T, TENERG      M23.4885
 4624 FORMAT (/, 113H AT END OF  'SUBTS4', COMMON BLOCKS ARE WRITTEN ON M23.4886
     1 'LUNIT2'  DURING THE 1ST ENERGIZATION OF  'STATISTICS'  CASE. ,/,M23.4887
     2 32H     KNT   KBASE  KLOAEP  NENERG,  14X,  1HT,  9X,  6HTENERG  M23.4888
     3  ,/,  4I8,  2E15.6    )                                          M23.4889
      CALL TABLES                                                       M25.1163
      TENERG = -T                                                       M25.1164
 1609 IF ( T  .LE.  TMAX )   GO TO 1611                                 M28.6571
      IF ( MEMSAV  .NE.  0 )   GO TO 1613                               M30. 999
 1611 LASTOV = NCHAIN                                                   M28.6573
      NCHAIN = 16                                                          13961
      RETURN                                                            M27.2603
 1613 LASTOV = NCHAIN                                                   M28.6574
      NCHAIN = 20                                                       M28.6575
      RETURN                                                            M28.6576
 9200 LASTOV = NCHAIN                                                   M27.2604
      LSTAT(18) = NCHAIN                                                M27.2605
      NCHAIN = 51                                                       M27.2606
99999 RETURN                                                               13963
      END                                                                  13964
