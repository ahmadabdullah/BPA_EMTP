      SUBROUTINE OVER42
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION  CURRR(101), FLUX(101), SSS(90)                            15667
C     THE FOLLOWING LIMIT CHECKS OVERFLOW OF  CURR, FLUX                   15668
      IF ( IPRSUP  .GE.  1 )                                            M32, 282
     1 WRITE ( LUNIT6, 4567 )                                           M32, 283
 4567 FORMAT ( 24H  "BEGIN MODULE OVER42." )                            M32, 284
      MAXPT = 101                                                          15669
      PI = TWOPI / 2.0                                                  M05. 806
      PICON = PI / 180.                                                    15671
      DO 3  I=1, 90                                                        15672
    3 SSS(I) = SINZ( I*PICON)                                           M18. 889
      D1 = 2.0                                                          M10. 373
      SQ2 = SQRTZ(D1) * 1000.                                           M18. 890
C     CHECK FOR "CONVERT ZNO" CONNECTION OF "REQUES" (OVERLAY 1):       M39.3405
      IF ( IOFBND .NE. 99876 )  GO TO 5                                         
      CALL ZINOLD                                                               
      GO TO 9200                                                                
    5 N=1                                                                  15675
C     READ INPUT CARD USING CIMAGE                                         15676
      CALL CIMAGE                                                          15677
      IF ( KOLBEG  .GT.  0 )   GO TO 2103                               M22.5950
      DECODE (80, 10, ABUFF(1) )  FREQ, VBASE, PBASE, IPNCH, KTHIRD     M12. 589
   10 FORMAT ( 3E8.0, 2I8 )                                             M12. 590
      GO TO 2105                                                        M22.5951
 2103 NFRFLD = 1                                                        M22.5952
      CALL FREONE ( FREQ )                                              M29.4116
      CALL FREONE ( VBASE )                                             M29.4117
      CALL FREONE ( PBASE )                                             M29.4118
      CALL FREONE ( D11 )                                               M29.4119
      IPNCH = D11                                                       M22.5957
      CALL FREONE ( D11 )                                               M29.4120
      KTHIRD = D11                                                      M22.5959
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M22.5960
 2105 IF ( FREQ  .EQ.  0.0 )   GO TO 2050                               M22.5961
      IF ( FREQ  .NE.  88. )   GO TO 7219                               M27.3438
C     CONNECT WITH ROUTINE WRITTEN BY PROF. NED MOHAN OF THE            M25.1222
C     UNIVERSITY OF MINNESOTA.   THIS GENERATES THE CURRENT             M25.1223
C     VS. FLUX POINTS NEEDED FOR  TYPE-96  HYSTERETIC INDUCTOR          M25.1224
C     MODELING.                                                         M25.1225
      WRITE (KUNIT6, 7215)                                              M35.9000
 7215 FORMAT ( 34H+FREQ=88 REQUESTS HYSTERESIS DATA.  )                 M25.1227
      CALL HYSDAT                                                       M25.1228
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M25.1229
      GO TO 5                                                           M25.1230
 7219 IF ( FREQ  .NE.  77. )   GO TO 7228                               M27.3439
C     CONNECT WITH ROUTINE WRITTEN BY DR. VLADIMIR BRANDWAJN            M27.3440
C     OF ONTARIO HYDRO.  THIS GENERATES EMTP DATA CARDS FOR             M27.3441
C     ZNO  SURGE ARRESTERS USING LEAST SQUARES FITTING.                 M27.3442
      WRITE (KUNIT6, 7221)                                              M35.9001
 7221 FORMAT ( 39H+FREQ=77 REQUESTS  ZNO  DATA GENERATOR.  )            M27.3444
      CALL ARRDAT                                                       M27.3445
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M27.3446
      GO TO 5                                                           M27.3447
 7228 WRITE (KUNIT6, 15)  FREQ, VBASE, PBASE, IPNCH                     VAX.3456
   15 FORMAT ( 13H+MISC. CONST.  ,  2X, 3E10.3, I5  )                      15682
      IF ( FREQ  .EQ.  -1. )   GO TO 19                                 M12. 591
      IF ( VBASE  .GT.  0.0 )   GO TO 18                                   15683
   16 KILL = 90                                                            15684
      FLSTAT(14) = VBASE                                                   15685
      FLSTAT(15) = PBASE                                                   15686
      LSTAT(19) = 16                                                       15687
      GO TO 9200                                                           15688
   18 IF ( PBASE  .LE.  0.0 )   GO TO 16                                   15689
      FLUX(1)=0.0                                                          15690
      CURRR(1)=0.0                                                         15691
      OMEGAH = TWOPI * FREQ                                                15692
      VFACT = SQ2 * VBASE / OMEGAH                                         15693
      CFACT=PBASE*1000.0/VBASE                                             15694
      DLTAT = PICON                                                        15695
      SLOPE1 = FLTINF                                                      15696
      C1OLD = 0.0                                                          15697
      VOLD = 0.0                                                           15698
      GO TO 20                                                          M12. 592
   19 IF ( PBASE  .LE.  0.0 )   PBASE = 1.0                             M12. 593
      IF ( VBASE  .LE.  0.0 )   VBASE = 1.0                             M12. 594
      VFACT = PBASE                                                     M12. 595
      CFACT = VBASE                                                     M12. 596
      C1OLD = - 1.0/FLTINF                                              M12. 597
      N = 0                                                             M12. 598
C     READ INPUT CARD USING CIMAGE                                         15699
   20 CALL CIMAGE                                                          15700
      IF ( KOLBEG  .GT.  0 )   GO TO 2113                               M22.5962
      DECODE(80,23,ABUFF)C1,V                                              15701
   23 FORMAT ( 2E16.0 )                                                    15702
      GO TO 2115                                                        M22.5963
 2113 NFRFLD = 1                                                        M22.5964
      CALL FREONE ( C1 )                                                M29.4121
      IF ( C1  .EQ.  9999. )   GO TO 2115                               M22.5966
      CALL FREONE ( V )                                                 M29.4122
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M22.5968
 2115 IF ( C1  .EQ.  9999. )   GO TO 100                                M22.5969
      IF ( FREQ  .GT.  0.0 )   GO TO 41                                 M12. 599
      WRITE (KUNIT6, 38)  C1, V                                         M35.9003
   38 FORMAT ( 14H+(I, L) POINT.  , 2X, 2E13.5  )                       M12. 601
      GO TO 49                                                          M12. 602
   41 WRITE (KUNIT6, 45)  C1, V                                         M35.9004
   45 FORMAT ( 14H+(I, V) POINT.  ,  2X, 2E13.5  )                         15705
   49 N = N + 1                                                         M12. 604
      IF( N .LE. MAXPT )  GO TO 70                                         15707
      KILL = 69                                                            15708
      LSTAT(12) = MAXPT                                                    15709
      LSTAT(19) = 70                                                       15710
      GO TO 9200                                                           15711
   70 IF ( FREQ  .GT.  0.0 )   GO TO 74                                 M12. 605
      IF ( C1  .LE.  C1OLD )   GO TO 75                                 M12. 606
      IF ( V  .LE.  0.0 )   GO TO 75                                    M12. 607
      GO TO 90                                                          M12. 608
   74 IF ( V  .GT.  VOLD )   GO TO 80                                   M12. 609
   75 LSTAT(12) = N-1                                                      15713
      FLSTAT(13) = C1                                                      15714
      FLSTAT(14) = C1OLD                                                   15715
      FLSTAT(15) = V                                                       15716
      FLSTAT(16) = VOLD                                                    15717
      KILL = 71                                                            15718
      LSTAT(19) = 75                                                       15719
      GO TO 9200                                                           15720
   80 IF( C1 .LE. C1OLD )  GO TO 75                                        15721
      SLOPE2  =  (V-VOLD) / (C1-C1OLD)                                     15722
      IF ( SLOPE1  .GE.  SLOPE2 )   GO TO 90                               15723
      WRITE(LUNIT6,82)                                                  M17.1201
   82 FORMAT(/,121H ****************************************************   15725
     1******************************************************************   15726
     2**    ,/,1X)                                                      M15.1526
      WRITE(LUNIT6,86)                                                  M17.1202
   86 FORMAT(112H WARNING.  THE LAST DATA POINT CAUSES THE V-I CURVE TO    15729
     1DEVIATE FROM THE EXPECTED CONVEX FORM.  THAT IS THE SLOPE  ,/,110H   15730
     2 OF THIS SECTION IS GREATER THAN THAT OF THE PREVIOUS SECTION.  AL   15731
     3THOUGH THIS IS NOT CONSIDERED AN ERROR, THIS    ,/,58H CONDITION W   15732
     4ARRENTS DOUBLE CHECKING THE DATA FOR ACCURACY.  )                    15733
      WRITE(LUNIT6,82)                                                  M17.1203
   90 SLOPE1=SLOPE2                                                        15735
      C1OLD=C1                                                             15736
      VOLD = V                                                             15737
      FLUX(N)=V*VFACT                                                      15738
      CURRR(N)=C1*CFACT                                                    15739
      GO TO 20                                                             15740
  100 IF ( N  .GT.  1 )   GO TO 1784                                       15741
      IF ( FREQ  .LT.  0.0 )   GO TO 1784                               M12. 610
      WRITE (KUNIT6, 45)  C1, V                                         M35.9005
      WRITE (LUNIT6, 1742)                                              M17.1205
 1742 FORMAT ( 112H PROBLEM ----- THE VERY FIRST POINT OF THE CHARACTERI   15744
     1STIC HAS JUST BEEN READ, AND IT HAS BEEN OBSERVED THAT THIS   ,/,    15745
     2 15X, 104HHAS VOLTAGE EQUAL TO ZERO.   THIS IS ILLEGAL.   THE FIRS   15746
     3T SECTION OF THE CURVE MUST HAVE POSITIVE SLOPE,     ,/,             15747
     4 15X, 106HAND THE ORIGIN (I=0, V=0) IS AN IMPLIED POINT, NOT TO BE   15748
     5 INPUTTED BY THE USER.   THE EMTP WILL DISCARD THE   ,/,          M28.7049
     6 15X, 106HJUST-READ CARD, AND BEGIN READING THE CHARACTERISTIC POI   15750
     7NTS ONCE AGAIN, UNDER THE ASSUMPTION THAT THE USER   ,/,             15751
     8 15X,  79HHAS OVERLOOKED THIS MINOR DETAIL.   TALK ABOUT A FORGIVI   15752
     9NG PROGRAM (SOMETIMES).    )                                         15753
      GO TO 20                                                             15754
 1784 WRITE (KUNIT6, 65)                                                M35.9006
   65 FORMAT ( 36H+SPECIAL TERMINATION-OF-POINTS CARD.   )              M02. 149
      CALL INTERP                                                               
      WRITE(LUNIT6, 105)                                                M17.1207
  105 FORMAT( /,  54H DERIVED SATURATION CURVE GIVING PEAK CURRENT VS. F   15759
     1LUX   ,/, 7X, 3HROW,17X, 13HCURRENT (AMP), 15X,                      15760
     2 15HFLUX (VOLT-SEC)   )                                              15761
      IF ( FREQ  .GT.  0.0 )   GO TO 195                                M12. 611
      PSIOLD = 0.0                                                      M12. 612
      IF ( N  .GE.  2 )   GO TO 124                                     M12. 613
      WRITE (LUNIT6, 117)                                               M17.1208
  117 FORMAT ( //, 104H NOTE. ---- THE JUST-READ CURRENT VS. INCREMENTALM12. 615
     1 INDUCTANCE CHARACTERISTIC DOES NOT CONTAIN TWO OR MORE    ,/,    M12. 616
     2         12X, 101HPOINTS.   NO CURVE IS THUS DEFINED, AND THE EMTPM12. 617
     3 CAN NOT INTEGRATE INDUCTANCE TO PRODUCE FLUX.   THIS    ,/,      M12. 618
     4         12X, 102H'SATURATION'  DATA CASE IS THUS BEING ABORTED INM12. 619
     5 OVERLAY 42, AND CONTROL WILL PASS ON TO READ THE NEXT    ,/,     M12. 620
     6         12X,  30HSUCH  'SATURATION'  DATA CASE.   , //,1X)       M15.1527
      GO TO 5                                                           M12. 622
  124 IF ( CURRR(1)  .EQ.  0.0 )   GO TO 138                            M12. 623
      D1 = FLUX(1) / VFACT                                              M12. 624
      WRITE (LUNIT6, 128)  D1                                           M17.1209
  128 FORMAT ( //, 110H WARNING. ---- THE JUST-READ CURRENT VS. INCREMENM12. 626
     1TAL INDUCTANCE CHARACTERISTIC DOES NOT BEGIN WITH A POINT FOR  ,/,M12. 627
     2           15X, 95HZERO CURRENT.   THIS MISSING POINT IS BEING AUTM12. 628
     3OMATICALLY SUPPLIED BY THE EMTP, WITH ASSOCIATED    ,/,           M12. 629
     4           15X,  66HINDUCTANCE VALUE TAKEN TO EQUAL THE FIRST-INPUM12. 630
     5TED VALUE, WHICH WAS,     E15.5,  18H  .   HAS THE USER    ,/,    M12. 631
     6           15X,  82HEVER SEEN SUCH AN INTELLIGENT, BENEVOLENT, CONM12. 632
     7SIDERATE, FORGIVING PROGRAM AS THIS.    ,   //,1X)                M15.1528
      DO 134  J=1, N                                                    M12. 634
      L = N + 1 - J                                                     M12. 635
      CURRR(L+1) = CURRR(L)                                             M12. 636
  134 FLUX(L+1) = FLUX(L)                                               M12. 637
      CURRR(1) = 0.0                                                    M12. 638
      N = N + 1                                                         M12. 639
  138 PSIOLD = 0.0                                                      M12. 640
      DO 142  J=2, N                                                    M12. 641
      D1  =  ( FLUX(J-1) + FLUX(J) ) / 2.0                              M12. 642
      D2 = CURRR(J) - CURRR(J-1)                                        M12. 643
      FLUX(J-1) = PSIOLD                                                M12. 644
  142 PSIOLD = PSIOLD + D1*D2                                           M12. 645
      FLUX(N) = PSIOLD                                                  M12. 646
      WRITE (LUNIT6, 147)                                               M17.1210
  147 FORMAT ( 10X,  100(1H-)  ,/,                                      M12. 648
     1                 10X,  89HREMEMBER.   THE JUST-COMPLETED CONVERSIOM12. 649
     1N BEGAN WITH A CURRENT VS. INCREMENTAL INDUCTANCE     ,/,         M12. 650
     2 10X, 105HCHARACTERISTIC, DUE TO MISCELLANEOUS DATA PARAMETER  'FRM12. 651
     3EQ'  OF COLUMNS 1-8 BEING PUNCHED WITH A VALUE OF      ,/,        M12. 652
     4      10X,  91H -1.0  .    TRAPEZOIDAL RULE INTEGRATION OF THE INDM12. 653
     5UCTANCE CURVE WAS USED, TO PRODUCE FLUX. ,/,                      M12. 654
     6 10X,  100(1H-)   )                                               M12. 655
      GO TO 500                                                         M12. 656
  195 CURRR(2) = CURRR(2) * SQ2 / 1000.                                 M12. 657
      I = 2                                                             M12. 658
  200 IF( I .GE. N )  GO TO 500                                            15763
      I=I+1                                                                15764
      Y=FLUX(I)                                                            15765
      CHECK=FLUX(2)                                                        15766
      TEE=0.0                                                              15767
      DFLUX=CHECK                                                          15768
      CNEW=CURRR(2)                                                        15769
      RATIO=CNEW/CHECK                                                     15770
      OLDC=0.0                                                             15771
      OLDF=0.0                                                             15772
      A1=0.0                                                               15773
      A2=0.0                                                               15774
      A3=0.0                                                               15775
      K=2                                                                  15776
      M=0                                                                  15777
  215 M=M+1                                                                15778
      TEE=TEE+DLTAT                                                        15779
      PSI=Y*SINZ(TEE)                                                   M18. 891
      IF( PSI .LE. CHECK )  GO TO 230                                      15781
      K=K+1                                                                15782
      OLDF=CHECK                                                           15783
      CHECK = FLUX(K)                                                      15784
      DFLUX=CHECK-OLDF                                                     15785
      OLDC=CNEW                                                            15786
      CNEW=CURRR(K)                                                        15787
      RATIO=(CNEW-OLDC)/DFLUX                                              15788
      IF( K .LE. I )  GO TO 224                                            15789
      KILL = 70                                                            15790
      LSTAT(19) = 224                                                      15791
      GOTO 9200                                                            15792
  224 IF( K .EQ. I )  GO TO 350                                            15793
  230 A1=A1+(OLDC+(PSI-OLDF)*RATIO)**2                                     15794
      GO TO 215                                                            15795
  350 CFACT=OLDC*2.0                                                       15796
      VFACT=OLDC**2                                                        15797
  310 A1=A1+VFACT                                                          15798
      RATIO=(PSI-OLDF)/DFLUX                                               15799
      A2=A2+CFACT*RATIO                                                    15800
      A3=A3+RATIO**2                                                       15801
      IF( M .GE. 90 )  GO TO 400                                           15802
      TEE=TEE+DLTAT                                                        15803
      M=M+1                                                                15804
      PSI=Y*SINZ(TEE)                                                   M18. 892
      GO TO 310                                                            15806
  400 A1=A1-VFACT/2.0 -90.0*CURRR(I)**2                                 M05. 808
      A2=A2-CFACT*RATIO / 2.0                                           M05. 809
      A3=A3- RATIO**2 / 2.0                                             M05. 810
      C1=(-A2+SQRTZ(A2**2-4.0*A1*A3))/(2.0*A3)                          M18. 893
      CURRR(I)=OLDC+C1                                                     15811
      GO TO 200                                                            15812
  500 IF ( KTHIRD  .NE.  1 )   GO TO 503                                M12. 659
      N1 = N - 1                                                        M12. 660
      DO 502  J=1, N1                                                   M12. 661
      L = N + 1 - J                                                     M12. 662
      D1 = -CURRR(L)                                                    M12. 663
      D2 = -FLUX(L)                                                     M12. 664
      I = -L                                                            M12. 665
      WRITE (LUNIT6, 504)  I, D1, D2                                    M17.1211
      IF ( IPNCH  .EQ.  0 )                                             M12. 667
     1 WRITE (LUNIT7, 505)  D1, D2                                      M17.1212
  502 CONTINUE                                                          M12. 669
  503 DO 510  I=1, N                                                    M12. 670
      IF ( I  .NE.  1 )   GO TO 1503                                    M12. 671
      IF ( KTHIRD  .EQ.  1 )   GO TO 510                                M12. 672
 1503 WRITE (LUNIT6, 504)  I, CURRR(I), FLUX(I)                         M17.1213
  504 FORMAT( I10, 2F30.10 )                                               15815
      IF ( I  .EQ.  1 )   GO TO 510                                     M12. 674
      IF ( IPNCH  .EQ.  0 )                                                15816
     1 WRITE (LUNIT7, 505)  CURRR(I), FLUX(I)                           M17.1214
  505 FORMAT ( 2E16.8 )                                                    15818
  510 CONTINUE                                                             15819
      WRITE (LUNIT6, 527)                                               M17.1215
  527 FORMAT ( 36X, 4H9999 )                                            M12. 676
      IF ( IPNCH  .EQ.  0 )                                             M12. 677
     1WRITE (LUNIT7, 531)                                               M17.1216
  531 FORMAT ( 12X, 4H9999 )                                            M12. 679
      IF ( FREQ  .EQ.  -1.0 )   GO TO 1029                              M12. 680
      WRITE(LUNIT6, 800)                                                M17.1217
  800 FORMAT( /,119H CHECK OF DERIVED CURVE BY INDEPENDENT REVERSE COMPU   15821
     1TATION.  ASSUMING SINUSOIDAL VOLTAGE (FLUX) AT LEVEL OF EACH POINT   15822
     2,   , /,  99H RMS CURRENT IS FOUND NUMERICALLY.  THIS CURVE SHOULD   15823
     3 BE EQUAL TO THE ORIGINAL I-V POINTS INPUTTED.     )                 15824
      WRITE(LUNIT6, 17)                                                 M17.1218
   17 FORMAT(2X, 3HROW, 5X, 15HCURRENT IN P.U. , 5X, 15HVOLTAGE IN P.U.)   15826
      I = 1                                                                15827
      BB = FLUX(1)                                                         15828
      DO 1024 L=2,N                                                        15829
      CC=0.0                                                               15830
      XX=FLUX(L)                                                           15831
      DO 1005 M=1,90                                                       15832
      V= XX * SSS(M)                                                       15833
      IF ( V .GT. BB)   GO TO 960                                       M19.2300
  940 J = I - 1                                                            15835
      BBB = FLUX(J)                                                        15836
      IF( BBB .LE. V )  GO TO 980                                          15837
      I = J                                                                15838
      BB = BBB                                                             15839
      GO TO 940                                                            15840
  960 I = I + 1                                                            15841
      BB = FLUX(I)                                                         15842
      IF( BB .LT. V )  GO TO 960                                           15843
  980 A1=(V-FLUX(I-1))*(CURRR(I)-CURRR(I-1))/(FLUX(I)-FLUX(I-1))           15844
      A2=(CURRR(I-1)+A1)**2                                                15845
      CC=CC+A2                                                             15846
 1005 CONTINUE                                                             15847
      C1=SQRTZ((CC-A2/2.0 )/90.0)*VBASE/(PBASE*1000.0)                  M18. 894
      V = FLUX(L) * OMEGAH / ( SQ2*VBASE )                                 15849
      WRITE(LUNIT6,1010)L,C1,V                                          M17.1219
 1010 FORMAT( I5, 2F20.8)                                                  15851
 1024 CONTINUE                                                             15852
 1029 WRITE (LUNIT6, 1031)                                              M17.1220
 1031 FORMAT ( //,1X)                                                   M15.1529
      GO TO 5                                                              15853
 2050 WRITE (KUNIT6, 2055)                                              M35.9008
 2055 FORMAT ( 45H+BLANK CARD TERMINATING ALL SATURATION CASES.    )    M12. 683
      CALL INTERP                                                       M35.9009
      LASTOV = NCHAIN                                                   M15.1530
      NCHAIN = 51                                                       M22.5970
      IF ( IPRSUP  .GE.  1 )                                            M32, 285
     1 WRITE ( LUNIT6, 4568 )                                           M32, 286
 4568 FORMAT ( 24H  "EXIT  MODULE OVER42." )                            M32, 287
      GO TO 99999                                                          15857
 9200 LASTOV = NCHAIN                                                   M15.1531
      NCHAIN = 51                                                       M15.1532
      LSTAT(18) = 42                                                       15859
      IF ( IPRSUP  .GE.  1 )                                            M32, 288
     1 WRITE ( LUNIT6, 4568 )                                           M32, 289
99999 RETURN                                                               15860
      END                                                                  15861
      SUBROUTINE HYSDAT                                                 M25.1232
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION B(50),H(50),ITOP(1,4),IBOT(1,4)                         M25.1234
C     SET THE TOP AND BOTTOM POINTERS.                                  M29.4123
      DATA ITOP/1,5,15,29/                                              M29.4124
      DATA IBOT/4,14,28,50/                                             M29.4125
C     SET THE B POINTS (KILOGAUSS)                                      M25.1236
      B(1)  =  -15.6                                                    M28.7050
      B(2)  =   14.6                                                    M28.7051
      B(3)  =   17.0                                                    M28.7052
      B(4)  =   17.1                                                    M28.7053
      B(5)  =  -16.5                                                    M28.7054
      B(6)  =  -15.63                                                   M28.7055
      B(7)  =  -14.4                                                    M28.7056
      B(8)  =  -11.2                                                    M28.7057
      B(9)  =    9.7                                                    M28.7058
      B(10) =   12.8                                                    M28.7059
      B(11) =   14.7                                                    M28.7060
      B(12) =   16.0                                                    M28.7061
      B(13) =   17.0                                                    M28.7062
      B(14) =   17.1                                                    M28.7063
      B(15) =  -16.6                                                    M28.7064
      B(16) =  -16.4                                                    M28.7065
      B(17) =  -15.9                                                    M28.7066
      B(18) =  -15.4                                                    M28.7067
      B(19) =  -14.2                                                    M28.7068
      B(20) =  -12.0                                                    M28.7069
      B(21) =    8.6                                                    M28.7070
      B(22) =   12.3                                                    M28.7071
      B(23) =   13.8                                                    M28.7072
      B(24) =   15.0                                                    M28.7073
      B(25) =   15.8                                                    M28.7074
      B(26) =   16.4                                                    M28.7075
      B(27) =   17.0                                                    M28.7076
      B(28) =   17.1                                                    M28.7077
      B(29) =  -16.6                                                    M28.7078
      B(30) =  -16.5                                                    M28.7079
      B(31) =  -16.15                                                   M28.7080
      B(32) =  -15.8                                                    M28.7081
      B(33) =  -15.5                                                    M28.7082
      B(34) =  -14.9                                                    M28.7083
      B(35) =  -14.2                                                    M28.7084
      B(36) =  -13.0                                                    M28.7085
      B(37) =  -11.0                                                    M28.7086
      B(38) =  - 8.0                                                    M28.7087
      B(39) =    5.35                                                   M28.7088
      B(40) =    7.4                                                    M28.7089
      B(41) =   10.0                                                    M28.7090
      B(42) =   12.0                                                    M28.7091
      B(43) =   13.0                                                    M28.7092
      B(44) =   14.0                                                    M28.7093
      B(45) =   14.9                                                    M28.7094
      B(46) =   15.6                                                    M28.7095
      B(47) =   16.1                                                    M28.7096
      B(48) =   16.6                                                    M28.7097
      B(49) =   17.0                                                    M28.7098
      B(50) =   17.1                                                    M28.7099
C     SET THE H POINTS (OERSTEDS)                                       M25.1242
      H( 1) =   .04                                                     M28.7100
      H( 2) =   .17                                                     M28.7101
      H( 3) =  1.60                                                     M28.7102
      H( 4) =  2.20                                                     M28.7103
      H( 5) =  -.40                                                     M28.7104
      H( 6) =  -.05                                                     M28.7105
      H( 7) =   .03                                                     M28.7106
      H( 8) =   .07                                                     M28.7107
      H( 9) =   .135                                                    M28.7108
      H(10) =   .21                                                     M28.7109
      H(11) =   .36                                                     M28.7110
      H(12) =   .665                                                    M28.7111
      H(13) =  1.60                                                     M28.7112
      H(14) =  2.20                                                     M28.7113
      H(15) =  -.60                                                     M28.7114
      H(16) =  -.30                                                     M28.7115
      H(17) =  -.10                                                     M28.7116
      H(18) =  -.02                                                     M28.7117
      H(19) =   .035                                                    M28.7118
      H(20) =   .066                                                    M28.7119
      H(21) =   .12                                                     M28.7120
      H(22) =   .19                                                     M28.7121
      H(23) =   .27                                                     M28.7122
      H(24) =   .40                                                     M28.7123
      H(25) =   .59                                                     M28.7124
      H(26) =   .92                                                     M28.7125
      H(27) =  1.60                                                     M28.7126
      H(28) =  2.20                                                     M28.7127
      H(29) =  -.60                                                     M28.7128
      H(30) =  -.40                                                     M28.7129
      H(31) =  -.18                                                     M28.7130
      H(32) =  -.08                                                     M28.7131
      H(33) =  -.03                                                     M28.7132
      H(34) =   .01                                                     M28.7133
      H(35) =   .035                                                    M28.7134
      H(36) =   .058                                                    M28.7135
      H(37) =   .07                                                     M28.7136
      H(38) =   .08                                                     M28.7137
      H(39) =   .10                                                     M28.7138
      H(40) =   .11                                                     M28.7139
      H(41) =   .14                                                     M28.7140
      H(42) =   .18                                                     M28.7141
      H(43) =   .218                                                    M28.7142
      H(44) =   .285                                                    M28.7143
      H(45) =   .39                                                     M28.7144
      H(46) =   .535                                                    M28.7145
      H(47) =   .70                                                     M28.7146
      H(48) =  1.00                                                     M28.7147
      H(49) =  1.60                                                     M28.7148
      H(50) =  2.20                                                     M28.7149
 3548 IF ( IPRSUP  .GE.  1 )                                            M25.1252
     1 WRITE (LUNIT6,90)                                                M25.1253
   90 FORMAT(1X,35HENTER ITYPE AND LEVEL IN 2I1 FORMAT)                 M25.1254
C     READ INPUT CARD USING CIMAGE.                                     M25.1255
      CALL CIMAGE                                                       M25.1256
      IF ( KOLBEG  .GT.  0 )   GO TO 3628                               M25.1257
      DECODE (80, 100, ABUFF(1) )   ITYPE, LEVEL, IPUNCH                M25.1258
  100 FORMAT ( 10I8 )                                                   M25.1259
      IF ( ITYPE  .NE.  0 )     GO TO 3645                              M25.1260
      IF ( LEVEL  .NE.  0 )     GO TO 3645                              M25.1261
      IF ( IPUNCH .NE.  0 )     GO TO 3645                              M25.1262
      WRITE (KUNIT6, 3622)                                              M35.9010
 3622 FORMAT ( 45H+BLANK CARD ENDING HYSTERESIS-CURVE REQUESTS.  )      M25.1264
      CALL INTERP                                                       M35.9011
      GO TO 9200                                                        M25.1265
 3628 NFRFLD = 3                                                        M25.1266
      CALL FREFLD ( VOLTBC(1) )                                         M31.6284
      ITYPE = VOLTBC(1)                                                 M31.6285
      LEVEL = VOLTBC(2)                                                 M31.6286
      IPUNCH = VOLTBC(3)                                                M31.6287
 3645 WRITE (KUNIT6, 3648)  ITYPE, LEVEL, IPUNCH                        M35.9012
 3648 FORMAT (  37H+BEGIN B-H.  (ITYPE, LEVEL, IPUNCH) =,               M25.1272
     1           3I4  )                                                 M25.1273
      IF (ITYPE .EQ. 1) GO TO 125                                       M25.1274
      WRITE (LUNIT6,110) ITYPE                                          M25.1275
  110 FORMAT(1X,40HJOB IS BEING HALTED- THE VALUE OF ITYPE=,I3,         M25.1276
     1 2X,10HIS ILLEGAL)                                                M25.1277
      GO TO 3548                                                        M25.1278
  125 IF( (LEVEL .GE. 1) .AND. (LEVEL .LE. 4) ) GO TO 150               M25.1279
      WRITE (LUNIT6,130) LEVEL                                          M25.1280
  130 FORMAT(1X,40HJOB IS BEING HALTED- THE VALUE OF LEVEL=,I3,         M25.1281
     1 2X,10HIS ILLEGAL)                                                M25.1282
      GO TO 3548                                                        M25.1283
  150 IF ( IPRSUP  .GE.  1 )                                            M25.1284
     1 WRITE(LUNIT6,155)                                                M25.1285
  155 FORMAT(1X,51HENTER ISAT(AMPS) AND FLXSAT(WB-TURNS) 2F10.4 FORMAT) M25.1286
C     READ INPUT CARD USING CIMAGE                                      M25.1287
      CALL CIMAGE                                                       M25.1288
      IF ( KOLBEG  .GT.  0 )   GO TO 3682                               M25.1289
      DECODE (80, 160, ABUFF(1) )   CURSAT, FLXSAT                      M25.1290
  160 FORMAT ( 10E8.0 )                                                 M25.1291
      GO TO 3695                                                        M25.1292
 3682 NFRFLD = 1                                                        M25.1293
      CALL FREONE ( CURSAT )                                            M29.4129
      CALL FREONE ( FLXSAT )                                            M29.4130
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M25.1296
 3695 WRITE (KUNIT6, 3703)  CURSAT, FLXSAT                              M35.9013
 3703 FORMAT ( 17H+CURSAT, FLXSAT =,  2E13.4 )                          M25.1298
      N1=ITOP(ITYPE,LEVEL)                                              M25.1299
      N2=IBOT(ITYPE,LEVEL)                                              M25.1300
      SCALEF=FLXSAT/B(N2-1)                                             M25.1301
      SCALEI=CURSAT/H(N2-1)                                             M25.1302
C     RESCALE THE LOOP AND WRITE TO THE PUNCH UNIT.                     M25.1303
      WRITE (LUNIT6, 3718)                                              M25.1304
 3718 FORMAT ( /,                                                       M25.1305
     1  20X,  39HDERIVED TYPE-96 CHARACTERISTIC FOLLOWS:,               M25.1306
     2  /,  20X,  9X,   7HCURRENT,  12X,  4HFLUX  )                     M25.1307
      DO 200 I=N1,N2                                                    M25.1308
      H(I)=SCALEI*H(I)                                                  M25.1309
      B(I)=SCALEF*B(I)                                                  M25.1310
      WRITE (LUNIT6,175) H(I),B(I)                                      M25.1311
  175 FORMAT ( 20X,  2E16.7 )                                           M25.1312
      IF ( IPUNCH  .EQ.  0 )                                            M25.1313
     1 WRITE (LUNIT7, 178)  H(I), B(I)                                  M25.1314
  178 FORMAT ( 2E16.8 )                                                 M25.1315
  200 CONTINUE                                                          M25.1316
      D1 = 9999.                                                        M25.1317
      WRITE (LUNIT6, 175)  D1                                           M25.1318
      IF ( IPUNCH  .EQ.  0 )                                            M25.1319
     1 WRITE (LUNIT7, 178)  D1                                          M25.1320
      WRITE (LUNIT6, 175)                                               M25.1321
      GO TO 3548                                                        M25.1322
 9200 RETURN                                                            M25.1323
      END                                                               M25.1324
      SUBROUTINE ARRDAT                                                 M27.3449
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
C     PROGRAM TO PERFORM LEAST-SQUARES FIT OF STRAIGHT-LINE SEGMENTS TO M27.3451
C     A SET OF DATA POINTS. THE NUMBER OF SEGMENTS IS LIMITED TO MAXEXP M27.3452
      DIMENSION V(200),C(200),VC(200),CC(200),CK(200)                   M37.8040
      DIMENSION NT(100),S(200),BG(200)                                  M37.8041
      DIMENSION VS(200),ND(2)                                           M38.5476
      REAL*8        TEXT1, TEXT2, TEXT3, TEXT4                          M27.3458
      REAL*8        TEXT92, TEXT55, TEXTB, TEXTC, TEXTNA( 60 )          M37.8043
      DATA  TEXT1   /  4HGAPP    /                                      M27.3459
      DATA  TEXT2   /  4HED      /                                      M27.3460
      DATA  TEXT3   /  4HGAPL    /                                      M27.3461
      DATA  TEXT4   /  4HESS     /                                      M27.3462
      DATA  TEXTB   /  6HBRANCH  /                                      M37.8044
      DATA  TEXT92  /  2H92      /                                      M37.8045
      DATA  TEXT55  /  5H5555.   /                                      M37.8046
C     DEFINE CONSTANTS************************************************* M37.8047
      MAXEXP = 100                                                              
      NBRAN = 60                                                                
      DERLIM = .05                                                              
      AINF = ALOG1Z( FLTINF )                                                   
      JCASE = 0                                                         M27.3465
 1200 ICAR = 0                                                          M37.8051
      NDAT = 0                                                          M37.8052
      CALL COPYA( BLANK, TEXTNA( 1 ), NBRAN )                           M37.8053
 1202 CONTINUE                                                          M37.8054
C     READ INPUT CARD USING CIMAGE                                      M37.8055
      CALL CIMAGE                                                       M27.3468
C     CHECK FOR "BRANCH" REQUEST                                        M37.8056
      DECODE( 80, 1208, ABUFF(1) ) TEXTC                                M37.8057
 1208 FORMAT( A6 )                                                      M37.8058
      IF( TEXTC .NE. TEXTB )  GO TO 1212                                M37.8059
C     PROCESS TERMINAL NODES INPUT************************************* M37.8060
      NKC = NDAT + 12                                                   M37.8061
      IF( NKC .LE. NBRAN )  GO TO 1308                                  M37.8062
      LSTAT( 19 ) = 1308                                                M38.5478
      WRITE( LUNIT6, 1302 ) NKC, NBRAN                                  M37.8064
 1302 FORMAT(/, 31H REQUIRED NUMBER OF NODES EQUAL,                     M37.8065
     1 I10, 36HIS GREATER THAN MAXIMUM WHICH EQUALS,                    M37.8066
     2 I10 )                                                            M37.8067
      GO TO 9200                                                        M37.8068
 1308 CONTINUE                                                          M37.8069
      NK1 = NDAT + 1                                                    M37.8070
      NDAT = NDAT + 12                                                  M37.8071
      DECODE( 80, 1304, ABUFF(1) ) (TEXTNA(JV), JV = NK1, NDAT )        M37.8072
 1304 FORMAT( 8X, 12A6 )                                                M37.8073
      WRITE( KUNIT6, 1305 )                                             M37.8074
 1305 FORMAT(  27H+BUS NAMES FOR EACH PHASE.    )                       M37.8075
      GO TO 1202                                                        M37.8076
C     PROCESS MISCELLANEOUS DATA CARD********************************** M37.8077
 1212 CONTINUE                                                          M37.8078
      DECODE(80, 1220, ABUFF(1)) NEXP,IPHASE,D66,IPRZNO,VREF,VFLASH     M38.5479
 1220 FORMAT( 2I12, E12.0, I12, 2E12.0 )                                M38.5480
      IF( NEXP .NE. 0 .OR. IPHASE .NE. 0 )  GO  TO  1232                M28.7152
      WRITE (KUNIT6, 1226)                                              M35.9014
 1226 FORMAT (  32H+BLANK CARD ENDS ARRESTER CASES.   )                 M27.3474
      GO TO 9900                                                        M27.3475
 1232 WRITE (KUNIT6, 1235) NEXP,IPHASE,IPRZNO,VREF,VFLASH               M38.5481
 1235 FORMAT( 10H+ARRESTER., 3I4, 2E12.3 )                              M38.5482
      IF ( IPRZNO  .GE.  3 )                                            M27.3478
     1 WRITE (LUNIT6, 1240)                                             M27.3479
 1240 FORMAT ( //,  22H BEGIN NEXT DATA CASE.,                          M27.3480
     1              35H **********  **********  **********,             M27.3481
     2              12H  **********     )                               M27.3482
      JCASE = JCASE + 1                                                 M27.3483
      IFIT = 1                                                          M37.8083
      IF( NEXP .GT. 0  )  GO   TO  1260                                 M37.8084
      IFIT = -1                                                         M37.8085
 1260 IF( VREF .GT. 0.0 )  GO  TO  1280                                 M38.5483
      VREF = 0.0                                                        M37.8086
 1280 IF( IPHASE  .LE.  0  )      IPHASE = 1                                    
      JKP = 0                                                           M38.5485
      ND( 2 ) = 0                                                       M38.5486
C     READ INPUT CARD USING CIMAGE.                                     M27.3514
      CALL CIMAGE                                                       M27.3515
      DECODE (80, 1540, ABUFF(1))  A1, A2, A3, A4, A5, AMIN             M27.3516
      WRITE (KUNIT6, 1324)  A1, A2, A3, A4                              M35.9016
 1324 FORMAT (   9H+RATINGS.,  4E10.2  )                                M27.3518
      IF( A1  .GT. 0.0  )    GO  TO  1340                               M27.3519
      LSTAT(19) = 1328                                                  M27.3520
      WRITE (LUNIT6, 1328)  A1                                          M27.3521
 1328 FORMAT ( /,  35H ILLEGAL NON-POSITIVE  "A1"  EQUALS,              M27.3522
     1  E15.5  )                                                        M27.3523
      GO TO 9200                                                        M27.3524
 1340 IF( A2  .GT.  0.0  )   GO   TO  1360                              M27.3525
      LSTAT(19) = 1348                                                  M27.3526
      WRITE (LUNIT6, 1348)  A2                                          M27.3527
 1348 FORMAT ( /,  35H ILLEGAL NON-POSITIVE  "A2"  EQUALS,              M27.3528
     1  E15.5  )                                                        M27.3529
      GO TO 9200                                                        M27.3530
 1360 IF( A3  .GT.  0.0  )   GO   TO   1380                             M27.3531
      LSTAT(19) = 1366                                                  M27.3532
      WRITE (LUNIT6, 1366)  A3                                          M27.3533
 1366 FORMAT ( /,  35H ILLEGAL NON-POSITIVE  "A3"  EQUALS,              M27.3534
     1  E15.5  )                                                        M27.3535
      GO TO 9200                                                        M27.3536
 1380 IF( A4  .GT.  0.0  )   GO   TO   1400                             M27.3537
      LSTAT(19) = 1387                                                  M27.3538
      WRITE (LUNIT6, 1387)  A4                                          M27.3539
 1387 FORMAT ( /,  35H ILLEGAL NON-POSITIVE  "A4"  EQUALS,              M27.3540
     1  E15.5  )                                                        M27.3541
      GO TO 9200                                                        M27.3542
 1400 IF( AMIN .GT. 0.0  )   GO  TO  1420                               M27.3543
      LSTAT(19) = 1402                                                  M27.3544
      WRITE (LUNIT6, 1402)  AMIN                                        M27.3545
 1402 FORMAT ( /,  37H ILLEGAL NON-POSITIVE  "AMIN"  EQUALS,            M27.3546
     1  E15.5  )                                                        M27.3547
      GO TO 9200                                                        M27.3548
 1420 IF( A5 .EQ. 0.0  )   A5 = 1.0                                     M27.3549
      IF( VREF .GT. 0.0 )  GO  TO  1421                                 M28.7158
      VREF = 2.0 * A2                                                   M37.8088
 1421 IF( A5 .EQ. 1.0  .AND.  VFLASH .EQ. 0.0 )                         M37.8089
     1 GO TO 1425                                                       M37.8090
      IF( A5 .NE. 1.0  .AND.  VFLASH .GT. 0.0 )                         M37.8091
     1 GO TO 1425                                                       M37.8092
      LSTAT( 19 ) = 1421                                                M37.8093
      WRITE( LUNIT6, 1422 ) A5, VFLASH                                  M37.8094
 1422 FORMAT( /, 34H ILLEGAL "A5"-"VFLASH" COMBINATION,                 M37.8095
     1 2E15.5 )                                                         M37.8096
      GO TO 9200                                                        M37.8097
 1425 IF( IPRZNO  .LT. 3  )   GO  TO  1451                              M37.8098
      WRITE( LUNIT6, 1320 ) NEXP, VREF, IPHASE, VFLASH                  M37.8099
 1320 FORMAT( 5X, 19H NUMBER OF SEGMENTS,                               M28.7163
     1        5X, 18H REFERENCE VOLTAGE,                                M28.7164
     2        5X, 17H NUMBER OF PHASES,                                 M37.8100
     3        5X, 18H FLASHOVER VOLTAGE, /,                             M37.8101
     4        5X, I19, 5X, E18.8, 5X, I17, 5X, E18.8 )                  M37.8102
      WRITE( LUNIT6, 1440 ) A1,A2,A3,A4,A5                              M28.7167
 1440 FORMAT ( 2X,                                                      M27.3552
     1  38HCONSTANTS FOR CONVERSION OF INPUT DATA    ,/,  7X,           M27.3553
     2  15HORIGINAL RATING,  12X,  10HNEW RATING,  4X,                  M27.3554
     3  18HVOLTAGE MULTIPLIER,  4X,                                     M27.3555
     4  18HCURRENT MULTIPLIER,  6X,                                     M27.3556
     5  24HADDITIONAL BLOCKS FACTOR   ,/,  4E22.8,  E30.8  )            M27.3557
 1451 AMIN = ALOG1Z ( AMIN * A4 )                                       M28.7168
      JD = 0                                                            M37.8103
      ALVREF = ALOG1Z( VREF )                                                   
      ALA5 = 0.0                                                        M38.5488
      IF( A5 .GT. 0.0 )   ALA5 = ALOG1Z( A5 )                           M38.5489
 1455 IF( IFIT .GT. 0 )  GO TO 1460                                             
      WRITE( LUNIT6, 1456 )   D66                                       M38.5491
 1456 FORMAT( 2X, 20H TOLERANCE "ERRLIM"., 5X, E15.6 )                  M38.5492
      IF( D66 .LE. 0.0 )   D66 = DERLIM                                 M38.5493
      ERRLIM = D66                                                      M38.5494
      NEXP = 1                                                          M37.8113
C     PROCESS CARDS WITH ARRESTER DATA********************************* M37.8114
 1460 IF( NEXP .LE. MAXEXP )  GO TO  1465                               M37.8115
      WRITE( LUNIT6, 3278 )   MAXEXP                                    M37.8116
      LSTAT( 19 ) = 1460                                                M37.8117
      GO TO 9200                                                        M37.8118
 1465 ICAR = 0                                                          M37.8119
      JKP = JKP + 1                                                     M27.3560
      DQ = ( A2 / A1 ) * A3                                             M37.8120
      J = 0                                                             M27.3563
      NK = 0                                                            M27.3564
 1480 I = 0                                                             M27.3565
      CK1 = 0.0                                                         M37.8121
      CI1 = 0.0                                                         M37.8122
      D = 0.0                                                           M27.3566
      E = 0.0                                                           M27.3567
      F = 0.0                                                           M27.3568
      G = 0.0                                                           M27.3569
      N6 = NK + 1                                                       M27.3570
C     READ INPUT CARD USING CIMAGE.                                     M27.3571
 1520 CALL CIMAGE                                                       M27.3572
      DECODE (80, 1540, ABUFF(1))  A, B                                 M27.3573
 1540 FORMAT ( 6E12.0 )                                                 M27.3574
      ICAR = ICAR + 1                                                   M27.3575
      IF( A .EQ. 0.0  .AND.  ICAR .GT. 2 )   GO  TO  1620               M37.8123
      WRITE (KUNIT6, 1546)  A, B                                        M35.9017
 1546 FORMAT (  13H+(I,V) POINT.,  2E15.6  )                            M27.3578
 1580 NK = NK + 1                                                       M27.3591
      IF( A .GT. CK1  .AND.  B .GT. CI1 )  GO TO  1590                  M37.8124
      WRITE( LUNIT6, 1587 ) A, B                                        M37.8125
 1587 FORMAT( /, 41H NONPOSITVE OR NONMOMTONIC "A" AND/OR "B",          M37.8126
     1 23H THE READ-IN VALUES ARE, 2E15.6 )                             M37.8127
      LSTAT( 19 ) = 1580                                                M37.8128
      GO TO 9200                                                        M37.8129
 1590 CK1 = A                                                           M37.8130
      CI1 = B                                                           M37.8131
      I = I + 1                                                         M27.3592
      A = A * A4                                                        M27.3593
      CC(NK) = A                                                        M27.3594
      VC(NK) = B *  DQ                                                  M27.3595
      A = ALOG1Z( A )                                                   M27.3596
      B = ALOG1Z( VC(NK) ) - ALVREF                                     M37.8132
      V(NK) = B                                                         M27.3598
      C(NK)= A                                                          M27.3599
      IF ( IPRZNO  .GE.  8 )                                            M28.7169
     1 WRITE (LUNIT6, 1600)  A, B                                       M37.8133
 1600 FORMAT (  5X, 4E25.10 )                                           M27.3602
      E = E + A                                                         M37.8134
      D = D + B                                                         M37.8135
      F = F + B * B                                                     M37.8136
      G = G + A * B                                                     M27.3606
      GO  TO  1520                                                      M27.3607
 1620 WRITE (KUNIT6, 1621)                                              M35.9018
 1621 FORMAT ( 32H+BLANK CARD ENDS CHARACTERISTIC.   )                  M35.9019
      CALL INTERP                                                       M35.9020
      IF ( IPRZNO  .GE.  2 )                                            M27.3609
     1 WRITE (LUNIT6, 1622)  ( C(IP), V(IP), IP=N6, NK )                M27.3610
 1622 FORMAT (                                                          M27.3611
     1  47H INPUT DATA AFTER CONVERSION TO A LOG-LOG PLANE,             M27.3612
     2  20H  ==================  ,/,  23X,  7HCURRENT,                  M27.3613
     3  18X,  7HVOLTAGE  ,/,  ( 5X, 2E25.15 ) )                         M27.3614
      IF( IFIT .LT. 0 )  GO TO 3100                                             
C     CODE FOR USER DEFINED SEGMENTS*********************************** M37.8138
      IF( IPRSUP .GT. 1 )   WRITE (LUNIT6, 1640) D,E,F,G                M27.3615
 1640 FORMAT ( 1X,  35HCONSTANTS FOR THE FITTING PROCEDURE   ,/,        M27.3616
     1  4H D =,  E16.7,  5X,  3HE =,  E16.7,  5X,                       M27.3617
     2   3HF =,  E16.7,  5X,  3HG =,  E16.7  )                          M27.3618
      J = J + 1                                                         M27.3619
      A = ( G - (D * E) / I ) / ( F - (D * D) / I )                     M37.8139
      JU = J + JD                                                       M27.3621
      S( JU ) = A                                                               
      BV = ( E - A * D ) / I                                                    
      BY = BV                                                           M38.5497
      BX = ABSZ( BY )                                                   M38.5498
      IF( ALA5 .EQ. 0.0 )   GO  TO  1642                                M38.5499
      BU = BV - A * ALA5                                                        
      IF( ABSZ( BU )  .GT.  BX )    BY = BU                             M38.5501
 1642 IF( ABSZ( BY )  .LE.  AINF )   GO  TO  1650                       M38.5502
      VCOR1 = AINF * BX / BV  -  BY                                     M38.5503
      VCORR = VCOR1 / A                                                 M38.5504
      DO 1645 KU = 1, NK                                                M38.5505
 1645 V( KU ) = V( KU ) - VCORR                                         M38.5506
      CREF = VREF                                                       M38.5507
      VREF = VREF * 10.0**VCORR                                         M38.5508
      WRITE( LUNIT6, 3354 )    CREF, VREF                               M38.5509
 3354 FORMAT( 2X,  47HVREFERENCE RESET TO PREVENT NUMERICAL OVERFLOW.,  M38.5510
     1 34H THE OLD AND NEW VALUES FOLLOW...., 2X, 2E15.6 )              M38.5511
      ALVREF = ALVREF + VCORR                                           M38.5512
      BV = BV + VCOR1                                                   M38.5513
      DO 1646 KU = 1, JU                                                M38.5514
 1646 BG( KU ) = BG( KU ) + VCOR1                                       M38.5515
      IF( JD .EQ. 0 )   GO  TO  1650                                    M38.5516
      DO 1648 KU = 1, JD                                                M38.5517
 1648 VS( KU ) = VS( KU ) - VCORR                                       M38.5518
 1650 BG( JU ) = BV                                                     M38.5519
      IF( IPRZNO  .GT.  1  )                                            M28.7170
     1 WRITE (LUNIT6, 1660)  J, I, S( JU ), BG( JU )                    M38.5520
 1660 FORMAT ( /,  27H CALCULATED VALUES.  J, I =,  2I5,                M27.3626
     1    5X,      15HS(JU), BG(JU) =,  2E20.10  )                      M27.3627
      NT( J ) = I                                                       M27.3628
      IF( J .LT. NEXP )    GO  TO  1480                                 M27.3629
      GO TO 1680                                                        M37.8141
C     START AUTOMATIC DETERMINATION OF SEGMENTS************************ M37.8142
 3100 JKB = N6 - 1                                                      M37.8143
      NEXP = 0                                                          M37.8144
 3200 JKB = JKB + 1                                                     M37.8145
      IF( JKB .GT. NK )  GO TO 1680                                     M37.8146
      IF( JKB .LT. NK )  GO TO 3250                                     M37.8147
      NT( J ) = NT( J ) - 1                                             M37.8148
      JKB = JKB -1                                                      M37.8149
 3250 J = J + 1                                                         M37.8150
      I1 = 1                                                            M37.8151
      NEXP = NEXP + 1                                                   M37.8152
      IF( NEXP .LE. MAXEXP )   GO  TO 3280                              M37.8153
      LSTAT( 19 ) = 3280                                                M37.8154
      WRITE( LUNIT6, 3278 )  MAXEXP                                     M37.8155
 3278 FORMAT( /, 47HTHE PRESENT CASE REQUIRES MORE THAN THE PRESENT,    M37.8156
     1  9H LIMIT OF, I4, 13H EXPONENTIALS  )                            M37.8157
      GO TO 9200                                                        M37.8158
 3280 JU = J + JD                                                       M37.8159
      E = C( JKB )                                                      M37.8160
      D = V( JKB )                                                      M37.8161
      F = D * D                                                         M37.8162
      G = E * D                                                         M37.8163
      IKB = JKB                                                         M37.8164
C     START INTERNAL FITTING LOOP************************************** M37.8165
 3300 IKB = IKB + 1                                                     M37.8166
      IF( IKB .LE. NK )  GO TO  3350                                    M37.8167
      NT( J ) = NT( J ) - 1                                             M37.8168
      GO TO 1680                                                        M37.8169
 3350 I1 = I1 + 1                                                       M37.8170
      A = C( IKB )                                                      M37.8171
      B = V( IKB )                                                      M37.8172
      E = E + A                                                         M37.8173
      D = D + B                                                         M37.8174
      F = F + B * B                                                     M37.8175
      G = G + A * B                                                     M37.8176
      D1 = D / I1                                                       M37.8177
      A = ( G - D1 * E ) / ( F - D1 * D )                               M37.8178
      BV = ( E - A * D ) / I1                                                   
      BY = BV                                                           M38.5522
      BX = ABSZ( BY )                                                   M38.5523
      IF( ALA5 .EQ. 0.0 )   GO  TO  3342                                M38.5524
      BU = BV - A * ALA5                                                        
      IF( ABSZ( BU )  .GT.  BX )    BY = BU                             M38.5526
 3342 IF( ABSZ( BY )  .LE.  AINF )   GO  TO  3352                       M38.5527
      VCOR1 = AINF * BX / BV  -  BY                                     M38.5528
      VCORR = VCOR1 / A                                                 M38.5529
      DO 3345 KU = 1, NK                                                M38.5530
 3345 V( KU ) = V( KU ) - VCORR                                         M38.5531
      CREF = VREF                                                       M38.5532
      VREF = VREF * 10.0**VCORR                                         M38.5533
      WRITE( LUNIT6, 3354 )  CREF, VREF                                 M38.5534
      ALVREF = ALVREF + VCORR                                           M38.5535
      BV = BV + VCOR1                                                   M38.5536
      DO 3346 KU = 1, JU                                                M38.5537
 3346 BG( KU ) = BG( KU ) + VCOR1                                       M38.5538
      IF( JD .EQ. 0 )   GO  TO  3352                                    M38.5539
      DO 3348 KU = 1, JD                                                M38.5540
 3348 VS( KU ) = VS( KU ) - VCORR                                       M38.5541
 3352 ALB = 10.0**BV                                                    M38.5542
C     CHECK LOCAL FITTING ERRORS AFTER EXPANSION*********************** M37.8181
      DO 3400 KS = JKB, IKB                                             M37.8182
      CK( KS ) = ALB * ( VC( KS ) / VREF )**A                                   
      ERR = ABSZ( CK( KS ) - CC( KS ) ) / CC( KS )                      M37.8184
      IF( ERR .GT. ERRLIM ) GO TO 3500                                  M37.8185
 3400 CONTINUE                                                          M37.8186
      BG( JU ) = BV                                                     M38.5543
      S( JU ) = A                                                       M37.8188
      NT( J ) = I1                                                      M37.8189
      IF( IPRZNO .GT. 1 )                                               M37.8190
     1 WRITE( LUNIT6, 1660 ) J, I1, S( JU ), BG( JU )                   M37.8191
      GO TO 3300                                                        M37.8192
C     REMOVE LAST POINT (ERROR TOO LARGE)****************************** M37.8193
 3500 JKB = IKB - 2                                                     M37.8194
      IF( NEXP .GT. 1 )  NT( J ) = NT( J ) - 1                          M37.8195
      GO TO 3200                                                        M37.8196
 1680 VMIN = ( AMIN - BG( JD+1 ) ) / S( JD+1 )                          M38.5544
      VS( JD+1 ) = VMIN                                                 M38.5545
      ND( JKP ) = NEXP                                                  M27.3632
      IF( NEXP .LT. 2 )   GO  TO  1720                                  M37.8197
      NB = NEXP - 1                                                     M37.8198
      DO 1700 I = 1, NB                                                 M27.3634
      K = I + JD                                                        M27.3635
      J = K + 1                                                         M27.3636
      A = ( BG(J) - BG(K)  ) / ( S(K) - S(J) )                          M37.8199
 1700 VS( J ) = A                                                       M38.5546
 1720 IF( IPRZNO  .LT.  2  )    GO  TO  1780                            M27.3639
      DO  1760  I = 1, NEXP                                             M27.3640
      JU = I + JD                                                       M27.3641
      WRITE (LUNIT6, 1740)  I, BG(JU), VS(JU)                           M37.8200
 1740 FORMAT ( I5, 4E20.10 )                                            M27.3643
 1760 CONTINUE                                                          M27.3644
 1780 WRITE (LUNIT6, 1800)                                              M27.3645
 1800 FORMAT ( /,  3X,  10H**********,                                  M27.3646
     1              30H  ERROR STATISTICS  **********    ,/,            M27.3647
     2         3X,   7HSEGMENT,  14X,  11HLOCAL ERROR,                  M27.3648
     3         8X,  17HACCUMULATED ERROR   )                            M27.3649
      JK = 0                                                            M27.3650
      B = 0.                                                            M27.3651
      DO  1840 J = 1, NEXP                                              M27.3652
      A = 0.0                                                           M27.3653
      JU = JD + J                                                       M27.3654
      BV = 10.0**BG( JU )                                               M38.5547
      I = NT( J )                                                       M27.3656
      KI = JK + 1                                                       M27.3657
      JK = JK + I                                                       M27.3658
      DO 1820 K = KI, JK                                                M37.8201
      CK( K ) = BV * ( VC( K ) / VREF )**S( JU )                        M38.5548
 1820 A = A + ( C(K)-S(JU)*V(K)-BG(JU) )**2                             M37.8203
      B = B + A                                                         M27.3662
 1840 WRITE (LUNIT6, 1860)   J, A, B                                    M37.8204
 1860 FORMAT ( I10, 2X, 2E20.10 )                                       M27.3664
      IF( IPRZNO  .LT.  1  )    GO  TO  1940                            M27.3665
      WRITE (LUNIT6, 1880)                                              M27.3666
 1880 FORMAT ( /,  35H  COMPARISON BETWEEN INPUT DATA AND,              M27.3667
     1             24H THE RECALCULATED POINTS  ,/,                     M27.3668
     2  8X,  13HINPUT VOLTAGE,  7X,  13HINPUT CURRENT,                  M27.3669
     3  2X,  18HCALCULATED CURRENT,  10H  &&&&&&&&    )                 M27.3670
      DO  1920 K = 1, JK                                                M27.3671
      WRITE (LUNIT6, 1900) VC(K),CC(K),CK(K)                            M27.3672
 1900 FORMAT ( 3E20.10 )                                                M27.3673
 1920 CONTINUE                                                          M27.3674
 1940 IF( A5 .EQ. 0.0 .OR.  A5  .EQ. 1.0 )  GO  TO  2080                M27.3675
      JD = JD + NEXP                                                    M37.8205
      IF( A5  .LT.  0.0  )   GO  TO  2020                               M27.3676
      IF ( IPRZNO  .GE.  1 )                                            M27.3677
     1 WRITE (LUNIT6, 1960)   A5                                        M27.3678
 1960 FORMAT ( /,                                                       M27.3679
     1  42H  REPROCESS INPUT DATA.  MULTIPLIER "A5" =, E20.10 )         M27.3680
      IF ( IPRZNO  .GE.  2 )                                            M27.3681
     1 WRITE (LUNIT6, 1622)                                             M27.3682
      JK = 0                                                            M27.3683
      JKP = JKP + 1                                                     M27.3685
      DO 2000 J = 1, NEXP                                               M27.3686
      I = NT(J)                                                         M27.3691
      KI = JK + 1                                                       M27.3692
      JK = JK + I                                                       M27.3693
      KP = 0                                                            M27.3694
      DO  1980  K = KI, JK                                              M37.8207
      KP = KP + 1                                                       M27.3696
      VC( K ) = VC( K ) * A5                                            M37.8208
      V( K ) = V( K ) + ALA5                                            M37.8209
 1980 CONTINUE                                                          M27.3707
      JU = J + JD                                                       M27.3709
      A = S( J )                                                        M37.8210
      S(JU) = A                                                         M27.3710
      BG( JU ) = BG( J ) - A * ALA5                                     M37.8211
      IF( IPRZNO .GT. 1 )                                               M28.7171
     1 WRITE (LUNIT6, 1660)   J, KP, S(JU), BG(JU)                      M37.8212
 2000 CONTINUE                                                          M27.3714
      A5 = 0.                                                           M27.3715
      GO  TO  1680                                                      M27.3716
 2020 CONTINUE                                                          M27.3717
      WRITE (LUNIT6, 2040) JCASE                                        M27.3718
 2040 FORMAT (    33H  PROCESS SECOND PART OF CASE NO.,  I6,            M27.3719
     1            20H   &&&&&&&&&&&&&&&&&   )                           M27.3720
C     READ INPUT CARD USING CIMAGE.                                     M27.3722
      CALL CIMAGE                                                       M27.3723
      DECODE (80, 1220, ABUFF(1))   NEXP                                M27.3724
      IFIT = 1                                                          M37.8213
      IF( NEXP .LE. MAXEXP )  GO  TO  2060                              M37.8214
      IFIT = -1                                                         M37.8215
 2060 A5 = 0.                                                           M27.3729
      GO  TO 1455                                                       M37.8216
 2080 CONTINUE                                                          M37.8217
      VFLASH = VFLASH / VREF                                            M37.8218
      IF( VFLASH .EQ. 0.0 )  VFLASH = -100.0                            M37.8219
 2220 WRITE (LUNIT6, 2240)                                              M27.3762
 2240 FORMAT ( //,  35H  **********   80-COLUMN CARD-IMAGE,             M27.3763
     1              33H LISTING OF UNIT-7 PUNCHED CARDS.,               M27.3764
     2              10H   *******     )                                 M27.3765
      WRITE (LUNIT6, 2243)                                              M27.3766
 2243 FORMAT ( 1X,                                                      M27.3767
     1  40H----------------------------------------,                    M27.3768
     2  40H----------------------------------------   )                 M27.3769
      WRITE (LUNIT6, 2245)  ( IP, IP=1, 8 )                             M27.3770
      N4 = 0                                                            M27.3771
      WRITE (LUNIT6, 2245)  ( N4, IP=1, 8 )                             M27.3772
 2245 FORMAT ( 1X,  8I10 )                                              M27.3773
      WRITE (LUNIT6, 2243)                                              M27.3774
      IF( A5 .EQ. 0. ) GO TO 2260                                       M27.3775
      WRITE (LUNIT6, 2248)  A2, A3, A4, TEXT3, TEXT4                    M27.3776
 2248 FORMAT (  11H C RATING =,  F9.2,  10H  V-MULT =,                  M27.3777
     1          E13.5,  10H  I-MULT =,  E13.5,  2X,  2A4 )              M27.3778
      WRITE (LUNIT7, 2252)  A2, A3, A4, TEXT3, TEXT4                    M27.3779
 2252 FORMAT (   10HC RATING =,  F9.2,  10H  V-MULT =,                  M27.3780
     1          E13.5,  10H  I-MULT =,  E13.5,  2X,  2A4 )              M27.3781
      GO TO 2320                                                        M27.3782
 2260 WRITE (LUNIT6, 2248) A2, A3, A4, TEXT1, TEXT2                     M27.3783
      WRITE (LUNIT7, 2252) A2, A3, A4, TEXT1, TEXT2                     M27.3784
 2320 CONTINUE                                                          M27.3785
      NKC = 2                                                           M37.8220
      DO  2440  K = 1, IPHASE                                           M27.3792
      IF( K .GT. 1 )  GO TO 2422                                        M37.8221
      WRITE( LUNIT6, 2262 ) TEXT92,TEXTNA(1),TEXTNA(2),TEXT55           M37.8222
 2262 FORMAT( 1X, A2, 2A6, 25X, A5 )                                    M37.8223
      WRITE( LUNIT7, 2263 ) TEXT92,TEXTNA(1),TEXTNA(2),TEXT55           M37.8224
 2263 FORMAT( A2, 2A6, 25X, A5 )                                        M37.8225
      WRITE (LUNIT6, 2341)                                              M37.8226
 2341 FORMAT ( 1X,  1HC, 12X,  12H  VREFERENCE,                         M37.8227
     1        15X,  10HVFLASHOVER   )                                   M37.8228
      WRITE( LUNIT6, 2381 ) VREF, VFLASH                                M37.8229
 2381 FORMAT( 1X,2E25.15 )                                              M37.8230
      WRITE (LUNIT7, 2343)                                              M37.8231
 2343 FORMAT (      1HC, 12X,  12H  VREFERENCE,                         M37.8232
     1        15X,  10HVFLASHOVER   )                                   M37.8233
      WRITE( LUNIT7, 2382 ) VREF, VFLASH                                M37.8234
 2382 FORMAT( 2E25.15 )                                                 M37.8235
      J = 0                                                             M37.8236
      WRITE (LUNIT6, 2340)                                              M37.8239
 2340 FORMAT ( 1X,  1HC, 12X,  12H  MULTIPLIER,                         M37.8240
     1        17X,  8HEXPONENT,  21X,  4HVMIN   )                       M37.8241
      WRITE (LUNIT7, 2344)                                              M37.8242
 2344 FORMAT (      1HC, 12X,  12H  MULTIPLIER,                         M37.8243
     1        17X,  8HEXPONENT,  21X,  4HVMIN   )                       M37.8244
      DO 2420 IJ = 1, JKP                                               M37.8245
      JU = J + 1                                                        M27.3796
      JK = J + ND( IJ )                                                 M38.5549
      DO 2380 I = JU, JK                                                M27.3798
      BV = 10.0**VS( I )                                                M38.5550
      BK = 10.0**BG( I )                                                M38.5551
      WRITE (LUNIT6, 2347)   BK, S( I ), BV                             M38.5552
 2347 FORMAT ( 1X,  3E25.15 )                                           M37.8248
      WRITE (LUNIT7, 2360)   BK, S( I ), BV                             M38.5553
 2360 FORMAT ( 3E25.15 )                                                M37.8250
 2380 CONTINUE                                                          M27.3803
      WRITE( LUNIT6, 2383 )                                             M28.7172
 2383 FORMAT ( 1X,  21X,  4H9999  )                                     M37.8251
      WRITE (LUNIT7, 2400)                                              M27.3806
 2400 FORMAT ( 21X,  4H9999  )                                          M37.8252
      J = JK                                                            M38.5554
 2420 CONTINUE                                                          M27.3808
      GO TO 2440                                                        M37.8255
 2422 CONTINUE                                                          M37.8256
       WRITE( LUNIT6, 2264 ) TEXT92,TEXTNA(NKC+1),TEXTNA(NKC+2),        M37.8257
     1 TEXTNA(1),TEXTNA(2),TEXT55                                       M37.8258
 2264 FORMAT( 1X, A2, 4A6, 13X, A5 )                                    M37.8259
      WRITE( LUNIT7, 2265 ) TEXT92,TEXTNA(NKC+1),TEXTNA(NKC+2),         M37.8260
     1 TEXTNA(1),TEXTNA(2),TEXT55                                       M37.8261
 2265 FORMAT( A2, 4A6, 13X, A5 )                                        M37.8262
      NKC = NKC + 2                                                     M37.8263
 2440 CONTINUE                                                          M27.3809
      WRITE (LUNIT6, 2243)                                              M27.3810
      WRITE (LUNIT6, 2447)                                              M27.3811
 2447 FORMAT ( /,  1X )                                                 M27.3812
      GO  TO  1200                                                      M27.3813
 9200 KILL = 1000                                                       M27.3814
 9900 RETURN                                                            M27.3815
      END                                                               M27.3816
      SUBROUTINE ZINOLD                                                 M36.3573
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED TO CONVERT OLD  ("M36." OR EARLIER VINTAGE )  M39.3422
C     ZINC OXIDE ARRESTER DATA INTO THE NEW FORM AS REQUIRED FOR THE    M39.3423
C     M37 (AND LATER EMTP VERSIONS).                                    M39.3424
C     THIS MODULE USES THE ANSI 77 FORTRAN FEATURES AND MAY, THEREFORE, M39.3425
C     NOT RUN ON SYSTEMS THAT DO NOT HAVE THOSE CAPABILITIES.           M39.3426
C     IT IS ASSUMED THAT THE DATA CASE TO BE PROCESSED IS A VALID ONE,  M39.3427
C     I.E., IT DID EXECUTE CORRECTLY ON THE OLDER PROGRAM VERSIONS      M39.3428
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION ZK(20)                                                  M39.3430
      CHARACTER*80 CBUFF, BBUFF(35)                                     M39.3431
      CHARACTER*30 CBLANK                                               M39.3432
      REAL*8       TEXT(14)                                             M39.3433
      WRITE (LUNIT6, 3408)                                              M39.3434
 3408 FORMAT ( 40H BEGIN EXECUTION OF "ZINOLD", TO CONVERT,             M39.3435
     1         29H FROM OLD TO NEW ZNO FORMATS.   )                     M39.3436
      IAREST = 0                                                                
      NOUP = NOUTPR                                                             
      NOUTPR = 1                                                                
      CBLANK = '                              '                         M39.3440
C     PROCESS BRANCH CARDS ******************************************** M39.3441
C     READ INPUT CARD USING CIMAGE                                      M39.3442
 100  CALL CIMAGE                                                       M39.3443
      DECODE( 80, 3417, ABUFF ) TEXT                                    M39.3444
 3417 FORMAT( 13A6, A2 )                                                M39.3445
      WRITE( CBUFF, 3417 ) TEXT                                                 
 3450 FORMAT( I2, 24X, E6.0 )                                           M39.3447
      IF( CBUFF(1:2) .NE. '92'  )  GO  TO  150                          M39.3448
      IF( CBUFF(28:32) .NE. '5555.'  .AND.  CBUFF(27:31) .NE. '5555.')  M39.3449
     1 GO  TO  150                                                              
      CBUFF( 27:32 ) = '      '                                                 
      CBUFF( 40:44 ) = '5555.'                                                  
      CBUFF( 15:20 ) = '      '                                                 
      CBUFF( 21:26 ) = '      '                                                 
 3418 FORMAT( A80 )                                                     M39.3455
      IAREST = IAREST + 1                                                       
      BBUFF( IAREST ) = CBUFF                                                   
      GO TO  100                                                                
 150  IF( CBUFF(1:6) .EQ. 'BLANK '  .OR.                                M39.3459
     1 CBUFF(1:30) .EQ. CBLANK )   THEN                                         
      IF( IAREST .GT. 0 )   GO  TO  200                                         
      ELSE                                                              M39.3462
      GO TO  100                                                                
      END IF                                                            M39.3464
      IF( IAREST .GT. 0 )   GO  TO  200                                         
      STOP                                                              M39.3466
C     PROCESS SWITCH DATA CARDS *************************************** M39.3467
C     READ CARDS USING CIMAGE                                           M39.3468
 200  CALL CIMAGE                                                       M39.3469
      DECODE( 80, 3417, ABUFF )   TEXT                                          
      WRITE( CBUFF, 3417 )  TEXT                                                
      IF( CBUFF(1:6) .EQ. 'BLANK '  .OR.                                M39.3472
     1 CBUFF(1:30) .EQ. CBLANK )   GO TO 300                                    
      GO TO 200                                                                 
C     PROCESS SOURCE CARDS ******************************************** M39.3475
C     READ CARDS USING CIMAGE                                           M39.3476
 300  CALL CIMAGE                                                       M39.3477
      DECODE( 80, 3417, ABUFF )  TEXT                                           
      WRITE( CBUFF, 3417 )  TEXT                                                
      IF( CBUFF(1:1) .EQ. '5' )   THEN                                          
C     READ CARDS USING CIMAGE                                           M39.3481
 340  CALL CIMAGE                                                       M39.3482
      DECODE( 80, 3417, ABUFF )  TEXT                                           
      WRITE( CBUFF, 3417 )  TEXT                                                
      IF( CBUFF(3:8) .EQ.  'FINISH' )  GO TO 300                                
      GO TO 340                                                                 
      ELSE IF( CBUFF(1:6) .EQ. 'BLANK '  .OR.                           M39.3487
     1 CBUFF(1:30) .EQ. CBLANK )    THEN                                        
      GO TO 400                                                                 
      ELSE IF( CBUFF(1:2) .EQ. '19' )   THEN                                    
      IBLANK = 0                                                                
C     READ CARDS USING CIMAGE                                           M39.3492
 350  CALL CIMAGE                                                       M39.3493
      DECODE( 80, 3417, ABUFF )  TEXT                                   M39.3494
      WRITE( CBUFF, 3417 )  TEXT                                                
      IF( CBUFF(1:6) .NE. 'BLANK ' .AND. CBUFF(1:30) .NE. CBLANK )      M39.3496
     1 GO TO 350                                                                
      IBLANK = IBLANK + 1                                               M39.3498
      IF( IBLANK .EQ. 1 )   GO  TO  350                                         
      END IF                                                            M39.3500
      GO TO 300                                                                 
 400  CONTINUE                                                          M39.3502
C     REMOVE INITIAL CONDITIONS, IF ANY ******************************* M39.3503
C     READ CARDS USING CIMAGE                                           M39.3504
      CALL CIMAGE                                                       M39.3505
      DECODE( 80, 3420, ABUFF )  ICOND                                  M39.3506
 3420 FORMAT( I2 )                                                      M39.3507
      IF( ICOND .GT. 1  .AND.  ICOND .LT. 6 )   GO TO 400                       
      IK = 0                                                                    
      GO TO 510                                                         M39.3510
C     PROCESS ARRESTER DATA ******************************************* M39.3511
C     READ CARDS USING CIMAGE                                           M39.3512
 500  CALL CIMAGE                                                       M39.3513
 510  CONTINUE                                                          M39.3514
      DECODE( 80, 3421, ABUFF )  IPHASE, ( ZK( K ), K = 1, 5 )          M39.3515
 3421 FORMAT( I8, 4E16.0, E8.0 )                                        M39.3516
      IK = IK + 1                                                       M39.3517
      WRITE( LUNIT7, 3418 ) BBUFF( IK )                                         
      IF( IPHASE .GT. 0 )   GO  TO  530                                         
      VREF = ZK( 5 )                                                            
      IF( VREF .EQ. 0.0 )    VREF = VREF1                               M39.3521
      VREF1 = VREF                                                              
      VFLASH = -100.0                                                           
      VZERO = ZK( 4 )                                                           
      WRITE( LUNIT7, 3422 ) VREF, VFLASH, VZERO                                 
 3422 FORMAT( 3E25.15 )                                                 M39.3526
      WRITE( LUNIT6, 3423 ) VREF, VFLASH, VZERO                         M39.3527
 3423 FORMAT( 1X, 3E25.15 )                                             M39.3528
      WRITE( LUNIT7, 3422 )   ( ZK( L ), L = 1, 3 )                             
      WRITE( LUNIT6, 3423 )   ( ZK( L ), L = 1, 3 )                     M39.3530
      WRITE( LUNIT7, 3424 )                                                     
 3424 FORMAT( 21X, 4H9999  )                                            M39.3532
      WRITE( LUNIT6, 3425 )                                             M39.3533
 3425 FORMAT( 1X, 21X, 4H9999  )                                        M39.3534
      GO TO 590                                                                 
 530  K2 = 4                                                                    
      DO 560 K = 1, 3                                                   M39.3537
      K1 = K2 + 1                                                       M39.3538
      K2 = K2 + 4                                                       M39.3539
C     READ CARDS USING CIMAGE                                           M39.3540
      CALL CIMAGE                                                       M39.3541
      DECODE( 80, 3421, ABUFF ) IPHASE, ( ZK( KI ), KI = K1, K2 )       M39.3542
      IF( K .EQ. 1  .OR.  K .EQ. 3 )  GO TO 555                                 
      GO TO 560                                                                 
 555  CALL CIMAGE                                                       M39.3545
      DECODE( 80, 3426, ABUFF ) N19                                     M39.3546
 3426 FORMAT( I16 )                                                     M39.3547
 560  CONTINUE                                                          M39.3548
C     READ CARDS USING CIMAGE                                           M39.3549
      CALL CIMAGE                                                       M39.3550
      DECODE( 80, 3427, ABUFF )   VREF                                  M39.3551
 3427 FORMAT( E16.0 )                                                   M39.3552
      VREF = VREF * 2.0                                                         
      VFLASH = ZK( 16 ) * VREF                                                  
      IF( VFLASH .GT. ( 1.5*VREF ) )  VFLASH = -100.                            
      IF( VFLASH .EQ. 0.0 )       VFLASH = -100.                                
      IF( ZK( 11 ) .EQ. ZK( 3 )  .AND.  ZK( 7 ) .EQ. ZK( 15 ) )         M39.3557
     1 VFLASH = -100.                                                           
      VZERO = ZK( 4 )                                                           
      WRITE( LUNIT7, 3422 )    VREF, VFLASH, VZERO                              
      WRITE( LUNIT6, 3423 )    VREF, VFLASH, VZERO                      M39.3561
      WRITE( LUNIT7, 3422 )    ( ZK( L ), L = 1, 3 )                            
      WRITE( LUNIT6, 3423 )    ( ZK( L ), L = 1, 3 )                    M39.3563
      IF( ZK( 7 ) .EQ. VSTART )  GO  TO  570                                    
      WRITE( LUNIT7, 3422 )    ( ZK( L ), L = 5, 7 )                            
      WRITE( LUNIT6, 3423 )    ( ZK( L ), L = 5, 7 )                    M39.3566
 570  WRITE( LUNIT7, 3424 )                                                     
      WRITE( LUNIT6, 3425 )                                             M39.3568
C     CHECK WHETHER GAPLESS ARRESTER ********************************** M39.3569
      IF( VFLASH .LT. 0.0 )   GO  TO  590                               M39.3570
      WRITE( LUNIT7, 3422 )    ( ZK( L ), L = 9, 11 )                           
      WRITE( LUNIT6, 3423 )    ( ZK( L ), L = 9, 11 )                   M39.3572
      IF( ZK( 11 ) .EQ. ZK( 15 ) )   GO  TO  580                                
      WRITE( LUNIT7, 3422 )    ( ZK( L ), L = 13, 15 )                          
      WRITE( LUNIT6, 3423 )    ( ZK( L ), L = 13, 15 )                  M39.3575
 580  WRITE( LUNIT7, 3424 )                                             M39.3576
      WRITE( LUNIT6, 3425 )                                             M39.3577
 590  IAREST = IAREST - 1                                               M39.3578
      IF( IAREST .GT. 0 )   GO TO 500                                           
C     CHECK FOR 'BEGIN NEW DATA CASE' ********************************* M39.3580
C     READ CARDS USING CIMAGE                                           M39.3581
 600  CALL CIMAGE                                                       M39.3582
      DECODE( 80, 3417, ABUFF )  TEXT                                   M39.3583
      WRITE( CBUFF, 3417 )  TEXT                                        M39.3584
      IF( CBUFF(1:7) .EQ. 'BEGIN N' )   GO TO  620                      M39.3585
      GO TO 600                                                                 
  620 NOUTPR = NOUP                                                             
      RETURN                                                            M39.3588
      END                                                               M36.3577
