      SUBROUTINE OVER14
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      EQUIVALENCE   ( MONCAR(1), KNT )                                          
      IF ( IPRSUP  .GE. 1 )                                             M32.4454
     1 WRITE ( LUNIT6, 4567 )                                           M32.4455
 4567 FORMAT ( 24H  "BEGIN MODULE OVER14." )                            M32.4456
C     DEFINE "KENTNB" AND "NBHDSW" VECTORS FOR MODULE "SWITCH"          M32.4457
C     OF OVERLAY 16 (CALLED BY "SUBTS1") VIA  "DO 2472":                M32.4458
 1310 N6 = 0                                                            M32.4459
      IF ( KSWTCH  .LE.  0 )   GO TO 2483                               M32.4460
      MSWTCH = LSWTCH * 3                                               M37.4735
      DO 2472  J=1, KSWTCH                                              M32.4461
      KENTNB(J) = N6 + 1                                                M32.4462
      K = KMSWIT(J)                                                     M32.4463
      NDX1 = LSWTCH + J                                                 M32.4464
      M = KMSWIT(NDX1)                                                  M32.4465
      DO 2461  I=1, KSWTCH                                              M32.4466
      IF ( I  .EQ.  J )   GO TO 2461                                    M32.4467
      NDX2 = LSWTCH + I                                                 M32.4468
      IF ( K  .NE.  KMSWIT(I)     .AND.                                 M32.4469
     1     K  .NE.  KMSWIT(NDX2)  .AND.                                 M32.4470
     2     M  .NE.  KMSWIT(I)     .AND.                                 M32.4471
     3     M  .NE.  KMSWIT(NDX2) )   GO TO 2461                         M32.4472
      N6 = N6 + 1                                                       M32.4473
      IF ( N6  .GT.  MSWTCH )  GO TO  1983                              M37.4736
      NBHDSW(N6) = I                                                    M32.4474
 2461 CONTINUE                                                          M32.4475
 2472 CONTINUE                                                          M32.4476
      KENTNB(KSWTCH+1) = N6 + 1                                         M32.4477
 2483 CONTINUE                                                          M33.1117
C     INITIALIZE COUNTERS FOR THE -666 BRANCHES    *   *   *   *   *   *M32.4479
      ISFD = 0                                                          M32.4480
      IBF = 0                                                           M32.4481
      IKF = 0                                                           M32.4482
      CALL MOVER0 ( FINIT(1), NTOT )                                       11673
      IF ( IPRSUP  .GT.  0 )                                               11674
     1 WRITE (LUNIT6, 4286)  INONL, NV, IBR, IT, NTOT, NENERG, KNT,     M17. 738
     2 FLTINF, FLZERO, OMEGA, DELTA2, DELTAT                               11676
 4286 FORMAT ( /,  44H VARIOUS SCALARS AT BEGINNING OF  'LAST14' .   ,/,M32.4483
     1 1X, 56H   INONL      NV     IBR      IT    NTOT  NENERG     KNT,    11678
     2 9X, 6HFLTINF, 9X, 6HFLZERO, 10X, 5HOMEGA, 9X, 6HDELTA2,             11679
     3 9X, 6HDELTAT, /,  1X, 7I8, 5E15.6  )                                11680
      I = INONL                                                            11681
      GO TO 73577                                                          11682
73571 IF( NLTYPE(I) .GT. 0 )  GO TO 73574                                  11683
      IF( NLTYPE(I) .NE. -97 )  GO TO 73572                                11684
      CURR(I) = 0.0                                                        11685
      IF( ANONL(I) .GE. 0.0 )  GO TO 73572                                 11686
      CURR(I) = 1.0                                                        11687
      ANONL(I) = -1.0 / FLTINF                                             11688
73572 K = NONLK(I)                                                         11689
      N1 = NONLM(I)                                                        11690
      M = IABS(N1)                                                         11691
      N15 = K                                                           M31.3048
      IF ( N15  .EQ.  1 )   N15 = M                                     M31.3049
      N16 = IABS ( KSSFRQ(N15) )                                        M31.3050
      OMEGA = TWOPI * SFREQ(N16)                                        M31.3051
      D1 = ( F(K) - F(M) ) / OMEGA                                         11694
      D2 = E(K) - E(M)                                                     11695
      IF ( NLTYPE(I)  .NE.  -96 )  GO TO 73584                          M25. 436
      N7 = NONLAD(I)                                                    M23.3394
      CURR(I) = CCHAR(N7+3)                                             M23.3395
      ANONL(I) = GSLOPE(N7)                                             M23.3396
      IF ( IPRSUP  .GE.  2 )                                            M23.3397
     1 WRITE (LUNIT6, 2620) I, N7, CURR(I), VNONL(I), ANONL(I)          M23.3398
 2620 FORMAT ( /,  21H TYPE-96 IN "OVER14".,                            M28.4659
     1  16H       I      N7,  21X, 4HCURR,  20X, 5HVNONL,               M28.4660
     2  20X, 5HANONL  ,/,  21X,  2I8,  3E25.15  )                       M28.4661
      GO TO 73585                                                       M23.3402
73584 IF ( NLTYPE(I)  .NE.  -98 )  GO TO 73574                          M23.3403
      NONLE(I) = -NONLE(I)                                              M04.   4
      J0 = NONLAD(I)                                                    M33.1118
      ANONL(I) = GSLOPE(J0) * ( D1/DELTA2 - D2 )                           11708
73585 K = NONLK(I)                                                      M23.3404
      M = IABS( NONLM(I) )                                                 11711
      FINIT(M) = FINIT(M) + ANONL(I)                                       11712
      FINIT(K) = FINIT(K) - ANONL(I)                                       11713
      IF( IPRSUP .GT. 0 )                                                  11714
     1 WRITE(LUNIT6, 594)  I, K, M, J0,  ( FINIT(J), J=1, NTOT )        M17. 739
  594 FORMAT( /,  38H FINIT(J), J=1, NTOT AT 594 OF MAIN14.   , 4I10,      11716
     1 /, ( 1X, 5E25.15 ) )                                                11717
73574 I = I - 1                                                            11718
73577 IF( I .GT. 0 )  GO TO 73571                                          11719
      IF( INONL .GT. 0   .AND.   IPRSUP .GT. 0 )                           11720
     1 WRITE (LUNIT6, 73578)  ( I, NONLK(I), NONLM(I), NLTYPE(I),       M17. 740
     2 NONLAD(I), NONLE(I), ILAST(I), VNONL(I), CURR(I),                   11722
     3 ANONL(I), VZERO(I), I=1, INONL )                                    11723
73578 FORMAT ( /,  59H NONLINEAR-ELEMENT TABLE BEFORE  (Y)  FORMATION, I   11724
     1N OVER14.  ,/,                                                       11725
     2 7X, 3HROW, 5X, 5HNONLK, 5X, 5HNONLM, 4X, 6HNLTYPE,                  11726
     3 4X, 6HNONLAD, 5X, 5HNONLE, 5X, 5HILAST, 10X, 5HVNONL,               11727
     4 11X, 4HCURR, 10X, 5HANONL, 10X, 5HVZERO, /,                         11728
     5 ( 7I10, 4E15.5 ) )                                                  11729
      CALL LAST14                                                       M32.4484
C     FOLLOWING 2 CARDS EXTRACTED FROM "LAST14" (NO OV16 USE):          M32.4485
      IF ( NUMSM  .GT.  0 )                                             M35.1409
     1 CALL PAST                                                        M32.4487
      LASTOV = NCHAIN                                                   M32.4488
      IF ( KILL  .GT.  0 )   GO TO 9600                                 M32.4489
      NCHAIN = NCHAIN + 1                                               M32.4490
C     SCAN BRANCH TABLES FOR THE -666 BRANCHES AND INITIALIZE BRANCH    M32.4491
      IF( IBR .LT. 2 ) GO TO 99999                                      M32.4492
C     VARIABLES    *   *   *   *   *   *   *   *   *   *   *   *   *   *M32.4493
      DO  401  KA = 2, IBR                                              M32.4494
         IF( LENGTH(KA) .NE. -666 ) GO TO 401                           M32.4495
         KB = KA - 1                                                    M32.4496
         IT2 = LENGTH(KB)                                               M32.4497
         IT2 = IABSZ( IT2 )                                             M32.4498
         IT2D = KA + IT2 - 1                                            M32.4499
         KC = 0                                                         M32.4500
         DO  400  KD = KB, IT2D                                         M32.4501
            KC = KC + 1                                                 M32.4502
            LFD = IABSZ( KBUS(KD) )                                     M32.4503
            MFD = IABSZ( MBUS(KD) )                                     M32.4504
  400       VOLT(KC) = E(MFD) - E(LFD)                                  M32.4505
         CALL BREQIV( IKF, ISFD, IBF )                                  M32.4506
  401 CONTINUE                                                          M32.4507
      GO TO 9800                                                        M32.4508
 1983 KILL = 1                                                          M37.4737
      WRITE ( LUNIT6, 1984 )                                            M37.4738
 1984 FORMAT ( 47H ------ SWITCH VECTOR NBHDSW OVERFLOW AT OVER14 ,     M37.4739
     1         50H TO SOLVE THE PROBLEM, JUST ENLARGE LIST 6 -----  )   M37.4740
      LSTAT(16) = 6                                                     M37.4741
 9600 LSTAT(18) = 14                                                    M32.4509
      NCHAIN = 51                                                       M32.4510
 9800 IF ( IPRSUP  .GE.  1 )                                            M32.4511
     1 WRITE (LUNIT6, 9817)                                             M32.4512
 9817 FORMAT ( 16H EXIT  "OVER14".   )                                  M32.4513
99999 RETURN                                                            M32.4514
      END                                                               M32.4515
      SUBROUTINE LAST14                                                 M32.4516
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION CBLHST(1)                                               M44. 621
      EQUIVALENCE  ( CNVHST(1),  CBLHST(1) )                            M44. 622
      DIMENSION INFDLI(1)                                               M44.1473
      EQUIVALENCE  ( NAMEBR(1),  INFDLI(1) )                                    
      DIMENSION  WK1(1)                                                 M44.1475
      EQUIVALENCE ( SEMAUX(1), WK1(1) )                                 M44.1476
      COMMON /FDQLCL/ KOFF1,KOFF2,KOFF3,KOFF4,KOFF5,KOFF6,KOFF7,KOFF8,  M44.1477
     1 KOFF9,KOFF10,KOFF13,KOFF14,KOFF15,KOFF16,KOFF17,KOFF18,          M44.1478
     2 KOFF19,KOFF20,KOFF21,KOFF22,KOFF23,KOFF24,KOFF25,INOFF1,INOFF2,  M44.1479
     3 INOFF3,INOFF4,INOFF5,NQTT, LCBL, LMODE, NQTW                     M44.1480
C     BEGINNING OF FORMATION OF THE REAL ADMITTANCE MATRIX (Y)          M32.4520
      ICUCPL = 0                                                        M44. 623
      JGLNN = 0                                                         M44. 624
      ISECTI = 400
      IF ( IPRSUP .GE. 1 )                                              M32.4521
     1 WRITE (LUNIT6, 2213) (KODSEM(I), I=1, IBR)                       M32.4522
 2213 FORMAT ( 45H TOP OF "LAST14".  KODSEM(1:IBR) FOLLOW  ....         M32.4523
     1   ,/,  ( 1X,  20I6 ) )                                           M32.4524
      IF ( IPRSUP  .GE.  5 )                                            M32.4525
     1 WRITE (LUNIT6, 2217)  ( INDHST(I), I=1, IBR )                    M32.4526
 2217 FORMAT ( 45H                   INDHST(1:IBR) FOLLOW  ....         M32.4527
     1   ,/,  ( 1X,  20I6 ) )                                           M32.4528
      KSS=LYMAT+1                                                               
      IALTER = 1                                                                
      L = 1                                                                     
C                                       ESTABLISHING L-TH Y-ROW FROM BRA   11730
  600 L=L+1                                                                     
      CALL MOVER0 ( F(1), NTOT )                                                
      K=1                                                                       
  620 N1=KBUS(K)                                                                
      IT2=LENGTH(K)                                                             
      IF ( IPRSUP  .GE.  6 )                                            M20.3914
     1 WRITE (LUNIT6, 607)  L, NTOT, K, N1, IT1, IT2, IBR, KSS          M32.4532
  607 FORMAT ( /,   8H AT 607.,   64H       L    NTOT       K      N1   M32.4533
     1  IT1     IT2     IBR     KSS      ,/,  8X,  8I8  )               M32.4534
      IF(N1.LT.0) GO TO 690                                                     
      IT2=IABS(IT2)                                                             
      IF(IT2.EQ.1) GO TO 650                                                    
      IT1=K                                                                     
      DO 630 I=1,IT2                                                            
      IF(KBUS(IT1).EQ.L) GO TO 700                                              
      IF(MBUS(IT1).EQ.L) GO TO 710                                              
      GO TO 630                                                                 
  700 ICHECK=0                                                                  
      GO TO 720                                                            11746
  710 ICHECK=1                                                                  
  720 I1 = I -1                                                                 
      N1=NR(IT1)                                                                
C                                          !>Column of branch IT1 being M44.1482
      DO 750  J0=1, IT2                                                         
      J = J0 -1                                                                 
      N2=K+J                                                                    
      IF(ICHECK.NE.0) GO TO 760                                                 
      N3=KBUS(N2)                                                               
      N4=MBUS(N2)                                                               
  730 IF(J.LE.I1) GO TO 770                                                     
      N2=NR(N2)+I1                                                              
  740 YX=TX(N2)                                                            11759
      IF( KODEBR(K) .GT. 0 )   YX = X(N2)                                  11760
      F(N3)=F(N3)+YX+C(N2)/2.0                                                  
      F(N4)=F(N4)-YX                                                            
      IF ( IPRSUP  .GE.  9 )                                            M32.4535
     1 WRITE (LUNIT6, 746)  N3, N4, N2, KODEBR(K), YX, C(N2),           M32.4536
     2                      F(N3), F(N4)                                M32.4537
  746 FORMAT (  39H COUPLED ELEM ADDS TO (Y).  N3, N4, N2,,             M32.4538
     1          37H KODEBR(K), YX, C(N2), F(N3), F(N4) =,               M32.4539
     2  4I10  ,/,  1X,  4E25.15  )                                      M32.4540
      GO TO 750                                                                 
  760 N3=MBUS(N2)                                                          11764
      N4=KBUS(N2)                                                          11765
      GO TO 730                                                            11766
  770 N2=N1+J                                                                   
      GO TO 740                                                            11768
  750 CONTINUE                                                             11769
  630 IT1=IT1+1                                                                 
  640 K=K+IT2                                                                   
      IF ( IPRSUP  .GE.  6 )                                            M20.3918
     1 WRITE (LUNIT6, 643)  K, IT2, IBR, KODSEM(K)                      M20.3919
  643 FORMAT ( /,  35H AT 643.   K, IT2, IBR, KODSEM(K) =,  4I8  )      M20.3920
      IF(K.LE.IBR) GO TO 620                                                    
      GO TO 780                                                            11773
  650 MABS=IABS(MBUS(K))                                                        
      IF(N1.EQ.L) GO TO 660                                                     
      IF(MABS.EQ.L) GO TO 670                                                   
      GO TO 640                                                            11777
  660 N4=MABS                                                                   
      GO TO 680                                                            11779
  670 N4=N1                                                                11780
  680 N3=NR(K)                                                                  
      N2=IABS(N3)                                                               
      if ( iprsup .ge. 3 )                                              VAX. 113
     1 write (*,*) ' Series R-L-C. N3, N2, X(N2), F(L), F(N4) =',       VAX. 114
     2                             N3, N2, X(N2), F(L), F(N4)           VAX. 115
      YX=X(N2)                                                                  
      IF(N3.GT.0) YX=1.0/YX                                                     
      F(L)=F(L)+YX                                                              
      F(N4)=F(N4)-YX                                                            
      IF(N3.GE.0)                                                          11787
     1 F(L)=F(L)+C(N2)/2.0                                                      
      if ( iprsup .ge. 3 )                                              VAX. 116
     1 write (*,*) ' After R-L-C.   F(L), F(N4) =',  F(L), F(N4)        VAX. 117
      GO TO 640                                                            11789
  690 IF ( IMODEL(K) .NE. -3 ) GO TO 3681                               M44.1483
C                                            For testing new logic whichM44.1486
 3681 IF ( IMODEL(K) .NE. -4 ) GO TO 3688                               M44.1487
      IOFF1 = 0                                                                 
      IOFF2 = IOFF1 + ISECTI                                                    
      IOFF3 = IOFF2 + ISECTI                                                    
      IOFF4 = IOFF3 + ISECTI                                                    
      IOFF5 = IOFF4 + ISECTI                                                    
      IOFF6 = IOFF5 + ISECTI                                                    
      IOFF7 = IOFF6 + ISECTI                                                    
      IOFF8 = IOFF7 + ISECTI 
      IF ( IOFF8 .LE. LYMAT ) GO TO 5337 
      WRITE(LUNIT6, 5338) LYMAT, IOFF8
 5338 FORMAT( ' IN OVER14, NEW MARTI LINE SOLUTION LOGIC OVERFLOWED STO'
     1, 'RAGE LIST NO. 5: LYMAT =', I5, ' NEEDED SPACE HERE IOFF8=',I5,
     2 '.',/, ' EXECUTION IS ABORTED, AND REDIMENSION WITH LARGER VALUE
     3OF LIST NO. 5 IS REQUIRED.'  )
      STOP
                                  
 5337 LCBL = 0                                                          M44.1503
      LMODE= 0                                                          M44.  17
 8893 KOFF21 = KOFF20 + NQTT                                                    
      KOFF22 = KOFF21 + LBRNCH                                                  
      KOFF23 = KOFF22 + NQTW                                                    
      KOFF24 = KOFF23 + NQTW                                                    
      KOFF25 = KOFF24 + 288                                                     
      KOFF11 = KOFF25 + 288 
                                                   
      IF ( KOFF11 .LT. LHIST) GO TO 5339
      WRITE (LUNIT6,5340)  LHIST, KOFF11
 5340 FORMAT( ' IN OVER14, NEW MARTI LINE SOLUTION LOGIC OVERFLOWED STO'
     1, 'RAGE LIST NO. 22: LHIST =', I5,' NEEDED SPACE HERE KOFF11=',I5,
     2 '.',/, ' EXECUTION IS ABORTED, AND REDIMENSION WITH LARGER VALUE
     3OF LIST NO. 22 IS REQUIRED.'  )
 5339 KQ = K                                                           
      IF ( L .GT. 2 ) GO TO 9743                                                
C                                                                       M44.1539
C        Evaluate integration constants of Q                            M44.1540
C                                                                       M44.1541
7896  KQ = K                                                            M44.1542
      NPHS = ABS(LENGTH(KQ))                                                    
      NPHS2 = NPHS*NPHS                                                         
      ICB=INFDLI(INOFF1+KQ)                                                     
      NQ1=INFDLI(INOFF2+KQ)                                                     
      NQ0=INFDLI(INOFF1+KQ)                                                     
      NNQ1=INFDLI(INOFF3+kQ)                                                    
      DO 103 IQ=1,NPHS2                                                         
      NTEQ = WK1(KOFF20+ICB)                                                    
      SUME = 0.D0                                                       M44.1551
      SUMD = SCONST(NQ1)                                                M44.1552
      IF ( IPRSUP .GT. 0 )                                              M44.1553
     1 write(*,4839)                                                    M44.1554
4839  FORMAT(45H  I  NTEQ  NNQ1  NNQ2  NNQ3   NQ4   NQ5   NQ6,          M44.1555
     142H        QK0(I)    Cj(ntermq)    Dj(ntermq),                    M44.1556
     242H    Ej(ntermq)      SUMdj(I)      SUMej(I))                    M44.1557
      JKL = 0                                                           M44. 627
      ICBSCN = LHIST + ICB                                                      
      NTICPL = CBLHST( ICBSCN )                                                 
      ICUCPL = ICUCPL + NTICPL                                                  
      ICUCPL  = ICUCPL * 3                                                      
      NK1 = LHIST + IFD                                                 M44. 632
     1    + ICUCPL - NTICPL * 3                                                 
      DO 100 I=1, NTEQ                                                          
      NQ2 = NQ1 + I                                                             
      NQ3 = NQ2 + NTEQ                                                          
      NNQ2 = NNQ1 + I                                                           
      NNQ3 = NNQ2 + NTEQ                                                        
      NQ4 = NNQ3 + NTEQ                                                         
      NQ5 = NNQ1 + 3 * NTEQ + 1                                                 
      NQ6 = NQ5 + 1                                                             
      IF ( SCONST(NQ2) .GE. 1.E+13 ) GO TO 1001                         M44. 634
      APIDT = SCONST(NQ3) * DELTAT                                      M44.1566
      CJ = DEXP(-APIDT)                                                         
      HJ = ( 1.D0 - CJ ) / APIDT                                        M44.1568
      APIDT = SCONST(NQ2) / SCONST(NQ3)                                 M44.1569
      DJ = APIDT * ( 1.D0 - HJ )                                                
      SCONST(NQ4) = - APIDT * ( CJ - HJ )                                       
      SCONST(NNQ2) = CJ                                                 M44.1572
      SCONST(NNQ3) = DJ                                                 M44.1573
      SUME        = SUME + SCONST(NQ4)                                          
      SCONST(NQ6) = SUME                                                M44.1575
      SUMD        = SUMD + DJ                                                   
      SCONST(NQ5) = SUMD                                                M44.1577
      GO TO 1002                                                        M44. 635
 1001 JKL = JKL + 1                                                     M44. 636
      IF (JKL .EQ. 2 ) GO TO 1004                                       M44. 637
      FAC1 = SCONST(NQ3)*DELTAT/1.E15                                           
      SCONST(N5) = EXPZ(-FAC1)                                                  
      DBLPR2 =  SCONST(N5)                                              M44. 640
      FAC3 = SCONST(NQ3+1) * DELTAT / 1.E15                                     
      FAC4 = SCONST(NQ3+1) / 1.E15                                              
      FAC5 = SCONST(NQ3) / 1.E15                                                
      CALL DCOSZ(FAC3, DBLPR3)                                                  
      CALL DSINZ(FAC3, DBLPR4)                                                  
      DBLPR1 = DBLPR2 * DBLPR3                                                  
      DBLPR2 = DBLPR2 * DBLPR4                                                  
      D18 = FAC4*FAC4 + FAC5*FAC5                                               
      NN10 = NK1  + 1                                                           
      NN11 = NN10 + 1                                                           
      NN12 = NN11 + 1                                                           
      NK1 = NK1 + 3                                                     M44. 652
      IF ( NK1 .GE. IHIST+LFD/2 )                                       M44. 653
     1 WRITE( LUNIT6 , 1005) NK1, IHIST, LFD                            M44. 654
 1005 FORMAT(28H Second part of CNVHST NK1= ,I4,                        M44. 655
     2       19H larger than IHIST ,I4,9H + LFD/2 , I4 )                M44. 656
      if ( nk1 .ge. IHIST+LFD/2 ) stop                                  M44. 657
      SK1R = SCONST(NQ2)/1.E+15                                         M44. 658
      SK1I = SCONST(NQ2+1)/1.E+15                                       M44. 659
      RLL = (  (UNITY - DBLPR1) * FAC5 + DBLPR2 * FAC4 )                M44. 660
      SLL = (  DBLPR2 * FAC5 - (UNITY - DBLPR1) * FAC4 )                M44. 661
      YLL = SK1R * RLL - SK1I * SLL                                             
      XLL = SK1R * DBLPR1 + SK1I * DBLPR2                               M44. 663
      SCONST(NNQ2) = 2*DBLPR1                                           M44. 664
      SCONST(NNQ3) = 2*SK1R - YLL/(DELTAT*D18)                                  
      SCONST(NQ4) = -2*( XLL - YLL/(DELTAT*D18) )                               
      SUMD        = SUMD + SCONST(NNQ3)                                         
      SCONST(NQ5) = SUMD                                                M44. 668
      SUME        = SUME + SCONST(NQ4)                                          
      SCONST(NQ6) = SUME                                                M44. 670
 1002 IF ( IPRSUP .GT. 0 )                                              M44.1578
     1 write(*,145) I,NTEQ,NNQ1,NNQ2,NNQ3,NQ4,NQ5,NQ6,                  M44.1579
     2          SCONST(NQ1),SCONST(NNQ2),SCONST(NNQ3),                  M44.1580
     3            SCONST(NQ4),SCONST(NQ5),SCONST(NQ6)                   M44.1581
145   FORMAT(1X,I2,3X,I3,2X,I4,2X,I4,2X,I4,2X,I4,2X,I4,2X,I4,           M44.1582
     1                                                6E14.5)           M44.1583
      IF ( JKL .EQ. 1 ) GO TO 100                                       M44. 671
 1004 JKL = 0                                                           M44. 672
100   CONTINUE                                                          M44.1584
      NNQ1 = NQ6                                                                
      NQ1 = NQ3 + 1                                                             
      ICB=ICB+1                                                         M44.1587
C     Q0(NQ0) = SCONST(NQ5)                                            !M44.1588
      WK1(KOFF22+NQ0) = SCONST(NQ5)                                             
      NQ0 = NQ0 + 1                                                     M44.1590
103   CONTINUE                                                          M44.1591
C                                                                       M44.1592
C        Evaluate equivalent admittance                                 M44.1593
C                                                                       M44.1594
102   ICB = INFDLI(INOFF1+K)                                                    
      ICU = INFDLI(INOFF4+KQ)                                                   
C      CALL QYQTR( NPHS, WK1(KOFF21+KQ),                                M44.1597
C     1            WK1(KOFF22+ICB), WK1(KOFF23+ICU) )                   M44.1598
C        This subroutine evaluates the matrix product Y = Q * D * Qt    M44.1599
C        where D is a diagonal matrix and Y will be symmetrical         M44.1600
C        All matrices are real.                                         M44.1601
      NNN1=NPHS*(NPHS+1)/2                                              M44.1602
      DO 1009 I=1,NNN1                                                  M44.1603
C      Y(I)=0.D0                                                        M44.1604
      WK1(KOFF23+ICU-1+I) = 0.0                                         M44.1605
1009  CONTINUE                                                          M44.1606
      IF ( IPRSUP .GT. 0 )                                              M44. 673
     1 write(*,4849)                                                    M44. 674
4849  FORMAT(30H  NNK   KM1    J   I   IJ   IK                          M44. 675
     142H       DYK-Y05           QJK           QIK,                    M44. 676
     228H             B   YEQ(KOFF23))                                  M44. 677
      DO 101 NNK=1,NPHS                                                 M44.1607
      KM1 = (NNK - 1 ) * NPHS                                           M44.1608
C      DYK = DY(NNK)                                                    M44.1609
      DYK = WK1(KOFF21+KQ-1+NNK)                                        M44.1610
      IJ = 0                                                            M44.1611
      DO 1029 J = 1, NPHS                                               M44.1612
      JK = KM1 + J                                                      M44.1613
C      QJK = Q(JK)                                                      M44.1614
      QJK = WK1(KOFF22+ICB-1+JK)                                        M44.1615
      DO 1039 I = 1 , J                                                 M44.1616
      IJ = IJ + 1                                                       M44.1617
      IK = KM1 + I                                                      M44.1618
C      QIK = Q(IK)                                                      M44.1619
      QIK = WK1(KOFF22+ICB-1+IK)                                        M44.1620
      B = QIK * QJK                                                     M44.1621
C      Y(IJ) = Y(IJ) + DYK * B                                          M44.1622
      WK1(KOFF23+ICU-1+IJ) = WK1(KOFF23+ICU-1+IJ) + DYK * B             M44.1623
      IF ( IPRSUP .GT. 0 )                                              M44. 678
     1 write(*,445)  NNK,KM1,J ,I,IJ ,IK,                               M44. 679
     2            DYK,QJK,QIK,B, WK1(KOFF23+ICU-1+IJ)                   M44. 680
 445   FORMAT(1X,I4,2X,I4,2X,I4,2X,I4,1X,I4,2X,I4,                      M44. 681
     1                                     5E14.5)                      M44. 682
1039  CONTINUE                                                          M44.1624
1029  CONTINUE                                                          M44.1625
101   CONTINUE                                                          M44.1626
9743  IF ( IPRSUP .GT. 0 )                                              M44.1640
     1 WRITE (*, *) ' K, IT2, L, N1, MBUS(K), IBR =',                   M44.1641
     2                K, IT2, L, N1, MBUS(K), IBR                       M44.1642
      IF ( IT2 .LT. 0 ) GO TO 682                                               
      IT2 = 1                                                                   
      IF ( -N1 .NE. L .AND.                                             M44.1645
     1     IABS ( MBUS(K) ) .NE. L ) GO TO 640                          M44.1646
      F(L) = F(L) + WK1(KOFF21+ICB)                                             
      GO TO 640                                                         M44.1648
  682 IT2 = -IT2                                                                
      IT1 = K - 1                                                               
      J0 = K                                                                    
      IADRS = INFDLI(INOFF4+kQ) - 1                                     M44.1652
      DO 683 IBI = 1,IT2                                                        
      IT1 = IT1 + 1                                                             
      IF ( -KBUS(IT1) .EQ. L ) GO TO 684                                M44.1655
      IF ( IABS ( MBUS(IT1) ) .EQ. L ) GO TO 685                        M44.1656
      GO TO 683                                                                 
  684 J0 = 0                                                                    
  685 IBRJ = K                                                                  
      DO 686 IBJ = 1,IT2                                                        
      IF ( J0 .EQ. 0 ) N1 = -KBUS(IBRJ)                                         
      IF ( J0 .NE. 0 ) N1 = IABS(MBUS(IBRJ) )                           M44.1662
      IF ( IBI .LT. IBJ )                                               M44.1663
     1 IYEQ = IADRS + IBI + ( IBJ - 1 ) * IBJ/2                         M44.1664
      IF ( IBI .GE. IBJ )                                               M44.1665
     1 IYEQ = IADRS + IBJ + ( IBI - 1 ) * IBI/2                         M44.1666
      F(N1) = F(N1) + WK1(KOFF23+IYEQ)                                  M44.1667
      IBRJ = IBRJ + 1                                                   M44.1668
  686 CONTINUE                                                          M44.1669
  683 CONTINUE                                                          M44.1670
      IT1 = IT1 + 1                                                             
      GO TO 640                                                         M44.1673
C                                                                       M44.1674
 3688 YX = ABSZ ( CI(K) )                                                       
C      IF (KODSEM(K) .NE. 0  .AND. IMODEL(K) .NE. -2) GO TO 6920        M31.3052
      IF (KODSEM(K) .NE. 0  .AND. IMODEL(K) .GE. 0) GO TO 6920          M44.1676
      IT2 = -IT2                                                                
      IF (IT2 .LT. 0 ) IT2 = 1                                                  
      N2 = K + IT2 - 1                                                          
      DO 21690  I = K, N2                                                       
      IF ( CK(I) .GE. 0.0 )  GO TO 21690                                        
      J = CIK(I)                                                        M29.1953
      CI(I) = CI(I) / ETA(J)                                            M29.1954
21690 CONTINUE                                                          M29.1955
      IT1=K                                                                     
      J0=K                                                                      
      DO 693 I=1,IT2                                                            
      IF((-KBUS(IT1)).EQ.L) GO TO 694                                      11813
      IF(MBUS(IT1).EQ.L) GO TO 695                                         11814
      GO TO 693                                                                 
  694 J0=0                                                                      
  695 CONTINUE                                                             11817
      N2=K                                                                      
      DO 698 N4=1,IT2                                                           
      IF(J0.EQ.0) GO TO 696                                                     
      N1=MBUS(N2)                                                               
      GO TO 697                                                            11822
  696 N1=-KBUS(N2)                                                              
  697 YX = 0.0                                                          M31.3054
      KJ=K                                                              M31.3055
      IJ=LITYPE(K)-1                                                    M31.3056
      N4J=IJ+N4                                                         M31.3057
      IJ=IJ+I                                                           M31.3058
      DO 1696 J=1,IT2                                                   M31.3059
      YX=YX+QFD(IJ)*ABSZ(CI(KJ))*QFD(N4J)                                       
      IF ( IPRSUP .GE. 3 )                                              M31.3061
     1 WRITE (LUNIT6, 2696) J, IT2, KJ,CI(KJ), N4J, QFD(N4J),IJ,        M31.3062
     2                     QFD(IJ),YX                                   M31.3063
 2696 FORMAT (10H AT  2696 , 10X,101H       J     IT2      KJ        CI(M31.3064
     1KJ)       N4J       QFD(N4J)      IJ        QFD(IJ)             YXM31.3065
     2,/, 20X, 3I8,E15.6,I8,E15.6,I8,2E15.6)                            M31.3066
      IJ=IJ+IT2                                                         M31.3067
      N4J=N4J+IT2                                                       M31.3068
 1696 KJ=KJ+1                                                           M31.3069
      PIT2 = IT2                                                        M34. 145
      YX = YX * PIT2                                                    M34. 146
      F(N1)=F(N1)+YX                                                    M29.1958
      IF ( IPRSUP .GE. 3 )                                              M29.1959
     1 WRITE (LUNIT6, 1697) N1, N2, N3, N4, YX, F(N1)                   M29.1960
 1697 FORMAT (64H      N1      N2      N3      N4             YX        M29.1961
     1    F(N1),/, 4I8, E15.6,2X,E15.6)                                 M29.1962
  698 N2=N2+1                                                                   
      IF ( IPRSUP .GE. 3 )                                              M29.1963
     1 WRITE (LUNIT6, 2697) CI1, L, F(L)                                M29.1964
 2697 FORMAT ( 29H CI1, L, AND F(L) AT 2697 ARE, 5X, E15.6,I8,E15.6)    M29.1965
  693 IT1=IT1+1                                                                 
C     TO RECOVER THE ARRAY CI                                           M29.1966
      N2 = K + IT2 - 1                                                  M29.1967
      DO 31690  I= K, N2                                                M29.1968
      IF ( CK(I) .GE. 0.0)  GO TO 31690                                 M29.1969
      J = CIK(I)                                                        M29.1970
      CI(I) = CI(I) * ETA(J)                                            M29.1971
31690 CONTINUE                                                          M29.1972
      GO TO 640                                                            11828
 6920 IT2 = IABS(KODEBR(K))                                             M23.3406
      N1 = CIK(K)                                                       M23.3407
      DO 13200 I=1, IT2                                                 M23.3409
      IF (N1 .GT. 0) CALL MOVER0(VOLT(1), IT2)                          M30. 390
      N9 = K + I - 1                                                    M23.3410
      IF (-KBUS(N9) .NE. L) GO TO 13010                                 M23.3411
      N3 = 1                                                            M23.3412
      D1 = 1.0                                                          M23.3413
      GO TO 13020                                                       M23.3414
13010 IF (IABS(MBUS(N9)) .NE. L) GO TO 13200                            M23.3415
      N3 = - 1                                                          M23.3416
      D1 = 1.0                                                          M23.3417
      IF (KODSEM(K) .LT. 0) D1 = -1.0                                   M23.3418
13020 KBR = K                                                           M23.3419
13030 N4 = ABSZ(CKI(KBR)) - 1.0                                         M23.3420
      N5 = N4 / IT2 + 1                                                 M23.3421
      N4 = N4 - (N5 - 1) * IT2 + 1                                      M23.3422
      IF (N1 .LT. 0 .AND. N4 .NE. I) GO TO 13140                        M23.3423
C      IF (KODSEM(KBR) .GT. 0 .AND. IMODEL(KBR) .NE. -2)                M31.3071
       IF (KODSEM(K) .NE. 0  .AND. IMODEL(K) .GE. 0)                    M44.1677
     1  GO TO 13040                                                     M31.3072
      N6 = - KODSEM(KBR) + 2                                            M23.3425
      D2 = SCONST(N6 - 2) + SCONST(N6 - 1)                              M23.3426
      GO TO 13050                                                       M23.3427
13040 N6 = KODSEM(KBR)                                                  M23.3428
      D2 = SCONST(N6)                                                   M23.3429
      N7 = CI(KBR)                                                      M23.3430
      N6 = N6 - N7 - N7 + 1                                             M23.3431
      IF (N7 .GT. 0) N6 = N6 + 7 * N7                                   M23.3432
13050 N7 = CK(KBR)                                                      M23.3433
      IF (N7) 13060, 13110, 13070                                       M23.3434
13060 D2 = D2 + SCONST(N6 + 1)                                          M23.3435
      GO TO 13110                                                       M23.3436
13070 N7 = N6 + 4 * (N7 - 1)                                            M23.3437
      DO 13100 J=N6, N7, 4                                              M23.3438
      IF (SCONST(J)) 13100, 13090, 13080                                M23.3439
13080 D2 = D2 + SCONST(J + 2)                                           M23.3440
13090 D2 = D2 + SCONST(J + 2)                                           M23.3441
13100 CONTINUE                                                          M23.3442
13110 IF (IPRSUP .GE. 6)                                                M23.3443
     1     WRITE (LUNIT6, 13115) K, L, I, N9, N3, N4, N5, N6, N7, D2    M23.3444
13115 FORMAT(100H AT 13110.      K       L       I      N9      N3      M23.3445
     1N4      N5      N6      N7                D2   /,10X,9I8,5X,E15.7)M23.3446
      IF (N1 .LT. 0) GO TO 13120                                        M23.3447
C  VOLT(.) IS THE I TH ROW OF (QFD) * (Y-MODAL)                         M23.3448
      N6 = (N4 - 1) * IT2 + I + N1 - 1                                  M23.3449
      VOLT(N5) = VOLT(N5) + QFD(N6) * D1 * D2                           M23.3450
      GO TO 13140                                                       M23.3451
13120 N6 = K + N5 - 1                                                   M23.3452
C  ADD(SUBTRACT) I TH ROW OF Y-PHASE TO(FROM) THE L-TH ROW OF (Y-TOTAL) M23.3453
      IF (N3 .GT. 0) GO TO 13130                                        M23.3454
      N6 = IABS(MBUS(N6))                                               M23.3455
      GO TO 13140                                                       M23.3456
13130 N6 = - KBUS(N6)                                                   M23.3457
      E(N6) = E(N6) + D1 * D2                                           M23.3458
13140 KBR = KBR + 1                                                     M23.3459
      IF (CKI(KBR - 1) .GT. 0.0) GO TO 13030                            M23.3460
      IF (N1 .LT. 0) GO TO 13190                                        M23.3461
      N4 = K + IT2 - 1                                                  M23.3462
      DO 13180 KBR=K, N4                                                M23.3463
      IF (N3 .GT. 0) GO TO 13150                                        M23.3464
      N5 = IABS(MBUS(KBR))                                              M23.3465
      GO TO 13160                                                       M23.3466
13150 N5 = - KBUS(KBR)                                                  M23.3467
13160 N6 = N1 + KBR - K                                                 M23.3468
      D1 = 0.0                                                          M23.3469
      DO 13170 J=1, IT2                                                 M23.3470
      D1 = D1 + VOLT(J) * QFD(N6)                                       M23.3471
      N6 = N6 + IT2                                                     M23.3472
13170 CONTINUE                                                          M23.3473
      IF (IPRSUP .GT. 5)                                                M23.3474
     1     WRITE (LUNIT6, 13175) N3, N5, KBR, F(N5), D1                 M23.3475
13175 FORMAT(17H AT 13170.  N3 = ,I5,5X,5HN5 = ,I5,5X,6HKBR = ,I5,5X,   M23.3476
     1     8HF(N5) = ,E15.7,5X,5HD1 = ,E15.7   )                        M23.3477
      F(N5) = F(N5) + D1                                                M23.3478
13180 CONTINUE                                                          M23.3479
13190 IF (N3 .GT. 0) GO TO 13010                                        M23.3480
13200 CONTINUE                                                          M23.3481
       IT2 = 1                                                          M23.3482
13210 IF (CKI(K) .LT. 0.0) GO TO 640                                    M23.3483
      K = K + 1                                                         M23.3484
      GO TO 13210                                                       M23.3485
  780 IF( NUM99 .LE. 0 )  GO TO 2801                                       11864
      DO 2793  I=1, INONL                                                  11865
      IF ( NLTYPE(I) .GT. 0 )  GO TO 2793                               M22.4604
      IF ( NLTYPE(I)  .EQ.  -98   .OR.                                  M23.3486
     1     NLTYPE(I)  .EQ.  -96 )   GO TO 2781                          M23.3487
      IF( ANONL(I) .GE. 0.0 )  GO TO 2793                                  11868
 2781 K = NONLK(I)                                                         11869
      M = IABS( NONLM(I) )                                                 11870
      IF( K .NE. L )  GO TO 2784                                           11871
      N4 = M                                                               11872
      GO TO 2786                                                           11873
 2784 IF( M .NE. L )  GO TO 2793                                           11874
      N4 = K                                                               11875
 2786 IF ( NLTYPE(I)  .NE.  -96 )  GO TO 2787                           M23.3488
      N3 = NONLAD(I) + 1                                                M23.3489
      YX = GSLOPE(N3)                                                   M23.3490
      F(L) = F(L) + YX                                                  M23.3491
      F(N4) = F(N4) - YX                                                M23.3492
      IF ( IPRSUP  .GE.  2)                                             M23.3493
     1  WRITE ( LUNIT6, 2790) I, K, M, L, N4, YX, F(L), F(N4)           M23.3494
 2790 FORMAT ( /,  31H TYPE-96 G ADDED TO ROW OF Y.  , 5I10, /, 3E25.15)M23.3495
      GO TO 2793                                                        M23.3496
 2787 N3 = NONLAD(I)                                                    M33.1119
      YX = GSLOPE(N3)                                                      11877
      F(L) = F(L) + YX                                                     11878
      F(N4) = F(N4) - YX                                                   11879
      IF( IPRSUP .GE. 2 )                                                  11880
     1 WRITE(LUNIT6, 2789)  I, K, M, L, N4, YX, F(L), F(N4)             M17. 745
 2789 FORMAT( /, 31H TYPE-98 G ADDED TO ROW OF (Y).  , 5I10,               11882
     1 /, 3E25.15 )                                                        11883
 2793 CONTINUE                                                             11884
 2801 KKS(L) = KSS                                                      M32.4541
      IF ( F(L) .NE. 0.0 )  GO TO 4308                                  M32.4542
      IF ( KCONST  .LE. 1 )   GO TO 2813                                M32.4543
      DO 2808  I=2, KCONST                                              M32.4544
      IF ( IFORM(I)  .NE.  18 )   GO TO 2808                            M32.4545
      N6 = TIME1(I)                                                     M32.4546
      IF ( N6  .EQ.  L )   GO TO 4308                                   M32.4547
 2808 CONTINUE                                                          M32.4548
C     CONNECT HIGH RESISTANCE TO GROUND FOR NONZERO DIAGONAL:           M32.4549
 2813 F(L) = EPSILN                                                     M32.4550
      WRITE (LUNIT6, 4108)  BUS(L), EPSILN                              M32.4551
 4108 FORMAT (   8H NODE  ",  A6,  19H"  HAS NO CONNECTED,              M32.4552
     1          39H LINEAR BRANCHES.   ADD (TO GROUND) G =,             M32.4553
     2   E13.4  ,/, 1X  )                                               M37.4742
 4308 IF( NUM99 .LE. 0 )  GO TO 73620                                   M32.4555
      DO 73610  I=1, INONL                                                 11993
      IF( NLTYPE(I) .GT. 0 )  GO TO 73610                                  11994
      K = NONLK(I)                                                         11995
      M = IABS( NONLM(I) )                                                 11996
      IF( K .NE. L )  GO TO 73600                                          11997
      IF( F(M) .EQ. 0.0 )  F(M) = 1.0 / FLTINF                             11998
      GO TO 73610                                                          11999
73600 IF( M .NE. L )  GO TO 73610                                          12000
      IF( F(K) .EQ. 0.0 )  F(K) = 1.0 / FLTINF                             12001
73610 CONTINUE                                                             12002
73620 IF ( IPRSUP  .GE.  4 )                                            M32.4556
     1 WRITE (LUNIT6, 783)  BUS(L),  ( F(I), I=1, NTOT )                M32.4557
  783 FORMAT (   8H NODE  ",  A6,  18H"  ROW OF (Y) ....                M32.4558
     1  ,/,  ( 1X,  5E25.15 ) )                                         M32.4559
      DO 885  I=2, NTOT                                                         
      IF ( F(I)  .EQ.  0.0 )                                            M32.4561
     1 GO TO 885                                                                
      KSS = KSS - 1                                                             
      KM(KSS) = I                                                       M32.4563
      YKM(KSS) = F(I)                                                   M32.4564
  885 CONTINUE                                                             12010
      KM(KSS) = -KM(KSS)                                                        
      IF( IPRSUP .GT. 0 )                                                  12012
     1WRITE(LUNIT6, 73647)  L, KKS(L), KSS,     INONL, NUM99            M17. 747
73647 FORMAT( 40H END OF STORAGE OF ROW OF (Y), AT 73647.   , 5I10 )    M32.4566
      IF ( KSS  .GT.  NTOT )   GO TO 894                                        
      WRITE (LUNIT6, 892)  L                                            M32.4568
  892 FORMAT ( 35H BEFORE ANY FACTORING,  (Y) STORAGE,                  M32.4569
     1         38H HAS ALMOST OVERFLOWED LIST 5 STORAGE.,               M32.4570
     2         35H   ONLY NTOT CELLS LEFT AT ROW  L =,  I4  )           M32.4571
      KILL = 1                                                          M19.1233
      LSTAT(16) = 5                                                     M32.4572
      LSTAT(14) = L                                                     M19.1235
      LSTAT(13) = 1                                                     M32.4573
      LSTAT(19) = 894                                                   M32.4574
      GO TO 9900                                                        M32.4575
  894 IF ( L  .LT.  KPARTB )   GO TO 600                                M32.4576
      KKS(1) = KSS                                                      M32.4577
      CALL MOVE0 ( KPSOUR(1), NTOT )                                    M37.4743
      L = 1                                                                     
      MP = 1000                                                         M37.4745
  202 IF ( MP .GT. NTOT )  GO TO 1000                                   M37.4746
      MP = MP + 1000                                                    M37.4747
      GO TO 202                                                         M37.4748
 1000 L = L + 1                                                                 
      M = L + MP                                                        M37.4750
      J = KKS(L)                                                                
 2000 J = J - 1                                                                 
      I = IABS( KM(J) )                                                         
      K = KPSOUR(I)                                                     M37.4754
      IF ( K .NE. 0  .AND.  K .LT. M )  M = K                           M37.4755
      IF ( KM(J)  .GT.  0 ) GO TO 2000                                  M37.4756
      J = KKS(L)                                                        M37.4757
 3000 J = J - 1                                                         M37.4758
      I = IABS ( KM(J) )                                                M37.4759
      IF ( I .GT. KPARTB )  GO TO 5000                                  M37.4760
      IF ( KPSOUR(I) .EQ. 0 )  KPSOUR(I) = M                            M37.4761
      IF ( KPSOUR(I) .EQ. M )  GO TO 5000                               M37.4762
      NP = KPSOUR(I)                                                    M37.4763
      DO 4000 MM = 2, KPARTB                                            M37.4764
 4000 IF ( KPSOUR(MM) .EQ. NP ) KPSOUR(MM) = M                          M37.4765
 5000 IF ( KM(J) .GT. 0 )  GO TO 3000                                   M37.4766
      IF ( L .LT. KPARTB ) GO TO 1000                                   M37.4767
      DO 6868 NP = 1, INONL                                             M37.4768
      IF ( NLTYPE(NP) .LT. 0 ) GO TO 6868                               M37.4769
      MARK = KPSOUR( NONLK(NP) )                                        M37.4770
      IF ( MARK .EQ. 0 ) GO TO 6868                                     M37.4771
      MARM = KPSOUR( IABS(NONLM(NP)) )                                  M37.4772
      IF ( MARM .EQ. 0  .OR.  MARM .EQ. MARK ) GO TO 6868               M37.4773
      DO 6768 NQ = 2, NTOT                                              M37.4774
 6768 IF ( KPSOUR(NQ) .EQ. MARM ) KPSOUR(NQ) = MARK                     M37.4775
 6868 CONTINUE                                                          M37.4776
      DO 5800 MM = 1, KSWTCH                                            M37.4778
      KNODE = KPSOUR( KMSWIT(MM) )                                      M37.4779
      MNODE = KPSOUR( KMSWIT(MM + LSWTCH) )                             M37.4780
      IF ( KNODE .EQ. MNODE ) GO TO 5800                                M37.4781
      IF ( KNODE .EQ. 0  .OR.  MNODE .EQ. 0 )  GO TO 5800               M37.4782
      DO 5700 NN = 2, NTOT                                              M37.4783
 5700 IF ( KPSOUR(NN) .EQ. MNODE ) KPSOUR(NN) = KNODE                   M37.4784
 5800 CONTINUE                                                          M37.4785
      IF( IPRSUP .GE. 1 )                                               M42.6639
     1 WRITE(LUNIT6, 903) KPARTB,NTOT,(KPSOUR(I),I=1,NTOT)              M37.4787
  903 FORMAT( /,  15H KPARTB, NTOT =, 2I10 ,/,                          M37.4788
     1  28H KPSOUR CELLS FOLOW........ , /,  ( 1X, 20I6 )  )            M37.4789
      LSTAT(27) = KSS                                                      12025
      IF( IPRSUP .LE. 2 )  GO TO 9900                                   M32.4581
      WRITE (LUNIT6, 3744)  ( KKS(I), I=1, KPARTB )                     M32.4582
 3744 FORMAT ( /,  36H ( KKS(I), I=1, KPARTB ) FOLLOW ....              M32.4583
     1   ,/,  ( 1X,  20I6 )  )                                          M32.4584
      WRITE (LUNIT6, 8193)  (I, KM(I), YKM(I), I = KSS, LYMAT)          M32.4585
 8193 FORMAT ( /, 2( 5X, 1HI, 7X, 5HKM(I), 19X, 6HYKM(I), 6X )          M32.4586
     1   ,/,   ( I6, I12, E25.16,   2I12, E25.16 )  )                   M32.4587
 9900 IF ( IPRSUP  .GE. 1 )                                             M32.4588
     1 WRITE (LUNIT6, 9906)                                             M32.4589
 9906 FORMAT ( 16H EXIT  "LAST14".  )                                   M32.4590
      RETURN                                                            M32.4591
      END                                                                  12043
      SUBROUTINE QYQTR(NPHS,DY,Q,Y)                                     VAX.2322
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C      INSERT DECK  LABCOM                                              VAX.2323
      DIMENSION DY(1)   ,Q(1)    ,Y(1)                                  VAX.2324
C                                                                       VAX.2325
      N1=NPHS*(NPHS+1)/2                                                VAX.2326
      DO 100 I=1,N1                                                     VAX.2327
      Y(I)=0.D0                                                         VAX.2328
100   CONTINUE                                                          VAX.2329
C                                                                       VAX.2330
      DO 101 K=1,NPHS                                                   VAX.2331
      KM1=(K-1)*NPHS                                                    VAX.2332
      DYK=DY(K)                                                         VAX.2333
      IJ=0                                                              VAX.2334
      DO 102 J=1,NPHS                                                   VAX.2335
      JK=KM1+J                                                          VAX.2336
      QJK=Q(JK)                                                         VAX.2337
      DO 103 I=1,J                                                      VAX.2338
      IJ=IJ+1                                                           VAX.2339
      IK=KM1+I                                                          VAX.2340
      QIK=Q(IK)                                                         VAX.2341
      B=QIK*QJK                                                         VAX.2342
      Y(IJ)=Y(IJ)+DYK*B                                                 VAX.2343
103   CONTINUE                                                          VAX.2344
102   CONTINUE                                                          VAX.2345
101   CONTINUE                                                          VAX.2346
      RETURN                                                            VAX.2347
      END                                                               VAX.2348
!      SUBROUTINE QYQTR(NPHS,DY,Q,Y)                                     VAX.2322
!      IMPLICIT REAL*8 (A-H, O-Z) ,
!     1      INTEGER*4 (I-N)
!C      INSERT DECK  LABCOM                                              VAX.2323
!      DIMENSION DY(1)   ,Q(1)    ,Y(1)                                  VAX.2324
!C                                                                       VAX.2325
!      N1=NPHS*(NPHS+1)/2                                                VAX.2326
!      DO 100 I=1,N1                                                     VAX.2327
!      Y(I)=0.D0                                                         VAX.2328
!100   CONTINUE                                                          VAX.2329
!C                                                                       VAX.2330
!      DO 101 K=1,NPHS                                                   VAX.2331
!      KM1=(K-1)*NPHS                                                    VAX.2332
!      DYK=DY(K)                                                         VAX.2333
!      IJ=0                                                              VAX.2334
!      DO 102 J=1,NPHS                                                   VAX.2335
!      JK=KM1+J                                                          VAX.2336
!      QJK=Q(JK)                                                         VAX.2337
!      DO 103 I=1,J                                                      VAX.2338
!      IJ=IJ+1                                                           VAX.2339
!      IK=KM1+I                                                          VAX.2340
!      QIK=Q(IK)                                                         VAX.2341
!      B=QIK*QJK                                                         VAX.2342
!      Y(IJ)=Y(IJ)+DYK*B                                                 VAX.2343
!103   CONTINUE                                                          VAX.2344
!102   CONTINUE                                                          VAX.2345
!101   CONTINUE                                                          VAX.2346
!      RETURN                                                            VAX.2347
!      END                                                               VAX.2348
      SUBROUTINE BREQIV( IKF, ISFD, IBF )                               M32.4592
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION  UR(40)                                                 M32.4595
C     THIS ROUTINE PRODUCES COMPANION MODEL FOR EACH BRANCH. IT ALSO    M32.4596
C     INITIALIZES THE CURRENT INJECTIONS FOR THE BRANCHES  *   *   *   *M32.4597
      IDK = IKF * 2                                                     M32.4598
      IKF = IKF + 1                                                     M32.4599
      ISC = IBF + 1                                                     M32.4600
      ISF = ISFD + 1                                                    M32.4601
      IF( IPRSUP .GT. 0 )                                               M32.4602
     1    WRITE( LUNIT6, 1 ) IKF, ISFD, IBF, IMFD(IDK+1), IMFD(IDK+2)   M32.4603
    1 FORMAT(41H INTEGER COUNTERS AT START OF BREQIV.....,7X,3HIKF,     M32.4604
     16X,4HISFD,7X,3HIBF,6X,4HIZFD,6X,4HIPFD,/,41X,5I10)                M32.4605
C     CALCULATE MODAL VOLTAGES FROM PHASE VALUES   *   *   *   *   *   *M32.4606
      CZ =  IT2                                                         M34. 147
      CZ = 1.0 / CZ                                                     M34. 148
      UR(1) = VOLT(1)                                                   M32.4608
      DO  2  KA = 2, IT2                                                M32.4609
         UR(1) = UR(1) + VOLT(KA)                                       M32.4610
    2    UR(KA) = ( VOLT(1) - VOLT(KA) ) * CZ                           M32.4611
      AR = UR(1) * CZ                                                   M32.4612
      IF( IPRSUP .GT. 0 )                                               M32.4613
     1    WRITE( LUNIT6, 3 ) AR, ( UR(KA), KA = 2, IT2 )                M32.4614
    3 FORMAT(25H MODAL VOLTAGES IN BREQIV,/,(2X,6E21.12))               M32.4615
C     PROCESS 'ZERO' SEQUENCE INFORMATION FIRST    *   *   *   *   *   *M32.4616
      IST = ISFD + 1                                                    M32.4617
      ISK = IMFD(IDK+1)                                                 M32.4618
      ISFD = ISFD + ISK * 5                                             M32.4619
      ISV = IBF - 2                                                     M32.4620
      DO  5  KA = IST, ISFD, 5                                          M32.4621
         ISV = ISV + 3                                                  M32.4622
         AL1 = RMFD(KA+1) / DELTA2                                      M32.4623
         AR1 = RMFD(KA)                                                 M32.4624
         AC1 = RMFD(KA+2)                                               M32.4625
         ARL = RMFD(KA+3)                                               M32.4626
         IF( AC1 .GT. 0 ) AC1 = DELTA2 / AC1                            M32.4627
         A2 = 0.                                                        M32.4628
         A1 = AL1                                                       M32.4629
         IF( AL1 .EQ. 0. .OR. ARL .EQ. 0. ) GO TO 4                     M32.4630
         A1 = ( AL1 * ARL ) / ( ARL + AL1 )                             M32.4631
         A2 = 2. * A1 / ARL                                             M32.4632
    4    AZR = 1. / ( AR1 + A1 + AC1 )                                  M32.4633
         AZI = ( AR1 - A1 + AC1 ) * AZR                                 M32.4634
         CIKFD(ISV) = -CIKFD(ISV+1) + AZR * AR                          M32.4635
C     STORE CONSTANTS FOR LATER USE IN THE TIME-STEP LOOP  *   *   *   *M32.4636
         RMFD(KA+1) = AZR                                               M32.4637
         RMFD(KA+2) = AC1                                               M32.4638
         RMFD(KA+3) = AZI                                               M32.4639
         RMFD(KA+4) = A2                                                M32.4640
    5 CONTINUE                                                          M32.4641
C     PROCESS THE REMAINING MODES  *   *   *   *   *   *   *   *   *   *M32.4642
      IBF = ISV + 2                                                     M32.4643
      IST = ISFD + 1                                                    M32.4644
      ISK = IMFD(IDK+2)                                                 M32.4645
      ISU = ISFD + ISK * 5                                              M32.4646
      ISK = ISK * 3                                                     M32.4647
C     LOOP ACROSS ALL 'POSITIVE' SEQUENCE BRANCHES *   *   *   *   *   *M32.4648
      DO  8  KA = IST, ISU, 5                                           M32.4649
         AL1 = RMFD(KA+1) / DELTA2                                      M32.4650
         AR1 = RMFD(KA)                                                 M32.4651
         ARL = RMFD(KA+3)                                               M32.4652
         AC1 = RMFD(KA+2)                                               M32.4653
         IF( AC1 .GT. 0. ) AC1 = DELTA2 / AC1                           M32.4654
         A2 = 0.                                                        M32.4655
         A1 = AL1                                                       M32.4656
         IF( ARL .EQ. 0. .OR. AL1 .EQ. 0. ) GO TO 6                     M32.4657
         A1 = ( AL1 * ARL ) / ( AL1 + ARL )                             M32.4658
         A2 = 2. * A1 / ARL                                             M32.4659
    6    AZR = 1. / ( AR1 + A1 + AC1 )                                  M32.4660
         AZI = ( AR1 - A1 + AC1 ) * AZR                                 M32.4661
C     INTERNAL LOOP ACROSS THE REMAINING ( IT2 - 1 ) MODES *   *   *   *M32.4662
         ISV = ISV + 3                                                  M32.4663
         ISW = ISV - ISK                                                M32.4664
         DO  7  KB = 2, IT2                                             M32.4665
            ISW = ISW + ISK                                             M32.4666
    7       CIKFD(ISW) = -CIKFD(ISW+1) + AZR * UR(KB)                   M32.4667
C     STORE CONSTANTS FOR LATER USE IN THE TIME-STEP LOOP  *   *   *   *M32.4668
         RMFD(KA+1) = AZR                                               M32.4669
         RMFD(KA+3) = AZI                                               M32.4670
         RMFD(KA+2) = AC1                                               M32.4671
         RMFD(KA+4) = A2                                                M32.4672
    8 CONTINUE                                                          M32.4673
      IBF = IBF + ( IT2 - 1 ) * ISK                                     M32.4674
      ISFD = ISU                                                        M32.4675
      IF( IPRSUP .LT. 1 ) GO TO 15                                      M32.4676
      WRITE( LUNIT6, 9 ) IKF                                            M32.4677
    9 FORMAT(26H ARRAYS FOR BRANCH SET NO.,I6,18H  AT END OF BREQIV)    M32.4678
      WRITE( LUNIT6, 10 ) ISF, ISFD, ( RMFD(KA), KA = ISF, ISFD )       M32.4679
   10 FORMAT(16H ARRAY RMFD FROM,I6,4H  TO,I6,/,(2X,6E21.11))           M32.4680
      WRITE( LUNIT6, 11 ) ISC, IBF, ( CIKFD(KA), KA = ISC, IBF )        M32.4681
   11 FORMAT(17H ARRAY CIKFD FROM,I6,4H  TO,I6,/,(2X,6E21.11))          M32.4682
   15 RETURN                                                            M32.4683
      END                                                               M32.4684
      SUBROUTINE PAST                                                   M20.3940
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED BY BRANDWAJN (TYPE-59) S.M. MODEL             M31.3077
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
C     INITIALIZATION AFTER PHASOR SOLUTION FOR TYPE-59 S.M.             M31.3079
C     THIS ROUTINE MOVES THE INITIAL CONDITIONS TO TIME '-DELTAT'       M20.3946
      IBR = IBROLD                                                      M38.2834
      ILK = 0                                                           M38.2835
      IN = 1                                                                    
      NWD = 24                                                                  
      IHT = 0                                                                   
      IVK = 0                                                                   
      I26 = 0                                                                   
      J30 = 1                                                                   
      I30 = 1                                                                   
      J75 = 27                                                                  
      AKO = 1.0 + DAMRAT                                                        
      DELTA6 = 6.0 * DELTAT                                                     
      DO 900 K = 1, NUMSM                                                       
      N1 = ISMDAT( J30+2 )                                                      
      N2 = ISMDAT( J30+5 )                                                      
      V1 = E(N1) - E(N2)                                                        
      N1 = ISMDAT( J30+3 )                                                      
      N2 = ISMDAT( J30+6 )                                                      
      V2 = E(N1) - E(N2)                                                        
      N1 = ISMDAT( J30+4 )                                                      
      N2 = ISMDAT( J30+7 )                                                      
      V3 = E(N1) - E(N2)                                                        
      CV0 = ( V1 + V2 + V3 ) * ASQRT3                                           
      IM = ISMDAT( J30 )                                                        
C     RETRIEVE AND STORE THE ELEMENTS OF MATRIX /YKM/ PERTAINING TO *   M32.4686
C     THE SYNCHRONOUS MACHINES CONNECTED TO NODES NODSMA(K),NODSMB(K),**M23.3704
C     AND NODSMC(K) ****************************************************M23.3705
      IDK = J75 + 54                                                            
      JD30 = J30 + 20                                                   M37.4812
      JE30 = J30 + 1                                                    M37.4813
      DO 20 JDK = 1, 3                                                  M23.3707
      K31 = JE30 + JDK                                                  M37.4814
      N1 = ISMDAT( K31 )                                                        
      N3 = KKS( N1 )                                                    M23.3711
      KBK = JDK + 6                                                     M23.3713
      K31 = JE30                                                        M37.4816
      DO 15 KP = JDK, KBK, 3                                            M23.3715
      K31 = K31 + 1                                                     M37.4817
      N1 = ISMDAT( K31 )                                                        
      N4 = N3 - 1                                                       M32.4687
 12   IF( IABS ( KM(N4) )  .NE.  N1 )   GO TO 14                        M39.2669
      ELP( IDK+KP ) = YKM( N4 )                                                 
      N5 = KP + JD30                                                    M37.4820
      ISMDAT( N5 ) = N4                                                 M37.4821
      GO  TO  15                                                        M37.4822
 14   N4 = N4 - 1                                                       M39.2670
      GO  TO  12                                                        M39.2671
 15   CONTINUE                                                          M23.3727
 20   CONTINUE                                                          M23.3728
      AP1 = 0.0                                                         M23.3729
      AP2 = 0.0                                                         M23.3730
      IDELTA = ISMDAT( J30+1 )                                                  
 800  ILK = ILK + 1                                                             
      I75 = I26 + 27                                                            
      C1 = CU( IN+11 )                                                          
      C2 = CU( IN+12 )                                                          
      C3 = CU( IN+13 )                                                          
      A1 = ELP( I75 )                                                           
      A2 = ELP(I75+1)                                                           
C     CALCULATE BRANCH HISTORY TERMS CIK ****************************** M39.2673
      CIK( IBR+1 ) = -C1 - A1*V1 - A2*(V2+V3)                           M38.2849
      CIK( IBR+2 ) = -C2 - A1*V2 - A2*(V1+V3)                           M38.2850
      CIK( IBR+3 ) = -C3 - A1*V3 - A2*(V1+V2)                           M38.2851
      IBR = IBR + 3                                                             
      IF( IPRSUP  .LT.  2  )    GO  TO  100                             M39.2674
      IZU = IBR - 2                                                     M21. 192
      WRITE( LUNIT6, 6600 )   ( CIK(IZN), IZN = IZU, IBR )              M39.2675
 6600 FORMAT( 1X, 18HCURRENT INJECTION , 3E21.12 )                      M39.2676
C     CALCULATE HISTORY TERMS FOR ROTOR CIRCUITS * * * * * * * * * * * *M20.3993
C     THESE CALCULATIONS USE THE CURRENTS FROM TIME T = 0.0 *********** M37.4829
 100  A1 = CU(  IN  )                                                   M39.2677
      A2 = CU( IN+1 )                                                   M38.2855
      A3 = CU( IN+3 )                                                   M38.2856
      A4 = CU( IN+4 )                                                   M38.2857
      A5 = CU( IN+5 )                                                   M38.2858
      A6 = CU( IN+6 )                                                   M38.2859
C     HISTORY TERMS FOR ROTOR CIRCUITS********************************* M37.4835
      IH = I75 + 17                                                             
      CU( IN+3 ) = ELP(IH+6) * A1 + (ELP(IH+7) + AKO * ELP(I26+7))      M38.2861
     1 * A3  + ELP(IH+8) * A4                                           M38.2862
      CU( IN+4 ) = ELP(IH+9) * A1 + ELP(IH+10) * A3 +                   M38.2863
     1 ( ELP(IH+11) +  AKO * ELP(I26+8) ) * A4                          M38.2864
      CU( IN+11 ) = 1.0                                                         
      CU( IN+5 ) = ELP(IH+12) * A2 + ( ELP(IH+13) + AKO                 M38.2866
     1 * ELP(I26+15) ) * A5 + ELP(IH+14) * A6                           M38.2867
      CU( IN+6 ) = ELP(IH+15) * A2 + ELP(IH+16) * A5 +                  M38.2868
     1 ( ELP(IH+17) + AKO * ELP(I26+16) ) * A6                          M38.2869
      CU( IN+12 ) = CU( IN+7 )                                                  
      CU( IN+13 ) = CU( IN+8 )                                                  
C     CALCULATE HISTORY TERMS FOR STATOR CIRCUITS********************** M37.4848
      CU( IN+2 ) = CV0 + CU( IN+2 ) / ELP( I26+17 )                             
      AFD = CU( IN+14 ) * OMEGA                                         M38.2872
      AFQ = CU( IN+16 ) * OMEGA                                         M38.2873
C     NOT FINISHED***************************************************** M37.4853
      CU(  IN  ) = CU( IN+7 ) + A1 / ELP(I75+39) + AFQ                          
      CU( IN+1 ) = CU( IN+8 ) + A2 * ELP(I75+41) - AFD                          
      IF( IPRSUP .LT. 1 )  GO  TO  150                                  M39.2678
      IKM = IN + NWD - 1                                                M37.4856
      WRITE( LUNIT6, 6601 )  ILK, (CU(IJ), IJ = IN, IKM )               M39.2679
 6601 FORMAT ( /, 40H CURRENT HISTORY TERMS FOR GENERATOR NO., I5 , /,  M39.2680
     1 ( 1X,  7E17.8 )  )                                               M20.4024
 150  A5 = ELP( I75 )                                                   M39.2681
      A6 = ELP(I75+1)                                                   M39.2682
      IF( IDELTA .EQ. 0 )  GO  TO  160                                          
      A11 = ( A5 - A6 ) / 3.0                                                   
      A5 = A11 + A11                                                    M39.2685
      A6 = -A11                                                         M39.2686
 160  AP1 = AP1 + A5                                                    M39.2687
      AP2 = AP2 + A6                                                    M39.2688
C     BACK-OFF THE MECHANICAL EQUATIONS TO T = -DELTAT   * * * * * * * *M30. 397
C     ANGLE = ANGLE - ( DELTAT * SPEED ) / 2.0    SPEED = TORQUE - Y *  M30. 399
C     SPEED    * * * * * * * * * * * * * * * * * * * * * * * * * * * * *M30. 400
      CZ = ELP( I26+26 )                                                M37.4859
      NUMASK = ISMDAT( I30+11 )                                         M37.4860
      NUM2 = NUMASK + NUMASK                                                    
      NUM4 = NUM2 + NUM2                                                M37.4862
      NLOCG = ISMDAT( I30+12 )                                          M37.4863
      NLOCE = ISMDAT( I30+13 )                                          M37.4864
      N1 = IHT + NUMASK                                                         
      N2 = N1 + NUM2                                                            
      N22 = IVK + NUM2                                                          
      M22 = N22                                                         M38.2879
      M26 = IHT + NUM4                                                          
      DO 21 KA = 1, NUMASK                                              M30. 408
      KB = N1 + KA                                                      M30. 410
      KD = N2 + KA                                                      M37.4869
      KE = M26 + KA                                                     M37.4870
      KC = KE + NUMASK                                                  M37.4871
      M22 = M22 + 1                                                     M37.4872
      SHP( M22 ) =  HISTQ( KB )                                                 
      HISTQ( KE ) = - SHP( M22 )                                                
 21   HISTQ( KD ) = HISTQ( KC ) / HISTQ( KB )                                   
      KB = N2 + NLOCG                                                           
      CD = ( ( AFD * A2  -  AFQ * A1 ) * TENM6 ) / OMEGA                M38.2884
      HISTQ( KB ) = HISTQ( KB ) - CD * CZ                               M37.4878
      IF( NLOCE .EQ. 0 )  GO  TO  22                                    M30. 415
      KB = N2 + NLOCE                                                           
      NLOCE1 = N22 + NLOCE                                                      
      CEXC = - CU( IN+10 ) * A3 * TENM6                                 M37.4881
      HISTQ( KB ) = HISTQ( KB ) - CEXC / SHP( NLOCE1 )                  M38.2885
 22   DO 23 KA = 1, NUMASK                                              M30. 418
      KD = IHT + KA                                                     M37.4883
      KB = KD + NUM2                                                            
      KC = M26 + KA                                                     M37.4885
 23   HISTQ( KB ) = HISTQ( KD ) + DELTA2 * HISTQ( KC )                  M38.2886
      KV = IVK + 1                                                      M33.1140
      KW = N2 + 1                                                               
      KU = M26 + 1                                                              
      CALL BANMUL( SHP(KV), HISTQ(KU), HISTQ(KW), NUMASK )              M38.2887
      KL = IVK + NUM2                                                   M38.2888
      IF( IPRSUP .GT. 1  )                                              M30. 426
     1 WRITE( LUNIT6, 6602 )   KV, KL, ( SHP(II), II = KV, KL )         M39.2689
 6602 FORMAT( 1X,  13HMATRIX Y FROM,  I6,  2X,  2HTO,  I6,              M39.2690
     1 /,  ( 2X,  6E20.11 ) )                                           M39.2691
C     TRIANGULARIZE AN APPROPRIATE PART OF THE MATRIX  'Y' ************ M37.4893
      CALL BANDEL( SHP( KV ), NUMASK )                                          
      IVK = KL + 5 * NUM2                                                       
      KA = IHT + 1                                                      M38.2892
      IHT = IHT + 3 * NUM2                                                      
      IF( IPRSUP .LT. 1 )   GO  TO  200                                 M39.2692
      WRITE( LUNIT6, 6603 )  KA, KB, ( HISTQ( KC ), KC = KA, IHT )      M39.2693
 6603 FORMAT( 18H  ARRAY HISTQ FROM, I5, 4H  TO, I5, /, (2X,6E20.11) )  M39.2694
      WRITE( LUNIT6, 6602 )  KV, KL, ( SHP(II), II = KV, KL )           M39.2695
      KL = KL + 3 * NUM2                                                M38.2896
      KV = KL + 1                                                       M38.2897
      KL = KL + 2 * NUM2                                                M38.2898
      WRITE( LUNIT6, 6602 )   KV, KL, ( SHP(II), II = KV, KL )          M39.2696
 200  IM = IM - 1                                                       M39.2697
      IN = IN + NWD                                                             
      I26 = I26 + 101                                                           
      I30 = I30 + 30                                                            
      IF( IM .GT. 0 ) GO TO 800                                         M38.2902
C     CALCULATE AND STORE THE CONSTANT OFF-SET TERMS FOR UPDATING OF THEM23.3750
C     MATRIX (YKM). THESE TERMS WILL BE USED ONLY FOR SATURABLE . ******M32.4696
C     SYNCHRONOUS MACHINES *********************************************M23.3752
      ELP( IDK+1 ) = ELP( IDK+1 ) - AP1                                         
      ELP( IDK+5 ) = ELP( IDK+5 ) - AP1                                         
      ELP( IDK+9 ) = ELP( IDK+9 ) - AP1                                         
      ELP( IDK+2 ) = ELP( IDK+2 ) - AP2                                         
      ELP( IDK+3 ) = ELP( IDK+3 ) - AP2                                         
      ELP( IDK+4 ) = ELP( IDK+4 ) - AP2                                         
      ELP( IDK+6 ) = ELP( IDK+6 ) - AP2                                         
      ELP( IDK+7 ) = ELP( IDK+7 ) - AP2                                         
      ELP( IDK+8 ) = ELP( IDK+8 ) - AP2                                         
      IF( IPRSUP .LE. 0 )    GO  TO   899                               M38.2912
      IDV = IDK + 9                                                     M38.2913
      JDK = IDK + 1                                                     M38.2914
      WRITE( LUNIT6, 6604 )    ( ELP( IU ),  IU = JDK,IDV )             M39.2698
 6604 FORMAT( 8H AT 900., ( /, 6E20.12 ) )                              M39.2699
      IDV = JD30 + 9                                                    M37.4907
      JDK = JD30 + 1                                                    M37.4908
      WRITE( LUNIT6, 6605 )  ( ISMDAT(IU), IU = JDK ,IDV )              M39.2700
 6605 FORMAT( 8H AT 900., /, 9I12, / )                                  M39.2701
 899  J30 = J30 + 30                                                            
      J75 = J75 + 101                                                           
 900  CONTINUE                                                          M20.4052
      IF( IPRSUP .LT. 2 )   GO  TO  2500                                M38.2918
      IBK = 0                                                           M38.2919
      DO 2400 I = 1, ILK                                                M38.2920
      IBL = IBK + 1                                                     M38.2921
      IBK = IBK + 101                                                   M38.2922
      WRITE( LUNIT6, 6606 )  ( ELP( IK ), IK = IBL, IBK )               M39.2702
 6606 FORMAT( /, ( 1X, 6E20.12 ) )                                      M39.2703
 2400 CONTINUE                                                          M38.2925
 2500 RETURN                                                            M38.2926
      END                                                               M20.4054
      SUBROUTINE BANMUL( AB, X, Y, N )                                  M37.4911
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE PERFORMS POSTMULTIPLICATION OF A VECTOR BY A TRI-     M37.4912
C     DIAGONAL MATRIX 'AB'. THE RESULTS ARE ADDED TO THE VECTOR 'Y'.    M37.4913
      DIMENSION  AB( 1 ), X( 1 ), Y( 1 )                                M37.4914
C     INITIALIZATION ************************************************** M37.4915
      S1 = 0.0                                                          M37.4916
      I2 = 0                                                            M37.4917
      D1 = X( 1 )                                                       M37.4918
      IF( N1 .EQ. 1 )   GO TO 20                                                
      N1 = N - 1                                                                
C     START LOOP ****************************************************** M37.4919
      DO 10 I = 1, N1                                                           
      D3 = X( I+1 )                                                     M37.4921
      I2 = I2 + 2                                                               
      D2 = AB( I2 )                                                             
      Y(I) = Y(I)  +  AB( I2-1 ) * D1  +  D2 * D3  +  S1                M37.4924
      S1 = D2 * D1                                                              
      D1 = D3                                                                   
 10   CONTINUE                                                          M37.4927
C     FINISH LAST ROW OPERATIONS ************************************** M38.2934
 20   Y( N ) = Y( N ) + AB( I2+1 ) * D1 + S1                            M38.2935
      RETURN                                                            M37.4928
      END                                                               M37.4929
      SUBROUTINE BANDEL( AB, N )                                        M37.4930
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     TRIANGULARIZATION OF SYMMETRIC TRIDIAGONAL MATRIX 'AB' STORED AS  M37.4931
C     A ONE VECTOR CONTAINING DIAGONALS AND LOWER (UPPER) OFF-DIAGONALS M37.4932
C     EACH DIAGONAL IS FOLLOWED BY AN APROPRIATE OFF-DIAGONAL . THERE   M37.4933
C     ARE ALTOGETHER  2 * N  ENTRIES IN THE VECTOR 'AB'.                M37.4934
      DIMENSION  AB( 1 )                                                M37.4935
C     INITIALIZATION *************************************************  M37.4936
      N2 = N + N                                                        M37.4937
      I2 = 2                                                                    
      D = 1.0 / AB( 1 )                                                         
C     START LOOP OVER REMAINING ENTRIES ******************************  M37.4940
 10   AB( I2-1 )  =  D                                                          
      E = AB( I2 )                                                              
      D = D * E                                                                 
      AB( I2 )  =  D                                                            
      I2 = I2 + 2                                                       M37.4945
      IF( I2  .GT.  N2 )   GO TO  20                                            
      D = 1.0 / ( AB( I2-1 ) -  D * E )                                         
      GO TO 10                                                                  
 20   RETURN                                                            M37.4949
      END                                                               M37.4950
