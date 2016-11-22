      SUBROUTINE OVER2 
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'
      INCLUDE  'LABL02.FOR;/NOLIST'                                           
C           %INCLUDE  '//c/tsu/cables.INS.FTN'                          M44.   1
      COMMON / LINEMODEL / KEXACT, NSOLVE, FMINSV, NUMRUN,NPHLMT
      COMMON / LINEMODEL / CHAR80, CHLMFS(18)
      CHARACTER*6 CHLMFS     ! 9-PHASE AS LIMIT FOR LMFS TEST
      CHARACTER*80 CHAR80
      CHARACTER*6 CHRPAD(18)

      DIMENSION  WK1(1)                                                 M44.   2
      EQUIVALENCE ( SEMAUX(1), WK1(1) )                                 M44.   3
      DIMENSION INFDLI(1)                                               M44.   4
      EQUIVALENCE  ( NAMEBR(1), INFDLI(1) )                                     
      DIMENSION  ICRIT(1)                                               M33. 146
      EQUIVALENCE  ( ICRIT(1), CRIT(1) )                                M33. 147
      REAL*8            TEXT1, TEXT2, TEXT3, TEXT4, TEXT5, TEXT6        M42.1512
      REAL*8            TEXT7, TEXT8, TEXT9, TEXT10, TEXT11, TEXT12     M31. 370
      REAL*8        TEXT13, TEXT14, TEXT15, TEXT16, TEXT17              M42.1513
      EQUIVALENCE  (INDTV(1), IADDRS), (INDTV(2), ITRANM)               M31. 371
      EQUIVALENCE  (INDTV(3), ITYOLD), (INDTV(4), ICHTR2)               M37.2209
      EQUIVALENCE (IPRSOV(39), NMAUTO)                                  M42.1514
C      CHARACTER*6   CHAR6  
C      CHARACTER*26  ALPHAN 
C      DATA ALPHAN  / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      DATA TEXT1   / 6HSTOP C /                                         M42.1515
      DATA TEXT2   / 6HASCADE /                                         M42.1516
      DATA TEXT3   / 6HUSE AB /                                         M42.1517
      DATA TEXT4   / 6HUSE RL /                                         M42.1518
      DATA TEXT5   / 6HCASCAD /                                         M42.1519
      DATA TEXT6   / 6HED PI  /                                         M42.1520
      DATA TEXT7   / 6HTRANSF /                                         M42.1521
      DATA TEXT8   / 6HORMER  /                                         M42.1522
      DATA TEXT9   / 6HTHREE  /                                         M42.1523
      DATA TEXT10  / 6HPHASE  /                                         M42.1524
      DATA TEXT11  / 6HBRANCH /                                         M31. 372
      DATA TEXT12  / 6H NAME: /                                         M31. 373
      DATA TEXT13  / 6HNONLIN /                                         M32.1048
      DATA TEXT14  / 6HES     /                                         M32.1049
      DATA TEXT16  / 6HLIN000 /                                                 
      DATA TEXT17  / 6HNLN000 /                                                 
      DATA  IBRNAM  /  0  /                                                     
      DATA  INONAM  /  0  /                                                     
      DATA  NFSCAN  /  0  /


      IF ( LASTOV .NE. 44 )  GO TO 2000
      K = NPHLMT  + 1            ! READY FOR NEXT BRANCH, LMFS CASE
      IBR = NPHLMT
      NTOT = 2 * NPHLMT + 1
      IT = 1
      IBR1 = 1
      DO 2789 JK = 1, NPHLMT
      M = JK + NPHLMT
      L = 2 * JK
C      WRITE (BUS(L), 2689)  CHLMFS(JK)(1:6)
      READ(CHLMFS(JK)(1:6), 2689) BUS(L)
 2689 FORMAT ( A6 ) 
C      WRITE (BUS(L+1), 2689)  CHLMFS(M)(1:6)
      READ(CHLMFS(M)(1:6), 2689)  BUS(L+1)
 2789 CONTINUE
      GO TO 4199   ! GO DIRECTLY TO USE $INCLUDE FOR LMFS Pi model half



C     FOLLOWING USED RATHER THAN DATA STATEMENTS DUE TO COMMON:         M37.2210
 2000 ITYOLD = 0                                                                
      KREQAB = 0                                                                
      ICHTR2 = 0                                                                
      IOLD = 0                                                          M39. 983
      KPH = 0                                                           M39. 984
      KSAT = 0                                                          M39. 985
      IBR1 = 0                                                          M39. 986
      NN17 = 0                                                                  
      NYCMP = 0                                                                 
      ISECTI = 400
 1000 IF ( IPRSUP  .GE.  1 )                                            M37.2214
     1 WRITE ( LUNIT6, 4567 )                                           M32.1053
 4567 FORMAT ( 23H BEGIN MODULE "OVER2".   )                            M32.1054
      L27DEP = LBRNCH * LSIZ27 / 2                                      M32.1055
      LL2 = 2                                                           M44.   6
      LL3 = 3                                                           M22.2515
      LL9 = 9                                                           M22.2516
      MXPHAS = 0                                                        M31. 374
      IFK = 0                                                           M31. 375
      LCOUNT = 0                                                        M31. 376
C     INITIALIZE COUNTERS FOR THE -666 BRANCHES  * * * * * * * * * * * *M32.1056
      IKF = 0                                                           M32.1057
      ISFD = 0                                                          M32.1058
      IBF = 0                                                           M32.1059
      ISGFD = -100                                                      M32.1060
      MREAD2 = 0                                                        M13.1210
      ICAS = 0                                                          M42.1529
      NRECUR = 0                                                        M28.2837
      MODEL = 0                                                         M29.1211
      IF (LASTOV .EQ. 4 ) GO TO 100                                     M29.1212
      DO 10004  I = 1, LFSEM                                            M32.1061
10004 SCONST(I) = 0.0                                                   M32.1062
      DO 10005  I = 1, LBRNCH                                           M32.1063
10005 IMODEL(I) = 0                                                     M32.1064
      DO 10006 I=1, LBRNCH                                              M29.1213
10006 LITYPE(I) = 0                                                     M29.1214
      IADDRS = 1                                                        M29.1215
  100 IT2=IT+2                                                          M42.1530
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M22.2517
      IPRINT=3                                                          M42.1531
      LSTAT(19)=100                                                     M42.1532
      IF(IT2.GT.LDATA) GO TO 9000                                       M42.1533
      IF( KSAT .GE. 0 )  GO TO 3986                                     M42.1534
      KSAT = 1                                                          M42.1535
      GO TO 100                                                         M42.1536
 3986 IF ( NTOT  .GT.  1 )   GO TO 3988                                 M13.1211
      IF ( MREAD2  .EQ.  0 )   GO TO 3989                               M13.1212
C     READ INPUT CARD USING CIMAGE                                      M13.1213
 3988 CALL CIMAGE                                                       M13.1214
 3989 MREAD2 = 1                                                        M13.1215

      DECODE ( 80, 2005, ABUFF )  CHAR80
 2005 FORMAT ( A )
      IF ( CHAR80(1:8) .NE. 'NO. OF P')  GO TO 2100  ! not LMFS case
      DO  2007   JK = 16, 80
      IF ( CHAR80(JK:JK) .NE. ' ' )  GO TO 2008
 2007 CONTINUE
      WRITE (LUNIT6, 2006)
 2006 FORMAT (' No integer value (format I1) was inputted for number',
     1 ' of phases in this LMFS case, run will be aborted. STOP.') 
      STOP
 2008 READ (CHAR80(JK:JK), 2189 ) NPHLMT

 2189 FORMAT ( I1)
      IF ( NOUTPR  .EQ.  0 )                                            
     1 WRITE (KUNIT6, 2010 ) NPHLMT
 2010 FORMAT ('+LMFS OF A ',I1, '-PHASE CIRCUIT' )
      CALL CIMAGE
      DECODE ( 80, 2005, ABUFF )  CHAR80
      IF ( CHAR80(1:7) .EQ. 'SENDING'  )  GO TO  2020
      WRITE (LUNIT6, 2015)
 2015 FORMAT('  CARD OF SENDING END NODES:  OF THIS LMFS CASE IS NOT',
     1 ' INPUTTED, AND CASE IS TO BE ABORTED. STOP.' )
      STOP
 2020 DECODE (80, 2025, ABUFF) (CHLMFS(JK), JK=1,NPHLMT)
 2025 FORMAT ( 18X, 9A6 )
      DO 2027 JK = 1, NPHLMT
      IF ( CHLMFS(JK)(1:6) .EQ.  '      ' )  GO TO 2028
 2027 CONTINUE
      GO TO 2030
 2028 WRITE (LUNIT6, 2029) NPHLMT
 2029 FORMAT (' PROGRAM OBSERVED AN INVALID BLANK NODE NAME FOR SENDING',
     1' END OF THIS ',I1, '-PHASE LMFS CIRCUIT. CASE IS TO BE STOPPED.')
      STOP
 2030 IF ( NOUTPR  .EQ.  0 )                                            
     1 WRITE (KUNIT6, 2035)
 2035 FORMAT('+SENDING END NODE NAMES (9A6 FORMAT)' )
      CALL CIMAGE
      DECODE ( 80, 2005, ABUFF )  CHAR80
      IF ( CHAR80(1:9) .EQ. 'RECEIVING'  )  GO TO  2040
      WRITE (LUNIT6, 2038)
 2038 FORMAT('  CARD OF RECEIVING END NODES: OF THIS LMFS CASE IS NOT',
     1 ' INPUTTED, AND CASE IS TO BE ABORTED. STOP.' )
      STOP
 2040 DECODE (80, 2045, ABUFF) (CHLMFS(JK), JK=NPHLMT+1,2*NPHLMT)
 2045 FORMAT ( 20X, 9A6 )
      DO 2047 JK = 1, NPHLMT
      IF ( CHLMFS(JK) (1:6) .EQ.  '      ' )  GO TO 2048
 2047 CONTINUE
      GO TO 2050
 2048 WRITE (LUNIT6, 2049) NPHLMT
 2049 FORMAT (' PROGRAM OBSERVED AN INVALID BLANK NAME FOR RECEIVING ',
     1 'END OF THIS ',I1, '-PHASE LMFS CIRCUIT. CASE IS TO BE STOPPED.')
      STOP
 2050 IF ( NOUTPR  .EQ.  0 )                                            
     1 WRITE (KUNIT6, 2055)
 2055 FORMAT('+RECEIVING END NODE NAMES (9A6 FORMAT)' )
      GO TO 100     ! END OF THREE "/" CARDS FOR LMFS DATA CASE


 2100 IF ( KOLBEG  .GT.  0 )   GO TO 6618                               M22.2518
      DECODE(80, 1, ABUFF)  ITYPE, BUS1, BUS2, BUS3, BUS4               M42.1537
    1 FORMAT (I2, 4A6, 9E6.2)                                           M42.1538
      GO TO 6621                                                        M22.2519
 6618 NFRFLD = 1                                                        M22.2520
      CALL FREFLD ( VOLTBC(1) )                                         M31. 381
      ITYPE = VOLTBC(1)                                                 M31. 382
      NFRFLD = 4                                                        M22.2523
      NRIGHT = -1                                                       M22.2524
      CALL FREONE ( D1 )                                                M29.1216
      BUS1 = TEXTA6(1)                                                  M22.2526
      BUS2 = TEXTA6(2)                                                  M22.2527
      BUS3 = TEXTA6(3)                                                  M22.2528
      BUS4 = TEXTA6(4)                                                  M22.2529
      NRIGHT = 0                                                        M22.2530
 6621 IF ( IPRSUP  .GE.  3  .AND. IBR1 .GE. 1  )                        M33. 148
     1 WRITE (LUNIT6, 7621)  IBR1, ITYOLD, KODSEM(IBR1),                M31. 384
     2                       ITRANM, ITYPE, LENGTH(IBR1)                M31. 385
 7621 FORMAT ( 45H CONT. TRANSP.?,  IBR1, ITYOLD, KODSEM(IBR1),,        M31. 386
     1         30H ITRANM, ITYPE, LENGTH(IBR1) =,  6I7   )              M32.1065
      IF ( BUS2  .NE.  TEXT12 )   GO TO 7682                                    
      N24 = 0                                                                   
      IF ( BUS1  .NE.  TEXT11 ) GO TO 7677                                      
      N1 = IBR + 1                                                      M33. 149
      CALL NAMEA6 ( BUS3, N24 )                                                 
      NAMEBR(N1) = N24                                                          
      IF ( NOUTPR  .EQ.  0 )                                            M31. 392
     1 WRITE (KUNIT6, 7674)  BUS3, N1                                   M35. 702
 7674 FORMAT ( 10H+MONIKER ",  A6,  23H" IS FOR NEXT BRANCH  +,         M31. 394
     1          I6,  2H .  )                                            M31. 395
 7675 IBRNAM = IBRNAM + 1                                                       
      IF ( IBRNAM .GT. IBR )  GO TO 7676                                        
      NAMEBR(IBRNAM) = 1                                                        
      IF ( NMAUTO .EQ. 0 ) GO TO 7675                                           
      CALL NMINCR ( TEXT16, IBRNAM )                                            
      N24 = 0                                                                   
      CALL NAMEA6 ( TEXT16, N24 )                                               
      NAMEBR(IBRNAM) = N24                                                      
      GO TO 7675                                                                
 7676 GO TO 3988                                                        M42.1551
 7677 IF ( BUS1 .NE. TEXT13 ) GO TO 7682                                        
      N1 = INONL + 1                                                    M33. 150
      CALL NAMEA6 ( BUS3, N24 )                                                 
      NAMESW(N1) = N24                                                          
      IF ( NOUTPR  .EQ.  0 )                                            M31. 400
     1 WRITE (KUNIT6, 7679)  BUS3, N1                                   M35. 703
 7679 FORMAT ( 10H+MONIKER ",  A6,                                      M31. 402
     1    26H" IS FOR NEXT NONLINEAR  +,   I6,  2H .   )                M31. 403
 7680 INONAM = INONAM + 1                                                       
      IF ( INONAM .GT. INONL ) GO TO 7681                                       
      NAMEBR(INONAM) = 1                                                        
      IF ( NMAUTO .EQ. 0 ) GO TO 7680                                           
      CALL NMINCR ( TEXT17, INONAM )                                            
      N24 = 0                                                                   
      CALL NAMEA6 ( TEXT17, N24 )                                               
      NAMENL(INONAM) = N24                                                      
      GO TO 7680                                                                
 7681 GO TO 3988                                                        M42.1563
 7682 IF (IBR1 .EQ. 0 )  GO TO 36621                                            
      IF (ITYOLD .GE. 0) GO TO 36621                                    M29.1218
      IF ( MODEL .GE. 1) GO TO 36621     ! MODEL phases untransposed
      IF(KODSEM(IBR1) .NE. 0 .AND.                                      M44.   7
     1     imodel(ibr1) .Eq. 0) GO TO 36621                                     
      IF(KODSEM(IBR1) .NE. 0 .AND. N13 .NE. 0) GO TO 36621                      
      IF (ITRANM .GT. 0) GO TO 36621                                    M29.1220
      IF (N13 .EQ. 0 .AND. IMODEL(IBR1) .EQ. -4 ) KTRLSW(3) = 0         M44.  12
      IF (ITYPE .GE. 0) GO TO 26621                                     M29.1221
      ITYM1 = IABSZ(ITYPE) - 1                                          M29.1222
      IF ( IMODEL(IBR1) .EQ. -3 ) GO TO 36621                           M44.   9
      IF ( ITYM1 .EQ. IABSZ(LENGTH(IBR1)) )  GO TO 36621                M29.1223
26621 IF ( LENGTH(IBR1) .NE. 3  .OR.  KTRLSW(3) .EQ. 0 )  GO TO 8200    M33. 151
      LITYPE(IBR1) = KTRLSW(3)                                          M33. 152
      GO TO  36621                                                      M33. 153
 8200 K = 1                                                                     
      N9 = IABSZ(LENGTH(IBR1) )                                                 
      IADD = IADDRS + 2*N9*N9 - 1                                               
      IF (IADD .LT. LFD )  GO TO  5950                                  M32.1067
      IPRINT = 21                                                       M32.1068
      LSTAT(19) =  5950                                                 M32.1069
      GO TO 9000                                                        M32.1070
 5950 N9SQ = N9 * N9                                                            
      LITYPE(IBR1)=-IADDRS                                                      
      IF (KODEBR(IBR1) .EQ. -1)  GO TO 15970                                    
      DO 5960  J=1, N9                                                          
      DO 5955  I=1, N9                                                  M29.1229
      VOLTI(K) = 1.                                                     M31. 409
      IF ( I .EQ. J .AND. I .NE. 1 )   VOLTI(K) = 1 - I                 M31. 410
      IF ( I .GT. J .AND. J .NE. 1 )   VOLTI(K) = 0.                    M31. 411
 5955 K = K + 1                                                         M29.1233
 5960 CONTINUE                                                          M29.1234
      N1 = IADDRS                                                       M29.1235
      DO 5970  I =1,N9SQ,N9                                                     
      K = I + N9 - 1                                                    M29.1238
      TEMP = 0.                                                         M29.1239
      DO 5965  J=I, K                                                           
 5965 TEMP = TEMP + VOLTI(J) ** 2                                       M31. 412
      TEMP = SQRTZ(TEMP)                                                M29.1242
      DO 5968  J=I, K                                                   M29.1243
      QFD(N1) = VOLTI(J)/TEMP                                           M31. 413
      SFD(N1) = 0.0                                                     M32.1073
 5968 N1 = N1 + 1                                                       M29.1245
 5970 CONTINUE                                                          M29.1246
      IF ( IPUNCH .NE. -4 .AND. N13 .EQ. 0.0  ) GO TO 50050             M44.  11
      IFQ = IFSEM                                                       M44.  12
      INOFF1 = LBRNCH                                                           
      INOFF2 = 2 * LBRNCH                                                       
      INOFF3 = 3 * LBRNCH                                                       
      INOFF4 = 4 * LBRNCH                                                       
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
      KOFF13 = KOFF10 +ISECTI                                           M44.  27
      KOFF14 = KOFF13 +ISECTI                                           M44.  28
      KOFF15 = KOFF14 +ISECTI                                           M44.  29
      KOFF16 = KOFF15 +ISECTI                                           M44.  30
      KOFF17 = KOFF16 +ISECTI                                                   
      KOFF18 = KOFF17 +ISECTI                                                   
      KOFF19 = KOFF18 +ISECTI                                                   
      KOFF20 = KOFF19                                                           
      IF ( KGROUP .NE. 1 ) WK1(KOFF19) =1                                       
      WK1(KOFF19) = WK1(KOFF19) + KGROUP                                        
      NCOUNT=IDM                                                                
      NPHS = N9                                                         M44.  38
      NPHS2 = N9 ** 2                                                   M44.  39
      NPHSU=(NPHS+1)*NPHS/2                                                     
      IBR2 = IBR - NPHS + 1                                             M44.  41
      IBR3 = IBR - NPHS + 3                                             M44.  42
      IFQ = IFQ + 1                                                             
      INFDLI(INOFF2 + IBR2) =                                           M44.  44
     1        IFQ                                                               
      INFDLI( INOFF1 + IBR2 ) =                                         M44.  46
     1              IDM                                                         
      INFDLI( INOFF4 + IBR2 ) = IDU                                             
      IF ( IPRSUP  .GE.  4 )                                            M44.  49
     1 WRITE (*,*) ' BEGIN TO READ Qi. NTLIN,IQ,IDM,IDU,IDT,IDQ= ',     M44.  50
     2                                 NTLIN,IQ,IDM,IDU,IDT,IDQ         M44.  51
      NNN1 = IADDRS                                                     M44.  52
      DO 902 I=1,NPHS2                                                          
      WK1(KOFF20+NCOUNT)=IDQ                                                    
      NP = 1                                                            M44.  55
      SCONST(IFQ) = QFD(NNN1)                                           M44.  56
      NNN1 = NNN1 + 1                                                   M44.  57
C      IF (NOUTPR .EQ. 0)                                               M44.  15
C     1 WRITE (*, 8262) NP , SCONST(IFQ)                                M44.  58
C 8262 FORMAT ( 24H+ Ti.  NP, SCONST(IFQ) =, I5,  E15.5  )              M44.  59
      WK1(KOFF20+NCOUNT) = NP                                                   
      J1 = 1                                                            M44.  61
 4888 J2 = J1 + 2                                                       M44.  62
      M =  NP                                                                   
      IF ( J2 .GT. M)  J2 = M                                           M44.  64
      JJ1 = J1 + IFQ                                                            
      JJ2 = J2 + IFQ                                                            
      IML = J1                                                                  
      DO 4844 JJ=JJ1,JJ2                                                        
      SCONST(JJ) = 0.0                                                          
      IML = IML + 1                                                             
 4844 CONTINUE                                                                  
C      IF (NOUTPR .EQ. 0)                                               M44.  16
C     1 WRITE (*, 4413)   ( SCONST(J), J=JJ1, JJ2 )                     M44.  72
C 4413 FORMAT ( 7H+ QK-I:, 3E14.5 )                                     M44.  73
      J1 = J1 + 3                                                               
      IF ( J1 .LE. M )  GO TO 4888                                              
C                                                                       M44.  76
      J1 = 1                                                            M44.  77
 4847 J2 = J1 + 2                                                       M44.  78
      M =  NP                                                                   
      IF ( J2 .GT. M)  J2 = M                                           M44.  80
      NN11 = JJ2 + J1                                                           
      NN13 = JJ2 + J2                                                           
      IML = J1                                                                  
      DO 4744 JJ=NN11,NN13                                                      
      SCONST(JJ) = 1.0                                                          
      IML = IML + 1                                                             
 4744 CONTINUE                                                                  
C      IF (NOUTPR .EQ. 0)                                               M44.  17
C     1 WRITE (*, 8414)  ( SCONST(J), J=J1, J2 )                        M44.  88
C 8414 FORMAT ( 7H+ QP-I:, 3E14.5 )                                     M44.  89
      J1 = J1 + 3                                                               
      IF ( J1 .LE. M )  GO TO 4847                                              
      IFQ = NN13 + 1                                                            
      NCOUNT=NCOUNT+1                                                           
      IDQ = IDQ + NP                                                            
902   CONTINUE                                                                  
      IDQ = IDQ -1                                                      M44.  96
      IDQ = 2 * IDQ                                                     M44.  97
      IDQ = IDQ + 1                                                     M44.  98
      IFQ = NN13                                                                
     1    + ( NN13 - IFSEM - NPHS2 )*3/2                                        
     2    + 2 * NPHS2                                                           
      IFSEM = IFQ                                                               
      INFDLI( INOFF3 + IBR2 ) =                                         M44. 103
     1                NN13                                                      
      IDM=IDM+NPHS2                                                             
      IDU=IDU+NPHSU                                                             
      IFKC=IFKC+N9                                                              
      IF ( IPRSUP  .GE.  4 )                                            M44. 109
     1 WRITE (*,*) 'Next bunch of coupled branches. ntlin=, kcount=',   M44. 110
     2                                              ntlin, kcount       M44. 111
      ITRANM = MODEL                                                    M44. 112
      MODEL = 0                                                         M44. 113
      KGROUP = 1                                                        M44. 114
50050 IF ( LENGTH(IBR1)  .EQ.  3 )  KTRLSW(3) = - IADDRS                M44. 116
      GO TO 25970                                                       M44. 117
15970 D1 = 2.0                                                          M44. 119
      D2 = 6.0                                                          M31. 419
      D3 = 1.0/SQRTZ(D1)                                                M31. 420
      D4 = 1.0/SQRTZ(D2)                                                M31. 421
      N1 = IADDRS                                                       M31. 422
      DO 25965  J = 1, 36                                               M31. 423
      IF ( J .GT. 9 )  GO TO 15975                                      M31. 424
      QFD(N1) = D4                                                      M31. 425
      GO TO 25960                                                       M31. 426
15975 IF ( J .GT. 12 )  GO TO 15980                                     M31. 427
      QFD(N1) = -D4                                                     M31. 428
      GO TO 25960                                                       M31. 429
15980 IF ( J .GT. 13 )  GO TO 15985                                     M31. 430
      QFD(N1) = D3                                                      M31. 431
      GO TO 25960                                                       M31. 432
15985 IF ( J .GT. 14 )  GO TO 15990                                     M31. 433
      QFD(N1) = -D3                                                     M31. 434
      GO TO 25960                                                       M31. 435
15990 IF ( J .GT. 18 )  GO TO 15991                                     M31. 436
      QFD(N1) = 0.0                                                     M31. 437
      GO TO 25960                                                       M31. 438
15991 IF ( J .GT. 20 )  GO TO 15992                                     M31. 439
      QFD(N1) = D4                                                      M31. 440
      GO TO 25960                                                       M31. 441
15992 IF ( J .GT. 21)  GO TO 15993                                      M31. 442
      QFD(N1) = - D1 * D4                                               M31. 443
      GO TO 25960                                                       M31. 444
15993 IF ( J .GT. 27)  GO TO 15994                                      M31. 445
      QFD(N1) = 0.0                                                     M31. 446
      GO TO 25960                                                       M31. 447
15994 IF ( J .GT. 28 )  GO TO 15995                                     M31. 448
      QFD(N1) = D3                                                      M31. 449
      GO TO 25960                                                       M31. 450
15995 IF ( J .GT. 29 )  GO TO 15996                                     M31. 451
      QFD(N1) = -D3                                                     M31. 452
      GO TO 25960                                                       M31. 453
15996 IF ( J .GT. 33 )  GO TO 15997                                     M31. 454
      QFD(N1) = 0.0                                                     M31. 455
      GO TO 25960                                                       M31. 456
15997 IF ( J .GT. 35 )  GO TO 15999                                     M33. 156
      QFD(N1) = D4                                                      M31. 458
      GO TO 25960                                                       M31. 459
15999 QFD(N1) = - D1 * D4                                               M31. 463
25960 SFD(N1) = 0.0                                                     M31. 464
      N1 = N1 + 1                                                       M31. 465
25965 CONTINUE                                                          M31. 466
25970 IF (IPRSUP .LT. 1 ) GO TO 5971                                    M31. 467
      N1 = IADDRS                                                       M29.1248
      N2 = N1 + N9SQ -1                                                 M29.1249
      WRITE (LUNIT6, 15968)  N1, N2, (QFD(I), I=N1, N2),                M32.1075
     1                       (SFD(I), I=N1, N2)                         M32.1076
15968 FORMAT ( 26H QFD(I) AND SFD(I) FOR I =, I6, 4H TO ,I6, 4H ARE,/,  M32.1077
     1           (1X, 8E15.7) )                                         M29.1252
      WRITE (LUNIT6, 25968) IBR1, IADDRS, LITYPE(IBR1)                  M29.1253
25968 FORMAT ( 44H AT 25968, IBR1, IADDRS AND LITYPE(IBR1) ARE,         M33. 157
     1         3I10)                                                    M29.1255
 5971 IADDRS = IADD + 1                                                 M29.1256
36621 ITRANM = 0                                                        M29.1257
      IF ( BUS1  .NE.  BLANK )   GO TO 110                              M29.1258
      IF( BUS2 .NE. BLANK )  GO TO 110                                  M42.1564
      IF(ITYPE.EQ.0)GOTO 4199                                           M42.1565
      IF ( ITYPE  .LT.  0 )   GO TO 110                                 M20. 805
      IF(ICAS.EQ.0)NPHCAS=ITYPE-51                                      M42.1566
      ICAS=-1                                                           M42.1567
      GO TO 147                                                         M42.1568
 4199 CONTINUE
C           =================  line-model F-scan building of network ===========
      IF ( KEXACT .NE. 88333 ) GO TO 5823
C     WRITE (*,*) ' Check F-scan data.  NTOT, IBR =',  ntot, ibr
      IF ( NFSCAN .GT. 0 ) GO TO 5823
      IF ( LASTOV .NE. 44 )  NUMRUN = NUMRUN + 1
      NFSCAN = NPHLMT
c     IF ( NFSCAN*2 .EQ. NTOT - 1  .AND.
C    1     NFSCAN .EQ. IBR ) GO TO 1583
C      IF ( NFSCAN*2 .EQ. NTOT-1 )  GO TO 1583
C      WRITE (*,*) ' Error F-scan data.  NTOT, IBR =',  ntot, ibr
C      stop
C 1583 DO 5281  M= 2, NTOT, 2
C      N9 = M / 2
C      CHAR6 = 'SEND_'//ALPHAN(N9:N9)
C      READ(CHAR6,4207) BUS(M)
C      CHAR6 = 'RECV_'//ALPHAN(N9:N9)
C      READ(CHAR6,4207) BUS(M+1)
C 4207 FORMAT ( A6 )
C  5281 continue

      DO 2500  M = 1, 2*NPHLMT
      CHRPAD(M)(1:6) = CHLMFS(M)(1:6)
      DO 2480  K = 1, 6
 2480 IF ( CHLMFS(M)(K:K) .EQ.  ' ' )
     1     CHRPAD(M)(K:K) = '#'
 2500 CONTINUE
      IF ( NUMRUN .GE. 2 )  GO TO 2540
      IF ( NPHLMT .GT. 3 )  GO TO 2510
      WRITE (BUFF77, 3629) NPHLMT, (CHRPAD(M)(1:6), M=1, 2*NPHLMT)
 3629 FORMAT ( '$INCLUDE, [TSU]LINESCANZ', I1, '.THL ',
C  3629 FORMAT ( '$INCLUDE, DRD5:[BOB]LINESCANZ', I1, '.THL ',
     1 6(A6,1X)  )    
      GO TO 2580
 2510 DO 2518  I = 1, 2
      IF ( I .EQ. 1 )
     1  WRITE (BUFF77, 2515) NPHLMT, (CHRPAD(M)(1:6), M=1, 7)
 2515 FORMAT ( '$INCLUDE,[TSU]LINESCANZ', I1, '.THL ',
     1 7(A6,1X), '$$'  )
      IF (I .EQ. 2)  WRITE(BUFF77, 2516)(CHRPAD(M)(1:6),M=8,2*NPHLMT)
 2516 FORMAT ('C ', 11(A6,1X) )
      NUMDCD = NUMDCD  +  1
      CALL INLMFS
 2518 CONTINUE
      GO TO 3988
 2540 IF ( NUMRUN .GT. 2 )   GO TO 2560
      IF ( NPHLMT .GT. 3 )   GO TO 2545
      WRITE (BUFF77, 3639) NPHLMT, (CHRPAD(M)(1:6), M=1, 2*NPHLMT)
 3639 FORMAT ( '$INCLUDE, [TSU]LINESCANP', I1,  '.THL ',
     1 6(A6, 1X) ) 
      GO TO 2580
 2545 DO 2558  I = 1, 2
      IF ( I .EQ. 1 )
     1  WRITE (BUFF77, 2550) NPHLMT, (CHRPAD(M)(1:6), M=1, 7)
 2550 FORMAT ( '$INCLUDE,[TSU]LINESCANP', I1,  '.THL ',
     1 7(A6, 1X), '$$' )
      IF (I .EQ. 2)  WRITE(BUFF77, 2516)(CHRPAD(M)(1:6),M=8,2*NPHLMT)
      NUMDCD = NUMDCD  +  1
      CALL INLMFS
 2558 CONTINUE
      GO TO 3988
 2560 DO 2568  I = 1, 2
      IF ( I .EQ. 1 )
     1 WRITE (BUFF77, 3649) NPHLMT, (CHRPAD(M)(1:6), M=1, 7)
 3649 FORMAT ( '$INCLUDE,[TSU]LINESCANM', I1,  '.THL ' ,
     1 7(A6,1X), '$$')
      IF (I .EQ. 2)  WRITE(BUFF77, 2516)(CHRPAD(M)(1:6),M=8,2*NPHLMT) 
      NUMDCD = NUMDCD  +  1
      CALL INLMFS
 2568 CONTINUE
      GO TO 3988
C     write (*,*) ' Internally encoded card '
 2580 NUMDCD = NUMDCD  +  1
      CALL INLMFS
      GO TO 3988
C           =================  End line-model F-scan building of network =======
 5823 IF ( NOUTPR  .EQ.  0 )                                            M20. 806
     1 WRITE (KUNIT6, 4198)                                             M35. 704
 4198 FORMAT (  37H+BLANK CARD TERMINATING BRANCH CARDS.    )           M20. 808
      NFSCAN = 0       ! reset NFSCAN for 2nd and 3rd LMFS cases
      IF ( NTOT  .LE.  LBUS )   GO TO 4192                              M29.1259
      IPRINT = 1                                                        M42.1569
      LSTAT(19)=110                                                     M42.1570
      GO TO 9000                                                        M42.1571
 4192 IF ( KSAT  .EQ.  0 )   GO TO 200                                  M31. 470
  110 N1=0                                                              M42.1572
      N2=0                                                              M42.1573
      N3=0                                                              M42.1574
      N4=0                                                              M42.1575
      ITYOLD = ITYPE                                                    M29.1260
      DO 120 I=1,NTOT                                                   M42.1576
      IF(BUS1 .EQ. BUS(I)) N1=I                                         M42.1577
      IF(BUS2 .EQ. BUS(I)) N2=I                                         M42.1578
      IF(BUS3 .EQ. BUS(I)) N3=I                                                 
      IF(BUS4 .EQ. BUS(I)) N4=I                                         M42.1580
  120 CONTINUE                                                          M42.1581
      IF ( MODEL .EQ. 0 ) GO TO 4005                                    M29.1261
C     ITYPE GETS NEGATIVE SIGN INTERNALLY FOR 2ND TO MTH PHASE          M29.1262
C     OF A UNTRANSPOSED LINE                                            M29.1263
      IF ( ICHECK .EQ. 3 )  ITYPE = -IABSZ(ITYPE)                       M29.1264
 4005 IF( BUS1 .NE. TEXT7 )  GO TO 4110                                 M42.1582
      IF( BUS2 .NE. TEXT8 )  GO TO 4110                                 M42.1583
      IF( KSAT .GT. 0 )  GO TO 4040                                     M42.1584
      IF( BUS3 .NE. TEXT9 )  GO TO 4003                                 M42.1585
      IF( BUS4 .NE. TEXT10 )  GO TO 4003                                M42.1586
      DECODE(80, 44002, ABUFF)  BUS6, YZERO                             M42.1587
44002 FORMAT( 26X, A6, E6.2 )                                           M42.1588
      IF ( NOUTPR  .EQ.  0 )                                            M20. 809
     1 WRITE (KUNIT6, 3996 )  BUS6, YZERO                               M35. 705
 3996 FORMAT ( 28H+3-PHASE XFORMER REQUEST.  ', A6, 3H'  , E12.4  )     M42.1589
      IF( YZERO .GT. 0.0 )  GO TO 3998                                  M42.1590
      KILL = 52                                                         M42.1591
      LSTAT(16) = 1                                                     M42.1592
      LSTAT(19) = 3998                                                  M42.1593
      GO TO 9200                                                        M42.1594
 3998 KPH = 1                                                           M42.1595
      GO TO 100                                                         M42.1596
 4003 CONTINUE                                                          M42.1597
      DECODE(80, 4013, ABUFF)  TR(IT), TX(IT), BUS5, RMAG, IOUTMG       M42.1598
 4013 FORMAT( 26X, 2E6.2, A6, E6.2, 29X, I1 )                           M42.1599
      IF ( IOUTMG  .LT.  2 )   GO TO 3400                               M18. 281
      NV = NV + 1                                                       M42.1600
      IF ( NV  .LE.  LSIZ12 )   GO TO 3400                              M38.1102
      IPRINT = 11                                                       M42.1601
      LSTAT(19)=3400                                                    M17. 210
      GO TO 9000                                                        M42.1602
 3400 NTOT = NTOT + 1                                                   M18. 283
      BUS(NTOT) = BUS5                                                  M42.1603
      IF ( BUS5  .NE.  BLANK )   GO TO 3401                             M18. 284
      KILL = 161                                                        M15. 492
      LSTAT(19) = 3401                                                  M18. 285
      GO TO 9200                                                        M15. 494
 3401 NODTOP = NTOT                                                     M18. 286
      KSWTCH = KSWTCH + 1                                               M42.1604
      IF( KSWTCH .LE. LSWTCH )  GO TO 4004                              M42.1605
      KILL = 47                                                         M42.1606
      LSTAT(19) = 4010                                                  M42.1607
      LSTAT(16) = LSWTCH                                                M42.1608
      GO TO 9200                                                        M42.1609
 4004 NEXTSW(KSWTCH) = NTOT                                             M32.1078
      IF( KPH .GT. 0 )  KPH = KPH + 1                                   M42.1610
      IF( N3 .NE. 1 )  GO TO 4002                                       M42.1611
      IF ( NOUTPR  .EQ.  0 )                                            M20. 811
     1 WRITE (KUNIT6, 4010)  TR(IT), TX(IT), RMAG                       M35. 706
 4010 FORMAT ( 14H+SAT. XFORMER.  , 2X, 3E11.3 )                        M42.1612
      KSAT =-1                                                          M42.1613
      INONL = INONL + 1                                                 M42.1614
      IF( INONL .LE. LNONL )  GO TO 4011                                M42.1615
 4012 IPRINT = 9                                                        M42.1616
      LSTAT(19)=4012                                                    M42.1617
      GO TO 9000                                                        M42.1618
 4011 VZERO(INONL) = TR(IT)                                             M42.1619
      ANONL(INONL) = TX(IT)                                             M42.1620
      CI1 = 0.0                                                         M42.1621
      CK1 = 0.0                                                         M42.1622
      NONLAD(INONL) = ICHAR + 1                                         M42.1623
      ITYPE = 98                                                        M42.1624
      NLTYPE(INONL) = -98                                               M42.1625
      CURR(INONL) = 1.0                                                 M16. 396
      IF( IPRSUP .GE. 2 )                                               M42.1626
     1WRITE(LUNIT6, 4014)  KSAT, IT, ICHAR, NTOT, IT3, INONL            M17. 213
 4014 FORMAT( /, 9H AT 4014   , 7I10 )                                  M42.1627
      LSTAT(18) = 187                                                   M28.2838
      GO TO 186                                                         M28.2839
 4002 IF ( NOUTPR  .EQ.  0 )                                            M20. 813
     1 WRITE (KUNIT6, 74003)   BUS3                                     M35. 707
74003 FORMAT( 40H+TRANSFORMER COPY USING REFERENCE NAME ', A6, 2H'.   ) M42.1628
      KTREF = 1                                                         M42.1629
 4006 IF( KTREF .LE. KSWTCH )  GO TO 4007                               M42.1630
      KILL = 48                                                         M42.1631
      LSTAT(19) = 4007                                                  M42.1632
      GO TO 9200                                                        M42.1633
 4007 IF( N3 .EQ. NEXTSW(KTREF) )  GO TO 4008                           M32.1079
      KTREF = KTREF + 1                                                 M42.1634
      GO TO 4006                                                        M42.1635
 4008 I = ISOURC(KTREF)                                                 M42.1636
      TCLOSE(KSWTCH) = TCLOSE(KTREF)                                    M42.1637
      TOPEN(KSWTCH) = TOPEN(KTREF)                                      M32.1080
      IF( I .LE. 0 )  GO TO 4027                                        M42.1638
      INONL = INONL + 1                                                 M42.1639
      IF( INONL .GT. LNONL )  GO TO 4012                                M42.1640
      NLTYPE(INONL) = NLTYPE(I)                                         M42.1641
      NONLAD(INONL) = NONLAD(I)                                         M42.1642
      NONLE(INONL) = NONLE(I)                                           M42.1643
      VZERO(INONL) = VZERO(I)                                           M42.1644
      ANONL(INONL) = ANONL(I)                                           M42.1645
      VNONL(INONL) = 0.0                                                M42.1646
      CURR(INONL) = CURR(I)                                             M16. 397
      ISOURC(KSWTCH) = INONL                                            M42.1647
 4027 KSAT = 1                                                          M42.1648
      GO TO 100                                                         M42.1649
 4040 IF( ITYPE .EQ. KSAT )  GO TO 4045                                 M42.1650
      KMSWIT(KSWTCH) = IBR                                              M32.1081
      ICRIT(KSWTCH) = KSAT - 1                                          M42.1651
      IF ( KSAT  .GE.  3 )   GO TO 4093                                 M22.2532
      BUS1 = TRASH                                                      M22.2533
      LSTAT(19) = 4093                                                  M22.2534
      GO TO 4067                                                        M22.2535
 4093 KSAT = 0                                                          M22.2536
      IF( ISOURC(KSWTCH) .GT. 0 )  NUM99 = NUM99 + 1                    M42.1652
      IF( KTREF .EQ. 0 )  GO TO 4041                                    M42.1653
      LSTAT(19) = 4040                                                  M42.1654
      GO TO 4067                                                        M42.1655
 4041 IF( KPH .EQ. 4 )  KPH = 0                                         M42.1656
      IF ( IPRSUP  .LT.  3 )   GO TO 84041                              M12. 250
      WRITE(LUNIT6, 34041)  KSWTCH, IBR, IT, INONL, ICHAR               M17. 215
34041 FORMAT( /, 66H DONE WITH SAT. XFORMER AT 34041.  KSWTCH, IBR, IT, M42.1657
     1INONL, ICHAR=  , 5I10 )                                           M42.1658
      WRITE(LUNIT6, 44041)  ( I, KBUS(I), MBUS(I), NR(I), LENGTH(I),    M17. 216
     1 KODEBR(I), BUS(I), I=1, IBR )                                    M42.1659
44041 FORMAT( /,  21H LINEAR BRANCH TABLE.   ,/,                        M42.1660
     1 13X, 3HROW, 11X, 4HKBUS, 11X, 4HMBUS, 13X, 2HNR, 9X, 6HLENGTH,   M42.1661
     2 9X, 6HKODEBR, 12X, 3HBUS, /, ( 1X, 6I15, A15 ) )                 M42.1662
      N = ICHAR                                                         M42.1663
      IF( INONL .GT. N )  N = INONL                                     M42.1664
      WRITE(LUNIT6, 54041)  ( I, NONLK(I), NONLM(I),                    M17. 217
     1                   NLTYPE(I), NONLAD(I), NONLE(I), ANONL(I),      M42.1665
     2 VZERO(I), VCHAR(I), GSLOPE(I), CCHAR(I), I=1, N )                M42.1666
54041 FORMAT( /, 18H N.L. ELEM ARRAYS.   ,/,                            M42.1667
     1 6X, 3HROW, 3X, 5HNONLK, 3X, 5HNONLM, 2X, 6HNLTYPE, 2X, 6HNONLAD, M42.1668
     2 3X, 5HNONLE,10X, 5HANONL,10X, 5HVZERO,10X, 5HVCHAR, 9X, 6HGSLOPE,M42.1669
     3 10X, 5HCCHAR, /,  ( 1X, 6I8, 5E15.5 ) )                          M42.1670
      WRITE (LUNIT6, 64041)  ( MODSWT(I), KMSWIT(I), ISOURC(I),         M32.1082
     1 NEXTSW(I), ICRIT(I), KPOS(I), TCLOSE(I), TOPEN(I), I=1, KSWTCH ) M32.1083
64041 FORMAT( /,  33H SWITCH ARRAYS WITH XFORMER INFO.   ,/,            M42.1671
     1 11X, 6HMODSWT, 4X, 6HKMSWIT, 4X, 6HISOURC,                       M32.1084
     2 4X, 6HNEXTSW, 5X, 5HICRIT, 6X, 4HKPOS, 9X, 6HTCLOSE,             M32.1085
     3 9X, 6H TOPEN, /, ( 7X, 6I10, 2E15.5 ) )                          M32.1086
      WRITE(LUNIT6, 74041)  ( I, TR(I), TX(I), C(I), I=1, IT )          M17. 219
74041 FORMAT( /, 31H LUMPED PARAM STORAGE TR, TX, C   ,/,               M42.1672
     1 ( 1X, I10, 3E20.6 ) )                                            M42.1673
84041 IF ( N1  .NE.  1 )   GO TO 4005                                   M12. 251
      IF ( N2  .NE.  1 )   GO TO 4005                                   M05. 330
      GO TO 200                                                         M05. 331
 4045 KSAT = KSAT + 1                                                   M42.1674
      IF ( NOUTPR  .EQ.  0 )                                            M20. 815
     1 WRITE (KUNIT6, 74046)  ITYPE                                     M35. 708
74046 FORMAT ( 10H+  WINDING, I2, 1H.  )                                M42.1675
      IF( ITYPE .GT. 1 )  GO TO 4034                                    M42.1676
      MODSWT(KSWTCH) = IBR + 1                                          M32.1087
      IF( IOUTMG .LE. 1 )  GO TO 14045                                  M42.1677
      IBRNCH(NV) = NODTOP                                               M42.1678
      JBRNCH(NV) = N2                                                   M42.1679
      IF( IOUTMG .EQ. 2 )  IOUTMG = 0                                   M42.1680
14045 IF( KPH .LE. 0 )  GO TO 4034                                      M42.1681
      IF( KPH .GT. 2 )  GO TO 4031                                      M42.1682
      KEEPT = NODTOP                                                    M42.1683
      KEEPB = N2                                                        M42.1684
      GO TO 4034                                                        M42.1685
 4031 CALL IBRINC                                                       VAX.  35
      KBUS(IBR) = NODTOP                                                M42.1687
      MBUS(IBR) = N2                                                    M42.1688
      IF( KPH .GT. 3 )  GO TO 4033                                      M42.1689
      NR(IBR) = IT                                                      M42.1690
      KODEBR(IBR) = 1                                                   M42.1691
      LENGTH(IBR) = 2                                                   M42.1692
      TR(IT) = YZERO                                                    M42.1693
      IDEAL = IT                                                        M42.1694
      CALL MOVER0 ( TX(IT), LL3 )                                       M22.2537
      CALL MOVER0 ( C(IT), LL3 )                                        M22.2538
      IT = IT + 1                                                       M42.1695
      CALL IBRINC                                                       VAX.  36
      NR(IBR) = IT                                                      M42.1697
      TR(IT) = -YZERO                                                   M42.1698
      IT = IT + 1                                                       M42.1699
      TR(IT) = YZERO                                                    M42.1700
      IT = IT + 1                                                       M42.1701
      KBUS(IBR) = KEEPB                                                 M42.1702
      NTOT = NTOT + 1                                                   M42.1703
      BUS(NTOT ) = BUS6                                                 M42.1704
      MBUS(IBR) = NTOT                                                  M42.1705
      KEEPB = NTOT                                                      M42.1706
64033 MODSWT(KSWTCH) = IBR + 1                                          M32.1088
      KODEBR(IBR) = 1                                                   M42.1707
      IF( IPRSUP .GE. 1 )                                               M42.1708
     1 WRITE(LUNIT6, 54033)  YZERO, KSWTCH, IBR, IT, INONL, ICHAR,      M22.2539
     2  KPH, KSAT,  IDEAL, KEEPB, KEEPT, NTOT, NODTOP, N1, N2           M22.2540
54033 FORMAT( /,  47H IDEAL XFORMER INTERCONNECT, AT 54033.  YZERO =,   M42.1709
     1 E15.6, 5I12, /, ( 1X, 10I12 ) )                                  M42.1710
      GO TO 4034                                                        M42.1711
 4033 NR(IBR) = IDEAL                                                   M42.1712
      LENGTH(IBR) = -2                                                  M42.1713
      KODEBR(IBR) = 1                                                   M42.1714
      CALL IBRINC                                                       VAX.  37
      KBUS(IBR) = KEEPB                                                 M42.1716
      MBUS(IBR) = KEEPT                                                 M42.1717
      NR(IBR) = IDEAL + 1                                               M42.1718
      GO TO 64033                                                       M42.1719
 4034 IF( KTREF .EQ. 0 )  GO TO 4044                                    M42.1720
      IF( KSAT .GT. 2 )  GO TO 4065                                     M42.1721
      IF( KPH .EQ. 2 )    YZERO = 2.0 * YZERO / TCLOSE(KTREF)**2        M42.1722
      IF( ISOURC(KTREF)  .LE.  0 )  GO TO 34035                         M42.1723
      NONLK(INONL) = NODTOP                                             M42.1724
      NONLM(INONL) = N2                                                 M42.1725
      IF( IOUTMG .GT. 0 )  NONLM(INONL) = -NONLM(INONL)                 M42.1726
34035 IREF = MODSWT(KTREF)                                              M32.1089
      CALL IBRINC                                                       VAX.  38
      NR(IBR) = NR(IREF)                                                M42.1728
      LENGTH(IBR) = -1                                                  M42.1729
      KBUS(IBR) = N1                                                    M42.1730
      MBUS(IBR) = NODTOP                                                M42.1731
      DECODE (80, 142, ABUFF)  IOUT                                     M05. 332
      IF( IOUT .GT. 0 )   MBUS(IBR) = -MBUS(IBR)                        M42.1732
      KPOS(KSWTCH) = N2                                                 M42.1733
44035 IREF = IREF + 1                                                   M42.1734
      N5 = IABS( LENGTH(IREF) )                                         M42.1735
      IF( IPRSUP .GT. 3 )                                               M42.1736
     1 WRITE(LUNIT6, 74035)  IBR, IT, KTREF, IREF, N5, KBUS(IBR),       M22.2541
     2  MBUS(IBR)                                                       M22.2542
74035 FORMAT( /,  51H REF. COMP. COPY OF R1-L1 AND MAG. BRANCH, AT 74035M42.1737
     1  ,/, 1X, 8I15 )                                                  M42.1738
      IF( N5 .EQ. 2 )  GO TO 100                                        M42.1739
      CALL IBRINC                                                       VAX.  39
      NR(IBR) = NR(IREF)                                                M42.1741
      LENGTH(IBR) = -1                                                  M42.1742
      KBUS(IBR) = NODTOP                                                M42.1743
      MBUS(IBR) = N2                                                    M42.1744
      I = IABS( NR(IBR) )                                               M19. 346
      IF(  TR(I)   .NE.   0.0 )  GO TO 44035                            M42.1745
      IF( IOUTMG .GT. 0 )  MBUS(IBR) = -MBUS(IBR)                       M42.1746
      GO TO 44035                                                       M42.1747
 4044 CONTINUE                                                          M42.1748
      DECODE (80, 4046, ABUFF)  TR(IT), TX(IT), C(IT), IOUT             M42.1749
 4046 FORMAT( 26X, 3E6.2, 35X, I1 )                                     M42.1750
      IF ( NOUTPR  .EQ.  0 )                                            M20. 817
     1 WRITE (KUNIT6, 54047)  ITYPE, TR(IT), TX(IT), C(IT)              M35. 709
54047 FORMAT ( 10H+  WINDING, I2, 2H. ,  3E12.4 )                       M35. 710
      I = IT + 1                                                        M42.1751
      CALL MOVER0 ( TR(I), LL2 )                                        M22.2543
      CALL MOVER0 ( TX(I), LL2 )                                        M22.2544
      CALL MOVER0 ( C(I), LL2 )                                         M22.2545
      IF( C(IT)  .GT.  0.0 )  GO TO 4047                                M42.1752
      KILL = 52                                                         M42.1753
      LSTAT(19) = 4047                                                  M42.1754
      LSTAT(16) = 3                                                     M42.1755
      GO TO 9200                                                        M42.1756
 4047 IF( KSAT .GT. 2 )  GO TO 4070                                     M42.1757
      TURN1 = C(IT)                                                     M42.1758
      IF( KPH .EQ. 2 )    YZERO = 2.0 * YZERO / TURN1**2                M42.1759
      KPOS(KSWTCH) = N2                                                 M42.1760
      TCLOSE(KSWTCH) = TURN1                                            M42.1761
      C(IT) = 0.0                                                       M42.1762
      IF( TR(IT) .NE. 0.0 )  GO TO 4049                                 M42.1763
      IF( TX(IT) .NE. 0.0 )  GO TO 4049                                 M42.1764
      LSTAT(19) = 4049                                                  M42.1765
      KILL = 52                                                         M42.1766
      LSTAT(16) = 2                                                     M42.1767
      GO TO 9200                                                        M42.1768
 4049 CALL IBRINC                                                       VAX.  40
      KBUS(IBR) = N1                                                    M42.1770
      MBUS(IBR) = NODTOP                                                M42.1771
      IF( IOUT .GT. 0 )   MBUS(IBR) = -MBUS(IBR)                        M42.1772
      LENGTH(IBR) = 1                                                   M42.1773
      NR(IBR) = -IT                                                     M42.1774
      IT = IT + 1                                                       M42.1775
      IF( RMAG .EQ. 0.0 )  GO TO 4052                                   M42.1776
      TR(IT) = RMAG                                                     M42.1777
 4048 CALL IBRINC                                                       VAX.  41
      KBUS(IBR) = NODTOP                                                M42.1779
      MBUS(IBR) = N2                                                    M42.1780
      LENGTH(IBR) = 1                                                   M42.1781
      NR(IBR) = -IT                                                     M42.1782
      IT = IT + 1                                                       M42.1783
      IF(  TX(IT-1)  .NE.  0.0 )  GO TO 100                             M42.1784
 4052 IT3 = NONLAD(INONL)                                               M42.1785
      IF( KPH .LE. 2 )  GO TO 4054                                      M42.1786
      IF(  TCLOSE(KSWTCH)   .EQ.   TCLOSE(KSWTCH-1)  )   GO TO 4054     M42.1787
24054 KILL = 52                                                         M42.1788
      LSTAT(19) = 4054                                                  M42.1789
      LSTAT(16) = 4                                                     M42.1790
      GO TO 9200                                                        M42.1791
 4054 NCOIL =-NONLE(INONL) - IT3                                        M42.1792
      IF( NCOIL .GT. 0 )  GO TO 4056                                    M42.1793
      ICHAR = IT3 - 1                                                   M42.1794
      INONL = INONL - 1                                                 M42.1795
      IF( IPRSUP .GE. 2 )                                               M42.1796
     1 WRITE(LUNIT6, 4053)  IT, IBR, ICHAR, INONL, KSWTCH, NCOIL,       M22.2546
     2  KSAT, KPH                                                       M22.2547
 4053 FORMAT( /,  37H MAG. BR. NOT ACTUALLY N.L., AT 4053.  ,/, 1X,8I15)M42.1797
      IF( NCOIL .LT. 0 )  GO TO 100                                     M42.1798
      TX(IT) = DELTA2 / GSLOPE(IT3) * XUNITS                            M42.1799
      IF( IOUTMG .GT. 0 )  N2 = -N2                                     M42.1800
      GO TO 4048                                                        M42.1801
 4056 NONLK(INONL) = NODTOP                                             M42.1802
      NONLM(INONL) = N2                                                 M42.1803
      IF( IOUTMG .GT. 0 )  NONLM(INONL) = -NONLM(INONL)                 M42.1804
      ISOURC(KSWTCH) = INONL                                            M42.1805
      GO TO 100                                                         M42.1806
 4065 CALL IBRINC                                                       VAX.  42
      KBUS(IBR) = N1                                                    M42.1808
      MBUS(IBR) = N2                                                    M42.1809
      NR(IBR) = NR(IREF)                                                M42.1810
      KODEBR(IBR) = 1                                                   M42.1811
      LENGTH(IBR) = -IABS( LENGTH(IREF) )                               M42.1812
      CALL IBRINC                                                       VAX.  43
      KBUS(IBR) = NEXTSW(KSWTCH)                                        M32.1090
      MBUS(IBR) = KPOS(KSWTCH)                                          M42.1814
      KODEBR(IBR) = 1                                                   M42.1815
      IREF = IREF + 1                                                   M42.1816
      NR(IBR) = NR(IREF)                                                M42.1817
      IREF = IREF + 1                                                   M42.1818
      IF( ITYPE .LE. ICRIT(KTREF) )  GO TO 4068                         M42.1819
      LSTAT(19) = 4067                                                  M42.1820
 4067 KILL = 49                                                         M42.1821
      J = NEXTSW(KTREF)                                                 M32.1091
      BUS1 = BUS(J)                                                     M42.1822
      LSTAT(13) = KSAT                                                  M39. 987
      LSTAT(16) = ICRIT(KTREF)                                          M42.1823
      GO TO 9200                                                        M42.1824
 4068 IF( ITYPE .LT. ICRIT(KTREF) )  GO TO 100                          M42.1825
      KTREF = 0                                                         M42.1826
      GO TO 100                                                         M42.1827
 4070 D1 = C(IT) / TURN1                                                M42.1828
      IF( KSAT .GT. 3 )  GO TO 4073                                     M42.1829
      TOPEN(KSWTCH) = C(IT)                                             M32.1092
      IF( KPH .LE. 2 )  GO TO 4073                                      M42.1830
      IF( TOPEN(KSWTCH)  .NE.  TOPEN(KSWTCH-1) )  GO TO 24054           M32.1093
 4073 IF( TX(IT)  .NE.  0.0 )  GO TO 4071                               M42.1831
      KILL = 52                                                         M42.1832
      LSTAT(19) = 4071                                                  M42.1833
      LSTAT(16) = 2                                                     M42.1834
      GO TO 9200                                                        M42.1835
 4071 C(IT) = 0.0                                                       M42.1836
      TX(IT) = TX(IT) / XUNITS                                          M42.1837
      CUT1 = 1.0 / TX(IT)                                               M42.1838
      CUT2 = CUT1*D1                                                    M42.1839
      CUT3 = -TR(IT) * CUT1                                             M42.1840
      TR(IT) = CUT1                                                     M42.1841
      TX(IT) = CUT3                                                     M42.1842
      CALL IBRINC                                                       VAX.  44
      IF( IPRSUP .GE. 2 )                                               M42.1844
     1WRITE(LUNIT6, 4076)  IT, D1, CUT1, CUT2, TR(IT), TX(IT)           M17. 228
 4076 FORMAT( /, 22H ROW 1 OF 2X2 AT 4076  , I10, 5E15.4  , /,1X)       M15. 496
      KBUS(IBR) = N1                                                    M42.1845
      MBUS(IBR) = N2                                                    M42.1846
      LENGTH(IBR) = 2                                                   M42.1847
      NR(IBR) = IT                                                      M42.1848
      KODEBR(IBR) = 1                                                   M42.1849
      CALL IBRINC                                                       VAX.  45
      IT = IT + 1                                                       M42.1851
      KBUS(IBR) = NEXTSW(KSWTCH)                                        M32.1094
      MBUS(IBR) = KPOS(KSWTCH)                                          M42.1852
      KODEBR(IBR) = 1                                                   M42.1853
      NR(IBR) = IT                                                      M42.1854
      TR(IT) = -CUT2                                                    M42.1855
      IT = IT + 1                                                       M42.1856
      TR(IT) = D1 * CUT2                                                M42.1857
      TX(IT) = CUT3                                                     M42.1858
      IF( IPRSUP .GE. 2 )                                               M42.1859
     1WRITE(LUNIT6, 4084)  IT                                           M17. 229
 4084 FORMAT( /, 22H ROW 2 OF 2X2 AT 4084    , I10, /,1X)               M15. 497
      IT = IT + 1                                                       M42.1860
      GO TO 100                                                         M42.1861
 4110 IF( BUS1 .NE. TEXT3 )  GO TO 4112                                 M42.1862
      KREQAB = 1                                                        M42.1863
      IF ( NOUTPR  .EQ.  0 )                                            M20. 819
     1 WRITE (KUNIT6, 4111)                                             M35. 711
 4111 FORMAT(  47H+BEGIN COUPLED, LUMPED ELEMENTS USING (A), (B).  )    M42.1864
      GO TO 100                                                         M42.1865
 4112 IF( BUS1 .NE. TEXT4 )  GO TO 4114                                 M42.1866
      KREQAB = 0                                                        M42.1867
      IF ( NOUTPR  .EQ.  0 )                                            M20. 821
     1 WRITE (KUNIT6, 4113)                                             M35. 712
 4113 FORMAT(  47H+BEGIN COUPLED, LUMPED ELEMENTS USING (R), (L).  )    M42.1868
      GO TO 100                                                         M42.1869
 4114 IF ( BUS1  .NE.  TEXT5 )   GO TO 7642                             M18. 287
      IF ( BUS2  .NE.  TEXT6 )   GO TO 7642                             M18. 288
      ICAS=1                                                            M42.1870
      IPRSOV(36) = IT                                                   M39. 988
      IPRSOV(35) = IBR + 1                                              M39. 989
      DECODE(80,764,ABUFF)NPHCAS,FREQCS                                 M42.1871
      IF ( NOUTPR  .EQ.  0 )                                            M20. 823
     1 WRITE (KUNIT6, 76701)  NPHCAS, FREQCS                            M35. 713
76701 FORMAT ( 25H+CASCADED-PI HEADER CARD.  , I10,  2X, E12.4  )       M42.1872
  764 FORMAT(26X,I6,E6.2)                                               M42.1873
      IDUMY = 3 * NPHCAS                                                M42.1874
      IDUMY = IDUMY * (IDUMY+1) / 2                                     M42.1875
      IF ( IDUMY  .LT.  LPAST )   GO TO 7671                            M18. 289
      LSTAT(19)=764                                                     M42.1876
      LSTAT(15) = 1                                                     M42.1877
      GO TO 76724                                                       M42.1878
 7671 IDUMY = NPHCAS * (NPHCAS+1) / 2                                   M18. 290
      IF(IDUMY.LT.LBUS)GOTO100                                          M42.1879
      LSTAT(19)=7671                                                    M17. 233
      LSTAT(15) = 2                                                     M42.1880
76724 KILL=53                                                           M42.1881
      LSTAT(16) = IDUMY                                                 M42.1882
      LSTAT(12)=NPHCAS                                                  M42.1883
      GOTO9200                                                          M42.1884
 7642 IF( ISGFD .LT. 0 ) GO TO 15                                       M32.1095
      IF( BUS1 .NE. TEXT11 .OR. BUS2 .NE. TEXT14 ) GO TO 15             M32.1096
      LENGTH(IBR1+1) = -666                                             M32.1097
      CALL FDDATA ( IKF, ISFD, IBF )                                    M32.1098
      IF( KILL .GT. 0 ) GO TO 9200                                      M32.1099
      ISGFD = -100                                                      M32.1100
      GO TO 100                                                         M32.1101
   15 IPRINT = 1                                                                
      IF(N1.NE.0) GO TO 130                                                     
      NTOT=NTOT+1                                                               
      LSTAT(19)=7642                                                    M17. 234
      IF(NTOT.GT.LBUS)GO TO 9000                                        M42.1887
      BUS(NTOT)=BUS1                                                            
      N1=NTOT                                                                   
  130 IF(N2.NE.0) GO TO 140                                                     
      NTOT=NTOT+1                                                       M42.1891
      LSTAT(19) = 130                                                   M42.1892
      IF(NTOT.GT.LBUS)GO TO 9000                                        M42.1893
      BUS(NTOT)=BUS2                                                    M42.1894
      N2=NTOT                                                           M42.1895
  140 IF( KSAT .GT. 0 )  GO TO 4040                                             
      IF ( ITYPE  .LT.  0 )   GO TO 64117                               M15. 498
      IF ( ITYPE  .GT.  50     .AND.                                    M16. 398
     1     ITYPE  .LE.  90 )   GO TO 64117                              M16. 399
      IF ( KOLBEG  .GT.  0 )   GO TO 132                                M20. 825
      IF ( MOLDAT  .GT.  0 )   GO TO 64117                              M28.2840
      DECODE(80, 64114, ABUFF )   ( TR(I), TX(I), C(I), I=IT, IT2 )     M42.1897
64114 FORMAT( 26X, 9E6.2 )                                              M42.1898
      GO TO 64117                                                       M20. 826
  132 KOLBEG = 27                                                       M20. 827
      NFRFLD = 3 * ( IT2 - IT + 1 )                                     M20. 828
      CALL FREFLD ( VOLTBC(1) )                                         M31. 471
      N9 = 1                                                            M20. 830
      DO 133  I=IT, IT2                                                 M20. 831
      TR(I) = VOLTBC(N9)                                                M31. 472
      TX(I) = VOLTBC(N9+1)                                              M31. 473
      C(I) = VOLTBC(N9+2)                                               M31. 474
  133 N9 = N9 + 3                                                       M20. 835
64117 IF ( KPH  .EQ.  0 )   GO TO 4115                                  M15. 500
      KILL  = 50                                                        M42.1899
      LSTAT(15) = KPH - 1                                               M42.1900
      LSTAT(19) = 4115                                                  M42.1901
      GO TO 9200                                                        M42.1902
 4115 IPRINT = 2                                                                
      ITRANS=0                                                          M42.1904
      IF(ITYPE.LT.51) GO TO 144                                         M42.1905
      IF(ITYPE.GT.90) GO TO 144                                         M42.1906
  147 ITRANS=ITYPE                                                      M42.1907
      ITYPE=ITYPE-50                                                    M42.1908
      N8 = IT2 - IT + 1                                                 M20. 836
      IF ( KOLBEG  .GT.  0 )   GO TO 4243                               M20. 837
      DECODE(80,145,ABUFF) (TR(I),TX(I),I=IT,IT2)                       M42.1909
  145 FORMAT(26X,3(E6.2,E12.2))                                         M42.1910
      GO TO 4246                                                        M20. 838
 4243 KOLBEG = 27                                                       M20. 839
      NFRFLD = 2 * N8                                                   M20. 840
      N9 = 1                                                            M20. 841
      CALL FREFLD ( VOLTBC(1) )                                         M31. 475
      DO 4245  I=IT, IT2                                                M20. 843
      TR(I) = VOLTBC(N9)                                                M31. 476
      TX(I) = VOLTBC(N9+1)                                              M31. 477
 4245 N9 = N9 + 2                                                       M20. 846
 4246 CALL MOVER0 ( C(IT), N8 )                                         M20. 847
  144 IF(ITYPE.GT.2 .AND. ITYPE.LE.50) GO TO 143                        M42.1911
      DECODE(80,142,ABUFF) IOUT                                         M42.1912
  142 FORMAT(79X,I1)                                                    M42.1913
      IF( IOUT .LE. 3 )  GO TO 54208                                    M29.1265
      IOUT = 3                                                          M42.1914
      IF ( NPOWER  .LT.  MAXPE )   GO TO 54201                          M20. 848
      IPRINT = 18                                                       M20. 849
      LSTAT(19) = 142                                                   M27. 656
      GO TO 9000                                                        M20. 851
54201 NPOWER = NPOWER + 1                                               M20. 852
      MPOWER = NPOWER + MAXPE                                           M35. 714
      KOUTVP(NPOWER) = NV + 1                                           M42.1915
      IF( ITYPE .LT. 90 )  GO TO 54203                                  M42.1916
      KOUTVP(MPOWER) = - ( INONL + 1 )                                  M35. 715
      GO TO 54205                                                       M42.1917
54203 KOUTVP(MPOWER) = IBR + 1                                          M35. 716
54205 IF( IPRSUP .GE. 2 )                                               M42.1918
     1WRITE(LUNIT6, 54207)  NPOWER, MAXPE, KOUTVP(NPOWER),              M22.2548
     2  KOUTVP(MPOWER)                                                  M35. 717
54207 FORMAT ( /, 21H POWER OUTPUT REQUEST, 4I10 )                      M42.1919
54208 CONTINUE                                                          M42.1920
      IF (IOUT.LT.2) GO TO 143                                          M29.1266
      NV=NV+1                                                           M42.1921
      IBRNCH(NV) = N1                                                   M42.1922
      JBRNCH(NV) = N2                                                   M42.1923
      IF( ITYPE .NE. 99 )  GO TO 138                                    M42.1924
      IF( N1 .LE. N2 )  GO TO 138                                       M42.1925
      IBRNCH(NV) = N2                                                   M42.1926
      JBRNCH(NV) = N1                                                   M42.1927
  138 IF( NV .LE. LSIZ12 )  GO TO 143                                   M38.1103
      IPRINT=11                                                         M42.1928
      LSTAT(19)=  143                                                   M42.1929
      GO TO 9000                                                        M42.1930
  143 IF (IOUT.EQ.2)                                                    M29.1267
     1             IOUT=0                                                       
      IF(ITYPE.LT.0) N1=-N1                                                     
      CALL IBRINC                                                       VAX.  46
      LSTAT(19)=  141                                                   M42.1933
      IF(IBR.GT.LBRNCH) GO TO 9000                                              
      IF ( ITYPE  .EQ.  92 )  ICHECK = 1                                M33. 158
      GO TO  (141,162,190,177,190,8520,8130,8178), ICHECK               M33. 159
  141 ICHECK=1                                                                  
      IF(ITYPE.GT.50) GO TO 186                                         M42.1936
      IF(IOUT.GT.0 .AND. ITYPE.LE.0) N2=-N2                             M42.1937
      IF ( BUS3  .NE.  BRANCH )   GO TO 6835                            M22.2550
      N16 = 1                                                                   
      TEXT15 = BUS4                                                             
      CALL NAMEA6 ( TEXT15, N16 )                                               
      IF ( N16 .EQ. -INTINF )  GO TO 6824                                       
      LSTAT(19) = 6829                                                  M22.2554
      KILL = 167                                                        M42.1942
      LSTAT(14) = N16                                                   M42.1943
      GO TO 9200                                                        M22.2557
 6824 N16 = 0                                                                   
      CALL NAMEA6 ( TEXTA5, N16 )                                               
      NAMEBR(IBR) = N16                                                         
      BUS3 = BLANK                                                      M22.2561
      BUS4 = BLANK                                                      M22.2562
      N3 = 1                                                            M22.2563
      N4 = 1                                                            M22.2564
 6835 IOLD = 0                                                                  
      IF(N3.EQ.1 .AND.                                                  M42.1947
     1             N4.EQ.1) GO TO 156                                           
      IOLD = 1                                                                  
      GO TO 155                                                         M42.1949
  150 LL = IABS( LENGTH(IOLD) )                                         M42.1950
      IF ( BUS3  .EQ.  COPY )   GO TO 6841                              M22.2566
      IF( IABS( KBUS(IOLD) )  .NE. N3 )  GO TO 28150                    M42.1951
      IF(  IABS( MBUS(IOLD) )   .EQ.   N4 )  GO TO 156                  M42.1952
      GO TO 28150                                                       M22.2567
 6841 N14 = NAMEBR(IOLD)                                                        
      IF ( BUS4 .EQ. TEXVEC(N14) ) GO TO 156                                    
28150 IOLD = IOLD + LL                                                  M42.1955
  155 IF( IOLD .LT. IBR )  GO TO 150                                    M42.1956
      LSTAT(19) = 150                                                   M42.1957
      KILL = 6                                                          M28.2842
      GO TO 9200                                                        M28.2843
  156 KBUS(IBR)=N1                                                      M42.1958
      MBUS(IBR)=N2                                                      M42.1959
      IF(IOLD.NE.0) GO TO 175                                                   
      LENGTH(IBR)=1                                                             
      IF(ITYPE.EQ.0) GO TO 170                                                  
      IBR1=IBR                                                                  
      KCOUNT=1                                                                  
      IF(ITYPE.EQ.1) GO TO 160                                                  
      IF(ITYPE.EQ.(-1)) GO TO 190                                               
      LSTAT(19) = 160                                                   M42.1967
 4218 KILL = 3                                                          M42.1968
      LSTAT(16) = ITYPE                                                 M42.1969
      GO TO 9200                                                        M42.1970
C     PI-EQUIVALENT ****************************************************M42.1971
  160 NR(IBR)=IT                                                                
      IF( KREQAB .GT. 0 )   KODEBR(IBR) = 1                             M42.1973
      IF ( MOLDAT  .EQ.  0 )   GO TO 171                                M28.2844
      IF ( KOLBEG  .GT.  0 )   GO TO 171                                M28.2845
      DECODE (80, 8331, ABUFF(1))  TR(IT), TX(IT), C(IT)                M28.2846
  171 IF ( NOUTPR  .NE.  0 )   GO TO 5411                               M28.2847
      IF( ITRANS .GT. 0 )  GO TO 54108                                  M28.2848
      WRITE (KUNIT6, 54777)  TR(IT), TX(IT), C(IT)                      M35. 718
54777 FORMAT ( 15H+1ST OF PI-CKT.  , 1X, 3E11.3  )                      M28.2850
      GO TO 5411                                                        M28.2851
54108 WRITE (KUNIT6, 54109)  TR(IT), TX(IT)                             M35. 719
54109 FORMAT ( 20H+1ST OF COUPLED R-L.  , 5X, 2E11.3  )                 M28.2853
 5411 ICHECK=2                                                          M28.2854
      IF( TR(IT) .EQ. -6666. .AND. TX(IT) .EQ. -6666. ) ISGFD = 100     M32.1103
      GO TO 8383                                                        M28.2855
  162 KCOUNT=KCOUNT+1                                                   M42.1974
      IF(ITYPE.NE.KCOUNT) GO TO 141                                     M42.1975
      LENGTH(IBR1)=KCOUNT                                               M42.1976
      IF ( KCOUNT  .GT.  MXPHAS )                                       M31. 478
     1 MXPHAS = KCOUNT                                                  M31. 479
      IT2=IT+ITYPE-1                                                    M42.1977
      IF ( MOLDAT  .EQ.  0 )   GO TO 8352                               M28.2856
      IF ( KOLBEG  .GT.  0 )   GO TO 8352                               M28.2857
      DO 8346  IT32=IT, IT2                                             M28.2858
C     READ INPUT CARD USING CIMAGE.                                     M28.2859
      IF ( IT32  .GT.  IT )                                             M28.2860
     1 CALL CIMAGE                                                      M28.2861
      DECODE (80, 8331, ABUFF(1))  TR(IT32), TX(IT32), C(IT32)          M28.2862
 8331 FORMAT ( 26X,  3E16.0 )                                           M28.2863
      IF ( NOUTPR  .NE.  0 )   GO TO 8346                               M28.2864
      IF ( IT32  .GT.  IT )   GO TO 8338                                M28.2865
      WRITE (KUNIT6, 8335) ITYPE, TR(IT32), TX(IT32), C(IT32)           M35. 720
 8335 FORMAT ( 6H+PHASE,  I3,  3E13.4 )                                 M28.2867
      GO TO 8346                                                        M28.2868
 8338 WRITE (KUNIT6, 8341)        TR(IT32), TX(IT32), C(IT32)           M35. 721
 8341 FORMAT ( 1H+,  3X,  5HCONT.,  3E13.4 )                            M28.2870
 8346 CONTINUE                                                          M28.2871
      GO TO 163                                                         M28.2872
 8352 IF( ITYPE .NE. 3 )  GO TO 47634                                   M28.2873
      D3 = ABSZ( TR(IT) )   +   ABSZ( TX(IT) )   +   ABSZ( TR(IT+1) )  +M18. 292
     1 ABSZ( TX(IT+1) )   +   ABSZ( TR(IT+2) )   +   ABSZ( TX(IT+2) )   M18. 293
      IF( D3 .GT. 0.0 )  GO TO 47634                                    M42.1978
      D3 = TR(IT-3)                                                     M42.1979
      D4 = TR(IT-2)                                                     M42.1980
      D5 = ( D3 + 2.0*D4 ) / 3.                                         M42.1981
      D6 = ( D3 - D4 ) / 3.                                             M42.1982
      TR(IT-3) = D5                                                     M42.1983
      TR(IT-1) = D5                                                     M42.1984
      TR(IT+2) = D5                                                     M42.1985
      TR(IT-2) = D6                                                     M42.1986
      TR(IT) = D6                                                       M42.1987
      TR(IT+1) = D6                                                     M42.1988
      D3 = TX(IT-3)                                                     M42.1989
      D4 = TX(IT-2)                                                     M42.1990
      D5 = ( D3 + 2.0*D4 ) / 3.                                         M42.1991
      D6 = ( D3 - D4 ) / 3.                                             M42.1992
      TX(IT-3) = D5                                                     M42.1993
      TX(IT-1) = D5                                                     M42.1994
      TX(IT+2) = D5                                                     M42.1995
      TX(IT-2) = D6                                                     M42.1996
      TX(IT) = D6                                                       M42.1997
      TX(IT+1) = D6                                                     M42.1998
47634 IF( ITRANS .GT. 0 )  GO TO 21693                                  M42.1999
      IT3 = IT+1                                                        M42.2000
      IF ( NOUTPR  .EQ.  0 )                                            M20. 853
     1 WRITE (KUNIT6, 54104)  TR(IT), TX(IT), C(IT), TR(IT3), TX(IT3)   M35. 722
54104 FORMAT ( 1H+, 5E10.3 )                                            M42.2001
      GO TO 21694                                                       M42.2002
21693 IT3 = ITYPE  +  (IT-1)                                            M42.2003
      N3 = IT + 2                                                       M42.2004
      IF( IT3 .LT. N3 )  N3 = IT3                                       M42.2005
      IF ( NOUTPR  .EQ.  0 )                                            M20. 855
     1 WRITE (KUNIT6, 54104)  TR(IT), TX(IT), TR(IT+1), TX(IT+1), TR(N3)M35. 723
21694 CONTINUE                                                          M42.2006
      IF(ITYPE.LE.3) GO TO 163                                          M42.2007
      IT3 = IT +3                                                       M42.2008
  165 IT32 = IT3 + 2                                                    M42.2009
      IF( IT2 .LT. IT32 )  IT32 = IT2                                   M42.2010
C     READ INPUT CARD USING CIMAGE.                                     M42.2011
      CALL CIMAGE                                                       M42.2012
      IF(ITRANS.EQ.0) GO TO 168                                         M42.2013
      N8 = IT32 - IT3 + 1                                               M20. 857
      IF ( KOLBEG  .GT.  0 )   GO TO 4254                               M20. 858
      DECODE(80,166,ABUFF) (TR(I),TX(I),I=IT3,IT32)                     M42.2014
  166 FORMAT(26X,3(E6.2,E12.2))                                         M42.2015
      GO TO 4256                                                        M20. 859
 4254 KOLBEG = 27                                                       M20. 860
      NFRFLD = 2 * N8                                                   M20. 861
      N9 = 1                                                            M20. 862
      CALL FREFLD ( VOLTBC(1) )                                         M31. 480
      DO 4255  I=IT3, IT32                                              M20. 864
      TR(I) = VOLTBC(N9)                                                M31. 481
      TX(I) = VOLTBC(N9+1)                                              M31. 482
 4255 N9 = N9 + 2                                                       M20. 867
 4256 CALL MOVER0 ( C(IT3), N8 )                                        M20. 868
      IF ( NOUTPR  .EQ.  0 )                                            M20. 869
     1 WRITE (KUNIT6, 54104)  TR(IT3), TX(IT3), TR(IT3+1),              M35. 724
     1                       TX(IT3+1), TR(IT32)                        M20. 871
      GO TO 169                                                         M42.2016
  168 IF ( KOLBEG  .GT.  0 )   GO TO 4262                               M20. 872
      DECODE (80, 2, ABUFF)  ( TR(I), TX(I), C(I), I=IT3, IT32 )        M42.2017
      GO TO 4264                                                        M20. 873
 4262 NFRFLD = 3 * ( IT32 - IT3 + 1 )                                   M20. 874
      KOLBEG = 27                                                       M20. 875
      N9 = 1                                                            M20. 876
      CALL FREFLD ( VOLTBC(1) )                                         M31. 483
      DO 4263  I=IT3, IT32                                              M20. 878
      TR(I) = VOLTBC(N9)                                                M31. 484
      TX(I) = VOLTBC(N9+1)                                              M31. 485
      C(I) = VOLTBC(N9+2)                                               M31. 486
 4263 N9 = N9 + 3                                                       M20. 882
 4264 I = IT3                                                           M20. 883
      IF( IT32 .GT. I )  I = I + 1                                      M42.2018
      N3=I                                                              M42.2019
      IF ( NOUTPR  .EQ.  0 )                                            M20. 884
     1 WRITE (KUNIT6, 54104)  TR(IT3), TX(IT3), C(IT3), TR(N3), TX(N3)  M35. 725
  169 IF(IT32.EQ.IT2) GO TO 163                                         M42.2020
      IT3 = IT3 +3                                                      M42.2021
      GO TO 165                                                         M42.2022
    2 FORMAT(26X,9E6.2)                                                 M42.2023
  163 KBUS(IBR) = N1                                                    M42.2024
      MBUS(IBR)=N2                                                      M42.2025
      NR(IBR)=IT                                                        M42.2026
      IF( KREQAB .GT. 0 )   KODEBR(IBR) = 1                             M42.2027
      IT=IT+ITYPE                                                       M42.2028
      IF( ICAS .EQ. -1 .AND. ITYPE .EQ. 2*NPHCAS) GO TO 148             M42.2029
      IF(ICAS.LE.0)GOTO100                                              M42.2030
      IF(ITYPE.LT.NPHCAS)GOTO100                                        M42.2031
      CALL OVER3                                                                
      GO TO 1000                                                                
  148 IEND = IBR1 + NPHCAS - 1                                          M42.2032
      DO 149 I = IBR1,IEND                                              M42.2033
  149 KODEBR(I) = -2                                                    M42.2034
      N3 = IBR1 + NPHCAS                                                M42.2035
      N4 = IBR1 + IBR                                                   M42.2036
      DO 115 I = N3,N4                                                  M42.2037
      KBUS(I) = 0                                                       M42.2038
  115 MBUS(I) = 0                                                       M42.2039
      ICAS = 0                                                          M42.2040
      IBR = IBR - NPHCAS                                                M42.2041
      LENGTH(IBR1) = NPHCAS                                             M42.2042
      IF ( NPHCAS  .GT.  MXPHAS )                                       M31. 487
     1 MXPHAS = NPHCAS                                                  M31. 488
      GO TO 100                                                         M42.2043
C     SERIES R-L-C  ****************************************************M42.2044
  170 NR(IBR)=-IT                                                       M42.2045
      ICHECK=1                                                          M42.2046
      IF ( MOLDAT  .EQ.  0 )   GO TO 8371                               M28.2874
      IF ( KOLBEG  .GT.  0 )   GO TO 8371                               M28.2875
      DECODE (80, 8331, ABUFF(1))  TR(IT), TX(IT), C(IT)                M28.2876
 8371 IF ( NOUTPR  .EQ.  0 )                                            M28.2877
     1 WRITE (KUNIT6, 54107)  TR(IT), TX(IT), C(IT)                     M35. 726
54107 FORMAT ( 14H+SERIES R-L-C.  , 2X, 3E11.3  )                       M42.2047
 8383 D1 = ABSZ(TR(IT))  +  ABSZ(TX(IT))  +  ABSZ(C(IT))                M28.2878
      IF( D1 .NE. 0.0 )  GO TO 4220                                     M42.2048
      KILL = 4                                                          M42.2049
      LSTAT(19) = 5411                                                  M17. 245
      GO TO 9200                                                        M42.2050
 4220 IT = IT + 1                                                       M42.2051
      GO TO 100                                                         M42.2052
C     DATA ALREADY AVAILABLE IN REFERENCE BRANCH ***********************M42.2053
  175 IF ( IMODEL(IOLD) .EQ. -3                                         M44. 122
     1  .OR. IMODEL(IOLD) .EQ. -4 )  GO TO 8100                         M44. 123
      IF (IMODEL(IOLD) .EQ. -2 ) GO TO 8100                             M44.   2
      IF (KODSEM(IOLD) .EQ. 0) GO TO 8600                               M23. 463
      IPUNCH = IABS(KODEBR(IOLD))                                       M23. 465
      IPS1 = 1                                                          M23. 466
      ICHECK = 6                                                        M23. 467
      IF (NOUTPR .EQ. 0)                                                M23. 468
     1     WRITE (KUNIT6, 21696) BUS3, BUS4                             M35. 727
      GO TO 8560                                                        M23. 470
 8520 N6 = 5                                                            M23. 471
      KBUS(IBR) = N1                                                    M23. 472
      MBUS(IBR) = N2                                                    M23. 473
      IF (NOUTPR .EQ. 0)                                                M23. 474
     1     WRITE (KUNIT6, 54111)                                        M35. 728
 8530 IOLD = IOLD + 1                                                   M23. 476
      IPS1 = IPS1 + 1                                                   M23. 477
 8540 IF (ITYPE .EQ. -1) GO TO 8560                                     M23. 478
 8550 KILL = 169                                                        M23. 479
      LSTAT(19) = 8550                                                  M23. 480
      GO TO 9200                                                        M23. 481
 8560 KODEBR(IBR) = -IPUNCH                                             M23. 482
      KODSEM(IBR) = KODSEM(IOLD)                                        M23. 483
      LENGTH(IBR) = LENGTH(IOLD)                                        M23. 484
      CI(IBR) = CI(IOLD)                                                M23. 485
      CK(IBR) = CK(IOLD)                                                M23. 486
      CIK(IBR) = CIK(IOLD)                                              M23. 487
      CKI(IBR) = CKI(IOLD)                                              M23. 488
      N3 = NR(IOLD)                                                     M23. 489
      NR(IBR) = N3                                                      M23. 490
      N5 = IFX + 1                                                      M23. 491
      IFX = IFX + N3                                                    M23. 492
      IF (IFX .LE. LHIST) GO TO 8570                                    M23. 493
      IPRINT = 22                                                       M23. 494
      LSTAT(19) = 8570                                                  M23. 495
      GO TO 9000                                                        M23. 496
 8570 INDHST(IBR) = N5                                                  M23. 497
      N3 = INDHST(IOLD)                                                 M23. 498
      IF (CI(IOLD) .GT. 0.0) N6 = N6 + 1                                M23. 499
      IF ( KODSEM(IOLD)  .GT.  0 )   N6 = 6                             M29.1268
      IF (N6 .GT. 0) CALL MOVER(CNVHST(N3), CNVHST(N5), N6)             M23. 500
      IF (IPS1 .LT. IPUNCH) GO TO 100                                   M23. 501
      IF (CKI(IOLD) .GT. 0.0) GO TO 8580                                M23. 502
      ICHECK = 1                                                        M23. 503
      GO TO 100                                                         M23. 504
 8580 CALL IBRINC                                                       VAX.  47
      IF (IBR .LE. LBRNCH) GO TO 8590                                   M23. 506
      IPRINT = 2                                                        M23. 507
      LSTAT(19) = 8590                                                  M23. 508
      GO TO 9000                                                        M23. 509
 8590 N6 = 0                                                            M23. 510
      KBUS(IBR) = -1                                                    M23. 511
      MBUS(IBR) =  1                                                    M23. 512
      GO TO 8540                                                        M23. 513
 8100 IF ( IMODEL(IOLD) .EQ. -2 )  GO TO 8987                           M44. 124
      IF (ITYPE .LT. 0 )                                                        
     1 WRITE (KUNIT6, 8710) ITYPE, BUS1, BUS2, BUS3, BUS4               M44. 127
 8710 FORMAT ( 25H+Reference cable branch: , I2, 4A6 )                  M44. 128
      CALL INTERP                                                               
      ICHECK = 8                                                                
      MODEL = LENGTH(IOLD)                                                      
      IBR1 = IBR                                                                
      LENGTH(IBR1) = LENGTH(IOLD)                                               
 8187 LITYPE(IBR) = LITYPE(IOLD)                                                
c     IMODL(IBR) = IMODL(IOLD)                                                  
      IMODEL(IBR) = IMODEL(IOLD)                                                
C      MATCON(IBR) = MATCON(IOLD)                                       M44. 137
c      IFBRC(IBR) = IBR                                                         
c      ICRBF(IBR) = IBR                                                         
      IFX = IFX + 1                                                             
      IF ( IFX .GT. LHIST ) STOP                                        M44. 143
      IFSEM = IFSEM + 1                                                         
      IF ( IFSEM .GT. LFSEM ) STOP                                      M44. 145
      CKI(IBR) = CKI(IOLD)                                                      
      ITEMQ    = KODSEM(IOLD)                                                   
      SCONST(IFSEM) = SCONST(ITEMQ)                                             
      KODSEM(IBR)   = IFSEM                                                     
      INDHST(IBR)   = IFX                                                       
      CKKJM(IBR)    = CKKJM(IOLD)                                               
      IIHST         = INDHST(IOLD)                                              
      CNVHST(IFX)   = CNVHST(IIHST)                                             
      INOFF5 = 5 * LBRNCH                                                       
      INFDLI(INOFF5+IBR)                                                M44. 158
     1       = CNVHST(IFX) / DELTAT + 1                                         
      ITEMQ = ITEMQ + 7 * CKI(IOLD)                                             
     1  + 11 * CKKJM(IOLD) + 9 + NN17                                           
      IFSEM = IFSEM + 7 * CKI(IBR)                                              
     1  + 11 * CKKJM(IBR)  + 9 +NN17                                            
      IF ( IFSEM .GT. LFSEM ) STOP                                      M44. 165
      NFIR = KODSEM(IOLD) + 1                                                   
      NSEC = KODSEM(IBR)  + 1                                                   
      CALL MOVER ( SCONST(NFIR),                                                
     1     SCONST(NSEC),ITEMQ-1)                                                
c      IFX = IFX + 14                                                           
      IFX = IFX + 19 + 1 + NYCMP                                        M44.  18
      IF( MODEL .NE. -ITYPE )  GO TO 100                                        
      IF ( -ITYPE .EQ. MODEL )                                                  
     1                          GO TO 50087                             M44. 175
50087 IFQ = IFSEM                                                               
C                                                   !the refernce branchM44. 177
      INOFF1 = LBRNCH                                                           
      INOFF2 = 2 * LBRNCH                                                       
      INOFF3 = 3 * LBRNCH                                                       
      INOFF4 = 4 * LBRNCH                                                       
      ICHECK = 1                                                                
      NPHS = MODEL                                                      M44. 184
      NPHS2 = MODEL ** 2                                                M44. 185
      NPHSU = ( MODEL + 1 ) * MODEL / 2                                 M44. 186
      IBR2 = IBR - NPHS + 1                                                     
      IBR3 = IBR - NPHS + 3                                                     
      IFQ = IFQ + 1                                                             
      INFDLI(INOFF2 + IBR2) =                                           M44. 191
     1       IFQ                                                                
      INFDLI( INOFF1 + IBR2 ) =                                         M44. 193
     1               IDM                                                        
      INFDLI( INOFF4 + IBR2 ) = IDU                                             
C        IADRSM(IOLD) contains the index for first cell of Qii(1,1), forM44. 196
C        double six phases circuits it should be 1*6+7(16+9+10+11+12+12)M44. 197
C+7(21+16+17+10+10+17)+6*8=1181 which been added 1 gives 1st cell of QiiM44. 198
      KOFF1 =900                                                                
      KOFF2 = KOFF1 + ISECTI                                                    
      KOFF3  = KOFF2  + ISECTI                                                  
      KOFF4  = KOFF3  + ISECTI                                                  
      KOFF5  = KOFF4  + ISECTI                                                  
      KOFF6  = KOFF5  + ISECTI                                                  
      KOFF7  = KOFF6  + ISECTI                                                  
      KOFF8  = KOFF7  + ISECTI                                                  
      KOFF9  = KOFF8  + ISECTI                                                  
      KOFF10 = KOFF9  + ISECTI                                                  
      KOFF13 = KOFF10 + ISECTI                                                  
      KOFF14 = KOFF13 + ISECTI                                                  
      KOFF15 = KOFF14 + ISECTI                                                  
      KOFF16 = KOFF15 + ISECTI                                                  
      KOFF17 = KOFF16 + ISECTI                                                  
      KOFF18 = KOFF17 + ISECTI                                                  
      KOFF19 = KOFF18 + ISECTI                                                  
      KOFF20 = KOFF19                                                           
      IF ( KGROUP .NE. 1 ) WK1(KOFF19) =1                               M44. 217
      WK1(KOFF19) = WK1(KOFF19) + KGROUP                                M44. 218
      NQ = 0                                                            M44. 220
      INTERM = IOLD - MODEL + 1                                                 
      IBRTER = IBR - MODEL + 1                                                  
      NPHS2 = LENGTH( INTERM ) ** 2                                     M44. 223
C     KQ = IADRSU( INTERM ) !index of Q - 'M'atrix for referenced branchM44. 224
      KQ = INFDLI(INOFF1+INTERM)                                        M44. 225
C     NKQ = IADRSU( IBRTER ) !index of Q - 'M'atrix for reference branchM44. 226
      NKQ = INFDLI(INOFF1+IBRTER)                                       M44. 227
      DO 8729 I = 1, NPHS2                                                      
      WK1(KOFF20+NKQ)=WK1(KOFF20+KQ)                                            
C     NTERMQ(NKQ) = NTERMQ(KQ)  !load # terms form refd brch to ref brchM44. 231
C     QK0(NKQ)    = QK0(KQ)     !Load feedforward coefficient Qk0 of QiiM44. 232
      NKQ = NKQ + 1                                                             
      KQ = KQ + 1                                                       M44. 234
8729  CONTINUE                                                          M44. 235
      IOLD = IOLD - MODEL + 1                                                   
      ISEC = IOLD + 1                                                   M44. 238
      KQ =  INFDLI(INOFF2+IOLD)                                                 
      KQ1 = INFDLI(INOFF3+IOLD)                                                 
      INEW = IBR - MODEL + 1                                            M44. 241
      NKQ  = INFDLI(INOFF2+INEW)                                                
      KQ1  = KQ1 - KQ + 1                                                       
      CALL MOVER ( SCONST(KQ),                                                  
     1         SCONST(NKQ),KQ1 )                                                
      NKQ = NKQ + KQ1 - 1                                                       
      IFQ = NKQ                                                                 
     1    + ( NKQ - INFDLI(INOFF2+INEW)                                 M44. 248
     2    + 1 - NPHS2 ) * 3 / 2                                                 
     3    + 2 * NPHS2                                                           
      IFSEM = IFQ                                                               
      INFDLI(INOFF3+IBR2)=NKQ                                                   
      IDM = IDM + NPHS2                                                         
      IDU = IDU + NPHSU                                                         
      IF( MODEL .NE. -ITYPE ) GO TO 100                                 M44. 259
      ICHECK = 1                                                                
      KGROUP = 1                                                        M44. 261
      GO TO 100                                                                 
 8178 IOLD = IOLD + 1                                                           
      KBUS(IBR) = N1                                                    M44. 264
      MBUS(IBR) = N2                                                    M44. 265
      GO TO  8187                                                       M44. 266
 8987 ICHECK = 7                                                        M33. 161
      MODEL = LENGTH(IOLD)                                                      
      IBR1 = IBR                                                        M33. 163
      LENGTH(IBR1) = 0                                                  M33. 164
      LITYPE(IBR) = LITYPE(IOLD)                                        M33. 165
 8110 IF (ITYPE .LT. 0  .AND.                                                   
     1    NOUTPR .EQ. 0 )                                               M44.  19
     1 WRITE (KUNIT6, 8810) ITYPE, BUS1, BUS2, BUS3, BUS4               M44.   4
 8810 FORMAT ( 26H+Reference Jose's branch: , I2, 4A6 )                 M44.   5
      CALL INTERP                                                               
      CKI(IBR) = CKI(IOLD)                                                      
      CKKJM(IBR) = CKKJM(IOLD)                                                  
      IMODEL(IBR) = IMODEL(IOLD)                                        M33. 168
      N5 = IFSEM + 1                                                            
      N6 = 7 * CKI(IBR) + 7 * CKKJM(IBR) + 5                            M33. 170
      IFSEM = IFSEM + N6                                                        
      IF ( IFSEM  .GT. LFSEM )  GO TO  8170                             M33. 172
      KODSEM(IBR) = N5                                                  M33. 173
      N3 = KODSEM(IOLD)                                                         
      CALL MOVER (SCONST(N3),SCONST(N5),N6)                                     
      N5 = IFX + 1                                                              
      IFX = IFX + 15                                                            
      IF ( IFX  .GT.  LHIST )  GO TO 8150                               M33. 178
      INDHST(IBR) = N5                                                  M33. 179
      N3 = INDHST(IOLD)                                                 M33. 180
      CNVHST(N5) = CNVHST(N3)                                                   
      LENGTH(IBR1) = LENGTH(IBR1) + 1                                           
      IF ( MODEL  .GT.  LENGTH(IBR1) )  GO TO  100                      M33. 183
      ICHECK = 1                                                        M33. 184
      ITRANM = MODEL                                                    M33. 185
      MODEL = 0                                                         M33. 186
      GO TO 100                                                         M33. 187
 8130 IOLD = IOLD + 1                                                   M33. 188
      KBUS(IBR) = N1                                                    M33. 189
      MBUS(IBR) = N2                                                    M33. 190
      GO TO  8110                                                       M33. 191
 8150 IPRINT = 22                                                       M33. 192
      LSTAT(19) = 8150                                                  M33. 193
      GO TO 9000                                                        M33. 194
 8170 IPRINT = 20                                                       M33. 195
      LSTAT(19) = 8170                                                  M33. 196
      GO TO 9000                                                        M33. 197
 8600 NR(IBR) = NR(IOLD)                                                M23. 514
      NAMEBR(IBR) = NAMEBR(IOLD)                                        M42.2054
      LENGTH(IBR)=-IABS(LENGTH(IOLD))                                   M42.2055
      LITYPE(IBR) = LITYPE(IOLD)                                        M33. 198
      KODEBR(IBR)=KODEBR(IOLD)                                          M42.2056
      CIK(IBR)=CIK(IOLD)                                                M42.2057
      CI(IBR)=CI(IOLD)                                                  M42.2058
      CK(IBR)=CK(IOLD)                                                  M42.2059
      CKI(IBR) = CKI(IOLD)                                              M14.1080
      IBR1 = IBR                                                        M29.1269
      IF (NOUTPR .EQ. 0)                                                M23. 515
     1 WRITE (KUNIT6, 21696)  BUS3, BUS4                                M35. 729
21696 FORMAT ( 28H+REFERENCE BRANCH.   COPY  ', A6, 6H' TO ',           M42.2060
     1 A6, 1H' )                                                        M42.2061
      IF(LL.EQ.1) GO TO 178                                             M42.2062
      MBUS(IBR)=IABS(N2)                                                M42.2063
      ICHECK=4                                                          M33. 199
      II=2                                                              M42.2064
C     READ INPUT CARD USING CIMAGE.                                     M42.2065
  176 CALL CIMAGE                                                       M42.2066
      IF ( KOLBEG  .GT.  0 )   GO TO 6634                               M22.2570
      DECODE(80, 1, ABUFF)  ITYPE, BUS1, BUS2                           M42.2067
      GO TO 6637                                                        M22.2571
 6634 NFRFLD = 1                                                        M22.2572
      CALL FREFLD ( VOLTBC(1) )                                         M31. 489
      ITYPE = VOLTBC(1)                                                 M31. 490
      NFRFLD = 2                                                        M22.2575
      NRIGHT = -1                                                       M22.2576
      CALL FREONE ( D1 )                                                M29.1270
      BUS1 = TEXTA6(1)                                                  M22.2578
      BUS2 = TEXTA6(2)                                                  M22.2579
      NRIGHT = 0                                                        M22.2580
 6637 IF ( ITYPE  .GT.  50     .AND.                                    M22.2581
     1     ITYPE  .LT.  91 )   ITYPE = ITYPE - 50                       M22.2582
      IF( IABS(ITYPE)  .EQ.  II )  GO TO 110                            M42.2068
      LSTAT(19) = 176                                                   M42.2069
      GO TO 4218                                                        M42.2070
  177 IF ( NOUTPR  .EQ.  0 )                                            M20. 895
     1 WRITE (KUNIT6, 54111)                                            M35. 730
54111 FORMAT(  31H+REFERENCE BRANCH.   COPY CONT.  )                    M42.2071
      IOLD=IOLD+1                                                       M42.2072
      KBUS(IBR)=N1                                                      M42.2073
      MBUS(IBR)=N2                                                      M42.2074
      NR(IBR)=NR(IOLD)                                                  M42.2075
      KODEBR(IBR)=KODEBR(IOLD)                                          M42.2076
      CKI(IBR) = CKI(IOLD)                                              M14.1082
      CIK(IBR)=CIK(IOLD)                                                M42.2077
      CI(IBR)=CI(IOLD)                                                  M42.2078
      CK(IBR)=CK(IOLD)                                                  M42.2079
      II=II+1                                                           M42.2080
      IF(II.LE.LL) GO TO 176                                            M42.2081
  178 ICHECK=1                                                          M42.2082
      ITRANM = 1                                                        M33. 200
      IF(ICAS.NE.1)GOTO100                                              M42.2083
      ICAS=0                                                            M42.2084
      CALL CIMAGE                                                       M42.2085
      DECODE(80,76621,ABUFF) BUS1,BUS2                                  M42.2086
76621 FORMAT(2X,2A6)                                                    M42.2087
      IF ( BUS1  .NE.  TEXT1 )   GO TO 7662                             M18. 297
      IF( BUS2 .EQ. TEXT2 )  GO TO 76617                                M42.2088
 7662 LSTAT(19) = 7662                                                  M18. 298
      KILL=58                                                           M42.2089
      GOTO 9200                                                         M42.2090
76617 IF ( NOUTPR  .EQ.  0 )                                            M20. 897
     1 WRITE (KUNIT6, 76620)                                            M35. 731
76620 FORMAT(28H+TERMINATION OF CASCADED PI.)                           M42.2091
      GO TO 100                                                         M42.2092
C     PROCESS NONLINEAR ELEMENT INSIDE SUBROUTINE  "NONLN2" .           M28.2879
  186 CALL NONLN2                                                       M28.2880
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28.2881
      GO TO 100                                                         M28.2882
C     PROCESS DISTRIBUTED LINE INSIDE SUBROUTINE  "DISTR2" .            M28.2883
  190 CALL DISTR2                                                       M39. 992
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28.2885
      IF ( NCHAIN  .NE.  4 )   GO TO 197                                M39. 993
      CALL OVER4                                                                
      GO TO 1000                                                                
  197 I = LSTAT(18)                                                     M39. 996
      IF ( I  .EQ.  100 )   GO TO 100                                   M28.2888
      IF ( I  .EQ.  141 )   GO TO 141                                   M28.2889
      CALL STOPTP                                                               
 200  IT = IT - 1                                                       M31. 491
      IF ( MXPHAS*MXPHAS  .LT.  LSIZ26 )   GO TO 1641                   M31. 492
      KILL = 1                                                          M32.1124
      IPRINT = 26                                                       M31. 493
      LSTAT(19) = 200                                                   M31. 494
      LSTAT(13) = MXPHAS                                                M31. 495
      GO TO 9000                                                        M32.1125
 1641 LASTOV = NCHAIN                                                   M31. 497
      NCHAIN = 5                                                        M42.2093
      KTRLSW(3) = 0                                                     M33. 201
      DO  1642   I = 1, IBR                                             M32.1126
      IF (LITYPE(I) .GE. 0 )  GO TO 1642                                M32.1127
      LITYPE(I) = - LITYPE(I)                                           M32.1128
 1642 CONTINUE                                                          M32.1129
 8675 IBRNAM = IBRNAM + 1                                                       
      IF ( IBRNAM .GT. IBR )  GO TO 8680                                        
      NAMEBR(IBRNAM) = 1                                                        
      IF ( NMAUTO .EQ. 0 ) GO TO 8675                                           
      CALL NMINCR ( TEXT16, IBRNAM )                                            
      N24 = 0                                                                   
      CALL NAMEA6 ( TEXT16, N24 )                                               
      NAMEBR(IBRNAM) = N24                                                      
      GO TO 8675                                                                
 8680 INONAM = INONAM + 1                                                       
      IF ( INONAM .GT. INONL ) GO TO 8681                                       
      NAMEBR(INONAM) = 1                                                        
      IF ( NMAUTO .EQ. 0 ) GO TO 8680                                           
      CALL NMINCR ( TEXT17, INONAM )                                            
      N24 = 0                                                                   
      CALL NAMEA6 ( TEXT17, N24 )                                               
      NAMENL(INONAM) = N24                                                      
      GO TO 8680                                                                
 8681 IF ( IPRSUP  .GE.  1 )                                            M42.2112
     1 WRITE (LUNIT6, 1644)  NTOT, IBR, IT, INONL, MXPHAS               M31. 499
 1644 FORMAT (  19H BOTTOM OF "OVER2".,                                 M31. 500
     1          40H    NTOT     IBR      IT   INONL  MXPHAS             M31. 501
     2  ,/,  19X,  10I8  )                                              M31. 502
      IF ( IPRSUP  .LE.  2 )   GO TO 9900                               M32.1130
      WRITE (LUNIT6, 208)   ( K, KBUS(K), MBUS(K), NR(K), KODEBR(K),    M17. 284
     1         LENGTH(K), KODSEM(K), INDHST(K), LITYPE(K),              M31. 503
     2         IMODEL(K), NAMEBR(K),  K=1, IBR )                        M42.2113
  208 FORMAT ( /,  47H BRANCH-TABLE INTEGER VECTORS AT END  'OVER2' . , M16. 538
     1 /,   88H     ROW    KBUS    MBUS      NR  KODEBR  LENGTH  KODSEM M31. 505
     2 INDHST  LITYPE  IMODEL  NAMEBR  ,/, ( 11I8 )  )                  M42.2114
      WRITE (LUNIT6, 209)   ( K, BUS(K), CI(K), CK(K), CIK(K), CKI(K),  M17. 285
     1                   CKKJM(K), K=1, IBR )                           M31. 507
  209 FORMAT ( /, 5X, 3HROW, 7X, 3HBUS, 13X, 2HCI, 13X, 2HCK,           M16. 543
     1 12X, 3HCIK, 12X, 3HCKI,10X,5HCKKJM,/,( I8, 4X, A6, 5E15.6 ) )    M31. 508
      WRITE (LUNIT6,2584) (I, TR(I), TX(I), R(I), C(I), I=1, IT)        M32.1131
 2584 FORMAT ( /, 40H ROWS 1 THROUGH IT OF PARAMETERS FOLLOW:           M32.1132
     1  ,/,    7X,  3HROW,  13X,  2HTR,  13X,  2HTX,  14X,              M32.1133
     2 1HR,  14X,  1HC  ,/,  ( I10,  4E15.5  ) )                        M32.1134
      GO TO 9900                                                        M32.1135
 9000 LSTAT(16) = IPRINT                                                M42.2115
      KILL = 1                                                          M42.2116
 9200 NCHAIN = 51                                                       M42.2117
      LSTAT(18) = 2                                                     M42.2118
 9900 IF ( IPRSUP  .GE.  1 )                                            M32.1136
     1 WRITE ( LUNIT6, 9906)  IBR, INONL, NTOT, KILL                    M32.1137
 9906 FORMAT ( 21H EXIT MODULE "OVER2".,                                M32.1138
     1         32H     IBR   INONL    NTOT    KILL                      M32.1139
     2    ,/,  21X,  10I8  )                                            M32.1140
99999 RETURN                                                            M42.2119
      END                                                               M42.2120
      SUBROUTINE FDDATA( IKF, ISFD, IBF )                               M32.1141
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     OVERLAY 2 MODULE USED ONLY FOR FREQUENCY-DEPENDENT                M32.1142
C     REPRESENTATION OF GENERATOR EQUIVALENTS.                          M32.1143
      INCLUDE  'LABL02.FOR;/NOLIST'                                           
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
C     THIS ROUTINE READS-IN THE BRANCH DATA FOR THE MODES  * * * * * * *M32.1147
C     I N I T I A L I Z E   C O U N T E R S    *   *   *   *   *   *   *M32.1148
      IDK = 2 * IKF                                                     M32.1149
      IKK = ISFD                                                        M32.1150
      IKF = IKF + 1                                                     M32.1151
      IF( IPRSUP .GT. 0 )                                               M32.1152
     1   WRITE ( LUNIT6, 1 ) IKF, IDK, IKK                              M32.1153
    1 FORMAT(33H AT START OF FDDATA IKF,IDK,IKK =,3I10)                 M32.1154
C     PROCES MODAL BRANCH DATA         *   *   *   *   *   *   *   *   *M32.1155
      DO  12  KA = 1, 2                                                 M32.1156
         IBK = 0                                                        M32.1157
C     READ INPUT CARD USING CIMAGE                                      M32.1158
    3    CALL CIMAGE                                                    M32.1159
         DECODE( 80, 4, ABUFF(1) ) AR1, AL1, AC1, ARL                   M32.1160
    4 FORMAT(4E16.0)                                                    M32.1161
         IFK = 3                                                        M32.1162
         IF( AR1 .EQ. 9999. ) GO TO 10                                  M32.1163
         D3 = AR1 + AL1 + AC1 + ARL                                     M32.1164
         IF( D3 .GT. 0. ) GO TO 6                                       M32.1165
      LSTAT( 19 ) = 6                                                   M37.2216
      GO TO 3719                                                        M37.2217
    6    IF( AL1 .GT. 0. ) GO TO 7                                      M32.1169
         AR1 = AR1 + ARL                                                M32.1170
         ARL = 0.                                                       M32.1171
    7    IBK = IBK + 1                                                  M32.1172
         IKK = IKK + 1                                                  M32.1173
         IGK = 7                                                        M32.1174
         IF ( IKK+4  .LE.  L27DEP )   GO TO 8                           M32.1175
      LSTAT( 19 ) = 8                                                   M37.2218
      GO TO 3742                                                        M37.2219
    8    RMFD(IKK) = AR1                                                M32.1179
         RMFD(IKK+1) = AL1                                              M32.1180
         RMFD(IKK+2) = AC1                                              M32.1181
         RMFD(IKK+3) = ARL                                              M32.1182
         RMFD(IKK+4) = 0.                                               M32.1183
         IKK = IKK + 4                                                  M32.1184
         IF( NOUTPR .EQ. 0 )                                            M32.1185
     1 WRITE (KUNIT6, 9)  AR1, AL1, AC1                                 M35. 732
    9 FORMAT(13H+ NEXT BRANCH,8X,3E10.3)                                M32.1187
         GO TO 3                                                        M32.1188
   10    IF( NOUTPR .EQ. 0 )                                            M32.1189
     1 WRITE (KUNIT6, 11)                                               M35. 733
   11 FORMAT(32H+SPECIAL TERMINATION OF BRANCHES)                       M32.1191
         IFK = 10                                                       M32.1192
         IF( IBK .GT. 0 )  GO TO 15                                     M37.2220
      LSTAT( 19 ) = 15                                                  M37.2221
      GO TO 3719                                                        M37.2222
 15      KB = IDK + KA                                                  M37.2223
         IGK = 12                                                       M32.1195
         IF( KB .LE. LSIZ27 ) GO TO 16                                  M37.2224
      LSTAT( 19 ) = 16                                                  M37.2225
      GO TO 3742                                                        M37.2226
   16    IMFD( KB ) = IBK                                               M37.2227
   12 CONTINUE                                                          M32.1198
      IF( IPRSUP .LT. 1 ) GO TO 14                                      M32.1199
      ISK = ISFD + 1                                                    M32.1200
      WRITE (LUNIT6, 13 ) ISK, IKK, ( RMFD(KA), KA = ISK, IKK )         M32.1201
   13 FORMAT(28H AT END OF FDDATA  RMFD FROM,I6,4H  TO,I6,/,            M32.1202
     1(2X,6E21.12))                                                     M32.1203
   14 ISFD = IKK                                                        M32.1204
      IGK = 14                                                          M32.1205
      IBF = IBF + ( IMFD(IDK+1) + IMFD(IDK+2) * (ITYPE-1) ) * 3         M32.1206
      IF ( IBF  .LE.  L27DEP )   GO TO 9999                             M37.2228
      LSTAT( 19 ) = 14                                                  M37.2229
 3742 KILL = 1                                                          M37.2230
      GO TO 9999                                                        M37.2231
 3719 KILL = 37                                                         M37.2232
 9999 RETURN                                                            M32.1208
      END                                                               M32.1209
      SUBROUTINE NONLN2                                                 M28.2891
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL02.FOR;/NOLIST'                                           
      REAL*8        TEXT1, TEXT2, TEXT3                                 M42.2121
      DATA  TEXT1   /  6HSINGLE  /                                      M28.2897
      DATA  TEXT2   /  6H FLASH  /                                      M28.2898
      IF ( IPRSUP  .GE.  1 )                                            M32.1210
     1 WRITE ( LUNIT6, 4567 )                                           M32.1211
 4567 FORMAT ( 24H  "BEGIN MODULE NONLN2." )                            M32.1212
      LL9 = 9                                                           M28.2899
C     FOLLOWING CHECK IS FOR SATURABLE TRANSFORMER CHAR.                M28.2900
      IF ( LSTAT(18)  .EQ.  187 )   GO TO 187                           M28.2901
      IF( ITYPE .GE. 91 )  GO TO 4221                                   M28.2902
      KILL = 3                                                          M28.2903
      LSTAT(19) = 4221                                                  M28.2904
      LSTAT(16) = ITYPE                                                 M28.2905
      GO TO 9999                                                        M28.2906
 4221 IPRINT = 9                                                        M28.2907
      INONL=INONL+1                                                     M28.2908
      LSTAT(19)= 6862                                                   M28.2909
      IF(INONL.GT.LNONL) GO TO 9000                                     M28.2910
      IF ( MOLDAT  .EQ.  0 )   GO TO 4236                               M28.2911
      DECODE (80, 5432, ABUFF(1))  TR(IT), TX(IT), C(IT),               M28.2912
     1                             TR(IT+1)                             M28.2913
 5432 FORMAT ( 26X,  4E12.0 )                                           M28.2914
 4236 IF ( BUS3  .NE.  BRANCH )   GO TO 6874                            M28.2915
      N16 = 1                                                                   
      TEXT3 = BUS4                                                              
      CALL NAMEA6 ( TEXT3, N16 )                                                
      IF ( N16 .EQ. -INTINF )  GO TO 6824                                       
      KILL = 167                                                        M28.2919
      LSTAT(19) = 4236                                                  M42.2126
      LSTAT(14) = N16                                                           
      GO TO 9999                                                        M28.2922
 6824 N16 = 0                                                                   
      CALL NAMEA6 ( TEXT3, N16 )                                                
      NAMENL(INONL) = N16                                                       
      BUS3 = BLANK                                                      M28.2926
      BUS4 = BLANK                                                      M28.2927
      N4 = 1                                                            M28.2928
      N3 = 1                                                            M28.2929
 6874 IBR = IBR - 1                                                     M28.2930
      NLTYPE(INONL) = ITYPE                                             M28.2931
      NONLK(INONL)=N1                                                   M28.2932
      IF(IOUT.GT.0) N2=-N2                                              M28.2933
      NONLM(INONL)=N2                                                   M28.2934
      VNONL(INONL)=TR(IT)                                               M28.2935
      IF ( ITYPE  .NE.  93     .AND.                                    M28.2936
     1     ITYPE  .NE.  96     .AND.                                    M28.2937
     2     ITYPE  .NE.  98 )   GO TO 185                                M28.2938
      VZERO(INONL)=TR(IT)                                               M28.2939
      ANONL(INONL)=TX(IT)                                               M28.2940
      VNONL(INONL)=0.                                                   M28.2941
  185 CURR(INONL)=0.                                                    M28.2942
      IF ( ITYPE  .EQ.  94 )   GO TO 270                                M28.2943
      IF( ITYPE .NE. 99 )  GO TO 73412                                  M28.2944
      D3 = C(IT)                                                        M28.2945
      IF ( MOLDAT  .GT.  0 )   GO TO 4257                               M28.2946
      DECODE (80, 73400, ABUFF(1) )   D3                                M28.2947
73400 FORMAT ( 38X,  E6.0 )                                             M28.2948
 4257 IF ( D3  .EQ.  0.0 )   D3 = 1.0                                   M28.2949
      ANONL(INONL) = D3                                                 M28.2950
      N10 = D3                                                          M29.1275
      VECNL1(INONL) = TX(IT)                                            M28.2951
      VECNL2(INONL) = TX(IT)                                            M28.2952
      NLTYPE(INONL) = -99                                               M28.2953
      NUM99 = NUM99 + 1                                                 M28.2954
      CURR(INONL) = 0.0                                                 M28.2955
      VZERO(INONL) = TR(IT+1)                                           M28.2956
      IF(  VZERO(INONL)  .GT.  VNONL(INONL)  )   VZERO(INONL) = 0.0     M28.2957
      IF( N3*N4 .NE. 1 )  GO TO 182                                     M28.2958
      IF ( NOUTPR  .EQ.  0 )                                            M28.2959
     1 WRITE (KUNIT6, 73406)  TR(IT), TX(IT), N10                       M35. 734
73406 FORMAT ( 21H+TYPE-99 NONLINEAR R.  , 2X, 2E11.3, I5 )             M28.2961
73408 CI1 = 0.0                                                         M28.2962
      GO TO 73420                                                       M28.2963
73412 IF( ITYPE .NE. 98 )  GO TO 73416                                  M28.2964
      NLTYPE(INONL) = -98                                               M28.2965
      NUM99 = NUM99 + 1                                                 M28.2966
      CURR(INONL) = 1.0                                                 M28.2967
      IF( N3*N4  .NE.  1 )  GO TO 182                                   M28.2968
      IF ( NOUTPR  .EQ.  0 )                                            M28.2969
     1 WRITE (KUNIT6, 73413)  TR(IT), TX(IT)                            M35. 735
73413 FORMAT ( 28H+TYPE-98 PSEUDO-NONLINEAR L.  , 2E11.3  )             M28.2971
      GO TO 73408                                                       M28.2972
73416 IF( ITYPE .NE. 97 )  GO TO 73418                                  M28.2973
      NLTYPE(INONL) = -97                                               M28.2974
      NUM99 = NUM99 + 1                                                 M28.2975
      ANONL(INONL) = TX(IT)                                             M28.2976
      IF( N3*N4  .NE.  1 )  GO TO 182                                   M28.2977
      IF ( NOUTPR  .EQ.  0 )                                            M28.2978
     1 WRITE (KUNIT6, 73417 )  TR(IT), TX(IT)                           M35. 736
73417 FORMAT ( 14H+TYPE-97 R(T).  , 10X, 2E12.4  )                      M28.2980
73418 IF ( ITYPE  .NE.  96 )   GO TO 4741                               M28.2981
      NLTYPE(INONL) = -96                                               M28.2982
      NONLAD(INONL) = ICHAR + 1                                         M28.2983
      ILAST(INONL) = NONLAD(INONL) + 6                                  M28.2984
      NUM99 = NUM99 + 1                                                 M28.2985
      VECNL1(INONL) = C(IT)                                             M28.2986
      IF ( N3*N4  .EQ.  1 )   GO TO 4736                                M28.2987
      ICHAR = ICHAR + 6                                                 M28.2988
      NONLE(INONL) = -ICHAR                                             M28.2989
      GO TO 182                                                         M28.2990
 4736 IF ( NOUTPR  .EQ.  0 )                                            M28.2991
     1 WRITE (KUNIT6, 4738)  TR(IT), TX(IT), C(IT)                      M35. 737
 4738 FORMAT ( 12H+HYSTERESIS.,  3E12.4  )                              M28.2993
 4741 CONTINUE                                                          M28.2994
      IF(N3.NE.1 .OR. N4.NE.1) GO TO 182                                M28.2995
      IF ( ITYPE  .EQ.  93     .AND.                                    M28.2996
     1     NOUTPR  .EQ.  0 )                                            M28.2997
     1 WRITE (KUNIT6, 54113)  TR(IT), TX(IT)                            M35. 738
54113 FORMAT ( 26H+N.L. INDUCTANCE, TYPE 93.  ,  2X, 2E11.3  )          M28.2999
      IF( ITYPE .GT. 91 )  GO TO 19                                             
      IF( NOUTPR  .EQ. 0 )                                              M37.2234
     1 WRITE (KUNIT6, 54114)  TR(IT), C(IT)                             M37.2235
54114 FORMAT( 25H+TIME-VARYING R, TYPE 91.  , 3X, 2E11.3 )              M37.2236
      IF( C(IT)  .EQ.  3333. )   GO  TO 12                              M37.2237
      KILL = 5                                                                  
      LSTAT( 19 ) = 20                                                  M37.2239
      GO TO 9999                                                        M37.2240
12    NLTYPE( INONL ) = 923                                                     
      GO TO 21                                                                  
19    IF( ITYPE .NE. 92 ) GO TO 80                                              
      IF( NOUTPR  .EQ.  0 )                                             M37.2244
     1 WRITE (KUNIT6, 54115)  TR(IT), TX(IT), C(IT)                     M37.2245
54115 FORMAT ( 15H+ GAP, TYPE 92., 1X, 3E11.3  )                        M37.2246
      IF( TR(IT) .NE. 5555. )  GO TO 20                                         
      KILL = 28                                                         M37.2248
      LSTAT( 19 ) = 20                                                  M37.2249
      GO TO 9999                                                        M37.2250
 20   IF( C(IT) .EQ. 4444.  .OR.  C(IT) .EQ. 5555. )  GO TO 21          M37.2251
      KILL = 5                                                                  
      LSTAT( 19 ) = 20                                                  M37.2253
      GO TO 9999                                                        M37.2254
 21   IPRINT = ICHAR + 1                                                M37.2255
      NONLAD( INONL ) = IPRINT                                          M37.2256
      IPRINT = 10                                                       M37.2257
C     READ CARDS USING CIMAGE                                           M37.2258
      CALL CIMAGE                                                       M37.2259
      IF( KOLBEG .GT. 0 ) GO TO 22                                      M37.2260
      DECODE (80, 149, ABUFF) D2, D3, D4                                M37.2261
 149  FORMAT( 3E25.0 )                                                  M37.2262
      GO TO 23                                                          M37.2263
 22   NFRFLD = 1                                                        M37.2264
      NRIGHT = 0                                                        M37.2265
      CALL FREONE( D2 )                                                 M37.2266
      CALL FREONE( D3 )                                                 M37.2267
      CALL FREONE( D4 )                                                 M37.2268
 23   CK1 = - FLTINF                                                    M37.2269
      A2 = 0.0                                                          M37.2270
      VZERO( INONL ) = D4                                                       
      IF( C(IT) .EQ. 4444. )   GO TO 40                                         
C     ENTER ZNO DATA CODE********************************************** M37.2273
      IF( D2 .GT. 0.0 )  GO TO 25                                       M37.2274
      KILL =28                                                                  
      LSTAT( 19 ) = 23                                                  M37.2276
      GO TO 9999                                                        M37.2277
 25   ANONL( INONL ) = D2                                                       
      IF( ITYPE  .EQ.  91  )  GO TO  45                                         
      IF( D3 .LE. 0.0 )  D3 = FLTINF                                    M37.2280
      VNONL( INONL ) = D3                                                       
      IF( NOUTPR .EQ. 0 )                                               M37.2282
     1 WRITE (KUNIT6, 57 ) D2, D3, D4                                   M37.2283
 57   FORMAT( 16H+VREF,VGAP,VINIT, 1X, 3E11.3 )                         M37.2284
C     READ-IN ARRESTER CHARACTERISTICS********************************* M37.2285
      ISEG = 2                                                          M37.2286
      IF( D3 .EQ. FLTINF )  ISEG = 1                                            
      IF( ISEG  .EQ.  2 )   VNONL( INONL ) = D3 * D2                            
      DO 39 IBK = 1, ISEG                                               M37.2289
      ICHAR = ICHAR + 1                                                 M37.2290
      ICHR = ICHAR                                                      M37.2291
      ICARD = 0                                                         M37.2292
 29   ICARD = ICARD + 1                                                 M37.2293
C     READ CARDS USING CIMAGE                                           M37.2294
      CALL CIMAGE                                                       M37.2295
      IF( KOLBEG .GT. 0 )  GO TO 30                                     M37.2296
      DECODE(80, 149, ABUFF) D2, D3, D4                                 M37.2297
      GO TO 31                                                          M37.2298
 30   NFRFLD = 1                                                        M37.2299
      NRIGHT = 0                                                        M37.2300
      CALL FREONE( D2 )                                                 M37.2301
      CALL FREONE( D3 )                                                 M37.2302
      CALL FREONE( D4 )                                                 M37.2303
 31   IF( D2 .EQ. 9999. )   GO TO 37                                            
      ICHAR = ICHAR + 1                                                 M37.2305
      IF( ICHAR .LE. LCHAR )  GO TO 36                                  M37.2306
      LSTAT( 19 ) = 36                                                  M37.2307
      GO TO 9000                                                        M37.2308
 36   CONTINUE                                                          M37.2309
      CCHAR( ICHAR ) = D2                                                       
      GSLOPE( ICHAR ) = D3                                                      
      VCHAR( ICHAR ) = D4                                                       
      IF( NOUTPR .EQ. 0 )                                               M37.2313
     1 WRITE(KUNIT6, 59 ) D2, D3, D4                                    M37.2314
 59   FORMAT( 12H+ BREAKPOINT , 4X, 3E11.4 )                            M37.2315
      GO TO 29                                                          M37.2316
 37   IF( ICARD .GT. 1 )   GO TO 38                                     M37.2317
C     NO VALID DATA ENCOUNTERED, TERMINATE RUN                          M37.2318
      KILL = 28                                                                 
      LSTAT( 19 ) = 37                                                  M37.2320
      GO TO 9999                                                        M37.2321
 38   IF( NOUTPR .EQ. 0 )                                               M37.2322
     1 WRITE(KUNIT6, 54118)                                             M37.2323
      IF( IBK .EQ. 1 ) ILAST( INONL ) = ICHAR                           M37.2324
C     CALCULATE INITIAL ( LINEAR ) SLOPE                                M37.2325
      D11 = CCHAR( ICHR+1 ) * ( VCHAR( ICHR+1 )**GSLOPE( ICHR+1 ) )     M37.2326
      CCHAR( ICHR ) = D11 / ( VCHAR( ICHR+1 ) * ANONL( INONL ) )        M37.2327
      VCHAR( ICHR ) = 0.0                                               M37.2328
 39   CONTINUE                                                          M37.2329
      NONLE( INONL ) = ICHAR                                            M37.2330
      NLTYPE( INONL ) = 921                                                     
      GO TO 100                                                         M37.2332
C     ENTER CODE FOR PIECEWISE LINEAR AND TIME VARYING RESISTANCE****** M37.2333
 40   A2 = D2                                                           M37.2334
      IF( D3 .LT. 0.0 )   D3 = FLTINF                                   M37.2335
      VNONL( INONL ) = D3                                                       
      VECNL1( INONL ) = TR( IT )                                                
      IF( NOUTPR .EQ. 0 )                                               M37.2338
     1 WRITE( KUNIT6, 58 ) D2, D3, D4                                   M37.2339
 58   FORMAT( 19H+RLIN,VFLASH,NFLASH, 1X, 3E10.3 )                      M37.2340
      GO TO  46                                                                 
 45   IF( NOUTPR  .EQ.  0 )                                             M37.2342
     1 WRITE( KUNIT6, 60 )  D2                                          M37.2343
 60   FORMAT(   9H+V START=, E15.5 )                                    M37.2344
      ANONL( INONL ) =  2.0 * D2                                                
      VNONL( INONL ) =  D2                                                      
      ILAST( INONL ) =  1                                                       
C     READ-IN NONLINEAR CHARACTERISTIC (MININMUM TWO DATA POINTS)****** M37.2348
 46   ICARD = 0                                                         M37.2349
 47   ICARD = ICARD + 1                                                 M37.2350
C     READ CARDS USING CIMAGE                                           M37.2351
      CALL CIMAGE                                                       M37.2352
      IF( KOLBEG .GT. 0 )  GO  TO  48                                           
      DECODE( 80, 149, ABUFF ) D2, D3                                   M37.2354
      GO TO 49                                                          M37.2355
 48   NFRFLD = 1                                                        M37.2356
      NRIGHT = 0                                                        M37.2357
      CALL FREONE( D2 )                                                 M37.2358
      CALL FREONE( D3 )                                                 M37.2359
 49   IF( D2 .EQ. 9999. )  GO TO  53                                            
      IF( NOUTPR .EQ. 0 )                                               M37.2361
     1 WRITE( KUNIT6, 59 ) D2, D3                                       M37.2362
      IF( ITYPE  .EQ.  91 )  GO TO 51                                           
      IF( D3 .GT. CK1 )  GO TO 51                                       M37.2364
      KILL = 5                                                          M37.2365
      LSTAT( 19 ) = 49                                                  M37.2366
      GO TO 9999                                                        M37.2367
 51   ICHAR = ICHAR + 1                                                 M37.2368
      IF( ICHAR .LE. LCHAR )  GO  TO  52                                        
      LSTAT( 19 ) = 51                                                  M37.2370
      GO TO 9000                                                        M37.2371
 52   VCHAR( ICHAR ) = D3 + A2 * D2                                             
      CK1 = D3                                                                  
      CCHAR( ICHAR ) = D2                                               M37.2374
      GO TO 47                                                          M37.2375
 53   IF( ICARD .GT. 2 )  GO  TO  54                                    M37.2376
      KILL = 28                                                                 
      LSTAT( 19 ) = 53                                                  M37.2378
      GO TO 9999                                                        M37.2379
 54   IF( NOUTPR .EQ. 0 )                                               M37.2380
     1 WRITE( KUNIT6, 54118 )                                           M37.2381
C     PROCESS INPUT DATA (CALCULATE SLOPE + CONSTANT, EXTEND DATA       M37.2382
C     POINTS TO INFINITY , DETERMINE VREFERENCE************************ M37.2383
      ICHR = NONLAD( INONL )                                                    
      IF( ITYPE  .EQ. 91 )   GO TO 74                                           
      D5 = ABSZ( VCHAR( ICHR ) )                                                
      D3 = ABSZ( VCHAR( ICHAR ) )                                               
      IF( D5 .GT. D3 )   D3 = D5                                                
      ANONL( INONL ) = D3                                                       
C     DETERMINE WHETHER ONLY UPPER HALF OF DATA SPECIFIED ************* M37.2390
      IF( VCHAR(ICHR) .GT. 0.0 .AND. CCHAR(ICHR) .GT. 0.0 )             M37.2391
     1 GO TO 55                                                                 
      GO TO 74                                                                  
C     ADD ( 0.0, 0.0 ) POINT TO THE USER'S DATA *********************** M37.2394
 55   JK = ICHAR + 1                                                    M37.2395
      IF( JK .LE. LCHAR )  GO TO 70                                             
      LSTAT( 19 ) = 55                                                  M37.2397
      GO TO 9000                                                                
 70   DO 71 JB = ICHR, ICHAR                                            M37.2399
      VCHAR( JK ) = VCHAR( JK-1 )                                       M37.2400
      CCHAR( JK ) = CCHAR( JK-1 )                                       M37.2401
 71   JK = JK - 1                                                       M37.2402
      VCHAR( ICHR ) = 0.0                                               M37.2403
      CCHAR( ICHR ) = 0.0                                               M37.2404
      ICHAR = ICHAR + 1                                                         
 74   ICHR = ICHR + 1                                                   M37.2406
      DO 75 JB = ICHR, ICHAR                                            M37.2407
      D5 = CCHAR( JB ) - CCHAR( JB-1 )                                  M37.2408
      IF( JB .EQ. ICHAR  .AND.  VCHAR( JB ) .EQ. VCHAR( JB-1) )         M39. 999
     1  GO TO 77                                                                
      D5 = D5 / ( VCHAR( JB ) - VCHAR( JB-1 ) )                                 
      GO TO 78                                                          M39.1001
 77   D5 = 0.0                                                          M39.1002
 78   D6 = CCHAR( JB ) - VCHAR( JB ) * D5                                       
      GSLOPE( JB-1 ) = D5                                                       
 75   CCHAR( JB-1 ) = D6                                                        
      ICHAR = ICHAR - 1                                                         
      NONLE( INONL ) = ICHAR                                                    
      IF( ITYPE  .EQ.  91  )  GO TO 76                                          
      IF( VCHAR( ICHR-1 ) .LT. 0.0 )    VCHAR( ICHR-1 ) = -FLTINF       M37.2416
      ILAST( INONL ) = 1                                                        
      IF( VNONL( INONL ) .EQ. FLTINF .OR. VNONL( INONL ) .EQ. 0.0 )     M37.2418
     1  ILAST( INONL ) = -1                                                     
      NLTYPE( INONL ) = 922                                                     
      GO TO 100                                                         M37.2421
 76   ILAST( INONL )  =  1                                                      
      VECNL1( INONL ) = -1.0                                                    
      GO TO 100                                                         M37.2424
 80   CI1 = - FLTINF                                                    M37.2425
73420 CK1 = CI1                                                         M28.3011
      IF ( ITYPE  .NE.  96 )   GO TO 73421                              M28.3012
      ICHAR = ICHAR + 7                                                 M28.3013
      GO TO 73422                                                       M28.3014
73421 IPRINT = ICHAR + 1                                                M28.3015
      NONLAD(INONL)=IPRINT                                              M28.3017
73422 IPRINT = 10                                                       M28.3018
C     READ INPUT CARD USING CIMAGE.                                     M28.3019
  187 CALL CIMAGE                                                       M28.3020
      IF ( KOLBEG  .GT.  0 )   GO TO 73423                              M28.3021
      DECODE(80, 188, ABUFF)  D2, XLONG                                 M28.3022
  188 FORMAT ( 3E16.0 )                                                 M37.2426
      GO TO 73424                                                       M28.3024
73423 NFRFLD = 1                                                        M28.3025
      NRIGHT = 0                                                        M28.3026
      CALL FREONE ( D2 )                                                M29.1277
      CALL FREONE ( XLONG )                                             M29.1278
73424 IF ( D2  .EQ.  9999. )   GO TO 189                                M28.3029
      IF ( NOUTPR  .EQ.  0 )                                            M28.3030
     1 WRITE (KUNIT6, 181) D2,XLONG                                     M35. 741
  181 FORMAT( 16H+    BREAKPOINT.  , 2E15.5  )                          M28.3032
      IF ( D2  .GT.  CI1 )   GO TO 4225                                 M28.3033
 4224 KILL = 5                                                          M28.3034
      LSTAT(19) = 181                                                   M28.3035
      GO TO 9999                                                        M28.3036
 4225 IF ( ITYPE  .NE.  96     .AND.                                    M28.3037
     1     ITYPE  .NE.  98 )   GO TO 4227                               M28.3038
      IF ( XLONG  .GT.  CK1 )   GO TO 4226                              M28.3039
      GO TO 4224                                                        M28.3040
 4227 IF ( XLONG  .GE.  CK1 )   GO TO 4226                              M28.3041
      IF( ITYPE .EQ. 97 )  GO TO 4226                                   M28.3043
      GO TO 4224                                                        M28.3044
 4226 ICHAR = ICHAR + 1                                                 M28.3045
      IF ( ICHAR  .LE.  LCHAR )   GO TO 6472                            M28.3046
      IPRINT = 10                                                       M28.3047
      LSTAT(19) = 4226                                                  M28.3048
      GO TO 9000                                                        M28.3049
 6472 D3 = D2 - CI1                                                     M28.3050
      D4 = XLONG - CK1                                                  M28.3051
      IF( ITYPE .NE. 99 )  GO TO 73430                                  M28.3052
      GSLOPE(ICHAR) = D3 / D4                                           M28.3053
      CCHAR(ICHAR) = CI1 - GSLOPE(ICHAR) * CK1                          M28.3054
      GO TO 73436                                                       M28.3055
73430 IF( ITYPE .NE. 98 )  GO TO 73437                                  M28.3056
      D1 = D4 / D3                                                      M28.3057
      GSLOPE(ICHAR) = DELTA2 / D1                                       M28.3058
      CCHAR(ICHAR) = CK1 - D1 * CI1                                     M28.3059
      GO TO 73436                                                       M28.3060
73437 IF( ITYPE .NE. 97 )  GO TO 73441                                  M28.3061
      IF ( XLONG  .GT.  0.0 )   GO TO 7344                              M28.3062
      KILL = 72                                                         M28.3063
      LSTAT(19) = 7344                                                  M28.3064
      FLSTAT(16) = XLONG                                                M28.3065
      GO TO 9999                                                        M28.3066
 7344 GSLOPE(ICHAR) = 1.0 / XLONG                                       M28.3067
73441 IF ( ITYPE  .NE.  96 )   GO TO 73442                              M28.3068
      CCHAR(ICHAR) = D2                                                 M28.3069
      VCHAR(ICHAR) = XLONG                                              M28.3070
      C1 = D2                                                           M28.3071
      CK1 = XLONG                                                       M28.3072
      IF ( IPRSUP  .GE.  4 )                                            M28.3073
     1 WRITE (LUNIT6, 4232)  ICHAR, INONL, NONLAD(INONL), D2, XLONG     M28.3074
 4232 FORMAT ( /,  16H TYPE-96  POINT.,  16H   ICHAR   INONL,           M28.3075
     1  3X,  13HNONLAD(INONL),  13X,  2HD2,  10X,  5HXLONG  ,/,         M28.3076
     2  16X,  2I8,  8X,  I8,  2E15.6  )                                 M28.3077
      GO TO 187                                                         M28.3078
73442 CCHAR(ICHAR) = D2                                                 M28.3079
73436 CI1 = D2                                                          M28.3080
      CK1 = XLONG                                                       M28.3081
      VCHAR(ICHAR)=XLONG                                                M28.3083
      GO TO 187                                                         M28.3084
  189 IF ( ITYPE  .NE.  96 )   GO TO 4758                               M28.3085
      N11 = NONLAD(INONL)                                               M28.3086
      N12 = ILAST(INONL)                                                M28.3087
      CCHAR(N12) = -CCHAR(ICHAR-1)                                      M28.3088
      VCHAR(N12) = -VCHAR(ICHAR-1)                                      M28.3089
      CCHAR(N11) = ICHAR - ILAST(INONL)                                 M28.3090
      N13 = CCHAR(N11)                                                  M28.3091
C     CALCULATE THE SLOPE AND INTERCEPT FOR MAJOR HYSTERESIS LOOP       M28.3092
C     SEGMENTS,  FOR  TYPE-96  PSEUDONONLINEAR ELEMENT.                 M28.3093
      N14 = ICHAR + 2                                                   M28.3094
      N15 = N14 + N13 - 1                                               M28.3095
      DO 5300  N16=N14, N15                                             M28.3096
      N17 = N16 - N13 - 1                                               M28.3097
      VCHAR(N16) = ( VCHAR(N17)  -  VCHAR(N17-1) )  /                   M28.3098
     1 ( CCHAR(N17)  -  CCHAR(N17-1)  )                                 M28.3099
      CCHAR(N16) = VCHAR(N17) - VCHAR(N16) * CCHAR(N17)                 M28.3100
      GSLOPE(N17) = 1.0 / VCHAR(N16)                                    M28.3101
      GSLOPE(N16) = -CCHAR(N16) / VCHAR(N16)                            M28.3102
 5300 CONTINUE                                                          M28.3103
      N16 = ICHAR + N13 + 1                                             M28.3104
      IF ( IPRSUP  .GE.  2 )                                            M28.3105
     1 WRITE (LUNIT6, 4234)  INONL, ICHAR, N11, N13, TR(IT),            M28.3106
     2                       VECNL1(INONL), ANONL(INONL)                M28.3107
 4234 FORMAT ( /,  17H PROCESS TYPE-96.,                                M28.3108
     1  32H   INONL   ICHAR     N11     N13,  14X,  6HTR(IT),           M28.3109
     2  7X,  13HVECNL1(INONL),  8X,  12HANONL(INONL)  ,/,               M28.3110
     3  17X,  4I8,  3E20.11  )                                          M28.3111
      CCHAR(ICHAR+1) = -CCHAR(N16)                                      M28.3112
      VCHAR(ICHAR+1) = VCHAR(N16)                                       M28.3113
      GSLOPE(N12) = GSLOPE(ICHAR)                                       M28.3114
      GSLOPE(ICHAR+1) = -GSLOPE(N16)                                    M28.3115
      ICHAR = ICHAR + N13 + 1                                           M28.3116
      IF ( IPRSUP  .GE.  3 )                                            M28.3117
     1 WRITE (LUNIT6, 4239)  ICHAR,  ( CCHAR(J), VCHAR(J), GSLOPE(J),   M28.3118
     2                       J=N11, ICHAR )                             M28.3119
 4239 FORMAT ( /,  8H ICHAR =,  I3,  5X,                                M28.3120
     1 57H(CCHAR(J), VCHAR(J), GSLOPE(J), J=N11, ICHAR)  FOLLOW ...     M28.3121
     2  ,/,  ( 1X,  6E20.11 )  )                                        M28.3122
C     HANDLING OF STEADY-STATE CURRENT AND FLUX CONDITIONS.             M28.3123
 5309 N12 = ILAST(INONL)                                                M28.3124
      N13 = NONLAD(INONL)                                               M28.3125
      N13 = CCHAR(N13)                                                  M28.3126
      N14 = N12+N13-1                                                   M28.3127
      IF ( TR(IT)  .EQ.  8888. )   GO TO 5330                           M28.3128
C     MAKE SURE THAT THE USER-SPECIFIED POINT LIES WITHIN THE MAJOR     M28.3129
C     HYSTERESIS LOOP.                                                  M28.3130
      DO 5310  N15=N12, N14                                             M28.3131
      IF ( TR(IT)  .GT.  CCHAR(N15) )   GO TO 5310                      M28.3132
      N16 = N15                                                         M28.3133
      GO TO 5313                                                        M28.3134
 5310 CONTINUE                                                          M28.3135
      N16 = N14 + 1                                                     M28.3136
 5313 N17 = N16 + N13 + 1                                               M28.3137
      D7 = VCHAR(N17) * TR(IT) + CCHAR(N17)  -  FLZERO                  M28.3138
      DO 5320  N15=N12, N14                                             M28.3139
      IF ( -TR(IT)  .GT.  CCHAR(N15) )   GO TO 5320                     M28.3140
      N16 = N15                                                         M28.3141
      GO TO 5323                                                        M28.3142
 5320 CONTINUE                                                          M28.3143
      N16 = N14 + 1                                                     M28.3144
 5323 N17 = N16 + N13 + 1                                               M28.3145
      D8 = -VCHAR(N17) * (-TR(IT))  -  CCHAR(N17)  +  FLZERO            M28.3146
      IF ( IPRSUP  .GE.  2 )                                            M28.3147
     1 WRITE (LUNIT6, 5327)  IT, N16, N17, D7, D8, ANONL(INONL), FLZERO M28.3148
 5327 FORMAT ( /,  20H TYPE-96 S.S. CHECK.,                             M28.3149
     1  24H      IT     N16     N17,  13X,  2HD7,  13X,  2HD8,          M28.3150
     2  3X,  12HANONL(INONL),  9X,  6HFLZERO  ,/,  20X,  3I8,  4E15.6 ) M28.3151
      IF ( ANONL(INONL)  .LE.  D8      .AND.                            M28.3152
     1     ANONL(INONL)  .GE.  D7 )    GO TO 5360                       M28.3153
      KILL = 204                                                        M28.3154
      LSTAT(19) = 5323                                                  M28.3155
      FLSTAT(14) = TR(IT)                                               M28.3156
      FLSTAT(15) = TX(IT)                                               M28.3157
      GO TO 9999                                                        M28.3158
C     IT IS DESIRED TO HAVE THE PROGRAM CALCULATE THE VALUES FOR        M28.3159
C     STEADY-STATE FLUX AND CURRENT BASED ON 70 PERCENT OF SATURATION.  M28.3160
 5330 D12 = 0.7D0                                                       M28.3161
      ANONL(INONL) = D12 * VCHAR(N14)                                   M28.3162
      DO 5340  N15=N12, N14                                             M28.3163
      IF ( CCHAR(N15)  .LT.  0.0 )   GO TO 5340                         M28.3164
      N16 = N15                                                         M28.3165
      GO TO 5343                                                        M28.3166
 5340 CONTINUE                                                          M28.3167
      N16 = N14 + 1                                                     M28.3168
 5343 N17 = N16 + N13 + 1                                               M28.3169
      D9 = -CCHAR(N17)                                                  M28.3170
      D12 = 0.3D0                                                       M28.3171
      D10 = ANONL(INONL) - D12 * D9                                     M28.3172
      DO 5350  N15=N12, N14                                             M28.3173
      IF ( D10  .GT.  VCHAR(N15) )   GO TO 5350                         M28.3174
      N16 = N15                                                         M28.3175
      GO TO 5353                                                        M28.3176
 5350 CONTINUE                                                          M28.3177
      N16 = N14 + 1                                                     M28.3178
 5353 N17 = N16 + N13 + 1                                               M28.3179
      VZERO(INONL) = GSLOPE(N16) * D10 + GSLOPE(N17)                    M28.3180
      IF ( IPRSUP  .GE.  2 )                                            M28.3181
     1 WRITE(LUNIT6,5355) N12,N14,N16,N17,D9,ANONL(INONL),VZERO(INONL), M28.3182
     1                                 GSLOPE(N16),GSLOPE(N17)          M28.3183
 5355 FORMAT(/, 112H PROGRAM CALCULATED SS PT.  N12  N14  N16  N17      M28.3184
     1    D9       ANONL       VZERO    GSLOPE(N16)    GSLOPE(N17),     M28.3185
     2  /,26X,4I5,3(1X,E15.6),2E15.6)                                   M28.3186
      GO TO 5380                                                        M28.3187
C     MAKE SURE THAT THE USER-SPECIFIED RESIDUAL FLUX IS WITHIN THE     M28.3188
C     MAJOR HYSTERESIS LOOP.                                            M28.3189
 5360 DO 5370  N15= N12, N14                                            M28.3190
      IF ( CCHAR(N15)  .LT.  0.0 )   GO TO 5370                         M28.3191
      N16 =N15                                                          M28.3192
      GO TO 5373                                                        M28.3193
 5370 CONTINUE                                                          M28.3194
      N16 = N14 +1                                                      M28.3195
 5373 N17 = N16 + N13 + 1                                               M28.3196
      D9 = -CCHAR(N17)                                                  M28.3197
 5380 D9 = ABSZ(D9)                                                     M28.3198
      D12 = VECNL1(INONL)                                               M28.3199
      D12 = ABSZ(D12)                                                   M28.3200
      IF ( D12  .LE.  D9+FLZERO )   GO TO 4758                          M28.3201
      KILL = 205                                                        M28.3202
      LSTAT(19) = 5380                                                  M28.3203
      FLSTAT(16) = VECNL1(INONL)                                        M28.3204
      FLSTAT(17) = D9                                                   M28.3205
      GO TO 9999                                                        M28.3206
 4758 IF ( N3*N4  .NE.  1 )   GO TO 100                                 M28.3207
      NONLE(INONL) = ICHAR                                              M28.3208
      IF ( ICHAR .LE. LCHAR )  GO TO 4763                               M28.3209
      LSTAT(19) = 4758                                                  M28.3210
      IPRINT = 10                                                       M28.3211
      GO TO 9000                                                        M28.3212
 4763 IF ( ITYPE .NE. 99 )  GO TO 54127                                 M28.3213
      DECODE (80, 54125, ABUFF(1) )  BUS1, BUS2                         M28.3214
54125 FORMAT ( 32X, 2A6 )                                               M28.3215
      IF ( BUS1  .NE.  TEXT1 )   GO TO 54127                            M28.3216
      IF ( BUS2  .NE.  TEXT2 )   GO TO 54127                            M28.3217
      J = NONLAD(INONL)                                                 M28.3218
      SGLFIR = VCHAR(J)                                                 M33. 202
      IF ( NOUTPR  .EQ.  0 )                                            M28.3220
     1 WRITE (KUNIT6, 54126)                                            M35. 742
54126 FORMAT ( 1H+, 39X, 9H'1-FLASH'   )                                M28.3222
54127 IF ( NOUTPR  .EQ.  0 )                                            M28.3223
     1 WRITE (KUNIT6, 54118)                                            M35. 743
54118 FORMAT( 36H+SPECIAL TERMINATION-OF-POINTS CARD.   )               M28.3225
      IF ( ITYPE  .NE.  93     .AND.                                    M28.3226
     1     ITYPE  .NE.  96     .AND.                                    M28.3227
     2     ITYPE  .NE.  98 )   GO TO 100                                M28.3228
      NONLE(INONL)=-ICHAR                                               M28.3229
      VNONL(INONL)=0.                                                   M28.3230
      GO TO 100                                                         M28.3231
  182 II=INONL-1                                                        M28.3232
      IF( II .GT. 0 )  GO TO 4231                                       M28.3233
      LSTAT(19) = 182                                                   M28.3234
      GO TO 4230                                                        M28.3235
 4231 IF ( NOUTPR  .EQ.  0 )                                            M28.3236
     1 WRITE (KUNIT6, 21696)  BUS3, BUS4                                M35. 744
21696 FORMAT ( 28H+REFERENCE BRANCH.   COPY  ",                         M28.3238
     1  A6,  6H" TO ",  A6,  1H"  )                                     M28.3239
      DO 183 I=1,II                                                     M28.3240
      IF ( BUS3  .EQ.  COPY )   GO TO 6881                              M28.3241
      IF( NONLK(I)  .NE.  N3 )  GO TO 183                               M28.3242
      IF( IABS(NONLM(I))  .NE.  N4 )  GO TO 183                         M28.3243
      GO TO 184                                                         M28.3244
 6881 N13 = NAMENL(I)                                                           
      IF ( BUS4 .EQ. TEXVEC(N13) )  GO TO 184                                   
  183 CONTINUE                                                          M28.3246
      LSTAT(19) = 183                                                   M28.3247
 4230 KILL = 6                                                          M28.3248
      GO TO 9999                                                        M28.3249
  184 IF ( ITYPE  .NE.  96 )   GO TO 269                                M28.3250
      ILAST(INONL) = ILAST(I)                                           M28.3252
      N16 = NONLAD(INONL)                                               M28.3253
      N17 = NONLAD(I)                                                   M28.3254
      CCHAR(N16) = CCHAR(N17)                                           M28.3255
      GO TO 5309                                                        M28.3256
  269 NONLAD(INONL) = NONLAD(I)                                         M28.3257
      IF ( ITYPE  .EQ.  94 )   GO TO 100                                M28.3259
      NONLE(INONL)=NONLE(I)                                             M28.3260
      IF( ITYPE .EQ. 99 )  GO TO 100                                    M28.3261
      VZERO(INONL)=VZERO(I)                                             M28.3262
      ANONL(INONL)=ANONL(I)                                             M28.3263
      IF( ITYPE .NE. 91 )  GO TO 72                                     M37.2427
      IF( C(IT) .NE. 3333. )   LSTAT( 19 ) = 269                        M37.2428
      GO TO 73                                                          M37.2429
 72   IF( ITYPE .NE. 92 )  GO TO 100                                    M37.2430
 73   IF( C(IT) .NE. 4444.  .AND.  C(IT) .NE. 5555. )                   M37.2431
     1 LSTAT( 19 ) = 269                                                M37.2432
      IF( LSTAT( 19 ) .EQ. 269 )  GO TO 4230                                    
      VNONL( INONL ) = VNONL( I )                                       M37.2434
      VECNL1( INONL ) = VECNL1( I )                                     M37.2435
      ILAST( INONL ) = ILAST( I )                                       M37.2436
      NLTYPE( INONL ) = NLTYPE( I )                                     M37.2437
      GO TO 100                                                         M28.3264
  270 IF ( ICHAR+11  .LE.  LCHAR )   GO TO 271                          M28.3265
 1270 IPRINT = 10                                                       M28.3266
      LSTAT(19) = 270                                                   M28.3267
      GO TO 9000                                                        M28.3268
  271 IAW = ICHAR + 1                                                   M28.3269
      NONLE(INONL) = IAW                                                M28.3270
      CALL MOVER0 ( VCHAR(IAW), LL9 )                                   M28.3271
      IF ( TX(IT)  .EQ.  0.0 )                                          M28.3272
     1 TX(IT) = 1.0                                                     M28.3273
      VCHAR(ICHAR+10) = TX(IT)                                          M28.3274
      IF ( C(IT)  .EQ.  0.0 )                                           M28.3275
     1 C(IT) = 1.0                                                      M28.3276
      VCHAR(ICHAR+11) = C(IT)                                           M28.3277
      IF ( N3  .NE.  1 )   GO TO 4271                                   M28.3278
      IF ( N4  .EQ.  1 )   GO TO 3271                                   M28.3279
 4271 ICHAR = ICHAR + 11                                                M28.3280
      GO TO 182                                                         M28.3281
 3271 NONLAD(INONL) = -IAW                                              M28.3282
      IF ( NOUTPR  .EQ.  0 )                                            M28.3283
     1 WRITE (KUNIT6, 2271)  TR(IT), TX(IT), C(IT)                      M35. 745
 2271 FORMAT ( 18H+TYPE-94 ARRESTER.,  E10.3, 2E11.4  )                 M28.3285
      IF ( ICHAR+18  .GT.  LCHAR )   GO TO 1270                         M28.3286
      J = 1                                                             M28.3287
      II = 4                                                            M28.3288
C     READ INPUT CARD USING CIMAGE                                      M28.3289
      CALL CIMAGE                                                       M28.3290
      N6 = J + ICHAR                                                    M28.3291
      N7 = II + ICHAR                                                   M28.3292
      DECODE (80, 1272, ABUFF(1) )  (CCHAR(I), I = N6, N7)              M28.3293
 1272 FORMAT ( 5E16.0 )                                                 M28.3294
      II = 3                                                            M28.3295
      N7 = II + ICHAR                                                   M28.3296
      IF ( NOUTPR  .EQ.  0 )                                            M28.3297
     1 WRITE (KUNIT6, 2272)  J, II, (CCHAR(I), I = N6, N7)              M35. 746
 2272 FORMAT ( 11H+  CONSTS. , I2, 1H-, I2, 1H.,  3E11.3 )              M28.3299
      DO 274  J=5, 17, 3                                                M28.3300
      II = J + 2                                                        M28.3301
      IF ( II  .GT.  18 )   II = 18                                     M28.3302
C     READ INPUT CARD USING CIMAGE                                      M28.3303
      CALL CIMAGE                                                       M28.3304
      N6 = J + ICHAR                                                    M28.3305
      N7 = II + ICHAR                                                   M28.3306
      DECODE (80, 1272, ABUFF(1)    )  (CCHAR(I), I = N6, N7)           M28.3307
  274 IF ( NOUTPR  .EQ.  0 )                                            M28.3308
     1 WRITE (KUNIT6, 2272)  J, II, (CCHAR(I), I = N6, N7)              M35. 747
      ICHAR = ICHAR + 18                                                M28.3310
  100 GO TO 9999                                                        M28.3311
 9000 LSTAT(16) = IPRINT                                                M28.3312
      KILL = 1                                                          M28.3313
 9999 IF ( IPRSUP  .GE.  3      .OR.                                    M28.3314
     1     KILL    .GT.  0  )                                           M28.3315
     2 WRITE (LUNIT6, 9998)  KILL, INONL, NUM99, ICHAR, ITYPE           M28.3316
 9998 FORMAT ( /,  17H EXIT  "NONLN2" .,                                M28.3317
     1             40H    KILL   INONL   NUM99   ICHAR   ITYPE          M28.3318
     2  ,/,  17X,  10I8  )                                              M28.3319
      LSTAT(18) = 0                                                     M28.3320
      IF ( IPRSUP  .GE.  1 )                                            M32.1213
     1 WRITE ( LUNIT6, 4568 )                                           M32.1214
 4568 FORMAT ( 24H  "EXIT  MODULE NONLN2." )                            M32.1215
      RETURN                                                            M28.3321
      END                                                               M28.3322
      SUBROUTINE DISTR2                                                 M39.1004
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL02.FOR;/NOLIST'                                           
      DIMENSION CBLHST(1)                                               M44.  20
      EQUIVALENCE  ( CNVHST(1),  CBLHST(1) )                            M44.  21
      DIMENSION  WK1(1)                                                 M43. 392
      EQUIVALENCE ( SEMAUX(1), WK1(1) )                                 M43. 393
      DIMENSION INFDLI(1)                                               M43. 394
      EQUIVALENCE  ( NAMEBR(1),  INFDLI(1) )                                    
      EQUIVALENCE  (OMEGA, XLONG1 ),  ( INDTV(4), ICHTR2 )              M38.1104
      EQUIVALENCE  (INDTV(1), IADDRS), (INDTV(2), ITRANM)               M31. 509
      REAL*8        TEXT1, TEXT2, TEXT3, TEXT4, TEXT5, TEXTMX           M31. 510
      REAL*8        TEXT6, TEXT7, TEXT8, TEXT9, TEXT10, TEXT11          M31. 511
      REAL*8        TEXT12, TEXT13, TEXT14                              M31. 512
      DIMENSION  TEXTMX(2)                                              M31. 513
      DATA  TEXT2   /  6H   CON  /                                      M28.3329
      DATA  TEXT3   /  6HSTANT   /                                      M28.3330
      DATA  TEXT4   /  6HSTEADY  /                                      M28.3331
      DATA  TEXT5   /  6H STATE  /                                      M28.3332
      DATA  TEXT6   /  1HA  /                                           M31. 514
      DATA  TEXT7   /  1HB  /                                           M31. 515
      DATA  TEXT8   /  1HC  /                                           M31. 516
      DATA  TEXT9   /  1HD  /                                           M31. 517
      DATA  TEXT10  /  1HE  /                                           M31. 518
      DATA  TEXT11  /  1HF  /                                           M31. 519
      DATA  TEXT12  /  1HG  /                                           M31. 520
      DATA  TEXT13  /  1HH  /                                           M31. 521
      DATA  TEXT14  /  1HI  /                                           M31. 522
      DATA  TEXTMX(1)   /  6H+TR(    /                                  M31. 523
      DATA  TEXTMX(2)   /  6H+TX(    /                                  M31. 524
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M32.1216
      DATA  IPSEM  / 0 /                                                M32.1217
      IF ( IPRSUP  .GE.  1 )                                            M32.1220
     1 WRITE ( LUNIT6, 4567 )                                           M32.1221
 4567 FORMAT ( 24H  "BEGIN MODULE DISTR2." )                            M32.1222
      N45 = LOCINT ( IDA )                                              M43. 396
      N46 = LOCINT ( IFKC )                                             M43. 397
      ISECTI = 400
      IF ( IPRSUP  .GE.  4 )                                            M43. 398
     1 write (*,*) ' Top distr2.  N45, N46, IDA, IFKC =',               M43. 399
     2                            N45, N46, IDA, IFKC                   M43. 400
      NUMAKI = 0                                                        M30. 137
      LSTAT(18) = 0                                                     M28.3333
      PDT0=0                                                            M43. 401
      IF ( IPRSUP  .GE.  2 )                                            M28.3334
     1 WRITE (LUNIT6, 3213)  ICHECK, MOLDAT, IBR, ITYPE                 M28.3335
 3213 FORMAT ( /,  18H BEGIN  "DISTR2" .,                               M28.3336
     1             32H  ICHECK  MOLDAT     IBR   ITYPE                  M28.3337
     2  ,/,  18X,  4I8  ,/,  1X  )                                      M28.3338
      N13 = 0                                                           M29.1283
      IF ( ICHECK  .EQ.  3 )   GO TO 196                                        
      IF ( ICHECK  .EQ.  5 )   GO TO 20000                                      
  190 IF ( MOLDAT  .EQ.  0 )   GO TO 3265                                       
C     DECODE (80, 3241, ABUFF(1))  TEXT1                                        
C  3241 FORMAT ( 75X,  A1 )                                               M28.3343
C     IF ( TEXT1  .EQ.  BLANK )   GO TO 3265                            M28.3344
      DECODE (80, 3256, ABUFF(1))  H1, AA, H3, XLONG, ILINE, IPUNCH     M28.3345
 3256 FORMAT ( 26X,  4E12.0,  2I2 )                                             
      IF (XLONG .GT. 0.) GO TO 21633                                            
      XLONG = ABSZ(XLONG)                                                       
      DECODE (80, 3257, ABUFF(1) )  TEXT1                                       
 3257 FORMAT ( 78X, A1 )                                                        
      IF ( TEXT1 .EQ. TEXT6  )   N13 = 10                                       
      IF ( TEXT1 .EQ. TEXT7  )   N13 = 11                                       
      IF ( TEXT1 .EQ. TEXT8  )   N13 = 12                                       
      IF ( TEXT1 .EQ. TEXT9  )   N13 = 13                                       
      IF ( TEXT1 .EQ. TEXT10 )   N13 = 14                                       
      IF ( TEXT1 .EQ. TEXT11 )   N13 = 15                                       
      IF ( TEXT1 .EQ. TEXT12 )   N13 = 16                                       
      IF ( TEXT1 .EQ. TEXT13 )   N13 = 17                                       
      IF ( TEXT1 .EQ. TEXT14 )   N13 = 18                                       
      IF ( N13 .NE. 0 )  GO TO 21633                                            
      DECODE (80, 3258, ABUFF(1) )  N13                                         
 3258 FORMAT (78X, I1)                                                  M31. 538
      GO TO 21633                                                       M28.3347
 3265 CONTINUE                                                          M28.3348
      DECODE (80,191,ABUFF) IPSEM                                               
  191 FORMAT(75X,I3)                                                    M28.3350
C      IF (IPSEM .NE. 0) GO TO 21633                                    M44.  23
      IF (IPSEM .NE. 0) GO TO 8010                                              
      DECODE (80,1920,ABUFF) H1,AA,H3,XLONG,ILINE,IPUNCH, N13,LINT      M31. 539
 1920 FORMAT(26X,4E6.2,4I2)                                             M31. 540
C 2345678901234567890123456789012345678901234567890123456789012345678901M43. 402
      IF ( IPRSUP  .GE.  4 )                                            M43. 403
     1 write (*,*) ' distr2, extract from col 55-56  N13 =',  N13       M43. 404
      GO TO 21633                                                       M28.3354
C RECURSIVE CONVOLUTION BRANCH PROCESSING BEGINS HERE for Semlyem (LJG'sM28.3355
 8010 IPUNCH = IABS ( IPSEM )                                                   
 8020 IF (IPUNCH .LE. 50)                                               M28.3361
     1           GO TO 8030                                                     
      KILL = 117                                                        M28.3362
      LSTAT(19) = 8030                                                  M28.3363
      GO TO 9999                                                        M28.3364
 8030 IPS1 = - IPUNCH                                                   M28.3365
      IPS2 = 0                                                          M28.3366
      IPS3 = 0                                                          M28.3367
      N5 = IPSEM                                                        M28.3368
 8040 CONTINUE                                                          M28.3369
      DECODE (80, 8045, ABUFF(1)) D1, D2, D3, N1, N2,  N3,    N4        M28.3370
 8045 FORMAT(26X,3E12.5,4I3)                                                    
      IF (IPSEM .NE. N5) GO TO 8050                                     M28.3372
      IF (N2 .LE. 0 .OR. N2 .GT. IPUNCH)                                M28.3373
     1      GO TO 8050                                                          
      N5 = N1 + (N2 - 1) * IPUNCH                                       M28.3374
      IF (N5 .LE. IPS1) GO TO 8050                                      M28.3375
      IF (N5 .NE. IPS1 + IPUNCH + 1) IPS3 = 1                           M28.3376
      IF (IPS2 .LE. IPUNCH .OR. IABS(MBUS(IBR)) .EQ. 1) GO TO 8060      M28.3377
 8050 KILL = 169                                                        M28.3378
      LSTAT(19) = 8050                                                  M28.3379
      GO TO 9999                                                        M28.3380
 8060 KODEBR(IBR) = IPUNCH                                                      
      N6 = IFX                                                          M28.3382
      N7 = IFX                                                          M28.3383
      IFSEM = IFSEM + 1                                                         
      IF (IFSEM .LE. LFSEM) GO TO 8100                                  M28.3385
 8090 IPRINT = 20                                                       M28.3386
      LSTAT(19) = 8090                                                  M28.3387
      GO TO 9000                                                        M28.3388
 8100 SCONST(IFSEM) = D1                                                        
      CI(IBR) = N3                                                              
      CK(IBR) = N4                                                              
      CIK(IBR) = IADDRS                                                 M32.1223
      CKI(IBR) = N5                                                     M28.3393
      INDHST(IBR) = IFX + 1                                             M28.3394
      IPS1 = N5                                                         M28.3395
      IPS2 = IPS2 + 1                                                   M28.3396
C     MAKE LENGTH(IBR) CONDITIONAL FOR TORONTO.  6 OCT 1981:            M31. 541
      IF ( IPS3  .EQ.  0 )                                              M31. 542
     1 LENGTH(IBR) = IPUNCH                                             M31. 543
      IF ( IPUNCH  .GT.  MXPHAS )                                       M31. 544
     1 MXPHAS = IPUNCH                                                  M31. 545
      BUS5 = TEXT2                                                      M28.3398
      BUS6 = TEXT3                                                      M28.3399
      IF (IPSEM .LT. 0) GO TO 8140                                      M28.3400
      KODSEM(IBR) = IFSEM                                               M28.3401
      IF (NOUTPR .EQ. 0)                                                M28.3402
     1 WRITE (KUNIT6, 8110)  D1, D2, N1, N2                             M35. 748
 8110 FORMAT (  13H+CONVOLUTION.,  2E13.4,  2I4  )                      M28.3404
      IF (N3 .LE. 0) GO TO 8160                                         M28.3405
      N6 = N6 + 1                                                       M28.3406
      IF (N6 .LE. LHIST) GO TO 8130                                     M28.3407
 8120 IPRINT = 22                                                       M28.3408
      LSTAT(19) = 8120                                                  M28.3409
      GO TO 9000                                                        M28.3410
 8130 CNVHST(N6) = D2                                                           
      IF (D2 .GT. DELTAT)                                               M28.3412
     1 GO TO 8160                                                               
      KILL = 197                                                        M28.3413
      LSTAT(19) = 8130                                                  M28.3414
      GO TO 9999                                                        M28.3415
 8140 KODSEM(IBR) = - IFSEM                                             M28.3416
      IF (NOUTPR .EQ. 0)                                                M28.3417
     1     WRITE (KUNIT6, 8150) N1, N2, D1                              M35. 749
 8150 FORMAT(29H+LUMPED ELEMENT CONVOLUTION (,I2,1H,,I2,1H)             M28.3419
     1     ,1X,E10.3)                                                   M28.3420
      IFSEM = IFSEM + 1                                                 M28.3421
      IF (IFSEM .GT. LFSEM) GO TO 8090                                  M28.3422
      IFX = IFX + 1                                                     M28.3423
      IF (IFX .GT. LHIST) GO TO 8120                                    M28.3424
      SCONST(IFSEM) = D2                                                M28.3425
 8160 IF (IPS2 .GT. IPUNCH) GO TO 8190                                  M28.3426
C     READ INPUT CARD USING CIMAGE                                      M28.3427
      CALL CIMAGE                                                       M28.3428
      N8 = N6 + 1                                                       M28.3429
      N6 = N6 + 5                                                       M28.3430
      IF (N6 .GT. LHIST) GO TO 8120                                     M28.3431
      IF (KOLBEG .LE. 0) GO TO 8161                                     M28.3432
      NFRFLD = 5                                                        M28.3433
      CALL FREFLD( CNVHST(N8) )                                         M28.3434
      GO TO 8162                                                        M28.3435
 8161 IF ( MOLDAT .EQ. 1)   GO TO 18161                                 M29.1290
      DECODE (80,18170, ABUFF(1))                                       M29.1291
     1                  (CNVHST(I), I=N8, N6)                                   
      GO TO 8162                                                        M29.1292
18161 CONTINUE                                                          M29.1293
      DECODE (80, 8170, ABUFF(1)) (CNVHST(I), I=N8, N6)                 M29.1294
 8162 D1 = CNVHST(N6)                                                           
18170 FORMAT(2X,5E15.8)                                                 M28.3439
 8170 FORMAT (2X, 6E12.5)                                               M28.3440
      CNVHST(N6) = CNVHST(N6) * TWOPI                                           
      IF (NOUTPR .EQ. 0)                                                M28.3442
     1 WRITE (KUNIT6, 8180)  CNVHST(N8), CNVHST(N8+1),                  M35. 750
     2                       CNVHST(N8+2)                               M28.3444
 8180 FORMAT (   12H+PHASOR Z,Y.,  3E12.4  )                            M28.3445
 8190 IF (N3) 8200, 8270, 8240                                          M28.3446
 8200 N9 = 0                                                            M28.3447
      NUMAKI = 1                                                        M28.3448
 8205 N8 = IFSEM + 1                                                    M28.3449
      N10 = - N3 - N9                                                   M28.3450
      IF (N10 .GT. 3) N10 = 3                                           M28.3451
      IFSEM = IFSEM + N10 + N10                                         M28.3452
      IF (IFSEM .GT. LFSEM) GO TO 8090                                  M28.3453
C     READ INPUT CARD USING CIMAGE                                      M28.3454
      CALL CIMAGE                                                       M28.3455
      IF (KOLBEG .LE. 0) GO TO 8206                                     M28.3456
      NFRFLD = IFSEM - N8 + 1                                           M28.3457
      CALL FREFLD( SCONST(N8) )                                         M28.3458
      GO TO 8208                                                        M28.3459
 8206 CONTINUE                                                          M28.3460
      DECODE (80, 8170, ABUFF(1)) (SCONST(I), I=N8, IFSEM)              M28.3461
 8208 IF (N9 .EQ. 0) GO TO 8207                                         M28.3462
      IF (SCONST(N8) - SCONST(N8 - 2) .LT. DELTAT) GO TO 8210           M28.3463
 8207 IF (N9 .EQ. 0 .AND. SCONST(N8) .LT. DELTAT) GO TO 8210            M28.3464
      IF (N10 .LE. 1) GO TO 8220                                        M28.3465
      IF (SCONST(N8 + 2) - SCONST(N8) .LT. DELTAT) GO TO 8210           M28.3466
      IF (N10 .LE. 2) GO TO 8220                                        M28.3467
      IF (SCONST(N8 + 4) - SCONST(N8 + 2) .GT. DELTAT) GO TO 8220       M28.3468
 8210 KILL = 197                                                        M28.3469
      LSTAT(19) = 8210                                                  M28.3470
      GO TO 9999                                                        M28.3471
 8220 N8 = N9 + 1                                                       M28.3472
      N9 = N9 + N10                                                     M28.3473
      IF (NOUTPR .EQ. 0)                                                M28.3474
     1     WRITE (KUNIT6, 8230) N8, N9                                  M35. 751
 8230 FORMAT(39H+AMETANI PROPAGATION IMPULSE, SEGMENTS ,I3,6H THRU ,I3) M28.3476
      IF (N9 .LT. - N3) GO TO 8205                                      M28.3477
      IF (D2 .LE. 0.0) D2 = TMAX                                        M28.3478
      IF (D2 - SCONST(IFSEM - 1) .LT. DELTAT .AND.                      M28.3479
     1     D2 .GT. SCONST(IFSEM - 1)) GO TO 8210                        M28.3480
      SCONST(IFSEM) = (UNITY - SCONST(IFSEM))/(D2 - SCONST(IFSEM - 1))  M28.3481
      IF (SCONST(IFSEM) .LT. 0.0) SCONST(IFSEM) = 0.0                   M28.3482
      IFX = IFX + 2                                                     M28.3483
      IF (IFX .GT. LHIST) GO TO 8120                                    M28.3484
      GO TO 8270                                                        M28.3485
 8240 N9 = 0                                                            M28.3486
 8250 N8 = IFSEM + 1                                                    M28.3487
      N10 = N3 - N9                                                     M28.3488
      IF (N10 .GT. 2) N10 = 2                                           M28.3489
      N11 = 3 * N10                                                     M28.3490
      IFSEM = IFSEM + 5 * N10                                           M28.3491
      IF (IFSEM .GT. LFSEM) GO TO 8090                                  M28.3492
C     READ INPUT CARD USING CIMAGE                                      M28.3493
      CALL CIMAGE                                                       M28.3494
      IF (KOLBEG .LE. 0) GO TO 8251                                     M28.3495
      NFRFLD = N11                                                      M28.3496
      CALL FREFLD( VOLTBC(1) )                                          M31. 546
      GO TO 8252                                                        M28.3498
 8251 CONTINUE                                                          M28.3499
      DECODE (80, 8170, ABUFF(1)) (VOLTBC(I), I=1, N11)                 M31. 547
 8252 N11 = 1                                                           M28.3501
      DO 8254 I=N8, IFSEM, 5                                            M28.3502
      SCONST(I + 0) = VOLTBC(N11 + 0)                                   M31. 548
      SCONST(I + 1) = VOLTBC(N11 + 1)                                   M31. 549
      SCONST(I + 2) = VOLTBC(N11 + 2)                                   M31. 550
      N11 = N11 + 3                                                     M28.3506
      IF (SCONST(I) .GE. 0.0) GO TO 8254                                M28.3507
 8254 CONTINUE                                                          M28.3508
      N8 = N9 + 1                                                               
      N9 = N9 + N10                                                             
      IF (NOUTPR .EQ. 0)                                                M28.3511
     1 WRITE (KUNIT6, 8260) N8, N9, VOLTBC(1), VOLTBC(2)                M35. 752
 8260 FORMAT ( 17H+PROPAGATION EXP.,  I3,  1H-,  I2,  2H. ,             M28.3513
     1         2E12.3  )                                                M28.3514
      IF (N9 .LT. N3)                                                   M28.3515
     1      GO TO 8250                                                          
      IFX = IFX + N3 + N3                                               M28.3516
      IF (IFX .GT. LHIST) GO TO 8120                                    M28.3517
 8270 IF (N4) 8280, 8340, 8310                                          M28.3518
 8280 N9 = 0                                                            M28.3519
 8290 N8 = IFSEM + 1                                                    M28.3520
      N10 = - N4 - N9                                                   M28.3521
      IF (N10 .GT. 3) N10 = 3                                           M28.3522
      IFSEM = IFSEM + N10 + N10                                         M28.3523
      IF (IFSEM .GT. LFSEM) GO TO 8090                                  M28.3524
C     READ INPUT CARD USING CIMAGE                                      M28.3525
      CALL CIMAGE                                                       M28.3526
      IF (KOLBEG .LE. 0) GO TO 8291                                     M28.3527
      NFRFLD = IFSEM - N8 + 1                                           M28.3528
      CALL FREFLD ( SCONST(N8) )                                        M28.3529
      GO TO 8293                                                        M28.3530
 8291 CONTINUE                                                          M28.3531
      DECODE (80, 8170, ABUFF(1)) (SCONST(I), I=N8, IFSEM)              M28.3532
 8293 IF (N9 .EQ. 0) GO TO 8292                                         M28.3533
      IF (SCONST(N8) - SCONST(N8 - 2) .LT. DELTAT) GO TO 8210           M28.3534
 8292 IF (N9 .EQ. 0 .AND. SCONST(N8) .NE. 0.0) GO TO 8210               M28.3535
      IF (N10 .LE. 1) GO TO 8294                                        M28.3536
      IF (SCONST(N8 + 2) - SCONST(N8) .LT. DELTAT) GO TO 8210           M28.3537
      IF (N10 .LE. 2) GO TO 8294                                        M28.3538
      IF (SCONST(N8 + 4) - SCONST(N8 + 2) .LT. DELTAT) GO TO 8210       M28.3539
 8294 N8 = N9 + 1                                                       M28.3540
      N9 = N9 + N10                                                     M28.3541
      IF (NOUTPR .EQ. 0)                                                M28.3542
     1     WRITE (KUNIT6, 8300) N8, N9                                  M35. 753
 8300 FORMAT(37H+AMETANI ADMITTANCE IMPULSE, SEGMENT ,I3,6H THRU ,I3)   M33. 204
      IF (N9 .LT. -N4) GO TO 8290                                       M28.3545
      IF (D3 .LE. 0.0) D3 = TMAX                                        M28.3546
      IF (D3 - SCONST(IFSEM - 1) .LT. DELTAT .AND.                      M28.3547
     1     D3 .GT. SCONST(IFSEM - 1)) GO TO 8210                        M28.3548
      SCONST(IFSEM) = SCONST(IFSEM) / (D3 - SCONST(IFSEM - 1))          M28.3549
      IF (D3 .LT. SCONST(IFSEM - 1)) SCONST(IFSEM) = 0.0                M28.3550
      IFX = IFX + 1                                                     M28.3551
      IF (IPSEM .GT. 0) IFX = IFX + 1                                   M28.3552
      IF (IFX .GT. LHIST) GO TO 8120                                    M28.3553
      GO TO 8340                                                        M28.3554
 8310 N9 = 0                                                            M28.3555
 8320 N8 = IFSEM + 1                                                            
      N10 = N4 - N9                                                     M28.3557
      IF (N10 .GT. 2) N10 = 2                                           M28.3558
      N11 = 3 * N10                                                     M28.3559
      IFSEM = IFSEM + 4 * N10                                           M28.3560
      IF (IFSEM .GT. LFSEM) GO TO 8090                                  M28.3561
C     READ INPUT CARD USING CIMAGE                                      M28.3562
      CALL CIMAGE                                                       M28.3563
      IF (KOLBEG .LE. 0) GO TO 8321                                     M28.3564
      NFRFLD = N11                                                      M28.3565
      CALL FREFLD( VOLTBC(1) )                                          M31. 552
      GO TO 8322                                                        M28.3567
 8321 CONTINUE                                                          M28.3568
      DECODE (80, 8170, ABUFF(1)) (VOLTBC(I), I=1, N11)                 M31. 553
 8322 N11 = 1                                                           M28.3570
      DO 8324 I=N8, IFSEM, 4                                            M28.3571
      SCONST(I + 0) = VOLTBC(N11 + 0)                                   M31. 554
      SCONST(I + 1) = VOLTBC(N11 + 1)                                   M31. 555
      SCONST(I + 2) = VOLTBC(N11 + 2)                                   M31. 556
      N11 = N11 + 3                                                     M28.3575
      IF (SCONST(I) .GE. 0.0) GO TO 8324                                M28.3576
 8324 CONTINUE                                                          M28.3577
      N8 = N9 + 1                                                       M28.3578
      N9 = N9 + N10                                                     M28.3579
      IF (NOUTPR .EQ. 0)                                                M28.3580
     1 WRITE (KUNIT6, 8330) N8, N9, VOLTBC(1), VOLTBC(2)                M35. 754
 8330 FORMAT (  17H+ADMITTANCE  EXP.,  I3,  1H-,I2,  2H. ,              M28.3582
     1          2E12.3  )                                               M28.3583
      IF (N9 .LT. N4) GO TO 8320                                        M28.3584
      IFX = IFX + N4                                                    M28.3585
      IF (IPSEM .GT. 0) IFX = IFX + N4                                  M28.3586
      IF (IFX .GT. LHIST) GO TO 8120                                    M28.3587
 8340 IF (N6 .GT. IFX) IFX = N6                                         M28.3588
      NR(IBR) = IFX - N7                                                        
      ICHECK = 5                                                        M28.3590
      NRECUR = NRECUR + 1                                                       
      IF ( NRECUR .EQ. IPUNCH )                                         M28.3592
     1 GO TO 8351                                                               
      GO TO 100                                                                 
 8350 CONTINUE                                                          M28.3594
      IF (IABS(MBUS(IBR)) .EQ. NTOT) NTOT = NTOT - 1                    M28.3595
      IF (IABS(KBUS(IBR)) .EQ. NTOT) NTOT = NTOT - 1                    M28.3596
      IBR = IBR - 1                                                     M28.3597
 8351 CKI(IBR) = - CKI(IBR)                                                     
      IF (IPS3 .EQ. 0) GO TO 8357                                       M28.3599
      N1 = - IADD - 1                                                   M32.1224
      N2 = IBR - IPS2 + 1                                               M28.3601
      DO 8353 I=N2, IBR                                                 M28.3602
      CIK(I) = N1                                                       M28.3603
 8353 CONTINUE                                                          M28.3604
      BUS5 = TEXT4                                                      M28.3605
      BUS6 = TEXT5                                                      M28.3606
 8357 N1 = IADDRS - 1                                                   M32.1225
      N3 = IPUNCH * IPUNCH                                              M28.3608
      IADD = IADDRS + N3                                                M32.1226
     1     + N3 - 1                                                             
      IF (IADD .LE. LFD) GO TO 8360                                     M32.1227
      IPRINT = 21                                                       M28.3611
      LSTAT(19) = 8360                                                  M28.3612
      GO TO 9000                                                        M28.3613
 8360 N2 = N1 + N3                                                              
      N4 = 1                                                            M28.3615
      N5 = 1                                                            M28.3616
      N8 = IPUNCH * 2                                                   M28.3617
C                   !total # of elements in matrix for thE coupled groupM43. 412
      IROW = 0                                                          M28.3618
C     READ INPUT CARD USING CIMAGE                                      M28.3619
 8370 CALL CIMAGE                                                       M28.3620
 8375 CONTINUE                                                          M28.3621
      IF (KOLBEG .LE. 0) GO TO 8376                                     M28.3622
18375 IROW = IROW + 1                                                   M28.3623
      N9OLD = (IROW - 1 ) * 6                                           M28.3624
      N9 = IROW * 6                                                     M28.3625
      IF ( N8 .LT. N9 )   GO TO 38376                                   M28.3626
      NFRFLD = 6                                                        M28.3627
      GO TO 18377                                                       M28.3628
38376 NFRFLD = N8 - N9OLD                                               M28.3629
      IF (NFRFLD .GT. 0 ) GO TO 18377                                   M28.3630
      IROW = 0                                                          M28.3631
      GO TO 18375                                                       M28.3632
18377 CALL FREFLD( VOLTBC(1) )                                          M31. 558
      GO TO 8377                                                        M28.3634
 8376 CONTINUE                                                          M28.3635
      DECODE (80,18376, ABUFF(1)) (VOLTBC(I), I=1, 6)                   M31. 559
18376 FORMAT (2X, 6E12.5)                                               M28.3637
 8377 IF (NOUTPR .EQ. 0)                                                M28.3638
     1 WRITE (KUNIT6, 8380) N5, (VOLTBC(I), I=1, 3)                     M35. 755
 8380 FORMAT (  9H+[TV] ROW,  I3,  1H.,  3E12.3  )                      M28.3640
      DO 8390 I=1, 5, 2                                                 M28.3641
      N6 = N4 + N1                                                      M28.3642
      SFD(N6) = VOLTBC(I)                                               M31. 561
      IF ( NUMAKI .EQ. 0 ) GO TO 8381                                   M28.3644
      N4 = N4 + IPUNCH                                                  M28.3645
      IF (N4 .GT. N3 ) GO TO 8400                                       M28.3646
      SFD(N4) = VOLTBC(I+1)                                             M31. 562
      GO TO 8382                                                        M28.3648
 8381 N6 = N4 + N2                                                      M28.3649
      SFD(N6) = VOLTBC(I+1)                                             M31. 563
 8382 N4 = N4 + IPUNCH                                                  M28.3651
      IF (N4 .GT.N3) GO TO 8400                                         M28.3652
 8390 CONTINUE                                                          M28.3653
      GO TO 8370                                                        M28.3654
 8400 N5 = N5 + 1                                                       M28.3655
      N4 = N5                                                           M28.3656
      IF (N5 .LE. IPUNCH) GO TO 8370                                    M28.3657
      N4 = 1                                                            M28.3658
      N5 = 1                                                            M28.3659
      IROW = 0                                                          M28.3660
C     READ INPUT CARD USING CIMAGE                                      M28.3661
 8410 CALL CIMAGE                                                       M28.3662
      IF (KOLBEG .LE. 0) GO TO 8415                                     M28.3663
28375 IROW = IROW + 1                                                   M28.3664
      N9OLD = (IROW - 1 ) * 6                                           M28.3665
      N9 = IROW * 6                                                     M28.3666
      IF ( N8 .LT. N9 )   GO TO 28376                                   M28.3667
      NFRFLD = 6                                                        M28.3668
      GO TO 28377                                                       M28.3669
28376 NFRFLD = N8 - N9OLD                                               M28.3670
      IF (NFRFLD .GT. 0 ) GO TO 28377                                   M28.3671
      IROW = 0                                                          M28.3672
      GO TO 28375                                                       M28.3673
28377 CALL FREFLD( VOLTBC(1) )                                          M31. 564
      GO TO 8417                                                        M28.3675
 8415 CONTINUE                                                          M28.3676
      DECODE (80, 18376, ABUFF(1)) (VOLTBC(I), I=1, 6)                  M31. 565
 8417 IF (NOUTPR .EQ. 0)                                                M28.3678
     1 WRITE (KUNIT6, 8420) N5, (VOLTBC(I), I=1, 3)                     M35. 756
 8420 FORMAT (  9H+[TI] ROW,  I3,  1H.,  3E12.3  )                      M28.3680
      DO 8430 I=1, 5, 2                                                 M28.3681
      N6 = N4 + N1                                                      M28.3682
      QFD(N6) = VOLTBC(I)                                               M31. 567
      IF ( NUMAKI .EQ. 0 ) GO TO 8421                                   M28.3684
      N4 = N4 + IPUNCH                                                  M28.3685
      IF (N4 .GT. N3 ) GO TO 8440                                       M28.3686
      QFD(N4) = VOLTBC(I+1)                                             M31. 568
      GO TO 8422                                                        M28.3688
 8421 N6 = N4 + N2                                                      M28.3689
      QFD(N6) = VOLTBC(I+1)                                             M31. 569
 8422 N4 = N4 + IPUNCH                                                  M28.3691
      IF (N4 .GT. N3) GO TO 8440                                        M28.3692
 8430 CONTINUE                                                          M28.3693
      GO TO 8410                                                        M28.3694
 8440 N5 = N5 + 1                                                       M28.3695
      N4 = N5                                                           M28.3696
      IF (N5 .LE. IPUNCH) GO TO 8410                                    M28.3697
      ICHECK = 1                                                        M28.3698
      NRECUR = 0                                                        M28.3699
      IADDRS = IADD + 1                                                 M32.1228
      GO TO 100                                                         M28.3700
20000 KBUS(IBR) = N1                                                    M28.3701
      MBUS(IBR) = N2                                                    M28.3702
      DECODE (80, 191, ABUFF(1)) N5                                     M28.3703
      IF (N5 .EQ. 0) GO TO 8350                                         M28.3704
      GO TO 8040                                                        M28.3705
C     END OF RECURSIVE CONVOLUTION BRANCH INPUT ************************M28.3706





21633 CONTINUE                                                                  
      IF (IPSEM .NE. 0) IMODEL(IBR) = -4                                M43. 413
      IF (IPSEM .EQ. 0) GO TO 1923                                              
      DECODE (80, 8045, ABUFF(1)) D1, D2, D3, NT1, NT2,  N3,    N4      M44.  25
 1923 IF (IPSEM .EQ. 0) IMODEL(IBR)=IPUNCH                                      
      IF(IMODEL(IBR) .NE. -4 ) GO TO 76893                              M43. 415
C     1 .AND.IMODEL(IBR).NE.-2)GO TO 76893    !If not Marti model bapassM44.  26
      PDT0=AA                                                           M43. 417
      IF(PDT0.GE.1..OR.PDT0.EQ.0.)GO TO 6100                            M43. 418
      PDT0=1.                                                           M43. 419
      WRITE(LUNIT6,1006)                                                M43. 420
 1006 FORMAT(//' A TOO LOW VALUE OF P*DELTAT WAS READ. P*DELTAT CHANGED M43. 421
     1 TO 1.'//)                                                        M43. 422
 6100 PDT=PDT0                                                          M43. 423
 9276 IFX = IFX + 1                                                             
      IF ( IFX .GT. LHIST ) GO TO 8120                                          
      IFSEM = IFSEM + 1                                                         
      IF ( IFSEM .GT. LFSEM ) GO TO 8090                                        
      IF ( IPSEM .NE. 0) ITYPE = ITYPE - 1                              M44.  27
      IF (IPSEM .EQ. 0) GO TO 1989                                              
      IF ( NOUTPR  .EQ.  0 )                                            M44.  29
     1 WRITE (KUNIT6, 1990)  NT1                                        M44.  30
 1990 FORMAT ( 16H Semlyen's  MODE,  I3,                                M44.  31
     1         18H  parameters begin   )                                M44.  32
      CKI(IBR) = N4                                                             
      NPZ = N4                                                          M44.  34
      SCONST(IFSEM) = D1                                                        
      KODSEM(IBR) = IFSEM                                                       
      INDHST(IBR) = IFX                                                         
      go to 1988                                                        M44.  38
 1989 N11 = -ITYPE                                                              
      IF ( NOUTPR  .EQ.  0 )                                            M43. 431
     1 WRITE (KUNIT6, 6729)  N11                                        M43. 432
 6729 FORMAT ( 11H Cable MODE,  I3,                                     M43. 433
     1         18H  parameters begin   )                                M43. 434
      AA = NOUTPR                                                       M43. 435
      IF ( H1  .EQ.  2.0 )   NOUTPR = 1                                 M43. 436
C     READ INPUT CARD USING CIMAGE                                      M43. 437
      CALL CIMAGE                                                       M43. 438
      DECODE (80, 6733, ABUFF(1))  NPZ, AK0ZC                                   
 6733 FORMAT ( I8, E32.20 )                                                     
      CKI(IBR) = NPZ                                                            
      SCONST(IFSEM) = AK0ZC                                                     
      KODSEM(IBR) = IFSEM                                                       
      INDHST(IBR) = IFX                                                         
      IF (NOUTPR .NE. 0) NPLL = NOUTPR                                  M44.  39
      IF ( NOUTPR  .EQ.  0 )                                            M43. 445
     1 WRITE (KUNIT6, 6735)  NPZ, AK0ZC                                 M43. 446
 6735 FORMAT ( 33H+Yc BEGINS. ORDER, Yc(INFINITY) =,                    M43. 447
     1         I5,  E11.3  )                                            M43. 448
      IF ( H1  .EQ.  1.0 )   NOUTPR = 1                                 M43. 449
      N11 = 1                                                                   
 6738 N12 = N11 + 2                                                             
      IF ( N12  .GT.  NPZ )   N12 = NPZ                                 M43. 452
C     READ INPUT CARD USING CIMAGE                                      M43. 453
      CALL CIMAGE                                                       M43. 454
      DECODE (80, 6741, ABUFF(1))  ( VOLTBC(II), II=N11, N12)           M43. 455
 6741 FORMAT ( 3E26.0 )                                                 M43. 456
      IF ( NOUTPR  .EQ.  0 )                                                    
     1 WRITE (KUNIT6, 6744) N11, N12, (VOLTBC(II), II=N11,N12 )         M43. 458
 6744 FORMAT ( 10H+RESIDUALS, I3, 1H-, I2, 1H.,  3E11.3 )               M43. 459
      NN11 = N11 + IFSEM                                                        
      NN12 = N12 + IFSEM                                                        
      I = N11                                                                   
      DO 72644  JJ = NN11, NN12                                                 
      SCONST(JJ) = VOLTBC(I)                                                    
      I = I + 1                                                                 
72644 CONTINUE                                                          M43. 466
      N11 = N11 + 3                                                             
      IF ( N11  .LE.  NPZ )  GO TO 6738                                         
      NYCMP = 0                                                                 
      N11 = 1                                                                   
 6753 N12 = N11 + 2                                                     M43. 470
      IF ( N12  .GT.  NPZ )   N12 = NPZ                                         
C     READ INPUT CARD USING CIMAGE                                      M31. 619
      CALL CIMAGE                                                       M43. 472
      DECODE (80, 6741, ABUFF)  ( VOLTBC(II), II=N11, N12 )                     
      IF ( NOUTPR  .EQ.  0 )                                            M43. 474
     1 WRITE (LUNIT6, 6755) N11, N12, (VOLTBC(II), II=N11, N12)         M43. 475
 6755 FORMAT (   6H+POLES, I3, 1H-, I2, 1H.,  3E12.4  )                 M43. 476
      NN11 = NN12 + N11                                                         
      NN13 = NN12 + N12                                                 M43. 478
      I = N11                                                                   
      DO 72655 JJ = NN11, NN13                                                  
      SCONST(JJ) = VOLTBC(I)                                            M43. 481
      IF (SCONST(JJ) .GE. 1.E+14                                        M44.  41
     1      .OR. SCONST(JJ) .LE. -1.E+14) NYCMP = NYCMP + 1             M44.  42
      I = I + 1                                                         M43. 482
72655 CONTINUE                                                          M43. 483
      N11 = N11 + 3                                                     M43. 484
      IF ( N11  .LE.  NPZ )   GO TO 6753                                        
C     GO TO 3567                                                        M44.  43
 3567 NYCMP = NYCMP * 3                                                 M44.  45
      NK1 = IFX + 19 + 1                                                M44.  46
      CNVHST(NK1) = NYCMP                                               M44.  47
      IF ( H1  .EQ.  1.0 )   NOUTPR = AA                                M43. 488
      IF(PDT0.EQ.0.)GO TO 45000                                         M43. 489
      NPZA=NPZ                                                          M43. 490
      II=0                                                              M43. 491
40021 N11=IFSEM+NPZA+II+1                                               M43. 492
      N12=N11+NPZA-1                                                    M43. 493
      XSUM=0.                                                           M43. 494
      YSUM=0.                                                           M43. 495
      DO 40000 JJ=N11,N12                                               M43. 496
      IF(SCONST(JJ)*DELTAT.LT.PDT)GO TO 40000                           M43. 497
      GO TO 40001                                                       M43. 498
40000 CONTINUE                                                          M43. 499
      GO TO 40007                                                       M43. 500
40001 IF (JJ.EQ.N11)JJ=JJ+1                                             M43. 501
      IF(JJ.EQ.N12)GO TO 40007                                          M43. 502
      DO 40002 I=JJ,N12                                                 M43. 503
      NN11=I-NPZA                                                       M43. 504
      XSUM=XSUM+SCONST(NN11)/SCONST(I)                                  M43. 505
40002 YSUM=YSUM+SCONST(NN11)/SCONST(I)**2                               M43. 506
      YSUM=XSUM/YSUM                                                    M43. 507
      IF(YSUM*DELTAT.GE.PDT)GO TO 40030                                 M43. 508
      PDT=PDT+1.                                                        M43. 509
      GO TO 40021                                                       M43. 510
40030 NN11=JJ-NPZA                                                      M43. 511
      IF(II.NE.0)GO TO 40003                                            M43. 512
      PDT=PDT0                                                          M43. 513
      NPZ=NPZ-N12+JJ                                                    M43. 514
      CKI(IBR)=NPZ                                                      M43. 515
      GO TO 40004                                                       M43. 516
40003 NPA=NPA-N12+JJ                                                    M43. 517
      CKKJM(IBR)=NPA                                                    M43. 518
40004 SCONST(NN11)=YSUM*XSUM                                            M43. 519
      SCONST(JJ)=YSUM                                                   M43. 520
      I=N12-JJ+1                                                        M43. 521
      IF(II.EQ.0 .AND. NOUTPR .EQ. 0)                                   M43. 522
     1 WRITE(LUNIT6,40005)I,SCONST(NN11),SCONST(JJ)                     M43. 523
40005 FORMAT ( ' Z0 EQUIVALENCE.  I, R*P, P =', I5, 2E20.13 )           M43. 524
      IF(II.NE.0  .AND. NOUTPR .EQ. 0  )                                M43. 525
     1 WRITE(LUNIT6,40006)I,SCONST(NN11),SCONST(JJ)                     M43. 526
40006 FORMAT ( ' A1 EQUIVALENCE.  I, R*P, P =', I5, 2E20.13 )           M43. 527
C      IF(PDT.NE.PDT0)                                                  M43. 528
C     1 WRITE(LUNIT6,40031)PDT                                          M43. 529
C40031 FORMAT(//' *** NOTE - PDT CHANGED TO *** ',F7.2//)               M43. 530
      N12=N12-I+1                                                       M43. 531
      JJ=NN11                                                           M43. 532
      DO 40008 I=N11,N12                                                M43. 533
      JJ=JJ+1                                                           M43. 534
40008 SCONST(JJ)=SCONST(I)                                              M43. 535
      NN13=JJ                                                           M43. 536
CCCC     POINTER FOR NO. OF COMPLEX POLES MUST BE RESET AFTER POLE 
CCCC     REDUCTION, THL.  MARCH 20, 1990
CCCC     40007 IF(II.NE.0)GO TO 40022 
40007 IF ( II .EQ. 0 )  GO TO 3179
      NTEMP = SCONST(NK1)     ! SAVE NO. OF COMPLEX POLES 
      NK1 = IFSEM + 5 * ( NPA + NPZ + 1 )   ! DEFINE THE NEW POINTER
      SCONST(NK1 ) = NTEMP  ! STORE THE NUMBER IN THE NEW CELL
      GO TO 40022
CCCC  END OF THL'S CORRECTION, MARCH 20, 1990    
C     READ INPUT CARD USING CIMAGE                                      M31. 635
 3179 IF (IPSEM .EQ. 0) GO TO 45000                                    
 1988 TAUO = D2                                                         M44.  49
      NPA = N3                                                          M44.  50
      GO TO 1992                                                        M44.  51
45000 CALL CIMAGE                                                       M43. 540
C 3456789012345678901234567890123456789012345678901234567890123456789012M43. 541
      DECODE (80, 6733, ABUFF(1)) NPA,TAUO                              M43. 542
 1992 IF (TAUO .GT. DELTAT) GO TO 6756                                          
      KILL = 197                                                        M43. 544
      FLSTAT(14) = TAUO                                                         
      LSTAT(19) = 6756                                                  M43. 546
      GO TO 9999                                                        M43. 547
 6756 CKKJM(IBR) = NPA                                                          
      IF ( IPSEM .NE. 0 ) GO TO 2016                                    M44.  52
      IF ( NOUTPR  .EQ.  0 )                                            M43. 549
     1 WRITE (LUNIT6, 6759)  NPA, TAUO                                  M43. 550
 6759 FORMAT ( 29H+A1 BEGINS. ORDER, TAU(SEC) =,  I6, E14.5  )          M43. 551
      IF ( H1  .EQ.  1.0 )   NOUTPR = 1                                 M43. 552
 2016 IF ( IPSEM .EQ. 0 ) GO TO 2002                                    M43. 553
      CALL CIMAGE                                                       M44.  53
      N8 = IFX + 15                                                     M44.  54
      N6 = IFX + 19                                                     M44.  55
      DECODE (80,18170, ABUFF(1))                                       M44.  56
     1           (CNVHST(I), I=N8, N6)                                          
      IF ( NOUTPR  .EQ.  0 )                                                    
     1 WRITE (KUNIT6, 2019)  (CNVHST(I), I=N8, N6)                      M44.  59
 2019 FORMAT ( 12H Phasor Z-Y.  3E12.3 )                                M44.  60
      N11 = NPZ * 2 + IFSEM + 1                                         M44.  61
      NT13 = N11 + NPA                                                  M44.  62
      NN17 = 0                                                          M44.  63
      DO 1996 J = 1, NPA/2                                              M44.  64
      CALL CIMAGE                                                       M44.  65
      NFRFLD = 6                                                        M44.  66
      IF (KOLBEG .LE. 0) GO TO 1993                                     M44.  67
      CALL FREFLD( VOLTBC(1) )                                          M44.  68
      GO TO 1994                                                        M44.  69
 1993 CONTINUE                                                          M44.  70
      DECODE (80,8170, ABUFF(1))                                        M44.  71
     1           (VOLTBC(I), I=1, NFRFLD)                               M44.  72
 1994 N12 = 1                                                           M44.  73
      IF ( NOUTPR  .EQ.  0 )                                                    
     1 WRITE (KUNIT6, 2017)  (VOLTBC(II), II=1,NFRFLD )                 M44.  75
 2017 FORMAT ( 17H Propagation Exp.  2E12.3 )                           M44.  76
      DO 1995 I=1, 6, 3                                                 M44.  77
      IF ( VOLTBC(N12) .EQ. -1 ) GO TO 2022                             M44.  78
      IF ( VOLTBC(N12) .NE. 1) GO TO  2021                              M44.  79
      NN17 = NN17 + 1                                                   M44.  80
      AKR = VOLTBC(N12+2)*VOLTBC(N12+1)                                 M44.  81
     1    - VOLTBC(N12+5)*VOLTBC(N12+4)                                 M44.  82
      AKR = VOLTBC(N12+2)                                               M44.  83
      AKR = AKR * 1.E+15                                                M44.  84
      AKI = VOLTBC(N12+2)*VOLTBC(N12+4)                                 M44.  85
     1    + VOLTBC(N12+5)*VOLTBC(N12+1)                                 M44.  86
      AKI = VOLTBC(N12+5)                                               M44.  87
      AKI = AKI * 1.E+15                                                M44.  88
      VOLTBC(N12+2) = AKR                                               M44.  89
      VOLTBC(N12+5) = AKI                                               M44.  90
      GO TO 2022                                                        M44.  91
 2021 DDDD = VOLTBC(N12 + 1) * VOLTBC(N12 + 2)                          M44.  92
      SCONST(N11) = DDDD                                                M44.  93
      IF ( VOLTBC(N12+1) .EQ. 0 ) VOLTBC(N12+1) = 1.                    M44.  94
      SCONST(NT13) = VOLTBC(N12 + 1)                                    M44.  95
      GO TO 2023                                                        M44.  96
 2022 SCONST(N11) = VOLTBC(N12 + 2)                                     M44.  97
      SCONST(NT13) = VOLTBC(N12 + 1) * 1.E+15                           M44.  98
 2023 N12 = N12 + 3                                                     M44.  99
      N11 = N11 + 1                                                     M44. 100
      NT13 = NT13 + 1                                                   M44. 101
 1995 CONTINUE                                                          M44. 102
 1996 CONTINUE                                                          M44. 103
      N11 =  IFSEM + 1                                                  M44. 105
      N13 = N11 + NPZ                                                   M44. 106
      DO 1997 J = 1, NPZ/2                                              M44. 107
      CALL CIMAGE                                                       M44. 108
      NFRFLD = 6                                                        M44. 109
      IF (KOLBEG .LE. 0) GO TO 1998                                     M44. 110
      CALL FREFLD( VOLTBC(1) )                                          M44. 111
      GO TO 1999                                                        M44. 112
 1998 CONTINUE                                                          M44. 113
      DECODE (80,8170, ABUFF(1))                                        M44. 114
     1           (VOLTBC(I), I=1, NFRFLD)                               M44. 115
 1999 N12 = 1                                                           M44. 116
      IF ( NOUTPR  .EQ.  0 )                                                    
     1 WRITE (KUNIT6, 2018)  (VOLTBC(II), II=1,NFRFLD )                 M44. 118
 2018 FORMAT ( 17H Char. Admt. Exp.  2E12.3 )                           M44. 119
      DO 2000 I=1, 6, 3                                                 M44. 120
      DDDD = VOLTBC(N12 + 1) * VOLTBC(N12 + 2)                          M44. 121
      SCONST(N11) = DDDD                                                M44. 122
      IF ( VOLTBC(N12+1) .EQ. 0 ) VOLTBC(N12+1) = 1.                    M44. 123
      SCONST(N13) = VOLTBC(N12 + 1)                                     M44. 124
      N12 = N12 + 3                                                     M44. 125
      N11 = N11 + 1                                                     M44. 126
      N13 = N13 + 1                                                     M44. 127
 2000 CONTINUE                                                          M44. 128
 1997 CONTINUE                                                          M44. 129
      NN17 = NN17 * 2                                                   M44. 130
      GO TO 2001                                                        M44. 131
 2002 N11 = 1                                                           M43. 554
 6764 N12 = N11 + 2                                                     M43. 555
      IF ( N12  .GT.  NPA )   N12 = NPA                                 M43. 556
C     READ INPUT CARD USING CIMAGE                                      M31. 646
      CALL CIMAGE                                                       M43. 557
      DECODE (80, 6741, ABUFF(1))  ( VOLTBC(II), II=N11, N12)           M43. 558
      IF ( NOUTPR  .EQ.  0 )                                            M43. 559
     1 WRITE (KUNIT6, 6744) N11, N12, (VOLTBC(II), II=N11,N12 )         M43. 560
      NN11 = NN13 + N11                                                 M43. 561
      NN12 = NN13 + N12                                                 M43. 562
      I = N11                                                           M43. 563
      DO 73644 JJ = NN11, NN12                                          M43. 564
      SCONST(JJ) = VOLTBC(I)                                            M43. 565
      I = I + 1                                                         M43. 566
73644 CONTINUE                                                          M43. 567
      N11 = N11 + 3                                                     M43. 568
      IF ( N11  .LE.  NPA )   GO TO 6764                                M43. 569
      NN17 = 0                                                          M43. 570
      N11 = 1                                                           M43. 571
 6772 N12 = N11 + 2                                                     M43. 572
      IF ( N12  .GT.  NPA )   N12 = NPA                                 M43. 573
C     READ INPUT CARD USING CIMAGE                                      M31. 663
      CALL CIMAGE                                                       M43. 574
      DECODE (80, 6741, ABUFF)  ( VOLTBC(II), II=N11, N12 )             M43. 575
      IF ( NOUTPR  .EQ.  0 )                                            M43. 576
     1 WRITE (LUNIT6, 6755) N11, N12, (VOLTBC(II), II=N11, N12)         M43. 577
      NN11 = NN12 + N11                                                 M43. 578
      NN13 = NN12 + N12                                                 M43. 579
      I = N11                                                           M43. 580
      DO 73655 JJ = NN11, NN13                                          M43. 581
      SCONST(JJ) = VOLTBC(I)                                            M43. 582
      IF (SCONST(JJ) .GE. 1.E+14                                        M44. 133
     1      .OR. SCONST(JJ) .LE. -1.E+14) NN17 = NN17 + 1               M44. 134
      I = I + 1                                                         M43. 583
73655 CONTINUE                                                          M43. 584
      N11 = N11 + 3                                                     M43. 585
      IF ( N11  .LE.  NPA )   GO TO 6772                                M43. 586
 2001 NN17 = NN17 * 3                                                   M44. 135
      NK1 = IFSEM + 5*NPA + 5*NPZ + 4 + 1                               M44. 136
      SCONST(NK1) = NN17                                                M44. 137
C      IF ( IMODEL(IBR) .EQ. -4 )    GO TO 40022                        M44. 138
      IF(PDT.EQ.0.)GO TO 40022                                          M43. 588
      II=2*NPZ                                                          M43. 589
      NPZA=NPA                                                          M43. 590
      GO TO 40021                                                       M43. 591
40022 NOUTPR = AA                                                       M43. 592
      CNVHST(IFX) = TAUO                                                M43. 593
C                                                                       M43. 594
      INOFF5 = 5 * LBRNCH                                                       
      INFDLI(INOFF5+IBR)=CNVHST(IFX)/DELTAT                             M43. 596
     1                               +1                                         
C                                                                       M43. 598
      IFX = IFX + 19 + 1 + NYCMP                                                
      IF ( IFX .GT. LHIST )  GO TO 8120                                 M43. 600
      IFSEM = IFSEM + 11 * NPA + NN17                                   M31. 681
     1                   + 7 * NPZ + 9                                          
      IF ( KCOUNT .EQ. MODEL .OR.                                       M43. 602
     1      NT1 .EQ. IPSEM ) IFQ = IFSEM                                        
      IF ( IFSEM .GT. LFSEM)  GO TO 8090                                M43. 604
      IF ( IPRSUP  .GE.  1 )                                            M43. 605
     1 WRITE (LUNIT6, 4834)  IFX, NPZ, NPA, IBR, IFSEM                  M43. 606
 4834 FORMAT ( 46H DONE MARTI CARDS.  IFX, NPZ, NPA, IBR,IFSEM =, 5I6 ) M43. 607
21105 ICHECK = 3                                                        M43. 608
      LENGTH(IBR1)=KCOUNT                                                       
      IF ( KCOUNT  .GT.  MXPHAS )                                       M43. 610
     1 MXPHAS = KCOUNT                                                  M43. 611
      IF ( IPRSUP  .GE.  1 )                                            M43. 612
     1 WRITE (LUNIT6, 6178) ITYPE, N13, MODEL, IPUNCH                   M43. 613
 6178 FORMAT ( 41H BELOW 21105. ITYPE, N13, MODEL, IPUNCH =,4I6)        M43. 614
C     IF ( IPUNCH  .NE.  -4                                             M43. 615
C    1    .AND. IPSEM .EQ. 0)   GO TO 100          !flag for jose's caseM44. 139
      IF ( IPSEM .NE.0 .AND. NT1 .EQ. IPSEM)  GO TO 50050               M44. 140
      IF ( KCOUNT .EQ. MODEL )  GO TO 50050                                     
      IF ( KCOUNT .NE. 1 )      GO TO 100                                       
      IF ( N13     .LT.  1 )    GO TO 100         
      MODEL = N13                                                       M43. 619
      IF (IPSEM .NE. 0) MODEL = IPSEM                                   M44. 141
      LITYPE(IBR) = IADDRS                                                      
      IF ( IPRSUP  .GE.  1 )                                            M43. 621
     1 WRITE (LUNIT6, 62078) MODEL, IADDRS, IBR1                        M43. 622
62078 FORMAT (59H   Cable MODEL FOR UNTRANSPOSED LINE, MODEL, IADDRS, IBM43. 623
     1R1 =, 3I10 )                                                      M43. 624
      IF ( N13 .EQ. 1 )  GO TO 50050
      GO TO 100                                                         M43. 625
C                                                                       M43. 626
C                                                                       M43. 627





76893 IMODEL(IBR)=IPUNCH                                                        
      IF(IPUNCH.NE.-2) GO TO 10105                                              
C     BEGINNING OF INPUT FOR Jose MARTI'S BRANCHES                      M31. 572
      IF (IPUNCH .NE. -2) IMARTI = IMARTI + 1                           M44. 142
      IF (ILUIS .NE. 0) WRITE (LUNIT6, 1067)                             M44. 143
 1067 FORMAT( 49H Attantion. You have mixed Luis- with Jose- model)     M44. 144
      PDT0=AA                                                           M43. 628
      IF(PDT0.GE.1..OR.PDT0.EQ.0.)GO TO 6000                            M43. 629
      PDT0=1.                                                           M43. 630
      WRITE(LUNIT6,6001)                                                M43. 631
 6001 FORMAT(//' A TOO LOW VALUE OF P*DELTAT WAS READ. P*DELTAT CHANGED M43. 632
     1 TO 1.'//)                                                        M43. 633
 6000 PDT=PDT0                                                          M43. 634
C 3456789012345678901234567890123456789012345678901234567890123456789012M43. 635
      IFX=IFX+1                                                                 
      IF ( IFX .GT. LHIST ) GO TO 8120                                          
      IFSEM = IFSEM + 1                                                         
      IF ( IFSEM .GT. LFSEM ) GO TO 8090                                        
      N11 = -ITYPE                                                              
      IF ( NOUTPR  .EQ.  0 )                                            M31. 578
     1 WRITE (KUNIT6, 7629)  N11                                        M35. 757
 7629 FORMAT ( 24H+MARTI TRANSMISSION MODE,  I3,                        M31. 580
     1         19H  PARAMETERS BEGIN.   )                               M31. 581
      AA = NOUTPR                                                       M31. 582
      IF ( H1  .EQ.  2.0 )   NOUTPR = 1                                 M31. 583
C     READ INPUT CARD USING CIMAGE                                      M31. 584
      CALL CIMAGE                                                       M31. 585
      DECODE (80, 7633, ABUFF(1))  NPZ, AK0ZC                           M31. 586
 7633 FORMAT ( I8, E32.20 )                                             M31. 587
      CKI(IBR) = NPZ                                                            
      SCONST(IFSEM) = AK0ZC                                                     
      KODSEM(IBR) = IFSEM                                                       
      INDHST(IBR) = IFX                                                         
      IF ( NOUTPR  .EQ.  0 )                                                    
     1 WRITE (KUNIT6, 7635)  NPZ, AK0ZC                                 M35. 758
 7635 FORMAT ( 33H+ZC BEGINS. ORDER, ZC(INFINITY) =,                    M31. 594
     1         I5,  E11.3  )                                            M31. 595
      IF ( H1  .EQ.  1.0 )   NOUTPR = 1                                 M31. 596
      N11 = 1                                                                   
 7638 N12 = N11 + 2                                                             
      IF ( N12  .GT.  NPZ )   N12 = NPZ                                 M31. 599
C     READ INPUT CARD USING CIMAGE                                      M31. 600
      CALL CIMAGE                                                       M31. 601
      DECODE (80, 7641, ABUFF(1))  ( VOLTBC(II), II=N11, N12)           M31. 602
 7641 FORMAT ( 3E26.0 )                                                 M31. 603
      IF ( NOUTPR  .EQ.  0 )                                                    
     1 WRITE (KUNIT6, 7644) N11, N12, (VOLTBC(II), II=N11,N12 )         M35. 759
 7644 FORMAT ( 10H+RESIDUALS, I3, 1H-, I2, 1H.,  3E11.3 )               M31. 606
      NN11 = N11 + IFSEM                                                        
      NN12 = N12 + IFSEM                                                        
      I = N11                                                                   
      DO 27644  JJ = NN11, NN12                                                 
      SCONST(JJ) = VOLTBC(I)                                                    
      I = I + 1                                                                 
27644 CONTINUE                                                          M31. 613
      N11 = N11 + 3                                                             
      IF ( N11  .LE.  NPZ )   GO TO 7638                                        
      N11 = 1                                                                   
 7653 N12 = N11 + 2                                                     M31. 617
      IF ( N12  .GT.  NPZ )   N12 = NPZ                                         
C     READ INPUT CARD USING CIMAGE                                      M31. 619
      CALL CIMAGE                                                       M31. 620
      DECODE (80, 7641, ABUFF)  ( VOLTBC(II), II=N11, N12 )                     
      IF ( NOUTPR  .EQ.  0 )                                            M31. 622
     1 WRITE (LUNIT6, 7655) N11, N12, (VOLTBC(II), II=N11, N12)         M31. 623
 7655 FORMAT (   6H+POLES, I3, 1H-, I2, 1H.,  3E12.4  )                 M31. 624
      NN11 = NN12 + N11                                                         
      NN13 = NN12 + N12                                                 M31. 626
      I = N11                                                                   
      DO 27655 JJ = NN11, NN13                                                  
      SCONST(JJ) = VOLTBC(I)                                            M31. 629
      I = I + 1                                                         M31. 630
27655 CONTINUE                                                          M31. 631
      N11 = N11 + 3                                                     M31. 632
      IF ( N11  .LE.  NPZ )   GO TO 7653                                        
      IF ( H1  .EQ.  1.0 )   NOUTPR = AA                                M31. 634
      IF(PDT0.EQ.0.)GO TO 55000                                         M43. 636
      NPZA=NPZ                                                          M43. 637
      II=0                                                              M43. 638
50021 N11=IFSEM+NPZA+II+1                                               M43. 639
      N12=N11+NPZA-1                                                    M43. 640
      XSUM=0.                                                           M43. 641
      YSUM=0.                                                           M43. 642
      DO 50000 JJ=N11,N12                                               M43. 643
      IF(SCONST(JJ)*DELTAT.LT.PDT)GO TO 50000                           M43. 644
      GO TO 50001                                                       M43. 645
50000 CONTINUE                                                          M43. 646
      GO TO 50007                                                       M43. 647
50001 IF (JJ.EQ.N11)JJ=JJ+1                                             M43. 648
      IF(JJ.EQ.N12)GO TO 50007                                          M43. 649
      DO 50002 I=JJ,N12                                                 M43. 650
      NN11=I-NPZA                                                       M43. 651
      XSUM=XSUM+SCONST(NN11)/SCONST(I)                                  M43. 652
50002 YSUM=YSUM+SCONST(NN11)/SCONST(I)**2                               M43. 653
      YSUM=XSUM/YSUM                                                    M43. 654
      IF(YSUM*DELTAT.GE.PDT)GO TO 50030                                 M43. 655
      PDT=PDT+1.                                                        M43. 656
      GO TO 50021                                                       M43. 657
50030 NN11=JJ-NPZA                                                      M43. 658
      IF(II.NE.0)GO TO 50003                                            M43. 659
      PDT=PDT0                                                          M43. 660
      NPZ=NPZ-N12+JJ                                                    M43. 661
      CKI(IBR)=NPZ                                                      M43. 662
      GO TO 50004                                                       M43. 663
50003 NPA=NPA-N12+JJ                                                    M43. 664
      CKKJM(IBR)=NPA                                                    M43. 665
50004 SCONST(NN11)=YSUM*XSUM                                            M43. 666
      SCONST(JJ)=YSUM                                                   M43. 667
      I=N12-JJ+1                                                        M43. 668
      IF(II.EQ.0  .AND. NOUTPR .EQ. 0)                                  M43. 669
     1 WRITE(LUNIT6,50005)I,SCONST(NN11),SCONST(JJ)                     M43. 670
50005 FORMAT ( ' Z0 EQUIVALENCE.  I, R*P, P =', I5, 2E20.13 )           M43. 671
      IF(II.NE.0  .AND. NOUTPR .EQ. 0)                                  M43. 672
     1 WRITE(LUNIT6,50006)I,SCONST(NN11),SCONST(JJ)                     M43. 673
50006 FORMAT ( ' A1 EQUIVALENCE.  I, R*P, P =', I5, 2E20.13 )           M43. 674
C      IF(PDT.NE.PDT0 .AND. NOUTPR .EQ. 0)                              M43. 675
C     1 WRITE(LUNIT6,50031)PDT                                          M43. 676
C50031 FORMAT(//' *** NOTE - PDT CHANGED TO *** ',F7.2//)               M43. 677
      N12=N12-I+1                                                       M43. 678
      JJ=NN11                                                           M43. 679
      DO 50008 I=N11,N12                                                M43. 680
      JJ=JJ+1                                                           M43. 681
50008 SCONST(JJ)=SCONST(I)                                              M43. 682
      NN13=JJ                                                           M43. 683
50007 IF(II.NE.0)GO TO 50022                                            M43. 684
C     READ INPUT CARD USING CIMAGE                                      M31. 635
55000 CALL CIMAGE                                                       M43. 685
C 3456789012345678901234567890123456789012345678901234567890123456789012M43. 686
      DECODE (80, 7633, ABUFF(1))  NPA, TAUO                                    
      IF (TAUO .GT. DELTAT)                                             M35. 760
     1                     GO TO 7656                                           
      KILL = 197                                                        M35. 761
      FLSTAT(14) = TAUO                                                         
      LSTAT(19) = 7656                                                  M35. 763
      GO TO 9999                                                        M35. 764
 7656 CKKJM(IBR) = NPA                                                          
      IF ( NOUTPR  .EQ.  0 )                                            M31. 639
     1 WRITE (LUNIT6, 7659)  NPA, TAUO                                  M31. 640
 7659 FORMAT ( 29H+A1 BEGINS. ORDER, TAU(SEC) =,  I6, E14.5  )          M31. 641
      IF ( H1  .EQ.  1.0 )   NOUTPR = 1                                 M31. 642
      N11 = 1                                                           M31. 643
 7664 N12 = N11 + 2                                                     M31. 644
      IF ( N12  .GT.  NPA )   N12 = NPA                                 M31. 645
C     READ INPUT CARD USING CIMAGE                                      M31. 646
      CALL CIMAGE                                                       M31. 647
      DECODE (80, 7641, ABUFF(1))  ( VOLTBC(II), II=N11, N12)           M31. 648
      IF ( NOUTPR  .EQ.  0 )                                            M31. 649
     1 WRITE (KUNIT6, 7644) N11, N12, (VOLTBC(II), II=N11,N12 )         M35. 766
      NN11 = NN13 + N11                                                 M31. 651
      NN12 = NN13 + N12                                                 M31. 652
      I = N11                                                           M31. 653
      DO 37644 JJ = NN11, NN12                                          M31. 654
      SCONST(JJ) = VOLTBC(I)                                            M31. 655
      I = I + 1                                                         M31. 656
37644 CONTINUE                                                          M31. 657
      N11 = N11 + 3                                                     M31. 658
      IF ( N11  .LE.  NPA )   GO TO 7664                                M31. 659
      N11 = 1                                                           M31. 660
 7672 N12 = N11 + 2                                                     M31. 661
      IF ( N12  .GT.  NPA )   N12 = NPA                                 M31. 662
C     READ INPUT CARD USING CIMAGE                                      M31. 663
      CALL CIMAGE                                                       M31. 664
      DECODE (80, 7641, ABUFF)  ( VOLTBC(II), II=N11, N12 )             M31. 665
      IF ( NOUTPR  .EQ.  0 )                                            M31. 666
     1 WRITE (LUNIT6, 7655) N11, N12, (VOLTBC(II), II=N11, N12)         M31. 667
      NN11 = NN12 + N11                                                 M31. 668
      NN13 = NN12 + N12                                                 M31. 669
      I = N11                                                           M31. 670
      DO 37655 JJ = NN11, NN13                                          M31. 671
      SCONST(JJ) = VOLTBC(I)                                            M31. 672
      I = I + 1                                                         M31. 673
37655 CONTINUE                                                          M31. 674
      N11 = N11 + 3                                                     M31. 675
      IF ( N11  .LE.  NPA )   GO TO 7672                                M31. 676
      IF(PDT.EQ.0.)GO TO 50022                                          M43. 688
      II=2*NPZ                                                          M43. 689
      NPZA=NPA                                                          M43. 690
      GO TO 50021                                                       M43. 691
50022 NOUTPR = AA                                                       M43. 692
      CNVHST(IFX) = TAUO                                                M31. 678
      IFX = IFX + 14                                                    M31. 679
      IF ( IFX .GT. LHIST )  GO TO 8120                                 M31. 680
      IFSEM = IFSEM + 7 * NPA + 7 * NPZ + 8                             M31. 681
      IF ( IFSEM .GT. LFSEM)  GO TO 8090                                M31. 682
      IF ( IPRSUP  .GE.  1 )                                            M31. 683
     1 WRITE (LUNIT6, 8434)  IFX, NPZ, NPA, IBR, IFSEM                  M31. 684
 8434 FORMAT ( 46H DONE MARTI CARDS.  IFX, NPZ, NPA, IBR,IFSEM =, 5I6 ) M31. 685
20105 ICHECK = 3                                                        M31. 686
      LENGTH(IBR1)=KCOUNT                                                       
      IF ( KCOUNT  .GT.  MXPHAS )                                       M31. 688
     1 MXPHAS = KCOUNT                                                  M31. 689
      IF ( IPRSUP  .GE.  1 )                                            M31. 690
     1 WRITE (LUNIT6, 6078) ITYPE, N13, MODEL, IPUNCH                   M31. 691
 6078 FORMAT ( 41H BELOW 20105. ITYPE, N13, MODEL, IPUNCH =,4I6)        M31. 692
      IF ( IPUNCH  .NE.  -2 )   GO TO 100                                       
      IF ( KCOUNT .EQ. MODEL )  GO TO 10010                                     
      IF ( KCOUNT .NE. 1 )      GO TO 100                                       
      IF ( N13     .LT.  1 )    GO TO 100        
      MODEL = N13                                                       M31. 697
      LITYPE(IBR) = IADDRS                                              M31. 698
      IF ( IPRSUP  .GE.  1 )                                            M31. 699
     1 WRITE (LUNIT6, 26078) MODEL, IADDRS, IBR1                        M31. 700
26078 FORMAT (59H MARTI'S MODEL FOR UNTRANSPOSED LINE, MODEL, IADDRS, IBM31. 701
     1R1 =, 3I10 )                                                      M31. 702
      IF ( N13 .EQ. 1 )  GO TO 10010
      GO TO 100                                                         M31. 703
10105 IF ( IPUNCH .NE. -3 ) GO TO 6552                                          
 6552 IF( KCOUNT .GT. 1 )  GO TO 5874                                   M43. 694
      XLONG1 = XLONG                                                            
      IF( XLONG .GT. 0.0 )  GO TO 5877                                          
      IF( ILINE .EQ. 2 )  GO TO 5877                                            
      KILL = 78                                                         M28.3712
      FLSTAT(16) = XLONG                                                M28.3713
      LSTAT(19) = 5874                                                  M28.3714
      GO TO 9999                                                        M28.3715
 5874 IF( XLONG .EQ. XLONG1 )  GO TO 5877                               M28.3716
      IF( XLONG .EQ. 0.0 )  GO TO 5877                                  M28.3717
      KILL = 77                                                         M28.3718
      LSTAT(19) = 191                                                   M28.3719
      FLSTAT(16) = XLONG                                                M28.3720
      FLSTAT(15) = XLONG1                                               M28.3721
      LSTAT(14) = KCOUNT                                                M28.3722
      GO TO 9999                                                        M28.3723
 5877 D17 = H1 + AA + H3 + XLONG                                                
      ICHTR2 = 0                                                        M37.2440
      IF ( D17 .NE. 0.0 )  ICHTR2 = 1                                   M37.2441
      XLONG = XLONG1                                                    M28.3725
      IF( KCOUNT .NE. 3 )  GO TO 198                                    M28.3726
      IF ( N13  .GT.  0 )  GO TO 198                                    M29.1295
      IF ( ICHTR2  .EQ.  0 )   GO TO 197                                M37.2442
      IF ( AA  .LE.  0.0 )   GO TO 5878                                 M28.3728
      IF ( H3  .GT.  0.0 )   GO TO 5879                                 M28.3729
 5878 KILL = 93                                                         M28.3730
      LSTAT(19) = 5878                                                  M28.3731
      FLSTAT(14) = AA                                                   M28.3732
      FLSTAT(15) = H3                                                   M28.3733
      GO TO 9999                                                        M28.3734
 5879 IBR15 = IBR1 + 5                                                  M28.3735
      DO 5890 I=IBR1,IBR15                                              M28.3736
 5890 KODEBR(I)=-1                                                      M28.3737
  198 IF(ILINE.GT.0) GO TO 192                                                  
      D2 = AA * TENM3                                                           
      CI1 = H3 / 1 000 000.                                                     
      IF( XOPT .GT. 0.0 ) D2 = D2 * 1000./( TWOPI*XOPT )                        
      IF( COPT .GT. 0.0 )   CI1 = CI1 / ( TWOPI*COPT )                          
      A=SQRTZ(D2/CI1)                                                           
      H2=XLONG*A*CI1                                                            
      GO TO 193                                                         M28.3745
  192 A=AA                                                                      
      H2=H3                                                                     
      IF(ILINE.EQ.1) H2=XLONG/H2                                                
  193 CIK(IBR)=H2                                                               
      IF ( NOUTPR  .EQ.  0 )                                            M28.3750
     1 WRITE (KUNIT6, 4104)  H1, AA, H3, A, H2                          M35. 767
 4104 FORMAT ( 1H+,  5E10.3 )                                           M28.3752
      IF ( IPRSUP  .GE.  4 )                                            M31. 705
     1 WRITE (LUNIT6, 5487)  IPUNCH, KCOUNT, MODEL, N13                 M31. 706
 5487 FORMAT (  42H BELOW 4104.  IPUNCH, KCOUNT, MODEL, N13 =,          M31. 707
     1  4I5  )                                                          M31. 708
      IF(H1.EQ.0. .OR. IPUNCH.GT.0) A=-A                                        
      CK(IBR)=H1*XLONG                                                          
      CI(IBR)=A                                                                 
      IF( IPUNCH .LT. 0 )                                               M28.3756
     1                GO TO 20160                                               
      ICHECK=3                                                                  
      LENGTH(IBR1)=KCOUNT                                                       
      IF ( KCOUNT  .GT.  MXPHAS )                                       M31. 709
     1 MXPHAS = KCOUNT                                                  M31. 710
      IF ( KCOUNT .EQ. MODEL)                                           M29.1296
     1                    GO TO 10010                                           
      IF ( KCOUNT .NE. 1 ) GO TO 100                                    M29.1297
      IF ( N13 .EQ. 0 )  MODEL = 0
      IF ( N13 .LE. 1 ) GO TO 100                                       M29.1298
      MODEL = N13                                                       M29.1299
      LITYPE(IBR) = IADDRS                                              M29.1300
      GO TO 100                                                                 
  196 KCOUNT=KCOUNT+1                                                           
      IF ( IPRSUP  .GE.  1 )                                            M31. 711
     1 WRITE (6, 7693)  KCOUNT, ITYPE, MODEL                            M31. 712
 7693 FORMAT ( 40H READY FOR [TI]?  KCOUNT, ITYPE, MODEL =, 3I6 )       M31. 713
      IF (IPSEM .NE. 0) GO TO 2011                                      M44. 146
      IF((-ITYPE).NE.KCOUNT)                                            M28.3761
     1                  GO TO 141                                               
 2011 IF ( KCOUNT .GT. 18)                                              M31. 714
     1                GO TO 9175                                                
      IF ( IPRSUP  .GE.  4 )                                            M43. 699
     1 write (*,*) ' Cable cont.?  NTLIN, IBR,  N1, N2 =',              M43. 700
     2                             NTLIN, IBR,  N1, N2                  M43. 701
      IF ( IPRSUP  .GE.  4 )                                            M43. 702
     1 write ( *,* ) 'CABLE CHECK?      model= , kcount=',              M43. 703
     2                                  model  , kcount                 M43. 704
      IF ( -IPUNCH .NE. 4 ) GO TO 5202                                  M43. 705
      IPUNCH = IMODEL(IBR-1)                                                    
      MBUS(IBR1)=IABS(MBUS(IBR1) )                                      M43. 707
      KBUS(IBR) = N1                                                            
      MBUS(IBR) = N2                                                    M43. 709
C      if ( n13 .eq. 0 ) N13Q = N13 + 1                                 M44. 147
C      IF (kcount .nq. N13Q ) GO TO 190                                 M43. 711
C      IF(MODEL.LE.1)GO TO 10004                                        M43. 712
      IF(-ITYPE.LE.MODEL)GO TO 190                                              
      IF (IPUNCH .EQ. -4 .AND. N13 .EQ. 0 ) GO TO 190                   M44. 148
C                                                                       M43. 714
C                                                                       M43. 715
50050 INOFF1 = LBRNCH                                                           
      INOFF2 = 2 * LBRNCH                                                       
      NOUTPR = NPLL                                                     M44. 149
      INOFF3 = 3 * LBRNCH                                                       
      INOFF4 = 4 * LBRNCH                                                       
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
      KOFF13 = KOFF10 +ISECTI                                           M43. 731
      KOFF14 = KOFF13 +ISECTI                                           M43. 732
      KOFF15 = KOFF14 +ISECTI                                           M43. 733
      KOFF16 = KOFF15 +ISECTI                                           M43. 734
      KOFF17 = KOFF16 +ISECTI                                                   
      KOFF18 = KOFF17 +ISECTI                                                   
      KOFF19 = KOFF18 +ISECTI                                                   
      KOFF20 = KOFF19                                                           
      IF ( KGROUP .NE. 1 )                                              M43. 739
     1                WK1(KOFF19) =1                                            
      WK1(KOFF19) = WK1(KOFF19)                                         M43. 741
     1                     + KGROUP                                             
      NCOUNT=IDM                                                                
      IF ( IPSEM .NE. 0) N13 = IPSEM                                    M44. 150
      NPHS = N13                                                        M43. 746
      NPHS2 = N13 ** 2                                                  M43. 747
      NPHSU=(NPHS+1)*NPHS/2                                                     
      IBR2 = IBR - NPHS + 1                                             M43. 749
      IBR3 = IBR - NPHS + 3                                             M43. 750
      IFQ = IFQ + 1                                                             
      INFDLI(INOFF2 + IBR2) =                                           M43. 752
     1        IFQ                                                               
C      INFDLI( INOFF1 + IBR2 ) =                                        M43. 754
C    1              IDM    !Remember intial adress for this group, 1--10M43. 755
      INFDLI( INOFF1 + IBR2 ) =  IADDRS                                 M44. 151
      INFDLI( INOFF1 + IBR2 ) =  IDM                                    M44. 152
C      NCOUNT = IADDRS                                                  M44. 153
      IADDRS = IADDRS + NPHS2                                           M44. 154
      INFDLI( INOFF4 + IBR2 ) = IDU                                             
      IF ( IPRSUP  .GE.  4 )                                            M43. 757
     1 WRITE (*,*) ' BEGIN TO READ Qi. NTLIN,IQ,IDM,IDU,IDT,IDQ= ',     M43. 758
     2                                 NTLIN,IQ,IDM,IDU,IDT,IDQ         M43. 759
      IF (IPSEM .NE. 0) GO TO 2003                                      M44. 155
      DO 902 I=1,NPHS2                                                          
      WK1(KOFF20+NCOUNT)=IDQ                                                    
      CALL CIMAGE                                                       M43. 762
      DECODE (80, 6366, ABUFF(1))  NP, SCONST(IFQ)                      M43. 763
 6366 FORMAT ( I8, E32.0 )                                              M43. 764
      IF (NOUTPR .NE. -7777                                             M43. 765
     1         .AND. NOUTPR .NE. 1 )                                    M44. 156
     2 WRITE (KUNIT6, 8262) NP , SCONST(IFQ)                            M43. 766
 8262 FORMAT ( 24H+ Ti.  NP, SCONST(IFQ) =, I5,  E15.5  )               M43. 767
      CALL INTERP                                                               
      WK1(KOFF20+NCOUNT) = NP                                                   
      IF(NP.GE.0) GO TO 1441                                                    
 1441 J1 = 1                                                            M43. 771
      NTICPL = 0                                                        M44. 157
 4888 J2 = J1 + 2                                                       M43. 772
      M =  NP                                                                   
      IF ( J2 .GT. M)  J2 = M                                           M43. 774
C     READ INPUT CARD USING CIMAGE                                      M43. 775
      CALL CIMAGE                                                       M43. 776
      DECODE (80, 105, ABUFF(1))  ( VOLTBC(J), J=J1, J2 )               M43. 777
 105  FORMAT (  3E26.0 )                                                        
      IF (NOUTPR .NE. -7777                                             M43. 779
     1         .AND. NOUTPR .NE. 1 )                                    M44. 158
     2 WRITE (KUNIT6, 4413)   ( VOLTBC(J), J=J1, J2 )                   M43. 780
 4413 FORMAT ( 7H+ QK-I:, 3E14.5 )                                      M43. 781
      JJ1 = J1 + IFQ                                                            
      JJ2 = J2 + IFQ                                                            
      IML = J1                                                                  
      DO 4844 JJ = JJ1 , JJ2                                                    
      IF ( ABS(SCONST(JJ)) .GE. 1.E+13 ) NTICPL = NTICPL + 1            M44. 159
      SCONST(JJ) = VOLTBC(IML)                                                  
      IML = IML + 1                                                             
 4844 CONTINUE                                                                  
      J1 = J1 + 3                                                               
      IF ( J1 .LE. M )  GO TO 4888                                              
      CALL INTERP                                                               
      INTCPL = LHIST + I                                                M44. 160
      CBLHST(INTCPL) = NTICPL                                           M44. 161
      J1 = 1                                                            M43. 793
 4847 J2 = J1 + 2                                                       M43. 794
      M =  NP                                                                   
      IF ( J2 .GT. M)  J2 = M                                           M43. 796
C     READ INPUT CARD USING CIMAGE                                      M43. 797
      CALL CIMAGE                                                       M43. 798
      DECODE (80, 105, ABUFF(1))  ( VOLTBC(J), J=J1, J2 )               M43. 799
      IF (NOUTPR .NE. -7777                                             M43. 800
     1      . AND.    NOUTPR .NE. 1)                                    M44. 162
     2 WRITE (KUNIT6, 8414)  ( VOLTBC(J), J=J1, J2 )                    M43. 801
 8414 FORMAT ( 7H+ QP-I:, 3E14.5 )                                      M43. 802
      NN11 = JJ2 + J1                                                           
      NN13 = JJ2 + J2                                                           
      IML = J1                                                                  
      DO 4744 JJ=NN11,NN13                                                      
      SCONST(JJ) = VOLTBC(IML)                                                  
      IML = IML + 1                                                             
 4744 CONTINUE                                                                  
      J1 = J1 + 3                                                               
      IF ( J1 .LE. M )  GO TO 4847                                              
      IFQ = NN13                                                        M43. 812
     1      + 1                                                                 
      CALL INTERP                                                               
      NCOUNT=NCOUNT+1                                                           
      IDQ = IDQ                                                         M43. 816
     1       + NP                                                               
902   CONTINUE                                                                  
      IDQ = IDQ -1                                                      M43. 819
      IDQ = 2 * IDQ                                                     M43. 820
      IDQ = IDQ + 1                                                     M43. 821
      IFQ = NN13                                                                
     1    + ( NN13 - IFSEM - NPHS2 ) * 3 / 2                                    
     2    + 2 * NPHS2                                                           
      IFSEM = IFQ                                                               
      INFDLI( INOFF3 + IBR2 ) =                                         M43. 826
     1                NN13                                                      
      IDM=IDM+NPHS2                                                             
      IDU=IDU+NPHSU                                                             
      GO TO 2009                                                        M44. 163
 2003 JKL = 0                                                           M44. 164
      IF (IPSEM .LT.3) JKL = 1                                          M44. 165
      DO 2010 J = 1, NPHS2                                              M44. 166
      WK1( KOFF20+NCOUNT) = 1                                                   
      NCOUNT = NCOUNT + 1                                               M44. 168
 2010 CONTINUE                                                          M44. 169
      IF( IPSEM .LE. 3) IP = IPSEM *2                                   M44. 170
      IF( IPSEM .GT. 3 .AND. IPSEM .LT. 6 ) IP = IPSEM*4                M44. 171
      IF (IPSEM .EQ. 6) IP = IPSEM*4                                    M44. 172
      KL = 0                                                            M44. 173
      IFQT = IFQ                                                        M44. 174
      IFQQ = IFQ                                                        M44. 175
      DO 2008 I = 1, IP                                                 M44. 176
      CALL CIMAGE                                                       M44. 177
      IF ( IPSEM .LE. 3 ) GO TO 2113                                    M44. 178
      IF ( I .LE. IPSEM*2 ) GO TO 2008                                  M44. 179
      GO TO 2114                                                        M44. 180
 2113 IF ( I .LE. IPSEM ) GO TO 2008                                    M44. 181
 2114 IF ( IPSEM .EQ. 6 ) NROWT = 6                                     M44. 182
      IF ( IPSEM .EQ. 6 )           GO TO 2014                          M44. 183
      IF (IPSEM .GT. 3 .AND. JKL .EQ. 0) NROWT = 6                      M44. 184
      IF (IPSEM .LE. 3 ) NROWT = 2 * IPSEM                              M44. 185
      IF (IPSEM .LE. 3 ) GO TO 2014                                     M44. 186
      IF (JKL .EQ. 1)        NROWT = IPSEM * 2 - 6                      M44. 187
      IF (NROWT .EQ. 6 )   JKL = 1                                      M44. 188
      IF (NROWT .LT. 6) JKL  = 0                                        M44. 189
      IF (IPSEM .GT. 3 .AND. IPSEM .EQ. 6 ) NROWT = 6                   M44. 190
 2014 NFRFLD = NROWT                                                    M44. 191
      IF (KOLBEG .LE. 0) GO TO 2005                                     M44. 192
      CALL FREFLD( VOLTBC(1) )                                          M44. 193
      GO TO 2006                                                        M44. 194
 2005 CONTINUE                                                          M44. 195
      DECODE (80,8170, ABUFF(1))                                        M44. 196
     1           (VOLTBC(IK), IK=1, NFRFLD)                             M44. 197
 2006 N12 = 1                                                           M44. 198
      IF ( NOUTPR  .EQ.  0 )                                                    
     1 WRITE (KUNIT6, 2020)  (VOLTBC(II), II=1,NFRFLD )                 M44. 200
 2020 FORMAT ( 14H Ti - Semlyen.  3E12.3 )                              M44. 201
      DO 2007 K=1, NROWT, 2                                             M44. 202
      KL = KL + 1                                                               
      SCONST(IFQ) = VOLTBC(N12)                                         M44. 204
      SCONST(IFQ + 1) = 0.                                              M44. 205
      SCONST(IFQ + 2) = 1.                                              M44. 206
      N12 = N12 + 2                                                     M44. 207
      IF ( KL .LT. IPSEM ) GO TO 2015                                   M44. 208
      KL = 0                                                                    
      IFQQ = IFQQ + 3                                                           
      IFQ = IFQQ                                                        M44. 211
      GO TO 2007                                                        M44. 212
 2015 IFQ = IFQ + IPSEM * 3                                             M44. 213
 2007 CONTINUE                                                          M44. 214
 2008 CONTINUE                                                          M44. 215
      IFQ = IFQT + IPSEM**2 * 3                                         M44. 216
      NN13 = IFQ - 1                                                    M44. 217
      IFQ = NN13                                                                
     1    + ( NN13 - IFSEM - NPHS2 ) * 3 / 2                                    
     2    + 2 * NPHS2                                                           
      IFSEM = IFQ                                                               
      INFDLI( INOFF3 + IBR2 ) =                                         M44. 222
     1                NN13                                                      
      IDM=IDM+NPHS2                                                             
      IDU=IDU+NPHSU                                                             
 2009 IF ( -ITYPE .EQ. NPHS                                                     
     1                      .OR. NT1 .EQ. IPSEM)                        M43. 831
     2 ICHECK=1                                                                 
      IF ( -ITYPE .EQ. NPHS                                                     
     1                      .OR. NT1 .EQ. IPSEM)                        M44. 227
     2 IFKC = IFKC + N13                                                        
      IF ( IPRSUP  .GE.  4 )                                            M43. 834
     1 WRITE (*,*) 'Next bunch of coupled branches. ntlin=, kcount=',   M43. 835
     2                                              ntlin, kcount       M43. 836
      ITRANM = MODEL                                                    M43. 837
      MODEL = 0                                                         M43. 838
      ICHECK = 1                                                                
      KGROUP = 1                                                        M43. 840
      NOUTPR = AA                                                       M44. 228
      GO TO 100                                                         M43. 841
C                                                                       M43. 842
 5202 LENGTH(IBR1)=KCOUNT                                               M43. 843
      IF ( KCOUNT  .GT.  MXPHAS )                                       M31. 715
     1 MXPHAS = KCOUNT                                                  M31. 716
      MBUS(IBR1)=IABS(MBUS(IBR1) )                                      M28.3763
      KBUS(IBR)=N1                                                      M28.3764
      MBUS(IBR)=N2                                                      M28.3765
      IF (IPUNCH .EQ. -2 .AND. N13 .EQ. 0 ) GO TO 190                   M44. 229
      IF (KCOUNT.LE.3) GO TO 190                                        M28.3766
      IF(MODEL.LE.1)GO TO 10004                                         M29.1302
      IF(KCOUNT.LE.MODEL)GO TO 190                                      M29.1303
10010 IADD=IADDRS+2*MODEL                                               M32.1229
     1       *MODEL-1                                                           
C   constant and frequence-dependent modal, 2XNPSXNPS cells sre requiredM43. 845
      IF ( IPRSUP  .GE.  1 )                                            M31. 717
     1 WRITE (LUNIT6, 27693)                                            M31. 718
27693 FORMAT ( 34H AT S.N. 10010, READY TO READ [TI] )                  M31. 719
      IF(IADD.LT. LFD  )GO TO 10007                                     M31. 720
C CHECK FOR OVERFLOW FOR TI ARRAY                                       M29.1306
      IPRINT=21                                                         M31. 721
      LSTAT(19) = 10010                                                 M31. 722
      GO TO 9000                                                        M29.1308
 9175 WRITE (LUNIT6, 9176)                                              M29.1309
 9176 FORMAT(53H LAST DISTRIBUTED PARAM. LINE HAS MORE THAN 18 PHASES)  M31. 723
      GO TO 9000                                                        M29.1311
C     NEXT READ IN COMPLEX TRANSFORMATION MATRIX  [TR] + J[TI] :        M31. 724
10007 N9 = 1                                                            M31. 725
 2711 N7 = 1                                                            M31. 726
 2716 N1 = N9 + IADDRS - 1                                                      
      I = 1                                                             M31. 728
C     READ INPUT CARD USING CIMAGE                                      M31. 729
 2722 CALL CIMAGE                                                       M31. 730
      NFRFLD = 6                                                                
      IF ( I+5  .GT.  MODEL )   NFRFLD = MODEL - I + 1                  M31. 732
      IF ( KOLBEG  .GT.  0 )   GO TO 2728                                       
      DECODE (80, 2721, ABUFF)  ( VOLTBC(J), J=1, NFRFLD )              M31. 734
 2721 FORMAT ( 6E12.0 )                                                 M31. 735
      GO TO 2733                                                        M31. 736
 2728 CALL FREFLD ( VOLTBC(1) )                                         M31. 737
 2733 IF ( NOUTPR  .EQ.  0 )                                                    
     1 WRITE (KUNIT6, 2736)  TEXTMX(N7), N9, I,                         M35. 768
     2                       ( VOLTBC(J), J=1, NFRFLD )                 M31. 740
 2736 FORMAT ( A4,  I2,  1H,,  I2,  4H)...,  6F6.3 )                    M31. 741
      DO 2739  N8=1, NFRFLD                                             M31. 742
      IF ( N7  .EQ.  1 ) QFD(N1) = VOLTBC(N8)                                   
      IF ( N7  .EQ.  2 ) SFD(N1) = VOLTBC(N8)                                   
      I = I + 1                                                         M31. 745
 2739 N1 = N1 + MODEL                                                   M31. 746
      IF ( I  .LE.  MODEL )   GO TO 2722                                M31. 747
      N7 = N7 + 1                                                       M31. 748
      IF ( N7  .LE.  2 )   GO TO 2716                                   M31. 749
      N9 = N9 + 1                                                       M31. 750
      IF ( N9  .LE.  MODEL )   GO TO 2711                               M31. 751
      IF( IPRSUP .LT. 2 ) GO TO 10005                                   M32.1230
      N9 = ITRANM * ITRANM                                              M32.1231
      WRITE(LUNIT6, 10003) N13,MODEL,IADD,N9,(QFD(I),I=IADDRS,IADD)     M32.1232
10003 FORMAT ( 40H FOR THIS UNTRANSPOSED LINE, N13, MODEL,,             M32.1233
     1         40H IADD, N9, (QFD(I), I=(IADD-N9), IADD) =,             M32.1234
     2   4I7  ,/,  ( 1X, 6E15.6 ) )                                     M32.1235
C     DEFINE NEW ADDRESS FOR FOLLOWING UNTRANSPOSED LINE                M32.1236
10005 IADDRS = IADD                                                     M32.1237
     1           + 1                                                            
      ITRANM = MODEL                                                            
      MODEL = 0                                                         M32.1239
      ICHECK = 1                                                                
      GO TO 100                                                                 
10004 CIK(IBR)=CIK(IBR1+1)                                              M29.1334
      IMODEL(IBR)=IMODEL(IBR1+1)                                        M31. 755
      IF(IMODEL(IBR).NE.-2) GO TO 10106                                 M31. 756
      KTRLSW(5) = 1                                                     M32.1241
      GO TO 100                                                         M31. 757
10106 CONTINUE                                                          M31. 758
  197 IF ( ICHTR2 .NE. 0 .AND. KCOUNT .EQ. 4 ) GO TO 5891               M37.2443
      IBRM=IBR1+1                                                       M28.3768
      IF ( KODEBR(IBR1) .EQ. -1)  IBRM = IBR1 + 2                       M31. 759
      CIK(IBR)=CIK(IBRM)                                                M28.3769
      CI(IBR)=CI(IBRM)                                                  M28.3770
      CK(IBR)=CK(IBRM)                                                  M28.3771
      NR(IBR)=NR(IBRM)                                                  M28.3772
      GO TO 5892                                                        M28.3773
 5891 IBRM=IBR-1                                                        M28.3774
      IBRM1=IBR-2                                                       M28.3779
      IF ( KODEBR(IBR1) .NE. -1 )  GO TO 15891                          M31. 760
      D1 = CIK(IBRM)                                                    M31. 761
      D2 = CI(IBRM)                                                     M31. 762
      D3 = CK(IBRM)                                                     M31. 763
      D4 = NR(IBRM)                                                     M31. 764
15891 CIK(IBRM)=CIK(IBRM1)                                              M31. 765
      CI(IBRM)=CI(IBRM1)                                                M28.3781
      CK(IBRM)=CK(IBRM1)                                                M28.3782
      NR(IBRM)=NR(IBRM1)                                                M28.3783
      IF ( KODEBR(IBR1) .NE. -1 )  GO TO 15895                          M31. 766
      CIK(IBRM1) = D1                                                   M31. 767
      CI(IBRM1) = D2                                                    M31. 768
      CK(IBRM1) = D3                                                    M31. 769
      NR(IBRM1) = D4                                                    M31. 770
15895 CIK(IBR)=CIK(IBRM)                                                M31. 771
      CI(IBR)=CI(IBRM)                                                  M31. 772
      CK(IBR)=CK(IBRM)                                                  M31. 773
      NR(IBR)=NR(IBRM)                                                  M31. 774
 5892 IF(KODEBR(IBR1).EQ.-1)GO TO 5895                                  M28.3784
      IF ( NOUTPR  .EQ.  0 )                                            M28.3785
     1 WRITE (KUNIT6, 54121)                                            M35. 769
54121 FORMAT ( 48H+3RD OR LATER UNIF.-TRANSPOSED DISTRIBUTED COND.   )  M28.3787
      GO TO 100                                                         M29.1335
 5895 IF ( NOUTPR  .EQ.  0 )                                            M28.3789
     1 WRITE (KUNIT6, 5900)                                             M35. 770
 5900 FORMAT(47H+4TH OR LATER DOUBLE CKT DISTRIBUTED CONDUCTOR.)        M28.3791
  100 LSTAT(18) = 100                                                   M28.3792
      GO TO 9999                                                        M28.3793
  141 LSTAT(18) = 141                                                   M28.3794
      GO TO 9999                                                        M28.3795
20160 NCHAIN = 4                                                                
      GO TO 9999                                                        M28.3798
 9000 LSTAT(16) = IPRINT                                                M28.3799
      KILL = 1                                                          M28.3800
 9999 IF ( IPRSUP  .GE.  3 )                                            M28.3801
     1 WRITE (LUNIT6, 9998)  KILL, NCHAIN, LSTAT(18), IBR, IFSEM        M28.3802
 9998 FORMAT ( /,  17H EXIT  "DISTR2" .,                                M28.3803
     1             40H    KILL  NCHAIN LSTAT18     IBR   IFSEM          M28.3804
     2  ,/,  17X,  10I8  )                                              M28.3805
      IF ( IPRSUP  .GE.  1 )                                            M32.1242
     1 WRITE ( LUNIT6, 4568 )                                           M32.1243
 4568 FORMAT ( 24H  "EXIT  MODULE DISTR2." )                            M32.1244
      RETURN                                                            M28.3806
      END                                                               M28.3807
      SUBROUTINE OVER3                                                  M39.1006
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION TRSER(1),TXSER(1)                                       M42.2135
      EQUIVALENCE (TRSER(1),E(1)),(TXSER(1),F(1))                       M42.2136
      DIMENSION  MAPCAS(1),  MAPINV(1), NODE1(1), NODE2(1)              M31. 775
      EQUIVALENCE  (VOLTI(1), NODE1(1)),  (VOLTK(1), NODE2(1))          M31. 776
      EQUIVALENCE  ( VOLT(1), MAPINV(1) )                               M31. 777
      EQUIVALENCE  ( MAPCAS(1), YKM(1) )                                M14.1233
      DIMENSION CASLNR(1),CASLNX(1)                                     M42.2137
      EQUIVALENCE (CASLNR(1),XK(1)),(CASLNX(1),XM(1))                   M42.2138
      DIMENSION CSER(1)                                                 M42.2139
      EQUIVALENCE ( CSER(1), KODE(1) )                                  M32.1245
      DIMENSION TRSHUN(1),TXSHUN(1),CSHUN(1)                            M42.2140
      EQUIVALENCE (TRSHUN(1),KK(1)),(TXSHUN(1),KKS(1)),                 M42.2141
     1(CSHUN(1),KKNONL(1))                                              M42.2142
      EQUIVALENCE  (IPRSOV(35), IPOINT), (IPRSOV(36), LOCZ11)           M39.1007
      REAL*8            TEXT1, TEXT2, TEXT3, TEXT4, TEXT5, TEXT6, TEXT7 M42.2143
      DATA TEXT1  / 4HSTOP /                                            M42.2144
      DATA TEXT2  / 4H CAS /                                            M42.2145
      DATA TEXT3  / 4HCADE /                                            M42.2146
      DATA TEXT4  / 4H     /                                            M42.2147
      LOCATN(I,J)=(J*J-J)/2+I                                           M32.1246
      IF ( IPRSUP  .GE.  1 )                                            M32.1247
     1 WRITE ( LUNIT6, 4567 )                                           M32.1248
 4567 FORMAT ( 23H  "BEGIN MODULE OVER3." )                             M32.1249
      LL0 = 0                                                           M22.2618
      LL3 = 3                                                           M23. 986
      IEND=NPHCAS                                                       M42.2148
      IDUMY=NPHCAS                                                      M42.2149
C     READ INPUT CARD USING CIMAGE.                                     M42.2150
      CALL CIMAGE                                                       M42.2151
      IF(IEND.GT.14)IDUMY=14                                            M42.2152
      DECODE(80,76501,ABUFF)DSECTJ,MULTIP,MSER,MBR,MSECT,               M42.2153
     1(MAPCAS(I),I=1,IDUMY)                                             M42.2154
76501 FORMAT(2X,E6.2,18I4)                                              M42.2155
      IF ( NOUTPR  .EQ.  0 )                                            M20.1016
     1 WRITE (KUNIT6, 76702)  DSECTJ,MULTIP,MSER,MBR,MSECT,             M35. 771
     1 ( MAPCAS(I), I=1, LL3 )                                          M23. 987
76702 FORMAT ( 10H+LINE POS., 1X, E11.3, 7I4 )                          M42.2156
      IF(DSECTJ.EQ.0.0)DSECTJ=1.0                                       M42.2157
      IF(MULTIP.EQ.0)MULTIP=1                                           M42.2158
      IF ( MULTIP  .GT.  0 )   GO TO 7671                               M18. 305
      KILL=55                                                           M42.2159
      LSTAT(19) = 7671                                                  M18. 306
      LSTAT(12)=MULTIP                                                  M42.2160
      GOTO9200                                                          M42.2161
 7671 ISTART = 15                                                       M18. 307
76504 IEND=IEND-14                                                      M42.2162
      IF(IEND.LE.0)GOTO76503                                            M42.2163
C     READ INPUT CARD USING CIMAGE.                                     M42.2164
      CALL CIMAGE                                                       M42.2165
      IDUMY=ISTART+IEND-1                                               M42.2166
      IF(IEND.GT.14)IDUMY=ISTART+13                                     M42.2167
      DECODE(80,76502,ABUFF)(MAPCAS(I),I=ISTART,IDUMY)                  M42.2168
76502 FORMAT(24X,14I4)                                                  M42.2169
      IF ( IDUMY  .GT.  ISTART+11 )   IDUMY = ISTART + 11               M42.2170
      IF ( NOUTPR  .EQ.  0 )                                            M20.1018
     1 WRITE (KUNIT6, 76703)  (MAPCAS(I),I=ISTART,IDUMY)                M35. 772
76703 FORMAT ( 1H+, 12I4 )                                              M42.2171
      ISTART=ISTART+14                                                  M42.2172
      GOTO76504                                                         M42.2173
76503 NPOS2=2*NPHCAS                                                    M42.2174
      NPOS3=3*NPHCAS                                                    M42.2175
      FREQCS=TWOPI*FREQCS                                               M42.2176
      LOCZNN=LOCZ11+LOCATN(NPHCAS,NPHCAS)-1                             M42.2177
      FREQX=XOPT*TWOPI                                                  M42.2178
      FREQC=COPT*TWOPI*1000000.0                                        M42.2179
      IF(XOPT.EQ.0.0)FREQX = 1000.                                      M27. 664
      IF(COPT.EQ.0.0)FREQC=1000000.0                                    M42.2180
      DO 76430 I=LOCZ11,LOCZNN                                          M42.2181
      TX(I)=FREQCS*TX(I)/FREQX                                          M42.2182
76430 C(I)=FREQCS*C(I)/(2.0*FREQC)                                      M42.2183
      CALL CXRED(TR(LOCZ11),TX(LOCZ11),NPHCAS,LL0)                      M22.2619
      DO 7655  I=LOCZ11,LOCZNN                                          M18. 308
      TR(I)=-TR(I)                                                      M42.2184
 7655 TX(I) = -TX(I)                                                    M18. 309
      IEND=LOCATN(NPOS3,NPOS3)                                          M42.2185
      CALL MOVER0 ( CASLNR(1), IEND )                                   M42.2186
      CALL MOVER0 ( CASLNX(1), IEND )                                   M42.2187
      DO 76432 I=1,NPHCAS                                               M42.2188
      DO 76432 J=I,NPHCAS                                               M42.2189
      IF(MAPCAS(I).LE.NPHCAS)GOTO76711                                  M42.2190
      KILL=54                                                           M42.2191
      LSTAT(19) = 7655                                                  M18. 310
      LSTAT(12)=MAPCAS(I)                                               M42.2192
      LSTAT(13)=NPHCAS                                                  M42.2193
      GO TO 9200                                                        M42.2194
76711 NPOS=LOCATN(MAPCAS(I),MAPCAS(J))+LOCZ11-1                         M42.2195
      IF(MAPCAS(I).GT.MAPCAS(J))NPOS=LOCATN(MAPCAS(J),MAPCAS(I))+       M42.2196
     1LOCZ11-1                                                          M42.2197
      NNPOS=LOCATN(I,J)                                                 M42.2198
      CASLNR(NNPOS)=TR(NPOS)/DSECTJ                                     M42.2199
      CASLNX(NNPOS)=TX(NPOS)/DSECTJ+C(NPOS)*DSECTJ                      M42.2200
      NNPOS=LOCATN(I,J+NPHCAS)                                          M42.2201
      CASLNR(NNPOS)=-TR(NPOS)/DSECTJ                                    M42.2202
      CASLNX(NNPOS)=-TX(NPOS)/DSECTJ                                    M42.2203
      IF(I.EQ.J)GOTO 76433                                              M42.2204
      NNPOS=LOCATN(J,I+NPHCAS)                                          M42.2205
      CASLNR(NNPOS)=-TR(NPOS)/DSECTJ                                    M42.2206
      CASLNX(NNPOS)=-TX(NPOS)/DSECTJ                                    M42.2207
76433 NNPOS=LOCATN(I+NPHCAS,J+NPHCAS)                                   M42.2208
      CASLNR(NNPOS)=TR(NPOS)/DSECTJ                                     M42.2209
76432 CASLNX(NNPOS)=TX(NPOS)/DSECTJ+C(NPOS)*DSECTJ                      M42.2210
76521 MULTIP=MULTIP-1                                                   M42.2211
      IF(MULTIP.GT.0)GOTO76505                                          M42.2212
      IEND=NPHCAS                                                       M42.2213
      IDUMY=NPHCAS                                                      M42.2214
C     READ INPUT CARD USING CIMAGE.                                     M42.2215
      CALL CIMAGE                                                       M42.2216
      IF(IEND.GT.14)IDUM Y=14                                           M42.2217
      DECODE (80, 4012, ABUFF )  TEXT5, TEXT6, TEXT7                    M42.2218
 4012 FORMAT( 2X, 3A4 )                                                 M42.2219
      IF( TEXT5 .NE. TEXT1 )  GO TO 4013                                M42.2220
      IF( TEXT6 .NE. TEXT2 )  GO TO 4013                                M42.2221
      IF( TEXT7 .EQ. TEXT3 )  GO TO 76600                               M42.2222
 4013 CONTINUE                                                          M42.2223
      DECODE(80,76501,ABUFF)DSECTJ,MULTIP,MSER,MBR,MSECT,               M42.2224
     1(MAPCAS(I),I=1,IDUMY)                                             M42.2225
      IF ( NOUTPR  .EQ.  0 )                                            M20.1020
     1 WRITE (KUNIT6, 76702)  DSECTJ, MULTIP, MSER, MBR, MSECT,         M35. 773
     2 ( MAPCAS(I), I=1, LL3 )                                          M23. 989
      IF(DSECTJ.EQ.0.0)DSECTJ=1.0                                       M42.2226
      IF(MULTIP.EQ.0)MULTIP=1                                           M42.2227
      IF(MULTIP.GT.0)GOTO76713                                          M42.2228
      LSTAT(19) = 4013                                                  M18. 311
      KILL=55                                                           M42.2229
      LSTAT(12)=MULTIP                                                  M42.2230
      GOTO9200                                                          M42.2231
76713 ISTART=15                                                         M42.2232
76506 IEND=IEND-14                                                      M42.2233
      IF(IEND.LE.0)GOTO76507                                            M42.2234
C     READ INPUT CARD USING CIMAGE.                                     M42.2235
      CALL CIMAGE                                                       M42.2236
      IDUMY=ISTART+IEND-1                                               M42.2237
      IF(IEND.GT.14)IDUMY=ISTART+13                                     M42.2238
      DECODE(80,76502,ABUFF)(MAPCAS(I),I=ISTART,IDUMY)                  M42.2239
      IF(IDUMY.GT.ISTART+12)IDUMY=ISTART+12                             M42.2240
      IF ( NOUTPR  .EQ.  0 )                                            M20.1022
     1 WRITE (KUNIT6, 76703)  (MAPCAS(I),I=ISTART,IDUMY)                M35. 774
      GOTO 76506                                                        M42.2241
76507 DO 76429 I=1,NPHCAS                                               M42.2242
      IDUMY=MAPCAS(I)                                                   M42.2243
76429 MAPINV(IDUMY)=I                                                   M42.2244
      IF(MSER.LE.0)GOTO 76510                                           M42.2245
      CALL MOVER0 ( TRSER(1), NPHCAS )                                  M42.2246
      CALL MOVER0 ( TXSER(1), NPHCAS )                                  M42.2247
      CALL MOVER0 ( CSER(1), NPHCAS )                                   M42.2248
C     READ INPUT CARD USING CIMAGE.                                     M42.2249
76511  CALL CIMAGE                                                      M42.2250
      DECODE (80, 7651, ABUFF(1) )   ITYPE                              M18. 312
 7651 FORMAT ( I2 )                                                     M18. 313
      IF(ITYPE.GE.0.AND.ITYPE.LE.NPHCAS)GOTO76714                       M42.2251
      LSTAT(19)=7651                                                    M17. 293
      KILL=54                                                           M42.2252
      LSTAT(12)=ITYPE                                                   M42.2253
      GOTO9200                                                          M42.2254
76714 IF ( ITYPE  .EQ.  0     .AND.                                     M20.1024
     1     NOUTPR  .EQ.  0 )                                            M20.1025
     2 WRITE (KUNIT6, 76705)                                            M35. 775
76705 FORMAT(37H+BLANK CARD TERMINATING SERIES R-L-C.  )                M42.2255
      IF(ITYPE.EQ.0)GOTO76510                                           M42.2256
      DECODE(80,76530,ABUFF)TRSER(ITYPE),TXSER(ITYPE),CSER(ITYPE)       M42.2257
76530 FORMAT ( 26X, 3E6.0 )                                             M05. 337
      IF ( NOUTPR  .EQ.  0 )                                            M20.1027
     1 WRITE(KUNIT6,76704)TRSER(ITYPE),TXSER(ITYPE),CSER(ITYPE)         M35. 776
76704 FORMAT ( 14H+SERIES R-L-C.  , 2X, 3E11.3  )                       M42.2258
      GOTO76511                                                         M42.2259
76510 IF(MBR.LE.0)GOTO 76513                                            M42.2260
      MBR1=0                                                            M42.2261
76516 MBR1=MBR1+1                                                       M42.2262
C     READ INPUT CARD USING CIMAGE.                                     M42.2263
      CALL CIMAGE                                                       M42.2264
      DECODE (80, 4012, ABUFF )  TEXT5, TEXT6, TEXT7                    M42.2265
      IF( TEXT5 .NE. TEXT4 )  GO TO 4018                                M42.2266
      IF( TEXT6 .NE. TEXT4 )  GO TO 4018                                M42.2267
      IF( TEXT7 .NE. TEXT4 )  GO TO 4018                                M42.2268
      IF ( NOUTPR  .EQ.  0 )                                            M20.1029
     1 WRITE (KUNIT6, 4017)                                             M35. 777
 4017 FORMAT(  45H+BLANK CARD TERMINATING SHUNT R-L-C BRANCHES.   )     M42.2269
      GO TO 76540                                                       M42.2270
 4018 CONTINUE                                                          M42.2271
      DECODE(80,76515,ABUFF)NODE1(MBR1),NODE2(MBR1),TRSHUN(MBR1),       M42.2272
     1TXSHUN(MBR1),CSHUN(MBR1)                                          M42.2273
76515 FORMAT(2X,2I6,12X,3E6.2)                                          M42.2274
      IF ( NOUTPR  .EQ.  0 )                                            M20.1031
     1 WRITE(KUNIT6,76706)NODE1(MBR1),NODE2(MBR1 ),TRSHUN(MBR1),        M35. 778
     1TXSHUN(MBR1),CSHUN(MBR1)                                          M42.2275
76706 FORMAT ( 7H+SHUNT. ,  2X, 2I4, 3E11.3  )                          M42.2276
      GOTO 76516                                                        M42.2277
76540 MBR1=MBR1-1                                                       M42.2278
76513 IF(MSECT.LE.0)GOTO 76505                                          M42.2279
      ISTART=LOCZ11                                                     M42.2280
      DO 76517 I=1,NPHCAS                                               M42.2281
      IDUMY=I                                                           M42.2282
      IF(IDUMY.GT.3)IDUMY=3                                             M42.2283
      IEND=ISTART+IDUMY-1                                               M42.2284
C     READ INPUT CARD USING CIMAGE.                                     M42.2285
      CALL CIMAGE                                                       M42.2286
      DECODE(80,76518,ABUFF)ITYPE, (TR(J),TX(J),C(J), J=ISTART,IEND)    M29.1336
76518 FORMAT(I2,24X,9E6.2)                                              M42.2287
      IF ( I  .GT.  1 )   GO TO 5264                                    M42.2288
      IF ( NOUTPR  .EQ.  0 )                                            M20.1033
     1 WRITE (KUNIT6, 54777)  TR(ISTART), TX(ISTART), C(ISTART)         M35. 779
54777 FORMAT ( 15H+1ST OF PI-CKT.  , 1X, 3E11.3  )                      M42.2289
      GO TO 5271                                                        M42.2290
 5264 IDUMY = ISTART + 1                                                M42.2291
      IF ( NOUTPR  .EQ.  0 )                                            M20.1035
     1 WRITE (KUNIT6, 54104)  TR(IDUMY), TX(IDUMY), C(IDUMY),           M35. 780
     2                        TR(IDUMY+1), TX(IDUMY+1)                  M20.1037
54104 FORMAT ( 1H+, 5E10.3 )                                            M42.2292
 5271 ISTART = IEND + 1                                                 M42.2293
      IDUMY=I                                                           M42.2294
76520 IDUMY=IDUMY-3                                                     M42.2295
      IF(IDUMY.LE.0)GOTO 76517                                          M42.2296
      IEND=ISTART+IDUMY-1                                               M42.2297
      IF(IDUMY.GT.3)IEND=ISTART+2                                       M42.2298
C     READ INPUT CARD USING CIMAGE.                                     M42.2299
      CALL CIMAGE                                                       M42.2300
      DECODE(80,76519,ABUFF) (TR(J),TX(J),C(J), J=ISTART,IEND)          M29.1337
76519 FORMAT(26X,9E6.2)                                                 M42.2301
      IENDD=IEND                                                        M42.2302
      IF(IDUMY.GT.3)IENDD=ISTART+1                                      M42.2303
      IF ( NOUTPR  .EQ.  0 )                                            M20.1038
     1 WRITE (KUNIT6, 54104)  TR(ISTART), TX(ISTART), C(ISTART),        M35. 781
     1 TR(ISTART+1), TX(ISTART+1)                                       M42.2304
      ISTART=IEND+1                                                     M42.2305
      GOTO 76520                                                        M42.2306
76517 CONTINUE                                                          M42.2307
      DO 76522 I=LOCZ11,LOCZNN                                          M42.2308
      TX(I)=FREQCS*TX(I)/FREQX                                          M42.2309
76522 C(I)=FREQCS*C(I)/(2.0*FREQC)                                      M42.2310
      CALL CXRED(TR(LOCZ11),TX(LOCZ11),NPHCAS,LL0)                      M22.2620
      DO 76551 I=LOCZ11,LOCZNN                                          M42.2311
      TR(I)=-TR(I)                                                      M42.2312
76551 TX(I)=-TX(I)                                                      M42.2313
76505 IF ( MSER  .EQ.  0 )   GO TO 6434                                 M18. 314
      DO 76435 I=1,NPHCAS                                               M42.2314
      DO 76435 J=1,NPHCAS                                               M42.2315
      NPOS=LOCATN(I,J+NPHCAS)                                           M42.2316
      NNPOS=LOCATN(I,J+NPOS2)                                           M42.2317
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2318
      CASLNR(NPOS)=0.0                                                  M42.2319
      CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2320
76435 CASLNX(NPOS)=0.0                                                  M42.2321
      DO 76436 I=1,NPHCAS                                               M42.2322
      DO 76436 J=I,NPHCAS                                               M42.2323
      NPOS=LOCATN(I+NPHCAS,J+NPHCAS)                                    M42.2324
      NNPOS=LOCATN(I+NPOS2,J+NPOS2)                                     M42.2325
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2326
      CASLNR(NPOS)=0.0                                                  M42.2327
      CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2328
76436 CASLNX(NPOS)=0.0                                                  M42.2329
      NREDCT=NPHCAS                                                     M42.2330
      DO 76442 IPH=1,NPHCAS                                             M42.2331
      IPHASE=MAPCAS(IPH)                                                M42.2332
      IF(TRSER(IPHASE).EQ.0.0.AND.TXSER(IPHASE).EQ.0.0.AND.CSER(IPHASE) M42.2333
     1.EQ.0.0)GOTO 76438                                                M42.2334
      IF(TRSER(IPHASE).EQ. 999 999. )   GO TO 76441                     M05. 338
      IF(CSER(IPHASE).EQ.0.0)GOTO 76439                                 M42.2335
      YMAG2=TRSER(IPHASE)**2+(FREQCS*TXSER(IPHASE)/FREQX-1.0/           M42.2336
     1(FREQCS*CSER(IPHASE)/FREQC))**2                                   M42.2337
      YSERR=TRSER(IPHASE)/YMAG2                                         M42.2338
      YSERX=(-FREQCS*TXSER(IPHASE)/FREQX+1.0/(FREQCS*CSER(IPHASE)       M42.2339
     1/FREQC))/YMAG2                                                    M42.2340
      GOTO 76440                                                        M42.2341
76439 YSERR=TRSER(IPHASE)/(TRSER(IPHASE)**2+(FREQCS*TXSER(IPHASE)/FREQX)M42.2342
     1**2)                                                              M42.2343
      YSERX=(-FREQCS*TXSER(IPHASE)/FREQX)/(TRSER(IPHASE)**2+(FREQCS*    M42.2344
     1TXSER(IPHASE)/FREQX)**2)                                          M42.2345
      GOTO 76440                                                        M42.2346
76441 YSERR=0.0                                                         M42.2347
      YSERX=0.0                                                         M42.2348
76440 NNPOS=LOCATN(IPH+NPHCAS,IPH+NPHCAS)                               M42.2349
      CASLNR(NNPOS)=YSERR                                               M42.2350
      CASLNX(NNPOS)=YSERX                                               M42.2351
      NNPOS=LOCATN(IPH+NPHCAS,IPH+NPHCAS+NREDCT)                        M42.2352
      CASLNR(NNPOS)=-YSERR                                              M42.2353
      CASLNX(NNPOS)=-YSERX                                              M42.2354
      NNPOS=LOCATN(IPH+NPHCAS+NREDCT,IPH+NPHCAS+NREDCT)                 M42.2355
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSERR                                 M42.2356
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSERX                                 M42.2357
      GOTO 76442                                                        M42.2358
76438 IDUMY=NPHCAS+IPH                                                  M42.2359
      DO 76443 I=1,IDUMY                                                M42.2360
      NNPOS=LOCATN(I,IDUMY)                                             M42.2361
      NPOS=LOCATN(I,IDUMY+NREDCT)                                       M42.2362
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2363
76443 CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2364
      NNPOS=LOCATN(IDUMY,IDUMY)                                         M42.2365
      NPOS=LOCATN(IDUMY+NREDCT,IDUMY+NREDCT)                            M42.2366
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2367
      CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2368
      IEND=NPOS2 + NREDCT                                               M42.2369
      ISTART=IDUMY+NREDCT                                               M42.2370
      DO 76444 I=ISTART,IEND                                            M42.2371
      NNPOS=LOCATN(IDUMY,I)                                             M42.2372
      NPOS=LOCATN(IDUMY+NREDCT,I)                                       M42.2373
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2374
76444 CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2375
      ISTART=NPOS2+1                                                    M42.2376
      IEND=IDUMY+NREDCT                                                 M42.2377
      DO 76445 I=ISTART,IEND                                            M42.2378
      NNPOS=LOCATN(IDUMY,I)                                             M42.2379
      NPOS=LOCATN(I,IDUMY+NREDCT)                                       M42.2380
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2381
76445 CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2382
      ISTART=IDUMY+NREDCT+1                                             M42.2383
      IEND=NPOS2+NREDCT                                                 M42.2384
      DO 76446 I=ISTART,IEND                                            M42.2385
      DO 76446 J=I,IEND                                                 M42.2386
      NPOS=LOCATN(I,J)                                                  M42.2387
      NNPOS=NPOS-1                                                      M42.2388
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2389
76446 CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2390
      DO 76447 J=ISTART,IEND                                            M42.2391
      IENDD=J-1                                                         M42.2392
      DO 76447 I=1,IENDD                                                M42.2393
      NNPOS=LOCATN(I,IENDD)                                             M42.2394
      NPOS=LOCATN(I,J)                                                  M42.2395
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2396
76447 CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2397
      NREDCT=NREDCT-1                                                   M42.2398
76442 CONTINUE                                                          M42.2399
      IF (IPRSUP . LT. 9) GO TO 76630                                   M42.2400
      WRITE(LUNIT6,76605)                                               M17. 302
      WRITE(LUNIT6,76631)                                               M17. 303
76631 FORMAT( 5X,41HY-MATRIX BEFORE SERIES R-L-C ELIMINATION.     )     M42.2401
      DO 76632 I = 1,NPOS3                                              M42.2402
      WRITE(LUNIT6,76605)                                               M17. 304
      ISTART = LOCATN(I-1,I-1) +1                                       M42.2403
      IDUMY = I + 9                                                     M42.2404
76633 IDUMY = IDUMY - 9                                                 M42.2405
      IEND = IDUMY + ISTART - 1                                         M42.2406
      IF (IDUMY .GT. 9) IEND = ISTART + 8                               M42.2407
      WRITE(LUNIT6,76604)(CASLNR(II),II = ISTART,IEND)                  M17. 305
      WRITE(LUNIT6,76604)(CASLNX(II),II = ISTART,IEND)                  M17. 306
      ISTART = ISTART + 9                                               M42.2408
      IF (IDUMY .GT. 9) GO TO 76633                                     M42.2409
76632 CONTINUE                                                          M42.2410
76630 CONTINUE                                                          M42.2411
      CALL CXRED(CASLNR,CASLNX,NPOS2+NREDCT,NPOS2)                      M42.2412
      IF (IPRSUP . LT. 9) GO TO 76650                                   M42.2413
      WRITE(LUNIT6,76605)                                               M17. 307
      WRITE(LUNIT6,76653)                                               M17. 308
76653 FORMAT( 5X,41HY-MATRIX AFTER SERIES R-L-C ELIMINATION.     )      M42.2414
      DO 76656 I = 1,NPOS2                                              M42.2415
      WRITE(LUNIT6,76605)                                               M17. 309
      ISTART = LOCATN(I-1,I-1) +1                                       M42.2416
      IDUMY = I + 9                                                     M42.2417
76659 IDUMY = IDUMY - 9                                                 M42.2418
      IEND = IDUMY + ISTART - 1                                         M42.2419
      IF (IDUMY .GT. 9) IEND = ISTART + 8                               M42.2420
      WRITE(LUNIT6,76604)(CASLNR(II),II = ISTART,IEND)                  M17. 310
      WRITE(LUNIT6,76604)(CASLNX(II),II = ISTART,IEND)                  M17. 311
      ISTART = ISTART + 9                                               M42.2421
      IF (IDUMY .GT. 9) GO TO 76659                                     M42.2422
76656 CONTINUE                                                          M42.2423
76650 CONTINUE                                                          M42.2424
      ISTART=LOCATN(NPOS2,NPOS2)+1                                      M42.2425
      IEND=LOCATN(NPOS3,NPOS3)                                          M42.2426
      I = IEND - ISTART + 1                                             M42.2427
      CALL MOVER0 ( CASLNR(ISTART), I )                                 M42.2428
      CALL MOVER0 ( CASLNX(ISTART), I )                                 M42.2429
 6434 IF ( MBR  .EQ.  0 )   GO TO 76450                                 M18. 315
      DO 76451 JBR=1,MBR1                                               M42.2430
      IF ( NODE1(JBR)  .NE.  NODE2(JBR) )   GO TO 6715                  M18. 316
      LSTAT(19) = 6434                                                  M18. 317
      KILL=56                                                           M42.2431
      LSTAT(12)=NODE1(JBR)                                              M42.2432
      LSTAT(13) = MBR1                                                  M42.2433
      LSTAT(14) = JBR                                                   M42.2434
      GOTO9200                                                          M42.2435
 6715 IF ( IABS( NODE1(JBR) )  .LE.  NPHCAS )   GO TO 6716              M18. 318
      LSTAT(19) = 6715                                                  M18. 319
      KILL=54                                                           M42.2436
      LSTAT(12)=NODE1(JBR)                                              M42.2437
      GOTO9200                                                          M42.2438
 6716 IF ( IABS( NODE2(JBR) )  .LE.  NPHCAS )   GO TO 76717             M18. 320
      LSTAT(19) = 6716                                                  M18. 321
      KILL=54                                                           M42.2439
      LSTAT(12)=NODE2(JBR)                                              M42.2440
      GOTO9200                                                          M42.2441
76717 IF(TRSHUN(JBR).NE.0.0.OR.TXSHUN(JBR).NE.0.0.OR.CSHUN(JBR).NE.0.0) M42.2442
     1 GO TO 6718                                                       M18. 322
      LSTAT(19) = 6718                                                  M18. 323
      KILL=57                                                           M42.2443
      LSTAT(12)=NODE1(JBR)                                              M42.2444
      LSTAT(13)=NODE2(JBR)                                              M42.2445
      GOTO9200                                                          M42.2446
 6718 IF ( CSHUN(JBR)  .EQ.  0.0 )   GO TO 76452                        M18. 324
      YMAG2=TRSHUN(JBR)**2+(FREQCS*TXSHUN(JBR)/FREQX-1.0/(CSHUN(JBR)*   M42.2447
     1FREQCS/FREQC))**2                                                 M42.2448
      YSHUNR=TRSHUN(JBR)/YMAG2                                          M42.2449
      YSHUNX=(-FREQCS*TXSHUN(JBR)/FREQX+1.0/(FREQCS*CSHUN(JBR)/FREQC )) M42.2450
     1/YMAG2                                                            M42.2451
      IF( IPRSUP .GE. 6 )                                               M42.2452
     1 WRITE(LUNIT6, 76721)  JBR, MBR1, NODE1(JBR), NODE2(JBR),         M17. 316
     2 TRSHUN(JBR), TXSHUN(JBR), CSHUN(JBR),                            M42.2453
     3 FREQX, FREQC, FREQCS, YMAG2, YSHUNR, YSHUNX                      M42.2454
76721 FORMAT( /,  46H SHUNT BRANCH ADMITTANCE CALCULATION AT 76721.  ,  M42.2455
     1 4I10, /, ( 1X, 3E30.8 ) )                                        M42.2456
      GOTO76453                                                         M42.2457
76452 YMAG2= TRSHUN(JBR)**2+(FREQCS*TXSHUN(JBR)/FREQX)**2               M42.2458
      YSHUNR=TRSHUN(JBR)/YMAG2                                          M42.2459
      YSHUNX=(-FREQCS*TXSHUN(JBR)/FREQX)/YMAG2                          M42.2460
      IF( IPRSUP .GE. 6 )                                               M42.2461
     1 WRITE(LUNIT6, 76721)  JBR, MBR1, NODE1(JBR), NODE2(JBR),         M17. 317
     2 TRSHUN(JBR), TXSHUN(JBR), CSHUN(JBR),                            M42.2462
     3 FREQX, FREQC, FREQCS, YMAG2, YSHUNR, YSHUNX                      M42.2463
76453 IF(NODE1(JBR).GE.0.AND.NODE2(JBR).GE.0)GOTO 76454                 M42.2464
      IF(NODE1(JBR).LE.0.AND.NODE2(JBR).LE.0)GOTO 76455                 M42.2465
      IF(NODE1(JBR).GT.0)GOTO 76456                                     M42.2466
      IDUMY=NODE1(JBR)                                                  M42.2467
      NODE1(JBR)=NODE2(JBR)                                             M42.2468
      NODE2(JBR)=IDUMY                                                  M42.2469
76456 NODE2(JBR)=-NODE2(JBR)                                            M42.2470
      IDUMY=NODE1(JBR)                                                  M42.2471
      NROW1=NPHCAS+MAPINV(IDUMY)                                        M42.2472
      NROW2=NPOS2+NODE2(JBR)                                            M42.2473
      NNPOS=LOCATN(NROW1,NROW1)                                         M42.2474
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSHUNR                                M42.2475
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSHUNX                                M42.2476
      NNPOS=LOCATN(NROW1,NROW2)                                         M42.2477
      CASLNR(NNPOS)=-YSHUNR                                             M42.2478
      CASLNX(NNPOS)=-YSHUNX                                             M42.2479
      NNPOS=LOCATN(NROW2,NROW2)                                         M42.2480
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSHUNR                                M42.2481
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSHUNX                                M42.2482
      NODE2(JBR)=-NODE2(JBR)                                            M42.2483
      GOTO 76451                                                        M42.2484
76454 IF(NODE1(JBR).EQ.0.OR.NODE2(JBR).EQ.0)GOTO76457                   M42.2485
      IDUMY=NODE1(JBR)                                                  M42.2486
      IDUMY2=NODE2(JBR)                                                 M42.2487
      IF(MAPINV(IDUMY).LT.MAPINV(IDUMY2))GOTO 76458                     M42.2488
      IDUMY=NODE2(JBR)                                                  M42.2489
      IDUMY2=NODE1(JBR)                                                 M42.2490
76458 NROW1=NPHCAS+MAPINV(IDUMY)                                        M42.2491
      NROW2=NPHCAS+MAPINV(IDUMY2)                                       M42.2492
      NNPOS=LOCATN(NROW1,NROW1)                                         M42.2493
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSHUNR                                M42.2494
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSHUNX                                M42.2495
      NNPOS=LOCATN(NROW1,NROW2)                                         M42.2496
      CASLNR(NNPOS)=CASLNR(NNPOS)-YSHUNR                                M42.2497
      CASLNX(NNPOS)=CASLNX(NNPOS)-YSHUNX                                M42.2498
      NNPOS=LOCATN(NROW2,NROW2)                                         M42.2499
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSHUNR                                M42.2500
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSHUNX                                M42.2501
      GOTO 76451                                                        M42.2502
76457 IDUMY=NODE1(JBR)+NODE2(JBR)                                       M42.2503
      NROW1=NPHCAS+MAPINV(IDUMY)                                        M42.2504
      NNPOS=LOCATN(NROW1,NROW1)                                         M42.2505
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSHUNR                                M42.2506
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSHUNX                                M42.2507
      GOTO 76451                                                        M42.2508
76455 NODE1(JBR)=-NODE1(JBR)                                            M42.2509
      NODE2(JBR)=-NODE2(JBR)                                            M42.2510
      IF(NODE1(JBR).EQ.0.OR.NODE2(JBR).EQ.0)GOTO 76460                  M42.2511
      IF(NODE1(JBR).LT.NODE2(JBR))GOTO 76459                            M42.2512
      IDUMY=NODE1(JBR)                                                  M42.2513
      NODE1(JBR)=NODE2(JBR)                                             M42.2514
      NODE2(JBR)=IDUMY                                                  M42.2515
76459 NROW1=NPOS2+NODE1(JBR)                                            M42.2516
      NROW2=NPOS2+NODE2(JBR)                                            M42.2517
      NNPOS=LOCATN(NROW1,NROW1)                                         M42.2518
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSHUNR                                M42.2519
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSHUNX                                M42.2520
      NNPOS=LOCATN(NROW1,NROW2)                                         M42.2521
      CASLNR(NNPOS)=CASLNR(NNPOS)-YSHUNR                                M42.2522
      CASLNX(NNPOS)=CASLNX(NNPOS)-YSHUNX                                M42.2523
      NNPOS=LOCATN(NROW2,NROW2)                                         M42.2524
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSHUNR                                M42.2525
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSHUNX                                M42.2526
      NODE1(JBR)=-NODE1(JBR)                                            M42.2527
      NODE2(JBR)=-NODE2(JBR)                                            M42.2528
      GOTO 76451                                                        M42.2529
76460 NROW1=NPOS2+NODE1(JBR)+NODE2(JBR)                                 M42.2530
      NNPOS=LOCATN(NROW1,NROW1)                                         M42.2531
      CASLNR(NNPOS)=CASLNR(NNPOS)+YSHUNR                                M42.2532
      CASLNX(NNPOS)=CASLNX(NNPOS)+YSHUNX                                M42.2533
      NODE1(JBR)=-NODE1(JBR)                                            M42.2534
      NODE2(JBR)=-NODE2(JBR)                                            M42.2535
76451 CONTINUE                                                          M42.2536
      NREDCT=0                                                          M42.2537
      DO 76461 I=1,NPHCAS                                               M42.2538
      NPOS=LOCATN(NPOS2+I-1,NPOS2+I-1)                                  M42.2539
      NNPOS=LOCATN(NPOS2+I,NPOS2+I)                                     M42.2540
      YMAG2=ABSZ(CASLNR(NNPOS))+ABSZ(CASLNX(NNPOS))                     M18. 325
      IF(YMAG2.EQ.0.0)GO TO 76461                                       M42.2541
      YMAG2=ABSZ(CASLNR(NPOS))+ABSZ(CASLNX(NPOS))                       M18. 326
      IF ( YMAG2  .NE.  0.0 )  GO TO 6470                               M18. 327
      LSTAT(19) = 6470                                                  M18. 328
      KILL= 60                                                          M42.2542
      LSTAT(12)=-I                                                      M42.2543
      GO TO 9200                                                        M42.2544
 6470 NREDCT = NREDCT + 1                                               M18. 329
76461 CONTINUE                                                          M42.2545
      IF (IPRSUP . LT. 9) GO TO 76640                                   M42.2546
      WRITE(LUNIT6,76605)                                               M17. 319
      WRITE(LUNIT6,76634)                                               M17. 320
76634 FORMAT(5X,41HY-MATRIX BEFORE SHUNT R-L-C ELIMATION.   )           M42.2547
      DO 76636 I = 1,NPOS3                                              M42.2548
      WRITE(LUNIT6,76605)                                               M17. 321
      ISTART = LOCATN(I-1,I-1) +1                                       M42.2549
      IDUMY = I + 9                                                     M42.2550
76638 IDUMY = IDUMY - 9                                                 M42.2551
      IEND = IDUMY + ISTART - 1                                         M42.2552
      IF (IDUMY .GT. 9) IEND = ISTART + 8                               M42.2553
      WRITE(LUNIT6,76604)(CASLNR(II),II = ISTART,IEND)                  M17. 322
      WRITE(LUNIT6,76604)(CASLNX(II),II = ISTART,IEND)                  M17. 323
      ISTART = ISTART + 9                                               M42.2554
      IF (IDUMY .GT. 9) GO TO 76638                                     M42.2555
76636 CONTINUE                                                          M42.2556
76640 CONTINUE                                                          M42.2557
      CALL CXRED(CASLNR,CASLNX,NPOS2+NREDCT,NPOS2)                      M42.2558
      IF (IPRSUP . LT. 9) GO TO 76651                                   M42.2559
      WRITE(LUNIT6,76605)                                               M17. 324
      WRITE(LUNIT6,76654)                                               M17. 325
76654 FORMAT(5X,38HY-MATRIX AFTER SHUNT R-L-C ELIMATION.   )            M42.2560
      DO 76657 I = 1,NPOS2                                              M42.2561
      WRITE(LUNIT6,76605)                                               M17. 326
      ISTART = LOCATN(I-1,I-1) +1                                       M42.2562
      IDUMY = I + 9                                                     M42.2563
76660 IDUMY = IDUMY - 9                                                 M42.2564
      IEND = IDUMY + ISTART - 1                                         M42.2565
      IF (IDUMY .GT. 9) IEND = ISTART + 8                               M42.2566
      WRITE(LUNIT6,76604)(CASLNR(II),II = ISTART,IEND)                  M17. 327
      WRITE(LUNIT6,76604)(CASLNX(II),II = ISTART,IEND)                  M17. 328
      ISTART = ISTART + 9                                               M42.2567
      IF (IDUMY .GT. 9) GO TO 76660                                     M42.2568
76657 CONTINUE                                                          M42.2569
76651 CONTINUE                                                          M42.2570
      ISTART=LOCATN(NPOS2,NPOS2)+1                                      M42.2571
      IEND=LOCATN(NPOS3,NPOS3)                                          M42.2572
      I = IEND - ISTART + 1                                             M42.2573
      CALL MOVER0 ( CASLNR(ISTART), I )                                 M42.2574
      CALL MOVER0 ( CASLNX(ISTART), I )                                 M42.2575
76450 DO 76463 I=1,NPHCAS                                               M42.2576
      DO 76463 J=1,NPHCAS                                               M42.2577
      NPOS=LOCATN(I,J+NPHCAS)                                           M42.2578
      NNPOS=LOCATN(I,J+NPOS2)                                           M42.2579
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2580
      CASLNR(NPOS)=0.0                                                  M42.2581
      CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2582
76463 CASLNX(NPOS)=0.0                                                  M42.2583
      DO 76464 I=1,NPHCAS                                               M42.2584
      DO 76464 J=I,NPHCAS                                               M42.2585
      NPOS=LOCATN(I+NPHCAS,J+NPHCAS)                                    M42.2586
      NNPOS=LOCATN(I+NPOS2,J+NPOS2)                                     M42.2587
      CASLNR(NNPOS)=CASLNR(NPOS)                                        M42.2588
      CASLNR(NPOS)=0.0                                                  M42.2589
      CASLNX(NNPOS)=CASLNX(NPOS)                                        M42.2590
76464 CASLNX(NPOS)=0.0                                                  M42.2591
      DO 76465 I=1,NPHCAS                                               M42.2592
      DO 76465 J=I,NPHCAS                                               M42.2593
      NPOS=LOCATN(MAPCAS(I),MAPCAS(J))+LOCZ11-1                         M42.2594
      IF(MAPCAS(I).GT.MAPCAS(J))NPOS=LOCATN(MAPCAS(J),MAPCAS(I))+       M42.2595
     1LOCZ11-1                                                          M42.2596
      NNPOS=LOCATN(I+NPHCAS,J+NPHCAS)                                   M42.2597
      CASLNR(NNPOS)=TR(NPOS)/DSECTJ                                     M42.2598
      CASLNX(NNPOS)=TX(NPOS)/DSECTJ+C(NPOS)*DSECTJ                      M42.2599
      NNPOS=LOCATN(I+NPHCAS,J+NPOS2)                                    M42.2600
      CASLNR(NNPOS)=-TR(NPOS)/DSECTJ                                    M42.2601
      CASLNX(NNPOS)=-TX(NPOS)/DSECTJ                                    M42.2602
      IF(I.EQ.J)GOTO 76466                                              M42.2603
      NNPOS=LOCATN(J+NPHCAS,I+NPOS2)                                    M42.2604
      CASLNR(NNPOS)=-TR(NPOS)/DSECTJ                                    M42.2605
      CASLNX(NNPOS)=-TX(NPOS)/DSECTJ                                    M42.2606
76466 NNPOS=LOCATN(I+NPOS2,J+NPOS2)                                     M42.2607
      CASLNR(NNPOS)=TR(NPOS)/DSECTJ+CASLNR(NNPOS)                       M42.2608
76465 CASLNX(NNPOS)=TX(NPOS)/DSECTJ+C(NPOS)*DSECTJ+CASLNX(NNPOS)        M42.2609
      IF (IPRSUP . LT. 9) GO TO 76641                                   M42.2610
      WRITE(LUNIT6,76605)                                               M17. 329
      WRITE(LUNIT6,76635)                                               M17. 330
76635 FORMAT(5X,36HY-MATRIX BEFORE SECTION ELIMINATION.    )            M42.2611
      DO 76637 I = 1,NPOS3                                              M42.2612
      WRITE(LUNIT6,76605)                                               M17. 331
      ISTART = LOCATN(I-1,I-1) +1                                       M42.2613
      IDUMY = I + 9                                                     M42.2614
76639 IDUMY = IDUMY - 9                                                 M42.2615
      IEND = IDUMY + ISTART - 1                                         M42.2616
      IF (IDUMY .GT. 9) IEND = ISTART + 8                               M42.2617
      WRITE(LUNIT6,76604)(CASLNR(II),II = ISTART,IEND)                  M17. 332
      WRITE(LUNIT6,76604)(CASLNX(II),II = ISTART,IEND)                  M17. 333
      ISTART = ISTART + 9                                               M42.2618
      IF (IDUMY .GT. 9) GO TO 76639                                     M42.2619
76637 CONTINUE                                                          M42.2620
76641 CONTINUE                                                          M42.2621
      CALL CXRED(CASLNR,CASLNX,NPOS3,NPOS2)                             M42.2622
      IF (IPRSUP . LT. 9) GO TO 76652                                   M42.2623
      WRITE(LUNIT6,76605)                                               M17. 334
      WRITE(LUNIT6,76655)                                               M17. 335
76655 FORMAT(5X,36HY-MATRIX AFTER SECTION ELIMINATION.    )             M42.2624
      DO 76658 I = 1,NPOS2                                              M42.2625
      WRITE(LUNIT6,76605)                                               M17. 336
      ISTART = LOCATN(I-1,I-1) +1                                       M42.2626
      IDUMY = I + 9                                                     M42.2627
76661 IDUMY = IDUMY - 9                                                 M42.2628
      IEND = IDUMY + ISTART - 1                                         M42.2629
      IF (IDUMY .GT. 9) IEND = ISTART + 8                               M42.2630
      WRITE(LUNIT6,76604)(CASLNR(II),II = ISTART,IEND)                  M17. 337
      WRITE(LUNIT6,76604)(CASLNX(II),II = ISTART,IEND)                  M17. 338
      ISTART = ISTART + 9                                               M42.2631
      IF (IDUMY .GT. 9) GO TO 76661                                     M42.2632
76658 CONTINUE                                                          M42.2633
76652 CONTINUE                                                          M42.2634
      ISTART=LOCATN(NPOS2,NPOS2)+1                                      M42.2635
      IEND=LOCATN(NPOS3,NPOS3)                                          M42.2636
      I = IEND - ISTART + 1                                             M42.2637
      CALL MOVER0 ( CASLNR(ISTART), I )                                 M42.2638
      CALL MOVER0 ( CASLNX(ISTART), I )                                 M42.2639
      GO TO 76521                                                       M42.2640
76600 IF ( NOUTPR  .EQ.  0 )                                            M20.1040
     1 WRITE (KUNIT6, 76620)                                            M35. 782
76620 FORMAT(28H+TERMINATION OF CASCADED PI.)                           M42.2641
      LOCZNN=LOCATN(NPOS2,NPOS2)+LOCZ11-1                               M42.2642
      IT=LOCZ11+LOCATN(NPOS2,NPOS2)                                     M42.2643
      TMAX=-1.0                                                         M42.2644
      ICAS=0                                                            M42.2645
      DO 76601 I=LOCZ11,LOCZNN                                          M42.2646
      J=I-LOCZ11+1                                                      M42.2647
      TR(I)=CASLNR(J)                                                   M42.2648
      TX(I)=CASLNX(J)                                                   M42.2649
76601 C(I)=0.0                                                          M42.2650
      DO 76610 I = IPOINT, IBR                                          M39.1008
76610 KODEBR(I)=-2                                                      M42.2651
      IF(IPRSUP.LT.2)GOTO100                                            M42.2652
      WRITE(LUNIT6,76607)                                               M17. 340
76607 FORMAT(5X,46HY-MATRIX FOR LINE REPRESENTED BY CASCADED-PI.     )  M42.2653
      DO 76602 I=1,NPOS2                                                M42.2654
      WRITE(LUNIT6,76605)                                               M17. 341
76605 FORMAT(//,1X)                                                     M15. 553
      ISTART=LOCATN(I-1,I-1)+1                                          M42.2655
      IDUMY=I+9                                                         M42.2656
76603 IDUMY=IDUMY-9                                                     M42.2657
      IEND=IDUMY+ISTART-1                                               M42.2658
      IF(IDUMY.GT.9)IEND=ISTART+8                                       M42.2659
      WRITE(LUNIT6,76604)(CASLNR(II),II=ISTART,IEND)                    M17. 342
      WRITE(LUNIT6,76604)(CASLNX(II),II=ISTART,IEND)                    M17. 343
76604 FORMAT(1X,9E13.5)                                                 M42.2660
      ISTART=ISTART+9                                                   M42.2661
      IF(IDUMY.GT.9)GOTO76603                                           M42.2662
76602 CONTINUE                                                          M42.2663
  100 LASTOV = NCHAIN                                                   M15. 554
      NCHAIN = 2                                                        M15. 555
      IF ( IPRSUP  .GE.  1 )                                            M32.1250
     1 WRITE ( LUNIT6, 4568 )                                           M32.1251
 4568 FORMAT ( 23H  "EXIT  MODULE OVER3." )                             M32.1252
      GO TO 99999                                                       M42.2664
 9200 LASTOV = NCHAIN                                                   M15. 556
      NCHAIN = 51                                                       M15. 557
      LSTAT(18) = 3                                                     M42.2665
      IF ( IPRSUP  .GE.  1 )                                            M32.1253
     1 WRITE ( LUNIT6, 4568 )                                           M32.1254
99999 RETURN                                                            M42.2666
      END                                                               M42.2667
      SUBROUTINE CXRED(A,C,N,M)                                         M42.2668
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    ELIMINATION OF VARIABLES M+1,...N IN SYMMETRIC COMPLEX MATRIX WITHM15. 558
C)    A=REAL PART, C=IMAGINARY PART. A AND C ARE                        M15. 559
C)    STORED AS TRIANGLE (1 ELEMENT FOR 1.COLUMN,2 FOR 2.COLUMN ETC.).  M15. 560
C)    RESULT IS REDUCED MATRIX IN COLUMNS 1,...M IN CASE OF REDUCTION   M15. 561
C)    (M UNEQUAL 0) OR NEGATIVE INVERSE MATRIX IN COLUMNS 1,...N IN CASEM15. 562
C)    OF INVERSION (M=0).                                               M15. 563
      DIMENSION A(1),C(1),B(30),D(30)                                   M42.2669
      J = N + 1                                                         M42.2670
      W=1.0                                                             M42.2671
      IF(M.GT.0) W=-W                                                   M42.2672
      IJ=N*J/2                                                          M42.2673
    3 J=J-1                                                             M42.2674
      IF(J.EQ.M) RETURN                                                 M42.2675
      H1=A(IJ)                                                          M42.2676
      G1=C(IJ)                                                          M42.2677
      X=1.0/(H1*H1+G1*G1)                                               M42.2678
      H1=-H1*X                                                          M42.2679
      G1=G1*X                                                           M42.2680
      B(J)=H1                                                           M42.2681
      D(J)=G1                                                           M42.2682
      IJ=IJ-J                                                           M42.2683
      K=0                                                               M42.2684
      IK=0                                                              M42.2685
C                                   BEGIN K-LOOP                        M42.2686
    4 IK=IK+K                                                           M42.2687
      I1=IK+1                                                           M42.2688
      K=K+1                                                             M42.2689
      IF(K.GT.N) GO TO 3                                                M42.2690
      IF(K.LT.J) GO TO 9                                                M42.2691
      IF(W.LT.0.) GO TO 3                                               M42.2692
      IF(K.EQ.J) GO TO 7                                                M42.2693
      I=IK+J                                                            M42.2694
    5 H2=A(I)                                                           M42.2695
      G2=C(I)                                                           M42.2696
      B(K) = H2*H1-G2*G1                                                M42.2697
      D(K)=H2*G1+G2*H1                                                  M42.2698
C                                   BEGIN I-LOOP                        M42.2699
      I2=IK+K                                                           M42.2700
      L=0                                                               M42.2701
      DO 6 I=I1,I2                                                      M42.2702
      L=L+1                                                             M42.2703
      X=B(L)                                                            M42.2704
      Y=D(L)                                                            M42.2705
      A(I)=A(I)+X*H2-Y*G2                                               M42.2706
    6 C(I)=C(I)+X*G2+Y*H2                                               M42.2707
      IF(K.LT.J) GO TO 4                                                M42.2708
      I=IK+J                                                            M42.2709
      A(I)=B(K)                                                         M42.2710
      C(I)=D(K)                                                         M42.2711
      GO TO 4                                                           M42.2712
C                                   END I-LOOP                          M42.2713
    7 I=IJ                                                              M42.2714
      DO 8 L=1,J                                                        M42.2715
      I=I+1                                                             M42.2716
      C(I)=D(L)                                                         M42.2717
    8 A(I)=B(L)                                                         M42.2718
      GO TO 4                                                           M42.2719
C                                   END K-LOOP                          M42.2720
    9 I=IJ+K                                                            M42.2721
      GO TO 5                                                           M42.2722
      END                                                               M42.2723
      SUBROUTINE OVER4                                                  M39.1009
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE1.FOR;/NOLIST'                                           
      DIMENSION  BUFFIN (8)                                             M16. 545
C     FREQUENCY DEPENDENT LINE. ********************************        M29.1338
      IF ( IPRSUP  .GE.  1 )                                            M32.1255
     1 WRITE ( LUNIT6, 4567 )                                           M32.1256
 4567 FORMAT ( 23H  "BEGIN MODULE OVER4." )                             M32.1257
      JFDEP2 = 2 * LFDEP + IFDEP2                                       M35. 783
      KFDEP2 = 4 * LFDEP + IFDEP2                                       M35. 784
      D1TTTT = T                                                        M20.1042
      IF ( KBURRO  .EQ.  1 )   GO TO 8716                               M22.2624
      IOFA1P = ( LOCF(WEIGHT(1)) - LOCF(YKM(1)) ) / 4                   M42.2726
      GO TO 8721                                                        M22.2625
 8716 IOFA1P = LSIZ23 / 4                                               M22.2626
 8721 IOFTM2 = 2 * IOFA1P                                               M22.2627
      IOFTM1 = 3 * IOFA1P                                               M42.2727
      IFDEP = IFDEP + 1                                                 M42.2728
      IF (IFDEP.LE.LFDEP) GO TO 20170                                   M42.2729
      KILL = 1                                                          M15. 564
      LSTAT(16) = 13                                                    M15. 565
      LSTAT(19)=20170                                                   M42.2730
      GO TO 9200                                                        M15. 566
20170 NR(IBR) = IFDEP                                                   M42.2731
      LENGTH(IBR) = KCOUNT                                              M42.2732
      CK(IBR) = -CK(IBR)                                                M42.2733
      D1 = -TENM3 / 100.                                                M05. 339
      IF( CK(IBR)  .GT.  D1 )   CK(IBR) = D1                            M05. 340
      CI(IBR) = ABSZ( CI(IBR) )                                         M18. 330
C     READ INPUT CARD USING CIMAGE.                                     M42.2734
      CALL CIMAGE                                                       M42.2735
      DECODE(80, 6105, ABUFF)  NTIME1, NTIME2, LOCMAX, LOCMIN,          M42.2736
     1 Z1, CUT1, CUT2, N10, RTOTAL, NII                                 M29.1339
 6105 FORMAT ( 4I8, 3F8.0, I8, F8.0, I8 )                               M29.1340
      IF( CUT1  .LE. 0.0 )  CUT1  = 10. * TENM3                         M05. 341
      IF( CUT2  .LE. 0.0 )  CUT2  = 100. * TENM3                        M05. 342
      IF ( NII  .LE.  0 )    NII = 100                                  M29.1341
      IF ( NOUTPR  .EQ.  0 )                                            M20.1043
     1 WRITE (KUNIT6, 6110)                                             M35. 785
 6110 FORMAT( 40H+FREQUENCY-DEPENDENCE MISC. DATA VALUES.   )           M42.2737
      N9 = NOUTPR                                                       M22.2630
      NOUTPR = N10                                                      M22.2631
      N1 = NTIME1                                                       M13.1217
      IF ( NTIME2  .GT.  N1 )   N1 = NTIME2                             M13.1218
      IF ( N1+3  .LE.  IOFA1P )   GO TO 6103                            M13.1219
      KILL = 145                                                        M13.1220
      LSTAT(14) = NTIME1                                                M13.1221
      LSTAT(15) = NTIME2                                                M13.1222
      LSTAT(16) = IOFA1P                                                M13.1223
      LSTAT(19) = 6103                                                  M13.1224
      GO TO 9200                                                        M13.1225
 6103 N2 = IOFTM1 + 1                                                   M22.2632
      N3 = IOFA1P + 1                                                   M22.2633
      N12 = IOFTM1 + NTIME1                                             M22.2634
      GO TO 5736                                                        M22.2635
C     READ INPUT CARD USING CIMAGE.                                     M42.2738
 6111 CALL CIMAGE                                                       M42.2739
      DECODE (80, 6115, ABUFF)  BUFFIN                                  M16. 546
      DO 26111  J = 1, 7, 2                                             M16. 549
      TIM1 (N2) = BUFFIN(J)                                             M16. 550
      A1P(N3) = BUFFIN(J+1)                                             M16. 551
      N2 = N2 + 1                                                       M16. 552
26111 N3 = N3 + 1                                                       M16. 553
      IF ( NOUTPR  .EQ.  0 )                                            M20.1045
     1 WRITE (KUNIT6, 6112)                                             M35. 786
 6112 FORMAT( 33H+   WEIGHTING FUNCTION A1 POINTS.    )                 M42.2740
 5736 IF ( N2  .LE.  N12 )   GO TO 6111                                 M22.2636
      N4 = IOFTM2 + 1                                                   M22.2637
      N5 = 1                                                            M22.2638
      GO TO 5741                                                        M22.2639
C     READ INPUT CARD USING CIMAGE.                                     M42.2741
 6116 CALL CIMAGE                                                       M42.2742
      DECODE (80, 6115, ABUFF)  BUFFIN                                  M16. 554
      DO 26116  J = 1, 7, 2                                             M16. 557
      TIM2 (N4) = BUFFIN(J)                                             M16. 558
      A2P(N5) = BUFFIN(J+1)                                             M16. 559
      N4 = N4 + 1                                                       M16. 560
26116 N5 = N5 + 1                                                       M16. 561
 6115 FORMAT( 8F10.0 )                                                  M42.2743
      IF ( NOUTPR  .EQ.  0    .AND.                                     M35. 787
     1     N12  .GT.  0 )                                               M35. 788
     1 WRITE (KUNIT6, 6117)                                             M35. 789
 6117 FORMAT( 33H+   WEIGHTING FUNCTION A2 POINTS.    )                 M42.2744
 5741 N12 = NTIME2 - N5                                                 M22.2640
      IF ( N12  .LE.  7 )                                               M22.2641
     1 NOUTPR = N9                                                      M22.2642
      IF ( N12  .GE.  0 )   GO TO 6116                                  M22.2643
      DO 6121  I=1, NTIME1                                              M22.2644
      N6 = IOFTM1 + I                                                   M16. 562
 6121 TIM1(N6) = TIM1(N6) / 1 000 000.                                  M16. 563
      DO 6122  I=1, NTIME2                                              M42.2745
      N7 = IOFTM2 + I                                                   M16. 564
 6122 TIM2(N7) = TIM2(N7) / 1 000 000.                                  M16. 565
      D3 = 0.0                                                          M42.2746
      DO 6125  I=2, NTIME1                                              M42.2747
      N2 = IOFTM1 + I                                                   M16. 566
      N4 = IOFA1P + I                                                   M16. 567
      D1 = TIM1(N2) - TIM1(N2-1)                                        M16. 568
      D2 = A1P(N4) + A1P(N4-1)                                          M16. 569
      IF( D1 .GT. 0.0 )  GO TO 6125                                     M42.2748
      KILL = 45                                                         M42.2749
      LSTAT(19) = 6125                                                  M29.1342
      LSTAT(17) = 1                                                     M29.1343
      LSTAT(14) = I                                                     M42.2750
      FLSTAT(15) = TIM1(N2)                                             M16. 570
      FLSTAT(16) = TIM1(N2-1)                                           M16. 571
      GO TO 9200                                                        M42.2751
 6125 D3 = D3 + D1 * D2                                                 M32.1258
      DO 6126  I=2, NTIME2                                              M42.2752
      N6 = IOFTM2 + I                                                   M16. 572
      D1 = TIM2(N6) - TIM2(N6-1)                                        M16. 573
      D2 = A2P(I) + A2P(I-1)                                            M42.2753
      IF( D1 .GT. 0.0 )  GO TO 6126                                     M42.2754
      KILL = 45                                                         M29.1345
      LSTAT(19) = 6126                                                  M29.1346
      LSTAT(17) = 2                                                     M29.1347
      LSTAT(14) = I                                                     M42.2755
      FLSTAT(15) = TIM2(N6)                                             M16. 574
      FLSTAT(16) = TIM2(N6-1)                                           M16. 575
      GO TO 9200                                                        M42.2756
 6126 D3 = D3 + D1 * D2                                                 M32.1259
      D3 = D3 * ONEHAF                                                  M32.1260
      IF ( NOUTPR  .EQ.  0 )                                            M20.1049
     1 WRITE (KUNIT6, 6129)  D3                                         M35. 790
 6129 FORMAT( 33H+   WEIGHTING FUNCTION A2 POINTS.,  F16.8 )            M35. 791
      IF ( RTOTAL  .GT.  0.0 )   GO TO 7820                             M29.1349
      D1 = 1.0 / D3                                                     M42.2757
      DO 6135  I=1, NTIME1                                              M42.2758
      N2 = IOFA1P + I                                                   M16. 576
 6135 A1P(N2) = A1P(N2) * D1                                            M16. 577
      DO 6136  I=1, NTIME2                                              M42.2759
 6136 A2P(I) = A2P(I) * D1                                              M42.2760
 7820 CUT2 = CUT2 * A2P(LOCMIN)                                         M29.1350
      ATHEO = RTOTAL / ( 2.0 * Z1 + RTOTAL )                            M29.1351
      IEND = NTIME2                                                     M42.2761
      DZERO = 0.0                                                       M42.2762
      I = 1                                                             M42.2763
      KFIN = 0                                                          M42.2764
      N = 0                                                             M42.2765
      TEND = 0.0                                                        M42.2766
 6200 A1 = AP(I)                                                        M42.2767
      ADITN = 0.0                                                       M29.1352
      N3 = IOFTM2 + I                                                   M16. 578
      N4 = IOFTM2 + IEND                                                M16. 579
      T1 = TP(N3)                                                       M16. 580
      TBOUND = TP(N4)                                                   M16. 581
      IWTENT(IFDEP2) = JST1                                             M42.2768
      WEIGHT(JST1) = 0.0                                                M42.2769
      N = N - 1                                                         M42.2770
      KFLOP = 1                                                         M42.2771
      MODE  = 0                                                         M42.2772
      IF ( IPRSUP  .GE.  1 )                                            M22.2645
     1 WRITE (LUNIT6, 6213)  IFDEP, IFDEP2, JST, I, IOFTM2, IEND,       M22.2646
     2                       DZERO, DELTAT                              M22.2647
 6213 FORMAT ( /,  25H BEGIN  A(T)  PROCESSING.,                        M22.2648
     1 48H   IFDEP  IFDEP2     JST       I  IOFTM2    IEND,             M22.2649
     2 11X,  5HDZERO,  10X,  6HDELTAT  ,/,  25X,  6I8,  2E16.7  )       M22.2650
      GO TO 6450                                                        M42.2773
 6220 IF( MODE .GT. 0 )  GO TO 6240                                     M42.2774
      IF( T .LE. TEND )  GO TO 6240                                     M42.2775
      KFLOP = 1                                                         M42.2776
      D1 = (A-A1) / (T-T1)                                              M42.2777
      A = A1 + D1*(TEND-T1)                                             M42.2778
      I = I - 1                                                         M42.2779
      T = TEND                                                          M42.2780
      GO TO 6250                                                        M42.2781
 6240 IF( I .LT. IEND )  GO TO 6250                                     M42.2782
      IF ( IPRSUP  .LE.  0 )   GO TO 6245                               M22.2651
      WRITE (LUNIT6, 6243)  I, IEND, JST, MODE, KFLOP, LWT,             M22.2652
     1                      KFIN, N, IFDEP, IFDEP2, IOFTM2,             M22.2653
     2                      LOCMIN, LOCMAX                              M22.2654
 6243 FORMAT ( /,  21H END RAW DATA POINTS.,                            M22.2655
     1 48H       I    IEND     JST    MODE   KFLOP     LWT,             M22.2656
     2 56H    KFIN       N   IFDEP  IFDEP2  IOFTM2  LOCMIN  LOCMAX  ,/, M22.2657
     3 21X,  13I8  )                                                    M22.2658
      WRITE (LUNIT6, 6244)  T1, A1, T, A, D5, D6, TEND, D3              M22.2659
 6244 FORMAT ( /,  15X,  2HT1,  14X,  2HA1,  15X,  1HT,  15X,           M22.2660
     1 1HA,  14X,  2HD5,  14X,  2HD6,  12X,  4HTEND,  14X,  2HD3   ,/,  M22.2661
     2 1X,  8E16.7  )                                                   M22.2662
 6245 I = -1                                                            M22.2663
      KFLOP = 1                                                         M42.2783
 6250 D1 = ( T - T1 ) * ONEHAF                                          M29.1353
      D18 = D1 * (A1 + A )                                              M29.1354
      D5 = D5 + D18                                                     M29.1355
      D6 = D6 + D1 * ( T1*A1 + T*A )                                    M42.2784
      T1 = T                                                            M42.2785
      A1 = A                                                            M42.2786
      IF( MODE .EQ. 0 )  GO TO 6260                                     M42.2787
      IF( I .GT. 0 )  GO TO 6450                                        M42.2788
      GO TO 6465                                                        M42.2789
 6260 ADITN = ADITN  + D18                                              M29.1356
      IF( KFLOP .EQ. 0 )  GO TO 6450                                    M29.1357
      D6 = D6 / DELTAT                                                  M42.2790
      IF ( IPRSUP  .GE.  4 )                                            M22.2664
     1 WRITE (LUNIT6, 6316)  I, JST, MODE, N, T, A, D5, D6              M22.2665
 6316 FORMAT ( /,  17H DONE WITH PANEL.,                                M22.2666
     1 32H       I     JST    MODE       N,  15X,  1HT,  15X,           M22.2667
     2 1HA,  14X,  2HD5,  14X,  2HD6  ,/,  17X,  4I8,  4E16.7  )        M22.2668
      IF( DZERO .NE. 0.0 )  GO TO 6320                                  M42.2791
      DZERO = D5                                                        M42.2792
      D3 = DZERO - D6                                                   M42.2793
      D4  = 1.0 + D3                                                    M42.2794
      D5  = 1.0 - D3                                                    M42.2795
      ETA(IFDEP) = D4 / D5                                              M42.2796
      D3 = 1.0 / D4                                                     M42.2797
      WEIGHT(JST) = D6 * D3                                             M42.2798
      GO TO 6455                                                        M42.2799
C     THE FOLLOWING CHANGES RESTORE WEIGHTING FUNCTIONS TO THE          M24. 604
C     WAY THEY WERE IN  "M21." .   REMOVE THE  "M22."  CHANGE           M24. 605
C     WHICH HAS (WE SUSPECT) LEAD TO INSTABILITY FOR AL'S CASES.        M24. 606
 6320 D1 = D6 - N * D5                                                  M24. 607
      D2 = (D5 - D1 ) * D3                                              M42.2800
      D1 = D1 * D3                                                      M24. 608
      WEIGHT(JST) = WEIGHT(JST) + D2                                    M42.2801
      IF( N .LE. 5    .AND.    IPRSUP .GE. 2 )                          M42.2802
     1WRITE(LUNIT6, 6323)  D1, D2, D3, WEIGHT(JST)                      M17. 350
 6323 FORMAT ( /,  23H ACCUMULATE  'WEIGHT' .,  4X,  12HWEIGHT(JST1),   M22.2670
     1 14X,  2HD2,  14X,  2HD3,  5X,  11HWEIGHT(JST)  ,/,               M22.2671
     2 23X,  4E16.7  )                                                  M22.2672
      WEIGHT(JST1) = D1                                                 M42.2803
      IF( KFIN .GT. 0 )  GO TO 6340                                     M42.2804
      IF( I .LE. LOCMIN )  GO TO 6450                                   M42.2805
      IF( A .LT. CUT2 )  GO TO 6450                                     M42.2806
      GO TO 6410                                                        M42.2807
 6340 IF( I .LE. LOCMAX )  GO TO 6450                                   M42.2808
      IF( A .GT. CUT1 )  GO TO 6450                                     M42.2809
 6410 MODE  = 1                                                         M42.2810
      TAILS  = T                                                        M42.2811
      AAMP = A                                                          M29.1358
 6450 IF( KFLOP .EQ. 0 )  GO TO 6460                                    M42.2812
      JST = JST1                                                        M42.2813
      JST1 = JST1 + 1                                                   M42.2814
      IF( JST .LE. LWT )  GO TO 6455                                    M42.2815
      KILL = 1                                                          M15. 567
      LSTAT(16) = 14                                                    M15. 568
      LSTAT(19)= 6455                                                   M42.2816
      GO TO 9200                                                        M15. 569
 6455 IF( I .LT. 0 )  GO TO 6470                                        M42.2817
      KFLOP = 0                                                         M42.2818
      D5 = 0.0                                                          M42.2819
      D6 = 0.0                                                          M42.2820
      TEND = TEND + DELTAT                                              M42.2821
      N = N + 1                                                         M42.2822
 6460 I = I + 1                                                         M42.2823
      N5 = IOFTM2 + I                                                   M16. 582
      T = TP(N5)                                                        M16. 583
      A = AP(I)                                                         M42.2824
      GO TO 6220                                                        M42.2825
 6465 D1 = 1.0 / ( D6/D5 - TAILS )                                      M42.2826
      D2 = D1 * D5                                                      M42.2827
      IF( IPRSUP .GE. 2 )                                               M42.2828
     1WRITE(LUNIT6, 6466)  IFDEP2, D2, D1, TAILS, D5, D6                M17. 351
 6466 FORMAT ( /,  24H TAIL INTEGRAL COMPLETE.,   8H  IFDEP2,           M22.2673
     1 14X,  2HD2,  14X,  2HD1,  11X,  5HTAILS,  14X,  2HD5,            M22.2674
     2 14X,  2HD6  ,/,  24X,  I8,  5E16.7  )                            M22.2675
      CON1(IFDEP2) = D2 * D3                                            M35. 792
      CON1(JFDEP2) = D1                                                 M35. 793
      CON1(KFDEP2) = TAILS                                              M35. 794
      IF( RTOTAL .LE. 0.0 )   GO TO 6475                                M29.1359
      IF ( IPRSUP .GE. 1 )                                              M29.1360
     1 WRITE (LUNIT6, 7812) D3, AAMP, ADITN, CON1(IFDEP2), D1,          M35. 795
     2                      TAILS, IFDEP2                               M29.1362
 7812 FORMAT (  25H  BEFORE TAIL ADJUSTMENT.  ,/,                       M29.1363
     1  18X, 2HD3,  16X, 4HAAMP,  15X, 5HADITN,  15X, 5HTAILP,          M29.1364
     2  15X, 5HTAILQ,  15X, 5HTAILS,  4X, 6HIFDEP2  ,/,                 M29.1365
     3  1X,  6E20.11,  I10 )                                            M29.1366
      TCOR = ATHEO - ADITN                                              M29.1367
C     START NEWTON RAPHSON ITERATIONS ON TAILQ * * * * * * * * * * * * *M29.1368
      BCT = ABSZ ( TCOR / AAMP )                                        M29.1369
      BCT = ALOGZ ( BCT )                                               M29.1370
      D18 = 0.0                                                         M29.1371
      IK = 0                                                            M29.1372
 7843 IK = IK + 1                                                       M29.1373
      IF ( IK .GT. NII )  GO TO 7904                                    M29.1374
      D12 = D1 + D18                                                    M29.1375
      IF ( IK .EQ. 1 )  GO TO 7910                                      M29.1376
      D17 = ABSZ ( D18 ) / D1                                           M29.1377
      IF ( D17  .LE.  EPSILN )   GO TO 7905                             M29.1378
 7910 D1 = D12                                                          M29.1379
      FSTAR = -ALOGZ ( D1 ) - D1 * TAILS - BCT                          M29.1380
      FDER = 1.0 / D1 + TAILS                                           M29.1381
      D18 = FSTAR / FDER                                                M29.1382
      IF ( IPRSUP  .GE.  1 )                                            M29.1383
     1 WRITE (LUNIT6, 7912) IK, FSTAR, FDER, D18, D1, BCT               M29.1384
 7912 FORMAT ( 1X, 16HON ITERATION NO.,  I5  ,/,  5E22.12 )             M29.1385
      DO 7914  IKP = 1, 6                                               M29.1386
      IF ( ABSZ(D18)  .LT.  D1 )   GO TO 7843                           M29.1387
 7914 D18 = D18 / 5.0                                                   M29.1388
      IF ( ABSZ(D18) .LT. D1 ) GO TO 7843                               M29.1389
 7904 KILL = 46                                                         M29.1390
      LSTAT(19) = 7904                                                  M29.1391
      LSTAT(14) = NII                                                   M29.1392
      LSTAT(15) = IFDEP                                                 M29.1393
      FLSTAT(15) = RTOTAL                                               M29.1394
      GO TO 9200                                                        M29.1395
 7905 CON1(JFDEP2) = D1                                                 M35. 796
      CON1(IFDEP2) = TCOR * D1 * D3                                     M35. 797
      IF ( IPRSUP  .GE.  1 )                                            M29.1398
     1 WRITE (LUNIT6, 7913) IFDEP2, CON1(IFDEP2), CON1(JFDEP2)          M35. 798
 7913 FORMAT (  24H SUCCESSFUL TERMINATION.,  8H  IFDEP2,               M29.1400
     1   15X, 5HTAILP,  15X, 5HTAILQ  ,/,  24X,  I8,  2E20.12 )         M29.1401
      GO TO 6475                                                        M42.2829
 6470 CON1(IFDEP2) = 0.0                                                M35. 799
      CON1(JFDEP2) = 0.0                                                M35. 800
      CON1(KFDEP2) = 0.0                                                M35. 801
 6475 NPOINT = JST - IWTENT(IFDEP2)                                     M42.2830
      IFDEP2 = IFDEP2 + 1                                               M42.2831
      JFDEP2 = JFDEP2 + 1                                               M35. 802
      KFDEP2 = KFDEP2 + 1                                               M35. 803
      IF ( KFIN  .GT.  0 )   GO TO 6600                                 M42.2832
      KFIN = 1                                                          M42.2833
      N6 = IOFA1P + LOCMAX                                              M16. 584
      CUT1 = CUT1 * A1P(N6)                                             M16. 585
      N = TIM1(IOFTM1+1) / DELTAT                                       M42.2834
      ISKIP(IFDEP) = N                                                  M42.2835
      TEND = N * DELTAT                                                 M42.2836
      NHIST(IFDEP) = NPOINT                                             M42.2837
      I = IOFA1P                                                        M42.2838
      IEND = NTIME1 + I                                                 M42.2839
      LOCMAX = LOCMAX + I                                               M42.2840
      I = I + 1                                                         M42.2841
      ATHEO = (2.0*Z1) / (2.0*Z1 + RTOTAL)                              M29.1402
      GO TO 6200                                                        M42.2842
 6600 ZINF(IFDEP) = Z1                                                  M42.2843
      NPOINT = NPOINT   +   ( ISKIP(IFDEP) - 1 )                        M42.2844
      IF( IPRSUP .GE. 1 )                                               M42.2845
     1WRITE(LUNIT6, 6601)  IBR, JST, NPOINT, ISKIP(IFDEP), NR(IBR),     M17. 352
     1 NHIST(IFDEP)                                                     M42.2846
 6601 FORMAT ( /,  25H READY TO EXIT  'OVER4' .,  12X,  3HIBR,          M22.2676
     1 12X,  3HJST,  9X,  6HNPOINT,  3X,  12HISKIP(IFDEP),              M22.2677
     2 8X,  7HNR(IBR),  3X,  12HNHIST(IFDEP)  ,/,  25X,  6I15  )        M22.2678
      IF( NPOINT  .GT.  NHIST(IFDEP)  )   NHIST(IFDEP) = NPOINT         M42.2847
      NHIST(IFDEP) = NHIST(IFDEP) + 1                                   M42.2848
      ICHECK = 3                                                        M42.2849
      LASTOV = NCHAIN                                                   M39.1010
      NCHAIN = 2                                                        M42.2850
      T = D1TTTT                                                        M20.1051
      IF ( IPRSUP  .GE.  1 )                                            M32.1261
     1 WRITE ( LUNIT6, 4568 )                                           M32.1262
 4568 FORMAT ( 23H  "EXIT  MODULE OVER4." )                             M32.1263
      GO TO 99999                                                       M42.2851
 9200 LASTOV = NCHAIN                                                   M15. 571
      NCHAIN = 51                                                       M15. 572
      LSTAT(18) = 4                                                     M42.2852
      IF ( IPRSUP  .GE.  1 )                                            M32.1264
     1 WRITE ( LUNIT6, 4568 )                                           M32.1265
99999 RETURN                                                            M42.2853
      END                                                               M42.2854
