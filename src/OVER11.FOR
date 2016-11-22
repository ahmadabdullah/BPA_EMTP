      SUBROUTINE OVER11
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'
      COMMON /a8sw/ A8SW(400)                                           M39.1586
      COMMON / LINEMODEL / KEXACT, NSOLVE, FMINSV, NUMRUN,NPHLMT
      COMMON / LINEMODEL / CHAR80, CHLMFS(18)
      CHARACTER*6 CHLMFS     ! 9-PHASE AS LIMIT FOR LMFS TEST
      CHARACTER*80 CHAR80

      CHARACTER*6  CHHOLD
      DIMENSION  FSHOLD(30000)
      DIMENSION  VIM(1),  JCH2(1)                                       M35.1234
      EQUIVALENCE  ( VOLT(1), VIM(1) ),  ( IMFD(1), JCH2(1) )           M35.1235
      EQUIVALENCE  ( MONCAR(1), KNT )                                           
C      PRECEDING "JCH2" USES "IMFD" JUST FOR "FREQUENCY SCAN".          M35.1236
C      AS SUCH, THERE MUST BE NO FREQ-DEPEND SOURCES PRESENT.           M35.1237
      REAL*8          AUPPER, TEXT1, TEXT2, TEXT3, TEXT4                M30. 160
      DIMENSION AUPPER(13)                                                 10149
      DATA  TEXT1   /  6HMAG     /                                      M20.2251
      DATA  TEXT2   /  6HANGLE   /                                      M20.2252
      DATA  TEXT3   /  6HREAL    /                                      M30. 161
      DATA  TEXT4   /  6HIMAG    /                                      M30. 162
      DATA  TEXT5   /  6HPCTMAG  /
      DATA  TEXT6   /  6HPCTANG  /
      DATA  NFSOUT  /  0  /
      LOCATN(I,J)=(J*J-J)/2+I                                              10150
      LL2 = 2                                                           M35.1238
      IF ( IPRSUP  .GE.  1 )                                            M32.3416
     1 WRITE ( LUNIT6, 4567 )                                           M32.3417
 4567 FORMAT ( 24H  "BEGIN MODULE OVER11." )                            M32.3418
      IF ( IPRSUP  .GE.  3 )   KSSOUT = 1                               M31.2581
      LLM1 = -1                                                         M22.3257
      LL0 = 0                                                           M22.3258
      LL1 = 1                                                           M22.3259
      LL2 = 2                                                           M22.3260
      IB = 1                                                                    
      SFROLD = -FLTINF                                                          
      IF( KSSOUT .NE. 1  .AND.  KSSOUT .NE. 3 )   GO TO 6010            M33. 870
      N6 = 0                                                                    
      DO 1854  K=1, IBR                                                         
      J = IABS( KBUS(K) )                                                       
      IF ( J  .EQ.  1 )   J = IABS( MBUS(K) )                                   
      IF ( J  .LE.  1 )   GO TO 1854                                            
      M = IABS ( KSSFRQ(J) )                                                    
      IF ( N6  .LE.  0 )   GO TO 1846                                   M31.2595
      NDX1 = LSIZ26                                                     M31.2596
      DO 1841  I=1, N6                                                          
      NDX1 = NDX1 + 1                                                   M31.2598
      IF ( SFREQ(M)  .EQ.  VIM(NDX1) )   GO TO 1854                             
 1841 CONTINUE                                                          M31.2600
 1846 N6 = N6 + 1                                                       M31.2601
      NDX1 = LSIZ26 + N6                                                M31.2602
      VIM(NDX1) = SFREQ(M)                                              M31.2603
      IF ( N6  .LT.  LSIZ26 )   GO TO 1854                              M31.2604
      WRITE (LUNIT6, 1851)  LSIZ26                                      M31.2605
 1851 FORMAT (  37H NUMBER OF PHASOR FREQUENCIES EXCEEDS,               M31.2606
     1          37H LIST SIZE 26 (WORKING VECTOR USAGE).   )            M31.2607
      KILL = 1                                                          M31.2608
      LSTAT(19) = 1854                                                  M31.2609
      LSTAT(17) = 26                                                            
      GO TO 9800                                                        M32.3420
 1854 CONTINUE                                                          M31.2612
      IF ( KOL132  .EQ.  132 )                                          M32.3421
     1 WRITE (LUNIT6, 6003)  VIM(LSIZ26+1)                              M32.3422
 6003 FORMAT( //, 113H SINUSOIDAL STEADY STATE SOLUTION, BRANCH BY BRANC   10154
     1H.  ALL FLOWS ARE AWAY FROM BUS, AND REAL PART, MAGNITUDE, OR P  ,   10155
     2 /,  41H IS PRINTED ABOVE THE IMAGINARY PART, THE,                M37.3786
     3     42H ANGLE, OR Q.   FIRST SOLUTION FREQUENCY =,               M31.2615
     4   E18.9,   9H   HERTZ.   )                                       M31.2616
      IF ( KOL132  .NE.  132 )                                          M32.3423
     1 WRITE (LUNIT6, 6004)   VIM(LSIZ26+1)                             M32.3424
 6004 FORMAT ( /, 40H 80-COLUMN PHASOR BRANCH FLOWS.  REVERSE,          M32.3425
     1            27H FLOWS OMITTED.  1ST FREQ =,  E13.5                M32.3426
     2   ,/,    7H  BUS K,  13X,  15H   VK-AMPLITUDE,                   M32.3427
     3  15H   VM-AMPLITUDE,  15H  IKM-AMPLITUDE,                        M32.3428
     4  15H   PKM (WATTS)    ,/,  15X,  5HBUS M,                        M32.3429
     5  15H   VK-DEGREES  ,  15H   VM-DEGREES  ,                        M32.3430
     6  15H   IKM-DEGREES ,  15H   QKM (VARS)         )                 M32.3431
      NDX1 = LSIZ26 + 2                                                 M31.2617
      NDX2 = LSIZ26 + N6                                                M31.2618
      IF ( N6  .GE.  2 )                                                M31.2619
     1 WRITE (LUNIT6, 6005)  ( VIM(I),  I=NDX1, NDX2 )                  M31.2620
 6005 FORMAT ( 31H ADDED SUBNETWORK FREQUENCIES :,  6E14.5 )            M31.2621
      IF ( KOL132  .EQ.  132 )                                          M32.3432
     1 WRITE (LUNIT6, 6007)                                             M32.3433
 6007 FORMAT (                                                          M31.2623
     3 2X, 5HBUS K, 37X, 12HNODE VOLTAGE, 21X, 14HBRANCH CURRENT,          10157
     4 10X, 10HPOWER FLOW, 5X, 10HPOWER LOSS, /,                           10158
     5 16X, 5HBUS M,  2(9X, 11HRECTANGULAR, 10X, 5HPOLAR),                 10159
     6 5X,  2(8X, 7HP AND Q)   )                                           10160
 6010 IF (IBR .LT. IB) GO TO 6200                                               
      XOPT = XOPTBR(IB)                                                 VAX.  95
      COPT = COPTBR(IB)                                                 VAX.  96
      D3 = TENM3                                                        M31.2582
      D4 = UNITY / 1 000 000.                                           M31.2583
      IF( XOPT .GT. 0.0 )  D3 = 1.0 / ( TWOPI * XOPT )                  VAX.  97
      IF( COPT .GT. 0.0 )  D4 = D4 / ( TWOPI * COPT )                   VAX.  98
      JB = IABS ( KBUS(IB) )                                                    
      IF ( JB  .EQ.  1 )   JB = IABS ( MBUS(IB) )                               
      N16 = IABS ( KSSFRQ(JB) )                                                 
      OMEGAL = D3 * TWOPI * SFREQ(N16)                                  VAX.  99
      OMEGAC = D4 * TWOPI * SFREQ(N16)                                  VAX. 100
      IF ( IMODEL(IB) .EQ. -4 ) OMEGAC = 1.0                                    
      IF ( SFREQ(N16)  .EQ.  SFROLD )   GO TO 6017                      M31.2627
      IF ( KSSOUT .NE. 1   .AND.   KSSOUT .NE. 3 )  GO TO 6016          M33. 871
      IF ( IB  .EQ.  1 )   GO TO 6016                                   M33. 872
      WRITE (LUNIT6, 6014)  SFROLD, SFREQ(N16)                          M33. 873
 6014 FORMAT ( 37H      %%%%%  FREQUENCY DISCONTINUITY:,                M31.2632
     1          6H   END,  E13.4,  6H  HZ.,,                            M31.2633
     2         28H   BEGIN BRANCHES EXCITED AT,   E18.9,                M31.2634
     3         17H   HERTZ.   %%%%%    )                                M31.2635
 6016 SFROLD = SFREQ(N16)                                               M33. 874
 6017 JB = IABS(NR(IB))                                                         
      L = IABS(LENGTH(IB))                                                 10163
      IF(KODEBR(IB).NE.-2) GO TO 6030                                           
      IF (KODSEM(IB) .NE. 0  .AND. IMODEL(IB) .NE. -2) GO TO 6030               
      NPHCAS=IABS(LENGTH(IB))                                              10165
      DO 7001 IX=1,NPHCAS                                                  10166
      IY=IB+IX-1                                                           10167
      NROW1=KBUS(IY)                                                       10168
      NROW1=NORDER(NROW1)                                                  10169
      E(IX)=SOLR(NROW1)                                                    10170
      F(IX)=SOLI(NROW1)                                                    10171
      NROW1=MBUS(IY)                                                       10172
      NROW1=NORDER(NROW1)                                                  10173
      N1 = IX + NPHCAS                                                  M19. 900
      E(N1) = SOLR(NROW1)                                               M19. 901
      F(N1) = SOLI(NROW1)                                               M19. 902
 7001 CONTINUE                                                             10176
      CALL MULT(TX(JB),F(1),DIAG(1),LL2*NPHCAS,LL0)                     M22.3261
      CALL MULT(TR(JB),E(1),DIAG(1),LL2*NPHCAS,LLM1)                    M22.3262
      CALL MULT(TR(JB),F(1),DIAB(1),LL2*NPHCAS,LL0)                     M22.3263
      CALL MULT(TX(JB),E(1),DIAB(1),LL2*NPHCAS,LL1)                     M22.3264
      IF ( KSSOUT .NE. 1  .AND.  KSSOUT .NE. 3 )  GO TO 7003            M33. 875
      DO 7002 IX=1,NPHCAS                                                  10182
      IY=IB+IX-1                                                           10183
      IZ=IX+NPHCAS                                                         10184
 7002 CALL SSOUT(IY,E(IX),F(IX),E(IZ),F(IZ),DIAG(IX),DIAB(IX),DIAG(IZ),    10185
     1DIAB(IZ))                                                            10186
 7003 IB=IB+NPHCAS                                                         10187
      GO TO 6010                                                           10188
 6030 IF( NR(IB) .GE. 0 )  GO TO 6060                                      10189
C              SERIES   R - L - C   CALCULATIONS                           10190
      IN = IABS(KBUS(IB))                                                       
      IN = NORDER(IN)                                                           
      JN = IABS(MBUS(IB))                                                       
      JN = NORDER(JN)                                                      10194
      IF (C(JB) .EQ. 0.0)   GO TO 6044                                     10195
      XX = TX(JB)*OMEGAL - 1.0/(C(JB)*OMEGAC)                              10196
      GO TO 6048                                                           10197
 6044 XX = TX(JB) * OMEGAL                                                 10198
 6048 DD = TR(JB) * TR(JB) +XX * XX                                        10199
      RD = TR(JB) / DD                                                     10200
      XD = -XX / DD                                                        10201
      ERK = SOLR(IN)                                                       10202
      EIK = SOLI(IN)                                                       10203
      ERM = SOLR(JN)                                                       10204
      EIM = SOLI(JN)                                                       10205
      DER = ERK - ERM                                                      10206
      DEI = EIK - EIM                                                      10207
      CURRK = RD*DER - XD*DEI                                              10208
      CURIK = RD*DEI + XD*DER                                              10209
      CI(IB) = -CURRK                                                      10210
      CK(IB) = 0.0                                                         10211
      IF(  C(JB)  .NE.  0.0 )   CK(IB) = CURIK / ( C(JB)*OMEGAC )          10212
      CURRM = -CURRK                                                       10213
      CURIM = -CURIK                                                       10214
      IF( KSSOUT .EQ. 1  .OR.  KSSOUT .EQ. 3 )                          M33. 876
     1CALL SSOUT( IB, ERK, EIK, ERM, EIM, CURRK, CURIK, CURRM, CURIM )     10216
      CIK(IB) = 0.0                                                        10217
 6050 IB = IB + 1                                                          10218
      GO TO 6010                                                           10219
 6060 IF (L .GT. 1) GO TO 6080                                             10220
C              SINGLE  PI  SECTION  CALCULATIONS                           10221
      KIB = IABS( KBUS(IB) )                                                    
      IN = NORDER(KIB)                                                          
      MIB = IABS(MBUS(IB))                                                      
      JN = NORDER(MIB)                                                          
      YR = TR(JB)                                                       M43.1146
      YI = TX(JB)                                                       M43.1147
      IF (IPRSUP .GE. 4)                                                M43.1148
     1 WRITE (*,*) 'I, JB, TR(JB), TX(JB), IMODEL(IB)= ',               M43.1149
     2              I, JB, TR(JB), TX(JB), IMODEL(IB)                   M43.1150
      IF ( IMODEL(IB) .EQ. -4 ) GO TO 6067                              M43.1151
      YI = TX(JB) * OMEGAL                                                 10226
      YY = TR(JB) * TR(JB) +YI *YI                                         10227
      YR = TR(JB) / YY                                                     10228
      YI = -YI / YY                                                        10229
 6067 ERK = SOLR(IN)                                                       10230
      EIK = SOLI(IN)                                                       10231
      ERM = SOLR(JN)                                                       10232
      EIM = SOLI(JN)                                                       10233
      DER = ERK - ERM                                                      10234
      DEI = EIK - EIM                                                      10235
      CTR = YR*DER - YI*DEI                                                10236
      CTI = YR*DEI + YI*DER                                                10237
      YIS = C(JB) * OMEGAC                                                 10238
      IF (KBUS(IB) .GE. 0)   GO TO 6076                                    10239
      YRS = R(JB)                                                          10240
      CSR = YRS*ERK - YIS*EIK                                              10241
      CSI = YRS*EIK + YIS*ERK                                              10242
      CURRK = CTR + CSR                                                    10243
      CURIK = CTI + CSI                                                    10244
      TR(JB) = CURRK                                                       10245
      TX(JB) = CURIK                                                       10246
      CSR = YRS*ERM - YIS*EIM                                              10247
      CSI = YRS*EIM + YIS*ERM                                              10248
      CURRM = CSR - CTR                                                    10249
      CURIM = CSI - CTI                                                    10250
      R(JB) = CURRM                                                        10251
      C(JB) = CURIM                                                        10252
      GO TO 6078                                                                
 6076 CIK(IB) = CTR                                                        10254
      CURRK = -YIS * EIK                                                   10255
      CURIK = YIS*ERK                                                      10256
      CURRM = - YIS*EIM                                                    10257
      CURIM = YIS * ERM                                                    10258
      CI(IB) = CURRK                                                       10259
      CK(IB) = CURRM                                                       10260
      CURRK = CURRK + CTR                                                  10261
      CURIK = CURIK + CTI                                                  10262
      CURRM = CURRM - CTR                                                  10263
      CURIM = CURIM - CTI                                                  10264
 6078 IF( KSSOUT .EQ. 1  .OR.  KSSOUT .EQ. 3 )                          M33. 877
     1CALL SSOUT( IB, ERK, EIK, ERM, EIM, CURRK, CURIK, CURRM, CURIM )     10266
      IB = IB + 1                                                       M23.1590
      GO TO 6094                                                        M23.1591
C              MULTIPLE  PI  SECTION  CALCULATIONS                         10268
 6080 IBF = IB - 1                                                              
      LD = JB -1                                                           10270
      LDD = JB -1                                                               
 6090 JB = IBF                                                                  
      CLIR = 0.0                                                           10273
      CLII = 0.0                                                           10274
      CLJR = 0.0                                                           10275
      CLJI = 0.0                                                           10276
      CTR = 0.0                                                            10277
      CTI = 0.0                                                            10278
 6100 LD = LD +1                                                                
      JB = JB +1                                                                
      IA = IABS(KBUS(JB))                                                  10281
      IA = NORDER(IA)                                                      10282
      JA = IABS(MBUS(JB))                                                  10283
      JA = NORDER(JA)                                                      10284
      DER = SOLR(IA) - SOLR(JA)                                                 
      DEI = SOLI(IA) - SOLI(JA)                                                 
      CSR = OMEGAC * C(LD)                                                      
      CTR = CTR + TR(LD)*DER - TX(LD)*DEI                                       
      CTI = CTI + TR(LD)*DEI + TX(LD)*DER                                       
      CLIR = CLIR - CSR*SOLI(IA)                                           10290
      CLII = CLII + CSR*SOLR(IA)                                                
      CLJR = CLJR - CSR*SOLI(JA)                                           10292
      CLJI = CLJI + CSR*SOLR(JA)                                                
      IF( KBUS(JB)  .GE.  0 )  GO TO 6110                                       
      CLIR = CLIR  +  R(LD)*SOLR(IA)                                       10295
      CLII = CLII  +  R(LD)*SOLI(IA)                                       10296
      CLJR = CLJR  +  R(LD)*SOLR(JA)                                            
      CLJI = CLJI  +  R(LD)*SOLI(JA)                                       10298
 6110 IF (JB .LT. IB) GO TO 6100                                                
      KD = LD                                                                   
      ERK = SOLR(IA)                                                       10301
      EIK = SOLI(IA)                                                       10302
      ERM = SOLR(JA)                                                       10303
      EIM = SOLI(JA)                                                       10304
 6140 IF (JB .EQ. IBF+L) GO TO 6160                                             
      KD = KD + JB - IBF                                                        
      JB = JB + 1                                                               
      IA = IABS(KBUS(JB))                                                       
      IA = NORDER(IA)                                                           
      JA = IABS(MBUS(JB))                                                       
      JA = NORDER(JA)                                                           
      DER = SOLR(IA) - SOLR(JA)                                                 
      DEI = SOLI(IA) - SOLI(JA)                                                 
      CSR = OMEGAC * C(KD)                                                 10314
      CTR = CTR + TR(KD)*DER - TX(KD)*DEI                                       
      CTI = CTI + TR(KD)*DEI + TX(KD)*DER                                       
      CLIR = CLIR - CSR*SOLI(IA)                                                
      CLII = CLII + CSR*SOLR(IA)                                           10318
      CLJR = CLJR - CSR*SOLI(JA)                                           10319
      CLJI = CLJI + CSR*SOLR(JA)                                           10320
      IF( KBUS(JB)  .GE.  0 )  GO TO 6140                                       
      CLIR = CLIR  +  R(KD)*SOLR(IA)                                            
      CLII = CLII  +  R(KD)*SOLI(IA)                                       10323
      CLJR = CLJR  +  R(KD)*SOLR(JA)                                       10324
      CLJI = CLJI  +  R(KD)*SOLI(JA)                                       10325
      GO TO 6140                                                                
 6160 CURRK = CTR + CLIR                                                        
      CURIK = CTI + CLII                                                   10328
      CURRM = -CTR + CLJR                                                  10329
      CURIM = -CTI + CLJI                                                  10330
      IF( KBUS(IB)  .GE.  0 )  GO TO 6164                                       
      LDD = LDD + 1                                                        10332
      TR(LDD) = CURRK                                                           
      TX(LDD) = CURIK                                                      10334
      R(LDD) = CURRM                                                            
      C(LDD) = CURIM                                                       10336
      GO TO 6166                                                           10337
 6164 CIK(IB) = CTR                                                        10338
      CI(IB) = CLIR                                                        10339
      CK(IB) = CLJR                                                        10340
 6166 IF( KSSOUT .EQ. 1  .OR.  KSSOUT .EQ. 3 )                          M33. 878
     1CALL SSOUT( IB, ERK, EIK, ERM, EIM, CURRK, CURIK, CURRM, CURIM )     10342
      IB = IB + 1                                                          10343
      IF ( IB  .LE.  IBF+L )   GO TO 6090                                       
 6094 IF (KODSEM(IB - 1) .EQ. 0  .OR. IMODEL(IB-1) .EQ. -2)             M31.2640
     1   GO TO 6010                                                     M31.2641
      IF (IMODEL(IB-1) .EQ.-4) GO TO 6010                               M43.1152
 6095 IF (CKI(IB - 1) .LT. 0.0) GO TO 6010                              M23.1593
      IB = IB + 1                                                       M23.1594
      GO TO 6095                                                        M23.1595
 6200 N1 = 2                                                            M36.1385
      CALL VECRSV ( DIAG(1), NTOT, N1 )                                 M32.3441
      CALL VECRSV ( DIAB(1), NTOT, N1 )                                 M32.3442
      CALL VECRSV ( GND(IOFGND+1), IOFFD, N1 )                          M32.3443
      CALL VECRSV ( BND(IOFBND+1), IOFFD, N1 )                          M32.3444
      IF ( NCURR  .LE.  0 )   NCURR = 1                                 M22.3265
      CALL MOVER0 ( E(1), NTOT )                                        M32.3445
      CALL MOVER0 ( F(1), NTOT )                                        M32.3446
      DO 7380  I=1, NTOT                                                M36.1386
      VR = SOLR(I)                                                         10371
      VI = SOLI(I)                                                         10372
      JJ = INDEX(I)                                                        10373
      KKK = INDEX(I+1)                                                     10374
      E(I) = E(I)  +  DIAG(I)*VR - DIAB(I)*VI                              10376
      F(I) = F(I)  +  DIAG(I)*VI + DIAB(I)*VR                              10377
      GO TO 7370                                                           10378
 7360 JJ = JJ + 1                                                          10379
 7370 IF( JJ .GE. KKK )  GO TO 7380                                        10380
      J = ILOC(JJ)                                                         10381
      ISUBS1 =  IOFGND+JJ                                               M16.1577
      ISUBS2 =  IOFBND+JJ                                               M16.1578
      DER = GND(ISUBS1)   *VR - BND(ISUBS2)   *VI                       M16.1579
      DEI = GND(ISUBS1)   *VI + BND(ISUBS2)   *VR                       M16.1582
      E(J) = E(J) + DER                                                    10385
      F(J) = F(J) + DEI                                                    10386
      E(I) = E(I)  +  GND(ISUBS1)   *SOLR(J) - BND(ISUBS2)   *SOLI(J)   M16.1585
      F(I) = F(I)  +  GND(ISUBS1)   *SOLI(J) + BND(ISUBS2)   *SOLR(J)   M16.1588
      GO TO 7360                                                           10390
 7380 CONTINUE                                                          M36.1387
      IF ( KSSOUT .EQ. 0 )  GO TO 7394                                  M36.1388
      D26 = 0.0                                                         M36.1389
      DO 7388  I=1, NTOT                                                M36.1390
 7388 D26 = D26  +  E(I) * SOLR(I) + F(I) * SOLI(I)                     M36.1391
      D26 = D26 * ONEHAF                                                        
      WRITE (LUNIT6, 2008)  D26                                         M36.1393
 2008 FORMAT ( /,  8X,                                                  M36.1394
     1 56HTOTAL NETWORK LOSS "PLOSS" BY SUMMING NODAL INJECTIONS =,     M36.1395
     2  E20.10 )                                                        M36.1396
 7394 IF (KSWTCH .LE. 0) GO TO 7396                                     M36.1397
      DO 6009 I=1,KCONST                                                M32.3448
      IF (NODE(I) .GT. 0) GO TO 6009                                    M32.3449
      IF (TSTART(I) .GE. 0) GO TO 6009                                  M32.3450
      IF (IABS(IFORM(I)) .NE. 14) GO TO 6009                            M32.3451
      KT=IABS(NODE(I))                                                  M32.3452
      K = NORDER(KT)                                                    M32.3453
      E(K)=E(K)-CREST(I)*COSZ(TIME1(I))                                 M32.3454
      F(K)=F(K)-CREST(I)*SINZ(TIME1(I))                                 M32.3455
 6009 CONTINUE                                                          M32.3456
      CALL MOVE0(NEXTSW(1), KSWTCH)                                     M32.3457
      CALL MOVER0(ENERGY(1), KSWTCH)                                    M32.3458
      DO 6111 I=1,KSWTCH                                                M32.3459
      IF (ADELAY(I) .EQ. 44444.)  GO TO 6111                            M33. 881
      IF (TCLOSE(I) .LT. 0.0) NEXTSW(I)=88                              M32.3461
 6111 CONTINUE                                                          M32.3462
      N8=0                                                              M32.3463
 8516 N9=0                                                              M32.3464
      N5=0                                                              M32.3465
      DO 8657 I=1,KSWTCH                                                M32.3466
      IF ( NEXTSW(I)  .EQ.  87 )   GO TO 8657                           M32.3467
      IF ( NEXTSW(I)  .EQ.  88 )   GO TO 8521                           M32.3468
      N9=N9+1                                                           M32.3469
      GO TO 8657                                                        M32.3470
 8521 KT = KMSWIT(I)                                                    M32.3471
      K = NORDER(KT)                                                    M32.3472
      NDX5 = LSWTCH + I                                                 M32.3473
      MT = KMSWIT(NDX5)                                                 M32.3474
      M = NORDER(MT)                                                    M32.3475
      N13=K                                                             M32.3476
 4511 IF ( N13 .EQ. NORDER(1) )   GO TO 4544                            M32.3477
      DO 4522 J=1,KSWTCH                                                M32.3478
      IF (NEXTSW(J) .LT. 88) GO TO 4522                                 M32.3479
      IF (I .EQ. J) GO TO 4522                                          M32.3480
      NT1 = KMSWIT(J)                                                   M32.3481
      N1 = NORDER(NT1)                                                  M32.3482
      NDX3 = LSWTCH + J                                                 M32.3483
      NT2 =KMSWIT(NDX3)                                                 M32.3484
      N2 = NORDER(NT2)                                                  M32.3485
      IF (N13 .EQ. N1) GO TO 4544                                       M32.3486
      IF (N13 .EQ. N2) GO TO 4544                                       M32.3487
 4522 CONTINUE                                                          M32.3488
      DO 4533 J=1,KCONST                                                M32.3489
      IF (NODE(J) .LT. 0) GO TO 4533                                    M33. 882
      JS = NORDER(NODE(J))                                              M32.3491
      IF (JS .NE. N13) GO TO 4533                                       M32.3492
      IF (TSTART(J) .GE.0.0) GO TO 4533                                 M32.3493
      IF (IABS(IFORM(J)) .NE. 14)  GO TO 4533                           M32.3494
      GO TO 4544                                                        M32.3495
 4533 CONTINUE                                                          M32.3496
      IF (IPRSUP .GE. 4)                                                M32.3497
     1 WRITE( LUNIT6, 5515 )  I, K, N13                                 M32.3498
 5515 FORMAT ( 22H CURRENT.  I, K, N13 =, 3I8 )                         M32.3499
      TCLOSE(I) = E(N13)                                                M32.3501
      ENERGY(I) = F(N13)                                                M32.3502
      E(N13) = 0.0                                                      M32.3503
      F(N13) = 0.0                                                      M32.3504
      NEXTSW(I) = 87                                                    M32.3505
      N8 = N8 + 1                                                       M32.3506
      N5 = 1                                                            M32.3507
      IF (N13 .EQ. M) M = K                                             M32.3508
      E(M) = E(M) + TCLOSE(I)                                           M32.3509
      F(M) = F(M) + ENERGY(I)                                           M32.3510
      M = NORDER(MT)                                                    M32.3511
      IF (N13 .EQ. M) GO TO 7657                                        M39.1587
      TCLOSE(I) = -TCLOSE(I)                                            M32.3513
      ENERGY(I) = -ENERGY(I)                                            M32.3514
 7657 K1 = KDEPSW( LSWTCH + I )                                         M39.1588
      IF ( K1 .NE. 8888 ) GO TO 8657                                    M39.1589
      MK = - ADELAY(I)                                                  M39.1590
      IF ( MK .LE. 0.0 ) GO TO 8657                                     M39.1591
      NUM = KSSFRQ(KT)                                                  M39.1592
      ANG = TWOPI * SFREQ(NUM) * DELTAT                                 M39.1593
      A8SW(MK+6) = TCLOSE(I) * COSZ(ANG) - ENERGY(I) * SINZ(ANG)        M39.1594
      GO TO 8657                                                        M32.3515
 4544 IF (N13 .EQ. M) GO TO 8657                                        M32.3516
      N13 = M                                                           M32.3517
      GO TO 4511                                                        M32.3518
 8657 CONTINUE                                                          M32.3519
      IF ( IPRSUP  .GE.  2 )                                            M32.3520
     1 WRITE ( LUNIT6, 5525 )  N5, N8, N9                               M32.3521
 5525 FORMAT ( 26H BELOW 8657.  N5, N8, N9 =, 3I8 )                     M32.3522
      IF (N5 .EQ. 0) GO TO 8669                                         M32.3523
      GO TO 8516                                                        M32.3524
 8669 N8=N8+N9                                                          M32.3525
      IF (N8 .EQ. KSWTCH) GO TO 8681                                    M32.3526
      WRITE (LUNIT6,8674) N8, KSWTCH                                    M32.3527
 8674 FORMAT ( 46HILLEGAL SWITCH/SOURCE CONNECTION. N8, KSWTCH =,2I8)   M32.3528
      KILL = 64                                                                 
      LSTAT(19) = 8669                                                  M42.6574
      BUS1 = TRASH                                                              
      GO TO 9800                                                        M42.6576
 8681 IF ( KSSOUT  .EQ.  0 )   GO TO 8715                               M32.3530
      IF ( KOL132  .EQ.  132 )                                          M32.3531
     1 WRITE (LUNIT6, 8685)                                             M32.3532
 8685 FORMAT (39H OUTPUT FOR STEADY STATE SWITCH CURRENT  ,/,           M32.3533
     1  7X,  6HNODE-K,  4X,  6HNODE-M,  10X,  6HI-REAL,                 M36.1398
     2  12X,  6HI-IMAG,  12X,  6HI-MAGN,  10X,  7HDEGREES,              M36.1399
     3  8X,  6HPOWER ,  10X,  8HREACTIVE )                              M36.1400
      IF ( KOL132  .EQ.  80 )                                           M32.3536
     1 WRITE (LUNIT6, 8686)                                             M32.3537
 8686 FORMAT ( 39H OUTPUT FOR STEADY STATE SWITCH CURRENT  ,/,          M32.3538
     1  1X,  6HNODE-K,  2X,  6HNODE-M,  10X,  6HI-REAL,                 M32.3539
     2  10X, 6HI-IMAG,  10X, 6HI-MAGN,  9X,  7HDEGREES  )               M32.3540
      DO 8713 I=1,KSWTCH                                                M32.3541
      NDX4 = LSWTCH + I                                                 M32.3542
      K = KMSWIT(I)                                                     M32.3543
      M = KMSWIT(NDX4)                                                  M32.3544
      IF (NEXTSW(I) .NE. 87) GO TO 8695                                 M32.3545
      D6 = TCLOSE(I)**2  +  ENERGY(I)**2                                M32.3546
      D4 = SQRTZ ( D6 )                                                 M32.3547
      D5 = ATAN2Z ( ENERGY(I), TCLOSE(I) )                              M32.3548
      D5 = D5 * 360. / TWOPI                                            M32.3549
      NN = NORDER(K)                                                    M36.1401
      POWER = ONEHAF * (SOLR(NN)*TCLOSE(I) + SOLI(NN)*ENERGY(I))        M36.1402
      REACT = ONEHAF * (SOLI(NN)*TCLOSE(I) - SOLR(NN)*ENERGY(I))        M36.1403
      IF ( KOL132  .EQ.  132 )                                          M32.3550
     1 WRITE (LUNIT6,8692) BUS(K), BUS(M), TCLOSE(I),                   M32.3551
     2     ENERGY(I), D4, D5, POWER, REACT                              M36.1404
 8692 FORMAT (5X,2A10,3E18.8,F13.4,2E18.8 )                             M36.1405
      IF ( KOL132  .EQ.  80 )                                           M32.3554
     1 WRITE (LUNIT6, 8693)  BUS(K), BUS(M), TCLOSE(I),                 M32.3555
     2                       ENERGY(I), D4, D5                          M32.3556
 8693 FORMAT ( 1X, A6, 2X, A6, 3E16.7,  F16.6  )                        M32.3557
      GO TO 8713                                                        M32.3558
 8695 IF (ADELAY(I) .EQ. 44444.)  GO TO 8703                            M33. 883
      IF (TCLOSE(I) .GT. 0.0 )  GO TO 8703                              M32.3560
      IF ( KOL132  .EQ.  132 )                                          M32.3561
     1 WRITE (LUNIT6, 8699 )  BUS(K), BUS(M)                            M32.3562
 8699 FORMAT ( 5X,  2A10,  2( 5X, 13HCLOSE AT T=0+ )  )                 M32.3563
      IF ( KOL132  .EQ.  80 )                                           M32.3564
     1 WRITE (LUNIT6, 8700)  BUS(K), BUS(M)                             M32.3565
 8700 FORMAT ( 1X, A6, 2X, A6,  2( 5X, 13HCLOSE AT T=0+ ) )             M32.3566
      GO TO 8713                                                        M32.3567
 8703 IF ( KOL132  .EQ.  132 )                                          M32.3568
     1 WRITE (LUNIT6,8705) BUS(K), BUS(M)                               M32.3569
 8705 FORMAT ( 5X, 2A10, 4( 10X, 4HOPEN, 6X ),  8X, 4HOPEN  )           M36.1406
      IF ( KOL132  .EQ.  80 )                                           M32.3571
     1 WRITE (LUNIT6, 8706)  BUS(K), BUS(M)                             M32.3572
 8706 FORMAT ( 1X, A6, 2X, A6,  3( 12X, 4HOPEN)  )                      M32.3573
 8713 CONTINUE                                                          M32.3574
 8715 KTRLSW(1) = 8877                                                  M32.3575
 7396 IF ( KSSOUT  .LE.  0 )  GO TO 8350                                M32.3576
      IF ( KOL132  .EQ.  132 )                                          M32.3577
     1 WRITE (LUNIT6, 7343)                                             M32.3578
 7343 FORMAT( /,113H SOLUTION AT NODES WITH KNOWN VOLTAGE.     NODES SHOM32.3579
     1RTED TOGETHER BY SWITCHES ARE SHOWN AS A GROUP OF NAMES, WITH  ,  M32.3580
     2 /, 112H THE PRINTED RESULT APPLYING TO THE COMPOSITE GROUP.   THEM32.3581
     2 ENTRY 'MVA' IS  SQRT(P**2 + Q**2)  IN UNITS OF POWER,  ,/,       M32.3582
     3  47H WHILE  'P.F.'  IS THE ASSOCIATED POWER FACTOR.   ,/,        M32.3583
     3 17X, 4HNODE, 16X, 19HSOURCE NODE VOLTAGE, 12X,                   M32.3584
     4 23HINJECTED SOURCE CURRENT, 14X, 21HINJECTED SOURCE POWER, /,    M32.3585
     5 17X, 4HNAME,  2( 9X, 11HRECTANGULAR, 10X, 5HPOLAR), 13X,         M32.3586
     6 7HP AND Q, 3X, 12HMVA AND P.F.  )                                M32.3587
      IF ( KOL132  .NE.  132 )                                          M32.3588
     1 WRITE (LUNIT6, 7346)                                             M32.3589
 7346 FORMAT (  38H 80-COLUMN INJECTION PRINTOUT AT NODES,              M32.3590
     1          20H WITH KNOWN VOLTAGE.   ,/,       16X, 4HNODE,        M32.3591
     2  15H  VK-MAGNITUDE ,  15H  IK-MAGNITUDE ,                        M32.3592
     3  15H    PK (WATTS) ,  15H  MVA (WATTS)  ,/,  16X, 4HNAME,        M32.3593
     4  15H  VK-DEGREES   ,  15H  IK-DEGREES   ,                        M32.3594
     5  15H    QK (VARS)  ,  15H  POWER FACTOR      )                   M32.3595
 8350 DO 7400  K=1, NTOT                                                M32.3596
      J = NORDER(K)                                                        10394
 7400 INDEX(J) = K                                                         10395
      IF ( IPRSUP  .GE.  3 )                                            M31.2642
     1 WRITE(LUNIT6, 7401)  ( I, NORDER(I), INDEX(I), KODE(I), E(I),    M17. 526
     1 F(I), DIAG(I), DIAB(I), I=1, NTOT )                                 10398
 7401 FORMAT ( /,  1X,                                                  M31.2643
     1  32H     ROW  NORDER   INDEX    KODE,  14X, 1HE,  14X,           M31.2644
     2  1HF,  11X, 4HDIAG,  11X, 4HDIAB  ,/, ( 1X, 4I8, 4E15.6 ) )      M31.2645
      I = NCURR + 1                                                        10400
 7405 IF( I .GT. NTOT )  GO TO 7456                                        10401
      J = KODE(I)                                                          10402
      IF( J .LE. I )  GO TO 7410                                           10403
      E(J) = E(J) + E(I)                                                   10404
      F(J) = F(J) + F(I)                                                   10405
      GO TO 7440                                                           10406
 7410 L = 1                                                                10407
      KKK = INDEX(I)                                                       10408
      IF( KKK .EQ. 1 )  GO TO 7440                                         10409
      AUPPER(1) = BUS(KKK)                                              M11. 122
 7420 IF( J .GE. I )  GO TO 7430                                           10411
      IF ( L  .LT.  13 )                                                M12. 421
     1 L = L + 1                                                        M12. 422
      KKK = INDEX(J)                                                       10413
      AUPPER(L) = BUS(KKK)                                              M11. 123
      IF ( AUPPER(L)  .EQ.  BLANK )                                     M32.3597
     1 AUPPER(L) = TERRA                                                M32.3598
      J = KODE(J)                                                          10415
      GO TO 7420                                                           10416
 7430 IF ( KSSOUT  .EQ.  0 )   GO TO 7440                               M12. 423
      WRITE (LUNIT6, 7431)                                              M28.4279
      N13 = L - 1                                                       M28.4280
      IF ( N13  .GT.  0 )                                               M28.4281
     1 WRITE (LUNIT6, 7431)  ( AUPPER(J), J=1, N13 )                    M28.4282
 7431 FORMAT ( 15X, A6 )                                                M28.4283
      BUS5 = AUPPER(L)                                                  M28.4284
      IB = -1                                                              10419
      RD = 1.0                                                             10420
      CALL SSOUT( IB, SOLR(I), SOLI(I), RD, RD, E(I), F(I), RD, RD )       10421
 7440 I = I + 1                                                            10422
      GO TO 7405                                                           10423
 7456 IF ( LOOPSS(1)  .LE.  0 )   GO TO 7452                            M31.2646
      LASTOV = NCHAIN                                                   M31.2647
      NCHAIN = 8                                                        M31.2648
      WRITE (LUNIT6, 8457)                                              M32.3599
 8457 FORMAT ( 42H COMPLETE ANOTHER INTERNAL PHASOR SOLUTION,           M32.3600
     1         43H FOR U.M.   EXIT "OVER11", BACK TO "OVER8". )         M32.3601
      GO TO 9900                                                        M32.3602
 7452 IF ( LOOPSS(1)  .EQ.  0 )   GO TO 7453                            M31.2650
      KCONST = LOOPSS(3)                                                M31.2651
      IBR = LOOPSS(5)                                                   M31.2652
C     WORK ON SYNCHRONOUS MACHINE STEADY-STATE SOLUTION OUTPUT, PLUS    M31.2654
C     CALCULATION OF INITIAL CONDITIONS FOR TIME  T = 0.0   .           M31.2655
 7453 IF ( NUMSM  .EQ.  0 )   GO TO 6207                                M31.2656
      CALL SMINT                                                        M35.1239
      IF( KILL .GT.  0 )  GO  TO  9800                                  M32.3603
 6207 DO 6210  I=1, NTOT                                                M20.2265
      J = NORDER(I)                                                        10425
      E(I) = SOLR(J)                                                       10426
 6210 F(I) = SOLI(J)                                                       10427
      IF( IPRSUP .GT. 0 )  WRITE(LUNIT6, 5581)                          M17. 528
 5581 FORMAT( 51H STEADY-STATE PHASOR NETWORK SOLUTION NOW COMPLETE.  )    10429
      IF( IPRSUP .GE. 1 )                                                  10430
     1 WRITE(LUNIT6, 5504)  ( BUS(I), I=1, NTOT )                       M17. 529
 5504 FORMAT (  33H (BUS(I), I=1, NTOT)  IN  OVER11.  ,/,                  10432
     1 ( 1X, 10A8 ) )                                                      10433
      IF ( TMAX  .GT.  0.0 )   GO TO 9700                               M21.  70
      IF ( FMAXFS  .EQ.  0.0 )   GO TO 7027                             M20.2266
      IF ( KNT  .EQ.  1  .AND. NSOLVE .EQ. 0 )   GO TO 7027
      GO TO 8102                                                        M20.2269
 7027 NUMNVO = 0                                                        M20.2270
C     READ INPUT CARD USING CIMAGE.                                        10437
 7030 CALL CIMAGE                                                          10438
      DECODE (80, 22, ABUFF)  IVOLT, AUPPER                                10439
   22 FORMAT(I2,13A6)                                                      10440
      IF ( IVOLT  .EQ.  1 )   GO TO 7050                                   10441
      IF ( IVOLT  .NE.  0 )   GO TO 7030                                M33. 884
      N1 = 0                                                               10442
      DO 6842  K=1, 13                                                     10443
      BUS1 = AUPPER(K)                                                     10444
      IF( BUS1 .NE. BLANK )  GO TO 6847                                    10445
 6842 CONTINUE                                                             10446
      WRITE(LUNIT6, 5416)                                               M35.1240
 5416 FORMAT(  49H+BLANK CARD ENDING NODE NAMES FOR VOLTAGE OUTPUT.    )M35.1241
      GO TO 8102                                                        M35.1242
 6847 WRITE(LUNIT6, 54163)                                              M17. 530
54163 FORMAT(  43H+CARD OF BUS NAMES FOR NODE-VOLTAGE OUTPUT.  )           10449
      K = 1                                                             M20.2271
 7048 IF ( K  .GT.  13 )   GO TO 7030                                   M20.2272
      BUS1 = AUPPER(K)                                                     10451
      IF( BUS1 .EQ. BLANK )  GO TO 7040                                    10452
      DO 7041  I=2, NTOT                                                   10453
      IF( BUS1 .EQ. BUS(I) )  GO TO 7043                                   10454
 7041 CONTINUE                                                             10455
      WRITE(LUNIT6, 7042)  BUS1                                         M17. 531
 7042 FORMAT( 5X,  40HREQUESTED OUTPUT FOR NONEXISTENT NODE  ', A6,        10457
     1  19H'  WILL BE IGNORED.   )                                         10458
      GO TO 7040                                                           10459
 7043 NUMNVO = NUMNVO + 1                                               M20.2273
      IF (NUMNVO .GT. LSIZ27 .OR. NUMNVO .GT. LSIZ12) GO TO 8080        M38.2216
      JCH2(NUMNVO) = I                                                  M35.1244
 7040 K = K + 1                                                         M20.2275
      GO TO 7048                                                        M20.2276
 8080 WRITE (LUNIT6, 8090)  NUMNVO, LSIZ12, LSIZ27                      M38.2217
 8090 FORMAT( 40H THE NUMBER OF REQUESTS FOR NODE VOLTAGE,              M35.1246
     1        39H OUTPUTS EXCEEDS THE MAXIMUM NUMBER  OF,               M35.1247
     2        45H NODE VOLTAGE OUTPUTS ("LSIZ12"), OR THE SIZE          M38.2218
     3  ,/,   40H OF LIST 27 ("LSIZ27").  TEMPORARY ERROR,              M35.1249
     4        31H STOP AT S.N. 8080 OF "OVER11".,   3I8  )              M35.1250
      CALL STOPTP                                                               
 7050 WRITE(LUNIT6, 54166)                                              M17. 532
54166 FORMAT( 41H+REQUEST FOR OUTPUT OF ALL NODE VOLTAGES.  )              10464
      IF (NTOT1 .GT. LSIZ27 .OR. NTOT1 .GT. LSIZ12)  GO TO 8080         M38.2219
      DO 8101 I=2,NTOT                                                     10465
 8101 JCH2(I-1) = I                                                     M35.1253
      NUMNVO = NTOT1                                                    M20.2278
      IF ( AUPPER(1)  .NE.  BLANK )  GO TO 7030                         M33. 885
 8102 IF ( FMAXFS  .EQ.  0.0 )  WRITE ( LUNIT6, 8103 )                  M35.1254
 8103 FORMAT ( //,  85H BEGIN STEADY-STATE PRINTOUT OF EMTP OUTPUT VARIAM35.1255
     1BLES.   NODE VOLTAGE OUTPUT FOLLOWS.     ,/,                      M35.1256
     2  7X, 3HBUS,  14X, 6HPHASOR, 7X, 8HANGLE IN, 16X, 4HREAL,         M35.1257
     3 11X, 9HIMAGINARY  ,/, 6X, 4HNAME, 11X, 9HMAGNITUDE,              M35.1258
     4  8X, 7HDEGREES,  16X, 4HPART, 16X, 4HPART  )                     M35.1259
      IF(ISTEAD.EQ.0) GO TO 8100                                        M35.1260
      IF ( NUMNVO  .EQ.  0 )   GO TO 8100                               M20.2287
      DO 8105  K=1, NUMNVO                                              M20.2288
      I = JCH2(K)                                                       M35.1261
      H1=E(I)                                                              10478
      H2=F(I)                                                              10479
      A=SQRTZ(H1**2+H2**2)                                              M18. 439
      GUS1=H1                                                           M05. 462
      IF(A.EQ.0.) GUS1=1.0                                              M05. 463
      CK1 = ATAN2Z(H2, GUS1) * 360. / TWOPI                             M18. 440
      IF ( FMAXFS  .EQ.  0 )                                            M20.2290
     1 WRITE (LUNIT6, 8104)  BUS(I), A, CK1, H1, H2                     M20.2291
 8104 FORMAT ( 4X, A6, E20.8, F15.6, 2E20.8 )                           M20.2292
      SOLR(K) = A                                                       M20.2293
      SOLI(K) = CK1                                                     M20.2294
      VOLTK(K) = H1                                                     M31.2657
      VOLT(K) = H2                                                      M31.2658
      IF ( K  .LT.  LSIZ26 )   GO TO 8105                               M31.2659
      WRITE (LUNIT6, 9104)   LSIZ26                                     M31.2660
 9104 FORMAT ( /,  37H PERMANENT ERROR STOP DUE TO OVERFLOW,            M31.2661
     1             36H OF LIST-26 VECTORS "VOLTK", "VOLT"., I8   )      M33. 887
      KILL = 1                                                          M31.2663
      LSTAT(19) = 8105                                                  M31.2664
      LSTAT(17) = 26                                                    M31.2665
      GO TO 9800                                                        M32.3606
 8105 CONTINUE                                                             10486
 8100 IF ( FMAXFS  .GT.  0.0 )   GO TO 8120                             M35.1262
      WRITE (LUNIT6, 8106)                                              M44. 355
 8106 FORMAT ( /,  35H EMTP BRANCH-CURRENT OUTPUT FOLLOWS   ,/,  8X,    M44. 356
     2 4HFROM, 10X, 2HTO, 11X, 9HMAGNITUDE, 7X, 8HANGLE IN, 16X,        M44. 357
     3 4HREAL, 11X, 9HIMAGINARY  ,/,  9X, 3HBUS,  9X, 3HBUS,            M44. 358
     4 11X, 9HOF PHASOR, 8X, 7HDEGREES,  2( 16X, 4HPART )   )           M44. 359
      DO 8110  I=1, IBR                                                 M44. 360
      ICHECK = MBUS(I)                                                  M44. 361
      IF ( ICHECK .GE. 0 ) GO TO 8110                                   M44. 362
      N1 = KBUS(I)                                                      M44. 363
      N2 = IABS ( NR(I) )                                               M44. 364
      H1 = TR(N2)                                                       M44. 365
      ICHECK = -ICHECK                                                  M44. 366
      N15 = ICHECK                                                      M44. 367
      IF ( N15 .EQ. 1 ) N15 = IABS ( N1 )                               M44. 368
      XOPT = XOPTBR(I)                                                  M44. 369
      COPT = COPTBR(I)                                                  M44. 370
      N16 = IABS ( KSSFRQ(N15) )                                        M44. 371
      OMEGA = TWOPI * SFREQ(N16)                                        M44. 372
      D2 = OMEGA * TENM3                                                M44. 373
      H3 = 1000. / D2                                                   M44. 374
      IF ( XOPT .GT. 0.0 ) D2 = OMEGA / ( XOPT * TWOPI )                M44. 375
      IF ( COPT .GT. 0.0 ) H3 =  1000000. * COPT * TWOPI / OMEGA        M44. 376
      IF ( N1 .GT. 0 ) GO TO 8107                                       M44. 377
      N1 = -N1                                                          M44. 378
      H2 = TX(N2)                                                       M44. 379
      GO TO 8108                                                        M44. 380
 8107 H2 = TX(N2) * D2                                                  M44. 381
      A = C(N2)                                                         M44. 382
      IF ( A .GT. 0.0 ) H2 = H2 - H3 / A                                M44. 383
      A = H1**2 + H2**2                                                 M44. 384
      GUS1 =  H1 / A                                                    M44. 385
      GUS2 = -H2 / A                                                    M44. 386
      GUS3 = E(N1) - E(ICHECK)                                          M44. 387
      GUS4 = F(N1) - F(ICHECK)                                          M44. 388
      H1 = GUS1 * GUS3 - GUS2 * GUS4                                    M44. 389
      H2 = GUS1 * GUS4 + GUS2 * GUS3                                    M44. 390
 8108 A = SQRTZ ( H1**2 + H2**2 )                                       M44. 391
      GUS1 = H1                                                         M44. 392
      IF ( A .EQ. 0. ) GUS1 = 1.0                                       M44. 393
      CK1 = ATAN2Z ( H2, GUS1 ) * 360. / TWOPI                          M44. 394
      WRITE (LUNIT6, 8109) BUS(N1), BUS(ICHECK), A, CK1, H1, H2         M44. 395
 8109 FORMAT ( 6X, A6,  6X, A6,  E20.8,  F15.6,  2E20.8 )               M44. 396
 8110 CONTINUE                                                          M44. 397
      IF ( NV  .EQ.  0 )   GO TO 8120                                   M35.1263
      WRITE (LUNIT6, 8111)                                              M35.1264
 8111 FORMAT ( /,  35H EMTP BRANCH-VOLTAGE OUTPUT FOLLOWS,              M35.1265
     1             25H (COLUMN-80 PUNCHES ONLY)   ,/,  8X,              M35.1266
     2 4HFROM, 10X, 2HTO, 11X, 9HMAGNITUDE, 7X, 8HANGLE IN, 16X,        M35.1267
     3 4HREAL, 11X, 9HIMAGINARY  ,/,  9X, 3HBUS,  9X, 3HBUS,            M35.1268
     4 11X, 9HOF PHASOR, 8X, 7HDEGREES,  2( 16X, 4HPART )   )           M35.1269
      DO 8115  I=1, NV                                                  M35.1270
      N1 = IBRNCH(I)                                                    M35.1271
      N2 = JBRNCH(I)                                                    M35.1272
      D1 = E(N1) - E(N2)                                                M35.1273
      D2 = F(N1) - F(N2)                                                M35.1274
      D3 = SQRTZ ( D1**2 + D2**2 )                                      M35.1275
      D4 = D1                                                           M35.1276
      IF ( D3 .EQ. 0.0 )  D4 = 1.0                                      M35.1277
      D5 = ATAN2Z ( D2, D4 ) * 360. / TWOPI                             M35.1278
 8115 WRITE (LUNIT6, 8116)  BUS(N1), BUS(N2), D3, D5, D1, D2            M35.1279
 8116 FORMAT ( 6X, A6, 6X, A6, E20.8, F15.6, 2E20.8 )                   M35.1280
 8120 IF ( FMAXFS  .EQ.  0.0 )   GO TO 7481                             M35.1281
      IF ( IPRSUP  .GE.  2 )                                            M20.2308
     1 WRITE (LUNIT6, 7457)  KNT, KCONST, FMINFS, DELFFS, FMAXFS        M20.2309
 7457 FORMAT ( /,  35H END FREQ. SCAN LOOP IN  'OVER11' .,              M20.2310
     1 16H     KNT  KCONST,  9X,  6HFMINFS,  9X,  6HDELFFS,             M20.2311
     2 9X,  6HFMAXFS  ,/,   35X,  2I8,  3E15.6  )                       M20.2312
      D4 = FMINFS                                                       M20.2313
      IF ( DELFFS  .LT.  0 )   D4 = ALOG1Z(D4)                          M20.2314
      IF ( KNT  .GT.  1 .OR. KEXACT .EQ. 88333 )   GO TO 7473       
      WRITE (LUNIT6, 7467)                                              M20.2316
 7467 FORMAT ( /, 106H THE   'FREQUENCY SCAN'   OUTPUT VECTOR HAS THE FOM20.2317
     1LLOWING FORMAT.   CELL NUMBER ONE CONTAINS THE FREQUENCY     ,/,  M20.2318
     2 111H OF THE PHASOR SOLUTION (OR THE BASE-10 LOGARITHM OF THIS, INM20.2319
     3 THE CASE OF GEOMETRIC FREQUENCY SPACING).   CELLS           ,/,  M20.2320
     4 115H NUMBERED TWO ONWARD CONTAIN THE PAIRS OF MAGNITUDE AND ANGLEM20.2321
     5 OF THE PHASOR NODE VOLTAGES.   THESE PAIRS ARE IN THE       ,/,  M20.2322
     6 113H ORDER REQUESTED BY THE USER ON THE CARD FOR SELECTIVE NODE VM20.2323
     7OLTAGE OUTPUT.   FOR PLOTTING PURPOSES, THESE OUTPUT         )    M20.2324
      WRITE (LUNIT6, 7468)                                              M20.2325
 7468 FORMAT ( 107H VARIABLES ARE TREATED AS THOUGH THEY WERE ORDINARY EM20.2326
     1MTP BRANCH CURRENTS (PLOT TYPE-CODE  '9' ).   BOTH THE       ,/,  M20.2327
     2 118H MAGNITUDE AND THE ANGLE USE THE ACTUAL NODE NAME FOR THE FIRM20.2328
     3ST IDENTIFYING VARIABLE,  AND THEN EITHER   'MAG   '   OR     ,/, M20.2329
     4  45H 'ANGLE'   FOR THE SECOND.        IN RESPONSE,               M30. 174
     5  39H TO AN IREQ REQUEST DURING MAY OF 1981,        ,/,           M30. 175
     6  48H RECTANGULAR OUTPUTS (USING 'REAL' AND 'IMAG' AS,            M30. 176
     7  44H SECOND IDENTIFYING NAMES FOR PLOTTING) HAVE   ,/,           M30. 177
     8  45H BEEN APPENDED.   IN THE PRINTED OUTPUT, THIS,               M30. 178
     9  48H ALTERNATE RECTANGULAR OUTPUT VECTOR FOLLOWS THE  )          M30. 179
      WRITE (LUNIT6, 7466)                                              M30. 180
 7466 FORMAT (                                                          M30. 181
     1  48H ORIGINAL POLAR ONE, WITH THE SAME NODE ORDERING,            M30. 182
     2  44H (ONLY WITH 'REAL' TAKING THE PLACE OF 'MAG'   ,/,           M30. 183
     3  41H AND 'IMAG' TAKING THE PLACE OF 'ANGLE').,                   M30. 184
     4  45H   THERE IS COLUMN ALIGNMENT (THE RECTANGULAR   ,/,          M30. 185
     5  42H OUTPUT FOR ANY NODE IS LOCATED VERTICALLY,                  M30. 186
     6  39H BELOW THE CORRESPONDING POLAR OUTPUT).         )            M30. 187
      IF ( IVOLT  .EQ.  1 )                                             M20.2331
     1 WRITE (LUNIT6, 7469)   ( BUS(I), I=LL2, NTOT )                   M35.1282
 7469 FORMAT ( 108H ACTUALLY, THE USER REQUESTED THE OUTPUT OF ALL NODE M20.2333
     1VOLTAGES BY MEANS OF A  '1'-PUNCH IN COLUMN NUMBER TWO.      ,/,  M20.2334
     2  75H IN THIS CASE, THE ORDERING OF NODES FOR OUTPUT PURPOSES IS AM20.2335
     3S FOLLOWS ....      ,/,  ( 1X, 20A6 )   )                         M20.2336
      WRITE (LUNIT6, 7470)                                              M20.2337
 7470 FORMAT ( 1X )                                                     M20.2338
 7473 IF ( IPLOT  .LT.  0 )   GO TO 7462                                M20.2339
      IF ( KNT  .GT.  1 )   GO TO 7459                                  M20.2340
      IF ( NSOLVE .EQ. 1 )  GO TO 7459
      N7 = NTOT + 4                                                     M30. 188
      IF ( KEXACT .EQ. 88333 )  N7 = N7 + 2
      N6 = 0                                                            M20.2342
      N5 = 4 * NUMNVO                                                   M30. 189
      N85 = N5
      IF ( KEXACT .EQ. 88333 )  N85 = 2 * N5
      N71 = 2 * NUMNVO
      N74 = 0
      IF ( KEXACT .EQ. 88333 )  N74 = NUMNVO
      LSTAT(32) = N5
      IF ( KEXACT .EQ. 88333 )  LSTAT(32) = N85+N71
      BEGMAX(1) = 0.0                                                   M31.2675
      REWIND LUNIT4                                                     M29.1667
      IF ( KEXACT .NE.  88333 )
     1 WRITE (LUNIT4)  DATE1, TCLOCK, N7, N6, N5, N85,
     1 ( BUS(I), I=1, NTOT), TEXT1, TEXT2, TEXT3, TEXT4                 M30. 190
      IF ( KEXACT .EQ.  88333 )
     1 WRITE (LUNIT4)  DATE1, TCLOCK, N7, N6, N5+N71, N85+N71,
     1 ( BUS(I), I=1, NTOT), TEXT1, TEXT2, TEXT3, TEXT4 ,TEXT5, TEXT6
      N8 = NTOT + 1                                                     M20.2347
      N9 = NTOT + 2                                                     M20.2348
      N18 = NTOT + 3                                                    M30. 191
      N19 = NTOT + 4                                                    M30. 192
      N28 = NTOT + 5
      N29 = NTOT + 6
      LTEMP = 1
      IF ( KEXACT .EQ. 88333 )  LTEMP = 2
      WRITE (LUNIT4) ( ( JCH2(I), JCH2(I), I=1, NUMNVO), JJ=1,LTEMP ),
     1               ( ( JCH2(I), JCH2(I), I=1, NUMNVO), JJ = 1,LTEMP),
     1               ( JCH2(I), JCH2(I), I = 1, N74 ),
     2 (((N8, N9, I=1, NUMNVO),(N18, N19, I=1, NUMNVO)),JJ=1,LTEMP),
     2 (N28, N29, I = 1, N74 ),
     2 (N28, N29, I = 1, N74 )
      IF ( IPRSUP  .GE.  1 )                                            M21.  73
     1 WRITE (LUNIT6, 7477)  N5, N6, N7, N8, N9, NUMNVO,                M21.  74
     2                       ( JCH2(I), I=1, NUMNVO )                   M35.1285
 7477 FORMAT ( /,  22H LUNIT4 HEADER RECORD.,                           M21.  76
     1 48H      N5      N6      N7      N8      N9  NUMNVO   ,/,        M21.  77
     2 22X,  6I8,  5X,  34H(JCH2(I), I=1, NUMNVO)  FOLLOW ...      ,/,  M35.1286
     3 ( 1X, 20I6 )   )                                                 M21.  79
 7459 IF ( M4PLOT  .EQ.  0 )   GO TO 7461                               M32.3607
      IF ( KEXACT .EQ. 88333 )  GO TO 3010
 3070 VOLTI(1) = D4                                                   
      IP = 1                                                            M29.1672
      IF ( KEXACT .NE. 88333 )  GO TO 3075
      IQ = ( KNT - 1 ) * 4 * NUMNVO
      IHALF = IQ + 2 * NUMNVO
      J = 1
      DO 3030  I = 1, NUMNVO
      VOLTI(IP+1) = FSHOLD (IQ+J)
      VOLTI(IP+2) = FSHOLD (IQ+J+1)
      J = J + 2
 3030 IP = IP + 2
      K = 1
      DO 3040 I = 1, NUMNVO
      VOLTI(IP+1) = FSHOLD (IHALF+K)
      VOLTI(IP+2) = FSHOLD (IHALF+K+1)
      K = K + 2
 3040 IP = IP + 2
 3075 DO 7460  I=1, NUMNVO                        
      VOLTI(IP+1) = SOLR(I)                                             M29.1674
      VOLTI(IP+2) = SOLI(I)                                             M29.1675
 7460 IP = IP + 2                                                       M29.1676
      DO 17460  I=1, NUMNVO                                             M30. 195
      VOLTI(IP+1) = VOLTK(I)                                            M31.2676
      VOLTI(IP+2) = VOLT(I)                                             M31.2677
17460 IP = IP + 2                                                       M30. 198
      IF ( KEXACT .NE. 88333 )  GO TO 3066
      N78 = N77 * 2 * N899 + (KNT-1)*2*NUMNVO
      J = 1
      DO 3069  I = 1, NUMNVO
      VOLTI(IP+1) = (VOLTI(J+1)/VOLTI(J+N77+1) -1.) * 100.
      FSHOLD(N78+J) = VOLTI(IP+1)
      VOLTI(IP+2) = (VOLTI(J+2)/VOLTI(J+N77+2) -1.) * 100.
      FSHOLD(N78+J+1) = VOLTI(IP+2)
      J = J + 2
 3069 IP = IP + 2
 3066 IF ( IPRSUP .GE. 3 )
     1   WRITE(LUNIT6,*) ' AFTER DO 17460, IP,VOLTI, FSHOLD=',
     1  IP,(VOLTI(IJ), IJ=1,IP), (FSHOLD(IJ),IJ=N78+1,N78+J)
      CALL PLTFIL ( IP )                                                M29.1677
      GO TO 7462                                                        M29.1678
 3010 JTEMP = KNT
      IF ( NSOLVE .EQ. 1  .AND. KNT .EQ. 1 )
     1   N899 = ALOGZ(FMAXFS/FMINSV) / ALOGZ(-DELFFS) + 2.5
      IF ( NSOLVE .EQ. 1 )  JTEMP = KNT + N899
      N77 = 4 * NUMNVO
      IK = (JTEMP-1) * N77
      DO 3015  I = 1, NUMNVO
      FSHOLD(IK+1) = SOLR(I)
      FSHOLD(IK+2) = SOLI(I)
 3015 IK = IK + 2
      DO 3020 I = 1 , NUMNVO
      FSHOLD(IK+1) = VOLTK(I)
      FSHOLD(IK+2) = VOLT( I)
 3020 IK = IK + 2
      IF ( NSOLVE .EQ. 0 )  GO TO 7462
      GO TO 3070
 7461 WRITE (LUNIT4)  D4,  ( SOLR(I), SOLI(I), I=1, NUMNVO ),           M30. 199
     1                    (VOLTK(I), VOLT(I), I=1, NUMNVO)              M31.2678
 7462 IF ( KEXACT .EQ. 88333  .AND. IPRSUP .LT. 1 )  GO TO 375
      IF ( IOUT  .EQ.  1 )        
     1 WRITE (LUNIT6, 7463)  KNT, FMINFS,                               M30. 201
     2                       ( SOLR(I), SOLI(I), I=1, NUMNVO )          M20.2355
 7463 FORMAT ( /,  30H OUTPUT VECTOR FOR STEP NUMBER, I5,               M20.2356
     1  16H .   FREQUENCY =,   E15.6,    4H HZ.   ,/,                   M20.2357
     2 ( 1X,  8E16.7)  )                                                M20.2358
      IF ( IOUT  .EQ.  1 )                                              M30. 202
     1 WRITE (LUNIT6, 7464)  (VOLTK(I), VOLT(I), I=1, NUMNVO)           M31.2679
 7464 FORMAT ( 1X,  8E16.7 )                                            M30. 204
  375 KNT = KNT + 1                                
      LASTOV = NCHAIN                                                   M20.2362
      NCHAIN = 8                                                        M20.2363
      IF ( KEXACT .NE. 88333 .OR. NSOLVE .EQ. 0 )  GO TO 9900
      IF ( NFSOUT .EQ. 3179 )   GO TO 9750
      IF ( KNT .EQ. 2 )  DUM123 = FMINSV * ((-DELFFS) **(N899-2))
      IF ( (FMINFS/DUM123 + EPSILN  -1. ) .GE. 0 ) GO TO 3080
      GO TO 9900                                                        M32.3610
 3080 WRITE(LUNIT6, 3081)
 3081 FORMAT(/////) 
      WRITE(LUNIT6, 3082)
 3082 FORMAT( 20X,'*****************************************************
     1**************************************'  )
      WRITE(LUNIT6, 3083) DATE1
 3083 FORMAT( 20X, '*', 89X,'*',
     2 /,20X, '*', 89X, '*', /, 20X, '*',18X, 'LINE MODEL FREQUENCY SCAN
     3 COMPARISON TABLE',12X,2A4,9X, '*')
      IF ( NUMRUN .EQ. 1 ) 
     1   WRITE (LUNIT6,3110)
 3110 FORMAT ( 20X, '*', 25X, 'ZERO-SEQUENCE IMPEDANCE TEST',36X, '*')
      IF ( NUMRUN .EQ. 2 )
     1  WRITE (LUNIT6, 3112)
 3112 FORMAT ( 20X, '*', 23X,'POSITIVE-SEQUENCE IMPEDANCE TEST',34X,'*')
      IF ( NUMRUN .EQ. 3 )
     1 WRITE( LUNIT6, 3114)
 3114 FORMAT ( 20X, '*', 28X,'MUTUAL IMPEDANCE TEST', 47X, '*' )
      WRITE (LUNIT6, 3116)
 3116 FORMAT ( 20X, '*', 89X, '*',/, 20X, 
     4 '*', 89X, '*' )
      WRITE(LUNIT6, 3082)
      WRITE(LUNIT6, 3085)
 3085 FORMAT( ///,31X, 'LINE MODEL BEING TESTED', 29X,
     1 'EXACT-PI MODEL',22X, ' % ERROR ',//,/, '  FREQUENCY', 2X, 
     1 'NODE', 6X, 'MAG', 8X, 'ANG', 8X, 'REAL', 8X, 'IMAG',10X, 'MAG', 
     2 8X, 'ANG', 8X, 'REAL', 7X, 'IMAG',9X,'PCTMAG  PCTANG' )
      NFSOUT = 3179
      FMINFS = FMINSV
      NF = N899 - 1
      NREF = 2 * NUMNVO
      IJK = 0
      DO 3100  I = 1, NF
      K = (I-1) * 4 * NUMNVO
      L = K + N899 * 4 * NUMNVO
      JPP = N899 * 4 * NREF + (I-1) * NREF
      IF ( I .EQ. 1 )  GO TO  3086
      FMINFS = FMINFS * (- DELFFS)
      N22 = JMOD(I-1,IOUT)
      IF ( N22 .NE. 0 )  GO TO 3100    !  THL, 10/30/89
 3086 IJK = IJK + 1
      IF ( IJK .GT. 101 )  GO TO 9900
      DO 3098  J = 1, NUMNVO
      NDUM = JCH2(J)
      N6 = 6
      CALL FLTOPT(FSHOLD(JPP+1),N6)
      CHHOLD = ANSI32(27:32)
      N6 = 6
      CALL FLTOPT(FSHOLD(JPP+2),N6)
      IF ( J .NE. 1 )  GO TO 3089
      WRITE ( LUNIT6, 3087) FMINFS, BUS(NDUM), (FSHOLD(K+M), M=1,2),
     1   (FSHOLD(K+NREF+M), M=1,2), (FSHOLD(L+M), M=1,2),
     1                      (FSHOLD(L+NREF+M), M = 1,2),
     2     CHHOLD, ANSI32(27:32)
 3087 FORMAT ( //,1P,1X, E10.3, 1X, A6, 1X, E10.3,1x,E10.3, 2X, E10.3,
     1  1x,E10.3, 4X,E10.3,1x,E10.3, 2X, E10.3,1x,E10.3,5X,0P,
     2  A6, 3X, A6 )
      GO TO 3092
 3089 WRITE (LUNIT6, 3091) BUS(NDUM), (FSHOLD(K+M),M=1,2),
     1   (FSHOLD(K+NREF+M), M=1,2), (FSHOLD(L+M), M=1,2),
     1                      (FSHOLD(L+NREF+M), M = 1,2),
     2     CHHOLD, ANSI32(27:32)
 3091 FORMAT(1P,12X,A6,1X,  E10.3,1x,E10.3, 2X, E10.3,
     1  1x,E10.3, 4X,E10.3,1x,E10.3, 2X, E10.3,1x,E10.3,5X,0P,
     2   A6, 3X, A6 )
 3092 K = K + 2
      L = L + 2
      IF ( IPRSUP .GE. 3 )
     1 WRITE(LUNIT6,*) ' FSHOLD FOR % ERROR:',JPP,
     2  (FSHOLD(JPP+M),M=1,2)
      JPP = JPP + 2
 3098 CONTINUE
 3100 CONTINUE
      GO TO 9900
 7481 ICHECK = 0                                                        M20.2365
      IF ( KEXACT .EQ. 88333 .AND. NSOLVE .EQ. 1 )  GO TO 3080
C     READ INPUT CARD USING CIMAGE                                         10545
 8121 CALL CIMAGE                                                          10546
      DECODE (80, 8134, ABUFF)  AUPPER                                     10547
 8134 FORMAT (13A6)                                                        10548
      DO 8139  I=1, 13                                                     10549
      IF( AUPPER(I)  .NE.  BLANK )  GO TO 8123                             10550
 8139 CONTINUE                                                             10551
      WRITE(LUNIT6, 8124)                                               M17. 542
 8124 FORMAT(  41H+BLANK CARD TERMINATING PLOT SPEC. CARDS.  )             10553
      GO TO 9205                                                           10555
 8123 ICHECK=1                                                             10556
      WRITE(LUNIT6, 8127)                                               M17. 543
 8127 FORMAT ( 46H+PLOT CARD DISCARDED IN SEARCH FOR BLANK CARD.   )    M16.1594
      GO TO 8121                                                           10559
 9205 LASTOV = NCHAIN                                                   M15. 779
      NCHAIN = 51                                                       M15. 780
      CALL RUNTYM (D1, D2)                                                 10561
      FLSTAT(3) = FLSTAT(3) + D1                                        M20.2366
      FLSTAT(4) = FLSTAT(4) + D2                                        M20.2367
      FLSTAT(9) = D1                                                       10564
      FLSTAT(10) = D2                                                      10565
      GO TO 9900                                                        M32.3611
 9700 LASTOV = NCHAIN                                                   M21.  80
      NCHAIN = 8                                                                
      LOOPSS(1) = 7766                                                  M32.3613
      GO TO 9900                                                        M32.3614
 9750 NFSOUT = 0     ! reset NFSOUT for 2nd and 3rd LMFS data cases
      GO TO 9900
 9800 LASTOV = NCHAIN                                                   M32.3615
      NCHAIN = 51                                                       M15. 782
      LSTAT(18) = 11                                                       10568
 9900 IF ( IPRSUP  .GE.  1 )                                            M32.3616
     1 WRITE ( LUNIT6, 4568 )                                           M32.3617
 4568 FORMAT ( 24H  "EXIT  MODULE OVER11." )                            M32.3618
99999 RETURN                                                               10569
      END                                                                  10570
      SUBROUTINE SMINT                                                  M20.2586
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED ONLY BY BRANDWAJN (TYPE-59) S.M. MODEL        M31.2684
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      REAL*8         TEXT1                                              M28.4287
      DATA  TEXT1   /  6H  1ST   /                                      M20.2596
      KARC = 0                                                          M38.2220
      LMSET = 0                                                         M20.2605
      N10 = 0                                                           M38.2221
      NWD = 24                                                                  
      N33 = 0                                                                   
      ILMASS = 0                                                                
      ILSTOR = 0                                                                
      NN10 = ISMDAT( 25 )                                               M37.3793
      IITER = 100                                                               
      AKM = EPSUBA                                                      M32.3619
      IF( AKM .LT. 2. )  AKM = 100.0                                    M37.3795
      AKN = 2. / ( 1. + DELTA2 / AKM )                                  M32.3621
      AKM = AKM / OMDT                                                  M33. 898
      IF ( IPRSUP  .GE.  1 )                                            M28.4288
     1 WRITE( LUNIT6, 6250 )  NUMSM, NST, DELTA2                        M39.1596
 6250 FORMAT ( /,  17H BEGIN  'SMINT' .,                                M39.1597
     1 24H   NUMSM     NST  DELTA2, /, 17X, 2I8, E15.6 )                M33. 900
      J30 = 1                                                                   
      DO 900 K = 1, NUMSM                                                       
      IN = 0                                                            M37.3798
      IA = ISMDAT( J30+2 )                                                      
      IA = NORDER( IA )                                                 M37.3800
 2    L = KODE( IA )                                                    M37.3801
      IF ( L  .LE.  IA  ) GO  TO  3                                     M39.1598
      IA = L                                                            M37.3802
      GO TO 2                                                           M20.2623
 3    CAR = E( IA )                                                     M39.1599
      CAI = F( IA )                                                     M37.3804
      IB = ISMDAT( J30+3 )                                                      
      IB = NORDER( IB )                                                 M37.3806
 4    L = KODE( IB )                                                    M39.1600
      IF ( L  .LE.  IB  ) GO  TO  5                                     M39.1601
      IB = L                                                            M37.3808
      GO TO 4                                                           M39.1602
 5    CBR = E( IB )                                                     M39.1603
      CBI = F( IB )                                                     M37.3810
      IC = ISMDAT( J30+4 )                                                      
      IC = NORDER( IC )                                                 M37.3812
 6    L = KODE( IC )                                                    M39.1604
      IF ( L  .LE.  IC  ) GO  TO  7                                     M39.1605
      IC = L                                                            M37.3814
      GO TO 6                                                           M39.1606
 7    CCR = E( IC )                                                     M39.1607
      CCI = F( IC )                                                     M37.3816
C     COMPUTE POSITIVE SEQUENCE CURRENTS    ****************************M33. 901
      Z( KARC+1 ) = CAR                                                 M38.2225
      Z( KARC+2 ) = CAI                                                 M38.2226
      Z( KARC+3 ) = CBR                                                 M38.2227
      Z( KARC+4 ) = CBI                                                 M38.2228
      Z( KARC+5 ) = CCR                                                 M38.2229
      Z( KARC+6 ) = CCI                                                 M38.2230
      A=(CBR+CCR)*ONEHAF                                                M20.2648
      B = ( CCI - CBI ) * SQRT32                                        M33. 902
      C1 = ( CCI+CBI ) * ONEHAF                                         M39.1608
      D1 = ( CBR - CCR ) * SQRT32                                       M33. 903
      REIA1 = ( CAR-A+B ) / 3.0                                         M39.1609
      AIMIA1 = ( CAI-C1+D1 ) / 3.0                                      M39.1610
      ADK = 1.0                                                         M39.1611
      ADS = 0.0                                                         M39.1612
      IDELTA = ISMDAT( J30+1 )                                                  
      IF( IDELTA  .EQ.  0 )     GO TO 423                                       
      ADK = ASQRT3                                                      M39.1615
      ADS = TWOPI / 12.0                                                M39.1616
 423  CAMAG = ( SQRTZ( REIA1**2 + AIMIA1**2 ) ) * ADK                   M39.1617
      CAANG = ATAN2Z( AIMIA1, REIA1 ) + ADS                             M39.1618
      IF( IPRSUP  .GE.  1 )                                             M39.1619
     1 WRITE( LUNIT6, 6251 )   CAMAG, CAANG                             M39.1620
 6251 FORMAT( 20H  POSITIVE SEQUENCE:, 4X, 5HAMPL=, 2X, E15.8,          M39.1621
     1 4X, 6HANGLE=, 2X, E15.8 )                                        M39.1622
C     COMPUTE ZERO AND NEGATIVE SEQUENCE CURRENTS ********************* M39.1623
      CA = ( CAR+CBR+CCR ) / 3.0                                        M39.1624
      CB = ( CAI+CBI+CCI ) / 3.0                                        M39.1625
      CMZER =  SQRTZ( CA**2 + CB**2 )                                   M39.1626
      CAZER = ATAN2Z( CB, CA )                                          M39.1627
      IF ( IPRSUP  .GE.  1 )                                            M20.2669
     1 WRITE( LUNIT6, 6252 )  CMZER, CAZER                              M39.1628
 6252 FORMAT( 6X, 14HZERO SEQUENCE:, 4X, 5HAMPL=, 2X, E15.8,            M39.1629
     1 4X, 6HANGLE=, 2X, E15.8 )                                        M39.1630
      CA = ( CAR-A-B ) / 3.0                                            M39.1631
      CB = ( CAI-C1-D1 ) / 3.0                                          M39.1632
      CMNEG = ( SQRTZ( CA**2 + CB**2 ) ) * ADK                          M39.1633
      CANEG = ATAN2Z( CB, CA ) - ADS                                    M39.1634
      IF( IPRSUP  .GE.  1 )                                             M39.1635
     1 WRITE( LUNIT6, 6253 )  CMNEG, CANEG                              M39.1636
 6253 FORMAT( 20H  NEGATIVE SEQUENCE:, 4X, 5HAMPL=, 2X, E15.8,          M39.1637
     1 4X, 6HANGLE=, 2X, E15.8 )                                        M39.1638
C     CALCULATE POSITIVE SEQUENCE TERMINAL VOLTAGE ******************** M39.1639
      KSM=KCONST+3*(K-NUMSM) - 2                                        M20.2680
      VAMAG = CREST (KSM )                                              M20.2681
      VAANG = TIME1 ( KSM )                                             M20.2682
      VAR = VAMAG * COSZ( VAANG )                                       M32.3624
      VAI = VAMAG * SINZ( VAANG )                                       M32.3625
      VAMAG = CREST( KSM+1 )                                            M32.3626
      VAANG = TIME1( KSM+1 )                                            M32.3627
      VBR = VAMAG * COSZ( VAANG )                                       M32.3628
      VBI = VAMAG * SINZ( VAANG )                                       M32.3629
      VAMAG = CREST( KSM+2 )                                            M32.3630
      VAANG = TIME1( KSM+2 )                                            M32.3631
      VCR = VAMAG * COSZ( VAANG )                                       M32.3632
      VCI = VAMAG * SINZ( VAANG )                                       M32.3633
      IF( IDELTA .EQ. 0 )    GO  TO  427                                        
      A = VAR                                                           M39.1641
      B = VAI                                                           M39.1642
      VAR = VAR - VBR                                                           
      VAI = VAI - VBI                                                   M39.1644
      VBR = VBR - VCR                                                           
      VBI = VBI - VCI                                                   M39.1646
      VCR = VCR - A                                                             
      VCI = VCI - B                                                     M39.1648
 427  A = ( VBR + VCR ) * ONEHAF                                        M39.1649
      B = ( VCI - VBI ) * SQRT32                                        M33. 905
      C1 = ( VCI + VBI ) * ONEHAF                                       M32.3636
      D1 = ( VBR - VCR ) * SQRT32                                       M33. 906
      A = ( VAR - A + B ) / 3.0                                         M32.3638
      B = ( VAI - C1 + D1 ) / 3.0                                       M32.3639
      VAMAG = SQRTZ( A**2 + B**2 )                                      M32.3640
      VAANG = ATAN2Z( B, A )                                            M32.3641
      VAMAG1 = VAMAG * THTW                                             M20.2687
      ILK = ISMDAT( J30 )                                                       
      IF (  ILK  .EQ.  1  )  GO  TO   12                                M39.1650
      IF(  IPRSUP  .GE.  2  )                                           M20.2690
     1 WRITE( LUNIT6, 6254 )   K, ILK, VAMAG, VAANG                     M39.1651
 6254 FORMAT ( /,  38H IN  'SMINT' ,  B4 DUAL CURRENT SPLIT.,           M39.1652
     1 16H       K     ILK, 10X, 5HVAMAG, 10X, 5HVAANG, /,              M39.1653
     2 38X,  2I8,  2E15.6  )                                            M20.2694
C     CALCULATE LOAD ANGLES FOR MULTIPLE GENERATORS ********************M20.2695
      D1 = 0.0                                                          M20.2696
      Q1 = 0.0                                                          M20.2697
      N2 = N10 + 1                                                      M38.2231
      N3 = N10 + ILK                                                    M38.2232
      DO 10 N1 = N2, N3                                                 M39.1654
      I75 = 101 * K - 74                                                        
      D1 = D1 + ELP( I75+2 )                                                    
 10   Q1 = Q1 + ELP( I75+3 )                                                    
      IF( D1 .NE. 0.0  .AND.  Q1 .NE. 0.0 )    GO TO 351                M38.2236
      KILL  =  106                                                      M38.2237
      K1 = ISMDAT( J30+2 )                                                      
      BUS1 = BUS( K1 )                                                          
      LSTAT( 14 ) = K                                                   M33. 910
      LSTAT( 18 ) = NCHAIN                                              M33. 911
      LSTAT( 19 )  =  300                                               M20.2707
      RETURN                                                            M20.2708
 351  D2  =  VAANG  -  CAANG                                            M20.2709
      PT = CAMAG * COSZ( D2 ) / D1                                      M20.2710
      QT = CAMAG * SINZ( D2 ) / Q1                                      M20.2711
C     CALCULATE NEGATIVE AND ZERO SEQUENCE CURRENT SPLIT COEFFICIENTS***M23.1903
      SCN  =  VAANG - CANEG                                             M22.3388
      PTN  =  CMNEG * COSZ( SCN ) / D1                                  M22.3389
      QTN  =  CMNEG * SINZ( SCN ) / Q1                                  M22.3390
      SCN  =  VAANG - CAZER                                             M22.3391
      PT0  =  CMZER * COSZ( SCN ) / D1                                  M22.3392
      QT0  =  CMZER * SINZ( SCN ) / Q1                                  M22.3393
 11   IK = N10 + 1                                                      M39.1658
      K14 = 4 * IK                                                      M37.3829
      I75 = 101 * IK - 74                                                       
      ARP = ELP( I75+2 )                                                        
      AIP = ELP( I75+3 )                                                        
      D1 = PT * ARP                                                     M37.3832
      Q1 = QT * AIP                                                     M37.3833
      CAMAG1 = SQRTZ( D1**2 + Q1**2 )                                   M20.2715
      CAANG1 = VAANG - ATAN2Z( Q1, D1 )                                 M20.2716
      IF ( IPRSUP  .GE.  2 )                                            M20.2717
     1 WRITE( LUNIT6, 6255 )  N1, ILK, CAMAG1, CAANG1                   M39.1659
 6255 FORMAT ( /,  21H AFTER CURRENT SPLIT.,  16H      N1     ILK,      M39.1660
     1 10X,  6HCAMAG1,  10X,  6HCAANG1  ,/,  21X,  2I8,  2E16.6  )      M20.2720
C     SPLIT THE NEGATIVE AND ZERO SEQUENCE CURRENTS ********************M23.1904
      D1 = PTN * ARP                                                    M39.1661
      Q1 = QTN * AIP                                                    M39.1662
      CMNEG = SQRTZ( D1**2 + Q1**2 )                                    M39.1663
      CANEG = VAANG - ATAN2Z( Q1, D1 )                                  M39.1664
      D1 = PT0 * ARP                                                    M39.1665
      Q1 = QT0 * AIP                                                    M39.1666
      CMZER = SQRTZ( D1**2 + Q1**2 )                                    M39.1667
      CAZER = VAANG - ATAN2Z( Q1, D1 )                                  M39.1668
      GO TO 13                                                          M20.2721
 12   CAMAG1 = CAMAG                                                    M39.1669
      CAANG1 = CAANG                                                    M20.2723
 13   CCR = CAMAG1 * COSZ ( CAANG1 )                                    M39.1670
      CCI = CAMAG1 * SINZ ( CAANG1 )                                    M20.2725
      WRITE( LUNIT6, 6256 )  CMNEG, CANEG, CMZER, CAZER                 M39.1671
 6256 FORMAT( 2X, 42HTABULATION OF NEG. AND ZERO SEQU. CURRENTS, /,     M39.1672
     1 2X, 9HNEG. SEQ.,2E20.7,/,2X,9HZER. SEQ., 2E20.7)                 M39.1673
C     CALCULATE ELECTRICAL POSITION OF THE D-AXIS   ********************M20.2726
      CAI = VAMAG * SINZ( VAANG )                                       M38.2243
      CAR = VAMAG * COSZ( VAANG )                                       M38.2244
      N1 = N10 + 1                                                      M23.1905
      I26 = 101 * N1 - 100                                                      
      I75 = I26 + 26                                                            
      I30 = 30 * N1 - 29                                                        
      ES = ELP( I26 )                                                           
      DS = ELP( I26+1 )                                                         
      CS = ELP( I26+8 )                                                         
      XL = ELP( I26+18 )                                                        
      BS = ELP( I26+19 )                                                        
      SSLD = ELP( I26+21 )                                                      
      SSAD = ELP( I26+22 )                                                      
      SSLQ = ELP( I26+23 )                                                      
      SSAQ = ELP( I26+24 )                                                      
      SSD = ES - XL                                                     M37.3851
      SSG = CS - XL                                                     M37.3852
      B6 = SSG / SSD                                                    M37.3853
      ACDF = 1.0                                                        M23.1910
      ACDE = 1.0                                                        M23.1911
      ISACT = ISMDAT( I30+8 )                                                   
      D1 = CAR + BS * CCR - CS * CCI                                    M38.2248
      Q1 = CAI + BS * CCI + CS * CCR                                    M38.2249
      D1=ATAN2Z(Q1,D1)                                                  M20.2736
C     CALCULATE PARK'S CURRENTS AND VOLTAGES   *************************M20.2737
      Q1 = CAMAG1 * THTW                                                M39.1674
      D2 = CAANG1 - D1                                                  M39.1675
      CID = Q1 * SINZ( D2 )                                             M39.1676
      CIQ = Q1 * COSZ( D2 )                                             M39.1677
      D2 = VAANG - D1                                                   M32.3643
      VQ = VAMAG1 * COSZ( D2 )                                          M32.3644
      CIF = ( VQ + BS * CIQ - ES * CID ) / DS                           M32.3645
C     ADD NEGATIVE SEQUENCE CORRECTIONS, IF NEEDED *   *   *   *   *   *M32.3646
      ADELD = CMNEG * COSZ( CANEG ) / CAMAG1                            M32.3647
      INEG = 0                                                          M32.3648
      IF( ABSZ( ADELD ) .GT. (10. * EPSILN ) )  INEG = 2                M32.3649
      CAMNEG = CMNEG * THTW                                             M32.3650
      N3 = 6                                                            M32.3651
      CALL MOVER0( X1( 1 ), N3 )                                        M37.3855
      CAN = -CANEG - D1                                                 M32.3653
      IF( INEG .LT. 1 )  GO  TO  300                                    M32.3654
      X1( 1 ) = CAMNEG * SINZ( CAN )                                    M37.3856
      X1( 2 ) = CAMNEG * COSZ( CAN )                                    M37.3857
      X1( 3 ) = 1.                                                      M37.3858
      X1( 4 ) = 1.                                                      M37.3859
      X1( 5 ) = AKM                                                     M37.3860
      X1( 6 ) = AKN                                                     M37.3861
      CALL UNCOR( X1(1), N1 )                                                   
 300  ACDE = 1.                                                         M32.3662
      ACDF = 1.                                                         M32.3663
C     CALCULATE THE TOTAL FLUX IN THE AIR-GAP      *   *   *   *   *   *M32.3665
      SF2 = ( CID + X1(1) ) / ELP( I26+20 ) + CIF + X1( 3 ) + X1( 4 )   M37.3863
      SF3 = ( CIQ + X1(2) ) / ELP( I26+20 ) + X1( 5 ) + X1( 6 )         M37.3864
      SFT = SQRTZ( SF2**2 + (SF3*B6)**2 )                               M37.3865
      IF( ISACT .EQ. 0 )  GO  TO  306                                           
      IPP = 0                                                           M32.3669
      IDD = 0                                                           M32.3670
      ACEE = 1.                                                         M32.3671
      ACEF = 1.                                                         M32.3672
      IF( SSLD .GE. SFT  .AND.  SSLQ .GE. SFT )      GO TO 306          M37.3867
      IF( SSLD .GE. SFT )  GO  TO  301                                  M37.3868
      ACDE = ( 1.0 - SSAD * SFT ) / ( 1.0 - SSAD * SSLD )               M32.3675
      IF( INEG .GT. 1 )  CALL SLOPE( SFT, SSLD, SSAD, ACEE)             M32.3676
 301  IF( SSLQ .GE. SFT )  GO  TO  302                                  M37.3869
      ACDF = ( 1.0 - SSAQ * SFT ) / ( 1.0 - SSAQ * SSLQ )               M32.3678
      IF( INEG .GT. 1 )  CALL SLOPE( SFT, SSLQ, SSAQ, ACEF )            M32.3679
      GO TO 302                                                         M32.3680
 304  CIF1 = CIF                                                        M32.3681
      CID1 = D1                                                         M32.3682
      IPP = IPP + 1                                                     M32.3683
      SF2 = ( CID + X1(1) ) / ELP( I26+20 ) + CIF + X1( 3 ) + X1( 4 )   M37.3870
      SF3 = ( CIQ + X1(2) ) / ELP( I26+20 ) + X1( 5 ) + X1( 6 )         M37.3871
      SFT = SQRTZ( SF2**2 + (SF3*B6)**2 )                               M35.1288
      ACEE = 1.                                                         M32.3687
      ACDE = 1.                                                         M32.3688
      IF( SFT .LE. SSLD )  GO TO 311                                    M32.3689
      ACDE = 1.0 / ( 1.0 + SSAD * ( SFT - SSLD ) )                      M32.3690
      IF( INEG .GT. 1 )    CALL SLOPE( SFT,SSLD,SSAD,ACEE)              M37.3872
 311  ACDF = 1.                                                         M32.3692
      ACEF = 1.                                                         M32.3693
      IF( SFT .LE. SSLQ )  GO TO 302                                    M32.3694
      ACDF = 1.0 / ( 1.0 + SSAQ * ( SFT - SSLQ ) )                      M32.3695
      IF( INEG .GT. 1 )    CALL SLOPE( SFT,SSLQ,SSAQ,ACEF )             M37.3873
 302  DS = ELP( I26+1 ) * ACDE                                          M37.3874
      ES = SSD * ACDE + XL                                              M37.3875
      CS = SSG * ACDF + XL                                              M37.3876
      B1 = CAR + BS * CCR - CS * CCI                                    M32.3700
      B2 = CAI + BS * CCI + CS * CCR                                    M32.3701
      D1 = ATAN2Z(B2,B1)                                                M32.3702
      D2 = CAANG1  - D1                                                 M32.3703
      CID = Q1 * SINZ( D2 )                                             M32.3704
      CIQ = Q1 * COSZ( D2 )                                             M32.3705
      D2 = VAANG - D1                                                   M32.3706
      VQ = VAMAG1 * COSZ( D2 )                                          M32.3707
      CIF = ( VQ + BS * CIQ - ES * CID ) / DS                           M32.3708
      CAN = -CANEG - D1                                                 M32.3709
      IF( INEG .EQ. 0 )  GO  TO  303                                    M32.3710
      X1( 1 ) = CAMNEG * SINZ( CAN )                                    M37.3877
      X1( 2 ) = CAMNEG * COSZ( CAN )                                    M37.3878
      X1( 3 ) = ACEE                                                    M37.3879
      X1( 4 ) = ACEF                                                    M37.3880
      X1( 5 ) = AKM                                                     M37.3881
      X1( 6 ) = AKN                                                     M37.3882
      CALL UNCOR( X1(1), N1 )                                           M37.3883
 303  IF( IPP .EQ. 0 )  GO TO 304                                       M32.3718
      IF (IDD .GT. 0 )  GO TO 306                                       M32.3719
      ADELD = ABSZ( ( D1-CID1 ) / CID1 ) + ABSZ( (CIF1-CIF) / CIF1 )    M37.3884
      IF( ADELD .LE. EPSILN  )  GO TO  305                              M39.1678
      IF( IPP .LE. IITER )  GO  TO  304                                 M37.3886
      KILL = 210                                                        M23.1964
      LSTAT( 19 ) = 306                                                 M23.1965
      LSTAT( 14 ) = N1                                                  M23.1966
      RETURN                                                            M23.1967
 305  IDD = 2                                                           M39.1679
      GO TO 304                                                         M32.3724
 306  VF = -ELP( I26+6 ) * CIF                                          M37.3887
      VD = VAMAG1 * SINZ(D2)                                            M32.3725
      IF ( IPRSUP  .GE.  1 )                                            M20.2748
     1 WRITE( LUNIT6, 6257 )  N1, D1, VD, VQ, VF, CID, CIQ, CIF         M39.1680
 6257 FORMAT ( /, 8X,  8HTERMINAL,  10X,  5HDELTA,  13X,  2HVD,         M39.1681
     1 13X,  2HVQ,  13X,  2HVF,  12X,  3HCID,  12X,  3HCIQ,             M20.2751
     2 12X,  3HCIF  ,/,  8X,  I8,  7E15.6  )                            M20.2752
C     STORE THE ELECTRICAL VARIABLES IN CORRESPONDING ARRAYS ***********M20.2753
      CID1 = CID                                                        M39.1682
      CIQ1 = CIQ                                                        M39.1683
      CIF1 = CIF                                                        M39.1684
C     STORE COEFFICIENTS FOR THE CALCULATION OF EQUIVALENT SATURATED****M23.1969
C     INDUCTANCES. THE VALUES STORED HERE WILL BE NECESSRY FOR THE FIRSTM23.1970
C     TIME STEP EVEN FOR AN UNSATURATED MACHINE  ***********************M23.1971
      ELP( I75+2 ) = ACDE                                                       
      ELP( I75+3 ) = ACDF                                                       
 106  CID = CID + X1( 1 )                                               M37.3890
      CU( N33+8 ) = CID                                                 M37.3891
      CAN = CAN - 2.0 * OMDT                                            M33. 922
      IF( INEG .EQ. 2 )  CU( N33+8 ) = CID1 + CAMNEG * SINZ( CAN )      M37.3892
      CU( N33+1 ) = CID                                                         
      CIQ = CIQ + X1( 2 )                                               M37.3894
      CU( N33+9 ) = CIQ                                                 M37.3895
      IF( INEG .EQ. 2 )  CU( N33+9 ) = CIQ1 + CAMNEG * COSZ( CAN )      M37.3896
      CU( N33+2 ) = CIQ                                                         
      CI0  =  CMZER * COSZ( CAZER ) * SQRT3                             M22.3411
      CU( N33+3 )  =  CI0                                                       
      CU( N33+11 ) = VF                                                         
      CIF = CIF + X1( 3 )                                               M37.3900
      CU( N33+4 ) = CIF                                                         
      CIKD = X1( 4 )                                                    M37.3902
      CU( N33+5 )  =  CIKD                                                      
      CIG = X1( 5 )                                                     M37.3904
      CU( N33+6 )   =  CIG                                                      
      CIKQ = X1( 6 )                                                    M37.3906
      CU( N33+7 )  =  CIKQ                                                      
C     CONVERT PARK'S CURRENTS TO PHASE QUANTITIES **********************M23.1986
      D1 = D1 + TWOPI / 4.0                                             M39.1685
      CI0 = CI0 * ASQRT3                                                M33. 923
      CA1 = COSZ( D1 )                                                  M38.2253
      SA1 = SINZ( D1 )                                                  M38.2254
      TSC1 = -CA1 * ONEHAF                                              M38.2255
      TSD1 = SA1 * SQRT32                                               M33. 924
      CB2 = TSC1 + TSD1                                                 M39.1686
      CC2 = TSC1 - TSD1                                                 M39.1687
      TSC1 = -SA1 * ONEHAF                                              M38.2256
      TSD1 = CA1 * SQRT32                                               M33. 925
      SB2 = TSC1 - TSD1                                                 M39.1688
      SC2 = TSC1 + TSD1                                                 M39.1689
      CU( N33+12 ) = (CA1 * CID + SA1 * CIQ ) / THTW + CI0                      
      CU( N33+13 ) = (CB2 * CID + SB2 * CIQ ) / THTW + CI0                      
      CU( N33+14 ) = (CC2 * CID + SC2 * CIQ ) / THTW + CI0                      
C     INITIALIZE THE MECHANICAL VARIABLES.   ***************************M20.2773
      CZ = ELP( I26+25 )                                                        
      D3 = D1 / CZ                                                      M20.2776
      D4 = OMEGA / CZ                                                   M20.2777
      AC1 = (ES * CID + DS * CIF + ELP( I26+3 ) * ACDE * CIKD) / OMEGA  M38.2258
      AC2 = ( ELP( I26+9 ) * CIG + ELP( I26+11 ) * CIKQ ) * ACDF        M38.2259
      AC2 = ( AC2 + CS * CIQ ) / OMEGA                                  M38.2260
      D22 = ( AC1 * CIQ - AC2 * CID ) * CZ * TENM6                              
      Q22 = -( VF * CZ * TENM6 * CIF ) / D4                             M39.1690
      D2 = ( (ES - CS) * CID1 + DS * CIF1 ) * CIQ1                      M22.3440
      D2 = D2 * TENM6 / D4                                              M30. 208
      NUMASK = ISMDAT( I30+11 )                                                 
      NLOCG = ISMDAT( I30+12 )                                                  
      NLOCE = ISMDAT( I30+13 )                                                  
      Q1 = -( VF * CIF1 * CZ * TENM6 ) / D4                             M37.3917
      IF( NLOCE .EQ. 0 )   Q1 = 0.0                                     M37.3918
      TOTET = D2 + Q1                                                           
      NUM1 = NUMASK - 1                                                 M37.3920
      NUM2 = NUMASK + NUMASK                                            M37.3921
      NUM4 = NUM2 + NUM2                                                M37.3922
      NUM6 = NUM4 + NUM2                                                M38.2262
      N2 = ILMASS * 12 + NUM2                                                   
      N4 = ILSTOR + NLOCG                                               M33. 929
      HISTQ( N4 ) = D3                                                          
      TOTDMP = 0.0                                                      M20.2793
      D5 = 0.0                                                          M20.2794
      N6 = N2 + 1                                                       M37.3927
      DO 15 N3 = 1, NUMASK                                              M37.3928
      D5 = D5 + SHP( N6 )                                                       
      N7 = N6 + NUM4                                                    M37.3930
      TOTDMP = TOTDMP + SHP( N7 )                                               
 15   N6 = N6 + 1                                                       M37.3932
      TOT = ( TOTET + TOTDMP * D4 ) / D5                                M20.2800
C     INITIALIZE TORQUES FOR GENERATOR NO.N10+1   **********************M20.2801
      ILB = ILSTOR + NUM4 + NUMASK                                      M38.2266
      DO 16  IL = 1, NUMASK                                             M38.2267
 16   HISTQ( IL+ILB ) = SHP( IL+N2 ) * TOT                              M38.2268
      IF ( IPRSUP  .LT.  2 )  GO TO  8818                               M37.3937
      WRITE( LUNIT6, 6258 )  N10, NLOCG, NLOCE, NUMASK                  M39.1691
 6258 FORMAT ( /,  29H AFTER TORQUE INITIALIZATION.,  12X,              M39.1692
     1 3HN10,  10X,  5HNLOCG,  10X,  5HNLOCE,  9X,  6HNUMASK   ,/,      M20.2808
     2 29X, 4(10X, I5))                                                 M20.2809
      N6 = ILB + 1                                                      M38.2269
      N7 = N6 + NUM1                                                    M37.3940
      WRITE( LUNIT6, 6259 )   ( HISTQ( I ), I = N6, N7 )                M39.1693
 6259 FORMAT ( /,  ( 1X,  5E21.12 )  )                                  M39.1694
 8818 NLOCG1 = ILB + NLOCG                                              M38.2271
      IF( NLOCE  .EQ.  0  )  GO TO 1500                                 M37.3944
      NLOCE1 = ILB + NLOCE                                              M38.2272
      HISTQ( NLOCE1 ) = HISTQ( NLOCE1 ) - Q1                            M38.2273
      AYE = HISTQ( NLOCE1 )                                             M38.2274
 1500 HISTQ( NLOCG1 ) = HISTQ( NLOCG1 ) - D2                            M38.2275
      AYR = HISTQ( NLOCG1 )                                             M38.2276
      IL = NLOCG                                                        M37.3950
      N4 = N4 - NLOCG                                                   M37.3951
      N20 = N2 + 3 * NUMASK                                                     
      IF ( NUMASK  .EQ.  1 )   GO TO 25                                 M20.2820
      IF ( IL  .EQ.  1 )   GO TO 18                                     M20.2821
C     CALCULATE  ANGLES TO THE LEFT OF THE ROTOR ***********************M20.2822
      N6 = N2 + NUM4 + 1                                                M37.3953
      N7 = N6 + NLOCG - 1                                               M37.3954
      D6 = 0.0                                                          M20.2824
      DO 19 N3 = N6, N7                                                 M37.3955
 19   D6 = D6 - SHP( N3 )                                                       
      D6 = D6 * D4                                                      M20.2827
      N6 = ILB                                                          M38.2278
      DO 20  IK = 1, IL                                                 M37.3958
      N6 = N6 + 1                                                       M37.3959
 20   D6 = D6 + HISTQ( N6 )                                                     
 21   IK = IL                                                           M38.2280
      IL = IL - 1                                                       M38.2281
      IF( IL .LT. 1 )   GO TO 18                                        M37.3961
      N3 = N2 + NUM4 + IK                                                       
      N6 = N4 + IK                                                      M37.3963
      N21  =  N20 + IK                                                  M20.2835
      D6 = D6  +  SHP( N3 ) * D4  -  HISTQ( ILB+IK )                    M38.2283
      HISTQ( N6-1 ) = HISTQ( N6 ) + D6 / SHP( N21-1 )                   M38.2284
      GO TO 21                                                          M20.2839
 18   IL = NLOCG                                                        M39.1695
      IF ( IL  .EQ.  NUMASK )   GO TO 24                                M20.2841
C     CALCULATE  ANGLES TO THE RIGHT OF THE ROTOR  *********************M20.2842
      D6 = 0.0                                                          M37.3966
      IF( IL  .EQ. 1  )    GO   TO  26                                  M25. 290
      N6 = N2 + NUM4 + 1                                                M37.3967
      N7 = N6 + NLOCG - 2                                               M37.3968
      DO 22 N3 = N6, N7                                                 M37.3969
 22   D6 = D6 - SHP( N3 )                                                       
      D6 = D6 * D4                                                      M37.3971
      N7 = NLOCG - 1                                                    M37.3972
      N6 = ILB                                                          M38.2286
      DO 23 IK = 1, N7                                                  M37.3974
      N6 = N6 + 1                                                       M37.3975
 23   D6 = D6 + HISTQ( N6 )                                             M38.2287
 26   IK = IL                                                           M38.2288
      IL = IL + 1                                                       M38.2289
      IF( IL .GT. NUMASK )   GO TO 24                                   M37.3977
      N3 = N2 + NUM4 + IK                                               M37.3978
      D6 = D6 + HISTQ( ILB+IK ) - SHP( N3 ) * D4                        M38.2290
      N6 = N4 + IK                                                      M37.3980
      N3 = N20 + IK                                                     M25. 292
      HISTQ( N6+1 ) = HISTQ( N6 ) - D6 / SHP( N3 )                      M38.2291
      GO TO 26                                                          M20.2859
 24   IF( NLOCE  .GT.  0  )      HISTQ( NLOCE1 ) = AYE + Q1             M38.2292
 25   HISTQ( NLOCG1 ) = AYR + D2                                        M38.2293
      D20 = D2                                                          M23.1992
C     STORE INITIAL (SYNCHRONOUS) SPEEDS ****************************** M38.2294
      N5 = N4 + NUMASK + 1                                              M38.2295
      N3 = N5 + NUM1                                                    M38.2296
      DO 27  IK = N5, N3                                                M38.2297
 27   HISTQ( IK ) = D4                                                  M38.2298
      IF( NUMASK .EQ. 1 )    GO TO 30                                   M37.3991
C     CALCULATE TORQUES ON THE SHAFT  **********************************M20.2874
      N6 = ILB - NUMASK                                                         
      DO 29 IK = 1, NUM1                                                M33. 930
      N3 = N20 + IK                                                     M20.2877
      N5 = N4 + IK                                                      M37.3993
      N6 = N6 + 1                                                       M37.3994
 29   HISTQ( N6 ) = ( HISTQ( N5 ) - HISTQ( N5+1 ) ) * SHP( N3 )         M38.2300
      IF( IPRSUP .LT. 2 )   GO   TO  30                                 M30. 211
      WRITE( LUNIT6, 6258 )  N10, NLOCG, NLOCE, NUMASK                  M39.1696
      N7 = ILB - 1                                                      M38.2301
      N6 = N7 - NUM1 + 1                                                M38.2302
      WRITE( LUNIT6, 6259 )  IN, ( HISTQ( I ), I = N6, N7 )             M39.1697
 30   N10 = N10 + 1                                                     M39.1698
      IN = IN + 1                                                       M39.1699
C     LOAD INITIAL CONDITIONS INTO TACS ARRAYS ************************ M38.2304
      KMSET = ISMDAT( I30+14 )                                          M37.4000
      IF( KMSET  .EQ. 0  )  GO TO 970                                           
      IPB = ILSTOR + NUMASK                                             M33. 933
      N31 = NUM2 + NUM1                                                 M37.4001
      DO 955 I = 1, KMSET                                               M39.1701
      LMSET = LMSET + 1                                                 M20.2900
      JMSET = ISMTAC(  LMSET  )                                         M20.2902
      IF( JMSET .GT. 0 )    GO TO 1696                                          
      JMSET = -JMSET                                                            
      IF( JMSET .GT. 7 )  GO  TO  1680                                          
      ETAC( LMSET ) = CU( N33+JMSET )                                           
      GO TO 955                                                         M39.1702
 1680 IF( JMSET .GT. 8 )    GO TO 1681                                          
      ETAC( LMSET ) = VD                                                M38.2312
      GO TO 955                                                         M39.1703
 1681 IF( JMSET .GT. 9 )    GO TO 1682                                          
      ETAC( LMSET ) = VQ                                                M38.2315
      GO TO 955                                                         M39.1704
 1682 IF( JMSET .GT. 10 )   GO TO 1683                                          
      ETAC( LMSET ) = ( VAR + VBR + VCR ) * ASQRT3                      M39.1705
      GO TO 955                                                         M39.1706
 1683 IF( JMSET .GT. 11 )   GO TO 1684                                          
      ETAC( LMSET ) = VF                                                M38.2321
      GO TO 955                                                         M39.1707
 1684 IF( JMSET .GT. 12 )   GO TO 1685                                          
      ETAC( LMSET ) = SFT                                               M38.2324
      GO TO 955                                                         M39.1708
 1685 IF( JMSET .GT. 13 )   GO TO 1686                                          
      ETAC( LMSET ) = ATAN2Z( SF3, SF2 )                                M38.2327
      GO TO 955                                                         M39.1709
 1686 IF( JMSET .GT. 14 )   GO TO 1687                                          
      ETAC( LMSET ) = D22                                               M38.2330
      GO TO 955                                                         M39.1710
 1687 IF( JMSET .GT. 15 )   GO TO 1688                                          
      ETAC( LMSET ) = Q22                                               M38.2333
      GO TO 955                                                         M39.1711
 1688 IF( JMSET .EQ. 17 )   GO TO 1689                                          
      ETAC( LMSET ) = AC1                                               M38.2336
      GO TO 955                                                         M39.1712
 1689 ETAC( LMSET ) = AC2                                               M38.2338
      GO TO 955                                                         M39.1713
 1696 IF( JMSET .GT. NUM2 ) GO TO 1697                                          
      N9 = ILSTOR +  JMSET                                              M33. 937
      ETAC( LMSET ) = HISTQ( N9 )                                               
      GO TO 955                                                         M39.1714
 1697 JMSET = ILSTOR + JMSET + NUM2                                     M38.2341
      ETAC( LMSET ) = HISTQ( JMSET )                                            
 955  CONTINUE                                                          M39.1715
C     THE FOLLOWING ROUTINE PRINTS INFO REQUESTED BY USER WHICH EXISTS  M38.2343
C     IMMEDIATELY AFTER MACHINE INITIALIZATION                          M38.2344
 970  BUS4 = TEXT1                                                      M39.1716
      IF( ELP( I75 )  .EQ.  0.0 )   GO TO 1201                          M38.2346
C     PRINT INDUCTANCE AND RESTANCE VALUES OF MACH. N10 IN PHYSICAL UNITM20.2912
      K1 = ISMDAT( J30+2 )                                              M38.2347
      WRITE( LUNIT6, 6260 )   N10, BUS( K1 ), IN                        M39.1717
 6260 FORMAT ( //, 6H MACH , I2, 10X,                                   M39.1718
     1 114HDATA PARAMETERS AND INITIAL CONDITIONS OF NEXT MACHINE FOLLOWM20.2915
     2.     -----------------------------------------------        ,/,  M20.2916
     3 2H ',  A6,  8H'   UNIT,  I3  )                                   M20.2917
      WRITE ( LUNIT6, 91993 )  ELP( I26+2 )                             M37.4009
91993 FORMAT ( /, 106H MACHINE REACTANCES AND RESISTANCES, IN OHMS  (QUAM21.  84
     1NTITIES LABELED AS INDUCTANCES ARE ACTUALLY REACTANCES).     ,/,  M21.  85
     2 3X,  E15.7,  5X,  35HLF   = D-AXIS FIELD SELF INDUCTANCE    )    M21.  86
      WRITE ( LUNIT6, 91994 )  ELP( I26+1 )                             M37.4010
91994 FORMAT (3X, E15.7, 5X, 46HLAF  = D-AXIS FIELD-ARMATURE MUTUAL INDUM20.2922
     1CTANCE   )                                                        M20.2923
      WRITE ( LUNIT6, 91995 )  ELP( I26+4 )                             M37.4011
91995 FORMAT (3X, E15.7, 5X, 44HLFKD = D-AXIS FIELD-DAMPER MUTUAL INDUCTM20.2925
     1ANCE   )                                                          M20.2926
      WRITE ( LUNIT6, 91996 )  ELP( I26 )                               M37.4012
91996 FORMAT (3X, E15.7, 5X, 62HLD   = D-AXIS ARMATURE SELF INDUCTANCE (M20.2928
     1SYNCHRONOUS REACTANCE)   )                                        M20.2929
      WRITE ( LUNIT6, 91997 )  ELP( I26+3 )                             M37.4013
91997 FORMAT (3X, E15.7, 5X, 47HLAKD = D-AXIS ARMATURE-DAMPER MUTUAL INDM20.2931
     1UCTANCE  )                                                        M20.2932
      WRITE ( LUNIT6, 91998 )  ELP( I26+5 )                             M37.4014
91998 FORMAT (3X, E15.7, 5X, 36HLKD  = D-AXIS DAMPER SELF INDUCTANCE   )M20.2934
      WRITE ( LUNIT6, 91999 )  ELP( I26+10 )                            M37.4015
91999 FORMAT (3X, E15.7, 5X, 39HLG   = Q-AXIS CIRCUIT 1 SELF INDUCTANCE)M20.2936
      WRITE ( LUNIT6, 92991 )  ELP( I26+9 )                             M37.4016
92991 FORMAT (3X, E15.7, 5X, 50HLAG  = Q-AXIS CIRCUIT 1-ARMATURE MUTUAL M20.2938
     1INDUCTANCE  )                                                     M20.2939
      WRITE ( LUNIT6, 92992 )  ELP( I26+12 )                            M37.4017
92992 FORMAT (3X, E15.7, 5X, 51HLGKQ = Q-AXIS CIRCUIT 1-CIRCUIT 2 MUTUALM20.2941
     1 INDUCTANCE   )                                                   M20.2942
      WRITE ( LUNIT6, 92993 )  ELP( I26+8 )                             M37.4018
92993 FORMAT (3X, E15.7, 5X, 38HLQ   = Q-AXIS ARMATURE SELF INDUCTANCE )M20.2944
      WRITE ( LUNIT6, 92994 )  ELP( I26+11 )                            M37.4019
92994 FORMAT (3X, E15.7, 5X, 50HLAKQ = Q-AXIS CIRCUIT 2-ARMATURE MUTUAL M20.2946
     1INDUCTANCE   )                                                    M20.2947
      WRITE ( LUNIT6, 92995 )  ELP( I26+13 )                            M37.4020
92995 FORMAT (3X, E15.7, 5X, 39HLKQ  = Q-AXIS CIRCUIT 2-SELF INDUCTANCE)M20.2949
      WRITE ( LUNIT6, 92996 )  ELP( I26+16 )                            M37.4021
92996 FORMAT (3X, E15.7, 5X, 30HL0   = ZERO SEQUENCE REACTANCE   )      M20.2951
      WRITE ( LUNIT6, 92997 )  ELP( I26+17 )                            M37.4022
92997 FORMAT (3X, E15.7, 5X, 31HR0   = ZERO SEQUENCE RESISTANCE   )     M37.4023
      WRITE ( LUNIT6, 92999 )  ELP( I26+6 )                             M37.4024
92999 FORMAT (3X, E15.7, 5X, 34HRF   = RESISTANCE OF FIELD WINDING   )  M20.2955
      WRITE ( LUNIT6, 93991 )  ELP( I26+19 )                            M37.4025
93991 FORMAT (3X, E15.7, 5X, 26HRA   = ARMATURE RESISTANCE   )          M20.2957
      WRITE ( LUNIT6, 93992 )  ELP( I26+7 )                             M37.4026
93992 FORMAT (3X, E15.7, 5X, 31HRKD  = D-AXIS DAMPER RESISTANCE   )     M20.2959
      WRITE ( LUNIT6, 93993 )  ELP( I26+14 )                            M37.4027
93993 FORMAT (3X, E15.7, 5X, 34HRG   = Q-AXIS CIRCUIT-1 RESISTANCE   )  M20.2961
      WRITE ( LUNIT6, 93994 )  ELP( I26+15 )                            M37.4028
93994 FORMAT (3X, E15.7, 5X, 34HRKQ  = Q-AXIS CIRCUIT-2 RESISTANCE   )  M20.2963
C     PRINT MECHANICAL DATA OF MACH. N10                                M20.2964
      WRITE( LUNIT6, 6261 )                                             M39.1719
 6261 FORMAT ( /,  83H MECHANICAL PARAMETERS OF GENERATOR, IN PHYSICAL UM39.1720
     1NITS AS SHOWN BY COLUMN HEADINGS.      )                          M20.2967
      WRITE( LUNIT6, 6262 )                                             M39.1721
 6262 FORMAT ( 3X,  17HMOMENT OF INERTIA,  12X,                         M39.1722
     1 33HSELF-DAMPING COEFFICIENTS OF MASS,                            M20.2970
     2 9X,  21HMUTUAL-DAMPING COEFF.,                                   M20.2971
     3 5X,  25HTORSIONAL SPRING CONSTANT    ,/,                         M20.2972
     4 7X,  13HOF ROTOR MASS,  10X,  15HSPEED-DEVIATION,                M20.2973
     5 6X,  14HABSOLUTE-SPEED,  9X,  21H(WITH FOLLOWING MASS),          M20.2974
     6 9X,  21H(WITH FOLLOWING MASS)       )                            M20.2975
      WRITE( LUNIT6, 6263 )                                             M39.1723
 6263 FORMAT ( 27H MILLION (N-M)/(RAD/SEC**2),                          M39.1724
     1  17X,  23HMILLION (N-M)/(RAD/SEC),                               M23.1997
     2  7X,  23HMILLION (N-M)/(RAD/SEC),                                M23.1998
     3  11X,  19HMILLION (N-M)/(RAD)        )                           M23.1999
      N15 = N2 + NUMASK                                                 M37.4029
      DO 1160 II = 1, NUMASK                                            M37.4030
      N15  =  N15  +  1                                                 M20.2988
      AJJ  =  SHP( N15 )                                                        
      ADL  =  SHP( N15+NUM4 )                                                   
      ADM  =  SHP( N15+NUMASK )                                                 
      ADK  =  SHP( N15+NUM2 )                                                   
      ADS  =  SHP( N15+3*NUMASK )                                               
 1160 WRITE( LUNIT6, 6264 )  AJJ, ADL, ADS, ADM, ADK                    M39.1725
 6264 FORMAT ( 1X,  E21.7,  5X,  2E20.7,  2E30.7  )                     M39.1726
 1201 IF( ELP( I75+1 )  .EQ.  0.0 )   GO TO 1218                        M38.2355
      WRITE( LUNIT6, 6265 )                                             M39.1727
 6265 FORMAT ( /, 102H TOTAL CURRENT INJECTED INTO NETWORK AT GENERATOR M39.1728
     1BUS, IN PHASE COORDINATES.   FOR A DUAL-MACHINE BUS,         ,/,  M20.3006
     2 113H THIS IS THE TOTAL INJECTION ( 'A' + 'B' ).   THE FIRST LINE M20.3007
     3DISPLAYS THE CURRENTS AS FOUND BY THE PHASOR NETWORK         ,/,  M20.3008
     4 114H SOLUTION (WHICH MAY BE UNBALANCED, IF THE NETWORK IS).   THEM20.3009
     5 2ND LINE SHOWS ONLY THE POSITIVE-SEQUENCE COMPONENT,        ,/,  M20.3010
     6 53H  MAGNITUDES OF THE CURRENTS ARE IN UNITS OF (AMPS) .)        M33. 947
      ADK = 1.0                                                         M39.1729
      ADS = 0.0                                                         M39.1730
      IF( IDELTA .EQ. 0 )   GO  TO  429                                 M39.1731
      ADK = SQRT3                                                       M39.1732
      ADS = TWOPI / 12.0                                                M39.1733
 429  D7 = Z( KARC+1 )                                                  M39.1734
      D8 = Z( KARC+2 )                                                  M38.2358
      D1 = SQRTZ( D7**2 + D8**2 )                                       M39.1735
      D2 = ATAN2Z( D8, D7 ) * RADEG                                     M39.1736
      D7 = Z( KARC+3 )                                                  M38.2361
      D8 = Z( KARC+4 )                                                  M38.2362
      D3 = SQRTZ( D7**2 + D8**2 )                                       M39.1737
      D4 = ATAN2Z( D8, D7 ) * RADEG                                     M39.1738
      D7 = Z( KARC+5 )                                                  M38.2365
      D8 = Z( KARC+6 )                                                  M38.2366
      D5 = SQRTZ( D7**2 + D8**2 )                                       M39.1739
      D6 = ATAN2Z( D8, D7 ) * RADEG                                     M39.1740
      WRITE( LUNIT6, 6290 )  D1, D2, D3, D4, D5, D6                     M39.1741
 6290 FORMAT(  18X,  22H PHASE  'A'  INJECTION,                         M39.1742
     1         18X,  22H PHASE  'B'  INJECTION,                         M39.1743
     2         18X,  22H PHASE  'C'  INJECTION,                         M39.1744
     3 /,  3(17X, 9HMAGNITUDE, 7X, 7HDEGREES)   ,/,                     M39.1745
     4     3(11X, E15.7, 2X, F12.7)     )                               M39.1746
      WRITE( LUNIT6, 6266 )                                             M39.1747
 6266 FORMAT ( 7H+ACTUAL  )                                             M39.1748
      D4 = CAMAG * ADK                                                  M39.1749
      D1 = ( CAANG - ADS ) * RADEG                                      M39.1750
      D2 = D1 + 240.                                                    M20.3033
      D5 = D2 - 360.                                                    M20.3034
      IF ( D5  .GT.  -180. )     D2 = D5                                M20.3035
      D3 = D1 + 120.                                                    M20.3036
      D5 = D3 - 360.                                                    M20.3037
      IF ( D5  .GT.  -180. )     D3 = D5                                M20.3038
      WRITE( LUNIT6, 6267 )  D4, D1, D4, D2, D4, D3                     M39.1751
 6267 FORMAT( 11H POS. SEQ.   ,    E15.7,  2X,  F12.7,                  M39.1752
     1 2(11X,  E15.7,  2X,  F12.7)   )                                  M20.3041
C     PRINT D,Q, AND 0 CURRENTS                                         M20.3042
      WRITE( LUNIT6, 6268 )                                             M39.1753
 6268 FORMAT ( /, 102H ARMATURE CURRENTS OF GENERATOR IN ROTATING REFEREM39.1754
     1NCE FRAME (D-Q-0 COORDINATES), IN UNITS OF  (AMPS) .         )    M20.3045
      WRITE( LUNIT6, 6269 ) CU(N33+1), CU(N33+2), CU(N33+3)             M39.1755
 6269 FORMAT( 20X, 2HID, 13X, 2HIQ, 13X, 2HI0, /, 7X, 3(E15.7) )        M39.1756
C     PRINT A,B,C PHASE CURRENTS (POS. SEQ. COMPONENT)                  M20.3079
      WRITE( LUNIT6, 6270 )                                             M39.1757
 6270 FORMAT( /, 101H POSITIVE-SEQUENCE COMPONENT OF GENERATOR ARMATURE M39.1758
     1CURRENT IN PHASE COORDINATES, IN UNITS OF (AMPS) . )              M33. 950
      D4 = CAMAG1                                                       M38.2371
      D1 = CAANG1 * RADEG                                               M38.2372
      D2 = D1 + 240.                                                    M20.3088
      D5 = D2 - 360.                                                    M20.3089
      IF ( D5  .GT.  -180. )     D2 = D5                                M20.3090
      D3 = D1 + 120.                                                    M20.3091
      D5 = D3 - 360.                                                    M20.3092
      IF ( D5  .GT.  -180. )     D3 = D5                                M20.3093
      WRITE( LUNIT6, 6271 )  D4, D1, D4, D2, D4, D3                     M39.1759
 6271 FORMAT(  18X,  22HARMATURE OF PHASE  'A',                         M39.1760
     1         18X,  22HARMATURE OF PHASE  'B',                         M39.1761
     2         18X,  22HARMATURE OF PHASE  'C',                         M39.1762
     3 /,  3(17X, 9HMAGNITUDE, 7X, 7HDEGREES)   ,/,                     M20.3098
     4     3(11X, E15.7, 2X, F12.7)     )                               M20.3099
C     PRINT FIELD CURRENT                                               M20.3101
      WRITE( LUNIT6, 6272 )                                             M39.1763
 6272 FORMAT ( /,  49H FIELD CURRENT OF GENERATOR IN UNITS OF  (AMPS) .)M39.1764
      WRITE( LUNIT6, 6273 )  CU( N33+4 ), CIF1                          M39.1765
 6273 FORMAT( 9X, 5HTOTAL, 15X, 12HDC-COMPONENT, /, 2(1X,E19.7) )       M39.1766
C     PRINT TORQUE ON GENERATOR ROTOR                                   M20.3108
      WRITE( LUNIT6, 6274 )                                             M39.1767
 6274 FORMAT ( /,                                                       M39.1768
     1 69H ELECTROMECHANICAL TORQUE OF GENERATOR, IN UNITS OF  MILLION (M20.3111
     2N - M). )                                                         M20.3112
      WRITE( LUNIT6, 6273 )  D22, D20                                   M39.1769
      IF ( NLOCE .EQ. 0 ) GO TO 1960                                    M37.4040
      WRITE( LUNIT6, 6275 )                                             M39.1770
 6275 FORMAT ( /,                                                       M39.1771
     1 67H ELECTROMECHANICAL TORQUE OF EXCITER, IN UNITS OF  MILLION (N M20.3123
     2- M).)                                                            M20.3124
      WRITE( LUNIT6, 6273 )  Q22, Q1                                    M39.1772
 1960 WRITE( LUNIT6, 6276 )  ELP( I26+21 ), ELP( I26+23 )               M39.1773
 6276 FORMAT(/,94H CRITICAL LEVEL OF TOTAL AIR GAP MMF AT WHICH SATURATIM39.1774
     1ON BEGINS, IN UNITS OF  (AMPERES) .         ,/,1X,2E25.9)         M23.2014
      WRITE( LUNIT6, 6277 )                                             M39.1775
 6277 FORMAT ( /,  62H MECHANICAL ANGLES OF ROTOR MASSES, IN UNITS OF  (M39.1776
     1 DEGREES)  .      )                                               M37.4044
      DO 2026 II=1,NUMASK                                               M20.3151
      N12 = ILSTOR + II                                                 M33. 952
      D4 = HISTQ( N12 ) * RADEG                                         M33. 953
      WRITE (LUNIT6, 6278 )  D4,  II                                    M39.1777
 6278 FORMAT (15X,F12.7,4X, 21H'THETA'  FOR MASS NO.,  I3  )            M39.1778
 2026 CONTINUE                                                          M20.3163
C     PRINT ANGULAR VELOCITIES                                          M20.3165
      WRITE( LUNIT6, 6279 )                                             M39.1779
 6279 FORMAT ( /,  61H ANGULAR VELOCITIES OF ROTOR MASSES, IN UNITS OF  M39.1780
     1(RAD/SEC) .       )                                               M20.3168
      N12 = ILSTOR + NUMASK                                             M33. 954
      D4 = HISTQ( N12+1 )                                               M38.2373
      DO 2255 I = 1, NUMASK                                             M38.2374
      WRITE( LUNIT6, 6280 )    D4, I                                    M39.1781
 6280 FORMAT (15X , F12.7 , 4X ,   21H'OMEGA'  FOR MASS NO.,  I3  )     M39.1782
 2255 CONTINUE                                                          M20.3183
      IF( NUM1 .EQ. 0 )  GO  TO  1218                                           
C     PRINT SHAFT TORQUES                                               M20.3188
      WRITE( LUNIT6, 6281 )                                             M39.1783
 6281 FORMAT ( /,                                                       M39.1784
     1 60H SHAFT TORQUES BETWEEN MASSES, IN UNITS OF  MILLION (N - M).) M20.3191
C     NOTE- TRANSIENT SHAFT TORQUE INCLUDES A DAMPING TORQUE TERM ALSO  M20.3193
C           BUT ABOVE IS CORRECT FOR STEADY STATE                       M20.3194
      N6 = ILSTOR + NUM4                                                M38.2376
      DO 2460 I = 1, NUM1                                               M33. 958
      N6 = N6 + 1                                                       M37.4047
      AJJ = HISTQ( N6 )                                                         
      N1 = I + 1                                                        M20.3201
      WRITE( LUNIT6, 6282 )  AJJ, I, I, N1                              M39.1785
 6282 FORMAT( 16X, E15.7, 4X, 2H'T, I2,                                 M39.1786
     1  34H' --- TORQUE ON SHAFT BETWEEN MASS, I3, 10H  AND MASS,  I3  )M20.3204
 2460 CONTINUE                                                          M20.3205
 1218 ILMASS = ILMASS + NUMASK                                          M38.2378
      ILSTOR = ILSTOR + NUM6                                                    
      N33 = N33 + NWD                                                           
      IF ( IPRSUP  .GE.  2 )                                            M20.3216
     1 WRITE( LUNIT6, 6283 )  K, N10, NUMSM, ISMDAT( J30 )              M39.1787
 6283 FORMAT ( /,  40H AFTER SHAFT-TORQUE CALC. IN  'SMINT ' .,         M39.1788
     1  32H       K     N10   NUMSM  IMDUAL  ,/,  40X,  5I8  )          M37.4068
      IF( IN .LT. ILK )   GO  TO  11                                    M39.1789
      J30 = J30 + 30                                                            
  900 CONTINUE                                                          M20.3221
C     ERASE SYNCHRONOUS MACHINES FROM SOURCE TABLES  * * * * * * * * * *M20.3222
      DO 911 I = 1, KCONST                                              M39.1790
      J = I                                                             M38.2382
      IF( TSTART(I) .EQ. -9988. )   GO TO  915                          M39.1791
 911  CONTINUE                                                          M39.1792
      GO TO 918                                                         M39.1793
 915  KCONST = J - 1                                                    M39.1794
 918  IF ( IPRSUP .GE. 1 )                                              M39.1795
     1 WRITE( LUNIT6, 6284 )   KCONST                                   M39.1796
 6284 FORMAT ( /,  32H UPON EXIT  'SMINIT' ,  KCONST =,  I3  )          M39.1797
      RETURN                                                            M20.3232
      END                                                               M20.3233
      SUBROUTINE UNCOR( SCI, I )                                        M32.3738
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED ONLY BY BRANDWAJN (TYPE-59) S.M. MODEL        M32.3739
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      DIMENSION SCI( 1 )                                                M22.3481
      DIMENSION VNR(3), VNI(3), AX(9), AY(9)                            M32.3740
      CNI = SCI(1)                                                      M32.3741
      CNR = SCI(2)                                                      M32.3742
      ACDE = SCI(3)                                                     M32.3743
      ACDF = SCI(4)                                                     M32.3744
      A19 = SCI( 5 )                                                    M32.3745
      TRA2 = SCI( 6 )                                                   M32.3746
C     PROCESS FIRST THE D - AXIS       *   *   *   *   *   *   *   *   *M32.3747
      I26 = 101 * I - 100                                                       
      N = 0                                                             M32.3748
      IWD1 = 2                                                          M32.3749
      IWD = IWD1                                                        M32.3750
      IF( ELP( I26+7 ) .EQ. 0. )  IWD = IWD - 1                                 
      IW = IWD * IWD                                                    M32.3752
      DO 2 J = 1, IW                                                    M32.3753
      AX(J) = 0.                                                        M32.3754
 2    AY(J) = 0.                                                        M32.3755
      A18 = ( 1. - ACDE ) * ELP( I26+20 )                                       
      AX(1) = -ELP( I26+6 )                                                     
      AY(1) = ( ELP( I26+2 ) - ELP( I26+1 ) * A18 ) * TRA2              M37.4075
      A1 = ELP( I26+1 ) * ACDE * 2.0                                            
      A2 = A19 * A19                                                    M37.4077
      A5 = 1. / ( A2 + 1. )                                             M32.3761
      A2 = A2 * A5                                                      M32.3762
      A19 = A19 * A5                                                    M32.3763
      A3 = A1 * A19                                                     M32.3764
      A4 = A1 * A2                                                      M32.3765
      VNR(1) = -(A3 * CNR - A4 * CNI )                                  M32.3766
      VNI(1) = -( A3 * CNI + A4 * CNR )                                 M32.3767
      IF( IWD .EQ. 1 )  GO  TO  3                                       M32.3768
      AY(2) = ELP( I26+4 ) * ACDE * TRA2                                        
      AY(3) = AY(2)                                                     M32.3770
      AX(4) = -ELP( I26+7 )                                                     
      AY(4) = ( ELP( I26+5 ) - ELP( I26+3 ) * A18 ) * TRA2              M37.4080
      A1 = ELP( I26+3 ) * ACDE * 2.0                                            
      A3 = A1 * A19                                                     M32.3774
      A4 = A1 * A2                                                      M32.3775
      VNR(2) = -( A3 * CNR - A4 * CNI )                                 M37.4082
      VNI(2) = -( A3 * CNI + A4 * CNR )                                 M32.3777
 3    CALL REDUCN( AX,AY,IWD,N )                                        M37.4083
      NK = 2                                                            M32.3779
      DO 4 J = 1, IWD1                                                  M32.3780
      A3 = 0.                                                           M32.3781
      A4 = 0.                                                           M32.3782
      NB = J - IWD                                                      M32.3783
      NK = NK + 1                                                       M32.3784
      IF( J .GT. IWD )  GO  TO 14                                       M37.4084
      DO 5 K = 1, IWD                                                   M32.3786
      NB = NB + IWD                                                     M32.3787
      A3 = A3 + AX(NB) * VNR(K) - AY(NB) * VNI(K)                       M32.3788
   5  A4 = A4 + AX(NB) * VNI(K) + AY(NB) * VNR(K)                       M32.3789
 14   IF( IPRSUP .GT. 1  )                                              M32.3790
     1  WRITE ( LUNIT6, 25 ) J, A3, A4                                  M32.3791
   25 FORMAT (  34H  NEGATIVE SEQUENCE CORRECTION FOR,                  M32.3792
     1          19H WINDING NUMBER J =,  I5,                            M32.3793
     2          33H   OF THE D-AXIS.  A3, A4 FOLLOW:                    M32.3794
     3    ,/,  2X,  2E25.15  )                                          M32.3795
 4    SCI( NK ) = A4                                                    M37.4085
C     PROCESS THE Q - AXIS     *   *   *   *   *   *   *   *   *   *   *M32.3797
      IWD2 = 2                                                          M32.3798
      IWD = IWD2                                                        M32.3799
      IF( ELP( I26+15 ) .EQ. 0. )   IWD = IWD - 1                               
      IF( ELP( I26+14 ) .EQ. 0. )   IWD = IWD - 1                               
      IF( IWD .EQ. 0 )   GO   TO     13                                 M32.3802
      IW = IWD * IWD                                                    M32.3803
      DO 6 J = 1, IW                                                    M32.3804
      AX(J) = 0.                                                        M32.3805
 6    AY(J) = 0.                                                        M32.3806
      A18 = ( 1. - ACDF ) * ELP( I26+20 )                                       
      AX(1) = -ELP( I26+14 )                                                    
      AY(1) = ( ELP( I26+10 ) - ELP( I26+9 ) * A18 ) * TRA2             M37.4090
      A1 = ELP( I26+9 ) * ACDF * 2.0                                            
      A3 = A1 * A19                                                     M32.3811
      A4 = A1 * A2                                                      M32.3812
      VNR(1) = -( A3 * CNR - A4 * CNI )                                 M32.3813
      VNI(1) = -( A3 * CNI + A4 * CNR )                                 M32.3814
      IF( IWD .EQ. 1 )  GO   TO  8                                      M32.3815
      AY(2) = ELP( I26+12 ) * ACDF * TRA2                                       
      AY(3) = AY(2)                                                     M32.3817
      AX(4) = -ELP( I26+15 )                                                    
      AY(4) = ( ELP( I26+13 ) - ELP( I26+11 ) * A18 ) * TRA2            M37.4094
      A1 = ELP( I26+11 ) * ACDF * 2.0                                           
      A3 = A1 * A19                                                     M32.3821
      A4 = A1 * A2                                                      M32.3822
      VNR(2) = -( A3 * CNR - A4 * CNI )                                 M32.3823
      VNI( 2 ) = -( A3 * CNI + A4 * CNR )                               M32.3824
 8    CALL REDUCN( AX, AY, IWD, N )                                     M32.3825
 13   DO 9  J = 1,  IWD2                                                M32.3826
      A3 = 0.                                                           M32.3827
      A4 = 0.                                                           M32.3828
      NB = J - IWD                                                      M32.3829
      NK = NK + 1                                                       M32.3830
      IF( J .GT. IWD )   GO  TO  15                                     M32.3831
      DO 10 K = 1, IWD                                                  M32.3832
      NB = NB + IWD                                                     M32.3833
      A3 = A3 + AX(NB) * VNR(K) - AY(NB) * VNI(K)                       M32.3834
  10  A4 = A4 + AX(NB) * VNI(K) + AY(NB) * VNR(K)                       M32.3835
 15   IF( IPRSUP .GT. 1 )                                               M32.3836
     1  WRITE ( LUNIT6, 26 ) J, A3, A4                                  M32.3837
   26 FORMAT (  34H  NEGATIVE SEQUENCE CORRECTION FOR,                  M32.3838
     1          19H WINDING NUMBER J =,  I5,                            M32.3839
     2          33H   OF THE Q-AXIS.  A3, A4 FOLLOW:                    M32.3840
     3    ,/,  2X,  2E25.15  )                                          M32.3841
 9    SCI( NK ) =  A3                                                   M32.3842
      IF( IPRSUP .GT. 1 )                                               M32.3843
     1 WRITE (LUNIT6, 30) ( SCI(K), K = 1, NK )                         M32.3844
   30 FORMAT (  36H  NEGATIVE SEQUENCE CORRECTIONS UPON,                M32.3845
     1          31H EXIT FROM "UNCOR".   SCI(1:NK),                     M32.3846
     2          13H  FOLLOW ....   ,/,  ( 2X,  6E21.12 ) )              M32.3847
      RETURN                                                            M22.3545
      END                                                               M22.3546
      SUBROUTINE SLOPE( SFT,SSLD,SSAD,ACEE)                             M32.3848
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      ADELD = SFT / SSLD - .9                                           M36.1407
      N5 = 10. * ADELD                                                  M34. 130
      N5 = ( N5 + 1 ) / 2                                               M37.4096
      SB = N5                                                           M34. 131
      SB =  SB  * .1                                                    M34. 132
      SF4 = SSLD * ( 0.9 + SB )                                         M36.1408
      SF5 = SSLD * ( 1.1 + SB )                                         M37.4097
      SF6 = SF4 / ( 1.0 + SSAD * ( SF4 - SSLD ) )                       M32.3854
      SF7 = SF5 / ( 1.0 + SSAD * ( SF5 - SSLD ) )                       M32.3855
      ACEE = ( SF7 - SF6 ) / ( SF5 - SF4 )                              M32.3856
      RETURN                                                            M32.3857
      END                                                               M32.3858
      SUBROUTINE REDUCN(X,Y,M,N)                                        M32.3859
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION X(1),Y(1),A1(18),B1(18)                                 M32.3860
      J = M                                                             M32.3861
      IK = M * M                                                        M32.3862
      NK = IK - M                                                       M32.3863
      M1 = M + 1                                                        M32.3864
 1    C = 1.D0 / ( X(IK)**2 + Y(IK)**2 )                                M32.3865
      D = -Y(IK) * C                                                    M32.3866
      C = X( IK ) * C                                                   M32.3867
      DO 2 K = 1, M                                                     M32.3868
      NKK = NK + K                                                      M32.3869
      B1( K ) = Y( NKK)                                                 M32.3870
 2    A1( K ) = X( NKK)                                                 M32.3871
      K = 1                                                             M32.3872
 4    MK = ( K - 1 ) * M                                                M32.3873
      MKJ = MK + J                                                      M32.3874
      A = -( X(MKJ) * C - Y(MKJ) * D )                                  M32.3875
      B = -( X(MKJ) * D + Y(MKJ) * C )                                  M32.3876
      I = 1                                                             M32.3877
 3    MK = MK + 1                                                       M32.3878
      X(MK) =  X(MK) + ( A * A1(I) - B * B1(I) )                        M32.3879
      Y(MK) = Y(MK) + (A * B1(I) + B * A1(I) )                          M32.3880
      I = I + 1                                                         M32.3881
      IF( I .LT. M1 )   GO   TO   3                                     M32.3882
      MKMJ = MK - M + J                                                 M32.3883
      X( MKMJ ) = A                                                     M32.3884
      Y( MKMJ ) = B                                                     M32.3885
      K = K + 1                                                         M32.3886
      IF( K .EQ. J )  K = K + 1                                         M32.3887
      IF( K .LT. M1 )   GO   TO   4                                     M32.3888
      DO 5 K = 1, M                                                     M32.3889
      NKK = NK + K                                                      M32.3890
      X(NKK) = A1(K) * C - B1(K) * D                                    M32.3891
 5    Y(NKK) = A1(K) * D + B1(K) * C                                    M32.3892
      X(IK) = C                                                         M32.3893
      Y(IK) = D                                                         M32.3894
      J = J - 1                                                         M32.3895
      IK = IK - M1                                                      M32.3896
      NK = NK - M                                                       M32.3897
      IF( J .GT. N )  GO   TO   1                                       M32.3898
      RETURN                                                            M32.3899
      END                                                               M32.3900
      SUBROUTINE SSOUT( L, ERK, EIK, ERM, EIM,                             10571
     1 CURRK, CURIK, CURRM, CURIM )                                        10572
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      IF ( IPRSUP  .GE.  1 )                                            M32.3904
     1 WRITE (LUNIT6, 1006)  L, ERK, EIK, ERM, EIM                      M32.3905
 1006 FORMAT ( 41H TOP OF "SSOUT".  L, ERK, EIK, ERM, EIM =,            M32.3906
     1    I6,  4E16.5  )                                                M32.3907
      IF ( M4PLOT .EQ. 1 )  CALL EMTSPY                                         
      PICON = 360. / TWOPI                                              M36.1410
      IF( L .GT. 0 )  GO TO 2020                                           10577
      BUS1 = BLANK                                                         10578
      GO TO 2030                                                           10579
 2020 K = IABS( KBUS(L) )                                               M28.4289
      M = IABS( MBUS(L) )                                                  10582
      IF ( MBUS(L)  .GT.  0   .AND.                                     M33.1023
     1     KSSOUT  .EQ.  3 )   GO TO 4500                               M33.1024
      BUS1 = BUS(K)                                                        10583
      IF( K .EQ. 1 )  BUS1 = TERRA                                         10584
      BUS2 = BUS(M)                                                        10585
      IF( M .EQ. 1 )  BUS2 = TERRA                                         10586
      IF ( BUS1  .EQ.  TRASH )   GO TO 4500                             M20.3234
      IF ( BUS2  .EQ.  TRASH )   GO TO 4500                             M20.3235
 2030 VK = SQRTZ( ERK**2  +  EIK**2 )                                   M18. 681
      VM = SQRTZ( ERM**2  +  EIM**2 )                                   M18. 682
      THK = 0.0                                                            10589
      THM = 0.0                                                            10590
      IF( VK .GT. 0.0 )  THK = PICON * ATAN2Z( EIK, ERK )               M18. 683
      IF( VM .GT. 0.0 )  THM = PICON * ATAN2Z( EIM, ERM )               M18. 684
      CMK = SQRTZ( CURRK**2  +  CURIK**2 )                              M18. 685
      CMM = SQRTZ( CURRM**2  +  CURIM**2 )                              M18. 686
      CAK = 0.0                                                            10595
      CAM = 0.0                                                            10596
      IF( CMK .GT. 0.0 )   CAK = PICON * ATAN2Z( CURIK, CURRK )         M18. 687
      IF( CMM .GT. 0.0 )   CAM = PICON * ATAN2Z( CURIM, CURRM )         M18. 688
      PK = ( ERK*CURRK + EIK*CURIK ) / 2.0                              M05. 478
      PM = ( ERM*CURRM + EIM*CURIM ) / 2.0                              M05. 479
      QK = ( -ERK*CURIK + EIK*CURRK ) / 2.0                             M05. 480
      QM = ( -ERM*CURIM + EIM*CURRM ) / 2.0                             M05. 481
      PLOSS = PK + PM                                                      10603
      QLOSS = QK + QM                                                      10604
      IF ( KSSOUT  .EQ.  1 )   GO TO 2032                               M31.2689
      IF ( VK  .GT.  0.0 )     GO TO 2032                               M31.2690
      IF ( VM  .GT.  0.0 )     GO TO 2032                               M31.2691
      IF ( CMK  .GT.  EPSILN )   GO TO 2032                             M31.2692
      IF ( CMM  .GT.  EPSILN )   GO TO 2032                             M31.2693
      GO TO 4500                                                        M31.2694
 2032 IF( L .GT. 0 )  GO TO 2040                                        M31.2695
C     BEGIN CODE FOR GENERATOR INJECTION (80 OR 132 COLUMNS):           M32.3911
      PLOSS = SQRTZ( PK**2 + QK**2 )                                    M18. 689
      QLOSS = 0.0                                                          10607
      IF( PLOSS .GT. 0.0 )  QLOSS = PK / PLOSS                             10608
      IF ( KOL132  .EQ.  132 )   GO TO 3214                             M32.3912
C     BEGIN 80-COLUMN CODE FOR GENERATOR INJECTION PRINTOUT:            M32.3913
      WRITE (LUNIT6, 3207)  BUS5, VK, CMK, PK, PLOSS                    M32.3914
 3207 FORMAT ( 1X,  A6,  13X,  4E15.7  )                                M32.3915
      WRITE (LUNIT6, 3208)  THK, CAK, QK, QLOSS                         M32.3916
 3208 FORMAT ( 20X,  2F15.4,  E15.7,  F15.10 )                          M32.3917
      GO TO 4500                                                        M32.3918
C     BEGIN 132-COLUMN CODE FOR GENERATOR INJECTION PRINTOUT:           M32.3919
 3214 WRITE (LUNIT6, 2034)  BUS5, ERK, VK, CURRK, CMK, PK, PLOSS        M32.3920
 2034 FORMAT ( 15X, A6,  3(5X, 2E15.7)  )                               M28.4291
      GO TO 2051                                                        M28.4292
C     BEGIN CODE FOR BRANCH FLOWS (80 OR 132 COLUMNS):                  M32.3921
 2040 IF ( KOL132  .EQ.  132 )   GO TO 2045                             M36.1411
C     BEGIN SPECIAL CODE DEDICATED TO 80-COL. BRANCH FLOW:              M32.3924
 3224 WRITE (LUNIT6, 3225)   BUS1,  VK,  VM, CMK, PK                    M32.3925
 3225 FORMAT ( /,  1X, A6,  13X,  4E15.7 )                              M32.3926
      WRITE (LUNIT6, 3229)  BUS2, THK, THM, CAK, QK                     M32.3927
 3229 FORMAT ( 14X, A6,  3F15.4,  E15.7 )                               M32.3928
      GO TO 4500                                                        M32.3929
C     BEGIN 132-COL. CODE FOR FORWARD BRANCH FLOW PRINTOUT:             M32.3930
 2045 WRITE(LUNIT6,2050)  BUS1, ERK, VK, CURRK, CMK, PK, PLOSS          M17. 593
 2050 FORMAT ( /, 1X, A6, 14X,  3(5X, 2E15.7)  )                        M32.3931
 2051 WRITE(LUNIT6, 2052)  EIK, THK, CURIK, CAK, QK, QLOSS              M28.4293
 2052 FORMAT ( 21X,   2(5X, E15.7, F15.4),  5X, 2E15.7 )                   10615
      IF ( L  .LT.  0 )   GO TO 4500                                    M32.3932
C     BEGIN CODE FOR REVERSE BRANCH FLOW, 132-COLUMN DISPLAY:           M32.3933
      WRITE(LUNIT6, 2054)  BUS2, ERM, VM, CURRM, CMM, PM                M32.3934
 2054 FORMAT ( /, 15X, A6,   3(5X, 2E15.7)  )                              10618
      WRITE(LUNIT6, 2052)  EIM, THM, CURIM, CAM, QM                     M17. 596
 3278 WRITE(LUNIT6, 2052)                                               M32.3935
 4500 RETURN                                                            M36.1412
      END                                                                  10622
