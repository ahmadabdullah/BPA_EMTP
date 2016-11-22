      SUBROUTINE OVER10
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      DIMENSION ITEMP(1)                                                M31.2445
      EQUIVALENCE  ( ITEMP(1), VOLTK(1) )                               M31.2446
      LOCATN(I,J)=(J*J-J)/2+I                                                   
      IF ( IPRSUP  .GE.  1 )                                            M37.3696
     1 WRITE (LUNIT6, 2941)  NTOT, IOFFD, LOOPSS(2)                     M37.3697
 2941 FORMAT ( 44H TOP OF "OVER10".   NTOT, IOFFD, LOOPSS(2) =, 2I8 )   M37.3698
      N14 = NTOT + LOOPSS(2)                                                    
      CALL MOVER0 ( SOLR(1), N14 )                                              
c 3456789012345678901234567890123456789012345678901234567890123456789012M44. 732
      CALL MOVER0 ( SOLI(1), N14 )                                      M35.1206
      DO 3009 I = 1, NTOT                                                       
      IA = INDEX(I)                                                             
      GO TO 3003                                                                
 3002 IA = IA +1                                                                
 3003 IF (IA .EQ. INDEX(I+1))                                           M42.6111
     1                   GO TO 3009                                             
      IB = ILOC(IA)                                                             
      IC = IA                                                                   
 3005 IC = IC +1                                                                
      IF (IC .EQ. INDEX(I+1)) GO TO 3002                                        
      IF (IB .LE. ILOC(IC)) GO TO 3005                                          
      ILOC(IA) = ILOC(IC)                                                       
      ILOC(IC) = IB                                                             
      IB = ILOC(IA)                                                             
      GO TO 3005                                                                
 3009 CONTINUE                                                                  
      N1 = 1 + IOFGND                                                           
      N2 = 1 + IOFBND                                                           
      CALL MOVER0 ( GND(N1), IOFFD)                                             
      CALL MOVER0 ( BND(N2),IOFFD)                                      M19. 885
      CALL MOVER0 ( DIAG(1), NTOT )                                             
      CALL MOVER0 ( DIAB(1), NTOT )                                     M42.6123
      IA = NTOT                                                         M42.6124
      IF ( IA  .LT.  IBR )   IA = IBR                                   M42.6125
      IF ( IA  .LT.  IT )   IA = IT                                     M42.6126
      IF( IPRSUP .GE. 3 )                                               M42.6127
     1 WRITE(LUNIT6,1139) ( I,NORDER(I), INDEX(I), ILOC(I),             M22.3250
     2  KBUS(I), KODE(I),  MBUS(I), NR(I), KODEBR(I), LENGTH(I),        M22.3251
     3  TR(I), TX(I), C(I), I=1, IA )                                   M22.3252
 1139 FORMAT ( //,  33H ARRAYS AT BEGINNING OF  OVER10 .   ,/,          M42.6128
     1 5X, 3HROW, 2X, 6HNORDER, 3X, 5HINDEX, 4X, 4HILOC,                M42.6129
     2 4X, 4HKBUS, 4X, 4HKODE, 4X, 4HMBUS, 6X, 2HNR,                    M42.6130
     3 2X, 6HKODEBR, 2X, 6HLENGTH, 13X, 2HTR, 13X, 2HTX, 14X, 1HC,      M42.6131
     4 /, ( 10I8, 3E15.4  ) )                                           M42.6132
C     INITIALIZE COUNTERS FOR THE -666 BRANCHES  * * * * * * * * * * * *M32.3320
      IKF = 0                                                           M32.3321
      ISFD = 0                                                          M32.3322
      I = 1                                                                     
C                                                                       M44. 734
C                        Beginning of bulding [y] from very first branchM44. 735
C                                                                       M44. 736
 3080 IC = IABS ( KBUS(I) )                                                     
      XOPT = XOPTBR(I)                                                  VAX.  75
      COPT = COPTBR(I)                                                  VAX.  76
      D3 = TENM3                                                        VAX.  77
      D4 = UNITY / 1 000 000.                                           VAX.  78
      IF( XOPT .GT. 0.0 )  D3 = 1.0 / ( TWOPI * XOPT )                  VAX.  79
      IF( COPT .GT. 0.0 )  D4 = D4 / ( TWOPI * COPT )                   VAX.  80
      OMLTEM =  D3 * TWOPI                                              VAX.  81
      OMCTEM =  D4 * TWOPI                                              VAX.  82
      IF ( IC  .EQ.  1 )  IC = IABS( MBUS(I) )                                  
      N16 = IABS ( KSSFRQ(IC) )                                                 
      OMEGAL = OMLTEM * SFREQ(N16)                                              
      OMEGAC = OMCTEM * SFREQ(N16)                                      M31.2460
      OMEGA = TWOPI * SFREQ(N16)                                        M31.2461
      IF ( IPRSUP  .GE.  3 )                                            M31.2462
     1 WRITE (LUNIT6, 3087)  I, IC, N16, OMEGA, OMEGAL, OMEGAC          M31.2463
 3087 FORMAT ( 42H NEXT BRANCH.   I, IC, N16, OMEGA, OMEGAL,,           M31.2464
     1          9H OMEGAC =,  3I6,  3E15.6  )                           M31.2465
      if ( iprsup .ge. 1 )                                              VAX.  83
     1 write (LUNIT6,*) ' BND(1) =', BND(1)                             VAX.  84
      IF( KODSEM(I) .EQ. 0 .AND.                                        M44. 737
     1      IMODEL(I) .EQ. 0 ) GO TO 5698                                       
      IF ( IMODEL(I) .EQ. -4 )  OMEGAC =1.0                             M44. 739
      IF (KODEBR(I) .EQ. -2                                             M31.2466
     1     .AND. KODSEM(I) .EQ. 0) GO TO 43400                                  
      IF (KODEBR(I) .EQ. -2 .AND. IMODEL(I) .EQ. -2) GO TO 43400        M31.2467
 5698 IF(IABS(LENGTH(I)).GT.1) GO TO 3300                                       
      IC = IABS(KBUS(I))                                                        
      IC = NORDER(IC)                                                           
      IG = IABS(MBUS(I))                                                        
      IG = NORDER(IG)                                                           
      J = IABS(NR(I))                                                           
      RR = TR(J)                                                                
      IF( IC .GT. IG )  GO TO 3130                                              
      IF( IC .LT. IG )  GO TO 3120                                              
      IF ( KBUS(I)  .EQ.  1 ) GO TO 3283                                        
      LSTAT(19) = 3112                                                  M15. 760
      LSTAT(15) = I                                                     M25. 266
      N1 = IABS( KBUS(I) )                                              M25. 267
      BUS1 = BUS(N1)                                                            
 3112 KILL = 17                                                         M42.6141
      GO TO 9999                                                        M42.6142
 3120 IN = IG                                                                   
      IM = IC                                                                   
      GO TO 3140                                                        M42.6145
 3130 IN = IC                                                                   
      IM = IG                                                                   
 3140 IA = INDEX(IM)                                                            
      IF (NR(I) .GE. 0) GO TO 3220                                              
      IF (C(J) .EQ. 0.0)   GO TO 3164                                           
      XX = TX(J)*OMEGAL -1.0/(C(J)*OMEGAC)                              M42.6151
      GO TO 3168                                                        M42.6152
 3164 XX = TX(J)*OMEGAL                                                         
 3168 DD = RR*RR +XX*XX                                                 M42.6154
      GG = RR/DD                                                                
      BB = -XX/DD                                                               
      GO TO 3180                                                        M42.6157
 3170 IA = IA + 1                                                       M42.6158
 3180 IF (IA .LT. INDEX(IM + 1)) GO TO 3200                                     
      LSTAT(19) = 3180                                                  M15. 761
      GO TO 3112                                                        M42.6160
 3200 IF (ILOC(IA) .NE. IN)                                             M42.6161
     1        GO TO 3170                                                        
      ISUBS1 =  IOFGND+IA                                               M16.1472
      GND(ISUBS1)    = GND(ISUBS1)    - GG                              M16.1473
      ISUBS1 =  IOFBND+IA                                               M16.1474
      BND(ISUBS1)    = BND(ISUBS1)    - BB                              M16.1475
      GO TO 3280                                                        M42.6162
 3220 XX = TX(J)*OMEGAL                                                         
      GG = RR                                                           M44. 742
      BB = XX                                                           M44. 743
      IF ( IMODEL(I) .EQ. -4 )                                          M44. 744
     1                   GO TO 3240                                             
      DD = RR*RR + XX*XX                                                M42.6164
      GG = RR/DD                                                        M42.6165
      BB = -XX/DD                                                       M42.6166
      GO TO 3240                                                        M42.6167
 3230 IA = IA + 1                                                       M42.6168
 3240 IF (IA .LT. INDEX(IM + 1)) GO TO 3260                             M42.6169
      LSTAT(19) = 3240                                                  M15. 762
      GO TO 3112                                                        M42.6170
 3260 IF (ILOC(IA).NE.IN)                                               M42.6171
     1     GO TO 3230                                                           
      ISUBS1 =  IOFGND+IA                                               M16.1476
      GND(ISUBS1)    = GND(ISUBS1)    - GG                              M16.1477
      ISUBS1 =  IOFBND+IA                                               M16.1478
      BND(ISUBS1)    = BND(ISUBS1)    - BB                              M16.1479
      BB = BB + C(J)*OMEGAC                                             M42.6172
      IF (KBUS(I) .LT. 0)                                               M42.6173
     1         GG = GG +R(J)                                                    
 3280 DIAG(IM) = DIAG(IM) + GG                                                  
      DIAB(IM) = DIAB(IM) + BB                                                  
      DIAG(IN) = DIAG(IN) + GG                                          M42.6176
      DIAB(IN) = DIAB(IN) + BB                                          M42.6177
 3283 I = I + 1                                                                 
      GO TO 3490                                                                
 3300 IS = IABS(NR(I))                                                          
      NT = IABS(LENGTH(I))                                                      
      IF (LENGTH(I) .LT. 0)                                             M42.6181
     1                 GO TO 3320                                               
      JT = NT*(NT + 1)/2                                                        
      IF (KODEBR(I) .LE. 0 ) GO TO 3515                                         
      IF ( KODSEM(I) .NE. 0  .AND. IMODEL(I) .NE. -2) go to 3515        M31.2470
      IF( JT .LE. LBUS )  GO TO 3504                                    M42.6183
      KILL = 51                                                         M42.6184
      K = IABS( KBUS(I) )                                               M42.6185
      M = IABS( MBUS(I) )                                               M42.6186
      LSTAT(19) = 3504                                                  M42.6187
      BUS1 = BUS(K)                                                             
      BUS2 = BUS(M)                                                             
      LSTAT(14) = NT                                                    M42.6188
      GO TO 9999                                                        M42.6189
C        For case of [A], [B], 1st form unsymmetric  [Q] = -[B] + jw[U]:M44. 749
 3504 CALL MOVER( TR(IS), F(1), JT )                                            
      N = IS + JT - 1                                                   M42.6191
      DO 3508  J=IS, N                                                  M42.6192
 3508 TR(J) = -TX(J)                                                    M42.6193
      CALL MOVER0 ( TX(IS), JT )                                        M42.6194
      CALL ADDMXD( TX(IS), OMEGA, TX(IS), NT )                                  
      IF( IPRSUP .GE. 2 )                                               M42.6196
     1WRITE(LUNIT6, 3505)  (  TR(J),  TX(J), J=IS, N )                  M17. 499
 3505 FORMAT( /,  39H AT 3505.   CMR +J CMI BEFORE INVERSION  ,/,       M42.6197
     1 ( 1X, 8E16.6 ) )                                                 M42.6198
 3515 L = IS                                                                    
      M = 1                                                             M42.6200
      N = 1                                                             M42.6201
      IF (IMODEL(I) .EQ. -3  .OR.                                       M44. 750
     1    IMODEL(I) .EQ. -4       ) GO TO 3320                                  
C     CHECK FOR THE PRESENCE OF THE  -666 BRANCHES *   *   *   *   *   *M32.3324
      IF( LENGTH(I+1) .NE. -666 )                                       M32.3325
     1                     GO TO 10                                             
      CALL SSEQIV( IKF, ISFD, OMEGAL, OMEGAC )                          M32.3326
C     CONVERT MODAL DATA TO PHASE COORDINATES  *   *   *   *   *   *   *M32.3327
      CZ = NT                                                           M34. 127
      AZR = ( VOLTI(1) - VOLTI(2) ) / CZ                                M37.3699
      AZI = ( VOLTK(1) - VOLTK(2) ) / CZ                                M37.3700
      LK = L                                                            M32.3331
      LB = L                                                            M32.3332
      KB = 1                                                            M32.3333
C     LOAD EQIVALENT IMPEDANCE INTO THE TR AND TX TABLES   *   *   *   *M32.3334
      DO  2  KA = 1, JT                                                 M32.3335
         TR(LK) = AZR                                                   M32.3336
         TX(LK) = AZI                                                   M32.3337
         IF( LB .NE. LK ) GO TO 2                                       M32.3338
         TR(LB) = TR(LB) + VOLTI(2)                                     M37.3701
         TX(LB) = TX(LB) + VOLTK(2)                                     M37.3702
         KB = KB + 1                                                    M32.3341
         LB = LB + KB                                                   M32.3342
    2    LK = LK + 1                                                    M32.3343
   10 DO  3530  J = 1, JT                                               M32.3344
      IF (KODEBR(I) .LE. 0 ) TX(L) = TX(L) *OMEGAL                      M31.2471
      IF (KODSEM(I) .NE. 0  .AND. IMODEL(I) .NE. -2)                    M31.2472
     1 TX(L) = TX(L) * OMEGAL                                           M31.2473
      IF( IPRSUP .GE. 3 )                                               M42.6202
     1WRITE(LUNIT6, 3524)  I, KBUS(I), MBUS(I), J, L, TR(L), TX(L)      M17. 500
 3524 FORMAT( 9H AT 3524    , 5I10, 2E20.8 )                            M42.6203
      IF( J .NE. N )  GO TO 3530                                        M42.6204
      E(M) = TR(L)**2  +  TX(L)**2                                      M42.6205
      M = M + 1                                                         M42.6206
      N = N + M                                                         M42.6207
 3530 L = L + 1                                                         M42.6208
      XA = TR(IS)*TR(IS) + TX(IS)*TX(IS)                                M42.6209
      IF( XA .GT. TOLMAT*E(1) )  GO TO 3570                             M42.6210
      N = 1                                                             M42.6211
 3560 KILL = 18                                                         M42.6212
      LSTAT(19) = 3560                                                  M42.6213
      N1 = IABS( KBUS(I) )                                              M42.6214
      N2 = IABS( MBUS(I) )                                              M42.6215
      LSTAT(14) = NT                                                    M42.6216
      BUS1 = BUS(N1)                                                    M20.2248
      BUS2 = BUS(N2)                                                    M20.2249
      FLSTAT(13) = TOLMAT                                               M42.6217
      FLSTAT(12) = XA                                                   M42.6218
      FLSTAT(11) = E(N)                                                 M42.6219
      GO TO 9999                                                        M42.6220
 3570 TR(IS) = TR(IS)/XA                                                        
      TX(IS) = -TX(IS)/XA                                               M42.6222
      IF (NT .LE. 1) GO TO 3320                                                 
      JS = IS                                                           M42.6224
      NE = IS                                                                   
      N1 = IS + JT - 1                                                  M42.6226
      DO 3900 N = 2, NT                                                 M42.6227
      DO 3630 M = 1, N                                                  M42.6228
      JS = JS +1                                                        M42.6229
      SOLR(M) = TR(JS)                                                  M35.1211
 3630 SOLI(M) = TX(JS)                                                  M35.1212
      M = N - 1                                                         M42.6230
      J = 1                                                             M42.6231
      JA = IS -1                                                        M42.6232
 3650 K = JA                                                            M42.6233
      XR = 0.0                                                          M42.6234
      XI = 0.0                                                          M42.6235
      LA = 0                                                            M42.6236
      DO 3680 L = 1, J                                                  M42.6237
      K = K + 1                                                         M42.6238
      LA = LA + 1                                                       M42.6239
      XR = XR + SOLR(LA)*TR(K) - SOLI(LA)*TX(K)                         M35.1213
 3680 XI = XI + SOLR(LA)*TX(K) + SOLI(LA)*TR(K)                         M35.1214
      K = K + J                                                         M42.6240
      IF( M .EQ. J )  GO TO 3760                                        M42.6241
      JA = JA + J                                                       M42.6242
      J = J + 1                                                         M42.6243
      DO 3740 LL = J, M                                                 M42.6244
      LA = LA + 1                                                       M42.6245
      XR = XR + SOLR(LA)*TR(K) - SOLI(LA)*TX(K)                         M35.1215
      XI = XI + SOLI(LA)*TR(K) + SOLR(LA)*TX(K)                         M35.1216
 3740 K = K + LL                                                        M42.6246
      TR(K) = XR                                                        M42.6247
      TX(K) = XI                                                        M42.6248
      GO TO 3650                                                        M42.6249
 3760 TR(K) = XR                                                        M42.6250
      TX(K) = XI                                                        M42.6251
      VR = 0.0                                                          M42.6252
      VI = 0.0                                                          M42.6253
      KE = NE                                                           M42.6254
      DO 3790 LB = 1, M                                                 M42.6255
      KE = KE + 1                                                       M42.6256
      VR = VR + SOLR(LB)*TR(KE) - SOLI(LB)*TX(KE)                       M35.1217
 3790 VI = VI + SOLR(LB)*TX(KE) + SOLI(LB)*TR(KE)                       M35.1218
      XR = SOLR(M+1) - VR                                               M35.1219
      XI = SOLI(M+1) - VI                                               M35.1220
      XA = XR*XR + XI*XI                                                M42.6257
      IF( XA .LT. TOLMAT*E(N) )  GO TO 3560                             M42.6258
      XTR = XR/XA                                                               
      XTI = -XI/XA                                                      M42.6260
      JE = NE                                                           M42.6261
      NE = NE + N                                                       M42.6262
      KY = NE                                                           M42.6263
      TR(NE) = XTR                                                      M42.6264
      TX(NE) = XTI                                                      M42.6265
      DO 3890 JC = 1, M                                                 M42.6266
      LC = N - JC                                                       M42.6267
      KY = KY - 1                                                       M42.6268
      KKK = KY                                                          M42.6269
      VVR = XTR*TR(KY) - XTI*TX(KY)                                     M42.6270
      VVI = XTR*TX(KY) + XTI*TR(KY)                                     M42.6271
      DO 3870 IC = 1, LC                                                M42.6272
      TR(JE) = TR(JE) + TR(KKK)*VVR - TX(KKK)*VVI                       M42.6273
      TX(JE) = TX(JE) + TR(KKK)*VVI + TX(KKK)*VVR                       M42.6274
      JE = JE - 1                                                       M42.6275
 3870 KKK = KKK - 1                                                     M42.6276
      TR(KY) = -VVR                                                     M42.6277
 3890 TX(KY) = -VVI                                                     M42.6278
      IF( IPRSUP .GE. 3 )                                               M42.6279
     1WRITE(LUNIT6, 3897)  IS,  ( TR(JC), TX(JC), JC=IS, N1 )           M17. 501
 3897 FORMAT( /, 9H AT 3897    , I10, /, ( 1X, 8E15.6 ) )               M42.6280
 3900 CONTINUE                                                          M42.6281
      IF (KODEBR(I) .LE. 0 ) GO TO 3320                                 M31.2474
      IF (KODSEM(I) .NE. 0  .AND.                                       M31.2475
     1         IMODEL(I) .NE. -2) GO TO 3320                            M44. 753
      LL = IS + JT - 1                                                  M42.6282
      IF( IPRSUP .GE. 2 )                                               M42.6283
     1WRITE(LUNIT6, 3506)  (  TR(J),  TX(J), J=IS, LL )                 M17. 502
 3506 FORMAT( /,  39H AT 3506.   CMR +J CMI AFTER  INVERSION  ,/,       M42.6284
     1 ( 1X, 8E16.6 ) )                                                 M42.6285
      CALL MULTMX( TR(IS), F(1), E(1), SOLR(1), NT )                    M35.1221
      CALL MOVER( E(1), TR(IS), JT )                                    M42.6286
      CALL MULTMX( TX(IS), F(1), E(1), SOLR(1), NT )                    M35.1222
      CALL MOVER( E(1), TX(IS), JT )                                    M42.6287
      L = IS + JT - 1                                                   M42.6288
      IF( IPRSUP .GE. 2 )                                               M42.6289
     1WRITE(LUNIT6, 3511)  ( J, TR(J), TX(J), J=IS, L )                 M17. 503
 3511 FORMAT( /,  36H AT 3511.   FINAL Y-BRANCH IN TR, TX  ,/,          M42.6290
     1 9X, 1HJ, 15X, 5HTR(J), 15X, 5HTX(J), /, ( 1X, I9, 2E20.8 ) )     M42.6291
C                                                                       M44. 754
C                      Beginning of Multi-phase branches                M44. 755
C                                                                       M44. 756
 3320 K = IS -1                                                                 
      II = I                                                                    
      IENDD = I + NT                                                            
 3330 NA1 = IABS(KBUS(I))                                                       
      NA1 = NORDER(NA1)                                                         
      NA2 = IABS(MBUS(I))                                                       
      NA2 = NORDER(NA2)                                                         
      J = II                                                                    
      GO TO 3440                                                        M42.6300
 3430 J = J +1                                                                  
 3440 K = K +1                                                          M42.6302
      IF (J .EQ. I)   GO TO 3340                                                
      NB1 = IABS(KBUS(J))                                               M42.6304
      NB1 = NORDER(NB1)                                                         
      NB2 = IABS(MBUS(J))                                                       
      NB2 = NORDER(NB2)                                                 M42.6307
      N1 = NA1 - NB1                                                    M42.6308
      IF( N1 .GT. 0 )                                                   M42.6309
     1              GO TO 4030                                                  
      IF( N1 .EQ. 0 )  GO TO 4020                                               
      MNA1B1 = NA1                                                              
      MXA1B1 = NB1                                                              
      GO TO 4040                                                        M42.6313
 4020 DIAG(NA1) = DIAG(NA1) + TR(K)*2.0                                         
      IF (KBUS(I) .LT. 0)   DIAG(NA1) = DIAG(NA1) +R(K)*2.0                     
      DIAB(NA1) = DIAB(NA1) +TX(K)*2.0 +C(K)*2.0*OMEGAC                 M42.6316
      GO TO 4110                                                        M42.6317
 4030 MNA1B1 = NB1                                                      M42.6318
      MXA1B1 = NA1                                                      M42.6319
 4040 L = INDEX(MNA1B1)                                                         
      GO TO 4060                                                                
 4050 L = L + 1                                                                 
 4060 IF (L .LT. INDEX(MNA1B1+1))                                       M42.6323
     1             GO TO 4080                                                   
      KILL = 19                                                         M42.6324
      LSTAT(19) = 4080                                                  M15. 763
      GO TO 9999                                                        M42.6325
 4080 M = ILOC(L)                                                               
      IF (M .NE. MXA1B1)                                                M42.6327
     1          GO TO 4050                                                      
      ISUBS1 =  IOFGND+L                                                M16.1480
      GND(ISUBS1)   = GND(ISUBS1)   + TR(K)                                     
      ISUBS1 =  IOFBND+L                                                M16.1482
      BND(ISUBS1)   = BND(ISUBS1)   +TX(K) +C(K)*OMEGAC                 M16.1483
      ISUBS1 =  IOFGND+L                                                M16.1484
      IF (KBUS(I) .LT. 0) GND(ISUBS1)=GND(ISUBS1)+R(K)                  M16.1485
      if ( iprsup .ge. 1 )                                              VAX.  85
     1 write (LUNIT6,*) ' Below 4080. L, BND(ISUBS1) =', L, BND(ISUBS1) VAX.  86
 4110 N1 = NA1 - NB2                                                    M42.6328
      IF( N1 .GT. 0 )                                                   M42.6329
     1                GO TO 4140                                                
      IF( N1 .EQ. 0 )  GO TO 4130                                               
      MNA1B2 = NA1                                                      M42.6331
      MXA1B2 = NB2                                                      M42.6332
      GO TO 4150                                                                
 4130 DIAG(NA1) = DIAG(NA1) - TR(K)*2.0                                 M42.6334
      DIAB(NA1) = DIAB(NA1) - TX(K)*2.0                                 M42.6335
      GO TO 4220                                                        M42.6336
 4140 MNA1B2 = NB2                                                      M42.6337
      MXA1B2 = NA1                                                      M42.6338
 4150 L = INDEX(MNA1B2)                                                 M42.6339
      GO TO 4170                                                        M42.6340
 4160 L = L + 1                                                         M42.6341
 4170 IF (L .LT. INDEX(MNA1B2 + 1)) GO TO 4190                          M42.6342
      LSTAT(19) = 4170                                                  M15. 764
      GO TO 3112                                                        M42.6343
 4190 M = ILOC(L)                                                       M42.6344
      IF (M .NE. MXA1B2) GO TO 4160                                     M42.6345
      ISUBS1 =  IOFGND+L                                                M16.1486
      GND(ISUBS1)   = GND(ISUBS1)   - TR(K)                             M16.1487
      ISUBS1 =  IOFBND+L                                                M16.1488
      BND(ISUBS1)   = BND(ISUBS1)   - TX(K)                             M16.1489
      if ( iprsup .ge. 1 )                                              VAX.  87
     1 write (LUNIT6,*) ' Below 4190.   L, BND(ISUBS1) =',              VAX.  88
     2                                  L, BND(ISUBS1)                          
 4220 N1 = NA2 - NB1                                                    M42.6346
      IF ( N1 .GT. 0 )  GO TO 4250                                      M42.6347
      IF( N1 .EQ. 0 )  GO TO 4240                                       M42.6348
      MNA2B1 = NA2                                                      M42.6349
      MXA2B1 = NB1                                                      M42.6350
      GO TO 4260                                                        M42.6351
 4240 DIAG(NA2) = DIAG(NA2) - TR(K)*2.0                                 M42.6352
      DIAB(NA2) = DIAB(NA2) -TX(K) * 2.0                                M42.6353
      GO TO 4330                                                        M42.6354
 4250 MNA2B1 = NB1                                                      M42.6355
      MXA2B1 = NA2                                                      M42.6356
 4260 L = INDEX(MNA2B1)                                                 M42.6357
      GO TO 4280                                                        M42.6358
 4270 L = L + 1                                                         M42.6359
 4280 IF (L .LT. INDEX(MNA2B1+1)) GO TO 4300                            M42.6360
      LSTAT(19) = 4280                                                  M15. 765
      GO TO 3112                                                        M42.6361
 4300 M = ILOC(L)                                                       M42.6362
      IF (M .NE. MXA2B1) GO TO 4270                                     M42.6363
      ISUBS1 =  IOFGND+L                                                M16.1490
      GND(ISUBS1)   = GND(ISUBS1)   - TR(K)                             M16.1491
      ISUBS1 =  IOFBND+L                                                M16.1492
      BND(ISUBS1)   = BND(ISUBS1)   - TX(K)                             M16.1493
      if ( iprsup .ge. 1 )                                              VAX.  89
     1 write (LUNIT6,*) ' Below 4300.   L, BND(ISUBS1) =',              VAX.  90
     2                                  L, BND(ISUBS1)                          
 4330 N1 = NA2 - NB2                                                    M42.6364
      IF( N1 .GT. 0 )  GO TO 4360                                       M42.6365
      IF( N1 .EQ. 0 )  GO TO 4350                                       M42.6366
      MNA2B2 = NA2                                                      M42.6367
      MXA2B2 = NB2                                                      M42.6368
      GO TO 4370                                                        M42.6369
 4350 DIAG(NA2) = DIAG(NA2) + TR(K)*2.0                                 M42.6370
      IF (KBUS(I) .LT. 0)   DIAG(NA2) = DIAG(NA2) +R(K)*2.0             M42.6371
      DIAB(NA2) = DIAB(NA2) +TX(K)*2.0 +C(K)*2.0*OMEGAC                 M42.6372
      GO TO 3430                                                        M42.6373
 4360 MNA2B2 = NB2                                                      M42.6374
      MXA2B2 = NA2                                                      M42.6375
 4370 L = INDEX(MNA2B2)                                                 M42.6376
      GO TO 4390                                                        M42.6377
 4380 L = L + 1                                                         M42.6378
 4390 IF (L .LT. INDEX(MNA2B2+1)) GO TO 4410                            M42.6379
      KILL = 20                                                         M42.6380
      LSTAT(19) = 4410                                                  M15. 766
      GO TO 9999                                                        M42.6381
 4410 M = ILOC(L)                                                       M42.6382
      IF (M .NE. MXA2B2)   GO TO 4380                                   M42.6383
      ISUBS1 =  IOFGND+L                                                M16.1494
      GND(ISUBS1)   = GND(ISUBS1)   + TR(K)                             M16.1495
      ISUBS1 =  IOFBND+L                                                M16.1496
      BND(ISUBS1)   = BND(ISUBS1)   +TX(K) +C(K)*OMEGAC                 M16.1497
      ISUBS1 =  IOFGND+L                                                M16.1498
      IF (KBUS(I) .LT. 0)   GND(ISUBS1)   = GND(ISUBS1)   +R(K)         M16.1499
      if ( iprsup .ge. 1 )                                              VAX.  91
     1 write (LUNIT6,*) ' Below 4410.   L, BND(ISUBS1) =',              VAX.  92
     2                                  L, BND(ISUBS1)                          
      if ( iprsup .ge. 1 .AND. l .eq. 1 )                               VAX.  93
     1 write (LUNIT6,*) ' k, tx(k), c(k), omegac =',                    VAX.  94
     2                    k, tx(k), c(k), omegac                                
      GO TO 3430                                                        M42.6384
C                                                      end of [y] budingM44. 761
 3340 N1 = NA2 - NA1                                                    M42.6385
      IF( N1 .GT. 0 )  GO TO 3360                                       M42.6386
      IF( N1 .LT. 0 )  GO TO 3370                                       M42.6387
      LSTAT(19) =3360                                                   M15. 767
      GO TO 3112                                                        M42.6388
 3360 IN = NA2                                                          M42.6389
      IM = NA1                                                          M42.6390
      GO TO 3380                                                        M42.6391
 3370 IN = NA1                                                          M42.6392
      IM = NA2                                                          M42.6393
 3380 IA = INDEX(IM)                                                    M42.6394
      GG = TR(K)                                                        M42.6395
      BB = TX(K)                                                        M42.6396
      GO TO 3400                                                        M42.6397
 3390 IA = IA + 1                                                       M42.6398
 3400 IF (IA .LT. INDEX(IM + 1)) GO TO 3410                             M42.6399
      LSTAT(19) = 3400                                                  M15. 768
      GO TO 3112                                                        M42.6400
 3410 IF (ILOC(IA) .NE. IN) GO TO 3390                                  M42.6401
      ISUBS1 =  IOFGND+IA                                               M16.1500
      GND(ISUBS1)    = GND(ISUBS1)    - GG                              M16.1501
      ISUBS1 =  IOFBND+IA                                               M16.1502
      BND(ISUBS1)    = BND(ISUBS1)    - BB                              M16.1503
      BB = BB +C(K) * OMEGAC                                            M42.6402
      IF (KBUS(I) .LT. 0)   GG = GG +R(K)                               M42.6403
      DIAG(IM) = DIAG(IM) + GG                                          M42.6404
      DIAB(IM) = DIAB(IM) + BB                                          M42.6405
      DIAG(IN) = DIAG(IN) + GG                                          M42.6406
      DIAB(IN) = DIAB(IN) + BB                                          M42.6407
      I = I +1                                                          M42.6408
      IF ( I.LT. IENDD)   GO TO 3330                                    M42.6409
      GO TO 3490                                                        M28.4277
C                                                                       M44. 762
C                                                                       M44. 763
43400 NPHCAS=IABS(LENGTH(I))                                            M42.6410
      LOCY11=NR(I)-1                                                    M42.6411
      DO 43401 IX=1,NPHCAS                                              M42.6412
      N1 = IX + I - 1                                                   M19. 886
      NROW1 = KBUS(N1)                                                  M19. 887
      NROW1=NORDER(NROW1)                                               M42.6413
      NNPOS=LOCATN(IX,IX)+LOCY11                                        M42.6414
      DIAG(NROW1)=DIAG(NROW1)+TR(NNPOS)                                 M42.6415
      DIAB(NROW1)=DIAB(NROW1)+TX(NNPOS)                                 M42.6416
      IX2=IX+1                                                          M42.6417
      IF(IX2.GT.NPHCAS) GO TO 43430                                     M42.6418
      DO 43402 IXX=IX2,NPHCAS                                           M42.6419
      N2 = IXX + I - 1                                                  M19. 888
      NROW2 = KBUS(N2)                                                  M19. 889
      NROW2=NORDER(NROW2)                                               M42.6420
      IF(NROW1.GT.NROW2)GOTO43420                                       M42.6421
      ISTART=INDEX(NROW1)                                               M42.6422
      ISTOP=INDEX(NROW1+1)-1                                            M42.6423
      DO 43403 IY=ISTART,ISTOP                                          M42.6424
      IF(NROW2.EQ.ILOC(IY))GOTO43404                                    M42.6425
43403 CONTINUE                                                          M42.6426
43420 ISTART=INDEX(NROW2)                                               M42.6427
      ISTOP=INDEX(NROW2+1)-1                                            M42.6428
      DO 43410 IY=ISTART,ISTOP                                          M42.6429
      IF(NROW1.EQ.ILOC(IY))GOTO43404                                    M42.6430
43410 CONTINUE                                                          M42.6431
43404 NNPOS=LOCATN(IX,IXX)+LOCY11                                       M42.6432
      ISUBS1 =  IOFGND+IY                                               M16.1504
      GND(ISUBS1)   =GND(ISUBS1)   +TR(NNPOS)                           M16.1505
      ISUBS1 =  IOFBND+IY                                               M16.1506
      BND(ISUBS1)   =BND(ISUBS1)   +TX(NNPOS)                           M16.1507
43402 CONTINUE                                                          M42.6433
43430 DO 43401 IXX = 1,NPHCAS                                           M42.6434
      N3 = IXX + I - 1                                                  M19. 890
      NROW2 = MBUS(N3)                                                  M19. 891
      NROW2=NORDER(NROW2)                                               M42.6435
      IF(NROW1.GT.NROW2)GOTO43412                                       M42.6436
      ISTART=INDEX(NROW1)                                               M42.6437
      ISTOP=INDEX(NROW1+1)-1                                            M42.6438
      DO 43405 IY=ISTART,ISTOP                                          M42.6439
      IF(NROW2.EQ.ILOC(IY))GOTO43406                                    M42.6440
43405 CONTINUE                                                          M42.6441
43412 ISTART=INDEX(NROW2)                                               M42.6442
      ISTOP=INDEX(NROW2+1)-1                                            M42.6443
      DO 43414 IY=ISTART,ISTOP                                          M42.6444
      IF(NROW1.EQ.ILOC(IY))GOTO43406                                    M42.6445
43414 CONTINUE                                                          M42.6446
43406 NNPOS=LOCATN(IX,IXX+NPHCAS)+LOCY11                                M42.6447
      ISUBS1 =  IOFGND+IY                                               M16.1508
      GND(ISUBS1)   =GND(ISUBS1)   +TR(NNPOS)                           M16.1509
      ISUBS1 =  IOFBND+IY                                               M16.1510
      BND(ISUBS1)   =BND(ISUBS1)   +TX(NNPOS)                           M16.1511
43401 CONTINUE                                                          M42.6448
      DO 43407 IX=1,NPHCAS                                              M42.6449
      N1 = IX + I - 1                                                   M19. 892
      NROW1 = MBUS(N1)                                                  M19. 893
      NROW1=NORDER(NROW1)                                               M42.6450
      NNPOS=LOCATN(IX+NPHCAS,IX+NPHCAS)+LOCY11                          M42.6451
      DIAG(NROW1)=DIAG(NROW1)+TR(NNPOS)                                 M42.6452
      DIAB(NROW1)=DIAB(NROW1)+TX(NNPOS)                                 M42.6453
      IX2=IX+1                                                          M42.6454
      IF(IX2.GT.NPHCAS)GOTO43407                                        M42.6455
      DO 43431 IXX = IX2,NPHCAS                                         M42.6456
      N2 = IXX + I - 1                                                  M19. 894
      NROW2 = MBUS(N2)                                                  M19. 895
      NROW2=NORDER(NROW2)                                               M42.6457
      IF(NROW1.GT.NROW2)GOTO43415                                       M42.6458
      ISTART=INDEX(NROW1)                                               M42.6459
      ISTOP=INDEX(NROW1+1)-1                                            M42.6460
      DO 43408 IY=ISTART,ISTOP                                          M42.6461
      IF(NROW2.EQ.ILOC(IY))GOTO43409                                    M42.6462
43408 CONTINUE                                                          M42.6463
43415 ISTART=INDEX(NROW2)                                               M42.6464
      ISTOP=INDEX(NROW2+1) -1                                           M42.6465
      DO 43416 IY=ISTART,ISTOP                                          M42.6466
      IF(NROW1.EQ.ILOC(IY))GOTO43409                                    M42.6467
43416 CONTINUE                                                          M42.6468
43409 NNPOS=LOCATN(IX+NPHCAS,IXX+NPHCAS)+LOCY11                         M42.6469
      ISUBS1 =  IOFGND+IY                                               M16.1512
      GND(ISUBS1)   =GND(ISUBS1)   +TR(NNPOS)                           M16.1513
      ISUBS1 =  IOFBND+IY                                               M16.1514
      BND(ISUBS1)   =BND(ISUBS1)   +TX(NNPOS)                           M16.1515
43431 CONTINUE                                                          M42.6470
43407 CONTINUE                                                          M42.6471
      I=I+NPHCAS                                                                
 3490 IF( I .LE. IBR )  GO TO 3080                                              
C                                                                       M44. 764
C                                                    end of [y] buildingM44. 765
C                                                                       M44. 766
      DO 4502 I = 1, NTOT                                                       
      E(I) = DIAG(I)**2 + DIAB(I)**2                                            
      IF (KODE(I) .GT. 0)   KODE(I) = -KODE(I)                          M42.6476
 4502 CONTINUE                                                          M42.6477
      N3 = 1                                                                    
      CALL VECRSV( TCLOSE(1), KSWTCH, N3 )                              M32.3346
      CALL VECRSV( DIAG(1), NTOT, N3 )                                          
      CALL VECRSV( DIAB(1), NTOT, N3 )                                          
      CALL VECRSV( GND(IOFGND+1), IOFFD, N3 )                                   
      CALL VECRSV( BND(IOFBND+1), IOFFD, N3 )                                   
      N12 = -4                                                                  
      N15 = 0                                                                   
      CALL VECRSV( VOLT(1), N12, N15 )                                          
      IF( IPRSUP .LT. 3 )  GO TO 4517                                   M42.6478
      WRITE(LUNIT6, 4513)  NTOT, IOFFD                                  M17. 506
 4513 FORMAT( /,  77H Y-MATRIX OF S.S. PHASOR SOLUTION, B4 STARTING ELIMM42.6479
     1INATION.    NTOT, IOFFD =  , 2I8, /, 9X, 1HI, 6X, 4HILOC,         M42.6480
     2 5X, 5HINDEX, 17X, 3HGND, 17X, 3HBND, 21X, 4HDIAG, 16X, 4HDIAB  ) M42.6481
      N3 = NTOT + 1                                                             
      IF( IOFFD .GT. N3 )  N3 = IOFFD                                           
      N1 = IOFGND + 1                                                           
      N2 = IOFBND + 1                                                           
      DO 4516  I=1, N3                                                          
      WRITE(LUNIT6, 4514)  I, ILOC(I), INDEX(I), GND(N1), BND(N2),      M17. 507
     1 DIAG(I), DIAB(I)                                                 M42.6487
 4514 FORMAT( 3I10, 2E20.8, 5X, 2E20.8 )                                M42.6488
      N1 = N1 + 1                                                               
 4516 N2 = N2 + 1                                                               
 4517 DO 4570 I = 1, NTOT                                                       
      IF (KODE(I) .GT. 0) GO TO 4570                                    M42.6492
      J = I                                                                     
      IM = -KODE(J)                                                     M42.6494
 4530 IF (IM .GT. 0 ) IN = NORDER(IM)                                   M32.3354
      IL = NORDER(J)                                                            
      IF (IM .EQ. 0) IN = IL                                            M42.6496
      IM = -KODE(IL)                                                    M42.6497
      KODE(IL) = IN                                                     M42.6498
      J = IL                                                            M42.6499
      IF (J .NE. I) GO TO 4530                                          M42.6500
 4570 CONTINUE                                                          M42.6501
      DO 4820 I = 1, NTOT                                               M42.6502
      J = KODE(I)                                                       M42.6503
      IF (J .GE. I) GO TO 4820                                          M42.6504
 4610 J = KODE(J)                                                       M42.6505
      N1 = J - I                                                        M42.6506
      IF( N1 .LT. 0 )  GO TO 4610                                       M42.6507
      IF( N1 .EQ. 0 )  GO TO 4820                                       M42.6508
      J = I                                                             M42.6509
      K = 0                                                             M42.6510
 4640 K = K + 1                                                         M42.6511
      IF ( K  .GT.  LSIZ26 )   GO TO 4670                               M31.2477
      ITEMP(K) = J                                                      M42.6512
      J = KODE(J)                                                       M42.6513
      IF (J .NE. I) GO TO 4640                                          M42.6514
      IF( K .GT. 1 )  GO TO 4680                                        M42.6515
      LSTAT(19) = 4670                                                  M15. 769
      GO TO 3112                                                        M42.6516
 4670 KILL = 21                                                         M42.6517
      LSTAT(19) = 4670                                                  M42.6518
      GO TO 9999                                                        M42.6519
 4680 MK = K - 1                                                        M42.6520
      DO 4760 M = 1, MK                                                 M42.6521
      KMM = K - M                                                       M42.6522
      DO 4750 N = 1, KMM                                                M42.6523
      N1 = ITEMP(N) - ITEMP(N+1)                                        M42.6524
      IF( N1 .GT. 0 )  GO TO 4740                                       M42.6525
      IF( N1 .LT. 0 )  GO TO 4750                                       M42.6526
      LSTAT(19) = 4740                                                  M15. 770
      GO TO 3112                                                        M42.6527
 4740 ITP = ITEMP(N)                                                    M42.6528
      ITEMP(N) = ITEMP(N+1)                                             M42.6529
      ITEMP(N+1) = ITP                                                  M42.6530
 4750 CONTINUE                                                          M42.6531
 4760 CONTINUE                                                          M42.6532
      J = ITEMP(1)                                                      M42.6533
      DO 4800 M = 2, K                                                  M42.6534
      L = ITEMP(M)                                                      M42.6535
      KODE(J) = L                                                       M42.6536
 4800 J = L                                                             M42.6537
      KODE(J) = ITEMP(1)                                                M42.6538
 4820 CONTINUE                                                          M42.6539
      CALL MOVER0 ( SOLR(1), N14 )                                              
      CALL MOVER0 ( SOLI(1), N14 )                                              
      IF ( NUMSUB  .LE.  0 ) GO TO 4905                                         
C     ASSIGN UNIT CURRENT SOURCES TO BEGIN THEVENIN CALCULATION:        M31.2482
      IF ( IPRSUP  .GE.  1 )                                            M31.2483
     1 WRITE (LUNIT6, 4823)  NUMSUB, ISUBEG(NUMSUB+1)                   M31.2484
 4823 FORMAT (  35H BEGIN PHASOR THEVENIN CALCULATION.,                 M31.2485
     1          29H   NUMSUB, ISUBEG(NUMSUB+1) =,  2I6  )               M31.2486
      IF ( IPRSUP  .LE.  1 )   GO TO 4837                               M31.2487
      WRITE (LUNIT6, 4827)  ( ISUBEG(I),  I=1, NUMSUB )                 M31.2488
 4827 FORMAT (    8H ISUBEG:,  20I6  )                                  M31.2489
      N1 = ISUBEG(NUMSUB+1) - 1                                         M31.2490
      WRITE (LUNIT6, 4831)  ( KSUB(I),  I=1, N1 )                       M31.2491
 4831 FORMAT (    6H KSUB:,  20I6  )                                    M31.2492
      WRITE (LUNIT6, 4832)  ( MSUB(I),  I=1, N1 )                       M31.2493
 4832 FORMAT (    6H MSUB:,  20I6  )                                    M32.3355
 4837 L = 1                                                                     
      DO 4892  N2=1, NUMSUB                                                     
      N1 = NTOT                                                                 
      N7 = ISUBEG(N2+1)                                                         
 4863 D8 = -1.0                                                                 
      J = KSUB(L)                                                       M31.2500
      DO 4878  N10=1, 2                                                         
      IF ( J  .EQ.  1 )  GO TO 4871                                             
      N12 = NORDER(J)                                                           
      N5 = N12 + N1                                                             
      SOLR(N5) = D8                                                             
      IF ( IPRSUP  .GE.  3 )                                            M31.2506
     1 WRITE (LUNIT6, 4869)  N5, J, N12, N2, L, D8                      M31.2507
 4869 FORMAT ( 42H THEVENIN SOURCE.  N5, J, N12, N2, L, D8 =,           M31.2508
     1         5I8,  E14.3  )                                           M31.2509
 4871 D8 = 1.0                                                                  
 4878 J = MSUB(L)                                                       M31.2511
      L = L + 1                                                         M31.2512
      IF ( L  .GE.  N7 ) GO TO 4892                                             
      N1 = N1 + NTOT                                                            
      GO TO 4863                                                                
 4892 CONTINUE                                                          M31.2516
 4905 N1 = 2 * LBUS                                                     M31.2517
      IF ( KCONST  .EQ.  0 )   GO TO 4977                               M19. 898
      DO 4976 I = 1, KCONST                                                     
      J = NODE(I)                                                               
      IF ( IABS( IFORM(I) ) .NE.14 ) GO TO 4976                                 
      IF ( TSTART(I)  .EQ.  5432. ) GO TO 4975                                  
      IF(  TSTART(I).GE. 0.0) GO TO 4976                                        
      IF( J .GE. -N1 )  GO TO 4975                                      M32.3356
      NODE(I) = NODE(I) + N1                                            M42.6543
      GO TO 4976                                                        M42.6544
 4975 IF ( IFORM(I+1)  .EQ.  18 )                                       M32.3357
     1 J = TIME1(I+1)                                                   M32.3358
      J = IABS(J)                                                       M32.3359
      J = NORDER(J)                                                             
      SOLR(J) = SOLR(J) + CREST(I) * COSZ(TIME1(I) )                            
      SOLI(J) = SOLI(J) + CREST(I) * SINZ(TIME1(I) )                            
      IF ( IPRSUP .GE. 5 )                                              M35.1225
     1 WRITE (LUNIT6, 1999) I,J,NODE(I),KODE(J),CREST(I),TIME1(I)       M35.1226
 1999 FORMAT( /,39H I,J,NODE(I),KODE(J),CREST(I),TIME1(I) ,             M35.1227
     1        4I4,2E18.6)                                               M35.1228
      IF ( IPRSUP .GE. 5 )                                              M35.1229
     1 WRITE(LUNIT6,1497) ISUBS1,ISUBS2,ISUBS3,ISUBS4,N1,N2,N3,IA,JA,   M35.1230
     2                     II,JJ,KKK,IY,IB,IX                           M35.1231
 1497 FORMAT(15I8)                                                      M35.1232
      IF (J .LE. NCURR)   GO TO 4976                                            
      K = KODE(J)                                                               
 4979 IF (K .EQ. J)  GO TO 4976                                                 
      SOLR(K) = SOLR(J)                                                         
      SOLI(K) = SOLI(J)                                                         
      K = KODE(K)                                                               
      GO TO 4979                                                                
 4976 CONTINUE                                                                  
 4977 NL = 0                                                                    
 4972 N12 = NL + 1                                                              
      N13 = NL + NTOT                                                           
      IF ( IPRSUP  .GE.  2 )                                            M31.2522
     1 WRITE (LUNIT6, 5004)  N12, N13,                                  M31.2523
     2                       ( SOLR(I), SOLI(I), I=N12, N13 )           M31.2524
 5004 FORMAT(   6H CELLS,  I5,  10H   THROUGH,  I5,                     M31.2525
     1  43H   OF RIGHT HAND SIDE VECTOR (COMPLEX) ....                  M31.2526
     2  ,/,  ( 1X,  8E16.7 )  )                                         M31.2527
      NL = NL + NTOT                                                            
      IF ( NL  .LE.  LOOPSS(2) ) GO TO 4972                                     
      IF( IPRSUP .GE. 2 )                                               M42.6554
     1 WRITE(LUNIT6, 5006)  ( KODE(I), I=1, NTOT )                      M17. 510
 5006 FORMAT ( /, 102H (KODE(I), I=1, NTOT)   IN  'OVER10'  JUST BEFORE M42.6555
     1BEGINNING NETWORK ELIMINATION ON STEADY-STATE  (Y) .   ,/,        M42.6556
     2 ( 1X, 20I6 ) )                                                   M42.6557
      I = 1                                                                     
      GO TO 5020                                                        M42.6559
 5010 NL = 0                                                                    
 5013 N12 = NL + I                                                              
      SO = SOLR(N12)*DIAG(I) - SOLI(N12)*DIAB(I)                                
      SOLI(N12) = SOLR(N12)*DIAB(I) + SOLI(N12)*DIAG(I)                         
      SOLR(N12) = SO                                                            
      NL = NL + NTOT                                                            
      IF ( NL  .LE.  LOOPSS(2) ) GO TO 5013                                     
      I = I + 1                                                                 
 5020 IF (I .GT. NCURR) GO TO 5500                                              
      IF (KODE(I) .GT. I) GO TO 5170                                            
      YY = DIAG(I)*DIAG(I) + DIAB(I)*DIAB(I)                                    
      IF( IPRSUP .GE. 6 )                                               M42.6564
     1 WRITE(LUNIT6, 5023)  I, NCURR, INDEX(I), INDEX(I+1),             M22.3254
     2  DIAG(I), DIAB(I)                                                M22.3255
 5023 FORMAT ( 41H NEW ROW  I  OF STEADY-STATE ELIMINATION.  ,          M42.6565
     1 7X, 1HI, 3X, 5HNCURR, 4X, 8HINDEX(I), 2X, 10HINDEX(I+1),         M42.6566
     2 8X, 7HDIAG(I), 8X, 7HDIAB(I), /,  41X, 2I8, 2I12, 2E20.10 )      M42.6567
      IF( YY .GT. TOLMAT*E(I) )  GO TO 5045                                     
      DO 5042  N23=1, NTOT                                              M37.3703
      IF( NORDER(N23) .EQ. I )  GO TO 5043                              M37.3704
 5042 CONTINUE                                                          M42.6569
      KILL = 22                                                         M42.6570
      LSTAT(19) = 5042                                                  M42.6571
      LSTAT(16) = I                                                     M42.6572
      GO TO 9999                                                           10000
 5043 WRITE(LUNIT6, 5041)  BUS(N23), E(I), YY, TOLMAT                   M37.3705
 5041 FORMAT( /, 107H CAUTION. ---- DURING Y-MATRIX ELIMINATION FOR STEA   10002
     1DY-STATE SOLUTION VOLTAGES, A NEAR-ZERO DIAGONAL ELEMENT  ,/,        10003
     2 15X, 10HFOR NODE ', A6,  92H' EXISTS JUST BEFORE RECIPROCATION.     10004
     3 USING MAGNITUDES SQUARED FOR ALL 3 QUANTITIES, WE HAVE  ,/,         10005
     4 15X, 25HORIGINAL DIAGONAL VALUE =, E12.3,                           10006
     5 24H,   QUESTIONABLE VALUE =, E12.3, 21H,   TOLERANCE RATIO =,       10007
     6 E12.3, 1H. ,/, 15X,                                                 10008
     7 107HTHE NODE IN QUESTION MAY BE CONNECTED TO OTHER NODES, FORMING   10009
     8 A SUBNETWORK.   BUT THE SUBNETWORK HAS NO (OR     )                 10010
      WRITE(LUNIT6, 5044)                                               M17. 514
 5044 FORMAT( 15X, 102HVERY WEAK) PATH TO GROUND OR OTHER KNOWN-VOLTAGE    10012
     1NODE IN THE STEADY-STATE.   SOLUTION VOLTAGES OF THIS  ,/,           10013
     2 15X,  35HSUBNETWORK WILL ALL BE SET TO ZERO.   )                    10014
      IF( SOLR(I) .NE. 0.0 )  GO TO 5046                                   10015
      IF( SOLI(I) .EQ. 0.0 )  GO TO 5048                                   10016
 5046 KILL = 23                                                            10017
      LSTAT(19) = 5046                                                     10018
      BUS1 = BUS(N23)                                                   M37.3706
      GO TO 9999                                                           10020
 5048 DIAG(I) = 0.0                                                     M12. 412
      DIAB(I) = 0.0                                                     M12. 413
      GO TO 15045                                                       M12. 414
 5045 DIAG(I) = DIAG(I) / YY                                               10022
      DIAB(I) = -DIAB(I)/YY                                                10023
15045 II = INDEX(I)                                                     M12. 415
      GO TO 5060                                                           10025
 5050 ISUBS1 =  IOFGND+II                                               M16.1517
      GND(ISUBS1)    = GJ                                               M16.1518
      ISUBS1 =  IOFBND+II                                               M16.1519
      BND(ISUBS1)    = BJ                                               M16.1520
      II = II + 1                                                          10028
 5060 IF (II .EQ. INDEX(I+1))GO TO 5010                                    10029
      J = ILOC(II)                                                         10030
      ISUBS1 =  IOFGND+II                                               M16.1521
      ISUBS2 =  IOFBND+II                                               M16.1522
      GJ = GND(ISUBS1)   *DIAG(I) - BND(ISUBS2)   *DIAB(I)              M16.1523
      ISUBS1 =  IOFGND+II                                               M16.1524
      ISUBS2 =  IOFBND+II                                               M16.1525
      BJ = GND(ISUBS1)   *DIAB(I) + BND(ISUBS2)   *DIAG(I)              M16.1526
      ISUBS1 =  IOFGND+II                                               M16.1527
      ISUBS2 =  IOFBND+II                                               M16.1528
      DIAG(J) = DIAG(J) - GJ*GND(ISUBS1)    + BJ*BND(ISUBS2)            M16.1529
      ISUBS1 =  IOFGND+II                                               M16.1530
      ISUBS2 =  IOFBND+II                                               M16.1531
      DIAB(J) = DIAB(J) - GJ*BND(ISUBS2)    - BJ*GND(ISUBS1)            M16.1532
      ISUBS1 =  IOFGND+II                                               M16.1533
      ISUBS2 =  IOFBND+II                                               M16.1534
      IF( IPRSUP .GE. 20 )                                                 10035
     1 WRITE(LUNIT6, 5063)  I, II, J, GJ, BJ, DIAG(I), DIAB(I),         M17. 515
     2 DIAG(J), DIAB(J), GND(ISUBS1)   , BND(ISUBS2)                    M16.1535
 5063 FORMAT( /,  78H Y(I,J)/Y(I,I)   AND   Y(J,J) - Y(J,I)*(Y(I,J)/Y(I,   10038
     1I))   AT 5063.   I, II, J =  , 3I10,                                 10039
     1 /, 1X, 8E16.6  )                                                    10040
      IF (J .GT. NCURR)   GO TO 5072                                       10041
      NL = 0                                                            M31.2537
 5068 N12 = NL + J                                                      M31.2538
      N13 = NL + I                                                      M31.2539
      SOLR(N12) = SOLR(N12) - GJ*SOLR(N13) + BJ*SOLI(N13)               M31.2540
      SOLI(N12) = SOLI(N12) - GJ*SOLI(N13) - BJ*SOLR(N13)               M31.2541
      NL = NL + NTOT                                                    M31.2542
      IF ( NL  .LE.  LOOPSS(2) )   GO TO 5068                           M31.2543
 5072 JA = INDEX(J)                                                        10044
      IA = II +1                                                           10045
 5080 IF (IA .EQ. INDEX(I+1)) GO TO 5050                                   10046
      IK = ILOC(IA)                                                        10047
      GO TO 5110                                                           10048
 5100 JA = JA + 1                                                          10049
 5110 IF (JA .LT. INDEX(J+1)) GO TO 5130                                   10050
      LSTAT(19) = 5110                                                  M15. 771
      GO TO 3112                                                           10052
 5130 JK = ILOC(JA)                                                        10053
      IF (IK .NE. JK)   GO TO 5100                                         10054
      ISUBS1 =  IOFGND+JA                                               M16.1536
      ISUBS2 =  IOFGND+IA                                               M16.1537
      ISUBS3 =  IOFBND+IA                                               M16.1538
      GND(ISUBS1)    = GND(ISUBS1)    - GJ*GND(ISUBS2)                  M16.1539
     1 + BJ*BND(ISUBS3)                                                 M16.1540
      ISUBS1 =  IOFBND+JA                                               M16.1541
      ISUBS2 =  IOFBND+IA                                               M16.1542
      ISUBS3 =  IOFGND+IA                                               M16.1543
      BND(ISUBS1)    = BND(ISUBS1)    - GJ*BND(ISUBS2)                  M16.1544
     1 -BJ * GND(ISUBS3)                                                M16.1545
      ISUBS1 =  IOFGND+JA                                               M16.1546
      ISUBS2 =  IOFBND+JA                                               M16.1547
      ISUBS3 =  IOFGND+IA                                               M16.1548
      ISUBS4 =  IOFBND+IA                                               M16.1549
      IF( IPRSUP .GE. 20 )                                                 10059
     1 WRITE(LUNIT6, 5134)  JA, JK, IA, GND(ISUBS1)   , BND(ISUBS2)   , M17. 516
     2 GND(ISUBS3)   , BND(ISUBS4)                                      M16.1551
 5134 FORMAT( /,  24H AT 5134.   JA, JK, IA = , 3I10, /, 1X, 4E20.6 )      10062
      IA = IA + 1                                                          10063
      JA = JA + 1                                                          10064
      GO TO 5080                                                           10065
 5170 II = INDEX(I)                                                        10066
      J = KODE(I)                                                          10067
      JJ = INDEX(J)                                                        10068
      GO TO 5190                                                           10069
 5180 II = II + 1                                                          10070
 5190 IF (II .GE. INDEX(I+1)) GO TO 5360                                   10071
      IA = ILOC(II)                                                        10072
      N1 = IA - J                                                          10073
      IF( N1 .GT. 0 )  GO TO 5230                                          10074
      IF( N1 .LT. 0 )  GO TO 5300                                          10075
      ISUBS1 =  IOFGND+II                                               M16.1552
      DIAG(J) = DIAG(J) + DIAG(I) + 2.*GND(ISUBS1)                      M16.1553
      ISUBS1 =  IOFBND+II                                               M16.1554
      DIAB(J) = DIAB(J) + DIAB(I) + 2.*BND(ISUBS1)                      M16.1555
      GO TO 5180                                                           10078
 5230 IF (JJ .LT. INDEX(J+1)) GO TO 5250                                   10079
      IF( IPRSUP .GE. 1 )                                                  10080
     1 WRITE(LUNIT6, 5231)  ( INDEX(I), ILOC(I), I=1, 40 )              M17. 517
 5231 FORMAT( /, 9H AT 5231   ,/, ( 1X, 10I12 ) )                          10082
      LSTAT(19) = 5231                                                  M15. 772
      GO TO 3112                                                           10084
 5250 JA = ILOC(JJ)                                                        10085
      IF (IA .EQ. JA)   GO TO 5290                                         10086
      JJ = JJ + 1                                                          10087
      GO TO 5230                                                           10088
 5290 ISUBS1 =  IOFGND+JJ                                               M16.1556
      ISUBS2 =  IOFGND+II                                               M16.1557
      GND(ISUBS1)    = GND(ISUBS1)    + GND(ISUBS2)                     M16.1558
      ISUBS1 =  IOFBND+JJ                                               M16.1559
      ISUBS2 =  IOFBND+II                                               M16.1560
      BND(ISUBS1)    = BND(ISUBS1)    + BND(ISUBS2)                     M16.1561
      JJ = JJ + 1                                                          10091
      GO TO 5180                                                           10092
 5300 KKK = INDEX(IA)                                                      10093
      GO TO 5320                                                           10094
 5310 KKK = KKK + 1                                                        10095
      IF (KKK .LT. INDEX(IA+1))   GO TO 5320                               10096
      KILL = 24                                                            10097
      LSTAT(19) = 5320                                                  M15. 773
      LSTAT(16) = KKK                                                      10099
      LSTAT(15) = IA                                                       10100
      LSTAT(14) = INDEX(IA+1)                                              10101
      GO TO 9999                                                           10102
 5320 KA = ILOC(KKK)                                                       10103
      IF (KA .NE. J)   GO TO 5310                                          10104
      ISUBS1 =  IOFGND+KKK                                              M16.1562
      ISUBS2 =  IOFGND+II                                               M16.1563
      GND(ISUBS1)     = GND(ISUBS1)     + GND(ISUBS2)                   M16.1564
      ISUBS1 =  IOFBND+KKK                                              M16.1565
      ISUBS2 =  IOFBND+II                                               M16.1566
      BND(ISUBS1)     = BND(ISUBS1)     + BND(ISUBS2)                   M16.1567
      GO TO 5180                                                           10107
 5360 NL = 0                                                            M31.2544
 5366 N12 = NL + J                                                      M31.2545
      N13 = NL + I                                                      M31.2546
      SOLR(N12) = SOLR(N12) + SOLR(N13)                                 M31.2547
      SOLI(N12) = SOLI(N12) + SOLI(N13)                                 M31.2548
      NL = NL + NTOT                                                    M31.2549
      IF ( NL  .LE.  LOOPSS(2) )   GO TO 5366                           M31.2550
      I = I + 1                                                            10110
      GO TO 5020                                                           10111
 5500 IF ( ISTEP  .NE.  -4567 )  GO TO 5757                             M36.1063
      ISTEP = 0                                                         M36.1064
      CALL FXSOUR                                                       M36.1065
      IF ( KILL  .GT.  0 )  GO TO 9999                                  M36.1066
      GO TO 6000                                                        M36.1067
 5757 I = NCURR                                                         M35.1233
 5510 IF (I .EQ. 0) GO TO 6000                                             10113
      IF (KODE(I) .GT. I) GO TO 5560                                       10114
      II = INDEX(I)                                                        10115
 5540 IF (II .EQ. INDEX(I+1)) GO TO 5570                                   10116
      J = ILOC(II)                                                         10117
      ISUBS1 =  IOFGND+II                                               M16.1568
      ISUBS2 =  IOFBND+II                                               M16.1569
      NL = 0                                                            M31.2551
 5547 N12 = NL + I                                                      M31.2552
      N13 = NL + J                                                      M31.2553
      SOLR(N12) = SOLR(N12)  -  GND(ISUBS1) * SOLR(N13)   +             M31.2554
     1                          BND(ISUBS2) * SOLI(N13)                 M31.2555
      SOLI(N12) = SOLI(N12)  -  GND(ISUBS1) * SOLI(N13)   -             M31.2556
     1                          BND(ISUBS2) * SOLR(N13)                 M31.2557
      IF( IPRSUP .GE. 20 )                                                 10122
     1 WRITE(LUNIT6, 5191)  NL, I, II, J, SOLR(N12), SOLI(N12),         M31.2558
     2    SOLR(N13), SOLI(N13), GND(ISUBS1), BND(ISUBS2)                M31.2559
 5191 FORMAT( /,  39H BACKSUBSTITUTION STEP.  NL, I, II, J =,           M31.2560
     1   4I9   ,/,   1X, 6E20.6  )                                      M31.2561
      NL = NL + NTOT                                                    M31.2562
      IF ( NL  .LE.  LOOPSS(2) )   GO TO 5547                           M31.2563
      II = II + 1                                                          10127
      GO TO 5540                                                           10128
 5560 J = KODE(I)                                                          10129
      NL = 0                                                            M31.2564
 5563 N12 = NL + I                                                      M31.2565
      N13 = NL + J                                                      M31.2566
      SOLR(N12) = SOLR(N13)                                             M31.2567
      SOLI(N12) = SOLI(N13)                                             M31.2568
      NL = NL + NTOT                                                    M31.2569
      IF ( NL  .LE.  LOOPSS(2) )   GO TO 5563                           M31.2570
 5570 I = I - 1                                                            10132
      GO TO 5510                                                           10133
 6000 LASTOV = NCHAIN                                                   M36.1068
      NCHAIN = 11                                                       M15. 775
      NL = 0                                                            M31.2571
 6007 N12 = NL + 1                                                      M31.2572
      N13 = NL + NTOT                                                   M31.2573
      IF( IPRSUP .GE. 2 )                                                  10135
     1 WRITE (LUNIT6, 6013)  NL, (SOLR(I), SOLI(I), I=N12, N13)         M31.2574
 6013 FORMAT( /, 43H S.S. SOLUTION AT EXIT FROM "OVER10".  NL =,        M37.3707
     1   I5  ,/, ( 1X, 10E13.4 ) )                                      M31.2576
      NL = NL + NTOT                                                    M31.2577
      IF ( NL  .LE.  LOOPSS(2) )   GO TO 6007                           M31.2578
      IF ( IPRSUP  .GE.  1 )                                            M32.3360
     1 WRITE ( LUNIT6, 4568 )                                           M32.3361
 4568 FORMAT ( 24H  "EXIT  MODULE OVER10." )                            M32.3362
      GO TO 99999                                                          10139
 9999 LASTOV = NCHAIN                                                   M15. 776
      NCHAIN = 51                                                       M15. 777
      LSTAT(18) = 10                                                       10141
      IF ( IPRSUP  .GE.  1 )                                            M32.3363
     1 WRITE ( LUNIT6, 4568 )                                           M32.3364
99999 RETURN                                                               10142
      END                                                                  10143
      SUBROUTINE FXSOUR                                                 M36.1076
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE OF OVERLAY 10 IS CALLED ONLY ONCE, IF AND ONLY        M36.1077
C     IF THE DATA CASE INVOLVES PHASOR POWER (P+JQ, ETC.)               M36.1078
C     CONSTRAINTS.  THIS IS EMTP LOAD FLOW OF "FIX SOURCE" REQUEST.     M36.1079
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      INCLUDE  'DECK10.FOR;/NOLIST'                                           
      DIMENSION   VDIFF(20)                                                     
      IF (IPRSUP .GE. 2) WRITE (LUNIT6,10)                              M37.3708
   10 FORMAT(/,33H TOP OF FXSOUR = LOAD FLOW MODULE)                    M37.3709
      LLM1 = -1                                                         M36.1085
      LL0 = 0                                                           M36.1086
      LL1 = 1                                                           M36.1087
      LL2 = 2                                                           M36.1088
      NEKSTP = 0                                                        M38.2208
      IF ( IPRSUP .LE. 0 )  GO TO 2000                                          
      CALL RUNTYM ( CC1, CC2 )                                          M36.1091
      WRITE ( LUNIT6, 8484 )  CC1, CC2                                  M36.1092
 8484 FORMAT ( 20H  @@@@@@@@  TIME =  , 2E20.5 )                        M36.1093
 2000 CONTINUE                                                          M39.1528
      DECODE (80,2250,ABUFF) NNNOUT, NITERA, NFLOUT,                    M36.1106
     1       NPRINT, RALCHK, CFITEV, CFITEA                             M39.1529
 2250 FORMAT ( 8X, 4I8, 3E8.0 )                                         M39.1530
      IF ( NITERA  .EQ.  0 )  NITERA = 500                                      
C     NEXT ONLY ALLOW NFLOUT = 1, 2, 4, 5, 10, OR 20 VALUES             M36.1110
      IF (NFLOUT .LE. 0  .OR.  NFLOUT .GT. 20)  NFLOUT = 20             M36.1111
 2256 IF ( 20/NFLOUT*NFLOUT .EQ. 20 )  GO TO 2259                       M36.1112
      NFLOUT = NFLOUT + 1                                               M36.1113
      GO TO 2256                                                        M36.1114
 4567 WRITE ( LUNIT6, 5678 )                                            M38.2211
 5678 FORMAT( 26H ***** NO FIX SOURCE ***** )                           M38.2212
      GO TO 9000                                                        M38.2213
 2259 IF (RALCHK .EQ. 0.0) RALCHK = 0.01                                        
      IF (CFITEV .EQ. 0.0) CFITEV = 0.2                                         
      IF (CFITEA .EQ. 0.0) CFITEA = 2.5                                         
      IF (NOUTPR .EQ. 0) WRITE (KUNIT6, 3636) NITERA, RALCHK            M39.1532
 3636 FORMAT ( 23H+ LAST FIX SOURCE CARD. , I6, F14.8 )                 M39.1533
      NKR = LSTAT(69)                                                   M39.1534
      IF ( NKR  .EQ.  0 )  GO TO 4567                                   M38.2214
      EKNOM1 = 0.0                                                      M39.1535
      DO 8765 I = 1, NKR                                                M39.1536
      PMAX = ABSZ( FIXBU5(I) )                                          M39.1537
      QMAX = ABSZ( FIXBU8(I) )                                          M39.1538
      IF ( EKNOM1 .LT. PMAX )  EKNOM1 = PMAX                            M39.1539
 8765 IF ( EKNOM1 .LT. QMAX )  EKNOM1 = QMAX                            M39.1540
      NEKITE = 0                                                                
      PICON = 360. / TWOPI                                              M36.1121
      N14 = NTOT + LOOPSS(2)                                            M36.1122
      NP = NCURR + 1                                                    M36.1123
      DO 1010  I = 1, NTOT                                              M36.1124
      JJ = NORDER(I)                                                    M36.1125
 1010 JNDEX(JJ) = I                                                     M36.1126
      MM = 0                                                            M36.1127
      DO 1030  JJ = NP, NTOT                                            M36.1128
      IF (JJ .EQ. NORDER(1)) GO TO 1030                                         
      IF (KODE(JJ) .GT. JJ) GO TO 1030                                          
      I = JNDEX(JJ)                                                             
      MM = MM + 1                                                               
      FXTEM1(MM) = BUS(I)                                                       
      KNDEX(MM) = I                                                             
      DO 1039 IXX = 1, KCONST                                           M36.1135
      IF (IABS(NODE(IXX)) .NE. I) GO TO 1039                                    
      IF (CREST(IXX) .NE. 0) FXTEM6(MM) = 1.0/CREST(IXX)                        
 1039 CONTINUE                                                          M36.1138
 1030 CONTINUE                                                          M36.1139
      CALL MOVER(SOLR(1), SOLRSV(1), N14)                                       
      CALL MOVER(SOLI(1), SOLISV(1), N14)                                       
      GO TO 3466                                                                
C     BEGIN CALCULATION OF NODE CURRENTS                                M36.1143
 1000 N3 = 2                                                                    
      CALL VECRSV( DIAGG(1), NTOT, N3 )                                 M36.1145
      CALL VECRSV( DIABB(1), NTOT, N3 )                                 M36.1146
      IF ( KBURRO .EQ. 1 ) GO TO 4372                                   M39.1541
      IF ( IOFFD .LE. LSTAT(14) )  GO TO 4372                                   
      WRITE ( LUNIT6, 4848 ) IOFFD, LSTAT(14)                           M39.1543
 4848 FORMAT ( 41H NO ENOUGH SPACE FOR LOAD FLOW .  IOFFD = ,           M39.1544
     1  I8, 33H BUT THE LIMIT SPACE IS LSIZ23 =  , I8 )                 M39.1545
      STOP                                                              M39.1546
 4372 CALL VECRSV( GNDD(1), IOFFD, N3 )                                 M39.1547
      CALL VECRSV( BNDD(1), IOFFD, N3 )                                 M36.1148
      N3 = -4                                                           M36.1149
      N15 = 0                                                           M36.1150
      CALL VECRSV( VOLT(1), N3, N15 )                                           
      NEKCC = 0                                                                 
      VDIFF(NPP) = 0.0                                                          
      IF ( NCURR  .LE.  0 )  NCURR = 1                                  M36.1154
      DO 7346  IP=1, NTOT                                               M36.1155
      E(IP) = 0.0                                                               
 7346 F(IP) = 0.0                                                               
      I = 1                                                             M36.1158
 7350 IF( I .GT. NTOT )  GO TO 7390                                             
      VR = SOLR(I)                                                      M36.1160
      VI = SOLI(I)                                                      M36.1161
      JJ = INDEX(I)                                                     M36.1162
      KKK = INDEX(I+1)                                                  M36.1163
      E(I) = E(I)  +  DIAGG(I)*VR - DIABB(I)*VI                         M36.1164
      F(I) = F(I)  +  DIAGG(I)*VI + DIABB(I)*VR                         M36.1165
      GO TO 7370                                                        M36.1166
 7360 JJ = JJ + 1                                                       M36.1167
 7370 IF( JJ .GE. KKK )  GO TO 7380                                     M36.1168
      J = ILOC(JJ)                                                      M36.1169
      DER = GNDD(JJ)   *VR - BNDD(JJ)   *VI                             M36.1170
      DEI = GNDD(JJ)   *VI + BNDD(JJ)   *VR                             M36.1171
      E(J) = E(J) + DER                                                 M36.1172
      F(J) = F(J) + DEI                                                 M36.1173
      E(I) = E(I)  +  GNDD(JJ)   *SOLR(J) - BNDD(JJ)   *SOLI(J)         M36.1174
      F(I) = F(I)  +  GNDD(JJ)   *SOLI(J) + BNDD(JJ)   *SOLR(J)         M36.1175
      GO TO 7360                                                        M36.1176
 7380 I = I + 1                                                         M36.1177
      GO TO 7350                                                        M36.1178
 7390 I = NCURR + 1                                                             
 7405 IF(I .GT. NTOT) GO TO 8888                                                
      IF (I .EQ. NORDER(1))  GO TO 7440                                         
      J = KODE(I)                                                       M36.1182
      IF (J .LE. I) GO TO 7410                                                  
      E(J) = E(I) + E(J)                                                        
      F(J) = F(I) + F(J)                                                        
      GO TO 7440                                                        M36.1186
C     BEGIN CALCULATION OF  P,Q,V FOR VOLTAGE SOURCES                   M36.1187
 7410 NEKCC = NEKCC + 1                                                         
      FXTEM5(NEKCC) = SQRTZ( SOLR(I)**2 + SOLI(I)**2 )                  M36.1189
      FXTEM4(NEKCC) = 0.0                                               M36.1190
      IF ( FXTEM5(NEKCC)  .GT.  0.0 )                                   M36.1191
     1     FXTEM4(NEKCC) = PICON * ATAN2Z( SOLI(I), SOLR(I) )           M36.1192
      FXTEM2(NEKCC) = ( SOLR(I) * E(I) + SOLI(I) * F(I) ) * ONEHAF      M36.1193
      FXTEM3(NEKCC) = ( SOLI(I) * E(I) - SOLR(I) * F(I) ) * ONEHAF      M36.1194
      IF (IPRSUP  .GT.  8)  WRITE (LUNIT6, 1050)  FXTEM1(NEKCC),        M36.1195
     1  FXTEM2(NEKCC),FXTEM3(NEKCC),FXTEM4(NEKCC),FXTEM5(NEKCC)         M36.1196
 1050 FORMAT ( 22H BUS,P,Q,ANGLE,V(PEAK), A6, 2X, 4E13.4 )              M36.1197
 7440 I = I + 1                                                         M36.1198
      GO TO 7405                                                        M36.1199
C     BEGIN TO CHECK P,Q,V, ANGLE, CORRECTION OF THEM                   M36.1200
 8888 NEKSTP = 1                                                                
      EKITEV = ( NITERA - NEKITE ) * ( NITERA - NEKITE )                M36.1202
      EKFACT = NITERA * NITERA                                          M36.1203
      EKITEV = EKITEV / EKFACT                                          M36.1204
      EKFACT = EKITEV * CFITEA                                                  
      EKITEV = EKITEV * CFITEV                                                  
      DO 7010 I = 1, NKR                                                M36.1208
      PSUM = 0.0                                                                
      QSUM = 0.0                                                                
      NEKN1 = 0                                                                 
      DO 7030 K = 1, NEKCC                                              M36.1212
      IF (FXTEM1(K) .EQ. FIXBU1(I)) GO TO 7171                                  
      IF (FXTEM1(K) .EQ. FIXBU2(I)) GO TO 7171                                  
      IF (FXTEM1(K) .NE. FIXBU3(I)) GO TO 7030                                  
 7171 PSUM = PSUM + FXTEM2(K)                                                   
      QSUM = QSUM + FXTEM3(K)                                                   
      NEKN1 = NEKN1 + 1                                                         
      IF (NEKN1 .EQ. 3)  GO TO 7272                                             
 7030 CONTINUE                                                          M36.1220
      IF (NEKN1 .NE. 0)  GO TO 7272                                             
      WRITE ( LUNIT6, 7032 )  I                                         M36.1222
 7032 FORMAT( 45H ERROR IN TABLE, NODE NAME ON FIX SOURCE CARD,         M36.1223
     1        43H DOES NOT FIT ANY NODE NAME ON SOURCE CARDS ,          M36.1224
     2   /,   18H FIX SOURCE NUMBER, I4, 16H WILL BE IGNORED )          M36.1225
      GO TO 7010                                                                
 7272 IF ( IPRSUP  .GE.  2 ) WRITE ( LUNIT6, 7035 ) I, PSUM, QSUM       M37.3723
 7035 FORMAT ( 25H FIX SOURCE, PSUM, QSUM : , I6, 2X, 2E13.4  )         M36.1228
      FIXB10(I) = PSUM                                                  M39.1548
      FIXB11(I) = QSUM                                                  M39.1549
C     DENOMINATOR IS CALCULATED                                         M36.1229
C     EKNOM1 = ONEHAF * ( ABSZ (PSUM) + ABSZ( FIXBU5(I) ) )             M39.1550
C     EKNOM2 = ONEHAF * ( ABSZ (QSUM) + ABSZ( FIXBU8(I) ) )             M39.1551
      EKDIF1 = 0.0                                                              
      EKDIF2 = 0.0                                                              
      IF (NEKFIX(I) .EQ. 2)  GO TO 8110                                         
      IF (EKNOM1 .LT. TOLMAT)  EKNOM1 = TOLMAT                          M36.1235
      EKDIF1 = (PSUM - FIXBU5(I)) / EKNOM1                                      
      IF (NEKFIX(I) .EQ. 1)  GO TO 8112                                         
 8110 EKDIF2 = (QSUM - FIXBU8(I)) / EKNOM1                                      
C     THE RELATIVE DIFFERENCE CHECK FOR P AND Q                         M36.1240
 8112 IF (ABSZ(EKDIF1) .LE. RALCHK .AND. ABSZ(EKDIF2) .LE. RALCHK)      M36.1241
     1 GO TO 3434                                                       M36.1242
C     THE ABSOLUTE DIFFERENCE CHECK FOR P AND Q                         M36.1243
C     IF ( ABSZ( PSUM - FIXBU5(I) )  .LT.  ABSCHK   .AND.               M39.1553
C    1  ABSZ(QSUM - FIXBU8(I)) .LT. ABSCHK )  GO TO 3434                M39.1554
      NEKSTP = 0                                                                
C     CHANGE PEAK VOLTAGE AND ANGLE                                     M36.1247
 3434 EKDIF1 = EKDIF1 * EKFACT                                          M37.3724
      IF ( EKDIF1  .GT.  1.0 )  EKDIF1 =   1.0                          M37.3725
      IF ( EKDIF1  .LT. -1.0 )  EKDIF1 = - 1.0                          M37.3726
      EKDIF2 = EKDIF2 * EKITEV                                          M37.3727
      IF ( EKDIF2  .GT.  0.01 ) EKDIF2 =   0.01                         M37.3728
      IF ( EKDIF2  .LT. -0.01 ) EKDIF2 = - 0.01                         M37.3729
      IF ( IPRSUP .GT. 1 )  WRITE ( LUNIT6, 3536 ) EKDIF1, EKDIF2       M37.3730
 3536 FORMAT ( 29H  VOLTAGE AND ANGLE CHANGE = , 2E20.6  )              M37.3731
      DO 8050 NP = 1, 3                                                 M37.3732
      GO TO  ( 4141, 4242, 4343 ),  NP                                  M39.1555
 4141 DO 4149  MM = 1, NEKCC                                            M37.3734
 4149 IF ( FXTEM1(MM)  .EQ.  FIXBU1(I) )   GO TO 8181                   M37.3735
      GO TO 7010                                                        M37.3736
 4242 IF ( FIXBU2(I)  .EQ.  BLANK ) GO TO 7010                          M37.3737
      DO 4249  MM = 1, NEKCC                                            M37.3738
 4249 IF ( FXTEM1(MM)  .EQ.  FIXBU2(I) )   GO TO 8282                   M37.3739
      GO TO 7010                                                        M37.3740
 4343 IF ( FIXBU3(I)  .EQ.  BLANK )  GO TO 7010                         M37.3741
      DO 4349  MM = 1, NEKCC                                            M37.3742
 4349 IF ( FXTEM1(MM)  .EQ.  FIXBU3(I) )   GO TO 8383                   M37.3743
      GO TO 7010                                                        M37.3744
 8181 EKN1 = 0.0                                                                
 6769 ANGEL = FXTEM4(MM) - EKDIF1                                       M39.1556
      IF (ANGEL .LE. FIXBU6(I) .AND. ANGEL .GE. FIXBU4(I)) GO TO 6363   M39.1557
      EKDIF1 = 0.1 * EKDIF1                                             M39.1558
C         From Ma Ren-ming.   Installed March, 1987.                    VAX.1225
      IF ( ABSZ ( EKDIF1 ) .GT. EPSILN ) GO TO 6769                     VAX.1226
      EKDIF1 = 0.0                                                      VAX.1227
      GO TO 6363                                                        VAX.1228
 8282 EKN1 = -120.0                                                             
      GO TO 6363                                                        M36.1262
 8383 EKN1 = 120.0                                                              
 6363 IF ( NEKFIX(I) .EQ. 2 )  FXTEM4(MM) = FIXBU5(I) + EKN1            M37.3752
      IF (NEKFIX(I) .EQ. 2) GO TO 8400                                          
C     CORRECTION OF ANGLES                                              M36.1267
      FXTEM4(MM) = FXTEM4(MM) - EKDIF1                                  M37.3753
C     VOLTAGE                                                           M36.1269
      IF ( NEKFIX(I)  .EQ.  1 )  FXTEM5(MM) = FIXBU8(I)                 M36.1270
      IF (NEKFIX(I) .EQ. 1) GO TO 8510                                          
C     CORRECTION OF VOLTAGES                                            M36.1272
 8400 FXTEM5(MM) = FXTEM5(MM)*( 1.0 - EKDIF2 )                          M37.3754
C     CHECK FOR MIN/MAX VOLTAGE                                         M36.1274
      IF (FXTEM5(MM)  .GT.  FIXBU9(I))  FXTEM5(MM) = FIXBU9(I)          M36.1275
      IF (FXTEM5(MM)  .LT.  FIXBU7(I))  FXTEM5(MM) = FIXBU7(I)          M36.1276
C     NEW VOLTAGE IS CALCULATED                                         M36.1277
 8510 EKDGR = FXTEM4(MM) / PICON                                        M36.1278
      NEKN4 = KNDEX(MM)                                                         
      NEKN4 = NORDER(NEKN4)                                                     
      SOLRSV(NEKN4) = FXTEM5(MM) * COSZ (EKDGR)                                 
      SOLISV(NEKN4) = FXTEM5(MM) * SINZ (EKDGR)                                 
      NKODE = KODE(NEKN4)                                                       
 8585 IF (NKODE .EQ. NEKN4) GO TO 8787                                          
      SOLRSV(NKODE) = SOLRSV(NEKN4)                                             
      SOLISV(NKODE) = SOLISV(NEKN4)                                             
      NKODE = KODE(NKODE)                                                       
      GO TO 8585                                                        M36.1288
C     CALCULATION OF VOLTAGE CHANGE                                     M36.1289
 8787 CCHANR = SOLRSV(NEKN4) - SOLR(NEKN4)                                      
      CCHANI = SOLISV(NEKN4) - SOLI(NEKN4)                                      
      IF ( ABSZ(CCHANI) .GT. ABSZ(CCHANR) ) CCHANR = CCHANI             M36.1292
      CCHANR = CCHANR * FXTEM6(MM)                                      M36.1293
      IF ( ABSZ(CCHANR) .LE. ABSZ(VDIFF(NPP)) )  GO TO 7777             M36.1294
      VDIFF(NPP) = CCHANR                                                       
      JNDEX(NPP) = I                                                            
 7777 IF (IPRSUP .GE. 4)  WRITE ( LUNIT6, 8800 ) FXTEM1(MM),            M37.3755
     1   SOLR(NEKN4), SOLI(NEKN4), SOLRSV(NEKN4), SOLISV(NEKN4)         M36.1298
 8800 FORMAT ( 32H BUSNAME,SOLR(NEKN4),SOLI(NEKN4),A6,2E16.6,           M36.1299
     1         20H   THE NEW VALUE IS ,  2E16.6 )                       M36.1300
 8050 CONTINUE                                                          M36.1302
 7010 CONTINUE                                                          M36.1303
      IF ( NEKSTP .EQ. 1 )  GO TO 2121                                          
      IF ( NFLKNT .GT. 0 )   GO TO 2222                                         
 2121 WRITE (LUNIT6, 3333) (VDIFF(K), K = 1, NPP)                       M39.1560
 3333 FORMAT ( 12H+VCHANG(K)= , 20F6.3 )                                M36.1307
      IF ( NPP .LT. 20   .AND.                                          M36.1308
     1  NEKSTP .NE. 1 )  GO TO 3476                                             
      IF ( NNNOUT  .EQ.  1 )                                            M36.1310
     1 WRITE (LUNIT6, 4444) (JNDEX(K), K = 1, NPP)                              
 4444 FORMAT ( 12H FIX SOURCE , 20I6 )                                  M36.1312
 3466 WRITE (LUNIT6, 3471)                                                      
 3471 FORMAT ( 1X )                                                     M36.1314
      NPP = 0                                                                   
 3476 NFLKNT = NFLOUT                                                           
 2222 IF (NEKSTP .EQ. 1) GO TO 6789                                             
      IF (NEKITE .GE. NITERA) GO TO 8900                                        
      NFLKNT = NFLKNT - 1                                                       
      DO 2517  IP=1, N14                                                M36.1320
      SOLR(IP) = SOLRSV(IP)                                             M36.1321
 2517 SOLI(IP) = SOLISV(IP)                                                     
      NEKITE = NEKITE + 1                                                       
      NPP = NPP + 1                                                             
      I = NCURR                                                                 
 5510 IF (I .EQ. 0) GO TO 1000                                                  
      IF (KODE(I) .GT. I) GO TO 5560                                    M36.1327
      II = INDEX(I)                                                     M36.1328
 5540 IF (II .EQ. INDEX(I+1)) GO TO 5570                                M36.1329
      J = ILOC(II)                                                      M36.1330
      ISUBS1 =  IOFGND+II                                               M36.1331
      ISUBS2 =  IOFBND+II                                               M36.1332
      SOLR(I) = SOLR(I)  -  GND(ISUBS1) * SOLR(J)   +                   M36.1333
     1                          BND(ISUBS2) * SOLI(J)                   M36.1334
      SOLI(I) = SOLI(I)  -  GND(ISUBS1) * SOLI(J)   -                   M36.1335
     1                          BND(ISUBS2) * SOLR(J)                   M36.1336
      II = II + 1                                                       M36.1337
      GO TO 5540                                                        M36.1338
 5560 J = KODE(I)                                                       M36.1339
      SOLR(I) = SOLR(J)                                                 M36.1340
      SOLI(I) = SOLI(J)                                                 M36.1341
 5570 I = I - 1                                                         M36.1342
      GO TO 5510                                                        M36.1343
 8900 WRITE ( LUNIT6, 8920 )  NITERA                                    M37.3762
 8920 FORMAT(/, 45H WARNING : THE NUMBER OF LOAD-FLOW ITERATIONS,       M37.3763
     1  8H EXCEEDS,I8,36H, BUT THE SOLUTION IS NOT CONVERGING,          M37.3764
     2 23H TO THE DESIRED RESULT.                                       M37.3765
     3  ,/,46H THE ITERATION PROCESS WILL BE STOPPED AT THIS,           M37.3766
     4 51H POINT TO CONTINUE WITH THE INITIALIZATION PROCESS.)          M37.3767
C     DONE WITH ALL ITERATIONS, REDEFINE VECTOR CREST AND TIME1         M37.3770
 6789 WRITE(LUNIT6, 6788) NEKITE                                        M36.1358
 6788 FORMAT (/, 38H NUMBER OF LOAD FLOW ITERATIONS USED =, I5,         M37.3771
     1           38H. STATUS OF THE SOURCES AFTER THE LAST,             M37.3772
     2           12H ITERATION :)                                       M37.3773
      IF (IPRSUP .GE. 1 .OR. NPRINT .EQ. 1) WRITE(LUNIT6,3952)          M39.1561
 3952 FORMAT ( 18H  ROW  NODE   NAME,  10X, 7HVOLTAGE,                  M39.1562
     1 4X, 7HDEGREES,  8X, 10HREAL POWER  3X, 14HREACTIVE POWER )       M39.1563
      DO 3300 M = 1, NKR                                                M39.1564
      BUS1 = FIXBU1(M)                                                  M39.1565
      N = 1                                                             M39.1566
 1199 IF ( BUS1  .EQ.  BLANK ) GO TO 3300                               M39.1567
      DO 3000 J = 1, NEKCC                                              M36.1360
      IF ( FXTEM1(J) .NE. BUS1 ) GO TO 3000                             M39.1568
      I = KNDEX(J)                                                              
      DO 3737  NFIX = 1, KCONST                                         M36.1362
      IF (IABS(NODE(NFIX)) .EQ. I) GO TO 3939                                   
 3737 CONTINUE                                                          M36.1364
      GO TO 3000                                                        M39.1570
 3939 CREST(NFIX) = FXTEM5(J)                                                   
      TIME1(NFIX) = FXTEM4(J) / PICON                                           
      IF ( IPRSUP .LT. 9  .AND.  NPRINT .NE. 1 ) GO TO 3000             M39.1571
      IF ( N  .GT.  1 ) GO TO 3739                                      M39.1572
      WRITE (LUNIT6, 3500)                                              M39.1573
     1  NFIX, I, BUS(I), CREST(NFIX), FXTEM4(J), FIXB10(M), FIXB11(M)   M39.1574
 3500 FORMAT ( 1X, I5, I6, 1X, A6, E17.6, F11.4, E18.6, E17.6 )         M39.1575
      GO TO 3000                                                        M39.1576
 3739 WRITE ( LUNIT6, 3500 ) NFIX, I, BUS(I), CREST(NFIX), FXTEM4(J)    M39.1577
 3000 CONTINUE                                                          M36.1371
      GO TO ( 2345, 3456, 3300 ), N                                     M39.1578
 2345 BUS1 = FIXBU2(M)                                                  M39.1579
      N = 2                                                             M39.1580
      GO TO 1199                                                        M39.1581
 3456 BUS1 = FIXBU3(M)                                                  M39.1582
      N = 3                                                             M39.1583
      GO TO 1199                                                        M39.1584
 3300 CONTINUE                                                          M39.1585
      IF ( IPRSUP .LE. 0 ) GO TO 9000                                           
      CALL RUNTYM ( CC1, CC2 )                                          M36.1373
      WRITE ( LUNIT6, 8484 )  CC1, CC2                                  M36.1374
      IF ( IPRSUP .LE. 6 )  GO TO 9000                                  M36.1375
      DO 5656 I = 1, KCONST                                             M36.1376
      WRITE (LUNIT6,5755) I, NODE(I),CREST(I),TIME1(I)                  M36.1377
 5755 FORMAT( /, 29H I,NODE(I),CREST(I),TIME1(I) ,2I4,2E18.6)           M36.1378
 5656 CONTINUE                                                          M36.1379
 9000 IF (IPRSUP .GE. 2) WRITE (LUNIT6,9010)                            M37.3782
 9010 FORMAT(31H EXIT FXSOUR = LOAD FLOW MODULE,/)                      M37.3783
      RETURN                                                            M37.3784
      END                                                               M36.1384
      SUBROUTINE SSEQIV( IKF, ISFD, OMEGAL, OMEGAC )                    M32.3365
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION  UR(1), UI(1)                                           M32.3368
      EQUIVALENCE ( VOLTI(1), UR(1) ), ( VOLTK(1), UI(1) )              M32.3369
C     THIS ROUTINE PRODUCES A S.S. EQUIVALENT OF THE BRANCHES.THIS      M32.3370
C     EQUIVALENT IS INSERTED INTO THE TR AND TX TABLES *   *   *   *   *M32.3371
      IDK = 2 * IKF                                                     M32.3372
      IKF = IKF + 1                                                     M32.3373
      IF( IPRSUP .GT. 0 )                                               M32.3374
     1    WRITE( LUNIT6, 1 ) IKF, ISFD, IMFD(IDK+1), IMFD(IDK+2)        M32.3375
    1 FORMAT(41H INTEGER COUNTERS AT START OF SSEQIV.....,10X,7X,3HIKF, M32.3376
     1 6X,4HISFD,6X,4HIZFD,6X,4HIPFD,/,51X,4I10)                        M32.3377
      DO  5  KB = 1, 2                                                  M32.3378
         UI(KB) = 0.                                                    M32.3379
         UR(KB) = 0.                                                    M32.3380
         KC = IDK + KB                                                  M32.3381
         ISF = IMFD(KC)                                                 M32.3382
         IST = ISFD + 1                                                 M32.3383
         ISU = ISFD + ISF * 5                                           M32.3384
         DO  3  KA = IST, ISU, 5                                        M32.3385
C     CALCULATE BRANCH IMPEDANCE   *   *   *   *   *   *   *   *   *   *M32.3386
            AR1 = RMFD(KA)                                              M32.3387
            AL1 = RMFD(KA+1) * OMEGAL                                   M32.3388
            AC1 = RMFD(KA+2) * OMEGAC                                   M32.3389
            ARL = RMFD(KA+3)                                            M32.3390
            AZR = 0.                                                    M32.3391
            AZI = AL1                                                   M32.3392
            IF( AC1 .NE. 0.0 ) AC1 = 1.0 / AC1                          M32.3393
C     PROCESS THE PARALLEL R AND L CONNECTION, FIRST   *   *   *   *   *M32.3394
            IF( ARL .EQ. 0.0 ) GO TO 2                                  M32.3395
            DEN = 1.0 / ( ARL * ARL + AL1 * AL1 )                       M32.3396
            AZR = ARL * ( AL1 * AL1 ) * DEN                             M32.3397
            AZI = AL1 * ( ARL * ARL ) * DEN                             M32.3398
    2       AZR = AZR + AR1                                             M32.3399
            AZI = AZI - AC1                                             M32.3400
C     INVERT IMPEDANCE TO SUM ADMITTANCES  *   *   *   *   *   *   *    M32.3401
            DEN = 1.0 / ( AZR * AZR + AZI * AZI )                       M32.3402
            UR(KB) = UR(KB) + AZR * DEN                                 M32.3403
    3       UI(KB) = UI(KB) - AZI * DEN                                 M32.3404
         IF( IPRSUP .GT. 0 )                                            M32.3405
     1       WRITE( LUNIT6, 4 ) KB, UR(KB), UI(KB)                      M32.3406
    4 FORMAT(41H EQUIVALENT MODAL ADMITTANCE FOR MODE NO.,I6,10X,       M32.3407
     12E22.10)                                                          M32.3408
         ISFD = ISU                                                     M32.3409
C     CONVERT MODAL ADMITTANCE TO IMPEDANCE*   *   *   *   *   *   *   *M32.3410
         DEN = 1.0 / ( UR(KB) * UR(KB) + UI(KB) * UI(KB) )              M32.3411
         UR(KB) = UR(KB) * DEN                                          M32.3412
    5    UI(KB) = -UI(KB) * DEN / OMEGAL                                M32.3413
      RETURN                                                            M32.3414
      END                                                               M32.3415
