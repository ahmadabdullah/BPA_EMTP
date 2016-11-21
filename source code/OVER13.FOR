      SUBROUTINE OVER13
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION CBLHST(1)                                               M44. 476
      EQUIVALENCE  ( CNVHST(1),  CBLHST(1) )                            M44. 477
      DIMENSION INFDLI(1)                                               M44. 768
      EQUIVALENCE  ( NAMEBR(1),  INFDLI(1) )                                    
      DIMENSION  CMR(1), CMI(1), VIM(1)                                 M31.2779
      EQUIVALENCE  ( VOLT(1), VIM(1) )                                  M31.2780
      EQUIVALENCE   ( KKS(1), CMR(1) ),    ( KKNONL(1), CMI(1) )           11077
      DIMENSION  EKREAL(50),EKIMAG(50),EMREAL(50),EMIMAG(50)            M29.1720
      DIMENSION  CLOSEV(50), FARV(50), CLOSEI(50), FARI(50)             M35.1323
      EQUIVALENCE (LSTAT(14), MDRIVE)                                   M35.1324
      DIMENSION  W1(1)                                                  M44. 770
      EQUIVALENCE ( YKM(1), W1(1) )                                     M44. 771
      DIMENSION  WK1(1)                                                 M44. 772
      EQUIVALENCE ( SEMAUX(1), WK1(1) )                                 M44. 773
      REAL*8        TEXT1, TEXT2                                        M15. 928
      COMMON /FDQLCL/ KOFF1,KOFF2,KOFF3,KOFF4,KOFF5,KOFF6,KOFF7,KOFF8,  M44. 478
     1 KOFF9,KOFF10,KOFF13,KOFF14,KOFF15,KOFF16,KOFF17,KOFF18,          M44. 479
     2 KOFF19,KOFF20,KOFF21,KOFF22,KOFF23,KOFF24,KOFF25,INOFF1,INOFF2,  M44. 480
     3 INOFF3,INOFF4,INOFF5,NQTT, LCBL, LMODE, NQTW                     M44. 481
      DATA TEXT1 / 6HPARAME /                                           M15. 929
      DATA TEXT2 / 6HTERS   /                                           M15. 930
      IF ( IPRSUP  .GE.  1 )                                            M32.4289
     1 WRITE ( LUNIT6, 4567 )   ISTEAD, IBR, NTOT                       M32.4290
 4567 FORMAT ( 23H BEGIN MODULE "OVER13".,                              M32.4291
     1         24H  ISTEAD     IBR    NTOT  ,/,  23X, 8I8 )             M32.4292
      JGLNN = 0                                                         M44. 482
      ISECTI = 400
      LL0 = 0                                                           M31.2783
      KCBL = 0                                                          M44.   7
      KMODE=0                                                           M44.   8
      LL1 = 1                                                           M22.4373
      MDRIVE = 0                                                        M32.4293
      MARTI = 0                                                         M35.1325
C     INITIALIZE COUNTERS FOR THE -666 BRANCHES*   *   *   *   *   *   *M32.4294
      IBF = 0                                                           M32.4295
      IKF = 0                                                           M32.4296
      ISFD = 0                                                          M32.4297
      FZERO = 0.0                                                       M31.2784
      IF(ISTEAD.GT.0) GO TO 546                                                 
      DO 540 I=1,IBR                                                            
      IF(KBUS(I).LT.0) GO TO 540                                                
      CI(I)=0.                                                                  
      CIK(I)=0.                                                            11087
      CK(I)=0.                                                             11088
  540 CONTINUE                                                             11089
  541 CALL MOVER0 ( E(1), NTOT )                                                
      CALL MOVER0 ( F(1), NTOT )                                                
      CALL MOVE0 (KODE(1), NTOT)                                        M32.4298
      CALL MOVER0 ( XK(1), LPAST )                                              
      CALL MOVER0 ( XM(1), LPAST )                                              
      CALL MOVER0 ( STAILK(1), LTAILS )                                    11094
      CALL MOVER0 ( STAILM(1), LTAILS )                                    11095
      GO TO 545                                                            11096
  546 L=ILINE+1                                                                 
      K = LPAST - ILINE                                                         
      CALL MOVER0 ( XK(L), K )                                                  
      CALL MOVER0 ( XM(L), K )                                                  
      K=0                                                                       
      ITADD=IT+1                                                                
      MARTI = 1                                                                 
  547 K=K+1                                                                     
      IF (IMODEL(K) .NE. -4) GO TO 4202                                 M44. 484
      JPL = 0                                                           M44. 485
      DO 2023 I = 1, IBR                                                M44. 486
      IF (KODSEM(I) .EQ.0 ) GO TO 2023                                  M44. 487
      KV = INFDLI(INOFF1+I)                                             M44. 488
      IF ( KV .EQ. 0) GO TO 2023                                        M44. 489
      IF ( KV .EQ. 1) GO TO 2024                                        M44. 490
      GO TO 2025                                                        M44. 491
 2024 NR(I) = LPAST + KV                                                M44. 492
      IT2 = LENGTH(I)                                                   M44. 493
      PHS2 = IT2 * IT2                                                  M44. 494
      DO 2022 J = 1, PHS2                                               M44. 495
      JPL = JPL + WK1(KOFF20+KV)                                        M44. 496
      KV = KV + 1                                                       M44. 497
 2022 CONTINUE                                                          M44. 498
      GO TO 2023                                                        M44. 499
 2025 NR(I) = LPAST + 2*JPL + 1                                         M44. 500
 2023 CONTINUE                                                          M44. 501
 4202 CONTINUE                                                          M44. 502
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M22.4374
      IF( IPRSUP  .GE.  1  )                                            M20.3830
     1 WRITE( LUNIT6,110) K,IBR,KBUS(K),NR(K),ISTEAD,KODEBR(K)          M20.3831
  110 FORMAT ( /,  44H IN  'OVER13' ,   NEXT COUPLED BRANCH GROUP.,     M22.4375
     1 30H         K       IBR   KBUS(K),                               M22.4376
     2 30H     NR(K)    ISTEAD KODEBR(K)  ,/,   44X,  6I10  )           M22.4377
      IF(K.GT.IBR) GO TO 521                                                    
      L=KBUS(K)                                                                 
      N15 = IABS(L)                                                             
      IF ( N15  .EQ.  1 )   N15 = IABS( MBUS(K) )                               
      N16 = IABS ( KSSFRQ(N15) )                                                
      OMEGA = TWOPI * SFREQ(N16)                                                
      STEADY = OMEGA * DELTAT                                                   
      IF(L.LT.0) GO TO 575                                                      
      M=IABS(MBUS(K))                                                           
      GO TO 560                                                            11109
  521 IF(INONL.EQ.0) GO TO 545                                                  
      IBADD=IBR                                                            11111
      DO 523 I=1,INONL                                                          
      IF ( NLTYPE(I)  .NE.  -99 )   GO TO 3684                          M22.4378
      IF ( ANONL(I)  .GE.  0.0 )   GO TO 523                            M22.4379
      ANONL(I) = -ANONL(I)                                              M22.4380
      K = NONLK(I)                                                              
      M = IABS( NONLM(I) )                                                      
      D6 = E(K) - E(M)                                                  M22.4383
      N4 = NONLAD(I)                                                    M33.1114
      CURR(I) = 1.0                                                     M22.4385
      IF ( D6  .LT.  0.0 )   CURR(I) = -1.0                             M22.4386
      IF ( ABSZ(D6)  .LE.  VCHAR(I) )   GO TO 3676                      M33.1115
      IF ( NOUTPR  .EQ.  0 )                                            M22.4388
     1 WRITE (LUNIT6, 3671)  BUS(K), BUS(M), D6                         M22.4389
 3671 FORMAT (  23H TYPE-99 ELEMENT FROM ',  A6,  6H' TO ',             M22.4390
     1 A6,  34H' HAS INITIAL CONDITION VOLTAGE OF,  E15.6,              M22.4391
     2  40H   WHICH IS BEYOND SEGMENT-1 BREAKPOINT.  ,/,   70X,         M22.4392
     3 56HTHE USER SHOULD EXPECT SPURIOUS TRANSIENTS AT TIME ZERO. )    M22.4393
 3676 GO TO 523                                                         M22.4394
 3684 J0 = NONLE(I)                                                     M22.4395
      IF(J0.GT.0) GO TO 523                                                11114
      IF ( NLTYPE(I)  .EQ.  -98 )   CURR(I) = 1.0                          11115
      K=NONLK(I)                                                           11116
      M=IABS(NONLM(I))                                                     11117
      IF(VZERO(I).EQ.0.) GO TO 522                                         11118
      IBADD=IBADD+1                                                        11119
      IF( NLTYPE(I) .EQ. -98 )  GO TO 522                                  11120
      CURR(I)=-CI(IBADD)                                                   11121
  522 IPP = K                                                           M31.2791
      IF ( IPP  .EQ.  1 )   IPP = M                                     M31.2792
      IPP = IABS ( KSSFRQ(IPP) )                                        M31.2793
      OMEGA = TWOPI * SFREQ(IPP)                                        M31.2794
      CI1 = ( F(K) - F(M) ) / OMEGA                                     M31.2795
      IF ( IPRSUP  .GE.  2 )                                            M23.2314
     1 WRITE (LUNIT6, 4275)  I, NLTYPE(I), K, M, CI1, OMEGA, F(K), F(M) M23.2315
 4275 FORMAT ( /,  11H FLUX CALC.,                                      M23.2316
     1  32H       I  NLTYPE       K       M,  12X,  3HCI1,              M23.2317
     2  10X,  5HOMEGA,  11X,  4HF(K),  11X,  4HF(M)  ,/,                M23.2318
     3  11X,  4I8,  4E15.6  )                                           M23.2319
      IF ( NLTYPE(I)  .NE.  -96 )   GO TO 7115                          M23.2320
      IF ( CI1  .NE.  0 )   GO TO 710                                   M23.2321
      CI1 = VECNL1(I)                                                   M23.2322
      N11 = NONLAD(I)                                                   M23.2323
      CCHAR(N11+3) = CURR(I)                                            M23.2324
      GO TO 7122                                                        M23.2325
C     CHECK THAT THE INITIAL FLUX-CURRENT POINT LIES WITHIN THE MAJOR   M23.2326
C     HYSTERESIS LOOP.                                                  M23.2327
  710 N10 = ILAST(I)                                                    M23.2328
      N11 = NONLAD(I)                                                   M23.2329
      N11 = CCHAR(N11)                                                  M23.2330
      N12 = NONLAD(I) + 3                                               M23.2331
      CCHAR(N12) = CURR(I)                                              M23.2332
      N12 = N10 + N11                                                   M23.2333
      IF ( CURR(I)  .GT.  CCHAR(N10) )   GO TO 720                      M23.2334
      D12 = VCHAR(N12+1) * CURR(I) + CCHAR(N12+1)                       M23.2335
      GO TO 726                                                         M23.2336
  720 IF ( CURR(I)  .LT.  CCHAR(N12-1) )   GO TO 730                    M23.2337
      N13 = N12 + N11 + 1                                               M25. 349
      D12 = VCHAR(N13) * CURR(I) + CCHAR(N13)                           M23.2339
  726 D13 = D12                                                         M23.2340
      D14 = D12                                                         M23.2341
      GO TO 760                                                         M23.2342
  730 N18 = N10+1                                                       M23.2343
      N19 = N12 - 1                                                     M23.2344
      DO 740  N14=N18,N19                                               M23.2345
      IF ( CURR(I)  .GT.  CCHAR(N14) )   GO TO 740                      M23.2346
      N15 = N14                                                         M23.2347
      GO TO 745                                                         M23.2348
  740 CONTINUE                                                          M23.2349
  745 N16 = N15 + N11 + 1                                               M23.2350
      D13 = VCHAR(N16) * CURR(I) + CCHAR(N16)                           M23.2351
      DO 750  N14=N18, N19                                              M23.2352
      IF ( -CURR(I)  .GT.  CCHAR(N14) )  GO TO 750                      M23.2353
      N15 = N14                                                         M23.2354
      GO TO 755                                                         M23.2355
  750 CONTINUE                                                          M23.2356
  755 N16 = N15 + N11 + 1                                               M23.2357
      D14 = VCHAR(N16) * CURR(I) - CCHAR(N16)                           M23.2358
      IF ( D14+FLZERO  .GE.  CI1            .AND.                       M23.2359
     1     CI1         .GE.  D13-FLZERO )   GO TO 7122                  M23.2360
      D12 = ONEHAF * ( D13 + D14 )                                      M23.2361
  760 IF ( D12  .EQ.  CI1 )   GO TO 7122                                M23.2362
      WRITE (LUNIT6, 773)  I, BUS(K), BUS(M), CI1, CURR(I)              M23.2363
  773 FORMAT ( /,  10X,  35HNOTE  ---- NONLINEAR ELEMENT NUMBER,  I4,   M23.2364
     1 53H  IS A TYPE-96 HYSTERETIC INDUCTOR WHICH IS CONNECTED    ,/,  M23.2365
     2  21X,  17HBETWEEN BUSSES  ',  A6,  9H'  AND  ',  A6,             M23.2366
     3  52H' .   THE INITIAL FLUX-CURRENT POINT AS FOUND BY THE    ,/,  M23.2367
     4  21X, 110HPHASOR STEADY-STATE SOLUTION HAS BEEN OBSERVED TO LIE OM23.2368
     5UTSIDE THE USER-DEFINED MAJOR HYSTERESIS LOOP, HOWEVER.      ,/,  M23.2369
     6  21X,  19HTHE INITIAL FLUX IS,  E14.4,  29H   AND THE INITIAL CURM23.2370
     7RENT IS,  E14.4,  35H .    THE EMTP SHALL NOW ALTER THIS     )    M23.2371
      WRITE (LUNIT6, 776)  D14, D13, D12                                M23.2372
  776 FORMAT ( 21X, 108HJUST-PRINTED FLUX SO AS TO MAKE IT LEGAL, WHILE M23.2373
     1HOLDING THE CURRENT CONSTANT.   THE LINE OF CONSTANT CURRENT  ,/, M23.2374
     2  21X,  105HINTERSECTS THE USER-SUPPLIED MAJOR HYSTERESIS LOOP AT M23.2375
     3TWO POINTS (POSSIBLY EQUAL, IF THE CURRENT IS LARGE          ,/,  M23.2376
     4  21X,  45HENOUGH).   THE  'UPPER'  IS CUT AT FLUX VALUE,         M23.2377
     5  E14.5,  36H ,   AND THE  'LOWER'  AT FLUX VALUE,  E14.5,  2H .  M25. 350
     6  ,/,  21X,  92HTHE INITIAL FLUX SHALL BE TAKEN BY THE EMTP TO BE M23.2379
     7THE AVERAGE OF THESE, WHICH HAS FLUX VALUE,  E15.5,  2H .   )     M23.2380
      CI1 = D12                                                         M23.2381
      IF ( IPRSUP  .GE.  3 )                                            M23.2382
     1 WRITE (LUNIT6, 4288)  N10, N11, N12, N18, N19, NONLE(I),         M23.2383
     2                       CURR(I), CI1, D13, D14, E(K), E(M)         M23.2384
 4288 FORMAT ( /,  17H TYPE-96 PROCESS.,                                M23.2385
     1  40H     N10     N11     N12     N18     N19,  12X,              M23.2386
     2  8HNONLE(I)  ,/,  17X,  5I8,  I10  ,/,                           M23.2387
     3  1X,  13X,  7HCURR(I),  17X,  3HCI1,  17X,  3HD13,  17X,         M23.2388
     4  3HD14,  16X,  4HE(K),  16X,  4HE(M)  ,/,  1X,  6E20.11 )        M23.2389
 7122 N6=NONLAD(I)                                                      M25. 351
      N7 = CCHAR(N6)                                                    M25. 352
      N12=N6+2                                                          M25. 353
      N15=ILAST(I)                                                      M25. 354
      N10=N15+N7-1                                                      M25. 355
      VNONL(I) = CI1                                                    M25. 356
      VCHAR(N6+4) = VNONL(I)                                            M25. 357
      GSLOPE(N6+4) = CURR(I)                                            M25. 358
      CCHAR(N6+4) = -1                                                  M25. 359
      IF ( E(K) .LT. E(M) ) GO TO 920                                   M25. 360
C     AN UPPER TRAJECTORY IS INITIALLY ASSUMED                          M25. 361
      CCHAR(N6+1) = 1                                                   M25. 362
      VCHAR(N6+5) = VCHAR(N10)                                          M25. 363
      GSLOPE(N6+5) = CCHAR(N10)                                         M25. 364
      D6 = 0.0                                                          M25. 365
      IF( ABSZ(VCHAR(N6+5)-VCHAR(N6+4))  .GT. FLZERO)GO TO 1895         M25. 366
      D12 = 0.0                                                         M25. 367
      VCHAR(N6) = 0.0                                                   M25. 368
      VCHAR(N6+1) = 0.0                                                 M25. 369
      CCHAR(N12) = N15 +N7                                              M25. 370
      GO TO 1312                                                        M25. 371
 1895 DO 1900 N11=N15,N10                                               M25. 372
      IF( GSLOPE(N6+4) .GT. CCHAR(N11)) GO TO 1900                      M25. 373
      CCHAR(N12) = N11                                                  M25. 374
      GO TO 1910                                                        M25. 375
 1900 CONTINUE                                                          M25. 376
      CCHAR(N12) = N10 + 1                                              M25. 377
 1910 N13 = CCHAR(N12)+N7+1                                             M25. 378
      D9 = VCHAR(N13)*GSLOPE(N6+4) + CCHAR(N13)                         M25. 379
      D12= VCHAR(N6+4) - D9                                             M25. 380
      GO TO 1700                                                        M25. 381
C     A DOWNER TRAJECTORY IS INITIALLY ASSUMED                          M25. 382
  920 CCHAR(N6+1) = -1                                                  M25. 383
      VCHAR(N6+5) = VCHAR(N15)                                          M25. 384
      GSLOPE(N6+5) = CCHAR(N15)                                         M25. 385
      D6 = 0.0                                                          M25. 386
      IF( ABSZ(VCHAR(N6+5)-VCHAR(N6+4)) .GT. FLZERO) GO TO 1945         M25. 387
      D12 = 0.0                                                         M25. 388
      VCHAR(N6) = 0.0                                                   M25. 389
      VCHAR(N6+1) = 0.0                                                 M25. 390
      CCHAR(N12) = N15 + N7                                             M25. 391
      GO TO 1212                                                        M25. 392
 1945 DO 1950 N11=N15,N10                                               M25. 393
      IF( -GSLOPE(N6+4) .GT. CCHAR(N11)) GO TO 1950                     M25. 394
      CCHAR(N12) = N11                                                  M25. 395
      GO TO 1960                                                        M25. 396
 1950 CONTINUE                                                          M25. 397
      CCHAR(N12) = N10 + 1                                              M25. 398
 1960 N13 = CCHAR(N12)+N7+1                                             M25. 399
      D9 = VCHAR(N13)* GSLOPE(N6+4) - CCHAR(N13)                        M25. 400
      D12 = D9 - VCHAR(N6+4)                                            M25. 401
 1700 VCHAR(N6) = (D12 - D6) / (VCHAR(N6+4) - VCHAR(N6+5))              M25. 402
      VCHAR(N6+1) = D12 - VCHAR(N6)*VCHAR(N6+4)                         M25. 403
      IF (CCHAR(N6+1) .EQ. 1) GO TO 1312                                M25. 404
 1212 N14 = CCHAR(N12)                                                  M25. 405
      N13 = N14 + N7 + 1                                                M25. 406
      D13 = GSLOPE(N14) * (1.0 + VCHAR(N6))                             M25. 407
      D13 = 1.0/D13                                                     M25. 408
      D14 = GSLOPE(N13) - GSLOPE(N14)*VCHAR(N6+1)                       M25. 409
      D14= D14 * D13                                                    M25. 410
      GO TO 1315                                                        M25. 411
 1312 N14 = CCHAR(N12)                                                  M25. 412
      N13 = N14 + N7+1                                                  M25. 413
      D13 = GSLOPE(N14) * (1.0 - VCHAR(N6))                             M25. 414
      D13 = 1.0 / D13                                                   M25. 415
      D14 = GSLOPE(N14) * VCHAR(N6+1) - GSLOPE(N13)                     M25. 416
      D14 = D14 * D13                                                   M25. 417
 1315 GSLOPE(N6+1) = DELTA2 / D13                                       M25. 418
      GSLOPE(N6) = CURR(I) -( E(K) - E(M))* GSLOPE(N6+1)                M25. 419
      VCHAR(N12) = VNONL(I)                                             M25. 420
      VCHAR(N6+3) = CURR(I)                                             M25. 421
      GSLOPE(N6+2) = CCHAR(N6+1)                                        M25. 422
      GSLOPE(N6+3) = CCHAR(N6+2)                                        M25. 423
      N8 = N6 + 5                                                       M25. 424
      IF ( IPRSUP  .GE.  3 )                                            M23.2458
     1 WRITE(LUNIT6, 4295)  N13,  (IPP, CCHAR(IPP), VCHAR(IPP),         M23.2459
     2                      GSLOPE(IPP), IPP=N6, N8 )                   M23.2460
 4295 FORMAT ( /,  19H DONE TYPE-96 INIT.,  5X,  5HN13 =,  I4  ,/,      M23.2461
     1  7X,  3HROW,  15X,  5HCCHAR,  15X,  5HVCHAR,  14X,  6HGSLOPE     M23.2462
     2  ,/,  ( 1X,  I9,  3E20.11 )  )                                   M23.2463
      GO TO 7117                                                        M23.2464
 7115 IF ( ANONL(I)  .EQ.  0.0 )   GO TO 523                            M23.2465
 7117 IF ( NOUTPR  .EQ.  0 )                                            M23.2466
     1 WRITE(LUNIT6,427) BUS(K),BUS(M),CI1                              M20.3834
  427 FORMAT( 23H INITIAL FLUX IN COIL ', A6, 6H' TO ', A6,                11125
     1 3H' =, E13.5 )                                                      11126
      IF ( ABSZ(CI1).GT.ABSZ(ANONL(I))     .AND.                        M20.3835
     1     NOUTPR  .EQ.  0 )                                            M20.3836
     2 WRITE(LUNIT6,426)                                                M20.3837
  426 FORMAT ( 132H WARNING.  ASSUMPTION THAT AC STEADY STATE HAS FUNDAM   11128
     1ENTAL FREQUENCY ONLY IS QUESTIONABLE WITH PRECEDING FLUX OUTSIDE     11129
     2LINEAR REGION)                                                       11130
      VNONL(I)=CI1-(E(K)-E(M))*DELTA2                                      11131
      IF( NLTYPE(I) .NE. -98 )  GO TO 523                                  11132
      IF ( VNONL(I)  .LT.  0.0 )   CURR(I) = -1.0                          11133
  523 CONTINUE                                                             11134
  545 ISTEAD=0                                                                  
      IPRINT=-1                                                                 
      ITADD = IT + 1                                                    M29.1721
C                                                                       M44. 774
C     READ INPUT CARD USING CIMAGE.                                        11137
C                                                                       M44. 775
  544 CALL CIMAGE                                                               
 5758 IF ( KOLBEG  .GT.  0 )   GO TO 4623                               M22.4396
      DECODE (80,7011,ABUFF) IVOLT                                              
      GO TO 4625                                                                
 4623 NFRFLD = 1                                                        M22.4398
      CALL FREONE ( D11 )                                               M29.1722
      IVOLT = D11                                                       M22.4400
 4625 IF ( IVOLT  .LE.  1 )GO TO 590                                            
      MDRIVE = 1                                                        M32.4299
      IF(IVOLT.EQ.2) GO TO 550                                                  
      IPRINT=1                                                             11142
      IF(IVOLT.EQ.3) GO TO 553                                                  
      IF(IVOLT.EQ.4) GO TO   570                                                
      IF (IVOLT .EQ. 5) GO TO 5600                                              
      KILL = 30                                                            11145
      LSTAT(19) = 550                                                      11146
      LSTAT(16) = IVOLT                                                    11147
      GO TO 9200                                                           11148
  550 IF ( KOLBEG  .GT.  0 ) GO TO 4633                                         
      DECODE( 80, 7011, ABUFF ) LL, BUS1, A, CI1, CK1, K                M23.2467
 7011 FORMAT( I2, A6, 3E15.8, 21X, I6 )                                         
      GO TO 4635                                                                
 4633 K = IVOLT                                                         M22.4404
      NRIGHT = -1                                                       M22.4405
      NFRFLD = 1                                                        M22.4406
      CALL FREONE ( D1 )                                                M29.1723
      BUS1 = TEXTA6(1)                                                  M22.4408
      NRIGHT = 0                                                        M22.4409
      CALL FREONE ( A )                                                 M29.1724
      CALL FREONE ( CI1 )                                               M29.1725
      CALL FREONE ( CK1 )                                               M29.1726
 4635 IF ( IPRINT  .EQ.  0 )   GO TO 54157                              M22.4413
      IF ( IPRINT  .EQ.  0 )                                            M44. 776
     1WRITE ( * ,* ) 'OVER13-CABLE.    CK1, TPI=',                      M44. 777
     2                                 CK1, TPI                         M44. 778
      WINIC = CK1 * TPI                                                         
      IPRINT=0                                                             11153
      IF ( NOUTPR  .EQ.  0 )                                            M20.3839
     1 WRITE (KUNIT6, 54153)  A, CI1, CK1                               M35.1327
54153 FORMAT ( 21H+NODE VOLT INIT COND.  , 2E11.3,  F7.1 )              M35.1328
      STEADY = CK1 * TWOPI * DELTAT                                             
      GO TO 4463                                                        M35.1329
54157 IF ( NOUTPR  .EQ.  0 )                                            M20.3841
     1 WRITE (KUNIT6, 54158)  A, CI1                                    M35.1330
54158 FORMAT ( 21H+NODE VOLT INIT COND.  , 2E11.3  )                    M11. 260
 4463 IF( K .GT. 0 ) GO TO 7005                                         M35.1331
      DO 551 K=2,NTOT                                                      11159
      IF(BUS(K).EQ.BUS1) GO TO 552                                         11160
  551 CONTINUE                                                             11161
      KILL = 31                                                            11162
      LSTAT(19) = 551                                                      11163
      GO TO 9200                                                           11164
 7005 IF( BUS(K) .EQ. BUS1 ) GO TO 552                                          
      KILL = 31                                                         M23.2471
      LSTAT(19) = 7005                                                  M23.2472
      GO TO 9200                                                        M23.2473
  552 E(K)=A                                                                    
      F(K)=CI1                                                                  
      GO TO 544                                                                 
  553 IF ( KOLBEG  .GT.  0 ) GO TO 4643                                         
      DECODE ( 80, 7020, ABUFF ) LL, BUS1, BUS2, CI1, CK1, A, D2, K     M23.2474
 7020 FORMAT( I2, 2A6, 4E15.8, I6 )                                     M23.2475
      GO TO 4645                                                        M22.4415
 4643 LL = IVOLT                                                                
      NRIGHT = -1                                                       M22.4417
      NFRFLD = 2                                                        M22.4418
      CALL FREONE ( D1 )                                                M29.1727
      BUS1 = TEXTA6(1)                                                  M22.4420
      BUS2 = TEXTA6(2)                                                  M22.4421
      NRIGHT = 0                                                        M22.4422
      NFRFLD = 1                                                        M22.4423
      CALL FREONE ( CI1 )                                               M29.1728
      CALL FREONE ( CK1 )                                               M29.1729
      CALL FREONE ( A )                                                 M29.1730
      CALL FREONE ( D2 )                                                M29.1731
      CALL FREONE ( D7 )                                                M29.1732
      K = D7                                                            M23.2478
 4645 IF ( NOUTPR  .EQ.  0 )                                            M22.4428
     1 WRITE (KUNIT6, 54159)  CI1, CK1, A                               M35.1332
54159 FORMAT ( 10H+LINEAR I., 4X, 3E12.4  )                                11172
      IF( K .GT. 0 ) GO TO 7000                                         M23.2479
      DO 554 K=1,IBR                                                            
      L=KBUS(K)                                                                 
      LL=IABS(L)                                                                
      M=IABS(MBUS(K))                                                           
      IF(BUS(LL).NE.BUS1) GO TO 554                                             
      IF(BUS(M).NE.BUS2) GO TO 554                                              
      IF(L.LT.0) GO TO 575                                                      
      GO TO 560                                                                 
  554 CONTINUE                                                             11181
      KILL = 32                                                            11182
      LSTAT(19) = 554                                                      11183
      GO TO 9200                                                           11184
 7000 L = KBUS(K)                                                       M23.2480
      LL = IABS(L)                                                      M23.2481
      M = IABS( MBUS(K) )                                               M23.2482
      IF( BUS(LL) .NE. BUS1 .OR. BUS(M) .NE. BUS2 ) GO TO 7001          M23.2483
      IF( L .LT. 0 ) GO TO 575                                                  
      GO TO 560                                                         M23.2485
 7001 KILL = 32                                                         M23.2486
      LSTAT(19) = 7000                                                  M23.2487
      GO TO 9200                                                        M23.2488
  560 N3=NR(K)                                                                  
      IF(N3.GT.0) GO TO 565                                                11186
      IF(ISTEAD.GT.0) GO TO 561                                            11187
      CI(K)=-CI1                                                           11188
      CK(K)=CK1                                                            11189
  561 N3=-N3                                                                    
      CIK(K)=-CI(K)-X(N3)*(E(L)-E(M))                                      11191
      IF(ISTEAD.GT.0) GO TO 547                                                 
      GO TO 544                                                            11193
  565 IT2 = IABS(LENGTH(K))                                                11194
      IF( ISTEAD .GT. 0 ) GO TO 568                                        11195
      CIK(K) = CI1                                                         11196
      CI(K) = CK1                                                          11197
      CK(K) = A                                                            11198
  568 CI1 = E(L)                                                           11199
      CK1 = E(M)                                                           11200
      VOLTI(1)=-CI1 / 2.0                                               M05. 499
      VOLTK(1)=-CK1/ 2.0                                                M05. 500
      VOLT(1)=CK1-CI1                                                      11203
      IF( LENGTH(K+1) .EQ. -666 ) VOLTK(1) = F(M) - F(L)                M32.4300
      N1=K+IT2-1                                                           11204
      IF(IT2.EQ.1) GO TO 567                                               11205
      IT1=K+1                                                              11206
      IF( IPRSUP  .GE.  1  )                                            M20.3845
     1 WRITE( LUNIT6, 111 ) CI1, CK1, IT1, N1, N3                       M20.3846
 111  FORMAT(17X,3HCI1,17X,3HCK1,2X,3HIT1,3X,2HN1,3X,2HN3,/,            M20.3847
     12(2X,E18.9),3I5)                                                  M20.3848
      DO 566 I=IT1,N1                                                      11207
      L=KBUS(I)                                                            11208
      M=IABS(MBUS(I))                                                      11209
      IF(ISTEAD.GT.0) GO TO 569                                            11210
C     READ INPUT CARD USING CIMAGE.                                        11211
      CALL CIMAGE                                                          11212
      IF ( KOLBEG  .GT.  0 )   GO TO 4653                               M22.4429
      DECODE(80, 7020, ABUFF)  N2, BUS1, BUS2, CIK(I), CI(I), CK(I)        11213
      GO TO 4655                                                        M22.4430
 4653 NFRFLD = 1                                                        M22.4431
      CALL FREONE ( D11 )                                               M29.1733
      N2 = D11                                                          M22.4433
      NRIGHT = -1                                                       M22.4434
      CALL FREONE ( D1 )                                                M29.1734
      BUS1 = TEXTA6(1)                                                  M22.4436
      CALL FREONE ( D1 )                                                M29.1735
      BUS2 = TEXTA6(1)                                                  M22.4438
      NRIGHT = 0                                                        M22.4439
      CALL FREFLD ( CIK(I) )                                            M22.4440
      CALL FREFLD ( CI(I) )                                             M22.4441
      CALL FREFLD ( CK(I) )                                             M22.4442
 4655 IF ( NOUTPR  .EQ.  0 )                                            M22.4443
     1 WRITE (KUNIT6, 54159)  CIK(I), CI(I), CK(I)                      M35.1333
      IF( BUS(L) .EQ. BUS1   .AND.   BUS(M) .EQ. BUS2 )  GO TO 569         11215
      KILL = 32                                                         M22.4444
      LSTAT(19) = 569                                                   M23.2489
      GO TO 9200                                                        M22.4445
  569 CI1=E(L)                                                             11219
      CK1=E(M)                                                             11220
      NKP = L                                                           M32.4301
      L=I-K+1                                                              11221
      VOLTI(L)=-CI1 / 2.0                                               M05. 501
      VOLTK(L)=-CK1 / 2.0                                               M05. 502
      VOLT(L)=CK1-CI1                                                      11224
      IF( LENGTH(K+1) .EQ. -666 ) VOLTK(L) = F(M) - F(NKP)              M32.4302
  566 CONTINUE                                                             11225
      IF( KODEBR(K) .LE. 0 )  GO TO 567                                    11226
      CALL MULT( X(N3), VOLT(1), CIK(K), IT2, LL1 )                     M22.4446
      L = K                                                                11228
      DO 38568  I=1, IT2                                                   11229
      IF( IPRSUP .GE. 3 )                                                  11230
     1 WRITE(LUNIT6, 38567)  I, VOLT(I), CIK(L)                         M17. 703
38567 FORMAT( /,  33H AT 38567, INIT  I(T-DELTAT) ....   , I10, 2E25.15)   11232
38568 L = L + 1                                                            11233
      GO TO 28567                                                          11234
  567 CALL MULT(TX(N3),VOLT,CIK(K),IT2,LL1)                             M22.4447
      IF( IPRSUP  .GE.  1  )                                            M20.3851
     1 WRITE( LUNIT6, 111 ) CI1, CK1, IT1, N1, N3                       M20.3852
      IF( LENGTH(K+1) .NE. -666 ) GO TO 28567                           M32.4303
C     PROCESS NEXT SET OF MODAL BRANCHES   *   *   *   *   *   *   *   *M32.4304
      OMG = STEADY / DELTAT                                             M32.4305
      CALL FDINT( IKF, ISFD, IBF, OMG )                                 M32.4306
28567 CONTINUE                                                             11236
      CALL MULT( C(N3),VOLTI,CI(K),IT2,LL1)                             M22.4448
      CALL MULT( C(N3),VOLTK,CK(K),IT2,LL1)                             M22.4449
      K = N1                                                               11239
      IF(ISTEAD.GT.0) GO TO 547                                            11240
      GO TO 544                                                            11241
  570 IF ( KOLBEG  .GT.  0 )   GO TO 4663                               M22.4450
      DECODE (80, 7020, ABUFF)  K, BUS1, BUS2, CI1, CK1                    11243
      GO TO 4665                                                        M22.4451
 4663 K = IVOLT                                                         M22.4452
      NRIGHT = -1                                                       M22.4453
      NFRFLD = 2                                                        M22.4454
      CALL FREONE ( D1 )                                                M29.1736
      BUS1 = TEXTA6(1)                                                  M22.4456
      BUS2 = TEXTA6(2)                                                  M22.4457
      NRIGHT = 0                                                        M22.4458
      NFRFLD = 1                                                        M22.4459
      CALL FREONE ( CI1 )                                               M29.1737
      CALL FREONE ( CK1 )                                               M29.1738
 4665 IF ( NOUTPR  .EQ.  0 )                                            M22.4462
     1 WRITE (KUNIT6, 54162)  CI1, CK1                                  M35.1334
54162 FORMAT ( 26H+NONLIN. BRANCH INIT COND., 2X, 2E11.3 )                 11245
      DO 571   K=1,INONL                                                   11246
      L=NONLK(K)                                                           11247
      M=IABS(NONLM(K))                                                     11248
      IF ( BUS(L)  .NE.  BUS1 )   GO TO 571                                11249
      IF ( BUS(M)  .EQ.  BUS2 )   GO TO 572                                11250
  571 CONTINUE                                                             11251
      KILL = 33                                                            11252
      LSTAT(19) = 571                                                      11253
      GO TO 9200                                                           11254
  572 N1=NONLAD(K)                                                         11255
      N2=NONLE(K)                                                          11256
      IF(N2.GT.0) GO TO 544                                                11257
      N2=-N2                                                               11258
      IF ( NLTYPE(K)  .NE.  -96 )  GO TO 7361                           M23.2490
      VNONL(K) = CI1 - (E(L) -E(M) )*DELTA2                             M23.2491
      N17 = NONLAD(K)                                                   M23.2492
      N18 = N17 + 1                                                     M23.2493
      N19 = N17 + 2                                                     M23.2494
      N20 = N17 + 3                                                     M23.2495
      CCHAR(N20) = CK1                                                  M23.2496
      CURR(K) = CCHAR(N20)                                              M23.2497
C     READ INPUT CARD USING CIMAGE.                                     M25. 425
      CALL CIMAGE                                                       M23.2499
      DECODE ( 80, 7018, ABUFF(1) ) N15, N16,                           M23.2500
     1                              ( VCHAR(IPP), IPP=N17, N20 )        M23.2501
 7018 FORMAT ( 2I10, 4E15.8)                                            M23.2502
      CCHAR(N18) = N15                                                  M23.2503
      CCHAR(N19) = N16                                                  M23.2504
      IF ( NOUTPR  .EQ.  0 )                                            M28.4568
     1 WRITE (KUNIT6, 7017)  N15, N16, VCHAR(N17), VCHAR(N17+1)         M35.1335
 7017 FORMAT ( 11H+  TYPE-96., 2I4, 2E15.6 )                            M23.2506
      GSLOPE(N19) = CCHAR(N18)                                          M23.2507
      GSLOPE(N20) = CCHAR(N19)                                          M23.2508
      N16 = CCHAR(N19)                                                  M23.2509
      N15 = N16 + CCHAR(N17) + 1                                        M23.2510
      IF ( CCHAR(N18)  .NE. 1 )  GO TO 4667                             M23.2511
      D13 = GSLOPE(N16) * (1.0 - VCHAR(N17) )                           M23.2512
      D13 = 1.0 / D13                                                   M23.2513
      D14 = GSLOPE(N16) * VCHAR(N18) - GSLOPE(N15)                      M23.2514
      D14 = D14 * D13                                                   M23.2515
      GO TO 4668                                                        M23.2516
 4667 D13 = GSLOPE(N16) * ( 1.0 + VCHAR(N17) )                          M23.2517
      D13 = 1.0 / D13                                                   M23.2518
      D14 = GSLOPE(N15) - GSLOPE(N16) * VCHAR(N18)                      M23.2519
      D14 = D14 * D13                                                   M23.2520
 4668 GSLOPE(N18) = DELTA2 / D13                                        M23.2521
      GSLOPE(N17) = VCHAR(N20)  -  GSLOPE(N18) * ( E(L) - E(M) )        M23.2522
      N18 = N17 + 4                                                     M25. 426
      N19 = N17 + 5                                                     M25. 427
C READ INPUT CARD USING CIMAGE.                                         M25. 428
      CALL CIMAGE                                                       M25. 429
      DECODE ( 80, 7026, ABUFF(1) )  N20,VCHAR(N18),                    M25. 430
     1                               VCHAR(N19),GSLOPE(N18),GSLOPE(N19) M25. 431
 7026 FORMAT ( I10 , 4E15.8 )                                           M25. 432
      CCHAR(N18) = N20                                                  M25. 433
      IF ( NOUTPR  .EQ.  0 )                                            M28.4570
     1 WRITE (KUNIT6, 7025)  N20, VCHAR(N18), VCHAR(N19)                M35.1336
 7025 FORMAT ( 11H+  TYPE-96., I8, 2E14.6 )                             M28.4572
      GO TO 544                                                         M23.2523
 7361 DO 573 I=N1, N2                                                   M23.2524
      IF(CCHAR(I).GT.CI1) GO TO 574                                        11260
  573 CONTINUE                                                             11261
      KILL = 34                                                            11262
      LSTAT(19) = 573                                                      11263
      FLSTAT(16) = CI1                                                     11264
      FLSTAT(15) = CCHAR(N2)                                               11265
      LSTAT(14) = N2                                                       11266
      LSTAT(13) = K                                                        11267
      GO TO 9200                                                           11268
  574 N1=I-1                                                               11269
      VR1=VCHAR(N1)                                                        11270
      CURR1=CCHAR(N1)                                                      11271
      CURR(K)=CI1                                                          11272
3      CI1=VR1+(VCHAR(I)-VR1)*(CI1-CURR1)/(CCHAR(I)-CURR1)              M44. 780
      IF(CK1.NE.0.) CI1=CK1                                                11274
      VNONL(K)=CI1-(E(L)-E(M))*DELTA2                                      11275
      GO TO 544                                                            11276
C 3456789012345678901234567890123456789012345678901234567890123456789012M44. 781
  575 IF ( IMODEL(K) .NE. -3 ) GO TO 3300                                       
C                                                                       M44. 783
C                                                                       M44. 784
 3300 N1 = -KBUS(K)                                                             
      N2=IABS(MBUS(K))                                                          
      IF (IMODEL(K) .EQ. -4) GO TO 7550                                         
C      IF(KODSEM(K) .NE. 0                                              M31.2796
C     1 .AND. IMODEL(K) .NE. -2) GO TO 5750      !for Semlyen or cas. PIM44. 787
      IF(KODSEM(K) .NE. 0                                               M44. 788
     1 .AND. IMODEL(K) .GE. 0) GO TO 5750                                       
7550  N4=LENGTH(K)                                                              
      IF ( N4 .GT. 0 )   N4 = 1                                                 
      ISD = ITADD                                                               
      IF (IMODEL(K)  .EQ. -2)  GO TO 60001                                      
      IF (IMODEL(K)  .EQ. -4)  GO TO 60006                              M44. 790
      IF(ISTEAD.EQ.0) GO TO 578                                                 
      CURR1=TR(ITADD)                                                           
      CURI1=TX(ITADD)                                                      11282
      CURR2=R(ITADD)                                                            
      CURI2=C(ITADD)                                                       11284
      ITADD=ITADD+1                                                             
      GO TO 579                                                            11286
CL                                                                      M31.2798
C     INITIALIZATION OF HISTORY VECTORS FOR MARTI'S  F.D. LINES         M31.2799
C     NON-ZERO INITIAL CONDITIONS                                       M31.2800
C                                                                       M44. 791
C                                                                       M44. 792
60006 IT2=IABS(N4)                                                              
      APH = IT2                                                                 
30006 NL = LITYPE(K)                                                            
      IF(ISTEAD.EQ.0) GO TO 60005                                               
      N5 = NL - 1                                                               
      IY = K                                                                    
      IZ = ITADD                                                        M44. 799
C                                                                       M44. 800
C                                       CALL INCDRV ( K, NPHS, DELTAT, OM44. 801
C                                                                       M44. 802
      N7 = IY                                                                   
      N8 = IZ                                                                   
      STEADY = OMEGA * DELTAT                                           M44. 805
      IF ( IPRSUP .GT. 0 )                                              M44. 806
     1 WRITE(LUNIT6,*) 'steady = omega * deltat', omega, deltat, steady      
      WDT=-DELTAT*OMEGA                                                 M44. 808
C                                                                       M44. 809
C                               Evaluate: vm(to), vm(to-dt), vk(to), vk(M44. 810
C                                                                       M44. 811
      IF ( ISTEAD .EQ. 0 )  OMEGS1 = 0.0                                        
      N1 = KODSEM(K)                                                            
      NRZ = CKI(K)                                                              
      NRA = CKKJM(K)                                                            
      IF ( IPRSUP .GT. 0 )                                              M44. 816
     1 WRITE(LUNIT6,8833) N1,NRA,NRZ                                         
8833  FORMAT(1X,I6,1X,I6,1X,I6,30H I       VKDT(I)       VMDT(I),       M44. 818
     1                                                  6HNN5 N6)       M44. 819
      IVBR = K                                                                  
      DO 1003 I=1, IT2                                                          
      NS1 = KODSEM(IVBR)                                                        
      NRZ = CKI(IVBR)                                                           
      NRA = CKKJM(IVBR)                                                         
      N1 = -KBUS(IVBR)                                                          
      N2 = IABS ( MBUS(IVBR) )                                                  
      NK1 = NS1 + 5*NRA + 5*NRZ + 4 + 1                                 M44. 503
      NN17 = SCONST(NK1)                                                M44. 504
      NN5 = NS1 + 5*NRZ + 5*NRA + 4 + 1 +1 +NN17                                
      PHF = ATAN2Z( F(N2),E(N2) )                                               
      HF = DSQRT( E(N2)**2 + F(N2)**2 )                                         
      SCONST(NN5) = HF * DCOS( WDT + PHF )                                      
      N6 = NN5 + 1                                                              
      PHF = ATAN2Z( F(N1),E(N1) )                                               
      HF = DSQRT( E(N1)**2 + F(N1)**2 )                                         
      SCONST(N6) = HF * DCOS( WDT + PHF )                                       
      IF ( IPRSUP .GT. 0 )                                              M44. 837
     1 WRITE(LUNIT6,5333) I,SCONST(NN5),SCONST(N6),NN5,N6                    
5333  FORMAT (9X, I5,1X,2E14.5,1X,1X,I6,1X,I6 )                         M44. 839
      IVBR = IVBR + 1                                                   M44. 840
1003  CONTINUE                                                          M44. 841
C        Single phase and constant Q                                    M44. 842
C        Evaluate Im' = Q**-1 * Im                                      M44. 843
C                 Ik' = Q**-1 * Ik                                      M44. 844
C 125   CALL QINV(QR,QI,QK0,W,NPHS,IADRSQ,NTERMQ) !Q MATRIX AND CONVERTSM44. 845
C                                                                       M44. 846
      IOFF1 = 0                                                                 
      IOFF2 = IOFF1 + ISECTI                                                    
      IOFF3 = IOFF2 + ISECTI                                                    
      IOFF4 = IOFF3 + ISECTI                                                    
      IOFF5 = IOFF4 + ISECTI                                                    
      IOFF6 = IOFF5 + ISECTI                                                    
      IOFF7 = IOFF6 + ISECTI                                                    
      IOFF8 = IOFF7 + ISECTI                                                    
      IF ( IOFF8 .LE. LYMAT ) GO TO 5337    
      WRITE (LUNIT6,5338)  LYMAT, IOFF8
 5338 FORMAT( ' IN OVER13, NEW MARTI LINE SOLUTION LOGIC OVERFLOWED STO'
     1, 'RAGE LIST NO. 5: LYMAT =', I5, ' NEEDED SPACE HERE IOFF8=',I5,
     2 '.',/, ' EXECUTION IS ABORTED, AND REDIMENSION WITH LARGER VALUE
     3OF LIST NO. 5 IS REQUIRED.' )
      STOP
 5337 KOFF25 = KOFF24 + 288                                             M44. 505
      KOFF11 = KOFF25 + 288                                                     
      IF ( KOFF11 .LT. LHIST) GO TO 5339
      WRITE (LUNIT6,5340)  LHIST, KOFF11
 5340 FORMAT( ' IN OVER13, NEW MARTI LINE SOLUTION LOGIC OVERFLOWED STO'
     1, 'RAGE LIST NO. 22: LHIST =', I5,' NEEDED SPACE HERE KOFF11=',I5,
     2 '.',/, ' EXECUTION IS ABORTED, AND REDIMENSION WITH LARGER VALUE
     3OF LIST NO. 22 IS REQUIRED.'  )
      STOP


 5339 NQ1 = INFDLI(INOFF2+K)                                                    
      NPHS2=IT2*IT2                                                     M44. 901
      KQVQ = INFDLI(INOFF1+K)                                                   
      KQV = LITYPE(K)                                                   M44. 506
      DO 609 IQ = 1, NPHS2                                                      
      NTEQ = WK1(KOFF20+KQVQ)                                                   
      QFD(KQV)=SCONST(NQ1)                                                      
      SFD(KQV)=0.D0                                                     M44. 908
      IF ( IPRSUP  .GE.  2 )                                            M44. 909
     1 WRITE(LUNIT6,5692)                                               M44. 910
5692  FORMAT ( 1X, 43HkQV NTEQ    NQ1    NQ2    NQ3   SCONST(NQ1),      M44. 911
     1      42H   SCONST(NQ2)   SCONST(NQ3)       QR(KQV))              M44. 912
      DO 801 LQ=1,NTEQ                                                          
      NQ2 = NQ1 + LQ                                                            
      NQ3 = NQ2 + NTEQ                                                          
      IF ( ABSZ( SCONST(NQ2) ) .GE. 1.E+13 ) GO TO 3000                 M44. 507
      D = OMEGA ** 2 + SCONST(NQ3) ** 2                                         
      QFD(KQV) = QFD(KQV) + SCONST(NQ2) * SCONST(NQ3) / D                       
      SFD(KQV) = SFD(KQV) - SCONST(NQ2) * OMEGA / D                             
      GO TO 3001                                                        M44. 508
 3000 JKL = JKL + 1                                                     M44. 509
      IF ( JKL .EQ. 2 ) GO TO 3002                                      M44. 510
      AKR = SCONST(NQ2) / 1.E+15                                        M44. 511
      AKI = SCONST(NQ2+1) / 1.E+15                                      M44. 512
      APR = SCONST(NQ3)   / 1.E+15                                      M44. 513
      API = SCONST(NQ3+1) / 1.E+15                                              
      DQQ3 = APR ** 2 + ( OMEGA + API ) ** 2                            M44. 515
      DDD3 = ( AKR * APR + AkI  * (OMEGA + API ) ) / DQQ3               M44. 516
      DDD4 = ( AKI * APR - AKR * ( OMEGA + API ) ) / DQQ3               M44. 517
      XK(KQV) = XK(KQV) + 2*DDD3                                        M44. 518
      IF ( JKL .EQ. 1 ) GO TO 3001                                      M44. 519
 3002 JKL = 0                                                           M44. 520
 3001 IF ( IPRSUP  .GE.  2 )                                            M44. 919
     1 WRITE(LUNIT6,1333) kQV,NTEQ,NQ1,NQ2,NQ3,SCONST(NQ1),                  
     2             SCONST(NQ2),SCONST(NQ3),QFD(KQV)                     M44. 921
1333  FORMAT ( 1X,I6,3X,I6,2X,I5,2X,I5,2X,I5, 4E14.5)                   M44. 922
      NQ2 = NQ2 + 1                                                     M44. 923
      NQ3 = NQ3 + 1                                                     M44. 924
801   CONTINUE                                                          M44. 925
      NQ1 = NQ1 + 2 * NTEQ + 1                                          M44. 926
      KQV = KQV + 1                                                     M44. 927
      KQVQ = KQVQ + 1                                                   M44. 521
609   CONTINUE                                                          M44. 928
      J1 = LITYPE(K)                                                    M44. 522
      JADD = J - 1 + NPHS2                                              M44. 523
      IF ( IPRSUP  .GE.  2 )                                            M44. 929
     1 WRITE (LUNIT6,*) 'Following are QR(k) by rows____________'            
      IF ( IPRSUP  .GE.  2 )                                            M44. 931
     1 WRITE (LUNIT6,4515)   ( QFD(J), J=J1, JADD )                          
      IF ( IPRSUP  .GE.  2 )                                            M44. 933
     1 WRITE (LUNIT6,*) 'Following are QI(k) by rows____________'            
      IF ( IPRSUP  .GE.  2 )                                            M44. 935
     1 WRITE (LUNIT6,4516)   ( SFD(J), J=J1, JADD )                         
 4515 FORMAT ( 7H+ Qr-I:, 3E14.5 )                                      M44. 937
 4516 FORMAT ( 7H+ Qi-I:, 3E14.5 )                                      M44. 938
      ICBR = ITADD                                                              
      DO 821 I=1, IT2                                                           
      R(ICBR) = -R(ICBR)                                                        
      C(ICBR) = -C(ICBR)                                                        
      IF ( IPRCBL .GT. 0 )                                              M44. 943
     1 WRITE(LUNIT6,*) '  AIMR(I)=-AIMR(I) , AIMI(I)=-AIMI(I) ',             
     2               R(ICBR), C(ICBR)                                   M44. 945
      ICBR = ICBR + 1                                                           
821   CONTINUE                                                          M44. 947
C                                                                       M44. 948
C        Invert matrix Q                                                M44. 949
C                                                                       M44. 950
C     KQ = IADRSU(K)                                                    M44. 951
C      KQ = INFDLI(INOFF1+K)                                            M44. 952
      KQ = LITYPE(K)                                                    M44. 524
C      CALL CINVL(QFD(KQ),SFD(KQ),IT2)                            !inverM44. 954
      LL=1                                                              M44. 956
      NPHS2 = IT2 * IT2                                                 M44. 957
CCCC   WSM & THL  comment out following DO loop on 14 Sept 88.   It             
CCCC              seems to us that initialization is not required.              
C     DO 7003 IFD = 1, 72                                               M44. 525
C     WK1(KOFF24+LL) = 0.0                                              M44. 959
C     WK1(KOFF24+LL+1)=0.0                                              M44. 960
C     LL=LL+2                              !skip two celles for next freM44. 961
C7003 CONTINUE                                                          M44. 962
      LL=1                                                              M44. 964
      NPHS2 = IT2 * IT2                                                 M44. 965
      DO 7002 IFD = 1, NPHS2                                            M44. 966
      WK1(KOFF24+LL) = QFD(KQ)                                                  
      WK1(KOFF24+LL+1)=SFD(KQ)                                          M44. 968
      LL=LL+2                                                                   
      KQ = KQ + 1                                                       M44. 970
 7002 CONTINUE                                                          M44. 971
      CALL COMINV(WK1(KOFF24+1),                                        M44. 973
     1               WK1(KOFF25+1),IT2, 60.)                                    
      T = 0.0000000                                                     M44. 975
      KQ = INFDLI(INOFF1+K)                                                     
      KQ = LITYPE(K)                                                    M44. 526
      LL=1                                                              M44. 978
      DO 7004 IFD = 1, NPHS2                                            M44. 979
      QFD(KQ)=WK1(KOFF25+LL)                                                    
      SFD(KQ)=WK1(KOFF25+LL+1)                                                  
      LL=LL+2                                                                   
      KQ = KQ + 1                                                       M44. 983
 7004 CONTINUE                                                          M44. 984
      KMV = K                                                           M44. 986
      ITAM = ITADD                                                      M44. 987
C      KQ = INFDLI(INOFF1+K)                                            M44. 988
      KQ = LITYPE(K)                                                    M44. 527
C     CALL MVECC(QFD(KQ),SFD(KQ),R(ITADD),C(ITADD),IT2,                 M44. 989
C    1    W1( IOFF1 + K ), W1( IOFF2 + K ) )         !Modal current Im'=M44. 990
      IF ( IPRSUP .GT. 0 )                                              M44. 991
     1 WRITE(LUNIT6,4933)                                                    
4933  FORMAT(14H  I    J   IT2                                          M44. 993
     133H    QFD(KQ)    SFD(KQ)   R(ITADD),                             M44. 994
     233H   C(ITADD)W1(IOFF1+K)W1(IOFF2+K),                             M44. 995
     333H  TR(ITADD)  TX(ITADD)W1(IOFF7+K),                             M44. 996
     411HW1(IOFF8+K))                                                   M44. 997
      DO 1070 IM = 1, IT2                                               M44. 998
      W1(IOFF1+KMV-1+IM) = 0.D0                                         M44. 999
      W1(IOFF2+KMV-1+IM) = 0.D0                                         M44.1000
      W1(IOFF7+KMV-1+IM) = 0.D0                                         M44.1001
      W1(IOFF8+KMV-1+IM) = 0.D0                                         M44.1002
1070  CONTINUE                                                          M44.1003
      DO 1071 JM = 1, IT2                                               M44.1004
      DO 1072 IM = 1, IT2                                               M44.1005
      W1(IOFF1+KMV-1+IM) = W1(IOFF1+KMV-1+IM)                                   
     1                    + QFD(KQ) * R(ITAM)                           M44.1007
     2                    - SFD(KQ) * C(ITAM)                           M44.1008
      W1(IOFF2+KMV-1+IM) = W1(IOFF2+KMV-1+IM)                                   
     1                    + QFD(KQ) * C(ITAM)                           M44.1010
     2                    + SFD(KQ) * R(ITAM)                           M44.1011
      W1(IOFF7+KMV-1+IM) = W1(IOFF7+KMV-1+IM)                                   
     1                    + QFD(KQ) * TR(ITAM)                          M44.1014
     2                    - SFD(KQ) * TX(ITAM)                          M44.1015
      W1(IOFF8+KMV-1+IM) = W1(IOFF8+KMV-1+IM)                                   
     1                    + QFD(KQ) * TX(ITAM)                          M44.1017
     2                    + SFD(KQ) * TR(ITAM)                          M44.1018
      IF ( IPRSUP .GT. 0 )                                              M44.1019
     1 WRITE(LUNIT6,4333) IM,JM,IT2,QFD(KQ),SFD(KQ),                         
     2 R(ITAM),C(ITAM),W1(IOFF1+KMV-1+IM),W1(IOFF2+KMV-1+IM),           M44.1021
     3 TR(ITAM),TX(ITAM),W1(IOFF7+KMV-1+IM),W1(IOFF8+KMV-1+IM)          M44.1022
4333  FORMAT ( 1X,I2,3X,I2,3X,I3, 10E11.4 )                             M44.1023
      KQ = KQ + 1                                                       M44.1024
1072  CONTINUE                                                          M44.1025
      ITAM = ITAM + 1                                                   M44.1026
1071  CONTINUE                                                          M44.1027
C     CALL MVECC(QFD(KQ),SFD(KQ),TR(ITADD),TX(ITADD),IT2,               M44.1029
C    1    W1( IOFF7 + K ), W1( IOFF8 + K ) )         !modal current Ik'=M44.1030
      ICBR = ITADD                                                      M44.1032
      DO 8214 I=1,NPHS                                                  M44.1033
      R(ICBR) = -R(ICBR)                                                        
      C(ICBR) = -C(ICBR)                                                M44.1035
      ICBR = ICBR + 1                                                   M44.1036
8214  CONTINUE                                                          M44.1037
C                                                                       M44.1038
C                                  Evaluate:   im'(to-dt)  ,  ik'(to-dt)M44.1039
C                                                                       M44.1040
      KADT = K                                                          M44.1041
      DO 101 IDT=1,IT2                                                  M44.1042
      N1 = KODSEM(KADT)                                                         
      NRZ = CKI(KADT)                                                           
      NRA = CKKJM(KADT)                                                         
      NK1 = N1 + 5*NRA + 5*NRZ + 4 + 1                                  M44. 528
      NN17 = SCONST(NK1)                                                M44. 529
      NN5 = N1 + 5*NRZ + 5*NRA + 4 + 2 + 1 +1 +nn17                             
      N6 = NN5 + 1                                                      M44.1047
      PHF = ATAN2Z( W1(IOFF2+KADT),W1(IOFF1+KADT) )                     M44.1050
      HF = DSQRT( W1(IOFF2+KADT)**2 + W1(IOFF1+KADT)**2 )               M44.1051
      SCONST(NN5) = HF * DCOS( WDT + PHF )                              M44.1052
      PHF = ATAN2Z( W1(IOFF8+KADT),W1(IOFF7+KADT) )                     M44.1053
      HF = DSQRT( W1(IOFF7+KADT)**2 + W1(IOFF8+KADT)**2 )               M44.1054
      SCONST(N6) = HF * DCOS( WDT + PHF )                               M44.1055
      IF ( IPRSUP .GT. 0 )                                              M44.1058
     1 WRITE(LUNIT6,*) '   AIM5DT,      AIK5DT ',                            
     2                SCONST(NN5), SCONST(N6)                           M44.1060
      KADT = KADT + 1                                                   M44.1061
101   CONTINUE                                                          M44.1062
C                                         Evaluate    Vm'    Vk'        M44.1063
C                                                     Vmj'   Vkj'       M44.1064
      KA = K                                                            M44.1065
      DO 9874 JH=1, IT2                                                 M44.1066
      N4 = INDHST(KA)                                                           
      CNVHST(N4+5) = 0.                                                         
      CNVHST(N4+6) = 0.                                                         
      CNVHST(N4+7) = 0.                                                         
      CNVHST(N4+8) = 0.                                                         
      KA = KA + 1                                                       M44.1072
9874  CONTINUE                                                          M44.1073
      KVD = K                                                           M44.1074
      KV = INFDLI(INOFF1+K)                                             M44.1075
      NQ1 = INFDLI(INOFF2+K)                                            M44.1076
C      LJ = KV                              ! 1st pole of Qii for the brM44.1077
      LJ = NR(K)                                                        M44. 530
c      IF ( KV .GT. 1) LJ = ( KV-1 ) * 2 + 1                            M44.1078
C      KILINE = LPAST                                                   M44.   9
C      LJ = KILINE + LJ                                                 M44.1080
C      NR(K) = LJ                                                       M44.1081
      IF ( IPRSUP  .GE.  1 )                                            M44.1082
     1 WRITE ( LUNIT6, 5467 )   k, ibr, NTOT                            M44.1083
5467  FORMAT ( 37H BEGIN Qt*Vk,m = Vk,m in mode domain ,                M44.1084
     1         24H       k     IBR    NTOT  ,/,  23X, 3I8 )             M44.1085
      DO 102 JV=1, IT2                                                          
      N4 = INDHST(KVD)                                                          
      DO 103 IV=1, IT2                                                          
      N1 = -KBUS(N7)                                                            
      N2 = IABS( MBUS(N7) )                                                     
      NTEQ=WK1(KOFF20+KV)                                                       
      SUMKR=SCONST(NQ1)*E(N1)                                                   
      SUMKI=SCONST(NQ1)*F(N1)                                                   
      SUMMR=SCONST(NQ1)*E(N2)                                                   
      SUMMI=SCONST(NQ1)*F(N2)                                                   
      IF ( IPRSUP .GT. 0 )                                              M44.1096
     1 WRITE(LUNIT6,4839)                                                    
4839  FORMAT(30H IQ  NTEQ    NQ1    NQ2    NQ3,                         M44.1098
     122H     S(NQ1)     S(NQ2),                                        M44.1099
     222H     S(NQ3)      E(N1),                                        M44.1100
     322H      E(N2)      HK2(),                                        M44.1101
     511H      HK3(),                                                   M44.1102
     422H      SUMKR      SUMMR)                                        M44.1103
      JKL = 0                                                           M44. 531
      DO 1020 IQ=1,NTEQ                                                         
      NQ2 = NQ1 + IQ                                                            
      NQ3 = NQ2 + NTEQ                                                          
      IF ( ABSZ( SCONST(NQ2) ) .GE. 1.E+13 ) GO TO 3003                 M44. 532
      D = OMEGA ** 2 + SCONST(NQ3) ** 2                                 M44.1107
      AR = SCONST(NQ2) * SCONST(NQ3) / D                                M44.1108
      AI = - SCONST(NQ2) * OMEGA / D                                    M44.1109
      GO TO 3004                                                        M44. 533
 3003 JKL = JKL + 1                                                     M44. 534
      IF ( JKL .EQ. 2 ) GO TO 3005                                      M44. 535
      AKR = SCONST(NQ2) / 1.E+15                                        M44. 536
      AKI = SCONST(NQ2+1) / 1.E+15                                      M44. 537
      APR = SCONST(NQ3)   / 1.E+15                                      M44. 538
      API = SCONST(NQ3+1) / 1.E+15                                              
      DQQ3 = APR ** 2 + ( OMEGA + API ) ** 2                            M44. 540
      AR = 2 * ( AKR * APR + AkI  * (OMEGA + API ) ) / DQQ3             M44. 541
      AI = 0.                                                           M44. 542
 3004 W1( IOFF3 + IQ ) = AR * E(N1) - AI * F(N1)                                
      W1( IOFF4 + IQ ) = AR * F(N1) + AI * E(N1)                                
      W1( IOFF5 + IQ ) = AR * E(N2) - AI * F(N2)                                
      W1( IOFF6 + IQ ) = AR * F(N2) + AI * E(N2)                                
      SUMKR = SUMKR + W1( IOFF3 + IQ )                                          
      SUMKI = SUMKI + W1( IOFF4 + IQ )                                          
      SUMMR = SUMMR + W1( IOFF5 + IQ )                                          
      SUMMI = SUMMI + W1( IOFF6 + IQ )                                          
      IF ( JKL .EQ. 1 ) GO TO 3006                                      M44. 543
 3005 JKL = 0                                                           M44. 544
 3006 IF ( IPRSUP .GT. 0 )                                              M44.1120
     1 WRITE(LUNIT6,9333) IQ,NTEQ,NQ1,NQ2,NQ3,SCONST(NQ1),                   
     2           SCONST(NQ2),SCONST(NQ3),E(N1),                         M44.1122
     3         E(N2),W1(IOFF3+IQ),W1(IOFF4+IQ),                         M44.1123
     4         SUMKR,SUMMR                                              M44.1124
9333  FORMAT ( 1X,I2,3X,I3,2X,I5,2X,I5,2X,I5, 9E11.4 )                  M44.1125
      NQ2 = NQ2 + 1                                                     M44.1126
      NQ3 = NQ3 + 1                                                     M44.1127
1020  CONTINUE                                                                  
      NQ1 = NQ1 + 2 * NTEQ + 1                                          M44.1129
      IF ( IPRSUP .GT. 0 )                                              M44.1130
     1 WRITE(LUNIT6,5433) IV,JV,CNVHST(N4+5),CNVHST(N4+7)                    
      CNVHST(N4+7)=CNVHST(N4+7)+SUMKR                                           
      CNVHST(N4+8)=CNVHST(N4+8)+SUMKI                                           
      CNVHST(N4+5)=CNVHST(N4+5)+SUMMR                                   M44.1134
      CNVHST(N4+6)=CNVHST(N4+6)+SUMMI                                           
      IF ( IPRSUP .GT. 0 )                                              M44.1136
     1 WRITE(LUNIT6,5433) KVD,JV,CNVHST(N4+5),CNVHST(N4+7)                  
5433  FORMAT (' KVD,JV,VMR5(KVD),VKR5(KVD)=',1X,I5,1X,I5,2X, 2E14.5 )   
C                                                                       M44.1139
C        Evaluate Vmj'(to-dt)                                           M44.1140
C                                                                       M44.1141
C     LJ = IADRSDT  !keep parallel pointer as tranformation matrix for VM44.1142
      LJ1 = LJ + NTEQ                                                   M44.1143
      IF ( IPRSUP .GT. 0 )                                              M44.1144
     1 WRITE(LUNIT6,4733)                                                    
4733  FORMAT(45H   LJ   LJ1  NTEQ    VMJ5DT(LJ)    VKJ5DT(LJ))          M44.1146
      DO 104 LQV=1,NTEQ                                                         
      PHF = ATAN2Z( W1(IOFF6+LQV),W1(IOFF5+LQV) )                       M44.1148
      HF = SQRTZ( W1(IOFF5+LQV)**2 + W1(IOFF6+LQV)**2 )                 M44.1149
      XM(LJ) = HF * COSZ( WDT + PHF )                                           
      PHF = ATAN2Z( W1(IOFF4+LQV),W1(IOFF3+LQV) )                       M44.1151
      HF = SQRTZ( W1(IOFF3+LQV)**2 + W1(IOFF4+LQV)**2 )                 M44.1152
      XM(LJ1) = HF * COSZ( WDT + PHF )                                          
      IF ( IPRSUP .GE. 1 )                                              
     1 WRITE(LUNIT6,9733)  LJ,  LJ1,  NTEQ,   XM(LJ),    XM(LJ1)             
9733  FORMAT( 2X,I8, 2X,I8, 2X,I4, 2E14.3 )                          
      LJ = LJ + 1                                                               
      LJ1 = LJ1 + 1                                                     M44.1158
104   CONTINUE                                                                  
      LJ = LJ + NTEQ                                                    M44.1160
      N7 = N7 + 1                                                       M44.1162
      IF ( IV .EQ. IT2 ) N7 = N7 - IT2                                          
      KV = KV + 1                                                       M44.1164
103   CONTINUE                                                          M44.1165
      PHF = ATAN2Z( CNVHST(N4+6) ,CNVHST(N4+5) )                        M44.1166
      HF = SQRTZ( CNVHST(N4+5)**2 + CNVHST(N4+6)**2 )                   M44.1167
      WK1( KOFF1 + KVD ) = HF * COSZ( WDT + PHF )                               
      PHF = ATAN2Z( CNVHST(N4+8) ,CNVHST(N4+7) )                        M44.1169
      HF = SQRTZ( CNVHST(N4+7)**2 + CNVHST(N4+8)**2 )                   M44.1170
      WK1( KOFF2 + KVD ) = HF * COSZ( WDT + PHF )                               
C      write(*,*) 'vm5dt(kvd), vk5dt(kvd) =,',                          M44.1172
C     1            WK1(KOFF1+KVD), WK1(KOFF2+KVD)                       M44.1173
      KVD = KVD + 1                                                     M44.1174
102   CONTINUE                                                          M44.1175
C                                                                       M44.1176
C        Evaluate:    Gmj = Ycj' * Vm'   Gkj' = Ycj' * Vk'              M44.1177
C                     Fk = Gk + Ik'                                     M44.1178
C                     Bm = Gm - Im'                                     M44.1179
      KY = K                                                            M44.1180
      DO 106 IV=1, IT2                                                          
C     CALL FJ(VKR5(IV),VKI5(IV),XR,XI,GKR,GKI                           M44.1182
C     1       ,VMR5(IV),VMI5(IV),EJA,EJY,GMR,GMI                        M44.1183
C     1       ,YPI(IADRS),YKI(IADRS),W,NTEY,0.D0,YK0(IV))               M44.1184
      N1 = KODSEM(KY)                                                           
      N4 = INDHST(KY)                                                   M44.1186
      SUMKR=SCONST(N1)*CNVHST(N4+7)                                             
      SUMKI=SCONST(N1)*CNVHST(N4+8)                                             
      SUMMR=SCONST(N1)*CNVHST(N4+5)                                             
      SUMMI=SCONST(N1)*CNVHST(N4+6)                                             
      NRZ = CKI(KY)                                                             
      NRA = CKKJM(KY)                                                           
      JKL = 0                                                           M44. 545
      DO 2020  II = 1, NRZ                                                      
      N2 = N1 + II                                                              
      N3 = N2 + NRZ                                                             
      IF ( ABSZ( SCONST(N2) ) .GE. 1.E+13 ) GO TO 5326                  M44. 546
      D1 = SCONST(N3) ** 2 + OMEGA ** 2                                         
      CZIRE = SCONST(N2) * SCONST(N3) / D1                                      
      CZIIM = - SCONST(N2) * OMEGA / D1                                         
      GO TO 5328                                                        M44. 547
 5326 JKL = JKL + 1                                                     M44. 548
      IF ( JKL .EQ. 2 ) GO TO 5327                                      M44. 549
      AKR = SCONST(N2) / 1.E+15                                         M44. 550
      AKI = SCONST(N2+1) / 1.E+15                                       M44. 551
      APR = SCONST(N3) / 1.E+15                                         M44. 552
      API = SCONST(N3+1) / 1.E+15                                               
      DJK3 = APR ** 2 + ( OMEGA + API ) ** 2                            M44. 554
      DDD3 = ( AKR * APR + AkI  * (OMEGA + API ) ) / DJK3               M44. 555
C     DDD4 = ( AKI * APR - AKR * ( OMEGA + API ) ) / DJK3               M44. 556
      CZIRE = 2 * DDD3                                                  M44. 557
      CZIIM = 0.                                                        M44. 558
 5328 CONTINUE                                                          M44. 559
      W1(IOFF3+II)=CZIRE*CNVHST(N4+7)-CZIIM*CNVHST(N4+8)                        
      W1(IOFF4+II)=CZIRE*CNVHST(N4+8)+CZIIM*CNVHST(N4+7)                        
      W1(IOFF5+II)=CZIRE*CNVHST(N4+5)-CZIIM*CNVHST(N4+6)                        
      W1(IOFF6+II)=CZIRE*CNVHST(N4+6)+CZIIM*CNVHST(N4+5)                        
      SUMKR=SUMKR+W1(IOFF3+II)                                          M44.1203
      SUMKI=SUMKI+W1(IOFF4+II)                                          M44.1204
      SUMMR=SUMMR+W1(IOFF5+II)                                          M44.1205
      SUMMI=SUMMI+W1(IOFF6+II)                                          M44.1206
      IF ( IPRSUP .GT. 0 )                                              M44.1207
     1 WRITE(LUNIT6,5343) N1,II,N2,N3,SCONST(N2),SCONST(N3)             M44.1208
5343  FORMAT( 1X, 20HN1,II,N2,N3,YPI,YKI=,                              M44.1209
     1             1X,I6,2X,I6,3X,I6,3X,I6,3X,2E14.5)                   M44.1210
      IF (JKL .EQ. 1) GO TO 2020                                        M44. 560
 5327 JKL = 0                                                           M44. 561
2020  CONTINUE                                                          M44.1211
      IF ( IPRSUP .GT. 0 )                                              M44.1212
     1 WRITE(LUNIT6,*) 'the VOLTAGE Vm & Vk. SUMMR,SUMMI,SUMKR,SUMKI',       
     2                                  SUMMR,SUMMI,SUMKR,SUMKI         M44.1214
C                                                                       M44.1215
C        Evaluate:   gmj(to-dt)                                         M44.1216
C                                                                       M44.1217
      IF ( IPRSUP .GT. 0 )                                              M44.1218
     1 WRITE(LUNIT6,5443)                                                    
5443  FORMAT(31HKY KODSEM(KY) CKI(KY) CKKJM(KY),                        M44.1220
     1       38H  NN5   N6    GMJDT(NN5)     GKJDT(N6))                 M44.1221
      NK1 = N1 + 5*NRA + 5*NRZ + 4 + 1                                  M44. 562
      NN17 = SCONST(NK1)                                                M44. 563
      JKL = 0                                                           M44. 564
      DO 107 LG = 1, NRZ                                                M44.1222
      NN5 = N1 + 5*NRZ + 5*NRA + 4 + 2 + 2 + LG + 1 + NN17              M44.1223
      N6 = NN5 + NRZ                                                    M44.1224
      N2 = N1 + LG                                                              
      IF ( ABSZ( SCONST(N2) ) .GE. 1.E+13 ) JKL = JKL + 1               M44. 566
      IF ( JKL .EQ. 2 ) GO TO 5829                                      M44. 567
      PHF = ATAN2Z( W1(IOFF6+LG),W1(IOFF5+LG) )                         M44.1227
      HF = DSQRT( W1(IOFF5+LG)**2 + W1(IOFF6+LG)**2 )                   M44.1228
      SCONST(NN5) = HF * DCOS( WDT + PHF )                                      
      PHF = ATAN2Z( W1(IOFF4+LG),W1(IOFF3+LG) )                         M44.1230
      HF = DSQRT( W1(IOFF3+LG)**2 + W1(IOFF4+LG)**2 )                   M44.1231
      SCONST(N6) = HF * DCOS( WDT + PHF )                                       
      IF ( IPRSUP .GT. 0 )                                              M44.1235
     1 WRITE(LUNIT6,5943)   KY,KODSEM(KY),NRZ,NRA,                      M44.1236
     2          NN5,N6,SCONST(NN5),SCONST(N6)                           M44.1237
 5943 FORMAT ( 1X,I5,8X,I6,5X,I6,7X,I6,2X,I6,2X,I6,2E14.5 )             M44.1238
      IF (JKL .EQ. 1) GO TO 107                                         M44. 568
 5829 JKL = 0                                                           M44. 569
  107 CONTINUE                                                          M44.1239
C                                                                       M44.1240
C        Evaluate:   Bm'                                                M44.1241
C                                                                       M44.1242
      CNVHST(N4+9)  = SUMMR - W1( IOFF1 + KY )                                  
      CNVHST(N4+10) = SUMMI - W1( IOFF2 + KY )                                  
      CNVHST(N4+11) = SUMKR + W1( IOFF7 + KY )                                  
      CNVHST(N4+12) = SUMKI + W1( IOFF8 + KY )                                  
      IF ( IPRSUP .GT. 0 )                                              M44.1251
     1 WRITE(LUNIT6,5444) KY,CNVHST(N4+9),CNVHST(N4+10),                     
     2                  CNVHST(N4+11),CNVHST(N4+12)                     M44.1253
5444  FORMAT (27HKY,BMR5(KY),BMI5,FKR5,FKI5=,1X,I2,2X, 4E14.5 )         M44.1254
      KY = KY + 1                                                       M44.1255
106   CONTINUE                                                          M44.1256
C                                                                       M44.1257
C        Evaluate:     imj(to-dt)  ikj(to-dt)                           M44.1258
C                                                                       M44.1259
      IF ( IPRSUP .GT. 0 )                                              M44.1260
     1 WRITE ( LUNIT6, 4576 )   k, ibr, NTOT                            M44.1261
4576  FORMAT ( 37H BEGIN Qt*Ik,m = Ik,m in phase domain,                M44.1262
     1         24H       k     IBR    NTOT  ,/,  23X, 3I8 )             M44.1263
      NQ1 = INFDLI(INOFF2+K)                                                    
      KA = K                                                            M44.1265
      KQ = INFDLI(INOFF1+K)                                             M44.1266
      KQK = INFDLI(INOFF1+KA)                                           M44.1267
      KV = INFDLI(INOFF1+K)                                             M44.  10
C      LJ = KV                              ! 1st pole of Qii for the brM44.  11
c      IF ( KV .GT. 1) LJ = ( KV-1 ) * 2 + 1                            M44.  12
C      KILINE = LPAST                                  !next cell is freM44.  14
C      LJ = KILINE + LJ                                                 M44.  15
C      NR(K) = LJ                                                       M44.  16
      LJ = NR(K)                                                        M44. 570
      DO 1109 JQA=1, IT2                                                        
      DO 1180 IQA=1, IT2                                                        
      NTEQ=WK1(KOFF20+KQ)                                                       
      SUMKR = SCONST(NQ1) * W1( IOFF7 + KA )                                    
      SUMKI = SCONST(NQ1) * W1( IOFF8 + KA )                                    
      SUMMR = SCONST(NQ1) * W1( IOFF1 + KA )                                    
      SUMMI = SCONST(NQ1) * W1( IOFF2 + KA )                                    
      IF ( IPRSUP .GT. 0 )                                              M44.1276
     1 WRITE(LUNIT6,4433)                                                    
4433  FORMAT(29HJQA NTEQ    NQ1    NQ2    NQ3,                          M44.1278
     118H SNT(NQ1) SNT(NQ2),                                            M44.1279
     127H SST(NQ3) AIMR(KA) AIMI(KA),                                   M44.1280
     227H  HM1(KA)  HK1(KA)    SUMKR,                                   M44.1281
     327H    SUMKI    SUMMR    SUMMI)                                   M44.1282
      DO 3020 IQ=1,NTEQ                                                 M44.1283
      NQ2 = NQ1 + IQ                                                            
      NQ3 = NQ2 + NTEQ                                                          
      IF ( ABSZ( SCONST(NQ2) ) .GE. 1.E+13 ) GO TO 3007                 M44. 571
      D = OMEGA ** 2 + SCONST(NQ3) ** 2                                 M44.1286
      AR = SCONST(NQ2) * SCONST(NQ3) / D                                M44.1287
      AI = -SCONST(NQ2) * OMEGA / D                                     M44.1288
      GO TO 3008                                                        M44. 572
 3007 JKL = JKL + 1                                                     M44. 573
      IF ( JKL .EQ. 2 ) GO TO 3009                                      M44. 574
      AKR = SCONST(NQ2) / 1.E+15                                        M44. 575
      AKI = SCONST(NQ2+1) / 1.E+15                                      M44. 576
      APR = SCONST(NQ3)   / 1.E+15                                      M44. 577
      API = SCONST(NQ3+1) / 1.E+15                                              
      DQQ3 = APR ** 2 + ( OMEGA + API ) ** 2                            M44. 579
      AR = 2 * ( AKR * APR + AkI  * (OMEGA + API ) ) / DQQ3             M44. 580
      AI = 0.                                                           M44. 581
 3008 W1( IOFF3 + IQ ) = AR * W1( IOFF7 + KA )                          M44.1289
     1                 - AI * W1( IOFF8 + KA )                          M44.1290
      W1( IOFF4 + IQ ) = AR * W1( IOFF8 + KA )                          M44.1291
     2                 + AI * W1( IOFF7 + KA )                          M44.1292
      W1( IOFF5 + IQ ) = AR * W1( IOFF1 + KA )                          M44.1293
     3                 - AI * W1( IOFF2 + KA )                          M44.1294
      W1( IOFF6 + IQ ) = AR * W1( IOFF2 + KA )                          M44.1295
     4                 + AI * W1( IOFF1 + KA )                          M44.1296
      SUMKR = SUMKR + W1( IOFF3 + IQ )                                  M44.1297
      SUMKI = SUMKI + W1( IOFF4 + IQ )                                  M44.1298
      SUMMR = SUMMR + W1( IOFF5 + IQ )                                  M44.1299
      SUMMI = SUMMI + W1( IOFF6 + IQ )                                  M44.1300
      IF ( JKL .EQ. 1 ) GO TO 3010                                      M44. 582
 3009 JKL = 0                                                           M44. 583
 3010 IF ( IPRSUP .GT. 0 )                                              M44.1302
     1 WRITE(LUNIT6,4773) JQA,NTEQ,NQ1,NQ2,NQ3,SCONST(NQ1),                  
     2           SCONST(NQ2),SCONST(NQ3),W1(IOFF7+KA),                  M44.1304
     3   W1(IOFF8+KA),W1(IOFF1+KA),W1(IOFF2+KA),SUMKR,                  M44.1305
     4                              SUMKI,SUMMR,SUMMI                   M44.1306
4773  FORMAT ( 1X,I2,3X,I2,2X,I5,2X,I5,2X,I5, 11E9.3 )                  M44.1307
      NQ2 = NQ2 + 1                                                     M44.1308
      NQ3 = NQ3 + 1                                                     M44.1309
3020  CONTINUE                                                          M44.1310
      NQ1 = NQ1 + 2 * NTEQ + 1                                          M44.1311
      NTEQ=WK1(KOFF20+KQ)                                                       
      LJ1 = LJ + NTEQ                                                   M44.1313
      DO 116 LA=1,NTEQ                                                  M44.1314
      PHF = ATAN2Z( W1(IOFF4+LA),W1(IOFF3+LA) )                         M44.1315
      HF = DSQRT( W1(IOFF3+LA)**2 + W1(IOFF4+LA)**2 )                   M44.1316
      XK(LJ1) = HF * DCOS( WDT + PHF )                                          
      PHF = ATAN2Z( W1(IOFF6+LA),W1(IOFF5+LA) )                         M44.1318
      HF = DSQRT( W1(IOFF5+LA)**2 + W1(IOFF6+LA)**2 )                   M44.1319
      XK(LJ) = HF * DCOS( WDT + PHF )                                           
      LJ = LJ + 1                                                       M44.1321
      LJ1 = LJ1 + 1                                                     M44.1322
116   CONTINUE                                                          M44.1323
      KQ=KQ+1                                                           M44.1324
      LJ = LJ + NTEQ                                                    M44.1325
1180  CONTINUE                                                          M44.1326
      KA = KA + 1                                                       M44.1327
1109  CONTINUE                                                                  
C                                                                       M44.1329
C        Evaluate:   Fmj' = A1j * Fk'  => fmj'(to), fm'(to)             M44.1330
C                    Bkj' = A1j * Bm'  => bkj'(to), bk'(to)             M44.1331
C                                                                       M44.1332
C      CALL FJ(FKR5(I),FKI5(I),FMJ5(IADRS),FMJI5(IADRS),FM5(I),SUMKI    M44.1333
C     1       ,BMR5(I),BMI5(I),BKJ5(IADRS),BKJI5(IADRS),BK5(I),SUMMI    M44.1334
C     2       ,API(IADRS),AKI(IADRS),W,NTEA,TAU(I),0.D0)                M44.1335
      KA = K                                                            M44.1336
      IADRS = 1                                                         M44.1337
      JKL = 0                                                           M44. 584
      DO 1112 IFA=1, IT2                                                M44.1338
      N4 = INDHST(KA)                                                           
      TAUA = CNVHST(N4)                                                 M44.1340
      EJWTR = DCOS(OMEGA * TAUA)                                        M44.1341
      EJWTI = -DSIN(OMEGA * TAUA)                                       M44.1342
      SUMKR = 0.D0                                                      M44.1343
      SUMKI = 0.D0                                                      M44.1344
      SUMMR = 0.D0                                                      M44.1345
      SUMMI = 0.D0                                                      M44.1346
      N1 = KODSEM(KA)                                                           
      NRZ = CKI(KA)                                                     M44.1348
      NRA = CKKJM(KA)                                                           
      IF ( IPRSUP .GT. 0 )                                              M44.1350
     1 WRITE(LUNIT6,4833)                                                    
4833  FORMAT(21H NK7  NRA  API(IADRS),                                  M44.1352
     436H       CZIRE       CZIIM                                       M44. 585
     136H  AKI(IADRS)    FKR5(KA)    FKI5(KA)      ,/,                  M44.1353
     236H    BMR5(KA)    BMI5(KA) FMJ5(IADRS),                          M44.1354
     336H BKJ5(IADRS)     FM5(II)     BK5(II))                          M44.1355
      DO 1028  II = 1, NRA                                                      
      N2 = N1 + 2*NRZ + II                                                      
      N3 = N2 + NRA                                                             
      NK1 = N1 + 5*NRA + 5*NRZ + 4 + 1                                          
      NN17 = SCONST(NK1)                                                        
      NN5 = N1 + 7*NRZ + 5*NRA + 8 + II +1 +NN17                                
      NN6 = NN5 + NRA                                                           
      IF ( ABS( SCONST(N2) ) .GE. 1.E13 ) GO TO 4338                    M44. 588
      D1 = SCONST(N3) ** 2 + OMEGA ** 2                                         
      CZIRE = SCONST(N2) * SCONST(N3) / D1                                      
      CZIIM = - SCONST(N2) * OMEGA / D1                                         
      go to 4337                                                        M44. 589
 4338 JKL = JKL + 1                                                     M44. 590
      IF ( JKL .EQ. 2) GO TO 5486                                               
      NK7 = N1 + 7*NRA + 7*NRZ + 9 + NN17 + II                                  
      NK8 = NK7 + NRA                                                           
      AKR = SCONST(N2) / 1.E+15                                         M44. 594
      AKI = SCONST(N2+1) / 1.E+15                                       M44. 595
      APR = SCONST(N3) / 1.E+15                                         M44. 596
      API = SCONST(N3+1) / 1.E+15                                               
      DKR = APR * AKR - API * AKI                                               
      DKI = APR * AKI + API * AKR                                               
      AKR = DKR                                                                 
      AKI = DKI                                                                 
      DJK3 = APR ** 2 + ( OMEGA + API ) ** 2                            M44. 602
      DDD3 = ( AKR * APR + AkI  * (OMEGA + API ) ) / DJK3               M44. 603
      DDD4 = ( AKI * APR - AKR * ( OMEGA + API ) ) / DJK3               M44. 604
      DJK4 = APR ** 2 + ( OMEGA - API ) ** 2                            M44. 605
      CCC3 = ( AKR * APR - AKI * ( OMEGA - API ) ) / DJK4               M44. 606
      CCC4 = ( AKR * ( OMEGA - API ) + AKI * APR ) / DJK4               M44. 607
      CZIRE = DDD3 + CCC3                                                       
      CZIIM = DDD4 - CCC4                                                       
      CZIRE = DDD3                                                              
      CZIIM = DDD4                                                              
 4337 CONTINUE                                                          M44. 612
      W1(IOFF3+II) = CZIRE * CNVHST(N4+11)                                      
     1             - CZIIM * CNVHST(N4+12)                                      
      W1(IOFF4+II) = CZIRE * CNVHST(N4+12)                                      
     2             + CZIIM * CNVHST(N4+11)                                      
      W1(IOFF5+II) = CZIRE * CNVHST(N4+9)                                       
     3             - CZIIM * CNVHST(N4+10)                                      
      W1(IOFF6+II) = CZIRE * CNVHST(N4+10)                                      
     4             + CZIIM * CNVHST(N4+9)                                       
      AR = W1(IOFF3+II) * EJWTR - W1(IOFF4+II) * EJWTI                  M44.1372
      AI = W1(IOFF3+II) * EJWTI + W1(IOFF4+II) * EJWTR                  M44.1373
      W1(IOFF3+II) = AR                                                         
      W1(IOFF4+II) = AI                                                 M44.1375
      SCONST(NN5) = AR                                                          
      AR = W1(IOFF5+II) * EJWTR - W1(IOFF6+II) * EJWTI                  M44.1377
      AI = W1(IOFF5+II) * EJWTI + W1(IOFF6+II) * EJWTR                  M44.1378
      W1(IOFF5+II) = AR                                                 M44.1379
      W1(IOFF6+II) = AI                                                         
      SCONST(NN6) = AR                                                          
      SUMKR = SUMKR + W1(IOFF3+II)                                      M44.1382
      SUMKI = SUMKI + W1(IOFF4+II)                                      M44.1383
      SUMMR = SUMMR + W1(IOFF5+II)                                      M44.1384
      SUMMI = SUMMI + W1(IOFF6+II)                                      M44.1385
      IF ( IPRSUP .GT. 0 )                                              M44.1386
     1 WRITE(LUNIT6,4334) NK7,NRA,SCONST(N3),SCONST(N2), CZIRE,         M44.1387
     2               CZIIM,  CNVHST(N4+11),CNVHST(N4+12),               M44.1388
     3                 CNVHST(N4+9),CNVHST(N4+10),                      M44.1389
     4        SCONST(NN5),SCONST(NN6),SUMKR,SUMMR                       M44.1390
4334  FORMAT ( 1X,I3,2X,I3,  6e16.7  ,/,  9x, 6e16.7 )                  M44.1391
      IF ( JKL .EQ. 1 ) GO TO 1028                                              
 5486 JKL = 0                                                                   
1028  CONTINUE                                                                  
      WK1( KOFF3 + KA ) = SUMKR                                                 
      WK1( KOFF4 + KA ) = SUMMR                                                 
      KA = KA + 1                                                               
      IF ( IPRSUP .GT. 0 )                                              M44.1396
     1 WRITE(LUNIT6,*) ' Fmj = A1j * Fk; Bkj = A1j * Bm'                     
      IF ( IPRSUP .GT. 0 )                                              M44.1398
     1 WRITE(LUNIT6,*)' SUMKR, jSUMKI, SUMMR, jSUMMI ',                      
     2             SUMKR,  SUMKI, SUMMR,  SUMMI                         M44.1400
1112   CONTINUE                                                         M44.1401
C                                                                       M44.1402
C        Fill up history vectors: bm'(history)                          M44.1403
C                                 fk'(history)                          M44.1404
C                                                                       M44.1405
      KA = K                                                            M44.1406
      IF (KMODE .EQ. 5 )     KOFF11 = KCBL                              M44.1407
      IF ( KOFF11 .LT. LHIST) GO TO 1114
      WRITE (LUNIT6,5340)  LHIST, KOFF11
      WRITE( LUNIT6, *) ' Nearby statement number is 1114. '
      STOP

 1114 DO 1115 IFA=1, IT2                                                        
      NN4 = INDHST(KA)                                                          
      TAUI = CNVHST(NN4)                                                        
      NTAUI = TAUI / DELTAT + 1                                                 
C      CALL FILLUP(CNVHST(NN4+11),CNVHST(NN4+12),FKH5(IADRS)            M44.1414
C     1           ,CNVHST(NN4+9), CNVHST(NN4+10),BMH5(IADRS)            M44.1415
C     2           ,OMEGA,WDT,NTAUI,TAUI)                                M44.1416
C      CALL FILLUP(FKR5(IFA),FKI5(IFA),FKH5(IADRS)                      M44.1417
C     1           ,BMR5(IFA),BMI5(IFA),BMH5(IADRS)                      M44.1418
C     2           ,W,WDT,NTAUI,TAUI)                                    M44.1419
      N1    =  NTAUI - 1                                                        
      AMAGK = DSQRT( CNVHST(NN4+11) ** 2 + CNVHST(NN4+12) ** 2 )                
      AMAGM = DSQRT( CNVHST(NN4+ 9) ** 2 + CNVHST(NN4+10) ** 2 )                
      ANGK  = ATAN2Z( CNVHST(NN4+12)      , CNVHST(NN4+11) )                    
      ANGM  = ATAN2Z( CNVHST(NN4+10)      , CNVHST(NN4+ 9) )                    
      WD    = WDT                                                               
      KOFF11 = KOFF11 + 0                                                       
      KOFF12 = KOFF11 + NTAUI                                                   
      DO 100 I = 1, N1                                                          
      WK1( KOFF11+I ) = AMAGK * DCOS( WD + ANGK )                               
      WK1( KOFF12+I ) = AMAGM * DCOS( WD + ANGM )                               
      WD = WD + WDT                                                             
100   CONTINUE                                                                  
      WK1( KOFF11+NTAUI ) = AMAGK *                                     M44.1436
     1                      DCOS( -OMEGA * TAUI + ANGK )                        
      WK1( KOFF12+NTAUI ) = AMAGM *                                     M44.1438
     2                      DCOS( -OMEGA * TAUI + ANGM )                        
      IF ( IPRSUP .GT. 0 )                                              M44.1440
     1 WRITE (LUNIT6, 80150) NTAUI                                           
80150 FORMAT (/, 47H FORWARD FUNCTIONS FKHIST AND BMHIST; POSITIONS  ,  M44.1442
     14H   1, 4H  TO, I6)                                               M44.1443
      IF ( IPRSUP .GT. 0 )                                              M44.1444
     1 WRITE (6,80145) (WK1(KOFF11+I),I=1,NTAUI),                       M44.1445
     2                 (WK1(KOFF12+I),I=1,NTAUI)                        M44.1446
80145 FORMAT (1X, 10E12.5)                                              M44.1447
      IF ( IPRCBL .GT. 0 )                                              M44.1448
     1WRITE(LUNIT6,*)'KA,FKR5(KA),FKI5(KA),FKH5(IADRS)=',                    
     2 KA,CNVHST(NN4+11),CNVHST(NN4+12),WK1(KOFF11+NTAUI)               M44.1450
      IF ( IPRCBL .GT. 0 )                                              M44.1451
     1WRITE (LUNIT6,*)'BMR5(KA),BMI5(KA),BMH5(IADRS),WDT,NTAUI=',            
     2 CNVHST(NN4+9),CNVHST(NN4+10),WK1(KOFF12+NTAUI),WDT,NTAUI         M44.1453
      KA = KA + 1                                                               
      KOFF12 = KOFF12 + NTAUI                                                   
      KOFF11 = KOFF12                                                           
1115  CONTINUE                                                                  
      KCBL = KOFF11                                                     M44.1458
      IF ( KOFF11 .LT. LHIST) GO TO 6258
      WRITE (LUNIT6,5340)  LHIST, KOFF11
      WRITE(LUNIT6, *) ' Nearby statement number is 6258. '
      STOP

 6258 IF ( KOFF11 .GT. KOFF25+288) KMODE = 5                            M44.1460
      K = K + IT2 -1                                                    M44.1461
      ITADD = ITADD + IT2 + ( IT2 - 1 ) * IT2 / 2                       M44.1462
      GO TO 547                                                                 
C                                                                       M44.1465
60001 IT2=IABS(N4)                                                              
      APH = IT2                                                                 
60003 NL = LITYPE(K)                                                            
      IF(ISTEAD.EQ.0) GO TO 60005                                               
C     INITIAL CONDITIONS FROM STEADY-STATE                              M31.2805
C        CONVERT TO MODE QUANTITIES                                     M31.2806
61594 N9 = IT2 * IT2                                                            
      J = NL -1                                                                 
      DO 6259  I = 1, N9                                                        
      J = J + 1                                                         M31.2810
 6259 VOLT(I) =  QFD(J)                                                         
      IF (IPRSUP .GE. 1) WRITE (LUNIT6, 3438) (VOLT(L), L=1, N9)        M35.1339
 3438 FORMAT ( /, 37H MATRIX [T] FOLLOWS.  VOLT(1:N9) ....,             M35.1340
     1       /,  ( 1X, 5E25.16 ) )                                      M35.1341
      CALL MOVER0 ( VOLTI(1), N9 )                                              
      N10 = 1                                                           M35.1343
      N12 = IT2 + 1                                                     M35.1344
      DO 3465  L=1, IT2                                                 M35.1345
      VOLTI(N10) = 1.0                                                  M35.1346
 3465 N10 = N10 + N12                                                   M35.1347
      IF (IPRSUP .GE. 1) WRITE (LUNIT6, 3466) (VOLTI(L), L=1, N9)       M35.1348
 3466 FORMAT ( 30H UNIT MATRIX.  VOLTI(1:N9) ... ,/, (1X, 5E25.16 ) )   M35.1349
      CALL DGELG ( VOLTI(1), VOLT(1), IT2, IT2, EPSILN, IER )           M35.1350
      IF ( IPRSUP  .GE.  1 )                                            M35.1351
     1 WRITE (LUNIT6, 3471)  IER,  ( VOLTI(M), M=1, N9 )                M35.1352
 3471 FORMAT ( /,  28H INVERSE OF  (ZTHEV).  IER =,  I2  ,/,            M35.1353
     1       ( 1X, 5E25.16 ) )                                          M35.1354
      N5 = NL - 1                                                               
      IY = K                                                                    
      IZ = ITADD                                                                
      DO 63594   I = 1, IT2                                                     
      N7 = IY                                                                   
      N8 = IZ                                                           M31.2826
      VSL = 0.0                                                         M31.2827
      VSLI = 0.0                                                        M31.2828
      VSR = 0.0                                                         M31.2829
      VSRI = 0.0                                                        M31.2830
      CSL = 0.0                                                         M31.2831
      CSLI = 0.0                                                        M31.2832
      CSR = 0.0                                                         M31.2833
      CSRI = 0.0                                                        M31.2834
      DO 62595  J = 1, IT2                                                      
      N1 = IABS(KBUS(N7))                                                       
      N2 = IABS(MBUS(N7))                                                       
      N5 = N5 + 1                                                       M31.2838
      H1 = QFD(N5)                                                              
      VSL = VSL + H1 * E(N1)                                                    
      VSLI = VSLI + H1 * F(N1)                                                  
      VSR = VSR + H1 * E(N2)                                                    
      VSRI = VSRI + H1 * F(N2)                                                  
      N6 = I + (J-1) * IT2                                                      
      H2 = VOLTI(N6)                                                            
      CSL = CSL + H2 * TR(N8)                                                   
      CSLI = CSLI + H2 * TX(N8)                                                 
      CSR = CSR + H2 * R(N8)                                                    
      CSRI = CSRI + H2 * C(N8)                                                  
      N7 = N7 + 1                                                               
      N8 = N8 + 1                                                               
      IF ( I .NE. 1 )  GO TO 62595                                      M31.2852
      IF ( IPRSUP .LE. 5 )  GO TO 62595                                 M31.2853
      WRITE (LUNIT6, 72595) J, E(N1),F(N1),E(N2),F(N2),                 M31.2854
     1                      TR(N8),TX(N8),R(N8),C(N8)                   M31.2855
72595 FORMAT(/, 37H INITIAL PHASE VALUES, MARTI'S BRANCH, I4, 8E11.4)   M31.2856
62595 CONTINUE                                                                  
      IF (IPRSUP .GE. 5)                                                M31.2858
     1 WRITE (LUNIT6, 72597) I,VSL,VSLI,VSR,VSRI,CSL,CSLI,CSR,CSRI      M31.2859
72597 FORMAT(/, 37H INITIAL MODAL VALUES, MARTI'S BRANCH, I4, 8E11.4)   M31.2860
      OMEGS1 = OMEGA                                                    M31.2861
      IF ( ISTEAD .EQ. 0 )  OMEGS1 = 0.0                                        
      N4 = INDHST(K)                                                            
      CNVHST(N4+5) = CSL                                                        
      CNVHST(N4+6) = CSR                                                        
      N1 = KODSEM(K)                                                            
      NRZ = CKI(K)                                                              
      NRA = CKKJM(K)                                                            
      DO 81105  II = 1, NRZ                                                     
      N2 = N1 + II                                                              
      N3 = N2 + NRZ                                                             
      D1 = SCONST(N3) ** 2 + OMEGS1 ** 2                                        
      CZIRE = SCONST(N2) * SCONST(N3) / D1                                      
      CZIIM = - SCONST(N2) * OMEGS1 / D1                                        
      NN5 = N1 + 5*NRZ + 5*NRA + 4 + II                                 M31.2875
      SCONST(NN5) = CSL * CZIRE - CSLI * CZIIM                                  
      N6 = NN5 + NRZ                                                    M31.2877
      SCONST(N6) = CSR * CZIRE - CSRI * CZIIM                                   
81105 CONTINUE                                                                  
      IF (IPRSUP .LT. 1)  GO TO 81108                                   M31.2880
      WRITE (LUNIT6, 81106) I                                           M31.2881
81106 FORMAT (/,53H INITIALIZATION OF HISTORY VECTORS OF MARTI'S BRANCH M31.2882
     1 , I4, 16H  NODE K, NODE M, 24H PARTIAL EQUIV. VOLTAGES)          M31.2883
      J1 = NN5 - NRZ + 1                                                        
      J2 = N6 - NRZ + 1                                                         
      WRITE (LUNIT6, 81107) (SCONST(II), II=J1,NN5)                             
81107 FORMAT ( 1X, 10E12.5)                                             M31.2887
      WRITE (LUNIT6, 81107) (SCONST(II), II=J2, N6)                             
C     FORWARD PERTURBATION FUNCTIONS  (F)                               M31.2889
81108 CKREAL = VSL + CNVHST(N4+1) * CSL - CNVHST(N4+2) * CSLI                   
      CKIMAG =VSLI + CNVHST(N4+1) *CSLI + CNVHST(N4+2) * CSL                    
      ABSFK = SQRTZ(CKREAL**2 + CKIMAG**2)                                      
      PHFK = FUNP13 (CKIMAG, CKREAL, TWOPI)                                     
      CMREAL = VSR + CNVHST(N4+1) * CSR - CNVHST(N4+2) * CSRI                   
      CMIMAG =VSRI + CNVHST(N4+1) *CSRI + CNVHST(N4+2) * CSR                    
      ABSFM = SQRTZ(CMREAL**2 + CMIMAG**2)                                      
      PHFM = FUNP13( CMIMAG, CMREAL, TWOPI)                                     
      NRF = CNVHST(N4)/DELTAT + 2.                                              
      NR(K) = ILINE + 1                                                         
      ILINE = ILINE + 1 + NRF                                                   
      IF ( ILINE .LE. LPAST)  GO TO 8109                                M32.4313
      IPRINT = 8                                                        M32.4314
      LSTAT(19) = 8109                                                  M32.4315
      GO TO 9000                                                        M32.4316
 8109 DO 81110  II = 1, NRF                                                     
      IX = NR(K) + II - 1                                                       
      XK(IX) = ABSFK * COSZ( (-NRF+II)*OMEGS1*DELTAT+PHFK )             M31.2905
      XM(IX) = ABSFM * COSZ( (-NRF+II)*OMEGS1*DELTAT+PHFM )             M31.2906
81110 CONTINUE                                                                  
      IF ( IPRSUP .LT. 1)  GO TO 81113                                  M31.2908
      J1 = IX - NRF + 1                                                         
      WRITE (LUNIT6, 81112) J1, IX                                              
81112 FORMAT (/, 39H FORWARD FUNCTIONS XK AND XM; POSITIONS,  I6,       M31.2911
     1 4H  TO, I6)                                                      M31.2912
      WRITE (LUNIT6, 81107) (XK(II), II=J1,IX), (XM(II),II=J1,IX)       M31.2913
C     HISTORY OF PARTIAL EQUIVALENT CURRENT SOURCES  (B)                M31.2914
81113 D9 = -OMEGS1 * CNVHST(N4)                                                 
      C2R = COSZ(D9)                                                            
      C2I = SINZ(D9)                                                            
      DO 81115  II = 1, NRA                                                     
      N2 = N1 + 2*NRZ + II                                                      
      N3 = N2 + NRA                                                             
      D1 = SCONST(N3) ** 2 + OMEGS1 ** 2                                        
      C3R = SCONST(N2) * SCONST(N3) / D1                                        
      C3I =-SCONST(N2) * OMEGS1 / D1                                            
      N7 = N1 + 7*NRZ + 5*NRA + 4 + II                                          
      D2 = C2R * C3R - C2I * C3I                                        M31.2925
      D3 = C2R * C3I + C2I * C3R                                        M31.2926
      SCONST(N7) = D2 * CMREAL - D3 * CMIMAG                                    
      N8 = N7 + NRA                                                             
      SCONST(N8) = D2 * CKREAL - D3 * CKIMAG                                    
81115 CONTINUE                                                                  
      IF (IPRSUP .LT. 1)  GO TO 63593                                   M31.2931
      J1 = N7 - NRA + 1                                                 M31.2932
      J2 = N8 - NRA + 1                                                 M31.2933
      WRITE (LUNIT6, 81120)                                             M31.2934
81120 FORMAT (/, 31H PARTIAL EQUIV. CURRENT SOURCES)                    M31.2935
      WRITE (LUNIT6, 81107) (SCONST(II),II=J1,N7),                              
     1                      (SCONST(II),II=J2,N8)                               
63593 K = K + 1                                                                 
      ITADD = ITADD + 1                                                         
63594 CONTINUE                                                                  
      ITADD = ITADD + IT2 * (IT2 - 1) / 2                               M31.2941
      IT1 = 0                                                           M31.2942
      K = K - 1                                                         M31.2943
      IF ( ISTEAD  .GT.  0 )  GO TO  547                                M35.1358
      GO TO  544                                                        M35.1359
C                                                                       M44.1466
C        USER-SUPPLIED D.C. CONDITIONS                                  M31.2946
C        SINGLE PHASE LINES                                             M31.2947
60005 N1=IABS(KBUS(K))                                                  M31.2948
      N2=IABS(MBUS(K))                                                  M31.2949
      TR(ITADD) = CI1                                                           
      TX(ITADD) = 0.0                                                   M31.2951
      R(ITADD) = A                                                              
      C(ITADD) = 0.0                                                    M31.2953
      F(N1) = 0.0                                                       M31.2954
      F(N2) = 0.0                                                       M31.2955
      OMEGS1 = 0.0                                                      M31.2956
      IF ( N4 .GE. 0 )  GO TO 61594                                             
C        MULTIPHASE LINES                                               M31.2958
60010 IX = ITADD                                                        M31.2959
      DO 60011 I1=2,IT2                                                 M31.2960
      II=K+I1-1                                                         M31.2961
      IF(NR(II).LT.0) IT1=1                                             M31.2962
      L=-KBUS(II)                                                       M31.2963
      M=MBUS(II)                                                        M31.2964
C     READ INPUT CARD USING CIMAGE                                      M31.2965
      CALL CIMAGE                                                       M31.2966
      DECODE ( 80, 17020, ABUFF ) N3,BUS1,BUS2,H1,H2,CI1,CK1            M31.2967
17020 FORMAT (I2, 2A6,4E15.8)                                           M31.2968
17021 FORMAT (1H , A6, 1X, A6, 4E13.5)                                  M31.2969
      WRITE(LUNIT6,17021) BUS1,BUS2,H1,H2,CI1,CK1                       M31.2970
      IF(BUS(L).NE.BUS1.OR.BUS(M).NE.BUS2)                                      
     1 CALL STOPTP                                                              
      IX=IX+1                                                           M31.2972
      TR(IX) = H1                                                               
      TX(IX) = 0.0                                                      M31.2974
      R(IX) = CI1                                                               
      C(IX) = 0.0                                                       M31.2976
      F(L) = 0.0                                                        M31.2977
      F(M) = 0.0                                                        M31.2978
60011 CONTINUE                                                          M31.2979
      MARTI = 1                                                         M35.1360
      GO TO 61594                                                               
  578 CURR1=CI1                                                            11287
      CURI1=CK1                                                                 
      CURR2=A                                                              11289
      CURI2=D2                                                                  
c                                                                       M44.1467
c                         Meyer_line with groung wire FD goes through heM44.1468
c                                                                       M44.1469
  579 VR1=E(N1)                                                                 
      VI1=F(N1)                                                                 
      VR2=E(N2)                                                            11293
      VI2=F(N2)                                                                 
  580 IT2=IABSZ(N4)                                                             
      YX = IT2                                                             11346
      GUS3=VR1                                                                  
      BI3=VI1                                                                   
      GUS4=VR2                                                                  
      BI4=VI2                                                                   
      VSL=CURR1                                                                 
      VSLI=CURI1                                                                
      VSR=CURR2                                                                 
      VSRI=CURI2                                                                
      IT1=0                                                                     
C     TRANSFORMATION OF INITIAL CONDITIONS FROM PHASE TO MODAL          M29.1740
C     QUANTITIES                                                        M29.1741
      NL = LITYPE(K)                                                            
      DO 34560  I=1, IT2                                                        
      N7 = K + I - 1                                                            
      L = KBUS(N7)                                                      M16.2564
      L = IABS(L)                                                               
      M = MBUS(N7)                                                              
      II = ITADD + I - 2                                                        
      IF ( IPRSUP  .GE.  2 )                                            M11. 261
     1WRITE(LUNIT6, 34572)  I, L, M, K, YX, CI(K), E(L), E(M),          M22.4463
     2  TR(II), R(II)                                                   M22.4464
34572 FORMAT ( /,  16H NEXT CONDUCTOR.,                                 M22.4465
     1 32H       I       L       M       K   ,/,  16X,  4I8  ,/,        M22.4466
     2 16X,  14X,  2HYX,  11X,  5HCI(K),  12X,  4HE(L),                 M22.4467
     3 12X,  4HE(M),  10X,  6HTR(II),  11X,  5HR(II)  ,/,               M22.4468
     4 16X,  6E16.7  )                                                  M22.4469
34560 CONTINUE                                                                  
  591 IPRINT=3                                                          M29.1743
      ITADD=ISD                                                         M29.1744
      NL1=NL+IT2*IT2-1                                                  M29.1745
      DO 593 I=1,IT2                                                            
      II=K+I-1                                                          M29.1747
      IF(NR(II).LT.0)IT1=1                                              M29.1748
      L=-KBUS(II)                                                       M29.1749
      M=MBUS(II)                                                        M29.1750
      IF(ISTEAD.GT.0)GO TO 594                                                  
      IF(I.EQ.1)GO TO 599                                               M29.1752
C     READ INPUT CARD USING CIMAGE                                      M29.1753
      CALL CIMAGE                                                       M29.1754
      DECODE (80, 7020, ABUFF) N3,BUS1,BUS2,CI1,CK1,A,D2                M29.1755
      WRITE (KUNIT6, 7021)                                              M35.1361
 7021 FORMAT (32H+STEADY STATE INITIAL CONDITIONS)                      M29.1757
      IF(BUS(L).NE.BUS1 .OR. BUS(M).NE.BUS2)GO TO 9020                          
  599 IF(ITADD.GT.LDATA)GO TO 9000                                              
      TR(ITADD)=CI1                                                     M29.1760
      TX(ITADD)=CK1                                                             
      R(ITADD)=A                                                        M29.1762
      C(ITADD)=D2                                                               
  594 ITADD=ITADD+1                                                             
      A=CI(II)                                                                  
      D2=ABSZ( CK(II) )                                                 M30. 311
      IF(A.LT.0.) A=-A*D2                                               M29.1767
      D2=D2/YX                                                          M29.1768
      VOLTI(I)=-A/D2                                                    M29.1769
      EKREAL(I)=E(M)                                                    M29.1770
      EKIMAG(I)=F(M)                                                    M29.1771
      EMREAL(I)=E(L)                                                    M29.1772
      EMIMAG(I)=F(L)                                                    M29.1773
  593 CONTINUE                                                          M29.1774
      IF ( IPRSUP .LT. 1 ) GO TO 1594                                   M29.1775
      N2 = ITADD - 1                                                    M29.1776
      WRITE (LUNIT6, 1593) NL,ISD,N2,IT2                                M29.1777
 1593 FORMAT (32H NL, ISD, N2 AND IT2 AT 1593 ARE, 4I10 )               M29.1778
      WRITE (LUNIT6, 2593) (TR(I),TX(I),R(I),C(I), I=ISD,N2)            M29.1779
 2593 FORMAT ( 41H (TR(I), TX(I), R(I), C(I), I=ISD,N2) ARE,/,          M29.1780
     1  (1X, 8E15.6) )                                                  M29.1781
      WRITE (LUNIT6, 3593) (EKREAL(I),EKIMAG(I),EMREAL(I),EMIMAG(I)     M29.1782
     1, I=1, IT2)                                                       M29.1783
 3593 FORMAT ( 58H (EKREAL(I), EKIMAG(I), EMREAL(I), EMIMAG(I), I=1,IT2)M29.1784
     1 ARE, /, (1X,8E15.6) )                                            M29.1785
 1594 DO 11594  JJ = 1, IT2                                             M30. 312
      II = K + JJ - 1                                                   M30. 313
      IF ( CK(II) .LT. 0.0 )  GO TO 21594                               M30. 314
11594 CONTINUE                                                          M30. 315
      GO TO 4594                                                        M30. 316
21594 N9 = IT2 * IT2                                                    M30. 317
      J = NL -1                                                         M30. 318
      DO 2594  I = 1, N9                                                        
      J = J + 1                                                         M30. 320
 2594 VOLT(I) =  QFD(J)                                                 M35.1362
      IF (IPRSUP .GE. 1) WRITE (LUNIT6, 3438) (VOLT(L), L=1, N9)        M35.1363
      CALL MOVER0 ( VOLTK(1), N9 )                                      M35.1364
      N10 = 1                                                           M35.1365
      N12 = IT2 + 1                                                     M35.1366
      DO 8465  L=1, IT2                                                         
      VOLTK(N10) = 1.0                                                  M35.1368
 8465 N10 = N10 + N12                                                   M35.1369
      IF (IPRSUP .GE. 1) WRITE (LUNIT6, 3467) (VOLTK(L), L=1, N9)       M35.1370
 3467 FORMAT ( ' UNIT MAXTRIX.  VOLTK(1:N9) ...',/, (1X,5E25.16))
      CALL DGELG ( VOLTK(1), VOLT(1), IT2, IT2, EPSILN, IER )           M35.1371
      IF ( IPRSUP  .GE.  1 )                                            M35.1372
     1 WRITE (LUNIT6, 3471)  IER,  ( VOLTK(M), M=1, N9 )                        
      N5 = NL - 1                                                       M30. 325
      N7 = ISD                                                          M30. 326
      DO  3594   I = 1, IT2                                             M30. 327
      VSL = 0.0                                                         M30. 328
      VSR = 0.0                                                         M30. 329
      CSL = 0.0                                                         M30. 330
      CSR = 0.0                                                         M30. 331
      DO 2595  J = 1, IT2                                               M30. 332
      N5 = N5 + 1                                                       M30. 333
      H1 = QFD(N5)                                                      M31.2985
      VSL = VSL + H1 * EKREAL(J)                                                
      VSR = VSR + H1 * EMREAL(J)                                        M30. 336
      N6 = I + (J-1) * IT2                                              M35.1374
      H2 = VOLTK(N6)                                                    M35.1375
      CSL = CSL + H2 * R(N7)                                                    
      CSR = CSR + H2 * TR(N7)                                           M30. 340
      N7 = N7 + 1                                                       M30. 341
 2595 CONTINUE                                                          M30. 342
      CLOSEV(I) = VSR                                                           
      FARV(I) = VSL                                                             
      CLOSEI(I) = CSR                                                           
      FARI(I) = CSL                                                             
      N7 = ISD                                                          M30. 347
 3594 CONTINUE                                                          M30. 348
C**** COMPLEX VOLTAGE & CURRENT VECTORS ARE STORED IN EKREAL,EKIMAG,    M29.1786
C     EMREAL,EMIMAG AND TR,TX,R,C                                       M29.1787
 4594 N6=ISD-1                                                          M30. 349
      DO 595 I=1,IT2                                                    M29.1789
      DO 595 J=1,I                                                      M29.1790
      GUS1=0.                                                           M29.1791
      N5=NL-1                                                           M29.1792
      DO 596 IP=1,IT2                                                   M29.1793
      JIP=N5+J                                                          M29.1794
      IIP=N5+I                                                          M29.1795
      GUS1=GUS1+QFD(JIP)*VOLTI(IP)*QFD(IIP)                             M31.2988
  596 N5=N5+IT2                                                         M29.1797
      N6=N6+1                                                           M29.1798
  595 X(N6)=GUS1                                                        M29.1799
      IF (IPRSUP .GE. 3)                                                M29.1800
     1 WRITE (LUNIT6,1595) ISD, N6, (X(I),I=ISD,N6)                     M29.1801
 1595 FORMAT ( 23H AT 1595 X(I) FROM I = , I8, 3X, 3HTO ,I8,            M29.1802
     1           3X, 3HARE,/, (1X,8E15.6) )                             M29.1803
       ISD1=ISD+IT2*(IT2+1)/2-1                                         M29.1804
       LL0 = 0                                                          M29.1805
      CALL REDU13(X(ISD),IT2,LL0)                                       M29.1806
      LL1 = 1                                                           M29.1807
      CALL MULT(X(ISD),TR(ISD),EMREAL,IT2,LL1)                          M29.1808
      CALL MULT(X(ISD),TX(ISD),EMIMAG,IT2,LL1)                          M29.1809
      CALL MULT(X(ISD),R(ISD),EKREAL,IT2,LL1)                           M29.1810
      CALL MULT(X(ISD),C(ISD),EKIMAG,IT2,LL1)                           M29.1811
C**** Y=AX+Y IS OBTAINED FOR SIDE M,K IN REAL AND IMAG                  M29.1812
      N5=NL-1                                                           M29.1813
      DO 597 I=1,IT2                                                    M29.1814
      VSL=0.                                                            M29.1815
      VSLI=0.                                                           M29.1816
      VSR=0.                                                            M29.1817
      VSRI=0.                                                           M29.1818
      II=K+I-1                                                          M29.1819
      A=CI(II)                                                          M29.1820
      IF(A.LT.0.)A=-A*CK(II)                                            M29.1821
      A=A*YX                                                            M29.1822
      DO 598 J=1,IT2                                                    M29.1823
      N5=N5+1                                                           M29.1824
      H1=QFD(N5)                                                        M31.2989
      VSL=VSL+H1*EKREAL(J)                                              M29.1826
      VSLI=VSLI+H1*EKIMAG(J)                                            M29.1827
      VSR=VSR+H1*EMREAL(J)                                              M29.1828
  598 VSRI=VSRI+H1*EMIMAG(J)                                            M29.1829
      VOLTI(I)=SQRTZ(VSL*VSL+VSLI*VSLI)*A                               M29.1830
      IF(VOLTI(I).EQ.0.)VSL=1.                                          M29.1831
      VOLT(I)=ATAN2Z(VSLI,VSL)                                          M29.1832
      VOLTK(I)=SQRTZ(VSR*VSR+VSRI*VSRI)*A                               M29.1833
      IF(VOLTK(I).EQ.0.)VSR=1.                                          M29.1834
      NDX1 = LSIZ26 + I                                                 M31.2990
  597 VIM(NDX1) = ATAN2Z(VSRI,VSR)                                      M31.2991
C *** END OF TRANSFORMATION FOR INITIAL CONDITIONS                      M29.1836
  592 II=LENGTH(K+1)                                                            
      IF (LENGTH(K) .GT. 0 )   II = LENGTH(K)                           M30. 350
      ITADD=ITADD+IT2*(IT2-1)/2                                         M29.1838
      IF (ISTEAD.LE.0)ITADD=IT+1                                        M29.1839
      IF ( IPRSUP .GE. 3 )                                              M29.1840
     1 WRITE (LUNIT6, 1592) (VOLTI(I),VOLT(I),VOLTK(I), I=1,IT2)        M31.2992
 1592 FORMAT(54H (VOLTI(I),VOLT(I), VOLTK(I),             I=1,IT2) ARE, M31.2993
     1  /, ( 1X, 6E20.10 )  )                                           M31.2994
      NDX1 = LSIZ26 + 1                                                 M31.2995
      NDX2 = LSIZ26 + IT2                                               M31.2996
      IF ( IPRSUP  .GE.  3 )                                            M31.2997
     1 WRITE (LUNIT6, 21592)  ( VIM(I), I=NDX1, NDX2 )                  M31.2998
21592 FORMAT ( 15H VIM(LSIZ26+I):,  6E19.10  )                          M31.2999
      K=K-1                                                                     
      I = 0                                                             M11. 263
 5019 I = I + 1                                                         M11. 264
      K=K+1                                                                11534
      H1=VOLTI(I)                                                               
      H2=VOLTK(I)                                                               
      CI1=VOLT(I)                                                               
      NDX1 = LSIZ26 + I                                                 M31.3000
      CK1 = VIM(NDX1)                                                           
      N4=NR(K)+1                                                                
      N2=II+N4                                                                  
      GUS1 = - N4                                                               
      GUS1 = GUS1 * STEADY                                                      
      GUS4=GUS1+(1.0-CIK(K))*STEADY                                             
      DO 582 N1=II,N2                                                           
      A=H1*COSZ(GUS1+CI1)                                                       
      D2=H2*COSZ(GUS1+CK1)                                                      
      GUS1=GUS1+STEADY                                                          
      XK(N1)=A                                                                  
  582 XM(N1)=D2                                                                 
      IF( IPRSUP .GE. 2 )                                                  11550
     1WRITE(LUNIT6, 44582)  K, KBUS(K), MBUS(K), II, N2, IT2,           M17. 711
     1 ( XK(N1), XM(N1), N1=II, N2 )                                       11552
44582 FORMAT ( /,                                                       M22.4509
     1  58H ONE MODE PAST HISTORY  (XK(J), XM(J), J=II, N2)  FOLLOWS.,  M22.4510
     2  48H       K KBUS(K) MBUS(K)      II      N2     IT2   ,/,       M22.4511
     3  58X,  6I8  ,/,  ( 1X,  8E16.7 )  )                              M22.4512
      IF( IPRSUP .GE. 1 )                                                  11555
     1WRITE(LUNIT6, 44583)  H1, H2, CI1, CK1, STEADY                    M17. 712
44583 FORMAT (  24H PHASOR QUANTITIES USED.,  14X,  2HH1,               M22.4513
     1 14X,  2HH2,  13X,  3HCI1,  13X,  3HCK1,  10X,  6HSTEADY  ,/,     M22.4514
     2 24X,  5E16.7  )                                                  M22.4515
      IF (CK(K).LT.0) GO TO 20750                                               
      A=H1*COSZ(GUS4+CI1)                                               M18. 748
      D2=H2*COSZ(GUS4+CK1)                                              M18. 749
      IF(CI(K).LT.0.) GO TO 584                                            11561
      CI1=(CK(K)+1.0) / 2.0                                             M05. 588
      CK1=1.0-CI1                                                          11563
      XK(N2)=CI1*A+CK1*D2                                                       
      XM(N2)=CI1*D2+CK1*A                                                  11565
      GO TO 583                                                            11566
20750 KF = CIK(K)                                                          11567
      IFDEP2 = KF + KF - 1                                                 11568
      A = 0.0                                                              11569
      D7 = H2                                                              11570
      D8 = CK1                                                             11571
      D9 = H1                                                              11572
      D10 = CI1                                                            11573
20755 D3 = CON1(IFDEP2)                                                 M35.1376
      D4 = CON1(IFDEP2 + 2 * LFDEP)                                     M35.1377
      D5 = CON1(IFDEP2 + 4 * LFDEP)                                     M35.1378
      IFTAIL = IFTAIL + 1                                                  11577
      IF( IFTAIL .LE. LTAILS )  GO TO 20762                                11578
      IPRINT = 15                                                          11579
      LSTAT(19)=20762                                                      11580
      GO TO 9000                                                           11581
20762 D6 = OMEGA * D5                                                      11582
      D2  =  D3  / ( D4*D4 + OMEGA*OMEGA )                                 11583
      D3 = D2 * OMEGA                                                      11584
      D4 = D2 * D4                                                         11585
      D8 = D8 - D6                                                         11586
      D10 = D10 - D6                                                       11587
      STAILK(IFTAIL) = D7 * ( D4*COSZ(D8)  +  D3*SINZ(D8)  )            M18. 750
      STAILM(IFTAIL) = D9 * (  D4*COSZ(D10)  +  D3*SINZ(D10)  )         M18. 751
      IF( IPRSUP .GE. 2 )                                                  11590
     1WRITE(LUNIT6, 20767)  IFDEP2, IFTAIL, K,          STAILK(IFTAIL), M17. 713
     1 STAILM(IFTAIL), D2, D3, D4, D5, D6, D7, D8, D9, D10                 11592
20767 FORMAT ( /,  19H EXP. TAIL HISTORY.,                              M22.4516
     1 24H  IFDEP2  IFTAIL       K,  6X,  14HSTAILK(IFTAIL),            M22.4517
     2 6X,  14HSTAILM(IFTAIL),  14X,  2HD2,  14X,  2HD3  ,/,            M22.4518
     3 19X,  3I8,  2E20.11,  2E16.7  ,/,  19X,  14X,  2HD4,             M22.4519
     4 14X,  2HD5,  14X,  2HD6,  14X,  2HD7,  14X,  2HD8,               M22.4520
     5 14X,  2HD9,  13X,  3HD10  ,/,  19X,  7E16.7  )                   M22.4521
      IF( A .GT. 0.0 )  GO TO 20775                                        11595
      A = 1.0                                                              11596
      D7 = H1                                                              11597
      D8 = CI1                                                             11598
      D9 = H2                                                              11599
      D10 = CK1                                                            11600
      IFDEP2 = IFDEP2 + 1                                                  11601
      GO TO 20755                                                          11602
C      M22.452220775 IF ( IT2  .EQ.  1 )   GO TO 20700                  M30. 351
20775 D7 = CI(K) * YX / ETA(KF)                                         M30. 352
      A = D7 * CLOSEV(I) - CLOSEI(I)                                    M30. 353
      D2 = D7 * FARV(I)  -  FARI(I)                                     M30. 354
      IF( IPRSUP .GE. 2 )                                                  11607
     1WRITE(LUNIT6, 20776)  K, I, IFDEP2, IFTAIL,                       M17. 714
     1 CLOSEV(I), FARV(I), CLOSEI(I), FARI(I), CI(K), A, D2                11609
20776 FORMAT ( /,  31H FREQ. DEP. LAST HISTORY POINT.,                  M22.4523
     1 32H       K       I  IFDEP2  IFTAIL,  7X,  9HCLOSEV(I),          M22.4524
     2 9X,  7HFARV(I),  7X,  9HCLOSEI(I)  ,/,  31X,                     M22.4525
     3 4I8,  3E16.7  ,/,  31X,  9X,  7HFARI(I),  11X,  5HCI(K),         M22.4526
     4 15X,  1HA,  14X,  2HD2  ,/,  31X,  4E16.7  )                     M22.4527
  584 XK(N2)=A                                                             11611
      XM(N2)=D2                                                            11612
  583 II=N2+1                                                              11613
      IF ( I  .LT.  IT2 )   GO TO 5019                                          
      IF(ISTEAD.GT.0) GO TO 547                                            11614
      GO TO 544                                                            11615
 5750 IT2 = IABS(KODEBR(K))                                                     
      N11 = K + IT2 - 1                                                         
      IF (ISTEAD .GT. 0) GO TO 5862                                             
      ITADD = NR(K) - 1                                                 M23.2528
      IT = ITADD                                                        M23.2529
C     READ INPUT CARD USING CIMAGE                                      M21. 174
      CALL CIMAGE                                                       M15.1077
      DO 5755 I=1, IT2                                                  M15.1078
      II = K + I - 1                                                    M16.2754
      ITADD = ITADD + 1                                                 M15.1079
      IF (I .NE. 1) GO TO 5763                                          M15.1080
      TR(ITADD) = CI1                                                   M15.1081
      TX(ITADD) = CK1                                                   M15.1082
      R(ITADD) = A                                                      M15.1083
      C(ITADD) = D2                                                     M15.1084
      GO TO 5759                                                        M15.1085
 5763 M = IABS(MBUS(II))                                                M20.3884
      L = IABS(KBUS(II))                                                M20.3885
      IF ( KOLBEG  .GT.  0 )   GO TO 4693                               M23.2530
      IF ( KOLBEG  .GT.  0 )   GO TO 4693                               M22.4529
      DECODE (80,7020,ABUFF) N3,BUS1,BUS2,TR(ITADD),TX(ITADD),          M15.1089
     1     R(ITADD),C(ITADD)                                            M15.1090
      GO TO 4695                                                        M22.4530
 4693 NFRFLD = 1                                                        M22.4531
      KOLBEG = 1                                                        M22.4532
      CALL FREONE ( D11 )                                               M29.1844
      N3 = D11                                                          M22.4534
      NRIGHT = -1                                                       M22.4535
      CALL FREONE ( D1 )                                                M29.1845
      BUS1 = TEXTA6(1)                                                  M22.4537
      CALL FREONE ( D1 )                                                M29.1846
      BUS2 = TEXTA6(1)                                                  M22.4539
      NRIGHT = 0                                                        M22.4540
      CALL FREFLD ( TR(ITADD) )                                         M22.4541
      CALL FREFLD ( TX(ITADD) )                                         M22.4542
      CALL FREFLD ( R(ITADD) )                                          M22.4543
      CALL FREFLD ( C(ITADD) )                                          M22.4544
 4695 IF ( NOUTPR  .EQ.  0 )                                            M22.4545
     1 WRITE (KUNIT6, 54159)  TR(ITADD),TX(ITADD),R(ITADD)              M35.1379
      IF (BUS1 .EQ. BUS(L) .AND. BUS2 .EQ. BUS(M) .AND. N3 .EQ. 3)      M15.1092
     1     GO TO 5767                                                   M15.1093
 5766 LSTAT(19) = 5766                                                  M15.1094
      KILL = 32                                                         M22.4546
      GO TO 9200                                                        M22.4547
C     READ INPUT CARD USING CIMAGE                                      M21. 175
 5767 CALL CIMAGE                                                       M15.1096
 5759 IF ( KOLBEG  .GT.  0 )   GO TO 4703                               M22.4548
      DECODE (80,7020,ABUFF) N3, BUS1, BUS2                             M15.1098
      GO TO 4705                                                        M22.4549
 4703 NFRFLD = 1                                                        M22.4550
      CALL FREONE ( D11 )                                               M29.1847
      N3 = D11                                                          M22.4552
      NRIGHT = -1                                                       M22.4553
      CALL FREONE ( D1 )                                                M29.1848
      BUS1 = TEXTA6(1)                                                  M22.4555
      CALL FREONE ( D1 )                                                M29.1849
      BUS2 = TEXTA6(1)                                                  M22.4557
      NRIGHT = 0                                                        M22.4558
 4705 IF ( BUS1  .NE.  TEXT1 )   GO TO 5755                             M22.4559
      IF (BUS2 .NE. TEXT2) GO TO 5755                                   M15.1100
      IF (N3 .NE. I) GO TO 5766                                         M15.1101
      DECODE (80,5764,ABUFF) CI1, CK1, A, D2                            M15.1102
 5764 FORMAT(14X,4E15.8)                                                M15.1103
      WRITE (KUNIT6, 5761) CI1, CK1, D2                                 M35.1380
 5761 FORMAT(  17H+MODAL Z AND Y.  ,  3E11.4  )                         M22.4561
C     READ INPUT CARD USING CIMAGE                                      M22.4562
      CALL CIMAGE                                                       M22.4563
      N3 = IABS(INDHST(II))                                             M23.2531
      IF (CI(II) .GT. 0.0) N3 = N3 + 1                                  M23.2532
      CNVHST(N3 + 0) = CI1                                              M23.2533
      CNVHST(N3 + 1) = CK1                                              M23.2534
      CNVHST(N3 + 2) = A                                                M23.2535
      CNVHST(N3 + 3) = D2                                               M23.2536
      CNVHST(N3 + 4) = STEADY / DELTAT                                  M23.2537
 5755 CONTINUE                                                          M15.1114
      ITADD = IT + 1                                                    M15.1115
 5862 DO 5808 I=K, N11                                                          
      IF (INDHST(I) .GT. 0) INDHST(I) = - INDHST(I)                     M23.2539
 5808 CONTINUE                                                          M23.2540
      ITADD = ITADD + IT2 * (IT2 + 1) / 2                               M23.2541
 5809 IF ( CKI(K) .LT. 0.0) GO TO 5810                                  M23.2542
      K = K + 1                                                         M23.2543
      GO TO 5809                                                        M23.2544
 5810 IF (ISTEAD .EQ. 0) GO TO 5758                                     M23.2545
      GO TO 547                                                         M23.2546
C     INITIAL CONDITION INPUT FOR EMTP SWITCH COMPONENT.                M20.3888
 5600 IF ( KOLBEG  .GT.  0 )   GO TO 4713                               M22.4565
      DECODE (80,5610,ABUFF) BUS1, BUS2, N1, N2, N3, D1, D2, D3, D4     M20.3890
 5610 FORMAT(2X,2A6,3I4,4E13.6)                                         M20.3891
      GO TO 4715                                                        M22.4566
 4713 NFRFLD = 1                                                        M22.4567
      NRIGHT = -1                                                       M22.4568
      CALL FREONE ( D1 )                                                M29.1850
      BUS1 = TEXTA6(1)                                                  M22.4570
      CALL FREONE ( D1 )                                                M29.1851
      BUS2 = TEXTA6(1)                                                  M22.4572
      NRIGHT = 0                                                        M22.4573
      CALL FREONE ( D11 )                                               M29.1852
      N1 = D11                                                          M22.4575
      CALL FREONE ( D11 )                                               M29.1853
      N2 = D11                                                          M22.4577
      CALL FREONE ( D11 )                                               M29.1854
      N3 = D11                                                          M22.4579
      CALL FREONE ( D1 )                                                M29.1855
      CALL FREONE ( D2 )                                                M29.1856
      CALL FREONE ( D3 )                                                M29.1857
      CALL FREONE ( D4 )                                                M29.1858
 4715 IF ( NOUTPR  .EQ.  0 )                                            M22.4584
     1     WRITE (KUNIT6, 5620)  N1, N2, N3, D1                         M35.1381
 5620 FORMAT ( 19H+SWITCH INIT. COND.,  3I6,  E13.4  )                  M20.3894
      DO 5630 K=1, KSWTCH                                               M20.3895
      L = IABS(KMSWIT(K))                                               M32.4319
      NDX1 = LSWTCH + K                                                 M32.4320
      M = IABS(KMSWIT(NDX1))                                            M32.4321
      IF (BUS1 .NE. BUS(L)) GO TO 5630                                  M20.3898
      IF (BUS2 .EQ. BUS(M)) GO TO 5640                                  M20.3899
 5630 CONTINUE                                                          M20.3900
      KILL = 31                                                         M20.3901
      LSTAT(19) = 5630                                                  M20.3902
      GO TO 9200                                                        M20.3903
 5640 TCLOSE(K) = D1                                                    M20.3904
      IFLAG = 0                                                         M23.2547
      ADELAY(K) = D2                                                    M20.3905
      ENERGY(K) = D3                                                    M20.3906
      CRIT(K) = D4                                                      M20.3907
      KPOS(K) = N1                                                      M20.3908
      KODE(L) = N2                                                      M32.4322
      KODE(M) = N3                                                      M32.4323
C     READ INPUT CARD USING CIMAGE                                      M32.4324
      CALL CIMAGE                                                       M32.4325
      DECODE (80, 5656, ABUFF) NEXTSW(K)                                M35.1382
 5656 FORMAT ( 14X, I4 )                                                M35.1383
      IF (NOUTPR .EQ. 0)                                                M32.4327
     1 WRITE (KUNIT6, 5643)  NEXTSW(K)                                  M35.1384
 5643 FORMAT (22H+  CONTINUE.  NEXTSW = , I4)                           M35.1385
      GO TO 544                                                         M20.3911
C     TRANSFER CONTROL TO 2ND HALF OF OVERLAY 13, IN "LAST13".          M29.1859
  590 IF ( MARTI  .EQ.  1 )  GO TO 4590                                 M35.1386
      DO 3590 KJ = 1, IBR                                                       
      IF ( IMODEL(KJ)  .NE.  -2 )  GO TO 3590                           M35.1388
      N4 = INDHST(KJ)                                                           
      NRF = CNVHST(N4) / DELTAT + 2.                                            
      NR(KJ) = ILINE + 1                                                        
      ILINE = ILINE + 1 + NRF                                                   
      IF ( ILINE  .LE.  LPAST )  GO TO  3590                            M35.1393
      IPRINT = 8                                                        M35.1394
      LSTAT(19) = 3590                                                  M35.1395
      GO TO 9000                                                        M35.1396
 3590 CONTINUE                                                          M35.1397
 4590 CALL LAST13                                                       M35.1398
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M29.1861
      LASTOV = NCHAIN                                                   M29.1862
      NCHAIN = NCHAIN + 1                                               M29.1863
      GO TO 9800                                                        M32.4330
 9020 WRITE (LUNIT6, 9021)                                              M29.1865
 9021 FORMAT(55H ERROR IN IDENTIFICATION OF NEXT BRANCH. JOB TERMINATED)M29.1866
      GO TO 9200                                                        M29.1867
 9000 LSTAT(16) = IPRINT                                                M29.1868
      KILL = 1                                                          M29.1869
 9200 LASTOV = NCHAIN                                                   M29.1870
      NCHAIN = 51                                                       M29.1871
      LSTAT(18) = LASTOV                                                M29.1872
 9800 IF ( IPRSUP  .GE.  1 )                                            M32.4331
     1 WRITE (LUNIT6, 9806)                                             M32.4332
 9806 FORMAT ( 23H EXIT MODULE  "OVER13".  )                            M32.4333
99999 RETURN                                                            M29.1873
      END                                                               M29.1874
      SUBROUTINE FDINT( IKF, ISFD, IBF, OMG )                           M32.4334
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION  UR(40), UI(40)                                         M32.4337
C     THIS ROUTINE INITIALIZES A)CAPACITOR VOLTAGES, B)BRANCH CURRENTS**M32.4338
      IDK = 2 * IKF                                                     M32.4339
      IKF = IKF + 1                                                     M32.4340
      ISC = IBF + 1                                                     M32.4341
      IF( IPRSUP .GT. 0 )                                               M32.4342
     1    WRITE( LUNIT6, 1 ) IKF, ISFD, IBF, IMFD(IDK+1), IMFD(IDK+2)   M32.4343
    1 FORMAT(41H INTEGER COUNTERS AT START OF FDINT......,7X,3HIKF,     M32.4344
     16X,4HISFD,7X,3HIBF,6X,4HIZFD,6X,4HIPFD,/,41X,5I10)                M32.4345
C     CALCULATE MODAL VOLTAGES FROM THE PHASE VALUES   *   *   *   *   *M32.4346
      CZ =  IT2                                                         M34. 143
      CZ = 1.0 / CZ                                                     M34. 144
      UR(1) = VOLT(1)                                                   M32.4348
      UI(1) = VOLTK(1)                                                  M32.4349
      DO  2  KA = 2, IT2                                                M32.4350
         UR(1) = UR(1) + VOLT(KA)                                       M32.4351
         UI(1) = UI(1) + VOLTK(KA)                                      M32.4352
         UR(KA) = ( VOLT(1) - VOLT(KA) ) * CZ                           M32.4353
    2    UI(KA) = ( VOLTK(1) - VOLTK(KA) ) * CZ                         M32.4354
      AR = UR(1) * CZ                                                   M32.4355
      AI = UI(1) * CZ                                                   M32.4356
      IF( IPRSUP .GT. 0 )                                               M32.4357
     1    WRITE( LUNIT6, 3 ) IKF, AR, AI, ( UR(I), UI(I), I = 2, IT2 )  M32.4358
    3 FORMAT(1X,24HMODAL VOLTAGES (SET NO.),I5,2X,27HREAL AND IMAGINARY M33.1116
     1IN PAIRS,/,(2X,6E20.11))                                          M32.4360
C     INITIALIZE  BRANCH  QUANTITIES   *   *   *   *   *   *   *   *   *M32.4361
C     PROCESS FIRST THE ZERO SEQUENCE DATA *   *   *   *   *   *   *   *M32.4362
      IST = ISFD + 1                                                    M32.4363
      ISU = ISFD + IMFD(IDK+1) * 5                                      M32.4364
      ISV = IBF                                                         M32.4365
      DO  5  KA = IST, ISU, 5                                           M32.4366
         ISV = ISV + 1                                                  M32.4367
         AR1 = RMFD(KA)                                                 M32.4368
         AL1 = RMFD(KA+1) * OMG                                         M32.4369
         ARL = RMFD(KA+3)                                               M32.4370
         AC1 = RMFD(KA+2)                                               M32.4371
         IF( AC1 .GT. 0. ) AC1 = 1. / ( AC1 * OMG )                     M32.4372
         AZR = AR1                                                      M32.4373
         AZI = AL1                                                      M32.4374
         IF( ARL .EQ. 0. .OR. AL1 .EQ. 0. ) GO TO 4                     M32.4375
         DEN = 1.0 / ( ARL * ARL + AL1 * AL1 )                          M32.4376
         AZR = AZR + ARL * ( AL1 * AL1 ) * DEN                          M32.4377
         AZI = AL1 * ( ARL * ARL ) * DEN                                M32.4378
    4    AZI = AZI - AC1                                                M32.4379
C     INVERT BRANCH IMPEDANCE TO OBTAIN ADMITTANCE *   *   *   *   *   *M32.4380
         DEN = 1.0 / ( AZR * AZR + AZI * AZI )                          M32.4381
         AZR = AZR * DEN                                                M32.4382
         AZI = -AZI * DEN                                               M32.4383
C     CALCULATE AND STORE BRANCH CURRENT   *   *   *   *   *   *   *   *M32.4384
         AUR = AR * AZR - AI * AZI                                      M32.4385
         AUI = AR * AZI + AI * AZR                                      M32.4386
         CIKFD(ISV+1) = AUR                                             M32.4387
C     CALCULATE CAPACITOR VOLTAGE  *   *   *   *   *   *   *   *   *   *M32.4388
         CIKFD(ISV+2) = -AUI * AC1                                      M32.4389
         ISV = ISV + 2                                                  M32.4390
    5 CONTINUE                                                          M32.4391
      IBF = ISV                                                         M32.4392
      ISFD = ISU                                                        M32.4393
      IST = ISFD + 1                                                    M32.4394
      ISU = ISFD + IMFD(IDK+2) * 5                                      M32.4395
      ISK = IMFD(IDK+2) * 3                                             M32.4396
      ISV = ISV - 2                                                     M32.4397
C     PROCESS THE REMAINING MODES  *   *   *   *   *   *   *   *   *   *M32.4398
C     START LOOP ACROSS ALL MODAL BRANCHES *   *   *   *   *   *   *   *M32.4399
      DO  8  KA = IST, ISU, 5                                           M32.4400
         AR1 = RMFD(KA)                                                 M32.4401
         AL1 = RMFD(KA+1) * OMG                                         M32.4402
         ARL = RMFD(KA+3)                                               M32.4403
         AC1 = RMFD(KA+2)                                               M32.4404
         IF( AC1 .GT. 0. ) AC1 = 1. / ( AC1 * OMG )                     M32.4405
         AZI = AL1                                                      M32.4406
         AZR = AR1                                                      M32.4407
         IF( ARL .EQ. 0. .OR. AL1 .EQ. 0. ) GO TO 6                     M32.4408
         DEN = 1.0 / ( ARL * ARL + AL1 * AL1 )                          M32.4409
         AZR = AZR + ARL * ( AL1 * AL1 ) * DEN                          M32.4410
         AZI = AL1 * ( ARL * ARL ) * DEN                                M32.4411
    6    AZI = AZI - AC1                                                M32.4412
C     INVERT BRANCH IMPEDANCE TO OBTAIN ADMITTANCE *   *   *   *   *   *M32.4413
         DEN = 1.0 / ( AZR * AZR + AZI * AZI )                          M32.4414
         AZR = AZR * DEN                                                M32.4415
         AZI = -AZI * DEN                                               M32.4416
C     START THE INTERNAL LOOP ACROSS MODES *   *   *   *   *   *   *   *M32.4417
         ISV = ISV + 3                                                  M32.4418
         ISW = ISV                                                      M32.4419
         DO  7  KB = 2, IT2                                             M32.4420
C     CALCULATE AND STORE BRANCH CURRENT   *   *   *   *   *   *   *   *M32.4421
            AUR = UR(KB) * AZR - UI(KB) * AZI                           M32.4422
            AUI = UR(KB) * AZI + UI(KB) * AZR                           M32.4423
            CIKFD(ISW+1) = AUR                                          M32.4424
C     CALCULATE CAPACITOR VOLTAGE  *   *   *   *   *   *   *   *   *   *M32.4425
            CIKFD(ISW+2) = -AUI * AC1                                   M32.4426
    7       ISW = ISW + ISK                                             M32.4427
    8 CONTINUE                                                          M32.4428
      IBF = IBF + ( IT2 - 1 ) * ISK                                     M32.4429
      ISFD = ISU                                                        M32.4430
      IF( IPRSUP .GT. 0 )                                               M32.4431
     1    WRITE( LUNIT6, 9 ) ISC, IBF, ( CIKFD(KA), KA = ISC, IBF)      M32.4432
    9 FORMAT(17H ARRAY CIKFD FROM,I6,4H  TO,I6,/,(2X,6E21.11))          M32.4433
      RETURN                                                            M32.4434
      END                                                               M32.4435
      FUNCTION FUNP13(Y,X,TWOPI)                                        M31.3003
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      IF(X.NE.0.0) GO TO 101                                            M31.3004
      IF(Y.NE.0.0) GO TO 102                                            M31.3005
      FUNP13=0.0                                                        M31.3006
      GO TO 110                                                         M31.3007
102   IF(Y.GT.0.0) GO TO 103                                            M31.3008
      FUNP13 = - TWOPI / 4.                                             M31.3009
      GO TO 110                                                         M31.3010
103   FUNP13 = TWOPI / 4.                                               M31.3011
      GO TO 110                                                         M31.3012
101   IF(Y.NE.0.0) GO TO 105                                            M31.3013
      IF(X.GT.0.0) GO TO 104                                            M31.3014
      FUNP13 = TWOPI / 2.                                               M31.3015
      GO TO 110                                                         M31.3016
104   FUNP13=0.0                                                        M31.3017
      GO TO 110                                                         M31.3018
105   FUNP13=ATAN2Z(Y,X)                                                M31.3019
110   RETURN                                                            M31.3020
      END                                                               M31.3021
      SUBROUTINE REDINV(X1,M,N)                                         M30. 355
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION X1(1),A1(20)                                            M30. 356
C     GAUSS-JORDAN ELIMINATION PROCESS PERFORMED ON A SQUARE MATRIX X   M30. 357
C)    THIS ROUTINE CAN ALSO BE USED FOR MATRIX INVERSION * * * * * * * *M30. 358
C     MAXIMUM ORDER OF MATRIX X IS 20                                   M30. 359
      J=M                                                               M30. 360
      IK=M**2                                                           M30. 361
      NK=IK-M                                                           M30. 362
 1    C=X1(IK)                                                          M30. 363
      C=1.0/C                                                           M30. 364
      CALL MOVER ( X1(NK+1), A1(1), M )                                 M30. 365
      K=1                                                               M30. 366
    4 MK=(K-1)*M                                                        M30. 367
      N9 = MK + J                                                       M30. 368
      B = -X1(N9) * C                                                   M30. 369
      I=1                                                               M30. 370
 3    MK=MK+1                                                           M30. 371
      X1(MK)=X1(MK)+B*A1(I)                                             M30. 372
      I=I+1                                                             M30. 373
      IF(I.LE.M) GO TO 3                                                M30. 374
      N9 = MK + J - M                                                   M30. 375
      X1(N9) = B                                                        M30. 376
      K=K+1                                                             M30. 377
      IF(K.EQ.J) K=K+1                                                  M30. 378
      IF(K.LE.M) GO TO 4                                                M30. 379
      DO 5 K=1,M                                                        M30. 380
      N9 = NK + K                                                       M30. 381
    5 X1(N9) = A1(K) * C                                                M30. 382
      X1(IK)=C                                                          M30. 383
      J=J-1                                                             M30. 384
      IK=IK-M-1                                                         M30. 385
      NK=NK-M                                                           M30. 386
      IF(J.GT.N) GO TO 1                                                M30. 387
      RETURN                                                            M30. 388
      END                                                               M30. 389
      SUBROUTINE LAST13                                                 M31.3022
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION   TAILP(1), TAILQ(1), TAILT(1), VIM(1)                  M32.4436
      EQUIVALENCE    ( LSTAT(14), MDRIVE ),  ( VOLT(1), VIM(1) )        M35.1399
      DOUBLE PRECISION DBLPR1, DBLPR2, DBLPR3, DBLPR4                   M29.1884
      IF ( IPRSUP  .GE.  1 )                                            M32.4438
     1 WRITE ( LUNIT6, 4567 )                                           M32.4439
 4567 FORMAT ( 23H BEGIN MODULE "LAST13".  )                            M32.4440
      I = IBR + 1                                                       M29.1885
  610 I = I - 1                                                         M23.2549
  620 IF (I .LE. 0) GO TO 650                                           M23.2550
      IF (KBUS(I) .GE. 0 .OR. KODSEM(I) .EQ. 0) GO TO 610               M23.2551
      IF (IMODEL(I) .EQ. -2)  GO TO 610                                 M31.3025
      IF (IMODEL(I) .EQ. -4)  GO TO 610                                 M43.2014
      I = I - LENGTH(I) + 1                                             M23.2552
      N15 = IABS( KBUS(I) )                                             M31.3026
      IF ( N15  .EQ.  1 )   N15 = IABS( MBUS(I) )                       M31.3027
      N16 = IABS ( KSSFRQ(N15) )                                        M31.3028
      STEADY = TWOPI * SFREQ(N16) * DELTAT                              M31.3029
      IT2 = IABS(KODEBR(I))                                             M23.2553
      IF (IPRSUP .LT. 6) GO TO 14009                                    M23.2554
      K = I                                                             M23.2555
      N7 = 1                                                            M23.2556
14000 WRITE (LUNIT6, 14999) K, KBUS(K), MBUS(K), NR(K), LENGTH(K),      M23.2557
     1          KODSEM(K), INDHST(K), KODEBR(K), CI(K), CK(K),          M23.2558
     2          CIK(K), CKI(K)                                          M23.2559
      N1 = IABS(KODSEM(K))                                              M23.2560
      N2 = IABS(INDHST(K))                                              M23.2561
      N3 = N1 - 1                                                       M23.2562
      N4 = N2 - 1                                                       M23.2563
      N8 = N4                                                           M23.2564
      IF (N7 .LE. IABS(KODEBR(K))) N8 = N8 + 6                          M23.2565
      IF (KODSEM(K) .GT. 0) GO TO 14002                                 M23.2566
      N3 = N3 + 2 - CK(K) - CK(K)                                       M23.2567
      N4 = N4 + 2                                                       M23.2568
      IF (CK(K) .LT. 0.0) GO TO 14001                                   M23.2569
      N3 = N3 + 6 * CK(K)                                               M23.2570
      N4 = N4 + CK(K) - 1                                               M23.2571
      GO TO 14005                                                       M23.2572
14001 CONTINUE                                                          M23.2573
      GO TO 14005                                                       M23.2574
14002 N3 = N3 + 1 - CI(K) - CI(K)                                       M23.2575
      N4 = N4 + 2                                                       M23.2576
      IF (CI(K) .LT. 0.0) GO TO 14003                                   M23.2577
      N3 = N3 + 7 * CI(K)                                               M23.2578
      N4 = N4 + 2 * CI(K) - 2                                           M23.2579
14003 N3 = N3 - CK(K) - CK(K)                                           M23.2580
      N4 = N4 + 2                                                       M23.2581
      IF (CK(K) .LT. 0.0) GO TO 14004                                   M23.2582
      N3 = N3 + 6 * CK(K)                                               M23.2583
      N4 = N4 + 2 * CK(K) - 2                                           M23.2584
14004 CONTINUE                                                          M23.2585
14005 WRITE (LUNIT6, 14996) N1, N3, (SCONST(L), L=N1, N3)               M23.2586
      IF (N8 .GT. N4) N4 = N8                                           M23.2587
      WRITE (LUNIT6, 14995) N2, N4, (CNVHST(L), L=N2, N4)               M23.2588
      K = K + 1                                                         M23.2589
      N7 = N7 + 1                                                       M23.2590
      IF (CKI(K - 1) .GT. 0.0) GO TO 14000                              M23.2591
14009 IF (INDHST(I) .GT. 0 .OR. NR(I) .LT. 0) GO TO 14100               M23.2592
C                                                                       M23.2593
C  COMPUTE PHASOR TERMINAL CONDITIONS(VOLTAGE AND CURRENT) AND VOLTAGE  M23.2594
C  TRAVELLING WAVES(PHASE FRAME IF CIK(.) .LT. 0 ELSE MODAL).           M23.2595
C                                                                       M23.2596
C     VOLT       +J*VOLTI        KBUS TERMINAL VOLTAGE                  M23.2597
C     VOLTK      +J*VIM          MBUS TERMINAL VOLTAGE                  M23.2598
C     SEMAUX(0)  +J*SEMAUX(I1)   KBUS TERMINAL CURRENT TOWARDS MBUS     M23.2599
C     SEMAUX(I2) +J*SEMAUX(I3)   MBUS TERMINAL CURRENT TOWARDS KBUS     M23.2600
C     SEMAUX(I4) +J*SEMAUX(I5)   OUTGOING VOLTAGE TRAVELLING WAVE FROM KM23.2601
C     SEMAUX(I6) +J*SEMAUX(I7)   OUTGOING VOLTAGE TRAVELLING WAVE FROM MM23.2602
C     SEMAUX(I8) +J*SEMAUX(N23)   INCOMING VOLTAGE TRAVELLING WAVE AT K M37.4727
C     SEMAUX(I10)+J*SEMAUX(I11)  INCOMING VOLTAGE TRAVELLING WAVE AT M  M23.2604
C                                                                       M23.2605
      I1 = IT2                                                          M23.2606
      I2 = I1 + IT2                                                     M23.2607
      I3 = I2 + IT2                                                     M23.2608
      I4 = I3 + IT2                                                     M23.2609
      I5 = I4 + IT2                                                     M23.2610
      I6 = I5 + IT2                                                     M23.2611
      I7 = I6 + IT2                                                     M23.2612
      I8 = I7 + IT2                                                     M23.2613
      N23 = I8 + IT2                                                    M37.4728
      I10 = N23 + IT2                                                   M37.4729
      I11 = I10 + IT2                                                   M23.2616
      N1 = ABSZ(CIK(I))                                                 M23.2617
      N2 = N1 + IT2 * IT2                                               M23.2618
      N3 = NR(I)                                                        M23.2619
      N4 = N3 + IT2 - 1                                                 M23.2620
      WDTHAF = ONEHAF * STEADY                                          M23.2621
      W = STEADY / DELTAT                                               M23.2622
      IF (CIK(I) .GT. 0.0) GO TO 14010                                  M23.2623
      CALL MOVER0(VOLT(1), IT2)                                         M23.2624
      CALL MOVER0(VOLTI(1), IT2)                                        M23.2625
      CALL MOVER0(VOLTK(1), IT2)                                        M23.2626
      CALL MOVER0 ( VIM(LSIZ26+1), IT2 )                                M31.3030
      CALL MOVER0(SEMAUX(1), 12 * IT2)                                  M23.2628
14010 DO 14080 K=1, IT2                                                 M23.2629
      IF (KODSEM(I) .LT. 0 .AND. CIK(I) .LT. 0.0) GO TO 14060           M23.2630
      D1 = 0.0                                                          M23.2631
      D2 = 0.0                                                          M23.2632
      D3 = 0.0                                                          M23.2633
      D4 = 0.0                                                          M23.2634
      D5 = 0.0                                                          M23.2635
      D6 = 0.0                                                          M23.2636
      D7 = 0.0                                                          M23.2637
      D8 = 0.0                                                          M23.2638
      DO 14020 J=N3, N4                                                 M23.2639
      II = I + J - N3                                                   M23.2640
      N9 = -KBUS(II)                                                    M23.2641
      D1 = D1 + QFD(N1) * E(N9) - QFD(N2) * F(N9)                               
      D2 = D2 + QFD(N1) * F(N9) + QFD(N2) * E(N9)                               
      N9 = IABS(MBUS(II))                                               M23.2644
      D3 = D3 + QFD(N1) * E(N9) - QFD(N2) * F(N9)                               
      D4 = D4 + QFD(N1) * F(N9) + QFD(N2) * E(N9)                               
      D5 = D5 + SFD(N1) * TR(J) - SFD(N2) * TX(J)                               
      D6 = D6 + SFD(N1) * TX(J) + SFD(N2) * TR(J)                               
      D7 = D7 + SFD(N1) * R(J) - SFD(N2) * C(J)                                 
      D8 = D8 + SFD(N1) * C(J) + SFD(N2) * R(J)                                 
      N1 = N1 + 1                                                       M23.2651
      N2 = N2 + 1                                                       M23.2652
14020 CONTINUE                                                          M23.2653
      IF (KODSEM(I) .LT. 0) GO TO 14050                                 M23.2654
      II = K + I - 1                                                    M23.2655
      N5 = - INDHST(II)                                                 M23.2656
      IF (CI(II) .GT. 0.0)                                                      
     1                  N5 = N5 + 1                                             
      IF (ABSZ(W - CNVHST(N5 + 4)) .LT. 0.0001) GO TO 14040             M23.2658
      N6 = - KBUS(II)                                                   M23.2659
      N7 = IABS(MBUS(II))                                               M23.2660
      WRITE (LUNIT6, 14030) BUS(N6), BUS(N7), K, CNVHST(N5 + 4), W      M23.2661
14030 FORMAT(//97H WARNING...  STEADY STATE MODAL PARAMETERS FOR RECURSIM23.2662
     1VE-CONVOLUTION COMPONENT CONNECTING NODES ',A6,7H' AND ',A6,1H',/,M23.2663
     2 13X,  9HFOR MODE ,I2, 40H ARE DETERMINED AT ANGULAR FREQUENCY OF M23.2664
     4  ,E12.5,51H RADIANS/SEC.  THE STEADY-STATE SOLUTION FREQUENCY ,/,M23.2665
     5 13X,  14HBEING USED IS ,E12.5,14H RADIANS/SEC.   )               M23.2666
14040 D17 = CNVHST(N5 + 2) ** 2                                                 
     1    +  CNVHST(N5 + 3) ** 2                                                
      D19 = (CNVHST(N5+2)*CNVHST(N5+0)                                  M23.2668
     1      + CNVHST(N5+3)*CNVHST(N5+1))/D17                            M44. 617
      D20 = (CNVHST(N5+2)*CNVHST(N5+1)                                  M23.2669
     1      - CNVHST(N5+3)*CNVHST(N5+0))/D17                            M44. 618
      D17 = SQRTZ( D19 * D19  +  D20 * D20 )                            M23.2670
      IF (D19 .LT. 0.0) GO TO 14043                                     M23.2671
      D17 = SQRTZ((D19 + D17) * ONEHAF)                                 M23.2672
      D18 = ONEHAF * D20 / D17                                          M23.2673
      GO TO 14047                                                       M23.2674
14043 D18 = SQRTZ(( - D19 + D17) * ONEHAF)                              M23.2675
      IF (D20 .LT. 0.0) D18 = - D18                                     M23.2676
      D17 = ONEHAF * D20 / D18                                          M23.2677
C  D17 + J*D18 IS THE CHARACTERISTIC IMPEDANCE OF MODE K AT SS FREQUENCYM23.2678
14047 D9 = ONEHAF * (D1  +  D17 * D5  -  D18 * D6)                              
      D13 = D1  -  D9                                                           
      D10 = ONEHAF * (D2  +  D17 * D6  +  D18 * D5)                             
      D14 = D2  -  D10                                                          
      D11 = ONEHAF * (D3  +  D17 * D7  -  D18 * D8)                     M23.2683
      D15 = D3  -  D11                                                  M23.2684
      D12 = ONEHAF * (D4  +  D17 * D8  +  D18 * D7)                     M23.2685
      D16 = D4  -  D12                                                          
      IF (CIK(I) .LT. 0.0) GO TO 14060                                  M23.2687
14050 VOLT(K) = D1                                                              
      VOLTI(K) = D2                                                             
      VOLTK(K) = D3                                                             
      NDX1 = LSIZ26 + K                                                 M31.3031
      VIM(NDX1) = D4                                                            
      N6 = K                                                            M23.2692
      SEMAUX(N6) = D5                                                           
      N6 = N6 + IT2                                                     M23.2694
      SEMAUX(N6) = D6                                                           
      N6 = N6 + IT2                                                     M23.2696
      SEMAUX(N6) = D7                                                           
      N6 = N6 + IT2                                                     M23.2698
      SEMAUX(N6) = D8                                                           
      IF (KODSEM(I) .LT. 0) GO TO 14080                                 M23.2700
      N6 = N6 + IT2                                                     M23.2701
      SEMAUX(N6) = D9                                                   M23.2702
      N6 = N6 + IT2                                                     M23.2703
      SEMAUX(N6) = D10                                                  M23.2704
      N6 = N6 + IT2                                                     M23.2705
      SEMAUX(N6) = D11                                                  M23.2706
      N6 = N6 + IT2                                                     M23.2707
      SEMAUX(N6) = D12                                                  M23.2708
      N6 = N6 + IT2                                                     M23.2709
      SEMAUX(N6) = D13                                                  M23.2710
      N6 = N6 + IT2                                                     M23.2711
      SEMAUX(N6) = D14                                                  M23.2712
      N6 = N6 + IT2                                                     M23.2713
      SEMAUX(N6) = D15                                                  M23.2714
      N6 = N6 + IT2                                                     M23.2715
      SEMAUX(N6) = D16                                                  M23.2716
      GO TO 14080                                                       M23.2717
14060 N6 = -KBUS(II)                                                    M23.2718
      VOLT(K) = E(N9)                                                   M23.2719
      VOLTI(K) = F(N9)                                                  M23.2720
      N9 = IABS(MBUS(II))                                               M23.2721
      VOLTK(K) = E(N9)                                                  M23.2722
      NDX1 = LSIZ26 + K                                                 M31.3033
      VIM(NDX1) = F(N9)                                                 M31.3034
      N9 = N3 + K - 1                                                   M23.2724
      N6 = K                                                            M23.2725
      SEMAUX(N6) = TR(N9)                                               M23.2726
      N6 = N6 + IT2                                                     M23.2727
      SEMAUX(N6) = TX(N9)                                               M23.2728
      N6 = N6 + IT2                                                     M23.2729
      SEMAUX(N6) = R(N9)                                                M23.2730
      N6 = N6 + IT2                                                     M23.2731
      SEMAUX(N6) = C(N9)                                                M23.2732
      IF (KODSEM(I) .LT. 0) GO TO 14080                                 M23.2733
      N7 = -CIK(II) + (K - 1) * IT2                                     M23.2734
      N8 = N8 + IT2 * IT2                                               M23.2735
      DO 14070 J=1, IT2                                                 M23.2736
      N6 = I4 + J                                                       M23.2737
      SEMAUX(N6) = SEMAUX(N6) + SFD(N7) * D9 - SFD(N8) * D10            M23.2738
      N6 = I5 + J                                                       M23.2739
      SEMAUX(N6) = SEMAUX(N6) + SFD(N7) * D10 + SFD(N8) * D9            M23.2740
      N6 = I6 + J                                                       M23.2741
      SEMAUX(N6) = SEMAUX(N6) + SFD(N7) * D11 - SFD(N8) * D12           M23.2742
      N6 = I7 + J                                                       M23.2743
      SEMAUX(N6) = SEMAUX(N6) + SFD(N7) * D12 + SFD(N8) * D11           M23.2744
      N6 = I8 + J                                                       M23.2745
      SEMAUX(N6) = SEMAUX(N6) + SFD(N7) * D13 - SFD(N8) * D14           M23.2746
      N6 = N23 + J                                                      M37.4730
      SEMAUX(N6) = SEMAUX(N6) + SFD(N7) * D14 + SFD(N8) * D13           M23.2748
      N6 = I10 + J                                                      M23.2749
      SEMAUX(N6) = SEMAUX(N6) + SFD(N7) * D15 - SFD(N8) * D16           M23.2750
      N6 = I11 + J                                                      M23.2751
      SEMAUX(N6) = SEMAUX(N6) + SFD(N7) * D16 + SFD(N8) * D15           M23.2752
      N7 = N7 + 1                                                       M23.2753
      N8 = N8 + 1                                                       M23.2754
14070 CONTINUE                                                          M23.2755
14080 CONTINUE                                                          M23.2756
      IF (IPRSUP .LT. 6) GO TO 14085                                    M23.2757
      WRITE (LUNIT6, 14081) D17, D18                                    M23.2758
14081 FORMAT(///,5X, 40HCHARACTERISTIC IMPEDANCE OF LAST MODE = ,       M23.2759
     1   E15.7, 4H+ J*, E15.7, //, 5X, 4HMODE,6X,9HK-VOLTAGE,6X,        M23.2760
     2   9HM-VOLTAGE,6X,9HK-CURRENT,6X,9HM-CURRENT,7X,7HK V OUT,8X,     M23.2761
     3   7HM V OUT, 9X, 6HK V IN,9X,6HM V IN , //)                      M23.2762
      DO 14084 L=1, IT2                                                 M23.2763
      N3 = I2 + L                                                       M23.2764
      N4 = I4 + L                                                       M23.2765
      N5 = I6 + L                                                       M23.2766
      N6 = I8 + L                                                       M23.2767
      N7 = I10 + L                                                      M23.2768
      WRITE (LUNIT6, 14082) L, VOLT(L), VOLTK(L), SEMAUX(L), SEMAUX(N3),M23.2769
     1          SEMAUX(N4), SEMAUX(N5), SEMAUX(N6), SEMAUX(N7)          M23.2770
14082 FORMAT(/,6X,I2,1X,8E15.7)                                         M27.1137
      N2 = I1 + L                                                       M23.2772
      N3 = I3 + L                                                       M23.2773
      N4 = I5 + L                                                       M23.2774
      N5 = I7 + L                                                       M23.2775
      N6 = N23 + L                                                      M37.4731
      N7 = I11 + L                                                      M23.2777
      NDX1 = LSIZ26 + L                                                 M31.3035
      WRITE (LUNIT6,14083) VOLTI(L), VIM(NDX1), SEMAUX(N2), SEMAUX(N3), M31.3036
     1          SEMAUX(N4), SEMAUX(N5), SEMAUX(N6), SEMAUX(N7)          M23.2779
14083 FORMAT(9X,8E15.7)                                                 M27.1138
14084 CONTINUE                                                          M23.2781
14085 IF (IFLAG .EQ. 1) GO TO 14100                                     M23.2782
      IF ( IPRSUP .GE. 1 )   WRITE (LUNIT6, 14090)                      M28.4573
14090 FORMAT(//,5X,113HTABLE OF DISCREPANCIES BETWEEN THE RESULTS OF TIMM23.2784
     1E-DOMAIN CONVOLUTION(ANLAYTIC) AND STEADY-STATE PHASOR SOLUTION., M23.2785
     3//,2X,6H KBUS ,5X,6H MBUS ,5X,3HROW,5X,3HCOL,7X,12HKBUS VOLTAGE,  M23.2786
     4  8X,12HMBUS VOLTAGE,8X,12HKBUS CURRENT,8X,12HMBUS CURRENT, //)   M23.2787
      IFLAG = 1                                                         M23.2788
14100 ITADD = NR(I)                                                     M23.2789
      DTHAF = ONEHAF * DELTAT                                           M23.2790
      MXSTRT = 0                                                        M23.2791
      K = I                                                             M23.2792
C  BEGIN LOOP OVER CONVOLUTIONS ....                                    M23.2793
14105 N1 = ABSZ(CKI(K)) - 1                                             M23.2794
      N2 = N1 / IT2 + 1                                                 M23.2795
      N1 = N1 - (N2 - 1) * IT2 + 1                                      M23.2796
      IF (ITADD .LT. 0) GO TO 14290                                     M23.2797
      N3 = KODSEM(K)                                                    M23.2798
      N4 = IABS(INDHST(K))                                              M23.2799
      IF (N3 .GT. 0) GO TO 14190                                        M23.2800
      N5 = CK(K)                                                        M23.2801
      IF (N5 .GE. 0) GO TO 14290                                        M23.2802
C  ALLOCATE AND INITIALIZE PAST HISTORY OF AMETANI LUMPED ELEMENT.      M23.2803
      N5 = - N3 - N5 - N5                                               M23.2804
      N6 = SCONST(N5) / DELTAT + 2.0                                    M23.2805
      LENGTH(K) = N6                                                    M23.2806
      IF (MXSTRT .NE. 0) GO TO 14150                                    M23.2807
      NR(K) = ILINE                                                             
      MXSTRT = ILINE                                                    M23.2809
      ILINE = ILINE + N6                                                M23.2810
      IF (ILINE .LE. LPAST) GO TO 14120                                 M23.2811
      LSTAT(19) = 14110                                                 M23.2812
14110 LSTAT(16) = 8                                                     M23.2813
      LSTAT(10) = N3                                                    M23.2814
      LSTAT(11) = N4                                                    M23.2815
      LSTAT(12) = N5                                                    M23.2816
      FLSTAT(15) = DELTAT                                               M23.2817
      FLSTAT(16) = CNVHST(N4)                                           M23.2818
      FLSTAT(17) = SCONST(N5)                                           M23.2819
      FLSTAT(18) = SCONST(N3 - 2)                                       M23.2820
      KILL = 1                                                          M23.2821
      GO TO 9200                                                        M23.2822
14120 IF (INDHST(K) .LT. 0) GO TO 14130                                 M23.2823
      N7 = NR(K)                                                        M23.2824
      CALL MOVER0(XK(N7), N6 - 1)                                       M23.2825
      GO TO 14290                                                       M23.2826
14130 D1 = VOLT(N2) - VOLTK(N2)                                         M23.2827
      NDX1 = LSIZ26 + N2                                                M31.3037
      D2 = VOLTK(N2) - VIM(NDX1)                                        M31.3038
      DO 14140 J=2, N6                                                  M23.2829
      D3 = (J - N6) * STEADY                                            M23.2830
      N8 = N7 + J                                                       M23.2831
      XK(N8) = D1 * COSZ(D3) - D2 * SINZ(D3)                            M23.2832
14140 CONTINUE                                                          M23.2833
      GO TO 14290                                                       M23.2834
14150 NR(K) = - MXSTRT                                                  M23.2835
      IF (ILINE .LT. MXSTRT + N6) ILINE = MXSTRT + N6                   M23.2836
      MXSTRT = 0                                                        M23.2837
      IF (ILINE .LE. LPAST) GO TO 14160                                 M23.2838
      LSTAT(19) = 14160                                                 M23.2839
      GO TO 14110                                                       M23.2840
14160 IF (INDHST(K) .LT. 0) GO TO 14170                                 M23.2841
      N7 = - NR(K)                                                      M23.2842
      CALL MOVER0(XM(N7), N6 - 1)                                       M23.2843
      GO TO 14290                                                       M23.2844
14170 N7 = - NR(K) - 2                                                  M23.2845
      D1 = VOLT(N2) - VOLTK(N2)                                         M23.2846
      NDX1 = LSIZ26 + N2                                                M31.3039
      D2 = VOLTK(N2) - VIM(NDX1)                                        M31.3040
      DO 14180 J=2, N6                                                  M23.2848
      D3 = (J - N6) * STEADY                                            M23.2849
      N8 = N7 + J                                                       M23.2850
      XM(N8) = D1 * COSZ(D3) - D2 * SINZ(D3)                            M23.2851
14180 CONTINUE                                                          M23.2852
      GO TO 14290                                                       M23.2853
C  ALLOCATE AND INITIALIZE PAST HISTORY FOR PROPAGATION CONVOLUTIONS.   M23.2854
14190 N5 = CI(K)                                                                
      IF (N5) 14195, 14250, 14200                                       M23.2856
14195 N3 = N3 - N5 - N5 + 1                                             M23.2857
      N6 = SCONST(N3 - 2) / DELTAT + 2.0                                M23.2858
      GO TO 14210                                                       M23.2859
14200 N6 = CNVHST(N4) / DELTAT + 2.0                                            
      N3 = N3 + 5 * N5 + 1                                              M23.2861
14210 LENGTH(K) = N6                                                            
      NR(K) = ILINE +1                                                          
      ILINE = ILINE + N6 +1                                                     
      IF (ILINE .LE. LPAST) GO TO 14220                                         
      LSTAT(19) = 14220                                                 M23.2866
      GO TO 14110                                                       M23.2867
14220 IF (INDHST(K) .LT. 0) GO TO 14230                                 M23.2868
      N7 = NR(K)                                                        M23.2869
      CALL MOVER0(XK(N7), N6 - 1)                                       M23.2870
      CALL MOVER0(XM(N7), N6 - 1)                                       M23.2871
      GO TO 14250                                                       M23.2872
14230 N7 = NR(K) - 2                                                    M23.2873
      N8 = I4 + N2                                                      M23.2874
      D1 = SEMAUX(N8)                                                   M23.2875
      N8 = I5 + N2                                                      M23.2876
      D2 = SEMAUX(N8)                                                   M23.2877
      N8 = I6 + N2                                                      M23.2878
      D3 = SEMAUX(N8)                                                   M23.2879
      N8 = I7 + N2                                                      M23.2880
      D4 = SEMAUX(N8)                                                   M23.2881
      DO 14240 J=2, N6                                                  M23.2882
      D6 = (J - N6 - 1) * STEADY                                        M23.2883
      D5 = COSZ(D6)                                                     M23.2884
      D6 = SINZ(D6)                                                     M23.2885
      N8 = N7 + J                                                       M23.2886
      XK(N8) = D1 * D5 - D2 * D6                                        M23.2887
      XM(N8) = D3 * D5 - D4 * D6                                        M23.2888
14240 CONTINUE                                                          M23.2889
C  ALLOCATE AND INITIALIZE PAST HISTORY FOR CHAR. ADMITTANCE CONVOLUTIONM23.2890
14250 N5 = CK(K)                                                        M23.2891
      IF (N5 .GE. 0) GO TO 14290                                        M23.2892
      N3 = N3 - N5 - N5                                                 M23.2893
      N6 = SCONST(N3 - 2) / DELTAT + 2.0                                M23.2894
      N7 = ILINE                                                        M23.2895
      ILINE = ILINE + N6                                                M23.2896
      IF (ILINE .LE. LPAST) GO TO 14260                                 M23.2897
      LSTAT(19) = 14260                                                 M23.2898
      GO TO 14110                                                       M23.2899
14260 IF (INDHST(K) .LT. 0) GO TO 14270                                 M23.2900
      CALL MOVER0(XK(N7), N6 - 1)                                       M23.2901
      CALL MOVER0(XM(N7), N6 - 1)                                       M23.2902
      GO TO 14290                                                       M23.2903
14270 N7 = N7 - 2                                                       M23.2904
      N8 = I8 + N2                                                      M23.2905
      D1 = VOLT(N2) - 2.0 * SEMAUX(N8)                                  M23.2906
      N8 = N23 + N2                                                     M37.4732
      D2 = VOLTI(N2) - 2.0 * SEMAUX(N8)                                 M23.2908
      N8 = I10 + N2                                                     M23.2909
      D3 = VOLTK(N2) - 2.0 * SEMAUX(N8)                                 M23.2910
      N8 = I11 + N2                                                     M23.2911
      NDX1 = LSIZ26 + N2                                                M31.3041
      D4 = VIM(NDX1) - 2.0 * SEMAUX(N8)                                 M31.3042
      DO 14280 J=2, N6                                                  M23.2913
      D6 = (J - N6) * STEADY                                            M23.2914
      D5 = COSZ(D6)                                                     M23.2915
      D6 = SINZ(D6)                                                     M23.2916
      N8 = N7 + J                                                       M23.2917
      XK(N8) = D1 * D5 - D2 * D6                                        M23.2918
      XM(N8) = D3 * D5 - D4 * D6                                        M23.2919
14280 CONTINUE                                                          M23.2920
14290 N3 = KODSEM(K)                                                    M23.2921
      IF (N3 .GT. 0) GO TO 14430                                        M23.2922
C  INITIALIZE HISTORY TERM OF LUMPED ELEMENT PARALLEL CAPACITOR         M23.2923
      N5 = CK(K)                                                        M23.2924
      N3 = - N3                                                         M23.2925
      SCONST(N4 + 1) = SCONST(N4 + 1) / DTHAF                           M23.2926
      IF (ITADD .LT. 0) GO TO 14310                                     M23.2927
      EEETA = CNVHST(N4)                                                M23.2928
      IF (INDHST(K) .LT. 0) GO TO 14300                                 M23.2929
      CNVHST(N4) = 0.0                                                  M23.2930
      GO TO 14310                                                       M23.2931
14300 D1 = VOLT(N2) - VOLTK(N2)                                         M23.2932
      NDX1 = LSIZ26 + N2                                                M31.3043
      D2 = VOLTI(N2) - VIM(NDX1)                                        M31.3044
      D3 = COSZ(STEADY)                                                 M23.2934
      D4 = - SINZ(STEADY)                                               M23.2935
      D5 = D1 * D3 - D2 * D4                                            M23.2936
      D6 = D1 * D4 + D2 * D3                                            M23.2937
      CNVHST(N4) = - SCONST(N3 + 1) * (D5 + WDTHAF * D6)                M23.2938
      D3 = D1 * (SCONST(N3) + SCONST(N3 + 1)) - CNVHST(N4)              M23.2939
      IF (N5 .GE. 0) GO TO 14310                                        M23.2940
      D9 = - D1 * SCONST(N3 + 2) * DTHAF                                M23.2941
14310 IF (N5) 14320, 14340, 14370                                       M23.2942
14320 N5 = N3 - N5 - N5                                                 M23.2943
      N3 = N3 + 2                                                       M23.2944
      N4 = N4 + 1                                                       M23.2945
      I0 = -1                                                           M23.2946
      ASSIGN 14340 TO MOON                                              M23.2947
      GO TO 14700                                                       M23.2948
C     ...  AMETANI INITIALIZATION  ...                                  M23.2949
14340 IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14670               M23.2950
      GO TO 14410                                                       M23.2951
C  INITIALIZE SEMLYEN LUMPED ELEMENT CONVOLUTION                        M23.2952
14370 N3 = N3 + 2                                                       M23.2953
      N5 = N3 + 4 * (N5 - 1)                                            M23.2954
      N4 = N4 + 1                                                       M23.2955
      ASSIGN 14390 TO MOON                                              M23.2956
      I0 = -1                                                           M23.2957
      GO TO 14800                                                       M23.2958
C     ...  SEMLYEN INITIALIZATION  ...                                  M23.2959
14390 IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14670               M23.2960
14410 D4 = SEMAUX(N2)                                                   M23.2961
      D5 = - D3                                                         M23.2962
      D6 = - D4                                                         M23.2963
      N8 = I + N1 - 1                                                   M23.2964
      N7 = - KBUS(N8)                                                   M23.2965
      N8 = IABS(MBUS(N8))                                               M23.2966
      IF ( IPRSUP  .GE.  1 )                                            M28.4576
     1     WRITE (LUNIT6, 14420) BUS(N7), BUS(N8), N1, N2, D3, D5, D4,  M23.2968
     2          D6                                                      M23.2969
14420 FORMAT(/,2X,A6,5X,A6,5X,I2,6X,I2,46X,E15.8,5X,E15.8,/,75X,        M23.2970
     1     2(5X,E15.8))                                                 M23.2971
      GO TO 14670                                                       M23.2972
C  TRANSMISSION LINE PROPAGATION IMPULSE CONVOLUTION INITIALIZATION     M23.2973
14430 N5 = CI(K)                                                        M23.2974
      N3 = N3 + 1                                                       M23.2975
      EEETA = CNVHST(N4)                                                M23.2976
      IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14440               M23.2977
      N8 = I4 + N2                                                      M23.2978
      D5 = SEMAUX(N8)                                                   M23.2979
      N8 = I5 + N2                                                      M23.2980
      D6 = SEMAUX(N8)                                                   M23.2981
      N8 = I6 + N2                                                      M23.2982
      D1 = SEMAUX(N8)                                                   M23.2983
      N8 = I7 + N2                                                      M23.2984
      D2 = SEMAUX(N8)                                                   M23.2985
      D3 = 0.0                                                          M23.2986
      D4 = 0.0                                                          M23.2987
      N8=I8+N2                                                          M28.4577
      N9=I10+N2                                                         M28.4578
      SOUTK=SEMAUX(N8)                                                  M28.4579
      SOUTM=SEMAUX(N9)                                                  M28.4580
      TRRSUM = 0.0                                                      M28.4581
      TRISUM = 0.0                                                      M28.4582
      IF (N5 .GE. 0) GO TO 14440                                        M23.2988
      D9 = 0.0                                                          M23.2989
      D10 = 0.0                                                         M23.2990
14440 IF (N5) 14450, 14525, 14500                                       M23.2991
C  AMETANI PROPAGATION CONVOLUTION INITIALIZATION                       M23.2992
14450 N5 = N3 - N5 - N5 - 2                                             M23.2993
      I0 = 1                                                            M23.2994
      ASSIGN 14470 TO MOON                                              M23.2995
      GO TO 14700                                                       M23.2996
C     ...  AMETANI INITIALIZATION  ...                                  M23.2997
14470 N3 = N5 + 2                                                       M23.2998
      GO TO 14525                                                       M23.2999
C  SEMLYEN PROPAGATION CONVOLUTION INITIALIZATION.                      M23.3000
14500 N5 = N3 + 5 * N5 - 5                                              M23.3001
      I0 = 2                                                            M23.3002
      ASSIGN 14520 TO MOON                                              M23.3003
      GO TO 14800                                                       M23.3004
C     ...  SEMLYEN INITIALIZATION  ...                                  M23.3005
14520 N3 = N5 + 5                                                       M23.3006
14525 IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14550               M23.3007
      N8 = N1 + I - 1                                                   M23.3008
      N7 = - KBUS(N8)                                                   M23.3009
      N8 = IABS(MBUS(N8))                                               M23.3010
      IF ( IPRSUP  .GE.  1 )                                            M28.4583
     1     WRITE (LUNIT6, 14540) BUS(N7), BUS(N8), N1, N2, D3, D4       M23.3012
14540 FORMAT(/,2X,A6,5X,A6,5X,I2,6X,I2,6X,E15.8,5X,E15.8)               M23.3013
C  TRANSMISSION LINE CHARACTERISTIC ADMITTANCE CONVOLUTION INITIALIZATIOM23.3014
      IF ( IPRSUP  .GE.  1 )                                            M28.4584
     1 WRITE (LUNIT6, 14415) TRRSUM, TRISUM                             M28.4585
14550 N5 = CK(K)                                                        M23.3015
      IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14560               M23.3016
      IF (ITADD.NE.11911) GO TO 14555                                   M28.4586
      D1SAVE=D1                                                         M28.4587
      D1=TRRSUM*D1SAVE-TRISUM*D2                                        M28.4588
      D2=TRISUM*D1+TRRSUM*D2                                            M28.4589
      D5SAVE=D5                                                         M28.4590
      D5=TRRSUM*D5SAVE-TRISUM*D6                                        M28.4591
      D6=TRISUM*D5+TRRSUM*D6                                            M28.4592
14555 CONTINUE                                                          M28.4593
      N8 = I8 + N2                                                      M28.4594
      D1 = VOLT(N2) - 2.0 * SEMAUX(N8)                                  M28.4595
      N8 = N23 + N2                                                     M37.4733
      D2 = VOLTI(N2) - 2.0 * SEMAUX(N8)                                 M28.4597
      N8 = I10 + N2                                                     M28.4598
      D5 = VOLTK(N2) - 2.0 * SEMAUX(N8)                                 M28.4599
      N8 = I11 + N2                                                     M28.4600
      NDX1 = LSIZ26 + N2                                                M31.3045
      D6 = VIM(NDX1) - 2.0 * SEMAUX(N8)                                 M31.3046
      N8 = KODSEM(K)                                                    M23.3025
      D3 = SCONST(N8) * D1                                              M23.3026
      D4 = SCONST(N8) * D5                                              M28.4602
      TRRSUM = SCONST(N8)                                               M28.4603
      TRISUM = 0.0                                                      M28.4604
      N8=I2+N2                                                          M28.4605
      SOUTK=SEMAUX(N2)                                                  M28.4606
      SOUTM=SEMAUX(N8)                                                  M28.4607
      IF (N5 .GE. 0) GO TO 14560                                        M23.3028
      D9 = - SCONST(N3 + 1) * VOLT(N2) * DTHAF                          M23.3029
      D10 = - SCONST(N3 + 1) * VOLTK(N2) * DTHAF                        M23.3030
14560 IF (N5) 14570, 14580, 14610                                       M23.3031
C  AMETANI CHARACTERISTIC ADMITTANCE CONVOLUTION INITIALIZATION         M23.3032
14570 N5 = N3 - N5 - N5 - 2                                             M23.3033
      I0 = 1                                                            M23.3034
      ASSIGN 14580 TO MOON                                              M23.3035
      GO TO 14700                                                       M23.3036
C     ...  AMETANI INITIALIZATION  ...                                  M23.3037
14580 IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14670               M23.3038
      GO TO 14650                                                       M23.3039
C  SEMLYEN CHARACTERISTIC ADMITTANCE CONVOLUTION INITIALIZATION         M23.3040
14610 N5 = N3 + 4 * (N5 - 1)                                            M23.3041
      I0 = 1                                                            M23.3042
      ASSIGN 14630 TO MOON                                              M23.3043
      GO TO 14800                                                       M23.3044
C     ...  SEMLYEN INITIALIZATION  ...                                  M23.3045
14630 IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14670               M23.3046
14650 N3 = I2 + N2                                                      M23.3047
      N4 = I8 + N2                                                      M23.3048
      N5 = I10 + N2                                                     M23.3049
      IF ( IPRSUP  .GE.  1 )                                            M28.4608
     1     WRITE (LUNIT6, 14660) D3, D4, SEMAUX(N4), SEMAUX(N5),        M23.3051
     2          SEMAUX(N2), SEMAUX(N3)                                  M23.3052
14660 FORMAT(1H ,79X,E15.8,5X,E15.8 / 35X,4(5X,E15.8))                  M27.1142
      IF ( IPRSUP  .GE.  1 )                                            M28.4609
     1 WRITE (LUNIT6, 14415) TRRSUM, TRISUM                             M28.4610
14415 FORMAT (22H TRRSUM AND TRISUM ARE, 5X, 2E15.7)                    M28.4611
C                                                                       M23.3054
14670 INDHST(K) = IABS(INDHST(K))                                       M23.3055
      NR(K) = IABS(NR(K))                                               M23.3056
      K = K + 1                                                         M23.3057
      IF (CKI(K - 1) .GE. 0.0) GO TO 14105                              M23.3058
      IF (IPRSUP .LT. 6) GO TO 610                                      M23.3059
      K = I                                                             M23.3060
      N7 = 1                                                            M23.3061
14680 WRITE (LUNIT6, 14999) K, KBUS(K), MBUS(K), NR(K), LENGTH(K),      M23.3062
     1          KODSEM(K), INDHST(K), KODEBR(K), CI(K), CK(K),          M23.3063
     2          CIK(K), CKI(K)                                          M23.3064
      N1 = IABS(KODSEM(K))                                              M23.3065
      N2 = IABS(INDHST(K))                                              M23.3066
      N3 = N1 - 1                                                       M23.3067
      N4 = N2 - 1                                                       M23.3068
      N8 = N4                                                           M23.3069
      IF (N7 .LE. IABS(KODEBR(K))) N8 = N8 + 6                          M23.3070
      IF (KODSEM(K) .GT. 0) GO TO 14682                                 M23.3071
      N3 = N3 + 2 - CK(K) - CK(K)                                       M23.3072
      N4 = N4 + 2                                                       M23.3073
      IF (CK(K) .LT. 0.0) GO TO 14681                                   M23.3074
      N3 = N3 + 6 * CK(K)                                               M23.3075
      N4 = N4 + CK(K) - 1                                               M23.3076
      GO TO 14685                                                       M23.3077
14681 N5 = NR(K)                                                        M23.3078
      N6 = N6 + N5 + LENGTH(K) - 1                                      M23.3079
      IF (NR(K) .GT. 0)                                                 M23.3080
     1          WRITE (LUNIT6, 14998) N5, N6, (XK(L), L=N5, N6)         M23.3081
      IF (NR(K) .LT. 0)                                                 M23.3082
     1          WRITE (LUNIT6, 14997) N5, N6, (XM(L), L=N5, N6)         M23.3083
      GO TO 14685                                                       M23.3084
14682 N3 = N3 + 1 - CI(K) - CI(K)                                       M23.3085
      N4 = N4 + 2                                                       M23.3086
      IF (CI(K) .LT. 0.0) GO TO 14683                                   M23.3087
      N3 = N3 + 7 * CI(K)                                               M23.3088
      N4 = N4 + 2 * CI(K) - 2                                           M23.3089
14683 N3 = N3 - CK(K) - CK(K)                                           M23.3090
      N4 = N4 + 2                                                       M23.3091
      IF (CK(K) .LT. 0.0) GO TO 14684                                   M23.3092
      N3 = N3 + 6 * CK(K)                                               M23.3093
      N4 = N4 + 2 * CK(K) - 2                                           M23.3094
14684 N5 = NR(K)                                                        M23.3095
      N6 = N5 + LENGTH(K) - 1                                           M23.3096
      IF (CK(K) .LT. 0.0) N6 = N6 + SCONST(N3 - 1) / DELTAT + 2.0       M23.3097
      WRITE (LUNIT6, 14998) N5, N6, (XK(L), L=N5, N6)                   M23.3098
      WRITE (LUNIT6, 14997) N5, N6, (XM(L), L=N5, N6)                   M23.3099
14685 WRITE (LUNIT6, 14996) N1, N3, (SCONST(L), L=N1, N3)               M23.3100
      IF (N8 .GT. N4) N4 = N8                                           M23.3101
      WRITE (LUNIT6, 14995) N2, N4, (CNVHST(L), L=N2, N4)               M23.3102
      K = K + 1                                                         M23.3103
      N7 = N7 + 1                                                       M23.3104
      IF (CKI(K - 1) .GT. 0.0) GO TO 14680                              M23.3105
14999 FORMAT(//,1X,4HK = ,I5,5X,7HKBUS = ,I5,5X,7HMBUS = ,I5,5X,        M23.3106
     1  5HNR = ,I5,5X,9HLENGTH = ,I5,5X,9HKODSEM = ,I5,/,1X,9HINDHST = ,M23.3107
     2  I5,5X,9HKODEBR = ,I5,5X,5HCI = ,F5.0,5X,5HCK = ,F5.0,5X,        M23.3108
     3  6HCIK = ,F5.0,5X,6HCKI = ,F5.0)                                 M23.3109
14998 FORMAT(//5X,14H(XK    (L), L=,I10,2H, ,I10,3H)..,/,(8(1X,E15.7))) M27.1143
14997 FORMAT(//5X,14H(XM    (L), L=,I10,2H, ,I10,3H)..,/,(8(1X,E15.7))) M27.1144
14996 FORMAT(//5X,14H(SCONST(L), L=,I10,2H, ,I10,3H)..,/,(8(1X,E15.7))) M27.1145
14995 FORMAT(//5X,14H(CNVHST(L), L=,I10,2H, ,I10,3H)..,/,(8(1X,E15.7))) M27.1146
      GO TO 610                                                         M23.3114
C  AMETANI PIECEWISE-LINEAR CONVOLUTION INITIALIZATION                  M23.3115
14700 IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14710               M23.3116
      D1 = D1 / WDTHAF                                                  M23.3117
      D2 = D2 / WDTHAF                                                  M23.3118
      IF (I0 .LT. 0) GO TO 14710                                        M23.3119
      D5 = D5 / WDTHAF                                                  M23.3120
      D6 = D6 / WDTHAF                                                  M23.3121
14710 DO 14730 J=N3, N5, 2                                              M23.3122
      D15 = DTHAF * SCONST(J + 1)                                       M23.3123
      IF (KODEBR(K) .GT. 0) SCONST(J + 1) = D15                         M23.3124
      IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14730               M23.3125
      D8 = W * SCONST(J)                                                M23.3126
      D7 = COSZ ( D8 )                                                  M23.3127
      D8 = SINZ ( D8 )                                                  M23.3128
      IF (J .EQ. N3) GO TO 14720                                        M23.3129
      D11 = D16 * (D1 * (D8 - D14) - D2 * (D7 - D13))                   M23.3130
      D3 = D3 + D11                                                     M23.3131
      D9 = D9 + D11                                                     M23.3132
      IF (I0 .LT. 0) GO TO 14720                                        M23.3133
      D12 = D16 * (D5 * (D8 - D14) - D6 * (D7 - D13))                   M23.3134
      D4 = D4 + D12                                                     M23.3135
      D10 = D10 + D12                                                   M23.3136
14720 D13 = D7                                                          M23.3137
      D14 = D8                                                          M23.3138
      D16 = D15                                                         M23.3139
14730 CONTINUE                                                          M23.3140
      IF (ITADD .LT. 0) GO TO 14750                                     M23.3141
      IF (INDHST(K) .LE. 0) GO TO 14740                                 M23.3142
      CNVHST(N4) = 0.0                                                  M23.3143
      N4 = N4 + 1                                                       M23.3144
      IF (I0 .LT. 0) GO TO 14750                                        M23.3145
      CNVHST(N4) = 0.0                                                  M23.3146
      N4 = N4 + 1                                                       M23.3147
      GO TO 14750                                                       M23.3148
14740 D8 = W * TMAX                                                     M23.3149
      D7 = COSZ ( D8 )                                                  M23.3150
      D8 = SINZ ( D8 )                                                  M23.3151
      D11 = D16 * (D1 * (D8 - D14) - D2 * (D7 - D13))                   M23.3152
      D3 = D3 + D11                                                     M23.3153
      CNVHST(N4) = D9 + D11                                             M23.3154
      N4 = N4 + 1                                                       M23.3155
      IF (I0 .LT. 0) GO TO 14750                                        M23.3156
      D12 = D16 * (D5 * (D8 - D14) - D6 * (D7 - D13))                   M23.3157
      D4 = D4 + D12                                                     M23.3158
      CNVHST(N4) = D10 + D12                                            M23.3159
      N4 = N4 + 1                                                       M23.3160
14750 GO TO MOON, (14340, 14470, 14580)                                 M23.3161
C  SEMLYEN EXPONENTIAL CONVOLUTION INITIALIZATION                       M23.3162
14800 N10 = 4                                                           M23.3163
      DXP = 1.0                                                         M28.4612
      DXQ = 0.0                                                         M28.4613
      IF (IABS(I0) .LE. 1) GO TO 14810                                  M23.3164
      D7 = EEETA / DELTAT                                                       
      N7 = D7                                                           M23.3166
      DXQ = D7 - N7                                                     M23.3167
      DXP = UNITY - DXQ                                                 M23.3168
      N10 = 5                                                           M23.3169
      IF (ITADD .LT. 0 .OR. INDHST(K) .GE. 0) GO TO 14810               M23.3170
      D8 = W * EEETA                                                            
      D7 = COSZ ( D8 )                                                  M23.3172
      D8 = - SINZ ( D8 )                                                M23.3173
      D9 = D1 * D7 - D2 * D8                                            M23.3174
      D2 = D1 * D8 + D2 * D7                                            M23.3175
      D1 = D9                                                           M23.3176
      IF (I0 .LT. 0) GO TO 14810                                        M23.3177
      D9 = D5 * D7 - D6 * D8                                            M23.3178
      D6 = D5 * D8 + D6 * D7                                            M23.3179
      D5 = D9                                                           M23.3180
      IF (IPRSUP .GE. 1)                                                M27.1147
     1 WRITE (LUNIT6, 14802) D1, D2, D5, D6                             M27.1148
14802 FORMAT (43H VALUES OF D1,D2,D5,D6 BEFORE DO 14940 LOOP,           M27.1149
     1 4E17.8)                                                          M27.1150
14810 DO 14940 J=N3, N5, N10                                            M23.3181
      STYPE=SCONST(J)                                                           
      IF (STYPE) 14940, 14820, 14830                                            
14820 D7 = SCONST(J + 1)                                                        
      D8 = 0.0                                                          M23.3184
      D11 = SCONST(J + 2)                                                       
      D12 = 0.0                                                         M23.3186
      IF (D7 .NE. 0.0) GO TO 14826                                      M27.1153
      SCONST(J+2) = DXP*D11                                             M27.1154
      SCONST(J+3) = DXQ*D11                                             M27.1155
      IF (I0.EQ.2) SCONST(J+4) =0.0                                     M28.4616
      CNVHST(N4) = 0.0                                                  M27.1157
      CNVHST(N4+1) = 0.0                                                M27.1158
      N4 = N4 + 2                                                       M27.1159
      IF (INDHST(K) .GE. 0) GO TO 14940                                 M27.1160
      CNVHST(N4-2) = D1*D11                                             M27.1161
      D3 = D3 + D1*D11                                                  M28.4617
      CNVHST(N4-1) = D5*D11                                             M27.1163
      D4 = D4 + D5*D11                                                  M28.4618
      GO TO 14940                                                       M27.1165
14826 D9  = D11 * D7                                                            
      D10 = 0.0                                                         M23.3188
      GO TO 14840                                                       M23.3189
14830 N9 = J + N10                                                              
      D7 = SCONST(J + 1)                                                        
      D8 = SCONST(N9 + 1)                                                       
      D11 = SCONST(J + 2)                                                       
      D12 = SCONST(N9 + 2)                                                      
      D9  = D11 * D7  -  D12 * D8                                               
      D10 = D11 * D8  +  D12 * D7                                               
C                                                                       M44. 619
14840 D13 = DELTAT * D7                                                         
      D14 = DELTAT * D8                                                         
      IF (D13 * D13 + D14 * D14 .GT. 0.0001) GO TO 14880                M23.3199
C                                                                       M44. 620
      N6 = 0                                                            M23.3200
      N8 = 1                                                            M23.3201
      D20 = EPSILN * EPSILN                                             M23.3202
      DBLPR1 = 0.0                                                      M23.3203
      DBLPR2 = 0.0                                                      M23.3204
      DBLPR3 = 0.0                                                      M23.3205
      DBLPR4 = 0.0                                                      M23.3206
      D15 = - UNITY                                                     M23.3207
      D16 = 0.0                                                         M23.3208
14850 N6 = N6 + 1                                                       M23.3209
      IF (N6 .LT. 15) GO TO 14870                                       M23.3210
      N8 = I + N1 - 1                                                   M23.3211
      N7 = - KBUS(N8)                                                   M23.3212
      N8 = IABS(MBUS(N8))                                               M23.3213
      WRITE (LUNIT6, 14860) BUS(N7), BUS(N8), N1, N2, I0,               M23.3214
     1          D17, D18, FLZERO                                        M23.3215
14860 FORMAT(//,107H WARNING...  THE TAYLOR'S SERIES USED TO CLACULATE TM23.3216
     1HE SEMLYEN CONVOLUTION COEFFIECIENT CONNECTED BETWEEN ',A6,7H' ANDM23.3217
     2 ',A6,1H',/,13X, 46HHAS FAILED TO CONVERGE.  THE MATRIX ENTRY IS (M23.3218
     3,I2,1H,,I2,  8H), TYPE ,I2, 21H.  RELATIVE ERROR IS ,E15.8,5H AND M23.3219
     4,E12.5,1H.,/ ,13X, 30HTHE CONVERGENCE TOLLERANCE IS ,E12.5,       M23.3220
     5 69H.  COMPUTATION WILL CONTINUE USING THE 'DEXPZ/DSINZ/DCOSZ' ROUM23.3221
     6TINES.    )                                                       M23.3222
      GO TO 14880                                                       M23.3223
14870 N7 = N8                                                           M23.3224
      N8 = N8 * (N6 + 1)                                                M23.3225
      D17 = -D15 * D13 + D16 * D14                                      M23.3226
      D16 = - D15 * D14 - D16 * D13                                     M23.3227
      D15 = D17                                                         M23.3228
      D17 = D15 / N8                                                    M23.3229
      D18 = D16 / N8                                                    M23.3230
      DBLPR1 = DBLPR1 + D17                                             M23.3231
      DBLPR2 = DBLPR2 + D18                                             M23.3232
      D17 = (D17 * D17  + D18 * D18) / (DBLPR1*DBLPR1 + DBLPR2*DBLPR2)  M23.3233
      D18 = N8 - N7                                                     M23.3234
      D18 = D18 / N8 / N7                                               M23.3235
      D19 = D15 * D18                                                   M23.3236
      D18 = D16 * D18                                                   M23.3237
      DBLPR3 = DBLPR3 + D19                                             M23.3238
      DBLPR4 = DBLPR4 + D18                                             M23.3239
      D18 = (D19 * D19 + D18 * D18) / (DBLPR3*DBLPR3 + DBLPR4*DBLPR4)   M23.3240
      IF (D18 .GT. D20 .OR. D17 .GT. D20) GO TO 14850                   M23.3241
      D15 = D11 * DBLPR1 - D12 * DBLPR2                                 M23.3242
      D16 = D11 * DBLPR2 + D12 * DBLPR1                                 M23.3243
      IF (KODEBR(K) .LT. 0) GO TO 14890                                 M23.3244
      D13 = EXPZ( - D13 )                                               M23.3245
      SCONST(J + 1) = D13 * COSZ(D14)                                   M23.3246
      SCONST(J + 2) = D15                                               M23.3247
      SCONST(J + 3) = D11 * DBLPR3 - D12 * DBLPR4                       M23.3248
      IF (STYPE .LE. 0.0) GO TO 14890                                   M28.4619
      SCONST(N9 + 1) = - D13 * SINZ(D14)                                M23.3250
      SCONST(N9 + 2) = D16                                              M23.3251
      SCONST(N9 + 3) = D11 * DBLPR4 + D12 * DBLPR3                      M23.3252
      GO TO 14890                                                       M23.3253
14880 DBLPR1 = - D13                                                            
      CALL DEXPZ(DBLPR1, DBLPR2)                                                
      DBLPR1 = - D14                                                            
      CALL DCOSZ(DBLPR1, DBLPR3)                                                
      CALL DSINZ(DBLPR1, DBLPR4)                                                
      DBLPR1 = DBLPR2 * DBLPR3                                                  
      DBLPR2 = DBLPR2 * DBLPR4                                                  
      D17 = D13 * D13  +  D14 * D14                                             
      DBLPR3 = ((UNITY - DBLPR1) * D13 - DBLPR2 * D14) / D17            M23.3262
      DBLPR4 = (-(UNITY - DBLPR1) * D14 - DBLPR2 * D13) / D17           M23.3263
      D15 = D11 * (UNITY - DBLPR3) + D12 * DBLPR4                       M23.3264
      D16 = - D11 * DBLPR4 + D12 * (UNITY - DBLPR3)                     M23.3265
      IF (KODEBR(K) .LT. 0) GO TO 14890                                 M23.3266
      SCONST(J + 1) = DBLPR1                                                    
      SCONST(J + 2) = D15                                               M23.3268
      SCONST(J + 3) = D11 * (DBLPR3 - DBLPR1) - D12 * (DBLPR4 - DBLPR2) M23.3269
      IF (STYPE .LE. 0.0) GO TO 14890                                   M28.4620
      SCONST(N9 + 1) = DBLPR2                                                   
      SCONST(N9 + 2) = D16                                              M23.3272
      SCONST(N9 + 3) = D11 * (DBLPR4 - DBLPR2) + D12 * (DBLPR3 - DBLPR1)M23.3273
14890 IF (IABS(I0) .GT. 1) GO TO 14895                                  M23.3274
      IF (I0 .GT. 0) GO TO 14893                                        M23.3275
      CNVHST(N4 + 0) = D15 * (VOLT(N2) - VOLTK(N2))                     M23.3276
      IF (STYPE .LE. 0.0) GO TO 14900                                   M28.4621
      CNVHST(N4 + 1) = D16 * (VOLT(N2) - VOLTK(N2))                     M23.3278
      GO TO 14900                                                       M23.3279
14893 CNVHST(N4 + 0) = D15 * VOLT(N2)                                   M23.3280
      CNVHST(N4 + 1) = D15 * VOLTK(N2)                                  M23.3281
      IF (STYPE .LE. 0.0) GO TO 14900                                   M28.4622
      CNVHST(N4 + 2) = D16 * VOLT(N2)                                   M23.3283
      CNVHST(N4 + 3) = D16 * VOLTK(N2)                                  M23.3284
      GO TO 14900                                                       M23.3285
14895 CNVHST(N4 + 0) = 0.0                                              M23.3286
      CNVHST(N4 + 1) = 0.0                                              M23.3287
      IF (KODEBR(K) .LT. 0) GO TO 14897                                 M23.3288
      D16 = SCONST(J + 2)                                               M23.3289
      D17 = SCONST(J + 3)                                               M23.3290
      SCONST(J + 2) = DXP * D16                                         M23.3291
      SCONST(J + 3) = DXQ * D16 + DXP * D17                             M23.3292
      SCONST(J + 4) = DXQ * D17                                         M23.3293
14897 IF (STYPE .LE. 0.0) GO TO 14900                                   M28.4623
      CNVHST(N4 + 2) = 0.0                                              M23.3295
      CNVHST(N4 + 3) = 0.0                                              M23.3296
      IF (KODEBR(K) .LT. 0) GO TO 14900                                 M23.3297
      D16 = SCONST(N9 + 2)                                              M23.3298
      D17 = SCONST(N9 + 3)                                              M23.3299
      SCONST(N9 + 2) = DXP * D16                                        M23.3300
      SCONST(N9 + 3) = DXQ * D16 + DXP * D17                            M23.3301
      SCONST(N9 + 4) = DXQ * D17                                        M23.3302
14900 IF (ITADD .LT. 0) GO TO 14940                                     M23.3303
      IF (INDHST(K) .LT. 0) GO TO 14920                                 M23.3304
      CNVHST(N4) = 0.0                                                  M23.3305
      N4 = N4 + 1                                                       M23.3306
      IF (I0 .LT. 0) GO TO 14910                                        M23.3307
      CNVHST(N4) = 0.0                                                  M23.3308
      N4 = N4 + 1                                                       M23.3309
14910 IF (STYPE .LE. 0.0) GO TO 14940                                   M28.4624
      CNVHST(N4) = 0.0                                                  M23.3311
      N4 = N4 + 1                                                       M23.3312
      IF (I0 .LT. 0) GO TO 14940                                        M23.3313
      CNVHST(N4) = 0.0                                                  M23.3314
      N4 = N4 + 1                                                       M23.3315
      GO TO 14940                                                       M23.3316
14920 WFAC = TENM6                                                      M28.4625
      WSHZ = W*WFAC                                                     M28.4626
      SPOLE = D7*WFAC                                                   M28.4627
      WPOLE = -D8*WFAC                                                  M28.4628
      RESR = D9*WFAC                                                    M28.4629
      RESI = D10*WFAC                                                   M28.4630
      TNR = RESR * SPOLE - RESI * WPOLE                                 M27.1171
      TNI = RESR * WSHZ                                                 M28.4631
      TDR = SPOLE * SPOLE + WPOLE * WPOLE - WSHZ * WSHZ                 M28.4632
      TDI = 2. * SPOLE * WSHZ                                           M28.4633
      FAC = 1. / (TDR * TDR + TDI * TDI)                                M27.1175
      TDR = TDR * FAC                                                   M27.1176
      TDI = TDI * FAC                                                   M27.1177
      TRR = TDR * TNR + TDI * TNI                                       M27.1178
      TRI = TDR * TNI - TDI * TNR                                       M27.1179
      TRRSUM=TRRSUM+TRR                                                 M27.1180
      TRISUM=TRISUM+TRI                                                 M27.1181
      D15 = TRR * D1 - TRI * D2                                         M27.1182
      IF (IPRSUP .GE. 1)                                                M27.1183
     1 WRITE (LUNIT6,14922) D3, TRR, TRI, D15                           M27.1184
14922 FORMAT (42H VALUES OF D3,TRR,TRI,D15 BEFORE D3 UPDATE,            M27.1185
     1 4E17.8)                                                          M27.1186
      D3 = D3 + D15                                                     M23.3323
      IF (J.LT.N5.OR.STYPE.GT.0.0) GO TO 14924                          M28.4634
      EOUTK=D3-SOUTK                                                    M28.4635
      D3=SOUTK                                                          M28.4636
      D15=D15-EOUTK                                                     M28.4637
      IF (IPRSUP.GE.1)                                                  M28.4638
     1 WRITE (LUNIT6,14923) D15,EOUTK                                   M28.4639
14923 FORMAT ( 14H D15 RESET TO ,  E15.8,                               M29.1886
     1   29H TO CORRECT OUTPUT K ERROR = ,  E15.8 )                     M29.1887
14924 CNVHST(N4) = D15 - CNVHST(N4)                                     M28.4642
      N4 = N4 + 1                                                       M23.3325
      IF (I0 .LT. 0) GO TO 14930                                        M23.3326
      D21 = TRR * D5 - TRI * D6                                         M27.1187
      IF (IPRSUP.GE.1)                                                  M27.1188
     1 WRITE (LUNIT6, 14926) D4, TRR, TRI, D21                          M28.4643
14926 FORMAT (42H VALUES OF D4,TRR,TRI,D21 BEFORE D4 UPDATE,            M28.4644
     1 4E17.8)                                                          M27.1191
      D4 = D4 + D21                                                     M23.3330
      IF (J.LT.N5.OR.STYPE.GT.0.0) GO TO 14928                          M28.4645
      EOUTM=D4-SOUTM                                                    M28.4646
      D4=SOUTM                                                          M28.4647
      D21=D21-EOUTM                                                     M28.4648
      IF (IPRSUP.GE.1)                                                  M28.4649
     1 WRITE (LUNIT6,14927) D21,EOUTM                                   M28.4650
14927 FORMAT ( 14H D21 RESET TO ,  E15.8,                               M29.1888
     1   29H TO CORRECT OUTPUT M ERROR = ,  E15.8 )                     M29.1889
14928 CNVHST(N4) = D21 - CNVHST(N4)                                     M28.4653
      N4 = N4 + 1                                                       M23.3332
14930 IF (STYPE .LE. 0.0) GO TO 14940                                   M28.4654
      TRRSUM = TRRSUM + TRR                                             M28.4655
      TRISUM = TRISUM + TRI                                             M28.4656
      D3 = D3 + D15                                                     M27.1192
      TNR = RESI * SPOLE + RESR * WPOLE                                 M27.1193
      TNI = RESI * WSHZ                                                 M28.4657
      TIR = TDR * TNR + TDI * TNI                                       M27.1195
      TII = TDR * TNI - TDI * TNR                                       M27.1196
      D16 = TIR * D1 + TII * D2                                         M27.1197
      IF (IPRSUP .GE. 1)                                                M27.1198
     1 WRITE (LUNIT6,14932) D16, TIR, TII, D16                          M27.1199
14932 FORMAT (44H VALUES OF D16,TIR,TII,D16 BEFORE D16 UPDATE,          M27.1200
     1 4E17.8)                                                          M27.1201
      CNVHST(N4) = D16 - CNVHST(N4)                                     M23.3336
      N4 = N4 + 1                                                       M23.3337
      IF (I0 .LT. 0) GO TO 14940                                        M23.3338
      D4 = D4 + D21                                                     M27.1202
      D22 = TIR * D5 + TII * D6                                         M27.1203
      CNVHST(N4) = D22 - CNVHST(N4)                                     M23.3341
      N4 = N4 + 1                                                       M23.3342
14940 CONTINUE                                                          M23.3343
      GO TO MOON, (14390, 14520, 14630)                                 M23.3344
  650 K = 2 * IFDEP                                                     M16.2782
      IF ( IPRSUP  .GE.  3 )                                            M16.2783
     1 WRITE (LUNIT6, 653)  ( KODSEM(I), I=1, IBR )                     M17. 734
  653 FORMAT ( /,  22H (KODSEM(I), I=1, IBR)  ,/,  ( 1X, 10I10 ) )      M16.2785
      IF ( IPRSUP  .GE.  3 )                                            M16.2786
     1     WRITE (LUNIT6, 654) (CNVHST(I), I=1, IFX)                    M23.3345
      LSTAT(28) = ILINE - 1                                             M23.3346
      LSTAT(42) = IFX                                                   M23.3347
  654 FORMAT ( /,  22H (CNVHST(I), I=1, IFX)  ,/,  ( 1X, 8E16.6 ) )     M31.3047
      I = 0                                                                11631
      IF ( K  .LE.  0 )   GO TO 6679                                    M22.4586
      IF ( IPRSUP  .LE.  0 )   GO TO 6675                               M22.4587
      WRITE (LUNIT6, 2307)                                              M22.4588
 2307 FORMAT ( /,  81H EXPONENTIAL-TAIL CONSTANTS READY FOR DELTAT-LOOP M22.4589
     1CONVOLUTION, AT END  'OVER13' .  ,/,  10X,  1HI,                  M22.4590
     2 13X,  7HCON1(I),  13X,  7HCON2(I),  13X,  7HCON3(I)   )          M22.4591
 6675 I = I + 1                                                            11632
      IQ2 = 2 * LFDEP + I                                               M35.1400
      IQ3 = 4 * LFDEP + I                                               M35.1401
      IF( I .GT. K )  GO TO 6679                                           11633
      D3 = CON1(I)                                                      M35.1402
      D4 = CON1(IQ2)                                                    M35.1403
      D5 = CON1(IQ3)                                                    M35.1404
      IF ( D5  .EQ.  0.0 )   GO TO 2314                                 M22.4592
      D6 = D4 * DELTAT                                                     11638
      D1 = EXPZ(-D6)                                                    M18. 785
      D6 = (D1-1.0) / D6                                                   11640
      D2 = D3 / D4                                                         11641
      CON1(I) = D2 * (1.0+D6)                                           M35.1405
      CON1(IQ2) = -D2 * (D6+D1)                                         M35.1406
      CON1(IQ3) = D1                                                    M35.1407
 2314 IF ( IPRSUP  .GE.  2 )                                            M22.4593
     1 WRITE(LUNIT6, 55426)  I, CON1(I), CON1(IQ2), CON1(IQ3)           M35.1408
55426 FORMAT ( 1X,  I10,  3E20.11 )                                     M22.4594
      GO TO 6675                                                           11649
 6679 LSTAT(35) = IFTAIL                                                   11650
      IF ( IPRSUP  .GE.  1 )                                            M32.4441
     1 WRITE ( LUNIT6, 4568 )                                           M32.4442
 4568 FORMAT (  9H AT 4568.        )                                    M32.4443
      IF ( NUMSM  .GT.  0 )    GO TO 9200                               M32.4444
      IF ( KCONST .GT.  0 )    GO TO 9200                               M32.4445
      IF ( MDRIVE  .GT.  0 )   GO TO 9200                               M32.4446
      KILL = 191                                                        M19.1214
      LSTAT(19) = 1423                                                  M19.1215
 9200 IF ( IPRSUP  .GE.  1 )                                            M32.4447
     1 WRITE ( LUNIT6, 9207 )   KILL, LSTAT(19)                         M32.4448
 9207 FORMAT ( 42H EXIT  MODULE "LAST13".  KILL, LSTAT(19) =,           M32.4449
     1  2I8  )                                                          M32.4450
 9999 RETURN                                                            M29.1893
      END                                                                  11657
      SUBROUTINE REDU13(A,N,M)                                          M29.1894
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THIS SUBROUTINE CAN BE USED FOR EITHER PARTIAL REDUCTION OR FOR   M29.1895
C)    COMPLETE INVERSION OF A REAL  N BY N  SYMMETRIC MATRIX  'A' .     M29.1896
C)    STORAGE FOR MATRIX ELEMENTS IS IN THE IMPLIED ORDER   (1,1),      M29.1897
C)    (1,2), (2,2), (1,3), (2,3), (3,3), (1,4),  ETC.,  FOR THE         M29.1898
C)    COMPLETE UPPER TRIANGLE.   VARIABLES  M+1, ...., N  ARE TO BE     M29.1899
C)    ELIMINATED (WHICH IS THE DEFINITION OF VARIABLE  'M', NOTE).      M29.1900
C)    RESULT IS REDUCED MATRIX IN COLUMNS 1,...M IN CASE OF REDUCTION   M29.1901
C)    (M UNEQUAL 0) OR NEGATIVE INVERSE MATRIX IN COLUMNS 1,...N IN CASEM29.1902
C)    OF INVERSION (M=0).                                               M29.1903
      DIMENSION  A(1), B(100)                                           M29.1904
      J = N + 1                                                         M29.1905
      W=1.0                                                             M29.1906
      IF(M.GT.0) W=-W                                                   M29.1907
      IJ=N*J/2                                                          M29.1908
    3 J=J-1                                                             M29.1909
      IF(J.EQ.M) RETURN                                                 M29.1910
      H1=-1.0/A(IJ)                                                     M29.1911
      B(J)=H1                                                           M29.1912
      IJ=IJ-J                                                           M29.1913
      K=0                                                               M29.1914
      IK=0                                                              M29.1915
C                                   BEGIN K-LOOP                        M29.1916
    4 IK=IK+K                                                           M29.1917
      I1=IK+1                                                           M29.1918
      K=K+1                                                             M29.1919
      IF(K.GT.N) GO TO 3                                                M29.1920
      IF(K.LT.J) GO TO 9                                                M29.1921
      IF(W.LT.0.) GO TO 3                                               M29.1922
      IF(K.EQ.J) GO TO 7                                                M29.1923
      I=IK+J                                                            M29.1924
    5 H2=A(I)                                                           M29.1925
      B(K)=H2*H1                                                        M29.1926
C                                   BEGIN I-LOOP                        M29.1927
      I2=IK+K                                                           M29.1928
      L=0                                                               M29.1929
      DO 6 I=I1,I2                                                      M29.1930
      L=L+1                                                             M29.1931
    6 A(I)=A(I)+B(L)*H2                                                 M29.1932
      IF(K.LT.J) GO TO 4                                                M29.1933
      I=IK+J                                                            M29.1934
      A(I)=B(K)                                                         M29.1935
      GO TO 4                                                           M29.1936
C                                   END I-LOOP                          M29.1937
    7 I=IJ                                                              M29.1938
      DO 8 L=1,J                                                        M29.1939
      I=I+1                                                             M29.1940
    8 A(I)=B(L)                                                         M29.1941
      GO TO 4                                                           M29.1942
C                                   END K-LOOP                          M29.1943
    9 I=IJ+K                                                            M29.1944
      GO TO 5                                                           M29.1945
      END                                                               M29.1946
