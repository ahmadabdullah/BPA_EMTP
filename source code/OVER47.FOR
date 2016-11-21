      SUBROUTINE  SUBR47                                                M16.6325
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK47.FOR;/NOLIST'                                           
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      DIMENSION  LLTEMP(20)                                             M35.9239
      DIMENSION  RTG(1), ITG(1), CTG(1)                                 M35.9240
      COMPLEX*16  CTG                                                   M35.9241
      EQUIVALENCE  ( KARRAY(1), ITG(1), RTG(1), CTG(1) )                M35.9242
      IF ( IPRSUP  .GE.  1 )  WRITE ( LUNIT6, 4567 )                    M35.9243
 4567 FORMAT ( 24H  BEGIN MODULE "SUBR47". )                            M35.9244
      N8 = NCHAIN                                                       M35.9245
      IF ( KBURRO .EQ. 1)  N8 =29                                       M35.9246
 1000 CALL  DIMENS ( LLTEMP(1), N8, TRASH, TRASH )                      M35.9247
      N7 = LLTEMP(2) * NBYTE(4) / NBYTE(3)                              M35.9248
      CC = 2 * NBYTE(2)                                                 M35.9249
      PPA = NBYTE(3)                                                    M35.9250
      CC = CC / PPA                                                     M35.9251
      DD = 2 + KTRLSW(3)                                                M35.9252
      PPA = 12. + 12. * DD * DD * ( 1. + 3. * CC * CC )                 M35.9253
      PPB = 36. + DD * ( 5. + CC )                                      M35.9254
      CC = N7 - 50                                                      M35.9255
      CC = 2. * PPA * CC + PPB * PPB                                    M35.9256
      CC = ( SQRTZ(CC) - PPB ) / PPA                                    M35.9257
      LDM = CC                                                          M35.9258
      LDN = CC * DD                                                     M35.9259
      IF ( IPRSUP  .GT.  0 )  WRITE ( LUNIT6, 2000 )  LDM, LDN          M35.9260
 2000 FORMAT ( 33H  THE CABLE NUMBER IS LIMITED TO ,   I4,              M35.9261
     1   /,    37H  THE CONDUCTOR NUMBER IS LIMITED TO ,  I4  )         M35.9262
      LDN2 = LDN + LDN                                                  M35.9263
      LMQ = LDM * LDM                                                   M35.9264
      LNQ = LDN * LDN                                                   M35.9265
      LNQ2 = LNQ + LNQ                                                  M35.9266
      LOQ = 3 * LDM                                                     M35.9267
      IOF01 = 1                                                         M35.9268
      IOF02 = IOF01 + LDN                                               M35.9269
      IOF03 = ( IOF02 + LDM ) * NBYTE(4) / NBYTE(3) + 2                 M35.9270
      IOF04 = IOF03 + LDM                                               M35.9271
      IOF05 = IOF04 + LDM                                               M35.9272
      IOF06 = IOF05 + LDM                                               M35.9273
      IOF07 = IOF06 + LDM                                               M35.9274
      IOF08 = IOF07 + LDM                                               M35.9275
      IOF09 = IOF08 + LDM                                               M35.9276
      IOF10 = IOF09 + LDM                                               M35.9277
      IOF11 = IOF10 + LDM                                               M35.9278
      IOF12 = IOF11 + LDM                                               M35.9279
      IOF13 = IOF12 + LDM                                               M35.9280
      IOF14 = IOF13 + LDM                                               M35.9281
      IOF15 = IOF14 + LDM                                               M35.9282
      IOF16 = IOF15 + LDN                                               M35.9283
      IOF17 = IOF16 + LDN                                               M35.9284
      IOF18 = IOF17 + LDN                                               M35.9285
      IOF19 = IOF18 + LDN                                               M35.9286
      IOF20 = IOF19 + LNQ                                               M35.9287
      IOF21 = IOF20 + LNQ                                               M35.9288
      IOF22 = IOF21 + LNQ                                               M35.9289
      IOF23 = IOF22 + LNQ                                               M35.9290
      IOF24 = IOF23 + LMQ                                               M35.9291
      IOF25 = IOF24 + LMQ                                               M35.9292
      IOF26 = IOF25 + LMQ                                               M35.9293
      IOF27 = IOF26 + LMQ                                               M35.9294
      IOF28 = IOF27 + LMQ                                               M35.9295
      IOF29 = IOF28 + LMQ                                               M35.9296
      IOF30 = IOF29 + LOQ                                               M35.9297
      IOF31 = IOF30 + LOQ                                               M35.9298
      IOF32 = IOF31 + LOQ                                               M35.9299
      IOF33 = IOF32 + LOQ                                               M35.9300
      IOF34 = IOF33 + 3 * LDN                                           M35.9301
      IOF35 = IOF34 + 7 * LDM                                                   
      IOF36 = (IOF35 + LNQ2) * NBYTE(3) / (NBYTE(2)*2) + 2              M35.9303
      IOF37 = IOF36 + LDN                                               M35.9304
      IOF38 = IOF37 + LNQ                                               M35.9305
      IOF39 = IOF38 + LNQ                                               M35.9306
      IOF40 = IOF39 + LNQ                                               M35.9307
      IOF41 = IOF40 + LNQ                                               M35.9308
      IOF42 = IOF41 + LNQ                                               M35.9309
      IOF43 = IOF42 + LNQ                                               M35.9310
      IOF44 = IOF43 + LNQ                                               M35.9311
      IOF45 = IOF44 + LNQ                                               M35.9312
      IOF46 = IOF45 + LNQ                                               M35.9313
      IOF47 = IOF46 + LNQ                                               M35.9314
      IOF48 = IOF47 + LNQ                                               M35.9315
      IOF49 = IOF48 + LNQ                                               M35.9316
      IOF50 = IOF49 + LNQ                                               M35.9317
      IOF51 = IOF50 + LNQ                                               M35.9318
      IOF52 = IOF51 + LNQ                                               M35.9319
      IOF53 = IOF52 + LNQ                                               M35.9320
C     STEP OVER LAST VECTOR TO FIND TOTAL MEMORY REQUIREMENTS:          M35.9321
      N13 = ( IOF53 + LNQ2 ) * 2 * NBYTE(2) / NBYTE(3)                  M35.9322
      IF ( N13  .LE.  N7 )  GO TO 7835                                  M35.9323
      LSTAT(19) = 7835                                                  M35.9324
      KILL = 225                                                        M35.9325
      GO TO 7842                                                        M35.9326
 7835 CALL GUTS47 (    ITG(IOF01), ITG(IOF02), RTG(IOF03), RTG(IOF04),  M35.9327
     1     RTG(IOF05), RTG(IOF06), RTG(IOF07), RTG(IOF08), RTG(IOF09),  M35.9328
     2     RTG(IOF10), RTG(IOF11), RTG(IOF12), RTG(IOF13), RTG(IOF14),  M35.9329
     3     RTG(IOF15), RTG(IOF16), RTG(IOF17), RTG(IOF18), RTG(IOF19),  M35.9330
     4     RTG(IOF20), RTG(IOF21), RTG(IOF22), RTG(IOF23), RTG(IOF24),  M35.9331
     5     RTG(IOF25), RTG(IOF26), RTG(IOF27), RTG(IOF28), RTG(IOF29),  M35.9332
     6     RTG(IOF30), RTG(IOF31), RTG(IOF32), RTG(IOF33), RTG(IOF34),  M35.9333
     8     RTG(IOF35), CTG(IOF36), CTG(IOF37), CTG(IOF38), CTG(IOF39),  M35.9334
     8     CTG(IOF40), CTG(IOF41), CTG(IOF42), CTG(IOF43), CTG(IOF44),  M35.9335
     9     CTG(IOF45), CTG(IOF46), CTG(IOF47), CTG(IOF48), CTG(IOF49),  M35.9336
     1     CTG(IOF50), CTG(IOF51), CTG(IOF52), CTG(IOF53),              M35.9337
     2     LDM, LDN, LDN2, LNQ2  )                                      M35.9338
 7842 IF ( KILL .GT. 0 ) LSTAT(18) = NCHAIN                             M35.9339
      LASTOV = 47                                                       M35.9340
      RETURN                                                            M35.9341
      END                                                               M35.9342
      SUBROUTINE  GUTS47 ( NGG, NCPP, AL1I, AL2I, AL3I, DCI, THC, BIO,  M35.9343
     1 BI1, BI2, BI3, BI4, BI5, HI,  DI, GN, RAD, WY, ZY, YZ, SC, QC,   M35.9344
     2 DR0, TH0, AL0, DIJ, DIR, ANG, ROI, ESI, USI, USR, GI, RADI, YZN, M35.9345
     3    QN, YO, YS, YC, ZS, ZC, ZE, ZP, ZPC, A, AI, B, BI,            M35.9346
     4    CA, CB, CC, CD, F, LDM, LDN, LDN2, LNQ2  )                    M35.9347
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      INCLUDE  'VOLT45.FOR;/NOLIST'                                           
      COMPLEX*16  YS(LDN, LDN), YC(LDN, LDN), ZS(LDN, LDN), ZC(LDN, LDN)M35.9348
      COMPLEX*16  ZE(LDN, LDN), AI(LDN, LDN), BI(LDN, LDN), ZP(LDN, LDN)M35.9349
      COMPLEX*16  ZPC(LDN, LDN), A(LDN, LDN),  B(LDN, LDN), YO(LDN, LDN)M35.9350
      COMPLEX*16  QN( LDN ), CMPLXZ, CJ , F(LDN, LDN2)                  M35.9351
      COMPLEX*16  CA(LDN, LDN), CB(LDN, LDN), CC(LDN, LDN), CD(LDN, LDN)M35.9352
      REAL*8        BUFSEM, CNAME, TEXT1, TEXT2, TEXT3, TEXT4           M35.9353
      DIMENSION  BUFSEM(14)                                             M16.6329
      DIMENSION  NGG(LDN), NCPP(LDM), RAD(LDN), YZN(LNQ2), WY(LDN)      M35.9354
      DIMENSION  SC(LDN,LDN), QC(LDN,LDN), GI(LDN,3)                    M35.9355
      DIMENSION  DR0(LDM, LDM), TH0(LDM, LDM), AL0(LDM, LDM)            M35.9356
      DIMENSION  DIJ(LDM, LDM), DIR(LDM, LDM), ANG(LDM, LDM)            M35.9357
      DIMENSION  RADI(LDM, 7),   ZY(LDN, LDN),  YZ(LDN, LDN)            M35.9358
      DIMENSION  ROI(LDM, 3), ESI(LDM, 3), USI(LDM, 3), USR(LDM, 3)     M35.9359
      DIMENSION  BIO(LDM), BI1(LDM), BI2(LDM), BI3(LDM), BI4(LDM)       M35.9360
      DIMENSION  BI5(LDM), DCI(LDM), THC(LDM),  HI(LDM),  DI(LDN)       M35.9361
      DIMENSION  AL1I(LDM),   AL2I(LDM),   AL3I(LDM),   GN(LDN)         M35.9362
      DATA  TEXT1   /  6HPUNCH    /                                     M31.6704
      DATA  TEXT2   /  6H LINE   /                                      M35.9363
      DATA  TEXT3   /  6HCABLE   /                                      M35.9364
      NRP =  0                                                                  
      MRR =  0                                                                  
      JUNIT4=77                                                                 
      MISPUN = 0                                                        M31.6705
      LNQ1 = LDN * LDN + 1                                              M35.9365
      IF ( IPRSUP  .GE.  1 )                                            M23.5863
     1 WRITE (LUNIT6, 2314)  LASTOV, IALTER, LUNIT2, LUNIT5,            M23.5864
     2                       SPEEDL, EPSILN, TWOPI                      M23.5865
 2314 FORMAT ( /,  18H ENTER  'GUTS47' .,                               M43.4009
     1  32H  LASTOV  IALTER  LUNIT2  LUNIT5,  14X,  6HSPEEDL,           M23.5867
     2  14X,  6HEPSILN,  15X,  5HTWOPI  ,/,  18X,  4I8,  3E20.11  )     M23.5868
      REWIND LUNIT2                                                     M17.1386
      REWIND LUNIT3                                                     M17.1387
      REWIND LUNIT4                                                     M29.5593
      IF ( IPRSUP .GE. 1)                                               M43.4010
     1 write (LUNIT6,*)  ' %%--  Ready to rewind lunit9.'               M43.4011
      REWIND LUNIT9                                                     M19.3298
      NUMAKI = 0                                                        M19.3299
      NCMOD = 0                                                         M20.6324
      FZERO = 0.0                                                       M22.6581
      CIMAG1 = CMPLXZ(FZERO, UNITY)                                     M22.6582
      CREAL1 = CMPLXZ(UNITY,FZERO)                                      M22.6583
      CZERO = CMPLXZ(FZERO, FZERO)                                      M22.6584
      IF (LASTOV .EQ. 45)  NUMAKI = 9                                   M22.6585
      IF ( LASTOV .EQ. 39 )  NUMAKI = 9                                 M35.9366
      IF (LASTOV .EQ. 43)  NUMAKI = 9                                   M22.6586
      IF ( IALTER  .NE.  2 )   GO TO 7407                               M16.6334
      L5SAVE = LUNIT5                                                   M16.6335
      LUNIT5 = LUNIT2                                                   M16.6336
 7407 LDISFR  =  LOCF( VOLTK(1) )     -     LOCF( VOLTI(1) )            M16.6337
      SPDLGT = SPEEDL                                                   M16.6338
      LOGSIX = LUNIT6                                                   M16.6339
      IPRS47 = IPRSUP                                                   M16.6340
      TEN = 10.                                                         M16.6341
      VALUE2 = EPSILN                                                   M23.5869
      VALUE1 = .5 * VALUE2/1000.                                        M16.6343
      VALUE3 = 20. * 1000. / ALOGZ(TEN)                                 M18.1064
      VALUE4 = 1.7810 7241 7990 D0                                      M16.6345
      VALUE5 = TENM3                                                    M16.6346
      VALU14 = 2.3025 8509 3000 D0                                      M16.6348
      LIU = 0                                                           M19.3301
      PAI = TWOPI/2.                                                    M16.6349
      U0 = 2 * TWOPI * TENM6 / TEN                                      M23.5870
      SPL2 = SPEEDL ** 2                                                M16.6351
      E0 = 1./U0/SPL2                                                   M16.6352
      U2P=U0/TWOPI                                                      M16.6353
      E2P=E0*TWOPI                                                      M16.6354
      NPAIS = 0                                                         M31.6706
      NCROS = 0                                                         M31.6707
      IRSEP = 0                                                         M31.6708
      XMAJOR = 1.                                                       M31.6709
      RSG = 1.E10                                                       M31.6710
    5 NGRND = 0                                                         M35.9367
C     READ INPUT CARD USING CIMAGE.                                     M16.6355
      CALL CIMAGE                                                       M35.9368
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6357
 4230 FORMAT ( 13A6, A2 )                                               M16.6358
      IF ( IALTER  .NE.  2 )                                            M16.6359
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1388
      IF ( BUFSEM(1)  .NE.  TEXT1 )   GO TO 8214                        M31.6711
      MISPUN = 1                                                        M31.6712
      GO TO 5                                                           M31.6713
 8214 CONTINUE                                                          M31.6714
      DECODE(80,900,ABUFF)  N1, N2, N3, N4, N5, N6, N7, N8, N9          M28.7359
  900 FORMAT(20I5)                                                      M16.6362
      NENERG = 0                                                        M43.4012
      IF (N1 .NE. 0)   GO TO 6                                          M16.6363
      IF (N2 .NE. 0)   GO TO 4501                                       M16.6364
      IF (N3 .NE. 0)   GO TO  4501                                      M16.6365
      WRITE (KUNIT6, 1200)                                              M35.9369
 1200 FORMAT(50H+BLANK CARD TERMINATING  'CABLE CONSTANTS'  CASES.)     M16.6367
      CALL INTERP                                                       M35.9370
      IF ( LASTOV  .EQ.  1 )   GO TO 7439                               M16.6368
      N1 = LASTOV                                                       M16.6369
      LASTOV = NCHAIN                                                   M16.6370
      NCHAIN = N1                                                       M16.6371
      IF ( IALTER  .EQ.  2 )                                            M16.6372
     1 LUNIT5 = L5SAVE                                                  M16.6373
      IF (IPUNCH .EQ. 0)   GO TO 7496                                   M19.3302
      D1 = 0.0                                                          M19.3303
      WRITE (LUNIT9)  D1, D1, D1                                        M19.3304
      REWIND LUNIT9                                                     M19.3305
      IF (IPRS47 .GE. 1)                                                M19.3306
     1 WRITE (LUNIT6, 11200)  D1, D1, D1, IPUNCH                        M19.3307
11200 FORMAT (22H LAST RECORD ON UNIT9., 3E15.6, /, 9H IPUNCH =, I10)   M19.3308
 7496 GO TO 9900                                                        M19.3309
 7439 LASTOV = NCHAIN                                                   M16.6375
      NCHAIN = 51                                                       M22.6587
      GO TO 9900                                                        M16.6377
 4501 KILL = 172                                                        M16.6378
      LSTAT(14) = N1                                                    M16.6379
      LSTAT(19) = 4501                                                  M16.6380
      GO TO 9200                                                        M16.6381
    6 ITYPEC = N1                                                       M16.6382
      ISYST = N2                                                        M16.6383
      IEARTH = N4                                                       M16.6384
      KMODE = N5                                                        M16.6385
      IZFLAG = N6                                                       M18.1066
      IYFLAG = N7                                                       M18.1067
C     INITIALIZE OPTIONAL "PUNCH" CARD VARIABLES (WSM, NOV, 81):        M31.6715
      NPAIS = 0                                                         M31.6716
      NCROS = 0                                                         M31.6717
      IRSEP = 0                                                         M31.6718
      XMAJOR = 0.0                                                      M31.6719
      RSG = 0.0                                                         M31.6720
      CNAME = BLANK                                                     M31.6721
      IF ( MISPUN  .NE.  1 )   GO TO 8234                               M31.6722
C     READ INPUT CARD USING CIMAGE.                                     M31.6723
      CALL CIMAGE                                                       M31.6724
      DECODE ( 80, 903, ABUFF(1) ) NPAIS, NCROS, IRSEP, XMAJOR,         M31.6725
     1                              RSG, CNAME                          M31.6726
  903 FORMAT( 3I5, 2E10.1, A1 )                                         M31.6727
 8234 MISPUN = 0                                                        M31.6728
      GO TO (10, 20, 20) , ITYPEC                                       M22.6588
C     OVERHEAD LINE DATA INPUT                                          M16.6387
   10 NCCT = N3                                                         M16.6388
      IF (N9 .NE. 0)  NENERG = N9                                               
      WRITE (KUNIT6, 1212)  ITYPEC,ISYST,NCCT,IEARTH,KMODE,IZFLAG,      M35.9371
     1                        IYFLAG,NENERG                             M43.4014
 1212 FORMAT(21H+MISC. DATA FOR LINES, 1X, 8I3)                         M43.4015
      IF(NPAIS.NE.0) WRITE(LUNIT6,1213) NPAIS,XMAJOR,RSG,CNAME          M31.6729
 1213 FORMAT(21H MISC. DATA FOR LINES, I3, 2E10.3, A1 )                 M31.6730
      DO 13  I=1, NCCT                                                  M16.6391
      IM = 4*(I-1) + 1                                                  M16.6392
      IN = IM + 3                                                       M16.6393
C     READ INPUT CARD USING CIMAGE.                                     M16.6394
      CALL CIMAGE                                                       M16.6395
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6396
      IF ( IALTER  .NE.  2 )                                            M16.6397
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1391
      DECODE(80, 900, ABUFF(1) )  (NCPP(J), J=IM, IN)                   M16.6399
      WRITE (KUNIT6, 1220)                                              M35.9372
 1220 FORMAT (42H+ADDITIONAL DATA FOR PHASE & GROUND WIRES.)            M28.7360
C     READ INPUT CARD USING CIMAGE.                                     M16.6402
      CALL CIMAGE                                                       M18.1071
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6403
      IF ( IALTER  .NE.  2 )                                            M16.6404
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1393
      DECODE(80,901,ABUFF(1))  (RADI(I,J), J=1,4), (DR0(I,J), J=1,2)    M35.9373
  901 FORMAT(8E10.1)                                                    M16.6408
      WRITE (KUNIT6, 1230)                                              M35.9374
 1230 FORMAT (40H+GEOMETRICAL DATA OF BUNDLED CONDUCTORS.)              M16.6410
C     READ INPUT CARD USING CIMAGE.                                     M16.6411
      CALL CIMAGE                                                       M16.6412
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6413
      IF ( IALTER  .NE.  2 )                                            M16.6414
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1395
      DECODE(80,901,ABUFF(1)) ( ROI(I,J), USR(I,J), J=1, 2 )            M16.6416
      WRITE (KUNIT6, 1240)                                              M35.9375
 1240 FORMAT (49H+RESISTIVITY & PERMEABILITY OF PHASE & GD. WIRES.)     M28.7361
   13 CONTINUE                                                          M16.6419
      NCC= 0                                                            M16.6420
      NPC = 0                                                           M16.6421
      DO 15  I=1, NCCT                                                  M16.6422
      IM = 4*(I-1) + 1                                                  M16.6423
      IN = IM + 1                                                       M16.6424
      NCC= NCC+ NCPP(IM)                                                M16.6425
      NPC=NPC + NCPP(IM) + NCPP(IN)                                     M16.6426
   15 CONTINUE                                                          M16.6427
      NP2 = NPC                                                         M35.9376
      LINES = ( NPC + 1) / 2                                            M16.6429
      IM = -1                                                           M16.6430
      DO 19   J=1, LINES                                                M16.6431
      IM = IM + 2                                                       M16.6432
      IN = IM + 1                                                       M16.6433
C     READ INPUT CARD USING CIMAGE.                                     M16.6434
      CALL CIMAGE                                                       M16.6435
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6436
      IF ( IALTER  .NE.  2 )                                            M16.6437
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1397
      DECODE(80,901,ABUFF(1))  ( THC(I), DCI(I), DI(I), I=IM, IN)       M35.9377
      IF (J .GT. 1)   GO TO 19                                          M16.6440
      WRITE (KUNIT6, 16)                                                M35.9378
   16 FORMAT (46H+HEIGHTS AND HORIZONTAL DISTANCE OF EACH LINE.)        M16.6442
   19 CONTINUE                                                          M16.6443
      GO TO 85                                                          M19.3310
C     CABLE DATA INPUT                                                  M20.6327
   20 NPC = N3                                                          M16.6446
      NPP = N8                                                          M20.6328
      NGRND=N9                                                          M28.7362
      NCC = 0                                                           M20.6329
      NPC2 = 0                                                          M35.9379
      IF (ITYPEC .EQ. 3)   GO TO 3010                                   M20.6330
      WRITE (KUNIT6, 3210) ITYPEC, ISYST,NPC,IEARTH,KMODE,IZFLAG,       M35.9380
     1                     IYFLAG, NGRND                                M28.7363
 3210 FORMAT(22H+MISC. DATA FOR CABLES, 8I3)                            M28.7364
      IF(NPAIS.NE.0) WRITE(LUNIT6,3211) NPAIS,NCROS,IRSEP,XMAJOR,RSG,   M32, 602
     1                                  CNAME                           M32, 603
 3211 FORMAT(22H MISC. DATA FOR CABLES, 3I3 , 2E8.1, A1 )               M31.6732
      IF (ISYST .NE. -1)   GO TO 21                                     M16.6449
      IF (IEARTH .NE. 99)   GO TO 21                                    M16.6450
      KILL = 173                                                        M16.6451
      LSTAT(19) = 3210                                                  M16.6452
      GO TO 9200                                                        M16.6453
   21 NP2 = NPC * 3                                                     M22.6589
      GO TO 3050                                                        M20.6331
 3010 WRITE (KUNIT6, 3030)  ITYPEC, ISYST, NPC, IEARTH, KMODE, IZFLAG,  M35.9381
     1  IYFLAG, NPP, NGRND                                              M28.7366
 3030 FORMAT (26H+MISC. DATA FOR PIPE CABLE, 9I3)                       M28.7367
      IF(NPAIS.NE.0) WRITE(LUNIT6,3031) NPAIS,NCROS,XMAJOR,RSG,CNAME    M31.6733
 3031 FORMAT(26H MISC. DATA FOR PIPE CABLE, 2I3, 2E8.1, A1 )            M31.6734
      IF(NPP.NE.1) NPP=1                                                M31.6735
C     READ INPUT CARD USING CIMAGE                                      M20.6335
      CALL CIMAGE                                                       M20.6336
      DECODE(80, 4230, ABUFF(1))  BUFSEM                                M20.6337
      IF (IALTER .NE. 2)                                                M20.6338
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M20.6339
      DECODE(80,901,ABUFF(1)) (RADP(I), I=1,3), ROP, USP, ES1, ES2      M20.6340
      WRITE (KUNIT6, 3035)                                              M35.9382
 3035 FORMAT (21H+PIPE CHARACTERISTIC.)                                 M20.6342
      IF (RADP(3) .NE. 0.0) GO TO 3036                                  M27.3831
      RADP(3) = RADP(2) + 100. * EPSILN                                 M27.3832
      ES2 = 1.0                                                         M27.3833
C     READ INPUT CARD USING CIMAGE                                      M20.6343
 3036 CALL CIMAGE                                                       M27.3834
      DECODE(80,4230,ABUFF(1)) BUFSEM                                   M20.6345
      IF (IALTER .NE. 2)                                                M20.6346
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M20.6347
      DECODE(80,901,ABUFF(1)) (DCI(I), THC(I), I=1, NPC)                M20.6348
      WRITE (KUNIT6, 3040)                                              M35.9383
 3040 FORMAT (34H+RELATION BETWEEN CABLES AND PIPE.)                    M20.6350
      NP2 = NPC * 3  +  NPP                                             M22.6590
C     READ INPUT CARD USING CIMAGE.                                     M16.6456
 3050 CALL CIMAGE                                                       M20.6352
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6458
      IF ( IALTER  .NE.  2 )                                            M16.6459
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1400
      DECODE(80,900,ABUFF(1)) (NCPP(I),I=1,NPC)                         M16.6461
      WRITE (KUNIT6, 3220)                                              M35.9384
 3220 FORMAT (36H+NUMBER OF CONDUCTORS IN EACH CABLE.)                  M16.6463
      IF ( ITYPEC  .EQ.  2 )   GO TO 22                                 M22.6591
      I = NPC                                                           M20.6353
 3223 THC(I) = THC(I) - THC(1)                                          M20.6354
      I = I - 1                                                         M20.6355
      IF ( I  .GT.  0 )   GO TO 3223                                    M20.6356
   22 DO 23  I=1, NPC                                                   M22.6592
C     READ INPUT CARD USING CIMAGE.                                     M16.6465
      CALL CIMAGE                                                       M16.6466
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6467
      IF ( IALTER  .NE.  2 )                                            M16.6468
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1402
      DECODE(80, 901, ABUFF(1))  (RADI(I,J), J=1, 7)                    M22.6593
      WRITE (KUNIT6, 3230) (RADI(I,J), J=1, 7)                          M35.9385
 3230 FORMAT(7H+RADII., 1X, 7F6.2 )                                     M22.6595
C     READ INPUT CARD USING CIMAGE.                                     M16.6473
      CALL CIMAGE                                                       M16.6474
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6475
      IF ( IALTER  .NE.  2 )                                            M16.6476
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1404
      DECODE(80,901,ABUFF(1)) (ROI(I,J),USR(I,J),USI(I,J),ESI(I,J),     M29.5594
     1   J=1, 2)                                                        M16.6479
      WRITE (KUNIT6, 3240)                                              M35.9386
 3240 FORMAT(50H+PHYSICAL CONSTANTS FOR CONDUCTORS AND INSULATORS.)     M16.6481
      IF (NCPP(I) .LE. 2)  GO TO 3245                                   M22.6596
C     READ INPUT CARD USING CIMAGE.                                     M22.6597
      CALL CIMAGE                                                       M22.6598
      DECODE(80, 4230, ABUFF(1)) BUFSEM                                 M22.6599
      IF (IALTER .NE. 2)                                                M22.6600
     1 WRITE(LUNIT2, 4230)  BUFSEM                                      M22.6601
      DECODE(80,901,ABUFF(1)) ROI(I,3),USR(I,3),USI(I,3),ESI(I,3)       M22.6602
      WRITE(KUNIT6, 3240)                                               M35.9387
      IF ( RADI(I,7) .NE. 0. ) GO TO 3239                               M35.9388
      RADI(I,7)=RADI(I,6) + 100.*EPSILN                                 M35.9389
      USI(I,3) = 1.0                                                    M23.5873
      ESI(I,3) = 1.0                                                    M23.5874
      GO TO 3239                                                        M23.5875
 3245 USI(I,3) = 1.0                                                    M23.5876
      ESI(I,3) = 1.0                                                    M23.5877
      IF (NCPP(I) .EQ. 2)  GO TO 3235                                   M23.5878
      IF (RADI(I,3) .NE. 0. ) GO TO 3231                                M35.9390
      RADI(I,3)=RADI(I,2) + 100.*EPSILN                                 M23.5880
      USI(I,1) = 1.0                                                    M35.9391
      ESI(I,1) = 1.0                                                    M23.5882
 3231  RADI(I,4) = RADI(I,3)                                            M23.5883
       RADI(I,5) = RADI(I,4)                                            M23.5884
       RADI(I,6) = RADI(I,5)                                            M23.5885
       RADI(I,7) = RADI(I,6)                                            M23.5886
      USI(I,2) = 1.0                                                    M23.5887
      ESI(I,2) = 1.0                                                    M23.5888
      USR(I,2) = 1.0                                                    M23.5889
      USR(I,3) = 1.0                                                    M23.5890
      ROI(I,2) = ROI(I,1)                                               M23.5891
      ROI(I,3) = ROI(I,2)                                               M23.5892
      GO TO 3239                                                        M23.5893
 3235 IF (RADI(I,5) .NE. 0.) GO TO 3237                                 M35.9392
      RADI(I,5)=RADI(I,4) + 100.*EPSILN                                 M23.5895
23235 USI(I,2) = 1.0                                                    M28.7383
      ESI(I,2) = 1.0                                                    M23.5897
 3237  RADI(I,6) = RADI(I,5)                                            M23.5898
       RADI(I,7) = RADI(I,6)                                            M23.5899
      USR(I,3) = 1.0                                                    M23.5900
      ROI(I,3) = ROI(I,2)                                               M23.5901
 3239 IF(NCPP(I) .LT. 2)  GO TO 23                                      M23.5902
      NPC2 = NPC2 + 1                                                   M22.6605
      NCMOD = NCMOD + 1 - NGRND                                         M31.6737
      IF ( NCPP(I)  .GE.  3 )   NCMOD = NCMOD + 1                       M22.6607
   23 NCC = NCC + NCPP(I)                                               M22.6608
      NPC2=NPC+NPC2                                                     M22.6609
      IF (ITYPEC .EQ. 2)   GO TO 24                                     M20.6360
      NCC = NCC + NPP                                                   M20.6361
      IF (NPP .EQ. 0)   GO TO 85                                        M20.6362
C     READ INPUT CARD USING CIMAGE                                      M20.6363
      CALL CIMAGE                                                       M20.6364
      DECODE(80, 4230, ABUFF(1))  BUFSEM                                M20.6365
      IF (IALTER .NE. 2)                                                M20.6366
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M20.6367
      DECODE(80,901,ABUFF(1)) (HI(I), DI(I), I=1, NPP)                  M20.6368
      WRITE (KUNIT6, 3055)                                              M35.9393
 3055 FORMAT (45H+HEIGHT AND HORIZONTAL DISTANCE OF EACH PIPE.)         M20.6370
      GO TO 85                                                          M20.6371
   24 LINES = (NPC + 3) / 4                                             M20.6372
      DO 93  J = 1, LINES                                               M16.6484
      IM = 4 * (J-1) + 1                                                M16.6485
      IN = IM + 3                                                       M16.6486
C     READ INPUT CARD USING CIMAGE.                                     M16.6487
      CALL CIMAGE                                                       M16.6488
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6489
      IF ( IALTER  .NE.  2 )                                            M16.6490
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1406
      DECODE(80,901,ABUFF(1))  (HI(I), DI(I), I = IM, IN)               M16.6492
      IF (J .GT. 1)   GO TO 93                                          M16.6493
      WRITE (KUNIT6, 3250)                                              M35.9394
 3250 FORMAT(46H+HEIGHT AND HORIZONTAL DISTANCE OF EACH CABLE.)         M16.6495
   93 CONTINUE                                                          M16.6496
   85 IF(NPC .LE. LDM  .AND.  NCC .LE. LDN) GO TO 1945                  M35.9395
      KILL = 225                                                        M35.9396
      LSTAT(14) = LDN                                                   M35.9397
      LSTAT(15) = LDM                                                   M35.9398
      LSTAT(19) = 85                                                    M35.9399
      GO TO 9200                                                        M35.9400
 1945 DO 1960 I = 1, NCC                                                M35.9401
 1960 NGG(I) = 0                                                        M35.9402
      IF ( NGRND  .LE.  3 )  GO TO 3983                                 M35.9403
      CALL CIMAGE                                                       M35.9404
      DECODE(80, 1999, ABUFF(1)) (NGG(I), I = 1, NPC ), NPIPE           M35.9405
 1999 FORMAT ( 2X, 78I1 )                                               M35.9406
      DO 1919 I = 1, NPC                                                M35.9407
      IF ( NGG(I)  .LE.  1 ) GO TO 1919                                 M35.9408
      JDX1 = NPC + I                                                    M35.9409
      JDX2 = NPC2 + I                                                   M35.9410
      KGC = NGG(I)                                                      M35.9411
      IF ( KGC  .LT.  5 )  NGG(I) = 0                                   M35.9412
      IF (KGC .NE. 3  .AND.  KGC .NE. 6) NGG(JDX1) = 1                  M35.9413
      IF (KGC .NE. 2  .AND.  KGC .NE. 5) NGG(JDX2) = 1                  M35.9414
 1919 CONTINUE                                                          M35.9415
      IF ( ITYPEC  .EQ.  2 ) GO TO 3983                                 M35.9416
      NGG(NCC) = NPIPE                                                  M35.9417
 3983 IF (NUMAKI .EQ. 0)   GO TO 87                                     M35.9418
      WRITE (LUNIT4)  ITYPEC, ISYST, NPC, NCC                           M20.6374
      IF (IPRS47 .GE. 1)                                                M19.3312
     1 WRITE (LUNIT6, 86)  ITYPEC, ISYST, NPC, NCC                      M19.3313
   86 FORMAT(16H AT 86 OF SUBR47, /, 4I5)                               M19.3314
C     READ INPUT CARD USING CIMAGE                                      M16.6497
   87 CALL CIMAGE                                                       M20.6375
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6499
      IF ( IALTER  .NE.  2 )                                            M16.6500
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1408
      DECODE (80, 902, ABUFF)  D9,FREQ,IK,IPS,DIST,J13,J14              M43.4016
  902 FORMAT (2E15.6, 2I5, F8.3, I10, I2)                               M43.4017
      IPUNCH = J13                                                      M19.3318
      ROE = D9                                                          M16.6504
      ITRNSF = J14                                                              
      IF (FREQ .EQ. 0.)   FREQ = STATFR                                 M16.6505
      IF (IPS .EQ. 0)   IPS = 1                                         M16.6506
      WRITE (KUNIT6, 3256)  ROE, FREQ, IK, IPS, DIST,                   M43.4019
     1     IPUNCH, ITRNSF                                               M43.4020
 3256 FORMAT (11H+FREQ. CARD, 2E10.3, 2I3, E8.3, 2I2)                   M43.4021
      CALL INTERP                                                       M35.9420
      IF (IEARTH .NE. 99)   GO TO 95                                    M16.6509
C     READ INPUT CARD USING CIMAGE.                                     M16.6510
      CALL CIMAGE                                                       M16.6511
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6512
      IF ( IALTER  .NE.  2 )                                            M16.6513
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1410
      DECODE(80,901,ABUFF)  DEP1, DEP2, ROE3, ROE4                      M16.6515
      WRITE (KUNIT6, 3280)                                              M35.9421
 3280 FORMAT (49H+DEPTHS & RESISTIVITIES OF 2ND & 3RD LAYER EARTH.)     M28.7384
C     READ INPUT CARD USING CIMAGE.                                     M16.6518
      CALL CIMAGE                                                       M16.6519
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6520
      IF ( IALTER  .NE.  2 )                                            M16.6521
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1412
      DECODE(80,901,ABUFF)  HTOJ2, HTOJ3, HTOJ4, HYUD2, HYUD3, HYUD4    M16.6523
      WRITE (KUNIT6, 3290)                                              M35.9422
 3290 FORMAT (49H+PERMEABILITY & PERMITTIVITY OF THE THREE LAYERS.)     M28.7385
      ALF1 = ROE / ROE3                                                 M16.6526
      ALF2 = ROE / ROE4                                                 M16.6527
   95 CONTINUE                                                          M31.6738
      ANPAIS=IABS (NPAIS)                                               M31.6739
      XTOTAL=XMAJOR*ANPAIS                                              M31.6740
      IF(NPAIS.EQ.0) XTOTAL=XMAJOR                                      M31.6741
      IF(NPAIS.GE.0)                                                    M35.9423
     1   CALL NYAN(ITYPEC,NPC,NCC,NCPP,NGRND,NCROS,NPAIS,LDM)           M35.9424
   96 IF (ITYPEC .NE. 1)   GO TO 200                                    M31.6743
C     OVERHEAD LINE DATA OUTPUTS & PRECAL.                              M24. 792
      JNC = 1                                                           M16.6530
      DO 130  I=1, NCCT                                                 M16.6531
      IM = 4*(I-1) + 1                                                  M16.6532
      DO 120  J=1, 2                                                    M16.6533
      J1 = IM + J + 1                                                   M16.6534
      IN = NCPP(J1)                                                     M16.6535
      IF ( IN  .EQ.  0 )   GO TO 120                                    M16.6536
      J2 = 2 * J - 1                                                    M16.6537
      I1 = I + NCCT                                                     M16.6538
      IF (IN .EQ. 1)   GO TO 111                                        M16.6539
      D0 = DR0(I,J)/SINZ(PAI/IN)                                        M35.9425
      USI(I,J) = RADI(I,J2)                                             M16.6541
      IN1 = IN - 1                                                      M16.6542
      DO 110  K=1, IN1                                                  M16.6543
  110 USI(I,J)=USI(I,J)*D0*SINZ(PAI/IN*K)                               M18.1076
      USI(I1,J)=USI(I,J)**(1./IN)                                       M16.6545
      GO TO 120                                                         M16.6546
  111 USI (I1, J) = RADI(I,J2)                                          M16.6547
  120 CONTINUE                                                          M16.6548
      IF (IPRSUP .GE. 1)                                                M16.6549
     1  WRITE (LUNIT6, 4001)  I, (USI(I1, J), J=1, 2)                   M17.1414
 4001 FORMAT (1X,7HCIRCUIT,I3,3H0  ,32HEQUIVALENT RADIUS OF PHASE WIRE ,M16.6551
     1  E10.4,/, 34X, 11HGROUND WIRE, E10.4)                            M16.6552
      DO 125  J=1, 2                                                    M16.6553
      J1 = IM + J - 1                                                   M16.6554
      IF ( NCPP(J1)  .NE.  0 )   GO TO 123                              M16.6555
      USI(I,J) = 0.0                                                    M16.6556
      ESI(I,J) = 0.0                                                    M16.6557
      GI(I,J) = 0.0                                                     M16.6558
      GO TO 125                                                         M16.6559
  123 J1 = 2 * J                                                        M16.6560
      J2 = J1 - 1                                                       M16.6561
      USI(I,J)  =  RADI(I,J1) * SQRTZ( U0*USR(I,J) / ROI(I,J) )         M18.1077
      ESI(I,J)  =  RADI(I,J2) * SQRTZ( U0*USR(I,J) / ROI(I,J) )         M18.1078
      IF ( RADI(I,J1)  .LT.  RADI(I,J2) )   GO TO 124                   M16.6564
      KILL = 174                                                        M16.6565
      LSTAT(14) = I                                                     M16.6566
      FLSTAT(15) = RADI(I,J2)                                           M16.6567
      FLSTAT(16) = RADI(I,J1)                                           M16.6568
      LSTAT(19) = 123                                                   M16.6569
      GO TO 9200                                                        M16.6570
  124 GI(I,J) = ROI(I,J)/PAI/(RADI(I,J2)**2 - RADI(I,J1)**2)            M43.4022
C     1          /NCPP(IM+J+1)                                          M43.4023
  125 CONTINUE                                                          M16.6572
  130 CONTINUE                                                          M16.6573
      DO 136 J=1,2                                                      M16.6574
      DO 135 I=1,NCCT                                                   M16.6575
      I1=I+NCCT                                                         M16.6576
      IM=4*(I-1)+1                                                      M16.6577
      J1=IM+J-1                                                         M16.6578
      J1 = NCPP(J1)                                                     M16.6579
      IF(J1.EQ.0) GO TO 135                                             M16.6580
      J2 = J1 + JNC - 1                                                 M16.6581
      DO 133 K=JNC,J2                                                   M16.6582
  133 RAD(K)=USI(I1,J)                                                  M16.6583
      JNC = J2 + 1                                                      M16.6584
  135 CONTINUE                                                          M16.6585
  136 CONTINUE                                                          M16.6586
      WRITE (LUNIT6, 170)                                               M17.1415
  170 FORMAT(//, 132H---------------------------------------------------M16.6588
     1------------------------------------------------------------------M16.6589
     2---------------,//,1X)                                            M22.6610
      IF ( ISYST  .NE.  0 )   GO TO 140                                 M16.6591
      WRITE (LUNIT6, 180)                                               M17.1416
  180 FORMAT (47H TABLE OF OVERHEAD UNTRANSPOSED LINE PARAMETERS,/,1X)  M22.6611
      GO TO 142                                                         M16.6594
  140 WRITE (LUNIT6, 182)                                               M17.1417
  182 FORMAT (45H TABLE OF OVERHEAD TRANSPOSED LINE PARAMETERS,/,1X)    M22.6612
  142 J1 = NPC - NCC                                                    M16.6597
      WRITE (LUNIT6, 171)  NCC, J1                                      M17.1418
  171 FORMAT(   10X,27HTOTAL NUMBER OF PHASE WIRES,I3,3X,12HGROUND WIRESM16.6599
     1 ,I3, /, 1X)                                                      M22.6613
      DO 150  I=1, NCCT                                                 M16.6601
      I1 = I + NCCT                                                     M16.6602
      IM = 4*(I-1) + 1                                                  M16.6603
      J1 = IM + 1                                                       M16.6604
      J2 = IM + 2                                                       M16.6605
      J3 = IM + 3                                                       M16.6606
      WRITE (LUNIT6, 184)  I, ( RADI(I,J), J= 1,  2), NCPP(J2), USI(I1,1M17.1419
     +)                                                                 M17.1420
  184 FORMAT ( 10X, 7HCIRCUIT, I2, /, 10X,                              M16.6608
     1 27HRADIUS | PHASE WIRE  (OUTER,  E11.4,  9H  , INNER,            M16.6609
     2 E11.4,  3H )*,  I2,  8H BUNDLES, /, 20X,                         M16.6610
     3 13HEQUIV. RADIUS, E11.4  )                                       M16.6611
      J1=NCPP(J1)                                                       M16.6612
      IF(J1.EQ.0) GO TO 148                                             M16.6613
      WRITE (LUNIT6, 185)  ( RADI(I,J), J= 3,  4), NCPP(J3), USI(I1,2)  M17.1421
  185 FORMAT (19X,  18HGROUND WIRE (OUTER,  E11.4,  9H  , INNER,        M16.6615
     1 E11.4,  3H )*,  I2,  8H BUNDLES, /,  20X,                        M16.6616
     2 13HEQUIV. RADIUS, E11.4  )                                       M16.6617
  148 WRITE(LUNIT6,186) (ROI(I,J),J=1,2),(USR(I,J),J=1,2),(GI(I,J),J=1,2M17.1422
     +)                                                                 M17.1423
  186 FORMAT ( 10X,  31HRESISTIVITY(OHM-M) | PHASE WIRE,                M16.6619
     1 E11.4,  3X,  11HGROUND WIRE,  E11.4,  /,  10X,                   M16.6620
     2 23HRELATIVE PERMEABILITY |, 8X,  F11.4,  14X,  F11.4,  /,        M16.6621
     3 10X,  22HDC RESISTANCE(OHM/M) |,  9X,  E11.4,                    M16.6622
     4 14X,  E11.4  )                                                   M16.6623
  150 CONTINUE                                                          M16.6624
      DO 160   I=1, NPC                                                 M16.6625
  160 HI(I) = (2. * DCI(I) + THC(I) ) / 3.                              M35.9426
      WRITE (LUNIT6, 165)  (I, HI(I), I=1, NPC)                         M17.1424
  165 FORMAT(10X, 29HEFFECTIVE HEIGHT OF THE LINE , 5(I5, F12.5))       M16.6628
      WRITE(LUNIT6,915) (DI(I),I=1,NPC)                                 M17.1425
  915 FORMAT ( 10X,  29HDISTANCE BETWEEN PHASES     |,  5F17.5  )       M16.6630
      WRITE (LUNIT6, 917)  FREQ, IK, IPS                                M17.1426
  917 FORMAT (10X, 20HBEGINNING FREQUENCY ,5X,E15.6,5X,18HNUMBER OF DECAM16.6632
     1DES , I5, 5X, 32HNUMBER OF POINTS IN EACH DECADE , I5)            M16.6633
      WRITE(LUNIT6,913) ROE                                             M17.1427
  913 FORMAT (10X, 19HEARTH | RESISTIVITY, F12.5)                       M16.6635
      IF (IEARTH .NE. 99)                                               M20.6377
     1   GO TO 167                                                      M20.6378
      WRITE(LUNIT6,916) DEP1,DEP2,ROE,ROE3,ROE4,HTOJ2,HTOJ3,HTOJ4,      M17.1428
     1  HYUD2,HYUD3,HYUD4                                               M16.6638
  916 FORMAT(10X,13H3-LAYER EARTH ,/,10X,30HDEPTH FROM SURFACE , 1ST LAYM16.6639
     1ER, F7.2,3X,9H2ND LAYER,F8.2,3X,21H3RD LAYER TO INFINITE,/,       M16.6640
     2 10X, 19HEARTH RESISTIVITY 0, E18.2, 6X, E14.2, 6X, E18.2,/,      M16.6641
     3 10X,23HRELATIVE PERMEABILITY 0, E14.2, 6X,E14.2,6X,E18.2,/,      M16.6642
     4 10X,23HRELATIVE PERMITTIVITY 0, E14.2,6X,E14.2,6X,E18.2)         M16.6643
  167 NW = NPC                                                          M20.6379
      NZ = NPC                                                          M20.6380
      NGRND=NCC                                                         M28.7386
      GO TO 700                                                         M35.9427
C     CABLE DATA OUTPUTS AND PRECALCULATION                             M20.6382
  200 DO 210 I=1,NPC                                                    M16.6646
      RAD(I) = RADI(I,7)                                                M23.5903
      DO 210 J=1,3                                                      M22.6622
      GI(I,J) = SPEEDL/SQRTZ(ESI(I,J))                                  M19.3321
  210 CONTINUE                                                          M16.6655
      WRITE (LUNIT6, 170)                                               M17.1429
      IF (ISYST .GE. 0)   GO TO 211                                     M16.6657
      WRITE (LUNIT6, 280)                                               M17.1430
  280 FORMAT (39H TABLE OF UNDER-GROUND CABLE PARAMETERS,/,1X)          M22.6623
      GO TO 214                                                         M28.7387
  211 IF (ISYST .GT. 0)   GO TO 213                                     M16.6661
      WRITE (LUNIT6, 281)                                               M17.1431
  281 FORMAT(58H TABLE OF CABLE PARAMETERS FOR CABLES ON THE EARTH SURFAM16.6663
     1CE)                                                               M16.6664
      GO TO 214                                                         M28.7388
  213 WRITE(LUNIT6,282)                                                 M17.1432
  282 FORMAT (35H TABLE OF OVERHEAD CABLE PARAMETERS,/,1X)              M22.6624
  214 IF ( NGRND  .GT.  3 ) GO TO 1980                                  M35.9428
      IF ( NGRND  .NE.  0 )  GO TO 325                                  M35.9429
      NGRND = NCC                                                       M35.9430
      GO TO 215                                                         M35.9431
  325 IF ( NGRND  .GT.  1 )  GO TO 330                                  M35.9432
      NGRND = NCC                                                       M35.9433
      IF ( ITYPEC  .EQ.  2 ) GO TO 215                                  M35.9434
      NGRND = NCC - 1                                                   M35.9435
      GO TO 215                                                         M35.9436
  330 IF( NGRND .GT. 2 ) GO TO 3333                                     M35.9437
      NGRND = NPC2                                                      M35.9438
      GO TO 215                                                         M35.9439
 3333 NGRND = NPC                                                       M35.9440
      GO TO 215                                                         M35.9441
 1980 DO 1982 I = 1, NCC                                                M35.9442
      KPD = NCC + 1 - I                                                 M35.9443
      IF ( NGG(KPD)  .GT.  0 ) GO TO 1982                               M35.9444
      NGRND = KPD                                                       M35.9445
      GO TO 215                                                         M35.9446
 1982 CONTINUE                                                          M35.9447
      WRITE ( LUNIT6, 1777 )                                            M35.9448
 1777 FORMAT( 43H  NO UNGROUNDED CONDUCTOR NEEDS CALCULATION )          M35.9449
      CALL STOPTP                                                               
  215 IF (ITYPEC .EQ. 2)   GO TO 1215                                   M20.6385
      WRITE (LUNIT6, 370)                                               M20.6386
  370 FORMAT (17H PIPE TYPE CABLES,/,1X)                                M22.6625
      IF (NPP .NE. 0)   GO TO 216                                       M20.6388
      WRITE (LUNIT6, 371)                                               M20.6389
  371 FORMAT (31H EARTH RETURN PATH NOT INCLUDED)                       M20.6390
  216 WRITE (LUNIT6, 380)                                               M20.6391
  380 FORMAT (10X, 23H PIPE AND ITS INSULATOR)                          M20.6392
      IF (NPP .NE. 0)  GO TO 1216                                       M21. 503
      RADP(2) = FLTINF                                                  M21. 504
      RADP(3) = FLTINF                                                  M21. 505
 1216 WRITE (LUNIT6, 381)  (RADP(I), I=1,3), ROP, USP                   M21. 506
  381 FORMAT (10X, 19HPIPE | INNER RADIUS, E13.5, 2X, 5HOUTER, E13.5,   M20.6394
     1 2X, 30HOUTER RADIUS OF PIPE INSULATOR, E13.5,/, 18X, 18HRESISTIVIM20.6395
     2TY(OHM-M), E16.5, 2X, 21HRELATIVE PERMEABILITY, F8.1)             M20.6396
      WRITE (LUNIT6, 382)  ES1, ES2                                     M20.6397
  382 FORMAT (10X, 40HINSULATOR | RELATIVE PERMITTIVITY  INNER,         M20.6398
     1   F8.1, 2X, 5HOUTER, F8.1)                                       M20.6399
      IF (ES2 .EQ. 0.0)   ES2 = 1.0                                     M21. 507
      IF (NPP .NE. 0)                                                   M20.6400
     1 WRITE (LUNIT6, 383)  (I, HI(I), DI(I), I=1, NPP)                 M20.6401
  383 FORMAT (10X, 4HPIPE, I2, 27H | DIST. FROM EARTH SURFACE, F9.4,    M20.6402
     1 1HM, 3X, 17HDIST. FROM PIPE 1, F9.4, 1HM)                        M20.6403
      WRITE (LUNIT6, 390)                                               M20.6404
  390 FORMAT (10X, 28HINNER CONDUCTORS (SC CABLES))                     M20.6405
      WRITE (LUNIT6, 391)  (I,DCI(I), I=1, NPC)                         M20.6406
  391 FORMAT (10X, 24HDIST. FROM PIPE CENTER |, 6(I5, F9.4))            M20.6407
      WRITE (LUNIT6, 392)  (THC(I), I=1, NPC)                           M20.6408
  392 FORMAT (10X, 24HANGLE TO FIRST CABLE   |, 6(F14.4))               M20.6409
 1215 DO 217  I=1, NPC                                                  M20.6410
      JN=NCPP(I)                                                        M22.6626
      WRITE (LUNIT6, 284) I, (J, RADI(I,J), J=1, 7)                     M29.5595
  284 FORMAT(10X,5HPHASE,I2,1X,17HBOUNDARY RADII  | ,/10X,7(I3,E13.5))  M24. 793
  217 WRITE (LUNIT6, 285)  (ROI(I, J), J =1,3), (USR(I, J), J=1,3),     M22.6629
     1(J,USI(I,J),ESI(I,J),GI(I,J),J=1,JN)                              M29.5596
  285 FORMAT(18X,24HRESISTIVITY(OHM-M)| CORE,E12.5,3X,6HSHEATH,E12.5,3X,M22.6631
     15HARMOR,E12.5/18X,25HRELATIVE PERMEABILITY |  ,F12.5,9X,F12.5,8X,FM28.7400
     212.5/3(18X,9HINSULATOR,I2,25H| (RELATIVE) PERMEABILITY,F 6.2,3X,  M28.7401
     312HPERMITTIVITY,F6.2,3X,13HVELOCITY(M/S),E12.5/,1X))              M28.7402
      IF(ITYPEC . NE . 3 )  GO TO 220                                   M22.6635
      THREE = 3.0                                                       M22.6636
      RLIMIT = ( 1.0 + 2.0/SQRTZ(THREE)) * RADI(1,7)                    M22.6637
      IF(NPC.EQ.2) RLIMIT=2.0*RADI(1,7)                                 M22.6638
      IF(NPC.EQ.1) RLIMIT=RADI(1,7)                                     M22.6639
      IF(RADP(1).GT.RLIMIT) GO TO 221                                   M22.6640
      WRITE(LUNIT6,393)                                                 M22.6641
  393 FORMAT(1H0,10X,59HPHYSICALLY INNER CONDUCTORS CANNOT BE EXSISTED IM22.6642
     1N THE PIPE.    /11X,56HPLEASE CHECK THE RADII OF THE PIPE AND INNEM28.7403
     2R CONDUCTORS.   //,1X)                                            M28.7404
      CALL STOPTP                                                               
  220 WRITE(LUNIT6,914) (I, HI(I), I= 1,NPC )                           M22.6646
  914 FORMAT (10X, 29HDISTANCE FROM EARTH SURFACE |, 5(I5, F12.5))      M22.6647
  221 DO 230 I=1,NPC                                                    M22.6648
      IF ( I .EQ. 1 ) GO TO 223                                         M31.6766
      I1=I-1                                                            M31.6767
      IF(NCPP(I1).GE.NCPP(I)) GO TO 223                                 M31.6768
      WRITE(LUNIT6,289)                                                 M31.6769
      CALL STOPTP                                                               
  289 FORMAT(1H0,10X,38HINVALID DATA OF 'NCPP(I)', NO FURTHER           M31.6771
     1,50HCALCULATION.  IT SHOULD BE 'NCPP(I-1).GE.NCPP(I)'.            M31.6772
     2/11X,23HPLEASE CHECK YOUR DATA.  )                                M31.6773
  223 CONTINUE                                                          M31.6774
      BIO(I) = RADI(I,1) * SQRTZ(U0/ROI(I,1) *USR(I,1))                 M22.6649
      BI1(I) = RADI(I,2) * SQRTZ(U0/ROI(I,1) *USR(I,1))                 M22.6650
      AL1I(I) = ALOGZ(RADI(I,3) / RADI(I,2) )                           M31.6775
      BI2(I) = RADI(I,3) * SQRTZ(U0/ROI(I,2) * USR(I,2))                M22.6665
      BI3(I) = RADI(I,4) * SQRTZ(U0/ROI(I,2) * USR(I,2))                M22.6666
      AL2I(I) = ALOGZ(RADI(I,5) / RADI(I,4) )                           M22.6667
      BI4(I)=RADI(I,5)*SQRTZ(U0/ROI(I,3)*USR(I,3))                      M22.6668
      BI5(I)=RADI(I,6)*SQRTZ(U0/ROI(I,3)*USR(I,3))                      M22.6669
      AL3I(I) = ALOGZ(RADI(I,7) / RADI(I,6) )                           M22.6670
  230 CONTINUE                                                          M16.6696
      IF (IPRS47 .GT. 1)                                                M18.1085
     1 WRITE (LUNIT6, 2437)  NPC, NCC, NPP, NCCT, NUMAKI, LASTOV,       M23.5904
     2  IPUNCH, IALTER, IEARTH, ITYPEC, ISYST, KMODE, IZFLAG, IYFLAG    M23.5905
 2437 FORMAT ( /,  18H VARIOUS INTEGERS.,                               M23.5906
     1  40H     NPC     NCC     NPP    NCCT  NUMAKI,                    M23.5907
     2  40H  LASTOV  IPUNCH  IALTER  IEARTH  ITYPEC,                    M23.5908
     3  32H   ISYST   KMODE  IZFLAG  IYFLAG  ,/,  18X,  14I8  )         M23.5909
      IF ( IPRSUP  .GE.  2 )                                            M23.5910
     1 WRITE (LUNIT6, 2441)  ( I, BI1(I), BI2(I), BI3(I),               M23.5911
     2                         AL1I(I), AL2I(I), I=1, NPC )             M23.5912
 2441 FORMAT ( /, 75H DERIVED VECTORS OF LENGTH  'NPC'  IN  'SUBR47' ,  M23.5913
     1 AS MATRIX GRUNT BEGINS.  ,/,  1X,  5X,  3HROW,  17X,  3HBI1,     M23.5914
     2  17X,  3HBI2,  17X,  3HBI3,  16X,  4HAL1I,  16X,  4HAL2I  ,/,    M23.5915
     3 ( 1X,  I8,  5E20.11 )  )                                         M23.5916
      IF (ITYPEC .EQ. 3) CALL PTZY1(RADI,DCI,THC,DR0,TH0,AL0,LDM)       M35.9451
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6671
      NW = NPC                                                          M20.6422
      NZ = NCC                                                          M20.6423
      IF (ITYPEC .NE. 3)   GO TO 1710                                   M20.6425
      NW = NPP                                                          M20.6426
      IF (NPP) 1750, 1750, 1711                                         M20.6427
 1710 WRITE (LUNIT6, 915)  (DI(I), I=1, NPC)                            M20.6428
 1711 WRITE (LUNIT6, 917) FREQ, IK, IPS                                 M20.6429
      WRITE (LUNIT6, 913)  ROE                                          M20.6430
      IF (IEARTH .NE. 99)   GO TO 700                                   M20.6431
      WRITE (LUNIT6,916) DEP1,DEP2,ROE,ROE3,ROE4,HTOJ2,HTOJ3,HTOJ4,     M20.6432
     1  HYUD2, HYUD3, HYUD4                                             M20.6433
  700 IF ( IPRSUP  .GE.  3 )                                            M21. 515
     1 WRITE (LUNIT6, 1714) (I, RAD(I), RADP(I), DI(I), HI(I), I=1, NW) M21. 516
 1714 FORMAT ( /,   25H BEFORE CALL TO  'SIMP' .,  7X,  1HI,            M21. 517
     1 12X,  3HRAD,  11X,  4HRADP,  13X,  2HDD,  14X,  1HH  ,/,         M21. 518
     2 ( 25X,  I8,  4E15.6 )  )                                         M21. 519
      IF(NPAIS.EQ.0) GO TO 610                                          M31.6776
      WRITE(LUNIT6,919) CNAME                                           M31.6777
      GO TO 615                                                         M31.6778
  610 IF(NCROS.EQ.0) GO TO 714                                          M31.6779
  615 IF(ITYPEC.NE.1) GO TO 620                                         M31.6780
      WRITE(LUNIT6,920) XTOTAL,XMAJOR,NPAIS                             M31.6781
      GO TO 714                                                         M31.6782
  620 IF(NPAIS.GT.0 .AND. NCROS.EQ.0) GO TO 625                         M31.6783
      WRITE(LUNIT6,921) XTOTAL,XMAJOR,NPAIS,RSG                         M31.6784
      GO TO 630                                                         M31.6785
  625 WRITE(LUNIT6,922) XTOTAL,XMAJOR,NPAIS                             M31.6786
  630 IF(NCROS.NE.0) WRITE(LUNIT6,923)                                  M31.6787
      IF(NPAIS.LT.0) WRITE(LUNIT6,924)                                  M31.6788
      IF(NPAIS.GT.0) WRITE(LUNIT6,925)                                  M31.6789
  919 FORMAT(1H0,10X,45HDATA CARDS BY PI-CIRCUIT MODELING ARE PUNCHED,  M37.9104
     1 51H OUT FOR THE FOLLOWING SPECIFICATIONS ; NODE NAME ',A1,1H' )  M31.6791
  920 FORMAT(10X,31HTOTAL LENGTH OF OVERHEAD LINE =, E12.5/10X,         M31.6792
     1 26HLENGTH OF ONE PI-SECTION = , E12.5,3X,                        M31.6793
     2 23HNUMBER OF PI SECTIONS = ,I3  )                                M31.6794
  921 FORMAT(10X,23HTOTAL LENGTH OF CABLE = ,E12.5  /10X,               M31.6795
     1 29HLENGTH OF ONE MAJOR SECTION = ,E12.5,3X,                      M31.6796
     2 26HNUMBER OF MAJOR SECTIONS = ,I3 /10X,                          M31.6797
     3 46HSHEATH GROUNDING RESISTANCE AT MAJOR SECTION ; ,E12.5)        M31.6798
  922 FORMAT(10X,23HTOTAL LRNGTH OF CABLE = ,E12.5  /10X,               M31.6799
     1 29HLENGTH OF ONE MAJOR SECTION =  ,E12.5,3X,                     M31.6800
     2 26HNUMBER OF MAJOR SECTIONS = ,I3   )                            M31.6801
  923 FORMAT(10X,25HTHE CABLE IS CROSSBONDED.   )                       M31.6802
  924 FORMAT(10X,28HDISCRETE PI-CIRCUIT MODELING  )                     M31.6803
  925 FORMAT(10X,31HHOMOGENEOUS PI-CIRCUIT MODELING   )                 M31.6804
  714 CALL SIMP (NW, HI, DI, RAD, ZY, DIR, DIJ, ANG, LDM, LDN )         M35.9452
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6672
      IF ( IPRSUP  .LT.  3 )   GO TO 1750                               M21. 521
      WRITE (LUNIT6, 1719)  NW, NZ, ISYST, ITYPEC, IEARTH, NPC, NCC     M21. 522
 1719 FORMAT ( /, 24H AFTER CALL TO  'SIMP' .,                          M21. 523
     1 56H      NW      NZ   ISYST  ITYPEC  IEARTH     NPC     NCC  ,/, M21. 524
     2 24X,  7I8  )                                                     M21. 525
      WRITE (LUNIT6, 1721)  (  ( ZY(I,J), J=1, NW),  I=1, NW )          M21. 526
 1721 FORMAT ( /, 30H ((ZY(I,J), J=1, NW), I=1, NW)  ,/, ( 1X, 8E15.8) )M24. 794
 1750 CALL YMATRX( ISYST,LUNIT6,NCPP,ZY,YZ,ESI,AL0,AL1I,AL2I,AL3I,      M35.9453
     1            YZN(1), YZN(LNQ1), LDM, LDN  )                        M35.9454
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6674
      IF ( IPRSUP  .GE.  3 )                                            M21. 528
     1 WRITE (LUNIT6, 1723)  (  (YZ(I,J), J=1, NZ),  I=1, NZ )          M21. 529
 1723 FORMAT ( /,  58H ((YZ(I,J), J=1, NZ), I=1, NZ)   AFTER CALL TO  'YM21. 530
     1MATRX' .  ,/,  ( 1X,  8E15.8 )   )                                M24. 795
      DO 702  I=1, NZ                                                   M20.6436
      DO 702  J=1, NZ                                                   M20.6437
      IF ( J  .LT.  I )   GO TO 702                                     M16.6702
      YS(I,J) = CMPLXZ(YZ(I,J), FZERO)                                  M22.6675
      YS(J,I) = YS(I,J)                                                 M22.6676
  702 CONTINUE                                                          M16.6705
      CALL MINV ( YS, NZ, F, LDN, LDN2 )                                M35.9455
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6677
      IF (ITYPEC .NE. 1)   GO TO 703                                    M20.6441
      IF (ISYST .EQ. 2)                                                 M35.9456
     1  CALL TRANSP( YS,NCPP,YZN(1),YZN(LDN2),CA,LDM,LDN )              M35.9457
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6678
  703 DO 705  I=1, NZ                                                   M20.6443
      DO 705  J=1, NZ                                                   M20.6444
      IF ( J  .LT.  I )   GO TO 705                                     M16.6709
      YZ(I,J)  =  REALZ ( YS(I,J) )                                     M18.1089
      YZ(J,I) = YZ(I,J)                                                 M16.6711
  705 CONTINUE                                                          M16.6712
      NX=NGRND                                                          M31.6806
      IF(NPAIS.GE.0 .AND. NCROS.NE.0) NX=4                              M31.6807
      IF(NPAIS.NE.0)                                                    M35.9458
     1 CALL GOMEN(ITYPEC,NPC,NX,NPAIS,NCROS,IRSEP,NCPP,LDM)             M35.9459
      IF ( IPRSUP  .GE.  3 )                                            M23.5917
     1 WRITE (LUNIT6, 2457)  ( ( YZ(I,J), J=1, NZ), I=1, NZ )           M23.5918
 2457 FORMAT ( /, 64H IN  'SUBR47' ,   CAPACITANCE.   ( (YZ(I,J), J=1, NM43.4024
     1Z), I=1, NZ )    ,/,  ( 1X,  6E20.11 )  )                         M23.5920
      IPRINT = 0                                                        M16.6713
 9003 CONTINUE                                                                  
  706 IF ( CK1  .NE.  -FLTINF )   GO TO 7446                            M16.6714
      IF ( DIST  .LE.  0.0 )   GO TO 7446                               M16.6715
      CK1 = DIST                                                        M16.6716
 7446 IF ( CI1  .NE.  -FLTINF )   GO TO 7449                            M16.6717
      IF ( ROE     .LE.  0.0 )   GO TO 7449                             M16.6718
      CI1 = ROE                                                         M16.6719
 7449 IF ( IALTER  .EQ.  2 )                                            M16.6720
     1 ROE    = CI1                                                     M16.6721
      III = 0                                                           M16.6722
      KKK = 1                                                           M16.6723
      IF ( IPRSUP  .GE.  1 )                                            M23.5921
     1 WRITE (LUNIT6, 2469)  NZ, IK, IALTER, IPRSUP, ROE, CI1,          M23.5922
     2                       CK1, DIST, YZ(1,1), YZ(1,2)                M23.5923
 2469 FORMAT ( /, 40H IN  'SUBR47' ,   DONE WITH CAPACITANCE.,          M23.5924
     1  32H      NZ      IK  IALTER  IPRSUP  ,/,                        M23.5925
     2  40X,  4I8  ,/,  1X,  17X,  3HROE,  17X,  3HCI1,                 M23.5926
     3  17X,  3HCK1,  16X,  4HDIST,  13X,  7HYZ(1,1),  13X,  7HYZ(1,2)  M23.5927
     4  ,/,  1X,  6E20.11  )                                            M23.5928
      IF ( IK  .GT.  0 )   GO TO 7456                                   M16.6724
      IPRINT = IPRINT + 1                                               M16.6725
      IF ( LASTOV .NE. 39 )  GO TO 7451                                 M35.9460
      IF ( IPRINT .GT. 1 )  GO TO 7451                                  M35.9461
      KMODE = 1                                                         M43.4026
      L1 = 1                                                            M35.9463
      L0 = 0                                                            M43.4027
      D9 = DIST * TENM3                                                 M35.9464
CCC      write (*,*)  ' %%-- guts47 write on  lunit9.',                 M43.4028
CCC     1             '    DIST, D9, =',  DIST, D9,                     M43.4029
      WRITE (LUNIT9)  L1, L1, D9, L0, ITRNSF                            M43.4030
 7451 IF ( IALTER  .NE.  1 )   GO TO 3007                               M35.9466
      VOLTI(IPRINT) = ROE                                               M16.6727
      VOLTK(IPRINT) = FREQ                                              M16.6728
      IF ( IPRINT  .LE.  LDISFR )   GO TO 3007                          M16.6729
      KILL = 170                                                        M16.6730
      LSTAT(14) = LDISFR                                                M16.6731
      LSTAT(19) = 7456                                                  M16.6732
      GO TO 9200                                                        M16.6733
 7456 FACTOR = IPS                                                      M34. 198
      FACTOR = VALU14 / FACTOR                                          M34. 199
      FDECAD = FREQ                                                     M16.6735
      IPRINT = IPRINT + 1                                               M16.6736
      VOLTBC(1) = FREQ                                                  M31.6809
      VOLTBC(2) = FREQ * 10. ** IK                                      M31.6810
      VOLTBC(3) = EXPZ(FACTOR)                                          M31.6811
      D13 = VOLTBC(2) / VOLTBC(1)                                       M35.9467
      VOLTBC(4) = ALOGZ(D13) / ALOGZ(VOLTBC(3)) + 1.5                   M35.9468
      VOLTBC(5) = IK                                                    M35.9469
      VOLTBC(6) = IPS                                                   M35.9470
      write (*,*) ' guts47.   Below S.N. 7456, VOLTBC(6) = ',  VOLTBC(6)M43.4031
      DIST = 0.0                                                        M16.6740
      ICHECK = IPRINT - 1                                               M16.6742
      GO TO 3007                                                        M16.6743
 3005 III = III + 1                                                     M16.6744
      FDECAD = FDECAD * 10.0                                            M16.6745
      FREQ = FDECAD                                                     M16.6746
      IPRINT = IPRINT + 1                                               M16.6747
      KKK = 1                                                           M16.6748
      GO TO 3007                                                        M16.6749
 3006 PKKK = KKK                                                        M34. 200
      FREQ = FDECAD * EXPZ(PKKK * FACTOR)                               M34. 201
      KKK = KKK + 1                                                     M16.6751
      IF (KKK .GT. IPS)   GO TO 3005                                    M16.6752
      IPRINT = IPRINT + 1                                               M16.6753
      IF (IPRS47 .GE. 1)                                                M16.6754
     1   WRITE (LUNIT6, 23006)    IPRINT, KKK, III, FREQ, CI1, CK1      M17.1440
23006 FORMAT(/, 63H NEXT LOGARITHMACALLY SPACED FREQUENCY.  IPRINT     KM16.6756
     1KK     III, 11X, 4HFREQ,12X, 3HCI1,12X,3HCK1,/,39X,3I8,3E16.6)    M16.6757
 3007 IF (NUMAKI .EQ. 9)  GO TO 730                                     M22.6679
      TEXT4 = TEXT2                                                             
      IF ( ITYPEC .NE. 1 )  TEXT4 = TEXT3                                       
      WRITE (LUNIT6, 941)  TEXT4, FREQ                                  M35.9473
  941 FORMAT ( //, 20H *******************  ,  A6,                      M35.9474
     1             35H CONSTANTS MATRICES FOR FREQUENCY =,              M35.9475
     2   E15.6,    40H HZ   ********************************** )        M35.9476
  730 W = FREQ * TWOPI                                                  M22.6681
      IF (NUMAKI .EQ. 1)   FREQS = FREQ                                 M19.3324
      CALL ZYMX( W,NZ,ISYST,NGRND, NGG,NCPP, RADI,ZY,YZ,DIR,DIJ,        M35.9477
     1  ANG,USI,USR,ESI,DR0,TH0,AL0,HI,DI,BIO,BI1,BI2,BI3,BI4,BI5,      M35.9478
     2  AL1I,AL2I,AL3I,DCI,  NX, YZN,    YS,YC,ZP,ZPC,ZS,ZE,ZC,         M35.9479
     3  CA,CB,CC,CD,F,  LDM, LDN, LDN2, LNQ2 )                          M35.9480
  800 IF(NPAIS.EQ.0) GO TO 810                                          M31.6812
      WRITE(LUNIT6,951)                                                 M31.6813
      NPK = LNQ2 / 3                                                    M35.9481
      NKI = NPK + 1                                                     M35.9482
      NKJ = NKI + NPK                                                   M35.9483
      CALL DATOUT(W,ZC,YC,RSG,XMAJOR,NX,NPAIS,NCROS,IRSEP,CNAME,        M35.9484
     1            LDN, YZN(1), YZN(NKI), YZN(NKJ), NPK )                M35.9485
  951 FORMAT(//1H0,10X,41H*****LISTS OF PUNCHED OUT DATA CARDS***** )   M31.6815
  810 CONTINUE                                                          M31.6816
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6682
      IF (KMODE .EQ. 0)   GO TO 750                                     M36.3637
      KTAB = NX                                                                 
      CALL PRCON( W, NX, ZC,ZS,YS,YC,YO,QN,GN,ZE,A,AI,B,BI,             M43.4034
     1 YZN, CA, CB, CC, F, LDN, LDN2, LNQ2, MRR, NRP )                  M43.4035
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6700
      IF (LASTOV .NE. 43)   GO TO 750                                   M19.3355
      IF (IPUNCH .EQ. 0)   GO TO 750                                    M19.3356
      IF (IK .GT. 0)   GO TO 2745                                       M19.3357
      IF (LIU .NE. 0)   GO TO 2745                                      M19.3358
      CCZERO = AIMAGZ(YS(1,1))/W/TENM6                                  M22.6701
      WRITE(LUNIT9)   CCZERO                                            M22.6702
      LIU = 1                                                           M19.3361
      IF (IPRS47 .GE. 1)                                                M19.3362
     1 WRITE(LUNIT6, 1745)  FREQ, CCZERO                                M22.6703
 1745 FORMAT (25H CAPACITANCE ON UNIT9 AT , E15.6, 3HHZ., 5X, E16.8)    M19.3364
 2745 RZERO = REALZ(ZS(1,1))                                            M19.3365
      XZERO = AIMAGZ( ZS(1,1) ) / W * 1000.                             M22.6704
      WRITE (LUNIT9)  RZERO, XZERO, FREQ                                M19.3367
      IF (IPRS47 .GE. 1)                                                M19.3368
     1 WRITE (LUNIT6, 3745)  RZERO, XZERO, FREQ                         M19.3369
 3745 FORMAT (21H R, L AND F ON UNIT9., 5X, 3E16.8)                     M19.3370
  750 IF (III .EQ. IK)   GO TO 9001                                             
      GO TO 3006                                                        M16.6773
CC                                            ** IDENTICAL EIGENVALUE ? M43.4037
 9001 IF (IPRINT.EQ.1) GO TO 9002                                               
      IF (NRP.GT.0) GO TO 9002                                                  
      RTIO=(MRR*1.0)/(IPRINT-1.0)                                               
      IF (RTIO.LT.0.75) GO TO 9002                                              
      NRP=1                                                                     
      FREQ=FREQSV                                                               
      DO 9004 I=1,(IPRINT-1)*KTAB                                               
      BACKSPACE LUNIT9                                                          
      write (*,*) ' GUTS47.  Backspace lunit 9.  iprint, i = ', IPRINT  M43.4046
 9004 CONTINUE                                                                  
      DO 9005 I=1,(IPRINT-1)                                                    
      BACKSPACE JUNIT4                                                          
 9005 CONTINUE                                                                  
      write (*,*) ' Backspace junit4.  junit4, iprint =',               M43.4051
     1                                 junit4, iprint                   M43.4052
      IPRINT=1                                                                  
      GO TO 9003                                                                
 9002 CONTINUE                                                                  
C     READ INPUT CARD USING CIMAGE                                      M19.3390
 2899 CALL CIMAGE                                                       M19.3391
      DECODE (80, 4230, ABUFF(1) )   BUFSEM                             M16.6776
      IF ( IALTER  .NE.  2 )                                            M16.6777
     1 WRITE (LUNIT2, 4230)  BUFSEM                                     M17.1444
      DECODE (80, 902, ABUFF)  D9, FREQ, IK, IPS, DIST, J13,J14         M43.4056
      FREQSV=FREQ                                                               
      IF (D9 .NE. 0.)   GO TO 3899                                      M20.6524
      IF (FREQ .EQ. 0.)  GO TO 929                                      M20.6525
 3899 ROE = D9                                                          M20.6526
      IPUNCH = J13                                                      M43.4058
      ITRNSF = J14                                                      M43.4059
      IF (FREQ .EQ. 0.)   FREQ = STATFR                                 M16.6782
      IF (IPS .EQ. 0)   IPS = 1                                         M16.6783
      WRITE (KUNIT6, 3256)  ROE, FREQ, IK, IPS, DIST,                   M43.4060
     1                      IPUNCH, ITRNSF                              M43.4061
      IF (IEARTH .NE. 99)   GO TO 706                                   M16.6785
C     READ INPUT CARD USING CIMAGE.                                     M16.6786
      CALL CIMAGE                                                       M16.6787
      DECODE(80,4230,ABUFF(1))   BUFSEM                                 M16.6788
      IF (IALTER .NE. 2)                                                M16.6789
     1 WRITE (LUNIT2, 4230)   BUFSEM                                    M17.1446
      DECODE(80,901,ABUFF)  DEP1,DEP2,ROE3,ROE4                         M16.6791
      WRITE (KUNIT6, 3280)                                              M35.9489
C     READ INPUT CARD USING CIMAGE.                                     M16.6793
      CALL CIMAGE                                                       M16.6794
      DECODE(80,4230,ABUFF(1))   BUFSEM                                 M16.6795
      IF (IALTER .NE. 2)                                                M16.6796
     1 WRITE (LUNIT2, 4230)   BUFSEM                                    M17.1448
      DECODE(80,901,ABUFF) HTOJ2,HTOJ3,HTOJ4,HYUD2,HYUD3,HYUD4          M16.6798
      WRITE (KUNIT6, 3290)                                              M35.9490
      ALF1 = ROE/ROE3                                                   M16.6800
      ALF2 = ROE/ROE4                                                   M16.6801
      GO TO 706                                                         M16.6802
  929 WRITE (KUNIT6, 930)                                               M35.9491
  930 FORMAT ( 40H+BLANK CARD TERMINATING FREQUENCY CARDS.  )           M35.9492
      CALL INTERP                                                               
      WRITE (LUNIT6, 4239)                                              M35.9494
 4239 FORMAT ( ///,  1X )                                               M35.9495
      GO TO 5                                                           M16.6805
 9200 LSTAT(18) = NCHAIN                                                M16.6806
      IF (IALTER .EQ. 2)                                                M16.6807
     1 LUNIT5 = L5SAVE                                                  M16.6808
      LASTOV=NCHAIN                                                     M16.6809
      NCHAIN=51                                                         M16.6810
 9900 IF ( IPRSUP  .GE.  1 )                                            M23.5938
     1 WRITE (LUNIT6, 2592)  NCHAIN, LUNIT5, NUMAKI, IALTER             M23.5939
 2592 FORMAT ( /,  17H EXIT  'SUBR47' .,                                M23.5940
     1  32H  NCHAIN  LUNIT5  NUMAKI  IALTER  ,/,  17X,  4I8  )          M23.5941
      RETURN                                                            M23.5942
      END                                                               M16.6812
      SUBROUTINE CROSA4(CZY,ICONT,LDN,CA,CB,CC,CD,CE,CF,CG,F,LDN2)      M35.9496
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      COMPLEX*16  CZY(LDN,LDN)                                          M35.9497
      COMPLEX*16  CA(LDN,LDN), CB(LDN,LDN), CC(LDN,LDN), CD(LDN,LDN)    M35.9498
      COMPLEX*16  CE(LDN,LDN), CF(LDN,LDN), CG(LDN,LDN), F(LDN, LDN2)   M35.9499
      COMPLEX*16  CWORK1                                                M32, 606
      WRITE ( 6, 1001 )                                                 M35.9500
 1001 FORMAT ( 33H  BEGIN MODULE "CROSA4".  ******  )                   M35.9501
      CWORK1=0.                                                         M32, 607
      DO 10 I=4,6                                                       M32, 608
      CWORK1 = CWORK1 + CZY(I,I)                                        M32, 609
   10 CONTINUE                                                          M32, 610
      CWORK1 = CWORK1/3.                                                M32, 611
      DO 20 I=4,6                                                       M32, 612
      CZY(I,I) = CWORK1                                                 M32, 613
   20 CONTINUE                                                          M32, 614
      CWORK1 = CZY(4,5) + CZY(4,6) + CZY(5,6)                           M32, 615
      CWORK1 = CWORK1/3.                                                M32, 616
      CZY(4,5) = CWORK1                                                 M32, 617
      CZY(4,6) = CWORK1                                                 M32, 618
      CZY(5,6) = CWORK1                                                 M32, 619
      DO 30 I = 1,3                                                     M32, 620
      CWORK1 = 0.                                                       M32, 621
      DO 40 J = 4,6                                                     M32, 622
      CWORK1 = CWORK1 + CZY(I,J)                                        M32, 623
   40 CONTINUE                                                          M32, 624
      CWORK1 = CWORK1/3.                                                M32, 625
      DO 50 J = 4,6                                                     M32, 626
      CZY(I,J) = CWORK1                                                 M32, 627
   50 CONTINUE                                                          M32, 628
   30 CONTINUE                                                          M32, 629
      DO 60 I = 1,6                                                     M32, 630
      DO 60 J = 1,6                                                     M32, 631
      IF(I.LE.3.AND.J.LE.3) GO TO 60                                    M32, 632
      IF(I.GE.J) GO TO 60                                               M32, 633
      CZY(J,I) = CZY(I,J)                                               M32, 634
   60 CONTINUE                                                          M32, 635
      IF(ICONT.EQ.0) CALL MINVN( CZY,6,3,0,LDN,                         M35.9502
     1               CA,CB,CC,CD,CE,CF,CG,F,LDN2 )                      M35.9503
      DO 70 J = 1,3                                                     M32, 637
      CZY(4,J) = CZY(4,J) + CZY(5,J) + CZY(6,J)                         M32, 638
      CZY(J,4) = CZY(J,4) + CZY(J,5) + CZY(J,6)                         M32, 639
   70 CONTINUE                                                          M32, 640
      CZY(4,4) = CZY(4,4) + CZY(5,4) + CZY(6,4)                         M32, 641
     1         + CZY(4,5) + CZY(5,5) + CZY(6,5)                         M32, 642
     2         + CZY(4,6) + CZY(5,6) + CZY(6,6)                         M32, 643
      IF(ICONT.EQ.0) CALL MINVN( CZY,4,3,0,LDN,                         M35.9504
     1               CA,CB,CC,CD,CE,CF,CG,F,LDN2 )                      M35.9505
      RETURN                                                            M31.6879
      END                                                               M31.6880
      SUBROUTINE MINVN( CINOUT, N, L, IX, LDN, CA, CB, CC, CD,          M35.9506
     1                  CWORK1, CWORK2, CWORK3, F, LDN2 )               M35.9507
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      COMPLEX*16  CA(LDN,LDN),CB(LDN,LDN),CC(LDN,LDN),CD(LDN,LDN)       M35.9508
      COMPLEX*16  CWORK1(LDN,LDN),CWORK2(LDN,LDN),CWORK3(LDN,LDN)       M35.9509
      COMPLEX*16  CINOUT(LDN,LDN), F(LDN, LDN2)                         M35.9510
      NML=N-L                                                           M31.6944
      CALL CUTMAT(CINOUT,CA,CB,CC,CD,N,L,LDN)                           M35.9511
      DO 10 I=1,NML                                                     M31.6946
      DO 10 J=1,NML                                                     M31.6947
      CWORK1(I,J)=CD(I,J)                                               M31.6948
   10 CONTINUE                                                          M31.6949
      CALL MINV(CWORK1,NML,F,LDN,LDN2)                                  M35.9512
      CALL MXMNM(CWORK1,CC,CWORK2,NML,NML,L,LDN)                        M35.9513
      CALL MXMNM(CB,CWORK2,CWORK1,L,NML,L,LDN)                          M35.9514
      DO 20 I=1,L                                                       M31.6953
      DO 20 J=1,L                                                       M31.6954
      CWORK1(I,J)=CA(I,J)-CWORK1(I,J)                                   M31.6955
      CINOUT(I,J)=CWORK1(I,J)                                           M31.6956
   20 CONTINUE                                                          M31.6957
      IF(IX.EQ.1) RETURN                                                M31.6958
      CALL MINV(CWORK1,L,F,LDN,LDN2)                                    M35.9515
      DO 30 I=1,L                                                       M31.6960
      DO 30 J=1,L                                                       M31.6961
      CINOUT(I,J)=CWORK1(I,J)                                           M31.6962
   30 CONTINUE                                                          M31.6963
      CALL MXMNM(CWORK2,CWORK1,CWORK3,NML,L,L,LDN)                      M35.9516
      DO 40 I=1,NML                                                     M31.6965
      DO 40 J=1,L                                                       M31.6966
      IPL=I+L                                                           M31.6967
      CINOUT(IPL,J)=-CWORK3(I,J)                                        M31.6968
   40 CONTINUE                                                          M31.6969
      DO 50 I=1,L                                                       M31.6970
      DO 50 J=1,L                                                       M31.6971
      CWORK1(I,J)=CA(I,J)                                               M31.6972
   50 CONTINUE                                                          M31.6973
      CALL MINV(CWORK1,L,F,LDN,LDN2)                                    M35.9517
      CALL MXMNM(CWORK1,CB,CWORK2,L,L,NML,LDN)                          M35.9518
      CALL MXMNM(CC,CWORK2,CWORK1,NML,L,NML,LDN)                        M35.9519
      DO 60 I=1,NML                                                     M31.6977
      DO 60 J=1,NML                                                     M31.6978
      CWORK1(I,J)=CD(I,J)-CWORK1(I,J)                                   M31.6979
   60 CONTINUE                                                          M31.6980
      CALL MINV(CWORK1,NML,F,LDN,LDN2)                                  M35.9520
      DO 70 I=1,NML                                                     M31.6982
      DO 70 J=1,NML                                                     M31.6983
      IPL=I+L                                                           M31.6984
      JPL=J+L                                                           M31.6985
      CINOUT(IPL,JPL)=CWORK1(I,J)                                       M31.6986
   70 CONTINUE                                                          M31.6987
      CALL MXMNM(CWORK2,CWORK1,CWORK3,L,NML,NML,LDN)                    M35.9521
      DO 80 I=1,L                                                       M31.6989
      DO 80 J=1,NML                                                     M31.6990
      JPL=J+L                                                           M31.6991
      CINOUT(I,JPL)=-CWORK3(I,J)                                        M31.6992
   80 CONTINUE                                                          M31.6993
      RETURN                                                            M31.6994
      END                                                               M31.6995
      SUBROUTINE CUTMAT(CINPUT,CA,CB,CC,CD,N,L,LDN)                     M35.9522
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      COMPLEX*16  CINPUT( LDN, LDN )                                    M35.9523
      COMPLEX*16  CA(LDN,LDN),CB(LDN,LDN),CC(LDN,LDN),CD(LDN,LDN)       M35.9524
      DO 10 I=1,L                                                       M31.6998
      DO 10 J=1,L                                                       M31.6999
      CA(I,J)=CINPUT(I,J)                                               M31.7000
   10 CONTINUE                                                          M31.7001
      NML=N-L                                                           M31.7002
      DO 20 I=1,L                                                       M31.7003
      DO 20 J=1,NML                                                     M31.7004
      JPL=J+L                                                           M31.7005
      CB(I,J)=CINPUT(I,JPL)                                             M31.7006
   20 CONTINUE                                                          M31.7007
      DO 30 I=1,NML                                                     M31.7008
      DO 30 J=1,L                                                       M31.7009
      IPL=I+L                                                           M31.7010
      CC(I,J)=CINPUT(IPL,J)                                             M31.7011
   30 CONTINUE                                                          M31.7012
      DO 40 I=1,NML                                                     M31.7013
      DO 40 J=1,NML                                                     M31.7014
      IPL=I+L                                                           M31.7015
      JPL=J+L                                                           M31.7016
      CD(I,J)=CINPUT(IPL,JPL)                                           M31.7017
   40 CONTINUE                                                          M31.7018
      RETURN                                                            M31.7019
      END                                                               M31.7020
      SUBROUTINE MXMNM(CA,CB,CC,L,M,N,LDN)                              M35.9525
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      COMPLEX*16  CA(LDN,LDN),CB(LDN,LDN),CC(LDN,LDN)                   M35.9526
      DO 10 I=1,L                                                       M31.7023
      DO 20 J=1,N                                                       M31.7024
      CC(I,J)=0.                                                        M31.7025
      DO 30 K=1,M                                                       M31.7026
      CC(I,J)=CC(I,J)+CA(I,K)*CB(K,J)                                   M31.7027
   30 CONTINUE                                                          M31.7028
   20 CONTINUE                                                          M31.7029
   10 CONTINUE                                                          M31.7030
      RETURN                                                            M31.7031
      END                                                               M31.7032
      SUBROUTINE DATOUT( W,ZC,YC,RS,XMAJOR,NUB6,NPAIS,NNCROS,           M35.9527
     1  IRSEP, CHA, LDN, R, AL, C , NPK )                               M35.9528
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C 8.19 DATA CARDS PUNCH OUT SUBROUTINE                                  M31.7034
      REAL*8        CHA                                                 M34. 204
      DIMENSION R(NPK),AL(NPK),C(NPK)                                   M35.9529
      COMPLEX*16  ZC(LDN,LDN),YC(LDN,LDN)                               M35.9530
      LUNIT6=6                                                          M31.7037
      LUNIT7=7                                                          M31.7038
C     LTYPE=0 ; OUTPUT RS                                               M31.7039
C     LTYPE=1 ; NOT OUTPUT RS                                           M31.7040
C     NTYPE=0 ; CROSSBONDED CABLE                                       M31.7041
C     NTYPE=1 ; NON-CROSSBONDED CABLE                                   M31.7042
C     MTYPE=0 ; ALL SHEATHES ARE CONECTED                               M31.7043
C     MTYPE=1 ; ALL SHEATHES ARE NOT CONECTED                           M31.7044
      NCROS=IABS(NPAIS)                                                 M31.7045
      LTYPE=0                                                           M31.7046
      IF(NPAIS.GT.0.AND.NNCROS.EQ.0) LTYPE=1                            M31.7047
      NTYPE=1                                                           M31.7048
      IF(NPAIS.LT.0.AND.NNCROS.NE.0) NTYPE=0                            M31.7049
      MTYPE=IRSEP                                                       M31.7050
      IF(NPAIS.GT.0.AND.NNCROS.EQ.0) MTYPE=1                            M31.7051
      NUB3=3                                                            M31.7052
      IF(NTYPE.EQ.1) NUB3=1                                             M31.7053
      L=0                                                               M31.7054
      XLENG=XMAJOR                                                      M31.7055
      IF(NTYPE.EQ.0) XLENG=XLENG/3.                                     M31.7056
      DO 400 I=1,NUB6                                                   M31.7057
      DO 400 J=1,NUB6                                                   M31.7058
      IF(J.GT.I) GO TO 400                                              M31.7059
      L=L+1                                                             M31.7060
      R(L)=REALZ(ZC(I,J))*XLENG                                         M31.7061
      AL(L)=AIMAGZ(ZC(I,J))/W*XLENG*1.E3                                M31.7062
      C(L)=AIMAGZ(YC(I,J))/W*XLENG*1.E6                                 M31.7063
  400 CONTINUE                                                          M31.7064
      WRITE(LUNIT6,1400)                                                M31.7065
      WRITE(LUNIT7,1401)                                                M31.7066
 1400 FORMAT(1H ,11H$VINTAGE, 1)                                        M31.7067
 1401 FORMAT(11H$VINTAGE, 1)                                            M31.7068
      L=0                                                               M31.7069
      IF(LTYPE.EQ.1) GO TO 610                                          M31.7070
      KSTEP=1                                                           M31.7071
      KK=NUB6/2+1                                                       M31.7072
      IF(NUB6.EQ.4) KK=4                                                M31.7073
      KSTOP=(NUB6+1)/2+1                                                M31.7074
      IF(MTYPE.NE.0) KSTOP=NUB6                                         M31.7075
      IF(NUB6.EQ.4) KSTOP=4                                             M31.7076
      IF(NUB6.EQ.7.AND.MTYPE.EQ.0) KSTOP=4                              M31.7077
      DO 900 K=KK,KSTOP,KSTEP                                           M31.7078
      WRITE(LUNIT6,1300) CHA,K,RS                                       M31.7079
      WRITE(LUNIT7,1301) CHA,K,RS                                       M31.7080
  900 CONTINUE                                                          M31.7081
 1300 FORMAT(1H ,2X,A1,3HIN ,I2,18X,E16.5)                              M31.7082
 1301 FORMAT(2X,A1,3HIN ,I2,18X,E16.5)                                  M31.7083
  610 CONTINUE                                                          M31.7084
C     I ; NUMBER OF MAJOR SECTION                                       M31.7085
C     J ; NUMBER OF MINOR SECTION                                       M31.7086
C     K ; NUMBER OF PHASE                                               M31.7087
      DO 100 I=1,NCROS                                                  M31.7088
      DO 200 J=1,NUB3                                                   M31.7089
      DO 300 K=1,NUB6                                                   M31.7090
C     FOLLOWING TWO STATEMENTS ARE FOR THE THIRD MINOR SECTION          M31.7091
      K2=K                                                              M31.7092
      IF(MTYPE.EQ.1) GO TO 910                                          M31.7093
      IF(J.EQ.3.OR.NTYPE.EQ.1) CALL CHA444(K,K2)                        M31.7094
  910 CONTINUE                                                          M31.7095
C     FOLLOWING TWO STATEMENTS ARE FOR CROSSBONDING                     M31.7096
      K1=K                                                              M31.7097
      IF(J.NE.1) CALL CHA645(K,K1)                                      M31.7098
C     FOLLOWING ONE STATEMENT IS FOR THE FIRST MINOR SECTION            M31.7099
      IF(J.EQ.1.AND.MTYPE.NE.1) CALL CHA444(K,K1)                       M31.7100
C     FOLLOWING TWO STATEMENTS ARE FOR SERIESE CONECTION OF             M31.7101
C                                 MAJOR SECTION                         M31.7102
      J1=J                                                              M31.7103
      IF(NTYPE.NE.1) CALL CHA312(J,J1)                                  M31.7104
C     FOLLOWING TWO STATEMENTS ARE FOR SENDING END                      M31.7105
      I1=I                                                              M31.7106
      IF(J.EQ.1) I1=I-1                                                 M31.7107
      IPRI=1                                                            M31.7108
      IF(I1.EQ.0) IPRI=2                                                M31.7109
      IF(I.NE.NCROS) GO TO 700                                          M31.7110
      IF(NTYPE.EQ.1.OR.J.EQ.3) IPRI=3                                   M31.7111
C     IPRI=1 ;                                                          M31.7112
C     IPRI=2 ; SENDING END                                              M31.7113
C     IPRI=3 ; RECIEVING END                                            M31.7114
  700 CONTINUE                                                          M31.7115
      IF(NCROS.EQ.1.AND.NTYPE.EQ.1) IPRI=4                              M31.7116
      CALL PRI(I,J,K,I1,J1,K1,K2,L,IPRI,                                M35.9531
     1         CHA, R, AL, C, NPK )                                     M35.9532
  300 CONTINUE                                                          M31.7118
  200 CONTINUE                                                          M31.7119
      IF(LTYPE.EQ.1) GO TO 100                                          M31.7120
      J=3                                                               M31.7121
      IF(NTYPE.EQ.1) J=1                                                M31.7122
      IF(I.EQ.NCROS) GO TO 2010                                         M31.7123
      DO 920 K=KK,KSTOP,KSTEP                                           M31.7124
      WRITE(LUNIT6,2000) CHA,I,J,K,CHA,K                                M31.7125
      WRITE(LUNIT7,2001) CHA,I,J,K,CHA,K                                M31.7126
  920 CONTINUE                                                          M31.7127
 2000 FORMAT(1H ,2X,A1,I2,I1,I2,6X,A1,3HIN ,I2)                         M31.7128
 2001 FORMAT(2X,A1,I2,I1,I2,6X,A1,3HIN ,I2)                             M31.7129
      GO TO 100                                                         M31.7130
 2010 CONTINUE                                                          M31.7131
      DO 930 K=KK,KSTOP,KSTEP                                           M31.7132
      WRITE(LUNIT6,2100) CHA,K,CHA,K                                    M31.7133
      WRITE(LUNIT7,2101) CHA,K,CHA,K                                    M31.7134
  930 CONTINUE                                                          M31.7135
 2100 FORMAT(1H ,2X,A1,3HOUT,I2,6X,A1,3HIN ,I2)                         M31.7136
 2101 FORMAT(2X,A1,3HOUT,I2,6X,A1,3HIN ,I2)                             M31.7137
  100 CONTINUE                                                          M31.7138
      WRITE(LUNIT6,1410)                                                M31.7139
      WRITE(LUNIT7,1411)                                                M31.7140
 1410 FORMAT(1H ,11H$VINTAGE, 0)                                        M31.7141
 1411 FORMAT(11H$VINTAGE, 0)                                            M31.7142
      RETURN                                                            M31.7143
      END                                                               M31.7145
      SUBROUTINE CHA645(K,K1)                                           M31.7146
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      IF(K.LE.3.OR.K.GE.7) RETURN                                       M31.7147
      KK=K-3                                                            M31.7148
      GO TO (10,20,30),KK                                               M31.7149
   10 K1=6                                                              M31.7150
      RETURN                                                            M31.7151
   20 K1=4                                                              M31.7152
      RETURN                                                            M31.7153
   30 K1=5                                                              M31.7154
      RETURN                                                            M31.7155
      END                                                               M31.7156
      SUBROUTINE CHA312(J,J1)                                           M31.7157
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      GO TO (10,20,30),J                                                M31.7158
   10 J1=3                                                              M31.7159
      RETURN                                                            M31.7160
   20 J1=1                                                              M31.7161
      RETURN                                                            M31.7162
   30 J1=2                                                              M31.7163
      RETURN                                                            M31.7164
      END                                                               M31.7165
      SUBROUTINE CHA444(K,KK)                                           M31.7166
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      IF(K.LE.3.OR.K.GT.7) RETURN                                       M31.7167
      KK=4                                                              M31.7168
      RETURN                                                            M31.7169
      END                                                               M31.7170
      SUBROUTINE PRI( I, J, K, I1, J1, K1, K2, L, IPRI,                 M35.9533
     1               CHA, R, AL, C, NPK )                               M35.9534
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      REAL*8        CHA                                                 M34. 205
      DIMENSION R(NPK),AL(NPK),C(NPK)                                   M35.9535
      LUNIT6=6                                                          M31.7173
      LUNIT7=7                                                          M31.7174
      GO TO (10,20,30,40),IPRI                                          M31.7175
   10 CONTINUE                                                          M31.7176
      IF(K.NE.1) GO TO 15                                               M31.7177
      WRITE(LUNIT6,1000) K,CHA,I1,J1,K1,CHA,I,J,K2,CHA,CHA              M31.7178
      WRITE(LUNIT7,1001) K,CHA,I1,J1,K1,CHA,I,J,K2,CHA,CHA              M31.7179
 1000 FORMAT(1H ,I2,A1,I2,I1,I2,A1,I2,I1,I2,A1,5HIN  1,A1,5H 11 1)      M31.7180
 1001 FORMAT(I2,A1,I2,I1,I2,A1,I2,I1,I2,A1,5HIN  1,A1,5H 11 1)          M31.7181
      RETURN                                                            M31.7182
   15 CONTINUE                                                          M31.7183
      WRITE(LUNIT6,1010) K,CHA,I1,J1,K1,CHA,I,J,K2                      M31.7184
      WRITE(LUNIT7,1011) K,CHA,I1,J1,K1,CHA,I,J,K2                      M31.7185
 1010 FORMAT(1H ,I2,A1,I2,I1,I2,A1,I2,I1,I2)                            M31.7186
 1011 FORMAT(I2,A1,I2,I1,I2,A1,I2,I1,I2)                                M31.7187
      RETURN                                                            M31.7188
   20 CONTINUE                                                          M31.7189
      L=L+1                                                             M31.7190
      WRITE(LUNIT6,1100) K,CHA,K1,CHA,I,J,K2,R(L),AL(L),C(L)            M31.7191
      WRITE(LUNIT7,1101) K,CHA,K1,CHA,I,J,K2,R(L),AL(L),C(L)            M31.7192
 1100 FORMAT(1H ,I2,A1,3HIN ,I2,A1,I2,I1,I2,12X,3E16.5)                 M31.7193
 1101 FORMAT(I2,A1,3HIN ,I2,A1,I2,I1,I2,12X,3E16.5)                     M31.7194
 2000 CONTINUE                                                          M31.7195
      IF(K.EQ.1) RETURN                                                 M31.7196
      DO 100 LL=2,K                                                     M31.7197
      L=L+1                                                             M31.7198
      WRITE(LUNIT6,1110) R(L),AL(L),C(L)                                M31.7199
      WRITE(LUNIT7,1111) R(L),AL(L),C(L)                                M31.7200
 1110 FORMAT(1H ,26X,3E16.5)                                            M31.7201
 1111 FORMAT(26X,3E16.5)                                                M31.7202
  100 CONTINUE                                                          M31.7203
      RETURN                                                            M31.7204
   30 CONTINUE                                                          M31.7205
      IF(K.NE.1) GO TO 35                                               M31.7206
      WRITE(LUNIT6,1200) K,CHA,I1,J1,K1,CHA,K2,CHA,CHA                  M31.7207
      WRITE(LUNIT7,1201) K,CHA,I1,J1,K1,CHA,K2,CHA,CHA                  M31.7208
 1200 FORMAT(1H ,I2,A1,I2,I1,I2,A1,3HOUT,I2,A1,5HIN  1,A1,5H 11 1)      M31.7209
 1201 FORMAT(I2,A1,I2,I1,I2,A1,3HOUT,I2,A1,5HIN  1,A1,5H 11 1)          M31.7210
      RETURN                                                            M31.7211
   35 CONTINUE                                                          M31.7212
      WRITE(LUNIT6,1210) K,CHA,I1,J1,K1,CHA,K2                          M31.7213
      WRITE(LUNIT7,1211) K,CHA,I1,J1,K1,CHA,K2                          M31.7214
 1210 FORMAT(1H ,I2,A1,I2,I1,I2,A1,3HOUT,I2)                            M31.7215
 1211 FORMAT(I2,A1,I2,I1,I2,A1,3HOUT,I2)                                M31.7216
      RETURN                                                            M31.7217
   40 CONTINUE                                                          M31.7218
      L=L+1                                                             M31.7219
      WRITE(LUNIT6,1300) K,CHA,K1,CHA,K2,R(L),AL(L),C(L)                M31.7220
      WRITE(LUNIT7,1301) K,CHA,K1,CHA,K2,R(L),AL(L),C(L)                M31.7221
 1300 FORMAT(1H ,I2,A1,3HIN ,I2,A1,3HOUT,I2,12X,3E16.5)                 M31.7222
 1301 FORMAT(I2,A1,3HIN ,I2,A1,3HOUT,I2,12X,3E16.5)                     M31.7223
      GO TO 2000                                                        M31.7224
      END                                                               M31.7225
      SUBROUTINE NYAN(ITYPE,NPC,NC,NCPP,NGRND,NCROS,NPAIS,LDM)          M35.9536
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION NCPP(LDM)                                               M35.9537
      LUNIT6=6                                                          M31.7228
      IF(ITYPE.EQ.1) RETURN                                             M31.7229
      IF(NCROS.EQ.0) RETURN                                             M31.7230
      IF(NPC.NE.3) GO TO 9000                                           M31.7231
      DO 100 I=1,3                                                      M31.7232
      IF(NCPP(I).EQ.1.OR.NCPP(I).GT.3) GO TO 9100                       M31.7233
  100 CONTINUE                                                          M31.7234
      NWORK=6+NGRND                                                     M31.7235
      IF(NWORK.NE.NC) GO TO 9200                                        M31.7236
      IF(NCPP(1).LE.NCPP(2).AND.NCPP(2).LE.NCPP(3)) RETURN              M31.7237
      GO TO 9300                                                        M31.7238
 9000 CONTINUE                                                          M31.7239
      WRITE(LUNIT6,7000)                                                M31.7240
      WRITE(LUNIT6,8000)                                                M31.7241
 8000 FORMAT(1H0,10X,31HNUMBER OF PHASES SHOULD BE '3'.)                M31.7242
      CALL STOPTP                                                               
 9100 CONTINUE                                                          M31.7244
      WRITE(LUNIT6,7000)                                                M31.7245
      WRITE(LUNIT6,8100)                                                M31.7246
 8100 FORMAT(1H0,10X,40HNUMBER OF CONDUCTORS IN ONE PHASE SHOULD,       M31.7247
     1               40HBE '2' (CORE AND SHEATH) WITH 'NGRND'='0,       M31.7248
     1               40H' FOR AN SC CABLE AND 'NGRND'='1' FOR A /       M31.7249
     1       1H ,10X,40HPT CABLE, OF '3' (CORE, SHEATH AND ARMOR,       M31.7250
     1               40H) WITH 'NGRND'='3, CONSIDERING THE FACT ,       M31.7251
     1               40HTHAT ALL THE 3-PHASE CABLES HAVE THE    /       M31.7252
     1       1H ,10X,40HSAME CONFIGURATION.                     )       M31.7253
      CALL STOPTP                                                               
 9200 CONTINUE                                                          M31.7255
      WRITE(LUNIT6,7000)                                                M31.7256
      WRITE(LUNIT6,8200)                                                M31.7257
 8200 FORMAT(1H0,10X,46HIN THE CASE OF 'NPAIS.GE.0 .AND. NCROS.NE.0',   M31.7258
     1 ,31HTHE FINAL NUMBER OF CONDUCTORS  /11X                         M31.7259
     2 ,56HCONSIDERING GROUNDED CONDUCTORS REDUCTION SHOULD BE '6'.     M31.7260
     1         /11X,40H'NGRND' SHOULD BE '3' FOR AN SC CABLE WI,        M31.7261
     1               40HTH ARMOR AND '1' FOR A PT CABLE.  IF A 3,       M31.7262
     1               40H-PHASE SC CABLE WITH ARMOR IS ENCLOSED  /       M31.7263
     1       1H ,10X,39HWITHIN A PIPE (I.E., PT CABLE), PLEASE  ,       M31.7264
     1               40HNEGRECT THE PIPE, I.E., REGARD AS AN SC ,       M31.7265
     1               40HCABLE WITH 'NGRAND'='3', CONSIDERING THE/       M31.7266
     1       1H ,10X,38HFACT THAT ALL THE 3-PHASE CABLES HAVE   ,       M31.7267
     1               40HTHE SAME CONFIGURATION.                 )       M31.7268
      CALL STOPTP                                                               
 9300 CONTINUE                                                          M31.7270
      WRITE(LUNIT6,7000)                                                M31.7271
      WRITE(LUNIT6,8300)                                                M31.7272
 8300 FORMAT(1H0,10X,40HEACH CABLE OF A 3-PHASE CABLE SYSTEM HAS,       M31.7273
     1               40H THE SAME CONFIGULATION IN GENERAL. IF N,       M31.7274
     1               40HOT, PLEASE ARRANGE THE DATA CARDS AS    /       M31.7275
     1       1H ,10X,40HFOLLOWS : FIRST COMES A CABLE OF WHICH T,       M31.7276
     1               40HHE NUMBER OF CONDUCTORS IS SMALLEST, SEC,       M31.7277
     1               40HOND COMES A CABLE OF THE SECOND SMALLEST/       M31.7278
     1       1H ,10X,40HNUMBER OF CONDUCTORS,.... PLEASE CHECK Y,       M31.7279
     1               40HOUR DATA.                               )       M31.7280
      CALL STOPTP                                                               
      RETURN                                                                    
 7000 FORMAT(1H0,10X,40HERRORS FOR A CROSSBONDED CABLE (NCROS.NE,       M31.7282
     1               40H.0) WHEN 'NPAIS'='0'.                   )       M31.7283
      END                                                               M31.7284
      SUBROUTINE GOMEN(ITYPE,NPC,NX,NPAIS,NCROS,IRSEP,NCPP,LDM)         M35.9538
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION NCPP(LDM)                                               M35.9539
      LUNIT6=6                                                          M31.7287
      IF(NPAIS.LT.0) GO TO 1000                                         M31.7288
      IF(NCROS.NE.0) GO TO 1200                                         M31.7289
      IF(IRSEP.EQ.0) RETURN                                             M31.7290
      GO TO 9120                                                        M31.7291
 1000 IF(NCROS.EQ.0) GO TO 1100                                         M31.7292
      IF(NX.LT.6.OR.NX.GT.7) GO TO 9010                                 M31.7293
 1200 IF(NPC.NE.3) GO TO 9020                                           M31.7294
      DO 100 I=1,3                                                      M31.7295
      IF(NCPP(I).EQ.1.OR.NCPP(I).GT.3) GO TO 9030                       M31.7296
  100 CONTINUE                                                          M31.7297
      IF(NCPP(1).GT.NCPP(2).OR.NCPP(2).GT.NCPP(3)) GO TO 9040           M31.7298
      IF(NX.EQ.6) RETURN                                                M31.7299
      IF(NPAIS.LE.0 .OR. NCROS.EQ.0) GO TO 200                          M31.7300
      IF(NX.EQ.4) RETURN                                                M31.7301
  200 IF(NCPP(2).EQ.2) RETURN                                           M31.7302
      GO TO 9110                                                        M31.7303
 1100 IF(NX.GT.7) GO TO 9050                                            M31.7304
      IF(NX.GE.6) GO TO 1200                                            M31.7305
      IF(NX.EQ.2) GO TO 1300                                            M31.7306
      IF(NX.EQ.3) GO TO 1400                                            M31.7307
      GO TO 9060                                                        M31.7308
 1400 IF(IRSEP.EQ.0) GO TO 9070                                         M31.7309
      IF(NPC.NE.1) GO TO 9080                                           M31.7310
      RETURN                                                            M31.7311
 1300 IF(NPC.NE.1) GO TO 9090                                           M31.7312
      RETURN                                                            M31.7313
 9010 CONTINUE                                                          M31.7314
      WRITE(LUNIT6,8010)                                                M31.7315
 8010 FORMAT(1H0,10X,40HIN THE CASE OF 'NPAIS.LT.0.AND.NCROS.NE.,       M31.7316
     1               40H0' THE FINAL NUMBER OF CONDUCTORS INCLUD,       M31.7317
     1               40HING GROUNDED CONDUCTORS REDUCTION SHOULD/       M31.7318
     1       1H ,10X,40HBE 6 OR 7, I.E., 'TOTAL NUMBER OF CONDUC,       M31.7319
     1               40HTORS OF THE CABLE SYSTEM'-'NGRND'='6' OR,       M31.7320
     1               40H '7', BUT 'NGRND' SHOULD BE '1' FOR A PT/       M31.7321
     1       1H ,10X,40HCABLE.                                  )       M31.7322
      CALL STOPTP                                                               
 9020 CONTINUE                                                          M31.7324
      WRITE(LUNIT6,8020)                                                M31.7325
 8020 FORMAT(1H0,10X,40HNUMBER OF PHASES SHOULD BE '3'.         )       M31.7326
      CALL STOPTP                                                               
 9030 CONTINUE                                                          M31.7328
      WRITE(LUNIT6,8030)                                                M31.7329
 8030 FORMAT(1H0,10X,40HNUMBER OF CONDUCTORS IN ONE PHASE SHOULD,       M31.7330
     1               40H BE '2' (CORE AND SHEATH) WITH 'NGRND'=',       M31.7331
     1               40H0' FOR AN SC CABLE AND 'NGRND'='1' FOR A/       M31.7332
     1       1H ,10X,40HPT CABLE, OR '3' (CORE, SHEATH AND ARMOR,       M31.7333
     1               40H) WITH 'NGRND'='3' FOR AN SC CABLE.     )       M31.7334
      CALL STOPTP                                                               
 9040 CONTINUE                                                          M31.7336
      WRITE(LUNIT6,8040)                                                M31.7337
 8040 FORMAT(1H0,10X,40HEACH CABLE OF A 3-PHASE CABLE SYSTEM HAS,       M31.7338
     1               40H THE SAME CONFIGULATION IN GENERAL. IF N,       M31.7339
     1               40HOT, PLEASE ARRANGE THE DATA CARDS AS    /       M31.7340
     1       1H ,10X,40HFOLLOWS : FIRST COMES A CABLE OF WHICH T,       M31.7341
     1               40HHE NUMBER OF CONDUCTORS IS SMALLEST, SEC,       M31.7342
     1               40HOND COMES A CABLE OF THE SECOND SMALLEST/       M31.7343
     1       1H ,10X,40HNUMBER OF CONDUCTORS,.... PLEASE CHECK Y,       M31.7344
     1               40HOUR DATA                                )       M31.7345
      CALL STOPTP                                                               
 9050 CONTINUE                                                          M31.7347
      WRITE(LUNIT6,8050)                                                M31.7348
 8050 FORMAT(1H0,10X,40HIN THE CASE OF 'NPAIS.LT.0', THE FINAL N,       M31.7349
     1               40HUMBER OF CONDUCTORS INCLUDING GROUNDED C,       M31.7350
     1               40HONDUCTORS REDUCTION SHOULD NOT BE       /       M31.7351
     1       1H ,10X,41HGREATER THAN '7'. PLEASE CHECK YOUR DATA.)      M31.7352
      CALL STOPTP                                                               
 9060 CONTINUE                                                          M31.7354
      WRITE(LUNIT6,8060)                                                M31.7355
 8060 FORMAT(1H0,10X,40HIN THE CASE OF 'NPAIS.LT.0', THE FINAL N,       M31.7356
     1               40HUMBER OF CONDUCTORS INCLUDING GROUNDED C,       M31.7357
     1               40HONDUCTORS REDUCTION SHOULD NOT BE '1' OR/       M31.7358
     1       1H ,10X,40H'4' OR '5'. PLEASE CHECK YOUR DATA.            )M31.7359
      CALL STOPTP                                                               
 9070 CONTINUE                                                          M31.7361
      WRITE(LUNIT6,8070)                                                M31.7362
 8070 FORMAT(1H0,10X,40HIN THIS CASE, IT SHOULD NOT BE 'IRSEP=0',       M31.7363
     1               40HPLEASE CHANGE 'IRSEP=1'                 )       M31.7364
      CALL STOPTP                                                               
 9080 CONTINUE                                                          M31.7366
      WRITE(LUNIT6,8080)                                                M31.7367
 8080 FORMAT(1H0,10X,40HIN THIS CASE, NUMBER OF PHASES (NPC) SHO,       M31.7368
     1               40HULD ONLY BE '1'. PLEASE CHECK YOUR DATA.)       M31.7369
      CALL STOPTP                                                               
 9090 CONTINUE                                                          M31.7371
      WRITE(LUNIT6,8090)                                                M31.7372
 8090 FORMAT(1H0,10X,40HSORRY, IN THE CASE OF 'NPAIS.LT.0', A TW,       M31.7373
     1               40HO PHASE CABLE (NPC=2) CONSISTING ONLY OF,       M31.7374
     1               40H CORE OR CONSISTING OF CORE AND SHEATH  /       M31.7375
     1       1H ,10X,40HCAN NOT BE DEALT WITH. PLEASE CHECK YOUR,       M31.7376
     1               40HDATA.                                   )       M31.7377
      CALL STOPTP                                                               
 9110 CONTINUE                                                          M31.7379
      WRITE(LUNIT6,8110)                                                M31.7380
 8110 FORMAT(1H0,10X,40HIN THIS CASE, NCPP(2) SHOULD BE '2' PLEA,       M31.7381
     1               40HSE CHECK YOUR DATA.                       )     M31.7382
      CALL STOPTP                                                               
 9120 CONTINUE                                                          M31.7384
      WRITE(LUNIT6,8120)                                                M31.7385
 8120 FORMAT(1H0,10X,34HINTHE CASE OF 'NPAIS.GT.0.AND.NCRO,             M31.7386
     1               34HS.EQ.0', IT SHOULD NOT BE 'IRSEP=1,             M31.7387
     1               34H',PLEASE CHANGE 'IRSEP=0'.        )             M31.7388
      CALL STOPTP                                                               
      RETURN                                                                    
      END                                                               M31.7390
      SUBROUTINE  PRCON(W,NCONPW, ZC,ZS,YS,YC,YO,QN,GN,ZE,A,AI,B,BI,    M35.9540
     1            AN,  CA, ZO, CC, F, LDN, LDN2, LNQ2, MRR, NRP )       M35.9541
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      DIMENSION  GN(LDN), AN(LNQ2)                                      M35.9542
      DIMENSION  TIR(20,20), TII(20,20)                                 VAX.4210
      DIMENSION  PP1(20,20), PP2(20,20)                                 VAX.4211
      DIMENSION  ZZ(30), PS(30), PING(200), ISEQ(15),KMAX(15)           VAX.4212
      COMPLEX*16  PP6(20,20),EE6(15)                                    VAX.4213
      COMPLEX*16  ANGLEC, CJW                                           M28.7416
      COMPLEX*16  CSQRTZ                                                VAX.4214
      COMPLEX*16  CMPLXZ, D1, D2, D3, D4                                M35.9543
      COMPLEX*16  CA(LDN, LDN), ZO(LDN, LDN), CC(LDN, LDN)              M35.9544
      COMPLEX*16  ZC(LDN, LDN), ZS(LDN, LDN), YS(LDN, LDN), YC(LDN, LDN)M35.9545
      COMPLEX*16  A(LDN, LDN), AI(LDN, LDN),  B(LDN, LDN), BI(LDN, LDN) M35.9546
      COMPLEX*16  YO(LDN, LDN), ZE(LDN, LDN), QN(LDN), F(LDN, LDN2)     M35.9547
      DATA  ISEQ  /  15*0  /                                            VAX.4215
      NTOL=IPRINT-1                                                             
      CJW = CMPLXZ ( FZERO, W )                                         M22.6708
      IF ( IPRS47  .GE.  1 )                                            M23.5943
     1 WRITE (LOGSIX, 2624)  NCONPW, NUMAKI, VALUE1, VALUE3, PAI, CJW   M23.5944
 2624 FORMAT ( /,  17H ENTER  'PRCON' .,  16H  NCONPW  NUMAKI,          M23.5945
     1  13X,  6HVALUE1,  13X,  6HVALUE3,  16X,  3HPAI,                  M23.5946
     2  11X,  8HREAL-CJW,  11X,  8HIMAG-CJW  ,/,  17X,  2I8,  5E19.10 ) M23.5947
      LL1 = 1                                                           M16.6817
      LL2 = 2                                                           M16.6818
      CALL MXM(ZC,YC,ZS,NCONPW,LDN)                                     M35.9548
      CALL EIGEN( CJW,ZS,NCONPW,  A,AI,QN,CA,ZO,CC,LDN  )               M35.9549
CCCC  *****                                                             VAX.4217
      IF (KILL .GT. 1)   GO TO 9200                                     M16.6821
      IF (ITYPEC .NE. 2)  GO TO 75                                      M22.6709
      N1 = NPC * 2                                                      M22.6710
      IF (NCONPW .NE. N1 ) GO TO 75                                     M31.7395
      ICORR = 0                                                         M22.6712
      NPC1 = NPC + 1                                                    M22.6713
      DO 65 I = NPC1, NPC2                                              M22.6714
      IF (REALZ(QN(I)) .GT. 0.)  GO TO 65                               M22.6715
      I1 = I + 1                                                        M22.6716
      IF (I1 .GT. NPC2)  I1 = I - 1                                     M22.6717
      IF (I1 .LE. NPC)  GO TO 65                                        M22.6718
      IF (REALZ(QN(I1)) .LT. 0.)  GO TO 63                              M22.6719
   61 QN(I) = QN(I1)                                                    M22.6720
      ICORR = 1                                                         M22.6721
      WRITE(LUNIT6, 901)  I                                             M22.6722
  901 FORMAT (1H0, 42HMODIFICATIONS OF MODAL QUANTITIES FOR MODE,       M22.6723
     1  I2, 10H ARE MADE. /)                                            M22.6724
      GO TO 65                                                          M22.6725
   63 I1 = I + 2                                                        M22.6726
      IF (I1 .GT. NPC2)  I1 = I - 2                                     M22.6727
      IF (REALZ(QN(I1)) .GT. 0.)  GO TO 61                              M22.6728
   65 CONTINUE                                                          M22.6729
      IF (ICORR .EQ. 0)  GO TO 75                                       M22.6730
      DO 700 I=NPC1, NPC2                                               M22.6731
      I1 = I - NPC                                                      M22.6732
      DO 700 J=1, NPC2                                                  M22.6733
      AI(I,J) = CZERO                                                   M23.5948
      IF (I1.EQ.J)  AI(I,J) = CREAL1                                    M23.5949
      IF(I .EQ. J)  AI(I,J) = -CREAL1                                   M23.5950
  700 CONTINUE                                                          M22.6737
      DO 71 I=1,NCONPW                                                  M28.7419
      DO 71  J=1, NCONPW                                                M28.7420
   71 A(I,J) = AI(I,J)                                                  M22.6740
      write(*,*) ' after #71 '                                          VAX.4218
      CALL MINV( A, NCONPW, F, LDN, LDN2 )                              M35.9550
   75 CONTINUE                                                          M22.6742
      DO 388 IZ=1,30                                                    VAX.4219
      PS(IZ)=0.                                                         VAX.4220
      ZZ(IZ)=0.                                                         VAX.4221
  388 CONTINUE                                                          VAX.4222
      DO 1960 I=1,NCONPW                                                M35.9551
      AN(I) = VALUE3 * REALZ(QN(I))                                     M18.1094
      GN(I) = W / AIMAGZ( QN(I) )                                       M22.6743
      IF (AN(I) .LT. 0.)  GO TO 50                                      M22.6744
      A1 = 1.05                                                         M22.6745
      SPDTOL = SPDLGT * A1                                              M22.6746
      IF (GN(I) .GT. SPDTOL)  GO TO 50                                  M22.6747
      IF (GN(I) .LT. SPDLGT)  GO TO 55                                  M22.6748
      GN(I) = SPDLGT                                                    M22.6749
      WRITE(LUNIT6, 961)                                                M22.6750
  961 FORMAT (1H0, 10X, 33HMODIFICATION FOR IMAG(Q) IS MADE.  //, 1X)   M22.6751
      GO TO 55                                                          M22.6752
   50 WRITE(LUNIT6, 962)                                                M22.6753
  962 FORMAT (1H0, 10X, 49HCONDUCTOR IMPEDANCE MAY INCLUDE NUMERICAL ERRM22.6754
     1ORS.  /,11X,82HTHE CORRECT SOLUTIONS FOR MODES SHOWING ERRORS CAN M22.6755
     2BE GIVEN BY THE VERY LAST MODE.  //, 1X)                          M22.6756
   55 CONTINUE                                                          M22.6757
C       IF (LASTOV .NE. 39 )  GO TO 600                                 M43.4062
C       IF (IPRINT .GT. 1 )  GO TO 600                                  M43.4063
      DO 60 J= 1, NCONPW                                                VAX.4225
      BI(I,J)=A(J,I)                                                    VAX.4226
      B(I,J)=AI(J,I)                                                    VAX.4227
      IF ( LASTOV .NE. 39 )  GO TO 60                                   M43.4064
      IF ( IPRINT .GT. 1 )  GO TO 60                                    M43.4065
      TIR(I,J) = REALZ(B(I,J))                                          VAX.4228
      TII(I,J) = AIMAGZ(B(I,J))                                         VAX.4229
   60 CONTINUE                                                          VAX.4230
  600 CONTINUE                                                          VAX.4231
 1960 CONTINUE                                                          M35.9555
      IF (NTOL.EQ.0 .OR. LASTOV .NE. 39) GO TO 5602                             
      KTHL=1                                                                    
      PING(KTHL)=ALOG1Z(W/TWOPI)                                        VAX.4234
      D55 = 1.0 /(W/TWOPI)                                              VAX.4235
      D56 = 1.0 / SQRTZ( W/TWOPI )                                      VAX.4236
      DO 5600 I=1,NCONPW                                                VAX.4237
C     EM=(REALZ(QN(I)))**2 + (AIMAGZ(QN(I)))**2                         VAX.4238
C     EA=2.0*ATAN2Z( AIMAG(QN(I)),REAL(QN(I)) )                         VAX.4239
      EM=SQRTZ((REALZ(QN(I)**2))**2 + (AIMAGZ(QN(I)**2))**2)            VAX.4240
      EA=ATAN2Z( AIMAGZ(QN(I)**2),REALZ(QN(I)**2) )                     VAX.4241
      DB=VALUE3*REALZ(QN(I))                                            VAX.4242
      VMODE=W/AIMAGZ(QN(I))                                             VAX.4243
 5601 CONTINUE                                                          VAX.4244
      PING(KTHL+1) = EM*D55                                                     
      PING(KTHL+2) = EA/(TWOPI/360.)                                            
      PING(KTHL+3) = VMODE                                                      
      PING(KTHL+4) = DB*D56                                                     
      KTHL = KTHL + 4                                                           
 5603 CONTINUE                                                          VAX.4250
 5600 CONTINUE                                                          VAX.4251
      CALL UNWIND( PING,KTHL,MRR,NRP,NTOL,ISEQ )                        VAX.4252
      DO 5613 I=1,NCONPW                                                VAX.4253
      EE6(I)=QN(I)**2                                                   VAX.4254
      DO 5613 J=1,NCONPW                                                VAX.4255
      PP6(I,J)=A(I,J)                                                   VAX.4256
 5613 CONTINUE                                                          VAX.4257
      DO 5612 I=1,NCONPW                                                VAX.4258
      QN(I) = CSQRTZ( EE6(ISEQ(I)) )                                    VAX.4259
      DO 5612 J=1,NCONPW                                                VAX.4260
      A(I,J)=PP6(I,ISEQ(J))                                             VAX.4261
 5612 CONTINUE                                                          VAX.4262
      DO 5701 I=1,NCONPW                                                VAX.4263
      DO 5701 J=1,NCONPW                                                VAX.4264
      AI(I,J)=A(I,J)                                                    VAX.4265
 5701 CONTINUE                                                          VAX.4266
      if ( iprsup .ge. 1 )                                              VAX.4267
     1 write(*,*) ' PRCON after S.N. 5701.   [A]  follows ....'         VAX.4268
      do 7801  i=1,nconpw                                               VAX.4269
      if ( iprsup .ge. 1 )                                              VAX.4270
     1 write(*,*) (a(i,j), j=1,nconpw)                                  VAX.4271
 7801 continue                                                          VAX.4272
      CALL MINV( AI, NCONPW, F, LDN, LDN2 )                             VAX.4273
      if ( iprsup .ge. 1 )                                              VAX.4274
     1 write(*,*) ' After "MINV".   [AI]  follows ..... '               VAX.4275
      do 7802 i=1,nconpw                                                VAX.4276
      if ( iprsup .ge. 1 )                                              VAX.4277
     1 write(*,*) (ai(i,j), j=1,nconpw)                                 VAX.4278
 7802 continue                                                          VAX.4279
      DO 5611 I=1,NCONPW                                                VAX.4280
      DO 5621 J=1,NCONPW                                                VAX.4281
      PP1(J,I)=SQRTZ( (REALZ(AI(I,J)))**2+(AIMAGZ(AI(I,J)))**2 )        VAX.4282
      PP2(J,I)=ATAN2Z( AIMAGZ(AI(I,J)) , REALZ(AI(I,J)) )               VAX.4283
 5621 CONTINUE                                                          VAX.4284
 5611 CONTINUE                                                          VAX.4285
      IF ( NTOL.EQ.1 ) GO TO 5800                                               
      GO TO 5810                                                                
 5800 CONTINUE                                                          VAX.4288
      DO 5801 J=1,NCONPW                                                        
      KMAX(J)=1                                                                 
      DO 5802 I=1,NCONPW                                                        
      IF ( PP1(I,J) .LE. PP1(KMAX(J),J) ) GO TO 5802                            
      KMAX(J)=I                                                                 
 5802 CONTINUE                                                          VAX.4294
 5801 CONTINUE                                                          VAX.4295
      WRITE (LUNIT9)  (KMAX(J),J=1,NCONPW)                              VAX.4296
 5810 CONTINUE                                                          VAX.4297
      if ( iprsup .ge. 1 )                                              VAX.4298
     1 write (*, *) ' PRCON.  Use normalization cells KMAX(1:NCONPW) =',VAX.4299
     1                          ( kmax(i), i=1,nconpw )                 VAX.4300
      DO 5614 J=1,NCONPW                                                        
      IF ( PP1(KMAX(J),J) .EQ. 0.0 ) GO TO 5614                                 
      DV=1.0/PP1(KMAX(J),J)                                             VAX.4303
      DA=PP2(KMAX(J),J)                                                 VAX.4304
      DO 5624 I=1,NCONPW                                                VAX.4305
      PP1(I,J)=PP1(I,J)*DV                                              VAX.4306
      PP2(I,J)=PP2(I,J)-DA                                              VAX.4307
C     *** to keep angles within principal value region                  VAX.4308
      IF ( PP2(I,J) .GT. TWOPI/2. )  PP2(I,J)=PP2(I,J) - TWOPI          VAX.4309
      IF ( PP2(I,J) .LT.-TWOPI/2. )  PP2(I,J)=PP2(I,J) + TWOPI          VAX.4310
      IF (IPRSUP .GE. 3)                                                VAX.4311
     1 write(*,*) 'just before 5624, i, j, pp1(i,j), pp2(i,j)',         VAX.4312
     1                              i, j, pp1(i,j), pp2(i,j)            VAX.4313
 5624 CONTINUE                                                          VAX.4314
 5614 CONTINUE                                                          VAX.4315
      FOUT = W / TWOPI                                                  VAX.4316
C   ******************** TEMPORARY DIAGNOSTIC ************              VAX.4317
      if ( iprsup .ge. 0 )                                              VAX.4318
     1 write (*, *) ' PRCON.  Freq =',  FOUT, '     PP1 follow : '      VAX.4319
      do 7701 i=1,nconpw                                                VAX.4320
      if ( iprsup .ge. 0 )                                              VAX.4321
     1 write (*, *) ( pp1(i,j), j=1, nconpw )                           VAX.4322
 7701 continue                                                          VAX.4323
      if ( iprsup .ge. 0 )                                              VAX.4324
     1 write (*, *) ' PP2  follow ... '                                 VAX.4325
      do 7702 i=1,nconpw                                                VAX.4326
      if ( iprsup .ge. 0 )                                              VAX.4327
     1 write (*, *) ( pp2(i,j), j=1,nconpw )                            VAX.4328
 7702 continue                                                          VAX.4329
      DO 6004 I=1,NCONPW                                                VAX.4330
      DO 6004 J=1,NCONPW                                                VAX.4331
      U1= (PP1(I,J))*COSZ(PP2(I,J))                                     VAX.4332
      U2= (PP1(I,J))*SINZ(PP2(I,J))                                     VAX.4333
      AI(J,I)=CMPLXZ(U1,U2)                                             VAX.4334
 6004 CONTINUE                                                          VAX.4335
      DO 6002 J=1,NCONPW                                                VAX.4336
      DO 6002 I=1,NCONPW                                                VAX.4337
      A(I,J)=AI(I,J)                                                    VAX.4338
 6002 CONTINUE                                                          VAX.4339
      CALL MINV ( A, NCONPW, F, LDN, LDN2 )                             VAX.4340
      DO 6003  I=1, NCONPW                                              VAX.4341
      DO 6003  J=1, NCONPW                                              VAX.4342
      BI(I,J)=A(J,I)                                                    VAX.4343
      B(I,J)=AI(J,I)                                                    VAX.4344
 6003 CONTINUE                                                          VAX.4345
 5602 CONTINUE                                                          VAX.4346
      DO 1470  I=1, NCONPW                                              VAX.4347
      DO 1470  J=1, NCONPW                                              VAX.4348
      AII = AIMAGZ ( AI(I,J) )                                          VAX.4349
      AIR = REALZ  ( AI(I,J) )                                          VAX.4350
      D18 = CABSZ ( AI(I,J) )                                           VAX.4351
      DEG = ATAN2Z ( AII, AIR ) * 180.0 / PAI                           VAX.4352
      ZE(I,J) = CMPLXZ ( D18, DEG )                                     VAX.4353
      ACOMI = AIMAGZ ( A(I,J) )                                         VAX.4354
      ACOMR = REALZ ( A(I,J) )                                          VAX.4355
      D19 = CABSZ ( A(I,J) )                                            VAX.4356
      DEG = ATAN2Z ( ACOMI, ACOMR ) * 180.0 / PAI                       VAX.4357
      YS(I,J) = CMPLXZ ( D19, DEG )                                     VAX.4358
 1470 CONTINUE                                                          VAX.4359
      IF ( NUMAKI .GT. 3 )  GO TO 5673                                  VAX.4360
      WRITE (LUNIT6, 2953)                                              VAX.4361
 2953 FORMAT (  /, 10X, 'VOLTAGE TRANSFORMATION MATRICES IN POLAR',     VAX.4362
     1                  ' COORDINATES (WITH ANGLES IN DEGREES).'        VAX.4363
     2   ,/, 15X, ' INVERSE OF [Tv] --- FROM PHASE TO MODE DOMAIN:'  )  VAX.4364
      CALL PRINT ( ZE, NCONPW, LL1, LDN )                               VAX.4365
      WRITE (LUNIT6, 4954)                                              VAX.4366
 4954 FORMAT ( 15X,  ' [Tv] --- FROM MODE TO PHASE DOMAIN:' )           VAX.4367
      CALL PRINT ( YS, NCONPW, LL1, LDN )                               VAX.4368
 5673 CALL MXM ( BI, YC, CA, NCONPW, LDN )                              VAX.4369
      CALL MXM ( CA, A, YS, NCONPW, LDN )                               VAX.4370
      DO 1980  I=1,NCONPW                                               VAX.4371
      DO 80  J=1,NCONPW                                                 VAX.4372
      IF(I.NE.J) GO TO 79                                               VAX.4373
      AA=AIMAGZ(YS(I,I))                                                VAX.4374
      BB=REALZ(YS(I,I))                                                 VAX.4375
      if ( iprsup .ge. 1 )                                              VAX.4376
     1 write(*,*) ' I, YS(I,I) =',  I, ys(i,i)                          VAX.4377
cc  $$$$ protection ?? $$$$                                             VAX.4378
C     IF((ABSZ(BB).LE.1.0E-18).OR.(ABSZ(AA).LE.1.0E-18))                VAX.4379
      IF((ABSZ(BB).LE.1.0E-18) .AND. (ABSZ(AA).LE.1.0E-18))             VAX.4380
     1 GO TO 9801                                                       VAX.4381
      GO TO 9802                                                        VAX.4382
 9801 CONTINUE                                                          VAX.4383
      YS(I,J)=YS(I,J)*1.0E10                                            VAX.4384
      ZS(I,I) = QN(I)**2 / YS(I,I) * 1.E-10                                     
C     ZO(I,I) = (QN(I)/YS(I,I))*1.0E10                                  VAX.4386
      write (*,*) 'qn, ys  for i=',qn(i),ys(i,j),i                      VAX.4387
      YS(I,I) = YS(I,I)*1.0E-10                                         VAX.4388
      GO TO 9803                                                        VAX.4389
 9802 CONTINUE                                                          VAX.4390
C     ZO(I,I) = QN(I)/YS(I,I)                                           VAX.4391
      ZS(I,I) = QN(I)**2 / YS(I,I)                                              
 9803 CONTINUE                                                          VAX.4393
cc  $$$$ end of protection  $$$$                                        VAX.4394
C     YO(I,I) = 1./ZO(I,I)                                              VAX.4395
C     ZS(I,I) = QN(I) * ZO(I,I)                                         VAX.4396
      YO(I,I) = CSQRTZ( YS(I,I)/ZS(I,I) )                                       
      ZO(I,I) = 1. / YO(I,I)                                                    
      GO TO 80                                                          M16.6876
   79 ZO(I,J)=CZERO                                                     M22.6775
      YO(I,J) = CZERO                                                   M22.6776
      ZS(I,J) = CMPLXZ(FZERO, FZERO)                                    M22.6777
   80 CONTINUE                                                          M16.6879
      IF (LASTOV .NE. 39)  GO TO 1980                                   M35.9561
      IF ( IPRINT .LT. 2 ) GO TO 1978                                   VAX.4399
C     YSRE = REALZ(YS(I,I))                                             VAX.4400
C     YSIM = AIMAGZ(YS(I,I))                                            VAX.4401
C     ZSRE = REALZ(ZS(I,I))                                             VAX.4402
C     ZSIM = AIMAGZ(ZS(I,I))                                            VAX.4403
CC      WRITE (LUNIT9)  W, YSRE,YSIM,ZSRE,ZSIM                          VAX.4404
      W1=W                                                              VAX.4405
C   ** THE UNIT HERE IS /km NOW! **                                     VAX.4406
C     ZZ(I)=ZSRE *1000.                                                 VAX.4407
C     ZZ(I+NCONPW)=ZSIM*1000.                                           VAX.4408
C     PS(I)=YSRE*1000.                                                  VAX.4409
C     PS(I+NCONPW)=YSIM*1000.                                           VAX.4410
      YCHARM = CABSZ(YO(I,I))                                           VAX.4411
      YCHARA = ATAN2Z( AIMAGZ(YO(I,I)), REALZ(YO(I,I)) )                VAX.4412
      ALPHA = REALZ(QN(I)) * 1000.                                      VAX.4413
      BETA = AIMAGZ(QN(I)) *1000.                                       VAX.4414
      IF (LASTOV .NE. 39)  GO TO 1980                                   VAX.4415
      IF ( IPRINT .LT. 2 ) GO TO 1978                                   VAX.4416
      WRITE(LUNIT9) W1,YCHARM, YCHARA, ALPHA, BETA                      VAX.4417
      DO 78 K = 1, NCONPW                                               VAX.4418
      PP1(K,I) = CABSZ(B(K,I))                                          VAX.4419
      PP2(K,I) = ATAN2Z( AIMAGZ(B(K,I)), REALZ(B(K,I)) )                VAX.4420
   78 CONTINUE                                                          VAX.4421
      WRITE(LUNIT9) ( PP1(K,I),PP2(K,I), K = 1, NCONPW )                VAX.4422
C    **********  THLTHL                                                 VAX.4423
 1978 IF ( IPRSUP .GE. 0 )                                              VAX.4424
C    **********  THLTHL                                                 VAX.4425
     1 WRITE (LUNIT6, 1979)  I, W, YCHARM, YCHARA, ALPHA, BETA          VAX.4426
 1979 FORMAT ( ' Ychar AND EIGENVALUE FOR MODE', I3,                    VAX.4427
     1' AT FREQUENCY =' , E12.5,  2X, 5H ARE , 4E12.5)                  VAX.4428
 1980 CONTINUE                                                          M35.9567
      IF (NUMAKI .GT. 3)   GO TO 9900                                   M20.6528
      WRITE (LUNIT6, 956)                                               VAX.4429
  956 FORMAT ( /, 10X, 'CHARACTERISTIC IMPEDANCES IN THE PHASE DOMAIN' )VAX.4430
      CALL MXM(A,ZO,CA,NCONPW,LDN)                                      M35.9568
      CALL MXM(CA,BI,ZE,NCONPW,LDN)                                     M35.9569
      CALL PRINT(ZE,NCONPW ,LL1,LDN)                                    M35.9570
   85 WRITE (LUNIT6, 305)                                               M19.3397
  305 FORMAT(//, 53X, 27H TABLE OF MODAL QUANTITIES.)                   M16.6886
      WRITE (LUNIT6, 315)                                               M17.1455
  315 FORMAT(//,11X, 5HMODAL,4X, 11HPROPAGATION, 7X, 15HMODAL IMPEDANCE,M16.6888
     112X, 5HMODAL, 6X, 26HCHARAC. IMP., NATURAL MODE, 4X, 26HCHARAC. ADM16.6889
     2M., NATURAL MODE,/, 5H MODE, 3X, 22HATTENUATION   VELOCITY, 7X,   M16.6890
     3 4HREAL, 7X, 9HIMAGINARY, 4X, 11HSUSCEPTANCE,7X, 4HREAL, 8X, 9HIMAM22.6778
     4GINARY, 9X, 4HREAL, 8X, 9HIMAGINARY,/,10X, 7H(DB/KM), 6X, 5H(M/S),M22.6779
     5 12X, 8HZ(OHM/M), 14X, 10HIMY(MHO/M), 9X, 17HSQRT(Z/Y) (OHM)  ,   M20.6529
     6 13X, 17HSQRT(Y/Z) (MHO)  )                                       M20.6530
      DO 205   I=1, NCONPW                                              M16.6895
      ZSR    = REALZ(ZS(I,I))                                           M18.1117
      ZSI   = AIMAGZ(ZS(I,I))                                           M18.1118
      YSI    = AIMAGZ(YS(I,I))                                          M18.1119
      ZOR    = REALZ(ZO(I,I))                                           M18.1120
      ZOI    = AIMAGZ(ZO(I,I))                                          M18.1121
      YOR     = REALZ(YO(I,I))                                          M18.1122
      YOI     = AIMAGZ(YO(I,I))                                         M18.1123
      WRITE (LUNIT6, 325)  I, AN(I), GN(I), ZSR, ZSI, YSI, ZOR, ZOI, YORM17.1456
     +, YOI                                                             M17.1457
  325 FORMAT(/, 1X, I3, 3X, 2E12.5, 1X, 3E14.6, 2(1X, 2E14.6) )         M16.6904
  205 CONTINUE                                                          M16.6905
      WRITE (LUNIT6, 335)                                               M17.1458
  335 FORMAT ( ///,  1X )                                               M23.5951
      GO TO 9900                                                        M23.5952
 9200 CONTINUE                                                          M22.6780
 9900 IF ( IPRS47  .GE.  1 )                                            M23.5953
     1 WRITE (LOGSIX, 2689)                                             M23.5954
 2689 FORMAT ( /,  16H EXIT  'PRCON' .  )                               M23.5955
      RETURN                                                            M23.5956
      END                                                               M16.6913
      SUBROUTINE UNWIND (PING,KTHL,MRR,NRP,NTOL,ISEQ)                   VAX.4431
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
CCCCC %INCLUDE  '//A/TSU/TPPLOTKOM.INS.FTN'                             VAX.4433
      DIMENSION PING(200),TPING(200)                                    VAX.4434
      DIMENSION PS(30),ZZ(30)                                           VAX.4435
      DIMENSION BOM(180),VFREQ(3), KUID(15), KUSE(15)                   VAX.4436
      DIMENSION ISEQ(15), ISEQA(15), ISEQT(15), IOLD(15)                VAX.4437
      DIMENSION SB(60), TT(4), TZZ(30), TPS(30)                         VAX.4438
      DATA  BOM  /  180*0.0D0  /                                        VAX.4439
      DATA  KUNF  / 0 /                                                 VAX.4440
C      WRITE(*,*) ' CHANGE ORDER ? 1 (for YES) or 2 (for NO)'           VAX.4441
C      READ(*,*) NY                                                     VAX.4442
      if ( iprsup .ge. 1 )                                              VAX.4443
     1 WRITE(*,*) ' Beginning of unwind.   KTHL, MRR, NRP, NTOL =',     VAX.4444
     2                                     KTHL, MRR, NRP, NTOL         VAX.4445
       NY=2                                                             VAX.4446
C      IF(NY .EQ. 2) GO TO 3800                                         VAX.4447
C      DO 3802 I=1,15                                                   VAX.4448
C      ISEQA(I)=I                                                       VAX.4449
C      ISEQT(I)=I                                                       VAX.4450
C 3802 CONTINUE                                                         VAX.4451
C      WRITE(*,*) ' FREQ ?'                                             VAX.4452
C      READ(*,*) CHF                                                    VAX.4453
C      WRITE(*,*) ' NEW ORDER ?'                                        VAX.4454
C      READ(*,3801) (ISEQA(K), K=1,15)                                  VAX.4455
C 3801 FORMAT( 15I3 )                                                   VAX.4456
C 3800 CONTINUE                                                         VAX.4457
      IF ((NTOL.GT.1).OR.(NRP.EQ.0)) GO TO 1050                         VAX.4458
      DO 1051 I=1,15                                                    VAX.4459
      ISEQ(I)=I                                                         VAX.4460
      IOLD(I)=I                                                         VAX.4461
 1051 CONTINUE                                                          VAX.4462
      DO 1052 I=1,180                                                   VAX.4463
      BOM(I)=0.0                                                        VAX.4464
 1052 CONTINUE                                                          VAX.4465
 1050 CONTINUE                                                          VAX.4466
      IF (ISEQ(1).NE.0) GO TO 10                                        VAX.4467
      DO 1001 I=1,15                                                    VAX.4468
      ISEQ(I)=I                                                         VAX.4469
 1001 CONTINUE                                                          VAX.4470
CC      WRITE(*,*)                                                      VAX.4471
CC     1 ' IDENTICAL EIGENVALUES ? 1 (for  YES) or 2 (for NO)'          VAX.4472
CC      READ(*,*) LWH                                                   VAX.4473
CC      IF (LWH .EQ. 2) GO TO 10                                        VAX.4474
CC      KUNF=1                                                          VAX.4475
CC      WRITE(*,*) ' MODE ?'                                            VAX.4476
CC      READ(*,4801) (KUID(K), K=1,15)                                  VAX.4477
CC 4801 FORMAT( 15I3 )                                                  VAX.4478
   10 CONTINUE                                                          VAX.4479
      DO 1002 I=1,3                                                     VAX.4480
      VFREQ(I)=0.                                                       VAX.4481
 1002 CONTINUE                                                          VAX.4482
CC    ****  READ DATA FROM 'PING' VECTOR  ****                          VAX.4483
      KPT=KTHL-1                                                        VAX.4484
      LI=KPT/4                                                          VAX.4485
      VFREQ(3)=VFREQ(2)                                                 VAX.4486
      VFREQ(2)=VFREQ(1)                                                 VAX.4487
      VFREQ(1)=PING(1)                                                  VAX.4488
      DO 1006 I=1,KPT+1                                                 VAX.4489
      TPING(I)=PING(I)                                                          
 1006 CONTINUE                                                          VAX.4491
      DO 1007 L=1,LI                                                    VAX.4492
      DO 1008 IM=1,4                                                    VAX.4493
      PING(4*L-IM+2)=TPING(4*ISEQ(L)-IM+2)                              VAX.4494
 1008 CONTINUE                                                          VAX.4495
      IOLD(L)=ISEQ(L)                                                   VAX.4496
 1007 CONTINUE                                                          VAX.4497
      DO 1003 I=1,KPT                                                   VAX.4498
      BOM(I+2*KPT)=BOM(I+KPT)                                           VAX.4499
      BOM(I+KPT)=BOM(I)                                                 VAX.4500
      BOM(I)=PING(I+1)                                                          
 1003 CONTINUE                                                          VAX.4502
      IF (NTOL .LT. 2)  RETURN                                          VAX.4503
CC     **** BEGIN TO SMOOTH ****                                        VAX.4504
CC  ** INITIALIZATION **                                                VAX.4505
CCCCC      DO 88 L=1, 15                                                VAX.4506
CCCCC      ISEQ(L) = L                                                  VAX.4507
CCCCC   88 CONTINUE                                                     VAX.4508
      DO 601 I=1,KPT                                                    VAX.4509
CCCCC      SB(I)=BOM(I)                                                 VAX.4510
      SB(I)=TPING(I+1)                                                  VAX.4511
  601 CONTINUE                                                          VAX.4512
      IC=0                                                              VAX.4513
      IQ=0                                                              VAX.4514
      IF ( IPRSUP .GE. 1 )  WRITE (*, *) '  *****  LI, kpt =',  LI, kpt VAX.4515
   99 IF (IC .GT. LI*(LI-1) ) GO TO 12                                  VAX.4516
      IF ( IQ .EQ. 1 ) GO TO 12                                         VAX.4517
      IQ=1                                                              VAX.4518
      DO 701 L=1,LI                                                     VAX.4519
      DO 702 IM=1, 4                                                    VAX.4520
      BOM(4*L-IM+1) = SB(4*ISEQ(L)-IM+1)                                VAX.4521
  702 CONTINUE                                                          VAX.4522
  701 CONTINUE                                                          VAX.4523
      IF ( IPRSUP .GE. 1 )                                              VAX.4524
     1 WRITE (*,*) ' Beginning  BOM(1:4*LI) =',  ( BOM(IP), IP=1, 4*LI )VAX.4525
C   * PROCESS THE DATA BETWEEN FREQ, FREQ-1 & FREQ-2 *                  VAX.4526
      DO 101 J=2, KPT-2, 4                                              VAX.4527
      DO 102 I=J+4, KPT-2, 4                                            VAX.4528
C   * TEST THE CROSSING OF ANG *                                        VAX.4529
      D1=BOM(J+KPT) - BOM(I+KPT)                                        VAX.4530
      D2=BOM(J) - BOM(I)                                                VAX.4531
      IF ( D1*D2 .LE. 0. ) GO TO 801                                    VAX.4532
      D1=BOM(J-1+KPT) - BOM(I-1+KPT)                                    VAX.4533
      D2=BOM(J-1) - BOM(I-1)                                                    
      IF ( D1*D2 .LE. 0. ) GO TO 801                                    VAX.4535
      GO TO 102                                                         VAX.4536
C   * TEST THE SLOPE CHANGE OF ANG , IF SWITCHING *                     VAX.4537
  801 CONTINUE                                                          VAX.4538
      AJ=0.                                                             VAX.4539
      AI=0.                                                             VAX.4540
      IF((BOM(J+KPT) - BOM(J+2*KPT)) .EQ. 0. ) AJ=1.0E-12               VAX.4541
      IF((BOM(I+KPT) - BOM(I+2*KPT)) .EQ. 0. ) AI=1.0E-12               VAX.4542
      SMJ1=(BOM(J) - BOM(J+KPT))/((BOM(J+KPT) - BOM(J+2*KPT))+AJ)       VAX.4543
      SMI1=(BOM(I) - BOM(I+KPT))/((BOM(I+KPT) - BOM(I+2*KPT))+AI)       VAX.4544
      SMJ2=(BOM(I) - BOM(J+KPT))/((BOM(J+KPT) - BOM(J+2*KPT))+AJ)       VAX.4545
      SMI2=(BOM(J) - BOM(I+KPT))/((BOM(I+KPT) - BOM(I+2*KPT))+AI)       VAX.4546
      IF ( IPRSUP .GE. 1 ) WRITE (*,*) ' SMJ1, SMJ2, SMI1, SMI2 =',     VAX.4547
     1                                   SMJ1, SMJ2, SMI1, SMI2         VAX.4548
      IF ((ABS( SMJ1 -1 ) .LT. ABS( SMJ2-1 )) .AND.                     VAX.4549
     1 (ABS( SMI1 -1 ) .LT. ABS( SMI2 -1 ))) GO TO 102                  VAX.4550
C   * TEST THE SLOPE CHANGE OF MAG , IF SWITCHING *                     VAX.4551
C     789012345678901234567890123456789012345678901234567890123456789012VAX.4552
      AJ=0.                                                             VAX.4553
      AI=0.                                                             VAX.4554
      IF((BOM(J-1+KPT) - BOM(J-1+2*KPT)) .EQ. 0. ) AJ=1.0E-12           VAX.4555
      IF((BOM(I-1+KPT) - BOM(I-1+2*KPT)) .EQ. 0. ) AI=1.0E-12           VAX.4556
      SMJ1=                                                             VAX.4557
     1 (BOM(J-1)-BOM(J-1+KPT))/((BOM(J-1+KPT)-BOM(J-1+2*KPT))+AJ)       VAX.4558
      SMI1=                                                             VAX.4559
     1 (BOM(I-1)-BOM(I-1+KPT))/((BOM(I-1+KPT)-BOM(I-1+2*KPT))+AI)       VAX.4560
      SMJ2=                                                             VAX.4561
     1 (BOM(I-1)-BOM(J-1+KPT))/((BOM(J-1+KPT)-BOM(J-1+2*KPT))+AJ)       VAX.4562
      SMI2=                                                             VAX.4563
     1 (BOM(J-1)-BOM(I-1+KPT))/((BOM(I-1+KPT)-BOM(I-1+2*KPT))+AI)       VAX.4564
      IF ((ABS( SMJ1 -1 ) .LT. ABS( SMJ2-1 )) .AND.                     VAX.4565
     1 (ABS( SMI1 -1 ) .LT. ABS( SMI2 -1 ))) GO TO 102                  VAX.4566
C   * SWITCHING *                                                       VAX.4567
      IQ=0                                                              VAX.4568
      T=ISEQ( (J+2)/4 )                                                 VAX.4569
      ISEQ( (J+2)/4 ) = ISEQ( (I+2)/4 )                                 VAX.4570
      ISEQ( (I+2)/4 ) = T                                               VAX.4571
      DO 401 IM=1,4                                                     VAX.4572
      TT(IM) = BOM(J+IM-2)                                              VAX.4573
      BOM(J+IM-2) = BOM(I+IM-2)                                         VAX.4574
      BOM(I+IM-2) = TT(IM)                                              VAX.4575
  401 CONTINUE                                                          VAX.4576
  102 CONTINUE                                                          VAX.4577
  101 CONTINUE                                                          VAX.4578
      IC=IC + 1                                                         VAX.4579
      GO TO 99                                                          VAX.4580
   12 CONTINUE                                                          VAX.4581
CC  **  BEGIN THE SECOND LOOP FOR VEL AND DB  **                        VAX.4582
C   * TEST THE SLOPE CHANGE OF VEL *                                    VAX.4583
      IC=0                                                              VAX.4584
      IQ=0                                                              VAX.4585
  899 IF (IC .GT. LI*(LI-1) ) GO TO 812                                 VAX.4586
      IF (IQ .EQ. 1 ) GO TO 812                                         VAX.4587
      IQ=1                                                              VAX.4588
      DO 9701 L=1,LI                                                    VAX.4589
      DO 9702 IM=1, 4                                                   VAX.4590
      BOM(4*L-IM+1) = SB(4*ISEQ(L)-IM+1)                                VAX.4591
 9702 CONTINUE                                                          VAX.4592
 9701 CONTINUE                                                          VAX.4593
C   * TEST THE SLOPE CHANGE OF VEL *                                    VAX.4594
      DO 8901 JB=1,LI                                                   VAX.4595
      JBEG=JB*4-1                                                       VAX.4596
      AJ=0.                                                             VAX.4597
      IF((BOM(JBEG+KPT)-BOM(JBEG+2*KPT)) .EQ. 0.) AJ=1.0E-12            VAX.4598
      SAJ=(BOM(JBEG)-BOM(JBEG+KPT))/                                    VAX.4599
     1 ((BOM(JBEG+KPT)-BOM(JBEG+2*KPT))+AJ)                             VAX.4600
      IF ( ABS( SAJ - 1 ) .GT. 0.5 ) GO TO 8001                         VAX.4601
 8901 CONTINUE                                                          VAX.4602
      GO TO 812                                                         VAX.4603
 8001 DO 8101 J=JBEG, KPT-1, 4                                          VAX.4604
      DO 8102 I=J+4, KPT-1, 4                                           VAX.4605
C   * TEST THE CROSSING OF VEL OR DB *                                  VAX.4606
      JB=(J+1)/4                                                        VAX.4607
      IB=(I+1)/4                                                        VAX.4608
      D1=BOM(J+KPT) - BOM(I+KPT)                                        VAX.4609
      D2=BOM(J) - BOM(I)                                                VAX.4610
      IF ( D1*D2 .LE. 0. ) GO TO 8801                                   VAX.4611
      D1=BOM(J+1+KPT) - BOM(I+1+KPT)                                    VAX.4612
      D2=BOM(J+1) - BOM(I+1)                                            VAX.4613
      IF ( D1*D2 .LE. 0. ) GO TO 8801                                   VAX.4614
      GO TO 8102                                                        VAX.4615
C      GO TO 8122                                                       VAX.4616
 8801 CONTINUE                                                          VAX.4617
      AJ=0.                                                             VAX.4618
      AI=0.                                                             VAX.4619
      IF((BOM(J+KPT) - BOM(J+2*KPT)) .EQ. 0. )  AJ=1.0E-12              VAX.4620
      IF((BOM(I+KPT) - BOM(I+2*KPT)) .EQ. 0. )  AI=1.0E-12              VAX.4621
      SMJ1=(BOM(J)-BOM(J+KPT))/((BOM(J+KPT)-BOM(J+2*KPT))+AJ)           VAX.4622
      SMI1=(BOM(I)-BOM(I+KPT))/((BOM(I+KPT)-BOM(I+2*KPT))+AI)           VAX.4623
      SMJ2=(BOM(I)-BOM(J+KPT))/((BOM(J+KPT)-BOM(J+2*KPT))+AJ)           VAX.4624
      SMI2=(BOM(J)-BOM(I+KPT))/((BOM(I+KPT)-BOM(I+2*KPT))+AI)           VAX.4625
      IF ((ABS( SMJ1 -1 ) .LT. ABS( SMJ2-1 )) .AND.                     VAX.4626
     1 (ABS( SMI1 -1 ) .LT. ABS( SMI2 -1 ))) GO TO 8102                 VAX.4627
C     1 (ABS( SMI1 -1 ) .LT. ABS( SMI2 -1 ))) GO TO 8132                VAX.4628
C   * TEST THE SLOPE CHANGE OF DB, IF SWITCH *                          VAX.4629
      AJ=0.                                                             VAX.4630
      AI=0.                                                             VAX.4631
      IF((BOM(J+1+KPT)-BOM(J+1+2*KPT)) .EQ. 0. ) AJ=1.0E-12             VAX.4632
      IF((BOM(I+1+KPT)-BOM(I+1+2*KPT)) .EQ. 0. ) AI=1.0E-12             VAX.4633
      SAJ=                                                              VAX.4634
     1 (BOM(I+1)-BOM(J+1+KPT))/((BOM(J+1+KPT)-BOM(J+1+2*KPT))+AJ)       VAX.4635
      SAI=                                                              VAX.4636
     1 (BOM(J+1)-BOM(I+1+KPT))/((BOM(I+1+KPT)-BOM(I+1+2*KPT))+AI)       VAX.4637
      IF ( ( ABS( SAJ-1 ) .GT. 1.0 ) .AND. ( ABS( SAI-1 ) .GT. 1.0 ))   VAX.4638
     1 GO TO 8102                                                       VAX.4639
C     1 GO TO 8142                                                      VAX.4640
C     789012345678901234567890123456789012345678901234567890123456789012VAX.4641
C   * SWITCHING *                                                       VAX.4642
      IQ=0                                                              VAX.4643
      T=ISEQ(JB)                                                        VAX.4644
      ISEQ(JB) = ISEQ(IB)                                               VAX.4645
      ISEQ(IB) = T                                                      VAX.4646
      DO 8401 IM=1,4                                                    VAX.4647
      TT(IM) = BOM(J+IM-3)                                              VAX.4648
      BOM(J+IM-3) = BOM(I+IM-3)                                         VAX.4649
      BOM(I+IM-3) = TT(IM)                                              VAX.4650
 8401 CONTINUE                                                          VAX.4651
C 8112 WRITE(*,*) VFREQ(1), I,J, 'STOP 1'                               VAX.4652
C      GO TO 8102                                                       VAX.4653
C 8122 WRITE(*,*) VFREQ(1), I,J, 'STOP 2'                               VAX.4654
C      GO TO 8102                                                       VAX.4655
C 8132 WRITE(*,*) VFREQ(1), I,J, 'STOP 3'                               VAX.4656
C      GO TO 8102                                                       VAX.4657
C 8142 WRITE(*,*) VFREQ(1), I,J, 'STOP 4'                               VAX.4658
C      GO TO 8102                                                       VAX.4659
 8102 CONTINUE                                                          VAX.4660
 8101 CONTINUE                                                          VAX.4661
      IC=IC+1                                                           VAX.4662
      GO TO 899                                                         VAX.4663
  812 CONTINUE                                                          VAX.4664
C   * CHECK THE EQUALITY BETWEEN TWO EIGENVALUES *                      VAX.4665
      IF(NRP.EQ.1) GO TO 2030                                           VAX.4666
      KU=0                                                              VAX.4667
      KUNF=0                                                            VAX.4668
      DO 1004 J=1, KPT-3, 4                                             VAX.4669
      MU=0                                                              VAX.4670
      DO 1005 I=J+4, KPT-3, 4                                           VAX.4671
      IF((ABS(BOM(J)-BOM(I)).GT.ABS(BOM(J)*1E-6)).OR.                   VAX.4672
     1 (ABS(BOM(J+1)-BOM(I+1)).GT.ABS(BOM(J+1)*1E-6)))GO TO 1005        VAX.4673
      IF((ABS(BOM(J+KPT)-BOM(I+KPT)).GT.ABS(BOM(J+KPT)*1E-6)).OR.       VAX.4674
     1 (ABS(BOM(J+1+KPT)-BOM(I+1+KPT)).GT.ABS(BOM(J+1+KPT)*1E-6)))      VAX.4675
     2  GO TO 1005                                                      VAX.4676
CCCC      IF((BOM(J).NE.BOM(I)).OR.(BOM(J+1).NE.BOM(I+1))) GO TO 1005   VAX.4677
CCCC      IF((BOM(J+KPT).NE.BOM(I+KPT)).OR.                             VAX.4678
CCCC     1 (BOM(J+1+KPT).NE.BOM(I+1+KPT))) GO TO 1005                   VAX.4679
      KUNF=1                                                            VAX.4680
      MU=1                                                              VAX.4681
      KUID(KU+1)=(I+3)/4                                                VAX.4682
      KU=KU+1                                                           VAX.4683
 1005 CONTINUE                                                          VAX.4684
      IF(MU.EQ.0) GO TO 1004                                            VAX.4685
      KUID(KU+1)=(J+3)/4                                                VAX.4686
      KU=KU+1                                                           VAX.4687
      if ( iprsup .ge. 1 )                                              VAX.4688
     1 write (*,*) ' Identical eigenvalues.   J, KU, KUID(1:KU) =',     VAX.4689
     2                                J, KU, ( KUID(IP), IP=1, KU )     VAX.4690
 1004 CONTINUE                                                          VAX.4691
      IF (KUNF.EQ.0) GO TO 2000                                         VAX.4692
      MRR=MRR+1                                                         VAX.4693
      GO TO 2000                                                        VAX.4694
 2030 CONTINUE                                                          VAX.4695
      DO 2007 I=1,KU                                                    VAX.4696
      KK=KU-I                                                           VAX.4697
      DO 2008 J=1,KK                                                    VAX.4698
      IF ( KUID(J).LT.KUID(J+1) ) GO TO 2008                            VAX.4699
      T=KUID(J)                                                         VAX.4700
      KUID(J)=KUID(J+1)                                                 VAX.4701
      KUID(J+1)=T                                                       VAX.4702
 2008 CONTINUE                                                          VAX.4703
      KUSE(I)=I                                                         VAX.4704
 2007 CONTINUE                                                          VAX.4705
      write(*,*) ' kuid after sorting : ',(KUID(I),I=1,KU)              VAX.4706
      DO 2002 I=1,KU                                                    VAX.4707
      KK=KU-I                                                           VAX.4708
      DO 2003 J=1,KK                                                    VAX.4709
      IF ( IOLD(KUID(J)) .LT. IOLD(KUID(J+1)) ) GO TO 2003              VAX.4710
      T=IOLD(KUID(J))                                                   VAX.4711
      IOLD(KUID(J))=IOLD(KUID(J+1))                                     VAX.4712
      IOLD(KUID(J+1))=T                                                 VAX.4713
      T=KUSE(J)                                                         VAX.4714
      KUSE(J)=KUSE(J+1)                                                 VAX.4715
      KUSE(J+1)=T                                                       VAX.4716
 2003 CONTINUE                                                          VAX.4717
 2002 CONTINUE                                                          VAX.4718
      DO 2011 I=1,KU                                                    VAX.4719
      KK=KU-I                                                           VAX.4720
      DO 2012 J=1,KK                                                    VAX.4721
      IF ( ISEQ(KUID(J)) .LT. ISEQ(KUID(J+1)) ) GO TO 2012              VAX.4722
      T=ISEQ(KUID(J))                                                   VAX.4723
      ISEQ(KUID(J))=ISEQ(KUID(J+1))                                     VAX.4724
      ISEQ(KUID(J+1))=T                                                 VAX.4725
 2012 CONTINUE                                                          VAX.4726
 2011 CONTINUE                                                          VAX.4727
      DO 2010 L=1,LI                                                    VAX.4728
      ISEQT(L)=ISEQ(L)                                                  VAX.4729
 2010 CONTINUE                                                          VAX.4730
      DO 2020 I=1,KU                                                    VAX.4731
      ISEQ(KUID(KUSE(I)))=ISEQT(KUID(I))                                VAX.4732
 2020 CONTINUE                                                          VAX.4733
   15 CONTINUE                                                          VAX.4734
C   * RE-ORDERING, IF NECESSARY *                                       VAX.4735
 2000 IF ( NY .EQ. 2 ) GO TO 2004                                       VAX.4736
      IF ( VFREQ(1) .NE. CHF ) GO TO 2004                               VAX.4737
      DO 500 L=1,LI                                                     VAX.4738
      ISEQT(L)=ISEQ(L)                                                  VAX.4739
  500 CONTINUE                                                          VAX.4740
      DO 501 L=1,LI                                                     VAX.4741
      ISEQ(L) = ISEQT(ISEQA(L))                                         VAX.4742
  501 CONTINUE                                                          VAX.4743
 2004 CONTINUE                                                          VAX.4744
      DO 2005 L=1,LI                                                    VAX.4745
      DO 2006 IM=1, 4                                                   VAX.4746
      BOM(4*L-IM+1) = SB(4*ISEQ(L)-IM+1)                                VAX.4747
 2006 CONTINUE                                                          VAX.4748
 2005 CONTINUE                                                          VAX.4749
      if ( iprsup .ge. 1 )                                              VAX.4750
     1 WRITE(*,*) ' End of unwind.  VFREQ(1) =',  VFREQ(1)              VAX.4751
      if ( iprsup .ge. 1 )                                              VAX.4752
     1 WRITE(*,*) ' End of unwind.   ISEQ(1:LI) =', (ISEQ(L),L=1,LI)    VAX.4753
      if ( iprsup .ge. 1 )                                              VAX.4754
     1 WRITE(*,*) ' End of unwind.   KTHL, MRR, NRP, NTOL =',           VAX.4755
     2                               KTHL, MRR, NRP, NTOL               VAX.4756
   11 CONTINUE                                                          VAX.4757
      DO 2001 I=1,KPT                                                   VAX.4758
      PING(I+1)=BOM(I)                                                  VAX.4759
 2001 CONTINUE                                                          VAX.4760
CC    **  REORDER THE MODAL QUANTITIES  **                              VAX.4761
CCCC      DO 1009 I=1,2*LI                                              VAX.4762
CCCC      TZZ(I)=ZZ(I)                                                  VAX.4763
CCCC      TPS(I)=PS(I)                                                  VAX.4764
CCCC 1009 CONTINUE                                                      VAX.4765
CCCC      DO 1010 I=1,LI                                                VAX.4766
CCCC      ZZ(I)=TZZ(ISEQ(I))                                            VAX.4767
CCCC      ZZ(I+LI)=TZZ(ISEQ(I)+LI)                                      VAX.4768
CCCC      PS(I)=TPS(ISEQ(I))                                            VAX.4769
CCCC      PS(I+LI)=TPS(ISEQ(I)+LI)                                      VAX.4770
CCCC 1010 CONTINUE                                                      VAX.4771
CCC      write(*,*) 'Done with UNWIND '                                 VAX.4772
      RETURN                                                            VAX.4773
      END                                                               VAX.4774
      SUBROUTINE ZYMX( W,NW,ISYST,NGRND, NGG,NCPP, RADI,ZY,YZ,DIR,      M35.9571
     1  DIJ,ANG,USI,USR,ESI,DR0,TH0,AL0,HI,DI,BIO,BI1,BI2,BI3,BI4,      M35.9572
     2  BI5,AL1I,AL2I,AL3I,DCI, NX, YZN,   YS,YC,ZP,ZPC,ZS,ZE,ZC,       M35.9573
     3  CA,CB,CC,CD,F, LDM, LDN, LDN2, LNQ2 )                           M35.9574
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMPLEX*16  ZPC(LDN, LDN), CMPLXZ, CJW                            M35.9575
      COMPLEX*16  YS(LDN, LDN), YC(LDN, LDN), ZP(LDN, LDN)              M35.9576
      COMPLEX*16  ZS(LDN, LDN), ZE(LDN, LDN), ZC(LDN, LDN)              M35.9577
      COMPLEX*16  CA(LDN, LDN), CB(LDN, LDN), CC(LDN, LDN)              M35.9578
      COMPLEX*16  CD(LDN, LDN), F(LDN, LDN2)                            M35.9579
      DIMENSION  NGG(LDN), NCPP(LDM), YZN(LNQ2)                         M35.9580
      DIMENSION  RADI(LDM, 7),  ZY(LDN, LDN),  YZ(LDN, LDN)             M35.9581
      DIMENSION  DIJ(LDM, LDM), ANG(LDM, LDM), USI(LDM, 3), USR(LDM, 3) M35.9582
      DIMENSION  DIR(LDM, LDM), ESI(LDM, 3),   DCI(LDM),    HI(LDM)     M35.9583
      DIMENSION  DR0(LDM, LDM), TH0(LDM, LDM), AL0(LDM, LDM), DI(LDN)   M35.9584
      DIMENSION  BIO(LDM), BI1(LDM),BI2(LDM), BI3(LDM), BI4(LDM)        M35.9585
      DIMENSION  BI5(LDM), AL1I(LDM), AL2I(LDM), AL3I(LDM)              M35.9586
      LL0=0                                                             M31.7396
      LL1=1                                                             M31.7397
      NX=NGRND                                                          M31.7398
      DO 5  I=1, LDN                                                    M35.9587
      DO 5  J=1, LDN                                                    M35.9588
      YS(I,J) = CZERO                                                   M22.6783
      YC(I,J) = CZERO                                                   M22.6784
      ZP(I,J) = CZERO                                                   M22.6785
      ZPC(I,J) = CZERO                                                  M22.6786
      ZS(I,J) = CZERO                                                   M22.6787
      ZC(I,J) = CZERO                                                   M22.6788
    5 ZE(I,J) = CZERO                                                   M22.6789
      CJW = CMPLXZ ( FZERO, W )                                         M22.6790
      LL2 = 2                                                           M16.6924
      IF ( IPRS47  .GE.  1 )                                            M23.5957
     1 WRITE (LOGSIX, 2706)  NW, ISYST, NP2, ITYPEC, NUMAKI, NPC,       M23.5958
     2                       NCC, IZFLAG, IYFLAG, W                     M23.5959
 2706 FORMAT ( /,  16H BEGIN  'ZYMX' .,  24H      NW   ISYST     NP2,   M23.5960
     1  48H  ITYPEC  NUMAKI     NPC     NCC  IZFLAG  IYFLAG,            M23.5961
     2  19X, 1HW, /, 16X, 9I8, E20.11 )                                 M23.5962
      GO TO  (710, 720, 30), ITYPEC                                     M20.6533
  710 CALL  OLZY ( W,NCPP,ZY,DIJ,ANG,USI,USR,ESI,HI,DI,ZS,ZE,ZC,        M35.9589
     1             LDM, LDN )                                           M35.9590
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6791
      GO TO 800                                                         M16.6927
  720 CALL  SCZY1 ( W, ISYST, ZY, DIR, DIJ, ANG, HI, DI, ZS, ZE,        M35.9591
     1              LDM, LDN )                                          M35.9592
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6792
      CALL  SCZY2 ( CJW, NCPP, RADI, USI, USR, BIO, BI1,                M35.9593
     1  BI2, BI3, BI4, BI5, AL1I, AL2I, AL3I, ZC, LDM, LDN )            M35.9594
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6793
      GO TO 800                                                         M20.6534
   30 IF ( NPP  .NE.  0 )                                               M20.6535
     1 CALL  SCZY1 ( W, ISYST, ZY, DIR, DIJ, ANG, HI, DI, ZS, ZE,       M35.9595
     2               LDM, LDN )                                         M35.9596
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6794
      CALL  SCZY2 ( CJW, NCPP, RADI, USI, USR, BIO, BI1,                M35.9597
     1  BI2, BI3, BI4, BI5, AL1I, AL2I, AL3I, ZC, LDM, LDN )            M35.9598
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6795
      CALL PTZY2(CJW,NCPP,DCI,DR0,TH0,AL0,ZP,ZPC,LDM,LDN)               M35.9599
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6796
  800 IF (NUMAKI .GT. 3)   GO TO 840                                    M20.6539
      IF (IPRS47 .LT. 1)   GO TO 840                                    M20.6540
  943 FORMAT(  /,10X,18HEARTH IMPEDANCE   )                             M16.6932
      WRITE(LUNIT6,944)                                                 M17.1460
  944 FORMAT (   / ,  10X,  31HCONDUCTOR INTERNAL IMPEDANCE      )      M16.6935
      CALL PRINT(ZC,NW,LL2,LDN)                                         M35.9600
      IF ( ITYPEC  .NE.  3 )   GO TO 105                                M20.6541
      WRITE(LUNIT6,961)                                                 M20.6542
  961 FORMAT( /, 10X,14HPIPE IMPEDANCE   )                              M20.6543
      CALL PRINT ( ZP, NW, LL2, LDN )                                   M35.9601
      IF ( NPP  .EQ.  0 )   GO TO 840                                   M20.6545
      WRITE(LUNIT6,962)                                                 M20.6546
  962 FORMAT( /, 10X,20HCONNECTION IMPEDANCE   )                        M20.6547
      CALL PRINT (ZPC, NW, LL2, LDN)                                    M35.9602
  105 WRITE (LUNIT6, 943)                                               M20.6549
      CALL PRINT(ZE,NW,LL2,LDN)                                         M35.9603
      IF ( ISYST  .EQ.  -1 )   GO TO 840                                M20.6551
      WRITE(LUNIT6, 946)                                                M22.6797
  946 FORMAT (  /, 10X, 18HSPACE IMPEDANCE   )                          M16.6940
      CALL PRINT(ZS,NW,LL2,LDN)                                         M35.9604
  840 DO 250  I=1, NW                                                   M20.6553
      DO 250  J=1, NW                                                   M20.6554
      C1 = E2P * YZ(I,J)                                                M22.6798
      YC(I,J) = CJW * CMPLXZ(C1, FZERO)                                 M22.6799
  250 ZC(I,J)=ZC(I,J)+ZE(I,J)+ZS(I,J)+ZP(I,J)+ZPC(I,J)                  M20.6556
      IF(ITYPEC .EQ. 1 .AND. NENERG .NE. 0)  GO TO 335                  VAX.4775
      I = NX                                                            M35.9605
 1945 I = I - 1                                                         M35.9606
      IF ( NGG(I)  .LT.  1 )  GO TO 1966                                M35.9607
      MDX = I + 1                                                       M35.9608
      DO 1931 J = 1, NW                                                 M35.9609
 1931 YZN(J) = REALZ(ZC(I, J))                                          M37.9120
      DO 1933 J = MDX, NX                                               M35.9611
      MDY = J - 1                                                       M35.9612
      DO 1933 K = 1, NW                                                 M35.9613
      YC(MDY, K) = YC(J, K)                                             M35.9614
 1933 ZC(MDY, K) = ZC(J, K)                                             M35.9615
      DO 1937 J = 1, NW                                                 M35.9616
 1937 ZC(NX, J) = CMPLXZ(YZN(J),FZERO)                                  M37.9121
      DO 1939 J = 1, NW                                                 M35.9618
 1939 YZN(J) = REALZ(ZC(J, I))                                          M37.9122
      DO 1941 J = MDX, NX                                               M35.9620
      MDY = J - 1                                                       M35.9621
      DO 1941 K = 1, NW                                                 M35.9622
      YC(K, MDY) = YC(K, J)                                             M35.9623
 1941 ZC(K, MDY) = ZC(K, J)                                             M35.9624
      DO 1943 J = 1, NW                                                 M35.9625
 1943 ZC(J, NX) = CMPLXZ(YZN(J),FZERO)                                  M37.9123
      NX = NX - 1                                                       M35.9627
 1966 IF ( I  .GT.  1 )  GO TO 1945                                     M35.9628
      IF (ITYPEC .NE. 1)   GO TO 300                                    M35.9629
      IF (NW-NX) 890, 340, 310                                          M35.9630
  300 IF(NX .EQ. NW)  GO TO 350                                         M35.9631
  310 CALL MINVN(ZC,NW,NX,LL1,LDN,CA,CB,CC,CD,ZE,ZP,ZPC,F,LDN2)         M35.9632
      IF(ITYPEC .NE. 1) GO TO 350                                       M31.7401
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6802
      GO TO 340                                                         VAX.4776
  335 NX = NW                                                                   
  340 IF (ISYST .EQ. 2)                                                 M35.9633
     1  CALL TRANSP( ZC,NCPP,YZN(1),YZN(LDN2),CA,LDM,LDN )              M35.9634
      IF (KILL .GE. 1)   GO TO 9200                                     M22.6803
      IF (IALTER .LE. 0)  GO TO 90                                      M23.5963
      IF (NUMAKI .LT. 9 )  GO TO 90                                     M24. 803
  350 IF(NCROS.EQ.0 .OR. NPAIS.LT.0) GO TO 890                          M31.7402
      NX=4                                                              M31.7403
      CALL CROSA4( ZC,LL0,LDN,CA,CB,CC,CD,ZE,ZP,ZPC,F,LDN2 )            M35.9635
      CALL CROSA4( YC,LL1,LDN,CA,CB,CC,CD,ZE,ZP,ZPC,F,LDN2 )            M35.9636
  890 N1 = NX                                                           M31.7406
      N2 = 2 * N1 * N1                                                  M24. 806
      K = 1                                                             M24. 807
      DO 898  J = 1, N1                                                 M24. 808
      DO 894  I = 1, N1                                                 M24. 809
      YZN(K) = REALZ(YC(I,J))                                           M35.9637
      L = K + 1                                                         M24. 812
      YZN(L) = AIMAGZ(YC(I,J))                                          M35.9638
      K = L + 1                                                         M24. 815
  894 CONTINUE                                                          M24. 816
  898 CONTINUE                                                          M24. 817
      WRITE (LUNIT3) (YZN(I), I=1, N2)                                  M35.9639
      IF (IPRSUP .LT. 1) GO TO 1983                                     M35.9640
      FREQ = W / TWOPI                                                  M35.9641
      WRITE (LUNIT6, 1900) FREQ, (YZN(I),I=1,N2)                        M35.9642
 1900 FORMAT ( 30H  Y WRITTEN ON LUNIT3 AT FREQ , E16.7,2X,             M35.9643
     1 3HARE,/, (1X,8E15.6) )                                           M35.9644
 1983 K = 1                                                             M35.9645
      DO 1918  J = 1, N1                                                M35.9646
      DO 1919  I = 1, N1                                                M35.9647
      YZN(K) = REALZ(ZC(I,J))                                           M35.9648
      L = K + 1                                                         M35.9649
      YZN(L) = AIMAGZ(ZC(I,J))                                          M35.9650
      K = L + 1                                                         M35.9651
 1919 CONTINUE                                                          M35.9652
 1918 CONTINUE                                                          M35.9653
      WRITE (LUNIT3) (YZN(I), I=1, N2)                                  M35.9654
      IF (IPRSUP .LT. 1) GO TO 90                                       M28.7437
      FREQ = W / TWOPI                                                  M28.7438
      WRITE (LUNIT6, 2010) FREQ, (YZN(I),I=1,N2)                        M35.9655
 2010 FORMAT ( 30H  Z WRITTEN ON LUNIT3 AT FREQ , E16.7,2X,             M35.9656
     1 3HARE,/, (1X,8E15.6) )                                           M28.7441
   90 IF (NUMAKI .GT. 3)   GO TO 900                                    M20.6558
      IF (IZFLAG .EQ. 0)   GO TO 100                                    M19.3402
      WRITE (LUNIT6, 94)                                                M18.1135
   94 FORMAT(/, 10X, 16HIMPEDANCE MATRIX)                               M18.1136
      CALL PRINT(ZC, NX  ,LL2, LDN )                                    M35.9657
      IF (IZFLAG .EQ. 1)   GO TO 150                                    M18.1138
  100 CONTINUE                                                          M22.6804
      WRITE (LUNIT6, 125)                                               M19.3404
  125 FORMAT(/, 10X, 25HRESISTANCE AND INDUCTANCE)                      M18.1140
C         Begin code associated with [Z] dump onto unit-34 plot file    VAX.4780
CCCC      K = 1                                                         VAX.4781
CCCC      SING(K) = ALOG1Z ( W / TWOPI )                                VAX.4782
      DO 140   I=1, NX                                                  M31.7408
      DO 140   J=1, NX                                                  M31.7409
      D1 = REALZ(ZC(I,J) )                                              M19.3405
      D2 = AIMAGZ( ZC(I,J) ) / W                                        M22.6805
CCCC      SING(K+1) = D1            ! Store resistance in REAL*4 vector VAX.4783
CCCC      SING(K+2) = D2            ! Store inductance in REAL*4 vector VAX.4784
CCCC      K = K + 2          !  Advance index to last-stored number     VAX.4785
  140 ZC(I,J) = CMPLXZ(D1, D2)                                          M19.3407
CCCC      WRITE (JUNIT4) ( SING(J), J=1, K ) ! Output vector written to VAX.4786
CCCC      write (*, 4488)  SING(1), SING(2), SING(3), SING(K-1), SING(K)VAX.4787
CCCC 4488 FORMAT ( ' SING(1), SING(2), SING(3), SING(K-1), SING(K) =',  VAX.4788
CCCC     1         F10.4, 4E13.4 )                                      VAX.4789
      CALL PRINT(ZC, NX   , LL2, LDN)                                   M35.9658
C     NOW RESTORE  'ZC'  TO IMPEDANCE IN  DO 4908  LOOP BELOW.          M18.1147
      DO 4908  I=1, NX                                                  M31.7411
      DO 4908  J=1, NX                                                  M31.7412
      D1 = REALZ(ZC(I,J))                                               M19.3408
      D2 = AIMAGZ( ZC(I,J) )  *  W                                      M22.6806
 4908 ZC(I,J) = CMPLXZ(D1, D2)                                          M19.3410
  150 IF (IYFLAG .EQ. 0)   GO TO 160                                    M18.1153
      WRITE (LUNIT6, 155)                                               M18.1154
  155 FORMAT(/, 10X, 17HADMITTANCE MATRIX)                              M18.1155
      CALL PRINT (YC, NX, LL2, LDN)                                     M35.9659
      IF (IYFLAG .EQ. 1)   GO TO 900                                    M18.1157
  160 WRITE (LUNIT6, 165)                                               M18.1158
  165 FORMAT(/, 10X, 27HCONDUCTANCE AND CAPACITANCE)                    M18.1159
      DO 180   I=1, NX                                                  M31.7414
      DO 180   J=1, NX                                                  M31.7415
      D3 = REALZ(YC(I,J))                                               M19.3411
      D4 = AIMAGZ( YC(I,J) ) / W                                        M22.6807
  180 YC(I,J) = CMPLXZ(D3,D4)                                           M19.3413
      CALL PRINT (YC, NX, LL2, LDN)                                     M35.9660
C     NOW RESTORE  'YC'  TO ADMITTANCE IN  DO 4928  LOOP BELOW.         M18.1166
      DO 4928  I=1, NX                                                  M31.7417
      DO 4928  J=1, NX                                                  M31.7418
      D1 = REALZ(YC(I,J))                                               M19.3414
      D2 = AIMAGZ( YC(I,J) ) * W                                        M22.6808
 4928 YC(I,J) = CMPLXZ(D1, D2)                                          M19.3416
 9200 CONTINUE                                                          M22.6809
  900 IF ( IPRS47  .GE.  1 )                                            M23.5966
     1 WRITE (LOGSIX, 2794)                                             M23.5967
 2794 FORMAT ( /,  15H EXIT  'ZYMX' .  )                                M23.5968
      RETURN                                                            M23.5969
      END                                                               M16.6982
      SUBROUTINE YMATRX (ISYST, LUNIT6, NCPP, ZY, YZ,                   M35.9661
     1   ESI, AL0, AL1I, AL2I, AL3I, A1, A2, LDM, LDN )                 M35.9662
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      DIMENSION  A1(LDN,LDN), A2(LDN,LDN), NCPP(LDM)                    M35.9663
      DIMENSION  ZY(LDN, LDN), YZ(LDN, LDN), AL0(LDM, LDM)              M35.9664
      DIMENSION  ESI(LDM, 3), AL1I(LDM), AL2I(LDM), AL3I(LDM)           M35.9665
      DO 10 I=1,LDN                                                     M35.9666
      DO 10 J=1,LDN                                                     M35.9667
      A1(I,J) = 0.0                                                     M22.6813
      A2(I,J) = 0.0                                                     M22.6814
      YZ(I,J) = 0.0                                                     M22.6815
   10 CONTINUE                                                          M22.6816
      IF ( IPRS47  .GE.  1 )                                            M22.6817
     1 WRITE (LUNIT6, 1719)  ISYST, NP2, ITYPEC, NPC, NPP               M22.6818
 1719 FORMAT ( /,  28H AT BEGINNING OF  'YMATRX' .,                     M22.6819
     1 40H   ISYST     NP2  ITYPEC     NPC     NPP    ,/,               M22.6820
     2 28X,  5I8  )                                                     M22.6821
      IF ( IPRS47  .GE.  2 )                                            M22.6822
     1 WRITE (LUNIT6, 1724)  ( I, NCPP(I), AL1I(I), AL2I(I), AL3I(I),   M22.6823
     2 ESI(I,1), ESI(I,2), ESI(I,3), I=1, 10)                           M22.6824
 1724 FORMAT ( /,  5X,  3HROW,  4X,  4HNCPP,  11X,  4HAL1I, 11X,4HAL2I, M22.6825
     1 11X,  4HAL3I,  7X,  8HESI(I,1),  7X,  8HESI(I,2),7X,8HESI(I,3),/,M22.6826
     2 ( 1X, I7, I8, 6E15.6 )   )                                       M22.6827
      IF(ITYPEC . EQ . 3 )  GO TO 25                                    M22.6828
      IF(ISYST.EQ.-1) GO TO 25                                          M22.6829
      DO 15 I=1,NPC                                                     M22.6830
      DO 15 J=1,NPC                                                     M22.6831
      IF(J.LT.I) GO TO 15                                               M22.6832
      YZ(I,J)=ZY(I,J)                                                   M22.6833
      YZ(J,I)=ZY(I,J)                                                   M22.6834
   15 CONTINUE                                                          M22.6835
      IF( ITYPEC . EQ . 1 ) GO TO 200                                   M22.6836
      DO 20 I=1,NCC                                                     M22.6837
      I1=I                                                              M22.6838
      IF(I.GT.NPC) I1=I-NPC                                             M22.6839
      IF(I.GT.NPC2) I1=I-NPC2                                           M22.6840
      DO 20 J=1,NCC                                                     M22.6841
      IF(J.LT.I) GO TO 20                                               M22.6842
      J1=J                                                              M22.6843
      IF(J.GT.NPC) J1=J-NPC                                             M22.6844
      IF(J.GT.NPC2) J1=J-NPC2                                           M22.6845
      YZ(I,J)=ZY(I1,J1)                                                 M22.6846
      YZ(J,I)=YZ(I,J)                                                   M22.6847
   20 CONTINUE                                                          M22.6848
   25 DO 40 I=1,NPC                                                     M22.6849
      I1=I+NPC                                                          M22.6850
      I2=I+NPC2                                                         M22.6851
      IF( I2 . GT . NCC )  GO TO 35                                     M22.6852
      A1(I2,I2)=AL3I(I)/ESI(I,3)                                        M22.6853
      A1(I2,I)=A1(I2,I2)                                                M22.6854
      A1(I,I2)=A1(I2,I2)                                                M22.6855
      A1(I1,I2)=A1(I2,I2)                                               M22.6856
      A1(I2,I1)=A1(I2,I2)                                               M22.6857
   35 IF(NCPP(I).EQ.1) I1=I                                             M22.6858
      IF(I1.GT.NPC2) GO TO 39                                           M22.6859
      A1(I1,I1)=AL2I(I)/ESI(I,2)+A1(I2,I2)                              M22.6860
      A1(I1,I)=A1(I1,I1)                                                M22.6861
      A1(I,I1)=A1(I1,I1)                                                M22.6862
   39 A1(I,I)=A1(I1,I1)+AL1I(I)/ESI(I,1)                                M22.6863
   40 CONTINUE                                                          M22.6864
      IF( ITYPEC . EQ . 3 )  GO TO 60                                   M22.6865
      DO 50 I=1,NCC                                                     M22.6866
      DO 50 J=1,NCC                                                     M22.6867
      IF(J.LT.I) GO TO 50                                               M22.6868
      YZ(I,J)=YZ(I,J)+A1(I,J)                                           M22.6869
      YZ(J,I)=YZ(I,J)                                                   M22.6870
   50 CONTINUE                                                          M22.6871
      GO TO 200                                                         M22.6872
   60 YPO1 = 0.0                                                        M22.6873
      IF(NPP.NE.0) YPO1 = ALPI / ES2                                    M22.6874
      IF(NPP.EQ.0) ZY(1,1)=0.                                           M22.6875
      IF(ISYST.NE.-1) YPO1 = YPO1 + ZY(1,1)                             M22.6876
      DO 80 I=1,NPC                                                     M22.6877
      I1=I+NPC                                                          M22.6878
      I2=I+NPC2                                                         M22.6879
      DO 70 J=1,NPC                                                     M22.6880
      J1=J+NPC                                                          M22.6881
      J2=J+NPC2                                                         M22.6882
      A2(I,J)=AL0(I,J)/ES1                                              M22.6883
      IF(NCPP(I).EQ.1) GO TO 65                                         M22.6884
      A2(I1,J)=A2(I,J)                                                  M22.6885
      A2(I1,J1)=A2(I,J)                                                 M22.6886
   65 IF(NCPP(J).EQ.1) GO TO 70                                         M22.6887
      A2(I,J1)=A2(I,J)                                                  M22.6888
      IF(NCPP(I).EQ.2) GO TO 67                                         M22.6889
      A2(I2,J)=A2(I,J)                                                  M22.6890
      A2(I2,J1)=A2(I,J)                                                 M22.6891
      A2(I2,J2)=A2(I,J)                                                 M22.6892
   67 IF(NCPP(J).EQ.2) GO TO 70                                         M22.6893
      A2(I,J2)=A2(I,J)                                                  M22.6894
      A2(I1,J2)=A2(I,J)                                                 M22.6895
   70 CONTINUE                                                          M22.6896
   80 CONTINUE                                                          M22.6897
      DO 90 I=1,NCC                                                     M22.6898
      IF(NPP.EQ.0) GO TO 85                                             M22.6899
      A1(I,NCC) = 0.                                                    M22.6900
      A1(NCC,I) = 0.                                                    M22.6901
      A2(I,NCC) = 0.                                                    M22.6902
      A2(NCC,I) = 0.                                                    M22.6903
   85 DO 90 J=1,NCC                                                     M22.6904
      IF(J.LT.I) GO TO 90                                               M22.6905
      YZ(I,J)=A1(I,J)+A2(I,J)+YPO1                                      M22.6906
      YZ(J,I)=YZ(I,J)                                                   M22.6907
   90 CONTINUE                                                          M22.6908
  200 IF ( IPRS47  .GE.  1 )                                            M23.5970
     1 WRITE (LOGSIX, 2876)  NCC, YPO1, ES1, ALPI, YZ(1,1), YZ(1,2)     M23.5971
 2876 FORMAT ( /,  17H EXIT  'YMATRX' .,  8H     NCC,  16X,  4HYPO1,    M23.5972
     1  17X,  3HES1,  16X,  4HALPI,  13X,  7HYZ(1,1),                   M23.5973
     2  13X,  7HYZ(1,2)  ,/,  17X,  I8,  5E20.11  )                     M23.5974
      RETURN                                                            M23.5975
      END                                                               M22.6910
      SUBROUTINE  SIMP(NW,H,DD,RAD,ZY,DIR,DIJ,ANG,LDM,LDN)              M35.9668
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      DIMENSION  RAD(LDN), H(LDM), DD(LDN),  ZY(LDN, LDN)               M35.9669
      DIMENSION  DIR(LDM, LDM), DIJ(LDM, LDM), ANG(LDM, LDM)            M35.9670
      IF ( IPRS47  .GE.  1 )                                            M23.5976
     1 WRITE (LOGSIX, 2924)  NW, ITYPEC, H(1), DD(1), DD(2), RAD(1),    M23.5977
     2                       RADP(3)                                    M23.5978
 2924 FORMAT ( /,  16H ENTER  'SIMP' .,  16H      NW  ITYPEC,           M23.5979
     1 16X,  4HH(1),  15X,  5HDD(1),  15X,  5HDD(2),  14X,  6HRAD(1),   M23.5980
     2 13X,  7HRADP(3)  ,/,  16X,  2I8,  5E20.11 )                      M23.5981
      DO 5  I=1, NW                                                     M16.6986
      DO 5  J=1, NW                                                     M16.6987
      DIJ(I,J) = 0.0                                                    M16.6988
      DIR(I,J) = 0.0                                                    M16.6989
      ANG(I,J) = 0.0                                                    M16.6990
    5 ZY(I,J) = 0.0                                                     M20.6630
      DO 30 I=1,NW                                                      M16.6992
      R = RAD(I)                                                        M20.6631
      IF ( ITYPEC  .EQ.  3 )   R = RADP(3)                              M20.6632
      DO 30 J=1,NW                                                      M16.6993
      IF (I .GE. J)   GO TO 15                                          M16.6994
      V = DD(I) - DD(J)                                                 M16.6995
      R1 = H(I) - H(J)                                                  M16.6996
      R2 = ABSZ ( H(I) + H(J) )                                         M31.7419
      ANG(I,J) = ATANZ(ABSZ(V)/R2)                                      M18.1173
      ANG(J,I)=ANG(I,J)                                                 M16.6999
      V = V*V                                                           M16.7000
      R1 = R1 * R1                                                      M16.7001
      R2 = R2 * R2                                                      M16.7002
      DIJ(I,J) = SQRTZ(V + R2)                                          M18.1174
      DIJ(J,I)=DIJ(I,J)                                                 M16.7004
      DIR(I,J) = SQRTZ(V + R1)                                          M18.1175
      DIR(J,I)=DIR(I,J)                                                 M16.7006
      GO TO 20                                                          M16.7007
   15 IF (I .GT. J)   GO TO 30                                          M16.7008
      ANG(I,J) = 0.                                                     M16.7009
      DIJ(I,J) = 2. * ABSZ ( H(I) )                                     M31.7420
      DIR(I,J) = R                                                      M20.6633
   20 ZY(I,J) = ALOGZ( DIJ(I,J)/DIR(I,J) )                              M20.6634
      ZY(J,I)=ZY(I,J)                                                   M16.7013
   30 CONTINUE                                                          M16.7014
      IF ( IPRS47  .GE.  1 )                                            M23.5982
     1 WRITE (LOGSIX, 2936)  DIR(1,2), DIJ(1,2), V, ZY(1,1), ZY(1,2)    M23.5983
 2936 FORMAT ( /,  15H EXIT  'SIMP' .,  12X,  8HDIR(1,2),               M23.5984
     1 12X,  8HDIJ(1,2),  19X,  1HV,  13X,  7HZY(1,1),                  M23.5985
     2 13X,  7HZY(1,2)  ,/,  15X,  5E20.11 )                            M23.5986
      IF ( IPRS47  .GE.  5 )                                            M23.5987
     1 WRITE (LOGSIX, 2948)  ( ( ZY(I,J), J=1, NW ),  I=1, NW )         M23.5988
 2948 FORMAT ( /,  47H EXIT  'SIMP' .   ((ZY(I,J), J=1, NW), I=1, NW)   M23.5989
     1 ,/,  ( 1X,  6E20.11 ) )                                          M23.5990
      RETURN                                                            M16.7015
      END                                                               M16.7016
      SUBROUTINE  SCZY1 ( W,ISYST,ZY,DIR,DIJ,ANG,HI,DI,ZS,ZE,LDM,LDN )  M35.9671
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      DIMENSION  ZY(LDN, LDN), DIR(LDM, LDM), DIJ(LDM, LDM)             M35.9672
      DIMENSION  ANG(LDM, LDM), HI(LDM), DI(LDN)                        M35.9673
      COMPLEX*16  ZS(LDN, LDN),  ZE(LDN, LDN)                           M35.9674
      COMPLEX*16  CJW, XE                                               M28.7454
      COMPLEX*16  CMPLXZ                                                M28.7455
      CJW = CMPLXZ ( FZERO, W )                                         M22.6912
      IF ( IPRS47  .GE.  1 )                                            M23.5991
     1 WRITE (LOGSIX, 3027)  ISYST, NPC, IEARTH, ITYPEC, ROE, U0, W     M23.5992
 3027 FORMAT ( /,  17H ENTER  'SCZY1' .,                                M23.5993
     1 32H   ISYST     NPC  IEARTH  ITYPEC,  17X,  3HROE,  18X,  2HU0,  M23.5994
     2 19X, 1HW, /, 17X, 4I8, 3E20.11 )                                 M23.5995
      C2 = U2P * ZY(1,1)                                                M22.6913
      IF(ITYPEC .EQ. 3) GO TO 50                                        M20.6635
      IF(ISYST.EQ.-1) GO TO 35                                          M16.7020
      DO 33   I = 1, NPC                                                M16.7021
      DO 33 J=1,NPC                                                     M16.7022
      C1 = U2P * ZY(I,J)                                                M22.6914
   33 ZS(I,J) = CJW * CMPLXZ ( C1, FZERO )                              M22.6915
   35 DO 40 I=1,NPC                                                     M16.7027
      DO 40 J=1,NPC                                                     M16.7028
      IF(J.LT.I) GO TO 40                                               M16.7029
      IF ( IEARTH  .EQ.  99 )   GO TO 37                                M16.7030
      BE1=DIR(I,J)*SQRTZ(U0/ROE)                                        M18.1178
      IF(ISYST.NE.-1) BE1=0.                                            M16.7032
      BE2=DIJ(I,J)*SQRTZ(U0/ROE)                                        M18.1179
      TH=ANG(I,J)                                                       M16.7034
      CALL ZEGEN(BE1,BE2,TH,W,XE,ISYST)                                 M16.7035
      GO TO 38                                                          M16.7036
   37 D12 = ABSZ( DI(J) - DI(I) )                                       M18.1180
      CALL ZEST ( HI(I), HI(J), D12, ROE, W, XE )                       M16.7038
   38 ZE(I,J) = XE                                                      M16.7039
      ZE(J,I)=XE                                                        M16.7040
   40 CONTINUE                                                          M16.7041
      IF ( IPRS47  .GE.  2 )                                            M23.5996
     1 WRITE (LOGSIX, 3039)                                             M23.5997
 3039 FORMAT ( /,  44H DIAGNOSTIC WITHIN  'SCZY1' ,   ZE(I,J)  FOR,     M23.5998
     1  18H(I,J)=1, ... NPC .    )                                      M23.5999
      LL0 = 0                                                           M23.6000
      IF ( IPRS47  .GE.  4 )                                            M23.6001
     1 CALL PRINT ( ZE(1,1), NPC, LL0, LDN )                            M35.9675
      IF( NPC . EQ . NCC ) GO TO 90                                     M22.6916
      IF (ISYST .EQ. -1)  GO TO  45                                     M22.6917
      DO 43  I=1, NCC                                                   M22.6918
      DO 43  J=1, NCC                                                   M22.6919
      IF (J .LT. I)  GO TO 43                                           M22.6920
      I1 = I                                                            M22.6921
      J1 = J                                                            M22.6922
      IF (I1 .GT. NPC2)  I1=I-NPC2                                      M22.6923
      IF (I1 .GT. NPC)  I1=I-NPC                                        M22.6924
      IF(J1 .GT. NPC2)  J1=J-NPC2                                       M22.6925
      IF (J1 .GT. NPC)  J1=J-NPC                                        M22.6926
      ZS(I,J) = ZS(I1,J1)                                               M22.6927
      ZS(J,I) = ZS(I,J)                                                 M22.6928
   43 CONTINUE                                                          M22.6929
   45 DO 47  I=1,NCC                                                    M22.6930
      DO 47  J=1,NCC                                                    M22.6931
      IF (J .LT. I)  GO TO 47                                           M22.6932
      I1 = I                                                            M22.6933
      J1 = J                                                            M22.6934
      IF (I1 .GT. NPC2)  I1=I-NPC2                                      M22.6935
      IF (I1 .GT. NPC)  I1=I-NPC                                        M22.6936
      IF(J1 .GT. NPC2)  J1=J-NPC2                                       M22.6937
      IF (J1 .GT. NPC)  J1=J-NPC                                        M22.6938
      ZE(I,J) = ZE(I1, J1)                                              M22.6939
      ZE(J,I) = ZE(I,J)                                                 M22.6940
   47 CONTINUE                                                          M22.6941
      GO TO 90                                                          M20.6636
   50 IF(ISYST.EQ.-1) GO TO 60                                          M20.6637
      IF(NPP.EQ.0) GO TO 90                                             M22.6942
      DO 55 I=1,NCC                                                     M20.6638
      DO 55 J=1,NCC                                                     M20.6639
      IF(J.LT.I) GO TO 55                                               M20.6640
      ZS(I,J) = CJW * CMPLXZ(C2, FZERO)                                 M22.6943
      ZS(J,I)=ZS(I,J)                                                   M20.6642
   55 CONTINUE                                                          M20.6643
   60 BE1=DIR(1,1)*SQRTZ(U0/ROE)                                        M20.6644
      IF(ISYST.NE.-1) BE1=0.                                            M20.6645
      BE2=DIJ(1,1)*SQRTZ(U0/ROE)                                        M20.6646
      TH=ANG(1,1)                                                       M20.6647
      CALL ZEGEN(BE1,BE2,TH,W,XE,ISYST)                                 M20.6648
      DO 70 I=1,NCC                                                     M20.6649
      DO 70 J=1,NCC                                                     M20.6650
   70 ZE(I,J)=XE                                                        M20.6651
   90 IF ( IPRS47  .GE.  2 )                                            M23.6003
     1 WRITE (LOGSIX, 3056)  XE                                         M23.6004
 3056 FORMAT ( /,  45H DIAGNOSTIC AT EXIT  'SCZY1' .   ZS(I,J)  FOR,    M23.6005
     1  33H  (I,J)=1, ... NPC .    REAL-XE =,  E20.11,                  M23.6006
     2 4X,  9HIMAG-XE =,  E20.11 )                                      M23.6007
      IF ( IPRS47  .GE.  4 )                                            M23.6008
     1 CALL PRINT ( ZS(1,1), NPC, LL0, LDN )                            M35.9676
      RETURN                                                            M23.6010
      END                                                               M16.7062
      SUBROUTINE  SCZY2 ( S, NCPP, RADI, USI, USR, BIO, BI1,            M35.9677
     1  BI2, BI3, BI4, BI5, AL1I, AL2I, AL3I, ZC, LDM, LDN )            M35.9678
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMMON  / KOMTHL /  PEKEXP                                        VAX.4790
      COMPLEX*16  S, SS, S1, S2, S3, S4, S5, S6, S8, SU0                VAX.4791
      COMPLEX*16  Z11, Z12, Z2I, Z2M, Z2O, Z23                          M28.7457
      COMPLEX*16  CEXPZ, CMPLXZ, CSQRTZ                                 M28.7458
      COMPLEX*16  S0, S7, Z3I, Z3M, Z3O, Z34                            M22.6944
      COMPLEX*16  C1, C2,  C3                                           M29.5598
      COMPLEX*16  ZC(LDN, LDN)                                          M35.9679
      DIMENSION  NCPP(LDM), RADI(LDM, 7), USI(LDM, 3), USR(LDM, 3)      M35.9680
      DIMENSION  BIO(LDM), BI1(LDM), BI2(LDM), BI3(LDM), BI4(LDM)       M35.9681
      DIMENSION  BI5(LDM), AL1I(LDM), AL2I(LDM), AL3I(LDM)              M35.9682
      LL1 = 1                                                           M20.6653
      LL2 = 2                                                           M20.6654
      IF ( IPRS47  .GE.  1 )                                            M23.6011
     1 WRITE (LOGSIX, 3107)  NPC, S                                     M23.6012
 3107 FORMAT ( /,  17H ENTER  'SCZY2' .,  8H     NPC,  14X,  6HREAL-S,  M23.6013
     1 14X,  6HIMAG-S  ,/,  17X,  I8,  2E20.11 )                        M23.6014
 1001 INM=0                                                             M35.9683
      DO 100 I=1,NPC                                                    M22.6946
      B0=BIO(I)                                                         M22.6947
      B1=BI1(I)                                                         M22.6948
      B2=BI2(I)                                                         M22.6949
      B3=BI3(I)                                                         M22.6950
      B4=BI4(I)                                                         M22.6951
      B5=BI5(I)                                                         M22.6952
      AL1=AL1I(I)                                                       M22.6953
      AL2=AL2I(I)                                                       M22.6954
      AL3=AL3I(I)                                                       M22.6955
      SS=CSQRTZ(S)                                                      M22.6956
      C1 = CMPLXZ(B0, FZERO)                                            M24. 821
      S0=C1*SS                                                          M24. 822
      C1 = CMPLXZ(B1, FZERO)                                            M24. 823
      S1 = C1 * SS                                                      M24. 824
      C1 = CMPLXZ(B2, FZERO)                                            M24. 825
      S2=C1*SS                                                          M24. 826
      C1 = CMPLXZ(B3, FZERO)                                            M24. 827
      S3=C1*SS                                                          M24. 828
      C1 = CMPLXZ(U2P, FZERO)                                           M24. 829
      SU0=S*C1                                                          M24. 830
      IF ( IPRS47  .GE.  2 )                                            M23.6015
     1 WRITE (LOGSIX, 3124)  I, B0, SU0, S0                             M23.6016
 3124 FORMAT ( /,  20H RE-LOOP OVER  'I' .,  8H       I,  18X,  2HB0,   M23.6017
     1 12X,  8HREAL-SU0,  12X,  8HIMAG-SU0,  13X,  7HREAL-S0,           M23.6018
     2 13X,  7HIMAG-S0  ,/,  19X,  I8,  5E20.11 )                       M23.6019
      C1 = CMPLXZ (USR(I,1), FZERO)                                     M24. 831
      IXA=0                                                             M22.6962
      XA=CABSZ(S0)                                                      M28.7459
      IF(RADI(I,1).LE.0.) XA=CABSZ(S1)                                  M28.7460
      IF(XA.GT.10.) IXA=1                                               M22.6964
      CALL BSIKM(S1,LL2,BIN,BKN,LL1,IXA)                                M24. 832
      IF(RADI(I,1).GT.0. ) GO TO 15                                     M22.6966
      Z11=SU0/S1*BIN(1)/BIN(2)*C1                                       M24. 833
      GO TO 25                                                          M22.6968
   15 S4=BIN(1)                                                         M22.6969
      S5=BIN(2)                                                         M22.6970
      S6=BKN(1)                                                         M22.6971
      Z12=BKN(2)                                                        M22.6972
      CALL BSIKM(S0,LL2,BIN,BKN,LL1,IXA)                                M24. 834
      IF (IXA .GT. 0)  GO TO 20                                         M22.6974
      SS = S5 * BKN(2) - Z12 * BIN(2)                                   M22.6975
      Z11=SU0/S1*(BKN(2)*S4+S6*BIN(2))/SS*C1                            M24. 835
      GO TO 25                                                          M22.6977
   20 SS=S1-S0                                                          M22.6978
      IF(CABSZ(SS) .GT. PEKEXP ) GO TO 23                               VAX.4792
      SS=CEXPZ(SS)                                                      M22.6980
      S7=S5*BKN(2)*SS-Z12*BIN(2)/SS                                     M28.7461
      Z11=SU0/S1*(BKN(2)*S4*SS+S6*BIN(2)/SS)/S7*C1                      M28.7462
      GO TO 25                                                          M22.6983
   23 Z11=SU0/S1*S4/S5*C1                                               M24. 837
   25 D1 = USI(I,1)*AL1                                                 M24. 838
      C1 = CMPLXZ(D1, FZERO)                                            M24. 839
      Z12=SU0*C1                                                        M24. 840
      Z2I = CZERO                                                       M24. 841
      Z2M = CZERO                                                       M24. 842
      Z2O = CZERO                                                       M24. 843
      D1 = USI(I,2) * AL2                                               M24. 844
      C1 = CMPLXZ(D1, FZERO)                                            M24. 845
      Z23 = SU0 * C1                                                    M24. 846
      IF ( IPRS47  .GE.  3 )                                            M23.6020
     1 WRITE (LOGSIX, 3136)  IXA, NCPP(I), RADI(I,1), Z11, Z12          M23.6021
 3136 FORMAT ( /,  1X,  16H     IXA NCPP(I),  11X,  9HRADI(I,1),        M23.6022
     1 12X,  8HREAL-Z11,  12X,  8HIMAG-Z11,                             M23.6023
     2 12X,  8HREAL-Z12,  12X,  8HIMAG-Z12  ,/,  1X,  2I8,  5E20.11 )   M23.6024
      Z3I = CZERO                                                       M24. 847
      Z3M = CZERO                                                       M24. 848
      Z3O = CZERO                                                       M24. 849
      D1 = USI(I,3) * AL3                                               M24. 850
      C1 = CMPLXZ(D1, FZERO)                                            M24. 851
      Z34 = SU0 * C1                                                    M24. 852
      IF(NCPP(I).EQ.1) GO TO 90                                         M22.6994
      IDO=2                                                             M22.6995
   28 IXA=0                                                             M28.7463
      XA=CABSZ(S2)                                                      M28.7464
      IF(XA.GT.10) IXA=1                                                M28.7465
      CALL BSIKM(S2,LL2,BIN,BKN,LL1,IXA)                                M28.7466
      S4=BIN(1)                                                         M22.6997
      S5=BIN(2)                                                         M22.6998
      S1=BKN(1)                                                         M22.6999
      S0=BKN(2)                                                         M22.7000
      CALL BSIKM(S3,LL2,BIN,BKN,LL1,IXA)                                M24. 854
      C1 = CMPLXZ(USR(I,IDO), FZERO)                                    M24. 855
      IF (IXA .GT. 0)  GO TO 35                                         M22.7002
      SS = BIN(2) * S0 - S5 * BKN(2)                                    M22.7003
      S7=SU0/S2*(S4*BKN(2)+S1*BIN(2))/SS*C1                             M24. 856
      S8=SU0/S3*(BIN(1)*S0+BKN(1)*S5)/SS*C1                             M24. 857
      GO TO 40                                                          M22.7006
   35 S6=S3-S2                                                          M22.7007
      IF(CABSZ(S6) .GT. PEKEXP ) GO TO 42                               VAX.4793
      S6=CEXPZ(S6)                                                      M22.7009
      SS=BIN(2)*S0*S6-S5*BKN(2)/S6                                      M22.7010
      S7=SU0/S2*(S4*BKN(2)/S6+S1*BIN(2)*S6)/SS*C1                       M28.7467
      S8=SU0/S3*(BIN(1)*S0*S6+BKN(1)*S5/S6)/SS*C1                       M24. 859
   40 IF (IDO .NE. 2 ) GO TO 41                                         M24. 860
      D1 = U2P*RADI(I,3)/RADI(I,4)/B2/B2*USR(I,2)                       M24. 861
      C2 = CMPLXZ(D1, FZERO)                                            M24. 862
      Z2M = C2 / SS                                                     M24. 863
      GO TO 44                                                          M24. 864
   41 D1 = U2P*RADI(I,5)/RADI(I,6)/B4/B4*USR(I,3)                       M24. 865
      C2 = CMPLXZ(D1, FZERO)                                            M24. 866
      IF (IDO .EQ. 3)  Z3M = C2/SS                                      M24. 867
      GO TO 44                                                          M22.7015
   42 S7=SU0/S2*S1/S0*C1                                                M24. 868
      S8=SU0/S3*BIN(1)/BIN(2)*C1                                        M24. 869
      Z2M = CZERO                                                       M24. 870
      Z3M = CZERO                                                       M24. 871
   44 IF ( IPRS47  .GE.  3 )                                            M23.6025
     1 WRITE (LOGSIX, 3148)  Z2I, Z2O, Z2M, Z23                         M23.6026
 3148 FORMAT ( /,  1X,                                                  M23.6027
     1 8X,  8HREAL-Z2I,  8X,  8HIMAG-Z2I,                               M23.6028
     2 8X,  8HREAL-Z2O,  8X,  8HIMAG-Z2O,                               M23.6029
     3 8X,  8HREAL-Z2M,  8X,  8HIMAG-Z2M,                               M23.6030
     4 8X,  8HREAL-Z23,  8X,  8HIMAG-Z23  ,/,  1X,  8E16.7  )           M23.6031
      IF ( IDO  .EQ.  3 )   GO TO 60                                    M23.6032
      Z2I=S7                                                            M22.7021
      Z2O=S8                                                            M22.7022
      Z3I = CZERO                                                       M24. 872
      Z3M = CZERO                                                       M24. 873
      Z3O = CZERO                                                       M24. 874
      Z34 = CZERO                                                       M24. 875
      IF(NCPP(I).EQ.2) GO TO 90                                         M22.7027
      SS=CSQRTZ(S)                                                      M22.7028
      C1 = CMPLXZ(B4, FZERO)                                            M24. 876
      S2=C1*SS                                                          M24. 877
      C1 = CMPLXZ(B5, FZERO)                                            M24. 878
      S3 = C1*SS                                                        M24. 879
      IDO=3                                                             M22.7031
      GO TO 28                                                          M22.7032
   60 Z3I=S7                                                            M22.7033
      Z3O=S8                                                            M22.7034
   90 S1=Z11+Z12+Z2I                                                    M22.7035
      S2=Z2O+Z23+Z3I                                                    M22.7036
      S3=Z3O+Z34                                                        M22.7037
      S4=S3-2.*Z3M                                                      M22.7038
      S5=S3-Z3M                                                         M22.7039
      S6=S2+S4                                                          M28.7468
      S7=S6-Z2M                                                         M28.7469
      S8 = CIMAG1                                                       M29.5599
      D1 = AIMAGZ(S5)                                                   M29.5600
      D2 = AIMAGZ(S6)                                                   M29.5601
      D3 = AIMAGZ(S7)                                                   M29.5602
      C1 = CMPLXZ(D1, FZERO)                                            M29.5603
      C2 = CMPLXZ(D2, FZERO)                                            M29.5604
      C3 = CMPLXZ(D3, FZERO)                                            M29.5605
      IF(REALZ(S5) .LT. 0.) S5=S8*C1                                    M29.5606
      IF(REALZ(S6) .LT. 0.) S6=S8*C2                                    M29.5607
      IF(REALZ(S7) .LT. 0.) S7=S8*C3                                    M29.5608
      ZC(I,I)=S1+S2-2.*Z2M+S4                                           M22.7040
      IF(NCPP(I).EQ.1) GO TO 100                                        M28.7474
      I1=I+NPC                                                          M22.7041
      IF(I1.GT.NPC2) GO TO 100                                          M22.7042
      ZC(I1,I1)=S6                                                      M28.7475
      ZC(I,I1)=S7                                                       M28.7476
      ZC(I1,I)=S7                                                       M28.7477
      IF(NCPP(I).EQ.2) GO TO 100                                        M22.7046
      I1=I+NPC2                                                         M22.7047
      ZC(I1,I1)=S3                                                      M22.7048
      ZC(I1,I)=S5                                                       M22.7049
      ZC(I,I1)=S5                                                       M22.7050
      I2=I+NPC                                                          M22.7051
      ZC(I1,I2)=S5                                                      M22.7052
      ZC(I2,I1)=S5                                                      M22.7053
      IF ( IPRS47  .GE.  3 )                                            M23.6033
     1 WRITE (LOGSIX, 3159)  S1, S2, ZC(I,I), ZC(I,I1)                  M23.6034
 3159 FORMAT ( /,  21H STORE  'ZC'  VALUES.,                            M23.6035
     1 13X,  7HREAL-S1,  13X,  7HIMAG-S1,                               M23.6036
     2 13X,  7HREAL-S2,  13X,  7HIMAG-S2  ,/,  21X,  4E20.11  ,/,       M23.6037
     3 8X,  12HREAL-ZC(I,I),  8X,  12HIMAG-ZC(I,I),                     M23.6038
     4 7X,  13HREAL-ZC(I,I1),  7X,  13HIMAG-ZC(I,I1)  ,/,               M23.6039
     5 1X,  4E20.11 )                                                   M23.6040
  100 CONTINUE                                                          M22.7054
      IF ( IPRS47  .GE.  1 )                                            M23.6041
     1 WRITE (LOGSIX, 3174)                                             M23.6042
 3174 FORMAT ( /,  16H EXIT  'SCZY2' .   )                              M23.6043
      RETURN                                                            M22.7055
      END                                                               M22.7056
      SUBROUTINE  PTZY1 ( RADI, DCI, THC, DR0, TH0, AL0, LDM )          M35.9684
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      DIMENSION  RADI(LDM, 7), DCI(LDM), THC(LDM)                       M35.9685
      DIMENSION  DR0(LDM, LDM), TH0(LDM, LDM), AL0(LDM, LDM)            M35.9686
      P2=2.*PAI                                                         M20.6720
      BP1=RADP(1)*SQRTZ(U0/ROP*USP)                                     M20.6721
      BP2=RADP(2)*SQRTZ(U0/ROP*USP)                                     M20.6722
      IF ( IPRS47  .GE.  1 )                                            M23.6044
     1 WRITE (LOGSIX, 3238)  NPP, NPC, BP1, BP2, RADP(3)                M23.6045
 3238 FORMAT ( /,  17H BEGIN  'PTZY1' .,  16H     NPP     NPC,          M23.6046
     1 17X,  3HBP1,  17X,  3HBP2,  13X,  7HRADP(3)  ,/,                 M23.6047
     2 17X,  2I8,  3E20.11 )                                            M23.6048
      IF (NPP .EQ. 0)   GO TO 5                                         M21. 544
      ALPI=ALOGZ(RADP(3)/RADP(2))                                       M20.6723
      GO TO 8                                                           M21. 545
    5 ALPI = 0.0                                                        M21. 546
    8 DO 50  I=1, NPC                                                   M21. 547
      DO 50 J=1,NPC                                                     M20.6725
      DR0(I,J)=DCI(I)*DCI(J)/RADP(1)**2                                 M20.6726
      IF(I-J) 10,20,50                                                  M20.6727
   10 TH0(I,J)=(THC(J)-THC(I))*PAI/180.                                 M20.6728
      IF(TH0(I,J).GT.P2) TH0(I,J)=TH0(I,J)-P2                           M20.6729
      DKL = SQRTZ(DCI(I)**2+DCI(J)**2-2.*DCI(I)*DCI(J)*COSZ(TH0(I,J)))  M20.6730
      AL0(I,J) = ALOGZ(RADP(1)/DKL)                                     M20.6731
      TH0(J,I)=TH0(I,J)                                                 M20.6732
      AL0(J,I)=AL0(I,J)                                                 M20.6733
      GO TO 50                                                          M20.6734
   20 TH0(I,J)=0.                                                       M20.6735
      DKL = RADI(I,7)                                                   M22.7057
      AL0(I,J) = ALOGZ(RADP(1)/DKL*(1.-DR0(I,J)))                       M20.6738
   50 CONTINUE                                                          M20.6739
      N = 19                                                            M20.6740
      DO 70   I=1, NPC                                                  M20.6741
      DO 70   J=1, NPC                                                  M20.6742
      CN = 0.0                                                          M20.6743
      IF (I .EQ. J)   GO TO 70                                          M20.6744
      IF (J .LT. I)   GO TO 70                                          M20.6745
      IF (DCI(I) * DCI(J) .LT. 1.0E-6)   GO TO 65                       M20.6746
      DO 60   K=1, N                                                    M20.6747
      AK = K                                                            M20.6748
      CN = DR0(I,J) **K * COSZ(AK * TH0(I,J)) / AK + CN                 M20.6749
   60 CONTINUE                                                          M20.6750
   65 AL0(I,J) = AL0(I,J) - CN                                          M20.6751
      AL0(J,I) = AL0(I,J)                                               M20.6752
   70 CONTINUE                                                          M20.6753
      IF ( IPRS47  .GE.  1 )                                            M23.6049
     1 WRITE (LOGSIX, 3256)  DKL, CN, ALPI, AL0(1,1), AL0(1,2)          M23.6050
 3256 FORMAT ( /,  16H EXIT  'PTZY1' .,  17X,  3HDKL,  18X,  2HCN,      M23.6051
     1 16X,  4HALPI,  12X,  8HAL0(1,1),  12X,  8HAL0(1,2)  ,/,          M23.6052
     2 16X,  5E20.11 )                                                  M23.6053
      IF ( IPRS47  .GE.  4 )                                            M23.6054
     1 WRITE (LOGSIX, 3263)  ( (AL0(I,J), J=1, NPC), I=1, NPC )         M23.6055
 3263 FORMAT ( /,  63H DIAGNOSTIC OUTPUT MATRIX.    ( (AL0(I,J), J=1, NPM23.6056
     1C), I=1, NPC)     ,/,  ( 1X,  6E20.11 ) )                         M23.6057
      RETURN                                                            M20.6754
      END                                                               M20.6755
      SUBROUTINE PTZY2(S,NCPP,DCI,DR0,TH0,AL0,ZP,ZPC,LDM,LDN)           M35.9687
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMMON  / KOMTHL /  PEKEXP                                        VAX.4794
      COMPLEX*16  S, SS, S1, S2, S3, S4, S5, S6, SU0                    M28.7478
      COMPLEX*16  CEXPZ, CMPLXZ, CSQRTZ                                 M28.7479
      COMPLEX*16  SE0, ZM, ZI, ZZO, ZZI                                 M28.7480
      COMPLEX*16  C1, C2, C3, C4, C5, C6                                M22.7058
      COMPLEX*16  ZP(LDN, LDN), ZPC(LDN, LDN)                           M35.9688
      DIMENSION  NCPP(LDM),  DCI(LDM)                                   M35.9689
      DIMENSION  DR0(LDM, LDM), TH0(LDM, LDM), AL0(LDM, LDM)            M35.9690
      UNITY = 1.0                                                       M22.7059
      CONTWO = 2.0                                                      M22.7060
      C3 = CMPLXZ(USP, FZERO)                                           M22.7061
      C4 = CMPLXZ(CONTWO, FZERO)                                        M22.7062
      C8 = ROP/2./PAI/RADP(1)/RADP(2)                                   M22.7063
      C6 = CMPLXZ(C8, FZERO)                                            M22.7064
      LL1 = 1                                                           M20.6758
      LL3 = 3                                                           M20.6759
      NBESS=19                                                          M20.6762
      KN=NBESS+1                                                        M20.6763
      SS=CSQRTZ(S)                                                      M20.6764
      SU0 = S * CMPLXZ(U2P, FZERO)                                      M22.7065
      SE0 = S * CMPLXZ(E2P, FZERO)                                      M22.7066
      S1 = SS * CMPLXZ(BP1, FZERO)                                      M22.7067
      S2 = SS * CMPLXZ(BP2, FZERO)                                      M22.7068
      ZM = CZERO                                                        M22.7069
      ZZO = SU0 * CMPLXZ(ALPI, FZERO)                                   M22.7070
      IXA=0                                                             M20.6771
      XA=CABSZ(S1)                                                      M28.7481
      IF(XA.GT.10.) IXA=1                                               M20.6773
      IF ( IPRS47  .GE.  1 )                                            M23.6058
     1 WRITE (LOGSIX, 3316)  NPC, NPP, XA, ALPI, S                      M23.6059
 3316 FORMAT ( /,  17H BEGIN  'PTZY2' .,  16H     NPC     NPP,          M23.6060
     1 18X,  2HXA,  16X,  4HALPI,  14X,  6HREAL-S,  14X,  6HIMAG-S      M23.6061
     2 ,/,  17X,  2I8,  4E20.11 )                                       M23.6062
      IF(NPP.EQ.0) GO TO 40                                             M28.7482
      CALL BSIKM(S2,KN,BIN,BKN,LL1,IXA)                                 M28.7483
      SE0=BIN(2)                                                        M28.7484
      S3=BKN(2)                                                         M28.7485
      S4=BIN(1)                                                         M28.7486
      S5=BKN(1)                                                         M28.7487
   40 CALL BSIKM(S1,KN,BIN,BKN,LL3,IXA)                                 M28.7488
      IF(NPP.NE.0) GO TO 42                                             M28.7489
      ZZI=SU0*C3*BKN(1)/BKN(2)/S1                                       M28.7490
      GO TO 55                                                          M28.7491
   42 IF (IXA .EQ. 1) GO TO 45                                          M28.7492
      SS = SE0*BKN(2)-BIN(2)*S3                                         M28.7493
      ZM = C6/SS                                                        M28.7494
      ZZO =SU0*C3/S2*(S4*BKN(2)+S5*BIN(2)                               M28.7495
     1     )/SS+ZZO                                                     M28.7496
      ZZI = SU0*C3*(BIN(1)*S3+BKN(1)*SE0)/SS/S1                         M28.7497
      GO TO 55                                                          M28.7498
   45 S6 = S2 - S1                                                      M28.7499
      IF (CABSZ(S6) .GT. PEKEXP ) GO TO 50                              VAX.4795
      S6 = CEXPZ(S6)                                                    M28.7501
      SS = SE0*BKN(2)*S6-BIN(2)*S3/S6                                   M28.7502
      ZM = C6/SS                                                        M28.7503
      ZZO=SU0*C3/S2*(S4*BKN(2)*S6+S5*BIN(2)/S6)                         M28.7504
     1    /SS+ZZO                                                       M28.7505
      ZZI=SU0*C3*(BIN(1)*S3/S6+BKN(1)*SE0*S6)/SS/S1                     M28.7506
      GO TO 55                                                          M28.7507
   50 ZZO=SU0*C3/S2*S4/SE0+ZZO                                          M28.7508
      ZZI=SU0*C3*BKN(1)/BKN(2)/S1                                       M28.7509
   55 CONTINUE                                                          M28.7510
      DO 30 I=1,NPC                                                     M20.6775
      DO 30 J=1,NPC                                                     M20.6776
      ZI = CZERO                                                        M22.7071
      IF(J.LT.I) GO TO 25                                               M20.6778
      IF(DCI(I)*DCI(J).LT.1.E-6) GO TO 21                               M20.6779
      DO 20 K=1,NBESS                                                   M20.6780
      K1=K+1                                                            M20.6781
      AK=K                                                              M20.6782
      CK=DR0(I,J)**K*COSZ(AK*TH0(I,J))                                  M20.6783
      C1 = CMPLXZ(CK, FZERO)                                            M22.7072
      C9 = AK * (USP + 1.)                                              M22.7073
      C2 = CMPLXZ(C9, FZERO)                                            M22.7074
      ZI = ZI + C1/ (C2 + S1 * BKN(K)/BKN(K1))                          M22.7075
   20 CONTINUE                                                          M20.6785
   21 C5 = CMPLXZ(AL0(I,J), FZERO)                                      M22.7076
      ZP(I,J)=SU0*(C3*C4*ZI + C5)                                       M28.7511
      ZP(J,I)=ZP(I,J)                                                   M20.6787
   25 I1 = I + NPC                                                      M22.7078
      J1=J+NPC                                                          M20.6789
      IF (NCPP(I) .EQ. 1)  GO TO 26                                     M22.7079
      ZP(I1,J)=ZP(I,J)                                                  M20.6790
      ZP(I1, J1) = ZP(I,J)                                              M22.7080
   26 IF (NCPP(J) .EQ. 1)  GO TO 30                                     M22.7081
      ZP(I,J1)=ZP(I,J)                                                  M20.6791
      I2 = I + NPC2                                                     M22.7082
      J2 = J + NPC2                                                     M22.7083
      IF (NCPP(I) .EQ.2)  GO TO 27                                      M22.7084
      ZP(I2,J) = ZP(I,J)                                                M22.7085
      ZP(I2,J2) = ZP(I,J)                                               M22.7086
      ZP(I2,J1) = ZP(I,J)                                               M22.7087
   27 IF (NCPP(J) .EQ. 2)  GO TO 30                                     M22.7088
      ZP(I,J2) = ZP(I,J)                                                M22.7089
      ZP(I1,J2) = ZP(I,J)                                               M22.7090
   30 CONTINUE                                                          M20.6793
      IF ( IPRS47  .GE.  2 )                                            M23.6063
     1 WRITE (LOGSIX, 3327)  ( ZP(1,J), J=1, 3 )                        M23.6064
 3327 FORMAT ( /,  8H MIDDLE.,                                          M23.6065
     1  8X,  12HREAL-ZP(1,1),  8X,  12HIMAG-ZP(1,1),                    M23.6066
     2  8X,  12HREAL-ZP(1,2),  8X,  12HIMAG-ZP(1,2),                    M23.6067
     3  8X,  12HREAL-ZP(1,3),  8X,  12HIMAG-ZP(1,3)  ,/,                M23.6068
     4  8X,  6E20.11  ,/,                                               M23.6069
     5  46H DIAGNOSTIC   ZP(I,J)  FOR  (I,J)=1, ... NPC .   )           M23.6070
      LL0 = 0                                                           M23.6071
      IF ( IPRS47  .GE.  5 )                                            M23.6072
     1 CALL PRINT ( ZP(1,1), NPC, LL0, LDN )                            M35.9691
      IF ( NPP  .EQ.  0 )   GO TO 90                                    M20.6794
      S1=ZZI+ZZO-2.*ZM                                                  M28.7512
      S2=ZZO-ZM                                                         M20.6812
      S3 = CIMAG1                                                       M29.5609
      D1 = AIMAGZ(S1)                                                   M29.5610
      D2 = AIMAGZ(S2)                                                   M29.5611
      C1 = CMPLXZ(D1, FZERO)                                            M29.5612
      C2 = CMPLXZ(D2, FZERO)                                            M29.5613
      IF(REALZ(S1) .LT. 0.) S1=S3*C1                                    M29.5614
      IF(REALZ(S2) .LT. 0.) S2=S3*C2                                    M29.5615
      IF ( IPRS47  .GE.  3 )                                            M23.6074
     1 WRITE (LOGSIX, 3345)  SS, ZM, ZZO                                M23.6075
 3345 FORMAT ( /,  6H PIPE.,                                            M23.6076
     1  13X,  7HREAL-SS,  13X,  7HIMAG-SS,                              M23.6077
     2  13X,  7HREAL-ZM,  13X,  7HIMAG-ZM,                              M23.6078
     3  12X,  8HREAL-ZZO,  12X,  8HIMAG-ZZO  ,/,  6X,  6E20.11 )        M23.6079
      NC1=NCC-NPP                                                       M20.6813
      IF (IPRS47 .GT. 1)                                                M20.6814
     1 WRITE (LOGSIX, 3354)  NCC, NC1, IXA, S1, S2                      M23.6080
 3354 FORMAT ( /,  11H MORE PIPE.,  24H     NCC     NC1     IXA,        M23.6081
     1  13X,  7HREAL-S1,  13X,  7HIMAG-S1,                              M23.6082
     2  13X,  7HREAL-S2,  13X,  7HIMAG-S2  ,/,  11X,  3I8,  4E20.11 )   M23.6083
      DO 65 I=1,NC1                                                     M20.6817
      ZP(I,NCC) = CZERO                                                 M22.7096
      ZP(NCC,I) = CZERO                                                 M22.7097
      ZPC(I,NCC)=S2                                                     M20.6820
      ZPC(NCC,I)=S2                                                     M20.6821
      DO 65 J=1,NC1                                                     M20.6822
      IF(J.LT.I) GO TO 65                                               M20.6823
      ZPC(I,J)=S1                                                       M20.6824
      ZPC(J,I)=S1                                                       M20.6825
   65 CONTINUE                                                          M20.6826
      ZP(NCC,NCC) = CZERO                                               M22.7098
      ZPC(NCC,NCC)=ZZO                                                  M20.6828
   90 IF ( IPRS47  .GE.  1 )                                            M23.6084
     1 WRITE (LOGSIX, 3368)  USP, PAI, U2P, E2P, SU0                    M23.6085
 3368 FORMAT ( /,  16H EXIT  'PTZY2' .,  16X,  3HUSP,  16X,  3HPAI,     M23.6086
     1 16X,  3HU2P,  16X,  3HE2P,  11X,  8HREAL-SU0,                    M23.6087
     2 11X,  8HIMAG-SU0  ,/,  16X,  6E19.10 )                           M23.6088
      RETURN                                                            M23.6089
      END                                                               M20.6830
      SUBROUTINE BSIKM (X, KN, BBIN, BBKN, IKM, IXA)                    M20.6831
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMPLEX*16  BK0, BK1, BI0, BJ1, X                                 M28.7516
      COMPLEX*16  Y, Y0, Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8                 M28.7517
      COMPLEX*16  CEXPZ, CMPLXZ, CLOGZ, CSQRTZ                          M28.7518
      COMPLEX*16  BBIN(KN), BBKN(KN)                                    M29.5616
      XA=CABSZ(X)                                                       M20.6834
      C1 = 3.75                                                         M23.6090
      C2 =  2.                                                          M23.6091
      Y = X/CMPLXZ(C1, FZERO)                                           M23.6092
      IF ( IPRS47  .GE.  5 )                                            M23.6093
     1 WRITE (LOGSIX, 3426)  KN, IKM, IXA, X, Y, XA                     M23.6094
 3426 FORMAT ( /,  17H BEGIN  'BSIKM' .,  24H      KN     IKM     IXA,  M23.6095
     1 9X,  6HREAL-X,  9X,  6HIMAG-X,  9X,  6HREAL-Y,  9X,  6HIMAG-Y,   M23.6096
     2 13X,  2HXA  ,/,  17X,  3I8,  5E15.6  )                           M23.6097
      IF (XA .GT. C1)  GO TO 25                                         M23.6098
      Y1 = Y * Y                                                        M20.6837
      Y2=Y1*Y1                                                          M20.6838
      Y3=Y2*Y1                                                          M20.6839
      Y4=Y3*Y1                                                          M20.6840
      Y5=Y4*Y1                                                          M20.6841
      Y6=Y5*Y1                                                          M20.6842
      BI0=1.+3.5156229*Y1+3.0899424*Y2+1.2067492*Y3+0.2659732*Y4+0.03607M20.6843
     268*Y5+0.0045813*Y6                                                M20.6844
      BJ1=X*(0.5+0.87890594*Y1+0.51498869*Y2+0.15084934*Y3+0.02658733*Y4M20.6845
     2+0.00301532*Y5+0.00032411*Y6)                                     M20.6846
      GO TO 29                                                          M28.7520
   25 Y0=CSQRTZ(X)                                                      M20.6848
      IF(IXA.NE.1) Y0=Y0*CEXPZ(-X)                                      M20.6849
      Y1=1./Y                                                           M20.6850
      Y2=Y1/Y                                                           M20.6851
      Y3=Y2/Y                                                           M20.6852
      Y4=Y3/Y                                                           M20.6853
      Y5=Y4/Y                                                           M20.6854
      Y6=Y5/Y                                                           M20.6855
      Y7=Y6/Y                                                           M20.6856
      Y8=Y7/Y                                                           M20.6857
      BI0=(0.39894228+0.01328592*Y1+0.00225319*Y2-0.00157565*Y3+0.009162M20.6858
     281*Y4-0.02057706*Y5+0.02635537*Y6-0.01647633*Y7+0.00392377*Y8)/Y0 M20.6859
      BJ1=(0.39894228-0.03988024*Y1-0.00362018*Y2+0.00163801*Y3-0.010315M20.6860
     255*Y4+0.02282967*Y5-0.02895312*Y6+0.01787654*Y7-0.00420059*Y8)/Y0 M20.6861
   29 IF (XA .GT. C2)  GO TO 35                                         M23.6099
      Y = X/CMPLXZ(C2, FZERO)                                           M23.6100
      Y0=CLOGZ(Y)                                                       M20.6864
      Y1=Y*Y                                                            M20.6865
      Y2=Y1*Y1                                                          M20.6866
      Y3=Y2*Y1                                                          M20.6867
      Y4=Y3*Y1                                                          M20.6868
      Y5=Y4*Y1                                                          M20.6869
      Y6=Y5*Y1                                                          M20.6870
      BK0=-Y0*BI0-0.57721566+0.42278420*Y1+0.23069756*Y2+0.03488590*Y3+0M20.6871
     2.00262698*Y4+0.00010750*Y5+0.00000740*Y6                          M20.6872
      BK1=Y0*BJ1+(1.+0.15443144*Y1-0.67278579*Y2-0.18156897*Y3-0.0191940M20.6873
     22*Y4-0.00110404*Y5-0.00004686*Y6)/X                               M20.6874
      GO TO 40                                                          M28.7521
   35 Y = CMPLXZ(C2, FZERO) / X                                         M23.6101
      Y0=CSQRTZ(X)                                                      M20.6877
      IF(IXA.NE.1) Y0=Y0*CEXPZ(X)                                       M20.6878
      Y1=Y*Y                                                            M20.6879
      Y2=Y1*Y                                                           M20.6880
      Y3=Y2*Y                                                           M20.6881
      Y4=Y3*Y                                                           M20.6882
      Y5=Y4*Y                                                           M20.6883
      BK0=(1.25331414-0.07832358*Y+0.02189568*Y1-0.01062446*Y2+0.0058787M20.6884
     22*Y3-0.00251540*Y4+0.00053208*Y5)/Y0                              M20.6885
      BK1=(1.25331414+0.23498619*Y-0.03655620*Y1+0.01504268*Y2-0.0078035M20.6886
     23*Y3+0.00325614*Y4-0.00068245*Y5)/Y0                              M20.6887
   40 BBIN(1) = BI0                                                     M20.6888
      BBIN(2)=BJ1                                                       M20.6889
      BBKN(1)=BK0                                                       M20.6890
      BBKN(2)=BK1                                                       M20.6891
      IF ( IPRS47  .GE.  5 )                                            M23.6102
     1 WRITE (LOGSIX, 3452)  BI0, BJ1, BK0, BK1                         M23.6103
 3452 FORMAT ( /,  9H SCALARS.,                                         M23.6104
     1  7X,  8HREAL-BI0,  7X,  8HIMAG-BI0,                              M23.6105
     2  7X,  8HREAL-BJ1,  7X,  8HIMAG-BJ1,                              M23.6106
     3  7X,  8HREAL-BK0,  7X,  8HIMAG-BK0,                              M23.6107
     4  7X,  8HREAL-BK1,  7X,  8HIMAG-BK1  ,/,  9X,  8E15.6  )          M23.6108
      IF(IKM.EQ.1) GO TO 70                                             M20.6892
      DO 45 IKN=3,KN                                                    M20.6893
      BBIN(IKN) = CREAL1                                                M23.6109
   45 BBKN(IKN) = CREAL1                                                M23.6110
      IF(IKM.EQ.3) GO TO 60                                             M20.6896
      DO 55   IKN = 3, KN                                               M20.6897
      IK1=IKN-1                                                         M20.6898
      IK2=IKN-2                                                         M20.6899
   55 BBIN(IKN) = BBIN(IK2) - 2. *IK2 / X * BBIN(IK1)                   M20.6900
      IF(IKM.EQ.2) GO TO 70                                             M20.6901
   60 DO 65 IKN=3,KN                                                    M20.6902
      IK1=IKN-1                                                         M20.6903
      IK2=IKN-2                                                         M20.6904
   65 BBKN(IKN) = BBKN(IK2) + 2. * IK2 / X * BBKN(IK1)                  M20.6905
   70 IF ( IPRS47  .GE.  6 )                                            M23.6111
     1 WRITE (LOGSIX, 3476)  ( L, BBIN(L), BBKN(L), L=1, KN )           M23.6112
 3476 FORMAT ( /,  18H EXIT    'BSIKM' .,  5X,  3HROW,                  M23.6113
     1  11X,  9HREAL-BBIN,  11X,  9HIMAG-BBIN,                          M23.6114
     2  11X,  9HREAL-BBKN,  11X,  9HIMAG-BBKN,                          M23.6115
     3  /,  (  18X,  I8,  4E20.11 )  )                                  M23.6116
      RETURN                                                            M23.6117
      END                                                               M20.6907
      SUBROUTINE  OLZY( W,NCPP,ZY,DIJ,ANG,USI,USR,ESI,HI,DI,ZS,ZE,ZC,   M35.9692
     1                  LDM,LDN )                                       M35.9693
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      DIMENSION  NCPP(LDM)                                              M35.9694
      DIMENSION  ZY(LDN, LDN), DIJ(LDM, LDM), ANG(LDM,LDM), DI(LDN)     M35.9695
      DIMENSION  USI(LDM, 3),  USR(LDM, 3),   ESI(LDM, 3),  HI(LDM)     M35.9696
      COMPLEX*16  ZS(LDN, LDN), ZE(LDN, LDN),  ZC(LDN, LDN)             M35.9697
      COMPLEX*16  CWU, S, SS, XC, XE                                    M28.7522
      COMPLEX*16  CMPLXZ, CSQRTZ                                        M28.7523
      S = CMPLXZ(FZERO, W)                                              M22.7099
      CWU = S * CMPLXZ(U2P, FZERO)                                      M22.7100
      SS = CSQRTZ ( S )                                                 M18.1200
      IF ( IPRS47  .GE.  1 )                                            M23.6118
     1 WRITE (LOGSIX, 3508)  IEARTH, NCCT, U2P, W, SS                   M23.6119
 3508 FORMAT ( /,  16H BEGIN  'OLZY' .,  16H  IEARTH    NCCT,           M23.6120
     1  17X, 3HU2P, 19X, 1HW,                                           M23.6121
     2  13X,  7HREAL-SS,  13X,  7HIMAG-SS  ,/,  16X,  2I8,  4E20.11 )   M23.6122
      LL1 = 1                                                           M16.7132
      DO 20  I=1, NPC                                                   M16.7133
      DO 20  J=1, NPC                                                   M16.7134
      IF ( J  .LT.  I )   GO TO 20                                      M16.7135
      ZS(I,J) = CWU * CMPLXZ(ZY(I,J), FZERO)                            M22.7101
      ZS(J, I) = ZS(I,J)                                                M22.7102
      IF ( IEARTH  .EQ.  99 )   GO TO 15                                M16.7140
      B1 = DIJ(I,J) * SQRTZ ( U0 / ROE )                                M18.1201
      B2 = ANG(I,J)                                                     M16.7142
      ZERO = 0.0                                                        M16.7143
      CALL ZEGEN ( ZERO, B1, B2, W, XE, LL1)                            M16.7144
      GO TO 18                                                          M16.7145
   15 D12 = ABSZ( DI(J) - DI(I) )                                       M18.1202
      CALL ZEST (HI(I), HI(J), D12, ROE, W, XE )                        M16.7147
   18 ZE(I,J) = XE                                                      M16.7148
      ZE(J,I) = XE                                                      M16.7149
   20 CONTINUE                                                          M16.7150
      IF ( IPRS47  .GE.  2 )                                            M23.6123
     1 WRITE (LOGSIX, 3523)  NCCT, NPC, U0, ROE                         M23.6124
 3523 FORMAT (                                                          M23.6125
     1  63H DIAGNOSTIC WITHIN  'OLZY' .   ZE(I,J)  FOR  (I,J)=1, ... NPCM23.6126
     2 .,  5X,  16H    NCCT     NPC,  13X,  2HU0,  12X,  3HROE  ,/,     M23.6127
     3  68X,  2I8,  2E15.6  )                                           M23.6128
      LL0 = 0                                                           M23.6129
      IF ( IPRS47  .GE.  4 )                                            M23.6130
     1 CALL PRINT ( ZE(1,1), NPC, LL0, LDN )                            M35.9698
      JNC = 1                                                           M16.7151
      DO 40  J = 1, 2                                                   M16.7152
      DO 40  I=1, NCCT                                                  M16.7153
      IM = 4*(I-1) + 1                                                  M16.7154
      J1 = IM + J + 1                                                   M16.7155
      J1 = NCPP(J1)                                                     M16.7156
      IF ( J1  .EQ.  0 )   GO TO 40                                     M16.7157
      B1 = USI(I,J)                                                     M16.7158
      B2 = ESI(I,J)                                                     M16.7159
      UR=USR(I,J)                                                       M16.7160
      CALL SKIN47 (B1,B2,UR,S,SS,XC)                                    M16.7161
      XC = XC / J1                                                      M16.7162
      J1 = IM + J - 1                                                   M16.7163
      J1 = NCPP(J1)                                                     M16.7164
      J2 = J1 + JNC - 1                                                 M16.7165
      DO 35  K=JNC, J2                                                  M16.7166
   35 ZC(K,K) = XC                                                      M16.7167
      IF ( IPRS47  .GE.  6 )                                            M23.6132
     1 WRITE (LOGSIX, 3541)  J, I, JNC, J2, J1, XC                      M23.6133
 3541 FORMAT ( /, 33H INSIDE  (J,I)  LOOP OF  'OLZY' .,                 M23.6134
     1  40H       J       I     JNC      J2      J1,  13X,  7HREAL-XC,  M23.6135
     2  13X,  7HIMAG-XC  ,/,  33X,  5I8,  2E20.11  )                    M23.6136
      JNC = J2 + 1                                                      M16.7168
   40 CONTINUE                                                          M16.7169
      IF ( IPRS47  .GE.  1 )                                            M23.6137
     1 WRITE (LOGSIX, 3554)                                             M23.6138
 3554 FORMAT ( /,  15H EXIT  'OLZY' .  )                                M23.6139
      RETURN                                                            M16.7170
      END                                                               M16.7171
      SUBROUTINE TRANSP(YYC, NCPP, ANN, JNN, ZNN , LDM, LDN)            M35.9699
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMPLEX*16  CMPLXZ                                                M28.7524
      COMPLEX*16  YYC(LDN,LDN), ZSS, ZMM, ZNN(LDN)                      M35.9700
      DIMENSION  NCPP(LDM), ANN(LDN), JNN(LDN)                          M35.9701
      IF ( IPRS47  .GE.  1 )                                            M23.6140
     1 WRITE (LOGSIX, 3611)  NCCT, YYC(1,1), YYC(1,2)                   M23.6141
 3611 FORMAT ( /,  18H ENTER  'TRANSP' .,  8H    NCCT,                  M23.6142
     1  7X,  13HREAL-YYC(1,1),  7X,  13HIMAG-YYC(1,1),                  M23.6143
     2  7X,  13HREAL-YYC(1,2),  7X,  13HIMAG-YYC(1,2)                   M23.6144
     3  ,/,  18X,  I8,  4E20.11  ,/,    60H DIAGNOSTIC INPUT MATRIX.   YM23.6145
     4YC(I,J)  FOR  (I,J)=1, ... I2 .   )                               M23.6146
      I2 = 6                                                            M23.6147
      LL0 = 0                                                           M23.6148
      IF ( IPRS47  .GE.  4 )                                            M23.6149
     1 CALL PRINT ( YYC(1,1), I2, LL0, LDN )                            M35.9702
      I2 = 0                                                            M20.6912
      DO 100 K=1, NCCT                                                  M20.6913
      K1 = (K-1)*4 + 1                                                  M20.6914
      J1 = I2 + 1                                                       M20.6915
      I2 = NCPP(K1) + I2                                                M20.6916
      JNN(K) = J1                                                       M20.6917
      ZSS = CZERO                                                       M22.7103
      ASS = 0.0                                                         M20.6919
      ZMM = CZERO                                                       M22.7104
      AMM = 0.0                                                         M20.6921
      DO 5  I=1, NCCT                                                   M20.6922
      ZNN(I) = CZERO                                                    M32, 647
      ANN(I) = 0.0                                                      M20.6924
    5 CONTINUE                                                          M20.6925
      DO 45  I=1, I2                                                    M20.6926
      DO 40  J=J1, I2                                                   M20.6927
      IF ( I  .LT.  J1 )   GO TO 20                                     M20.6928
      IF ( J - I )   40,  10,  15                                       M20.6929
   10 ZSS = ZSS + YYC(I,J)                                              M20.6930
      ASS = ASS + 1.0                                                   M20.6931
      GO TO 40                                                          M20.6932
   15 ZMM = ZMM + YYC(I,J)                                              M20.6933
      AMM = AMM + 1.0                                                   M20.6934
      GO TO 40                                                          M20.6935
   20 IF ( K  .EQ.  1 )   GO TO 40                                      M20.6936
      DNN =  ( J - JNN(K) ) / ( JNN(K) - 1 )                            M20.6937
      IF ( DNN  .LT.  1.0 )   DNN = 0.0                                 M20.6938
      L = DNN + 1.0                                                     M20.6939
      ZNN(L) = ZNN(L) + YYC(I,J)                                        M20.6940
      ANN(L) = ANN(L) + 1.0                                             M20.6941
   40 CONTINUE                                                          M20.6942
   45 CONTINUE                                                          M20.6943
      ZSS = ZSS/CMPLXZ(ASS, FZERO)                                      M22.7105
      ZMM = ZMM/ CMPLXZ(AMM, FZERO)                                     M22.7106
      K1 = K - 1                                                        M20.6946
      IF ( K1  .LE.  0 )   GO TO 55                                     M20.6947
      DO 50  I=1, K1                                                    M20.6948
      ZNN(I) = ZNN(I) / CMPLXZ(ANN(I), FZERO)                           M22.7107
   50 CONTINUE                                                          M20.6950
   55 DO 95  I=1, I2                                                    M20.6951
      DO 90 J=J1, I2                                                    M20.6952
      IF ( J  .LT.  I )   GO TO 90                                      M20.6953
      IF ( I  .LT.  J1 )   GO TO 70                                     M20.6954
      IF ( J - I )   90,  60,  65                                       M20.6955
   60 YYC(I,J) = ZSS                                                    M20.6956
      GO TO 90                                                          M20.6957
   65 YYC(I,J) = ZMM                                                    M20.6958
      GO TO 85                                                          M20.6959
   70 IF ( K  .EQ.  1 )   GO TO 85                                      M20.6960
      DNN = ( J - JNN(K) ) / ( JNN(K) - 1 )                             M20.6961
      IF ( DNN  .LT.  1.0 )   DNN = 0.0                                 M20.6962
      L = DNN + 1.0                                                     M20.6963
      YYC(I,J) = ZNN(L)                                                 M20.6964
   85 YYC(J,I) = YYC(I,J)                                               M20.6965
   90 CONTINUE                                                          M20.6966
   95 CONTINUE                                                          M20.6967
  100 CONTINUE                                                          M20.6968
      IF ( IPRS47  .GE.  1 )                                            M23.6151
     1 WRITE (LOGSIX, 3623)  YYC(1,1), YYC(1,2)                         M23.6152
 3623 FORMAT ( /,  17H EXIT  'TRANSP' .,                                M23.6153
     1  7X,  13HREAL-YYC(1,1),  7X,  13HIMAG-YYC(1,1),                  M23.6154
     2  7X,  13HREAL-YYC(1,2),  7X,  13HIMAG-YYC(1,2)                   M23.6155
     3  ,/,  17X,       4E20.11  ,/,    60H DIAGNOSTIC OUTPUT MATRIX.  YM23.6156
     4YC(I,J)  FOR  (I,J)=1, ... I2 .   )                               M23.6157
      IF ( IPRS47  .GE.  4 )                                            M23.6158
     1 CALL PRINT ( YYC(1,1), I2, LL0, LDN )                            M35.9703
      RETURN                                                            M20.6969
      END                                                               M20.6970
      SUBROUTINE SKIN47(B1,B2,UR,CJW,SJW,ZCC)                           M16.7172
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMMON  / KOMTHL /  PEKEXP                                        VAX.4796
      COMPLEX*16  CJW, SJW, X1, X2, X3, X4, ZCC                         M28.7525
      COMPLEX*16  CEXPZ, CMPLXZ                                         M28.7526
      COMPLEX*16  C1, C2                                                M22.7108
      C1 = CMPLXZ(U2P, FZERO)                                           M22.7109
      C2 = CMPLXZ(UR, FZERO)                                            M22.7110
      X1 = CMPLXZ(B1, FZERO) * SJW                                      M22.7111
      X2 = CMPLXZ(B2, FZERO) * SJW                                      M22.7112
      IXA = 0                                                           M16.7176
      XA = CABSZ(X2)                                                    M18.1203
      LL1 = 1                                                           M16.7178
      LL2 = 2                                                           M20.6971
      D15 = VALUE1 * VALUE5 / 100.                                      M16.7179
      IF ( IPRS47  .GE.  2 )                                            M23.6160
     1 WRITE (LOGSIX, 3672)  B1, B2, D15, XA, XB                        M23.6161
 3672 FORMAT ( /,  18H BEGIN  'SKIN47' .,  18X,  2HB1,  18X,  2HB2,     M23.6162
     1  17X,  3HD15,  18X,  2HXA,  18X,  2HXB  ,/,  18X,  5E20.11  )    M23.6163
      IF (B1 .LT. D15)   GO TO 30                                       M16.7180
      XB = CABSZ(X1)                                                    M18.1204
      TEN = 10.                                                         M23.6164
      IF (XA .LT. TEN)  GO TO 10                                        M23.6165
      D1 = 15./4.                                                       M16.7183
      IF (XB .GT. D1)  IXA = 1                                          M16.7184
   10 CALL BSIKM ( X1, LL2, BIN, BKN, LL1, IXA )                        M20.6972
      X3 = BIN(2)                                                       M20.6973
      X4 = BKN(2)                                                       M20.6974
      CALL BSIKM ( X2, LL2, BIN, BKN, LL1, IXA )                        M20.6975
      IF (IXA .EQ. 1) GO TO 20                                          M16.7189
      ZCC = CJW * C1/X2 * (BIN(1) * X4 + BKN(1) * X3) * C2              M22.7113
      X3 = BIN(2) * X4  -  BKN(2) * X3                                  M20.6977
      GO TO 25                                                          M16.7192
   20 X1=X2-X1                                                          M16.7193
CCCC  D2 = 50.                                                          VAX.4797
      IF (CABSZ(X1) .GT. PEKEXP )  GO TO 33                             VAX.4798
      X1=CEXPZ(X1)                                                      M18.1206
      ZCC= CJW * C1  / X2  *  ( BIN(1)*X4*X1 + BKN(1)*X3/X1 ) * C2      M22.7114
      X3 = BIN(2) * X4 * X1  -  BKN(2) * X3 / X1                        M20.6979
   25 ZCC = ZCC/X3                                                      M16.7198
      GO TO 40                                                          M16.7199
   30 CALL BSIKM ( X2, LL2, BIN, BKN, LL1, LL1 )                        M20.6980
   33 ZCC= CJW * C1  / X2  *  BIN(1) / BIN(2)  *  C2                    M22.7115
   40 IF ( IPRS47  .GE.  2 )                                            M23.6168
     1 WRITE (LOGSIX, 3688)  UR, CJW, ZCC                               M23.6169
 3688 FORMAT ( /,  17H EXIT  'SKIN47' .,  18X,  2HUR,                   M23.6170
     1  12X,  8HREAL-CJW,  12X,  8HIMAG-CJW,                            M23.6171
     2  12X,  8HREAL-ZCC,  12X,  8HIMAG-ZCC  ,/,  17X,  5E20.11  )      M23.6172
      RETURN                                                            M23.6173
      END                                                               M16.7203
      SUBROUTINE ZEGEN(BE1,BE2,TH,W,XE,ISYST)                           M16.7204
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMMON  / KOMTHL /  PEKEXP                                        VAX.4799
      COMPLEX*16  CJ, CJW, X1, X2, XE                                   M28.7527
      COMPLEX*16  CEXPZ, CMPLXZ, CSQRTZ                                 M28.7528
      COMPLEX*16  C1                                                    M22.7116
      UNITY = 1.0                                                       M22.7117
      C1 = CMPLXZ(U2P, FZERO)                                           M22.7118
      CJ = CIMAG1                                                       M22.7119
      EUC = 2./VALUE4                                                   M16.7207
      D1 = 2.0                                                          M20.6982
      SQ2 = SQRTZ(D1)                                                   M20.6983
      LL1 = 1                                                           M20.6984
      LL2 = 2                                                           M20.6985
      IF (ISYST .GE. 0)   GO TO 140                                     M16.7209
      CJW = CJ * CMPLXZ(W, FZERO)                                       M22.7120
      XE=CSQRTZ(CJW)                                                    M18.1209
      X1 = CMPLXZ(BE1, FZERO) * XE                                      M22.7121
      X2 = CMPLXZ(BE2, FZERO) * XE                                      M22.7122
      IXA=0                                                             M16.7214
      XA=CABSZ(X2)                                                      M18.1210
      IF(XA.GT.10.) IXA=1                                               M16.7216
      CALL BSIKM(X1,LL2,BIN,BKN,LL1,IXA)                                M20.6986
      XE=BKN(1)                                                         M20.6987
      IF(XA.GT.100.) GO TO 130                                          M16.7217
      CALL BSIKM(X2,LL2,BIN,BKN,LL1,IXA)                                M20.6988
      IF (IXA .GT. 0)   GO TO 120                                       M16.7221
  110 XE = CJW * C1 * (XE - BKN(1))                                     M22.7123
      GO TO 5                                                           M16.7223
  120 XE=CJW*C1 *(XE-BKN(1) /CEXPZ(X2-X1))/CEXPZ(X1)                    M28.7529
      GO TO 5                                                           M16.7227
  130 XA1=CABSZ(X1)                                                     M18.1213
      IF(XA1 .GT. PEKEXP ) GO TO 140                                    VAX.4800
      XE = CJW * C1 * XE/CEXPZ(X1)                                      M22.7125
      GO TO 5                                                           M16.7232
  140 XE = CZERO                                                        M22.7126
    5 E = BE2 * SQRTZ ( W )                                             M22.7127
      IF ( IPRS47  .GE.  2 )                                            M23.6174
     1 WRITE (LOGSIX, 3741)  ISYST, IXA, BE1, BE2, XA, TH, XA1, XE, W,E M23.6175
 3741 FORMAT ( /,  18H WITHIN  'ZEGEN' .,  16H   ISYST     IXA,         M23.6176
     1  16X,  3HBE1,  16X,  3HBE2,  17X,  2HXA,  17X,  2HTH,            M23.6177
     2  16X,  3HXA1  ,/,  18X,  2I8,  5E19.10  ,/,  1X,                 M23.6178
     3  13X,  7HREAL-XE,  13X,  7HIMAG-XE,                              M23.6179
     4  19X, 1HW, 19X, 1HE,/, 1X, 4E20.11 )                             M23.6180
      IF ( E  .GT.  5.)   GO TO 60                                      M22.7128
      R2 = E ** 4                                                       M16.7237
      R1=R2/16.                                                         M16.7238
      SN = E**2/8.                                                      M16.7239
      BN=R1/12.                                                         M16.7240
      CN = E/3.                                                         M16.7241
      DN=5./4.                                                          M16.7242
      EN = E**3/45.                                                     M16.7243
      FN=5./3.                                                          M16.7244
      ITER=11                                                           M16.7245
      IF(E.GE.1. ) ITER=21                                              M22.7129
      ITER = 21                                                         M16.7247
   15 DO 50 I=1,ITER                                                    M16.7248
      T=I-1                                                             M16.7249
      T1=T*2.                                                           M16.7250
      T2=T*4.                                                           M16.7251
      CS1=COSZ((T2+2.)*TH)                                              M18.1216
      SS1=SINZ((T2+2.)*TH)                                              M18.1217
      CS2=COSZ((T2+4.)*TH)                                              M18.1218
      SS2=SINZ((T2+4.)*TH)                                              M18.1219
      CS3=COSZ((T2+1.)*TH)                                              M18.1220
      CS4=COSZ((T2+3.)*TH)                                              M18.1221
      IF (I.GT. 1)   GO TO 30                                           M16.7258
      A1 = SN * CS1                                                     M16.7259
      A2=SN*SS1                                                         M16.7260
      A3=BN*CS2                                                         M16.7261
      A4=BN*SS2                                                         M16.7262
      B1=CN*CS3                                                         M16.7263
      B2=DN*A1                                                          M16.7264
      B3=EN*CS4                                                         M16.7265
      B4=FN*A3                                                          M16.7266
      EVENNN = A1 + A2 + A3 + A4 + B1 + B2 + B3 + B4                    M30.1368
      GO TO 50                                                          M16.7267
   30 T3=-T1*(T1+1.)**2*(T1+2.)                                         M16.7268
      T4=-(T1+1.)*(T1+2.)**2*(T1+3.)                                    M16.7269
      T5=-(T2-1.)*(T2+1.)**2*(T2+3.)                                    M16.7270
      T6=1./T2+1./(T1+1.)+1./(T1+2.)-1./(T2+4.)                         M16.7271
      T7=-(T2+1.)*(T2+3.)**2*(T2+5.)                                    M16.7272
      T8=1./(T2+2.)+1./(T1+2.)+1./(T1+3.)-1./(T2+6.)                    M16.7273
      SN=SN*R1/T3                                                       M16.7274
      BN=BN*R1/T4                                                       M16.7275
      CN=CN*R2/T5                                                       M16.7276
      DN=DN+T6                                                          M16.7277
      EN=EN*R2/T7                                                       M16.7278
      FN=FN+T8                                                          M16.7279
      A1=A1+SN*CS1                                                      M16.7280
      A2=A2+SN*SS1                                                      M16.7281
      A3=A3+BN*CS2                                                      M16.7282
      A4=A4+BN*SS2                                                      M16.7283
      B1=B1+CN*CS3                                                      M16.7284
      B2=B2+DN*SN*CS1                                                   M16.7285
      B3=B3+EN*CS4                                                      M16.7286
      B4=B4+FN*BN*CS2                                                   M16.7287
      VERBIN = EVENNN                                                   M30.1369
      EVENNN = A1 + A2 + A3 + A4 + B1 + B2 + B3 + B4                    M30.1370
      IF ((1.-VERBIN/EVENNN)*(1.-VERBIN/EVENNN) .LT. 1.E-12)            M30.1371
     1     GO TO 888                                                    M30.1372
   50 CONTINUE                                                          M16.7288
  888 CONTINUE                                                          M30.1373
      P1=PAI*(1.-A3)/4.+A1*ALOGZ(EUC/E) + TH*A2 + B2 + SQ2*(B3-B1)      M22.7130
      Q1=0.5+(1.-A3)*ALOGZ(EUC/E)-TH*A4-PAI*A1/4.-B4+SQ2*(B1+B3)        M22.7131
      GO TO 70                                                          M16.7291
   60 CS1=SQ2*COSZ(TH)                                                  M18.1224
      CS2=COSZ(2.*TH)*2.                                                M18.1225
      CS3=SQ2*COSZ(3.*TH)                                               M18.1226
      CS4=3.*SQ2*COSZ(5.*TH)                                            M18.1227
      P1=(CS1+(CS4/E**3+CS3/E-CS2)/E)/E                                 M16.7296
      Q1=(CS1+(CS4/E**2-CS3)/E**2)/E                                    M16.7297
   70 XE = CMPLXZ(W,FZERO) * C1 * (CMPLXZ(P1,FZERO) + CJ *              M22.7132
     1    CMPLXZ(Q1, FZERO)) + XE                                       M22.7133
      IF ( IPRS47  .GE.  2 )                                            M23.6181
     1 WRITE (LOGSIX, 3782)  P1, Q1, XE                                 M23.6182
 3782 FORMAT ( /,  16H EXIT  'ZEGEN' .,                                 M23.6183
     1  17X, 2HP1, 17X, 2HQ1,                                           M23.6184
     2  12X,  7HREAL-XE,  12X,  7HIMAG-XE  ,/,  16X,  4E19.10  )        M23.6185
      RETURN                                                            M16.7299
      END                                                               M16.7300
      SUBROUTINE EIGEN ( CJW, P, N, A, AI, QN, Q, XX, YY, LDN )         M35.9704
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMPLEX*16  AD, CJW, QI, S, SA                                    M28.7530
      COMPLEX*16  CMPLXZ, CSQRTZ                                        M28.7531
      COMPLEX*16  Q(LDN, LDN), XX(LDN, LDN), YY(LDN, LDN)               M35.9705
      COMPLEX*16  P(LDN, LDN), A(LDN, LDN), AI(LDN, LDN), QN(LDN)       M35.9706
      COMPLEX*16  C1, C2                                                M22.7134
C     EIGENVALUE CALCULATION SUBROUTINE.   'KVALUE'  =  ITERATION LIMIT.M16.7364
      KVALUE = 20                                                       M28.7532
      C1 = CJW/CMPLXZ(SPDLGT, FZERO)                                    M22.7136
      C2 = CMPLXZ(UNITY, FZERO)                                         VAX.4801
      QI = C2 / C1                                                      VAX.4802
      QI = QI * QI                                                      VAX.4803
      IF ( IPRS47  .GE.  1 )                                            M23.6186
     1 WRITE (LOGSIX, 3806)  N, CJW, QI                                 M23.6187
 3806 FORMAT ( /,  17H ENTER  'EIGEN' .,  8H       N,                   M23.6188
     1  12X,  8HREAL-CJW,  12X,  8HIMAG-CJW,                            M23.6189
     2  13X,  7HREAL-QI,   13X,  7HIMAG-QI  ,/,                         M23.6190
     3  17X,  I8,  4E20.11  ,/,  1X  )                                  M23.6191
      DO 5 I=1,N                                                        M16.7368
      DO 4 J=1,N                                                        M16.7369
    4 P(I,J)=P(I,J)*QI                                                  VAX.4804
    5 P(I, I) = P(I, I) - C2                                            M22.7139
      DO 10 I=1,N                                                       M16.7372
      DO 10 J=1,N                                                       M16.7373
   10 Q(I,J)=P(I,J)                                                     M16.7374
      L=0                                                               M16.7375
   15 L=L+1                                                             M16.7376
      IQ=0                                                              M16.7377
      IF(L-N) 20,90,90                                                  M16.7378
   20 IQ=IQ+1                                                           M16.7379
      IF(IQ.LE. KVALUE ) GO TO 25                                       M16.7380
      IQ=IQ-1                                                           M28.7533
      WRITE(LUNIT6,902) IQ                                              M28.7534
  902 FORMAT(5X,36HWARNING ; A HIGHER ACCURACY CAN'T BE,                M37.9124
     1          27H ACHIEVED BY THIS COMPUTER.,   /                     M37.9125
     2 5X,42HEIGEN VALUES   1 VECTORS AT ITTERATION IQ= ,               M28.7537
     3 I3,      12H IS ADOPTED.    )                                    M28.7538
      GO TO 63                                                          M28.7539
   25 DO 30 I=1,N                                                       M16.7386
      DO 30 J=1,N                                                       M16.7387
      XX(I,J) = Q(I,J)                                                  M16.7388
   30 YY(I,J) = Q(I,J)                                                  M16.7389
      DO 35 I=1,N                                                       M16.7390
      DO 35 J=1,N                                                       M16.7391
      Q(I,J)=CZERO                                                      M22.7140
      DO 35 K=1,N                                                       M16.7393
   35 Q(I,J) = Q(I,J) + XX(I,K) * YY(K,J)                               M16.7394
      DM=0.                                                             M16.7395
      DO 40 I=1,N                                                       M16.7396
      DX=CABSZ(Q(I,I))                                                  M18.1235
      IF(DX.LE.DM) GO TO 40                                             M16.7398
      DM=DX                                                             M16.7399
      IM=I                                                              M16.7400
   40 CONTINUE                                                          M16.7401
      QI=1./Q(IM,IM)                                                    M16.7402
      DO 45 I=1,N                                                       M16.7403
      DO 45 J=1,N                                                       M16.7404
   45 Q(I,J)=Q(I,J)*QI                                                  M16.7405
      DM=0.                                                             M16.7406
      DO 50 I=1,N                                                       M16.7407
      IF(I.EQ.IM) GO TO 50                                              M16.7408
      DX=CABSZ(Q(I,IM))                                                 M18.1236
      IF(DX.LE.DM) GO TO 50                                             M16.7410
      DM=DX                                                             M16.7411
      I1=I                                                              M16.7412
      I2=IM                                                             M16.7413
   50 CONTINUE                                                          M16.7414
      IF (DM .NE. 0.)   GO TO 60                                        M16.7415
      DO 59   I=1, N                                                    M16.7416
      IF(I.EQ.IM) GO TO 59                                              M16.7417
      DX=CABSZ(Q(IM,I))                                                 M18.1237
      IF(DX.LE.DM) GO TO 59                                             M16.7419
      DM=DX                                                             M16.7420
      I1=IM                                                             M16.7421
      I2=I                                                              M16.7422
   59 CONTINUE                                                          M16.7423
   60 SA = Q(IM, IM) / Q(I1, I2)                                        M20.6991
      S = XX(IM, IM) / XX(I1, I2)                                       M20.6992
      R = CABSZ(S / SA)                                                 M20.6993
      D1 = 50.* EPSILN                                                  M35.9707
C     NON-64-BIT COMPLEX MATH REDEFINES TOLERANCE IN "SYSDEP":          M35.9708
      IF ( ZNVREF .NE. 0.0 )  D1 = 50. * ZNVREF                         M35.9709
      D2 = R - UNITY                                                    M20.6994
      IF ( IPRS47  .GE.  31 )                                           M23.6192
     1 WRITE (LOGSIX, 3827)  L, IQ, KVALUE, D2, D1                      M23.6193
 3827 FORMAT (  42H DONE ANOTHER ITERATION.   L, IQ, KVALUE =,          M23.6194
     1  3I8,  10X,  8HD2, D1 =,  2E15.6  )                              M23.6195
      IF (ABSZ(D2) .GT. D1)   GO TO 20                                  M18.1240
   63 DO 65 I=1,N                                                       M28.7540
      A(I,L)=Q(I,IM)                                                    M16.7430
   65 AI(L,I)=Q(IM,I)                                                   M16.7431
      AD=CZERO                                                          M22.7142
      DO 70 I=1,N                                                       M16.7433
   70 AD=AD+P(IM,I)*Q(I,IM)                                             M16.7434
      QN(L)=AD/Q(IM,IM)                                                 M16.7435
      AD=CZERO                                                          M22.7143
      DO 75 I=1,N                                                       M16.7437
   75 AD=AD+AI(L,I)*A(I,L)                                              M16.7438
      AD=1./AD                                                          M16.7439
      DO 80 I=1,N                                                       M16.7440
   80 AI(L,I)=AD*AI(L,I)                                                M16.7441
      DO 85 I=1,N                                                       M16.7442
      DO 85 J=1,N                                                       M16.7443
      Q(I,J)=P(I,J)-A(I,L)*QN(L)*AI(L,J)                                M16.7444
   85 P(I,J)=Q(I,J)                                                     M16.7445
      IF ( IPRS47  .GE.  7 )                                            M23.6196
     1 WRITE (LOGSIX, 3842)  L, QN(L), AD                               M23.6197
 3842 FORMAT (  21H EIGENVALUE FINISHED.,  8H       L,                  M23.6198
     1  10X,  10HREAL-QN(L),  10X,  10HIMAG-QN(L),                      M23.6199
     2  13X,  7HREAL-AD,  13X,  7HIMAG-AD  ,/,                          M23.6200
     3  21X,  I8,  4E20.11  ,/,  1X  )                                  M23.6201
      GO TO 15                                                          M16.7446
   90 DM=0.                                                             M16.7447
      DO 95 I=1,N                                                       M16.7448
      DX=CABSZ(P(I,I))                                                  M18.1243
      IF(DX.LE.DM) GO TO 95                                             M16.7450
      DM=DX                                                             M16.7451
      IM=I                                                              M16.7452
   95 CONTINUE                                                          M16.7453
      AD=CZERO                                                          M22.7144
      DO 98 I=1,N                                                       M16.7455
      AD=AD+P(IM,I)*P(I,IM)                                             M16.7456
   98 A(I,L)=P(I,IM)/P(IM,IM)                                           M16.7457
      QN(L)=AD/P(IM,IM)                                                 M16.7458
      DO 100 I=1,N                                                      M16.7459
  100 AI(L,I)=P(IM,I)/QN(L)                                             M16.7460
      DO 110 I=1,N                                                      M16.7461
  110 QN(I) = C1 * CSQRTZ(C2 + QN(I))                                   M22.7145
 9200 IF ( IPRS47  .GE.  3 )                                            M23.6202
     1 WRITE (LOGSIX, 3854)  DM, SPDLGT, P(IM,IM), ( QN(I), I=1, N )    M23.6203
 3854 FORMAT ( /, 35H DONE ALL EIGENVALUES IN  'EIGEN' .,               M23.6204
     1  18X,  2HDM,  14X,  6HSPDLGT,  7X,  13HREAL-P(IM,IM),            M23.6205
     2  7X,  13HIMAG-P(IM,IM)  ,/,  35X,  4E20.11,   /,                 M23.6206
     3  49H COMPLEX EIGENVALUES  (QN(I), I=1, N)  FOLLOW ...  ,/,       M23.6207
     4  ( 1X,  6E20.11 )  )                                             M23.6208
      IF ( IPRS47  .GE.  1 )                                            M23.6209
     1 WRITE (LOGSIX, 3867)                                             M23.6210
 3867 FORMAT ( /,  88H DIAGNOSTIC UPON EXIT  'EIGEN' .   MATRIX OF EIGENM23.6211
     1VECTORS  A(I,L)  FOR  (I,L)=1, ... N .    )                       M23.6212
      LL0 = 0                                                           M23.6213
      IF ( IPRS47  .GE.  6 )                                            M23.6214
     1 CALL PRINT ( A(1,1), N, LL0, LDN )                               M35.9710
      RETURN                                                            M23.6216
      END                                                               M16.7468
      SUBROUTINE  ZEST ( H1, H2, E, RES, OMG, S )                       M16.7469
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMPLEX*16  QQ, BBB, ROM, S, SA, S1, S2                           M28.7541
      COMPLEX*16  S3, S5, S6, S8, SP12, SP23, SM12                      M28.7542
      COMPLEX*16  SM23, SQ1, SQ3, U, U1, U2, Z                          M28.7543
      COMPLEX*16  CEXPZ, CMPLXZ, CSQRTZ                                 M28.7544
      COMPLEX*16  C1, C2, C3, C4, C5, C6, C7, C8                        M22.7147
      TOJ = U0                                                          M23.6217
      IF ( IPRS47  .GE.  2 )                                            M23.6218
     1 WRITE (LOGSIX, 3917)  H1, H2, E, RES, OMG                        M23.6219
 3917 FORMAT ( /,  16H ENTER  'ZEST' .,  18X,  2HH1,  18X,  2HH2,       M23.6220
     1  19X,  1HE,  17X,  3HRES,  17X,  3HOMG  ,/,  16X,  5E20.11  )    M23.6221
      HKR = SPDLGT**2                                                   M16.7472
      YUD=1./TOJ/HKR                                                    M16.7473
      H1PH2=H1+H2                                                       M16.7474
      AB = 10. * E/H1PH2                                                M16.7475
      OMG2=OMG*OMG                                                      M16.7476
      D1 = TOJ * OMG/RES                                                M22.7148
      ROM = CMPLXZ(FZERO, D1)                                           M22.7149
      BP=PAI/2.                                                         M16.7478
      RMAX=0.                                                           M16.7479
      S = CZERO                                                         M22.7150
      L=2                                                               M16.7481
      RAM=0.                                                            M16.7482
  700 RAM2=RAM*RAM                                                      M16.7483
      D2 = RAM2 + OMG2 * (1. - HTOJ2 * HYUD2)/HKR                       M22.7151
      C1 = CMPLXZ(D2, FZERO)                                            M22.7152
      C2 = ROM * CMPLXZ(HTOJ2, FZERO)                                   M22.7153
      S1 = CSQRTZ(C1 + C2)                                              M22.7154
      D3 = RAM2 + OMG2 * (1. - HTOJ3 * HYUD3)/HKR                       M22.7155
      C3 = CMPLXZ(D3, FZERO)                                            M22.7156
      C4 = CMPLXZ(ALF1, FZERO) * ROM * CMPLXZ(HTOJ3, FZERO)             M22.7157
      S2 = CSQRTZ(C3 + C4)                                              M22.7158
      D4 = RAM2 + OMG2 * (1. - HTOJ4 * HYUD4)/HKR                       M22.7159
      C5 = CMPLXZ(D4, FZERO)                                            M22.7160
      C6 = CMPLXZ(ALF2, FZERO) * ROM * CMPLXZ(HTOJ4, FZERO)             M22.7161
      S3 = CSQRTZ(C5 + C6)                                              M22.7162
      S6=S2                                                             M16.7487
      S5=S1                                                             M16.7488
      D5 = 1./HTOJ2/TOJ                                                 M22.7163
      S1 = S1 * CMPLXZ(D5, FZERO)                                       M22.7164
      D6 = 1./HTOJ3/TOJ                                                 M22.7165
      S2 = S2 * CMPLXZ(D6, FZERO)                                       M22.7166
      D7 = 1./HTOJ4/TOJ                                                 M22.7167
      S3 = S3 * CMPLXZ(D7, FZERO)                                       M22.7168
      S8 = CMPLXZ(TOJ, FZERO) * S1                                      M22.7169
      SP12=S1+S2                                                        M16.7493
      SP23=S2+S3                                                        M16.7494
      SM12=S1-S2                                                        M16.7495
      D8 = 2.*( DEP1 - DEP2)                                            M22.7170
      C7 = CMPLXZ(D8,FZERO)                                             M22.7171
      SM23 = (S2 - S3) * CEXPZ(S6 * C7)                                 M22.7172
      D9 = -2. * DEP1                                                   M22.7173
      C8 = CMPLXZ(D9, FZERO)                                            M22.7174
      BBB = CEXPZ(S5 * C8)                                              M22.7175
      D10 = EXPZ(-H1PH2 * RAM) * COSZ(E * RAM)                          M22.7176
      CONTWO = 2.0                                                      M22.7177
      Z=CMPLXZ(FZERO,CONTWO )*(SP12*SP23+SM12*SM23+BBB*(SM12*SP23+SP12  M22.7178
     1 * SM23)) *                                                       M22.7179
     1 CMPLXZ(D10, FZERO)/((CMPLXZ(RAM, FZERO) +S8) *(SP12*SP23+SM12*   M22.7180
     1SM23)                                                             M22.7181
     2 + (CMPLXZ(RAM,FZERO) - S8) * BBB*(SM12*SP23+SP12*SM23))          M22.7182
      IF ( IPRS47  .GE.  5 )                                            M23.6222
     1 WRITE (LOGSIX, 3928)  L, BP, AB, RAM, RMAX, Z                    M23.6223
 3928 FORMAT ( /,  1X,  8H       L,  18X,  2HBP,  18X,  2HAB,           M23.6224
     1  17X,  3HRAM,  16X,  4HRMAX,  14X,  6HREAL-Z,  14X,  6HIMAG-Z    M23.6225
     2  ,/,  1X,  I8,  6E20.11  )                                       M23.6226
      GO TO (702,701),L                                                 M16.7501
  701 U1=Z                                                              M16.7502
      JD=50                                                             M16.7503
      GO TO 650                                                         M16.7504
  600 U1 = CZERO                                                        M22.7183
  650 IF (BP .LT. AB)   GO TO 300                                       M16.7506
      JD = 100                                                          M16.7507
      RMIN=RMAX                                                         M16.7508
      RMAX=10./H1PH2                                                    M16.7509
      L=1                                                               M16.7510
      RAM=RMAX                                                          M16.7511
      GO TO 700                                                         M16.7512
  702 U2=Z                                                              M16.7513
      GO TO 210                                                         M16.7514
  300 RMIN=RMAX                                                         M16.7515
      RMAX=BP/E                                                         M16.7516
      BP=BP+PAI                                                         M16.7517
      U2 = CZERO                                                        M22.7184
  210 JJJ=1                                                             M16.7519
      QQ=U1+U2                                                          M16.7520
      N=123                                                             M16.7521
      DX=RMAX-RMIN                                                      M16.7522
      D11 = DX/2.                                                       M22.7185
      SQ1 = QQ * CMPLXZ(D11, FZERO)                                     M22.7186
      IF ( IPRS47  .GE.  6 )                                            M23.6227
     1 WRITE (LOGSIX, 3943)  DX, HTOJ2, RMIN, OMG2, TOJ, VALUE5, QQ     M23.6228
 3943 FORMAT ( /,  1X,  14X,  2HDX,  11X,  5HHTOJ2,  12X,  4HRMIN,      M23.6229
     1  12X,  4HOMG2,  13X,  3HTOJ,  10X,  6HVALUE5,  9X,  7HREAL-QQ,   M23.6230
     2  9X,  7HIMAG-QQ  ,/,  1X,  8E16.7  )                             M23.6231
   75 DX2=DX                                                            M16.7524
      DX=DX/2.0                                                         M16.7525
      RAM=RMIN+DX                                                       M16.7526
      N=2*N                                                             M16.7527
      IF(JJJ.EQ.1) N=1                                                  M16.7528
      JJJ=100                                                           M16.7529
      DO 77 KN=1,N                                                      M16.7530
      RAM2=RAM*RAM                                                      M16.7531
      D2 = RAM2 + OMG2 * (1. - HTOJ2 * HYUD2)/HKR                       M22.7187
      C1 = CMPLXZ(D2, FZERO)                                            M22.7188
      C2 = ROM * CMPLXZ(HTOJ2, FZERO)                                   M22.7189
      S1 = CSQRTZ(C1 + C2)                                              M22.7190
      D3 = RAM2 + OMG2 * (1. - HTOJ3 * HYUD3)/HKR                       M22.7191
      C3 = CMPLXZ(D3, FZERO)                                            M22.7192
      C4 = CMPLXZ(ALF1, FZERO) * ROM * CMPLXZ(HTOJ3, FZERO)             M22.7193
      S2 = CSQRTZ(C3 + C4)                                              M22.7194
      D4 = RAM2 + OMG2 * (1. - HTOJ4 * HYUD4)/HKR                       M22.7195
      C5 = CMPLXZ(D4, FZERO)                                            M22.7196
      C6 = CMPLXZ(ALF2, FZERO) * ROM * CMPLXZ(HTOJ4, FZERO)             M22.7197
      S3 = CSQRTZ(C5 + C6)                                              M22.7198
      S6=S2                                                             M16.7535
      S5=S1                                                             M16.7536
      D5 = 1./HTOJ2/TOJ                                                 M22.7199
      S1 = S1 * CMPLXZ(D5, FZERO)                                       M22.7200
      D6 = 1./HTOJ3/TOJ                                                 M22.7201
      S2 = S2 * CMPLXZ(D6, FZERO)                                       M22.7202
      D7 = 1./HTOJ4/TOJ                                                 M22.7203
      S3 = S3 * CMPLXZ(D7, FZERO)                                       M22.7204
      S8 = CMPLXZ(TOJ, FZERO) * S1                                      M22.7205
      SP12=S1+S2                                                        M16.7541
      SP23=S2+S3                                                        M16.7542
      SM12=S1-S2                                                        M16.7543
      D8 = 2.*( DEP1 - DEP2)                                            M22.7206
      C7 = CMPLXZ(D8,FZERO)                                             M22.7207
      SM23 = (S2 - S3) * CEXPZ(S6 * C7)                                 M22.7208
      D9 = -2. * DEP1                                                   M22.7209
      C8 = CMPLXZ(D9, FZERO)                                            M22.7210
      BBB = CEXPZ(S5 * C8)                                              M22.7211
      D10 = EXPZ(-H1PH2 * RAM) * COSZ(E * RAM)                          M22.7212
      Z=CMPLXZ(FZERO,CONTWO)*(SP12*SP23+SM12*SM23+BBB*(SM12*SP23+SP12*  M22.7213
     1 SM23))*                                                          M22.7214
     1 CMPLXZ(D10, FZERO)/((CMPLXZ(RAM, FZERO) +S8) *(SP12*SP23+SM12*   M22.7215
     1SM23)                                                             M22.7216
     2 + (CMPLXZ(RAM,FZERO) - S8) * BBB*(SM12*SP23+SP12*SM23))          M22.7217
      RAM=RAM+DX2                                                       M16.7549
      U=2.*Z                                                            M16.7550
   77 QQ=QQ+U                                                           M16.7551
      D12 = DX/2.                                                       M22.7218
      SQ3 = QQ * CMPLXZ(D12, FZERO)                                     M22.7219
      SA = SQ1 - SQ3                                                    M16.7553
      R = CABSZ(SA)/CABSZ(SQ3)                                          M18.1266
      V5 = VALUE5 * .5                                                  M16.7555
      IF ( IPRS47  .GE.  24 )                                           M23.6232
     1 WRITE (LOGSIX, 3956)  N, R, DX, RAM, QQ                          M23.6233
 3956 FORMAT ( /,  13H BOTTOM LOOP.,  8H       N,  19X,  1HR,           M23.6234
     1  18X,  2HDX,  17X,  3HRAM,  13X,  7HREAL-QQ,  13X,  7HIMAG-QQ    M23.6235
     2  ,/,  13X,  I8,  5E20.11  )                                      M23.6236
      IF ( R .LE. V5)   GO TO 50                                        M16.7556
      SQ1 = SQ3                                                         M16.7557
      GO TO 75                                                          M16.7558
   50 S = S + SQ3                                                       M16.7559
      IF(JD.EQ.100) GO TO 55                                            M16.7560
      GO TO 600                                                         M16.7561
   55 C9 = .5 * U0/ PAI                                                 M23.6237
      S = S * CMPLXZ(OMG, FZERO) * CMPLXZ(C9, FZERO)                    M23.6238
      IF ( IPRS47  .GE.  2 )                                            M23.6239
     1 WRITE (LOGSIX, 3968)  OMG, VALUE2, S                             M23.6240
 3968 FORMAT ( /,  15H EXIT  'ZEST' .,  17X,  3HOMG,  14X,  6HVALUE2,   M23.6241
     1  14X,  6HREAL-S,  14X,  6HIMAG-S  ,/,  15X,  4E20.11  )          M23.6242
      RETURN                                                            M16.7563
      END                                                               M16.7564
      SUBROUTINE MINV ( TCMPX, M, F, LDN, LDN2 )                        M35.9711
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      COMPLEX*16  AD, CC, D                                             M28.7545
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMPLEX*16  TCMPX(LDN,LDN), F(LDN, LDN2),  D2,  CMPLXZ, FNEW      M43.4066
      COMPLEX*16  FIDENT                                                M43.4067
      DIMENSION  FR(10,20), FI(10,20), FNEW(10,20), FIDENT(10,20)       M43.4068
      D1 = 0.0                                                          M16.7568
      DO 4 I=1,M                                                        M16.7569
      DO 4 J=1,M                                                        M16.7570
      D2 = TCMPX(I,J)                                                   M18.1270
      IF ( CABSZ ( D2 ) .GT. D1)   D1 = CABSZ(D2)                       M18.1271
      FR(I,J) = REALZ ( D2 )                                            M43.4069
    4 FI(I,J) = AIMAGZ ( D2 )                                           M43.4070
CCCC    4 F(I,J) = D2                                                   M16.7574
      D1 = D1 * VALUE2                                                  M23.6243
      IF ( IPRS47  .GE.  1 )                                            M23.6244
     1 WRITE(LOGSIX, 4005) M, VALUE2, D1, F(1, 1)                       M23.6245
 4005 FORMAT ( /,  16H START  'MINV' .,  8H       M,  14X,  6HVALUE6,   M23.6246
     1  18X,  2HD1,  9X,  11HREAL-F(1,1),  9X,  11HIMAG-F(1,1)  ,/,     M23.6247
     2  16X,  I8,  4E20.11  ,/,                                         M23.6248
     3  47H DIAGNOSTIC   TCMPX(I,J)  FOR  (I,J)=1, ... M .    )         M23.6249
      LL0 = 0                                                           M23.6250
      IF ( IPRS47  .GE.  7 )                                            M23.6251
     1 CALL PRINT ( TCMPX(1,1), M, LL0, LDN )                           M35.9713
      DO 30   I=1, M                                                    M16.7575
      J1=M+1                                                            M16.7576
      M1=M*2                                                            M16.7577
      DO 25   J = J1, M1                                                M16.7578
      IF ((J-M) .EQ. I)   GO TO 20                                      M16.7579
CCCC     F(I,J) = CZERO                                                 M27.3835
      FR(I,J) = 0.0                                                     M43.4071
      FI(I,J) = 0.0                                                     M43.4072
      GO TO 25                                                          M16.7581
CCCC  20 F(I,J) = CREAL1                                                M16.7582
   20 FR(I,J) = 1.0                                                     M43.4073
      FI(I,J) = 0.0                                                     M43.4074
   25 CONTINUE                                                          M16.7583
   30 CONTINUE                                                          M16.7584
      DO 115  K1 =1, M                                                  M16.7585
      D9 = 0.0                                                          M16.7586
      N=K1                                                              M16.7587
      DO 50   I2 = K1, M                                                M16.7588
CCCC     BX=CABSZ(F(I2,K1))                                             M18.1274
CCCC     IF (BX .LE. D9)   GO TO 50                                     M16.7590
      D18 = ABSZ ( FR(I2,K1) )                                          M43.4075
      D19 = ABSZ ( FI(I2,K1) )                                          M43.4076
      IF ( D18 .LT. D19 ) D18 = D19                                     M43.4077
      IF ( D18 .LE. D9 )  GO TO 50                                      M43.4078
      D9 = D18                                                          M16.7591
      N=I2                                                              M16.7592
   50 CONTINUE                                                          M16.7593
C      write (*,*) ' Next variable.  K1, N, D9, D1 =',                  M43.4079
C     1                              K1, N, D9, D1                      M43.4080
      IF ( D9 .GT. D1 ) GO TO 60                                        M43.4081
      WRITE (LUNIT6, 5706)  M, K1, D9                                   M17.1472
 5706 FORMAT ( /, 106H STOP. ---- MATRIX INVERSION WITHIN SUBROUTINE  'MM43.4082
     1INV'  HAS BEEN SUSPENDED, DUE TO FAILURE TO FIND A LARGE     ,/,  M16.7597
     2        12X,  58HENOUGH PIVOT ELEMENT.   THE MATRIX IN QUESTION ISM16.7598
     3 OF ORDER,  I5,  36H ,    WITH BREAKDOWN HAVING OCCURRED     ,/,  M16.7599
     4       12X,  51HWHILE WORKING ON THE ELIMINATION OF VARIABLE NUMBEM16.7600
     5R,  I5,   '.   In this columns, the largest real or'       ,/,    M16.7601
     6       12X,  ' imaginary part had absolute value',                M16.7602
     7 E14.3,  45H ,   WHILE THE APPLICABLE NEAR-ZERO TOLERANCE     )   M16.7603
      WRITE(LUNIT6, 5707) D1, VALUE2                                    M23.6253
 5707 FORMAT (12X,  11HIS EQUAL TO,   E14.3,   36H .    THIS LATTER NUMBM16.7605
     1ER IS EQUAL TO,   E14.3,   20H   TIMES THE LARGEST           ,/,  M16.7606
     2       12X,  67HELEMENT OF THE ORIGINAL INPUT MATRIX (CONSIDERING M16.7607
     3ABSOLUTE VALUES).     )                                           M16.7608
      STOP                                                              M43.4083
   60 IF (N .EQ. K1)   GO TO 75                                         M16.7610
      DO 70   J = K1, M1                                                M16.7611
CCCC     CC=F(K1,J)                                                     M16.7612
      CCR = FR(K1,J)                                                    M43.4084
      CCI = FI(K1,J)                                                    M43.4085
CCCC     F(K1,J)=F(N,J)                                                 M16.7613
      FR(K1,J) = FR(N,J)                                                M43.4086
      FI(K1,J) = FI(N,J)                                                M43.4087
CCCC   70 F(N,J)=CC                                                     M16.7614
      FR(N,J) = CCR                                                     M43.4088
   70 FI(N,J) = CCI                                                     M43.4089
   75 DO 100 I=1,M                                                      M16.7615
      IF (I.EQ. K1)   GO TO 100                                         M16.7616
CCCC     D = F(I,K1)                                                    M16.7617
      DR = FR(I,K1)                                                     M43.4090
      DI = FI(I,K1)                                                     M43.4091
      DO 95   J=K1, M1                                                  M16.7618
CCCC  F(I,J) = F(I,J) - D/F(K1,K1) * F(K1,J)                            M16.7619
      D14 = FR(K1,K1)**2 + FI(K1,K1)**2                                 M43.4092
      D22 = DR * FR(K1,K1) + DI * FI(K1,K1)                             M43.4093
      D23 = DI * FR(K1,K1) - DR * FI(K1,K1)                             M43.4094
      D5 = D22 * FR(K1,J) - D23 * FI(K1,J)                              M43.4095
      D6 = D22 * FI(K1,J) + D23 * FR(K1,J)                              M43.4096
      FR(I,J) = FR(I,J) - D5 / D14                                      M43.4097
      FI(I,J) = FI(I,J) - D6 / D14                                      M43.4098
C      write (*,*) ' Revise F(I,J).  I, J, K1, F(I,J) =',               M43.4099
C     1                              I, J, K1, FR(I,J), FI(I,J)         M43.4100
   95 CONTINUE                                                          M16.7620
  100 CONTINUE                                                          M16.7621
CCCC     AD = F(K1, K1)                                                 M16.7622
      ADR = FR(K1,K1)                                                   M43.4101
      ADI = FI(K1,K1)                                                   M43.4102
      D14 = ADR**2 + ADI**2                                             M43.4103
      ADR = ADR / D14                                                   M43.4104
      ADI = -ADI / D14                                                  M43.4105
      DO 110  J=K1, M1                                                  M16.7623
CCCC 110 F(K1,J) = F(K1,J)/AD                                           M16.7624
      D16  =     FR(K1,J) * ADR - FI(K1,J) * ADI                        M43.4106
      FI(K1,J) = FR(K1,J) * ADI + FI(K1,J) * ADR                        M43.4107
  110    FR(K1,J) = D16                                                 M43.4108
C  110 write (*,*) ' New  F(K1,J).   K1, J, FR(K1,J), FI(K1,J) =',      M43.4109
C     1                              K1, J, FR(K1,J), FI(K1,J)          M43.4110
  115 CONTINUE                                                          M16.7625
      DO 325  I = 1, M                                                  M43.4111
      DO 325 J=1,M                                                      M43.4112
      J1=J+M                                                            M43.4113
  325 FNEW(I,J) = CMPLXZ ( FR(I,J1), FI(I,J1) )                         M43.4114
      DO 625  I = 1, M                                                  M43.4115
      DO 625 J=1, M                                                     M43.4116
      FIDENT(I,J) = CZERO                                               M43.4117
      DO 288  K=1, M                                                    M43.4118
  288 FIDENT(I,J) = FIDENT(I,J) + TCMPX(I,K) * FNEW(k,j)                M43.4119
  625 CONTINUE                                                          M43.4120
C      WRITE (*,*) ' Minv, [A]*[A]-1  follows.   M =',  M               M43.4121
C      DO 725  I = 1, M                                                 M43.4122
C  725 write (*,*) ' Row', I, ( fident(i,j), j=1, m )                   M43.4123
      DO 125  I = 1, M                                                  M16.7626
      DO 125 J=1,M                                                      M16.7627
      J1=J+M                                                            M16.7628
CCCC 125 TCMPX(I,J) = F(I,J1)                                           M18.1276
  125 TCMPX(I,J) = CMPLXZ ( FR(I,J1), FI(I,J1) )                        M43.4124
C  125 write (*,*) ' Transfer.',                                        M43.4125
C     1  ' I, J, J1, FR(I,J1), FI(I,J1), TCMPX(I,J) =',                 M43.4126
C     2    I, J, J1, FR(I,J1), FI(I,J1), TCMPX(I,J)                     M43.4127
      IF ( IPRS47  .GE.  1 )                                            M23.6254
     1 WRITE (LOGSIX, 4027)  TCMPX(1,1), TCMPX(1,2)                     M23.6255
 4027 FORMAT ( /,  24H EXIT  'MINV'  NORMALLY.,                         M23.6256
     1  5X,  15HREAL-TCMPX(1,1),  5X,  15HIMAG-TCMPX(1,1),              M23.6257
     2  5X,  15HREAL-TCMPX(1,2),  5X,  15HIMAG-TCMPX(1,2)  ,/,          M23.6258
     3  24X,  4E20.11  ,/,                                              M23.6259
     4  55H DIAGNOSTIC INVERSE.   TCMPX(I,J)  FOR  (I,J)=1, ... M.  )   M23.6260
      IF ( IPRS47  .GE.  7 )                                            M23.6261
     1 CALL PRINT ( TCMPX(1,1), M, LL0, LDN )                           M35.9714
  150 RETURN                                                            M16.7630
      END                                                               M16.7631
      SUBROUTINE MXM(XM,YYM,C,N,LDN)                                    M35.9715
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMPLEX*16  XM(LDN,LDN), YYM(LDN,LDN), C(LDN,LDN)                 M35.9716
      IF ( IPRS47  .GE.  1 )                                            M23.6263
     1 WRITE (LOGSIX, 4062)  N, XM(1,1), YYM(1,1)                       M23.6264
 4062 FORMAT ( /,  15H ENTER  'MXM' .,  8H       N,                     M23.6265
     1  7X,  13HREAL- XM(1,1),  7X,  13HIMAG- XM(1,1),                  M23.6266
     2  7X,  13HREAL-YYM(1,1),  7X,  13HIMAG-YYM(1,1)  ,/,              M23.6267
     3  15X,  I8,  4E20.11  ,/,                                         M23.6268
     4  57H DIAGNOSTIC LEFT FACTOR.   XM(I,J)  FOR  (I,J)=1, ... N .  ) M23.6269
      IF ( IPRS47  .LT.  8 )   GO TO 4079                               M23.6270
      LL0 = 0                                                           M23.6271
      CALL PRINT ( XM(1,1), N, LL0, LDN )                               M35.9717
      WRITE (LOGSIX, 4073)                                              M23.6273
 4073 FORMAT ( /,                                                       M23.6274
     1  58H DIAGNOSTIC RIGHT FACTOR.  YYM(I,J)  FOR  (I,J)=1, ... N . ) M23.6275
      CALL PRINT ( YYM(1,1), N, LL0, LDN )                              M35.9718
 4079 DO 10  I=1, N                                                     M23.6277
      DO 10 J=1,N                                                       M16.7636
      C(I,J)=CZERO                                                      M22.7221
      DO 10 K=1,N                                                       M16.7638
   10 C(I,J) = C(I,J) + XM(I,K) * YYM(K,J)                              M20.6997
      IF ( IPRS47  .GE.  1 )                                            M23.6278
     1 WRITE (LOGSIX, 4091)  C(1,1), C(1,2)                             M23.6279
 4091 FORMAT ( /,  14H EXIT  'MXM' .,                                   M23.6280
     1  9X,  11HREAL-C(1,1),  9X,  11HIMAG-C(1,1),                      M23.6281
     2  9X,  11HREAL-C(1,2),  9X,  11HIMAG-C(1,2)  ,/,                  M23.6282
     3  14X,  4E20.11  ,/,                                              M23.6283
     4  52H DIAGNOSTIC PRODUCT.   C(I,J)  FOR  (I,J)=1, ... N .   )     M23.6284
      IF ( IPRS47  .GE.  8 )                                            M23.6285
     1 CALL PRINT ( C(1,1), N, LL0, LDN )                               M35.9719
      RETURN                                                            M16.7640
      END                                                               M16.7641
      SUBROUTINE PRINT(C,N,IFORM,LDN)                                   M35.9720
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL47.FOR;/NOLIST'                                           
      COMPLEX*16  C(LDN,LDN)                                            M35.9721
      DIMENSION  WORKR(8), WORKI(8)                                     M16.7645
      REAL*8          TEXT1, TEXT2, TEXT3                               M16.7646
      DATA  TEXT1   /  3HROW  /                                         M16.7647
      DATA  TEXT2   /  3H     /                                         M16.7648
      NLINE = ( N + 7 ) / 8                                             M16.7649
      DO 4695  I=1, N                                                   M16.7650
      TEXT3 = TEXT2                                                     M16.7651
      IM = -7                                                           M16.7652
      DO 4681  K=1, NLINE                                               M16.7653
      IM = IM + 8                                                       M16.7654
      IN = IM + 7                                                       M16.7655
      IF ( IN  .GT.  N )   IN = N                                       M16.7656
      L = 0                                                             M16.7657
      DO 4617  J=IM, IN                                                 M16.7658
      L = L + 1                                                         M16.7659
      WORKR(L) = REALZ( C(I,J) )                                        M18.1279
 4617 WORKI(L) = AIMAGZ( C(I,J) )                                       M18.1280
      IF ( K  .EQ.  NLINE )                                             M16.7662
     1 TEXT3 = TEXT1                                                    M16.7663
      IF ( IFORM  .EQ.  1 )   GO TO 4658                                M16.7664
      WRITE (LUNIT6, 4649)  TEXT3,  ( WORKR(M), M=1, L )                M17.1474
 4649 FORMAT ( 1X, A3, 8E16.7  )                                        M16.7666
      WRITE (LUNIT6, 4651)  ( WORKI(M), M=1, L )                        M17.1475
 4651 FORMAT ( 4X, 8E16.7 )                                             M16.7668
      GO TO 4681                                                        M16.7669
 4658 WRITE (LUNIT6, 4659)  TEXT3,  ( WORKR(M), M=1, L )                M17.1476
 4659 FORMAT ( 1X, A3, 8F16.7 )                                         M16.7671
      WRITE (LUNIT6, 4661)  ( WORKI(M), M=1, L )                        M17.1477
 4661 FORMAT ( 4X, 8F16.7  )                                            M16.7673
 4681 CONTINUE                                                          M16.7674
 4695 WRITE (LUNIT6, 4632)  I                                           M17.1478
 4632 FORMAT ( 1H+, I2, /, 1X )                                         M16.7676
      RETURN                                                            M16.7677
      END                                                               M16.7678
