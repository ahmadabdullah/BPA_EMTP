      SUBROUTINE SUBR10                                                 M22. 732
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
C %INCLUDE  '//c/tsu/cables.INS.FTN'                                    M43.   3
      KTAB = 0                                                          M13. 179
 3000 IF ( NCHAIN  .GT.  20 )   GO TO 9000                              M22. 734
      IF (KILL .EQ. 0) GO TO 1679                                       M22. 735
      NCHAIN = 51                                                       M22. 736
      GO TO 3000                                                        M22. 737
 1679 N24 = NCHAIN                                                      M36. 255
      IF ( N24 .LT. 1 )  N24 = 1                                        M36. 256
      IPRSUP = IPRSOV ( N24 )                                           M36. 257
      IPRCBL = IPRSUP                                                           
      IF ( M4PLOT .EQ. 1 )  CALL EMTSPY                                         
      IF ( NCHAIN .EQ. 0 )  NCHAIN = 1                                          
      GO TO  (3001, 3002, 3003, 3004, 3005, 3006, 3007, 3008, 3009,     M42. 805
     1 3010, 3011, 3012, 3013, 3014, 3015, 3016, 3016, 3016, 3016,      M42. 806
     2 3020),  NCHAIN                                                   M42. 807
 3001 CALL OVER1 
      GO TO 3000                                                        M42. 809
 3002 CALL OVER2 
      GO TO 3000                                                        M42. 811
 3003 CALL STOPTP                                                               
      GO TO 3000                                                        M42. 812
 3004 CALL STOPTP                                                               
      GO TO 3000                                                        M42. 813
 3005 CALL OVER5 
      GO TO 3000                                                        M42. 815
 3006 CALL OVER6 
      GO TO 3000                                                        M42. 817
 3007 CALL OVER7 
      GO TO 3000                                                        M42. 819
 3008 CALL OVER8 
      GO TO 3000                                                        M42. 821
 3009 CALL OVER9 
      GO TO 3000                                                        M42. 823
 3010 CALL OVER10
      GO TO 3000                                                        M42. 825
 3011 CALL OVER11
      GO TO 3000                                                        M42. 827
 3012 CALL OVER12
      GO TO 3000                                                        M42. 829
 3013 CALL OVER13
      GO TO 3000                                                        M42. 831
 3014 CALL OVER14
      GO TO 3000                                                        M42. 833
 3015 CALL OVER15
      GO TO 3000                                                        M42. 835
 3016 CALL OVER16
      GO TO 3000                                                        M42. 837
 3020 CALL OVER20
      GO TO 3000                                                        M42. 839
C)                                                                      M15. 341
C     THE EMTP SOLUTION PROCESS PROPER CONSISTS OF NUMEROUS SECONDARY-  M28. 283
C     LEVEL OVERLAYS, EACH CALLED FROM MODULE  MAIN10  USING            M42. 840
C     MODULE NUMBER  'NCHAIN'  AS THE INDICATOR OF WHERE CONTROL IS TO  M42. 841
C     PASS NEXT.   THE FOLLOWING IS A TABULATION OF VALID  'NCHAIN'     M42. 842
C     VALUES, ALONG WITH THE FUNCTION OF THE CORRESPONDING CODE.   WITH M42. 843
C     THE POSSIBLE EXCEPTION OF THE TIME-STEP LOOP,  'NCHAIN'  IS THE   M42. 844
C     UTPF  OVERLAY NUMBER OF THE CODE TO WHICH CONTROL IS TO BE        M42. 845
C     TRANSFERED NEXT.   THE OVERLAY NUMBER OF THE TIME-STEP LOOP       M42. 846
C     IS GIVEN BY  'NCHAIN'  CORRESPONDING TO THE FIRST QUARTER OF      M42. 847
C     THE TIME-STEP-LOOP SEGMENTATION, ON MACHINES NOT USING ECS        M42. 848
C     OVERLAYING.                                                       M42. 849
C                                                                       M42. 850
C     1.  INPUT OF MISCELLANEOUS DATA CARDS.                            M42. 851
C     2.  INPUT BRANCH DATA.                                            M42. 852
C     3.  CODE ASSOCIATED WITH THE CASCADING OF PI-CIRCUITS FOR         M42. 853
C         STEADY-STATE PHASOR SOLUTIONS ONLY.                           M42. 854
C     4.  INPUT AND PROCESSING OF FREQUENCY-DEPENDENCE DATA FOR         M42. 855
C         DISTRIBUTED-PARAMETER LINE MODE.                              M42. 856
C     5.  INPUT OF SWITCH AND SOURCE DATA CARDS.                        M42. 857
C     6.  NETWORK CONNECTIVITY OUTPUT.   SETUP OF TRANSIENT-NETWORK     M42. 858
C         RENUMBERING TABLES.                                           M42. 859
C     7.  NETWORK NODE RENUMBERING ROUTINE (JOHN WALKER'S OLD           M42. 860
C         SUBROUTINE NUMBER).                                           M42. 861
C     8.  CONVERT TABLES TO NEW NODE NUMBERS.   FIND STEADY-STATE       M42. 862
C         PHASOR EQUIVALENTS FOR DISTRIBUTED BRANCHES.                  M42. 863
C     9.  SET UP STEADY-STATE PHASOR NETWORK RENUMBERING TABLES.        M42. 864
C     10.  FORM THE STEADY-STATE ADMITTANCE MATRIX (Y).   SOLVE FOR     M42. 865
C          STEADY-STATE PHASOR VOLTAGES.                                M42. 866
C     11.  OUTPUT STEADY-STATE PHASOR SOLUTION (IF REQUESTED).          M42. 867
C     12.  BRANCH-TABLE AND SWITCH-TABLE PROCESSING, AS PREPARATION     M42. 868
C          FOR THE INTEGRATION IN TIME-STEP LOOP.                       M42. 869
C     13.  SETUP INITIAL CONDITIONS ON LUMPED ELEMENTS, AND PAST        M42. 870
C          HISTORY FOR DISTRIBUTED LINES.                               M42. 871
C     14.  FORM (Y) FOR THE TRANSIENTS NETWORK.   TRIANGULARIZE THE     M42. 872
C          FIRST PARTITION (NONSWITCH/SOURCE NODES).                    M42. 873
C     15.  FINAL SETUP OPERATIONS BEFORE TIME-STEP LOOP.                M42. 874
C     16.  FIRST QUARTER OF TIME-STEP LOOP (CHECKING FOR CHANGES OF     M42. 875
C          SWITCHES AND PSEUDO-NONLINEAR ELEMENTS, RETRIANGULARIZATION  M42. 876
C          OF  YBB  AND CALCULATION OF THEVENIN IMPEDANCE VECTORS).     M42. 877
C     17.  SECOND QUARTER OF TIME-STEP LOOP (BRANCH-TABLE HISTORY       M42. 878
C          UPDATING, ADDITION OF BRANCH CONTRIBUTIONS TO NODAL          M42. 879
C          INJECTED CURRENT VECTOR  I ).                                M42. 880
C     18.  THIRD QUARTER OF TIME-STEP LOOP (SOURCE UPDATE, REPEAT       M42. 881
C          SOLUTION OF   (Y)V = I   FOR NODE VOLTAGE VECTOR  V ).       M42. 882
C     19.  FOURTH QUARTER OF TIME-STEP LOOP (SOLUTION OF 3-PHASE        M42. 883
C          NONLINEARITIES AND COMPENSATION-BASED ROTATING               M31. 158
C          MACHINERY [TYPE-50 S.M., U.M.],  SUPERPOSITION TO            M31. 159
C          GIVE THE TOTAL SOLUTION INCLUDING COMPENSATION).             M31. 160
C     20.  PUNCH AND PRINT TERMINAL CONDITIONS (IF REQUESTED).          M42. 884
C          CATALOG PLOT-DATA POINTS ON THE DISK AS A PERMANENT FILE, IF M42. 885
C          MISC. DATA PARAMETER  'ICAT'  IS POSITIVE.                   M42. 886
C                                                                       M42. 887
 9000 RETURN                                                            M22. 743
      END                                                               M42. 888
      SUBROUTINE TAPSAV ( NARRAY, N1, N2, N3 )                          M22. 744
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     NEAR-UNIVERSAL MODULE FOR DUMPING OR RESTORING (CENTRAL MEMORY    M32. 251
C     VS. DISK) OF  /LABEL/ .   THIS DOES NOT WORK FOR THOSE            M28. 285
C     COMPUTERS LIKE PRIME AND BURROUGHS WHERE  COMMON  BLOCKS          M28. 286
C     ARE NOT ORDERED REGULARLY IN MEMORY.   SWITCH "KBURRO"            M29. 948
C     SELECTS BETWEEN DISK OR VIRTUAL MEMORY (/C29B01/).                M29. 949
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK29.FOR;/NOLIST'                                           
      DIMENSION  NARRAY(1), KPEN(2)                                     M32. 252
      IF ( IPRSUP  .LT.  1 )   GO TO 5840                               M22. 747
      N9 = 0                                                            M39.  60
      KPEN(2) = 0                                                       M39.  61
      N4 = LOCINT( NARRAY(1) )                                          M29. 951
      WRITE (LUNIT6, 5831)  N1, N2, N3,  KBURRO , N4                    M39.  62
 5831 FORMAT ( /,   18H TOP OF  'TAPSAV'.,                              M32. 254
     1 48H      N1      N2      N3  KBURRO              N4              M39.  63
     2        ,/,  18X,  4I8, I16  )                                    M39.  64
C     FOLLOWING CHECK NORMALLY SENDS VAX EMTP TO 6327 (DISK IS          M32. 257
C     ONLY WANTED FOR TABLE SAVING WITHIN A SIMULATION FOR              M32. 258
C     TEST PURPOSES):                                                   M32. 259
 5840 IF ( KBURRO .EQ. 1 ) GO TO 6327                                           
 5448 IF ( N3  .GT.  1 )   GO TO 5891                                   M29. 958
      WRITE (N1)  ( NARRAY(I), I=1, N2 )                                M22. 755
      GO TO 9000                                                        M29. 959
 5891 READ (N1)   ( NARRAY(I), I=1, N2 )                                M22. 757
      GO TO 9000                                                        M29. 960
 6327 IF ( NCHAIN  .EQ.  20     .AND.                                   M29. 961
     1     MEMSAV  .EQ.  1  )   GO TO 5448                              M29. 962
      IF ( NCHAIN  .EQ.  1  )   GO TO 5448                              M29. 963
      IF ( NCHAIN .EQ. 6  .OR.                                          VAX.  30
     1     NCHAIN .EQ. 8 ) GO TO 9000                                   VAX.  31
C     PRECEDING "IF" BRANCHES WILL SEND  "MEMSAV=1"  TABLE              M29. 964
C     SAVING OF OVERLAY 20 AND  "START AGAIN"  TABLE RESTORING          M29. 965
C     OF OVERLAY 1 TO DISK, ALWAYS.   THIS IS FOR PERMANENT             M29. 966
C     (DISK) STORAGE.   "STATISTICS"  AND  "SYSTEMATIC"  DATA           M29. 967
C     CASES, ON THE OTHER HAND, SHALL DUMP TO  /C29B01/ .               M29. 968
C     FIRST 50 CELLS OF /C29B01/ (KARRAY) ARE SAVED FOR RTM USE.        M30. 108
      N13 = 29                                                          M32. 261
      CALL DIMENS ( KPEN(1), N13, TRASH, TRASH )                        M32. 262
      KVECSV = 2*(IT+IT+IBR+NTOT+IOFFD)+KSWTCH+LHIST                    M35. 282
      N9 = LTLABL + KVECSV * NBYTE(3)/NBYTE(4)                          M32. 264
      IF ( N9  .LT.  KPEN(2)+50 )   GO TO 6342                          M32. 265
      WRITE (LUNIT6, 6335)  N2, KPEN(2), N9, NCHAIN                     M35. 283
 6335 FORMAT ( 37H ERROR STOP IN "TAPSAV".  OVERFLOW OF,                M32. 267
     1         33H /C29B01/ STORAGE.  N2, KPEN(2) =,  2I8,              M35. 284
     2         24H     NEEDED STORAGE N9 =,  I8     ,/,                 M37. 897
     1 48H MEMORY REQUIREMENT IN INTEGER WORDS FOR VIRTUAL  ,           M39.  65
     2 48H COMPUTER IMPLEMENTATION OF TAPSAV. STORAGE MUST ,/,          M35. 286
     3 49H PROVIDE FOR ALL OF --/LABEL/--( DECK "LABCOM" ),   ,         M39.  66
     4 55H THE SEVERAL USAGES OF "VECRSV" AND "VECISV"(OVER6-11),  ,    M39.  67
     5 21H PLUS 50 EXTRA CELLS.   ,/,  9H NCHAIN =,  I5  )              M35. 287
      CALL STOPTP                                                               
 6342 J = 50                                                            M32. 277
      IF ( N3  .GT.  1 )   GO TO 6352                                   M32. 278
      DO 6347  K=1, N2                                                  M32. 279
      J = J + 1                                                         M32. 280
 6347 KARRAY(J) = NARRAY(K)                                             M32. 281
      GO TO 9000                                                        M32. 282
 6352 DO 6356  K=1, N2                                                  M32. 283
      J = J + 1                                                         M32. 284
 6356 NARRAY(K) = KARRAY(J)                                             M32. 285
 9000 IF ( IPRSUP  .GE.  1 )                                            M32. 286
     1 WRITE (LUNIT6, 9003)   N9, KPEN(2)                               M35. 288
 9003 FORMAT ( 31H EXIT "TAPSAV".   N9, KPEN(2) =,  2I8  )              M35. 289
      RETURN                                                            M32. 289
      END                                                               M22. 759
      SUBROUTINE DGELG(R,A,M,N,EPS,IER)                                 M35. 290
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
       DIMENSION A(1), R(1)                                             M35. 291
C        PURPOSE                                                        M35. 292
C           TO SOLVE A GENERAL SYSTEM OF SIMULTANEOUS LINEAR EQUATIONS. M35. 293
C        USAGE                                                          M35. 294
C           CALL DGELG(R,A,M,N,EPS,IER)                                 M35. 295
C        DESCRIPTION OF PARAMETERS                                      M35. 296
C           R      - DOUBLE PRECISION M BY N RIGHT HAND SIDE MATRIX     M35. 297
C                    (DESTROYED). ON RETURN R CONTAINS THE SOLUTIONS    M35. 298
C                    OF THE EQUATIONS.                                  M35. 299
C           A      - DOUBLE PRECISION M BY M COEFFICIENT MATRIX         M35. 300
C                    (DESTROYED).                                       M35. 301
C           M      - THE NUMBER OF EQUATIONS IN THE SYSTEM.             M35. 302
C           N      - THE NUMBER OF RIGHT HAND SIDE VECTORS.             M35. 303
C           EPS    - SINGLE PRECISION INPUT CONSTANT WHICH IS USED AS   M35. 304
C                    RELATIVE TOLERANCE FOR TEST ON LOSS OF             M35. 305
C                    SIGNIFICANCE.                                      M35. 306
C           IER    - RESULTING ERROR PARAMETER CODED AS FOLLOWS         M35. 307
C                    IER=0  - NO ERROR,                                 M35. 308
C                    IER=-1 - NO RESULT BECAUSE OF M LESS THAN 1 OR     M35. 309
C                             PIVOT ELEMENT AT ANY ELIMINATION STEP     M35. 310
C                             EQUAL TO 0,                               M35. 311
C                    IER=K  - WARNING DUE TO POSSIBLE LOSS OF SIGNIFI-  M35. 312
C                             CANCE INDICATED AT ELIMINATION STEP K+1,  M35. 313
C                             WHERE PIVOT ELEMENT WAS LESS THAN OR      M35. 314
C                             EQUAL TO THE INTERNAL TOLERANCE EPS TIMES M35. 315
C                             ABSOLUTELY GREATEST ELEMENT OF MATRIX A.  M35. 316
C        REMARKS                                                        M35. 317
C           INPUT MATRICES R AND A ARE ASSUMED TO BE STORED COLUMNWISE  M35. 318
C           IN M*N RESP. M*M SUCCESSIVE STORAGE LOCATIONS. ON RETURN    M35. 319
C           SOLUTION MATRIX R IS STORED COLUMNWISE TOO.                 M35. 320
C           THE PROCEDURE GIVES RESULTS IF THE NUMBER OF EQUATIONS M IS M35. 321
C           GREATER THAN 0 AND PIVOT ELEMENTS AT ALL ELIMINATION STEPS  M35. 322
C           ARE DIFFERENT FROM 0. HOWEVER WARNING IER=K - IF GIVEN -    M35. 323
C           INDICATES POSSIBLE LOSS OF SIGNIFICANCE. IN CASE OF A WELL  M35. 324
C           SCALED MATRIX A AND APPROPRIATE TOLERANCE EPS, IER=K MAY BE M35. 325
C           INTERPRETED THAT MATRIX A HAS THE RANK K. NO WARNING IS     M35. 326
C           GIVEN IN CASE M=1.                                          M35. 327
C        METHOD                                                         M35. 328
C           SOLUTION IS DONE BY MEANS OF GAUSS-ELIMINATION WITH         M35. 329
C           COMPLETE PIVOTING.                                          M35. 330
C     INTRINSIC  ABSZ                                                   M35. 331
      IF ( M  .LE.  0 )   GO TO 23                                      M35. 332
C     SEARCH FOR GREATEST ELEMENT IN MATRIX A                           M35. 333
      IER=0                                                             M35. 334
      PIV=0.0                                                           M35. 335
      MM=M*M                                                            M35. 336
      NM=N*M                                                            M35. 337
      DO 3 L=1,MM                                                       M35. 338
      TB= ABSZ(A(L))                                                    M35. 339
      IF ( TB  .LE.  PIV )   GO TO 3                                    M35. 340
      PIV=TB                                                            M35. 341
      I=L                                                               M35. 342
    3 CONTINUE                                                          M35. 343
      TOL=EPS*PIV                                                       M35. 344
C     A(I) IS PIVOT ELEMENT. PIV CONTAINS THE ABSOLUTE VALUE OF A(I).   M35. 345
C     START ELIMINATION LOOP                                            M35. 346
      LST=1                                                             M35. 347
      DO 17 K=1,M                                                       M35. 348
C     TEST ON SINGULARITY                                               M35. 349
      IF ( PIV  .LE.  0.0 )   GO TO 23                                  M35. 350
      IF ( IER  .NE.  0 )   GO TO 7                                     M35. 351
      IF ( PIV  .GT.  TOL )   GO TO 7                                   M35. 352
      IER=K-1                                                           M35. 353
    7 PIVI=1.0 /A(I)                                                    M35. 354
      J=(I-1)/M                                                         M35. 355
      I=I-J*M-K                                                         M35. 356
      J=J+1-K                                                           M35. 357
C     I+K IS ROW-INDEX, J+K COLUMN-INDEX OF PIVOT ELEMENT               M35. 358
C     PIVOT ROW REDUCTION AND ROW INTERCHANGE IN RIGHT HAND SIDE R      M35. 359
      DO 8 L=K,NM,M                                                     M35. 360
      LL=L+I                                                            M35. 361
      TB=PIVI*R(LL)                                                     M35. 362
      R(LL)=R(L)                                                        M35. 363
    8 R(L)=TB                                                           M35. 364
C     IS ELIMINATION TERMINATED                                         M35. 365
      IF ( K  .GE.  M )   GO TO 18                                      M35. 366
C     COLUMN INTERCHANGE IN MATRIX A                                    M35. 367
      LEND=LST+M-K                                                      M35. 368
      IF ( J  .LE.  0 )   GO TO 12                                      M35. 369
      II=J*M                                                            M35. 370
      DO 11 L=LST,LEND                                                  M35. 371
      TB=A(L)                                                           M35. 372
      LL=L+II                                                           M35. 373
      A(L)=A(LL)                                                        M35. 374
   11 A(LL)=TB                                                          M35. 375
C     ROW INTERCHANGE AND PIVOT ROW REDUCTION IN MATRIX A               M35. 376
   12 DO 13 L=LST,MM,M                                                  M35. 377
      LL=L+I                                                            M35. 378
      TB=PIVI*A(LL)                                                     M35. 379
      A(LL)=A(L)                                                        M35. 380
   13 A(L)=TB                                                           M35. 381
C     SAVE COLUMN INTERCHANGE INFORMATION                               M35. 382
      A(LST)=J                                                          M35. 383
C     ELEMENT REDUCTION AND NEXT PIVOT SEARCH                           M35. 384
      PIV=0.0                                                           M35. 385
      LST=LST+1                                                         M35. 386
      J=0                                                               M35. 387
      DO 16 II=LST,LEND                                                 M35. 388
      PIVI=-A(II)                                                       M35. 389
      IST=II+M                                                          M35. 390
      J=J+1                                                             M35. 391
      DO 15 L=IST,MM,M                                                  M35. 392
      LL=L-J                                                            M35. 393
      A(L)=A(L)+PIVI*A(LL)                                              M35. 394
      TB= ABSZ(A(L))                                                    M35. 395
      IF ( TB  .LE.  PIV )   GO TO 15                                   M35. 396
      PIV=TB                                                            M35. 397
      I=L                                                               M35. 398
   15 CONTINUE                                                          M35. 399
      DO 16 L=K,NM,M                                                    M35. 400
      LL=L+J                                                            M35. 401
   16 R(LL)=R(LL)+PIVI*R(L)                                             M35. 402
   17 LST=LST+M                                                         M35. 403
C     END OF ELIMINATION LOOP                                           M35. 404
C     BACK SUBSTITUTION AND BACK INTERCHANGE                            M35. 405
   18 IF ( M  .GT.  1 )   GO TO 19                                      M35. 406
      IF ( M  .EQ.  1 )   GO TO 22                                      M35. 407
      GO TO 23                                                          M35. 408
   19 IST=MM+M                                                          M35. 409
      LST=M+1                                                           M35. 410
      DO 21 I=2,M                                                       M35. 411
      II=LST-I                                                          M35. 412
      IST=IST-LST                                                       M35. 413
      L=IST-M                                                           M35. 414
      L=A(L)+ 0.5                                                       M35. 415
      DO 21 J=II,NM,M                                                   M35. 416
      TB=R(J)                                                           M35. 417
      LL=J                                                              M35. 418
      DO 20 K=IST,MM,M                                                  M35. 419
      LL=LL+1                                                           M35. 420
   20 TB=TB-A(K)*R(LL)                                                  M35. 421
      K=J+L                                                             M35. 422
      R(J)=R(K)                                                         M35. 423
   21 R(K)=TB                                                           M35. 424
   22 RETURN                                                            M35. 425
C     ERROR RETURN                                                      M35. 426
   23 IER=-1                                                            M35. 427
      RETURN                                                            M35. 428
      END                                                               M35. 429
      SUBROUTINE MATMUL(AUM,BUM)                                        M32. 290
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MATRIX ALGEBRA MODULE USED BY UNIVERSAL MACHINE (U.M.)            M32. 291
      DIMENSION AUM(3,3),BUM(3,3),CUM(3,3)                              M32. 292
      N5 = 3                                                            M32. 293
      DO 20 N1 = 1,N5                                                   M32. 294
      DO 20 N2 = 1,N5                                                   M32. 295
      CUM(N1,N2) = AUM(N1,1) * BUM(1,N2)                                M32. 296
      DO 10 N3 = 2,N5                                                   M32. 297
   10 CUM(N1,N2) =  CUM(N1,N2) + AUM(N1,N3) * BUM(N3,N2)                M32. 298
   20 CONTINUE                                                          M32. 299
      DO 30 N1 = 1,N5                                                   M32. 300
      DO 30 N2 = 1,N5                                                   M32. 301
      AUM(N1,N2) = CUM(N1,N2)                                           M32. 302
   30 CONTINUE                                                          M32. 303
      RETURN                                                            M32. 304
      END                                                               M32. 305
      SUBROUTINE MATVEC(AUM,YUM)                                        M32. 306
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MATRIX ALGEBRA MODULE USED BY UNIVERSAL MACHINE (U.M.)            M32. 307
      DIMENSION AUM(3,3),YUM(15),X(3)                                   M32. 308
      N1 = 3                                                            M32. 309
      DO 10 N2 = 1,N1                                                   M32. 310
   10 X(N2) = 0.0                                                       M32. 311
      DO 30 N2 = 1,N1                                                   M32. 312
      DO 20 N3 = 1,N1                                                   M32. 313
   20 X(N2) = X(N2) + AUM(N2,N3) * YUM(N3)                              M32. 314
   30 CONTINUE                                                          M32. 315
      DO 40 N2 = 1,N1                                                   M32. 316
   40 YUM(N2) = X(N2)                                                   M32. 317
      RETURN                                                            M32. 318
      END                                                               M32. 319
      SUBROUTINE PLTFIL ( K )                                           M32. 320
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     INSTALLATION-DEPENDENT MODULE WHICH IS CALLED FOR                 M32. 321
C     OUTPUT-VECTOR DUMPING BY  "SUBTS3"  AND  "OVER20"  IF             M32. 322
C     "M4PLOT" OF  /BLANK/  IS NONZERO.   THIS                          M35. 430
C     IS ALTERNATIVE TO CONVENTIONAL IN-LINE DUMPING ON DISK.           M32. 324
C     MODULE SHOULD BE UNIVERSAL FOR COMPUTERS USING FORTRAN 77         M37. 899
C     COMPILERS AND REAL*4 VARIABLES WHICH GIVE SINGLE PRECISION.       M37. 900
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      SAVE                                                              M38. 116
      DATA  N17   /  0  /   ! INITIALIZE ROLLING PLOT FREQ COUNT        M38. 117
      IF ( M4PLOT  .EQ.  1 )   GO TO 7286   ! SIMULATOR USE             M32. 341
C     WE PASS HERE WITH M4PLOT=2,  INDICATING DISK STORAGE,  BUT        M32. 342
C     SINGLE-PRECISION (REAL*4) NUMBERS ONLY:                           M32. 343
      IF ( K  .LE.  450 )  GO TO 7273                                   M32. 344
      WRITE (LUNIT6, 7269)  K                                           M32. 345
 7269 FORMAT (  ' ^^^^^^^^^^^^^^^   ERROR STOP IN "PLTFIL"',            M32. 346
     1          '   ^^^^^^^^^^^^^^'   ,/,  ' ^^^^^^  TOO MANY',         M32. 347
     2          ' OUTPUT VARIABLES (',  I3,  ' )  FOR USE',             M32. 348
     3          ' REAL*4 PLOT FILE.   LIMIT = 450.'   )                 M32. 349
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 901
 7273 DO 7278  J=1, K                                                   M32. 351
 7278 FORBYT(J) = VOLTI(J)                                              M32. 352
C     FOLLOWING APOLLO CARD REPLACES 2 PRECEDING VAX ONES:              M38. 118
C7273 CALL VEC_$DP_SP ( VOLTI(1), FORBYT(1), K )                        M38. 119
      WRITE (LUNIT4)  ( FORBYT(J), J=1, K )                             M32. 353
      GO TO 9000         ! EXIT MODULE AFTER POSSIBLE DIAGNOSTIC        M35. 432
 7286 IF ( IPRSUP  .GE.  3 )                                            M35. 433
     1 WRITE (LUNIT6, 7301)  INDBUF, MFLUSH, K, LIMBUF, NEWVEC,         M35. 434
     2        NUMCRD,       ( VOLTI(J), J=1, 10 )                       M37. 902
 7301 FORMAT ( ' TOP OF "PLTFIL".  INDBUF, MFLUSH, K,',                 M35. 435
     1  ' LIMBUF, NEWVEC, NUMCRD =', 6I10  ,/,  1X,  10E13.4 )          M35. 436
      N7 = 0                                                            M32. 362
      IF ( INDBUF  .GT.  0 )   GO TO 7308  ! NOT VERY 1ST STEP          M35. 437
C     PLOT DATA STORAGE BEGINS AFTER "TABLES" USAGE FOR "LABCOM"        M35. 438
C     PLUS 50 MISC. CELLS, PLUS EMTP DATA CARDS.  OFFSET IS:            M35. 439
      MFLUSH = 0   ! WE HAVE FLUSHED PLTBUF TO DISK ZERO TIMES          M35. 440
      INDBEG = LTLABL + 51    ! PLOT POINTS OFFSET BY "LABCOM"          M35. 441
 7304 INDBUF = INDBEG   ! RESET PLOT STORAGE AT BEGINNING               M35. 442
      NEWVEC = INDBUF + 1    ! PLOT DATA BEGINS AFTER DATA CARDS        M32. 369
 7308 IF ( INDBUF + K .LE. LIMBUF )  GO TO 7374   ! NOT FULL YET        M35. 443
      WRITE (LUNIT6, 7311)  INDBUF, LIMBUF                              M35. 444
      WRITE (MUNIT6, 7311)  INDBUF, LIMBUF                              M35. 445
 7311 FORMAT ( '   % % % % % %   Suspended',                            M37. 903
     1         ' simulation;  plot data space exhausted;  use',         M38. 120
     2         ' SPY.   INDBUF, LIMBUF =',  2I8  )                      M38. 121
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 904
      WRITE (MUNIT6, 7312)                                              M35. 450
 7312 FORMAT ( '   % %  Time-sharing disabled.   Send',                 M38. 122
     1         ' user-keyed interrupt to silence alarm.' )              M38. 123
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 905
      LL10 = 10                                                         M36. 260
      CALL HONKER ( LL10 )   ! DISASTER-LEVEL AUDIBLE INDICATION        M36. 261
      N13 = KBREAK      ! REMEMBER PRESENT VALUE, FOR AFTER SPY         M35. 452
      LOCKBR = 1     ! ENSURE FORCED STAY IN SPY UNTIL "UNLOCK"         M36. 262
      LASTOV = 9911  ! REMEMBER OVERFLOW FOR "FLAGER", "SPYINK"         M38. 124
      CALL EMTSPY    ! TRANSFER CONTROL TO SPY, TO MANAGE SPACE         M38. 125
      GO TO 7308    ! B4 CONTINUE, CHECK THAT USER DID HIS JOB          M35. 456
 7374 DO 7389  J=1, K  ! TRANSFER EACH CELL J OF OUTPUT VECTOR          M35. 457
      INDBUF = INDBUF + 1     ! INDEX TO NEXT UNUSED PLTBUF CELL        M35. 458
 7389 PLTBUF(INDBUF) = VOLTI(J)  !  END  DO 7389  LOOP OVER "J"         M35. 459
C     FOLLOWING 2 APOLLO CARDS REPLACE 3 PRECEDING VAX ONES:            M38. 126
C7374 CALL VEC_$DP_SP ( VOLTI(1), PLTBUF(INDBUF+1), K )                 M38. 127
C     INDBUF = INDBUF + K                                               M38. 128
      N17 = N17 + 1   ! ANOTHER STEP SINCE LAST ROLLING SERVICE         M38. 129
      IF ( N17 .LT. KSLOWR ) GO TO 9000  ! NOT ENOUGH POINTS YET        M38. 130
      N17 = 0     ! RESET COUNTER TO DELAY ROLLING AFTER PRESENT        M38. 131
      IF ( MONITR .EQ. 0 ) GO TO 7396   ! NO ROLLING VECTOR PLOT        M38. 132
      CALL TEKPLT    ! ADD TO RIGHT EDGE OF PRESENT VECTOR PLOT         M38. 133
      IF ( MONITR .NE. 8765 ) GO TO 7396  ! VECTOR ADDITION DONE        M38. 134
      CALL TIMVAL   ! REGENERATE PLOT, WITH RIGHT HALF ON LEFT          M38. 135
      MONITR = 1   ! PLOT REGENERATED, SO TURN OFF REQUEST FLAG         M38. 136
 7396 IF ( MONITS .NE. 0 )  CALL CHRPLT ! ROLLING CHARACTER PLOT        M38. 137
 9000 IF ( IPRSUP  .GE.  4 )                                            M32. 414
     1 WRITE (LUNIT6, 9004)                                             M32. 415
 9004 FORMAT (  ' EXIT "PLTFIL".'  )                                    M32. 416
      RETURN                                                            M32. 432
      END                                                               M32. 433
      SUBROUTINE PLTLU2 ( D2, VOLTI )                                   M32. 434
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     CALLED BY "TACS2" ONLY, IF AND ONLY IF M4PLOT .NE. 0              M32. 436
C     THIS MODULE IS UNIVERSAL FOR FORTRAN 77 COMPILERS AND             M37. 906
C     COMPUTERS FOR WHICH REAL*4 CORRESPONDS TO SINGLE PRECISION.       M37. 907
      DIMENSION  VOLTI(1)                                               M32. 437
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      REAL*4  FORBYT(150)                                                       
      IF ( IOFGND .GT. 149 )                                                    
     1 CALL STOPTP                                                              
      N12 = IOFGND + 1                                                  M32. 441
      READ (LUNIT2)  ( FORBYT(J), J=1, N12 )                            M32. 442
      D2 = FORBYT(1)                                                    M32. 443
      DO 1942  J=1, IOFGND                                              M32. 444
 1942 VOLTI(J) = FORBYT(J+1)                                            M32. 445
      IF ( IPRSUP  .GE.  1 )                                            M32. 446
     1 WRITE (LUNIT6, 1978)  D2, VOLTI(1), VOLTI(IOFGND)                M32. 447
 1978 FORMAT ( 38H EXIT "PLTLU2".  D2, VOLTI(1,IOFGND) =,               M39.  68
     1  3E14.5  )                                                       M32. 449
      RETURN                                                            M32. 450
      END                                                               M32. 451
      SUBROUTINE VECRSV ( ARRAY, N13, N2 )                              M32. 452
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE FOR VECTOR DUMPING/RESTORING OF "OVER6", "OVER8", ETC.     M37. 910
C     THIS IS UNIVERSAL FOR VIRTUAL COMPUTERS WHICH CHOSE TO            M37. 911
C     USE /C29B01/ SPACE FOR THIS, AS WELL AS ALL OF "LABCOM".          M37. 912
C     ALSO NEEDED ARE UNCOUNTED HOLLERITH.  PARALLEL TO "VECISV".       M37. 913
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK29.FOR;/NOLIST'                                           
      DIMENSION  ARRAY(1), FARRAY(1)                                    M34.   5
      EQUIVALENCE  ( KARRAY(1),  FARRAY(1) )                            M34.   6
C     BLOCK /VECCOM/ IS SHARED BY "VECRSV" AND "VECISV".                M34.   7
C     KOFVEC(KNTVEC) REMEMBERS INDEX FOR KNTVEC-TH DUMPING.             M37. 914
      COMMON  /VECCOM/  KNTVEC,  KOFVEC(20)                             M37. 915
      IF ( IPRSUP  .GE.  1 )                                            M37. 916
     1 WRITE (LUNIT6, 1623) N13, N2, KNTVEC                             M37. 917
 1623 FORMAT ( 27H BEGIN "VECRSV".  N13, N2 =,  2I8,                    M34.  12
     1    '     KNTVEC =',  I8 )                                        M37. 918
      IF ( N2  .NE.  0 )  GO TO 1638                                    M34.  14
      IF ( N13  .GE.  0 )  KNTVEC = N13                                 M37. 919
      IF ( N13  .LT.  0 )  KNTVEC = KNTVEC + N13                        M37. 920
      IF ( IPRSUP  .GE.  2 )                                            M37. 921
     1 WRITE (LUNIT6, 1629)  N13                                        M37. 922
 1629 FORMAT ( ' INITIALIZATION OF KNTVEC.  N13 =',  I10 )              M37. 923
      GO TO 9000                                                        M34.  19
 1638 IF ( N2  .EQ.  1 )   GO TO 1671                                   M34.  20
C     BEGIN CODE TO RESTORE  (ARRAY(K), K=1, N13)  FROM TANK:           M34.  21
      KNTVEC = KNTVEC + 1                                               M37. 924
      N4 = KOFVEC(KNTVEC)                                               M37. 925
      IF ( IPRSUP  .GE.  2 )                                            M37. 926
     1 WRITE (LUNIT6, 1640)  KNTVEC, N4                                 M37. 927
 1640 FORMAT ( ' READY TO RESTORE.  KNTVEC, N4 =',  2I10 )              M37. 928
      IF ( N13  .LE.  0 )   GO TO 9000                                  M35. 462
      DO 1641  K=1, N13                                                 M34.  27
      ARRAY(K) = FARRAY(N4)                                             M34.  28
 1641 N4 = N4 + 1                                                       M34.  29
      GO TO 9000                                                        M34.  30
C     BEGIN CODE TO DUMP  (ARRAY(K), K=1, N13)  INTO TANK:              M34.  31
 1671 IF ( KNTVEC .GT. 0 )   GO TO 1674                                 M37. 929
      N14 = NBYTE(3) / NBYTE(4) ! RELATIVE LENGTHS  REAL/INTEGER        M37. 930
      KOFVEC(1) =  ( LTLABL + 1 ) / N14  +  51  ! BEGIN STORAGE         M37. 931
      IF ( IPRSUP  .GE.  2 )                                            M37. 932
     1 WRITE (LUNIT6, 1673)  KOFVEC(1)                                  M37. 933
 1673 FORMAT ( ' INITIALIZE KOFVEC(1) =',  I10 )                        M37. 934
 1674 KNTVEC = KNTVEC + 1                                               M37. 935
      N4 = KOFVEC(KNTVEC)                                               M37. 936
      IF ( IPRSUP  .GE.  2 )                                            M37. 937
     1 WRITE (LUNIT6, 1675)  KNTVEC, N4                                 M37. 938
 1675 FORMAT ( ' READY TO DUMP.  KNTVEC, N4 =',  2I10 )                 M37. 939
      IF ( N13  .LE.  0 )   GO TO 1683                                  M35. 463
      DO 1676  K=1, N13                                                 M34.  41
      FARRAY(N4) = ARRAY(K)                                             M34.  42
 1676 N4 = N4 + 1                                                       M34.  43
 1683 IF ( KNTVEC  .GE.  20 )  ! IF /VECCOM/ STORAGE EXCEEDED,          M37. 940
     1 CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD         M37. 941
      KOFVEC(KNTVEC+1) = N4                                             M37. 942
      IF ( IPRSUP  .GE.  2 )                                            M37. 943
     1 WRITE (LUNIT6, 1687)  KOFVEC(KNTVEC+1)                           M37. 944
 1687 FORMAT ( ' DEFINE  KOFVEC(KNTVEC+1) =',  I10 )                    M37. 945
 9000 IF ( IPRSUP  .GE.  1 )                                            M37. 946
     1 WRITE (LUNIT6, 9007)  ARRAY(1), ARRAY(2), ARRAY(N13)             M34.  50
 9007 FORMAT ( 33H EXIT "VECRSV".  ARRAY(1;2;N13) =,  3E15.6 )          M34.  51
      IF ( IPRSUP .GE. 2 )  WRITE (LUNIT6, 9011) KOFVEC                 M37. 947
 9011 FORMAT ( ' KOFVEC =',  20I6  )                                    M37. 948
      RETURN                                                            M34.  54
      END                                                               M32. 514
      SUBROUTINE VECISV ( KARR, N13, N2 )                               M32. 515
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE FOR VECTOR DUMPING/RESTORING OF "OVER6", "OVER8", ETC.     M37. 949
C     THIS IS UNIVERSAL FOR VIRTUAL COMPUTERS WHICH CHOSE TO            M37. 950
C     USE /C29B01/ SPACE FOR THIS, AS WELL AS ALL OF "LABCOM".          M37. 951
C     ALSO NEEDED ARE UNCOUNTED HOLLERITH.  PARALLEL TO "VECISV".       M37. 952
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK29.FOR;/NOLIST'                                           
      DIMENSION   FARRAY(1)                                             M34.  58
      EQUIVALENCE  ( KARRAY(1),  FARRAY(1) )                            M34.  59
      DIMENSION  KARR(1)                                                M34.  60
C     BLOCK /VECCOM/ IS SHARED WITH "VECRSV" (SEE FOR MORE INFO)        M34.  61
      COMMON  /VECCOM/  KNTVEC,  KOFVEC(20)                             M37. 953
      IF ( IPRSUP  .GE.  1 )                                            M34.  63
     1 WRITE (LUNIT6, 1423)  N13, N2                                    M34.  64
 1423 FORMAT ( 27H BEGIN "VECISV".  N13, N2 =,  2I8  )                  M34.  65
      IF ( N2  .EQ.  1 )   GO TO 1471                                   M34.  66
C     BEGIN CODE TO RESTORE  (KARR(K), K=1, N13)  FROM TANK:            M34.  67
      KNTVEC = KNTVEC + 1                                               M37. 954
      N4 = KOFVEC(KNTVEC)                                               M37. 955
      IF ( IPRSUP  .GE.  2 )                                            M37. 956
     1 WRITE (LUNIT6, 1428)  KNTVEC, N4                                 M37. 957
 1428 FORMAT ( ' READY TO RESTORE.  KNTVEC, N4 =',  2I10 )              M37. 958
      DO 1441  K=1, N13                                                 M34.  73
      KARR(K) = FARRAY(N4)                                              M34.  74
 1441 N4 = N4 + 1                                                       M34.  75
      GO TO 9000                                                        M34.  76
C     BEGIN CODE TO DUMP  (KARR(K), K=1, N13)  INTO TANK:               M34.  77
 1471 IF ( KNTVEC .GT. 0 )  GO TO 1474                                  M37. 959
      N14 = NBYTE(3) / NBYTE(4) ! RELATIVE LENGTHS  REAL/INTEGER        M37. 960
      KOFVEC(1) =  ( LTLABL + 1 ) / N14  +  51                          M37. 961
      IF ( IPRSUP .GE. 1 )                                              M37. 962
     1 WRITE (LUNIT6, 1473)  KOFVEC(1)                                  M37. 963
 1473 FORMAT ( ' INITIALIZE KOFVEC(1) =',  I10 )                        M37. 964
 1474 KNTVEC = KNTVEC + 1                                               M37. 965
      N4 = KOFVEC(KNTVEC)                                               M37. 966
      IF ( IPRSUP  .GE.  1 )                                            M34.  83
     1 WRITE (LUNIT6, 1475)  KNTVEC, N4                                 M37. 967
 1475 FORMAT ( ' READY TO DUMP.  KNTVEC, N4 =',  2I10 )                 M37. 968
      KOFVEC(KNTVEC) = N4  ! CORRECT INTEGER-VECTOR BEGINNING           M37. 969
      DO 1476  K=1, N13                                                 M34.  87
      FARRAY(N4) = KARR(K)                                              M34.  88
 1476 N4 = N4 + 1                                                       M34.  89
      IF ( KNTVEC .GE. 20 )   ! IF /VECCOM/ STORAGE EXCEEDED,           M37. 970
     1 CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD         M37. 971
      KOFVEC(KNTVEC+1) = N4                                             M37. 972
      IF ( IPRSUP  .GE.  1 )                                            M34.  92
     1 WRITE (LUNIT6, 1482)  KOFVEC(KNTVEC+1)                           M37. 973
 1482 FORMAT ( ' DEFINE KOFVEC(KNTVEC+1) =',  I10 )                     M37. 974
 9000 IF ( IPRSUP  .GE.  1 )                                            M34.  95
     1 WRITE (LUNIT6, 9007)  KARR(1), KARR(2), KARR(N13)                M34.  96
 9007 FORMAT ( 32H EXIT "VECISV".  KARR(1;2;N13) =,  3I10  )            M34.  97
      IF ( IPRSUP .GE. 2 )  WRITE (LUNIT6, 9011) KOFVEC                 M37. 975
 9011 FORMAT ( ' KOFVEC =',  20I6  )                                    M37. 976
      RETURN                                                            M34. 100
      END                                                               M32. 566
      SUBROUTINE VECRXX ( ARRAY, N13, N2 )                              M32. 567
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL (NON-VIRTUAL) FORM OF MODULE FOR BINARY I/O.  IF        M32. 568
C     EXTRACTED FROM UTPF FOR USE, CONVERT NAME "VECRXX" TO "VECRSV"    M32. 569
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION  ARRAY(1), FARRAY(1)                                    M32. 571
      IF ( IPRSUP  .GE.  1 )                                            M32. 572
     1 WRITE (LUNIT6, 1575)  N13, N2                                    M32. 573
 1575 FORMAT ( 27H BEGIN "VECRSV".  N13, N2 =,  2I8  )                  M32. 574
      IF ( N2  .NE.  0 )  GO TO 1638                                    M32. 575
C     ZERO N2 MEANS THAT WE WANT TO POSITION TAPE FOR NEXT READ:        M32. 576
      IF ( N13  .GE.  0 )   GO TO 1592                                  M32. 577
      N6 = -N13                                                                 
      DO 1586  J=1, N6                                                  M32. 579
 1586 BACKSPACE LUNT13                                                  M32. 580
      GO TO 9000                                                        M32. 581
 1592 REWIND LUNT13                                                     M32. 582
      IF ( N13  .EQ.  0 )   GO TO 1612                                  M32. 583
      DO 1607  J=1, N13                                                 M32. 584
 1607 READ (LUNT13)  N14                                                M32. 585
 1612 IF ( IPRSUP  .GE.  1 )                                            M32. 586
     1 WRITE (6, 1613)  N13                                             M32. 587
 1613 FORMAT (  ' POSITION MAGNETIC TAPE.  N13 =',  I4  )               M32. 588
      N13 = 3                                                           M32. 589
      GO TO 9000                                                        M32. 590
 1638 IF ( N2  .EQ.  1 )   GO TO 1671                                   M32. 591
C     BEGIN CODE TO RESTORE  (ARRAY(K), K=1, N13)  FROM TAPE:           M32. 592
      READ (LUNT13)  ( ARRAY(K), K=1, N13 )                             M32. 593
      GO TO 9000                                                        M32. 594
C     BEGIN CODE TO DUMP  (ARRAY(K), K=1, N13)  ONTO TAPE:              M32. 595
 1671 WRITE (LUNT13)  ( ARRAY(K), K=1, N13 )                            M32. 596
 9000 IF ( IPRSUP  .GE.  1 )                                            M32. 597
     1 WRITE (LUNIT6, 9007)  ARRAY(1), ARRAY(2), ARRAY(N13)             M32. 598
 9007 FORMAT ( 33H EXIT "VECRSV".  ARRAY(1;2;N13) =,  3E15.6 )          M32. 599
      RETURN                                                            M32. 600
      END                                                               M32. 601
      SUBROUTINE VECIXX ( KARR, N13, N2 )                               M32. 602
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL (NON-VIRTUAL) FORM OF MODULE FOR BINARY I/O.  IF        M32. 603
C     EXTRACTED FROM UTPF FOR USE, CONVERT NAME "VECIXX" TO "VECISV"    M32. 604
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION  KARR(1)                                                M32. 606
      IF ( IPRSUP  .GE.  1 )                                            M32. 607
     1 WRITE (LUNIT6, 1423)  N13, N2                                    M32. 608
 1423 FORMAT ( 27H BEGIN "VECISV".  N13, N2 =,  2I8  )                  M32. 609
      IF ( N2  .EQ.  1 )   GO TO 1471                                   M32. 610
C     BEGIN CODE TO RESTORE  (KARR(K), K=1, N13)  FROM TAPE:            M32. 611
      READ (LUNT13)  ( KARR(K), K=1, N13 )                              M32. 612
      GO TO 9000                                                        M32. 613
C     BEGIN CODE TO DUMP  (KARR(K), K=1, N13)  ONTO TAPE:               M32. 614
 1471 WRITE (LUNT13)  ( KARR(K), K=1, N13 )                             M32. 615
 9000 IF ( IPRSUP  .GE.  1 )                                            M32. 616
     1 WRITE (LUNIT6, 9007)  KARR(1), KARR(2), KARR(N13)                M32. 617
 9007 FORMAT ( 32H EXIT "VECISV".  KARR(1;2;N13) =,  3I10  )            M32. 618
      RETURN                                                            M32. 619
      END                                                               M32. 620
      SUBROUTINE NAMEA6 ( TEXT1, N24 )                                  M38. 138
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE FOR MAINTAINANCE OF ALPHANUMERIC VECTOR TEXVEC OF          M38. 139
C     "LABCOM".  MAXBUS OF "BLKCOM" IS LAST USED CELL.  N24 CHOOSES     M38. 140
C     MODE OF USE:  0 WILL ADD TEXT1, POSITIVE WILL LOCATE IT,          M38. 141
C     AND NEGATIVE WILL DESTROY (REMOVE) IT.                            M38. 142
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      REAL*8        TEXT1, TEXT2                                        M38. 145
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M38. 146
      DATA  N17   /   0   /                                                     
      DATA  TEXT2   /  6HUNUSED  /                                              
      IF ( MAXBUS .LE. 0 )  GO TO 3423                                          
      DO 3416  J=1, MAXBUS                                                      
      IF ( TEXT1 .EQ. TEXVEC(J) ) GO TO 3446                                    
 3416 CONTINUE                                                          M38. 152
 3423 IF ( N24 .NE. 0 )  GO TO 3438                                             
      IF ( N17 .EQ. 0 )  GO TO 3434                                     M38. 154
      TEXVEC(N17) = TEXT1                                                       
      N24 = N17                                                                 
      DO 3428  J=1, MAXBUS                                                      
      IF ( TEXVEC(J) .NE. TEXT2 ) GO TO 3428                                    
      N17 = J                                                                   
      GO TO 9000                                                                
 3428 CONTINUE                                                          M38. 161
      N17 = 0                                                                   
      GO TO 9000                                                                
 3434 MAXBUS = MAXBUS + 1                                                       
      IF ( MAXBUS .GT. LSIZE7 )  STOP                                           
      TEXVEC(MAXBUS) = TEXT1                                                    
      N24 = MAXBUS                                                              
      GO TO 9000                                                                
 3438 IF ( IPRSUP .GE. 1 )                                              M38. 169
     1 WRITE (LUNIT6, 3442)  MAXBUS, TEXT1, N24                         M38. 170
 3442 FORMAT ( 39H  +++++  SEARCH OF EMTP NAME VECTOR BUS,              M38. 171
     1         13H THROUGH CELL,  I5,                                   M38. 172
     2         35H   IN  "NAMEA6"  SHOWS NO MATCH FOR                   M38. 173
     3    ,/,  10H         ",  A6,  20H".   RETURN -INTINF.,  I10 )     M43.   1
      N24 = -INTINF                                                             
      GO TO 9000                                                                
 3446 IF ( N24 .LT. 0 )  GO TO 3455                                             
      N24 = J                                                                   
      GO TO 9000                                                                
 3455 TEXVEC(J) = TEXT2                                                         
      N17 = J                                                                   
 9000 IF ( IPRSUP .GE. 6 )                                              M38. 182
     1 WRITE (LUNIT6, 9004)  TEXT1, MAXBUS, N24, J                      M38. 183
 9004 FORMAT ( 40H EXIT "NAMEA6".  TEXT1, MAXBUS, N24, J =,             M38. 184
     1           2X, A6, 3I10  )                                        M38. 185
      RETURN                                                            M38. 186
      END                                                               M38. 187
      SUBROUTINE TABLES                                                 M25.  96
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UTILITY WHICH IS USED TO BOTH DUMP AND RESTORE EMTP               M28. 288
C     TABLES (CENTRAL MEMORY VS. DISK).  USAGE IS FOR BOTH              M28. 289
C     "STATISTICS" (OVER12, OVER15, OVER20) AND  "START AGAIN"          M28. 290
C     (OVER1, OVER20).   CALL TO  "TAPSAV"  DUMPS  /LABEL/ .            M28. 291
C     ALSO USED BY  $RESTART  REQUEST OF  "CIMAGE"  (CALLED             M29. 974
C     FROM  "LOOKIE"  WHICH IS CALLED BY  "SUBTS3"  OF  OV16).          M29. 975
C     ALSO USED BY  "RESTORE"  REQUEST OF RTM, WHERE TABLE              M30. 111
C     RESTORATION IS IN  "KATALG"  OF OVERLAY 20.                       M37. 977
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      COMMON  /COMLOCK/  LOCKER(2)                                              
C     NOTE ABOUT DECK "SYNMAC".   IF EMTP S.M. MODELING                 M38. 188
C     BRANDWAJN (TYPE-59), IS TO BE DELETED,                            M38. 189
C     THEN ALL S.M. SUBROUTINES ( SMDAT, SMOUT, SMPFIT,                 M38. 190
C     SMINT, UNCOR, PREMEC, ELEC, PAST,                                 M38. 191
C     UPDATE, INCREM ) ARE TO BE DESTROYED,  AND                        M38. 192
C     DECK "SYNMAC" IS TO BE REMOVED FROM THE PRESENT MODULE.           M31. 167
C     BUT THEN  "DIMENSION Z(1)"  SHOULD BE ADDED IN ITS                M39.  69
C     PLACE,  AND THE  "N4 ="  CALCULATION INVOLVING "LOCINT"           M31. 169
C     SHOULD BE REPLACED BY THE SIMPLE STATEMENTS  "N4 = 1" .           M31. 170
C     COMMENT CARD IMMEDIATELY PRECEDING "SYNMAC" -------------         M31. 171
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
C     COMMENT CARD IMMEDIATELY FOLLOWING "SYNMAC" -------------         M31. 172
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      DIMENSION  INTEGX(1)                                              M39.  70
      EQUIVALENCE  ( X(1), INTEGX(1) )                                  M39.  71
      DIMENSION  BUSONE(1), IDISTX(1)                                   M33. 108
      EQUIVALENCE  ( BUS1, BUSONE(1) ), ( NENERG, IDISTX(1) )           M37. 978
      DIMENSION  KPEN(1), ITEMP(1),  JTEMP(1),  KTEMP(1)                M31. 174
      EQUIVALENCE ( KPEN(1), BUS1 ),  ( ITEMP(1), BUSUM(1) )            M28. 305
      EQUIVALENCE  ( JTEMP(1), ETAC(1) ),  ( KTEMP(1), Z(1) )           M39.  72
      EQUIVALENCE  ( MONCAR(2), KBASE )                                         
      DIMENSION IPRSAV(4)                                                       
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M42.1016
      DATA  IPRSAV   / 0,0,0,0 /                                        M42.1017
      LL1 = 1                                                           M25. 101
      LL2 = 2                                                           M25. 102
      LL4 = 4                                                           M42.1018
      NWORD1 = LOCINT( VOLTBC(1) )  -  LOCINT( KPEN(1) )                M31. 176
      NWORD2 = LOCINT(IDISTX(1)) - LOCINT(LUNSAV(15))                   M39.  73
      N4 = LOCINT( MSMOUT ) -  LOCINT( Z(1) )  +  1                     M39.  74
      N5 = LOCINT(LBSTAC)  -  LOCINT(ETAC(1))  +  1                     M38. 195
      IF ( KBASE  .EQ.  0 )                                             M28. 309
     1 NWORD1 = LOCINT(IDISTX(1)) - LOCINT(BUSONE(1))                   M28. 310
      N9 = LOCINT(ISTART)  -  LOCINT(BUSUM(1))  +  1                    M28. 311
      REWIND LUNIT2                                                     M39.  75
      IF ( NCHAIN  .EQ.  1 )   GO TO 3289                               M29. 976
      IF ( NCHAIN  .EQ.  20 )  GO TO 3289                               M29. 977
 3289 IF ( IPRSUP  .GE.  1 )                                            M29. 978
     1 WRITE (LUNIT6, 2721) N4, N5, NWORD1, NWORD2, LTLABL, N9,         M31. 179
     2  NCHAIN, LASTOV, LUNIT2, T                                       M25. 109
 2721 FORMAT ( /,  19H WITHIN  "TABLES" .,                              M25. 110
     1  48H      N4      N5  NWORD1  NWORD2  LTLABL      N9,            M31. 180
     2  24H  NCHAIN  LASTOV  LUNIT2,  14X,  1HT  ,/,                    M25. 112
     3  19X,  9I8,  E15.6  )                                            M31. 181
      IF ( NCHAIN  .EQ.  1 )        GO TO 5342                          M28. 317
      IF ( MEMSAV  .EQ.  1016 )     GO TO 5342                          M30. 114
      IF ( NCHAIN  .LT.  LASTOV )   GO TO 5342                          M25. 114
      WRITE (LUNIT2)  LOCKER                                            M28. 318
      WRITE (LUNIT2)  (KPEN(I), I = 1, NWORD1)                          M28. 319
      WRITE (LUNIT2)  (IPRSOV(I), I = 35, NWORD2)                       M39.  76
C     STORE IPRSOV(16-19) IN IPRSAV AT 1ST CALL TO TABLES FROM OVER12   M42.1019
      CALL MOVE (IPRSOV(16), IPRSAV(1), LL4)                            M42.1020
      CALL TAPSAV ( INTEGX(1), LUNIT2, LTLABL, LL1 )                    M25. 117
      IF ( NUMSM  .NE.  0 )                                             M30. 115
     1 WRITE (LUNIT2)  (KTEMP(I), I=1, N4), (JTEMP(I), I=1, N5)         M31. 182
      WRITE (LUNIT2)  ( ITEMP(I),  I=1, N9 )                            M28. 320
      GO TO 5359                                                        M25. 120
 5342 READ  (LUNIT2)  LOCKER                                            M28. 323
      IF ( IPRSUP .GE. 9 )                                              M37. 982
     1 WRITE (LUNIT6, 66) LOCKER                                        M37. 983
   66 FORMAT ( ' AFTER 1ST READ.  LOCKER =', 2I8  )                     M37. 984
      N3 = NCHAIN                                                       M28. 324
      N2 = IPRSUP                                                       M32. 621
      N24 = NUMDCD                                                              
      READ  (LUNIT2)  (KPEN(I), I = 1, NWORD1)                          M28. 325
      READ  (LUNIT2)  (IPRSOV(I), I = 35, NWORD2)                       M39.  77
      CALL TAPSAV ( INTEGX(1), LUNIT2, LTLABL, LL2 )                    M25. 123
      IF ( IPRSUP .GE. 9 )                                              M37. 985
     1 WRITE (LUNIT6, 69) NUMSM,N4,N5,N9                                M37. 986
   69 FORMAT ( 32H AFTER TAPSAV: NUMSM,N4,N5,N9 = , 4I5)                M37. 987
      IF ( NUMSM  .NE.  0 )                                             M30. 117
     1 READ (LUNIT2)  (KTEMP(I), I=1, N4), (JTEMP(I), I=1, N5)          M31. 183
      READ (LUNIT2)  ( ITEMP(I),  I=1, N9 )                             M28. 326
      NCHAIN = N3                                                       M28. 329
      IPRSUP = N2                                                       M32. 622
      NUMDCD = N24                                                              
C     RESTORE ALTERNATE TIME-STEP LOOP DIAGNOSTIC PRINTOUT              M42.1021
C     REQUEST FOR THE FIRST ENERGIZATION IN A STATISTICS CASE           M42.1022
      DO 1473  J = 1, 4                                                 M42.1023
      IF ( IPRSOV(J+15) .NE. IPRSAV(J) )  GO TO 1482                    M42.1024
 1473 CONTINUE                                                          M42.1025
      GO TO 5359                                                        M42.1026
 1482 DO 1487  J = 1, 4                                                 M42.1027
      N1 = IPRSOV(J+15)                                                 M42.1028
      IPRSOV(J+15) = IPRSAV(J)                                          M42.1029
      IPRSOV(J+30) = N1                                                 M42.1030
 1487 CONTINUE                                                          M42.1031
 5359 IF ( IPRSUP .GE. 1 )                                              M37. 988
     1 WRITE (LUNIT6, 5364)                                             M37. 989
 5364 FORMAT (  15H EXIT "TABLES".  )                                   M37. 990
      RETURN                                                            M37. 991
      END                                                               M25. 128
      SUBROUTINE CSUP( L )                                              M39.  78
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      DIMENSION ARG( 50), ACC( 20), AMX( 20)                            M28. 331
      DIMENSION IOP( 50), IFL( 20), IDN( 20)                            M28. 332
C     000  B = THE ARGUMENT AND LATER THE VALUE OF THE FUNCTION,        M14. 232
C     000      BEFORE IT IS AFFECTED BY THE ALGEBRAIC OPERATION         M14. 233
C     000      WHICH WILL UPDATE 'A' .                                  M14. 234
C     000  A = INTERMEDIATE VALUES OF THE SUPPLEMENTAL                  M14. 235
C     000      VARIABLE OR DEVICE .                                     M14. 236
C     000  XTCS( L) = THE FINAL VALUE OF THE VARIABLE OR DEVICE .       M14. 237
      KJSUP = KINSUP + LSTAT(65)                                        M39.  79
      KKSUP = KJSUP  + LSTAT(65)                                        M39.  80
 2001 IF ( IPRSUP .LT. 6 )  GO TO 1000                                  M28. 333
      WRITE (LUNIT6,1001) T, NSUP, KARG, KPAR                           M39.  81
 1001 FORMAT (34H0ENTERING SUBROUTINE  CSUP  AT  T=, E13.6     ,/,      M28. 334
     1 8H0E NSUP=, I6, 8H   KARG=, I8,  8H   KPAR=, I6 )                M39.  82
      WRITE (LUNIT6,1002) ( I,  ILNTAB(I+KSPVAR),                       M39.  83
     1  INSUP(I+KJSUP), INSUP(I+KKSUP), I = 1, NSUP )                   M39.  84
 1002 FORMAT ( 32H  NUMBER  SUPVAR    JSUP    KSUP ,/, (4I8) )          M39.  85
      WRITE ( LUNIT6, 1033 ) KARG                                       M39.  86
 1033 FORMAT ( 9H  KARG = , I8 ,/, 24H       N  IOPSUP  IFNSUP,         M39.  87
     1  48H  IRGSUP    IDEV     KDJ     KDK  ILDEV1  ILDEV2  )          M39.  88
      DO 2034  I = 1, NSUP                                              M39.  89
      N1 = INSUP( KJSUP + I )                                           M39.  90
      IF (  N1  .LT.  0 )  GO TO 2014                                   M39.  91
      N2 = INSUP( KKSUP + I )                                           M39.  92
      WRITE (LUNIT6,2008) (N,IVARB(N+1),IVARB(N+2),                     M39.  93
     2 IVARB(N+2), N = N1, N2, 3)                                       M39.  94
 2008 FORMAT ( 4I8 )                                                    M39.  95
      GO TO 2034                                                        M39.  96
 2014 N1 = -N1                                                          M39.  97
      WRITE (LUNIT6, 2022 ) N1, IVARB(N1), IVARB(N1+1),                 M39.  98
     1 IVARB(N1+2), IVARB(N1+3), IVARB(N1+4)                            M39.  99
 2022 FORMAT ( I8, 24X, 5I8 )                                           M39. 100
 2034 CONTINUE                                                          M39. 101
      IF ( KPAR .NE. 0 )                                                M22. 766
     1      WRITE (LUNIT6,1004) ( I, PARSUP(I+KPRSUP), I=1, KPAR )      M37. 997
 1004 FORMAT ( 2H0E, 5X, 10HPARSUP ...            ,/,                   M28. 341
     1 (3H E , 5(I3,  1X, E15.6, 3X)))                                  M28. 342
 1000 NNN = KXTCS + NUK + LSTAT(64)                                     M39. 102
      I = L                                                             M39. 103
 1234 A = 0.0                                                           M39. 104
      N1 = INSUP( KJSUP + I )                                           M39. 105
      N2 = INSUP( KKSUP + I )                                           M39. 106
      IF ( N1 .LT. 0 )  GO TO 600                                       M39. 107
      IF ( N2 .LT. 0    )  GO TO 5000                                   M28. 348
C     ------  OLD TACS PSEUDO-FORTRAN "VARIABLE"  ------                M28. 350
      NOP = 0                                                           M28. 351
      K = N1 - 3                                                        M39. 108
   20 K = K + 3                                                         M39. 109
      IF ( K .GT. N2 )  GO TO 11                                        M28. 354
      M  = IVARB( K + 2 )                                               M39. 110
      M1 = IVARB( K + 3 )                                               M39. 111
      M2 = IVARB( K + 1 )                                               M39. 112
      IF ( M2 .GT. 0 )  B = XTCS( KXTCS + M1 )                          M39. 113
      IF ( M2 .LT. 0 )  B = PARSUP( KPRSUP + M1 )                       M39. 114
      IF ( M .GT. 10 )  GO TO 30                                        M14. 279
      IF ( M .LE. 0  .OR.  M .GT. 5 )  GO TO 100                        M14. 280
      GO TO ( 101, 102, 103, 104, 105), M                               M14. 281
   30 IF ( M .GT. 20 )  GO TO 40                                        M14. 282
      M = M - 10                                                        M14. 283
      GO TO (111,112,113,114,115,116,117,118,119,120), M                M14. 284
   40 IF ( M .GT. 30 )  GO TO 50                                        M14. 285
      M = M - 20                                                        M14. 286
      GO TO (121,122,123,124,125,126,127,128,129,130), M                M14. 287
   50 IF ( M .GT. 40 )  GO TO 60                                        M28. 355
      M = M - 30                                                        M28. 356
      GO TO ( 131, 132, 133, 134 ), M                                   M38. 209
   60 CONTINUE                                                          M28. 358
  101 CONTINUE                                                          M14. 289
      IF ( A .GE. 1.0  .AND.  B .GE. 1.0 )  GO TO 99                    M14. 290
      GO TO 98                                                          M14. 291
  102 CONTINUE                                                          M14. 292
      IF ( A .GE. 1.0  .OR.  B .GE. 1.0 )  GO TO 99                     M14. 293
      GO TO 98                                                          M14. 294
  103 CONTINUE                                                          M14. 295
      IF ( B .LT. 1.0 )  GO TO 99                                       M14. 296
      GO TO 98                                                          M14. 297
  104 CONTINUE                                                          M14. 298
      IF ( A .GE. 1.0  .AND.  B .GE. 1.0 )  GO TO 98                    M14. 299
      GO TO 99                                                          M14. 300
  105 CONTINUE                                                          M14. 301
      IF ( A .LT. 1.0  .AND.  B .LT. 1.0 )  GO TO 99                    M14. 302
      GO TO 98                                                          M14. 303
   99 A = 1.0                                                           M14. 304
      GO TO 20                                                          M14. 305
   98 A = 0.0                                                           M14. 306
      GO TO 20                                                          M14. 307
  111 B = SINZ( B)                                                      M18. 224
      GO TO 100                                                         M14. 309
  112 B = COSZ( B)                                                      M18. 225
      GO TO 100                                                         M14. 311
  113 B = TANZ( B)                                                      M18. 226
      GO TO 100                                                         M14. 313
  114 B = COTANZ( B)                                                    M18. 227
      GO TO 100                                                         M14. 315
  115 B = SINHZ( B)                                                     M18. 228
      GO TO 100                                                         M14. 317
  116 B = COSHZ( B)                                                     M18. 229
      GO TO 100                                                         M14. 319
  117 B = TANHZ( B)                                                     M18. 230
      GO TO 100                                                         M14. 321
  118 B = ASINZ( B)                                                     M18. 231
      GO TO 100                                                         M14. 323
  119 B = ACOSZ( B)                                                     M18. 232
      GO TO 100                                                         M14. 325
  120 B = ATANZ( B)                                                     M18. 233
      GO TO 100                                                         M14. 327
  121 B = EXPZ( B)                                                      M18. 234
      GO TO 100                                                         M14. 329
  122 B = ALOGZ( B)                                                     M18. 235
      GO TO 100                                                         M14. 331
  123 B = ALOG1Z( B)                                                    M18. 236
      GO TO 100                                                         M14. 333
  124 B = SQRTZ( B)                                                     M18. 237
      GO TO 100                                                         M14. 335
  125 B = ABSZ( B)                                                      M18. 238
      GO TO 100                                                         M14. 337
  126 B = AINTZ( B)                                                     M18. 239
      GO TO 100                                                         M14. 339
  127 B = - B                                                           M14. 340
      GO TO 100                                                         M14. 341
  128 D7 = 1.0                                                          M23. 184
      D8 = B                                                            M23. 185
      ASSIGN 7128 TO IDIV                                               M14. 344
      GO TO 500                                                         M14. 345
 7128 B = DIV                                                           M14. 346
      GO TO 100                                                         M14. 347
  129 B = B * TWOPI / 360.0                                             M14. 348
      GO TO 100                                                         M14. 349
  130 B = B / TWOPI * 360.0                                             M14. 350
      GO TO 100                                                         M14. 351
  131 N5 = B                                                            M16. 313
      N6 = N5 / 6                                                       M16. 314
      IF ( N5  .LT.  0 )                                                M16. 315
     1 N5 = N5 + 6 * (N6+1)                                             M16. 316
      N7 = N5 - 6 * N6                                                  M16. 317
      IF ( N7  .EQ.  0 )   N7 = 6                                       M16. 318
      B = N7                                                            M16. 319
      GO TO 100                                                         M16. 320
  132 D7 = B                                                            M28. 359
      B = 1.0                                                           M28. 360
      IF ( D7 .LT. 0.0 )  B = -1.0                                      M28. 361
      GO TO 100                                                         M28. 362
  133 D7 = B                                                            M28. 363
      B = 1.0                                                           M28. 364
      IF ( D7 .GT. 10.0 * FLZERO )  B = 0.0                             M28. 365
      GO TO 100                                                         M28. 366
  134 D2 = 0.0                                                          M38. 210
      B = RANDNM(D2)                                                    M38. 211
  100 CONTINUE                                                          M28. 367
      IF ( NOP .GT. 0 )  GO TO 6113                                     M28. 368
      M = IABS( M2)                                                     M14. 353
      GO TO ( 201, 202, 203, 204, 205), M                               M14. 354
  201 A = A + B                                                         M14. 355
      GO TO 20                                                          M14. 356
  202 A = A - B                                                         M14. 357
      GO TO 20                                                          M14. 358
  203 A = A * B                                                         M14. 359
      GO TO 20                                                          M14. 360
  204 D7 = A                                                            M23. 186
      D8 = B                                                            M23. 187
      ASSIGN 7204 TO IDIV                                               M14. 363
      GO TO 500                                                         M14. 364
 7204 A = DIV                                                           M14. 365
      GO TO 20                                                          M14. 366
  205 A = A ** B                                                        M14. 367
      GO TO 20                                                          M14. 368
  500 IF ( D7 .EQ. 0.0 )  GO TO 510                                     M23. 188
      IF ( D8 .EQ. 0.0 )  GO TO 520                                     M23. 189
      N = ALOG1Z( ABSZ(D7)) - ALOG1Z( ABSZ(D8))                         M23. 190
      IF ( N  .LT. -IUTY(KIUTY+11) )  GO TO 510                         M39. 115
      IF ( N  .GT.  IUTY(KIUTY+11) )  GO TO 520                         M39. 116
      DIV = D7 / D8                                                     M23. 191
      GO TO 530                                                         M14. 375
  520 DIV = FLTINF                                                      M14. 376
      IF ( D7 .LT. 0.0 )  DIV = - DIV                                   M23. 192
      IF ( D8 .LT. 0.0 )  DIV = - DIV                                   M23. 193
      GO TO 530                                                         M14. 379
  510 DIV = 0.0                                                         M14. 380
  530 GO TO IDIV,( 7128, 7204, 6120)                                    M28. 369
C     ------  FREE-FORMAT FORTRAN EXPRESSION  ------                    M28. 371
 5000 N2 = -N2                                                          M28. 372
C     :: LOAD IOP( NOP)  AND  ARG( NOP)                                 M28. 373
      K = 0                                                             M28. 374
      DO 5025  J = N1, N2, 3                                            M39. 117
        K = K + 1                                                       M28. 379
        IOP( K) = IVARB( J + 2 )                                        M39. 118
        ARG( K) = 0.0                                                   M28. 381
        I1 = IVARB( J + 1 ) + 2                                         M39. 119
        I2 = IVARB( J + 3 )                                             M39. 120
        GO TO ( 5010, 5025, 5015, 5020), I1                             M28. 384
C      :: NUMERICAL ARGUMENT                                            M28. 385
 5010   NDX4 = KPRSUP + I2                                              M28. 386
        ARG( K) = PARSUP( NDX4)                                         M28. 387
        GO TO 5025                                                      M28. 388
C      :: TACS VARIABLE                                                 M28. 389
 5015   NDX4 = KXTCS + I2                                               M28. 390
        ARG( K) = XTCS( NDX4)                                           M28. 391
        GO TO 5025                                                      M28. 392
C      :: FORTRAN TACS FUNCTION                                         M28. 393
 5020   ARG( K) = I2                                                    M28. 394
 5025 CONTINUE                                                          M28. 395
C    :::  CALCULATE VALUE OF FORTRAN EXPRESSION  :::                    M28. 397
      ZFL = 10.0 * FLZERO                                               M28. 398
      JFL = 1                                                           M28. 399
      IFL( 1) = K                                                       M28. 400
      NOP = K                                                           M28. 401
 6010 IDN( JFL) =  0                                                    M28. 402
      IDN(JFL+1) = 0                                                    M29. 981
 6015 ACC( JFL) =  0.0                                                  M28. 403
      AMX( JFL) = 1.0                                                   M28. 404
 6020 IF ( JFL .EQ. 0 )  GO TO 6200                                     M28. 405
      I1 = IFL( JFL)                                                    M28. 406
      IF ( I1 .EQ. 0 )  GO TO 6111                                      M28. 407
      K1 = IOP( I1)                                                     M28. 408
      IF ( K1 .EQ. 0 )  GO TO 6028                                      M28. 409
      IF ( K1 .GT. 7 )  GO TO 6025                                      M28. 410
      GO TO ( 6110,6121,6130,6121,6150,6121,6170), K1                   M28. 411
 6025 IF ( K1 .LT. 14 )  GO TO 6180                                     M28. 412
      K1 = K1 - 13                                                      M28. 413
      GO TO ( 6114, 6115, 6116, 6117, 6118), K1                         M28. 414
 6026 AMX( JFL) = 1.0                                                   M28. 415
 6027 IOP( I1) = 0                                                      M28. 416
 6028   IFL( JFL) = IFL( JFL) - 1                                       M28. 417
        GO TO 6020                                                      M28. 418
C                                     ::  K1 = (  ::                    M28. 420
 6110 IF (IDN( JFL) .NE. -1)  GO TO 6112                                M28. 421
 6111 JFL = JFL - 1                                                     M28. 422
      GO TO 6020                                                        M28. 423
 6112 B = ACC( JFL)                                                     M28. 424
      M = ARG( I1 - 1 )                                                 M28. 425
      IF ( M .NE. 0 )  GO TO 30                                         M28. 426
 6113   ARG( I1 - 1 ) = B                                               M28. 427
        JFL = JFL - 1                                                   M28. 428
        IOP( I1) = 0                                                    M28. 429
        GO TO 6020                                                      M28. 430
C                                      ::  K1 = *  ::                   M28. 432
 6114 AMX( JFL) = AMX( JFL) * ARG( I1)                                  M28. 433
      GO TO 6027                                                        M28. 434
C                                      ::  K1 = /  ::                   M28. 436
 6115 D7 = AMX( JFL)                                                    M28. 437
      D8 = ARG( I1)                                                     M28. 438
      ASSIGN 6120  TO  IDIV                                             M28. 439
      GO TO 500                                                         M28. 440
 6120 AMX( JFL) = DIV                                                   M28. 441
      GO TO 6027                                                        M28. 442
C                                       ::  K1 = **  ::                 M28. 444
 6116 I2 = I1                                                           M38. 213
 6776 I2 = I2 - 1                                                       M38. 214
      IF ( IOP( I2) .EQ. 0 )  GO TO 6776                                M38. 215
      IF ( IOP( I2) .EQ. 7 )  GO TO 6119                                M28. 447
      ARG( I2) = ARG( I2) ** ARG( I1)                                   M28. 448
      GO TO 6027                                                        M28. 449
 6119 I1 = I2                                                           M28. 450
      GO TO 6170                                                        M28. 451
C                                       ::  K1 = -  ::                  M28. 453
 6118 ARG( I1) = - ARG( I1)                                             M28. 454
C                                       ::  K1 = +  ::                  M28. 456
 6117 ACC( JFL) = ACC( JFL) + ARG( I1) * AMX( JFL)                      M28. 457
      GO TO 6026                                                        M28. 458
C                                       ::  K1 = NOT  ::                M28. 460
 6121 D7 = ARG( I1)                                                     M28. 461
      ARG( I1) = 1.0                                                    M28. 462
      IF ( D7.GT.ZFL )  ARG( I1) = 0.0                                  M28. 463
      IF ( K1 .EQ. 6 )  GO TO 6150                                      M28. 464
C                                       ::  K1 = OR  ::                 M28. 466
 6130 D7 = ARG( I1)                                                     M28. 467
      ARG( I1) = 0.0                                                    M28. 468
      IF ( D7.GT.ZFL .AND. AMX( JFL) .GT.ZFL )  ARG( I1) = 1.0          M28. 469
      D7 = ACC( JFL)                                                    M28. 470
      ACC( JFL) = 0.0                                                   M28. 471
      IF ( D7.GT.ZFL .OR. ARG( I1) .GT.ZFL)  ACC(JFL) = 1.0             M28. 472
      GO TO 6026                                                        M28. 473
C                                       ::  K1 = AND  ::                M28. 475
 6150 D7 = AMX( JFL)                                                    M28. 476
      AMX( JFL) = 0.0                                                   M28. 477
      IF ( D7.GT.ZFL .AND. ARG( I1) .GT.ZFL)  AMX( JFL) = 1.0           M28. 478
      GO TO 6027                                                        M28. 479
C                                       :: K1 = )                       M28. 481
 6170 JFL = JFL + 1                                                     M28. 482
      IFL( JFL) = I1 - 1                                                M28. 483
      IOP( I1) = 0                                                      M28. 484
      GO TO 6010                                                        M28. 485
C                                       :: K1 = .NN.                    M28. 487
 6180 IF ( IDN( JFL + 1 ) .EQ. -1 )  GO TO 6185                         M28. 488
        JFL = JFL + 1                                                   M28. 489
        IFL( JFL) = I1 - 1                                              M28. 490
        IDN( JFL) = -1                                                  M28. 491
        GO TO 6015                                                      M28. 492
 6185 IOP( I1) = 0                                                      M28. 493
      IDN( JFL + 1 ) = 0                                                M28. 494
      D7 = ACC( JFL)                                                    M28. 495
      D8 = ACC( JFL + 1 )                                               M28. 496
      ACC( JFL) = 0.0                                                   M28. 497
      I2 = K1 - 7                                                       M28. 498
      GO TO ( 6188, 6189, 6190, 6191, 6192, 6193), I2                   M28. 499
 6188 IF ( D8 .NE. D7 )  GO TO 6195                                     M28. 500
      GO TO 6020                                                        M28. 501
 6189 IF ( D8 .EQ. D7 )  GO TO 6195                                     M28. 502
      GO TO 6020                                                        M28. 503
 6190 IF ( D8 .LT. D7 )  GO TO 6195                                     M28. 504
      GO TO 6020                                                        M28. 505
 6191 IF ( D8 .LE. D7 )  GO TO 6195                                     M28. 506
      GO TO 6020                                                        M28. 507
 6192 IF ( D8 .GE. D7 )  GO TO 6195                                     M28. 508
      GO TO 6020                                                        M28. 509
 6193 IF ( D8 .GT. D7 )  GO TO 6195                                     M28. 510
      GO TO 6020                                                        M28. 511
 6195 ACC( JFL) = 1.0                                                   M28. 512
      GO TO 6020                                                        M28. 513
C     :: EXIT FORTRAN EXPRESSION                                        M28. 515
 6200 A = ACC( 1)                                                       M28. 516
      GO TO 11                                                          M28. 517
C     ------  DEVICES  START  HERE  ------                              M14. 384
  600 N1 = - N1                                                         M39. 121
      NN = IVARB(N1)                                                    M39. 122
      IF ( N2 .EQ. 60  .OR.  N2 .EQ. 61 )  GO TO 602                    M39. 123
      IF ( N2 .EQ. 63  .OR.  N2 .EQ. 67 )  GO TO 602                    M39. 124
      J = IVARB( N1 + 1 )                                               M39. 125
      K = IVARB( N1 + 2 )                                               M39. 126
      B = 0.0                                                           M14. 387
      DO 601 N= J, K                                                    M14. 388
      M = KXTCS + KSUS( KALKSU + N )                                    M39. 127
  601 B = B + XTCS(M) * KSUS( KKSUS + N )                               M39. 128
  602 IF ( N2 .GT. 67 )  GO TO 10                                       M39. 129
      N2 = N2 - 49                                                      M39. 130
      GO TO (650,651,651,653,654,655,656,657,658,659,                   M39. 131
     1       660,661,662,663,664,664,666,667), N2                       M39. 132
C     ---  FREQUENCY SENSORS  ---                                       M14. 393
  650 N = 1                                                             M14. 394
      IF ( B .LT. 0.0 )  N = -1                                         M14. 395
      M  = IVARB( N1 + 3 )                                              M39. 133
      D9 = PARSUP( NN + 1 )                                             M39. 134
      D  = PARSUP( NN + 2 )                                             M39. 135
      D7 = PARSUP( NN )                                                 M39. 136
      A = D7                                                            M23. 196
      IF ( M .NE. 0 )  GO TO 6501                                       M39. 137
      PARSUP(NN+1) = -1.0                                               M39. 138
      GO TO 6507                                                        M39. 139
 6501 IF ( N  .EQ.  M )  GO TO 6508                                     M39. 140
      IF ( D .EQ. 0.0 )  GO TO 6507                                     M39. 141
      IF ( B .NE. 0.0 )  GO TO 6502                                     M39. 142
      D8 = T                                                            M23. 197
      GO TO 6503                                                        M39. 143
 6502 D8 = T - DELTAT / ( 1.0 - D / B )                                 M39. 144
 6503 IF ( D9 .LT. 0.0 )  GO TO 6506                                    M39. 145
      G = ONEHAF / ( D8 - D9 )                                          M23. 200
      IF ( D7 .EQ. 0.0 )  GO TO 6505                                    M39. 146
      H = G / D7 - 1.0                                                  M23. 202
      IF ( H .LT. PARSUP(NN+3) )  GO TO 6505                            M39. 147
      IF ( IUTY(KIUTY+3) .EQ. 0 ) GO TO 6507                            M42.1032
      IUTY(KIUTY+3) = IUTY(KIUTY+3) - 1                                 M42.1033
      NDX1 = ILNTAB( KSPVAR + I )                                       M38. 218
      WRITE (LUNIT6, 6504)  TEXVEC(NDX1), T, D7, G                      M39. 148
 6504 FORMAT ( 5X, 35HWARNING.  ----  FREQUENCY SENSOR  ',  A6,  23H'  HM39. 149
     1AS ZERO CROSSING AT,   E15.6,  28H    SEC.   BUT NEW FREQUENCY ,/,M20. 387
     2 21X,   2HOF,   E13.4,   61H HZ   DIFFERS BY OVER FIFTY PERCENT FRM20. 388
     3OM THE OLD FREQUENCY OF,   E13.4,  19H HZ .    REJECT IT.     )   M20. 389
      GO TO 6507                                                        M39. 150
 6505 PARSUP(NN) = G                                                    M39. 151
      A = G                                                             M14. 421
 6506 PARSUP(NN+1) = D8                                                 M39. 152
 6507 IVARB(N1+3) = N                                                   M39. 153
 6508 NDX1 = KDEV2  -  N1                                               M39. 154
      PARSUP(NN+2) = B                                                  M39. 155
      GO TO 11                                                          M14. 425
C     ---  RELAYS AND LEVEL-TRIGGERS  ---                               M14. 426
  651 IF ( PARSUP(NN) .NE. 0.0 )  B = B * PARSUP(NN)                    M39. 156
      M = IVARB( N1 + 3 )                                               M39. 157
      N = IVARB( N1 + 4 )                                               M39. 158
      D9 = PARSUP( NN + 1 )                                             M39. 159
      D8 = ABSZ( PARSUP(NN+2) )                                         M39. 160
      IF ( M .NE. 0 )  D9 = D9 + XTCS( KXTCS + M )                      M38. 226
      D = 0.0                                                           M14. 433
      IF ( N .NE. 0 )  D = XTCS( KXTCS + N )                            M38. 227
      IF ( N2 .EQ. 2 )  D = ABSZ( D)                                    M18. 242
      IF ( D .GE. D9  .AND.  D8 .GT. 1.5 )  A = B                       M39. 161
      IF ( D .LT. D9  .AND.  D8 .LT. 1.5 )  A = B                       M39. 162
      GO TO 11                                                          M14. 438
C     ---  VARIABLE TRANSPORT DELAY  ---                                M28. 533
  653 N5 = PARSUP(NN)                                                   M39. 163
      N6 = PARSUP( NN + 2 )                                             M39. 164
      N7 = IVARB( N1 + 4 )                                              M39. 165
      D7 = PARSUP( NN + 1 )                                             M39. 166
      J = IVARB( N1 + 3 )                                               M39. 167
      NDX6 = KXTCS + J                                                  M28. 544
      IF ( J .NE. 0 )  D7 = D7 + XTCS( NDX6)                            M28. 545
      D7 = D7 / DELTAT                                                  M28. 546
65310 D8 = D7 + FLZERO * 10.                                            M28. 547
      D9 = D7 - FLZERO * 10.                                            M28. 548
      IF ( D8 .GE. 0.0 )  GO TO 65320                                   M28. 549
      IF ( IUTY(KIUTY+3) .EQ. 0 )  GO TO 65313                          M42.1034
      IUTY(KIUTY+3) = IUTY(KIUTY+3) - 1                                 M42.1035
      NDX6 = ILNTAB( KSPVAR + I )                                       M38. 230
      WRITE (LUNIT6, 65316)  TEXVEC( NDX6), T                           M38. 231
65316 FORMAT (5X, 47HWARNING.  ----  VALUE OF DELAY BECAME NEGATIVE ,   M28. 554
     1  5HFOR ', A6, 11H' AT TIME =, E14.6,                             M28. 555
     2  30H BUT LOWER LIMIT NALUE = 0.0 .,                              M28. 556
     3  / 21X, 34HTHIS MESSAGE WILL NOT BE REPEATED.  )                 M28. 557
65313 D7 = 0.0                                                          M28. 558
      GO TO 65310                                                       M28. 559
65320 IF ( D9 .GT. 0.0 )  GO TO 65330                                   M28. 560
      A = B                                                             M28. 561
      GO TO 11                                                          M37.1019
65330 M1 = 0                                                            M28. 563
      J = 0                                                             M28. 564
65350 J = J + 1                                                         M28. 565
      IF ( D9 .GT. J )  GO TO 65350                                     M28. 566
      IF ( J .LE. D8 )  M1 = 1                                          M28. 567
      IF ( J .LE. N6 )  GO TO 65360                                     M28. 568
      IF ( IUTY(KIUTY+2) .EQ. 0 )  GO TO 65353                          M42.1036
      IUTY(KIUTY+3) = IUTY(KIUTY+3) - 1                                 M42.1037
      NDX6 = ILNTAB( KSPVAR + I )                                       M38. 232
      D4 = DELTAT * N6                                                  M28. 572
      WRITE (LUNIT6, 65356) D4, TEXVEC( NDX6), T                        M38. 233
65356 FORMAT (5X, 40HWARNING.  ----  VALUE OF DELAY EXCEEDED ,          M28. 574
     1 21HMAX. DELAY VALUE OF ', E14.6, 7H' FOR ', A6,                  M28. 575
     2 11H' AT TIME =, E14.6,                                           M28. 576
     3 / 21X, 34HTHIS MESSAGE WILL NOT BE REPEATED. )                   M28. 577
65353 J = N6                                                            M28. 578
      M1 = 1                                                            M28. 579
65360 N4 = N7 - J                                                       M28. 580
      IF ( N4 .LT. N5 )  N4 = N4 + N6                                   M28. 581
      NDX6 = KPRSUP + N4                                                M39. 172
      A = PARSUP( NDX6)                                                 M39. 173
      IF ( M1 .EQ. 1 )  GO TO 11                                        M37.1020
      N3 = N4 + 1                                                       M28. 585
      IF ( N3 .GE.  N5 + N6  )  N3 = N3 - N6                            M28. 586
      D5 = B                                                            M28. 587
      NDX6 = KPRSUP + N3                                                M39. 174
      IF ( J .GT. 1 )  D5 = PARSUP( NDX6)                               M39. 175
      A = A - ( J - D7 ) * ( A - D5 )                                   M28. 590
      GO TO 11                                                          M28. 596
C     ---  PULSE VARIABLE TRANSPORT DELAY  ---                          M28. 598
  654 M = IVARB( N1 + 3 )                                               M39. 176
      D9 = PARSUP( NN + 1 )                                             M39. 177
      NDX1 = KXTCS  +  M                                                M22. 863
      IF ( M .NE. 0 )  D9 = D9 + XTCS(NDX1)                             M23. 220
      D = PARSUP(NN)                                                    M39. 178
      D7 = PARSUP( NN + 2 )                                             M39. 179
      IF ( D7 .EQ. -9999. )  GO TO 65400                                M23. 222
      NDX1 = ILNTAB( KSPVAR + I )                                       M38. 235
      IF ( B .LE. 0.0 )   GO TO 65402                                   M42.1038
      IF ( IUTY(KIUTY+3) .EQ. 0 ) GO TO 65402                           M42.1039
      IUTY(KIUTY+3) = IUTY(KIUTY+3) - 1                                 M42.1040
      WRITE (LUNIT6,65401)  TEXVEC(NDX1), D9, T                         M39. 180
65401 FORMAT (5X,  67HWARNING.  ----  THE PULSE FREQUENCY AT THE PULSE TM20. 396
     1RANSPORT DELAY  ',  A6, 30H'  IS TOO FAST FOR THE PRESENT    ,/,  M20. 397
     2 21X,   8HDELAY OF,  E13.4,  25H SEC   AT SIMULATION TIME,        M20. 398
     3 E13.4,  46H SEC. USE DEVICE TYPE 53 INSTEAD OF TYPE 54 . ,/,     M39. 181
     4 21X, 50H ******** THE ANSWER MAY BE WRONG LATER ********   )     M39. 182
      GO TO 65402                                                       M14. 484
65400 IF ( B .GT. 0.0  .AND.  D .EQ. -9999. ) PARSUP(NN) = T            M42.1041
      IF ( B .LE. 0.0  .AND.  D .NE. -9999. )  PARSUP(NN+2) = T         M42.1042
65402 IF ( T .LT. D+D9-10.*FLZERO  .OR.  D .EQ. -9999. )                M28. 602
     1 GO TO 11                                                         M28. 603
      IF ( T .GE. D7+D9-10.*FLZERO  .AND.  D7 .NE. -9999. )             M28. 604
     1 GO TO 65403                                                      M28. 605
      A = 1.0                                                           M14. 489
      GO TO 11                                                          M14. 490
65403 PARSUP(NN) = -9999.                                               M39. 185
      PARSUP(NN+2) = -9999.                                             M39. 186
      GO TO 11                                                          M14. 493
C     ---  DIGITIZER  ---                                               M14. 494
  655 M = IVARB( N1 + 3 )                                               M39. 187
      N = IVARB( N1 + 4 )                                               M39. 188
      IF ( PARSUP(NN) .NE. 0.0 )  B = B * PARSUP(NN)                    M39. 189
      NDX1 = KPRSUP +  M                                                M39. 190
      A = PARSUP(NDX1)                                                  M39. 191
      IF ( M .EQ. N )  GO TO 11                                         M14. 499
      J = M + 1                                                         M14. 500
      DO 65500 K= J, N                                                  M14. 501
      M = N - K + J                                                     M14. 502
      NDX1 = KPRSUP +  M                                                M39. 192
      IF ( B .GE. PARSUP(NDX1))  GO TO 65501                            M39. 193
65500 CONTINUE                                                          M14. 504
      GO TO 11                                                          M14. 505
65501 NDX1 = KPRSUP +  M                                                M39. 194
      A = PARSUP(NDX1)                                                  M39. 195
      GO TO 11                                                          M14. 507
C     ---  POINT-BY-POINT NON-LINEARITY  ---                            M14. 508
  656 M = IVARB( N1 + 3 )                                               M39. 196
      N = IVARB( N1 + 4 )                                               M39. 197
      IF ( PARSUP(NN) .NE. 0.0 )  B = B * PARSUP(NN)                    M39. 198
      NDX1 = KPRSUP +  M + 1                                            M39. 199
      A = PARSUP(NDX1)                                                  M39. 200
      IF ( N .LE. M+1 )  GO TO 11                                       M14. 513
      NDX1 = KPRSUP +  M                                                M39. 201
      IF ( B .LE. PARSUP(NDX1))  GO TO 11                               M39. 202
      J = M + 2                                                         M14. 515
      DO 65600 K=J,N,2                                                  M14. 516
      NDX1 = KPRSUP +  K                                                M39. 203
      IF ( B .LE. PARSUP(NDX1))  GO TO 65601                            M39. 204
65600 CONTINUE                                                          M14. 518
      NDX1 = KPRSUP +  N                                                M39. 205
      A = PARSUP(NDX1)                                                  M39. 206
      GO TO 11                                                          M14. 520
65601 NDX1 = KPRSUP + K+1                                               M39. 207
      NDX2 = KPRSUP + K-1                                               M39. 208
      NDX3 = KPRSUP + K                                                 M39. 209
      NDX4 = KPRSUP + K-2                                               M39. 210
      D9 = ( PARSUP(NDX1)-PARSUP(NDX2) )/( PARSUP(NDX3)-PARSUP(NDX4))   M39. 211
      A = PARSUP(NDX2) + D9 *( B - PARSUP(NDX4) )                       M39. 212
      GO TO 11                                                          M14. 523
C     ---  TIME - SEQUENCED  SWITCH  ---                                M15. 343
  657 M = IVARB( N1 + 3 )                                               M39. 213
      N = IVARB( N1 + 4 )                                               M39. 214
      N3 = PARSUP( NN + 1 )                                             M39. 215
      N4 = PARSUP( NN + 2 )                                             M39. 216
      IF ( N4 .EQ. N )  GO TO 65701                                     M15. 348
      IF ( N4 .EQ. 0 )  N4 = M - 1                                      M15. 349
      J = N4 + 1                                                        M15. 350
      NDX1 = KPRSUP +  J                                                M39. 217
      IF ( T .LT. PARSUP(NDX1) )  GO TO 65701                           M39. 218
      PARSUP(NN+2) = J                                                  M39. 219
      N3 = N3 + 1                                                       M15. 353
      IF ( N3 .EQ. 2 )  N3 = 0                                          M15. 354
      PARSUP(NN+1) = N3                                                 M39. 220
65701 IF ( N3 .EQ. 0 )  GO TO 11                                        M15. 356
      A = B                                                             M15. 357
      GO TO 11                                                          M15. 358
C     ---  CONTROLLED INTEGRATOR AND COUNTER  ---                       M28. 607
  658 IF ( IVARB(N1+3) .NE. -9999 )  GO TO 4658                         M39. 221
      NDX6 = NNN + I                                                    M39. 222
        A = XTCS( NDX6) + B                                             M28. 615
        GO TO 11                                                        M28. 616
 4658 N5 = IVARB( N1 + 3 )                                              M39. 223
      IF ( N5 .EQ. 0 )  GO TO 4721                                      M28. 618
      NDX6 = KXTCS + N5                                                 M28. 619
      IF ( XTCS( NDX6) .GT. 0.0 )  GO TO 4721                           M28. 620
      A = 0.0                                                           M28. 621
      PARSUP(NN) = 0.0                                                  M39. 224
      N6 = IVARB( N1 + 4 )                                              M39. 225
      IF ( N6 .EQ. 0 )  GO TO 11                                        M28. 624
      NDX6 = KXTCS + N6                                                 M28. 625
      A = XTCS( NDX6)                                                   M28. 626
      PARSUP(NN) = ( PARSUP(NN+1) - PARSUP(NN+2) ) / 2.0  * A           M39. 226
      GO TO 11                                                          M28. 628
 4721 A = ( B + PARSUP(NN) ) / PARSUP(NN+1)                             M39. 227
      PARSUP(NN) = B - PARSUP(NN+2) * A                                 M39. 228
      GO TO 11                                                          M28. 631
C     ---  SIMPLE  DERIVATIVE  ---                                      M28. 633
  659 A = ( B - PARSUP(NN+1) )  *  PARSUP(NN)                           M39. 229
      PARSUP(NN+1) = B                                                  M39. 230
      GO TO 11                                                          M28. 638
C     ---  INPUT  IF - BLOCK  ---                                       M28. 640
  660 D7 = PARSUP(NN)                                                   M39. 231
      N3 = IVARB( N1 + 3 )                                              M39. 232
      N4 = IVARB( N1 + 4 )                                              M39. 233
      NDX1 = KXTCS + N4                                                 M28. 647
      IF ( N4 .NE. 0 )  D7 = D7 + XTCS( NDX1)                           M28. 648
      NDX1 = KXTCS + N3                                                 M28. 649
      D7 = XTCS( NDX1) - D7                                             M28. 650
      N = 2                                                             M39. 234
      IF ( D7 .GT. -FLZERO )  N = 1                                     M28. 652
      IF ( D7 .GT. +FLZERO )  N = 0                                     M39. 235
      J = IVARB(N1+1) + N                                               M39. 236
      NDX2 = KALKSU + J                                                 M37.1021
      NDX3 = KKSUS + J                                                  M37.1022
      NDX4 = KXTCS + KSUS( NDX2)                                        M38. 248
      A = XTCS( NDX4)  *  KSUS( NDX3)                                   M37.1024
      GO TO 11                                                          M28. 660
C     ---  INPUT  SIGNAL  SELECTOR  ---                                 M28. 662
  661 NDX3 = KXTCS + IVARB( N1 + 3 )                                    M39. 237
      NDX4 = KXTCS + IVARB( N1 + 4 )                                    M39. 238
      D1 = XTCS( NDX4)                                                  M28. 669
      A = PARSUP( NN + 1 )                                              M39. 239
      IF ( D1 .LT. 0.5 )  GO TO 11                                      M28. 671
      A = PARSUP( NN + 2 )                                              M39. 240
      IF ( D1 .GE. 6.5 )  GO TO 11                                      M28. 673
      A = 0.0                                                           M28. 674
      IF ( D1 .LT. 5.5 )  GO TO 66110                                   M28. 675
        IF ( NDX3 .EQ. KXTCS )  GO TO 11                                M28. 676
        A = XTCS( NDX3)                                                 M28. 677
        GO TO 11                                                        M28. 678
66110 J = D1 - ONEHAF                                                   M39. 241
      J = IVARB(N1+2) - J                                               M39. 242
      NDX1 = KALKSU + J                                                 M37.1025
      M = KSUS( NDX1)                                                   M38. 251
      IF ( M .EQ. 0 )  GO TO 11                                         M28. 684
      NDX1 = KXTCS + M                                                  M28. 685
      NDX2 = KKSUS + J                                                  M37.1027
      A = XTCS( NDX1)  *  KSUS( NDX2)                                   M37.1028
      GO TO 11                                                          M28. 688
C     ---  TRACK  AND  SAMPLE  ---                                      M28. 690
  662 A = PARSUP( NN + 2 )                                              M39. 243
      N = IVARB( N1 + 3 )                                               M39. 244
      IF ( N .EQ. 0 )  GO TO 66210                                      M28. 695
      NDX2 = KXTCS + N                                                  M28. 696
      IF ( XTCS( NDX2) .GT. FLZERO )  A = B                             M28. 697
66210 N = IVARB( N1 + 4 )                                               M39. 245
      M = 0                                                             M28. 701
      IF ( N .EQ. 0 )  GO TO 66220                                      M28. 702
      NDX3 = KXTCS + N                                                  M28. 703
      IF ( XTCS( NDX3) .GT. FLZERO )  M = 1                             M28. 704
66220 IF ( PARSUP(NN) .EQ. 1.0  .OR.  M .EQ. 0 )  GO TO 66230           M39. 246
      A = B                                                             M28. 706
      PARSUP(NN) = 1.0                                                  M39. 247
      GO TO 66240                                                       M28. 708
66230 IF ( PARSUP(NN) .EQ. 1.0  .AND.  M .EQ. 0 )                       M39. 248
     1  PARSUP(NN) = 0.0                                                M39. 249
66240 PARSUP(NN+2) = A                                                  M39. 250
      GO TO 11                                                          M28. 712
C     ---  INSTANTANEOUS  MIN/MAX  ---                                  M28. 714
  663 NDX1 = KALKSU + IVARB( N1 + 1 )                                   M39. 251
      K = 2                                                             M28. 717
66310 IF ( KSUS( NDX1) .GT. 0 )  GO TO 66320                            M38. 254
      NDX1 = NDX1 + 1                                                   M28. 719
      K = K + 1                                                         M28. 720
      GO TO 66310                                                       M28. 721
66320 NDX4 = KXTCS + KSUS(NDX1)                                         M38. 255
      NDX2 = NDX1 - LSTAT(63)                                           M39. 252
      D11 = XTCS(NDX4) * KSUS(NDX2)                                     M39. 253
      D10 = D11                                                         M32. 632
      IF ( K .GT. 5 )  GO TO 66330                                      M28. 723
      NDX3 = NDX1                                                       M28. 724
      DO 66340  J = K, 5                                                M28. 725
        NDX3 = NDX3 + 1                                                 M28. 726
      NDX2 = NDX2 + 1                                                   M32. 633
        N = KSUS( NDX3)                                                 M38. 256
        IF ( N .EQ. 0 )  GO TO 66340                                    M28. 728
        NDX6 = KXTCS + N                                                M28. 731
      D4 = XTCS(NDX6) * KSUS(NDX2)                                      M37.1036
      IF ( D4 .LT. D10 )   D10 = D4                                     M32. 635
      IF ( D4 .GT. D11 )   D11 = D4                                     M32. 636
      IF ( IPRSUP  .GE.  1 )                                            M32. 637
     1 WRITE (LUNIT6, 7234)  K, J, NDX2, NDX3, NDX6, KSUS(NDX2),        M37.1037
     1                       XTCS(NDX6)                                 M32. 639
 7234 FORMAT ( 36H NEXT INPUT; K, J, NDX2, NDX3, NDX6,,                 M32. 640
     1         25H KSUS(NDX2), XTCS(NDX6) =,  6I8, E13.3  )             M37.1038
66340  CONTINUE                                                         M28. 734
66330 A = D11                                                           M32. 642
      IF ( PARSUP(NN+1) .GE. 0.0 )  GO TO 11                            M39. 254
      A = D10                                                           M32. 643
      GO TO 11                                                          M28. 741
C     --- MIN/MAX TRACKING, CONTROLLED ACCUMULATOR OR COUHTER ---       M39. 255
  664 NDX4 = KXTCS + IVARB( N1 + 3 )                                    M39. 256
      IF ( NDX4 .EQ. KXTCS )  GO TO 6641                                M39. 257
      IF ( XTCS( NDX4) .LE. FLZERO )  GO TO 6641                        M39. 258
      A = PARSUP( NN + 2 )                                              M39. 259
      GO TO 6643                                                        M39. 260
 6641 NDX5 = KXTCS + IVARB( N1 + 4 )                                    M39. 261
      IF ( NDX5 .EQ. KXTCS )  GO TO 6642                                M39. 262
      IF ( XTCS( NDX5) .LE. FLZERO )  GO TO 6642                        M39. 263
      A = PARSUP(NN)                                                    M39. 264
      GO TO 11                                                          M28. 758
 6642 IF ( N2 .EQ. 16 ) GO TO 665                                       M39. 265
      A = PARSUP(NN)                                                    M39. 266
      RDEV1 = PARSUP( NN + 1 )                                          M39. 267
      IF ( RDEV1 .EQ. -1.0  .AND.  B .LT. A )  A = B                    M39. 268
      IF ( RDEV1 .EQ. +1.0  .AND.  B .GT. A )  A = B                    M39. 269
      GO TO 6643                                                        M39. 270
  665 A = PARSUP(NN) + B                                                M39. 271
 6643 PARSUP(NN) = A                                                    M39. 272
      GO TO 11                                                          M39. 273
  666 IVARB(N1+4) = IVARB(N1+4) + 1                                     M39. 274
      K = IVARB( N1 + 3 )                                               M39. 275
      IF ( IVARB(N1+4) .GT. K ) IVARB(N1+4) = 1                         M39. 276
      NDX1 = NN + IVARB( N1 + 4 )                                       M39. 277
      PARSUP(NDX1) = B * B                                              M39. 278
      DO 6666 JI = 1, K                                                 M39. 279
 6666 A = A + PARSUP( NN + JI )                                         M39. 280
      A = SQRTZ( A * PARSUP(NN) )                                       M39. 281
      GO TO 11                                                          M39. 282
  667 J = IVARB( N1 + 1 )                                               M39. 283
      K = IVARB( N1 + 2 )                                               M39. 284
      B = 0.0                                                           M39. 285
      DO 1166 MJ = J, K                                                 M39. 286
      N = KKSUS + MJ                                                    M39. 287
      IF ( KSUS(N)  .EQ.  9 ) GO TO 1166                                M39. 288
      M = KALKSU + MJ                                                   M39. 289
      BB = XTCS( KXTCS + KSUS(M) ) * KSUS(N)                            M39. 290
      NJ = MJ                                                           M39. 291
 1144 NJ = NJ - 1                                                       M39. 292
      N = N - 1                                                         M39. 293
      M = M - 1                                                         M39. 294
      IF ( NJ  .LT.  J ) GO TO 1155                                     M39. 295
      IF ( KSUS(N)  .NE.  9 ) GO TO 1155                                M39. 296
      BB = BB * XTCS( KXTCS + KSUS(M) )                                 M39. 297
      GO TO 1144                                                        M39. 298
 1155 B = B + BB                                                        M39. 299
 1166 CONTINUE                                                          M39. 300
      A = B * PARSUP( NN + 3 )                                          M39. 301
      AA = A * PARSUP( NN )                                             M39. 302
      IF ( AA .GE. PARSUP(NN+1) ) GO TO 6677                            M39. 303
      A = PARSUP(NN+1) / PARSUP(NN)                                     M39. 304
      GO TO 11                                                          M39. 305
 6677 IF ( AA .LE. PARSUP(NN+2) ) GO TO 11                              M39. 306
      A = PARSUP(NN+2) / PARSUP(NN)                                     M39. 307
   11 NDX1 = NNN + I                                                    M39. 308
      XTCS(NDX1) = A                                                    M22. 937
   10 I = INSUP(KINSUP+I)                                               M39. 309
      IF ( I .GT. 0 ) GO TO 1234                                        M37.1046
 9999 RETURN                                                            M37.1047
      END                                                               M14. 527
