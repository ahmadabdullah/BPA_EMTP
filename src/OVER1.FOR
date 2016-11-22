      SUBROUTINE OVER1 
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR/NOLIST'    ! WSM + THL
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'LABL02.FOR;/NOLIST'                                           
C      %INCLUDE  '//c/tsu/cables.INS.FTN'                               M43.   5
C     TO AVOID "INSERT DECK TACSAR" HERE, USE SMALL PART OF IT:         M31. 190
      REAL*4  R4(1)                                                     M43.   6
      EQUIVALENCE  ( VOLTI(1), R4(1) )                                  M43.   7
      DIMENSION  KPEN(1), IBUSUM(1)                                     M28. 784
      EQUIVALENCE  ( BUS1, KPEN(1) ),  ( BUSUM(1), IBUSUM(1) )          M28. 785
      EQUIVALENCE  (MONCAR(1),    KNT),   (MONCAR(2),  KBASE)           M37.1048
      EQUIVALENCE                         (MONCAR(4),    ISW)           M37.1049
      EQUIVALENCE  (MONCAR(5),  IDIST),   (MONCAR(6),  ITEST)           M37.1050
      EQUIVALENCE                         (MONCAR(8), JSEEDR)           M37.1051
      EQUIVALENCE  (MONCAR(9), KLOAEP),   (MONCAR(10), MTAPE)           M37.1052
      EQUIVALENCE (IPRSOV(39), NMAUTO)                                  M42.1046
      REAL*8         AUPPER, TEXT1, TEXT2, TEXT6, DATEXX(2)             M42.1047
      REAL*8         TEXT3,  TEXT4, TEXT5, TEXT9, TCLOXX(2)             M33. 112
      DIMENSION      AUPPER(14)                                         M28. 788
      DIMENSION LSTACS(8)                                               M39. 310
      character*80  disk_file                                           M43.   8
      CHARACTER*132  ANSI132                                            M42.1048
      COMMON  /COMLOCK/  LOCKER(2)                                              
C     DEFAULT LIST SIZES FOR TACS PROPORTIONING OF EMTP LIST 19.        M22. 943
      DATA  TEXT2   /  6HNAME    /                                      M31. 193
      DATA  TEXT6   /  6HCOPY    /                                      M31. 194
      DATA  TEXT1   /  6HTACS O  /                                      M22. 959
      DATA  TEXT3   /  6HTACS H  /                                      M28. 790
      DATA  TEXT4   /  6HTACS S  /                                      M28. 791
      DATA  TEXT5   /  6H4       /                                      M30. 120
      data  text6   /  6hTACS    /
      DATA LL1  /  1 /                                                  M23. 292
      DATA LL6  /  6 /                                                  M32. 647
      DATA LL8  /  8 /                                                  M23. 294
      DATA LL11 / 11 /                                                  M32. 648
      DATA LL20 / 20 /                                                  M23. 296
      DATA LL24 / 24 /                                                  M23. 297
      DATA LL25 / 25 /                                                  M23. 298
      DATA LL30 / 30 /                                                  M23. 299
      DATA LL40 / 40 /                                                  M24. 318
      DATA LL60 / 60 /                                                  M39. 311
      DATA LL64 / 64 /                                                  M23. 306
      DATA LL80 / 80 /                                                  M23. 307
      IF ( IPRSUP  .GE.  1 )                                            M32. 649
     1 WRITE ( LUNIT6, 4567 )                                           M32. 650
 4567 FORMAT ( 23H  "BEGIN MODULE OVER1." )                             M32. 651
      LSTACS(1)  =  20                                                  M38. 263
      LSTACS(2)  =  90                                                  M39. 312
      LSTACS(3)  = 100                                                  M38. 265
      LSTACS(4)  =  20                                                  M38. 266
      LSTACS(5)  =  30                                                  M38. 267
      LSTACS(6)  = 250                                                  M39. 313
      LSTACS(7)  = 350                                                  M39. 314
      LSTACS(8)  =  60                                                  M39. 315
      PU = FLTINF                                                       M42.1050
      CALL MOVE0 ( KTRLSW(1), LL6 )                                     M32. 652
      CALL MOVE0 ( IPNTV(1), LL11 )                                     M32. 653
      CALL MOVE0 ( LOOPSS(1), LL11 )                                            
      MONCAR(7) = 0                                                             
      NMAUTO = 0                                                                
      BUSUM(1) = TEXT2                                                          
      NRIGHT = 0                                                        M37.1057
      IOFGND = 0                                                                
      MOLDAT = 0                                                        M28. 793
      ISTEP = 0                                                         M20. 420
      KLOAEP = 0                                                        M20. 421
      TENERG = 1.E+20                                                   M25. 155
      BRANCH = TEXT2                                                    M31. 196
      COPY   = TEXT6                                                    M31. 197
      IALTER = 0                                                        M39. 317
      ISPRIN = 0                                                        M20. 422
      ISPLOT = 0                                                        M20. 423
      INDSTP = 1                                                        M20. 424
      NOUTPR = 0                                                        M20. 425
      LUNIT1 = 1                                                        M15. 367
      LUNIT2 = 2                                                        M15. 368
      LUNIT3 = 3                                                        M15. 369
      LUNIT4 = 4                                                        M15. 370
      LUNIT5 = 5                                                        M15. 371
      LUNIT6 = 6                                                        M15. 372
      LUNIT7 = 7                                                        M15. 373
      LUNIT8 = 8                                                        M15. 374
      LUNIT9 = 9                                                        M20. 446
      LUNT10 = 10                                                       M20. 447
      LUNT11 = 11                                                       M20. 448
      LUNT12 = 12                                                       M20. 449
      LUNT13 = 13                                                       M20. 450
      LUNT14 = 14                                                       M20. 451
      LUNT15 = 15                                                       M20. 452
      SPEEDL = 2.997925 D8                                              M16. 331
      PEAKND(1) = 0.0                                                   M25. 156
      KBURRO = 0                                                        M22.1188
C     ASSIGN DEFAULT RELATIVE PRECISION FOR 6 EMTP VARIABLE TYPES.      M35. 465
C        1 --- ALPHANUMERIC              2 --- COMPLEX                  M22.1190
C        3 --- FLOATING-POINT NUMERIC    4 --- INTEGER NUMERIC          M22.1191
C        5 --- 3 OF 'SUBR31'             6 --- 4 OF 'SUBR31'            M22.1192
C     SETTING ALL EQUAL TO UNITY MEANS THAT ALL HAVE EQUAL LENGTH.      M22.1193
      DO 7831  J=1, 6                                                   M22.1194
 7831 NBYTE(J) = 1                                                      M22.1195
      CALL DIMENS ( LSTAT(1), NCHAIN, BUS1, BUS2 )                      M22.1196
      LBUS   = LSTAT( 1)                                                M22.1197
      LBRNCH = LSTAT( 2)                                                M22.1198
      LDATA  = LSTAT( 3)                                                M22.1199
      LEXCT  = LSTAT( 4)                                                M22.1200
      LYMAT  = LSTAT( 5)                                                M22.1201
      LSWTCH = LSTAT( 6)                                                M22.1202
      LSIZE7 = LSTAT( 7)                                                        
      LPAST  = LSTAT( 8)                                                M22.1204
      LNONL  = LSTAT( 9)                                                M22.1205
      LCHAR  = LSTAT(10)                                                M22.1206
      LSMOUT = LSTAT(11)                                                        
      LSIZ12 = LSTAT(12)                                                        
      LFDEP  = LSTAT(13)                                                M22.1209
      LWT    = LSTAT(14)                                                M22.1210
      LTAILS = LSTAT(15)                                                M22.1211
      LIMASS = LSTAT(16)                                                        
      LSYN   = LSTAT(17)                                                        
      MAXPE  = LSTAT(18)                                                M22.1214
      LTACST = LSTAT(19)                                                M22.1215
      LFSEM  = LSTAT(20)                                                M31. 199
      LFD    = LSTAT(21)                                                M22.1217
      LHIST  = LSTAT(22)                                                M22.1218
      LSIZ23 = LSTAT(23)                                                M22.1219
      LCOMP  = LSTAT(24)                                                M25. 157
      LSPCUM = LSTAT(25)                                                M27. 556
      LSIZ26 = LSTAT(26)                                                M31. 200
      LSIZ27 = LSTAT(27)                                                M32. 656
      LSIZ28 = LSTAT(28)                                                M32. 656
C     ASSIGN  'N1'  EQUAL TO THE NUMBER OF EMTP LISTS OF VARIABLE       M22.1220
C     DIMENSIONING (OF THE SOLUTION OVERLAYS).                          M22.1221
      N1 = 28                                                           M32. 657
      LTLABL = LSTAT(N1+1)                                              M22.1223
      LOCKER(1) = BUS1                                                  M42.1053
      LOCKER(2) = BUS2                                                  M42.1054
C      write (*,*) ' Save  LOCKER =',  LOCKER                           M42.1055
      D13 = LTACST                                                              
      D13 = D13 / 1600.                                                         
      DO 1108  J=1, 8                                                           
 1108 LSTACS(J) = LSTACS(J) * D13                                               
      N1 = -9999                                                        M22.1224
      CALL COPYI ( N1, LSTAT(1), LL60 )                                 M39. 319
      CALL SYSDEP                                                       M42.1056
      CALL MOVER0 ( FLSTAT(1), LL20 )                                   M22.1226
      CALL RUNTYM ( D1, D2 )                                            M22.1227
      FLSTAT(1) = FLSTAT(1) - D1                                        M22.1228
      FLSTAT(2) = FLSTAT(2) - D2                                        M22.1229
      TENM6 = TENM3**2                                                  M15. 376
      MAXZNO = 50                                                               
      EPSZNO = EPSILN                                                           
      EPWARN = TENM3                                                            
      EPSTOP = 0.1                                                              
      ZNOLIM(1) = 1.0                                                           
      ZNOLIM(2) = 1.5                                                           
      KBASE = 0                                                         M13. 288
      ISW = 0                                                           M21.  12
      KPRCHG(1) = -7777                                                 M23. 308
      JFLSOS = 0                                                        M23. 309
      REWIND LUNIT1                                                     M20. 453
      REWIND LUNIT3                                                     M22.1230
      REWIND LUNIT4                                                     M17. 107
      KNT=1                                                             M42.1057
      MTAPE = LUNIT5                                                    M20. 454
      IFDEP = 0                                                         M37.1069
      NTCSEX = 0                                                        M20. 458
      NSTACS = 0                                                        M20. 459
      KANAL = 0                                                         M20. 460
C     INITIALIZE "KPARTB" WITH NONZERO VALUE SO LATER MINUS             M32. 658
C     SIGN CAN BE APPLIED AS FLAG OF "RENUMBER BYPASS" USAGE:           M32. 659
      KPARTB = 1000                                                     M32. 660
C     "SYSDEP" WILL REDEFINE IF LOW-PREC. COMPLEX ("CABLE CONSTANTS"):  M35. 466
      ZNVREF = 0.0                                                      M35. 467
      FMAXFS = 0.0                                                      M20. 462
      BEGMAX(1) = 0.0                                                   M31. 201
      BEGMAX(2) = 0.0                                                   M31. 202
      BEGMAX(3) = FLTINF                                                M31. 203
      KTREF = 0                                                         M20. 463
      NCOMP = 0                                                         M20. 466
      NUMSM = 0                                                         M20. 467
      NUMUM = 0                                                         M27. 558
      NUMOUT = 0                                                        M27. 559
      KILL = 0                                                          M20. 468
      NUM99 = 0                                                         M20. 469
      MODOUT = 0                                                        M20. 471
      INECHO = 0                                                        M20. 474
      BUS(1) = BLANK                                                    M42.1058
      IAVERG = 0                                                        M22.1233
      CALL MOVE0 ( ISOURC(1), LSWTCH )                                  M42.1059
      CALL MOVE0 ( KODEBR(1), LBRNCH )                                  M42.1060
      CALL MOVE0 ( KODSEM(1), LBRNCH)                                   M14. 534
      CALL MOVE0 ( LENGTH(1), LBRNCH )                                  M20. 475
      CALL MOVE0 ( INDHST(1), LBRNCH )                                  M16. 334
      CALL MOVER0 ( BVALUE(1), LSIZ12 )                                 M38. 285
      ISWENT = 1                                                        M42.1061
      OMEGA = 0.0                                                       M42.1062
      DEGMIN = 0.0                                                      M42.1063
      DEGMAX = 0.0                                                      M42.1064
      SGLFIR = TWOPI                                                    M12. 176
      JST = 0                                                           M42.1065
      JST1 = 1                                                          M42.1066
      IFDEP2 = 1                                                        M42.1067
      IADD = 0                                                          M32. 666
      IFSEM = 0                                                         M14. 536
C                                                                       M43.   9
      IDM =  1                                                          M43.  10
      IDQ =  1                                                          M43.  11
      IDU =  1                                                          M43.  12
      IDT =  1                                                          M43.  13
      IADQQ = 1                                                         M43.  14
      IQ = 0                                                            M43.  15
      IDA = 0                                                           M43.  16
      IDY = 0                                                           M43.  17
      IFKC = 0                                                          M43.  18
      NK = 0                                                            M43.  19
      NGROUP = 0                                                        M43.  20
      NTURN = 0                                                         M43.  21
      NFDBR = 27                                                                
      NFDPH = 9                                                                 
      NFDHST = 5400                                                             
C      NFDPOL = 25      ! Assign maximum order of rational approximationM43.  25
      NFDPOL = 30                                                       M43.  26
      NTLIN = 0                                                                 
      LU2 = 69                                                                  
      LU6 = LUNIT6                                                              
C      IF ( ABUFF(1:5) .NE. 'FILE:' )  GO TO 5223                       M43.  30
C      write (*,*) ' Ready to open  ABUFF(6:40) =',  ABUFF(6:40)        M43.  31
C      OPEN ( UNIT=LU2,  STATUS='OLD',  FILE=ABUFF(6:40) )              M43.  32
C      write (*,*) ' Successful open ABUFF(6:40) =', ABUFF(6:40)        M43.  33
C     CALL CIMAGE  !Erase spcl request of cables by rding 1st real brnchM43.  34
C                                                                       M43.  35
 5223 IFX = 0                                                           M43.  36
      IP=2                                                              M14. 537
      IY=2                                                              M14. 538
      ICAT = 0                                                          M42.1068
      IF ( NOUTPR  .NE.  0 )   GO TO 15                                 M32. 667
      IF ( KOL132  .EQ.  132 )   GO TO 6452                             M32. 669
      WRITE (LUNIT6, 6438)  LTLABL, LBUS, LBRNCH, LDATA, LEXCT,         M32. 670
     1 LYMAT, LSWTCH, LSIZE7, LPAST, LNONL, LCHAR, LSMOUT, LSIZ12,      M38. 286
     2 LFDEP, LWT, LTAILS, LIMASS, LSYN, MAXPE, LTACST, LFSEM, LFD,     M39. 320
     3 LHIST, LSIZ23, LCOMP, LSPCUM, LSIZ26, LSIZ27, LSIZ28             M38. 287
 6438 FORMAT ( 39H *********  BEGIN "M40." EMTP SOLUTION.,              M42.1069
     1         17H   SIZE /LABEL/ =,  I7,                               M32. 675
     2         16H  INTEGER WORDS.   ,/,                                M32. 676
     3         20H LIST LIMITS  1-10 :,  10I6  ,/,                      M32. 677
     4         20H LIST LIMITS 11-20 :,  10I6  ,/,                      M32. 678
     5         20H LIST LIMITS 21-END:,  10I6  )                        M32. 679
      GO TO 15                                                          M32. 680
 6452 WRITE(LUNIT6, 83044)  LOCKER                                      M32. 681
83044 FORMAT ( ' ASSOCIATED USER DOCUMENTATION IS THE 864-PAGE',        M35. 470
     1         ' EMTP RULE BOOK DATED  JUNE, 1984.',                    M35. 471
     2    3X,  'VERSION M43.   VARDIM TIME/DATE =', 2I7 )               M42.1070
      WRITE (LUNIT6, 5241)  LTLABL, LBUS, LBRNCH, LDATA, LEXCT,         M32. 685
     1 LYMAT, LSWTCH, LSIZE7, LPAST, LNONL, LCHAR, LSMOUT, LSIZ12,      M38. 290
     2 LFDEP, LWT, LTAILS, LIMASS, LSYN, MAXPE, LTACST, LFSEM, LFD,     M39. 323
     3 LHIST, LSIZ23, LCOMP, LSPCUM, LSIZ26, LSIZ27, LSIZ28             M38. 291
 5241 FORMAT (   69H INDEPENDENT LIST LIMITS FOLLOW.   TOTAL LENGTH OF  M42.1071
     1 /LABEL/  EQUALS , I8,  16H  INTEGER WORDS.,                      M38. 292
     2  3X,  6I6  ,/,  ( 1X, 21I6, I5 ) )                               M38. 293
      WRITE (LUNIT6, 83049)                                             M32. 686
      WRITE(LUNIT6, 83047)  ( I, I=1, 8 )                               M32. 688
83047 FORMAT ( 130H DESCRIPTIVE INTERPRETATION OF NEW-CASE INPUT DATA 1 M42.1072
     1INPUT DATA CARD IMAGES PRINTED BELOW, ALL 80 COLUMNS, CHARACTER BYM42.1073
     2 CHARACTER.      ,/, 51X, 1H0,  8(9X, I1)  )                      M42.1074
      J = 0                                                             M42.1075
      WRITE (LUNIT6, 83048)  ( J, I=1, 8 )                              M32. 689
83048 FORMAT ( 51X, 1H0,  8(9X, I1)  )                                  M42.1076
      WRITE(LUNIT6, 83049)                                              M32. 690
83049 FORMAT (                                                          M35. 472
     1 51H --------------------------------------------------,          M35. 473
     2  1H+,  40H----------------------------------------,              M35. 474
     3        40H----------------------------------------   )           M35. 475
C     BEGIN LOOP OVER ALL INPUT DATA CARDS READ BY "OVER1" :            M32. 692
C     READ INPUT CARD USING CIMAGE.                                     M42.1077
   15 CALL CIMAGE                                                       M42.1078
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M22.1237
      NRIGHT = -2                                                       M22.1238
      N9 = KOLBEG                                                       M22.1239
      KOLBEG = 1                                                        M22.1240
      DECODE (80, 3246, ABUFF(1) )   TEXCOL                             M22.1241
 3246 FORMAT ( 80A1 )                                                   M22.1242
      CALL FREONE ( D1 )                                                M29. 989
 3247 NRIGHT = 0                                                        M22.1244
      IF ( N9  .EQ.  -INTINF )   KOLBEG = N9                            M22.1245
      IF ( NFRFLD  .GT.  0 )   GO TO 3280                               M22.1246
      IF ( NOUTPR  .EQ.  0 )                                            M20. 492
     1 WRITE (KUNIT6, 3270)                                             M35. 476
 3270 FORMAT( 31H+BLANK TERMINATION-OF-RUN CARD.   )                    M42.1079
      CALL  INTERP                                                      M35. 477
      IVOLT = 7777                                                      M23. 313
 3273 KILL = 9999                                                       M12. 177
      NCHAIN = 31                                                       M42.1080
      GO TO 9800                                                        M16. 338
 3280 CALL REQUES                                                       M28. 799
      I = LSTAT(18)                                                     M28. 800
C     REQUEST WORD NUMBER  0  IMPLIES MISCELLANEOUS DATA CARDS:         M28. 801
      IF ( I  .EQ.  0 )   GO TO 2843                                    M28. 802
C     NEXT COME EXCEPTIONS HANDLED OUTSIDE SUBROUTINE:                  M28. 803
      IF ( I  .EQ.  15 )   GO TO 8015                                   M28. 804
      IF ( I  .EQ.  32 )   GO TO 8032                                   M28. 805
      IF ( I  .EQ.  33 )   GO TO 8033                                   M28. 806
C     NEXT COME EXCEPTIONAL TERMINATIONS (SPECIAL JUMPS):               M28. 807
      IF ( I  .EQ.  11 )   GO TO 3273                                   M28. 808
      IF ( I  .EQ.  28 )   GO TO 3247                                   M28. 809
      IF ( I  .EQ.  34 )   GO TO 6523                                   M28. 810
      IF ( I  .EQ.  38 )   GO TO 15                                     M28. 811
      IF ( I  .EQ.  40 )   GO TO 4308                                   M28. 812
C     NEXT COME EXITS TO OTHER OVERLAYS:                                M28. 813
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28. 814
      IF ( NCHAIN  .NE.  1 )   GO TO 9800                               M28. 815
C     CONTINUE IN LOOP OF REQUEST WORDS, IF NONE OF ABOVE.              M28. 816
      GO TO 15                                                          M28. 817
C     $$$$$    SPECIAL-REQUEST WORD NO. 15.   'START AGAIN'       $$$$$ M43.  37
 8015 IALTER = LUNIT2                                                   M43.  38
      IF ( NOUTPR  .EQ.  0 )                                            M43.  39
     1 WRITE (KUNIT6, 2857)                                             M43.  40
 2857 FORMAT (   40H+CONTINUE PARTIALLY-COMPLETED DATA CASE. )          M43.  41
      IF ( TEXCOL(13) .NE. TEXT5 )  GO TO 2861                          M43.  42
      DECODE (80, 2852,  ABUFF)      DISK_FILE                          M43.  43
 2852 FORMAT ( A80 )                                                    M43.  44
      DO 2845  J=1, 2                                                   M43.  45
      N7 = INDEX ( DISK_FILE, ',' )                                     M43.  46
 2845 DISK_FILE(1:N7) = ' '                                             M43.  47
      DO 2848  J=N7, 80                                                 M43.  48
      IF ( DISK_FILE(J:J) .NE. ' ' ) GO TO 2849                         M43.  49
 2848 CONTINUE                                                          M43.  50
 2849 write (*,*) ' Name of old PL4 file  DISK_FILE(J:80) =',           M43.  51
     1                                    DISK_FILE(J:80)               M43.  52
      LUNT77 = 77                                                       M43.  53
      OPEN ( UNIT=LUNT77, STATUS='OLD', FILE=DISK_FILE(J:80),           M43.  54
     1       FORM='UNFORMATTED' )                                       M43.  55
      TEXCOL(13) = BLANK                                                M43.  56
      REWIND LUNT77                                                     M43.  57
      READ (LUNT77)  DATEXX, TCLOXX, NUMNAM, NUMNVO, NUMBCO,            M43.  58
     1               NUMBRN                                             M43.  59
      write (*,*) ' 1st record.  NUMNAM, NUMNVO, NUMBCO, NUMBRN =',     M43.  60
     1                           NUMNAM, NUMNVO, NUMBCO, NUMBRN         M43.  61
      IF ( LBUS+LSIZE7  .GE.  NUMNAM    .AND.                           M43.  62
     1     LSIZ12  .GE.  NUMNVO    .AND.                                M43.  63
     2     LBRNCH .GE. NUMBRN )   GO TO 2859                            M43.  64
      WRITE (LUNIT6, 2858)  NUMNAM, NUMNVO, NUMBRN                      M43.  65
 2858 FORMAT ( 33H TEMPORARY ERROR STOP IN "OVER1".,                    M43.  66
     1         38H   PROGRAM DIMENSIONING IS INADEQUATE.,               M43.  67
     2         27H   NUMNAM, NUMNVO, NUMBRN =, 3I8 )                    M43.  68
      CALL STOPTP                                                       M43.  69
 2859 REWIND LUNT77                                                     M43.  70
      N8 = LBUS                                                         M43.  71
      IF ( LBUS .GE. NUMNAM ) N8 = NUMNAM - 1                           M43.  72
      write (*,*) ' New name logic.  n8, numnam, lbus =',               M43.  73
     1                               n8, numnam, lbus                   M43.  74
      READ (LUNT77)  DATEXX, TCLOXX, NUMNAM, NUMNVO, NUMBCO,            M43.  75
     1               NUMBRN, ( BUS(J), J=1, N8)                         M43.  76
      WRITE (*, 9442) ( BUS(J), J=1, N8 )                               M43.  77
 9442 FORMAT ( ' BUS(1:N8) =', 10A7 )                                   M43.  78
      REWIND LUNT77                                                     M43.  79
      NUM888 = NUMNAM - N8                                              M43.  80
      write (*,*) ' Ready for final, full read with  NUM888 =',         M43.  81
     1                                               num888             M43.  82
      READ (LUNT77)  DATEXX, TCLOXX, NUMNAM, NUMNVO, NUMBCO,            M43.  83
     1               NUMBRN, ( BUS(J), J=1, N8),                        M43.  84
     2               ( TEXVEC(J), J=1, NUM888 )                         M43.  85
      write (*,*) ' After all names are read.  Next, IBSOUT.'           M43.  86
      IF ( NUMNVO  .GT.  0 )                                            M43.  87
     1 READ (LUNT77)  ( IBSOUT(J),  J=1, NUMNVO )                       M43.  88
      write (*,*) ' After IBSOUT.  Next, ....'                          M43.  89
      IF ( NUMBRN  .GT.  0 )                                            M43.  90
     1 READ (LUNT77)  ( KBUS(J), MBUS(J),  J=1, NUMBRN )                M43.  91
      write (*,*) ' After KBUS, MBUS.'                                  M43.  92
      WRITE (LUNIT4)  DATE1, TCLOCK, NUMNAM, NUMNVO, NUMBCO,            M43.  93
     1               NUMBRN, ( BUS(J), J=1, N8),                        M43.  94
     2               ( TEXVEC(J), J=1, NUM888 )                         M43.  95
      write (*, 3899)  DATE1, TCLOCK                                    M43.  96
 3899 format ( ' over1, lunit4 date and time =', 2A4, 2x, 2a4 )         M43.  97
      IF ( NUMNVO  .GT.  0 )                                            M43.  98
     1 WRITE (LUNIT4)  ( IBSOUT(J), J=1, NUMNVO )                       M43.  99
      IF ( NUMBRN  .GT.  0 )                                            M43. 100
     1 WRITE (LUNIT4)  ( KBUS(J), MBUS(J), J=1, NUMBRN )                M43. 101
      N18 = NUMNVO + NUMBRN + 1                                         M43. 102
      write (*,*) ' Enter loop over numbers.  N18 =',  N18              M43. 103
      DO 6528  J=1, 99999                                               M43. 104
      READ (LUNT77, END=6539)  ( R4(K), K=1, N18 )                      M43. 105
      if ( iprsup .eq. 7 .or. iprsup .gt. 9 )                           M43. 106
     1 write (*,*) ' J, R4(1) =',  J, R4(1)                             M43. 107
      IF ( R4(1) .EQ. -9999. ) GO TO 6539                               M43. 108
 6528 WRITE (LUNIT4)  ( R4(K), K=1, N18 )                               M43. 109
 6539 write (*,*) ' Done transferring LUNT77 to LUNIT4.  J =',  J       M43. 110
 2861 CALL RUNTYM ( D1, D2 )                                            M43. 111
      N18 = LOCKER(1)                                                   M42.1081
      N19 = LOCKER(2)                                                   M42.1082
      CALL PFATCH                                                       M22.1331
      CALL TABLES                                                       M28. 822
      write (*,*) ' N18, N19, LOCKER(1), LOCKER(2) =',                  M42.1084
     1              N18, N19, LOCKER(1), LOCKER(2)                      M42.1085
      FLSTAT(1) = -D1                                                   M28. 823
      FLSTAT(2) = -D2                                                   M28. 824
      IF ( N18 .EQ. LOCKER(1)  .AND.                                    M28. 825
     1     N19 .EQ. LOCKER(2) )  GO TO 2863                             M42.1086
      KILL = 201                                                        M22.1338
      LSTAT(19) = 2861                                                  M22.1339
      GO TO 9200                                                        M22.1341
 2863 CONTINUE                                                          M28. 826
C     READ INPUT CARD USING CIMAGE.                                     M28. 827
 2868 CALL CIMAGE                                                       M28. 828
      DECODE ( 80, 1212, ABUFF )  IJK                                   M37.1074
 1212 FORMAT ( 54X, I6 )                                                M37.1075
      IF ( IJK  .NE.  0 )  GO TO 6161                                   M37.1076
      IF ( KOLBEG  .GT.  0 )   GO TO 2872                               M28. 829
      DECODE (80, 2870, ABUFF) N3, D7, D8                                       
 2870 FORMAT ( I8, 2E16.0  )                                            M37.1078
      GO TO 2875                                                        M28. 832
 2872 NFRFLD = 1                                                        M28. 833
      CALL FREONE ( D3 )                                                M29. 991
      N3 = D3                                                           M28. 835
      CALL FREONE ( D7 )                                                M29. 992
      CALL FREONE ( D8 )                                                        
      CALL FREONE ( DDD )                                               M37.1080
 2875 IF ( N3  .EQ.  9999 )   GO TO 2879                                M28. 837
      IF ( NOUTPR  .EQ.  0 )                                            M28. 838
     1 WRITE (KUNIT6, 2876)  N3, D7, D8                                 M37.1081
 2876 FORMAT (  16H+ALTERED SWITCH.,  I4, 2E13.4  )                     M37.1082
      TCLOSE(N3) = D7                                                   M28. 841
      IF (D8 .GT. 0.0) TOPEN(N3) = D8                                           
      GO TO 2868                                                        M28. 842
 6161 IF ( IABS(IJK)  .EQ.  1111 ) GO TO 6363                           M37.1084
      CALL SWMODF                                                       M37.1085
      GO TO 2868                                                        M37.1086
 6363 CALL TACS1C                                                       M37.1087
      GO TO 2868                                                        M37.1088
 2879 IF ( NOUTPR  .EQ.  0 )                                            M28. 843
     1 WRITE (KUNIT6, 2882)                                             M35. 505
 2882 FORMAT (  37H+TERMINATOR FOR SWITCH CLOSING TIMES.   )            M28. 845
      READ (LUNIT2)  LOCKER                                             VAX.  32
      DO 2476  J=1, 9999                                                M42.1087
      READ (LUNIT2, END=2479)  ANSI132                                  M42.1088
 2474 FORMAT ( A132 )                                                   M42.1089
 2476 WRITE (LUNIT6, 2474)  ANSI132                                     M42.1090
 2479 LIMSTP = 0                                                        M28. 846
      INDSTP = 1                                                        M28. 847
      ISPLOT = 0                                                        M28. 848
      FLSTAT(14) = T                                                    M28. 849
      NCHAIN = 1                                                        M28. 850
      LASTOV = 0                                                        M28. 851
      GO TO 15                                                          M28. 852
C     $$$$$  SPECIAL REQUEST-WORD NO. 32.  'ABSOLUTE TACS DIMENSIONS'  $M22.1469
 8032 IF ( NOUTPR  .EQ.  0 )                                            M28. 853
     1 WRITE (KUNIT6, 7020)                                             M35. 506
 7020 FORMAT(36H+SET ABSOLUTE TACS LIST SIZE LIMITS.   )                M22.1471
C     READ INPUT CARD USING CIMAGE                                      M22.1472
      CALL CIMAGE                                                       M22.1473
      IF (KOLBEG .GT. 0) GO TO 7030                                     M22.1474
      CALL INTCHK(LL1, LL80, LL8)                                       M22.1475
      IF (KILL .GT. 0) GO TO 9200                                       M22.1476
      DECODE (80, 5, ABUFF) (LSTACS(I), I=1, 8)                         M39. 324
      GO TO 7050                                                        M22.1478
 7030 NFRFLD = 10                                                       M22.1479
      CALL FREFLD( VOLTBC(1) )                                          M31. 208
      DO 7040 I=1, 8                                                    M39. 325
 7040 LSTACS(I) = VOLTBC(I)                                             M31. 209
 7050 IF (LSTACS(1) .LT. 5 )   LSTACS(1) = 5                            M28. 855
      IF ( NOUTPR  .EQ.  0 )                                            M28. 856
     1 WRITE (KUNIT6, 7060) LSTACS(1), LSTACS(2), LSTACS(3)             M35. 507
 7060 FORMAT(30H+1ST TACS DIMENSIONS CARD.     , 3I6)                   M22.1484
      GO TO 15                                                          M22.1498
C     $$$$$  SPECIAL REQUEST-WORD NO. 33.  'RELATIVE TACS DIMENSIONS'  $M22.1499
 8033 IF ( NOUTPR  .EQ.  0 )                                            M28. 860
     1 WRITE (KUNIT6, 7110)                                             M35. 509
 7110 FORMAT ( 47H+PROPORTIONAL ALLOCATION OF TOTAL TACS STORAGE. )     M22.1501
C     READ INPUT CARD USING CIMAGE                                      M22.1502
      CALL CIMAGE                                                       M22.1503
      IF (KOLBEG .GT. 0) GO TO 7120                                     M22.1504
      CALL EXPCHK(LL1, LL80, LL8)                                       M22.1505
      IF (KILL .GT. 0) GO TO 9200                                       M22.1506
      DECODE (80, 3415, ABUFF) (VOLTBC(I), I=1, 10)                     M31. 212
      GO TO 7130                                                        M22.1508
 7120 NFRFLD = 10                                                       M22.1509
      CALL FREFLD( VOLTBC(1) )                                          M31. 213
 7130 IF ( NOUTPR  .EQ.  0 )                                            M28. 862
     1 WRITE (KUNIT6, 7140) VOLTBC(1), VOLTBC(2), VOLTBC(3)             M35. 510
 7140 FORMAT(23H+RELATIVE LIST SIZES.    , 3E9.2)                       M22.1512
      D1 = 0.0                                                          M22.1523
      DO 7170 I=1, 8                                                    M39. 326
 7170 D1 = D1 + VOLTBC(I)                                               M31. 218
      D1 = LTACST * NBYTE(3) / D1                                       M27. 571
      LSTACS( 1) = VOLTBC( 1) * D1 / ( 4 * NBYTE(3) + 8 * NBYTE(4) )    M39. 327
      LSTACS( 2) = VOLTBC( 2) * D1 / ( 2 * NBYTE(3) + NBYTE(4) )        M39. 328
      LSTACS( 3) = VOLTBC( 3) * D1 / ( 2 * NBYTE(4) )                   M39. 329
      LSTACS( 4) = VOLTBC( 4) * D1 / ( 5 * NBYTE(3) + NBYTE(4) )        M39. 330
      LSTACS( 5) = VOLTBC( 5) * D1 / ( 3 * NBYTE(4) )                   M39. 331
      LSTACS( 6) = VOLTBC( 6) * D1 / NBYTE(4)                           M39. 332
      LSTACS( 7) = VOLTBC( 7) * D1 / NBYTE(3)                           M39. 333
      LSTACS( 8) = VOLTBC( 8) * D1 / ( 6 * NBYTE(3) + 2 * NBYTE(4) )    M39. 334
      GO TO 15                                                          M22.1542
C     BEGIN PROCESSING FLOATING POINT MISC. DATA CARD ....              M28. 866
 2843 IF ( NOUTPR .NE. 0 .AND. IPRSUP .GT. 0) NOUTPR = 0                M22.1552
      XOPT = STATFR                                                     M28. 867
      COPT = STATFR                                                     M28. 868
      KOLBEG = N9                                                       M23. 393
      IF ( KOLBEG .GT. 0) GO TO 4201                                    M22.1553
      CALL EXPCHK ( LL1,  LL80,  LL8 )                                  M20. 620
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M08.  83
      DECODE (80, 3415, ABUFF(1) )  DELTAT, TMAX, D1, D2, D3, TOLMAT, T M20. 621
 3415 FORMAT( 10E8.0 )                                                  M42.1091
      IF ( T  .EQ.  0.0 )   T = 0.0                                     M21.  15
      GO TO 4202                                                        M20. 622
 4201 NFRFLD = 1                                                        M20. 623
      NRIGHT = 0                                                        M20. 624
      CALL FREONE ( DELTAT )                                            M29. 993
      CALL FREONE ( TMAX   )                                            M29. 994
      CALL FREONE ( D1     )                                            M29. 995
      CALL FREONE ( D2     )                                            M29. 996
      CALL FREONE ( D3     )                                            M29. 997
      CALL FREONE ( TOLMAT )                                            M29. 998
      CALL FREONE ( T      )                                            M29. 999
 4202 IF ( NOUTPR  .EQ.  0 )                                            M20. 632
     1 WRITE (KUNIT6, 4205)  DELTAT, TMAX, D1                           M35. 512
 4205 FORMAT ( 13H+MISC. DATA.   , 3E12.3  )                            M42.1092
      IF( IOFBND .NE. 33666 ) GO TO 4206                                M39. 335
      NCHAIN = 41                                                       M39. 336
      XOPT = D1                                                                 
      GO TO 9800                                                        M39. 337
C     READ INPUT CARD USING CIMAGE.                                     M42.1094
 4206 CALL CIMAGE                                                       M39. 338
      IF ( KOLBEG  .GT.  0 )   GO TO 4207                               M20. 660
      CALL INTCHK ( LL1, LL80, LL8 )                                    M20. 661
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M08.  85
      DECODE (80, 5, ABUFF) IOUT, IPLOT, IDOUBL, KSSOUT, MAXOUT,        M36. 268
     1                            IPUN, MEMSAV, ICAT, N1, N2            M22.1555
    5 FORMAT( 10I8 )                                                    M42.1095
      GO TO 4208                                                        M20. 669
 4207 NFRFLD = 10                                                       M20. 670
      CALL FREFLD ( VOLTBC(1) )                                         M31. 234
      IOUT   = VOLTBC(1)                                                M31. 235
      IPLOT  = VOLTBC(2)                                                M36. 269
      IDOUBL = VOLTBC(3)                                                M31. 237
      KSSOUT = VOLTBC(4)                                                M31. 238
      MAXOUT = VOLTBC(5)                                                M31. 239
      IPUN   = VOLTBC(6)                                                M31. 240
      MEMSAV = VOLTBC(7)                                                M31. 241
      ICAT   = VOLTBC(8)                                                M31. 242
      N1 = VOLTBC(9)                                                    M31. 243
      N2 = VOLTBC(10)                                                   M31. 244
 4208 NENERG = N1                                                       M36. 270
      IF ( IPLOT .EQ. 0 )  IPLOT = 1                                    M36. 271
      IF ( M4PLOT .EQ. 1  .AND.                                                 
     1     IPLOT .EQ. -1 )                                                      
     2 IPLOT = 1                                                                
      IF ( IPLOT .EQ. -1 )  ISPLOT = INTINF                             M33. 116
      IF ( NOUTPR  .EQ.  0 )                                            M36. 275
     1 WRITE (KUNIT6, 4210)  IOUT, IPLOT, IDOUBL, KSSOUT, MAXOUT,       M36. 276
     1                       IPUN, MEMSAV, ICAT, N1, N2                 M22.1557
 4210 FORMAT ( 13H+MISC. DATA.  , 2I5, 8I3 )                            M42.1096
      BEGMAX(1) = MAXOUT                                                M31. 245
      MAXOUT = 2                                                        M31. 246
      IF ( N2  .EQ.  0 )   GO TO 6519                                   M20. 688
      CALL COPYI ( N2, IPRSOV(1), LL30 )                                M23. 394
 6519 IPRSUP = IPRSOV(1)                                                M20. 691
      IF ( ICAT .GT. 2 ) ICAT = 0                                       M42.1097
      IF (N1 .EQ. 0) GO TO 600                                          M20. 692
C     READ INPUT CARD USING CIMAGE                                      M42.1098
 6523 CALL CIMAGE                                                       M23. 395
      IF ( KOLBEG  .GT.  0 )   GO TO 623                                M20. 693
      CALL INTCHK ( LL1, LL24, LL8 )                                    M20. 694
      CALL EXPCHK ( LL25, LL64, LL8 )                                   M20. 695
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M08.  88
      DECODE (80, 620, ABUFF)  ISW, ITEST, IDIST, AINCR, XMAXMX, DEGMIN,M42.1099
     1  DEGMAX, D4, SIGMAX, JSEEDR                                      M19. 296
  620 FORMAT (3I8, 6F8.0, I8 )                                          M13. 296
      GO TO 624                                                         M20. 696
  623 NFRFLD = 3                                                        M20. 697
      CALL FREFLD ( VOLTBC(1) )                                         M31. 247
      ISW    = VOLTBC(1)                                                M31. 248
      ITEST  = VOLTBC(2)                                                M31. 249
      IDIST  = VOLTBC(3)                                                M31. 250
      NFRFLD = 1                                                        M20. 702
      CALL FREONE ( AINCR  )                                            M29.1000
      CALL FREONE ( XMAXMX )                                            M29.1001
      CALL FREONE ( DEGMIN )                                            M29.1002
      CALL FREONE ( DEGMAX )                                            M29.1003
      CALL FREONE ( D4     )                                            M29.1004
      CALL FREONE ( SIGMAX )                                            M29.1005
      CALL FREFLD ( VOLTBC(1) )                                         M31. 251
      JSEEDR = VOLTBC(1)                                                M31. 252
  624 IF ( NOUTPR  .EQ.  0 )                                            M20. 711
     1 WRITE (KUNIT6, 630)  ISW, ITEST, IDIST, AINCR                    M35. 513
  630 FORMAT(17H+STATISTICS DATA.,3I8, F9.4 )                           M42.1100
      IF ( XMAXMX  .EQ.  0.0 )   XMAXMX = 2.0                           M28. 869
      IF(AINCR.EQ.0.0) AINCR = UNITY / 20.                              M05. 322
      IF ( D4  .GT.  0.0 )   STATFR = D4                                M42.1101
      IF ( DEGMAX  .EQ.  0.0 )   DEGMAX = 360.                          M42.1102
      IF (SIGMAX .EQ. 0.0)   SIGMAX = 4.0                               M13. 297
      IF ( JSEEDR  .GT.  0 )   JSEEDR = INTINF                          M19. 297
       IF (  KBASE  .NE.  INTINF )                                      M36. 277
     2 KBASE = 1                                                        M23. 398
      BEGMAX(1) = 1.0                                                   M31. 253
      IF ( NENERG  .NE.  INTINF )   GO TO 600                           M23. 399
      NCHAIN = 29                                                       M23. 400
      GO TO 9800                                                        M23. 401
  600 IF( D1 .EQ. 0.0 )  GO TO 6260                                     M42.1103
      IF( D1 .EQ. XOPT )  GO TO 6260                                    M42.1104
      IF ( NOUTPR  .EQ.  0 )                                            M20. 714
     1 WRITE(LUNIT6, 6255)  XOPT, D1                                    M20. 715
 6255 FORMAT ( 36H ----- WARNING.   NONZERO MISC. DATA,                 M32. 693
     1         34H PARAMETER "XOPT" DIFFERS FROM THE,                   M32. 694
     2         19H POWER FREQUENCY OF,  F8.2,                           M32. 695
     3         21H .   THIS IS UNUSUAL.         ,/,                     M32. 696
     2 7X,  10HA VALUE OF, E13.4,  88H WAS READ FROM COLUMNS 17-24 OF THM42.1105
     3E DATA CARD JUST READ.   EXECUTION WILL CONTINUE USING   ,/,      M42.1106
     4 7X,  49HTHIS VALUE, AS SUSPICIOUS AS IT SEEMS TO THE EMTP   )    M25. 189
 6260 XOPT = D1                                                         M42.1107
      IF ( D2  .EQ.  0.0 )   GO TO 6265                                 M42.1108
      IF( D2 .EQ. COPT )  GO TO 6265                                    M42.1109
      IF ( NOUTPR  .EQ.  0 )                                            M20. 716
     1 WRITE(LUNIT6, 6256)  COPT, D2                                    M20. 717
 6256 FORMAT ( 36H ----- WARNING.   NONZERO MISC. DATA,                 M32. 697
     1         34H PARAMETER "COPT" DIFFERS FROM THE,                   M32. 698
     2         19H POWER FREQUENCY OF,  F8.2,                           M32. 699
     3         21H .   THIS IS UNUSUAL.         ,/,                     M32. 700
     2 7X,  10HA VALUE OF, E13.4,  88H WAS READ FROM COLUMNS 25-32 OF THM42.1110
     3E DATA CARD JUST READ.   EXECUTION WILL CONTINUE USING   ,/,      M42.1111
     4 7X,  49HTHIS VALUE, AS SUSPICIOUS AS IT SEEMS TO THE EMTP   )    M25. 190
 6265 COPT = D2                                                         M42.1112
      IF ( D3  .GT.  0.0 )   EPSILN = D3                                M42.1113
      IF( TOLMAT .LE. 0.0 )  TOLMAT = EPSILN                            M42.1114
      TOLMAT = TOLMAT**2                                                M42.1115
      IF (IPLOT.EQ.0) IPLOT = 1                                         M42.1116
      IF( IPLOT .LT. 0 )  GO TO 6279                                    M42.1117
      IF(  (IPLOT/2)*2   .EQ.   IPLOT  ) 
     1 WRITE (LUNIT6, *) '    -----  WARNING!  Even  IPLOT =', iplot
 6279 IF (IOUT.EQ.0) IOUT = 1                                           M42.1119
      IF ( TMAX  .LE.  0.0     .AND.                                    M21.  17
     1     DELTAT  .LE.  0.0 )     DELTAT = 1.0                         M21.  18
      IF ( DELTAT  .GT.  0.0     .AND.                                  M20. 718
     1     T  .GE.  0.0 )   GO TO 4215                                  M20. 719
      KILL = 2                                                          M42.1120
      FLSTAT(16) = DELTAT                                               M42.1121
      LSTAT(19) = 4215                                                  M20. 720
      GO TO 9200                                                        M42.1122
 4215 D12 = TMAX / DELTAT  +  1.0                                       M27. 617
      D13 = INTINF                                                      M27. 618
      IF ( D12 .LT. D13 )   GO TO 4223                                  M27. 619
      KILL = 81                                                         M42.1123
      LSTAT(19) = 4215                                                  M27. 620
      FLSTAT(15) = D12                                                  M27. 621
      FLSTAT(16) = D13                                                  M27. 622
      GO TO 9200                                                        M42.1124
 4223 IOUTIN = IOUT                                                     M42.1125
      DELTA2 = DELTAT / 2.0                                             M27. 623
      DLTINV = 1.0 / DELTAT                                                     
      TMAX = TMAX - DELTA2                                              M33. 117
      IF ( MEMSAV .EQ. 1 )  TMAX = TMAX + DELTAT                        M33. 118
      I = 1                                                             M42.1126
      IF ( IPUN  .LT.  0 )   GO TO 4303                                 M23. 402
      IF ( KPRCHG(1)  .EQ.  -7777 )   GO TO 4213                        M23. 403
      GO TO 4312                                                        M23. 404
 4303 IPUN = 0                                                          M23. 405
C     READ INPUT CARD USING CIMAGE.                                     M42.1127
 4308 CALL CIMAGE                                                       M23. 406
      IF ( KOLBEG  .GT.  0 )   GO TO 4217                               M20. 722
      CALL INTCHK ( LL1, LL80, LL8 )                                    M20. 723
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M08.  90
      DECODE(80, 4211, ABUFF)  ( KPRCHG(I), MULTPR(I), I=1, 5 )         M42.1128
 4211 FORMAT( 10I8 )                                                    M42.1129
      GO TO 4219                                                        M20. 724
 4217 NFRFLD = 10                                                       M20. 725
      CALL FREFLD ( VOLTBC(1) )                                         M31. 254
      J = 1                                                             M20. 727
      DO 4218  I=1, 5                                                   M20. 728
      KPRCHG(I) = VOLTBC(J)                                             M31. 255
      MULTPR(I) = VOLTBC(J+1)                                           M31. 256
 4218 J = J + 2                                                         M20. 731
 4219 IF ( NOUTPR  .EQ.  0 )                                            M20. 732
     1 WRITE (KUNIT6, 14211)   (KPRCHG(I), MULTPR(I), I=1, 3)           M35. 514
14211 FORMAT( 11H+PRINTOUT :,  6I6  )                                   M27. 625
      DO 4212  I=1, 5                                                   M42.1130
      IF( KPRCHG(I) .EQ. 0 )  GO TO 4213                                M42.1131
 4212 CONTINUE                                                          M42.1132
      I = 6                                                             M42.1133
 4213 KPRCHG(I) = INTINF                                                M42.1134
      IF ( KTREF  .NE.  -7777 )   GO TO 4312                            M23. 407
      KTREF = 0                                                         M23. 408
      GO TO 15                                                          M23. 409
 4312 IF ( IPRSUP  .GE.  1 )                                            M23. 410
     1 WRITE(LUNIT6, 4258 )  DELTAT, TMAX, XOPT, COPT, EPSILN, TOLMAT   M17. 141
 4258 FORMAT( 10X, 6HDELTAT, 11X, 4HTMAX, 11X, 4HXOPT, 11X, 4HCOPT,     M42.1135
     1 9X, 6HEPSILN, 9X, 6HTOLMAT, /, 1X, 6E15.5 , /,1X)                M15. 407
      IF ( IPRSUP  .LE.  0 )   GO TO 4266                               M42.1136
      IF (NENERG .EQ. 0)   GO TO 4266                                   M13. 300
      IF ( NOUTPR  .EQ.  0 )                                            M20. 734
     1 WRITE (LUNIT6, 4264)  ISW, ITEST, IDIST, AINCR, XMAXMX, DEGMIN,  M20. 735
     1 DEGMAX, STATFR                                                   M42.1137
 4264 FORMAT ( /,  47H STATISTICS PARAMETERS.     ISW   ITEST   IDIST , M42.1138
     1 10X, 5HAINCR, 9X, 6HXMAXMX, 9X, 6HDEGMIN, 9X, 6HDEGMAX, 9X,      M42.1139
     2 6HSTATFR, /, 23X, 3I8, 5E15.5 )                                  M42.1140
 4266 IF ( IFDEP  .NE.  -5555 )   GO TO 4269                            M23. 411
      IFDEP = 0                                                         M23. 412
      GO TO 15                                                          M23. 413
 4269 XUNITS = 1000.                                                    M23. 414
      IF( XOPT .GT. 0.0 )   XUNITS = TWOPI * XOPT                       M42.1141
      NTOT=1                                                            M42.1142
      MAXBUS = 0                                                                
      N23 = 0                                                           M38. 300
      CALL NAMEA6( BLANK, N23 )                                         M38. 301
      ICHECK=1                                                          M42.1143
      IBR=0                                                             M42.1144
      ICHAR=0                                                           M42.1145
      INONL=0                                                           M42.1146
      KSWTCH = 0                                                        M42.1147
      NUMSUB = 0                                                        M27. 626
      NPOWER = 0                                                        M42.1148
      KSWPE4 = 0                                                        M42.1149
      NV=0                                                              M42.1150
      NUMNVO = 0                                                        M32. 702
      IT=1                                                              M42.1151
      N1 = 0                                                            M14. 553
      N12 = JFLSOS / 100                                                M23. 415
      N15 = JFLSOS - 100*N12                                            M23. 416
      N13 = N15 / 10                                                    M23. 417
      N14 = N15 - 10*N13                                                M23. 418
      LSTAT(14) = N12                                                   M23. 419
      LSTAT(15) = N13                                                   M23. 420
      LSTAT(16) = N14                                                   M23. 421
      CALL MIDOV1                                                       M22.1558
      LSTAT(39) = 137                                                   M22.1559
C     READ INPUT CARD USING CIMAGE                                      M13. 301
 2691 CALL CIMAGE                                                       M14. 554
      DECODE (80, 3245, ABUFF(1) )   ( AUPPER(I), I=1, 14 )             M22.1560
 3245 FORMAT ( 13A6,  A2  )                                             M22.1561
      IF ( AUPPER( 1) .EQ. TEXT1 )  GO TO 2697                          M28. 870
      IF ( AUPPER( 1) .EQ. TEXT3 )  GO TO 2697                          M28. 871
      IF ( AUPPER( 1) .EQ. TEXT4 )  GO TO 2699                          M28. 872
      if ( aupper( 1) .ne. text6 ) go to 7722
      write (kunit6, 5389)
 5389 format ( ' Begin TACS. ==========================' )
      newtac = 1
      ntcsex = 1
      niunrs = 1
      write (*,*) ' Prepare to call  NTACS1  from over1.'
      call ntacs1
      write (*,*) ' Back from ntacs1, back in over1.'
      go to 4284
 7722 GO TO 4281                                                        M28. 873
 2697 NTCSEX = 1                                                        M28. 874
 2699 N1 = 1                                                            M28. 875
      IF ( NOUTPR .NE. 0 )  GO TO 2691                                  M28. 876
      IF ( NTCSEX .EQ. 0 )  GO TO 22699                                 M28. 877
      IF ( NOUTPR  .EQ.  0 )                                            M28. 878
     1 WRITE (KUNIT6, 32699)                                            M35. 515
32699 FORMAT ( 45H+TACS HYBRID SETUP.  TACS DATA CARDS FOLLOW.  )       M28. 880
      DECODE ( 80, 1984, ABUFF(1) )  LSTAT(52)                          M39. 340
 1984 FORMAT ( 18X, I2 )                                                M39. 341
      GO TO 2691                                                        M28. 881
22699 IF ( NOUTPR  .EQ.  0 )                                            M28. 882
     1 WRITE (KUNIT6, 42699)                                            M35. 516
42699 FORMAT ( 45H+TACS STAND-ALONE SETUP.  DATA CARDS FOLLOW.  )       M28. 884
      DECODE ( 80, 1984, ABUFF(1) )  LSTAT(52)                          M39. 342
      GO TO 2691                                                        M14. 558
 4281 IF ( N1  .EQ.  0 )   GO TO 4284                                   M14. 559
      CALL MOVE ( LSTACS(1), LSTAT(61), LL8 )                           M39. 343
      KTAB = 1                                                          M28. 885
      CALL TACS1                                                        M14. 560
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M13. 330
      IF ( NTCSEX+NSTACS  .GT.  0 )   GO TO 4276                        M13. 331
      INDSTP = 1                                                        M19. 299
      LIMSTP = KPRCHG(1)                                                M19. 300
      CALL RUNTYM ( D1, D2 )                                            M21.  19
      FLSTAT(1) = FLSTAT(1) + D1                                        M21.  20
      FLSTAT(2) = FLSTAT(2) + D2                                        M21.  21
      FLSTAT(7) = FLSTAT(7) - D1                                        M21.  22
      FLSTAT(8) = FLSTAT(8) - D2                                        M21.  23
      NCHAIN = 12                                                       M13. 335
      GO TO 9800                                                        M16. 341
C     READ INPUT CARD USING CIMAGE                                      M13. 337
 4276 CALL CIMAGE                                                       M13. 338
 4284 NCHAIN = 2                                                        M14. 561
      GO TO 9800                                                        M16. 342
 9200 NCHAIN = 51                                                       M42.1152
      LSTAT(18) = 1                                                     M42.1153
 9800 LASTOV = 1                                                        M16. 349
      N5 = LOCINT ( IDA )                                               M43. 112
      N6 = LOCINT ( IFKC )                                              M43. 113
C      write (*,*) ' End over1.  N5, N6, IDA, IFKC =',                  M43. 114
C     1                          N5, N6, IDA, IFKC                      M43. 115
      IF ( IPRSUP  .GE.  1 )                                            M32. 703
     1 WRITE (  LUNIT6, 4568 )                                          M32. 704
 4568 FORMAT ( 23H "EXIT  MODULE OVER1." )                              M32. 705
99999 RETURN                                                            M42.1154
      END                                                               M42.1155
      SUBROUTINE TACS1C                                                 M37.1093
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     CALLED ONLY BY OVER1 FOR START AGAIN USAGE                        M37.1094
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      REAL*8        ALNODE                                              M37.1098
 1000 IF ( IPRSUP  .GE.  1 )                                            M37.1099
     1 WRITE ( LUNIT6,4567 )                                            M37.1100
 4567 FORMAT ( 24H  "BEGIN MODULE TACS1C." )                            M37.1101
      DECODE (80, 187, ABUFF(1))                                        M37.1102
     1  N, ALNODE, DUM1, DUM3, DUM2, IJK, PRX, PRU                      M39. 344
  187 FORMAT (I2,A6,2X,3E10.0,14X,I6,2E10.0)                            M39. 345
      IF ( NIU  .LT.  12 )  GO TO 2868                                  M39. 346
      NDY5 = KUD1 - 5                                                   M37.1105
      DO 6565 I = 12, NIU                                               M39. 347
      NDY5 = NDY5 + 5                                                   M37.1108
      NDX1 = ILNTAB( KALIU + I )                                        M39. 348
      IF ( TEXVEC(NDX1) .EQ. ALNODE ) GO TO 6767                        M38. 304
 6565 CONTINUE                                                          M37.1111
      WRITE ( LUNIT6, 1313 )                                            M37.1112
 1313 FORMAT ( 49H $$$ NO SUCH SOURCS, THE CARD WILL BE IGNORED $$$ )   M37.1113
      GO TO 2868                                                        M37.1114
 6767 IF ( N .EQ. 0 ) GO TO 6811                                        M37.1115
      IUTY(KIUTY+I) = N                                                 M37.1118
 6811 IF (DUM1 .NE. 0. .OR. IJK .LT. 0) UD1(NDY5+1) = DUM1              M37.1119
      IF (DUM2 .EQ. 0. .AND. IJK .GT. 0) GO TO 6822                     M37.1120
      UD1(NDY5+2) = DUM2                                                M37.1121
      IF (IUTY(KIUTY+I) .EQ. 14) UD1(NDY5+2) = DUM2*TWOPI/360.          M39. 349
      IF (IUTY(KIUTY+I) .NE. 23) GO TO 6822                             M39. 350
      IF ( UD1(NDY5+2) .LT. DELTAT ) UD1(NDY5+2) = DELTAT               M37.1124
 6822 IF (DUM3 .NE. 0. .OR. IJK .LT. 0) UD1(NDY5+3) = DUM3              M37.1125
      IF ( PRX .NE. 0. .OR. IJK .LT. 0) UD1(NDY5+4) = PRX               M37.1126
      IF ( PRU .NE. 0. ) UD1(NDY5+5) = PRU                              M37.1127
      IF ( NOUTPR .GT. 0 ) WRITE ( LUNIT6, 1515 )                       M37.1128
 1515 FORMAT (  34H ANOTHER TACS SOURCE CHANGING CARD  )                M37.1129
 2868 RETURN                                                            M37.1130
      END                                                               M37.1131
      SUBROUTINE  SWMODF                                                M37.1132
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     CALLED ONLY BY OVER1 FOR START AGAIN USAGE                        M37.1133
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      REAL*8         TEXT14, TEXT15                                     M39. 351
      DATA  TEXT14   /  6HING     /                                     M37.1138
      DATA  TEXT15   / 6HCLOSED /                                       M37.1139
 1000 IF ( IPRSUP  .GE.  1 )                                            M37.1140
     1 WRITE ( LUNIT6, 4567 )                                           M37.1141
 4567 FORMAT ( 23H BEGIN MODULE "SWMODF". )                             M37.1142
      DECODE (80, 35, ABUFF(1) )   IT2, BUS1, BUS2, GUS3, GUS4, CK1,    M37.1143
     1                             A, IJK, BUS4, BUS5, BUS6, JDU, J     M39. 352
   35 FORMAT (I2,2A6,4E10.0,I6,A4,2A6,2X,2I1)                           M42.1156
      DO 3535  MSW = 1, KSWTCH                                          M37.1146
      K = KMSWIT(MSW)                                                   M37.1147
      IF ( BUS(K) .NE. BUS1 ) GO TO 3535                                M37.1148
      M = KMSWIT(MSW+LSWTCH)                                            M37.1149
      IF ( BUS(M) .EQ. BUS2 ) GO TO 3510                                M37.1150
 3535 CONTINUE                                                          M37.1151
 3636 WRITE ( LUNIT6, 3131  )                                           M37.1152
 3131 FORMAT ( 43H NO SUCH SWITCH, THE CARD WILL BE DISCARDED )         M37.1153
      GO TO 209                                                         M37.1154
 3510 IF ( IT2 .NE. 0  .OR.  KSWTYP(MSW) .NE. 0 )  GO TO 209            M37.1155
      IF ( BUS4  .NE.  TEXT14 )  GO TO 7218                             M37.1157
      IF ( NOUTPR  .EQ.  0 )  WRITE ( KUNIT6, 1218 )                    M37.1158
 1218 FORMAT ( 45H+PERMANENTLY-CLOSED SWITCH USED FOR METERING.   )     M37.1159
      GUS3 = -1.0                                                       M37.1160
      GUS4 = FLTINF                                                     M37.1161
      GO TO 216                                                         M37.1162
 7218 IF ( NOUTPR  .EQ.  0 )                                            M37.1163
     1 WRITE (KUNIT6, 36 )  GUS3, GUS4, CK1, A                          M37.1164
   36 FORMAT ( 8H+SWITCH.  ,  2X, 4E10.2  )                             M37.1165
      IF ( A    .EQ.  0.0   )   GO TO 216                               M37.1166
      IF ( GUS4 .NE. 0.0 .OR. IJK .LT. 0 ) ADELAY(MSW)=ABSZ(GUS4)       M37.1167
      GUS4=ABSZ(A)                                                      M37.1168
      IF (GUS3 .GE. 0.) GO TO 216                                       M37.1169
      GUS3 = 0.                                                         M37.1170
      IF( IPRSUP .GE. 1 )  WRITE (LUNIT6, 217)                          M37.1171
  217 FORMAT ( 16X, 22HTCLOSE CHANGED TO ZERO  )                        M37.1172
  216 IF ( GUS3 .NE. 0.0 .OR. IJK .LT. 0 ) TCLOSE(MSW)=GUS3             M37.1173
      IF ( GUS4 .NE. 0.0 .OR. IJK .LT. 0 ) TOPEN(MSW)=GUS4              M37.1174
      IF ( CK1  .NE. 0.0 .OR. IJK .LT. 0 ) CRIT(MSW)=CK1                M37.1175
  209 IF ( IPRSUP  .GE.  1 )                                            M37.1176
     1 WRITE ( LUNIT6, 4568 )                                           M37.1177
 4568 FORMAT ( 25H  EXIT  MODULE "SWMODF." )                            M37.1178
99999 RETURN                                                            M37.1179
      END                                                               M37.1180
      SUBROUTINE REQUES                                                 M28. 886
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      DIMENSION  ANGLEX(1), FARRAY(1)                                   M32. 706
      EQUIVALENCE  ( ANGLEX(1), ANGLE )                                 M28. 890
      EQUIVALENCE (MONCAR(2), KBASE),   (MONCAR(3), LTDELT)             M37.1181
      EQUIVALENCE (IPRSOV(39), NMAUTO)                                  M42.1157
      REAL*8          TEXTAX,  TEXTAY                                   M29.1006
      COMMON  /SYSTEMATIC/  LINSYS                                      M42.1158
      DIMENSION       TEXTAX(300), JPNTR(100), TEXTAY(100)              M37.1182
      COMMON  /LINEMODEL/  KEXACT,NSOLVE, FMINSV, NUMRUN,NPHLMT
      COMMON / LINEMODEL / CHAR80, CHLMFS(18)
      CHARACTER*6 CHLMFS     ! 9-PHASE AS LIMIT FOR LMFS TEST
      CHARACTER*80 CHAR80

C     $$$$$    SPECIAL-REQUEST WORD NO. 1.   'XFORMER'           $$$$$  M28. 894
      DATA  TEXTAY(1)   /  6HX       /                                  M28. 895
      DATA  JPNTR(1)    /  1  /                                         M28. 896
      DATA  TEXTAX(1)    /  6HXFORME  /                                 M28. 897
      DATA  TEXTAX(2)    /  6HR       /                                 M28. 898
C     $$$$$    SPECIAL-REQUEST WORD NO. 2.   'SATURATION'        $$$$$  M28. 899
      DATA  TEXTAY(2)   /  6HS       /                                  M28. 900
      DATA  JPNTR(2)    /  3  /                                         M28. 901
      DATA  TEXTAX(3)    /  6HSATURA  /                                 M28. 902
      DATA  TEXTAX(4)    /  6HTION    /                                 M28. 903
C     $$$$$    SPECIAL-REQUEST WORD NO. 3.   'TYPE99 LIMIT'      $$$$$  M43. 117
      DATA  TEXTAY(3)   /  6HTL       /                                 M43. 118
      DATA  JPNTR(3)    /  5  /                                         M28. 906
      DATA  TEXTAX(5)    /  6HTYPE99  /                                 M43. 119
      DATA  TEXTAX(6)    /  6HLIMIT   /                                 M43. 120
C     $$$$$    SPECIAL-REQUEST WORD NO. 4.   'REPLOT'            $$$$$  M28. 909
      DATA  TEXTAY(4)   /  6HR       /                                  M28. 910
      DATA  JPNTR(4)    /  7  /                                         M28. 911
      DATA  TEXTAX(7)    /  6HREPLOT  /                                 M28. 912
C     $$$$$    SPECIAL-REQUEST WORD NO. 5.   'BEGIN NEW DATA CASE' $$$  M28. 913
      DATA  TEXTAY(5)   /  6HBNDC    /                                  M28. 914
      DATA  JPNTR(5)    /  8  /                                         M28. 915
      DATA  TEXTAX(8)    /  6HBEGIN   /                                 M28. 916
      DATA  TEXTAX(9)    /  6HNEW     /                                 M28. 917
      DATA  TEXTAX(10)   /  6HDATA    /                                 M28. 918
      DATA  TEXTAX(11)   /  6HCASE    /                                 M28. 919
C     $$$$$    SPECIAL-REQUEST WORD NO. 6.   'LINE CONSTANTS'    $$$$$  M28. 920
      DATA  TEXTAY(6)   /  6HLC      /                                  M28. 921
      DATA  JPNTR(6)    /  12  /                                        M28. 922
      DATA  TEXTAX(12)   /  6HLINE    /                                 M28. 923
      DATA  TEXTAX(13)   /  6HCONSTA  /                                 M28. 924
      DATA  TEXTAX(14)   /  6HNTS     /                                 M28. 925
C     $$$$$    SPECIAL-REQUEST WORD NO. 7.   'POSTPROCESS               M28. 926
C                                               PLOT FILE'       $$$$$  M28. 927
      DATA  TEXTAY(7)   /  6HPPF     /                                  M28. 928
      DATA  JPNTR(7)    /  15  /                                        M28. 929
      DATA  TEXTAX(15)   /  6HPOSTPR  /                                 M28. 930
      DATA  TEXTAX(16)   /  6HOCESS   /                                 M28. 931
      DATA  TEXTAX(17)   /  6HPLOT    /                                 M28. 932
      DATA  TEXTAX(18)   /  6HFILE    /                                 M28. 933
C     $$$$$    SPECIAL-REQUEST WORD NO. 8.   'PLOTTER PAPER HEIGHT' $$  M28. 934
      DATA  TEXTAY(8)   /  6HPPH     /                                  M28. 935
      DATA  JPNTR(8)    /  19  /                                        M28. 936
      DATA  TEXTAX(19)   /  6HPLOTTE  /                                 M28. 937
      DATA  TEXTAX(20)   /  6HR       /                                 M28. 938
      DATA  TEXTAX(21)   /  6HPAPER   /                                 M28. 939
      DATA  TEXTAX(22)   /  6HHEIGHT  /                                 M28. 940
C     $$$$$    SPECIAL-REQUEST WORD NO. 9.   'PRINTER LINES PER INCH'   M28. 941
      DATA  TEXTAY(9)   /  6HPLPI    /                                  M28. 942
      DATA  JPNTR(9)    /  23  /                                        M28. 943
      DATA  TEXTAX(23)   /  6HPRINTE  /                                 M28. 944
      DATA  TEXTAX(24)   /  6HR       /                                 M28. 945
      DATA  TEXTAX(25)   /  6HLINES   /                                 M28. 946
      DATA  TEXTAX(26)   /  6HPER     /                                 M28. 947
      DATA  TEXTAX(27)   /  6HINCH    /                                 M28. 948
C     $$$$$    SPECIAL-REQUEST WORD NO. 10.   'MODE VOLTAGE OUTPUT' $$$ M28. 949
      DATA  TEXTAY(10)  /  6HMVO     /                                  M28. 950
      DATA  JPNTR(10)    /  28  /                                       M28. 951
      DATA  TEXTAX(28)   /  6HMODE    /                                 M28. 952
      DATA  TEXTAX(29)   /  6HVOLTAG  /                                 M28. 953
      DATA  TEXTAX(30)   /  6HE       /                                 M28. 954
      DATA  TEXTAX(31)   /  6HOUTPUT  /                                 M28. 955
C     $$$$$    SPECIAL-REQUEST WORD NO. 11.   'END LAST DATA CASE' $$$$ M28. 956
      DATA  TEXTAY(11)  /  6HELDC    /                                  M28. 957
      DATA  JPNTR(11)    /  32  /                                       M28. 958
      DATA  TEXTAX(32)   /  6HEND     /                                 M28. 959
      DATA  TEXTAX(33)   /  6HLAST    /                                 M28. 960
      DATA  TEXTAX(34)   /  6HDATA    /                                 M28. 961
      DATA  TEXTAX(35)   /  6HCASE    /                                 M28. 962
C     $$$$$    SPECIAL-REQUEST WORD NO. 12.   'ANALYTIC SOURCES USAGE'  M28. 963
      DATA  TEXTAY(12)  /  6HASU     /                                  M28. 964
      DATA  JPNTR(12)    /  36  /                                       M28. 965
      DATA  TEXTAX(36)   /  6HANALYT  /                                 M28. 966
      DATA  TEXTAX(37)   /  6HIC      /                                 M28. 967
      DATA  TEXTAX(38)   /  6HSOURCE  /                                 M28. 968
      DATA  TEXTAX(39)   /  6HS       /                                 M28. 969
      DATA  TEXTAX(40)   /  6HUSAGE   /                                 M28. 970
C     $$$$$ SPECIAL-REQUEST WORD NO. 13.   'LIMIT ON PLOT OSCILLATIONS' M28. 971
      DATA  TEXTAY(13)  /  6HLOPO    /                                  M28. 972
      DATA  JPNTR(13)    /  41  /                                       M28. 973
      DATA  TEXTAX(41)   /  6HLIMIT   /                                 M28. 974
      DATA  TEXTAX(42)   /  6HON      /                                 M28. 975
      DATA  TEXTAX(43)   /  6HPLOT    /                                 M28. 976
      DATA  TEXTAX(44)   /  6HOSCILL  /                                 M28. 977
      DATA  TEXTAX(45)   /  6HATIONS  /                                 M28. 978
C     $$$$$    SPECIAL-REQUEST WORD NO. 14.   'TACS EMTP SOURCES'  $$$$ M28. 979
      DATA  TEXTAY(14)  /  6HTES     /                                  M28. 980
      DATA  JPNTR(14)    /  46  /                                       M28. 981
      DATA  TEXTAX(46)   /  6HTACS    /                                 M28. 982
      DATA  TEXTAX(47)   /  6HEMTP    /                                 M28. 983
      DATA  TEXTAX(48)   /  6HSOURCE  /                                 M28. 984
      DATA  TEXTAX(49)   /  6HS       /                                 M28. 985
C     $$$$$    SPECIAL-REQUEST WORD NO. 15.   'START AGAIN'       $$$$$ M28. 986
      DATA  TEXTAY(15)  /  6HSA      /                                  M28. 987
      DATA  JPNTR(15)    /  50  /                                       M28. 988
      DATA  TEXTAX(50)   /  6HSTART   /                                 M28. 989
      DATA  TEXTAX(51)   /  6HAGAIN   /                                 M28. 990
C     $$$$$    SPECIAL-REQUEST WORD NO. 16.   'SEMLYEN SETUP'     $$$$$ M28. 991
      DATA  TEXTAY(16)  /  6HSS      /                                  M28. 992
      DATA  JPNTR(16)    /  52  /                                       M28. 993
      DATA  TEXTAX(52)   /  6HSEMLYE  /                                 M28. 994
      DATA  TEXTAX(53)   /  6HN       /                                 M28. 995
      DATA  TEXTAX(54)   /  6HSETUP   /                                 M28. 996
C     $$$$  SPECIAL REQUEST-WORD NO. 17.   'LINEAR BIAS USAGE' $$$$$$$$$M42.1159
      DATA  TEXTAY(17)   /  6HLBU      /                                M42.1160
      DATA  JPNTR(17)    /  55  /                                       M42.1161
      DATA  TEXTAX(55)   /  6HLINEAR  /                                 M42.1162
      DATA  TEXTAX(56)   /  6HBIAS    /                                 M42.1163
      DATA  TEXTAX(57)   /  6HUSAGE   /                                 M42.1164
C     $$$$$    SPECIAL-REQUEST WORD NO. 18.   'CABLE CONSTANTS'   $$$$$ M28.1003
      DATA  TEXTAY(18)  /  6HCC      /                                  M28.1004
      DATA  JPNTR(18)    /  58  /                                       M28.1005
      DATA  TEXTAX(58)   /  6HCABLE   /                                 M28.1006
      DATA  TEXTAX(59)   /  6HCONSTA  /                                 M28.1007
      DATA  TEXTAX(60)   /  6HNTS     /                                 M28.1008
C     $$$$$    SPECIAL-REQUEST WORD NO. 19.   'AUTO NAME'         $$$$$ M42.1165
      DATA  TEXTAY(19)  /  6HAN      /                                  M42.1166
      DATA  JPNTR(19)    /  61  /                                       M28.1011
      DATA  TEXTAX(61)   /  6HAUTO    /                                 M42.1167
      DATA  TEXTAX(62)   /  6HNAME    /                                 M42.1168
C     $$$$$    SPECIAL-REQUEST WORD NO. 20.   'RENUMBER BYPASS'   $$$$$ M28.1014
      DATA  TEXTAY(20)  /  6HRB      /                                  M28.1015
      DATA  JPNTR(20)    /  63  /                                       M28.1016
      DATA  TEXTAX(63)   /  6HRENUMB  /                                 M28.1017
      DATA  TEXTAX(64)   /  6HER      /                                 M28.1018
      DATA  TEXTAX(65)   /  6HBYPASS  /                                 M28.1019
C     $$$$$    SPECIAL-REQUEST WORD NO. 21.   'FREQUENCY SCAN'    $$$$$ M28.1020
      DATA  TEXTAY(21)  /  6HFS      /                                  M28.1021
      DATA  JPNTR(21)    /  66  /                                       M28.1022
      DATA  TEXTAX(66)   /  6HFREQUE  /                                 M28.1023
      DATA  TEXTAX(67)   /  6HNCY     /                                 M28.1024
      DATA  TEXTAX(68)   /  6HSCAN    /                                 M28.1025
C     $$$$$    SPECIAL-REQUEST WORD NO. 22.   'FREE FORMAT'       $$$$$ M28.1026
      DATA  TEXTAY(22)  /  6HFF      /                                  M28.1027
      DATA  JPNTR(22)    /  69  /                                       M28.1028
      DATA  TEXTAX(69)   /  6HFREE    /                                 M28.1029
      DATA  TEXTAX(70)   /  6HFORMAT  /                                 M28.1030
C     $$$$$    SPECIAL-REQUEST WORD NO. 23.   'DIAGNOSTIC'        $$$$$ M28.1031
      DATA  TEXTAY(23)  /  6HD       /                                  M28.1032
      DATA  JPNTR(23)    /  71  /                                       M28.1033
      DATA  TEXTAX(71)   /  6HDIAGNO  /                                 M28.1034
      DATA  TEXTAX(72)   /  6HSTIC    /                                 M28.1035
C     $$$$$    SPECIAL-REQUEST WORD NO. 24.   'POWER FREQUENCY'   $$$$$ M28.1036
      DATA  TEXTAY(24)  /  6HPF      /                                  M28.1037
      DATA  JPNTR(24)    /  73  /                                       M28.1038
      DATA  TEXTAX(73)   /  6HPOWER   /                                 M28.1039
      DATA  TEXTAX(74)   /  6HFREQUE  /                                 M28.1040
      DATA  TEXTAX(75)   /  6HNCY     /                                 M28.1041
C     $$$$$    SPECIAL-REQUEST WORD NO. 25.   'FILE REQUEST'      $$$$$ M28.1042
      DATA  TEXTAY(25)  /  6HFR      /                                  M28.1043
      DATA  JPNTR(25)    /  76  /                                       M28.1044
      DATA  TEXTAX(76)   /  6HFILE    /                                 M28.1045
      DATA  TEXTAX(77)   /  6HREQUES  /                                 M28.1046
      DATA  TEXTAX(78)   /  6HT       /                                 M28.1047
C     $$$$$    SPECIAL-REQUEST WORD NO. 26.   'USER IDENTIFICATION' $$$ M28.1048
      DATA  TEXTAY(26)  /  6HUI      /                                  M28.1049
      DATA  JPNTR(26)    /  79  /                                       M28.1050
      DATA  TEXTAX(79)   /  6HUSER    /                                 M28.1051
      DATA  TEXTAX(80)   /  6HIDENTI  /                                 M28.1052
      DATA  TEXTAX(81)   /  6HFICATI  /                                 M28.1053
      DATA  TEXTAX(82)   /  6HON      /                                 M28.1054
C     $$$$$    SPECIAL-REQUEST WORD NO. 27.   'CONVERT ZNO'  $$$$$$$$$$ M39. 354
      DATA  TEXTAY(27)  /  6HCZ      /                                  M39. 355
      DATA  JPNTR(27)    /  83  /                                       M28.1057
      DATA  TEXTAX(83)   /  6HCONVER  /                                 M39. 356
      DATA  TEXTAX(84)   /  6HT       /                                 M39. 357
      DATA  TEXTAX(85)   /  6HZNO     /                                 M39. 358
C     $$$$$    SPECIAL-REQUEST WORD NO. 28.   'ABORT DATA CASE'   $$$$$ M28.1061
      DATA  TEXTAY(28)  /  6HADC     /                                  M28.1062
      DATA  JPNTR(28)    /  86  /                                       M28.1063
      DATA  TEXTAX(86)   /  6HABORT   /                                 M28.1064
      DATA  TEXTAX(87)   /  6HDATA    /                                 M28.1065
      DATA  TEXTAX(88)   /  6HCASE    /                                 M28.1066
C     $$$$$    SPECIAL-REQUEST WORD NO. 29.   'KILL CODES'        $$$$$ M28.1067
      DATA  TEXTAY(29)  /  6HKC      /                                  M28.1068
      DATA  JPNTR(29)    /  89  /                                       M28.1069
      DATA  TEXTAX(89)   /  6HKILL    /                                 M28.1070
      DATA  TEXTAX(90)   /  6HCODES   /                                 M28.1071
C     $$$$$    SPECIAL-REQUEST WORD NO. 30.   'HIGH RESISTANCE'   $$$$$ M28.1072
      DATA  TEXTAY(30)  /  6HHR      /                                  M28.1073
      DATA  JPNTR(30)    /  91  /                                       M28.1074
      DATA  TEXTAX(91)   /  6HHIGH    /                                 M28.1075
      DATA  TEXTAX(92)   /  6HRESIST  /                                 M28.1076
      DATA  TEXTAX(93)   /  6HANCE    /                                 M28.1077
C     $$$$$    SPECIAL-REQUEST WORD NO. 31.   'AVERAGE OUTPUT'    $$$$$ M28.1078
      DATA  TEXTAY(31)  /  6HAO      /                                  M28.1079
      DATA  JPNTR(31)    /  94  /                                       M28.1080
      DATA  TEXTAX(94)   /  6HAVERAG  /                                 M28.1081
      DATA  TEXTAX(95)   /  6HE       /                                 M28.1082
      DATA  TEXTAX(96)   /  6HOUTPUT  /                                 M28.1083
C     $$$$$  SPECIAL REQUEST-WORD NO. 32.  'ABSOLUTE TACS DIMENSIONS'  $M28.1084
      DATA  TEXTAY(32)   /  6HATD     /                                 M28.1085
      DATA  JPNTR(32)    /  97  /                                       M28.1086
      DATA  TEXTAX(97)   /  6HABSOLU  /                                 M28.1087
      DATA  TEXTAX(98)   /  6HTE      /                                 M28.1088
      DATA  TEXTAX(99)   /  6HTACS    /                                 M28.1089
      DATA  TEXTAX(100)  /  6HDIMENS  /                                 M28.1090
      DATA  TEXTAX(101)  /  6HIONS    /                                 M28.1091
C     $$$$$  SPECIAL REQUEST-WORD NO. 33.  'RELATIVE TACS DIMENSIONS'  $M28.1092
      DATA  TEXTAY(33)   /  6HRTD     /                                 M28.1093
      DATA  JPNTR(33)    /  102  /                                      M28.1094
      DATA  TEXTAX(102)  /  6HRELATI  /                                 M28.1095
      DATA  TEXTAX(103)  /  6HVE      /                                 M28.1096
      DATA  TEXTAX(104)  /  6HTACS    /                                 M28.1097
      DATA  TEXTAX(105)  /  6HDIMENS  /                                 M28.1098
      DATA  TEXTAX(106)  /  6HIONS    /                                 M28.1099
C     $$ SPECIAL REQUEST-WORD NO. 34.  'TABULATE ENERGIZATION RESULTS'  M28.1100
      DATA  TEXTAY(34)   /  6HTER     /                                 M28.1101
      DATA  JPNTR(34)    /  107  /                                      M28.1102
      DATA  TEXTAX(107)  /  6HTABULA  /                                 M28.1103
      DATA  TEXTAX(108)  /  6HTE      /                                 M28.1104
      DATA  TEXTAX(109)  /  6HENERGI  /                                 M28.1105
      DATA  TEXTAX(110)  /  6HZATION  /                                 M28.1106
      DATA  TEXTAX(111)  /  6HRESULT  /                                 M28.1107
      DATA  TEXTAX(112)  /  6HS       /                                 M28.1108
C     $$ SPECIAL REQUEST-WORD NO. 35. 'STATISTICS OUTPUT SALVAGE'       M28.1109
      DATA  TEXTAY(35)   /  6HSOS     /                                 M28.1110
      DATA  JPNTR(35)    /  113  /                                      M28.1111
      DATA  TEXTAX(113)  /  6HSTATIS  /                                 M28.1112
      DATA  TEXTAX(114)  /  6HTICS    /                                 M28.1113
      DATA  TEXTAX(115)  /  6HOUTPUT  /                                 M28.1114
      DATA  TEXTAX(116)  /  6HSALVAG  /                                 M28.1115
      DATA  TEXTAX(117)  /  6HE       /                                 M28.1116
C     $$$$  SPECIAL REQUEST-WORD NO. 36.   'OMIT BASE CASE'    $$$$$$$$$M28.1117
      DATA  TEXTAY(36)   /  6HOBC     /                                 M28.1118
      DATA  JPNTR(36)    /  118  /                                      M28.1119
      DATA  TEXTAX(118)  /  6HOMIT    /                                 M28.1120
      DATA  TEXTAX(119)  /  6HBASE    /                                 M28.1121
      DATA  TEXTAX(120)  /  6HCASE    /                                 M28.1122
C     $$$$  SPECIAL REQUEST-WORD NO. 37.   'CHANGE SWITCH'     $$$$$$$$$M42.1169
      DATA  TEXTAY(37)   /  6HCS      /                                 M42.1170
      DATA  JPNTR(37)    /  121  /                                      M28.1126
      DATA  TEXTAX(121)  /  6HCHANGE  /                                 M42.1171
      DATA  TEXTAX(122)  /  6HSWITCH  /                                 M42.1172
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 38.   'MISCELLANEOUS DATA       M28.1129
C                                              CARDS'          $$$$$$$$$M28.1130
      DATA  TEXTAY(38)   /  6HMDC     /                                 M28.1131
      DATA  JPNTR(38)    /  123  /                                      M28.1132
      DATA  TEXTAX(123)  /  6HMISCEL  /                                 M28.1133
      DATA  TEXTAX(124)  /  6HLANEOU  /                                 M28.1134
      DATA  TEXTAX(125)  /  6HS       /                                 M28.1135
      DATA  TEXTAX(126)  /  6HDATA    /                                 M28.1136
      DATA  TEXTAX(127)  /  6HCARDS   /                                 M28.1137
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 39.   'REDEFINE TOLERANCE       M28.1138
C                                              EPSILN'         $$$$$$$$$M28.1139
      DATA  TEXTAY(39)   /  6HRTE     /                                 M28.1140
      DATA  JPNTR(39)    /  128  /                                      M28.1141
      DATA  TEXTAX(128)  /  6HREDEFI  /                                 M28.1142
      DATA  TEXTAX(129)  /  6HNE      /                                 M28.1143
      DATA  TEXTAX(130)  /  6HTOLERA  /                                 M28.1144
      DATA  TEXTAX(131)  /  6HNCE     /                                 M28.1145
      DATA  TEXTAX(132)  /  6HEPSILN  /                                 M28.1146
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 40.   'CHANGE PRINTOUT          M28.1147
C                                              FREQUENCY       $$$$$$$$$M28.1148
      DATA  TEXTAY(40)   /  6HCPF     /                                 M28.1149
      DATA  JPNTR(40)    /  133  /                                      M28.1150
      DATA  TEXTAX(133)  /  6HCHANGE  /                                 M28.1151
      DATA  TEXTAX(134)  /  6HPRINTO  /                                 M28.1152
      DATA  TEXTAX(135)  /  6HUT      /                                 M28.1153
      DATA  TEXTAX(136)  /  6HFREQUE  /                                 M28.1154
      DATA  TEXTAX(137)  /  6HNCY     /                                 M28.1155
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 41.   'BEGIN PEAK               M28.1156
C                                              VALUE SEARCH'   $$$$$$$$$M28.1157
      DATA  TEXTAY(41)   /  6HBPVS    /                                 M28.1158
      DATA  JPNTR(41)    /  138  /                                      M28.1159
      DATA  TEXTAX(138)  /  6HBEGIN   /                                 M28.1160
      DATA  TEXTAX(139)  /  6HPEAK    /                                 M28.1161
      DATA  TEXTAX(140)  /  6HVALUE   /                                 M28.1162
      DATA  TEXTAX(141)  /  6HSEARCH  /                                 M28.1163
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 42.   'TIME OF                  M28.1164
C                                              DICE ROLL'    $$$$$$$$$$$M28.1165
      DATA  TEXTAY(42)   /  6HTODR    /                                 M28.1166
      DATA  JPNTR(42)    /  142  /                                      M28.1167
      DATA  TEXTAX(142)  /  6HTIME    /                                 M28.1168
      DATA  TEXTAX(143)  /  6HOF      /                                 M28.1169
      DATA  TEXTAX(144)  /  6HDICE    /                                 M28.1170
      DATA  TEXTAX(145)  /  6HROLL    /                                 M28.1171
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 43.   'ZINC OXIDE'   $$$$$$$$$$$M37.1183
      DATA  TEXTAY(43)   /  6HZO      /                                 M37.1184
      DATA  JPNTR(43)    /  146  /                                      M28.1174
      DATA  TEXTAX(146)  /  6HZINC    /                                 M37.1185
      DATA  TEXTAX(147)  /  6HOXIDE   /                                 M37.1186
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 44.   'PEAK VOLTAGE'  $$$$$$$$$$M28.1177
C     $$$$$$$                                  MONITOR'      $$$$$$$$$$$M28.1178
      DATA  TEXTAY(44)   /  6HPVM     /                                 M28.1179
      DATA  JPNTR(44)    /  148  /                                      M28.1180
      DATA  TEXTAX(148)  /  6HPEAK    /                                 M28.1181
      DATA  TEXTAX(149)  /  6HVOLTAG  /                                 M28.1182
      DATA  TEXTAX(150)  /  6HE       /                                 M28.1183
      DATA  TEXTAX(151)  /  6HMONITO  /                                 M28.1184
      DATA  TEXTAX(152)  /  6HR       /                                 M28.1185
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 45.   'ABSOLUTE U.M.  $$$$$$$$$$M28.1186
C                                              DIMENSIONS'    $$$$$$$$$$M28.1187
      DATA  TEXTAY(45)   /  6HAUMD    /                                 M28.1188
      DATA  JPNTR(45)    /  153  /                                      M28.1189
      DATA  TEXTAX(153)  /  6HABSOLU  /                                 M28.1190
      DATA  TEXTAX(154)  /  6HTE      /                                 M28.1191
      DATA  TEXTAX(155)  /  6HU.M.    /                                 M28.1192
      DATA  TEXTAX(156)  /  6HDIMENS  /                                 M28.1193
      DATA  TEXTAX(157)  /  6HIONS    /                                 M28.1194
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 46.   'RELATIVE U.M.  $$$$$$$$$$M28.1195
C                                              DIMENSIONS'    $$$$$$$$$$M28.1196
      DATA  TEXTAY(46)   /  6HRUMD    /                                 M28.1197
      DATA  JPNTR(46)    /  158  /                                      M28.1198
      DATA  TEXTAX(158)  /  6HRELATI  /                                 M28.1199
      DATA  TEXTAX(159)  /  6HVE      /                                 M28.1200
      DATA  TEXTAX(160)  /  6HU.M.    /                                 M28.1201
      DATA  TEXTAX(161)  /  6HDIMENS  /                                 M28.1202
      DATA  TEXTAX(162)  /  6HIONS    /                                 M28.1203
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 47.   'TIME STEP LOOP'  $$$$$$$$M28.1204
      DATA  TEXTAY(47)   /  6HTSL     /                                 M28.1205
      DATA  JPNTR(47)    /  163  /                                      M28.1206
      DATA  TEXTAX(163)  /  6HTIME    /                                 M28.1207
      DATA  TEXTAX(164)  /  6HSTEP    /                                 M28.1208
      DATA  TEXTAX(165)  /  6HLOOP    /                                 M28.1209
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 48.   'ALTERNATE DIAGNOSTIC     M28.1210
C                                                    PRINTOUT'  $$$$$$$$M28.1211
      DATA  TEXTAY(48)   /  6HADP     /                                 M28.1212
      DATA  JPNTR(48)    /  166  /                                      M28.1213
      DATA  TEXTAX(166)  /  6HALTERN  /                                 M28.1214
      DATA  TEXTAX(167)  /  6HATE     /                                 M28.1215
      DATA  TEXTAX(168)  /  6HDIAGNO  /                                 M28.1216
      DATA  TEXTAX(169)  /  6HSTIC    /                                 M28.1217
      DATA  TEXTAX(170)  /  6HPRINTO  /                                 M28.1218
      DATA  TEXTAX(171)  /  6HUT      /                                 M28.1219
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 49.   'TACS WARN LIMIT' $$$$$$$$M31. 257
      DATA  TEXTAY(49)   /  6HTWL     /                                 M31. 258
      DATA  JPNTR(49)    /  172  /                                      M29.1010
      DATA  TEXTAX(172)  /  6HTACS    /                                 M31. 259
      DATA  TEXTAX(173)  /  6HWARN    /                                 M31. 260
      DATA  TEXTAX(174)  /  6HLIMIT   /                                 M31. 261
C     $$$$$    SPECIAL-REQUEST WORD NO. 50.   'MARTI SETUP'       $$$$$ M29.1014
      DATA  TEXTAY(50)  /  6HMS      /                                  M29.1015
      DATA  JPNTR(50)    /  175  /                                      M29.1016
      DATA  TEXTAX(175)  /  6HMARTI   /                                 M29.1017
      DATA  TEXTAX(176)  /  6HSETUP   /                                 M29.1018
C     $$$$$    SPECIAL-REQUEST WORD NO. 51.   'JMARTI SETUP' $$$$$$$$$$ M32. 707
      DATA  TEXTAY(51)  /  6HJMS     /                                  M32. 708
      DATA  JPNTR(51)    /  177  /                                      M29.1021
      DATA  TEXTAX(177)  /  6HJMARTI  /                                 M32. 709
      DATA  TEXTAX(178)  /  6HSETUP   /                                 M32. 710
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 52.   'CUSTOM PLOT FILE' $$$$$$ M29.1024
      DATA  TEXTAY(52)   /  6HCPF     /                                 M29.1025
      DATA  JPNTR(52)    /  179  /                                      M29.1026
      DATA  TEXTAX(179)  /  6HCUSTOM  /                                 M29.1027
      DATA  TEXTAX(180)  /  6HPLOT    /                                 M29.1028
      DATA  TEXTAX(181)  /  6HFILE    /                                 M29.1029
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 53.   'OUTPUT WIDTH 132' $$$$$$ M32. 711
      DATA  TEXTAY(53)   /  6HOW1     /                                 M32. 712
      DATA  JPNTR(53)    /  182  /                                      M32. 713
      DATA  TEXTAX(182)  /  6HOUTPUT  /                                 M32. 714
      DATA  TEXTAX(183)  /  6HWIDTH   /                                 M32. 715
      DATA  TEXTAX(184)  /  6H132     /                                 M32. 716
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 54.   'OUTPUT WIDTH 80'  $$$$$$ M32. 717
      DATA  TEXTAY(54)   /  6HOW8     /                                 M32. 718
      DATA  JPNTR(54)    /  185  /                                      M32. 719
      DATA  TEXTAX(185)  /  6HOUTPUT  /                                 M32. 720
      DATA  TEXTAX(186)  /  6HWIDTH   /                                 M32. 721
      DATA  TEXTAX(187)  /  6H80      /                                 M32. 722
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 55.   'MODIFY SWITCH LOGIC  $$$ M32. 723
      DATA  TEXTAY(55)   /  6HMSL     /                                 M32. 724
      DATA  JPNTR(55)    /  188  /                                      M32. 725
      DATA  TEXTAX(188)  /  6HMODIFY  /                                 M32. 726
      DATA  TEXTAX(189)  /  6HSWITCH  /                                 M32. 727
      DATA  TEXTAX(190)  /  6HLOGIC   /                                 M32. 728
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 56.   'FAULT DATA USAGE' $$$$$$ M32. 729
      DATA  TEXTAY(56)   /  6HFDU     /                                 M32. 730
      DATA  JPNTR(56)    /  191  /                                      M32. 731
      DATA  TEXTAX(191)  /  6HFAULT   /                                 M32. 732
      DATA  TEXTAX(192)  /  6HDATA    /                                 M32. 733
      DATA  TEXTAX(193)  /  6HUSAGE   /                                 M32. 734
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 57.   'FIX SOURCE'  $$$$$$      M35. 517
      DATA  TEXTAY(57)   /  6HFXS     /                                 M37.1187
      DATA  JPNTR(57)    /  194       /                                 M37.1188
      DATA  TEXTAX(194)  /  6HFIX     /                                 M35. 518
      DATA  TEXTAX(195)  /  6HSOURCE  /                                 M35. 519
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 58.   'USER SUPPLIED     $$$$$$ M37.1189
C     $$$$$$$                                  SWITCH TIMES'     $$$$$$ M37.1190
      DATA  TEXTAY(58)   /  6HUSST    /                                 M37.1191
      DATA  JPNTR(58)    /  196  /                                      M37.1192
      DATA  TEXTAX(196)  /  6HUSER    /                                 M37.1193
      DATA  TEXTAX(197)  /  6HSUPPLI  /                                 M37.1194
      DATA  TEXTAX(198)  /  6HED      /                                 M37.1195
      DATA  TEXTAX(199)  /  6HSWITCH  /                                 M37.1196
      DATA  TEXTAX(200)  /  6HTIMES   /                                 M37.1197
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 59.   'AMETANI SETUP'  $$$      M42.1173
      DATA  TEXTAY(59)   /  6HAS      /                                 M42.1174
      DATA  JPNTR(59)    /  201       /                                 M42.1175
      DATA  TEXTAX(201)  /  6HAMETAN  /                                 M42.1176
      DATA  TEXTAX(202)  /  6HI       /                                 M42.1177
      DATA  TEXTAX(203)  /  6HSETUP   /                                 M42.1178
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 60.   'HAUER SETUP'  $$$$$      M42.1179
      DATA  TEXTAY(60)   /  6HHS      /                                 M42.1180
      DATA  JPNTR(60)    /  204       /                                 M42.1181
      DATA  TEXTAX(204)  /  6HHAUER   /                                 M42.1182
      DATA  TEXTAX(205)  /  6HSETUP   /                                 M42.1183
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 61.   'LINE MODEL FREQ. SCAN  $$
      DATA  TEXTAY(61)   /  6HLMFS    /
      DATA  JPNTR(61)    /  206       /                                 M42.1184
      DATA  TEXTAX(206)  / 6HLINE    /
      DATA  TEXTAX(207)  / 6HMODEL  /
      DATA  TEXTAX(208)  / 6HFREQ   /
      DATA  TEXTAX(209)  / 6HSCAN   /
      DATA  JPNTR(62)    /  210      /
      DATA  JPNTR(63)    /    0        /             
      DATA LL1  /  1 /                                                  M28.1222
      DATA LL8  /  8 /                                                  M28.1224
      DATA LL16 / 16 /                                                  M28.1226
      DATA LL25 / 25 /                                                  M28.1229
      DATA LL32 / 32 /                                                  M28.1231
      DATA LL33 / 33 /                                                  M28.1232
      DATA LL40 / 40 /                                                  M28.1233
      DATA LL48 / 48 /                                                  M28.1234
      DATA LL49 / 49 /                                                  M28.1235
      DATA LL56 / 56 /                                                  M28.1236
      DATA LL80 / 80 /                                                  M28.1240
      LSTAT(18) = 0                                                     M28.1241
      IF ( IPRSUP  .GE.  1 )                                            M32. 737
     1 WRITE ( LUNIT6, 4567 )                                           M32. 738
 4567 FORMAT ( 24H  "BEGIN MODULE REQUES." )                            M32. 739
      DO 3307  I=1, 9999                                                M28.1242
      N1 = JPNTR(I)                                                     M28.1243
      N2 = JPNTR(I+1) - 1                                               M28.1244
C     NEXT CHECK IF LAST REQUEST WORD HAS BEEN EXHAUSED.  IF SO,        M28.1245
C     EXIT WITH  LSTAT(18)  EQUAL TO ZERO.                              M28.1246
      IF ( N2  .LT.  0 )   GO TO 9200                                   M28.1247
      IF ( IPRSUP  .GE.  35 )                                           M28.1248
     1 WRITE (LUNIT6, 3285)  I,  ( TEXTAX(J), J=N1, N2 )                M28.1249
 3285 FORMAT ( /,  21H SPECIAL-REQUEST WORD,  I4,  2H .,  10A6  )       M28.1250
      IF ( TEXTAX(N1)  .EQ.  BLANK )   GO TO 3306                       M28.1251
      L = 0                                                             M28.1252
      N3 = N2 - N1 + 1                                                  M28.1253
      IF ( N3  .NE.  NFRFLD )   GO TO 3306                              M28.1254
      DO 3291  J=N1, N2                                                 M28.1255
      L = L + 1                                                         M28.1256
      IF ( TEXTA6(L)  .NE.  TEXTAX(J) )   GO TO 3306                    M28.1257
 3291 CONTINUE                                                          M28.1258
 3294 LSTAT(18) = I                                                     M28.1259
C     NEXT CHECK FOR EXCEPTIONAL REQUEST WORDS WHICH ARE                M28.1260
C     PROCESSED OUTSIDE OF SUBROUTINE, BY CALLING MODULE.               M28.1261
      IF ( I  .EQ.  15 )   GO TO 9200                                   M28.1262
      IF ( I  .EQ.  32 )   GO TO 9200                                   M28.1263
      IF ( I  .EQ.  33 )   GO TO 9200                                   M28.1264
      GO TO (                                                           M28.1265
     1 8001, 8002, 8003, 8004, 8005, 8006, 8007, 8008, 8009, 8010,      M28.1266
     2 8011, 8012, 8013, 8014, 9200, 8016, 8017, 8018, 8019, 8020,      M28.1267
     3 8021, 8022, 8023, 8024, 8025, 8026, 8027, 8028, 8029, 8030,      M28.1268
     4 8031, 9200, 9200, 8034, 8035, 8036, 8037, 8038, 8039, 8040,      M28.1269
     5 8041, 8042, 8043, 8044, 8045, 8046, 8047, 8048, 8049, 8050,      M28.1270
     6 8051, 8052, 8053, 8054, 8055, 8056, 8057, 8058, 8059, 8060,      M29.1032
     7 8061, 8062  ),  I                  
 3306 IF ( TEXTA6(1)  .EQ.  TEXTAY(I) )   GO TO 3294                    M28.1272
 3307 CONTINUE                                                          M28.1273
C     CONTROL WILL NEVER REACH  "STOP"  WHICH FOLLOWS.                  M28.1274
      CALL STOPTP                                                               
C     $$$$$    SPECIAL-REQUEST WORD NO. 1.   'XFORMER'           $$$$$  M28.1276
 8001 IF ( NOUTPR  .EQ.  0 )                                            M28.1277
     1 WRITE (KUNIT6, 83056)                                            M35. 524
83056 FORMAT(  50H+REQUEST FOR TRANSFORMER IMPEDANCE-MATRIX ROUTINE.   )M28.1279
      NCHAIN = 41                                                       M28.1280
      GO TO 5617                                                        M28.1281
C     $$$$$    SPECIAL-REQUEST WORD NO. 2.   'SATURATION'        $$$$$  M28.1282
 8002 IF ( NOUTPR  .EQ.  0 )                                            M28.1283
     1 WRITE (KUNIT6, 83057)                                            M35. 525
83057 FORMAT(  41H+REQUEST FOR MAGNETIC-SATURATION ROUTINE.  )          M28.1285
      NCHAIN = 42                                                       M28.1286
      GO TO 5617                                                        M28.1287
C     $$$$$    SPECIAL-REQUEST WORD NO. 3.   'TYPE99 LIMIT'      $$$$$  M43. 121
 8003 IF ( KOLBEG  .GT.  0 )   GO TO 3352                               M43. 122
      DECODE (80, 2642, ABUFF(1))  MAX99M                               M28.1319
      GO TO 3354                                                        M28.1320
 3352 NFRFLD = 1                                                        M28.1321
      CALL FREONE ( D1 )                                                M29.1034
      MAX99M = D1                                                       M28.1323
 3354 IF ( NOUTPR  .EQ.  0 )                                            M43. 123
     1 WRITE (KUNIT6, 3355)  MAX99M                                     M35. 527
 3355 FORMAT ( '+REDEFINE TYPE-99 MESSAGE LIMIT TO', I6   )             M28.1298
      GO TO 15                                                          M28.1309
C     $$$$$    SPECIAL-REQUEST WORD NO. 4.   'REPLOT'            $$$$$  M28.1295
 8004 IF ( NOUTPR  .EQ.  0 )                                            M28.1296
     1 WRITE (KUNIT6, 3364)                                             M35. 527
 3364 FORMAT (  34H+REQUEST TO RE-PLOT OLD PLOT DATA.    )              M28.1298
      DEGMAX = 0.0                                                      M28.1299
      IALTER = LUNIT4                                                   M28.1300
      CALL MIDOV1                                                       M28.1301
      CALL PFATCH                                                       M28.1302
      NCHAIN = 31                                                       M28.1303
      GO TO 5617                                                        M28.1304
C     $$$$$    SPECIAL-REQUEST WORD NO. 5.   'BEGIN NEW DATA CASE' $$$  M28.1305
 8005 IF ( NOUTPR  .EQ.  0 )                                            M28.1306
     1 WRITE (KUNIT6, 4154 )                                            M35. 528
 4154 FORMAT(  37H+MARKER CARD PRECEDING NEW DATA CASE.    )            M28.1308
      GO TO 15                                                          M28.1309
C     $$$$$    SPECIAL-REQUEST WORD NO. 6.   'LINE CONSTANTS'    $$$$$  M28.1310
 8006 NCHAIN = 44                                                       M35. 529
      IF ( IPRSUP .GE. 1 )  WRITE (LUNIT6, 3411)                        M35. 530
 3411 FORMAT ( 36H SET NCHAIN=44 FOR "LINE CONSTANTS".   )              M35. 531
      GO TO 5617                                                        M28.1315
C     $$$$$    SPECIAL-REQUEST WORD NO. 7.   'POSTPROCESS               M28.1316
C                                               PLOT FILE'       $$$$$  M28.1317
 8007 IF ( KOLBEG  .GT.  0 )   GO TO 3375                               M28.1318
      DECODE (80, 2642, ABUFF(1))  IOFBND                               M28.1319
      GO TO 3379                                                        M28.1320
 3375 NFRFLD = 1                                                        M28.1321
      CALL FREONE ( D1 )                                                M29.1034
      IOFBND = D1                                                       M28.1323
 3379 WRITE (KUNIT6, 3382)  IOFBND                                      M35. 532
 3382 FORMAT ( 22H+POSTPROCESS.  IPLOT =,  I6 )                         M28.1325
      READ (LUNIT2)  BUS1, BUS2, BUS3, BUS4, N1, N2,                    M28.1326
     1               N3, N4, (BUS5, I=1, N1)                            M28.1327
      N13 = N2 + N4                                                     M32. 740
      IF ( IPRSUP  .GT.  0 )                                            M28.1329
     1 WRITE (LUNIT6, 3383)  BUS1, BUS2, N1, N2, N3, N4                 M28.1330
 3383 FORMAT ( /,  25H AFTER 1ST RECORD.  DATE=,  2A4, 4I5 )            M28.1331
      IF ( N2  .GT.  0 )                                                M28.1332
     1 READ (LUNIT2)  ( N5, I=1, N2 )                                   M28.1333
      IF ( N4  .GT.  0 )                                                M28.1334
     1 READ (LUNIT2)  ( N5, I=1, N4 )                                   M28.1335
      IF ( M4PLOT  .EQ.  0 )  GO TO 3385                                M32. 741
      IOFGND = 1                                                        M32. 742
      CALL PLTLU2 ( EPSUBA, FARRAY(1) )                                 M32. 743
      GO TO 3386                                                        M32. 744
 3385 READ (LUNIT2)  EPSUBA, FARRAY(1)                                  M32. 745
 3386 LTDELT = -6789                                                    M32. 746
      IOFGND = N13                                                      M32. 747
      GO TO 15                                                          M28.1338
C     $$$$$    SPECIAL-REQUEST WORD NO. 8.   'PLOTTER PAPER HEIGHT' $$  M28.1339
 8008 IF ( KOLBEG  .GT.  0 )   GO TO 2608                               M28.1340
      CALL EXPCHK ( LL25, LL80, LL8 )                                   M28.1341
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28.1342
      DECODE (80, 2605, ABUFF)  SZPLT                                   M28.1343
 2605 FORMAT ( 32X, E8.0 )                                              M28.1344
      GO TO 2612                                                        M28.1345
 2608 NFRFLD = 1                                                        M28.1346
      CALL FREONE ( SZPLT )                                             M29.1035
 2612 IF ( NOUTPR  .EQ.  0 )                                            M28.1348
     1 WRITE (KUNIT6, 2614)  SZPLT                                      M35. 533
 2614 FORMAT ( 32H+NEW PLOTTER PAPER-HEIGHT LIMIT.   , 2X, E13.3 )      M28.1350
      GO TO 15                                                          M28.1351
C     $$$$$    SPECIAL-REQUEST WORD NO. 9.   'PRINTER LINES PER INCH'   M28.1352
 8009 IF ( KOLBEG  .GT.  0 )   GO TO 2628                               M28.1353
      CALL INTCHK ( LL25, LL80, LL8 )                                   M28.1354
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28.1355
      DECODE (80, 2642, ABUFF)  LNPIN                                   M28.1356
      GO TO 2631                                                        M28.1357
 2628 NFRFLD = 1                                                        M28.1358
      CALL FREFLD ( VOLTBC(1) )                                         M31. 266
      LNPIN = VOLTBC(1)                                                 M31. 267
 2631 IF ( NOUTPR  .EQ.  0 )                                            M28.1361
     1 WRITE (KUNIT6, 2634)  LNPIN                                      M35. 534
 2634 FORMAT (  38H+NEW PRINTER SPACING, LINES/DISTANCE =, 2X, I8 )     M28.1363
      GO TO 15                                                          M28.1364
C     $$$$$    SPECIAL-REQUEST WORD NO. 10.   'MODE VOLTAGE OUTPUT' $$$ M28.1365
 8010 IF ( KOLBEG  .GT.  0 )   GO TO 2644                               M28.1366
      CALL INTCHK ( LL25, LL80, LL8 )                                   M28.1367
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28.1368
      DECODE (80, 2642, ABUFF)  MODOUT                                  M28.1369
 2642 FORMAT ( 32X, 6I8 )                                               M28.1370
      GO TO 2646                                                        M28.1371
 2644 NFRFLD = 1                                                        M28.1372
      CALL FREFLD ( VOLTBC(1) )                                         M31. 268
      MODOUT = VOLTBC(1)                                                M31. 269
 2646 IF ( MODOUT  .LE.  0 )   MODOUT = 3                               M28.1375
      IF ( NOUTPR  .EQ.  0 )                                            M28.1376
     1 WRITE (KUNIT6, 2648)  MODOUT                                     M35. 535
 2648 FORMAT ( 39H+REQUEST FOR TRICKY KARRENBAUER OUTPUT,   , I3,       M28.1378
     1 8H  MODES.    )                                                  M28.1379
      GO TO 15                                                          M28.1380
C     $$$$$    SPECIAL-REQUEST WORD NO. 11.   'END LAST DATA CASE' $$$$ M28.1381
 8011 IF ( NOUTPR  .EQ.  0 )                                            M28.1382
     1 WRITE (KUNIT6, 2654)                                             M35. 536
 2654 FORMAT (  40H+SPECIAL TERMINATOR RECORD FOR ALL DATA.    )        M28.1384
      GO TO 15                                                          M28.1385
C     $$$$$    SPECIAL-REQUEST WORD NO. 12.   'ANALYTIC SOURCES USAGE'  M28.1386
 8012 IF ( NOUTPR  .EQ.  0 )                                            M28.1387
     1 WRITE (KUNIT6, 2667)                                             M35. 537
 2667 FORMAT (  47H+REQUEST FOR USE OF ANALYTIC TYPE 1-10 SOURCES.   )  M28.1389
      KANAL = 1                                                         M28.1390
      GO TO 15                                                          M28.1391
C     $$$$$ SPECIAL-REQUEST WORD NO. 13.   'LIMIT ON PLOT OSCILLATIONS' M28.1392
 8013 IF ( KOLBEG  .GT.  0 )   GO TO 2671                               M28.1393
      DECODE (80, 2642, ABUFF(1) )  NSMTH                               M28.1394
      GO TO 2673                                                        M28.1395
 2671 NFRFLD = 1                                                        M28.1396
      CALL FREFLD ( VOLTBC(1) )                                         M31. 270
      NSMTH = VOLTBC(1)                                                 M31. 271
 2673 IF ( NOUTPR  .EQ.  0 )                                            M28.1399
     1 WRITE (KUNIT6, 2675 )  NSMTH                                     M35. 538
 2675 FORMAT ( 37H+CHANGE SUCCESSIVE OSCILLATION LIMIT., 2X, I8 )       M28.1401
      GO TO 15                                                          M28.1402
C     $$$$$    SPECIAL-REQUEST WORD NO. 14.   'TACS EMTP SOURCES'  $$$$ M28.1403
 8014 IF ( NOUTPR  .EQ.  0 )                                            M28.1404
     1 WRITE (KUNIT6, 2682)                                             M35. 539
 2682 FORMAT (  47H+TACS NAMES CONTROLLING TYPE 1-10 EMTP SOURCES.     )M28.1406
      IF ( KOLBEG  .GT.  0 )   GO TO 2683                               M28.1407
      DECODE (80, 2685, ABUFF(1) ) (VSTACS(J), J=1, 10)                 M28.1408
 2685 FORMAT (20X, 10A6 )                                               M28.1409
      GO TO 2686                                                        M28.1410
 2683 NRIGHT = -2                                                       M28.1411
      CALL FREONE ( D1 )                                                M29.1036
      DO 2688  J=1, 10                                                  M28.1413
 2688 VSTACS(J) = TEXTA6(J)                                             M28.1414
 2686 NSTACS = 10                                                       M28.1415
 2687 IF ( VSTACS(NSTACS)  .NE.  BLANK )   GO TO 2690                   M28.1416
      NSTACS = NSTACS - 1                                               M28.1417
      IF ( NSTACS  .GT.  0 )   GO TO 2687                               M28.1418
 2690 GO TO 15                                                          M28.1419
C     $$$$$    SPECIAL-REQUEST WORD NO. 15.   'START AGAIN'       $$$$$ M28.1420
C              THIS REQUEST IS HANDLED OUTSIDE OF SUBROUTINE.           M28.1421
C     $$$$$    SPECIAL-REQUEST WORD NO. 16.   'SEMLYEN SETUP'     $$$$$ M28.1422
 8016 IF ( NOUTPR  .EQ.  0 )                                            M28.1423
     1 WRITE (KUNIT6, 2705)                                             M35. 540
 2705 FORMAT(43H+REQUEST FOR SEMLYEN STEP-RESPONSE ROUTINE.)            M28.1425
      NCHAIN = 45                                                       M28.1426
      GO TO 5617                                                        M28.1427
C     $$$$  SPECIAL REQUEST-WORD NO. 17.   'LINEAR BIAS USAGE' $$$$$$$$$M42.1186
 8017 IF ( NOUTPR .EQ. 0 )                                              M36. 294
     1 WRITE (LUNIT6, 8719)                                             M36. 295
 8719 FORMAT ( 40H+RAMPED LINEAR VARIATION OF RANDOM BIAS.   )          M42.1187
      LINSYS = 1                                                        M42.1188
      GO TO 15                                                          M36. 299
C     $$$$$    SPECIAL-REQUEST WORD NO. 18.   'CABLE CONSTANTS'   $$$$$ M28.1434
 8018 CONTINUE                                                          M35. 541
      DECODE (80, 2732, ABUFF)  D13, KTRLSW(3)                          M35. 542
 2732 FORMAT ( 48X, E8.0, I8 )                                          M35. 543
      IF ( D13  .GT.  0.0 )                                             M35. 544
     1 ZNVREF = D13                                                             
      IF ( NOUTPR  .EQ.  0 )                                            M35. 546
     1 WRITE (KUNIT6, 2726)  KTRLSW(3)                                  M35. 547
 2726 FORMAT ( 39H+TRANSFER TO "CABLE CONSTANTS".  TYPE =,              M35. 548
     1   I6  )                                                          M35. 549
      NCHAIN = 47                                                       M28.1438
      GO TO 5617                                                        M28.1439
C     $$$$$    SPECIAL-REQUEST WORD NO. 19.   'AUTO NAME'         $$$$$ M42.1189
 8019 NMAUTO = NMAUTO + 1                                                       
      IF ( NMAUTO .GE. 2 )  NMAUTO = 0                                          
      IF ( NOUTPR  .EQ.  0 )                                            M42.1192
     1 WRITE (KUNIT6, 2743)  NMAUTO                                     M42.1193
 2743 FORMAT ( 40H+TOGGLE BRANCH NAMING OPTION.   NMAUTO =, I8 )        M42.1194
      GO TO 15                                                          M42.1195
C     $$$$$    SPECIAL-REQUEST WORD NO. 20.   'RENUMBER BYPASS'   $$$$$ M28.1487
 8020 IF ( NOUTPR  .EQ.  0 )                                            M28.1488
     1 WRITE (KUNIT6, 2758)                                             M35. 550
 2758 FORMAT (  41H+BYPASS OF TRANSIENT NETWORK RENUMBERING.   )        M28.1490
C     NEGATIVE "KPARTB" (1000 OR EXPONENT OF "HIGH RESISTANCE")         M32. 750
C     IS FLAG CARRIED INTO "OVER6" TO BYPASS OVERLAY 7:                 M32. 751
      KPARTB = -IABS ( KPARTB )                                         M32. 752
      GO TO 15                                                          M28.1492
C     $$$$$    SPECIAL-REQUEST WORD NO. 21.   'FREQUENCY SCAN'    $$$$$ M28.1493
 8021 IF ( KOLBEG  .GT.  0 )   GO TO 2773                               M28.1494
      CALL EXPCHK ( LL25, LL48, LL8 )                                   M28.1495
      CALL INTCHK ( LL49, LL56, LL8 )                                   M28.1496
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28.1497
      DECODE (80, 2779, ABUFF(1) )   FMINFS, DELFFS, FMAXFS, N8         M28.1498
 2779 FORMAT ( 24X, 3E8.0, I8 )                                         M28.1499
      GO TO 2776                                                        M28.1500
 2773 NFRFLD = 4                                                        M28.1501
      CALL FREFLD ( VOLTBC(1) )                                         M31. 276
      FMINFS = VOLTBC(1)                                                M31. 277
      DELFFS = VOLTBC(2)                                                M31. 278
      FMAXFS = VOLTBC(3)                                                M31. 279
      N8     = VOLTBC(4)                                                M31. 280
 2776 IF ( NOUTPR .NE. 0 )  GO TO 2781
      IF ( KEXACT .EQ. 88333 )  GO TO 2778
      WRITE (KUNIT6, 2780)  FMINFS, DELFFS, FMAXFS, N8         
 2780 FORMAT ( 8H+F-SCAN.,  3E12.3,  I5  )                              M28.1509
      GO TO 2781
 2778 WRITE(KUNIT6, 3779) FMINFS, FMAXFS, N8
 3779 FORMAT('+LINE MODEL FREQ SCAN.', 2E12.3, I3)
      FMINSV = FMINFS
 2781 IF ( FMINFS  .LE.  0.0 )   GO TO 2785                          
      IF ( FMAXFS  .LE.  FMINFS )   GO TO 2785                          M28.1511
      IF ( N8  .NE.  0 )   GO TO 2782                                   M28.1512
      IF ( DELFFS  .LE.  0.0 )   GO TO 2785                             M28.1513
      GO TO 15                                                          M28.1514
 2782 IF ( N8  .LT.  0 )   GO TO 2785                                   M28.1515
      D7 = 10.                                                          M28.1516
      D8 = ALOGZ(D7) / N8                                               M28.1517
      DELFFS = -EXPZ(D8)                                                M28.1518
      GO TO 15                                                          M28.1519
 2785 KILL = 193                                                        M28.1520
      LSTAT(14) = N8                                                    M28.1521
      LSTAT(19) = 2785                                                  M28.1522
      GO TO 9200                                                        M28.1523
C     $$$$$    SPECIAL-REQUEST WORD NO. 22.   'FREE FORMAT'       $$$$$ M28.1524
 8022 CONTINUE                                                          M28.1525
      DECODE (80, 2796, ABUFF(1) )   BUS4, BUS5                         M28.1526
 2796 FORMAT ( 16X, A1, 7X, A1 )                                        M28.1527
      IF ( NOUTPR  .EQ.  0 )                                            M28.1528
     1 WRITE (KUNIT6, 2801)  BUS4, BUS5                                 M35. 552
 2801 FORMAT (  27H+FREE-FIELD CHARACTERS.   ',  A1,                    M28.1530
     1 11H'   AND   ',  A1,  3H' .    )                                 M28.1531
      IF ( BUS4  .NE.  BLANK )   CSEPAR = BUS4                          M28.1532
      IF ( BUS5  .NE.  BLANK )   CHCONT = BUS5                          M28.1533
      GO TO 15                                                          M28.1534
C     $$$$$    SPECIAL-REQUEST WORD NO. 23.   'DIAGNOSTIC'        $$$$$ M28.1535
 8023 IF ( KOLBEG  .GT.  0 )   GO TO 2814                               M28.1536
      DECODE (80, 2811, ABUFF(1) )  (IPRSOV(I), I=1, 30)                M28.1537
 2811 FORMAT ( 20X, 30I2 )                                              M28.1538
      GO TO 2816                                                        M28.1539
 2814 NFRFLD = 30                                                       M28.1540
      CALL FREFLD ( VOLTBC(1) )                                         M31. 281
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28.1542
      DO 2815  I=1, 30                                                  M28.1543
 2815 IPRSOV(I) = VOLTBC(I)                                             M31. 282
 2816 IPRSUP = IPRSOV(1)                                                M28.1545
      IF ( NOUTPR  .EQ.  0 )                                            M28.1546
     1 WRITE (KUNIT6, 2813)  ( IPRSOV(I), I=1, 5 )                      M35. 553
 2813 FORMAT ( 27H+DIAGNOSTIC PRINTOUT CODES.,  5I4  )                  M28.1548
      GO TO 15                                                          M28.1549
C     $$$$$    SPECIAL-REQUEST WORD NO. 24.   'POWER FREQUENCY'   $$$$$ M28.1550
 8024 IF ( KOLBEG  .GT.  0 )   GO TO 2820                               M28.1551
      DECODE (80, 2605, ABUFF(1) )   STATFR                             M28.1552
      GO TO 2822                                                        M28.1553
 2820 NFRFLD = 1                                                        M28.1554
      CALL FREONE ( STATFR )                                            M29.1041
 2822 IF ( NOUTPR  .EQ.  0 )                                            M28.1556
     1 WRITE (KUNIT6, 2824)  STATFR                                     M35. 554
 2824 FORMAT ( 28H+REDEFINED POWER FREQUENCY =,  E12.3,  5H  HZ.   )    M28.1558
      GO TO 15                                                          M28.1559
C     $$$$$    SPECIAL-REQUEST WORD NO. 25.   'FILE REQUEST'      $$$$$ M28.1560
 8025 IF ( NOUTPR  .EQ.  0 )                                            M28.1561
     1 WRITE (KUNIT6, 4654)                                             M35. 555
 4654 FORMAT (  27H+CALL SUBROUTINE "MIDOV1" .  )                       M28.1563
      CALL MIDOV1                                                       M28.1564
      GO TO 15                                                          M28.1565
C     $$$$$    SPECIAL-REQUEST WORD NO. 26.   'USER IDENTIFICATION' $$$ M28.1566
 8026 IF ( KOLBEG  .GT.  0 )   GO TO 4661                               M28.1567
      DECODE (80, 4658, ABUFF(1) )   USERID                             M28.1568
 4658 FORMAT ( 24X,  8A6  )                                             M28.1569
      GO TO 4664                                                        M28.1570
 4661 NRIGHT = -2                                                       M28.1571
      CALL FREONE ( D1 )                                                M29.1042
      NRIGHT = 0                                                        M28.1573
      USERID = TEXTA6(1)                                                M28.1574
 4664 IF ( NOUTPR  .EQ.  0 )                                            M28.1575
     1 WRITE (KUNIT6, 4667)  USERID                                     M35. 556
 4667 FORMAT ( 21H+USER IDENTIFICATION.,  1X,  4A6  )                   M28.1577
      GO TO 15                                                          M28.1578
C     $$$$$    SPECIAL-REQUEST WORD NO. 27.   'CONVERT ZNO'  $$$$$$$$$$ M39. 359
 8027 IOFBND = 99876                                                            
      NCHAIN = 42                                                       M39. 361
      IF ( NOUTPR  .EQ.  0 )                                            M39. 362
     1 WRITE (KUNIT6, 4685)                                             M39. 363
 4685 FORMAT (  48H+REQUEST TO CONVERT OLD ZNO DATA TO NEW FORMATS. )   M39. 364
      GO TO 5617                                                        M39. 365
C     $$$$$    SPECIAL-REQUEST WORD NO. 28.   'ABORT DATA CASE'   $$$$$ M28.1590
 8028 IF ( NOUTPR  .EQ.  0 )                                            M28.1591
     1 WRITE (KUNIT6, 4695)                                             M35. 558
 4695 FORMAT (  33H+REQUEST TO ABORT THIS DATA CASE.   )                M28.1593
      L = 0                                                             M28.1594
C     READ INPUT CARD USING CIMAGE                                      M28.1595
 4699 CALL CIMAGE                                                       M28.1596
      N9 = KOLBEG                                                       M28.1597
      KOLBEG = 1                                                        M28.1598
      NRIGHT = -2                                                       M28.1599
      CALL FREONE ( D1 )                                                M29.1044
      N1 = JPNTR(5)                                                     M28.1601
      N2 = JPNTR(6) - 1                                                 M28.1602
      K = 0                                                             M28.1603
      DO 4706  I=N1, N2                                                 M28.1604
      K = K + 1                                                         M28.1605
      IF ( TEXTA6(K)  .NE.  TEXTAX(I) )   GO TO 4711                    M28.1606
 4706 CONTINUE                                                          M28.1607
      GO TO 15                                                          M28.1608
 4711 L = L + 1                                                         M28.1609
      IF ( NOUTPR  .EQ.  0 )                                            M28.1610
     1 WRITE (KUNIT6, 4713)  L                                          M35. 559
 4713 FORMAT (  25H+   DISCARDED CARD NUMBER,  I5,                      M28.1612
     1  19H   OF SKIPPED CASE.     )                                    M28.1613
      GO TO 4699                                                        M28.1614
C     $$$$$    SPECIAL-REQUEST WORD NO. 29.   'KILL CODES'        $$$$$ M28.1615
 8029 IPNTV(1) = -8888                                                  M28.1616
      IF ( KOLBEG  .GT.  0 )   GO TO 4724                               M28.1617
      DECODE (80, 4721, ABUFF(1) )  KILL, IPNTV(2)                      M28.1618
 4721 FORMAT ( 32X,  2I8  )                                             M28.1619
      GO TO 4726                                                        M28.1620
 4724 NFRFLD = 2                                                        M28.1621
      CALL FREFLD ( VOLTBC(1) )                                         M31. 283
      KILL = VOLTBC(1)                                                  M31. 284
      IPNTV(2) = VOLTBC(2)                                              M31. 285
 4726 WRITE (KUNIT6, 4728)  KILL, IPNTV(2)                              M35. 560
 4728 FORMAT ( 27H+LISTING OF ERROR MESSAGES.,  2I8  )                  M28.1626
      IPNTV(3) = KILL                                                   M28.1627
      WRITE (LUNIT6, 4733)  KILL                                        M28.1628
 4733 FORMAT ( /,  28H MESSAGE OF KILL-CODE NUMBER,  I4,  1H.  )        M28.1629
      BUS1 = TRASH                                                      M29.1045
      BUS2 = TRASH                                                      M29.1046
      BUS3 = TRASH                                                      M29.1047
      BUS4 = TRASH                                                      M29.1048
      BUS5 = TRASH                                                      M29.1049
      BUS6 = TRASH                                                      M29.1050
      DO 4734  J=11, 17                                                 M29.1051
      LSTAT(J) = 0                                                      M29.1052
 4734 FLSTAT(J) = 0.0                                                   M29.1053
      NCHAIN = 51                                                       M28.1630
      GO TO 9200                                                        M28.1631
C     $$$$$    SPECIAL-REQUEST WORD NO. 30.   'HIGH RESISTANCE'   $$$$$ M28.1632
 8030 N7 = KPARTB                                                       M32. 753
      IF ( KOLBEG  .GT.  0 )   GO TO 4735                               M32. 754
      DECODE (80, 4721, ABUFF(1) )   KPARTB                             M28.1634
      CALL INTCHK ( LL33, LL40, LL8 )                                   M28.1635
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M28.1636
      GO TO 4737                                                        M28.1637
 4735 NFRFLD = 1                                                        M28.1638
      CALL FREFLD ( VOLTBC(1) )                                         M31. 286
      KPARTB = VOLTBC(1)                                                M31. 287
 4737 D1 = 10.0 ** KPARTB                                               M28.1641
      IF ( NOUTPR  .EQ.  0 )                                            M28.1642
     1 WRITE (KUNIT6, 4739)  D1                                         M35. 561
 4739 FORMAT ( 35H+EXPONENT OF HIGH RESISTANCE.   R =,  E12.2  )        M28.1644
C     POSSIBLE NEGATIVE SIGN ON PREVIOUS "KPARTB" MUST BE               M32. 755
C     RETAINED AS FLAG FOR "RENUMBER BYPASS" USAGE:                     M32. 756
      IF ( N7  .LT.  0 )   KPARTB = -KPARTB                             M32. 757
      GO TO 15                                                          M28.1645
C     $$$$$    SPECIAL-REQUEST WORD NO. 31.   'AVERAGE OUTPUT'    $$$$$ M28.1646
 8031 IAVERG = 1                                                        M28.1647
      NSMTH = INTINF                                                    M28.1648
      IF ( NOUTPR  .EQ.  0 )                                            M28.1649
     1 WRITE (KUNIT6, 7010)                                             M35. 562
 7010 FORMAT ( 36H+REQUEST TO AVERAGE (SMOOTH) OUTPUT.   )              M28.1651
      GO TO 15                                                          M28.1652
C     $$$$$  SPECIAL REQUEST-WORD NO. 32.  'ABSOLUTE TACS DIMENSIONS'  $M28.1653
C     $$$$$  SPECIAL REQUEST-WORD NO. 33.  'RELATIVE TACS DIMENSIONS'  $M28.1654
C     THESE TWO REQUESTS HANDLED OUTSIDE OF SUBROUTINE.                 M28.1655
C     $$ SPECIAL REQUEST-WORD NO. 34.  'TABULATE ENERGIZATION RESULTS'  M28.1656
 8034 WRITE (KUNIT6, 7183)                                              M35. 563
 7183 FORMAT ( 40H+REQUEST FOR  'STATISTICS'  TERMINATION.  )           M28.1658
      NENERG = INTINF                                                   M28.1659
      GO TO 15                                                          M28.1660
C     $$ SPECIAL REQUEST-WORD NO. 35.  'STATISTICS OUTPUT SALVAGE'      M28.1661
 8035 IF ( KOLBEG  .GT.  0 )   GO TO 7193                               M28.1662
      DECODE (80, 7191, ABUFF(1) )   JFLSOS                             M28.1663
 7191 FORMAT ( 29X, I3 )                                                M28.1664
      GO TO 7195                                                        M28.1665
 7193 NFRFLD = 1                                                        M28.1666
      CALL FREONE ( D1 )                                                M29.1054
      JFLSOS = D1                                                       M28.1668
 7195 IF ( JFLSOS  .GT.  0 )   GO TO 7199                               M28.1669
C     FIND RANDOM INTEGER  'JFLSOS'  BETWEEN ZERO AND 999.              M28.1670
      CALL RUNTYM ( D1, D2 )                                            M28.1671
      SEED = SEEDY( TCLOCK(1) )  +  1000. * ( D1 + D2 )                 M28.1672
      N13 = ALOG1Z(SEED)  +  EPSILN                                     M28.1673
      N13 = N13 - 2                                                     M28.1674
      SEED = SEED / 10.**N13                                            M28.1675
      JFLSOS = SEED                                                     M28.1676
 7199 IF ( NOUTPR  .EQ.  0 )                                            M28.1677
     1 WRITE (KUNIT6, 7200)  JFLSOS                                     M35. 564
 7200 FORMAT ( 38H+DISK STORAGE OF ENERGIZATION RESULTS., I8 )          M28.1679
      GO TO 15                                                          M28.1680
C     $$$$  SPECIAL REQUEST-WORD NO. 36.   'OMIT BASE CASE'    $$$$$$$$$M28.1681
 8036 IF ( NOUTPR  .EQ.  0 )                                            M28.1682
     1 WRITE (KUNIT6, 7203)                                             M35. 565
 7203 FORMAT (  46H+OMIT BASE CASE IF STATISTICS/SYSTEMATIC CASE.   )   M28.1684
      KBASE = INTINF                                                    M28.1685
      GO TO 15                                                          M28.1686
C     $$$$  SPECIAL REQUEST-WORD NO. 37.   'CHANGE SWITCH'     $$$$$$$$$M42.1196
 8037 IF ( NOUTPR .EQ. 0 )                                              M42.1197
     1 WRITE (KUNIT6, 7206)                                             M42.1198
 7206 FORMAT (  42H+CONVERT SWITCHED-L,R TO PSEUDO-NONLINEAR. )         M42.1199
      IOFBND = 33666                                                            
      GO TO 15                                                          M42.1201
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 38.   'MISCELLANEOUS DATA       M28.1691
C                                              CARDS'          $$$$$$$$$M28.1692
 8038 IFDEP = -5555                                                     M28.1693
      WRITE (KUNIT6, 7212)                                              M35. 566
 7212 FORMAT (  40H+REQUEST RECORD BEFORE MISC. DATA CARDS.  )          M28.1695
      GO TO 15                                                          M28.1696
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 39.   'REDEFINE TOLERANCE       M28.1697
C                                              EPSILN'         $$$$$$$$$M28.1698
 8039 IF ( KOLBEG  .GT.  0 )   GO TO 7217                               M28.1699
      DECODE (80, 2605, ABUFF(1) )   EPSILN                             M28.1700
      GO TO 7223                                                        M28.1701
 7217 NFRFLD = 1                                                        M28.1702
      CALL FREONE ( EPSILN )                                            M29.1055
 7223 WRITE (KUNIT6, 7226)  EPSILN                                      M35. 567
 7226 FORMAT ( 32H+MISC. DATA CONSTANT  'EPSILN' .,  E12.3  )           M28.1705
      GO TO 15                                                          M28.1706
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 40.   'CHANGE PRINTOUT          M28.1707
C                                              FREQUENCY       $$$$$$$$$M28.1708
 8040 IF ( NOUTPR  .EQ.  0 )                                            M28.1709
     1 WRITE (KUNIT6, 7234)                                             M35. 568
 7234 FORMAT (  44H+REQUEST RECORD BEFORE PRINTOUT FREQUENCIES.   )     M28.1711
      KTREF = -7777                                                     M28.1712
      GO TO 15                                                          M28.1713
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 41.   'BEGIN PEAK               M28.1714
C                                              VALUE SEARCH'   $$$$$$$$$M28.1715
 8041 IF ( KOLBEG  .GT.  0 )   GO TO 7247                               M28.1716
      DECODE (80, 2605, ABUFF(1) )   BEGMAX(2)                          M31. 288
      GO TO 7249                                                        M28.1718
 7247 NFRFLD = 1                                                        M28.1719
      CALL FREFLD ( BEGMAX(2) )                                         M31. 289
 7249 IF ( NOUTPR  .EQ.  0 )                                            M28.1721
     1 WRITE (KUNIT6, 7252)  BEGMAX(2)                                  M35. 569
 7252 FORMAT ( 24H+EXTREMA CALC. BEGINS AT,  E13.4,  10H  SECONDS.  )   M28.1723
      IF ( BEGMAX(2)  .NE.  -1. )   GO TO 15                            M31. 291
C     READ INPUT DATA CARD USING CIMAGE                                 M31. 292
      CALL CIMAGE                                                       M31. 293
      DECODE (80, 7253, ABUFF(1) )   (BEGMAX(IP), IP=2, 6)              M31. 294
 7253 FORMAT ( 10E8.0 )                                                 M31. 295
      IF ( NOUTPR  .EQ.  0 )                                            M31. 296
     1 WRITE (KUNIT6, 7254)  ( BEGMAX(IP), IP=2, 5 )                    M35. 570
 7254 FORMAT (  9H+(T1,T2):,  4E10.2  )                                 M31. 298
      GO TO 15                                                          M28.1724
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 42.   'TIME OF                  M28.1725
C                                              DICE ROLL'    $$$$$$$$$$$M28.1726
 8042 IF ( KOLBEG  .GT.  0 )   GO TO 7255                               M28.1727
      DECODE (80, 2605, ABUFF(1) )   TENERG                             M28.1728
      GO TO 7261                                                        M28.1729
 7255 NFRFLD = 1                                                        M28.1730
      CALL FREONE ( TENERG )                                            M29.1057
 7261 IF ( NOUTPR  .EQ.  0 )                                            M28.1732
     1 WRITE (KUNIT6, 7262)  TENERG                                     M35. 571
 7262 FORMAT ( 31H+STATISTICS TABLE-SAVING TIME =,  E12.3,              M28.1734
     1          6H  SEC.  )                                             M28.1735
      GO TO 15                                                          M28.1736
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 43.   'ZINC OXIDE'   $$$$$$$$$$$M37.1202
 8043 IF ( KOLBEG  .GT.  0 )  GO TO 7266                                M37.1203
      DECODE (80, 7264, ABUFF)  N13,  ( FLSTAT(M), M=15, 19 )           M37.1204
 7264 FORMAT ( 16X, I8, 5E8.0 )                                         M37.1205
      GO TO 7268                                                        M37.1206
 7266 NFRFLD = 6                                                        M37.1207
      CALL FREFLD ( FLSTAT(14) )                                        M37.1208
      N13 = FLSTAT(14)                                                          
 7268 IF ( N13 .GT. 0 )  MAXZNO = N13                                           
      IF ( FLSTAT(15) .GT. 0.0 )  EPSZNO = FLSTAT(15)                   M37.1211
      IF ( FLSTAT(16) .GT. 0.0 )  EPWARN = FLSTAT(16)                   M37.1212
      IF ( FLSTAT(17) .GT. 0.0 )  EPSTOP = FLSTAT(17)                   M37.1213
      IF ( FLSTAT(18) .GT. 0.0 )  ZNOLIM(1) = FLSTAT(18)                M37.1214
      IF ( FLSTAT(19) .GT. 0.0 )  ZNOLIM(2) = FLSTAT(19)                M37.1215
      IF ( NOUTPR .EQ. 0 )                                              M37.1216
     1 WRITE (KUNIT6, 7270)  MAXZNO, EPSZNO, EPWARN, EPSTOP             M37.1217
 7270 FORMAT ( 11H+ZNO CONST.,  I4,  3E11.3 )                           M37.1218
      GO TO 15                                                          M37.1219
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 44.   'PEAK VOLTAGE'  $$$$$$$$$$M28.1743
C     $$$$$$$                                  MONITOR'      $$$$$$$$$$$M28.1744
 8044 PEAKND(1) = FLZERO                                                M28.1745
      IF ( NOUTPR  .EQ.  0 )                                            M28.1746
     1 WRITE (KUNIT6, 7273)                                             M35. 572
 7273 FORMAT ( 35H+OVERALL PROBLEM PEAK NODE VOLTAGE.   )               M28.1748
      GO TO 15                                                          M28.1749
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 45.   'ABSOLUTE U.M.  $$$$$$$$$$M28.1750
C                                              DIMENSIONS'    $$$$$$$$$$M28.1751
 8045 IF ( KOLBEG  .GT.  0 )   GO TO 7279                               M28.1752
      DECODE (80, 7276, ABUFF(1))  NCLFIX, NUMFIX,                      M28.1753
     1                             IOTFIX, IBSFIX                       M28.1754
 7276 FORMAT ( 32X, 6I8 )                                               M28.1755
      GO TO 7282                                                        M28.1756
 7279 NFRFLD = 4                                                        M28.1757
      CALL FREFLD ( VOLTBC(1) )                                         M31. 299
      NCLFIX = VOLTBC(1)                                                M31. 300
      NUMFIX = VOLTBC(2)                                                M31. 301
      IOTFIX = VOLTBC(3)                                                M31. 302
      IBSFIX = VOLTBC(4)                                                M31. 303
 7282 IF ( NCLFIX  .GT.  0 )   GO TO 7284                               M28.1763
      NCLFIX = 20                                                       M28.1764
      NUMFIX = 3                                                        M28.1765
      IOTFIX = 50                                                       M28.1766
      IBSFIX = 60                                                       M28.1767
 7284 IF ( NOUTPR  .EQ.  0 )                                            M28.1768
     1 WRITE (KUNIT6, 7287) NCLFIX, NUMFIX, IOTFIX, IBSFIX              M35. 573
 7287 FORMAT ( 18H+U.M. TABLE SIZES.,  4I6  )                           M28.1770
      GO TO 15                                                          M28.1771
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 46.   'RELATIVE U.M.  $$$$$$$$$$M28.1772
C                                              DIMENSIONS'    $$$$$$$$$$M28.1773
 8046 IF ( KOLBEG  .GT.  0 )   GO TO 7298                               M28.1774
      DECODE (80, 7276, ABUFF(1))  ( VOLTBC(K), K=1, 4 )                M31. 304
      GO TO 7303                                                        M28.1776
 7298 NFRFLD = 4                                                        M28.1777
      CALL FREFLD ( VOLTBC(1) )                                         M31. 305
 7303 D1 = 0.0                                                          M28.1779
      DO 7306  J=1, 4                                                   M28.1780
 7306 D1 = D1 + VOLTBC(J)                                               M31. 306
      IF ( D1  .GT. 0.0 )   GO TO 7310                                  M28.1782
      VOLTBC(1) =  42                                                   M31. 307
      VOLTBC(2) =  23                                                   M31. 308
      VOLTBC(3) =  10                                                   M31. 309
      VOLTBC(4) =  10                                                   M31. 310
      GO TO 7303                                                        M28.1787
 7310 D1 = LSPCUM * NBYTE(3) / D1                                       M28.1788
      NCLFIX  =  VOLTBC(1)*D1 / ( 4*NBYTE(3) +  4*NBYTE(4) )            M31. 311
      NUMFIX  =  VOLTBC(2)*D1 / (17*NBYTE(3) + 12*NBYTE(4) )            M31. 312
      IOTFIX  =  VOLTBC(3)*D1 / (               2*NBYTE(4) )            M31. 313
      IBSFIX  =  VOLTBC(4)*D1 / (     1*NBYTE(1)           )            M31. 314
      IF ( NOUTPR  .EQ.  0 )                                            M28.1793
     1 WRITE (KUNIT6, 7309)  NCLFIX, NUMFIX, IOTFIX, IBSFIX             M35. 574
 7309 FORMAT ( 20H+DERIVED U.M. SIZES.,  4I6  )                         M28.1795
      GO TO 15                                                          M28.1796
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 47.   'TIME STEP LOOP'  $$$$$$$$M28.1797
 8047 IF ( NOUTPR  .EQ.  0 )                                            M28.1798
     1 WRITE (KUNIT6, 7314)                                             M35. 575
 7314 FORMAT ( 36H+TRANSFER CONTROL TO TIME-STEP LOOP.    )             M28.1800
      NCHAIN = 16                                                       M28.1801
      T = FLSTAT(14)                                                    M28.1802
      CALL MOVER0 ( FLSTAT(3), 4 )                                      M28.1803
      CALL RUNTYM ( D1, D2 )                                            M28.1804
      FLSTAT(1) = D1 + FLSTAT(1)                                        M28.1805
      FLSTAT(2) = D2 + FLSTAT(2)                                        M28.1806
      FLSTAT(7) = -D1                                                   M28.1807
      FLSTAT(8) = -D2                                                   M28.1808
      GO TO 15                                                          M28.1809
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 48.   'ALTERNATE DIAGNOSTIC     M28.1810
C                                                    PRINTOUT'  $$$$$$$$M28.1811
 8048 IF ( KOLBEG  .GT.  0 )   GO TO 7322                               M28.1812
      DECODE (80, 2642, ABUFF(1))   ( IPRSOV(J+30), J=1, 4 )            M28.1813
      GO TO 7329                                                        M28.1814
 7322 NFRFLD = 4                                                        M28.1815
      CALL FREFLD ( VOLTBC(1) )                                         M31. 315
      DO 7326  J=1, 4                                                   M28.1817
 7326 IPRSOV(J+30) = VOLTBC(J)                                          M31. 316
 7329 IF ( NOUTPR  .EQ.  0 )                                            M28.1819
     1 WRITE (KUNIT6, 7331)  ( IPRSOV(J+30), J=1, 4 )                   M35. 576
 7331 FORMAT ( 22H+DELTAT-LOOP PRINTOUT.,  4I6  )                       M28.1821
      GO TO 15                                                          M28.1822
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 49.   'TACS WARN LIMIT' $$$$$$$$M31. 317
 8049 IF ( KOLBEG  .GT.  0 )   GO TO 7334                               M31. 318
      DECODE (80, 2582, ABUFF(1))   LSTAT(51), PU                       M42.1202
 2582 FORMAT( 16X, I8, E8.0 )                                           M42.1203
      GO TO 7335                                                        M31. 320
 7334 NFRFLD = 2                                                        M42.1204
      CALL FREFLD ( VOLTBC(1) )                                         M31. 322
      LSTAT(51) = VOLTBC(1)                                                     
      PU = VOLTBC(2)                                                            
 7335 IF ( NOUTPR  .EQ.  0 )                                            M31. 324
     1 WRITE (KUNIT6, 7336)  LSTAT(51), PU                              M42.1207
 7336 FORMAT ( 32H+WARNING CONTROLS.  LIM, T-BEG =, I6, E10.2 )         M42.1208
      GO TO 15                                                          M31. 328
C     $$$$$    SPECIAL-REQUEST WORD NO. 50.   'MARTI SETUP'       $$$$$ M29.1081
 8050 CONTINUE                                                                  
C     $$$$$    SPECIAL-REQUEST WORD NO. 51.   'JMARTI SETUP' $$$$$$$$$$ M32. 758
 8051 IF ( NOUTPR  .EQ.  0 )                                            M32. 759
     1 WRITE (KUNIT6, 7352)                                             M35. 578
 7352 FORMAT ( 42H+REQUEST FOR NEW, FORTIFIED "MARTI SETUP".  )         M32. 761
      NCHAIN = 39                                                       M32. 762
      GO TO 5617                                                        M32. 763
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 52.   'CUSTOM PLOT FILE' $$$$$$ M29.1101
 8052 N4 = M4PLOT                                                       M32. 764
      IF ( N4  .EQ.  0 )  M4PLOT = 2                                    M32. 765
      IF ( N4  .EQ.  2 )  M4PLOT = 0                                    M32. 766
      IF ( NOUTPR  .EQ.  0 )                                            M32. 767
     1 WRITE (KUNIT6, 7359)  M4PLOT                                     M35. 579
 7359 FORMAT ( 35H+NON-STD. CHOICE OF DISK PLOT FILE.,                  M32. 769
     1         11H   M4PLOT =,  I2  )                                   M32. 770
      GO TO 15                                                          M29.1106
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 53.   'OUTPUT WIDTH 132' $$$$$$ M32. 771
 8053 IF ( NOUTPR  .EQ.  0 )                                            M32. 772
     1 WRITE (KUNIT6, 7364)                                             M35. 580
 7364 FORMAT ( 36H+USE FULL-WIDTH (132-COL.) PRINTOUT.   )              M32. 774
      KOL132 = 132                                                      M32. 775
      GO TO 15                                                          M32. 776
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 54.   'OUTPUT WIDTH 80'  $$$$$$ M32. 777
 8054 IF ( NOUTPR  .EQ.  0 )                                            M32. 778
     1 WRITE (KUNIT6, 7368)                                             M35. 581
 7368 FORMAT ( 31H+USE NARROW (80-COL.) PRINTOUT.    )                  M32. 780
      KOL132 = 80                                                       M32. 781
      GO TO 15                                                          M32. 782
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 55.   'MODIFY SWITCH LOGIC  $$$ M32. 783
 8055 N7 = KTRLSW(6) + 1                                                M32. 784
      IF ( N7  .GE.  2 )  N7 = 0                                        M32. 785
      KTRLSW(6) = N7                                                    M32. 786
      IF ( NOUTPR  .EQ.  0 )                                            M32. 787
     1 WRITE (KUNIT6, 7374)  N7                                         M35. 582
 7374 FORMAT ( 40H+REQUEST FOR ALTERED LOGIC.  KTRLSW(6) =, I2 )        M32. 789
      GO TO 15                                                          M32. 790
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 56.   'FAULT DATA USAGE' $$$$$$ M32. 791
 8056 IF ( KOLBEG  .GT.  0 )   GO TO 7379                               M32. 792
      DECODE (80, 2642, ABUFF(1))   IOFBND                              M32. 793
      GO TO 7382                                                        M32. 794
 7379 NFRFLD = 1                                                        M32. 795
      CALL FREFLD ( VOLTBC(1) )                                         M32. 796
      IOFBND = VOLTBC(1)                                                M32. 797
 7382 IF ( NOUTPR  .EQ.  0 )                                            M32. 798
     1 WRITE (KUNIT6, 7385)                                             M35. 583
 7385 FORMAT (  35H+REQUEST FOR GENERATOR EQUIVALENTS.  )               M32. 800
      ISTEP = -6633                                                             
      NCHAIN = 29                                                       M32. 802
      GO TO 5617                                                        M32. 803
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 57.   'FIX SOURCE'  $$$$$$      M36. 307
 8057 ISTEP = -4567                                                             
      IF ( NOUTPR  .EQ.  0 )                                            M36. 309
     1 WRITE (KUNIT6, 7388)                                             M36. 310
 7388 FORMAT ( 38H+DECLARATION OF DESIRED LOAD FLOW USE.    )           M36. 311
      GO TO 15                                                          M35. 584
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 58.   'USER SUPPLIED     $$$$$$ M37.1220
C     $$$$$$$                                  SWITCH TIMES'     $$$$$$ M37.1221
 8058 IF ( KOLBEG  .GT.  0 )   GO TO 7396                               M37.1222
      DECODE (80, 2642, ABUFF(1))  N14                                  M37.1223
      GO TO 7402                                                        M37.1224
 7396 NFRFLD = 1                                                        M37.1225
      CALL FREFLD ( VOLTBC(1) )                                         M37.1226
      N14 = VOLTBC(1)                                                   M37.1227
 7402 MONCAR(7) = N14                                                   M38. 305
      IF ( N14 .EQ. 0 )   MONCAR(7) = 24                                M38. 306
      IF ( NOUTPR  .EQ.  0 )                                            M37.1229
     1 WRITE (KUNIT6, 7406)  MONCAR(7)                                  M39. 366
 7406 FORMAT ( 40H+FILE OF RANDOM SWITCHING TIMES.  UNIT =, I6  )       M37.1231
      GO TO 15                                                          M37.1232
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 59.   'AMETANI SETUP'  $$$      M42.1210
 8059 IF ( NOUTPR  .EQ.  0 )                                            M28.1429
     1 WRITE (LUNIT6, 7411)                                             M28.1430
 7411 FORMAT ( 43H+REQUEST FOR AMETANI STEP-RESPONSE ROUTINE.   )       M28.1431
      NCHAIN = 46                                                       M28.1432
      GO TO 5617                                                        M28.1433
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 60.   'HAUER SETUP'  $$$$$      M42.1211
 8060 IF ( NOUTPR  .EQ.  0 )                                            M28.1738
     1 WRITE (LUNIT6,7418)                                              M28.1739
 7418 FORMAT ( 44H+REQUEST FOR HAUER IMPULSE-RESPONSE ROUTINE.   )      M28.1740
      NCHAIN = 48                                                       M28.1741
      NENERG = 49                                                       M29.1058
      GO TO 5617                                                        M28.1742
C     $$$$$$$  SPECIAL REQUEST-WORD NO. 61.   'LINE MODEL FREQ. SCAN  $$
 8061 KEXACT = 88333
      NSOLVE = 0
      KBRNUM = 0
      GO TO 8021
C     $$$$$$$  ADDITIONAL KEY WORDS GO IN SLOTS BELOW:                  M36. 312
 8062 CONTINUE                  
 5617 FLSTAT(7) = -9999.                                                M39. 367
      CK1 = -FLTINF                                                     M28.1829
      CI1 = -FLTINF                                                     M28.1830
   15 CONTINUE                                                          M28.1831
 9200 IF ( IPRSUP  .GE.  5 )                                            M28.1832
     1 WRITE (LUNIT6, 9201)  NCHAIN, LUNIT6, LSTAT(18), KILL            M28.1833
 9201 FORMAT ( /,  17H EXIT  "REQUES" .,                                M28.1834
     1             32H  NCHAIN  LUNIT6 LSTAT18    KILL                  M28.1835
     2       ,/,  17X,  4I8  )                                          M28.1836
      IF ( IPRSUP  .GE.  1 )                                            M32. 804
     1 WRITE ( LUNIT6, 4568 )                                           M32. 805
 4568 FORMAT ( 24H  "EXIT  MODULE REQUES." )                            M32. 806
      RETURN                                                            M28.1837
      END                                                               M28.1838
      SUBROUTINE SYSDEP                                                 M40. 445
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      COMMON  /KOMTHL /  PEKEXP                                                 
      DIMENSION  INTBUS(1)                                              M31. 332
      EQUIVALENCE  (INTBUS(1), BUS1 )                                   M31. 333
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      REAL*8         BUSNM1, BUSNM2, BUSNM3, TEXT1, TEXT2               M24. 320
      CHARACTER*1      LETTRA, LETTRB, LETTRC, COLXXX(18)               M35. 586
C     FIRST 5 CHARACTERS OF FILE NAME "COL" ARE RESERVED                M28.1839
C     FOR EXPLICIT DIRECTORY (E.G., "[PLT]" ), IF DESIRED.              M28.1840
      DATA COLXXX(1)  / ' '  /                                          M35. 587
      DATA COLXXX(2)  / ' '  /                                          M35. 588
      DATA COLXXX(3)  / ' '  /                                          M35. 589
      DATA COLXXX(4)  / ' '  /                                          M35. 590
      DATA COLXXX(5)  / ' '  /                                          M35. 591
      DATA LETTRA   / 'A'  /                                            M35. 592
      DATA LETTRB   / 'B'  /                                            M35. 593
      DATA LETTRC   / 'C'  /                                            M35. 594
      DATA COLXXX(15) / '.' /                                           M35. 595
      DATA COLXXX(16) / 'P'  /                                          M35. 596
      DATA COLXXX(17) / 'L'  /                                          M35. 597
      DATA COLXXX(18) / '4'  /                                          M35. 598
      DATA BUSNM1  / 6H               /                                 M24. 337
      DATA BUSNM2  / 8H........ /                                       M24. 338
      DATA BUSNM3  / 6HTERRA  /                                         M24. 339
      DATA TEXT1   / 1H,  /                                             M24. 340
      DATA TEXT2   / 1H$  /                                             M24. 341
      DO 1108  I=1, 18                                                  M29.1134
 1108 COL(I) = COLXXX(I)                                                M29.1135
      LUNIT2 = 19                                                       M29.1137
      LUNIT4 = 20                                                       M29.1138
      CALL SYSPLT (LUNIT4)   ! DEFINE L4PLOT=LUNIT4 FOR "DEKPLT"        M35. 599
      LUNTSP = 14    ! THE CATALOG COMMAND OF "SPYING" USES THIS        M35. 600
      MFLUSH = 0                                                        M29.1140
      IF ( M4PLOT  .EQ.  1 )   GO TO 1355                               M35. 601
      IF ( LLBUFF  .EQ.  -3333 )   GO TO 1342                           M24. 347
      IF (NENERG .EQ. 49 ) CLOSE (UNIT=LUNIT1, DISPOSE='SAVE')          M35. 602
      IF (NENERG .NE. 49 ) CLOSE (UNIT=LUNIT1, DISPOSE='DELETE')        M35. 603
      CLOSE(UNIT=LUNIT2)                                                M29.1145
      CLOSE ( UNIT=LUNIT3 )                                             M35. 604
      IF (ICAT .EQ. 0) GO TO 120                                        M24. 351
      IF (ICAT .GT. 2) GO TO 120                                        M24. 352
  100 CLOSE (UNIT=LUNIT4, DISPOSE='SAVE')                               M29.1146
      GO TO 140                                                         M24. 354
  120 CLOSE (UNIT=LUNIT4, DISPOSE='DELETE')                             M29.1147
  140 CONTINUE                                                          M24. 356
      CLOSE ( UNIT=LUNIT9 )                                             M35. 605
      CLOSE ( UNIT=LUNT10 )                                             M35. 606
      CLOSE ( UNIT=LUNT11 )                                             M35. 607
      CLOSE ( UNIT=LUNT12 )                                             M35. 608
      CLOSE ( UNIT=LUNT13 )                                             M35. 609
      CLOSE ( UNIT=LUNT14 )                                             M35. 610
      CLOSE ( UNIT=LUNT15 )                                             M35. 611
 1342 OPEN (UNIT=LUNIT1, TYPE='NEW', FORM='FORMATTED')                  M36. 313
      OPEN (UNIT=LUNIT2, TYPE='SCRATCH', FORM='FORMATTED',              M29.1150
     1      DISPOSE='DELETE' )                                          M29.1151
      OPEN ( UNIT=LUNIT3, TYPE='SCRATCH', FORM='UNFORMATTED',           M35. 612
     1      DISPOSE='DELETE' )                                          M35. 613
C               UNIT 8 IS CALCOMP PLOT FILE (UNUSED WITH OUR VAX-VERSATEM24. 367
C               CONNECTION )                                            M24. 368
      OPEN ( UNIT=LUNT10, TYPE='SCRATCH', FORM='UNFORMATTED' )          M35. 614
      OPEN ( UNIT=LUNT11, TYPE='SCRATCH', FORM='UNFORMATTED' )          M35. 615
      OPEN ( UNIT=LUNT12, TYPE='SCRATCH', FORM='UNFORMATTED' )          M35. 616
      OPEN ( UNIT=LUNT13, TYPE='SCRATCH', FORM='UNFORMATTED' )          M35. 617
      OPEN ( UNIT=LUNT14, TYPE='SCRATCH', FORM='UNFORMATTED' )          M35. 618
      OPEN ( UNIT=LUNT15, TYPE='SCRATCH', FORM='UNFORMATTED' )          M35. 619
 1355 IF (LLBUFF .NE. -3333)  GO TO 1359                                M29.1152
      CALL SETTYM                                                       M24. 379
      IPRSOV(38) = 0                                                    M39. 368
      LLBUFF = 1025                                                     M24. 381
      IF ( LTACST  .GE.  3000 )   GO TO 1359                            M37.1233
C     VAX LINK-EDITS ACCORDING TO LONGEST COMMON BLOCK, NOT             M31. 346
C     ACCORDING TO 1ST APPEARANCE (SIZE OF VARDIM OUTPUT):              M31. 347
      LTLABL = LTLABL  +  2 * (3000 - LTACST)                           M37.1234
      LTACST = 3000                                                     M37.1235
 1359 STATFR = 60.                                                      M28.1844
      KBURRO = 1                                                        M29.1153
      PEKEXP = 43                                                               
      SZPLT = 10.                                                       M24. 387
      SZBED = 72.                                                       M24. 388
      EPSUBA = 1.D-16                                                   M24. 389
      EPOMEG = 1.D-15                                                   M24. 390
      EPDGEL = 1.D-16                                                   M24. 391
      IPRSOV(37) = 10                                                   M39. 369
      LNPIN = 6                                                         M24. 394
      NSMTH = 50                                                        M24. 395
      CALL DATE44 ( DATE1(1) )                                          M24. 397
      CALL TIME44 ( TCLOCK(1) )                                         M24. 398
      IF ( NOUTPR  .EQ.  0 )                                            M24. 402
     1 WRITE (LUNIT6, 6301)                                             M24. 403
 6301 FORMAT (  ' ELECTROMAGNETIC TRANSIENTS PROGRAM (EMTP),',          M25. 191
     1          ' DIGITAL (DEC) VAX-11/780 TRANSLATION AS',             M25. 192
     2          ' USED BY BPA IN PORTLAND, OREGON  97208;',             M28.1845
     3          '  USA.'  )                                             M28.1846
      DECODE (4, 3641, DATE1(1) ) COL(25), COL(6), COL(7)               M24. 412
 3641 FORMAT (2A1, 1X, A1)                                              M24. 413
      IF ( COL(25)  .EQ.  '0' ) GO TO 3642                              M35. 620
      IF ( COL(6)   .EQ.  '0' ) COL(6) = LETTRA                         M35. 621
      IF ( COL(6)   .EQ.  '1' ) COL(6) = LETTRB                         M35. 622
      IF ( COL(6)   .EQ.  '2' ) COL(6) = LETTRC                         M35. 623
 3642 CONTINUE                                                          M25. 194
      DECODE (4, 3641, DATE1(2) ) COL(8)                                M25. 195
      DECODE (16, 3652, TCLOCK(1) ) ( COL(J), J=9, 14 )                 M24. 419
 3652 FORMAT (2A1, 1X, A1, 4X, A1, 1X, 2A1 )                            M24. 420
      WRITE (ANSI32, 3668)  ( COL(J), J=1, 18 )                         M36. 314
 3668 FORMAT ( 18A1, 14X )                                              M36. 315
      IF (IPRSUP .GE. 1 )                                               M24. 423
     1 WRITE (LUNIT6, 3672) ANSI32                                      M36. 316
 3672 FORMAT ( /,  ' IN  #SYSDEP# ,   ANSI32 =',  A32 )                 M36. 317
      OPEN (UNIT=LUNIT4, STATUS='NEW', FILE=ANSI32,                     M36. 318
     2       FORM='UNFORMATTED')                                        M29.1156
      BLANK = BUSNM1                                                    M24. 427
      TRASH = BUSNM2                                                    M24. 428
      TERRA = BUSNM3                                                    M24. 429
      CSEPAR = TEXT1                                                    M24. 430
      CHCONT = TEXT2                                                    M24. 431
      TENM3 = 1.D-3                                                     M24. 432
      UNITY = 1.D0                                                      M24. 433
      ONEHAF = 0.5D0                                                    M24. 434
      INTINF = 10000000                                                 M33. 119
      NBYTE(1) = 2                                                      M24. 436
      NBYTE(2) = 2                                                      M32. 808
C     ZNVREF = 1.E-6  ! 32-BIT "COMPLEX" IN "CABLE CONSTANTS"           M35. 624
C     FOLLOWING ZERO DEFINITION IS REDUNDANT.                           M32. 809
      KTRLSW(6) = 0   ! USE "= 1" IF SIMPLE LOGIC IS DESIRED            M32. 810
      NBYTE(3) = 2                                                      M24. 437
      FLTINF = 1.D+20                                                   M24. 438
      FLZERO = 1.D-12                                                   M24. 439
      EPSILN = 1.D-8                                                    M24. 440
      TWOPI = 6.28318530717958647692D+00                                M24. 441
      USERID = BLANK                                                    M24. 444
      IF ( NOUTPR  .EQ.  0 )                                            M28.1848
     1 WRITE (LUNIT6, 6305)  DATE1, TCLOCK, (COL(J), J=6,18)            M28.1849
 6305 FORMAT ( 45H DATE (MM/DD/YY) AND TIME OF DAY (HH.MM.SS.)=,        M28.1850
     1    1X, 2A4, 2X, 2A4,  11X,  'NAME OF VAX/VMS PLOT DATA',         M38. 307
     2    ' FILE (IF ANY) =',  13A1 )                                   M38. 308
      IF ( M4PLOT .NE. 1 )   ! IF NOT INTERACTIVE EMTP USAGE,           M35. 625
     1 M4PLOT = 2     ! USE "PLTFIL" FOR REAL*4 PLOT FILE ON DISK       M35. 626
      LUNIT5 = -5    ! INTERACTIVE OR NOT USES "NEXTCARD", NOT 5        M35. 627
 9999 RETURN                                                            M29.1163
      ENTRY NEXTCARD                                                    M29.1164
C     THIS ENTRY IS USED ONLY FOR INTERACTIVE EMTP.  IT GETS            M35. 628
C     NEXT CARD IMAGE FROM MEMORY RATHER THAN UNIT 5.                   M29.1166
      N7 = NUMDCD + 1  ! NEXT DATA CARD IS RIGHT AFTER LAST             M35. 629
 1472 IF ( IPRSPY .LT. 5 )  GO TO 1486  ! JUMP AROUND DIAGNOSTIC        M37.1236
      WRITE (MUNIT6, 1477)  N7, FILE6(N7)                               M37.1237
 1477 FORMAT ( ' IN "NEXTCARD":',  I5, 1X, A80 )                        M35. 632
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37.1238
 1486 IF ( N7 .LE. NUMCRD ) GO TO 1488 ! AT LEAST 1 CARD REMAINS        M37.1239
      WRITE (LUNIT6, 1483)  NUMCRD                                      M37.1240
 1483 FORMAT (   '   ****  ****   DATA CRISIS.   LAST',                 M37.1241
     1           ' CARD HAS BEEN READ.   NUMCRD =', I6                  M37.1242
     2   ,/,     '                USE "DATA" COMMAND OF',               M35. 637
     3           ' SPY TO READ IN NEXT BLOCK OF DATA.'  )               M35. 638
      IF ( M4PLOT .NE. 1 )  GO TO 9000   ! SET KILL, THEN EXIT          M37.1243
      CALL EMTSPY     ! ALLOW USER TO CHANGE DATA CARD STORAGE          M35. 640
      GO TO 1472   ! LOOP BACK FOR ANOTHER TRY AT READING               M35. 641
 1488 READ (FILE6(N7), 1489)  ( ABUFF(J), J=1, 10 )                     M35. 642
 1489 FORMAT ( 10A8 )                                                   M35. 643
      GO TO 9200    ! EXIT MODULE WITH NEW CARD IMAGE IN ABUFF          M37.1244
 9000 KILL = 7654       ! POSITIVE KILL IS EOF FLAG IN "CIMAGE"         M37.1245
 9200 RETURN                                                            M37.1246
      END                                                               M40. 447
      SUBROUTINE MIDOV1                                                 M24. 450
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE CALLED ONLY FROM ONE LOCATION IN "OVER1" OF OVERLAY        M37.1247
C     ONE.  IT SHOULD BE ACCEPTABLE TO ALL FORTRAN 77 COMPILERS         M37.1248
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      EQUIVALENCE    (MONCAR(3), LTDELT)                                M37.1249
      IF ( M4PLOT .EQ. 1  .AND. ! IF INTERACTIVE EXECUTION (SPY),       M37.1250
     1     NENERG .EQ. 0 )      ! AND IF NOT MONTE CARLO STUDY,         M37.1251
     1 TMAX = FLTINF       ! SET END-TIME OF STUDY TO INFINITY          M36. 322
C     FLAG FOR POSTPROCESSING (LTDELT=-6789) SKIPS THE                  M28.1852
C     TAMPERING WITH OLD PLOT FILE NOW CONNECTED TO  LUNIT2:            M31. 350
      IF ( LTDELT  .EQ.  -6789 )   GO TO 1815                           M28.1854
      CLOSE (UNIT=LUNIT2, STATUS='DELETE' )                             M37.1252
      OPEN (UNIT=LUNIT2, STATUS='SCRATCH', FORM='UNFORMATTED')          M37.1253
 1815 IF (JFLSOS .EQ. 0) GO TO 4271                                     M29.1170
      IF (LASTOV .EQ. 20) GO TO 4271                                    M36. 323
      CLOSE (UNIT=LUNIT3)                                               M37.1254
      CLOSE (UNIT=LUNIT9)                                               M37.1255
      N4 = LUNIT3    ! 1ST OF TWO UNITS TO BE OPENED AS SCRATCH         M37.1256
 5910 WRITE (ANSI16, 5914)  N4, (LSTAT(J), J=14,16)                     M37.1257
 5914 FORMAT (2HST, I1, 3HLOG, 3I1, 4H.DAT, 3X )                        M37.1258
      OPEN ( UNIT=N4, STATUS='NEW', FILE=ANSI16,FORM='UNFORMATTED')
      IF (N4 .EQ. LUNIT9 )  GO TO 4271   ! BOTH OPENED, SO EXIT         M37.1260
      N4 = LUNIT9         ! SWITCH TO SECOND OF TWO I/O CHANNELS        M37.1261
      GO TO 5910                                                        M24. 471
 4271 IF ( NENERG .EQ. 0 )  GO TO 5933                                  M36. 327
C     "STATISITICS"  REQUIRES FORMATTED WRITES TO UNIT 12, WHICH        M32. 813
C     CONFLICTS WITH USUAL USAGE.                                       M32. 814
      CLOSE ( UNIT=LUNT12 )                                             M35. 644
      OPEN ( UNIT=LUNT12, TYPE='SCRATCH' )                              M37.1262
 5933 IF ( M4PLOT .NE. 1 ) GO TO 5947     ! NOT INTERACTIVE EMTP        M35. 646
      IF ( IPLOT .GE. 0 )  GO TO 5947  ! YES, PLOTTING REQUESTED        M35. 647
      IPLOT = INTINF    ! WRITE LITTLE TO PLOT FILE, NOT NOTHING        M35. 648
 5947 RETURN                                                            M35. 649
      END                                                               M24. 473
      SUBROUTINE NMINCR ( TEXTA, N12 )                                  M40. 448
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE DESIGNED TO SERIALIZE INPUT ROOT NAME  TEXTA  WITH DECIMAL M40. 449
C     COMPONENT NUMBER  N12,  ENCODING ONLY REQUIRED DIGITS.   IT IS    M40. 450
C     ASSUMED THAT A FORTRAN 77 COMPILER IS BEING USED, AND THAT        M40. 451
C     "ALPHANUMERIC" IS TRANSLATED TO SOMETHING OTHER THAN  CHARACTER*6 M40. 452
      REAL*8        TEXTA          ! INPUT ARGUMENT CARRIES A6 ROOT NAMEM40. 453
      CHARACTER*6  TEXT1, TEXT2     ! LOCAL CHARACTER-HANDLING VARIABLESM40. 454
      WRITE (TEXT1, 4523)  N12                 ! ENCODE COMPONENT NUMBERM40. 455
 4523 FORMAT ( I6 )        ! 6-DIGIT FORMAT ALLOWS NUMBER THROUGH 999999M40. 456
      DO 4538  J=1, 6     ! SEARCH FOR FIRST NON-BLANK IN ENCODED NUMBERM40. 457
      IF ( TEXT1(J:J) .NE. ' ' )  GO TO 4552        !  IF NONBLANK, EXITM40. 458
 4538 CONTINUE          ! END  DO 4538  LOOP TO FIND NON-BLANK LEFT EDGEM40. 459
 4552 WRITE (TEXT2, 4556)  TEXTA ! TRANSFER INPUT ALPHANUMERIC TO CHAR*6M40. 460
 4556 FORMAT ( A6 )       ! ALPHANUMERIC VARIABLES ARE 6 CHARACTERS WIDEM40. 461
      TEXT2(J:6) = TEXT1(J:6)     ! ADD COMPONENT NUMBER ONTO INPUT NAMEM40. 462
      READ (TEXT2, 4556)  TEXTA  ! CONVERT BACK FROM CHAR*6 TO ALPHANUM.M40. 463
C     WRITE (*,*)  ' Exit "NMINCR".   N12 =',  N12,  '   TEXTA =', TEXTAM40. 464
      RETURN                                                            M40. 465
      END                                                               M40. 466
      SUBROUTINE  TACS1                                                 M13. 340
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      REAL*8        STACS, ALPH, ALNODE, ALNM1, ALNM2                   M39. 372
      DIMENSION  STACS(11), ALPH(5), DUM(3), DUMJ(13)                   M39. 373
      REAL*8        DUMJ, SMINUS, SPLUS, SBN(2)                         M39. 374
      DATA  STACS(1)  / 6HTIMEX /                                       M39. 375
      DATA  STACS(2)  / 6HISTEP /                                       M39. 376
      DATA  STACS(3)  / 6HDELTAT/                                       M39. 377
      DATA  STACS(4)  / 6HFREQHZ/                                       M39. 378
      DATA  STACS(5)  / 6HOMEGAR/                                       M39. 379
      DATA  STACS(6)  / 6HZERO  /                                       M39. 380
      DATA  STACS(7)  / 6HPLUS1 /                                       M39. 381
      DATA  STACS(8)  / 6HMINUS1/                                       M39. 382
      DATA  STACS(9)  / 6HUNITY /                                       M39. 383
      DATA  STACS(10) / 6HINFNTY/                                       M39. 384
      DATA  STACS(11) / 6HPI    /                                       M39. 385
      DATA  SPLUS    /  1H+  /                                          M13. 350
      DATA  SMINUS   /  1H-  /                                          M13. 351
      DATA  SBN(1)   /  6HNUM.   /                                      M39. 386
      DATA  SBN(2)   /  6HDEN.   /                                      M39. 387
      IF ( IPRSUP  .GE.  1 ) WRITE ( LUNIT6, 4567 )                     M39. 388
 4567 FORMAT ( 23H  "BEGIN MODULE TACS1." )                             M32. 882
      IF (IPRSUP .GT. 0)                                                M22.1586
     1          WRITE (LUNIT6, 701) LTACST, (LSTAT(I), I=61, 68)        M39. 389
  701 FORMAT ( 6X, 35HLTACST, (LSTACS(I), I=1, 8) ... .     ,/,9I10/ )  M39. 390
      IF ( NCHAIN  .EQ.  1 )  GO TO 1001                                M38. 320
      NDY5 = KUD1 + ( NIU - 12 ) * 5                                    M38. 321
      GO TO 2210                                                        M39. 391
 1001 KRSBLK =  30                                                      M39. 392
      KUD1   =  KRSBLK + LSTAT(61) * 4                                  M39. 393
      KPRSUP =  KUD1   + LSTAT(64) * 5                                  M39. 394
      KXAR   =  KPRSUP + LSTAT(67)                                      M39. 395
      KXAI   =  KXAR   + LSTAT(68)                                      M39. 396
      KXTCS  =  KXAI   + LSTAT(68)                                      M39. 397
      KAWKCS =  KXTCS  + LSTAT(68) * 2                                  M39. 398
      KBWKCS =  KAWKCS + LSTAT(68)                                      M39. 399
      KATCS  =  KBWKCS + LSTAT(68)                                      M39. 400
      KBTCS  =  KATCS  + LSTAT(62)                                      M39. 401
      KISBLK = (KBTCS  + LSTAT(62) ) * NBYTE(3) / NBYTE(4) + 1          M39. 402
      KKSUS  =  KISBLK + LSTAT(61) * 8                                  M39. 403
      KALKSU =  KKSUS  + LSTAT(63)                                      M39. 404
      KIUTY  =  KALKSU + LSTAT(63)                                      M39. 405
      KINSUP =  KIUTY  + LSTAT(64)                                      M39. 406
      KIVARB =  KINSUP + LSTAT(65) * 3                                  M39. 407
      KLNTAB =  KIVARB + LSTAT(66)                                      M39. 408
      KJOUT  =  KLNTAB + LSTAT(68)                                      M39. 409
      KCOLCS =  KJOUT  + LSTAT(68)                                      M39. 410
      N1 = ( KCOLCS + LSTAT(62) ) * NBYTE(4) / NBYTE(3) + 1             M39. 411
      KALIU  =  KLNTAB                                                  M39. 412
      KSPVAR =  KALIU  + LSTAT(64)                                      M39. 413
      IF (IPRSUP .LT. 2)  GO TO 7811                                    M37.1288
      WRITE (LUNIT6, 703)  N1, KISBLK, KRSBLK, KKSUS , KALKSU,          M39. 414
     1 KIUTY , KUD1  , KINSUP, KIVARB, KPRSUP,                          M39. 415
     2 KAWKCS, KXAR  , KXTCS , KLNTAB, KATCS , KCOLCS, KJOUT            M39. 416
  703 FORMAT (   18H  USE CELLS  N1 = ,  I8 ,/,                         M39. 417
     1   11H  KISBLK = ,I5,  11H  KRSBLK = ,I5,  11H  KKSUS  = ,I5,     M38. 365
     2   11H  KALKSU = ,I5,  11H  KIUTY  = ,I5,  11H  KUD1   = ,I5,     M39. 418
     3   11H  KINSUP = ,I5,  11H  KIVARB = ,I5 ,/,                      M39. 419
     4   11H  KPRSUP = ,I5,  11H  KAWKCS = ,I5,  11H  KXAR   = ,I5,     M39. 420
     5   11H  KXTCS  = ,I5,  11H  KLNTAB = ,I5,  11H  KATCS  = ,I5,     M39. 421
     6   11H  KCOLCS = ,I5,  11H  KJOUT  = ,I5, /  )                    M39. 422
 7811 LSTAT(39) = N1                                                    M37.1307
      KONCUR = 0                                                                
      IF (N1 .LT. LTACST) GO TO 781                                     M22.1810
      KILL = 1                                                          M22.1811
      LSTAT(19) = 781                                                   M22.1812
      LSTAT(16) = 19                                                    M22.1813
      GO TO 9000                                                        M22.1814
  781 DO 99 I=1, 8                                                      M39. 423
   99 LSTAT( I) = LSTAT(I+60)                                           M39. 424
      DO 3737 I = 30, LTACST                                            M39. 425
 3737 PARSUP(I) = 0.0                                                   M39. 426
      ISOUR = LSTAT(4)                                                  M39. 427
      DO 3399 I = 1, ISOUR                                              M39. 428
      N23 = 0                                                           M38. 379
      IF ( I .GT. 11 )  GO TO 3388                                      M39. 429
      ALNM1 = STACS(I)                                                  M39. 430
      CALL NAMEA6(ALNM1, N23)                                           M39. 431
 3388 ILNTAB( KALIU+I) = N23                                            M39. 432
 3399 IUTY( KIUTY+I) = 0                                                M39. 433
      IUTY(KIUTY+3) = 20                                                        
      SPTACS(29) = FLTINF                                                       
      IF ( PU .EQ. FLTINF ) GO TO 4499                                  M42.1417
      SPTACS(29) = PU                                                   M42.1418
      IUTY(KIUTY+2) = NUK                                               M42.1419
      IUTY(KIUTY+3) = 0                                                 M42.1420
      IF (PU .LE. 0.) IUTY(KIUTY+3) = NUK                               M42.1421
 4499 D1 = ALOG1Z(FLTINF)                                               M42.1422
      IUTY(KIUTY+11) = D1 + 0.5                                         M39. 435
C                                                        $$$  INPUT  $$$M13. 364
      LBSTAC  =  LOCF( ISMTAC(1) )     -     LOCF( ETAC(1) )            M18. 246
      NTOTAC = 0                                                        M13. 366
      NSU = 0                                                           M13. 372
      NSUDV = LSTAT(63)                                                 M39. 436
      NUK = 0                                                           M37.1310
      NUKI = KISBLK - 8                                                 M38. 413
      NUKR = KRSBLK - 4                                                 M38. 414
      NIU = 11                                                          M28.1907
      NDY5 = KUD1 - 5                                                   M37.1311
      NIUNRS = 12                                                       M28.1908
      IUTY( KIUTY + 1 ) = NIUNRS                                        M28.1909
      NSUP = 0                                                          M14. 627
      KARG = KIVARB - 3                                                 M39. 437
      KPAR = 0                                                          M14. 629
      IOUTCS = 0                                                        M39. 438
      KXIC = 0                                                          M39. 439
      LL0 = 0                                                           M22.1820
      LL1 = 1                                                           M22.1821
      LL2 = 2                                                           M22.1822
      LL3 = 3                                                           M22.1823
      GO TO 2210                                                        M37.1312
  100 CALL CIMAGE                                                               
 2210 IF ( KILL  .GT.  0 )   GO TO 9000                                 M37.1313
      IF ( KOLBEG  .GT.  0 )   GO TO 6574                               M22.1826
      DECODE ( 80, 176, ABUFF(1) ) N                                    M39. 440
  176 FORMAT ( I2 )                                                     M39. 441
 1234 IF ( N .EQ. 88 .OR. N .EQ. 98 .OR. N .EQ. 99 ) GO TO 113          M39. 442
      IF ( N  .EQ.  33 ) GO TO 2222                                     M39. 443
      IF ( N  .EQ.  77 ) GO TO 3333                                     M39. 444
      IF ( N .GT. 10  .AND.  N .LT. 25 ) GO TO 1066                     M39. 445
      IF ( N .GT. 89  .AND.  N .LT. 94 ) GO TO 1066                     M39. 446
      IF ( KOLBEG  .GT.  0 ) GO TO 1111                                 M39. 447
      DECODE (80, 177, ABUFF(1)) (DUMJ(I), I = 1, 5 )                   M37.1314
      DECODE (80, 178, ABUFF(1) )   N, ALNODE,                          M37.1315
     1  (ALPH(I),I=1,5),(DUM(I),I=1, 3 ), ALNM1, ALNM2                  M37.1316
  177 FORMAT (10X,5(1X,A6,1X))                                          M22.1833
  178 FORMAT (I2,A6,2X,5(A1,7X),3E6.0,2A6)                              M22.1834
      GO TO 6590                                                        M22.1835
 6574 NFRFLD = 1                                                        M22.1836
      CALL FREFLD ( VOLTBC(1) )                                         M31. 354
      N = VOLTBC(1)                                                     M31. 355
      GO TO 1234                                                        M39. 448
 1111 NRIGHT = -1                                                       M39. 449
      CALL FREONE ( D1 )                                                M29.1178
      ALNODE = TEXTA6(1)                                                M22.1841
      DO 6587  I=1, 5                                                   M22.1842
 6576 BUS1 = TEXCOL(KOLBEG)                                             M22.1843
      IF ( BUS1  .NE.  BLANK )   GO TO 6577                             M22.1844
      KOLBEG = KOLBEG + 1                                               M22.1845
      GO TO 6576                                                        M22.1846
 6577 IF ( BUS1  .EQ.  SMINUS )   GO TO 6578                            M22.1847
      IF ( BUS1  .EQ.  SPLUS )   GO TO 6578                             M22.1848
      IF ( BUS1  .EQ.  CSEPAR )   GO TO 6584                            M22.1849
      ALPH(I) = SPLUS                                                   M22.1850
      GO TO 6581                                                        M22.1851
 6578 ALPH(I) = BUS1                                                    M22.1852
      KOLBEG = KOLBEG + 1                                               M22.1853
 6581 CALL FREONE ( D1 )                                                M29.1179
      DUMJ(I) = TEXTA6(1)                                               M37.1317
      GO TO 6587                                                        M22.1857
 6584 ALPH(I) = BLANK                                                   M22.1858
      DUMJ(I) = BLANK                                                   M37.1318
      KOLBEG = KOLBEG + 1                                               M22.1861
 6587 CONTINUE                                                          M22.1862
      NRIGHT = 0                                                        M22.1863
      NFRFLD = 3                                                        M22.1864
      CALL FREFLD ( DUM(1) )                                            M22.1865
      NRIGHT = -1                                                       M22.1866
      NFRFLD = 2                                                        M22.1867
      CALL FREONE ( D1 )                                                M29.1180
      ALNM1 = TEXTA6(1)                                                 M22.1869
      ALNM2 = TEXTA6(2)                                                 M22.1870
      NRIGHT = 0                                                        M22.1871
 6590 N1 = 0                                                            M39. 450
      DO 136  I=1, 5                                                    M13. 393
      IF ( DUMJ(I)  .EQ.  BLANK )   GO TO 136                           M39. 451
      IF ( ALPH(I)  .EQ.  SMINUS )   GO TO 135                          M13. 395
      IF ( ALPH(I)  .EQ.  SPLUS )   GO TO 135                           M13. 396
  133 KILL = 137                                                        M13. 397
      LSTAT(14) = I                                                     M13. 398
      BUS1 = ALNODE                                                     M13. 399
      BUS2 = DUMJ(I)                                                    M37.1320
      BUS3 = ALPH(I)                                                    M13. 401
      LSTAT(19) = 133                                                   M13. 402
      GO TO 9000                                                        M13. 403
  135 N1 = 1                                                            M13. 406
  136 CONTINUE                                                          M13. 407
      IF ( N1  .GT.  0 )   GO TO 139                                    M13. 408
      IF ( ALNODE  .NE.  BLANK )   GO TO 139                            M13. 409
      IF ( N  .NE.  1 ) GO TO 1399                                      M39. 452
      IUTY( KIUTY+6) = 9999                                             M39. 453
      IF ( NOUTPR  .EQ.  0 ) WRITE ( KUNIT6, 152 )                      M39. 454
  152 FORMAT (  42H+REQUEST FOR OUTPUT OF ALL TACS VARIABLES.   )       M39. 455
      GO TO 100                                                         M39. 456
 1399 IF ( N  .NE.  0 )   GO TO 139                                     M39. 457
      IF ( NOUTPR  .EQ.  0 )  WRITE ( KUNIT6, 137 )                     M39. 458
  137 FORMAT (  45H+BLANK CARD TERMINATING ALL TACS DATA CARDS .   )    M39. 459
      GO TO 101                                                         M13. 413
  139 IF ( DUM(1)  .EQ.  0.0 )  DUM(1) = 1.0                            M39. 460
      IF ( NOUTPR  .EQ.  0 )                                            M20. 751
     1 WRITE (KUNIT6, 179)  ALNODE, N, DUM(1)                           M35. 652
  179 FORMAT ( 16H+TACS FUNCTION ', A6, 8H', ORDER, I2, 1H., E14.4  )   M13. 417
      IF ( N .LT. 8  .AND.  N .GE. 0 )   GO TO 118                      M37.1321
      KILL = 115                                                        M37.1322
      LSTAT(19) = 118                                                   M13. 422
      IF ( NOUTPR  .EQ.  0 )  WRITE ( LUNIT6, 163 )                     M39. 461
  163 FORMAT(82H0LIMIT.  DIMENSIONS IN TACS HAVE NOT BEEN SET TO ACCEPT M13. 424
     1AN ORDER GREATER THAN  7.                                   ,/,   M37.1323
     286H IT IS SUGGESTED TO BREAK THIS MONSTRUOUS BLOCK INTO A SET OF SM13. 426
     3MALLER ONES IN CASCADE.  )                                        M13. 427
 7433 BUS1 = ALNODE                                                     M13. 428
      LSTAT(14) = N                                                     M13. 429
      GO TO 9000                                                        M13. 430
  118 NUK = NUK + 1                                                     M37.1324
      NUKI = NUKI + 8                                                   M38. 416
      NUKR = NUKR + 4                                                   M38. 417
      IF ( NUK .LE. LSTAT(61) )   GO TO 8101                            M39. 462
      KILL = 122                                                        M13. 433
      LSTAT( 16) = LSTAT(61)                                            M39. 463
      LSTAT( 17) = 1                                                    M14. 639
      LSTAT(19) = 8101                                                  M13. 436
      GO TO 9000                                                        M13. 437
 8101 NDX1 = KATCS  + NUK                                               M38. 420
      N23 = 0                                                           M38. 421
      CALL NAMEA6( ALNODE, N23 )                                        M38. 422
      ATCS(NDX1) = N23                                                  M38. 423
      ISBLK(NUKI+1) = NSU + 1                                           M38. 424
      ISBLK(NUKI+4) = 0                                                 M38. 425
 7409 N1 = 0                                                            M13. 441
      DO 7413  I=1, 5                                                   M13. 442
      IF ( DUMJ(I) .EQ. BLANK )  GO TO 7413                             M37.1331
      N1 = N1 + 1                                                       M38. 427
      NSU = NSU + 1                                                     M13. 445
      IF ( NSU .LE. NSUDV )   GO TO 8102                                M38. 428
      KILL = 122                                                        M13. 447
      LSTAT( 16) = LSTAT(63)                                            M39. 464
      LSTAT( 17) = 3                                                    M14. 641
      LSTAT(19) = 8102                                                  M13. 450
      GO TO 9000                                                        M13. 451
 8102 NDX1 = KALKSU + NSU                                               M22.1889
      N23 = 0                                                           M38. 430
      ALNODE = DUMJ(I)                                                  M39. 465
      CALL NAMEA6( ALNODE, N23 )                                        M39. 466
      KSUS(NDX1) = N23                                                  M38. 433
      NDX1 = KKSUS  + NSU                                               M22.1892
      KSUS(NDX1) = 1                                                    M22.1893
      IF ( ALPH(I)  .EQ.  SMINUS )   KSUS(NDX1) = -1                    M22.1895
      IF ( I  .EQ.  5 )  GO TO 7413                                     M38. 434
      MM = I + 1                                                        M38. 435
      DO 7418 IJ = MM, 5                                                M38. 436
      IF ( DUMJ(I)  .NE.  DUMJ(IJ) ) GO TO 7418                         M38. 437
      DUMJ(IJ) = BLANK                                                  M38. 438
      KSUS(NDX1) = KSUS(NDX1) + 1                                       M38. 439
      IF ( ALPH(IJ) .EQ. SMINUS )  KSUS(NDX1) = KSUS(NDX1) - 2          M38. 440
 7418 CONTINUE                                                          M38. 441
      IF ( KSUS(NDX1)  .NE.  0 )  GO TO 7413                            M38. 442
      NSU = NSU - 1                                                     M38. 443
      N1  =  N1 - 1                                                     M38. 444
 7413 CONTINUE                                                          M13. 455
      IF ( N1  .GT.  0 )   GO TO 7420                                   M38. 445
      KILL = 136                                                        M13. 457
      NDX1 = ATCS( KATCS + NUK )                                        M39. 467
      BUS1 = TEXVEC(NDX1)                                               M39. 468
      LSTAT(14) = N                                                     M13. 459
      LSTAT(19) = 7420                                                  M15. 425
      WRITE (LUNIT6,3)                                                  M17. 150
    3 FORMAT ( /,  34H ERROR.  NO INPUT NODE SPECIFIED.   )             M32. 895
      GO TO 9000                                                        M13. 463
 7420 IF ( N  .EQ.  0 )  GO TO 103                                      M37.1336
      RSBLK(NUKR+1) = 999999.99                                         M38. 446
      J = KPAR + 1                                                      M39. 469
      ISBLK(NUKI+3) = J                                                 M38. 447
      KCC = N * 6                                                       M39. 470
      KPAR = J + KCC + 3                                                M39. 471
      IF ( KPAR .LE. LSTAT(67) )   GO TO 8103                           M39. 472
      KILL = 122                                                        M13. 472
      LSTAT( 16) = LSTAT(67)                                            M39. 473
      LSTAT( 17) = 7                                                    M39. 474
      LSTAT(19) = 8103                                                  M13. 475
      GO TO 9000                                                        M13. 476
  103 RSBLK(NUKR+1) = DUM(1)                                            M38. 450
      ISBLK(NUKI+3) = 0                                                 M38. 451
 8103 ISBLK(NUKI+2) = N + 1                                             M38. 452
      NDX1 = KXAR + NUK                                                 M37.1347
      XAR(NDX1)  = DUM(2)                                               M37.1348
      NDX1 = KXAI + NUK                                                 M37.1349
      XAR(NDX1)  = DUM(3)                                               M39. 475
      NDX1 = KAWKCS + NUK                                               M38. 453
      N23 = 0                                                           M38. 454
      CALL NAMEA6( ALNM1, N23 )                                         M38. 455
      AWKCS(NDX1)  = N23                                                M38. 456
      NDX1 = KBWKCS + NUK                                               M38. 457
      N23 = 0                                                           M38. 458
      CALL NAMEA6( ALNM2, N23 )                                         M38. 459
      AWKCS(NDX1)  = N23                                                M39. 476
      IF (ALNM1 .NE. BLANK  .OR.  ALNM2 .NE. BLANK  .OR.                M37.1353
     1  DUM(2) .NE. 0.0 .OR. DUM(3) .NE. 0.0 ) ISBLK(NUKI+4) = -1       M38. 461
      IF ( N .EQ. 0 )  GO TO 100                                        M37.1355
      N3 = KCC                                                          M39. 477
      IF ( KCC .GT. 18 ) N3 = 18                                        M39. 478
      J1 = J + 1                                                        M39. 479
      NDX3 = KPRSUP + J1 + N3                                           M39. 480
      IJK = 0                                                           M39. 481
      DO 6517 J2 = J, J1                                                M39. 482
      CALL CIMAGE                                                       M13. 483
      IJK = IJK + 1                                                     M39. 483
      NDX1 = KPRSUP + J2                                                M39. 484
      IF ( KOLBEG  .GT.  0 )   GO TO 6514                               M22.1923
      NDX2 = KPRSUP + KPAR                                              M39. 485
      DECODE (80, 181, ABUFF(1)) (PARSUP(I), I=NDX1, NDX2, 6)           M39. 486
  181 FORMAT (8E10.0)                                                   M13. 485
      GO TO 6517                                                        M22.1927
 6514 NFRFLD =  1                                                       M37.1359
      N22 = N + 1                                                               
      DO 6565 IS = 1, N22                                               M39. 488
      CALL FREFLD ( PARSUP( NDX1 ) )                                    M39. 489
 6565 NDX1 = NDX1 + 6                                                   M39. 490
      NDX1 = KPRSUP + J2                                                M39. 491
 6517 IF ( NOUTPR  .EQ.  0 )                                            M39. 492
     1   WRITE (KUNIT6, 182) SBN(IJK), (PARSUP(I), I=NDX1, NDX3, 6)     M39. 493
  182 FORMAT ( 3H+  , A6,  4E10.2 )                                     M39. 494
      DO 102  I = J, KPAR, 6                                            M39. 495
      NDX1 = KPRSUP  + I                                                M39. 496
      PARSUP(NDX1+3) = PARSUP(NDX1+1)                                   M39. 497
      IF ( PARSUP(NDX1)  .EQ.  0.0 )   GO TO 119                        M39. 498
      PARSUP(NDX1) = PARSUP(NDX1) * DUM(1)                              M39. 499
      PARSUP(NDX1+2) = PARSUP(NDX1)                                     M39. 500
      GO TO 102                                                         M15. 429
  119 IF ( I .NE. KPAR - 3  .AND. I .NE. J )  GO TO 102                 M39. 501
      IF ( PARSUP(NDX1+1)  .NE.  0.0 )   GO TO 102                      M39. 502
      KILL = 116                                                        M15. 432
      LSTAT(19) = 119                                                   M15. 433
      LSTAT( 17) = 0                                                    M28.1911
      IF (I .EQ. J .AND. I .NE. KPAR-3) LSTAT(17) = 1                   M39. 503
      GO TO 7433                                                        M15. 434
  102 CONTINUE                                                          M15. 435
      GO TO 100                                                         M15. 436
  101 KONSCE = NIU                                                      M39. 504
      KONSUP = NSUP                                                     M39. 505
      IF ( NUK .GT. 0 )  GO TO 231                                      M39. 506
      NUK = 1                                                           M38. 463
      NSU = 1                                                           M38. 464
      ATCS(KATCS+1) = 1                                                 M38. 465
      ISBLK(KISBLK+4) = 0                                               M38. 466
      ISBLK(KISBLK+1) = 1                                               M38. 467
      KSUS(KALKSU+1) = 1                                                M38. 468
      KSUS(KKSUS+1) = 1                                                 M38. 469
      RSBLK(KRSBLK+1) = 1.0                                             M38. 470
      ISBLK(KISBLK+3) = 0                                               M38. 471
      ISBLK(KISBLK+2) = 1                                               M38. 472
      AWKCS(KAWKCS+1) = 1                                               M38. 475
      AWKCS(KBWKCS+1) = 1                                               M39. 507
  231 IF ( IPRSUP  .LT.  2 )  GO TO 4444                                M39. 508
      IF ( NUK  .EQ.  0 )  GO TO 2315                                   M39. 509
      WRITE ( LUNIT6, 1998 )  NUK                                       M39. 510
 1998 FORMAT ( 7H NUK = , I4 ,/, 10X,                                   M39. 511
     1         27H ALKSX    KSJ   KFST    KNI  )                        M39. 512
      NUKI = KISBLK - 8                                                 M39. 513
      DO 2000  N = 1, NUK                                               M39. 514
      NUKI = NUKI + 8                                                   M39. 515
 2000 WRITE (LUNIT6,1999)  N, ATCS(KATCS+N), ISBLK(NUKI+1),             M39. 516
     1 ISBLK(NUKI+2), ISBLK(NUKI+3)                                     M39. 517
 1999 FORMAT ( 2X, I4, 2X, F8.0, 3I7 )                                  M39. 518
      WRITE ( LUNIT6, 2005 )                                            M39. 519
     1   NSU, (N, KSUS(KALKSU+N), KSUS(KKSUS+N), N=1,NSU)               M39. 520
 2005 FORMAT ( 8H0 NSU = , I3  ,/, 8X, 11HKSU    KSUS  ,/,              M39. 521
     1  ( 1X, I3, 1X, 2I6 )  )                                          M39. 522
 2315 WRITE (LUNIT6,2006) NIU                                           M39. 523
 2006 FORMAT ( 8H0 NIU = ,I3 ,/, 8X,11HALIU   IUTY, 7X, 3HUD1,          M39. 524
     1   12X, 3HUD2, 12X, 3HUD3, 12X, 3HUD4, 12X, 3HUD5 )               M39. 525
      NDX5 = KUD1 - 5                                                   M39. 526
      DO 2323 N= 1, NIU                                                 M39. 527
      N1 = ILNTAB(KALIU+N)                                              M39. 528
      IF ( N  .GT.  11 )  GO TO 2121                                    M39. 529
      WRITE ( LUNIT6, 2306 ) N, TEXVEC(N1)                              M39. 530
      GO TO 2323                                                        M39. 531
 2121 NDX5 = NDX5 + 5                                                   M39. 532
      WRITE (LUNIT6,2306) N,TEXVEC(N1),IUTY(KIUTY+N),UD1(NDX5+1)        M39. 533
     1   ,UD1(NDX5+2), UD1(NDX5+3),  UD1(NDX5+4), UD1(NDX5+5)           M39. 534
 2306 FORMAT ( 1X,I3, 1X, A8,I6,5E15.6 )                                M39. 535
 2323 CONTINUE                                                          M39. 536
      IF ( NSUP .EQ. 0 )  GO TO 4444                                    M39. 537
      WRITE (LUNIT6,2007) NSUP, ( N, ILNTAB(KSPVAR+N),                  M39. 538
     1  INSUP(KJSUP+N), INSUP(KKSUP+N), N=1, NSUP )                     M39. 539
 2007 FORMAT (7H0 NSUP=, I6, /, 8X,                                     M39. 540
     1   24H  SUPVAR    JSUP    KSUP, /, ( 4I8 ) )                      M39. 541
      WRITE ( LUNIT6, 1033 ) KARG                                       M39. 542
 1033 FORMAT (9H  KARG = , I8 ,/, 24H       N  IOPSUP  IFNSUP,          M39. 543
     1  48H  IRGSUP    IDEV     KDJ     KDK  ILDEV1  ILDEV2  )          M39. 544
      DO 2034  I = 1, NSUP                                              M39. 545
      N1 = INSUP( KJSUP + I )                                           M39. 546
      IF (  N1  .LT.  0 )  GO TO 2014                                   M39. 547
      N2 = INSUP( KKSUP + I )                                           M39. 548
      WRITE (LUNIT6,2008) (N,IVARB(N+1),IVARB(N+2),                     M39. 549
     2 IVARB(N+2), N = N1, N2, 3)                                       M39. 550
 2008 FORMAT (4I8 )                                                     M39. 551
      GO TO 2034                                                        M39. 552
 2014 N1 = -N1                                                          M39. 553
      WRITE (LUNIT6, 2022 ) N1, IVARB(N1), IVARB(N1+1),                 M39. 554
     1 IVARB(N1+2), IVARB(N1+3), IVARB(N1+4)                            M39. 555
 2022 FORMAT ( I8, 24X, 5I8 )                                           M39. 556
 2034 CONTINUE                                                          M39. 557
      IF ( KPAR .NE. 0 )                                                M39. 558
     1   WRITE (LUNIT6,2009) KPAR, ( PARSUP(KPRSUP+N), N=1,KPAR )       M39. 559
 2009 FORMAT (7H  KPAR=, I8, 6X, 18HPARSUP AS FOLLOWS: ,/,(8E16.6))     M39. 560
      N22 = NSUDV + 1                                                   M39. 561
      IF ( NSUDV .LT. LSTAT(63) )                                       M39. 562
     1     WRITE (LUNIT6,10031) NSUDV, ( N, KSUS(KALKSU+N),             M39. 563
     2      KSUS(KKSUS+N), N = N22,  LSTAT(63))                         M39. 564
10031 FORMAT (8H0 NSUDV=, I4/12X,13HALKSU    KSUS/(1X, 3I8))            M39. 565
      GO TO  4444                                                       M39. 566
 1066 IF ( KILL  .GT.  0 )   GO TO 9000                                 M37.1385
      IF ( KOLBEG  .GT.  0 )   GO TO 6534                               M22.1968
      DECODE (80, 187, ABUFF(1))                                        M13. 562
     1            N, ALNODE, DUM(1), DUM(3), DUM(2), PRX, PRU           M15. 438
  187 FORMAT (I2,A6,2X,3E10.0,20X,2E10.0)                               M14. 648
      GO TO 6537                                                        M22.1969
 6534 NRIGHT = -1                                                       M39. 567
      CALL FREONE ( D1 )                                                M29.1181
      ALNODE = TEXTA6(1)                                                M22.1975
      NRIGHT = 0                                                        M22.1976
      NFRFLD = 5                                                        M22.1977
      CALL FREFLD ( VOLTBC(1) )                                         M31. 358
      DUM(1) = VOLTBC(1)                                                M31. 359
      DUM(3) = VOLTBC(2)                                                M31. 360
      DUM(2) = VOLTBC(3)                                                M31. 361
      PRX    = VOLTBC(4)                                                M31. 362
      PRU    = VOLTBC(5)                                                M31. 363
 6537 NIU = NIU + 1                                                     M39. 568
      NDY5 = NDY5 + 5                                                   M37.1386
      IF ( NIU  .GT.  LSTAT(64) )   GO TO 8107                          M39. 569
      NDX1 = KIUTY + NIU                                                M28.1917
      IUTY(NDX1) = N                                                    M22.1986
      NDX1 = KALIU  + NIU                                               M22.1987
      N23 = 0                                                           M38. 478
      CALL NAMEA6( ALNODE, N23 )                                        M38. 479
      ILNTAB(NDX1) = N23                                                M38. 480
      UD1(NDY5+1)  = DUM(1)                                             M37.1389
      UD1(NDY5+2) = DUM( 2)                                             M37.1390
      IF ( N .NE. 14 )  GO TO 8108                                      M39. 570
      UD1(NDY5+2)  = DUM(2) * TWOPI/360.                                M37.1391
 8108 IF ( N .NE. 23 )  GO TO 8109                                      M39. 571
      IF ( UD1(NDY5+2) .LT. DELTAT )  UD1(NDY5+2) = DELTAT              M37.1392
 8109 CONTINUE                                                          M28.1924
      UD1(NDY5+3)  = DUM(3)                                             M37.1393
      UD1(NDY5+4)  = PRX                                                M37.1394
      UD1(NDY5+5)  = FLTINF                                             M37.1395
      IF ( PRU .NE. 0.0 )  UD1(NDY5+5)  = PRU                           M37.1396
      IF ( NOUTPR  .EQ.  0 )                                            M20. 765
     1 WRITE (KUNIT6, 170)  ( DUM(I), I=1, 3 )                          M35. 656
  170 FORMAT (  13H+TACS SOURCE.  ,   3E12.3  )                         M13. 589
      IF ( NCHAIN  .EQ.  1 )  GO TO 100                                 M39. 572
 1090 IF ( N  .NE. 90 ) GO TO 1091                                      M37.1400
      DO 476 J=2,NTOT                                                   M37.1401
      IF ( ALNODE .EQ. BUS(J) )  GO TO 479                              M37.1402
  476 CONTINUE                                                          M37.1403
      WRITE (LUNIT6,168) ALNODE                                         M38. 482
  168 FORMAT (18H  TYPE 90 SOURCE ",A6,23H" IS NOT A RECOGNIZABLE,      M38. 483
     1 45H NODE NAME IN EMTP. THE CARD WILL BE DISCARD  )               M38. 484
      GO TO 8579                                                        M38. 485
 1091 IF ( N1 .NE. 91  .AND.  N1 .NE. 93 )  GO TO 1092                  M37.1410
      DO 478 J=1,KSWTCH                                                 M37.1411
      K = IABS(KMSWIT(J))                                               M37.1412
      NDX2 = LSWTCH + J                                                 M37.1413
      M = IABS( KMSWIT(NDX2))                                           M37.1414
      IF ( ALNODE  .EQ.  BUS(K) )   GO TO 479                           M37.1415
      IF ( ALNODE  .EQ.  BUS(M) )   GO TO 479                           M37.1416
  478 CONTINUE                                                          M37.1417
      WRITE (LUNIT6, 55168) ALNODE                                      M38. 486
55168 FORMAT (19H  SOURCE 90 OR 91 ",A6,23H" IS NOT A RECOGNIZABLE,     M38. 487
     1 52H SWITCH NODE NAME IN EMTP. THE CARD WILL BE DISCARD  )        M38. 488
 8579 NIU  = NIU  - 1                                                   M38. 489
      NDY5 = NDY5 - 5                                                   M38. 490
      GO TO 9000                                                        M38. 491
 1092 IF ( N1  .NE.  92 )   GO TO 8579                                  M39. 573
      WRITE ( LUNIT6, 5566 )                                            M39. 574
 5566 FORMAT ( 46H  ----- THIS CARD WILL BE IGNORED, BECAUSE #92,       M39. 575
     1         33H SOURCE CANNOT BE CHANGED   ----  )                   M39. 576
      GO TO  8579                                                       M39. 577
  479 UD1(NDY5+2) = J                                                   M38. 505
      GO TO 9000                                                        M37.1438
 8107 KILL = 122                                                        M37.1439
      LSTAT( 16) = LSTAT(64)                                            M39. 578
      LSTAT( 17) = 4                                                    M37.1441
      LSTAT(19) = 8107                                                  M37.1442
      GO TO 9000                                                        M37.1443
 2222 IF ( KOLBEG  .GT.  0 )   GO TO 6554                               M39. 579
      DECODE (80, 190, ABUFF(1))  (DUMJ(I), I=1, 13)                    M39. 580
  190 FORMAT ( 2X, 13A6 )                                               M39. 581
      GO TO 6557                                                        M39. 582
 6554 NFRFLD = 13                                                       M39. 583
      NRIGHT = -1                                                       M39. 584
      CALL FREONE ( D1 )                                                M39. 585
      NRIGHT = 0                                                        M39. 586
      DO 6555  I=1, 13                                                  M39. 587
 6555 DUMJ(I) = TEXTA6(I)                                               M39. 588
 6557 JR = 0                                                            M39. 589
      DO 1199 I = 1, 13                                                 M39. 590
      IF ( DUMJ(I) .EQ. BLANK ) GO TO 1199                              M39. 591
      IOUTCS = IOUTCS + 1                                               M39. 592
      IF ( IOUTCS  .GT.  LSTAT(68) ) GO TO 4466                         M39. 593
      JR = 1                                                            M39. 594
      N23 = 0                                                           M42.1423
      ALNODE = DUMJ(I)                                                  M39. 596
      CALL NAMEA6( ALNODE, N23 )                                        M39. 597
      NDX1 = KJOUT + IOUTCS                                             M39. 598
      JOUT(NDX1) = N23                                                  M39. 599
 1199 CONTINUE                                                          M39. 600
      IF ( JR .EQ. 1 ) GO TO 3535                                       M39. 601
      IF ( NOUTPR .EQ. 0 ) WRITE ( KUNIT6, 4455 )                       M39. 602
 3535 IF ( NOUTPR  .EQ.  0 ) WRITE ( KUNIT6, 154 )                      M39. 603
  154 FORMAT (  39H+TACS VARIABLES FOR EMTP OUTPUT VECTOR.   )          M39. 604
      GO TO 100                                                         M39. 605
 4466 LSTAT(19) = 1199                                                  M39. 606
 9300 KILL = 122                                                        M39. 607
      LSTAT(16) = LSTAT(68)                                             M39. 608
      LSTAT(17) = 8                                                     M39. 609
      GO TO 9000                                                        M39. 610
 3333 IF ( KOLBEG  .GT.  0 )   GO TO 6564                               M39. 611
      DECODE (80,195,ABUFF(1))  ALNODE, PRX                             M39. 612
  195 FORMAT ( 2X, A6, 2X, E10.0)                                       M39. 613
      GO TO 6567                                                        M39. 614
 6564 NFRFLD = 1                                                        M39. 615
      NRIGHT = -1                                                       M39. 616
      CALL FREONE ( D1 )                                                M39. 617
      ALNODE = TEXTA6(1)                                                M39. 618
      NRIGHT = 0                                                        M39. 619
      CALL FREONE ( PRX )                                               M39. 620
 6567 IF ( ALNODE  .NE.  BLANK )   GO TO 3838                           M39. 621
      IF ( NOUTPR .EQ. 0 ) WRITE ( KUNIT6, 4455 )                       M39. 622
 4455 FORMAT ( 24H+IGNOR THE ILLEGAL CARD. )                            M39. 623
      GO TO 100                                                         M39. 624
 3838 KXIC = KXIC + 1                                                   M39. 625
      IF ( KXIC .LT. LSTAT(68) ) GO TO 3377                             M39. 626
      LSTAT(19) = 3377                                                  M39. 627
      GO TO 9300                                                        M39. 628
 3377 NDX1 = KXTCS + LSTAT(68) + KXIC                                   M39. 629
      XTCS(NDX1) = PRX                                                  M39. 630
      NDX2 = KXTCS + KXIC                                               M39. 631
      N23 = 0                                                           M39. 632
      CALL NAMEA6( ALNODE, N23 )                                        M39. 633
      XTCS(NDX2) = N23                                                  M39. 634
      IF ( NOUTPR .EQ. 0 )  WRITE (KUNIT6, 4195)  ALNODE, PRX           M39. 635
 4195 FORMAT (14H+INIT. COND. ', A6, 1H', E16.6)                        M39. 636
      GO TO 100                                                         M39. 637
  113 CALL TACS1A                                                       M39. 638
      IF ( KILL .GT. 0 ) GO TO 9000                                     M39. 639
      IF ( NCHAIN .EQ. 1 ) GO TO 100                                    M39. 640
      KTAB = KTAB + 1                                                   M39. 641
      IF ( KTAB  .LE.  LSTAT(68) )  GO TO 4444                          M39. 642
      LSTAT(19) = 113                                                   M39. 643
      GO TO 9300                                                        M39. 644
 4444 CALL TACS1B                                                       M39. 645
 9000 IF ( IPRSUP  .GE.  1 )                                            M33. 122
     1 WRITE ( LUNIT6,4568 )                                            M32. 912
 4568 FORMAT ( 23H  "EXIT  MODULE TACS1." )                             M32. 913
      RETURN                                                            M33. 123
      END                                                               M24. 522
      SUBROUTINE TACS1A                                                 M37.1446
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      REAL*8        ALPH, DUMJ                                          M38. 507
      DIMENSION  ALPH(5), DUM(3), DUMJ(13)                              M38. 508
      REAL*8       TEXT1, TEXT2, TEXT4, SMINUS, SPLUS, SMULTP           M39. 646
      REAL*8       SUPFN, SUPOP, TEXT5, TEXT6, TEXT7                    M39. 647
      REAL*8        ALNODE, ALNM1, ALNM2, ALPHI                         M28.1927
      DIMENSION SUPFN( 35), SUPOP( 6)                                   M38. 510
      REAL*8       EL, ALNRCL, SEPCH, OPNAME, ATMPBF, BTMPBF            M28.1929
      REAL*8       CSPRCH, CURCH, CURCH1, CONTCH, CHDUM1, CHDUM2        M28.1930
      REAL*8       EQLSGN, CH9, CHDOLR, COMMA, CHE, CHD                 M28.1931
      REAL*8       CHA, CHN, CHO, CHT, CHQ, CHL, CHG, CHR               M28.1932
      DIMENSION EL(100), ALNRCL(10), SEPCH(8), OPNAME(18)               M28.1934
      DIMENSION ATMPBF(20), BTMPBF(80)                                  M28.1935
      DIMENSION ARGEL(100)                                              M28.1936
      DIMENSION IEL(100)                                                M28.1937
      DATA ALNRCL( 1) /6H1     /                                        M28.1938
      DATA ALNRCL( 2) /6H2     /                                        M28.1939
      DATA ALNRCL( 3) /6H3     /                                        M28.1940
      DATA ALNRCL( 4) /6H4     /                                        M28.1941
      DATA ALNRCL( 5) /6H5     /                                        M28.1942
      DATA ALNRCL( 6) /6H6     /                                        M28.1943
      DATA ALNRCL( 7) /6H7     /                                        M28.1944
      DATA ALNRCL( 8) /6H8     /                                        M28.1945
      DATA ALNRCL( 9) /6H9     /                                        M28.1946
      DATA ALNRCL(10) /6H0     /                                        M28.1947
      DATA SEPCH( 1) /6H(     /                                         M28.1948
      DATA SEPCH( 2) /6H)     /                                         M28.1949
      DATA SEPCH( 3) /6H+     /                                         M28.1950
      DATA SEPCH( 4) /6H-     /                                         M28.1951
      DATA SEPCH( 5) /6H/     /                                         M28.1952
      DATA SEPCH( 6) /6H*     /                                         M28.1953
      DATA SEPCH( 7) /6H.     /                                         M28.1954
      DATA SEPCH( 8) /6H**    /                                         M28.1955
      DATA OPNAME( 1) /6H(     /                                        M28.1956
      DATA OPNAME( 2) /6H.NOT. /                                        M28.1957
      DATA OPNAME( 3) /6H.OR.  /                                        M28.1958
      DATA OPNAME( 4) /6H.ORNOT/                                        M28.1959
      DATA OPNAME( 5) /6H.AND. /                                        M28.1960
      DATA OPNAME( 6) /6H.ANDNT/                                        M28.1961
      DATA OPNAME( 7) /6H)     /                                        M28.1962
      DATA OPNAME( 8) /6H.NE.  /                                        M28.1963
      DATA OPNAME( 9) /6H.EQ.  /                                        M28.1964
      DATA OPNAME(10) /6H.LT.  /                                        M28.1965
      DATA OPNAME(11) /6H.LE.  /                                        M28.1966
      DATA OPNAME(12) /6H.GE.  /                                        M28.1967
      DATA OPNAME(13) /6H.GT.  /                                        M28.1968
      DATA OPNAME(14) /6H*     /                                        M28.1969
      DATA OPNAME(15) /6H/     /                                        M28.1970
      DATA OPNAME(16) /6H**    /                                        M28.1971
      DATA OPNAME(17) /6H+     /                                        M28.1972
      DATA OPNAME(18) /6H-     /                                        M28.1973
      DATA EQLSGN /6H=     /                                            M28.1974
      DATA CHDOLR /6H$     /                                            M28.1975
      DATA CH9    /6H9     /                                            M28.1976
      DATA  CHE   /1HE/                                                 M28.1977
      DATA  CHD   /1HD/                                                 M28.1978
      DATA  COMMA /1H,/                                                 M28.1979
      DATA  CHA   /1HA/                                                 M28.1980
      DATA  CHN   /1HN/                                                 M28.1981
      DATA  CHO   /1HO/                                                 M28.1982
      DATA  CHT   /1HT/                                                 M28.1983
      DATA  CHQ   /1HQ/                                                 M28.1984
      DATA  CHL   /1HL/                                                 M28.1985
      DATA  CHG   /1HG/                                                 M28.1986
      DATA  CHR   /1HR/                                                 M28.1987
      DATA SUPFN( 1) /5H     /                                          M24. 537
      DATA SUPFN( 2) /5HAND. /                                          M24. 538
      DATA SUPFN( 3) /5HOR.  /                                          M24. 539
      DATA SUPFN( 4) /5HNOT. /                                          M24. 540
      DATA SUPFN( 5) /5HNAND./                                          M24. 541
      DATA SUPFN( 6) /5HNOR. /                                          M24. 542
      DATA SUPFN( 7) /5H     /                                          M24. 543
      DATA SUPFN( 8) /5H     /                                          M24. 544
      DATA SUPFN( 9) /5H     /                                          M24. 545
      DATA SUPFN(10) /5H     /                                          M24. 546
      DATA SUPFN(11) /5H     /                                          M24. 547
      DATA SUPFN(12) /5HSIN  /                                          M24. 548
      DATA SUPFN(13) /5HCOS  /                                          M24. 549
      DATA SUPFN(14) /5HTAN  /                                          M24. 550
      DATA SUPFN(15) /5HCOTAN/                                          M24. 551
      DATA SUPFN(16) /5HSINH /                                          M24. 552
      DATA SUPFN(17) /5HCOSH /                                          M24. 553
      DATA SUPFN(18) /5HTANH /                                          M24. 554
      DATA SUPFN(19) /5HASIN /                                          M24. 555
      DATA SUPFN(20) /5HACOS /                                          M24. 556
      DATA SUPFN(21) /5HATAN /                                          M24. 557
      DATA SUPFN(22) /5HEXP  /                                          M24. 558
      DATA SUPFN(23) /5HLOG  /                                          M24. 559
      DATA SUPFN(24) /5HLOG10/                                          M24. 560
      DATA SUPFN(25) /5HSQRT /                                          M24. 561
      DATA SUPFN(26) /5HABS  /                                          M24. 562
      DATA SUPFN(27) /5HTRUNC/                                          M24. 563
      DATA SUPFN(28) /5HMINUS/                                          M24. 564
      DATA SUPFN(29) /5HINVRS/                                          M24. 565
      DATA SUPFN(30) /5HRAD  /                                          M24. 566
      DATA SUPFN(31) /5HDEG  /                                          M24. 567
      DATA SUPFN(32) /5HSEQ6 /                                          M24. 568
      DATA SUPFN(33)  /5HSIGN /                                         M28.1989
      DATA SUPFN(34)  /5HNOT  /                                         M28.1990
      DATA SUPFN(35)  /5HRAN  /                                         M38. 511
      DATA SUPOP( 1) /5H+    /                                          M24. 569
      DATA SUPOP( 2) /5H-    /                                          M24. 570
      DATA SUPOP( 3) /5H*    /                                          M24. 571
      DATA SUPOP( 4) /5H/    /                                          M24. 572
      DATA SUPOP( 5) /5H0    /                                          M24. 573
      DATA SUPOP( 6) /5H.    /                                          M24. 574
      DATA  TEXT4   / 6HCOUNTR /                                        M28.1991
      DATA  TEXT5   /  6HINPUT   /                                      M35. 659
      DATA  TEXT6   /  6HOUTPUT  /                                      M35. 660
      DATA  TEXT7   /  6HINSIDE  /                                      M35. 661
      DATA  SPLUS    /  1H+  /                                          M24. 580
      DATA  SMINUS   /  1H-  /                                          M24. 581
      DATA  SMULTP   /  1H*  /                                          M39. 648
      DATA  TEXT1   /  6HVALUE   /                                      M35. 662
      IF ( IPRSUP  .GE.  1 )                                            M32. 914
     1 WRITE ( LUNIT6, 4567 )                                           M32. 915
 4567 FORMAT ( 24H  "BEGIN MODULE TACS1A." )                            M32. 916
      KJSUP = KINSUP + LSTAT(65)                                        M39. 649
      KKSUP = KJSUP  + LSTAT(65)                                        M39. 650
      DECODE (80,180,ABUFF(1))  N, ALNODE, M                            M14. 651
  180 FORMAT ( I2, A6, I2)                                              M14. 652
  144 NSUP = NSUP + 1                                                   M39. 651
      IF ( NSUP .LE. LSTAT(65) )  GO TO 2515                            M39. 652
      KILL = 122                                                        M14. 672
      LSTAT( 19) = 2515                                                 M39. 653
      LSTAT( 16) = LSTAT(65)                                            M39. 654
      LSTAT( 17) = 5                                                    M14. 675
      GO TO 9000                                                        M14. 676
 2515 NDX1 = KSPVAR +  NSUP                                             M39. 655
      N23 = 0                                                           M38. 518
      CALL NAMEA6( ALNODE, N23 )                                        M38. 519
      ILNTAB(NDX1) = N23                                                M38. 520
      INSUP(KINSUP+NSUP) = 0                                            M37.1472
      IF ( M .NE. 0 )  GO TO 10700                                      M14. 679
      DECODE (80,12516,ABUFF(1))  ALNM1                                 M28.1995
12516 FORMAT ( 10X, A1)                                                 M28.1996
      IF ( ALNM1 .NE. EQLSGN )  GO TO 12517                             M28.1997
      IF ( NOUTPR .EQ. 0 ) WRITE ( KUNIT6, 1251 )                       M39. 656
 1251 FORMAT ( 38H+TACS SUPPLEMENTAL FORTRAN EXPRESSION  )              M39. 657
C     ***  READ  THIS  ARBITRARY  LOGICAL/ALGEBRAIC                     M28.2001
C     ***   FREE-FORMAT  FORTRAN  EXPRESSION                            M28.2002
C     ***  * * * * * * * * * * * * * * * * *   ***                      M28.2003
C                                                                       M28.2004
      IARGEL =  0                                                       M28.2005
      ILST   =  0                                                       M28.2006
      ICURCH = 11                                                       M28.2007
      ISRCHL =  1                                                       M28.2008
      IFLPNT =  0                                                       M28.2009
      ILGLPH =  0                                                       M28.2010
      ILGNUM =  0                                                       M28.2011
      ILGCL = 0                                                         M28.2012
      ITMPBF =  0                                                       M28.2013
      IFSTCH = 12                                                       M28.2014
      CURCH = CHDOLR                                                    M28.2015
      CSPRCH = CSEPAR                                                   M28.2016
      CSEPAR = COMMA                                                    M28.2017
      CONTCH = CHCONT                                                   M39. 658
      IF ( CONTCH .EQ. CH9 )  CONTCH = CHDOLR                           M28.2019
      IFREE = 100                                                       M29.1182
      CALL MOVE0 (IEL(1),IFREE)                                         M29.1183
C                                                                       M28.2020
C    :::  INTERPRET NEXT CHARACTER   :::                                M28.2021
50010 ICURCH = ICURCH + 1                                               M28.2022
      IF ( ICURCH .GT. 80 ) GO TO 50030                                 M28.2023
      CURCH1 = CURCH                                                    M28.2024
      CURCH = TEXCOL( ICURCH)                                           M28.2025
      IF ( CURCH .EQ. BLANK )  GO TO 50035                              M28.2026
      IF ( CURCH .EQ. CONTCH )  GO TO 50040                             M28.2027
      K1 =  0                                                           M28.2028
      DO 50015  J= 1, 7                                                 M28.2029
        IF ( CURCH .EQ. SEPCH( J))  K1 = J                              M28.2030
50015   CONTINUE                                                        M28.2031
      IF ( ISRCHL .EQ. 1 )  GO TO 50065                                 M28.2032
      IF ( ILGCL.EQ.1 .AND. IFLPNT.EQ.2 )  GO TO 50120                  M28.2033
      IF ( K1 .EQ. 0 )  GO TO 50020                                     M28.2034
      IF ( ILGNUM  .EQ.  0 )  GO TO 50016                               M28.2035
      IF ( K1 .NE. 3  .AND.  K1 .NE. 4 )  GO TO 50016                   M28.2036
      IF ( CURCH1 .EQ. CHE  .OR.  CURCH1 .EQ. CHD )  GO TO 50020        M28.2037
50016 IF ( K1 .NE. 6  .OR.  CURCH1 .NE. SEPCH( 6) )  GO TO 50017        M28.2038
      K1 = 8                                                            M28.2039
      GO TO 50165                                                       M28.2040
50017 IF ( K1 .GE. 1  .AND.  K1 .LE. 6 )  GO TO 50120                   M28.2041
C                                                                       M28.2042
C     :::  THIS CHAR IS '.' NOT FIRST  :::                              M28.2043
      IFLPNT = IFLPNT + 1                                               M28.2044
      IF ( ILGCL .EQ. 1 )  GO TO 50020                                  M28.2045
C                                                                       M28.2046
C     :::  THIS  '.'  FOLLOWS ALPH OR NUM  :::                          M28.2047
      GO TO 50021                                                       M28.2048
50019 IF ( ILGCL .EQ. 1 )  GO TO 50120                                  M28.2049
C                                                                       M28.2050
C     :::  THIS CHAR NOT FIRST NOT SEPAR  :::                           M28.2051
50020 IF ( ILGLPH .GT. 0 )  ILGLPH = ILGLPH + 1                         M28.2052
      IF ( ILGNUM .GT. 0 )  ILGNUM = ILGNUM + 1                         M28.2053
      GO TO 50010                                                       M28.2054
C                                                                       M28.2055
C     :::  PREREAD  4  CHAR  :::                                        M28.2056
50021 IF ( ICURCH .GT. 77 )  GO TO 50024                                M28.2057
      CURCH1 = TEXCOL( ICURCH + 1 )                                     M28.2058
      CHDUM1 = TEXCOL( ICURCH + 2 )                                     M28.2059
      CHDUM2 = TEXCOL( ICURCH + 3 )                                     M28.2060
      IF ( CHDUM2 .EQ. SEPCH( 7) )  GO TO 50022                         M28.2061
      IF ( ICURCH .GT. 76 )  GO TO 50024                                M28.2062
      IF ( TEXCOL( ICURCH + 4 ) .NE. SEPCH( 7) )  GO TO 50024           M28.2063
      IF(CURCH1.EQ.CHN .AND. CHDUM1.EQ.CHO .AND. CHDUM2.EQ.CHT )        M28.2064
     1  GO TO 50023                                                     M28.2065
      IF (CURCH1.EQ.CHA .AND. CHDUM1.EQ.CHN .AND. CHDUM2.EQ.CHD )       M28.2066
     1  GO TO 50023                                                     M28.2067
      GO TO 50024                                                       M28.2068
50022 IF (CURCH1.EQ.CHO .AND. CHDUM1.EQ.CHR )  GO TO 50023              M28.2069
      IF (CURCH1.EQ.CHN .AND. CHDUM1.EQ.CHE )  GO TO 50023              M28.2070
      IF (CURCH1.EQ.CHE .AND. CHDUM1.EQ.CHQ )  GO TO 50023              M28.2071
      IF (CHDUM1.NE.CHT .AND. CHDUM1.NE.CHE )  GO TO 50024              M28.2072
      IF (CURCH1.NE.CHL .AND. CURCH1.NE.CHG )  GO TO 50024              M28.2073
50023 ILGCL = 1                                                         M28.2074
50024 IF ( ILGLPH+ILGNUM .GT. 0 )  GO TO 50019                          M28.2075
      IF ( ILGCL .EQ. 0 )  GO TO 50025                                  M28.2076
      ILGLPH = 1                                                        M28.2077
      GO TO 50010                                                       M28.2078
50025 DO 50026  J = 1, 10                                               M28.2079
      IF ( CURCH1 .EQ. ALNRCL( J) )  GO TO 50028                        M28.2080
50026 CONTINUE                                                          M28.2081
50027   IKILL1 = 12                                                     M28.2082
        IKILL2 = 50026                                                  M28.2083
        LSTAT( 16) = ICURCH                                             M28.2084
       GO TO 50900                                                      M28.2085
50028 ILGNUM = 1                                                        M28.2086
      GO TO 50010                                                       M28.2087
C                                                                       M28.2088
C    :::  HIT COL. 81  :::                                              M28.2089
50030 IF ( ISRCHL .EQ. 1 )  GO TO 50185                                 M28.2090
      ISRCHL = -1                                                       M28.2091
      GO TO 50120                                                       M28.2092
C                                                                       M28.2093
C    :::  HIT A BLANK  :::                                              M28.2094
50035 IF ( ISRCHL .EQ. 1 )  GO TO 50010                                 M28.2095
      IF ( IFLPNT .EQ. 1  .AND. CURCH1 .EQ. SEPCH(7)                    M28.2096
     1  .AND.  ILGNUM .EQ. 0 )  GO TO 50027                             M28.2097
      ISRCHL = 1                                                        M28.2098
      GO TO 50120                                                       M28.2099
C                                                                       M28.2100
C    :::  HIT CONTINUATION-CHARACTER  :::                               M28.2101
50040 IF ( ISRCHL .EQ. 1 )  GO TO 50050                                 M28.2102
      ICURCH = ICURCH - 1                                               M28.2103
      IF ( ICURCH .LT. IFSTCH )  GO TO 50050                            M28.2104
      DO 50045  J = IFSTCH, ICURCH                                      M28.2105
        ITMPBF = ITMPBF + 1                                             M28.2106
50045   ATMPBF( ITMPBF) = TEXCOL( J)                                    M28.2107
50050 ICURCH = 0                                                        M28.2108
      IFSTCH = 1                                                        M28.2109
      CALL CIMAGE                                                       M28.2110
      GO TO 50010                                                       M28.2111
C                                                                       M28.2112
C    :::  FIRST CHARACTER OF A NEW WORD  :::                            M28.2113
50065 ISRCHL = 0                                                        M28.2114
      IFSTCH = ICURCH                                                   M28.2115
      IF ( K1 .NE. 6 )  GO TO 50070                                     M28.2116
      IF ( ICURCH .EQ. 80 )  GO TO 50070                                M28.2117
      IF ( TEXCOL( ICURCH+1 ) .EQ. SEPCH( 6) )  GO TO 50010             M28.2118
50070 IF ( K1 .NE. 7 )  GO TO 50075                                     M28.2119
      IFLPNT = 1                                                        M28.2120
      GO TO 50021                                                       M28.2121
50075 IF ( K1 .GE. 1  .AND.  K1 .LE. 6 )  GO TO 50165                   M28.2122
C                                                                       M28.2123
C    :::  FIRST CHAR. OF NUMER.  OR  ALPHANUM.  :::                     M28.2124
50080 DO 50085  J = 1, 10                                               M28.2125
        IF ( CURCH .EQ. ALNRCL( J))  GO TO 50090                        M28.2126
50085   CONTINUE                                                        M28.2127
      ILGLPH = 1                                                        M28.2128
      GO TO 50010                                                       M28.2129
50090 ILGNUM = 1                                                        M28.2130
      GO TO 50010                                                       M28.2131
C                                                                       M28.2132
C    :::  CURRENT WORD IS COMPLETE  :::                                 M28.2133
50120 CURCH1 = TEXCOL( ICURCH)                                          M28.2134
      TEXCOL( ICURCH) = COMMA                                           M28.2135
      IF ( ILGNUM .LE. 20 )  GO TO 50125                                M28.2136
        IKILL1 = 10                                                     M28.2137
        IKILL2 = 50120                                                  M28.2138
        LSTAT( 16) = ICURCH - 1                                         M28.2139
        GO TO 50900                                                     M28.2140
50125 IF ( ILGLPH .LE. 6 )  GO TO 50130                                 M28.2141
        IKILL1 = 11                                                     M28.2142
        IKILL2 = 50125                                                  M28.2143
        LSTAT( 16) = ICURCH - 1                                         M28.2144
        GO TO 50900                                                     M28.2145
50130 IF ( ITMPBF .EQ. 0 )  GO TO 50150                                 M28.2146
C     : WORD IS SPREAD OVER MORE THAN ONE CARD                          M28.2147
      DO 50135  J = 1, 80                                               M28.2148
50135   BTMPBF( J) = TEXCOL( J)                                         M28.2149
      DO 50140  J = 1, ITMPBF                                           M28.2150
50140   TEXCOL( J) = ATMPBF( J)                                         M28.2151
      I1 = ILGNUM + ILGLPH + 1                                          M28.2152
      ITMPBF = ITMPBF + 1                                               M28.2153
      DO 50145  J = ITMPBF, I1                                          M28.2154
        K = J - ITMPBF + 1                                              M28.2155
50145   TEXCOL( J) = BTMPBF( K)                                         M28.2156
50150 IF ( ILGNUM .GT. 0 )  GO TO 50155                                 M28.2157
      IF ( ILGLPH .GT. 0 )  GO TO 50160                                 M28.2158
C                                                                       M28.2159
C    :::  STORING NUMERICAL WORD  :::                                   M28.2160
50155 IF ( IFLPNT .LE. 1 )  GO TO 50156                                 M28.2161
      IKILL1 = 13                                                       M28.2162
      IKILL2 = 50155                                                    M28.2163
      LSTAT( 16) = ICURCH - 1                                           M28.2164
50156 NFRFLD = 1                                                        M28.2165
      NRIGHT = 0                                                        M28.2166
      KOLBEG = IFSTCH                                                   M28.2167
      IARGEL = IARGEL + 1                                               M28.2168
      CALL FREFLD( ARGEL( IARGEL))                                      M28.2169
      ILST = ILST + 1                                                   M28.2170
      IEL( ILST) = -ILST -1                                             M28.2171
      EL( ILST)  = IARGEL                                               M28.2172
      ILGNUM = 0                                                        M28.2173
      IFLPNT = 0                                                        M28.2174
      GO TO 50170                                                       M28.2175
C                                                                       M28.2176
C    :::  STORING ALPHANUMERIC WORD  :::                                M28.2177
50160 NFRFLD = 1                                                        M28.2178
      NRIGHT = -1                                                       M28.2179
      KOLBEG = IFSTCH                                                   M28.2180
      CALL FREONE( D1)                                                  M29.1184
      ILST = ILST + 1                                                   M28.2182
      IEL( ILST) = ILST + 1                                             M28.2183
      EL( ILST)  = TEXTA6( 1)                                           M28.2184
      ILGLPH = 0                                                        M28.2185
      ILGCL = 0                                                         M28.2186
      IFLPNT = 0                                                        M28.2187
      GO TO 50170                                                       M28.2188
C                                                                       M28.2189
C     :::  STORING  OPERATOR  :::                                       M28.2190
50165 ISRCHL = 1                                                        M28.2191
      ILST = ILST + 1                                                   M28.2192
      IEL( ILST) = ILST + 1                                             M28.2193
      EL( ILST) = SEPCH( K1)                                            M28.2194
      GO TO 50010                                                       M28.2195
C                                                                       M28.2196
C    :::  RESTORE CURRENT LINE  :::                                     M28.2197
50170 IF ( ITMPBF .EQ. 0 )  GO TO 50180                                 M28.2198
      ITMPBF = 0                                                        M28.2199
      DO 50175  J = 1, 80                                               M28.2200
50175   TEXCOL( J) = BTMPBF( J)                                         M28.2201
50180 TEXCOL( ICURCH) = CURCH1                                          M28.2202
      IF ( ISRCHL ) 50185, 50065, 50010                                 M28.2203
C                                                                       M28.2204
C    :::  OUT : WHOLE EXPRESSION HAS BEEN INTERPRETED  :::              M28.2205
50185 IF ( ILST .NE. 0 )  GO TO 50190                                   M28.2206
      IKILL1 = 2                                                        M28.2207
      IKILL2 = 50185                                                    M28.2208
      GO TO 50900                                                       M28.2209
50190 IF ( ILST .LE. 100 )  GO TO 50195                                 M28.2210
      IKILL1 = 8                                                        M28.2211
      IKILL2 = 50190                                                    M28.2212
      GO TO 50900                                                       M28.2213
50195 J = IEL( ILST)                                                    M28.2214
      IEL( ILST) = 999                                                  M28.2215
      IF ( J .LT. 0 )  IEL( ILST) = -999                                M28.2216
      CSEPAR = CSPRCH                                                   M28.2217
C                                                                       M28.2218
C     ***  CHECK  VALIDITY  AND  PRE-PROCESS                            M28.2219
C     ***   THIS  FORTRAN  EXPRESSION                                   M28.2220
C     ***   * * * * * * * * * * * * * * * * *                           M28.2221
C                                                                       M28.2222
      IFST = 1                                                          M28.2223
      ILST1 = ILST                                                      M28.2224
C                                                                       M28.2226
C    :::  CHECK BRACKETS  :::                                           M28.2227
50200 I1 = 0                                                            M28.2228
      K1 = 0                                                            M28.2229
      I2 = 0                                                            M28.2230
      I = IFST                                                          M29.1185
50201 ALNM1 = EL(I)                                                     M29.1186
        IF ( ALNM1 .EQ. OPNAME( 1))  GO TO 50215                        M28.2233
        IF ( ALNM1 .NE. OPNAME( 7))  GO TO 50220                        M28.2234
        I1 = I1 - 1                                                     M28.2235
        IF ( I1 )  50205, 50210, 50220                                  M28.2236
50205   IKILL1 = 3                                                      M28.2237
        IKILL2 = 50205                                                  M28.2238
        GO TO 50900                                                     M28.2239
50210   IF ( K1 .EQ. 1 )  GO TO 50220                                   M28.2240
        K1 = 1                                                          M28.2241
        IF ( I2 .EQ. 0  .OR.  I .LT. ILST )  GO TO 50220                M28.2242
        EL( IFST) = BLANK                                               M28.2243
        EL( ILST) = BLANK                                               M28.2244
        IEL( IFST) = 0                                                  M28.2245
        IEL( ILST) = 0                                                  M28.2246
        IFST = IFST + 1                                                 M28.2247
        ILST = ILST - 1                                                 M28.2248
        ILST1 = ILST                                                    M28.2249
        J = IEL( ILST)                                                  M28.2250
        IEL( ILST) = 999                                                M28.2251
        IF ( J .LT. 0 )  IEL( ILST) = -999                              M28.2252
        IF ( IFST .LE. ILST )  GO TO 50200                              M28.2253
          IKILL1 = 2                                                    M28.2254
          IKILL2 = 50210                                                M28.2255
          GO TO 50900                                                   M28.2256
50215   IF ( I .EQ. IFST )  I2 = 1                                      M28.2257
        I1 = I1 + 1                                                     M28.2258
50220 I = I + 1                                                         M29.1187
      IF ( I .LE. ILST) GO TO 50201                                     M29.1188
      IF ( I1 .LE. 0 )  GO TO 50225                                     M28.2260
        IKILL1 = 1                                                      M28.2261
        IKILL2 = 50220                                                  M28.2262
        GO TO 50900                                                     M28.2263
C                                                                       M28.2264
C    :::  CHECK ALL PAIRS OF ADJACENT ELEMENTS  :::                     M28.2265
50225 I = IFST - 1                                                      M28.2266
50230 I = I + 1                                                         M28.2267
      IF ( I .GT. ILST )  GO TO 50485                                   M28.2268
50235 K1 = 0                                                            M28.2269
      ALNM1 = EL( I)                                                    M28.2270
      IF ( ALNM1 .EQ. BLANK )  GO TO 50230                              M28.2271
      DO 50240  J = 1, 18                                               M28.2272
        IF ( ALNM1 .EQ. OPNAME( J))  K1 = J                             M28.2273
50240   CONTINUE                                                        M28.2274
      K2 = 0                                                            M28.2275
      K = I                                                             M28.2276
50245 K = K + 1                                                         M28.2277
      IF ( K .GT. ILST )  GO TO 50255                                   M28.2278
      ALNM2 = EL( K)                                                    M28.2279
      IF ( ALNM2 .EQ. BLANK )  GO TO 50245                              M28.2280
      DO 50250  J = 1, 18                                               M28.2281
        IF ( ALNM2 .EQ. OPNAME( J))  K2 = J                             M28.2282
50250   CONTINUE                                                        M28.2283
50255 IF ( K1 .NE. 0 )  GO TO 50265                                     M28.2284
        K3 = 0                                                          M28.2285
        DO 50260  J = 12, 35                                            M38. 521
          IF ( ALNM1 .EQ. SUPFN( J))  K3 = J                            M28.2287
50260    CONTINUE                                                       M28.2288
50265 IF ( I .EQ. ILST )  GO TO 50470                                   M28.2289
      IF ( K1 .EQ. 0 )  GO TO 50380                                     M28.2290
      IF ( K2. LT. 2 )  GO TO 50270                                     M28.2291
        IF ( K1 .GE. 14 )  GO TO 50300                                  M28.2292
50270 IF ( K2 .LT. 2  .OR.  K2 .GT. 16 )  GO TO 50275                   M28.2293
        IF ( K1.EQ.3  .OR.  K1.EQ.5  .OR.  K1.EQ.7  )  GO TO 50275      M28.2294
        IF ( K1 .GE. 2  .AND.  K1 .LE. 13 )  GO TO 50300                M28.2295
50275 IF ( K2 .LT. 3  .OR.  K2 .GT. 16 )  GO TO 50280                   M28.2296
        IF ( K1 .EQ. 3  .OR.  K1 .EQ. 5 )  GO TO 50300                  M28.2297
50280 IF ( K2 .LT. 5  .OR.  K2 .GT. 16 )  GO TO 50285                   M28.2298
        IF ( K2 .EQ. 7 )  GO TO 50285                                   M28.2299
        IF ( K1 .EQ. 1 )  GO TO 50300                                   M28.2300
50285 IF ( K2 .GT. 2 )  GO TO 50290                                     M28.2301
        IF ( K1 .EQ. 7 )  GO TO 50300                                   M28.2302
50290 IF ( I .NE. IFST )  GO TO 50295                                   M28.2303
        IF ( K1 .GE. 5  .AND.  K1 .LE. 16 )  GO TO 50315                M28.2304
50295 GO TO 50320                                                       M28.2305
50300 IKILL1 = 5                                                        M28.2306
      IKILL2 = 50300                                                    M28.2307
      BUS2 = ALNM1                                                      M28.2308
      BUS3 = ALNM2                                                      M28.2309
      GO TO 50900                                                       M28.2310
50315 IKILL1 = 6                                                        M28.2311
      IKILL2 = 50315                                                    M28.2312
      BUS2 = ALNM1                                                      M28.2313
      GO TO 50900                                                       M28.2314
50320 IF ( K1 .NE. 1  .OR.  K2 .NE. 7 )  GO TO 50355                    M28.2315
        EL( I) = BLANK                                                  M28.2316
        EL( K) = BLANK                                                  M28.2317
        IF ( I .EQ. IFST )  GO TO 50330                                 M28.2318
50325   I = I - 1                                                       M28.2319
        IF ( EL( I) .EQ. BLANK )  GO TO 50325                           M28.2320
50330   IF ( K .EQ. ILST )  GO TO 50340                                 M28.2321
50335   K = K + 1                                                       M28.2322
        IF ( EL( K) .EQ. BLANK )  GO TO 50335                           M28.2323
50340   IF ( I .NE. IFST )  GO TO 50345                                 M28.2324
          IFST = K                                                      M28.2325
          GO TO 50200                                                   M28.2326
50345   IF ( K .NE. ILST )  GO TO 50350                                 M28.2327
          ILST = I                                                      M28.2328
          ILST1 = ILST                                                  M28.2329
          J = IEL( ILST)                                                M28.2330
          IEL( ILST) = 999                                              M28.2331
          IF ( J .LT. 0 )  IEL( ILST) = -999                            M28.2332
          IF ( IFST .LE. ILST )  GO TO 50200                            M28.2333
            IKILL1 = 2                                                  M28.2334
            IKILL2 = 50345                                              M28.2335
            GO TO 50900                                                 M28.2336
50350   J = IEL( I)                                                     M28.2337
        IEL( I) = K                                                     M28.2338
        IF ( J .LT. 0 )  IEL( I) = -K                                   M28.2339
        GO TO 50235                                                     M28.2340
50355 IF ( K2 .NE. 2 )  GO TO 50380                                     M28.2341
        IF ( K1 .NE. 3 )  GO TO 50360                                   M28.2342
          EL( K) = OPNAME( 4)                                           M28.2343
          GO TO 50365                                                   M28.2344
50360   IF ( K1 .NE. 5 )  GO TO 50380                                   M28.2345
          EL( K) = OPNAME( 6)                                           M28.2346
50365   EL( I) = BLANK                                                  M28.2347
        IF ( I .NE. IFST )  GO TO 50370                                 M28.2348
          IFST = K                                                      M28.2349
          GO TO 50375                                                   M28.2350
50370   I = I - 1                                                       M28.2351
        IF ( EL( I) .EQ. BLANK )  GO TO 50370                           M28.2352
        I1 = IEL( I)                                                    M28.2353
        IEL( I) = K                                                     M28.2354
        IF ( I1 .LT. 0 )  IEL( I) = -K                                  M28.2355
50375   I = K                                                           M28.2356
        GO TO 50235                                                     M28.2357
50380 IF ( K1 .EQ. 2 )  EL( I) = OPNAME( 4)                             M28.2358
      IF ( K1 .NE. 0 )  GO TO 50400                                     M38. 522
        IF ( K2 .NE. 0  .AND.  K2 .NE. 2 )  GO TO 50385                 M28.2360
          IKILL1 = 5                                                    M28.2361
          IKILL2 = 50380                                                M28.2362
          BUS2 = ALNM1                                                  M28.2363
          BUS3 = ALNM2                                                  M28.2364
          GO TO 50900                                                   M28.2365
50385   IF ( K3 .NE. 0  .OR.  K2 .NE. 1 )  GO TO 50390                  M38. 523
          IKILL1 = 5                                                    M28.2368
          IKILL2 = 50385                                                M28.2369
          BUS2 = ALNM1                                                  M28.2370
          BUS3 = ALNM2                                                  M28.2371
          GO TO 50900                                                   M28.2372
50390   IF ( K3 .EQ. 0  .OR.  K2 .EQ. 1 )  GO TO 50400                  M38. 524
          IKILL1 = 7                                                    M28.2374
          IKILL2 = 50390                                                M28.2375
          BUS2 = ALNM1                                                  M28.2376
          GO TO 50900                                                   M28.2377
C    :::  INSERT  '+'  OR  '.OR.'  WHERE NEEDED  :::                    M28.2380
50400 I1 = 0                                                            M28.2381
      I2 = 0                                                            M28.2382
      I3 = K                                                            M28.2383
      I4 = 0                                                            M28.2384
      IF ( K1 .NE. 0  .OR.  I .NE. IFST )  GO TO 50405                  M28.2385
      IF ( K3 .EQ. 0 )  GO TO 50445                                     M28.2386
        I1 = 1                                                          M28.2387
        GO TO 50435                                                     M28.2388
50405 IF ( K1 .NE. 1 )  GO TO 50230                                     M28.2389
      IF ( I .NE. IFST )  GO TO 50410                                   M28.2390
        I1 = 1                                                          M28.2391
      I4 = 1                                                            M28.2392
        K = I                                                           M28.2393
        GO TO 50435                                                     M28.2394
50410 IF ( K2 .GE. 17 )  GO TO 50230                                    M28.2395
      IF ( K2 .EQ. 2  .OR.  K2 .EQ. 3 )  GO TO 50230                    M28.2396
      IF ( K2 .EQ. 0 )  GO TO 50420                                     M28.2397
        I1 = 1                                                          M28.2398
        GO TO 50435                                                     M28.2399
50420 K4 = 0                                                            M28.2400
      DO 50425  J = 12, 35                                              M38. 525
        IF ( ALNM2 .EQ. SUPFN( J))  K4 = J                              M28.2402
50425  CONTINUE                                                         M28.2403
      IF ( K4 .NE. 0 )  GO TO 50435                                     M28.2404
50430 K = K + 1                                                         M28.2405
      IF ( EL( K) .EQ. BLANK )  GO TO 50430                             M28.2406
        GO TO 50445                                                     M28.2407
50435 K = K + 1                                                         M28.2408
      IF ( K .LE. ILST )  GO TO 50440                                   M28.2409
        IF ( K1 .EQ. 0  .AND.  K3 .NE. 0 )  GO TO 50455                 M28.2410
        IKILL1 = 1                                                      M28.2411
        IKILL2 = 50435                                                  M28.2412
        GO TO 50900                                                     M28.2413
50440 IF ( EL( K) .EQ. OPNAME( 1))  I1 = I1 + 1                         M28.2414
      IF ( I1 .EQ. 0 )  GO TO 50445                                     M28.2415
      IF ( EL( K) .EQ. OPNAME( 7))  I1 = I1 - 1                         M28.2416
      GO TO 50435                                                       M28.2417
50445 IF ( EL(K).NE.OPNAME(3) .AND. EL(K).NE.OPNAME(4)                  M28.2418
     1  .AND. EL(K).NE.OPNAME(5) .AND. EL(K).NE.OPNAME(6) )             M28.2419
     2   GO TO 50450                                                    M28.2420
        I2 = 1                                                          M28.2421
50450 IF ( I .EQ. IFST )  GO TO 50455                                   M28.2422
        J = IEL( I)                                                     M28.2423
        IEL( I) = IFREE                                                 M28.2424
        IF ( J .LT. 0 )  IEL( I) = -IFREE                               M28.2425
        IEL( IFREE) = I3                                                M28.2426
        GO TO 50460                                                     M28.2427
50455 IFST = IFREE                                                      M28.2428
      IEL( IFREE) = I                                                   M28.2429
50460 IF ( IFREE .GT. ILST1 )  GO TO 50465                              M28.2430
        IKILL1 = 8                                                      M28.2431
        IKILL2 = 50460                                                  M28.2432
        GO TO 50900                                                     M28.2433
50465 EL( IFREE) = OPNAME( 17)                                          M28.2434
      IF ( I2 .EQ. 1 )  EL( IFREE) = OPNAME( 3)                         M28.2435
      IFREE = IFREE - 1                                                 M28.2436
      IF ( I4  .EQ.  1 )  GO TO 50235                                   M28.2437
      GO TO 50230                                                       M28.2438
50470 IF ( K1 .EQ. 0  .OR.  K1 .EQ. 7 )  GO TO 50475                    M28.2439
        IKILL1 = 4                                                      M28.2440
        IKILL2 = 50470                                                  M28.2441
        BUS2 = ALNM1                                                    M28.2442
        GO TO 50900                                                     M28.2443
50475 IF ( K1 .NE. 0 )  GO TO 50485                                     M28.2444
      IF ( K3 .EQ. 0 )  GO TO 50480                                     M28.2445
        IKILL1 = 7                                                      M28.2446
        IKILL2 = 50475                                                  M28.2447
        BUS2 = ALNM1                                                    M28.2448
        GO TO 50900                                                     M28.2449
50480 IF ( I .NE. IFST )  GO TO 50485                                   M28.2450
        I2 = 0                                                          M28.2451
        GO TO 50455                                                     M28.2452
C                                                                       M28.2453
C    :::  CHANGE  ".NN.EXPRESSION"  INTO  ".NN.+EXPRESSION)"            M28.2454
50485 I = IFST                                                          M28.2455
50490 I = IEL( I)                                                       M28.2456
      IF ( I .LT. 0 )  I = -I                                           M28.2457
      IF ( I .EQ. 999 )  GO TO 50535                                    M28.2458
      K1 = 0                                                            M28.2459
      ALNM1 = EL( I)                                                    M28.2460
      DO 50495  J = 8, 13                                               M28.2461
        IF ( ALNM1 .EQ. OPNAME( J))  K1 = J                             M28.2462
50495   CONTINUE                                                        M28.2463
      IF ( K1 .EQ. 0 )  GO TO 50490                                     M28.2464
      K = I                                                             M28.2465
      I1 = 0                                                            M28.2466
      I3 = 0                                                            M28.2467
50500 I2 = K                                                            M28.2468
      K = IEL( K)                                                       M28.2469
      IF ( K .LT. 0 )  K = -K                                           M28.2470
      IF ( K .EQ. 999 )  GO TO 50515                                    M28.2471
      K2 = 0                                                            M28.2472
      ALNM2 = EL( K)                                                    M28.2473
      DO 50505  J = 1, 7                                                M28.2474
        IF ( ALNM2 .EQ. OPNAME( J))  K2 = J                             M28.2475
50505   CONTINUE                                                        M28.2476
      IF ( I3 .NE. 0 )  GO TO 50510                                     M28.2477
        I3 = 1                                                          M28.2478
        IF ( K2 .GT. 2 )  GO TO 50510                                   M28.2479
          EL( IFREE) = OPNAME( 17)                                      M28.2480
          GO TO 50525                                                   M28.2481
50510 IF ( K2 .EQ. 0 )  GO TO 50500                                     M28.2482
      IF ( K2 .EQ. 1 )  I1 = I1 + 1                                     M28.2483
      IF ( I1 .EQ. 0 )  GO TO 50520                                     M28.2484
      IF ( K2 .EQ. 7 )  I1 = I1 - 1                                     M28.2485
      GO TO 50500                                                       M28.2486
50515 ILST = IFREE                                                      M28.2487
50520 EL( IFREE) = OPNAME( 7)                                           M28.2488
50525 J = IEL( I2)                                                      M28.2489
      IEL( I2) = IFREE                                                  M28.2490
      IF ( J .LT. 0 )  IEL( I2) = -IFREE                                M28.2491
      IEL( IFREE) = K                                                   M28.2492
      IF ( IFREE .GT. ILST1 )  GO TO 50530                              M28.2493
        IKILL1 = 8                                                      M28.2494
        IKILL2 = 50525                                                  M28.2495
        GO TO 50900                                                     M28.2496
50530 IFREE = IFREE - 1                                                 M28.2497
      IF ( I2 .EQ. I )  GO TO 50500                                     M28.2498
      GO TO 50490                                                       M28.2499
C                                                                       M28.2500
C    :::  CHANGE  "EXPRESSION.NN."  INTO  "(EXPRESSION.NN."             M28.2501
50535 J = ILST                                                          M28.2502
      GO TO 50550                                                       M28.2503
50540 DO 50545  J = 1, 100                                              M28.2504
        IF ( IEL( J) .EQ. I )  GO TO 50550                              M28.2505
        IF ( IEL( J) .EQ. -I)  GO TO 50550                              M28.2506
50545   CONTINUE                                                        M28.2507
50550 IF ( J .EQ. IFST )  GO TO 50600                                   M28.2508
      I = J                                                             M28.2509
      K1 = 0                                                            M28.2510
      ALNM1 = EL( I)                                                    M28.2511
      DO 50555  J = 8, 13                                               M28.2512
        IF ( ALNM1 .EQ. OPNAME( J))  K1 = J                             M28.2513
50555   CONTINUE                                                        M28.2514
      IF ( K1 .EQ. 0 )  GO TO 50540                                     M28.2515
      I2 = I                                                            M28.2516
      I1 = 0                                                            M28.2517
50560 DO 50565  K = 1, 100                                              M28.2518
        IF ( IEL( K) .EQ. I2 )  GO TO 50570                             M28.2519
        IF ( IEL( K) .EQ. -I2)  GO TO 50570                             M28.2520
50565   CONTINUE                                                        M28.2521
50570 K2 = 0                                                            M28.2522
      ALNM2 = EL( K)                                                    M28.2523
      I2 = K                                                            M28.2524
      DO 50575  J = 1, 7                                                M28.2525
        IF ( ALNM2 .EQ. OPNAME( J))  K2 = J                             M28.2526
50575   CONTINUE                                                        M28.2527
      IF ( K2 .NE. 0 )  GO TO 50580                                     M28.2528
        IF ( K .NE. IFST )  GO TO 50560                                 M28.2529
        IFST = IFREE - 2                                                M28.2530
        GO TO 50590                                                     M28.2531
50580 IF ( K2 .EQ. 7 )  I1 = I1 + 1                                     M28.2532
      IF ( I1 .EQ. 0 )  GO TO 50585                                     M28.2533
      IF ( K2 .EQ. 1 )  I1 = I1 - 1                                     M28.2534
      GO TO 50560                                                       M28.2535
50585 I2 = IEL(K)                                                       M28.2536
      IEL( K) = IFREE - 2                                               M28.2537
      IF (K2 .EQ. 1) GO TO 50590                                        M29.1189
      IEL (K) = IFREE - 1                                               M29.1190
      GO TO 50592                                                       M29.1191
50590 IEL( IFREE -2 ) = IFREE - 1                                       M28.2538
      EL(IFREE-2) = OPNAME(3)                                           M29.1192
50592 IEL( IFREE -1 ) = IFREE                                           M29.1193
      IEL( IFREE    ) = I2                                              M28.2540
      EL( IFREE -1 ) = OPNAME( 1)                                       M28.2546
      EL( IFREE    ) = OPNAME( 17)                                      M28.2547
      IFREE = IFREE - 3                                                 M28.2548
      IF (K2 .GT. 1) IFREE = IFREE + 1                                  M29.1194
      IF (IFREE .GE. ILST1) GO TO 50540                                 M29.1195
        IKILL1 = 8                                                      M29.1196
        IKILL2 = 50590                                                  M29.1197
        GO TO 50900                                                     M29.1198
C                                                                       M28.2550
C    :::  CHECK HOMOLOGY OF SUCCESSIVE OPERATORS  :::                   M28.2551
50600 I = IFST                                                          M28.2552
      GO TO 50610                                                       M28.2553
50605 I = IEL( I)                                                       M28.2554
      IF ( I .LT. 0 )  I = -I                                           M28.2555
      IF ( I .EQ. 999 )  GO TO 50645                                    M28.2556
50610 K1 = 0                                                            M28.2557
      ALNM1 = EL( I)                                                    M28.2558
      DO 50615  J = 3, 6                                                M28.2559
        IF ( ALNM1 .EQ. OPNAME( J))  K1 = 2                             M28.2560
50615   CONTINUE                                                        M28.2561
      DO 50620  J = 14, 18                                              M28.2562
        IF ( ALNM1 .EQ. OPNAME( J))  K1 = 1                             M28.2563
50620   CONTINUE                                                        M28.2564
      IF ( K1 .EQ. 0 )  GO TO 50605                                     M28.2565
      K = I                                                             M28.2566
      I1 = 0                                                            M28.2567
50625 K = IEL( K)                                                       M28.2568
      IF ( K .LT. 0 )  K = -K                                           M28.2569
      IF ( K .EQ. 999 )  GO TO 50605                                    M28.2570
      K2 = 0                                                            M28.2571
      ALNM2 = EL( K)                                                    M28.2572
      DO 50630  J = 1, 18                                               M28.2573
        IF ( ALNM2 .EQ. OPNAME( J))  K2 = J                             M28.2574
50630   CONTINUE                                                        M28.2575
      IF ( K2 .EQ. 0 )  GO TO 50625                                     M28.2576
      IF ( K2 .EQ. 1 )  I1 = I1 + 1                                     M28.2577
      IF ( I1 .EQ. 0 )  GO TO 50635                                     M28.2578
      IF ( K2 .EQ. 7 )  I1 = I1 - 1                                     M28.2579
      GO TO 50625                                                       M28.2580
50635 IF ( K1 .NE. 1 )  GO TO 50640                                     M28.2581
        IF ( K2 .LT. 3  .OR.  K2 .GT. 6 )  GO TO 50605                  M28.2582
        IKILL1 = 9                                                      M28.2583
        IKILL2 = 50635                                                  M28.2584
        BUS2 = ALNM1                                                    M28.2585
        BUS3 = ALNM2                                                    M28.2586
        GO TO 50900                                                     M28.2587
50640 IF ( K2 .LT. 14 )  GO TO 50605                                    M28.2588
        IKILL1 = 9                                                      M28.2589
        IKILL2 = 50640                                                  M28.2590
        BUS2 = ALNM1                                                    M28.2591
        BUS3 = ALNM2                                                    M28.2592
        GO TO 50900                                                     M28.2593
C                                                                       M28.2594
C    :::  STORE 'EL' INTO IVARB, PARSUP  :::                            M39. 659
50645 NDX1 = KJSUP + NSUP                                               M28.2596
      INSUP( NDX1) = KARG + 3                                           M39. 660
      I = IFST                                                          M28.2598
      GO TO 50655                                                       M28.2599
50650 IF ( K .EQ. 999 )  GO TO 50685                                    M28.2600
      I = K                                                             M28.2601
50655 K = IEL( I)                                                       M28.2602
      IF ( K .GT. 0 )  GO TO 50660                                      M28.2603
C     : NUMERICAL ARGUMENT                                              M28.2604
      K = -K                                                            M28.2605
      IVARB(KARG+1) = -1                                                M39. 661
      KPAR = KPAR + 1                                                   M28.2608
      IVARB(KARG+3) = KPAR                                              M39. 662
      J = EL( I)                                                        M28.2611
      NDX1 = KPRSUP + KPAR                                              M28.2612
      PARSUP( NDX1) = ARGEL( J)                                         M28.2613
      GO TO 50650                                                       M28.2614
50660 ALNM1 = EL( I)                                                    M28.2615
      DO 50665  J = 1, 18                                               M28.2616
        IF ( ALNM1 .EQ. OPNAME( J))  GO TO 50675                        M28.2617
50665   CONTINUE                                                        M28.2618
      DO 50670  J = 12, 35                                              M38. 527
        IF ( ALNM1 .EQ. SUPFN( J))  GO TO 50680                         M28.2620
50670   CONTINUE                                                        M28.2621
C     : TACS VARIABLE                                                   M28.2622
      IVARB(KARG+1) = 1                                                 M39. 663
      N23 = 0                                                           M38. 529
      CALL NAMEA6( ALNM1, N23 )                                         M38. 530
      IVARB(KARG+3) = N23                                               M39. 664
      GO TO 50650                                                       M28.2627
C     : OPERATOR                                                        M28.2628
50675 KARG = KARG + 3                                                   M39. 665
      IVARB(KARG+2) = J                                                 M39. 666
      IVARB(KARG+1) = 0                                                 M39. 667
      IVARB(KARG+3) = 0                                                 M39. 668
      GO TO 50650                                                       M28.2636
C     : FUNCTION                                                        M28.2637
50680 IVARB(KARG+1) = 2                                                 M39. 669
      IVARB(KARG+3) = J - 1                                             M39. 670
      GO TO 50650                                                       M28.2642
C     : EXIT                                                            M28.2643
50685 NDX1 = KKSUP + NSUP                                               M28.2644
      INSUP( NDX1) = -KARG                                              M39. 671
      NDX1 = KJSUP + NSUP                                               M28.2646
      IF ( KARG - INSUP( NDX1) .LT. 150 )  GO TO 12599                  M39. 672
      IKILL1 = 8                                                        M28.2648
      IKILL2 = 50685                                                    M28.2649
C                                                                       M28.2651
C     ***  KILL  CODES  FOR  FORTRAN  EXPRESSION  ***                   M28.2652
C     ***  * * * * * * * * * * * * * * * * * * *  ***                   M28.2653
50900 KILL = 218                                                        M28.2654
      LSTAT( 19) = IKILL2                                               M28.2655
      LSTAT( 17) = IKILL1                                               M28.2656
      BUS1 = ALNODE                                                     M28.2657
      GO TO 9000                                                        M28.2658
C                                                                       M28.2659
C     ***  OLD  FIXED-FORMAT  PSEUDO-FORTRAN  EXPRESSION  ***           M28.2660
C     ***  * * * * * * * * * * * * * * * * * * * * * * *  ***           M28.2661
12517 IF ( NOUTPR .EQ. 0 )  WRITE ( KUNIT6, 9304 )                      M39. 673
 9304 FORMAT ( 28H+TACS SUPPLEMENTAL VARIABLE  )                        M39. 674
      DECODE (80, 18001, ABUFF(1)) PRX, ALPH(1), DUMJ(1),               M37.1475
     1     DUMJ(6), ALPH(2), DUMJ(2), DUMJ(7), ALPH(3),                 M37.1476
     2     DUMJ(3), PRU, ALPH(4), DUMJ(4), DUMJ(9),                     M37.1477
     4     ALPH(5), DUMJ(5), DUMJ(10)                                   M37.1478
18001 FORMAT (10X,E10.0,2(A1,A5,A6),A1,A5,E6.0,2(A1,A5,A6))             M14. 685
      NDX1 = KJSUP  +  NSUP                                             M22.2011
      INSUP(NDX1) = KARG + 3                                            M39. 675
      IF ( PRX .EQ. 0.0 )  GO TO 126                                    M14. 687
      KARG = KARG + 3                                                   M39. 676
      IVARB(KARG+1) = -1                                                M39. 677
      IVARB(KARG+2) = 0                                                 M39. 678
      KPAR = KPAR + 1                                                   M14. 691
      IVARB(KARG+3) = KPAR                                              M39. 679
      NDX1 = KPRSUP +  KPAR                                             M22.2019
      PARSUP(NDX1) = PRX                                                M22.2020
  126 DO 125 I=1,5                                                      M14. 694
      IF ( ALPH( I) .NE. BLANK )  GO TO 12601                           M14. 695
C     PREVIOUS RECORD FAILS OCCASIONALLY FOR BURROUGHS EMTP             M32. 923
C     (SEE VOL. XI  EMTP MEMO,  PAGE DTTM-4,  24 DEC 1981).             M32. 924
C     YET FOLLOWING BURROUGHS PATCH PRODUCES FATAL VAX ERROR:           M32. 925
C     IF ( .NOT. (ALPH(I) .IS. BLANK) )   GO TO 12601                   M32. 926
      IF ( I .EQ. 3 )  GO TO 12602                                      M14. 696
      IF ( DUMJ(I+5) .EQ. BLANK )  GO TO 125                            M37.1479
      IF ( I .NE. 1 )  GO TO 12603                                      M14. 698
      ALPH( 1) = SPLUS                                                  M14. 699
      GO TO 12700                                                       M14. 700
12603 LSTAT( 14) = 1                                                    M14. 701
      BUS1 = DUMJ(I+5)                                                  M37.1480
      GO TO 12610                                                       M14. 703
12602 IF ( PRU .EQ. 0.0 )  GO TO 125                                    M14. 704
      LSTAT( 14) = 2                                                    M14. 705
      FLSTAT( 16) = PRU                                                 M14. 706
      GO TO 12610                                                       M14. 707
12601 IF ( I .NE. 3  .AND.  DUMJ(I+5) .NE. BLANK )  GO TO 12700         M37.1481
      IF ( I .EQ. 3  .AND.  PRU .NE. 0.0 )  GO TO 12700                 M14. 709
      LSTAT( 14) = 3                                                    M14. 710
      BUS2 = ALPH( I)                                                   M14. 711
12610 KILL = 139                                                        M14. 712
      LSTAT(19) = 12610                                                 M32. 927
      BUS3 = ALNODE                                                     M14. 714
      GO TO 9000                                                        M14. 715
12700 KARG = KARG + 3                                                   M39. 680
      DO 12701 J=1,6                                                    M14. 717
      IF ( ALPH(I) .EQ. SUPOP(J) )  GO TO 12702                         M14. 718
12701 CONTINUE                                                          M14. 719
      KILL = 130                                                        M14. 720
      LSTAT( 19) = 12702                                                M14. 721
      BUS1 = ALPH( I)                                                   M14. 722
      BUS2 = ALNODE                                                     M14. 723
      GO TO 9000                                                        M14. 724
12702 IVARB(KARG+1) = J                                                 M39. 681
      DO 12703  J = 1, 35                                               M38. 535
      IF ( DUMJ(I) .EQ. SUPFN(J) )  GO TO 12704                         M37.1482
12703 CONTINUE                                                          M14. 728
      KILL = 132                                                        M14. 729
      LSTAT( 19) = 12704                                                M14. 730
      BUS1 = DUMJ(I)                                                    M37.1483
      BUS2 = ALNODE                                                     M14. 732
      GO TO 9000                                                        M14. 733
12704 IVARB(KARG+2) = J - 1                                             M39. 682
      IF ( I .EQ. 3 )  GO TO 12710                                      M14. 735
      N23 = 0                                                           M38. 537
      TEXT2 = DUMJ(I+5)                                                 M38. 538
      CALL NAMEA6( TEXT2, N23 )                                         M38. 539
      IVARB(KARG+3) = N23                                               M39. 683
      GO TO 125                                                         M14. 737
12710 IVARB(KARG+1) = - IVARB(KARG+1)                                   M39. 684
      KPAR = KPAR + 1                                                   M14. 739
      IVARB(KARG+3) = KPAR                                              M39. 685
      NDX1 = KPRSUP +  KPAR                                             M22.2042
      PARSUP(NDX1) = PRU                                                M22.2043
  125 CONTINUE                                                          M14. 742
      NDX1 = KKSUP  +  NSUP                                             M22.2044
      INSUP(NDX1) = KARG                                                M39. 686
12599 CONTINUE                                                          M28.2664
      IF ( KARG - KIVARB .LT. LSTAT(66))  GO TO 12721                   M39. 687
      KILL = 122                                                        M14. 745
      LSTAT( 19) = 12721                                                M14. 746
      LSTAT( 16) = LSTAT(66)                                            M39. 688
      LSTAT( 17) = 6                                                    M14. 748
      GO TO 9000                                                        M14. 749
12721 IF ( KPAR .LE. LSTAT(67) )  GO TO 12722                           M39. 689
      KILL = 122                                                        M14. 751
      LSTAT( 19) = 12722                                                M14. 752
      LSTAT( 16) = LSTAT(67)                                            M39. 690
      LSTAT( 17) = 7                                                    M14. 754
      GO TO 9000                                                        M14. 755
12722 NDX1 = KJSUP  +  NSUP                                             M22.2046
      IF ( KARG .GE. INSUP(NDX1) )  GO TO 9000                          M39. 691
      KILL = 133                                                        M14. 757
      BUS1 = ALNODE                                                     M14. 758
      LSTAT( 19) = 104                                                  M14. 759
      GO TO 9000                                                        M14. 760
10700 IF ( NOUTPR  .EQ.  0 )                                            M20. 779
     1 WRITE (KUNIT6, 19305)   M                                        M39. 692
19305 FORMAT ( 31H+TACS SUPPLEMENTAL DEVICE TYPE , I2 )                 M39. 693
      DECODE (80, 18002, ABUFF(1)) ALPH(1), DUMJ(1),                    M37.1490
     1  ALPH(2), DUMJ(2), ALPH(3), DUMJ(3), ALPH(4), DUMJ(4),           M37.1491
     2  ALPH(5), DUMJ(5),  (DUM(I), I=1, 3), ALNM1, ALNM2               M37.1492
18002 FORMAT ( 10X, 5(A1,A6,1X), 3E6.0, 2A6)                            M14. 765
      IF ( M .GE. 50 )  GO TO 10701                                     M39. 694
      KILL = 134                                                        M14. 767
      LSTAT( 19) = 10701                                                M14. 768
      BUS1 = ALNODE                                                     M14. 769
      LSTAT( 14) = M                                                    M14. 770
      GO TO 9000                                                        M14. 771
10701 KARG = KARG + 4                                                   M39. 695
      IF ( (KARG - KIVARB + 4) .LE. LSTAT(66) )  GO TO 10703            M39. 696
      KILL = 122                                                        M14. 774
      LSTAT( 16) = LSTAT(66)                                            M39. 697
      LSTAT( 17) = 6                                                    M39. 698
      LSTAT( 19) = 10703                                                M14. 777
      GO TO 9000                                                        M14. 778
10703 NDX1 = KJSUP  +  NSUP                                             M22.2052
      INSUP(NDX1) = - KARG                                              M39. 699
      NDX1 = KKSUP  +  NSUP                                             M22.2054
      INSUP( NDX1) = M                                                  M39. 700
      IVARB( KARG) = KPRSUP + KPAR + 1                                  M39. 701
      IVARB(KARG+2) = NSUDV                                             M39. 702
      N1 = 0                                                            M14. 782
      DO 1071 I=1,5                                                     M14. 783
      IF ( M .EQ. 61  .OR.  M .EQ. 63 )  GO TO 1107                     M38. 550
      IF ( DUMJ(I) .EQ. BLANK )  GO TO 1071                             M37.1495
      N1 = N1 + 1                                                       M14. 785
 1107 IF ( NSUDV .GT. NSU )  GO TO 1072                                 M38. 551
      KILL = 122                                                        M14. 788
      LSTAT( 16) = LSTAT(63)                                            M39. 703
      LSTAT( 17) = 3                                                    M37.1498
      LSTAT( 19) = 1072                                                 M14. 791
      GO TO 9000                                                        M14. 792
 1072 NDX1 = KALKSU +  NSUDV                                            M37.1499
      N23 = 0                                                           M38. 553
      TEXT2 = DUMJ(I)                                                   M38. 554
      CALL NAMEA6( TEXT2, N23 )                                         M38. 555
      KSUS(NDX1) = N23                                                  M38. 556
      NDX1 = KKSUS  +  NSUDV                                            M37.1501
      KSUS(NDX1) = 1                                                    M37.1502
      IF ( ALPH(I) .EQ. SMINUS )  KSUS(NDX1) = -1                       M37.1503
      IF ( ALPH(I) .EQ. SMULTP )  KSUS(NDX1) = 9                        M39. 704
      NSUDV = NSUDV - 1                                                 M38. 557
 1071 CONTINUE                                                          M14. 796
      IF ( N1 .GT. 0 )  GO TO 1073                                      M14. 797
      IF ( M .EQ. 61 .OR. M .EQ. 63 )  GO TO 1073                       M28.2669
      KILL = 135                                                        M14. 798
      BUS1 = ALNODE                                                     M14. 799
      LSTAT( 19) = 1073                                                 M14. 800
      GO TO 9000                                                        M14. 801
 1073 IVARB(KARG+1) = NSUDV + 1                                         M39. 705
      MPAR = KPRSUP + KPAR                                              M39. 706
      PARSUP(MPAR+1) = DUM( 1)                                          M39. 707
      PARSUP(MPAR+2) = DUM( 2)                                          M39. 708
      PARSUP(MPAR+3) = DUM( 3)                                          M39. 709
      N23 = -9999                                                       M38. 561
      IF ( ALNM1  .EQ.  TEXT4 ) GO TO 1177                              M38. 562
      N23 = 0                                                           M38. 563
      CALL NAMEA6( ALNM1, N23)                                          M38. 564
 1177 IVARB(KARG+3) = N23                                               M39. 710
 1188 N23 = 0                                                           M39. 711
      CALL NAMEA6( ALNM2, N23)                                          M38. 568
      IVARB(KARG+4) = N23                                               M39. 712
      IF ( M .GT. 67 ) GO TO 6799                                       M39. 713
      MOON = M - 49                                                     M39. 714
      GO TO ( 650, 651, 651, 653, 654, 655, 655, 655, 658, 659, 660,    M39. 715
     1             661, 662, 663, 663, 6511, 666, 667 ), MOON           M39. 716
  650 IVARB(KARG+3) = 0                                                 M39. 717
      KPAR = KPAR + 4                                                   M39. 718
      PARSUP(MPAR+4) = DUM(2)                                           M39. 719
      IF ( DUM(2) .EQ. 0.0 ) PARSUP(MPAR+4) = 0.5                       M39. 720
      GO TO 3636                                                        M39. 721
  651 IF ( DUM(3) .EQ. 0.0 ) PARSUP(MPAR+3) = 2.0                       M39. 722
 6511 KPAR = KPAR + 3                                                   M39. 723
 3636 KARG = KARG + 1                                                   M39. 724
      IF ( KPAR  .LE.  LSTAT(67) )  GO TO 9000                          M39. 725
      KILL = 122                                                        M39. 726
      LSTAT(16) = LSTAT(67)                                             M39. 727
      LSTAT(17) = 7                                                     M39. 728
      LSTAT(19) = 3636                                                  M39. 729
      GO TO 9000                                                        M39. 730
  653 IF ( ALNM1 .EQ. BLANK ) PARSUP(MPAR+3) = PARSUP(MPAR+2)           M39. 731
      D9 = PARSUP(MPAR+3) / DELTAT  -  FLZERO * 10.                     M39. 732
      I = 0                                                             M28.2676
10706 I = I + 1                                                         M28.2677
      D10 = I                                                           M28.2678
      IF ( D9 .GT. D10 )  GO TO 10706                                   M28.2679
      PARSUP(MPAR+3) = D10                                              M39. 733
      KPAR = KPAR + 5                                                   M39. 734
      PARSUP(MPAR+1) = KPAR                                             M39. 735
      PARSUP(MPAR+4) = 0.0                                              M39. 736
      IVARB(KARG+4) = - KPAR                                            M39. 737
      N6 = KPAR + I - 1                                                 M39. 738
C     WSM PATCH FOR DAN GOLDSWORTH TYPE-53 HISTORY.  27 MAR 83          M35. 677
10707 IF ( ALNM2 .EQ. TEXT1  )   GO TO 1707                             M39. 739
      PARSUP(MPAR+4) = DUM(1)                                           M39. 740
      DUM(1) = 0.0                                                      M39. 741
 1707 IF ( IPRSUP .GE. 1 )  WRITE (LUNIT6, 8841)  ALNM2                 M39. 742
 8841 FORMAT ( 30H TEST DELAY LINE I.C., VALUE =,  A6  )                M35. 680
      DO 10717  I1 = KPAR, N6                                           M39. 743
      NDX6 = KPRSUP + I1                                                M39. 744
10717 PARSUP(NDX6) = DUM(1)                                             M39. 745
      KPAR = N6                                                         M39. 746
      GO TO 3636                                                        M39. 747
  654 IF (PARSUP(MPAR+1) .EQ. 0.0) PARSUP(MPAR+1) = -9999.              M39. 748
      IF (PARSUP(MPAR+3) .EQ. 0.0) PARSUP(MPAR+3) = -9999.              M39. 749
      GO TO 6511                                                        M39. 750
  655 IVARB(KARG+3) = - KPAR - 4                                        M39. 751
      KPAR = KPAR + 3                                                   M39. 752
C     READ INPUT CARD USING CIMAGE                                      M22.2102
10711 CALL CIMAGE                                                       M14. 842
      IF ( KOLBEG  .GT.  0 )   GO TO 6544                               M22.2103
      DECODE ( 80, 10712, ABUFF( 1))  PRX, PRU                          M14. 843
10712 FORMAT ( 2E16.0)                                                  M14. 844
      GO TO 6547                                                        M22.2104
 6544 NFRFLD = 1                                                        M22.2105
      CALL FREFLD ( VOLTBC(1) )                                         M31. 364
      PRX = VOLTBC(1)                                                   M31. 365
      IF ( PRX  .EQ.  9999. )   GO TO 10713                             M33. 124
      CALL FREFLD ( VOLTBC(1) )                                         M31. 366
      PRU = VOLTBC(1)                                                   M31. 367
 6547 IF ( PRX  .EQ.  9999. )   GO TO 10713                             M22.2111
      IF ( NOUTPR  .EQ.  0 )                                            M20. 787
     1 WRITE (KUNIT6, 10715)  PRX, PRU                                  M35. 684
10715 FORMAT ( 18H+DATA VALUES.             ,  2E13.4)                  M14. 847
      KPAR = KPAR + 1                                                   M39. 753
      NDX1 = KPRSUP +  KPAR                                             M39. 754
      PARSUP(NDX1) = PRX                                                M39. 755
      IF ( M .EQ. 55  .OR.  M .EQ. 57 )  GO TO 10711                    M15. 449
      KPAR = KPAR + 1                                                   M39. 756
      NDX1 = KPRSUP +  KPAR                                             M39. 757
      PARSUP(NDX1) = PRU                                                M39. 758
      GO TO 10711                                                       M14. 853
10713 IVARB(KARG+4) = - KPAR                                            M39. 759
      IF ( NOUTPR  .EQ.  0 )                                            M20. 789
     1 WRITE (KUNIT6, 10716)                                            M35. 685
10716 FORMAT ( 41H+END OF DATA VALUES FOR LAST-READ DEVICE.   )         M14. 856
      IF ( M .EQ. 57 )  PARSUP(MPAR+2) = 0.                             M39. 760
      IF ( M .EQ. 57 )  PARSUP(MPAR+3) = 0.                             M39. 761
      GO TO  3636                                                       M39. 762
  658 IF ( PARSUP(MPAR+1) .EQ. 0.0 )  PARSUP(MPAR+1) = 1.0              M39. 763
      PARSUP(MPAR+3) = 2.0 / DELTAT * PARSUP(MPAR+3) / PARSUP(MPAR+1)   M39. 764
      PARSUP(MPAR+1) = PARSUP(MPAR+2)  /  PARSUP(MPAR+1)                M39. 765
      PARSUP(MPAR+2) = PARSUP(MPAR+1)  +  PARSUP(MPAR+3)                M39. 766
      PARSUP(MPAR+3) = PARSUP(MPAR+1)  -  PARSUP(MPAR+3)                M39. 767
      IF (PARSUP(MPAR+2) .NE. 0.0 .OR. ALNM1 .EQ. TEXT4) GO TO 6511     M39. 768
 5657 KILL = 214                                                        M38. 579
      BUS1 = ALNODE                                                     M28.2710
      LSTAT( 19) = 10714                                                M28.2711
      GO TO 9000                                                        M28.2712
  659 IF ( PARSUP(MPAR+1) .EQ. 0.0 )  PARSUP(MPAR+1) = 1.0              M39. 769
      PARSUP(MPAR+1) = PARSUP(MPAR+1)  /  DELTAT                        M39. 770
      GO TO 6511                                                        M39. 771
  660 IF ( ALNM1 .EQ. BLANK )  GO TO 10720                              M39. 772
      IF ( DUMJ(1)  .EQ. BLANK )  GO TO 10720                           M37.1508
      IF ( DUMJ(2)  .EQ. BLANK )  GO TO 10720                           M37.1509
      IF ( DUMJ(3)  .EQ. BLANK )  GO TO 10720                           M37.1510
      IF ( DUMJ(4)  .NE. BLANK )  GO TO 10720                           M37.1511
      IF ( DUMJ(5)  .NE. BLANK )  GO TO 10720                           M37.1512
      KPAR = KPAR + 2                                                   M39. 773
      GO TO 3636                                                        M39. 774
10720 KILL = 215                                                        M28.2728
      BUS1 = ALNODE                                                     M28.2729
      LSTAT( 19) = 10720                                                M28.2730
      GO TO 9000                                                        M28.2731
  661 IF ( ALNM2 .NE. BLANK )  GO TO 6511                               M39. 775
      KILL = 216                                                        M28.2735
      BUS1 = ALNODE                                                     M28.2736
      LSTAT( 19) = 10721                                                M28.2737
      GO TO 9000                                                        M28.2738
  662 PARSUP(MPAR+1) = 0.0                                              M39. 776
      GO TO 6511                                                        M39. 777
  663 IF (DUM( 2) .EQ. -1.0  .OR.  DUM( 2) .EQ. 1.0)  GO TO 6511        M39. 778
      KILL = 217                                                        M28.2747
      BUS1 = ALNODE                                                     M28.2748
      LSTAT( 19) = 10723                                                M28.2749
      LSTAT( 17) = M                                                    M28.2750
      FLSTAT( 14) = DUM( 2)                                             M28.2751
      GO TO 9000                                                        M28.2752
  666 PARSUP(MPAR+1) = DUM(1) * DELTAT                                  M39. 779
      IVARB(KARG+3) = - 1.0 / PARSUP( MPAR + 1 ) - 0.5                  M39. 780
      IVARB(KARG+4) = - 1                                               M39. 781
      KPAR = KPAR - IVARB( KARG + 3 ) + 1                               M39. 782
      IF ( ALNM2 .NE. TEXT1  )   DUM(2) = 0.0                           M39. 783
      J1 = MPAR + 2                                                     M39. 784
      J2 = MPAR + N + 1                                                 M39. 785
      DO 6661 J = J1, J2                                                M39. 786
 6661 PARSUP(J) = DUM(2)                                                M39. 787
      GO TO 3636                                                        M39. 788
  667 PARSUP(MPAR+2) = PARSUP(MPAR+2) * DUM(1)                          M39. 789
      PARSUP(MPAR+3) = PARSUP(MPAR+3) * DUM(1)                          M39. 790
      KPAR = KPAR + 4                                                   M39. 791
      GO TO 3636                                                        M39. 792
 6799 IF ( NOUTPR .EQ. 0 ) WRITE ( KUNIT6, 1104 )                       M39. 793
 1104 FORMAT ( 22H+NO SUCH SUP. DEVICE.  )                              M39. 794
 9000 IF ( IPRSUP  .GE.  1 )                                            M33. 125
     1 WRITE ( LUNIT6, 4568 )                                           M32. 975
 4568 FORMAT ( 24H  "EXIT  MODULE TACS1A." )                            M32. 976
      RETURN                                                            M33. 126
      END                                                               M28.2771
      SUBROUTINE TACS1B                                                 M28.2772
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      DIMENSION  DUMJ(13)                                               M38. 610
      REAL*8        DUMJ, DELAY                                         M38. 611
      DATA  DELAY   / 6HDELAY  /                                        M38. 612
      IF ( IPRSUP  .GE.  1 )                                            M32. 977
     1 WRITE ( LUNIT6, 4567 )                                           M32. 978
 4567 FORMAT ( 24H  "BEGIN MODULE TACS1B." )                            M32. 979
      LL0 = 0                                                           M28.2782
      LL1 = 1                                                           M28.2783
      LL2 = 2                                                           M28.2784
      LL3 = 3                                                           M28.2785
      LLM1 = -1                                                         M28.2786
      ISOUR = LSTAT(64)                                                 M39. 795
      KJSUP = KINSUP + LSTAT(65)                                        M39. 796
      KKSUP = KJSUP  + LSTAT(65)                                        M39. 797
      KXAI  = KXAR + LSTAT(68)                                          M39. 798
      KBTCS = KATCS + LSTAT(62)                                         M39. 799
      KBWKCS = KAWKCS + LSTAT(68)                                       M39. 800
 1000 IF (NCHAIN .EQ. 1) GO TO 5566                                             
      I = NSUP                                                                  
      GO TO 5577                                                                
 5566 NUKI = KISBLK - 8                                                 M39. 802
      NEXD = 1                                                          M39. 803
      KARGSA = KARG + 3                                                 M42.1424
      IUTY(KIUTY+7) = KARG + 4                                          M42.1425
      IPRSOV(35) = -8888                                                M39. 804
      NDXB = NUK + 1                                                    M38. 617
      MC = 1                                                            M39. 805
      MINS = 0                                                          M37.1575
      IUSER = 0                                                         M37.1576
C                               $$$  ORDERING  $$$                      M37.1577
      KINT = - 1000                                                     M39. 806
      GO TO 1555                                                        M39. 807
 4747 IF ( IABS(IA) .NE. 1 )  GO TO 1984                                        
      I = 1                                                             M38. 618
      IUSER = 9999                                                              
      IF ( IA  .LE. 0 ) NUKI = NUKI + NUK * 8 + 8                       M38. 619
  555 NUKI = NUKI + 8                                                   M38. 620
      IF ( IA .LT. 0 )  NUKI = NUKI - 16                                M38. 621
      GO TO 6677                                                        M38. 622
 2555 I = I + 1                                                         M38. 623
      IF ( I  .LE.  NUK )  GO TO 555                                    M38. 624
      GO TO 2254                                                                
 1984 MPQ = 0                                                                   
      MPP = 0                                                           M37.1591
      MKK = 0                                                           M37.1592
      NGP = 0                                                           M37.1593
      KINT = 1000                                                       M39. 809
 1900 MPK = 0                                                                   
      I = 1                                                             M37.1595
      NUKI = KISBLK - 8                                                 M38. 626
 1910 NUKI = NUKI + 8                                                   M38. 627
      IF ( I  .GT.  NUK )  GO TO 8103                                   M38. 628
      IF ( ISBLK(NUKI+4)  .GT.  0 ) GO TO 1950                          M38. 629
      INFIR = IABS( ISBLK(NUKI+1) )                                     M42.1426
      INLST = IABS( ISBLK(NUKI+9) ) - 1                                 M42.1427
      IF ( I .EQ. NUK )  INLST = NSU                                    M37.1601
      NAMOUT = ATCS(KATCS+I)                                            M38. 632
      IZS = ISBLK( NUKI + 2 )                                           M38. 633
      IF (MPQ .EQ. 0  .OR.  IZS .EQ. 1 ) GO TO 1911                             
      J = ISBLK( NUKI + 3 )                                             M38. 634
      NDX1 = KPRSUP  +  J                                               M39. 810
      NDX2 = NDX1 + 1                                                   M37.1607
      IF ( PARSUP(NDX2) .EQ. 0 ) GO TO 1911                                     
      M = IZS * 6 + J - 4                                               M39. 812
      DN = - PARSUP(NDX1) / PARSUP(NDX2)                                M39. 813
      DO 1266  L=J, M, 6                                                M39. 814
      DN = -DN                                                          M37.1612
      NDX1 = KPRSUP + L                                                 M39. 815
      IF (PARSUP(NDX1) .NE. PARSUP(NDX1+1)*DN) GO TO 1911                       
      IF (PARSUP(NDX2)*PARSUP(NDX1+1) .LT. 0) GO TO 1911                        
 1266 CONTINUE                                                          M37.1616
      GO TO 1983                                                                
 1911 DO 1940 J = INFIR, INLST                                          M37.1618
      NAMIN = KSUS( KALKSU + J )                                        M38. 636
      DO 1930 K = 1, NIU                                                M37.1620
 1930 IF ( NAMIN .EQ. ILNTAB(KALIU+K) ) GO TO 1940                      M38. 637
      DO 1920 K = 1, NSUP                                               M37.1622
      NDX2 = KSPVAR + K                                                 M37.1624
      IF ( NAMIN .NE. ILNTAB(NDX2) ) GO TO 1920                         M38. 638
      IF ( INSUP(KINSUP+K) .NE. 0 ) GO TO 1940                          M37.1627
      GO TO 1950                                                        M37.1628
 1920 CONTINUE                                                          M37.1629
      DO 1919 K = 1, NUK                                                M37.1630
      NAMSBK = ATCS(KATCS+K)                                            M38. 639
      IF ( NAMIN .NE. NAMSBK ) GO TO 1919                               M38. 640
      NDX1 = KISBLK + K * 8 - 4                                         M38. 641
      IF ( ISBLK(NDX1) .GT. 0 ) GO TO 1940                              M38. 642
      GO TO 1950                                                        M37.1633
 1919 CONTINUE                                                          M37.1634
 1940 CONTINUE                                                          M37.1635
 1983 MPK = 1                                                                   
 6677 NDXB = NDXB - 1                                                           
      ISBLK(NUKI+4) = NDXB                                                      
 6767 NDY = KISBLK + NDXB * 8 - 1                                       M38. 644
      ISBLK(NDY) = 0                                                            
 1555 IF (MINS .EQ. NSUP) GO TO 1950                                            
      NNN = 0                                                                   
 4646 JJJ = 0                                                                   
      MMM = 0                                                                   
      MM = MC                                                           M39. 820
      DO 1980  IS = MM, NSUP                                                    
      IF ( INSUP(KINSUP+IS) .NE. 0 ) GO TO 1980                         M37.1648
      N1 = INSUP( KJSUP + IS )                                          M39. 821
      N3 = INSUP( KKSUP + IS )                                          M39. 822
      N2 = IABS( N3 )                                                   M38. 647
      NSTEP = 1                                                         M39. 823
      IF ( N1  .LT.  0 ) GO TO 1970                                     M39. 824
      J  = N1                                                           M37.1652
      K2 = N2                                                           M37.1653
      NSTEP = 3                                                         M39. 825
 1973 IF ( N1 .LT. 0 ) GO TO 2973                                       M39. 826
      N4 = IVARB( J + 1 )                                               M39. 827
      IF ( N4 .LT. 0 ) GO TO 1959                                       M38. 650
      IF ( N3 .LE. 0  .AND.  N4 .NE. 1 ) GO TO 1959                     M38. 651
      NAMSUP = IVARB( J + 3 )                                           M39. 828
      GO TO 1853                                                        M38. 653
 2973 NAMSUP = KSUS(KALKSU+J)                                           M38. 654
 1853 DO 1951 K = 1, NUK                                                M37.1656
      NAMSBK = ATCS( KATCS + K )                                        M38. 655
      IF ( NAMSBK .NE. NAMSUP ) GO TO 1951                              M38. 656
      NDX1 = KISBLK + K * 8 - 4                                         M38. 657
      IF ( ISBLK(NDX1) .GT. 0 ) GO TO 1959                              M38. 658
      GO TO 1990                                                        M37.1659
 1951 CONTINUE                                                          M37.1660
      DO 1953 K = 1, NSUP                                               M37.1661
      IF ( ILNTAB(KSPVAR+K) .NE. NAMSUP ) GO TO 1953                    M38. 659
      IF ( INSUP(KINSUP+K)  .NE.  0 ) GO TO 1959                        M37.1664
      GO TO 1990                                                        M37.1665
 1953 CONTINUE                                                          M37.1666
 1959 J = J + NSTEP                                                     M39. 829
      IF ( J  .LE.  K2 )  GO TO 1973                                    M37.1668
      IF ( N1 .GT. 0 )  GO TO 1978                                      M39. 830
      IF ( J  .GT. K2+1 )  GO TO 1851                                   M37.1670
      NAMSUP = IVARB( NN + 3 )                                          M39. 831
      IF ( NAMSUP  .GT.  1 ) GO TO 1853                                 M38. 661
      GO TO 1959                                                        M37.1673
 1851 IF ( J .GT. K2+2 ) GO TO 1978                                     M37.1674
      NAMSUP = IVARB(  NN + 4 )                                         M39. 832
      IF ( NAMSUP  .GT.  1 ) GO TO 1853                                 M38. 663
 1978 MMM = 1                                                                   
      IF ( NDXB .LE. NUK  .OR.  KINT .NE. -1000 )  GO TO 4848           M39. 833
      IUTY(KIUTY+4) = IS                                                M39. 834
      KINT = - 100                                                      M39. 835
      GO TO 4949                                                        M39. 836
 4848 IF (NNN .EQ. 0) ISBLK(NDY) = IS                                           
      IF (NNN .NE. 0) INSUP(KINSUP+NNN) = IS                                    
 4949 NNN = IS                                                          M39. 838
      INSUP(KINSUP+IS) = -1                                                     
      MINS = MINS + 1                                                           
      GO TO 1980                                                        M37.1683
 1970 IF ( N2 .EQ. 53  .AND. IVARB(NN+3) .EQ. 1 ) GO TO 1978            M39. 839
      NN = - N1                                                         M39. 840
      K2 = IVARB( NN + 2 )                                              M39. 841
      J  = IVARB( NN + 1 )                                              M39. 842
      GO TO 1973                                                        M37.1686
 1990 IF ( JJJ  .EQ.  1 )  GO TO 1980                                   M37.1687
      JJJ = 1                                                           M37.1688
      MC = IS                                                                   
 1980 CONTINUE                                                          M37.1690
      IF (MMM .EQ. 1 .AND. MINS .LT. NSUP) GO TO 4646                           
 1950 IF (IUSER .EQ. 9999) GO TO 2555                                           
      IF (NDXB .EQ. 1) GO TO 2254                                               
      IF ( KINT  .LT. 0 )  GO TO 4747                                   M39. 845
      IF ( I .EQ. 0 )  GO TO 1900                                       M38. 666
      I = I + 1                                                         M37.1694
      GO TO 1910                                                        M37.1695
 7104 KILL = 122                                                                
      LSTAT( 16) = LSTAT(68)                                            M39. 846
      LSTAT( 17) = 8                                                    M39. 847
      LSTAT(19) = 7104                                                  M37.1699
      GO TO 9000                                                        M37.1700
  777 NUKR = KRSBLK + NUK * 4                                                   
      WRITE ( LUNIT6,  7171 )                                           M37.1702
 7171 FORMAT ( 47H --------- MORE THAN ONE LIMITER IN THE ZS LOOP,      M38. 669
     1 44H INSERT A DELAY S-BLOCK TO DECOUPLE THE LOOP  )               M38. 670
      NUKI = KISBLK - 4                                                 M38. 671
      DO 2468 LC = 1, NUK                                               M38. 672
      NUKI = NUKI + 8                                                   M38. 673
 2468 IF ( ISBLK(NUKI) .GT. 0 ) ISBLK(NUKI) = ISBLK(NUKI) + 1           M38. 674
      NUK = NUK + 1                                                     M38. 675
      IF ( NUK  .LE.  LSTAT(61) )  GO TO 1347                           M39. 848
      KILL = 122                                                        M38. 677
      LSTAT(16) = LSTAT(61)                                             M39. 849
      LSTAT(17) = 1                                                     M38. 679
      LSTAT(19) = 1347                                                  M38. 680
      GO TO 9000                                                        M38. 681
 1347 NSU = NSU + 1                                                     M38. 682
      IF ( NSU  .LE.  NSUDV ) GO TO 1357                                M39. 850
      KILL = 122                                                        M38. 684
      LSTAT(16) = LSTAT(63)                                             M39. 851
      LSTAT(17) = 3                                                     M38. 686
      LSTAT(19) = 1357                                                  M38. 687
      GO TO 9000                                                        M38. 688
 1357 J = KPAR + 1                                                      M39. 852
      NDX1 = KPRSUP + KPAR                                              M39. 853
      KPAR = KPAR + 10                                                  M39. 854
      IF ( KPAR  .LE.  LSTAT(67) )  GO TO 1367                          M39. 855
      KILL = 122                                                        M38. 693
      LSTAT(16) = LSTAT(67)                                             M39. 856
      LSTAT(17) = 7                                                     M39. 857
      LSTAT(19) = 1367                                                  M38. 696
      GO TO 9000                                                        M38. 697
 1367 NUKI = NUKI + 4                                                   M38. 698
      ISBLK(NUKI+1) = NSU                                               M42.1428
      ISBLK(NUKI+2) = 2                                                 M38. 700
      ISBLK(NUKI+3) =  J                                                M38. 701
      ISBLK(NUKI+4) = NDXB                                              M38. 702
      CALL NMINCR( DELAY, NEXD )                                        M42.1429
      NEXD = NEXD + 1                                                   M42.1430
      N23 = 0                                                           M38. 705
      CALL NAMEA6( DELAY, N23 )                                         M38. 706
      ATCS(KATCS+NUK) = N23                                             M38. 707
      KSUS(KKSUS+NSU) = 1                                               M38. 708
      KSUS(KALKSU+NSU) = KSUS( KALKSU + K )                             M38. 709
      KSUS(KALKSU+K) = N23                                              M38. 710
      RSBLK(NUKR+4) = 0.0                                               M38. 711
      XAR(KXAR+NUK) = 0.0                                               M38. 712
      XAR(KXAI+NUK) = 0.0                                               M39. 859
      AWKCS(KAWKCS+NUK) = 1.0                                           M39. 860
      AWKCS(KBWKCS+NUK) = 1.0                                           M39. 861
      PARSUP(NDX1+1) = 1.0                                              M39. 862
      PARSUP(NDX1+2) = 1.0                                              M39. 863
      PARSUP(NDX1+3) = 1.0                                              M39. 864
      PARSUP(NDX1+4) = 1.0                                              M39. 865
      PARSUP(NDX1+5) = 0.0                                              M39. 866
      PARSUP(NDX1+6) = 0.0                                              M39. 867
      PARSUP(NDX1+7) = - DELTAT / 2.0                                   M39. 868
      PARSUP(NDX1+8) = - PARSUP(NDX1+5)                                 M39. 869
      PARSUP(NDX1+9) = PARSUP(NDX1+7)                                   M39. 870
      PARSUP(NDX1+10) = PARSUP(NDX1+8)                                  M39. 871
      DO 4488 JC = 2, NUK                                               M38. 720
      ISBLK(NUKI+7) = ISBLK( NUKI - 1 )                                 M38. 721
 4488 NUKI = NUKI - 8                                                   M38. 722
      MKK = 0                                                           M38. 723
      NDY = KISBLK + NDXB * 8 - 1                                       M38. 724
      ISBLK(NDY) = 0                                                    M38. 725
      GO TO 1900                                                        M38. 726
 8103 MPQ = 0                                                           M37.1707
      IF ( MPK  .EQ.  1 ) GO TO 1900                                            
      IF ( NGP  .GT.  0 )  GO TO 8811                                           
      IF ( MKK  .GT.  0 )  GO TO 1997                                           
      IF ( MPP .EQ. 1 ) GO TO 1996                                              
      MPQ = 1                                                           M37.1712
      MPP = 1                                                           M37.1713
      GO TO 1900                                                                
 1996 NUKI = KISBLK - 8                                                 M38. 727
      DO 1414 I = 1, NUK                                                        
      NUKI = NUKI + 8                                                   M38. 729
      IF (ISBLK(NUKI+4)  .GE.  0) GO TO 1414                                    
      NAMOUT = ATCS( KATCS + I )                                        M38. 731
 1408 NP =  1                                                                   
      ATCS(KBTCS+NP) = NAMOUT                                                   
      ISBLK(KISBLK+8) = 0                                                       
      J11 = KISBLK + 5                                                  M38. 734
      J12 = J11 + NUK * 8 - 2                                           M38. 735
      DO 4247  J13 = J11, J12, 8                                        M38. 736
      ISBLK(J13) = 0                                                    M38. 737
 4247 ISBLK(J13+1) = 0                                                  M38. 738
      ISBLK(KISBLK+5) =  I                                                      
      MNP = 1                                                                   
      NUKM = KISBLK                                                     M38. 740
 1444 NQ = NP                                                                   
      NUKQ = KISBLK + NQ * 8 - 8                                        M38. 741
      J = 1                                                             M37.1726
      NUKJ = KISBLK - 8                                                 M38. 742
 1427 NUKJ = NUKJ + 8                                                   M38. 743
      IF ( ISBLK(NUKJ+4)  .GT.  0 ) GO TO 1429                          M38. 744
      N11 = IABS( ISBLK(NUKJ+1) )                                       M42.1431
      N12 = IABS( ISBLK(NUKJ+9) ) - 1                                   M42.1432
      IF ( J  .EQ.  NUK )  N12 = NSU                                    M37.1730
      NAMSBK = ATCS( KATCS + J )                                        M38. 747
 1433 NIK = 0                                                           M37.1732
      DO 1421 K = N11, N12                                                      
      NAMIN = KSUS( KALKSU + K )                                        M38. 748
      L =  1                                                                    
      NUKL = KISBLK - 8                                                 M38. 749
 1423 NUKL = NUKL + 8                                                   M38. 750
      NAMEXM = ATCS( KBTCS + L )                                        M39. 873
      IF ( NAMIN .NE. NAMEXM ) GO TO 1425                                       
      IF (ISBLK(NUKJ+6) .GT. 0) GO TO 1429                                      
      IRR = ISBLK( NUKL + 8 )                                                   
      IF (IRR .EQ. 1 .AND. NAMSBK .EQ. NAMOUT) GO TO 777                        
      IF (NAMSBK .EQ. NAMOUT) GO TO 1421                                        
      IF ( ISBLK(NUKJ+4) .LT. 0 .OR. IRR .EQ. 1) ISBLK(NUKJ+6) = 1      VAX. 164
      IF ( ISBLK(NUKJ+6) .LT. 0 ) GO TO 1421                                    
      IF ( NIK  .EQ.  1 )  GO TO 1474                                           
      NIK = 1                                                                   
      NQ = NQ + 1                                                               
      NUKQ = NUKQ + 8                                                   M38. 759
      ATCS(KBTCS+NQ) = NAMSBK                                                   
      IF ( ISBLK(NUKJ+6) .EQ. 0 ) ISBLK(NUKJ+6) = -1                            
 1474 ISBLK(NUKQ+8) = ISBLK( NUKJ + 6 )                                         
      IF ( ISBLK(NUKJ+6) .LT. 0 ) GO TO 1421                                    
      IF ( ISBLK(NUKJ+4) .LT. 0 ) GO TO 1616                                    
      GO TO 1429                                                                
 1616 MNP = MNP + 1                                                             
      NUKM = NUKM + 8                                                   M38. 765
      ISBLK(NUKM+5) = J                                                         
      GO TO 1429                                                        M37.1754
 1425 L = L + 1                                                                 
      IF ( L  .LE.  NP )  GO TO 1423                                            
 1421 CONTINUE                                                          M37.1757
 1429 J = J + 1                                                                 
      IF ( J .LE. NUK )  GO TO 1427                                             
      IF ( NQ  .EQ.  NP )  GO TO 1714                                           
      NJ = NQ - NP                                                      M37.1761
      NP1 = NP * 8                                                      M38. 767
      NUKJ = KISBLK - 8                                                 M38. 768
      DO  1449  J = 1, NJ                                               M37.1762
      NUKJ = NUKJ + 8                                                   M38. 769
      ATCS(KBTCS+J) = ATCS(KBTCS+J + NP)                                M39. 875
 1449 ISBLK(NUKJ+8) = ISBLK(NUKJ+8 + NP1)                               M38. 771
      NP = NJ                                                           M37.1765
      GO TO 1444                                                                
 1714 IF (MKK .NE. 0) GO TO 1814                                                
      JK2 = KCOLCS + LSTAT(62)                                          M39. 876
      MKK = JK2 - MNP                                                   M37.1769
      NUKM = KISBLK - 8                                                 M38. 773
      DO 1834  JJ = 1, MNP                                              M37.1770
      NUKM = NUKM + 8                                                   M38. 774
 1834 ICOLCS( MKK + JJ ) = ISBLK( NUKM + 5 )                            M38. 775
      GO TO 1414                                                        M37.1772
 1814 JK1 = MKK + 1                                                             
      IJK = 0                                                           M37.1774
      NUKM = KISBLK - 8                                                 M38. 776
      DO 1840 JJ = 1, MNP                                               M37.1775
      NUKM = NUKM + 8                                                   M38. 777
      DO 1861 JK = JK1, JK2                                             M37.1776
      IF (ICOLCS(JK) .NE. ISBLK(NUKM+5)) GO TO 1861                     M38. 778
      IF (JJ .EQ. 1) GO TO 1878                                                 
      ISBLK(NUKM+5) = 0                                                 M38. 779
      IJK = IJK + 1                                                     M37.1780
      GO TO 1840                                                        M37.1781
 1861 CONTINUE                                                          M37.1782
 1840 CONTINUE                                                          M37.1783
      IF (IJK .NE. 0) GO TO 1888                                                
      MKK = MKK - MNP                                                   M37.1785
      NUKK = KISBLK - 3                                                 M38. 780
      DO  1883  JL = 1, MNP                                                     
      NUKK = NUKK + 8                                                   M38. 781
 1883 ICOLCS(MKK+JL) = ISBLK( NUKK )                                    M38. 782
      GO TO 1414                                                        M37.1788
 1878 IF ( MNP .EQ. 1 )  GO TO 1414                                             
      JJJ = JK                                                          M37.1790
       NUKJL = KISBLK - 8                                               M38. 783
      DO 1898 JL = 2, MNP                                               M37.1791
      NUKJL = NUKJL + 8                                                 M38. 784
      DO 1897  JM = JK1, JK2                                            M37.1792
      IF (ICOLCS(JM) .NE. ISBLK(NUKJL+5)) GO TO 1897                    M38. 785
      IF ( JM  .GT.  JJJ )  GO TO 1898                                  M37.1794
      JJJ = JM                                                          M37.1795
      GO TO 1898                                                        M37.1796
 1897 CONTINUE                                                          M37.1797
      CALL STOPTP                                                               
 1898 CONTINUE                                                          M37.1799
      IF ( JJJ  .EQ.  JK ) GO TO 1414                                   M37.1800
      MNP = ICOLCS(JK)                                                  M37.1801
      NNN = JK - JJJ                                                    M37.1802
      DO 1881 JN = 1, NNN                                               M37.1803
      JNI = JK - JN + 1                                                 M37.1804
 1881 ICOLCS(JNI) = ICOLCS(JNI-1)                                       M37.1805
      ICOLCS(JJJ) = MNP                                                 M37.1806
      GO TO 1414                                                        M37.1807
 1888 MKK = MKK - MNP + IJK                                                     
      MJUMP = MKK                                                       M37.1809
      NUKM = KISBLK - 8                                                 M38. 786
      DO 1893  JL = 1,MNP                                               M37.1810
      NUKM = NUKM + 8                                                   M38. 787
      IF ( ISBLK(NUKM+5)  .EQ.  0 )  GO TO 1893                         M38. 788
      MJUMP = MJUMP + 1                                                 M37.1812
      ICOLCS(MJUMP) = ISBLK( NUKM + 5 )                                 M38. 789
 1893 CONTINUE                                                          M37.1814
 1414 CONTINUE                                                                  
      IF ( MKK  .EQ.  0 )  GO TO  8811                                          
 1997 MKK = MKK + 1                                                             
      IF ( MKK  .GT.  JK2 ) GO TO 8811                                  M37.1818
      NFUN = ICOLCS(MKK)                                                M42.1433
      NUKI = KISBLK + NFUN * 8 - 8                                      M42.1434
      IF ( ISBLK( NUKI + 4 )  .GT.  0 ) GO TO 1997                      M42.1435
 4141 INFIR = IABS( ISBLK(NUKI+1) )                                     M42.1436
      INLST = IABS( ISBLK(NUKI+9) ) - 1                                 M42.1437
      IF ( NFUN .EQ. NUK )  INLST = NSU                                 M42.1438
      NOM = 0                                                           M42.1439
      DO 4556 J1 = INFIR, INLST                                         M42.1440
      NAMIN = KSUS( KALKSU + J1 )                                       M42.1441
      DO 4667 J2 = 1, NSUP                                              M42.1442
      NDX2 = KSPVAR + J2                                                M42.1443
      IF ( NAMIN .NE. ILNTAB(NDX2) ) GO TO 4667                         M42.1444
      IF ( INSUP(KINSUP+J2) .NE. 0 ) GO TO 4556                         M42.1445
      IF ( NOM  .GT. 0 ) GO TO 4343                                     M42.1446
      NOM = 1                                                           M42.1447
      KARGSA = KARGSA + 1                                               M42.1448
      IVARB(KARGSA) = - NFUN                                            M42.1449
      ISBLK(NUKI+1) = - ISBLK(NUKI+1)                                   M42.1450
 4343 KARGSA = KARGSA + 1                                               M42.1451
      IVARB(KARGSA) = J2                                                M42.1452
      IUTY(KIUTY+8) = KARGSA                                            M42.1453
      IF ( KARGSA - KIVARB  .LT.  LSTAT(66) )  GO TO 4556               M42.1454
      KILL = 122                                                        M42.1455
      LSTAT( 19) = 4343                                                 M42.1456
      LSTAT( 16) = LSTAT(66)                                            M42.1457
      LSTAT( 17) = 6                                                    M42.1458
      GO TO 9000                                                        M42.1459
 4667 CONTINUE                                                          M42.1460
 4556 CONTINUE                                                          M42.1461
      I = 0                                                             M42.1462
      GO TO 6677                                                                
 8811 NGP = 0                                                                   
      I = 0                                                             M37.1825
      NUKJ = KISBLK - 8                                                 M38. 792
      DO 8822 JJ = 1, NUK                                               M37.1826
      NUKJ = NUKJ + 8                                                   M38. 793
      IF ( ISBLK(NUKJ+4)  .GT.  0 ) GO TO 8822                          M38. 794
      IJ9 = IABS( ISBLK(NUKJ+9) )                                       M42.1463
      IF ( JJ  .EQ.  NUK ) IJ9 = NSU + 1                                M37.1829
      IJ = IJ9 - IABS( ISBLK(NUKJ+1) )                                  M42.1464
      IF ( IJ  .LE.  NGP ) GO TO 8822                                   M37.1831
      NGP = IJ                                                          M37.1832
      NFUN = JJ                                                         M42.1465
 8822 CONTINUE                                                          M37.1834
      NUKI = KISBLK + NFUN * 8 - 8                                      M42.1466
      GO TO 4141                                                        M42.1467
 2254 IF ( MINS .LT. NSUP ) GO TO 9200                                  M39. 877
      NJ = LSTAT(64) + NSUP                                             M39. 878
      KTAB = NUK + NJ                                                   M39. 879
      IF ( KTAB .GT. LSTAT(68) )  GO TO 7104                            M39. 880
      KOFSCE = KTAB                                                     M39. 881
      KALIU = KLNTAB + NUK                                              M39. 882
      KSPVAR = KALIU + LSTAT(64)                                        M39. 883
      NDX1 = KLNTAB + KTAB + 1                                          M39. 884
      NDX2 = KLNTAB + NJ + 1                                            M39. 885
      DO 1001 I = 1, NJ                                                 M39. 886
 1001 ILNTAB(NDX1-I) = ILNTAB(NDX2-I)                                   M39. 887
      NUKI = KISBLK - 8                                                 M38. 799
      DO  2264  I = 1, NUK                                              M37.1838
      NUKI = NUKI + 8                                                   M38. 800
      K = KLNTAB + ISBLK( NUKI + 4 )                                    M38. 801
 2264 ILNTAB(K) = ATCS( KATCS + I )                                     M38. 802
      IF ( IPRSUP .GT. 0 )                                              M37.1848
     1 WRITE ( LUNIT6, 8844 ) ( ILNTAB(KLNTAB+I), I = 1, NUK )          M38. 808
 8844 FORMAT ( 18H  $$$ ILNTAB $$$ =,/, ( 5X, 15I8 ) )                  M38. 809
C                                                 $$$  FORM TABLES  $$$ M37.1855
 2540 IF ( NSUP .EQ. 0 )  GO TO 21100                                   M39. 888
      I = 1                                                             M37.1859
 5577 J = INSUP( KJSUP + I )                                            M39. 889
      K = INSUP( KKSUP + I )                                            M39. 890
      IF ( J .LT. 0 )  GO TO 2116                                       M39. 891
      K1 = K                                                            M37.1866
      IF ( K .LT. 0 )  K = -K                                           M37.1867
      DO 2113  N = J, K, 3                                              M39. 892
      IF ( IVARB(N+1) .LT. 0 )  GO TO 2113                              M39. 893
      IF ( K1 .LT. 0  .AND.  IVARB(N+1) .NE. 1 )  GO TO 2113            M39. 894
      NIVARB = IVARB( N + 3 )                                           M39. 895
      DO 2114  M = 1, KTAB                                              M37.1872
      NDX2 = KLNTAB + M                                                 M37.1874
      IF ( NIVARB .EQ. ILNTAB(NDX2) )  GO TO 2175                       M39. 896
 2114 CONTINUE                                                          M37.1876
 2021 KILL = 143                                                        M37.1877
      BUS1 = TEXVEC( NIVARB )                                           M39. 897
      BUS2 = TEXVEC( ILNTAB( KSPVAR + I ) )                             M38. 813
      LSTAT( 19) = 2115                                                 M37.1882
      GO TO 9000                                                        M37.1883
 2175 IVARB(N+3) = M                                                    M39. 898
 2113 CONTINUE                                                          M37.1891
      GO TO 2111                                                        M37.1892
 2116 J = - J                                                           M39. 899
      IF ( IVARB(J+4) .GT. 1 )  GO TO  7162                             M39. 900
      IF ( IVARB(J+4) .EQ. 1 ) IVARB(J+4) = 0                           M39. 901
      IVARB(J+4) = - IVARB(J+4)                                         M39. 902
      GO TO  7165                                                       M37.1900
 7162 DO 2118  M = 1, KTAB                                              M37.1901
      NDX2 = KLNTAB + M                                                 M37.1902
      IF ( IVARB(J+4) .EQ. ILNTAB(NDX2) )  GO TO 2719                   M39. 903
 2118 CONTINUE                                                          M37.1904
 2721 KILL = 143                                                        M37.1905
      BUS1 = TEXVEC( IVARB(J+4) )                                       M39. 904
      BUS2 = TEXVEC( ILNTAB( KSPVAR + I ) )                             M38. 825
      LSTAT( 19) = 2119                                                 M37.1909
      GO TO 9000                                                        M37.1910
 2719 IVARB(J+4) = M                                                    M39. 905
 7165 IF (IVARB(J+3) .GT. 1 .OR. IVARB(J+3) .EQ. -9999) GO TO 7163      M39. 906
      IF ( IVARB(J+3) .EQ. 1 ) IVARB(J+3) = 0                           M39. 907
      IVARB(J+3) = - IVARB(J+3)                                         M39. 908
      GO TO  7161                                                       M37.1920
 7163 DO 2182  M = 1, KTAB                                              M37.1921
      NDX2 = KLNTAB + M                                                 M37.1922
      IF ( IVARB(J+3) .EQ. ILNTAB(NDX2) )  GO TO 2199                   M39. 909
 2182 CONTINUE                                                          M37.1924
      IF ( K .EQ. 58  .AND.  IVARB(J+3) .EQ. -9999) GO TO 7161          M39. 910
 2821 KILL = 143                                                        M37.1928
      BUS1 = TEXVEC( IVARB(J+3) )                                       M39. 911
      BUS2 = TEXVEC( ILNTAB( KSPVAR + I ) )                             M39. 912
      LSTAT( 19) = 2192                                                 M37.1932
      GO TO 9000                                                        M37.1933
 2199 IVARB(J+3) = M                                                    M39. 913
 7161 JK = IVARB(J+2)                                                   M39. 914
      JM = IVARB(J+1)                                                   M39. 915
      DO 1170  N = JM, JK                                               M39. 916
      NDX1 = KALKSU + N                                                 M37.1945
      IF ( KSUS( NDX1) .NE. 1 )  GO TO 1171                             M38. 836
      KSUS(NDX1) = 0                                                    M38. 837
      GO TO 1170                                                        M37.1948
 1171 DO 1180  M = 1, KTAB                                              M37.1949
      NDX2 = KLNTAB + M                                                 M37.1950
      IF ( KSUS(NDX1) .EQ. ILNTAB(NDX2) )  GO TO 1199                   M39. 917
 1180 CONTINUE                                                          M37.1952
 1122 KILL = 143                                                        M37.1953
      BUS1 = TEXVEC( KSUS(NDX1) )                                       M38. 839
      BUS2 = TEXVEC( ILNTAB( KSPVAR + I ) )                             M38. 840
      LSTAT( 19) = 1190                                                 M37.1957
      GO TO 9000                                                        M37.1958
 1199 KSUS(NDX1) = M                                                    M38. 841
 1170 CONTINUE                                                          M37.1966
      IF ( K .NE. 50 ) GO TO 2111                                       M39. 918
      NN = IVARB(J)                                                     M39. 919
      MM = KXTCS + NUK + I + LSTAT(64)                                  M39. 920
      XTCS(MM) = PARSUP(NN)                                             M39. 921
 2111 I = I + 1                                                         M37.1967
      IF ( I  .LE.  NSUP )  GO TO 5577                                  M37.1968
      IF ( NCHAIN  .NE.  18 )  GO TO  21100                             M39. 922
      NDX1 = KINSUP + NSUP                                              M39. 923
      INSUP(NDX1) = - 1                                                 M39. 924
      IF ( NSUP .EQ. KONSUP+1 )  GO TO 9000                             M39. 925
      INSUP(NDX1-1) = NSUP                                              M39. 926
      GO TO 9000                                                        M39. 927
 9200 KILL = 500                                                        M37.2026
      WRITE ( LUNIT6, 9202 )                                            M38. 851
 9202 FORMAT (  43H  THE CASE WILL BE KILLED BECAUSE THERE ARE ,        M38. 852
     1    50H SOME LOOPS IN SUPPLEMENTAL VARIABLES OR DEVICES. ,/,      M38. 853
     2    49H  THIS IS NOT ALLOWED.    THE LOOPED SUPPLEMENTAL,         M38. 854
     3    25H  BLOCKS ARE AS FOLLOWS :   )                              M38. 855
      J = 0                                                             M38. 856
      DO 9208 I = 1, NSUP                                               M38. 857
      IF ( INSUP(KINSUP+I)  .NE.  0 ) GO TO 9208                        M38. 858
      J = J + 1                                                         M38. 859
      NDX1 = ILNTAB( KSPVAR + I )                                       M38. 860
      WRITE ( LUNIT6, 9204 )   J, TEXVEC(NDX1)                          M38. 861
 9204 FORMAT ( 10H  ********, 2X, I5, 4X, A6,  10H  ******** )          M38. 862
 9208 CONTINUE                                                          M38. 863
      GO TO 9000                                                        M37.2027
21100 DO 211 I=1,NSU                                                    M39. 928
      NDX3 = KALKSU + I                                                 M33. 137
      DO 212 J = 1, KTAB                                                M39. 929
      NDX2 = KLNTAB + J                                                 M22.2406
      IF ( KSUS(NDX3) .EQ. ILNTAB(NDX2) )  GO TO 213                    M38. 952
  212 CONTINUE                                                          M13. 877
 3212 NUKK = KISBLK - 8                                                 M38. 953
      DO 466 K = 1, NUK                                                 M38. 954
      NUKK = NUKK + 8                                                   M38. 955
      NDX1 = IABS( ISBLK(NUKK+1) )                                      M42.1468
      NDX2 = IABS( ISBLK(NUKK+9) ) - 1                                  M42.1469
      IF ( K  .EQ.  NUK )  NDX2 = NSU                                   M37.2116
      IF ( I .GE. NDX1  .AND.  I .LE. NDX2 )  GO TO 469                 M37.2117
  466 CONTINUE                                                          M13. 880
      CALL STOPTP                                                               
  469 K = ISBLK(NUKK+4)                                                 M38. 958
  472 KILL = 121                                                        M13. 888
      LSTAT(19) = 472                                                   M13. 889
      BUS1 = TEXVEC( KSUS(NDX3) )                                       M38. 959
      NDX1 = KLNTAB + K                                                 M22.2420
      BUS2 = TEXVEC( ILNTAB(KLNTAB+K) )                                 M38. 960
      WRITE (LUNIT6,497) BUS1, BUS2                                     M38. 961
  497 FORMAT(17H0 ERROR.  ENTRY =,A6,26H= REFERENCED IN FUNCTION =,A6,  M13. 893
     115H= IS UNDEFINED. )                                              M13. 894
      GO TO 9000                                                        M13. 895
  213 KSUS(NDX3) = J                                                    M39. 930
  211 CONTINUE                                                          M13. 897
      NUKI = KISBLK - 8                                                 M38. 963
      DO 10 I = 1, NUK                                                  M37.2123
      NUKI = NUKI + 8                                                   M38. 964
      M = ISBLK( NUKI + 4 )                                             M38. 965
      NUKM = KISBLK + M * 8 - 8                                         M38. 966
      NUKR = KRSBLK + M * 4 - 4                                         M38. 967
      ISBLK(NUKM+5)  = 0                                                M38. 968
      ISBLK(NUKM+6)  = 0                                                M38. 969
      RSBLK(NUKR+2)  = -FLTINF                                          M38. 970
      RSBLK(NUKR+3)  =  FLTINF                                          M38. 971
      DO 20 L = 1, 2                                                    M37.2129
      IF ( L .EQ. 2 )  GO TO 50                                         M37.2130
      N6 = AWKCS( KAWKCS + I )                                          M38. 972
      GO TO 51                                                          M37.2132
   50 N6 = AWKCS( KBWKCS + I )                                          M39. 931
   51 IF ( N6 .EQ. 1 )  GO TO 20                                        M38. 974
      DO 30 J=1,KTAB                                                    M37.2135
      IF ( N6 .EQ. ILNTAB(KLNTAB+J) )  GO TO 40                         M38. 975
   30 CONTINUE                                                          M37.2137
      KILL = 131                                                        M37.2138
      BUS1 = TEXVEC( ILNTAB( KLNTAB + M ) )                             M38. 976
      BUS6 = TEXVEC(N6)                                                 M38. 977
      LSTAT(14) = L                                                     M37.2140
      LSTAT(19) = 30                                                    M37.2141
      WRITE (LUNIT6,99) BUS6 , BUS1                                     M38. 978
   99 FORMAT (24H ERROR.  NAME OF LIMIT  , A6, 13H  AT BLOCK = ,        M37.2143
     1   A6, 20H  IS UNIDENTIFIABLE. )                                  M38. 979
      GO TO 9000                                                        M37.2145
   40 IF ( L .EQ. 2 )  GO TO 41                                         M37.2146
      ISBLK(NUKM+5) = J                                                 M38. 980
      GO TO 20                                                          M37.2148
   41 ISBLK(NUKM+6) = J                                                 M38. 981
   20 CONTINUE                                                          M37.2150
      IF ( XAR(KXAR+I)  .EQ.  0.0    .AND.                              M37.2151
     1     XAR(KXAI+I)  .EQ.  0.0  )   GO TO 10                         M39. 932
      RSBLK(NUKR+2)  =  XAR( KXAR + I )                                 M38. 982
      RSBLK(NUKR+3)  =  XAR( KXAI + I )                                 M39. 933
   10 CONTINUE                                                          M37.2155
      IF ( IPRSUP  .LT.  2 )  GO TO 1006                                M13. 911
      WRITE ( LUNIT6, 2007 )                                            M38. 984
 2007 FORMAT ( 42H         N    IFLS   ILIM1   ILIM2   NUKSP )          M38. 985
      NUKN = KISBLK - 8                                                 M38. 986
      DO  5654  N = 1, NUK                                              M38. 987
      NUKN = NUKN + 8                                                   M38. 988
 5654 WRITE (LUNIT6,2010) N, ISBLK(NUKN+4), ISBLK(NUKN+5),              M38. 989
     1                       ISBLK(NUKN+6), ISBLK(NUKN+7)               M38. 990
 2010 FORMAT ( 2X, 5I8 )                                                M38. 991
      WRITE (LUNIT6,2013) (N, KSUS(KALKSU+N), N=1,NSU)                  M38. 992
 2013 FORMAT ( 1H0, 17X,3HKSU/( 2X, 2I8  )  )                           M38. 993
      IF ( NSUP  .EQ.  0 )  GO TO  1012                                 M39. 934
      WRITE ( LUNIT6, 1033 ) KARG                                       M39. 935
 1033 FORMAT ( 9H  KARG = , I8 ,/, 24H       N  IOPSUP  IFNSUP,         M39. 936
     1  48H  IRGSUP    IDEV     KDJ     KDK  ILDEV1  ILDEV2  )          M39. 937
      DO 2034  I = 1, NSUP                                              M39. 938
      N1 = INSUP( KJSUP + I )                                           M39. 939
      IF (  N1  .LT.  0 )  GO TO 2014                                   M39. 940
      N2 = INSUP( KKSUP + I )                                           M39. 941
      WRITE (LUNIT6,2008) (N,IVARB(N+1),IVARB(N+2),                     M39. 942
     2 IVARB(N+2), N = N1, N2, 3)                                       M39. 943
 2008 FORMAT ( 4I8 )                                                    M39. 944
      GO TO 2034                                                        M39. 945
 2014 N1 = -N1                                                          M39. 946
      WRITE (LUNIT6, 2022 ) N1, IVARB(N1), IVARB(N1+1),                 M39. 947
     1 IVARB(N1+2), IVARB(N1+3), IVARB(N1+4)                            M39. 948
 2022 FORMAT ( I8, 24X, 5I8 )                                           M39. 949
 2034 CONTINUE                                                          M39. 950
      IF ( NSUDV .LT. LSTAT(63) )  WRITE ( LUNIT6, 2015 )               M39. 951
     1 (N, KSUS(KALKSU+N), KSUS(KKSUS+N), N=NSUDV+1, LSTAT(63))         M39. 952
 2015 FORMAT ( 1H0,11X,13HALKSU    KSUS, /, (1X, 3I8) )                 M38. 999
 1012 WRITE (LUNIT6,2017) KTAB, (N, ILNTAB(KLNTAB+N), N=1,KTAB)         M38.1000
 2017 FORMAT (9H0 KTAB = ,I5/9X,7H ILNTAB ,/,(1X,2I8))                  M39. 953
      WRITE (LUNIT6,2018) NUK                                           M39. 954
 2018 FORMAT ( 10H   NUK  = ,I6  )                                      M39. 955
 1006 N2 = 0                                                            M19. 317
      MIN = NUK + NIU                                                   M37.2167
      NKN = NUK + LSTAT(64)                                             M39. 956
      DO 2046  K=1, KTAB                                                M19. 318
      IF ( K .GT. MIN  .AND.  K .LE. NKN ) GO TO 2046                   M37.2168
      NDX1 = KLNTAB + K                                                 M22.2457
      N6 = ILNTAB( KLNTAB + K )                                         M38.1004
      N1 = K + 1                                                        M19. 320
      DO 2045  J=N1, KTAB                                               M19. 321
      NDX1 = KLNTAB + J                                                 M22.2459
      IF ( ILNTAB(NDX1)  .NE.  N6 )   GO TO 2045                        M38.1005
      N2 = N2 + 1                                                       M19. 323
      BUS6 = TEXVEC( N6 )                                               M38.1006
      WRITE (LUNIT6, 2041)  K, J, BUS6                                  M19. 324
 2041 FORMAT ( /,   15H ENTRIES NUMBER,  I5,  6H   AND,  I5,            M19. 325
     1  48H   OF THE TACS NAME-VECTOR BOTH HAVE CONTENTS  ',            M19. 326
     2 A6,  3H' .   )                                                   M19. 327
 2045 CONTINUE                                                          M19. 328
 2046 CONTINUE                                                          M19. 329
      IF ( N2  .EQ.  0 )   GO TO 2072                                   M19. 330
      KILL = 189                                                        M19. 335
      LSTAT(15) = N2                                                    M19. 336
      LSTAT(19) = 2059                                                  M19. 337
      GO TO 9000                                                        M19. 338
C                            *****  OUTPUT REQUEST  *****               M38.1007
 2072 IF ( IUTY(KIUTY+6) .NE. 9999 ) GO TO 153                          M39. 957
      IOUTCS = 0                                                        M38.1034
      N1 = NUK + NIU                                                    M38.1035
      N2 = NUK + LSTAT(64)                                              M39. 958
      DO 157  I=1, KTAB                                                 M38.1037
      IF ( I .GT. N1 .AND. I .LE. N2 ) GO TO 157                        M38.1038
      IOUTCS = IOUTCS + 1                                               M38.1039
      NDX1 = KJOUT  + IOUTCS                                            M38.1040
      JOUT(NDX1) = I                                                    M38.1041
  157 CONTINUE                                                          M38.1042
      GO TO 3072                                                        M38.1043
  153 JR = 0                                                            M38.1044
      IF ( IOUTCS .LE. 0 )  GO TO 3712                                  M42.1470
      DO 109 I = 1, IOUTCS                                              M39. 959
      J8 = JOUT( KJOUT + I )                                            M39. 960
      DO 228 J=1,KTAB                                                   M38.1048
      IF ( J8 .EQ. ILNTAB(KLNTAB+J) )  GO TO 229                        M39. 961
  228 CONTINUE                                                          M38.1051
      IF (NOUTPR .EQ. 0) WRITE (LUNIT6, 232 ) TEXVEC(J8)                M39. 962
  232 FORMAT ( /, 49H   THE USER HAS REQUESTED A TACS VARIABLE NAMED ", M38.1053
     1 A6, 55H" FOR OUTPUT PURPOSES. BUT THIS IS A NON-EXISTANT TACS  , M38.1054
     5 /,  47H VARIABLE, SO THE REQUEST WILL BE DISREGARDED.   )        M38.1055
      GO TO 109                                                         M38.1056
  229 JR = JR + 1                                                       M39. 963
      NDX1 = KJOUT + JR                                                 M39. 964
      JOUT(NDX1) = J                                                    M38.1059
  109 CONTINUE                                                          M38.1060
 3712 IOUTCS = JR                                                       M39. 965
      IF ( IOUTCS  .GT.  0 )   GO TO 111                                M38.1065
      IF ( TMAX  .LE.  0.0 )   GO TO 111                                M38.1066
      IF ( NTCSEX+NSTACS  .GT.  0 )   GO TO 111                         M38.1067
      KILL = 140                                                        M38.1068
      LSTAT(19) = 23153                                                 M38.1069
      GO TO 9000                                                        M38.1070
  111 IF ( IPRSUP  .GE.  1 )                                            M38.1075
     1    WRITE (LUNIT6,192) (JOUT(KJOUT+I), I=1,IOUTCS)                M38.1076
  192 FORMAT(44H0PROGRAM OUTPUT WILL BE PREPARED FOR NODES 0/(1X,16I8)) M38.1077
C                             ******   INITIAL CONDITION   **********   M38.1078
 3072 IF ( KXIC .EQ. 0 ) GO TO 231                                      M39. 966
      KXTCS2 = KXTCS + LSTAT(68)                                        M39. 967
      DO 8284 I = 1, KXIC                                               M39. 968
      AWKCS(KAWKCS+I) = XTCS( KXTCS + I )                               M39. 969
 8284 XTCS(KXTCS+I) = 0.0                                               M39. 970
      JR = 0                                                            M39. 971
      DO 4111 I = 1, KXIC                                               M39. 972
      J8 = AWKCS( KAWKCS + I )                                          M39. 973
      DO 411 K=1,KTAB                                                   M37.2187
  411 IF ( J8 .EQ. ILNTAB(KLNTAB+K) )  GO TO 412                        M39. 974
      WRITE ( LUNIT6, 7686 )  TEXVEC(J8)                                M39. 975
 7686 FORMAT ( 45H $$$$$ IGNOR THE INITIAL CONDITION CARD SINCE,        M37.2190
     1  16H NO SUCH NAME ( , A6,  27H ) IN THE TACS TABLE $$$$$ )       M37.2191
      GO TO 4111                                                        M39. 976
  412 JR = JR + 1                                                       M39. 977
      XTCS(KXTCS+K) = XTCS( KXTCS2 + I )                                M39. 978
 4111 CONTINUE                                                          M39. 979
      CALL MOVER0 ( XTCS(KXTCS2+1), KXIC )                              M39. 980
      KXIC = JR                                                         M39. 981
 6195 IF ( KXIC .EQ. 0  .OR.  IPRSUP .LT. 2 )  GO TO 231                M39. 982
      DO 3434 I = 1, KTAB                                               M37.2201
      NDX1 = ILNTAB( KLNTAB + I )                                       M38.1089
 3434 IF ( XTCS(KXTCS+I) .NE. 0.0 )                                     M37.2202
     1 WRITE (LUNIT6,196)  TEXVEC(NDX1), XTCS(KXTCS+I)                  M38.1090
  196 FORMAT (34H  USER-DEFINED INITIAL CONDITION  , A6, E20.8)         M37.2204
  231 IF ( IPLOT  .LT.  0 )   GO TO 9000                                M38.1091
      N1 = 0                                                            M13. 978
      WRITE (LUNIT4) DATE1, TCLOCK, MAXBUS, IOUTCS, N1, N1,             M38.1092
     1 ( TEXVEC(I), I = 1, MAXBUS )                                     M38.1093
      N2 = KCOLCS + 1                                                   M38.1094
      N3 = KCOLCS + IOUTCS                                              M38.1095
      DO 2378 I = N2, N3                                                M38.1096
      N1 = N1 + 1                                                       M38.1097
      J = JOUT( KJOUT + N1 )                                            M38.1098
 2378 ICOLCS(I) = ILNTAB( KLNTAB + J )                                  M38.1099
      WRITE (LUNIT4)  ( ICOLCS(I), I = N2, N3 )                         M38.1100
 9000 IF ( IPRSUP  .GE.  1 )                                            M33. 144
     1 WRITE ( LUNIT6, 4568 )                                           M32.1029
 4568 FORMAT ( 24H  "EXIT  MODULE TACS1B." )                            M32.1030
      RETURN                                                            M33. 145
      END                                                               M13. 985
      SUBROUTINE EXPCHK ( N1, N2, N5 )                                  M09.  10
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      REAL*8         TEXNUM, TEXT1, X                                   M12. 246
      REAL*8         TEXTP, TEXTN                                       M23. 434
      DIMENSION X(80), TEXNUM(11)                                       M23. 435
      DATA  TEXT1   /  1HE  /                                           M08.  99
      DATA TEXTP / 1H+ /                                                M23. 436
      DATA TEXTN / 1H- /                                                M23. 437
      DATA  TEXNUM(1)   /  1H1  /                                       M08. 100
      DATA  TEXNUM(2)   /  1H2  /                                       M08. 101
      DATA  TEXNUM(3)   /  1H3  /                                       M08. 102
      DATA  TEXNUM(4)   /  1H4  /                                       M08. 103
      DATA  TEXNUM(5)   /  1H5  /                                       M08. 104
      DATA  TEXNUM(6)   /  1H6  /                                       M08. 105
      DATA  TEXNUM(7)   /  1H7  /                                       M08. 106
      DATA  TEXNUM(8)   /  1H8  /                                       M08. 107
      DATA  TEXNUM(9)   /  1H9  /                                       M08. 108
      DATA  TEXNUM(10)  /  1H0  /                                       M08. 109
      DATA  TEXNUM(11)  /  1H.  /                                       M23. 438
      DECODE (80, 2618, ABUFF)  ( X(I), I=1, 80 )                       M08. 110
 2618 FORMAT ( 80A1 )                                                   M08. 111
      DO 2625 I = N1, N2, N5                                            M23. 439
      KEY = 0                                                           M23. 440
      DO 2621 J = 1, N5                                                 M23. 441
      N3 = I + J -1                                                     M23. 442
      IF( X(N3) .EQ. TEXT1 ) GO TO 2622                                 M23. 443
      IF( KEY .GT. 0 ) GO TO 2620                                       M23. 444
      DO 2619 K = 1, 11                                                 M23. 445
 2619 IF( X(N3) .EQ. TEXNUM(K) ) KEY = K                                M23. 446
      GO TO 2621                                                        M23. 447
 2620 IF( X(N3) .EQ. TEXTP .OR. X(N3) .EQ. TEXTN ) GO TO 2622           M23. 448
 2621 CONTINUE                                                          M23. 449
      GO TO 2625                                                        M23. 450
 2622 N4 = I + N5 -1                                                    M23. 451
      DO 2623 L = 1, 10                                                 M23. 452
      IF( X(N4) .EQ. TEXNUM(L) ) GO TO 2625                             M23. 453
 2623 CONTINUE                                                          M23. 454
 2624 KILL = 97                                                         M23. 455
      LSTAT(14) = I                                                     M23. 456
      LSTAT(15) = N4                                                    M23. 457
      LSTAT(19) = 2624                                                  M23. 458
      RETURN                                                            M23. 459
 2625 CONTINUE                                                          M23. 460
      RETURN                                                            M23. 461
      END                                                               M08. 132
      SUBROUTINE  INTCHK ( N1, N2, N5 )                                 M08. 133
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      REAL*8         X                                                  M21.  24
      DIMENSION X(80)                                                   M08. 135
      DECODE (80, 2642, ABUFF)  ( X(I), I=1, 80 )                       M08. 136
 2642 FORMAT ( 80A1 )                                                   M08. 137
      N6 = N1 - 1                                                       M08. 138
      DO 2648  I=N1, N2                                                 M08. 139
      IF ( X(I)  .EQ.  BLANK )   GO TO 2648                             M08. 140
      N7 = I - N6                                                       M08. 141
      N8 = N7 / N5                                                      M08. 142
      N3 = N8 * N5                                                      M08. 143
      IF ( N3  .EQ.  N7 )   GO TO 2648                                  M08. 144
      N4 = N6  +  ( N8 + 1 ) * N5                                       M08. 145
      IF ( N4  .GT.  N2 )   GO TO 2645                                  M08. 146
      IF ( X(N4)  .NE.  BLANK )   GO TO 2648                            M08. 147
 2645 KILL = 98                                                         M08. 148
      LSTAT(14) = I                                                     M08. 149
      LSTAT(15) = N4                                                    M08. 150
      BUS1 = X(I)                                                       M08. 151
      LSTAT(19) = 2642                                                  M08. 152
      RETURN                                                            M08. 153
 2648 CONTINUE                                                          M08. 154
      RETURN                                                            M08. 155
      END                                                               M08. 156
      SUBROUTINE DATE44 ( A )                                           M40. 523
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THE PURPOSE OF SUBROUTINE  DATE44  IS TO INTERROGATE THE          M15. 476
C)    INSTALLATION CALENDAR, AND RETURN THE CURRENT DATE THROUGH THE    M15. 477
C)    ARGUMENT OF THE SUBROUTINE.   EIGHT BCD CHARACTERS ARE ALLOWED,   M15. 478
C)    WITH THE FIRST (LEFT) FOUR CHARACTERS TO BE PLACED IN  A(1) ,     M15. 479
C)    AND THE FINAL (RIGHT) FOUR PLACED IN  A(2) .   A STATEMENT LIKE   M15. 480
C)             WRITE (LUNIT6, 4041)  A                                  M17. 205
C)        4041 FORMAT ( 1X, 2A4 )                                       M15. 482
C)    THUS OUTPUTS THE CURRENT DATE AS FIRST THE MONTH, THEN THE DAY,   M15. 483
C)    AND FINALLY THE YEAR, SEPARATED BY SLASHES (MM/DD/YY) .           M15. 484
C)    SUBROUTINE  DATE44  IS OF COURSE INSTALLATION DEPENDENT.          M15. 485
C)    EUROPEAN (OR GENERALLY NON-UNITED-STATES, PERHAPS) USERS OF THIS  M15. 486
C)    PROGRAM MAY WANT TO REVERSE THE ORDER OF APPEARANCE OF THE MONTH  M15. 487
C)    AND THE DAY, IN CONFORMITY WITH ESTABLISHED EUROPEAN USAGE.       M15. 488
C     INSTALLATION-DEPENDENT  EMTP  MODULE WRITTEN FOR THE  DEC         M24. 589
C     VAX-11/780.    'IDATE'  IS A  DEC  SYSTEM SUBROUTINE WHICH        M24. 590
C     RETURNS THE MONTH, DAY, AND YEAR (OF CENTURY) AS THREE  INTEGER*2 M24. 591
C     NUMERICAL VALUES.                                                 M24. 592
      REAL*8        A(2)                                                M24. 593
      INTEGER*2  N1, N2, N3                                             M24. 594
      CALL IDATE ( N1, N2, N3 )                                         M24. 595
      N4 = N1 / 10                                                      M24. 596
      N5 = N1 - 10*N4                                                   M24. 597
      N6 = N2 / 10                                                      M24. 598
      N7 = N2 - 10*N6                                                   M24. 599
      ENCODE (4, 1386, A(1) )   N4, N5, N6                              M24. 600
 1386 FORMAT ( 2I1,  1H/,  1I1  )                                       M24. 601
      ENCODE (4, 1394, A(2) )   N7, N3                                  M24. 602
 1394 FORMAT ( I1,  1H/,  I2  )                                         M24. 603
      RETURN                                                            M40. 524
      END                                                               M40. 525
      SUBROUTINE PFATCH                                                 M40. 526
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS INSTALLATION-DEPENDENT MODULE SERVES TO CONNECT A            M28.2820
C     FILE TO  I/O  CHANNEL  IALTER  (/BLANK/ VARIABLE),                M28.2821
C     BASED ON THE FILE SPECIFICATION CONTAINED ON LAST-                M28.2822
C     READ DATA CARD.   DIFFERENT USAGES INCLUDE:                       M28.2823
C           REPLOT, FILE SPECIFICATION                                  M28.2824
C           START AGAIN, FILE SPECIFICATION                             M28.2825
C     FREE-FORMAT IS HERE REQUIRED, TEMPORARILY.                        M28.2826
C        MODULE WRITTEN SPECIALLY FOR  DEC  VAX-11/780 .                M27. 633
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      BYTE  FILEN(25)                                                   M27. 635
      IF ( M4PLOT .NE. 1 )  GO TO 4519  ! NOT INTERACTIVE EMTP          M35. 692
      WRITE (PROM80, 4504)                                              M35. 693
 4504 FORMAT ( '    SEND VAX DISK FILE NAME:'  )                        M35. 694
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M35. 695
      READ (MUNIT5, 4507)  ( TEXCOL(J), J=1, 30 )                       M35. 696
 4507 FORMAT ( 30A1 )                                                   M35. 697
      TEXCOL(31) = CSEPAR   ! PUT "," TERMINATOR AFTER NAME             M35. 698
      IALTER = LUNIT2   ! CONNECT EMTP TABLES FILE TO THIS UNIT         M35. 699
 4519 N4 = 0                                                            M35. 700
      ENCODE (25, 4523, FILEN(1))                                       M27. 637
 4523 FORMAT ( 25X )                                                    M27. 638
      DO 4532  K=KOLBEG, 80                                             M27. 639
      IF ( TEXCOL(K)  .EQ.  BLANK )   GO TO 4532                        M27. 640
      IF ( TEXCOL(K)  .EQ.  CSEPAR )  GO TO 4536                        M27. 641
      N4 = N4 + 1                                                       M27. 642
      ENCODE (1, 3041, FILEN(N4))  TEXCOL(K)                            M27. 643
 3041 FORMAT ( 80A1 )                                                   M27. 644
 4532 CONTINUE                                                          M27. 645
 4536 NFRFLD = 1                                                        M27. 646
      KOLBEG = K + 1                                                    M27. 647
      N7 = IALTER                                                       M28.2827
      IF ( IPRSUP  .GE.  1 )                                            M28.2828
     1 WRITE (LUNIT6, 4548)  IALTER, FILEN                              M28.2829
 4548 FORMAT ( /, ' READY TO CONNECT FILE TO UNIT  "IALTER" =',         M28.2830
     1  I3,   ' .   "FILEN" =',  25A1  )                                M28.2831
      CLOSE (UNIT=N7)                                                   M28.2832
      OPEN  (UNIT=N7, TYPE='OLD', FORM='UNFORMATTED',                   M28.2833
     1       NAME=FILEN)                                                M27. 652
      IF ( IPRSUP  .GE.  1 )                                            M27. 653
     1 WRITE (LUNIT6, 4561)                                             M27. 654
 4561 FORMAT ( /,  ' SUCCESSFUL FILE OPENING IN  "PFATCH" .' )          M27. 655
      ICAT = 2                                                          M28.2834
      RETURN                                                            M40. 527
      END                                                               M40. 528
