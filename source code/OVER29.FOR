      SUBROUTINE SUBR29                                                 M32.6529
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK29.FOR;/NOLIST'                                           
      INCLUDE  'LABL29.FOR;/NOLIST'                                           
      REAL*8        TEXT                                                M32.6533
      DIMENSION  TEXT(1), ARRAY(1)                                      M32.6534
      EQUIVALENCE  ( KARRAY(1), TEXT(1), ARRAY(1) )                     M32.6535
      IF ( IPRSUP  .GE.  1 )                                            M32.6536
     1 WRITE (LUNIT6, 2371)  LSWTCH, LSIZ12, LBUS                       M38.5418
 2371 FORMAT ( 44H AT TOP OF "SUBR29".  LSWTCH, LSIZ12, LBUS =, 3I8 )   M38.5419
      NVAR = LSWTCH                                                     M32.6539
      IF ( LSIZ12  .GT.  LSWTCH )  NVAR = LSIZ12                        M38.5420
      CALL DIMENS ( KARRAY(1), NCHAIN, TRASH, TRASH )                   M32.6541
      DO 2364  J=1, 9999, 2                                             M35.8579
      IF ( KARRAY(J)  .NE.  0 )   GO TO 2364                            M35.8580
      MAXO29 = KARRAY(J+1)                                              M35.8581
      GO TO 2368                                                        M35.8582
 2364 CONTINUE                                                          M35.8583
      CALL STOPTP                                                               
 2368 IOFBUS = 1                                                        M35.8585
      IOFKMS  =  ( IOFBUS + 2 * LBUS ) * NBYTE(1) / NBYTE(4)            M32.6544
      IOFKDE  =  IOFKMS  +  2 * LSWTCH                                  M32.6545
      IOFIBR  =  IOFKDE  +  LSWTCH                                      M32.6546
      IOFJBR  =  IOFIBR  +  LSIZ12                                      M38.5421
      IOFAKE  =  ( IOFJBR + LSIZ12 ) * NBYTE(4) / NBYTE(3)  +  1        M38.5422
      IOFTST  =  IOFAKE  +  LSWTCH                                      M32.6549
      IOFTCL  =  IOFTST  +  LSWTCH                                      M32.6550
      IOFTOP  =  IOFTCL  +  LSWTCH                                      M32.6551
      IOFARR  =  IOFTOP  +  2*LSWTCH                                    M32.6552
      N9  =  IOFARR * NBYTE(3) / NBYTE(4)                               M32.6553
      IF ( IPRSUP  .GE.  2 )                                            M32.6554
     1 WRITE (LUNIT6, 2388)  NVAR,  ( NBYTE(J), J=1, 4 ),  MAXO29,      M32.6555
     2                  IOFBUS, IOFKMS, IOFKDE, IOFIBR, IOFJBR, IOFAKE, M32.6556
     3                  IOFTST, IOFTCL, IOFTOP, IOFARR, N9              M32.6557
 2388 FORMAT (                                                          M32.6558
     1  46H AFTER OFFSET COMPUTATION.  NVAR, NBYTE(1:4) =,  5I5,        M32.6559
     1     5X,  8HMAXO29 =,  I8                                         M32.6560
     2  ,/, 41H   IOFBUS  IOFKMS  IOFKDE  IOFIBR  IOFJBR,               M32.6561
     3      40H  IOFAKE  IOFTST  IOFTCL  IOFTOP  IOFARR  ,/, 1X, 10I8 ) M32.6562
      IF ( N9  .LT.  MAXO29 )  GO TO 3462                               M32.6563
      WRITE (LUNIT6, 3457)  N9, MAXO29                                  M32.6564
 3457 FORMAT (                                                          M32.6565
     1  /,  48H TEMPORARY ERROR STOP IN "SUBR29" OF OVERLAY 29.,        M32.6566
     2      48H   WORKING STORAGE OF /C29B01/ ("VARDIM" OUTPUT),        M32.6567
     3      20H MUST EQUAL AT LEAST,  I7,   8H   WORDS                  M32.6568
     4  ,/, 49H TO SOLVE THE PROBLEM, ALTHOUGH USER-DIMENSIONING,       M32.6569
     5      18H ONLY PROVIDED FOR,  I7,     14H   WORDS (BOTH,          M32.6570
     6      42H INTEGER FIGURES).   REDIMENSION THE EMTP.    )          M32.6571
      CALL STOPTP                                                               
 3462 CALL GUTS29 ( TEXT(IOFBUS), KARRAY(IOFKMS), KARRAY(IOFKDE),       M32.6573
     1             KARRAY(IOFIBR), KARRAY(IOFJBR), ARRAY(IOFAKE),       M32.6574
     2              ARRAY(IOFTST),  ARRAY(IOFTCL), ARRAY(IOFTOP)  )     M32.6575
      IF ( IPRSUP  .GE.  1 )                                            M32.6576
     1 WRITE (LUNIT6, 3489)  IOFARR, KILL                               M32.6577
 3489 FORMAT (  37H AT EXIT OF "SUBR29".  IOFARR, KILL =,  2I8  )       M32.6578
      RETURN                                                            M32.6579
      END                                                               M32.6580
      SUBROUTINE GUTS29  ( BUS, KMSWIT, KDEPSW, IBRNCH, JBRNCH,         M32.6581
     1                     AKEY, TSTAT, TCLOSE, TOPEN  )                M32.6582
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      REAL*8        BUS                                                 M32.6583
      DIMENSION BUS(1), KMSWIT(1), KDEPSW(1), IBRNCH(1), JBRNCH(1)      M32.6584
      DIMENSION AKEY(1), TSTAT(1), TCLOSE(1), TOPEN(1)                  M32.6585
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK29.FOR;/NOLIST'                                           
      INCLUDE  'LABL29.FOR;/NOLIST'                                           
      DIMENSION ARRAY(1)                                                M32.6586
      EQUIVALENCE ( ARRAY(1), KARRAY(1) )                               M32.6587
      EQUIVALENCE  (MONCAR(1),    KNT),   (MONCAR(2),  KBASE)           M37.7919
      EQUIVALENCE  (MONCAR(5),  IDIST),   (MONCAR(6),  ITEST)           M37.7920
      EQUIVALENCE  (MONCAR(9), KLOAEP)                                  M37.7921
      DIMENSION  LLTEMP(20), MMTEMP(50)                                 M42.7798
      REAL*8        TEXT1, TEXT2, TEXT3                                 M35.8586
      REAL*8        TEXT5, TEXT6, TEXT7                                 M32.6589
      DATA  TEXT1  / 6HMISC.  /                                         M32.6590
      DATA  TEXT2  / 6HSTATIS /                                         M32.6591
      DATA  TEXT3  / 6HTICS D /                                         M32.6592
      DATA  TEXT5  / 6HBEGIN  /                                         M32.6594
      DATA  TEXT6  / 6HNEW DA /                                         M32.6595
      DATA  TEXT7  / 6HBNDC   /                                         M32.6596
      IF ( IPRSUP  .GE.  1 )                                            M32.6597
     1 WRITE (LUNIT6, 4823)  IOFBND, KBURRO, ISTEP                      M32.6598
 4823 FORMAT ( 42H TOP OF "GUTS29".  IOFBND, KBURRO, ISTEP =,           M33.2105
     1  3I8  )                                                          M32.6600
      N18 =  MAXO29 * NBYTE(4)/NBYTE(3)  -  IOFARR                      M32.6601
      IF ( ISTEP  .NE.  -6633 )  GO TO 5627                             M42.7799
C     BEGIN "FAULT DATA USAGE" (FROM "REQUES" OF OVERLAY 1):            M32.6603
C     NEXT SOLVE FOR MAXIMUM DIMENSION X:  2(X)**2 + 4(X) - N18 = 0     M32.6604
      D18 = 1 + N18 / 2                                                         
      N3 = SQRTZ ( D18 )  -  1.0                                                
      D7 = N3                                                           M32.6607
      N2 = SQRTZ ( D7 )                                                         
      IF ( IOFBND .LE. 0 )  IOFBND = N2                                 M32.6609
      IF ( IOFBND  .LE.  N2 )  GO TO 4839                               M32.6610
      WRITE (6, 4832)  IOFBND, N2                                       M32.6611
 4832 FORMAT (  41H INSUFFICIENT WORKING SPACE FROM "VARDIM",           M32.6612
     1          37H DIMENSIONING TO HONOR USER-REQUESTED,               M32.6613
     2          34H MAXIMUM NUMBER OF BUSSES EQUAL TO,  I5,  2H .       M32.6614
     3   ,/,    45H USER CAN RERUN WITH REDUCED MAXIMUM EQUAL TO,       M32.6615
     4   I5,     2H .    )                                              M32.6616
      CALL STOPTP                                                               
 4839 N3 = IOFBND**2                                                            
      N7 = N3**2                                                                
      WRITE (LUNIT6, 4847)  IOFBND                                              
 4847 FORMAT (  1H+, 40X,   5HMAX =,  I3  )                             M32.6621
      IOFRAY = 1                                                                
      IOFSAV = IOFRAY + N7                                                      
      IOFSOL = IOFSAV + N7                                                      
      IOFRHS = IOFSOL + N3                                                      
      IOFMAT = IOFRHS + N3                                                      
      IOFKSP = IOFMAT + 2*N3                                                    
      IOFKSP = IOFKSP * NBYTE(4) / NBYTE(3) + 1                                 
      N8  =  IOFKSP + N7                                                        
      LSPARS = 1                                                                
      IF ( N8 .LT. LOOPSS(2) )                                                  
     1 LSPARS = N3                                                              
      N4 = 2 * IOFBND                                                           
      IF ( IPRSUP .GE. 1 )                                              M32.6634
     1 WRITE (LUNIT6, 4851)  IOFSAV, IOFSOL, IOFRHS, IOFMAT,            M32.6635
     2              IOFKSP,  N3, N4, IOFBND, LSPARS                     M32.6636
 4851 FORMAT (  41H IOFSAV, IOFSOL, IOFRHS, IOFMAT, IOFKSP =,  5I6      M32.6637
     1   ,/,    38H ARRAY DIMENSIONS N3, N4, N2, LSPARS =,  4I6  )      M32.6638
      CALL FLTDAT ( ARRAY(IOFRAY), ARRAY(IOFSAV), ARRAY(IOFSOL),        M32.6639
     1           ARRAY(IOFRHS), ARRAY(IOFMAT), KARRAY(IOFKSP),          M32.6640
     2              N3, N4, IOFBND, LSPARS )                            M32.6641
      GO TO 9800                                                                
C     END "FAULT DATA USAGE" SERVICING;  BEGIN MONTE CARLO:             M32.6643
C     REMOVE POSSIBLE MINUS SIGN FROM  'XMAXMX'  (INDICATING            M28.6617
C     USER-REQUEST FOR BUILT-IN RANDOM NUMBERS OF  "SANDNM" ).          M28.6618
 5627 XMAXMX = ABSZ ( XMAXMX )                                          M32.6644
C     RESET 'AINCR' IF BIAS OF  55  IS PRESENT (INDICATING              M27.2653
C     STATISTICS JUST FOR MINIMA?).                                     M27.2654
      IF (AINCR .GE. 55.0)  AINCR = AINCR - 55.0                        M27.2655
      IF (AINCR .EQ. 0.0)  AINCR = .05                                  M27.2656
      KNT = 0                                                           M35.8587
      NSTAT = LSTAT(32)                                                 M32.6645
      IF ( LASTOV  .GT.  1 )   GO TO 6309                               M23.5356
      KNT = 1                                                           M35.8588
      NENERG = 0                                                        M23.5357
      N6 = 0                                                            M23.5358
      N7 = 0                                                            M23.5359
      N8 = 0                                                            M23.5360
C     READ INPUT CARD USING CIMAGE.                                     M23.5361
 6227 CALL CIMAGE                                                       M23.5362
      IF ( KOLBEG  .GT.  0 )   GO TO 6263                               M23.5363
      DECODE (80, 6234, ABUFF(1) )   ( LLTEMP(I), I=1, 10 )             M23.5364
 6234 FORMAT ( 10I8 )                                                   M23.5365
      DO 6239  I=1, 10                                                  M23.5366
      IF ( LLTEMP(I)  .EQ.  0 )   GO TO 6239                            M23.5367
      IF ( LLTEMP(I)  .NE.  9999 )   GO TO 6236                         M23.5368
      N8 = 9999                                                         M23.5369
      GO TO 6243                                                        M23.5370
 6236 N7 = N7 + 1                                                       M23.5371
      IF ( N7 .GT. 50 )  GO TO 6270                                             
      MMTEMP(N7) = LLTEMP(I)                                            M36.3564
 6239 CONTINUE                                                          M23.5373
 6243 WRITE (KUNIT6, 6249)  ( LLTEMP(I), I=1, 9 )                       M35.8589
 6249 FORMAT ( 12H+FILE NAMES.,  9I4  )                                 M23.5375
      IF ( N8  .EQ.  9999 )   GO TO 6294                                M23.5376
      GO TO 6227                                                        M23.5377
 6263 NFRFLD = 1                                                        M23.5378
 6269 CALL FREONE ( D7 )                                                M29.2670
      IF ( D7  .EQ.  9999. )   GO TO 6282                               M23.5380
      IF ( D7  .EQ.  0.0 )   GO TO 6273                                 M23.5381
      N7 = N7 + 1                                                       M23.5382
      IF ( N7 .LE. 50 )  GO TO 6272                                             
 6270 WRITE (LUNIT6, 6271)                                              M36.3566
 6271 FORMAT ( 34H TEMPORARY ERROR STOP IN "GUTS29".,                   M36.3567
     1         32H   OVERFLOW  MMTEMP(50).   STOP.     )                M36.3568
      CALL STOPTP                                                               
 6272 MMTEMP(N7) = D7                                                   M36.3570
 6273 GO TO 6269                                                        M23.5384
 6282 DO 6286  I=1, 9                                                   M23.5385
 6286 LLTEMP(I) = MMTEMP(I)                                             M36.3571
      WRITE (KUNIT6, 6249)  ( LLTEMP(I), I=1, 9 )                       M35.8590
 6294 DO 1320  M=1, N7                                                  M31.3616
      N13 = MMTEMP(M)                                                   M36.3572
      N2 = N13/100                                                      M23.5391
      N5 = N13 - 100 * N2                                               M23.5392
      N3 = N5 / 10                                                      M23.5393
      N4 = N5 - 10*N3                                                   M23.5394
      IF ( IPRSUP  .GE.  1 )                                            M23.5395
     1 WRITE(LUNIT6, 6299) M, N7, N13, N2, N3, N4, LUNIT3,              M31.3618
     2                       LUNIT6, LUNIT9                             M23.5397
 6299 FORMAT ( /, 33H IN  'GUTS29' ,  READY TO ATTACH.,                 M33.2106
     1  40H       M      N7     N13      N2      N3,                    M31.3619
     2  32H      N4  LUNIT3  LUNIT6  LUNIT9  ,/,  33X,  9I8  )          M23.5400
      LSTAT(14) = N2                                                    M23.5401
      LSTAT(15) = N3                                                    M23.5402
      LSTAT(16) = N4                                                    M23.5403
      LSTAT(13) = 9                                                     M31.3620
      CALL STATRS                                                       M23.5405
      READ(LUNIT1)  NTOT, NENI                                          M31.3621
      READ(LUNIT1) PU                                                   M35.8591
      NPNI = IABSZ(NENI)                                                M31.3622
      NENERG = NENERG + NENI                                            M31.3623
      NPNERG = IABSZ(NENERG)                                            M31.3624
      IF ( IPRSUP  .GE.  3 )                                            M31.3625
     1 WRITE(LUNIT6, 1848)  NTOT, NENI                                  M31.3626
 1848 FORMAT (/,  34H AFTER 2-INTEGER READ FROM LUNIT1.,                M31.3627
     1  16H    NTOT    NENI,/, 34X, 2I8)                                M31.3628
      NEXOUT(M) = NTOT                                                  M33.2107
      IF (M .NE. 1)  GO TO 260                                          M32.6651
      WRITE(LUNIT9)  NTOT                                               M31.3631
  260 READ (LUNIT1)  DATE1, TCLOCK, IVOLT, N6, NDX1, NDX2, N16,         M32.6652
     1     NSMOUT, IOUTCS, NUMOUT, N18, NUMNVO, NUMBCO, NC, KSWTCH      M32.6653
      NV = NC - NUMBCO                                                  M35.8592
      IF ( M .NE. 1 )  GO TO 270                                        M32.6654
      WRITE (LUNIT9)  DATE1, TCLOCK, IVOLT, N6, NDX1, NDX2, N16,        M32.6655
     1     NSMOUT, IOUTCS, NUMOUT, N18, NUMNVO, NUMBCO, NC, KSWTCH      M32.6656
  270 J1 = NTOT + 1                                                     M32.6657
      J2 = NTOT + N6                                                    M32.6658
      J3 = J2 + NDX1                                                    M32.6659
      J4 = J2 + NDX2                                                    M32.6660
      J5 = J4 + 1                                                       M32.6661
      J6 = J4 + N16                                                     M32.6662
      READ (LUNIT1)  (BUS(I), I=1, NTOT),                               M32.6663
     1               (BUS(I),I=J1,J2),                                  M32.6664
     2    ( BUS(I),I=J3,J4), (BUS(I), I=J5,J6 )                         M32.6665
      IF ( M .NE. 1 )  GO TO 275                                        M32.6666
      WRITE (LUNIT9)  (BUS(I), I=1, NTOT),                              M32.6667
     1               (BUS(I),I=J1,J2),                                  M32.6668
     2    ( BUS(I),I=J3,J4), (BUS(I), I=J5,J6 )                         M32.6669
  275 IF ( IPRSUP .GE. 1 )                                              M32.6670
     1 WRITE (LUNIT6, 3207)  NC,  NSMOUT, IOUTCS, IVOLT, NDX1,          M32.6671
     2           NDX2,N16, NUMOUT,N18,NUMNVO,NUMBCO,NC                  M32.6672
 3207 FORMAT ( 27H INTEGERS WRITTEN ON LUNIT9, 5X, 12I8 )               M32.6673
      TEXTA6(M) = BUS(NTOT)                                             M32.6674
      IF ( IVOLT  .NE.  1 )   GO TO 4103                                M32.6675
      READ (LUNIT1)   ( M13, I=2, NTOT )                                M34. 166
      IF ( M .NE. 1 )  GO TO 4104                                       M32.6677
      WRITE (LUNIT9)   ( I, I=2, NTOT )                                 M32.6678
      GO TO 4104                                                        M32.6679
 4103 IF( NUMNVO .GT. 0 )                                               M32.6680
     1 READ (LUNIT1)  ( KARRAY(I), I=1, NUMNVO )                        M32.6681
      IF ( M .NE. 1 )  GO TO 4104                                       M32.6682
      IF( NUMNVO .GT. 0 )                                               M32.6683
     1 WRITE (LUNIT9)  ( KARRAY(I), I=1, NUMNVO )                       M32.6684
 4104 IF( NC .GT. 0 )                                                   M32.6685
     1 READ (LUNIT1)  (IBRNCH(I), I = 1, NC), (JBRNCH(I), I = 1, NC)    M32.6686
      IF ( M .NE. 1 )  GO TO 285                                        M32.6687
      IF( NC .GT. 0 )                                                   M32.6688
     1 WRITE (LUNIT9)  (IBRNCH(I), I = 1, NC), (JBRNCH(I), I = 1, NC)   M32.6689
  285 NSTAT = NC + NUMNVO                                               M32.6690
      IF ( M .EQ. 1 )  LSTAT(32) = NSTAT                                M32.6691
      IPNTV(M) = NSTAT                                                  M32.6692
      INDTV(M) = KSWTCH                                                 M32.6693
      IF (M .EQ. 1)  GO TO 310                                          M32.6694
      N2 = M - 1                                                        M32.6695
      IF (NEXOUT(M) .NE. NEXOUT(N2) ) GO TO 9200                        M32.6696
      IF (TEXTA6(M) .NE. TEXTA6(N2) ) GO TO 9200                        M32.6697
      IF ( IPNTV(M) .NE. IPNTV(N2) ) GO TO 9200                         M32.6698
      IF (INDTV(M) .NE. INDTV(N2) ) GO TO 9200                          M32.6699
  310 DO 320  JJ=1, NPNI                                                M31.3662
      READ (LUNIT1)  ( VOLTBC(J), J=1, NSTAT )                          M31.3663
      IF (IPRSUP .GE. 3)                                                M31.3664
     1 WRITE (LUNIT6, 1310)   ( VOLTBC(J), J=1, NSTAT )                 M31.3665
 1310 FORMAT ( 22H  VOLTBC  FROM LUNIT1., /, (1X, 8E15.6) )             M31.3666
      WRITE (LUNIT9)  ( VOLTBC(J),  J=1, NSTAT )                        M31.3667
  320 CONTINUE                                                          M31.3669
  330 LSTAT(13) = 3                                                     M31.3670
      CALL STATRS                                                       M31.3671
      DO 340  I=1, KSWTCH                                               M32.6700
      J = LSWTCH + I                                                    M32.6701
      READ (LUNIT1) KMSWIT(I),KMSWIT(J),AKEY(I),TSTAT(I)                M32.6702
     1 , TOPEN(J), KDEPSW(I)                                            M32.6703
      IF (M .NE. 1)  GO TO 340                                          M32.6704
      WRITE(LUNIT3) KMSWIT(I),KMSWIT(J),AKEY(I),TSTAT(I)                M32.6705
     1 , TOPEN(J), KDEPSW(I)                                            M32.6706
  340 CONTINUE                                                          M32.6707
      READ (LUNIT1) KLOAEP                                              M32.6708
      IF ( M .NE. 1 )   GO TO 410                                       M32.6709
      WRITE (LUNIT3)  KLOAEP                                            M32.6710
  410 DO 420  JJ=1, NPNI                                                M31.3677
      READ(LUNIT1)  RANOFF, (TCLOSE(I), I=1, KSWTCH)                    M32.6711
      WRITE(LUNIT3)  RANOFF, (TCLOSE(I), I=1, KSWTCH)                   M32.6712
      READ(LUNIT1)  RANOFF, (TOPEN(I), I=1, KSWTCH)                     M32.6713
      WRITE(LUNIT3)  RANOFF, (TOPEN(I), I=1, KSWTCH)                    M32.6714
  420 CONTINUE                                                          M31.3681
 1320 CONTINUE                                                          M31.3682
      LSTAT(13) = 0                                                     M31.3683
      CALL STATRS                                                       M31.3684
 6309 IF ( KBASE .EQ. 29 )  GO TO 1621                                  M33.2108
C     READ INPUT CARD USING CIMAGE                                      M32.6716
 6315 CALL CIMAGE                                                       M32.6717
 6319 CONTINUE                                                          M32.6718
C     CHECK FOR TRUNCATED STATISTICS MISC. DATA CARD:                   M32.6719
      DECODE (80, 1620, ABUFF) BUS1, BUS2, BUS3, BUS4                   M32.6720
 1620 FORMAT ( 40X, 4A6 )                                               M32.6721
      IF ( BUS1 .NE. TEXT1 )  GO TO 6369                                M32.6722
      IF ( BUS2 .NE. TEXT2 )  GO TO 6369                                M32.6723
      IF ( BUS3 .NE. TEXT3 )  GO TO 6369                                M32.6724
      KBASE = 29                                                        M33.2109
 1621 CONTINUE                                                          M33.2110
      DECODE (80,9620, ABUFF)    D1, D2                                 M32.6725
 9620 FORMAT ( 24X, 2E8.0 )                                             M32.6726
      IF ( D1 .NE. 0.0 )  AINCR = D1                                    M32.6727
      IF ( D2 .NE. 0.0 )  XMAXMX = D2                                   M32.6728
      WRITE (KUNIT6, 9630)   ITEST, IDIST, AINCR, XMAXMX                M35.8593
 9630 FORMAT ( 17H+STATISTICS DATA., 2I7, 2F9.4 )                       M32.6730
      GO TO 6315                                                        M32.6731
C     CHECK FOR "BEGIN NEW DATA CASE" OR "BNDC" CARD:                   M32.6732
 6369 CONTINUE                                                          M32.6733
      DECODE (80, 1605, ABUFF)  BUS1, BUS2, BUS3, BUS4                  M33.2111
 1605 FORMAT ( 13A6 )                                                   M32.6735
      IF ( BUS1 .EQ. TEXT5  .AND.  BUS2 .EQ. TEXT6 )  GO TO 16379       M33.2112
       IF ( BUS1 .EQ. TEXT7  .AND.  BUS2 .EQ. BLANK )  GO TO 16379      M33.2113
      IF ( BUS1 .EQ. BLANK  .AND. BUS2 .EQ. BLANK  .AND.                M33.2114
     1     KBASE .EQ. 28 )  GO TO 16379                                 M33.2115
       GO TO 6379                                                       M33.2116
16379 WRITE (KUNIT6, 1608)                                              M35.8594
 1608 FORMAT( 44H+NEW-CASE MARKER CARD BOUNDS THIS-CASE DATA. )         M32.6742
      GO TO 9900                                                        M33.2118
 6379 NTEMP = NSTAT                                                     M32.6743
      IF ( NSTAT .LT. KSWTCH)  NTEMP = KSWTCH                           M32.6744
      NARRAY = IABS(NENERG) * (NTEMP + 1 ) * 2                          M32.6745
      IF ( AINCR .GE. 0.0 )  GO TO 6389                                 M32.6746
      LIMINC = ABSZ(AINCR)                                              M32.6747
      GO TO 6399                                                        M32.6748
 6389 XMAXMX = 5.0 * XMAXMX                                             M32.6749
      LIMINC = XMAXMX/AINCR + 1.0                                       M13.3004
 6399 IOFIBS = (IOFARR + NARRAY) * NBYTE(3)/NBYTE(4)                    M34. 167
      IOFKOU = IOFIBS + NUMNVO + NC + NC                                M13.3014
      IOFPNT = IOFKOU + LIMINC                                          M22.5470
      IOFNSU = IOFPNT + LIMINC                                          M22.5471
      N2 = IOFNSU + LIMINC - 1                                          M22.5472
      IF ( IPRSUP  .GE.  1 )                                            M22.5473
     1 WRITE (LUNIT6, 5672)  MAXO29, NSTAT, KSWTCH, NENERG, NUMNVO, NC, M32.6751
     1 LIMINC, N2, XMAXMX, AINCR                                        M22.5475
 5672 FORMAT ( /,  20H BEFORE CALL INNR29.,                             M32.6752
     1 32H  MAXO29   NSTAT  KSWTCH  NENERG,                             M32.6753
     2 32H  NUMNVO      NC  LIMINC      N2,  9X,  6HXMAXMX,             M22.5478
     3 10X,  5HAINCR  ,/,  20X,  8I8,  2E15.6  )                        M22.5479
      IF ( N2  .LE.  MAXO29 )   GO TO 20                                M32.6754
      KILL=88                                                           M13.3025
      LSTAT(15) = N2                                                    M20.4896
      LSTAT(16) = MAXO29                                                M33.2119
      LSTAT(13) = IABS(NENERG) + 3                                      M20.4899
      LSTAT(14) = LIMINC                                                M20.4900
      LSTAT(19) = 20                                                    M13.3027
      GO TO 9800                                                        M32.6755
   20 CALL INNR29 ( ARRAY(IOFARR), KARRAY(IOFIBS),                      M34. 168
     1       KARRAY(IOFKOU), KARRAY(IOFPNT), KARRAY(IOFNSU),            M34. 169
     2     BUS(1), KMSWIT(1), KDEPSW(1), IBRNCH(1), JBRNCH(1),          M32.6758
     3     AKEY(1), TSTAT(1), TCLOSE(1), TOPEN(1)  )                    M32.6759
      IF ( KBASE .EQ. 29 )  GO TO 6319                                  M33.2120
      IF ( KBASE .EQ. 28 )  GO TO 6315                                  M33.2121
 9200 KILL = 206                                                        M31.3688
      BUS1 = TEXTA6(M)                                                  M33.2122
      BUS2 = TEXTA6(N2)                                                 M32.6762
      LSTAT(11) = NEXOUT(N2)                                            M31.3691
      LSTAT(12) = IPNTV(N2)                                             M31.3692
      LSTAT(13) = INDTV(N2)                                             M31.3693
      LSTAT(14) = NSTAT                                                 M31.3694
      LSTAT(19) = 300                                                   M31.3695
 9800 LSTAT(18) = NCHAIN                                                M32.6763
      LASTOV = NCHAIN                                                   M32.6764
      NCHAIN = 51                                                       M32.6765
 9900 IF ( IPRSUP  .GE.  1 )                                            M32.6766
     1 WRITE (LUNIT6, 9903)  KILL, NCHAIN, N18                          M32.6767
 9903 FORMAT ( 36H EXIT "GUTS29".  KILL, NCHAIN, N18 =,  3I6  )         M33.2123
      RETURN                                                            M32.6769
      END                                                               M13.3035
      SUBROUTINE INNR29 ( ARRAY, IBSOUT, KOUNT, KPOINT, NSUM,           M32.6770
     1                    BUS, KMSWIT, KDEPSW, IBRNCH, JBRNCH,          M32.6771
     2                    AKEY, TSTAT, TCLOSE, TOPEN  )                 M32.6772
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  ARRAY(1), IBSOUT(1), KOUNT(1), KPOINT(1)               M32.6773
      DIMENSION  NSUM(1), BUS(1), KMSWIT(1), KDEPSW(1)                  M32.6774
      DIMENSION  IBRNCH(1), JBRNCH(1), AKEY(1)                          M32.6775
      DIMENSION  TSTAT(1), TCLOSE(1), TOPEN(1)                          M32.6776
      REAL*8        BUS                                                 M32.6777
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK29.FOR;/NOLIST'                                           
      INCLUDE  'LABL29.FOR;/NOLIST'                                           
      DIMENSION  KOUNT1(25), KOUNT2(25), KOUNT3(25)                     M13.3046
      DIMENSION  NORMAL(25), ITEMP(264), TEXNAM(10)                     VAX.2984
      EQUIVALENCE  (MONCAR(1),    KNT),   (MONCAR(2),  KBASE)           M37.7924
      EQUIVALENCE                         (MONCAR(4),    ISW)           M37.7925
      EQUIVALENCE  (MONCAR(5),  IDIST),   (MONCAR(6),  ITEST)           M37.7926
      EQUIVALENCE  (MONCAR(9), KLOAEP)                                  M37.7927
      REAL*8        TEXT1, TEXT2, TEXT3, TEXT4, TEXT5, TEXT6            M32.6778
      REAL*8        TEXT7, TEXT8, TEXNAM, AUPPER                        M38.5424
      DIMENSION AUPPER(264)                                             VAX.2985
      DATA  TEXT1  / 6HMISC.  /                                         M32.6781
      DATA  TEXT2  / 6HSTATIS /                                         M32.6782
      DATA  TEXT3  / 6HTICS D /                                         M32.6783
      DATA  TEXT4  / 6HATA    /                                         M32.6784
      DATA  TEXT5  / 6HBEGIN  /                                         M32.6785
      DATA  TEXT6  / 6HNEW DA /                                         M32.6786
      DATA  TEXT7  / 6HBNDC   /                                         M32.6787
      DATA  TEXT8  / 6H CONT. /                                         M38.5426
      DATA  TEXNAM(1)   /  6HVOLTAG  /                                  M35.8597
      DATA  TEXNAM(2)   /  6HE       /                                  M35.8598
      DATA  TEXNAM(3)   /  6HCURREN  /                                  M35.8599
      DATA  TEXNAM(4)   /  6HT       /                                  M35.8600
      DATA  TEXNAM(5)   /  6H POWER  /                                  M35.8601
      DATA  TEXNAM(6)   /  6H        /                                  M35.8602
      DATA  TEXNAM(7)   /  6HENERGY  /                                  M35.8603
      DATA  TEXNAM(8)   /  6H        /                                  M35.8604
      DATA  TEXNAM(9)   /  6H NODE   /                                  M35.8605
      DATA  TEXNAM(10)  /  6HBRANCH  /                                  M35.8606
      DATA NORMAL(1), NORMAL(2), NORMAL(3),NORMAL(4) / 0, 0, 0, 0 /     M13.3048
      DATA NORMAL(5), NORMAL(6), NORMAL(7), NORMAL(8) / 1, 2, 4, 8 /    M13.3049
      DATA NORMAL(9),NORMAL(10),NORMAL(11),NORMAL(12) / 12,18,24,28 /   M13.3050
      DATA NORMAL(13),NORMAL(14),NORMAL(15),NORMAL(16) / 30,28,24,18 /  M13.3051
      DATA NORMAL(17), NORMAL(18), NORMAL(19), NORMAL(20) / 12,8,4,2 /  M13.3052
      DATA NORMAL(21), NORMAL(22), NORMAL(23), NORMAL(24) / 1, 0, 0, 0 /M13.3053
      DATA NORMAL(25) / 0 /                                             M13.3054
 4380 IF ( IPRSUP .GE. 1 )                                              M32.6788
     1 WRITE (LUNIT6, 4381)  ISW, LASTOV, LUNIT9                        M32.6789
 4381 FORMAT ( 40H TOP OF "INNR29".  ISW, LASTOV, LUNIT9 =,  3I8 )      M33.2124
      LLM1 = -1                                                         M35.8607
      JDUMMY = 999                                                      M13.3056
      IN1 = 1                                                           M13.3057
      IN5 = 5                                                           M13.3058
      IN10 = 10                                                         M13.3059
      IN15 = 15                                                         M13.3060
      IN20 = 20                                                         M13.3061
      IN25 = 25                                                         M13.3062
      IN30 = 30                                                         M13.3063
      LL264 = 264                                                               
      ICNOVF = 0                                                                
      NPNERG = IABS(NENERG)                                             M13.3064
      TOTAL = NPNERG                                                    M13.3065
      ICONT = 0                                                         M38.5428
      N22 = 0                                                           VAX.2987
      N9 = NV + 1                                                       M32.6791
      DD1 = ALOG1Z(FLTINF)                                              M39.3289
      INFEXP = DD1 + 0.5                                                M39.3290
      IF (ISW .EQ. 4444)  GO TO 399                                     M14.2610
      KNTOUT = 0                                                        M13.3066
      K9 = 0                                                            M32.6792
      REWIND LUNIT9                                                     M17. 987
      IF ( KNT .EQ. 1 ) GO TO 7                                         M35.8609
      READ(LUNIT9)  NTOT, NENERG                                        M23.5412
      GO TO 9                                                           M23.5413
    7 READ (LUNIT9)  NTOT                                               M23.5414
    9 IF ( JFLSOS .GT. 0 )  READ (LUNIT9)  PU                           M35.8610
C     READ (LUNIT9)  DATE1, TCLOCK, IVOLT, N6, NDX1, NDX2, N16,         M42.7804
C    1        NSMOUT, IOUTCS, NUMOUT, N18, NUMNVO, NUMBCO, NC, KSWTCH   M42.7805
C     READ (LUNIT9)  (BUS(I), I=1, NTOT),                               M42.7807
C    1               (BUS(I), I=J1, J2),                                M42.7808
C    2      ( BUS(I),I=J3,J4), (BUS(I), I=J5,J6 )                       M42.7809
C     write (*,*) ' INNR29.  Ready for LUNIT9 READ.  J2 =', J2          M42.7810
      READ (LUNIT9)  DATE1, TCLOCK, IVOLT, MAXBUS, NDX1, NDX2, N16,     M42.7811
     1     NSMOUT, IOUTCS, NUMOUT, N18, NUMNVO, NUMBCO, NC, KSWTCH      M42.7812
      IF ( IPRSUP .GE. 3)
     1  write (LUNIT6,*) ' INNR29.  NTOT, MAXBUS, N16 =',                 
     1                       NTOT, MAXBUS, N16                          M43.  27
      J2 = NTOT + MAXBUS + N16                                          M43.  28
      READ (LUNIT9)  ( BUS(I), I=1, J2 )                                M42.7814
      IF ( IPRSUP  .GE.  1 )                                            M32.6804
     1 WRITE (LUNIT6, 3207)  NC,  NSMOUT, IOUTCS, IVOLT, NDX1,          M32.6805
     2           NDX2, N16, NUMOUT, N18, NUMNVO, NUMBCO, NC             M32.6806
 3207 FORMAT ( 26H INTEGERS READ FROM LUNIT9, 5X, 12I8 )                M32.6807
      IF ( IVOLT  .NE.  1 )  GO TO 4103                                 M32.6808
      READ (LUNIT9)   ( M13, I=2, NTOT )                                M34. 170
      GO TO 4104                                                        M32.6810
 4103 IF( NUMNVO .GT. 0 )                                               M32.6811
     1 READ (LUNIT9)  ( IBSOUT(I), I=1, NUMNVO )                        M32.6812
 4104 IF( NC .GT. 0 )                                                   M32.6813
     1 READ (LUNIT9)  (IBRNCH(I), I = 1, NC), (JBRNCH(I), I = 1, NC)    M32.6814
      NSTAT = LSTAT(32)                                                 M32.6815
      N1 = 1                                                            M32.6816
      N2 = NSTAT                                                        M32.6817
      I0 = 0                                                            M32.6818
   35 I0 = I0 + 1                                                       M32.6819
      IF ( I0 .GT. NPNERG )  GO TO 4108                                 M32.6820
      READ (LUNIT9) ( ARRAY(J), J=N1, N2 )                              M32.6821
      IF ( IPRSUP .GE. 6 )                                              M32.6822
     1 WRITE (LUNIT6, 36)  N1, N2, ( ARRAY(J), J=N1, N2 )               M32.6823
   36 FORMAT ( /,  34H READ  ARRAY(I) ,   BETWEEN LIMITS ,  2I5,        M32.6824
     1         /, ( 1X, 10E13.4  ) )                                    M32.6825
      N2END = N2                                                        M32.6826
      N1 = N1 + NSTAT                                                   M32.6827
      N2 = N2 + NSTAT                                                   M32.6828
      GO TO 35                                                          M32.6829
 4108 BASEV = PU                                                        M32.6830
      N13 = 0                                                           M32.6831
      GO TO 1600                                                        M33.2125
 6543 IF ( M4PLOT .EQ. 1 )  CALL EMTSPY                                 M37.7928
      ICONT = 0                                                         M38.5429
      N22 = 0                                                           VAX.2988
C     READ INPUT CARD USING CIMAGE                                      M32.6834
16543 CALL CIMAGE                                                       M38.5430
C     NEXT CHECK FOR TRUNCATED STATISTICS MISC. DATA CARD:              M32.6849
 1600 CALL MOVE0 (ITEMP(1), LL264)                                      VAX.2989
      DECODE (80, 1620, ABUFF) BUS1, BUS2, BUS3, BUS4                   M32.6850
 1620 FORMAT ( 40X, 4A6 )                                               M32.6851
      IF ( BUS1 .NE. TEXT1 ) GO TO 2620                                 M32.6852
      IF ( BUS2 .NE. TEXT2 ) GO TO 2620                                 M32.6853
      KBASE = 29                                                        M32.6854
      GO TO 9200                                                        M33.2127
 2620 N13 = N13 + 1                                                     M32.6856
      ICONT = ICONT + 1                                                 M38.5432
CCCC  N11 = (ICONT-1) * 11 + 1                                          VAX.2990
      N11 = N22 + 1                                                     VAX.2991
      IF ( N11 .LE. LL264 )  GO TO 2630                                 VAX.2992
      ICONT = 24                                                                
      ICNOVF = 1                                                                
      GO TO 4727                                                        M42.7819
C2630 N22 = ICONT * 11                                                  VAX.2994
 2630 N22 = N22 + 11                                                    VAX.2995
      IF ( KOLBEG .GT. 0 )  GO TO 4723                                  M37.7930
      DECODE (80,7610,ABUFF) N8, C8, (AUPPER(K),K=N11,N22)              M38.5435
 7610 FORMAT (I2,E12.3,11A6)                                            M32.6858
      IF ( ICONT .GT. 1 )  GO TO 4725                                   M38.5436
      IBROPT = N8                                                       M38.5437
      BASEV = C8                                                        M38.5438
      GO TO 4725                                                        M32.6859
 4723 NFRFLD = 1                                                        M32.6860
      KOLBEG = 1                                                        M32.6861
      CALL FREONE ( D11 )                                               M32.6862
      IBROPT = D11                                                      M32.6863
      CALL FREONE ( BASEV )                                             M32.6864
      NRIGHT = -1                                                       M32.6865
      NFRFLD = 11                                                       M32.6866
      CALL FREONE ( D1 )                                                M32.6867
      NRIGHT = 0                                                        M32.6868
      DO 4724  K=N11, N22                                               M38.5439
 4724 AUPPER(K) = TEXTA6(K)                                             M32.6870
 4725 IF ( AUPPER (N22) .NE. TEXT8 ) GO TO 4727                         M38.5440
      N22 = N22 - 1                                                     VAX.2996
      WRITE (KUNIT6, 9615)                                              VAX.2997
 9615 FORMAT ( 43H+  CONTINUED DICE REQUEST.  SEE LAST CARD:       )    VAX.2998
      GO TO 16543                                                       M38.5442
 4727 VMAXSV = 0.0                                                      M38.5443
      VMINSV = FLTINF                                                   M38.5444
      IALL = 0                                                          M38.5445
      TOTMAX = 0.0                                                      M38.5446
      IF ( IBROPT .LT. 0 )  GO TO 7612                                  M38.5447
      IF ( IBROPT  .NE.  1 )  GO TO 1725                                M32.6872
      KILL = 91                                                         M32.6873
      LSTAT(19) = 1725                                                  M32.6874
      GO TO 9200                                                        M32.6875
 1725 KEND = N22                                                        M38.5448
 7611 IVOLT = 0                                                         M32.6877
      MPR=0                                                             M32.6878
      KFL=0                                                             M32.6879
      GO TO 1615                                                        M32.6880
 7612 continue                                                          VAX.2999
      KEND = N22                                                        VAX.3000
      GO TO 7611                                                        M32.6884
 1615 IF ( IBROPT .EQ. 0  .OR.  IBROPT .EQ. -1 ) GO TO 7615             M32.6885
      IF ( BASEV .EQ. 0 )  BASEV = 1.0                                  M32.6886
      IF ( IBROPT .EQ. -2 )   N16 = 3                                           
      IF ( IBROPT .EQ. -3 )   N16 = 5                                           
      IF ( IBROPT .EQ. -4 )   N16 = 7                                           
      WRITE (KUNIT6, 4615)  TEXNAM(10), TEXNAM(N16),                    M35.8620
     1                       TEXNAM(N16+1)                              M35.8621
 4615 FORMAT ( 23H+STATISTICAL OUTPUT OF , A6, 1X, A6, A1, E13.4 )      M35.8622
      GO TO 2800                                                        M32.6897
 7615 IF ( BASEV .EQ. 0.0 ) GO TO 7616                                  M32.6898
      IF ( BASEV  .EQ.  PU )   GO TO 7640                               M32.6899
      N16 = 1                                                                   
      IF ( IBROPT .GE. 0 )   N17 =  9                                           
      IF ( IBROPT .LT. 0 )   N17 = 10                                           
      WRITE (KUNIT6, 4615)  TEXNAM(N17), TEXNAM(N16),                   M35.8626
     1                       TEXNAM(N16+1)                              M35.8627
      GO TO 2800                                                        M32.6903
 7616 BASEV = PU                                                        M32.6906
      MPR=1                                                             M32.6907
 7620 DO 7625  K=1, KEND                                                M32.6908
      IF ( AUPPER(K) .EQ. BLANK ) GO TO 7625                            M32.6909
      KFL = 1                                                           M32.6910
      GO TO 7630                                                        M32.6911
 7625 CONTINUE                                                          M32.6912
 7630 IF ( KFL .EQ. 0 ) GO TO 7635                                      M32.6913
      N16 = 1                                                           M35.8628
      IF ( IBROPT .GE. 0 )   N17 =  9                                           
      IF ( IBROPT .LT. 0 )   N17 = 10                                           
      WRITE (KUNIT6, 4615)  TEXNAM(N17), TEXNAM(N16),                   M35.8631
     1                       TEXNAM(N16+1), BASEV                       M35.8632
      GO TO 7615                                                        M32.6920
 7635 WRITE (KUNIT6, 7636)                                              M35.8633
 7636 FORMAT(48H+BLANK CARD TERMINATING STATISTICS OUTPUT CARDS.  )     M32.6925
      CALL INTERP                                                       M35.8634
      KBASE = 28                                                        M33.2128
      IF ( N13 .EQ. 1 )  GO TO 2800                                     M37.7931
      GO TO 180                                                         M33.2129
 7640 IF ( MPR  .EQ.  1 )   GO TO 2800                                  M35.8635
      N17 = 9                                                                   
      IF ( IBROPT .LT. 0 )  N17 = 10                                            
      N16 = 1                                                                   
      WRITE (KUNIT6, 4615)  TEXNAM(N17), TEXNAM(N16),                   M35.8639
     1                       TEXNAM(N16+1)                              M35.8640
 2800 NUMSRT = 0                                                        M37.7932
      IF ( ICNOVF .EQ. 0 )  GO TO  801                                  M42.7821
      WRITE (LUNIT6, 2625)                                              M42.7822
 2625 FORMAT ( 76H OVERFLOW MAXIMUM LIMIT OF 24 CONT. CARDS, CURRENT REQVAX.3001
     1UEST CARD IS IGNORED. )                                           M42.7824
      ICNOVF = 0                                                                
  801 IF ( IBROPT .NE. 0 )  N33 = KEND/2                                M42.7826
      I = 0                                                             M42.7827
      DO 840  K = 1, N22                                                M38.5456
      IF ( KBASE .NE. 28 )  GO TO 290                                   M37.7934
      K9 = K * NSTAT                                                    M32.6936
      IBROPT = 0                                                        M32.6937
  210 KNTOUT = KNTOUT + 1                                               M32.6938
      IF ( KNTOUT .GT. NSTAT )  GO TO 15071                             M42.7828
      IF ( IBROPT .EQ. -1 )  GO TO 230                                  M37.7936
      IF ( IVOLT .NE. 1 )  GO TO 220                                    M32.6940
      IF ( KNTOUT .EQ. 1 )  GO TO 210                                   M32.6941
      IF ( KNTOUT .GT. NTOT )  GO TO 15071                              M37.7937
      BUS1 = BUS(KNTOUT)                                                M32.6943
      GO TO 40                                                          M32.6944
  220 IF ( KNTOUT .GT. NUMNVO )  GO TO 15071                            M37.7938
      N1 = IBSOUT(KNTOUT)                                               M32.6946
      BUS1 = BUS(N1)                                                    M32.6947
      GO TO 40                                                          M32.6948
  230 I = I + 1                                                         M37.7939
      N1 = IBRNCH(I)                                                    M32.6952
      N2 = JBRNCH(I)                                                    M32.6953
      BUS1 = BUS(N1)                                                    M32.6954
      BUS2 = BUS(N2)                                                    M32.6955
      GO TO 40                                                          M32.6956
  290 IF ( IBROPT .EQ. 0 )  GO TO 7645                                  M32.6957
      IF ( K .GT. N33)  GO TO 6543                                      M38.5457
CC    K1 = K * 2 + ( ICONT - 2 )                                        VAX.3002
      K1 = 2 * K - 1                                                    VAX.3003
      K2 = K1 + 1                                                       M42.7830
      BUS1  =  AUPPER(K1)                                               M32.6961
      BUS2  =  AUPPER(K2)                                               M32.6962
      IF ( BUS1 .EQ. BLANK .AND. BUS2 .EQ. BLANK )  GO TO 5070          M38.5465
      N1 = 0                                                                    
      N2 = 0                                                                    
      DO 7813  J=1, J2                                                          
      IF ( N1 .EQ. 0  .AND.  BUS(J) .EQ. BUS1 )  N1 = J                 M32.6966
      IF ( N2 .EQ. 0  .AND.  BUS(J) .EQ. BUS2 )  N2 = J                 M32.6967
 7813 CONTINUE                                                                  
      N3 = N1 * N2                                                      M32.6969
      IF ( N3 .GT. 1 )  GO TO 7835                                      M32.6970
      IF ( N3 .EQ. 0 )                                                  M32.6971
     1 WRITE (LUNIT6, 817)  BUS1, BUS2                                  M32.6972
  817 FORMAT (  ' ONE OR MORE NONEXISTENT NODE NAMES IN BRANCH',        M32.6973
     1  ' OUTPUT  "', A6, 8H"  TO  ",A6, 27H" .   THIS REQUEST IGNORED.)M32.6974
      GO TO 5070                                                        M35.8641
 7835 IF ( IBROPT .EQ. -2  .OR.  IBROPT .EQ. -4 )  GO TO 1845           M32.6976
      DO 1835  I=1, NV                                                  M32.6977
      IF ( IBRNCH(I) .NE. N1  .OR.                                      M32.6978
     1     JBRNCH(I) .NE. N2 )  GO TO 1835                              M32.6979
      GO TO 1855                                                        M32.6980
 1835 CONTINUE                                                          M32.6981
      GO TO 1852                                                        M32.6982
 1845 DO  1850  I=N9, NC                                                M32.6983
      IF ( IBRNCH(I) .NE. N1  .OR.                                      M32.6984
     1     JBRNCH(I) .NE. N2 )  GO TO 1850                              M32.6985
      GO TO 1855                                                        M32.6986
 1850 CONTINUE                                                          M32.6987
 1852 WRITE (LUNIT6, 817)  BUS1, BUS2                                   M32.6988
      GO TO 5070                                                        M35.8642
 1855 KNTOUT = NUMNVO + I                                               M32.6990
      GO TO 40                                                          M32.6991
 7645 BUS1 = AUPPER(K)                                                  M32.6992
      IF(BUS1.EQ.BLANK) GO TO 5070                                      M35.8643
      DO 7650 I=2,NTOT                                                  M32.6994
      IF(BUS1.EQ.BUS(I)) GO TO 7660                                     M32.6995
 7650 CONTINUE                                                          M32.6996
      WRITE (LUNIT6,3053) BUS1                                          M32.6997
 3053 FORMAT( 5X,  48HREQUEST FOR VOLTAGE OUTPUT OF NONEXISTENT NODE ', M32.6998
     1 A6,   18H' WILL BE IGNORED.   )                                  M32.6999
      GO TO 5070                                                        M35.8644
 7660  DO  1653  J = 1, NUMNVO                                          M32.7001
      IF ( IBSOUT(J) .EQ. I )  KNTOUT = J                               M32.7002
 1653 CONTINUE                                                          M32.7003
   40 VMAX = ABSZ ( ARRAY(KNTOUT) )                                     M32.7004
      NUMSRT = NUMSRT + 1                                               M37.7940
      ITEMP(K) = KNTOUT                                                 M35.8645
      AINSAV = AINCR                                                    M32.7005
      IF ( AINCR .LT. 0.)   VMIN = ABSZ(ARRAY(KNTOUT))                  M32.7006
      N2END = KNTOUT + (NPNERG - 1) * NSTAT                             M32.7007
      DO 45  J = KNTOUT, N2END, NSTAT                                   M32.7008
      ARRAY(J) = ABSZ( ARRAY(J) )                                       M28.6623
      IF ( ARRAY(J) .GT. VMAX )  VMAX = ARRAY(J)                        M28.6624
      IF ( AINCR .GE. 0. )  GO TO 45                                    M32.7009
      IF ( ARRAY(J) .LT. VMIN )  VMIN = ARRAY(J)                        M32.7010
   45 CONTINUE                                                          M28.6625
      IF ( VMAXSV .LT. VMAX )  VMAXSV = VMAX                            M35.8646
      IF ( VMINSV .GT. VMIN )  VMINSV = VMIN                            M35.8647
  145 IF (AINCR .GE. 0.)  GO TO 46                                      M35.8648
      AINSAV = VMAX/VMIN/(LIMINC-1)                                     M32.7012
      BASEV = VMIN                                                      M32.7013
   46 CALL MOVE0 (KOUNT(1), LIMINC)                                     M32.7014
      CALL MOVE0 ( NSUM(1), LIMINC)                                     M13.3118
      I1=0                                                              M32.7015
      SXI=0.0                                                           M13.3135
      SXISQ=0.0                                                         M13.3136
      N88 = KNTOUT                                                      M32.7016
   59 I1 = I1 + 1                                                       M13.3137
      IF ( IPRSUP  .GE.  2 )                                            M22.5495
     1 WRITE (LUNIT6, 5714)  I1, NPNERG, BASEV                          M32.7017
 5714 FORMAT ( /,  9H AT 5714.,   24H      I1  NPNERG        ,          M32.7018
     1 12X,  5HBASEV  ,/,  9X, 2I8,  E15.6  )                           M32.7019
      IF ( I1 .GT. NPNERG )   GO TO 3860                                M35.8649
      IF ( IALL .EQ. 0 )  GO TO 5724                                    M35.8650
      TOTMAX = 0.0                                                      M35.8651
      IF ( K9 .EQ. 0 )  GO TO 5730                                      M42.7832
      IF ( IBROPT .EQ. -1 )  GO TO 5722                                 M42.7833
      DO 5720  L6 =1, NUMNVO                                            M42.7834
      N1 = L6 + (I1-1) * NSTAT                                          M42.7835
      IF (TOTMAX .LT. ARRAY(N1) )  TOTMAX = ARRAY(N1)                   M42.7836
 5720 CONTINUE                                                          M42.7837
      GO TO 5744                                                        M42.7838
 5722 DO 5723  L7 = NUMNVO+1, NSTAT                                     M42.7839
      N1 = L7 + (I1-1) * NSTAT                                          M42.7840
      IF (TOTMAX .LT. ARRAY(N1) )  TOTMAX = ARRAY(N1)                   M42.7841
 5723 CONTINUE                                                          M42.7842
      GO TO 5744                                                        M42.7843
 5730 DO 5734  KJ = 1, N22                                              M42.7844
      I = ITEMP(KJ)                                                     M35.8653
      IF ( I .EQ. 0 )  GO TO 5734                                       M35.8654
      N1 = I + (I1-1) * NSTAT                                           M35.8655
      IF (TOTMAX .LT.  ARRAY(N1) )  TOTMAX = ARRAY(N1)                  M35.8656
 5734 CONTINUE                                                          M35.8657
      GO TO 5744                                                        M35.8658
 5724 TOTMAX = ARRAY(N88)                                               M35.8659
      N88 = N88 + NSTAT                                                 M32.7021
 5744 TOTMAX = TOTMAX / BASEV                                           M35.8660
      XI = TOTMAX                                                       M13.3142
      XISQ = XI * XI                                                    M13.3143
      SXI = SXI + XI                                                    M13.3144
      SXISQ = SXISQ + XISQ                                              M13.3145
      if ( ainsav .le. 0.0 ) go to 6543
      DEX = TOTMAX / AINSAV + 1.0                                       M32.7023
      INDEX = DEX                                                       M13.3147
      IF (INDEX .LE. LIMINC )   GO TO 6020                              M14.2611
      WRITE (LUNIT6, 3)                                                 M17. 997
      GO TO 46901                                                       M37.7941
 6020 KOUNT(INDEX) = KOUNT(INDEX) + 1                                   M14.2614
      GO TO 59                                                          M13.3149
 3860 CALL INTERP                                                       M35.8662
      WRITE (LUNIT6, 6426)                                              M35.8663
 6426 FORMAT ( ///,1X)                                                  M15.1359
      WRITE (LUNIT6, 6434 )                                             M17. 999
 6434 FORMAT ( 132H ----------------------------------------------------M13.3153
     1------------------------------------------------------------------M13.3154
     2-------------   )                                                 M13.3155
      IF ( IALL .EQ. 0 )  GO TO 6464                                    M35.8664
      WRITE (LUNIT6, 16434)                                             M37.7942
16434 FORMAT ( 1X, 13( 10HSUMMARY    ) )                                M37.7943
      WRITE (LUNIT6, 6434 )                                             M37.7944
      IF ( IBROPT .NE. 0 )  GO TO 6439                                  M35.8665
      WRITE (LUNIT6, 95)  BASEV                                         M35.8666
   95 FORMAT ( 121H A DISTRIBUTION OF PEAK OVERVOLTAGES AMONG ALL OUTPUTM35.8667
     1 NODES ON THE LAST CARD HAVING THE SAME BASE VOLTAGE FOLLOWS.   THM35.8668
     2IS   ,/,                                                          M35.8669
     3 108H STATISTICAL DISTRIBUTION IS FOR THE MAXIMUM OF THE PEAKS AT M35.8670
     4ALL OF THESE OUTPUT NODES HAVING BASE VOLTAGE =, E14.5   )        M35.8671
      IF (K9 .NE. 0)  IBROPT = -1                                       M42.7845
      GO TO 68                                                          M35.8672
 6439 WRITE (LUNIT6, 6440)                                              M35.8673
 6440 FORMAT ( 116H A DISTRIBUTION OF PEAK VALUES AMONG ALL OUTPUT BRANCM35.8674
     1HES ON THE LAST CARD HAVING THE SAME BASE VALUE FOLLOWS.   THIS ) M35.8675
      IF ( IBROPT .EQ. -1 )   N16 = 1                                           
      IF ( IBROPT .EQ. -2 )   N16 = 3                                           
      IF ( IBROPT .EQ. -3 )   N16 = 5                                           
      IF ( IBROPT .EQ. -4 )   N16 = 7                                           
      WRITE (LUNIT6, 6798)  TEXNAM(N16), TEXNAM(N16+1), BASEV           M35.8680
 6798 FORMAT ( 44H STATISTICAL DISTRIBUTION IS FOR THE MAXIMUM,         M35.8681
     1         36H OF THE PEAKS AT ALL OF THESE OUTPUT,                 M35.8682
     2         22H BRANCHES HAVING BASE ,  A6, A1, 2H =, E14.5 )        M35.8683
      GO TO 68                                                          M35.8684
 6464 IF ( IBROPT .NE. 0 )  GO TO 2167                                  M35.8685
      WRITE (LUNIT6, 65 )   BUS1, BASEV                                 M32.7026
   65 FORMAT (  52H STATISTICAL DISTRIBUTION OF PEAK VOLTAGE AT NODE  ',M13.3158
     1 A6, 3H' .,  5X,  36HBASE VOLTAGE FOR PER-UNIT PRINTOUT =, E14.5 )M13.3159
      GO TO 68                                                          M13.3160
 2167 IF ( IBROPT .EQ. -1 )   N16 = 1                                           
      IF ( IBROPT .EQ. -2 )   N16 = 3                                           
      IF ( IBROPT .EQ. -3 )   N16 = 5                                           
      IF ( IBROPT .EQ. -4 )   N16 = 7                                           
      WRITE (LUNIT6, 3167)   TEXNAM(N16), TEXNAM(N16+1),                M35.8690
     1           BUS1, BUS2, TEXNAM(N16), TEXNAM(N16+1), BASEV          M35.8691
 3167 FORMAT( 34H STATISTICAL DISTRIBUTION OF PEAK ,  A6, A1,           M35.8692
     1        13H AT BRANCH  ",  A6,  8H"  TO  ",  A6,                  M35.8693
     2        11H".    BASE ,  A6, A1,                                  M35.8694
     3        22H FOR PER-UNIT OUTPUT =,  E14.5  )                      M35.8695
   68 KEY = 0                                                           M13.3165
      IF ( IPRSUP  .GE.  1 )                                            M13.3166
     1 WRITE (LUNIT6, 69)  SXI, TOTAL, SXISQ, BASEV, VMAX, AINSAV       M32.7038
   69 FORMAT ( /, 9H AT 69     , 6E17.5   )                             M13.3168
      TERM2 = SXI * SXI                                                 M13.3169
      TERM1 = TOTAL * SXISQ                                             M13.3170
      XMEAN1 = SXI/TOTAL                                                M13.3171
      IF(TOTAL.EQ.1.0) GO TO 100                                        M13.3172
      XVAR1 = (TERM1-TERM2)/((TOTAL)*(TOTAL-1.0))                       M13.3173
      IF (XVAR1 .LT. 0.0)   XVAR1 = 0.0                                 M19.2036
      GO TO 102                                                         M13.3174
  100 KEY = 1                                                           M13.3175
      XVAR1 = 0.0                                                       M13.3176
  102 STDEV1 = SQRTZ(XVAR1)                                             M18. 868
46900 SXIFI=0.0                                                         M13.3179
      SX2FI=0.0                                                         M13.3180
      IS = 0                                                            M13.3181
      TXMAX = VMAX/BASEV                                                M32.7039
      XINT = TXMAX/AINSAV + 1.0                                         M32.7040
      MAXINT = XINT                                                     M13.3184
      IF ( AINCR .LT. 0.0 )  GO TO 5010                                 M35.8696
      IF(MAXINT .LE. LIMINC)   GO TO 5010                               M13.3185
      WRITE (LUNIT6,3)                                                  M17.1003
    3 FORMAT ( /, 108H NOTICE ----- OVERVOLTAGE TABULATION FOR THIS VOLTM13.3187
     1AGE VARIABLE CAN NOT CONTINUE, DUE TO INSUFFICIENT WORKING   ,/,  M13.3188
     2 14X,       98HSPACE.   STATISTICS MISCELLANEOUS DATA PARAMETER  'M13.3189
     3XMAXMX'  HAS BEEN EXCEEDED BY THE PEAK PER UNIT    ,/,            M13.3190
     4 14X,       56HOVERVOLTAGE (ACTUALLY, EXCEEDED BY  5.0  TIMES OR MM13.3191
     5ORE).   )                                                         M13.3192
      GO TO 46901                                                       M32.7041
 5010 IF ( IBROPT .EQ. -1 )   N16 = 1                                           
      IF ( IBROPT .EQ. -2 )   N16 = 3                                           
      IF ( IBROPT .EQ. -3 )   N16 = 5                                           
      IF ( IBROPT .EQ. -4 )   N16 = 7                                           
      WRITE (LUNIT6, 6010)  TEXNAM(N16), TEXNAM(N16+1),                 M35.8701
     1                      TEXNAM(N16), TEXNAM(N16+1)                  M35.8702
 6010 FORMAT (   2X,  8HINTERVAL, 13X,  A6, A1,  10X,                   M35.8703
     1           A6, A1,  3H IN,                                        M35.8704
     3 6X, 9HFREQUENCY, 5X, 10HCUMULATIVE, 12X, 8HPER CENT )            M32.7043
      WRITE (LUNIT6, 6018)                                              M32.7052
 6018 FORMAT ( 4X, 6HNUMBER, 9X, 11HIN PER UNIT, 6X, 14HPHYSICAL UNITS, M32.7053
     5 6X, 9H(DENSITY), 6X, 9HFREQUENCY, 5X, 18H.GE. CURRENT VALUE )    M32.7054
      DO 5050  M =1, MAXINT                                             M13.3199
      MM=M-1                                                            M13.3200
      IF(M.EQ.1) MM=1                                                   M13.3201
      NSUM(M)=NSUM(MM) + KOUNT(M)                                       M13.3202
      R = AINSAV * (M-1)                                                M32.7055
      XXI = R                                                           M13.3204
      FI = KOUNT(M)                                                     M13.3205
      XIFI = XXI * FI                                                   M13.3206
      X2FI = XXI * XXI * FI                                             M13.3207
      SXIFI=SXIFI + XIFI                                                M13.3208
      SX2FI=SX2FI + X2FI                                                M13.3209
      IF (IPRSUP .GE. 1)                                                M13.3210
     1  WRITE (LUNIT6, 5013)   SXIFI, TOTAL, SX2FI                      M17.1005
 5013 FORMAT (/, 8H AT 5013, 3E17.5)                                    M13.3212
      R = R * BASEV                                                     M32.7056
      PERCEN = ((NPNERG - NSUM(M))/TOTAL)*100                           M13.3214
      IF(NSUM(M).GT.0) GO TO 5011                                       M13.3215
      IS = 1                                                            M13.3216
      M1 = M                                                            M13.3217
      XI1 =XXI                                                          M13.3218
      R1 = R                                                            M13.3219
      KONT1 = KOUNT(M)                                                  M13.3220
      NSUM1 = NSUM(M)                                                   M13.3221
      PRCEN1 = PERCEN                                                   M13.3222
      GO TO 5050                                                        M13.3223
 5011 IF ( IS. GT. 0 )                                                  M13.3224
     1 WRITE (LUNIT6, 5020 ) M1, XI1, R1,  KONT1, NSUM1, PRCEN1         M17.1006
      IS = 0                                                            M13.3226
      IF(NSUM(M) .EQ. NPNERG) GO TO 5015                                M13.3227
      WRITE (LUNIT6,5020)  M,XXI, R, KOUNT(M), NSUM(M), PERCEN          M17.1007
 5020 FORMAT ( 1X, I9, F20.5, E20.6, 2I15, F20.3 )                      M13.3229
      GO TO 5050                                                        M13.3230
 5015 MEND = M                                                          M13.3231
      GO TO 5055                                                        M13.3232
 5050 CONTINUE                                                          M13.3233
 5055 WRITE (LUNIT6,5020) MEND,XXI,R,KOUNT(MEND),NSUM(MEND),PERCEN      M17.1008
      IF (IPRSUP .GE. 1)                                                M13.3235
     1  WRITE (LUNIT6, 5014)   SXIFI, TOTAL, SX2FI                      M17.1009
 5014 FORMAT (/, 8H AT 5014, 3E17.5)                                    M13.3237
      XMEAN = SXIFI/TOTAL                                               M13.3238
      IF ( KEY .NE. 0 )   GO TO 5066                                    M13.3239
      XVAR = ((TOTAL*SX2FI)-(SXIFI*SXIFI))/(TOTAL*(TOTAL-1.0))          M13.3240
      IF( XVAR .LT. 0.0 ) XVAR = 0.0                                    M13.3241
      STDDEV = SQRTZ(XVAR)                                              M18. 869
      WRITE (LUNIT6, 5064) XMEAN, XMEAN1, XVAR, XVAR1, STDDEV, STDEV1   M17.1010
 5064 FORMAT(/, 76H DISTRIBUTION PARAMETERS FOR THE ABOVE DATA.   GROUPEM13.3244
     1D DATA   UNGROUPED DATA,/,38X, 7HMEAN = ,3X,F10.7,6X,F10.7,/,34X, M13.3245
     211HVARIANCE = ,3X,F10.7,6X,F10.7,/,29X,16HSTD DEVIATION = ,3X,    M13.3246
     3F10.7,6X,F10.7 )                                                  M13.3247
      GO TO 46901                                                       M32.7057
 5066 WRITE (LUNIT6, 5069)                                              M17.1011
 5069 FORMAT(/,111H THE NUMBER OF ENERGIZATIONS FOR THIS STATISTICS RUN M13.3250
     1IS 1.  VARIANCE AND STANDARD DEVIATION CANNOT BE COMPUTED. )      M13.3251
46901 CALL MOVE0 (KOUNT(1), LIMINC)                                     M13.3253
      CALL MOVE0 ( NSUM(1), LIMINC )                                    M13.3254
      IF ( K9 .EQ. 0 )  GO TO 15070                                     M37.7945
      IF ( IALL .EQ. 0 )   GO TO 210                                    M37.7947
      IALL = 0                                                          M37.7949
      IF (KNTOUT .GE. NSTAT)  GO TO 1840                                M42.7846
      GO TO 210                                                         M37.7950
15070 IF ( IALL .EQ. 1 )  GO TO 1840                                    M37.7951
 5070 IF ( K .EQ. N22  .AND. IBROPT .EQ. 0 )  GO TO 5071                M38.5467
      IF ( K .EQ.  N33  .AND. IBROPT .LT. 0 )  GO TO 5071               M38.5468
      GO TO 840                                                         M35.8708
15071 IF ( IBROPT .EQ. -1 )  GO TO 5071                                 M42.7847
      KNTOUT = KNTOUT - 1                                               M37.7956
      IF ( NUMSRT .NE. 1 )  GO TO 15073                                 M37.7957
      NUMSRT = 0                                                        M37.7958
      GO TO 210                                                         M37.7959
15073 NUMSRT = 0                                                        M37.7960
      GO TO 15072                                                       M37.7961
 5071 IF ( NUMSRT .EQ. 1 )  GO TO 1840                                  M37.7962
15072 IALL = 1                                                          M37.7963
      VMAX = VMAXSV                                                     M35.8710
      VMIN = VMINSV                                                     M35.8711
      GO TO 145                                                         M35.8712
  840 CONTINUE                                                          M32.7059
 1840 WRITE (LUNIT6, 2840)                                              M37.7964
 2840 FORMAT ( /, 1X )                                                  M37.7965
      IF ( KBASE .NE. 28 )  GO TO 6543                                  M37.7966
  180 IF ( NENERG  .LT.  0 )   GO TO 9200                               M33.2131
  399 WRITE (LUNIT6, 400)                                               M17.1016
  400 FORMAT (///, 49X, 29H SAMPLE CURVE SYMBOLS---A,B,C)               M13.3359
      WRITE (LUNIT6, 402)                                               M17.1017
  402 FORMAT (49X, 29H THEORETICAL CURVE SYMBOL---D)                    M13.3361
      WRITE (LUNIT6, 404)                                               M17.1018
  404 FORMAT (49X, 29H COMMON CURVE SYMBOL--------*)                    M13.3363
      NSW = 0                                                           M13.3364
      NSSW = 0                                                          M13.3365
      REWIND  LUNIT3                                                    M17.1019
      IF ( IPRSUP  .GE.  1 )                                            M32.7063
     1 WRITE (6, 3447)  ISW, KSWTCH, LSWTCH, NTOT                       M32.7064
 3447 FORMAT ( 27H ISW, KSWTCH, LSWTCH, NOT =,  4I6  )                  M32.7065
      IF ( ISW  .EQ.  4444 )                                            M32.7066
     1 READ (LUNIT3)  ( BUS(I),  I = 1, NTOT )                          M32.7067
      IF ( IPRSUP  .GE.  1 )                                            M32.7068
     1 WRITE (LUNIT6, 406)                                              M32.7069
  406 FORMAT (  37H SWITCH VECTORS READ FROM  'LUNIT3' .                M32.7070
     1  ,/,  24H     ROW   BUS1     BUS2,     11X, 4HAKEY,              M32.7071
     2  10X, 5HTSTAT,  13X, 2HDT,  2X, 6HKDEPSW  )                      M32.7072
      DO 1405  I=1, KSWTCH                                              M32.7073
      J = LSWTCH + I                                                    M32.7074
      READ (LUNIT3) KMSWIT(I), KMSWIT(J), AKEY(I), TSTAT(I),            M32.7075
     1              TOPEN(J), KDEPSW(I)                                 M32.7076
      IF ( IPRSUP  .GE.  1 )                                            M32.7077
     1 WRITE (LUNIT6, 1406)  I, KMSWIT(I), KMSWIT(J), AKEY(I),          M32.7078
     2                       TSTAT(I), TOPEN(J), KDEPSW(I)              M32.7079
 1406 FORMAT ( 1X, 3I8, 3E15.6, I8 )                                    M32.7080
 1405 CONTINUE                                                          M32.7081
      READ (LUNIT3) KLOAEP                                              M32.7082
  410 NSW = NSW + 1                                                     M13.3369
      IF (NSW .GT. KSWTCH)   GO TO 415                                  M13.3370
      IF (ABSZ(AKEY(NSW))  .NE. 44444. )   GO TO 410                    M30.1091
      NSSW = NSSW + 1                                                   M13.3372
      IF (KLOAEP .EQ. 0)   GO TO 412                                    M13.3373
      IF (KDEPSW(NSW) .EQ. 0)   GO TO 411                               M13.3374
      TCLOSE(NSSW) = TSTAT(NSW) + TSTAT(KLOAEP)                         M32.7083
      N = KDEPSW(NSW)                                                   M19.2039
      VARTOT = TOPEN(NSW+LSWTCH) ** 2 + TOPEN(N+LSWTCH) ** 2            M44. 757
 1411 IF (KDEPSW(N) .EQ. 0)   GO TO 414                                 M19.2040
      TCLOSE(NSSW) = TCLOSE(NSSW) + TSTAT(N)                            M19.2041
      N = KDEPSW(N)                                                     M19.2042
      VARTOT = VARTOT + TOPEN(N+LSWTCH) ** 2                            M44. 758
      GO TO 1411                                                        M19.2043
  411 TCLOSE(NSSW) = TSTAT(KLOAEP)                                      M19.2044
      GO TO 414                                                         M13.3378
  412 IF (KDEPSW(NSW) .EQ. 0)   GO TO 413                               M13.3379
      N = KDEPSW(NSW)                                                   M13.3380
      TCLOSE(NSSW) = TSTAT(NSW) + TSTAT(N)                              M19.2045
      VARTOT = TOPEN(NSW+LSWTCH) ** 2 + TOPEN(N+LSWTCH) ** 2            M44. 759
 1412 IF (KDEPSW(N) .EQ. 0)   GO TO 414                                 M19.2046
      N = KDEPSW(N)                                                     M19.2047
      TCLOSE(NSSW) = TCLOSE(NSSW) + TSTAT(N)                            M19.2048
      VARTOT = VARTOT + TOPEN(N+LSWTCH) ** 2                            M44. 760
      GO TO 1412                                                        M19.2049
  413 TCLOSE(NSSW) = TSTAT(NSW)                                         M19.2050
  414 TOPEN(NSSW + LSWTCH) = TOPEN(NSW + LSWTCH)                        M32.7084
      IF ( KDEPSW(NSW) .NE. 0 )                                         M44. 761
     1  TOPEN(NSSW+LSWTCH) = SQRTZ(VARTOT)                              M44. 762
      KMSWIT(NSSW) = KMSWIT(NSW)                                        M32.7085
      KMSWIT(NSSW + LSWTCH) = KMSWIT(NSW + LSWTCH)                      M32.7086
      KPOINT(NSSW) = NSW                                                M13.3387
      GO TO 410                                                         M13.3388
  415 DO 1415   I = 1, NSSW                                             M19.2051
      TSTAT(I) = TCLOSE(I)                                              M19.2052
 1415 CONTINUE                                                          M19.2053
      IF (IPRSUP .GE. 1)                                                M19.2054
     1 WRITE (LUNIT6, 1710)  (TSTAT(J), J=1, NSSW)                      M19.2055
 1710 FORMAT(14H TSTAT AT 1710, (1X, 6E15.6) )                          M19.2056
      N1 = 0                                                            M19.2057
      N9 = NENERG * KSWTCH                                              M32.7087
      DO 418   I = 1, NENERG                                            M13.3390
      N5 = N1 + 1                                                       M16.4343
      N6 = N1 + KSWTCH                                                  M16.4344
      READ (LUNIT3)  BIAS, (ARRAY(I1), I1 = N5, N6)                     M17.1024
      M5 = N9 + N5                                                      M32.7088
      M6 = N9 + N6                                                      M32.7089
      READ (LUNIT3)  BIAS, (ARRAY(I1), I1 = M5, M6)                     M32.7090
      RANOFF = BIAS                                                     M32.7091
      IF ( ITEST .EQ. 1 )  RANOFF = 0.0                                 M32.7092
      IF ( IPRSUP  .GE.  1 )                                            M13.3392
     1 WRITE (LUNIT6, 4629)  KSWTCH, NENERG, N1, BIAS,                  M17.1025
     2  (ARRAY(I1) , I1 = N5, N6), (ARRAY(I1),I1=M5,M6)                 M39.3303
 4629 FORMAT ( /,  40H READ LOGICAL 3.  KSWTCH  NENERG      N1,         M13.3395
     1 11X, 4HBIAS, /,  16X, 3I8, E15.5, /, (1X, 8E16.6 ) )             M13.3396
      DO 417   J = 1, NSSW                                              M13.3397
      K = KPOINT(J)                                                     M13.3398
      IF ( ITEST .EQ. 0  .OR. ITEST .EQ. 1 )  GO TO 2417                M32.7093
      IF ( ITEST .GT. 2 )  GO TO 1417                                   M32.7094
      IF ( AKEY(K) .EQ. +44444. )  GO TO  2410                          M32.7095
      RANOFF = 0.0                                                      M32.7096
      GO TO 2417                                                        M32.7097
 2410 RANOFF = BIAS                                                     M32.7098
      GO TO 2417                                                        M32.7099
 1417 IF ( AKEY(K) .EQ. -44444. )  GO TO 2410                           M32.7100
      RANOFF = 0.0                                                      M32.7101
 2417 N7 = N1 + K                                                       M32.7102
      IF ( AKEY(K) .EQ. -44444.)  N7 = N1+K+N9                          M39.3304
      N8 = N1 + J                                                       M19.2058
      ARRAY(N8) = ARRAY(N7) - RANOFF                                    M32.7103
  417 CONTINUE                                                          M13.3400
      N1 = N1 + NSSW                                                    M13.3401
  418 CONTINUE                                                          M13.3402
      SQRT3 = 3.0                                                       M22.5502
      SQRT3 = SQRTZ(SQRT3)                                              M22.5503
      I = 1                                                             M13.3404
  419 CONTINUE                                                          M13.3405
      CALL MOVE0  (KOUNT1(1), IN25)                                     M13.3406
      CALL MOVE0 (KOUNT2(1), IN25)                                      M13.3407
      CALL MOVE0  (KOUNT3(1), IN25)                                     M13.3408
      SDT = ABSZ(TOPEN(I + LSWTCH))                                     M32.7104
      DELTSC = SDT/3.0                                                  M13.3410
      FCLOSC = NENERG * .13242/30                                       M13.3411
      DELTSU = SQRT3*SDT/12.0                                           M13.3412
      FCLOSU = NENERG/720.0                                             M13.3413
      I2 = I + 1                                                        M13.3414
      I3 = I + 2                                                        M13.3415
      IF (I2 .GT. NSSW)   GO TO 430                                     M13.3416
      IF (TOPEN(I2+LSWTCH) .NE. TOPEN(I+LSWTCH))  GO TO 430             M32.7105
      IF (I3 .GT. NSSW)   GO TO  530                                    M13.3418
      IF (TOPEN(I3+LSWTCH) .NE. TOPEN(I+LSWTCH) )  GO TO 530            M32.7106
      GO TO 630                                                         M13.3420
  430 K1 = KMSWIT(I)                                                    M32.7107
      K2 = KMSWIT(I + LSWTCH)                                           M32.7108
      BUS1 = BUS(K1)                                                    M13.3423
      BUS2 = BUS(K2)                                                    M13.3424
      WRITE (LUNIT6, 432)  BUS1, BUS2                                   M17.1026
  432 FORMAT (///, 6X, 9H SWITCH ', A6, 8H'  TO  ', A6, 1H')            M13.3426
      IF (IDIST .GT. 0)   GO TO 436                                     M13.3427
      IF (TOPEN(I + LSWTCH) .LT. 0.0)   GO TO 436                       M32.7109
      WRITE (LUNIT6, 434)   FCLOSC                                      M17.1027
  434 FORMAT (5X, E12.4, 20H SWITCH CLOSINGS/COL)                       M13.3430
      GO TO 440                                                         M13.3431
  436 WRITE (LUNIT6, 434)   FCLOSU                                      M17.1028
  440 WRITE (LUNIT6, 442)                                               M17.1029
  442 FORMAT ( 17X, 8H COLUMNS)                                         M13.3434
      WRITE (LUNIT6, 444)  IN5, IN10, IN15, IN20, IN25, IN30            M17.1030
  444 FORMAT (10X, I1, 5(3X, I2))                                       M13.3436
      WRITE(LUNIT6, 446)                                                M17.1031
  446 FORMAT ( 5X,  31H+----+----+----+----+----+----+   )              M35.8713
      GO TO 660                                                         M13.3441
  530 K1 = KMSWIT(I)                                                    M32.7110
      K2 = KMSWIT(I + LSWTCH)                                           M32.7111
      K3 = KMSWIT(I2)                                                   M32.7112
      K4 = KMSWIT(I2 + LSWTCH)                                          M32.7113
      BUS1 = BUS(K1)                                                    M13.3446
      BUS2 = BUS(K2)                                                    M13.3447
      BUS3 = BUS(K3)                                                    M13.3448
      BUS4 = BUS(K4)                                                    M13.3449
      WRITE (LUNIT6, 532)  BUS1, BUS2, BUS3, BUS4                       M17.1033
  532 FORMAT (///, 6X, 2(9H SWITCH ', A6, 8H'  TO  ', A6, 1H', 12X))    M13.3451
      IF (IDIST .GT. 0)   GO TO 536                                     M13.3452
      IF (TOPEN(I + LSWTCH) .LT. 0.0)   GO TO 536                       M32.7114
      WRITE (LUNIT6, 534)   FCLOSC, FCLOSC                              M17.1034
  534 FORMAT ( 5X, 2(E12.4, 20H SWITCH CLOSINGS/COL, 10X))              M13.3455
      GO TO 540                                                         M13.3456
  536 WRITE (LUNIT6, 534)   FCLOSU, FCLOSU                              M17.1035
  540 WRITE (LUNIT6, 542)                                               M17.1036
  542 FORMAT (17X, 8H COLUMNS, 36X, 8H COLUMNS)                         M13.3459
      WRITE (LUNIT6, 544)  IN5, IN10, IN15, IN20, IN25, IN30,           M17.1037
     1                IN5, IN10, IN15, IN20, IN25, IN30                 M13.3461
  544 FORMAT( 10X, I1, 5(3X, I2), 16X, I1, 5(3X, I2))                   M13.3462
      WRITE (LUNIT6, 546)                                               M17.1038
  546 FORMAT (5X, 2(31H+----+----+----+----+----+----+, 11X))           M35.8714
      GO TO 660                                                         M13.3468
  630 K1 = KMSWIT(I)                                                    M32.7115
      K2 = KMSWIT(I + LSWTCH)                                           M32.7116
      K3 = KMSWIT(I2)                                                   M32.7117
      K4 = KMSWIT(I2 + LSWTCH)                                          M32.7118
      K5 = KMSWIT(I3)                                                   M32.7119
      K6 = KMSWIT(I3 + LSWTCH)                                          M32.7120
      BUS1 = BUS(K1)                                                    M13.3475
      BUS2 = BUS(K2)                                                    M13.3476
      BUS3 = BUS(K3)                                                    M13.3477
      BUS4 = BUS(K4)                                                    M13.3478
      BUS5 = BUS(K5)                                                    M13.3479
      BUS6 = BUS(K6)                                                    M13.3480
      WRITE (LUNIT6, 632)  BUS1, BUS2, BUS3, BUS4, BUS5, BUS6           M17.1040
  632 FORMAT (///, 6X, 9H SWITCH ', A6, 8H'  TO  ', A6, 1H', 2(12X,     M13.3482
     1                 9H SWITCH ', A6, 8H'  TO  ', A6, 1H'))           M13.3483
      IF (IDIST.GT.0)   GO TO 636                                       M13.3484
      IF (TOPEN(I +LSWTCH) .LT. 0.0)   GO TO 636                        M32.7121
      WRITE (LUNIT6, 634)   FCLOSC, FCLOSC, FCLOSC                      M17.1041
  634 FORMAT (5X, 3(E12.4, 20H SWITCH CLOSINGS/COL, 10X))               M13.3487
      GO TO 640                                                         M13.3488
  636 WRITE (LUNIT6, 634)   FCLOSU, FCLOSU, FCLOSU                      M17.1042
  640 WRITE (LUNIT6, 642)                                               M17.1043
  642 FORMAT (17X, 8H COLUMNS, 2(36X, 8H COLUMNS))                      M13.3491
      WRITE (LUNIT6, 644)  IN5, IN10, IN15, IN20, IN25, IN30,           M17.1044
     1                IN5, IN10, IN15, IN20, IN25, IN30,                M13.3493
     2                IN5, IN10, IN15, IN20, IN25, IN30                 M13.3494
  644 FORMAT (10X, I1, 5(3X, I2), 2(16X, I1, 5(3X, I2)))                M13.3495
      WRITE (LUNIT6, 646)                                               M17.1045
  646 FORMAT (5X, 3(31H+----+----+----+----+----+----+, 11X))           M35.8715
  660 DO 665   N = 1, NENERG                                            M13.3501
      L = (N-1) *NSSW                                                   M13.3502
      N3 = I + L                                                        M16.4349
      TWDEP = ARRAY(N3) - TSTAT(I)                                      M19.2060
      IF (KDEPSW(I) .EQ. 0)   GO TO 1660                                M19.2061
      M = KDEPSW(I)                                                     M19.2062
      NN3 = M + L                                                       M19.2063
      TWDEP = TWDEP - ARRAY(NN3) + TSTAT(M)                             M19.2064
 1660 IF (IDIST .GT. 0)   GO TO 662                                     M19.2065
      IF (TOPEN(I + LSWTCH) .LT. 0.0)   GO TO 662                       M32.7122
      L1 = 13 + (TWDEP + DELTSC/2.) / DELTSC                            M19.2066
      KOUNT1(L1) = KOUNT1(L1) + 1                                       M13.3506
      IF (IPRSUP .GE. 1)                                                M19.2067
     1 WRITE (LUNIT6, 661)  N3, ARRAY(N3), L1, KOUNT1(L1)               M19.2068
  661 FORMAT (/, 54H AT 661 OF SUBR29,N3, ARRAY(N3), L1 AND KOUNT1(L1) AM19.2069
     1RE, 10X, I10, E15.6, 2I10)                                        M19.2070
      GO TO 665                                                         M13.3507
  662 L1 = 13 + (TWDEP + DELTSU/2.) / DELTSU                            M19.2071
      KOUNT1(L1) = KOUNT1(L1) + 1                                       M13.3509
      IF (IPRSUP .GE. 1)                                                M19.2072
     1 WRITE (LUNIT6, 661) N3, ARRAY(N3), L1, KOUNT1(L1)                M19.2073
  665 CONTINUE                                                          M13.3510
      IF (I2 .GT. NSSW)   GO TO  700                                    M13.3511
      IF (TOPEN(I2+LSWTCH) .NE. TOPEN(I+LSWTCH))  GO TO 700             M32.7123
      DO 670  N = 1, NENERG                                             M13.3513
      L = (N-1) * NSSW                                                  M13.3514
      N4 = I2 + L                                                       M16.4352
      TWDEP = ARRAY(N4) - TSTAT(I2)                                     M19.2074
      IF (KDEPSW(I2) .EQ. 0)   GO TO 1666                               M19.2075
      M = KDEPSW(I2)                                                    M19.2076
      NN4 = M + L                                                       M19.2077
      TWDEP = TWDEP - ARRAY(NN4) + TSTAT(M)                             M19.2078
 1666 IF (IDIST .GT. 0)   GO TO 668                                     M19.2079
      IF (TOPEN(I2 + LSWTCH) .LT. 0.0  )   GO TO 668                    M32.7124
      L2 = 13 + (TWDEP + DELTSC/2.) / DELTSC                            M19.2080
      KOUNT2(L2) = KOUNT2(L2) + 1                                       M13.3518
      GO TO 669                                                         M19.2081
  668 L2 = 13 + (TWDEP + DELTSU / 2.) / DELTSU                          M19.2082
      KOUNT2(L2) = KOUNT2(L2) + 1                                       M13.3521
  669 IF (IPRSUP .GE. 1)                                                M19.2083
     1 WRITE (LUNIT6, 1669) N4, ARRAY(N4), L2, KOUNT2(L2)               M19.2084
 1669 FORMAT (55H AT 1669 OF SUBR29,N4, ARRAY(N4), L2 AND KOUNT2(L2) AREM19.2085
     1, 10X, I10, E15.6, 2I10)                                          M19.2086
  670 CONTINUE                                                          M13.3522
      IF (IPRSUP .GE. 1)                                                M19.2087
     1 WRITE (LUNIT6, 671)  I, I2, I3, TOPEN( I + LSWTCH ),             M32.7125
     2           TOPEN(I2+LSWTCH), TOPEN(I3+LSWTCH)                     M32.7126
  671 FORMAT (21H I, I2, I3 AND DT ARE, 10X, 3I5, 3E15.6)               M19.2089
      IF (I3 .GT. NSSW)   GO TO 700                                     M13.3523
      IF (TOPEN(I3+LSWTCH) .NE. TOPEN(I+LSWTCH))  GO TO 700             M32.7127
      DO 675 N=1, NENERG                                                M13.3525
      L = (N-1) * NSSW                                                  M13.3526
      N5 = I3 + L                                                       M16.4355
      TWDEP = ARRAY(N5) - TSTAT(I3)                                     M19.2090
      IF (KDEPSW(I3) .EQ. 0)   GO TO 1671                               M19.2091
      M = KDEPSW(I3)                                                    M19.2092
      NN5 = M + L                                                       M19.2093
      TWDEP = TWDEP - ARRAY(NN5) + TSTAT(M)                             M19.2094
 1671 IF (IDIST .GT. 0)   GO TO 672                                     M19.2095
      IF(TOPEN(I3+LSWTCH) .LT. 0.0 )   GO TO 672                        M32.7128
      L3 = 13 + (TWDEP + DELTSC/2.) / DELTSC                            M19.2096
      KOUNT3(L3) = KOUNT3(L3) + 1                                       M13.3530
      GO TO 673                                                         M19.2097
  672 L3 = 13+ (TWDEP + DELTSU/2.) / DELTSU                             M19.2098
      KOUNT3(L3) = KOUNT3(L3) + 1                                       M13.3533
  673 IF (IPRSUP .GE. 1)                                                M19.2099
     1 WRITE (LUNIT6, 674) N5, ARRAY(N5), L3, KOUNT3(L3)                M19.2100
  674 FORMAT (55H AT 674 OF SUBR29, N5, ARRAY(N5), L3 AND KOUNT3(L3) AREM19.2101
     1, 10X, I10, E15.6, 2I10)                                          M19.2102
  675 CONTINUE                                                          M13.3534
  700 KURVE1 = 1                                                        M13.3535
      KURVE2 = 2                                                        M13.3536
      KURVE3 = 3                                                        M13.3537
      KURVE4 = 4                                                        M13.3538
      N13 = -5678                                                               
      CALL PLOTNG ( LLM1, N13 )                                                 
      DO 780   LINE = 1, 25                                             M13.3539
      KOLUM1 = 0                                                        M13.3540
      KOLUM2 = 0                                                        M13.3541
      KOLUM3 = 0                                                        M13.3542
      IF (IDIST .GT. 0)   GO TO 753                                     M13.3543
      IF (TOPEN(I+LSWTCH) .LT. 0.0)   GO TO 753                         M32.7129
      KOLUM1 = 5 + KOUNT1(LINE)/FCLOSC + 0.5                            M13.3545
      KOLNUM = 5 + NORMAL(LINE)                                         M13.3546
      IF (IPRSUP .GE. 1)                                                M19.2103
     1 WRITE(LUNIT6, 751)  LINE, KOUNT1(LINE), NORMAL(LINE)             M19.2104
  751 FORMAT(/, 36H AT 751, LINE, KOUNT1 AND NORMAL ARE, 10X, 3I10)     M19.2105
      GO TO 770                                                         M13.3547
  753 KOLUM1 = 5 + KOUNT1(LINE)/FCLOSU + 0.5                            M13.3548
      IF (IPRSUP .GE. 1)                                                M19.2106
     1 WRITE (LUNIT6, 1754)  LINE, KOUNT1(LINE)                         M19.2107
 1754 FORMAT (16H LINE AND KOUNT1, 10X, 2I10)                           M19.2108
      KOLNUM = 35                                                       M13.3549
      GO TO 770                                                         M13.3550
  754 IF (IDIST .GT. 0)   GO TO 758                                     M13.3551
      IF (TOPEN(I+LSWTCH) .LT. 0.0)   GO TO 758                         M32.7130
      KOLUM2 = 47 + KOUNT2(LINE)/FCLOSC + 0.5                           M13.3553
      KOLNUM = 47 + NORMAL(LINE)                                        M13.3554
      GO TO 772                                                         M13.3555
  758 KOLUM2 = 47 + KOUNT2(LINE)/FCLOSU + 0.5                           M13.3556
      IF (IPRSUP .GE. 1)                                                M19.2109
     1 WRITE (LUNIT6, 1758)  LINE, KOUNT2(LINE)                         M19.2110
 1758 FORMAT (16H LINE AND KOUNT2, 10X, 2I10)                           M19.2111
      KOLNUM = 77                                                       M13.3557
      GO TO 772                                                         M13.3558
  759 IF(IDIST .GT. 0)   GO TO 763                                      M13.3559
      IF (TOPEN(I+LSWTCH) .LT. 0.0)   GO TO 763                         M32.7131
      KOLUM3 = 89 + KOUNT3(LINE)/FCLOSC + 0.5                           M13.3561
      KOLNUM = 89 + NORMAL(LINE)                                        M13.3562
      GO TO 774                                                         M13.3563
  763 KOLUM3 = 89 + KOUNT3(LINE)/FCLOSU + 0.5                           M13.3564
      IF (IPRSUP .GE. 1)                                                M19.2112
     1 WRITE (LUNIT6, 1763)  LINE, KOUNT3(LINE)                         M19.2113
 1763 FORMAT (16H LINE AND KOUNT3, 10X, 2I10)                           M19.2114
      KOLNUM = 119                                                      M13.3565
      GO TO 774                                                         M13.3566
  770 CALL  PLOTNG (KURVE4, KOLNUM)                                     M13.3567
      CALL  PLOTNG (KURVE1, KOLUM1)                                     M35.8718
      IF (TOPEN(I2+LSWTCH) .NE. TOPEN(I+LSWTCH) )  GO TO 779            M35.8719
      GO TO 754                                                         M13.3571
  772 CALL PLOTNG (KURVE4, KOLNUM)                                      M13.3572
      CALL PLOTNG (KURVE2, KOLUM2)                                      M35.8720
      IF ( TOPEN(I3+LSWTCH) .NE. TOPEN(I+LSWTCH) )  GO TO 779           M35.8721
      GO TO 759                                                         M13.3576
  774 CALL PLOTNG (KURVE4, KOLNUM)                                      M13.3577
      CALL PLOTNG (KURVE3, KOLUM3)                                      M13.3579
  779 IF (LINE .NE. 13)   GO TO 705                                     M35.8722
      TOLMAT = TSTAT(I)                                                         
      N13 = -4321                                                               
      CALL PLOTNG ( KURVE1, N13 )                                               
  733 IF (I2 .GT. NSSW)   GO TO 705                                             
      IF ( TOPEN(I2+LSWTCH) .NE. TOPEN(I+LSWTCH) )  GO TO 705           M35.8727
      TOLMAT = TSTAT(I2)                                                        
      CALL PLOTNG ( KURVE2, N13 )                                               
  738 IF (I3 .GT. NSSW)   GO TO 705                                             
      IF ( TOPEN(I3+LSWTCH) .NE. TOPEN(I+LSWTCH) )  GO TO 705           M35.8731
      TOLMAT = TSTAT(I3)                                                        
      CALL PLOTNG ( KURVE3, N13 )                                               
  705 CALL PLOTNG ( JDUMMY, LLM1 )                                              
  780 CONTINUE                                                          M13.3633
      IF (I2 .GT. NSSW)   GO TO 799                                     M13.3634
      IF (TOPEN(I2+LSWTCH) .NE. TOPEN(I+LSWTCH))   GO TO 799            M32.7139
      IF (I3. GT. NSSW)   GO TO 809                                     M13.3636
      IF (TOPEN(I3+LSWTCH) .NE. TOPEN(I+LSWTCH))   GO TO 809            M32.7140
      GO TO 813                                                         M13.3638
  799 WRITE (LUNIT6,805)                                                M17.1060
  805 FORMAT (3X, 5H TIME)                                              M13.3640
      IF (IDIST .GT. 0 )   GO TO 821                                    M13.3641
      IF (TOPEN(I + LSWTCH) .LT. 0.0) GO TO 821                         M32.7141
      WRITE (LUNIT6, 825)  DELTSC                                       M17.1061
  825 FORMAT (E12.4)                                                    M13.3644
      GO TO 826                                                         M13.3645
  821 WRITE (LUNIT6, 825)   DELTSU                                      M17.1062
  826 WRITE (LUNIT6, 865)                                               M17.1063
  865 FORMAT (2X, 9H SEC/LINE)                                          M13.3648
      GO TO 991                                                         M13.3649
  809 WRITE (LUNIT6, 810)                                               M17.1064
  810 FORMAT (3X, 5H TIME, 37X, 5H TIME)                                M13.3651
      IF (IDIST .GT. 0 )   GO TO 831                                    M13.3652
      IF (TOPEN(I + LSWTCH) .LT. 0.0)   GO TO 831                       M32.7142
      WRITE (LUNIT6, 835)   DELTSC, DELTSC                              M17.1065
  835 FORMAT (E12.4, 30X, E12.4)                                        M13.3655
      GO TO 836                                                         M13.3656
  831 WRITE (LUNIT6, 835)   DELTSU, DELTSU                              M17.1066
  836 WRITE (LUNIT6, 875)                                               M17.1067
  875 FORMAT( 2X, 9H SEC/LINE, 34X, 9H SEC/LINE)                        M13.3659
      GO TO 992                                                         M13.3660
  813 WRITE (LUNIT6, 814)                                               M17.1068
  814 FORMAT (3X, 5H TIME, 2(37X, 5H TIME))                             M13.3662
      IF (IDIST .GT. 0)   GO TO 841                                     M13.3663
      IF (TOPEN(I + LSWTCH)  .LT. 0.0)  GO TO 841                       M32.7143
      WRITE (LUNIT6, 845)   DELTSC, DELTSC, DELTSC                      M17.1069
  845 FORMAT (E12.4, 2(30X, E12.4))                                     M13.3666
      GO TO 848                                                         M13.3667
  841 WRITE (LUNIT6, 845)   DELTSU, DELTSU, DELTSU                      M17.1070
  848 WRITE (LUNIT6, 850)                                               M17.1071
  850 FORMAT (2X,  9H SEC/LINE, 2(34X, 9H SEC/LINE))                    M13.3670
      I = I + 3                                                         M13.3671
      GO TO 1000                                                        M13.3672
  991 I = I + 1                                                         M13.3673
      GO TO 1000                                                        M13.3674
  992 I = I + 2                                                         M13.3675
 1000 IF (I .LE. NSSW)   GO TO 419                                      M13.3676
      NM1 = NENERG - 1                                                  M13.3677
      DO 929  I = 1, NSSW                                               M13.3678
      IF ( NM1  .LE.  0 )   GO TO 1905                                  M14.2622
      DO 905  J = 1, NM1                                                M13.3679
      M = J * NSSW + I                                                  M13.3680
      MM1 = M - NSSW                                                    M13.3681
      IF (ARRAY(M)  .GE.  ARRAY(MM1))   GO TO 905                       M13.3682
      TEMP = ARRAY(M)                                                   M13.3683
      DO 903   JN = 1, J                                                M13.3684
      JN1 = JN * NSSW                                                   M13.3685
      L = M - JN1                                                       M13.3686
      N6 = L + NSSW                                                     M16.4358
      IF (TEMP .GE. ARRAY(L))   GO TO 904                               M13.3687
      ARRAY(N6) = ARRAY(L)                                              M16.4359
  903 CONTINUE                                                          M13.3689
      ARRAY(I) = TEMP                                                   M13.3690
      GO TO 905                                                         M13.3691
  904 ARRAY(N6) = TEMP                                                  M16.4360
  905 CONTINUE                                                          M13.3693
 1905 IF (IDIST .EQ. 0)   GO TO 900                                     M14.2623
  901 ABC = ABSZ(TOPEN(I+LSWTCH)) * SQRT3                               M32.7144
      B = TSTAT(I) -ABC                                                 M13.3696
      C = TSTAT(I) + ABC                                                M13.3697
      GO TO 902                                                         M13.3698
  900 IF (TOPEN(I + LSWTCH) .LT. 0.0)   GO TO 901                       M32.7145
  902 DN = 0.0                                                          M13.3700
      FS = 0.0                                                          M13.3701
      IL = 1                                                            M13.3702
  906 IF (IL .LE. NM1)   GO TO 1906                                     M15.1360
      JB = I                                                            M15.1361
      GO TO 908                                                         M15.1362
 1906 DO 907   K = IL, NM1                                              M15.1363
      J = K                                                             M13.3704
      JB = J * NSSW + I                                                 M13.3705
      JA = JB - NSSW                                                    M13.3706
      IF (ARRAY(JA) .NE. ARRAY(JB))   GO TO 909                         M13.3707
  907 CONTINUE                                                          M13.3708
  908 J = NENERG                                                        M13.3709
      JA = JB                                                           M13.3710
  909 IL = J + 1                                                        M13.3711
      FI = FS                                                           M13.3712
      FS = J                                                            M34. 171
      FS = FS / TOTAL                                                   M34. 172
      IF (IDIST .GT. 0 )   GO TO 917                                    M13.3714
      IF (TOPEN(I + LSWTCH) .LT. 0.0 )   GO TO 917                      M32.7146
      IF (TOPEN(I +LSWTCH) .GT. 0.0)   GO TO 912                        M32.7147
      GO TO 929                                                         M13.3717
  912 Z = (ARRAY(JA) - TSTAT(I))/TOPEN(I+LSWTCH)                        M32.7148
      AZ = ABSZ(Z)                                                      M18. 875
      T = 1.0/(1.0 + .2316419 * AZ )                                    M13.3720
      D = .3989423 * EXPZ(-Z*Z/2.0)                                     M18. 876
      Y = 1.0 - D*T*((((1.330274 * T - 1.821256) * T + 1.781478) * T    M13.3722
     1 -.3565638) * T + .3193815)                                       M13.3723
      IF (Z .GE. 0.0)   GO TO 927                                       M13.3724
      Y = 1.0 - Y                                                       M13.3725
      GO TO 927                                                         M13.3726
  917 IF (ARRAY(JA) .GT. B)   GO TO 923                                 M13.3727
      Y = 0.0                                                           M13.3728
      GO TO 927                                                         M13.3729
  923 IF (ARRAY(JA) .LE. C)   GO TO 925                                 M13.3730
      Y = 1.0                                                           M13.3731
      GO TO 927                                                         M13.3732
  925 Y = (ARRAY(JA) - B)/(2 * ABC)                                     M13.3733
  927 EI = ABSZ(Y - FI)                                                 M18. 877
      ES = ABSZ(Y - FS)                                                 M18. 878
      IF (DN .LT. ES)   DN = ES                                         M14.2626
      IF (IL - NENERG)  906, 908, 928                                   M13.3737
  928 Z = DN * SQRTZ(TOTAL)                                             M18. 879
      IF (IPRSUP .GE. 1)                                                M19.2115
     1 WRITE (LUNIT6, 1927)  AZ, Y, ES, DN, Z                           M19.2116
 1927 FORMAT(/, 33H AT 1927, AZ, Y, ES, DN AND Z ARE, 10X, 5E15.6)      M19.2117
      IF ( Z .GT. 0.27 )   GO TO 2                                      M13.3739
      PROB = 0.0                                                        M13.3740
      GO TO 950                                                         M13.3741
    2 IF ( Z .GE. 1.0 )   GO TO 6                                       M13.3742
      C1 = EXPZ(-1.233701/Z**2)                                         M18. 880
      N1 = ALOG1Z(C1) * 32.                                             M19.2118
      IF (-N1 .LT. INFEXP)   GO TO 1928                                 M19.2119
      C8 = 0.0                                                          M14.2629
      GO TO 5                                                           M14.2630
 1928 C2 = C1*C1                                                        M14.2631
      C4 = C2 *C2                                                       M13.3745
      C8 = C4 * C4                                                      M13.3746
    5 PROB = (2.506628/Z) * C1 * ( 1.0 + C8 * (1.0 + C8 * C8))          M13.3749
      GO TO 950                                                         M13.3750
    6 IF (Z .LT. 3.1)   GO TO 8                                         M13.3751
      PROB = 1.0                                                        M13.3752
      GO TO 950                                                         M13.3753
    8 C1 = EXPZ(-2.0 * Z * Z)                                           M18. 882
      C2 = C1 * C1                                                      M13.3755
      C4 = C2 *C2                                                       M13.3756
      C8 = C4 *C4                                                       M13.3757
      PROB = 1.0 - 2.0 * (C1 -C4 + C8 *(C1-C8))                         M13.3758
  950 PROB = 1.0 - PROB                                                 M13.3759
      WRITE (LUNIT6, 940)   PROB                                        M17.1072
  940 FORMAT (75X,  31H KOLMOGOROV-SMIRNOV TEST RESULT, 10X, E15.6)     M13.3761
  929 CONTINUE                                                          M13.3762
 9200 IF ( ISW .EQ. 4444 )  KBASE = 28                                  M33.2132
      LSTAT(18) = 29                                                    M33.2133
      LASTOV = NCHAIN                                                   M15.1364
      NCHAIN = 51                                                       M13.3765
      RETURN                                                            M13.3766
      END                                                               M13.3767
      SUBROUTINE PLOTNG ( KKRV, KKLM )                                  M32.7149
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE CALLED ONLY BY "INNR29" OF OVERLAY 29.           M35.8735
C     SOME LOGIC IS SIMILAR TO "LINPLT" OF OVERLAY 31.                  M35.8736
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION  KUT(131), KTSIN(4), KDIG(10)                           M35.8737
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M35.8738
      DATA  KUT(1)  / 1H  /                                             M35.8739
      DATA  KTSIN(1)   /  1HA  /                                        M13.3823
      DATA  KTSIN(2)   /  1HB  /                                        M13.3824
      DATA  KTSIN(3)   /  1HC  /                                        M13.3825
      DATA  KTSIN(4)   /  1HD  /                                        M13.3826
      DATA  KOM        /  1H*  /                                        M13.3827
      DATA  KLANK      /  1H   /                                        M13.3828
      DATA  KDIG(1)   /  1H1  /                                         M35.8740
      DATA  KDIG(2)   /  1H2  /                                         M35.8741
      DATA  KDIG(3)   /  1H3  /                                         M35.8742
      DATA  KDIG(4)   /  1H4  /                                         M35.8743
      DATA  KDIG(5)   /  1H5  /                                         M35.8744
      DATA  KDIG(6)   /  1H6  /                                         M35.8745
      DATA  KDIG(7)   /  1H7  /                                         M35.8746
      DATA  KDIG(8)   /  1H8  /                                         M35.8747
      DATA  KDIG(9)   /  1H9  /                                         M35.8748
      DATA  KDIG(10)  /  1H0  /                                         M35.8749
      IF ( IPRSUP .GE. 1 )  WRITE (LUNIT6, 1349)  KKRV, KKLM            M35.8750
 1349 FORMAT ( 31H TOP OF "PLOTNG".  KKRV, KKLM =,  2I8  )              M35.8751
      KRV = KKRV                                                        M35.8752
      KLM = KKLM                                                        M32.7153
      IF ( KLM .EQ. -5678 )  GO TO 350                                          
      IF ( KLM  .GT.  131 )   GO TO 5831                                        
      IF ( KLM .EQ. -4321 ) GO TO 4832                                          
      IF (KLM .GE. 0 )   GO TO 3652                                             
      WRITE (LUNIT6, 230)   KUT                                         M17.1073
  230 FORMAT ( 1X, 131A1 )                                              M13.3831
  350 DO 370 J = 1, 131                                                 M13.3832
  370 KUT(J) = KLANK                                                    M13.3833
      KUT(5) = KDIG(1)                                                          
      GO TO 9000                                                        M35.8758
C     BEGIN CODE TO ADD CURVE "KRV" TO COLUMN "KLM" OF KUT(131)         M35.8759
 3652 IF ( KRV .NE. 2 )  GO TO 3664                                     M35.8760
      IF ( KUT(47) .EQ. KLANK ) KUT(47) = KDIG(1)                               
 3664 IF ( KRV .NE. 3 )  GO TO 3671                                     M35.8762
      IF ( KUT(89) .EQ. KLANK ) KUT(89) = KDIG(1)                               
 3671 IF ( KUT(KLM) .EQ. KLANK )    GO TO 3688                                  
      IF ( KUT(KLM) .EQ. KDIG(1) )  GO TO 3688                                  
      KUT(KLM) = KOM                                                            
      GO TO 9000                                                                
 3688 KUT(KLM) = KTSIN(KRV)                                                     
      GO TO 9000                                                                
C     BEGIN CODE TO ENCODE MEAN CLOSING TIME STORED IN TOLMAT:          M35.8770
 4832 N4 = 6                                                                    
      IF ( KRV .EQ. 2 )  N4 = 48                                                
      IF ( KRV .EQ. 3 )  N4 = 90                                                
      N5 = N4 + 11                                                              
      IF ( IPRSUP .GE. 1 )  WRITE (LUNIT6, 4834)  N4, TOLMAT            M35.8775
 4834 FORMAT ( 28H LUNT12 WRITE.  N4, TOLMAT =,  I8, E15.3  )           M37.7967
      REWIND LUNT12                                                     M35.8777
      WRITE (LUNT12, 4837)  TOLMAT                                      M35.8778
 4837 FORMAT ( E12.3 )                                                  M35.8779
      REWIND LUNT12                                                     M35.8780
      READ  (LUNT12, 4844)  ( KUT(J), J=N4, N5 )                        M35.8781
 4844 FORMAT ( 12A1 )                                                   M35.8782
      GO TO 9000                                                        M35.8783
C     BEGIN CODE TO ENCODE 3-DIGIT KLM ON RIGHT EDGE OF KUT:            M35.8784
 5831 IF ( KRV .EQ. 1 )  KLM = KLM -  5                                         
      IF ( KRV .EQ. 2 )  KLM = KLM - 47                                         
      IF ( KRV .EQ. 3 )  KLM = KLM - 89                                         
      IF ( KLM .LT. 1000 )  GO TO 5856                                  M35.8788
      WRITE (LUNIT6, 5845)  KRV, KLM                                    M35.8789
 5845 FORMAT ( 34H TOO LARGE KLM IN "PLOTNG".  STOP.,                   M35.8790
     1         13H   KRV, KLM =,  2I8,  8X, 14HKUT(1:131) ...           M35.8791
     2         ,/,  1X, 131A1 )                                         M35.8792
 5856 N6 = KLM / 100                                                    M35.8793
      N1 = KLM - 100 * N6                                               M35.8794
      N7 = N1 / 10                                                      M35.8795
      N8 = N1 - 10 * N7                                                 M35.8796
      IF ( N7 .EQ. 0 )  N7 = 10                                         M35.8797
      IF ( N8 .EQ. 0 )  N8 = 10                                         M35.8798
      N14 = 120                                                                 
      IF ( KRV .EQ. 2 )  N14 = 124                                              
      IF ( KRV .EQ. 3 )  N14 = 128                                              
      KUT(N14)   =  KDIG(N6)                                                    
      KUT(N14+1) =  KDIG(N7)                                                    
      KUT(N14+2) =  KDIG(N8)                                                    
      IF ( IPRSUP .GE. 1 )                                              M35.8805
     1 WRITE (LUNIT6, 9045)  KRV, KLM, N14, N6, N7, N8, KUT             M35.8806
 9045 FORMAT ( 42H ENCODE KLM.   KRV, KLM, N14, N6, N7, N8 =,           M35.8807
     1         6I6,  8X,  15HKUT(1:131) ....  ,/,  1X, 131A1  )         M35.8808
 9000 IF ( IPRSUP .GE. 3 )                                              M35.8809
     1 WRITE (LUNIT6, 9004)  KRV, KLM,  KUT                             M35.8810
 9004 FORMAT ( 27H EXIT "PLOTNG",  KRV, KLM =,  2I6,                    M35.8811
     1    8X,  24HKUT(1:131) FOLLOWS .....  ,/,  1X,  131A1  )          M35.8812
      RETURN                                                            M13.3841
      END                                                               M13.3842
      SUBROUTINE FLTDAT ( ARRAY,  ARRSAV,  SOLN,  RHS,  YMAT,           M32.7157
     1                KSPARS,   MAXSQ, NMAX2, NMAX, LSPARS )            M32.7158
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  ARRAY(MAXSQ,MAXSQ), ARRSAV(MAXSQ,MAXSQ)                M32.7159
      DIMENSION  SOLN(MAXSQ), RHS(MAXSQ), YMAT(NMAX,NMAX2)              M32.7160
      DIMENSION  KSPARS(LSPARS,LSPARS)                                  M32.7161
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      REAL*8        TEXTA, TEXTB, TEXT1, TEXT2, CHARA                   M32.7163
      REAL*8        CHARB, CHARC, CHARQ, TEXT80, TEXT9                  M35.8813
C     FOLLOWING DIMENSIONED VECTOR WITH "35" REALLY SHOULD BE           M32.7165
C     VARIABLY-DIMENSIONED TO "IOFBND" USER-REQUESTED SIZE.  BUT        M32.7166
C     IT IS EASIER TO LEAVE THEM FIXED, BIGGER THAN EVER NEEDED:        M32.7167
      DIMENSION  RTHEV(35),  OUTPUT(35), NEGATE(35), XTHEV(35)          M32.7168
      DIMENSION  TEXTA(35), TEXTB(35), RZERO(35), XZERO(35)             M32.7169
      DIMENSION  ICHARA(35), ICHARB(35), ADMIT(35)                      M32.7170
      DIMENSION  TEXT80(14), TEXT1(6), TEXT2(6), TEXT9(1)               M35.8814
      DATA  KORIG  / 1HX  /                                             M32.7172
      DATA  KFILL  / 1HF  /                                             M32.7173
      DATA KBLANK  /  1H  /                                             M32.7174
      DATA  CHARA   / 6HA        /                                      M32.7175
      DATA  CHARB   / 6HB        /                                      M32.7176
      DATA  CHARC   / 6HC        /                                      M32.7177
      DATA  CHARQ   / 6H?        /                                      M32.7178
      LL35 = 35                                                                 
      DRIVE = 1.0                                                               
      LIMIT = MAXSQ**2                                                  M32.7181
 5270 LOOP = 1                                                                  
      CALL MOVE0 ( NEGATE(1), LL35 )                                            
C     ENTER LOOP OVER SEQUENCE NETWORKS (ZERO FIRST, THEN POSITIVE):    M32.7184
 5286 N13 = 2 * MAXSQ                                                           
      CALL MOVER0 ( YMAT(1,1), N13 )                                            
      IPRSUP = IPRSOV(NCHAIN)                                                   
C     READ INPUT CARD USING CIMAGE.                                     M32.7188
      CALL CIMAGE                                                       M32.7189
      DECODE (80, 5306, ABUFF)  NITMAX, N13, EPSILN, PERCEN,            M32.7190
     1                  IPUNCH, IPNCOM, BUS1, BUS2, BUS3, BUS4          M32.7191
 5306 FORMAT ( 2I8, 2E8.0, 2I8, 4X, 4A1 )                               M32.7192
      IF ( NITMAX .GT. 0   .OR.                                                 
     1     EPSILN  .GT.  0.0 )                                                  
     2 GO TO 5311                                                               
      WRITE (LUNIT6, 5308)                                              M32.7196
 5308 FORMAT ( 38H+BLANK CARD TERMINATES "FDU" SUBCASES.   )            M32.7197
      GO TO 9900                                                                
 5311 WRITE (LUNIT6, 5314)  NITMAX, N13, EPSILN, PERCEN,                M32.7199
     1                      IPNCOM, BUS1, BUS2, BUS3, BUS4              M32.7200
 5314 FORMAT (  6H+MISC.,  2I5, 2E12.3, I4,  1X,  4A1  )                M32.7201
      IF ( IPRSUP .LE. 0 )                                                      
     1 IPRSUP = N13                                                             
      IF ( IPRSUP .LE. 0 )  IPRSUP = 1                                          
      IF ( BUS1 .NE. BLANK )  CHARA = BUS1                                      
      IF ( BUS2 .NE. BLANK )  CHARB = BUS2                                      
      IF ( BUS3 .NE. BLANK )  CHARC = BUS3                                      
      IF ( BUS4 .NE. BLANK )  CHARQ = BUS4                                      
      SCALE = PERCEN                                                            
      NTOT = 0                                                                  
C     ENTER LOOP WITH TOP AT 5321 WHERE ANOTHER BRANCH CARD IS READ:    M32.7211
C     READ INPUT CARD USING CIMAGE.                                     M32.7212
 5321 CALL CIMAGE                                                       M32.7213
      DECODE (80, 5326, ABUFF)   K, M, D6                                       
 5326 FORMAT ( 2I8, E8.0 )                                              M32.7215
      IF ( K  .LE.  0     .OR.                                          M32.7216
     1     M  .LE.  0     .OR.                                          M32.7217
     2    D6  .LE.  0 )   GO TO 5347                                    M32.7218
      IF ( K  .GT.  NTOT )   NTOT = K                                           
      IF ( M  .GT.  NTOT )   NTOT = M                                           
      IF ( NTOT  .LE.  NMAX )  GO TO 5335                               M32.7221
      WRITE (LUNIT6, 5331)  NTOT                                        M32.7222
 5331 FORMAT ( /,  45H  @@@@@@@  OVERFLOW ERROR STOP.  GENERATOR OR,    M33.2134
     1             35H NODE NUMBER IS TOO LARGE.   NTOT =,   I5  )      M33.2135
      KILL = 1                                                          M32.7225
      LSTAT(19) = 5331                                                  M32.7226
      LSTAT(16) = 23                                                    M32.7227
      GO TO 9900                                                        M32.7228
 5335 WRITE (LUNIT6, 5336)  K, M, D6                                    M32.7229
 5336 FORMAT (  22H+BRANCH CARD.  NODES =,  2I4,  6H   X =,  E13.4 )    M32.7230
      D6 = 1.0 / D6                                                             
      YMAT(K,M) = -D6                                                           
      YMAT(M,K) = -D6                                                           
      YMAT(K,K) = YMAT(K,K) + D6                                                
      YMAT(M,M) = YMAT(M,M) + D6                                                
      GO TO 5321                                                                
 5347 WRITE (LUNIT6, 5348)                                              M32.7237
 5348 FORMAT ( 35H+BLANK CARD TERMINATING LINE CARDS.   )               M32.7238
      NTOT2 = 2 * NTOT                                                          
      IF ( IPRSUP  .GE.  2 )                                            M32.7240
     1 WRITE (LUNIT6, 5354)  NTOT                                       M32.7241
 5354 FORMAT (   36H ADMITTANCE MATRIX FOLLOWS.   NTOT =,  I3  )        M33.2136
      NTOTSQ = NTOT * NTOT                                                      
      NTOTP1 = NTOT + 1                                                         
      NTOTM1 = NTOT - 1                                                         
      MAXCOL = 2 * NTOT - 1                                                     
      DO 5375  J=1, NTOT                                                        
      N12 = NTOT + J                                                            
      YMAT(J,N12) = 1.0                                                         
C     READ INPUT CARD USING CIMAGE.                                     M32.7250
      CALL CIMAGE                                                       M32.7251
      DECODE (80, 5357, ABUFF) L, TEXTA(L), TEXTB(L), RTHEV(L),         M32.7252
     1                          XTHEV(L), N13                           M32.7253
 5357 FORMAT ( I2, 2A6, 2E10.0, I6 )                                    M32.7254
      DECODE (80, 5358, ABUFF)  TEXT1, TEXT2                            M32.7255
 5358 FORMAT ( 2X, 12A1 )                                               M32.7256
      WRITE (LUNIT6, 5360)  L, RTHEV(L), XTHEV(L), N13                  M32.7257
 5360 FORMAT ( 10H+THEVENIN.,  I6, 2E13.4, I6  )                        M32.7258
      IF ( NEGATE(L) .EQ. 0 )  NEGATE(L) = N13                          M32.7259
      N4 = 0                                                                    
      DO 5363  M=1, 6                                                           
      IF ( TEXT1(M) .NE. CHARQ )  GO TO 5361                            M32.7262
      ICHARA(L) = M                                                     M32.7263
      N4 = N4 + 1                                                               
 5361 IF ( TEXT2(M) .NE. CHARQ )  GO TO 5363                            M32.7265
      ICHARB(L) = M                                                     M32.7266
      N4 = N4 + 1                                                               
 5363 CONTINUE                                                          M32.7268
      IF ( N4 .GE. 2 )  GO TO 5368                                      M32.7269
      ICHARA(L) = 6                                                     M32.7270
      ICHARB(L) = 6                                                     M32.7271
      DO 5366  K=2, 6                                                   M32.7272
      IF ( TEXT1(K) .EQ. BLANK   .AND.                                  M32.7273
     1     TEXT1(K-1) .EQ. CHARA )  ICHARA(L) = K-1                     M32.7274
      IF ( TEXT2(K) .EQ. BLANK   .AND.                                  M32.7275
     1     TEXT2(K-1) .EQ. CHARA )  ICHARB(L) = K-1                     M32.7276
 5366 CONTINUE                                                          M32.7277
 5368 ADMIT(L) = 1.0 / XTHEV(L)                                                 
      IF ( IPRSUP  .GE.  2 )  WRITE (LUNIT6, 5371)                      M32.7279
     1     L, RTHEV(L), XTHEV(L),   L, (YMAT(L,L), L=1, NTOT)           M32.7280
 5371 FORMAT ( 4H ROW, I2, 9H.   X/R =, F11.3, 10H   XTHEV =, F11.3,    M33.2137
     1  5H   Y(,   I2, 10H,1:NTOT) =,  3E13.5  ,/, 1X,  10E13.5  )      M33.2138
 5375 CONTINUE                                                          M32.7283
C     NEXT ASSEMBLE CONSTANT PART OF JACOBIAN IN ARRSAV ARRAY.  THIS    M32.7284
C     IS TOP OF LOOP OVER ADMITTANCE LEVEL (FOR SEQUENTIAL ADJUSTMENT   M32.7285
C     OF [Y] TO MINIMIZE SHOCK TO NEWTON SOLUTION):                     M32.7286
 5379 N7 = 0                                                            M32.7287
      CALL MOVER0 ( ARRSAV(1,1), LIMIT )                                M32.7288
      DO 5422  KK=1, NTOT                                                       
      N8 = N7                                                           M32.7290
      DO 5408  I=1, NTOT                                                        
      N7 = N7 + 1                                                       M32.7292
      IF ( SCALE .GT. PERCEN )   GO TO 5385                             M32.7293
      SOLN(N7) = 0.0                                                            
      IF ( I .EQ. KK )                                                          
     1 SOLN(N7) = ADMIT(KK)                                                     
 5385 N4 = ( I - 1 ) * NTOT  +  I                                       M32.7297
C     ARRSAV(N7,N4) = -GEN(I)          ! dF(I)/dYS(I) = -VS(I)          M32.7298
      IF ( LSPARS .EQ. 1 )  GO TO 5395                                          
      DO 5394  L=1, NTOTSQ                                              M32.7300
 5394 KSPARS(N7,L) = KBLANK                                             M32.7301
      KSPARS(N7,N4) = KORIG                                             M32.7302
 5395 IF ( I .EQ. KK )                                                          
     1 ARRSAV(N7,N7) = ARRSAV(N7,N7) + DRIVE                                    
      DO 5402  J=1, NTOT                                                        
      IF ( J .EQ. KK ) GO TO 5402                                               
      ARRSAV(N7,N8+J) = SCALE*YMAT(I,J)                                         
      IF ( LSPARS .NE. 1 )                                                      
     1 KSPARS(N7,N8+J) = KORIG                                                  
 5402 CONTINUE                                                                  
 5408 CONTINUE                                                                  
 5422 CONTINUE                                                                  
      IF ( IPRSUP  .GE.  2 )                                            M32.7313
     1 WRITE (LUNIT6, 5425)  ( SOLN(J), J=1, NTOTSQ )                   M32.7314
 5425 FORMAT ( 31H INITIAL GUESS SOLN(1:NTOTSQ) =,   6E15.6  ,/,        M33.2139
     1  1X,  8E15.6 )                                                   M32.7316
      IF ( IPRSUP  .GE.  2 )                                            M32.7317
     1 WRITE (LUNIT6, 5437)                                             M32.7318
 5437 FORMAT ( /,  35H PARTIAL CONSTANT JACOBIAN FOLLOWS.  )            M33.2140
      DO 5447  I=1, NTOTSQ                                              M32.7320
      IF ( IPRSUP  .GE.  3 )                                            M32.7321
     1 WRITE (LUNIT6, 5443)  ( ARRSAV(I,J), J=1, NTOTSQ )               M32.7322
 5443 FORMAT ( 1X, 10E13.5 )                                            M32.7323
 5447 CONTINUE                                                          M32.7324
      IF ( IPRSUP  .LE. 1 )  GO TO 5458                                 M32.7325
      IF ( LSPARS .EQ. 1 )   GO TO 5458                                         
      WRITE (LUNIT6, 5753)                                                      
      DO 5457  K=1, NTOTSQ                                                      
      WRITE (LUNIT6, 5755)  K,  ( KSPARS(K,L), L=1, NTOTSQ )            M32.7329
 5457 CONTINUE                                                          M32.7330
 5458 NITER = 0                                                                 
      WRITE (LUNIT6, 5443)                                                      
      GO TO 5723                                                                
C     TOP OF LOOP FOR NEXT NEWTON ITERATION:                            M32.7334
 5481 CALL MOVER ( ARRSAV(1,1), ARRAY(1,1), LIMIT )                             
      N7 = 0                                                                    
C     INSIDE DO 5506, RHS(I) IS FUNCTION F(I);  NEWTON "-" ADDED AT 5529M32.7337
      DO 5506  KK=1, NTOT                                                       
      N8 = N7                                                                   
      DO 5502  I=1, NTOT                                                        
      N7 = N7 + 1                                                               
      N4 = ( I - 1 ) * NTOT + I                                                 
      IF ( I .EQ. KK )                                                          
     1 RHS(N7) = ( SOLN(N7) - ADMIT(I) ) * DRIVE                                
      IF ( I .NE. KK )                                                          
     1 RHS(N7) = SOLN(N4) * SOLN(N7)                                            
C     ENTER LOOP TO EVALUATE SUM OVER J OF Y(I,J)*V(J) FOR J .NE. I:    M32.7347
      D13 = YMAT(I,KK)*DRIVE                                                    
      N2 = N8                                                                   
      DO 5492  J=1, NTOT                                                        
      N2 = N2 + 1                                                               
      IF ( J .EQ. KK )  GO TO 5492                                              
      D13 = D13 + YMAT(I,J) * SOLN(N2)                                          
 5492 CONTINUE                                                                  
      IF ( IPRSUP .GE. 6 )                                              M32.7355
     1 WRITE (LUNIT6, 5496)  KK, I, N4, N7, N2,  SCALE, RHS(N7), D13    M32.7356
 5496 FORMAT ( 33H BELOW 5492.  KK, I, N4, N7, N2 =,   5I5  ,/,         M33.2141
     1         35H              SCALE, RHS(N7), D13 =,   3E15.5  )      M33.2142
      RHS(N7) = RHS(N7) + SCALE * D13                                           
      IF ( I .EQ. KK )  GO TO 5502                                              
      ARRAY(N7,N7) = ARRAY(N7,N7) + SOLN(N4)                                    
      ARRAY(N7,N4) = ARRAY(N7,N4) + SOLN(N7)                                    
      IF ( IPRSUP .LE. 1 )  GO TO 5502                                  M32.7363
      IF ( LSPARS .EQ. 1 )  GO TO 5502                                          
      KSPARS(N7,N7) = KORIG                                                     
      KSPARS(N7,N4) = KORIG                                             M32.7366
 5502 CONTINUE                                                                  
 5506 CONTINUE                                                                  
      IF ( IPRSUP  .GE.  2 )                                            M32.7369
     1 WRITE (LUNIT6, 5517)                                             M32.7370
 5517 FORMAT ( /,  44H FINAL JACOBIAN AFTER VARIABLE PART HAS BEEN,     M33.2143
     1             47H ADDED.  RHS = FUNCTION EVALUATION (ALSO DONE).)  M33.2144
      DO 5529  I=1, NTOTSQ                                                      
      IF ( IPRSUP  .GE.  4 )                                            M32.7374
     1 WRITE (LUNIT6, 5527)  I,  RHS(I), ( ARRAY(I,J), J=1, NTOTSQ )    M32.7375
 5527 FORMAT (   I3,   7H. RHS =,   E15.6,   18H   Y(I,1:NTOTSQ) =,     M33.2145
     1  6E15.6   ,/,  1X, 8E15.6 )                                      M32.7377
 5529 RHS(I) = -RHS(I)                                                          
      IF ( IPRSUP  .LE.  1 )  GO TO 5538                                M32.7379
      IF ( LSPARS  .EQ.  1 )  GO TO 5538                                        
      WRITE (LUNIT6, 5753)                                              M32.7381
      DO 5537  K=1, NTOTSQ                                              M32.7382
      WRITE (LUNIT6, 5755)  K,  ( KSPARS(K,L), L=1, NTOTSQ )            M32.7383
 5537 CONTINUE                                                          M32.7384
 5538 DO 5585  I=1, NTOTSQ                                                      
      IF ( I .EQ. 1 )  GO TO 5563                                               
      N6 = I - 1                                                                
      DO 5561  J=1, N6                                                          
      IF ( ARRAY(I,J) .EQ. 0.0 )  GO TO 5561                                    
      IF ( LSPARS .EQ. 1 )  GO TO 5541                                          
      IF ( KSPARS(I,J) .EQ. KBLANK )                                            
     1 KSPARS(I,J) = KFILL                                                      
 5541 D3 = ARRAY(I,J)                                                           
      ARRAY(I,J) = 0.0                                                          
      N8 = J + 1                                                                
      DO 5548  L=N8, NTOTSQ                                                     
 5548 ARRAY(I,L) = ARRAY(I,L) - D3 * ARRAY(J,L)                                 
      RHS(I) = RHS(I) - D3 * RHS(J)                                             
      IF ( IPRSUP  .GE.  5 )                                            M32.7399
     1 WRITE (LUNIT6, 5549)  I, J, RHS(I), ( ARRAY(I,L), L=1, NTOTSQ )  M32.7400
 5549 FORMAT (  7H ZERO (,   I2,  1H,,  I2,  11H). RHS(I) =,   E15.6,   M33.2146
     1   14H   A(I,1:N9) =,   5E15.6  ,/,  1X,  8E15.6  )               M33.2147
 5561 CONTINUE                                                                  
 5563 IF ( IPRSUP  .GE.  4 )                                            M32.7404
     1 WRITE (LUNIT6, 5564)  I, I, ARRAY(I,I)                           M32.7405
 5564 FORMAT ( 39H DIAGONAL READY TO BE RECIPROCATED,  A(,   I2,        M33.2148
     1  1H,,  I2,   3H) =,   E15.6  )                                   M33.2149
      D4 = 1.0 / ARRAY(I,I)                                                     
      ARRAY(I,I) = 1.0                                                          
      IF ( I .EQ. NTOTSQ )  GO TO 5574                                          
      N8 = I + 1                                                                
      DO 5572  L=N8, NTOTSQ                                                     
      IF ( ARRAY(I,L) .EQ. 0.0 )  GO TO 5572                            M32.7413
      ARRAY(I,L) = D4 * ARRAY(I,L)                                              
      IF ( LSPARS .EQ. 1 )  GO TO 5572                                          
      IF ( KSPARS(I,L) .EQ. KBLANK )                                    M32.7416
     1 KSPARS(I,L) = KFILL                                              M32.7417
 5572 CONTINUE                                                          M32.7418
 5574 RHS(I) = D4 * RHS(I)                                                      
      IF ( IPRSUP  .GE.  3 )                                            M32.7420
     1 WRITE (LUNIT6, 5576) I, RHS(I), (ARRAY(I,J), J=1, NTOTSQ)        M32.7421
 5576 FORMAT ( 10H FINAL ROW,   I3,   8H.  RHS =,   E15.6,              M33.2150
     1  18H   A(I,1:NTOTSQ) =,   5E15.6  ,/,  1X,  8E15.6  )            M33.2151
 5585 CONTINUE                                                          M32.7424
      IF ( IPRSUP  .GE.  2 )                                            M32.7425
     1 WRITE (LUNIT6, 5594)                                             M32.7426
 5594 FORMAT ( 44H DONE WITH DOWNWARD, BEGIN BACKSUBSTITUTION.  )       M33.2152
      I = NTOTSQ                                                        M32.7428
 5614 I = I - 1                                                                 
      N8 = I + 1                                                        M32.7430
      DO 5627  J=N8, NTOTSQ                                                     
      IF ( ARRAY(I,J) .EQ. 0.0 )  GO TO 5627                            M32.7432
      RHS(I) = RHS(I) - ARRAY(I,J) * RHS(J)                                     
 5627 CONTINUE                                                          M32.7434
      IF ( I .GT. 1 )  GO TO 5614                                               
      IF ( IPRSUP  .GE.  2 )                                            M32.7436
     1 WRITE (LUNIT6, 5697)  ( RHS(J), J=1, NTOTSQ )                    M32.7437
 5697 FORMAT ( 28H COMPLETE CORRECTION VECTOR:, 6E15.6 ,/, 1X, 8E15.6 ) M33.2153
      D4 = 0.0                                                                  
      DO 5706  J=1, NTOTSQ                                              M32.7440
      IF ( ABSZ( RHS(J) ) .GT. D4 )                                             
     1 D4 = ABSZ( RHS(J) )                                                      
 5706 SOLN(J) = SOLN(J) + RHS(J)                                                
      IF ( IPRSUP  .GE.  1 )                                            M32.7444
     1 WRITE (LUNIT6, 5714)  NITER, D4                                  M32.7445
 5714 FORMAT ( 48H LARGEST CORRECTION AT NEWTON ITERATION  NITER =,     M33.2154
     1  I3,  10H  IS  D4 =,   E15.6  )                                  M33.2155
      IF ( IPRSUP  .GE.  2 )                                            M32.7448
     1 WRITE (LUNIT6, 5716)  ( SOLN(I), I=1, NTOTSQ )                   M32.7449
 5716 FORMAT ( 29H CORRECTED SOLUTION ESTIMATE:, 6E15.6 ,/, 1X, 8E15.6 )M33.2156
      IF ( D4 .LT. EPSILN )  GO TO 5729                                         
 5723 NITER = NITER + 1                                                         
      IF ( NITER .LE. NITMAX )  GO TO 5481                                      
      WRITE (LUNIT6, 5727)  NITMAX,  D4,  SCALE                         M32.7454
 5727 FORMAT (  /,  46H ***********  NONCONVERGENCE ERROR.  ITERATION,  M33.2157
     1              35H LIMIT HAS BEEN REACHED.   NITMAX =,   I3        M33.2158
     2         ,/,  26H   PEAK CORRECTION (ABS) =,   E15.6,             M33.2159
     3              33H   FOR ADMITTANCE LEVEL "SCALE" =,   F6.3 )      M33.2160
      GO TO 5778                                                                
 5729 IF ( ABSZ( SCALE - 1.0 )  .GT.  EPSILN )   GO TO 5782             M34. 175
C     PROBLEM IS SOLVED;  ONLY CONVERSION FROM "Y" TO "X" REMAINS:      M32.7461
      WRITE (LUNIT6, 5737)                                              M32.7462
 5737 FORMAT ( /, 44H FINAL GENERATOR EQUIVALENT IMPEDANCES, PLUS,      M33.2161
     1            47H CONFIRMATION OF ORIGINAL GENERATOR PARAMETERS.,   M33.2162
     1  /,       42H RESISTANCE IS IGNORED DURING THE INTERNAL,         M33.2163
     1            47H COMPUTATION, AND ADDED AS FIXED % ONLY AT END.,   M33.2164
     1  /,       44H X-FAULT IS SHORT CIRCUIT REACTANCE COMPUTED,       M33.2165
     2            42H AFTER REMOVAL OF USER-FLAGGED GENERATORS.,        M33.2166
     2  /,  3X, 3HGEN,  10X, 5HX-NEW,  10X, 5HR-NEW, 8X,  7HX-FAULT,    M33.2167
     3       9X, 6HX-THEV,  7X, 8HX/R-THEV  )                           M32.7470
      N18 = 0                                                                   
      REWIND LUNIT1                                                             
C     NEXT WE ADD Y-GEN TO [Y], TO CALCULATE SHORT CIRCUIT CURRENT:     M32.7473
      DO 5734  J=1, NTOT                                                        
      IF ( NEGATE(J) .NE. 0 )  GO TO 5734                                       
      N8 = ( J - 1 ) * NTOT  +  J                                               
      YMAT(J,J) = YMAT(J,J) + SOLN(N8)                                          
 5734 CONTINUE                                                                  
C     NEXT WE INVERT TOTAL [Y], TO GIVE S.S. CURRENTS AS DIAGONALS:     M32.7479
      DO 6753  K=1, NTOT                                                        
      D3 = 1.0 / YMAT(K,K)                                                      
      N6 = K + 1                                                                
      DO 6716  L=N6, NTOT2                                                      
 6716 YMAT(K,L) = YMAT(K,L) * D3                                                
      DO 6748  M=1, NTOT                                                        
      IF ( M .EQ. K )  GO TO 6748                                               
      D2 = YMAT(M,K)                                                            
      DO 6737  L=N6, NTOT2                                                      
 6737 YMAT(M,L) = YMAT(M,L) - D2 * YMAT(K,L)                                    
 6748 CONTINUE                                                                  
 6753 CONTINUE                                                                  
C     DONE WITH YMAT INVERSION;  OUTPUT GENERATOR GOODIES:              M32.7492
      DO 5746  J=1, NTOT                                                        
      N8 = ( J - 1 ) * NTOT  +  J                                               
      OUTPUT(J) = FLTINF                                                        
      IF ( SOLN(N8) .NE. 0.0 )                                                  
     1 OUTPUT(J) = 1.0 / SOLN(N8)                                               
      D7 = OUTPUT(J) / RTHEV(J)                                                 
      N2 = NTOT + J                                                             
      WRITE (LUNIT6, 5739) J, OUTPUT(J), D7, YMAT(J,N2),                M32.7500
     1                     XTHEV(J), RTHEV(J)                           M32.7501
 5739 FORMAT ( 1X, I5, 4E15.6,  F15.3  )                                M32.7502
      IF ( NEGATE(J) .EQ. 1 )                                                   
     1 OUTPUT(J) = FLTINF                                                       
      D7 = OUTPUT(J) / RTHEV(J)                                                 
      IF ( LOOP .EQ. 2 )  GO TO 5740                                            
      RZERO(J) = D7                                                     M32.7507
      XZERO(J) = OUTPUT(J)                                              M32.7508
      GO TO 5746                                                        M32.7509
C     WITH BOTH Z-GROUND AND Z-SKY MODES NOW KNOWN, PUNCH BRANCH CARDS: M32.7510
 5740 IF ( NEGATE(J) .NE. 0 )  GO TO 5746                                       
      N6 = 51                                                                   
      N4 = ICHARA(J)                                                    M32.7513
      N5 = ICHARB(J)                                                    M32.7514
      TEXT9(1) = CHARA                                                          
      CALL PACKA1 ( TEXT9(1), TEXTA(J), N4 )                            M35.8816
      CALL PACKA1 ( TEXT9(1), TEXTB(J), N5 )                            M35.8817
      IF ( IPNCOM .EQ. 0 )  GO TO 5743                                          
      WRITE (LUNIT7, 5741)                                              M32.7518
 5741 FORMAT ( 1HC )                                                    M32.7519
      WRITE (LUNIT1, 5742)                                              M32.7520
 5742 FORMAT ( 2H C  )                                                  M32.7521
      N18 = N18 + 1                                                             
 5743 IF ( IPUNCH  .EQ.  0 )                                            M32.7523
     1 WRITE (LUNIT7, 5744)  N6, TEXTA(J), TEXTB(J), RZERO(J), XZERO(J) M32.7524
 5744 FORMAT ( I2, 1H,,  2( A6, 1H, ),  6X,  3H,,,,  E23.14,            M37.7968
     1         1H,,  E23.14,  6H,,,,,,   )                              M37.7969
      WRITE (LUNIT1, 5745)  N6, TEXTA(J), TEXTB(J), RZERO(J), XZERO(J)  M32.7527
 5745 FORMAT ( I3, 1H,,  2( A6, 1H, ),  6X,  3H,,,,  E23.14,            M37.7970
     1         1H,,  E23.14,  6H,,,,,,   )                              M37.7971
      N18 = N18 + 1                                                             
      N4 = ICHARA(J)                                                    M32.7531
      N5 = ICHARB(J)                                                    M32.7532
      TEXT9(1) = CHARB                                                  M35.8818
      CALL PACKA1 ( TEXT9(1), TEXTA(J), N4 )                            M35.8819
      CALL PACKA1 ( TEXT9(1), TEXTB(J), N5 )                            M35.8820
      N6 = 52                                                           M32.7535
      IF ( IPUNCH  .EQ.  0 )                                            M32.7536
     1 WRITE (LUNIT7, 5744)  N6, TEXTA(J), TEXTB(J), D7, OUTPUT(J)      M32.7537
       WRITE (LUNIT1, 5745)  N6, TEXTA(J), TEXTB(J), D7, OUTPUT(J)      M32.7538
      N18 = N18 + 1                                                             
      TEXT9(1) = CHARC                                                  M35.8821
      CALL PACKA1 ( TEXT9(1), TEXTA(J), N4 )                            M35.8822
      CALL PACKA1 ( TEXT9(1), TEXTB(J), N5 )                            M35.8823
      N6 = 53                                                           M32.7542
      IF ( IPUNCH  .EQ.  0 )                                            M32.7543
     1 WRITE (LUNIT7, 2744)  N6, TEXTA(J), TEXTB(J)                     M32.7544
 2744 FORMAT ( I2, 1H,,  2( A6, 1H, ),  6X,  10H,,,,,,,,,,   )          M32.7545
      WRITE (LUNIT1, 2745)  N6, TEXTA(J), TEXTB(J)                      M32.7546
 2745 FORMAT ( I3, 1H,,  2( A6, 1H, ),  6X,  10H,,,,,,,,,,   )          M32.7547
      N18 = N18 + 1                                                             
 5746 CONTINUE                                                                  
      IF ( IPRSUP .LE. 1 )  GO TO 5764                                  M32.7550
      IF ( LSPARS .EQ. 1 )  GO TO 5764                                          
      WRITE (LUNIT6, 5753)                                              M32.7552
 5753 FORMAT ( /,  44H SPARSITY PATTERN OF TRIANGULARIZED JACOBIAN,     M33.2168
     1             15H FOLLOWS  .....,  /,  6X,  10H1234567890,         M33.2169
     2 60H123456789012345678901234567890123456789012345678901234567890 )M33.2170
      DO 5756  K=1, NTOTSQ                                              M32.7556
      WRITE (LUNIT6, 5755)  K,  ( KSPARS(K,L), L=1, NTOTSQ )            M32.7557
 5755 FORMAT (  I3,   3H : ,   120A1  )                                 M33.2171
 5756 CONTINUE                                                          M32.7559
 5764 LOOP = LOOP + 1                                                   M32.7560
      WRITE (LUNIT6, 5443)                                                      
      IF ( LOOP .LE. 2 )  GO TO 5286                                            
      IF ( IPUNCH  .NE.  0 )  GO TO 5778                                        
      WRITE (LUNIT6, 5767)                                              M32.7564
 5767 FORMAT ( 46H 80-COLUMN CARD IMAGES OF COUPLED R-L BRANCHES,       M32.7565
     1         47H THAT MAY BE PUNCHED ON LUNIT7 (IF IPUNCH = 0).       M32.7566
     2        ,/, 41H 1234567890123456789012345678901234567890,         M32.7567
     3             40H1234567890123456789012345678901234567890          M32.7568
     4        ,/, 41H ----------------------------------------,         M32.7569
     5            40H----------------------------------------    )      M32.7570
      REWIND LUNIT1                                                     M32.7571
      DO 5773  J=1, N18                                                         
      READ (LUNIT1, 5770)  TEXT80                                               
 5770 FORMAT ( 13A6, A3 )                                                       
 5773 WRITE (LUNIT6, 5770)  TEXT80                                      M32.7575
 5778 GO TO 5270                                                                
 5782 SCALE = SCALE + PERCEN                                                    
      IF ( SCALE .GT. 1.0 )  SCALE = 1.0                                        
      GO TO 5379                                                                
 9900 IF ( IPRSOV(NCHAIN) .GE. 1 )                                      M32.7580
     1 WRITE (LUNIT6, 9904)  KILL                                       M32.7581
 9904 FORMAT (  23H EXIT "FLTDAT".  KILL =,  I5  )                      M32.7582
      RETURN                                                            M32.7583
      END                                                               M32.7584
      SUBROUTINE STATRS                                                 M31.3699
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     INSTALLATION-DEPENDENT MODULE OF OVERLAY 29,  CALLED ONLY         M31.3700
C     FOR  "TABULATE ENERGIZATION RESULTS"  USAGE.                      M31.3701
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      CHARACTER*20 FILN20                                               M31.3703
      BYTE FILNAM(20), C1                                               M31.3704
      EQUIVALENCE (FILN20, FILNAM(1) )                                  M31.3705
      DATA  C1  /  1H   /                                               M31.3706
      IF ( IPRSUP  .GE.  1 )                                            M31.3707
     1 WRITE (LUNIT6, 1793)  ( LSTAT(I), I=13, 16 )                     M31.3708
 1793 FORMAT ( ' TOP OF "STATRS" .   LSTAT(13:16) =',  4I5  )           M31.3709
      ENCODE (14, 1804, FILNAM(1))   ( LSTAT(I), I=13, 16 )             M31.3710
 1804 FORMAT ( 2HST, I1, 3HLOG, 3I1, 5H.DAT  )                          M31.3711
      DO 4256 J = 15, 20                                                M31.3712
 4256 FILNAM(J) = C1                                                    M31.3713
      CLOSE (UNIT=LUNIT1)                                               M31.3714
      IF ( LSTAT(13)  .EQ.  0 )   GO TO 9000                            M31.3715
      IF ( IPRSUP .GE. 1 )                                              M31.3716
     1 WRITE (LUNIT6, 1808) FILNAM                                      M31.3717
 1808 FORMAT ( ' IN "STATRS", B4 OPEN OF UNIT LUNIT1 .' , 20A1)         M31.3718
      OPEN ( UNIT=LUNIT1, TYPE='OLD', NAME=FILN20,                      M31.3719
     1      FORM='UNFORMATTED' )                                        M31.3720
      WRITE (LUNIT6, 1822)  LSTAT(13), FILNAM                           M31.3721
 1822 FORMAT ( 20X,                                                     M31.3722
     1  '----  SUCCESSFUL  OPEN   OF  LUNIT',  I1,  '  DATA',           M31.3723
     2  ' ON DISK.   PERMANENT FILE NAME = ', 20A1, 2H .   )            M31.3724
 9000 RETURN                                                            M31.3725
      END                                                               M31.3726
