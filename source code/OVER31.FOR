      SUBROUTINE SUBR31                                                 M22.5507
      IMPLICIT REAL*4 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
C     FLAG-1.   BEGIN CLASS-1  /BLANK/  VARIABLES                       M10.   1
C     FLAG-2.   BEGIN CLASS-2  /BLANK/  VARIABLES                       M10.   3
C               (FLOATING-POINT NUMERIC USAGE ONLY, WITH SCALARS        M10.   4
C                PRECEDING ARRAYS).                                     M10.   5
      REAL*8            CI1, CK1, DELTAT, DELTA2, FREQCS                M42.   4
      REAL*8           EPSILN, XUNITS, AINCR, XMAXMX                    M42.   5
      REAL*8           ZNOLIM   , EPSZNO, EPWARN, EPSTOP, T, TOLMAT     M38.   1
      REAL*8           TWOPI, TMAX, OMEGA, COPT, XOPT, SZPLT            M13.   1
      REAL*8          SZBED, SGLFIR, SIGMAX,  EPSUBA,  EPDGEL,  EPOMEG  M19.   2
      REAL*8        FMINFS, DELFFS, FMAXFS, TENERG, BEGMAX              M31.   4
      REAL*8          TENM3, TENM6, UNITY, ONEHAF, PEAKND               M27.   1
      REAL*8  FLTINF, FLZERO, DEGMIN, DEGMAX, STATFR, VOLTBC            M31.   5
      REAL*8          FLSTAT    , ANGLE, PU, DLTINV, SPEEDL             M39.   1
C     FLAG-3.   BEGIN CLASS-3  /BLANK/  VARIABLES                       M10.   7
C               (INTEGER-NUMERIC USAGE ONLY, WITH ARRAYS                M10.   8
C                PRECEDING SCALARS).                                    M10.   9
      INCLUDE  'DECK31.FOR;/NOLIST'                                           
      COMMON   /LDEC31/  KALCOM                                                 
      DIMENSION ARRAY(1), EVDOUB(1)                                     M28.6626
      REAL*8         AUPPER, DAYTIM, TEXTAX, HORZL, SEXT, BUSLST        M09.  41
      REAL*8         SLOT, ALPHA, HEADL, VERTL, BLANKA, BUSVEC          M29.2671
      REAL*8          TEXT1, TEXT2, TEXT3, TEXT4, TEXT5, TEXT6, TEXT7   M09.  43
      REAL*8          TEXT8, CSTXT, PLTLE                               M09.  44
      REAL*8         TEXT9, TEXT10, TEXT11, TEXT12, TEXT13              M09.  45
      REAL*8          TEXT14, TEXT15, TEXT16,         ARCH10            M33.2172
      REAL*8         TEXT18, TEXT19, TEXT20                             M28.6627
      REAL*8         TEXT22, TEXT23, TEXT24, TEXT25, TEXT26             M22.5510
      REAL*8         TEXT27, TEXT28, TEXT29                             M23.5544
      REAL*8         TEXT30, TEXT31, TEXT32, TEXT33                     M28.6628
      REAL*8          TSTEP, XIN, BX, XYPLOT, FLONG1, EVDOUB            M28.6629
C     DECLARATION2   INTD8, INTD9, MAXEV, KARRAY, LLTEMP                M22.5511
C     DECLARATION2   LONG1, LONG2, LONG3, JHMSP                         M28.6630
C     DECLARATION2   MMMIN1, MM0, MM1, MM2, MM3, MM4, MM6               M29.2673
C     DECLARATION2   MM7, MM8, MM9, MM11, MM13                          M28.6632
      DIMENSION    INTD8(150),  INTD9(150),  BXSING(150)                M32.7585
      DIMENSION  ARCH10(2), MULPLT(5), BLANKA(1), BUSVEC(6)             M29.2674
      DIMENSION  ALPHA(52),  MPLOT(4),  HEADL(3),  VERTL(3)             M09.  46
      DIMENSION  BX(1),  SLOT(8),  EV(2),  HORZL(4),  SEXT(13)          M39.3305
      DIMENSION  XIN(150),  BUSLST(1), IBSOUT(1), IBRNCH(1), JBRNCH(1)  M09.  48
      DIMENSION  AUPPER(13), DAYTIM(3), TEXTAX(32), XYPLOT(9)           M28.6633
      DIMENSION  ISWX(4), EVH(4), EVDH(4), ISWW(4), ITIMES(4), KPLTQ(4) M09.  50
      DIMENSION  CSTXT(13), PLTLE(52), XYSHOR(8)                        M28.6634
      DIMENSION  VRTNUM(14),  JPNTR(4),  KPEN(4)                        M19.2122
      DIMENSION  TEXT14(2), TEXT16(2)                                   M29.2675
      DIMENSION  LLTEMP(20)                                             M22.5513
      EQUIVALENCE  ( TSTEP, XIN(1) ),     ( BX(1), XIN(2) )             M09.  53
      EQUIVALENCE  ( KARRAY(1), ARRAY(1), EV(1), BUSLST(1),             M22.5514
     1    EVDOUB(1), IBSOUT(1), IBRNCH(1), JBRNCH(1)  )                 M28.6635
      EQUIVALENCE  ( BUSVEC(1), BUS1 )                                  M29.2676
      EQUIVALENCE   ( MONCAR(2), KBASE )                                        
      DATA  TEXT1   / 6HPRINT  /                                        M09.  54
      DATA  TEXT2   / 6HHEAD O /                                        M09.  55
      DATA  TEXT3   / 6HN      /                                        M09.  56
      DATA  TEXT4   / 6HFF     /                                        M09.  57
      DATA  TEXT5   / 6HSMOOTH /                                        M09.  58
      DATA  TEXT6   / 6HBRANCH /                                        M09.  59
      DATA  TEXT7  / 6HHEIGHT /                                         M09.  60
      DATA  TEXT8   / 6HMARGIN /                                        M09.  61
      DATA  TEXT9   / 6HCALCOM /                                        M09.  62
      DATA  TEXT10  / 6HP PLOT /                                        M09.  63
      DATA  TEXT11  / 6HPRINTE /                                        M09.  64
      DATA  TEXT12  / 6HR PLOT /                                        M09.  65
      DATA  TEXT13  / 6HP PRIN /                                        M09.  66
      DATA  TEXT14(1)   / 6HPLOT T     /                                M29.2677
      DATA  TEXT14(2)   / 6HYPE        /                                M29.2678
      DATA  TEXT15  / 6HPAGE    /                                       M33.2173
      DATA  TEXT16(1)   / 6HNODE N     /                                M29.2679
      DATA  TEXT16(2)   / 6HAMES       /                                M29.2680
      DATA  TEXT18   / 6HPEN CH /                                       M19.2123
      DATA  TEXT19   / 6HOICE   /                                       M19.2124
      DATA  TEXT20   / 6HA1     /                                       M22.5516
      DATA  TEXT22   /  6HPLOT L  /                                     M22.5518
      DATA  TEXT23   /  6HINE LI  /                                     M22.5519
      DATA  TEXT24   /  6HMIT     /                                     M22.5520
      DATA  TEXT25   /  6HSUPERI  /                                     M22.5521
      DATA  TEXT26   /  6HMPOSE   /                                     M22.5522
      DATA  TEXT27   /  6HSCALE   /                                     M23.5546
      DATA  TEXT28   /  6HX-Y PL  /                                     M23.5547
      DATA  TEXT29   /  6HOT      /                                     M23.5548
      DATA  TEXT30   /  6HFOURIE  /                                     M28.6636
      DATA  TEXT31   /  6HR ON    /                                     M28.6637
      DATA  TEXT32   /  6HR OFF   /                                     M28.6638
      DATA  TEXT33   /  6HBOUNDS  /                                     VAX.3004
      DATA   TEXTAX(1)   / 6HDEGREE /                                   M09.  67
      DATA   TEXTAX(2)   / 6HS BASE /                                   M09.  68
      DATA   TEXTAX(3)   / 6HD ON 6 /                                   M09.  69
      DATA   TEXTAX(4)   / 6H0 HZ   /                                   M09.  70
      DATA   TEXTAX(5)   / 6HCYCLES /                                   M09.  71
      DATA   TEXTAX(6)   / 6H BASED /                                   M09.  72
      DATA   TEXTAX(7)   / 6H ON 60 /                                   M09.  73
      DATA   TEXTAX(8)   / 6H HZ    /                                   M09.  74
      DATA   TEXTAX(9)   / 6HSECOND /                                   M09.  75
      DATA   TEXTAX(10)  / 6HS      /                                   M09.  76
      DATA   TEXTAX(11)  / 6H       /                                   M09.  77
      DATA   TEXTAX(12)  / 6H       /                                   M09.  78
      DATA   TEXTAX(13)  / 6HMILLIS /                                   M09.  79
      DATA   TEXTAX(14)  / 6HECONDS /                                   M09.  80
      DATA   TEXTAX(15)  / 6H       /                                   M09.  81
      DATA   TEXTAX(16)  / 6H       /                                   M09.  82
      DATA   TEXTAX(17)  / 6HMICROS /                                   M09.  83
      DATA   TEXTAX(18)  / 6HECONDS /                                   M09.  84
      DATA   TEXTAX(19)  / 6H       /                                   M09.  85
      DATA   TEXTAX(20)  / 6H       /                                   M09.  86
      DATA   TEXTAX(21)  / 6HFREQUE /                                   M19.2125
      DATA   TEXTAX(22)  / 6HNCY IN /                                   M19.2126
      DATA   TEXTAX(23)  / 6H HERTZ /                                   M19.2127
      DATA   TEXTAX(24)  / 6H       /                                   M19.2128
      DATA   TEXTAX(25)  / 6HLOG10  /                                   M19.2129
      DATA   TEXTAX(26)  / 6HFREQUE /                                   M19.2130
      DATA   TEXTAX(27)  / 6HNCY IN /                                   M19.2131
      DATA   TEXTAX(28)  / 6H HERTZ /                                   M19.2132
      DATA   TEXTAX(29)  / 6HSPACE  /                                   M23.5549
      DATA   TEXTAX(30)  / 6HFOR    /                                   M23.5550
      DATA   TEXTAX(31)  / 6HX-Y    /                                   M23.5551
      DATA   TEXTAX(32)  / 6HPLOT   /                                   M23.5552
      DATA  KSCALE  /  0  /                                             VAX.3005
      DATA  LLMIN3 / -3 /                                               M28.6639
      DATA  LLMIN1 / -1 /                                               M28.6640
      DATA  LL0    / 0  /                                               M28.6641
      DATA  LL1    / 1  /                                               M28.6642
      DATA  LL2    / 2  /                                               M28.6643
      DATA  LL3    / 3  /                                               M28.6644
      DATA  LL6    / 6  /                                               M28.6646
      DATA  LL10   / 10 /                                               M28.6650
      DATA  LL16   / 16 /                                               M28.6653
      DATA  LL18   / 18 /                                               M28.6654
      DATA  LL24   / 24 /                                               M28.6655
      DATA  LL78   / 78 /                                               M28.6656
      BLANKA(1) = BLANK                                                 M29.2681
      LONG1 = NCHAIN                                                    M28.6658
      IF ( KBURRO .EQ. 1)  LONG1 =29                                    M28.6659
      CALL DIMENS (LLTEMP(1),LONG1,TRASH,TRASH)                         M28.6660
      DO 5654  I=1, 9999, 2                                             M35.8824
      IF ( LLTEMP(I)  .NE.  0 )   GO TO 5654                            M22.5527
      MAXEV = LLTEMP(I+1) * NBYTE(6) / NBYTE(5)                         M28.6661
      GO TO 5655                                                        M35.8825
 5654 CONTINUE                                                          M22.5529
      CALL STOPTP                                                               
 5655 MAXEV = MAXEV - 8                                                 M35.8827
      C1E12 = 1.E36                                                     M16.4361
C     THE FOLLOWING VARIABLES ARE USED AS FLOATING-POINT ARGUMENTS OF   M09.  90
C     CALCOMP SUBROUTINE CALLS.   THEY ARE JUST CONSTANTS, WHICH ARE    M09.  91
C     DEFINED USING EQUAL SIGNS SO EITHER SINGLE OR DOUBLE PRECISION    M09.  92
C     IS POSSIBLE WITHOUT PROGRAM CHANGES.                              M09.  93
      ZERO = 0.0                                                        M09.  94
      ONE = 1.0                                                         M15.1392
      HGT1 = 0.12                                                       M09.  95
      HGT2 = 0.2                                                        M09.  96
      FOURTH = .25                                                      M09.  97
      HALF = 0.5                                                        M09.  98
      FL1P5 = 1.5                                                       M09.  99
      FLTWO = 2.0                                                       M09. 100
      FL2P5 = 2.5                                                       M09. 101
      FL3 = 3.0                                                         M09. 102
      FL3P5 = 3.5                                                       M28.6662
      FL10 = 10.0                                                       M09. 103
      FL90 = 90.                                                        M09. 106
C     OTHER CALCOMP ARGUMENTS ARE LISTED HERE JUST FOR CONVENIENCE .... M09. 107
C     COUNTP, DLEN, DSTRT, DY, HPI, RICP, SZBED, SZPLT, VMIN, VHS, VHS1 M09. 108
C     THE FOLLOWING ARE LONG INTEGERS (MAY BE DOUBLE LENGTH),           M28.6663
C     USED AS ARGUMENTS OF NON-CALCOMP SUBROUTINE CALLS.  "MM"          M28.6664
      MMMIN1 = -1                                                       M28.6665
      MM0 = 0                                                           M28.6666
      MM1 = 1                                                           M28.6667
      MM2 = 2                                                           M28.6668
      MM3 = 3                                                           M29.2682
      MM4 = 4                                                           M28.6669
      MM6 = 6                                                           M28.6670
      MM7 = 7                                                           M28.6671
      MM8 = 8                                                           M28.6672
      MM9 = 9                                                           M28.6673
      MM11 = 11                                                         M28.6674
      MM13 = 13                                                         M28.6675
      XYPLOT(1) = 0.0                                                   M23.5555
      TOLRCE = .0001                                                    M09. 131
      VS = 1.0                                                          M09. 132
      VHS = 8.0                                                         M09. 133
      VH = 10.0                                                         M09. 134
      TAXMAX = 0.0                                                      M19.2133
      VPLOFF = 0.0                                                      M19.2134
      MRGN = 2                                                          M09. 135
      KBOUND = 0                                                        VAX.3006
      IOUT = 1                                                          M09. 136
      MPAGE = 0                                                         M33.2174
      KPGRID = 0                                                        M19.2135
      MULPLT(1) = 0                                                     M22.5530
      LINLIM = 100                                                      M22.5531
C     DIMENSION OF BUFFER INTEGER ARRAYS   'INTD8'  AND  'INTD9' .      M20.4903
      LIMBIN = 150                                                      M20.4904
      JDUMY = 999                                                       M09. 137
      IF ( IPRSUP  .GE.  1 )                                            M13.3843
     1 WRITE (LUNIT6, 962)  LLBUFF, KILL, LNPIN, NSMTH, SZBED, SZPLT,   M17.1075
     2                 DATE1, TCLOCK                                    M13.3845
  962 FORMAT ( /, 50H BEGIN  'SUBR31' .  LLBUFF    KILL   LNPIN   NSMTH,M13.3846
     1 10X, 5HSZBED, 10X, 5HSZPLT, 5X, 5HDATE1, 4X, 6HTCLOCK, /,        M13.3847
     2 18X, 4I8, 2E15.5,   2(2X, 2A4)   )                               M13.3848
      DO 963  J=1, 4                                                    M28.6676
  963 KPEN(J) = 1                                                       M28.6677
      KPRHD = 1                                                         M19.2138
      NFOUR = 0                                                         M28.6678
      KALCOM = 0                                                        M19.2139
      IF (KILL.EQ.9999) GO TO 2740                                      M09. 143
      LONG1 = 6                                                         M28.6679
      LONG2 = 1                                                         M28.6680
      LONG3 = 1                                                         M28.6681
      DO 980  I=1, 13                                                   M09. 145
      CSTXT(I) = BLANK                                                  M09. 146
      CALL PACKCH ( BLANKA(1), SEXT(1), LONG1, LONG2, LONG3 )           M29.2683
  980 LONG2 = LONG2 + 6                                                 M28.6683
      DO 983 J = 1, 52                                                  M09. 149
  983 PLTLE(J) = BLANK                                                  M09. 150
      COUNTP = 0.0                                                      M09. 151
      IF ( IPRSUP  .GE.  1 )                                            M09. 152
     1 WRITE (LUNIT6, 982)  SEXT                                        M17.1076
  982 FORMAT ( /,  27H VECTOR  'SEXT'  AS  13A6 .,  13A6  ,/, 1X, 13A6 )M19.2140
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.2684
 1000 REWIND LUNIT4                                                     M17.1077
      IF ( IOUT .GE. 2 )  WRITE (LUNIT6, 1005)                          M17.1078
 1005 FORMAT ( ///,1X)                                                  M15.1394
 1020 TSTEP=0.0                                                         M09. 159
      IALF=0                                                            M09. 160
C     READ INPUT CARD USING CIMAGE                                      M09. 162
 1050 CALL CIMAGE                                                       M09. 163
      DECODE (80, 1060, ABUFF)  ITP, AUPPER                             M09. 164
 1060 FORMAT ( I2, 13A6 )                                               M09. 165
      IF ( IPLOT  .LT.  0 )   GO TO 1070                                M13.3849
      IF ( LUNIT4 .LE.  0 )   GO TO 1070                                M29.2685
      IF ( AUPPER(1) .NE. TEXT15 )  GO TO 10452                         M33.2175
      MPAGE = 1                                                         M33.2176
      GO TO 1050                                                        M33.2177
10452 IF ( AUPPER(1) .NE. TEXT9 )  GO TO 1052                           M33.2178
      IF ( AUPPER(2) .NE. TEXT10 )  GO TO 1054                          M09. 167
      IOUT = 1                                                          M09. 168
      WRITE (KUNIT6, 1051)                                              M35.8828
 1051 FORMAT ( 26H+REQUEST FOR CALCOMP PLOT.  )                         M09. 170
      GO TO 1050                                                        M09. 171
 1052 IF ( AUPPER(1) .NE. TEXT11 )  GO TO 1061                          M09. 172
      IF ( AUPPER(2) .NE. TEXT12 )  GO TO 1061                          M09. 173
      IOUT = 2                                                          M09. 174
      WRITE (KUNIT6, 1053)                                              M35.8829
 1053 FORMAT ( 31H+REQUEST FOR LINE PRINTER PLOT.  )                    M09. 176
      GO TO 1050                                                        M09. 177
 1054 IF ( AUPPER(2) .NE. TEXT13 )  GO TO 1061                          M09. 178
      IOUT = 3                                                          M09. 179
      WRITE (KUNIT6, 1055)                                              M35.8830
 1055 FORMAT ( 44H+REQUEST FOR CALCOMP AND LINE PRINTER PLOTS.  )       M09. 181
      GO TO 1050                                                        M09. 182
 1061 IF ( AUPPER(1) .NE. TEXT1 )  GO TO 1067                           M09. 183
      IF ( AUPPER(2)  .NE.  TEXT2 )   GO TO 1067                        M09. 184
      IF ( AUPPER(3)  .NE.  TEXT3 )   GO TO 1064                        M09. 185
      KPRHD = 1                                                         M09. 186
      WRITE (KUNIT6, 1062)                                              M35.8831
 1062 FORMAT(  39H+REQUEST FOR TYPING OF PLOT CHARACTERS.    )          M09. 188
      GO TO 1050                                                        M09. 189
 1064 IF ( AUPPER(3)  .NE.  TEXT4  )   GO TO 1067                       M09. 190
      KPRHD = 0                                                         M09. 191
      WRITE (KUNIT6, 1065)                                              M35.8832
 1065 FORMAT(  40H+REQUEST FOR DRAWING OF PLOT CHARACTERS.    )         M09. 193
      GO TO 1050                                                        M09. 194
 1067 IF ( AUPPER(1) .NE. TEXT33 )   GO TO 5843                         VAX.3007
      KBOUND = 1                                                        VAX.3008
      WRITE (KUNIT6, 4865)                                              VAX.3009
 4865 FORMAT(  41H+SCALE Y AXIS SO NO CURVE EXCEEDS LIMITS.  )          VAX.3010
      GO TO 1050                                                        VAX.3011
 5843 IF ( AUPPER(1)  .NE.  TEXT18 )   GO TO 1079                       VAX.3012
      IF ( AUPPER(2)  .NE.  TEXT19 )   GO TO 1079                       M19.2142
      DECODE (80, 1072, ABUFF(1) )   KPGRID, ISWW                       M19.2143
 1072 FORMAT ( 16X, 5I8 )                                               M19.2144
      WRITE (KUNIT6, 1073)  KPGRID, ISWW                                M35.8833
 1073 FORMAT ( 20H+GRID & PEN CHOICES.,  5I5  )                         M28.6684
      DO 1077  I=1, 4                                                   M19.2147
      IF ( ISWW(I)  .GT.  0 )                                           M19.2148
     1 KPEN(I) = ISWW(I)                                                M19.2149
 1077 CONTINUE                                                          M19.2150
      GO TO 1050                                                        M19.2151
 1079 IF ( AUPPER(1)  .NE.  TEXT22 )   GO TO 7392                       M22.5532
      IF ( AUPPER(2)  .NE.  TEXT23 )   GO TO 7392                       M22.5533
      IF ( AUPPER(3)  .NE.  TEXT24 )   GO TO 7392                       M22.5534
      DECODE (80, 7381, ABUFF(1) )   LINLIM                             M22.5535
 7381 FORMAT ( 24X, 3I8  )                                              M22.5536
      WRITE (KUNIT6, 7386)  LINLIM                                      M35.8834
 7386 FORMAT (  38H+LINE LIMIT FOR SPARSE PRINTER PLOTS =,  I8 )        M22.5538
      GO TO 1050                                                        M22.5539
 7392 IF ( AUPPER(1)  .NE.  TEXT25 )   GO TO 7406                       M22.5540
      IF ( AUPPER(2)  .NE.  TEXT26 )   GO TO 7406                       M22.5541
      DECODE (80, 7381, ABUFF(1) )   MULPLT(1), MULPLT(4), MULPLT(5)    M22.5542
      IF ( MULPLT(1)  .LE.  0 )   MULPLT(1) = 1                         M22.5543
      WRITE (KUNIT6, 7398)  MULPLT(1), MULPLT(4), MULPLT(5)             M35.8835
 7398 FORMAT (  21H+GRAPH SUPERPOSITION.,  3I8  )                       M22.5545
      MULPLT(2) = 0                                                     M22.5546
      MULPLT(3) = 0                                                     M22.5547
      GO TO 1050                                                        M22.5548
 7406 IF ( AUPPER(1)  .NE.  TEXT27 )   GO TO 7627                       M23.5557
      DECODE ( 80, 7613, ABUFF(1) )  D4                                 M23.5558
 7613 FORMAT ( 24X, E8.0 )                                              M23.5559
      WRITE (KUNIT6, 7618)  D4                                          M35.8836
 7618 FORMAT ( 26H+SCALING FACTOR FOR PLOTS.,  F10.5  )                 M23.5561
      D4 = 1.0 / D4                                                     M23.5562
      IF ( KALCOM  .NE. 1 ) GO TO 7622                                  VAX.3013
      CALL FACTOR ( D4 )                                                M23.5563
      GO TO 1050                                                        M23.5564
 7622 KSCALE = 1                                                        VAX.3014
      D4FACT = D4                                                       VAX.3015
      GO TO 1050                                                        VAX.3016
 7627 IF ( AUPPER(1)  .NE.  TEXT28 )   GO TO 7661                       M23.5565
      IF ( AUPPER(2)  .NE.  TEXT29 )   GO TO 7661                       M23.5566
      WRITE (KUNIT6, 7636)                                              M35.8837
 7636 FORMAT (  40H+PLOT ONE EMTP VARIABLE AGAINST ANOTHER.  )          M28.6686
      TEXTAX(29) = AUPPER(3)                                            M23.5570
      TEXTAX(30) = AUPPER(4)                                            M23.5571
      TEXTAX(31) = AUPPER(5)                                            M23.5572
      TEXTAX(32) = AUPPER(6)                                            M23.5573
C     READ INPUT CARD USING CIMAGE                                      M23.5574
      CALL CIMAGE                                                       M23.5575
      IF ( KOLBEG  .GT.  0 )   GO TO 7644                               M23.5576
      DECODE (80, 7641, ABUFF(1) )   ( XYPLOT(I), I=1, 3 )              M23.5577
 7641 FORMAT ( 10E8.0 )                                                 M23.5578
      GO TO 7648                                                        M23.5579
 7644 NFRFLD = 3                                                        M23.5580
      CALL FREFLD ( XYPLOT(1) )                                         M23.5581
 7648 IF ( XYPLOT(1)  .LE.  0.0 )   XYPLOT(1) = 8.0                     M23.5582
      IF ( XYPLOT(1)  .EQ.  9999. ) XYPLOT(1) = 0.0                     M28.6687
      WRITE (KUNIT6, 7649)   ( XYPLOT(I), I=1, 3 )                      M35.8838
 7649 FORMAT ( 8H+X-AXIS.,  3E13.4 )                                    M23.5584
C     READ INPUT CARD USING CIMAGE                                      M23.5585
      CALL CIMAGE                                                       M23.5586
      IF ( KOLBEG  .GT.  0 )   GO TO 7651                               M23.5587
      DECODE (80, 7641, ABUFF(1) )   ( XYPLOT(I), I=4, 9 )              M28.6688
      GO TO 7653                                                        M23.5589
 7651 NFRFLD = 6                                                        M29.2686
      CALL FREFLD ( XYPLOT(4) )                                         M23.5591
 7653 IF ( XYPLOT(4)  .LE.  0.0 )   XYPLOT(4) = 8.0                     M23.5592
      IF ( XYPLOT(7)  .LE.  0.0 )   XYPLOT(7) = 10.0                    M23.5593
      IF ( XYPLOT(9)  .LE.  0.0 )   XYPLOT(9) = 10.0                    M28.6689
      WRITE (KUNIT6, 7654)   ( XYPLOT(I), I=4, 6 )                      M35.8839
 7654 FORMAT ( 8H+Y-AXIS.,  3E13.4 )                                    M23.5595
      GO TO 1050                                                        M23.5596
 7661 IF ( AUPPER(1)  .NE.  TEXT30 )   GO TO 7679                       M28.6690
      IF ( AUPPER(2)  .NE.  TEXT31 )   GO TO 7671                       M28.6691
      DECODE (80, 7381, ABUFF(1))  NFOUR                                M28.6692
      IF ( NFOUR  .LE.  1 )   NFOUR = 30                                M28.6693
      WRITE (KUNIT6, 7664)  NFOUR                                       M35.8840
 7664 FORMAT (  33H+FOURIER SERIES STARTED.  NFOUR =,  I8  )            M28.6695
      MAXEVK = MAXEVK * NBYTE(5) / NBYTE(3)                             M28.6696
      GO TO 1050                                                        M28.6697
 7671 IF ( AUPPER(2)  .NE.  TEXT32 )   GO TO 7679                       M28.6698
      NFOUR = 0                                                         M28.6699
      WRITE (KUNIT6, 7674)                                              M35.8841
 7674 FORMAT (  38H+FOURIER SERIES ENDED.  BACK TO PLOTS.  )            M28.6701
      MAXEVK = MAXEVK * NBYTE(3) / NBYTE(5)                             M28.6702
      GO TO 1050                                                        M28.6703
 7679 IF ( ITP  .EQ.  0 )   GO TO 1070                                  M28.6704
      IF( ITP .EQ. 1 )  GO TO 1160                                      M09. 196
      IF( ITP .EQ. 2 )  GO TO 1550                                      M09. 197
      LSTAT(16) = ITP                                                   M09. 198
      KILL = 76                                                         M09. 199
      LSTAT(19) = 1070                                                  M09. 200
      GO TO 9200                                                        M09. 201
 1070 DO 1080  I=1, 13                                                  M09. 202
      IF ( AUPPER(I)  .NE.  BLANK )   GO TO 1106                        M13.3850
 1080 CONTINUE                                                          M09. 204
      WRITE (KUNIT6, 1100)                                              M35.8842
 1100 FORMAT (41H+BLANK CARD TERMINATING PLOT SPEC. CARDS.)             M09. 206
      CALL INTERP                                                       M35.8843
      IF ( TAXMAX  .EQ.  0.0 )   GO TO 2720                             M19.2153
      IF ( KALCOM  .EQ.  0 )   GO TO 2720                               M19.2154
      N8 = -6666                                                        M19.2155
      GO TO 2656                                                        M19.2156
 1106 IF ( IPLOT   .GE.  0     .AND.                                    M29.2687
     1     LUNIT4  .GT.  0 )   GO TO 1120                               M29.2688
      WRITE (KUNIT6, 1108)                                              M35.8844
 1108 FORMAT ( 49H+PLOT CARD IGNORED IN QUEST FOR START OF NEW CASE   ) M13.3854
      GO TO 1050                                                        M13.3855
 1120 IF ( IALF  .LT.  52 )   GO TO 1126                                M09. 208
      WRITE (KUNIT6, 1122)                                              M35.8845
 1122 FORMAT (  34H+OVERFLOW SUBTITLE CARD DISCARDED.   )               M09. 210
      GO TO 1050                                                        M11. 315
 1126 IALF = IALF + 13                                                  M09. 212
      CALL PACKCH ( AUPPER(1), ALPHA(IALF-12), MM6, MM1, MM13 )         M28.6705
      J1 = IALF - 12                                                    M09. 214
      IF ( IPRSUP .GE. 1 )                                              M10. 241
     1  WRITE (LUNIT6, 126)  ( ALPHA(J), J = J1, IALF )                 M17.1087
  126 FORMAT ( 30H AT 1126 PACKED SUBTITLE TEXT., /, 1X, 13A10 )        M10. 243
      K = 0                                                             M09. 215
      DO 1127 J = J1, IALF                                              M09. 216
      K = K + 1                                                         M09. 217
 1127 PLTLE(J) = AUPPER(K)                                              M09. 218
      WRITE (KUNIT6, 1140)                                              M35.8846
 1140 FORMAT( 20H+PLOT SUBTITLE CARD.  )                                M09. 220
      GO TO 1050                                                        M11. 316
 1550 CALL PACKCH ( AUPPER(1), SEXT(1), MM6, MM1, MM13 )                M28.6706
      IF ( IPRSUP .GE. 1 )                                              M10. 244
     1  WRITE (LUNIT6, 155)  SEXT                                       M17.1089
  155 FORMAT ( 32H AT 1550 PACKED CASE TITLE TEXT., /, 1X, 13A10 )      M10. 246
      DO 1551 J = 1, 13                                                 M09. 223
 1551 CSTXT(J) = AUPPER(J)                                              M09. 224
      WRITE (KUNIT6, 1155)                                              M35.8847
 1155 FORMAT( 22H+PLOT CASE-TITLE TEXT.   )                             M09. 226
      GO TO 1050                                                        M09. 227
 1160 CALL COPYA ( BLANK, SLOT(1), MM8 )                                M28.6707
      JSLOT = 3                                                         M09. 231
      IF ( KOLBEG  .GT.  0 )   GO TO 7402                               M22.5550
      DECODE (80, 1180, ABUFF)  ITP, ICP, IHS, HPI, HMIN, HMAX,         M09. 232
     1 VMIN, VMAX, (SLOT(J), J=1, 4), BUS1, BUS2, BUS3, BUS4,           M09. 233
     2 BUS5, BUS6                                                       M09. 234
 1180 FORMAT ( I2, 2I1, E3.0, 2E4.0, E5.0, E4.0, 4A6,  2(2A6, A4)  )    M09. 235
      GO TO 7405                                                        M22.5551
 7402 NFRFLD = 1                                                        M22.5552
      CALL FREONE ( FLONG1 )                                            M29.2689
      ITP = FLONG1                                                      M28.6709
      CALL FREONE ( FLONG1 )                                            M29.2690
      ICP = FLONG1                                                      M28.6711
      CALL FREONE ( FLONG1 )                                            M29.2691
      IHS = FLONG1                                                      M28.6713
      NFRFLD = 5                                                        M28.6714
      CALL FREFLD ( VOLTBC(1) )                                         M31.3727
      HPI  = VOLTBC(1)                                                  M31.3728
      HMIN = VOLTBC(2)                                                  M31.3729
      HMAX = VOLTBC(3)                                                  M31.3730
      VMIN = VOLTBC(4)                                                  M31.3731
      VMAX = VOLTBC(5)                                                  M31.3732
      NFRFLD = 4                                                        M22.5564
      NRIGHT = -1                                                       M22.5565
      CALL FREONE ( FLONG1 )                                            M29.2692
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M22.5567
      NRIGHT = 0                                                        M22.5568
      DO 7403  J=1, 4                                                   M22.5569
 7403 SLOT(J) = TEXTA6(J)                                               M22.5570
C     TEMPORARILY BLANK OUT THE TITLES IN FREE-FORMAT CASE              M22.5571
      BUS1 = BLANK                                                      M22.5572
      BUS2 = BLANK                                                      M22.5573
      BUS3 = BLANK                                                      M22.5574
      BUS4 = BLANK                                                      M22.5575
      BUS5 = BLANK                                                      M22.5576
      BUS6 = BLANK                                                      M22.5577
 7405 IF ( ISPRIN  .NE.  43 )   GO TO 7408                              M22.5578
      IF ( DEGMAX  .EQ.  0.0 )    GO TO 7408                            M22.5579
      IHS = 4                                                           M22.5580
      ICP = 4                                                           M22.5581
      HPI = TMAX / HMIN                                                 M22.5582
      HMIN = 0.0                                                        M22.5583
      HMAX = TMAX                                                       M22.5584
      IF ( SLOT(1)  .EQ.  TEXT20 )   GO TO 7408                         M22.5585
      VMIN = DEGMIN                                                     M22.5586
      VMAX = DEGMAX                                                     M22.5587
 7408 HMIN  =  HMIN * ( 1.0 - FLZERO )                                  M22.5588
      HMAX  =  HMAX * ( 1.0 + FLZERO )                                  M16.4363
      IF ( XYPLOT(1)  .EQ.  0.0 )   GO TO 7413                          M28.6722
      HPI = ( HMAX - HMIN ) * 0.1                                       M23.5600
 7413 NCRV = ICP                                                        M23.5601
      IF ( SLOT(1) .NE. TEXT8 )  GO TO 83                               M09. 237
      WRITE (KUNIT6, 80)  IHS                                           M35.8848
   80 FORMAT ( 34H+GRAPH SEPARATION CARD.   'KSEP' =, I3 )              M12. 570
      IF ( IHS .GE. 2 )  GO TO 82                                       M09. 240
      WRITE ( LUNIT6,81 ) IHS, MRGN                                     M17.1092
   81 FORMAT ( 35H THE SPECIFIED GRAPH SEPARATION OF , I2, 48H INCHES ISM09. 242
     1 CONSIDERED TOO SMALL AND IS RESET TO , I1, 8H INCHES. )          M09. 243
      IHS = MRGN                                                        M09. 244
   82 MRGN = IHS                                                        M09. 245
      GO TO 1050                                                        M09. 246
   83 IF ( SLOT(1) .NE. TEXT7 )  GO TO 1185                             M09. 247
      WRITE (KUNIT6, 1181)  HMIN, HMAX                                  M35.8849
 1181 FORMAT ( 28H+GRAPH SIZE ADJUSTMENT CARD., 2E11.3 )                M12. 572
      D1 = HMIN + HMAX + VPLOFF                                         M19.2157
      IF ( D1 .LE. SZPLT )  GO TO 11182                                 M09. 251
      WRITE (LUNIT6, 11181) HMIN, HMAX, D1, SZPLT                       M17.1094
11181 FORMAT ( 5X, 41HNOTE ---- THE REQUESTED BOTTOM MARGIN OF , E9.3,  M09. 253
     121H AND GRAPH HEIGHT OF , E9.3,18H REQUIRES A TOTAL , /, 5X,      M09. 254
     2 16HPAPER HEIGHT OF , E9.3, 72H.  THIS IS GREATER THAN THE HEIGHT M09. 255
     3SPECIFIED IN =CALL PAPRSZ=, WHICH IS , E9.3, /, 5X,               M10. 247
     4109HTHE REQUESTED VALUES WILL BE IGNORED AND THE LAST SPECIFIED (OM09. 257
     5R DEFAULT IF NO HEIGHT VALUES WERE EVER GIVEN) , /, 5X,13HWILL BE M09. 258
     6USED.   )                                                         M09. 259
      IF ( TAXMAX  .GT.  0.0 )                                          M19.2158
     1 WRITE (LUNIT6, 21181)  VPLOFF                                    M19.2159
21181 FORMAT ( 5X,  85HACTUALLY, THE JUST-QUOTED PAPER HEIGHT IS THE PAPM19.2160
     1ER HEIGHT MINUS THE OFFSET HEIGHT OF,     E13.4,  6H   FOR    ,/, M19.2161
     2 5X, 110HTHE ONE OR MORE GRAPHS WHICH HAVE ALREADY BEEN DRAWN VERTM19.2162
     3ICALLY BELOW THE UPCOMING PLOT.   THE USER SHOULD NOT        ,/,  M19.2163
     4 5X, 113HINCREASE VERTICAL DIMENSIONS UNLESS HE IS AT THE BOTTOM OM19.2164
     5F THE PAPER, OR UNLESS HE IS SURE THAT HE HAS SUFFICIENT     ,/,  M19.2165
     6 5X, 101HVERTICAL SPACE LEFT ON THE PAPER FOR AT LEAST ONE MORE PLM19.2166
     7OT.   THE SIZE ADJUSTMENT REMAINS CANCELLED.         )            M19.2167
      GO TO 1050                                                        M09. 260
11182 VSNEW = HMIN - VS                                                 M09. 261
      IF ( KALCOM  .EQ.  1 )                                            M23.5602
     1 CALL PLOT ( ZERO, VSNEW, LLMIN3 )                                M23.5603
      VS = HMIN                                                         M09. 263
      VHS = HMAX                                                        M09. 264
      VH = VHS + VS + 1.0                                               M09. 265
      GO TO 1050                                                        M09. 266
 1185 IF ( SLOT(1)  .NE.  TEXT6 )   GO TO 1188                          M09. 267
      JSLOT = 7                                                         M09. 268
      WRITE (KUNIT6, 1182)  HPI, HMIN, HMAX                             M35.8850
 1182 FORMAT ( 14H+** PLOT CARD. ,  2X,  3E11.3 )                       M12. 574
      CALL INTERP                                                       M35.8851
C     READ INPUT CARD USING CIMAGE                                      M09. 271
      CALL CIMAGE                                                       M09. 272
      DECODE ( 80, 1187, ABUFF )  NCRV, SLOT                            M09. 273
 1187 FORMAT ( 2X, I1, 21X, 8A6 )                                       M09. 274
      IF ( NCRV .EQ. 0 )  NCRV = 4                                      M09. 275
      WRITE (KUNIT6, 1184)                                              M35.8852
 1184 FORMAT ( 45H+CONTINUATION TO READ BRANCH NODE-PAIR NAMES. )       M09. 277
      CALL INTERP                                                       M35.8853
      GO TO 1195                                                        M09. 278
 1188 IF ( SLOT(1)  .NE.  TEXT5 )   GO TO 1195                          M09. 279
      TOLRCE = VMIN * VMIN                                              M09. 280
      WRITE (KUNIT6, 1189 ) VMIN                                        M35.8854
 1189 FORMAT ( 37H+REDEFINITION OF SMOOTHING TOLERANCE., 2X, E10.2 )    M12. 575
      D1 = 0.1                                                          M09. 283
      IF ( VMIN  .GT.  D1 )                                             M09. 284
     1 WRITE (LUNIT6, 1190)  VMIN                                       M17.1098
 1190 FORMAT ( 51H ****WARNING****  REQUESTED SMOOTHING TOLERANCE OF ,  M09. 286
     1 E11.3, 27H MAY CAUSE INACCURATE PLOT. )                          M09. 287
      GO TO 1050                                                        M09. 288
 1195 IF ( ICP  .EQ.  8 )   GO TO 7412                                  M28.6723
      IF ( ICP  .EQ.  9 )   GO TO 7412                                  M28.6724
      IF ( ICP  .GT.  0   .AND.                                         M28.6725
     1     ICP  .LE.  4 )   GO TO 7412                                  M28.6726
      WRITE (LUNIT6, 7407)  ICP                                         M28.6727
 7407 FORMAT (  41H ILLEGAL PLOT-VARIABLE TYPE CODE  "ICP" =,           M28.6728
     1  I3,     36H .   THIS PLOT REQUEST IS CANCELLED.  )              M28.6729
      GO TO 1000                                                        M28.6730
 7412 CALL PACKCH ( BUSVEC(1), HEADL(1), MM6, MM1, MM2 )                M29.2693
      CALL PACKCH ( BUSVEC(3), HEADL(1), MM4, MM13, MM1 )               M29.2694
      CALL PACKCH ( BUSVEC(4), VERTL(1), MM6, MM1, MM3 )                M29.2695
      IF ( IPRSUP .GE. 1 )                                              M10. 248
     1  WRITE (LUNIT6, 1196)  HEADL, VERTL                              M17.1099
 1196 FORMAT ( 32H AT 1195 PACKED PLOT TITLE TEXT., 3A10, /,            M10. 250
     1  32H AT 1195 PACKED VERT AXIS LABEL., 3A10 )                     M10. 251
      IF ( JSLOT  .NE.  3 )   GO TO 4589                                M35.8855
      WRITE (KUNIT6, 1182)  HPI, HMIN, HMAX                             M35.8856
      CALL INTERP                                                       M35.8857
 4589 IF ( HPI  .NE.  0.0 )   GO TO 1192                                M35.8858
C     READ INPUT CARD USING CIMAGE                                      M09. 298
      CALL CIMAGE                                                       M09. 299
      IF ( KOLBEG  .GT.  0 )   GO TO 8212                               M22.5589
      DECODE (80, 1183, ABUFF)  HPI, HMIN, HMAX, VMIN, VMAX             M09. 300
 1183 FORMAT ( 5E16.0 )                                                 M09. 301
      GO TO 8215                                                        M22.5590
 8212 NFRFLD = 5                                                        M28.6737
      CALL FREFLD ( VOLTBC(1) )                                         M31.3733
      HPI  = VOLTBC(1)                                                  M31.3734
      HMIN = VOLTBC(2)                                                  M31.3735
      HMAX = VOLTBC(3)                                                  M31.3736
      VMIN = VOLTBC(4)                                                  M31.3737
      VMAX = VOLTBC(5)                                                  M31.3738
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M22.5597
 8215 WRITE (KUNIT6, 1186)                                              M35.8859
 1186 FORMAT (  47H+RE-READ OF FLOATING-POINT FIELDS FOR ACCURACY.  )   M09. 303
      CALL INTERP                                                       M35.8860
 1192 IF ( NFOUR  .EQ.  0 )   GO TO 21192                               M28.6744
      D23 = HPI - 7.6                                                           
      IF ( D23  .GT.  -.001   .AND.                                     M38.5470
     1     D23  .LT.  0.001 )  NFOUR = -NFOUR                           M38.5471
      GO TO 21220                                                       M28.6746
21192 IF ( HPI  .LE.  0.0 )   GO TO 1193                                M28.6747
      IF ( HMAX  .LE.  0.0 )   GO TO 1193                               M12. 580
      IF ( HMAX  .GT.  HMIN )   GO TO 1197                              M12. 581
 1193 WRITE (LUNIT6, 1194)  HPI, HMIN, HMAX                             M17.1102
 1194 FORMAT ( 5X,  92HPLOT CARD ERROR.   TIME-AXIS SPECIFICATION IS ILLM12. 583
     1EGAL.   THIS PLOT REQUEST IS BEING SKIPPED.   ,/, 20X, 3E15.5  )  M12. 584
      GO TO 1000                                                        M12. 585
 1197 D1 = ( HMAX - HMIN ) / HPI                                        M12. 586
      HPIL = HPI                                                        M09. 305
      IF ( IOUT .EQ. 2 )  GO TO 1227                                    M09. 306
      IF ( D1 .LE. SZBED )  GO TO 1227                                  M09. 307
      WRITE(LUNIT6, 1224 )  HPI, HMIN, HMAX, D1                         M17.1103
 1224 FORMAT( 5X,  27HNOTE ----- A TIME SCALE OF , E13.4,  62H WAS READ M09. 309
     1FROM COLUMNS 5-7 OF THE PRECEDING PLOT-REQUEST CARD.    ,/,       M09. 310
     2 5X,  39HTOGETHER WITH THE REQUESTED MINIMUM OF , E13.4,          M09. 311
     3 49H UNITS (READ FROM COLUMNS 8-11) AND THE REQUESTED   ,/,       M09. 312
     4 5X,  11HMAXIMUM OF , E13.4, 64H UNITS (READ FROM COLUMNS 12-15), M09. 313
     5THIS IMPLIES A PLOT OF LENGTH , E13.4 )                           M09. 314
      WRITE (LUNIT6, 1225) SZBED                                        M17.1104
 1225 FORMAT ( 1H+,106X, 15H, WHICH EXCEEDS ,/,                         M09. 316
     6 5X,  40HTHE CURRENTLY IMPOSED FLAT-BED LIMIT OF , E13.4, 67H INCHM09. 317
     7ES.  THE SCALE ON THE TIME AXIS WILL BE CHANGED BY THE EMTP SO ,/,M28.6748
     8 5X, 114HTHAT THE SPECIFIED RANGE COVERS EXACTLY 12 INCHES OF PAPEM09. 319
     9R.   IF THE USER HAS QUESTIONS, CALL PROGRAM MAINTENANCE.   )     M09. 320
      HPI = (HMAX - HMIN) / 12.                                         M09. 321
 1227 IF ( IOUT .EQ. 1 )  GO TO 1229                                    M09. 322
      IF ( D1 .GE. 3.0 )  GO TO 1229                                    M09. 323
      WRITE(LUNIT6, 1224 )  HPI, HMIN, HMAX, D1                         M17.1105
      HMAX = 3.0 * HPI + HMIN                                           M09. 325
      WRITE (LUNIT6, 1228) HMAX                                         M17.1106
 1228 FORMAT ( 1H+,106X, 21H, WHICH IS CONSIDERED, /, 5X,               M09. 327
     6  99HTOO SHORT FOR THE REQUESTED LINEPRINTER PLOT.  THE REQUESTED M09. 328
     7MAXIMUM WILL BE RESET BY THE T. P. TO , E13.6, /, 5X,             M09. 329
     8   34HTO GIVE A PLOT LENGTH OF 3 INCHES. )                        M09. 330
 1229 COUNTP = COUNTP + 1.0                                             M09. 331
      KIKOY=0                                                           M22.5599
      IF((IOUT.EQ.2).AND.(VMIN.NE.0.0.OR.VMAX.NE.0.0))KIKOY=99          M22.5600
      IF (VMIN.NE.0.0.OR.VMAX.NE.0.0) GO TO 1220                        M09. 332
      VMIN=-C1E12                                                       M09. 333
      VMAX=C1E12                                                        M09. 334
 1220 VMAXR  =  VMIN  +  (VMAX - VMIN) * (VH - VS) / VHS                M09. 335
      VMINR =  VMIN  -  (VMAX - VMIN) * VS / VHS                        M09. 336
21220 IF ( IHS .GE. 1 .AND. IHS .LE. 7 )  GO TO 1230                    M28.6749
      WRITE (LUNIT6, 1221) IHS                                          M17.1107
 1221 FORMAT ( 5X, 112HTHE NUMBER SPECIFYING THE UNITS ON THE HORIZONTALM11. 319
     1 SCALE, PUNCHED IN COLUMN 4 OF THE PLOT REQUEST CARD, MUST BE  ,  M11. 320
     2  /, 5X, 73HBETWEEN 1 AND 7 (INCLUSIVE).  THE NUMBER READ FROM THEM20.4906
     3 LAST SUCH CARD WAS,  I2, 1H. , /, 5X, 31HTHIS PLOT REQUEST IS CANM11. 322
     4CELLED.  )                                                        M11. 323
      GO TO 1000                                                        M11. 324
 1230 GO TO (1240, 1260, 1280, 1300, 1320, 1280, 1280) , IHS            M19.2175
 1240 TMULT = 360. * STATFR                                             M20.4907
      GO TO 1340                                                        M09. 341
 1260 TMULT = STATFR                                                    M20.4908
      GO TO 1340                                                        M09. 343
 1280 TMULT=1.                                                          M09. 344
      GO TO 1340                                                        M09. 345
 1300 TMULT=1000.                                                       M09. 346
      GO TO 1340                                                        M09. 347
 1320 TMULT=1000000.                                                    M09. 348
 1340 HMIN = HMIN / TMULT                                               M09. 349
      HMAX=HMAX/TMULT                                                   M09. 350
      MHORIZ = -22                                                      M09. 351
      IF ( XYPLOT(1)  .GT.  0.0 )                                       M23.5604
     1 IHS = 8                                                          M23.5605
      J = 4*(IHS-1)  +  1                                               M09. 352
      CALL PACKCH ( TEXTAX(J), HORZL(1), MM6, MM1, MM4 )                M28.6750
      IF ( IPRSUP .GE. 1 )                                              M10. 252
     1  WRITE (LUNIT6, 1341)  HORZL                                     M17.1108
 1341 FORMAT ( 32H AT 1341 PACKED TIME AXIS LABEL., 3A10 )              M10. 254
      IF (VMIN.GE.0.0) GO TO 1460                                       M09. 354
      IF (VMAX.LE.0.0) GO TO 1480                                       M09. 355
      HA  =  -VMIN * VHS / (VMAX - VMIN)                                M09. 356
      GO TO 1500                                                        M09. 357
 1460 HA=0.0                                                            M09. 358
      GO TO 1500                                                        M09. 359
 1480 HA = VHS                                                          M09. 360
 1500 READ (LUNIT4)   DATE1, TCLOCK,  ( INTD8(J), J=1, 4)               M20.4909
      NUMNAM = INTD8(1)                                                 M20.4910
      NUMNVO = INTD8(2)                                                 M20.4911
      NUMBCO = INTD8(3)                                                 M20.4912
      NC     = INTD8(4)                                                 M20.4913
      JCNT = NUMNAM + NUMNVO + 2*NC                                     M22.5601
      JCNT = JCNT * NBYTE(1) / NBYTE(4)                                 M22.5602
      IF ( MAXEV .GE. JCNT )  GO TO 1520                                M09. 363
      LSTAT(14) = MAXEV                                                 M09. 364
      LSTAT(15) = JCNT                                                  M09. 365
      LSTAT(19) = 1500                                                  M09. 366
      KILL = 96                                                         M09. 367
      GO TO 9200                                                        M09. 368
 1520 IF ( MULPLT(1)  .GT.  0 )   GO TO 1523                            M22.5603
      MULPLT(2) = 0                                                     M22.5604
      MULPLT(3) = 0                                                     M22.5605
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.2696
 1523 REWIND LUNIT4                                                     M22.5606
      READ (LUNIT4)   DATE1, TCLOCK,  ( INTD8(J), J=1, 4),              M20.4914
     1 ( BUSLST(J), J=1, NUMNAM )                                       M09. 371
      IF ( NUMNVO  .GT.  LIMBIN )   GO TO 34231                         M20.4915
      IF ( NC  .LE.  LIMBIN )   GO TO 44231                             M20.4916
34231 WRITE (LUNIT6, 24231)  LIMBIN, NUMNVO, NC                         M20.4917
24231 FORMAT ( /,  35H OVERFLOW ERROR STOP IN  'SUBR31' .,              M20.4918
     1 24H  LIMBIN  NUMNVO      NC   ,/,  35X,  3I8  )                  M20.4919
      CALL STOPTP                                                               
44231 IF ( IPRSUP  .GE.  1 )                                            M20.4921
     1 WRITE (LUNIT6, 4231)  DATE1, TCLOCK, NUMNAM, NUMBCO, NC,         M17.1112
     2 ( BUSLST(J), J=1, NUMNAM )                                       M09. 374
 4231 FORMAT ( /,  68H FIRST THIRD OF PLOT-FILE HEADER INFO READ FROM LOM09. 375
     1GICAL 4 IN SUBR31.,  7X,  8HDATE1(2),  6X,  9HTCLOCK(2),          M22.5607
     2 24H  NUMNAM  NUMBCO      NC  ,/,  68X,  7X,  2A4,                M22.5608
     3 7X,  2A4,  3I8  ,/,  ( 25H (BUSLST(J), J=1, NUMNAM),  1X,        M22.5609
     4 10A7 )    )                                                      M22.5610
      IOFIBR = NUMNVO                                                   M09. 378
      IOFJBR = NUMNVO + NC                                              M09. 379
      IOFBUS = NUMNVO + 2*NC                                            M09. 380
      IF ( IPRSUP .GE. 1 )                                              M09. 381
     1 WRITE ( LUNIT6,1525 ) IOFIBR, IOFJBR, IOFBUS                     M17.1113
 1525 FORMAT ( /,  9H OFFSETS.,   24H  IOFIBR  IOFJBR  IOFBUS   ,/,     M22.5611
     1 9X,  3I8  )                                                      M22.5612
      J = NUMNAM                                                        M09. 384
 4224 N4 = J + IOFBUS                                                   M16.4364
      BUSLST(N4) = BUSLST(J)                                            M16.4365
      J = J - 1                                                         M09. 386
      IF ( J  .GT.  0 )   GO TO 4224                                    M09. 387
      IF ( NUMNVO  .GT.  0 )                                            M09. 388
     1 READ (LUNIT4)   ( INTD8(J), J=1, NUMNVO )                        M20.4922
      DO 84224  J=1, NUMNVO                                             M20.4923
84224 IBSOUT(J) = INTD8(J)                                              M20.4924
      IF ( IPRSUP  .GE.  1 )                                            M09. 390
     1 WRITE (LUNIT6, 64225)  NUMNVO, ( IBSOUT(J), J=1, NUMNVO)         M17.1115
64225 FORMAT ( /,  8H NUMNVO=, I4, 10X,                                 M09. 392
     1  37H(IBSOUT(J), J=1, NUMNVO)  FOLLOW ....  ,/, ( 1X, 20I6 ) )    M09. 393
      N1 = NUMNVO + 1                                                   M09. 394
      N2 = NUMNVO + NC                                                  M09. 395
      N5 = N1 + NC                                                      M16.4366
      N6 = N2 + NC                                                      M16.4367
      IF ( NC  .EQ.  0 )   GO TO 54224                                  M22.5613
      READ (LUNIT4)   ( INTD8(J), J=1, NC),  ( INTD9(J), J=1, NC )      M22.5614
      IF ( IPRSUP  .GE.  1 )                                            M21. 382
     1 WRITE (LUNIT6, 4219)   ( INTD8(J), J=1, NC),                     M21. 383
     2                        ( INTD9(J), J=1, NC)                      M21. 384
 4219 FORMAT ( /,  41H (INTD8(J), J=1, NC), (INTD9(J), J=1, NC)    ,/,  M21. 385
     1 ( 1X,  20I6 )   )                                                M21. 386
      N11 = N1                                                          M20.4926
      N12 = N5                                                          M20.4927
      DO 34222  J=1, NC                                                 M20.4928
      IBRNCH(N11) = INTD8(J)                                            M20.4929
      JBRNCH(N12) = INTD9(J)                                            M20.4930
      N11 = N11 + 1                                                     M20.4931
34222 N12 = N12 + 1                                                     M20.4932
      IF (IPRSUP .LT. 1)   GO TO 54224                                  M16.4369
      WRITE (LUNIT6, 4222)  N1, N2                                      M17.1117
 4222 FORMAT (4H N1=, I10, 3HN2=, I10, /, 49H IBRNCH(J) AND JBRNCH(J+NC)M16.4371
     1, J=N1, N2 AT 4222....)                                           M16.4372
      DO 54223  J = N1, N2                                              M16.4373
      N6 = J + NC                                                       M16.4374
54223 WRITE (LUNIT6, 64223)  IBRNCH(J), JBRNCH(N6)                      M17.1118
64223 FORMAT (1X, 2I10)                                                 M16.4376
54224 CALL PACKCH ( DATE1(1), DAYTIM(1), MM4, MM1, MM2 )                M28.6751
      CALL PACKCH ( BLANKA(1), DAYTIM(1), MM4, MM9, MM1 )               M29.2697
      CALL PACKCH ( TCLOCK(1), DAYTIM(1), MM4, MM11, MM2 )              M28.6753
      IF ( IPRSUP  .GE.  1 )                                            M09. 405
     1 WRITE (LUNIT6, 4223)  DAYTIM                                     M17.1119
 4223 FORMAT ( 23H AT 4223 PACKED DAYTIM., 8A10 )                       M10. 255
      NUMBVO = NC - NUMBCO                                              M09. 408
      I = 0                                                             M09. 409
      JPLT=0                                                            M09. 410
      IF (ICP.GT.7) GO TO 1660                                          M09. 411
 1560 I=I+1                                                             M09. 412
      IF (I.NE.5) GO TO 1580                                            M09. 413
      IF ( JPLT  .EQ.  0 )   GO TO 1900                                 M09. 414
      GO TO 1880                                                        M09. 415
 1580 IF (SLOT(I).EQ.BLANK) GO TO 1560                                  M09. 416
      K=0                                                               M09. 417
 1600 K=K+1                                                             M09. 418
      IF ( K  .LE.  NUMNVO )   GO TO 1640                               M09. 419
      WRITE (LUNIT6,1620) SLOT(I)                                       M17.1120
 1620 FORMAT( 5X,  10HBUS NAME =, A6,  80H= OF THE USER=S LAST-READ PLOTM09. 421
     1 CARD IS NOT THE NAME OF A BUS HAVING NODE VOLTAGE   ,/,          M09. 422
     2 5X,  89HOUTPUT.   HENCE THIS FIELD WILL BE IGNORED BY THE EMTP (TM28.6754
     3REATED AS IF IT HAD BEEN BLANK).    )                             M09. 424
      WRITE(LUNIT6, 1621)                                               M17.1121
 1621 FORMAT( 5X, 103HTHE USER IS REMINDED THAT CORRECT SPELLING AND THEM09. 426
     1 CONSISTENT POSITIONING OF ALL BLANKS WITHIN THE DATA   ,/,       M09. 427
     2 5X,  48HFIELDS OF WIDTH 6 FOR ALL BUS NAMES IS REQUIRED.    )    M09. 428
 1627 SLOT(I) = BLANK                                                   M20.4933
      GO TO 1560                                                        M09. 429
 1640 L = IBSOUT(K) + IOFBUS                                            M09. 430
      IF ( SLOT(I)  .NE.  BUSLST(L) )   GO TO 1600                      M09. 431
      JPLT=JPLT+1                                                       M09. 432
      MPLOT(JPLT)=K                                                     M09. 433
      SLOT(JPLT) = BUSLST(L)                                            M09. 434
      IF ( JPLT  .LT.  I )   GO TO 1627                                 M20.4934
      GO TO 1560                                                        M09. 435
 1660 DO 1680  J=1, JSLOT, 2                                            M09. 436
      IF ( SLOT(J)  .EQ.  BLANK   .AND.                                 M09. 437
     1                          SLOT(J+1)  .EQ.  BLANK )   GO TO 1680   M09. 438
      SLOT(JPLT+1) = SLOT(J)                                            M09. 439
      SLOT(JPLT+2) = SLOT(J+1)                                          M09. 440
      JPLT = JPLT + 2                                                   M09. 441
 1680 CONTINUE                                                          M09. 442
      IF ( JPLT .EQ. 0 )  GO TO 1900                                    M09. 443
      LPLT=0                                                            M09. 444
      JBEGBC = NUMBVO + 1                                               M09. 445
      DO 1860 I=1,JPLT,2                                                M09. 446
      IF (ICP.EQ.9) GO TO 1720                                          M09. 447
      IB = 1                                                            M09. 448
      IL = JBEGBC                                                       M09. 449
      N1 = NUMNVO + 1                                                   M09. 450
      GO TO 1740                                                        M09. 451
 1720 IB = JBEGBC                                                       M09. 452
      IL = NC + 1                                                       M09. 453
      N1 = NUMNVO + JBEGBC                                              M09. 454
 1740 IF (IB.LT.IL) GO TO 1780                                          M09. 455
      WRITE (LUNIT6,1760) SLOT(I),SLOT(I+1)                             M17.1122
 1760 FORMAT( 5X, 100HTHE USER=S LAST-READ PLOT CARD REQUESTS A PLOT FORM09. 457
     1 A BRANCH-VARIABLE WHICH IS IDENTIFIED BY TERMINAL    ,/,         M09. 458
     2 5X,   7HNAMES =, A6,  7H= AND =, A6,  75H=.   BUT THE EMTP CANNOTM28.6755
     3 FIND THIS REQUESTED VARIABLE IN THE LIST OF OUTPUT   ,/,         M09. 460
     4 5X,  59HVARIABLES, SO THIS PARTICULAR PLOT REQUEST MUST BE IGNOREM09. 461
     5D.   )                                                            M09. 462
      WRITE(LUNIT6, 1621)                                               M17.1123
      WRITE(LUNIT6, 1763)                                               M17.1124
 1763 FORMAT( 5X, 101HALSO, THE USER SHOULD BE REMINDED THAT BRANCH-OUTPM09. 465
     1UT REQUESTS ARE MADE USING COLUMN-80 PUNCHES ON THE   ,/,         M09. 466
     2 5X, 111HBRANCH CARDS IN QUESTION.   THE USER SHOULD DOUBLE-CHECK M09. 467
     3THAT HE REALLY HAS REQUESTED THE OUTPUT VARIABLE WHICH   ,/,      M09. 468
     4 5X, 112HHE IS TRYING TO PLOT (AND WHICH GOT HIM IN TROUBLE).   ONM09. 469
     5E COMMON ERROR IS TO REQUEST ONLY BRANCH-CURRENT OUTPUT   ,/,     M09. 470
     6 5X, 113H(A 1-PUNCH IN COLUMN 80) AND THEN TRY TO PLOT BRANCH VOLTM09. 471
     7AGE ---- OR VICE VERSA.   FINALLY, THE USER SHOULD CHECK  ,/,     M09. 472
     8 5X, 112HTHAT BRANCH OUTPUT IS EVEN POSSIBLE FOR THE COMPONENT IN M09. 473
     9QUESTION, SINCE COLUMN-80 PUNCHES MAY BE IGNORED IF THE   )       M09. 474
      WRITE(LUNIT6, 1764)                                               M17.1125
 1764 FORMAT( 5X, 101HCOMPONENT IN QUESTION DOES NOT PROVIDE FOR SUCH OUM09. 476
     1TPUT.   ANY BRANCH-OUTPUT REQUEST FOR A MULTI-PHASE   ,/,         M09. 477
     2 5X,  57HDISTRIBUTED LINE FALLS INTO THIS CLASS, IT WILL BE NOTED.M09. 478
     3 )                                                                M09. 479
      SLOT(I) = BLANK                                                   M09. 480
      SLOT(I+1) = BLANK                                                 M09. 481
      GO TO 1860                                                        M09. 482
 1780 N7 = IB + IOFIBR                                                  M16.4378
      N8 = IB + IOFJBR                                                  M16.4379
      N2 = IBRNCH(N7) + IOFBUS                                          M16.4380
      N3 = JBRNCH(N8) + IOFBUS                                          M16.4381
      IF ( IPRSUP .GE. 1 )                                              M09. 485
     1 WRITE ( LUNIT6,1781 ) N2, N3, BUSLST(N2), BUSLST(N3)             M17.1126
 1781 FORMAT ( 8H AT 1780, 2I10, 5X, 2A8 )                              M09. 487
      IF ( SLOT(I)  .NE.  BUSLST(N2) )   GO TO 1800                     M09. 488
      IF ( SLOT(I+1)  .NE.  BUSLST(N3) )   GO TO 1820                   M09. 489
C               NODE PAIR FOUND - SIGN CORRECT                          M09. 490
      MPLOT(LPLT+1) = N1                                                M09. 491
      GO TO 1840                                                        M09. 492
 1800 IF ( SLOT(I+1)  .NE.  BUSLST(N2) )   GO TO 1820                   M09. 493
      IF ( SLOT(I)  .NE.  BUSLST(N3) )   GO TO 1820                     M09. 494
C               NODE PAIR FOUND - SIGN NEGATIVE                         M09. 495
      MPLOT(LPLT+1) = -N1                                               M09. 496
      GO TO 1840                                                        M09. 497
 1820 IB = IB + 1                                                       M09. 498
      N1 = N1 + 1                                                       M09. 499
      GO TO 1740                                                        M09. 500
 1840 LPLT=LPLT+1                                                       M09. 501
 1860 CONTINUE                                                          M09. 502
      JPLT=LPLT                                                         M09. 503
      IF ( JPLT .EQ. 0 )  GO TO 1900                                    M09. 504
      JSLOT = JSLOT + 1                                                 M09. 505
 1880 IF ( IPRSUP  .EQ.  0 )   GO TO 1929                               M09. 506
      WRITE (LUNIT6, 1883)  JPLT, ( SLOT(I), I=1, JSLOT )               M17.1127
 1883 FORMAT ( /, 17H AT 1883 OF OV31.   , I10, /, ( 1X, A6 ) )         M09. 508
      WRITE (LUNIT6, 1884)  ( MPLOT(I), I=1, 4 )                        M17.1128
 1884 FORMAT ( 4I10 )                                                   M09. 510
      GO TO 1929                                                        M09. 511
 1900 WRITE (LUNIT6,1920)                                               M17.1129
 1920 FORMAT( 5X, 105HNO VALID PLOT-VARIABLE NAME OR NAMES WAS PUNCHED OM09. 513
     1N THE USER=S LAST-READ PLOT CARD.   HENCE THE EMTP WILL   ,/,     M28.6756
     2 5X,  90HIGNORE THIS PLOT CARD COMPLETELY, AND GO ON TO READ THE NM09. 515
     3EXT ONE (FUN AND GAMES CONTINUE).      )                          M09. 516
      GO TO 1000                                                        M09. 517
 1929 K = NUMNVO + NC                                                   M09. 518
      JPLT2 = JPLT * 2                                                  M09. 519
      MAXEVK = ( MAXEV/JPLT2 ) * JPLT2 - ( 4*JPLT )                     M09. 520
      MAXEVK = MAXEVK * NBYTE(4) / NBYTE(5)                             M22.5615
      J = JPLT2 + 4*JPLT                                                M09. 521
      IF ( MAXEVK .GE. J )  GO TO 1935                                  M09. 522
      WRITE (LUNIT6,1930 ) MAXEVK, J                                    M17.1130
 1930 FORMAT ( 5X,  77H *** THE PLOTTING ARRAY EV HAS BEEN DIMENSIONED SM09. 524
     1UCH THAT THE WORKING SIZE IS , I10,  20H.  THIS IS LESS THAN,     M09. 525
     2 I10, /, 5X,   70HWHICH IS THE SIZE REQUIRED FOR THIS PLOT.  THIS M09. 526
     3REQUEST IS CANCELLED.  )                                          M09. 527
      GO TO 1000                                                        M09. 528
 1935 KPL = 0                                                           M09. 529
      KPLT = 0                                                          M09. 530
      IOVFL = 0                                                         M09. 531
      IEND = 0                                                          M09. 532
      EVMX=0.0                                                          M09. 533
      VMAXL = 0.0                                                       M09. 534
      VMINL = 0.0                                                       M09. 535
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.2698
      REWIND LUNT13                                                     M32.7586
      DO 2110  J=1, JPLT                                                M09. 537
      ISWX(J) = 0                                                       M09. 538
      EVH(J) = 0.0                                                      M09. 539
      EVDH(J) = 0.0                                                     M09. 540
 2110 CONTINUE                                                          M09. 541
      N9 = 0                                                            M23.5606
      IF ( XYPLOT(1)  .NE.  0     .OR.                                  M28.6757
     1     NFOUR      .NE.  0 )                                         M28.6758
     2 N9 = 1                                                           M28.6759
      IF ( IPRSUP  .GE.  2 )                                            M23.5609
     1 WRITE (LUNIT6, 2117)  K, JPLT, N9, MAXEVK, MPLOT                 M23.5610
 2117 FORMAT ( /,  28H BEFORE READ OF PLOT POINTS.,                     M23.5611
     1  32H       K    JPLT      N9  MAXEVK  ,/,  28X,  4I8  ,/,        M23.5612
     2  25H 'MPLOT'  VECTOR FOLLOWS.,  16I5  )                          M23.5613
      NDX1 = 2 * LINLIM                                                 M32.7587
      CALL MOVER0 ( EV(1), NDX1 )                                       M32.7588
 2115 IF ( M4PLOT  .EQ.  0 )   GO TO 2102                               M32.7589
      READ (LUNIT4)  TSING,  ( BXSING(J), J=1, K )                      M32.7590
      TSTEP = TSING                                                     M32.7591
      DO 2108  J=1, K                                                   M32.7592
 2108 BX(J) = BXSING(J)                                                 M32.7593
      GO TO 2105                                                        M32.7594
 2102 READ (LUNIT4)  TSTEP, (BX(J), J=1, K )                            M32.7595
 2105 IF ( TSTEP  .EQ.  -9999.    .OR.    TSTEP  .GT.  HMAX ) GO TO 2200M32.7596
      IF ( TSTEP  .LT.  HMIN )   GO TO 2115                             M09. 544
      DO 2140 J=1,JPLT                                                  M09. 545
      J1=MPLOT(J)                                                       M09. 546
      IF (J1.GE.0) GO TO 2120                                           M09. 547
      J1=-J1                                                            M09. 548
      BX(J1)=-BX(J1)                                                    M09. 549
 2120 IF ( N9  .EQ.  0 )   GO TO 2127                                   M23.5614
      KPLT = KPLT + 1                                                   M23.5615
      KPL = KPL + 1                                                     M23.5616
      IF ( NFOUR  .NE.  0 )   GO TO 2123                                M28.6760
      EV(KPL) = BX(J1)                                                  M23.5617
      GO TO 2140                                                        M23.5618
 2123 EVDOUB(KPL) = BX(J1)                                              M28.6761
      GO TO 2140                                                        M28.6762
 2127 KPL = KPL + 2                                                     M23.5619
      EV( KPL-1 ) = TSTEP                                               M09. 551
      EV( KPL ) = BX(J1)                                                M09. 552
      KPLT = KPLT + 2                                                   M09. 553
 2140 CONTINUE                                                          M09. 554
      IF ( KPL  .GE.  MAXEVK )   GO TO 2150                             M23.5620
      GO TO 2115                                                        M09. 556
 2150 IOVFL = 1                                                         M09. 557
      GO TO 2210                                                        M09. 558
 2200 IEND = 1                                                          M09. 559
      IF ( LASTOV  .EQ.  1 )                                            M20.4935
     1 TMAX = EV(KPL-1)                                                 M20.4936
      IF ( KPLT  .NE.  0 )   GO TO 2210                                 M09. 560
      WRITE (LUNIT6, 2205)  HMIN, HMAX                                  M17.1133
 2205 FORMAT ( 57H NO PLOT POINTS FOUND BETWEEN THE REQUESTED TIME LIMITM09. 562
     1S    ,  E12.3,   4H TO , E12.3,    33H .   THIS PLOT REQUEST CANCEM09. 563
     2LLED.   )                                                         M09. 564
      GO TO 1000                                                        M09. 565
 2210 IF ( IPRSUP  .EQ.  0 )   GO TO 2215                               M09. 566
      JK = KPLT/JPLT2                                                   M09. 567
      WRITE (LUNIT6,2211) JK                                            M17.1134
 2211 FORMAT ( /, 66H AT 2211, AFTER READING FROM 4, THE NUMBER OF POINTM09. 569
     1S PER CURVE ARE, I10 )                                            M09. 570
      IF ( IPRSUP  .LT.  3 )   GO TO 2215                               M23.5621
      J1 = 8                                                            M23.5622
      IF ( IPRSUP  .GT.  39 )   J1 = KPL                                M23.5623
      WRITE (LUNIT6, 2213)  ( EV(J), J=1, J1 )                          M23.5624
 2213 FORMAT ( /,  19H EV(J) PLOT POINTS.  ,/,  ( 1X,  10E13.4 ) )      M23.5625
 2215 IF ( XYPLOT(1)  .GT.  0.0 )   GO TO 2540                          M23.5626
      IF ( NFOUR  .EQ.  0 )   GO TO 7682                                M28.6763
C     CALCULATE FOURIER SERIES IN MODULE  "SERIES" .                    M28.6764
      CALL SERIES ( NFOUR, KPL, JPLT, MAXEVK )                          M28.6765
      GO TO 1000                                                        M28.6766
 7682 DO 2340  J=1, JPLT                                                M28.6767
      J1 = J*2                                                          M09. 572
      DO 2320 I = J1, KPL, JPLT2                                        M09. 573
      EVP = EV(I)                                                       M09. 574
      IF ( ISWX(J) .EQ. 1 )  GO TO 2240                                 M09. 575
      EVDP = EVP - EVH(J)                                               M09. 576
      IF ( EVDP*EVDH(J)  .GE.  0.0 )   GO TO 2250                       M09. 577
      IF ( ISWW(J)  .EQ.  1 )  GO TO 2255                               M09. 578
      ITIMES(J) = ITIMES(J) + 1                                         M09. 579
      IF ( ITIMES(J)  .LE.  NSMTH )   GO TO 2260                        M09. 580
      ISWX(J) = 1                                                       M09. 581
      WRITE (LUNIT6, 2220 )  J, EV(I-1), NSMTH                          M17.1135
 2220 FORMAT( 5X, 106HWHILE SCANNING THE DATA POINTS FOR CURVES OF THE LM09. 583
     1AST-READ PLOT CARD, A NEED FOR SMOOTHING OF CURVE NUMBER  ,/,     M09. 584
     2 5X, I2,  13H BEYOND TIME , E12.4,  47H SECONDS HAS BEEN DETERMINEM09. 585
     3D.   AT THIS POINT,  , I3,  27H SUCCESSIVE, UNINTERRUPTED,    ,/, M09. 586
     4 5X, 107HALTERNATING RELATIVE MAXIMA AND RELATIVE MINIMA HAVE BEENM09. 587
     5 OBSERVED.   THIS IS TAKEN AS A SIGN OF A SPURIOUS   ,/,          M09. 588
     6 5X, 107HMATHEMATICAL OSCILLATION, SOMETHING WHICH SHOULD NOT EXISM09. 589
     7T PHYSICALLY (AT LEAST NOT FOR AN INTELLIGENT USER   ,/,          M09. 590
     8 5X, 107HWHO HAS PICKED THE TIME-STEP SIZE =DELTAT= AND THE OUTPUTM09. 591
     9 FREQUENCY "IPLOT" PROPERLY).   AT THIS POINT, THE   )            M29.2699
      WRITE(LUNIT6, 2221)                                               M17.1136
 2221 FORMAT( 5X, 100HOMNISCIENT AND OMNIPOTENT EMTP (OTHERWISE AFFECTIOM28.6768
     1NATELY REFERRED TO AS =BIG BROTHER= BY THOSE USERS   ,/,          M09. 595
     2 5X, 111HWHO ARE ACCUSTOMED TO HIS MODUS OPERANDI) HAS DECIDED TO M09. 596
     3SMOOTH THIS CURVE FOR ALL LATER TIME.   THIS SMOOTHING   ,/,      M09. 597
     4 5X, 112HINVOLVES SIMPLY THE AVERAGING OF SUCCESSIVE ORDINATES IN M09. 598
     5THE OUTPUT FILE OF PLOT-VARIABLE POINTS FOR THIS CURVE,   ,/,     M09. 599
     6 5X,  42HBEFORE PLOTTING BEYOND THIS POINT IN TIME.   )           M09. 600
 2240 IP1 = I + JPLT2                                                   M22.5616
      IF( IP1 .LE. KPL ) GO TO 2241                                     M22.5617
      IP1 = I - JPLT2                                                   M22.5618
      EV(I) = EV(IP1)                                                   M22.5619
      GO TO 2242                                                        M22.5620
 2241 EV(I) = ( EV(IP1) + EVP ) / 2.0                                   M22.5621
 2242 EVP = EV(I)                                                       M22.5622
      GO TO 2260                                                        M09. 603
 2250 ISWW(J) = 1                                                       M09. 604
      GO TO 2260                                                        M09. 605
 2255 ITIMES(J) = 1                                                     M09. 606
      ISWW(J) = 0                                                       M09. 607
 2260 EVDH(J) = EVDP                                                    M09. 608
      EVH(J) = EVP                                                      M09. 609
      D1 = ABS(EVP)                                                     M09. 610
      IF ( D1  .GT.  EVMX )   EVMX = D1                                 M09. 611
      IF ( EVP  .LT.  VMINR )   GO TO 2280                              M09. 612
      IF ( EVP  .GT.  VMAXR )   GO TO 2300                              M09. 613
      GO TO 2310                                                        M09. 614
 2280 EV(I) = VMINR                                                     M09. 615
      GO TO 2310                                                        M09. 616
 2300 EV(I) = VMAXR                                                     M09. 617
 2310 IF ( IOUT  .EQ.  1 )   GO TO 2320                                 M09. 618
      IF ( EV(I)  .GT.  VMAXL )   VMAXL = EV(I)                         M09. 619
      IF ( EV(I)  .LT.  VMINL )   VMINL = EV(I)                         M09. 620
 2320 CONTINUE                                                          M09. 621
 2340 CONTINUE                                                          M09. 622
      IF ( IOVFL  .EQ.  0 )   GO TO 2350                                M09. 623
      WRITE (LUNT13) ( EV(J),J=1,KPL )                                  M32.7597
      IF ( IEND  .EQ.  1 )   GO TO 2350                                 M09. 625
      KPL = 0                                                           M09. 626
      GO TO 2115                                                        M09. 627
 2350 STP = 1.0  +  (HPI/TMULT)/  (  EV(JPLT2+1) - EV(1) )              M09. 628
      IF ( IOUT  .EQ.  1 )   GO TO 2346                                 M21. 387
      IF ( VMAXL  .NE.  0.0 )   GO TO 2346                              M21. 388
      IF ( VMINL  .NE.  0.0 )   GO TO 2346                              M21. 389
      WRITE (LUNIT6, 2345)                                              M21. 390
 2345 FORMAT ( /,  98H ABORT THIS PLOT REQUEST, BECAUSE ALL VARIABLES ARM21. 391
     1E IDENTICALLY ZERO OVER THE REQUESTED TIME SPAN.    )             M21. 392
      GO TO 1000                                                        M21. 393
 2346 JSTP = STP                                                        M21. 394
      IF( IPRSUP .GE. 1 )                                               M09. 630
     1 WRITE(LUNIT6, 2342)  JPLT, KPLT, VMAX, VMIN, EVMX, VMINR, VMAXR  M17.1138
 2342 FORMAT( /,  25H PLOT PARAMETERS AT 2342.  , 2I10, 5E15.5 )        M09. 632
      IF (VMAX.NE.C1E12.OR.VMIN.NE.(-C1E12)) GO TO 2540                 M09. 633
      IF(KIKOY.EQ.99)GO TO 2540                                         M22.5623
      IF (EVMX.EQ.0.0) EVMX=1.0                                         M09. 634
      A=8.0                                                             M09. 635
      EVMXF = EVMX                                                      VAX.3017
      IF ( KBOUND .EQ. 1 ) GO TO 2360                                   VAX.3018
      EVMXF = EVMX * 4.0                                                M09. 636
      EVMXF = EVMXF / 5.0                                               M09. 637
 2360 D1=EVMXF-A                                                        M09. 638
      IF (D1.EQ.0.0) GO TO 2400                                         M09. 639
      IF (D1.GT.0.0) GO TO 2380                                         M09. 640
      A=A/10.                                                           M09. 641
      GO TO 2360                                                        M09. 642
 2380 A=A*10.                                                           M09. 643
      D1=EVMXF-A                                                        M09. 644
      IF (D1.EQ.0.0) GO TO 2400                                         M09. 645
      IF (D1.GT.0.0) GO TO 2380                                         M09. 646
      D1=EVMXF- A / 2.0                                                 M09. 647
      IF (D1.EQ.0.0) GO TO 2440                                         M09. 648
      IF (D1.LT.0.0) GO TO 2420                                         M09. 649
 2400 VMAX=A                                                            M09. 650
      GO TO 2520                                                        M09. 651
 2420 D1=EVMXF- A / 4.0                                                 M09. 652
      IF (D1.LT.0.0) GO TO 2460                                         M09. 653
      IF (D1.EQ.0.0) GO TO 2480                                         M09. 654
 2440 VMAX=A/2.0                                                        M09. 655
      GO TO 2520                                                        M09. 656
 2460 IF (EVMXF-A/8.0.LE.0.0) GO TO 2500                                M09. 657
 2480 VMAX=A/4.0                                                        M09. 658
      GO TO 2520                                                        M09. 659
 2500 VMAX=A/8.0                                                        M09. 660
 2520 VMIN=-VMAX                                                        M09. 661
 2540 CONTINUE                                                          M09. 662
      DO 2550 J=1,13                                                    M09. 663
 2550 AUPPER(J) = BLANK                                                 M09. 664
      LONG1 = 1                                                         M28.6769
      JSL = JPLT                                                        M09. 666
      IF ( ICP .GE. 8 )  JSL = JPLT2                                    M09. 667
      DO 2568 I = 1,JSL                                                 M09. 668
      IF ( SLOT(I) .EQ. BLANK )  SLOT(I) = TERRA                        M09. 669
      CALL PACKCH ( SLOT(I), AUPPER(1), MM6, LONG1, MM1 )               M28.6770
      LONG1 = LONG1 + 6                                                 M28.6771
      CALL PACKCH ( BLANKA(1), AUPPER(1), MM4, LONG1, MM1 )             M29.2700
 2568 LONG1 = LONG1 + 2                                                 M28.6773
      JCHAR = LONG1                                                     M28.6774
      IF ( IPRSUP  .GE.  1 )                                            M09. 675
     1 WRITE (LUNIT6, 2569)  AUPPER                                     M17.1139
 2569 FORMAT ( 27H AT 2569 PACKED NODE NAMES., 8A10 )                   M10. 257
      IF ( IPRSUP .GE. 1 )                                              M09. 678
     1 WRITE (LUNIT6,4100) TOLRCE                                       M17.1140
 4100 FORMAT ( 11H TOLERANCE=, E12.6 )                                  M09. 680
      IF ( XYPLOT(1)  .EQ.  0.0 )   GO TO 2570                          M28.6775
C     IF ( IOUT  .EQ.  1 )   GO TO 12581  VB REMOVAL (JUNE 82)          M32.7598
      VMINL = XYPLOT(5)                                                 M28.6777
      VMAXL = XYPLOT(6)                                                 M28.6778
      JPLT2 = 1                                                         M28.6779
 2570 DX = HPI / TMULT                                                  M28.6780
      DY =  (VMAX - VMIN) / VHS                                         M09. 682
      D1 = LNPIN                                                        M09. 683
      DXL = ( HPIL / TMULT ) / D1                                       M09. 684
      DXL2 = DXL / 2.0                                                  M09. 685
      DYL = ( VMAXL - VMINL ) / 130.                                    M09. 686
      IF(KIKOY.EQ.99)DYL=(VMAX-VMIN)/130.                               M22.5624
      IF ( IPRSUP .GE. 1 )                                              M09. 687
     1  WRITE (LUNIT6,2571) DXL, VMAXL, VMINL, DYL                      M17.1141
 2571 FORMAT ( 8H AT 2580, 4E15.6 )                                     M09. 689
      JPTS = KPLT / JPLT2                                               M09. 690
      IF ( JPTS  .LT.  LINLIM )   JPTS = LINLIM                         M22.5625
      LNFLG = 0                                                         M09. 691
      IPL1 = 1                                                          M09. 692
 2580 IPL2 = IPL1 + NCRV - 1                                            M09. 693
      IF ( IPL2 .GT. JPLT )  IPL2 = JPLT                                M09. 694
      KPL = KPLT                                                        M09. 695
      JOVFL = IOVFL                                                     M09. 696
      LNCK = 0                                                          M09. 697
      IF ( IPRSUP .GE. 1 )                                              M09. 698
     1  WRITE (LUNIT6, 2583) IPL1, IPL2                                 M17.1142
 2583 FORMAT (16H AT 2580, IPL1,2, 2I10 )                               M09. 700
      IF ( IOUT .EQ. 1 )  GO TO 12581                                   M29.2702
      IF ( LNFLG .EQ. 1 )  GO TO 3260                                   M09. 702
      IF ( MPAGE .EQ. 1 )  WRITE (LUNIT6, 3179)                         M33.2179
 3179 FORMAT (  1H1  )                                                  M33.2180
      IF ( MPAGE .EQ. 0 )  WRITE (LUNIT6, 3180)                         M33.2181
 3180 FORMAT ( ////,1X)                                                 M15.1395
      IF ( BUS1 .NE. BLANK )  GO TO 3190                                M09. 705
      IF ( BUS2 .NE. BLANK )  GO TO 3190                                M09. 706
      IF ( BUS3 .NE. BLANK )  GO TO 3190                                M09. 707
      GO TO 3205                                                        M09. 708
 3190 WRITE (LUNIT6,3200) BUS1, BUS2, BUS3                              M17.1144
 3200 FORMAT (58X, 2A6, A4, //,1X)                                      M15.1396
 3205 DO 3206 J = 1, 13                                                 M09. 711
      IF ( CSTXT(J) .NE. BLANK )  GO TO 3207                            M09. 712
 3206 CONTINUE                                                          M09. 713
      GO TO 3209                                                        M09. 714
 3207 WRITE (LUNIT6, 3208 ) CSTXT                                       M17.1145
 3208 FORMAT ( 27X, 13A6, /,1X)                                         M15.1397
 3209 IF ( IALF .EQ. 0 )  GO TO 3212                                    M11. 326
      DO 3210 J = 1, IALF, 13                                           M11. 327
      J1 = J + 12                                                       M09. 718
 3216 WRITE (LUNIT6, 3211)  ( PLTLE(K), K=J, J1 )                       M17.1146
 3211 FORMAT ( 27X, 13A6 )                                              M09. 724
      DO 3217 K = J, J1                                                 M09. 725
 3217 PLTLE(K) = BLANK                                                  M09. 726
 3210 CONTINUE                                                          M09. 727
 3212 J = COUNTP                                                        M11. 328
      WRITE (LUNIT6, 3220)  DATE1, TCLOCK, J, ICP, SLOT                 M17.1147
 3220 FORMAT ( /, 1X, 2A4, 2X, 2A4, 6X, I2, /, 10H PLOT TYPE, 5X,       M09. 730
     1 I1, /,  11H NODE NAMES, 5X, 8( A6, 3X ), /,1X)                   M15.1398
      K = 4 * ( IHS - 1 )  +  1                                         M09. 732
      J1 = K + 3                                                        M09. 733
      WRITE (LUNIT6, 3230)  ( TEXTAX(J), J=K, J1 )                      M17.1148
 3230 FORMAT ( 1X, 4A6, /,1X)                                           M15.1399
      D1 = ABS ( VMAXL )                                                M09. 736
      D2 = ABS ( VMINL )                                                M09. 737
      IF(KIKOY.EQ.99)D1=ABS(VMAX)                                       M22.5626
      IF(KIKOY.EQ.99)D2=ABS(VMIN)                                       M22.5627
      IF ( D1 .LT. D2 )  D1 = D2                                        M09. 738
      K = 0                                                             M09. 739
      IF ( D1 .LT. 1.0 )  GO TO 3232                                    M09. 740
 3231 IF ( D1 .LT. 10.0 )  GO TO 3235                                   M09. 741
      K = K + 1                                                         M09. 742
      D1 = D1 / 10.0                                                    M09. 743
      GO TO 3231                                                        M09. 744
 3232 IF ( D1*FLTINF   .LT.   1.0 )   GO TO 3235                        M13.3856
43232 IF ( D1  .GE.  1.0 )   GO TO 3235                                 M13.3857
      K = K - 1                                                         M09. 746
      D1 = D1 * 10.0                                                    M09. 747
      GO TO 43232                                                       M13.3858
 3235 IF ( K .NE. 0 )  GO TO 3237                                       M09. 749
      WRITE (LUNIT6, 3236) BUS4, BUS5, BUS6                             M17.1149
 3236 FORMAT ( 51X, 2A6, A4 )                                           M09. 751
      EXPNT = 1.0                                                       M09. 752
      GO TO 3240                                                        M09. 753
 3237 WRITE (LUNIT6, 3238) BUS4, BUS5, BUS6, K                          M17.1150
 3238 FORMAT ( 51X, 2A6, A4, 12H  (  X 10**(, I3, 4H)  )  )             M09. 755
      K = - K                                                           M09. 756
      EXPNT = 10.0 ** K                                                 M09. 757
 3240 D3 = VMINL                                                        M09. 758
      IF(KIKOY.EQ.99)D3=VMIN                                            M22.5628
      DO 3250 J = 1, 14                                                 M09. 759
      VRTNUM(J) = D3 * EXPNT                                            M09. 760
 3250 D3 = D3 + (DYL * 10.0)                                            M09. 761
      WRITE ( LUNIT6, 3255 ) VRTNUM                                     M17.1151
 3255 FORMAT ( F7.3, F8.3, F9.3, 10F10.3, F8.3 )                        M09. 763
      LONG1 = JDUMY                                                     M28.6781
      CALL LINPLT ( MM0, LONG1 )                                        M28.6782
      IF ( XYPLOT(1)  .NE.  0.0 )   GO TO 2581                          M28.6783
      LCNT = -1                                                         M09. 765
      HPT = HMIN                                                        M09. 766
      JCOL = 0                                                          M09. 767
      MDPT = 0                                                          M09. 768
      IF((VMINL.GT.0.0.AND.KIKOY.NE.99).OR.(VMIN.GT.0.0.AND.            M22.5629
     1    KIKOY.EQ.99)  ) GO TO 3260                                    M22.5630
      IF((VMAXL.LT.0.0.AND.KIKOY.NE.99).OR.(VMAX.LT.0.0.AND.            M22.5631
     1    KIKOY.EQ.99)  ) GO TO 3260                                    M22.5632
      D3 = -VMINL / DYL  +  1.5                                         M09. 771
      IF(KIKOY.EQ.99)D3=-VMIN/DYL+1.5                                   M22.5633
      MDPT = D3                                                         M09. 772
 3260 IF ( IOUT .EQ. 2 )  GO TO 5975                                    M09. 773
      GO TO 12581                                                       M29.2703
C     BEGIN X-Y PRINTER PLOT CODE  &&&&&&&&&&&&&&&&&&&&&&&&&&&          M28.6784
 2581 N1 = XYPLOT(1) * LNPIN                                            M29.2704
      N2 = XYPLOT(4) * XYPLOT(9)                                        M28.6787
      N3 = N1 * N2                                                      M28.6788
      N4 = MAXEVK - N3                                                  M28.6789
      N11 = KPL * NBYTE(5) / NBYTE(4)                                   M28.6790
      IF ( N4  .GT.  N11 )   GO TO 2739                                 M28.6791
      WRITE (LUNIT6, 2734)  N11, MAXEVK, N3, KPL                        M28.6792
 2734 FORMAT (  5X,  34H X-Y PRINTER PLOT CANCELLED DUE TO,             M28.6793
     1               28H INSUFFICIENT WORKING SPACE.,  4I8 )            M28.6794
      GO TO 2819                                                        M28.6795
 2739 N5 = N11 + 1                                                      M28.6796
      N6 = N5 + N3                                                      M28.6797
      DO 2744  J=N5, N6                                                 M28.6798
 2744 KARRAY(J) = 0                                                     M28.6799
      D1 = N1 / ( XYPLOT(3) - XYPLOT(2) )                               M28.6800
      D2 = N2 / ( XYPLOT(6) - XYPLOT(5) )                               M28.6801
      N10 = -XYPLOT(5) * D2  +  0.5                                     M28.6802
      IF ( N10  .GT.  131 )   N10 = 0                                   M32.7600
      IF ( N10  .LT.    0 )   N10 = 0                                   M28.6804
      N5 = 1                                                            M28.6805
      N9 = JPLT / 2                                                     M28.6806
      DO 2773  I=1, MAXEVK                                              M28.6807
      DO 2768  J=1, N9                                                  M28.6808
      K = ( EV(N5) - XYPLOT(2) ) * D1  +  0.5                           M28.6809
      IF ( K  .GT.  N1 )   K = N1                                       M28.6810
      IF ( K  .LE.   0 )   K = 1                                        M28.6811
      L = ( EV(N5+1) - XYPLOT(5) ) * D2  +  0.5                         M28.6812
      IF ( L  .GT.  N2 )   L = N2                                       M28.6813
      IF ( L  .LE.   0 )   L = 1                                        M28.6814
      N8 = N11 + (K-1)*N2 + L                                           M28.6815
      IF ( KARRAY(N8)  .GT.  0 )   GO TO 2756                           M28.6816
      KARRAY(N8) = J                                                    M28.6817
      GO TO 2763                                                        M28.6818
 2756 IF ( KARRAY(N8)  .NE.  J )                                        M28.6819
     1 KARRAY(N8) = 99                                                  M28.6820
 2763 N5 = N5 + 2                                                       M28.6821
      IF ( N5  .GT.  KPL )   GO TO 2781                                 M28.6822
 2768 CONTINUE                                                          M28.6823
 2773 CONTINUE                                                          M28.6824
      CALL STOPTP                                                               
 2781 N8 = N11                                                          M28.6826
      N7 = LNPIN                                                        M28.6827
      N9 = 77                                                           M28.6828
      D3 = XYPLOT(2)                                                    M28.6829
      D4 = ( XYPLOT(3) - XYPLOT(2) ) / XYPLOT(1)                        M28.6830
      CALL LINPLT ( LL0, LL1 )                                          M28.6831
      DO 2816  K=1, N1                                                  M28.6832
      IF ( N10  .GT.  0 )                                               M28.6833
     1 CALL LINPLT ( LLMIN1, N10 )                                      M28.6834
      N7 = N7 - 1                                                       M28.6835
      DO 2807  J=1, N2                                                  M28.6836
      N8 = N8 + 1                                                       M28.6837
      M = KARRAY(N8)                                                    M28.6838
      IF ( M  .EQ.  0 )   GO TO 2807                                    M28.6839
      CALL LINPLT ( M, J )                                              M28.6840
 2807 CONTINUE                                                          M28.6841
      CALL LINPLT ( N9, LLMIN1 )                                        M28.6842
      IF ( N7  .GT.  0 )   GO TO 2816                                   M28.6843
      D3 = D3 + D4                                                      M28.6844
      WRITE (KUNIT6, 2811)  D3                                          M35.8862
 2811 FORMAT ( 1H+, E12.3 )                                             M28.6846
      N7 = LNPIN                                                        M28.6847
 2816 CONTINUE                                                          M28.6848
 2819 D1 = 1.0 / D1                                                     M28.6849
      D2 = 1.0 / D2                                                     M28.6850
      WRITE (LUNIT6, 2822)  D1, D2                                      M28.6851
 2822 FORMAT (  35H JUST-COMPLETED X-Y PLOT: DX/LINE =,                 M28.6852
     1  E14.6, 5X,   11HDY/COLUMN =,  E14.6  )                          M28.6853
C     X-Y PRINTER PLOT ENDS HERE.   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& M28.6854
      IF ( IOUT  .EQ.  2 )   GO TO 1000                                 M28.6855
12581 IF ( KALCOM  .EQ.  1 )   GO TO 2582                               M28.6856
      KALCOM = 1                                                        M19.2177
      CALL BEGPLT                                                       M19.2178
      IF ( KSCALE .EQ. 1 )                                              VAX.3019
     1 CALL FACTOR ( D4FACT )                                           VAX.3020
      CALL PLOT ( ONE, VS, LLMIN3 )                                     M19.2179
 2582 IPRSOV(38) = IPRSOV(38) + 1                                       M39.3306
      IF ( MULPLT(2)  .GT.  0 )   GO TO 5975                            M22.5634
      JALF=1                                                            M09. 775
      CALL NEWPEN ( LL1 )                                               M19.2181
      CALL PRNTHD ( KPRHD )                                             M09. 776
      D7 = VHS                                                          M19.2182
      IF ( KPGRID  .GT.  0 )   D7 = D7 + 0.1                            M19.2183
      CALL SYMBOL ( HALF, D7, HGT1, SEXT(1), ZERO, LL78 )               M19.2184
      CALL SYMBOL ( FL2P5, VHS+HALF, HGT2, HEADL(1), ZERO, LL16 )       M09. 778
      IF ( MULPLT(1)  .EQ.  0 )   GO TO 2593                            M22.5635
      CALL SYMBOL ( HALF, FOURTH, HGT1, DAYTIM(1), ZERO, LL18 )         M22.5636
      CALL SYMBOL ( HALF, FL3P5,  HGT1, USERID,    ZERO, LL6  )         M28.6857
      IF ( MULPLT(4)  .EQ.  0 )   GO TO 5962                            M22.5637
      CALL PLOT ( ZERO, VHS, LL3 )                                      M22.5638
      CALL PLOT ( ZERO, ZERO, LL2 )                                     M22.5639
      D4 = -.08                                                         M22.5640
      D6 = .08                                                          M22.5641
      D5 = -.0001                                                       M22.5642
 2589 CALL PLOT ( D4, D5, LL3 )                                         M22.5643
      CALL PLOT ( D6, D5, LL2 )                                         M22.5644
      D5 = D5 + 1.0                                                     M22.5645
      IF ( D5  .LE.  VHS )   GO TO 2589                                 M22.5646
      GO TO 5963                                                        M22.5647
 2593 CALL SYMBOL ( HALF, HALF, HGT1, DAYTIM(1), ZERO, LL18 )           M22.5648
      CALL NUMBER ( FL3, HALF, HGT1, COUNTP, ZERO, LLMIN1 )             M09. 780
      CALL PACKCH ( TEXT14(1), ARCH10(1), MM6, MM1, MM2 )               M29.2705
      CALL SYMBOL (HALF, FOURTH, HGT1, ARCH10(1) , ZERO, LL10 )         M15.1402
      RICP = ICP                                                        M09. 782
      CALL NUMBER( FLTWO, FOURTH, HGT1, RICP, ZERO, LLMIN1 )            M09. 783
      CALL PACKCH ( TEXT16(1), ARCH10(1), MM6, MM1, MM2 )               M29.2706
      CALL SYMBOL ( HALF, ZERO, HGT1, ARCH10(1), ZERO, LL10 )           M15.1405
      CALL SYMBOL ( FLTWO, ZERO, HGT1, AUPPER(1), ZERO, JCHAR )         M09. 785
 5962 VHS1 = D7 - HALF                                                  M22.5649
 2600 IF (JALF.GT.IALF) GO TO 2620                                      M09. 787
      CALL SYMBOL ( FL1P5, VHS1, HGT1, ALPHA(JALF), ZERO, LL78 )        M09. 788
      VHS1 = VHS1 - HALF                                                M09. 789
      JALF = JALF + 13                                                  M09. 790
      GO TO 2600                                                        M09. 791
 2620 IF ( XYPLOT(1)  .EQ.  0.0 )   GO TO 3496                          M23.5629
      N7 = KPL / 2                                                      M23.5630
      IF ( IPRSUP  .GE.  1 )                                            M23.5631
     1 WRITE (LUNIT6, 3413)  KPL, JPLT, EV(1), EV(2)                    M23.5632
 3413 FORMAT ( /,  41H BEFORE CALL TO  'SCALE'  FOR  X-Y  PLOT.,        M23.5633
     1  16H     KPL    JPLT,  11X,  4HX(1),  11X,  4HY(1)  ,/,          M23.5634
     2 41X,  2I8,  2E15.6  )                                            M23.5635
      IF ( IPRSUP  .GE.  1 )                                            M23.5636
     1 WRITE (LUNIT6, 3416)  XYPLOT                                     M23.5637
 3416 FORMAT ( /,  30H VECTOR  'XYPLOT'  FOLLOWS ...  ,/,  1X, 8E15.5 ) M23.5638
C     NOW TRANSFER TO SINGLE-PRECISION VECTOR FOR CALCOMP               M28.6862
C     CALLS.   XYPLOT  INTERFACED WITH  "FREFLD", RECALL.               M28.6863
      DO 3417  I=1, 8                                                   M28.6864
 3417 XYSHOR(I) = XYPLOT(I)                                             M28.6865
      IF ( XYPLOT(2)  .EQ.  XYPLOT(3) )   GO TO 3419                    M23.5639
      EV(KPL+1)  =  XYPLOT(2)                                           M23.5640
      EV(KPL+3)  =  ( XYPLOT(3) - XYPLOT(2) ) / XYPLOT(1)               M23.5641
      GO TO 3422                                                        M23.5642
 3419 CALL SCALE ( EV(1), XYSHOR(1), N7, LL2, XYSHOR(7) )               M28.6866
 3422 IF ( XYPLOT(5)  .EQ.  XYPLOT(6) )   GO TO 3426                    M23.5644
      EV(KPL+2)  =  XYPLOT(5)                                           M23.5645
      EV(KPL+4)  =  ( XYPLOT(6) - XYPLOT(5) ) / XYPLOT(4)               M23.5646
      GO TO 3429                                                        M23.5647
 3426 CALL SCALE ( EV(2), XYSHOR(4), N7, LL2, XYSHOR(7) )               M28.6867
 3429 D4 = EV(KPL+3)                                                    M23.5649
      D5 = EV(KPL+4)                                                    M23.5650
      IF ( IPRSUP  .GE.  2 )                                            M23.5651
     1 WRITE (LUNIT6, 3432)  EV(KPL+1), EV(KPL+2), D4, D5, EV(1), EV(2) M23.5652
 3432 FORMAT ( /,  20H SCALING PARAMETERS.,  9X,  6HX-BIAS,  9X,        M23.5653
     1  6HY-BIAS,  10X,  5HDX/IN,  10X,  5HDY/IN,  11X,  4HX(1),        M23.5654
     2  11X,  4HY(1)  ,/,  20X,  6E15.6  )                              M23.5655
      DO 3438  I=1, JPLT, 2                                             M23.5656
      N5 = KPL + I                                                      M23.5657
      EV(N5) = EV(KPL+1)                                                M23.5658
      EV(N5+1) = EV(KPL+2)                                              M23.5659
      N5 = N5 + JPLT                                                    M23.5660
      EV(N5) = D4                                                       M23.5661
 3438 EV(N5+1) = D5                                                     M23.5662
      N5 = N5 + 1                                                       M23.5663
      IF ( IPRSUP  .GE.  2 )                                            M23.5664
     1 WRITE (LUNIT6, 3440)  ( EV(I), I=KPL, N5 )                       M23.5665
 3440 FORMAT ( /,  25H (EV(I), I=KPL, N5)  ....   ,/,  ( 1X, 10E13.4 )) M23.5666
      CALL AXIS ( ZERO, ZERO, HORZL(1), LL24, XYSHOR(1),                M28.6868
     1            ZERO, EV(KPL+1), D4 )                                 M23.5668
      CALL AXIS ( ZERO, ZERO, VERTL(1), LL16, XYSHOR(4),                M28.6869
     1            FL90, EV(KPL+2), D5 )                                 M23.5670
      IF ( KPGRID  .EQ.  0 )   GO TO 3442                               M23.5671
      J1 = XYPLOT(1)                                                    M23.5672
      J2 = XYPLOT(4)                                                    M23.5673
      CALL NEWPEN ( KPGRID )                                            M23.5674
      CALL GRID ( ZERO, ZERO, ONE, ONE, J1, J2 )                        M23.5675
 3442 J = 0                                                             M23.5676
      IF ( XYPLOT(8)  .GT.  0.0 )                                       M23.5677
     1 J = N7 / XYPLOT(8)                                               M23.5678
      N5 = KPL / JPLT                                                   M23.5679
      N12 = 0                                                           M23.5680
      DO 3447  I=1, JPLT, 2                                             M23.5681
      N12 = N12 + 1                                                     M23.5682
      CALL NEWPEN ( KPEN(N12) )                                         M23.5683
      IF ( IPRSUP  .GE.  2 )                                            M23.5684
     1 WRITE (LUNIT6, 3444)  I, J, JPLT, N5                             M23.5685
 3444 FORMAT ( /,  30H READY TO CALL  'LINE'  AGAIN.,                   M23.5686
     1  32H       I       J    JPLT      N5  ,/,  30X,  4I8  )          M23.5687
 3447 CALL LINE ( EV(I), EV(I+1), N5, JPLT, J, I )                      M23.5688
      TAXMAX = XYPLOT(1) + 1.0                                          M23.5689
      XYPLOT(1) = 0.0                                                   M23.5690
      IPL2 = JPLT                                                       M23.5691
      GO TO 2656                                                        M23.5692
 3496 IF ( KPRHD  .EQ.  0 )   GO TO 2624                                M23.5693
      CALL PRNTHD ( LL2 )                                               M09. 793
 2624 CALL AXIS ( ZERO, ZERO, VERTL(1),  LL16, VHS, FL90, VMIN,         M09. 794
     1 DY )                                                             M14.2632
 5963 IF ( KPRHD  .GT.  0 )                                             M22.5650
     1 CALL PRNTHD ( LL1 )                                              M22.5651
      IF ( IPL1 .NE. 1 )  GO TO 5970                                    M09. 797
      DLEN = (HMAX - HMIN) * TMULT / HPI                                M09. 798
      IF ( DLEN .LT. 9.0 )  DLEN = 9.0                                  M09. 799
      DSTRT = HMIN * TMULT                                              M09. 800
 5970 IF ( MULPLT(1)  .EQ.  0 )   GO TO 7435                            M22.5652
      IF ( MULPLT(5)  .EQ.  0 )   GO TO 7435                            M22.5653
      CALL PLOT ( ZERO, HA, LL3 )                                       M22.5654
      CALL PLOT ( DLEN, HA, LL2 )                                       M22.5655
      D4 = HA - .08                                                     M22.5656
      D6 = HA + .08                                                     M22.5657
      D5 = .9999                                                        M22.5658
 7433 CALL PLOT ( D5, D4, LL3 )                                         M22.5659
      CALL PLOT ( D5, D6, LL2 )                                         M22.5660
      D5 = D5 + 1.0                                                     M22.5661
      IF ( D5  .LE.  DLEN )   GO TO 7433                                M22.5662
      GO TO 7438                                                        M22.5663
 7435 CALL AXIS ( ZERO, HA, HORZL(1), MHORIZ, DLEN, ZERO, DSTRT, HPI )  M22.5664
 7438 IF ( DLEN  .GT.  TAXMAX )                                         M22.5665
     1 TAXMAX = DLEN                                                    M19.2187
      IF ( KPGRID  .EQ.  0 )   GO TO 5975                               M19.2188
      J1 = DLEN + FLZERO                                                M19.2189
      J2 = VHS  + FLZERO                                                M19.2190
      CALL NEWPEN ( KPGRID )                                            M19.2191
      CALL GRID ( ZERO, ZERO, ONE, ONE, J1, J2 )                        M19.2192
 5975 IEND = KPLT                                                       M09. 803
C     &&&&&&&&&&&&&&&&&   REMOVE  CALL MOVER0  &&&&&&&&&&&&&            M28.6870
      IF ( IOVFL .EQ. 0 )  GO TO 5980                                   M09. 809
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29.2707
      REWIND LUNT13                                                     M32.7601
      IEND = MAXEVK                                                     M09. 811
      READ (LUNT13) ( EV(J), J = 1, IEND )                              M32.7602
      IF ( IPRSUP .GE. 1 )  WRITE ( LUNIT6,5979 ) IEND                  M17.1154
 5979 FORMAT ( 23H EV READ FROM LOGICAL 9, I10 )                        M09. 814
 5980 IF ( IOUT .EQ. 1 )  GO TO 5981                                    M09. 815
      IF ( LNFLG .EQ. 1 )  GO TO 3115                                   M09. 816
 3000 JEND = 0                                                          M09. 817
      KEND = IEND - JPLT2                                               M09. 818
      DO 3005 I = IPL1, IPL2                                            M09. 819
 3005 JPNTR(I) = 2*I - 1                                                M09. 820
      HLO = HPT - DXL2                                                  M19.2193
      HHI = HPT + DXL2                                                  M09. 822
 3010 IF ( IPRSUP  .GE.  5 )                                            M19.2194
     1 WRITE (LUNIT6, 3015)  HLO, HPT, HHI, TMAX                        M20.4937
 3015 FORMAT ( 13X,  3HHLO,  12X,  3HHPT,  12X,  3HHHI,                 M20.4938
     1 11X,  4HTMAX  ,/,  1X,  4E15.6  )                                M20.4939
      DO 3100 I = IPL1, IPL2                                            M09. 826
      KOLD = 0                                                          M09. 827
 3020 J = JPNTR(I)                                                      M09. 828
      EVP = EV(J)                                                       M09. 829
      IF ( EVP  .LT.  HLO )   GO TO 3100                                M19.2195
      IF ( EVP  .GE.  HHI )   GO TO 3100                                M19.2196
      KLM = ( EV(J+1) - VMINL ) / DYL  +  1.5                           M19.2197
      IF(KIKOY.EQ.99)KLM=(EV(J+1)-VMIN)/DYL+1.5                         M22.5670
      IF ( 1  .GT.  KLM )  KLM = 1                                      M32.7603
      IF ( 131 .LT. KLM )  KLM = 131                                    M32.7604
      IF ( KLM  .EQ.  KOLD )   GO TO 3040                               M09. 833
      IF ( KLM  .LE.  9 )   JCOL = 1                                    M09. 834
      LONG1 = I                                                         M28.6871
      LONG2 = KLM                                                       M28.6872
      CALL LINPLT ( LONG1, LONG2 )                                      M28.6873
      KOLD = KLM                                                        M09. 836
      IF ( IPRSUP .GE. 5 )                                              M09. 837
     1  WRITE (LUNIT6, 3035) J, EV(J), EV(J+1), I,KLM                   M17.1156
 3035 FORMAT ( 8H AT 3040 , I10, 2E15.6, 2I10 )                         M09. 839
 3040 JPNTR(I) = JPNTR(I) + JPLT2                                       M09. 840
      D4 = TMAX - EVP                                                   M19.2198
      IF ( D4  .GT.  0.0 )   GO TO 3020                                 M19.2199
      JEND = 1                                                          M19.2200
 3100 CONTINUE                                                          M09. 842
      IF ( IPRSUP  .GE.  6 )                                            M20.4940
     1 WRITE (LUNIT6, 13100)  LNCK, JPTS, LCNT, LNPIN, JCOL, JOVFL,     M20.4941
     2                        JEND, MDPT, IPL1, IPL2, LLMIN1            M20.4942
13100 FORMAT ( 1X,  40H    LNCK    JPTS    LCNT   LNPIN    JCOL,        M20.4943
     1 48H   JOVFL    JEND    MDPT    IPL1    IPL2  LLMIN1   ,/,        M20.4944
     2 1X,  11I8  )                                                     M20.4945
      IF ( JEND  .EQ.  1 )   GO TO 3103                                 M19.2201
      DO 3101 J = IPL1, IPL2                                            M09. 843
      KTRM = ( J*2 - 1 ) + KEND                                         M09. 844
      IF ( JPNTR(J) .GT. KTRM )  GO TO 3101                             M09. 845
      GO TO 3102                                                        M09. 846
 3101 CONTINUE                                                          M09. 847
      JEND = 1                                                          M09. 848
 3103 IF ( JOVFL  .NE.  0 )   GO TO 3115                                M19.2202
 3102 LCNT = LCNT + 1                                                   M09. 850
      LONG1 = JDUMY                                                     M28.6874
      CALL LINPLT ( LONG1, MMMIN1 )                                     M28.6875
      LNCK = LNCK + 1                                                   M09. 852
      IF ( LNCK .GT. JPTS )  GO TO 3111                                 M09. 853
      IF ( LCNT .NE. LNPIN )  GO TO 3110                                M09. 854
      IF ( JCOL  .NE.  0 )   GO TO 3109                                 M15.1406
      HHPT = HPT * TMULT                                                M23.5694
      IF ( HHPT  .GE.  0.9999 )   GO TO 3106                            M23.5695
      WRITE (KUNIT6, 3105)   HHPT                                       M35.8863
 3105 FORMAT ( 1H+, 1X, F8.6 )                                          M22.5673
      GO TO 3109                                                        M15.1410
 3106 D4 = HHPT * .001                                                  M23.5697
      IF ( D4  .GE.  0.9999 )   GO TO 3108                              M15.1412
      WRITE (KUNIT6, 3107)   HHPT                                       M35.8864
 3107 FORMAT ( 1H+, 2X, F7.3 )                                          M15.1414
      GO TO 3109                                                        M15.1415
 3108 D4 = HHPT * 1.E-6                                                 M23.5699
      IF ( D4  .GE.  1.0 )   GO TO 3119                                 M15.1417
      WRITE (KUNIT6, 3104)   HHPT                                       M35.8865
 3104 FORMAT ( 1H+, 2X, F7.0 )                                          M15.1419
      GO TO 3109                                                        M15.1420
 3119 WRITE (KUNIT6, 3121)   HHPT                                       M35.8866
 3121 FORMAT ( 1H+, 2X, E9.3 )                                          M15.1422
 3109 LCNT = 0                                                          M15.1423
 3110 IF ( JEND .EQ. 1 )  GO TO 3115                                    M09. 859
      IF ( MDPT .EQ. 0 )  GO TO 31                                      M10. 259
      IF ( MDPT .GT. 9 )  GO TO 30                                      M10. 260
      J1 = LCNT + 1                                                     M10. 261
      IF ( J1 .EQ. LNPIN )  GO TO 31                                    M10. 262
   30 LONG1 = MDPT                                                      M28.6876
      CALL LINPLT ( MMMIN1, LONG1 )                                     M28.6877
   31 JCOL = 0                                                          M10. 264
      HLO = HHI                                                         M19.2203
      HPT = HLO + DXL2                                                  M19.2204
      HHI = HLO + DXL                                                   M19.2205
      GO TO 3010                                                        M09. 864
 3111 IF ( IOUT .EQ. 2 )  GO TO 3112                                    M09. 865
      LNFLG = 1                                                         M09. 866
      WRITE (LUNIT6, 310)                                               M17.1161
  310 FORMAT ( 99H ****THE NUMBER OF LINES PRINTED FOR THIS PLOT REQUESTM09. 868
     1 NOW EXCEEDS THE TOTAL NUMBER OF DATA POINTS. )                   M09. 869
      WRITE (LUNIT6, 311)                                               M17.1162
  311 FORMAT ( 92H ****SINCE CALCOMP PLOT WAS ALSO SPECIFIED, ONLY PRINTM09. 871
     1ER PLOT IS CANCELLED FOR THIS REQUEST. )                          M09. 872
      GO TO 3115                                                        M09. 873
 3112 WRITE (LUNIT6, 310)                                               M17.1163
      WRITE (LUNIT6, 312)                                               M17.1164
  312 FORMAT ( 75H ****SINCE ONLY PRINTER PLOT WAS SPECIFIED, THIS PLOT M09. 876
     1REQUEST IS CANCELLED. )                                           M09. 877
      GO TO 1000                                                        M09. 878
 3115 IF ( IOUT .EQ. 2 )  GO TO 2645                                    M09. 879
 5981 DO 6240 I = IPL1, IPL2                                            M09. 880
      J = 2*I - 1                                                       M09. 881
      IF ( TOLRCE .GT. FLZERO )  GO TO 5991                             M09. 882
      KPLTQ(I) = IEND / JPLT2                                           M09. 883
      J1 = J + IEND                                                     M09. 884
      GO TO 6145                                                        M09. 885
 5991 KPLTQ(I) = 0                                                      M09. 886
      IBASE = J                                                         M09. 887
      ISTORE = J                                                        M09. 888
      INDX = J + 1 + JPLT2                                              M09. 889
      VOLD = EV(INDX)                                                   M09. 890
 6000 EVBASX = EV(IBASE)                                                M09. 891
      EVBASY = EV(IBASE+1)                                              M09. 892
      EV(ISTORE) = EVBASX                                               M09. 893
      EV(ISTORE+1) = EVBASY                                             M09. 894
      KPLTQ(I) = KPLTQ(I) + 1                                           M09. 895
      ISTORE = ISTORE + JPLT2                                           M09. 896
      IPONTR = IBASE + JPLT2                                            M09. 897
      HVEC =  ( EV(IPONTR) - EVBASX ) / DX                              M09. 898
      VVEC = ( EV(IPONTR+1) - EVBASY ) / DY                             M09. 899
      DENOM = HVEC*HVEC + VVEC*VVEC                                     M09. 900
 6020 IPONTR = IPONTR + JPLT2                                           M09. 901
      IF ( IOVFL  .EQ.  0 )   GO TO 6025                                M09. 902
      IF ( IPONTR  .GT.  IEND )   GO TO 6140                            M09. 903
      GO TO 6030                                                        M09. 904
 6025 IF ( IPONTR  .GT.  KPLT )   GO TO 6140                            M09. 905
 6030 HDIF =  ( EV(IPONTR) - EVBASX ) / DX                              M09. 906
      VNEW = EV(IPONTR+1)                                               M09. 907
      VDIF  =  ( VNEW - EVBASY ) / DY                                   M09. 908
      TERM = HDIF*HDIF + VDIF*VDIF                                      M09. 909
      ENUMR  =  HVEC*HDIF  +  VVEC*VDIF                                 M09. 910
      DISQR  =  TERM  -  ENUMR*ENUMR / DENOM                            M09. 911
      IF ( IPONTR  .LE. 10000   .AND.   I .EQ. 1   .AND.  IPRSUP .GE. 5)M09. 912
     1 WRITE (LUNIT6, 16030 )  I, ISTORE, IPONTR, EV(IPONTR), EVBASX,   M17.1165
     1 DX, VNEW, EVBASY, DY, DENOM, ENUMR, DISQR, HDIF, VDIF,           M09. 914
     2 HVEC, VVEC, TERM                                                 M09. 915
16030 FORMAT ( /, 10H AT 16030  , 3I10, 6E15.5, /, ( 1X, 8E15.5 ) )     M09. 916
      IF ( DISQR  .GT.  TOLRCE )   GO TO 6040                           M09. 917
      VCHNGE  =  VNEW - VOLD                                            M09. 918
      VOLD = VNEW                                                       M09. 919
      IF ( VVEC*VCHNGE .GT. -FLZERO )  GO TO 6020                       M09. 920
 6040 VOLD = VNEW                                                       M09. 921
      IBASE = IPONTR - JPLT2                                            M09. 922
      GO TO 6000                                                        M09. 923
 6140 IPONTR  =  IPONTR - JPLT2                                         M09. 924
      EV(ISTORE)  =  EV(IPONTR)                                         M09. 925
      EV(ISTORE+1)  =  EV(IPONTR+1)                                     M09. 926
      KPLTQ(I) = KPLTQ(I) + 1                                           M09. 927
      J1 = ISTORE + JPLT2                                               M09. 928
 6145 EV(J1) = HMIN                                                     M09. 929
      EV(J1+1) = VMIN                                                   M09. 930
      J1 = J1 + JPLT2                                                   M09. 931
      EV(J1) = DX                                                       M09. 932
      EV(J1+1) = DY                                                     M09. 933
 6240 CONTINUE                                                          M09. 934
 6245 IF ( IPRSUP .GE. 1 )                                              M09. 935
     1 WRITE (LUNIT6, 2630)  ( KPLTQ(I), I=IPL1, IPL2 )                 M17.1166
 2630 FORMAT ( /, 60H AT 2630,AFTER SMOOTHING, THE NUMBER OF POINTS PER M09. 937
     1CURVE ARE, /, 10X, 4I10 )                                         M09. 938
      D6 = VHS - FOURTH * ( 1.0 + MULPLT(3) )                           M22.5674
      D9 = D6 + HGT1 / 2.0                                              M28.6878
      N5 = IPL1 + MULPLT(3) - 1                                         M22.5675
 2640 DO 2642 IPL = IPL1, IPL2                                          M09. 939
      J1 = 2*IPL - 1                                                    M09. 941
      CALL NEWPEN ( KPEN(IPL) )                                         M19.2207
      N5 = N5 + 1                                                       M22.5676
      CALL LINE ( EV(J1), EV(J1+1), KPLTQ(IPL), JPLT2, JSTP, N5 )       M22.5677
      IF ( IOVFL  .EQ.  0 )   GO TO 2641                                M19.2208
      IF ( JOVFL  .GT.  0 )   GO TO 2642                                M19.2209
 2641 IF ( MULPLT(1)  .GT.  0 )   GO TO 7452                            M22.5678
      D7 = DLEN - 1.2                                                   M22.5679
      D8 = IPL                                                          M19.2211
      CALL NUMBER ( D7, VHS1, HGT1, D8, ZERO, LLMIN1 )                  M19.2212
      D7 = DLEN - 1.0                                                   M19.2213
      CALL PLOT ( D7, VHS1, LL3 )                                       M19.2214
      CALL PLOT ( DLEN, VHS1, LL2 )                                     M19.2215
      D7 = DLEN - 0.5                                                   M19.2216
      D8 = VHS1 + 0.1                                                   M19.2217
      CALL SYMBOL ( D7, D8, HGT1, IPL, ZERO, LLMIN1 )                   M19.2218
      VHS1 = VHS1 - HALF                                                M19.2219
      GO TO 2642                                                        M22.5680
 7452 D7 = DLEN + 0.5                                                   M22.5681
      IF ( IPRSUP  .GE.  1 )                                            M22.5682
     1 WRITE (LUNIT6, 7502)  IPL, ICP, MULPLT, TAXMAX, VHS, D6, SLOT    M22.5683
 7502 FORMAT ( /,  24H LEGEND.     IPL     ICP  ,/,                     M22.5684
     1 8X,  6I8,  3E20.6  ,/,  ( 1X,  20A6  ) )                         M22.5685
      D8 = N5                                                           M22.5686
      CALL NUMBER ( D7, D6, HGT1, D8, ZERO, LLMIN1 )                    M22.5687
      D7 = D7 + .35                                                     M22.5688
      CALL SYMBOL ( D7, D9, HGT1, N5, ZERO, LLMIN1 )                    M28.6879
      D7 = D7 + .35                                                     M22.5690
      N6 = IPL                                                          M22.5691
      IF ( ICP  .GE.  8 )                                               M22.5692
     1 N6 = 2 * IPL - 1                                                 M22.5693
      CALL SYMBOL ( D7, D6, HGT1, SLOT(N6), ZERO, LL6 )                 M22.5694
      IF ( MULPLT(3)  .EQ.  0 )                                         M22.5695
     1 TAXMAX = TAXMAX + 2.0                                            M22.5696
      MULPLT(3) = MULPLT(3) + 1                                         M22.5697
      IF ( ICP  .LT.  8 )   GO TO 7468                                  M22.5698
      D7 = D7 + 0.8                                                     M22.5699
      N6 = N6 + 1                                                       M22.5700
      CALL SYMBOL ( D7, D6, HGT1, SLOT(N6), ZERO, LL6 )                 M22.5701
 7468 D6 = D6 - .25                                                     M22.5702
      D9 = D9 - .25                                                     M28.6880
 2642 CONTINUE                                                          M09. 943
 2645 IF ( IOVFL  .EQ.  0 )   GO TO 2650                                M09. 944
      KPL  = KPL  - IEND                                                M09. 945
      IF ( KPL   .EQ.  0 )   GO TO 2650                                 M09. 946
      IEND = KPL                                                        M09. 948
      IF ( IEND .GT. MAXEVK )  IEND = MAXEVK                            M09. 949
      READ (LUNT13)  ( EV(J), J=1, IEND )                               M32.7605
      IF ( IPRSUP .GE. 1 )  WRITE ( LUNIT6,5979 ) IEND                  M17.1168
      J1 = KPL - IEND                                                   M09. 952
      IF ( J1 .EQ. 0 )  JOVFL = 0                                       M09. 953
      GO TO 5980                                                        M09. 954
 2650 IF ( IOUT .EQ. 1 )  GO TO 2653                                    M10. 266
      IF ( LCNT .EQ. 0 )  GO TO 2651                                    M10. 267
      IF ( JCOL .EQ. 1 )  GO TO 2651                                    M10. 268
      IF ( LNFLG .EQ. 1 )  GO TO 2651                                   M10. 269
      HHPT = HPT * TMULT                                                M23.5702
      IF ( HHPT  .GE.  1.0 )   GO TO 2648                               M23.5703
      WRITE (KUNIT6, 3105)   HHPT                                       M35.8867
      GO TO 2651                                                        M21. 397
 2648 WRITE (KUNIT6, 3121)   HHPT                                       M35.8868
 2651 LONG1 = 1                                                         M35.8869
      LONG2 = 75                                                        M35.8870
      CALL LINPLT ( LONG1, LONG2 )                                              
      WRITE (LUNIT6, 2652)                                              M35.8872
 2652 FORMAT ( /, 14H END OF GRAPH. , /,1X)                             M15.1424
      IF ( IOUT .EQ. 2 )  GO TO 2680                                    M09. 958
 2653 D7 = VH + 1.0                                                     M19.2220
      MULPLT(2) = MULPLT(2) + 1                                         M22.5703
      IF ( MULPLT(2)  .LT.  MULPLT(1) )   GO TO 1000                    M22.5704
      MULPLT(1) = 0                                                     M22.5705
      D8 = VPLOFF + D7                                                  M19.2221
      D9 = D8 + VH - .001                                               M19.2222
      IF ( D9  .LE.  SZPLT )   GO TO 2657                               M19.2223
 2656 CALL PLOT ( ZERO, -VPLOFF, LLMIN3 )                               M19.2224
      GO TO 2659                                                        M19.2225
 2657 VPLOFF = D8                                                       M19.2226
      CALL PLOT ( ZERO, D7, LLMIN3 )                                    M19.2227
      GO TO 2680                                                        M19.2228
 2659 HMS = ( TAXMAX + MRGN ) * 1.001                                   M19.2229
      TAXMAX = 0.0                                                      M19.2230
      VPLOFF = 0.0                                                      M19.2231
      JHMSP = HMS                                                       M09. 961
      HMS = JHMSP                                                       M22.5706
      JHMS = JHMSP                                                      M09. 962
 2646 IF ( JHMSP .GT. 60 )  JHMSP = 60                                  M09. 963
      CALL ADVANZ ( JHMSP )                                             M09. 964
      IF ( JHMSP .EQ. 0 )  GO TO 2647                                   M09. 965
      JHMS = JHMS - 60                                                  M09. 966
      IF ( JHMS .LE. 0 )  GO TO 2680                                    M09. 967
      JHMSP = JHMS                                                      M09. 968
      GO TO 2646                                                        M09. 969
 2647 CALL PLOT (  HMS, ZERO, LLMIN3 )                                  M12. 587
 2680 IF ( N8  .EQ.  -6666 )   GO TO 2720                               M19.2232
      IF ( IPL2  .EQ.  JPLT )   GO TO 1000                              M19.2233
      IPL1 = IPL2 + 1                                                   M09. 972
      GO TO 2580                                                        M09. 973
 2720 IF ( KALCOM  .EQ.  1 )                                            M19.2234
     1 CALL PLOT ( ZERO, -VS, LLMIN3 )                                  M19.2235
      LONG1 = KALCOM                                                    M28.6881
      CALL ENDPLT ( LONG1 )                                             M28.6882
      IF ( ISPRIN  .NE.  43 )   GO TO 7416                              M22.5708
      LASTOV = NCHAIN                                                   M22.5709
      NCHAIN = 43                                                       M22.5710
      GO TO 9999                                                        M22.5711
 7416 LASTOV = NCHAIN                                                   M22.5712
      IF ( KBASE  .NE.  1 )   GO TO 2724                                M19.2236
 2723 KBASE = 2                                                         M19.2237
      NCHAIN = 12                                                       M19.2238
      GO TO 9999                                                        M19.2239
 2724 NCHAIN = 51                                                       M19.2240
      GO TO 9999                                                        M19.2241
 2740 CALL FINTP                                                                
      CALL STOPTP                                                               
 9200 LSTAT(18) = 31                                                    M09. 982
      LASTOV = NCHAIN                                                   M15.1427
      NCHAIN = 51                                                       M13.3859
 9999 RETURN                                                            M19.2243
      END                                                               M09. 986
      SUBROUTINE SERIES ( NFOUR, KPL, JPLT, MAXEVK )                    M28.6883
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DECK31.FOR;/NOLIST'                                           
      DIMENSION EVDOUB(2)                                               M39.3307
      EQUIVALENCE ( EVDOUB(1), KARRAY(1) )                              M28.6887
      IF ( NFOUR  .GT.  0 )   GO TO 3184                                M28.6888
      NFOUR = -NFOUR                                                    M28.6889
      N2 = KPL                                                          M28.6890
      D1 = EVDOUB(KPL) - EVDOUB(1)                                      M28.6891
 3163 N2 = N2 - 1                                                       M28.6892
      D2 = EVDOUB(N2)  - EVDOUB(1)                                      M28.6893
      IF ( D1*D2  .GT.  0.0 )   GO TO 3175                              M28.6894
      IF ( ABSZ(D1)  .GT.  ABSZ(D2) )   N2 = N2 - 1                     M28.6895
      WRITE (LUNIT6, 3168)  KPL, N2, EVDOUB(N2+1)                       M28.6896
 3168 FORMAT ( /,                                                       M28.6897
     1         37H BACK UP FINAL TIME FROM POINT NUMBER,                M28.6898
     1   I5,   18H   TO POINT NUMBER,                                   M28.6899
     2   I5,   44H ,   SO THAT SIGNAL WILL BE NEARLY PERIODIC.          M28.6900
     3   ,/,   44H THE FOLLOWING UNUSED POINT, WHICH HAS VALUE,         M28.6901
     4         34H CLOSEST TO POINT NUMBER 1, EQUALS,  E15.6  )         M28.6902
      KPL = N2                                                          M28.6903
      GO TO 3184                                                        M28.6904
 3175 D1 = D2                                                           M28.6905
      GO TO 3163                                                        M28.6906
 3184 N4 = KPL / 2  + 2                                                 M28.6907
      N1 = KPL + 2*N4                                                   M28.6908
      IF ( N1  .LE.  MAXEVK )   GO TO 3209                              M28.6909
      WRITE (LUNIT6, 3203)  N1, MAXEVK                                  M28.6910
 3203 FORMAT ( /,  37H SORRY, NO FOURIER SERIES IS POSSIBLE,            M28.6911
     1             33H DUE TO LACK OF EMTP TABLE SPACE.,                M28.6912
     2      I8,    35H   FLOATING-POINT CELLS ARE NEEDED,  ,/,          M28.6913
     3             11H WHILE ONLY,   I8,                                M28.6914
     4             39H   ARE AVAILABLE.   ON TO NEXT REQUEST. )         M28.6915
      GO TO 9000                                                        M28.6916
 3209 IOFFA = KPL                                                       M28.6917
      IOFFB = KPL + N4                                                  M28.6918
      IF ( KPL  .GE.  2 )   GO TO 3216                                  M28.6919
      WRITE (LUNIT6, 3213)  KPL                                         M28.6920
 3213 FORMAT ( /,  37H SORRY, NO FOURIER SERIES IS POSSIBLE,            M28.6921
     1             37H DUE TO LACK OF DATA.   THE NUMBER OF,            M28.6922
     2             26H DATA POINTS FOUND IS ONLY,  I5,                  M28.6923
     3              3H  .   ,/,   24H TWO OR MORE ARE NEEDED.,          M28.6924
     4             24H ON TO THE NEXT REQUEST.  )                       M28.6925
      GO TO 9000                                                        M28.6926
 3216 IF ( JPLT  .EQ.  1 )   GO TO 3226                                 M28.6927
      WRITE (LUNIT6, 3219)  JPLT                                        M28.6928
 3219 FORMAT ( /,  37H SORRY, NO FOURIER SERIES IS POSSIBLE,            M28.6929
     1             21H DUE TO THE NAMING OF,  I4,                       M28.6930
     2             36H   VARIABLES.   ONLY ONE IS ALLOWED.,             M28.6931
     3             21H  ON TO NEXT REQUEST.  )                          M28.6932
      GO TO 9000                                                        M28.6933
 3226 WRITE (LUNIT6, 3231)  KPL, EVDOUB(1), EVDOUB(2),                  M28.6934
     1                      EVDOUB(KPL-1), EVDOUB(KPL)                  M28.6935
 3231 FORMAT ( /,  33H BEGIN FOURIER SERIES CALCULATION,                M28.6936
     1              6H USING,  I6,                                      M28.6937
     2             34H   EQUIDISTANT POINTS.   BEGINNING,               M28.6938
     3             13H TWO POINTS =    ,/,  2E20.10,  9X,               M28.6939
     4             19HENDING TWO POINTS =,  2E20.10  )                  M28.6940
      IF ( IPRSUP  .GE.  2 )                                            M28.6941
     1 WRITE (LUNIT6, 3238)  ( EVDOUB(J), J=1, KPL )                    M28.6942
 3238 FORMAT ( /,  37H LIST OF ALL DATA POINTS FOLLOWS ....             M28.6943
     1        ,/, ( 1X,  10E13.4 ) )                                    M28.6944
      PI = TWOPI * ONEHAF                                               M28.6945
      AN = KPL                                                          M28.6946
      AN = AN * ONEHAF                                                  M28.6947
      J = AN + 1.6                                                      M28.6948
      M = AN + 1.0                                                      M28.6949
      IF ( NFOUR  .GT.  M )   NFOUR = M                                 M28.6950
      D12 = 1.0 / AN                                                    M28.6951
      D7 = PI * D12                                                     M28.6952
      C1 = COSZ ( D7 )                                                  M28.6953
      S1 = SINZ ( D7 )                                                  M28.6954
      CP = 1.0                                                          M28.6955
      SP = 0.0                                                          M28.6956
      NDX1 = IOFFA                                                      M28.6957
      NDX2 = IOFFB                                                      M28.6958
      DO 4288  L=1, M                                                   M28.6959
      GK2 = 0.0                                                         M28.6960
      GK1 = 0.0                                                         M28.6961
      K = KPL                                                           M28.6962
 4020 GK = EVDOUB(K)  +  CP * GK1 * 2.0  -  GK2                         M28.6963
      GK2 = GK1                                                         M28.6964
      GK1 = GK                                                          M28.6965
      K = K - 1                                                         M28.6966
      IF ( K  .GT.  1 )   GO TO 4020                                    M28.6967
      AP = ( EVDOUB(1) + GK1*CP - GK2 ) * D12                           M28.6968
      BP = SP * GK1 * D12                                               M28.6969
      IF ( L  .EQ.  1 )   GO TO 4030                                    M28.6970
      IF ( L  .NE.  J )   GO TO 4040                                    M28.6971
      BP = 0.0                                                          M28.6972
 4030 AP = AP * ONEHAF                                                  M28.6973
 4040 NDX1 = NDX1 + 1                                                   M28.6974
      NDX2 = NDX2 + 1                                                   M28.6975
      EVDOUB(NDX1) = AP                                                 M28.6976
      EVDOUB(NDX2) = BP                                                 M28.6977
      AP = C1 * CP  -  S1 * SP                                          M28.6978
      SP = C1 * SP  +  S1 * CP                                          M28.6979
      CP = AP                                                           M28.6980
 4288 CONTINUE                                                          M28.6981
      WRITE (LUNIT6, 4317)                                              M28.6982
 4317 FORMAT ( /,  42H COEFFICIENTS OF RESULTANT FOURIER SERIES,,       M28.6983
     1    49H WITH  "COMPLEX AMPLITUDE"  BEING THE SQUARE ROOT          M28.6984
     2       ,/,   37H OF THE SUM OF THE SQUARES OF THE TWO,            M28.6985
     3    46H PRECEDING ENTRIES.   THE FINAL COLUMN APPLIES,            M28.6986
     3    19H TO THIS AMPLITUDE.                                        M28.6987
     4   ,/,  2X,  8HHARMONIC,  14X,  6HCOSINE,  16X,  4HSINE,          M28.6988
     5       13X,  7HCOMPLEX,  9X,  11HFRACTION OF                      M28.6989
     6   ,/,  4X,  6HNUMBER,  9X,  11HCOEFFICIENT,  9X,                 M28.6990
     7            11HCOEFFICIENT,  11X,  9HAMPLITUDE,                   M28.6991
     8        9X, 11HFUNDAMENTAL   )                                    M28.6992
      NDX1 = IOFFA                                                      M28.6993
      NDX2 = IOFFB                                                      M28.6994
      D7 = EVDOUB(NDX1+2)**2  +  EVDOUB(NDX2+2)**2                      M28.6995
      D7 = SQRTZ ( D7 )                                                 M28.6996
      DO 4329  K=1, NFOUR                                               M28.6997
      L = K - 1                                                         M28.6998
      NDX1 = NDX1 + 1                                                   M28.6999
      NDX2 = NDX2 + 1                                                   M28.7000
      D5 = EVDOUB(NDX1)**2 + EVDOUB(NDX2)**2                            M28.7001
      D6 = SQRTZ ( D5 )                                                 M28.7002
      D4 = D6 / D7                                                      M28.7003
 4329 WRITE (LUNIT6, 4345)  L, EVDOUB(NDX1), EVDOUB(NDX2),              M28.7004
     1                         D6, D4                                   M28.7005
 4345 FORMAT ( 1X, I9,  3E20.10, F20.8 )                                M28.7006
      WRITE (LUNIT6, 4352)                                              M28.7007
 4352 FORMAT ( //, 1X )                                                 M28.7008
 9000 RETURN                                                            M28.7009
      END                                                               M28.7010
      SUBROUTINE BEGPLT                                                 M19.2244
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      IF ( IPRSOV(38) .EQ. 0 )                                          M39.3308
     1 CALL PLOTS (0, 0, 0 )                                            M24. 753
      RETURN                                                            M19.2248
      END                                                               M19.2249
      SUBROUTINE ENDPLT ( KALCOM )                                      M28.7011
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS SYSTEM-DEPENDENT MODULE IS ALWAYS CALLED ONCE AS CONTROL     M22.5714
C     LEAVES  'SUBR31'  DURING A NORMAL EXIT (NO ERROR STOP).           M22.5715
C     ARGUMENT  "KALCOM"  IS ZERO IF THE DATA CASE DID NO               M28.7012
C     CALCOMP PLOTTING, OR UNITY IF THERE WERE ONE OR MORE              M28.7013
C     SUCH PLOTS.   FOR MOST COMPUTERS, THE CALCOMP BUFFER              M28.7014
C     IS NOT CLOSED HERE, BUT RATHER IN  "FINTP" .                      M28.7015
      RETURN                                                            M22.5716
      END                                                               M22.5717
      SUBROUTINE FINTP                                                  M15.1430
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      IF ( IPRSOV(38) .GT. 0 )                                          M39.3309
     1 CALL PLOT ( 0.0, 0.0, 999 )                                      M24. 756
      IF (IVOLT .EQ. 7777)                                              M24. 757
     1 CLOSE (UNIT=20,DISPOSE='DELETE')                                 M24. 758
      RETURN                                                            M15.1431
      END                                                               M15.1432
      SUBROUTINE AXIS ( XX, YY, TITLE, NUMCH, SIZE, ANG,                M15.1433
     1                  BEGIN, SCALE )                                  M14.2636
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE  'AXIS'  WAS WRITTEN BY W. SCOTT MEYER OF BPA IN JUNE OF   M14.2637
C     1976, ESPECIALLY FOR BPA EMTP USAGE ONLY.   ANYONE HAVING THE REALM14.2638
C     CALCOMP MODULE OF THE SAME NAME SHOULD DISCARD THIS PRESENT       M14.2639
C     APPROXIMATE EQUIVALENT.   UNFORTUNATELY, BPA DOES NOT FALL INTO   M14.2640
C     THIS CLASS (OUR SYSTEM PROGRAMMERS THOUGHT THEY WERE BEING CLEVER M14.2641
C     BY ADDING TWO MORE ARGUMENTS TO THE MODULE WHICH IS AVAILABLE IN  M14.2642
C     OUR SYSTEM LIBRARY).                                              M14.2643
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      REAL*8          TEXT1, TITLE                                      M21. 401
      DATA  TIC   /  .075  /                                            M14.2644
      DATA  TICD2   /  .03  /                                           M15.1435
      DATA  HGT1   /  0.2  /                                            M15.1436
      DATA  HGT2   /  .15  /                                            M15.1437
      DATA  HGT3   /  .12  /                                            M15.1438
      DATA  TEXT1   / 6H( 10**     /                                    M15.1439
      D1 = ABS(BEGIN)                                                   M14.2645
      D2 = ABS( BEGIN + SIZE*SCALE)                                     M14.2646
      IF ( D2  .GT.  D1 )   D1 = D2                                     M14.2647
      IF ( D1  .LE.  0.0 )   GO TO 9000                                 M14.2648
      NDIG = 0                                                          M14.2649
 2313 IF ( D1  .LE.  1.000001 )   GO TO 2321                            M14.2650
      NDIG = NDIG - 1                                                   M14.2651
      D1 = D1 * 0.1                                                     M14.2652
      GO TO 2313                                                        M14.2653
 2321 IF ( D1  .GE.  .0999999 )   GO TO 2336                            M14.2654
      NDIG = NDIG + 1                                                   M14.2655
      D1 = D1 * 10.                                                     M14.2656
      GO TO 2321                                                        M14.2657
 2336 IF ( ANG    .GT.  0.0 )   GO TO 2345                              M15.1440
      IF ( NDIG  .GE.  2 )   GO TO 2345                                 M14.2659
      IF ( NDIG  .LT.  -3 )   GO TO 2345                                M14.2660
      NDIG = 0                                                          M14.2661
 2345 D3 = 10.**NDIG                                                    M14.2662
      SCAL1 = SCALE * D3                                                M14.2663
      BEG1 = BEGIN * D3                                                 M14.2664
      LENGTH = SIZE  +  .000001                                         M14.2665
      AXMID = SIZE*0.5 - 2.0                                            M14.2666
      D3 = -NDIG                                                        M14.2667
      NCHABS = IABS(NUMCH)                                              M14.2668
      IF ( ANG    .GT.  0.0 )   GO TO 2506                              M15.1441
      CALL PLOT ( XX, YY, 3 )                                           M14.2673
      XMAX = XX + SIZE                                                  M14.2674
      CALL PLOT ( XMAX, YY, 2 )                                         M14.2675
      D2 = BEG1  +  LENGTH * SCAL1                                      M14.2676
 2427 D1 = XX + LENGTH                                                  M14.2677
      CALL PLOT ( D1, YY+TIC, 3 )                                       M14.2678
      CALL PLOT ( D1, YY-TIC, 2 )                                       M14.2679
      CALL NUMBER ( D1-0.2, YY-0.3, HGT2, D2, ANG, 2 )                  M15.1442
      LENGTH = LENGTH - 1                                               M14.2681
      D2 = D2 - SCAL1                                                   M14.2682
      IF ( LENGTH  .GT.  0 )   GO TO 2427                               M14.2683
      D1 = YY - 0.6                                                     M14.2684
      D2 = XX + AXMID                                                   M14.2685
      CALL SYMBOL ( D2, D1, HGT1, TITLE, ANG, NCHABS )                  M15.1443
      IF ( NDIG  .EQ.  0 )   GO TO 2438                                 M14.2689
      D2 = D2 + 5.0                                                     M14.2690
      CALL SYMBOL ( D2, D1, HGT1, TEXT1,  ANG, 6 )                      M15.1444
      D2 = D2 + 1.2                                                     M14.2692
      CALL NUMBER ( D2, D1+.07, HGT1, D3, ANG, -1 )                     M15.1445
      D2 = D2 + 0.4                                                     M14.2694
      CALL SYMBOL ( D2, D1, HGT1, 1H), ANG, 1 )                         M15.1446
 2438 GO TO 2569                                                        M14.2696
 2506 CALL PLOT ( XX, YY+SIZE, 3 )                                      M14.2697
      CALL PLOT ( XX, YY, 2 )                                           M14.2698
      D1 = YY                                                           M14.2699
      D2 = BEG1                                                         M14.2700
      N3 = 0                                                            M14.2701
 2554 CALL PLOT ( XX+TIC, D1, 3 )                                       M14.2702
      CALL PLOT ( XX-TICD2, D1, 2 )                                     M15.1447
      IF ( IPRSUP  .GE.  1 )                                            M23.5706
     1WRITE (6, 4216)  XX, D1, HGT3, D2, ANG                            M23.5707
 4216 FORMAT (  46H BEFORE Y-AXIS CALL TO  'NUMBER'  IN  'AXIS' .,      M23.5708
     1  13X,  2HXX,  13X,  2HD1,  11X,  4HHGT3,  13X,  2HD2,            M23.5709
     2  12X,  3HANG  ,/,  46X,  4E15.6  )                               M23.5710
      CALL NUMBER ( XX-0.1, D1-0.2, HGT3, D2, ANG, 3 )                  M15.1448
      D2 = D2 + SCAL1                                                   M14.2705
      D1 = D1 + 1.0                                                     M14.2706
      N3 = N3 + 1                                                       M14.2707
      IF ( N3  .LE.  LENGTH )   GO TO 2554                              M14.2708
      D1 = YY + AXMID                                                   M14.2709
      D4 = XX - 0.3                                                     M14.2710
      CALL SYMBOL ( D4, D1, HGT1, TITLE, ANG, NCHABS )                  M15.1449
      IF ( NDIG  .EQ.  0 )   GO TO 2569                                 M14.2714
      D1 = D1 + 4.0                                                     M14.2715
      CALL SYMBOL ( D4, D1, HGT1, TEXT1, ANG, 6 )                       M15.1450
      D1 = D1 + 1.2                                                     M14.2717
      CALL NUMBER ( D4-.07, D1, HGT1, D3, ANG, -1 )                     M15.1451
      D1 = D1 + 0.4                                                     M14.2719
      CALL SYMBOL ( D4, D1, HGT1, 1H), ANG, 1 )                         M15.1452
 2569 RETURN                                                            M15.1453
 9000 WRITE (LUNIT6, 9006)                                              M17.1171
 9006 FORMAT ( //,   97H ERROR STOP WITHIN BPA CDC MODULE  'AXIS'  OF OVM14.2725
     1ERLAY 31.     GO SEE PROGRAM MAINTENANCE AT ONCE.     )           M14.2726
      CALL STOPTP                                                               
      END                                                               M14.2728
      SUBROUTINE  GRID ( D1, D2, D3, D4, N1, N2 )                       M19.2251
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE HAS BEEN WRITTEN FOR USE ONLY ON THE BPA CDC-6500     M19.2252
C     COMPUTER INSTALLATION, WHICH DOES NOT HAVE LEGITIMATE CALCOMP.    M19.2253
C     IT SIMULATES THE CALCOMP MODULE OF THE SAME NAME.   ANYONE WHO    M19.2254
C     HAS REAL CALCOMP PLOTTING SOFTWARE WITH THESE SUBROUTINES SHOULD  M19.2255
C     DESTROY THIS MODULE.        W. SCOTT MEYER, APRIL 1977.           M19.2256
      LL2 = 2                                                           M19.2257
      LL3 = 3                                                           M19.2258
      D8 = N2 * D4  +  D2                                               M19.2259
      D5 = D1                                                           M19.2260
      D6 = D2                                                           M19.2261
      I = 0                                                             M19.2262
 1878 I = I + 1                                                         M19.2263
      IF ( I  .GT.  N1 )   GO TO 1972                                   M19.2264
      D5 = D5 + D3                                                      M19.2265
      CALL PLOT ( D5, D6, LL3)                                          M19.2266
      IF ( D6  .GT.  D2 )   GO TO 1912                                  M19.2267
      D6 = D8                                                           M19.2268
      GO TO 1941                                                        M19.2269
 1912 D6 = D2                                                           M19.2270
 1941 CALL PLOT ( D5, D6, LL2)                                          M19.2271
      GO TO 1878                                                        M19.2272
 1972 D6 = D2                                                           M19.2273
      D5 = D1                                                           M19.2274
      D8 = D1 + N1 * D3                                                 M19.2275
      J = -1                                                            M19.2276
 1988 J = J + 1                                                         M19.2277
      IF ( J  .GT.  N2 )   GO TO 2029                                   M19.2278
      CALL PLOT ( D5, D6, LL3)                                          M19.2279
      IF ( D5  .GT.  D1 )   GO TO 2005                                  M19.2280
      D5 = D8                                                           M19.2281
      GO TO 2013                                                        M19.2282
 2005 D5 = D1                                                           M19.2283
 2013 CALL PLOT ( D5, D6, LL2)                                          M19.2284
      D6 = D6 + D4                                                      M19.2285
      GO TO 1988                                                        M19.2286
 2029 RETURN                                                            M19.2287
      END                                                               M19.2288
      SUBROUTINE LINPLT ( KRV, KLM )                                    M10. 275
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C                                                                       M10. 276
C****** THIS IS THE STANDARD VERSION OF LINPLT ******                   M10. 277
C                                                                       M10. 278
C)       LINPLT IS THE LINE PRINTER PLOT SUBROUTINE.  IT FILLS AND      M15.1454
C)    PRINTS AN OUTPUT ARRAY,  KUT , WITH THE NECESSARY SYMBOLS FROM THEM15.1455
C)    ARRAY KTSIN.  THE POSITION AND TYPE OF SYMBOL IS DETERMINED IN THEM15.1456
C)    CALLING SUBROUTINE,  =SUBR31= .                                   M15.1457
C        BEFORE PRINTING, FOR EACH CURVE,  LINPLT  FILLS THE COLUMNS BE-M10. 283
C     TWEEN THE MINIMUM AND MAXIMUM POINTS WITH THE CORRESPONDING SYM-  M10. 284
C     BOL.  IF A DIFFERENT CURVE SYMBOL IS ENCOUNTERED, THE COMMON      M10. 285
C     SYMBOL  *  IS INSERTED.  THIS PROCEDURE INSURES THAT =SPIKES= WILLM10. 286
C     APPEAR AS A LINE OF SYMBOLS.   ARRAYS  =MAX=  AND  =MIN=  ARE USEDM10. 287
C     FOR THIS PURPOSE.   BUT X-Y PRINTER PLOT BYPASSES THIS,           M28.7016
C     VIA  KRV = 77  SPECIAL FLAG.                                      M28.7017
C        IMMEDIATELY AFTER PRINTING, THE ARRAY IS BLANKED, TO BE READY  M10. 289
C     FOR THE NEXT LINE OF DATA POINTS, BEFORE RETURNING.               M10. 290
C        THE NORMAL RANGE OF VALUES FOR THE CALLING ARGUMENTS ARE --    M10. 291
C             KRV  1 TO 4   ( INCLUSIVE )                               M10. 292
C             KLM  1 TO 131 ( INCLUSIVE )                               M10. 293
C     WHEN CALLED WITH THESE VALUES,  LINPLT  WILL PLACE THE =KRV= CURVEM10. 294
C     SYMBOL IN THE =KLM= COLUMN OF THE OUTPUT ARRAY, UNLESS THAT COLUMNM10. 295
C     ALREADY CONTAINS A DIFFERENT KRV SYMBOL, IN WHICH CASE THE COMMON M10. 296
C     SYMBOL IS USED.                                                   M10. 297
C        THE SPECIAL VALUES OF KRV AND KLM ARE --                       M10. 298
C          KRV = -1   PLACE THE TIME AXIS LINE SYMBOL, 1, IN THE KLM    M10. 299
C                     COLUMN OF THE ARRAY.  THIS SYMBOL IS ERASED BY ANYM10. 300
C                     SUBSEQUENT CURVE SYMBOL.                          M10. 301
C          KRV = 0    THE FIRST CALL TO LINPLT USES THIS AND A DUMMY    M10. 302
C                     ARGUMENT FOR KLM TO INITIALIZE LINPLT.  THE ORDI- M10. 303
C                     NATE AXIS AND TIC MARKS ARE PRINTED.  THE FIRST   M10. 304
C                     OUTPUT LINE OF DATA WILL OVERWRITE THE AXIS.      M10. 305
C          KLM = -1   THIS AND A DUMMY ARGUMENT FOR KRV CAUSES THE OUT- M10. 306
C                     PUT ARRAY TO BE PRINTED.                          M10. 307
C          KRV = 99   PLACES THE INTERSECTION SYMBOL  "*"               M28.7018
C                     (VARIABLE KOM) IN COLUMN  KLM.                    M28.7019
C          KRV = 77   BYPASSES THE VERTICAL FILL BETWEEN MIN            M28.7020
C                     AND MAX, JUST BEFORE PRINTING.                    M28.7021
C                                                                       M10. 308
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION  KUT(131), KTSIN(4)                                     M12. 588
      DIMENSION  MAX(4), MIN(4)                                         M10. 309
      DATA  KTSIN(1)   /  1HA  /                                        M10. 310
      DATA  KTSIN(2)   /  1HB  /                                        M10. 311
      DATA  KTSIN(3)   /  1HC  /                                        M10. 312
      DATA  KTSIN(4)   /  1HD  /                                        M10. 313
      DATA  KOM        /  1H*  /                                        M10. 314
      DATA  KLINE      /  1H1  /                                        M10. 315
      DATA  KLANK      /  1H   /                                        M10. 316
      DATA  KXLINE     /  1H-  /                                        M10. 317
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M32.7606
      DATA  KUT(1)  / 1H  /                                             M32.7607
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M35.8873
      DATA  MIN(1) / 0 /,    MAX(1) / 0 /,    K / 0 /                   M35.8874
      IF ( KRV  .EQ.  99 )   GO TO 407                                  M28.7022
      JGO = 4                                                           M10. 318
      IF ( KRV  .EQ.  -1 )   JGO = 1                                    M10. 319
      IF ( KRV  .EQ.  0 )   JGO = 2                                     M10. 320
      IF ( KLM  .EQ.  -1 )   JGO = 3                                    M10. 321
      GO TO  ( 100, 200, 300, 400 ), JGO                                M10. 322
  100 KUT(KLM) = KLINE                                                  M10. 323
      GO TO 999                                                         M10. 324
  200 K = 1                                                             M10. 325
      DO 210 J = 1, 131                                                 M10. 326
  210 KUT(J) = KLANK                                                    M10. 327
      DO 220 J = 1, 131, 10                                             M10. 328
  220 KUT(J) = KLINE                                                    M10. 329
      WRITE (LUNIT6, 230) KUT                                           M17.1172
  230 FORMAT ( 1X, 131A1 )                                              M10. 331
      DO 240 J = 1, 131                                                 M10. 332
  240 KUT(J) = KXLINE                                                   M10. 333
      WRITE (LUNIT6, 250) KUT                                           M17.1173
  250 FORMAT ( 1H+, 131A1 )                                             M10. 335
      GO TO 350                                                         M10. 336
  300 IF ( KRV  .EQ.  77 )   GO TO 334                                  M28.7023
      DO 330  J=1, 4                                                    M28.7024
      JMIN = MIN(J)                                                     M10. 338
      IF ( JMIN .EQ. 132 )  GO TO 330                                   M10. 339
      JMAX = MAX(J)                                                     M10. 340
      IF ( JMIN  .EQ.  JMAX )   GO TO 330                               M10. 341
      DO 320  I=JMIN, JMAX                                              M10. 342
      IF ( KUT(I) .EQ. KTSIN(J) )  GO TO 320                            M10. 343
      IF ( KUT(I) .EQ. KLANK )  GO TO 310                               M10. 344
      IF ( KUT(I) .EQ. KLINE )  GO TO 310                               M10. 345
      KUT(I) = KOM                                                      M10. 346
      GO TO 320                                                         M10. 347
  310 KUT(I) = KTSIN(J)                                                 M10. 348
  320 CONTINUE                                                          M10. 349
  330 CONTINUE                                                          M10. 350
  334 IF ( K  .EQ.  0 )   GO TO 340                                     M28.7025
      K = 0                                                             M10. 352
      WRITE (LUNIT6, 250 )  KUT                                         M17.1174
      GO TO 350                                                         M10. 354
  340 WRITE (LUNIT6, 230)  KUT                                          M17.1175
  350 DO 360  J=1, 4                                                    M10. 356
      MAX(J) = 1                                                        M10. 357
  360 MIN(J) = 132                                                      M10. 358
      DO 370 J = 1, 131                                                 M10. 359
  370 KUT(J) = KLANK                                                    M10. 360
      GO TO 999                                                         M10. 361
  400 IF ( KLM  .GT.  MAX(KRV) )   MAX(KRV) = KLM                       M10. 362
      IF ( KLM  .LT.  MIN(KRV) )   MIN(KRV) = KLM                       M10. 363
      IF ( KUT(KLM) .EQ. KTSIN(KRV) )  GO TO 999                        M10. 364
      IF ( KUT(KLM) .EQ. KLANK )  GO TO 410                             M10. 365
      IF ( KUT(KLM) .EQ. KLINE )  GO TO 410                             M10. 366
  407 KUT(KLM) = KOM                                                    M28.7026
      GO TO 999                                                         M10. 368
  410 KUT(KLM) = KTSIN(KRV)                                             M10. 369
  999 CONTINUE                                                          M10. 370
      RETURN                                                            M10. 371
      END                                                               M10. 372
      SUBROUTINE PAPRSZ ( HORIZ, VERT )                                 M02.  90
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THE REAL (I.E., NON-DUMMY) SUBROUTINE PAPRSZ IS USED WITH BPA'S   M15.1458
C)    EAI FLATBED PLOTTER.   THIS IS THE WAY THE DIMENSIONS OF THE TABLEM15.1459
C)    UPON WHICH THE EMTP CAN LEGALLY DRAW PLOTS IS COMMUNICATED TO THE M28.7027
C)    SYSTEM.   THE PLOTTING SOFTWARE THEN WILL PROTECT THE HARDWARE,   M15.1461
C)    NOT ALLOWING THE PEN TO EXCEED THE  X  AND  Y  COORDINATES WHICH  M15.1462
C)    ARE SPECIFIED BY THE ARGUMENTS IN THE CALL TO THIS SUBROUTINE.    M15.1463
C)    FOR REGULAR CALCOMP DRUM PLOTTING (WHERE THERE IS NO TABLE AS SUCHM15.1464
C)    ), LEAVE IN THIS DUMMY SUBROUTINE AS IT IS WRITTEN HERE.   CALLINGM15.1465
C)    IT WILL DO NOTHING, OF COURSE.                                    M15.1466
      RETURN                                                            M02. 100
      END                                                               M02. 101
      SUBROUTINE ADVANZ ( JHMSP )                                       M02. 102
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THE REAL (I.E., NON-DUMMY) SUBROUTINE ADVANZ IS USED WITH BPA'S   M15.1467
C)    EAI FLATBED PLOTTER, TO ROLL  'JHMSP'  INCHES OF NEW PAPER ONTO   M15.1468
C)    THE PLOTTING TABLE, MAKING ROOM FOR THE UPCOMING PLOT TO BE       M15.1469
C)    DRAWN.   BUT WITH REGULAR CALCOMP DRUM PLOTTING (WHERE THERE IS   M15.1470
C)    NO TABLE, AS SUCH), THE PRESENT REPLACEMENT MODULE SHOULD BE USED,M15.1471
C)    WHICH HAS TWO EFFECTS.   FIRST, THE ROLLING OF THE PAPER IS       M15.1472
C)    BYPASSED, OF COURSE.   ALSO, BY SETTING  'JHMSP'  TO ZERO, THE    M15.1473
C)    ASSOCIATED PLOT ORIGIN-CHANGING IS EFFECTIVELY DISABLED.          M15.1474
      JHMSP = 0                                                         M02. 111
      RETURN                                                            M02. 112
      END                                                               M02. 113
      SUBROUTINE PRNTHD ( N1 )                                          M02. 114
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THE REAL (I.E., NON-DUMMY) SUBROUTINE PRNTHD IS USED WITH BPA'S   M15.1475
C)    EAI FLATBED PLOTTER.   IT IS ASSOCIATED WITH THE PRINTHEAD FEATUREM15.1476
C)    OF THE EAI PLOTTER, WHERE ALPHANUMERIC CHARACTERS MAY BE TYPED    M15.1477
C)    RATHER THAN DRAWN WITH THE PEN, IN ORDER TO SPEED UP THE PLOTTING M15.1478
C)    OPERATION.   SUBROUTINE PRNTHD IS CALLED WITH DIFFERENT VALUES    M15.1479
C)    FOR THE ARGUMENT  'N1'  IN ORDER TO CHANGE MODES ON THIS          M15.1480
C)    PRINTHEAD USAGE, AS FOLLOWS ......                                M15.1481
C)           0  -----  DRAWN CHARACTERS, USING THE PEN.                 M15.1482
C)           1  -----  HORIZONTALLY-TYPED LETTERING                     M15.1483
C)           2  -----  VERTICALLY-TYPED LETTERING                       M15.1484
C)    FOR REGULAR CALCOMP DRUM PLOTTING WHERE ALL CHARACTERS MUST BE    M15.1485
C)    DRAWN WITH THE PEN, LEAVE IN THIS DUMMY SUBROUTINE AS IT IS       M15.1486
C)    WRITTEN HERE.   CALLING IT WILL DO NOTHING, OF COURSE.            M15.1487
      RETURN                                                            M02. 128
      END                                                               M02. 129
